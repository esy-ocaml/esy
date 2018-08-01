/**
 * Utilities for mocking npm registry.
 *
 * @flow
 */

import type {ServerResponse} from 'http';
import type {Gzip} from 'zlib';

const path = require('path');
const crypto = require('crypto');
const deepResolve = require('super-resolve');
const http = require('http');
const invariant = require('invariant');
const semver = require('semver');

const fsUtils = require('./fs');

export type PackageRegistry = {
  packages: PackageCollection,
  serverUrl: string,
};

type PackageCollection = Map<string, PackageEntry>;

export type PackageDesc = {|
  path: string,
  packageJson: Object,
  shasum?: string,
|};
export type PackageEntry = Map<string, PackageDesc>;

export type PackageRunDriver = (
  string,
  Array<string>,
  {registryUrl: string},
) => Promise<{|stdout: Buffer, stderr: Buffer|}>;

async function definePackage(
  packageRegistry: PackageRegistry,
  packageJson: {name: string, version: string},
  options: {shasum?: string} = {},
) {
  const {name, version} = packageJson;
  invariant(name != null, 'Missing "name" in package.json');
  invariant(version != null, 'Missing "version" in package.json');

  let packageEntry = packageRegistry.packages.get(name);

  if (!packageEntry) {
    packageRegistry.packages.set(name, (packageEntry = new Map()));
  }

  const packagePath = await fsUtils.createTemporaryFolder();
  await fsUtils.writeJson(path.join(packagePath, 'package.json'), packageJson);
  packageEntry.set(version, {
    path: packagePath,
    packageJson,
    shasum: options.shasum,
  });
  return packagePath;
}

async function defineLocalPackage(
  packageRegistry: PackageRegistry,
  packagePath: string,
  packageJson: {name: string, version: string},
) {
  const {name, version} = packageJson;
  invariant(name != null, 'Missing "name" in package.json');
  invariant(version != null, 'Missing "version" in package.json');

  await fsUtils.mkdir(packagePath);
  await fsUtils.writeJson(path.join(packagePath, 'package.json'), packageJson);
}

async function getPackageEntry(
  packages: PackageCollection,
  name: string,
): Promise<?PackageEntry> {
  return packages.get(name);
}

async function getPackageArchiveStream(
  packageRegistry: PackageRegistry,
  name: string,
  version: string,
): Promise<Gzip> {
  const packageEntry = await getPackageEntry(packageRegistry.packages, name);

  if (!packageEntry) {
    throw new Error(`Unknown package "${name}"`);
  }

  const packageVersionEntry = packageEntry.get(version);

  if (!packageVersionEntry) {
    throw new Error(`Unknown version "${version}" for package "${name}"`);
  }

  return fsUtils.packToStream(packageVersionEntry.path, {
    virtualPath: '/package',
  });
}

async function getPackageArchivePath(
  packageRegistry: PackageRegistry,
  name: string,
  version: string,
): Promise<string> {
  const packageEntry = await getPackageEntry(packageRegistry.packages, name);

  if (!packageEntry) {
    throw new Error(`Unknown package "${name}"`);
  }

  const packageVersionEntry = packageEntry.get(version);

  if (!packageVersionEntry) {
    throw new Error(`Unknown version "${version}" for package "${name}"`);
  }

  const archivePath = await fsUtils.createTemporaryFile(`${name}-${version}.tar.gz`);

  await fsUtils.packToFile(archivePath, packageVersionEntry.path, {
    virtualPath: '/package',
  });

  return archivePath;
}

async function getPackageArchiveHash(
  packageRegistry: PackageRegistry,
  name: string,
  version: string,
): Promise<string | Buffer> {
  const stream = await getPackageArchiveStream(packageRegistry, name, version);

  return new Promise((resolve, reject) => {
    const hash = crypto.createHash('sha1');
    hash.setEncoding('hex');

    // Send the archive to the hash function
    stream.pipe(hash);

    stream.on('end', () => {
      const finalHash = hash.read();
      invariant(finalHash, 'The hash should have been computated');
      resolve(finalHash);
    });
  });
}

async function getPackageHttpArchivePath(
  packageRegistry: PackageRegistry,
  name: string,
  version: string,
): Promise<string> {
  const packageEntry = await getPackageEntry(packageRegistry.packages, name);

  if (!packageEntry) {
    throw new Error(`Unknown package "${name}"`);
  }

  const packageVersionEntry = packageEntry.get(version);

  if (!packageVersionEntry) {
    throw new Error(`Unknown version "${version}" for package "${name}"`);
  }

  const archiveUrl = `${packageRegistry.serverUrl}/${name}/-/${name}-${version}.tgz`;

  return archiveUrl;
}

async function getPackageDirectoryPath(
  packageRegistry: PackageRegistry,
  name: string,
  version: string,
): Promise<string> {
  const packageEntry = await getPackageEntry(packageRegistry.packages, name);

  if (!packageEntry) {
    throw new Error(`Unknown package "${name}"`);
  }

  const packageVersionEntry = packageEntry.get(version);

  if (!packageVersionEntry) {
    throw new Error(`Unknown version "${version}" for package "${name}"`);
  }

  return packageVersionEntry.path;
}

async function initialize(
  options?: {persistent?: boolean} = {},
): Promise<PackageRegistry> {
  const packages = new Map();
  for (const packageFile of await fsUtils.walk(path.join(__dirname, '../fixtures'), {
    filter: ['package.json'],
  })) {
    const packageJson = await fsUtils.readJson(packageFile);
    const {name, version} = packageJson;

    if (name.startsWith('git-')) {
      continue;
    }

    let packageEntry = packages.get(name);

    if (!packageEntry) {
      packages.set(name, (packageEntry = new Map()));
    }

    packageEntry.set(version, {
      path: require('path').dirname(packageFile),
      packageJson,
    });
  }

  async function processPackageInfo(
    params: ?Array<string>,
    res: ServerResponse,
  ): Promise<boolean> {
    if (!params) {
      return false;
    }

    const [, scope, localName] = params;
    const name = scope ? `${scope}/${localName}` : localName;

    const packageEntry = await getPackageEntry(packages, name);

    if (!packageEntry) {
      return processError(res, 404, `Package not found: ${name}`);
    }

    const versions = Array.from(packageEntry.keys());

    const data = JSON.stringify({
      name,
      versions: Object.assign(
        {},
        ...(await Promise.all(
          versions.map(async version => {
            const packageVersionEntry = packageEntry.get(version);
            invariant(packageVersionEntry, 'This can only exist');

            return {
              [version]: Object.assign({}, packageVersionEntry.packageJson, {
                dist: {
                  shasum:
                    packageVersionEntry.shasum ||
                    (await getPackageArchiveHash(packageRegistry, name, version)),
                  tarball: await getPackageHttpArchivePath(
                    packageRegistry,
                    name,
                    version,
                  ),
                },
              }),
            };
          }),
        )),
      ),
      ['dist-tags']: {latest: semver.maxSatisfying(versions, '*')},
    });

    res.writeHead(200, {['Content-Type']: 'application/json'});
    res.end(data);

    return true;
  }

  async function processPackageTarball(
    params: ?Array<string>,
    res: ServerResponse,
  ): Promise<boolean> {
    if (!params) {
      return false;
    }

    const [, scope, localName, version] = params;
    const name = scope ? `${scope}/${localName}` : localName;

    const packageEntry = await getPackageEntry(packages, name);

    if (!packageEntry) {
      return processError(res, 404, `Package not found: ${name}`);
    }

    const packageVersionEntry = packageEntry.get(version);

    if (!packageVersionEntry) {
      return processError(res, 404, `Package not found: ${name}@${version}`);
    }

    res.writeHead(200, {
      ['Content-Type']: 'application/octet-stream',
      ['Transfer-Encoding']: 'chunked',
    });

    const packStream = fsUtils.packToStream(packageVersionEntry.path, {
      virtualPath: '/package',
    });
    packStream.pipe(res);

    return true;
  }

  function processError(
    res: ServerResponse,
    statusCode: number,
    errorMessage: string,
  ): boolean {
    console.error(errorMessage);

    res.writeHead(statusCode);
    res.end(errorMessage);

    return true;
  }

  const serverUrl = await new Promise((resolve, reject) => {
    const server = http.createServer(
      (req, res) =>
        void (async () => {
          try {
            if (
              await processPackageInfo(
                req.url.match(/^\/(?:(@[^\/]+)\/)?([^@\/][^\/]*)$/),
                res,
              )
            ) {
              return;
            }

            if (
              await processPackageTarball(
                req.url.match(/^\/(?:(@[^\/]+)\/)?([^@\/][^\/]*)\/-\/\2-(.*)\.tgz$/),
                res,
              )
            ) {
              return;
            }

            processError(res, 404, `Invalid route: ${req.url}`);
          } catch (error) {
            processError(res, 500, error.stack);
          }
        })(),
    );

    if (!options.persistent) {
      // We don't want the server to prevent the process from exiting
      server.unref();
    }
    server.listen(() => {
      const {port} = server.address();
      resolve(`http://localhost:${port}`);
    });
  });

  const packageRegistry = {serverUrl, packages};

  return packageRegistry;
}

module.exports = {
  getPackageDirectoryPath,
  getPackageHttpArchivePath,
  getPackageArchivePath,
  definePackage,
  defineLocalPackage,
  initialize,
};
