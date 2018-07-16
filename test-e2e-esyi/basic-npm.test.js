/* @flow */

const setup = require('./setup');

describe(`Basic tests for npm packages`, () => {
  test(
    `it should correctly install a single dependency that contains no sub-dependencies`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {[`no-deps`]: `1.0.0`},
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('no-deps')`)).resolves.toMatchObject({
          name: `no-deps`,
          version: `1.0.0`,
        });
      },
    ),
  );

  test(
    `it should correctly install a dependency that itself contains a fixed dependency`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {[`one-fixed-dep`]: `1.0.0`},
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('one-fixed-dep')`)).resolves.toMatchObject({
          name: `one-fixed-dep`,
          version: `1.0.0`,
          dependencies: {
            [`no-deps`]: {
              name: `no-deps`,
              version: `1.0.0`,
            },
          },
        });
      },
    ),
  );

  test(
    `it should correctly install a dependency that itself contains a range dependency`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {[`one-range-dep`]: `1.0.0`},
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('one-range-dep')`)).resolves.toMatchObject({
          name: `one-range-dep`,
          version: `1.0.0`,
          dependencies: {
            [`no-deps`]: {
              name: `no-deps`,
              version: `1.1.0`,
            },
          },
        });
      },
    ),
  );

  test.skip(
    `it should correctly install an inter-dependency loop`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {[`dep-loop-entry`]: `1.0.0`},
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(
          source(
            // eslint-disable-next-line
            `require('dep-loop-entry') === require('dep-loop-entry').dependencies['dep-loop-exit'].dependencies['dep-loop-entry']`,
          ),
        );
      },
    ),
  );

  test.skip(
    `it should install from archives on the filesystem`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {[`no-deps`]: setup.getPackageArchivePath(`no-deps`, `1.0.0`)},
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('no-deps')`)).resolves.toMatchObject({
          name: `no-deps`,
          version: `1.0.0`,
        });
      },
    ),
  );

  test.skip(
    `it should install the dependencies of any dependency fetched from the filesystem`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {
          [`one-fixed-dep`]: setup.getPackageArchivePath(`one-fixed-dep`, `1.0.0`),
        },
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('one-fixed-dep')`)).resolves.toMatchObject({
          name: `one-fixed-dep`,
          version: `1.0.0`,
          dependencies: {
            [`no-deps`]: {
              name: `no-deps`,
              version: `1.0.0`,
            },
          },
        });
      },
    ),
  );

  test.skip(
    `it should install from files on the internet`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {[`no-deps`]: setup.getPackageHttpArchivePath(`no-deps`, `1.0.0`)},
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('no-deps')`)).resolves.toMatchObject({
          name: `no-deps`,
          version: `1.0.0`,
        });
      },
    ),
  );

  test.skip(
    `it should install the dependencies of any dependency fetched from the internet`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {
          [`one-fixed-dep`]: setup.getPackageHttpArchivePath(`one-fixed-dep`, `1.0.0`),
        },
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('one-fixed-dep')`)).resolves.toMatchObject({
          name: `one-fixed-dep`,
          version: `1.0.0`,
          dependencies: {
            [`no-deps`]: {
              name: `no-deps`,
              version: `1.0.0`,
            },
          },
        });
      },
    ),
  );

  test.skip(
    `it should install from local directories`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {[`no-deps`]: setup.getPackageDirectoryPath(`no-deps`, `1.0.0`)},
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('no-deps')`)).resolves.toMatchObject({
          name: `no-deps`,
          version: `1.0.0`,
        });
      },
    ),
  );

  test.skip(
    `it should install the dependencies of any dependency fetched from a local directory`,
    setup.makeTemporaryEnv(
      {
        name: 'root',
        version: '1.0.0',
        dependencies: {
          [`one-fixed-dep`]: setup.getPackageDirectoryPath(`one-fixed-dep`, `1.0.0`),
        },
      },
      async ({path, run, source}) => {
        await run(`install`);

        await expect(source(`require('one-fixed-dep')`)).resolves.toMatchObject({
          name: `one-fixed-dep`,
          version: `1.0.0`,
          dependencies: {
            [`no-deps`]: {
              name: `no-deps`,
              version: `1.0.0`,
            },
          },
        });
      },
    ),
  );
});
