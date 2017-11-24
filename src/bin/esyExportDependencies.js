/**
 * @flow
 */

import type {CommandContext} from './esy';

import {getSandbox, getBuildConfig} from './esy';
import * as Task from '../build-task';
import * as Sandbox from '../sandbox';
import * as Env from '../environment';
import * as Graph from '../graph.js';
import * as Child from '../lib/child_process.js';
import * as fs from '../lib/fs.js';
import {PromiseQueue} from '../lib/Promise.js';

const esyBin = require.resolve('../../bin/_esy');

export default async function esyExport(ctx: CommandContext) {
  const sandbox = await getSandbox(ctx);
  const config = await getBuildConfig(ctx);

  const toExport = [];
  Graph.traverse(sandbox.root, build => {
    switch (build.sourceType) {
      case 'immutable':
        toExport.push(build);
        break;
      case 'transient':
        ctx.reporter.warn(
          `${build.packagePath} is a linked dependency, skipping it for export...`,
        );
        break;
      case 'root':
        // do nothing
        break;
    }
  });

  const toExportMissing = [];
  await Promise.all(
    toExport.map(async build => {
      const finalInstallPath = config.getFinalInstallPath(build);
      if (!await fs.exists(finalInstallPath)) {
        toExportMissing.push(build);
      }
    }),
  );

  if (toExportMissing.length > 0) {
    ctx.error('unable to export some of the artefacts, run "esy build" command');
  }

  const exportQueue = new PromiseQueue({concurrency: config.buildConcurrency});

  await Promise.all(
    toExport.map(build =>
      exportQueue.add(async () => {
        const finalInstallPath = config.getFinalInstallPath(build);
        await Child.spawn(esyBin, ['export-build', finalInstallPath], {stdio: 'inherit'});
      }),
    ),
  );
}
