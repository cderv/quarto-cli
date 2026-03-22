/*
* create.test.ts
*
* Copyright (C) 2020-2022 Posit Software, PBC
*
*/

import { execProcess } from "../../../src/core/process.ts";
import { dirname, fromFileUrl, join } from "../../../src/deno_ral/path.ts";
import { walkSync } from "../../../src/deno_ral/fs.ts";
import { CreateResult } from "../../../src/command/create/cmd-types.ts";
import { assert } from "testing/asserts";
import { quartoDevCmd } from "../../utils.ts";

// Resolve the repo's resource directory from the test file location.
// tests/smoke/create/create.test.ts → src/resources/
const testDir = dirname(fromFileUrl(import.meta.url));
const resourceDir = join(testDir, "..", "..", "..", "src", "resources");

const kCreateTypes: Record<string, string[]> = {
  "project": ["website", "default", "book", "website:blog"],
  "extension": [
    "filter",
    "shortcode",
    "revealjs-plugin",
    "journal",
    "format:html",
    "format:pdf",
    "format:docx",
    "format:revealjs",
    "engine",
  ],
};

const tempDir = Deno.makeTempDirSync();
for (const type of Object.keys(kCreateTypes)) {
  for (const template of kCreateTypes[type]) {
    Deno.test(`quarto create ${type} ${template}`, async (t) => {
      // Configure the item to test
      const artifactName = template.replaceAll(/:/gm, "");
      const artifactPath = join(tempDir, artifactName);
      const createDirective = {
        type,
        directive: {
          directory: artifactPath,
          template,
          name: artifactName,
        },
      };

      // Create the artifact
      let result: CreateResult | undefined = undefined;
      await t.step(`> quarto ${type} ${template}`, async () => {
        // test quarto cmd render
        const cmd = [quartoDevCmd(), "create", "--json"];
        const stdIn = JSON.stringify(createDirective);
        const process = await execProcess({
          cmd: cmd[0],
          args: cmd.slice(1),
          stdout: "piped",
          stderr: "piped",
        }, stdIn);
        assert(process.success, process.stderr);
        if (process.stdout) {
          result = JSON.parse(process.stdout) as CreateResult;
        }
        assert(process.success, process.stderr);
      });

      // Verify all created files are user-writable.
      // NOTE: In dev environments, resource files are already writable (0o644),
      // so this test passes even without ensureUserWritable. It guards against
      // regressions; the unit test in file-permissions.test.ts covers the
      // read-only → writable transition directly.
      if (Deno.build.os !== "windows") {
        await t.step(`> check writable ${type} ${template}`, () => {
          for (const entry of walkSync(artifactPath)) {
            if (entry.isFile) {
              const stat = Deno.statSync(entry.path);
              assert(
                stat.mode !== null && (stat.mode! & 0o200) !== 0,
                `File ${entry.path} is not user-writable (mode: ${stat.mode?.toString(8)})`,
              );
            }
          }
        });
      }

      // Render the artifact
      await t.step(`> render ${type} ${template}`, async () => {
        const path = result!.path;
        const openfiles = result!.openfiles;
        assert(
          openfiles.length > 0,
          `Artifact ${type} ${template} failed to produce any files to open.`,
        );

        // Build engine extensions before rendering
        if (template === "engine") {
          const buildCmd = [quartoDevCmd(), "call", "build-ts-extension"];
          const buildProcess = await execProcess({
            cmd: buildCmd[0],
            args: buildCmd.slice(1),
            cwd: path,
            stdout: "piped",
            stderr: "piped",
          });
          assert(buildProcess.success, buildProcess.stderr);
        }

        for (const file of openfiles) {
          if (file.endsWith(".qmd")) {
            // provide a step name and function
            const cmd = [quartoDevCmd(), "render", file];
            const process = await execProcess({
              cmd: cmd[0],
              args: cmd.slice(1),
              cwd: path,
              stdout: "piped",
              stderr: "piped",
            });
            assert(process.success, process.stderr);
          }
        }
      });

      // Cleanup the artifact dir
      Deno.removeSync(artifactPath, { recursive: true });
    });
  }
}

// Test that simulates read-only resource files (as found in Nix/deb installs).
// Makes source template files read-only before running quarto create, then
// verifies that output files are still user-writable.
if (Deno.build.os !== "windows") {
  Deno.test(
    "quarto create extension filter - read-only source files",
    async (t) => {
      const templateDir = join(resourceDir, "create", "extensions", "filter");
      const sourceFiles: { path: string; originalMode: number }[] = [];

      // Collect all files and make them read-only
      await t.step("> make source templates read-only", () => {
        for (const entry of walkSync(templateDir)) {
          if (entry.isFile) {
            const stat = Deno.statSync(entry.path);
            if (stat.mode !== null) {
              sourceFiles.push({
                path: entry.path,
                originalMode: stat.mode!,
              });
              Deno.chmodSync(entry.path, stat.mode! & ~0o222);
            }
          }
        }
        // Verify at least one file was made read-only
        assert(
          sourceFiles.length > 0,
          "Should have found template files to make read-only",
        );
      });

      try {
        // Create the extension from read-only sources
        const artifactPath = join(tempDir, "readonly-filter-test");
        const createDirective = {
          type: "extension",
          directive: {
            directory: artifactPath,
            template: "filter",
            name: "readonlyfiltertest",
          },
        };

        await t.step("> quarto create with read-only sources", async () => {
          const cmd = [quartoDevCmd(), "create", "--json"];
          const stdIn = JSON.stringify(createDirective);
          const process = await execProcess({
            cmd: cmd[0],
            args: cmd.slice(1),
            stdout: "piped",
            stderr: "piped",
          }, stdIn);
          assert(process.success, process.stderr);
        });

        // Verify all output files are user-writable
        await t.step("> verify output files are writable", () => {
          let fileCount = 0;
          for (const entry of walkSync(artifactPath)) {
            if (entry.isFile) {
              const stat = Deno.statSync(entry.path);
              assert(
                stat.mode !== null && (stat.mode! & 0o200) !== 0,
                `File ${entry.path} is not user-writable (mode: ${stat.mode?.toString(8)})`,
              );
              fileCount++;
            }
          }
          assert(fileCount > 0, "Should have created at least one file");
        });

        // Cleanup artifact
        Deno.removeSync(artifactPath, { recursive: true });
      } finally {
        // Always restore original permissions
        for (const { path, originalMode } of sourceFiles) {
          try {
            Deno.chmodSync(path, originalMode);
          } catch {
            // Best-effort restore
          }
        }
      }
    },
  );
}
