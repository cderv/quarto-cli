/*
 * file-permissions.test.ts
 *
 * Copyright (C) 2020-2026 Posit Software, PBC
 */

import { unitTest } from "../test.ts";
import { assert, assertEquals } from "testing/asserts";
import {
  ensureUserWritable,
  safeModeFromFile,
} from "../../src/deno_ral/fs.ts";

unitTest(
  "file-permissions - ensureUserWritable fixes read-only files",
  // deno-lint-ignore require-await
  async () => {
    if (Deno.build.os === "windows") return;

    const tempDir = Deno.makeTempDirSync({ prefix: "quarto-perm-test" });
    try {
      const testFile = `${tempDir}/readonly.txt`;
      Deno.writeTextFileSync(testFile, "test content");

      // Make file read-only (simulate system-installed resource)
      Deno.chmodSync(testFile, 0o444);

      const modeBefore = safeModeFromFile(testFile);
      assert(modeBefore !== undefined);
      assert(
        (modeBefore! & 0o200) === 0,
        "File should be read-only before fix",
      );

      ensureUserWritable(testFile);

      const modeAfter = safeModeFromFile(testFile);
      assertEquals(
        modeAfter,
        0o644,
        "Mode should be exactly 0o644 (0o444 | 0o200) — only user write bit added",
      );
    } finally {
      Deno.removeSync(tempDir, { recursive: true });
    }
  },
);

unitTest(
  "file-permissions - ensureUserWritable leaves writable files unchanged",
  // deno-lint-ignore require-await
  async () => {
    if (Deno.build.os === "windows") return;

    const tempDir = Deno.makeTempDirSync({ prefix: "quarto-perm-test" });
    try {
      const testFile = `${tempDir}/writable.txt`;
      Deno.writeTextFileSync(testFile, "test content");
      Deno.chmodSync(testFile, 0o644);

      const modeBefore = safeModeFromFile(testFile);

      ensureUserWritable(testFile);

      const modeAfter = safeModeFromFile(testFile);
      assertEquals(
        modeAfter,
        modeBefore,
        "Mode should be unchanged for already-writable file",
      );
    } finally {
      Deno.removeSync(tempDir, { recursive: true });
    }
  },
);
