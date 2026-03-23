/*
 * file-permissions.test.ts
 *
 * Copyright (C) 2020-2026 Posit Software, PBC
 */

import { unitTest } from "../test.ts";
import { assert, assertEquals } from "testing/asserts";
import { join } from "../../src/deno_ral/path.ts";
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
      const testFile = join(tempDir, "readonly.txt");
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
      const testFile = join(tempDir, "writable.txt");
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

// Simulates the Nix/deb scenario: Deno.copyFileSync from a read-only source
// preserves the read-only mode on the copy. ensureUserWritable must fix it.
unitTest(
  "file-permissions - copyFileSync from read-only source then ensureUserWritable",
  // deno-lint-ignore require-await
  async () => {
    if (Deno.build.os === "windows") return;

    const tempDir = Deno.makeTempDirSync({ prefix: "quarto-perm-test" });
    try {
      // Create a "source resource" file and make it read-only
      const src = join(tempDir, "source.lua");
      Deno.writeTextFileSync(src, "-- filter code");
      Deno.chmodSync(src, 0o444);

      // Copy it (this is what quarto create does internally)
      const dest = join(tempDir, "dest.lua");
      Deno.copyFileSync(src, dest);

      // Without the fix, dest inherits 0o444 from src
      const modeBefore = safeModeFromFile(dest);
      assert(modeBefore !== undefined);
      assert(
        (modeBefore! & 0o200) === 0,
        "Copied file should inherit read-only mode from source",
      );

      // Apply the fix
      ensureUserWritable(dest);

      const modeAfter = safeModeFromFile(dest);
      assertEquals(
        modeAfter,
        0o644,
        "Copied file should be user-writable after ensureUserWritable",
      );
    } finally {
      // Restore write on source so cleanup succeeds
      Deno.chmodSync(join(tempDir, "source.lua"), 0o644);
      Deno.removeSync(tempDir, { recursive: true });
    }
  },
);
