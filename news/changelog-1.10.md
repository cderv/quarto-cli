All changes included in 1.10:

## Commands

### `quarto create`

- Fix `quarto create` producing read-only files when Quarto is installed via system packages (e.g., `.deb`). Files copied from installed resources now have user-write permission ensured.
