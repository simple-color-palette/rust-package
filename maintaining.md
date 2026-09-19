# Maintaining

## Publish a new version

The [Release workflow](.github/workflows/release.yml) publishes the crate with [trusted publishing](https://crates.io/docs/trusted-publishing).

1. Bump `version` in `Cargo.toml`.
2. Run `cargo update --workspace` to sync `Cargo.lock`.
3. Commit and push: `git commit -am "1.2.3" && git push`
4. Run the Release workflow on `main` from the Actions tab, or with `gh workflow run release.yml`. It tests, publishes the crate, and pushes the `1.2.3` tag.
5. Create a release on GitHub.

A published version cannot be deleted or reused. It can only be yanked with `cargo yank --version 1.2.3`.
