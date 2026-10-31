# Contributing

## Uploading to Hackage

Hackage's doc builder lacks `libsecretspec`, so docs are built and uploaded locally.

1. Load the environment (`direnv allow`, see `.envrc`) so `libsecretspec` is found by `pkg-config`.
2. Export a Hackage API token: `export HACKAGE_KEY=...`.
3. Upload candidates for all packages: `just candidate`, then review them on Hackage.
4. Publish: `just release`.
