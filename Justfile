#!/usr/bin/env -S just --justfile

packages := "effectful-config-file effectful-config-file-secretspec"

default:
    @just --list

# Build Haddock tarballs for Hackage
docs:
    stack haddock --haddock-for-hackage

[private]
check-env:
    @test -n "${HACKAGE_KEY:-}" || { echo "HACKAGE_KEY is not set" >&2; exit 1; }
    @pkg-config --exists libsecretspec || { echo "libsecretspec not found via pkg-config (see .envrc)" >&2; exit 1; }

# Upload packages and docs as Hackage candidates
candidate *pkgs=packages: check-env docs
    for p in {{pkgs}}; do stack upload --candidate --test-tarball "$p" && stack upload --candidate --documentation "$p" || exit 1; done

# Publish packages and docs to Hackage
release *pkgs=packages: check-env docs
    for p in {{pkgs}}; do stack upload --test-tarball "$p" && stack upload --documentation "$p" || exit 1; done
