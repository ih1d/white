#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC="$ROOT/src"

echo "=== Running Fourmolu ==="
fourmolu --mode inplace --config "$ROOT/fourmolu.yaml" "$SRC"/*.hs

echo "=== Running HLint ==="
hlint "$SRC" --hint "$ROOT/.hlint.yaml"

echo "=== Done ==="
