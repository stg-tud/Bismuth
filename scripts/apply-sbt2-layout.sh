#!/usr/bin/env bash
# Script to reorganize Modules/ from the old sbt layout to the sbt2 layout.
#
# Old layout:
#   Modules/X/{shared,jvm,js,native}/src/{main,test}/scala/...
#
# New layout:
#   Modules/X/src/{main,test}/{scala,scalajvm,scalajs,scalanative}/...
#
# Mapping:
#   shared/src/main/scala -> src/main/scala
#   shared/src/test/scala -> src/test/scala
#   jvm/src/main/scala    -> src/main/scalajvm
#   jvm/src/test/scala    -> src/test/scalajvm
#   js/src/main/scala     -> src/main/scalajs
#   js/src/test/scala     -> src/test/scalajs
#   native/src/main/scala -> src/main/scalanative
#   native/src/test/scala -> src/test/scalanative
#
# Also moves test resources:
#   shared/src/test/resources -> src/test/resources
#   jvm/src/test/resources    -> src/test/$SRC_SET/resources (if needed)
#
# Usage:
#   git checkout sbt2b   # or the branch with code changes but old layout
#   bash apply-sbt2-layout.sh
#   git add -A
#   git commit -m "apply sbt2 layout"

set -euo pipefail

MODULES=("Modules/Channels" "Modules/Lore" "Modules/Reactives")

for MOD in "${MODULES[@]}"; do
  echo "=== Processing $MOD ==="

  # Platform dirs to source-set mappings
  declare -A PLATFORM_MAP=(
    ["shared"]="scala"
    ["jvm"]="scalajvm"
    ["js"]="scalajs"
    ["native"]="scalanative"
  )

  for PLATFORM in shared jvm js native; do
    SRC_SET="${PLATFORM_MAP[$PLATFORM]}"

    # --- main sources ---
    OLD_MAIN="$MOD/$PLATFORM/src/main/scala"
    NEW_MAIN="$MOD/src/main/$SRC_SET"

    if [ -d "$OLD_MAIN" ]; then
      echo "  main: $PLATFORM -> $SRC_SET"
      mkdir -p "$NEW_MAIN"
      # Move all files from old to new, preserving subdirectory structure
      for f in $(find "$OLD_MAIN" -type f); do
        rel="${f#$OLD_MAIN/}"
        dest="$NEW_MAIN/$rel"
        mkdir -p "$(dirname "$dest")"
        mv "$f" "$dest"
      done
    fi

    # --- test sources ---
    OLD_TEST="$MOD/$PLATFORM/src/test/scala"
    NEW_TEST="$MOD/src/test/$SRC_SET"

    if [ -d "$OLD_TEST" ]; then
      echo "  test: $PLATFORM -> $SRC_SET"
      mkdir -p "$NEW_TEST"
      for f in $(find "$OLD_TEST" -type f); do
        rel="${f#$OLD_TEST/}"
        dest="$NEW_TEST/$rel"
        mkdir -p "$(dirname "$dest")"
        mv "$f" "$dest"
      done
    fi

    # --- test resources ---
    OLD_TEST_RES="$MOD/$PLATFORM/src/test/resources"
    if [ -d "$OLD_TEST_RES" ]; then
      # Shared test resources go to src/test/resources (not inside a source set)
      if [ "$PLATFORM" = "shared" ]; then
        NEW_TEST_RES="$MOD/src/test/resources"
      else
        NEW_TEST_RES="$MOD/src/test/$SRC_SET/resources"
      fi
      echo "  test resources: $PLATFORM -> ${NEW_TEST_RES#$MOD/}"
      mkdir -p "$NEW_TEST_RES"
      for f in $(find "$OLD_TEST_RES" -type f); do
        rel="${f#$OLD_TEST_RES/}"
        dest="$NEW_TEST_RES/$rel"
        mkdir -p "$(dirname "$dest")"
        mv "$f" "$dest"
      done
    fi

    # --- main resources ---
    OLD_MAIN_RES="$MOD/$PLATFORM/src/main/resources"
    if [ -d "$OLD_MAIN_RES" ]; then
      NEW_MAIN_RES="$MOD/src/main/$SRC_SET/resources"
      echo "  main resources: $PLATFORM -> $SRC_SET"
      mkdir -p "$NEW_MAIN_RES"
      for f in $(find "$OLD_MAIN_RES" -type f); do
        rel="${f#$OLD_MAIN_RES/}"
        dest="$NEW_MAIN_RES/$rel"
        mkdir -p "$(dirname "$dest")"
        mv "$f" "$dest"
      done
    fi
  done

  # --- Clean up old platform dirs ---
  for PLATFORM in shared jvm js native; do
    OLD_DIR="$MOD/$PLATFORM"
    if [ -d "$OLD_DIR" ]; then
      # Remove build artifacts (target/) that might remain in old platform dirs
      rm -rf "$OLD_DIR/target" 2>/dev/null || true
      # Remove empty directory trees
      find "$OLD_DIR" -depth -type d -empty -delete 2>/dev/null || true
      # If the platform dir itself is now empty, remove it
      if [ -d "$OLD_DIR" ] && [ -z "$(ls -A "$OLD_DIR" 2>/dev/null)" ]; then
        rmdir "$OLD_DIR"
      fi
    fi
  done

  echo "  done"
done

echo ""
echo "=== Done. Now run: git add -A && git status ==="
