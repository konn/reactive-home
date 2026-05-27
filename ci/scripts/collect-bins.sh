#!/usr/bin/env bash
set -euo pipefail
DEST="$(pwd)/${1}"

echo "[*] Create directory tree"
TESTS="${DEST}"/tests
TESTS_LIST="${DEST}"/tests.list
EXES="${DEST}"/exes
EXES_LIST="${DEST}"/exes.list
BENCHS="${DEST}"/benchs
BENCHS_LIST="${DEST}"/benchs.list
set -x
mkdir -p "${TESTS}"
mkdir -p "${EXES}"
mkdir -p "${BENCHS}"
touch "${TESTS_LIST}"
touch "${EXES_LIST}"
touch "${BENCHS_LIST}"
set +x

echo "[*] Places artifacts into the correct place"

jq -rMCc '."install-plan"[] | select(.style == "local") | select(."bin-file" != null) | {path: ."bin-file", component: ."component-name"}' dist-newstyle/cache/plan.json | while read -r OBJ; do
    COMPONENT=$(jq -rMCc '.component' <<< "${OBJ}")
    BIN=$(jq -rMCc '.path' <<< "${OBJ}")
    TYPE=$(echo "${COMPONENT}" | cut -d':' -f1)
    echo "---"
    echo "- Comp: ${COMPONENT}"
    echo "- Path: ${BIN}"
    echo "- Type: ${TYPE}"
    case "${TYPE}" in
        exe)
            COPY_DEST=${EXES}
            LIST=${EXES_LIST}
        ;;
        test)
            COPY_DEST=${TESTS}
            LIST=${TESTS_LIST}
        ;;
        *)
            COPY_DEST=${BENCHS}
            LIST=${BENCHS_LIST}
        ;;
    esac
    if [ -f "${BIN}" ]; then
        echo "Copying to ${COPY_DEST}"
        cp "${BIN}" "${COPY_DEST}/"
        basename "${BIN}" >> "${LIST}"
    else
        echo "Binary not found. Skipped."
    fi
done
echo "[*] Collected. Compressing..."

TARBALL="${DEST}.tar.zst"
tar --use-compress-program="zstdmt -8" -caf "${TARBALL}" "./${1}"
echo "[*] Compressed as: ${TARBALL}"
