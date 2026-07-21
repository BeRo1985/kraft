#!/usr/bin/env bash
#
# runtests.sh - Compile (with FreePascal / fpc) and run the Kraft test suite.
#
# Every test is a standalone console program (*.dpr) that uses the `kraft`
# unit from the parent directory (../kraft.pas). The sources are written to be
# both Delphi- and FreePascal-compatible: under FPC the
#
#     {$ifdef fpc}
#      {$mode delphi}
#     {$endif}
#     {$apptype console}
#
# header block selects Delphi mode, while Delphi (dcc32/dcc64) simply builds
# the .dpr project directly.
#
# Usage:
#   ./runtests.sh                 compile and run every test
#   ./runtests.sh boxtest hull*   compile and run only the matching tests
#   ./runtests.sh -c              compile only, do not run
#   ./runtests.sh -q              quiet: show test output only on failure
#   ./runtests.sh --clean         remove build artifacts (bin/, units/) and exit
#   ./runtests.sh -h              show this help
#
# Environment overrides:
#   FPC           fpc binary to use               (default: fpc)
#   FPCFLAGS      extra flags appended to fpc      (default: empty)
#   RUN_TIMEOUT   seconds allowed per test, 0=off  (default: 180)
#
set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR" || exit 1

# --- Configuration ---------------------------------------------------------
FPC="${FPC:-fpc}"
UNIT_DIR=".."                       # location of kraft.pas
BIN_DIR="bin"                       # compiled executables
UNIT_OUT="units"                    # .ppu / .o intermediates (kraft.ppu is cached here)
RUN_TIMEOUT="${RUN_TIMEOUT:-180}"   # seconds per test run, 0 disables the timeout
EXTRA_FPCFLAGS="${FPCFLAGS:-}"

COMPILE_ONLY=0
VERBOSE=1                           # 1 = always print test output, 0 = only on failure

usage() { sed -n '3,28p' "$0" | sed 's/^# \{0,1\}//'; }

# --- Argument parsing ------------------------------------------------------
SELECT=()
while [ $# -gt 0 ]; do
  case "$1" in
    -c|--compile-only) COMPILE_ONLY=1 ;;
    -q|--quiet)        VERBOSE=0 ;;
    --clean)           rm -rf "$BIN_DIR" "$UNIT_OUT"; echo "Removed $BIN_DIR/ and $UNIT_OUT/"; exit 0 ;;
    -h|--help)         usage; exit 0 ;;
    --)                shift; while [ $# -gt 0 ]; do SELECT+=("$1"); shift; done; break ;;
    -*)                echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
    *)                 SELECT+=("$1") ;;
  esac
  shift
done

# --- Preconditions ---------------------------------------------------------
if ! command -v "$FPC" >/dev/null 2>&1; then
  echo "error: fpc compiler '$FPC' not found in PATH (set FPC=/path/to/fpc)" >&2
  exit 1
fi
if [ ! -f "$UNIT_DIR/kraft.pas" ]; then
  echo "error: $UNIT_DIR/kraft.pas not found - run this script from src/tests/" >&2
  exit 1
fi

# --- Assemble the list of tests to build -----------------------------------
TESTS=()
if [ ${#SELECT[@]} -gt 0 ]; then
  for pat in "${SELECT[@]}"; do
    base="${pat%.dpr}"; base="${base%.pas}"
    matched=0
    for f in $base.dpr; do
      [ -f "$f" ] || continue
      TESTS+=("${f%.dpr}"); matched=1
    done
    [ "$matched" -eq 0 ] && echo "warning: no test matches '$pat'" >&2
  done
else
  for f in *.dpr; do
    [ -f "$f" ] || continue
    TESTS+=("${f%.dpr}")
  done
fi

if [ ${#TESTS[@]} -eq 0 ]; then
  echo "error: no tests to build" >&2
  exit 1
fi

mkdir -p "$BIN_DIR" "$UNIT_OUT"

# Force Delphi mode and point the unit search path at ../kraft.pas. -Xs strips
# the binary; kraft.ppu is cached in $UNIT_OUT so it is only compiled once.
FPC_ARGS=( -Mdelphi -O2 -Xs -vw -Fu"$UNIT_DIR" -FE"$BIN_DIR" -FU"$UNIT_OUT" )
[ -n "$EXTRA_FPCFLAGS" ] && FPC_ARGS+=( $EXTRA_FPCFLAGS )

# --- Build / run loop ------------------------------------------------------
echo "fpc:        $("$FPC" -iV) ($(command -v "$FPC"))"
echo "tests:      ${#TESTS[@]}"
echo "unit path:  $UNIT_DIR"
[ "$COMPILE_ONLY" -eq 1 ] && echo "mode:       compile-only"
echo "=========================================================================="

compile_fail=0
run_fail=0
timed_out=0
passed=0
FAILED=()

for name in "${TESTS[@]}"; do
  printf '%-26s ' "$name"

  build_log="$BIN_DIR/$name.build.log"
  if ! "$FPC" "${FPC_ARGS[@]}" "$name.dpr" >"$build_log" 2>&1; then
    echo "COMPILE FAILED"
    sed 's/^/    /' "$build_log"
    compile_fail=$((compile_fail+1))
    FAILED+=("$name (compile)")
    continue
  fi

  if [ "$COMPILE_ONLY" -eq 1 ]; then
    echo "compiled"
    passed=$((passed+1))
    continue
  fi

  # --- run ---
  exe="$BIN_DIR/$name"
  run_log="$BIN_DIR/$name.run.log"
  if [ "$RUN_TIMEOUT" -gt 0 ]; then
    timeout -k 5 "$RUN_TIMEOUT" "$exe" >"$run_log" 2>&1; rc=$?
  else
    "$exe" >"$run_log" 2>&1; rc=$?
  fi

  if [ "$rc" -eq 0 ]; then
    echo "OK"
    [ "$VERBOSE" -eq 1 ] && sed 's/^/    /' "$run_log"
    passed=$((passed+1))
  elif [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
    echo "TIMEOUT (>${RUN_TIMEOUT}s)"
    sed 's/^/    /' "$run_log"
    timed_out=$((timed_out+1))
    FAILED+=("$name (timeout)")
  else
    echo "RUN FAILED (exit $rc)"
    sed 's/^/    /' "$run_log"
    run_fail=$((run_fail+1))
    FAILED+=("$name (exit $rc)")
  fi
done

# --- Summary ---------------------------------------------------------------
echo "=========================================================================="
echo "passed: $passed   compile-failed: $compile_fail   run-failed: $run_fail   timed-out: $timed_out"
if [ ${#FAILED[@]} -gt 0 ]; then
  echo "failures:"
  for f in "${FAILED[@]}"; do echo "  - $f"; done
  exit 1
fi
echo "all good."
exit 0
