#!/usr/bin/env bash

run_suite () {
  local TEST_FILE=$1
  local PASSES=$2
  local ENCODER=$3
  local DECODER=$4
  local ENCODER_OPTS=$5
  local DECODER_OPTS=$6
  
  echo "*** Running encoder $ENCODER for suite $TEST_FILE ($PASSES times)"
  touch temp.bin
  for i in {1..5}; do
    local TIME_SNAP=$(time $ENCODER $ENCODER_OPTS $TEST_FILE temp.bin 2>&1)
    echo $TIME_SNAP 2>&1
  done

  echo "*** Running decoder $DECODER for suite $TEST_FILE ($PASSES times)"
  for i in {1..5}; do
    local TIME_SNAP=$(time $DECODER $DECODER_OPTS temp.bin temp2.dat)
#    if [[ -n temp2.dat -ef temp.bin ]]; then
#      echo "Warning: file content mismatch"
#    fi
    echo $TIME_SNAP
  done
}

run_suite pan-tadeusz.txt 5 "./AdaptiveArithmeticCompress" "./AdaptiveArithmeticDecompress" "" ""
run_suite zeros.1m  5 "./AdaptiveArithmeticCompress" "./AdaptiveArithmeticDecompress" "" ""
run_suite random.1m 5 "./AdaptiveArithmeticCompress" "./AdaptiveArithmeticDecompress" "" ""
run_suite pan-tadeusz.txt 5 "ac-haskell" "ac-haskell" "encode" "decode"
run_suite zeros.1m  5 "ac-haskell" "ac-haskell" "encode" "decode"
run_suite random.1m 5 "ac-haskell" "ac-haskell" "encode" "decode"
