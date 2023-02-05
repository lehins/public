#!/bin/sh

# Scripts tat produce multiple lines can be defined and executed like so:

HS_SCRIPT=`cat <<EOF
main = putStrLn "Hello\nWorld"
EOF
`
runhaskell <<< "${HS_SCRIPT}"

# Single line output scripts can be defined and executed like so:

echo `runhaskell <<EOF
main = putStrLn "Hello World"
EOF
`
