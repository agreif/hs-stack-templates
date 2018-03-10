#!/bin/sh

if test $# -eq 0; then
    echo "usage: $0 <passwd> [<passwd>] ..."
    exit 1
fi

stack runhaskell app/passwd.hs $*
