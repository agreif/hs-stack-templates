#!/bin/sh

which yesod
if test $? -ne 0; then
    stack build yesod-bin
fi

stack clean

PGUSER=yesod-riot-auth \
    PGPASS=yesod-riot-auth \
    PGDATABASE=yesod-riot-auth \
    stack exec -- yesod devel
