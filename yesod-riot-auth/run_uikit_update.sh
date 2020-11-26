#!/bin/sh

for f in `find . -name 'uikit*'`; do echo "`head -1 $f | awk '{print $3}'` $f"; done

remoteVersion=`curl https://getuikit.com/assets/uikit/dist/js/uikit.js 2>/dev/null | head -1 | awk '{print $3}'`
localVersion=`head -1 static/js/uikit.js | awk '{print $3}'`

echo "local : $localVersion"
echo "remote: $remoteVersion"

if test "$localVersion" = "$remoteVersion"; then
    exit 0
fi

read -p "update? [y/n] " answer

if test "$answer" = "y"; then
    cd static
    filename=uikit-${remoteVersion}.zip
    echo "download $filename..."
    curl -L -O -s https://github.com/uikit/uikit/releases/download/v${remoteVersion}/$filename
    echo "extract..."
    unzip -q -o $filename
    echo "cleanup..."
    rm $filename
    echo "git status..."
    git st . --short
    for f in `find . -name 'uikit*'`; do echo "`head -1 $f | awk '{print $3}'` $f"; done
fi
