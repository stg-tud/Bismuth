#!/usr/bin/env fish
cd target
pwd
rm -rf webview
git clone https://github.com/webview/webview.git
cd webview
pwd
git switch --detach cbbdee44afff22867de9fd88a9fc8350d9bdd399
set -x TMPDIR (pwd)/.amalg-tmp
mkdir -p $TMPDIR
python3 scripts/amalgamate/amalgamate.py --base core --search include --output ../../Modules/Webview/src/main/resources/scala-native/webview.h src
cd ..
rm -rf webview
