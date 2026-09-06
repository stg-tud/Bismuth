#!/usr/bin/env fish
cd target
pwd
rm -rf webview
git clone https://github.com/webview/webview.git
cd webview
pwd
git switch --detached f1a9d6b6fb8bcc2e266057224887a3d628f30f90
python3 scripts/amalgamate/amalgamate.py --base core --search include --output ../../Modules/Webview/src/main/resources/scala-native/webview.h src
cd ..
rm -rf webview
