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
# re-apply the local patch (WebKitGTK smooth scrolling makes mouse wheel gliding/accelerating)
sed -i 's/    webkit_settings_set_javascript_can_access_clipboard(settings, true);/&\n    \/\/ WebKitGTK enables smooth scrolling by default, which makes mouse wheel\n    \/\/ scrolling glide\/accelerate. Use plain discrete scroll steps instead.\n    webkit_settings_set_enable_smooth_scrolling(settings, false);/' ../../Modules/Webview/src/main/resources/scala-native/webview.h
cd ..
rm -rf webview
