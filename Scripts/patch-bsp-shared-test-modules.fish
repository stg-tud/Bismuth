#!/usr/bin/env fish

# IntelliJ's BSP importer drops the module dependencies of the synthetic
# "shared sources" test modules of a project matrix: it maps a BSP dependency
# target onto the platform module (rdts/rdtsJS/rdtsNative) instead of the
# shared module, and for test modules that edge would close a cycle
# (rdts -> rdts...test (shared) -> rdts), so it is discarded entirely.
# See https://youtrack.jetbrains.com/issue/SCL-23703
#
# This adds the missing (acyclic) edges to the shared main modules by hand.
# It is idempotent, and gets undone by the next BSP reimport.
# Run with the project closed, otherwise IntelliJ overwrites the files again.

argparse n/dry-run -- $argv; or exit 2

set -l modules (path dirname (status filename))/../.idea/modules
if not test -d $modules
    echo "no such directory: $modules" >&2
    exit 1
end

# module to patch, then the module dependencies it should have, tab separated
set -l patches \
    "rdts(-+JS+Native)(test+-)test (shared)\trdts(JS+Native) (shared)" \
    "reactives(-+JS+Native)(test+-)test (shared)\treactives(JS+Native) (shared)" \
    "channels(-+JS+Native)(test+-)test (shared)\tchannels(JS+Native) (shared)\trdts(JS+Native) (shared)\trdts(-+JS+Native)(test+-)test (shared)" \
    "lore(-+JS)(test+-)test (shared)\tloreJS (shared)\treactives(JS+Native) (shared)"

for patch in $patches
    set -l names (string split \t -- (string unescape -- $patch))
    set -l iml $modules/$names[1].iml

    if not test -f $iml
        echo "skipping $names[1], no such module" >&2
        continue
    end

    echo "patching $names[1]"
    set -l content (cat $iml)

    for name in $names[2..]
        if string match -qe -- "module-name=\"$name\"" $content
            echo "  already depends on $name"
            continue
        end
        if set -q _flag_dry_run
            echo "  would add $name"
            continue
        end
        # insert right before the end of the single <component> element
        set content (string replace -r '^  </component>$' \
            "    <orderEntry type=\"module\" module-name=\"$name\" scope=\"TEST\" />\n  </component>" \
            -- $content)
        echo "  added $name"
    end

    set -q _flag_dry_run; or printf '%s\n' $content >$iml
end
