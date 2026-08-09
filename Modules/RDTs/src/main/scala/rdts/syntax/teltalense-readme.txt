This module is based on quicklens.
To make the quicklens history available use the following:


# add the quicklens remote
git remote add quicklens git@github.com:softwaremill/quicklens.git

# fetch the commit we want to merge
git fetch quicklens d4c24cab20067b393985caf129c41c70f40a3640

# go to the starting point in this repository
git checkout d269a46d0138c2799f18b9168f23e2a95f415105

# potentially you need to clear the path
# rm -r Modules/Deltalens

# make the subtree merge commit deterministic
export GIT_AUTHOR_NAME="subtree"
export GIT_AUTHOR_EMAIL="subtree@localhost"
export GIT_AUTHOR_DATE="2024-06-02T13:22:35+02:00"
export GIT_COMMITTER_NAME="subtree"
export GIT_COMMITTER_EMAIL="subtree@localhost"
export GIT_COMMITTER_DATE="2024-06-02T13:22:35+02:00"

# recreate the subtree merge
# (same pinned env vars + same inputs => same commit hash every time)
git subtree add --prefix="Modules/Deltalens" d4c24cab20067b393985caf129c41c70f40a3640

# store the generated ID somewhere (with the env vars above this is stable/reproducible)
set -l merged (git rev-parse HEAD)

# graft replace the history less merge with the subtree version
git replace --graft 30fbc6544bd803b894efc3bf993cd84705a92e6b b9155b94ee6a4a12b363a8d785fe41360e980ee6
