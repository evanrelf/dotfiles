#!/usr/bin/env bash

set -Eeuo pipefail
IFS=$'\n\t'

map=$(
  cat "$HOME/.config/jj/config.toml" \
| yj -tj \
| jq --raw-output '
      .aliases
    | with_entries(.value = first(.value[] | select(contains("#!/usr/bin/env bash"))))
  ' \
)

temp=$(mktemp -d)
echo "+ cd $temp"
cd "$temp"
for key in $(echo "$map" | jq --raw-output 'keys[]'); do
  echo "$map" | jq --raw-output ".\"$key\"" > "jj-$key"
  (set -x; shellcheck "jj-$key")
done
