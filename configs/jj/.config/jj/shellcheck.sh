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

for key in $(echo "$map" | jq --raw-output 'keys[]'); do
  temp=$(mktemp -t "$key")
  echo "$map" | jq --raw-output ".\"$key\"" > "$temp"
  shellcheck "$temp"
done
