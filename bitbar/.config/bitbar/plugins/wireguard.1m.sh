#!/usr/bin/env bash

export PATH="/usr/local/bin:$PATH"

interface="mullvad-us2"
# cache_prefix="$(dirname "$0")/../cache"

case "$1" in
  connect )
    sudo wg-quick up $interface
    ;;
  disconnect )
    sudo wg-quick down $interface
    ;;
esac

if wg show &>/dev/null; then
  echo "Not Secure | color=red dropdown=false"
  echo "---"
  echo "Disconnected"
  echo "Connect to $interface | bash='$0' param1=connect terminal=false refresh=true"
  echo "---"

  # if [ -e "${cache_prefix}_connected.json" ]; then
  #   rm "${cache_prefix}_connected.json"
  # fi

  # if [ ! -e "${cache_prefix}_disconnected.json" ]; then
  #   curl --silent --max-time 5 ifconfig.co/json -o "${cache_prefix}_disconnected.json" \
  #   || curl --silent --max-time 5 ifconfig.me -o "${cache_prefix}_disconnected.json"
  # fi

  # jq -r ".ip, .city, .country" "${cache_prefix}_disconnected.json" 2>/dev/null \
  # || cat "${cache_prefix}_disconnected.json"
else
  echo "Secure | dropdown=false"
  echo "---"
  echo "Connected to $interface"
  echo "Disconnect | bash='$0' param1=disconnect terminal=false refresh=true"
  echo "---"

  # if [ -e "${cache_prefix}_disconnected.json" ]; then
  #   rm "${cache_prefix}_disconnected.json"
  # fi

  # if [ ! -e "${cache_prefix}_connected.json" ]; then
  #   curl --silent --max-time 5 ifconfig.co/json -o "${cache_prefix}_connected.json" \
  #   || curl --silent --max-time 5 ifconfig.me -o "${cache_prefix}_connected.json"
  # fi

  # jq -r ".ip, .city, .country" "${cache_prefix}_connected.json" 2>/dev/null \
  # || cat "${cache_prefix}_connected.json"
fi

