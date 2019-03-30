#!/usr/bin/env bash

sed -i 's/^#\(en_US.UTF-8\)/\1/g' "$(GetPackageOriginalFile glibc /etc/locale.gen)"
echo 'LANG=en_US.UTF-8' > "$(CreateFile /etc/locale.conf)"
sed -i \
  -e 's/^#\(Color\)/\1/g' \
  -e 's/^#\(VerbosePkgLists\)/\1/g' \
  "$(GetPackageOriginalFile pacman /etc/pacman.conf)"
CreateLink /etc/localtime /usr/share/zoneinfo/America/Los_Angeles
