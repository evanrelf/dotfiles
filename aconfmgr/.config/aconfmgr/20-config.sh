#!/bin/sh

# /etc/systemd/logind.conf
sed -i 's/^#HandlePowerKey=poweroff/HandlePowerKey=suspend/g' \
  "$(GetPackageOriginalFile systemd /etc/systemd/logind.conf)"

# # /etc/systemd/sleep.conf
# sed -i \
#   -e 's/^#SuspendMode=/SuspendMode=suspend/g' \
#   -e 's/^#SuspendState=.*/SuspendState=disk/g' \
#   "$(GetPackageOriginalFile systemd /etc/systemd/sleep.conf)"

# /etc/locale.gen
sed -i 's/^#\(en_US.UTF-8\)/\1/g' \
  "$(GetPackageOriginalFile glibc /etc/locale.gen)"

# /etc/locale.conf
echo 'LANG=en_US.UTF-8' > "$(CreateFile /etc/locale.conf)"

# /etc/pacman.conf
sed -i \
  -e 's/^#\(Color\)/\1/g' \
  -e 's/^#\(VerbosePkgLists\)/\1/g' \
  -e 's/^#\(UseDelta\)/\1/g' \
  "$(GetPackageOriginalFile pacman /etc/pacman.conf)"

# /etc/localtime
CreateLink /etc/localtime /usr/share/zoneinfo/America/Los_Angeles
