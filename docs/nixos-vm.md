# NixOS VM

Run NixOS in a virtual machine on a Mac. Inspired by
[Mitchell Hashimoto's NixOS config](https://github.com/mitchellh/nixos-config).

1. Install VMWare Fusion: `brew install --cask vmware-fusion`.
1. Download NixOS ISO (minimal 64-bit ARM): <https://nixos.org/download>.
1. Create new VM
    1. Choose "Other Linux 6.x kernel 64-bit Arm" as the operating system type.
    1. Customize settings.
        - "Sharing" > Enable "Enable Shared Folders"
        - "Sharing" > Add host's home folder as shared folder
        - "Default Applications" > Disable opening across operating systems
        - "Keyboard & Mouse" > Duplicate default "Profile" profile, then disable
          all mappings under "Key Mappings" and "Fusion Shortcuts"
        - "Processors & Memory" > Allocate as many cores and as much memory as
          possible
        - "Display" > Enable "Accelerate 3D Graphics"
        - "Display" > "Accelerate 3D Graphics" > Max out "Shared graphics
          memory"
        - "Display" > Enable "Use full resolution for Retina display"
        - "Hard Disk (NVMe)" > Choose a higher max disk size (e.g. 100+ GB)
        - "Sound Card" > Optionally disable "Connect Sound Card" or click
          "Remove Sound Card"
        - "Camera" > Click "Remove Camera"

TODO:

```
$ /Applications/VMware\ Fusion.app/Contents/Library/vmrun getGuestIPAddress ~/Virtual\ Machines.localized/NixOS.vmwarevm/NixOS.vmx -wait
```
