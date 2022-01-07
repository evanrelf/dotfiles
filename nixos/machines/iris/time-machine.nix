{ ... }:

/*
=== NOTE ===

Some manual configuration is required to set up Samba users. You can run the
following command to add your user to Samba:

$ sudo smbpasswd -a $(whoami)

You'll be prompted for a password. This password is Samba-specific, but
nothing's stopping you from using your system password.

You can list all Samba users with the following command:

$ sudo pdbedit --list --verbose
*/

{
  services.samba = {
    enable = true;
    enableNmbd = false;
    extraConfig = ''
      winbind use default domain = yes
      vfs objects = acl_xattr fruit streams_xattr
      fruit:metadata = stream
      fruit:veto_appledouble = no
      fruit:posix_rename = yes
      fruit:zero_file_id = yes
      fruit:wipe_intentionally_left_blank_rfork = yes
      fruit:delete_empty_adfiles = yes
    '';
    shares.time-machine = {
      "path" = "/data/time-machine";
      "writeable" = true;
      "fruit:time machine" = true;
    };
  };

  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
    };
    extraServiceFiles.samba = ''
      <?xml version="1.0" standalone='no'?><!--*-nxml-*-->
      <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
      <service-group>
        <name replace-wildcards="yes">%h</name>
        <service>
          <type>_smb._tcp</type>
          <port>445</port>
        </service>
        <!-- Not sure what this is doing -->
        <service>
          <type>_adisk._tcp</type>
          <port>9</port>
          <txt-record>sys=adVF=0x100</txt-record>
          <txt-record>dk0=adVN=time-machine,adVF=0x82</txt-record>
        </service>
        <!-- Pretend to be a rack-mounted Mac Pro for the cool Finder icon -->
        <service>
          <type>_device-info._tcp</type>
          <port>9</port>
          <txt-record>model=MacPro7,1@ECOLOR=226,226,224</txt-record>
        </service>
      </service-group>
    '';
  };
}

/*
=== RESOURCES ===

Introduction to Samba
https://ubuntu.com/server/docs/samba-file-server

smp.conf options:
https://www.samba.org/samba/docs/current/man-html/smb.conf.5.html

fruit module options:
https://www.samba.org/samba/docs/current/man-html/vfs_fruit.8.html

Recommendations for improving macOS support:
https://wiki.samba.org/index.php/Configure_Samba_to_Work_Better_with_Mac_OS_X
https://jansblog.org/2021/05/16/samba-based-timemachine-with-big-sur/
https://fy.blackhats.net.au/blog/html/2021/03/22/time_machine_on_samba_with_zfs.html

Docker image that configures Samba and Avahi for you:
https://github.com/mbentley/docker-timemachine
*/
