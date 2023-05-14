(use-modules
 (gnu)
 (gnu system nss)
 (guix utils)
 (gnu services desktop)
 (ice-9 popen)
 (ice-9 rdelim)
 (ice-9 format))
(use-service-modules networking ssh authentication desktop dbus)
(use-package-modules certs shells)

(define %device-partition-efi  "/dev/sda1")
(define %device-partition-root "/dev/sda2")

(operating-system
 (locale "en_US.utf8")
 (timezone "America/Toronto")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "oss-guixvm")
 (users (cons (user-account
               (name "user")
               (comment "user")
               (group "users")
               (home-directory "/home/user")
               (shell #~(string-append #$oksh "/bin/ksh"))
               (supplementary-groups
                '("wheel" "netdev" "input" "kvm" "cdrom" "audio" "video" "tty")))
              %base-user-accounts))
 (packages (append (map specification->package+output
                        '("screen"
                          "oksh"
                          "git-minimal"
                          "rsync"
                          "btrfs-progs"
                          "gnupg"
                          "curl"
                          "polkit"
                          "nss-certs"
                          "dbus"
                          "cryptsetup"
                          "rsync"
                          "openssl"
                          "libusb"
                          "dosfstools"))
                   %base-packages))
 (services (append (list (service dhcp-client-service-type)
                         (service openssh-service-type)
                         (service accountsservice-service-type)
                         (service elogind-service-type))
                   %base-services))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-removable-bootloader)
              (targets '("/boot/efi"))
              (timeout 5)
              (keyboard-layout keyboard-layout)))
 (mapped-devices
  (list (mapped-device
         (source
          (uuid (let* ((port (open-input-pipe (format #f
                                                      "blkid -s UUID -o value ~a"
                                                      %device-partition-root)))
                       (str (read-line port)))
                  (close-pipe port)
                  str)))
         (target "guixsdvm")
         (type luks-device-mapping))))
 (file-systems
  (append
   (map (lambda (item)
             (file-system
              (device "/dev/mapper/guixsdvm")
              (mount-point (car item))
              (type "btrfs")
              (options (car (cdr item)))
              (dependencies mapped-devices)))
           '(("/"        "subvol=root")
             ("/swap"    "subvol=swap")
             ("/boot"    "subvol=boot")
             ("/gnu"     "subvol=gnu")
             ("/home"    "subvol=home")
             ("/var/log" "subvol=log")
             ("/data"    "subvol=data")))
   (list
    (file-system
     (mount-point "/boot/efi")
     (device
      (uuid (let* ((port (open-input-pipe (format #f
                                                  "blkid -s UUID -o value ~a"
                                                  %device-partition-efi)))
                   (str (read-line port)))
              (close-pipe port)
              str)
            'fat32))
     (type "vfat")))
   %base-file-systems))
 (swap-devices
  (list
   (swap-space
    (target "/swap/swapfile")
    (dependencies file-systems)))))

