(use-modules
 (gnu)
 (gnu services)
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

(define (block-device-uuid device-partition)
  (let* ((port (open-input-pipe (format #f
                                        "blkid -s UUID -o value ~a"
                                        device-partition)))
         (str (read-line port)))
    (close-pipe port)
    str))

(operating-system
 (locale "en_US.utf8")
 (timezone "America/Toronto")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "personal-guix")
 (users (cons (user-account
               (name "user")
               (comment "Guix user")
               (group "users")
               (home-directory "/home/user")
               (shell (file-append oksh "/bin/oksh"))
               (supplementary-groups
                '("wheel" "netdev" "input" "kvm" "cdrom" "audio" "video" "tty")))
              %base-user-accounts))
 (packages (append (map specification->package+output
                        '("screen"
                          "oksh"
                          "git-minimal"
                          "glibc-locales"
                          "rsync"
                          "btrfs-progs"
                          "gnupg"
                          "curl"
                          "polkit"
                          "dbus"
                          "cryptsetup"
                          "rsync"
                          "openssl"
                          "libusb"
                          "dosfstools"))
                   %base-packages))
 (services (append (list (service dhcpcd-service-type
			                      (dhcpcd-configuration
				                   (option    '("rapid_commit" "interface_mtu"))
				                   (no-option '("nd_rdnss" "domain_name"))
				                   (no-hook   '("hostname"))))
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
          (uuid (block-device-uuid %device-partition-root)))
         (target "guixvm")
         (type luks-device-mapping))))
 (file-systems
  (append
   (map (lambda (item)
             (file-system
              (device "/dev/mapper/guixvm")
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
      (uuid (block-device-uuid %device-partition-efi) 'fat32))
     (type "vfat")))
   %base-file-systems))
 (swap-devices
  (list
   (swap-space
    (target "/swap/swapfile")
    (dependencies file-systems)))))


