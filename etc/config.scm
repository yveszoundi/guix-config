(use-modules
 (gnu)
 (gnu system nss)
 (ice-9 popen)
 (ice-9 rdelim)
 (ice-9 format)
 (guix utils)
 (guix modules))
(use-service-modules networking ssh authentication desktop dbus)
(use-package-modules certs tls
                     text-editors screen emacs
                     linux cryptsetup gnupg disk
                     polkit glib
                     shells curl rsync version-control)

(define %device-partition-efi  "/dev/sda1")
(define %device-partition-root "/dev/sda2")

(define (btrfs-mapped-fs mapped-devices devpath mnts subvols)
  (define (-btrfs-mapped-fs -acc -mapped-devices -devpath -mnts -subvols)
    (if (= 0 (length -mnts))
        -acc
        (let ((-mnt    (car -mnts))
              (-subvol (car -subvols)))
          (-btrfs-mapped-fs
           (cons
            (file-system
             (device -devpath)
             (mount-point -mnt)
             (type "btrfs")
             (options (string-append "subvol=" -subvol))
             (dependencies -mapped-devices))
            -acc)
           -mapped-devices
           -devpath
           (cdr -mnts)
           (cdr -subvols)))))
  (-btrfs-mapped-fs
   '()
   mapped-devices
   devpath
   (reverse mnts)
   (reverse subvols)))

(operating-system
 (locale "en_US.utf8")
 (timezone "America/Toronto")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "oss-guixsd")
 (users
  (append
   (list
    (user-account
     (name "user")
     (comment "user")
     (group "users")
     (home-directory "/home/user")
     (supplementary-groups
      '("wheel" "netdev" "input" "kvm" "cdrom" "audio" "video" "tty"))))
   %base-user-accounts))
 (packages
  (append
   (list
    nss-certs openssl
    mg emacs
    btrfs-progs cryptsetup gnupg dosfstools
    polkit dbus
    screen curl git-minimal rsync)
   %base-packages))
 (services
  (append
   (list
    (service dhcp-client-service-type)
    (service openssh-service-type)
    (service accountsservice-service-type)
    (service elogind-service-type))
   %base-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-removable-bootloader)
   (targets '("/boot/efi"))
   (timeout 5)
   (keyboard-layout keyboard-layout)))
 (mapped-devices
  (list (mapped-device
         (source
          (uuid (let* ((port (open-input-pipe (format #f "blkid -s UUID -o value ~a" %device-partition-root)))
                       (str (read-line port)))
                  (close-pipe port)
                  str)))
         (target "guixsdvm")
         (type luks-device-mapping))))
 (file-systems
  (append
   (btrfs-mapped-fs
    mapped-devices
    "/dev/mapper/guixsdvm"
    '("/"    "/swap" "/boot" "/gnu" "/home" "/var/log" "/data")
    '("root" "swap"  "boot"  "gnu"  "home"  "log"      "data"))
   (cons
    (file-system
     (mount-point "/boot/efi")
     (device
      (uuid (let* ((port (open-input-pipe (format #f "blkid -s UUID -o value ~a" %device-partition-efi)))
                   (str (read-line port)))
              (close-pipe port)
              str)
            'fat32))
     (type "vfat"))
    '())
   %base-file-systems))
 (swap-devices
  (list
   (swap-space
    (target "/swap/swapfile")
    (dependencies (filter (file-system-mount-point-predicate "/swap")
                          file-systems))))))

