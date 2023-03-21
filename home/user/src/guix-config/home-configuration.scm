;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages gl)
             (gnu packages tls)
             (gnu packages pkg-config)
             (gnu packages crates-io)
             (gnu packages crates-graphics)
             (gnu packages wm)
             (gnu packages suckless)
             (gnu packages xorg)
             (gnu packages guile)
             (gnu packages admin)
             (gnu packages linux)
             (gnu packages libffi)
             (gnu packages libbsd)
             (gnu packages xdisorg)
             (gnu packages pciutils)
             (gnu packages build-tools)
             (gnu packages freedesktop)
             (gnu services)
             (gnu home services)
             (guix gexp)
             (guix build-system cargo)
             (guix utils)
             (guix licenses)
             (guix packages)
             (guix download)
             (guix git-download)
             (gnu home services shells))

(define rclip-client-cli
  (let ((commit "90c7e9257a61bd346fd6d89db9e8f30dd1eff2e7"))
    (package
     (name "rclip-client-cli")
     (version (string-append "1.0.3" "-" (string-take commit 8)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/yveszoundi/guix-rclip-client-cli-wayland")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xq7r0kggs97mbdwpvldvrqd5vgqck918jfyvzbvr0srjw9v1cir"))))
     (build-system cargo-build-system)
     (arguments
      `(#:cargo-inputs
        (("rust-clap"            ,rust-clap-3)
         ("rust-nix"             ,rust-nix-0.26)
         ("rust-rustls"          ,rust-rustls-0.20)
         ("rust-wayland-sys"     ,rust-wayland-sys-0.28)
         ("rust-wayland-scanner" ,rust-wayland-scanner-0.28)
         ("rust-dirs"            ,rust-dirs-4)
         ("rust-xml-rs"          ,rust-xml-rs-0.8)
         ("rust-wl-clipboard-rs" ,rust-wl-clipboard-rs-0.4)
         ("rust-serde"           ,rust-serde-1)
         ("rust-serde-derive"    ,rust-serde-derive-1)
         ("rust-toml"            ,rust-toml-0.5))))
     (native-inputs
      `(("pkg-config" ,pkg-config)))
     (home-page
      "https://github.com/yveszoundi/rclip")
     (synopsis
      "Share clipboard text over a network.")
     (description
      "Simple clipboard utility for sharing text over a network.")
     (license gpl3+))))

(define libdrm-2.4.113
  (package
   (inherit libdrm)
   (name "libdrm")
   (version "2.4.113")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://dri.freedesktop.org/libdrm/libdrm-"
                  version ".tar.xz"))
            (sha256
             (base32
              "1qg54drng3mxm64dsxgg0l6li4yrfzi50bgj0r3fnfzncwlypmvz"))))))

(define wayland-1.21.0
  (package
   (inherit wayland)
   (name "wayland")
   (version "1.21.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://gitlab.freedesktop.org/wayland/wayland/-/releases/"
                                version "/downloads/" name "-" version ".tar.xz"))
            (sha256
             (base32
              "1b0ixya9bfw5c9jx8mzlr7yqnlyvd3jv5z8wln9scdv8q5zlvikd"))))
   (propagated-inputs
    (list libffi))))

(define wayland-protocols-1.27
  (package
   (inherit wayland-protocols)
   (name "wayland-protocols")
   (version "1.27")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://gitlab.freedesktop.org/wayland/wayland-protocols/-/releases/"
                  version "/downloads/" name "-" version ".tar.xz"))
            (sha256
             (base32
              "0p1pafbcc8b8p3175b03cnjpbd9zdgxsq0ssjq02lkjx885g2ilh"))))
   (inputs
    (modify-inputs (package-inputs wayland-protocols)
                   (replace "wayland" wayland-1.21.0)))))

(define xorg-server-xwayland-22.1.5
  (package
   (inherit xorg-server-xwayland)
   (name "xorg-server-xwayland")
   (version "22.1.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://xorg.freedesktop.org/archive/individual"
                         "/xserver/xwayland-" version ".tar.xz"))
     (sha256
      (base32
       "0whnmi2v1wvaw8y7d32sb2avsjhyj0h18xi195jj30wz24gsq5z3"))))
   (inputs
    (modify-inputs (package-inputs xorg-server-xwayland)
                   (prepend libbsd libxcvt)
                   (replace "wayland" wayland-1.21.0)
                   (replace "wayland-protocols" wayland-protocols-1.27)))))

(define wlroots-0.16.0
  (package
   (inherit wlroots)
   (name "wlroots")
   (version "0.16.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.freedesktop.org/wlroots/wlroots")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "18rfr3wfm61dv9w8m4xjz4gzq2v3k5vx35ymbi1cggkgbk3lbc4k"))))
   (inputs
    (modify-inputs (package-inputs wlroots)
                   (prepend `(,hwdata "pnp"))))
   (propagated-inputs
    (modify-inputs (package-propagated-inputs wlroots)
                   (prepend libdrm-2.4.113)
                   (replace "wayland" wayland-1.21.0)
                   (replace "wayland-protocols" wayland-protocols-1.27)
                   (replace "xorg-server-xwayland" xorg-server-xwayland-22.1.5)))
   (arguments
    (substitute-keyword-arguments
     (package-arguments wlroots)
     ((#:phases phases)
      #~(modify-phases
         #$phases
         (add-after 'unpack 'patch-hwdata-path
                    (lambda* (#:key inputs #:allow-other-keys)
                             (substitute* "backend/drm/meson.build"
                                          (("/usr/share/hwdata/pnp.ids")
                                           (search-input-file inputs "share/hwdata/pnp.ids")))))))))))

(define dwl-custom
  (package
   (inherit dwl)
   (name "dwl-custom")
   (version "0.0.1")
   (inputs
    (modify-inputs (package-inputs dwl)
                   (replace "wlroots" wlroots-0.16.0)))
   (arguments
    `(#:tests? #f
               #:make-flags
               (list
                (string-append "CC=" ,(cc-for-target))
                (string-append "PREFIX=" (assoc-ref %outputs "out")))
               #:phases
               (modify-phases %standard-phases
                              (replace 'configure
                                       (lambda* (#:key inputs outputs #:allow-other-keys)
                                                (substitute* "config.def.h"
                                                             (("vip") (string-append "user")))
                                                #t)))))
   (source
    (origin
     (inherit (package-source dwl))
     (uri (git-reference
           (url "https://github.com/yveszoundi/dwl-customization")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1xv4yc9nmrhrdx1d1zsyji6bn08989qarzzxa30a1yf0rddli79a"))))))

(define dwlb-custom
  (package
   (inherit dwl)
   (name "dwlb-custom")
   (version "0.0.1")
   (inputs
    (append
     (map specification->package+output
          '("pixman" "fcft" "uthash"))
     (list wlroots-0.16.0)))
   (source
    (origin
     (inherit (package-source dwl))
     (uri (git-reference
           (url "https://github.com/yveszoundi/dwlb-customization")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "005v50n81r7wjnb98nzln564aafmx7cmamnzkpvad88f9m4sny8j"))))))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 ;; (packages (specifications->packages (list "openssl@3.0.7")))
 (packages (append
            (map specification->package+output
                 '("bemenu" "foot" "neofetch" "wlr-randr" "icecat"))
            (list dwl-custom dwlb-custom rclip-client-cli)))

 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list (service home-bash-service-type
                 (home-bash-configuration
                  (aliases '(("grep" . "grep --color=auto")
                             ("ll"   . "ls -l")
                             ("mg"   . "mg -n")
                             ("em"   . "emacs -nw")
                             ("ls"   . "ls -p --color=auto")))
                  (bashrc (list (local-file
                                 "dotfiles/bash/bashrc"
                                 "bashrc")))
                  (bash-profile (list (local-file
                                       "dotfiles/bash/bash_profile"
                                       "bash_profile")))))
        (simple-service 'some-useful-env-vars-service
                        home-environment-variables-service-type
                        `(("TERM"                        . "xterm-256color")
                          ("COLORTERM"                   . "xterm-256color")
                          ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                          ("SCREENRC"                    . "$HOME/.config/screen/screenrc")))
        (simple-service 'dot-configs-service
                        home-files-service-type
                        `((".config/foot/foot.ini" ,(local-file "dotfiles/foot/foot.ini"))
                          (".local/bin/start-dwl"
                           ,(local-file "dotfiles/dwl/start-dwl.sh" #:recursive? #t))
                          (".local/share/dwl/autostart.sh"
                           ,(local-file "dotfiles/dwl/autostart.sh" #:recursive? #t))
                          (".local/bin/xwrap"
                           ,(local-file "dotfiles/x11/xwrap.sh" #:recursive? #t))
                          (".local/share/rclip/der-cert-pub.der"
                           ,(local-file "dotfiles/rclip/der-cert-pub.der"))
                          (".config/rclip/config-client.toml"
                           ,(local-file "dotfiles/rclip/config-client.toml"))
                          (".local/bin/rclip-copy"
                           ,(local-file "dotfiles/rclip/rclip-copy.sh" #:recursive? #t))
                          (".local/bin/rclip-paste"
                           ,(local-file "dotfiles/rclip/rclip-paste.sh" #:recursive? #t))
                          (".local/bin/rclip-clear"
                           ,(local-file "dotfiles/rclip/rclip-clear.sh" #:recursive? #t))
                          (".config/emacs/init.el"
                           ,(local-file "dotfiles/emacs/init.el"))
                          (".config/screen/screenrc"
                           ,(local-file "dotfiles/screen/screenrc")))))))
