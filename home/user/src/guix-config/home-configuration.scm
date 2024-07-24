;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages gcc)
             (gnu packages datastructures)
             (gnu packages tls)
             (gnu packages pkg-config)
             (gnu packages perl)
             (gnu packages crates-io)
             (gnu packages crates-tls)
             (gnu packages wm)
             (gnu packages crates-graphics)
             (gnu packages freedesktop)
             (gnu packages xdisorg)
             (gnu packages image)
             (gnu services)
             (gnu home services)
             (gnu home services shells)
             (guix gexp)
             (guix utils)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system cargo)
             (guix build-system meson)
             (guix build-system emacs))

(define emacs-rimero-theme
  (let ((commit "a2e706c2b34f749019979a133f08a2d94a1104b3"))
    (package
     (name "emacs-rimero-theme")
     (version (string-append "0.0.5" "-" (string-take commit 8)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/yveszoundi/emacs-rimero-theme")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kcvvaizggzi7s3dlh611bkirdf6y89kzddc273drdks705s01wh"))))
     (build-system emacs-build-system)
     (home-page "https://github.com/yveszoundi/emacs-rimero-theme")
     (synopsis "A dark emacs theme that is easy on the eyes.")
     (description
      "Theme with a dark background suitable for UI and terminal usage.")
     (license license:gpl3+))))

(define rclip-client-cli
  (let ((commit "35942b1735a307759bfa6ac9eeb07a740044b96a"))
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
                "0q8ly3f0wxkx4bwall6fj3842mb8p2d008h5flcagc5cs69fzh88"))))
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
      `(("perl" ,perl)
        ("pkg-config" ,pkg-config)))
     (home-page
      "https://github.com/yveszoundi/rclip")
     (synopsis
      "Share clipboard text over a network.")
     (description
      "Simple clipboard utility for sharing text over a network.")
     (license license:gpl3+))))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 ;; (packages (specifications->packages (list "openssl@3.0.7")))
 (packages (append
            (map specification->package+output
                 '("bemenu" "foot" "neofetch" "librewolf" "font-awesome"
                   "wlr-randr" "wl-clipboard" "swaybg" "imv" "sway" "waybar"
                   "qtwayland" "lxqt-qtplugin" "jq"  "egl-wayland" "xwininfo"
                   "pcmanfm" "arc-icon-theme" "adwaita-icon-theme"
                   "zathura" "zathura-pdf-mupdf" "pinentry-emacs"
                   "emacs-next-pgtk" "emacs-pinentry" "emacs-systemd-mode"
                   "emacs-move-text" "emacs-dockerfile-mode" "emacs-pinentry"
                   "emacs-dockerfile-mode" "emacs-avy" "emacs-rust-mode"
                   "emacs-xclip" "emacs-markdown-mode" "emacs-jinja2-mode"
                   "emacs-rainbow-mode" "emacs-markdown-mode" "emacs-yaml-mode"))
            (list emacs-rimero-theme rclip-client-cli)))
 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list
   (simple-service 'some-useful-env-vars-service
                   home-environment-variables-service-type
                   `(("TERM"                        . "xterm-256color")
                     ("COLORTERM"                   . "xterm-256color")
                     ("BEMENU_OPTS"                 . "--hf 'ffcc00FF' --tf 'ffcc00FF'")
                     ("WLR_NO_HARDWARE_CURSORS"     . "1")
                     ("ENV"                         . "$HOME/.kshrc")
                     ("WLR_RENDERER_ALLOW_SOFTWARE" . "0")
                     ("SCREENRC"                    . "$HOME/.config/screen/screenrc")
                     ("CLUTTER_BACKEND"             . "wayland")
                     ("QT_QPA_PLATFORM"             . "wayland")
                     ("MOZ_ENABLE_WAYLAND"          . "1")
                     ("XDG_SESSION_TYPE"            . "wayland")
                     ("XDG_SESSION_DESKTOP"         . "sway")
                     ("XDG_CURRENT_DESKTOP"         . "sway")))
   (simple-service 'dot-configs-service
                   home-files-service-type
                   `((".config/foot/foot.ini" ,(local-file "dotfiles/foot/foot.ini"))
                     (".config/sway/config" ,(local-file "dotfiles/sway/config"))
                     (".kshrc" ,(local-file "dotfiles/ksh/kshrc"))                     
                     (".local/bin/xwrap"
                      ,(local-file "dotfiles/x11/xwrap.sh" #:recursive? #t))
                     (".local/share/rclip/der-cert-pub.der"
                      ,(local-file "dotfiles/rclip/der-cert-pub.der"))
                     (".config/rclip/config-client.toml"
                      ,(local-file "dotfiles/rclip/config-client.toml"))
                     (".config/emacs/init.el"
                      ,(local-file "dotfiles/emacs/init.el"))
                     (".config/screen/screenrc"
                      ,(local-file "dotfiles/screen/screenrc")))))))
