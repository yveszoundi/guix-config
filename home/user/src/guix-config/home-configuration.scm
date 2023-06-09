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
             (guix build-system emacs)
             (dwl-guile home-service)
             (dwl-guile patches)
             (dtao-guile home-service))

(define dwl-guile-latest
  (let ((commit "616193f53a4e0c85b278c283a706658ed47d0db3"))
    (package
     (inherit dwl-guile)
     (name "dwl-guile-latest")
     (version (string-append "2.0.0" "-" (string-take commit 8)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/engstrand-config/dwl-guile")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mypdhn4rrg93gal3x6047jxm5gwpk8cs1f0x193ciwr4n1iwmmd")))))))

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

(define-public wbg
  (package
   (name "wbg")
   (version "1.0.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://codeberg.org/dnkl/wbg")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "182cyp97lxwxl5r6f25irvm62ii0j1knmpwlpwa1w00j2xchx89w"))))
   (build-system meson-build-system)
   (arguments
    `(#:build-type "release"))
   (native-inputs
    (list pkg-config wayland-protocols gcc-10 tllist))
   (inputs
    (list wlroots wayland pixman libpng libjpeg-turbo))
   (license license:expat)
   (home-page "https://codeberg.org/dnkl/wbg")
   (synopsis "Super simple wallpaper application for Wayland compositors")
   (description "Super simple wallpaper application for
                              Wayland compositors implementing the layer-shell protocol.")))

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

(define (%tags-and-layout)
  (append
   (map
    (lambda (tag)
      (let ((str (string-append "^p(8)" (number->string tag) "^p(8)"))
            (index (- tag 1)))
        (dtao-block
         (interval 0)
         (events? #t)
         (click `(match button
                        (0 (dtao:view ,index))))
         (render `(cond
                   ((dtao:selected-tag? ,index)
                    ,(string-append "^bg(#ff9933)^fg(#ffffff)" str "^fg()^bg()"))
                   ((dtao:urgent-tag? ,index)
                    ,(string-append "^bg(#ff0000)^fg(#ffffff)" str "^fg()^bg()"))
                   ((dtao:active-tag? ,index)
                    ,(string-append "^bg(#FFFF00)^fg(#000000)" str "^fg()^bg()"))
                   (else ,str))))))
    (iota 9 1))
   (list
    (dtao-block
     (events? #t)
     (click `(dtao:next-layout))
     (render `(string-append "^p(4)" (dtao:get-layout)))))))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 ;; (packages (specifications->packages (list "openssl@3.0.7")))
 (packages (append
            (map specification->package+output
                 '("bemenu" "foot" "neofetch" "icecat"
                   "wlr-randr" "wl-clipboard" "swaybg" "imv"
                   "qtwayland" "lxqt-qtplugin" "jq"  "egl-wayland" "xwininfo"
                   "pcmanfm" "arc-icon-theme" "adwaita-icon-theme"
                   "zathura" "zathura-pdf-mupdf" "pinentry-emacs"
                   "emacs-next-pgtk" "emacs-pinentry" "emacs-systemd-mode"
                   "emacs-move-text" "emacs-dockerfile-mode" "emacs-pinentry"
                   "emacs-dockerfile-mode" "emacs-avy" "emacs-rust-mode"
                   "emacs-xclip" "emacs-markdown-mode" "emacs-jinja2-mode"
                   "emacs-rainbow-mode" "emacs-markdown-mode" "emacs-yaml-mode"))
            (list emacs-rimero-theme wbg rclip-client-cli)))
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
                     ("SCREENRC"                    . "$HOME/.config/screen/screenrc")))
   (simple-service 'dot-configs-service
                   home-files-service-type
                   `((".config/foot/foot.ini" ,(local-file "dotfiles/foot/foot.ini"))
                     (".kshrc" ,(local-file "dotfiles/ksh/kshrc"))
                     (".local/share/dwl-guile/autostart.sh"
                      ,(local-file "dotfiles/dwl/autostart.sh" #:recursive? #t))
                     (".local/bin/xwrap"
                      ,(local-file "dotfiles/x11/xwrap.sh" #:recursive? #t))
                     (".local/share/rclip/der-cert-pub.der"
                      ,(local-file "dotfiles/rclip/der-cert-pub.der"))
                     (".config/rclip/config-client.toml"
                      ,(local-file "dotfiles/rclip/config-client.toml"))
                     (".config/emacs/init.el"
                      ,(local-file "dotfiles/emacs/init.el"))
                     (".config/screen/screenrc"
                      ,(local-file "dotfiles/screen/screenrc"))))
   (service home-dwl-guile-service-type
            (home-dwl-guile-configuration
             (package
              (patch-dwl-guile-package dwl-guile-latest
                                       #:patches (list %patch-xwayland)))
             (native-qt? #t)
             (auto-start? #t)
             (config '())))
   (service home-dtao-guile-service-type
            (home-dtao-guile-configuration
             (auto-start? #t)
             (config
              (dtao-config
               ;; A font string in fcft format.
               (font "monospace:style=bold:size=12")
               ;; Read `root', `border' and `text' colors from dwl-guile.
               (background-color "#331614FF")
               (border-color "333333FF")
               (foreground-color "FFFFFFFF")
               (padding-left 8)
               (padding-right 8)
               (padding-top 2)
               (padding-bottom 2)
               ;; Request an exclusive zone for the bar to prevent overlapping.
               (exclusive? #t)
               ;; Layer to render the bar in (LAYER-TOP, LAYER-BOTTOM, LAYER-OVERLAY, LAYER-BACKGROUND).
               (layer 'LAYER-BOTTOM)
               ;; Render the bar at the bottom of the screen.
               (bottom? #f)
               ;; Height of the bar in pixels. Set to #f for automatic height based on font size.
               (height #f)
               ;; Delimiter string of arbitrary length inserted between blocks.
               (delimiter #f)
               ;; Additional spacing on each side of the delimiter string.
               (block-spacing 0)
               (left-blocks (%tags-and-layout))
               (center-blocks (list
                               (dtao-block
                                (events? #t) ;; Must be enabled to correctly re-render upon event/state change
                                (render `(dtao:title)))))
               (right-blocks
                (list
                 (dtao-block
                  (interval 1)
                  (render `(strftime "%A, %d %b (w.%V) %T" (localtime (current-time)))))))
               ;; List of Guile module dependencies needed to run your blocks.
               (modules '((ice-9 match)
                          (ice-9 popen)
                          (ice-9 rdelim)
                          (srfi srfi-1)))))))
   (simple-service
    'change-dwl-guile
    home-dwl-guile-service-type
    '((setq inhibit-defaults? #t)
      (dwl:set-tty-keys "C-M")
      (set-layouts 'default "[M]"    'dwl:monocle
                   'tile    "[]="    'dwl:tile)
      (set-keys "C-t <return>"       '(dwl:spawn "bemenu-run" "-l" "10")
                "C-t c"              '(dwl:spawn "foot")
                "C-t [62] S-c"       '(dwl:spawn "rclip-client-cli" "--command" "WRITE")
                "C-t [62] S-v"       '(dwl:spawn "rclip-client-cli" "--command" "READ")
                "C-t n"              '(dwl:focus-stack 1)
                "C-t p"              '(dwl:focus-stack -1)
                "C-t ["              '(dwl:change-masters -1)
                "C-t ]"              '(dwl:change-masters 1)
                "C-t [50] S-["       '(dwl:change-master-factor -0.05)
                "C-t [50] S-]"       '(dwl:change-master-factor 0.05)
                "C-t <tab>"          '(dwl:cycle-layout 1)
                "C-t <left>"         '(dwl:focus-monitor 'DIRECTION-LEFT)
                "C-t <right>"        '(dwl:focus-monitor 'DIRECTION-RIGHT)
                "C-t <up>"           '(dwl:focus-monitor 'DIRECTION-UP)
                "C-t <down>"         '(dwl:focus-monitor 'DIRECTION-DOWN)
                "C-t [50] S-<left>"  '(dwl:tag-monitor 'DIRECTION-LEFT)
                "C-t [50] S-<right>" '(dwl:tag-monitor 'DIRECTION-RIGHT)
                "C-t [50] S-<up>"    '(dwl:tag-monitor 'DIRECTION-UP)
                "C-t [50] S-<down>"  '(dwl:tag-monitor 'DIRECTION-DOWN)
                "C-t k"              'dwl:kill-client
                "C-t `"              'dwl:zoom
                "C-t [62] S-e"       'dwl:toggle-fullscreen
                "C-t [62] S-<space>" 'dwl:toggle-floating
                "C-t q"              'dwl:quit
                "C-t <escape>"       'dwl:quit
                "C-<mouse-left>"     'dwl:move
                "C-<mouse-middle>"   'dwl:toggle-floating
                "C-<mouse-right>"    'dwl:resize
                "C-t 1"              '(dwl:view 1)
                "C-t 2"              '(dwl:view 2)
                "C-t 3"              '(dwl:view 3)
                "C-t 4"              '(dwl:view 4)
                "C-t 5"              '(dwl:view 5)
                "C-t 6"              '(dwl:view 6)
                "C-t 7"              '(dwl:view 7)
                "C-t 8"              '(dwl:view 8)
                "C-t 9"              '(dwl:view 9)
                "C-t [62] S-1"       '(dwl:tag 1)
                "C-t [62] S-2"       '(dwl:tag 2)
                "C-t [62] S-3"       '(dwl:tag 3)
                "C-t [62] S-4"       '(dwl:tag 4)
                "C-t [62] S-5"       '(dwl:tag 5)
                "C-t [62] S-6"       '(dwl:tag 6)
                "C-t [62] S-7"       '(dwl:tag 7)
                "C-t [62] S-8"       '(dwl:tag 8)
                "C-t [62] S-9"       '(dwl:tag 9))
      (add-hook! dwl:hook-startup
                 (lambda ()
                   (dwl:spawn (string-append
                               (getenv "XDG_DATA_HOME")
                               file-name-separator-string
                               "dwl-guile"
                               file-name-separator-string
                               "autostart.sh")))))))))
