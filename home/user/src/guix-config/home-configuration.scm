;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages gl)
             (gnu packages wm)
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
             (guix utils)
             (guix packages)
             (guix download)
             (guix git-download)
             (gnu home services shells))

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
   (name "dwl")
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
   (name "dwlb")
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
            (list dwl-custom dwlb-custom)))

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
                          ("XDG_CURRENT_DESKTOP"         . "dwl")
                          ("XDG_SESSION_TYPE"            . "wayland")
                          ("WLR_NO_HARDWARE_CURSORS"     . "1")
                          ("WLR_RENDERER_ALLOW_SOFTWARE" . "0")
                          ("ELM_ENGINE"                  . "wayland_egl")
                          ("MOZ_ENABLE_WAYLAND"          . "1")
                          ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                          ("GDK_BACKEND"                 . "wayland")
                          ("SCREENRC"                    . "$HOME/.config/screen/screenrc")))
        (simple-service 'dot-configs-service
                        home-files-service-type
                        `((".config/foot/foot.ini" ,(local-file "dotfiles/foot/foot.ini"))
                          (".local/bin/start-dwl"
                           ,(local-file "dotfiles/dwl/start-dwl.sh" #:recursive? #t))
                          (".local/bin/xwrap"
                           ,(local-file "dotfiles/x11/xwrap.sh" #:recursive? #t))
                          (".local/share/dwl/autostart.sh"
                           ,(local-file "dotfiles/dwl/autostart.sh" #:recursive? #t))
                          (".config/emacs/init.el"
                           ,(local-file "dotfiles/emacs/init.el"))
                          (".config/screen/screenrc"
                           ,(local-file "dotfiles/screen/screenrc")))))))
