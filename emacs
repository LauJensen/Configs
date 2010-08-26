
                                        ;=== Serve client calls

(server-start)

;;;;;;;;;;;;;;; LOAD PATH AND AUTOLOADS

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/twit")
(add-to-list 'load-path "~/.emacs.d/gist.el/")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/emacs-w3m")
(add-to-list 'load-path "~/.emacs.d/magit/")
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(add-to-list 'load-path "~/.emacs.d/slime/contrib/")
(add-to-list 'load-path "~/coding/clojure-root/clojure-mode/")

(require 'wl)
(require 'org)
(require 'erc)
(require 'ido)
(require 'gist)
(require 'magit)
(require 'mime-w3m)
(require 'org-mime)
(require 'yasnippet)
(require 'color-theme)
(require 'clojure-mode)

; Initialize yas for use with draft-mode (hook later)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")

;=== GLOBALS

(ido-mode 'both)  ; User ido mode for both buffers and files
(setq backup-directory-alist (list (cons ".*" (expand-file-name "~/.emacsbackup/")))) ; Temp files
(setq x-select-enable-clipboard t) ; Integrate with X11s clipboard
(set-default-font "-bitstream-Bitstream Vera Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(global-font-lock-mode 1) ;; Enable syntax highlighting when editing code.
(show-paren-mode 1) ; Highlight the matching paren
(tool-bar-mode -1)  ; Remove bloat
(menu-bar-mode -1)  ; --- || ---
(setq transient-mark-mode t) ; Highlight selected regions
(setq visible-bell t)        ; Flash program border on beep
(setq inhibit-startup-screen t) ; Dont load the about screen on load
(setq scroll-step 1)            ; Only scroll down 1 line at a time
(setq-default indent-tabs-mode nil) ; Dont indent with tabs
(column-number-mode t) ; Show cursors X + Y coordinates in modeline
(setq c-basic-offset 4) ; Indenting is 4 spaces
(set-language-environment "UTF-8");"Latin-1") ; Default would be utf8
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "/usr/bin/conkeror")

;=== THEMES

; Custom theme by etate
(defun color-theme-dark-bliss ()
  (interactive)
  (color-theme-install
   '(color-theme-dark-bliss
     ((foreground-color . "#eeeeee")
      (background-color . "#001122")
      (background-mode . dark)
      (cursor-color . "#ccffcc"))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (default ((t (nil))))

     (font-lock-builtin-face ((t (:foreground "#f0f0aa"))))
     (font-lock-comment-face ((t (:italic t :foreground "#aaccaa"))))
     (font-lock-delimiter-face ((t (:foreground "#aaccaa"))))
     (font-lock-constant-face ((t (:bold t :foreground "#ffaa88"))))
     (font-lock-doc-string-face ((t (:foreground "#eeccaa"))))
     (font-lock-doc-face ((t (:foreground "#eeccaa"))))
     (font-lock-reference-face ((t (:foreground "#aa99cc"))))
     (font-lock-function-name-face ((t (:foreground "#ffbb66"))))
     (font-lock-keyword-face ((t (:foreground "#ccffaa"))))
     (font-lock-preprocessor-face ((t (:foreground "#aaffee"))))
     (font-lock-string-face ((t (:foreground "#bbbbff")))))))

; Custom theme by etate (beta)
(defun sample-light ()
  (interactive)
  (color-theme-install
   '(sample-light
      ((background-color . "#ffffff")
      (background-mode . light)
      (border-color . "#ffffff")
      (cursor-color . "#ffadad")
      (foreground-color . "#000000")
      (mouse-color . "black"))
     (fringe ((t (:background "#ffffff"))))
     (mode-line ((t (:foreground "#4f4f4f" :background "#ffffff"))))
     (region ((t (:background "#e8e8e8"))))
     (font-lock-builtin-face ((t (:foreground "#9853f3"))))
     (font-lock-comment-face ((t (:foreground "#3d3d3d"))))
     (font-lock-function-name-face ((t (:foreground "#ff4242"))))
     (font-lock-keyword-face ((t (:foreground "#1241de"))))
     (font-lock-string-face ((t (:foreground "#8f8f8f"))))
     (font-lock-type-face ((t (:foreground"#3d3d3d"))))
     (font-lock-variable-name-face ((t (:foreground "#934ab5"))))
     (minibuffer-prompt ((t (:foreground "#295dff" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))

;= Not all themes update all things
(color-theme-initialize)
(color-theme-sitaramv-nt)
(color-theme-dark-bliss)
(sample-light)
(color-theme-charcoal-black)


; TWIT

(autoload 'twit-show-recent-tweets	"twit" "" t) ; most recent direct tweets (!)
(autoload 'twit-show-at-tweets		"twit" "" t) ; directed to you
(autoload 'twit-show-friends 		"twit" "" t) ; your friends
(autoload 'twit-show-followers 		"twit" "" t) ; your followers

(autoload 'twit-follow-recent-tweets	"twit" "" t) ; at idle, check at background

(autoload 'twit-post			"twit" "" t)
(autoload 'twit-post-region		"twit" "" t)
(autoload 'twit-post-buffer		"twit" "" t)
(autoload 'twit-direct			"twit" "" t) ; tweet to person

(autoload 'twit-add-favorite		"twit" "" t) ; Add to favourite: (*) star
(autoload 'twit-remove-favorite 	"twit" "" t)

(autoload 'twit-add-friend  		"twit" "" t) ; follow a friend
(autoload 'twit-remove-friend 		"twit" "" t) ; emove a frienda

(setq twit-show-user-images t)
(setq twit-user-image-dir "~/.emacs.d/twit/")

;=== ERC

;; Enable logging
(custom-set-variables
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track))))
(custom-set-faces)

(setq erc-log-channels-directory "~/.irclogs/logs/")
(setq erc-save-buffer-on-part t)

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

;; Only track my nick(s)
(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(setq erc-keywords '("Lau " "lau " "Lau:" "lau:" "ljensen"))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(setq erc-fill-static-center 15)
(setq erc-fill-column 85)

; in a discussion about highlighting new query windows:
;<bojohan> LauJensen: i would hook into erc-track, using
;	  erc-track-list-changed-hook


; Notify on highlights
(defun clean-message (s)
  (setq s (replace-regexp-in-string "'" "&apos;"
  (replace-regexp-in-string "\"" "&quot;"
  (replace-regexp-in-string "&" "&amp;"
  (replace-regexp-in-string "<" "&lt;"
  (replace-regexp-in-string ">" "&gt;" s)))))))

(defun call-libnotify (matched-type nick msg)
  (let* ((cmsg  (split-string (clean-message msg)))
	(nick   (first (split-string nick "!")))
	(msg    (mapconcat 'identity (rest cmsg) " ")))
    (shell-command-to-string
     (format "notify-send -t 10000 -i /usr/share/icons/hicolor/scalable/apps/emacs.svg -u critical '%s says:' '%s'" nick msg))))

(add-hook 'erc-text-matched-hook 'call-libnotify)

; Be overly eager to identify URLs
(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#archlinux" "#conkeror" "#clojure")))

;=== WANDERLUST

(load-file "~/.emacs.d/wl-init.el")

(setq display-time-mail-function
      '(lambda () wl-modeline-biff-status))

(defun mail ()
  (interactive)
  (wl)
  (my-wl-biff))

(setq elmo-imap4-default-server "your.imap.server"
      elmo-imap4-default-user "imap.username"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-use-modified-utf7 nil)

;;; Hit f to view link in default-browser
(add-hook 'mime-view-mode-hook (lambda () (local-set-key "f" 'browse-url)))
;
;(add-hook 'draft-mode-hook (lambda () (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
;(add-hook 'org-mode-hook   (lambda () (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

;;; ORG-MODE

(require 'remember)

;; Connect with org-mode
(org-remember-insinuate)
;;
;
;; Templates for Notes
(setq org-remember-templates
 '(("Note" ?n "* NOTE %?\n%i\n%a" "~/Organize/notes.org" "Notes")
   ("Download" ?d "* DL %?\n%i\n%a" "~/Organize/notes.org" "Download")
   ("Login" ?l "* LOGIN %?\n%i\n%a" "~/Organize/notes.org" "Logins")
   ("Idea" ?i "* %^{Title}\n%i\n%a" "~/Organize/notes.org" "Brainstorm")
   ("Clipboard" ?c "* %^{Description} %T\n%x" "~/Organize/notes.org" "Clipboard")))

;; Remember frames
;;   - $ emacsclient -e '(make-remember-frame)'
;;
;; Org-remember splits windows, force it to a single window
(add-hook 'remember-mode-hook  'delete-other-windows)

;; Automatic closing of remember frames
(defadvice remember-finalize (after delete-remember-frame activate)
  "Advise remember-finalize to close the frame if it is the remember frame"
  (if (equal "*Remember*" (frame-parameter nil 'name))
    (delete-frame))
)
(defadvice remember-destroy (after delete-remember-frame activate)
  "Advise remember-destroy to close the frame if it is the remember frame"
  (if (equal "*Remember*" (frame-parameter nil 'name))
    (delete-frame))
)

;; Initialization of remember frames
(defun make-remember-frame ()
  "Create a new frame and run org-remember"
  (interactive)
  (make-frame '((name . "*Remember*") (width . 80) (height . 10)))
  (select-frame-by-name "*Remember*")
  (org-remember)
)


(setq org-directory "~/Organize/")
(setq org-default-notes-file "~/Organize/notes.org")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map "\C-cr" 'org-remember)


;; from: http://emacs-fu.blogspot.com/search/label/org-mode

(defun djcb-remember-frame ()
  "turn the current frame into a small popup frame for remember mode;
this is meant to be called with
     emacsclient -c -e '(djcb-remember-frame)'"
  (modify-frame-parameters nil
    '( (name . "_Remember_") ;; must be same as in mode-hook below
       (width .  80)
       (height . 20)
       (vertical-scroll-bars . nil)
       (menu-bar-lines . nil)
       (tool-bar-lines . nil)))
  (org-remember)
  (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only....
  (delete-other-windows))

;; when we're in such a remember-frame, close it when done.
(add-hook 'org-remember-mode-hook
  (lambda()
    (define-key org-remember-mode-map (kbd "C-c C-c")
      '(lambda()(interactive)
         (let ((remember-frame-p
                 (string= (frame-parameter nil 'name) "_Remember_")))
           (when remember-frame-p (make-frame-invisible))  ;; hide quickly

           (org-remember-finalize)
           (when remember-frame-p (delete-frame)))))))

(setq org-agenda-files '("~/Organize/"))  ; Where Org-mode checks for TODO items
(setq org-todo-keywords
      '((sequence "A" "B" "C" "TODO" "DONE" "WAITING" "DELEGATED")))

(setq org-todo-keyword-faces  ; Pretty rendering
      '(("A" . org-warning) ("B" . (:foreground "orange")) ("C" . (:foreground "blue"))))

(setq org-hide-leading-stars t) ; leading stars are colored black

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;=== CLOJURE

(setq swank-clojure-jar-path "~/.swank-clojure/swank-clojure-1.0.jar"
      swank-clojure-jar-home "~/.swank-clojure/"
      swank-clojure-classpath
      (list
       "~/.swank-clojure/*"
       "~/coding/lisp/tools/enlive/src"
       "~/coding/lisp/tools/penumbra/src"
       "~/coding/lisp/tools/penumbra/lib/*"
       "~/coding/lisp/tools/criterium/src/"
       "~/coding/lisp/tools/clj-processing/src/"
       "~/coding/lisp/clojure/libs/*"
       "~/coding/lisp/projects/clojureql/src"
       "~/coding/lisp/projects/clojureql/api/src"
       "~/coding/lisp/projects/clojureql/demos/src"
       "~/coding/lisp/projects/clojureql/backends/mysql/src"
       "~/coding/lisp/projects/clojureql/backends/derby/src"
       "~/coding/lisp/projects/clojureql/backends/postgres/src")
      swank-clojure-extra-vm-args
      (list
       "-Xms1024m"
       "-Xmx1024m"
       "-server"
       "-Dcom.sun.management.jmxremote"
       "-Djava.library.path=/home/lau/coding/lisp/tools/penumbra/native/linux/x86_64/"))

;;;;;;;;;;;;;;; (can be overridden by other modes)

(defun go-back ()
  " When editing code, this function will store your current buffer config
    (location, size, cursor pos, etc) and switch to a buffer named #clojure.
    It then rebinds F12, so that if you hit it again, you'll be taken back
    to the stored buffer configuration. "
  (interactive)
  (jump-to-register 9)
  (global-set-key [f12] 'go-to-clojure))

(defun go-to-clojure ()
    " When editing code, this function will store your current buffer config
    (location, size, cursor pos, etc) and switch to a buffer named #clojure.
    It then rebinds F12, so that if you hit it again, you'll be taken back
    to the stored buffer configuration. "
  (interactive)
  (window-configuration-to-register 9)
  (switch-to-buffer "#clojure")
  (delete-other-windows)
  (global-set-key [f12] 'go-back))

(defun irc-connect ()
  " I forgot if this only works with an erc.el file I hacked myself. Normally
    you are always prompted for all options "
  (interactive)
  (setq psw (read-passwd "Password: "))
  (erc :server "localhost"        :port 6667 :nick "lau" :password psw)
  (erc :server "irc.freenode.net" :port 6667 :nick "LauJensen" :password psw))

;=== GLOBALS KEY BINDINGS

(global-set-key (kbd "C-z") 'set-mark-command)
(global-set-key [C-tab] 'other-window)
(global-set-key "\r" 'newline-and-indent)

(global-set-key [f1] 'irc-connect)
(global-set-key [f2] 'magit-status)
(global-set-key [f3] 'twit-show-at-tweets)
(global-set-key [f4] 'open-and-run-lisp)
(global-set-key [f5] 'start-web-dev)
(global-set-key [f6] 'slime-eval-buffer)
(global-set-key [f12] 'go-to-clojure)

(global-set-key (kbd "C-M-p") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-o") 'shrink-window-horizontally)

(global-set-key (kbd "C-.") 'find-tag)

;=== HOOKS

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'wl-init-hook 'mime-w3m-insinuate)

(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)
             (define-key clojure-mode-map "\C-x\C-e" 'lisp-eval-last-sexp)
             (local-set-key (kbd "<s-tab>") 'slime-eval-defun)
             (local-set-key (kbd "M-RET") 'dabbrev-expand)))

(setq auto-mode-alist
      (append '(("\.lisp$" . lisp-mode)
                ("\.lsp$" . lisp-mode)
                ("\.cl$" . lisp-mode)
                ("\\.js$" . javascript-mode)
                ("\\.java$" . java-mode)
                ("SConstruct$" . python-mode)
                ("\.py$" . python-mode)
                ("\.asd$" . lisp-mode)
                ("\.system$" . lisp-mode)
                ("\\.org$" . org-mode)
                ("\\.mbox$" . vm-mode)
                ("\\.muse$" . muse-mode)
                ("\\.clj$" . clojure-mode))
              auto-mode-alist))

(eval-after-load "slime"
  '(progn
     ;; SBCL
     ;; (add-to-list 'slime-lisp-implementations
     ;;              '(sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix))

     ;; Fuzzy-Completion
     (require 'slime-fuzzy)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (slime-setup)))

;=== HELPER FUNCTIONS

(defvar find-file-root-prefix "/sudo:root@localhost:"
  "The prefix of root user use in Emacs.")

(defun find-file-root (file)
  "Find file with root."
  (interactive "fFind file as sudo: ")
  (find-file (concat find-file-root-prefix file)))

(defun find-file-smb(file)
  "Access file through samba protocol."
  (interactive "fFind file as samba: ")
  (find-file (concat "/smb:" file)))

(defun start-web-dev (file)
  " Find your .el file to launch Jetty, and eval it "
  (interactive "FFile: ")
  (find-file file)
  (eval-buffer))

; Simple macro that lets me select a region, hit M-x blog
; and then the selection is htmlized calling 'my-htmlize' and put
; into a pre-tag with inlined CSS
(fset 'blog
   (lambda (&optional arg) "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item
      (quote ([134217848 109 121 tab return 26 134217790 23 24 111] 0 "%d")) arg)))

(if (not (fboundp 'switch-to-other-buffer))
;; Code stolen Xemacs' files.el
(defun switch-to-other-buffer (arg)
  "Switch to the previous buffer.  With a numeric arg, n, switch to the nth
most recent buffer.  With an arg of 0, buries the current buffer at the
bottom of the buffer stack."
  (interactive "p")
  (if (eq arg 0)
      (bury-buffer (current-buffer)))
  (switch-to-buffer
   (if (<= arg 1) (other-buffer (current-buffer))
     (nth (1+ arg) (buffer-list)))))
)

(if (not (fboundp 'next-buffer))
    (defun next-buffer ()
  "Switch to the next buffer in cyclic order."
  (interactive)
  (switch-to-other-buffer 0)))

(if (not (fboundp 'previous-buffer))
     (defun previous-buffer ()
  "Switch to the previous buffer in cyclic order."
  (interactive)
  (while (string-match "\\` "
                       (buffer-name (switch-to-other-buffer
                                     (- (length (buffer-list)) 2)))))))

(global-set-key (kbd "M-p") 'next-buffer)
(global-set-key (kbd "M-o") 'previous-buffer)

;=== Prepare window

(when window-system
	(set-frame-height (selected-frame) 50)
	(set-frame-width (selected-frame) 100))

;=== Load ELPA and the projects therein (swank+slime)

(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))

