;;; init-wl.el --- Configuration file for the Wanderlust Email client
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;; Maintainer: Henry G. Weller
;;
;; Created: Thu Jul 17 09:49:48 2008 (+0100)
;; Version: 0.1
;; Last-updated: Sun Apr 12 15:41:18 2009 (+0100)
;;           By: Henry Weller
;;     Update #: 3
;; URL: http://www.emacswiki.org/emacs/hgw-init-wl.el
;; Keywords: convenience
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; ----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; This is my complete configuration file for Wanderlust, the excellent Email
;; client, which has been cobbled together from the examples and documentation
;; supplied with Wanderlust, reading the source-code, and scanning the mailing
;; lists wl-en@lists.airs.net, wl@lists.airs.net and of course from the web.
;; Mailing list postings by Ron Isaacson, Vitaly Mayatskikh, Yoichi NAKAYAMA
;; have been particularly useful as have configuration files posted on the web
;; by Jared Rhine, Angus Lees and YAMASHITA Junji.
;;
;; ----------------------------------------------------------------------------
;;
;;; Change log:
;;
;; Version 0.1
;; * Initial release
;;
;; ----------------------------------------------------------------------------
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ----------------------------------------------------------------------------
;;
;;; Code:

;;; Package paths
(add-to-list 'load-path            (expand-file-name "/usr/share/emacs/site-lisp/wl"))
(add-to-list 'load-path            (expand-file-name "/usr/share/emacs/site-lisp/semi"))
(add-to-list 'load-path            (expand-file-name "/usr/share/emacs/site-lisp/flim"))
(add-to-list 'load-path            (expand-file-name "/usr/share/emacs/site-lisp/apel"))
(add-to-list 'load-path            (expand-file-name "/usr/share/emacs/site-lisp/bbdb"))
(add-to-list 'load-path            (expand-file-name "/usr/share/emacs/23.2/lisp/mail/"))

;; ----------------------------------------------------------------------------
;;; Autoload Wanderlust on command "wl"

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" "Compose with Wanderlust." t)

;; ----------------------------------------------------------------------------
;;; w3m octet configuration for handling attachments

(setq org-mime-library 'semi)
(setq default-mime-charset-for-write 'utf-8)  ;'iso-8859-1)
(setq default-mime-charset           'utf-8)  ;'iso-8859-1)

(add-to-list 'load-path             (expand-file-name "~/.emacs.d/emacs-w3m"))
(require 'octet)
(octet-mime-setup)

;(mel-use-module 'mel-q '("quoted-printable" "Q"))
;(mel-use-module 'mel-q-ccl '("quoted-printable" "Q")))
; TODO: Hook these guys into the mail-mode

(setq mel-b-ccl-module nil)
(setq mel-q-ccl-module nil)
(setq base64-external-encoder '("mimencode"))
(setq base64-external-decoder '("mimencode" "-u"))
(setq base64-external-decoder-option-to-specify-file '("-o"))
(setq quoted-printable-external-encoder '("mimencode" "-q"))
(setq quoted-printable-external-decoder '("mimencode" "-q" "-u"))
(setq quoted-printable-external-decoder-option-to-specify-file '("-o"))
(setq base64-internal-decoding-limit 0)
(setq base64-internal-encoding-limit 0)
(setq quoted-printable-internal-decoding-limit 0)
(setq quoted-printable-internal-encoding-limit 0)

(setq-default mime-transfer-level 8)
(setq mime-header-accept-quoted-encoded-words t)



;; ----------------------------------------------------------------------------
;;; Basic configuration

(setq user-full-name "YOUR NAME"
      user-mail-address "YOUR EMAIL ADDRESS")

(setq ;wl-icon-directory "~/.emacs.d/packages/wanderlust/etc/icons"
      ;; Offline and synchronization
      wl-plugged t
      elmo-imap4-use-modified-utf7 t
      elmo-imap4-use-cache t
      elmo-nntp-use-cache t
      elmo-pop3-use-cache t
      wl-ask-range nil

      elmo-message-fetch-confirm nil
      ;elmo-message-fetch-threshold 250000
      ;;elmo-network-session-idle-timeout 30

      wl-fcc ".sent"
      wl-fcc-force-as-read t
      wl-from (concat user-full-name " <" user-mail-address ">")
      wl-organization "YOUR COMPANY"

      wl-summary-incorporate-marks '("N" "U" "!" "A" "F" "$")  ;fetch all marks

      ;; Automatic signature insertion
					; signature-insert-at-eof t
      ; signature-delete-blank-lines-at-eof t

      ;; User Email addresses
      wl-user-mail-address-list nil

      wl-draft-reply-buffer-style 'keep
      ;wl-interactive-send nil
      ;wl-interactive-exit nil

      ;; Windows and decoration
      wl-folder-use-frame nil
      wl-highlight-body-too t
      wl-use-highlight-mouse-line nil
      wl-show-plug-status-on-modeline t
      wl-message-window-size '(1 . 2)
      )

;; Set mail-icon to be shown universally in the modeline.
(setq global-mode-string
      (cons
       '(wl-modeline-biff-status
         wl-modeline-biff-state-on
         wl-modeline-biff-state-off)
       global-mode-string))

;; Use wanderlust for default compose-mail
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; ----------------------------------------------------------------------------
;;; Folders

(setq wl-stay-folder-window t
      wl-folder-window-width 30
      wl-folder-desktop-name "Email"
      wl-default-folder ".inbox"
      my-wl-default-filing-folder ".Archive"
      wl-default-spec "%"  ; TODO: Leverage this
      wl-draft-folder ".draft"  ; TODO: draft(s) ?
      wl-trash-folder ".trash"
      wl-queue-folder ".queue"
      wl-interactive-save-folders nil

      ;;wl-dispose-folder-alist '(("^." . remove))
      wl-use-petname t
      wl-folder-petname-alist nil
      wl-fldmgr-make-backup  t
      wl-fldmgr-sort-group-first  t

      elmo-folder-update-confirm t
      elmo-folder-update-threshold 1000

      wl-folder-check-async  t

      wl-folder-notify-deleted t
      wl-fldmgr-add-complete-with-current-folder-list t
      wl-folder-info-save t
      wl-folder-many-unsync-threshold  100
      wl-highlight-folder-by-numbers 1

      )

;; ----------------------------------------------------------------------------
;;; Local mail directory path

;(setq my-maildir-path "~/Maildir"
;      wl-folders-file (concat my-maildir-path "/folders")
;      elmo-localdir-folder-path my-maildir-path
;      elmo-maildir-folder-path my-maildir-path
;      elmo-search-namazu-default-index-path my-maildir-path
;      elmo-archive-folder-path my-maildir-path)

;; BUILD the folder tree automatically
;; Note: if you change the hierarchy and want to rebuild the tree do
;; rm -rf ~/Emacs/Wanderlust/Elmo/folder
(setq wl-folder-hierarchy-access-folders
      '("^.\\([^/.]+[/.]\\)*[^/.]+\\(:\\|@\\|$\\)"
    "^-[^.]*\\(:\\|@\\|$\\)"
    "^@$"
   "^'$"))

;; ----------------------------------------------------------------------------
;;; Summary

(setq wl-auto-select-next 'unread
      wl-summary-width nil
      wl-summary-weekday-name-lang "en"
      wl-summary-showto-folder-regexp ".Sent.*"
      ;;wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s"
      wl-summary-line-format "%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"

      ;; Summary threads
      wl-thread-insert-opened t
      wl-thread-open-reading-thread t
     )

;; ----------------------------------------------------------------------------
;;; Draft:

(setq wl-draft-config-alist
      '(
        ((string-match ".*x\\.xxxxxx.*\\|.*xxxxxxx/.*" wl-draft-parent-folder)
         ("From" . "xxxxx xxxxxx <x.xxxxxx@xxxxxx.xx.xx>")
         ("Organization" . "Xxxxxxx")
         ("X-Attribution" . "XXX")
         (signature . "~/Maildir/signature"))
        ((string-match ".*enquiries.*\\|.*Enquiries.*" wl-draft-parent-folder)
         ("From" . "Enquiries <enquiries@xxxxxxx.xx.xx>")
         ("Bcc" . "Enquiries <enquiries@xxxxxxx.xx.xx>")
         ("X-Attribution" . "XXXX")
         (signature . "~/Maildir/signature"))
        ((string-match ".*@imap\\.gmail\\.com.*" wl-draft-parent-folder)
         ("From" . "Xxxxx Xxxxxx <xxxxxxx0@gmail.com>")
         ("Organization" . nil)
         ("X-Attribution" . "XXX")
         (signature . "~/Maildir/signature"))
        )

      wl-draft-reply-without-argument-list
      '(("Followup-To" .
         (("Mail-Followup-To" "Mail-Reply-To" "Reply-To") nil ("Followup-To")))
        ("Mail-Followup-To" .
         (("Mail-Followup-To") nil nil))
        ("Newsgroups" .
         (("Mail-Reply-To" "Reply-To" "To") ("Newsgroups")))
        ("Mail-Reply-To" .
         (("Mail-Reply-To" "Reply-To") nil nil))
        ("Reply-To" .
         (("Reply-To") nil nil))
        (wl-draft-self-reply-p . (("To") nil))
        ("From" . (("From") nil nil)))
      )

;;;  boxquote
;; provides a set of functions for using a text quoting style that
;; partially boxes in the left hand side of an area of text, such a marking
;; style might be used to show externally included text or example code.
;;
;; ,----
;; | The default style looks like this.
;; `----
;; using e.g. `boxquote-region'
;; `boxquote-unbox' removes the box
(require 'boxquote)

;; ----------------------------------------------------------------------------
;;; Message:

(setq mime-view-mailcap-files '("~/Maildir/mailcap")
      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^X-Attribution:"
        "^\\(Posted\\|Date\\):"
        )
      wl-message-sort-field-list
      '("^From"
        "^Organization:"
        "^X-Attribution:"
        "^Subject"
        "^Date"
        "^To"
        "^Cc"))

(eval-after-load "mime-view"
  '(progn
     (ctree-set-calist-strictly
      'mime-acting-condition
      '((mode . "play")
        (type . application)(subtype . pdf)
        (method . my-mime-save-content-find-file)))))

;; ----------------------------------------------------------------------------
;;; Refiling:
;; See also `bbdb-wl-refile-alist' and `wl-init-hook'

(defcustom wl-general-refile-rule-alist nil
  "General rule alist which may be extended to include the `From' `folder'
entries defined in the BBDB by `bbdb-wl-refile-alist'.
e.g.
'((\"From\"
   (\"teranisi@isl.ntt.co.jp\" . \"+teranisi\"))
  (\"x-ml-name\"
   (\"^Wanderlust\"    . \"+wl\")
   (\"^Elips\" . \"+elips\")))"
  :type '(repeat (list (string :tag "Field")
                       (repeat :inline t
                               (cons (regexp :tag "Value")
                                     (string :tag "Folder")))))
  :group 'wl-pref)
; Set the default value of wl-refile-rule-alist
(setq wl-refile-rule-alist wl-general-refile-rule-alist)

;; ----------------------------------------------------------------------------
;;; Configure BBDB to manage Email addresses

(require 'bbdb-wl)
(bbdb-wl-setup)

(setq bbdb-use-pop-up t ;; Allow pop-ups
      bbdb-pop-up-target-lines 2
      bbdb/mail-auto-create-p t ;; auto collection
      bbdb-wl-ignore-folder-regexp "^@" ;; folders without auto collection
      bbdb-north-american-phone-numbers-p nil
      bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0)))
      bbdb-dwim-net-address-allow-redundancy t
      bbdb-offer-save 1 ;save w/o asking
      bbdb-always-add-address t
      bbdb-completion-type nil
      bbdb-complete-name-allow-cycling t
      bbdb-use-alternate-names t  ;use nicks (AKA)

      ;; shows the name of bbdb in the summary

      ;; Not with wl-summary-showto-folder-regexp
      ;;wl-summary-from-function 'bbdb-wl-from-func
      ;; Use the default:
      wl-summary-from-function 'wl-summary-default-from

      ;; Using BBDB for pet names is OK
      wl-summary-get-petname-function 'bbdb-wl-get-petname
      )

;; ----------------------------------------------------------------------------
;;; Configure recently used Email addresses

;;(require 'recent-addresses)

;;(setq recent-addresses-file
;;      (expand-file-name "~/Emacs/Wanderlust/recent-addresses"))

;; ----------------------------------------------------------------------------
;;; Configure supercite to manage citations

(require 'supercite)
(autoload 'sc-cite-original "supercite" nil t)
(setq sc-nested-citation-p t
      sc-citation-leader ""
      sc-auto-fill-region-p nil
      sc-confirm-always-p nil)

(defun my-sc-header ()
  "Insert `Dear <sc-author>,' at the beginning of replies."
  (let ((sc-mumble "")
        (whofrom (sc-whofrom)))
    (if whofrom
        (insert "Dear "
                (sc-hdr "" (sc-mail-field "sc-author"))
                ","))))

(add-to-list 'sc-rewrite-header-list '(my-sc-header) t)

(setq sc-preferred-header-style 8)

;; ----------------------------------------------------------------------------
;;; Sending

;; Don't split large messages
(setq mime-edit-split-message nil)

(defun bestinclass-smtp-server ()
  "Configure the use of the GMail SMTP server for sending"
  (setq wl-smtp-connection-type . nil)
  (setq wl-smtp-authenticate-type nil
        wl-smtp-posting-port 25
        wl-smtp-posting-server "YOUR SMTP SERVER"
        wl-local-domain "YOUR DOMAIN.com"))

;; ----------------------------------------------------------------------------
;;; Add hooks

; Allow org-table creation within drafs
 (add-hook 'mail-mode-hook 'turn-on-orgtbl)

(add-hook
 'wl-init-hook
 '(lambda ()
    (set-frame-position (selected-frame) 663 0)
    (set-frame-height (selected-frame) 70)
    (set-frame-width (selected-frame) 114)
    (bestinclass-smtp-server) ;; Set the default smtp server to zen
    (my-bbdb-wl-refile-alist) ;; Add the BBDB refiling folders
    (run-with-idle-timer 30 t 'my-wl-auto-save-draft-buffers)

    ;; Add support for (signature . "filename")
    (unless (assq 'signature wl-draft-config-sub-func-alist)
      (wl-append wl-draft-config-sub-func-alist
                 '((signature . wl-draft-config-sub-signature))))

    (defun mime-edit-insert-signature (&optional arg)
      "Redefine to insert a signature file directly, not as a tag."
      (interactive "P")
      (insert-signature arg))

    ;; Keep track of recently used Email addresses
    ;;(recent-addresses-mode 1)
    ))

(add-hook
 'wl-folder-mode-hook
 '(lambda ()
    (hl-line-mode t)
    (local-set-key "\M-m" 'mairix-search)
    ))

(add-hook
 'wl-summary-mode-hook
 '(lambda ()
    (hl-line-mode t)

    ;; Key bindings
    (local-set-key "D" 'wl-thread-delete)
    (local-set-key "b" 'wl-summary-resend-bounced-mail)
    (local-set-key "\C-d" 'my-wl-summary-delete-and-move-prev)
    (local-set-key "\C-cQ" 'my-wl-delete-whole-folder)
    (local-set-key "\C-cb" 'my-bbdb-wl-refile-alist)
    (local-set-key "\C-a"
                   '(lambda ()
                      (interactive)
                      (wl-summary-reply-with-citation 1)))
    (local-set-key "\M-m" 'mairix-search)
    ))

(add-hook
 'wl-summary-exec-hook
 '(lambda ()
    ;; Synchronise the folder with the server after executing the summary
    ;; operation
    (wl-summary-sync-update)
    ))

(add-hook
 'wl-message-buffer-created-hook
 '(lambda ()
    (setq truncate-lines nil) ;; Fold over-lenght lines
    ))

(add-hook
 'wl-draft-mode-hook
 '(lambda ()
    ;; Key bindings
    (local-set-key "\C-c\C-k" 'my-wl-draft-kill-force)
    ))

;; Add supercite support
(add-hook 'mail-citation-hook 'sc-cite-original)

(add-hook
 'mime-view-mode-hook
 '(lambda ()
    "Change [mouse-2] to drag-scroll rather than follow link.
Set [(return)] to execute the mime-button.
Set the `f' key to run `find-file' on the attached entity.
Set the `C-f' key to run `find-file-at-point'.
Set the `w' key to run `wget'.
Set the `j' key to run `mime-preview-quit'."
    ;; Key bindings
    (local-set-key [down-mouse-2] 'mouse-drag-drag)
    (local-set-key [(return)] 'my-mime-button-exec)
    (local-set-key [?f] 'my-mime-find-file-current-entity)
    (local-set-key [(control ?f)] 'find-file-at-point)
    (local-set-key [?w] 'wget)
    (local-set-key [?o] 'wget-open)
    (local-set-key [?j] 'mime-preview-quit)
    (local-set-key [?s] '(lambda ()
                           (interactive)
                           (mime-preview-quit)
                           (wl-summary-sync)))
    ))

(add-hook
 'wl-biff-notify-hook
 '(lambda ()
    (my-wl-update-current-summaries)
    ))

;; Automatically add mailing list fields
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

;; Smilies
(add-hook
 'wl-message-redisplay-hook
 '(lambda () (smiley-region (point-min) (point-max))
    ))

(add-hook
 'wl-draft-cited-hook
 '(lambda ()
     (and (featurep 'smiley-mule)
          (smiley-toggle-buffer -1))
     ))

;; ----------------------------------------------------------------------------
;;; Extension Functions

;(defadvice mime-encode-region (around use-unibyte-buffer
;				      (start end encoding) activate)
;  "Use unibyte buffer while encoding."
;  (let ((string (prog1
;		    (buffer-substring start end)
;		  (delete-region (goto-char start) end))))
;    (insert (with-temp-buffer
;	      (set-buffer-multibyte nil)
;	      (insert (string-as-unibyte string))
;	      (setq start (point-min)
;		    end (point-max))
;	      ad-do-it
;	      (buffer-string)))))

(defun wl-draft-config-sub-signature (content)
  "Insert the signature at the end of the MIME message."
  (let ((signature-insert-at-eof nil)
        (signature-file-name content))
    (goto-char (mime-edit-content-end))
    (insert-signature)))

(defun my-bbdb-wl-refile-alist ()
  "Add the `From To' refile to `folder' entries from the BBDB to the
`wl-refile-rule-alist'."
  (interactive)
  (let ((from-rule-alist (list '("From" "To")))
        (records (bbdb-records))
        (record))
    (while records
      (setq record (car records))
      (let ((email-addrs (bbdb-record-net record))
            (folder (bbdb-record-getprop record 'folder))
            (email-addr))
        (if folder
            (progn
              (while email-addrs
                (setq email-addr (car email-addrs))
                (setq from-rule-alist
                      (append from-rule-alist (list (cons email-addr folder))))
                (setq email-addrs (cdr email-addrs))))))
      (setq records (cdr records)))
;    (setq wl-refile-rule-alist
 ;         (append wl-general-refile-rule-alist (list from-rule-alist)))
    ))

;; ----------------------------------------------------------------------------
;;; User Functions

(defun my-wl-draft-kill-force ()
  (interactive)
  (wl-draft-kill t))

(defun my-wl-delete-whole-folder ()
  (interactive)
  (wl-summary-target-mark-all)
  (wl-summary-target-mark-delete)
  (wl-summary-exec)
  (wl-summary-exit))

(defun my-wl-check-mail-primary ()
  (interactive)
  (unless (get-buffer wl-folder-buffer-name)
    (wl))
  (delete-other-windows)
  (switch-to-buffer wl-folder-buffer-name)
  (goto-char (point-min))
  (next-line 1)
  (wl-folder-jump-to-current-entity))

(defun my-wl-auto-save-draft-buffers ()
  (let ((buffers (wl-collect-draft)))
    (save-excursion
      (while buffers
        (set-buffer (car buffers))
        (if (buffer-modified-p) (wl-draft-save))
        (setq buffers (cdr buffers))))))

(defun my-wl-update-current-summaries ()
  (let ((buffers (wl-collect-summary)))
    (while buffers
      (with-current-buffer (car buffers)
        (save-excursion
          (wl-summary-sync-update)))
      (setq buffers (cdr buffers)))))

(defun my-wl-summary-delete-and-move-prev ()
  (interactive)
  (let (wl-summary-move-direction-downward)
    (call-interactively 'wl-summary-delete)))

(defun my-wl-summary-goto-to-folder (folder)
  "Goto FOLDER from the summary buffer after closing it."
  (wl-summary-exit t)
  (set-buffer (get-buffer wl-folder-buffer-name))
  (wl-folder-goto-folder-subr folder))

(defun my-wl-goto-to-folder (folder)
  "Goto FOLDER from either the folders or summary buffer after closing it."
  (if (string= (buffer-name) wl-summary-buffer-name)
      (my-wl-summary-goto-to-folder search-folder)
    (wl-folder-goto-folder-subr search-folder)))

(defun wl-rehilight ()
  "Re-highlight message."
  (let ((beg (point-min))
        (end (point-max)))
    (put-text-property beg end 'face nil)
    (wl-highlight-message beg end t)))

(defun my-mime-save-content-find-file (entity &optional situation)
  "Save the attached mime ENTITY and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (let* ((name (or (mime-entity-safe-filename entity)
                   (format "%s" (mime-entity-media-type entity))))
         (dir (if (eq t mime-save-directory)
                  default-directory
                mime-save-directory))
         (filename (expand-file-name
                    (file-name-nondirectory name) temporary-file-directory)))
    (mime-write-entity-content entity filename)
    (select-frame (make-frame))
    (find-file filename)
    ))

(defun my-mime-view-emacs-mode (entity &optional situation)
  "Internal method for mime-view to display the mime ENTITY in a buffer with an
appropriate emacs mode."
  (let ((buf (get-buffer-create
              (format "%s-%s" (buffer-name) (mime-entity-number entity)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mime-insert-text-content entity)
      ;;(mule-caesar-region (point-min) (point-max))
      ;; Set emacs mode here
      (set-buffer-modified-p nil)
      )
    (let ((win (get-buffer-window (current-buffer))))
      (or (eq (selected-window) win)
          (select-window (or win (get-largest-window)))
          ))
    (view-buffer buf)
    (goto-char (point-min))
    ))

(defun my-mime-find-file-current-entity ()
  "Save the current mime entity and load it with `find-file-other-frame'
so that the appropriate emacs mode is selected according to the file extension."
  (interactive)
  (let ((entity (get-text-property (point) 'mime-view-entity)))
    (if entity
        (my-mime-save-content-find-file entity)))
  )

(require 'elmo)

(defun my-bbdb-insert-folder ()
  "Interactively select the destination folder and store in BBDB."
  (interactive)
  (let* ((record (or (bbdb-current-record t)
                     (error "current record unexists!")))
         (name (wl-summary-read-folder my-wl-default-filing-folder))
         (folder-path (wl-folder-get-elmo-folder name)))
    (bbdb-insert-new-field
     record 'folder (elmo-folder-name-internal folder-path)))
  ;; Update the wl refiling database
  (my-bbdb-wl-refile-alist))

(defun my-mime-button-exec ()
  "Execute the button under point without using the mouse."
  (interactive)
  (let (buf point func data)
    (save-window-excursion
      (setq buf (current-buffer)
            point (point)
            func (get-text-property (point) 'mime-button-callback)
            data (get-text-property (point) 'mime-button-data)
            ))
    (save-excursion
      (set-buffer buf)
      (goto-char point)
      (if func
          (apply func data))
      )))

;; ----------------------------------------------------------------------------
;;; Key-bindings

;(global-set-key "\C-xm" 'my-wl-check-mail-primary)
(define-key bbdb-mode-map [(control f)] 'my-bbdb-insert-folder)

;; ----------------------------------------------------------------------------
;;; Simple mairix interface

(setq my-mairix "mairix")
(setq my-mairix-search-folder ".Search")

(defun my-mairix-search (args)
  "Mairix search"
  (interactive "sSearch: ")
  (let ((buf (get-buffer-create "*mairix*"))
        (msg))
    (set-buffer buf)
    (erase-buffer)
    (apply 'call-process my-mairix nil buf nil (split-string args))
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (setq msg (buffer-substring (point-min) (point))))
    (kill-buffer buf)
    (if (string-match "Matched \\([0-9]+\\) messages" msg)
        (let ((cnt (match-string 1 msg)))
          (if (not (string= cnt "0"))
              (my-wl-goto-to-folder my-mairix-search-folder))
          (message msg))
      (error msg))))

;; ----------------------------------------------------------------------------
;;; General mairix interface

(require 'mairix)

(add-to-list 'mairix-display-functions '(wl mairix-wl-display))
(add-to-list 'mairix-get-mail-header-functions '(wl mairix-wl-fetch-field))

(setq mairix-file-path "~/Maildir/"
      mairix-search-file "Search"
      mairix-mail-program 'wl
      mairix-wl-search-folder-prefix ".")

(defun mairix-wl-display (folder)
  "Display FOLDER using Wanderlust."
  ;; If Wanderlust is running (Folder buffer exists)...
  (if (get-buffer wl-folder-buffer-name)
      ;; Check if we are in the summary buffer, close it and
      ;; goto the Folder buffer
      (if (string= (buffer-name) wl-summary-buffer-name)
          (progn
            (wl-summary-exit t)
            (set-buffer (get-buffer wl-folder-buffer-name))))
    ;; Otherwise Wanderlust is not running so start it
    (wl))
  ;; From the Folder buffer goto FOLDER first stripping off mairix-file-path
  ;; to leave the wl folder name
  (when (string-match
         (concat (regexp-quote (expand-file-name mairix-file-path)) "\\(.*\\)")
         folder)
    (wl-folder-goto-folder-subr
     (concat mairix-wl-search-folder-prefix (match-string 1 folder)))))


(defun mairix-wl-fetch-field (field)
  "Get mail header FIELD for current message using Wanderlust."
  (when wl-summary-buffer-elmo-folder
    (elmo-message-field
     wl-summary-buffer-elmo-folder
     (wl-summary-message-number)
     (intern (downcase field)))))

(define-key wl-summary-mode-map (kbd "A") 'wl-summary-reply)
(define-key wl-summary-mode-map (kbd "a") 'wl-summary-reply-with-citation)
(define-key wl-draft-mode-map   (kbd "C-h") 'org-mime-htmlize)

;; ----------------------------------------------------------------------------
;;; init-wl.el ends here



(defun mime-display-multipart/related (entity situation)
  (let* ((param-start (mime-parse-msg-id
                       (std11-lexical-analyze
                        (cdr (assoc "start"
                                    (mime-content-type-parameters
                                     (mime-entity-content-type entity)))))))
         (original-major-mode-cell (assq 'major-mode situation))
         (default-situation (cdr (assq 'childrens-situation situation))))
    (if original-major-mode-cell
        (setq default-situation
              (cons original-major-mode-cell default-situation)))
    (dolist (ent
             (or (and param-start (mime-find-entity-from-content-id
                                   param-start
                                   entity))
                 (mime-entity-children entity)))
      (mime-display-entity ent nil default-situation))))

(require 'filladapt)

;; from a WL mailing list post by Per b. Sederber
;; Re-fill messages that arrive poorly formatted
(defun wl-summary-refill-message (all)
  (interactive "P")
  (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
      (progn
        (wl-summary-toggle-disp-msg 'on)
        (save-excursion
          (set-buffer wl-message-buffer)
          (goto-char (point-min))
          (re-search-forward "^$")
          (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
            (forward-line 1))
          (let* ((buffer-read-only nil)
                 (find (lambda (regexp)
                         (save-excursion
                           (if (re-search-forward regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
                 (start (point))
                 (end (if all
                          (point-max)
                        (min (funcall find "^[^>\n]* wrote:[ \n]+")
                             (funcall find "^>>>>>")
                             (funcall find "^ *>.*\n *>")
                             (funcall find "^-----Original Message-----")))))
            (save-restriction
              (narrow-to-region start end)
              (filladapt-mode 1)
              (fill-region (point-min) (point-max)))))
        (message "Message re-filled"))
    (message "No message to re-fill")))

(define-key wl-summary-mode-map "\M-q" 'wl-summary-refill-message)

;;;; Biff

;; It does not seem like WL provides a function to list all the
;; folders it knows by name, so here's one.
(defun my-wl-folder-name-list ()
  "Return a list of all folder names."
  ;; XXX: It seems the first folder always has ID 1.
  ;; `wl-folder-get-next-folder' is broken in my version of WL: it
  ;; wouldn't accept folder names, so we have to rely on IDs.
  (let (folder-list
	(id 1)
	(entity (wl-folder-get-folder-name-by-id 1)))
    (setq folder-list (list entity))
    (setq entity (wl-folder-get-next-folder id))
    (while entity
      (setq id (wl-folder-get-entity-id entity))
      (setq folder-list (cons entity folder-list))
      (setq entity (wl-folder-get-next-folder id)))
    folder-list))

;; XXX: The doc is wrong, `wl-biff-check-folder-list' cannot hold
;; regexps meaningfully. We have to list all our folders. However,
;; this can only be done once WL is started, so we leave it to the
;; main .emacs.
(defun my-mail-notify ()
  (message "New mail"))

(defun my-wl-biff ()
  "Set up biff on all WL folders."
  (setq wl-biff-check-folder-list (my-wl-folder-name-list)
	wl-biff-check-interval 60)
  (add-hook 'wl-biff-notify-hook 'my-mail-notify)
  (setq wl-biff-use-idle-timer t)
  (wl-biff-start))

;; My IMAP server (Dovecot) interprets the call to
;; `elmo-folder-exists-p' as a client query that should unmark new
;; mails and leave them as simply Unread (instead of Recent).
(defun my-elmo-folder-exists-p (folder) t)
(defadvice wl-biff-check-folders (around my-disable-exists-test activate)
  "Disable `elmo-folder-exists-p' and make it return t."
  (let ((real-elmo-folder-exists-p
	 (symbol-function 'elmo-folder-exists-p)))
    (fset 'elmo-folder-exists-p (symbol-function 'my-elmo-folder-exists-p))
    ad-do-it
    (fset 'elmo-folder-exists-p real-elmo-folder-exists-p)))

