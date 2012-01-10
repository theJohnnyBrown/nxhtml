;;; js-utils.el --- 
;; 
;; Filename: bookmarkletify-js.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: 2011-12-27T04:12:56+0100
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar jsut-bookmarklet-template-file (expand-file-name "etc/js/bm-base.js" nxhtml-install-dir))

;;;###autoload
(defun jsut-mk-bookmarklet (js-buffer)
  (interactive (list (current-buffer)))
  (let* ((re-my-namespace (rx bol (* space) "var" (+ space) "myNamespace" (* space) "=" (* space)
                              (any "'\"")
                              (submatch (* nonl))
                              (any "'\"")))

         ;; fix-me:
         (re-my-url-src (rx bol (* space) "//" (+ space) "myURL" (* space) "=" (* space)
                            (any "'\"")
                            (submatch (* nonl))
                            (any "'\"")))
         (re-my-url-bm (rx bol (* space) "var" (+ space) "myURL" (* space) "=" (* space)
                           (any "'\"")
                           (submatch (* nonl))
                           (any "'\"")))
         my-namespace
         my-url)
    (with-current-buffer js-buffer
      (let ((here (point)))
        (save-restriction
          (widen)
          (goto-char (point-min))
          (if (not (re-search-forward re-my-namespace nil t))
              (error "Can't find line with 'var myNamespace=...' in source buffer %s" js-buffer)
            (setq my-namespace (match-string 1)))
          (goto-char (point-min))
          (if (not (re-search-forward re-my-url-src nil t))
              (error "Can't find line with '// myURL=...' in source buffer %s" js-buffer)
            (setq my-url (match-string 1)))
          (goto-char here))))
    (when my-namespace
      (let* ((tbuf-opened t)
             (template-buf (or (find-buffer-visiting jsut-bookmarklet-template-file)
                               (setq tbuf-opened nil)
                               (find-file-noselect (find-buffer-visiting jsut-bookmarklet-template-file))))
             )
        (with-current-buffer template-buf
          (let ((here (point)))
            (save-restriction
              (widen)
              (goto-char (point-min))
              (if (not (re-search-forward re-my-namespace nil t))
                  (error "Can't find line with 'var myNamespace=...' in template buffer %s" template-buf)
                (replace-match my-namespace t t nil 1))
              (goto-char (point-min))
              (if (not (re-search-forward re-my-url-bm nil t))
                  (error "Can't find line with 'var myURL=...' in template buffer %s" template-buf)
                (replace-match my-url t t nil 1)))
            (goto-char here)))
        (jsut-bookmarkletify template-buf)
        (unless tbuf-opened (kill-buffer template-buf))
        ))))

;;;###autoload
(defun jsut-bookmarkletify (js-bm-buffer)
  (interactive (list (current-buffer)))
  (with-current-buffer js-bm-buffer
    (let* ((js-in (buffer-substring-no-properties (point-min) (point-max)))
           (js js-in)
           (outbuf (get-buffer-create "*BOOKMARK*")))
      (setq js (replace-regexp-in-string "/\\*\\(?:.\\|\n\\)*\\*/" "" js))
      (setq js (replace-regexp-in-string "\\(^\\|;\\)\s*//.*\n" "\\1" js))
      (when (string-match-p "[^\\]\"" js)
        (error "The javascript code contains \". This can't be used in bookmarklets."))
      (setq js (replace-regexp-in-string "\n" " " js))
      (setq js (replace-regexp-in-string "\s+" " " js))
      (with-current-buffer outbuf
        (erase-buffer)
        (html-mumamo-mode)
        (insert
         "\n<a href=\"javascript:"
         js
         "; void 0;\">BOOKMARKLET</a>\n"
         "<!-- Bookmark length = " (number-to-string (length js)) " -->\n\n"
         ))
      (display-buffer outbuf))))

(defcustom jsut-plovr-jar-file "PATH-TO/plovr-96feca4d303b.jar"
  "Path to plovr jar file..
For information about plovr see URL `http://plovr.com/'."
  :type 'file
  :group 'js-utils)

(defsubst jsut-plovr-file (js-file) (concat js-file ".plovr.js"))

;;;###autoload
(defun jsut-plovr-dev-info (plovr-file)
  (interactive (list (buffer-file-name)))
  (when (or (string-match-p "\.plovr.js$" plovr-file)
            (y-or-n-p "File does not end in .plovr.js - are you sure it is a plovr conf file? "))
    (let* ((buf (get-buffer-create "*JSUT plovr dev info*"))
           (plovr-buf (find-buffer-visiting plovr-file))
           (was-visiting plovr-buf)
           id)
      (setq plovr-buf (or plovr-buf (find-file-noselect plovr-file)))
      (with-current-buffer plovr-buf
        (let ((here (point)))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (search-forward "{")
            (let ((json (json-read-object)))
              (goto-char here)
              (setq id (cdr (assoc 'id json)))))))

      (with-current-buffer buf
        (erase-buffer)
        (insert
         "To start plovr as a compiling server enter this in a command shell:\n"
         "  java -jar "
         (shell-quote-argument (convert-standard-filename jsut-plovr-jar-file))
         " serve "
         (shell-quote-argument (file-name-nondirectory plovr-file))
         "\n\nThen access the compiled js file with\n"
         "  http://localhost:9810/compile?id=" id
        ))
      (display-buffer buf))))

;;;###autoload
(defun jsut-plovr-compile (js-file)
  "Compile JS-FILE with plovr/closure compiler."
  (interactive (list (buffer-file-name)))
  (if (not (file-exists-p jsut-plovr-jar-file))
      (when (y-or-n-p "Can't find plovr. Do you want to customize jsut-plovr-jar-file? ")
        (customize-option-other-window 'jsut-plovr-jar-file))
    (let* ((plovr-file (if (string-match-p "\.plovr.js$" js-file)
                           js-file
                         (jsut-plovr-file js-file)))
           (cmd-template "java -jar %s build %s"))
      (if (not (file-exists-p plovr-file))
          (if (not (y-or-n-p "A specific plovr config file is needed, but not found. Create it? "))
              (message "Can't compile without this file")
            (let* ((plovr-template "
// See http://www.plovr.com/options.html
{
    \"id\": %S,
    \"inputs\": [%S],
    \"paths\": \".\",
    \"externs\": [%S],
    \"custom-externs-only\": true,
    \"mode\":\"advanced\",
    \"output-file\": %S,
    \"output-wrapper\": \"/* Copyright 2011 YOUR NAME */ (function(){%%output%%})();\",
    \"output-charset\": \"UTF-8\"
}")
                   (id (file-name-sans-extension (file-name-nondirectory js-file)))
                   (inp (file-name-nondirectory js-file))
                   (ext (concat (file-name-nondirectory (file-name-sans-extension js-file)) "-externs.js"))
                   (out (concat (file-name-nondirectory (file-name-sans-extension js-file)) "-cld.js"))
                   (plovr-conf (format plovr-template id inp ext out))
                   (buf (find-file plovr-file))
                   )
              (with-current-buffer buf
                (insert plovr-conf))))
        (let ((compile-command (format cmd-template
                                       (shell-quote-argument
                                        (convert-standard-filename jsut-plovr-jar-file))
                                       (shell-quote-argument
                                        (convert-standard-filename plovr-file)))))
          (message "cmd=%s" compile-command)
          (call-interactively 'compile))
        )
      )
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; js-utils.el ends here
