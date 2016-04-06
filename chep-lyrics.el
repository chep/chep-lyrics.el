;;; chep-lyrics.el
;;
;; Copyright (C) 2014 Cédric Chépied <cedric.chepied@gmail.com>
;;
;; Based on emms-get-lyrics.el Copyright (C) 2013 andres.ramirez
;;                             Copyright (C) 2007 Jay Belanger
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; Copyright (C) Cédric Chépied 2014
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcustom chep-lyrics-url
  "http://www.azlyrics.com"
  "Base url for chep-lyrics"
  :type 'string
  :group 'chep)

(defun chep-lyrics-url (artist title)
  (setq artist (downcase artist))
  (when (string-prefix-p "the" artist)
    (setq artist (substring artist 4)))
  (concat
   chep-lyrics-url
   "/lyrics/"
   (replace-regexp-in-string
    " " ""
    (concat
     artist
     "/"
     (downcase title)
     ".html"))))

(defun chep-lyrics-w3m (url buffer)
  (message url)
  (call-process "w3m" nil buffer nil "-dump" url))

(defun chep-lyrics (artist title fn)
  (let ((bname (concat "Lyrics: " title " by " artist)))
    (cond ((get-buffer bname)
           (switch-to-buffer bname))
          (t
           (let ((buffer (get-buffer-create bname)))
             (set-buffer buffer)
             (funcall fn (chep-lyrics-url artist title) buffer)
             (goto-char (point-min))
             (kill-line 5)
             (if (search-forward "Request Lyrics" nil t)
                 (progn
                   (erase-buffer)
                   (insert "Unable to find lyrics for " title " by " artist))
               (search-forward "Submit Corrections" nil t)
               (previous-line 4)
               (move-beginning-of-line nil)
               (delete-region (point) (point-max))
               (insert "------------------")
               (newline)
               (insert (concat "Found on " chep-lyrics-url)))
               (newline)
               (insert (chep-lyrics-url artist title))
             (switch-to-buffer buffer)
             (goto-char (point-min)))))))

(defun chep-lyrics-current()
  (interactive)
  (assert (ampc-on-p))
  (ampc-send-command 'status)
  (return-from ampc-status)
  (let ((stopped (equal (cdr (assq 'state ampc-status)) "stop")))
    (unless stopped
      (let ((artist (ampc-clean-tag 'Artist (cdr (assq 'Artist ampc-status))))
            (title (ampc-clean-tag 'Title (cdr (assq 'Title ampc-status)))))
        (chep-lyrics artist title 'chep-lyrics-w3m)))))


(provide 'chep-lyrics)
