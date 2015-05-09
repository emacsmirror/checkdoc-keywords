;;; checkdoc-keywords.el --- check for known values in Keywords header

;; Copyright 2009, 2010, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 7
;; Keywords: lisp, maint, checkdoc
;; URL: http://user42.tuxfamily.org/checkdoc-keywords/index.html
;; EmacsWiki: CheckDoc

;; checkdoc-keywords.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; checkdoc-keywords.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code extends M-x checkdoc to check that Keywords: contains
;; at least one keyword known to `finder-by-keywords'.  See the docstring of
;; `checkdoc-keywords-comment' below for more.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21 and Emacs 20.

;;; Install:

;; Put checkdoc-keywords.el in one of your `load-path' directories
;; and in your .emacs add
;;
;;     (autoload 'checkdoc-keywords-comment "checkdoc-keywords")
;;     (add-hook 'checkdoc-comment-style-hooks 'checkdoc-keywords-comment)
;;
;; There's an autoload cookie on the function if you know how to use
;; `update-file-autoloads' and friends, after which add or customize the
;; hook.

;;; History:

;; Version 1 - the first version
;; Version 2 - don't insert an empty keyword
;; Version 3 - some help for completing-help.el
;; Version 4 - return nil so as not to short-circuit the hook
;; Version 5 - undo completing-help.el setups on unload-feature
;; Version 6 - emacs21 completing-read requires strings in the table
;;           - don't double comma if existing Keywords ends in comma
;; Version 7 - new email

;;; Code:

(require 'checkdoc)
(require 'finder)
(require 'lisp-mnt)

;;-----------------------------------------------------------------------------
;; xemacs21 compatibility

(if (eval-when-compile (fboundp 'lm-keywords-finder-p))
    ;; emacs21 up
    (eval-and-compile ;; quieten the byte compiler
      (defalias 'checkdoc-keywords--lm-keywords-finder-p
        'lm-keywords-finder-p))

  ;; xemacs21
  (eval-and-compile ;; quieten the byte compiler
    (require 'cl)) ;; for `intersection' in xemacs21
  (defun checkdoc-keywords--lm-keywords-finder-p (&optional file)
    (intersection
     (mapcar 'symbol-name (mapcar 'car finder-known-keywords))
     (split-string (or (lm-keywords file) "")
                   ",?[ \t]") ;; similar to emacs23
     :test 'equal)))

(if (eval-when-compile (fboundp 'lm-copyright-mark))
    ;; emacs21 up
    (eval-and-compile ;; quieten the byte compiler
      (defalias 'checkdoc-keywords--lm-copyright-mark
        'lm-copyright-mark))

  ;; xemacs21
  (defun checkdoc-keywords--lm-copyright-mark (&optional file)
    (let ((case-fold-search t))
      (goto-char (point-min))
      (if (re-search-forward "^;+[ \t]*Copyright" nil t)
	  (point)))))

;;-----------------------------------------------------------------------------

;;;###autoload
(defun checkdoc-keywords-comment ()
  "Check for at least one known \"Keyword:\" value.
This function is designed for use in `checkdoc-comment-style-hooks'.

It checks the file has a Keywords header line like

    ;; Keywords: foo, bar

and that at least one keyword is in `finder-known-keywords' so
that the file can appear under `finder-by-keyword'.  Extra
private keywords are fine, as long as there's at least one
standard one.

If there's no known keyword or no Keywords header at all then
you're prompted to choose one to insert.

`lm-keywords-finder-p' is used to check for a known keyword.
Files meant only as a sub-component of another Lisp file probably
shouldn't appear under `finder-by-keyword' and so won't have a
known keyword, but currently there's no way to flag that (perhaps
`lm-keywords-finder-p' could get something).  For now just answer
\"n\" to the query.

The checkdoc-keywords.el home page is
URL `http://user42.tuxfamily.org/checkdoc-keywords/index.html'"

  (unless (checkdoc-keywords--lm-keywords-finder-p)

    (save-excursion
      ;; `fmt' to append to existing Keywords line.  Some files have just
      ;; space as a separator, but don't try to detect that, just put a
      ;; comma always.
      (let ((fmt ", %s")
            keyword)

        (goto-char (point-min))
        (if (re-search-forward (lm-get-header-re "Keywords") nil t)
	    (progn
	      (end-of-line)
	      (skip-chars-backward " \t,"))

          ;; no existing Keywords, make `fmt' insert a whole new one
          (setq fmt ";; Keywords: %s\n")
          ;; put it after the copyright line(s), or if no such then after
          ;; the first-line summary
          (goto-char (or (checkdoc-keywords--lm-copyright-mark)
                         (point-min)))
          (forward-line)
          ;; past blank line after Copyright
          (if (looking-at "\n")
              (goto-char (match-end 0)))
          ;; insert newline if Copyright at end of buffer
          (unless (memq (char-before) '(nil ?\n))
            (setq fmt (concat "\n" fmt))))

        (if (checkdoc-y-or-n-p "`Keywords:' has none of `finder-known-keywords'.  Add one? ")
            (setq keyword
                  ;; Crib note: completion table keys must be strings for
                  ;; emacs21, and also completing-help.el 3.13
                  ;; `completing-help-alist-p' only recognises strings
                  (completing-read "Keyword: "
                                   (mapcar (lambda (elem)
                                             (cons (symbol-name (car elem))
                                                   (cdr elem)))
                                           finder-known-keywords)
                                   nil ;; predicate
                                   t))) ;; require-match
        ;; completing-read returns empty string "" on just pressing return,
        ;; don't insert that or it ends up as a trailing comma, which Emacs
        ;; 23.1 treats as part of the last symbol.  Instead consider an
        ;; empty return as don't want to add.
        (if (equal "" keyword)
            (setq keyword nil))

        (if keyword
            (progn
              (insert (format fmt keyword))
              nil) ;; ok

          ;; not ok
          (checkdoc-create-error
           "`Keywords:' doesn't have anything from `finder-known-keywords'"
           (point)
           (line-end-position))))))

  ;; Return nil if user chose not to fix.  `checkdoc-comment-style-hooks'
  ;; seems to say return an error string, but doing so means its
  ;; `run-hook-with-args-until-success' stops, so subsequent style checks
  ;; are not run.  Could justify nil here if "n" from the user meant don't
  ;; change because it's in fact ok.
  nil)

;;;###autoload
(custom-add-option 'checkdoc-comment-style-hooks 'checkdoc-keywords-comment)

;; LocalWords: docstring el foo

(provide 'checkdoc-keywords)

;;; checkdoc-keywords.el ends here
