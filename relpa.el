;;; relpa.el --- Reddit ELPA menu for Elpaca         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; URL: https://github.com/progfolio/RELPA
;; Created: December 10, 2019
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (elpaca "0.0"))
;; Version: 0.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'elpaca)
(require 'cl-lib)

(defgroup relpa nil "Reddit ELPA."
  :group 'applications
  :prefix "relpa-")

(defcustom relpa-cache-file (expand-file-name "cache/relpa.eld" elpaca-directory)
  "Name of the RELPA cache file."
  :type 'path)
(defcustom relpa-base-url "https://www.reddit.com"
  "Base Reddit URL." :type 'string)
(defcustom relpa-results-limit 100
  "Number of results to fetch. Note: max of 100 per API." :type 'integer)
(defcustom relpa-trusted-submitters nil "List of trusted recipe submitters." :type 'list)
(defcustom relpa-vote-ratio-threshold 0.8 "Excludes packages with a vote ratio lower than the threshold." :type 'float)

(defun relpa-trusted-submitter-p (item)
  "Return t if ITEM submitter a member of `relpa-trusted-submitters'."
  (member (plist-get item :submitter) relpa-trusted-submitters))

(defun relpa-elected-p (item)
  "Return t if ITEM has a vote ratio above `relpa-vote-ratio-threshold'."
  (>= (plist-get item :vote-ratio) relpa-vote-ratio-threshold))

(defcustom relpa-item-functions (list #'relpa-trusted-submitter-p #'relpa-elected-p)
  "Abnormal hook which to filter menu items.
Each function is called with a menu item candidate as its sole argument.
If it returns t, the menu item is kept. Otherwise the menu item is discarded." :type 'list)

(defvar url-http-end-of-headers)
(defvar json-object-type)
(defvar relpa--cache (elpaca--read-file relpa-cache-file) "Cache for items.")
(declare-function json-read-from-string "json")

(defun relpa--json (url)
  "Return JSON from URL."
  (with-current-buffer (url-retrieve-synchronously url 'silent)
    (let ((s (decode-coding-region url-http-end-of-headers (point-max) 'utf-8 t)))
      (if (fboundp #'json-parse-string)
          (json-parse-string s :object-type 'alist)
        (require 'json)
        (let ((json-object-type 'alist))
          (json-read-from-string s))))))

(defun relpa--recipes (subreddit &optional sorter)
  "Return recipes from SUBREDDIT/SORTER."
  (if-let ((url (string-join
                 (list relpa-base-url
                       "r" subreddit
                       (format ".json?sort=%s&limit=%d"
                               (or sorter 'top) relpa-results-limit))
                 "/"))
           (json (relpa--json url))
           (data (alist-get 'data json))
           (children (cl-coerce (alist-get 'children data) 'list)))
      (cl-loop for post in children
               for data = (alist-get 'data post)
               for recipe = (let ((form (ignore-errors
                                          (car (read-from-string
                                                (alist-get 'title data))))))
                              (and (listp form) form))
               for id = (car recipe)
               for submitter = (alist-get 'author data)
               do (setf (car recipe) (symbol-name (car recipe)))
               (push :package recipe)
               for item = (list
                           :source (concat "r/" subreddit)
                           :submitter submitter
                           :date (ignore-errors
                                   (time-convert (alist-get 'utc_created data) nil))
                           :vote-ratio (alist-get 'upvote_ratio data)
                           :description (alist-get 'selftext data)
                           :recipe recipe)
               when (and recipe (cl-every (lambda (pred) (funcall pred item))
                                          relpa-item-functions))
               collect (cons id item))))

;;;###autoload
(defun relpa (request)
  "Menu function which provides RELPA recipes.
Supports `index` and `update` REQUESTs."
  (or (and (not (eq request 'update)) relpa--cache)
      (prog2
          (message "Updating RELPA menu.")
          (setq relpa--cache (relpa--recipes "relpa"))
        (elpaca--write-file relpa-cache-file (prin1 relpa--cache))
        (message "RELPA menu updated."))))

(provide 'relpa)
;;; relpa.el ends here
