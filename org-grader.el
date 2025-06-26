;;; org-grader.el --- Support for grading papers in orgmode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Peter J. Jones

;; Author: Peter J. Jones <peter@jonesbunch.com>
;; Maintainer: Peter J. Jones <peter@jonesbunch.com>
;; Created: 2025
;; Version: 1.0
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/pjones/org-grader

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-element)

;; Silence the code linter:
(declare-function org-columns-quit "org-colview")
(defvar org-columns-current-fmt-compiled)
(defvar org-export-global-macros)

(defgroup org-grader nil
  "A minor mode for using Org to grade papers."
  :link '(url-link :tag "Website" "https://github.com/pjones/org-grader")
  :link '(emacs-library-link :tag "Library Source" "org-grader.el")
  :group 'org
  :prefix "org-grader-")

(defcustom org-grader-points-property "POINTS"
  "The property to store the number of points awarded for a task.
This is calculated from the number of checked checkboxes by the
`org-grader-points-property-update' function."
  :type 'string)

(defcustom org-grader-points-per-checkbox-property "POINTS_PER_CHECKBOX"
  "The property to store the number of points per checkbox.
When a checkbox is checked this many points are awarded.  Defaults to 1."
  :type 'string)

(defcustom org-grader-template-property "TEMPLATE"
  "The name of a property where the template file name is stored.
This property is used by the org capture system to fetch the template
for the current assignment."
  :type 'string)

(defcustom org-grader-after-insert-hook nil
  "Hook run after inserting a template.
This is a good place to put something like `org-narrow-to-subtree'."
  :type 'hook)

(defvar org-grader-template-point nil
  "The location of point after inserting the last template.")

(defun org-grader--fetch-assignment-template ()
  "Return the current assignment's template."
  (let ((template (org-entry-get nil org-grader-template-property t)))
    (unless template
      (error "There is no %s property in this tree"
             org-grader-template-property))
    (org-file-contents template)))

(defun org-grader--move-to-assignment-parent ()
  "Move point to the location where the template will be inserted."
  (when-let ((epom (org-element-lineage-map
                       (org-element-at-point)
                       (lambda (e) (if (org-entry-get e org-grader-template-property nil) e))
                     '(headline) t t)))
    (goto-char (org-element-property :begin epom))))

(defun org-grader--checkbox-points (struct item)
  "Return the number of points for checkbox ITEM.
STRUCT is taken from `org-list-struct'."
  (let ((points
         (string-to-number
          (or (org-entry-get nil org-grader-points-per-checkbox-property t)
              "1"))))
    (if (string= "[X]" (org-list-get-checkbox item struct)) points 0)))

(defun org-grader-points-property-update ()
  "Update the points property based on the checkbox count."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-list-search-forward (org-item-beginning-re))
    (when (org-at-item-p)
      (let* ((struct (org-list-struct))
             (parents (org-list-parents-alist struct))
             (all-items (mapcar #'car struct))
             parent-list sum)
        (mapc
         (lambda (item)
           (when (and (org-list-get-checkbox item struct)
                      (not (org-list-get-parent item struct parents))
                      (not (memq item parent-list)))
             (push item parent-list)))
         all-items)
        (setq sum (apply #'+ (mapcar
                              (apply-partially #'org-grader--checkbox-points struct)
                              parent-list)))
        (org-back-to-heading)
        (org-entry-put (point) org-grader-points-property (int-to-string sum))))))

(defun org-grader-template-insert ()
  "Insert a template in the current tree.
The template file name will be taken from the
`org-grader-template-property' property in the current tree."
  (interactive)
  (let* ((key "A")
         (template `(,key "Current org-grader assignment" entry
                          (function org-grader--move-to-assignment-parent)
                          (function org-grader--fetch-assignment-template)
                          :empty-lines 1
                          :immediate-finish t
                          :jump-to-captured t
                          :prepare-finalize (lambda ()
                                              (setq org-grader-template-point
                                                    (point)))))
         (org-capture-templates (list template)))
    (org-capture nil key)
    (let ((start (point)))
      (run-hooks 'org-grader-after-insert-hook)
      (when (and (= start (point)) org-grader-template-point)
        ;; Improve on jump-to-captured by respecting %?:
        (goto-char org-grader-template-point)
        (setq org-grader-template-point nil)))))

(defun org-grader-macro-column (name)
  "Return the value for NAME column.

This function is exposed as an `org-mode' macro that can be used to
access a computed property using the column view.  For example, to get
the POINTS column using a macro:

  {{{og-column(POINTS)}}}

This macro will compute the column and return it."
  (let ((columns (make-hash-table :test 'equal)))
    (org-with-wide-buffer
     (org-back-to-heading)
     (org-columns)
     (dotimes (i (length org-columns-current-fmt-compiled))
       (let* ((col (+ (line-beginning-position) i))
              (key (get-char-property col 'org-columns-key))
              (val (get-char-property col 'org-columns-value-modified)))
         (puthash key val columns)))
     (org-columns-quit))
    (gethash name columns)))

;;;###autoload
(define-minor-mode org-grader-mode
  "A minor mode for using Org to grade papers."
  :init-value nil
  :lighter nil
  (if org-grader-mode
      (progn
        (add-hook 'org-checkbox-statistics-hook
                  #'org-grader-points-property-update nil t)
        (add-to-list 'org-export-global-macros
                     (cons "og-column" #'org-grader-macro-column)
                     nil (lambda (a b) (string= (car a) (car b)))))
    (remove-hook 'org-checkbox-statistics-hook
                 #'org-grader-points-property-update t)
    (setq org-export-global-macros
          (assoc-delete-all "og-column" org-export-global-macros))))

(provide 'org-grader)
;;; org-grader.el ends here
