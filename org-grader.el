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

;; FIXME: Is this bad form?
(make-variable-buffer-local 'org-capture-templates)

(defgroup org-grader nil
  "A minor mode for using Org to grade papers."
  :link '(url-link :tag "Website" "https://github.com/pjones/org-grader")
  :link '(emacs-library-link :tag "Library Source" "org-grader.el")
  :group 'org
  :prefix "org-grader-")

(defcustom org-grader-capture-key "g"
  "Which key to use in `org-capture-templates'."
  :type 'string)

(defcustom org-grader-points-property "POINTS"
  "The property to store the number of points awarded for a task.
This is calculated from the number of checked checkboxes by the
`org-grader-points-property-update' function."
  :type 'string)

(defcustom org-grader-template-property "TEMPLATE"
  "The name of a property where the template file name is stored.
This property is used by the org capture system to fetch the template
for the current assignment."
  :type 'string)

(defun org-grader--make-capture-template ()
  "Construct a template for `org-capture-templates'."
  `(,org-grader-capture-key "Current org-grader assignment" entry
    (function org-grader--move-to-assignment-parent)
    (function org-grader--fetch-assignment-template)
    :empty-lines 1
    :immediate-finish t
    :jump-to-captured t))

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
  (if (string= "[X]" (org-list-get-checkbox item struct)) 1 0))

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

(define-minor-mode org-grader-mode
  "A minor mode for using Org to grade papers."
  :init-value nil
  :lighter nil
  (let ((template (org-grader--make-capture-template)))
    (if org-grader-mode
        (progn
          (org-num-mode -1)
          (add-to-list 'org-capture-templates template)
          (add-hook 'org-checkbox-statistics-hook #'org-grader-points-property-update))
      (let ((templates (remove template org-capture-templates)))
        (setq org-capture-templates templates)
        (remove-hook 'org-checkbox-statistics-hook #'org-grader-points-property-update t)))))

(provide 'org-grader)
;;; org-grader.el ends here
