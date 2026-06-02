;;; org-tsv2markdown.el --- Convert org markdown tsv formats for table items  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, outline, hideshow
;; URL: https://codeberg.org/Anoncheg/org-tsv2markdown
;; Version: 0.1
;; Created: 2 jun 2026
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>


;;; Commentary:
;;

;;; Code:

(defun org-tsv2markdown-tsv2org (beg end)
  "Convert tab-separated lines in the region into an Org mode table.
Each line is split at tabs ('\\t'), replaced with Org table column separator '|'.
The region is bounded by BEG and END."
  (interactive "r")
  (when (region-active-p)
    (save-excursion
      (save-restriction
        (narrow-to-region (1- beg) (1+ end))
          (goto-char end)
          (while (bolp)
            (backward-char))
          (setq end (point))
          (replace-regexp-in-region "[\t]+" "|" beg end)
          (replace-regexp-in-region "^" "|" beg end)
          (replace-regexp-in-region "\n" "|\n" beg end)
          (insert "|") ; at the end of the last line
          (goto-char beg)
          (unless (eq end (line-end-position)) ; if more than one line
            (forward-line)
            (insert "|-\n")
            (goto-char beg))
          (forward-char)))))

(defun org-tsv2markdown-org2markdown (beg end)
  "Convert an Org mode table in the region back to a Markdown table.
Dynamically counts columns to generate the correct number of Markdown separators.
The region is bounded by BEG and END."
  (interactive "r")
  (when (region-active-p)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))

        ;; 1. Find the first data/header row to count the columns
        (let ((col-count 0)
              (separator-str ""))
          (when (re-search-forward "^[ \t]*|[^-\n]" nil t)
            (beginning-of-line)
            ;; Count the number of '|' characters in this row, minus 1
            (setq col-count (1- (count-matches "|" (line-beginning-position) (line-end-position))))

            ;; Construct the dynamic Markdown separator (e.g., "|---|---|---|")
            (when (> col-count 0)
              (setq separator-str (concat "|" (mapconcat 'identity (make-list col-count "---") "|") "|")))

            ;; 2. Replace any Org hline with our dynamically generated Markdown separator
            (goto-char (point-min))
            (while (re-search-forward "^\\([ \t]*\\)|[-+]+|$" nil t)
              (replace-match (concat "\\1" separator-str) t nil))))

        ;; 3. Clean up trailing whitespace before newlines
        (replace-regexp-in-region "[ \t]+$" "" (point-min) (point-max))))))

(defun org-tsv2markdown-table-to-tsv (beg end)
  "Convert an Org or Markdown table in the region into tab-separated text (TSV).
Automatically detects and strips out either style of separator line (hlines).
The region is bounded by BEG and END."
  (interactive "r")
  (when (region-active-p)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)

        ;; 1. Automatically delete Org (|-+-) or Markdown (|---|) separators
        ;; matches any line starting with optionally indented '|' containing only dashes, pluses, spaces, or pipes.
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*|[-+| ]+|\n" nil t)
          (replace-match ""))

        ;; 2. Clean "|-" lines
        (flush-lines "^|-$" (point-min) (point-max))

        ;; 3. Convert internal pipe delimiters (and surrounding padding spaces) into a single tab
        (replace-regexp-in-region "[ \t]*|[ \t]*" "\t" (point-min) (point-max))

        ;; 4. Clean up the rogue leading tabs caused by the old left-border pipes
        (replace-regexp-in-region "^\t" "" (point-min) (point-max))

        ;; 5. Clean up the rogue trailing tabs caused by the old right-border pipes
        (replace-regexp-in-region "\t$" "" (point-min) (point-max))

        ;; 6. Clean up any trailing whitespace left over on empty lines
        (replace-regexp-in-region "[ \t]+$" "" (point-min) (point-max))))))

(defun org-tsv2markdown-items2org (beg end)
  "Reformat lines in region from markdown style to Org-style items.

- Replaces '- **LABEL:**' with '- LABEL ::'
- Removes all '**'
- Replaces the first colon in each line with ':: '"
  (interactive "r")
  (when (region-active-p)
    (save-excursion
      (save-restriction
        (let ((beg (save-excursion (goto-char beg) (line-beginning-position)))
              (end (save-excursion (goto-char end) (line-end-position))))
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (< (point) (point-max))
            (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                   ;; Convert '- **LABEL:**' to '- LABEL ::'
                   (line2 (replace-regexp-in-string
                           "^[ \t]*- \\*\\*\\([^*]+\\):\\*\\*" "- \\1 ::" line))
                   ;; Replace first colon with ':: '
                   (line2 (replace-regexp-in-string "^[ \t]*\\([^:]+\\): ?" "- \\1 :: " line2 1)))
              (unless (string-equal line line2)
                (delete-region (line-beginning-position) (line-end-position))
                (insert line2))
              (forward-line 1))))))))

(defun org-tsv2markdown-reformat-table-or-items (beg end)
  "Reformat selected region as Org table or Org-style items.

If region is in Org mode and contains tabs, convert to Org table.
Else, reformat markdown items ('- **LABEL:**') to Org style ('- LABEL ::').
If a single line, surround with '*' as simple emphasis."
  (interactive "r")
  (cond
   ((and (derived-mode-p 'org-mode)
         (region-active-p)
         (save-excursion (goto-char beg) (and (re-search-forward "^|" end t) (re-search-forward "|$" end t))))
    (org-tsv2markdown-table-to-tsv beg end))
   ((and (derived-mode-p 'org-mode)
         (region-active-p)
         (save-excursion (goto-char beg) (re-search-forward "\t" end t)))
    (org-tsv2markdown-tsv2org beg end))
   ((and (region-active-p)
         (> (count-lines beg end) 1))
    (org-tsv2markdown-items2org beg end))
   (t
    ;; Single line: surround with '*' for emphasis
    (let ((beg (if (region-active-p) beg (line-beginning-position)))
          (end (if (region-active-p) end (line-end-position))))
      (save-excursion
        (goto-char end)
        (insert "*")
        (when (let ((c (char-after))) (and c (not (memq c '(?\n ?\r)))))
          (insert " "))
        (goto-char beg)
        (when (let ((c (char-before))) (and c (not (memq c '(?\n ?\r)))))
          (insert " "))
        (insert "*"))))))

;;;; provide

(provide 'org-tsv2markdown)

;;; org-tsv2markdown.el ends here
