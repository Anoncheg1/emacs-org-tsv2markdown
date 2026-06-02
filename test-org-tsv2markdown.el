(require 'ert)

(defun my--test-helper (input-text expected-text)
  "Helper function to setup a temporary buffer, insert INPUT-TEXT,
run `org-tsv2markdown-org2markdown` on the whole buffer, and assert it matches EXPECTED-TEXT."
  (with-temp-buffer
    (insert input-text)
    ;; Activate the region for the buffer to simulate user selection
    (transient-mark-mode 1)
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    ;; Run the function under test
    (org-tsv2markdown-org2markdown (region-beginning) (region-end))
    ;; Assert the result
    (should (string= (buffer-string) expected-text))))

(ert-deftest test-org2markdown-standard ()
  "Test Case 1: Convert a standard 2x2 Org table with plus signs in the hline."
  (let ((input (concat "| Header 1 | Header 2 |\n"
                       "|----------+----------|\n"
                       "| Cell 1   | Cell 2   |\n"
                       "| Cell 3   | Cell 4   |"))
        (expected (concat "| Header 1 | Header 2 |\n"
                          "|---|---|\n"
                          "| Cell 1   | Cell 2   |\n"
                          "| Cell 3   | Cell 4   |")))
    (my--test-helper input expected)))

(ert-deftest test-org2markdown-dashes-only ()
  "Test Case 2: Convert a minimalist 2-column Org table with only dashes in the hline."
  (let ((input (concat "| Name | Age |\n"
                       "|------------|\n"
                       "| Alice| 30  |\n"
                       "| Bob  | 25  |"))
        (expected (concat "| Name | Age |\n"
                          "|---|---|\n"
                          "| Alice| 30  |\n"
                          "| Bob  | 25  |")))
    (my--test-helper input expected)))

(ert-deftest test-org2markdown-edge-case-hyphens ()
  "Test Case 3: Ensure content hyphens and dashed text inside cells are not mangled."
  (let ((input (concat "| Task | Status |\n"
                       "|------+--------|\n"
                       "| Fix the login-page bug | Done |\n"
                       "| --- This is a note inside a cell --- | Pending |"))
        (expected (concat "| Task | Status |\n"
                          "|---|---|\n"
                          "| Fix the login-page bug | Done |\n"
                          "| --- This is a note inside a cell --- | Pending |")))
    (my--test-helper input expected)))

(ert-deftest test-org2markdown-multiple-hlines ()
  "Test Case 4: Handle single-column tables with multiple horizontal separator lines."
  (let ((input (concat "| Top Header |\n"
                       "|------------|\n"
                       "| Sub Header |\n"
                       "|------------|\n"
                       "| Data Row   |"))
        ;; Dynamically scales to 1 column: |---|
        (expected (concat "| Top Header |\n"
                          "|---|\n"
                          "| Sub Header |\n"
                          "|---|\n"
                          "| Data Row   |")))
    (my--test-helper input expected)))

(ert-deftest test-org2markdown-indented ()
  "Test Case 5: Handle tables that are indented with spaces while preserving indentation."
  (let ((input (concat "  | Header 1 | Header 2 |\n"
                       "  |----------+----------|\n"
                       "  | Cell 1   | Cell 2   |"))
        ;; The updated function handles indentation and adds the correct number of separators
        (expected (concat "  | Header 1 | Header 2 |\n"
                          "  |---|---|\n"
                          "  | Cell 1   | Cell 2   |")))
    (my--test-helper input expected)))

(ert-deftest test-org2markdown-no-hline ()
  "Test Case 6: A table completely lacking an hline should remain untouched."
  (let ((input (concat "| Col 1 | Col 2 |\n"
                       "| Val 1 | Val 2 |"))
        (expected (concat "| Col 1 | Col 2 |\n"
                          "| Val 1 | Val 2 |")))
    (my--test-helper input expected)))

(ert-deftest test-org2markdown-whitespace-in-hline ()
  "Test Case 7: Hlines containing spaces like | --- + --- | shouldn't match strict separator regex."
  (let ((input (concat "| Header 1 | Header 2 |\n"
                       "| --- + --- |\n"
                       "| Cell 1   | Cell 2   |"))
        (expected (concat "| Header 1 | Header 2 |\n"
                          "| --- + --- |\n"
                          "| Cell 1   | Cell 2   |")))
    (my--test-helper input expected)))

(ert-deftest test-org2markdown-empty-cells ()
  "Test Case 8: Complex 3-column layout with empty cells scales correctly to 3 separators."
  (let ((input (concat "| H1 | H2 | H3 |\n"
                       "|----+----+----|\n"
                       "|    |    | X  |\n"
                       "||||"))
        ;; Dynamically matches the 3 columns: |---|---|---|
        (expected (concat "| H1 | H2 | H3 |\n"
                          "|---|---|---|\n"
                          "|    |    | X  |\n"
                          "||||")))
    (my--test-helper input expected)))


(defun my--test-helper-universal-tsv (input-text expected-text)
  "Helper to test the universal table-to-tsv conversion function."
  (with-temp-buffer
    (insert input-text)
    (transient-mark-mode 1)
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (org-tsv2markdown-table-to-tsv (region-beginning) (region-end))
    (should (string= (buffer-string) expected-text))))

(ert-deftest org-table-to-tsv-from-markdown ()
  "Verify conversion from a standard Markdown table to TSV."
  (let ((input (concat "| Header 1 | Header 2 |\n"
                       "| --- | --- |\n"
                       "| Cell 1 | Cell 2 |"))
        (expected (concat "Header 1\tHeader 2\n"
                          "Cell 1\tCell 2")))
    (my--test-helper-universal-tsv input expected)))

(ert-deftest org-table-to-tsv-from-org ()
  "Verify conversion from a standard Org-mode table to TSV."
  (let ((input (concat "| Header 1 | Header 2 |\n"
                       "|----------+----------|\n"
                       "| Cell 1   | Cell 2   |"))
        (expected (concat "Header 1\tHeader 2\n"
                          "Cell 1\tCell 2")))
    (my--test-helper-universal-tsv input expected)))

(ert-deftest org-table-to-tsv-from-org-our ()
  "Verify conversion from a standard Org-mode table to TSV."
  (let ((input (concat "|Header 1|Header 2|\n"
                       "|-\n"
                       "|Cell 1|Cell 2|"))
        (expected (concat "Header 1\tHeader 2\n"
                          "Cell 1\tCell 2")))
    (my--test-helper-universal-tsv input expected)))
