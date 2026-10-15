#!/bin/bash
emacs -Q --batch --no-site-file -l ert.el -l org-tsv2markdown.el -l test-org-tsv2markdown.el -f ert-run-tests-batch-and-exit || exit 1
