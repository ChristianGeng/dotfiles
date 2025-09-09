
(require 'org)

(defun my-tbl-export (name)
  "Search for table named `NAME` and export."
  (interactive "s")
  (outline-show-all)
  (let ((case-fold-search t))
    (if (search-forward-regexp (concat "#\\+TBLNAME: +" name) nil t)
    (progn
      (next-line)
      (org-table-export (format "%s.csv" name) "orgtbl-to-csv")))))
