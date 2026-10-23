;;; vui-table-span-test.el --- Table headers over several columns -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; A column of a `vui-table' with `:header-span N' puts its header over
;; itself and the N-1 columns after it.  The cells stay in their own
;; columns.

;;; Code:

(require 'buttercup)
(require 'vui)

(defun vts--render (&rest args)
  "Return the text of a `vui-table' made with ARGS."
  (with-temp-buffer
    (vui-render (apply #'vui-table args))
    (buffer-substring-no-properties (point-min) (point-max))))

(describe "vui-table :header-span"
  (it "puts a header over the columns of its group"
    (expect (vts--render
             :columns '((:header "ID")
                        (:header "SEVERITY" :header-span 2)
                        (:align :right)
                        (:header "TITLE")
                        (:header "DEFECTS" :header-span 2)
                        ())
             :rows '(("12" "high" "9.8" "Heap" "6" "2/3/1")))
            :to-equal (concat "ID SEVERITY TITLE DEFECTS\n"
                              "12 high 9.8 Heap  6 2/3/1")))

  (it "widens the first column of a group the header does not fit"
    (expect (vts--render
             :columns '((:header "SEVERITY" :header-span 2) () (:header "Z"))
             :rows '(("a" "b" "c")))
            :to-equal "SEVERITY Z\na      b c"))

  (it "lays out a header over one column as before"
    (expect (vts--render
             :columns '((:header "A" :header-span 1) (:header "B"))
             :rows '(("1" "2")))
            :to-equal (vts--render
                       :columns '((:header "A") (:header "B"))
                       :rows '(("1" "2")))))

  (it "draws unicode borders around the group"
    (expect (vts--render
             :columns '((:header "G" :header-span 2) () (:header "Z"))
             :rows '(("a" "b" "c"))
             :border :unicode)
            :to-equal (concat "┌───────┬───┐\n"
                              "│ G     │ Z │\n"
                              "├───┬───┼───┤\n"
                              "│ a │ b │ c │\n"
                              "└───┴───┴───┘")))

  (it "draws ascii borders around the group"
    (expect (vts--render
             :columns '((:header "G" :header-span 2) () (:header "Z"))
             :rows '(("a" "b" "c"))
             :border :ascii)
            :to-equal (concat "+-------+---+\n"
                              "| G     | Z |\n"
                              "+---+---+---+\n"
                              "| a | b | c |\n"
                              "+---+---+---+")))

  (it "keeps the junctions of a separator row in the body"
    (expect (vts--render
             :columns '((:header "G" :header-span 2) ())
             :rows '(("a" "b") :separator ("c" "d"))
             :border :unicode)
            :to-equal (concat "┌───────┐\n"
                              "│ G     │\n"
                              "├───┬───┤\n"
                              "│ a │ b │\n"
                              "├───┼───┤\n"
                              "│ c │ d │\n"
                              "└───┴───┘")))

  (it "widens the group for a wide header with borders"
    (expect (vts--render
             :columns '((:header "GROUPS" :header-span 2) ())
             :rows '(("a" "b"))
             :border :ascii)
            :to-equal (concat "+--------+\n"
                              "| GROUPS |\n"
                              "+----+---+\n"
                              "| a  | b |\n"
                              "+----+---+")))

  (it "keeps a cell whole in the column a header widened past its :width"
    (expect (vts--render
             :columns '((:header "A WIDE GROUP HEADER" :header-span 2
                         :width 5 :truncate t)
                        () (:header "Z"))
             :rows '(("abcdefghij" "b" "c")))
            :to-equal (concat "A WIDE GROUP HEADER Z\n"
                              "abcdefghij        b c")))

  (it "signals for a column in a group with a span of its own"
    (expect (vts--render
             :columns '((:header "G" :header-span 2) (:header-span 2) ())
             :rows '(("a" "b" "c")))
            :to-throw 'error))

  (it "signals for a column in a group with a header of its own"
    (expect (vts--render
             :columns '((:header "G" :header-span 2) (:header "B"))
             :rows '(("a" "b")))
            :to-throw 'error))

  (it "signals for a group past the last column"
    (expect (vts--render
             :columns '((:header "A") (:header "G" :header-span 2))
             :rows '(("a" "b")))
            :to-throw 'error)))

(provide 'vui-table-span-test)
;;; vui-table-span-test.el ends here
