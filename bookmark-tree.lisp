(defpackage :bookmark-tree
  (:nicknames :bm-tree)
  (:shadowing-import-from :cl-containers
                          #:enqueue #:dequeue
                          #:filter #:finish)
  (:use :cl :ol :iterate :cl-containers
        :bookmarks)
  (:export
   #:build-tree
   #:category
   #:walk-bm-tree
   #:walk-on))

(in-package :bookmark-tree)

;;; generate a hierarchical structure from the categorised (tagged)
;;; bookmark table

(defun classify (bookmark-list &optional blacklist)
  "produce a table mapping all categories (except those in
`blacklist') to lists of bookmarks contained in `bookmark-list'."
  ;; every call of bookmark-generator should give a bookmark
  (let ((category-table (make-hash-table)))
    (flet ((store (category bookmark)
             (push bookmark (gethash category category-table))))
      (iterate-elements bookmark-list
                        (lambda (bm)
                          (aif (set-difference (categories bm) blacklist)
                               (dolist (c it) (store c bm))
                               (store :empty bm)))))
    category-table))

(defun table-sizes (table &optional include-empty)
  "For a hash-`table' of sequences, find the keys with the longest
sequence."
  (let ((sizes (make-array 10 :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v)
               (if (or include-empty (not (eq k :empty)))
                (vector-push-extend (cons k (length v)) sizes)))
             table)
    (sort sizes #'> :key #'cdr)))

(defun largest-key (table)
  "For a hash-`table' of sequences, find the key with the longest
sequence, break ties by sorting alphabetically."
  (let ((key nil)
        (size -1))
    (maphash (lambda (k v &aux (l (length v)))
               (cond ((eq k :empty))
                     ((> l size) (setf key k
                                       size l))
                     ((and (= l size)
                           (string-greaterp (mkstr key) (mkstr k)))
                      (setf key k))))
             table)
    (values key size)))

(defun sort-bookmarks (bookmark-list)
  (sort bookmark-list #'string-not-greaterp :key #'title))


(defun build-tree (bookmark-list &optional blacklist)
  (let* ((cls1 (classify bookmark-list blacklist))
         (tree (build-tree-part cls1 blacklist (size bookmark-list)))
         (empty (sort-bookmarks (gethash :empty cls1))))
    (if (length=0 empty)
        tree
        (append1 tree empty))))

(defpar refinement-limit 2)


(defun build-tree-part (cls1 blacklist &optional (nr-of-bookmarks -2))
  ;; choose the category with the most entries
  (mvbind (cat1 size1) (largest-key cls1)
    (when cat1
      (let* ((cat1-bookmarks (gethash cat1 cls1))
             (blacklist-1 (cons cat1 blacklist)))
        ;; remove category from the table
        (remhash cat1 cls1)
        ;; remove the bookmarks in `cat1' from the other lists for the
        ;; other categories
        (maphash (lambda (k v) (aif (nset-difference v cat1-bookmarks)
                               (setf (gethash k cls1) it)
                               (remhash  k cls1)))
                 cls1)
        (cons (cond
                ;; if this category is small, don't group further
                ((<= size1 refinement-limit)
                 (append1 (sort-bookmarks cat1-bookmarks)
                          (build-tree-part cls1 blacklist-1)))
                ;; if this category contains all bookmarks considered,
                ;; don't use grouping
                ((= nr-of-bookmarks size1)
                 (build-tree cat1-bookmarks blacklist-1))
                (t (cons cat1 (build-tree cat1-bookmarks blacklist-1))))
              (build-tree-part cls1 blacklist-1))))))

;; todo add mechanisms to allow prioritisation of categories


;;; iteration on trees
(defun walk-bm-tree% (tree category-fun bookmark-fun)
  (labels ((recurse (tree)
             (cond ((null tree))
                   ((atom tree)
                    (funcall bookmark-fun tree))
                   ((and (symbolp (car tree))
                         (category-p (car tree)))
                    (funcall category-fun (car tree)
                             (lambda () (recurse (cdr tree)))))
                   (t (recurse (car tree))
                      (recurse (cdr tree))))))
    (recurse tree)))

(defmacro! walk-bm-tree (tree category-expr bookmark-expr)
  `(walk-bm-tree% ,tree
                  (lambda (bm:category ,g!continue)
                    (macrolet ((walk-on () `(funcall ,',g!continue)))
                      ,category-expr))
                  (lambda (bm:bookmark)
                    ,bookmark-expr)))


