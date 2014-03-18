(defpackage :bookmark-tree
  (:nicknames :bm-tree)
  (:shadowing-import-from :cl-containers
                          #:enqueue #:dequeue
                          #:filter #:finish)
  (:use :cl :ol :iterate :cl-containers
        :bookmarks)
  (:export
   #:build-tree))

(in-package :bookmark-tree)

;;; generate a hierarchical structure from the categorised (tagged)
;;; bookmark table

(defun classify (bookmark-list &optional blacklist)
  ;; every call of bookmark-generator should give a bookmark
  (let ((category-table (make-hash-table)))
    (flet ((store (category bookmark)
             (push bookmark (gethash category category-table))))
      (dolist (bm bookmark-list) 
        (aif (set-difference (categories bm) blacklist)
             (dolist (c it) (store c bm))
             (store :empty bm))))
    category-table))

(defun table-sizes (table &optional include-empty)
  (let ((sizes (make-array 10 :adjustable t :fill-pointer 0)))
    (maphash (lambda (k v)
               (if (or include-empty (not (eq k :empty)))
                (vector-push-extend (cons k (length v)) sizes)))
             table)
    (sort sizes #'> :key #'cdr)))

(defun build-tree (bookmark-list &optional blacklist)
  (let* ((cls1 (classify bookmark-list blacklist))
         (tree (build-tree-part cls1 blacklist))
         (empty (gethash :empty cls1)))
    (if (length=0 empty)
        tree
        (append1 tree empty))))

(defun build-tree-part (cls1 blacklist)
  (let ((cats (table-sizes cls1)))
    (unless (length=0 cats)
      (let* ((cat1 (car (elt cats 0)))
             (cat1bms (gethash cat1 cls1))
             (bl1 (cons cat1 blacklist)))
        ;; remove these categories
        (remhash cat1 cls1)
        (maphash (lambda (k v) (aif (nset-difference v cat1bms)
                               (setf (gethash k cls1) it)
                               (remhash  k cls1)))
                 cls1)
        (cons (cons cat1 (build-tree cat1bms bl1))
              (build-tree-part cls1 bl1))))))



