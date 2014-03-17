(in-package :cl-user)

(defpackage :bookmarks
  (:nicknames :bm)
  (:use :cl :ol :iterate
        :cl-containers)
  (:export
   #:add-bookmark
   #:bookmark-exists))

(defpackage :bookmark-categories
  (:nicknames :cat))

(in-package :bookmarks)

(defvar bookmarks (make-container 'simple-associative-container
                                  :test 'equal))

;; todo timestamp for url?? -> sort by date
(defclass bookmark ()
  ((title :initarg :title
          :initform ""
          :accessor title)
   (url :initarg :url
        :initform "http://"
        :accessor url)
   (user-categories :initarg :user-categories
                    :initform nil
                    :accessor user-categories)
   title-categories
   url-categories
   auto-categories
   (categories :accessor categories)))

(create-standard-print-object bookmark title url (user-categories))

(defun recons (a d c)
  (setf (car c) a
        (cdr c) d)
  c)

(defun cat (category)
  (if (symbolp category)
      category
      (intern category :cat)))

(defun mkcat (x)
  (cond ((and (consp x) (symbolp (cdr x)))
         x)
        ((and (consp x) (stringp (cdr x)))
         (recons (car x) (cat (cdr x)) x))
        ((symbolp x) (cons (symbol-value x) x))
        ((stringp x) (cons x (cat x)))
        (t (error "invalid title->category spec ~A" x))))


(defpar title->category
    (mapcar #'mkcat
            '("ARMA"
              "DCS"
              "lisp"
              "Warthog"
              ("A-10" . "Warthog")
              ("A10" . "Warthog")
              "Blackshark"
              ("black shark" . "Blackshark")
              ("Ka-50" . "Blackshark")
              )))

(defpar url->category
    (mapcar #'cat
            '("SimHQ"
              "ImDB"
              "youtube"
              "wikipedia"
              "GitHub")))

(defvar category-logic nil)

(defmethod match ((variant (eql 'title-categories)) t->c title)
  (if (search (car t->c) title :test #'char-equal)
      (cdr t->c)
      nil))

(defmethod match ((variant (eql 'url-categories)) u->c url)
  (handler-case (match 'title-categories (cons (symbol-name u->c) u->c)
                       (split-sequence-element url #\/ 2))
    (split-sequence-overflow () nil)))

(define-condition split-sequence-overflow ()
  (sequence separator index))

(defun split-sequence-element (sequence separator index &key test)
  (labels ((next-position (start)
             (position separator sequence :start start :test test))
           (nth-position (start n)
             (if (zerop n) start
                 (nth-position (+ 1 (or (next-position start)
                                        (error 'split-sequence-overflow :sequence sequence :separator separator :index index))) (- n 1)))))
    (let* ((begin-elt (nth-position 0 index))
           (end-elt (next-position begin-elt)))
      (subseq sequence begin-elt end-elt))))

(defmethod compute-categories ((variant (eql 'title-categories)) (bm bookmark))
  (with-slots (title) bm
    (filter (lambda (x) (match variant x title)) title->category)))

(defmethod compute-categories ((variant (eql 'url-categories)) (bm bookmark))
  (with-slots (url) bm
    (filter (lambda (x) (match variant x url)) url->category)))

(defun update-categories (variant bm)
  (setf (slot-value bm variant) (compute-categories variant bm)))

(defun update-all-categories (bm)
  (dolist (variant '(title-categories url-categories auto-categories categories))
    (update-categories variant bm)))

(defmethod compute-categories ((variant (eql 'categories)) (bm bookmark))
  ;; todo keep categories sorted.  
  (with-slots #1=(user-categories title-categories url-categories auto-categories) bm
              (append . #1#)))

(defmethod compute-categories ((variant (eql 'auto-categories)) (bm bookmark))
  (let ((category-set (make-hash-table))
        auto-cats
        (create-flag t))
    (labels ((insert (x) (setf (gethash x category-set) t))
             (insert-list% (list) (mapc #'insert list))
             (add-cat (x) (unless (gethash x category-set)
                            (insert x)
                            (push x auto-cats)
                            (setf create-flag t)))
             (apply-logic (logic)
               (aif (funcall logic category-set)
                    (add-cat it))))
      ;; load the stuff available  so far
      (dolist (variant '(user-categories title-categories url-categories))
        (insert-list% (slot-value bm variant)))
      ;; fixpoint iteration on the logic
      (do () ((not create-flag))
        (setf create-flag nil)
        (mapc #'apply-logic category-logic))
      auto-cats)))

(defun remove* (list sequence &key test)
  (if (null list)
      sequence
      (remove* (cdr list) (remove (car list) sequence :test test) :test test)))


(defmacro! create-category-logic (formula)
  ;; formulas can contain `and', `or' and `not'
  (let ((variables (remove* '(and or not) (flatten formula) :test #'eq)))
    `(lambda (,g!table)
       (let ,(mapcar #`(,a1 (gethash ',a1 ,g!table)) variables)
         ,formula))))
;; todo do we allow creation of rules at runtime? that might require a
;; different approach

(defun get-bookmark (url)
  (item-at bookmarks url))

(define-condition bookmark-exists ()
  ((bookmark :initarg :bookmark
             :reader bookmark)))

(defun add-bookmark (url &optional (title "") categories)
  (aif (get-bookmark url)
       (error 'bookmark-exists :bookmark it))
  ;; todo error recovery strategy?
  (let ((bm (make-instance 'bookmark :url url :title title :user-categories categories)))
    (update-all-categories bm)
    (setf (item-at bookmarks url) bm)))



(defun all-bookmarks ()
  bookmarks)


;;; some utility functions
(define-condition empty-parameter ()
  ((name :initarg :name
         :initform nil)))

(defmacro ensure-non-empty-param (&rest params)
  `(progn
     ,@(mapcar #`(when (length=0 ,a1)
                   (error 'empty-parameter :name ',a1))
               params)))

(defun parse-positive-integer (string)
  (if (every #'digit-char-p string)
      (parse-integer string)))


