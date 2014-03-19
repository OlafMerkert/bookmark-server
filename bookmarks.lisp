(in-package :cl-user)

(defpackage :bookmarks
  (:nicknames :bm)
  (:shadowing-import-from :cl-containers
                          #:enqueue #:dequeue
                          #:filter #:finish)
  (:use :cl :ol :iterate
        :cl-containers)
  (:export
   #:add-bookmark
   #:bookmark-exists
   #:all-bookmarks
   #:bookmarks-in-category
   #:bookmark
   #:title
   #:url
   #:categories
   #:user-categories
   #:category-p
   #:all-known-categories
   #:add-category
   #:category-error
   #:user-category-present
   #:user-category-not-present
   #:bookmark-error
   #:bookmark-not-exists
   #:remove-category
   #:get-bookmark%
   #:cannot-remove-auto-category
   #:add-categories))

(defpackage :bookmark-categories
  (:nicknames :cat))

(in-package :bookmarks)

(defvar bookmarks (make-container 'simple-associative-container
                                  :test 'equal))

;; todo timestamp for url?? -> sort by date
(defclass bookmark ()
  ((title :initarg :title
          :initform ""
          :reader title)
   (url :initarg :url
        :initform "http://"
        :reader url)
   (user-categories :initarg :user-categories
                    :initform nil
                    :reader user-categories)
   (title-categories :reader title-categories)
   (url-categories :reader url-categories)
   (auto-categories :reader auto-categories)
   (categories :reader categories)))

(create-standard-print-object bookmark title url (user-categories))

;;; conditions
(define-condition split-sequence-overflow ()
  ((sequence :initarg :sequence)
   (separator :initarg :separator)
   (index :initarg :index)))

(define-condition bookmark-error ()
  ((bookmark :initarg :bookmark
             :reader bookmark)))

(define-condition bookmark-exists (bookmark-error)
  ())

(define-condition bookmark-not-exists (bookmark-error)
  ())

(define-condition category-error ()
  ((category :initarg :category
             :reader category)))

(define-condition user-category-present (category-error)
  ())

(define-condition user-category-not-present (category-error)
  ())

(define-condition cannot-remove-auto-category (category-error)
  ())

(define-condition invalid-category-identifier ()
  ((category :initarg :category
             :reader category)))

;;; 
(defun recons (a d c)
  (setf (car c) a
        (cdr c) d)
  c)

(defun cat (category)
  (cond ((length=0 category)
         (error 'invalid-category-identifier :category category))
        ((symbolp category)
         category)
        ((stringp category)
         (intern category :cat))
        (t (error 'invalid-category-identifier :category category))))

(defun mkcat (x)
  (cond ((and (consp x) (symbolp (cdr x)))
         x)
        ((and (consp x) (stringp (cdr x)))
         (recons (car x) (cat (cdr x)) x))
        ((symbolp x) (cons (symbol-name x) x))
        ((stringp x) (cons x (cat x)))
        (t (error "invalid title->category spec ~A" x))))

(defmacro! create-category-logic (formula category)
  ;; formulas can contain `and', `or' and `not'
  (let ((variables (remove* '(and or not) (flatten formula) :test #'eq)))
    `(lambda (,g!table)
       (let ,(mapcar #`(,a1 (gethash ',a1 ,g!table)) variables)
         (when ,formula
           ',(cat category))))))
;; todo do we allow creation of rules at runtime? that might require a

;; todo use regexp for more flexibility
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
              "FSX"
              "test"
              ("review" . "test")
              "math"
              "Feynman"
              "algebra"
              "geometry"
              "manual"
              "News"
              ("Nachrichten" . "News")
              "bug"
              "Karl May"
              "Rowan Atkinson"
              "Comic"
              ("Cartoon" . "Comic")
              ("TV Series" . "TV")
              "dansk"
              ("Danmark" . "dansk")
              ("Dänisch" . "dansk")
              ("Dänemark" . "dansk")
              ("det nye talkshow" . "dansk")
              "simulator"
              ("simu" . "simulator")
              "python"
              "numpy"
              "scipy"
              "pyqt"
              "gtk"
              ("eclm" . "lisp")
              "clim"
              "blog"
              "proof"
              "wetter"
              ("meteo" . "wetter")
              ("prevision" . "wetter")
              "dict"
              ("wörterbuch" . "dict")
              "museum"
              ("museen" . "museum")
              "garten"
              "flug"
              ("flight" . "flug")
              ("flieger" . "flug")
              ("fighter jet" . "flug")
              ("gunship" . "heli")
              "heli"
              ("hubschrauber" . "heli")
              "airport"
              ("aeroporto" . "airport")
              ("flughafen" . "airport")
              "modell"
              ("model" . "modell")
              "buch"
              "fahrrad"
              ("bike" . "fahrrad")
              "git"
              "blender"
              "photoshop"
              "emacs"
              "howto"
              ("how to" . "howto")
              ("how-to" . "howto")
              "fedora"
              "debian"
              "arch"
              "windows"
              "linux"
              "gentoo"
              "ubuntu"
              "program"
              )))

(defpar url->category
    (mapcar #'mkcat
            '("SimHQ"
              "ImDB"
              "youtube"
              "wikipedia"
              "GitHub"
              "Unibas"
              "SNS"
              "gutenberg"
              "amazon"
              "geizhals"
              (".dk" . "dansk")
              ("digitalcombatsimulator" . "DCS")
              "forums"
              ("eagle.ru" . "DCS")
              ("bistudio.com" . "ARMA")
              "cliki"
              ("ted.com" . "TEDtalk")
              "lisp"
              ("blogspot" . "blog")
              "arxiv"
              "wetter"
              ("meteo" . "wetter")
              "math"
              "sourceforge"
              "emacswiki"
              "armaholic"
              )))

(defpar category-logic
    (list (create-category-logic cat::|cliki| cat::|lisp|)
          (create-category-logic cat::|clim| cat::|lisp|)))

(defmethod match ((variant (eql 'title-categories)) t->c title)
  (if (search (car t->c) title :test #'char-equal)
      (cdr t->c)
      nil))

(defmethod match ((variant (eql 'url-categories)) u->c url)
  (handler-case (match 'title-categories u->c
                       (split-sequence-element url #\/ 2))
    (split-sequence-overflow () nil)))


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
    (ol:filter (lambda (x) (match variant x title)) title->category)))

(defmethod compute-categories ((variant (eql 'url-categories)) (bm bookmark))
  (with-slots (url) bm
    (ol:filter (lambda (x) (match variant x url)) url->category)))

(defun update-categories (variant bm)
  (setf (slot-value bm variant) (compute-categories variant bm)))

(defun update-all-categories (bm)
  (dolist (variant '(title-categories url-categories auto-categories categories))
    (update-categories variant bm)))

(defmethod compute-categories ((variant (eql 'categories)) (bm bookmark))
  ;; todo keep categories sorted.  
  (with-slots #1=(user-categories title-categories url-categories auto-categories) bm
              (remove-duplicates (append . #1#))))

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





;; different approach

(defun get-bookmark (url)
  (mvbind (bm present) (item-at bookmarks url)
    (if present bm
        (error 'bookmark-not-exists :bookmark url))))

(defun get-bookmark% (url)
  (item-at bookmarks url))

(defgeneric edit-bookmark (bookmark prop new-value))
;; todo perhaps better to use generalised variables??

(defmethod edit-bookmark ((bm bookmark) (prop (eql 'url)) (new-value string))
  (setf (slot-value bm prop) new-value)
  (update-categories 'url-categories bm)
  (update-categories 'auto-categories bm)
  (update-categories 'categories bm))

(defmethod edit-bookmark ((bm bookmark) (prop (eql 'title)) (new-value string))
  (setf (slot-value bm prop) new-value)
  (update-categories 'title-categories bm)
  (update-categories 'auto-categories bm)
  (update-categories 'categories bm))

(defmethod edit-bookmark ((bm bookmark) (prop (eql 'url+title)) (new-value list))
  (setf (slot-value bm 'url) (car new-value)
        (slot-value bm 'title) (cdr new-value))
  (update-all-categories bm))

(defmethod edit-bookmark ((bm bookmark) (prop (eql 'user-categories)) (new-value list))
  (setf (slot-value bm prop) (mapcar #'cat new-value))
  (update-categories 'auto-categories bm)
  (update-categories 'categories bm))

(bind-multi ((prop url title user-categories))
  (defsetf prop (bm) (new-value) `(edit-bookmark ,bm 'prop ,new-value)))

(defmethod edit-bookmark ((bm string) prop new-value)
  (edit-bookmark (get-bookmark bm) prop new-value))

(defmethod add-categories ((bm string) categories)
  (add-categories (get-bookmark bm) categories))

(defmethod add-categories ((bm bookmark) categories)
  (let ((categories (mapcar #'cat categories)))
    (setf (user-categories bm)
          (union categories (user-categories bm)))))

(defmethod remove-category ((bm string) category)
  (remove-category (get-bookmark bm) category))

(defmethod remove-category ((bm bookmark) category)
  (let ((category (cat category))
        (uc (user-categories bm)))
    (cond ((member category uc)
           (setf (user-categories bm)
                 (remove category uc)))
          ((or (member category (title-categories bm))
               (member category (url-categories bm))
               (member category (auto-categories bm)))
           (error 'cannot-remove-auto-category :category category))
          (t (error 'user-category-not-present)))))


(defun add-bookmark (url &optional (title "") categories)
  (aif (get-bookmark% url)
       (error 'bookmark-exists :bookmark it))
  ;; todo error recovery strategy?
  (let ((bm (make-instance 'bookmark :url url :title title :user-categories categories)))
    (update-all-categories bm)
    (setf (item-at bookmarks url) bm)))

(defun update-all-bookmark-categories ()
  (iterate-key-value bookmarks
                     (ilambda (k bm) (update-all-categories bm))))



(defun all-bookmarks (&optional predicate)
  (let (bms)
    (iterate-key-value bookmarks
                       (ilambda (k bm) (when (or (not predicate)
                                            (funcall predicate bm))
                                    (push bm bms))))
    bms))

(defun bookmarks-in-category (category)
  (all-bookmarks (lambda (bm) (member category (categories bm) :test 'eq))))


(defun all-categories ()
  (let (categories)
    (iterate-key-value bookmarks
                       (ilambda (k bm) (aif (categories bm) (push it categories))))
    (remove-duplicates (flatten categories))))

(defun all-known-categories ()
  (let (categories)
    (do-symbols (s :bookmark-categories)
      (push s categories))
    (sort categories #'string<= :key #'symbol-name)))

(defun category-p (cat)
  (or (and (symbolp cat) cat)
      (and (stringp cat)
           (find-symbol cat :bookmark-categories))))


;; saving/loading bookmarks from custom JSON
(defun save-bookmarks (pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    ;; todo
    (cl-json:with-array (stream)
      (iterate-key-value
       bookmarks
       (ilambda (key bm)
         (cl-json:as-array-member (stream)
           (cl-json:with-object (stream)
             (cl-json:encode-object-member 'url (url bm) stream)
             (cl-json:encode-object-member 'title (title bm) stream)
             (cl-json:encode-object-member 'categories
                                           (mapcar #'symbol-name (user-categories bm))
                                           stream))))))))

(defun load-bookmarks (pathname)
  (let ((json (cl-json:decode-json-from-source pathname)))
    ;; todo
    (mapc (lambda (bm)
            (alist-bind (url title categories) bm
              (add-bookmark url title categories)))
          json)
    bookmarks))

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


