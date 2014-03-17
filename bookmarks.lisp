(defpackage :bookmarks
  (:nicknames :bm)
  (:use :cl :ol :iterate
        :clsql)
  (:export
   #:connect-database
   #:bookmark
   #:title
   #:url
   #:categories
   #:category
   #:name
   #:bookmarks
   #:all-bookmarks
   #:all-categories
   #:add
   #:get-category-by-id
   #:id
   #:get-by-id
   #:save-changes
   #:delete-object
   #:assign-bookmark-category
   #:unassign-bookmark-category
   #:assign-bookmark-categories
   #:unassign-bookmark-categories
   #:category-by-name
   #:category-by-id-or-name
   #:bookmark-by-url
   #:bookmark-by-id
   #:delete-bookmark
   #:db-object-not-found
   #:class
   #:column
   #:value
   #:empty-parameter))

(in-package :bookmarks)

(file-enable-sql-reader-syntax)

(defparameter bookmarks-database-path
  #P "/var/tmp/bookmarks-database.sqlite")

(setf *default-caching* nil)

(def-view-class bookmark ()
  ((id :db-kind :key
       :db-constraints :not-null
       :type integer
       :initform (sequence-next 'bookmark-id-seq)
       :accessor id)
   (title :initarg :title
          :accessor title
          :type (string 100))
   (url :initarg :url
        :accessor url
        :type (string 200))
   (categories :accessor categories%
               :db-kind :join
               :db-info (:join-class bookmark-category
                                     :home-key id
                                     :foreign-key bookmark-id
                                     :target-slot category
                                     :set t)))
  (:base-table bookmark))

(defun categories (bookmark)
  (mapcar #'first (categories% bookmark)))


;; todo figure out why we get a pair with the join class

(create-standard-print-object bookmark (id) title url)

(def-view-class category ()
  ((id :db-kind :key
       :db-constraints :not-null
       :type integer
       :initform (sequence-next 'category-id-seq)
       :accessor id)
   (name :initarg :name
         :accessor name
         :type (string 100))
   (bookmarks :accessor bookmarks%
              :db-kind :join
              :db-info (:join-class bookmark-category
                                    :home-key id
                                    :foreign-key category-id
                                    :target-slot bookmark
                                    :set t)))
  (:base-table category))

(defun bookmarks (category)
  (mapcar #'first (bookmarks% category)))


(create-standard-print-object category (id) name)


(def-view-class bookmark-category ()
  ((bookmark-id :initarg :bookmark-id
                :accessor bookmark-id
                :type integer
                :db-constraints :not-null)
   (category-id :initarg :category-id
                :accessor category-id
                :type integer
                :db-constraints :not-null)
   ;; todo constraint on both columns
   (bookmark :db-kind :join
             :db-info (:join-class bookmark
                                   :home-key bookmark-id
                                   :foreign-key id
                                   :retrieval :immediate))
   (category :db-kind :join
             :db-info (:join-class category
                                   :home-key category-id
                                   :foreign-key id
                                   :retrieval :immediate)))
  (:base-table bookmark-category))

(defun initialise-database ()
  (dolist (sequence '(bookmark-id-seq category-id-seq))
    (unless (sequence-exists-p sequence)
      (create-sequence sequence)))
  (dolist (table '(bookmark category bookmark-category))
    (unless (table-exists-p table)
      (create-view-from-class table))))

(defun connect-database (&key initialise)
  (connect (list bookmarks-database-path) :database-type :sqlite3)
  (if initialise
      (initialise-database)))

(defun clear-database (confirm)
  (when (eq confirm :confirm)
    (delete-records :from 'bookmark)
    (delete-records :from 'bookmark-category)
    (delete-records :from 'category)
    t))

;;; often used selects
(define-condition db-object-not-found ()
  ((class :initarg :class
          :initform nil)
   (column :initarg :column
           :initform nil)
   (value :initarg :value
          :initform nil)))

(defun all-bookmarks ()
  (select 'bookmark :order-by [title]
          :flatp t :refresh t))

(defun all-categories ()
  (select 'category :order-by [name]
          :flatp t :refresh t))

(defun add (&rest args)
  (let ((obj (apply #'make-instance args)))
    (update-records-from-instance obj)
    obj))

(defun get-by-id (class id)
  (aif (select class :where [= [id] id] :flatp t)
       (first it)
       (error 'db-object-not-found :class class :column 'id :value id)))

(define-condition empty-parameter ()
  ((name :initarg :name
         :initform nil)))

(defmacro ensure-non-empty-param (&rest params)
  `(progn
     ,@(mapcar #`(when (length=0 ,a1)
                   (error 'empty-parameter :name ',a1))
               params)))

(defun category-by-name (name)
  (ensure-non-empty-param name)
  (aif (select 'category :where [= [name] name] :flatp t)
       (first it)
       (add 'category :name name)))

(defun parse-positive-integer (string)
  (if (every #'digit-char-p string)
      (parse-integer string)))

(defun category-by-id-or-name (id-or-name)
  (aif (parse-positive-integer id-or-name)
       (get-by-id 'category it)
       (category-by-name id-or-name)))

(defun bookmark-by-url (url &optional (title ""))
  (ensure-non-empty-param url)
  (aif (select 'bookmark :where [= [url] url] :flatp t)
       (let ((bm (first it)))
         (values bm (string= (title bm) title)))
       (values (add 'bookmark :title title :url url) t)))

;; todo add-bookmark function with restart to handle existing bookmarks

(defun bookmark-by-id (id)
  (get-by-id 'bookmark id))

(defun save-changes (object)
  (update-records-from-instance object))

(defun delete-object (object)
  (let ((class (class-name-of object))
        (id (id object)))
    (delete-records :from class
                    :where [= [id] id])))

(defun delete-bookmark (bm)
  ;;(error "Please don't delete me :-(")
  (let ((id (id bm)))
    (delete-records :from 'bookmark-category
                    :where [= [bookmark-id] id])
    (delete-records :from 'bookmark
                    :where [= [id] id])))


;;; categories and bookmarks
(defun assign-bookmark-category (bookmark category)
  "return T if assignment was created, NIL if already present."
  (let ((bid (id bookmark)) (cid (id category)))
    (unless (select 'bookmark-category
                    :where [and [= [bookmark-id] bid] [= [category-id] cid]])
      (let ((x (make-instance 'bookmark-category
                              :bookmark-id bid
                              :category-id cid)))
        (save-changes x)
        (update-instance-from-records bookmark)
        (update-instance-from-records category))
      t)))

(defun assign-bookmark-categories (bookmark categories)
  (mapc (clambda (assign-bookmark-category bookmark x!category)) categories))


(defun unassign-bookmark-category (bookmark category)
  (let ((bid (id bookmark)) (cid (id category)))
    (prog1 (delete-records :from 'bookmark-category
                           :where [and [= [bookmark-id] bid] [= [category-id] cid]])
      (update-instance-from-records bookmark)
      (update-instance-from-records category))))

(defun unassign-bookmark-categories (bookmark categories)
  (mapc (clambda (unassign-bookmark-category bookmark x!category)) categories))
