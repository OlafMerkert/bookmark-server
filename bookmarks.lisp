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
   #:unassign-bookmark-category))

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

;;; often used selects

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
  (aif (select class :where [= [id] id]
               :flatp t)
       (first it)
       ;; todo error message
       nil))

(defun save-changes (object)
  (update-records-from-instance object))

(defun delete-object (object)
  (let ((class (class-name-of object))
        (id (id object)))
    (delete-records :from class
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

(defun unassign-bookmark-category (bookmark category)
  (let ((bid (id bookmark)) (cid (id category)))
    (prog1 (delete-records :from 'bookmark-category
                           :where [and [= [bookmark-id] bid] [= [category-id] cid]])
      (update-instance-from-records bookmark)
      (update-instance-from-records category))))
