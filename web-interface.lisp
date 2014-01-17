(defpackage :bookmark-web-interface
  (:nicknames :bm-web)
  (:use :cl :ol :iterate :web-utils
        :hunchentoot :cl-who)
  (:export))

(in-package :bookmark-web-interface)

;; (setq *dispatch-table*
;;       (list
;;        'dispatch-easy-handlers))

(eval-when (:load-toplevel :execute)
(register-web-application "Bookmark Server" "/bookmarks"))

(ew
(defun destruct-input (input)
   (let ((label-pos (position :label input)))
     (values (getf (cdr input) :name)
             (getf (cdr input) :label)
             (append (subseq input 0 label-pos)
                     (subseq input (+ label-pos 2)))))))

(defmacro table-form ((&key title (submit-text title)) &body inputs)
  `(htm
    (:fieldset
     (:legend (esc ,title))
     ;; todo make it fit the form size
     (:table
      ,@(mapcar (lambda (input)
                  (mvbind (name label cleaned-input)
                      (destruct-input input)
                    `(:tr
                      (:td (:label :for ,name (esc ,label)))
                      (:td ,cleaned-input))))
                inputs)
      (:tr (:td)
           (:td (:input :type "submit" :value ,submit-text)))))))

(defun top-level-nav ()
  (html/node
    (:p
     (:a :href "/bookmarks" "All Bookmarks")
     " - "
     (:a :href "/categories" "All Categories"))))

(defparameter return-to "bookmark")

(defun bm-list-item (bookmark &optional category)
  (html/node (:tr (:td (:a :target "_blank" :href (bm:url bookmark)
                           (esc (bm:title bookmark))))
                  (:td (:a  :href (uri "/bookmark" :id (bm:id bookmark))
                            (icon "edit")))
                  ;; different behaviour when in the category view
                  (:td (:a :href (uri "/delete-bookmark" :confirm "true" :id (bm:id bookmark))
                           (icon "delete")))
                  (if category
                      (htm
                       (:td (:a :href (uri "/unassign-category"
                                           :category-id (bm:id category)
                                           :bookmark-id (bm:id bookmark)
                                           :return-to return-to)
                                (icon "remove" "remove from category"))))))))

(define-easy-handler (bookmarks :uri "/bookmarks") ()
  (html/document (:title "All Bookmarks")
    (top-level-nav)
    (:h1 "All Bookmarks")
    (:form :method :post :action "/new-bookmark"
           (table-form (:title "Create new bookmark" :submit-text "Create bookmark")
             (:input :type "text" :name "title" :size 50 :label "Title:")
             (:input :type "text" :name "url" :size 50 :label "URL:" :value "http://"))
           ;; todo categories
           )
    (:table (mapc #'bm-list-item (bm:all-bookmarks)))))

(define-easy-handler (new-bookmark :uri "/new-bookmark") (title url)
  ;; todo input sanitising
  (goto (bm:add 'bm:bookmark :title title :url url))
  #|(redirect "/bookmarks")|#)

(define-easy-handler (show-bookmark :uri "/bookmark") (id)
  (let ((bookmark (bm:get-by-id 'bm:bookmark (parse-integer/safe id))))
    (html/document (:title (bm:title bookmark))
      (top-level-nav)
      (:h1 "Bookmark: " (esc (bm:title bookmark)))
      (:p (:a :target "_blank" :href (bm:url bookmark) (esc (bm:url bookmark))))
      (:form :method :post :action "/edit-bookmark"
             (:input :type "hidden" :name "id" :value (str (bm:id bookmark)))
             (table-form (:title "Edit bookmark" :submit-text "Save changes")
               (:input :type "text" :name "title" :size 50 :label "Title:"
                       :value (esc (bm:title bookmark)))
               (:input :type "text" :name "url" :size 50 :label "URL:"
                       :value (esc (bm:url bookmark))))
             (:ul
              (dolist (category (bm:categories bookmark))
                (htm (:li (:a :href (own-url category)
                              (esc (bm:name category)))
                          "&nbsp;"
                          (:a :href (uri "/unassign-category"
                                         :bookmark-id (bm:id bookmark)
                                         :category-id (bm:id category)
                                         :return-to "bookmark")
                              (icon "remove"))))))
             ;; todo get a list of only those categories which are not
             ;; yet assigned to this bookmark
             (:ul
              (dolist (category (bm:all-categories))
                (htm (:li (:a :href (own-url category)
                              (esc (bm:name category)))
                          "&nbsp;"
                          (:a :href (uri "/assign-category"
                                         :bookmark-id (bm:id bookmark)
                                         :category-id (bm:id category)
                                         :return-to "bookmark")
                              (icon "add"))))))
             ;; todo create categories on the fly
             )
      (:p (:a :href (uri "/delete-bookmark" :confirm "true" :id (bm:id bookmark))
              (icon "delete") "Delete this bookmark")))))

(define-easy-handler (edit-bookmark :uri "/edit-bookmark") (id title url)
  (let ((bookmark (bm:get-by-id 'bm:bookmark (parse-integer/safe id))))
    ;; todo sanitise data
    (setf (bm:title bookmark) title
          (bm:url bookmark) url)
    (bm:save-changes bookmark)
    (goto bookmark)))

(defmethod own-url ((bookmark bm:bookmark))
  (uri "/bookmark" :id (bm:id bookmark)))

(defmethod own-url ((bookmark (eql 'bm:bookmark)))
  "/bookmarks")

(defmacro define-delete-handler ((class description title-function) &body body)
  (let* ((function-name (symb 'delete- class))
         (url (conc "/delete-" (string-downcase description))))
    `(define-easy-handler (,function-name :uri ,url) (id confirm)
       (let ((object (bm:get-by-id ',class (parse-integer/safe id))))
         (if (string-equal confirm "confirmed")
             (progn
               (bm:delete-object object)
               (goto ',class))
             ;; otherwise produce confirmation
             (html/document (:title (,title-function object))
               (top-level-nav)
               (:h1 ,(format nil "Delete ~A: " description)
                    (esc (,title-function object)))
               ,@body
               (:p (:a :href (uri ,url :confirm "confirmed" :id (bm:id object)) 
                       "Yes")
                   "&nbsp;"
                   (:a :href (own-url object)
                       "No"))))))))

(define-delete-handler (bm:bookmark "bookmark" bm:title)
  (:p "Really delete this bookmark pointing to "
      (esc (bm:url object))
      " ?"))


(defun goto (object)
  (redirect (own-url object)))

;;; ----------------------------------------------------------------------
(define-easy-handler (categories :uri "/categories") ()
  (html/document (:title "All Categories")
    (top-level-nav)
    (:h1 "All Categories")
    (:form :method :post :action "/new-category"
           (table-form (:title "Create new category" :submit-text "Create category")
             (:input :type "text" :name "name" :size 50 :label "Name:")))
    (:table
     (dolist (category (bm:all-categories))
       (htm (:tr (:td (:a :href (uri "/category" :id (bm:id category))
                          (esc (bm:name category))))
                 (:td (:a :href (uri "/delete-category" :confirm "true"
                                     :id (bm:id category))
                          (icon "delete")))))))))

(define-easy-handler (new-category :uri "/new-category") (name)
  ;; todo input sanitising
  (goto (bm:add 'bm:category :name name))
  #|(redirect "/categories")|#)

(define-easy-handler (show-category :uri "/category") (id)
  (let ((category (bm:get-by-id 'bm:category (parse-integer/safe id))))
    (html/document (:title (bm:name category))
      (top-level-nav)
      (:h1 "Category: " (esc (bm:name category)))
      (:form :method :post :action "/edit-category"
             (:input :type "hidden" :name "id" :value (mkstr (bm:id category)))
             (table-form (:title "Edit category" :submit-text "Save changes")
               (:input :type "text" :name "name" :size 50 :label "Name:"
                       :value (esc (bm:name category)))))
      (:p (:a :href (uri "/delete-category" :confirm "true" :id (bm:id category))
              (icon "delete") "delete this category"))
      (:table (let ((return-to "category"))
                (mapc (clambda bm-list-item x! category) (bm:bookmarks category)))))))

(define-easy-handler (edit-category :uri "/edit-category") (id name)
  (let ((category (bm:get-by-id 'bm:category (parse-integer/safe id))))
    ;; todo sanitise data
    (setf (bm:name category) name)
    (bm:save-changes category)
    (goto category)))

(define-delete-handler (bm:category "category" bm:name)
  (:p "Really delete this category"))

(defmethod own-url ((category bm:category))
  (uri "/category" :id (bm:id category)))

(defmethod own-url ((category (eql 'bm:category)))
  "/categories")

;;; association between category and bookmark
(define-easy-handler (assign-category :uri "/assign-category")
    (bookmark-id category-id return-to)
  (let ((bookmark (bm:get-by-id 'bm:bookmark (parse-integer/safe bookmark-id)))
        (category (bm:get-by-id 'bm:category (parse-integer/safe category-id))))
    (bm:assign-bookmark-category bookmark category)
    (if (string-equal return-to "category")
        (goto category)
        (goto bookmark))))

(define-easy-handler (unassign-category :uri "/unassign-category")
    (bookmark-id category-id return-to)
  (let ((bookmark (bm:get-by-id 'bm:bookmark (parse-integer/safe bookmark-id)))
        (category (bm:get-by-id 'bm:category (parse-integer/safe category-id))))
    (bm:unassign-bookmark-category bookmark category)
    (if (string-equal return-to "category")
        (goto category)
        (goto bookmark))))

(defun parse-integer/safe (string)
  ;; todo implement safety
  (parse-integer string))

;;; icon serving
(defparameter icon-path #P"/usr/share/icons/gnome/22x22/actions/")

(defun icon-url (name &optional (suffix ".png"))
  (conc "/bm-icons/" (string-downcase (mkstr name)) suffix))

(defun icon (name &optional (description name))
  (html/node (:img :src (icon-url name) :alt description :title description)))

;; setup some stock icons
(dolist (icon '(("edit" "stock_properties.png")
                ("delete" "stock_delete.png")
                ("add" "list-add.png")
                ("remove" "list-remove.png")))
  (setup-static-content (icon-url (first icon))
                        (merge-pathnames (second icon) icon-path)))
