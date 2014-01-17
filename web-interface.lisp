(defpackage :bookmark-web-interface
  (:nicknames :bm-web)
  (:use :cl :ol :iterate :web-utils
        :hunchentoot :cl-who)
  (:export))

(in-package :bookmark-web-interface)


(eval-when (:load-toplevel :execute)
  (register-web-application "Bookmark Server" "/bookmarks"))

(defpar bm-root '(bookmarks))

;;; first all the possible AJAX requests
(define-easy-handler (bookmark-delete :uri "/bookmarks/bookmark/delete")
    (id)
  ;; todo delete category assignments?
  (bookmarks:delete-object
   (bookmarks:get-by-id 'bookmark id)))

(defun parse-categories (categories)
  (assert (stringp categories))
  (mapcar #'bookmarks:category-by-id-or-name
          (split-sequence:split-sequence #\, categories)))

(define-easy-handler (bookmark-new :uri "/bookmarks/bookmark/new")
    (title url categories)
  (let ((bm (bookmarks:add 'bookmark :title title :url url)))
    (bookmarks:assign-bookmark-categories bm
                                          (parse-categories categories))))

(define-easy-handler (bookmark-edit :uri "/bookmarks/bookmark/edit")
    (id title url)
  (let ((bm (bm:get-by-id 'bookmark id)))
    (setf (bm:title bm) title
          (bm:url bm) url)
    (bm:save-changes bm)))

(define-easy-handler (bookmark-assign-category :uri "/bookmarks/bookmark/category/assign")
    (id categories)
  (bm:assign-bookmark-categories (bm:get-by-id 'bookmark id)
                                 (parse-categories categories)))

(define-easy-handler (bookmark-unassign-category :uri "/bookmarks/bookmark/category/unassign")
    (id categories)
  (bm:unassign-bookmark-categories (bm:get-by-id 'bookmark id)
                                   (parse-categories categories)))

;;; now the main UI

(define-easy-handler (bookmarks-list :uri "/bookmarks/list") ()
  (html/document (:title "Bookmarks"
                         :style "/bookmarks/style.css")
    (:h1 "Bookmarks")
    ;; todo Form for creating new bookmarks
    (:form :name "bookmark-new" :onsubmit "false"
           (:input :id "bookmark-id" :type "hidden" :value "")
           (:input :id "bookmark-title" :type "text" :value "")
           (:input :id "bookmark-url" :type "text" :value "")
           (:button :type "submit"
                    "New"))
    ;; todo List of present bookmarks
    (:ul
     (let (odd)
       (dolist (bm (bm:all-bookmarks))
         (htm (:li :id (bm:id bm)
                   :class (if (notf odd) "odd" "even")
                   (:a :href (bm:url bm) (esc (bm:title bm)))
                   (:br)
                   (:span :class "hidden" (esc (bm:url bm))))))))))
