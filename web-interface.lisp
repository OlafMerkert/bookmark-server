(in-package :cl-user)

(defpackage :bookmark-web-interface
  (:nicknames :bm-web)
  (:shadowing-import-from :parenscript :this :in)
  (:use :cl :ol :web-utils
        :hunchentoot :cl-who
        :parenscript)
  (:export))

(in-package :bookmark-web-interface)

(setf hunchentoot:*catch-errors-p* nil)

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
                         :style "/bookmarks/style.css"
                         :script "/jquery.min.js"
                         :script "/bookmarks/logic.js")
    (:h1 "Bookmarks")
    ;; todo Form for creating new bookmarks
    (:form :name "bookmarkNew"
           (:input :id "bookmarkId" :type "hidden" :value "")
           (:input :id "bookmarkTitle" :type "text" :value "")
           (:input :id "bookmarkUrl" :type "text" :value "")
           (:button :type "submit"
                    "New"))
    ;; todo List of present bookmarks
    (:ul
     (let (odd)
       (dolist (bm (bm:all-bookmarks))
         (htm (:li :id (bm:id bm)
                   :class (if (notf odd) "bookmark odd" "bookmark even")
                   (:a :href (bm:url bm) (esc (bm:title bm)))
                   (:br)
                   (:span :class "hidden" (esc (bm:url bm))))))))))

(defmacro/ps @@ (&rest args)
    `(chain ,@args))

(defmacro/ps $! (obj handler args &body body)
  "Install an event handler using jQuery on the solected object."
  `(@@ ($ ,obj) (,handler (lambda ,args ,@body))))

(define-easy-handler (bookmarks-js :uri "/bookmarks/logic.js") ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    ($! document ready ()
      ;; hiding/unhiding url of bookmark
      ($! ".bookmark" click ()
        (let ((verbose-url (@@ ($ this) (find "span"))))
          (if (@@ verbose-url (has-class "hidden"))
              (@@ verbose-url (remove-class "hidden"))
              (@@ verbose-url (add-class "hidden"))
              ))))))

(define-easy-handler (bookmarks-css :uri "/bookmarks/style.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    ((".odd") (:background "#f1f6fe"))
    ((".even") (:background "#f2f4f5"))
    ((".hidden") (:display "none"))))

