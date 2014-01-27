(in-package :cl-user)

(defpackage :bookmark-web-interface
  (:nicknames :bm-web)
  (:shadowing-import-from :parenscript :this :in)
  (:use :cl :ol :web-utils
        :hunchentoot :cl-who
        :parenscript)
  (:export
   #:start-bookmark-server))

(in-package :bookmark-web-interface)

(setf hunchentoot:*catch-errors-p* nil)

(eval-when (:load-toplevel :execute)
  (register-web-application "Bookmark Server" "/bookmarks"))

(defun start-bookmark-server ()
  (bm:connect-database)
  (load-web-library :jquery)
  (start-server))

(ew (defpar bm-root '(bookmarks)))

(define-easy-handler (bookmarks-start :uri "/bookmarks") ()
  (html/document (:title #1="Bookmark Server")
    (:h1 #1#)
    (:p "View the " (:a :href (breadcrumb->url '(bookmarks list)) "list") " of bookmarks.")))

;;; first all the possible AJAX requests

(defmacro define-ajax-action (breadcrumb parameters &body body)
  `(define-easy-handler (,(apply #'symb 'ajax- (splice-in '- breadcrumb ))
                          :uri ,(breadcrumb->url (append bm-root breadcrumb)))
       ,parameters
     (format nil "~S" (progn ,@body))))
;; todo integrate error handling
;; todo can we unify error handling between js and cl?

(define-ajax-action (bookmark delete) (id)
  (handler-case
      (prog1 'success
        (bm:delete-bookmark (bm:bookmark-by-id id)))
    (bm:db-object-not-found () 'already-deleted)))

(defun parse-categories (categories)
  (when (stringp categories)
    (mapcar #'bookmarks:category-by-id-or-name
            (split-sequence:split-sequence #\, categories))))

(define-ajax-action (bookmark new) (title url categories)
  ;; todo check for duplicates
  (mvbind (bm correct-title) (bm:bookmark-by-url url title)
    (if correct-title
        (prog1 'success
          (when categories
            (bookmarks:assign-bookmark-categories
             bm (parse-categories categories))))
        'already-exists)))

(define-ajax-action (bookmark edit) (id title url)
  (handler-case
      (let ((bm (bm:bookmark-by-id id)))
        (setf (bm:title bm) title
              (bm:url bm) url)
        (bm:save-changes bm)
        'success)
    (bm:db-object-not-found () 'does-not-exist)))

(define-ajax-action (bookmark category assign) (id categories)
  ;; todo error handling
  (bm:assign-bookmark-categories
   (bm:get-by-id 'bm:bookmark id) (parse-categories categories)))

(define-ajax-action (bookmark category unassign) (id categories)
  ;; todo error handling
  (bm:unassign-bookmark-categories
   (bm:bookmark-by-id id) (parse-categories categories)))

;;; now the main UI

(define-easy-handler (bookmarks-list :uri "/bookmarks/list") ()
  (html/document (:title "Bookmarks"
                         :style "/bookmarks/style.css"
                         :script "/scripts/jquery-1.10.2.min.js"
                         :script "/bookmarks/logic.js")
    (:h1 "Bookmarks")
    ;; Form for creating new bookmarks
    (:form :id "bookmarkNew"
           (:input :id "bookmarkId" :type "hidden" :value "")
           (:input :id "bookmarkTitle" :type "text" :value "")
           (:input :id "bookmarkUrl" :type "text" :value "")
           (:input :type "submit" :value "New"))
    ;; List of present bookmarks
    (:ul
     (let (odd)
       (dolist (bm (bm:all-bookmarks))
         (htm (:li :id (bm:id bm)
                   :class (if (notf odd) "bookmark odd" "bookmark even")
                   (:a :href (bm:url bm) (esc (bm:title bm)))
                   (:br)
                   (:span :class "hidden" (esc (bm:url bm))))))))
    (:p "Use [p] and [n] keys to select any entry.")))

;;; todo move generally useful parenscript macros to some utility collection
(ew
(defmacro/ps @@ (&rest args)
    `(chain ,@args))

(defmacro/ps $! (obj handler args &body body)
  "Install an event handler using jQuery on the solected object."
  `(@@ ($ ,obj) (,handler (lambda ,args ,@body)))))

(define-easy-handler (bookmarks-js :uri "/bookmarks/logic.js") ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
      (defun bookmark-new ()
        (let ((bm-url (@@ ($ "#bookmarkUrl") (val)))
              (bm-title (@@ ($ "#bookmarkTitle") (val))))
          ;; todo abort on empty url (title can be autofilled)
          (@@ $ (ajax
                 (create url "/bookmarks/bookmark/new"
                         data (create url bm-url 
                                      title bm-title)
                         type "GET" 
                         ;; todo better feedback on error and success
                         success (lambda ()
                                   (alert "New Bookmark created"))
                         error (lambda ()
                                 (alert "Failure creating bookmark")))
                 ;; todo insert bookmark into current display (if successfull)
                 ))))

      
    ;; selecting bookmarks
    (defvar *selected-bookmark-index* -1)

    (defun valid-bookmark-index-p (index)
      (and (<= 0 index)
           (< index (@ ($ ".bookmark") length))))
      
    (defun get-bookmark-at-index (index)
      (let ((bms ($ ".bookmark")))
        (if (valid-bookmark-index-p index)
            (@@ bms (eq index))
            (@@ bms (eq 0)))))

    (defun bookmark-select (index)
      (unless (= *selected-bookmark-index* index)
        ;; first remove the mark from previous selection
        (@@ (get-bookmark-at-index *selected-bookmark-index*)
            (remove-class "selected"))
        ;; then add the mark to the next
        (@@ (get-bookmark-at-index index)
            (add-class "selected"))
        (setf *selected-bookmark-index* index)))
    ;; fix highlighting

    (defun bookmark-select-next ()
      (let ((index  (+ *selected-bookmark-index* 1)))
        (if (valid-bookmark-index-p index)
            (bookmark-select index))))

    (defun bookmark-select-prev ()
      (let ((index  (- *selected-bookmark-index* 1)))
        (if (valid-bookmark-index-p index)
            (bookmark-select index))))

    ($! document ready ()
      ;; hiding/unhiding url of bookmark
      ($! ".bookmark" click ()
        (let ((verbose-url (@@ ($ this) (find "span"))))
          (if (@@ verbose-url (has-class "hidden"))
              (@@ verbose-url (remove-class "hidden"))
              (@@ verbose-url (add-class "hidden")))))

      ;; creating new bookmark
      ($! "#bookmarkNew" submit (event)
        (@@ event (prevent-default))
        (bookmark-new))

      ;; setup keyboard bindings
      ($! "body" keydown (event)
        (@@ event (prevent-default))
        (case (@ event which)
          (80 (bookmark-select-prev)) ; p
          (78 (bookmark-select-next)) ; n
          ))
      nil
      )))

(define-easy-handler (bookmarks-css :uri "/bookmarks/style.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    ((".odd") (:background "#f1f6fe"))
    ((".even") (:background "#f2f4f5"))
    ((".hidden") (:display "none"))
    ((".selected") (:background-color "yellow"))))

