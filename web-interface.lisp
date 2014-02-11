(in-package :cl-user)

(defpackage :bookmark-web-interface
  (:nicknames :bm-web)
  (:shadowing-import-from :parenscript #:this #:in)
  (:use :cl :ol :web-utils
        :hunchentoot :cl-who
        :parenscript
        :ajax-actions)
  (:export
   #:start-bookmark-server))

(in-package :bookmark-web-interface)

(setf hunchentoot:*catch-errors-p* nil)

(eval-when (:load-toplevel :execute)
  (register-web-application "Bookmark Server" "/bookmarks"))

(defun start-bookmark-server ()
  (bm:connect-database :initialise t)
  (load-web-library :jquery)
  (start-server))

(ew (defpar bm-root '(bookmarks)))

(define-easy-handler (bookmarks-start :uri "/bookmarks") ()
  (html/document (:title #1="Bookmark Server")
    (:h1 #1#)
    (:p "View the " (:a :href (breadcrumb->url '(bookmarks list)) "list") " of bookmarks.")))

;;; first all the possible AJAX requests

(define-ajax-action+ (bookmark delete) ()
  (let ((id (selected-bookmark-id)))
    (ajax-call
     (:server (:id id)
              (bm:delete-bookmark (bm:bookmark-by-id id)))
     (:client () ()
              (user-message "Bookmark deleted")
              ;; todo update the dom
              )
     (bm:db-object-not-found (bm:value) ((id (mkstr bm:value)))
                             (user-message "Bookmark with id " id " was already deleted.")))))

(defun parse-categories (categories)
  (when (stringp categories)
    (mapcar #'bookmarks:category-by-id-or-name
            (split-sequence:split-sequence #\, categories))))

;; todo perhaps move this into bookmarks package?
(define-condition object-exists ()
  ((class :initarg :class
          :initform nil)
   (conflicting-slots :initarg :conflicting-slots
                      :initform nil)
   (object :initarg :object
           :initform nil)))

(define-ajax-action+ (bookmark new) ()
    (form-bind (bookmark-url
                bookmark-title)
      (@@ console (log "create bookmark"))
      ;; todo categories
      (ajax-call
       ;; here comes the server side lisp code
       (:server (:url bookmark-url :title bookmark-title)
                (mvbind (bm correct-title) (bm:bookmark-by-url url title)
                  (unless correct-title
                    (signal 'object-exists :class 'bm:bookmark
                            :object bm
                            :conflicting-slots (list 'bm:title)))
                  bm))
       ;; conditions to handle, starting with the condition name
       (object-exists (object)          ; slots of the  condition
                      ;; let for the data to transmit to the client
                      ((id (bm:id object))
                       (title (bm:title object)))
                      ;; here comes the js code
                      (user-message "Bookmark exists already, but with title " title))
       (bm:empty-parameter (bm:name) ((name (mkstr bm:name)))
                        (user-message "Please fill out " name))
       ;; finally, the code to run on success, with no
       ;; conditions occurring, again starting with let
       ;; containing data to transmit
       (:client (bm) ((id (bm:id bm)))
                (user-message "New Bookmark created")
                ;; todo add the new bookmark to the list
                ))))

(define-ajax-action+ (bookmark edit) ()
  (form-bind (bookmark-id
              bookmark-url
              bookmark-title)
    (ajax-call
     (:server (:id bookmark-id :url bookmark-url :title bookmark-title)
              (let ((bm (bm:bookmark-by-id id)))
                ;; todo make sure there are no empty strings here
                (setf (bm:title bm) title
                      (bm:url bm) url)
                (bm:save-changes bm)
                bm))
     (:client (bm) ((title (bm:title bm)))
              (user-message "Bookmark " title " successfully edited.")
              ;; todo update the contents of the UI
              )
     (bm:db-object-not-found
      (bm:value) ((id (mkstr bm:value)))
      (user-message "Cannot edit deleted bookmark with id " id ".")))))

(bind-multi ((assign assign unassign)
             (bm:assign-bookmark-categories bm:assign-bookmark-categories bm:unassign-bookmark-categories))
  (define-ajax-action+ (bookmark category assign) (categories)
                 (let ((id (selected-bookmark-id)))
                   (ajax-call
                    ;; todo currently need categories to be a string
                    (:server (:id id :categories categories)
                             (bm:assign-bookmark-categories
                              (bm:get-by-id 'bm:bookmark id) (parse-categories categories))
                             (values))
                    (:client () ()
                             (user-message "Updated categories for bookmark")
                             ;; todo trigger redisplay if in hierarchy view
                             )
                    (bm:db-object-not-found
                     (bm:value) ((id (mkstr bm:value)))
                     (user-message "Cannot alter categories for deleted bookmark with id " id))))))

;;; now the main UI

(defmacro cc (symbol-or-string)
  (cl-json:lisp-to-camel-case (mkstr symbol-or-string)))

(define-easy-handler (bookmarks-list :uri "/bookmarks/list") ()
  (html/document (:title "Bookmarks"
                         ;; todo use breadcrumbs?
                         :style "/bookmarks/style.css"
                         :script "/scripts/jquery-1.10.2.min.js"
                         :script "/bookmarks/ajax/actions.js"
                         :script "/bookmarks/logic.js"
                         )
    (:h1 "Bookmarks")
    ;; Form for creating new bookmarks
    (:form :id (cc bookmark-new)
           (:input :id (cc bookmark-id) :type "hidden" :value "")
           (:input :id (cc bookmark-title) :type "text" :value "")
           (:input :id (cc bookmark-url) :type "text" :value "")
           (:input :type "submit" :value "New"))
    ;; List of present bookmarks
    (:ul :id (cc bookmarks-list)
     (let (odd)
       (dolist (bm (bm:all-bookmarks))
         (htm (:li :id (bm:id bm)
                   :class (if (notf odd) "bookmark odd" "bookmark even")
                   (:a :href (bm:url bm) (esc (bm:title bm)))
                   (:br)
                   (:span :class "hidden" (esc (bm:url bm))))))))
    (:p "Use [p] and [n] keys to select any entry.")))

;;; todo move generally useful parenscript macros to some utility collection
(define-easy-handler (bookmarks-js :uri (breadcrumb->url (append1 bm-root "logic.js"))) ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
   (defun user-message% (message)
     (@@ console (log message))
      (alert message)
      nil)
    
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

    (defun selected-bookmark-id ()
      (@@ (get-bookmark-at-index *selected-bookmark-index*)
          (attr "id")))

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
        ;; todo don't do this if inside a form
        (case (@ event which)
          (80 (bookmark-select-prev))   ; p
          (78 (bookmark-select-next))   ; n
          ))
      ;; don't return anything, otherwise we block other important actions
      (values))))

(define-easy-handler (bookmarks-css :uri "/bookmarks/style.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    ((".odd") (:background "#f1f6fe"))
    ((".even") (:background "#f2f4f5"))
    ((".hidden") (:display "none"))
    ((".selected") (:background-color "yellow"))))
