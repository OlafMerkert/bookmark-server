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
              (bm:delete-bookmark (bm:bookmark-by-id id))
              id)
     (:client (id) ((id (mkstr "#" id)))
              (user-message "Bookmark deleted")
              (@@ console (log id))
              ;; update the dom
              (hide+remove id))
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
    ;; todo categories
    (if (length=0 bookmark-url)
        (user-message "Cannot create bookmark with empty url.")
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
         (object-exists (object)        ; slots of the  condition
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
         (:client (bm) ((id (mkstr (bm:id bm))) (title (bm:title bm)) (url (bm:url bm)))
                  ;; clear the form
                  (form-value bookmark-title "")
                  (form-value bookmark-url "")
                  (user-message "New Bookmark created")
                  ;; add the new bookmark to the list
                  (let ((bm-html ($ (who-ps-html
                                     (:li :id id
                                          :class "bookmark" ; todo add odd or even accordingly
                                          (:a :href url :target "_blank" title)
                                          (:br)
                                          (:span :class "hidden" url))))))
                    (@@ bm-html (hide))
                    (@@ bm-html (append-to (cch bookmarks-list)))
                    (@@ bm-html (show "normal"))))))))

;; two actions for editing, one for loading stuff into the form
(define-ajax-action+ (bookmark edit-selected) ()
  (let ((id (selected-bookmark-id)))
    (ajax-call
     (:server (:id id)
              (bm:bookmark-by-id id))
     (:client (bm) ((id (bm:id bm)) (title (bm:title bm))(url (bm:url bm)))
              (form-value bookmark-id id)
              (form-value bookmark-title title)
              (form-value bookmark-url url)
              (form-value bookmark-new-submit "Save changes")))))



;; todo allow aborting editing of a bookmark
(define-ajax-action+ (bookmark edit) ()
  (form-bind (bookmark-id
              bookmark-url
              bookmark-title)
    (let ((lt (length=0 bookmark-title))
          (lu (length=0 bookmark-url)))
      (if (or lt lu)
          (progn
            (when lt (user-message "Cannot clear title of bookmark"))
            (when lu (user-message "Cannot clear empty bookmark url.")))
          (ajax-call
           (:server (:id bookmark-id :url bookmark-url :title bookmark-title)
                    (let ((bm (bm:bookmark-by-id id)))
                      ;; todo make sure there are no empty strings here
                      (setf (bm:title bm) title
                            (bm:url bm) url)
                      (bm:save-changes bm)
                      bm))
           (:client (bm) ((id (mkstr "#" (bm:id bm))) (title (bm:title bm)) (url (bm:url bm)))
                    ;; update the contents of the UI
                    (let ((li ($ id)))
                      (@@ li (children "a") (eq 0)
                             (attr "href" url)
                             (text title))
                      (@@ li (children "span") (eq 0) (text url)))
                    (user-message "Bookmark " title " successfully edited.")
                    ;; reset form field
                    (form-value bookmark-id "")
                    (form-value bookmark-title "")
                    (form-value bookmark-url "")
                    (form-value bookmark-new-submit "New"))
           (bm:db-object-not-found
            (bm:value) ((id (mkstr bm:value)))
            (user-message "Cannot edit deleted bookmark with id " id ".")))))))

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



(define-easy-handler (bookmarks-list :uri "/bookmarks/list") ()
  (html/document (:title "Bookmarks"
                         ;; todo use breadcrumbs?
                         :style "/bookmarks/style.css"
                         :script "/scripts/jquery-1.10.2.min.js"
                         :script "/scripts/utils.js"
                         :script "/bookmarks/ajax/actions.js"
                         :script "/bookmarks/logic.js"
                         )
    (:h1 "Bookmarks")
    (:a :href (ps-inline (user-message "Hallo du!")) "Give me a message")
    (:div :id (cc message-container))
    ;; Form for creating new bookmarks
    (:form :id (cc bookmark-new)
           (:input :id (cc bookmark-id) :type "hidden" :value "")
           (:input :id (cc bookmark-title) :type "text" :value "")
           (:input :id (cc bookmark-url) :type "text" :value "")
           (:input :id (cc bookmark-new-submit) :type "submit" :value "New"))
    ;; Form for editing and so on
    (:form :id (cc bookmark-operations)
           (:button :type "button" :onclick (ps-inline (bookmark-select-prev)) "Previous")
           (:button :type "button" :onclick (ps-inline (bookmark-select-next)) "Next")
           (:button :type "button" :onclick (ps-inline (bookmark-edit-selected)) "Edit")
           (:button :type "button" :onclick (ps-inline (bookmark-delete)) "Delete"))
    ;; List of present bookmarks
    (:ul :id (cc bookmarks-list)
     (let (odd)
       (dolist (bm (bm:all-bookmarks))
         (htm (:li :id (bm:id bm)
                   :class (if (notf odd) "bookmark odd" "bookmark even")
                   (:a :href (bm:url bm) :target "_blank" (esc (bm:title bm)))
                   (:br)
                   (:span :class "hidden" (esc (bm:url bm))))))))
    (:p "Use [Alt-p] and [Alt-n] keys to select any entry.")))

;;; todo move generally useful parenscript macros to some utility collection
(define-easy-handler (bookmarks-js :uri (breadcrumb->url (append1 bm-root "logic.js"))) ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    (defun user-message% (message)
      (@@ console (log message))
      (add-message message)
      nil)
    ;; todo fade out messages automatically after some time, or after
    ;; new messages come in

    ;; todo new message should appear at the top
    ;; todo move the message container to the side (should not disturb
    ;; general layout)

    ;; todo find out whether this sort of user message stuff is provided by jquery already

    (defun add-message (message-html)
      (let ((div ($ "<div/>"
                    (create html message-html
                            "class" "message")))
            (dismiss-link ($ "<a/>"
                             (create html "Dismiss"
                                     href "#"))))
        (@@ dismiss-link (append-to div))
        (@@ div (hide))
        (@@ div (append-to (cch message-container)))
        ($! dismiss-link click (event)
          (@@ event (prevent-default))
          ;; fade out
          (hide+remove (@@ ($ this) (parent))))
        ;; fade in
        (@@ div (show "normal"))))
    
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

      ;; creating new bookmark, respectively edit
      ($! "#bookmarkNew" submit (event)
        (@@ event (prevent-default))
        (if (= 0 (length (form-value bookmark-id)))
            (bookmark-new)
            (bookmark-edit)))

      ;; setup keyboard bindings
      #|(bind-keys "body"
                 ;; todo figure out how to require ALT + key ; ; ; ;
                 ;; todo don't do this if inside a form ; ; ; ;
      (p (bookmark-select-prev))
      (n (bookmark-select-next))
      (d (bookmark-delete))
      (e (bookmark-edit-selected)))|#
      
      ;; don't return anything, otherwise we block other important actions
      (values))))

(define-easy-handler (bookmarks-css :uri "/bookmarks/style.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    ((".odd") (:background "#f1f6fe"))
    ((".even") (:background "#f2f4f5"))
    ((".hidden") (:display "none"))
    ((".selected") (:background-color "yellow"))
    (("#messageContainer")
     (:position "absolute"
                :top "20px"
                :right "20px"
                :width "30%"
                :background "khaki"))
    ((".message") ( ;;:background-color "lightgray"
                   :border "3px double orangered"
                   :padding "2px"
                   :margin "2px"
                   :vertical-align "middle"))
    ((".message" "a") (:margin-left "5em" :font-size "70%"))
    ))
