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
  (load-web-library :jquery)
  (load-web-library :jquery-sticky)
  (start-server))

(ew (defpar bm-root '(bookmarks)))

(define-ajax-action+ (bookmark categories add) (bm categories)
  ;; `bm' should be a DOM node, `categories' a list of strings
  ;; first check if the categories is already added
  (cond ((length=0 categories)
         ;; nothing supplied, just get rid of the input 
         (remove-category-form bm))
        (t (ajax-call
            (:server (:url (bookmark-url bm) :categories (@@ categories (join ",")))
                     (bm:add-categories url (split-sequence:split-sequence #\, categories)))
            (:client () ()
                     (add-categories-ui bm categories)
                     (remove-category-form bm))
            ;; exception handling
            (bm:bookmark-not-exists
             () ()
             (user-message "Bookmark was deleted."))
            ))))

(define-ajax-action+ (bookmark category remove) (bm category)
  ;; `bm' and `category' should be DOM nodes
  (let ((cat (@@ ($ category) (text))))
    (if (not (member cat (bookmark-categories bm)))
        (user-message #1="Category not assigned")
        (ajax-call
         (:server (:url (bookmark-url bm) :category cat)
                  (bm:remove-category url category))
         (:client () ()
                  (@@ ($ category) (remove)))
         ;; exception handling
         (bm:bookmark-not-exists
          () ()
          (user-message "Bookmark was deleted."))
         (bm:user-category-not-present
          () ()
          (user-message #1#))
         (bm:cannot-remove-auto-category
          () ()
          (user-message "Cannot remove automatically assigned category.")))))
  )

(define-easy-handler (bookmarks-start :uri "/bookmarks") ()
  (html/document (:title #1="Bookmark Server")
    (:h1 #1#)
    (:p "View the " (:a :href (breadcrumb->url '(bookmarks list)) "list") " of bookmarks.")))

(setup-static-content "/scripts/jquery-ui-1.10.4.custom.min.css"
                      #P"/home/olaf/Projekte/bookmark-server/jquery-ui-1.10.4.custom.min.css"
                      "/scripts/jquery-ui-1.10.4.custom.min.js"
                      #P"/home/olaf/Projekte/bookmark-server/jquery-ui-1.10.4.custom.min.js")

(defmacro bookmark/document ((&key title) &body body)
  `(html/document (:title ,title
                          ;; todo use breadcrumbs?
                          :style "/bookmarks/style.css"
                          :script "/scripts/jquery-1.10.2.min.js"
                          :script "/scripts/sticky/sticky.js"
                          :style "/scripts/sticky/sticky.css"
                          :script "/scripts/jquery-ui-1.10.4.custom.min.js"
                          :style "/scripts/jquery-ui-1.10.4.custom.min.css"
                          :script "/scripts/utils.js"
                          :script "/bookmarks/ajax/actions.js"
                          :script "/bookmarks/logic.js"
                          )
     (:h1 ,title)
     ,@body))


(define-easy-handler (bookmarks-list :uri "/bookmarks/list") (category)
  (let ((category (bm:category-p category)))
    (bookmark/document (:title "All bookmarks as list")
      ;; todo Form for creating new bookmarks
      ;; List of present bookmarks
      (:p (:a :href "/bookmarks/tree" "Tree View"))
      (category-filters (bm:all-known-categories) category)
      (:div :id (cc bookmarks-list)
            (mapc #'single-bookmark (aif category
                                         (bm:bookmarks-in-category it)
                                         (bm:all-bookmarks))))
      )))

(define-easy-handler (bookmarks-tree :uri "/bookmarks/tree") (category)
  (let ((category (bm:category-p category)))
    (bookmark/document (:title "All bookmarks as tree")
      ;; todo Form for creating new bookmarks
      ;; List of present bookmarks
      (:p (:a :href "/bookmarks/list" "List View"))
      (category-filters (bm:all-known-categories) category)
      (:div :id (cc bookmarks-tree)
            (labels ((render-bm-tree (tree)
                       (cond ((null tree))
                             ((atom tree)
                              (single-bookmark tree))
                             ((symbolp (car tree))
                              (htm (:fieldset :class "category"
                                              (:legend :class "categories"
                                                       (str (car tree)))
                                              (render-bm-tree (cdr tree)))))
                             (t (render-bm-tree (car tree))
                                (render-bm-tree (cdr tree))))))
              (render-bm-tree (aif category
                                   (bm-tree:build-tree 
                                    (bm:bookmarks-in-category it) (list it))
                                   (bm-tree:build-tree (bm:all-bookmarks))))))
      )))

(defun category-filters (categories &optional active)
  (html/node
    (:div :class "categories filters"
          (dolist (c categories)
            (htm (:a :href (if (eq c active) "?"
                               (conc "?category=" (symbol-name c)))
                     :class (if (eq c active) "selected-category" "")
                     (str c))
                 (str " ")))))
  )


(defun single-bookmark (bm)
  (html/node (:div :class "bookmark"
                   (:a :class "bookmark-link" :target "_blank" :href (bm:url bm)
                       (esc (bm:title bm)))
                   (str " ")
                   (:span :class "categories"
                          (dolist (c (bm:categories bm))
                            (htm (:a :class "category" :href "#"
                                     (str c))
                                 (str " "))) )
                   (str "&nbsp;")
                   (:button :class "add-tag" "+"))))

(define-easy-handler (bookmarks-js :uri (breadcrumb->url (append1 bm-root "logic.js"))) ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    ;; autocompletion code
    (defun split (val)
      (@@ val (split (regex ",\\s*"))))
    (defun extract-last (term)
      (@@ (split term) (pop)))

    (defun provide-autocomplete (input completions)
      (@@ input
          ;; disable TAB key
          (bind "keydown"
                (lambda (e)
                  (if (and (= (@ e key-code) (@ $ ui key-code |TAB|))
                           (@@ ($ this) (data "ui-autocomplete") menu active))
                      (@@ e (prevent-default)))))
          (autocomplete
           (create min-length 0
                   source (lambda (request response)
                            (response (@@ $ ui autocomplete (filter completions
                                                                    (extract-last (@ request term))))))
                   focus (lambda () f)
                   select (lambda (event ui)
                            (let ((terms (split (@ this value))))
                              (@@ terms (pop))
                              (@@ terms (push (@ ui item value)))
                              (@@ terms (push ""))
                              (setf (@ this value) (@@ terms (join ", ")))
                              f))))))



    ;; bookmark specific code
    (defun current-bookmark (node)
      (let* ((node ($ node))
             (parents (@@ node (parents-until ".bookmark"))))
        (if (< 0 (@ parents.length))
            (@@ parents (last) (parent))
            (@@ node (parent)))))

    (defun bookmark-title (bm)
      (@@ bm (children "a.bookmark-link") (text)))

    (defun bookmark-url (bm)
      (@@ bm (children "a.bookmark-link") (attr "href")))

    (defun bookmark-categories-dom (bm)
      (@@ bm (children ".categories") (children)))

    (defun bookmark-categories (bm)
      (@@ $ (map (bookmark-categories-dom bm)
                 get-text)))

    (defun category-click ()
      (@@ event (prevent-default))
      (let* ((category ($ this))
             (cat (@@ category (text))))
        (if (confirm (concatenate 'string "Remove category " cat "?"))
            (bookmark-category-remove (current-bookmark category) category))))

    (defun add-category-ui (bm category)
      (let ((cat-el ($ (who-ps-html (:a :class "category" :href "#" category)))))
        (@@ cat-el (click category-click))
        (@@ bm (children ".categories") (append " ") (append cat-el))
        ;; todo add to filter list??
        ))

    (defun add-categories-ui (bm categories)
      (dolist (cat categories)
        (add-category-ui bm cat)))

    (defun all-categories ()
      (@@ $ (map ($ ".filters a")
                 get-text)))

    (defun category-form (bm)
      (@@ bm (find "input.category-input")))

    (defun has-category-form (bm)
      (< (length (category-form bm))))

    (defun remove-category-form (bm)
      (hide+remove (category-form bm) ))

    (defun new-category-form (button bm)
      (let ((input ($ (who-ps-html (:input :type "text" :size "30" :class "category-input")))))
        ;; todo enable autocompletion
        (provide-autocomplete input (all-categories))
        ;; hide and attach
        (@@ input (hide)
                  (insert-after button)
                  (show "normal"))
        ;; allow using ENTER to complete
        (@@ input (bind "keydown"
                        (lambda (e)
                          (when (= (@ e key-code) (@ $ ui key-code |ENTER|))
                            (@@ e (prevent-default))
                            (user-message "enter") ; todo get this to work
                            (process-category-form (current-bookmark this))))))
        ;; move the focus to the field
        (@@ input (focus))
        ))

    (defun process-category-form (bm)
      (let* ((input (category-form bm))
             (categories (remove-empty (split (@@ input (val))))))
        (bookmark-categories-add bm categories)))


    (bind-event document ready ()
      (bind-event "button.add-tag" click ()
        (let ((bm (current-bookmark this)))
          (if (has-category-form bm)
              (process-category-form bm)
              (new-category-form ($ this) bm))))

      (@@ ($ ".bookmark a.category") (click category-click))

      (values))))

(define-easy-handler (bookmarks-css :uri "/bookmarks/style.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    ((".hidden") (
                  :visibility "hidden"
                  :margin-right "1em"
                  :display "block"
                  :float "right"
                  :text-align "right"
                  :color "gray"))
    ((".selected") (:background-color "yellow"))
    ((".selected-category") (:background-color "lightgreen"))
    (("#messageContainer") (
                            :position "absolute"
                            :top "20px"
                            :right "20px"
                            :width "30%"
                            :background "khaki"))
    ((".filters") (
                   :margin "1ex"
                   :padding "3pt"
                   :border "solid gray 1px"))
    (("span.categories") (:margin-left "2ex"))
    (("span.categories") (
                          :font-size "80%")
     ;; todo macro for generating link styling
     (("a:link") (
                  :color "orange"
                  :text-decoration "none"))
     (("a:visited") (
                     :color "orange"
                     :text-decoration "none"))
     (("a:focus") (
                   :color "orange"
                   :text-decoration "none"))
     (("a:hover") (
                   :color "orange"
                   :text-decoration "none"))
     (("a:active") (
                    :color "orange"
                    :text-decoration "none")))
    (("div.categories") (
                         :font-size "80%")
     ;; todo macro for generating link styling
     (("a:link") (
                  :color "orange"
                  :text-decoration "none"))
     (("a:visited") (
                     :color "orange"
                     :text-decoration "none"))
     (("a:focus") (
                   :color "orange"
                   :text-decoration "none"))
     (("a:hover") (
                   :color "orange"
                   :text-decoration "none"))
     (("a:active") (
                    :color "orange"
                    :text-decoration "none")))
    (("legend.categories") (
                            :font-size "90%"
                            :color "darkred"))
    (("table") (:border-collapse "collapse"))
    ((".bookmark")  (
                     ;; :border "solid 1px lightgray"
                     :padding "2px")
     (("a:link") (
                  :color "blue"
                  :text-decoration "none"))
     (("a:visited") (
                     :color "darkblue"
                     :text-decoration "none"))
     (("a:focus") (
                   :color "blue"
                   :text-decoration "underline"))
     (("a:hover") (
                   :color "blue"
                   :text-decoration "underline"))
     (("a:active") (
                    :color "red"
                    :text-decoration "underline"))
     (("button.add-tag") (
                          :font-size "70%;")))
    ((".ui-autocomplete") (
                           :background-color "#f0f0f0"
                           :font-size "90%"
                           :color "darkred"
                           :border "solid 1px lightgray"))
    ((".ui-state-focus") (
                          :color "red"
                          :background-color "#e0e0e0"))))
