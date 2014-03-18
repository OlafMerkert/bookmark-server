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

(define-easy-handler (bookmarks-start :uri "/bookmarks") ()
  (html/document (:title #1="Bookmark Server")
    (:h1 #1#)
    (:p "View the " (:a :href (breadcrumb->url '(bookmarks list)) "list") " of bookmarks.")))

(defmacro bookmark/document ((&key title) &body body)
  `(html/document (:title ,title
                          ;; todo use breadcrumbs?
                          :style "/bookmarks/style.css"
                          :script "/scripts/jquery-1.10.2.min.js"
                          :script "/scripts/sticky/sticky.js"
                          :style "/scripts/sticky/sticky.css"
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
                 (lambda (c) (@@ ($ c) (text))))))

    (defun category-click ()
      (@@ event (prevent-default))
      (user-message (current-bookmark ($ this)))
      (user-message (@@ ($ this) (text))))

    (defun add-category-ui (bm category)
      (let ((cat-el ($ (who-ps-html (:a :class "category" :href "#" category)))))
        (@@ cat-el (click category-click))
        (@@ bm (children ".categories") (append " ") (append cat-el))
        ;; todo add to filter list??
        ))

    (bind-event document ready ()
      (bind-event "button.add-tag" click ()
        (let ((bm (current-bookmark this)))
          (add-category-ui bm "super")
          ))

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
                          :font-size "70%;"))) ))
