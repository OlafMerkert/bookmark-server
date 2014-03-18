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
     (:a :href (ps-inline (user-message "Hallo du!")) "Give me a message")
     ,@body))


(define-easy-handler (bookmarks-list :uri "/bookmarks/list") (category)
  (bookmark/document (:title "All bookmarks as list")
    ;; todo Form for creating new bookmarks
    ;; List of present bookmarks
    (:br)
    ;; todo display list of categories for quick filtering
    (:div :id (cc bookmarks-list)
            (mapc #'single-bookmark (bm:all-bookmarks)))
    ))

(define-easy-handler (bookmarks-tree :uri "/bookmarks/tree") ()
  (bookmark/document (:title "All bookmarks as tree")
    ;; todo Form for creating new bookmarks
    ;; List of present bookmarks
    (:br)
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
              (render-bm-tree (bm-tree:build-tree (bm:all-bookmarks)))))
    ))


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
            (@@ parent (last) (parent))
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
      (user-message (@@ ($ this) (text))))

    (defun add-category-ui (bm category)
      (let ((cat-el ($ (who-ps-html (:a :class "category" :href "#" category)))))
        (@@ cat-el (click category-click))
        (@@ bm (children ".categories") (append " ") (append cat-el))))

    (bind-event document ready ()
      (bind-event "button.add-tag" click ()
        (let ((bm (current-bookmark this)))
          (add-category-ui bm "super")
          ))

      (@@ ($ "a.category") (click category-click))
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
    (("#messageContainer") (
                            :position "absolute"
                            :top "20px"
                            :right "20px"
                            :width "30%"
                            :background "khaki"))
    (("span.categories") (
                          :margin-left "2ex"
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
    (("a.category") (
                
                 
                     ))
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
