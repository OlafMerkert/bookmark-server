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
    (:table :id (cc bookmarks-table)
            (dolist (bm (bm:all-bookmarks))
              (htm
               (:tr :class "bookmark"
                    (:td :class "bookmark-link" 
                         (:a :target "_blank" :href (bm:url bm)
                             (esc (bm:title bm))))
                    (:td :class "categories"
                         (dolist (c (bm:categories bm))
                           (str c)
                           (str " ")))
                    #|(:td :class "url"
                         (:span :class ".hidden"(esc (bm:url bm))))|#))))
    ))

(define-easy-handler (bookmarks-tree :uri "/bookmarks/tree") ()
  (bookmark/document (:title "All bookmarks as tree")
    ;; todo Form for creating new bookmarks
    ;; List of present bookmarks
    (:br)
    (:table :id (cc bookmarks-table)
            (labels ((render-bm-tree (tree)
                       (cond ((null tree))
                             ((atom tree)
                              (htm (:div :class "bookmark"
                                         (:a :target "_blank" :href (bm:url tree)
                                             (esc (bm:title tree)))
                                         (str " ")
                                         (:span :class "categories"
                                                (dolist (c (bm:categories tree))
                                                  (str c)
                                                  (str " ")) ))))
                             ((symbolp (car tree))
                              (htm (:fieldset :class "category"
                                              (:legend :class "categories"
                                                       (str (car tree)))
                                              (render-bm-tree (cdr tree)))))
                             (t (render-bm-tree (car tree))
                                (render-bm-tree (cdr tree))))))
              (render-bm-tree (bm-tree:build-tree (bm:all-bookmarks)))))
    ))

(define-easy-handler (bookmarks-js :uri (breadcrumb->url (append1 bm-root "logic.js"))) ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (ps
    (bind-event document ready ()
      (@@ ($ ".hidden" ) (hide) (css "visibility" "visible"))
      
      ;; hiding/unhiding url of bookmark
      (bind-event ".bookmark" mouseover ()
        (@@ ($ this) (find ".hidden") (show)))
      (bind-event ".bookmark" mouseout ()
        (@@ ($ this) (find ".hidden") (hide)))
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
    ((".categories") (
                      :font-size "80%"
                      :color "orange"))
    (("legend.categories") (
                            :font-size "90%"
                            :color "darkred"))
    (("table") (:border-collapse "collapse"))
    (("td") (
             :border "solid 1px lightgray"
             :padding "2px"))
    ((".bookmark") ()
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
                    :text-decoration "underline"))) ))
