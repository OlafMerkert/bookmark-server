(defpackage :bookmark-chrome-import
  (:nicknames :bm-chrimp)
  (:use :cl :ol :iterate )
  (:export))

(in-package :bookmark-chrome-import)

(defpar chrome-bookmark-file
    #P"~/.config/google-chrome/Default/Bookmarks")

(defun keys (json)
  (mapcar #'car json))

(defun collect-bookmarks-from-json (json)
  (let ((type (assoc1 :type json)))
    (cond ((equal type "folder")
           (mapc #'collect-bookmarks-from-json
                 (assoc1 :children json)))
          ((equal type "url")
           (bm:add-bookmark (assoc1 :url json)
                            (assoc1 :name json))))))

(defun import-chrome-bookmarks ()
  (let ((json (cl-json:decode-json-from-source chrome-bookmark-file)))
    (collect-bookmarks-from-json (assoc1* :other :roots json))
    (collect-bookmarks-from-json (assoc1* :bookmark--bar :roots json))))
