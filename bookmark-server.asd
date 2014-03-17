(defsystem bookmark-server
  :depends-on (ol-utils
               web-utils
               hunchentoot cl-who
               clsql clsql-sqlite3
               split-sequence
               css-lite
               ;; uiop
               parenscript
               cl-json)
  :serial t
  :components ((:file "bookmarks")
               (:file "chrome-import")
               (:file "ajax-actions")
               (:file "web-interface")))
