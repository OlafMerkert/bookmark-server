(defsystem bookmark-server
  :depends-on (ol-utils
               web-utils
               hunchentoot cl-who
               clsql clsql-sqlite3
               split-sequence
               css-lite
               ;; uiop
               parenscript
               cl-json
               cl-containers)
  :serial t
  :components ((:file "bookmarks")
               (:file "bookmark-rules")
               (:file "chrome-import")
               (:file "bookmark-tree")
               (:file "ajax-actions")
               (:file "web-interface")
               ))
