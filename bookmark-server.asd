(defsystem bookmark-server
  :depends-on (ol-utils
               web-utils
               hunchentoot cl-who
               clsql clsql-sqlite3
               split-sequence)
  :serial t
  :components ((:file "bookmarks")
               (:file "web-interface")))
