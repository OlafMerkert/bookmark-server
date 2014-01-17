(in-package :bookmarks)

(locally-enable-sql-reader-syntax)

;; (start-sql-recording)

;; (setf b1 (make-instance 'bookmark :title "xy" :url "test")
;;       b2 (make-instance 'bookmark :title "yz" :url "another")
;;       b3 (make-instance 'bookmark :title "ab" :url "certainly"))

;; (dolist (x (list b1 b2 b3))
;;   (update-records-from-instance x))

(web-utils:start-server)
