(defpackage :ajax-actions
  (:shadowing-import-from :parenscript #:this #:in)
  (:use :cl :ol
        :web-utils
        :hunchentoot :cl-who
        :parenscript)
  (:export
   #:ajax-action-js-code
   #:define-ajax-action+
   #:jslet-produce-json
   #:ajax-action-server-component
   #:ajax-call
   #:define-ajax-action))

(in-package :ajax-actions)

(defpar breadcrumb-ajax-root '(bookmarks ajax))

(defmacro define-ajax-action (breadcrumb parameters &body body)
  `(define-easy-handler (,(apply #'symb 'ajax- (splice-in '- breadcrumb ))
                          :uri ,(breadcrumb->url (append breadcrumb-ajax-root breadcrumb)))
       ,parameters
     (format nil "~S" (progn ,@body))))

;; *** integration of lisp and js code via ajax
;; information needed:
;; a name for the ajax call
;; the parameters and returned information
;; condition handling; optimally taking care of both the lisp and js side
;; if there is some return data other than condition handling stuff, provide a clear separation between js and lisp, we are bound to have nesting like (js (lisp (js ...)))
;; **** js conditions for ajax query
;; `complete': when the request is complete
;; `error': when complete, but with error
;; `success': when complete, and no error

;; essentially we can always generate a generic error message if something goes wrong `error', and continue code on `success'.
;; **** lisp conditions
;; might contain data (depending on condition class), which we might want to preprocess on the server side, then we send the data to the client.
;; **** js code generation
;; we want one single toplevel form for each ajax action, which contains both the js and lisp code, perhaps the js code can be collected somewhere and produced as one single script.

(defvar ajax-action-js-code (make-hash-table))

(define-easy-handler (ajax-actions-js :uri (breadcrumb->url (append1 breadcrumb-ajax-root "actions.js"))) ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (with-output-to-string (stream)
    (maphash (ilambda (k v)
               (princ v stream)
               (terpri stream))
             ajax-action-js-code)))

(defun jslet-produce-json (bindings)
  (with-gensyms!
    `(with-output-to-string (,g!stream)
       (json:with-object (,g!stream)
         ,@(mapcan (lambda (b)
                     (mvbind (key value)
                         (cond ((symbolp b) (values b b))
                               ((consp b) (values-list b))
                               (t (error "invalid binding ~A in jslet expression" b)))
                       `((json::next-aggregate-member 'json::object ,g!stream)
                         ;; write out a string directly, because
                         ;; cl-json always puts "" around our nice
                         ;; symbol names
                         (write-string ,(funcall json:*lisp-identifier-name-to-json* (mkstr key))
                                       ,g!stream)
                         (write-char #\: ,g!stream)
                         (json:encode-json ,value ,g!stream))))
                   bindings)))))

(defun ajax-action-server-component (breadcrumb action-name ajax-call-server ajax-call-client ajax-call-conditions)
  (with-gensyms!
    (let ((function-name (symb 'ajax-action- action-name)))
      `(define-easy-handler (,function-name :uri ,(breadcrumb->url (append breadcrumb-ajax-root breadcrumb)))
           ;; extract the parameters
           ,(mapcar #'symb (odd-elements (first ajax-call-server)))
         (handler-case
             ;; use multiple values to make return of :server part
             ;; available in the :client part
             ;; todo perhaps there is a more consistent way even?
             ;; check out :no-error !!
             (mvbind ,(first ajax-call-client)
                 (progn
                   ,@(rest ajax-call-server))
               ;; handle condition "NONE"
               ,(jslet-produce-json
                 (list* '(:condition "NONE")
                        (second ajax-call-client))))
           ,@(mapcar (lambda (condition)
                       (dbind (cond-name slots jslet &rest body) condition
                         (declare (ignore body))
                         `(,cond-name (,g!condition)
                                      (with-slots ,slots ,g!condition
                                        ,(jslet-produce-json
                                          (list* `(condition ,(mkstr cond-name))
                                                 jslet))))))
                     ajax-call-conditions))))))

(defmacro define-ajax-action+ (breadcrumb js-parameters &body js-code)
  (let* ((action-name (apply #'symb (splice-in '- breadcrumb)))
         (ajax-call (tree-find-if (lambda (x) (and (consp x) (eq (car x) 'ajax-call)))
                                  js-code))
         (ajax-call-server (assoc1 :server (rest ajax-call)))
         (ajax-call-client (assoc1 :client (rest ajax-call)))
         (ajax-call-conditions (remove-if (lambda (x) (keywordp (car x))) (rest ajax-call))))
    ;; for the js code generation, add the breadcrumb information
    (setf (car ajax-call) 'ajax-call%
          (cdr ajax-call) (cons breadcrumb (cdr ajax-call)))
    `(progn
       ,(ajax-action-server-component breadcrumb action-name ajax-call-server ajax-call-client ajax-call-conditions)
       ,(ajax-action-client-component action-name js-parameters js-code))))

(defun ajax-action-client-component (action-name js-parameters js-code) 
  `(setf (gethash ',action-name ajax-action-js-code)
         (ps (defun ,action-name ,js-parameters ,@js-code))))

(defmacro/ps ajax-call% (breadcrumb &rest handlers)
  (let ((ajax-call-server (assoc1 :server handlers))
        (ajax-call-client (assoc1 :client handlers))
        (ajax-call-conditions (remove-if (lambda (x) (keywordp (car x))) handlers)))
    `(@@ $ (ajax (create :url ,(breadcrumb->url (append breadcrumb-ajax-root breadcrumb))
                         :data (create ,@(first ajax-call-server))
                         :type "GET"
                         :error (lambda ()
                                  (alert ,(format nil "Server-Client communication problem: Action ~A failed" (breadcrumb->url breadcrumb))))
                         :success
                         (lambda (json)
                           (cond
                             ,@(mapcar
                               (lambda (condition)
                                 (dbind (name slots js-let &rest body) condition
                                   (declare (ignore slots))
                                   `((= (@ json condition) ,(mkstr name))
                                     (symbol-macrolet ,(mapcar #`(,(unbox1 a1) (@ json ,(unbox1 a1))) js-let)
                                       ,@body))))
                               ;; normal condition comes first
                               (cons (list* "NONE" ajax-call-client)
                                     ajax-call-conditions)))))))))
