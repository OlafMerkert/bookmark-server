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

(defun jslet-produce-json (bindings)
  `(with-output-to-string (*json-output*)
    (json:with-object ()
      ,@(mapcar
         (lambda (b)
           (cond ((symbolp b)
                  `(json:encode-object-member ',b ,b))
                 ((consp b)
                  `(json:encode-object-member ',(first b) ,(second b)))
                 (t (error "invalid binding ~A in jslet expression" b))))
         bindings))))

(defun ajax-action-server-component (breadcrumb action-name ajax-call-server ajax-call-client ajax-call-conditions)
  (with-gensyms!
    (let ((function-name (symb 'ajax-action- action-name)))
      `(define-easy-handler (,function-name :uri ,(breadcrumb->url (append breadcrumb-ajax-root breadcrumb)))
           ;; extract the parameters
           ,(mapcar #'symb (odd-elements (first ajax-call-server)))
         (handler-case
             (progn
               ,@(rest ajax-call-server)
               ;; handle condition "NONE"
               ,(jslet-produce-json
                 (list* '(:condition 'none)
                        (first ajax-call-client))))
           ,(mapcar (lambda (condition)
                      (dbind (cond-name slots jslet &rest body) condition
                        (declare (ignore body))
                        `(,cond-name (,g!condition)
                                     (with-slots ,slots ,g!condition
                                       ,(jslet-produce-json
                                         (list* `(:condition ,cond-name)
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
                               (cons (list* 'none nil ajax-call-client)
                                     ajax-call-conditions)))))))))
