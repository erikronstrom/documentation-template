;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DOCUMENTATION-TEMPLATE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/documentation-template/output.lisp,v 1.19 2014-11-23 12:12:59 edi Exp $

;;; Copyright (c) 2006-2014, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :documentation-template)

(defun write-entry-header (name type &key (write-name-p t))
  "Writes the header for a documentation entry of name NAME and
type TYPE.  The HTML anchor will only get a 'name' attribute if
WRITE-NAME-P is true and NAME is not a SETF name."
  (case *format*
    (:html
     (format t "~%~%<!-- Entry for ~A -->~%~%<p><br><span class=\"type\">[~A]</span><br><a class=\"none\"~@[ name='~A'~]>" 
             name type (and write-name-p (atom name) (string-downcase name))))
    (:markdown
     (format t "~%~%(~A)~%" 
             type (and write-name-p (atom name) (string-downcase name))))))

(defun auto-format (string)
  (case *format*
    (:html
     (setf string (cl-ppcre:regex-replace-all "(?<!\\w)(\\:[\\w\\-]+)" string "<code>\\1</code>"))
     (setf string (cl-ppcre:regex-replace-all "\\bNOTE: " string "<strong>NOTE:</strong> "))
     (loop for sym in *all-exported-symbols* do
           (setf string (cl-ppcre:regex-replace-all (format nil "\\b(~a)\\b" (cl-ppcre:regex-replace-all "([\\*\\+])" sym "\\\\\\1")) string "<a href=\"#\\1\"><code>\\1</code></a>"))))
    (:markdown
     (setf string (cl-ppcre:regex-replace-all "(?<!\\w)(\\:[\\w\\-]+)" string "`\\1`"))
     (loop for sym in *all-exported-symbols* do
           (setf string (cl-ppcre:regex-replace-all (format nil "\\b(~a)\\b" (cl-ppcre:regex-replace-all "([\\*\\+])" sym "\\\\\\1")) string "`\\1`")))))
  string)

(defun write-entry-footer (name doc-string)
  "Writes the footer for a documentation entry for the name NAME
including the documentation string DOC-STRING."
  (case *format*
    (:html
     (let ((doc-string (when doc-string (cl-ppcre:regex-replace-all "\\n\\n" (escape-string-iso-8859-1 doc-string) (format nil "</p>~%<p>")))))
       (format t "~%<blockquote>~%~%<p>~@[~A~]</p>~%~%</blockquote>~%~%<!-- End of entry for ~A -->~%"
               ;(and doc-string (escape-string-iso-8859-1 doc-string))
               (when doc-string (auto-format doc-string))
               name)))
    (:markdown
     (format t "~%~%~@[~A~]~%~%" (when doc-string (auto-format doc-string))))))

(defun write-constant-entry (symbol doc-string)
  "Writes a full documentation entry for the constant SYMBOL."
  (write-entry-header symbol "Constant")
  (case *format*
    (:html (format t "<b>~A</b></a>" (string-downcase symbol)))
    (:markdown (format t "**~A**" (string-downcase symbol))))
  (write-entry-footer symbol doc-string))

(defun write-special-var-entry (symbol doc-string)
  "Writes a full documentation entry for the special variable
SYMBOL."
  (write-entry-header symbol "Special variable")
  (case *format*
    (:html (format t "<b>~A</b></a>" (string-downcase symbol)))
    (:markdown (format t "**~A**" (string-downcase symbol))))
  (write-entry-footer symbol doc-string))

(defun write-class-entry (symbol doc-string)
  "Writes a full documentation entry for the class SYMBOL."
  (write-entry-header symbol (if (subtypep symbol 'condition)
                               "Condition type" "Standard class"))
  
  (case *format*
    (:html (format t "<b>~A</b></a>" (string-downcase symbol)))
    (:markdown (format t "**~A**" (string-downcase symbol))))
  (write-entry-footer symbol doc-string))

(defun write-lambda-list* (lambda-list &optional specializers)
  "The function which does all the work for WRITE-LAMBDA-LIST and
calls itself recursive if needed."
  (let (body-seen after-required-args-p (firstp t))
    (dolist (part lambda-list)
      (cond (body-seen (setq body-seen nil))
            (t (when (and (consp part) after-required-args-p)
                 (setq part (first part)))
               (unless firstp
                 (write-char #\Space))
               (setq firstp nil)
               (cond ((consp part)
                      ;; a destructuring lambda list - recurse
                      (write-char #\()
                      (write-lambda-list* part)
                      (write-char #\)))
                     ((member part '(&key &optional &rest &allow-other-keys &aux &environment &whole))
                      ;; marks these between <tt> and </tt>
                      (setq after-required-args-p t)
                      (case *format*
                        (:html (format t "<tt>~A</tt>" (escape-string (string-downcase part))))
                        (:markdown (format t "`~A`" (string-downcase part)))))
                     ((eq part '&body)
                      ;; we don't really write '&BODY', we write it
                      ;; like in the CLHS
                      (setq body-seen t
                            after-required-args-p t)
                      (write-string "declaration* statement*"))
                     (t
                      (let ((specializer (pop specializers)))
                        (cond ((and specializer (not (eq specializer t)))
                               ;; add specializers if there are any left
                               (case *format*
                                 (:html
                                  (write-string (escape-string
                                                 (string-downcase
                                                  (format nil "(~A ~A)" part specializer)))))
                                 (:markdown
                                  (write-string (string-downcase
                                                  (format nil "(_~A_ _~A_)" part specializer))))))
                              (t
                               (case *format*
                                 (:html
                                  (write-string (escape-string (string-downcase part))))
                                 (:markdown
                                  (format t "_~a_" (string-downcase part))))))))))))))

(defun write-lambda-list (lambda-list &key (resultp t) specializers)
  "Writes the lambda list LAMBDA-LIST, optionally with the
specializers SPECIALIZERS.  Adds something like `=> result' at
the end if RESULTP is true."
  (write-string (case *format* (:html "<i>") (:markdown "")))
  (write-lambda-list* lambda-list specializers)
  (write-string (case *format* (:html "</i>") (:markdown "")))
  (when resultp
    (case *format*
      (:html (write-string " =&gt; <i>result</i></a>"))
      (:markdown (write-string " => _result_")))))

(defun write-macro-entry (symbol lambda-list doc-string)
  "Writes a full documentation entry for the macro SYMBOL."
  (write-entry-header symbol "Macro")
  (format t "<b>~A</b> " (string-downcase symbol))
  (write-lambda-list lambda-list)
  (write-entry-footer symbol doc-string))

(defun write-function-entry (name lambda-list doc-string other-entries
                                  &key genericp signature-only-p specializers qualifiers)
  "Writes a full documentation entry for the function, generic
function, or method with name NAME.  NAME is a generic function
if GENERICP is true, SPECIALIZERS is a list of specializers,
i.e. in this case NAME is a method.  Likewise, QUALIFIERS is a
list of qualifiers.  SIGNATURE-ONLY-P means that we don't want a
full header."
  (let* ((setfp (consp name))
         (symbol (if setfp (second name) name))
         (type (cond (specializers :method)
                     (genericp :generic-function)
                     (t :function)))
         ;; check if this is a reader for which there is a writer (so
         ;; we have an accessor) with the same signature
         (writer (and (not setfp)
                      (find-if (lambda (entry)
                                 (and (equal `(setf ,name) (first entry))
                                      (eq type (second entry))
                                      (or (null specializers)
                                          (equal specializers (rest (fifth entry))))))
                               other-entries)))
         (resultp (and (not setfp)
                       (null (intersection '(:before :after)
                                           qualifiers)))))    
    (cond (signature-only-p
           (write-string "<a class=none>"))
          (t
           (write-entry-header name (if writer
                                      (ecase type
                                        (:method "Specialized accessor")
                                        (:generic-function "Generic accessor")
                                        (:function "Accessor"))
                                      (ecase type
                                        (:method "Method")
                                        (:generic-function "Generic function")
                                        (:function "Function")))
                               :write-name-p (null specializers))))
    (cond
     (setfp
      (case *format*
        (:html (format t "<tt>(setf (</tt><b>~A</b> " (string-downcase symbol)))
        (:markdown (format t "`(setf (~a " (string-downcase symbol))))
      (write-lambda-list (rest lambda-list) :resultp resultp :specializers (rest specializers))
      (case *format*
        (:html (write-string "<tt>)</tt> "))
        (:markdown (write-string "` ")))
      ;; we should use the specializer here as well
      (case *format*
        (:html (format t "<i>~A</i><tt>)</tt></a>~(~{<tt> ~S</tt>~^~}~)" (string-downcase (first lambda-list)) qualifiers))
        (:markdown (format t "`~a`) ~(~{<tt> ~S</tt>~^~}~)" (string-downcase (first lambda-list)) qualifiers))))
     (t
      (case *format*
        (:html (format t "<b>~A</b> " (string-downcase symbol)))
        (:markdown (format t "**~a** " (string-downcase symbol))))
      (write-lambda-list lambda-list :specializers specializers :resultp resultp)
      (case *format*
        (:html (format t "~(~{<tt> ~S</tt>~^~}~)" qualifiers))
        (:markdown "~(~{<tt> ~S</tt>~^~}~)" qualifiers))))
    (when writer
      ;; if this is an accessor, the add the writer immediately after
      ;; the reader..
      (case *format*
        (:html (format t "~%<br>"))
        (:markdown (format t "~%~%")))
      (destructuring-bind (name doc-type lambda-list doc-string &optional specializers qualifiers)
          writer
        (declare (ignore doc-type doc-string))
        (write-function-entry name lambda-list nil nil
                              :signature-only-p t
                              :specializers specializers
                              :qualifiers qualifiers))
      ;; ...and remove it from the list of entries which haven't been
      ;; written yet
      (setq other-entries (remove writer other-entries))))
  (unless signature-only-p
    (write-entry-footer name doc-string))
  other-entries)

(defun write-entry (entry other-entries)
  "Write one documentation entry corresponding to ENTRY.
OTHER-ENTRIES is the list of the remaining entries waiting to be
written.  OTHER-ENTRIES, probably updated, will be returned."
  (destructuring-bind (name doc-type lambda-list doc-string &optional specializers qualifiers)
      entry
    (unless (or (consp name) specializers)
      ;; add NAME to index list unless it's a SETF name or the name of
      ;; a method
      (push name *symbols*))
    (ecase doc-type
      (:constant (write-constant-entry name doc-string))
      (:special-var (write-special-var-entry name doc-string))
      (:class (write-class-entry name doc-string))
      (:macro (write-macro-entry name lambda-list doc-string))
      (:function (setq other-entries
                       (write-function-entry name lambda-list doc-string other-entries)))
      (:generic-function (setq other-entries
                               (write-function-entry name lambda-list doc-string other-entries
                                                     :genericp t)))
      (:method (setq other-entries
                     (write-function-entry name lambda-list doc-string other-entries
                                           :specializers specializers
                                           :qualifiers qualifiers)))))
  other-entries)

(defun write-page-header (package-name subtitle version symbols)
  "Writes the header of the HTML page.  Assumes that the library
has the same name as the package.  Adds a list of all exported
symbols with links."
  (case *format*
    (:html (format t "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">
<html> 

<head>
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">
  <title>~A - ~A</title>
  <style type=\"text/css\">
  pre { padding:5px; background-color:#e0e0e0 }
  h3 { margin-top: 40px }
  h3, h4 { text-decoration: underline; }
  a { text-decoration: none; padding: 1px 2px 1px 2px; }
  a:visited { text-decoration: none; padding: 1px 2px 1px 2px; }
  a:hover { text-decoration: none; padding: 1px 1px 1px 1px; border: 1px solid #000000; } 
  a:focus { text-decoration: none; padding: 1px 2px 1px 2px; border: none; }
  a.none { text-decoration: none; padding: 0; }
  a.none:visited { text-decoration: none; padding: 0; } 
  a.none:hover { text-decoration: none; border: none; padding: 0; } 
  a.none:focus { text-decoration: none; border: none; padding: 0; } 
  a.noborder { text-decoration: none; padding: 0; } 
  a.noborder:visited { text-decoration: none; padding: 0; } 
  a.noborder:hover { text-decoration: none; border: none; padding: 0; } 
  a.noborder:focus { text-decoration: none; border: none; padding: 0; }  
  pre.none { padding:5px; background-color:#ffffff }
  .type { color: #999 }
  </style>
</head>

<body bgcolor=white>

<h2>~2:*~A - ~A</h2>

<blockquote>
  <h3><a name=\"abstract\" class=\"none\">Abstract</a></h3>

  <p>The code comes with a <a href=\"http://www.opensource.org/licenses/bsd-license.php\">BSD-style
  license</a> so you can basically do with it whatever you want.</p>

  <p><span style=\"color: red\">Download shortcut:</span> <a href=\"http://weitz.de/files/~A.tar.gz\">http://weitz.de/files/~:*~A.tar.gz</a>.</p>
</blockquote>

<h3><a class=none name=\"contents\">Contents</a></h3>
<ol>
  <li><a href=\"#download\">Download</a>
  <li><a href=\"#dictionary\">The ~A dictionary</a>
    <ol>
~{      <li><a href=\"#~A\"><code>~:*~A</code></a>
~}    </ol>
  <li><a href=\"#ack\">Acknowledgements</a>
</ol>

<h3><a class=none name=\"download\">Download</a></h3>

<p>~A together with this documentation can be downloaded from <a
href=\"http://weitz.de/files/~A.tar.gz\">http://weitz.de/files/~:*~A.tar.gz</a>. The
current version is ~A.</p>

<h3><a class=none name=\"dictionary\">The ~A dictionary</a></h3>

"
          package-name subtitle (string-downcase package-name) package-name
          symbols package-name (string-downcase package-name) version package-name))
    (:markdown
     (format t "# ~A - ~A

## Abstract

The code comes with
a [BSD-style license](http://www.opensource.org/licenses/bsd-license.php) so you can basically do with it whatever you want.

## Contents
- [Download](#download)
- The ~A dictionary
~{  - [`~A`](#~:*~A)
~}
- [Acknowledgements](#acknowledgements)

## Download

~A together with this documentation can be downloaded from [www.](http://www.)
current version is ~a.

## The ~A dictionary
" package-name subtitle (string-downcase package-name)
               symbols (string-downcase package-name) version (string-downcase package-name)))))

(defun write-page-footer ()
  "Writes the footer of the page."
  (case *format*
    (:html (write-string "

<h3><a class=none name=\"ack\">Acknowledgements</a></h3>

<p>
This documentation was prepared with a <a href=\"https://github.com/erikronstrom/documentation-template\">patched version</a> of <a href=\"https://edicl.github.io/documentation-template/\">DOCUMENTATION-TEMPLATE</a>.
</p>

</body>
</html>"))
    (:markdown
     (write-string "

## Acknowledgements
This documentation was prepared with a [patched version](https://github.com/erikronstrom/documentation-template) of [DOCUMENTATION-TEMPLATE](https://edicl.github.io/documentation-template/)
"))))

(defun create-template (package &key (target (or *target*
                                                 #-:lispworks (error "*TARGET* not specified.")
                                                 #+:lispworks
                                                 (capi:prompt-for-file "Select an output target:"
                                                                       :operation :save
                                                                       :filters '("HTML Files" "*.html"
                                                                                  "All Files" "*.*")
                                                                       :filter "*.html")))
                                     (format :html)
                                     (subtitle nil subtitle-provided-p)
                                     (version nil)
                                     ((:maybe-skip-methods-p *maybe-skip-methods-p*)
                                      *maybe-skip-methods-p*)
                                     (if-exists :supersede)
                                     (if-does-not-exist :create))
  "Writes an HTML page with preliminary documentation entries and an
index for all exported symbols of the package PACKAGE to the file
TARGET.  If MAYBE-SKIP-METHODS-P is true, documentation entries for
inidividual methods are skipped if the corresponding generic function
has a documentation string."
  (when target
    (setq *target* target))
  (when format
    (setq *format* format))
  (unless subtitle-provided-p
    (let ((system (ignore-errors (asdf/system:find-system package))))
      (when system
        (setf subtitle (asdf/system:system-description (asdf/system:find-system :cl-smufl))))))
  (let (*symbols*)
    (with-open-file (*standard-output* target
                                       :direction :output
                                       :if-exists if-exists
                                       :if-does-not-exist if-does-not-exist)
      (let ((body
             (with-output-to-string (*standard-output*)
               (let* ((entries (collect-all-doc-entries package))
                      (*all-exported-symbols* (remove-duplicates (mapcar (lambda (x) (format nil "~(~a~)" x)) (mapcar #'first entries)) :test #'equalp)))
                 (loop
                  (let ((entry (or (pop entries) (return))))
                    (setq entries (write-entry entry entries))))))))
        (write-page-header (package-name package) subtitle version
                           (mapcar #'string-downcase (reverse *symbols*)))
        (write-string body)
        (write-page-footer))))
  (values))
