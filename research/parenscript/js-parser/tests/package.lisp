;;;; package.lisp
;;; Defines the package used by the unit tests
;;;
;;; Copyright (c) 2005-2006 James Wright
;;; See LICENSE for full licensing details.
;;;

(defpackage :js-parser-tests
  (:use :cl :rtest :cl-ppcre :js-parser)
  (:nicknames)
  (:import-from js-parser
                ;; lexer-specific symbols for testing
                javascript-lexer
                make-lexer-function
                regexp-re
                token
                token-terminal token-value token-start token-end
                next-token
                coerce-token
                make-load-form
                set-cursor
                position-to-line/column
                encountered-line-terminator
                
                ;; source-model structure types
                source-element
                special-value
                identifier
                numeric-literal
                string-literal
                array-literal
                object-literal
                re-literal
                new-expr
                fn-call
                property-access
                unary-operator
                binary-operator
                conditional
                comma-expr
                var-decl-statement
                var-decl
                statement-block
                if-statement
                do-statement
                while
                for
                for-in
                continue-statement
                break-statement
                return-statement
                with
                switch
                case-clause
                default-clause
                throw-statement
                try
                catch-clause
                finally-clause
                function-decl
                function-expression
                continuation-function
                thunk-function
                continuation-call
                suspend-statement
                resume-statement
                import-decl
                add-handler
                remove-handler
                
                ;; constructors for source-model structures
                make-source-element
                make-special-value
                make-identifier
                make-numeric-literal
                make-string-literal
                make-array-literal
                make-object-literal
                make-re-literal
                make-new-expr
                make-fn-call
                make-property-access
                make-unary-operator
                make-binary-operator
                make-conditional
                make-comma-expr
                make-var-decl-statement
                make-var-decl
                make-statement-block
                make-if-statement
                make-do-statement
                make-while
                make-for
                make-for-in
                make-continue-statement
                make-break-statement
                make-return-statement
                make-with
                make-switch
                make-case-clause
                make-default-clause
                make-throw-statement
                make-try
                make-catch-clause
                make-finally-clause
                make-function-decl
                make-function-expression
                make-suspend-statement
                make-resume-statement
                make-import-decl
                make-add-handler
                make-remove-handler
                
                ;; frequently-used accessors
                source-element-start
                source-element-end
                
                ;; structure management
                get-constructor
                structure-slots
                make-keyword

                ;; pretty-printer-specific symbols
                ;pretty-print
                ;with-indent
                ;*indent*
                ;*escape-script-end-tags*

                ;; ugly-printer symbols
                ;genvar
                ;*genvar-counter*
                ;ugly-print

                ))
                