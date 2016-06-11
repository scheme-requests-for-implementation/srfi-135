;;; Experimental version of kernel0.

(define-library (srfi 135 kernel1)

  (export

   ;; for internal use only

   complain               ; for reporting illegal arguments

   text-rtd               ; FIXME: for debugging only
   %new-text              ; FIXME: for debugging only
   text.k text.chunks     ; FIXME: for debugging only

   %text-length           ; non-checking version
   %text-ref              ; non-checking version
   %string->text          ; 1-argument version

   N                      ; preferred text size for pieces of long texts
   the-empty-text         ; there should be only one empty text

   ;; will be exported by (srfi 135)

   text?
   text-tabulate
   text-length
   text-ref
   subtext
   text-concatenate
   )

  (import (scheme base)
          (primitives typetag vector-like? vector-ref:trusted))

  (include "kernel1.body.scm"))
