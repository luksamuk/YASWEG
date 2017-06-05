;;(define-module (yasweg generator)
;;  #:export ())

(use-modules ((ice-9 format)))

;; ============== Global variables =========================

(define *stahtml-file* #f)


;; ============== Global utilities =========================

(define yasweg-open
  (λ (file-name)
    (if *stahtml-file*
        (error "Static HTML file is already open!")
        (begin
          ;; Open HTML file
          (set! *stahtml-file* (open-file file-name "w"))
          ;; Write beginning of HTML file
          (display "<!DOCTYPE html>\n" *stahtml-file*)
          (display "<html>\n" *stahtml-file*)))))

(define yasweg-close
  (λ ()
    (if (not *stahtml-file*)
        (error "Static HTML file is not open!")
        (begin
          (display "</html>\n" *stahtml-file*)
          (close-port *stahtml-file*)
          (set! *stahtml-file* #f)))))


;; ================= Basic section macros ========================

(define-syntax yasweg-on-page
  (syntax-rules ()
    ((yasweg-on-page filename body ...)
     (if (not *stahtml-file*)
         (begin
           (yasweg-open filename)
           body ...
           (yasweg-close))
         (error "Static HTML file is already open!")))))

(define-syntax yasweg-link-css
  (syntax-rules ()
    ((yasweg-link-css sheet-ref)
     (if *stahtml-file*
         (begin
           (display
            (format #f "<link rel=\"stylesheet\" href=\"~a\">\n"
                    sheet-ref)
            *stahtml-file*))))))

(define-syntax yasweg-on-head
  (syntax-rules ()
    ((yasweg-on-head body ...)
     (if *stahtml-file*
         (begin
           ;; Start header
           (display "<head>\n" *stahtml-file*)
           ;; Include boostrap
           (yasweg-link-css
            "https://s3.amazonaws.com/codecademy-content/courses/ltp/css/bootstrap.css")
           ;; Push other arguments
           body ...
           ;; Close header
           (display "</head>\n" *stahtml-file*))
         (error "Static HTML file was not opened!")))))

(define-syntax yasweg-on-body
  (syntax-rules ()
    ((yasweg-on-body body ...)
     (if *stahtml-file*
         (begin
           ;; Follow the on-head model
           (display "<body>\n" *stahtml-file*)
           body ...
           (display "</head>\n" *stahtml-file*))
         (error "Static HTML file was not opened!")))))

;; ===================== Basic formatting macros ======================

(define-syntax yasweg-h
  (syntax-rules ()
    ((yasweg-h level text)
     (if *stahtml-file*
         (begin
           (display
            (format #f "<h~d>~a</h~d>\n"
                    level text level)
            *stahtml-file*))))))

(define-syntax yasweg-p
  (syntax-rules ()
    ((yasweg-p text)
     (if *stahtml-file*
         (begin
           (display
            (format #f "<p>~a</p>\n" text)
            *stahtml-file*))))))

(define-syntax yasweg-on-div
  (syntax-rules ()
    ((yasweg-on-div body ...)
     (if *stahtml-file*
         (begin
           (display "<div>\n" *stahtml-file*)
           body ...
           (display "</div>\n" *stahtml-file*))))))

(define-syntax yasweg-on-divclass
  (syntax-rules ()
    ((yasweg-on-divclass class body ...)
     (if *stahtml-file*
         (begin
           (display (format #f "<div class=\"~a\">\n"
                            class)
                    *stahtml-file*)
           body ...
           (display "</div>\n" *stahtml-file*))))))

;; ================== Basic inline formatting macros ==========



;; ===================== Bootstrap support ====================

(define-syntax yasweg-on-navbar
  (syntax-rules ()
    ((yasweg-on-navbar body ...)
     (yasweg-on-divclass
      "nav"
      body ...))))

(define-syntax yasweg-list
  (syntax-rules ()
    ((yasweg-list class list)
     (if *stahtml-file*
         (begin
           ;; Start list
           (display (format #f "<ul class=\"~a\">\n" class)
                    *stahtml-file*)
           ;; For each element in list, add said element.
           (map (λ (element)
                  ;; Begin list element
                  (display "<li>" *stahtml-file*)
                  (if (pair? element)
                      ;; If we're dealing with a pair, we can just
                      ;; toss it and its href
                      (display (format #f "<a href=\"~a\">~a</a>"
                                       (cdr element) (car element))
                               *stahtml-file*)
                      ;; Else we'll just toss nothing as href
                      (display (format #f "<a href=\"#\">~a</a>" element)
                               *stahtml-file*))
                  ;; End list element
                  (display "</li>\n" *stahtml-file*))
                list)
           ;; Close list
           (display "</ul>\n" *stahtml-file*))))))
                        



;; ==================== Debugging and testing =================


(define generate-test-page
  (λ ()
    (yasweg-on-page
     "test.html"
     (yasweg-on-head
      (yasweg-link-css "main.css"))
     (yasweg-on-body
      (yasweg-on-navbar
       (yasweg-on-divclass
        "container"
        (yasweg-list 'pull-left
                     '(("Back to Parent" . "#about")))
        (yasweg-list 'pull-right
                     '((About . about)))))
      (yasweg-on-div
       (yasweg-h 1 "My First Webpage")
       (yasweg-p "This webpage was generated using YASWEG."))
      )
     )))
