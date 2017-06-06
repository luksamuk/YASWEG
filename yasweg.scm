(define-module (yasweg generator))
(export yasweg-open
        yasweg-close
        yasweg-on-page
        yasweg-link-css
        yasweg-on-head
        yasweg-on-body
        yasweg-h
        yasweg-p
        yasweg-on-div
        yasweg-on-divclass
        yasweg-img
        yasweg-newline
        yasweg-set-metadata
        yasweg-on-navbar
        yasweg-ulist
        yasweg-on-container
        yasweg-on-ribbon
        yasweg-on-footer
        yasweg-button)

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
     (yasweg-p #f text))
    ((yasweg-p class text)
     (if *stahtml-file*
         (begin
           (display
            (if class
                (format #f "<p class=\"~a\">~a</p>\n"
                        class text)
                (format #f "<p>~a</p>\n" text))
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

(define-syntax yasweg-img
  (syntax-rules ()
    ((yasweg-img img-src)
     (if *stahtml-file*
         (display (format #f "<img src=\"~a\"/>\n"
                          img-src)
                  *stahtml-file*)))
    ((yasweg-img class img-src)
     (if *stahtml-file*
         (display (format #f "<img class=\"~a\" src=\"~a\"/>"
                          class img-src)
                  *stahtml-file*)))))

(define-syntax yasweg-newline
  (syntax-rules ()
    ((yasweg-newline)
     (if *stahtml-file* (display "<br/>\n" *stahtml-file*)))))
         

;; ================== Basic inline formatting macros ==========

;; TO-DO

;; ================== Metadata macros =========================

(define yasweg-set-metadata
  (λ (metadata-list)
    (if *stahtml-file*
        (map (λ (item)
               (if (list? item)
                   (cond
                    ;; Define charset
                    ((eq? (car item) 'charset)
                     (display (format #f "<meta charset=\"~a\">\n"
                                      (cadr item))
                              *stahtml-file*))
                    ;; Define author
                    ((eq? (car item) 'author)
                     (display (format #f "<meta name=\"author\" content=\"~a\">\n"
                                      (cadr item))
                              *stahtml-file*))
                    ;; Define description
                    ((eq? (car item) 'description)
                     (display (format #f "<meta name=\"description\" content=\"~a\">\n"
                                      (cadr item))
                              *stahtml-file*))
                    ;; Define title
                    ((eq? (car item) 'title)
                     (display (format #f "<title>~a</title>\n"
                                      (cadr item))
                              *stahtml-file*)))))
             metadata-list))))


;; ===================== Bootstrap support ====================

(define-syntax yasweg-on-navbar
  (syntax-rules ()
    ((yasweg-on-navbar body ...)
     (yasweg-on-divclass
      "nav"
      body ...))))

(define-syntax yasweg-ulist
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

(define-syntax yasweg-on-container
  (syntax-rules ()
    ((yasweg-on-container body ...)
     (yasweg-on-divclass "container" body ...))))

(define-syntax yasweg-on-ribbon
  (syntax-rules ()
    ((yasweg-on-jumbotron body ...)
     (yasweg-on-divclass
      "jumbotron"
      (yasweg-on-container
       body ...)))))

(define-syntax yasweg-on-footer
  (syntax-rules ()
    ((yasweg-on-footer body ...)
     (yasweg-on-divclass
      "panel-footer"
      (yasweg-on-container
       body ...)))))

(define-syntax yasweg-button
  (syntax-rules ()
    ((yasweg-button type text)
     (yasweg-button type "" "" text))
    ((yasweg-button type ref text)
     (yasweg-button type ref "" text))
    ((yasweg-button type ref class text)
     (if *stahtml-file*
         (begin
           (display (format #f "<a href=\"~a\" type=\"button\" class=\"~a\">~a</a>\n"
                            ;; 1. href
                            ref
                            ;; 2. class (and type)
                            (format #f "~a~a"
                                    (if (not (string=? class ""))
                                        (format #f "~a " class)
                                        "")
                                    (cond
                                     ((eq? type 'outline-primary) "btn-outline-primary")
                                     ((eq? type 'outline-secondary) "btn-outline-secondary")
                                     ((eq? type 'primary) "btn-primary")
                                     ((eq? type 'secondary) "btn-secondary")
                                     (#t "btn-primary")))
                            ;; 3. text
                            text)
                    *stahtml-file*))))))
