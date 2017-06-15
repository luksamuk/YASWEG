(use-modules ((yasweg generator) #:prefix yas:))

(define (generate-file file)
  (yas:on-page
   file
   
   (yas:on-head
    (yas:set-metadata '((charset utf-8)
                           (title "YASWEG Static Page")
                           (author alchemist)
                           (description "Another static page generated with YASWEG.")))
    (yas:link-css "main.css"))
   
   (yas:on-body
    ;; Navigation
    (yas:on-navbar
     (yas:on-container
      (yas:ulist 'pull-left
                    '(("Back to Parent" . "#about")))
      (yas:ulist 'pull-right
                    '((About . about)))))

    ;; Big Ribbon
    (yas:on-ribbon
     (yas:h 1 "My First Webpage")
     (yas:p "This webpage was generated using YASWEG.")
     (yas:p (yas:inline-link "about:blank"
                                   "This is a test link that directs you to nothing."
                                   'blank))
     (yas:button 'outline-primary "Oh please press me"))

    (yas:on-divclass
     "basic-info"
     (yas:on-divclass
      "light-block"
      (yas:on-row
       (yas:on-col '((md 6))
                      (yas:h 3 "Light Block.")
                      (yas:p "Testing.")
                      (yas:newline)
                      (yas:button 'outline-primary "#" "centerobj" "Press me"))
       
       (yas:on-col '((md 6) (xs 6))
                      (yas:img 'centerobj "img/lena.jpg"))))
     
     (yas:on-divclass
      "bold-block"
      (yas:on-row
       (yas:on-col '((md 6))
                      (yas:img 'centerobj "img/lena.jpg"))
       (yas:on-col '((md 6))
                      (yas:h 3 "Bold block.")
                      (yas:p "Testing again.")
                      (yas:button 'outline-primary "Press me")))))

    ;; Footer
    (yas:on-footer
     (yas:p
      "text-muted"
      "Copyright blablabla"))))

  (format #t "YASWEG TEST: File ~a successfully compiled.\n" file))

(generate-file "test/test.html")
