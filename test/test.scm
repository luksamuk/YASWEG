(use-modules ((yasweg generator)))

(yasweg-on-page
 "test/test.html"
 
 (yasweg-on-head
  (yasweg-set-metadata '((charset utf-8)
                         (title "YASWEG Static Page")
                         (author alchemist)
                         (description "Another static page generated with YASWEG.")))
  (yasweg-link-css "main.css"))
 
 (yasweg-on-body
  ;; Navigation
  (yasweg-on-navbar
   (yasweg-on-container
    (yasweg-ulist 'pull-left
                  '(("Back to Parent" . "#about")))
    (yasweg-ulist 'pull-right
                  '((About . about)))))

  ;; Big Ribbon
  (yasweg-on-ribbon
   (yasweg-h 1 "My First Webpage")
   (yasweg-p "This webpage was generated using YASWEG.")
   (yasweg-p (yasweg-inline-link "about:blank"
                                 "This is a test link that directs you to nothing."
                                 'blank))
   (yasweg-button 'outline-primary "Oh please press me"))

  (yasweg-on-divclass
   "basic-info"
   (yasweg-on-divclass
    "light-block"
    (yasweg-on-row
     (yasweg-on-col '((md 6))
                    (yasweg-h 3 "Light Block.")
                    (yasweg-p "Testing.")
                    (yasweg-newline)
                    (yasweg-button 'outline-primary "#" "centerobj" "Press me"))
     
     (yasweg-on-col '((md 6))
                    (yasweg-img 'centerobj "img/lena.jpg"))))
   
   (yasweg-on-divclass
    "bold-block"
    (yasweg-on-row
     (yasweg-on-col '((md 6))
                    (yasweg-img 'centerobj "img/lena.jpg"))
     (yasweg-on-col '((md 6))
                    (yasweg-h 3 "Bold block.")
                    (yasweg-p "Testing again.")
                    (yasweg-button 'outline-primary "Press me")))))

  ;; Footer
  (yasweg-on-footer
   (yasweg-p
    "text-muted"
    "Copyright blablabla"))))

(format #t "YASWEG TEST: Test file successfully compiled.\n")
