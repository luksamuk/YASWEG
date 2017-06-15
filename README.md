YASWEG - Yet Another Static Website Generator
=============================================

There is nothing serious NOR anything to be maintained here.
I do not intend to take this to the ends of the world. We good? Good.

YASWEG is a static website generator, as the title says.
It implements a simple Scheme API foi doing so, and is designed to run on GNU Guile.
YASWEG will then write a static HTML file from the information provided.
The goal is to build such simple thing, for study purposes.

By using YASWEG, you can generate an HTML page like

```html
<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="bootstrap.css">
        <link rel="stylesheet" href="main.css">
    </head>

    <body>
        <h1>My Webpage</h1>
        <p>This is a webpage.</p>
        <br/>
        <p>Cool, isn't it?</p>
    </body>
</html>
```

By using such simple lisp-y syntax:

```scheme
(use-modules ((yasweg generator) #:prefix yas:))

(yas:on-page "test.html"
  (yas:on-head
    (yas:link-css "main.css"))
  (yas:on-body
    (yas:h 1 "My Webpage")
    (yas:p "This is a webpage.")
    (yas:newline)
    (yas:p "Cool, isn't it?")))
```

Usage
-----
YASWEG is built atop GNU Guile, so you might as well just compile YASWEG once, then import the `(yasweg generator)` module, and you're good to go.

Inside is a single test example, being used to help create the project.

LICENSE
-------

The LGPL v3 License. See LICENSE for details.

Copyright 2017 Lucas Vieira.

