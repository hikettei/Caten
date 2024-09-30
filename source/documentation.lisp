(in-package :caten-user)

(define-page ("Home" "index.md")
  (title "Home")
  (body "Welcome to Caten docs")
  )

(define-page ("Quickstart" "quickstart.md")
  (title "QuickStart")
  (subtitle "Install")
  (body "(ql:quickload :caten)")
  )

(define-page ("Development" "development.md")
  (title "Development"))
