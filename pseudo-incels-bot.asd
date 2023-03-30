;;;; pseudo-incels-bot.asd

(asdf:defsystem #:pseudo-incels-bot
  :description "Describe pseudo-incels-bot here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on   (#:alexandria #:uiop #:iterate #:trivia
                 #:dexador #:cl-json #:serapeum #:bordeaux-threads
                 #:chanl #:clack #:websocket-driver #:websocket-driver-client
                 #:assoc-utils #:conf  #:CL-PPCRE #:str #:log4cl)

  :components ((:file "package")
               (:file "pseudo-incels-bot")))
