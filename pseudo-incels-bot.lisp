;;;; pseudo-incels-bot.lisp

(in-package #:pseudo-incels-bot)


; (ql:quickload :pseudo-incels-bot)

; (ql:quickload :chanl)

;(ql:quickload :conf)
(hash-keys *config*)


(setf *discord-client* (wsd:make-client "wss://gateway.discord.gg/?v=10&encoding=json"))

(defvar  APPLICATION-ID  (gethash :DISCORD-APP-ID *config*))

(defvar PUBLIC-KEY  (gethash :DISCORD-PUBLIC-KEY *config*))

(defvar token   (gethash :DISCORD-TOKEN *config*))

(defvar permisions "352724191280")

(defvar chat-gpt-endpoint "https://api.openai.com/v1/chat/completions")

(defvar *heartbeat-task*)

(defvar *open-ai-api-key* (gethash :gpt-token *config*))

(defclass discord-msg  ()
     ((op-code
        :accessor op-code
        :initarg :op-code)
      (event-data
        :accessor event-data
        :initarg :event-data)
      (sequence-number
        :accessor sequence-number
        :initarg :sequence-number)
      (event-name
        :accessor event-name
        :initarg  event-name)))

(defun make-sys (s)
  (when s
    (list (dict :role  :system  :content (car s)))))

(defun merge-assistants (s)
  (when s
   (map 'list #'(LAMBDA (x) (dict :role  :assistant :content x))  s)))

(defvar *openai-model* "gpt-3.5-turbo")

(defun davinci-message (message &key (number-of-words 200) (system-message '()) (assistant-messages '()))
  (json:encode-json-to-string
   (dict
     :model *openai-model*
     :messages (concatenate 'list
                             (list (dict :role "user" :content  message))
                             (make-sys system-message)
                             (merge-assistants assistant-messages)))))


(defun send-davinci-message (message)
 (cl-json:decode-json-from-string
  (dex:post  chat-gpt-endpoint     ; ;
    :headers (list (cons "Authorization"  (format nil "Bearer ~a" *open-ai-api-key*)) (cons "Content-Type"  "application/json"))
    :content  (davinci-message message)
    :verbose t)))

(defvar  *html-response* '())

(defvar *html-stream-in*  (make-instance 'ch:bounded-channel  :size 10))



(defun get-response-content (msg)
   (au:aget (au:aget (car (au:aget msg :choices )) :message ) :content))


(defvar *heartbeat* (json:encode-json-alist-to-string (acons :op  1 (acons :d  251 '()))))


(defun keep-alive (interval connection)
 (let ((milis (* 1000 interval)))
  (loop (sleep interval)
        (print (format nil "after ~d miliseconds have passed, you are relieved to hear 'lub-dub, lub-dub'" milis))
        (print (wsd:send connection *heartbeat*)))))

(defvar mst-tmp
 '((:T) (:S) (:OP . 10)
   (:D (:HEARTBEAT--INTERVAL . 41250)
    (:--TRACE "[\"gateway-prd-us-east1-d-2ts5\",{\"micros\":0.0}]"))))




(defun get-heartbeat--interval  (msg)
 (au:aget (au:aget msg :d) :heartbeat--interval))

(defun make-on-message (connection)
  (lambda (msg-)
    (let* ((msg (cl-json:decode-json-from-string msg-))
           (op-code (au:aget msg :op)))
      (case op-code
        (10  (let ((heartbeat (/ (get-heartbeat--interval msg) 1000)))
               (setf *heartbeat-task*  (ch:pexec () (keep-alive heartbeat connection)))))
        (print msg)))))

(defun connect->discord ()
 (let* ((discord-client (wsd:make-client "wss://gateway.discord.gg/?v=10&encoding=json"))
        (on-message (make-on-message discord-client)))
  (wsd:on :message discord-client on-message)
  (wsd:start-connection discord-client)))


;(defparameter kourafeksala (connect->discord ))
