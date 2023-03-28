;;;; pseudo-incels-bot.lisp

(in-package #:pseudo-incels-bot)

; (ql:quickload :pseudo-incels-bot)

; (ql:quickload :chanl)

; (ql:quickload :conf)


(setf *discord-client* (wsd:make-client "wss://gateway.discord.gg/?v=10&encoding=json"))

(defvar application-id  (gethash :DISCORD-APP-ID *config*))

(defvar public-key  (gethash :DISCORD-PUBLIC-KEY *config*))

(defvar *discord-token*   (gethash :DISCORD-TOKEN *config*))

(defvar permisions "352724191280")

(defvar chat-gpt-endpoint "https://api.openai.com/v1/chat/completions")

(defvar *heartbeat-task*)

(defvar *open-ai-api-key* (gethash :gpt-token *config*))


(defvar *input-channel* (make-instance 'ch:bounded-channel  :size 100))

(defvar *output-channel*  (make-instance 'ch:bounded-channel  :size 100))


(defclass discord-message ()
     ((op-code
        :accessor op-code
        :initarg :op-code)
      (event-data
        :accessor event-data
        :initarg :event-data)
      (sequence-number
        :initform nil
        :accessor sequence-number
        :initarg :sequence-number)
      (event-name
        :initform nil
        :accessor event-name
        :initarg  :event-name)
      (message-trace

        :initform nil
        :accessor message-trace
        :initarg  :message-trace)))

(defun get-response-content (msg)
   (au:aget (au:aget (car (au:aget msg :choices )) :message ) :content))

(defvar mst-tmp
 '((:T) (:S) (:OP . 10)
   (:D (:HEARTBEAT--INTERVAL . 41250)
    (:--TRACE "[\"gateway-prd-us-east1-d-2ts5\",{\"micros\":0.0}]"))))

(defmethod initialize-instance :around ((obj discord-message) &key  raw-message)
  (if raw-message
   (let* ((message  (cl-json:decode-json-from-string raw-message))
          (op-code  (au:aget message :op))
          (event-data (au:aget message :d))
          (sequence-number (au:aget message :s))
          (event-name (au:aget message :t))
          (message-trace (au:aget message :--TRACE)))
     (call-next-method obj :op-code op-code :event-data event-data  :sequence-number sequence-number :event-name event-name :message-trace message-trace))
   (call-next-method)))

(defgeneric op-code-reaction* (msg channel code)
  (:documentation "Testing 'dynamic dispatch' "))


(defun op-code-reaction (msg channel)
  (op-code-reaction* msg channel (op-code msg)))


(defmethod op-code-reaction*  ((msg discord-message)  channel code)
  (format nil "dispatch with op-code ~a" code))

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

(defun output-message-loop (channel connection)
   (ch:pexec ()
    (loop
      (let ((message (ch:recv channel)))
        (print (format nil "output message: ~a" message))
        (wsd:send connection message)))))

(defun keep-alive (interval channel)
 (let ((milis (* 1000 interval))
       (heartbeat (json:encode-json-alist-to-string  (pairlis  '(:op  :d) '(1 251)))))
  (loop (sleep interval)
        (print (format nil "after ~d miliseconds have passed, you are relieved to hear 'lub-dub, lub-dub'" milis))
        (ch:send channel heartbeat))))

(defmethod op-code-reaction*  ((msg discord-message)  channel (code (eql 10)))
  (let ((interval (/ (au:aget (event-data msg) :heartbeat--interval) 1000)))
   (setf *heartbeat* (ch:pexec () (keep-alive interval channel)))))

(defun make-on-message (channel)
  (lambda (raw-msg)
    (print (format nil "Recieved message: ~a" raw-msg))
    (let ((msg (make-instance 'discord-message :raw-message raw-msg)))
     (op-code-reaction msg channel))))

(defvar *output-loop-task*)


(defun connect->discord ()
 (let* ((discord-client (wsd:make-client "wss://gateway.discord.gg/?v=10&encoding=json"))
        (on-message (make-on-message *output-channel*)))
  (setf *output-loop-task* (output-message-loop *output-channel* discord-client))
  (wsd:on :message discord-client on-message)
  (wsd:start-connection discord-client)))

(defvar *bot-identification-data*
 (let (( properties '(("os" . "linux") ("browser" . "common-lisp") ("device" . "common-lisp"))))
  (pairlis '(:op :d) (list 2 (pairlis  '(:token :intents :properties)
                                        (list *discord-token*  32256 properties))))))




;
;(defparameter aliluia (connect->discord))
;  (json:encode-json-alist-to-string))
;  2  	Identify  	Send  	Starts a new session during the initial handshake.)

