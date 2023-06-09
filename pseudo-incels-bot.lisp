;;;; pseudo-incels-bot.lisp
(in-package #:pseudo-incels-bot)
;  (ql:quickload :pseudo-incels-bot))

; (ql:quickload :log4cl.log4slime)

(defgeneric event-action (event-data event-hash))

(defgeneric op-code-reaction* (msg channel code))

(defvar  *html-response* '())

(defvar *html-stream-in*  (make-instance 'ch:bounded-channel  :size 10))

(defvar *discord-client*)

(defvar *discord-session-id* "")

(defvar *discord-base-url* "https://discord.com/api/v10")

(defvar *command-channel* (make-instance 'ch:bounded-channel  :size 100))

(defvar *discord-application-id*  (gethash :DISCORD-APP-ID *config*))

(defvar public-key  (gethash :DISCORD-PUBLIC-KEY *config*))

(defvar *discord-token*   (gethash :DISCORD-TOKEN *config*))

(defvar *bot-identification-data*
       (let (( properties '( ("os" . "linux") ("browser" . "common-lisp") ("device" . "common-lisp"))))
        (json:encode-json-alist-to-string  (pairlis '(:op :d) (list 2 (pairlis  '(:token :intents :properties)  (list *DISCORD-TOKEN* 32256  properties)))))))

(defvar *guild-id* (gethash :guild-id *config*))

(defvar permisions "352724191280")

(defvar chat-gpt-endpoint "https://api.openai.com/v1/chat/completions")

(defvar *heartbeat-task*)

(defvar *output-loop-task*)

(defvar *open-ai-api-key* (gethash :gpt-token *config*))

(defvar *gateway-channel*  (make-instance 'ch:bounded-channel  :size 100))

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

(defmethod op-code-reaction*  ((msg discord-message)  channel (code (eql 0)))
  (let ((event-hash  (SXHASH  (event-name   msg)))
        (data (event-data msg)))
    (log:info  "Dispatching event ~a with hash ~a" (event-name   msg) event-hash)
    (event-action data  event-hash)))

(defvar *discord-client*)

(defvar *discord-base-url* "https://discord.com/api/v10")

(defvar *command-channel* (make-instance 'ch:bounded-channel  :size 100))

(defvar *discord-application-id*  (gethash :DISCORD-APP-ID *config*))

(defvar public-key  (gethash :DISCORD-PUBLIC-KEY *config*))

(defvar *discord-token*   (gethash :DISCORD-TOKEN *config*))

(defparameter *bot-headers* (pairlis  '("Authorization" "Content-Type" ) (list (format nil "Bot ~a" *discord-token*)  "application/json")))

(defvar *guild-id* (gethash :guild-id *config*))

(defvar permisions "352724191280")

(defvar chat-gpt-endpoint "https://api.openai.com/v1/chat/completions")

(defvar *heartbeat-task*)

(defvar *open-ai-api-key* (gethash :gpt-token *config*))

(defvar *gateway-channel*  (make-instance 'ch:bounded-channel  :size 100))

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
  (log:info  "dispatch with op-code ~a" code))

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
  (dex:post  chat-gpt-endpoint
    :headers (list (cons "Authorization"  (format nil "Bearer ~a" *open-ai-api-key*)) (cons "Content-Type"  "application/json"))
    :content  (davinci-message message)
    :verbose t)))

(defun user-id->channel (user-id)
 (cl-json:decode-json-from-string
  (dex:post  (format nil  "~a~a" *discord-base-url* "/users/@me/channels")
    :headers *bot-headers*
    :content  (json:encode-json-alist-to-string (list (cons :recipient_id user-id)))
    :verbose t)))

(defun post->channel (channel-id message)
  (log:info channel-id)
  (dex:post
    (format nil "~a/channels/~a/messages"  *discord-base-url*   channel-id)
    :headers *bot-headers*
    :content (json:encode-json-alist-to-string (list (cons :content  message) (cons :tts "false")))))

(defmethod event-action ((event-data list ) (event-hash (eql 668586304912467256)))
  (loop for i in (au:aget event-data :mentions)
        do
        (when (string= "PseudoAndrewTate"  (au:aget i :username))
          (log:info "Incel alert")
          (let  ((content (PPCRE:regex-replace "\(\<@\\d*\>\\s*\)*" (au:aget event-data :content) ""))
                 (author-id (au:aget (au:aget event-data :author) :id)))
            (ch:pexec ()
             (handler-case
              (let* ((dm-channel-data (user-id->channel  author-id))
                     (dm-channel-id (au:aget dm-channel-data :id))
                     (response (send-davinci-message content))
                     (narrative (get-response-content response)))
                (loop for i from 0 to (length narrative) by 1992
                  do (post->channel dm-channel-id  (str:substring  i  (+ 1992 i) narrative))))
              (error (c)
               (log:info "error when trying to communicate with CHATGPT: ~a" c))))

            (loop-finish)))))

(defmethod event-action ((event-data list) (event-hash (eql 2229288255679708455))) ;;"READY"
  (setf *discord-session-id* (au:aget event-data :session--id)))

(defmethod event-action ((event-data list ) event-hash) ;; General
  (log:info "Event data ~a" event-data))

(defun get-response-content (msg)
   (au:aget (au:aget (car (au:aget msg :choices )) :message ) :content))

(defun output-message-loop (channel connection)
  (setf *heartbeat-task* (ch:pexec ()
                          (loop
                            (let ((message (ch:recv channel)))
                              (log:info "output message: ~a" message)
                              (wsd:send connection message))))))

(defun keep-alive (interval channel)
 (let ((milis (* 1000 interval))
       (heartbeat (json:encode-json-alist-to-string  (pairlis  '(:op  :d) '(1 251)))))
  (loop (sleep interval)
        ; (log:info (format nil "after ~d miliseconds have passed, you check for pulse" milis))
        (ch:send channel heartbeat))))

(defmethod op-code-reaction*  ((msg discord-message)  channel (code (eql 10)))
  (let ((interval (/ (au:aget (event-data msg) :heartbeat--interval) 1000)))
   (setf *heartbeat* (ch:pexec ()
                       (keep-alive interval channel)))))

(defvar *identification-state* 0)

(defmethod op-code-reaction*  ((msg discord-message)  channel (code (eql 11)))
  (log:info "operation code 11, you are relieved to hear 'lub-dub, lub-dub'")
  (when (= 0 *identification-state*)
   (ch:send channel *bot-identification-data*)
   (log:info "just for once, we identify as they/them")
   (setf *identification-state* 1)))

(defun make-on-message (channel)
  (lambda (raw-msg)
    (let ((msg (make-instance 'discord-message :raw-message raw-msg)))
     (log:info "Recieved message with op-code ~a" (op-code msg))
     (log:info "Raw message: ~a" raw-msg)
     (op-code-reaction msg channel))))

(defun connect->discord ()
 (let* ((discord-client (wsd:make-client "wss://gateway.discord.gg/?v=10&encoding=json"))
        (on-message (make-on-message *gateway-channel*)))
  (setf *discord-client* discord-client)
  (setf *output-loop-task* (output-message-loop *gateway-channel* discord-client))
  (wsd:on :message discord-client on-message)
  (wsd:start-connection discord-client)))

(defun reconnect ()
  (handler-case  (ch:kill (ch:task-thread *heartbeat-task*))
   (error (c)
     (log:error  "Error closing *heartbeat-task* ~a" c)))
  (handler-case (wsd:close-connection *discord-client*)
   (error (c)
     (log:error "Error closing *heartbeat-task* : ~a" c)))
  (connect->discord))

(defmethod op-code-reaction*  ((msg discord-message) channel (code (eql 7)))
 (reconnect)
 (log:info "Reconnecting"))


(defvar  *program-loop*  nil)

(defun main ()
  (ch:pexec ()
    (progn
      (connect->discord)
      (loop
       (sleep 900)
       (reconnect)))))

