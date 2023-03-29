;;;; pseudo-incels-bot.lisp

(in-package #:pseudo-incels-bot)

; (ql:quickload :CL-PPCRE)
; (ql:quickload :pseudo-incels-bot)

; (ql:quickload :chanl)

; (ql:quickload :conf)
                        ; (wsd:make-client "wss://gateway.discord.gg/?v=10&encoding=json"))

(defgeneric event-action (event-data event-hash))

(defgeneric op-code-reaction* (msg channel code))

(defvar  *html-response* '())

(defvar *html-stream-in*  (make-instance 'ch:bounded-channel  :size 10))

(defvar *discord-client*)

(defvar *discord-base-url* "https://discord.com/api/v10")

(defvar *command-channel* (make-instance 'ch:bounded-channel  :size 100))

(defvar *discord-application-id*  (gethash :DISCORD-APP-ID *config*))

(defvar public-key  (gethash :DISCORD-PUBLIC-KEY *config*))

(defvar *discord-token*   (gethash :DISCORD-TOKEN *config*))

(defvar *bot-identification-data*
       (let (( properties '( ("os" . "linux") ("browser" . "common-lisp") ("device" . "common-lisp"))))
        (json:encode-json-alist-to-string  (pairlis '(:op :d) (list 2 (pairlis  '(:token :intents :properties)  (list *DISCORD-TOKEN* 32256  properties)))))))

(print *bot-identification-data*)

(defvar *guild-id* (gethash :guild-id *config*))

(defvar permisions "352724191280")

(defvar chat-gpt-endpoint "https://api.openai.com/v1/chat/completions")

(defvar *heartbeat-task*)

; (ch:kill (slot-value  *heartbeat* 'CH::THREAD))

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
      (event-action data  event-hash)))

; (defgeneric  discord-event  num
;   (:documentation "Testing 'dynamic dispatch' "))

; (defmethod op-code-reaction*  ((msg discord-message)  channel (code (eql 10))))


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
  (print (format nil "dispatch with op-code ~a" code)))


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

; (print (send-davinci-message "tell me a story for little children with dragons"))



; In order to open and send a direct message to a user, you need these endpoints.
;
; For creating a new direct message
;
; POST /users/@me/channels
;
; For sending messages:
;
; POST /channels/{channel.id}/messages

(defun user-id->channel (user-id)
 (cl-json:decode-json-from-string
  (dex:post  (format nil  "~a~a" *discord-base-url* "/users/@me/channels")
    :headers *bot-headers*
    :content  (json:encode-json-alist-to-string (list (cons :recipient_id user-id)))
    :verbose t)))

(defun post->channel (channel-id message)
  (print channel-id)
  (dex:post
    (format nil "~a/channels/~a/messages"  *discord-base-url*   channel-id)
    :headers *bot-headers*
    :content (json:encode-json-alist-to-string (list (cons :content  message) (cons :tts "false")))))


(defmethod event-action ((event-data list ) (event-hash (eql 668586304912467256)))
  (loop for i in (au:aget event-data :mentions)
        do
        (when (string= "PseudoAndrewTate"  (au:aget i :username))
          (let  ((content (PPCRE:regex-replace "\(\<@\\d*\>\\s*\)*" (au:aget event-data :content) ""))
                 (author-id (au:aget (au:aget event-data :author) :id)))
            (ch:pexec ()
             (let* ((dm-channel-data (user-id->channel  author-id))
                    (dm-channel-id (au:aget dm-channel-data :id))
                    (response (send-davinci-message content))
                    (narrative (get-response-content response)))
               ; (setf -d dm-channel-id)
               (setf *html-response* narrative)
               (post->channel dm-channel-id  narrative)))
            (loop-finish)))))

(print *html-response*)
(defmethod event-action ((event-data list ) event-hash)
  (print event-hash))

(defvar narrative
  "Once upon a time, in a far-off land, there was a dragon named Taragon who had a peculiar craving for power. Despite his immense strength, he yearned for even more power to rule over the kingdom as he saw fit.

One day, while flying over a deserted mountain peak, Taragon stumbled upon a tiny bottle with a genie trapped inside. The genie promised Taragon three wishes in exchange for setting him free.

With no hesitation, Taragon demanded his first wish - he wanted to be the most powerful dragon in the land. The genie granted his wish, and Taragon immediately felt an unparalleled power surging through his veins.

But as days passed, Taragon's thirst for power only grew stronger, and he wished for more - he wanted to be invincible and immortal. The genie, bound by the rules of the granting of wishes, fulfilled his desires.

As time went by, Taragon's unquenchable thirst for power left him with no allies or friends. He had become feared and ostracized by everyone in the kingdom. Even his fellow dragons were wary of his ambitions.

Finally, one day, Taragon's loneliness and despair grew so great that he knew he had made a grave mistake in seeking ultimate power. Wisely, he used his last wish to undo the previous two and returned to his former dragon self.

Thanks to the genie, Taragon learned a valuable lesson - that true power lies not in dominance, but in the strength of companionship and friendship.

And from that day on, Taragon flew through the skies of the kingdom with newfound wisdom and humility, spreading his message of love and friendship to all who would listen.")



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
        (print (format nil "after ~d miliseconds have passed, you check for pulse" milis))
        (ch:send channel heartbeat))))

(defmethod op-code-reaction*  ((msg discord-message)  channel (code (eql 10)))
  (let ((interval (/ (au:aget (event-data msg) :heartbeat--interval) 1000)))
   (setf *heartbeat* (ch:pexec ()
                       (keep-alive interval channel)))))

(defvar *identification-state* 0)

(defmethod op-code-reaction*  ((msg discord-message)  channel (code (eql 11)))
  (print "operation code 11, you are relieved to hear 'lub-dub, lub-dub'")
  (when (= 0 *identification-state*)
   (ch:send channel *bot-identification-data*)
   (print "just for once, we identify as they/them")
   (setf *identification-state* 1)))

(defun make-on-message (channel)
  (lambda (raw-msg)
    (print (format nil "Recieved message: ~a" raw-msg))
    (let ((msg (make-instance 'discord-message :raw-message raw-msg)))
     (op-code-reaction msg channel))))

(defvar *output-loop-task*)


(defun connect->discord ()
 (let* ((discord-client (wsd:make-client "wss://gateway.discord.gg/?v=10&encoding=json"))
        (on-message (make-on-message *gateway-channel*)))
  (setf *discord-client* discord-client)
  (setf *output-loop-task* (output-message-loop *gateway-channel* discord-client))
  (wsd:on :message discord-client on-message)
  (wsd:start-connection discord-client)))


(defvar *bot-opt*
  (let (( properties '(("os" . "linux") ("browser" . "common-lisp") ("device" . "common-lisp"))))
    (json:encode-json-alist-to-string
      (pairlis '(:op :d)
               (list 8 (pairlis
                         '(:guild_id :query :limit)
                         (list "41771983444115456" "" 0)))))))

(defparameter commands (format nil "~a/applications/~a/commands" *discord-base-url* *discord-application-id*))

(defparameter channels  (format nil "~a/guilds/~a/channels"  *discord-base-url*   *guild-id*))


(defvar chan-id  1088787295135617085)


(defparameter channel-post (format nil "~a/channels/~a/messages"  *discord-base-url*   chan-id))


; (defun encode-discord-command (command)
;   (json:encode-json-to-string
;    (dict
;      :model *openai-model*
;      :messages (concatenate 'list
;                              (list (dict :role "user" :content  message))
;                              (make-sys system-message)
;                              (merge-assistants assistant-messages)))))



; Get Guild Channels
; GET/guilds/{guild.id}/channels
;
; Create Global Application Command
; POST/applications/{application.id}/commands



; (print (dex:post   channel-post :headers headers :content (json:encode-json-alist-to-string (list (cons :content "Πάρτε και μια πίπα" ) (cons :tts "false")))))
;  /channels/{channel.id}/messages)
; (loop for i in (cl-json:decode-json-from-string "[{\"id\": \"1088784698823688243\", \"type\": 4, \"name\": \"Text Channels\", \"position\": 0, \"flags\": 0, \"parent_id\": null, \"guild_id\": \"1088784698119036991\", \"permission_overwrites\": []}, {\"id\": \"1088784698823688244\", \"type\": 4, \"name\": \"Voice Channels\", \"position\": 0, \"flags\": 0, \"parent_id\": null, \"guild_id\": \"1088784698119036991\", \"permission_overwrites\": []}, {\"id\": \"1088784698823688245\", \"last_message_id\": \"1090343266634977280\", \"type\": 0, \"name\": \"general\", \"position\": 0, \"flags\": 0, \"parent_id\": \"1088784698823688243\", \"topic\": null, \"guild_id\": \"1088784698119036991\", \"permission_overwrites\": [], \"rate_limit_per_user\": 0, \"nsfw\": false}, {\"id\": \"1088784698823688246\", \"last_message_id\": null, \"type\": 2, \"name\": \"General\", \"position\": 0, \"flags\": 0, \"parent_id\": \"1088784698823688244\", \"bitrate\": 64000, \"user_limit\": 0, \"rtc_region\": null, \"guild_id\": \"1088784698119036991\", \"permission_overwrites\": [], \"rate_limit_per_user\": 0, \"nsfw\": false}, {\"id\": \"1088787295135617085\", \"last_message_id\": \"1088787351184089089\", \"type\": 0, \"name\": \"chatgpt\", \"position\": 1, \"flags\": 0, \"parent_id\": null, \"topic\": null, \"guild_id\": \"1088784698119036991\", \"permission_overwrites\": [], \"rate_limit_per_user\": 0, \"nsfw\": false}]")
;   do (when (string= (au:aget i :name)  "chatgpt")
;        (return (parse-integer (au:aget i :id)))))
;
; (defun commnad-loop (channel connection)
;    (ch:pexec ()
;     (loop
;       (let ((command (ch:recv channel)))
;         (print (format nil "sending command ~a" command))
;         (dex:post  (format nil "https://discord.com/api/v10/applications/~a/commands") ; ;
;           :headers  (pairlis  '("Authorization" "Content-Type" ) (list (format nil "Bot ~a" *discord-token*)  "application/json"))
;           :content  (davinci-message message)
;           :verbose t)))))
;(defparameter savarakatranemia (connect->discord))



; (defparameter event-msg (json:decode-json-from-string  "{\"t\":\"MESSAGE_CREATE\",\"s\":7,\"op\":0,\"d\":{\"type\":0,\"tts\":false,\"timestamp\":\"2023-03-29T11:16:05.911000+00:00\",\"referenced_message\":null,\"pinned\":false,\"nonce\":\"1090595233214758912\",\"mentions\":[],\"mention_roles\":[],\"mention_everyone\":false,\"member\":{\"roles\":[],\"premium_since\":null,\"pending\":false,\"nick\":null,\"mute\":false,\"joined_at\":\"2023-03-24T11:21:40.780000+00:00\",\"flags\":0,\"deaf\":false,\"communication_disabled_until\":null,\"avatar\":null},\"id\":\"1090595233353449574\",\"flags\":0,\"embeds\":[],\"edited_timestamp\":null,\"content\":\"\",\"components\":[],\"channel_id\":\"1088787295135617085\",\"author\":{\"username\":\"heefoo\",\"public_flags\":0,\"id\":\"579677178136887315\",\"global_name\":null,\"display_name\":null,\"discriminator\":\"4417\",\"avatar_decoration\":null,\"avatar\":\"13509c95c14038d610c82e9be42e8c1c\"},\"attachments\":[],\"guild_id\":\"1088784698119036991\"}}"))

; (loop for i in (au:aget poutsa :mentions)
;       do  (print (au:aget i :username)))
;


; (print (car (au:aget poutsa :mentions)))

; (defvar content (au:aget poutsa :content))

; (ppcre:scan-to-strings "\<@[^\>]*\>"  content)
; (print content)

;  (ppcre:scan-to-strings "[^b]*b" "aaabd"))

; (PPCRE:regex-replace "\@[1-9]*" content "frob")
; (defvar poutsa
;       '((:TYPE . 0) (:TTS) (:TIMESTAMP . "2023-03-29T19:41:52.209000+00:00")
;         (:REFERENCED--MESSAGE) (:PINNED) (:NONCE . "1090722514729959424")
;         (:MENTIONS
;          ((:USERNAME . "PseudoAndrewTate") (:PUBLIC--FLAGS . 0)
;           (:MEMBER (:ROLES "1090343266228117646") (:PREMIUM--SINCE) (:PENDING) (:NICK)
;            (:MUTE) (:JOINED--AT . "2023-03-28T18:34:52.305013+00:00") (:FLAGS . 0)
;            (:DEAF) (:COMMUNICATION--DISABLED--UNTIL) (:AVATAR))
;           (:ID . "1089180261163470870") (:GLOBAL--NAME) (:DISPLAY--NAME)
;           (:DISCRIMINATOR . "1873") (:BOT . T) (:AVATAR--DECORATION) (:AVATAR))
;          ((:USERNAME . "heefoo") (:PUBLIC--FLAGS . 0)
;           (:MEMBER (:ROLES) (:PREMIUM--SINCE) (:PENDING) (:NICK) (:MUTE)
;            (:JOINED--AT . "2023-03-24T11:21:40.780000+00:00") (:FLAGS . 0) (:DEAF)
;            (:COMMUNICATION--DISABLED--UNTIL) (:AVATAR))
;           (:ID . "579677178136887315") (:GLOBAL--NAME) (:DISPLAY--NAME)
;           (:DISCRIMINATOR . "4417") (:AVATAR--DECORATION)
;           (:AVATAR . "13509c95c14038d610c82e9be42e8c1c")))
;         (:MENTION--ROLES) (:MENTION--EVERYONE)
;         (:MEMBER (:ROLES) (:PREMIUM--SINCE) (:PENDING) (:NICK) (:MUTE)
;          (:JOINED--AT . "2023-03-24T11:21:40.780000+00:00") (:FLAGS . 0) (:DEAF)
;          (:COMMUNICATION--DISABLED--UNTIL) (:AVATAR))
;         (:ID . "1090722514952540353") (:FLAGS . 0) (:EMBEDS) (:EDITED--TIMESTAMP)
;         (:CONTENT . "<@1089180261163470870>  <@579677178136887315>  parte poutso")
;         (:COMPONENTS) (:CHANNEL--ID . "1088787295135617085")
;         (:AUTHOR (:USERNAME . "heefoo") (:PUBLIC--FLAGS . 0)
;          (:ID . "579677178136887315") (:GLOBAL--NAME) (:DISPLAY--NAME)
;          (:DISCRIMINATOR . "4417") (:AVATAR--DECORATION)
;          (:AVATAR . "13509c95c14038d610c82e9be42e8c1c"))
;         (:ATTACHMENTS) (:GUILD--ID . "1088784698119036991")))
;


; ; ( 579677178136887315)
;
;
; (defvar pipa'((:ID . "1090359362993803295") (:TYPE . 1)
;               (:LAST--MESSAGE--ID . "1090591299557675069") (:FLAGS . 0)
;               (:RECIPIENTS
;                ((:ID . "579677178136887315") (:USERNAME . "heefoo") (:GLOBAL--NAME)
;                 (:DISPLAY--NAME) (:AVATAR . "13509c95c14038d610c82e9be42e8c1c")
;                 (:AVATAR--DECORATION) (:DISCRIMINATOR . "4417") (:PUBLIC--FLAGS . 0)))))

