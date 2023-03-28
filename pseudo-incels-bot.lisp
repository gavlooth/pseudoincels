;;;; pseudo-incels-bot.lisp

(in-package #:pseudo-incels-bot)

; (ql:quickload :pseudo-incels-bot)

; (ql:quickload :chanl)

; (ql:quickload :conf)
                        ; (wsd:make-client "wss://gateway.discord.gg/?v=10&encoding=json"))


(defvar *discord-client*)

(defvar application-id  (gethash :DISCORD-APP-ID *config*))

(defvar public-key  (gethash :DISCORD-PUBLIC-KEY *config*))

(defvar *discord-token*   (gethash :DISCORD-TOKEN *config*))

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
        (on-message (make-on-message *gateway-channel*)))
  (setf *discord-client* discord-client)
  (setf *output-loop-task* (output-message-loop *gateway-channel* discord-client))
  (wsd:on :message discord-client on-message)
  (wsd:start-connection discord-client)))

(defvar *bot-identification-data*
       (let (( properties '( ("$os" . "linux") ("$browser" . "common-lisp") ("$device" . "common-lisp"))))
        (json:encode-json-alist-to-string  (pairlis '(:op :d) (list 2 (pairlis  '(:token :intents :properties)  (list *DISCORD-TOKEN* 32256  properties)))))))

(defvar *bot-opt*
  (let (( properties '(("os" . "linux") ("browser" . "common-lisp") ("device" . "common-lisp"))))
    (json:encode-json-alist-to-string
      (pairlis '(:op :d)
               (list 8 (pairlis
                         '(:guild_id :query :limit)
                         (list "41771983444115456" "" 0)))))))



(cl-json:decode-json-from-string "{\"t\":\"READY\",\"s\":1,\"op\":0,\"d\":{\"v\":10,\"user_settings\":{},\"user\":{\"verified\":true,\"username\":\"PseudoAndrewTate\",\"mfa_enabled\":false,\"id\":\"1089180261163470870\",\"global_name\":null,\"flags\":0,\"email\":null,\"display_name\":null,\"discriminator\":\"1873\",\"bot\":true,\"avatar\":null},\"session_type\":\"normal\",\"session_id\":\"73d6704c4725675627e6d790218d2603\",\"resume_gateway_url\":\"wss://gateway-us-east1-d.discord.gg\",\"relationships\":[],\"private_channels\":[],\"presences\":[],\"guilds\":[{\"unavailable\":true,\"id\":\"1088784698119036991\"}],\"guild_join_requests\":[],\"geo_ordered_rtc_regions\":[\"bucharest\",\"milan\",\"frankfurt\",\"rotterdam\",\"russia\"],\"application\":{\"id\":\"1089180261163470870\",\"flags\":0},\"_trace\":[\"[\\\"gateway-prd-us-east1-d-v46p\\\",{\\\"micros\\\":102555,\\\"calls\\\":[\\\"id_created\\\",{\\\"micros\\\":2004,\\\"calls\\\":[]},\\\"session_lookup_time\\\",{\\\"micros\\\":1099,\\\"calls\\\":[]},\\\"session_lookup_finished\\\",{\\\"micros\\\":16,\\\"calls\\\":[]},\\\"discord-sessions-blue-prd-2-150\\\",{\\\"micros\\\":96413,\\\"calls\\\":[\\\"start_session\\\",{\\\"micros\\\":72401,\\\"calls\\\":[\\\"discord-api-54ccd7b85-t4z6w\\\",{\\\"micros\\\":63264,\\\"calls\\\":[\\\"get_user\\\",{\\\"micros\\\":14916},\\\"get_guilds\\\",{\\\"micros\\\":9366},\\\"send_scheduled_deletion_message\\\",{\\\"micros\\\":7},\\\"guild_join_requests\\\",{\\\"micros\\\":1},\\\"authorized_ip_coro\\\",{\\\"micros\\\":10}]}]},\\\"starting_guild_connect\\\",{\\\"micros\\\":59,\\\"calls\\\":[]},\\\"presence_started\\\",{\\\"micros\\\":450,\\\"calls\\\":[]},\\\"guilds_started\\\",{\\\"micros\\\":108,\\\"calls\\\":[]},\\\"guilds_connect\\\",{\\\"micros\\\":1,\\\"calls\\\":[]},\\\"presence_connect\\\",{\\\"micros\\\":23340,\\\"calls\\\":[]},\\\"connect_finished\\\",{\\\"micros\\\":23346,\\\"calls\\\":[]},\\\"build_ready\\\",{\\\"micros\\\":22,\\\"calls\\\":[]},\\\"clean_ready\\\",{\\\"micros\\\":0,\\\"calls\\\":[]},\\\"optimize_ready\\\",{\\\"micros\\\":1,\\\"calls\\\":[]},\\\"split_ready\\\",{\\\"micros\\\":24,\\\"calls\\\":[]}]}]}]\"]}}")

; (print *bot-identification-data*)
; (json:encode-json-alist-to-string (print  (pairlis  '(:op  :d) '(1 251))))

;  (wsd:send connection message))

; (ch:send *gateway-channel*   *bot-identification-data*)

;
;(defparameter aliluia (connect->discord))
;  (json:encode-json-alist-to-string))
;  2  	Identify  	Send  	Starts a new session during the initial handshake.)


; (cl-json:decode-json-from-string  "{\"t\":\"READY\",\"s\":1,\"op\":0,\"d\":{\"v\":10,\"user_settings\":{},\"user\":{\"verified\":true,\"username\":\"pseudoincels-bot\",\"mfa_enabled\":false,\"id\":\"1089180261163470870\",\"global_name\":null,\"flags\":0,\"email\":null,\"display_name\":null,\"discriminator\":\"1873\",\"bot\":true,\"avatar\":null},\"session_type\":\"normal\",\"session_id\":\"fa28650c82f04fddadb19ef698382bb7\",\"resume_gateway_url\":\"wss://gateway-us-east1-c.discord.gg\",\"relationships\":[],\"private_channels\":[],\"presences\":[],\"guilds\":[],\"guild_join_requests\":[],\"geo_ordered_rtc_regions\":[\"bucharest\",\"milan\",\"frankfurt\",\"rotterdam\",\"russia\"],\"application\":{\"id\":\"1089180261163470870\",\"flags\":0},\"_trace\":[\"[\\\"gateway-prd-us-east1-c-bgz7\\\",{\\\"micros\\\":101461,\\\"calls\\\":[\\\"id_created\\\",{\\\"micros\\\":1625,\\\"calls\\\":[]},\\\"session_lookup_time\\\",{\\\"micros\\\":375,\\\"calls\\\":[]},\\\"session_lookup_finished\\\",{\\\"micros\\\":17,\\\"calls\\\":[]},\\\"discord-sessions-blue-prd-2-287\\\",{\\\"micros\\\":98617,\\\"calls\\\":[\\\"start_session\\\",{\\\"micros\\\":52213,\\\"calls\\\":[\\\"discord-api-6856d85b7d-8lltt\\\",{\\\"micros\\\":45334,\\\"calls\\\":[\\\"get_user\\\",{\\\"micros\\\":13115},\\\"get_guilds\\\",{\\\"micros\\\":2609},\\\"send_scheduled_deletion_message\\\",{\\\"micros\\\":11},\\\"guild_join_requests\\\",{\\\"micros\\\":3147},\\\"authorized_ip_coro\\\",{\\\"micros\\\":10}]}]},\\\"starting_guild_connect\\\",{\\\"micros\\\":61,\\\"calls\\\":[]},\\\"presence_started\\\",{\\\"micros\\\":384,\\\"calls\\\":[]},\\\"guilds_started\\\",{\\\"micros\\\":2,\\\"calls\\\":[]},\\\"guilds_connect\\\",{\\\"micros\\\":1,\\\"calls\\\":[]},\\\"presence_connect\\\",{\\\"micros\\\":45932,\\\"calls\\\":[]},\\\"connect_finished\\\",{\\\"micros\\\":45937,\\\"calls\\\":[]},\\\"build_ready\\\",{\\\"micros\\\":16,\\\"calls\\\":[]},\\\"clean_ready\\\",{\\\"micros\\\":0,\\\"calls\\\":[]},\\\"optimize_ready\\\",{\\\"micros\\\":1,\\\"calls\\\":[]},\\\"split_ready\\\",{\\\"micros\\\":1,\\\"calls\\\":[]}]}]}]\"]}}")



; ((:T . "READY") (:S . 1) (:OP . 0)
;  (:D (:V . 10) (:USER--SETTINGS)
;   (:USER (:VERIFIED . T) (:USERNAME . "pseudoincels-bot") (:MFA--ENABLED)
;    (:ID . "1089180261163470870") (:GLOBAL--NAME) (:FLAGS . 0) (:EMAIL)
;    (:DISPLAY--NAME) (:DISCRIMINATOR . "1873") (:BOT . T) (:AVATAR))
;   (:SESSION--TYPE . "normal")
;   (:SESSION--ID . "fa28650c82f04fddadb19ef698382bb7")
;   (:RESUME--GATEWAY--URL . "wss://gateway-us-east1-c.discord.gg")
;   (:RELATIONSHIPS) (:PRIVATE--CHANNELS) (:PRESENCES) (:GUILDS)
;   (:GUILD--JOIN--REQUESTS)
;   (:GEO--ORDERED--RTC--REGIONS "bucharest" "milan" "frankfurt" "rotterdam"
;    "russia")
;   (:APPLICATION (:ID . "1089180261163470870") (:FLAGS . 0))
;   (:--TRACE
;    "[\"gateway-prd-us-east1-c-bgz7\",{\"micros\":101461,\"calls\":[\"id_created\",{\"micros\":1625,\"calls\":[]},\"session_lookup_time\",{\"micros\":375,\"calls\":[]},\"session_lookup_finished\",{\"micros\":17,\"calls\":[]},\"discord-sessions-blue-prd-2-287\",{\"micros\":98617,\"calls\":[\"start_session\",{\"micros\":52213,\"calls\":[\"discord-api-6856d85b7d-8lltt\",{\"micros\":45334,\"calls\":[\"get_user\",{\"micros\":13115},\"get_guilds\",{\"micros\":2609},\"send_scheduled_deletion_message\",{\"micros\":11},\"guild_join_requests\",{\"micros\":3147},\"authorized_ip_coro\",{\"micros\":10}]}]},\"starting_guild_connect\",{\"micros\":61,\"calls\":[]},\"presence_started\",{\"micros\":384,\"calls\":[]},\"guilds_started\",{\"micros\":2,\"calls\":[]},\"guilds_connect\",{\"micros\":1,\"calls\":[]},\"presence_connect\",{\"micros\":45932,\"calls\":[]},\"connect_finished\",{\"micros\":45937,\"calls\":[]},\"build_ready\",{\"micros\":16,\"calls\":[]},\"clean_ready\",{\"micros\":0,\"calls\":[]},\"optimize_ready\",{\"micros\":1,\"calls\":[]},\"split_ready\",{\"micros\":1,\"calls\":[]}]}]}]")))

; "Recieved message: {\"t\":null,\"s\":null,\"op\":11,\"d\":null}"


; "Recieved message:
; {\"t\":\"READY\",\"s\":1,\"op\":0,\"d\":{\"v\":10,\"user_settings\":{},\"user\":{\"verified\":true,\"username\":\"pseudoincels-bot\",\"mfa_enabled\":false,\"id\":\"1089180261163470870\",\"global_name\":null,\"flags\":0,\"email\":null,\"display_name\":null,\"discriminator\":\"1873\",\"bot\":true,\"avatar\":null},\"session_type\":\"normal\",\"session_id\":\"7e914f4898c0e2b76df23dfc32b878a5\",\"resume_gateway_url\":\"wss://gateway-us-east1-d.discord.gg\",\"relationships\":[],\"private_channels\":[],\"presences\":[],\"guilds\":[],\"guild_join_requests\":[],\"geo_ordered_rtc_regions\":[\"bucharest\",\"milan\",\"frankfurt\",\"rotterdam\",\"russia\"],\"application\":{\"id\":\"1089180261163470870\",\"flags\":0},\"_trace\":[\"[\\\"gateway-prd-us-east1-d-cg17\\\",{\\\"micros\\\":102294,\\\"calls\\\":[\\\"id_created\\\",{\\\"micros\\\":2046,\\\"calls\\\":[]},\\\"session_lookup_time\\\",{\\\"micros\\\":1286,\\\"calls\\\":[]},\\\"session_lookup_finished\\\",{\\\"micros\\\":25,\\\"calls\\\":[]},\\\"discord-sessions-blue-prd-2-36\\\",{\\\"micros\\\":97524,\\\"calls\\\":[\\\"start_session\\\",{\\\"micros\\\":86474,\\\"calls\\\":[\\\"discord-api-6856d85b7d-hgpsv\\\",{\\\"micros\\\":79003,\\\"calls\\\":[\\\"get_user\\\",{\\\"micros\\\":18323},\\\"get_guilds\\\",{\\\"micros\\\":6937},\\\"send_scheduled_deletion_message\\\",{\\\"micros\\\":8},\\\"guild_join_requests\\\",{\\\"micros\\\":1639},\\\"authorized_ip_coro\\\",{\\\"micros\\\":10}]}]},\\\"starting_guild_connect\\\",{\\\"micros\\\":52,\\\"calls\\\":[]},\\\"presence_started\\\",{\\\"micros\\\":378,\\\"calls\\\":[]},\\\"guilds_started\\\",{\\\"micros\\\":2,\\\"calls\\\":[]},\\\"guilds_connect\\\",{\\\"micros\\\":1,\\\"calls\\\":[]},\\\"presence_connect\\\",{\\\"micros\\\":10580,\\\"calls\\\":[]},\\\"connect_finished\\\",{\\\"micros\\\":10592,\\\"calls\\\":[]},\\\"build_ready\\\",{\\\"micros\\\":22,\\\"calls\\\":[]},\\\"clean_ready\\\",{\\\"micros\\\":1,\\\"calls\\\":[]},\\\"optimize_ready\\\",{\\\"micros\\\":1,\\\"calls\\\":[]},\\\"split_ready\\\",{\\\"micros\\\":0,\\\"calls\\\":[]}]}]}]\"]}}"



; ((:T . "READY") (:S . 1) (:OP . 0)
;  (:D (:V . 10) (:USER--SETTINGS)
;   (:USER (:VERIFIED . T) (:USERNAME . "PseudoAndrewTate") (:MFA--ENABLED)
;    (:ID . "1089180261163470870") (:GLOBAL--NAME) (:FLAGS . 0) (:EMAIL)
;    (:DISPLAY--NAME) (:DISCRIMINATOR . "1873") (:BOT . T) (:AVATAR))
;   (:SESSION--TYPE . "normal")
;   (:SESSION--ID . "73d6704c4725675627e6d790218d2603")
;   (:RESUME--GATEWAY--URL . "wss://gateway-us-east1-d.discord.gg")
;   (:RELATIONSHIPS) (:PRIVATE--CHANNELS) (:PRESENCES)
;   (:GUILDS ((:UNAVAILABLE . T)
;             (:ID . "1088784698119036991")))
;   (:GUILD--JOIN--REQUESTS)
;   (:GEO--ORDERED--RTC--REGIONS "bucharest" "milan" "frankfurt" "rotterdam"
;    "russia")
;   (:APPLICATION (:ID . "1089180261163470870") (:FLAGS . 0))
;   (:--TRACE
;    "[\"gateway-prd-us-east1-d-v46p\",{\"micros\":102555,\"calls\":[\"id_created\",{\"micros\":2004,\"calls\":[]},\"session_lookup_time\",{\"micros\":1099,\"calls\":[]},\"session_lookup_finished\",{\"micros\":16,\"calls\":[]},\"discord-sessions-blue-prd-2-150\",{\"micros\":96413,\"calls\":[\"start_session\",{\"micros\":72401,\"calls\":[\"discord-api-54ccd7b85-t4z6w\",{\"micros\":63264,\"calls\":[\"get_user\",{\"micros\":14916},\"get_guilds\",{\"micros\":9366},\"send_scheduled_deletion_message\",{\"micros\":7},\"guild_join_requests\",{\"micros\":1},\"authorized_ip_coro\",{\"micros\":10}]}]},\"starting_guild_connect\",{\"micros\":59,\"calls\":[]},\"presence_started\",{\"micros\":450,\"calls\":[]},\"guilds_started\",{\"micros\":108,\"calls\":[]},\"guilds_connect\",{\"micros\":1,\"calls\":[]},\"presence_connect\",{\"micros\":23340,\"calls\":[]},\"connect_finished\",{\"micros\":23346,\"calls\":[]},\"build_ready\",{\"micros\":22,\"calls\":[]},\"clean_ready\",{\"micros\":0,\"calls\":[]},\"optimize_ready\",{\"micros\":1,\"calls\":[]},\"split_ready\",{\"micros\":24,\"calls\":[]}]}]}]")))
;
; "Recieved message: {\"t\":null,\"s\":null,\"op\":11,\"d\":null}"

; -----  HTTP

(defvar *command-channel* (make-instance 'ch:bounded-channel  :size 100))

"/guilds/{guild.id}/members/search  "

; Authorization: Bot MTk4NjIyNDgzNDcxOTI1MjQ4.Cl2FMQ.ZnCjm1XVW7vRze4b7Cq4se7kKWs


(defun output-message-loop (channel http)
   (ch:pexec ()
    (loop
      (let ((message (ch:recv channel)))
        (print (format nil "output message: ~a" message))
        (wsd:send connection message)))))




; import { axios } from "@pipedream/platform"
; export default defineComponent({
;                                 props: {
;                                         discord_bot: {
;                                                       type: "app",
;                                                       app: "discord_bot",}}
;
;                                 ,
;                                 async run({steps, $}) {
;                                                        return await axios($, {
;                                                                               method: 'POST',
;                                                                               url: `https://discord.com/api/channels/<your channel id here>/messages`,
;                                                                               headers: {
;                                                                                         "Authorization": `Bot ${this.discord_bot.$auth.bot_token}`,}
;                                                                               ,
;                                                                               data: {
;                                                                                      embeds: [ steps.generate_embed.$return_value.embed]}})}
;
;
;                                 ,})
;
