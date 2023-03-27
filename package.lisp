;;;; package.lisp

(defpackage #:pseudo-incels-bot
  (:use #:cl)
  (:import-from :chanl #:pexec)
  (:import-from :serapeum #:dict)
  (:import-from :trivia #:match)
  (:local-nicknames (:ch :chanl)
                    (au :assoc-utils)))



(in-package #:pseudo-incels-bot)

(defvar *config* (conf:get-conf-hash  (conf:init-conf "./"  "incel-bot.conf")))

(defun hash-keys (hash-table)
 (loop for key being the hash-keys of hash-table collect key))


