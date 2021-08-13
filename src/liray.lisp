(defpackage liray
  (:use :cl)
  (:export :cli))
(in-package :liray)

(ql:quickload '("dexador" "cl-json" "log4cl" "clish"))

(defparameter *home*  (or (uiop:getenv "LIRAY") "~/.config/liray"))
(defparameter *debug* t)
(defparameter *cache* '())

(defun join (str list)
  (if (null list)
      ""
    (let ((result (first list)))
      (dolist (item (cdr list))
        (setf result (concatenate 'string result str item)))
      result)))

(defun split (string &optional (separator #\Space))
  "Return a list from a string splited at each separators"
  (loop for i = 0 then (1+ j)
        as j = (position separator string :start i)
        as sub = (subseq string i j)
        unless (string= sub "") collect sub
        while j))

(defun string->path (string)
  (let ((path (if (char= #\@ (char string 0))
                  (pathname (concatenate 'string *home* (subseq string 1)))
                  string)))
    path))

(defun get-alist-value (alist key)
    (cdr (assoc key alist)))

(defun set-alist-value (alist key value)
    (if (assoc key alist)
        (setf (cdr (assoc key alist)) value)
        (push (cons key value) alist)))

(defun set-cache (key value)
  (if (assoc key *cache*)
    (setf (cdr (assoc key *cache*)) value)
    (push (cons key value) *cache*)))

(defun load-cache (&optional (file (or #p"~/.config/liray/cache")))
  (if (probe-file file)
      (with-open-file (in file) (with-standard-io-syntax (setf *cache* (read in))))
      (set-cache :template '((:INBOUNDS
                              ((:TAG . "socks")
                               (:PORT . 10800)
                               (:LISTEN . "127.0.0.1")
                               (:PROTOCOL . "socks")
                               (:SETTINGS
                                (:AUTH . "noauth")
                                (:UDP . T))
                               (:SNIFFING
                                (:ENABLED . T)
                                (:DEST-OVERRIDE "http" "tls")))
                              ((:TAG . "http")
                               (:PORT . 10801)
                               (:LISTEN . "127.0.0.1")
                               (:PROTOCOL . "http")
                               (:SNIFFING
                                (:ENABLED . T)
                                (:DEST-OVERRIDE "http" "tls"))
                               (:SETTINGS
                                (:AUTH . "noauth")
                                (:UDP . T))))
                             (:OUTBOUNDS
                              ((:TAG . "direct")
                               (:PROTOCOL . "freedom")
                               (:SETTINGS))
                              ((:TAG . "block")
                               (:PROTOCOL . "blackhole")
                               (:SETTINGS
                                (:VNEXT)
                                (:SERVERS)
                                (:RESPONSE
                                 (:TYPE . "http")))
                               (:STREAM-SETTINGS)
                               (:MUX)))
                             (:ROUTING
                              (:DOMAIN-STRATEGY . "IPOnDemand")
                              (:RULES
                               ((:TYPE . "field")
                                (:OUTBOUND-TAG . "direct")
                                (:DOMAIN "geosite:cn"))
                               ((:TYPE . "field")
                                (:OUTBOUND-TAG . "direct")
                                (:IP "geoip:cn" "geoip:private"))))))))

(defun dump-cache (&optional (file (or #p"~/.config/liray/cache")))
  (uiop:ensure-all-directories-exist (list file))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (pprint *cache* out))))

(defun fetch-servers (url)
    (loop for item in (split (base64:base64-string-to-string (dex:get url)) #\NewLine)
          for tmp = (split item #\:)
          for server = (json:decode-json-from-string (base64:base64-string-to-string (subseq (second tmp) 2)))
          collect (cons (cons :p (first tmp)) server)))

(defun tcping (address port &optional (timeout 3))
    (let ((start (get-internal-real-time)))
        (handler-case
            (progn (usocket:socket-close (usocket:socket-connect address port :timeout timeout)) (round (/ (- (get-internal-real-time) start) 1000)))
          (error (condition) (if (eq (type-of condition) 'usocket:timeout-error) nil (pprint condition))))))

(defun same-server? (a b)
    (every (lambda (key) (equal (assoc key a) (assoc key b)))
           '(:add :port :ps)))

(defun add-server (server &key (subscribe "default"))
    (let ((servers (cdr (assoc :servers *cache*))))
         (if (member server servers :test #'same-server?)
             (format t "Server ~a already exists!~%" (cdr (assoc :ps server)))
             (set-cache :servers (cons (cons (cons :sub subscribe) server) servers)))))

(defun update-subscribe (subscribe)
    (let ((servers (fetch-servers (getf subscribe :url))))
         (loop for server in servers
               do (add-server server))))

(defun update-subscribes ()
    (loop for subscribe in (cdr (assoc :subscribes *cache*))
          do (update-subscribe subscribe))
  (dump-cache))

(defun subscribe (subscribe &key name (update t))
  (let* ((subs (cdr (assoc :subscribes *cache*)))
         (name (if name name (format nil "sub~a" (length subs)))))
    (if (member name subs :key (lambda (item) (getf item :name)) :test #'equal)
        (format t "Subscribe ~a already exists!~%" name)
        (set-cache :subscribes (push (list :name name :url subscribe) subs)))
    (when update
      (update-subscribes))
    (get-alist-value *cache* :subscribes))
  (dump-cache))

(defun test-server (server)
    (let ((host (cdr (assoc :add server)))
          (port (cdr (assoc :port server))))
         (format t "Tcping ~a:~a...~%" host port)
      (set-alist-value server :delay (tcping host port))
      server))

(defun test-servers ()
    (set-alist-value *cache* :servers
                     (loop for server in (cdr (assoc :servers *cache*))
                           collect (test-server server)))
  (dump-cache))

(defun sort-servers ()
    (set-alist-value *cache* :servers
                     (sort
                           (get-alist-value *cache* :servers)
                        (lambda (a b)
                            (if (and a b)
                                (< a b)
                                a))
                        :key (lambda (item) (cdr (assoc :delay item)))))
  (dump-cache))

(defun generate-config (&optional (count 6))
  (let ((template (copy-alist (cdr (assoc :template *cache*))))
        (servers (subseq (cdr (assoc :servers *cache*) ) 0 count)))
    (loop for server in servers
          collect (let* ((users (list (pairlis (list :id :alter-id :security :level)
                                               (list (cdr (assoc :id server))
                                                     (cdr (assoc :aid server))
                                                     "auto"
                                                     0))))
                         (vnext (list (pairlis (list :users :port :address)
                                               (list users
                                                     (cdr (assoc :port server))
                                                     (cdr (assoc :add server))))))
                         (settings (pairlis (list :vnext :delay)
                                            (list vnext (cdr (assoc :delay server)))))
                         (stream (pairlis (list :mux :kcp-settings :tls-settings :ws-settings :security  :network)
                                          (list (pairlis '(:enabled :concurrency) (list t 8))
                                                nil
                                                nil
                                                (if (equal (cdr (assoc :net server)) "ws")
                                                    (pairlis '(:path :headers)
                                                             (list (cdr (assoc :path server))
                                                                   (acons :-host (cdr (assoc :host server)) '())))
                                                    nil)
                                                (cdr (assoc :type server))
                                                (cdr (assoc :net server)))))
                         (outbound (pairlis (list :protocol :stream-settings :settings :tag)
                                            (list (cdr (assoc :p server))
                                                  stream
                                                  settings
                                                  (cdr (assoc :ps server))))))
                    (push outbound (cdr (assoc :outbounds template)))
                    outbound))
    template))

(defun save-config (&optional (config (generate-config)) (file #p"~/.config/liray/config.json"))
  (uiop:ensure-all-directories-exist (list file))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (when *debug*
        (format t "Save config to ~A~%" file))
      (json:encode-json config out))))

(defun list-servers (&key test update)
  (when update (update-subscribes))
  (when test (test-servers))
  (sort-servers)
  (dolist (server (get-alist-value *cache* :servers))
    (format t "~a ~a ~a ~a~%"
            (get-alist-value server :ps)
            (get-alist-value server :add)
            (get-alist-value server :port)
            (get-alist-value server :delay))))

(clish:defcli cache
    (show
     (lambda (&optional key)
       (if key
        (format t "~a:~%~A~%" key (cdr (assoc (intern (string-upcase key) :keyword) *cache*)))
        (mapcar (lambda (item) (format t "~A:~%~A~%" (string-downcase (string (car item)))
                                  (cdr item))) *cache*)))))

(defun start-v2ray (&key update (sort t))
  (when update
    (update-subscribes))
  (when sort
    (test-servers)
    (sort-servers))
  (pprint *cache*)
  (save-config)
  (uiop:run-program
   "v2ray -config ~/.config/liray/config.json"
    :ignore-error-status t
    :output :interactive
    :force-shell nil))

(clish:defcli cli
  (cache #'cache)
  (sub #'subscribe)
  (test #'test-servers)
  (list #'list-servers)
  (start #'start-v2ray))
  ;; (:pre (lambda (command args) (load-cache))))

(load-cache)

(defun main (&rest args)
  (format t "Liray~%")
  (apply #'cli args))
