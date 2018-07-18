;;; package --- Mongo ELisp Driver
;;; Commentary:
;;; Provide MongoDB driver access from Emacs.  This uses the ffi module to wrap the mongo
;;; c driver.  The elisp-ffi module can be found here: https://github.com/skeeto/elisp-ffi

;;; Code

(defgroup mongo  nil
  "Minor mode for interacting with MongoDB from within JSON buffers."
  :prefix "mongo-"
  :group 'help
  :link '(url-link "http://github.com/mongodb/mongo-mode"))

(defconst mongomode-version "0.1-beta"
  "mongo-mode version")

(require 'ffi)
(require 'json)

(ffi-ensure)

;;; Initialize the connection
(ffi-call mlib "mongoc_init" [:void])

;;; Functions
;;; Setup Client connection
;;; TODO: don't make these interactive
;;; TODO: prefix these with mongo-- to make them private

(defun mongo-client-new (URI)
  "Connect to the MongoDB database with connection string URI"
  (interactive "sURI:")
  (setq client (ffi-call mlib "mongoc_client_new" [:pointer :pointer] URI)))

;;; Select database
(defun mongo-get-database (db)
  "Wrapper around mongoc_client_get_database"
  (interactive "sdb:")
  (setq database (ffi-call mlib "mongoc_client_get_database" [:pointer :pointer :pointer] client db)))

;;; select collection
(defun mongo-get-collection (db coll)
  "Wrapper around mongoc_client_get_collection"
  (interactive "sdb:\nscollection:" )
  (setq collection (ffi-call mlib "mongoc_client_get_collection" [:pointer :pointer :pointer :pointer] client db coll)))

;;; make a write concern object
(defun mongo-new-write-concern ()
    "Make a new write concern object"
    (ffi-call mlib "mongoc_write_concern_new" [:pointer]))

;;; destroy a write concern object
(defun mongo-destroy-write-concern (write-concern)
    "Destroy a write concern object"
    (ffi-call mlib "mongoc_write_concern_destroy" [:void :pointer] write-concern))

(defun bson-new()
  "Create an empty bson document"
  (ffi-call blib "bson_new" [:pointer]))

(defun bson-new-from-json (document)
  "Create a bson object from json"
  (ffi-call blib "bson_new_from_json" [:pointer :pointer :sint8 :pointer] document -1 nil))

(defun bson-as-json (bson)
  "Return relaxed extended json for the bson object"
  (ffi-get-string (ffi-call blib "bson_as_relaxed_extended_json" [:pointer :pointer :pointer] bson nil)))

;;; simple command
(defun mongo-command-simple (command &optional db-name)
  "Call command simple"
  (interactive "scommand:")
  (let* ((db-name (or db-name "admin"))
         (command-bson (bson-new-from-json command))
         (reply (ffi-call blib "bson_new" [:pointer]))
         (retval
           (ffi-call mlib "mongoc_client_command_simple"
                     [:sint8 :pointer :pointer :pointer :pointer :pointer :pointer]
                     client
                     db-name
                     command-bson
                     nil
                     reply
                     nil)))
    (bson-as-json reply)))

;;; insert
(defun mongo-insert(document)
  "Insert a document"
  (interactive "sdocument:")
  (let* ((document (bson-new-from-json document))
         (write-concern (mongo-new-write-concern))
         (retval (ffi-call mlib "mongoc_collection_insert"
                           [:sint8 :pointer :sint8 :pointer :pointer :pointer]
                           collection
                           0
                           document
                           write-concern
                           nil)))
    (mongo-destroy-write-concern write-concern)
    retval))

(defun mongo-exhaust-cursor (cursor)
  "Read all the documents out of the CURSOR."
  (let ((doc (bson-new))
        (string "")
        retval)
    (while (> (ffi-call hlib "cursor_next_bson_wrap" [:sint8 :pointer :pointer] cursor doc) 0)
      (setq string (concat string (bson-as-json doc) "\n")))
    string))

;;; find_with_opts
(defun mongo-find-with-opts (filter &optional opts)
  "Do a find command and printout all docs"
  (interactive "sfilter:\nsopts:")
  (or opts (setq opts "{}"))
  (let* ((filter (bson-new-from-json filter))
         (opts (bson-new-from-json opts))
         (cursor (ffi-call mlib "mongoc_collection_find_with_opts" [:pointer :pointer :pointer :pointer :pointer]
                            collection
                            filter
                            opts
                            nil)))
    (mongo-exhaust-cursor cursor)))


;;; mongoc_collection_aggregate
(defun mongo-collection-aggregate (pipeline &optional opts query-flags)
  "Execute an aggregation pipeline"
  (interactive "spipeline:\nsopts:")
  (or opts (setq opts "{}"))
  (or query-flags (setq query-flags 0)) ; set default value
  (let* ((pipeline-bson (bson-new-from-json pipeline))
         (opts (bson-new-from-json opts))
         (cursor (ffi-call mlib "mongoc_collection_aggregate" [:pointer :pointer :sint16 :pointer :pointer :pointer]
                           collection
                           query-flags
                           pipeline-bson
                           opts
                           nil)))
    (mongo-exhaust-cursor cursor)))

(defun mongo-collection-create-bulk-operation (collection &optional write-concern)
  "Create a bulk operation"
  (or write-concern (setq write-concern (mongo-new-write-concern)))
  (ffi-call mlib "mongoc_collection_create_bulk_operation" [:pointer :pointer :sint8 :pointer] collection 1 write-concern))

(defun mongo-bulk-operation-insert-with-opts(bulkop document &optional opts)
  "Perform an insert into a bulk operation"
  (or opts (setq opts "{}"))
  (let* ((document (bson-new-from-json document))
         (opts (bson-new-from-json opts)))
    (ffi-call mlib "mongoc_bulk_operation_insert_with_opts" [:sint8 :pointer :pointer :pointer :pointer]
              bulkop
              document
              opts
              nil)))

;;; mongoc_bulk_operation_execute ; Maybe only this one, not the buld inserts
(defun mongo-bulk-operation-execute (bulkop)
  "Execute the bulk operation"
  (let* ((reply (bson-new))
         (retval (ffi-call mlib "mongoc_bulk_operation_execute" [:uint32 :pointer :pointer :pointer] bulkop reply nil)))
    (bson-as-json reply)))


(defun mongo-list-databases ()
  (let* ((result-json (mongo-command-simple "{\"listDatabases\": 1}"))
         (dbs (json-read-from-string result-json)))
    dbs))

(defun mongo-server-status ()
  (let* ((result-json (mongo-command-simple "{\"serverStatus\": 1}"))
         (stats (json-read-from-string result-json)))
    stats))

(defun mongo-collection-stats (db-name collection-name)
  (let* ((command (format "{\"collStats\": \"%s\"}" collection-name))
         (result-json (mongo-command-simple command db-name))
         (stats (json-read-from-string result-json)))
    stats))

(defun mongo-list-collections (db-name)
  (let* ((result-json (mongo-command-simple "{\"listCollections\": 1}" db-name))
         (collections (json-read-from-string result-json)))
    collections))

(defun mongo-single-document (db-name coll-name)
  (let* ((command (format "{\"find\": \"%s\", \"limit\": 1}" coll-name))
         (result-json (mongo-command-simple command db-name))
         (result (json-read-from-string result-json))
         (docs (alist-get 'firstBatch (alist-get 'cursor result))))
    (aref docs 0)))

(defun mongo-render-database (db)
  ;; TODO right justify the db sizes
  "Generate a string representation of a db"
  (let* ((name (alist-get 'name db))
         (size (alist-get 'sizeOnDisk db))
         (colored-name (propertize name 'face 'font-lock-function-name-face)))
    (format "%d %s" size colored-name)))

(defun render-server-status (stats connection-str)
  (let* ((process-type (alist-get 'process stats))
         (version (alist-get 'version stats))
         (text (string-join (list process-type version connection-str) " ")))
    (propertize text 'face 'font-lock-type-face)))

(defun render-collection-line (name coll-stats)
  ;; TODO: we should use stuff from the collection stats too
  (propertize name 'face 'font-lock-function-name-face))

(defun mongo-show-document (db-name coll-name)
  "Open a buffer displaying a single document from the collection"
  (let* ((doc (mongo-single-document db-name coll-name))
         (content (json-encode doc))
         (buf (get-buffer-create (format "mongo-%s-%s" db-name coll-name))))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert content)
    (js-mode)
    (json-pretty-print-buffer)
    (beginning-of-buffer)))


(defun jump-to-document-view ()
  (interactive)
  (mongo-show-document current-db-name (thing-at-point 'word)))

(defun mongo-show-collections (db-name)
  (let* ((response (mongo-list-collections db-name))
         (collections (alist-get 'firstBatch (alist-get 'cursor (mongo-list-collections db-name))))
         (collection-names (mapcar (lambda (c) (alist-get 'name c)) collections))
         (collection-stats (mapcar (lambda (c) (mongo-collection-stats db-name c)) collection-names))
         (lines (mapcar* 'render-collection-line collection-names collection-stats))
         (collection-content (string-join lines "\n"))
         (db-content (propertize db-name 'face 'font-lock-type-face))
         (content (concat db-content "\n" collection-content))
         (buf (get-buffer-create (format "mongo-%s" db-name))))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert content)
    ;; TODO: is a global the best way of doing this?
    (setq current-db-name db-name)
    (local-set-key (kbd "RET") 'jump-to-document-view)))

(defun jump-to-collections-view ()
  (interactive)
  (mongo-show-collections (thing-at-point 'word)))

(defun mongo-show-dbs ()
  ;; TODO: the buffer we open should not be editable
  (interactive)
  (let* ((response (mongo-list-databases))
         (stats (mongo-server-status))
         (dbs (alist-get 'databases response))
         (lines (mapcar 'mongo-render-database dbs))
         (db-content (string-join lines "\n"))
         (connection-display (render-server-status stats mongo--url))
         (content (concat connection-display "\n" db-content))
         (buf (get-buffer-create "mongo-dbs")))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert content)
    (local-set-key (kbd "RET") 'jump-to-collections-view)))

;;; Other mongoc functions to wrap
;;; mongoc_client_get_server_status()
;;; mongoc_collection_find_and_modify_with_opts()
;;; mongoc_collection_remove()
;;; mongoc_collection_update()
;;; mongoc_database_drop_with_opts()
;;; mongoc_database_get_collection_names()

;; Helpers
(defun mongo--ensure-login ()
  (if (boundp 'mongo--url)
      () ; do nothing
    (call-interactively 'mongo-login)))

(defun mongo--quit ()
  (print "Calling Mongo--quit")
  (if (boundp 'mongo--url)
      ()));(ffi-destroy ffi-context)))

(defun mongo-turn-on ())

(defun mongo-turn-off ()
  (mongo--quit))

;; Interactives
(defun mongo-ping ()
  (interactive)
  (mongo--ensure-login)
  (print (mongo-command-simple "{\"ping\": 1}")))

(defun mongo-find-region (start-point end-point)
  (interactive "r")
  (mongo--ensure-login)
  (let* ((region-contents (buffer-substring start-point end-point))
         (query-output (mongo-find-with-opts region-contents)))
    (insert "\n" query-output)))

(defun mongo-aggregate-region (start-point end-point)
  (interactive "r")
  (mongo--ensure-login)
  (let* ((region-contents (buffer-substring start-point end-point))
         (agg-output (mongo-collection-aggregate region-contents)))
    (insert "\n" agg-output)))

(defun mongo-insert-region (start-point end-point)
  (interactive "r")
  (mongo--ensure-login)
  (let* ((region-contents (buffer-substring start-point end-point))
         (output (mongo-insert region-contents)))
    (print output)))


(defun mongo-login (url)
  (interactive "sURL: ")
  (if (= (length url) 0)
      (setq mongo--url "mongodb://localhost:27017")
    (setq mongo--url url))
  (mongo-client-new mongo--url)
  (mongo-get-database "test")
  (mongo-get-collection "test" "test")
  ; is kill-buffer-hook the rights place to do this?
  (add-hook 'kill-buffer-hook 'mongo--quit))

;; https://stackoverflow.com/a/9029082
(defun mapcar* (f &rest xs)
  "MAPCAR for multiple sequences"
  (if (not (memq nil xs))
    (cons (apply f (mapcar 'car xs))
      (apply 'mapcar* f (mapcar 'cdr xs)))))


;; Binding nonsense

;;;###autoload
(define-minor-mode mongo-mode
  "Interact with mongodb"
  :lighter " Wg" ; TODO: copy/pasta can we kill this?
  (progn
    (if mongo-mode
        (mongo-turn-on)
      (mongo-turn-off))
    (font-lock-mode 1))) ; TODO: copy/pasta; kill font-lock-mode trigger?

(provide 'mongo)
;;; mongo.el ends here
