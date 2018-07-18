;;; package --- Mongo ELisp Driver
;;; Commentary:

;;; Provide MongoDB driver access from Emacs.  This uses a fork of Emacs that links the mongo c
;;; driver c driver.  The elisp-ffi module can be found here: https://github.com/skeeto/elisp-ffi.
;;; See [BUILDEMACS.md](BUILDEMACS.md) for instructions on building Emacs.

;;; Code

(defgroup mongo  nil
  "Minor mode for interacting with MongoDB from within JSON buffers."
  :prefix "mongo-"
  :group 'help
  :link '(url-link "http://github.com/mongodb/mongo-mode"))

(defconst mongomode-version "0.2-beta"
  "mongo-mode version")

(require 'json)

;;; Initialize the connection
(mongo-init)

;;; mongoc_collection_aggregate
(defun mongo-collection-aggregate (pipeline &optional opts query-flags)
  "Execute an aggregation pipeline"
  (interactive "spipeline:\nsopts:")
  (or opts (setq opts "{}"))
  (or query-flags (setq query-flags 0)) ; set default value
  (let* ((command (format "{\"aggregate\": \"%s\", \"pipeline\": %s, \"cursor\": {}}" collection-name pipeline))
         (output (json-read-from-string(mongo-command-with-opts command opts))))
    output))


(defun mongo-list-databases ()
  (let* ((database-name "admin")
         (result-json (mongo-simple-command "{\"listDatabases\": 1}"))
         (dbs (json-read-from-string result-json)))
    dbs))

(defun mongo-server-status ()
  (let* ((result-json (mongo-simple-command "{\"serverStatus\": 1}"))
         (stats (json-read-from-string result-json)))
    stats))

(defun mongo-collection-stats (db-name collection-name)
  (let* ((database-name db-name)
         (command (format "{\"collStats\": \"%s\"}" collection-name))
         (result-json (mongo-simple-command command))
         (stats (json-read-from-string result-json)))
    stats))

(defun mongo-list-collections (db-name)
  (let* ((database-name db-name)
         (result-json (mongo-simple-command "{\"listCollections\": 1}"))
         (collections (json-read-from-string result-json)))
    collections))

(defun mongo-single-document (db-name coll-name)
   (let* ((database-name db-name)
          (command (format "{\"find\": \"%s\", \"limit\": 1}" coll-name))
         (result-json (mongo-simple-command command))
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
  (mongo-show-document current-db-name (thing-at-point 'symbol)))

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
  (mongo-show-collections (thing-at-point 'symbol)))

(defun mongo-show-dbs ()
  ;; TODO: the buffer we open should not be editable
  (interactive)
  (let* ((response (mongo-list-databases))
         (stats (mongo-server-status))
         (dbs (alist-get 'databases response))
         (lines (mapcar 'mongo-render-database dbs))
         (db-content (string-join lines "\n"))
         (connection-display (render-server-status stats mongo-uri))
         (content (concat connection-display "\n" db-content))
         (buf (get-buffer-create "mongo-dbs")))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert content)
    (local-set-key (kbd "RET") 'jump-to-collections-view)))

;; Interactives
(defun mongo-ping ()
  (interactive)
  (print (mongo-simple-command "{\"ping\": 1}")))

(defun mongo-find-region (start-point end-point)
  (interactive "r")
  (let* ((region-contents (buffer-substring start-point end-point))
         (query-output (mongo-find region-contents "{}")))
    (insert "\n" query-output)))

(defun mongo-aggregate-region (start-point end-point)
  (interactive "r")
  (let* ((region-contents (buffer-substring start-point end-point))
         (agg-output (json-encode (alist-get 'firstBatch (alist-get 'cursor (mongo-collection-aggregate region-contents))))))
    (insert "\n" agg-output)))

(defun mongo-insert-region (start-point end-point)
  (interactive "r")
  (let* ((region-contents (buffer-substring start-point end-point))
         (output (mongo-insert-one region-contents "{}")))
    (print output)))

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
