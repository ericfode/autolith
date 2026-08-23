(in-package #:autolith)

;;;; -- Nous Tool Gateway Web Access --

;;; The managed Firecrawl contract follows NousResearch/hermes-agent reference
;;; commit f293e7206b4ddd66042329442c6afebc19a8808d. The gateway accepts the
;;; ordinary Firecrawl v2 search and scrape routes with the Nous OAuth bearer.

(defparameter *nous-web-run-description*
  "Web access through the Nous Tool Gateway's managed Firecrawl service.

Supported commands:
* `search_query`: web search with optional domain filters.
* `image_query`: image search with optional domain filters.
* `open`: scrape a public absolute HTTP(S) URL and return bounded numbered text.
* `find`: scrape a public absolute HTTP(S) URL and return matching lines.

Search results contain direct URLs rather than provider reference IDs. Pass those
URLs to `open` or `find`. A call accepts at most four operations total. Calls use
the active Nous OAuth account and may consume Nous subscription tool credits."
  "The provider-visible web.run instructions for Nous Tool Gateway sessions.")

(defparameter *nous-web-supported-command-names*
  '("search_query" "image_query" "open" "find" "response_length")
  "The web.run fields implemented by the managed Firecrawl gateway.")

(defparameter *nous-web-maximum-operations* 4
  "The maximum managed Firecrawl operations accepted in one web.run call.")

(defparameter *nous-web-maximum-response-bytes* (* 2 1024 1024)
  "The largest managed Firecrawl response body accepted by Autolith.")

(defparameter *nous-web-maximum-output-bytes* (* 1024 1024)
  "The largest complete managed web.run result returned to the model.")

(defparameter *nous-web-maximum-query-characters* 2000
  "The largest managed search query accepted from a model.")

(defparameter *nous-web-maximum-domain-count* 20
  "The largest domain allowlist accepted for one managed search.")

(defparameter *nous-web-maximum-domain-characters* 255
  "The largest domain name accepted in a managed search allowlist.")

(defparameter *nous-web-maximum-find-pattern-characters* 500
  "The largest page-search pattern accepted from a model.")

(defparameter *nous-web-maximum-title-characters* 300
  "The largest search-result title returned to the model.")

(defparameter *nous-web-maximum-description-characters* 2000
  "The largest search-result description returned to the model.")

(defparameter *nous-web-maximum-url-characters* 2048
  "The largest search-result URL returned to the model.")

(defparameter *nous-web-blocked-hostnames*
  '("localhost" "metadata.google.internal" "metadata.goog")
  "Hostnames that managed Firecrawl page access must never receive.")


;;;; -- Provider Tool Schema --

(-> nous-web--array-schema (json-object string) json-object)
(defun nous-web--array-schema (items description)
  "Return a bounded managed Firecrawl operation array schema."
  (let ((schema (web--array-schema items description)))
    (setf (gethash "maxItems" schema) *nous-web-maximum-operations*)
    schema))

(-> nous-web--bounded-string-property (string (integer 1)) json-object)
(defun nous-web--bounded-string-property (description maximum-characters)
  "Return a string property bounded to MAXIMUM-CHARACTERS."
  (let ((property (tool-string-property description)))
    (setf (gethash "maxLength" property) maximum-characters)
    property))

(-> nous-web--search-query-schema () json-object)
(defun nous-web--search-query-schema ()
  "Return the search object accepted by the managed Firecrawl gateway."
  (let ((domains
          (web--string-array-schema
           "Optional domains that search results must come from.")))
    (setf (gethash "maxItems" domains) *nous-web-maximum-domain-count*
          (gethash "maxLength" (json-get domains "items"))
          *nous-web-maximum-domain-characters*)
    (web--object-schema
     (json-object
      "q" (nous-web--bounded-string-property
           "Search query."
           *nous-web-maximum-query-characters*)
      "domains" domains)
     :required '("q"))))

(-> nous-web--open-operation-schema () json-object)
(defun nous-web--open-operation-schema ()
  "Return one absolute-URL open operation schema."
  (let ((line-property
          (tool-integer-property "Positive line number at which to start.")))
    (setf (gethash "minimum" line-property) 1)
    (web--object-schema
     (json-object
      "ref_id" (nous-web--bounded-string-property
                "Public absolute HTTP(S) URL to scrape."
                *nous-web-maximum-url-characters*)
      "lineno" line-property)
     :required '("ref_id"))))

(-> nous-web--find-operation-schema () json-object)
(defun nous-web--find-operation-schema ()
  "Return one absolute-URL find operation schema."
  (web--object-schema
   (json-object
    "ref_id" (nous-web--bounded-string-property
              "Public absolute HTTP(S) URL to scrape."
              *nous-web-maximum-url-characters*)
    "pattern" (nous-web--bounded-string-property
               "Text pattern to find."
               *nous-web-maximum-find-pattern-characters*))
   :required '("ref_id" "pattern")))

(-> nous-web-run-parameters () json-object)
(defun nous-web-run-parameters ()
  "Return the provider-visible web.run schema used by Nous sessions."
  (let ((schema
          (web--object-schema
           (json-object
            "search_query"    (nous-web--array-schema
                                (nous-web--search-query-schema)
                                "Search the web through managed Firecrawl.")
            "image_query"     (nous-web--array-schema
                                (nous-web--search-query-schema)
                                "Search for images through managed Firecrawl.")
            "open"            (nous-web--array-schema
                                (nous-web--open-operation-schema)
                                "Scrape public URLs and return bounded page text.")
            "find"            (nous-web--array-schema
                                (nous-web--find-operation-schema)
                                "Find text in pages named by public URLs.")
            "response_length" (web--enum-schema
                                '("short" "medium" "long")
                                "Control result counts and page window sizes.")))))
    (setf (gethash "description" schema)
          "Supply at most four operations total across all command arrays.")
    schema))

(-> nous-web--provider-tool-schema (json-object) json-object)
(defun nous-web--provider-tool-schema (tool)
  "Return TOOL with Nous's exact managed web.run contract when applicable."
  (if (json-string= (json-get tool "name") "run")
      (let ((copy (json-object-copy tool)))
        (setf (gethash "description" copy) *nous-web-run-description*
              (gethash "parameters" copy) (nous-web-run-parameters))
        copy)
      tool))

(-> nous-web--provider-tool-namespaces (vector) vector)
(defun nous-web--provider-tool-namespaces (tool-namespaces)
  "Return TOOL-NAMESPACES with web.run narrowed to Nous gateway capabilities."
  (map 'vector
       (lambda (namespace)
         (if (and (json-object-p namespace)
                  (json-string= (json-get namespace "type") "namespace")
                  (json-string= (json-get namespace "name") "web")
                  (vectorp (json-get namespace "tools")))
             (let ((copy (json-object-copy namespace)))
               (setf (gethash "description" copy)
                     "Nous Tool Gateway managed web access."
                     (gethash "tools" copy)
                     (map 'vector
                          #'nous-web--provider-tool-schema
                          (json-get namespace "tools")))
               copy)
             namespace))
       tool-namespaces))

(defmethod provider-request-object :around
    ((provider nous-provider-mixin)
     (conversation conversation)
     (tool-namespaces vector)
     &key goal-context compaction-p)
  "Advertise the managed Firecrawl subset of web.run to Nous models."
  (call-next-method provider
                    conversation
                    (nous-web--provider-tool-namespaces tool-namespaces)
                    :goal-context goal-context
                    :compaction-p compaction-p))


;;;; -- Gateway Transport --

(-> nous-web--fail (string &rest t) nil)
(defun nous-web--fail (control &rest arguments)
  "Signal a web.run tool error formatted by CONTROL and ARGUMENTS."
  (error 'tool-error
         :message (apply #'format nil control arguments)
         :tool-name "web.run"))

(-> nous-web--utf8-byte-length (string) (integer 0))
(defun nous-web--utf8-byte-length (text)
  "Return the number of bytes required to encode TEXT as UTF-8."
  (length (sb-ext:string-to-octets text :external-format ':utf-8)))

(-> nous-web--encoded-output (json-object) string)
(defun nous-web--encoded-output (output)
  "Encode OUTPUT, rejecting a result larger than the managed output ceiling."
  (let ((encoded (json-encode output)))
    (when (> (nous-web--utf8-byte-length encoded)
             *nous-web-maximum-output-bytes*)
      (nous-web--fail
       "Nous Tool Gateway output exceeded ~:D bytes."
       *nous-web-maximum-output-bytes*))
    encoded))

(-> nous-web--request-headers (oauth-credentials) list)
(defun nous-web--request-headers (credentials)
  "Return managed Firecrawl request headers for CREDENTIALS."
  (openai-compatible--authenticated-headers
   credentials
   :accept "application/json"
   :content-type "application/json"))

(-> nous-web--response-body-string (t) string)
(defun nous-web--response-body-string (body)
  "Return bounded UTF-8 text from a gateway response BODY, closing streams."
  (labels ((too-large ()
             (nous-web--fail
              "Nous Tool Gateway response exceeded ~:D bytes."
              *nous-web-maximum-response-bytes*))

           (decode (octets)
             (when (> (length octets) *nous-web-maximum-response-bytes*)
               (too-large))
             (handler-case
                 (sb-ext:octets-to-string octets :external-format ':utf-8)
               (error ()
                 (nous-web--fail
                  "Nous Tool Gateway response was not valid UTF-8.")))))
    (cond
      ((null body)
       "")
       ((stringp body)
        (when (or (> (length body) *nous-web-maximum-response-bytes*)
                  (> (nous-web--utf8-byte-length body)
                     *nous-web-maximum-response-bytes*))
          (too-large))
        body)
      ((typep body '(vector (unsigned-byte 8)))
       (decode body))
      ((streamp body)
       (unwind-protect
             (if (subtypep (stream-element-type body) 'character)
                 (let* ((limit (1+ *nous-web-maximum-response-bytes*))
                        (characters (make-string limit))
                        (count (read-character-sequence characters body)))
                   (when (> count *nous-web-maximum-response-bytes*)
                     (too-large))
                   (nous-web--response-body-string
                    (subseq characters 0 count)))
                (let* ((limit (1+ *nous-web-maximum-response-bytes*))
                       (octets
                         (make-array limit
                                     :element-type '(unsigned-byte 8)))
                       (count (read-sequence octets body)))
                  (decode (subseq octets 0 count))))
         (ignore-errors (close body))))
      (t
       (nous-web--fail "Nous Tool Gateway returned an unreadable response body.")))))

(-> nous-web--http-request (string list string) (values string integer t))
(defun nous-web--http-request (url headers content)
  "POST CONTENT to URL and return a bounded string body, status, and headers."
  (multiple-value-bind (body status response-headers)
      (handler-case
          (dexador:post url
                        :headers headers
                        :content content
                        :force-binary t
                        :want-stream t
                        :keep-alive nil
                        :connect-timeout 30
                        :read-timeout 60)
        (http-request-failed (condition)
          (values (response-body condition)
                  (response-status condition)
                  (response-headers condition))))
    (values (nous-web--response-body-string body) status response-headers)))

(-> nous-web--status-message (integer) string)
(defun nous-web--status-message (status)
  "Return a bounded user-facing explanation for gateway HTTP STATUS."
  (case status
    (401
     "Nous Tool Gateway rejected the OAuth credentials; run autolith auth nous.")
    ((402 403)
     "The Nous account is not entitled to the requested Tool Gateway operation.")
    (429
     "Nous Tool Gateway rate-limited the request.")
    (t
     (format nil "Nous Tool Gateway returned HTTP ~D." status))))

(-> nous-web--request-document
    (string oauth-credentials json-object)
    json-object)
(defun nous-web--request-document (url credentials payload)
  "POST PAYLOAD to a managed gateway URL and return its validated JSON object."
  (multiple-value-bind (body status headers)
      (nous-web--http-request
       url
       (nous-web--request-headers credentials)
       (json-encode payload))
    (declare (ignore headers))
    (unless (= status 200)
      (nous-web--fail "~A" (nous-web--status-message status)))
    (let ((document
            (handler-case
                (json-decode body)
              (error ()
                (nous-web--fail
                 "Nous Tool Gateway returned malformed JSON.")))))
      (unless (json-object-p document)
        (nous-web--fail "Nous Tool Gateway returned a non-object response."))
      (multiple-value-bind (success present-p)
          (gethash "success" document)
        (when (and present-p (not success))
          (let ((detail (json-get document "error")))
            (nous-web--fail
             "Nous Tool Gateway refused the request~@[: ~A~]."
             (and (non-empty-string-p detail) detail)))))
      document)))


;;;; -- Command Validation --

(-> nous-web--absolute-url-p (t) boolean)
(defun nous-web--absolute-url-p (value)
  "Return true when VALUE is an absolute HTTP or HTTPS URL with a host."
  (if (and
       (non-empty-string-p value)
       (handler-case
           (let* ((uri (quri:uri value))
                  (scheme (quri:uri-scheme uri))
                  (host (quri:uri-host uri)))
             (and (stringp scheme)
                  (member scheme '("http" "https") :test #'string=)
                  (non-empty-string-p host)))
         (error ()
           nil)))
      t
      nil))

(-> nous-web--normalized-host (string) string)
(defun nous-web--normalized-host (host)
  "Return HOST in the form used by managed page-access policy checks."
  (string-downcase
   (string-right-trim
    '(#\.)
    (string-trim '(#\[ #\]) host))))

(-> nous-web--blocked-ipv4-address-p (vector) boolean)
(defun nous-web--blocked-ipv4-address-p (address)
  "Return true when IPv4 ADDRESS is private, local, reserved, or non-routable."
  (let ((first (aref address 0))
        (second (aref address 1))
        (third (aref address 2)))
    (if (or (= first 0)
            (= first 10)
            (and (= first 100) (<= 64 second 127))
            (= first 127)
            (and (= first 169) (= second 254))
            (and (= first 172) (<= 16 second 31))
            (and (= first 192) (or (= second 0) (= second 168)))
            (and (= first 198)
                 (or (<= 18 second 19)
                     (and (= second 51) (= third 100))))
            (and (= first 203) (= second 0) (= third 113))
            (>= first 224))
        t
        nil)))

(-> nous-web--blocked-ipv6-address-p (vector) boolean)
(defun nous-web--blocked-ipv6-address-p (address)
  "Return true when IPv6 ADDRESS is private, local, reserved, or non-routable."
  (let ((first (aref address 0))
        (second (aref address 1)))
    (cond
      ((and (every #'zerop (subseq address 0 10))
             (= (aref address 10) 255)
             (= (aref address 11) 255))
       (nous-web--blocked-ipv4-address-p (subseq address 12 16)))
      ((and (every #'zerop (subseq address 0 12))
            (nous-web--blocked-ipv4-address-p (subseq address 12 16)))
       t)
      ((every #'zerop address)
       t)
      ((and (every #'zerop (subseq address 0 15))
            (= (aref address 15) 1))
       t)
      ((member first '(#xfc #xfd #xff))
       t)
      ((and (= first #xfe)
            (member (logand second #xc0) '(#x80 #xc0)))
       t)
      ((and (= first #x20)
            (= second #x01)
            (= (aref address 2) #x0d)
            (= (aref address 3) #xb8))
       t)
      (t
       nil))))

(-> nous-web--blocked-network-address-p (t) boolean)
(defun nous-web--blocked-network-address-p (address)
  "Return true when resolved ADDRESS is outside public internet space."
  (cond
    ((and (vectorp address) (= (length address) 4))
     (nous-web--blocked-ipv4-address-p address))
    ((and (vectorp address) (= (length address) 16))
     (nous-web--blocked-ipv6-address-p address))
    (t
     t)))

(-> nous-web--blocked-hostname-p (string) boolean)
(defun nous-web--blocked-hostname-p (host)
  "Return true when HOST names a local or cloud-metadata endpoint."
  (if (or (member host *nous-web-blocked-hostnames* :test #'string=)
          (and (> (length host) (length ".localhost"))
               (string= host ".localhost"
                        :start1 (- (length host) (length ".localhost")))))
      t
      nil))

(-> nous-web--public-url-p (t) boolean)
(defun nous-web--public-url-p (value)
  "Return true when VALUE is an absolute HTTP(S) URL resolving only publicly."
  (if (and
       (nous-web--absolute-url-p value)
       (handler-case
           (let* ((uri (quri:uri value))
                  (host (nous-web--normalized-host (quri:uri-host uri))))
             (and (not (nous-web--blocked-hostname-p host))
                  (let ((addresses (usocket:get-hosts-by-name host)))
                    (and (consp addresses)
                         (every
                          (lambda (address)
                            (not (nous-web--blocked-network-address-p address)))
                          addresses)))))
         (error ()
           nil)))
      t
      nil))

(-> nous-web--response-length (json-object) string)
(defun nous-web--response-length (commands)
  "Return and validate COMMANDS' response length."
  (let ((value (or (json-get commands "response_length") "short")))
    (unless (and (stringp value)
                 (member value '("short" "medium" "long") :test #'string=))
      (nous-web--fail "Unknown response_length ~S." value))
    value))

(-> nous-web--search-limit (string) (integer 1))
(defun nous-web--search-limit (response-length)
  "Return the per-query result limit for RESPONSE-LENGTH."
  (cond
    ((string= response-length "short") 5)
    ((string= response-length "medium") 10)
    (t 20)))

(-> nous-web--page-limits (string) (values (integer 1) (integer 1)))
(defun nous-web--page-limits (response-length)
  "Return line and character ceilings for RESPONSE-LENGTH."
  (cond
    ((string= response-length "short")
     (values 80 12000))
    ((string= response-length "medium")
     (values 160 24000))
    (t
     (values 320 48000))))

(-> nous-web--operation-vector (json-object string) (option vector))
(defun nous-web--operation-vector (commands name)
  "Return COMMANDS' optional operation vector named NAME."
  (let ((value (json-get commands name)))
    (when value
      (unless (vectorp value)
        (nous-web--fail "~A must be an array." name))
      value)))

(-> nous-web--operation-count (json-object) (integer 0))
(defun nous-web--operation-count (commands)
  "Return the number of managed Firecrawl operations in COMMANDS."
  (loop for name in '("search_query" "image_query" "open" "find")
        for operations = (json-get commands name)
        when (vectorp operations)
          sum (length operations)))

(-> nous-web--validate-commands (json-object) null)
(defun nous-web--validate-commands (commands)
  "Reject command fields and batch sizes outside the managed gateway contract."
  (loop for name being the hash-keys of commands
        unless (member name *nous-web-supported-command-names* :test #'string=)
          do (nous-web--fail
              "The Nous Tool Gateway web backend does not support ~A."
              name))
  (let ((operation-count (nous-web--operation-count commands)))
    (unless (plusp operation-count)
      (nous-web--fail "The Nous Tool Gateway request contains no web operations."))
    (when (> operation-count *nous-web-maximum-operations*)
      (nous-web--fail
       "The Nous Tool Gateway accepts at most ~D operations per web.run call."
       *nous-web-maximum-operations*)))
  nil)


;;;; -- Firecrawl Search --

(-> nous-web--search-payload (json-object string (integer 1)) json-object)
(defun nous-web--search-payload (query source limit)
  "Return one Firecrawl search payload for QUERY, SOURCE, and LIMIT."
  (let ((text (json-get query "q"))
        (domains (json-get query "domains")))
    (unless (non-empty-string-p text)
      (nous-web--fail "Every Nous gateway search needs a non-empty q."))
    (when (> (length text) *nous-web-maximum-query-characters*)
      (nous-web--fail
       "Search q must not exceed ~D characters."
       *nous-web-maximum-query-characters*))
    (when domains
      (unless (and (vectorp domains)
                   (every #'non-empty-string-p domains))
        (nous-web--fail "Search domains must be non-empty strings."))
      (when (> (length domains) *nous-web-maximum-domain-count*)
        (nous-web--fail
         "A search accepts at most ~D domains."
         *nous-web-maximum-domain-count*))
      (loop for domain across domains
            when (> (length domain) *nous-web-maximum-domain-characters*)
              do (nous-web--fail
                  "Search domains must not exceed ~D characters."
                  *nous-web-maximum-domain-characters*)))
    (let ((payload
            (json-object
             "query" text
             "limit" limit
             "sources" (json-array source))))
      (when (and (vectorp domains) (plusp (length domains)))
        (setf (gethash "includeDomains" payload) domains))
      payload)))

(-> nous-web--validate-searches (vector string (integer 1)) null)
(defun nous-web--validate-searches (queries source limit)
  "Validate all Firecrawl QUERIES before any managed request is sent."
  (loop for query across queries
        do (unless (json-object-p query)
             (nous-web--fail "Every Nous gateway search must be an object."))
           (nous-web--search-payload query source limit))
  nil)

(-> nous-web--bounded-search-text (t (integer 1)) (option string))
(defun nous-web--bounded-search-text (value maximum-characters)
  "Return VALUE as bounded search text, or NIL when it is not a string."
  (and (stringp value)
       (subseq value 0 (min (length value) maximum-characters))))

(-> nous-web--normalize-search-result (t string) (option json-object))
(defun nous-web--normalize-search-result (result source)
  "Return one bounded Firecrawl RESULT for SOURCE, omitting nested payloads."
  (when (json-object-p result)
    (let ((normalized (json-object)))
      (labels ((copy-text (name maximum-characters)
                 (let ((value
                         (nous-web--bounded-search-text
                          (json-get result name)
                          maximum-characters)))
                   (when value
                     (setf (gethash name normalized) value))))

               (copy-url (name)
                 (let ((value (json-get result name)))
                   (when (and (nous-web--absolute-url-p value)
                              (<= (length value)
                                  *nous-web-maximum-url-characters*))
                     (setf (gethash name normalized) value))))

               (copy-integer (name)
                 (let ((value (json-get result name)))
                   (when (integerp value)
                     (setf (gethash name normalized) value)))))
        (copy-text "title" *nous-web-maximum-title-characters*)
        (copy-text "description" *nous-web-maximum-description-characters*)
        (copy-url "url")
        (copy-integer "position")
        (when (string= source "images")
          (copy-url "imageUrl")
          (copy-integer "imageWidth")
          (copy-integer "imageHeight")))
      (and (plusp (hash-table-count normalized)) normalized))))

(-> nous-web--search-results
    (json-object string (integer 1))
    vector)
(defun nous-web--search-results (document source limit)
  "Return at most LIMIT bounded SOURCE results from a Firecrawl DOCUMENT."
  (let* ((data (json-get document "data"))
         (results
           (or (and (json-object-p data)
                    (let ((values
                            (or (json-get data source)
                                (and (string= source "web")
                                     (json-get data "results")))))
                      (and (vectorp values) values)))
               (and (string= source "web") (vectorp data) data)
               (let ((values
                       (or (json-get document source)
                           (and (string= source "web")
                                (json-get document "results")))))
                 (and (vectorp values) values))
               (nous-web--fail
                "Nous Tool Gateway returned no ~A search results array."
                source)))
         (normalized nil))
    (loop for result across results
          while (< (length normalized) limit)
          for value = (nous-web--normalize-search-result result source)
          when value
            do (push value normalized))
    (coerce (nreverse normalized) 'vector)))

(-> nous-web--run-searches
    (vector string (integer 1) oauth-credentials)
    vector)
(defun nous-web--run-searches (queries source limit credentials)
  "Run Firecrawl QUERIES for SOURCE with LIMIT using CREDENTIALS."
  (map 'vector
       (lambda (query)
         (unless (json-object-p query)
           (nous-web--fail "Every Nous gateway search must be an object."))
         (let* ((payload (nous-web--search-payload query source limit))
                (document
                  (nous-web--request-document
                   (nous-firecrawl-search-endpoint)
                   credentials
                   payload)))
           (json-object
            "q" (json-get query "q")
             "results" (nous-web--search-results document source limit))))
       queries))


;;;; -- Firecrawl Page Access --

(-> nous-web--reported-scrape-urls (json-object) list)
(defun nous-web--reported-scrape-urls (data)
  "Return every non-empty effective URL reported in Firecrawl scrape DATA."
  (let ((metadata (json-get data "metadata")))
    (when (json-object-p metadata)
      (loop for name in '("sourceURL" "url")
            for value = (json-get metadata name)
            unless (or (null value)
                       (and (stringp value) (zerop (length value))))
              collect (progn
                        (unless (stringp value)
                          (nous-web--fail
                           "Nous Tool Gateway returned invalid page URL metadata."))
                        value)))))

(-> nous-web--reported-scrape-url (json-object) (option string))
(defun nous-web--reported-scrape-url (data)
  "Return the preferred effective URL reported in Firecrawl scrape DATA."
  (first (nous-web--reported-scrape-urls data)))

(-> nous-web--validate-page-operations (vector symbol) null)
(defun nous-web--validate-page-operations (operations kind)
  "Validate all page OPERATIONS of KIND before any managed request is sent."
  (loop for operation across operations
        do (unless (json-object-p operation)
             (nous-web--fail
              "Every Nous gateway page operation must be an object."))
            (let ((url (json-get operation "ref_id")))
              (unless (non-empty-string-p url)
                (nous-web--fail
                 "Nous gateway page operations require a public absolute HTTP(S) URL, not ~S."
                 url))
              (when (> (length url) *nous-web-maximum-url-characters*)
                (nous-web--fail
                 "Nous gateway page URLs must not exceed ~D characters."
                 *nous-web-maximum-url-characters*))
              (unless (nous-web--public-url-p url)
                (nous-web--fail
                 "Nous gateway page operations require a public absolute HTTP(S) URL, not ~S."
                 url)))
            (ecase kind
              (:open
               (let ((line (json-get operation "lineno")))
                 (when (and line
                            (not (and (integerp line) (plusp line))))
                   (nous-web--fail "open lineno must be a positive integer."))))
              (:find
               (let ((pattern (json-get operation "pattern")))
                 (unless (non-empty-string-p pattern)
                   (nous-web--fail "find pattern must be a non-empty string."))
                 (when (> (length pattern)
                          *nous-web-maximum-find-pattern-characters*)
                   (nous-web--fail
                    "find pattern must not exceed ~D characters."
                    *nous-web-maximum-find-pattern-characters*))))))
    nil)

(-> nous-web--scrape-data (string oauth-credentials) json-object)
(defun nous-web--scrape-data (url credentials)
  "Scrape URL through managed Firecrawl and return validated response data."
  (let* ((document
           (nous-web--request-document
            (nous-firecrawl-scrape-endpoint)
            credentials
            (json-object
             "url" url
             "formats" (json-array "markdown" "links")
             "onlyMainContent" t)))
         (raw-data (json-get document "data"))
         (data
           (cond
             ((json-object-p raw-data)
              raw-data)
             ((json-object-p document)
              document)
             (t
              (nous-web--fail
               "Nous Tool Gateway returned invalid scrape data.")))))
    (dolist (reported-url (nous-web--reported-scrape-urls data))
      (when (> (length reported-url) *nous-web-maximum-url-characters*)
        (nous-web--fail
         "Nous Tool Gateway reported a page URL exceeding ~D characters."
         *nous-web-maximum-url-characters*))
      (unless (nous-web--public-url-p reported-url)
        (nous-web--fail
         "Nous Tool Gateway redirected page access to a non-public URL.")))
    data))

(-> nous-web--scrape-markdown (json-object) string)
(defun nous-web--scrape-markdown (data)
  "Return the page markdown in Firecrawl scrape DATA."
  (let ((markdown (json-get data "markdown")))
    (unless (stringp markdown)
      (nous-web--fail "Nous Tool Gateway returned no page markdown."))
    markdown))

(-> nous-web--scrape-title (json-object) (option string))
(defun nous-web--scrape-title (data)
  "Return the best bounded page title in Firecrawl scrape DATA."
  (let ((metadata (json-get data "metadata")))
    (and (json-object-p metadata)
         (let ((title (json-get metadata "title")))
           (and (non-empty-string-p title)
                (nous-web--bounded-search-text
                 title *nous-web-maximum-title-characters*))))))

(-> nous-web--scrape-url (json-object string) string)
(defun nous-web--scrape-url (data requested-url)
  "Return Firecrawl's effective page URL or REQUESTED-URL."
  (or (nous-web--reported-scrape-url data) requested-url))

(-> nous-web--scrape-links (json-object (integer 1)) vector)
(defun nous-web--scrape-links (data limit)
  "Return at most LIMIT numbered absolute links from Firecrawl scrape DATA."
  (let ((links (json-get data "links"))
        (seen (make-hash-table :test #'equal))
        (results nil))
    (when (vectorp links)
      (loop for url across links
            when (and (nous-web--absolute-url-p url)
                      (<= (length url) *nous-web-maximum-url-characters*)
                      (not (gethash url seen))
                      (< (length results) limit))
              do (setf (gethash url seen) t)
                 (push (json-object "id" (1+ (length results)) "url" url)
                       results)))
    (coerce (nreverse results) 'vector)))

(-> nous-web--open-result
    (json-object json-object string)
    json-object)
(defun nous-web--open-result (operation data response-length)
  "Return one bounded open-page result for OPERATION and scrape DATA."
  (let* ((requested-url (json-get operation "ref_id"))
         (start-line (or (json-get operation "lineno") 1))
         (markdown (nous-web--scrape-markdown data))
         (lines (text--split-lines markdown)))
    (unless (and (integerp start-line) (plusp start-line))
      (nous-web--fail "open lineno must be a positive integer."))
    (when (> start-line (max 1 (length lines)))
      (nous-web--fail
       "open lineno ~D exceeds the page's ~D lines."
       start-line
       (length lines)))
    (multiple-value-bind (line-count character-limit)
        (nous-web--page-limits response-length)
      (multiple-value-bind (content ranges visible-end truncated-p)
          (text--numbered-line-window
           lines start-line line-count character-limit)
        (declare (ignore ranges))
        (json-object
         "requested_url" requested-url
         "url" (nous-web--scrape-url data requested-url)
         "title" (or (nous-web--scrape-title data) "")
         "start_line" start-line
         "end_line" visible-end
         "total_lines" (length lines)
         "truncated" (if (or truncated-p (< visible-end (length lines))) t false)
         "content" content
         "links" (nous-web--scrape-links data 50))))))

(-> nous-web--bounded-line (string) string)
(defun nous-web--bounded-line (line)
  "Return LINE capped for a compact find result."
  (subseq line 0 (min (length line) 500)))

(-> nous-web--find-result
    (json-object json-object string)
    json-object)
(defun nous-web--find-result (operation data response-length)
  "Return matching page lines for one find OPERATION and scrape DATA."
  (let* ((requested-url (json-get operation "ref_id"))
         (pattern (json-get operation "pattern"))
         (lines (text--split-lines (nous-web--scrape-markdown data)))
         (limit (cond
                  ((string= response-length "short") 10)
                  ((string= response-length "medium") 20)
                  (t 40)))
         (matches nil))
    (unless (non-empty-string-p pattern)
      (nous-web--fail "find pattern must be a non-empty string."))
    (loop for line across lines
          for number from 1
          when (search pattern line :test #'char-equal)
            do (push (json-object
                      "line" number
                      "text" (nous-web--bounded-line line))
                     matches)
          when (= (length matches) limit)
            do (return))
    (json-object
     "requested_url" requested-url
     "url" (nous-web--scrape-url data requested-url)
     "title" (or (nous-web--scrape-title data) "")
     "pattern" pattern
     "matches" (coerce (nreverse matches) 'vector))))


;;;; -- Provider Execution --

(defmethod provider-web-run
    ((provider nous-provider-mixin)
     (credentials oauth-credentials)
     (context tool-context)
     &key (commands (json-object)))
  "Run web.run through the Nous Tool Gateway's managed Firecrawl service."
  (declare (ignore provider context))
  (nous-web--validate-commands commands)
  (let* ((response-length (nous-web--response-length commands))
         (search-limit (nous-web--search-limit response-length))
         (web-searches (nous-web--operation-vector commands "search_query"))
         (image-searches (nous-web--operation-vector commands "image_query"))
         (opens (nous-web--operation-vector commands "open"))
         (finds (nous-web--operation-vector commands "find"))
         (scrape-cache (make-hash-table :test #'equal))
         (output (json-object)))
    (when web-searches
      (nous-web--validate-searches web-searches "web" search-limit))
    (when image-searches
      (nous-web--validate-searches image-searches "images" search-limit))
    (when opens
      (nous-web--validate-page-operations opens ':open))
    (when finds
      (nous-web--validate-page-operations finds ':find))
    (labels ((scrape (operation)
               (let ((url (json-get operation "ref_id")))
                 (multiple-value-bind (cached present-p)
                     (gethash url scrape-cache)
                   (if present-p
                       cached
                       (setf (gethash url scrape-cache)
                             (nous-web--scrape-data url credentials)))))))
      (when web-searches
        (setf (gethash "search_query" output)
              (nous-web--run-searches
               web-searches "web" search-limit credentials)))
      (when image-searches
        (setf (gethash "image_query" output)
              (nous-web--run-searches
               image-searches "images" search-limit credentials)))
      (when opens
        (setf (gethash "open" output)
              (map 'vector
                   (lambda (operation)
                     (nous-web--open-result
                      operation (scrape operation) response-length))
                   opens)))
      (when finds
        (setf (gethash "find" output)
              (map 'vector
                   (lambda (operation)
                     (nous-web--find-result
                      operation (scrape operation) response-length))
                   finds)))
      (tool-success (nous-web--encoded-output output)))))
