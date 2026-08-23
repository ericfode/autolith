(in-package #:autolith)

;;;; -- Nous Tool Gateway Test Support --

(-> nous-web-test--credentials (configuration) oauth-credentials)
(defun nous-web-test--credentials (configuration)
  "Return request-scoped credentials for managed gateway tests."
  (make-instance 'oauth-credentials
                 :access-token "nous-web-access-token"
                 :refresh-token "nous-web-refresh-token"
                 :id-token nil
                 :account-id "nous-web-account"
                 :expires-at nil
                 :source-path (configuration-nous-auth-path configuration)))

(-> nous-web-test--provider (configuration) nous-provider-mixin)
(defun nous-web-test--provider (configuration)
  "Return a Nous provider suitable for direct web protocol tests."
  (nous-provider--make configuration
                       (nous-credential-manager-create configuration)
                       "nous-web-test-session"))

(-> nous-web-test--context (configuration) tool-context)
(defun nous-web-test--context (configuration)
  "Return a tool context for managed gateway tests."
  (make-instance 'tool-context
                 :configuration configuration
                 :worker nil
                 :conversation
                 (conversation-create configuration
                                      :identifier "nous-web-test-conversation")))

(-> nous-web-test--object-keys (json-object) list)
(defun nous-web-test--object-keys (object)
  "Return OBJECT's sorted string keys."
  (sort (loop for key being the hash-keys of object collect key) #'string<))

(-> nous-web-test--tool-error-p (function string) boolean)
(defun nous-web-test--tool-error-p (function expected-text)
  "Return true when FUNCTION signals a tool error containing EXPECTED-TEXT."
  (handler-case
      (progn
        (funcall function)
        nil)
    (tool-error (condition)
      (if (test-object-contains-string-p condition expected-text) t nil))))

(-> nous-web-test--host-addresses (string) list)
(defun nous-web-test--host-addresses (host)
  "Return deterministic public or blocked addresses for URL-policy tests."
  (cond
    ((string= host "127.0.0.1")
     (list #(127 0 0 1)))
    ((string= host "169.254.169.254")
     (list #(169 254 169 254)))
     ((string= host "::1")
      (list #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
     ((string= host "::127.0.0.1")
      (list #(0 0 0 0 0 0 0 0 0 0 0 0 127 0 0 1)))
     (t
      (list #(93 184 216 34)))))


(-> nous-web-test--session-routing () null)
(defun nous-web-test--session-routing ()
  "Test web schema, credentials, and execution follow the session route."
  (let* ((base (test-configuration))
         (configuration
           (configuration--clone base :web-route ':nous))
         (context (nous-web-test--context configuration))
         (tool
           (make-instance 'web-run-tool
                          :namespace "web"
                          :name "run"
                          :description *web-run-description*
                          :parameters (web-run-parameters)))
         (credentials (nous-web-test--credentials configuration))
         (selected-provider nil)
         (selected-manager nil))
    (test-call-with-function-replacements
     (list
      (list 'provider-create
            (lambda (configuration)
              (declare (ignore configuration))
              (error "Inference provider creation must not serve an explicit Nous web route.")))
      (list 'call-with-credentials
            (lambda (manager function &key force-refresh)
              (declare (ignore force-refresh))
              (setf selected-manager manager)
              (funcall function credentials)))
      (list 'provider-web-run
            (lambda (provider credentials context &key commands)
              (declare (ignore credentials context commands))
              (setf selected-provider provider)
              (tool-success "routed"))))
     (lambda ()
       (let ((result (tool-execute tool context (json-object))))
         (test-assert
          (and (tool-result-success-p result)
               (typep selected-provider 'nous-provider-mixin)
               (typep selected-manager 'nous-credential-manager))
          "an explicit Nous web route uses Nous credentials and execution independently"))))
    (let* ((provider (provider-create configuration))
           (conversation (tool-context-conversation context))
           (namespace
             (json-object
              "type" "namespace"
              "name" "web"
              "description" "ordinary web"
              "tools"
              (json-array
               (json-object "name" "run"
                            "description" *web-run-description*
                            "parameters" (web-run-parameters)))))
           (encoded
             (json-encode
              (provider-request-object provider conversation
                                       (json-array namespace)))))
      (test-assert
       (and (search "Nous Tool Gateway managed web access" encoded)
            (search "image_query" encoded)
            (null (search "screenshot" encoded)))
       "Codex inference advertises the managed Firecrawl schema when routed to Nous")))
  nil)


;;;; -- Provider Schema and Configuration --

(-> nous-web-test--schema-and-configuration () null)
(defun nous-web-test--schema-and-configuration ()
  "Test managed gateway endpoints and the provider-visible Nous web schema."
  (let ((saved-gateway
          (uiop:getenv "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL")
           (test-assert
            (and (string= (nous-firecrawl-gateway-url)
                          "https://firecrawl-gateway.nousresearch.com")
                 (string= (nous-firecrawl-search-endpoint)
                          "https://firecrawl-gateway.nousresearch.com/v2/search")
                 (string= (nous-firecrawl-scrape-endpoint)
                          "https://firecrawl-gateway.nousresearch.com/v2/scrape"))
            "Nous managed Firecrawl uses the official gateway routes by default")
           (sb-posix:setenv "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL"
                            "https://gateway.nous.test/root/" 1)
           (test-assert
            (and (string= (nous-firecrawl-gateway-url)
                          "https://gateway.nous.test/root")
                 (string= (nous-firecrawl-search-endpoint)
                          "https://gateway.nous.test/root/v2/search")
                 (string= (nous-firecrawl-scrape-endpoint)
                          "https://gateway.nous.test/root/v2/scrape"))
            "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL overrides the gateway origin")
           (sb-posix:setenv "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL"
                            "http://gateway.nous.test" 1)
           (test-assert
            (handler-case
                (progn
                  (nous-firecrawl-gateway-url)
                  nil)
              (configuration-error (condition)
                (test-object-contains-string-p condition "must be an HTTPS URL")))
            "the managed gateway override rejects insecure HTTP origins"))
      (nous-provider-test--restore-environment
       "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL"
       saved-gateway)))
  (let* ((ordinary-parameters (web-run-parameters))
         (ordinary-properties (json-get ordinary-parameters "properties"))
         (ordinary-search
           (json-get (json-get ordinary-properties "search_query") "items"))
         (ordinary-search-properties (json-get ordinary-search "properties"))
         (ordinary-tool
           (json-object "name" "run"
                        "description" *web-run-description*
                        "parameters" ordinary-parameters))
         (namespace
           (json-object "type" "namespace"
                        "name" "web"
                        "description" "ordinary web"
                        "tools" (json-array ordinary-tool)))
         (transformed
           (first (coerce
                   (nous-web--provider-tool-namespaces (json-array namespace))
                   'list)))
         (nous-tool (aref (json-get transformed "tools") 0))
         (nous-parameters (json-get nous-tool "parameters"))
         (nous-properties (json-get nous-parameters "properties"))
         (nous-search
           (json-get (json-get nous-properties "search_query") "items"))
         (nous-open
           (json-get (json-get nous-properties "open") "items")))
    (test-assert
     (equal (nous-web-test--object-keys nous-properties)
            '("find" "image_query" "open" "response_length" "search_query"))
     "Nous advertises only managed Firecrawl web.run commands")
    (test-assert
     (and (null (json-get (json-get nous-search "properties") "recency"))
          (search "Public absolute HTTP(S) URL"
                  (json-get
                   (json-get (json-get nous-open "properties") "ref_id")
                   "description"))
          (= (json-get (json-get nous-properties "search_query") "maxItems")
             *nous-web-maximum-operations*)
          (= (json-get
              (json-get (json-get nous-search "properties") "q")
              "maxLength")
             *nous-web-maximum-query-characters*)
          (= (json-get
              (json-get (json-get nous-search "properties") "domains")
              "maxItems")
             *nous-web-maximum-domain-count*)
          (= (json-get
              (json-get
               (json-get (json-get nous-search "properties") "domains")
               "items")
              "maxLength")
             *nous-web-maximum-domain-characters*)
          (= (json-get
              (json-get (json-get nous-open "properties") "ref_id")
              "maxLength")
             *nous-web-maximum-url-characters*)
          (search "four operations total"
                  (json-get nous-parameters "description"))
          (search "subscription tool credits" (json-get nous-tool "description")))
     "Nous's provider schema describes its exact gateway contract and bounds")
    (test-assert
     (and (json-object-p (json-get ordinary-properties "click"))
          (json-object-p (json-get ordinary-search-properties "recency"))
          (eq (json-get ordinary-tool "parameters") ordinary-parameters))
     "narrowing the Nous schema does not mutate the ordinary web.run contract"))
  nil)


;;;; -- Managed Firecrawl Transport --

(-> nous-web-test--transport () null)
(defun nous-web-test--transport ()
  "Test managed search, image, scrape, bounded output, and shared scrape caching."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (saved-gateway
           (uiop:getenv "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL"))
         (provider (nous-web-test--provider configuration))
         (credentials (nous-web-test--credentials configuration))
         (context (nous-web-test--context configuration))
         (page-markdown
           (with-output-to-string (stream)
             (loop for number from 1 to 100
                   do (format stream
                              "line ~D~:[~; needle~]~%"
                              number
                              (member number '(2 90))))))
          (page-title
            (make-string (+ *nous-web-maximum-title-characters* 50)
                         :initial-element #\t))
          (overlong-link
            (concatenate
             'string
             "https://linked.test/"
             (make-string *nous-web-maximum-url-characters*
                          :initial-element #\l)))
         (requests nil))
    (unwind-protect
         (progn
           (sb-posix:setenv "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL"
                            "https://gateway.nous.test/" 1)
           (test-call-with-function-replacements
            (list
             (list 'usocket:get-hosts-by-name
                   #'nous-web-test--host-addresses)
             (list
              'dexador:post
              (lambda (url &key headers content &allow-other-keys)
                (let* ((payload (json-decode content))
                       (source-array (json-get payload "sources"))
                       (source (and (vectorp source-array)
                                    (plusp (length source-array))
                                    (aref source-array 0))))
                  (push (list :url url :headers headers :payload payload) requests)
                  (cond
                    ((string= url "https://gateway.nous.test/v2/search")
                     (cond
                       ((string= source "web")
                        (values
                         (json-encode
                          (json-object
                           "success" t
                           "data"
                           (json-object
                            "web"
                            (json-array
                              (json-object
                               "title" "Web result"
                               "url" "https://result.test/web"
                               "description"
                               (make-string
                                (+ *nous-web-maximum-description-characters* 50)
                                :initial-element #\d)
                               "markdown" (make-string 5000 :initial-element #\m)
                               "nested" (json-object "untrusted" "payload"))))))
                         200
                         nil))
                       ((string= source "images")
                        (values
                         (json-encode
                          (json-object
                           "success" t
                           "data"
                           (json-object
                            "images"
                            (json-array
                              (json-object
                               "title" "Image result"
                               "url" "https://result.test/image-page"
                               "imageUrl" "https://result.test/image.png"
                               "imageWidth" 640
                               "imageHeight" 480)))))
                         200
                         nil))
                       (t
                        (error "Unexpected Firecrawl source ~S." source))))
                    ((string= url "https://gateway.nous.test/v2/scrape")
                     (values
                      (json-encode
                       (json-object
                        "success" t
                        "data"
                        (json-object
                         "markdown" page-markdown
                         "links" (json-array
                                  "https://linked.test/one"
                                  "https://linked.test/one"
                                  "mailto:ignored@example.test"
                                  overlong-link)
                         "metadata"
                         (json-object
                          "title" page-title
                          "sourceURL" "https://example.test/final"))))
                      200
                      nil))
                    (t
                     (error "Unexpected managed gateway URL ~S." url)))))))
            (lambda ()
              (let* ((commands
                       (json-object
                        "search_query"
                        (json-array
                         (json-object
                          "q" "common lisp"
                          "domains" (json-array "lisp-lang.org")))
                        "image_query"
                        (json-array (json-object "q" "lambda calculus"))
                        "open"
                        (json-array
                         (json-object "ref_id" "https://example.test/page"))
                        "find"
                        (json-array
                         (json-object "ref_id" "https://example.test/page"
                                      "pattern" "needle"))
                        "response_length" "short"))
                     (result
                       (provider-web-run
                        provider credentials context :commands commands))
                     (output (json-decode (tool-result-content result)))
                     (open-result (aref (json-get output "open") 0))
                     (find-result (aref (json-get output "find") 0))
                     (web-result
                       (aref
                        (json-get (aref (json-get output "search_query") 0)
                                  "results")
                        0))
                     (ordered-requests (nreverse requests))
                     (search-request (first ordered-requests))
                     (image-request (second ordered-requests))
                     (scrape-request (third ordered-requests)))
                (test-assert
                 (and (tool-result-success-p result)
                      (= (length ordered-requests) 3)
                      (string= (getf search-request :url)
                               "https://gateway.nous.test/v2/search")
                      (string= (getf image-request :url)
                               "https://gateway.nous.test/v2/search")
                      (string= (getf scrape-request :url)
                               "https://gateway.nous.test/v2/scrape"))
                 "Nous web.run uses exact Firecrawl routes and shares one scrape")
                (dolist (request ordered-requests)
                  (test-assert
                   (string=
                    (nous-provider-test--header-value
                     "Authorization" (getf request :headers))
                    "Bearer nous-web-access-token")
                   "every managed gateway request uses the scoped Nous OAuth token"))
                (let ((search-payload (getf search-request :payload))
                      (image-payload (getf image-request :payload))
                      (scrape-payload (getf scrape-request :payload)))
                  (test-assert
                   (and (string= (json-get search-payload "query") "common lisp")
                        (= (json-get search-payload "limit") 5)
                        (equalp (json-get search-payload "sources")
                                (json-array "web"))
                        (equalp (json-get search-payload "includeDomains")
                                (json-array "lisp-lang.org")))
                   "web search maps sources and domain filters to Firecrawl")
                  (test-assert
                   (equalp (json-get image-payload "sources")
                           (json-array "images"))
                   "image search selects Firecrawl's images source")
                  (test-assert
                   (and (string= (json-get scrape-payload "url")
                                 "https://example.test/page")
                        (equalp (json-get scrape-payload "formats")
                                (json-array "markdown" "links"))
                        (eq (json-get scrape-payload "onlyMainContent") t))
                   "page access sends the bounded Firecrawl scrape contract"))
                (test-assert
                 (and (= (length (json-get web-result "description"))
                         *nous-web-maximum-description-characters*)
                      (null (json-get web-result "markdown"))
                      (null (json-get web-result "nested")))
                 "search results expose only bounded scalar fields")
                (test-assert
                 (and (= (json-get open-result "start_line") 1)
                      (= (json-get open-result "end_line") 80)
                      (eq (json-get open-result "truncated") t)
                      (search "     1  line 1" (json-get open-result "content"))
                      (= (length (json-get open-result "title"))
                         *nous-web-maximum-title-characters*)
                      (= (length (json-get open-result "links")) 1))
                 "open returns a numbered bounded page window with safe metadata")
                (test-assert
                 (and (string= (json-get find-result "pattern") "needle")
                      (= (length (json-get find-result "matches")) 2)
                      (= (length (json-get find-result "title"))
                         *nous-web-maximum-title-characters*)
                      (= (json-get (aref (json-get find-result "matches") 0)
                                   "line")
                         2))
                 "find returns bounded case-insensitive page matches")))))
      (nous-provider-test--restore-environment
       "AUTOLITH_NOUS_FIRECRAWL_GATEWAY_URL"
       saved-gateway)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)


;;;; -- Validation and Failure Mapping --

(-> nous-web-test--validation-and-failures () null)
(defun nous-web-test--validation-and-failures ()
  "Test preflight rejection and managed gateway response failures without network."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (provider (nous-web-test--provider configuration))
         (credentials (nous-web-test--credentials configuration))
         (context (nous-web-test--context configuration))
         (request-count 0))
    (unwind-protect
         (test-call-with-function-replacements
          (list
            (list 'usocket:get-hosts-by-name
                  #'nous-web-test--host-addresses)
           (list 'dexador:post
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (incf request-count)
                   (error "Preflight validation reached the network."))))
          (lambda ()
            (dolist
                (case
                 (list
                   (list
                    (json-object
                     "search_query"
                     (json-array (json-object "q" "valid")
                                 (json-object "q" "")))
                    "non-empty q")
                   (list
                    (json-object
                     "open" (json-array (json-object "ref_id" "https://")))
                    "absolute HTTP(S) URL")
                   (list
                    (json-object
                     "open"
                     (json-array
                      (json-object "ref_id" "http://127.0.0.1/private")))
                    "public absolute HTTP(S) URL")
                   (list
                    (json-object
                     "open"
                     (json-array
                      (json-object "ref_id" "http://[::1]/private")))
                    "public absolute HTTP(S) URL")
                   (list
                    (json-object
                     "open"
                     (json-array
                      (json-object
                       "ref_id" "http://[::127.0.0.1]/private")))
                    "public absolute HTTP(S) URL")
                   (list
                    (json-object
                     "search_query"
                     (json-array
                      (json-object
                       "q" (make-string
                            (1+ *nous-web-maximum-query-characters*)
                            :initial-element #\q))))
                    "Search q must not exceed")
                   (list
                    (json-object
                     "search_query"
                     (json-array
                      (json-object
                       "q" "valid"
                       "domains"
                       (coerce
                        (loop repeat (1+ *nous-web-maximum-domain-count*)
                              collect "example.test")
                        'vector))))
                    "accepts at most 20 domains")
                   (list
                    (json-object
                     "search_query"
                     (json-array
                      (json-object
                       "q" "valid"
                       "domains"
                       (json-array
                        (make-string
                         (1+ *nous-web-maximum-domain-characters*)
                         :initial-element #\d)))))
                    "Search domains must not exceed")
                   (list
                    (json-object
                     "open"
                     (json-array
                      (json-object
                       "ref_id"
                       (concatenate
                        'string
                        "https://example.test/"
                        (make-string *nous-web-maximum-url-characters*
                                     :initial-element #\u)))))
                    "page URLs must not exceed")
                   (list
                    (json-object
                     "find"
                     (json-array
                      (json-object "ref_id" "https://example.test"
                                   "pattern" "")))
                    "non-empty string")
                   (list
                    (json-object
                     "find"
                     (json-array
                      (json-object
                       "ref_id" "https://example.test"
                       "pattern"
                       (make-string
                        (1+ *nous-web-maximum-find-pattern-characters*)
                        :initial-element #\p))))
                    "find pattern must not exceed")
                   (list
                    (json-object
                     "click" (json-array (json-object "ref_id" "turn0search0")))
                    "does not support click")
                   (list
                    (json-object
                     "search_query"
                     (json-array (json-object "q" "one")
                                 (json-object "q" "two")
                                 (json-object "q" "three"))
                     "image_query"
                     (json-array (json-object "q" "four")
                                 (json-object "q" "five")))
                    "at most 4 operations")
                   (list
                    (json-object
                     "search_query" (json-array (json-object "q" "valid"))
                     "response_length" 7)
                    "Unknown response_length")))
              (destructuring-bind (commands expected-text) case
                (test-assert
                 (nous-web-test--tool-error-p
                  (lambda ()
                     (provider-web-run
                      provider credentials context :commands commands))
                  expected-text)
                 (format nil "Nous web.run rejects invalid input containing ~S"
                         expected-text))))
            (test-assert (= request-count 0)
                         "Nous web.run validates the full command batch before I/O")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (provider (nous-web-test--provider configuration))
         (credentials (nous-web-test--credentials configuration))
         (context (nous-web-test--context configuration)))
    (unwind-protect
          (labels ((request-signals (body status expected-text)
                     (test-call-with-function-replacements
                      (list
                       (list 'dexador:post
                             (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (values body status nil))))
                      (lambda ()
                        (nous-web-test--tool-error-p
                         (lambda ()
                           (nous-web--request-document
                            "https://gateway.nous.test/v2/search"
                            credentials
                            (json-object "query" "test")))
                         expected-text))))

                   (redirect-signals (metadata)
                     (test-call-with-function-replacements
                      (list
                       (list 'usocket:get-hosts-by-name
                             #'nous-web-test--host-addresses)
                       (list 'dexador:post
                             (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (values
                                (json-encode
                                 (json-object
                                  "success" t
                                  "data"
                                  (json-object
                                   "markdown" "private redirect"
                                   "metadata" metadata)))
                                200
                                nil))))
                      (lambda ()
                        (nous-web-test--tool-error-p
                         (lambda ()
                           (provider-web-run
                            provider
                            credentials
                            context
                            :commands
                            (json-object
                             "open"
                             (json-array
                              (json-object
                               "ref_id" "https://example.test/page")))))
                         "redirected page access to a non-public URL"))))

                   (output-limit-signals ()
                     (test-call-with-function-replacements
                      (list
                       (list 'dexador:post
                             (lambda (&rest arguments)
                               (declare (ignore arguments))
                               (values
                                (json-encode
                                 (json-object
                                  "success" t
                                  "data"
                                  (json-object
                                   "web"
                                   (json-array
                                    (json-object
                                     "title" "Result"
                                     "url" "https://result.test/page")))))
                                200
                                nil))))
                      (lambda ()
                        (let ((*nous-web-maximum-output-bytes* 16))
                          (nous-web-test--tool-error-p
                           (lambda ()
                             (provider-web-run
                              provider
                              credentials
                              context
                              :commands
                              (json-object
                               "search_query"
                               (json-array (json-object "q" "test")))))
                           "output exceeded"))))))
            (dolist (case '((401 "OAuth credentials")
                            (402 "not entitled")
                            (403 "not entitled")
                            (429 "rate-limited")))
              (destructuring-bind (status expected-text) case
                (test-assert
                 (request-signals "{}" status expected-text)
                 (format nil "Nous gateway HTTP ~D is classified" status))))
            (test-assert
             (request-signals "{" 200 "malformed JSON")
             "Nous gateway malformed JSON is a bounded tool error")
            (test-assert
             (request-signals
              "{\"success\":false,\"error\":\"denied\"}"
              200
              "refused the request")
             "Nous gateway unsuccessful JSON is a bounded tool error")
            (test-assert
             (request-signals
              (make-string (1+ *nous-web-maximum-response-bytes*)
                           :initial-element #\x)
              200
              "response exceeded")
             "Nous gateway response bodies have a hard byte ceiling")
            (test-assert
             (let ((*nous-web-maximum-response-bytes* 3))
               (nous-web-test--tool-error-p
                (lambda ()
                  (nous-web--response-body-string
                   (make-string-input-stream
                    (coerce (list (code-char #xE9) (code-char #xE9))
                            'string))))
                "response exceeded"))
             "character response streams are bounded by encoded UTF-8 bytes")
            (test-assert
             (output-limit-signals)
             "the complete encoded web.run output has a hard byte ceiling")
            (test-assert
             (redirect-signals
              (json-object
               "sourceURL" "https://example.test/public"
               "url" "http://127.0.0.1/private"))
             "every reported Firecrawl redirect URL is checked")
            (test-assert
             (redirect-signals
              (json-object
               "sourceURL" ""
               "url" "http://127.0.0.1/private"))
             "an empty sourceURL falls back to the reported Firecrawl url"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-nous-web () null)
(defun test-nous-web ()
  "Run Nous Tool Gateway web access tests."
  (nous-web-test--session-routing)
  (nous-web-test--schema-and-configuration)
  (nous-web-test--transport)
  (nous-web-test--validation-and-failures)
  nil)
