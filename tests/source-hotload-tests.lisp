(in-package #:autolith)

;;;; -- Source Hot-Load Fixtures --

(-> test-source-hotload-first () keyword)
(defun test-source-hotload-first ()
  "Return the baseline value used by source hot-load tests."
  ':baseline)

(defparameter *test-source-hotload-value* ':baseline
  "The baseline variable used by source hot-load tests.")

(defvar *test-source-hotload-stable-value* ':baseline
  "The non-reloadable variable used by source hot-load rejection tests.")

(-> test-source-hotload-other () keyword)
(defun test-source-hotload-other ()
  "Return the second-file baseline used by source-boundary tests."
  ':baseline)

(-> test-source-hotload-baseline-source () string)
(defun test-source-hotload-baseline-source ()
  "Return the tracked source fixture represented by the running test image."
  (format nil
          "(in-package #:autolith)~2%~
           (defun test-source-hotload-first ()~%~
           ~2T\"Return the baseline fixture value.\"~%~
           ~2T:baseline)~2%~
           (defparameter *test-source-hotload-value* :baseline~%~
           ~2T\"The source hot-load fixture value.\")~2%~
           (defvar *test-source-hotload-stable-value* :baseline)~%"))

(-> test-source-hotload-target-source
    (&key (:first keyword) (:value t) (:side-effect-p boolean)
          (:package-name symbol))
    string)
(defun test-source-hotload-target-source
    (&key (first ':target) (value ':target) side-effect-p
          (package-name '#:autolith))
  "Return one changed tracked source fixture."
  (format nil
          "(in-package ~S)~2%~
           (defun test-source-hotload-first ()~%~
           ~2T\"Return the changed fixture value.\"~%~
           ~2T~S)~2%~
           (defparameter *test-source-hotload-value* ~S~%~
           ~2T\"The changed source hot-load fixture value.\")~2%~
           (defvar *test-source-hotload-stable-value* :baseline)~@[~2%~
           (eval-when (:load-toplevel) (print :side-effect))~]~%"
          package-name first value side-effect-p))

(defparameter *test-source-hotload-command-metadata*
  '(:name "/source-hotload"
    :aliases ("/source-hotload-alias")
    :argument nil
    :description "Source hot-load command fixture"
    :tip "Source hot-load command fixture tip."
    :busy-behavior :inspect
    :terminal-behavior :shared
    :call-lambda-list ()
    :slash-argument-mode :none)
  "Complete application command metadata used by source shape tests.")

(-> test-source-hotload-shape-source
    (&key (:generic-options list) (:command-metadata list)
          (:command-lambda-list list))
    string)
(defun test-source-hotload-shape-source
    (&key
       (generic-options '((:documentation "Source hot-load generic fixture.")))
       (command-metadata *test-source-hotload-command-metadata*)
       (command-lambda-list '(application)))
  "Return source fixtures for structural definition rejection tests."
  (let ((generic
          (append '(defgeneric test-source-hotload-generic (value))
                  generic-options))
        (command
          (list 'define-application-command
                'test-source-hotload-command
                command-metadata
                command-lambda-list
                '(declare (ignore application))
                ':continue)))
    (format nil "(in-package #:autolith)~2%~S~2%~S~%"
            generic command)))

(-> test-source-hotload-plist-replace (list keyword t) list)
(defun test-source-hotload-plist-replace (plist key value)
  "Return a copy of PLIST with KEY replaced by VALUE."
  (let ((result (copy-list plist)))
    (setf (getf result key) value)
    result))

(-> test-source-hotload-write (pathname string) pathname)
(defun test-source-hotload-write (pathname source)
  "Write exact fixture SOURCE to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string source stream))
  pathname)

(-> test-source-hotload-install (string) null)
(defun test-source-hotload-install (source)
  "Evaluate supported fixture definitions from SOURCE without journaling them."
  (dolist (source-form (source-read-forms source))
    (let ((form (source-form-form source-form)))
      (when (definition-form-p form)
        (eval form))))
  nil)

(-> test-source-hotload-git (configuration list) string)
(defun test-source-hotload-git (configuration arguments)
  "Run one Git fixture command."
  (self-git-command configuration arguments))

(-> test-call-with-source-hotload-fixture (function) t)
(defun test-call-with-source-hotload-fixture (function)
  "Call FUNCTION with an isolated clean Git source baseline and image lineage."
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-source-hotload-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (source-pathname (merge-pathnames "src/hotload.lisp" root))
         (other-pathname (merge-pathnames "src/other.lisp" root))
         (configuration nil)
         (baseline nil)
         (previous-first (symbol-function 'test-source-hotload-first))
         (previous-other (symbol-function 'test-source-hotload-other))
         (previous-value *test-source-hotload-value*)
         (previous-state-initialized-p *image-state-initialized-p*)
         (previous-commit-identifier *active-image-commit-identifier*)
         (previous-history-commit *active-image-history-commit*)
         (previous-lineage-identifier *active-image-lineage-identifier*)
         (first-target
           (definition-key '(defun test-source-hotload-first () :baseline)))
         (value-target
           (definition-key
            '(defparameter *test-source-hotload-value* :baseline)))
         (other-target
           (definition-key '(defun test-source-hotload-other () :baseline)))
         (cached-first nil)
         (cached-first-p nil)
         (cached-value nil)
         (cached-value-p nil)
         (cached-other nil)
         (cached-other-p nil))
    (multiple-value-setq (cached-first cached-first-p)
      (gethash first-target *exploratory-definitions*))
    (multiple-value-setq (cached-value cached-value-p)
      (gethash value-target *exploratory-definitions*))
    (multiple-value-setq (cached-other cached-other-p)
      (gethash other-target *exploratory-definitions*))
    (unwind-protect
         (progn
           (test-source-hotload-write source-pathname
                                      (test-source-hotload-baseline-source))
           (test-source-hotload-write
            other-pathname
            (format nil
                    "(in-package #:autolith)~2%~
                     (defun test-source-hotload-other () :baseline)~%"))
           (test-source-hotload-write
            (merge-pathnames "src/shapes.lisp" root)
            (test-source-hotload-shape-source))
           (test-source-hotload-write
            (merge-pathnames "src/state/source-hotload.lisp" root)
            (format nil
                    "(in-package #:autolith)~2%~
                     (defun test-source-hotload-other () :baseline)~%"))
           (test-source-hotload-write
            (merge-pathnames "autolith.asd" root)
            (format nil "(asdf:defsystem #:fixture)~%"))
           (test-source-hotload-write (merge-pathnames "qlfile" root)
                                      (format nil "~%"))
           (test-source-hotload-write (merge-pathnames "qlfile.lock" root)
                                      (format nil "~%"))
           (test-source-hotload-write (merge-pathnames "sbcl.version" root)
                                      (format nil "2.6.6~%"))
           (setf configuration (test-configuration-for-source-root root))
           (test-source-hotload-git configuration '("init" "--quiet"))
           (test-source-hotload-git
            configuration
            '("config" "user.name" "Autolith Source Hotload Tests"))
           (test-source-hotload-git
            configuration
            '("config" "user.email" "autolith-test@example.invalid"))
           (test-source-hotload-git configuration '("add" "."))
           (test-source-hotload-git
            configuration
            '("commit" "--quiet" "--no-gpg-sign" "-m" "Create baseline"))
           (setf baseline
                 (string-trim
                  '(#\Space #\Tab #\Newline #\Return)
                  (test-source-hotload-git configuration '("rev-parse" "HEAD"))))
           (test-source-hotload-install (test-source-hotload-baseline-source))
           (remhash first-target *exploratory-definitions*)
           (remhash value-target *exploratory-definitions*)
           (remhash other-target *exploratory-definitions*)
           (setf *image-state-initialized-p* nil
                 *active-image-commit-identifier* nil
                 *active-image-history-commit* nil
                 *active-image-lineage-identifier* nil)
           (let ((*active-image-build-record*
                   (list :active-image-build
                         :version 1
                         :source-commit baseline
                         :source-clean-p t))
                 (*source-hotload-replay-root* root)
                 (*image-commit-replay-probe-function*
                   (lambda (checked-configuration script identifier)
                     (declare (ignore checked-configuration script identifier))
                     nil)))
             (image-state-load configuration)
             (funcall function
                      configuration root source-pathname other-pathname baseline)))
      (setf (symbol-function 'test-source-hotload-first) previous-first
            (symbol-function 'test-source-hotload-other) previous-other
            *test-source-hotload-value* previous-value
            *image-state-initialized-p* previous-state-initialized-p
            *active-image-commit-identifier* previous-commit-identifier
            *active-image-history-commit* previous-history-commit
            *active-image-lineage-identifier* previous-lineage-identifier)
      (if cached-first-p
          (setf (gethash first-target *exploratory-definitions*) cached-first)
          (remhash first-target *exploratory-definitions*))
      (if cached-value-p
          (setf (gethash value-target *exploratory-definitions*) cached-value)
          (remhash value-target *exploratory-definitions*))
      (if cached-other-p
          (setf (gethash other-target *exploratory-definitions*) cached-other)
          (remhash other-target *exploratory-definitions*))
      (when (probe-file root)
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore)))))

(-> test-source-hotload-checker (&optional function) mutation-checker)
(defun test-source-hotload-checker (&optional function)
  "Return a callback checker invoking optional FUNCTION."
  (make-instance
   'callback-mutation-checker
   :active-callback
   (or function
       (lambda (configuration source)
         (declare (ignore configuration source))
         "source hot-load checks passed"))))


;;;; -- Source Hot-Load Behavior --

(-> test-source-hotload-installation-and-replay () null)
(defun test-source-hotload-installation-and-replay ()
  "Test uncommitted installation, provenance, persistence, and replay policy."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (declare (ignore root other-pathname))
     (let* ((target-source (test-source-hotload-target-source))
            (head-before baseline)
            (commit nil)
            (count nil)
            (source-states nil))
       (test-source-hotload-write source-pathname target-source)
       (multiple-value-setq (commit count source-states)
         (source-hotload-apply
          configuration
          (test-source-hotload-checker)
          '("src/hotload.lisp")
          :title "Load source-backed definitions"))
       (test-assert (= count 2)
                    "source hot loading installs every changed definition")
       (test-assert (equal source-states '(:uncommitted))
                    "source hot loading records uncommitted target state")
       (test-assert (and (eq (test-source-hotload-first) ':target)
                         (eq *test-source-hotload-value* ':target))
                    "source hot loading mutates the running active image")
       (test-assert
        (string= head-before
                 (source-hotload--commit configuration "HEAD"))
        "source hot loading does not commit or rewrite tracked Git source")
       (test-assert (string= (uiop:read-file-string source-pathname)
                             target-source)
                    "source hot loading leaves exact tracked source untouched")
       (let* ((loaded (image-commit-load
                       configuration
                       (image-commit-identifier commit)
                       :history-commit (image-commit-history-commit commit)))
              (entries (image-commit-entries loaded))
              (first-entry
                (find (definition-key
                       '(defun test-source-hotload-first () :baseline))
                      entries
                      :key (lambda (entry) (getf entry :target))
                      :test #'string=)))
         (test-assert
          (and (= (length entries) 2)
               (every (lambda (entry)
                        (source-hotload-provenance-p
                         (getf entry :source-provenance)))
                      entries))
          "private image entries retain complete source provenance")
         (test-assert
          (and (string= (getf (getf first-entry :source-provenance)
                              :baseline-commit)
                        baseline)
               (string= (getf (getf first-entry :source-provenance)
                              :relative-pathname)
                        "src/hotload.lisp")
               (non-empty-string-p
                (getf (getf first-entry :source-provenance)
                      :base-file-blob))
               (string=
                (getf (getf first-entry :source-provenance)
                      :target-file-blob)
                (source-hotload--source-blob configuration target-source)))
          "source provenance retains commits, pathname, and file blobs")
         (let ((wrong-target (copy-tree first-entry))
               (wrong-package (copy-tree first-entry))
               (wrong-source (copy-tree first-entry))
               (wrong-base (copy-tree first-entry))
               (missing-base (copy-tree first-entry)))
           (setf (getf wrong-target :target) "AUTOLITH::OTHER-TARGET"
                 (getf wrong-package :package) "CL-USER"
                 (getf wrong-source :source)
                 "(defun test-source-hotload-first () :other)"
                 (getf (getf wrong-base :source-provenance) :base-source)
                 "(defun test-source-hotload-first () :wrong-base)"
                 (getf (getf missing-base :source-provenance)
                       :base-present-p)
                 nil
                 (getf (getf missing-base :source-provenance) :base-source)
                 nil
                 (getf (getf missing-base :source-provenance) :base-identity)
                 nil)
           (test-assert
            (every (lambda (entry)
                     (not (image-commit--entry-p entry)))
                   (list wrong-target
                         wrong-package
                         wrong-source
                         wrong-base
                         missing-base))
            "manifest validation rejects inconsistent source overlay entries"))
         (test-assert
          (search "self-replay-source-definition"
                  (uiop:read-file-string
                   (image-commit-script-pathname loaded)))
          "private reconstruction scripts use source-aware replay")
         (test-assert
          (find-if
           (lambda (record)
             (and (eq (first record) ':mutation)
                  (eq (getf (rest record) :kind) ':source-load)
                  (eq (getf (rest record) :result) ':committed)
                  (string= (getf (rest record) :image-commit)
                           (image-commit-identifier commit))))
           (mutation-journal-read-records configuration))
          "source hot loading journals selected private image publication")
         (test-assert
          (eq (self-replay-source-definition
               (getf first-entry :package)
               (getf first-entry :source)
               (getf first-entry :source-provenance))
              ':suppressed)
          "private replay suppresses an overlay already present in tracked source")
         (test-source-hotload-write source-pathname
                                    (test-source-hotload-baseline-source))
         (test-source-hotload-install (test-source-hotload-baseline-source))
         (load (image-commit-script-pathname loaded))
         (test-assert (and (eq (test-source-hotload-first) ':target)
                           (eq *test-source-hotload-value* ':target))
                      "private replay applies overlays while tracked source is at base")
         (let ((divergent-source
                 (test-source-hotload-target-source
                  :first ':divergent
                  :value ':baseline)))
           (test-source-hotload-write source-pathname divergent-source)
           (test-source-hotload-install divergent-source)
           (let ((conflict nil))
             (handler-case
                 (self-replay-source-definition
                  (getf first-entry :package)
                  (getf first-entry :source)
                  (getf first-entry :source-provenance))
               (source-overlay-conflict (condition)
                 (setf conflict condition)))
             (test-assert
              (and conflict
                   (eq (source-hotload-error-reason conflict)
                       ':definition-diverged)
                   (non-empty-string-p
                    (source-overlay-conflict-actual-identity conflict))
                   (eq (test-source-hotload-first) ':divergent))
              "divergent tracked source fails closed with structured identities"))
           (test-assert
            (eq
             (handler-bind
                 ((source-overlay-conflict
                    (lambda (condition)
                      (invoke-restart
                       (find-restart 'skip-source-overlay condition)))))
               (self-replay-source-definition
                (getf first-entry :package)
                (getf first-entry :source)
                (getf first-entry :source-provenance)))
             ':skipped)
             "divergent replay offers an explicit skip-source-overlay restart"))))))
  nil)

(-> test-source-hotload-committed-source () null)
(defun test-source-hotload-committed-source ()
  "Test source-backed installation from an ordinary committed source change."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (declare (ignore root other-pathname baseline))
     (test-source-hotload-write source-pathname
                                (test-source-hotload-target-source))
     (test-source-hotload-git configuration '("add" "src/hotload.lisp"))
     (test-source-hotload-git
      configuration
      '("commit" "--quiet" "--no-gpg-sign" "-m" "Change definitions"))
     (multiple-value-bind (commit count states)
         (source-hotload-apply
          configuration
          (test-source-hotload-checker)
          '("src/hotload.lisp")
          :title "Load committed source definitions")
       (test-assert (and commit (= count 2) (equal states '(:committed)))
                    "committed ordinary source can be installed and published")
       (test-assert (eq (test-source-hotload-first) ':target)
                    "committed source changes take effect without rebuilding"))))
  nil)

(-> test-source-hotload-atomic-rollback () null)
(defun test-source-hotload-atomic-rollback ()
  "Test rollback after installation, checks, publication, and source races."
  (dolist (failure '(:installation :check :publication
                     :source-snapshot :source-boundary-snapshot))
    (test-call-with-source-hotload-fixture
     (lambda (configuration root source-pathname other-pathname baseline)
       (declare (ignore root baseline))
       (test-source-hotload-write
        source-pathname
        (if (eq failure ':installation)
            (test-source-hotload-target-source
             :value '(error "Injected source initialization failure."))
            (test-source-hotload-target-source)))
       (let ((checker
               (if (eq failure ':check)
                   (test-source-hotload-checker
                    (lambda (checked-configuration source)
                      (declare (ignore checked-configuration source))
                      (error "Injected source checker failure.")))
                   (test-source-hotload-checker)))
             (*image-commit-replay-probe-function*
               (case failure
                 (:publication
                  (lambda (checked-configuration script identifier)
                    (declare (ignore checked-configuration script identifier))
                    (error "Injected replay probe failure.")))
                 (:source-snapshot
                  (lambda (checked-configuration script identifier)
                    (declare (ignore checked-configuration script identifier))
                    (test-source-hotload-write
                     source-pathname
                     (test-source-hotload-target-source
                      :first ':raced
                      :value ':raced))
                    nil))
                 (:source-boundary-snapshot
                  (lambda (checked-configuration script identifier)
                    (declare (ignore checked-configuration script identifier))
                    (test-source-hotload-write
                     other-pathname
                     (format nil
                             "(in-package #:autolith)~2%~
                              (defun test-source-hotload-other () :raced)~%"))
                    nil))
                 (otherwise
                  (lambda (checked-configuration script identifier)
                    (declare (ignore checked-configuration script identifier))
                    nil)))))
         (test-assert
          (handler-case
              (progn
                (source-hotload-apply
                 configuration checker '("src/hotload.lisp")
                 :title "Reject source-backed definitions")
                nil)
            (error ()
              t))
          "source hot-load transaction propagates its failure")
         (test-assert
          (and (eq (test-source-hotload-first) ':baseline)
               (eq *test-source-hotload-value* ':baseline))
          "failed source hot-load batches restore every installed definition")
         (test-assert (null (image-commit-current configuration))
                      "failed source hot-load batches do not select a private commit")
         (test-assert (null (image-commit-pending-records configuration))
                      "failed source hot-load batches discard installed journal records")))))
  nil)

(-> test-source-hotload-protected-kernel () null)
(defun test-source-hotload-protected-kernel ()
  "Test source transactions reject their implementation before installation."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (declare (ignore source-pathname other-pathname baseline))
     (let* ((protected-pathname
              (merge-pathnames "src/state/source-hotload.lisp" root))
            (previous-function
              (symbol-function 'test-source-hotload-other))
            (previous-value *test-source-hotload-value*)
            (previous-active-identifier *active-image-commit-identifier*)
            (previous-active-history *active-image-history-commit*)
            (journal (configuration-journal-path configuration))
            (previous-journal
              (and (probe-file journal) (uiop:read-file-string journal)))
            (reason nil))
       (test-source-hotload-write
        protected-pathname
        (format nil
                "(in-package #:autolith)~2%~
                 (defun test-source-hotload-other () :replaced-kernel)~%"))
       (setf reason
             (handler-case
                 (progn
                   (source-hotload-apply
                    configuration
                    (test-source-hotload-checker)
                    '("src/state/source-hotload.lisp")
                    :title "Reject transaction replacement")
                   nil)
               (source-hotload-error (condition)
                 (source-hotload-error-reason condition))))
       (test-assert (eq reason ':protected-source-changed)
                    "transaction implementation changes require a rebuild")
       (test-assert
        (and (eq (symbol-function 'test-source-hotload-other)
                 previous-function)
             (eq (test-source-hotload-other) ':baseline)
             (eq *test-source-hotload-value* previous-value)
             (equal *active-image-commit-identifier*
                    previous-active-identifier)
             (equal *active-image-history-commit* previous-active-history)
             (null (image-commit-current configuration))
             (null (image-commit-pending-records configuration))
             (equal (and (probe-file journal)
                         (uiop:read-file-string journal))
                    previous-journal))
        "protected transaction rejection preserves exact previous live state"))))
  nil)

(-> test-source-hotload-post-pointer-race () null)
(defun test-source-hotload-post-pointer-race ()
  "Test source races after pointer selection restore all preceding state."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (declare (ignore root other-pathname baseline))
     (test-source-hotload-write
      source-pathname
      (test-source-hotload-target-source
       :first ':first-overlay
       :value ':first-overlay))
     (let* ((first-commit
              (source-hotload-apply
               configuration
               (test-source-hotload-checker)
               '("src/hotload.lisp")
               :title "Load selection race base"))
            (pointer
              (configuration-current-image-commit-path configuration))
            (previous-pointer (uiop:read-file-string pointer))
            (previous-function
              (symbol-function 'test-source-hotload-first))
            (previous-active-identifier *active-image-commit-identifier*)
            (previous-active-history *active-image-history-commit*)
            (writer
              (symbol-function 'image-commit--write-form-atomically))
            (race-fired-p nil)
            (reason nil))
       (test-source-hotload-write
        source-pathname
        (test-source-hotload-target-source
         :first ':second-overlay
         :value ':second-overlay))
       (unwind-protect
            (progn
              (setf (symbol-function 'image-commit--write-form-atomically)
                    (lambda (pathname form)
                      (prog1 (funcall writer pathname form)
                        (when (and (not race-fired-p)
                                   (equal pathname pointer))
                          (setf race-fired-p t)
                          (test-source-hotload-write
                           source-pathname
                           (test-source-hotload-target-source
                            :first ':raced
                            :value ':raced))))))
              (setf reason
                    (handler-case
                        (progn
                          (source-hotload-apply
                           configuration
                           (test-source-hotload-checker)
                           '("src/hotload.lisp")
                           :title "Reject post-pointer source race")
                          nil)
                      (source-hotload-error (condition)
                        (source-hotload-error-reason condition)))))
         (setf (symbol-function 'image-commit--write-form-atomically)
               writer))
       (test-assert
        (and race-fired-p (eq reason ':source-snapshot-changed))
        "source changes after pointer publication fail final selection")
       (test-assert
        (and (string= (uiop:read-file-string pointer) previous-pointer)
             (equal *active-image-commit-identifier*
                    previous-active-identifier)
             (equal *active-image-history-commit* previous-active-history)
             (string= (image-commit-identifier
                       (image-commit-current configuration))
                      (image-commit-identifier first-commit))
             (eq (symbol-function 'test-source-hotload-first)
                 previous-function)
             (eq (test-source-hotload-first) ':first-overlay)
             (eq *test-source-hotload-value* ':first-overlay)
             (null (image-commit-pending-records configuration)))
        "post-pointer failure restores pointer, active state, and definitions"))))
  nil)

(-> test-source-hotload-structural-boundary () null)
(defun test-source-hotload-structural-boundary ()
  "Test generic and command registration structure requires a rebuild."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (declare (ignore source-pathname other-pathname))
     (let ((shape-pathname (merge-pathnames "src/shapes.lisp" root)))
       (labels ((reason-for-current-shape ()
                  "Return the source preflight reason for the current shape."
                  (handler-case
                      (progn
                        (source-hotload-plan
                         configuration
                         '("src/shapes.lisp")
                         :baseline baseline)
                        nil)
                    (source-hotload-error (condition)
                      (source-hotload-error-reason condition)))))
         (dolist (generic-options
                  '(((:documentation "Changed generic documentation."))
                    ((:documentation "Source hot-load generic fixture.")
                     (:method-combination progn))))
           (test-source-hotload-write
            shape-pathname
            (test-source-hotload-shape-source
             :generic-options generic-options))
           (test-assert
            (eq (reason-for-current-shape) ':definition-signature-changed)
            "generic options are structural source changes"))
         (dolist (case
                  '((:name "/changed-source-hotload")
                    (:aliases ("/changed-source-hotload-alias"))
                    (:argument "VALUE")
                    (:description "Changed command description")
                    (:tip "Changed command tip.")
                    (:busy-behavior :hold)
                    (:terminal-behavior :exclusive)
                    (:call-lambda-list (&optional value))
                    (:slash-argument-mode :first)))
           (destructuring-bind (key value) case
             (test-source-hotload-write
              shape-pathname
              (test-source-hotload-shape-source
               :command-metadata
               (test-source-hotload-plist-replace
                *test-source-hotload-command-metadata* key value)))
             (test-assert
              (eq (reason-for-current-shape) ':definition-signature-changed)
              "every command metadata field is structural")))
         (test-source-hotload-write
          shape-pathname
          (test-source-hotload-shape-source
           :command-lambda-list '(application value)))
         (test-assert
          (eq (reason-for-current-shape) ':definition-signature-changed)
          "application command handler lambda lists are structural")))))
  nil)

(-> test-source-hotload-successive-batches () null)
(defun test-source-hotload-successive-batches ()
  "Test successive overlays, failed replacement rollback, and base retirement."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (declare (ignore root other-pathname baseline))
     (test-source-hotload-write
      source-pathname
      (test-source-hotload-target-source
       :first ':first-overlay
       :value ':first-overlay))
     (let ((first-commit
             (source-hotload-apply
              configuration
              (test-source-hotload-checker)
              '("src/hotload.lisp")
              :title "Load first source overlay")))
       (test-assert
        (and (eq (test-source-hotload-first) ':first-overlay)
             (eq *test-source-hotload-value* ':first-overlay))
        "the first source overlay is active before a successive batch")
       (test-source-hotload-write
        source-pathname
        (test-source-hotload-target-source
         :first ':second-overlay
         :value ':second-overlay))
       (let ((probed-entries nil))
         (let ((*image-commit-replay-probe-function*
                 (lambda (checked-configuration script identifier)
                   (declare (ignore checked-configuration script identifier))
                   (setf probed-entries
                         (copy-tree *image-commit-replay-probe-entries*))
                   (error "Injected successive replay probe failure."))))
           (test-assert
            (handler-case
                (progn
                  (source-hotload-apply
                   configuration
                   (test-source-hotload-checker)
                   '("src/hotload.lisp")
                   :title "Reject second source overlay")
                  nil)
              (image-commit-error ()
                t))
            "a failed successive publication propagates its replay failure"))
         (test-assert
          (= (length probed-entries) 2)
          "publication probes the complete effective successive overlay set"))
       (test-assert
        (and (eq (test-source-hotload-first) ':first-overlay)
             (eq *test-source-hotload-value* ':first-overlay))
        "failed successive publication restores the preceding private overlay")
       (test-assert
        (string= (image-commit-identifier
                  (image-commit-current configuration))
                 (image-commit-identifier first-commit))
        "failed successive publication preserves the selected private commit")
       (let ((second-commit
               (source-hotload-apply
                configuration
                (test-source-hotload-checker)
                '("src/hotload.lisp")
                :title "Load second source overlay")))
         (test-assert
          (and (eq (test-source-hotload-first) ':second-overlay)
               (eq *test-source-hotload-value* ':second-overlay)
               (= (length (image-commit-entries second-commit)) 2))
          "a successful successive batch replaces the preceding overlay"))
       (test-source-hotload-write source-pathname
                                  (test-source-hotload-baseline-source))
       (multiple-value-bind (base-commit count source-states)
           (source-hotload-apply
            configuration
            (test-source-hotload-checker)
            '("src/hotload.lisp")
            :title "Retire source overlays at base")
         (test-assert
          (and (= count 2)
               (equal source-states '(:committed))
               (eq (test-source-hotload-first) ':baseline)
               (eq *test-source-hotload-value* ':baseline))
          "returning tracked source to base installs the baseline definitions")
         (test-assert
          (null (image-commit-entries base-commit))
          "returning tracked source to base retires private overlay entries")))))
  nil)

(-> test-source-hotload-startup-conflict-atomicity () null)
(defun test-source-hotload-startup-conflict-atomicity ()
  "Test source-overlay preflight prevents partial selected-commit startup replay."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (declare (ignore root other-pathname baseline))
     (test-source-hotload-write source-pathname
                                (test-source-hotload-target-source))
     (source-hotload-apply
      configuration
      (test-source-hotload-checker)
      '("src/hotload.lisp")
      :title "Create startup conflict fixture")
     (test-source-hotload-write
      source-pathname
      (test-source-hotload-target-source
       :first ':baseline
       :value ':divergent))
     (test-source-hotload-install (test-source-hotload-baseline-source))
     (remhash (definition-key '(defun test-source-hotload-first () :baseline))
              *exploratory-definitions*)
     (remhash
      (definition-key '(defparameter *test-source-hotload-value* :baseline))
      *exploratory-definitions*)
     (let ((failures (image-state-load configuration)))
       (test-assert
        (= (length failures) 1)
        "startup reports one selected source-overlay conflict")
       (test-assert
        (and (eq (test-source-hotload-first) ':baseline)
             (eq *test-source-hotload-value* ':baseline))
        "startup preflights all source overlays before installing any entry")
       (let* ((commit (image-commit-current configuration))
              (entries (image-commit-entries commit))
              (first-entry
                (find (definition-key
                       '(defun test-source-hotload-first () :baseline))
                      entries
                      :key (lambda (entry) (getf entry :target))
                      :test #'string=))
              (value-entry
                (find (definition-key
                       '(defparameter *test-source-hotload-value* :baseline))
                      entries
                      :key (lambda (entry) (getf entry :target))
                      :test #'string=)))
         (test-source-hotload-write source-pathname
                                    (test-source-hotload-baseline-source))
         (test-source-hotload-install (test-source-hotload-baseline-source))
         (let ((*source-hotload-preflight-results*
                 (image-commit--preflight-source-entries commit)))
           (flet ((replay (entry)
                    "Replay one source-aware test ENTRY."
                    (self-replay-source-definition
                     (getf entry :package)
                     (getf entry :source)
                     (getf entry :source-provenance))))
             (replay first-entry)
             (test-source-hotload-write
              source-pathname
              (test-source-hotload-target-source :value ':divergent))
             (replay value-entry)))
         (test-assert
          (and (eq (test-source-hotload-first) ':target)
               (eq *test-source-hotload-value* ':target))
          "startup replay uses one complete preflight snapshot across entries")))))
  nil)

(-> test-source-hotload-clean-replay-probe () null)
(defun test-source-hotload-clean-replay-probe ()
  "Test replay probing against a generated base view of the configured worktree."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (declare (ignore root other-pathname))
     (test-source-hotload-write source-pathname
                                (test-source-hotload-target-source))
     (let ((probe-count 0))
       (let ((*image-commit-replay-probe-function*
               (lambda (checked-configuration script identifier)
                 (declare (ignore identifier))
                 (test-assert
                  (= (length *image-commit-replay-probe-entries*) 2)
                  "source publication binds its complete entries for replay probing")
                 (image-commit--call-with-source-probe-script
                  checked-configuration
                  script
                  *image-commit-replay-probe-entries*
                  (lambda (probe-script)
                    (incf probe-count)
                    (test-assert
                     (not (equal probe-script script))
                     "source-aware probing wraps the private reconstruction script")
                    (test-source-hotload-install
                     (test-source-hotload-baseline-source))
                    (test-assert
                     (and (eq (test-source-hotload-first) ':baseline)
                          (eq *test-source-hotload-value* ':baseline))
                     "the probe starts from baseline live definitions")
                    (load probe-script)
                    (test-assert
                     (and (eq (test-source-hotload-first) ':target)
                          (eq *test-source-hotload-value* ':target))
                     "the generated configured-worktree base view replays overlays")
                    nil)))))
         (multiple-value-bind (commit count source-states)
             (source-hotload-apply
              configuration
              (test-source-hotload-checker)
              '("src/hotload.lisp")
              :title "Probe source overlays from base")
           (test-assert
            (and commit
                 (= count 2)
                 (= probe-count 1)
                 (equal source-states '(:uncommitted))
                 (string= baseline
                          (image-commit-source-commit commit)))
            "source publication completes after one configured base replay probe"))))))
  nil)

(-> test-source-hotload-clean-process-probe () null)
(defun test-source-hotload-clean-process-probe ()
  "Test a configured worktree source overlay in the real clean replay process."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (relative-pathname "src/state/image-commits.lisp")
         (pathname (merge-pathnames
                    relative-pathname
                    (configuration-source-root configuration)))
         (target (definition-key
                  '(defparameter *image-commit-replay-probe-version* 1)))
         (base-source
           (format nil
                   "(defparameter *image-commit-replay-probe-version* 0~%~2T\"The clean replay test baseline.\")"))
         (base-definition (self-read-form base-source :read-eval nil))
         (identifier (make-identifier))
         (script (merge-pathnames "probe/source-reconstruct.lisp" root)))
    (unwind-protect
         (multiple-value-bind (target-definition target-source)
             (source-hotload--tracked-definition
              pathname target (find-package '#:autolith))
           (let* ((provenance
                    (list :version 1
                          :repository-commit "clean-process-test"
                          :baseline-commit "clean-process-test"
                          :relative-pathname relative-pathname
                          :package "AUTOLITH"
                          :source-state ':committed
                          :base-present-p t
                          :base-source base-source
                          :base-identity
                          (source-definition-identity base-definition)
                          :base-file-blob "clean-process-test-base"
                          :target-file-blob "clean-process-test-target"
                          :definition-target target
                          :target-identity
                          (source-definition-identity target-definition)))
                  (entries
                    (list
                     (list :kind ':legacy
                           :id (make-identifier)
                           :target "clean-process-base-assertion"
                           :source
                           "(unless (= *image-commit-replay-probe-version* 0) (error \"Recorded base definition was not active before source replay.\"))")
                     (list :kind ':legacy
                           :id (make-identifier)
                           :target "clean-process-sentinel"
                           :source
                           "(setf *image-commit-replay-probe-version* 99)")
                     (list :kind ':definition
                           :id (make-identifier)
                           :target target
                           :package "AUTOLITH"
                           :source target-source
                           :source-provenance provenance)
                     (list :kind ':legacy
                           :id (make-identifier)
                           :target "clean-process-assertion"
                           :source
                           "(unless (= *image-commit-replay-probe-version* 1) (error \"Source overlay was suppressed during its clean replay probe.\"))"))))
             (image-commit-write-script
              script
              :identifier identifier
              :title "Probe configured source overlay"
              :entries entries)
             (let ((*image-commit-replay-probe-entries* entries))
               (test-assert
                (null (image-commit-replay-probe
                       configuration script identifier))
                "a clean configured-worktree process applies overlays from base"))))
      (when (probe-file root)
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore))))
  nil)

(-> test-source-hotload-error-paths () null)
(defun test-source-hotload-error-paths ()
  "Test structured rebuild failures for unsupported source boundary changes."
  (test-call-with-source-hotload-fixture
   (lambda (configuration root source-pathname other-pathname baseline)
     (labels ((reason-for (thunk)
                "Return the structured hot-load reason signaled by THUNK."
                (handler-case
                    (progn (funcall thunk) nil)
                  (source-hotload-error (condition)
                    (source-hotload-error-reason condition))))

              (restore-fixture ()
                "Restore all tracked fixture files to BASELINE."
                (test-source-hotload-write source-pathname
                                           (test-source-hotload-baseline-source))
                (test-source-hotload-write
                 other-pathname
                 (format nil
                         "(in-package #:autolith)~2%~
                          (defun test-source-hotload-other () :baseline)~%"))
               (test-source-hotload-write
                (merge-pathnames "autolith.asd" root)
                (format nil "(asdf:defsystem #:fixture)~%"))
               (test-source-hotload-write (merge-pathnames "qlfile" root)
                                          (format nil "~%"))))
        (let ((cases
                (list
                 (list
                  ':definition-deleted-or-renamed
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname
                     (format nil
                             "(in-package #:autolith)~2%~
                              (defun test-source-hotload-first () :baseline)~%"))))
                 (list
                  ':definition-signature-changed
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname
                     (format nil
                             "(in-package #:autolith)~2%~
                              (defun test-source-hotload-first (value) value)~2%~
                              (defparameter *test-source-hotload-value* :baseline)~2%~
                              (defvar *test-source-hotload-stable-value* :baseline)~%"))))
                 (list
                  ':definition-order-changed
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname
                     (format nil
                             "(in-package #:autolith)~2%~
                              (defparameter *test-source-hotload-value* :target)~2%~
                              (defun test-source-hotload-first () :target)~2%~
                              (defvar *test-source-hotload-stable-value* :baseline)~%"))))
                 (list
                  ':definition-added
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname
                     (concatenate
                      'string
                      (test-source-hotload-target-source)
                      (format nil
                              "~%(defun test-source-hotload-added () :added)~%")))))
                 (list
                  ':non-reloadable-definition
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname
                     (format nil
                             "(in-package #:autolith)~2%~
                              (defun test-source-hotload-first () :baseline)~2%~
                              (defparameter *test-source-hotload-value* :baseline)~2%~
                              (defvar *test-source-hotload-stable-value* :target)~%"))))
                 (list
                  ':top-level-form-changed
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname
                     (test-source-hotload-target-source :side-effect-p t))))
                 (list
                  ':top-level-form-changed
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname
                     (test-source-hotload-target-source
                      :package-name '#:cl-user))))
                 (list
                  ':build-boundary-changed
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname (test-source-hotload-target-source))
                    (test-source-hotload-write
                     (merge-pathnames "autolith.asd" root)
                     (format nil "(asdf:defsystem #:fixture :serial t)~%"))))
                 (list
                  ':build-boundary-changed
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname (test-source-hotload-target-source))
                    (test-source-hotload-write
                     (merge-pathnames "qlfile" root)
                     (format nil "alexandria~%"))))
                 (list
                  ':incomplete-source-boundary
                  (lambda ()
                    (test-source-hotload-write
                     source-pathname (test-source-hotload-target-source))
                    (test-source-hotload-write
                     other-pathname
                     (format nil
                             "(in-package #:autolith)~2%~
                              (defun test-source-hotload-other () :target)~%")))))))
         (dolist (case cases)
           (restore-fixture)
           (destructuring-bind (expected setup) case
             (funcall setup)
             (test-assert
              (eq (reason-for
                   (lambda ()
                     (source-hotload-plan
                      configuration '("src/hotload.lisp")
                      :baseline baseline)))
                  expected)
              "unsupported source changes signal a structured rebuild reason"))))
       (restore-fixture)
       (delete-file source-pathname)
       (test-assert
        (eq (reason-for
             (lambda ()
               (source-hotload-plan
                configuration '("src/hotload.lisp")
                :baseline baseline)))
            ':invalid-source-path)
        "deleted source paths require a rebuild before hot loading"))))
  (let* ((registry (make-default-tool-registry))
         (tool (tool-registry-find registry "self" "load-source-changes"))
         (properties (json-get (tool-parameters tool) "properties"))
         (paths (json-get properties "paths")))
    (test-assert (typep tool 'self-load-source-changes-tool)
                 "the default registry exposes self.load-source-changes")
    (test-assert (= (json-get paths "minItems") 1)
                 "the source hot-load tool requires a non-empty explicit path batch"))
  nil)