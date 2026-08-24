(in-package #:autolith)

;;;; -- Source Change Model --

(defclass source-hotload-change ()
  ((relative-pathname
    :initarg :relative-pathname
    :reader source-hotload-change-relative-pathname
    :type non-empty-string
    :documentation "The changed definition file relative to the source root.")
   (package
    :initarg :package
    :reader source-hotload-change-package
    :type package
    :documentation "The active package in which the definition is read.")
   (definition
    :initarg :definition
    :reader source-hotload-change-definition
    :type list
    :documentation "The parsed target definition.")
   (source
    :initarg :source
    :reader source-hotload-change-source
    :type string
    :documentation "The exact target definition source text.")
   (base-source
    :initarg :base-source
    :reader source-hotload-change-base-source
    :type string
    :documentation "The exact baseline definition source.")
   (previous-source
    :initarg :previous-source
    :reader source-hotload-change-previous-source
    :type (option string)
    :documentation "The effective live definition source restored by rollback.")
   (provenance
    :initarg :provenance
    :reader source-hotload-change-provenance
    :type list
    :documentation "The portable Git, path, blob, and definition provenance."))
  (:documentation "One preflighted tracked definition ready for live installation."))

(defparameter *source-hotload-guarded-paths*
  '("autolith.asd" "qlfile" "qlfile.lock" "sbcl.version"
    "src/core/package.lisp")
  "Tracked build, dependency, and package files that always require a rebuild.")

(-> source-hotload--error
    (configuration keyword keyword string
     &key (:relative-pathname (option string)) (:pathname (option pathname)))
    nil)
(defun source-hotload--error
    (configuration stage reason message &key relative-pathname pathname)
  "Signal one structured hot-load failure requiring a source rebuild."
  (error 'source-hotload-error
         :message message
         :tool-name "self.load-source-changes"
         :pathname
         (or pathname
             (and relative-pathname
                  (merge-pathnames relative-pathname
                                   (configuration-source-root configuration))))
         :stage stage
         :reason reason
         :relative-pathname relative-pathname))

(-> source-hotload--git-output (configuration list) string)
(defun source-hotload--git-output (configuration arguments)
  "Run Git ARGUMENTS and return its output or signal a structured failure."
  (handler-case
      (self-git-command configuration arguments)
    (error (condition)
      (source-hotload--error
       configuration
       ':git
       ':git-failed
       (format nil "Git could not establish the source hot-load boundary: ~A"
               condition)))))

(-> source-hotload--git-trimmed-output (configuration list) string)
(defun source-hotload--git-trimmed-output (configuration arguments)
  "Return Git ARGUMENTS output without surrounding horizontal or line whitespace."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (source-hotload--git-output configuration arguments)))

(-> source-hotload--commit (configuration string) non-empty-string)
(defun source-hotload--commit (configuration designator)
  "Resolve Git commit DESIGNATOR to a full immutable identity."
  (let ((commit
          (source-hotload--git-trimmed-output
           configuration
           (list "rev-parse" "--verify" (format nil "~A^{commit}" designator)))))
    (unless (non-empty-string-p commit)
      (source-hotload--error
       configuration
       ':git
       ':invalid-baseline
       (format nil "Git revision ~S does not identify a source commit."
               designator)))
    commit))

(-> source-hotload--known-baseline (configuration) (option string))
(defun source-hotload--known-baseline (configuration)
  "Return the tracked source commit represented by the current base image."
  (image-commit--base-source-commit
   (image-commit-current configuration)))

(-> source-hotload--baseline
    (configuration (option string))
    non-empty-string)
(defun source-hotload--baseline (configuration requested)
  "Return the verified source baseline represented by the running image."
  (let* ((known (source-hotload--known-baseline configuration))
         (selected (or requested known)))
    (unless selected
      (source-hotload--error
       configuration
       ':validation
       ':unknown-baseline
       "The running image has no tracked source baseline; rebuild Autolith before loading source changes."))
    (let ((baseline (source-hotload--commit configuration selected)))
      (when (and known
                 (not (string= baseline
                               (source-hotload--commit configuration known))))
        (source-hotload--error
         configuration
         ':validation
         ':baseline-mismatch
         "The requested baseline is not the source revision represented by the running image; rebuild before crossing this source boundary."))
      (when (and (boundp '*active-image-build-record*)
                 (let ((record *active-image-build-record*))
                   (and (listp record)
                        (string= (or (getf (rest record) :source-commit) "")
                                 baseline)
                        (not (getf (rest record) :source-clean-p)))))
        (source-hotload--error
         configuration
         ':validation
         ':dirty-image-baseline
         "The running image was built from uncommitted source, so its baseline definitions cannot be reconstructed safely. Rebuild from clean source first."))
      (let ((merge-base
              (source-hotload--git-trimmed-output
               configuration
               (list "merge-base" baseline "HEAD"))))
        (unless (string= merge-base baseline)
          (source-hotload--error
           configuration
           ':validation
           ':baseline-not-ancestor
           "The running image source baseline is not an ancestor of the current checkout; rebuild before loading this branch.")))
      baseline)))

(-> source-hotload--pathname (configuration string) (values pathname string))
(defun source-hotload--pathname (configuration requested)
  "Return REQUESTED's existing canonical tracked src/ Lisp pathname and name."
  (let* ((root (uiop:ensure-directory-pathname
                (truename (configuration-source-root configuration))))
         (editable-root (merge-pathnames "src/" root))
         (requested-pathname (pathname requested))
         (pathname (merge-pathnames requested-pathname root)))
    (unless (and (non-empty-string-p requested)
                 (not (uiop:absolute-pathname-p requested-pathname))
                 (string-equal (or (pathname-type pathname) "") "lisp")
                 (uiop:subpathp pathname editable-root)
                 (probe-file pathname))
      (source-hotload--error
       configuration
       ':validation
       ':invalid-source-path
       "Source hot loading is limited to existing tracked Common Lisp files under src/."
       :relative-pathname requested
       :pathname pathname))
    (let* ((canonical (truename pathname))
           (relative (enough-namestring canonical root)))
      (source-hotload--git-output
       configuration
       (list "ls-files" "--error-unmatch" "--" relative))
      (values canonical relative))))

(-> source-hotload--paths (configuration list) list)
(defun source-hotload--paths (configuration requested)
  "Validate and return unique pathname and repository-name pairs from REQUESTED."
  (unless requested
    (source-hotload--error
     configuration
     ':validation
     ':no-paths
     "At least one explicit tracked Common Lisp source path is required."))
  (let ((result nil))
    (dolist (name requested)
      (unless (stringp name)
        (source-hotload--error
         configuration
         ':validation
         ':invalid-source-path
         "Every source hot-load path must be a string."))
      (multiple-value-bind (pathname relative)
          (source-hotload--pathname configuration name)
        (when (find relative result :key #'rest :test #'string=)
          (source-hotload--error
           configuration
           ':validation
           ':duplicate-source-path
           (format nil "Source path ~A was requested more than once." relative)
           :relative-pathname relative
           :pathname pathname))
        (push (cons pathname relative) result)))
    (nreverse result)))

(-> source-hotload--diff-status (configuration string) list)
(defun source-hotload--diff-status (configuration baseline)
  "Return Git name-status rows from BASELINE through the current working tree."
  (loop for line in (uiop:split-string
                     (source-hotload--git-output
                      configuration
                      (list "diff" "--name-status" baseline "--"))
                     :separator '(#\Newline #\Return))
        unless (zerop (length line))
          collect (uiop:split-string line :separator '(#\Tab))))

(-> source-hotload--overlay-pathnames (configuration) list)
(defun source-hotload--overlay-pathnames (configuration)
  "Return paths represented by the selected source-aware private overlays."
  (remove-duplicates
   (loop for entry in (image-commit-base-entries configuration)
         for provenance = (getf entry :source-provenance)
         when provenance
           collect (getf provenance :relative-pathname))
   :test #'string=))

(-> source-hotload--validate-boundary
    (configuration string list)
    null)
(defun source-hotload--validate-boundary (configuration baseline paths)
  "Reject source-boundary changes that cannot be installed definition-wise."
  (let ((requested (mapcar #'rest paths))
        (changed-source nil))
    (dolist (row (source-hotload--diff-status configuration baseline))
      (let* ((status (first row))
             (names (rest row))
             (name (first (last names))))
        (when (find name *source-hotload-guarded-paths* :test #'string=)
          (source-hotload--error
           configuration
           ':preflight
           ':build-boundary-changed
           (format nil "Tracked build, package, or dependency input ~A changed; rebuild Autolith instead of hot loading."
                   name)
           :relative-pathname name))
        (when (and name
                   (uiop:string-prefix-p "src/" name)
                   (string-equal (or (pathname-type (pathname name)) "")
                                 "lisp"))
          (when (or (uiop:string-prefix-p "A" status)
                    (uiop:string-prefix-p "D" status)
                    (uiop:string-prefix-p "R" status)
                    (uiop:string-prefix-p "C" status))
            (source-hotload--error
             configuration
             ':preflight
             ':source-layout-changed
             (format nil "Source file addition, deletion, copy, or rename ~A requires a rebuild."
                     name)
             :relative-pathname name))
          (pushnew name changed-source :test #'string=))))
    (dolist (name changed-source)
      (unless (find name requested :test #'string=)
        (source-hotload--error
         configuration
         ':preflight
         ':incomplete-source-boundary
         (format nil "Changed Common Lisp source ~A is outside the explicit hot-load batch."
                 name)
         :relative-pathname name)))
    (let ((overlay-pathnames
            (source-hotload--overlay-pathnames configuration)))
      (dolist (name requested)
        (unless (or (find name changed-source :test #'string=)
                    (find name overlay-pathnames :test #'string=))
          (source-hotload--error
           configuration
           ':preflight
           ':unchanged-source-path
           (format nil "Requested source ~A has no changes from the active-image baseline or selected source overlay."
                   name)
           :relative-pathname name)))))
  nil)

(-> source-hotload--file-at
    (configuration string string)
    string)
(defun source-hotload--file-at (configuration commit relative-pathname)
  "Return RELATIVE-PATHNAME's exact contents from Git COMMIT."
  (handler-case
      (self-git-command
       configuration
       (list "show" (format nil "~A:~A" commit relative-pathname)))
    (error (condition)
      (source-hotload--error
       configuration
       ':preflight
       ':source-layout-changed
       (format nil "Baseline source ~A is unavailable: ~A"
               relative-pathname condition)
       :relative-pathname relative-pathname))))

(-> source-hotload--blob-at
    (configuration string string)
    non-empty-string)
(defun source-hotload--blob-at (configuration commit relative-pathname)
  "Return Git's blob identity for RELATIVE-PATHNAME at COMMIT."
  (source-hotload--git-trimmed-output
   configuration
   (list "rev-parse" (format nil "~A:~A" commit relative-pathname))))

(-> source-hotload--working-blob (configuration string) non-empty-string)
(defun source-hotload--working-blob (configuration relative-pathname)
  "Return Git's blob identity for the current working source file."
  (source-hotload--git-trimmed-output
   configuration
   (list "hash-object" "--" relative-pathname)))

(-> source-hotload--source-blob
    (configuration string)
    non-empty-string)
(defun source-hotload--source-blob (configuration source)
  "Return Git's blob identity for the exact characters in SOURCE."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
   (uiop:run-program
    (list "git"
          "-C"
          (namestring (configuration-source-root configuration))
          "hash-object"
          "--stdin")
    :input (make-string-input-stream source)
    :output ':string
    :error-output ':output)))

(-> source-hotload--path-snapshots (configuration list) list)
(defun source-hotload--path-snapshots (configuration paths)
  "Return exact working-tree blob snapshots for validated PATHS."
  (loop for (pathname . relative-pathname) in paths
        collect (list pathname
                      relative-pathname
                      (source-hotload--working-blob
                       configuration relative-pathname))))

(-> source-hotload--source-state (configuration string) keyword)
(defun source-hotload--source-state (configuration relative-pathname)
  "Return whether RELATIVE-PATHNAME matches HEAD or has uncommitted changes."
  (if (zerop
       (length
        (source-hotload--git-trimmed-output
         configuration
         (list "status" "--porcelain" "--" relative-pathname))))
      ':committed
      ':uncommitted))

(-> source-hotload--form-source (string source-form) string)
(defun source-hotload--form-source (source source-form)
  "Return SOURCE-FORM's exact text from SOURCE."
  (subseq source
          (source-form-start source-form)
          (source-form-end source-form)))

(-> source-hotload--definition-table
    (configuration string list string)
    hash-table)
(defun source-hotload--definition-table
    (configuration relative-pathname source-forms side)
  "Return unique supported SOURCE-FORMS keyed by signature for one SIDE."
  (let ((definitions (make-hash-table :test #'equal)))
    (dolist (source-form source-forms)
      (let ((form (source-form-form source-form)))
        (when (definition-form-p form)
          (let ((target (definition-key form)))
            (when (gethash target definitions)
              (source-hotload--error
               configuration
               ':preflight
               ':duplicate-definition
               (format nil "~A source contains duplicate definition ~A in ~A."
                       side target relative-pathname)
               :relative-pathname relative-pathname))
            (setf (gethash target definitions) source-form)))))
    definitions))

(-> source-hotload--form-order-token (source-form) list)
(defun source-hotload--form-order-token (source-form)
  "Return SOURCE-FORM's stable identity for source-order validation."
  (let ((form (source-form-form source-form)))
    (if (definition-form-p form)
        (list ':definition (definition-key form))
        (list ':other (source-definition-identity form)))))

(-> source-hotload--order-preserved-p (list list) boolean)
(defun source-hotload--order-preserved-p (base-forms target-forms)
  "Return true when TARGET-FORMS preserve all baseline top-level form order."
  (let ((target-tokens (mapcar #'source-hotload--form-order-token target-forms))
        (position -1))
    (dolist (base-form base-forms t)
      (let ((next
              (position (source-hotload--form-order-token base-form)
                        target-tokens
                        :start (1+ position)
                        :test #'equal)))
        (unless next
          (return nil))
        (setf position next)))))

(-> source-hotload--private-entry (configuration string) (option list))
(defun source-hotload--private-entry (configuration target)
  "Return the selected private definition entry for TARGET, when present."
  (find-if (lambda (entry)
             (image-commit--entry-matches-p entry ':definition target))
           (image-commit-base-entries configuration)
           :from-end t))

(-> source-hotload--provenance
    (string string string keyword string string list list)
    list)
(defun source-hotload--provenance
    (repository-commit baseline relative-pathname source-state
     base-file-blob target-file-blob definition base-definition)
  "Return complete portable provenance for one source definition overlay."
  (list :version 1
        :repository-commit repository-commit
        :baseline-commit baseline
        :relative-pathname relative-pathname
        :package "AUTOLITH"
        :source-state source-state
        :base-present-p t
        :base-source nil
        :base-identity
        (and base-definition (source-definition-identity base-definition))
        :base-file-blob base-file-blob
        :target-file-blob target-file-blob
        :definition-target (definition-key definition)
        :target-identity (source-definition-identity definition)))

(-> source-hotload--file-changes
    (configuration string string pathname string string string)
    list)
(defun source-hotload--file-changes
    (configuration baseline repository-commit pathname relative-pathname
     base-file-blob target-file-blob)
  "Return supported definition changes in one preflighted source file."
  (let* ((package (find-package '#:autolith))
         (base-source
           (source-hotload--file-at configuration baseline relative-pathname))
         (target-source (uiop:read-file-string pathname))
         (actual-target-file-blob
           (source-hotload--source-blob configuration target-source))
         (base-forms (source-read-forms base-source :package package))
         (target-forms (source-read-forms target-source :package package))
         (base-definitions
           (source-hotload--definition-table
            configuration relative-pathname base-forms "Baseline"))
         (target-definitions
           (source-hotload--definition-table
            configuration relative-pathname target-forms "Target"))
         (base-other
           (loop for source-form in base-forms
                 for form = (source-form-form source-form)
                 unless (definition-form-p form)
                   collect form))
         (target-other
           (loop for source-form in target-forms
                 for form = (source-form-form source-form)
                 unless (definition-form-p form)
                   collect form))
         (source-state
           (source-hotload--source-state configuration relative-pathname))
         (changes nil))
    (unless (string= actual-target-file-blob target-file-blob)
      (source-hotload--error
       configuration
       ':preflight
       ':source-snapshot-changed
       (format nil "Tracked source ~A changed while its definitions were being read."
               relative-pathname)
       :relative-pathname relative-pathname
       :pathname pathname))
    (unless (equal (mapcar #'source-definition-identity base-other)
                   (mapcar #'source-definition-identity target-other))
      (source-hotload--error
       configuration
       ':preflight
       ':top-level-form-changed
       (format nil "Unsupported top-level, package, or side-effect form changed in ~A; rebuild Autolith instead."
               relative-pathname)
       :relative-pathname relative-pathname
       :pathname pathname))
    (maphash
     (lambda (target base-source-form)
       (let ((target-source-form (gethash target target-definitions)))
         (unless target-source-form
           (source-hotload--error
            configuration
            ':preflight
            ':definition-deleted-or-renamed
            (format nil "Definition ~A was deleted or changed identity in ~A; rebuild Autolith instead."
                    target relative-pathname)
            :relative-pathname relative-pathname
            :pathname pathname))
         (unless
             (string=
              (source-definition-identity
               (source-definition-shape
                (source-form-form base-source-form)))
              (source-definition-identity
               (source-definition-shape
                (source-form-form target-source-form))))
           (source-hotload--error
            configuration
            ':preflight
            ':definition-signature-changed
            (format nil "Definition ~A changed callable or structural signature in ~A; rebuild Autolith instead."
                    target relative-pathname)
            :relative-pathname relative-pathname
            :pathname pathname))))
     base-definitions)
    (unless (source-hotload--order-preserved-p base-forms target-forms)
      (source-hotload--error
       configuration
       ':preflight
       ':definition-order-changed
       (format nil "Existing top-level definition order changed in ~A; rebuild Autolith instead."
               relative-pathname)
       :relative-pathname relative-pathname
       :pathname pathname))
    (dolist (target-source-form target-forms)
      (let ((definition (source-form-form target-source-form)))
        (when (definition-form-p definition)
          (let* ((target (definition-key definition))
                 (base-source-form (gethash target base-definitions))
                 (base-definition
                   (and base-source-form
                        (source-form-form base-source-form)))
                 (exact-base-source
                   (and base-source-form
                        (source-hotload--form-source base-source
                                                   base-source-form)))
                 (private-entry
                   (source-hotload--private-entry configuration target))
                 (private-source (and private-entry
                                      (getf private-entry :source)))
                 (private-definition
                   (and private-source
                        (self-read-form private-source
                                        :read-eval nil
                                        :package package)))
                 (target-identity (source-definition-identity definition))
                 (base-matches-p
                   (and base-definition
                        (string= target-identity
                                 (source-definition-identity base-definition))))
                 (private-matches-p
                   (and private-definition
                        (string= target-identity
                                 (source-definition-identity
                                  private-definition)))))
            (unless base-definition
              (source-hotload--error
               configuration
               ':preflight
               ':definition-added
               (format nil "Definition ~A was added in ~A; rebuild Autolith instead."
                       target relative-pathname)
               :relative-pathname relative-pathname
               :pathname pathname))
            (unless (and base-matches-p
                         (or (null private-entry) private-matches-p))
              (when (eq (first definition) 'defvar)
                (source-hotload--error
                 configuration
                 ':preflight
                 ':non-reloadable-definition
                 (format nil
                         "DEFVAR definition ~A changed in ~A; rebuild Autolith instead."
                         target relative-pathname)
                 :relative-pathname relative-pathname
                 :pathname pathname))
              (let* ((provenance
                       (source-hotload--provenance
                        repository-commit
                        baseline
                        relative-pathname
                        source-state
                        base-file-blob
                        target-file-blob
                        definition
                        base-definition))
                     (previous-source
                       (or (gethash target *exploratory-definitions*)
                           private-source
                           exact-base-source)))
                (setf (getf provenance :base-source) exact-base-source)
                (push
                 (make-instance
                  'source-hotload-change
                  :relative-pathname relative-pathname
                  :package package
                  :definition definition
                  :source
                  (source-hotload--form-source target-source target-source-form)
                  :base-source exact-base-source
                  :previous-source previous-source
                  :provenance provenance)
                 changes)))))))
    (nreverse changes)))

(-> source-hotload-plan
    (configuration list &key (:baseline (option string)))
    (values list non-empty-string non-empty-string list))
(defun source-hotload-plan (configuration requested-paths &key baseline)
  "Preflight REQUESTED-PATHS and return supported changes and Git identities."
  (unless (and *image-state-initialized-p*
               (non-empty-string-p *active-image-lineage-identifier*))
    (source-hotload--error
     configuration
     ':validation
     ':uninitialized-image-state
     "The running image has not initialized its private mutation lineage."))
  (when (image-commit-pending-records configuration)
    (source-hotload--error
     configuration
     ':validation
     ':pending-live-mutations
     "Commit or discard existing exploratory self mutations before loading an authoritative tracked-source batch."))
  (let* ((baseline (source-hotload--baseline configuration baseline))
         (repository-commit
           (source-hotload--commit configuration "HEAD"))
         (paths (source-hotload--paths configuration requested-paths)))
    (source-hotload--validate-boundary configuration baseline paths)
    (let* ((snapshots (source-hotload--path-snapshots configuration paths))
           (changes
             (loop for (pathname relative-pathname target-file-blob)
                     in snapshots
                   append
                   (source-hotload--file-changes
                    configuration
                    baseline
                    repository-commit
                    pathname
                    relative-pathname
                    (source-hotload--blob-at
                     configuration baseline relative-pathname)
                    target-file-blob))))
      (unless changes
        (source-hotload--error
         configuration
         ':preflight
         ':no-supported-definition-changes
         "The requested source boundary contains no changed supported definitions."))
      (values changes baseline repository-commit snapshots))))


(-> source-hotload--verify-snapshot
    (configuration string string list)
    null)
(defun source-hotload--verify-snapshot
    (configuration baseline repository-commit snapshots)
  "Require the complete tracked source boundary to match its snapshot."
  (unless (string= (source-hotload--commit configuration "HEAD")
                   repository-commit)
    (source-hotload--error
     configuration
     ':selection
     ':source-snapshot-changed
     "The checked-out source commit changed during source hot loading; the private image commit was not selected."))
  (source-hotload--validate-boundary
   configuration
   baseline
   (mapcar (lambda (snapshot)
             (cons (first snapshot) (second snapshot)))
           snapshots))
  (dolist (snapshot snapshots)
    (destructuring-bind (pathname relative-pathname expected) snapshot
      (let ((actual
              (handler-case
                  (source-hotload--working-blob
                   configuration relative-pathname)
                (source-hotload-error ()
                  nil))))
        (unless (and actual (string= actual expected))
          (source-hotload--error
           configuration
           ':selection
           ':source-snapshot-changed
           (format nil
                   "Tracked source ~A changed during source hot loading; the private image commit was not selected."
                   relative-pathname)
           :relative-pathname relative-pathname
           :pathname pathname)))))
  nil)


;;;; -- Atomic Installation and Publication --

(-> source-hotload--rollback (configuration list serious-condition) null)
(defun source-hotload--rollback (configuration records original-condition)
  "Restore RECORDS in reverse or signal compound active-image corruption."
  (let ((restoration-condition nil))
    (dolist (record (reverse records))
      (let* ((properties (rest record))
             (identifier (getf properties :id))
             (undo-action (gethash identifier *exploratory-undo-actions*)))
        (handler-case
            (progn
              (unless undo-action
                (error "No exact undo action remains for mutation ~A." identifier))
              (funcall undo-action)
              (remhash identifier *exploratory-undo-actions*)
              (mutation-journal-append
               configuration
               (list :mutation
                     :kind ':discard
                     :id identifier
                     :lineage *active-image-lineage-identifier*
                     :target (getf properties :target)
                     :result ':discarded
                     :detail "Rolled back failed source hot-load batch.")))
          (error (condition)
            (unless restoration-condition
              (setf restoration-condition condition))))))
    (when restoration-condition
      (error 'active-image-corruption
             :message
             "A failed source hot-load batch could not restore the active image."
             :original-condition original-condition
             :restoration-condition restoration-condition)))
  nil)

(-> source-hotload-apply
    (configuration mutation-checker list
     &key (:baseline (option string)) (:title string))
    (values image-commit integer list))
(defun source-hotload-apply
    (configuration checker requested-paths &key baseline title)
  "Atomically install, check, and privately commit requested tracked changes."
  (with-live-mutation
    (multiple-value-bind
        (changes baseline repository-commit source-snapshots)
        (source-hotload-plan configuration requested-paths :baseline baseline)
      (let* ((title (self-validate-commit-title title))
             (transaction-identifier (make-identifier))
             (records nil)
             (commit nil)
             (source-states
               (remove-duplicates
                (mapcar (lambda (change)
                          (getf (source-hotload-change-provenance change)
                                :source-state))
                        changes)
                :test #'eq)))
        (mutation-journal-append
         configuration
         (list :mutation
               :kind ':source-load
               :id transaction-identifier
               :lineage *active-image-lineage-identifier*
               :baseline-commit baseline
               :repository-commit repository-commit
               :paths (mapcar #'second source-snapshots)
               :definitions
               (mapcar (lambda (change)
                         (definition-key
                          (source-hotload-change-definition change)))
                       changes)
               :result ':pending))
        (handler-case
            (progn
              (dolist (change changes)
                (multiple-value-bind (result identifier record)
                    (self--install-definition-journaled
                     configuration
                     (source-hotload-change-source change)
                     :package (source-hotload-change-package change)
                     :read-eval nil
                     :previous-source
                     (source-hotload-change-previous-source change)
                     :provenance (source-hotload-change-provenance change))
                  (declare (ignore result identifier))
                  (push record records)))
              (setf records (nreverse records))
              (mutation-checker-check-active
               checker
               configuration
               (with-output-to-string (stream)
                 (dolist (change changes)
                   (write-string (source-hotload-change-source change) stream)
                   (terpri stream)
                   (terpri stream))))
              (setf commit
                    (image-commit-publish
                     configuration
                     :title title
                     :mutation-records records
                     :additional-entries nil
                     :selection-check
                     (lambda ()
                       (source-hotload--verify-snapshot
                        configuration
                        baseline
                        repository-commit
                        source-snapshots))))
              (mutation-journal-append
               configuration
               (list :mutation
                     :kind ':source-load
                     :id transaction-identifier
                     :lineage *active-image-lineage-identifier*
                     :baseline-commit baseline
                     :repository-commit repository-commit
                     :image-commit (image-commit-identifier commit)
                     :history-commit (image-commit-history-commit commit)
                     :result ':committed))
              (values commit (length changes) source-states))
          (error (condition)
            (unless commit
              (source-hotload--rollback configuration records condition)
              (mutation-journal-append
               configuration
               (list :mutation
                     :kind ':source-load
                     :id transaction-identifier
                     :lineage *active-image-lineage-identifier*
                     :baseline-commit baseline
                     :repository-commit repository-commit
                     :result ':failed
                     :condition (bounded-string condition :limit 2000))))
            (error condition)))))))

(defmethod tool-execute ((tool self-load-source-changes-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Install and privately commit one explicit tracked source change batch."
  (declare (ignore tool))
  (let ((paths-value (tool-argument arguments "paths" :required t)))
    (unless (vectorp paths-value)
      (source-hotload--error
       (tool-context-configuration context)
       ':validation
       ':invalid-source-paths
       "self.load-source-changes paths must be an array of repository-relative strings."))
    (multiple-value-bind (commit count source-states)
        (self-call-with-restarts
         (lambda ()
           (source-hotload-apply
            (tool-context-configuration context)
            (tool-context-effective-mutation-checker context)
            (coerce paths-value 'list)
            :baseline (tool-argument arguments "baseline")
            :title (tool-argument arguments "title" :required t)))
         :restart-name (tool-argument arguments "restart")
         :restart-value-source (tool-argument arguments "restart-value"))
      (tool-success
       (format nil
               "Loaded and privately committed ~D source definition~:P as ~A.~%Source state~:P: ~{~(~A~)~^, ~}~%Replay script: ~A"
               count
               (image-commit-identifier commit)
               source-states
               (image-commit-script-pathname commit))))))