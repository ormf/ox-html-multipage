;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; changes in ox.el to generalize exporting to multipage output.
;;;
;;; The body of `org-export-as' is split into two external functions,
;;; which can get used as components by other backends:
;;;
;;; 1. `org-export--collect-tree-info' collects the complete
;;;    parse-tree plus necessary information into the info proplist.
;;;
;;; 2. `org-export--transcode-headline' takes a headline and the info
;;;    proplist as arguments to generate the string for the output
;;;    file.
;;;
;;; The redefinition of `org-export-as' below should behave
;;; identically to its previous definition.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-export--collect-tree-info
    (backend &optional subtreep visible-only body-only ext-plist)
  "Parse current Org Buffer into a tree suitable for final
processing for the BACKEND and store it in the :parse-tree
property of info along with additional entries like
:headline-numbering :section-url-lookup and :id-alist. Return
info.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.

If narrowing is active in the current buffer, only transcode its
narrowed part.

If a region is active, transcode that region.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without surrounding template.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

Return info for further processing."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (org-export-barf-if-invalid-backend backend)
  (org-fold-core-ignore-modifications
    (save-excursion
      (save-restriction
        ;; Narrow buffer to an appropriate region or subtree for
        ;; parsing.  If parsing subtree, be sure to remove main
        ;; headline, planning data and property drawer.
        (cond ((org-region-active-p)
	       (narrow-to-region (region-beginning) (region-end)))
	      (subtreep
	       (org-narrow-to-subtree)
	       (goto-char (point-min))
	       (org-end-of-meta-data)
               ;; Make the region include top heading in the subtree.
               ;; This way, we will be able to retrieve its export
               ;; options when calling
               ;; `org-export--get-subtree-options'.
               (when (bolp) (backward-char))
	       (narrow-to-region (point) (point-max))))
        ;; Initialize communication channel with original buffer
        ;; attributes, unavailable in its copy.
        (let* ((org-export-current-backend (org-export-backend-name backend))
	       (info (org-combine-plists
		      (org-export--get-export-attributes
		       backend subtreep visible-only body-only)
		      (org-export--get-buffer-attributes)))
	       (parsed-keywords
	        (delq nil
		      (mapcar (lambda (o) (and (eq (nth 4 o) 'parse) (nth 1 o)))
			      (append (org-export-get-all-options backend)
				      org-export-options-alist))))
	       tree modified-tick)
	  ;; Update communication channel and get parse tree.  Buffer
	  ;; isn't parsed directly.  Instead, all buffer modifications
	  ;; and consequent parsing are undertaken in a temporary copy.
	  (org-export-with-buffer-copy
           (font-lock-mode -1)
	   ;; Run first hook with current back-end's name as argument.
	   (run-hook-with-args 'org-export-before-processing-hook
			       (org-export-backend-name backend))
	   (org-export-expand-include-keyword)
	   (org-export--delete-comment-trees)
	   (org-macro-initialize-templates org-export-global-macros)
	   (org-macro-replace-all org-macro-templates parsed-keywords)
	   ;; Refresh buffer properties and radio targets after previous
	   ;; potentially invasive changes.
	   (org-set-regexps-and-options)
	   (org-update-radio-target-regexp)
           (setq modified-tick (buffer-chars-modified-tick))
	   ;;  Possibly execute Babel code.  Re-run a macro expansion
	   ;;  specifically for {{{results}}} since inline source blocks
	   ;;  may have generated some more.  Refresh buffer properties
	   ;;  and radio targets another time.
	   (when org-export-use-babel
	     (org-babel-exp-process-buffer)
	     (org-macro-replace-all '(("results" . "$1")) parsed-keywords)
             (unless (eq modified-tick (buffer-chars-modified-tick))
	       (org-set-regexps-and-options)
	       (org-update-radio-target-regexp))
             (setq modified-tick (buffer-chars-modified-tick)))
	   ;; Run last hook with current back-end's name as argument.
	   ;; Update buffer properties and radio targets one last time
	   ;; before parsing.
	   (goto-char (point-min))
	   (save-excursion
	     (run-hook-with-args 'org-export-before-parsing-hook
			         (org-export-backend-name backend)))
           (unless (eq modified-tick (buffer-chars-modified-tick))
	     (org-set-regexps-and-options)
	     (org-update-radio-target-regexp))
           (setq modified-tick (buffer-chars-modified-tick))
	   ;; Update communication channel with environment.
	   (setq info
	         (org-combine-plists
		  info (org-export-get-environment backend subtreep ext-plist)))
           ;; Pre-process citations environment, i.e. install
	   ;; bibliography list, and citation processor in INFO.
	   (org-cite-store-bibliography info)
           (org-cite-store-export-processor info)
	   ;; De-activate uninterpreted data from parsed keywords.
	   (dolist (entry (append (org-export-get-all-options backend)
				  org-export-options-alist))
	     (pcase entry
	       (`(,p ,_ ,_ ,_ parse)
	        (let ((value (plist-get info p)))
		  (plist-put info
			     p
			     (org-export--remove-uninterpreted-data value info))))
	       (_ nil)))
	   ;; Install user's and developer's filters.
	   (setq info (org-export-install-filters info))
	   ;; Call options filters and update export options.  We do not
	   ;; use `org-export-filter-apply-functions' here since the
	   ;; arity of such filters is different.
	   (let ((backend-name (org-export-backend-name backend)))
	     (dolist (filter (plist-get info :filter-options))
	       (let ((result (funcall filter info backend-name)))
	         (when result (setq info result)))))
	   ;; Parse buffer.
	   (setq tree (org-element-parse-buffer nil visible-only))
	   ;; Prune tree from non-exported elements and transform
	   ;; uninterpreted elements or objects in both parse tree and
	   ;; communication channel.
	   (org-export--prune-tree tree info)
	   (org-export--remove-uninterpreted-data tree info)
	   ;; Call parse tree filters.
	   (setq tree (org-export-filter-apply-functions
                       (plist-get info :filter-parse-tree) tree info))
	   ;; Now tree is complete, compute its properties and add them
	   ;; to communication channel.
	   (setq info (org-export--collect-tree-properties tree info))
           (setq global-tree tree)  ;;; for debugging purposes, remove later
           ;; Process citations and bibliography.  Replace each citation
	   ;; and "print_bibliography" keyword in the parse tree with
	   ;; the output of the selected citation export processor.
           (org-cite-process-citations info)
           (org-cite-process-bibliography info)
	   ;; Eventually transcode TREE.  Wrap the resulting string into
	   ;; a template.
           info))))))

(defun org-export--transcode-headline (headline info)
  "transcode the headline tree into a string according to the
backend and return the string."
  (let* ((body (org-element-normalize-string
		(or (org-export-data headline info) "")))
	 (inner-template (alist-get 'inner-template
				    (plist-get info :translate-alist)))
	 (full-body (org-export-filter-apply-functions
		     (plist-get info :filter-body)
		     (if (not (functionp inner-template)) body
		       (funcall inner-template body info))
		     info))
	 (template (cdr (assq 'template
			      (plist-get info :translate-alist))))
         (output
          (if (or (not (functionp template)) body-only) full-body
	    (funcall template full-body info))))
    ;; Call citation export finalizer.
    (setq output (org-cite-finalize-export output info))
    ;; Remove all text properties since they cannot be
    ;; retrieved from an external process.  Finally call
    ;; final-output filter and return result.
    (org-no-properties
     (org-export-filter-apply-functions
      (plist-get info :filter-final-output)
      output info))))

(defun org-export-as
    (backend &optional subtreep visible-only body-only ext-plist)
  "Transcode current Org buffer into BACKEND code.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.

If narrowing is active in the current buffer, only transcode its
narrowed part.

If a region is active, transcode that region.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without surrounding template.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

Return code as a string."

  (let ((info (org-export--collect-tree-info
               backend subtreep visible-only body-only)))
    (org-export--transcode-headline (plist-get info :parse-tree) info)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code specific to the multipage backend (currently html centric)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun replace-chars-with-dash (chars string)
  (cl-reduce (lambda (accum x) (replace-regexp-in-string (format "%s+" x) "-" accum))
           chars
           :initial-value string))

(defun remove-chars (chars string)
  (cl-reduce (lambda (accum x) (replace-regexp-in-string (format "%s+" x) "" accum))
           chars
           :initial-value string))

(defun string-to-backend-filename (string extension)
   (format
    "%s%s"
    (remove-chars
     '("(" ")" "," ";" "{" "}" "'" "\\")
     (replace-chars-with-dash
      '(" " "_" ":" "/")
      (downcase (string-trim string))))
    extension))

(defun string-prepend-chapters (string levels maxlevel)
  "Prepend the chapter outline numbers given in levels to
string. Truncate levels to maxlevel or pad with zeroes if
required."
  (cl-loop
   for i from (1- maxlevel) downto 0
   with result = string
   do (setq result (format "%02d_%s" (or (nth i levels) 0) result))
   finally (return result)))

(defun org-export--make-section-url-lookup (headline-numbering extension)
  "Return an assoc-list containing entries for all headline-numbers
with a plist containing title and url entries for the section and
its navigation."
  (let ((url-lookup
         (cl-loop
          for curr-entry on headline-numbering
          for next = (cadr curr-entry)
          for prev-title = nil then curr-title
          for curr-title = (org-element-property :raw-value (caar curr-entry)) then next-title
          for next-title = (org-element-property :raw-value (car next))
          for prev-url = nil then curr-url
          for curr-url = (string-prepend-chapters
                          (string-to-backend-filename curr-title extension)
                          (cdar curr-entry)
                          org-export-headline-levels)
          then next-url 
          for next-url = (and next
                              (string-prepend-chapters
                               (string-to-backend-filename next-title extension)
                               (cdr next)
                               org-export-headline-levels))
          collect (list (cdar curr-entry)
                        :section-title curr-title :section-url curr-url
                        :section-title-next next-title :section-url-next next-url
                        :section-title-prev prev-title :section-url-prev prev-url))))
    (dolist (lookup url-lookup)
      (let ((up (cdr (assoc (butlast (car lookup)) url-lookup))))
        (when up
          (plist-put (cdr lookup) :section-up-title (plist-get up :section-title))
          (plist-put (cdr lookup) :section-up-url (plist-get up :section-url)))))
    url-lookup))

(defun org-export--collect-multipage-tree-properties (info)
  "add :section-url-lookup entry to info. INFO is used as communication
channel."
  (org-combine-plists
   info
   (list :section-url-lookup
         (org-export--make-section-url-lookup
          (plist-get info :headline-numbering)
          (plist-get info :file-extension)))))

(defun org-export--get-headline-number (element info)
  "return the headline-numbering of the page containing
element. This requires that :headline-numbering has already been
added to info (done in org-export--collect-tree-properties)."
  (let* ((headline-numbering (plist-get info :headline-numbering))
         (elem element)
         (headline-number (alist-get elem headline-numbering)))
    (while (and elem (not headline-number))
      (setf elem (org-element-property :parent elem))
      (setq headline-number (alist-get elem headline-numbering)))
    headline-number))

(defun get-headline-from-link-name (name tree info)
  "utility function to obtain the headline-numbering from a supplied
link name in tree."
  (org-element--get-headline-number
   (org-export-resolve-fuzzy-link
    (org-element-map tree 'link
      (lambda (link)
        (if (equal (org-element-property :path link) name)
            link))
      nil t)
    info)
   info))

(defun org-remove-subheadlines (org-node &optional remove-parent)
  "remove all elements starting with and including the first headline
in the children of org-node. Returns a new ast with all elements
copied into it with the :parent property removed in the top node."
  (let* (contents-end
         headline
         (props (copy-sequence (nth 1 org-node)))
         (new (list (car org-node) props))
         (new-children (cl-loop
                        for x in (org-element-contents org-node)
                        for i from 0
                        do (if (and
                                (consp x)
                                (eq (org-element-type x) 'headline))
                               (progn
                                 (setq contents-end (org-element-property :begin x))
                                 (setq headline t)))
                        until headline
                        collect x)))
    (if remove-parent (cl-remf props :parent))
    (apply 'org-element-adopt-elements new new-children)))

(defun write-string-to-file (string encoding filename)
  (let ((coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (write-region (encode-coding-string string encoding)
                  nil filename nil :silent)
    nil))

(defun org-export-multipage-to-dir
    (backend dir &optional async subtreep visible-only body-only ext-plist
	     post-process)
  "Call `org-export--transcode-headline' (in combination with
`org-export--collect-tree-info' this replaces `org-export-as' in
single-page export) on all section subtrees with output to a
specified dir.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.  DIR is the name of the output dir, as
a string.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer will then be accessible
through the `org-export-stack' interface.

Optional arguments SUBTREEP, VISIBLE-ONLY, BODY-ONLY and
EXT-PLIST are similar to those used in `org-export-as', which
see.

Optional argument POST-PROCESS is called with DIR as its
argument and happens asynchronously when ASYNC is non-nil.  It
has to return a dir name, or nil.  Export back-ends can use this
to send the output dir through additional processing, e.g,

  (defun org-latex-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
    (interactive)
    (let ((outfile (org-export-output-file-name \".tex\" subtreep)))
      (org-export-to-file \\='latex outfile
        async subtreep visible-only body-only ext-plist
        #\\='org-latex-compile)))

When expressed as an anonymous function, using `lambda',
POST-PROCESS needs to be quoted.

The function returns either a file name returned by POST-PROCESS,
or DIR."
  (declare (indent 2))
  (if (not (file-writable-p dir)) (error "Output dir not writable")
    (let* ((ext-plist (org-combine-plists `(:output-dir ,dir
                                                        :file-extension ,extension
                                                        :multipage t)
                                          ext-plist))
	   (encoding (or org-export-coding-system buffer-file-coding-system))
           (info ;;; NEW: add a multipage processing hook returning an updated info plist.                  
            (org-export--collect-multipage-tree-properties
             (org-export--collect-tree-info ;;; here everything happens!!!
              backend subtreep visible-only body-only ext-plist)))
           (headline-numbering (plist-get info :headline-numbering))
           (subtree-headline-numbering
            (mapcar
             (lambda (section-entry)
               (cons (org-remove-subheadlines
                      (car section-entry))
                     (cdr section-entry)))
             headline-numbering))
           (section-url-lookup (plist-get info :section-url-lookup))
           (section-trees (mapcar 'car subtree-headline-numbering))
           (section-filenames (mapcar
                               (lambda (headline-number)
                                 (plist-get
                                  (alist-get
                                   (cdr headline-number)
                                   section-url-lookup)
                                  :section-url))
                               headline-numbering)))
      ;;; add the section subtrees to :headline-numbering, necessary
      ;;; to make the headline-numbering accessible when generating
      ;;; the body of the individual pages.
      (plist-put info :headline-numbering
                 (append
                  headline-numbering
                  subtree-headline-numbering))
      (setq global-info info) ;;; for debugging purposes, remove later
      (cl-loop
       for file in section-filenames
       for tree in section-trees
       do
       (if async
           (org-export-async-start
               (lambda (file) (org-export-add-to-stack (expand-file-name file) backend))
             `(let ((output (org-export--transcode-headline tree info))
                    (file (format "%s/%s" dir file)))
                (message "writing '%s'" file)
                (write-string-to-file output encoding file)
                (or (ignore-errors (funcall ',post-process ,file)) ,file)))
         (let ((output (org-export--transcode-headline tree info))
               (file (format "%s/%s" dir file)))
           (message "writing '%s'" file)
           (write-string-to-file output encoding file)
           (when (and (org-export--copy-to-kill-ring-p) (org-string-nw-p output))
             (org-kill-new output))
           ;; Get proper return value.
           (or (and (functionp post-process) (funcall post-process file))
               file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; code specific to html multipage export
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-html-export-to-multipage-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output directory's name."
  (interactive)
  (let* ((extension (concat
		     (when (> (length org-html-extension) 0) ".")
		     (or (plist-get ext-plist :html-extension)
			 org-html-extension
			 "html")))
	 (dir (or "html" (org-export-output-file-name "" subtreep)))
	 (org-export-coding-system org-html-coding-system))
    (org-export-multipage-to-dir 'html dir
      async subtreep visible-only body-only ext-plist)))

;;; multipage definition (needs to be put into ox-html to work):

(defun org-html--get-multipage-url (element info)
  "find the headline numbering of the element's page and look up its
url in :section-url-lookup."
  (plist-get
   (cdr
    (assoc
     (org-export--get-headline-number element info)
     (plist-get info :section-url-lookup)
     ))
   :section-url))

(defun org-html--get-multipage-url (element info)
  "find the headline numbering of the element's page and look up its
url in :section-url-lookup."
  (plist-get
   (alist-get
       (org-export--get-headline-number element info)
       (plist-get info :section-url-lookup))
   :section-url))

;;; rewritten functions from ox-html:

(defun org-html--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (pcase type
	     ((or `headline `inlinetask) :CUSTOM_ID)
	     ((or `radio-target `target) :value)
	     (_ :name))
	   datum)))
    (cond
     ((and user-label
	   (or (plist-get info :html-prefer-user-labels)
	       ;; Used CUSTOM_ID property unconditionally.
	       (memq type '(headline inlinetask))))
      (format "#%s" user-label))
     ((and named-only
	   (not (memq type '(headline inlinetask radio-target target)))
	   (not user-label))
      nil)
     (t
      (if (plist-get info :multipage)
          (format "%s#%s"
                  (org-html--get-multipage-url datum info)
                  (org-export-get-reference datum info))
        (format "#%s" (org-export-get-reference datum info)))))))


(defun org-html-link (link desc info)
  "Transcode a LINK object from Org to HTML.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((html-ext (plist-get info :html-extension))
	 (dot (when (> (length html-ext) 0) "."))
	 (link-org-files-as-html-maybe
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
	    ;; needed.  See `org-html-link-org-files-as-html'.
            (save-match-data
	      (cond
	       ((and (plist-get info :html-link-org-files-as-html)
                     (let ((case-fold-search t))
                       (string-match "\\(.+\\)\\.org\\(?:\\.gpg\\)?$" raw-path)))
	        (concat (match-string 1 raw-path) dot html-ext))
	       (t raw-path)))))
	 (type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (org-string-nw-p desc))
	 (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto" "news"))
	    (url-encode-url (concat type ":" raw-path)))
	   ((string= "file" type)
	    ;; During publishing, turn absolute file names belonging
	    ;; to base directory into relative file names.  Otherwise,
	    ;; append "file" protocol to absolute file name.
	    (setq raw-path
		  (org-export-file-uri
		   (org-publish-file-relative-name raw-path info)))
	    ;; Possibly append `:html-link-home' to relative file
	    ;; name.
	    (let ((home (and (plist-get info :html-link-home)
			     (org-trim (plist-get info :html-link-home)))))
	      (when (and home
			 (plist-get info :html-link-use-abs-url)
			 (file-name-absolute-p raw-path))
		(setq raw-path (concat (file-name-as-directory home) raw-path))))
	    ;; Maybe turn ".org" into ".html".
	    (setq raw-path (funcall link-org-files-as-html-maybe raw-path info))
	    ;; Add search option, if any.  A search option can be
	    ;; relative to a custom-id, a headline title, a name or
	    ;; a target.
	    (let ((option (org-element-property :search-option link)))
	      (if (not option) raw-path
		(let ((path (org-element-property :path link)))
		  (concat raw-path
			  "#"
			  (org-publish-resolve-external-link option path t))))))
	   (t raw-path)))
	 (attributes-plist
	  (org-combine-plists
	   ;; Extract attributes from parent's paragraph.  HACK: Only
	   ;; do this for the first link in parent (inner image link
	   ;; for inline images).  This is needed as long as
	   ;; attributes cannot be set on a per link basis.
	   (let* ((parent (org-export-get-parent-element link))
		  (link (let ((container (org-export-get-parent link)))
			  (if (and (eq 'link (org-element-type container))
				   (org-html-inline-image-p link info))
			      container
			    link))))
	     (and (eq link (org-element-map parent 'link #'identity info t))
		  (org-export-read-attribute :attr_html parent)))
	   ;; Also add attributes from link itself.  Currently, those
	   ;; need to be added programmatically before `org-html-link'
	   ;; is invoked, for example, by backends building upon HTML
	   ;; export.
	   (org-export-read-attribute :attr_html link)))
	 (attributes
	  (let ((attr (org-html--make-attribute-string attributes-plist)))
	    (if (org-string-nw-p attr) (concat " " attr) ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; Image file.
     ((and (plist-get info :html-inline-images)
	   (org-export-inline-image-p
	    link (plist-get info :html-inline-image-rules)))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(if (not destination) desc
	  (format "<a href=\"#%s\"%s>%s</a>"
		  (org-export-get-reference destination info)
		  attributes
		  desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  ;; ID link points to an external file.
	  (`plain-text
	   (let ((fragment (concat "ID-" path))
		 ;; Treat links to ".org" files as ".html", if needed.
		 (path (funcall link-org-files-as-html-maybe
				destination info)))
	     (format "<a href=\"%s#%s\"%s>%s</a>"
		     path fragment attributes (or desc destination))))
	  ;; Fuzzy link points nowhere.
	  (`nil
	   (format "<i>%s</i>"
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Link points to a headline.
	  (`headline
	   (let ((href (org-html--reference destination info))
		 ;; What description to use?
		 (desc
		  ;; Case 1: Headline is numbered and LINK has no
		  ;; description.  Display section number.
		  (if (and (org-export-numbered-headline-p destination info)
			   (not desc))
		      (mapconcat #'number-to-string
				 (org-export-get-headline-number
				  destination info) ".")
		    ;; Case 2: Either the headline is un-numbered or
		    ;; LINK has a custom description.  Display LINK's
		    ;; description or headline's title.
		    (or desc
			(org-export-data
			 (org-element-property :title destination) info)))))
	     (format "<a href=\"%s\"%s>%s</a>" href attributes desc)))
	  ;; Fuzzy link points to a target or an element.
	  (_
           (if (and destination
                    (memq (plist-get info :with-latex) '(mathjax t))
                    (eq 'latex-environment (org-element-type destination))
                    (eq 'math (org-latex--environment-type destination)))
               ;; Caption and labels are introduced within LaTeX
	       ;; environment.  Use "ref" or "eqref" macro, depending on user
               ;; preference to refer to those in the document.
               (format (plist-get info :html-equation-reference-format)
                       (org-html--reference destination info))
             (let* ((ref (org-html--reference destination info))
                    (org-html-standalone-image-predicate
                     #'org-html--has-caption-p)
                    (counter-predicate
                     (if (eq 'latex-environment (org-element-type destination))
                         #'org-html--math-environment-p
                       #'org-html--has-caption-p))
                    (number
		     (cond
		      (desc nil)
		      ((org-html-standalone-image-p destination info)
		       (org-export-get-ordinal
			(org-element-map destination 'link #'identity info t)
			info '(link) 'org-html-standalone-image-p))
		      (t (org-export-get-ordinal
			  destination info nil counter-predicate))))
                    (desc
		     (cond (desc)
			   ((not number) "No description for this link")
			   ((numberp number) (number-to-string number))
			   (t (mapconcat #'number-to-string number ".")))))
               (format "<a href=\"%s\"%s>%s</a>" ref attributes desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" (org-html-encode-plain-text path))))
	(format "<a href=\"#%s\" %s%s>%s</a>"
		fragment
		(format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, \
'%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
			fragment fragment)
		attributes
		(format (org-export-get-coderef-format path desc)
			(org-export-resolve-coderef path info)))))
     ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\"%s>%s</a>"
	      (org-html-encode-plain-text path)
	      attributes
	      desc))
     ;; External link without a description part.
     (path
      (let ((path (org-html-encode-plain-text path)))
	(format "<a href=\"%s\"%s>%s</a>" path attributes path)))
     ;; No path, only description.  Try to do something useful.
     (t
      (format "<i>%s</i>" desc)))))

(defun org-html--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
	 (todo (and (plist-get info :with-todo-keywords)
		    (let ((todo (org-element-property :todo-keyword headline)))
		      (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 (text (org-export-data-with-backend
		(org-export-get-alt-title headline info)
		(org-export-toc-entry-backend 'html)
		info))
	 (tags (and (eq (plist-get info :with-tags) t)
		    (org-export-get-tags headline info))))
    (format "<a href=\"%s\">%s</a>"
            ;; Label
            (if (plist-get info :multipage)
                (format "%s" (plist-get
                              (alist-get
                               headline-number
                               (plist-get info :section-url-lookup))
                              :section-url))
              (format "#%s" (org-html--reference headline info)))
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (concat (mapconcat #'number-to-string headline-number ".")
			  ". "))
	     (apply (plist-get info :html-format-headline-function)
		    todo todo-type priority text tags :section-number nil)))))
