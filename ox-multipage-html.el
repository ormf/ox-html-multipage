;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; changes in ox.el to generalize exporting, facilitating multipage
;;; export.
;;;
;;; `org-export-as' is modified to generalize ox to facilitate
;;; exporting to multiple pages: Its former body is split into two
;;; external functions, which can get used by other backends like
;;; multipage:
;;;
;;; 1. `org-export--collect-tree-info' collects the complete
;;;    parse-tree plus necessary information in info.
;;;
;;; 2. `org-export--transcode-headline' generates the string for the
;;;    output file.
;;;
;;; The redefinition of `org-export-as' should behave identically to
;;; its previous definition.
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
           ;;           (setq global-tree1 (org-remove-subheadlines tree))
	   ;; Now tree is complete, compute its properties and add them
	   ;; to communication channel.
	   (setq info (org-export--collect-tree-properties tree info))
           (setq global-tree tree)
           ;; Process citations and bibliography.  Replace each citation
	   ;; and "print_bibliography" keyword in the parse tree with
	   ;; the output of the selected citation export processor.
           (org-cite-process-citations info)
           (org-cite-process-bibliography info)
	   ;; Eventually transcode TREE.  Wrap the resulting string into
	   ;; a template.
;;;           (plist-put info :parse-tree (car (nth 0 (plist-get info :headline-numbering))))
           (setq global-info info)
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

(require 'f)

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
    "%s.%s"
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
  "for all headline-numbers create an assoc-list entry with the
title and a filename"
  (cl-loop
   for entry
   in headline-numbering
   collect (let* ((headline-numbers (cdr entry))
                  (title (org-element-property :raw-value (car entry)))
                  (filename (string-prepend-chapters
                             (string-to-backend-filename title extension)
                             headline-numbers
                             org-export-headline-levels)))
             (list headline-numbers title filename))))

(defun org-export--collect-multipage-tree-properties (info)
  (org-combine-plists
   info
   (list :section-url-lookup
         (org-export--make-section-url-lookup
          (plist-get info :headline-numbering)
          (plist-get info :file-extension)))))

(defun get-link-headline-number (link info)
  "return the headline-numbering from given link element. This
requires that :headline-numbering has already been added to info
(done in org-export--collect-tree-properties)."
  (let* ((headline-numbering (plist-get info :headline-numbering))
         (elem link)
         (headline-number (alist-get headline-numbering elem)))
    (while (and elem (not headline-number))
      (setf elem (org-element-property :parent elem))
      (setq headline-number (alist-get headline-numbering elem)))
    headline-number))

(defun get-headline-from-link-name (name tree info)
  "utility function to obtain the headline-numbering from a supplied
link name in tree."
  (get-link-headline-number
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
    (let* ((ext-plist (org-combine-plists `(:output-dir ,dir :file-extension ,extension) ext-plist))
	   (encoding (or org-export-coding-system buffer-file-coding-system))
           (info ;;; NEW: add a multipage processing hook returning an updated info plist.                  
            (org-export--collect-multipage-tree-properties
             (org-export--collect-tree-info ;;; here everything happens!!!
              backend subtreep visible-only body-only ext-plist)))
           (subtree-headline-numbering '())
           (headline-numbering (plist-get info :headline-numbering))
           (section-url-lookup (plist-get info :section-url-lookup))
           (section-trees (mapcar
                           (lambda (section-entry)
                             (let ((subtree
                                    (org-remove-subheadlines
                                     (car section-entry))))
                               (push
                                (cons subtree (cdr section-entry))
                                subtree-headline-numbering)
                               subtree))
                           headline-numbering))
           (section-filenames (mapcar
                               (lambda (headline-number)
                                 (nth 1
                                      (alist-get
                                       (cdr headline-number)
                                       section-url-lookup)))
                               headline-numbering)))
      ;;; add the section subtrees to :headline-numbering, necessary
      ;;; to make the headline-numbering accessible when generating
      ;;; the body of the individual pages.
      (plist-put info :headline-numbering
                 (append
                  headline-numbering
                  (reverse subtree-headline-numbering)))
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
                (f-write-text output encoding file)
                (or (ignore-errors (funcall ',post-process ,file)) ,file)))
         (let ((output (org-export--transcode-headline tree info))
               (file (format "%s/%s" dir file)))
           (message "writing '%s'" file)
           (f-write-text output encoding file)
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


;;; LATER: rename to org-html-multipage-toc and add to html-multipage
;;; template

;;; original definition:

(defun org-html-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-html--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let ((toc (concat "<div id=\"text-table-of-contents\" role=\"doc-toc\">"
			 (org-html--toc-text toc-entries)
			 "</div>\n")))
	(if scope toc
	  (let ((outer-tag (if (org-html--html5-fancy-p info)
			       "nav"
			     "div")))
	    (concat (format "<%s id=\"table-of-contents\" role=\"doc-toc\">\n" outer-tag)
		    (let ((top-level (plist-get info :html-toplevel-hlevel)))
		      (format "<h%d>%s</h%d>\n"
			      top-level
			      (org-html--translate "Table of Contents" info)
			      top-level))
		    toc
		    (format "</%s>\n" outer-tag))))))))

;;; multipage definition:

(defun org-html--get-multipage-url (headline info)
  (caddr (assq (cdr (assq headline (plist-get info :headline-numbering)))
               (plist-get info :section-url-lookup))))

(defun org-html--format-multipage-toc-headline (headline info)
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
    (format "<a href=\"./%s\">%s</a>"
	    ;; Label.
            (org-html--get-multipage-url headline info)
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (concat (mapconcat #'number-to-string headline-number ".")
			  ". "))
	     (apply (plist-get info :html-format-headline-function)
		    todo todo-type priority text tags :section-number nil)))))


(defun org-html-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-html--format-multipage-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let ((toc (concat "<div id=\"text-table-of-contents\" role=\"doc-toc\">"
			 (org-html--toc-text toc-entries)
			 "</div>\n")))
	(if scope toc
	  (let ((outer-tag (if (org-html--html5-fancy-p info)
			       "nav"
			     "div")))
	    (concat (format "<%s id=\"table-of-contents\" role=\"doc-toc\">\n" outer-tag)
		    (let ((top-level (plist-get info :html-toplevel-hlevel)))
		      (format "<h%d>%s</h%d>\n"
			      top-level
			      (org-html--translate "Table of Contents" info)
			      top-level))
		    toc
		    (format "</%s>\n" outer-tag))))))))
