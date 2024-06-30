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
;;; 2. `org-export--transcode-headline' takes a headline and the info(caar)
;;;    proplist as arguments to generate the string for the output
;;;    file.
;;;
;;; The redefinition of `org-export-as' below should behave
;;; identically to its previous definition.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; `org-export-collect-footnote-definitions' is changed to obtain the
;;; reference number the same way as in the footnote reference itself
;;; rather than counting from zero as implemented before.

(defun org-export-collect-footnote-definitions (info &optional data body-first)
  "Return an alist between footnote numbers, labels and definitions.

INFO is the current export state, as a plist.

Definitions are collected throughout the whole parse tree, or
DATA when non-nil.

Sorting is done by order of references.  As soon as a new
reference is encountered, other references are searched within
its definition.  However, if BODY-FIRST is non-nil, this step is
delayed after the whole tree is checked.  This alters results
when references are found in footnote definitions.

Definitions either appear as Org data or as a secondary string
for inlined footnotes.  Unreferenced definitions are ignored."
  (let (labels alist)
    (org-export--footnote-reference-map
     (lambda (f)
       ;; Collect footnote number, label and definition.
       (let ((l (org-element-property :label f)))
	 (unless (and l (member l labels))
	   (let ((n (org-export-get-footnote-number f info)))
             (push (list n l (org-export-get-footnote-definition f info)) alist)))
	 (when l (push l labels))))
     (or data (plist-get info :parse-tree)) info body-first)
    (nreverse alist)))

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
                         (funcall inner-template body info headline))
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
           ;; each entry in headline-numbering will become a single
           ;; page in multipage output. We first generate a new
           ;; headline-numbering with each entry being a copy of the
           ;; original enries with removed subheadlines.
           (stripped-section-headline-numbering
            (mapcar
             (lambda (section-entry)
               (cons (org-remove-subheadlines
                      (car section-entry))
                     (cdr section-entry)))
             headline-numbering))
           ;; section-trees is a list of all sections which get
           ;; exported to a single page
           (section-trees (mapcar 'car stripped-section-headline-numbering))
           (section-url-lookup (plist-get info :section-url-lookup))
           (section-filenames (mapcar
                               (lambda (section)
                                 (org-html--get-multipage-page-url section info))
                               (mapcar 'car headline-numbering))))
      ;; add stripped-section-headline-numbering to
      ;; :headline-numbering, to make the headline-numbering
      ;; accessible when generating the body of the individual pages.
      (plist-put info :headline-numbering
                 (append
                  headline-numbering
                  stripped-section-headline-numbering))
      (plist-put info :section-filenames section-filenames)
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

(defun org-html--get-multipage-section-urls (element info)
  "Return the section-url plist of the page containing ELEMENT."
  (alist-get
   (org-export--get-headline-number element info)
   (plist-get info :section-url-lookup)))

(defun org-html--get-multipage-page-url (element info)
  "Return the url of the page containing ELEMENT."
  (plist-get (org-html--get-multipage-section-urls element info) :section-url))

;;; rewritten functions from ox-html:

;;; `org-html--reference' contains an additional predicate argument
;;; STRIPPED which determines whether the reference should be just the
;;; org-reference generated by org, or have the form
;;; \"page-url#org-reference\", needed for anchor href elements in
;;; multipage situations. Note that for single-page output the
;;; function will *always* return the stripped reference regardless of
;;; the supplied STRIPPED argument.

(defun org-html--reference (datum info &optional named-only stripped)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets.

When STRIPPED is nil and info contains the :multipage predicate,
return the reference with full url-name of the page in the form
\"page-url#org-reference\", in all other cases return
\"org-reference\".
"
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
      (format "%s" user-label))
     ((and named-only
	   (not (memq type '(headline inlinetask radio-target target)))
	   (not user-label))
      nil)
     (t
      (if (and (plist-get info :multipage) (not stripped))
          (format "%s#%s"
                  (org-html--get-multipage-page-url datum info)
                  (org-export-get-reference datum info))
        (format "%s" (org-export-get-reference datum info)))))))

(defun org-html-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (when (eq (org-element-type prev) 'footnote-reference)
       (plist-get info :html-footnote-separator)))
   (let* ((n (org-export-get-footnote-number footnote-reference info))
	  (id (format "fnr.%d%s"
		      n
		      (if (org-export-footnote-first-reference-p
			   footnote-reference info)
			  ""
			".100"))))
     (format
      (plist-get info :html-footnote-format)

      (org-html--anchor
       id n
       ;;; we probably don't need this as footnotes are *always* on
       ;;; the same page as the reference. It's enough to make sure,
       ;;; footnotes are only appearing on the page where they are
       ;;; referenced.
       (if nil ;;; (plist-get info :multipage)
           (format " class=\"footref\" href=\"%s#fn.%d\" role=\"doc-backlink\""
                   (org-html--get-multipage-url footnote-reference info) n)
         (format " class=\"footref\" href=\"#fn.%d\" role=\"doc-backlink\"" n))
       info)))))

;;; we need data in the function args for multipage.

(defun org-html-footnote-section (info &optional data)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (pcase (org-export-collect-footnote-definitions info data)
    (`nil nil)
    (definitions
      (format
       (plist-get info :html-footnotes-section)
       (org-html--translate "Footnotes" info)
       (format
	"\n%s\n"
	(mapconcat
	 (lambda (definition)
	   (pcase definition
	     (`(,n ,_ ,def)
	      ;; `org-export-collect-footnote-definitions' can return
	      ;; two kinds of footnote definitions: inline and blocks.
	      ;; Since this should not make any difference in the HTML
	      ;; output, we wrap the inline definitions within
	      ;; a "footpara" class paragraph.
	      (let ((inline? (not (org-element-map def org-element-all-elements
				    #'identity nil t)))
		    (anchor (org-html--anchor
			     (format "fn.%d" n)
			     n
			     (format " class=\"footnum\" href=\"#fnr.%d\" role=\"doc-backlink\"" n)
			     info))
		    (contents (org-trim (org-export-data def info))))
		(format "<div class=\"footdef\">%s %s</div>\n"
			(format (plist-get info :html-footnote-format) anchor)
			(format "<div class=\"footpara\" role=\"doc-footnote\">%s</div>"
				(if (not inline?) contents
				  (format "<p class=\"footpara\">%s</p>"
					  contents))))))))
	 definitions
	 "\n"))))))

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
	     (if (plist-get info :multipage)
                 (format "<a href=\"%s\"%s>%s</a>" href attributes desc)
               (format "<a href=\"#%s\"%s>%s</a>" href attributes desc))))
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
               (if (plist-get info :multipage)
                   (format "<a href=\"%s\"%s>%s</a>" ref attributes desc)
                 (format "<a href=\"#%s\"%s>%s</a>" ref attributes desc))))))))
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

(defun org-html-footnote-section (info &optional data)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (pcase (org-export-collect-footnote-definitions info data)
    (`nil nil)
    (definitions
      (format
       (plist-get info :html-footnotes-section)
       (org-html--translate "Footnotes" info)
       (format
	"\n%s\n"
	(mapconcat
	 (lambda (definition)
	   (pcase definition
	     (`(,n ,_ ,def)
	      ;; `org-export-collect-footnote-definitions' can return
	      ;; two kinds of footnote definitions: inline and blocks.
	      ;; Since this should not make any difference in the HTML
	      ;; output, we wrap the inline definitions within
	      ;; a "footpara" class paragraph.
	      (let ((inline? (not (org-element-map def org-element-all-elements
				    #'identity nil t)))
		    (anchor (org-html--anchor
			     (format "fn.%d" n)
			     n
			     (format " class=\"footnum\" href=\"#fnr.%d\" role=\"doc-backlink\"" n)
			     info))
		    (contents (org-trim (org-export-data def info))))
		(format "<div class=\"footdef\">%s %s</div>\n"
			(format (plist-get info :html-footnote-format) anchor)
			(format "<div class=\"footpara\" role=\"doc-footnote\">%s</div>"
				(if (not inline?) contents
				  (format "<p class=\"footpara\">%s</p>"
					  contents))))))))
	 definitions
	 "\n"))))))

(defun org-html-nav (info data)
  "Return nav string for multipage Navigation.

INFO is a plist used as a communication channel.

DATA contains the supbtree of the section/page to export
"
  (let ((section-urls )))

  "")

(defun org-html-inner-template (contents info &optional data)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.

DATA contains the subtree of the parse tree of the section to be
exported for multipage export.
"
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; Navigation
   (if (plist-get info :multipage)
       (org-html-nav info data)
     "")
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info data)))




;;; adjust different reference sections:

(defun org-html-list-of-listings (info)
  "Build a list of listings.
INFO is a plist used as a communication channel.  Return the list
of listings as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-listings info)))
    (when lol-entries
      (concat "<div id=\"list-of-listings\">\n"
	      (let ((top-level (plist-get info :html-toplevel-hlevel)))
		(format "<h%d>%s</h%d>\n"
			top-level
			(org-html--translate "List of Listings" info)
			top-level))
	      "<div id=\"text-list-of-listings\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"listing-number\">%s</span>"
					 (org-html--translate "Listing %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-html--reference entry info t t))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (cl-incf count))
				  " "
				  title)
			(format "<a href=\"#%s\">%s %s</a>"
				label
				(format initial-fmt (cl-incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))

(defun org-html-list-of-tables (info)
  "Build a list of tables.
INFO is a plist used as a communication channel.  Return the list
of tables as a string, or nil if it is empty."
  (let ((lol-entries (org-export-collect-tables info)))
    (when lol-entries
      (concat "<div id=\"list-of-tables\">\n"
	      (let ((top-level (plist-get info :html-toplevel-hlevel)))
		(format "<h%d>%s</h%d>\n"
			top-level
			(org-html--translate "List of Tables" info)
			top-level))
	      "<div id=\"text-list-of-tables\">\n<ul>\n"
	      (let ((count 0)
		    (initial-fmt (format "<span class=\"table-number\">%s</span>"
					 (org-html--translate "Table %d:" info))))
		(mapconcat
		 (lambda (entry)
		   (let ((label (org-html--reference entry info t t))
			 (title (org-trim
				 (org-export-data
				  (or (org-export-get-caption entry t)
				      (org-export-get-caption entry))
				  info))))
		     (concat
		      "<li>"
		      (if (not label)
			  (concat (format initial-fmt (cl-incf count))
				  " "
				  title)
			(format "<a href=\"#%s\">%s %s</a>"
				label
				(format initial-fmt (cl-incf count))
				title))
		      "</li>")))
		 lol-entries "\n"))
	      "\n</ul>\n</div>\n</div>"))))

(defun org-html-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((attributes (org-export-read-attribute :attr_html example-block)))
    (if (plist-get attributes :textarea)
	(org-html--textarea-block example-block)
      (format "<pre class=\"example\"%s>\n%s</pre>"
	      (let* ((reference (org-html--reference example-block info nil t))
		     (a (org-html--make-attribute-string
			 (if (or (not reference) (plist-member attributes :id))
			     attributes
			   (plist-put attributes :id reference)))))
		(if (org-string-nw-p a) (concat " " a) ""))
	      (org-html-format-code example-block info)))))

(defun org-html-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language inline-src-block))
	 (code (org-html-fontify-code
		(org-element-property :value inline-src-block)
		lang))
	 (label
	  (let ((lbl (org-html--reference inline-src-block info t t)))
	    (if (not lbl) "" (format " id=\"%s\"" lbl)))))
    (format "<code class=\"src src-%s\"%s>%s</code>" lang label code)))

(defun org-html-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent paragraph))
	 (parent-type (org-element-type parent))
	 (style '((footnote-definition " class=\"footpara\"")
		  (org-data " class=\"footpara\"")))
	 (attributes (org-html--make-attribute-string
		      (org-export-read-attribute :attr_html paragraph)))
	 (extra (or (cadr (assq parent-type style)) "")))
    (cond
     ((and (eq parent-type 'item)
	   (not (org-export-get-previous-element paragraph info))
	   (let ((followers (org-export-get-next-element paragraph info 2)))
	     (and (not (cdr followers))
		  (memq (org-element-type (car followers)) '(nil plain-list)))))
      ;; First paragraph in an item has no tag if it is alone or
      ;; followed, at most, by a sub-list.
      contents)
     ((org-html-standalone-image-p paragraph info)
      ;; Standalone image.
      (let ((caption
	     (let ((raw (org-export-data
			 (org-export-get-caption paragraph) info))
		   (org-html-standalone-image-predicate
		    #'org-html--has-caption-p))
	       (if (not (org-string-nw-p raw)) raw
		 (concat "<span class=\"figure-number\">"
			 (format (org-html--translate "Figure %d:" info)
				 (org-export-get-ordinal
				  (org-element-map paragraph 'link
				    #'identity info t)
				  info nil #'org-html-standalone-image-p))
			 " </span>"
			 raw))))
	    (label (org-html--reference paragraph info nil t)))
	(org-html--wrap-image contents info caption label)))
     ;; Regular paragraph.
     (t (format "<p%s%s>\n%s</p>"
		(if (org-string-nw-p attributes)
		    (concat " " attributes) "")
		extra contents)))))

(defun org-html-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (format "<blockquote%s>\n%s</blockquote>"
	  (let* ((reference (org-html--reference quote-block info t t))
		 (attributes (org-export-read-attribute :attr_html quote-block))
		 (a (org-html--make-attribute-string
		     (if (or (not reference) (plist-member attributes :id))
			 attributes
		       (plist-put attributes :id reference)))))
	    (if (org-string-nw-p a) (concat " " a) ""))
	  contents))

(defun org-html-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((block-type (org-element-property :type special-block))
         (html5-fancy (and (org-html--html5-fancy-p info)
                           (member block-type org-html-html5-elements)))
         (attributes (org-export-read-attribute :attr_html special-block)))
    (unless html5-fancy
      (let ((class (plist-get attributes :class)))
        (setq attributes (plist-put attributes :class
                                    (if class (concat class " " block-type)
                                      block-type)))))
    (let* ((contents (or contents ""))
	   (reference (org-html--reference special-block info nil t))
	   (a (org-html--make-attribute-string
	       (if (or (not reference) (plist-member attributes :id))
		   attributes
		 (plist-put attributes :id reference))))
	   (str (if (org-string-nw-p a) (concat " " a) "")))
      (if html5-fancy
	  (format "<%s%s>\n%s</%s>" block-type str contents block-type)
	(format "<div%s>\n%s\n</div>" str contents)))))

(defun org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
	   (code (org-html-format-code src-block info))
	   (label (let ((lbl (org-html--reference src-block info t t)))
		    (if lbl (format " id=\"%s\"" lbl) "")))
	   (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
					   "ruby" "scheme" "clojure" "php" "html")))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format "<div class=\"org-src-container\">\n%s%s\n</div>"
		;; Build caption.
		(let ((caption (org-export-get-caption src-block)))
		  (if (not caption) ""
		    (let ((listing-number
			   (format
			    "<span class=\"listing-number\">%s </span>"
			    (format
			     (org-html--translate "Listing %d:" info)
			     (org-export-get-ordinal
			      src-block info nil #'org-html--has-caption-p)))))
		      (format "<label class=\"org-src-name\">%s%s</label>"
			      listing-number
			      (org-trim (org-export-data caption info))))))
		;; Contents.
		(if klipsify
		    (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
			    lang
			    label
			    (if (string= lang "html")
				" data-editor-type=\"html\""
			      "")
			    code)
		  (format "<pre class=\"src src-%s\"%s>%s</pre>"
                          lang label code)))))))

(defun org-html-table (table contents info)
  "Transcode a TABLE element from Org to HTML.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (org-html-table--table.el-table table info)
    ;; Standard table.
    (let* ((caption (org-export-get-caption table))
	   (number (org-export-get-ordinal
		    table info nil #'org-html--has-caption-p))
	   (attributes
	    (org-html--make-attribute-string
	     (org-combine-plists
	      (list :id (org-html--reference table info t t))
	      (and (not (org-html-html5-p info))
		   (plist-get info :html-table-attributes))
	      (org-export-read-attribute :attr_html table))))
	   (alignspec
	    (if (bound-and-true-p org-html-format-table-no-css)
		"align=\"%s\""
	      "class=\"org-%s\""))
	   (table-column-specs
	    (lambda (table info)
	      (mapconcat
	       (lambda (table-cell)
		 (let ((alignment (org-export-table-cell-alignment
				   table-cell info)))
		   (concat
		    ;; Begin a colgroup?
		    (when (org-export-table-cell-starts-colgroup-p
			   table-cell info)
		      "\n<colgroup>")
		    ;; Add a column.  Also specify its alignment.
		    (format "\n%s"
			    (org-html-close-tag
			     "col" (concat " " (format alignspec alignment)) info))
		    ;; End a colgroup?
		    (when (org-export-table-cell-ends-colgroup-p
			   table-cell info)
		      "\n</colgroup>"))))
	       (org-html-table-first-row-data-cells table info) "\n"))))
      (format "<table%s>\n%s\n%s\n%s</table>"
	      (if (equal attributes "") "" (concat " " attributes))
	      (if (not caption) ""
		(format (if (plist-get info :html-table-caption-above)
			    "<caption class=\"t-above\">%s</caption>"
			  "<caption class=\"t-bottom\">%s</caption>")
			(concat
			 "<span class=\"table-number\">"
			 (format (org-html--translate "Table %d:" info) number)
			 "</span> " (org-export-data caption info))))
	      (funcall table-column-specs table info)
	      contents))))

(defun org-html-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language inline-src-block))
	 (code (org-html-fontify-code
		(org-element-property :value inline-src-block)
		lang))
	 (label
	  (let ((lbl (org-html--reference inline-src-block info t t)))
	    (if (not lbl) "" (format " id=\"%s\"" lbl)))))
    (format "<code class=\"src src-%s\"%s>%s</code>" lang label code)))

(defun org-html-target (target _contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((ref (org-html--reference target info nil t)))
    (org-html--anchor ref nil nil info)))
