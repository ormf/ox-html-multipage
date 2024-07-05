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

;;;
;;; CHANGES:
;;;
;;;  - Added "Chapter %s", "Section %s" and "Fig. %s" to
;;;    org-export-dictionary
;;;
;;;  - link reference to an inline image will get "Fig. <num>" label.
;;;
;;;  - link reference to a chapter or section will get "Chapter <num>"
;;;    or "Section <num>" label.
;;;


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
;;; Utils
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(setq org-export-dictionary
      (append
       '(("Section %s"
          ("ar" :default "انظر قسم %s")
          ("cs" :default "sekce %s")
          ("da" :default "afsnit %s")
          ("de" :default "Abschnitt %s")
          ("es" :ascii "seccion %s" :html "secci&oacute;n %s" :default "sección %s")
          ("et" :html "peat&#252;kki %s" :utf-8 "peatükki %s")
;;          ("fa" :default "نمایش بخش %s")
          ("fr" :default "section %s")
          ("it" :default "sezione %s")
;;          ("ja" :default "セクション %s を参照")
          ("nl" :default "sectie %s"
           :html "sectie&nbsp;%s" :latex "sectie~%s")
          ("pt_BR" :html "se&ccedil;&atilde;o %s" :default "seção %s"
           :ascii "secao %s")
          ("ro" :default "secțiunea %s")
          ("ru" :html "&&#1088;&#1072;&#1079;&#1076;&#1077;&#1083; %s"
           :utf-8 "раздел %s")
          ("sl" :default "poglavje %d")
          ("tr" :default "bölüm %s")
;;          ("zh-CN" :html "&#21442;&#35265;&#31532;%s&#33410;" :utf-8 "参见第%s节")
          )
         ("Chapter %s"
          ("ar" :default "الفصل %s")
          ("cs" :default "kapitola %s")
          ("da" :default "kapitel %s")
          ("de" :default "Kapitel %s")
          ("es" :ascii "capitulo %s" :html "cap&iacute;tulo %s" :default "capítulo %s")
          ("et" :html "peat&#252;kk %s" :utf-8 "peatükk %s")
          ("fa" :default "فصل %s")
          ("fr" :default "chapitre %s")
          ("it" :default "capitolo %s")
          ("ja" :default "章 %s")
          ("nl" :default "hoofdstuk %s"
           :html "hoofdstuk&nbsp;%s" :latex "hoofdstuk~%s")
          ("pt_BR" :ascii "capitulo %s" :html "cap&iacute;tulo %s" :default "capítulo %s")
          ("ro" :default "capitol %s")
          ("ru" :html "&#1075;&#1083;&#1072;&#1074;&#1072;&nbsp;%s"
           :utf-8 "глава %s")
          ("sl" :default "odsek %s")
          ("tr" :html "b&#246;l&#252;m" :default "bölüm %s")
          ("zh-CN" :html "&#31456;&#33410;" :utf-8 "章节 %s"))
         ("Fig. %s"
          ("de" :default "Abb. %s")))
       org-export-dictionary))

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
          (cl-remove-if
           (lambda (hl-num)
             (> (length hl-num)
                (or (plist-get info :with-toc) 0)))
           (plist-get info :headline-numbering)
           :key 'cdr)
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

(defun org-remove-subheadlines (org-node &optional join-empty-bodies remove-parent)
  "remove all elements starting with and including the first headline
in the children of ORG-NODE. Returns a new ast with all elements
starting at START-CHILD-IDX copied into it with the :parent
property optionally removed in the top node."
  (let* (contents-end
         headline
         (props (copy-sequence (nth 1 org-node)))
         (new (list (car org-node) props))
         (new-children (cl-loop
                        for x in (org-element-contents org-node)
                        for i from 0
                        with exit = nil
                        do (if (and
                                (consp x)
                                (eq (org-element-type x) 'headline))
                               (if (and join-empty-bodies (= i 0))
                                   (progn
                                     (setq x (org-remove-subheadlines x join-empty-bodies)))
                                 (progn
                                   (setq contents-end (org-element-property :begin x))
                                   (setq exit t))))
                        until exit
                        collect x)))
    (if remove-parent (cl-remf props :parent))
    (apply 'org-element-adopt-elements new new-children)))

(defun find-headline (headline-number headline-numbering)
  "return the headline in headline numbering for a given
headline-number."
  (cond
   ((null headline-numbering) nil)
   ((equal headline-number (cdar headline-numbering))
    (caar headline-numbering))
   (t (find-headline headline-number (cdr headline-numbering)))))

(defun body-empty? (element)
  "check if first child of element is a headline"
  (not (eq (org-element-type (car (org-element-contents element)))
           'headline)))

(defun join-empty-body (headlines)
  (cl-loop for (prev curr-headline) on (cons nil headlines)
           while curr-headline
           if (body-empty? (car prev))
           collect curr-headline))

(defun write-string-to-file (string encoding filename)
  (let ((coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (write-region (encode-coding-string string encoding)
                  nil filename nil :silent)
    nil))

;; TODO: org-html--get-multipage-page-url is html specific, should be
;; generic.
;; org-export--make-section-url-lookup

(defun reverse-assoc-list (assoc-list)
  (mapcar (lambda (entry) (list (cdr entry) (car entry))) assoc-list))

(defun org-html--get-new-section-url-names (info)
  nil
  )


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
           (info ;;; NEW: added a multipage processing hook returning
                 ;;; an updated info plist.
            (org-export--collect-multipage-tree-properties
             (org-export--collect-tree-info ;;; here everything happens!!!
              backend subtreep visible-only body-only ext-plist)))
           (headline-numbering (plist-get info :headline-numbering))
           (join-subheadlines-on-empty-body t)
           (stripped-section-subheadline-numbering '())
           (max-toc-depth (or (plist-get info :with-toc) 0))
           (exported-headline-numbering
            (join-empty-body
             (cl-remove-if (lambda (hl-num) (> (length hl-num) max-toc-depth))
                           headline-numbering :key 'cdr)))
           ;; each entry in exported-headline-numbering will become a
           ;; single page in multipage output. section-trees is a
           ;; list of all sections which get exported to a single page
           (section-trees
            (cl-loop
             for section-entry in exported-headline-numbering
             collect (let* ((section-number (cdr section-entry)))
                       (if (< (length section-number) max-toc-depth)
                           (org-remove-subheadlines
                            (car section-entry)
                            join-subheadlines-on-empty-body)
                         (car section-entry)))))
           ;; stripped-section-headline-numbering is the equivalent of
           ;; headline-numbering but replacing the car of its elements
           ;; with the stripped version of the headlines.
           (stripped-section-headline-numbering
            (cl-mapcar 'cons
                       (cl-loop for section in section-trees
                                ;; collect the section headline rather than the subheadline:
                                append (org-element-map section 'headline (lambda (x) x)))
                       (mapcar 'cdr headline-numbering)))
           (section-filenames (mapcar
                               (lambda (hl-number)
                                 (org-html--headline-number-to-page-url (cdr hl-number) info))
                               exported-headline-numbering))
           ;; lookup from all toc headline-numbers to the tl-headline.
           ;; The headline number of a subheadline in a joined section
           ;; returns the tl-headline of the section/page where it
           ;; appears.
           (tl-hl-lookup
            (let* ((tl-headline-numbers (mapcar 'cdr exported-headline-numbering))
                   (stripped-headlines (reverse-assoc-list stripped-section-headline-numbering))
                   (tl-headline-lookup (mapcar (lambda (x)
                                                 (list x (cadr (assoc x stripped-headlines))))
                                               tl-headline-numbers))
                   (toc-headline-numbers (mapcar 'cdr headline-numbering)))
              (cl-loop
               for toc-entry in toc-headline-numbers
               collect (cl-loop
                        for entry = toc-entry then (butlast entry)
                        for result = (assoc entry tl-headline-lookup)
                        if result return (cons toc-entry (cdr result))))))
           )
      ;; add stripped-section-headline-numbering to
      ;; :headline-numbering, to make the headline-numbering
      ;; accessible when generating the body of the individual pages.
      (plist-put info :headline-numbering
                 (append
                  headline-numbering
                  stripped-section-headline-numbering))
      (plist-put info :tl-hl-lookup tl-hl-lookup)
      (plist-put info :stripped-section-headline-numbering
                 stripped-section-headline-numbering)
      (plist-put info :section-trees section-trees)
      (plist-put info :exported-headline-numbering
                 exported-headline-numbering)
      (plist-put info :section-filenames
                 section-filenames)
      ;; maintain a assoc list between the stripped headlines and the
      ;; original headlines for link lookup of stripped headlines.

      (plist-put info :stripped-hl-to-parse-tree-hl
                 (append
                  (cl-mapcar 'cons
                             (mapcar 'car stripped-section-headline-numbering)
                             (mapcar 'car exported-headline-numbering))
                  (cl-mapcar 'cons
                             (mapcar 'car stripped-section-headline-numbering)
                             (mapcar 'car exported-headline-numbering))))

      (plist-put info :section-filenames section-filenames)
      (plist-put info :new-section-url-names (org-html--get-new-section-url-names info))
      
      (setq global-info info) ;;; for debugging purposes, remove later
      (cl-loop
       for file in section-filenames
       for tl-headline in section-trees
       do
       (progn
         (plist-put info :tl-headline tl-headline)
         (plist-put info :tl-headline-number (alist-get tl-headline stripped-section-headline-numbering))
         (if async
             (org-export-async-start
                 (lambda (file) (org-export-add-to-stack (expand-file-name file) backend))
               `(let ((output (org-export--transcode-headline tl-headline info))
                      (file (format "%s/%s" dir file)))
                  (message "writing '%s'" file)
                  (write-string-to-file output encoding file)
                  (or (ignore-errors (funcall ',post-process ,file)) ,file)))
           (let ((output (org-export--transcode-headline tl-headline info))
                 (file (format "%s/%s" dir file)))
             (message "writing '%s'" file)
             (write-string-to-file output encoding file)
             (when (and (org-export--copy-to-kill-ring-p) (org-string-nw-p output))
               (org-kill-new output))
             ;; Get proper return value.
             (or (and (functionp post-process) (funcall post-process file))
                 file))))))))

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

(defun org-html--headline-number-to-page-url (headline-number info)
  (plist-get
   (alist-get
    headline-number
    (plist-get info :section-url-lookup))
   :section-url))

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
       ;; We don't need the following as footnotes are in most cases
       ;; on the same page as the reference. It's enough to make sure,
       ;; footnotes are only appearing on the page where they are
       ;; referenced. It is included here as a stub for a later option
       ;; for multipage export to make footnotes appear at the end of
       ;; a document on a dedicated page.
       ;;
       ;; (if (plist-get info :multipage)
       ;;     (format " class=\"footref\" href=\"%s#fn.%d\" role=\"doc-backlink\""
       ;;             (org-html--get-multipage-url footnote-reference info) n)
       ;;   (format " class=\"footref\" href=\"#fn.%d\" role=\"doc-backlink\"" n))
       ;;
       (format " class=\"footref\" href=\"#fn.%d\" role=\"doc-backlink\"" n)
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

(defun org-html-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((numberedp (org-export-numbered-headline-p headline info))
           (numbers (org-export-get-headline-number headline info))
           (level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :html-toplevel-hlevel))))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (org-export-data todo info)))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (text (org-export-data (org-element-property :title headline) info))
           (tags (and (plist-get info :with-tags)
                      (org-export-get-tags headline info)))
           (full-text (funcall (plist-get info :html-format-headline-function)
                               todo todo-type priority text tags info))
           (contents (or contents ""))
	   (id (org-html--reference
                (or (alist-get ;; for multipage context, otherwise nil
                     headline
                     (plist-get info :stripped-hl-to-parse-tree-hl))
                    headline)
                info nil t))
	   (formatted-text
	    (if (plist-get info :html-self-link-headlines)
		(format "<a href=\"#%s\">%s</a>" id full-text)
	      full-text)))
      (if (org-export-low-level-p headline info)
          ;; This is a deep sub-tree: export it as a list item.
          (let* ((html-type (if numberedp "ol" "ul")))
	    (concat
	     (and (org-export-first-sibling-p headline info)
		  (apply #'format "<%s class=\"org-%s\">\n"
			 (make-list 2 html-type)))
	     (org-html-format-list-item
	      contents (if numberedp 'ordered 'unordered)
	      nil info nil
	      (concat (org-html--anchor id nil nil info) formatted-text))
             "\n"
	     (and (org-export-last-sibling-p headline info)
		  (format "</%s>\n" html-type))))
	;; Standard headline.  Export it as a section.
        (let ((extra-class
	       (org-element-property :HTML_CONTAINER_CLASS headline))
	      (headline-class
	       (org-element-property :HTML_HEADLINE_CLASS headline))
              (first-content (car (org-element-contents headline))))
          (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                  (org-html--container headline info)
                  (format "outline-container-%s" id)
                  (concat (format "outline-%d" level)
                          (and extra-class " ")
                          extra-class)
                  (format "\n<h%d id=\"%s\"%s>%s</h%d>\n"
                          level
                          id
			  (if (not headline-class) ""
			    (format " class=\"%s\"" headline-class))
                          (concat
                           (and numberedp
                                (format
                                 "<span class=\"section-number-%d\">%s</span> "
                                 level
                                 (concat (mapconcat #'number-to-string numbers ".") ".")))
                           formatted-text)
                          level)
                  ;; When there is no section, pretend there is an
                  ;; empty one to get the correct <div
                  ;; class="outline-...> which is needed by
                  ;; `org-info.js'.
                  (if (eq (org-element-type first-content) 'section) contents
                    (concat (org-html-section first-content "" info) contents))
                  (org-html--container headline info)))))))

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
                      (let ((headline-number (org-export-get-headline-number
                                              destination info)))
                        (if (> (length headline-number) 1)
                            (format
                             (org-html--translate "Section %s" info)
                             (mapconcat #'number-to-string
                                        headline-number "."))
                            (format
                             (org-html--translate "Chapter %s" info)
                             (mapconcat #'number-to-string
                                        headline-number "."))))
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
                    (numbered-ref
		     (cond
		      (desc nil)
		      ((org-html-standalone-image-p destination info)
                       (format (org-html--translate "Fig. %s" info)
                               (org-export-get-ordinal
                                (org-element-map destination 'link #'identity info t)
                                info '(link) 'org-html-standalone-image-p)))
		      (t (org-export-get-ordinal
			  destination info nil counter-predicate))))
                    (desc
		     (cond (desc)
			   ((not numbered-ref) "No description for this link")
			   ((numberp numbered-ref) (number-to-string number))
                           ((stringp numbered-ref) numbered-ref)
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

(defun org-html--toc-text (toc-entries)
  "Return innards of a table of contents, as a string.
TOC-ENTRIES is an alist where key is an entry title, as a string,
and value is its relative level, as an integer."
  (setq debug toc-entries)
  (let* ((prev-level (1- (cddar toc-entries)))
	 (start-level prev-level))
    (concat
     (mapconcat
      (lambda (entry)
	(let ((headline (car entry))
              (hidden (cadr entry))
	      (level (cddr entry)))
	  (concat
	   (format
            (let* ((cnt (- level prev-level))
                   (times (if (> cnt 0) (1- cnt) (- cnt))))
              (setq prev-level level)
              (concat
               (org-html--make-string
                times (cond ((> cnt 0) "\n<ul>\n<li>")
                            ((< cnt 0) "</li>\n</ul>\n")))
               (if (> cnt 0) "\n<ul>\n<li%s>" "</li>\n<li%s>")))
            (if hidden " class=\"toc-hidden\"" ""))
	   headline)))
      toc-entries "")
     (org-html--make-string (- prev-level start-level) "</li>\n</ul>\n"))))

(defun org-html--hidden-in-toc? (headline-number tl-headline-number)
  (and
   (> (length headline-number) 1)
   (let ((l1 (length headline-number))
         (l2 (length tl-headline-number)))
     (cond
      ((= l1 l2) (not (equal (butlast headline-number)
                             (butlast tl-headline-number))))
      ((> l1 l2) (not (equal tl-headline-number
                             (butlast headline-number))))
      (t (not (equal (butlast headline-number)
                     (butlast (butlast tl-headline-number)))))))))

(defun org-html--format-toc-headline (headline info)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((headline-number (org-export-get-headline-number headline info))
         (tl-headline-number (plist-get info :tl-headline-number))
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
    (format "<a %s>%s</a>"
            ;; Target
            (if (plist-get info :multipage)
                (format "href=\"%s\"%s"
                        (concat
                         (plist-get
                          (alist-get
                           headline-number
                           (plist-get info :section-url-lookup))
                          :section-url)
                         (format "#%s" (org-html--reference headline info)))
                        (if (equal headline-number tl-headline-number)
                            "class=\"toc-entry toc-active\""
                          "class=\"toc-entry\""))
              (format "#%s" (org-html--reference headline info)))
	    ;; Body.
	    (concat
	     (and (not (org-export-low-level-p headline info))
		  (org-export-numbered-headline-p headline info)
		  (concat
                   (mapconcat #'number-to-string headline-number ".")
                   "&nbsp;&nbsp;"))
	     (apply (plist-get info :html-format-headline-function)
		    todo todo-type priority text tags :section-number nil)))))

(defun org-html-toc (depth info &optional scope)
  "Build a table of contents.
DEPTH is an integer specifying the depth of the table.  INFO is
a plist used as a communication channel.  Optional argument SCOPE
is an element defining the scope of the table.  Return the table
of contents as a string, or nil if it is empty."
  (let* ((toc-entries
	  (mapcar (lambda (headline)
                    (let ((headline-number
                           (org-export-get-headline-number headline info)))
                      (cl-list*
                       (org-html--format-toc-headline
                        headline
                        info)
                       (org-html--hidden-in-toc? headline-number
                                                 (plist-get info :tl-headline-number))
                       (org-export-get-relative-level headline info))))
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

(defun org-html-nav-left (info data)
  "Return nav string for multipage Navigation.

INFO is a plist used as a communication channel.

DATA contains the supbtree of the section/page to export
"
  (let ((url-prev (plist-get
                   (plist-get info :tl-section-urls)
                   :section-url-prev)))
    (if url-prev
        (format "<nav id=\"nav-left\"><a href=\"%s\" class=\"nav-left\"><i class=\"angle-left\"></i></a></nav>"
                url-prev)
      (format "<nav id=\"nav-left\"><a href=\"%s\" class=\"nav-left\"><i class=\"angle-left-inactive\"></i></a></nav>"
                ""))))

(defun org-html-nav-right (info data)
  "Return nav string for multipage Navigation.

INFO is a plist used as a communication channel.

DATA contains the subtree of the section/page to export
"
  (let ((url-next (plist-get
                   (plist-get info :tl-section-urls)
                   :section-url-next)))
    (if url-next
        (format "<nav id=\"nav-right\"><a href=\"%s\" class=\"nav-right\"><i class=\"angle-right\"></i></a></nav>"
                url-next)
      (format "<nav id=\"nav-right\"><a href=\"%s\" class=\"nav-right\"><i class=\"angle-right-inactive\"></i></a></nav>"
                ""))))

(defun org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
                                  ;; FIXME: Use Emacs 22 style here, see `coding-system-get'.
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    "")))))
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	     "</script><script src=\""
	     org-html-klipse-js
	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	     org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))

(defun org-html-inner-template (contents info &optional data)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.

DATA contains the subtree of the parse tree of the section to be
exported for multipage export.
"
  ;; Navigation
  (let ((tl-section-urls
         (alist-get
          (plist-get global-info :tl-headline-number)
          (plist-get global-info :section-url-lookup))))
    (format "<div id=\"page-main-body\">\n%s\n<div id=\"page-text-body\">%s</div>%s</div>"
            (org-html-nav-left
             (cl-list* :tl-section-urls tl-section-urls info) data)
            (concat
             ;; Document contents.
             contents
             ;; Footnotes section.
             (or (org-html-footnote-section info data) "")
             ;; Postamble.
             (org-html--build-pre/postamble 'postamble info))
            (org-html-nav-right
             (cl-list* :tl-section-urls tl-section-urls info) data))))

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
