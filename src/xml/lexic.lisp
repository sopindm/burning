(in-package :burning-xml)

(deflexeme open-bracket "<")
(deflexeme id (:and (:or #\_ (:letter))
		    (:star (:or #\_ (:letter) (:digit)))))
(deflexeme closing-bracket ">")
(deflexeme tag-closing-bracket "/>")
(deflexeme close-tag-open-bracket "</")
(deflexeme spaces (:positive (:space)) :skipped t)
(deflexeme leq "=")
(deflexeme lstring (:and #\" (:star (:any)) #\") :minimal t)

(deflexeme linteger (:and (:maybe (:or #\- #\+)) (:positive (:digit))))

(deflexeme lfloat (:and (:maybe (:or #\- #\+))
		       (:or "nan"
			    (:and (:positive (:digit)) "." (:star (:digit))
				  (:maybe (:and (:or #\e  #\E) 
						(:maybe (:or #\- #\+))
						(:positive (:digit))))))))

(deflexic xml-lexic 
    open-bracket 
  id 
  closing-bracket 
  spaces 
  leq
  lstring 
  linteger 
  lfloat
  tag-closing-bracket 
  close-tag-open-bracket)

(defgrammar xml-grammar
    ((xml (open-tag xml-nodes closing-tag))
     (xml short-tag)
     (open-tag (open-bracket id attributes closing-bracket))
     (attributes eps)
     (attributes (id leq attribute-value attributes))
     (attribute-value linteger)
     (attribute-value lstring)
     (attribute-value lfloat)
     (closing-tag (close-tag-open-bracket id closing-bracket))
     (short-tag (open-bracket id attributes tag-closing-bracket))
     (xml-nodes eps)
     (xml-nodes (xml xml-nodes)))
  :start xml)

(defparameter xml-machine (create-state-machine xml-lexic))
(defparameter xml-parser (make-lr-parser (make-lr-table xml-grammar)))

