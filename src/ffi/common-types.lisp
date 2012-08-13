(in-package :burning-ffi)

(defvar *ffi-types* ())

(defun %add-type (external-value internal-value)
  (if (not (equal (%get-type external-value) internal-value))
      (pushnew `(,external-value . ,internal-value) *ffi-types*)))

(defun %get-type (external-value)
  (rest (assoc external-value *ffi-types* :test #'equal)))

(defmacro deffitype (external-name internal-name)
  `(%add-type ,external-name ,internal-name))

(defgeneric get-type (name &rest args))

(defmethod get-type (name &rest args)
  (declare (ignore args))
  (%get-type name))

(cffi:defcfun "free_dynamic_string" :void (value :pointer))

(defun translate-dynamic-string (string)
  (free-dynamic-string (second string))
  (first string))

(cffi:defctype dynamic-string
    (:wrapper :string+ptr
	      :from-c translate-dynamic-string))

(deffitype :void :void)
(deffitype :bool :boolean)
(deffitype :int :int)
(deffitype :short :short)
(deffitype :long :long)
(deffitype :uint :uint)
(deffitype :ushort :ushort)
(deffitype :ulong :ulong)
(deffitype :float :float)
(deffitype :double :double)
(deffitype :string :string)
(deffitype :dynamic-string 'dynamic-string)