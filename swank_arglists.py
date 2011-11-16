### swank-arglists.lisp --- arglist related code ??
##
## Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
##          Tobias C. Rittweiler <tcr@freebits.de>
##          and others
##
## Transcribed into Python:
##          Samium Gromoff <_deepfire@feelingofgreen.ru> 
##
## License: Public Domain
##

import cl

from   cl       import *
from   cl       import _keyword as keyword, _intern0 as intern0

from   pergamum import *

# (in-package :swank)

# (eval-when (:compile-toplevel :load-toplevel :execute)
#   (swank-require :swank-c-p-c))

#### Utilities

from cl import _compose as compose

def length_equal(seq, n):
        """Test for whether SEQ contains N number of elements. I.e. it's equivalent
to (= (LENGTH SEQ) N), but besides being more concise, it may also be more
efficiently implemented."""
        # Was complicated by efficient support of cons-lists
        return len(seq) == n

def memq(item, list):
        return member(item, list, test = eq)

@block
def exactly_one_p(*values):
        """If exactly one value in VALUES is non-NIL, this value is returned.
Otherwise NIL is returned."""
        found = nil
        for v in values:
                if v:
                        if found:
                                return_from(exactly_one_p, nil)
                        else:
                                found = v
        return found

def valid_operator_symbol_p(symbol):
  "Is SYMBOL the name of a function, a macro, or a special-operator?"
  return (fboundp(symbol) or
          macro_function(symbol) or
          special_operator_p(symbol) or
          member(symbol, [])) # Was: (member symbol '(declare declaim)))

def function_exists_p(form):
        return (valid_function_name_p(form) and
                fboundp(form) and
                t)

### MULTIPLE-VALUE-OR: impossible                
# (defmacro multiple_value_or (&rest forms)
#   (if (null forms)
#       nil
#       (let ((first (first forms))
#             (rest (rest forms)))
#         `(let* ((values (multiple_value_list ,first))
#                 (primary_value (first values)))
#           (if primary_value
#               (values_list values)
#               (multiple_value_or ,@rest))))))
N_A = keyword("not-available")
def arglist_available_p(arglist):
        return arglist is not N_A


def with_available_arglist(form, body):
        return (N_A if form[0] is N_A else
                body(*form))

#### Arglist Definition

arglist = defstruct("arglist",
                    "provided_args",      # list of the provided actual arguments       
                    "required_args",      # list of the required arguments              
                    "optional_args",      # list of the optional arguments              
                    "key_p",              # whether &key appeared                       
                    "keyword_args",       # list of the keywords                        
                    "rest",               # name of the &rest or &body argument (if any)
                    "body_p",             # whether the rest argument is a &body        
                    "allow_other_keys_p", # whether &allow-other-keys appeared          
                    "aux_args",           # list of &aux variables                      
                    "any_p",              # whether &any appeared                       
                    "any_args",           # list of &any arguments  [*]                 
                    "known_junk",         # &whole, &environment                        
                    "unknown_junk")       # unparsed stuff

def arglist_p(x):
        return typep(x, arglist)

###
### [*] The &ANY lambda keyword is an extension to ANSI Common Lisp,
###     and is only used to describe certain arglists that cannot be
###     described in another way.
###
###     &ANY is very similiar to &KEY but while &KEY is based upon
###     the idea of a plist (key1 value1 key2 value2), &ANY is a
###     cross between &OPTIONAL, &KEY and *FEATURES* lists:
###
###        a) (&ANY :A :B :C) means that you can provide any (non-null)
###              set consisting of the keywords `:A', `:B', or `:C' in
###              the arglist. E.g. (:A) or (:C :B :A).
###
###        (This is not restricted to keywords only, but any self-evaluating
###         expression is allowed.)
###
###        b) (&ANY (key1 v1) (key2 v2) (key3 v3)) means that you can
###              provide any (non-null) set consisting of lists where
###              the CAR of the list is one of `key1', `key2', or `key3'.
###              E.g. ((key1 100) (key3 42)), or ((key3 66) (key2 23))
###
###
###     For example, a) let us describe the situations of EVAL-WHEN as
###
###       (EVAL-WHEN (&ANY :compile-toplevel :load-toplevel :execute) &BODY body)
###
###     and b) let us describe the optimization qualifiers that are valid
###     in the declaration specifier `OPTIMIZE':
###
###       (DECLARE (OPTIMIZE &ANY (compilation-speed 1) (safety 1) ...))
###

## This is a wrapper object around anything that came from Slime and
## could not reliably be read.
arglist_dummy = defstruct("arglist_dummy",
                          "string_representation")

def arglist_dummy_p(x):
        return typep(x, arglist_dummy)

def make_arglist_dummy(string_representation):
        return arglist_dummy(string_representation)

def empty_arg_p(dummy):
        return (arglist_dummy_p(dummy) and
                not len(dummy.string_representation))

_provided, _required, _optional, _rest, _key, _any  = mapcar(intern0, 
                                                             ["_provided",
                                                              "_required",
                                                              "_optional",
                                                              "_rest",
                                                              "_key",
                                                              "_any"])
lambda_list_keywords = [_provided, _required, _optional, _rest, _key, _any]

###################################################################################
# (defmacro do-decoded-arglist (decoded-arglist &body clauses)
#   (assert (loop for clause in clauses
# 		thereis (member (car clause) +lambda-list-keywords+)))
#   (flet ((parse-clauses (clauses)
# 	   (let* ((size    (length +lambda-list-keywords+))
# 		  (initial (make-hash-table :test (function eq) :size size))
# 		  (main    (make-hash-table :test (function eq) :size size))
# 		  (final   (make-hash-table :test (function eq) :size size)))
# 	     (loop for clause in clauses
# 		   for lambda-list-keyword = (first clause)
# 		   for clause-parameter    = (second clause)
# 		   doing (cond ((eq clause-parameter :initially)
# 				(setf (gethash lambda-list-keyword initial) clause))
# 			       ((eq clause-parameter :finally)
# 				(setf (gethash lambda-list-keyword final) clause))
# 			       (t
# 				(setf (gethash lambda-list-keyword main) clause)))
# 		   finally
# 		(return (values initial main final)))))
# 	 (generate-main-clause (clause arglist)
# 	   (destructure-case clause
#              ((&provided (&optional arg) . body)
#               (let ((gensym (gensym "PROVIDED-ARG+")))
# 		`(dolist (,gensym (arglist.provided-args ,arglist))
# 		   (declare (ignorable ,gensym))
# 		   (let (,@(when arg `((,arg ,gensym))))
# 		     ,@body))))
# 	     ((&required (&optional arg) . body)
# 	      (let ((gensym (gensym "REQUIRED-ARG+")))
# 		`(dolist (,gensym (arglist.required-args ,arglist))
# 		   (declare (ignorable ,gensym))
# 		   (let (,@(when arg `((,arg ,gensym))))
# 		     ,@body))))
# 	     ((&optional (&optional arg init) . body)
# 	      (let ((optarg (gensym "OPTIONAL-ARG+")))
# 		`(dolist (,optarg (arglist.optional-args ,arglist))
# 		   (declare (ignorable ,optarg))
# 		   (let (,@(when arg  `((,arg (optional-arg.arg-name ,optarg))))
# 			 ,@(when init `((,init (optional-arg.default-arg ,optarg)))))
# 		     ,@body))))
# 	     ((&key (&optional keyword arg init) . body)
# 	      (let ((keyarg (gensym "KEY-ARG+")))
# 		`(dolist (,keyarg (arglist.keyword-args ,arglist))
# 		   (declare (ignorable ,keyarg))
# 		   (let (,@(when keyword `((,keyword (keyword-arg.keyword ,keyarg))))
# 			 ,@(when arg     `((,arg (keyword-arg.arg-name ,keyarg))))
# 			 ,@(when init    `((,init (keyword-arg.default-arg ,keyarg)))))
# 		     ,@body))))
# 	     ((&rest (&optional arg body-p) . body)
# 	      `(when (arglist.rest ,arglist)
# 		 (let (,@(when arg    `((,arg (arglist.rest ,arglist))))
# 		       ,@(when body-p `((,body-p (arglist.body-p ,arglist)))))
# 		   ,@body)))
# 	     ((&any (&optional arg) . body)
#               (let ((gensym (gensym "REQUIRED-ARG+")))
#                 `(dolist (,gensym (arglist.any-args ,arglist))
#                     (declare (ignorable ,gensym))
#                     (let (,@(when arg `((,arg ,gensym))))
#                       ,@body)))))))
#     (let ((arglist (gensym "DECODED-ARGLIST+")))
#       (multiple-value-bind (initially-clauses main-clauses finally-clauses)
# 	  (parse-clauses clauses)
# 	`(let ((,arglist ,decoded-arglist))
# 	   (block do-decoded-arglist
# 	     ,@(loop for keyword in (&provided &required &optional &rest &key &any)
# 		     append (cddr (gethash keyword initially-clauses))
# 		     collect (let ((clause (gethash keyword main-clauses)))
# 			       (when clause (generate-main-clause clause arglist)))
# 		     append (cddr (gethash keyword finally-clauses)))))))))
###################################################################################

### Arglist Printing

# def print_decoded_arglist(arglist, operator = nil, provided_args = nil, highlight = nil):
#         def space():
#                 ## Kludge: When OPERATOR is not given, we don't want to
#                 ## print a space for the first argument.
#                 nonlocal operator
#                 if not operator:
#                         operator = t
#                 else:
#                         write_char(" ")
#                         pprint_newline(keyword("fill"))
#         def with_highlighting(body, index = nil):
#                 if index == car(highlight):
#                         princ("===> ")
#                         body()
#                         princ(" <===")
#                 else:
#                         body()
#         def print_arglist_recursively(argl, index = nil):
#                 if index == car(highlight):
#                         print_decoded_arglist(argl, keyword("highlight"), cdr(highlight))
#                         print_decoded_arglist(argl)
#         index = 0
#         def body(stream):
#                 if operator:
#                         print_arg(operator)
#                         pprint_indent(keyword("current"), 1)   # 1 due to possibly added space
#                 def do_decoded_arglist_body():
#                         pass
#                 do_decoded_arglist(do_decoded_arglist_body)
#         return print_unreadable_object(nil, nil,
#                                        body,
#                                        prefix = "(", suffix = ")")

# (defun print-decoded-arglist (arglist &key operator provided-args highlight)
#   (macrolet ((space ()
#                ## Kludge: When OPERATOR is not given, we don't want to
#                ## print a space for the first argument.
#                `(if (not operator)
#                     (setq operator t)
#                     (progn (write-char #\space
# )
#                            (pprint-newline :fill))))
#              (with-highlighting ((&key index) &body body)
#                `(if (eql ,index (car highlight))
#                     (progn (princ "===> ") ,@body (princ " <==="))
#                     (progn ,@body)))
#              (print-arglist-recursively (argl &key index)
#                `(if (eql ,index (car highlight))
#                     (print-decoded-arglist ,argl :highlight (cdr highlight))
#                     (print-decoded-arglist ,argl))))
#     (let ((index 0))
#       (pprint-logical-block (nil nil :prefix "(" :suffix ")")
#         (when operator
#           (print-arg operator)
#           (pprint-indent :current 1))   # 1 due to possibly added space
#         (do-decoded-arglist (remove-given-args arglist provided-args)
#           (&provided (arg)
#              (space)
#              (print-arg arg)
#              (incf index))
#           (&required (arg)
#              (space)
#              (if (arglist-p arg)
#                  (print-arglist-recursively arg :index index)
#                  (with-highlighting (:index index)
#                    (print-arg arg)))
#              (incf index))
#           (&optional :initially
#              (when (arglist.optional-args arglist)
#                (space)
#                (princ (quote &optional))))
#           (&optional (arg init-value)
#              (space)
#              (if (arglist-p arg)
#                  (print-arglist-recursively arg :index index)
#                  (with-highlighting (:index index)
#                    (if (null init-value)
#                        (print-arg arg)
#                        (format t "~:@<~A ~S~@:>" arg init-value))))
#              (incf index))
#           (&key :initially
#              (when (arglist.key-p arglist)
#                (space)
#                (princ (quote &key))))
#           (&key (keyword arg init)
#              (space)
#              (if (arglist-p arg)
#                  (pprint-logical-block (nil nil :prefix "(" :suffix ")")
#                    (prin1 keyword) (space)
#                    (print-arglist-recursively arg :index keyword))
#                  (with-highlighting (:index keyword)
#                    (cond ((and init (keywordp keyword))
#                           (format t "~:@<~A ~S~@:>" keyword init))
#                          (init
#                           (format t "~:@<(~S ..) ~S~@:>" keyword init))
#                          ((not (keywordp keyword))
#                           (format t "~:@<(~S ..)~@:>" keyword))
#                          (t
#                           (princ keyword))))))
#           (&key :finally
#              (when (arglist.allow-other-keys-p arglist)
#                (space)
#                (princ (quote &allow-other-keys))))
#           (&any :initially
#              (when (arglist.any-p arglist)
#                (space)
#                (princ (quote &any))))
#           (&any (arg)
#              (space)
#              (print-arg arg))
#           (&rest (args bodyp)
#              (space)
#              (princ (if bodyp (body &body) (body &rest)))
#              (space)
#              (if (arglist-p args)
#                  (print-arglist-recursively args :index index)
#                  (with-highlighting (:index index)
#                    (print-arg args))))
#           ## FIXME: add &UNKNOWN-JUNK?
#           )))))

def print_arg(arg):
        arg = (arg.string_representation if arglist_dummy_p(arg) else
               arg)
        return (prin1 if keywordp(arg) else
                princ)(arg)

# (defun print-decoded-arglist-as-template (decoded-arglist &key
#                                           (prefix "(") (suffix ")"))
#   (let ((first-p t))
#     (flet ((space ()
#              (unless first-p
#                (write-char #\space
# ))
#              (setq first-p nil))
#            (print-arg-or-pattern (arg)
#              (etypecase arg
#                (symbol        (if (keywordp arg) (prin1 arg) (princ arg)))
#                (string        (princ arg))
#                (list          (princ arg))
#                (arglist-dummy (princ (arglist-dummy.string-representation arg)))
#                (arglist       (print-decoded-arglist-as-template arg)))
#              (pprint-newline :fill)))
#       (pprint-logical-block (nil nil :prefix prefix :suffix suffix)
#         (do-decoded-arglist decoded-arglist
#           (&provided ())  # do nothing# provided args are in the buffer already.
#           (&required (arg)
#             (space) (print-arg-or-pattern arg))
#           (&optional (arg)
#             (space) (princ "[") (print-arg-or-pattern arg) (princ "]"))
#           (&key (keyword arg)
#             (space) 
#             (prin1 (if (keywordp keyword) keyword (list (quote quote) keyword)))
#             (space)
#             (print-arg-or-pattern arg)
#             (pprint-newline :linear))
#           (&any (arg)
#             (space) (print-arg-or-pattern arg))
#           (&rest (args)
#             (when (or (not (arglist.keyword-args decoded-arglist))
#                       (arglist.allow-other-keys-p decoded-arglist))
#               (space)
#               (format t "~A..." args))))))))

defvar("_arglist_pprint_bindings_",
       [("_print_case_",     keyword("downcase")),
        ("_print_pretty_",   t),
        ("_print_circle_",   nil),
        ("_print_readably_", nil),
        ("_print_level_",    10),
        ("_print_length_",   20),
        ("_print_escape_",   nil)])

defvar("_arglist_show_packages_", t)

def with_arglist_io_syntax(body):
        package = symbol_value("_package_")
        def wsios_body():
                with progv(_package_ = (symbol_value("_package_") if symbol_value("_arglist_show_packages_") else
                                        package)):
                        return with_bindings(symbol_value("_arglist_pprint_bindings_"),
                                             body)
        return with_standard_io_syntax(wsios_body)

def decoded_arglist_to_string(decoded_arglist, operator = nil, highlight = nil, print_right_margin = nil):
        def body(stream):
                def inner_body():
                        with progv(_print_right_margin_ = print_right_margin):
                                print_decoded_arglist(decoded_arglist,
                                                      operator = operator,
                                                      highlight = highlight)
                with progv(_standard_output_ = stream):
                        with_arglist_io_syntax(inner_body)
        return with_output_to_string(body)

def decoded_arglist_to_template_string(decoded_arglist, prefix = "(", suffix = ")"):
        def body(stream):
                def inner_body():
                        with progv(_print_right_margin_ = print_right_margin):
                                print_decoded_arglist(decoded_arglist,
                                                      operator = operator,
                                                      highlight = highlight)
                with progv(_standard_output_ = stream):
                        with_arglist_io_syntax(
                                lambda: print_decoded_arglist_as_template(
                                        decoded_arglist,
                                        prefix = prefix,
                                        suffix = suffix))
        return with_output_to_string(body)

#### Arglist Decoding / Encoding

def decode_required_arg(arg):
        "ARG can be a symbol or a destructuring pattern."
        return etypecase(arg,
                         (symbol,        arg),
                         (arglist_dummy, arg),
                         (list,          decode_arglist(arg)))

def encode_required_arg(arg):
        return etypecase(arg,
                         (symbol,  arg),
                         (arglist, encode_arglist(arg)))

keyword_arg = defstruct("keyword_arg",
                        "keyword",
                        "arg_name",
                        "default_arge")

def keyword_arg_p(x):
        return typep(x, keyword_arg)

def make_keyword_arg(keyword, arg_name, default_arg):
        return keyword_arg(keyword, arg_name, default_arg)

# (defun decode-keyword-arg (arg)
#   """Decode a keyword item of formal argument list.
# Return three values: keyword, argument name, default arg."""
#   (flet ((intern-as-keyword (arg)
#            (intern (etypecase arg
#                      (symbol (symbol-name arg))
#                      (arglist-dummy (arglist-dummy.string-representation arg)))
#                    keyword-package)))
#     (cond ((or (symbolp arg) (arglist-dummy-p arg))
#            (make-keyword-arg (intern-as-keyword arg) arg nil))
#           ((and (consp arg)
#                 (consp (car arg)))
#            (make-keyword-arg (caar arg)
#                              (decode-required-arg (cadar arg))
#                              (cadr arg)))
#           ((consp arg)
#            (make-keyword-arg (intern-as-keyword (car arg)) (car arg) (cadr arg)))
#           (t
#            (error "Bad keyword item of formal argument list")))))

# (defun encode-keyword-arg (arg)
#   (cond
#     ((arglist-p (keyword-arg.arg-name arg))
#      ## Destructuring pattern
#      (let ((keyword/name (list (keyword-arg.keyword arg)
#                                (encode-required-arg
#                                 (keyword-arg.arg-name arg)))))
#        (if (keyword-arg.default-arg arg)
#            (list keyword/name
#                  (keyword-arg.default-arg arg))
#            (list keyword/name))))
#     ((eql (intern (symbol-name (keyword-arg.arg-name arg))
#                   keyword-package)
#           (keyword-arg.keyword arg))
#      (if (keyword-arg.default-arg arg)
#          (list (keyword-arg.arg-name arg)
#                (keyword-arg.default-arg arg))
#          (keyword-arg.arg-name arg)))
#     (t
#      (let ((keyword/name (list (keyword-arg.keyword arg)
#                                (keyword-arg.arg-name arg))))
#        (if (keyword-arg.default-arg arg)
#            (list keyword/name
#                  (keyword-arg.default-arg arg))
#            (list keyword/name))))))

# (progn
#   (assert (equalp (decode-keyword-arg (quote x))
#                   (make-keyword-arg :x (quote x) nil)))
#   (assert (equalp (decode-keyword-arg (quote (x t)))
#                   (make-keyword-arg :x (quote x) t)))
#   (assert (equalp (decode-keyword-arg (quote ((:x y))))
#                   (make-keyword-arg :x (quote y) nil)))
#   (assert (equalp (decode-keyword-arg (quote ((:x y) t)))
#                   (make-keyword-arg :x (quote y) t))))

### FIXME suppliedp?
# (defstruct (optional-arg
#             (:conc-name optional-arg.)
#             (:constructor make-optional-arg (arg-name default-arg)))
#   arg-name
#   default-arg)

# (defun decode-optional-arg (arg)
#   """Decode an optional item of a formal argument list.
# Return an OPTIONAL-ARG structure."""
#   (etypecase arg
#     (symbol        (make-optional-arg arg nil))
#     (arglist-dummy (make-optional-arg arg nil))
#     (list          (make-optional-arg (decode-required-arg (car arg))
#                                       (cadr arg)))))

# (defun encode-optional-arg (optional-arg)
#   (if (or (optional-arg.default-arg optional-arg)
#           (arglist-p (optional-arg.arg-name optional-arg)))
#       (list (encode-required-arg
#              (optional-arg.arg-name optional-arg))
#             (optional-arg.default-arg optional-arg))
#       (optional-arg.arg-name optional-arg)))

# (progn
#   (assert (equalp (decode-optional-arg (quote x))
#                   (make-optional-arg (quote x) nil)))
#   (assert (equalp (decode-optional-arg (quote (x t)))
#                   (make-optional-arg (quote x) t))))

# (define-modify-macro nreversef () nreverse "Reverse the list in PLACE.")

# (defun decode-arglist (arglist)
#   "Parse the list ARGLIST and return an ARGLIST structure."
#   (etypecase arglist
#     ((eql :not-available) (return-from decode-arglist
#                             :not-available))
#     (list))
#   (loop
#     with mode = nil
#     with result = (make-arglist)
#     for arg = (if (consp arglist)
#                   (pop arglist)
#                   (progn
#                     (prog1 arglist
#                       (setf mode (quote &rest)
#                             arglist nil))))
#     do (cond
#          ((eql mode (quote &unknown-junk))
#           ## don't leave this mode -- we don't know how the arglist
#           ## after unknown lambda-list keywords is interpreted
#           (push arg (arglist.unknown-junk result)))
#          ((eql arg (quote &allow-other-keys))
#           (setf (arglist.allow-other-keys-p result) t))
#          ((eql arg (quote &key))
#           (setf (arglist.key-p result) t
#                 mode arg))
#          ((memq arg (&optional &rest &body &aux))
#           (setq mode arg))
#          ((memq arg (&whole &environment))
#           (setq mode arg)
#           (push arg (arglist.known-junk result)))
#          ((and (symbolp arg)
#                (string= (symbol-name arg) (string (quote #:&any
# )))) # may be interned
#           (setf (arglist.any-p result) t) #  in any *package*.
#           (setq mode (quote &any)))
#          ((memq arg lambda-list-keywords)
#           (setq mode (quote &unknown-junk))
#           (push arg (arglist.unknown-junk result)))
#          (t
#           (ecase mode
#             (&key
#                (push (decode-keyword-arg arg)
#                      (arglist.keyword-args result)))
#             (&optional
#                (push (decode-optional-arg arg)
#                      (arglist.optional-args result)))
#             (&body
#                (setf (arglist.body-p result) t
#                      (arglist.rest result) arg))
#             (&rest
#                (setf (arglist.rest result) arg))
#             (&aux
#                (push (decode-optional-arg arg)
#                      (arglist.aux-args result)))
#             ((nil)
#                (push (decode-required-arg arg)
#                      (arglist.required-args result)))
#             ((&whole &environment)
#                (setf mode nil)
#                (push arg (arglist.known-junk result)))
#             (&any
#                (push arg (arglist.any-args result))))))
#         until (null arglist)
#     finally (nreversef (arglist.required-args result))
#     finally (nreversef (arglist.optional-args result))
#     finally (nreversef (arglist.keyword-args result))
#     finally (nreversef (arglist.aux-args result))
#     finally (nreversef (arglist.any-args result))
#     finally (nreversef (arglist.known-junk result))
#     finally (nreversef (arglist.unknown-junk result))
#     finally (assert (or (and (not (arglist.key-p result)) 
#                              (not (arglist.any-p result)))
#                         (exactly-one-p (arglist.key-p result) 
#                                        (arglist.any-p result))))
#     finally (return result)))

# (defun encode-arglist (decoded-arglist)
#   (append (mapcar (function encode-required-arg) (arglist.required-args decoded-arglist))
#           (when (arglist.optional-args decoded-arglist)
#             (&optional))
#           (mapcar (function encode-optional-arg) (arglist.optional-args decoded-arglist))
#           (when (arglist.key-p decoded-arglist)
#             (&key))
#           (mapcar (function encode-keyword-arg) (arglist.keyword-args decoded-arglist))
#           (when (arglist.allow-other-keys-p decoded-arglist)
#             (&allow-other-keys))
#           (when (arglist.any-args decoded-arglist)
#             (list* (quote &any) (arglist.any-args decoded-arglist)))
#           (cond ((not (arglist.rest decoded-arglist))
#                  ())
#                 ((arglist.body-p decoded-arglist)
#                  `(&body ,(arglist.rest decoded-arglist)))
#                 (t
#                  `(&rest ,(arglist.rest decoded-arglist))))
#           (when (arglist.aux-args decoded-arglist)
#             `(&aux ,(arglist.aux-args decoded-arglist)))
#           (arglist.known-junk decoded-arglist)
#           (arglist.unknown-junk decoded-arglist)))

#### Arglist Enrichment

def arglist_keywords(lambda_list):
        """Return the list of keywords in ARGLIST.
As a secondary value, return whether &allow-other-keys appears."""
        decoded_arglist = decode_arglist(lambda_list)
        return (decoded_arglist.keyword_args,
                decoded_arglist.allow_other_keys_p)

# (defun methods-keywords (methods)
#   """Collect all keywords in the arglists of METHODS.
# As a secondary value, return whether &allow-other-keys appears somewhere."""
#   (let ((keywords ())
# 	(allow-other-keys nil))
#     (dolist (method methods)
#       (multiple-value-bind (kw aok)
# 	  (arglist-keywords
# 	   (swank-mop:method-lambda-list method))
# 	(setq keywords (remove-duplicates (append keywords kw)
#                                           :key (function keyword-arg.keyword))
# 	      allow-other-keys (or allow-other-keys aok))))
#     (values keywords allow-other-keys)))

# (defun generic-function-keywords (generic-function)
#   """Collect all keywords in the methods of GENERIC-FUNCTION.
# As a secondary value, return whether &allow-other-keys appears somewhere."""
#   (methods-keywords
#    (swank-mop:generic-function-methods generic-function)))

# (defun applicable-methods-keywords (generic-function arguments)
#   """Collect all keywords in the methods of GENERIC-FUNCTION that are
# applicable for argument of CLASSES.  As a secondary value, return
# whether &allow-other-keys appears somewhere."""
#   (methods-keywords
#    (multiple-value-bind (amuc okp)
#        (swank-mop:compute-applicable-methods-using-classes
#         generic-function (mapcar (function class-of) arguments))
#      (if okp
#          amuc
#          (compute-applicable-methods generic-function arguments)))))

# (defgeneric extra-keywords (operator &rest args)
#    (:documentation """Return a list of extra keywords of OPERATOR (a
# symbol) when applied to the (unevaluated) ARGS.
# As a secondary value, return whether other keys are allowed.
# As a tertiary value, return the initial sublist of ARGS that was needed
# to determine the extra keywords."""))

### We make sure that symbol-from-KEYWORD-using keywords come before
### symbol-from-arbitrary-package-using keywords. And we sort the
### latter according to how their home-packages relate to *PACKAGE*.
###
### Rationale is to show those key parameters first which make most
### sense in the current context. And in particular: to put
### implementation-internal stuff last.
###
### This matters tremendeously on Allegro in combination with
### AllegroCache as that does some evil tinkering with initargs,
### obfuscating the arglist of MAKE-INSTANCE.
###

# (defmethod extra-keywords :around (op &rest args)
#   (declare (ignorable op args))
#   (multiple-value-bind (keywords aok enrichments) (call-next-method)
#     (values (sort-extra-keywords keywords) aok enrichments)))

def make_package_comparator(reference_packages):
        """Returns a two-argument test function which compares packages
according to their used-by relation with REFERENCE-PACKAGES. Packages
will be sorted first which appear first in the PACKAGE-USE-LIST of the
reference packages."""
        package_use_table = make_hash_table()
        ## Walk the package dependency graph breadth-fist, and fill
        ## PACKAGE-USE-TABLE accordingly.
        queue = copy_list(reference_packages)
        bfn   = 0		# Breadth-First Number
        while queue:
                p = queue.pop()
                if not gethash(p, package_use_table)[1]:
                        package_use_table[p] = bfn
                        bfn += 1
                        queue.extend(copy_list(package_use_list(p)))
        def comparator(p1, p2):
                bfn1 = gethash(p1, package_use_table)
                bfn2 = gethash(p2, package_use_table)
                return ((bfn1 <= bfn2) if bfn1 and bfn2 else
                        bfn1           if bfn1          else
                        nil            if bfn2          else	# p2 is used, p1 not
                        string_less_or_equal(package_name(p1), package_name(p2)))
        return comparator

def sort_extra_keywords(kwds):
        return stable_sort(kwds, make_package_comparator([keyword_package,
                                                          symbol_value("_package_")]),
                           key = compose(symbol_package, slotting("keyword")))

def keywords_of_operator(operator):
        """Return a list of KEYWORD-ARGs that OPERATOR accepts.
This function is useful for writing EXTRA-KEYWORDS methods for
user-defined functions which are declared &ALLOW-OTHER-KEYS and which
forward keywords to OPERATOR."""
        return with_available_arglist(arglist_from_form(ensure_list(operator)),
                                      lambda arglist: values(arglist.keyword_args,
                                                             arglist.allow_other_keys_p))

# (defmethod extra-keywords (operator &rest args)
#   ## default method
#   (declare (ignore args))
#   (let ((symbol-function (symbol-function operator)))
#     (if (typep symbol-function (quote generic-function))
#         (generic-function-keywords symbol-function)
#         nil)))

# (defun class-from-class-name-form (class-name-form)
#   (when (and (listp class-name-form)
#              (= (length class-name-form) 2)
#              (eq (car class-name-form) (quote quote)))
#     (let* ((class-name (cadr class-name-form))
#            (class (find-class class-name nil)))
#       (when (and class
#                  (not (swank-mop:class-finalized-p class)))
#         ## Try to finalize the class, which can fail if
#         ## superclasses are not defined yet
#         (handler-case (swank-mop:finalize-inheritance class)
#           (program-error (c)
#             (declare (ignore c)))))
#       class)))

# (defun extra-keywords/slots (class)
#   (multiple-value-bind (slots allow-other-keys-p)
#       (if (swank-mop:class-finalized-p class)
#           (values (swank-mop:class-slots class) nil)
#           (values (swank-mop:class-direct-slots class) t))
#     (let ((slot-init-keywords
#            (loop for slot in slots append
#                  (mapcar (lambda (initarg)
#                            (make-keyword-arg
#                             initarg
#                             (swank-mop:slot-definition-name slot)
#                             (swank-mop:slot-definition-initform slot)))
#                          (swank-mop:slot-definition-initargs slot)))))
#       (values slot-init-keywords allow-other-keys-p))))

# (defun extra-keywords/make-instance (operator &rest args)
#   (declare (ignore operator))
#   (unless (null args)
#     (let* ((class-name-form (car args))
#            (class (class-from-class-name-form class-name-form)))
#       (when class
#         (multiple-value-bind (slot-init-keywords class-aokp)
#             (extra-keywords/slots class)
#           (multiple-value-bind (allocate-instance-keywords ai-aokp)
#               (applicable-methods-keywords
#                (function allocate-instance) (list class))
#             (multiple-value-bind (initialize-instance-keywords ii-aokp)
#                 (ignore-errors
#                   (applicable-methods-keywords
#                    (function initialize-instance) (list (swank-mop:class-prototype class))))
#               (multiple-value-bind (shared-initialize-keywords si-aokp)
#                   (ignore-errors
#                     (applicable-methods-keywords
#                      (function shared-initialize) (list (swank-mop:class-prototype class) t)))
#                 (values (append slot-init-keywords
#                                 allocate-instance-keywords
#                                 initialize-instance-keywords
#                                 shared-initialize-keywords)
#                         (or class-aokp ai-aokp ii-aokp si-aokp)
#                         (list class-name-form))))))))))

# (defun extra-keywords/change-class (operator &rest args)
#   (declare (ignore operator))
#   (unless (null args)
#     (let* ((class-name-form (car args))
#            (class (class-from-class-name-form class-name-form)))
#       (when class
#         (multiple-value-bind (slot-init-keywords class-aokp)
#             (extra-keywords/slots class)
#           (declare (ignore class-aokp))
#           (multiple-value-bind (shared-initialize-keywords si-aokp)
#               (ignore-errors
#                 (applicable-methods-keywords
#                  (function shared-initialize) (list (swank-mop:class-prototype class) t)))
#             ## FIXME: much as it would be nice to include the
#             ## applicable keywords from
#             ## UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, I don't really see
#             ## how to do it: so we punt, always declaring
#             ## &ALLOW-OTHER-KEYS.
#             (declare (ignore si-aokp))
#             (values (append slot-init-keywords shared-initialize-keywords)
#                     t
#                     (list class-name-form))))))))

# (defmethod extra-keywords ((operator (eql (quote make-instance)))
#                            &rest args)
#   (multiple-value-or (apply (function extra-keywords/make-instance) operator args)
#                      (call-next-method)))

# (defmethod extra-keywords ((operator (eql (quote make-condition)))
#                            &rest args)
#   (multiple-value-or (apply (function extra-keywords/make-instance) operator args)
#                      (call-next-method)))

# (defmethod extra-keywords ((operator (eql (quote error)))
#                            &rest args)
#   (multiple-value-or (apply (function extra-keywords/make-instance) operator args)
#                      (call-next-method)))

# (defmethod extra-keywords ((operator (eql (quote signal)))
#                            &rest args)
#   (multiple-value-or (apply (function extra-keywords/make-instance) operator args)
#                      (call-next-method)))

# (defmethod extra-keywords ((operator (eql (quote warn)))
#                            &rest args)
#   (multiple-value-or (apply (function extra-keywords/make-instance) operator args)
#                      (call-next-method)))

# (defmethod extra-keywords ((operator (eql (quote cerror)))
#                            &rest args)
#   (multiple-value-bind (keywords aok determiners)
#       (apply (function extra-keywords/make-instance) operator
#              (cdr args))
#     (if keywords
#         (values keywords aok
#                 (cons (car args) determiners))
#         (call-next-method))))

# (defmethod extra-keywords ((operator (eql (quote change-class)))
#                            &rest args)
#   (multiple-value-bind (keywords aok determiners)
#       (apply (function extra-keywords/change-class) operator (cdr args))
#     (if keywords
#         (values keywords aok
#                 (cons (car args) determiners))
#         (call-next-method))))

# (defun enrich-decoded-arglist-with-keywords (decoded-arglist keywords allow-other-keys-p)
#   "Modify DECODED-ARGLIST using KEYWORDS and ALLOW-OTHER-KEYS-P."
#   (when keywords
#     (setf (arglist.key-p decoded-arglist) t)
#     (setf (arglist.keyword-args decoded-arglist)
#           (remove-duplicates
#            (append (arglist.keyword-args decoded-arglist)
#                    keywords)
#            :key (function keyword-arg.keyword))))
#   (setf (arglist.allow-other-keys-p decoded-arglist)
#         (or (arglist.allow-other-keys-p decoded-arglist)
#             allow-other-keys-p)))

# (defun enrich-decoded-arglist-with-extra-keywords (decoded-arglist form)
#   """Determine extra keywords from the function call FORM, and modify
# DECODED-ARGLIST to include them.  As a secondary return value, return
# the initial sublist of ARGS that was needed to determine the extra
# keywords.  As a tertiary return value, return whether any enrichment
# was done."""
#   (multiple-value-bind (extra-keywords extra-aok determining-args)
#       (apply (function extra-keywords) form)
#     ## enrich the list of keywords with the extra keywords
#     (enrich-decoded-arglist-with-keywords decoded-arglist
#                                           extra-keywords extra-aok)
#     (values decoded-arglist
#             determining-args
#             (or extra-keywords extra-aok))))

# (defgeneric compute-enriched-decoded-arglist (operator-form argument-forms)
#   (:documentation
#    """Return three values: DECODED-ARGLIST, DETERMINING-ARGS, and
# ANY-ENRICHMENT, just like enrich-decoded-arglist-with-extra-keywords.
# If the arglist is not available, return :NOT-AVAILABLE."""))

# (defmethod compute-enriched-decoded-arglist (operator-form argument-forms)
#   (with-available-arglist (decoded-arglist) 
#       (decode-arglist (arglist operator-form))
#     (enrich-decoded-arglist-with-extra-keywords decoded-arglist
#                                                 (cons operator-form
#                                                       argument-forms))))

# (defmethod compute-enriched-decoded-arglist ((operator-form (eql (quote with-open-file)))
#                                              argument-forms)
#   (declare (ignore argument-forms))
#   (multiple-value-bind (decoded-arglist determining-args)
#       (call-next-method)
#     (let ((first-arg (first (arglist.required-args decoded-arglist)))
#           (open-arglist (compute-enriched-decoded-arglist (quote open) nil)))
#       (when (and (arglist-p first-arg) (arglist-p open-arglist))
#         (enrich-decoded-arglist-with-keywords
#          first-arg
#          (arglist.keyword-args open-arglist)
#          nil)))
#     (values decoded-arglist determining-args t)))

# (defmethod compute-enriched-decoded-arglist ((operator-form (eql (quote apply)))
#                                              argument-forms)
#   (let ((function-name-form (car argument-forms)))
#     (when (and (listp function-name-form)
#                (length= function-name-form 2)
#                (memq (car function-name-form) (quote (quote function))))
#       (let ((function-name (cadr function-name-form)))
#         (when (valid-operator-symbol-p function-name)
#           (let ((function-arglist
#                  (compute-enriched-decoded-arglist function-name
#                                                    (cdr argument-forms))))
#             (return-from compute-enriched-decoded-arglist
#               (values (make-arglist :required-args
#                                     (list (quote function))
#                                     :optional-args
#                                     (append
#                                      (mapcar (lambda (arg)
#                                               (make-optional-arg arg nil))
#                                              (arglist.required-args function-arglist))
#                                      (arglist.optional-args function-arglist))
#                                     :key-p
#                                     (arglist.key-p function-arglist)
#                                     :keyword-args
#                                     (arglist.keyword-args function-arglist)
#                                     :rest
#                                     (quote args)
#                                     :allow-other-keys-p
#                                     (arglist.allow-other-keys-p function-arglist))
#                       (list function-name-form)
#                       t)))))))
#   (call-next-method))

# (defun delete-given-args (decoded-arglist args)
#   "Delete given ARGS from DECODED-ARGLIST."
#   (macrolet ((pop-or-return (list)
# 	# `(if (null ,list)
# 	#	    (return-from do-decoded-arglist)
# 	#	    (pop ,list))
#               ))
#     (do-decoded-arglist decoded-arglist
#       (&provided ()
#        (assert (eq (pop-or-return args)
#                    (pop (arglist.provided-args decoded-arglist)))))
#       (&required ()
#        (pop-or-return args)
#        (pop (arglist.required-args decoded-arglist)))
#       (&optional ()
#        (pop-or-return args)
#        (pop (arglist.optional-args decoded-arglist)))
#       (&key (keyword)
#        ## N.b. we consider a keyword to be given only when the keyword
#        ## _and_ a value has been given for it.
#        (loop for (key value) on args by (function cddr)
# 	     when (and (eq keyword key) value)
# 	       do (setf (arglist.keyword-args decoded-arglist)
# 			(remove keyword (arglist.keyword-args decoded-arglist)
# 				:key (function keyword-arg.keyword))))))))
#   decoded-arglist)

# (defun remove-given-args (decoded-arglist args)
#   ## FIXME: We actually needa deep copy here.
#   (delete-given-args (copy-arglist decoded-arglist) args))

#### Arglist Retrieval

def arglist_from_form(form):
        return (N_A if not form else
                arglist_dispatch(form[0], form[1:]))

def arglist_dispatch(form, tail):
        not_implemented()

# (export (quote arglist-dispatch))
# (defgeneric arglist-dispatch (operator arguments)
#   ## Default method
#   (:method (operator arguments)
#     (unless (and (symbolp operator) (valid-operator-symbol-p operator))
#       (return-from arglist-dispatch :not-available))

#     (multiple-value-bind (decoded-arglist determining-args)
#         (compute-enriched-decoded-arglist operator arguments)
#       (with-available-arglist (arglist) decoded-arglist
#         ## replace some formal args by determining actual args
#         (setf arglist (delete-given-args arglist determining-args))
#         (setf (arglist.provided-args arglist) determining-args)
#         arglist))))

# (defmethod arglist-dispatch ((operator (eql (quote defmethod))) arguments)
#   (match (cons operator arguments)
#     (((quote defmethod) ((function function-exists-p) gf-name) . rest)
#      (let ((gf (fdefinition gf-name)))
#        (when (typep gf (quote generic-function))
#          (with-available-arglist (arglist) (decode-arglist (arglist gf))
#            (let ((qualifiers (loop for x in rest
#                                    until (or (listp x) (empty-arg-p x))
#                                    collect x)))
#              (return-from arglist-dispatch
#                (make-arglist :provided-args (cons gf-name qualifiers)
#                              :required-args (list arglist)
#                              :rest "body" :body-p t)))))))
#     (_)) # Fall through
#   (call-next-method))

# (defmethod arglist-dispatch ((operator (eql (quote define-compiler-macro))) arguments)
#   (match (cons operator arguments)
#     (((quote define-compiler-macro) ((function function-exists-p) gf-name) . _)
#      (let ((gf (fdefinition gf-name)))
#        (with-available-arglist (arglist) (decode-arglist (arglist gf))
#          (return-from arglist-dispatch
#            (make-arglist :provided-args (list gf-name)
#                          :required-args (list arglist)
#                          :rest "body" :body-p t)))))
#     (_)) # Fall through
#   (call-next-method))


# (defmethod arglist-dispatch ((operator (eql (quote eval-when))) arguments)
#   (declare (ignore arguments))
#     (let ((eval-when-args (quote (:compile-toplevel :load-toplevel :execute))))
#     (make-arglist 
#      :required-args (list (make-arglist :any-p t :any-args eval-when-args))
#      :rest (quote #:body
#            ) :body-p t)))


# (defmethod arglist-dispatch ((operator (eql (quote declare))) arguments)
#   (let* ((declaration      (cons operator (last arguments)))
#          (typedecl-arglist (arglist-for-type-declaration declaration)))
#     (if (arglist-available-p typedecl-arglist)
#         typedecl-arglist
#         (match declaration
#           (((quote declare) (((function consp) typespec) . decl-args)) 
#            (with-available-arglist (typespec-arglist) 
#                (decoded-arglist-for-type-specifier typespec)
#              (make-arglist
#               :required-args (list (make-arglist
#                                     :required-args (list typespec-arglist)
#                                     :rest (quote #:variables
# ))))))
#           (((quote declare) (decl-identifier . decl-args))
#            (decoded-arglist-for-declaration decl-identifier decl-args))
#           (_ (make-arglist :rest (quote #:declaration-specifiers
#                                   )))))))

# (defmethod arglist-dispatch ((operator (eql (quote declaim))) arguments)
#   (arglist-dispatch (quote declare) arguments))


# (defun arglist-for-type-declaration (declaration)
#   (flet ((%arglist-for-type-declaration (identifier typespec rest-var-name)
#            (with-available-arglist (typespec-arglist) 
#                (decoded-arglist-for-type-specifier typespec)
#              (make-arglist 
#               :required-args (list (make-arglist 
#                                     :provided-args (list identifier)
#                                     :required-args (list typespec-arglist)
#                                     :rest rest-var-name))))))
#     (match declaration
#       (((quote declare) ((quote type) ((function consp) typespec) . decl-args)) 
#        (%arglist-for-type-declaration (quote type) typespec (quote #:variables
#                                                              )))
#       (((quote declare) ((quote ftype) ((function consp) typespec) . decl-args)) 
#        (%arglist-for-type-declaration (quote ftype) typespec (quote 
#                                                               #:function-names
#                                                               )))
#       (((quote declare) (((function consp) typespec) . decl-args)) 
#        (with-available-arglist (typespec-arglist) 
#            (decoded-arglist-for-type-specifier typespec)
#          (make-arglist
#           :required-args (list (make-arglist
#                                 :required-args (list typespec-arglist)
#                                 :rest (quote #:variables
#                                        ))))))
#       (_ :not-available))))

# (defun decoded-arglist-for-declaration (decl-identifier decl-args)
#   (declare (ignore decl-args))
#     (with-available-arglist (arglist)
#       (decode-arglist (declaration-arglist decl-identifier))
#     (setf (arglist.provided-args arglist) (list decl-identifier))
#     (make-arglist :required-args (list arglist))))

# (defun decoded-arglist-for-type-specifier (type-specifier)
#   (etypecase type-specifier
#     (arglist-dummy :not-available)
#     (cons (decoded-arglist-for-type-specifier (car type-specifier)))
#     (symbol
#      (with-available-arglist (arglist)
#          (decode-arglist (type-specifier-arglist type-specifier))
#        (setf (arglist.provided-args arglist) (list type-specifier))
#        arglist))))

### Slimefuns

### We work on a RAW-FORM, or BUFFER-FORM, which represent the form at
### user's point in Emacs. A RAW-FORM looks like
###
###       ("FOO" ("BAR" ...) "QUUX" ("ZURP" SWANK::%CURSOR-MARKER%))
###
### The expression before the cursor marker is the expression where
### user's cursor points at. An explicit marker is necessary to
### disambiguate between
###
###       ("IF" ("PRED")
###             ("F" "X" "Y" %CURSOR-MARKER%))
###
### and
###       ("IF" ("PRED")
###             ("F" "X" "Y") %CURSOR-MARKER%)

### Notice that for a form like (FOO (BAR |) QUUX), where | denotes
### user's point, the following should be sent ("FOO" ("BAR" ""
### %CURSOR-MARKER%)). Only the forms up to point should be
### considered.

@block
def autodoc(raw_form, print_right_margin = nil):
        """Return a string representing the arglist for the deepest subform in
RAW-FORM that does have an arglist. The highlighted parameter is
wrapped in ===> X <===."""
        def body():
                form, arglist, obj_at_cursor, form_path = find_subform_with_arglist(
                        parse_raw_form(raw_form))
                return (print_variable_to_string(obj_at_cursor)
                        if boundp_and_interesting(obj_at_cursor) else
                        with_available_arglist(
                                arglist,
                                lambda arglist:
                                        decoded_arglist_to_string(
                                        arglist,
                                        print_right_margin = print_right_margin,
                                        operator = form[0],
                                        highlight = form_path_to_arglist_path(form_path,
                                                                              form,
                                                                              arglist))))
        def handler(c):
                if not debug_on_swank_error():
                        with progv(_pring_right_margin_ = print_right_margin):
                                return_from(autodoc,
                                            format(nil, "Arglist Error: \"%s\"", c))
        handler_bind(lambda: with_buffer_syntax(body),
                     (error_,
                      handler))

def boundp_and_interesting(symbol):
        return (symbol and
                symbolp(symbol) and
                boundp(symbol) and
                symbol not in set([t, nil]) and
                not keywordp(symbol))

def print_variable_to_string(symbol):
        "Return a short description of VARIABLE-NAME, or NIL."
        with progv(_print_pretty_ = t,
                   _print_level_ = 4,
                   _print_length_ = 10,
                   _print_lines_ = 1,
                   _print_readably_ = nil):
                value = symbol_value(symbol)
                return call__truncated_output_to_string(
                        75, lambda s: without_printing_errors(
                                lambda: format(s, "%s %s%s",
                                               symbol, symbol_value("_echo_area_prefix_"),
                                               value),
                                object = value,
                                stream = s))

def complete_form(raw_form):
        """Read FORM-STRING in the current buffer package, then complete it
by adding a template for the missing arguments."""
        ## We do not catch errors here because COMPLETE-FORM is an
        ## interactive command, not automatically run in the background like
        ## ARGLIST-FOR-ECHO-AREA.
        def body():
                arglist, provided_args = find_immediately_containing_arglist(parse_raw_form, raw_form)
                return with_available_arglist(
                        arglist,
                        lambda arglist:
                                decoded_arglist_to_template_string(
                                delete_given_args(arglist,
                                                  remove_if(empty_arg_p, provided_args,
                                                            from_end = t, count = 1)),
                                prefix = "", suffix = ""))
        return with_buffer_syntax(body)

def completions_for_keyword(keyword_string, raw_form):
        """Return a list of possible completions for KEYWORD-STRING relative
to the context provided by RAW-FORM."""
        arglist = find_immediately_containing_arglist(parse_raw_form(raw_form))
        if arglist_available_p(arglist):
                ## It would be possible to complete keywords only if we are in
                ## a keyword position, but it is not clear if we want that.
                keywords = (mapcar(slotting("keyword"), arglist.keyword_args) +
                            remove_if_not(keywordp, arglist.any_args))
                keyword_name = tokenize_symbol(keyword_string)
                matching_keywords = find_matching_symbols_in_list(
                        keyword_name, keywords, make_compound_prefix_matcher("-"))
                converter = completion_output_symbol_converter(keyword_string)
                strings = mapcar(converter, mapcar(symbol_name, matching_keywords))
                completion_set = format_completion_set(strings, nil, "")
                return [completion_set,
                        longest_compound_prefix(completion_set)]

defparameter("_cursor_marker_", intern0("%cursor-marker%"))
_cursor_marker_ = symbol_value("_cursor_marker_")

def find_subform_with_arglist(form):
        """Returns four values:

     The appropriate subform of `form' which is closest to the
     +CURSOR-MARKER+ and whose operator is valid and has an
     arglist. The +CURSOR-MARKER+ is removed from that subform.

     Second value is the arglist. Local function and macro definitions
     appearing in `form' into account.

     Third value is the object in front of +CURSOR-MARKER+.

     Fourth value is a form path to that object."""
        def yield_success(form, local_ops):
                form, obj_at_cursor, form_path = extract_cursor_marker(form)
                return values(form,
                              if_let(assoc(car(form), local_ops, test = op_equal),
                                     lambda entry: decode_arglist(cdr(entry)),
                                     lambda: arglist_from_form(form)),
                              obj_at_cursor,
                              form_path)
        def yield_failure():
                return values(nil, N_A)
        def operator_p(operator, local_ops):
                return ((symbolp(operator) and
                         valid_operator_symbol_p(operator)) or
                        assoc(operator, local_ops, test = op_equal))
        def op_equal(op1, op2):
                return cond((lambda: symbolp(op1) and symbolp(op2),
                             lambda: op1 is op2),
                            (lambda: arglist_dummy_p(op1) and arglist_dummy_p(op2),
                             lambda: string_equal(op1.string_representation,
                                                  op2.string_representation)))
        def grovel_form(form, local_ops):
                """Descend FORM top-down, always taking the rightest branch,
                until +CURSOR-MARKER+."""
                last_subform = form[-1]
                new_ops = []
                def set_new_ops(x): nonlocal new_ops; new_ops = x; return x
                return destructuring_bind(
                        the(list, form),
                        lambda operator, *args:
                                ## N.b. the user's cursor is at the rightmost, deepest
                                ## subform right before +CURSOR-MARKER+.
                        cond((lambda: last_subform is _cursor_marker_,
                              lambda: (yield_success(form, local_ops)
                                       if operator_p(operator, local_ops) else
                                       yield_failure())),
                             (lambda: not operator_p(operator, local_ops),
                              lambda: grovel_form(last_subform, local_ops)),
                             ## Make sure to pick up the arglists of local
                             ## function/macro definitions.
                             (lambda: set_new_ops(extract_local_op_arglists(operator, args)),
                              lambda: (grovel_form(last_subform, new_ops + local_ops) or
                                       yield_success(form, local_ops))),
                             ## Some typespecs clash with function names, so we make
                             ## sure to bail out early.
                             (lambda: operator in set([intern0("declare"),
                                                       intern0("declaim")]),
                              lambda: yield_success(form, local_ops)),
                             ## Mostly uninteresting, hence skip.
                             (lambda: operator in set([intern0("quote"),
                                                       intern0("function")]),
                              lambda: yield_failure()),
                             (t,
                              lambda: (grovel_form(last_subform, local_ops) or
                                       yield_success(form, local_ops)))))
        return (yield_failure() if not form else
                grovel_form(form, []))

# (defun extract-cursor-marker (form)
#   """Returns three values: normalized `form' without +CURSOR-MARKER+,
# the object in front of +CURSOR-MARKER+, and a form path to that
# object."""
#   (labels ((grovel (form last path)
#              (let ((result-form))
#                (loop for (car . cdr) on form do
#                      (cond ((eql car +cursor-marker+)
#                             (decf (first path))
#                             (return-from grovel
#                               (values (nreconc result-form cdr)
#                                       last
#                                       (nreverse path))))
#                            ((consp car)
#                             (multiple-value-bind (new-car new-last new-path)
#                                 (grovel car last (cons 0 path))
#                               (when new-path # CAR contained cursor-marker?
#                                 (return-from grovel
#                                   (values (nreconc
#                                            (cons new-car result-form) cdr)
#                                           new-last
#                                           new-path))))))
#                      (push car result-form)
#                      (setq last car)
#                      (incf (first path))
#                      finally
#                        (return-from grovel
#                          (values (nreverse result-form) nil nil))))))
#     (grovel form nil (list 0))))

# (defgeneric extract-local-op-arglists (operator args)
#   (:documentation
#    """If the form `(OPERATOR ,@ARGS) is a local operator binding form,
#      return a list of pairs (OP . ARGLIST) for each locally bound op.""")
#   (:method (operator args)
#     (declare (ignore operator args))
#     nil)
#   ## FLET
#   (:method ((operator (eql (quote cl:flet))) args)
#     (let ((defs (first args))
#           (body (rest args)))
#       (cond ((null body) nil)            # `(flet ((foo (x) |'
#             ((atom defs) nil)            # `(flet ,foo (|'
#             (t (%collect-op/argl-alist defs)))))
#   ## LABELS
#   (:method ((operator (eql (quote cl:labels))) args)
#     ## Notice that we only have information to "look backward" and
#     ## show arglists of previously occuring local functions.
#     (destructuring-bind (defs . body) args
#       (unless (or (atom defs) (null body))   # `(labels ,foo (|'
#         (let ((current-def (car (last defs))))
#           (cond ((atom current-def) nil) # `(labels ((foo (x) ...)|'
#                 ((not (null body))
#                  (extract-local-op-arglists (quote cl:flet) args))
#                 (t
#                  (let ((def.body (cddr current-def)))
#                    (when def.body
#                      (%collect-op/argl-alist defs)))))))))
#   ## MACROLET
#   (:method ((operator (eql (quote cl:macrolet))) args)
#     (extract-local-op-arglists (quote cl:labels) args)))

# (defun %collect-op/argl-alist (defs)
#   (setq defs (remove-if-not (lambda (x)
#                               ## Well-formed FLET/LABELS def?
#                               (and (consp x) (second x)))
#                             defs))
#   (loop for (name arglist . nil) in defs
#         collect (cons name arglist)))

# (defun find-immediately-containing-arglist (form)
#   """Returns the arglist of the subform _immediately_ containing
# +CURSOR-MARKER+ in `form'. Notice, however, that +CURSOR-MARKER+ may
# be in a nested arglist \(e.g. `(WITH-OPEN-FILE (<here>'\), and the
# arglist of the appropriate parent form \(WITH-OPEN-FILE\) will be
# returned in that case."""
#   (flet ((try (form-path form arglist)
#            (let* ((arglist-path (form-path-to-arglist-path form-path
#                                                            form
#                                                            arglist))
#                   (argl (apply (function arglist-ref)
#                                arglist
#                                arglist-path))
#                   (args (apply (function provided-arguments-ref)
#                                (cdr form)
#                                arglist
#                                arglist-path)))
#              (when (and (arglist-p argl) (listp args))
#                (values argl args)))))
#     (multiple-value-bind (form arglist obj form-path)
#         (find-subform-with-arglist form)
#       (declare (ignore obj))
#       (with-available-arglist (arglist) arglist
#         ## First try the form the cursor is in (in case of a normal
#         ## form), then try the surrounding form (in case of a nested
#         ## macro form).
#         (multiple-value-or (try form-path form arglist)
#                            (try (butlast form-path) form arglist)
#                            :not-available)))))

# (defun form-path-to-arglist-path (form-path form arglist)
#   """Convert a form path to an arglist path consisting of arglist
# indices."""
#   (labels ((convert (path args arglist)
#              (if (null path)
#                  nil
#                  (let* ((idx      (car path))
#                         (idx*     (arglist-index idx args arglist))
#                         (arglist* (and idx* (arglist-ref arglist idx*)))
#                         (args*    (and idx* (provided-arguments-ref args
#                                                                     arglist
#                                                                     idx*))))
#                    ## The FORM-PATH may be more detailed than ARGLIST#
#                    ## consider (defun foo (x y) ...), a form path may
#                    ## point into the function's lambda-list, but the
#                    ## arglist of DEFUN won't contain as much information.
#                    ## So we only recurse if possible.
#                    (cond ((null idx*)
#                           nil)
#                          ((arglist-p arglist*)
#                           (cons idx* (convert (cdr path) args* arglist*)))
#                          (t
#                           (list idx*)))))))
#     (convert
#      ## FORM contains irrelevant operator. Adjust FORM-PATH.
#      (cond ((null form-path) nil)
#            ((equal form-path (quote (0))) nil)
#            (t
#             (destructuring-bind (car . cdr) form-path
#               (cons (1- car) cdr))))
#      (cdr form)
#      arglist)))

# (defun arglist-index (provided-argument-index provided-arguments arglist)
#   """Return the arglist index into `arglist' for the parameter belonging
# to the argument (NTH `provided-argument-index' `provided-arguments')."""
#   (let ((positional-args#
#          (positional-args-number arglist))
#         (arg-index provided-argument-index))
#     (with-struct (arglist. key-p rest) arglist
#       (cond
#         ((< arg-index positional-args#
#             ) # required + optional
#          arg-index)
#         ((and (not key-p) (not rest))   # more provided than allowed
#          nil)
#         ((not key-p)                    # rest + body
#          (assert (arglist.rest arglist))
#          positional-args#
#          ) 
#         (t                              # key
#          ## Find last provided &key parameter
#          (let* ((argument      (nth arg-index provided-arguments))
#                 (provided-keys (subseq provided-arguments positional-args#
#                                 )))
#            (loop for (key value) on provided-keys by (function cddr)
#                  when (eq value argument)
#                  return (match key
#                             (((quote quote) symbol) symbol)
#                             (_ key)))))))))

# (defun arglist-ref (arglist &rest indices)
#   """Returns the parameter in ARGLIST along the INDICIES path. Numbers
# represent positional parameters (required, optional), keywords
# represent key parameters."""
#   (flet ((ref-positional-arg (arglist index)
#            (check-type index (integer 0 *))
#            (with-struct (arglist. provided-args required-args optional-args rest) 
#                arglist
#              (loop for args in (list provided-args required-args 
#                                      (mapcar (function optional-arg.arg-name) optional-args))
#                    for args#
#               = (length args)
#                    if (< index args#
#                          )
#                      return (nth index args)
#                    else
#                      do (decf index args#
#                          )
#                    finally (return (or rest nil)))))
#          (ref-keyword-arg (arglist keyword)
#            ## keyword argument may be any symbol,
#            ## not only from the KEYWORD package.
#            (let ((keyword (match keyword
#                             (((quote quote) symbol) symbol)
#                             (_ keyword))))
#              (do-decoded-arglist arglist
#                (&key (kw arg) (when (eq kw keyword)
#                                 (return-from ref-keyword-arg arg)))))
#            nil))
#     (dolist (index indices)
#       (assert (arglist-p arglist))
#       (setq arglist (if (numberp index)
#                         (ref-positional-arg arglist index)
#                         (ref-keyword-arg arglist index))))
#     arglist))

# (defun provided-arguments-ref (provided-args arglist &rest indices)
#   """Returns the argument in PROVIDED-ARGUMENT along the INDICES path
# relative to ARGLIST."""
#   (check-type arglist arglist)
#   (flet ((ref (provided-args arglist index)
#            (if (numberp index)
#                (nth index provided-args)
#                (let ((provided-keys (subseq provided-args
#                                             (positional-args-number arglist))))
#                  (loop for (key value) on provided-keys
#                        when (eq key index)
#                          return value)))))
#     (dolist (idx indices)
#       (setq provided-args (ref provided-args arglist idx))
#       (setq arglist (arglist-ref arglist idx)))
#     provided-args))

# (defun positional-args-number (arglist)
#   (+ (length (arglist.provided-args arglist))
#      (length (arglist.required-args arglist))
#      (length (arglist.optional-args arglist))))

# (defun parse-raw-form (raw-form)
#   """Parse a RAW-FORM into a Lisp form. I.e. substitute strings by
# symbols if already interned. For strings not already interned, use
# ARGLIST-DUMMY."""
#   (unless (null raw-form)
#     (loop for element in raw-form 
#           collect (etypecase element
#                     (string (read-conversatively element))
#                     (list   (parse-raw-form element))
#                     (symbol (prog1 element
#                               ## Comes after list, so ELEMENT can't be NIL.
#                               (assert (eq element +cursor-marker+))))))))

# (defun read-conversatively (string)
#   """Tries to find the symbol that's represented by STRING.

# If it can't, this either means that STRING does not represent a
# symbol, or that the symbol behind STRING would have to be freshly
# interned. Because this function is supposed to be called from the
# automatic arglist display stuff from Slime, interning freshly
# symbols is a big no-no.

# In such a case (that no symbol could be found), an object of type
# ARGLIST-DUMMY is returned instead, which works as a placeholder
# datum for subsequent logics to rely on."""
#   (let* ((string  (string-left-trim (quote (#\Space #\Tab #\Newline
#                                            ) string)))
#          (length  (length string))
# 	 (type    (cond ((zerop length) nil)
#                         ((eql (aref string 0) #\'
#                           )
#                          :quoted-symbol)
#                         ((search "#'" string :end2 (min length 2))
#                          :sharpquoted-symbol)
#                         ((and (eql (aref string 0) #\"
#                                )
#                               (eql (aref string (1- length)) #\"
#                                ))
#                          :string)
#                         (t
#                          :symbol))))
#     (multiple-value-bind (symbol found?)
# 	(case type
#           (:symbol             (parse-symbol string))
#           (:quoted-symbol      (parse-symbol (subseq string 1)))
#           (:sharpquoted-symbol (parse-symbol (subseq string 2)))
#           (:string             (values string t))
#           (t                   (values string nil)))
#       (if found?
#           (ecase type
#             (:symbol             symbol)
#             (:quoted-symbol      `(quote ,symbol))
#             (:sharpquoted-symbol `(function ,symbol))
#             (:string             string))
# 	  (make-arglist-dummy string)))))

# (defun test-print-arglist ()
#   (flet ((test (arglist string)
#            (let* ((*package* (find-package :swank))
#                   (actual  (decoded-arglist-to-string (decode-arglist arglist))))
#              (unless (string= actual string)
#                (warn "Test failed: ~S => ~S~%  Expected: ~S"
#                      arglist actual string)))))
#     (test (quote (function cons)) "(function cons)")
#     (test (quote (quote cons)) "(quote cons)")
#     (test (quote (&key (function (function +)))) "(&key (function #'+))")
#     (test (quote (&whole x y z)) "(y z)")
#     (test (quote (x &aux y z)) "(x)")
#     (test (quote (x &environment env y)) "(x y)")
#     (test (quote (&key ((function f)))) "(&key ((function ..)))")
#     (test (quote (eval-when (&any :compile-toplevel :load-toplevel :execute) &body body))
# 	  "(eval-when (&any :compile-toplevel :load-toplevel :execute) &body body)")
#     (test (quote (declare (optimize &any (speed 1) (safety 1))))
# 	  "(declare (optimize &any (speed 1) (safety 1)))")
#     ))

# (defun test-arglist-ref ()
#   (macrolet ((soft-assert (form)
#                # `(unless ,form
#                #    (warn "Assertion failed: ~S~%" ',form))
#                ))
#     (let ((sample (decode-arglist (quote (x &key ((:k (y z))))))))
#       (soft-assert (eq (arglist-ref sample 0)    (quote x)))
#       (soft-assert (eq (arglist-ref sample :k 0) (quote y)))
#       (soft-assert (eq (arglist-ref sample :k 1) (quote z)))

#       (soft-assert (eq (provided-arguments-ref (quote (a :k (b c))) sample 0)    (quote a)))
#       (soft-assert (eq (provided-arguments-ref (quote (a :k (b c))) sample :k 0) (quote b)))
#       (soft-assert (eq (provided-arguments-ref (quote (a :k (b c))) sample :k 1) (quote c))))))

# (test-print-arglist)
# (test-arglist-ref)
