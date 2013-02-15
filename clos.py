
def poor_man_case(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval or (cval is True) or (cval is t)) if not isinstance(cval, list) else
                    val in cval):
                        return body() if isinstance(body, cold_function_type) else body

def poor_man_defstruct(name, *slots):
        return collections.namedtuple(name, slots)

def only_specified_keys(**keys):
        return dict(((k, v) for k, v in keys.items()
                     if specifiedp(k)))


# STANDARD-OBJECT and basic protocols

# Unregistered Issue C-J-COULD-BE-EXTENDED-TO-FOLLOW-M-J-WITHIN-COMMENTS
def class_of(x):
        return getattr(x, "__class__")

@defclass
class standard_object_t():
        def __init__(self, **initargs):
                super().__init__() # Unregistered Issue PYTHON-OBJECT-DOES-NOT-ACCEPT-ARGUMENTS-BUT-SEE-SUPER-CONSIDERED-HARMFUL
                initialize_instance(self, **initargs)

def slot_boundp(object, slot):            return hasattr(object, slot)

def initialize_instance(instance, **initargs):
        """Called by MAKE-INSTANCE to initialize a newly created INSTANCE. The
generic function is called with the new INSTANCE and the defaulted
initialization argument list.

The system-supplied primary method on INITIALIZE-INSTANCE initializes
the slots of the instance with values according to the INITARGS and
the :INITFORM forms of the slots. It does this by calling the generic
function SHARED-INITIALIZE with the following arguments: the instance,
T (this indicates that all slots for which no initialization arguments
are provided should be initialized according to their :INITFORM
forms), and the INITARGS.

Programmers can define methods for INITIALIZE-INSTANCE to specify
actions to be taken when an instance is initialized. If only after
methods are defined, they will be run after the system-supplied
primary method for initialization and therefore will not interfere
with the default behavior of INITIALIZE-INSTANCE."""
        # Unregistered Issue COMPLIANCE-SPEC-UNCLEAR-ON-NEED-FOR-INITARG-VALIDITY-CHECK
        shared_initialize(instance, t, **initargs)
        return instance

def reinitialize_instance(instance, **initargs):
        """The generic function REINITIALIZE-INSTANCE can be used to
change the values of local slots of an INSTANCE according to
INITARGS. This generic function can be called by users.

The system-supplied primary method for REINITIALIZE-INSTANCE checks
the validity of INITARGS and signals an error if an initarg is
supplied that is not declared as valid. The method then calls the
generic function SHARED-INITIALIZE with the following arguments: the
INSTANCE, NIL (which means no slots should be initialized according to
their initforms), and the INITARGS it received."""
        not_implemented("check of validity of INITARGS")
        shared_initialize(instance, nil, **initargs)
        return instance

def shared_initialize(instance, slot_names, **initargs):
        """shared-initialize instance slot-names &rest initargs &key &allow-other-keys => instance

Method Signatures:

shared-initialize (instance standard-object) slot-names &rest initargs

Arguments and Values:

instance---an object.

slot-names---a list or t.

initargs---a list of keyword/value pairs (of initialization argument names and values).

Description:

The generic function SHARED-INITIALIZE is used to fill the slots of an
instance using INITARGS and :INITFORM forms. It is called when an
instance is created, when an instance is re-initialized, when an
instance is updated to conform to a redefined class, and when an
instance is updated to conform to a different class. The generic
function SHARED-INITIALIZE is called by the system-supplied primary
method for INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE,
UPDATE-INSTANCE-FOR-REDEFINED-CLASS, and
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS.

The generic function SHARED-INITIALIZE takes the following arguments:
the INSTANCE to be initialized, a specification of a set of SLOT-NAMES
accessible in that INSTANCE, and any number of INITARGS. The arguments
after the first two must form an initialization argument list. The
system-supplied primary method on SHARED-INITIALIZE initializes the
slots with values according to the INITARGS and supplied :INITFORM
forms. SLOT-NAMES indicates which slots should be initialized
according to their :INITFORM forms if no initargs are provided for
those slots.

The system-supplied primary method behaves as follows, regardless of
whether the slots are local or shared:

    If an initarg in the initialization argument list specifies a
    value for that slot, that value is stored into the slot, even if a
    value has already been stored in the slot before the method is
    run.

    Any slots indicated by SLOT-NAMES that are still unbound at this
    point are initialized according to their :INITFORM forms. For any
    such slot that has an :INITFORM form, that form is evaluated in
    the lexical environment of its defining DEFCLASS form and the
    result is stored into the slot. For example, if a before method
    stores a value in the slot, the :INITFORM form will not be used to
    supply a value for the slot.

    The rules mentioned in Section 7.1.4 (Rules for Initialization
    Arguments) are obeyed.

The SLOTS-NAMES argument specifies the slots that are to be
initialized according to their :INITFORM forms if no initialization
arguments apply. It can be a list of slot names, which specifies the
set of those slot names; or it can be the symbol T, which specifies
the set of all of the slots.

7.1.4 Rules for Initialization Arguments

The :INITARG slot option may be specified more than once for a given
slot.

The following rules specify when initialization arguments may be
multiply defined:

 * A given initialization argument can be used to initialize more than
   one slot if the same initialization argument name appears in more
   than one :INITARG slot option.

 * A given initialization argument name can appear in the lambda list
   of more than one initialization method.

 * A given initialization argument name can appear both in an :INITARG
   slot option and in the lambda list of an initialization method.

If two or more initialization arguments that initialize the same slot
are given in the arguments to MAKE-INSTANCE, the leftmost of these
initialization arguments in the initialization argument list supplies
the value, even if the initialization arguments have different names.

If two or more different initialization arguments that initialize the
same slot have default values and none is given explicitly in the
arguments to MAKE-INSTANCE, the initialization argument that appears
in a :DEFAULT-INITARGS class option in the most specific of the
classes supplies the value. If a single :DEFAULT-INITARGS class option
specifies two or more initialization arguments that initialize the
same slot and none is given explicitly in the arguments to
MAKE-INSTANCE, the leftmost in the :DEFAULT-INITARGS class option
supplies the value, and the values of the remaining default value
forms are ignored.

Initialization arguments given explicitly in the arguments to
MAKE-INSTANCE appear to the left of defaulted initialization
arguments. Suppose that the classes C1 and C2 supply the values of
defaulted initialization arguments for different slots, and suppose
that C1 is more specific than C2; then the defaulted initialization
argument whose value is supplied by C1 is to the left of the defaulted
initialization argument whose value is supplied by C2 in the defaulted
initialization argument list. If a single :DEFAULT-INITARGS class
option supplies the values of initialization arguments for two
different slots, the initialization argument whose value is specified
farther to the left in the :DEFAULT-INITARGS class option appears
farther to the left in the defaulted initialization argument list.

If a slot has both an :INITFORM form and an :INITARG slot option, and
the initialization argument is defaulted using :DEFAULT-INITARGS or is
supplied to MAKE-INSTANCE, the captured :INITFORM form is neither used
nor evaluated."""
        # Unregistered Issue COMPLIANCE-INITFORM-MECHANISM-NOT-AVAILABLE
        # Unregistered Issue COMPLIANCE-MULTIPLY-DEFINED-INITARGS-NOT-SUPPORTED
        # Unregistered Issue COMPLIANCE-INDIRECT-SLOT-INITARG-RELATIONSHIPS-NOT-SUPPORTED
        # Unregistered Issue COMPLIANCE-DEFAULT-INITARGS-NOT-SUPPORTED
        instance.__dict__.update(initargs)
        return instance

# Generic functions

@defclass
class method_t(standard_object_t):
        "All methods are of this type."

@defclass
class funcallable_standard_class_t(standard_object_t):
        "All funcallable instances are of this type."
        def __call__(self, *args, **keys):
                return self.function(*args, **keys)

@defclass
class generic_function_t(funcallable_standard_class_t):
        "All generic functions are of this type."
        def __init__(self, **initargs): # Simulate a :BEFORE method.
                self.__dependents__ = set()
                py.here("args: %s", initargs)
                super().__init__(**initargs)

# Dependent maintenance protocol

# It is convenient for portable metaobjects to be able to memoize
# information about other metaobjects, portable or otherwise. Because
# class and generic function metaobjects can be reinitialized, and
# generic function metaobjects can be modified by adding and removing
# methods, a means must be provided to update this memoized
# information.
#
# The dependent maintenance protocol supports this by providing a way
# to register an object which should be notified whenever a class or
# generic function is modified. An object which has been registered
# this way is called a dependent of the class or generic function
# metaobject. The dependents of class and generic function metaobjects
# are maintained with ADD-DEPENDENT and REMOVE-DEPENDENT. The
# dependents of a class or generic function metaobject can be accessed
# with MAP-DEPENDENTS. Dependents are notified about a modification by
# calling UPDATE-DEPENDENT. (See the specification of UPDATE-DEPENDENT
# for detailed description of the circumstances under which it is
# called.)
#
# To prevent conflicts between two portable programs, or between
# portable programs and the implementation, portable code must not
# register metaobjects themselves as dependents. Instead, portable
# programs which need to record a metaobject as a dependent, should
# encapsulate that metaobject in some other kind of object, and record
# that object as the dependent. The results are undefined if this
# restriction is violated.
#
# This example shows a general facility for encapsulating metaobjects
# before recording them as dependents. The facility defines a basic
# kind of encapsulating object: an UPDATER. Specializations of the
# basic class can be defined with appropriate special updating
# behavior. In this way, information about the updating required is
# associated with each updater rather than with the metaobject being
# updated.
#
# Updaters are used to encapsulate any metaobject which requires
# updating when a given class or generic function is modified. The
# function RECORD-UPDATER is called to both create an UPDATER and add
# it to the dependents of the class or generic function. Methods on
# the generic function UPDATE-DEPENDENT, specialized to the specific
# class of UPDATER do the appropriate update work.
#
# (defclass updater ()
#      ((dependent :initarg :dependent :reader dependent)))
#
# (defun record-updater (class dependee dependent &rest initargs)
#   (let ((updater (apply #'make-instance class :dependent dependent initargs)))
#     (add-dependent dependee updater)
#     updater))
#
# A FLUSH-CACHE-UPDATER simply flushes the cache of the dependent when
# it is updated.
#
# (defclass flush-cache-updater (updater) ())
#
# (defmethod update-dependent (dependee (updater flush-cache-updater) &rest args)
#   (declare (ignore args))
#   (flush-cache (dependent updater)))

def add_dependent(metaobject, dependent):
        """add-dependent metaobject dependent

Arguments:

The METAOBJECT argument is a class or generic function metaobject.

The DEPENDENT argument is an object.

Values:

The value returned by this generic function is unspecified.

Purpose:

This generic function adds DEPENDENT to the dependents of
METAOBJECT. If DEPENDENT is already in the set of dependents it is not
added again (no error is signaled).

The generic function MAP-DEPENDENTS can be called to access the set of
dependents of a class or generic function. The generic function
REMOVE-DEPENDENT can be called to remove an object from the set of
dependents of a class or generic function. The effect of calling
ADD-DEPENDENT or REMOVE-DEPENDENT while a call to MAP-DEPENDENTS on
the same class or generic function is in progress is unspecified.

The situations in which ADD-DEPENDENT is called are not specified."""
        metaobject.__dependents__.add(dependent)

def remove_dependent(metaobject, dependent):
        """remove-dependent metaobject dependent

Arguments:

The METAOBJECT argument is a class or generic function metaobject.

The DEPENDENT argument is an object.

Values:

The value returned by this generic function is unspecified.

Purpose:

This generic function removes DEPENDENT from the dependents of
METAOBJECT. If DEPENDENT is not one of the dependents of metaobject,
no error is signaled.

The generic function MAP-DEPENDENTS can be called to access the set of
dependents of a class or generic function. The generic function
ADD-DEPENDENT can be called to add an object from the set of
dependents of a class or generic function. The effect of calling
ADD-DEPENDENT or REMOVE-DEPENDENT while a call to MAP-DEPENDENTS on
the same class or generic function is in progress is unspecified.

The situations in which REMOVE-DEPENDENT is called are not specified."""
        if dependent in metaobject.__dependents__:
                metaobject.__dependents__.remove(dependent)

def map_dependents(metaobject, function):
        """map-dependents metaobject function

Arguments:

The METAOBJECT argument is a class or generic function metaobject.

The FUNCTION argument is a function which accepts one argument.

Values:

The value returned is unspecified.

Purpose:

This generic function applies FUNCTION to each of the dependents of
METAOBJECT. The order in which the dependents are processed is not
specified, but function is applied to each dependent once and only
once. If, during the mapping, ADD-DEPENDENT or REMOVE-DEPENDENT is
called to alter the dependents of METAOBJECT, it is not specified
whether the newly added or removed dependent will have function
applied to it."""
        mapc(function, metaobject.__dependents__)

def update_dependent(metaobject, dependent, **initargs):
        """update-dependent metaobject dependent &rest initargs

Arguments:

The METAOBJECT argument is a class or generic function metaobject. It
is the metaobject being reinitialized or otherwise modified.

The DEPENDENT argument is an object. It is the dependent being
updated.

The INITARGS argument is a list of the initialization arguments for
the metaobject redefinition.

Values:

The value returned by UPDATE-DEPENDENT is unspecified.

Purpose:

This generic function is called to update a dependent of METAOBJECT.

When a class or a generic function is reinitialized each of its
dependents is updated. The INITARGS argument to UPDATE-DEPENDENT is
the set of initialization arguments received by REINITIALIZE-INSTANCE.

When a method is added to a generic function, each of the generic
function's dependents is updated. The INITARGS argument is a list of
two elements: the symbol ADD-METHOD, and the method that was added.

When a method is removed from a generic function, each of the generic
function's dependents is updated. The INITARGS argument is a list of
two elements: the symbol REMOVE-METHOD, and the method that was
removed.

In each case, MAP-DEPENDENTS is used to call UPDATE-DEPENDENT on each
of the dependents. So, for example, the update of a generic function's
dependents when a method is added could be performed by the following
code:

  (map-dependents generic-function
                  #'(lambda (dep)
                      (update-dependent generic-function
                                        dep
                                        'add-method
                                        new-method)))
"""
        # Unregistered Issue COMPLIANCE-UPDATE-DEPENDENT-DOES-NOT-REALLY-DO-ANYTHING
        pass

# Generic function methods

@defclass
class method_combination_t():
        "All method combinations are of this type."

@defclass
class standard_method_t(method_t):
        def __init__(self, **initargs):
                super().__init__(**initargs)
                standard_method_shared_initialize(self, **initargs)
        def __call__(self, gfun_args, next_methods):
                return self.function(gfun_args, next_methods)

@defclass
class standard_generic_function_t(generic_function_t):
        def __init__(self, **initargs):
                super().__init__(**initargs)
                standard_generic_function_shared_initialize(self, **initargs)
        # def __call__ ..is installed during EMF computation, with the proper arglist.

def update_generic_function_and_dependents(generic_function, **initargs):
        set_funcallable_instance_function(generic_function,
                                          compute_discriminating_function(generic_function))
        map_dependents(generic_function,
                       lambda dep: update_dependent(generic_function, dep, **initargs))

def standard_generic_function_shared_initialize(generic_function,
                                                 argument_precedence_order = None,
                                                 declarations = None,
                                                 documentation = None,
                                                 lambda_list = None,
                                                 method_combination = None,
                                                 method_class = None,
                                                 name = None,
                                                 # extensions
                                                 filename = None,
                                                 lineno = None):
        """Initialization of Generic Function Metaobjects

A generic function metaobject can be created by calling
MAKE-INSTANCE. The initialization arguments establish the definition
of the generic function. A generic function metaobject can be
redefined by calling REINITIALIZE-INSTANCE. Some classes of generic
function metaobject do not support redefinition; in these cases,
REINITIALIZE-INSTANCE signals an error.

Initialization of a generic function metaobject must be done by
calling MAKE-INSTANCE and allowing it to call
INITIALIZE-INSTANCE. Reinitialization of a GENERIC-FUNCTION metaobject
must be done by calling REINITIALIZE-INSTANCE. Portable programs must
not call INITIALIZE-INSTANCE directly to initialize a generic function
metaobject. Portable programs must not call SHARED-INITIALIZE directly
to initialize or reinitialize a generic function metaobject. Portable
programs must not call CHANGE-CLASS to change the class of any generic
function metaobject or to turn a non-generic-function object into a
generic function metaobject.

Since metaobject classes may not be redefined, no behavior is
specified for the result of calls to
UPDATE-INSTANCE-FOR-REDEFINED-CLASS on generic function
metaobjects. Since the class of a generic function metaobject may not
be changed, no behavior is specified for the results of calls to
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS on generic function metaobjects.

During initialization or reinitialization, each initialization
argument is checked for errors and then associated with the generic
function metaobject. The value can then be accessed by calling the
appropriate accessor as shown in Table 3.

This section begins with a description of the error checking and
processing of each initialization argument. This is followed by a
table showing the generic functions that can be used to access the
stored initialization arguments. The section ends with a set of
restrictions on portable methods affecting generic function metaobject
initialization and reinitialization.

In these descriptions, the phrase ``this argument defaults to value''
means that when that initialization argument is not supplied,
initialization or reinitialization is performed as if value had been
supplied. For some initialization arguments this could be done by the
use of default initialization arguments, but whether it is done this
way is not specified. Implementations are free to define default
initialization arguments for specified generic function metaobject
classes. Portable programs are free to define default initialization
arguments for portable subclasses of the class GENERIC-FUNCTION.

Unless there is a specific note to the contrary, then during
reinitialization, if an initialization argument is not supplied, the
previously stored value is left unchanged.

    The :ARGUMENT-PRECEDENCE-ORDER argument is a list of symbols.

    An error is signaled if this argument appears but the :LAMBDA-LIST
    argument does not appear. An error is signaled if this value is
    not a proper list or if this value is not a permutation of the
    symbols from the required arguments part of the :LAMBDA-LIST
    initialization argument.

    When the generic function is being initialized or reinitialized,
    and this argument is not supplied, but the :LAMBDA-LIST argument
    is supplied, this value defaults to the symbols from the required
    arguments part of the :LAMBDA-LIST argument, in the order they
    appear in that argument. If neither argument is supplied, neither
    are initialized (see the description of :LAMBDA-LIST.)

    The :DECLARATIONS argument is a list of declarations.

    An error is signaled if this value is not a proper list or if each
    of its elements is not a legal declaration.

    When the generic function is being initialized, and this argument
    is not supplied, it defaults to the empty list.

    The :DOCUMENTATION argument is a string or NIL.

    An error is signaled if this value is not a string or NIL.

    If the generic function is being initialized, this argument
    defaults to NIL.

    The :LAMBDA-LIST argument is a lambda list.

    An error is signaled if this value is not a proper generic
    function lambda list.

    When the generic function is being initialized, and this argument
    is not supplied, the generic function's lambda list is not
    initialized. The lambda list will be initialized later, either
    when the first method is added to the generic function, or a later
    reinitialization of the generic function.

    The :METHOD-COMBINATION argument is a method combination
    metaobject.

    The :METHOD-CLASS argument is a class metaobject.

    An error is signaled if this value is not a subclass of the class
    METHOD.

    When the generic function is being initialized, and this argument
    is not supplied, it defaults to the class STANDARD-METHOD.

    The :NAME argument is an object.

    If the generic function is being initialized, this argument
    defaults to NIL.

After the processing and defaulting of initialization arguments
described above, the value of each initialization argument is
associated with the generic function metaobject. These values can then
be accessed by calling the corresponding generic function. The
correspondences are as follows:

Table 2: Initialization arguments and accessors for generic function metaobjects.

Initialization Argument         Generic Function
--------------------------------------------------------------------------
:argument-precedence-order      generic-function-argument-precedence-order
:declarations                   generic-function-declarations
:documentation                  documentation
:lambda-list                    generic-function-lambda-list
:method-combination             generic-function-method-combination
:method-class                   generic-function-method-class
:name                           generic-function-name

Methods:

It is not specified which methods provide the initialization and
reinitialization behavior described above. Instead, the information
needed to allow portable programs to specialize this behavior is
presented as a set of restrictions on the methods a portable program
can define. The model is that portable initialization methods have
access to the generic function metaobject when either all or none of
the specified initialization has taken effect."""
        # Unregistered Issue COMPLIANCE-METHOD-CLASS-ARGUMENT-TYPE-CHECK-NOT-PRECISE-ENOUGH
        if specifiedp(argument_precedence_order):
                if not specifiedp(lambda_list):
                        error("MAKE-INSTANCE STANDARD-GENERIC-FUNCTION: :ARGUMENT-PRECEDENCE-ORDER "
                              "was provided, but :LAMBDA-LIST was not.")
                elif not (listp(argument_precedence_order) and
                          set(argument_precedence_order) == set(lambda_list[0])):
                        error("MAKE-INSTANCE STANDARD-GENERIC-FUNCTION: :ARGUMENT-PRECEDENCE-ORDER, "
                              "when specified, must be a permutation of fixed arguments in :LAMBDA-LIST.  "
                              "Was: %s;  fixed LAMBDA-LIST args: %s.",
                              repr(argument_precedence_order), lambda_list[0])
                generic_function.argument_precedence_order = tuple(argument_precedence_order)
        elif specifiedp(lambda_list):
                generic_function.argument_precedence_order = tuple(lambda_list[0])
        generic_function.declarations        = tuple(defaulted(declarations, nil,
                                                                type = (pylist_t,
                                                                        (satisfies_t, valid_declaration_p))))
        generic_function.documentation       = defaulted(documentation, nil,
                                              type = (or_t, string_t, (eql_t, nil)))
        if specifiedp(lambda_list):
                # XXX: not_implemented("lambda-list validation")
                generic_function.lambda_list = lambda_list
        generic_function.method_combination  = defaulted(method_combination, standard_method_combination_t,
                                                          type = cold_class_type)
        generic_function.method_class        = defaulted(method_class, standard_method_t,
                                                          type = cold_class_type) # method metaclass
        generic_function.name                = defaulted(name, nil)
        # The discriminating function may reuse the
        # list of applicable methods without calling
        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
        # (ii) the generic function has not been reinitialized,
        generic_function.__applicable_method_cache__ = dict() # (_list, type) -> list
        generic_function.__methods__ = dict()
        filename, lineno = (defaulted(filename, "<unknown>"),
                            defaulted(lineno,   0))
        update_generic_function_and_dependents(
                generic_function,
                **_only_specified_keys(argument_precedence_order = argument_precedence_order,
                                       declarations = declarations,
                                       documentation = documentation,
                                       lambda_list = lambda_list,
                                       method_combination = method_combination,
                                       method_class = method_class,
                                       name = name,
                                       # extensions
                                       filename = filename,
                                       lineno = lineno))
        # Simulate a python function (XXX: factor):
        generic_function.__doc__ = documentation
        # generic_function.__code__.co_filename    = filename
        # generic_function.__code__.co_firstlineno = lineno
        return generic_function

def generic_function_argument_precedence_order(x): return x.argument_precedence_order
def generic_function_declarations(x):              return x.declarations
def generic_function_lambda_list(x):               return x.lambda_list
def generic_function_method_combination(x):        return x.method_combination
def generic_function_method_class(x):              return x.method_class
def generic_function_name(x):                      return x.name

def generic_function_p(x): return functionp(x) and hasattr(x, "__methods__")  # XXX: CL+
def specializerp(x):       return ((x is t)        or
                                    typep(x, (or_t, type, (pytuple_t, (eql_t, eql), t))))

def get_generic_fun_info(generic_function):
        return (len(generic_function.lambda_list[0]), # nreq
                nil,
                [],
                len(generic_function.lambda_list[3]),
                generic_function.lambda_list)

def generic_function_methods(x):                   return x.__methods__.values()

# DEFINE-METHOD-COMBINATION

__method_combinations__ = dict()

def define_method_combination(name, method_group_specifiers, body,
                              arguments = None, generic_function = None):
        """Syntax:

define-method-combination name [[short-form-option]]

=> name

define-method-combination name lambda-list (method-group-specifier*) [(:arguments . args-lambda-list)] [(:generic-function generic-function-symbol)] [[declaration* | documentation]] form*

=> name

short-form-option::= :documentation documentation |
                     :identity-with-one-argument identity-with-one-argument |
                     :operator operator

method-group-specifier::= (name {qualifier-pattern+ | predicate} [[long-form-option]])

long-form-option::= :description description |
                    :order order |
                    :required required-p

Arguments and Values:

args-lambda-list---a define-method-combination arguments lambda list.

declaration---a declare expression; not evaluated.

description---a format control.

documentation---a string; not evaluated.

forms---an implicit progn that must compute and return the form that specifies how the methods are combined, that is, the effective method.

generic-function-symbol---a symbol.

identity-with-one-argument---a generalized boolean.

lambda-list---ordinary lambda list.

name---a symbol. Non-keyword, non-nil symbols are usually used.

operator---an operator. Name and operator are often the same symbol. This is the default, but it is not required.

order---:most-specific-first or :most-specific-last; evaluated.

predicate---a symbol that names a function of one argument that returns a generalized boolean.

qualifier-pattern---a list, or the symbol *.

required-p---a generalized boolean.

Description:

The macro DEFINE-METHOD-COMBINATION is used to define new types of
method combination.

There are two forms of DEFINE-METHOD-COMBINATION. The short form is a
simple facility for the cases that are expected to be most commonly
needed. The long form is more powerful but more verbose. It resembles
DEFMACRO in that the body is an expression, usually using backquote,
that computes a form. Thus arbitrary control structures can be
implemented. The long form also allows arbitrary processing of method
qualifiers.

Short Form

    The short form syntax of DEFINE-METHOD-COMBINATION is recognized
    when the second subform is a non-NIL symbol or is not
    present. When the short form is used, name is defined as a type of
    method combination that produces a Lisp form (operator method-call
    method-call ...). The operator is a symbol that can be the name of
    a function, macro, or special operator. The operator can be
    supplied by a keyword option; it defaults to name.

    Keyword options for the short form are the following:

        The :DOCUMENTATION option is used to document the
        METHOD-COMBINATION type; see description of long form below.

        The :IDENTITY-WITH-ONE-ARGUMENT option enables an optimization
        when its value is true (the default is false). If there is
        exactly one applicable method and it is a primary method, that
        method serves as the effective method and operator is not
        called. This optimization avoids the need to create a new
        effective method and avoids the overhead of a function
        call. This option is designed to be used with operators such
        as PROGN, AND, +, and MAX.

        The :OPERATOR option specifies the name of the operator. The
        operator argument is a symbol that can be the name of a
        function, macro, or special form.

    These types of method combination require exactly one qualifier
    per method. An error is signaled if there are applicable methods
    with no qualifiers or with qualifiers that are not supported by
    the method combination type.

    A method combination procedure defined in this way recognizes two
    roles for methods. A method whose one qualifier is the symbol
    naming this type of method combination is defined to be a primary
    method. At least one primary method must be applicable or an error
    is signaled. A method with :AROUND as its one qualifier is an
    auxiliary method that behaves the same as an around method in
    standard method combination. The function CALL-NEXT-METHOD can
    only be used in around methods; it cannot be used in primary
    methods defined by the short form of the DEFINE-METHOD-COMBINATION
    macro.

    A method combination procedure defined in this way accepts an
    optional argument named order, which defaults
    to :MOST-SPECIFIC-FIRST. A value of :MOST-SPECIFIC-LAST reverses
    the order of the primary methods without affecting the order of
    the auxiliary methods.

    The short form automatically includes error checking and support
    for around methods.

    For a discussion of built-in method combination types, see Section
    7.6.6.4 (Built-in Method Combination Types).

Long form (snipped)

    The long form syntax of DEFINE-METHOD-COMBINATION is recognized
    when the second subform is a list.

    The LAMBDA-LIST receives any arguments provided after the name of
    the method combination type in the :METHOD-COMBINATION option to
    DEFGENERIC.

    A list of method group specifiers follows. Each specifier selects
    a subset of the applicable methods to play a particular role,
    either by matching their qualifiers against some patterns or by
    testing their qualifiers with a predicate. These method group
    specifiers define all method qualifiers that can be used with this
    type of method combination.

    The car of each METHOD-GROUP-SPECIFIER is a symbol which names a
    variable. During the execution of the forms in the body of
    DEFINE-METHOD-COMBINATION, this variable is bound to a list of the
    methods in the method group. The methods in this list occur in the
    order specified by the :ORDER option.

    If QUALIFIER-PATTERN is a symbol it must be *. A method matches a
    qualifier-pattern if the method's list of qualifiers is equal to
    the QUALIFIER-PATTERN (except that the symbol * in a
    qualifier-pattern matches anything). Thus a qualifier-pattern can
    be one of the following: the empty list, which matches unqualified
    methods; the symbol *, which matches all methods; a true list,
    which matches methods with the same number of qualifiers as the
    length of the list when each qualifier matches the corresponding
    list element; or a dotted list that ends in the symbol * (the *
    matches any number of additional qualifiers).

    Each applicable method is tested against the qualifier-patterns
    and predicates in left-to-right order. As soon as a
    qualifier-pattern matches or a predicate returns true, the method
    becomes a member of the corresponding method group and no further
    tests are made. Thus if a method could be a member of more than
    one method group, it joins only the first such group. If a method
    group has more than one qualifier-pattern, a method need only
    satisfy one of the qualifier-patterns to be a member of the group.

    The name of a predicate function can appear instead of
    qualifier-patterns in a method group specifier. The predicate is
    called for each method that has not been assigned to an earlier
    method group; it is called with one argument, the method's
    qualifier list. The predicate should return true if the method is
    to be a member of the method group. A predicate can be
    distinguished from a qualifier-pattern because it is a symbol
    other than nil or *.

    If there is an applicable method that does not fall into any
    method group, the function invalid-method-error is called.

    Method group specifiers can have keyword options following the
    qualifier patterns or predicate. Keyword options can be
    distinguished from additional qualifier patterns because they are
    neither lists nor the symbol *. The keyword options are as
    follows:

        The :DESCRIPTION option is used to provide a description of
        the role of methods in the method group. Programming
        environment tools use (APPLY #'FORMAT STREAM FORMAT-CONTROL
        (METHOD-QUALIFIERS METHOD)) to print this description, which
        is expected to be concise. This keyword option allows the
        description of a method qualifier to be defined in the same
        module that defines the meaning of the method qualifier. In
        most cases, FORMAT-CONTROL will not contain any format
        directives, but they are available for generality. If
        :DESCRIPTION is not supplied, a default description is
        generated based on the variable name and the qualifier
        patterns and on whether this method group includes the
        unqualified methods.

        The :ORDER option specifies the order of methods. The order
        argument is a form that evaluates to :MOST-SPECIFIC-FIRST or
        :MOST-SPECIFIC-LAST. If it evaluates to any other value, an
        error is signaled. If :ORDER is not supplied, it defaults to
        :MOST-SPECIFIC-FIRST.

        The :REQUIRED option specifies whether at least one method in
        this method group is required. If its value is true and the
        method group is empty (that is, no applicable methods match
        the qualifier patterns or satisfy the predicate), an error is
        signaled. If :REQUIRED is not supplied, it defaults to NIL.

    The use of method group specifiers provides a convenient syntax to
    select methods, to divide them among the possible roles, and to
    perform the necessary error checking. It is possible to perform
    further filtering of methods in the body forms by using normal
    list-processing operations and the functions METHOD-QUALIFIERS and
    INVALID-METHOD-ERROR. It is permissible to use SETQ on the
    variables named in the method group specifiers and to bind
    additional variables. It is also possible to bypass the method
    group specifier mechanism and do everything in the body
    forms. This is accomplished by writing a single method group with
    * as its only qualifier-pattern; the variable is then bound to a
    list of all of the applicable methods, in MOST-SPECIFIC-FIRST
    order.

    The BODY forms compute and return the form that specifies how the
    methods are combined, that is, the effective method. The effective
    method is evaluated in the null lexical environment augmented with
    a local macro definition for CALL-METHOD and with bindings named
    by symbols not accessible from the COMMON-LISP-USER package. Given
    a method object in one of the lists produced by the method group
    specifiers and a list of next methods, CALL-METHOD will invoke the
    method such that CALL-NEXT-METHOD has available the next methods.

    When an effective method has no effect other than to call a single
    method, some implementations employ an optimization that uses the
    single method directly as the effective method, thus avoiding the
    need to create a new effective method. This optimization is active
    when the effective method form consists entirely of an invocation
    of the CALL-METHOD macro whose first subform is a method object
    and whose second subform is NIL or unsupplied. Each
    DEFINE-METHOD-COMBINATION body is responsible for stripping off
    redundant invocations of PROGN, AND, MULTIPLE-VALUE-PROG1, and the
    like, if this optimization is desired.

    The list (:ARGUMENTS . LAMBDA-LIST) can appear before any
    declarations or documentation string. This form is useful when the
    method combination type performs some specific behavior as part of
    the combined method and that behavior needs access to the
    arguments to the generic function. Each parameter variable defined
    by LAMBDA-LIST is bound to a form that can be inserted into the
    effective method. When this form is evaluated during execution of
    the effective method, its value is the corresponding argument to
    the generic function; the consequences of using such a form as the
    place in a SETF form are undefined. Argument correspondence is
    computed by dividing the :ARGUMENTS LAMBDA-LIST and the generic
    function LAMBDA-LIST into three sections: the required parameters,
    the optional parameters, and the keyword and rest parameters. The
    arguments supplied to the generic function for a particular call
    are also divided into three sections; the required arguments
    section contains as many arguments as the generic function has
    required parameters, the optional arguments section contains as
    many arguments as the generic function has optional parameters,
    and the keyword/rest arguments section contains the remaining
    arguments. Each parameter in the required and optional sections of
    the :ARGUMENTS LAMBDA-LIST accesses the argument at the same
    position in the corresponding section of the arguments. If the
    section of the :ARGUMENTS LAMBDA-LIST is shorter, extra arguments
    are ignored. If the section of the :ARGUMENTS LAMBDA-LIST is
    longer, excess required parameters are bound to forms that
    evaluate to NIL and excess optional parameters are bound to their
    initforms. The keyword parameters and rest parameters in the
    :ARGUMENTS LAMBDA-LIST access the keyword/rest section of the
    arguments. If the :ARGUMENTS LAMBDA-LIST contains &key, it behaves
    as if it also contained &allow-other-keys.

    In addition, &whole var can be placed first in the :ARGUMENTS
    LAMBDA-LIST. It causes var to be bound to a form that evaluates to
    a list of all of the arguments supplied to the generic
    function. This is different from &rest because it accesses all of
    the arguments, not just the keyword/rest arguments.

    Erroneous conditions detected by the body should be reported with
    METHOD-COMBINATION-ERROR or INVALID-METHOD-ERROR; these functions
    add any necessary contextual information to the error message and
    will signal the appropriate error.

    The BODY forms are evaluated inside of the bindings created by the
    lambda list and method group specifiers. Declarations at the head
    of the BODY are positioned directly inside of bindings created by
    the lambda list and outside of the bindings of the method group
    variables. Thus method group variables cannot be declared in this
    way. LOCALLY may be used around the BODY, however.

    Within the BODY forms, GENERIC-FUNCTION-SYMBOL is bound to the
    generic function object.

    Documentation is attached as a documentation string to name (as
    kind method-combination) and to the method combination object.

    Note that two methods with identical specializers, but with
    different qualifiers, are not ordered by the algorithm described
    in Step 2 of the method selection and combination process
    described in Section 7.6.6 (Method Selection and
    Combination). Normally the two methods play different roles in the
    effective method because they have different qualifiers, and no
    matter how they are ordered in the result of Step 2, the effective
    method is the same. If the two methods play the same role and
    their order matters, an error is signaled. This happens as part of
    the qualifier pattern matching in define-method-combination.

If a DEFINE-METHOD-COMBINATION form appears as a top level form, the
compiler must make the method combination name be recognized as a
valid method combination name in subsequent defgeneric forms. However,
the method combination is executed no earlier than when the
DEFINE-METHOD-COMBINATION form is executed, and possibly as late as
the time that generic functions that use the method combination are
executed."""
        ## define_method_combination(name, method_group_specifiers, body,
        #                            arguments = None, generic_function = None)
        # Unregistered Issue COMPLIANCE-ARGUMENTS-LAMBDA-LIST-NOT-IMPLEMENTED
        # Unregistered Issue COMPLIANCE-ERRORNEOUS-CONDITION-REPORTING
        # Unregistered Issue COMPLIANCE-SPECIAL-CASE-(APPLY #'FORMAT STREAM FORMAT-CONTROL (METHOD-QUALIFIERS METHOD))-NOT-IMPLEMENTED
        # Unregistered Issue PERFORMANCE-SINGLE-APPLICABLE-METHOD-OPTIMISATION
        check_type(method_group_specifiers,
                  (pylist_t,
                   (varituple_t,
                    symbol_t,       # group name
                    (or_t, (pylist_t, (or_t, pytuple_t, (eql_t, star))), # We're off the spec a little here,
                                                                         # but it's a minor syntactic issue.
                          function_t),
                    # the rest is actually a plist, but we cannot (yet) describe it
                    # in terms of a type.
                    (maybe_t, (pytuple_t,
                               (eql_t, make_keyword("description")),
                               string_t)),
                    (maybe_t, (pytuple_t,
                               (eql_t, make_keyword("order")),
                               (member_t,
                                make_keyword("most-specific-first"),
                                make_keyword("most-specific-last")))),
                    (maybe_t, (pytuple_t,
                               (eql_t, make_keyword("required")),
                               (member_t, t, nil))))))
        # check_type(arguments, (maybe, lambda_list_))
        check_type(arguments, (maybe_t, (pytuple_t,
                                         (pylist_t, string_t),
                                         (pylist_t, string_t),
                                         (maybe_t, string_t),
                                         (pylist_t, string_t),
                                         (maybe_t, string_t))))
        check_type(generic_function, (maybe_t, symbol_t))
        ### VARI-BIND, anyone?
        # def vari_bind(x, body):
        #         # don't lambda lists actually rule this, hands down?
        #         posnal, named = argspec_value_varivals(inspect.getfullargspec(body), x)
        #         return body()
        method_group = poor_man_defstruct("method_group",
                                           "name",
                                           "qualifier_spec",
                                           "description",
                                           "most_specific_first",
                                           "required")
        groups = dict()
        for mgspec in method_group_specifiers:
                gname, qualifier_spec = mgspec[:2]
                options = mgspec[2:]
                options_dict = map_into_hash_star(lambda keyword, v: (symbol_name(keyword), v), options)
                (lambda description = "Method group %s.",
                        required = nil,
                        order = make_keyword("most_specific_first"):
                        groups.update({gname: method_group(gname,
                                                           qualifier_spec,
                                                           description,
                                                           order is make_keyword("most_specific_first"),
                                                           required)}))(**options_dict)
        def method_combination(applicable_methods, *args, **keys):
               # The LAMBDA-LIST receives any arguments provided after the name of
               # the method combination type in the :METHOD-COMBINATION option to
               # DEFGENERIC.
               # /citation
               #
               # The BODY forms are evaluated inside of the bindings created by the
               # lambda list and method group specifiers.
               # /citation
               #
               # The effective method is evaluated in the null lexical environment
               # augmented with a local macro definition for CALL-METHOD and with
               # bindings named by symbols not accessible from the COMMON-LISP-USER
               # package.
               # /citation
               #
               # Within the BODY forms, GENERIC-FUNCTION-SYMBOL is bound to the
               # generic function object.
               # /citation
               def method_qualifiers_match_pattern_p(qualifiers, pattern):
                       return (t if pattern is star else
                               qualifiers == pattern)
               grouped_methods = defaultdict(list)
               for method in applicable_methods:
                       qualifiers = method_qualifiers(method)
                       for group in groups.values():
                               qualifier_spec = group.qualifier_spec
                               ## qualifier_spec:
                               # (or_t, (pylist_t, (or_t, star, list)),
                               #        function_t),
                               if ((listp(qualifier_spec) and
                                    any(method_qualifiers_match_pattern_p(qualifiers, x)
                                        for x in qualifier_spec))
                                   # must be an fbound symbol, as per the above TYPEP
                                   or qualifier_spec(qualifiers)):
                                       grouped_methods[group.name].append(method)
                       else:
                               ## XXX: ought to be a call to INVALID-METHOD-ERROR
                               error("Applicable method %s with qualifiers %s does not fall into any method group.",
                                     method, qualifiers)
               for group in groups.values():
                       if group.required and not grouped_methods[group.name]:
                               error("Method group %s requires at least one method.", group.name)
                       if not group.most_specific_first:
                               grouped_methods[group.name].reverse()
               ## So: must bind group names and CALL-METHOD, which, in turn
               ## must bind CALL-NEXT-METHOD and NEXT-METHOD-P.  I presume.
               # BODY must, therefore, return some kind of an AST representation?
               # I guess, well, that we could play the CL games with that.
               # Yes, I'm thinking of real macros..
               # Maybe just arg it?
               body_args = dict(grouped_methods)
               def call_method(method, next_methods = None):
                       next_methods = defaulted(next_methods, [])
                       # sounds like we ought to look up the compiled METHOD-LAMBDA?
                       # Given a method object in one of the lists produced
                       # by the method group specifiers and a list of next
                       # methods, CALL-METHOD will invoke the method such that
                       # CALL-NEXT-METHOD has available the next methods.
                       method_lambda
               body_args.update({ "call_method": call_method })
               return body(**body_args)
        method_combination.name                        = the(symbol_t, name)
        method_combination.__method_group_specifiers__ = method_group_specifiers
        return method_combination

# Standard method combination

# 7.6.6.2 Standard Method Combination
#
# Standard method combination is supported by the class
# standard-generic-function. It is used if no other type of method
# combination is specified or if the built-in method combination type
# standard is specified.
#
# Primary methods define the main action of the effective method, while
# auxiliary methods modify that action in one of three ways. A primary
# method has no method qualifiers.
#
# An auxiliary method is a method whose qualifier is :before, :after,
# or :around. Standard method combination allows no more than one
# qualifier per method; if a method definition specifies more than one
# qualifier per method, an error is signaled.
#
# * A before method has the keyword :before as its only qualifier. A
#   before method specifies code that is to be run before any primary
#   methods.
#
# * An after method has the keyword :after as its only qualifier. An
#   after method specifies code that is to be run after primary methods.
#
# * An around method has the keyword :around as its only qualifier. An
#   around method specifies code that is to be run instead of other
#   applicable methods, but which might contain explicit code which
#   calls some of those shadowed methods (via call-next-method).
#
# The semantics of standard method combination is as follows:
#
# * If there are any around methods, the most specific around method is
#   called. It supplies the value or values of the generic function.
#
# * Inside the body of an around method, call-next-method can be used to
#   call the next method. When the next method returns, the around
#   method can execute more code, perhaps based on the returned value or
#   values. The generic function no-next-method is invoked if
#   call-next-method is used and there is no applicable method to
#   call. The function next-method-p may be used to determine whether a
#   next method exists.
#
# * If an around method invokes call-next-method, the next most specific
#   around method is called, if one is applicable. If there are no
#   around methods or if call-next-method is called by the least
#   specific around method, the other methods are called as follows:
#
#     -- All the before methods are called, in most-specific-first
#        order. Their values are ignored. An error is signaled if
#        call-next-method is used in a before method.
#
#     -- The most specific primary method is called. Inside the body of
#        a primary method, call-next-method may be used to call the next
#        most specific primary method. When that method returns, the
#        previous primary method can execute more code, perhaps based on
#        the returned value or values. The generic function
#        no-next-method is invoked if call-next-method is used and there
#        are no more applicable primary methods. The function
#        next-method-p may be used to determine whether a next method
#        exists. If call-next-method is not used, only the most specific
#        primary method is called.
#
#     -- All the after methods are called in most-specific-last
#        order. Their values are ignored. An error is signaled if
#        call-next-method is used in an after method.
#
# * If no around methods were invoked, the most specific primary method
#   supplies the value or values returned by the generic function. The
#   value or values returned by the invocation of call-next-method in
#   the least specific around method are those returned by the most
#   specific primary method.
#
# In standard method combination, if there is an applicable method but
# no applicable primary method, an error is signaled.
#
# The before methods are run in most-specific-first order while the
# after methods are run in least-specific-first order. The design
# rationale for this difference can be illustrated with an
# example. Suppose class C1 modifies the behavior of its superclass, C2,
# by adding before methods and after methods. Whether the behavior of
# the class C2 is defined directly by methods on C2 or is inherited from
# its superclasses does not affect the relative order of invocation of
# methods on instances of the class C1. Class C1's before method runs
# before all of class C2's methods. Class C1's after method runs after
# all of class C2's methods.
#
# By contrast, all around methods run before any other methods run. Thus
# a less specific around method runs before a more specific primary
# method.
#
# If only primary methods are used and if call-next-method is not used,
# only the most specific method is invoked; that is, more specific
# methods shadow more general ones.
standard_method_combination_t = method_combination_t # Crude XXX
# standard_method_combination = define_method_combination(
#         i("STANDARD"),
#         [(i("around"),  [(keyword("around"),)]),
#          (i("before"),  [(keyword("before"),)]),
#          (i("primary"), [tuple()],
#                          (keyword("required"), t)),
#          (i("after"),   [(keyword("after"),)],
#                          (keyword("order"),    make_keyword("most-specific-last")))],
#         lambda: # "around", "before", "primary" and "after" are bound "magically",
#                 # to avoid duplication.
#                 [])

# MAKE-METHOD-LAMBDA

def make_method_lambda(generic_function, method, lambda_expression, environment):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The METHOD argument is a (possibly uninitialized) method metaobject.

The LAMBDA-EXPRESSION argument is a lambda expression.

The ENVIRONMENT argument is the same as the &environment argument to
macro expansion functions.

Values: This generic function returns two values. The first is a
lambda expression, the second is a list of initialization arguments
and values.

Purpose: This generic function is called to produce a lambda
expression which can itself be used to produce a method function for a
method and generic function with the specified classes. The generic
function and method the method function will be used with are not
required to be the given ones. Moreover, the METHOD metaobject may be
uninitialized.

Either the function COMPILE, the special form FUNCTION or the function
COERCE must be used to convert the lambda expression to a method
function. The method function itself can be applied to arguments with
APPLY or FUNCALL.

When a method is actually called by an effective method, its first
argument will be a list of the arguments to the generic function. Its
remaining arguments will be all but the first argument passed to
CALL-METHOD. By default, all method functions must accept two
arguments: the list of arguments to the generic function and the list
of next methods.

For a given generic function and method class, the applicable methods
on MAKE-METHOD-LAMBDA and COMPUTE-EFFECTIVE-METHOD must be consistent
in the following way: each use of CALL-METHOD returned by the method
on COMPUTE-EFFECTIVE-METHOD must have the same number of arguments,
and the method lambda returned by the method on MAKE-METHOD-LAMBDA
must accept a corresponding number of arguments.

Note that the system-supplied implementation of CALL-NEXT-METHOD is
not required to handle extra arguments to the method function. Users
who define additional arguments to the method function must either
redefine or forego CALL-NEXT-METHOD. (See the example below.)

When the method metaobject is created with MAKE-INSTANCE, the method
function must be the value of the :FUNCTION initialization
argument. The additional initialization arguments, returned as the
second value of this generic function, must also be passed in this
call to MAKE-INSTANCE."""
        not_implemented()
        """Return an expression compileable (by whom? compute-effective-method?)
        down to a function, accepting (gf-arglist &rest (subseq c-m-args 1)),
        responsible to invoke the method and ."""
        # (defmacro call-method (method &rest c-m-args)
        #   (apply method.function
        #          gf-arglist (subseq c-m-args 1)))

# COMPUTE-EFFECTIVE-METHOD

def compute_effective_method(generic_function, combin, applicable_methods):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The METHOD-COMBINATION argument is a method combination metaobject.

The METHODS argument is a list of method metaobjects.

Values: This generic function returns two values. The first is an
effective method, the second is a list of effective method options.

Purpose: This generic function is called to determine the effective
method from a sorted list of method metaobjects.

An effective method is a form that describes how the applicable
methods are to be combined. Inside of effective method forms are
CALL-METHOD forms which indicate that a particular method is to be
called. The arguments to the CALL-METHOD form indicate exactly how the
method function of the method should be called. (See
MAKE-METHOD-LAMBDA for more details about method functions.)

An effective method option has the same interpretation and syntax as
either the :ARGUMENTS or the :GENERIC-FUNCTION option in the long form
of DEFINE-METHOD-COMBINATION.

More information about the form and interpretation of effective
methods and effective method options can be found under the
description of the DEFINE-METHOD-COMBINATION macro in the CLOS
specification.

This generic function can be called by the user or the
implementation. It is called by discriminating functions whenever a
sorted list of applicable methods must be converted to an effective
method."""
        not_implemented()

def compute_applicable_methods_using_classes(generic_function, classes):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The CLASSES argument is a list of class metaobjects.

Values: This generic function returns two values. The first is a
possibly empty list of method metaobjects. The second is either true
or false.

Purpose: This generic function is called to attempt to determine the
method applicability of a generic function given only the classes of
the required arguments.

If it is possible to completely determine the ordered list of
applicable methods based only on the supplied classes, this generic
function returns that list as its first value and true as its second
value. The returned list of method metaobjects is sorted by precedence
order, the most specific method coming first. If no methods are
applicable to arguments with the specified classes, the empty list and
true are returned.

If it is not possible to completely determine the ordered list of
applicable methods based only on the supplied classes, this generic
function returns an unspecified first value and false as its second
value.

When a generic function is invoked, the discriminating function must
determine the ordered list of methods applicable to the
arguments. Depending on the generic function and the arguments, this
is done in one of three ways: using a memoized value; calling
COMPUTE-APPLICABLE-METHODS-USING-CLASSES; or calling
COMPUTE-APPLICABLE-METHODS. (Refer to the description of
COMPUTE-DISCRIMINATING-FUNCTION for the details of this process.)

The following consistency relationship between
COMPUTE-APPLICABLE-METHODS-USING-CLASSES and
COMPUTE-APPLICABLE-METHODS must be maintained: for any given generic
function and set of arguments, if
COMPUTE-APPLICABLE-METHODS-USING-CLASSES returns a second value of
true, the first value must be equal to the value that would be
returned by a corresponding call to COMPUTE-APPLICABLE-METHODS. The
results are undefined if a portable method on either of these generic
functions causes this consistency to be violated.

The list returned by this generic function will not be mutated by the
implementation. The results are undefined if a portable program
mutates the list returned by this generic function."""
        return compute_applicable_methods_using_types(generic_function,
                                                       types_from_args(generic_function,
                                                                        classes,
                                                                        class_eq_))

def types_from_args(generic_function, arguments, type_modifier = None):
        nreq, applyp, metatypes, nkeys, arg_info = get_generic_fun_info(generic_function)
        # (declare (ignore applyp metatypes nkeys))
        types_rev = []
        for i in range(nreq):
                if not arguments:
                        error_need_at_least_n_args(generic_function_name(generic_function),
                                                   nreq)
                        arg = arguments.pop()
                        types_rev.append([type_modifier, arg] if type_modifier else
                                         arg)
        return (types_rev, arg_info)

def arg_info_precedence(arg_info: "lambda list, actually.."):
        return range(len(arg_info[0]))

def compute_applicable_methods_using_types(generic_function, types_):
        definite_p, possibly_applicable_methods = t, []
        # Not safe against method list modifications by another thread!
        for method in generic_function_methods(generic_function):
                specls = method_specializers(method) # Was: if (consp method)
                types = list(types_)
                possibly_applicable_p, applicable_p = t, t
                for specl in specls:
                        (_,
                         specl_applicable_p,
                         specl_possibly_applicable_p) = specializer_applicable_using_type_p(specl, pop(types))
                        if not specl_applicable_p:
                                applicable_p = nil
                        if not specl_possibly_applicable_p:
                                possibly_applicable_p = nil
                                break
                if possibly_applicable_p:
                        if not applicable_p: definite_p = nil
                        possibly_applicable_methods[0:0] = [method]
                nreq, applyp, metatypes, nkeys, arg_info = get_generic_fun_info(generic_function)
                # (declare (ignore nreq applyp metatypes nkeys))
                precedence = arg_info_precedence(arg_info)
                return values(sort_applicable_methods(precedence,
                                                      reversed(possibly_applicable_methods),
                                                      types),
                              definite_p)

def type_from_specializer(specl):
        if specl is t:
                return t
        elif isinstance(specl, tuple):
                if not member(car(specl), [class_, class_eq, eql]): # protoype_
                        error("%s is not a legal specializer type.", specl)
                return specl
        elif specializerp(specl): # Was a little bit more involved.
                return specializer_type(specl)
        else:
                error("%s is neither a type nor a specializer.", specl)

def specializer_applicable_using_type_p(specl, type):
        specl = type_from_specializer(specl)
        if specl is t:
                return values(t, t)
        ## This is used by C-A-M-U-T and GENERATE-DISCRIMINATION-NET-INTERNAL,
        ## and has only what they need.
        return ((nil, t) if atom(type) or car(type) is t else
                poor_man_case(car(type),
                               # (and    (saut-and specl type)),
                               # (not    (saut-not specl type)),
                               # (class_,     saut_class(specl, type)),
                               # (prototype  (saut-prototype specl type)),
                               (class_eq,   lambda: saut_class_eq(specl, type)),
                               # (class-eq   (saut-class-eq specl type)),
                               # (eql    (saut-eql specl type)),
                               (t,       lambda: error("%s cannot handle the second argument %s.",
                                                       "specializer-applicable-using-type-p",
                                                       type))))

def saut_class_eq(specl, type):
       if car(specl) is eql:
               return (nil, type_of(specl[1]) is type[1])
       else:
               pred = poor_man_case(car(specl),
                                     (class_eq, lambda: specl[1] is type[1]),
                                     (class_,   lambda: (specl[1] is type[1] or
                                                         memq(specl[1], cpl_or_nil(type[1])))))
               return (pred, pred)

def sort_applicable_methods(precedence, methods, types):
        def sorter(class1, class2, index):
                class_ = types[index] # Was: (type-class (nth index types))
                cpl = class_.__mro__  # Was: ..dependent on boot state
                return (class1 if memq(class2, memq(class1, cpl)) else # XXX: our MEMQ is horribly inefficient!
                        class2)
        return sort_methods(methods,
                             precedence,
                             sorter)

def sort_methods(methods, precedence, compare_classes_function):
        def sorter(method1, method2):
                for index in precedence:
                        specl1 = nth(index, method_specializers(method1)) # XXX: Was (if (listp method1) ..)
                        specl2 = nth(index, method_specializers(method2)) # XXX: Was (if (listp method2) ..)
                        order  = order_specializers(specl1, specl2, index, compare_classes_function)
                        if order:
                                return order is specl1
        return stable_sort(methods, sorter)

def order_specializers(specl1, specl2, index, compare_classes_function):
        type1 = specializer_type(specl1) # Was: (if (eq **boot-state** 'complete) ..)
        type2 = specializer_type(specl2) # Was: (if (eq **boot-state** 'complete) ..)
        return ([]     if specl1 is specl1 else
                specl2 if atom(type1)      else # is t?
                specl1 if atom(type2)      else # is t?
                poor_man_case(car(type1),
                               (type, lambda: case(car(type2),
                                                       (type, compare_classes_function(specl1, specl2, index)),
                                                       (t, specl2))),
                     # (prototype (case (car type2)
                     #             (class (funcall compare-classes-function
                     #                             specl1 specl2 index))
                     #             (t specl2)))
                     # (class-eq (case (car type2)
                     #             (eql specl2)
                     #             ;; FIXME: This says that all CLASS-EQ
                     #             ;; specializers are equally specific, which
                     #             ;; is fair enough because only one CLASS-EQ
                     #             ;; specializer can ever be appliable.  If
                     #             ;; ORDER-SPECIALIZERS should only ever be
                     #             ;; called on specializers from applicable
                     #             ;; methods, we could replace this with a BUG.
                     #             (class-eq nil)
                     #             (class type1)))
                     (eql,  lambda: poor_man_case(car(type2),
                                                   # similarly
                                                   (eql, []),
                                                   (t, specl1)))))

def compute_applicable_methods(generic_function, arguments):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The ARGUMENTS argument is a list of objects.

Values: This generic function returns a possibly empty list of method
metaobjects.

Purpose: This generic function determines the method applicability of
a generic function given a list of required ARGUMENTS. The returned
list of method metaobjects is sorted by precedence order with the most
specific method appearing first. If no methods are applicable to the
supplied arguments the empty list is returned.

When a generic function is invoked, the discriminating function must
determine the ordered list of methods applicable to the
arguments. Depending on the generic function and the arguments, this
is done in one of three ways: using a memoized value; calling
COMPUTE-APPLICABLE-METHODS-USING-CLASSES; or calling
COMPUTE-APPLICABLE-METHODS. (Refer to the description of
COMPUTE-DISCRIMINATING-FUNCTION for the details of this process.)

The arguments argument is permitted to contain more elements than the
generic function accepts required arguments; in these cases the extra
arguments will be ignored. An error is signaled if arguments contains
fewer elements than the generic function accepts required arguments.

The list returned by this generic function will not be mutated by the
implementation. The results are undefined if a portable program
mutates the list returned by this generic function."""
        return values_project(0, compute_applicable_methods_using_types(generic_function,
                                                                        types_from_args(generic_function,
                                                                                        arguments,
                                                                                        eql)))

def error_need_at_least_n_args(function, n):
        error("The function %s requires at least %d arguments.", function, n)

__sealed_classes__ = set([object,
                          integer_t, boolean_t, float_t, complex_t,
                          string_t,
                          hash_table_t,
                          function_t,
                          stream_t,
                          pytuple_t, pybytes_t, pylist_t, pybytearray_t, pyset_t, pyfrozenset_t,
                          BaseException, Exception] +
                         [ type_of(x)
                           for x in [None,            # NoneType
                                     Ellipsis,        # ellipsis
                                     NotImplemented,  # NotImplementedType
                                     integer_t,       # type
                                     "".find,         # builtin_function_or_method
                                     ast,             # module
                                     sys.stdin,       # _io.TextIOWrapper
                                     car.__code__,    # code object
                                     py.this_frame(), # frame
                                     ] ])

def class_sealed_p(x):
        return x in __sealed_classes__

def compute_discriminating_function(generic_function):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

Values: The value returned by this generic function is a function.

Purpose: This generic function is called to determine the
discriminating function for a generic function. When a generic
function is called, the installed discriminating function is called
with the full set of arguments received by the generic function, and
must implement the behavior of calling the generic function:
determining the ordered set of applicable methods, determining the
effective method, and running the effective method.

To determine the ordered set of applicable methods, the discriminating
function first calls COMPUTE-APPLICABLE-METHODS-USING-CLASSES. If
COMPUTE-APPLICABLE-METHODS-USING-CLASSES returns a second value of
false, the discriminating function then calls
COMPUTE-APPLICABLE-METHODS.

When COMPUTE-APPLICABLE-METHODS-USING-CLASSES returns a second value
of true, the discriminating function is permitted to memoize the first
returned value as follows. The discriminating function may reuse the
list of applicable methods without calling
COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:

    (i) the generic function is being called again with required
        arguments which are instances of the same classes,
    (ii) the generic function has not been reinitialized,
    (iii) no method has been added to or removed from the
          generic function,
    (iv) for all the specializers of all the generic function's
         methods which are classes, their class precedence lists
         have not changed and
    (v) for any such memoized value, the class precedence list of
        the class of each of the required arguments has not changed.

Determination of the effective method is done by calling
COMPUTE-EFFECTIVE-METHOD. When the effective method is run, each
method's function is called, and receives as arguments: (i) a list of
the arguments to the generic function, and (ii) whatever other
arguments are specified in the call-method form indicating that the
method should be called. (See MAKE-METHOD-LAMBDA for more information
about how method functions are called.)

The generic function COMPUTE-DISCRIMINATING-FUNCTION is called, and
its result installed, by ADD-METHOD, REMOVE-METHOD,
INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE."""
        (function_name,
         lambda_list,
         applicable_method_cache,
         # filename,
         # lineno
         ) = (generic_function.name,
                    generic_function.lambda_list,
                    generic_function.__applicable_method_cache__,
                    # generic_function.__code__.co_filename,
                    # generic_function.__code__.co_lineno
              )
        if not function_name:
                error("In COMPUTE-DISCRIMINATING-FUNCTION: name is %s.", function_name)
        fixed, optional, args, keyword, keys = lambda_list
        nfixed = len(fixed)
        def dfun_compute_applicable_methods(generic_function, args):
                if len(args) < nfixed:
                        error_need_at_least_n_args(function_name, nfixed)
                dispatch_args      = args[:nfixed]
                dispatch_arg_types = tuple(type(x) for x in dispatch_args)
                # The discriminating function may reuse the
                # list of applicable methods without calling
                # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
                # (iv) for all the specializers of all the generic function's
                #      methods which are classes, their class precedence lists
                #      have not changed and
                # XXX: not_implemented()
                # Issue COMPUTE-DISCRIMINATING-FUNCTION-REQUIREMENT-4-UNCLEAR-NOT-IMPLEMENTED
                # (v) for any such memoized value, the class precedence list of
                #     the class of each of the required arguments has not changed.
                unsealed_classes = set(x for x in dispatch_arg_types if not class_sealed_p(x))
                applicable_method_cache_key = dispatch_arg_types + reduce(lambda acc, x: acc + x.__mro__,
                                                                          sorted(unsealed_classes, key = lambda type: type.__name__),
                                                                          ())
                # We ought to pay the high price of (iv) and (v), because we can't hook
                # into the Python's object system.
                applicable, hit = gethash(applicable_method_cache_key, applicable_method_cache)
                if hit:
                        # The discriminating function may reuse the
                        # list of applicable methods without calling
                        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
                        # (i) the generic function is being called again with required
                        #     arguments which are instances of the same classes,
                        return applicable
                py.here("gf: %s, ll: %s", generic_function, generic_function.lambda_list)
                _, methods, okayp = compute_applicable_methods_using_classes(generic_function,
                                                                             dispatch_arg_types)
                if okayp:
                        applicable_method_cache[applicable_method_cache_key] = methods
                        return methods
                else:
                        return compute_applicable_methods(generic_function,
                                                          dispatch_args)
        ## compute_discriminating_function(generic_function, function_name, lambda_list,
        ##                                 fixed, optional, args, keyword, keys,
        ##                                 nfixed):
        new_dfun_ast = ast_functiondef(
            symbol_name(function_name),
            lambda_list,
            # How do we access methods themselves?
            [_ast_return(
                 ast_funcall(ast_funcall("compute_effective_method",
                                           [_ast_name(symbol_name(function_name)),
                                            None, # method combination
                                            ast_funcall("dfun_compute_applicable_methods",
                                                         [_ast_name(symbol_name(function_name)),
                                                          [ ast_name(x) for x in fixed ]])]),
                              [ ast_name(x) for x in fixed + [ x[0] for x in optional ] ],
                              map_into_hash_star(lambda key, default: (key, ast_name(default)),
                                                   keyword),
                              starargs = ast_name(args) if args else None,
                              kwargs   = ast_name(keys) if keys else None))])
        if t:
                import more_ast # Shall we concede, and import it all?
                format(t, "; generic function '%s':\n%s",
                       function_name, more_ast.pp_ast_as_code(new_dfun_ast))
        env = dict(compute_effective_method    = compute_effective_method,
                       find_symbol_or_fail            = find_symbol_or_fail,
                       dfun_compute_applicable_methods = dfun_compute_applicable_methods)
        return ast_compiled_name(
                    symbol_name(function_name),
                    new_dfun_ast,
                    filename = "" # defaulted(filename, "")
                    ,
                    lineno   = 0 # lineno
                    ,
                    function = function_name(function_name),
                    globals  = env,
                    locals   = env)

# ENSURE-GENERIC-FUNCTION

def ensure_generic_function_using_class(generic_function, function_name,
                                        argument_precedence_order = None,
                                        declarations = None,
                                        documentation = None,
                                        generic_function_class = standard_generic_function_t,
                                        lambda_list = None,
                                        method_class = None,
                                        method_combination = None,
                                        name = nil,
                                        # incompatible..
                                        globals = None,
                                        filename = None,
                                        lineno = None,
                                        **keys):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject or NIL.

The FUNCTION-NAME argument is a symbol or a list of the form (SETF
SYMBOL).

The :GENERIC-FUNCTION-CLASS argument is a class metaobject or a class
name. If it is not supplied, it defaults to the class named
STANDARD-GENERIC-FUNCTION. If a class name is supplied, it is
interpreted as the class with that name. If a class name is supplied,
but there is no such class, an error is signaled.

For the interpretation of additional keyword arguments, see
``Initialization of Generic Function Metaobjects''.

Values: The result is a generic function metaobject.

Purpose: The generic function ENSURE-GENERIC-FUNCTION-USING-CLASS is
called to define or modify the definition of a globally named generic
function. It is called by the ENSURE-GENERIC-FUNCTION function. It can
also be called directly.

The first step performed by this generic function is to compute the
set of initialization arguments which will be used to create or
reinitialize the globally named generic function. These initialization
arguments are computed from the full set of keyword arguments received
by this generic function as follows:

    The :GENERIC-FUNCTION-CLASS argument is not included in the
    initialization arguments.

    If the :METHOD-CLASS argument was received by this generic
    function, it is converted into a class metaobject. This is done by
    looking up the class name with FIND-CLASS. If there is no such
    class, an error is signalled.

    All other keyword arguments are included directly in the
    initialization arguments.

If the GENERIC-FUNCTION argument is NIL, an instance of the class
specified by the :GENERIC-FUNCTION-CLASS argument is created by
calling MAKE-INSTANCE with the previously computed initialization
arguments. The function name FUNCTION-NAME is set to name the generic
function. The newly created generic function metaobject is returned.

If the class of the GENERIC-FUNCTION argument is not the same as the
class specified by the :GENERIC-FUNCTION-CLASS argument, an error is
signaled.

Otherwise the generic function GENERIC-FUNCTION is redefined by
calling the REINITIALIZE-INSTANCE generic function with
GENERIC-FUNCTION and the initialization arguments. The
GENERIC-FUNCTION argument is then returned."""
        # Unregistered Issue COMPLIANCE-SETF-LIST-NAMES-NOT-SUPPORTED
        # Unregistered Issue COMPLIANCE-GENERIC-FUNCTION-CLASS-AS-NAME-NOT-SUPPORTED
        # Unregistered Issue COMPLIANCE-GENERIC-FUNCTION-REINITIALIZE-INSTANCE-SURROGATE-CALLED
        ###
        ### First step, "compute the set of initialization arguments":
        if generic_function:
                # DEFGENERIC (CLHS) documentation speaks so about method removal/addition:
                ## The effect of the DEFGENERIC macro is as if the following three steps
                ## were performed: first, methods defined by previous DEFGENERIC forms
                ## are removed; second, ENSURE-GENERIC-FUNCTION is called; and finally,
                ## methods specified by the current DEFGENERIC form are added to the
                ## generic function.
                ## /citation
                # ..however, in the documentation of ENSURE-GENERIC-FUNCTION (AMOP):
                ## The behavior of this function is actually implemented by the generic
                ## function ENSURE-GENERIC-FUNCTION-USING-CLASS. When ENSURE-GENERIC-FUNCTION
                ## is called, it immediately calls ENSURE-GENERIC-FUNCTION-USING-CLASS and
                ## returns that result as its own.
                # ..and so, we decide that AMOP trumps CLHS.
                mapc(curry(remove_method, generic_function),
                     generic_function_methods(generic_function))
        if lambda_list:
                fixed, optional, args, keyword, kwarg = lambda_list
                if any(x[1] is not None for x in list(optional) + list(keyword)):
                        error("Generic function arglist cannot specify default parameter values.")
        initargs = only_specified_keys(
                argument_precedence_order = argument_precedence_order,
                declarations              = declarations,
                documentation             = documentation,
                lambda_list               = lambda_list,
                method_class              = method_class,
                method_combination        = method_combination,
                name                      = function_name, # name # Issue RESEARCH-COMPLIANCE-ENSURE-GENERIC-FUNCTION-USING-CLASS-NAME-ARGUMENT
                # incompatible..
                filename                  = filename,
                lineno                    = lineno)
        initargs.update(keys)
        py.here("args: %s", initargs)
        ###
        ### Second step:
        if not generic_function:
                # If the GENERIC-FUNCTION argument is NIL, an instance of the class
                # specified by the :GENERIC-FUNCTION-CLASS argument is created by
                # calling MAKE-INSTANCE with the previously computed initialization arguments.
                # The function name FUNCTION-NAME is set to name the generic function.
                generic_function = generic_function_class_t(**initargs)
                # standard_generic_function_shared_initialize is called by s-g-f.__init__
                frost.setf_global(generic_function, function_name, globals = defaulted(globals, pyb.globals()))
        else:
                if class_of(generic_function) is not generic_function_class:
                        # If the class of the GENERIC-FUNCTION argument is not the same as the
                        # class specified by the :GENERIC-FUNCTION-CLASS argument, an error is
                        # signaled.
                        error("ENSURE-GENERIC-FUNCTION-USING-CLASS: ")
                # Otherwise the generic function GENERIC-FUNCTION is redefined by
                # calling the REINITIALIZE-INSTANCE generic function with
                # GENERIC-FUNCTION and the initialization arguments. The
                # GENERIC-FUNCTION argument is then returned.
                # reinitialize_instance(generic_function, **initargs) # does not do much, beyond __dict__ update
                standard_generic_function_shared_initialize(generic_function, **initargs)
        return generic_function

def ensure_generic_function(function_name, globals = None, **keys):
        """Arguments:

The FUNCTION-NAME argument is a symbol or a list of the form (SETF
SYMBOL).

Some of the keyword arguments accepted by this function are actually
processed by ENSURE-GENERIC-FUNCTION-USING-CLASS, others are processed
during initialization of the generic function metaobject (as described
in the section called ``Initialization of Generic Function
Metaobjects'').

Values: The result is a generic function metaobject.

Purpose: This function is called to define a globally named generic
function or to specify or modify options and declarations that pertain
to a globally named generic function as a whole. It can be called by
the user or the implementation.

It is the functional equivalent of DEFGENERIC, and is called by the
expansion of the DEFGENERIC and DEFMETHOD macros.

The behavior of this function is actually implemented by the generic
function ENSURE-GENERIC-FUNCTION-USING-CLASS. When
ENSURE-GENERIC-FUNCTION is called, it immediately calls
ENSURE-GENERIC-FUNCTION-USING-CLASS and returns that result as its
own.

The first argument to ENSURE-GENERIC-FUNCTION-USING-CLASS is computed
as follows:

    If FUNCTION-NAME names a non-generic function, a macro, or a
    special form, an error is signaled.

    If FUNCTION-NAME names a generic function, that generic function
    metaobject is used.

    Otherwise, NIL is used.

The second argument is FUNCTION-NAME. The remaining arguments are the
complete set of keyword arguments received by
ENSURE-GENERIC-FUNCTION."""
        maybe_gfun = defaulted(global_(the(symbol_t, function_name),
                                       defaulted(globals, pyb.globals())),
                                nil)
        if functionp(maybe_gfun) and not generic_function_p(maybe_gfun):
                error("%s already names an ordinary function.", function_name)
        return ensure_generic_function_using_class(maybe_gfun, function_name, **keys)

def defgeneric(_ = None,
               argument_precedence_order = None,
               documentation = None,
               method_combination = standard_method_combination_t,
               generic_function_class = standard_generic_function_t,
               method_class = standard_method_t):
# Unregistered Issue: COMPLIANCE-DEFGENERIC-METHOD-DESCRIPTIONS-UNIMPLEMENTABLE
        """defgeneric function-name gf-lambda-list [[option | {method-description}*]]

=> new-generic

option::= (:argument-precedence-order parameter-name+) |
          (declare gf-declaration+) |
          (:documentation gf-documentation) |
          (:method-combination method-combination method-combination-argument*) |
          (:generic-function-class generic-function-class) |
          (:method-class method-class)

method-description::= (:method method-qualifier* specialized-lambda-list [[declaration* | documentation]] form*)

Arguments and Values:

FUNCTION-NAME---a function name.

GENERIC-FUNCTION-CLASS---a non-NIL symbol naming a class.

GF-DECLARATION---an optimize declaration specifier; other declaration specifiers are not permitted.

GF-DOCUMENTATION---a string; not evaluated.

GF-LAMBDA-LIST---a generic function lambda list.

METHOD-CLASS---a non-NIL symbol naming a class.

METHOD-COMBINATION-ARGUMENT---an object.

METHOD-COMBINATION-NAME---a symbol naming a method combination type.

METHOD-QUALIFIERS, SPECIALIZED-LAMBDA-LIST, DECLARATIONS, DOCUMENTATION, FORMS---as per DEFMETHOD.

NEW-GENERIC---the generic function object.

PARAMETER-NAME---a symbol that names a required parameter in the
LAMBDA-LIST. (If the :ARGUMENT-PRECEDENCE-ORDER option is specified,
each required parameter in the LAMBDA-LIST must be used exactly once
as a PARAMETER-NAME.)

Description:

The macro DEFGENERIC is used to define a generic function or to
specify options and declarations that pertain to a generic function as
a whole.

If FUNCTION-NAME is a list it must be of the form (SETF SYMBOL). If
(FBOUNDP FUNCTION-NAME) is false, a new generic function is
created. If (FDEFINITION FUNCTION-NAME) is a generic function, that
generic function is modified. If FUNCTION-NAME names an ordinary
function, a macro, or a special operator, an error is signaled.

The effect of the DEFGENERIC macro is as if the following three steps
were performed: first, methods defined by previous DEFGENERIC forms
are removed; second, ENSURE-GENERIC-FUNCTION is called; and finally,
methods specified by the current DEFGENERIC form are added to the
generic function.

Each METHOD-DESCRIPTION defines a method on the generic function. The
lambda list of each method must be congruent with the lambda list
specified by the GF-LAMBDA-LIST option. If no method descriptions are
specified and a generic function of the same name does not already
exist, a generic function with no methods is created.

The GF-LAMBDA-LIST argument of defgeneric specifies the shape of
lambda lists for the methods on this generic function. All methods on
the resulting generic function must have lambda lists that are
congruent with this shape. If a DEFGENERIC form is evaluated and some
methods for that generic function have lambda lists that are not
congruent with that given in the DEFGENERIC form, an error is
signaled. For further details on method congruence, see Section 7.6.4
(Congruent Lambda-lists for all Methods of a Generic Function).

The generic function passes to the method all the argument values
passed to it, and only those; default values are not supported. Note
that optional and keyword arguments in method definitions, however,
can have default initial value forms and can use SUPPLIED-P
parameters.

The following options are provided. Except as otherwise noted, a given
option may occur only once.

    The :argument-precedence-order option is used to specify the order
    in which the required arguments in a call to the generic function
    are tested for specificity when selecting a particular
    method. Each required argument, as specified in the gf-lambda-list
    argument, must be included exactly once as a parameter-name so
    that the full and unambiguous precedence order is supplied. If
    this condition is not met, an error is signaled.

    The declare option is used to specify declarations that pertain to
    the generic function.

    An optimize declaration specifier is allowed. It specifies whether
    method selection should be optimized for speed or space, but it
    has no effect on methods. To control how a method is optimized, an
    optimize declaration must be placed directly in the defmethod form
    or method description. The optimization qualities speed and space
    are the only qualities this standard requires, but an
    implementation can extend the object system to recognize other
    qualities. A simple implementation that has only one method
    selection technique and ignores optimize declaration specifiers is
    valid.

    The special, ftype, function, inline, notinline, and declaration
    declarations are not permitted. Individual implementations can
    extend the declare option to support additional declarations. If
    an implementation notices a declaration specifier that it does not
    support and that has not been proclaimed as a non-standard
    declaration identifier name in a declaration proclamation, it
    should issue a warning.

    The declare option may be specified more than once. The effect is
    the same as if the lists of declaration specifiers had been
    appended together into a single list and specified as a single
    declare option.

    The :documentation argument is a documentation string to be
    attached to the generic function object, and to be attached with
    kind function to the function-name.

    The :generic-function-class option may be used to specify that the
    generic function is to have a different class than the default
    provided by the system (the class standard-generic-function). The
    class-name argument is the name of a class that can be the class
    of a generic function. If function-name specifies an existing
    generic function that has a different value for the
    :generic-function-class argument and the new generic function
    class is compatible with the old, change-class is called to change
    the class of the generic function; otherwise an error is signaled.

    The :method-class option is used to specify that all methods on
    this generic function are to have a different class from the
    default provided by the system (the class standard-method). The
    class-name argument is the name of a class that is capable of
    being the class of a method.

    The :method-combination option is followed by a symbol that names
    a type of method combination. The arguments (if any) that follow
    that symbol depend on the type of method combination. Note that
    the standard method combination type does not support any
    arguments. However, all types of method combination defined by the
    short form of define-method-combination accept an optional
    argument named order, defaulting to :most-specific-first, where a
    value of :most-specific-last reverses the order of the primary
    methods without affecting the order of the auxiliary methods.

The method-description arguments define methods that will be
associated with the generic function. The method-qualifier and
specialized-lambda-list arguments in a method description are the same
as for defmethod.

The form arguments specify the method body. The body of the method is
enclosed in an implicit block. If function-name is a symbol, this
block bears the same name as the generic function. If function-name is
a list of the form (setf symbol), the name of the block is symbol.

Implementations can extend defgeneric to include other options. It is
required that an implementation signal an error if it observes an
option that is not implemented locally.

defgeneric is not required to perform any compile-time side
effects. In particular, the methods are not installed for invocation
during compilation. An implementation may choose to store information
about the generic function for the purposes of compile-time
error-checking (such as checking the number of arguments on calls, or
noting that a definition for the function name has been seen)."""
        if _ is not None:
                error("DEFGENERIC must be used be as a decorator call.")
                # The rationale is that the gfun arglist is precious, and
                # the decorator is the only place to have a sane arglist.
        if documentation is not None:
                error("DEFGENERIC :DOCUMENTATION is provided through the docstring instead.")
        def do_defgeneric(fn):
                # option::= (:argument-precedence-order parameter-name+) |
                #           (declare gf-declaration+) |
                #           (:documentation gf-documentation) |
                #           (:method-combination method-combination method-combination-argument*) |
                #           (:generic-function-class generic-function-class) |
                #           (:method-class method-class)
                _, sym, __ = interpret_toplevel_value(fn, functionp)
                return ensure_generic_function(sym,
                                               argument_precedence_order = argument_precedence_order,
                                               documentation             = fn.__doc__,
                                               method_combination        = method_combination,
                                               generic_function_class    = generic_function_class,
                                               lambda_list               = function_lambda_list(fn),
                                               method_class              = method_class,
                                               #
                                               filename      = fn.__code__.co_filename,
                                               lineno        = fn.__code__.co_firstlineno)
        return do_defgeneric

def method_agrees_with_qualifiers_specializers(method, qualifiers, specializers):
        """7.6.3 Agreement on Parameter Specializers and Qualifiers

Two methods are said to agree with each other on parameter
specializers and qualifiers if the following conditions hold:

1. Both methods have the same number of required parameters. Suppose
the parameter specializers of the two methods are P1,1...P1,n and
P2,1...P2,n.

2. For each 1<=i<=n, P1,i agrees with P2,i. The parameter specializer
P1,i agrees with P2,i if P1,i and P2,i are the same class or if
P1,i=(eql object1), P2,i=(eql object2), and (eql object1
object2). Otherwise P1,i and P2,i do not agree.

3. The two lists of qualifiers are the same under equal."""
        lambda_list = method_lambda_list(method)
        return (len(lambda_list[0]) == len(specializers)
                and all(((ms is s)
                         or (listp(ms) and listp(s)
                             and len(ms) == len(s) == 2
                             and ms[0] == s[0] == eql
                             and eql(ms[1], s[1])))
                        for m, ms in
                        zip(method_specializers(method), specializers))
                and method_qualifiers(method) == qualifiers)

def generic_function_lambda_list_incongruent_with_method_list_p(generic_function_lambda_list,
                                                                 method_lambda_list):
        """7.6.4 Congruent Lambda-lists for all Methods of a Generic Function

These rules define the congruence of a set of lambda lists, including
the lambda list of each method for a given generic function and the
lambda list specified for the generic function itself, if given.

1. Each lambda list must have the same number of required parameters.

2. Each lambda list must have the same number of optional
parameters. Each method can supply its own default for an optional
parameter.

3. If any lambda list mentions &rest or &key, each lambda list must
mention one or both of them.

4. If the generic function lambda list mentions &key, each method must
accept all of the keyword names mentioned after &key, either by
accepting them explicitly, by specifying &allow-other-keys, or by
specifying &rest but not &key. Each method can accept additional
keyword arguments of its own. The checking of the validity of keyword
names is done in the generic function, not in each method. A method is
invoked as if the keyword argument pair whose name is
:allow-other-keys and whose value is true were supplied, though no
such argument pair will be passed.

5. The use of &allow-other-keys need not be consistent across lambda
lists. If &allow-other-keys is mentioned in the lambda list of any
applicable method or of the generic function, any keyword arguments
may be mentioned in the call to the generic function.

6. The use of &aux need not be consistent across methods.

If a method-defining operator that cannot specify generic function
options creates a generic function, and if the lambda list for the
method mentions keyword arguments, the lambda list of the generic
function will mention &key (but no keyword arguments)."""
# Unregistered Issue COMPLIANCE-SPEC-UNCLEAR-LAST-PASSAGE-LAMBDA-LIST-CONGRUENCE
        gf_fixed, gf_optional, gf_args, gf_keyword, gf_keys = generic_function_lambda_list
        m_fixed,  m_optional,  m_args,  m_keyword,  m_keys  = method_lambda_list
        return ((len(gf_fixed)    != len(m_fixed) and
                 "the method has %s required arguments than the generic function" %
                 ("more" if len(m_fixed) > len(gf_fixed) else "less"))                                or
                (len(gf_optional) != len(m_optional) and
                 "the method has %s optional arguments than the generic function" %
                 ("more" if len(m_fixed) > len(gf_fixed) else "less"))                                or
                (xorf(gf_args, m_args) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                # XXX: #3 compliance -- still looks fishy
                (xorf(gf_keyword or gf_keys,
                       m_keyword  or m_keys) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                (((not gf_keyword) or
                  m_keys           or
                  not (set(gf_keyword) - set(m_keyword))) and
                 "but the method does not accept each of the &KEY arguments %s" % tuple([gf_keyword])))

# Method addition and removal

def add_method(generic_function, method):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The METHOD argument is a method metaobject.

Values: The GENERIC-FUNCTION argument is returned.

Purpose: This generic function associates an unattached method with a
generic function.

An error is signaled if the lambda list of the method is not congruent
with the lambda list of the generic function. An error is also
signaled if the method is already associated with some other generic
function.

If the given method agrees with an existing method of the generic
function on parameter specializers and qualifiers, the existing method
is removed by calling REMOVE-METHOD before the new method is
added. See the section of the CLOS Specification called ``Agreement on
Parameter Specializers and Qualifiers'' for a definition of agreement
in this context.

Associating the method with the generic function then proceeds in four
steps: (i) add METHOD to the set returned by GENERIC-FUNCTION-METHODS
and arrange for METHOD-GENERIC-FUNCTION to return
GENERIC-FUNCTION; (ii) call ADD-DIRECT-METHOD for each of the method's
specializers; (iii) call COMPUTE-DISCRIMINATING-FUNCTION and install
its result with SET-FUNCALLABLE-INSTANCE-FUNCTION; and (iv) update the
dependents of the generic function.

The generic function ADD-METHOD can be called by the user or the
implementation."""
        # Unregistered Issue COMPLIANCE-UNCLEAR-METHOD-LAMBDA-LIST-SPECIALIZERS-INCLUSION
        congruence_error = generic_function_lambda_list_incongruent_with_method_list_p(
                generic_function_lambda_list(generic_function),
                method_lambda_list(method))
        if congruence_error:
                error("attempt to add the method %s to the generic function %s; but %s.",
                      method, generic_function, congruence_error)
        if slot_boundp(method, "__generic_function__") and method.__generic_function__:
                error("ADD-METHOD called to add %s, when it was already attached to %s.",
                      method, method.__generic_function__)
        old_method = [ m for m in generic_function_methods(generic_function)
                       if method_agrees_with_qualifiers_specializers(m,
                                                                      method_qualifiers(method),
                                                                      method_specializers(method)) ]
        if old_method:
                remove_method(generic_function, old_method[0])
        generic_function.__methods__[method.specializers] = method
        method.__generic_function__ = generic_function
        for s in method_specializers(method):
                add_direct_method(s, method)
        update_generic_function_and_dependents(generic_function, add_method = method)
        return generic_function

def set_funcallable_instance_function(funcallable_instance, function):
        """set-funcallable-instance-function funcallable-instance function

Arguments:

The FUNCALLABLE-INSTANCE argument is a funcallable instance (it must
have been returned by ALLOCATE-INSTANCE (FUNCALLABLE-STANDARD-CLASS)).

The FUNCTION argument is a function.

Values:

The value returned by this function is unspecified.

Purpose:

This function is called to set or to change the function of a
funcallable instance. After SET-FUNCALLABLE-INSTANCE-FUNCTION is
called, any subsequent calls to FUNCALLABLE-INSTANCE will run the new
FUNCTION."""
        # XXX: better to:
        # 1. override __call__ with a properly-arglisted thing
        # 2. pass through __code__ and maybe others
        funcallable_instance.function = function

def add_direct_method(specializer, method):
        """This generic function is called to maintain a set of
backpointers from a SPECIALIZER to the set of methods specialized to
it. If METHOD is already in the set, it is not added again (no error
is signaled).

This set can be accessed as a list by calling the generic function
SPECIALIZER-DIRECT-METHODS. Methods are removed from the set by
REMOVE-DIRECT-METHOD.

The generic function ADD-DIRECT-METHOD is called by ADD-METHOD
whenever a method is added to a generic function. It is called once
for each of the specializers of the METHOD. Note that in cases where a
specializer appears more than once in the specializers of a METHOD,
this generic function will be called more than once with the same
specializer as argument.

The results are undefined if the SPECIALIZER argument is not one of
the specializers of the METHOD argument."""
        not_implemented("maintain a set of backpointers from a SPECIALIZER to the set of methods specialized to it")

# METHOD metamethods

def standard_method_shared_initialize(method,
                                       qualifiers = None,
                                       lambda_list = None,
                                       specializers = None,
                                       function = None,
                                       documentation = None,
                                       slot_definition = None,
                                       # extensions
                                       filename = None,
                                       lineno = None):
        """Initialization of Method Metaobjects

A method metaobject can be created by calling MAKE-INSTANCE. The
initialization arguments establish the definition of the METHOD. A
method metaobject cannot be redefined; calling REINITIALIZE-INSTANCE
signals an error.

Initialization of a METHOD metaobject must be done by calling
MAKE-INSTANCE and allowing it to call INITIALIZE-INSTANCE. Portable
programs must not call INITIALIZE-INSTANCE directly to initialize a
method metaoject. Portable programs must not call shared-initialize
directly to initialize a method metaobject. Portable programs must not
call CHANGE-CLASS to change the class of any method metaobject or to
turn a non-method object into a method metaobject.

Since metaobject classes may not be redefined, no behavior is
specified for the result of calls to
UPDATE-INSTANCE-FOR-REDEFINED-CLASS on method metaobjects. Since the
class of a method metaobject cannot be changed, no behavior is
specified for the result of calls to
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS on method metaobjects.

During initialization, each initialization argument is checked for
errors and then associated with the METHOD metaobject. The value can
then be accessed by calling the appropriate accessor as shown in Table
4.

This section begins with a description of the error checking and
processing of each initialization argument. This is followed by a
table showing the generic functions that can be used to access the
stored initialization arguments. The section ends with a set of
restrictions on portable methods affecting method metaobject
initialization.

In these descriptions, the phrase ``this argument defaults to value''
means that when that initialization argument is not supplied,
initialization is performed as if value had been supplied. For some
initialization arguments this could be done by the use of default
initialization arguments, but whether it is done this way is not
specified. Implementations are free to define default initialization
arguments for specified method metaobject classes. Portable programs
are free to define default initialization arguments for portable
subclasses of the class method.

    The :QUALIFIERS argument is a list of method qualifiers. An error
    is signaled if this value is not a proper list, or if any element
    of the list is not a non-null atom. This argument defaults to the
    empty list.

    The :LAMBDA-LIST argument is the unspecialized lambda list of the
    method. An error is signaled if this value is not a proper lambda
    list. If this value is not supplied, an error is signaled.

    The :SPECIALIZERS argument is a list of the specializer
    metaobjects for the METHOD. An error is signaled if this value is
    not a proper list, or if the length of the list differs from the
    number of required arguments in the :LAMBDA-LIST argument, or if
    any element of the list is not a specializer metaobject. If this
    value is not supplied, an error is signaled.

    The :FUNCTION argument is a method function. It must be compatible
    with the methods on COMPUTE-EFFECTIVE-METHOD defined for this
    class of method and generic function with which it will be
    used. That is, it must accept the same number of arguments as all
    uses of CALL-METHOD that will call it supply. (See
    COMPUTE-EFFECTIVE-METHOD for more information.) An error is
    signaled if this argument is not supplied.

    When the METHOD being initialized is an instance of a subclass of
    STANDARD-ACCESSOR-METHOD, the :SLOT-DEFINITION initialization
    argument must be provided. Its value is the direct slot definition
    metaobject which defines this accessor method. An error is
    signaled if the value is not an instance of a subclass of
    DIRECT-SLOT-DEFINITION.

    The :documentation argument is a string or NIL. An error is
    signaled if this value is not a string or NIL. This argument
    defaults to NIL.

After the processing and defaulting of initialization arguments
described above, the value of each initialization argument is
associated with the method metaobject. These values can then be
accessed by calling the corresponding generic function. The
correspondences are as follows:"""
        method.qualifiers = defaulted(qualifiers, [],
                                       type = (pylist_t, (and_t, symbol_t, (not_t, (eql_t, nil)))))
        if not specifiedp(lambda_list):
                error("SHARED-INITIALIZE STANDARD-METHOD: :LAMBDA-LIST must be supplied.")
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-LAMBDA-LIST-VALIDATION-NOT-IMPLEMENTED
        method.lambda_list = lambda_list
        if not specifiedp(specializers):
                error("SHARED-INITIALIZE STANDARD-METHOD: :SPECIALIZERS must be supplied.")
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-SPECIALIZER-VALIDATION-NOT-IMPLEMENTED
        #  o  (pylist, method_specializer)
        #  o  length == len(lambda_list[0])
        method.specializers = specializers
        if not specifiedp(function):
                error("SHARED-INITIALIZE STANDARD-METHOD: :FUNCTION must be supplied.")
        method.function = function
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-SLOT-DEFINITION-OPTION-NOT-IMPLEMENTED
        ## Later:
        # if typep(method, standard_accessor_method):
        #         if not specifiedp(slot_definition):
        #                 error("SHARED-INITIALIZE STANDARD-METHOD: :SLOT-DEFINITION must be supplied.")
        #         if not typep(slot_definition, direct_slot_definition):
        #                 error("SHARED-INITIALIZE STANDARD-METHOD: the supplied value of :SLOT-DEFINITION must be an instance of a subclass of DIRECT-SLOT-DEFINITION.")
        method.documentation = defaulted(documentation, nil,
                                          type = (or_t, string_t, (eql_t, nil)))
        return method

def method_qualifiers(x):       return x.qualifiers
def method_lambda_list(x):      return x.lambda_list
def method_specializers(x):     return x.specializers

def remove_method(generic_function, method):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The METHOD argument is a method metaobject.

Values: The GENERIC-FUNCTION argument is returned.

Purpose: This generic function breaks the association between a
generic function and one of its methods.

No error is signaled if the method is not among the methods of the
generic function.

Breaking the association between the method and the generic function
proceeds in four steps: (i) remove method from the set returned by
GENERIC-FUNCTION-METHODS and arrange for METHOD-GENERIC-FUNCTION to
return NIL; (ii) call REMOVE-DIRECT-METHOD for each of the method's
specializers; (iii) call COMPUTE-DISCRIMINATING-FUNCTION and install
its result with SET-FUNCALLABLE-INSTANCE-FUNCTION; and (iv) update the
dependents of the generic function.

The generic function REMOVE-METHOD can be called by the user or the
implementation."""
        del generic_function.__methods__[method.specializers]
        method.__generic_function__ = nil
        for s in method_specializers(method):
                remove_direct_method(s, method)
        update_generic_function_and_dependents(generic_function, remove_method = method)
        return generic_function

def remove_direct_method(specializer, method):
        """remove-direct-method specializer method

Arguments:

The specializer argument is a specializer metaobject.

The method argument is a method metaobject.

Values:

The value returned by remove-direct-method is unspecified.

Purpose:

This generic function is called to maintain a set of backpointers from
a SPECIALIZER to the set of methods specialized to it. If METHOD is in
the set it is removed. If it is not, no error is signaled.

This set can be accessed as a list by calling the generic function
SPECIALIZER-DIRECT-METHODS. Methods are added to the set by
ADD-DIRECT-METHOD.

The generic function REMOVE-DIRECT-METHOD is called by REMOVE-METHOD
whenever a method is removed from a generic function. It is called
once for each of the specializers of the method. Note that in cases
where a specializer appears more than once in the specializers of a
method, this generic function will be called more than once with the
same specializer as argument.

The results are undefined if the specializer argument is not one of
the specializers of the method argument."""
        not_implemented()

# DEFMETHOD

def defmethod(fn):
        """defmethod function-name {method-qualifier}* specialized-lambda-list [[declaration* | documentation]] form*

=> new-method

function-name::= {symbol | (setf symbol)}

method-qualifier::= non-list

specialized-lambda-list::= ({var | (var parameter-specializer-name)}*
                            [&optional {var | (var [initform [supplied-p-parameter] ])}*]
                            [&rest var]
                            [&key{var | ({var | (keywordvar)} [initform [supplied-p-parameter] ])}*
                                 [&allow-other-keys] ]
                            [&aux {var | (var [initform] )}*] )
parameter-specializer-name::= symbol | (eql eql-specializer-form)

Arguments and Values:

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

var---a variable name.

eql-specializer-form---a form.

Form---a form.

Initform---a form.

Supplied-p-parameter---variable name.

new-method---the new method object.

Description:

The macro DEFMETHOD defines a method on a generic function.

If (FBOUNDP FUNCTION-NAME) is NIL, a generic function is created with
default values for the argument precedence order (each argument is
more specific than the arguments to its right in the argument list),
for the generic function class (the class STANDARD-GENERIC-FUNCTION),
for the method class (the class STANDARD-METHOD), and for the method
combination type (the standard method combination type). The lambda
list of the generic function is congruent with the lambda list of the
method being defined; if the DEFMETHOD form mentions keyword
arguments, the lambda list of the generic function will mention
..... key (but no keyword arguments). If FUNCTION-NAME names an
ordinary function, a macro, or a special operator, an error is
signaled.

If a generic function is currently named by FUNCTION-NAME, the lambda
list of the method must be congruent with the lambda list of the
generic function. If this condition does not hold, an error is
signaled. For a definition of congruence in this context, see Section
7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function).

Each METHOD-QUALIFIER argument is an object that is used by method
combination to identify the given method. The method combination type
might further restrict what a method qualifier can be. The standard
method combination type allows for unqualified methods and methods
whose sole qualifier is one of the keywords :BEFORE, :AFTER,
or :AROUND.

The SPECIALIZED-LAMBDA-LIST argument is like an ordinary lambda list
except that the names of required parameters can be replaced by
specialized parameters. A specialized parameter is a list of the
form (VAR PARAMETER-SPECIALIZER-NAME). Only required parameters can be
specialized. If PARAMETER-SPECIALIZER-NAME is a symbol it names a
class; if it is a list, it is of the form (EQL
EQL-SPECIALIZER-FORM). The parameter specializer name (EQL
EQL-SPECIALIZER-FORM) indicates that the corresponding argument must
be EQL to the object that is the value of EQL-SPECIALIZER-FORM for the
method to be applicable. The EQL-SPECIALIZER-FORM is evaluated at the
time that the expansion of the DEFMETHOD macro is evaluated. If no
parameter specializer name is specified for a given required
parameter, the parameter specializer defaults to the class t. For
further discussion, see Section 7.6.2 (Introduction to Methods).

The FORM arguments specify the method body. The body of the method is
enclosed in an implicit block. If FUNCTION-NAME is a symbol, this
block bears the same name as the generic function. If FUNCTION-NAME is
a list of the form (SETF SYMBOL), the name of the block is symbol.

The class of the method object that is created is that given by the
method class option of the generic function on which the method is
defined.

If the generic function already has a method that agrees with the
method being defined on parameter specializers and qualifiers,
DEFMETHOD replaces the existing method with the one now being
defined. For a definition of agreement in this context. see Section
7.6.3 (Agreement on Parameter Specializers and Qualifiers).

The parameter specializers are derived from the parameter specializer
names as described in Section 7.6.2 (Introduction to Methods).

The expansion of the DEFMETHOD macro ``refers to'' each specialized
parameter (see the description of ignore within the description of
declare). This includes parameters that have an explicit parameter
specializer name of T. This means that a compiler warning does not
occur if the body of the method does not refer to a specialized
parameter, while a warning might occur if the body of the method does
not refer to an unspecialized parameter. For this reason, a parameter
that specializes on T is not quite synonymous with an unspecialized
parameter in this context.

Declarations at the head of the method body that apply to the method's
lambda variables are treated as bound declarations whose scope is the
same as the corresponding bindings.

Declarations at the head of the method body that apply to the
functional bindings of CALL-NEXT-METHOD or NEXT-METHOD-P apply to
references to those functions within the method body forms. Any outer
bindings of the function names CALL-NEXT-METHOD and NEXT-METHOD-P, and
declarations associated with such bindings are shadowed[2] within the
method body forms.

The scope of free declarations at the head of the method body is the
entire method body, which includes any implicit local function
definitions but excludes initialization forms for the lambda
variables.

DEFMETHOD is not required to perform any COMPILE-TIME side effects. In
particular, the methods are not installed for invocation during
compilation. An implementation may choose to store information about
the generic function for the purposes of COMPILE-TIME
error-checking (such as checking the number of arguments on calls, or
noting that a definition for the function name has been seen).

Documentation is attached as a documentation string to the method
object."""
## 7.6.2 Introduction to Methods
#
# Methods define the class-specific or identity-specific behavior and
# operations of a generic function.
#
# A method object is associated with code that implements the method's
# behavior, a sequence of parameter specializers that specify when the
# given method is applicable, a lambda list, and a sequence of
# qualifiers that are used by the method combination facility to
# distinguish among methods.
#
# A method object is not a function and cannot be invoked as a
# function. Various mechanisms in the object system take a method
# object and invoke its method function, as is the case when a generic
# function is invoked. When this occurs it is said that the method is
# invoked or called.
#
# A method-defining form contains the code that is to be run when the
# arguments to the generic function cause the method that it defines
# to be invoked. When a method-defining form is evaluated, a method
# object is created and one of four actions is taken:
#
# * If a generic function of the given name already exists and if a
#   method object already exists that agrees with the new one on
#   parameter specializers and qualifiers, the new method object
#   replaces the old one. For a definition of one method agreeing with
#   another on parameter specializers and qualifiers, see Section
#   7.6.3 (Agreement on Parameter Specializers and Qualifiers).
#
# * If a generic function of the given name already exists and if
#   there is no method object that agrees with the new one on
#   parameter specializers and qualifiers, the existing generic
#   function object is modified to contain the new method object.
#
# * If the given name names an ordinary function, a macro, or a
#   special operator, an error is signaled.
#
# * Otherwise a generic function is created with the method specified
#   by the method-defining form.
#
# If the lambda list of a new method is not congruent with the lambda
# list of the generic function, an error is signaled. If a
# method-defining operator that cannot specify generic function
# options creates a new generic function, a lambda list for that
# generic function is derived from the lambda list of the method in
# the method-defining form in such a way as to be congruent with
# it. For a discussion of congruence, see Section 7.6.4 (Congruent
# Lambda-lists for all Methods of a Generic Function).
#
# Each method has a specialized lambda list, which determines when
# that method can be applied. A specialized lambda list is like an
# ordinary lambda list except that a specialized parameter may occur
# instead of the name of a required parameter. A specialized parameter
# is a list (variable-name parameter-specializer-name), where
# parameter-specializer-name is one of the following:
#
# a symbol
#
#     denotes a parameter specializer which is the class named by that
#     symbol.
#
# a class
#
#     denotes a parameter specializer which is the class itself.
#
# (eql form)
#
#     denotes a parameter specializer which satisfies the type
#     specifier (eql object), where object is the result of evaluating
#     form. The form form is evaluated in the lexical environment in
#     which the method-defining form is evaluated. Note that form is
#     evaluated only once, at the time the method is defined, not each
#     time the generic function is called.
#
# Parameter specializer names are used in macros intended as the
# user-level interface (defmethod), while parameter specializers are
# used in the functional interface.
#
# Only required parameters may be specialized, and there must be a
# parameter specializer for each required parameter. For notational
# simplicity, if some required parameter in a specialized lambda list
# in a method-defining form is simply a variable name, its parameter
# specializer defaults to the class t.
#
# Given a generic function and a set of arguments, an applicable
# method is a method for that generic function whose parameter
# specializers are satisfied by their corresponding arguments. The
# following definition specifies what it means for a method to be
# applicable and for an argument to satisfy a parameter specializer.
#
# Let <A1, ..., An> be the required arguments to a generic function in
# order. Let <P1, ..., Pn> be the parameter specializers corresponding
# to the required parameters of the method M in order. The method M is
# applicable when each Ai is of the type specified by the type
# specifier Pi. Because every valid parameter specializer is also a
# valid type specifier, the function typep can be used during method
# selection to determine whether an argument satisfies a parameter
# specializer.
#
# A method all of whose parameter specializers are the class t is
# called a default method; it is always applicable but may be shadowed
# by a more specific method.
#
# Methods can have qualifiers, which give the method combination
# procedure a way to distinguish among methods. A method that has one
# or more qualifiers is called a qualified method. A method with no
# qualifiers is called an unqualified method. A qualifier is any
# non-list. The qualifiers defined by the standardized method
# combination types are symbols.
#
# In this specification, the terms ``primary method'' and ``auxiliary
# method'' are used to partition methods within a method combination
# type according to their intended use. In standard method
# combination, primary methods are unqualified methods and auxiliary
# methods are methods with a single qualifier that is one of :around,
# :before, or :after. Methods with these qualifiers are called around
# methods, before methods, and after methods, respectively. When a
# method combination type is defined using the short form of
# define-method-combination, primary methods are methods qualified
# with the name of the type of method combination, and auxiliary
# methods have the qualifier :around. Thus the terms ``primary
# method'' and ``auxiliary method'' have only a relative definition
# within a given method combination type.
#
## 7.6.3 Agreement on Parameter Specializers and Qualifiers
#
# Two methods are said to agree with each other on parameter
# specializers and qualifiers if the following conditions hold:
#
# 1. Both methods have the same number of required parameters. Suppose
# the parameter specializers of the two methods are P1,1...P1,n and
# P2,1...P2,n.
#
# 2. For each 1<=i<=n, P1,i agrees with P2,i. The parameter
# specializer P1,i agrees with P2,i if P1,i and P2,i are the same
# class or if P1,i=(eql object1), P2,i=(eql object2), and (eql object1
# object2). Otherwise P1,i and P2,i do not agree.
#
# 3. The two lists of qualifiers are the same under equal.
#
## 7.6.4 Congruent Lambda-lists for all Methods of a Generic Function
#
# These rules define the congruence of a set of lambda lists,
# including the lambda list of each method for a given generic
# function and the lambda list specified for the generic function
# itself, if given.
#
# 1. Each lambda list must have the same number of required
# parameters.
#
# 2. Each lambda list must have the same number of optional
# parameters. Each method can supply its own default for an optional
# parameter.
#
# 3. If any lambda list mentions &rest or &key, each lambda list must
# mention one or both of them.
#
# 4. If the generic function lambda list mentions &key, each method
# must accept all of the keyword names mentioned after &key, either by
# accepting them explicitly, by specifying &allow-other-keys, or by
# specifying &rest but not &key. Each method can accept additional
# keyword arguments of its own. The checking of the validity of
# keyword names is done in the generic function, not in each method. A
# method is invoked as if the keyword argument pair whose name is
# :allow-other-keys and whose value is true were supplied, though no
# such argument pair will be passed.
#
# 5. The use of &allow-other-keys need not be consistent across lambda
# lists. If &allow-other-keys is mentioned in the lambda list of any
# applicable method or of the generic function, any keyword arguments
# may be mentioned in the call to the generic function.
#
# 6. The use of &aux need not be consistent across methods.
#
# If a method-defining operator that cannot specify generic function
# options creates a generic function, and if the lambda list for the
# method mentions keyword arguments, the lambda list of the generic
# function will mention &key (but no keyword arguments).
        generic_function, definedp = frost.global_(fn.__name__, globals())
        fixed, optional, args, keyword, keys = lambda_list = function_lambda_list(fn)
        if not definedp:
                _, sym, __ = interpret_toplevel_value(fn, functionp)
                generic_function = ensure_generic_function(sym,
                                                           lambda_list = lambda_list,
                                                           # the rest is defaulted
                                                           )
        method_class = generic_function_method_class(generic_function)
        methfun_lambda, methfun_args = make_method_lambda(generic_function,
                                                          class_prototype(method_class),
                                                          fn, ___env___)
        method = make_instance(
                method_class,
                qualifiers = [], # XXX
                lambda_list = lambda_list,
                specializers = tuple(make_method_specializers(
                                         [ gethash(name, method.__annotations__, t)[0]
                                           for name in fixed ])),
                function = not_implemented("somehow compile", methfun_lambda)
                **methfun_args)
        add_method(generic_function, method)
        return method

def make_method_specializers(specializers):
        def parse(name):
                return (# name                                    if specializerp(name) else
                        name                                      if name is t                   else
                                                                  # ..special-case, since T isn't a type..
                        name                                      if isinstance(name, type)      else
                                                                  # Was: ((symbolp name) `(find-class ',name))
                        poor_man_ecase(car(name),
                                        (eql,       lambda: intern_eql_specializer(name[1])),
                                        (class_eq_, lambda: class_eq_specializer(name[1]))) if isinstance(name, tuple)      else
                        ## Was: FIXME: Document CLASS-EQ specializers.
                        error("%s is not a valid parameter specializer name.", name))
        return [ parse(x) for x in specializers ]

# Init

def init():
        "Initialise the Common Lisp compatibility layer."
        init_condition_system()
        string_set("*DEFAULT-PATHNAME-DEFAULTS*", os.path.getcwd())
        return t

# Thoughts on thunking

#
##
### Thunking is defined as code movement, introduced by the need for
### statement sequencing in AST nodes lowerable to IR incapable of sequencing.
### It is a kind of situation perceived to be rare for target IRs.
###
### From the standpoint of thunking, There are two kinds of expressions:
###  - thunk acceptors -- high-level IRs lowering to IRs "capable" of storing
###                       named functions with an arbitrary amount of subforms.
###    In short, anything lowering to:
###      - Module, Interactive
###      - FunctionDef, ClassDef
###      - For, While, If, With
###      - TryExcept, TryFinally, ExceptHandler
###  - thunk requestors -- high-level IRs with implicit PROGN (including PROGN itself),
###                        lowering to "incapable" IRs.
###
### Code movement introduces a lexical scope difference, the relevance of which is
###  in its effect on the free variables of the moved expression.
### This difference is twofold, in the context of Python:
###  - expression-wise de-scoping, where the difference is introduced by the
###    containing lambda expressions
###  - statement-wise scope change, with the differences introduced by assignment
###    statements (including destructuring)
### The second kind of difference can be avoided entirely, by placing the thunks,
###  generated by an expression, immediately before the statement containing the
###  expression.
##
#

# *PRINT/READ-<foo>* docstrings

"""Controls the format in which arrays are printed. If it is false,
the contents of arrays other than strings are never printed. Instead,
arrays are printed in a concise form using #< that gives enough
information for the user to be able to identify the array, but does
not include the entire array contents. If it is true, non-string
arrays are printed using #(...), #*, or #nA syntax."""

"""*PRINT-BASE* and *PRINT-RADIX* control the printing of
rationals. The value of *PRINT-BASE* is called the current output
base.

The value of *PRINT-BASE* is the radix in which the printer will print
rationals. For radices above 10, letters of the alphabet are used to
represent digits above 9."""

"""The value of *PRINT-CASE* controls the case (upper, lower, or
mixed) in which to print any uppercase characters in the names of
symbols when vertical-bar syntax is not used.

*PRINT-CASE* has an effect at all times when the value of
*PRINT-ESCAPE* is false. *PRINT-CASE* also has an effect when the
value of *PRINT-ESCAPE* is true unless inside an escape context
(i.e., unless between vertical-bars or after a slash)."""

"""Controls the attempt to detect circularity and sharing in an object
being printed.

If false, the printing process merely proceeds by recursive descent
without attempting to detect circularity and sharing.

If true, the printer will endeavor to detect cycles and sharing in the
structure to be printed, and to use #n= and #n# syntax to indicate the
circularities or shared components.

If true, a user-defined PRINT-OBJECT method can print objects to the
supplied stream using WRITE, PRIN1, PRINC, or FORMAT and expect
circularities and sharing to be detected and printed using the #n#
syntax. If a user-defined PRINT-OBJECT method prints to a stream other
than the one that was supplied, then circularity detection starts over
for that stream.

Note that implementations should not use #n# notation when the Lisp
reader would automatically assure sharing without it (e.g., as happens
with interned symbols)."""

"""If false, escape characters and package prefixes are not output
when an expression is printed.

If true, an attempt is made to print an expression in such a way that
it can be READ again to produce an equal expression. (This is only a
guideline; not a requirement. See *PRINT-READABLY*.)

For more specific details of how the value of *PRINT-ESCAPE* affects
the printing of certain types, see Section 22.1.3 (Default
Print-Object Methods)."""

"""Controls whether the prefix ``#:'' is printed before apparently
uninterned symbols. The prefix is printed before such symbols if and
only if the value of *PRINT-GENSYM* is true."""

"""*PRINT-LENGTH* controls how many elements at a given level are
printed. If it is false, there is no limit to the number of components
printed. Otherwise, it is an integer indicating the maximum number of
elements of an object to be printed. If exceeded, the printer will
print ``...'' in place of the other elements. In the case of a dotted
list, if the list contains exactly as many elements as the value of
*PRINT-LENGTH*, the terminating atom is printed rather than printing
``...''.

*PRINT-LEVEL* and *PRINT-LENGTH* affect the printing of an any object
printed with a list-like syntax. They do not affect the printing of
symbols, strings, and bit vectors."""

"""*PRINT-LEVEL* controls how many levels deep a nested object will
print. If it is false, then no control is exercised. Otherwise, it is
an integer indicating the maximum level to be printed. An object to be
printed is at level 0; its components (as of a list or vector) are at
level 1; and so on. If an object to be recursively printed has
components and is at a level equal to or greater than the value of
*PRINT-LEVEL*, then the object is printed as ``#''.

*PRINT-LEVEL* and *PRINT-LENGTH* affect the printing of an any object
printed with a list-like syntax. They do not affect the printing of
symbols, strings, and bit vectors."""

"""When the value of *PRINT-LINES* is other than NIL, it is a limit on
the number of output lines produced when something is pretty
printed. If an attempt is made to go beyond that many lines, ``..'' is
printed at the end of the last line followed by all of the suffixes
(closing delimiters) that are pending to be printed."""

"""If it is not NIL, the pretty printer switches to a compact style of
output (called miser style) whenever the width available for printing
a substructure is less than or equal to this many ems."""

"""The PPRINT dispatch table which currently controls the pretty printer.

Initial value is implementation-dependent, but the initial entries all
use a special class of priorities that have the property that they are
less than every priority that can be specified using
SET-PPRINT-DISPATCH, so that the initial contents of any entry can be
overridden."""

"""Controls whether the Lisp printer calls the pretty printer.

If it is false, the pretty printer is not used and a minimum of
whitespace[1] is output when printing an expression.

If it is true, the pretty printer is used, and the Lisp printer will
endeavor to insert extra whitespace[1] where appropriate to make
expressions more readable.

*PRINT-PRETTY* has an effect even when the value of *PRINT-ESCAPE* is
false."""

"""*PRINT-BASE* and *PRINT-RADIX* control the printing of
rationals. The value of *PRINT-BASE* is called the current output
base.

If the value of *PRINT-RADIX* is true, the printer will print a radix
specifier to indicate the radix in which it is printing a rational
number. The radix specifier is always printed using lowercase
letters. If *PRINT-BASE* is 2, 8, or 16, then the radix specifier used
is #b, #o, or #x, respectively. For integers, base ten is indicated by
a trailing decimal point instead of a leading radix specifier; for
ratios, #10r is used."""

"""If *PRINT-READABLY* is true, some special rules for printing
objects go into effect. Specifically, printing any object O1 produces
a printed representation that, when seen by the Lisp reader while the
standard readtable is in effect, will produce an object O2 that is
similar to O1. The printed representation produced might or might not
be the same as the printed representation produced when
*PRINT-READABLY* is false. If printing an object readably is not
possible, an error of type print-not-readable is signaled rather than
using a syntax (e.g., the ``#<'' syntax) that would not be readable by
the same implementation. If the value of some other printer control
variable is such that these requirements would be violated, the value
of that other variable is ignored.

Specifically, if *PRINT-READABLY* is true, printing proceeds as if
*PRINT-ESCAPE*, *PRINT-ARRAY*, and *PRINT-GENSYM* were also true, and
as if *PRINT-LENGTH*, *PRINT-LEVEL*, AND *PRINT-LINES* were false.

If *PRINT-READABLY* is false, the normal rules for printing and the
normal interpretations of other printer control variables are in
effect.

Individual methods for PRINT-OBJECT, including user-defined methods,
are responsible for implementing these requirements.

If *READ-EVAL* is false and *PRINT-READABLY* is true, any such method
that would output a reference to the ``#.'' reader macro will either
output something else or will signal an error (as described above)."""

"""If it is non-NIL, it specifies the right margin (as integer number
of ems) to use when the pretty printer is making layout decisions.

If it is NIL, the right margin is taken to be the maximum line length
such that output can be displayed without wraparound or truncation. If
this cannot be determined, an implementation-dependent value is
used."""
