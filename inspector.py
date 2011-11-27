import dis

from cl import *
from pergamum import *
from swank import *

###
### Inspection.
###
# makeReplResult <- function(value) {
#   string <- printToString(value)
#   list(quote(`:write-string`), string,
#        quote(`:repl-result`))
# }
def make_repl_result(value):
       string = print_to_string(value)
       return [keyword("write-string"), string, keyword("repl-result")]

# makeReplResultFunction <- makeReplResult
make_repl_result_function = make_repl_result

# sendReplResult <- function(slimeConnection, value) {
#   result <- makeReplResultFunction(value)
#   sendToEmacs(slimeConnection, result)
# }
### def send_repl_result(slime_connection, value):
###         result = make_repl_result_function(value)
###         return send_to_emacs(slime_connection, result)

# sendReplResultFunction <- sendReplResult
### send_repl_result_function = send_repl_result

###
### Presentations
###
record_repl_results = True
object_to_presentation_id = dict()
presentation_id_to_object = dict()

def clear_presentation_tables():
        object_to_presentation_id = dict()
        presentation_id_to_object = dict()

presentation_counter = 0

# (defun save-presented-object (object)
#   "Save OBJECT and return the assigned id.
# If OBJECT was saved previously return the old id."
#   (let ((object (if (null object) *nil-surrogate* object)))
#     ;; We store *nil-surrogate* instead of nil, to distinguish it from
#     ;; an object that was garbage collected.
#     (or (gethash object *object-to-presentation-id*)
#         (let ((id (incf *presentation-counter*)))
#           (setf (gethash id *presentation-id-to-object*) object)
#           (setf (gethash object *object-to-presentation-id*) id)
#           id))))
def save_presented_object(object):
        """Save OBJECT and return the assigned id.
If OBJECT was saved previously return the old id."""
        # debug_printf(">>               - s_p_o(%s), type %s", object, type(object[0]))
        object = (nil_surrogate if nonep(object) else object),
        def store(object):
                global presentation_counter
                presentation_counter += 1
                object_to_presentation_id[object] = presentation_counter
                presentation_id_to_object[presentation_counter] = object
                # debug_printf("==               - s_p_o(%s), id %d, type %s", object, presentation_counter, type(object[0]))
                return presentation_counter
        o, hasp = gethash(object, object_to_presentation_id)
        return o if hasp else store(object)

def lookup_presented_object(id):
        if integerp(id):
                (object,), foundp = gethash(id, presentation_id_to_object)
                # debug_printf("--               - l_p_o: %s  -%s-> %s", id, foundp, object)
                if object is nil_surrogate:
                        return False, True
                else:
                        return object, foundp
        elif listp(id):
                if id[0] == keyword("frame_var"): # XXX: actually, frame_local, but..
                        thread_id, frame_id, index = id[1:]
                        frame = env.sldb_state.frames[frame_id]
                        try:
                                # debug_printf(">>               - o_f_l(%s, %s)", frame, index)
                                v = ordered_frame_locals(frame)[index]
                                # debug_printf("<<               - o_f_l(%s, %s) -> %s", frame, index, v)
                        except Exception as cond:
                                pass
                                # debug_printf("EE               - o_f_l(%s, %s) -> %s", frame, index, cond)
                        return handler_case(lambda: ordered_frame_locals(frame)[index],
                                            error = lambda c: error(c),
                                            # error = lambda _: (None, None),
                                            no_error = lambda value: (value, True))
                elif id[0] == keyword("inspected_part"):
                        part_index, inspectee_parts = id[1], env.inspectee_parts
                        if part_index < len(env.inspectee_parts):
                                return None, None
                        else:
                                return inspector_nth_part(part_index), True
                else:
                        error("Bad presented object ID: %s", id)

def lookup_presented_object_or_lose(id):
        object, foundp = lookup_presented_object(id)
        return object if foundp else error("Attempt to access unrecorded object (id %s).", id)

def clear_repl_results():
        clear_presentation_tables()

def present_repl_results(values):
        # debug_printf(">>               - p_r_r(%s)", values)
        def send(value):
                id = record_repl_results and save_presented_object(value)
                send_to_emacs([keyword("presentation-start"), id,                     keyword("repl-result")])
                send_to_emacs([keyword("write-string"),       value.__repr__(),       keyword("repl-result")])
                send_to_emacs([keyword("presentation-end"),   id,                     keyword("repl-result")])
                send_to_emacs([keyword("write-string"),       "\n",                   keyword("repl-result")])
        format(t, "\n")
        if not values:
                send_to_emacs([keyword("write-string"),       "; No value.",          keyword("repl-result")])
        else:
                mapc(send, values)

#### Presentation menu protocol
##
## To define a menu for a type of object, define a method
## menu-choices-for-presentation on that object type.  This function
## should return a list of two element lists where the first element is
## the name of the menu action and the second is a function that will be
## called if the menu is chosen. The function will be called with 3
## arguments:
##
## choice: The string naming the action from above
##
## object: The object
##
## id: The presentation id of the object
##
## You might want append (when (next-method-p) (call-next-method)) to
## pick up the Menu actions of superclasses.
##

presentation_active_menu = None

def menu_choices_for_presentation_id(id):
        global presentation_active_menu
        ob, presentp = lookup_presented_object(id)
        if not presentp:
                return intern0("not-present")
        else:
                menu_and_actions = menu_choices_for_presentation(ob)
                presentation_active_menu = [id] + menu_and_actions
                return mapcar(first, menu_and_actions)

# (defun swank-ioify (thing)
#   (cond ((keywordp thing) thing)
# 	((and (symbolp thing)(not (find #\: (symbol-name thing))))
# 	 (intern (symbol-name thing) 'swank-io-package))
# 	((consp thing) (cons (swank-ioify (car thing)) (swank-ioify (cdr thing))))
# 	(t thing)))
def swank_ioify(thing):
        if keywordp(thing):
                return thing
        elif symbolp(thing) and not find(":", symbol_name(thing)):
                return intern0(symbol_name(thing), "SWANK-IO-PACKAGE")
        elif listp(thing):
                return [swank_ioify(first(thing))] + swank_ioify(rest(thing))
        else:
                return thing

# (defun execute-menu-choice-for-presentation-id (id count item)
#   (let ((ob (lookup-presented-object id)))
#     (assert (equal id (car *presentation-active-menu*)) ()
# 	    "Bug: Execute menu call for id ~a  but menu has id ~a"
# 	    id (car *presentation-active-menu*))
#     (let ((action (second (nth (1- count) (cdr *presentation-active-menu*)))))
#       (swank-ioify (funcall action item ob id)))))
def execute_menu_choice_for_presentation_id(id, count, item):
        ob, _ = lookup_presented_object(id)
        if id != first(presentation_active_menu):
                error("Bug: Execute menu call for id ~a  but menu has id ~a",
                      id, first(presentation_active_menu))
        action = rest(presentation_active_menu)[count - 1][1]
        return swank_ioify(action(item, ob, id))

def pathnamep(x):
        "A flaming heuristic.."
        return stringp(x) and x.find(".") != -1 and x.find(os.sep) != -1
def menu_choices_for_presentation(ob):
        if pathnamep(ob):
                file_exists, lisp_type = os.access(ob, os.R_OK)
                coerced_source_file = ob.split(".")[-1] + ["py"]
                source_file = (ob.split(os.sep)[-1] != "py" and
                               letf(".".join(coerced_source_file),
                                    lambda source:
                                            os.access(source, os.R_OK) and source))
                fasl_file = (file_exists and
                             namestring(truename(ob)) == namestring(truename(compile_file_pathname(coerced_source_file))))
                remove(None,
                       [file_exists and not fasl_file and
                        ["Edit this file",
                         lambda choice, object, id:
                                 ed_in_emacs(namestring(truename(object))) and None]],
	     # (and file-exists
	     #      (list "Dired containing directory"
	     #    	(lambda (choice object id)
	     #    	  (declare (ignore choice id))
	     #    	  (ed-in-emacs (namestring
	     #    			(truename
	     #    			 (merge-pathnames
	     #    			  (make-pathname :name "" :type "") object))))
	     #    	  nil)))
                       [file_exists and
                        ["Dired containing directory",
                         lambda choice, object, id:
                                 error("Not implemented: Dired containing directory")]]
	     # (and fasl-file
	     #      (list "Load this fasl file"
	     #    	(lambda (choice object id)
	     #    	  (declare (ignore choice id object))
	     #    	  (load ob)
	     #    	  nil)))
                       [fasl_file and
                        ["Load this fasl file",
                         lambda choice, object, id:
                                 error("Not implemented: Load this fasl file")]]
	     # (and fasl-file
	     #      (list "Delete this fasl file"
	     #    	(lambda (choice object id)
	     #    	  (declare (ignore choice id object))
	     #    	  (let ((nt (namestring (truename ob))))
	     #    	    (when (y-or-n-p-in-emacs "Delete ~a? " nt)
	     #    	      (delete-file nt)))
	     #    	  nil)))
                       [fasl_file and
                        ["Delete this fasl file",
                         lambda choice, object, id:
                                 error("Not implemented: Delete this fasl file")]]
                       [source_file and
                        ["Edit source file",
                         lambda choice, object, id:
                                 ed_in_emacs(namestring(truename(source_file))) and None]]
                       [source_file and
                        ["Load source file",
                         lambda choice, object, id:
                                 load(source_file) and None]])
        if functionp(ob):
                return [["Disassemble",
                         lambda _, object, __:
                                 dis.dis(object)]]
        else:
                return None

def inspect_presentation(id, reset_p):
        what = lookup_presented_object_or_lose(id)
        # debug_printf("<<               - l_p_o_o_l(%s) -> %s, a %s", id, what, type(what))
        if reset_p:
                reset_inspector(env.slime_connection)
        return inspect_object(env.slime_connection, what)

send_repl_results_function = present_repl_results
