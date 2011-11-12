import inspect

import cl
from   cl import *

import sb_di

def maybe_block_start_location(location):
        # (if (sb!di:code-location-unknown-p loc)
        #  (let* ((block (sb!di:code-location-debug-block loc))
        #         (start (sb!di:do-debug-block-locations (loc block)
        #                 (return loc))))
        #   (cond ((and (not (sb!di:debug-block-elsewhere-p block))
        #           start)
        #          (format *debug-io* "~%unknown location: using block start~%")
        #          start)
        #    (t
        #     loc)))
        #  loc)
        return location

def code_location_source_form(location, context):
        location = maybe_block_start_location(location)
        form_num = sb_di.code_location_form_number(location)
        translations, form = get_toplevel_form(location)
        if not form_num < len(translations):
                error("The source path no longer exists.")
        return sb_di.source_path_context(form,
                                         translations[form_num],
                                         context)
        ## Ha, and I expected this to be:
        return inspect.getsource(loc.function)
