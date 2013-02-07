def attrify_args(self, locals, *names):
        for name in names:
                setattr(self, name, locals[name])

class function():
        def __init__(self, name, result_type, arglist, bb, linkage = "private", visibility_style = "default"):
                attrify_args(self, locals(),
                             "name", "result_type", "arglist", "bb",
                             "linkage", "visibility_style")
