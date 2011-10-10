import platform

py_ver_major = platform.python_version_tuple()[0] 

def py2p(): return py_ver_major == '2'
def py3p(): return py_ver_major == '3'

if py2p():
        from neutrality_py2 import stringp, printf, fprintf, file_content, set_file_content, to_unicode, to_raw_text, hunter_seeker, package_hunter_seeker
elif py3p():
        from neutrality_py3 import stringp, printf, fprintf, file_content, set_file_content, to_unicode, to_raw_text, hunter_seeker, package_hunter_seeker
        pass

printf("python neutrality: initialised for version %s", py_ver_major)

__all__ = ['py2p',
           'py3p',
           ###
           'stringp',
           'printf',
           'fprintf',
           'file_content',
           'set_file_content',
           'to_unicode',
           'to_raw_text',
           ###
           'hunter_seeker',
           'package_hunter_seeker',
           ]
