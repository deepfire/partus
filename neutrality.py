import platform
import sys

py_ver_major = platform.python_version_tuple()[0] 
py_ver_minor = platform.python_version_tuple()[1]

def py2p(): return py_ver_major == '2'
def py3p(): return py_ver_major == '3'

def py3minor_atleast(x): return int(py_ver_minor) >= x

if py2p():
        from neutrality_py2 import stringp, do_write_string, file_content, set_file_content, to_unicode, to_raw_text, hunter_seeker, package_hunter_seeker
elif py3p():
        from neutrality_py3 import stringp, do_write_string, file_content, set_file_content, to_unicode, to_raw_text, hunter_seeker, package_hunter_seeker
        pass

# do_write_string("python neutrality: initialised for version %s.%s\n" % (py_ver_major, py_ver_minor), sys.stdout)

__all__ = ['py2p',
           'py3p',
           'py3atleastp',
           ###
           'stringp',
           'do_write_string',
           'file_content',
           'set_file_content',
           'to_unicode',
           'to_raw_text',
           ###
           'hunter_seeker',
           'package_hunter_seeker',
           ]
