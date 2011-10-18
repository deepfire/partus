from importlib import abc
import sys

def stringp(o):
        return type(o) is str

def _write_string(string, stream):
        print(string, file = stream, end = '')
        return string

def file_content(path):
        with open(path, "r", encoding = 'utf-8-sig') as f:
                return f.read()

def set_file_content(path, content):
        with open(path, "w", encoding = 'utf-8') as f:
                fprintf(f, "%s", content)

def to_unicode(x):
        return x

def to_raw_text(x):
        return x

class hunter_seeker(abc.Finder, abc.Loader):
        names = set()
        method = None
        def configure(method, names):
                (hunter_seeker.names, hunter_seeker.method) = (names, method)
        def disable():
                (hunter_seeker.names, hunter_seeker.method) = (set(), None)
        def load_module(name):
                return hunter_seeker.method(name)
        def find_module(fullname, path = None):
                return hunter_seeker if fullname in hunter_seeker.names else None

class package_hunter_seeker(abc.Finder, abc.Loader):
        dict = {}
        def configure(dict):
                package_hunter_seeker.dict = dict
        def disable():
                package_hunter_seeker.dict = None
        def load_module(name):
                return package_hunter_seeker.dict[name]
        def find_module(fullname, path = None):
                return package_hunter_seeker if fullname in package_hunter_seeker.dict else None

sys.meta_path += [hunter_seeker, package_hunter_seeker]
