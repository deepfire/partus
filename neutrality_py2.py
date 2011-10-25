import sys

def stringp(o):
        return (type(o) is str) or (type(o) is unicode)

def _write_string(string, stream):
        print >> stream, string
        return string

def file_content(path):
        with open(path, "r") as f:
                return unicode(f.read(), 'utf-8')

def set_file_content(path, content):
        with open(path, "w") as f:
                _write_string(content.encode('utf-8'), f)

def to_unicode(x):
        return unicode(x, 'utf-8')

def to_raw_text(x):
        return x.encode('utf-8')

class hunter_seeker(object):
        names = set()
        method = None
        @classmethod
        def configure(hunter_seeker, method, names):
                (hunter_seeker.names, hunter_seeker.method) = (names, method)
        @classmethod
        def disable(hunter_seeker):
                (hunter_seeker.names, hunter_seeker.method) = (set(), None)
        def load_module(self, fullname, path = None):
                return self.method(fullname)
        def find_module(self, fullname, path):
                return self if fullname in self.names else None

class package_hunter_seeker(object):
        dict = {}
        @classmethod
        def configure(package_hunter_seeker, dict):
                package_hunter_seeker.dict = dict
        @classmethod
        def disable(package_hunter_seeker):
                package_hunter_seeker.dict = None
        def load_module(self, name):
                return self.dict[name]
        def find_module(self, fullname, path = None):
                return self if fullname in self.dict else None

sys.meta_path += [hunter_seeker(), package_hunter_seeker()]
