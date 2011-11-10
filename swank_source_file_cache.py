#### Source_file cache
###
### To robustly find source locations in CMUCL and SBCL it's useful to
### have the exact source code that the loaded code was compiled from.
### In this source we can accurately find the right location, and from
### that location we can extract a "snippet" of code to show what the
### definition looks like. Emacs can use this snippet in a best-match
### search to locate the right definition, which works well even if
### the buffer has been modified.
###
### The idea is that if a definition previously started with
### `(define-foo bar' then it probably still does.
###
### Whenever we see that the file on disk has the same
### `file-write-date' as a location we're looking for we cache the
### whole file inside Lisp. That way we will still have the matching
### version even if the file is later modified on disk. If the file is
### later recompiled and reloaded then we replace our cache entry.
###
### This code has been placed in the Public Domain.  All warranties
### are disclaimed.

import cl

from cl import *
from pergamum import *

from swank_backend import defimplementation

defvar("_cache_sourcecode_", t)
"""When true complete source files are cached.
The cache is used to keep known good copies of the source text which
correspond to the loaded code. Finding definitions is much more
reliable when the exact source is available, so we cache it in case it
gets edited on disk later."""

defvar("_source_file_cache_", dict())
"""Cache of source file contents.
Maps from truename to source_cache_entry structure."""

source_cache_entry = defstruct("source_cache_entry",
                               "text",
                               "date")

@defimplementation
def buffer_first_change(filename):
        """Load a file into the cache when the user modifies its buffer.
    This is a win if the user then saves the file and tries to M-. into it."""
        if not source_cached_p(filename):
                ignore_errors(
                        lambda: source_cache_get(filename, file_write_date(filename)))
        return nil

def get_source_code(filename, code_date):
        """Return the source code for FILENAME as written on DATE in a string.
If the exact version cannot be found then return the current one from disk."""
        return (source_cache_get(filename, code_date) or
                read_file(filename))

def source_cache_get(filename, date):
    """Return the source code for FILENAME as written on DATE in a string.
Return NIL if the right version cannot be found."""
    if symbol_value("_cache_sourcecode_"):
            entry, therep = gethash(filename, symbol_value("_source_file_cache_"))
            return (entry.text
                    if entry and date == entry.date       else # Cache miss.
                    # File on disk has the correct version.
                    lret(read_file(filename),
                         lambda source:
                                 symbol_value("_source_file_cache_").update({
                                            filename: make_source_cache_entry(source, date) }))
                    if file_write_date(filename) == date else
                    nil)

def source_cached_p (filename):
        "Is any version of FILENAME in the source cache?"
        return gethash(filename, symbol_value("_source_file_cache_"))[1]

def read_file (filename):
        "Return the entire contents of FILENAME as a string."
        ## Was:
        # :external_format (or (guess_external_format filename)
        #                      (find_external_format "latin_1")
        #                      :default)
        return file_as_string(filename)

#### Snippets

defvar("_source_snippet_size_", 256)
"""Maximum number of characters in a snippet of source code.
Snippets at the beginning of definitions are used to tell Emacs what
the definitions looks like, so that it can accurately find them by
text search."""

def read_snippet (stream, position = None):
        """Read a string of upto *SOURCE-SNIPPET-SIZE* characters from STREAM.
If POSITION is given, set the STREAM's file position first."""
        if position:
                file_position(stream, position)
        #+sbcl
        skip_comments_and_whitespace(stream)
        return read_upto_n_chars(stream, symbol_value("_source_snippet_size_"))

def read_snippet_from_string (string, position = None):
        return with_input_from_string(string,
                                      lambda s: read_snippet(s, position))

def skip_comments_and_whitespace(stream):
        return case(peek_char(nil, stream),
                    (set([" ", "\t", "\n", "\r", "\p"]),
                     lambda: (read_char(stream) and
                              skip_comments_and_whitespace(stream))),
                    ("#",
                     lambda: (read_line(stream) and
                              skip_comments_and_whitespace(stream))))

def read_upto_n_chars(stream, n):
        "Return a string of upto N chars from STREAM."
        return stream.read(n)

