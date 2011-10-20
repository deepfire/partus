from cl import *

class swank_reader_error(Exception):
        def __init__(self, packet, cause):
                self.packet, self.cause = packet, cause

def read_message(stream, package):
        packet = read_packet(stream)
        return handler_case(
                lambda: read_form(packet, package),
                Exception = (lambda c: # XXX: originally, caught only READER-ERROR
                                     error(make_condition(swank_reader_error,
                                                          packet, cause))))

# use peek-char to detect EOF, read-sequence may return 0 instead of
# signaling a condition.
def read_packet(stream):
        peek_char(None, stream)
        header = read_chunk(stream, 6)
        length = parse_integer(header, radix = 0x10)
        return read_chunk(stream, length)

def read_chunk(stream, length):
        buffer = make_list(length, initial_element = None)
        count = read_sequence(buffer, stream)
        if count != length:
                error("Short read: length=%d, count=%d", length, count)
        return buffer

# FIXME: no one ever tested this and will probably not work.
setq("_validate_input_", None)
# "Set to true to require input that strictly conforms to the protocol"

def read_form(string, package):
        def body():
                with env.let(_package_ = package):
                        if symbol_value("_validate_input_"):
                                return validating_read(string)
                        else:
                                return read_from_string(string)
        return with_standard_io_syntax(body)

def validating_read(string):
        def body(stream):
                with env.let(_standard_input_, stream):
                        return simple_read()
        return with_input_from_string(string, body)

def peek_char(peek_type, stream = nil, eof_error_p = True, eof_value = None, recursive_p = None):
        return "a"

def read_sequence(sequence, stream, start = 0, end = None):
        return 0

def simple_read(istream):
        "Read a form that conforms to the protocol, otherwise signal an error."
        c = read_char(istream)
        if c == "\"":
                def body(str):
                        c = read_char(istream)
                        while c != "\"":
                                write_char((read_char(istream)
                                            if c == "\\" else
                                            c), str)
                                c = read_char(istream)
                return with_output_to_string(body)
        elif c == "(":
                ## Don't we just loove Python for the lack of concise loops?
                # (#\( (loop collect (simple-read)
                #            while (ecase (read-char)
                #                    (#\) nil)
                #                    (#\space t))))
                acc = []
                x = simple_read (istream); acc.append(x)
                c = read_char(istream)
                goon = c == " "
                while goon:
                        if c != ")":
                                error("SIMPLE-READ: only spaces are allowed as token separators.")
                        x = simple_read (istream); acc.append(x)
                        c = read_char(istream)
                        goon = c == " "
                return acc
        elif c == "'":
                return [quote, simple_read(istream)]
        else:
                def body(str):
                        ch = c
                        goon = True
                        while goon:
                                if ch == "\\":
                                        write_char(read_char(istream), str)
                                elif ch == " " or ch == ")":
                                        unread_char(ch, istream)
                                        return
                                else:
                                        write_char(ch, str)
                                ch = read_char(istream, None, nil)
                                goon = ch is not nil
                string = with_output_to_string(body)
                return (parse_integer(string) if digit_char_p(c) else
                        intern(string))

##### Output
def write_message(message, package, stream):
        string = prin1_to_string_for_emacs(message, package)
        length = codepoint_length(string)
        with env.let(_print_pretty_ = nil):
                format(stream, "%06d", length)
        write_string(string, stream)
        finish_output(stream)

def prin1_to_string_for_emacs(object, package):
        def body():
                with env.let(_print_case_     = keyword("downcase"),
                             _print_readably_ = nil,
                             _print_pretty_   = nil,
                             _package_        = package):
                        return prin1_to_string(object)
        return with_standard_io_syntax(body)
