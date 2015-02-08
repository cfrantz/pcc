from StringIO import StringIO

def cstr(s):
    ret = StringIO()
    state = 0
    i = 0
    for ch in s:
        n = ord(ch)
        # Control chars, single quote, or DEL and beyond
        if n<32 or n==39 or n>=127:
            if state == 1:
                ret.write("'")
                state = 0
            if i:
                ret.write(",")
            ret.write("%d" % n)
        else:
            if state == 0:
                if i:
                    ret.write(",")
                ret.write("'")
                state = 1
            ret.write(ch)
        i += 1

    if state == 1:
        ret.write("'")
    if i:
        ret.write(",")
    ret.write("%d" % 0)
    
    return ret.getvalue()


def rename(filename, ext):
    fn, _ = filename.rsplit('.', 1)
    if ext is None:
        return fn
    return '.'.join((fn, ext))
