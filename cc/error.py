import sys
import os

_tty = os.isatty(sys.stderr.fileno())
_colors = {
    'black': '\033[30m',
    'red':   '\033[31m',
    'green': '\033[32m',
    'brown': '\033[33m',
    'blue':  '\033[34m',
    'purple': '\033[35m',
    'cyan':  '\033[36m',
    'white': '\033[37m',

    'gray':       '\033[30;1m',
    'lightred':   '\033[31;1m',
    'lightgreen': '\033[32;1m',
    'yellow':     '\033[33;1m',
    'lightblue':  '\033[34;1m',
    'lightpurple': '\033[35;1m',
    'lightcyan':  '\033[36;1m',
    'lightwhite': '\033[37;1m',
    
    'reset': '\033[0m',
}

def color(color, s, reset=True):
    if _tty:
        sys.stderr.write(_colors[color])
    sys.stderr.write(s)
    if _tty and reset:
        sys.stderr.write(_colors['reset'])


def fatal(msg, *args):
    color('lightred', 'FATAL: ')
    print >>sys.stderr, msg % args
    raise Exception('fatal error')
    #sys.exit(1)

def warn(msg, *args):
    color('yellow', 'WARNING: ')
    print >>sys.stderr, msg % args

def info(msg, *args):
    color('cyan', 'INFO: ')
    print >>sys.stderr, msg % args
