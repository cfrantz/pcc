#!/usr/bin/env python

import sys
import macropy.activate
import parser
from dumper import Dumper
from cemit import emit
import json

if __name__ == '__main__':
    t = parser.parse(sys.stdin.read())
#    print(json.dumps(t, indent=4, cls=Dumper))
    print t
    print emit(t)

