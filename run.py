#!/usr/bin/env python

import sys
import macropy.activate
from cpeg import *

if __name__ == '__main__':
    nsmap = {
        'xsi': "http://www.w3.org/2001/XMLSchema-instance",
        'c': 'urn:c-ast'
    }

    t = TranslationUnit.parse(sys.stdin.read())
    print t.__xmlstr__(tag='nodes', nsmap=nsmap)


# vim: ts=4 sts=4 sw=4 expandtab:
