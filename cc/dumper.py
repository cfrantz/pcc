from collections import OrderedDict
import json
import cast

class Dumper(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, cast.AST):
            d = OrderedDict()
            d['class'] = obj.__class__.__name__
            for k in obj._fields:
                d[k] = getattr(obj, k)
            return d
        return json.JSONEncoder.default(self, obj)


