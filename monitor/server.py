#!/usr/bin/env python

import json
import screen_reader as sr
from pyscreenshot import grab
from werkzeug.wrappers import Request, Response
import numpy as np

development = True

if development:
    img = './nn/wvxnki9nwfxhKrtlGFynmdvfuHmPthrl.png'
else:
    def callback():
        return np.array(grab()) / 255.
    img = callback

@Request.application
def application(_request):
    return Response(json.dumps(sr.read(img)))

if __name__ == '__main__':
    from werkzeug.serving import run_simple
    run_simple('localhost', 4000, application)
