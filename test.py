#!/usr/bin/env python

import subprocess
import sys
import urllib

def curl(s):
    subprocess.call(['curl', s])

_, port, user, text = map(lambda s: urllib.quote(s.decode('utf8').encode('utf8')), sys.argv)
curl('http://localhost:%s/?channel_name=slacktest&user_id=U2147483697&user_name=%s&command=/jpgto&text=%s' % (port, user, text))
