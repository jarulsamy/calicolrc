import sys

if sys.platform == 'cli':
    import pyprof

import nntplib
server = nntplib.NNTP('news.gmane.org')
print server.description('gmane.comp.python.general')
server.quit()
