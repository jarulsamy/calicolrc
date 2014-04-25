

all: 
	rm -f `find | grep Index.ipynb`
	python make.py

cache:
	find | grep ipynb$ | grep -v checkpoints | grep -v attic | grep -v Untitled | xargs -d"\n" -n 1 -i wget -O /dev/null "http://nbviewer.org/urls/bitbucket.org/ipre/calico/raw/master/notebooks/{}?create=1"

clean::
	rm -f `find | grep Index.ipynb`
