
all: es.po
	msgfmt es.po -o es/LC_MESSAGES/calico.mo

calico.pot: ../*/*.py 
	xgettext -L Python ../*/*.py -o calico.pot

