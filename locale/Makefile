
all: es/LC_MESSAGES/calico.mo fr/LC_MESSAGES/calico.mo

es/LC_MESSAGES/calico.mo: es.po
	msgfmt es.po -o es/LC_MESSAGES/calico.mo

fr/LC_MESSAGES/calico.mo: fr.po
	msgfmt fr.po -o fr/LC_MESSAGES/calico.mo

calico.pot: ../*/*.py 
	xgettext -L Python ../*/*.py -o calico.pot

clean:
	rm -f fr/LC_MESSAGES/calico.mo es/LC_MESSAGES/calico.mo