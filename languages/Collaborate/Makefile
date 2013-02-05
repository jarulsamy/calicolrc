CalicoCollaborate.exe: CalicoCollaborate.cs
	dmcs -pkg:gtk-sharp-2.0 -r:webkit-sharp -target:library -lib:/usr/lib/mono/gac/webkit-sharp/1.1.15.0__eaa1d335d2e19745 -lib:../../bin -r:Calico.exe CalicoCollaborate.cs

Collaborate.exe: Collaborate.cs
	dmcs -pkg:gtk-sharp-2.0 -r:webkit-sharp -lib:/usr/lib/mono/gac/webkit-sharp/1.1.15.0__eaa1d335d2e19745 Collaborate.cs

clean::
	rm -rf *.exe *~

run:
	mono CalicoCollaborate.exe
