MCS=MONO_PATH=../../../bin:../../../modules dmcs -sdk:4

all:  ../PJScheme.dll calicoscheme.py

../PJScheme.dll: calicoscheme.cs Scheme.cs calicoscheme-rm.ss Makefile ObjectType.cs Rational.cs 
	$(MCS) Scheme.cs calicoscheme.cs ObjectType.cs Rational.cs -target:library -lib:../../../bin -r:Microsoft.Scripting -r:Microsoft.CSharp -r:IronPython.dll -r:Microsoft.Dynamic -r:Calico.exe -pkg:gtk-sharp-2.0 -r:Mono.TextEditor.dll -r:System.Numerics -out:../PJScheme.dll

PJScheme.exe: calicoscheme.cs Scheme.cs calicoscheme-rm.ss Makefile ObjectType.cs Rational.cs 
	$(MCS) -debug Scheme.cs calicoscheme.cs ObjectType.cs Rational.cs -target:exe -lib:../../../bin -r:Microsoft.Scripting -r:Microsoft.CSharp -r:IronPython.dll -r:Microsoft.Dynamic -r:Calico.exe -pkg:gtk-sharp-2.0 -r:Mono.TextEditor.dll -r:System.Numerics -out:PJScheme.exe

calicoscheme.py: calicoscheme-rm.ss translate_rm.py Scheme.py Makefile
	python translate_rm.py "calicoscheme-rm.ss" "calicoscheme.py"
	cp calicoscheme.py ../../../src/calico/scheme.py

calicoscheme.cs: calicoscheme-rm.ss translate_rm.py Scheme.cs Makefile
	python translate_rm.py "calicoscheme-rm.ss" "calicoscheme.cs"

calicoscheme-ds.ss: reader-cps.ss environments-cps.ss parser-cps.ss \
		interpreter-cps.ss unifier-cps.ss ds-transformer.ss
	cat reader-cps.ss environments-cps.ss parser-cps.ss interpreter-cps.ss unifier-cps.ss > calicoscheme-cps.ss
	echo "(load \"ds-transformer.ss\")" > compile-ds.ss
	echo "(delete-file \"calicoscheme-ds.ss\")" >> compile-ds.ss
	echo "(ds-transform-file \"calicoscheme-cps.ss\" \"calicoscheme-ds.ss\")" >> compile-ds.ss
	echo "(exit)" >> compile-ds.ss
	petite compile-ds.ss

calicoscheme-rm.ss: calicoscheme-ds.ss rm-transformer.ss
	echo "(load \"rm-transformer.ss\")" > compile-rm.ss
	echo "(delete-file \"calicoscheme-rm.ss\")" >> compile-rm.ss
	echo "(compile-level-output)" >> compile-rm.ss
	echo "(rm-transform-file \"calicoscheme-ds.ss\" \"calicoscheme-rm.ss\")" >> compile-rm.ss
	echo "(exit)" >> compile-rm.ss
	petite compile-rm.ss

run: calicoscheme-rm.ss no-csharp-support.ss
	echo "(case-sensitive #t)" > calicoscheme-run.ss
	echo "(load \"calicoscheme-rm.ss\")" >> calicoscheme-run.ss
	echo "(load \"no-csharp-support.ss\")" >> calicoscheme-run.ss
	echo "(begin (display (run start)) (newline))" >> calicoscheme-run.ss
	petite calicoscheme-run.ss

clean:
	$(RM) *.exe *~ calicoscheme-ds.ss calicoscheme-rm.ss compile-rm.ss compile-ds.ss make-fact.ss \
		fact.cs make-calicoscheme.ss calicoscheme.exe calicoscheme.cs calicoscheme-cps.ss \
		calicoscheme-run.ss calicoscheme.ss fact-rm.ss fact-ds.ss all*.ss testall.ss

