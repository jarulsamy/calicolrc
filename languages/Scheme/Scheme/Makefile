MCS=MONO_PATH=../../../bin:../../../modules dmcs -sdk:4

calicoscheme.py: calicoscheme-rm.ss translate_rm.py Scheme.py 
	python translate_rm.py "calicoscheme-rm.ss" "calicoscheme.py"

#../Calicoscheme.dll: calicoscheme.cs Scheme.cs calicoscheme-rm.ss Makefile ObjectType.cs 
#	$(MCS) -debug Scheme.cs calicoscheme.cs ObjectType.cs -target:library -lib:../../../bin -r:Microsoft.Scripting -r:IronPython.dll -r:Microsoft.Dynamic -r:Microsoft.Scripting.Core -r:Calico.exe -pkg:gtk-sharp-2.0 -r:Mono.TextEditor.dll -out:../Calicoscheme.dll

../PJScheme.dll: calicoscheme.cs Scheme.cs calicoscheme-rm.ss Makefile ObjectType.cs Rational.cs 
#	sed -i.BAK "s/= >/=>/" calicoscheme.cs
	$(MCS) Scheme.cs calicoscheme.cs ObjectType.cs Rational.cs -target:library -lib:../../../bin -r:Microsoft.Scripting -r:Microsoft.CSharp -r:IronPython.dll -r:Microsoft.Dynamic -r:Calico.exe -pkg:gtk-sharp-2.0 -r:Mono.TextEditor.dll -r:System.Numerics -out:../Calicoscheme.dll

#Scheme.dll: Scheme.cs Makefile
#	$(MCS) -debug Scheme.cs -target:library -lib:../../../bin -r:Microsoft.Scripting -r:IronPython.dll -r:Microsoft.Dynamic -r:Microsoft.VisualBasic.dll -r:Microsoft.Scripting.Core -out:Scheme.dll

calicoscheme.cs: calicoscheme-rm.ss translate_rm.py Scheme.cs
	python translate_rm.py "calicoscheme-rm.ss" "calicoscheme.cs" --noprimitives

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

#calicoscheme.cs: calicoscheme-rm.ss scheme-to-csharp.ss
#	echo "(load \"scheme-to-csharp.ss\")" > make-calicoscheme.ss
#	echo "(delete-file \"calicoscheme.cs\")" >> make-calicoscheme.ss
#	echo "(convert-file \"calicoscheme-rm.ss\" \"calicoscheme.cs\")" >> make-calicoscheme.ss
#	echo "(exit)" >> make-calicoscheme.ss
#	petite make-calicoscheme.ss
#	indent -di0 -i3 calicoscheme.cs 

clean:
	$(RM) *.exe *~ calicoscheme-ds.ss calicoscheme-rm.ss compile-rm.ss compile-ds.ss make-fact.ss \
		fact.cs make-calicoscheme.ss calicoscheme.exe calicoscheme.cs calicoscheme-cps.ss \
		calicoscheme-run.ss calicoscheme.ss fact-rm.ss fact-ds.ss all*.ss testall.ss

