MCS=MONO_PATH=../../../bin:../../../modules mcs -sdk:2

../PJScheme.dll: pjscheme.cs Scheme.cs pjscheme-rm.ss Makefile
	$(MCS) -debug Scheme.cs pjscheme.cs -target:library -lib:../../../bin -r:Microsoft.Scripting -r:IronPython.dll -r:Microsoft.Dynamic -r:Microsoft.VisualBasic.dll -r:Microsoft.Scripting.Core -out:../PJScheme.dll

pjscheme-ds.ss: reader-cps.ss environments-cps.ss parser-cps.ss \
		interpreter-cps.ss unifier-cps.ss ds-transformer.ss
	cat reader-cps.ss environments-cps.ss parser-cps.ss interpreter-cps.ss unifier-cps.ss > pjscheme-cps.ss
	echo "(load \"ds-transformer.ss\")" > compile-ds.ss
	echo "(delete-file \"pjscheme-ds.ss\")" >> compile-ds.ss
	echo "(ds-transform-file \"pjscheme-cps.ss\" \"pjscheme-ds.ss\")" >> compile-ds.ss
	echo "(exit)" >> compile-ds.ss
	petite compile-ds.ss

pjscheme-rm.ss: pjscheme-ds.ss rm-transformer.ss
	echo "(load \"rm-transformer.ss\")" > compile-rm.ss
	echo "(delete-file \"pjscheme-rm.ss\")" >> compile-rm.ss
	echo "(compile-level-output)" >> compile-rm.ss
	echo "(rm-transform-file \"pjscheme-ds.ss\" \"pjscheme-rm.ss\")" >> compile-rm.ss
	echo "(exit)" >> compile-rm.ss
	petite compile-rm.ss

run: pjscheme-rm.ss no-csharp-support.ss
	echo "(case-sensitive #t)" > pjscheme-run.ss
	echo "(load \"pjscheme-rm.ss\")" >> pjscheme-run.ss
	echo "(load \"no-csharp-support.ss\")" >> pjscheme-run.ss
	echo "(begin (display (run start)) (newline))" >> pjscheme-run.ss
	petite pjscheme-run.ss

pjscheme.cs: pjscheme-rm.ss scheme-to-csharp.ss
	echo "(load \"scheme-to-csharp.ss\")" > make-pjscheme.ss
	echo "(delete-file \"pjscheme.cs\")" >> make-pjscheme.ss
	echo "(convert-file \"pjscheme-rm.ss\" \"pjscheme.cs\")" >> make-pjscheme.ss
	echo "(exit)" >> make-pjscheme.ss
	petite make-pjscheme.ss
	indent -di0 -i3 pjscheme.cs &> /dev/null

clean:
	$(RM) *.exe *~ pjscheme-ds.ss pjscheme-rm.ss compile-rm.ss compile-ds.ss make-fact.ss \
		fact.cs make-pjscheme.ss pjscheme.exe pjscheme.cs pjscheme-cps.ss \
		pjscheme-run.ss pjscheme.ss fact-rm.ss fact-ds.ss all*.ss testall.ss

