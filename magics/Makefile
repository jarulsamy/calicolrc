

Magics.dll: Magics.cs ../bin/Calico.exe
	dmcs Magics.cs -target:library -lib:../bin/ \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-r:Mono.Cairo.dll \
		-pkg:glib-sharp-2.0 

clean::
	rm -rf *~ Magics.dll
