

Magics.dll: Magics.cs ../bin/Calico.exe
	dmcs Magics.cs -target:library -lib:../bin/ -lib:../modules/ \
		-r:Calico.exe \
		-r:Graphics.dll \
		-pkg:gtk-sharp-2.0 \
		-r:Mono.Cairo.dll \
		-pkg:glib-sharp-2.0 

clean::
	rm -rf *~ Magics.dll
