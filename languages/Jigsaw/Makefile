
CalicoJigsaw.dll: CalicoJigsaw.cs ../../bin/Calico.exe
	dmcs -sdk:4 CalicoJigsaw.cs \
		Jigsaw/Printing.cs \
		-lib:../../bin \
		-r:Jigsaw.exe \
		-r:Calico.exe \
		-r:Microsoft.Dynamic \
		-r:Mono.Cairo.dll \
		-pkg:gtk-sharp-2.0 \
		-target:library

clean:
	rm -f *~ Calico*.dll