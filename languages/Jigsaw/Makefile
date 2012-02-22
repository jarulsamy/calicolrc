
CalicoJigsaw.dll: CalicoJigsaw.cs ../../bin/Calico.exe
	dmcs -sdk:4 CalicoJigsaw.cs \
		-lib:../../bin \
		-r:Jigsaw.exe \
		-r:Calico.exe \
		-r:Microsoft.Dynamic \
		-pkg:gtk-sharp-2.0 \
		-target:library

clean:
	rm -f *~ Calico*.dll