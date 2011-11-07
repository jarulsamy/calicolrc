
CalicoJigsaw.dll: CalicoJigsaw.cs ../../bin/Calico.exe
	gmcs CalicoJigsaw.cs \
		-lib:../../bin \
		-lib:Jigsaw/bin/Debug \
		-r:Jigsaw.exe \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-target:library

clean:
	rm -f *~ Calico*.dll