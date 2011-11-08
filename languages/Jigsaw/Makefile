
CalicoJigsaw.dll: CalicoJigsaw.cs ../../bin/Calico.exe
	gmcs CalicoJigsaw.cs \
		-lib:Jigsaw/obj/x86/Debug \
		-lib:../../bin \
		-r:Jigsaw.exe \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-target:library

clean:
	rm -f *~ Calico*.dll