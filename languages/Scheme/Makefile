
CalicoScheme.dll: CalicoScheme.cs ../../bin/Calico.exe
	gmcs -sdk:2 CalicoScheme.cs \
		-lib:../../bin \
		-r:PJScheme.dll \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-r:Microsoft.VisualBasic \
		-r:Microsoft.Scripting.dll \
		-r:IronPython.dll \
		-r:Microsoft.Dynamic \
		-target:library

clean:
	rm -f *~ Calico*.dll