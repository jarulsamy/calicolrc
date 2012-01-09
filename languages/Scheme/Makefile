
CalicoScheme.dll: CalicoScheme.cs ../../bin/Calico.exe
	gmcs -debug CalicoScheme.cs \
		-lib:../../bin \
		-r:PJScheme.dll \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-r:Microsoft.VisualBasic \
		-r:Microsoft.Scripting.dll \
		-r:Microsoft.Scripting.Core.dll \
		-r:IronPython.dll \
		-r:Microsoft.Dynamic \
		-r:Mono.TextEditor \
		-target:library

clean:
	rm -f *~ Calico*.dll