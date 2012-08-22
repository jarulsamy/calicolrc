
CalicoScheme.dll: CalicoScheme.cs ../../bin/Calico.exe PJScheme.dll
	mcs -debug CalicoScheme.cs \
		-lib:../../bin \
		-r:PJScheme.dll \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-r:Microsoft.Dynamic \
		-r:Microsoft.VisualBasic \
		-r:Microsoft.Scripting.dll \
		-r:Microsoft.Scripting.Core.dll \
		-r:IronPython.dll \
		-r:Microsoft.Dynamic \
		-r:Mono.TextEditor \
		-target:library

CalicoScheme.exe: CalicoScheme.cs ../../bin/Calico.exe PJScheme.dll
	mcs -debug CalicoScheme.cs \
		-lib:../../bin \
		-r:PJScheme.dll \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-r:Microsoft.Dynamic \
		-r:Microsoft.VisualBasic \
		-r:Microsoft.Scripting.dll \
		-r:Microsoft.Scripting.Core.dll \
		-r:IronPython.dll \
		-r:Microsoft.Dynamic \
		-r:Mono.TextEditor \
		-main:CalicoSchemeEngine \
		-target:exe

clean:
	rm -f *~ Calico*.dll