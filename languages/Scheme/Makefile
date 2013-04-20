
CalicoScheme.dll: CalicoScheme.cs ../../bin/Calico.exe PJScheme.dll getline.cs
	dmcs -debug CalicoScheme.cs getline.cs \
		-lib:../../bin \
		-r:PJScheme.dll \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-r:Microsoft.Dynamic \
		-r:Microsoft.Scripting.dll \
		-r:Microsoft.Scripting.Core.dll \
		-r:IronPython.dll \
		-r:Microsoft.Dynamic \
		-r:Mono.TextEditor \
		-target:library

CalicoScheme.exe: CalicoScheme.cs ../../bin/Calico.exe PJScheme.dll getline.cs
	dmcs -debug CalicoScheme.cs getline.cs \
		-lib:../../bin \
		-r:PJScheme.dll \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-r:Microsoft.Dynamic \
		-r:Microsoft.Scripting.dll \
		-r:Microsoft.Scripting.Core.dll \
		-r:IronPython.dll \
		-r:Microsoft.Dynamic \
		-r:Mono.TextEditor \
		-main:CalicoSchemeEngine \
		-target:exe

clean:
	rm -f *~ Calico*.dll