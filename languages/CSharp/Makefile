
CalicoCSharp.dll: CalicoCSharp.cs ../../bin/Calico.exe
	dmcs -sdk:4 CalicoCSharp.cs \
		-lib:. \
		-lib:../../bin \
		-r:Calico.exe \
		-r:Mono.CSharp.dll \
		-pkg:gtk-sharp-2.0 \
		-r:Microsoft.Dynamic \
		-r:Microsoft.Scripting.dll \
		-r:Microsoft.Scripting.Core.dll \
		-r:IronPython.dll \
		-r:Microsoft.Dynamic \
		-r:Mono.TextEditor \
		-target:library

clean:
	rm -f *~ Calico*.dll