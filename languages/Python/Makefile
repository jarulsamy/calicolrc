
CalicoPython.dll: CalicoPython.cs ../../bin/Calico.exe
	mcs CalicoPython.cs \
		-lib:../../bin \
		-r:Calico.exe \
		-r:IronPython \
		-r:IronPython.Modules \
		-r:Microsoft.Scripting \
		-r:Microsoft.Scripting.Core \
		-r:Microsoft.Dynamic \
		-r:Mono.TextEditor \
		-pkg:gtk-sharp-2.0 \
		-target:library

clean:
	rm -f *~ Calico*.dll