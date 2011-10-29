
CalicoPython.dll: CalicoPython.cs ../../bin/Calico.exe
	gmcs CalicoPython.cs \
		-lib:../../bin \
		-r:Calico.exe \
		-r:IronPython \
		-r:IronPython.Modules \
		-r:Microsoft.Scripting \
		-r:Microsoft.Scripting.Core \
		-target:library

clean:
	rm -f *~ Calico*.dll