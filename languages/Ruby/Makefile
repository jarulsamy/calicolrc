
CalicoRuby.dll: CalicoRuby.cs ../../bin/Calico.exe
	gmcs CalicoRuby.cs \
		-lib:../../bin \
		-r:Calico.exe \
		-r:IronRuby \
		-r:Microsoft.Scripting \
		-r:Microsoft.Scripting.Core \
		-target:library

clean:
	rm -f *~ Calico*.dll