
CalicoRuby.dll: CalicoRuby.cs ../../bin/Calico.exe
	dmcs CalicoRuby.cs \
		-lib:../../bin \
		-r:Calico.exe \
		-r:IronRuby \
		-r:Microsoft.Scripting \
		-r:Microsoft.Scripting.Core \
		-r:Mono.TextEditor \
		-pkg:gtk-sharp-2.0 \
		-target:library

clean:
	rm -f *~ Calico*.dll