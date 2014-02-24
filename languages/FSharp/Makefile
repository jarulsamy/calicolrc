

CalicoFSharp.dll: CalicoFSharp.cs
	dmcs -sdk:2 -lib:../../bin -lib:../../modules/ -pkg:gtk-sharp-2.0 -r:FSharp.Core.dll -r:FSharp.Compiler.Service.dll -r:Calico.exe CalicoFSharp.cs /target:library

clean::
	rm -rf CalicoFSharp.dll *~
