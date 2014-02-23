

CalicoFSharp.dll: CalicoFSharp.cs
	dmcs -sdk:2 -lib:../../bin -lib:../../modules/ -pkg:gtk-sharp-2.0 -r:FSharp.Core.dll -r:FSharp.Compiler.Service.dll -r:Calico.exe CalicoFSharp.cs /target:library
#	mcs -lib:../../bin -lib:../../modules/ -pkg:gtk-sharp-2.0 -r:FSharp.Core.dll -r:FSharp.Compiler.Service.dll -r:Calico.exe CalicoFSharp.cs /target:library
#	mcs -lib:../../bin -pkg:gtk-sharp-2.0 -r:Fsharp.Compiler.CodeDom.dll -r:calicofsharpmod.dll -r:Calico.exe CalicoFSharp.cs /target:library


FSharpShell.dll: FSharpShell.cs
	gmcs FSharpShell.cs /target:library

