
all: Scribbler.dll SimScribbler.dll

Scribbler.dll: Scribbler.cs ../Myro.cs
	dmcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:System.Drawing \
		Scribbler.cs \
		-out:Scribbler.dll \
		-target:library

SimScribbler.dll: SimScribbler.cs ../Myro.cs
	dmcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:../../FarseerPhysics.dll \
		-r:System.Drawing \
		SimScribbler.cs \
		-out:SimScribbler.dll \
		-target:library
