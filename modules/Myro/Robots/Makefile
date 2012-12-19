
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
		-o:Scribbler.dll \
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
		-o:SimScribbler.dll \
		-target:library


NXT.dll: NXT.cs NXTNet.cs MotorPort.cs SensorState.cs Command.cs \
	CommandType.cs SensorType.cs ../Myro.cs 
	dmcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:System.Drawing \
		-pkg:gtk-sharp-2.0 \
		NXT.cs NXTNet.cs MotorPort.cs SensorState.cs Command.cs CommandType.cs \
		SensorType.cs \
		-o:NXT.dll \
		-target:library

