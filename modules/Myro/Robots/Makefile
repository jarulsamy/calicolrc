
all: Scribbler.dll SimScribbler.dll SimPioneer.dll NxtRobot.dll

NxtRobot.dll: NxtRobot/NxtRobot.cs NxtRobot/NxtNet.cs NxtRobot/MotorPort.cs \
	NxtRobot/SensorState.cs NxtRobot/Command.cs \
	NxtRobot/CommandType.cs NxtRobot/SensorType.cs \
	NxtRobot/DeviceInfo.cs NxtRobot/SensorPort.cs \
	NxtRobot/SensorMode.cs \
	NxtRobot/MotorModes.cs NxtRobot/MotorRegulationMode.cs \
	NxtRobot/MotorRunState.cs NxtRobot/MotorState.cs \
	NxtRobot/Error.cs NxtRobot/Extensions.cs NxtRobot/Version.cs 
	dmcs -sdk:4 -lib:../../bin -lib:../../modules \
		-r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:System.Drawing \
		-pkg:gtk-sharp-2.0 \
		-out:NxtRobot.dll \
		-target:library \
		$^ 

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

SimPioneer.dll: SimPioneer.cs ../Myro.cs
	dmcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:../../FarseerPhysics.dll \
		-r:System.Drawing \
		SimPioneer.cs \
		-out:SimPioneer.dll \
		-target:library
