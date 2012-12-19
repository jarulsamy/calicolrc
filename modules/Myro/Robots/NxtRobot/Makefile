../NxtRobot.dll: NxtRobot.cs NxtNet.cs MotorPort.cs SensorState.cs Command.cs \
	CommandType.cs SensorType.cs DeviceInfo.cs SensorPort.cs SensorMode.cs \
	MotorModes.cs MotorRegulationMode.cs MotorRunState.cs MotorState.cs \
	Error.cs Extensions.cs Version.cs
	dmcs -sdk:4 -lib:../../../bin -lib:../../../modules \
		-r:../../../../bin/IronPython.dll \
		-r:../../../Myro.dll \
		-r:../../../Graphics.dll \
		-r:../../../../bin/IronPython.dll \
		-r:../../../../bin/Microsoft.Scripting.dll \
		-r:../../../../bin/Microsoft.Dynamic.dll \
		-r:System.Drawing \
		-pkg:gtk-sharp-2.0 \
		-out:../NxtRobot.dll \
		-target:library \
		$^ 

clean::
	rm -rf ../NxtRobot.dll *.dll *~