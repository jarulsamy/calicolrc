
all: Bioloid.dll Scribbler.dll SimScribbler.dll SimPioneer.dll NxtRobot.dll \
	Finch.dll Hummingbird.dll Arduino.dll SimScribblerRounded.dll \

Arduino.dll: Arduino.cs 
	MONO_PATH=../../bin:../../../languages/Java/lib dmcs -sdk:4 \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-out:Arduino.dll \
		-target:library \
		Arduino.cs

ROSARDrone.dll: ROSARDrone.cs ROSRobot.dll
	MONO_PATH=../../bin:../../../languages/Java/lib dmcs -sdk:4 \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:System.Drawing \
		-r:../../ros.dll \
		-r:../../MyROS.dll \
		-r:ROSRobot.dll \
		-r:IKVM.OpenJDK.Beans \
		-r:IKVM.OpenJDK.Charsets \
		-r:IKVM.OpenJDK.Corba \
		-r:IKVM.OpenJDK.Core \
		-r:IKVM.OpenJDK.Jdbc \
		-r:IKVM.OpenJDK.Management \
		-r:IKVM.OpenJDK.Media \
		-r:IKVM.OpenJDK.Misc \
		-r:IKVM.OpenJDK.Naming \
		-r:IKVM.OpenJDK.Remoting \
		-r:IKVM.OpenJDK.Security \
		-r:IKVM.OpenJDK.SwingAWT \
		-r:IKVM.OpenJDK.Text \
		-r:IKVM.OpenJDK.Tools \
		-r:IKVM.OpenJDK.Util \
		-r:IKVM.OpenJDK.XML.API \
		-r:IKVM.OpenJDK.XML.Bind \
		-r:IKVM.OpenJDK.XML.Crypto \
		-r:IKVM.OpenJDK.XML.Parse \
		-r:IKVM.OpenJDK.XML.Transform \
		-r:IKVM.OpenJDK.XML.WebServices \
		-r:IKVM.OpenJDK.XML.XPath \
		-r:IKVM.Reflection \
		-r:IKVM.Runtime \
		-r:IKVM.Runtime.JNI\
		-pkg:gtk-sharp-2.0 \
		-out:ROSARDrone.dll \
		-target:library \
		ROSARDrone.cs

ROSRobot.dll: ROSRobot.cs
	MONO_PATH=../../bin:../../../languages/Java/lib dmcs -sdk:4 \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:System.Drawing \
		-r:../../MyROS.dll \
		-r:../../ros.dll \
		-r:IKVM.OpenJDK.Beans \
		-r:IKVM.OpenJDK.Charsets \
		-r:IKVM.OpenJDK.Corba \
		-r:IKVM.OpenJDK.Core \
		-r:IKVM.OpenJDK.Jdbc \
		-r:IKVM.OpenJDK.Management \
		-r:IKVM.OpenJDK.Media \
		-r:IKVM.OpenJDK.Misc \
		-r:IKVM.OpenJDK.Naming \
		-r:IKVM.OpenJDK.Remoting \
		-r:IKVM.OpenJDK.Security \
		-r:IKVM.OpenJDK.SwingAWT \
		-r:IKVM.OpenJDK.Text \
		-r:IKVM.OpenJDK.Tools \
		-r:IKVM.OpenJDK.Util \
		-r:IKVM.OpenJDK.XML.API \
		-r:IKVM.OpenJDK.XML.Bind \
		-r:IKVM.OpenJDK.XML.Crypto \
		-r:IKVM.OpenJDK.XML.Parse \
		-r:IKVM.OpenJDK.XML.Transform \
		-r:IKVM.OpenJDK.XML.WebServices \
		-r:IKVM.OpenJDK.XML.XPath \
		-r:IKVM.Reflection \
		-r:IKVM.Runtime \
		-r:IKVM.Runtime.JNI\
		-pkg:gtk-sharp-2.0 \
		-out:ROSRobot.dll \
		-target:library \
		ROSRobot.cs

Finch.dll: Finch.cs
	mcs -sdk:4 -lib:../../../bin \
		-lib:../../../bin/linux \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:HidSharp.dll \
		-r:System.Drawing \
		-pkg:gtk-sharp-2.0 \
		-out:Finch.dll \
		-r:System.Numerics \
		-target:library \
		$^ 

Hummingbird.dll: Hummingbird.cs Finch.cs
	mcs -sdk:4 -lib:../../../bin \
		-lib:../../../bin/linux \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:Finch.dll \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:HidSharp.dll \
		-r:System.Drawing \
		-pkg:gtk-sharp-2.0 \
		-out:Hummingbird.dll \
		-target:library \
		Hummingbird.cs

NxtRobot.dll: NxtRobot/NxtRobot.cs NxtRobot/NxtNet.cs NxtRobot/MotorPort.cs \
	NxtRobot/SensorState.cs NxtRobot/Command.cs \
	NxtRobot/CommandType.cs NxtRobot/SensorType.cs \
	NxtRobot/DeviceInfo.cs NxtRobot/SensorPort.cs \
	NxtRobot/SensorMode.cs \
	NxtRobot/MotorModes.cs NxtRobot/MotorRegulationMode.cs \
	NxtRobot/MotorRunState.cs NxtRobot/MotorState.cs \
	NxtRobot/Error.cs NxtRobot/Extensions.cs NxtRobot/Version.cs 
	mcs -sdk:4 -lib:../../bin -lib:../../modules \
		-r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
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
	mcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:System.Drawing \
		-r:System.Numerics \
		Scribbler.cs \
		-out:Scribbler.dll \
		-target:library

Bioloid.dll: Bioloid.cs ../Myro.cs
	mcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:System.Drawing \
		-r:System.Numerics \
		Bioloid.cs \
		-out:Bioloid.dll \
		-target:library

SimScribbler.dll: SimScribbler.cs ../Myro.cs
	mcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:../../FarseerPhysics.dll \
		-r:System.Drawing \
		-pkg:gtk-sharp-2.0 \
		-r:System.Numerics \
		SimScribbler.cs \
		-out:SimScribbler.dll \
		-target:library

SimScribblerRounded.dll: SimScribblerRounded.cs ../Myro.cs
	mcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../../bin/Calico.exe \
		-r:../../Graphics.dll \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:../../FarseerPhysics.dll \
		-r:System.Drawing \
		-pkg:gtk-sharp-2.0 \
		-r:System.Numerics \
		SimScribblerRounded.cs \
		-out:SimScribblerRounded.dll \
		-target:library

SimPioneer.dll: SimPioneer.cs ../Myro.cs
	mcs -sdk:4 -r:../../../bin/IronPython.dll \
		-r:../../Myro.dll \
		-r:../../Graphics.dll \
		-r:../../../bin/Calico.exe \
		-r:../../../bin/IronPython.dll \
		-r:../../../bin/Microsoft.Scripting.dll \
		-r:../../../bin/Microsoft.Dynamic.dll \
		-r:../../FarseerPhysics.dll \
		-pkg:gtk-sharp-2.0 \
		-r:System.Drawing \
		-r:System.Numerics \
		SimPioneer.cs \
		-out:SimPioneer.dll \
		-target:library

clean:
	rm -rf *~ *.dll

