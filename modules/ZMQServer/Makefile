

../ZMQServer.dll: ZMQServer.cs
	dmcs -r:../clrzmq.dll -r:../Newtonsoft.Json.dll ZMQServer.cs -out:../ZMQServer.dll -t:library

ZMQServer.exe: ZMQServer.cs
	dmcs -debug -r:../clrzmq.dll -r:../Newtonsoft.Json.dll ZMQServer.cs
