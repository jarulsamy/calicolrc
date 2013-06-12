

all: ../MyROS.dll

../MyROS.dll: MyROS.cs 
	MONO_PATH=../../bin:../../languages/Java/lib dmcs -sdk:4 -target:library \
		MyROS.cs \
		-lib:.. \
		-unsafe \
		-r:ros \
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
		-r:IKVM.Runtime.JNI \
		-out:../MyROS.dll \

clean::
	rm -rf ../MyROS.dll */*~ *~
