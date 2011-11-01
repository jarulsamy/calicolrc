
all: ../Graphics.dll

../Graphics.dll: Graphics.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:library Graphics.cs  \
		-lib:../../bin \
		-lib:../../modules \
		-r:IronPython \
		-r:Microsoft.Dynamic \
		-r:System.Drawing \
		-r:Mono.Cairo \
		-r:FarseerPhysics.dll \
		-r:GifLib.dll \
		-pkg:gtk-sharp-2.0 \
		-out:../Graphics.dll 

clean::
	rm -rf ../Graphics.dll *~
