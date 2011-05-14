
all: ../Graphics.dll

MONO_PATH=../../bin

# Polygon.cs Vector.cs Collision.cs
../Graphics.dll: Graphics.cs
	MONO_PATH=$(MONO_PATH) gmcs -d:DEBUG -target:library Graphics.cs  -r:IronPython \
		-r:System.Drawing \
		-out:../Graphics.dll -pkg:gtk-sharp-2.0 -r:Mono.Cairo.dll 

clean::
	rm -rf ../Graphics.dll *~
