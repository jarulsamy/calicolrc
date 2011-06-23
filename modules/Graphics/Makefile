
all: ../Graphics.dll

MONO_PATH=../../bin 

# Polygon.cs Vector.cs Collision.cs
../Graphics.dll: Graphics.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:library Graphics.cs  \
		-lib:../ \
		-r:IronPython \
		-r:System.Drawing \
		-r:Mono.Cairo.dll \
		-r:FarseerPhysics.dll \
		-r:GifLib.dll \
		-pkg:gtk-sharp-2.0 \
		-out:../Graphics.dll 

clean::
	rm -rf ../Graphics.dll *~
