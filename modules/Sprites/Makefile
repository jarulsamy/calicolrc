

all: ../Sprites.dll

../Sprites.dll: Sprites.cs 
	MONO_PATH=../../bin dmcs -sdk:4 -target:library \
		Sprites.cs \
		-lib:../../bin \
		-lib:.. \
		-unsafe \
		-r:Events.dll \
		-r:Graphics.dll \
		-r:Myro.dll \
		-r:Tao.Sdl.dll \
		-r:SdlDotNet \
		-r:System.Drawing \
		-r:Microsoft.Dynamic \
		-r:IronPython \
	        -r:IronRuby \
		-r:System.Numerics \
		-r:FarseerPhysics.dll \
		-out:../Sprites.dll \
		-pkg:gtk-sharp-2.0 \
		-r:Mono.Cairo.dll \
		-r:JigsawAttributes.dll \
		-pkg:glib-sharp-2.0 

clean::
	rm -rf ../Sprites.dll */*~ *~
