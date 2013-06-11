

all: ../Myro.dll

../Myro.dll: Myro.cs ../Graphics/Graphics.cs
	MONO_PATH=../../bin dmcs -sdk:4 -target:library \
		Myro.cs \
		-lib:.. \
		-unsafe \
		-r:Graphics.dll \
		-r:Tao.Sdl.dll \
		-r:SdlDotNet \
		-r:System.Drawing \
		-r:Microsoft.Dynamic \
		-r:JigsawAttributes \
		-r:IronPython \
	        -r:IronRuby \
		-r:FarseerPhysics.dll \
		-out:../Myro.dll \
		-pkg:gtk-sharp-2.0 \
		-r:Mono.Cairo.dll \
		-pkg:glib-sharp-2.0 
	cd Robots; make

clean::
	rm -rf ../Myro.dll */*~ *~
