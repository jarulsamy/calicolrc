all: ../Myro.dll Robots

../Myro.dll: Myro.cs ../Graphics/Graphics.cs 
	MONO_PATH=../../bin mcs -sdk:4 -target:library \
        Myro.cs \
        -lib:.. \
        -lib:../../bin \
        -unsafe \
        -r:Calico.exe \
        -r:Graphics.dll \
        -r:Events.dll \
        -r:Tao.Sdl.dll \
        -r:SdlDotNet \
        -r:System.Drawing \
        -r:Microsoft.Dynamic \
        -r:JigsawAttributes \
        -r:IronPython \
        -r:IronRuby \
        -r:System.Numerics \
        -r:FarseerPhysics.dll \
        -out:../Myro.dll \
        -pkg:gtk-sharp-2.0 \
        -r:Mono.Cairo.dll \
        -pkg:glib-sharp-2.0 

Robots: 
	cd Robots; make

clean::
	rm -rf ../Myro.dll */*~ *~
