
all: ../Graphics.dll ../Shapes.dll

../Graphics.dll: Graphics.cs
	MONO_PATH=../../bin mcs -sdk:4 -target:library \
		Graphics.cs  \
		-lib:../../bin \
		-lib:../ \
                -lib:Graphviz4Net.Core/obj/Release/\
		-r:../../bin/Mono.Cairo.dll \
		-r:IronPython \
		-r:Graphviz4Net.dll\
		-r:Microsoft.Dynamic \
		-r:Graphviz4Net \
		-r:System.Drawing \
		-r:FarseerPhysics.dll \
		-r:GifLib.dll \
		-pkg:gtk-sharp-2.0 \
		-out:../Graphics.dll 

../Shapes.dll: Shapes.cs Graphics.cs
	MONO_PATH=../../bin gmcs -sdk:4 -target:library \
		Shapes.cs  \
		-lib:../../bin \
		-lib:../ \
		-r:../../bin/Mono.Cairo.dll \
		-r:IronPython \
		-r:Graphics \
		-r:Microsoft.Dynamic \
		-r:Graphviz4Net \
		-r:System.Drawing \
		-r:Mono.Cairo \
		-r:FarseerPhysics.dll \
		-r:GifLib.dll \
		-pkg:gtk-sharp-2.0 \
		-out:../Shapes.dll 

clean::
	rm -rf ../Graphics.dll ../Shapes.dll *~
