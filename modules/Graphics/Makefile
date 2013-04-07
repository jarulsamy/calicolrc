
all: ../Graphics.dll ../Shapes.dll

../Graphics.dll: Graphics.cs
	MONO_PATH=../../bin dmcs -sdk:4 -target:library \
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
		-pkg:gtk-sharp-2.0 \
		-pkg:gtk-dotnet-2.0 \
		-out:../Graphics.dll

../Shapes.dll: Shapes.cs Graphics.cs
	MONO_PATH=../../bin dmcs -sdk:4 -target:library \
		Shapes.cs  \
		-lib:../../bin \
		-lib:../ \
		-r:../../bin/Mono.Cairo.dll \
		-r:IronPython \
		-r:Graphics \
		-r:Myro \
		-r:Microsoft.Dynamic \
		-r:Graphviz4Net \
		-r:System.Drawing \
		-r:Mono.Cairo \
		-r:FarseerPhysics.dll \
		-pkg:gtk-sharp-2.0 \
		-pkg:gtk-dotnet-2.0 \
		-out:../Shapes.dll 

clean::
	rm -rf ../Graphics.dll ../Shapes.dll *~
