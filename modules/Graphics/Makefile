
all: ../Graphics.dll ../Shapes.dll

../Graphviz4Net.dll: Graphviz4Net.Core/bin/Release/Graphviz4Net.dll
	cp Graphviz4Net.Core/bin/Release/Graphviz4Net.dll ../Graphviz4Net.dll

../Graphics.dll: Graphics.cs ../Graphviz4Net.dll
	MONO_PATH=../../bin dmcs -debug -sdk:4 -target:library \
		Graphics.cs  \
		-lib:../../bin \
		-lib:../ \
		-r:Mono.Cairo.dll \
		-r:Events.dll \
		-r:IronPython \
		-r:Graphviz4Net.dll\
		-r:Microsoft.Dynamic \
		-r:Graphviz4Net \
		-r:System.Drawing \
		-r:FarseerPhysics.dll \
		-r:JigsawAttributes \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-pkg:gtk-dotnet-2.0 \
		-out:../Graphics.dll

../Shapes.dll: Shapes.cs ../Graphics.dll 
	MONO_PATH=../../bin dmcs -sdk:4 -target:library \
		Shapes.cs  \
		-lib:../../bin \
		-lib:../ \
		-r:Events.dll \
		-r:Mono.Cairo.dll \
		-r:IronPython \
		-r:Graphics \
		-r:Myro \
		-r:Microsoft.Dynamic \
		-r:Graphviz4Net \
		-r:System.Drawing \
		-r:Mono.Cairo \
		-r:FarseerPhysics.dll \
		-r:JigsawAttributes \
		-r:Calico.exe \
		-pkg:gtk-sharp-2.0 \
		-pkg:gtk-dotnet-2.0 \
		-out:../Shapes.dll 

clean::
	rm -rf ../Graphics.dll ../Shapes.dll *~
