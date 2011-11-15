
all: ../Graphics.dll ../Shapes.dll

../Graphics.dll: Graphics.cs
	gmcs -sdk:4 -target:library Graphics.cs  \
		-lib:../../bin \
		-lib:../modules \
		-r:IronPython \
		-r:Microsoft.Dynamic \
		-r:System.Drawing \
		-r:Mono.Cairo \
		-r:FarseerPhysics.dll \
		-r:GifLib.dll \
		-pkg:gtk-sharp-2.0 \
		-out:../Graphics.dll 

../Shapes.dll: Shapes.cs Graphics.cs
	gmcs -sdk:4 -target:library Shapes.cs  \
		-lib:../../bin \
		-lib:../../modules \
		-r:IronPython \
		-r:Graphics \
		-r:Microsoft.Dynamic \
		-r:System.Drawing \
		-r:Mono.Cairo \
		-r:FarseerPhysics.dll \
		-r:GifLib.dll \
		-pkg:gtk-sharp-2.0 \
		-out:../Shapes.dll 

clean::
	rm -rf ../Graphics.dll ../Shapes.dll *~
