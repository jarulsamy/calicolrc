
all: ../Myro.dll

../Myro.dll: Myro.cs ../Graphics/Graphics.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:library \
		Myro.cs \
		-lib:../../bin \
		-lib:../../modules \
		-unsafe \
		-r:Graphics.dll \
		-r:Tao.Sdl.dll \
		-r:SdlDotNet \
		-r:System.Drawing \
		-r:IronPython \
	        -r:IronRuby \
		-r:FarseerPhysics.dll \
		-out:../Myro.dll \
		-pkg:gtk-sharp-2.0 \
		-r:Mono.Cairo.dll \
		-pkg:glib-sharp-2.0 

clean::
	rm -rf ../Myro.dll */*~ *~
