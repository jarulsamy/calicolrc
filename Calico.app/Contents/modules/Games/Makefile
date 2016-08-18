
all: ../Games.dll

MONO_PATH=../../bin:/usr/lib/cli/pango-sharp-2.0:/usr/lib/mono/2.0/:/usr/lib/mono/gtk-sharp-2.0/:/usr/lib/cli/gtk-sharp-2.0/:/usr/lib/cli/gdk-sharp-2.0/:/usr/lib/cli/glib-sharp-2.0

../Games.dll: Games.cs 
	MONO_PATH=$(MONO_PATH) gmcs -target:library \
		Games.cs \
		-lib:../ \
		-r:Graphics.dll \
		-r:System.Drawing \
		-r:Tao.Sdl \
		-r:IronPython \
	        -r:IronRuby \
		-out:../Games.dll \
		-pkg:gtk-sharp-2.0 \
		-r:Mono.Cairo.dll \
		-pkg:glib-sharp-2.0 

clean::
	rm -rf ../Games.dll */*~ *~
