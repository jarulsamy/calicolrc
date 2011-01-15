
all: ../Myro.dll

MONO_PATH=../../bin:/usr/lib/cli/pango-sharp-2.0:/usr/lib/mono/2.0/:/usr/lib/mono/gtk-sharp-2.0/:/usr/lib/cli/gtk-sharp-2.0/:/usr/lib/cli/gdk-sharp-2.0/:/usr/lib/cli/glib-sharp-2.0

../Myro.dll: Myro.cs ../Graphics/Graphics.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:library \
		Myro.cs \
		-lib:../ \
		-r:Graphics.dll \
		-r:IronPython -out:../Myro.dll \
		-pkg:gtk-sharp-2.0 \
		-r:Mono.Cairo.dll \
		-pkg:glib-sharp-2.0 

clean::
	rm -rf ../Myro.dll */*~ *~
