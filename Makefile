BUILD=2.0.0-alpha
MONO_PATH=bin:/usr/lib/cli/pango-sharp-2.0:/usr/lib/mono/2.0/:/usr/lib/mono/gtk-sharp-2.0/:/usr/lib/cli/gtk-sharp-2.0/:/usr/lib/cli/gdk-sharp-2.0/:/usr/lib/cli/glib-sharp-2.0

all: modules/Graphics.dll modules/Myro.dll languages/Python/CalicoPython.dll \
	modules/Conx.dll bin/Calico.exe languages/Ruby/CalicoRuby.dll 

build: clean-build all 
	cd ..; zip -r trunk/Calico-$(BUILD).zip trunk/* -x \*/.svn/\* \*~ 
	scp Calico-$(BUILD).zip dblank@myro.roboteducation.org:html/download/unstable/

modules/Graphics.dll:
	cd modules/Graphics; make

modules/Conx.dll:
	cd modules/Conx; make

modules/Myro.dll: 
	cd modules/Myro; make ../Myro.dll

languages/Python/CalicoPython.dll: bin/Calico.exe languages/Python/CalicoPython.cs
	cd languages/Python; make

languages/Ruby/CalicoRuby.dll: bin/Calico.exe languages/Ruby/CalicoRuby.cs
	cd languages/Ruby; make

languages/Scheme/Scheme/PJScheme.dll:
	cd languages/Scheme/Scheme; make

clean-build:
	rm -f Calico-*.zip
	rm -rf `find | grep "~$$"`

bin/Calico.exe:
	cd Source; xbuild Calico.sln

bin/calico.exe: bin/calico.cs
	MONO_PATH=$(MONO_PATH) gmcs -target:exe \
		bin/calico.cs \
		-r:IronPython.dll \
		-r:Microsoft.Scripting \
		-r:Microsoft.Dynamic \
		-r:Mono.Posix \
		/win32icon:examples/images/blueslug.ico \
		-out:bin/calico.exe 

clean:
	rm -f modules/Graphics.dll modules/Myro.dll modules/Conx.dll bin/Calico.exe \
		languages/Scheme/Scheme/PJScheme.dll Calico*.zip languages/*/Calico*.dll

