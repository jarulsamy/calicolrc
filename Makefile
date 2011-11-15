BUILD=2.0.0-alpha
MONO_PATH=bin:/usr/lib/cli/pango-sharp-2.0:/usr/lib/mono/2.0/:/usr/lib/mono/gtk-sharp-2.0/:/usr/lib/cli/gtk-sharp-2.0/:/usr/lib/cli/gdk-sharp-2.0/:/usr/lib/cli/glib-sharp-2.0

all: 	bin/Calico.exe \
	modules/Graphics.dll \
	modules/Myro.dll \
	languages/Python/CalicoPython.dll \
	languages/Ruby/CalicoRuby.dll \
	languages/Jigsaw \
	languages/Scheme/Scheme

bin/Calico.exe: Source/Calico/AssemblyInfo.cs  Source/Calico/Language.cs \
	Source/Calico/CustomStream.cs  Source/Calico/LanguageManager.cs \
	Source/Calico/Document.cs      Source/Calico/Main.cs \
	Source/Calico/Engine.cs        Source/Calico/MainWindow.cs \
	Source/Calico/History.cs       Source/Calico/TabCompletion.cs 
	cd Source && xbuild Calico.sln

languages/Python/CalicoPython.dll: languages/Python/CalicoPython.cs
	cd languages/Python && make

languages/Ruby/CalicoRuby.dll: languages/Ruby/CalicoRuby.cs
	cd languages/Ruby && make

languages/Jigsaw/CalicoJigsaw.dll: languages/Jigsaw/CalicoJigsaw.cs languages/Jigsaw/Jigsaw.exe
	cs languages/Jigsaw && make

languages/Jigsaw/Jigsaw.exe: 
	cd languages/Jigsaw && xbuild Jigsaw.sln

modules/Graphics.dll: modules/Graphics/Graphics.cs modules/FarseerPhysics.dll modules/GifLib.dll
	cd modules/Graphics && make

modules/Shapes.dll: modules/Graphics/Graphics.cs modules/Graphics/Shapes.cs 
	cd modules/Graphics && make

modules/Reflection.dll: modules/Reflection/Reflection.cs 
	cd modules/Reflection && make

modules/Myro.dll: modules/Myro/Myro.cs modules/Graphics.dll modules/FarseerPhysics.dll
	cd modules/Myro && make

modules/Conx.dll: modules/Conx/Conx.cs
	cd modules/Conx && make

modules/Csv.dll: modules/Csv/Csv.cs
	cd modules/Csv && make

modules/ExpressionEngine.dll: modules/ExpressionEngine/Expression.cs
	cd modules/ExpressionEngine && make

modules/Games.dll: modules/Games/Games.cs
	cd modules/Games && make

modules/GifLib.dll: 
	cd modules/GifLib && make

modules/Kinect.dll: modules/Kinnect/Kinnect.cs
	cd modules/Kinnect && make

modules/FarseerPhysics.dll: 
	cd modules/Physics && xbuild "Farseer Physics.csproj"

clean:
	rm -f modules/*.dll
	rm -f bin/Calico.exe
	rm -f languages/Calico*.dll
	rm -f Calico-*.zip
	rm -rf `find | grep "~$$"`

build: clean-build all 
	cd ..; zip -r trunk/Calico-$(BUILD).zip trunk/* -x \*/.svn/\* \*~ 
	scp Calico-$(BUILD).zip dblank@myro.roboteducation.org:html/download/unstable/

.MAKE: $(RECURSIVE_CLEAN_TARGETS) $(RECURSIVE_TARGETS) ctags-recursive \
	install-am install-strip tags-recursive

.PHONY: $(RECURSIVE_CLEAN_TARGETS) $(RECURSIVE_TARGETS) CTAGS GTAGS \
	all all-am am--refresh check check-am clean clean-generic \
	ctags ctags-recursive dist dist-all dist-bzip2 dist-gzip \
	dist-lzma dist-shar dist-tarZ dist-xz dist-zip distcheck \
	distclean distclean-generic distclean-tags distcleancheck \
	distdir distuninstallcheck dvi dvi-am html html-am info \
	info-am install install-am install-binSCRIPTS install-data \
	install-data-am install-dist_pkgdataDATA install-dvi \
	install-dvi-am install-exec install-exec-am install-html \
	install-html-am install-info install-info-am install-man \
	install-pdf install-pdf-am install-ps install-ps-am \
	install-strip installcheck installcheck-am installdirs \
	installdirs-am maintainer-clean maintainer-clean-generic \
	mostlyclean mostlyclean-generic pdf pdf-am ps ps-am tags \
	tags-recursive uninstall uninstall-am uninstall-binSCRIPTS \
	uninstall-dist_pkgdataDATA

