
CSHARP = ../Source/Calico/Main.cs \
	 ../Source/Calico/MainWindow.cs \
	 ../Source/Calico/AssemblyInfo.cs \
	 ../Source/Calico/Chat.cs \
	 ../Source/Calico/Config.cs \
	 ../Source/Calico/CustomStream.cs \
	 ../Source/Calico/DebugTextMarker.cs \
	 ../Source/Calico/Document.cs \
	 ../Source/Calico/Engine.cs \
	 ../Source/Calico/History.cs \
	 ../Source/Calico/Language.cs \
	 ../Source/Calico/LanguageManager.cs \
	 ../Source/Calico/Printing.cs \
	 ../Source/Calico/TabCompletion.cs \

all: es/LC_MESSAGES/calico.mo fr/LC_MESSAGES/calico.mo

es/LC_MESSAGES/calico.mo: es.po
	msgfmt es.po -o es/LC_MESSAGES/calico.mo

fr/LC_MESSAGES/calico.mo: fr.po
	msgfmt fr.po -o fr/LC_MESSAGES/calico.mo

calico.pot: $(CSHARP)
	touch calico.pot
	xgettext --keyword=_ --language=C# --from-code=UTF-8 $(CSHARP) -j -o calico.pot

clean:
	rm -f fr/LC_MESSAGES/calico.mo es/LC_MESSAGES/calico.mo