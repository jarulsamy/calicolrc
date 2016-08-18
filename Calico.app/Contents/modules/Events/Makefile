
all: ../Events.dll 

../Events.dll: Events.cs
	MONO_PATH=../../bin dmcs -sdk:4 -target:library \
		Events.cs  \
		-lib:../../bin \
		-lib:../ \
		-r:Calico.exe \
		-r:Microsoft.Dynamic \
		-r:JigsawAttributes \
		-pkg:gtk-sharp-2.0 \
		-out:../Events.dll

clean::
	rm -rf ../Events.dll *~
