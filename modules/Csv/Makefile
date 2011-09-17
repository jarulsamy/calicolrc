
MONO_PATH=../../bin

../Csv.dll: Csv.cs 
	MONO_PATH=$(MONO_PATH) gmcs \
		-target:library Csv.cs \
		-out:../Csv.dll \
		-lib:../ \
		-r:IronPython \

clean::
	rm -rf ../Csv.dll *~
