
MONO_PATH=../../bin

../Csv.dll: Csv.cs 
	MONO_PATH=$(MONO_PATH) dmcs \
		-target:library Csv.cs \
		-out:../Csv.dll \
		-lib:../ -lib:../../bin \
		-r:IronPython.dll 

clean::
	rm -rf ../Csv.dll *~
