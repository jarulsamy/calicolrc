
../DataStructures.dll: DataStructures.cs
	MONO_PATH=../../bin dmcs -sdk:4 -target:library \
		DataStructures.cs  \
		-lib:../../bin \
		-lib:../ \
		-lib:../../languages/Spreadsheet \
		-r:IronPython \
		-r:Microsoft.Dynamic \
		-r:JigsawAttributes \
		-r:CalicoSpreadsheet \
		-out:../DataStructures.dll 

clean::
	rm -rf ../DataStructures.dll *~
