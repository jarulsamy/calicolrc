
../Finch.dll: Finch.cs
	dmcs -sdk:4 -lib:../../../../bin \
		-r:../../../Myro.dll \
		-r:../../../Graphics.dll \
		-r:../../../../bin/IronPython.dll \
		-r:../../../../bin/Microsoft.Scripting.dll \
		-r:../../../../bin/Microsoft.Dynamic.dll \
		-r:HidSharp.dll \
		-r:System.Drawing \
		-pkg:gtk-sharp-2.0 \
		-out:../Finch.dll \
		-target:library \
		$^ 

clean:
	rm -rf *~ ../Finch.dll
