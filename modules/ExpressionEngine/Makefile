
../ExpressionEngine.dll: ExpressionEngine.cs
	gmcs -sdk:2 -target:library \
		ExpressionEngine.cs \
		-lib:../../bin \
		-r:Microsoft.Scripting \
		-r:IronPython \
		-out:../ExpressionEngine.dll

clean::
	rm -rf ../ExpressionEngine.dll *~
