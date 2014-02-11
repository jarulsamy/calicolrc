../lib/dynamicjava.dll: ../lib/dynamicjava.jar 
	cd ../lib/; mono ./ikvmc.exe -target:library dynamicjava.jar

../lib/dynamicjava.jar:
	ant dist
	cp ./DynamicJava-1.1.5/lib/dynamicjava.jar ../lib/

clean::
	rm -rf ../lib/dynamicjava.jar ../lib/dynamicjava.dll
