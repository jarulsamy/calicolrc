JAVACC_HOME=/usr/share/java/
JAVA6_HOME=/usr/lib/jvm/java-6-openjdk-amd64/
JAVA5_HOME=/usr/lib/jvm/java-6-openjdk-amd64/

../lib/dynamicjava.dll: dynamicjava.jar
	../lib/ikvmc.exe -target:library dynamicjava.jar -out:../lib/dynamicjava.dll

dynamicjava.jar:
	ant compile
	ant jar

clean::
	ant clean
	rm -rf ../lib/dynamicjava.dll *~


