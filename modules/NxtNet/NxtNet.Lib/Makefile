
html: ../../NxtNet.dll
	mcs *.cs /doc:doc.xml -t:library
	mdoc update -i doc.xml -o docfiles ../../NxtNet.dll
	mdoc export-html -o html docfiles
clean:
	rm -rf doc.xml docfiles html Command.dll
