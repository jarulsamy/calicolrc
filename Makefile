BUILD=0.1.5

all: modules/Graphics.dll modules/Myro.dll languages/PJScheme.dll \
	modules/Conx.dll

build: clean-build all 
	cd ..; zip -r Pyjama/Pyjama-$(BUILD).zip Pyjama/* -x \*/.svn/\* \*~ 
	scp Pyjama-$(BUILD).zip dblank@myro.roboteducation.org:html/download/

modules/Graphics.dll:
	cd modules/Graphics; make

modules/Conx.dll:
	cd modules/Conx; make

modules/Myro.dll: 
	cd modules/Myro; make ../Myro.dll

languages/PJScheme.dll:
	cd languages/Scheme; make

clean-build:
	rm -f Pyjama-*.zip *~

clean:
	rm -f modules/Graphics.dll modules/Myro.dll modules/Conx.dll \
		languages/PJScheme.dll Pyjama*.zip

# mono ../bin/ipy.exe ../bin/pyc.py *.py /main:__main__.py /target:exe
# ISSUES:
# needed to have main file named __main__.py
# needed to have files in directory?
# needed to have everything in same dir?