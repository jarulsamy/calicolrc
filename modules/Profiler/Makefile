PYTHON = python
MONO = mono
IPY = ../../bin/ipy.exe

PYPROF = $(PYTHON) prof.py
IPYPROF = LD_LIBRARY_PATH=. $(MONO) --profile=py $(IPY) -S

SRC = py.c
OBJ = libmono-profiler-py.so

DLLSRC = Profiler.cs
DLL = ../Profiler.dll

.PHONY: all clean shell test testpy

all: $(OBJ) $(DLL)

$(OBJ): $(SRC)
	gcc -g -shared -fPIC -o $(OBJ) $(SRC) `pkg-config --cflags --libs mono`

$(DLL): $(DLLSRC)
	mcs -t:library -out:$(DLL) $(DLLSRC) -r:Mono.Posix.dll

clean:
	rm -f $(OBJ) $(DLL)

testpy:
	$(PYPROF) test.py

shell: all
	$(IPYPROF)

test: all
	$(IPYPROF) test.py

test.out: all
	$(IPYPROF) test.py > test.out
