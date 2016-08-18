

# Where is IronPython.dll:
MONO_PATH=../../bin
MONO_PATH=../../bin:/usr/lib/mono/gtk-sharp-2.0/:/usr/lib/cli/gtk-sharp-2.0/

LuaEnv.dll: LuaEnv.cs 
	MONO_PATH=$(MONO_PATH) gmcs -target:library \
		LuaEnv.cs \
		-r:IronPython \
		-r:LuaSharp \
		-pkg:gtk-sharp-2.0 \
		-out:LuaEnv.dll 

LuaTest.exe: LuaTest.cs 
	MONO_PATH=$(MONO_PATH) gmcs -target:exe \
		LuaTest.cs \
		-r:LuaSharp \
		-out:LuaTest.exe 
