MONO_LIB_PATH=/usr/lib/mono/4.0
IKVMSTUB=ikvmstub -r:$(MONO_LIB_PATH)/mscorlib.dll -nostdlib 
#IKVMSTUB=ikvmstub.exe
IKVMSTUB=ikvmstub.exe -r:$(MONO_LIB_PATH)/mscorlib.dll -nostdlib  -r:$(MONO_LIB_PATH)/System.Web.Services.dll   -r:$(MONO_LIB_PATH)/System.Drawing.dll   -r:$(MONO_LIB_PATH)/System.dll   -r:$(MONO_LIB_PATH)/System.Core.dll

all: Common.jar Csv.jar Math.jar mscorlib.jar Reflection.jar Rhyduino.jar Graphics.jar FarseerPhysics.jar Games.jar Kinect.jar Processing.jar Myro.jar gtk-sharp.jar gdk-sharp.jar 

Common.jar: ../../../modules/Common.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Common.dll


Csv.jar: ../../../modules/Csv.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Csv.dll

Math.jar: ../../../modules/Math.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Math.dll

mscorlib.jar:
	 $(IKVMSTUB) /usr/lib/mono/4.0/mscorlib.dll 

Reflection.jar: ../../../modules/Reflection.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Reflection.dll

Rhyduino.jar: ../../../modules/Rhyduino.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Rhyduino.dll

Graphics.jar: ../../../modules/Graphics.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Graphics.dll

FarseerPhysics.jar: ../../../modules/FarseerPhysics.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/FarseerPhysics.dll

Games.jar: ../../../modules/Games.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Games.dll

Kinect.jar: ../../../modules/Kinect.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Kinect.dll

Processing.jar: ../../../modules/Processing.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ ../../../modules/Processing.dll

Myro.jar: ../../../modules/Myro.dll
	 $(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/  -r:../../../bin/Calico.exe ../../../modules/Myro.dll

gtk-sharp.jar:
	$(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ /usr/lib/mono/gac/gtk-sharp/2.12.0.0__35e10195dab3c99f/gtk-sharp.dll

gdk-sharp.jar:
	$(IKVMSTUB) -lib:../../../bin/ -lib:/usr/lib/cli/gtk-sharp-2.0/ -lib:/usr/lib/cli/glib-sharp-2.0/ -lib:/usr/lib/cli/atk-sharp-2.0/ -lib:/usr/lib/cli/gdk-sharp-2.0/ -lib:/usr/lib/cli/pango-sharp-2.0/ /usr/lib/mono/gac/gdk-sharp/2.12.0.0__35e10195dab3c99f/gdk-sharp.dll

