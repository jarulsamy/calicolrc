"C:\Program Files (x86)\GnuWin32\bin\zip.exe" -r Calico-2.4.4-windows-all.zip Calico -x "Calico/papers/*" "Calico/bin/mac/*" "Calico/modules/*" "*/.git/*" "Calico/src/*" "Calico/Source/*" "Calico/modules/*/*" "Calico/StartCalico" "Calico/StartCalico.app/*" "Calico/languages/Jigsaw/Jigsaw/*" "Calico/server/*" "Calico/plugins/*" "Calico/README.txt" "Calico/NOTES" "Calico/make*" "Calico/Makefile"


"C:\Program Files (x86)\GnuWin32\bin\zip.exe" -r -v -g Calico-2.4.4-windows-all.zip "Calico/modules/*.dll" "Calico/modules/Myro/Robots/*.dll"
 

"C:\Program Files (x86)\GnuWin32\bin\zip.exe" -r -v -g Calico-2.4.4-windows-all.zip "Calico/languages/*/SyntaxModes/*.*" "Calico/languages/*/*.py" "Calico/languages/*/*/*.py"