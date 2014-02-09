
all: ../modules/Widgets.dll 
	xbuild

../modules/Widgets.dll: Calico/Widgets.cs
	mcs Calico/Widgets.cs -target:library -out:../modules/Widgets.dll

