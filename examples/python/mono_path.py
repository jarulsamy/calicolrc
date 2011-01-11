import Microsoft
try:
    registry = Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\")
except:
    registry = None
if registry:
    version = registry.GetValue("DefaultCLR") # '2.8'
    registry = Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\%s\\" % version)
    print registry.GetValue("SdkInstallRoot")

