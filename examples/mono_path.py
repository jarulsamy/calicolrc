import Microsoft
python> Microsoft.Win32
<module 'Win32' (CLS module, 3 assemblies loaded)>
python> Microsoft.Win32.Registry
<type 'Registry'>
python> dir(Microsoft.Win32.Registry)
['ClassesRoot', 'CurrentConfig', 'CurrentUser', 'DynData', 'Equals', 'Finalize', 'GetHashCode', 'GetType', 'GetValue', 'LocalMachine', 'MemberwiseClone', 'PerformanceData', 'ReferenceEquals', 'SetValue', 'ToString', 'Users', '__all__', '__class__', '__delattr__', '__doc__', '__format__', '__getattribute__', '__hash__', '__init__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__']
python> Microsoft.Win32.Registry.LocalMachine
<Microsoft.Win32.RegistryKey object at 0x000000000000002C [HKEY_LOCAL_MACHINE]>
python> dir(Microsoft.Win32.Registry.LocalMachine)
['Close', 'CreateObjRef', 'CreateSubKey', 'DeleteSubKey', 'DeleteSubKeyTree', 'DeleteValue', 'Dispose', 'Equals', 'Finalize', 'Flush', 'GetAccessControl', 'GetHashCode', 'GetLifetimeService', 'GetSubKeyNames', 'GetType', 'GetValue', 'GetValueKind', 'GetValueNames', 'InitializeLifetimeService', 'MemberwiseClone', 'Name', 'OpenRemoteBaseKey', 'OpenSubKey', 'ReferenceEquals', 'SetAccessControl', 'SetValue', 'SubKeyCount', 'ToString', 'ValueCount', '__class__', '__delattr__', '__doc__', '__enter__', '__exit__', '__format__', '__getattribute__', '__hash__', '__init__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__']
python> Microsoft.Win32.Registry.LocalMachine.GetValue("Software")
python> Microsoft.Win32.Registry.LocalMachine.GetValue("Software\Novell")
python> Microsoft.Win32.Registry.LocalMachine.GetValue("Software\Novell\Mono\DefaultCLR")
python> Microsoft.Win32.Registry.LocalMachine.OpenSubkey("Software\Novell\Mono\DefaultCLR")
Traceback (most recent call last):
  File "C:\Users\dblank\Desktop\Pyjama\src\engine.py", line 133, in execute
    source.Execute(self.manager.scope)
  File "<string>", line 1, in <module>
<type 'exceptions.AttributeError'>: 'RegistryKey' object has no attribute 'OpenSubkey'
python> Microsoft.Win32.Registry.LocalMachine.OpenSubKey("Software\Novell\Mono\DefaultCLR")
python> Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\Novell\Mono\DefaultCLR")
<Microsoft.Win32.RegistryKey object at 0x000000000000002D [HKEY_LOCAL_MACHINE\Software\Novell\Mono\DefaultCLR]>
python> Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\DefaultCLR")
<Microsoft.Win32.RegistryKey object at 0x000000000000002E [HKEY_LOCAL_MACHINE\Software\Novell\Mono\DefaultCLR]>
python> regKey = Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\DefaultCLR")
python> regKey
<Microsoft.Win32.RegistryKey object at 0x000000000000002F [HKEY_LOCAL_MACHINE\Software\Novell\Mono\DefaultCLR]>
python> dir(regKey)
['Close', 'CreateObjRef', 'CreateSubKey', 'DeleteSubKey', 'DeleteSubKeyTree', 'DeleteValue', 'Dispose', 'Equals', 'Finalize', 'Flush', 'GetAccessControl', 'GetHashCode', 'GetLifetimeService', 'GetSubKeyNames', 'GetType', 'GetValue', 'GetValueKind', 'GetValueNames', 'InitializeLifetimeService', 'MemberwiseClone', 'Name', 'OpenRemoteBaseKey', 'OpenSubKey', 'ReferenceEquals', 'SetAccessControl', 'SetValue', 'SubKeyCount', 'ToString', 'ValueCount', '__class__', '__delattr__', '__doc__', '__enter__', '__exit__', '__format__', '__getattribute__', '__hash__', '__init__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasshook__']
python> regKey.GetSubKeyNames()
Array[str](())
python> regKey = Microsoft.Win32.Registry.LocalMachine.CreateSubKey("Software\\Novell\\Mono\\")
python> regKey.GetSubKeyNames()
Array[str](('2.8', 'DefaultCLR'))
python> regKey.GetValue("DefaultCLR")
'2.8'
