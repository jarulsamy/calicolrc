#
# Pyjama - Scripting Environment
#
# Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $Id$

import clr
import System
import time
import os

# How to list relevant items out of a reference:
# [getattr(clr.References[2], x) for x in dir(clr.References[2]) if type(getattr(clr.References[2], x)) is type]

def get_references():
    """
    >>> get_references()
    [Name, Name, ...]
    """
    return [r.ManifestModule.Name for r in clr.References]

def get_reference(dll_file):
    """
    >>> get_reference("Myro.dll")
    [<reference>, <reference>, ...]
    """
    for reference in clr.References:
        if reference.ManifestModule.Name == dll_file:
            return reference
    return None

def get_class_names(dll_file):
    """
    >>> get_classes("Myro.dll")
    ['Myro']
    """
    reference = get_reference(dll_file)
    return set([c.Name for c in reference.ManifestModule.GetTypes() if not c.Name.startswith("_")])

def get_class(dll_file, _class):
    """
    >>> get_class('Myro.dll', 'Myro')
    <class>
    """
    reference = get_reference(dll_file)
    for c in reference.ManifestModule.GetTypes():
        if not c.Name.startswith("_") and c.Name == _class:
            return c

def get_items(dll_file, class_name, item_type):
    # method_type is from System.Reflection.MemberTypes.Method
    # How to get all of the items in a class:
    # >>> [x.Name for x in clr.References[2].ManifestModule.GetTypes()[0].GetMembers()]
    # ['init', 'forward', 'backward', 'Equals', 'GetHashCode', 'GetType', 'ToString', 'robot', '_Scribbler', '_Robot']
    # MemberType -> System.Reflection.MemberTypes.Method
    items = []
    reference = get_reference(dll_file)
    if reference:
        manifest = reference.ManifestModule
        if manifest:
            mtypes = manifest.GetTypes()
            if mtypes:
                for c in mtypes:
                    print "considering 1:", c.Name
                    if c is not None and c.Name == class_name:
                        for member in c.GetMembers():
                            print "considering 2:", member.Name
                            if (member and 
                                member.Name not in ["GetType"] and 
                                (int(System.Reflection.MethodAttributes.VtableLayoutMask) & int(member.Attributes)) == 0 and 
                                member.MemberType == item_type and
                                not member.Name.startswith("_")):
                                items.append((c, member))
    return items

def get_methods(dll_file, _class):
    return get_items(dll_file, _class, System.Reflection.MemberTypes.Method)

def get_fields(dll_file, _class):
    # How to find static fields in class:
    # >>> clr.References[2].ManifestModule.GetTypes()[0].GetFields()[0].Name
    # 'robot'
    return get_items(dll_file, _class, System.Reflection.MemberTypes.Field)

        # How many params does each take? What is return value?
        # repr(m)
        # The following is about Myro.forward(), m:
        # '<System.Reflection.MonoMethod object at 0x0000000000000066 [Void forward(Single, Nullable`1)]>'
        # >>> m.GetParameters()
        # Array[ParameterInfo]((<System.Reflection.ParameterInfo object at 0x000000000000006C [Single power]>, <System.Reflection.ParameterInfo object at 0x000000000000006D [System.Nullable`1[[System.Single, mscorlib, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]] time]>))
        # >>> dir(m.GetParameters()[0])
        # ['Attributes', 'AttrsImpl', 'ClassImpl', 'DefaultValue', 'DefaultValueImpl', 'Equals', 'Finalize', 'GetCustomAttributes', 'GetHashCode', 'GetIDsOfNames', 'GetOptionalCustomModifiers', 'GetRequiredCustomModifiers', 'GetType', 'GetTypeInfo', 'GetTypeInfoCount', 'Invoke', 'IsDefined', 'IsIn', 'IsLcid', 'IsOptional', 'IsOut', 'IsRetval', 'Member', 'MemberImpl', 'MemberwiseClone', 'MetadataToken', 'Name', 'NameImpl', 'ParameterType', 'Position', 'PositionImpl', 'RawDefaultValue',...]
        # p is a parameter:
        # p.Attributes
        # <enum System.Reflection.ParameterAttributes: Optional, HasDefault>
        # p.DefaultValue, p.ParameterType, 
        # 1, System.Single, 

if __name__ == "__main__":
    
    import sys
    for full_dll in sys.argv[1:]:
        clr.AddReference(full_dll)
        print full_dll
        path, dll = os.path.split(full_dll)
        for class_name in get_class_names(dll):
            print "   Class:", class_name
            for (c,item) in get_methods(dll, class_name):
                print "      Method:", item.Name
            for (c,item) in get_fields(dll, class_name):
                print "      Field:", item.Name

#clr.AddReference("webkit-sharp")
#import WebKit
#[t.FullName for t in get_reference("webkit-sharp.dll").GetTypes()]
#['WebKit.Download', 'WebKit.DownloadError', 'WebKit.DownloadStatus', 'WebKit.EditingBehavior', 'WebKit.Global', 'WebKit.HitTestResult', 'WebKit.HitTestResultClass', 'WebKit.HoveringOverLinkArgs', 'WebKit.LoadCommittedArgs', 'WebKit.LoadFinishedArgs', 'WebKit.LoadProgressChangedArgs', 'WebKit.LoadStartedArgs', 'WebKit.LoadStatus', 'WebKit.NavigationRequestedArgs', 'WebKit.NavigationResponse', 'WebKit.NetworkError', 'WebKit.NetworkRequest', 'WebKit.NetworkResponse', 'GtkSharp.WebkitSharp.ObjectManager', 'WebKit.PluginError', 'WebKit.PolicyError', 'WebKit.PopulatePopupArgs', 'WebKit.SecurityOrigin', 'WebKit.SoupAuthDialog', 'WebKit.StatusBarTextChangedArgs', 'WebKit.TitleChangedArgs', 'WebKit.WebBackForwardList', 'WebKit.WebDataSource', 'WebKit.WebDatabase', 'WebKit.WebFrame', 'WebKit.WebHistoryItem', 'WebKit.WebInspector', 'WebKit.WebNavigationAction', 'WebKit.WebNavigationReason', 'WebKit.WebPolicyDecision', 'WebKit.WebResource', 'WebKit.WebSettings', 'WebKit.WebView', 'WebKit.WebViewTargetInfo', 'WebKit.WebWindowFeatures', 'WebKit.WindowObjectClearedArgs', 'WebKit.WebView+LoadCommittedVMDelegate', 'WebKit.WebView+PopulatePopupVMDelegate', 'WebKit.WebView+WindowObjectClearedVMDelegate', 'WebKit.WebView+LoadFinishedVMDelegate', 'WebKit.WebView+NavigationRequestedVMDelegate', 'WebKit.WebView+HoveringOverLinkVMDelegate', 'WebKit.WebView+StatusBarTextChangedVMDelegate', 'WebKit.WebView+LoadProgressChangedVMDelegate', 'WebKit.WebView+SelectionChangedVMDelegate', 'WebKit.WebView+TitleChangedVMDelegate', 'WebKit.WebView+LoadStartedVMDelegate', 'WebKit.WebView+IconLoadedVMDelegate', 'WebKit.HoveringOverLinkHandler', 'WebKit.LoadCommittedHandler', 'WebKit.LoadFinishedHandler', 'WebKit.LoadProgressChangedHandler', 'WebKit.LoadStartedHandler', 'WebKit.NavigationRequestedHandler', 'WebKit.PopulatePopupHandler', 'WebKit.StatusBarTextChangedHandler', 'WebKit.TitleChangedHandler', 'WebKit.WindowObjectClearedHandler']