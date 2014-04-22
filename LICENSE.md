All of the software developed for the [Institute for Personal Robots
in Education](http://calicoproject.org/IPRE_Software_Licenses) is
released under OSI-approved open source licenses. We encourage
collaboration in the community in our endeavor to explore the use of
robots in education. In this spirit, IPRE has developed a series of
projects, some which rely on other open source projects.

'''Myro''' - A set of interfaces for accessing and interacting with
robots. These are written in C# by researchers from Microsoft
Research, Georgia Tech, and Bryn Mawr College. These are released
under the [ Microsoft Public
License](http://opensource.org/licenses/ms-pl.html) and copyright is
retained by their owners. Myro has optional dependencies on
[SDL](https://www.libsdl.org/license.php), and
[eSpeak](https://bitbucket.org/ipre/calico/src/master/bin/mac/eSpeak/License.txt
). You can find the Myro code in the
[Calico/modules/Myro](https://bitbucket.org/ipre/calico/src/master/modules/Myro/)
folder.

'''Calico''' - An editor and shell environment for writing and
interacting with code in a dynamic manner. It is written in C# by
researchers from Microsoft Research, Georgia Tech, and Bryn Mawr
College. It is released under the
[Microsoft Public License](http://opensource.org/licenses/ms-pl.html)
and copyright is retained by their owners. Calico uses
[Gtk](http://www.gtk.org/), which has been released under the
[LGPL](http://www.gnu.org/licenses/lgpl.html). You can find the Calico
code in the [Calico/Source](https://bitbucket.org/ipre/calico/src/master/Source) folder.

'''Libraries and Plugins''' - Various libraries loaded by any of the
supported languages, including Python, Ruby, JavaScript, Basic,
etc. These are released under many different open source licenses. The
libraries that come with CPython (and can be loaded by IronPython) are
listed, along with their licenses, in the [IronPython Community
Edition](http://fepy.sourceforge.net/license.html). These are often
based, derived, or depend on other open source code.

'''Infrastructure''' - All of the infrastructure is built on open and
freely available standards. [ECMA
334](http://www.ecma-international.org/publications/standards/Ecma-334.htm)
defines the C# language, and [ECMA
335](http://www.ecma-international.org/publications/standards/Ecma-335.htm)
defines the Common Language Infrastructure (CLI). Microsoft has an
implementation of both of these in the form of their C# compiler, and
.NET Runtime. A free, but limited, C# compiler is available for
Microsoft Windows from Microsoft as [Visual Studio
Express](http://www.microsoft.com/express/), and the .NET runtime is
available as a free download from Microsoft. The Mono project has
implementations of both of these ECMA standards;
[Mono's](http://www.mono-project.com/FAQ:_Licensing) C# compiler is
released under the [GNU
GPL](http://www.gnu.org/licenses/licenses.html), and the Mono Runtime
libraries are released under the [GNU
LGPL](http://www.gnu.org/licenses/licenses.html). The [Python
License](http://www.python.org/psf/license/) allows Python to be free
for most any purpose.

Contributors should follow the rules specified by their chosen
license, and should not mix licenses across library boundaries as
indicated by the folder structure above.
