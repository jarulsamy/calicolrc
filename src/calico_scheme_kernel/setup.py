from distutils.core import setup
from distutils.command.install import install
import sys

class install_with_kernelspec(install):
    def run(self):
        install.run(self)
        from IPython.kernel.kernelspec import install_kernel_spec
        install_kernel_spec('kernelspec', 'calico_scheme_kernel', replace=True)

svem_flag = '--single-version-externally-managed'
if svem_flag in sys.argv:
    # Die, setuptools, die.
    sys.argv.remove(svem_flag)

setup(name='calico_scheme_kernel',
      version='0.0',
      description='A Scheme kernel for IPython',
      url="https://bitbucket.org/ipre/calico/src/master/src/calico_scheme_kernel/",
      long_description="A long description",
      author='Douglas Blank',
      author_email='doug.blank@gmail.com',
      py_modules=['calico_scheme_kernel'],
      requires=["calico"],
      cmdclass={'install': install_with_kernelspec},
      classifiers = [
          'Framework :: IPython',
          'License :: OSI Approved :: BSD License',
          'Programming Language :: Python :: 3',
          'Topic :: System :: Shells',
      ]
)
