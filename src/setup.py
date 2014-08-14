from distutils.core import setup
import sys

svem_flag = '--single-version-externally-managed'
if svem_flag in sys.argv:
    # Die, setuptools, die.
    sys.argv.remove(svem_flag)

setup(name='calico',
      version='0.0',
      description='Tools for Python and IPython',
      long_description="A long description",
      author='Douglas Blank',
      author_email='doug.blank@gmail.com',
      url="https://bitbucket.org/ipre/calico/src/master/src/",
      packages=['calico', 'calico.magics'],
      classifiers = [
          'Framework :: IPython',
          'License :: OSI Approved :: BSD License',
          'Programming Language :: Python :: 3',
          'Topic :: System :: Shells',
      ]
)
