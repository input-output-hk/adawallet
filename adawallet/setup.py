import re
from pathlib import Path
from distutils.core import setup

def get_version():
    text = Path("adawallet/__init__.py").read_text()
    match = re.search(r'^__version__ = "(.+)"', text, re.M)
    if not match:
        raise Exception("Unable to find __version__ in name/__init__.py")
    return match.group(1)


setup(
    name='adawallet',
    version=get_version(),
    scripts=['bins/adawallet'],
    packages=['adawallet']
)
