# Guile 1.8.8 for GNU TeXmacs

## Install dependency
```shell script
sudo apt install automake libtool gettext libgmp-dev libreadline-dev flex texinfo cmake
```

## Build by autotool
```shell script
./autogen.sh
./configure --disable-error-on-warning
make -j4
```

## Build by CMake
```shell script
mkdir build && cd build
cmake ..
make -j4
```
## Step by step guide to make a debian package for GNU Guile 1.8.8 by yourself

Step 1: Prepare your build environment.
```
sudo apt install devscripts
```

Step 2: Clone the texmacs branch of [guile](https://github.com/texmacs/guile).
```
git clone --depth 10 -b texmacs --single-branch  git@github.com:texmacs/guile.git
```

Step 3: Make your own debs.
```
cd guile
dpkg-buildpackage -us -uc -b
```

Great! Now the wanted debs has been generated:
```
$ ls ../*deb
../guile-1.8_1.8.8+1-10_amd64.deb         ../guile-1.8-dev_1.8.8+1-10_amd64.deb  ../guile-1.8-libs_1.8.8+1-10_amd64.deb
../guile-1.8-dbgsym_1.8.8+1-10_amd64.deb  ../guile-1.8-doc_1.8.8+1-10_all.deb    ../guile-1.8-libs-dbgsym_1.8.8+1-10_amd64.deb
```
Install `guile-1.8`, `guile-1.8-dev`, `guile-1.8-libs` via `dpkg`.

To install the pre-packaged deb for Debian, just download the wanted deb from [Binary GNU/Linux packages for TeXmacs](http://www.texmacs.org/tmweb/download/linux-packages.en.html). Or compile GNU TeXmacs by yourself according to [Compiling TeXmacs from the source code](http://www.texmacs.org/tmweb/download/sources.en.html) or [Build with CMake on GNU/Linux](https://github.com/texmacs/texmacs/wiki/Build-with-CMake-on-GNU-Linux).
