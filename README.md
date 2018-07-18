# EMongo -- A Mongo Driver for Emacs

> An elisp driver for Mongo, and Emacs minor mode for working with json documents, commands, and
> queries. It is built using a patched version of Emacs that dynamically links with the
> [mongo c driver](http://mongoc.org/). Additionally it can take queries or documents from a buffer,
> and insert results back into the buffer.

## Examples

See [examples.txt](./examples.txt) for a walk through example, and
[examples-after.txt](examples-after.txt) for what it looks like after running the example.

## Warnings

__This code is completely unsupported. Use at your own risk.__

## Depedencies:

1. [Emacs](https://www.gnu.org/software/emacs/)
1. [Mongo C Driver](http://mongoc.org/)

See below for full installation instructions.

## Installation

Below targets running on OS X with [homebrew](https://brew.sh/):

### Build the Mongo C Driver

    brew install pkgconfig cmake
    mkdir mongoc
    pushd mongoc
        curl -LO https://github.com/mongodb/mongo-c-driver/releases/download/1.11.0/mongo-c-driver-1.11.0.tar.gz
        tar xzf mongo-c-driver-1.11.0.tar.gz
        cd mongo-c-driver-1.11.0
        mkdir cmake-build
        cd cmake-build
        cmake -DENABLE_AUTOMATIC_INIT_AND_CLEANUP=OFF ..
        make -j 4
        sudo make install
    popd

### Build Emacs itself

The file patch.diff is in this repo.

    git clone -b emacs-26.1 git://git.sv.gnu.org/emacs.git
    patch -p1 < patch.diff
    cd emacs
    ./autogen.sh
    ./configure --without-makeinfo
    make -j 4

Binary is in src/emacs.

Add to your `~/.emacs`:

    (add-to-list 'load-path (concat (getenv "MONGO_MODE_HOME") "/mongo-mode"))
    (require 'mongo)
