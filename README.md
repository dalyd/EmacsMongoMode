# EMongo -- A Mongo Driver for Emacs

> An elisp driver for Mongo, and Emacs minor mode for working with json documents, commands, and
> queries.
>

## Warnings

__This code is completely unsupported. Use at your own risk.__

## Depedencies:

1. [Emacs](https://www.gnu.org/software/emacs/)
1. [Mongo C Driver](http://mongoc.org/)
2. [lib-ffi](https://sourceware.org/libffi/)
3. [elisp-ffi](https://github.com/skeeto/elisp-ffi)

See below for full installation instructions.

## Installation

Below targets running on OS X with [homebrew](https://brew.sh/):

Command overview:

    mkdir mongo-mode-install
    cd mongo-mode-install
    MONGO_MODE_HOME="$PWD"

    # Install Mongo C Drivers
    brew install pkgconfig
    mkdir mongoc
    pushd mongoc
        curl -LO https://github.com/mongodb/mongo-c-driver/releases/download/1.8.1/mongo-c-driver-1.8.1.tar.gz
        tar xzf mongo-c-driver-1.8.1.tar.gz
        cd mongo-c-driver-1.8.1
        ./configure --prefix="$MONGO_MODE_HOME/lib"
        make
        make install
        cd ..
    popd

    # Install elisp-ffi glue
    brew install libffi
    git clone git@github.com:skeeto/elisp-ffi.git
    pushd elisp-ffi
        PKG_CONFIG_PATH="$(brew --prefix libffi)/lib/pkgconfig"
        PKG_CONFIG_PATH="$PKG_CONFIG_PATH" make
    popd

    # Install mongo-mode
    # TODO: use public repo
    git clone git@github.com:rtimmons/skunkworks-2017q3.git mongo-mode
    pushd mongo-mode
        PKG_CONFIG_PATH="$MONGO_MODE_HOME/lib/lib/pkgconfig" make
    popd

    echo "export MONGO_MODE_HOME=\"$MONGO_MODE_HOME\"" \
        >> ~/.zshrc

Add to your `~/.emacs`:

    (add-to-list 'load-path (concat (getenv "MONGO_MODE_HOME") "/mongo-mode"))
    (add-to-list 'load-path (concat (getenv "MONGO_MODE_HOME") "/elisp-ffi"))
    (setq mlib (concat (getenv "MONGO_MODE_HOME") "/lib/lib/libmongoc-1.0.dylib"))
    (setq blib (concat (getenv "MONGO_MODE_HOME") "/lib/lib/libbson-1.0.dylib"))
    (setq hlib (concat (getenv "MONGO_MODE_HOME") "/mongo-mode/emongo-glue.so"))
    (require 'mongo)

## Examples

See [examples.txt](./examples.txt)

