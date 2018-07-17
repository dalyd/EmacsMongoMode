# Building Emacs

## Build the Mongo C Driver

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

## Build Emacs itself

The file patch.diff is in this repo.

    git clone -b emacs-26.1 git://git.sv.gnu.org/emacs.git
    patch -p1 < patch.diff
    cd emacs
    ./autogen.sh
    ./configure --without-makeinfo
    make -j 4

Binary is in src/emacs.
