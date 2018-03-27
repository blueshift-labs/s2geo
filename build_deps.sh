#!/usr/bin/env bash

DEPS_LOCATION=_build/deps

if [ -d "$DEPS_LOCATION" ]; then
    echo "s2-geometry-library fork already exist. delete $DEPS_LOCATION for a fresh checkout."
    exit 0
fi

OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')

##echo $OS
##echo $KERNEL

S2_GEOMETRY_REPO=https://github.com/google/s2geometry.git
S2_GEOMETRY_REV=$1

case $OS in
    Linux)
        case $KERNEL in
            CentOS)

                echo "Linux, CentOS"
                sudo yum -y install automake cmake gcc-c++ git libtool openssl-devel wget libgflags-dev libgoogle-glog-dev
            ;;

            Ubuntu)

                echo "Linux, Ubuntu"

                sudo apt-get -y install g++ make cmake libssl-dev libgflags-dev libgoogle-glog-dev
            ;;

            *) echo "Your system $KERNEL is not supported"
        esac
    ;;

    Darwin)
        brew install cmake openssl gflags glog
        export OPENSSL_ROOT_DIR=$(brew --prefix openssl)
        export OPENSSL_INCLUDE_DIR=$OPENSSL_ROOT_DIR/include/
        export OPENSSL_LIBRARIES=$OPENSSL_ROOT_DIR/lib
        ;;

    *) echo "Your system $OS is not supported"
esac

mkdir -p $DEPS_LOCATION

#checkout repo

pushd $DEPS_LOCATION
git clone ${S2_GEOMETRY_REPO}
pushd s2geometry
git checkout ${S2_GEOMETRY_REV}
popd
popd

#build

mkdir -p $DEPS_LOCATION/s2geometry/build
pushd $DEPS_LOCATION/s2geometry/build
cmake -DWITH_GLOG=OFF -DCMAKE_BUILD_TYPE=Release  ..
make -j 3
make install
popd