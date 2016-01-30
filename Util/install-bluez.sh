WORK_DIR=$(mktemp -d)

pushd $WORK_DIR
  apt-get install \
  libglib2.0-dev \
  libdbus-1-dev \
  libudev-dev \
  libical-dev \
  libreadline6-dev \
  \
  checkinstall \

  wget https://www.kernel.org/pub/linux/bluetooth/bluez-5.37.tar.xz
  tar xf bluez-5.30.tar.xz

  pushd bluez-5.37
    ./configure
    make
    checkinstall

  popd
popd

rm -rf $WORK_DIR
