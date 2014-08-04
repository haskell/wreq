#!/bin/bash -e

# get ready to collect binaries for packaging
rm -rf /tmp/bin && mkdir -p /tmp/bin

cabal sandbox init

cabal install --only-dependencies -j
cabal configure
cabal build -j

if [ ! $(uname) == "Darwin" ]; then
  cabal sdist; yackage-upload dist/wreq-0.2.1.0.tar.gz http://localhost:3500/

pushd examples
cabal sandbox init
cabal sandbox add-source ..
cabal install -j
popd examples

##/vagrant/scripts/haskell/copy-binaries.sh wreq.cabal /tmp/bin/

##  source /vagrant/scripts/common/deb-pkg.sh /tmp/bin/ \
##    wreq \
##    0.1.0.0 \
##    'wreq binaries'
fi

echo done.
