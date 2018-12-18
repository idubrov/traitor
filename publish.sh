#!/bin/sh

DIR=`dirname $0`

pushd "${DIR}/traitor-derive"
cargo publish
popd

pushd "${DIR}"
cargo publish
popd

