#!/bin/bash

# Exit if any subcommand fails
set -e

cd zokrates_fs_resolver && WITH_LIBSNARK=1 LIBSNARK_SOURCE_PATH=$HOME/libsnark cargo kcov && cd ..
cd zokrates_core && WITH_LIBSNARK=1 LIBSNARK_SOURCE_PATH=$HOME/libsnark cargo kcov && cd ..
cd zokrates_cli && WITH_LIBSNARK=1 LIBSNARK_SOURCE_PATH=$HOME/libsnark cargo kcov && cd ..
bash <(curl -s https://codecov.io/bash)
echo "Uploaded code coverage"
