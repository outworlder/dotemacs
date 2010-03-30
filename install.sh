#!/usr/bin/env bash

# From http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-in
SCRIPT_PATH="${BASH_SOURCE[0]}";
if([ -h "${SCRIPT_PATH}" ]) then
  while([ -h "${SCRIPT_PATH}" ]) do SCRIPT_PATH=`readlink "${SCRIPT_PATH}"`; done
fi
pushd . > /dev/null
cd `dirname ${SCRIPT_PATH}` > /dev/null
SCRIPT_PATH=`pwd`;
popd  > /dev/null

echo "Creating symlinks"

ln -s $SCRIPT_PATH/.emacs ~/.emacs
ln -s $SCRIPT_PATH/children ~/.emacs.children

echo "Done."
