#!/bin/bash
function linkString {
  ls $1 | grep ".asd$" | cat | while read line; do echo ' ln -s ' $PWD/$1/$line 'systems/burning-'$line; done 
}

function link3rdString {
  ls $1 | grep ".asd$" | cat | while read line; do echo ' ln -s ' $PWD/$1/$line 'systems/'$line; done 
}

ls 3rdparty/ | cat | while read line; do link3rdString 3rdparty/$line; done | sh
ls src/ | cat | while read line; do linkString src/$line; done | sh
