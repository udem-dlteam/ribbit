#!/bin/sh

OUTPUT="a.out"
SOURCES=""
LASTSOURCE=""
LISTING=""

while [ "$#" != "0" ]; do
  case "$1" in
    -o) # output
      shift
      OUTPUT="$1"
      ;;
    -l) # listing
      LISTING="yes"
      ;;
    *)
      LASTSOURCE="$1"
      SOURCES="$SOURCES $LASTSOURCE"
      if [ ! -e "$LASTSOURCE" ]; then
        echo "*** file does not exist: $LASTSOURCE"
        exit 1
      fi
      ;;
  esac
  shift
done

if test "$LISTING" != "" ; then
    LISTING="-l $OUTPUT.lst"
fi

nasm -f bin "$LISTING" -o "$OUTPUT" $SOURCES
chmod +x "$OUTPUT"

if test "$LISTING" != "" ; then
    cat "$OUTPUT.lst"
    rm "$OUTPUT.lst"
fi
