# usage: source scripts/ppm2gif.sh output.gif *.ppm

first=$1
shift

magick -delay 5 -loop 0 $@ -dither FloydSteinberg -colors 256 -layers Optimize $first
