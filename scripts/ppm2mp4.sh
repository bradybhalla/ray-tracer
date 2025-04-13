# usage: source scripts/ppm2mp4.sh "*.ppm" out.mp4

ffmpeg -framerate 30 -pattern_type glob -i $1 -c:v libx264 -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" -pix_fmt yuv420p $2
