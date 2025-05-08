DUNE = dune

build:
	$(DUNE) build

render:
	$(DUNE) exec bin/single_render.exe > tmp.ppm
	open tmp.ppm

animation:
	rm -rf animation/
	mkdir animation
	$(DUNE) exec bin/animation.exe
	ffmpeg -y -framerate 30 -i "animation/frame_%05d.ppm" \
		-c:v libx264 -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" \
		-pix_fmt yuv420p tmp.mp4
	open tmp.mp4

clean:
	$(DUNE) clean
	rm -f tmp.ppm
	rm -rf animation
	rm -f tmp.mp4
	rm -rf *.trace

# only works for MacOS
profile:
	$(DUNE) build
	xctrace record --output . --template "Time Profiler" \
		--target-stdout - --launch -- ./_build/default/bin/single_render.exe

.PHONY: build render animation clean profile
