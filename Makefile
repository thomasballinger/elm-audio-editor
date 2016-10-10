all: build.js

build.js: *.elm
	elm make Main.elm --output=build.js

test: src/*.elm tests/*.elm
	cd tests; elm test Main.elm

assets: audio1.ogg audio2.ogg audio3.ogg

audio1.ogg:
	wget https://upload.wikimedia.org/wikipedia/commons/5/52/Hands_Across_the_Sea.ogg --output-document audio1.ogg

audio2.ogg:
	wget https://upload.wikimedia.org/wikipedia/commons/8/84/At_the_Jazz_Band_Ball_-_U.S._Coast_Guard_Band.ogg --output-document audio2.ogg
	# from https://commons.wikimedia.org/wiki/File:At_the_Jazz_Band_Ball_-_U.S._Coast_Guard_Band.ogg

audio3.ogg:
	wget https://upload.wikimedia.org/wikipedia/commons/2/22/Colonel_Bogey.ogg --output-document audio3.ogg
	# from https://commons.wikimedia.org/wiki/File:Colonel_Bogey.ogg

build: build.js index.html main.css assets
	rm -rf build
	mkdir build
	cp main.css *.ogg index.html build.js build/

deploy: build
	aws s3 sync build 's3://remixcast.com/editor/' --delete
