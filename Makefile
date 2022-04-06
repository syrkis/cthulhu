DOCKER_IMAGE ?= syrkis/cthulhu:latest
FP2022_PASSWORD ?= 

# NuGet package info
API-KEY ?= ghp_***********************************
PACKAGE_ID ?= cthulhu
VERSION ?= 0.1.0

image:
ifeq ($(OS),Windows_NT)
	docker build . -t ${DOCKER_IMAGE} --build-arg FP2022_PASSWORD=${FP2022_PASSWORD}
else
	sudo docker build . -t ${DOCKER_IMAGE} --build-arg FP2022_PASSWORD=${FP2022_PASSWORD}
endif

run-image:
	docker run -it --mount src=${CURDIR},target=/cthulhu,type=bind ${DOCKER_IMAGE}

publish:
	dotnet nuget \
	push ScrabbleBot/bin/Release/<${PACKAGE_ID}>.<VERSION>.nupkg \
	--source https://nuget.pkg.github.com/jesper-bengtson/index.json --api-key
	${API-KEY}
