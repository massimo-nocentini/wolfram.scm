
FROM --platform=$BUILDPLATFORM ghcr.io/massimo-nocentini/wolframengine.docker:14.3 AS base

FROM --platform=$BUILDPLATFORM ubuntu:latest

WORKDIR /home/ubuntu

COPY --from=base /home/wolframengine/dist/wstp.h /usr/local/include/
COPY --from=base /home/wolframengine/dist/libWSTP64i4.so /usr/local/lib/

RUN apt-get update && apt-get install -y sudo build-essential clang && ldconfig

COPY src src

RUN cd src && clang -O3 -o prog prog.c  -lWSTP64i4 && ./prog