
FROM --platform=${BUILDPLATFORM} ghcr.io/massimo-nocentini/wolframengine.docker:14.3-${BUILDARCH} AS base

ARG BUILDPLATFORM
FROM --platform=${BUILDPLATFORM} ghcr.io/massimo-nocentini/aux.scm:master

COPY --from=base /home/wolframengine/dist/wstp.h /usr/local/include/
COPY --from=base /home/wolframengine/dist/libWSTP64i4.so /usr/local/lib/
COPY src src

RUN ldconfig && cd src && make install && cd .. && rm -rf src
