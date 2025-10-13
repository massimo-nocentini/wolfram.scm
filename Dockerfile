
FROM ghcr.io/massimo-nocentini/wolframengine.docker:wstpkernel as base

FROM ghcr.io/massimo-nocentini/aux.scm:alpine

COPY --from=base /home/wolframengine/wstp.h /usr/local/include/
COPY --from=base /home/wolframengine/libWSTP64i4.so /usr/local/lib/
COPY src src

RUN apk add --no-cache gcompat && cd src && make install # && cd .. && rm -rf src
