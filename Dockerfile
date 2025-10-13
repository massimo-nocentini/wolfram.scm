
FROM ghcr.io/massimo-nocentini/wolframengine.docker:wstpkernel as base

FROM ghcr.io/massimo-nocentini/aux.scm:master

COPY --from=base /home/wolframengine/wstp.h /usr/local/include/
COPY --from=base /home/wolframengine/libWSTP64i4.so /usr/local/lib/
COPY src src

RUN cd src && make install # && cd .. && rm -rf src
