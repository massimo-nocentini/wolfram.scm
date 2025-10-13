
FROM ghcr.io/massimo-nocentini/aux.scm:alpine

COPY src src

RUN apk add --no-cache gcompact && cd src && make install # && cd .. && rm -rf src
