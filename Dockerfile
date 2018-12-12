FROM haskell:latest

RUN stack setup
RUN stack update

WORKDIR /opt/server
COPY . .

RUN stack build
RUN stack test

CMD stack exec IF708-KV-exe
