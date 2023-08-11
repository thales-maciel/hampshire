FROM haskell:9.2.8 as build

RUN mkdir /opt/build

COPY . /opt/build

RUN cd /opt/build && cabal update && cabal build hampshire

FROM debian:buster-slim

RUN mkdir -p /opt/app/hampshire

WORKDIR /opt/app/hampshire

RUN apt-get update -y && apt-get install -y libpq5

COPY --from=build /opt/build/dist-newstyle/build/x86_64-linux/ghc-9.2.8/hampshire-0.1.0.0/x/hampshire/build/hampshire/hampshire .

COPY --from=build /opt/build/hampshire/migrations ./migrations

EXPOSE 8080

CMD [ "./hampshire" ]

