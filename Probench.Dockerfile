FROM docker.io/sbtscala/scala-sbt:eclipse-temurin-21.0.8_9_1.12.0_3.7.4

WORKDIR /src
COPY . .

RUN sbt proBench/Universal/packageBin

FROM docker.io/eclipse-temurin:21-jdk-noble

RUN apt-get update -y
RUN apt-get install unzip

WORKDIR /project


COPY --from=0 ["/src/Modules/Examples/Protocol Benchmarks/target/universal/probench.zip", "."]
RUN unzip probench.zip
RUN rm probench.zip

ENTRYPOINT ["/project/probench/bin/probench"]
