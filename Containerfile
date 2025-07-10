FROM docker.io/eclipse-temurin:21-jdk-noble
RUN apt-get update && apt-get --yes upgrade
# get core tooling useful for working within the container
RUN apt-get install --yes --no-install-recommends fish just
# get JS stuff
RUN apt-get install --yes --no-install-recommends npm
# get native stuff
RUN apt-get install --yes --no-install-recommends clang
# get webview dependencies
RUN apt-get install --yes --no-install-recommends pkg-config libgtk-3-dev libwebkit2gtk-4.1-dev libwebkitgtk-6.0-dev libgtk-4-dev clang-format

RUN curl https://raw.githubusercontent.com/sbt/sbt/2293bddfefe382a48c1672df4e009bd7c5df32f4/sbt -o /usr/bin/sbt && \
    chmod +x /usr/bin/sbt
