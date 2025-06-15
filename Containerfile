FROM docker.io/sbtscala/scala-sbt:eclipse-temurin-21.0.7_6_1.11.2_3.7.1
RUN apt-get update && apt-get --yes upgrade
# get core tooling useful for working within the container
RUN apt-get install --yes --no-install-recommends fish just
# get JS stuff
RUN apt-get install --yes --no-install-recommends npm
# get native stuff
RUN apt-get install --yes --no-install-recommends clang
# get webview dependencies
RUN apt-get install --yes --no-install-recommends pkg-config libgtk-3-dev libwebkit2gtk-4.1-dev libwebkitgtk-6.0-dev libgtk-4-dev clang-format


# the original container sets this for reasons. We unset it here becaues we DON’T want to use the scala version from the build
ENV SCALA_VERSION=
