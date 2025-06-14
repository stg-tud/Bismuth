FROM docker.io/sbtscala/scala-sbt:eclipse-temurin-21.0.7_6_1.11.2_3.7.1
RUN apt-get update && apt-get --yes upgrade
RUN apt-get install --yes --no-install-recommends npm clang just pkg-config libwebkit2gtk-4.1-dev libjavascriptcoregtk-4.1-dev
