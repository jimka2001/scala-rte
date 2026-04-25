FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.10_7_1.10.0

# Rebuild image when this file changes

RUN apt-get update \
  && RUNLEVEL=1 DEBIAN_FRONTEND=noninteractive \
     apt-get install -y --no-install-recommends \
       graphviz \
       gnuplot \
       git \
       csh \
       curl \
       gnupg \
  && sbt sbtVersion

WORKDIR /app

COPY . .

RUN sbt update
RUN sbt compile