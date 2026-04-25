FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.22_7_1.9.9

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

RUN sbt clean compile
