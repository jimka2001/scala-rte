FROM docker.io/sbtscala/scala-sbt:eclipse-temurin-11.0.17_8_1.8.2_2.13.10

# The .gitlab-ci.yml file at the top level directory checks whether this
# file has changed, and if so rebuilds the docker image.

RUN apt-get update                              \
  && RUNLEVEL=1 DEBIAN_FRONTEND=noninteractive  \
     apt-get install -y --no-install-recommends \
       graphviz \
       gnuplot \
       git \
       csh \
       curl \
       gnupg \
  && sbt sbtVersion

