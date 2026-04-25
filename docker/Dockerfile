FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.22_7_1.9.9_2.13.13


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

RUN sbt update
RUN sbt compile
