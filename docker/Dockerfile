FROM eclipse-temurin:21-jdk-jammy

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      curl \
      gnupg \
      git \
      graphviz \
      gnuplot \
      csh \
 && rm -rf /var/lib/apt/lists/*

# Install sbt
RUN curl -fsSL https://repo.scala-sbt.org/scalasbt/debian/gpg.key | gpg --dearmor -o /usr/share/keyrings/sbt.gpg \
 && echo "deb [signed-by=/usr/share/keyrings/sbt.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" > /etc/apt/sources.list.d/sbt.list \
 && apt-get update \
 && apt-get install -y sbt

WORKDIR /app

COPY . .

RUN sbt sbtVersion
RUN sbt clean compile
