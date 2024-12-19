FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y swi-prolog-nox --no-install-recommends

COPY . .

RUN swipl pack --install logicalenglish-0.0.3.zip

CMD ["swipl", "parse_le.pl"]
