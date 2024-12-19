FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y software-properties-common \
    && apt-add-repository ppa:swi-prolog/stable \
    && apt-get install -y swi-prolog-nox --no-install-recommends

COPY . .

RUN swipl pack install -y logicalenglish-0.0.3.zip

CMD ["swipl", "parse_le.pl"]
