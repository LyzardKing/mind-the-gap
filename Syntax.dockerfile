FROM swipl:latest
WORKDIR /app
CMD ["swipl", "-q", "parse_le.pl"]
