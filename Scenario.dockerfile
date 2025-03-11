FROM swipl:latest
WORKDIR /app
CMD ["swipl", "parse_le.pl"]
