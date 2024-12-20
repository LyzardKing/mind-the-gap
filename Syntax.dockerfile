FROM swipl:latest
COPY . .

# RUN swipl pack install -y logicalenglish-1.0.zip

# WORKDIR /app
CMD ["swipl", "parse_le.pl"]
