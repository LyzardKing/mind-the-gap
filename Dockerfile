FROM swipl:latest
WORKDIR /app

COPY logicalenglish /app/logicalenglish
COPY highway_code.le /app/highway_code.le
COPY test /app/test
COPY requirements.txt /app/requirements.txt

RUN apt-get update && apt-get install -y python3 python3-pip
RUN apt-get clean && rm -rf /var/lib/apt/lists/*
RUN pip install --break-system-packages -r /app/requirements.txt

CMD python3 /app/test/test.py
