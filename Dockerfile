FROM zatonovo/r-base
RUN cd /app/crant && git pull origin master

COPY . /app/fractalrock
WORKDIR /app/fractalrock
RUN crant -SCi
