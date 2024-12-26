# Build stage 0
FROM erlang:27.1.1-alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY . wispo_api

# And build the release
WORKDIR wispo_api
RUN apk add --no-cache g++ && \
    apk add --no-cache make && \
    make && make rel

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++ && \
    apk add --no-cache g++ && \
    apk add --no-cache make

# Install the released application
COPY --from=0 /buildroot/wispo_api /opt/wispo_api

# Expose relevant ports
EXPOSE 8989

CMD ["/opt/wispo_api/_rel/wispo_api_release/bin/wispo_api_release", "foreground"]
