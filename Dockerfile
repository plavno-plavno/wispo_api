# Build stage 0
FROM erlang:27.1.1-alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY . wispo_api

# And build the release
WORKDIR wispo_api
RUN apk add openssl && \
    apk add ncurses-libs && \
    apk add libstdc++ && \
    apk add g++ && \
    apk add make && \
    apk add git && \
    make && make rel

# Build stage 1
FROM alpine

# Install some libs
RUN apk add openssl && \
    apk add ncurses-libs && \
    apk add libstdc++ && \
    apk add g++ && \
    apk add make

# Install the released application
COPY --from=0 /buildroot/wispo_api /opt/wispo_api

CMD ["/opt/wispo_api/_rel/wispo_api_release/bin/wispo_api_release", "foreground"]
