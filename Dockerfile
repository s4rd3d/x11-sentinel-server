# syntax=docker/dockerfile:1.2-labs

# Building
# ========
#
# Use the following command to build the image:
#
#     $ docker build --ssh default . -t x11-sentinel-server
#

# Target Alpine version
ARG ALPINE_VERSION=3.16

# Target Erlang version
ARG ERLANG_VERSION=23

#===============================================================================
# Builder
#===============================================================================

FROM erlang:${ERLANG_VERSION}-alpine AS build

# Application name
ARG APP_NAME=x11_sentinel_server

# Environment profile.
ARG REBAR_PROFILE=production

# Rebar base directory
ARG REBAR_BASE_DIR=/opt/_rebar

# Set up environment variables
ENV APP_NAME=${APP_NAME} \
    REBAR_PROFILE=${REBAR_PROFILE}

# Install git for fetching non-hex depenencies.
#
# build-base: jiffy's build time dependency,
# libstdc++: jiffy's runtime dependency
RUN --mount=type=cache,id=apk-global,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk update && \
    apk upgrade && \
    apk add --update build-base openssh-client git libstdc++

# Set up workplace
WORKDIR /opt/app

# Prepare application dependencies
COPY rebar.config rebar.lock Makefile .
RUN --mount=type=cache,id=hex-x11-sentinel-server,sharing=locked,target=/root/.cache/rebar3 \
    --mount=type=ssh \
    make install-deps

# Compile the dependencies.
RUN --mount=type=cache,id=hex-x11-sentinel-server,sharing=locked,target=/root/.cache/rebar3 \
    --mount=type=ssh \
    make compile

# Copy application code (excluding entries in .dockerignore!)
COPY . .

# Build application release
RUN --network=none \
    --mount=type=cache,id=hex-x11-sentinel-server,sharing=locked,target=/root/.cache/rebar3 \
    make release

# Copy release to preparation room
RUN mkdir -p /opt/build &&\
    cp -a /opt/_rebar/${REBAR_PROFILE}/rel/${APP_NAME}/* /opt/build

#===============================================================================
# Runtime
#===============================================================================

FROM alpine:${ALPINE_VERSION} AS runtime

# Install required tools, dependencies
#
# * `libstdc++` is required by jiffy
# * `ncurses` is required by ERTS
#
RUN --mount=type=cache,id=apk-global,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update libstdc++ ncurses

# Set up a destination for the application
WORKDIR /opt/app

# Set up secure defaults
RUN chown nobody:nogroup -Rh /opt/app

# Copy release contents
COPY --from=build --chown=nobody:nogroup /opt/build ./

# Copy entrypoint
COPY docker/entrypoint.sh /

# Start application as a non-root user

USER nobody

# Set up default entrypoint and command
ENTRYPOINT ["/entrypoint.sh"]
CMD ["foreground"]
