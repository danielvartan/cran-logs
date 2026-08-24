# Container image for Google Cloud Run. Build it from the repository root:
#
#   docker build -t cran-logs .

FROM rocker/r-ver:4.6.1

LABEL org.opencontainers.image.title="CRAN Download Statistics"
LABEL org.opencontainers.image.vendor="Daniel Vartanian"

# System Dependencies -----
#
# The list below is what Posit Package Manager reports for the packages in
# `renv.lock` on Ubuntu 24.04. The binaries it serves link against these
# libraries, and a missing one only shows up when the package is loaded (the
# `-dev` packages alone are not enough: `fs` needs `libuv1`, for instance).
#
# Regenerate the list with `Rscript R/prepare-system-requirements.R` after
# changing `renv.lock`.

RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake \
    curl \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff-dev \
    libuv1-dev \
    libwebp-dev \
    libxml2-dev \
    make \
    zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

# R Dependencies -----
#
# `renv` restores the exact package versions recorded in `renv.lock`. The
# repositories set by the base image serve Linux binaries, so the restore does
# not compile from source.

ENV RENV_CONFIG_REPOS_OVERRIDE=${CRAN}
ENV RENV_PATHS_LIBRARY=/usr/local/lib/R/site-library
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE

RUN R -e "install.packages('renv', repos = Sys.getenv('CRAN'))"

WORKDIR /srv/dashboard

COPY renv.lock renv.lock

RUN R -e \
  "renv::restore( \
    lockfile = 'renv.lock', \
    library = Sys.getenv('RENV_PATHS_LIBRARY'), \
    prompt = FALSE \
  )"

# Loading the packages here turns a missing system library into a build
# failure instead of a container that starts and then dies on the first
# request.

RUN R -q -e \
  "invisible( \
    lapply( \
      c( \
        'bslib', 'DT', 'ggplot2', 'here', 'plotly', 'sass', 'shiny' \
      ), \
      library, \
      character.only = TRUE \
    ) \
  )"

# Application -----

COPY .here ./.here
COPY app.R ./app.R
COPY R ./R
COPY www ./www

# The theme embeds Google fonts, which are downloaded once and cached. Warming
# the cache at build time keeps the first request fast and removes the runtime
# dependency on fonts.googleapis.com.
#
# The cache lives under `XDG_CACHE_HOME` rather than a home directory so that
# the build, which runs as root, and the app, which runs as `dashboard`, read
# and write the same place.

ENV XDG_CACHE_HOME=/srv/dashboard/.cache

RUN R -q -e \
  "bslib::bs_theme( \
    bg = 'white', \
    fg = 'black', \
    primary = '#0559BE', \
    base_font = sass::font_google('Noto Sans') \
  ) |> \
  sass::sass() |> \
  invisible()"

# Runtime -----

RUN useradd --create-home --shell /bin/bash dashboard \
  && chown -R dashboard:dashboard /srv/dashboard

USER dashboard

ENV PORT=8080

EXPOSE 8080

CMD [ \
  "R", \
  "--quiet", \
  "--no-save", \
  "-e", \
  "shiny::runApp( \
    '/srv/dashboard', \
    host = '0.0.0.0', \
    port = as.integer(Sys.getenv('PORT', 8080)), \
    launch.browser = FALSE \
  )" \
]
