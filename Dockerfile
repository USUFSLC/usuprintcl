FROM alpine:latest as APPLICATION

RUN apk add sbcl bash
RUN apk add inotify-tools bash wget
RUN apk add cups cups-libs cups-client cups-filters

WORKDIR /app
COPY . /app

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp

# Install quicklisp
RUN set -x; \
  sbcl --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --quit && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  rm /root/quicklisp.lisp

# Get deps
RUN sbcl --load usuprintcl.asd --eval '(ql:quickload :usuprintcl)'

CMD /etc/init.d/cupsd start ; bash watch.sh
