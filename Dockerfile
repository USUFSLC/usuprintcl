FROM alpine:latest as APPLICATION

RUN apk add sbcl bash
RUN apk add inotify-tools bash wget
RUN apk add cups cups-libs cups-client cups-filters

# Add user 
RUN adduser -h /home/runner -D runner
RUN addgroup runner lpadmin

RUN mkdir /home/runner/app
COPY . /home/runner/app
WORKDIR /home/runner
RUN chown -R runner /home/runner

USER runner

RUN wget https://beta.quicklisp.org/quicklisp.lisp -O /home/runner/quicklisp.lisp
RUN set -x; \
  sbcl --load /home/runner/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --quit && \
  echo '#-quicklisp (load #P"/home/runner/quicklisp/setup.lisp")' > /home/runner/.sbclrc && \
  rm /home/runner/quicklisp.lisp
  
WORKDIR /home/runner/app

RUN sbcl --load usuprintcl.asd --eval '(ql:quickload :usuprintcl)'

USER root
CMD cupsd ; su runner -c "bash watch.sh"
