FROM rocker/r-base

RUN mkdir -p /usr/local/tractor
COPY . /usr/local/tractor/
WORKDIR /usr/local/tractor

RUN make install
ENV TRACTOR_HOME /usr/local/tractor
ENV PATH $TRACTOR_HOME/bin:$PATH
ENV MANPATH $TRACTOR_HOME/man:$MANPATH

RUN groupadd -r tractor && useradd -r -g tractor tractor
RUN chown -R tractor:tractor .
USER tractor

CMD [ "/bin/bash" ]
