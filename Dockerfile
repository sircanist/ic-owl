FROM maven
COPY . /ic-owl
COPY owls /owls
RUN mkdir /tmp/repair
RUN cd /ic-owl/concept-fx-dl-only && mvn install
WORKDIR /ic-owl
ENTRYPOINT /ic-owl/default-run.sh
