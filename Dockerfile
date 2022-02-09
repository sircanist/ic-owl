FROM maven
COPY . /ic-owl
RUN cd /ic-owl/concept-fx-dl-only && mvn install
WORKDIR /ic-owl
ENTRYPOINT /ic-owl/default-run.sh
