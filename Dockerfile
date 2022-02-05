FROM maven
COPY . /ic-owl
RUN cd /ic-owl/concept-fx-dl-only && mvn install
ENTRYPOINT /ic-owl/run.sh 
