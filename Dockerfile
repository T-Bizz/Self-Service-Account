FROM jetty:9

ENV JAVA_OPTIONS -Dlogback.configurationFile=/appConf/logback.xml

EXPOSE 8080
EXPOSE 443
COPY webapps/root.war /var/lib/jetty/webapps/root.war
COPY jettyConf/* /var/lib/jetty/start.d/
COPY jettyConf/keystore /var/lib/jetty/etc/keystore
RUN chmod go-rwx /var/lib/jetty/etc/keystore
