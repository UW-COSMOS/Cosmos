FROM mysql/mysql-router:latest
COPY router.conf /

ENTRYPOINT ["/run.sh"]
CMD ["mysqlrouter", "-a", "/router.conf"]

