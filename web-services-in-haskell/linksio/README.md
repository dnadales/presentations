# linksio

To build this application you need to install Postgres in your system. On
Ubuntu this can be done by running:

```sh
sudo apt-get install postgresql postgresql-server-dev-9.6
```

replacing `9.6` by the postgresql version installed on your system.

This application assumes the existence of a `tester` Postgres user, with
password `test`, and connects to the `linksdb` database. You can create these
in a Unix system as follows:

```sh
sudo su - postgres
psql template1
CREATE USER tester WITH PASSWORD 'test';
CREATE DATABASE linksdb;
GRANT ALL PRIVILEGES ON DATABASE "linksdb" to tester;
```

Once you start interacting with the application you can see the data that is
created by using `psql`:

```sh
sudo - su postgres
psql
\c linksdb
\dt
select * from link;
```
