set -e # exit immediately if a single command fails

sudo apt-get update -qq

sudo apt-get install postgresql-9.3 -y
sudo apt-get install postgis postgresql-9.3-postgis-2.1 -y

sudo pip install csvkit

sudo su postgres <<SETUP
    psql -c 'CREATE ROLE "ec2-user" LOGIN'
    createdb sunrooms
    psql -c 'CREATE EXTENSION postgis; \
        CREATE EXTENSION postgis_topology; \
        CREATE EXTENSION fuzzystrmatch; \
        CREATE EXTENSION postgis_tiger_geocoder'

    # Must be run as the `postgres` user.
    prefix="/usr/share/postgresql/9.3/contrib/postgis-2.1"
    psql sunrooms -f "$prefix/postgis.sql"
    psql sunrooms -f "$prefix/postgis_comments.sql"
    psql sunrooms -f "$prefix/spatial_ref_sys.sql"

    exit # log out as `postgres` user.
SETUP

