# Data source files
foia_schema="data/foia-22606/foia.sql"
cc_tax_assessors_foia="data/foia-22606/FOI22606.CSV" 
building_footprints="data/building-footprints/buildings"

# Load data from FOIA request into postgres.
# Note that the schema for the foia table was already set up using csvsql from csvkit.
psql sunrooms -f $foia_schema
cat $cc_tax_assessors_foia \
    | sed 's/$//' \
    | psql sunrooms -c "COPY foia FROM stdin WITH (FORMAT CSV, HEADER true)"

# Load building footprints dataset from the City of Chicago data portal into postgres.
shp2pgsql -D -W "LATIN1" $building_footprints footprints | psql sunrooms
