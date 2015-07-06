include config.mk

.PHONY: all
all: ccaddresses.table chibuildings.table 

.PHONY: clean
clean:	
	rm addressPointChi.*
	rm ccaddresses.zip	

97634.sql:
	wget --no-use-server-timestamps -O $@ "http://spatialreference.org/ref/sr-org/7634/postgis/"

.PHONY: il_stateplane_spatial_ref
il_stateplane_spatial_ref: 97634.sql
	# Chicago Building Footprints dataset has coordinates in the IL State Plane
	# coordinate system (also see Cook County Address Points metadata).
	# Note that PostGIS doesn't accept the ESRI:102671 SRID, so I'm using
	# SR-ORG:7634, which is very similar, instead.
	sudo su postgres -c "psql $(DB_NAME) -f $<"

.PHONY: chibuildings.table
chibuildings.table: chibuildings.sql il_stateplane_spatial_ref
	psql -d $(DB_NAME) -f $<

join:
	# source bin/activate	
	python join.py

ccaddresses.zip:
	wget --no-use-server-timestamps -O $@ "https://datacatalog.cookcountyil.gov/api/geospatial/jev2-4wjs?method=export&format=Shapefile"

addressPointChi.shp: ccaddresses.zip
	unzip -j $<
	touch $@

.PHONY: ccaddresses.table 
ccaddresses.table: addressPointChi.shp
	# Synthesize address field out of components.
	shp2pgsql -I -s 4326 -d $< $(basename $@) | psql -d $(DB_NAME)
	psql -d $(DB_NAME) -c "ALTER TABLE ccaddresses ADD COLUMN address varchar(171)"
	psql -d $(DB_NAME) -c "UPDATE ccaddresses SET address = concat_ws(' ', \
		addrnocom, \
		stnamecom, \
		uspspn, \
		uspsst, \
		zip5 \
	)"

