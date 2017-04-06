include config.mk

.PHONY : all
all : classified.csv

edifice : 
	git clone https://github.com/chihacknight/$@.git
	$(MAKE) -C $@

bin/activate : requirements.txt
	virtualenv .
	source $@; \
		pip install "numpy>=1.9.2"; \
		pip install "scipy>=0.16.0"; \
		pip install -r $<

classified.csv : bin/activate classify.py edifice
	source $<; \
		python $(word 2, $^)	

# Set target-specific TABLE variable for each target which modifies a db table.
%.table %.clean : TABLE = $(basename $@)

# Create database for the classification_tool web application.
.ONESHELL: classification_tool.table
classification_tool.table: edifice.table
	# There are 426,626 records in the 'edifice' table, but only 422,037 unique
	# PINs. Selecting DISTINCT pins allows us to use the PIN as the primary key
	# in the resulting table, although it may discard some non-redundant
	# information in the rows with duplicate PINs. There happen to be 426,626
	# rows when grouped by pin and footprint, so ordering by pin and footprint
	# is a convenient way to ensure that the DISTINCT rows are selected
	# deterministically.
	TOTAL=$$(psql --tuples-only -c "SELECT COUNT(*) FROM edifice")
	DISTINCT=$$(psql --tuples-only -c \
			 "SELECT COUNT(*) FROM (\
	             SELECT pin, footprint \
	             FROM edifice GROUP BY pin, footprint\
	          ) AS _")
	if [ $$TOTAL -eq $$DISTINCT ]; then
		psql -c "CREATE TABLE ${TABLE} AS SELECT DISTINCT ON (pin) * FROM edifice ORDER BY pin, footprint; \
			CREATE TYPE BuildingType AS ENUM ('UNKNOWN', 'SUNROOM', 'BAY_SUNROOM', 'SUNPORCH'); \
			ALTER TABLE ${TABLE} ADD COLUMN building_type BuildingType";
	else
		echo "ERROR: Rows are not uniquely identified by (pin, footprint), so \
		      ORDER BY (pin, footprint) will not completely determine which \
			  rows are returned by the SELECT DISTINCT statement. Please \
			  a different criterion for ordering the rows (you may need to \
			  include additional fields).";
	fi
	psql -U postgres -c "GRANT SELECT ON ${TABLE} TO PUBLIC; \
		GRANT UPDATE (building_type) ON ${TABLE} TO PUBLIC"
	touch $@

classification_tool.clean:
	psql -U postgres -c "DROP TABLE IF EXISTS ${TABLE}; \
		DROP TYPE IF EXISTS BuildingType"
	rm -f ${TABLE}.table

classification-tool/image-cache: classification_tool.table
	mkdir -p $@;
	PINS=$$(psql --tuples-only -c "SELECT pin FROM classification_tool WHERE property_class = 211 AND stories > 2 AND exterior_construction != 'Frame'" | sed "/^$$/d")
	for PIN in $$PINS; do echo "http://cookcountyassessor.com/PropertyImage.aspx?pin=$$PIN"; done \
		| wget --input-file=- --wait=2 --random-wait --directory-prefix=$@ --output-file=cache-images.errors
	for IMAGE in $@/PropertyImage.aspx?pin=*; \
	do \
		mv $$IMAGE $@/$${IMAGE#$@/PropertyImage.aspx?pin=}.jpg
	done
