include config.mk

.PHONY: all
all: building_footprints_address_strings 

.PHONY: building_footprints_address_strings
building_footprints_address_strings:
	psql $(DB_NAME) -c "ALTER TABLE footprints ADD COLUMN address varchar(100)"
	psql -d $(DB_NAME) -c "UPDATE footprints SET \
		address = concat(concat_ws(' ', \
			label_hous, \
			unit_name, \
			pre_dir1, \
			st_name1, \
			suf_dir1, \
			st_type1), ', CHICAGO, IL')"
join:
	# source bin/activate	
	python join.py
