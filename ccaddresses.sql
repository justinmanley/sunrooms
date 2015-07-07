ALTER TABLE ccaddresses ADD COLUMN address varchar(171);
UPDATE ccaddresses SET address = concat_ws(' ', addrnocom, stnamecom, uspspn, \
    uspsst, zip5);
