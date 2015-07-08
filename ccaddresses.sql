ALTER TABLE ccaddresses ADD COLUMN address varchar(171);
UPDATE ccaddresses SET address = concat_ws(' ', addrnocom, stnamecom, uspspn, uspsst, zip5);

SELECT AddGeometryColumn('ccaddresses', 'latlng', 4326, 'POINT', 2);
UPDATE ccaddresses SET latlng = CASE WHEN longitude IS NULL OR latitude IS NULL
    THEN ST_SetSRID(ST_MakePoint(0.0, 0.0), 4326)
    ELSE ST_GeomFromText( 
        'POINT(' || concat_ws(' ', longitude, latitude) || ')', 4326
    )
    END;


