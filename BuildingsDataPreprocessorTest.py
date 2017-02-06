import unittest
import testing.postgresql
import psycopg2
import numpy as np
import os

from numpy.testing import assert_array_equal

from BuildingsDataPreprocessor import BuildingsDataPreprocessor


class BuildingsDataPreprocessorTest(unittest.TestCase):
    def setUp(self):
        os.environ["POSTGIS_GDAL_ENABLED_DRIVERS"] = "PNG"

        self.postgresql = testing.postgresql.Postgresql()
        self.conn = psycopg2.connect(**self.postgresql.dsn())

        cursor = self.conn.cursor()
        cursor.execute("CREATE EXTENSION postgis")
        cursor.execute(
                "CREATE TABLE edifice ("
                "    footprint GEOMETRY(MultiPolygon,97634),"
                "    stories INTEGER,"
                "    property_class INTEGER,"
                "    tax_code VARCHAR(5),"
                "    building_use VARCHAR(15),"
                "    exterior_construction VARCHAR(15)"
                ")")

    def tearDown(self):
        self.postgresql.stop()

    def test_preprocess(self):
        cursor = self.conn.cursor()

        # From https://github.com/chihacknight/edifice.
        with open('edifice/97634.sql', 'r') as insertSRID_97634:
            cursor.execute(insertSRID_97634.read())

        # These examples extracted with the query:
        #   SELECT * FROM (SELECT ST_AsText(footprint) AS wkt FROM edifice) _
        #   ORDER BY CHAR_LENGTH(wkt) DESC LIMIT 1000
        cursor.execute("INSERT INTO edifice VALUES("
                       "ST_GeomFromText('MULTIPOLYGON((("
                       "  1117285.1431121 1936676.5385121,"
                       "  1117086.6431121 1936790.0385121,"
                       "  1117284.6431121 1936905.0385121,"
                       "  1117285.1431121 1936676.5385121)))', 97634),"
                       "5, 1, 'A', 'A', 'brick')")
        cursor.execute("INSERT INTO edifice VALUES("
                       "ST_GeomFromText('MULTIPOLYGON((("
                       "  1117285.1431121 1936676.5385121,"
                       "  1117086.6431121 1936790.0385121,"
                       "  1117284.6431121 1936905.0385121,"
                       "  1117285.1431121 1936676.5385121)))', 97634),"
                       "7, 2, 'A', 'B', 'frame')")

        preprocessor = BuildingsDataPreprocessor(self.conn)

        encoded_record = preprocessor.preprocess()

        # Only test the non-footprint part of the preprocessed vector.
        assert_array_equal([row[-8:] for row in encoded_record], np.array([
            [5, 1, 0, 1, 1, 0, 1, 0],
            [7, 0, 1, 1, 0, 1, 0, 1]]))

    def test_footprint(self):
        cursor = self.conn.cursor(
                cursor_factory=psycopg2.extras.RealDictCursor)

        # From https://github.com/chihacknight/edifice.
        with open('edifice/97634.sql', 'r') as insertSRID_97634:
            cursor.execute(insertSRID_97634.read())

        cursor.execute("INSERT INTO edifice VALUES("
                       "ST_GeomFromText('MULTIPOLYGON((("
                       "  1117285.1431121 1936676.5385121,"
                       "  1117086.6431121 1936790.0385121,"
                       "  1117284.6431121 1936905.0385121,"
                       "  1117285.1431121 1936676.5385121)))', 97634),"
                       "5, 1, 'A', 'A', 'brick')")

        cursor.execute("SELECT ST_DumpValues(ST_AsRaster(footprint, 10, 10, '1BB'), 1, false) AS footprint"
                       " FROM edifice")

        # cursor.execute("SELECT ST_AsPng(ST_AsRaster(footprint, 256, 256, '1BB')) AS footprint"
        #                " FROM edifice")
        # for i, row in enumerate(cursor):
        #     with open('/tmp/footprint.png', 'wb') as f:
        #         f.write(row['footprint'])
