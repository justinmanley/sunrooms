import psycopg2  # type: ignore # NOQA - used in type annotation
import psycopg2.extras
import numpy

# Type aliases
columnName = str
columnValue = str


class BuildingsDataPreprocessor:
    # Columns with a finite number of values << than the number of rows are
    # treated as enums.
    enum_columns = [
        "property_class",
        "tax_code",
        "building_use",
        "exterior_construction"
    ]

    table = "edifice"

    # ranges is a dictionary in which the keys are string column names,
    # and the values are dictionaries which map elements in the range of
    # the column to an integer index.
    ranges = {}  # type: Dict[columnName, Dict[columnValue, int]]

    def __init__(self, conn):
        # type: (psycopg2.connection) -> None

        self.conn = conn

        cursor = conn.cursor(cursor_factory=psycopg2.extras.RealDictCursor)

        # Using ORDER BY ensures that the indices assigned to each column are
        # stable across executions of this program.
        for column in self.enum_columns:
            self.ranges[column] = {}
            cursor.execute("SELECT {column} FROM {table} "
                           "GROUP BY {column} "
                           "ORDER BY {column} "
                           .format(column=column, table=self.table))
            for i, row in enumerate(cursor):
                self.ranges[column][row[column]] = i

    def oneHotEncode(self, column, value):
        # type: (columnName, columnValue) -> numpy.ndarray[int]
        v = numpy.zeros(len(self.ranges[column]))
        v[self.ranges[column][value]] = 1
        return v

    # http://www.math.uci.edu/icamp/summer/research_11/park/shape_descriptors_survey_part2.pdf
    # https://math.uci.edu/icamp/summer/research_11/park/shape_descriptors_survey_part3.pdf
    def preprocessFootprint(self, footprint):
        # The rasterized image is represented as a 2-dimensional array.
        return [pixel for row in footprint for pixel in row]

    def preprocess(self):
        # type: () -> numpy.ndarray[numpy.ndarray[int]]

        cursor = self.conn.cursor(
                cursor_factory=psycopg2.extras.RealDictCursor)
        cursor.execute(("SELECT ST_DumpValues("
                        "  ST_AsRaster(footprint, 64, 64, '1BB'),"
                        "    1, false) AS footprint, "
                        " stories, "
                        "{enum_columns} FROM {table}").format(
                       enum_columns=", ".join(self.enum_columns),
                       table=self.table))

        data = list()

        for i, row in enumerate(cursor):
            encodedValues = list()

            encodedValues.extend(self.preprocessFootprint(row['footprint']))
            encodedValues.append(row['stories'])
            encodedValues.extend(self.oneHotEncode('property_class',
                                 row['property_class']))
            encodedValues.extend(self.oneHotEncode('tax_code',
                                 row['tax_code']))
            encodedValues.extend(self.oneHotEncode('building_use',
                                 row['building_use']))
            encodedValues.extend(self.oneHotEncode('exterior_construction',
                                 row['exterior_construction']))

            data.append(encodedValues)

        return numpy.array(data)
