import os
import psycopg2
import psycopg2.extras
import sklearn.cluster

conn = psycopg2.connect(
    database=os.environ['PGDATABASE'],
    user=os.environ['PGUSER'],
    cursor_factory=psycopg2.extras.RealDictCursor
    )

preprocessor = BuildingsDataPreprocessor(conn)

sklearn.cluster.MeanShift().fit_predict(preprocessor.preprocess())
