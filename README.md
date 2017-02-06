sunrooms
========

An architectural research project into the question: Where are Chicago's [sunroom apartments](http://greycity.chicagomaroon.com/article/a-sunroom-of-ones-own-2/)?

Code for linking the [City of Chicago building footprints](https://data.cityofchicago.org/Buildings/Building-Footprints-deprecated-August-2015-/qv97-3bvb) dataset with Cook County [property](http://www.cookcountypropertyinfo.com/Pages/PIN-Search.aspx) and [address points](https://datacatalog.cookcountyil.gov/GIS-Maps/ccgisdata-Address-Point-Chicago/jev2-4wjs) datasets has been moved to [chihacknight/edifice](https://github.com/chihacknight/edifice).

Code for identifying sunroom apartments using the combined above datasets will be developed in this repository.

Development
===========


python -m unittest discover --pattern *Test.py

# For development with mypy.
git clone https://github.com/machinalis/mypy-data
MYPYPATH=mypy-data/numpy-mypy mypy BuildingsDataPreprocessor.py
