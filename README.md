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

Notes
=====

Using the [dollar gesture recognizer][] yielded a 19% false positive rate (19% of labeled non-sunrooms were incorrectly classified as sunrooms), and 23% recall (meaning that 77% of bona fide labeled sunrooms were missed). Note that the partial sunroom fragment ('SUNROOM') didn't match at all. The full sunroom example ('SUNROOM_FULL') was the only one that matched. The implication is that the dollar gesture recognizer is only good for recognizing full matches - it's no good at all for partial matches.

[dollar gesture recognizer]: http://depts.washington.edu/madlab/proj/dollar/index.html

