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
