## By Ben Sauerwine
## Yep, it's trivial.  Just enough to get automake working.
## TO REPRODUCE ./configure:
##  autoreconf --force --install
bin_PROGRAMS = modelsearch modelsummary
modelsearch_SOURCES = serial_mod_search.cpp Model.cpp marglik.cpp 
modelsearch_CXXFLAGS = $(AM_CXXFLAGS)
modelsummary_SOURCES = summarize_binary.cpp Model.cpp marglik.cpp
modelsummary_CXXFLAGS = $(AM_CXXFLAGS)


