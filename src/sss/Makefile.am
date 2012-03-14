## By Ben Sauerwine
## Yep, it's trivial.  Just enough to get automake working.
## TO REPRODUCE ./configure:
##  autoreconf --force --install
bin_PROGRAMS = modelsearch modelsummary
modelsearch_SOURCES = serial_mod_search.cpp Model.cpp marglik.cpp newrun.cpp newmat/NEWMAT.H newmat/NEWMAT1.CPP newmat/NEWMAT2.CPP newmat/NEWMAT3.CPP newmat/NEWMAT4.CPP newmat/NEWMAT5.CPP newmat/NEWMAT6.CPP newmat/NEWMAT7.CPP newmat/NEWMAT8.CPP newmat/NEWMAT9.CPP
modelsearch_CXXFLAGS = $(AM_CXXFLAGS)
modelsummary_SOURCES = summarize_binary.cpp Model.cpp marglik.cpp newrun.cpp newmat/NEWMAT.H newmat/NEWMAT1.CPP newmat/NEWMAT2.CPP newmat/NEWMAT3.CPP newmat/NEWMAT4.CPP newmat/NEWMAT5.CPP newmat/NEWMAT6.CPP newmat/NEWMAT7.CPP newmat/NEWMAT8.CPP newmat/NEWMAT9.CPP
modelsummary_CXXFLAGS = $(AM_CXXFLAGS)


