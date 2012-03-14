## By Ben Sauerwine
## Yep, it's trivial.  Just enough to get automake working.
## TO REPRODUCE ./configure:
##  autoreconf --force --install
bin_PROGRAMS = bfrm
bfrm_SOURCES = bfrm.cpp
bfrm_CXXFLAGS = $(AM_CXXFLAGS)

lib_LTLIBRARIES = libbfrm.la
bfrm_LDADD = libbfrm.la

libbfrm_la_HEADERS = \
bfrmdata.h   bfrmrawdata.h   inversewishart.h  newmatap.h  newmatrc.h   precisio.h     tnt_array1d_utils.h  tnt_i_refvec.h    tnt_version.h \
bfrmevo.h    bfrmresult.h    controlw.h  mdp.h             newmat.h    newmatrm.h   solution.h     tnt_array2d.h        tnt_math_utils.h  util.h        \
bfrm.h       bfrmscreener.h  extreal.h   Model.h           newmatio.h  newran.h     stdafx.h       tnt_array2d_utils.h  tnt_stopwatch.h                 \
bfrmprior.h  boolean.h       include.h   myexcept.h        newmatnl.h  nminclude.h  tnt_array1d.h  tnt.h                tnt_subscript.h


libbfrm_ladir = $(includedir)/bfrm

libbfrm_la_SOURCES = \
                    $(libbfrm_a_HEADERS) \
bandmat.cpp   bfrmresult.cpp    evalue.cpp   inversewishart.cpp  Model.cpp     newmat2.cpp  newmat6.cpp  newmatex.cpp  solution.cpp  submat.cpp \
bfrm.cpp      bfrmscreener.cpp  extreal.cpp  jacobi.cpp          myexcept.cpp  newmat3.cpp  newmat7.cpp  newmatnl.cpp  sort.cpp      svd.cpp    \
bfrmdata.cpp  bfrmprior.cpp    cholesky.cpp      fft.cpp      ltqnorm.cpp         newfft.cpp    newmat4.cpp  newmat8.cpp  newmatrm.cpp  stat.cpp      util.cpp   \
bfrmevo.cpp   bfrmrawdata.cpp    hholder.cpp  mdp.cpp             newmat1.cpp   newmat5.cpp  newmat9.cpp  newran.cpp    stdafx.cpp DoBfrm.cpp
