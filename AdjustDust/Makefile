#FC=/nas02/apps/pgi-11.1/linux86-64/11.1/bin/pgf90

APATH          = ./
LIBS          = $(LDIRS) $(LIBS_SYS) $(LIBS_COMPILE)
LIBS_DEBUG    = $(LDIRS) $(LIBS_DEBUG_SYS) $(LIBS_COMPILE)
LIBPATH       = $(SMK_BIN)/libsmoke.a $(TOOLS_BIN)/libedsstools.a
LIBPATH_DEBUG = $(SMK_BIN)/libsmoke.debug.a $(TOOLS_BIN)/libedsstools.debug.a
##

all: apply_tf_2fdust

debug: apply_tf_2fdust.debug

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<

AD2IV=apply_tf_2fdust.o

apply_tf_2fdust: $(AD2IV) $(ALLI) $(LIBPATH)
	echo 'APPLY_TF_2FDUST:'
	$(FC) $(FFLAGS) -o $(APATH)/$(@) $(AD2IV) $(LIBS)

apply_tf_2fdust.debug: $(AD2IV) $(ALLI) $(LIBPATH_DEBUG)
	echo 'APPLY_TF_2FDUST (debug):'
	$(FC) $(FFLAGS) -o $(@) $(AD2IV) $(LIBS_DEBUG)

#
## Module dependencies
#
include $(MD_SRC)/modmake.inc
include $(ETDSRC)/modmake.inc
include $(FS_ROOT)/modmake.inc

apply_tf_2fdust.o: $(MODLISTS) $(MODSTCY) $(MODGRID)
	$(FC) $(FFLAGS) -c -o $@ apply_tf_2fdust.f

clean:
	/bin/rm -f *.o
