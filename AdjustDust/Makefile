FC=pgf90

APATH          = .
IOAPIDIR=/nas01/depts/ie/cempd/apps/ioapi_31/07262011/Linux2_x86_64pg103_pgcc_nomp_kure
NETCDFDIR=/nas01/depts/ie/cempd/apps/netcdf/netcdf-3.6.3/Linux86_64-pgf90-pgcc_105/lib
LDIRS         = -L$(IOAPIDIR) -L$(NETCDFDIR)
LIBS_SYS=-lioapi -lnetcdf
LIBS          = $(LDIRS) $(LIBS_SYS)
FFLAGS        = -O2 -Msave -Mbackslash -Mbyteswapio -Kieee -Mextend -Bstatic -I$(IOAPIDIR)
##

all: apply_tf_2fdust

.f.o:
	$(FC) $(FFLAGS) -c -o $@ $<

AD2IV=apply_tf_2fdust.o

apply_tf_2fdust: $(AD2IV) $(ALLI)
	echo 'APPLY_TF_2FDUST:'
	$(FC) $(FFLAGS) -o $(APATH)/$(@) $(AD2IV) $(LIBS)

apply_tf_2fdust.o: 
	$(FC) $(FFLAGS) -c -o $@ apply_tf_2fdust.f

clean:
	/bin/rm -f *.o
