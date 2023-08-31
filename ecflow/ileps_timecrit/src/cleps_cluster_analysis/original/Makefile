FC = gfortran
FCFLAGS = -g -fcheck=all -fbacktrace
FCFLAGS = -O2 -g -fbacktrace

ifeq ($(ECCODES_INCLUDE),)
  FCFLAGS += -I/usr/lib64/gfortran/modules
  LDFLAGS = -leccodes -leccodes_f90
else
  FCFLAGS += $(ECCODES_INCLUDE)
  LDFLAGS = $(ECCODES_LIB)
endif

PROGRAMS = cluster_util_eccodes

all: $(PROGRAMS)

cluster_util_eccodes: clink.o favg.o fstd.o selrm.o wards.o cstat.o fcor.o stdv.o

%: %.o
	$(FC) $(LDFLAGS) -o $@ $^

# General rule for building exe.o from exe.f90 or exe.F90; $< is used
# in order to list only the first prerequisite (the source file) and
# not the additional prerequisites such as module or include files
%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

%.o: %.F90
	$(FC) -c $(FCFLAGS) $<

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod .MOD

veryclean: clean
	rm -f *~ $(PROGRAMS)
