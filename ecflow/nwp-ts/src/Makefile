# IMPORTANT NOTICE
# Environment varibles ECCODES_INCLUDE and ECCODES_LIB are set with the command:
# "module load ecmwf-toolbox", that must be executed before "make"

FC = gfortran
FCFLAGS = -I/usr/lib64/gfortran/modules $(ECCODES_INCLUDE) -O2
LDFLAGS = $(ECCODES_LIB)

# Additonal flags for debug
# FCFLAGS += -g -fcheck=all -fbacktrace

PROGRAMS = grib_set_tranges

all: $(PROGRAMS)

install: all
	install -d $(TCWORK)/bin
	install -t $(TCWORK)/bin $(PROGRAMS)

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
