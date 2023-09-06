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

PROGRAMS = cleps_cluster_analysis

all: $(PROGRAMS)

cleps_cluster_analysis.o: cla_inputdata.o cla_stats.o cla_algorithms.o
cleps_cluster_analysis: cla_inputdata.o cla_stats.o cla_algorithms.o

install: all
	mkdir -p $(TCWORK)
	# continue here

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
