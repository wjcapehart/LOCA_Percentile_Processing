
# ${F90} -o LOCA_Colate_to_HUCS  -I${NETCDFINC} -L${NETCDFLIB} -lnetcdff ./LOCA_Colate_to_HUCS.f90

FCFLAGS = -I${NETCDFINC}
FLFLAGS = -L${NETCDFLIB} -lnetcdff

SRCS = ./LOCA_Colate_to_HUCS.f90

PROGRAM = LOCA_Colate_to_HUCS



all: $(PROGRAM)

	all: $(PROGRAM)

  $(PROGRAM): $(SRCS)
		$(F90) $(FCFLAGS) $@ $<

  %.o: %.f90
		$(F90) $(FLFLAGS) -o $@ $<

  %.mod: %.h
		$(F90) $(FLFLAGS) -o $@ $<

  clean:
		rm -f *.o *.mod
