
SRCS = ./LOCA_Colate_to_HUCS.f90

FCFLAGS = -I${NETCDFINC} -L${NETCDFLIB} -lnetcdff

PROGRAM = LOCA_Colate_to_HUCS

all: $(PROGRAM)

$(PROGRAM): $(SRCS)
	    $(F90) $(FCFLAGS) $@ $<
