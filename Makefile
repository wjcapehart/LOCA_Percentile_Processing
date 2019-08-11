# ${F90} -o LOCA_Colate_to_HUCS.exe  -I${NETCDFINC} -L${NETCDFLIB} -lnetcdff ./LOCA_Colate_to_HUCS.f90

FCFLAGS = " -I${NETCDFINC} "
FLFLAGS = " -L${NETCDFLIB} -lnetcdff "

SRCS = ./LOCA_Colate_to_HUCS.f90

PROGRAM = ./LOCA_Colate_to_HUCS.exe



all: $(PROGRAM)

$(PROGRAM): $(SRCS)
	$(F90) $(FCFLAGS) $(FLFLAGS) $@ $<


clean:
	rm -f *.o *.mod


COM=${FC}

CPPFLAGS=-E
COMPFLAGS=

EXE=hello

SOURCES=sayhello.F schleppe.F schnuppi.F
MODS=words_schleppe.F words_schnuppi.F

OBJS=$(SOURCES:.F=.o)
MODOBJS=$(MODS:.F=.o)

# this may also be replaced by the ar command (creating archive)
all: $(MODOBJS) $(OBJS)
    $(FCOM) $(OBJS) $(MODOBJS) -o $(EXE)

%.for: %.F
    rm -f $*.for
    $(FCOM) $(CPPFLAGS) $*.F > $*.for

$(MODOBJS): %.o: %.for
    $(FCOM) -c -o $@ $(COMPFLAGS) $*.for

$(OBJS): %.o: %.for
    $(FCOM) -c -o $@ $(COMPFLAGS) $*.for

# clean: remove .for, .o, .do, and .stb files
.PHONY: clean
clean:
    -rm -f *.for *.o *.stb *.mod
