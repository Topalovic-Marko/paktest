# Compiler
FC = ifort

# Compiler flags
FFLAGS = -O2 -g

# Source file
SRC = paktest.F90

# Executable name
EXEC = paktest

# Default target
all: $(EXEC)

$(EXEC): $(SRC)
	$(FC) $(FFLAGS) -o $@ $(SRC)

clean:
	rm -f $(EXEC) *.mod *.o

.PHONY: all clean
