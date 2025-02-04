# ============================================================================
# Name        : Makefile
# Author      : mt
# Version     :
# Copyright   : Your copyright notice
# Description : Makefile for Hello World in Fortran
# ============================================================================
# Makefile for paktest program

# Compiler
FC = ifort

# Compiler flags
FFLAGS = -O2 -g

# Source file
SRC = paktest.f90

# Executable name
EXEC = paktest

# Default target
all: $(EXEC)

$(EXEC): $(SRC)
	$(FC) $(FFLAGS) -o $@ $(SRC)

clean:
	rm -f $(EXEC) *.mod *.o

.PHONY: all clean