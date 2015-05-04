all: clean python kawano

python:
	f2py -c -m interpolation interpolation.f

kawano:
	gfortran kawano_steriles.f interpolation.f

clean:
	rm -rf *.o *.so *.mod *.out
