CXX=clang++
CXXFLAGS=-O3 -Wall -Werror -pedantic -DNDEBUG

all: clean compile run

compile: day_8.cpp
	$(CXX) $(CXXFLAGS) -o day_8.exe -std=c++17 day_8.cpp

clean:
	rm -rf *.obj *.exe

run: day_8.exe
	./day_8.exe