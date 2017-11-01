CC=gcc
CFLAGS = $$(pkg-config --cflags libmongoc-1.0 libbson-1.0) -fpic
LDLIBS = $$(pkg-config --libs libmongoc-1.0 libbson-1.0) --shared

all: emongo_glue.so

helper.o: helper.c

# This should be .dylib for mac
emongo_glue.so: helper.o
	$(CC) $(LDLIBS) helper.o -o emongo_glue.so

clean:
	rm -f *.o *.so *.dylib
