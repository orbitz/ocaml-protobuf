.PHONY: all lib clean test install

all: lib

lib:
	$(MAKE) -C lib

install: all
	$(MAKE) -C lib install

test:
	$(MAKE) -C lib test

clean:
	$(MAKE) -C lib clean

