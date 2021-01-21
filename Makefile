SRC := \
	connection-options.scm \
	transmission-filter.scm \
	transmission-labels.scm \
	transmission-update-seed-priority.scm \

default:
	chicken-install -n

install:
	chicken-install

test:
	chicken-install -test

clean:
	chicken-clean

lint: $(SRC)
	chicken-lint $(SRC)

test-new-egg:
	test-new-egg transmission https://raw.githubusercontent.com/SiIky/transmission.scm/master/transmission.release-info

.PHONY: clean default install lint test test-new-egg
