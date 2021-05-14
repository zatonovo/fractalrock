PACKAGE = $(shell pwd | xargs basename)
REPOSITORY = $(PACKAGE)
VERSION ?= 1.0.0
IMAGE ?= $(REPOSITORY):$(VERSION)
PORT ?= 8004
VARS ?=

ifdef DISPLAY
	XAUTH := /home/$(shell who | awk '{print $$1}')/.Xauthority
	XVARS := --network=host -e DISPLAY=$(DISPLAY) -v $(XAUTH):/root/.Xauthority
endif

HOST_DIR := $(shell pwd)
MOUNT_HOSTDIR := -v $(HOST_DIR):/app/$(PACKAGE)

CONTAINER_ID = $(shell docker ps | grep '$(REPOSITORY)' | awk '{print $$1}')

.PHONY: all run test clean
all:
	docker build -t $(IMAGE) .
	docker tag $(IMAGE) $(REPOSITORY):latest

# Open http://localhost:8004/ocpu/library/$(PACKAGE)
run: all
	# Local build is unnecessary
	#crant -SCi
	docker run -d -p $(PORT):8004 $(XMOUNT) $(MOUNT_HOSTDIR) $(REPOSITORY)

run-isolated: all
	docker run -d -p $(PORT):8004 $(REPOSITORY)

stop:
	docker stop $(CONTAINER_ID)

bash:
	docker exec $(VARS) -it -u jovyan $(CONTAINER_ID) bash

r:
	docker run $(VARS) -it $(XVARS) -u jovyan $(REPOSITORY) R

notebook: all
	@mkdir -p notebooks
	docker run -it -p 8888:8888 $(MOUNT_HOSTDIR) -w /app/$(PACKAGE)/notebooks $(REPOSITORY) jupyter notebook --allow-root

shiny:
	R --vanilla -e "library(shiny); shiny::runApp('shiny')"

