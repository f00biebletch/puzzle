#
# Copyright 2011 Kevin McIntire, Gianluca Filippini
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not 
# use this file except in compliance with the License. You may obtain a copy 
# of the License at 
#
#    http://www.apache.org/licenses/LICENSE-2.0 
#
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
#
ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := ndexe
DEPLOY_PROD  := user@prod:/usr/local/lib/pz/ndexe
DEPLOY_IUT   := user@iut:/usr/local/lib/pz/ndexe
DEPLOY_DEV   := /usr/local/lib/pzd/ndexe

RELEASE_FILE := release.txt
APP_SRC      := src/ndexe.app
APP_TGT      := ebin/ndexe.app
NDEXE_VER    := $(shell head $(RELEASE_FILE))
MAJOR        := $(shell echo $(NDEXE_VER) | cut -f 1 -d '.')
MINOR        := $(shell echo $(NDEXE_VER) | cut -f 2 -d '.')
BUILD        := $(shell echo $(NDEXE_VER) | cut -f 3 -d '.')
PATCH        := $(shell echo $(NDEXE_VER) | cut -f 4 -d '.')
NEW_BUILD    := $(shell echo "$(BUILD)+1"|bc)
OLD_VER      := $(MAJOR).$(MINOR).$(BUILD).$(PATCH)
NEW_VER      := $(MAJOR).$(MINOR).$(NEW_BUILD).$(PATCH)

all: erl ebin/$(APP).app
dev: clean erl tag stage_dev
iut: clean erl stage_iut
prod: clean erl tag stage

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app: src/$(APP).app
	@cp -v src/$(APP).app $@

stage:
	@sed -e s/%%NDEXE_VERSION%%/\"$(NEW_VER)\"/g ${APP_SRC} > ${APP_TGT}
	@echo "deploy ndexe"
	@scp -C -P 2345 ebin/* $(DEPLOY_PROD)/ebin

stage_iut:
	@sed -e s/%%NDEXE_VERSION%%/\"$(NEW_VER)\"/g ${APP_SRC} > ${APP_TGT}
	@echo "deploy ndexe to iut"
	@scp -C ebin/* $(DEPLOY_IUT)/ebin

stage_dev:
	@sed -e s/%%NDEXE_VERSION%%/\"$(NEW_VER)\"/g ${APP_SRC} > ${APP_TGT}
	@echo "deploy ndexe to dev"
	@echo "@cp ebin/* $(DEPLOY_DEV)/ebin"
	@cp ebin/* $(DEPLOY_DEV)/ebin

release:
	@sed -e s/%%NDEXE_VERSION%%/\"$(NEW_VER)\"/g ${APP_SRC} > ${APP_TGT}
	@echo $(NEW_VER) > $(RELEASE_FILE)
	@echo "Version: $(NEW_VER)"

tag:
	@sed -e s/%%NDEXE_VERSION%%/\"$(OLD_VER)\"/g ${APP_SRC} > ${APP_TGT}

