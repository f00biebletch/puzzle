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
APP          := pzd
DEPLOY_PROD  := user@prod:/usr/local/lib/pzd/lib/pzd-0.7.0
DEPLOY_IUT   := user@iut:/usr/local/lib/pzd/lib/pzd-0.7.0
DEPLOY_DEV   := /usr/local/lib/pzd/lib/pzd-0.7.0

all: erl ebin/$(APP).app
prod: clean all stage
iut: clean all stage_iut
dev: clean all stage_dev

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app: src/$(APP).app
	@cp -v src/$(APP).app $@

stage:
	@echo "deploy pzd"
	@scp -C -P 2345 ebin/* $(DEPLOY_PROD)/ebin

stage_iut:
	@echo "deploy pzd to iut"
	@scp -C ebin/* $(DEPLOY_IUT)/ebin

stage_dev:
	@echo "deploy pzd to dev"
	@cp -r ebin/* $(DEPLOY_DEV)/ebin

web:
	@echo "deploying web"
	@scp -r -P 2345 priv/www/* $(DEPLOY_PROD)/priv/www

web_iut:
	@echo "deploying web to iut"
	@scp -r priv/www/* $(DEPLOY_IUT)/priv/www

web_dev:
	@echo "deploying web to dev"
	@cp -r priv/www/* $(DEPLOY_DEV)/priv/www

tags:
	@etags --output src/TAGS src/*.erl src/*.hrl
