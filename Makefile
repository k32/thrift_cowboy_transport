#
# This makefile is only for running tests.
# Compile this library as an ordinary rebar3
# dependency
#
PROTO_PATH:=test
JS_PATH:=test
ERL_GEN_PATH:=test
THRIFT:=thrift
REBAR:=rebar3

.PHONY: test
test: $(JS_PATH)/gen-nodejs/TestService.js \
      $(JS_PATH)/node_modules/thrift/lib/nodejs/lib/thrift/thrift.js \
      $(ERL_GEN_PATH)/test_service_thrift.erl
	$(REBAR) ct

.PHONY: clean
clean:
	$(REBAR) clean
	rm $(ERL_GEN_PATH)/test_service_thrift.erl \
		$(ERL_GEN_PATH)/test_service_thrift.hrl \
		$(ERL_GEN_PATH)/test_constants.hrl \
		$(ERL_GEN_PATH)/test_types.hrl \
		$(ERL_GEN_PATH)/test_types.erl
	rm -rf $(JS_PATH)/node_modules
	rm -rf $(JS_PATH)/gen-nodejs

$(ERL_GEN_PATH)/test_service_thrift.erl: $(PROTO_PATH)/test.thrift
	$(THRIFT) -r --gen erl --out $(ERL_GEN_PATH) $<

$(JS_PATH)/gen-nodejs/TestService.js: $(PROTO_PATH)/test.thrift
	$(THRIFT) -r --gen js:node -o $(JS_PATH) $<

$(JS_PATH)/node_modules/thrift/lib/nodejs/lib/thrift/thrift.js:
	cd $(JS_PATH) && yarn
