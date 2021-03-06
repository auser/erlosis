LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= $(shell cat VERSION | tr -d '\n')
CC							= erlc
ERL							= erl
EBIN						= ebin
INCLUDE_DIRS 		= include
CFLAGS					= +debug_info -W0 -I $(INCLUDE_DIRS) -pa $(EBIN) -I gen-erl/
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
DEPS_DIR 				= deps
EBIN_DIRS				= $(wildcard $(DEPS_DIR)/*/ebin) $(wildcard include/*/ebin)
APP							= erlosis

all: compile

compile:
	@$(ERL) -pa $(EBIN_DIRS) -pa $(EBIN) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

boot:
	(cd ebin; $(ERL) -pa src -pa ebin -pz deps/*/ebin -noshell -run make_boot write_scripts $(APP) $(VERSION);)

debug:
	@$(ERL) -pa $(EBIN_DIRS) -pa $(EBIN) -noinput +B -eval 'case make:all([{d, debug}]) of up_to_date -> halt(0); error -> halt(1) end.'
	
test: debug
	$(ERL) 	-noshell -pa $(EBIN) \
					-pa deps/*/ebin \
					-s conf_reader test \
					-s conf_writer test \
					-s init stop
clean:
	rm -rf $(EBIN)/*.beam $(EBIN)/erl_crash.dump erl_crash.dump $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script $(EBIN)/$(APP)-*.tar.gz *.log