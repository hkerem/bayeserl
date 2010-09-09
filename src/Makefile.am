###
###    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@medra.com.tr>
###
###--------------------------------------------------------------------------
###    This file is part of MyDLP.
###
###    MyDLP is free software: you can redistribute it and/or modify
###    it under the terms of the GNU General Public License as published by
###    the Free Software Foundation, either version 3 of the License, or
###    (at your option) any later version.
###
###    MyDLP is distributed in the hope that it will be useful,
###    but WITHOUT ANY WARRANTY; without even the implied warranty of
###    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###    GNU General Public License for more details.
###
###    You should have received a copy of the GNU General Public License
###    along with MyDLP.  If not, see <http://www.gnu.org/licenses/>.
###--------------------------------------------------------------------------

dist_erl_DATA = \
	bayeserl.erl \
	bayeserl_memory_store.erl \
	bayeserl_server.erl \
	bayeserl_sup.erl

dist_hrl_DATA = \
	bayeserl.hrl

beam_DATA = \
	$(dist_erl_DATA:.erl=.beam) \
	bayeserl.app

boot_DATA = \
	bayeserl.boot \
	bayeserl.script

dist_noinst_DATA = \
	overview.edoc.in

html_DATA = \
	$(dist_erl_DATA:.erl=.html) \
	erlang.png \
	edoc-info \
	stylesheet.css \
	index.html \
	overview-summary.html \
	modules-frame.html \
	packages-frame.html

erldir = $(ERLANG_INSTALL_LIB_DIR_bayeserl)/src
hrldir = $(ERLANG_INSTALL_LIB_DIR_bayeserl)/include
beamdir = $(ERLANG_INSTALL_LIB_DIR_bayeserl)/ebin
privdir = $(ERLANG_INSTALL_LIB_DIR_bayeserl)/priv
htmldir = $(ERLANG_INSTALL_LIB_DIR_bayeserl)/doc
bootdir = $(libdir)/erlang/bin
ERLCFLAGS += -W

$(beam_DATA): $(dist_hrl_DATA)
$(boot_DATA): $(beam_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

.rel.boot:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. $<

.rel.script:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. $<

$(html_DATA): $(dist_erl_DATA) overview.edoc 
	@echo "Generating documentation..."
	@$(ERL) -noshell -run edoc_run application bayeserl '"."' '[{dir,"."}]'

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,bayeserl})" -noshell -s init stop

run: $(beam_DATA) $(boot_DATA)
	@echo "Running application..."
	@$(ERL) -sname console -boot bayeserl

clean-local:
	rm -rf $(beam_DATA) $(boot_DATA) $(html_DATA) bayeserl.rel overview.edoc

