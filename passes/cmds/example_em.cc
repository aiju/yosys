#include "kernel/yosys.h"
#include "kernel/drivertools.h"
#include "kernel/topo_scc.h"
#include "kernel/functional.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

const char *reserved_keywords[] = {
	"alignas","alignof","and","and_eq","asm","atomic_cancel","atomic_commit","atomic_commit",
	"atomic_noexcept","auto","bitand","bitor","bool","break","case","case",
	"catch","char","char16_t","char32_t","char8_t","class","co_await","co_await",
	"co_return","co_yield","compl","concept","const","const_cast","consteval","consteval",
	"constexpr","constinit","continue","decltype","default","delete","delete",
	"do","double","dynamic_cast","else","enum","explicit","export","export",
	"extern","false","float","for","friend","goto","if","inline","inline",
	"int","long","mutable","namespace","new","noexcept","not","not_eq","not_eq",
	"nullptr","operator","or","or_eq","private","protected","public","public",
	"reflexpr","register","reinterpret_cast","requires","return","short","short",
	"signed","sizeof","static","static_assert","static_cast","struct","struct",
	"switch","synchronized","template","this","thread_local","throw","throw",
	"true","try","typedef","typeid","typename","union","unsigned","unsigned",
	"using","virtual","void","volatile","wchar_t","while","xor","xor","xor_eq",
	nullptr
};

struct CppScope {
	pool<std::string> used_names;
	dict<IdString, std::string> name_map;

	CppScope() {
		for(const char **p = reserved_keywords; *p != nullptr; p++)
			reserve(*p);
	}
	void reserve(std::string name) {
		used_names.insert(name);
	}
	std::string insert(IdString id) {
		std::string str = RTLIL::unescape_id(id);
		for(size_t i = 0; i < str.size(); i++)
			if(strchr("!\"#%&'()*+,-./:;<=>?@[]\\^`{|}~ ", str[i]))
				str[i] = '_';
		if(used_names.count(str) == 0){
			used_names.insert(str);
			name_map.insert({id, str});
			return str;
		}
		for (int idx = 0 ; ; idx++){
			std::string suffixed = str + "_" + std::to_string(idx);
			if (used_names.count(suffixed) == 0) {
				used_names.insert(suffixed);
				if(name_map.count(id) == 0)
					name_map.insert({id, suffixed});
				return suffixed;
			}
		}
	}
	std::string operator[](IdString id) {
		if(name_map.count(id) > 0)
			return name_map[id];
		else
			return insert(id);
	}
};

struct CppWriter {
	FILE *f;
	CppWriter() : f(stdout) {}
	CppWriter(const char *name) { f = fopen(name, "w"); }
	~CppWriter() { if(f != stdout) fclose(f); }
	void printf(const char *fmt, ...)
	{
		va_list va;
		va_start(va, fmt);
		vfprintf(f, fmt, va);
		va_end(va);
	}
};

struct CppStruct {
	std::string name;
	dict<IdString, std::string> types;
	CppScope scope;
	CppStruct(std::string name) : name(name) {
		scope.reserve("out");
		scope.reserve("dump");
	}
	void insert(IdString name, std::string type) {
		scope.insert(name);
		types.insert({name, type});
	}
	void print(CppWriter &f) {
		f.printf("struct %s {\n", name.c_str());
		for (auto p : types) {
			f.printf("\t%s %s;\n", p.second.c_str(), scope[p.first].c_str());
		}
		f.printf("\n\ttemplate <typename T> void dump(T &out) {\n");
		for (auto p : types) {
			f.printf("\t\tout(\"%s\", %s);\n", RTLIL::unescape_id(p.first).c_str(), scope[p.first].c_str());
		}
		f.printf("\t}\n};\n\n");
	}
	std::string operator[](IdString field) {
		return scope[field];
	}
};

struct ExampleFn {
	IdString name;
	int width;
	dict<IdString, Const> parameters;

	ExampleFn(IdString name, int width) : name(name), width(width) {}
	ExampleFn(IdString name, int width, dict<IdString, Const> parameters) : name(name), width(width), parameters(parameters) {}

	bool operator==(ExampleFn const &other) const {
		return name == other.name && parameters == other.parameters && width == other.width;
	}

	unsigned int hash() const {
		return mkhash(name.hash(), parameters.hash());
	}
};

typedef ComputeGraph<ExampleFn, int, IdString, IdString> ExampleGraph;

ExampleGraph compute_graph;

struct ExampleEmPass : public Pass
{
	ExampleEmPass() : Pass("example_em", "drivertools example") {}

    void help() override
	{
		//   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
		log("\n");
    }

	bool is_single_output(IdString type)
	{
		auto it = yosys_celltypes.cell_types.find(type);
		return it != yosys_celltypes.cell_types.end() && it->second.outputs.size() <= 1;
	}

	int output_width(Cell *cell)
	{
		if(is_single_output(cell->type)){
			for (auto it : cell->connections()) {
				if(yosys_celltypes.cell_output(cell->type, it.first)){
					return it.second.size();
				}
			}
		}
		return -1;
	}

	ExampleGraph calculate_compute_graph(RTLIL::Module *module)
	{
			DriverMap dm;
			dm.add(module);

			idict<DriveSpec> queue;
			idict<Cell *> cells;

			IntGraph edges;
			std::vector<int> graph_nodes;

			auto enqueue = [&](DriveSpec const &spec) {
				int index = queue(spec);
				if (index == GetSize(graph_nodes))
					graph_nodes.emplace_back(compute_graph.add(ExampleFn(ID($pending), 0), index).index());
				//if (index >= GetSize(graph_nodes))
				return compute_graph[graph_nodes[index]];
			};

			for (auto cell : module->cells()) {
				if (cell->type.in(ID($assert), ID($assume), ID($cover), ID($check)))
					enqueue(DriveBitMarker(cells(cell), 0));
			}

			for (auto wire : module->wires()) {
				if (!wire->port_output)
					continue;
				enqueue(DriveChunk(DriveChunkWire(wire, 0, wire->width))).assign_key(wire->name);
			}

			for (int i = 0; i != GetSize(queue); ++i)
			{
				DriveSpec spec = queue[i];
				printf("%d %s\n", i, log_signal(spec));
				ExampleGraph::Ref node = compute_graph[i];

				if (spec.chunks().size() > 1) {
					node.set_function(ExampleFn(ID($$concat), spec.size()));

					for (auto const &chunk : spec.chunks()) {
						node.append_arg(enqueue(chunk));
					}
				} else if (spec.chunks().size() == 1) {
					DriveChunk chunk = spec.chunks()[0];
					if (chunk.is_wire()) {
						DriveChunkWire wire_chunk = chunk.wire();
						if (wire_chunk.is_whole()) {
							node.sparse_attr() = wire_chunk.wire->name;
							if (wire_chunk.wire->port_input) {
								node.set_function(ExampleFn(ID($$input), wire_chunk.width, {{wire_chunk.wire->name, {}}}));
							} else {
								DriveSpec driver = dm(DriveSpec(wire_chunk));
								node.set_function(ExampleFn(ID($$buf), wire_chunk.width));

								node.append_arg(enqueue(driver));
							}
						} else {
							DriveChunkWire whole_wire(wire_chunk.wire, 0, wire_chunk.wire->width);
							node.set_function(ExampleFn(ID($$slice), wire_chunk.width, {{ID(offset), wire_chunk.offset}, {ID(width), wire_chunk.width}}));
							node.append_arg(enqueue(whole_wire));
						}
					} else if (chunk.is_port()) {
						DriveChunkPort port_chunk = chunk.port();
						if (port_chunk.is_whole()) {
							if (dm.celltypes.cell_output(port_chunk.cell->type, port_chunk.port)) {
								if (port_chunk.cell->type.in(ID($dff), ID($ff)))
								{
									Cell *cell = port_chunk.cell;
									node.set_function(ExampleFn(ID($$state), port_chunk.width, {{cell->name, {}}}));
									node.sparse_attr() = port_chunk.cell->name;
									for (auto const &conn : cell->connections()) {
										if (!dm.celltypes.cell_input(cell->type, conn.first))
											continue;
										enqueue(DriveChunkPort(cell, conn)).assign_key(cell->name);
									}
								}
								else
								{
									if(is_single_output(port_chunk.cell->type))
										node.set_function(ExampleFn(ID($$buf), port_chunk.width));
									else
										node.set_function(ExampleFn(ID($$cell_output), port_chunk.width, {{port_chunk.port, {}}}));
									node.append_arg(enqueue(DriveBitMarker(cells(port_chunk.cell), 0)));
									node.sparse_attr() = port_chunk.cell->name;
								}
							} else {
								node.set_function(ExampleFn(ID($$buf), port_chunk.width));

								DriveSpec driver = dm(DriveSpec(port_chunk));
								node.append_arg(enqueue(driver));
							}
						} else {
							DriveChunkPort whole_port(port_chunk.cell, port_chunk.port, 0, GetSize(port_chunk.cell->connections().at(port_chunk.port)));
							node.set_function(ExampleFn(ID($$slice), port_chunk.width, {{ID(offset), port_chunk.offset}}));
							node.append_arg(enqueue(whole_port));
						}
					} else if (chunk.is_constant()) {
						node.set_function(ExampleFn(ID($$const), chunk.size(), {{ID(value), chunk.constant()}}));
						node.sparse_attr() = "$const" + std::to_string(chunk.size()) + "b" + chunk.constant().as_string();
					} else if (chunk.is_multiple()) {
						node.set_function(ExampleFn(ID($$multi), chunk.size()));
						for (auto const &driver : chunk.multiple().multiple())
							node.append_arg(enqueue(driver));
					} else if (chunk.is_marker()) {
						Cell *cell = cells[chunk.marker().marker];
						ExampleFn fn(cell->type, chunk.size(), cell->parameters);
						if(cell->type.in({ID($lt), ID($le), ID($gt), ID($ge), ID($eq), ID($ne)})){
							int a_width = cell->parameters[ID(A_WIDTH)].as_int();
							int b_width = cell->parameters[ID(B_WIDTH)].as_int();
							bool a_signed = cell->parameters[ID(A_SIGNED)].as_bool();
							bool b_signed = cell->parameters[ID(B_SIGNED)].as_bool();
							log_assert(a_width == b_width);
							log_assert(a_signed == b_signed);
							fn.parameters = {{ID(WIDTH), a_width}};
							if(!a_signed && !cell->type.in({ID($eq), ID($ne)}))
								fn.name = IdString("$u" + cell->type.str().substr(1));
						}else if(cell->type.in({ID($or), ID($and), ID($xor)})){
							int a_width = cell->parameters[ID(A_WIDTH)].as_int();
							int b_width = cell->parameters[ID(B_WIDTH)].as_int();
							int y_width = cell->parameters[ID(Y_WIDTH)].as_int();
							log_assert(a_width == b_width && b_width == y_width);
							fn.parameters = {{ID(WIDTH), a_width}};
						}else if(cell->type.in({ID($logic_and), ID($logic_or)})){
							int a_width = cell->parameters[ID(A_WIDTH)].as_int();
							int b_width = cell->parameters[ID(B_WIDTH)].as_int();
							int y_width = cell->parameters[ID(Y_WIDTH)].as_int();
							log_assert(a_width == b_width && y_width == 1);
							fn.parameters = {{ID(WIDTH), a_width}};
						}else if(cell->type.in({ID($reduce_and), ID($reduce_or), ID($reduce_xor), ID($reduce_bool), ID($logic_not), ID($not)})){
							int a_width = cell->parameters[ID(A_WIDTH)].as_int();
							int y_width = cell->parameters[ID(Y_WIDTH)].as_int();
							log_assert(y_width == 1);
							fn.parameters = {{ID(WIDTH), a_width}};
						}else if(cell->type == ID($shl)){
							int y_width = cell->parameters[ID(Y_WIDTH)].as_int();
							fn.parameters.insert({ID(WIDTH), y_width});
						}
						fn.width = output_width(cell);
						node.set_function(fn);
						auto connections = cell->connections();
						connections.sort();
						for (auto const &conn : connections) {
							if (!dm.celltypes.cell_input(cell->type, conn.first))
								continue;

							node.append_arg(enqueue(DriveChunkPort(cell, conn)));
						}
					} else if (chunk.is_none()) {
						node.set_function(ExampleFn(ID($$undriven), chunk.size()));

					} else {
						log_error("unhandled drivespec: %s\n", log_signal(chunk));
						log_abort();
					}
				} else {
					log_abort();
				}
			}

			// Perform topo sort and detect SCCs
			ExampleGraph::SccAdaptor compute_graph_scc(compute_graph);

			bool scc = false;
			std::vector<int> perm;
			topo_sorted_sccs(compute_graph_scc, [&](int *begin, int *end) {
				perm.insert(perm.end(), begin, end);
				if (end > begin + 1)
				{
					log_warning("SCC:");
					for (int *i = begin; i != end; ++i)
						log(" %d(%s)(%s)", *i, compute_graph[*i].function().name.c_str(), compute_graph[*i].has_sparse_attr() ? compute_graph[*i].sparse_attr().c_str() : "");
					log("\n");
					scc = true;
				}
			}, /* sources_first */ true);
			compute_graph.permute(perm);
			if(scc) log_error("combinational loops, aborting\n");

			// Forward $$buf unless we have a name in the sparse attribute
			std::vector<int> alias;
			perm.clear();

			for (int i = 0; i < compute_graph.size(); ++i)
			{
				if (compute_graph[i].function().name == ID($$buf) && !compute_graph[i].has_sparse_attr() && compute_graph[i].arg(0).index() < i)
				{

					alias.push_back(alias[compute_graph[i].arg(0).index()]);
				}
				else
				{
					alias.push_back(GetSize(perm));
					perm.push_back(i);
				}
			}
			compute_graph.permute(perm, alias);
			return compute_graph;
	}

	void printCpp(std::string const & name, ExampleGraph &compute_graph)
	{
		dict<IdString, int> inputs, state;
		CppWriter f("example.cc");

		// Dump the compute graph
		for (int i = 0; i < compute_graph.size(); ++i)
		{
			auto ref = compute_graph[i];
			if(ref.function().name == ID($$input))
				inputs[ref.function().parameters.begin()->first] = ref.function().width;
			if(ref.function().name == ID($$state))
				state[ref.function().parameters.begin()->first] = ref.function().width;
		}
		f.printf("#include \"mysim.h\"\n");
		CppStruct input_struct(name + "_Inputs");
		for (auto const &input : inputs)
			input_struct.insert(input.first, "Signal<" + std::to_string(input.second) + ">");
		CppStruct output_struct(name + "_Outputs");
		for (auto const &key : compute_graph.keys())
			if(state.count(key.first) == 0)
				output_struct.insert(key.first, "Signal<" + std::to_string(compute_graph[key.second].function().width) + ">");
		CppStruct state_struct(name + "_State");
		for (auto const &state_var : state)
			state_struct.insert(state_var.first, "Signal<" + std::to_string(state_var.second) + ">");

		idict<std::string> node_names;
		CppScope locals;

		input_struct.print(f);
		output_struct.print(f);
		state_struct.print(f);

		f.printf("void %s(%s_Inputs const &input, %s_Outputs &output, %s_State const &current_state, %s_State &next_state)\n{\n", name.c_str(), name.c_str(), name.c_str(), name.c_str(), name.c_str());
		locals.reserve("input");
		locals.reserve("output");
		locals.reserve("current_state");
		locals.reserve("next_state");
		for (int i = 0; i < compute_graph.size(); ++i)
		{
			auto ref = compute_graph[i];
			int width = ref.function().width;
			std::string name;
			f.printf("/* %d */", ref.attr());
			if(ref.has_sparse_attr())
				name = locals.insert(ref.sparse_attr());
			else
				name = locals.insert("\\n" + std::to_string(i));
			node_names(name);
			if(ref.function().name == ID($$input))
				f.printf("\tSignal<%d> %s = input.%s;\n", width, name.c_str(), input_struct[ref.function().parameters.begin()->first].c_str());
			else if(ref.function().name == ID($$state))
				f.printf("\tSignal<%d> %s = current_state.%s;\n", width, name.c_str(), state_struct[ref.function().parameters.begin()->first].c_str());
			else if(ref.function().name == ID($$buf))
				f.printf("\tSignal<%d> %s = %s;\n", width, name.c_str(), node_names[ref.arg(0).index()].c_str());
			else if(ref.function().name == ID($$cell_output))
				f.printf("\tSignal<%d> %s = %s.%s;\n", width, name.c_str(), node_names[ref.arg(0).index()].c_str(), RTLIL::unescape_id(ref.function().parameters.begin()->first).c_str());
			else if(ref.function().name == ID($$const))
				f.printf("\tSignal<%d> %s = $const<%d>(%d);\n", width, name.c_str(), width, ref.function().parameters.begin()->second.as_int());
			else if(ref.function().name == ID($$undriven))
				f.printf("\tSignal<%d> %s; //undriven\n", width, name.c_str());
			else if(ref.function().name == ID($$slice))
				f.printf("\tSignal<%d> %s = slice<%d>(%s, %d);\n", width, name.c_str(), width, node_names[ref.arg(0).index()].c_str(), ref.function().parameters.at(ID(offset)).as_int());
			else if(ref.function().name == ID($$concat)){
				f.printf("\tauto %s = concat(", name.c_str());
				for (int i = 0, end = ref.size(); i != end; ++i){
					if(i > 0)
						f.printf(", ");
					f.printf("%s", node_names[ref.arg(i).index()].c_str());
				}
				f.printf(");\n");
			}else{
				f.printf("\t");
				if(ref.function().width > 0)
					f.printf("Signal<%d>", ref.function().width);
				else
					f.printf("%s_Outputs", log_id(ref.function().name));
				f.printf(" %s = %s", name.c_str(), log_id(ref.function().name));
				if(ref.function().parameters.count(ID(WIDTH))){
					f.printf("<%d>", ref.function().parameters.at(ID(WIDTH)).as_int());
				}
				f.printf("(");
				for (int i = 0, end = ref.size(); i != end; ++i)
					f.printf("%s%s", i>0?", ":"", node_names[ref.arg(i).index()].c_str());
				f.printf("); //");
				for (auto const &param : ref.function().parameters)
				{
					if (param.second.empty())
						f.printf("[%s]", log_id(param.first));
					else
						f.printf("[%s=%s]", log_id(param.first), log_const(param.second));
				}
				f.printf("\n");
			}
		}

		for (auto const &key : compute_graph.keys())
		{
			f.printf("\t%s.%s = %s;\n", state.count(key.first) > 0 ? "next_state" : "output", state_struct[key.first].c_str(), node_names[key.second].c_str());
		}
		f.printf("}\n");
	}

	void execute(std::vector<std::string> args, RTLIL::Design *design) override
	{
		size_t argidx = 1;
		extra_args(args, argidx, design);

		for (auto module : design->selected_modules()) {
			auto compute_graph = calculate_compute_graph(module);
			printCpp(RTLIL::unescape_id(module->name), compute_graph);
		}
		log("Plugin test passed!\n");
	}
} ExampleEmPass;

PRIVATE_NAMESPACE_END
