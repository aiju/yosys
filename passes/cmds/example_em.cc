#include "kernel/yosys.h"
#include "kernel/drivertools.h"
#include "kernel/topo_scc.h"
#include "kernel/functional.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

const char *reserved_keywords[] = {
	"alignas","alignof","and","and_eq","asm","atomic_cancel","atomic_commit",
	"atomic_noexcept","auto","bitand","bitor","bool","break","case",
	"catch","char","char16_t","char32_t","char8_t","class","co_await",
	"co_return","co_yield","compl","concept","const","const_cast","consteval",
	"constexpr","constinit","continue","decltype","default","delete",
	"do","double","dynamic_cast","else","enum","explicit","export",
	"extern","false","float","for","friend","goto","if","inline",
	"int","long","mutable","namespace","new","noexcept","not","not_eq",
	"nullptr","operator","or","or_eq","private","protected","public",
	"reflexpr","register","reinterpret_cast","requires","return","short",
	"signed","sizeof","static","static_assert","static_cast","struct",
	"switch","synchronized","template","this","thread_local","throw",
	"true","try","typedef","typeid","typename","union","unsigned",
	"using","virtual","void","volatile","wchar_t","while","xor","xor_eq",
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

struct CppFunction {
	IdString name;
	int width;
	dict<IdString, Const> parameters;

	CppFunction(IdString name, int width) : name(name), width(width) {}
	CppFunction(IdString name, int width, dict<IdString, Const> parameters) : name(name), width(width), parameters(parameters) {}

	bool operator==(CppFunction const &other) const {
		return name == other.name && parameters == other.parameters && width == other.width;
	}

	unsigned int hash() const {
		return mkhash(name.hash(), parameters.hash());
	}
};

typedef ComputeGraph<CppFunction, int, IdString, IdString> CppComputeGraph;

class CppComputeGraphFactory {
	CppComputeGraph &graph;
	using T = CppComputeGraph::Ref;
	static bool is_single_output(IdString type)
	{
		auto it = yosys_celltypes.cell_types.find(type);
		return it != yosys_celltypes.cell_types.end() && it->second.outputs.size() <= 1;
	}
public:
	CppComputeGraphFactory(CppComputeGraph &g) : graph(g) {}
	T slice(T a, int in_width, int offset, int out_width) {
		assert(offset + out_width <= in_width);
		return graph.add(CppFunction(ID($$slice), out_width, {{ID(offset), offset}}), 0, std::array<T, 1>{a});
	}
	T extend(T a, int in_width, int out_width, bool is_signed) {
		assert(in_width < out_width);
		if(is_signed)
			return graph.add(CppFunction(ID($sign_extend), out_width, {{ID(WIDTH), out_width}}), 0, std::array<T, 1>{a});
		else
			return graph.add(CppFunction(ID($zero_extend), out_width, {{ID(WIDTH), out_width}}), 0, std::array<T, 1>{a});
	}
	T concat(T a, int a_width, T b, int b_width) {
		return graph.add(CppFunction(ID($$concat), a_width + b_width), 0, std::array<T, 2>{a, b});
	}
	T add(T a, T b, int width) { return graph.add(CppFunction(ID($add), width), 0, std::array<T, 2>{a, b}); }
	T sub(T a, T b, int width) { return graph.add(CppFunction(ID($sub), width), 0, std::array<T, 2>{a, b}); }
	T bitwise_and(T a, T b, int width) { return graph.add(CppFunction(ID($and), width), 0, std::array<T, 2>{a, b}); }
	T bitwise_or(T a, T b, int width) { return graph.add(CppFunction(ID($or), width), 0, std::array<T, 2>{a, b}); }
	T bitwise_xor(T a, T b, int width) { return graph.add(CppFunction(ID($xor), width), 0, std::array<T, 2>{a, b}); }
	T bitwise_not(T a, int width) { return graph.add(CppFunction(ID($not), width), 0, std::array<T, 1>{a}); }
	T neg(T a, int width) { return graph.add(CppFunction(ID($neg), width), 0, std::array<T, 1>{a}); }
	T mux(T a, T b, T s, int width) { return graph.add(CppFunction(ID($mux), width), 0, std::array<T, 3>{a, b, s}); }
	T pmux(T a, T b, T s, int width, int) { return graph.add(CppFunction(ID($pmux), width), 0, std::array<T, 3>{a, b, s}); }
	T reduce_and(T a, int) { return graph.add(CppFunction(ID($reduce_and), 1), 0, std::array<T, 1>{a}); }
	T reduce_or(T a, int) { return graph.add(CppFunction(ID($reduce_or), 1), 0, std::array<T, 1>{a}); }
	T reduce_xor(T a, int) { return graph.add(CppFunction(ID($reduce_xor), 1), 0, std::array<T, 1>{a}); }
	T eq(T a, T b, int) { return graph.add(CppFunction(ID($eq), 1), 0, std::array<T, 2>{a, b}); }
	T ne(T a, T b, int) { return graph.add(CppFunction(ID($ne), 1), 0, std::array<T, 2>{a, b}); }
	T gt(T a, T b, int) { return graph.add(CppFunction(ID($gt), 1), 0, std::array<T, 2>{a, b}); }
	T ge(T a, T b, int) { return graph.add(CppFunction(ID($ge), 1), 0, std::array<T, 2>{a, b}); }
	T ugt(T a, T b, int) { return graph.add(CppFunction(ID($ugt), 1), 0, std::array<T, 2>{a, b}); }
	T uge(T a, T b, int) { return graph.add(CppFunction(ID($uge), 1), 0, std::array<T, 2>{a, b}); }
	T logical_shift_left(T a, T b, int y_width, int) { return graph.add(CppFunction(ID($shl), y_width, {{ID(WIDTH), y_width}}), 0, std::array<T, 2>{a, b}); }
	T logical_shift_right(T a, T b, int y_width, int) { return graph.add(CppFunction(ID($shr), y_width, {{ID(WIDTH), y_width}}), 0, std::array<T, 2>{a, b}); }
	T arithmetic_shift_right(T a, T b, int y_width, int) { return graph.add(CppFunction(ID($asr), y_width, {{ID(WIDTH), y_width}}), 0, std::array<T, 2>{a, b}); }

	T constant(RTLIL::Const value) {
		return graph.add(CppFunction(ID($$const), value.size(), {{ID(value), value}}), 0);
	}
	T input(IdString name, int width) { return graph.add(CppFunction(ID($$input), width, {{name, {}}}), 0); }
	T state(IdString name, int width) { return graph.add(CppFunction(ID($$state), width, {{name, {}}}), 0); }
	T cell_output(T cell, IdString type, IdString name, int width) {
		if (is_single_output(type))
			return cell;
		else
			return graph.add(CppFunction(ID($$cell_output), width, {{name, {}}}), 0, std::array<T, 1>{cell});
	}
	T multiple(vector<T> args, int width) {
		return graph.add(CppFunction(ID($$multiple), width), 0, args);
	}
	T undriven(int width) {
		return graph.add(CppFunction(ID($$undriven), width), 0);
	}

	T create_pending(int width) {
		return graph.add(CppFunction(ID($$pending), width), 0);
	}
	void update_pending(T pending, T node) {
		assert(pending.function().name == ID($$pending));
		pending.set_function(CppFunction(ID($$buf), pending.function().width));
		pending.append_arg(node);
	}
	void declare_output(T node, IdString name) {
		node.assign_key(name);
	}
	void declare_state(T node, IdString name) {
		node.assign_key(name);
	}
	void suggest_name(T node, IdString name) {
		node.sparse_attr() = name;
	}
};

template <class T, class Factory>
class CellSimplifier {
	Factory &factory;
	T reduce_shift_width(T b, int b_width, int y_width, int &reduced_b_width) {
		log_assert(y_width > 0);
		int new_width = sizeof(int) * 8 - __builtin_clz(y_width);
		if (b_width <= new_width) {
			reduced_b_width = b_width;
			return b;
		} else {
			reduced_b_width = new_width;
			T lower_b = factory.slice(b, b_width, 0, new_width);
			T overflow = factory.gt(b, factory.constant(RTLIL::Const(y_width, b_width)), b_width);
			return factory.mux(lower_b, factory.constant(RTLIL::Const(y_width, new_width)), overflow, new_width);
		}
	}
public:
	T reduce_or(T a, int width) {
		if (width == 1)
			return a;
		return factory.reduce_or(a, width);
	}
	T extend(T a, int in_width, int out_width, bool is_signed) {
		if(in_width == out_width)
			return a;
		if(in_width > out_width)
			return factory.slice(a, in_width, 0, out_width);
		return factory.extend(a, in_width, out_width, is_signed);
	}
	T logical_shift_left(T a, T b, int y_width, int b_width) {
		int reduced_b_width;
		T reduced_b = reduce_shift_width(b, b_width, y_width, reduced_b_width);
		return factory.logical_shift_left(a, reduced_b, y_width, reduced_b_width);
	}
	T logical_shift_right(T a, T b, int y_width, int b_width) {
		int reduced_b_width;
		T reduced_b = reduce_shift_width(b, b_width, y_width, reduced_b_width);
		return factory.logical_shift_right(a, reduced_b, y_width, reduced_b_width);
	}
	T arithmetic_shift_right(T a, T b, int y_width, int b_width) {
		int reduced_b_width;
		T reduced_b = reduce_shift_width(b, b_width, y_width, reduced_b_width);
		return factory.arithmetic_shift_right(a, reduced_b, y_width, reduced_b_width);
	}
	CellSimplifier(Factory &f) : factory(f) {}
	T handle(IdString cellType, dict<IdString, Const> parameters, dict<IdString, T> inputs)
	{
		int a_width = parameters.at(ID(A_WIDTH), Const(-1)).as_int();
		int b_width = parameters.at(ID(B_WIDTH), Const(-1)).as_int();
		int y_width = parameters.at(ID(Y_WIDTH), Const(-1)).as_int();
		bool a_signed = parameters.at(ID(A_SIGNED), Const(0)).as_bool();
		bool b_signed = parameters.at(ID(B_SIGNED), Const(0)).as_bool();
		if(cellType.in({ID($add), ID($sub), ID($and), ID($or), ID($xor), ID($xnor)})){
			bool is_signed = a_signed && b_signed;
			T a = extend(inputs.at(ID(A)), a_width, y_width, is_signed);
			T b = extend(inputs.at(ID(B)), b_width, y_width, is_signed);
			if(cellType == ID($add))
				return factory.add(a, b, y_width);
			else if(cellType == ID($sub))
				return factory.sub(a, b, y_width);
			else if(cellType == ID($and))
				return factory.bitwise_and(a, b, y_width);
			else if(cellType == ID($or))
				return factory.bitwise_or(a, b, y_width);
			else if(cellType == ID($xor))
				return factory.bitwise_xor(a, b, y_width);
			else if(cellType == ID($xnor))
				return factory.bitwise_not(factory.bitwise_xor(a, b, y_width), y_width);
			else
				log_abort();
		}else if(cellType.in({ID($eq), ID($ne), ID($eqx), ID($nex), ID($le), ID($lt), ID($ge), ID($gt)})){
			bool is_signed = a_signed && b_signed;
			int width = max(a_width, b_width);
			T a = extend(inputs.at(ID(A)), a_width, width, is_signed);
			T b = extend(inputs.at(ID(B)), b_width, width, is_signed);
			if(cellType.in({ID($eq), ID($eqx)}))
				return extend(factory.eq(a, b, width), 1, y_width, false);
			if(cellType.in({ID($ne), ID($nex)}))
				return extend(factory.ne(a, b, width), 1, y_width, false);
			else if(cellType == ID($lt))
				return extend(is_signed ? factory.gt(b, a, width) : factory.ugt(b, a, width), 1, y_width, false);
			else if(cellType == ID($le))
				return extend(is_signed ? factory.ge(b, a, width) : factory.uge(b, a, width), 1, y_width, false);
			else if(cellType == ID($gt))
				return extend(is_signed ? factory.gt(a, b, width) : factory.ugt(a, b, width), 1, y_width, false);
			else if(cellType == ID($ge))
				return extend(is_signed ? factory.ge(a, b, width) : factory.uge(a, b, width), 1, y_width, false);
			else
				log_abort();
		}else if(cellType.in({ID($logic_or), ID($logic_and)})){
			T a = reduce_or(inputs.at(ID(A)), a_width);
			T b = reduce_or(inputs.at(ID(B)), b_width);
			T y = cellType == ID($logic_and) ? factory.bitwise_and(a, b, 1) : factory.bitwise_or(a, b, 1);
			return extend(y, 1, y_width, false);
		}else if(cellType == ID($not)){
			T a = extend(inputs.at(ID(A)), a_width, y_width, a_signed);
			return factory.bitwise_not(a, y_width);
		}else if(cellType == ID($pos)){
			return extend(inputs.at(ID(A)), a_width, y_width, a_signed);
		}else if(cellType == ID($neg)){
			T a = extend(inputs.at(ID(A)), a_width, y_width, a_signed);
			return factory.neg(a, y_width);
		}else if(cellType == ID($logic_not)){
			T a = reduce_or(inputs.at(ID(A)), a_width);
			T y = factory.bitwise_not(a, 1);
			return extend(y, 1, y_width, false);
		}else if(cellType.in({ID($reduce_or), ID($reduce_bool)})){
			T a = reduce_or(inputs.at(ID(A)), a_width);
			return extend(a, 1, y_width, false);
		}else if(cellType == ID($reduce_and)){
			T a = factory.reduce_and(inputs.at(ID(A)), a_width);
			return extend(a, 1, y_width, false);
		}else if(cellType.in({ID($reduce_xor), ID($reduce_xnor)})){
			T a = factory.reduce_xor(inputs.at(ID(A)), a_width);
			T y = cellType == ID($reduce_xnor) ? factory.bitwise_not(a, 1) : a;
			return extend(y, 1, y_width, false);
		}else if(cellType == ID($shl) || cellType == ID($sshl)){
			T a = extend(inputs.at(ID(A)), a_width, y_width, a_signed);
			T b = inputs.at(ID(B));
			return logical_shift_left(a, b, y_width, b_width);
		}else if(cellType == ID($shr) || cellType == ID($sshr)){
			int width = max(a_width, y_width);
			T a = extend(inputs.at(ID(A)), a_width, width, a_signed);
			T b = inputs.at(ID(B));
			T y = a_signed && cellType == ID($sshr) ?
				arithmetic_shift_right(a, b, width, b_width) :
				logical_shift_right(a, b, width, b_width);
			return extend(y, width, y_width, a_signed);
		}else if(cellType == ID($shiftx) || cellType == ID($shift)){
			int width = max(a_width, y_width);
			T a = extend(inputs.at(ID(A)), a_width, width, cellType == ID($shift) && a_signed);
			T b = inputs.at(ID(B));
			T shr = logical_shift_right(a, b, width, b_width);
			if(b_signed) {
				T sign_b = factory.slice(b, b_width, b_width - 1, 1);
				T shl = logical_shift_left(a, factory.neg(b, b_width), width, b_width);
				T y = factory.mux(shr, shl, sign_b, width);
				return extend(y, width, y_width, false);
			} else {
				return extend(shr, width, y_width, false);
			}
		}else if(cellType == ID($mux)){
			int width = parameters.at(ID(WIDTH)).as_int();
			return factory.mux(inputs.at(ID(A)), inputs.at(ID(B)), inputs.at(ID(S)), width);
		}else if(cellType == ID($pmux)){
			int width = parameters.at(ID(WIDTH)).as_int();
			int s_width = parameters.at(ID(S_WIDTH)).as_int();
			return factory.pmux(inputs.at(ID(A)), inputs.at(ID(B)), inputs.at(ID(S)), width, s_width);
		}else if(cellType == ID($concat)){
			T a = inputs.at(ID(A));
			T b = inputs.at(ID(B));
			return factory.concat(a, a_width, b, b_width);
		}else if(cellType == ID($slice)){
			int offset = parameters.at(ID(OFFSET)).as_int();
			T a = inputs.at(ID(A));
			return factory.slice(a, a_width, offset, y_width);
		}else{
			log_error("unhandled cell in CellSimplifier %s\n", cellType.c_str());
		}
	}
};

template <class T, class Factory>
class ComputeGraphConstruction {
	std::deque<DriveSpec> queue;
	dict<DriveSpec, T> graph_nodes;
	idict<Cell *> cells;
	DriverMap driver_map;
	Factory& factory;
	CellSimplifier<T, Factory> simplifier;

	T enqueue(DriveSpec const &spec)
	{
		auto it = graph_nodes.find(spec);
		if(it == graph_nodes.end()){
			auto node = factory.create_pending(spec.size());
			graph_nodes.insert({spec, node});
			queue.emplace_back(spec);
			return node;
		}else
			return it->second;
	}
public:
	ComputeGraphConstruction(Factory &f) : factory(f), simplifier(f) {}
	void add_module(Module *module)
	{
		driver_map.add(module);
		for (auto cell : module->cells()) {
			if (cell->type.in(ID($assert), ID($assume), ID($cover), ID($check)))
				enqueue(DriveBitMarker(cells(cell), 0));
		}
		for (auto wire : module->wires()) {
			if (wire->port_output) {
				T node = enqueue(DriveChunk(DriveChunkWire(wire, 0, wire->width)));
				factory.declare_output(node, wire->name);
			}
		}
	}
	void process_queue()
	{
		for (; !queue.empty(); queue.pop_front()) {
			DriveSpec spec = queue.front();
			T pending = graph_nodes.at(spec);

			if (spec.chunks().size() > 1) {
				auto chunks = spec.chunks();
				T node = enqueue(chunks[0]);
				int width = chunks[0].size();
				for(size_t i = 1; i < chunks.size(); i++) {
					node = factory.concat(node, width, enqueue(chunks[i]), chunks[i].size());
					width += chunks[i].size();
				}
				factory.update_pending(pending, node);
			} else if (spec.chunks().size() == 1) {
				DriveChunk chunk = spec.chunks()[0];
				if (chunk.is_wire()) {
					DriveChunkWire wire_chunk = chunk.wire();
					if (wire_chunk.is_whole()) {
						if (wire_chunk.wire->port_input) {
							T node = factory.input(wire_chunk.wire->name, wire_chunk.width);
							factory.suggest_name(node, wire_chunk.wire->name);
							factory.update_pending(pending, node);
						} else {
							DriveSpec driver = driver_map(DriveSpec(wire_chunk));
							T node = enqueue(driver);
							factory.suggest_name(node, wire_chunk.wire->name);
							factory.update_pending(pending, node);
						}
					} else {
						DriveChunkWire whole_wire(wire_chunk.wire, 0, wire_chunk.wire->width);
						T node = factory.slice(enqueue(whole_wire), wire_chunk.wire->width, wire_chunk.offset, wire_chunk.width);
						factory.update_pending(pending, node);
					}
				} else if (chunk.is_port()) {
					DriveChunkPort port_chunk = chunk.port();
					if (port_chunk.is_whole()) {
						if (driver_map.celltypes.cell_output(port_chunk.cell->type, port_chunk.port)) {
							if (port_chunk.cell->type.in(ID($dff), ID($ff)))
							{
								Cell *cell = port_chunk.cell;
								T node = factory.state(cell->name, port_chunk.width);
								factory.suggest_name(node, port_chunk.cell->name);
								factory.update_pending(pending, node);
								for (auto const &conn : cell->connections()) {
									if (driver_map.celltypes.cell_input(cell->type, conn.first)) {
										T node = enqueue(DriveChunkPort(cell, conn));
										factory.declare_state(node, cell->name);
									}
								}
							}
							else
							{
								T cell = enqueue(DriveChunkMarker(cells(port_chunk.cell), 0, port_chunk.width));
								factory.suggest_name(cell, port_chunk.cell->name);
								T node = factory.cell_output(cell, port_chunk.cell->type, port_chunk.port, port_chunk.width);
								factory.suggest_name(node, port_chunk.cell->name.str() + "$" + port_chunk.port.str());
								factory.update_pending(pending, node);
							}
						} else {
							DriveSpec driver = driver_map(DriveSpec(port_chunk));
							factory.update_pending(pending, enqueue(driver));
						}
					} else {
						DriveChunkPort whole_port(port_chunk.cell, port_chunk.port, 0, GetSize(port_chunk.cell->connections().at(port_chunk.port)));
						T node = factory.slice(enqueue(whole_port), whole_port.width, port_chunk.offset, port_chunk.width);
						factory.update_pending(pending, node);
					}
				} else if (chunk.is_constant()) {
					T node = factory.constant(chunk.constant());
					factory.suggest_name(node, "$const" + std::to_string(chunk.size()) + "b" + chunk.constant().as_string());
					factory.update_pending(pending, node);
				} else if (chunk.is_multiple()) {
					vector<T> args;
					for (auto const &driver : chunk.multiple().multiple())
						args.push_back(enqueue(driver));
					T node = factory.multiple(args, chunk.size());
					factory.update_pending(pending, node);
				} else if (chunk.is_marker()) {
					Cell *cell = cells[chunk.marker().marker];
					dict<IdString, T> connections;
					for(auto const &conn : cell->connections()) {
						if(driver_map.celltypes.cell_input(cell->type, conn.first))
							connections.insert({ conn.first, enqueue(DriveChunkPort(cell, conn)) });
					}
					T node = simplifier.handle(cell->type, cell->parameters, connections);
					factory.update_pending(pending, node);
				} else if (chunk.is_none()) {
					T node = factory.undriven(chunk.size());
					factory.update_pending(pending, node);
				} else {
					log_error("unhandled drivespec: %s\n", log_signal(chunk));
					log_abort();
				}
			} else {
				log_abort();
			}
		}
	}
};

struct ExampleEmPass : public Pass
{
	ExampleEmPass() : Pass("example_em", "drivertools example") {}

    void help() override
	{
		//   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
		log("\n");
    }
/*
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
*/

	CppComputeGraph calculate_compute_graph(RTLIL::Module *module)
	{
		CppComputeGraph compute_graph;
		CppComputeGraphFactory factory(compute_graph);
		ComputeGraphConstruction<CppComputeGraph::Ref, CppComputeGraphFactory> construction(factory);
		construction.add_module(module);
		construction.process_queue();

		// Perform topo sort and detect SCCs
		CppComputeGraph::SccAdaptor compute_graph_scc(compute_graph);

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

		// Forward $$buf
		std::vector<int> alias;
		perm.clear();

		for (int i = 0; i < compute_graph.size(); ++i)
		{
			auto node = compute_graph[i];
			if (node.function().name == ID($$buf) && node.arg(0).index() < i)
			{
				int target_index = alias[node.arg(0).index()];
				auto target_node = compute_graph[perm[target_index]];
				if(!target_node.has_sparse_attr() && node.has_sparse_attr())
					target_node.sparse_attr() = node.sparse_attr();
				alias.push_back(target_index);
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

	void printCpp(std::string const & name, CppComputeGraph &compute_graph)
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
			else if(ref.function().name == ID($$const)){
				auto c = ref.function().parameters.begin()->second;
				if(c.size() <= 32){
					f.printf("\tSignal<%d> %s = $const<%d>(%#x);\n", width, name.c_str(), width, (uint32_t) c.as_int());
				}else{
					f.printf("\tSignal<%d> %s = $const<%d>({%#x", width, name.c_str(), width, (uint32_t) c.as_int());
					while(c.size() > 32){
						c = c.extract(32, c.size() - 32);
						f.printf(", %#x", c.as_int());
					}
					f.printf("});\n");
				}
			}else if(ref.function().name == ID($$undriven))
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
