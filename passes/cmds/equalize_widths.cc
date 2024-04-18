#include "kernel/yosys.h"
#include "kernel/drivertools.h"
#include "kernel/topo_scc.h"
#include "kernel/functional.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

struct EqualizeWidthsPass : public Pass
{
	EqualizeWidthsPass() : Pass("equalize_widths", "equalize widths") {}

    void help() override
	{
		//   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
		log("\n");
    }

	void execute(std::vector<std::string> args, RTLIL::Design *design) override
	{
		size_t argidx = 1;
		extra_args(args, argidx, design);

		for (auto module : design->selected_modules()) {
			for (auto cell : module->cells()) {
                if(cell->type.in({ID($add), ID($sub), ID($and), ID($or), ID($xor)})){
                    int a_width = cell->parameters[ID(A_WIDTH)].as_int();
                    int b_width = cell->parameters[ID(B_WIDTH)].as_int();
                    int y_width = cell->parameters[ID(Y_WIDTH)].as_int();
                    bool a_signed = cell->parameters[ID(A_SIGNED)].as_bool();
                    bool b_signed = cell->parameters[ID(B_SIGNED)].as_bool();
                    int width = max(a_width, b_width);
                    assert(y_width == width);
                    if(a_width != width){
                        SigSpec a = cell->getPort(ID(A));
                        a.extend_u0(width, a_signed);
                        cell->setPort(ID(A), a);
                        cell->setParam(ID(A_WIDTH), width);
                    }
                    if(b_width != width){
                        SigSpec b = cell->getPort(ID(B));
                        b.extend_u0(width, b_signed);
                        cell->setPort(ID(B), b);
                        cell->setParam(ID(B_WIDTH), width);
                    }
                }
                if(cell->type.in({ID($logic_or), ID($logic_and), ID($or), ID($and)})){
                    int a_width = cell->parameters[ID(A_WIDTH)].as_int();
                    int b_width = cell->parameters[ID(B_WIDTH)].as_int();
                    bool a_signed = cell->parameters[ID(A_SIGNED)].as_bool();
                    bool b_signed = cell->parameters[ID(B_SIGNED)].as_bool();
                    int width = max(a_width, b_width);
                    if(a_width != width){
                        SigSpec a = cell->getPort(ID(A));
                        a.extend_u0(width, a_signed);
                        cell->setPort(ID(A), a);
                        cell->setParam(ID(A_WIDTH), width);
                    }
                    if(b_width != width){
                        SigSpec b = cell->getPort(ID(B));
                        b.extend_u0(width, b_signed);
                        cell->setPort(ID(B), b);
                        cell->setParam(ID(B_WIDTH), width);
                    }
                }
                if(cell->type.in({ID($lt), ID($gt), ID($le), ID($ge), ID($eq), ID($ne)})){
                    int a_width = cell->parameters[ID(A_WIDTH)].as_int();
                    int b_width = cell->parameters[ID(B_WIDTH)].as_int();
                    bool a_signed = cell->parameters[ID(A_SIGNED)].as_bool();
                    bool b_signed = cell->parameters[ID(B_SIGNED)].as_bool();
                    int width = max(a_width, b_width) + (a_signed != b_signed);
                    if(a_width != width){
                        SigSpec a = cell->getPort(ID(A));
                        a.extend_u0(width, a_signed);
                        cell->setPort(ID(A), a);
                        cell->setParam(ID(A_WIDTH), width);
                        cell->setParam(ID(A_SIGNED), a_signed || b_signed);
                    }
                    if(b_width != width){
                        SigSpec b = cell->getPort(ID(B));
                        b.extend_u0(width, b_signed);
                        cell->setPort(ID(B), b);
                        cell->setParam(ID(B_WIDTH), width);
                        cell->setParam(ID(B_SIGNED), a_signed || b_signed);
                    }
                }
            }
		}
	}
} EqualizeWidthsPass;

PRIVATE_NAMESPACE_END
