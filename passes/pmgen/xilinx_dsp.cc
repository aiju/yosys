/*
 *  yosys -- Yosys Open SYnthesis Suite
 *
 *  Copyright (C) 2012  Clifford Wolf <clifford@clifford.at>
 *
 *  Permission to use, copy, modify, and/or distribute this software for any
 *  purpose with or without fee is hereby granted, provided that the above
 *  copyright notice and this permission notice appear in all copies.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 *  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 *  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 *  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 *  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#include "kernel/yosys.h"
#include "kernel/sigtools.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

#include "passes/pmgen/xilinx_dsp_pm.h"

void pack_xilinx_simd(Module *module, const std::vector<Cell*> &selected_cells)
{
	std::deque<Cell*> simd12, simd24;

	for (auto cell : selected_cells) {
		if (!cell->type.in("$add"))
			continue;
		SigSpec Y = cell->getPort("\\Y");
		if (!Y.is_chunk())
			continue;
		if (!Y.as_chunk().wire->get_strpool_attribute("\\use_dsp").count("simd"))
			continue;
		if (GetSize(Y) > 25)
			continue;
		SigSpec A = cell->getPort("\\A");
		SigSpec B = cell->getPort("\\B");
		if (GetSize(Y) <= 13) {
			if (GetSize(A) > 12)
				continue;
			if (GetSize(B) > 12)
				continue;
			simd12.push_back(cell);
		}
		else {
			if (GetSize(A) > 24)
				continue;
			if (GetSize(B) > 24)
				continue;
			simd24.push_back(cell);
		}
	}

	auto addDsp = [module] {
		Cell *cell = module->addCell(NEW_ID, "\\DSP48E1");
		cell->setParam("\\ACASCREG", 0);
		cell->setParam("\\ADREG", 0);
		cell->setParam("\\A_INPUT", Const("DIRECT"));
		cell->setParam("\\ALUMODEREG", 0);
		cell->setParam("\\AREG", 0);
		cell->setParam("\\BCASCREG", 0);
		cell->setParam("\\B_INPUT", Const("DIRECT"));
		cell->setParam("\\BREG", 0);
		cell->setParam("\\CARRYINREG", 0);
		cell->setParam("\\CARRYINSELREG", 0);
		cell->setParam("\\CREG", 0);
		cell->setParam("\\DREG", 0);
		cell->setParam("\\INMODEREG", 0);
		cell->setParam("\\MREG", 0);
		cell->setParam("\\OPMODEREG", 0);
		cell->setParam("\\PREG", 0);
		cell->setParam("\\USE_MULT", Const("NONE"));

		cell->setPort("\\D", Const(0, 24));
		cell->setPort("\\INMODE", Const(0, 5));
		cell->setPort("\\ALUMODE", Const(0, 4));
		cell->setPort("\\OPMODE", Const(0, 7));
		cell->setPort("\\CARRYINSEL", Const(0, 3));
		cell->setPort("\\ACIN", Const(0, 30));
		cell->setPort("\\BCIN", Const(0, 18));
		cell->setPort("\\PCIN", Const(0, 48));
		cell->setPort("\\CARRYIN", Const(0, 1));
		return cell;
	};

	SigSpec AB;
	SigSpec C;
	SigSpec P;
	SigSpec CARRYOUT;
	auto f12 = [&AB,&C,&P,&CARRYOUT,module](Cell *lane) {
		SigSpec A = lane->getPort("\\A");
		SigSpec B = lane->getPort("\\B");
		SigSpec Y = lane->getPort("\\Y");
		A.extend_u0(12, lane->getParam("\\A_SIGNED").as_bool());
		B.extend_u0(12, lane->getParam("\\B_SIGNED").as_bool());
		AB.append(A);
		C.append(B);
		if (GetSize(Y) < 13)
			Y.append(module->addWire(NEW_ID, 13-GetSize(Y)));
		else
			log_assert(GetSize(Y) == 13);
		P.append(Y.extract(0, 12));
		CARRYOUT.append(Y[12]);
	};
	while (simd12.size() > 1) {
		AB = SigSpec();
		C = SigSpec();
		P = SigSpec();
		CARRYOUT = SigSpec();

		Cell *lane1 = simd12.front();
		simd12.pop_front();
		Cell *lane2 = simd12.front();
		simd12.pop_front();
		Cell *lane3 = nullptr;
		Cell *lane4 = nullptr;

		if (!simd12.empty()) {
			lane3 = simd12.front();
			simd12.pop_front();
			if (!simd12.empty()) {
				lane4 = simd12.front();
				simd12.pop_front();
			}
		}

		log("Analysing %s.%s for Xilinx DSP SIMD12 packing.\n", log_id(module), log_id(lane1));

		Cell *cell = addDsp();
		cell->setParam("\\USE_SIMD", Const("FOUR12"));
		// X = A:B
		// Y = 0
		// Z = C
		cell->setPort("\\OPMODE", Const::from_string("0110011"));

		log_assert(lane1);
		log_assert(lane2);
		f12(lane1);
		f12(lane2);
		if (lane3) {
			f12(lane3);
			if (lane4)
				f12(lane4);
			else {
				AB.append(Const(0, 12));
				C.append(Const(0, 12));
				P.append(module->addWire(NEW_ID, 12));
				CARRYOUT.append(module->addWire(NEW_ID, 1));
			}
		}
		else {
			AB.append(Const(0, 24));
			C.append(Const(0, 24));
			P.append(module->addWire(NEW_ID, 24));
			CARRYOUT.append(module->addWire(NEW_ID, 2));
		}
		log_assert(GetSize(AB) == 48);
		log_assert(GetSize(C) == 48);
		log_assert(GetSize(P) == 48);
		log_assert(GetSize(CARRYOUT) == 4);
		cell->setPort("\\A", AB.extract(18, 30));
		cell->setPort("\\B", AB.extract(0, 18));
		cell->setPort("\\C", C);
		cell->setPort("\\P", P);
		cell->setPort("\\CARRYOUT", CARRYOUT);

		module->remove(lane1);
		module->remove(lane2);
		if (lane3) module->remove(lane3);
		if (lane4) module->remove(lane4);

		module->design->select(module, cell);
	}
}


void pack_xilinx_dsp(dict<SigBit, Cell*> &bit_to_driver, xilinx_dsp_pm &pm)
{
	auto &st = pm.st_xilinx_dsp;

#if 1
	log("\n");
	log("preAdd:     %s\n", log_id(st.preAdd, "--"));
	log("ffAD:       %s\n", log_id(st.ffAD, "--"));
	log("ffADmux:    %s\n", log_id(st.ffADmux, "--"));
	log("ffA:        %s\n", log_id(st.ffA, "--"));
	log("ffAmux:     %s\n", log_id(st.ffAmux, "--"));
	log("ffB:        %s\n", log_id(st.ffB, "--"));
	log("ffBmux:     %s\n", log_id(st.ffBmux, "--"));
	log("ffC:        %s\n", log_id(st.ffC, "--"));
	log("ffCmux:     %s\n", log_id(st.ffCmux, "--"));
	log("ffD:        %s\n", log_id(st.ffD, "--"));
	log("ffDmux:     %s\n", log_id(st.ffDmux, "--"));
	log("dsp:        %s\n", log_id(st.dsp, "--"));
	log("ffM:        %s\n", log_id(st.ffM, "--"));
	log("ffMmux:     %s\n", log_id(st.ffMmux, "--"));
	log("postAdd:    %s\n", log_id(st.postAdd, "--"));
	log("postAddMux: %s\n", log_id(st.postAddMux, "--"));
	log("ffP:        %s\n", log_id(st.ffP, "--"));
	log("ffPmux:     %s\n", log_id(st.ffPmux, "--"));
#endif

	log("Analysing %s.%s for Xilinx DSP packing.\n", log_id(pm.module), log_id(st.dsp));

	Cell *cell = st.dsp;
	bit_to_driver.insert(std::make_pair(cell->getPort("\\P")[17], cell));
	SigSpec P = st.sigP;

	if (st.preAdd) {
		log("  preadder %s (%s)\n", log_id(st.preAdd), log_id(st.preAdd->type));
		bool A_SIGNED = st.preAdd->getParam("\\A_SIGNED").as_bool();
		bool D_SIGNED = st.preAdd->getParam("\\B_SIGNED").as_bool();
		if (st.sigA == st.preAdd->getPort("\\B"))
			std::swap(A_SIGNED, D_SIGNED);
		st.sigA.extend_u0(30, A_SIGNED);
		st.sigD.extend_u0(25, D_SIGNED);
		cell->setPort("\\A", st.sigA);
		cell->setPort("\\D", st.sigD);
		cell->connections_.at("\\INMODE") = Const::from_string("00100");

		if (st.ffAD) {
			if (st.ffADmux) {
				SigSpec S = st.ffADmux->getPort("\\S");
				cell->setPort("\\CEAD", st.ffADenpol ? S : pm.module->Not(NEW_ID, S));
			}
			else
				cell->setPort("\\CEAD", State::S1);
			cell->setParam("\\ADREG", 1);
		}

		cell->setParam("\\USE_DPORT", Const("TRUE"));

		pm.autoremove(st.preAdd);
	}
	if (st.postAdd) {
		log("  postadder %s (%s)\n", log_id(st.postAdd), log_id(st.postAdd->type));

		SigSpec &opmode = cell->connections_.at("\\OPMODE");
		if (st.postAddMux) {
			log_assert(st.ffP);
			opmode[4] = st.postAddMux->getPort("\\S");
			pm.autoremove(st.postAddMux);
		}
		else if (st.ffP && st.sigC == P)
			opmode[4] = State::S0;
		else
			opmode[4] = State::S1;
		opmode[6] = State::S0;
		opmode[5] = State::S1;

		if (opmode[4] != State::S0) {
			if (st.postAddMuxAB == "\\A")
				st.sigC.extend_u0(48, st.postAdd->getParam("\\B_SIGNED").as_bool());
			else
				st.sigC.extend_u0(48, st.postAdd->getParam("\\A_SIGNED").as_bool());
			cell->setPort("\\C", st.sigC);
		}

		pm.autoremove(st.postAdd);
	}

	if (st.clock != SigBit())
	{
		cell->setPort("\\CLK", st.clock);

		if (st.ffA) {
			SigSpec A = cell->getPort("\\A");
			SigSpec D = st.ffA->getPort("\\D");
			SigSpec Q = pm.sigmap(st.ffA->getPort("\\Q"));
			A.replace(Q, D);
			if (st.ffAmux) {
				SigSpec Y = st.ffAmux->getPort("\\Y");
				SigSpec AB = st.ffAmux->getPort(st.ffAenpol ? "\\B" : "\\A");
				SigSpec S = st.ffAmux->getPort("\\S");
				A.replace(Y, AB);
				cell->setPort("\\CEA2", st.ffAenpol ? S : pm.module->Not(NEW_ID, S));
			}
			else
				cell->setPort("\\CEA2", State::S1);
			cell->setPort("\\A", A);

			cell->setParam("\\AREG", 1);
		}
		if (st.ffB) {
			SigSpec B = cell->getPort("\\B");
			SigSpec D = st.ffB->getPort("\\D");
			SigSpec Q = st.ffB->getPort("\\Q");
			B.replace(Q, D);
			if (st.ffBmux) {
				SigSpec Y = st.ffBmux->getPort("\\Y");
				SigSpec AB = st.ffBmux->getPort(st.ffBenpol ? "\\B" : "\\A");
				SigSpec S = st.ffBmux->getPort("\\S");
				B.replace(Y, AB);
				cell->setPort("\\CEB2", st.ffBenpol ? S : pm.module->Not(NEW_ID, S));
			}
			else
				cell->setPort("\\CEB2", State::S1);
			cell->setPort("\\B", B);

			cell->setParam("\\BREG", 1);
		}
		if (st.ffC) {
			SigSpec C = cell->getPort("\\C");
			SigSpec D = st.ffC->getPort("\\D");
			SigSpec Q = st.ffC->getPort("\\Q");
			C.replace(Q, D);

			if (st.ffCmux) {
				SigSpec Y = st.ffCmux->getPort("\\Y");
				SigSpec AB = st.ffCmux->getPort(st.ffCenpol ? "\\B" : "\\A");
				SigSpec S = st.ffCmux->getPort("\\S");
				C.replace(Y, AB);

				cell->setPort("\\CEC", st.ffCenpol ? S : pm.module->Not(NEW_ID, S));
			}
			else
				cell->setPort("\\CEC", State::S1);
			cell->setPort("\\C", C);

			cell->setParam("\\CREG", 1);
		}
		if (st.ffD) {
			SigSpec D_ = cell->getPort("\\D");
			SigSpec D = st.ffD->getPort("\\D");
			SigSpec Q = st.ffD->getPort("\\Q");
			D_.replace(Q, D);

			if (st.ffDmux) {
				SigSpec Y = st.ffDmux->getPort("\\Y");
				SigSpec AB = st.ffDmux->getPort(st.ffDenpol ? "\\B" : "\\A");
				SigSpec S = st.ffDmux->getPort("\\S");
				D_.replace(Y, AB);

				cell->setPort("\\CED", st.ffDenpol ? S : pm.module->Not(NEW_ID, S));
			}
			else
				cell->setPort("\\CED", State::S1);
			cell->setPort("\\D", D_);

			cell->setParam("\\DREG", 1);
		}
		if (st.ffM) {
			if (st.ffMmux) {
				SigSpec S = st.ffMmux->getPort("\\S");
				cell->setPort("\\CEM", st.ffMenpol ? S : pm.module->Not(NEW_ID, S));
				pm.autoremove(st.ffMmux);
			}
			else
				cell->setPort("\\CEM", State::S1);
			SigSpec D = st.ffM->getPort("\\D");
			SigSpec Q = st.ffM->getPort("\\Q");
			P.replace(pm.sigmap(D), Q);

			cell->setParam("\\MREG", State::S1);
			pm.autoremove(st.ffM);
		}
		if (st.ffP) {
			if (st.ffPmux) {
				SigSpec S = st.ffPmux->getPort("\\S");
				cell->setPort("\\CEP", st.ffPenpol ? S : pm.module->Not(NEW_ID, S));
				st.ffPmux->connections_.at("\\Y").replace(P, pm.module->addWire(NEW_ID, GetSize(P)));
			}
			else
				cell->setPort("\\CEP", State::S1);
			SigSpec D = st.ffP->getPort("\\D");
			SigSpec Q = st.ffP->getPort("\\Q");
			P.replace(pm.sigmap(D), Q);
			st.ffP->connections_.at("\\Q").replace(P, pm.module->addWire(NEW_ID, GetSize(P)));

			cell->setParam("\\PREG", State::S1);
		}

		log("  clock: %s (%s)", log_signal(st.clock), "posedge");

		if (st.ffA)
			log(" ffA:%s", log_id(st.ffA));

		if (st.ffAD)
			log(" ffAD:%s", log_id(st.ffAD));

		if (st.ffB)
			log(" ffB:%s", log_id(st.ffB));

		if (st.ffC)
			log(" ffC:%s", log_id(st.ffC));

		if (st.ffD)
			log(" ffD:%s", log_id(st.ffD));

		if (st.ffM)
			log(" ffM:%s", log_id(st.ffM));

		if (st.ffP)
			log(" ffP:%s", log_id(st.ffP));

		log("\n");
	}

	if (GetSize(P) < 48)
		P.append(pm.module->addWire(NEW_ID, 48-GetSize(P)));
	cell->setPort("\\P", P);

	pm.blacklist(cell);
}

struct XilinxDspPass : public Pass {
	XilinxDspPass() : Pass("xilinx_dsp", "Xilinx: pack DSP registers") { }
	void help() YS_OVERRIDE
	{
		//   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
		log("\n");
		log("    xilinx_dsp [options] [selection]\n");
		log("\n");
		log("Pack registers into Xilinx DSPs\n");
		log("\n");
	}
	void execute(std::vector<std::string> args, RTLIL::Design *design) YS_OVERRIDE
	{
		log_header(design, "Executing XILINX_DSP pass (pack DSPs).\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++)
		{
			// if (args[argidx] == "-singleton") {
			// 	singleton_mode = true;
			// 	continue;
			// }
			break;
		}
		extra_args(args, argidx, design);

		for (auto module : design->selected_modules()) {
			pack_xilinx_simd(module, module->selected_cells());

			xilinx_dsp_pm pm(module, module->selected_cells());
			dict<SigBit, Cell*> bit_to_driver;
			auto f = [&bit_to_driver](xilinx_dsp_pm &pm){ pack_xilinx_dsp(bit_to_driver, pm); };
			pm.run_xilinx_dsp(f);

			// Look for ability to convert C input from another DSP into PCIN
			//   NB: Needs to be done after pattern matcher has folded all
			//       $add cells into the DSP
			for (auto cell : module->cells()) {
				if (cell->type != "\\DSP48E1")
					continue;
				if (cell->parameters.at("\\CREG", State::S1).as_bool())
					continue;
				SigSpec &opmode = cell->connections_.at("\\OPMODE");
				if (opmode.extract(4,3) != Const::from_string("011"))
					continue;
				SigSpec C = pm.sigmap(cell->getPort("\\C"));
				if (C.has_const())
					continue;
				auto it = bit_to_driver.find(C[0]);
				if (it == bit_to_driver.end())
					continue;
				auto driver = it->second;

				// Unextend C
				int i;
				for (i = GetSize(C)-1; i > 0; i--)
					if (C[i] != C[i-1])
						break;
				if (i > 48-17)
					continue;
				if (driver->getPort("\\P").extract(17, i) == C.extract(0, i)) {
					cell->setPort("\\C", Const(0, 48));
					Wire *cascade = module->addWire(NEW_ID, 48);
					driver->setPort("\\PCOUT", cascade);
					cell->setPort("\\PCIN", cascade);
					opmode[6] = State::S1;
					opmode[5] = State::S0;
					opmode[4] = State::S1;
					bit_to_driver.erase(it);
				}
			}
		}
	}
} XilinxDspPass;

PRIVATE_NAMESPACE_END
