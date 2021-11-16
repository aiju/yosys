/*
 *  yosys -- Yosys Open SYnthesis Suite
 *
 *  Copyright (C) 2012  Aki "lethalbit" Van Ness <aki@yosyshq.com> <aki@lethalbit.net>
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

// MYAU - Metadata for Yosys-Assisted Utilities

#include "kernel/rtlil.h"
#include "kernel/register.h"
#include "kernel/sigtools.h"
#include "kernel/celltypes.h"
#include "kernel/cellaigs.h"
#include "kernel/log.h"
#include <string>
#include <unordered_map>
#include <vector>

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN


struct MetadataWriter
{
    private:
        std::ostream &f;
        bool _use_selection;
        std::unordered_map<std::string, std::vector<Cell*>> _cells{};

        // XXX(aki): this was pulled from the json backend, needs to be pulled
        // out possibly into some sort of utilities file, or integrated into rtlil.h
        // directly
        string get_string(string str)
        {
            string newstr = "\"";
            for (char c : str) {
                if (c == '\\')
                    newstr += c;
                newstr += c;
            }
            return newstr + "\"";
        }

        // XXX(aki): I know this is far from ideal but i'm out of spoons and cant focus so
        // it'll have to do for now,
        void coalesce_cells(Module* mod)
        {
            for (auto cell : mod->cells()) {
                const auto cell_type = get_string(RTLIL::unescape_id(cell->type));

                if (_cells.find(cell_type) == _cells.end())
                    _cells.emplace(cell_type, std::vector<Cell*>());

                _cells.at(cell_type).push_back(cell);
            }
        }

    public:
    MetadataWriter(std::ostream &f, bool use_selection) noexcept: f{f}, _use_selection{use_selection} { }

    void write_metadata(Design *design)
    {
        log_assert(design != nullptr);

        design->sort();

        f << stringf("{\n");
        f << stringf("  \"generator\": %s,\n", get_string(yosys_version_str).c_str());
        // XXX(aki): Replace this with a proper version info eventually:tm:
        f << stringf("  \"version\": \"0.0.0\",\n");
        f << stringf("  \"modules\": [\n");

        bool first{true};
        for (auto mod : _use_selection ? design->selected_modules() : design->modules()) {
            if (!first)
                f << stringf(",\n");
            write_module(mod);

            f << stringf("\n");

            first = false;
        }

        f << stringf("  ]\n");
        f << stringf("}\n");
    }

    void write_module(Module* mod)
    {
        log_assert(mod != nullptr);

        coalesce_cells(mod);

        f << stringf("    {\n");
        f << stringf("      \"name\": %s,\n", get_string(RTLIL::unescape_id(mod->name)).c_str());
        f << stringf("      \"cell_sorts\": [\n");

        bool first_sort{true};
        for (auto& sort : _cells) {
            if (!first_sort)
                f << stringf(",\n");

            write_cell_sort(sort);

            first_sort = false;
        }
        f << stringf("\n");

        f << stringf("      ],\n");
        f << stringf("      \"connections\": [\n");

        f << stringf("      ]\n");
        f << stringf("    }");
    }


    void write_cell_sort(std::pair<const std::string, std::vector<Cell*>>& sort)
    {
        const auto port_cell = sort.second.front();

        f << stringf("        {\n");
        f << stringf("          \"type\": %s,\n", sort.first.c_str());
        f << stringf("          \"ports\": [\n");

        bool first_port{true};
        for (auto con : port_cell->connections()) {
            if (!first_port)
                f << stringf(",\n");

            f << stringf("          {\n");
            f << stringf("            \"name\": %s,\n", get_string(RTLIL::unescape_id(con.first)).c_str());
            f << stringf("            \"direction\": \"");
            if (port_cell->input(con.first))
                f << stringf("i");
            if (port_cell->input(con.first))
                f << stringf("o");
            f << stringf("\",\n");
            if (con.second.size() == 1)
                f << stringf("            \"range\": [0, 0]\n");
            else
                f << stringf("            \"range\": [%d, %d]\n", con.second.size(), 0);
            f << stringf("          }");

            first_port = false;
        }
        f << stringf("\n");

        f << stringf("          ],\n          \"cells\": [\n");
        bool first_cell{true};
        for (auto& cell : sort.second) {
            if (!first_cell)
                f << stringf(",\n");

            write_cell(cell);

            first_cell = false;
        }
        f << stringf("\n");
        f << stringf("          ]\n");
        f << stringf("        }");
    }

    void write_cell(Cell* cell)
    {
        log_assert(cell != nullptr);

        f << stringf("            {\n");
        f << stringf("              \"name\": %s,\n", get_string(RTLIL::unescape_id(cell->name)).c_str());
        f << stringf("              \"attributes\": {\n");

        bool first_attr{true};
        for (auto& attr : cell->attributes) {
            if (!first_attr)
                f << stringf(",\n");
            const auto attr_val = attr.second.decode_string();
            if (attr_val.size() > 0)
                f << stringf("                %s: \"%s\"\n", get_string(RTLIL::unescape_id(attr.first)).c_str(), attr_val.c_str());
            else
                f << stringf("                %s: true\n", get_string(RTLIL::unescape_id(attr.first)).c_str());

            first_attr = false;
        }

        f << stringf("              },\n");
        f << stringf("              \"parameters\": {\n");

        bool first_param{true};
        for (auto& param : cell->parameters) {
            if (!first_param)
                f << stringf(",\n");
            const auto param_val = param.second.decode_string();
            if (param_val.size() > 0)
                f << stringf("                %s: \"%s\"\n", get_string(RTLIL::unescape_id(param.first)).c_str(), param_val.c_str());
            else
                f << stringf("                %s: true\n", get_string(RTLIL::unescape_id(param.first)).c_str());

            first_param = false;
        }

        f << stringf("              },\n");
        f << stringf("            }");
    }
};

struct MetadataBackend : public Backend {
    MetadataBackend() : Backend("metadata", "generate design metadata") { }
    void help() override
    {
        //   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
        log("\n");
        log("    metadata [options] [selection]\n");
        log("\n");
        log("Write a JSON metadata for the current design\n");
        log("\n");
        log("\n");
    }

    void execute(std::ostream *&f, std::string filename, std::vector<std::string> args, RTLIL::Design *design) override
    {
        size_t argidx{1};
        extra_args(f, filename, args, argidx);

        log_header(design, "Executing metadata backend.\n");

        MetadataWriter metadata_writier(*f, false);
        metadata_writier.write_metadata(design);
    }

} MetadataBackend;


struct MetadataPass : public Pass {
    MetadataPass() : Pass("metadata", "write design metadata") { }

    void help() override
    {
        //   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
        log("\n");
        log("    metadata [options] [selection]\n");
        log("\n");
        log("Write a JSON metadata for the current design\n");
        log("\n");
        log("    -o <filename>\n");
        log("        write to the specified file.\n");
        log("\n");
        log("See 'help write_metadata' for a description of the JSON format used.\n");
        log("\n");
    }
    void execute(std::vector<std::string> args, RTLIL::Design *design) override
    {
        std::string filename{};

        size_t argidx;
        for (argidx = 1; argidx < args.size(); argidx++)
        {
            if (args[argidx] == "-o" && argidx+1 < args.size()) {
                filename = args[++argidx];
                continue;
            }
            break;
        }
        extra_args(args, argidx, design);

        std::ostream *f;
        std::stringstream buf;

        if (!filename.empty()) {
            rewrite_filename(filename);
            std::ofstream *ff = new std::ofstream;
            ff->open(filename.c_str(), std::ofstream::trunc);
            if (ff->fail()) {
                delete ff;
                log_error("Can't open file `%s' for writing: %s\n", filename.c_str(), strerror(errno));
            }
            f = ff;
        } else {
            f = &buf;
        }


        MetadataWriter metadata_writier(*f, false);
        metadata_writier.write_metadata(design);

        if (!filename.empty()) {
            delete f;
        } else {
            log("%s", buf.str().c_str());
        }
    }

} MetadataPass;

PRIVATE_NAMESPACE_END