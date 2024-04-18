#include "example.cc"

struct DumpHeader {
    template<size_t n>
    void operator ()(const char *name, Signal<n> value) {
        printf("$var wire %d %s %s $end\n", (int)n, name, name);
    }
};

struct Dump {
    template<size_t n>
    void operator ()(const char *name, Signal<n> value) {
        printf("b");
        for(size_t i = n; i-- > 0; )
            printf("%c", value[i] ? '1' : '0');
        printf(" %s\n", name);
    }
};

uint32_t memory[4096] = {
0x00200093,
0x00200113,
0x00111863,
0x00102023,
0x00108093,
0xfe0008e3,
0x00008193,
0x402181b3,
0xfe304ee3,
0xfe0186e3,
0x00110113,
0xfc000ee3
};

int main(int argc, char **argv)
{
    picorv32_Inputs inputs;
    picorv32_Outputs outputs;
    picorv32_State state;
    picorv32_State next_state;

    printf("$timescale 1ps $end\n$scope module logic $end\n");
    {DumpHeader d;
    inputs.dump(d);
    outputs.dump(d);
    printf("$scope module state $end\n");
    state.dump(d);}
    printf("$upscope $end\n$upscope $end\n$enddefinitions $end\n$dumpvars\n");
    {Dump d;
    inputs.dump(d);
    outputs.dump(d);
    state.dump(d);}
    printf("$end\n");

    inputs.mem_ready = $const<1>(0);
    for(int i = 0; i < 40000; i++){
        printf("#%d\n", i);
        inputs.resetn = $const<1>(i > 0);
        picorv32(inputs, outputs, state, next_state);
        {Dump d;
        inputs.dump(d);
        outputs.dump(d);
        state.dump(d);}
        state = next_state;

        int addr = as_int(outputs.mem_addr);
        if(as_bool(outputs.mem_valid) && as_bool(outputs.mem_wstrb) && as_bool(inputs.mem_ready)){
            fprintf(stderr, "%d\n", as_int(outputs.mem_wdata));
        }
        inputs.mem_rdata = $const<32>(addr < sizeof(memory) ? memory[addr / 4] : 0);
        inputs.mem_ready = {!as_bool(inputs.mem_ready) && as_bool(outputs.mem_valid)};
    }
}