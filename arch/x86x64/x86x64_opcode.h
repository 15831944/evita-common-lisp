#if MACH == MACH_x86
    #include "../x86/x86_opcode.h"
#elif MACH == MACH_x64
    #include "../x64/x64_opcode.h"
#else
    #error Unspported MACH
#endif // MACH == MACH_x86
