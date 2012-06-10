#if MACH == MACH_x86
    #include "../x86/x86_arch.h"
    using namespace X86;
#elif MACH == MACH_x64
    #include "../x64/x64_arch.h"
    using namespace X64;
#else
    #error Unspported MACH
#endif // MACH == MACH_x86
