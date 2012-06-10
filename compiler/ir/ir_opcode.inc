#if !defined(DEFIROP)
    #define DEFIROP(mp_name)    #error "You must define DEFIROP.
#endif // !defined(DEFIROP)

DEFIROP(ANCHOR)         //  pseudo
DEFIROP(BRANCH)         //  BRANCH                         %bx true false
DEFIROP(CALL)           //  CALL        ty      %vd     <- callee %vy
DEFIROP(CLOSE)          //  CLOSE                          %frame
DEFIROP(CLOSURE)        //  CLOSURE     ty      %rd     <- templ %vy
DEFIROP(COUNT)          //  COUNT       fixnum  %rd     <- %vx
DEFIROP(FRAME)          //  FRAME       ty      %rd     <- %frame ofs
DEFIROP(ENTRY)          //  ENTRY       ty      %vd     <- check
DEFIROP(EQ)             //  EQ          bool    %bd     <- %sx %sy
DEFIROP(EXIT)           //  EXIT
DEFIROP(GE)             //  GE          bool    %bd     <- %sx %sy
DEFIROP(GT)             //  GT          bool    %bd     <- %sx %sy
DEFIROP(GO)             //  GO          %rx
DEFIROP(JUMP)           //  JUMP        label
DEFIROP(LE)             //  LE          bool    %bd     <- %sx %sy
DEFIROP(LOAD)           //  LOAD        ty      %rd     <- %rx
DEFIROP(LOADTIMEVALUE)  //  LOADTIMEVALUE ty    %rd     <- form read-only-p
DEFIROP(KEYSUPPLIED)    //  KEYSUPPLIED bool    %bd     <- %keys key
DEFIROP(KEYVAL)         //  KEYVAL      ty      %rd     <- %keys key
DEFIROP(MVRESTORE)      //  MVRESTORE   ty      %vd     <- %rx
DEFIROP(MVSAVE)         //  MVSAVE      ty      %rd     <- %vx
DEFIROP(LT)             //  LT          bool    %bd     <- %sx %sy
DEFIROP(NE)             //  NE          bool    %bd     <- %sx %sy
DEFIROP(NONLOCAL)       //  NONLOCAL    ty      %sd     <-
DEFIROP(NTHVALUE)       //  NTHVALUE    ty      %rd     <- %sx %vy
DEFIROP(OPENBIND)       //  OPENBIND    frame   %frame  <- (sym %sx)*
DEFIROP(OPENBLOCK)      //  OPENBLOCK   frame   %frame  <- name label
DEFIROP(OPENCATCH)      //  OPENCATCH   frame   %frame  <- %sx label
DEFIROP(OPENFINALLY)    //  OPENFINALLY frame   %frame  <- fun %vy
DEFIROP(OPENSIMPLE)     //  OPENSIMPLE  frame   %frame  <- code
DEFIROP(OPENTAGBODY)    //  OPENTAGBODY frame   %frame  <-
DEFIROP(PARSEKEYS)      //  PARSEKEYS   ty      %keys   <- %rx allow keys
DEFIROP(PHI)            //  PHI         ty      %rd     <- %sx+
DEFIROP(PROJECT)        //  PROJECT     ty      %rd     <- %vx nth
DEFIROP(PROLOGUE)       //  PROLOGUE    ty      %vd     <- %vx rest
DEFIROP(RET)            //  RET                            %sx
DEFIROP(RETURNFROM)     //  RETURNFROM                     %rx %vy
DEFIROP(RUNTIMECAST)    //  RUNTIMECAST ty      %rd     <- %rx
DEFIROP(SELECT)         //  SELECT      ty      %rd     <- %bx %sx %sy
DEFIROP(SIGMA)          //  SIGMA       ty      %rd     <- %sx
DEFIROP(SLOT)           //  SLOT        ty      %rd     <- class slot %rx
DEFIROP(STORE)          //  STORE                          %rx %sy
DEFIROP(TAG)            //  TAG         ty      %rd     <- %frame label
DEFIROP(TBLJMP)         //  TBLJMP                         %sx label+
DEFIROP(THROW)          //  THROW                          %sx %vy
DEFIROP(TLV)            //  TLV         ty      %rd     <- tlvrec
DEFIROP(TRAPIF)         //  TRAPIF                         %bx name %vz
DEFIROP(TRAPIFNOT)      //  TRAPIFNOT                      %bx name %vz
DEFIROP(TYPEP)          //  TYPEP       bool    %bd     <- %sx type
DEFIROP(UNREACHABLE)    //  UNREACHABLE
DEFIROP(USE)            //  USE                             %sx
DEFIROP(UPVARDEF)       //  UPVARDEF    ty      %rd     <- var
DEFIROP(VALUES)         //  VALUES      ty      %vd     <- %sx*
DEFIROP(VALUESA)        //  VALUES*     ty      %vd     <- %sx+
DEFIROP(VARDEF)         //  VARDEF      ty      %rd     <- var %sy


// Optimization

DEFIROP(ADD)            // ADD ty      %rd <- %sx %sy
DEFIROP(DIV)            // DIV ty      %rd <- %sx %sy
DEFIROP(MUL)            // MUL ty      %rd <- %sx %sy
DEFIROP(NEG)            // NEG ty      %rd <- %rx
DEFIROP(TRUNCATE)       // TRUNCATE ty %rd <- %sx %sy
DEFIROP(SHL)            // SHL ty      %rd <- %sx %sy
DEFIROP(SHR)            // SHR ty      %rd <- %sx %sy
DEFIROP(SUB)            // SUB ty      %rd <- %sx %sy
DEFIROP(LOGAND)         // LOGAND ty   %rd <- %sx %sy
DEFIROP(LOGIOR)         // LOGIOR ty   %rd <- %sx %sy
DEFIROP(LOGXOR)         // LOGXOR ty   %rd <- %sx %sy
DEFIROP(LOGEQV)         // LOGEQV ty   %rd <- %sx %sy

DEFIROP(BOX)            // BOX   ty %rd <- %sx
DEFIROP(UNBOX)          // UNBOX ty %rd <- %rx

DEFIROP(BOUND)          // BOUND ty %rd <- %array %index
DEFIROP(ELT)            // ELT   ty %rd <- %sx %sy

// CG
DEFIROP(COPY)           // COPY  ty %rd <- %sx
DEFIROP(FRAMEREF)       // FRAMERF (ptr t) %rd <-
DEFIROP(PHICOPY)        // PHICOPY ty %rd <- %sx
DEFIROP(RELOAD)         // RELOAD ty %rd <- %sx
DEFIROP(SPILL)          // SPILL ty %rd <- %sx
DEFIROP(SPLIT)          // SPLIT ty %rd <- %sx
DEFIROP(STACKDEF)       // STACKDEF ty %rd <- %rx %ry
DEFIROP(STACKVEC)       // STACKVEC ty %rd <- %sx+
DEFIROP(SWAP)           // SWAP ty %qd <- %rx %ry
DEFIROP(SWAPGET)        // SWAPGET ty %rd <- %qx %ry
DEFIROP(TLVNAME)        // TLVNAME fixnum %rd <- tlvrec
DEFIROP(VARREF)         // VARREF  (ptr cell_ty) %rd <- %base %var
DEFIROP(VARSLOT)        // VARSLOT (ptr ty) %rd <- %base %var
DEFIROP(VECREF)         // VECREF (ptr ty) %rd <- %vector nth

#include cg_opcode_inc

DEFIROP(MAX_1)

#undef DEFIROP

#define DEFPROC_SAME(mp_name, mp_same) \
    void process_##mp_name(Instruction* p) { process_##mp_same(p); }
