
// @note: right now these are not aliased for debugging reasons
enum legacy_prefixes{
    ASM_PREFIX_none = 0,
    
    ASM_PREFIX_F0 = 0x1,  // lock
    ASM_PREFIX_F2 = 0x2,  // repne repnz
    ASM_PREFIX_F3 = 0x4,  // rep   repe   repz
    ASM_PREFIX_64 = 0x8,  // fs override
    ASM_PREFIX_65 = 0x10, // gs override
    ASM_PREFIX_66 = 0x20, // operand size override
    ASM_PREFIX_67 = 0x40, // address size override
    
    ASM_PREFIX_lock                 = ASM_PREFIX_F0,
    ASM_PREFIX_gs                   = ASM_PREFIX_65,
    ASM_PREFIX_repe                 = ASM_PREFIX_F3,
    ASM_PREFIX_repne                = ASM_PREFIX_F2,
    
    ASM_PREFIX_SSE_double           = ASM_PREFIX_F2,
    ASM_PREFIX_SSE_float            = ASM_PREFIX_F3,
    ASM_PREFIX_SSE_packed_float     = 0,
    ASM_PREFIX_SSE_packed_double    = ASM_PREFIX_66,  // 0x66
    
    ASM_PREFIX_NON_PACKED_OP_double = ASM_PREFIX_66,  // 0x66
    ASM_PREFIX_NON_PACKED_OP_float  = 0,
    ASM_PREFIX_P_BIG                = ASM_PREFIX_66, // A p* function (padd, por, punpcklbw, ...) which is 128 bit.
    
    ASM_PREFIX_VEX = 0x100,
};

struct prefixes{
    enum legacy_prefixes legacy_prefixes;
    enum register_encoding vex_register;
};

func struct prefixes create_prefixes(enum legacy_prefixes prefixes){
    return (struct prefixes){.legacy_prefixes = prefixes};
}

func struct prefixes no_prefix(void){
    return (struct prefixes)zero_struct;
}

enum asm_register{
    ASM_REGISTER_invalid = -1,
    
    ASM_REGISTER_rax =  0,
    ASM_REGISTER_rcx =  1,
    ASM_REGISTER_rdx =  2,
    ASM_REGISTER_rbx =  3,
    ASM_REGISTER_rsp =  4,
    ASM_REGISTER_rbp =  5,
    ASM_REGISTER_rsi =  6,
    ASM_REGISTER_rdi =  7,
    ASM_REGISTER_r8  =  8,
    ASM_REGISTER_r9  =  9,
    ASM_REGISTER_r10 = 10,
    ASM_REGISTER_r11 = 11,
    ASM_REGISTER_r12 = 12,
    ASM_REGISTER_r13 = 13,
    ASM_REGISTER_r14 = 14,
    ASM_REGISTER_r15 = 15,
    
    // WARNING: this has to work with 'emit_inline_asm_block'
    
    ASM_REGISTER_xmm0  =  0,
    ASM_REGISTER_xmm1  =  1,
    ASM_REGISTER_xmm2  =  2,
    ASM_REGISTER_xmm3  =  3,
    ASM_REGISTER_xmm4  =  4,
    ASM_REGISTER_xmm5  =  5,
    ASM_REGISTER_xmm6  =  6,
    ASM_REGISTER_xmm7  =  7,
    ASM_REGISTER_xmm8  =  8,
    ASM_REGISTER_xmm9  =  9,
    ASM_REGISTER_xmm10 = 10,
    ASM_REGISTER_xmm11 = 11,
    ASM_REGISTER_xmm12 = 12,
    ASM_REGISTER_xmm13 = 13,
    ASM_REGISTER_xmm14 = 14,
    ASM_REGISTER_xmm15 = 15,
    
};

enum asm_arg_kind{
    ASM_ARG_invalid,
    
    ASM_ARG_register,                // e.g. rax, xmm0, zmm0
    ASM_ARG_memory_operand,          // e.g. gs:[rax], dword ptr[1337]
    ASM_ARG_declaration_reference,   // e.g. s.member, array[0]
    ASM_ARG_declaration_dereference, // e.g. [s.member], *s.member, s.member[0]
    ASM_ARG_immediate,               // e.g. 1337, sizeof(int)
    
    ASM_ARG_count,
};

struct asm_operand{
    enum asm_arg_kind kind;
    enum register_kind register_kind_when_loaded;
    smm size;
    
    union{
        struct{ // ASM_ARG_register
            enum asm_register reg;
        };
        struct{ // ASM_ARG_memory_operand
            enum asm_register base;
            enum asm_register index;
            smm scale;
            smm offset;
        };
        struct{ // ASM_ARG_declaration_reference
            struct ast_declaration *declaration;
            u64 declaration_offset;
            b32 is_inline_asm_function_argument;
            b32 is_inline_asm_function_argument_that_needs_to_be_an_integer;
            b32 is_inline_asm_function_argument_that_can_be_integer;
        };
        struct{ // ASM_ARG_immediate
            u64 immediate;
        };
        struct{
            u8 *data; // for MEMONIC_bytes
        };
    };
};

enum memonic{
    MEMONIC_none,
    
    MEMONIC_lock_prefix,
    MEMONIC_repe_prefix,
    
    MEMONIC_mov,
    MEMONIC_movzx, MEMONIC_movsx, MEMONIC_movsxd,
    MEMONIC_xchg,
    MEMONIC_bswap,
    
    MEMONIC_lea,
    
    // @WARNING: ordered in the same way REG_OPCODE_* are!
    MEMONIC_add, MEMONIC_or,
    MEMONIC_adc, MEMONIC_sbb,
    MEMONIC_and, MEMONIC_sub,
    MEMONIC_xor, MEMONIC_cmp,
    
    // @WARNING: ordered in the same way as REG_OPCODE_* are!
    MEMONIC_rol, MEMONIC_ror,
    MEMONIC_rcl, MEMONIC_rcr,
    MEMONIC_shl, MEMONIC_shr,
    MEMONIC_sal, MEMONIC_sar,
    
    MEMONIC_xgetbv,
    MEMONIC_cpuid,
    MEMONIC_clflush,
    
    MEMONIC_rdtsc, MEMONIC_rdtscp,
    
    MEMONIC_int3,
    MEMONIC_ret,
    
    MEMONIC_inc, MEMONIC_dec,
    MEMONIC_neg,
    
    MEMONIC_mul,
    MEMONIC_imul,
    MEMONIC_div,
    
    MEMONIC_bt, MEMONIC_bts, MEMONIC_btr, MEMONIC_btc,
    
    MEMONIC_bsf, MEMONIC_bsr,
    
    MEMONIC_popcnt,
    MEMONIC_lzcnt,
    
    // @WARNING: these must be in the order of opcode as we use the formula
    //               'opcode = 0x90 + memonic - MEMONIC_seto'
    //           to determine the opcode.
    MEMONIC_seto,  MEMONIC_setno,
    MEMONIC_setc,  MEMONIC_setnc,
    MEMONIC_setz,  MEMONIC_setnz,
    MEMONIC_setbe, MEMONIC_setnbe,
    MEMONIC_sets,  MEMONIC_setns,
    MEMONIC_setp,  MEMONIC_setnp,
    MEMONIC_setl,  MEMONIC_setnl,
    MEMONIC_setle, MEMONIC_setnle,
    
    MEMONIC_cmovo,  MEMONIC_cmovno,
    MEMONIC_cmovc,  MEMONIC_cmovnc,
    MEMONIC_cmovz,  MEMONIC_cmovnz,
    MEMONIC_cmovbe, MEMONIC_cmovnbe,
    MEMONIC_cmovs,  MEMONIC_cmovns,
    MEMONIC_cmovp,  MEMONIC_cmovnp,
    MEMONIC_cmovl,  MEMONIC_cmovnl,
    MEMONIC_cmovle, MEMONIC_cmovnle,
    
    MEMONIC_xadd,
    MEMONIC_cmpxchg,
    MEMONIC_cmpxchg8b,
    MEMONIC_cmpxchg16b,
    
    MEMONIC_movsb,
    MEMONIC_movsw,
    // MEMONIC_movsd,
    MEMONIC_movsq,
    MEMONIC_stosb,
    MEMONIC_stosw,
    MEMONIC_stosd,
    MEMONIC_stosq,
    
    MEMONIC_int,
    
    //
    // SSE instructions
    //
    
    MEMONIC_pause, MEMONIC_sfence, MEMONIC_lfence, MEMONIC_mfence,
    
    // Memory-to-register/register-to-memory/register-to-register data movement
    MEMONIC_movss,  MEMONIC_movsd,
    MEMONIC_movaps, MEMONIC_movapd,
    MEMONIC_movups, MEMONIC_movupd,
    MEMONIC_movdqa, MEMONIC_movdqu,
    
    MEMONIC_movhlps, MEMONIC_movlhps,
    
    MEMONIC_movhpd, MEMONIC_movhps, 
    MEMONIC_movlpd, MEMONIC_movlps, 
    
    // Arithmetic
    MEMONIC_addps,   MEMONIC_addss,   MEMONIC_addpd,   MEMONIC_addsd,
    MEMONIC_subps,   MEMONIC_subss,   MEMONIC_subpd,   MEMONIC_subsd,
    MEMONIC_mulps,   MEMONIC_mulss,   MEMONIC_mulpd,   MEMONIC_mulsd,
    MEMONIC_divps,   MEMONIC_divss,   MEMONIC_divpd,   MEMONIC_divsd,
    MEMONIC_rcpps,   MEMONIC_rcpss,
    MEMONIC_sqrtps,  MEMONIC_sqrtss,  MEMONIC_sqrtpd,  MEMONIC_sqrtsd,
    MEMONIC_maxps,   MEMONIC_maxss,   MEMONIC_maxpd,   MEMONIC_maxsd,
    MEMONIC_minps,   MEMONIC_minss,   MEMONIC_minpd,   MEMONIC_minsd,
    MEMONIC_rsqrtps, MEMONIC_rsqrtss,
    MEMONIC_haddps,  MEMONIC_haddpd,
    
    MEMONIC_xorps,   MEMONIC_xorpd,
    MEMONIC_orps,    MEMONIC_orpd,
    MEMONIC_andps,   MEMONIC_andpd,
    MEMONIC_andnps,  MEMONIC_andnpd,
    
    MEMONIC_cmpps, MEMONIC_cmpss, MEMONIC_cmppd, MEMONIC_cmpsd,
    
    // @note: these have to be in order for the emit.
    // cmpps pseudo-opcodes
    MEMONIC_cmpeqps,    // cmpps xmm1, xmm2, 0
    MEMONIC_cmpltps,    // cmpps xmm1, xmm2, 1
    MEMONIC_cmpleps,    // cmpps xmm1, xmm2, 2
    MEMONIC_cmpunordps, // cmpps xmm1, xmm2, 3
    MEMONIC_cmpneqps,   // cmpps xmm1, xmm2, 4
    MEMONIC_cmpnltps,   // cmpps xmm1, xmm2, 5
    MEMONIC_cmpnleps,   // cmpps xmm1, xmm2, 6
    MEMONIC_cmpordps,   // cmpps xmm1, xmm2, 7
    
    // cmpss pseudo-opcodes
    MEMONIC_cmpeqss,    // cmpss xmm1, xmm2, 0
    MEMONIC_cmpltss,    // cmpss xmm1, xmm2, 1
    MEMONIC_cmpless,    // cmpss xmm1, xmm2, 2
    MEMONIC_cmpunordss, // cmpss xmm1, xmm2, 3
    MEMONIC_cmpneqss,   // cmpss xmm1, xmm2, 4
    MEMONIC_cmpnltss,   // cmpss xmm1, xmm2, 5
    MEMONIC_cmpnless,   // cmpss xmm1, xmm2, 6
    MEMONIC_cmpordss,   // cmpss xmm1, xmm2, 7
    
    // cmppd pseudo-opcodes
    MEMONIC_cmpeqpd,    // cmppd xmm1, xmm2, 0
    MEMONIC_cmpltpd,    // cmppd xmm1, xmm2, 1
    MEMONIC_cmplepd,    // cmppd xmm1, xmm2, 2
    MEMONIC_cmpunordpd, // cmppd xmm1, xmm2, 3
    MEMONIC_cmpneqpd,   // cmppd xmm1, xmm2, 4
    MEMONIC_cmpnltpd,   // cmppd xmm1, xmm2, 5
    MEMONIC_cmpnlepd,   // cmppd xmm1, xmm2, 6
    MEMONIC_cmpordpd,   // cmppd xmm1, xmm2, 7
    
    // cmpsd pseudo-opcodes
    MEMONIC_cmpeqsd,    // cmpsd xmm1, xmm2, 0
    MEMONIC_cmpltsd,    // cmpsd xmm1, xmm2, 1
    MEMONIC_cmplesd,    // cmpsd xmm1, xmm2, 2
    MEMONIC_cmpunordsd, // cmpsd xmm1, xmm2, 3
    MEMONIC_cmpneqsd,   // cmpsd xmm1, xmm2, 4
    MEMONIC_cmpnltsd,   // cmpsd xmm1, xmm2, 5
    MEMONIC_cmpnlesd,   // cmpsd xmm1, xmm2, 6
    MEMONIC_cmpordsd,   // cmpsd xmm1, xmm2, 7
    
    // Data shuffle and unpacking
    MEMONIC_shufps, MEMONIC_shufpd,
    
    MEMONIC_pshufb,
    // MEMONIC_pshufw,
    MEMONIC_pshufd,
    
    MEMONIC_pshufhw,
    MEMONIC_pshuflw,
    
    MEMONIC_comiss, MEMONIC_ucomiss, MEMONIC_comisd, MEMONIC_ucomisd,
    
    MEMONIC_ldmxcsr,
    MEMONIC_stmxcsr,
    
    MEMONIC_pxor, MEMONIC_por,
    MEMONIC_pand, MEMONIC_pandn,
    
    MEMONIC_packsswb, MEMONIC_packssdw,
    MEMONIC_packuswb, MEMONIC_packusdw,

    MEMONIC_unpckhps, MEMONIC_unpckhpd,
    MEMONIC_unpcklps, MEMONIC_unpcklpd,
    
    MEMONIC_punpckhbw, MEMONIC_punpckhwd, MEMONIC_punpckhdq, MEMONIC_punpckhqdq,
    MEMONIC_punpcklbw, MEMONIC_punpcklwd, MEMONIC_punpckldq, MEMONIC_punpcklqdq,
    
    MEMONIC_pmaddwd,
    
    MEMONIC_pmullw,
    MEMONIC_pmulld,
    
    MEMONIC_paddb,  MEMONIC_paddw,  MEMONIC_paddd,  MEMONIC_paddq,
    MEMONIC_psubb,  MEMONIC_psubw,  MEMONIC_psubd,  MEMONIC_psubq,
    
    MEMONIC_paddsb,  MEMONIC_paddsw,
    MEMONIC_psubsb,  MEMONIC_psubsw,
    MEMONIC_paddusb, MEMONIC_paddusw,
    MEMONIC_psubusb, MEMONIC_psubusw,
    
    MEMONIC_psadbw,
    
    MEMONIC_pcmpgtb, MEMONIC_pcmpgtw, MEMONIC_pcmpgtd, 
    // MEMONIC_pcmpltb, MEMONIC_pcmpltw, MEMONIC_pcmpltd, 
    
    MEMONIC_pavgb, MEMONIC_pavgw,
    
    MEMONIC_pcmpeqb, MEMONIC_pcmpeqw, MEMONIC_pcmpeqd,
    MEMONIC_pminub,  MEMONIC_pminuw,
    MEMONIC_pmaxub,  MEMONIC_pmaxuw,
    MEMONIC_pminsb,  MEMONIC_pminsw,
    MEMONIC_pmaxsb,  MEMONIC_pmaxsw,
    
    MEMONIC_pmuludq,
    MEMONIC_pmulhw,
    MEMONIC_pmulhuw,
    
    MEMONIC_movmskps, MEMONIC_movmskpd,
    MEMONIC_pmovmskb, 
    MEMONIC_maskmovdqu,
    
    // @Warning: this order has to work with 'emit_inline_asm_block'
    // @note: psraq does not _really_ exist (maybe in avx512), but we need it for some tricks in 'emit_asm_block'
    MEMONIC_psrlw, MEMONIC_psraw, MEMONIC_psllw,  // psrlw/psraw/psllw xmm, xmm/m128 | psrlw/psraw/psllw xmm, imm8
    MEMONIC_psrld, MEMONIC_psrad, MEMONIC_pslld,  // psrld/psrad/pslld xmm, xmm/m128 | psrld/psrad/pslld xmm, imm8
    MEMONIC_psrlq, MEMONIC_psraq, MEMONIC_psllq,  // psrlq/-----/psllq xmm, xmm/m128 | psrlq/-----/psllq xmm, imm8
    MEMONIC_psrldq,               MEMONIC_pslldq, //                                 | psrldq/---/pslldq xmm, imm8
    
    MEMONIC_cvtsi2ss, MEMONIC_cvtsi2sd,
    MEMONIC_cvtss2si, MEMONIC_cvttss2si,
    MEMONIC_cvtsd2si, MEMONIC_cvttsd2si,
    MEMONIC_cvtps2dq, MEMONIC_cvttps2dq,
    MEMONIC_cvtpd2dq, MEMONIC_cvttpd2dq,
    MEMONIC_cvtdq2ps, MEMONIC_cvtdq2pd,
    
    MEMONIC_cvtpd2ps, MEMONIC_cvtps2pd,
    MEMONIC_cvtsd2ss, MEMONIC_cvtss2sd,
    
    MEMONIC_ptest,
    
    MEMONIC_pcmpestri, MEMONIC_pcmpistri,
    
    MEMONIC_movd, MEMONIC_movq,
    
    MEMONIC_movntps, MEMONIC_movntpd, MEMONIC_movntdq,
    MEMONIC_movnti,
    
    MEMONIC_prefetch,
    
    MEMONIC_pextrb, MEMONIC_pextrw, MEMONIC_pextrd, MEMONIC_pextrq,
    MEMONIC_pinsrb, MEMONIC_pinsrw, MEMONIC_pinsrd, MEMONIC_pinsrq,
    
    MEMONIC_palignr,
    
    MEMONIC_aesdec,
    
    MEMONIC_vmovdqu,
    MEMONIC_vmovdqa,
    
    MEMONIC_vmovd,
    MEMONIC_vmovq,
    
    MEMONIC_vmovups,
    MEMONIC_vmovss,
    
    MEMONIC_vptest,
    
    MEMONIC_vaddps,
    MEMONIC_vsubps,
    MEMONIC_vmulps,
    
    MEMONIC_vpminub,
    MEMONIC_vpxor,
    MEMONIC_vpcmpeqb,
    
    MEMONIC_vpshufb,
    MEMONIC_vshufps,
    MEMONIC_vblendps,
    MEMONIC_vperm2f128,
    MEMONIC_vinsertf128,
    
    MEMONIC_vpsrlw, MEMONIC_vpsraw, MEMONIC_vpsllw,  // vpsrlw/vpsraw/vpsllw xmm, xmm, xmm/m128 | vpsrlw/vpsraw/vpsllw xmm, xmm, imm8
    MEMONIC_vpsrld, MEMONIC_vpsrad, MEMONIC_vpslld,  // vpsrld/vpsrad/vpslld xmm, xmm, xmm/m128 | vpsrld/vpsrad/vpslld xmm, xmm, imm8
    MEMONIC_vpsrlq, MEMONIC_vpsraq, MEMONIC_vpsllq,  // vpsrlq/------/vpsllq xmm, xmm, xmm/m128 | vpsrlq/------/vpsllq xmm, xmm, imm8
    MEMONIC_vpsrldq,                MEMONIC_vpslldq, //                                         | vpsrldq/----/vpslldq xmm, xmm, imm8
    
    MEMONIC_vpmovmskb,
    
    MEMONIC_crc32,
    
    MEMONIC_return_from_inline_asm_function,
    MEMONIC_bytes,
    
    MEMONIC_count,
};

enum asm_operand_kind_flags{
    
    ASM_OP_KIND_reg8  = 0x1,
    ASM_OP_KIND_reg16 = 0x2,
    ASM_OP_KIND_reg32 = 0x4,
    ASM_OP_KIND_reg64 = 0x8,
    
    ASM_OP_KIND_non_reg8 = ASM_OP_KIND_reg16 | ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64,
    ASM_OP_KIND_any_reg  = ASM_OP_KIND_reg8  | ASM_OP_KIND_reg16 | ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64,
    
    ASM_OP_KIND_mem8   = 0x10,
    ASM_OP_KIND_mem16  = 0x20,
    ASM_OP_KIND_mem32  = 0x40, 
    ASM_OP_KIND_mem64  = 0x80,
    ASM_OP_KIND_mem128 = 0x100,
    ASM_OP_KIND_mem256 = 0x200,
    
    ASM_OP_KIND_any_mem = ASM_OP_KIND_mem8 | ASM_OP_KIND_mem16 | ASM_OP_KIND_mem32 | ASM_OP_KIND_mem64 | ASM_OP_KIND_mem128 | ASM_OP_KIND_mem256,
    
    ASM_OP_KIND_regm8  = ASM_OP_KIND_mem8  | ASM_OP_KIND_reg8,
    ASM_OP_KIND_regm16 = ASM_OP_KIND_mem16 | ASM_OP_KIND_reg16,
    ASM_OP_KIND_regm32 = ASM_OP_KIND_mem32 | ASM_OP_KIND_reg32,
    ASM_OP_KIND_regm64 = ASM_OP_KIND_mem64 | ASM_OP_KIND_reg64,
    
    ASM_OP_KIND_non_regm8 = ASM_OP_KIND_regm16 | ASM_OP_KIND_regm32 | ASM_OP_KIND_regm64,
    ASM_OP_KIND_any_regm  = ASM_OP_KIND_regm8  | ASM_OP_KIND_regm16 | ASM_OP_KIND_regm32 | ASM_OP_KIND_regm64,
    
    ASM_OP_KIND_imm8  = 0x400,
    ASM_OP_KIND_imm16 = 0x800,
    ASM_OP_KIND_imm32 = 0x1000,
    ASM_OP_KIND_imm64 = 0x2000,
    
    ASM_OP_KIND_any_imm = ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 | ASM_OP_KIND_imm64,
    
    ASM_OP_KIND_xmm = 0x4000,
    ASM_OP_KIND_xmmm32  = ASM_OP_KIND_mem32  | ASM_OP_KIND_xmm,
    ASM_OP_KIND_xmmm64  = ASM_OP_KIND_mem64  | ASM_OP_KIND_xmm,
    ASM_OP_KIND_xmmm128 = ASM_OP_KIND_mem128 | ASM_OP_KIND_xmm,
    
    ASM_OP_KIND_ymm = 0x8000,
    
    ASM_OP_KIND_ymmm256 = ASM_OP_KIND_mem256 | ASM_OP_KIND_ymm,
    
    // ASM_OP_KIND_zmm  = 0x10000,
    // ASM_OP_KIND_zmmm = 0x20000,
    
};

static struct{
    struct string memonic;
    u32 amount_of_operands;
    enum asm_operand_kind_flags operand_kind_flags[8];
} asm_parse_table[MEMONIC_count] = {
    // Zero operand memonics @cleanup: allow expicitly specifying implict registers?
    
    [MEMONIC_return_from_inline_asm_function] = {.memonic = const_string("return"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_any_reg | ASM_OP_KIND_xmm | ASM_OP_KIND_ymm },
    [MEMONIC_bytes] = {.memonic = const_string("bytes"), .amount_of_operands = 0}, // We handle this manually.
    
    [MEMONIC_lock_prefix] = {.memonic = const_string("lock") },
    [MEMONIC_repe_prefix] = {.memonic = const_string("repe") },
    
    [MEMONIC_xgetbv] = {.memonic = const_string("xgetbv"), .amount_of_operands = 0 },
    [MEMONIC_cpuid]  = {.memonic = const_string("cpuid"),  .amount_of_operands = 0 },
    [MEMONIC_rdtsc]  = {.memonic = const_string("rdtsc"),  .amount_of_operands = 0 },
    [MEMONIC_rdtscp] = {.memonic = const_string("rdtscp"), .amount_of_operands = 0 },
    [MEMONIC_int3]   = {.memonic = const_string("int3"),   .amount_of_operands = 0 },
    [MEMONIC_ret]    = {.memonic = const_string("ret"),    .amount_of_operands = 0 },
    
    [MEMONIC_movsb]  = {.memonic = const_string("movsb"), .amount_of_operands = 0 },
    [MEMONIC_movsw]  = {.memonic = const_string("movsw"), .amount_of_operands = 0 },
    [MEMONIC_movsq]  = {.memonic = const_string("movsq"), .amount_of_operands = 0 },
    [MEMONIC_stosb]  = {.memonic = const_string("stosb"), .amount_of_operands = 0 },
    [MEMONIC_stosw]  = {.memonic = const_string("stosw"), .amount_of_operands = 0 },
    [MEMONIC_stosd]  = {.memonic = const_string("stosd"), .amount_of_operands = 0 },
    [MEMONIC_stosq]  = {.memonic = const_string("stosq"), .amount_of_operands = 0 },
    
    [MEMONIC_pause]  = {.memonic = const_string("pause"), .amount_of_operands = 0},
    
    [MEMONIC_clflush] = {.memonic = const_string("clflush"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_any_regm & ~ASM_OP_KIND_any_reg},
    [MEMONIC_int]  = {.memonic = const_string("int"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_inc] = {.memonic = const_string("inc"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_any_regm },
    [MEMONIC_dec] = {.memonic = const_string("dec"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_any_regm },
    [MEMONIC_neg] = {.memonic = const_string("neg"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_any_regm },
    
    [MEMONIC_div] = {.memonic = const_string("div"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_any_regm },
    [MEMONIC_imul] = {.memonic = const_string("imul"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_any_regm },
    [MEMONIC_mul] = {.memonic = const_string("mul"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_any_regm },
    
    [MEMONIC_bswap] = { .memonic = const_string("bswap"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64 },
    
    [MEMONIC_seto]   = {.memonic = const_string("seto"),   .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setno]  = {.memonic = const_string("setno"),  .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setc]   = {.memonic = const_string("setc"),   .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setnc]  = {.memonic = const_string("setnc"),  .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setz]   = {.memonic = const_string("setz"),   .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setnz]  = {.memonic = const_string("setnz"),  .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setbe]  = {.memonic = const_string("setbe"),  .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setnbe] = {.memonic = const_string("setnbe"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_sets]   = {.memonic = const_string("sets"),   .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setns]  = {.memonic = const_string("setns"),  .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setp]   = {.memonic = const_string("setp"),   .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setnp]  = {.memonic = const_string("setnp"),  .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setl]   = {.memonic = const_string("setl"),   .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setnl]  = {.memonic = const_string("setnl"),  .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setle]  = {.memonic = const_string("setle"),  .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    [MEMONIC_setnle] = {.memonic = const_string("setnle"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm8 },
    
    [MEMONIC_cmovo]   = {.memonic = const_string("cmovo"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovno]  = {.memonic = const_string("cmovno"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovc]   = {.memonic = const_string("cmovc"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovnc]  = {.memonic = const_string("cmovnc"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovz]   = {.memonic = const_string("cmovz"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovnz]  = {.memonic = const_string("cmovnz"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovbe]  = {.memonic = const_string("cmovbe"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovnbe] = {.memonic = const_string("cmovnbe"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovs]   = {.memonic = const_string("cmovs"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovns]  = {.memonic = const_string("cmovns"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovp]   = {.memonic = const_string("cmovp"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovnp]  = {.memonic = const_string("cmovnp"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovl]   = {.memonic = const_string("cmovl"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovnl]  = {.memonic = const_string("cmovnl"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovle]  = {.memonic = const_string("cmovle"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_cmovnle] = {.memonic = const_string("cmovnle"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    
    [MEMONIC_mov] = {.memonic = const_string("mov"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 | ASM_OP_KIND_imm64 },
    
    [MEMONIC_lea] = {.memonic = const_string("lea"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_any_regm & ~ASM_OP_KIND_any_reg },
    
    [MEMONIC_movzx]  = {.memonic = const_string("movzx"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_regm8  | ASM_OP_KIND_regm16 },
    [MEMONIC_movsx]  = {.memonic = const_string("movsx"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_regm8  | ASM_OP_KIND_regm16 },
    [MEMONIC_movsxd] = {.memonic = const_string("movsxd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64, .operand_kind_flags[1] = ASM_OP_KIND_regm32 },
    
    [MEMONIC_xchg] = { .memonic = const_string("xchg"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm },
    
    [MEMONIC_add] = {.memonic = const_string("add"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 },
    [MEMONIC_or]  = {.memonic = const_string("or"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 },
    [MEMONIC_adc] = {.memonic = const_string("adc"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 },
    [MEMONIC_sbb] = {.memonic = const_string("sbb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 },
    [MEMONIC_and] = {.memonic = const_string("and"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 },
    [MEMONIC_sub] = {.memonic = const_string("sub"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 },
    [MEMONIC_xor] = {.memonic = const_string("xor"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 },
    [MEMONIC_cmp] = {.memonic = const_string("cmp"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_regm | ASM_OP_KIND_imm8 | ASM_OP_KIND_imm16 | ASM_OP_KIND_imm32 },
    
    // @note: we use ASM_OP_KIND_any_gpr for the right side as opposed to reg8 or some cl thing.
    //        we then check for cl manually. Maybe we should have a cl thing tho.
    [MEMONIC_rol] = {.memonic = const_string("rol"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg | ASM_OP_KIND_imm8},
    [MEMONIC_ror] = {.memonic = const_string("ror"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg | ASM_OP_KIND_imm8},
    [MEMONIC_rcl] = {.memonic = const_string("rcl"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg | ASM_OP_KIND_imm8},
    [MEMONIC_rcr] = {.memonic = const_string("rcr"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg | ASM_OP_KIND_imm8},
    [MEMONIC_shl] = {.memonic = const_string("shl"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg | ASM_OP_KIND_imm8},
    [MEMONIC_shr] = {.memonic = const_string("shr"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg | ASM_OP_KIND_imm8},
    [MEMONIC_sal] = {.memonic = const_string("sal"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg | ASM_OP_KIND_imm8},
    [MEMONIC_sar] = {.memonic = const_string("sar"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg | ASM_OP_KIND_imm8},
    
    // @cleanup: allow any reg for rhs? the rhs always gets modulated
    [MEMONIC_bt]  = {.memonic = const_string("bt"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_regm8, .operand_kind_flags[1] = ASM_OP_KIND_non_reg8 | ASM_OP_KIND_imm8},
    [MEMONIC_btr] = {.memonic = const_string("btr"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_regm8, .operand_kind_flags[1] = ASM_OP_KIND_non_reg8 | ASM_OP_KIND_imm8},
    [MEMONIC_bts] = {.memonic = const_string("bts"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_regm8, .operand_kind_flags[1] = ASM_OP_KIND_non_reg8 | ASM_OP_KIND_imm8},
    [MEMONIC_btc] = {.memonic = const_string("btc"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_regm8, .operand_kind_flags[1] = ASM_OP_KIND_non_reg8 | ASM_OP_KIND_imm8},
    
    [MEMONIC_bsf]    = {.memonic = const_string("bsf"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_bsr]    = {.memonic = const_string("bsr"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_popcnt] = {.memonic = const_string("popcnt"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    [MEMONIC_lzcnt]  = {.memonic = const_string("lzcnt"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_non_reg8, .operand_kind_flags[1] = ASM_OP_KIND_non_regm8 },
    
    [MEMONIC_xadd]    = {.memonic = const_string("xadd"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg},
    [MEMONIC_cmpxchg] = {.memonic = const_string("cmpxchg"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_any_regm, .operand_kind_flags[1] = ASM_OP_KIND_any_reg},
    
    // @note: allow any size as pointer. The 8b/16b already makes you think about the size.
    [MEMONIC_cmpxchg8b]  = {.memonic = const_string("cmpxchg8b"),  .amount_of_operands = 1, .operand_kind_flags[0] = (ASM_OP_KIND_any_regm & ~ASM_OP_KIND_any_reg) },
    [MEMONIC_cmpxchg16b] = {.memonic = const_string("cmpxchg16b"), .amount_of_operands = 1, .operand_kind_flags[0] = (ASM_OP_KIND_any_regm & ~ASM_OP_KIND_any_reg) },
    
    //
    // SSE instructions
    //
    
    [MEMONIC_cmpps] = {.memonic = const_string("cmpps"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_cmppd] = {.memonic = const_string("cmppd"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_cmpsd] = {.memonic = const_string("cmpsd"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_cmpss] = {.memonic = const_string("cmpss"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    // cmpps pseudo-opcodes
    [MEMONIC_cmpeqps]    = {.memonic = const_string("cmpeqps"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpltps]    = {.memonic = const_string("cmpltps"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpleps]    = {.memonic = const_string("cmpleps"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpunordps] = {.memonic = const_string("cmpunordps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpneqps]   = {.memonic = const_string("cmpneqps"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpnltps]   = {.memonic = const_string("cmpnltps"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpnleps]   = {.memonic = const_string("cmpnleps"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpordps]   = {.memonic = const_string("cmpordps"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    // cmpss pseudo-opcodes
    [MEMONIC_cmpeqss]    = {.memonic = const_string("cmpeqss"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_cmpltss]    = {.memonic = const_string("cmpltss"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_cmpless]    = {.memonic = const_string("cmpless"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_cmpunordss] = {.memonic = const_string("cmpunordss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_cmpneqss]   = {.memonic = const_string("cmpneqss"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_cmpnltss]   = {.memonic = const_string("cmpnltss"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_cmpnless]   = {.memonic = const_string("cmpnless"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_cmpordss]   = {.memonic = const_string("cmpordss"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    
    // cmppd pseudo-opcodes
    [MEMONIC_cmpeqpd]    = {.memonic = const_string("cmpeqpd"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpltpd]    = {.memonic = const_string("cmpltpd"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmplepd]    = {.memonic = const_string("cmplepd"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpunordpd] = {.memonic = const_string("cmpunordpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpneqpd]   = {.memonic = const_string("cmpneqpd"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpnltpd]   = {.memonic = const_string("cmpnltpd"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpnlepd]   = {.memonic = const_string("cmpnlepd"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_cmpordpd]   = {.memonic = const_string("cmpordpd"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    // cmpsd pseudo-opcodes
    [MEMONIC_cmpeqsd]    = {.memonic = const_string("cmpeqsd"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    [MEMONIC_cmpltsd]    = {.memonic = const_string("cmpltsd"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    [MEMONIC_cmplesd]    = {.memonic = const_string("cmplesd"),    .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    [MEMONIC_cmpunordsd] = {.memonic = const_string("cmpunordsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    [MEMONIC_cmpneqsd]   = {.memonic = const_string("cmpneqsd"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    [MEMONIC_cmpnltsd]   = {.memonic = const_string("cmpnltsd"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    [MEMONIC_cmpnlesd]   = {.memonic = const_string("cmpnlesd"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    [MEMONIC_cmpordsd]   = {.memonic = const_string("cmpordsd"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_orps] = {.memonic = const_string("orps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_orpd] = {.memonic = const_string("orpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_xorps] = {.memonic = const_string("xorps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_xorpd] = {.memonic = const_string("xorpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_andps] = {.memonic = const_string("andps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_andpd] = {.memonic = const_string("andpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_andnps] = {.memonic = const_string("andnps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_andnpd] = {.memonic = const_string("andnpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_addps] = {.memonic = const_string("addps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_addss] = {.memonic = const_string("addss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_addpd] = {.memonic = const_string("addpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_addsd] = {.memonic = const_string("addsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_subps] = {.memonic = const_string("subps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_subss] = {.memonic = const_string("subss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_subpd] = {.memonic = const_string("subpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_subsd] = {.memonic = const_string("subsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_mulps] = {.memonic = const_string("mulps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_mulss] = {.memonic = const_string("mulss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_mulpd] = {.memonic = const_string("mulpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_mulsd] = {.memonic = const_string("mulsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_divps] = {.memonic = const_string("divps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_divss] = {.memonic = const_string("divss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_divpd] = {.memonic = const_string("divpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_divsd] = {.memonic = const_string("divsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_rcpps] = {.memonic = const_string("rcpps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_rcpss] = {.memonic = const_string("rcpss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    // [MEMONIC_rcppd] = {.memonic = const_string("rcppd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    // [MEMONIC_rcpsd] = {.memonic = const_string("rcpsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_sqrtps] = {.memonic = const_string("sqrtps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_sqrtss] = {.memonic = const_string("sqrtss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_sqrtpd] = {.memonic = const_string("sqrtpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_sqrtsd] = {.memonic = const_string("sqrtsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_minps] = {.memonic = const_string("minps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_minss] = {.memonic = const_string("minss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_minpd] = {.memonic = const_string("minpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_minsd] = {.memonic = const_string("minsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_maxps] = {.memonic = const_string("maxps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_maxss] = {.memonic = const_string("maxss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_maxpd] = {.memonic = const_string("maxpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_maxsd] = {.memonic = const_string("maxsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_rsqrtps] = {.memonic = const_string("rsqrtps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_rsqrtss] = {.memonic = const_string("rsqrtss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    // [MEMONIC_rsqrtpd] = {.memonic = const_string("rsqrtpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    // [MEMONIC_rsqrtsd] = {.memonic = const_string("rsqrtsd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64},
    
    [MEMONIC_haddps] = {.memonic = const_string("haddps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_haddpd] = {.memonic = const_string("haddpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_movss]  = {.memonic = const_string("movss"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm32,  .operand_kind_flags[1] = ASM_OP_KIND_xmmm32  },
    [MEMONIC_movsd]  = {.memonic = const_string("movsd"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm64,  .operand_kind_flags[1] = ASM_OP_KIND_xmmm64  },
    [MEMONIC_movaps] = {.memonic = const_string("movaps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm128, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_movups] = {.memonic = const_string("movups"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm128, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_movapd] = {.memonic = const_string("movapd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm128, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_movupd] = {.memonic = const_string("movupd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm128, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_movhpd] = {.memonic = const_string("movhpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm64, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64 }, 
    [MEMONIC_movlpd] = {.memonic = const_string("movlpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm64, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64 },
    [MEMONIC_movhps] = {.memonic = const_string("movhps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm64, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64 },
    [MEMONIC_movlps] = {.memonic = const_string("movlps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm64, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64 },
    
    [MEMONIC_movntps] = {.memonic = const_string("movntps"), .amount_of_operands = 2, .operand_kind_flags[0] = (ASM_OP_KIND_xmmm128 & ~ASM_OP_KIND_xmm), .operand_kind_flags[1] = ASM_OP_KIND_xmm },
    [MEMONIC_movntpd] = {.memonic = const_string("movntpd"), .amount_of_operands = 2, .operand_kind_flags[0] = (ASM_OP_KIND_xmmm128 & ~ASM_OP_KIND_xmm), .operand_kind_flags[1] = ASM_OP_KIND_xmm },
    [MEMONIC_movntdq] = {.memonic = const_string("movntdq"), .amount_of_operands = 2, .operand_kind_flags[0] = (ASM_OP_KIND_xmmm128 & ~ASM_OP_KIND_xmm), .operand_kind_flags[1] = ASM_OP_KIND_xmm },
    
    [MEMONIC_movnti] = {.memonic = const_string("movnti"), .amount_of_operands = 2, .operand_kind_flags[0] = (ASM_OP_KIND_regm32 | ASM_OP_KIND_regm64) & ~ASM_OP_KIND_any_reg,  .operand_kind_flags[1] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64 },
    
    [MEMONIC_movdqa] = {.memonic = const_string("movdqa"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm128, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_movdqu] = {.memonic = const_string("movdqu"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm128, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_movmskps]  = {.memonic = const_string("movmskps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64, .operand_kind_flags[1] = ASM_OP_KIND_xmm},
    [MEMONIC_movmskpd]  = {.memonic = const_string("movmskpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64, .operand_kind_flags[1] = ASM_OP_KIND_xmm},
    
    [MEMONIC_shufps] = {.memonic = const_string("shufps"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_shufpd] = {.memonic = const_string("shufpd"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    // [MEMONIC_pshufb] = {.memonic = const_string("pshufb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    // [MEMONIC_pshufw] = {.memonic = const_string("pshufw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_pshufd] = {.memonic = const_string("pshufd"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_ldmxcsr] = {.memonic = const_string("ldmxcsr"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm32 & ~ASM_OP_KIND_reg32, },
    [MEMONIC_stmxcsr] = {.memonic = const_string("stmxcsr"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_regm32 & ~ASM_OP_KIND_reg32, },
    
    [MEMONIC_pshufhw] = {.memonic = const_string("pshufhw"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_pshuflw] = {.memonic = const_string("pshuflw"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_comiss]  = {.memonic = const_string("comiss"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_ucomiss] = {.memonic = const_string("ucomiss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_comisd]  = {.memonic = const_string("comisd"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    [MEMONIC_ucomisd] = {.memonic = const_string("ucomisd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32},
    
    [MEMONIC_pxor]  = {.memonic = const_string("pxor"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_por]   = {.memonic = const_string("por"),   .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_pand]  = {.memonic = const_string("pand"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_pandn] = {.memonic = const_string("pandn"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_packsswb] = {.memonic = const_string("packsswb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_packuswb] = {.memonic = const_string("packuswb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_packssdw] = {.memonic = const_string("packssdw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_packusdw] = {.memonic = const_string("packusdw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_unpcklps] = {.memonic = const_string("unpcklps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_unpcklpd] = {.memonic = const_string("unpcklpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_unpckhps] = {.memonic = const_string("unpckhps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_unpckhpd] = {.memonic = const_string("unpckhpd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_punpcklbw]  = {.memonic = const_string("punpcklbw"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_punpcklwd]  = {.memonic = const_string("punpcklwd"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_punpckldq]  = {.memonic = const_string("punpckldq"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_punpcklqdq] = {.memonic = const_string("punpcklqdq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_punpckhbw]  = {.memonic = const_string("punpckhbw"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_punpckhwd]  = {.memonic = const_string("punpckhwd"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_punpckhdq]  = {.memonic = const_string("punpckhdq"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_punpckhqdq] = {.memonic = const_string("punpckhqdq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_pmaddwd] = {.memonic = const_string("pmaddwd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_paddb] = {.memonic = const_string("paddb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_paddw] = {.memonic = const_string("paddw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_paddd] = {.memonic = const_string("paddd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_paddq] = {.memonic = const_string("paddq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_paddsb] = {.memonic = const_string("paddsb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_paddsw] = {.memonic = const_string("paddsw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_paddusb] = {.memonic = const_string("paddusb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_paddusw] = {.memonic = const_string("paddusw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_psubsb] = {.memonic = const_string("psubsb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_psubsw] = {.memonic = const_string("psubsw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_psubusb] = {.memonic = const_string("psubusb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_psubusw] = {.memonic = const_string("psubusw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_pavgb] = {.memonic = const_string("pavgb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_pavgw] = {.memonic = const_string("pavgw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_psadbw] = {.memonic = const_string("psadbw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_psubb] = {.memonic = const_string("psubb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_psubw] = {.memonic = const_string("psubw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_psubd] = {.memonic = const_string("psubd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_psubq] = {.memonic = const_string("psubq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_pmullw] = {.memonic = const_string("pmullw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_pmulld] = {.memonic = const_string("pmulld"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_pmuludq] = {.memonic = const_string("pmuludq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_pmulhw]  = {.memonic = const_string("pmulhw"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_pmulhuw] = {.memonic = const_string("pmulhuw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_psllw] = {.memonic = const_string("psllw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_pslld] = {.memonic = const_string("pslld"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_psllq] = {.memonic = const_string("psllq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    
    [MEMONIC_psrlw] = {.memonic = const_string("psrlw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_psrld] = {.memonic = const_string("psrld"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_psrlq] = {.memonic = const_string("psrlq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    
    [MEMONIC_psraw] = {.memonic = const_string("psraw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_psrad] = {.memonic = const_string("psrad"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_psraq] = {.memonic = const_string("psraq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    
    [MEMONIC_pslldq] = {.memonic = const_string("pslldq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_imm8 },
    [MEMONIC_psrldq] = {.memonic = const_string("psrldq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_pcmpgtb] = {.memonic = const_string("pcmpgtb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_pcmpgtw] = {.memonic = const_string("pcmpgtw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 }, 
    [MEMONIC_pcmpgtd] = {.memonic = const_string("pcmpgtd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 }, 
    // [MEMONIC_pcmpltb] = {.memonic = const_string("pcmpltb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 }, 
    // [MEMONIC_pcmpltw] = {.memonic = const_string("pcmpltw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 }, 
    // [MEMONIC_pcmpltd] = {.memonic = const_string("pcmpltd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 }, 
    
    [MEMONIC_cvtdq2ps] = {.memonic = const_string("cvtdq2ps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_cvtdq2pd] = {.memonic = const_string("cvtdq2pd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_cvtps2dq] = {.memonic = const_string("cvtps2dq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_cvtpd2dq] = {.memonic = const_string("cvtpd2dq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_cvttps2dq] = {.memonic = const_string("cvttps2dq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_cvttpd2dq] = {.memonic = const_string("cvttpd2dq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_cvtpd2ps] = {.memonic = const_string("cvtpd2ps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    [MEMONIC_cvtps2pd] = {.memonic = const_string("cvtps2pd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_cvtsi2ss] = {.memonic = const_string("cvtsi2ss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_regm64 | ASM_OP_KIND_regm32},
    [MEMONIC_cvtsi2sd] = {.memonic = const_string("cvtsi2sd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_regm64 | ASM_OP_KIND_regm32},
    
    [MEMONIC_cvtss2si]  = {.memonic = const_string("cvtss2si"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg64 | ASM_OP_KIND_reg32, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32 },
    [MEMONIC_cvttss2si] = {.memonic = const_string("cvttss2si"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg64 | ASM_OP_KIND_reg32, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32 },
    [MEMONIC_cvtsd2si]  = {.memonic = const_string("cvtsd2si"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg64 | ASM_OP_KIND_reg32, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64 },
    [MEMONIC_cvttsd2si] = {.memonic = const_string("cvttsd2si"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg64 | ASM_OP_KIND_reg32, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64 },
    
    [MEMONIC_cvtsd2ss] = {.memonic = const_string("cvtsd2ss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm64 },
    [MEMONIC_cvtss2sd] = {.memonic = const_string("cvtss2sd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32 },
    
    [MEMONIC_prefetch] = {.memonic = const_string("prefetch"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_imm8, .operand_kind_flags[1] = ASM_OP_KIND_any_regm & ~ASM_OP_KIND_any_reg },
    
    [MEMONIC_movd] = {.memonic = const_string("movd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_regm32, .operand_kind_flags[1] = ASM_OP_KIND_regm32 | ASM_OP_KIND_xmm },
    [MEMONIC_movq] = {.memonic = const_string("movq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_regm64, .operand_kind_flags[1] = ASM_OP_KIND_regm64 | ASM_OP_KIND_xmm },
    
    [MEMONIC_movhlps] = {.memonic = const_string("movhlps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm},
    [MEMONIC_movlhps] = {.memonic = const_string("movlhps"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm},
    
    [MEMONIC_sfence] = {.memonic = const_string("sfence"), .amount_of_operands = 0},
    [MEMONIC_lfence] = {.memonic = const_string("lfence"), .amount_of_operands = 0},
    [MEMONIC_mfence] = {.memonic = const_string("mfence"), .amount_of_operands = 0},
    
    //
    // SSE2 @incomplete: move stuff from above down here!
    //
    [MEMONIC_pminub]  = {.memonic = const_string("pminub"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_pminuw]  = {.memonic = const_string("pminuw"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_pmaxub]  = {.memonic = const_string("pmaxub"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_pmaxuw]  = {.memonic = const_string("pmaxuw"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_pminsb]  = {.memonic = const_string("pminsb"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_pminsw]  = {.memonic = const_string("pminsw"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_pmaxsb]  = {.memonic = const_string("pmaxsb"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_pmaxsw]  = {.memonic = const_string("pmaxsw"),  .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_pcmpeqb] = {.memonic = const_string("pcmpeqb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_pcmpeqw] = {.memonic = const_string("pcmpeqw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    [MEMONIC_pcmpeqd] = {.memonic = const_string("pcmpeqd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
    [MEMONIC_pextrw] = {.memonic = const_string("pextrw"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_regm16 | ASM_OP_KIND_reg32,  .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_pinsrw] = {.memonic = const_string("pinsrw"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_regm16 | ASM_OP_KIND_reg32, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_pmovmskb] = {.memonic = const_string("pmovmskb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64, .operand_kind_flags[1] = ASM_OP_KIND_xmm}, // @cleanup: what is this instruction it is maybe supposed to be reg, xmm ? but maybe also xmm? 
    [MEMONIC_maskmovdqu] = {.memonic = const_string("maskmovdqu"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm},
    
    //
    // SSE3
    //
    [MEMONIC_palignr] = {.memonic = const_string("palignr"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_pshufb] = {.memonic = const_string("pshufb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    //
    // SSE4.1
    //
    [MEMONIC_ptest] = {.memonic = const_string("ptest"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    [MEMONIC_pextrb] = {.memonic = const_string("pextrb"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_regm8 | ASM_OP_KIND_reg32,  .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_pextrd] = {.memonic = const_string("pextrd"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_regm32, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_pextrq] = {.memonic = const_string("pextrq"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_regm64, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_pinsrb] = {.memonic = const_string("pinsrb"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_regm8 | ASM_OP_KIND_reg32,  .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_pinsrd] = {.memonic = const_string("pinsrd"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_regm32, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_pinsrq] = {.memonic = const_string("pinsrq"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_regm64, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_crc32] = {.memonic = const_string("crc32"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64, .operand_kind_flags[1] = ASM_OP_KIND_any_regm },
    
    
    //
    // SSE4.2
    //
    
    [MEMONIC_pcmpistri] = {.memonic = const_string("pcmpistri"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_pcmpestri] = {.memonic = const_string("pcmpestri"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    //
    // AES
    //
    [MEMONIC_aesdec] = {.memonic = const_string("aesdec"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
    
    //
    // AVX
    //
    [MEMONIC_vmovups] = {.memonic = const_string("vmovups"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    
    [MEMONIC_vmovdqu] = {.memonic = const_string("vmovdqu"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_ymmm256 | ASM_OP_KIND_xmmm128, .operand_kind_flags[1] = ASM_OP_KIND_ymmm256 | ASM_OP_KIND_xmmm128 },
    [MEMONIC_vmovdqa] = {.memonic = const_string("vmovdqa"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_ymmm256 | ASM_OP_KIND_xmmm128, .operand_kind_flags[1] = ASM_OP_KIND_ymmm256 | ASM_OP_KIND_xmmm128 },
    
    
    [MEMONIC_vaddps]  = {.memonic = const_string("vaddps"),  .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    [MEMONIC_vsubps]  = {.memonic = const_string("vsubps"),  .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    [MEMONIC_vmulps]  = {.memonic = const_string("vmulps"),  .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    [MEMONIC_vpminub] = {.memonic = const_string("vpminub"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    [MEMONIC_vpxor]   = {.memonic = const_string("vpxor"),   .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    [MEMONIC_vpshufb] = {.memonic = const_string("vpshufb"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    [MEMONIC_vpcmpeqb]= {.memonic = const_string("vpcmpeqb"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    
    [MEMONIC_vblendps] = {.memonic = const_string("vblendps"),  .amount_of_operands = 4, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256, .operand_kind_flags[3] = ASM_OP_KIND_imm8 },
    [MEMONIC_vshufps]  = {.memonic = const_string("vshufps"),   .amount_of_operands = 4, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256, .operand_kind_flags[3] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_vperm2f128]  = {.memonic = const_string("vperm2f128"),  .amount_of_operands = 4, .operand_kind_flags[0] = ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_ymmm256, .operand_kind_flags[3] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_vinsertf128] = {.memonic = const_string("vinsertf128"), .amount_of_operands = 4, .operand_kind_flags[0] = ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128, .operand_kind_flags[3] = ASM_OP_KIND_imm8 },
    
    // @incomplete: this one is weird, it can take 2 or three operands!
    [MEMONIC_vmovss] = {.memonic = const_string("vmovss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32 },
    
    [MEMONIC_vptest] = {.memonic = const_string("vptest"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    
    [MEMONIC_vpmovmskb] = {.memonic = const_string("vpmovmskb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm },
    
    [MEMONIC_vmovd] = {.memonic = const_string("vmovd"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_regm32, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_regm32 },
    [MEMONIC_vmovq] = {.memonic = const_string("vmovq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_regm64, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_regm64 },
    
    
    
    [MEMONIC_vpsllw] = {.memonic = const_string("vpsllw"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_vpslld] = {.memonic = const_string("vpslld"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_vpsllq] = {.memonic = const_string("vpsllq"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    
    [MEMONIC_vpsrlw] = {.memonic = const_string("vpsrlw"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_vpsrld] = {.memonic = const_string("vpsrld"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_vpsrlq] = {.memonic = const_string("vpsrlq"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    
    [MEMONIC_vpsraw] = {.memonic = const_string("vpsraw"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_vpsrad] = {.memonic = const_string("vpsrad"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    [MEMONIC_vpsraq] = {.memonic = const_string("vpsraq"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_imm8 },
    
    [MEMONIC_vpslldq] = {.memonic = const_string("vpslldq"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    [MEMONIC_vpsrldq] = {.memonic = const_string("vpsrldq"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmm, .operand_kind_flags[2] = ASM_OP_KIND_imm8 },
    
    
};


struct asm_instruction{
    struct asm_instruction *next;
    struct token *token;
    enum memonic memonic;
    s32 amount_of_operands;
    s32 byte_offset_in_function;
    u32 prefixes;
    
    struct asm_operand operands[8];
};

func struct asm_instruction *push_asm_instruction(struct context *context, enum memonic memonic, u32 amount_of_operands, struct token *token){
    struct asm_instruction *asm_instruction = push_struct(context->arena, struct asm_instruction);
    asm_instruction->memonic = memonic;
    asm_instruction->amount_of_operands = amount_of_operands;
    asm_instruction->token = token;
    return asm_instruction;
}

func struct asm_operand asm_maybe_parse_gpr(struct context *context){
    struct asm_operand arg = zero_struct;
    
    if(!peek_token(context, TOKEN_identifier)) return arg;
    
    struct token *ident_token = next_token(context);
    arg.reg = ASM_REGISTER_invalid;
    arg.register_kind_when_loaded = REGISTER_KIND_gpr;
    
    struct string ident = token_get_string(ident_token);
    
    if(string_match(ident, string("rax"))){
        arg.reg  = ASM_REGISTER_rax;
        arg.size = 8;
    }else if(string_match(ident, string("rcx"))){
        arg.reg  = ASM_REGISTER_rcx;
        arg.size = 8;
    }else if(string_match(ident, string("rdx"))){
        arg.reg  = ASM_REGISTER_rdx;
        arg.size = 8;
    }else if(string_match(ident, string("rbx"))){
        arg.reg  = ASM_REGISTER_rbx;
        arg.size = 8;
    }else if(string_match(ident, string("rsi"))){
        arg.reg  = ASM_REGISTER_rsi;
        arg.size = 8;
    }else if(string_match(ident, string("rdi"))){
        arg.reg  = ASM_REGISTER_rdi;
        arg.size = 8;
    }else if(string_match(ident, string("rsp"))){
        arg.reg  = ASM_REGISTER_rsp;
        arg.size = 8;
    }else if(string_match(ident, string("rbp"))){
        arg.reg  = ASM_REGISTER_rbp;
        arg.size = 8;
    }else if(string_match(ident, string("r8"))){
        arg.reg  = ASM_REGISTER_r8;
        arg.size = 8;
    }else if(string_match(ident, string("r9"))){
        arg.reg  = ASM_REGISTER_r9;
        arg.size = 8;
    }else if(string_match(ident, string("r10"))){
        arg.reg  = ASM_REGISTER_r10;
        arg.size = 8;
    }else if(string_match(ident, string("r11"))){
        arg.reg  = ASM_REGISTER_r11;
        arg.size = 8;
    }else if(string_match(ident, string("r12"))){
        arg.reg  = ASM_REGISTER_r12;
        arg.size = 8;
    }else if(string_match(ident, string("r13"))){
        arg.reg  = ASM_REGISTER_r13;
        arg.size = 8;
    }else if(string_match(ident, string("r14"))){
        arg.reg  = ASM_REGISTER_r14;
        arg.size = 8;
    }else if(string_match(ident, string("r15"))){
        arg.reg  = ASM_REGISTER_r15;
        arg.size = 8;
    }else if(string_match(ident, string("eax"))){
        arg.reg  = ASM_REGISTER_rax;
        arg.size = 4;
    }else if(string_match(ident, string("ecx"))){
        arg.reg  = ASM_REGISTER_rcx;
        arg.size = 4;
    }else if(string_match(ident, string("edx"))){
        arg.reg  = ASM_REGISTER_rdx;
        arg.size = 4;
    }else if(string_match(ident, string("ebx"))){
        arg.reg  = ASM_REGISTER_rbx;
        arg.size = 4;
    }else if(string_match(ident, string("esi"))){
        arg.reg  = ASM_REGISTER_rsi;
        arg.size = 4;
    }else if(string_match(ident, string("edi"))){
        arg.reg  = ASM_REGISTER_rdi;
        arg.size = 4;
    }else if(string_match(ident, string("esp"))){
        arg.reg  = ASM_REGISTER_rsp;
        arg.size = 4;
    }else if(string_match(ident, string("ebp"))){
        arg.reg  = ASM_REGISTER_rbp;
        arg.size = 4;
    }else if(string_match(ident, string("r8d"))){
        arg.reg  = ASM_REGISTER_r8;
        arg.size = 4;
    }else if(string_match(ident, string("r9d"))){
        arg.reg  = ASM_REGISTER_r9;
        arg.size = 4;
    }else if(string_match(ident, string("r10d"))){
        arg.reg  = ASM_REGISTER_r10;
        arg.size = 4;
    }else if(string_match(ident, string("r11d"))){
        arg.reg  = ASM_REGISTER_r11;
        arg.size = 4;
    }else if(string_match(ident, string("r12d"))){
        arg.reg  = ASM_REGISTER_r12;
        arg.size = 4;
    }else if(string_match(ident, string("r13d"))){
        arg.reg  = ASM_REGISTER_r13;
        arg.size = 4;
    }else if(string_match(ident, string("r14d"))){
        arg.reg  = ASM_REGISTER_r14;
        arg.size = 4;
    }else if(string_match(ident, string("r15d"))){
        arg.reg  = ASM_REGISTER_r15;
        arg.size = 4;
    }else if(string_match(ident, string("ax"))){
        arg.reg  = ASM_REGISTER_rax;
        arg.size = 2;
    }else if(string_match(ident, string("cx"))){
        arg.reg  = ASM_REGISTER_rcx;
        arg.size = 2;
    }else if(string_match(ident, string("dx"))){
        arg.reg  = ASM_REGISTER_rdx;
        arg.size = 2;
    }else if(string_match(ident, string("bx"))){
        arg.reg  = ASM_REGISTER_rbx;
        arg.size = 2;
    }else if(string_match(ident, string("si"))){
        arg.reg  = ASM_REGISTER_rsi;
        arg.size = 2;
    }else if(string_match(ident, string("di"))){
        arg.reg  = ASM_REGISTER_rdi;
        arg.size = 2;
    }else if(string_match(ident, string("sp"))){
        arg.reg  = ASM_REGISTER_rsp;
        arg.size = 2;
    }else if(string_match(ident, string("bp"))){
        arg.reg  = ASM_REGISTER_rbp;
        arg.size = 2;
    }else if(string_match(ident, string("r8w"))){
        arg.reg  = ASM_REGISTER_r8;
        arg.size = 2;
    }else if(string_match(ident, string("r9w"))){
        arg.reg  = ASM_REGISTER_r9;
        arg.size = 2;
    }else if(string_match(ident, string("r10w"))){
        arg.reg  = ASM_REGISTER_r10;
        arg.size = 2;
    }else if(string_match(ident, string("r11w"))){
        arg.reg  = ASM_REGISTER_r11;
        arg.size = 2;
    }else if(string_match(ident, string("r12w"))){
        arg.reg  = ASM_REGISTER_r12;
        arg.size = 2;
    }else if(string_match(ident, string("r13w"))){
        arg.reg  = ASM_REGISTER_r13;
        arg.size = 2;
    }else if(string_match(ident, string("r14w"))){
        arg.reg  = ASM_REGISTER_r14;
        arg.size = 2;
    }else if(string_match(ident, string("r15w"))){
        arg.reg  = ASM_REGISTER_r15;
        arg.size = 2;
    }else if(string_match(ident, string("al"))){
        arg.reg  = ASM_REGISTER_rax;
        arg.size = 1;
    }else if(string_match(ident, string("cl"))){
        arg.reg  = ASM_REGISTER_rcx;
        arg.size = 1;
    }else if(string_match(ident, string("dl"))){
        arg.reg  = ASM_REGISTER_rdx;
        arg.size = 1;
    }else if(string_match(ident, string("bl"))){
        arg.reg  = ASM_REGISTER_rbx;
        arg.size = 1;
    }else /*if(string_match(ident, string("ah"))){ for now these are dead
    arg.reg  = ASM_REGISTER_rax;
    arg.size = 1;
    }else if(string_match(ident, string("ch"))){
    arg.reg  = ASM_REGISTER_rcx;
    arg.size = 1;
    }else if(string_match(ident, string("dh"))){
    arg.reg  = ASM_REGISTER_rdx;
    arg.size = 1;
    }else if(string_match(ident, string("bh"))){
    arg.reg  = ASM_REGISTER_rbx;
    arg.size = 1;
    }else */if(string_match(ident, string("sil"))){
        arg.reg  = ASM_REGISTER_rsi;
        arg.size = 1;
    }else if(string_match(ident, string("dil"))){
        arg.reg  = ASM_REGISTER_rdi;
        arg.size = 1;
    }else if(string_match(ident, string("spl"))){
        arg.reg  = ASM_REGISTER_rsp;
        arg.size = 1;
    }else if(string_match(ident, string("bpl"))){
        arg.reg  = ASM_REGISTER_rbp;
        arg.size = 1;
    }else if(string_match(ident, string("r8b"))){
        arg.reg  = ASM_REGISTER_r8;
        arg.size = 1;
    }else if(string_match(ident, string("r9b"))){
        arg.reg  = ASM_REGISTER_r9;
        arg.size = 1;
    }else if(string_match(ident, string("r10b"))){
        arg.reg  = ASM_REGISTER_r10;
        arg.size = 1;
    }else if(string_match(ident, string("r11b"))){
        arg.reg  = ASM_REGISTER_r11;
        arg.size = 1;
    }else if(string_match(ident, string("r12b"))){
        arg.reg  = ASM_REGISTER_r12;
        arg.size = 1;
    }else if(string_match(ident, string("r13b"))){
        arg.reg  = ASM_REGISTER_r13;
        arg.size = 1;
    }else if(string_match(ident, string("r14b"))){
        arg.reg  = ASM_REGISTER_r14;
        arg.size = 1;
    }else if(string_match(ident, string("r15b"))){
        arg.reg  = ASM_REGISTER_r15;
        arg.size = 1;
    }else{
        prev_token(context);
    }
    
    if(arg.reg != ASM_REGISTER_invalid){
        arg.kind = ASM_ARG_register;
    }
    
    return arg;
}

func struct asm_operand asm_maybe_parse_xmm_register(struct context *context){
    struct asm_operand arg = zero_struct;
    
    if(!peek_token(context, TOKEN_identifier)) return arg;
    arg.reg = ASM_REGISTER_invalid;
    arg.register_kind_when_loaded = REGISTER_KIND_xmm;
    
    struct token *ident = next_token(context);
    
    struct string string = token_get_string(ident);
    if(string.size == 4){
        if(string_front_match(string, "xmm") && '0' <= string.data[3] && string.data[3] <= '9'){
            arg.reg  = ASM_REGISTER_xmm0 + string.data[3] - '0';
            arg.size = 16;
        }
    }else if(string.size == 5){
        if(string_front_match(string, "xmm1") && '0' <= string.data[4] && string.data[4] <= '5'){
            arg.reg  = ASM_REGISTER_xmm10 + string.data[4] - '0';
            arg.size = 16;
        }
    }
    
    if(arg.reg == ASM_REGISTER_invalid){
        prev_token(context);
    }else{
        arg.kind = ASM_ARG_register;
    }
    
    return arg;
}

func struct asm_operand asm_maybe_parse_ymm_register(struct context *context){
    struct asm_operand arg = zero_struct;
    
    if(!peek_token(context, TOKEN_identifier)) return arg;
    arg.reg = ASM_REGISTER_invalid;
    arg.register_kind_when_loaded = REGISTER_KIND_xmm;
    
    struct token *ident = next_token(context);
    
    struct string string = token_get_string(ident);
    if(string.size == 4){
        if(string_front_match(string, "ymm") && '0' <= string.data[3] && string.data[3] <= '9'){
            arg.reg  = ASM_REGISTER_xmm0 + string.data[3] - '0';
            arg.size = 32;
        }
    }else if(string.size == 5){
        if(string_front_match(string, "ymm1") && '0' <= string.data[4] && string.data[4] <= '5'){
            arg.reg  = ASM_REGISTER_xmm10 + string.data[4] - '0';
            arg.size = 32;
        }
    }
    
    if(arg.reg == ASM_REGISTER_invalid){
        prev_token(context);
    }else{
        arg.kind = ASM_ARG_register;
    }
    
    return arg;
}

func struct asm_operand asm_maybe_parse_expression_operand(struct context *context){
    
    struct asm_operand operand = {0};
    
    u8 *expression_at = arena_current(&context->ir_arena);
    
    // @cleanup: ASM_ARG_declaration_dereference for '*decl' and 'decl[10]'.
    struct expr expr = parse_expression(context, /*skip_comma = */true);
    if(context->should_exit_statement) return operand;
    
    if(expr.ir->kind == IR_integer_literal){
        // Integer literals are fine!
        operand.kind      = ASM_ARG_immediate;
        operand.immediate = integer_literal_as_u64(&expr);
        operand.size      = expr.resolved_type->size;
        
        pop_from_ir_arena(context, (struct ir_integer_literal *)expr.ir);
        return operand;
    }
    
    operand.kind = ASM_ARG_declaration_reference;
    operand.size = expr.resolved_type->size;
    
    if(context->in_inline_asm_function && expr.ir->kind == IR_identifier){
        struct ir_identifier *ident = (struct ir_identifier *)expr.ir;
        
        //
        // @cleanup: is there not just a flag somewhere we can check?
        //
        for_ast_list(context->current_function->type->argument_list){
            struct ast_declaration *decl = (struct ast_declaration *)it->value;
            if(ident->decl == decl){
                operand.is_inline_asm_function_argument = true;
                break;
            }
        }
        // fall through
    }
    
    //
    // Check that the expression does not allocate any registers!
    //
    
    int expression_okay = 1;
    
    if(((struct ir *)expression_at)->kind != IR_identifier){
        expression_okay = 0;    
    }else{
        struct ir_identifier *identifier = (struct ir_identifier *)expression_at;
        operand.declaration = identifier->decl;
        expression_at += sizeof(struct ir_identifier);
        
        struct ast_type *type = operand.declaration->type;
        u64 offset = 0;
        
        u8 *expression_end = arena_current(&context->ir_arena);
        
        while(expression_at < expression_end){
            
            if(((struct ir *)expression_at)->kind == IR_member){
                struct ir_dot_or_arrow *dot = (struct ir_dot_or_arrow *)expression_at;
                expression_at += sizeof(struct ir_dot_or_arrow);
                
                offset += dot->member->offset_in_type;
                type = dot->member->type;
            }else if(((struct ir *)expression_at)->kind == IR_integer_literal){
                struct ir_integer_literal *integer = (struct ir_integer_literal *)expression_at;
                expression_at += sizeof(struct ir_integer_literal);
                struct expr integer_expr = {
                    .ir = &integer->base,
                    .resolved_type = integer->type,
                };
                
                if(((struct ir *)expression_at)->kind == IR_array_subscript){
                    struct ir_subscript *subscript = (struct ir_subscript *)expression_at;
                    expression_at += sizeof(struct ir_subscript);
                    
                    type = subscript->type;
                    offset += type->size * integer_literal_as_u64(&integer_expr);
                }else{
                    expression_okay = 0;
                    break;
                }
            }else{
                expression_okay = 0;
                break;
            }
        }
        
        operand.declaration_offset = offset;
    }
    
    if(!expression_okay){
        report_error(context, expr.token, "Currently, only expressions of the form 'a.b.c' or 'array[<const>]' are allowed as inline asm operands.");
    }
    
    struct ast_type *type = expr.resolved_type;
    if(type->kind != AST_integer_type && type->kind != AST_float_type && type->kind != AST_pointer_type && !(type->flags & TYPE_FLAG_is_intrin_type)){
        char *remark = "";
        if(type->kind == AST_union || type->kind == AST_struct){
            remark = " (If you defined your own *mm types, please mark them __declspec(intrin_type)).";
        }
        
        report_error(context, expr.token, "Invalid type for asm operand.%s", remark);
    }
    
    operand.register_kind_when_loaded = get_register_kind_for_type(type);
    
    return operand;
}

func struct asm_operand asm_maybe_parse_memory_operand(struct context *context){
    struct asm_operand arg = zero_struct;
    
    struct token *size_ident = get_current_token(context);
    struct string size_ident_string = token_get_string(size_ident);
    
    smm size = 0; // zero means inferable
    if(string_match(size_ident_string, string("byte"))){
        size = 1;
    }else if(string_match(size_ident_string, string("word"))){
        size = 2;
    }else if(string_match(size_ident_string, string("dword"))){
        size = 4;
    }else if(string_match(size_ident_string, string("qword"))){
        size = 8;
    }else if(string_match(size_ident_string, string("xmmword"))){
        size = 16;
    }else if(string_match(size_ident_string, string("ymmword"))){
        size = 32;
    }
    
    // we want to allow byte, word, dword, qword as variable names.
    // thus we check here if the next token is exactly ptr
    if(size){
        next_token(context);
        
        if(!peek_token(context, TOKEN_identifier)){
            prev_token(context); // size_ident
            return arg;
        }
        
        if(!string_match(token_get_string(next_token(context)), string("ptr"))){
            prev_token(context); // ptr
            prev_token(context); // size_ident
            return arg;
        }
    }
    
    b32 is_gs_relative = false;
    if(peek_token(context, TOKEN_identifier)){
        struct token *token = get_current_token(context);
        if(string_match(token_get_string(token), string("gs"))){
            next_token(context);
            expect_token(context, TOKEN_colon, "Expected a ':' to follow the 'gs' prefix.");
            is_gs_relative = true;
        }
    }
    
    if(!peek_token_eat(context, TOKEN_open_index)){
        if(size || is_gs_relative){
            report_error(context, get_current_token_for_error_report(context), "Expected a '[' in a memory operand.");
        }
        return arg;
    }
    
    enum asm_register base  = -1;
    enum asm_register index = -1;
    smm scale  = 1;
    smm offset = 0;
    b32 offset_is_negative = false;
    
    b32 finished = false;
    
    // [rax + 4 * rcx +/- 0x123]
    if(peek_token(context, TOKEN_identifier)){
        struct asm_operand reg = asm_maybe_parse_gpr(context);
        if(reg.kind != ASM_ARG_invalid){
            base = reg.reg;
            
            finished = (peek_token_eat(context, TOKEN_plus) == null);
            
            if(finished && peek_token(context, TOKEN_minus)){
                offset_is_negative = true;
                goto skip_scale;
            }
        }
    }
    
    if(!finished && peek_token(context, TOKEN_base10_literal)){
        struct token *scale_or_offset = next_token(context);
        if(peek_token_eat(context, TOKEN_times)){
            struct asm_operand reg = asm_maybe_parse_gpr(context);
            if(reg.kind == ASM_ARG_invalid){
                report_syntax_error(context, get_current_token(context), "Expected a register name after '*' in asm memory operand.");
            }else{
                scale = parse_base10_literal(context, scale_or_offset).value;
                index = reg.reg;
                
                finished = peek_token_eat(context, TOKEN_plus) || peek_token_eat(context, TOKEN_minus);
            }
        }else{
            prev_token(context);
        }
    }
    
    if(!finished){
        skip_scale:
        
        struct token *start_token = get_current_token(context);
        struct asm_operand operand = asm_maybe_parse_expression_operand(context);
        expect_token(context, TOKEN_closed_index, "Expected ']' ending the memory operand.");
        
        if(operand.kind == ASM_ARG_immediate){
            // @cleanup: report on overflow and so on.
            offset = operand.immediate;
        }else if(operand.kind == ASM_ARG_declaration_reference){
            
            smm pointer_to_size = 0;
            if(operand.declaration->type->kind != AST_pointer_type){
                // @note: The token is sort of a hack.
                report_error(context, start_token, "Expression inside memory operand must be a pointer.");
            }else{
                pointer_to_size = ((struct ast_pointer_type *)operand.declaration->type)->pointer_to->size;
            }
            
            operand.kind = ASM_ARG_declaration_dereference;
            if(size){
                operand.size = size;
            }else{
                // Infer the size from the type of the expression.
                operand.size = pointer_to_size;
            }
            
            return operand;
        }else if(operand.kind == ASM_ARG_declaration_dereference){
            report_error(context, start_token, "Cannot have a derefenrence inside of a memory operand (e.g.: 'mov [*pointer], 1' is illegal).");
        }else{
            // @note: There should have already been an error, but just to make sure, here is a bad error message!
            report_syntax_error(context, get_current_token_for_error_report(context), "Invalid asm memory operand.");
        }
    }else{
        expect_token(context, TOKEN_closed_index, "Expected ']' ending the memory operand.");
    }
    
    
    arg.kind   = ASM_ARG_memory_operand;
    arg.base   = base;
    arg.index  = index;
    arg.scale  = scale;
    arg.offset = offset_is_negative ? -offset : offset;
    arg.size   = size;
    if(is_gs_relative) context->current_asm_flags |= ASM_PREFIX_gs;
    
    return arg;
}

func void add_memonic_to_string_to_memonic_table(struct string string, enum memonic memonic){

    u64 hash = string_djb2_hash(string);
    for(u32 i = 0; i < array_count(globals.memonic_table); i++){
        u32 index = (hash + i) & (array_count(globals.memonic_table) - 1);
        if(globals.memonic_table[index].string.data == 0){
            globals.memonic_table[index].string = string;
            globals.memonic_table[index].memonic = memonic;
            return;
        }
        
#if 1
        if(string_match(string, globals.memonic_table[index].string)){
            print("memonic '%.*s' added twice\n", string.size, string.data);
            os_panic(1);
            return;
        }
#endif
    }
    invalid_code_path;
}

func enum memonic lookup_memonic_from_string(struct string string){
    enum memonic memonic = MEMONIC_none;
    u64 hash = string_djb2_hash(string);
    for(u32 i = 0; i < array_count(globals.memonic_table); i++){
        u32 index = (hash + i) & (array_count(globals.memonic_table) - 1);
        if(globals.memonic_table[index].string.data == 0) break;
        
        if(string_match(string, globals.memonic_table[index].string)){
            memonic = globals.memonic_table[index].memonic;
            return memonic;
        }
    }
    return memonic;
}

func struct asm_operand parse_asm_operand(struct context *context){
    
    struct asm_operand operand = asm_maybe_parse_gpr(context);
    if(operand.kind != ASM_ARG_invalid) return operand;
    
    operand = asm_maybe_parse_xmm_register(context);
    if(operand.kind != ASM_ARG_invalid) return operand;
    
    operand = asm_maybe_parse_ymm_register(context);
    if(operand.kind != ASM_ARG_invalid) return operand;
    
    operand = asm_maybe_parse_memory_operand(context);
    if(operand.kind != ASM_ARG_invalid) return operand;
    
    return asm_maybe_parse_expression_operand(context);
}

func struct asm_instruction *parse_asm_instruction(struct context *context){
    context->current_asm_flags = 0;
    
    retry_because_there_was_a_prefix:
    // this does not work because of int
    // struct token *memonic = expect_token(context, TOKEN_identifier, "Expected a memonic in '__asm__' block");
    
    struct token *token = next_token(context);
    enum memonic memonic = lookup_memonic_from_string(token_get_string(token));
    
    if(memonic == MEMONIC_lock_prefix){
        context->current_asm_flags |= ASM_PREFIX_lock;
        goto retry_because_there_was_a_prefix;
    }
    
    if(memonic == MEMONIC_repe_prefix){
        context->current_asm_flags |= ASM_PREFIX_repe;
        goto retry_because_there_was_a_prefix;
    }
    
    if(memonic == MEMONIC_movsd){
        if((peek_token(context, TOKEN_closed_curly) != null) || lookup_memonic_from_string(token_get_string(get_current_token(context))) != MEMONIC_none){
            //
            // if the instruction is movsd and the next token is either a '}' indicating the end of the end of the __asm__-block,
            // or a memonic then its a rep-movsd situation.
            // In this case, just return the movsd here
            //
            
            return push_asm_instruction(context, MEMONIC_movsd, 0, token);
        }
    }
    
    if(memonic == MEMONIC_bytes){
        struct asm_instruction *wrapper_instruction = push_asm_instruction(context, MEMONIC_bytes, 0, token);
        
        expect_token(context, TOKEN_open_curly, "Expected a '{' after 'bytes' in inline asm block.");
        
        u8 *data = push_data(&context->scratch, u8, 0x100);
        u32 amount = 0;
        u32 max = 0x100;
        
        while(in_current_token_array(context) && !peek_token(context, TOKEN_closed_curly)){
            
            struct token *byte_token = next_token(context);
            
            if(!token){
                report_error(context, get_current_token_for_error_report(context), "Expected byte in bytes section.");
                return wrapper_instruction;
            }
            
            struct string lit = byte_token->string;
            if(lit.size >= 2 && lit.data[0] == '0' && (lit.data[1]|32) == 'x'){
                lit.size -= 2;
                lit.data += 2;
            }
            
            if(lit.size != 2){
                report_error(context, byte_token, "Expected a byte of the form \"cd\" or \"0xcd\" in bytes memonic.");
                return wrapper_instruction;
            }
            
            u64 value = parse_hex_string_to_u64(&lit, 0);
            
            if(amount + 1 > max){
                u8 *new_data = push_data(&context->scratch, u8, 2 * max);
                memcpy(new_data, data, amount);
                max = 2 * max;
                data = new_data;
            }
            
            data[amount++] = (u8)value;
            
            peek_token_eat(context, TOKEN_comma);
        }
        
        wrapper_instruction->operands[0].size = amount;
        wrapper_instruction->operands[0].data = push_data(context->arena, u8, amount);
        memcpy(wrapper_instruction->operands[0].data, data, amount);
        
        expect_token(context, TOKEN_closed_curly, "Expected a '}' at the end of bytes.");
        return wrapper_instruction;
    }
    
    u32 amount_of_operands = asm_parse_table[memonic].amount_of_operands;
    struct asm_instruction *asm_instruction = push_asm_instruction(context, memonic, amount_of_operands, token);
    
    assert(memonic < MEMONIC_count);
    for(u32 operand_index = 0; operand_index < amount_of_operands; operand_index++){
        enum asm_operand_kind_flags desired_operand_flags = asm_parse_table[memonic].operand_kind_flags[operand_index];
        
        if(operand_index){
            expect_token(context, TOKEN_comma, "Expected a ',' separating asm operands.");
        }
        
        struct token *operand_token = get_current_token(context);
        struct asm_operand operand = parse_asm_operand(context);
        
        switch(operand.kind){
            case ASM_ARG_invalid: return null;
            case ASM_ARG_register:{
                if(operand.register_kind_when_loaded == REGISTER_KIND_xmm){
                    // xmm
                    b32 should_error = false;
                    if(operand.size == 16 && !(ASM_OP_KIND_xmm & desired_operand_flags)) should_error = true;
                    if(operand.size == 32 && !(ASM_OP_KIND_ymm & desired_operand_flags)) should_error = true;
                    
                    if(should_error){
                        report_error(context, operand_token, "Operand %u of '%.*s' is invalid.", operand_index + 1, token->size, token->data);
                    }
                }else if(operand.register_kind_when_loaded == REGISTER_KIND_gpr){
                    // gpr
                    b32 should_error = false;
                    if(operand.size == 1 && !(ASM_OP_KIND_reg8  & desired_operand_flags)) should_error = true;
                    if(operand.size == 2 && !(ASM_OP_KIND_reg16 & desired_operand_flags)) should_error = true;
                    if(operand.size == 4 && !(ASM_OP_KIND_reg32 & desired_operand_flags)) should_error = true;
                    if(operand.size == 8 && !(ASM_OP_KIND_reg64 & desired_operand_flags)) should_error = true;
                    
                    if(should_error){
                        report_error(context, operand_token, "Operand %u of '%.*s' is invalid.", operand_index + 1, token->size, token->data);
                    }
                }else{
                    invalid_code_path;
                }
            }break;
            case ASM_ARG_declaration_dereference:
            case ASM_ARG_memory_operand:{
                b32 should_error = false;
                
                switch(operand.size){
                    case 1:  should_error = !(desired_operand_flags & ASM_OP_KIND_mem8);   break;
                    case 2:  should_error = !(desired_operand_flags & ASM_OP_KIND_mem16);  break;
                    case 4:  should_error = !(desired_operand_flags & ASM_OP_KIND_mem32);  break;
                    case 8:  should_error = !(desired_operand_flags & ASM_OP_KIND_mem64);  break;
                    case 16: should_error = !(desired_operand_flags & ASM_OP_KIND_mem128); break;
                    case 32: should_error = !(desired_operand_flags & ASM_OP_KIND_mem256); break;
                    
                    case 0:{
                        //
                        // we are supposed to infer the operand size!
                        //
                        u32 memory_flags = desired_operand_flags & ASM_OP_KIND_any_mem;
                        
                        if(!memory_flags){
                            report_error(context, operand_token, "Operand %u of '%.*s' cannot be a memory operand.", operand_index + 1, token->size, token->data);
                        }
                        
                        if(__popcnt(memory_flags) == 1){
                            //
                            // infer the operand size
                            //
                            
                            if(memory_flags & ASM_OP_KIND_mem8)   operand.size = 1;
                            if(memory_flags & ASM_OP_KIND_mem16)  operand.size = 2;
                            if(memory_flags & ASM_OP_KIND_mem32)  operand.size = 4;
                            if(memory_flags & ASM_OP_KIND_mem64)  operand.size = 8;
                            if(memory_flags & ASM_OP_KIND_mem128) operand.size = 16;
                            if(memory_flags & ASM_OP_KIND_mem256) operand.size = 32;
                        }else{
                            //
                            // This will be handled explicitly below (in the big switch).
                            //
                        }
                    }break;
                    invalid_default_case();
                }
                
                if(should_error){
                    report_error(context, operand_token, "Operand %u of '%.*s' is invalid.", operand_index + 1, token->size, token->data);
                }
            }break;
            case ASM_ARG_declaration_reference:{
                b32 should_error = false;
                
                struct ast_type *type = operand.declaration->type;
                if(operand.is_inline_asm_function_argument && type->kind == AST_integer_type){
                    
                    if((desired_operand_flags & ASM_OP_KIND_any_imm) == (desired_operand_flags & (ASM_OP_KIND_any_regm | ASM_OP_KIND_any_imm))){
                        // If the operand needs to be an integer and we supplied a 'inline_asm_argument'
                        
                        operand.is_inline_asm_function_argument_that_needs_to_be_an_integer = true;
                        break;
                    }
                    
                    if(desired_operand_flags & ASM_OP_KIND_any_imm){
                        operand.is_inline_asm_function_argument_that_can_be_integer = true;
                        break;
                    }
                }
                
                if(type->kind == AST_integer_type){
                    
                    if(type->size == 1 && !(ASM_OP_KIND_regm8  & desired_operand_flags)) should_error = true;
                    if(type->size == 2 && !(ASM_OP_KIND_regm16 & desired_operand_flags)) should_error = true;
                    if(type->size == 4 && !(ASM_OP_KIND_regm32 & desired_operand_flags)) should_error = true;
                    if(type->size == 8 && !(ASM_OP_KIND_regm64 & desired_operand_flags)) should_error = true;
                }else if(type->kind == AST_float_type){
                    
                    if(operand.size ==  4 && !(ASM_OP_KIND_xmmm32 & desired_operand_flags)) should_error = true;
                    if(operand.size ==  8 && !(ASM_OP_KIND_xmmm64 & desired_operand_flags)) should_error = true;
                }else if(type->kind == AST_pointer_type){
                    
                    if(type->size == 8 && !(ASM_OP_KIND_regm64 & desired_operand_flags)) should_error = true;
                }else if(type->flags & TYPE_FLAG_is_intrin_type){
                    
                    if(operand.size == 16 && !(ASM_OP_KIND_xmmm128 & desired_operand_flags)) should_error = true;
                    if(operand.size == 32 && !(ASM_OP_KIND_ymmm256 & desired_operand_flags)) should_error = true;
                }
                
                if(should_error){
                    report_error(context, operand_token, "Operand %u of '%.*s' is invalid.", operand_index + 1, token->size, token->data);
                }
            }break;
            case ASM_ARG_immediate:{
                // @cleanup: this should warn if the immediate does not fit. This needs to know both signness things.
                
                if(desired_operand_flags & ASM_OP_KIND_imm64){
                    // we are good, any integer literal fits in a 64 bit
                }else if(desired_operand_flags & ASM_OP_KIND_imm32){
                    
                }else if(desired_operand_flags & ASM_OP_KIND_imm16){
                    
                }else if(desired_operand_flags & ASM_OP_KIND_imm8){
                    operand.size = 1;
                }else{
                    report_error(context, operand_token, "Operand %u of '%.*s' cannot be an integer.", operand_index + 1, token->size, token->data);
                }
            }break;
            invalid_default_case();
        }
        
        asm_instruction->operands[operand_index] = operand;
    }
    
    smm regm_index = -1;
    for(smm operand_index = 0; operand_index < amount_of_operands; operand_index++){
        if(asm_instruction->operands[operand_index].size == 0){
            assert(asm_instruction->operands[operand_index].kind == ASM_ARG_memory_operand || /*[void_ptr]*/asm_instruction->operands[operand_index].kind == ASM_ARG_declaration_dereference);
        }
        
        if(asm_instruction->operands[operand_index].kind == ASM_ARG_memory_operand || asm_instruction->operands[operand_index].kind == ASM_ARG_declaration_dereference){
            if(regm_index == -1){
                regm_index = operand_index;
            }else{
                report_error(context, token, "x64-instructions can only have one memory operand.");
            }
        }
    }
    
    switch(memonic){
        
        //
        // Two operand instructions that either have matching operand sizes, or an immediate as the second one.
        //
        
        case MEMONIC_movnti:
        case MEMONIC_cmovo:  case MEMONIC_cmovc:  case MEMONIC_cmovz:  case MEMONIC_cmovbe:  case MEMONIC_cmovs:  case MEMONIC_cmovp:  case MEMONIC_cmovl:  case MEMONIC_cmovle:
        case MEMONIC_cmovno: case MEMONIC_cmovnc: case MEMONIC_cmovnz: case MEMONIC_cmovnbe: case MEMONIC_cmovns: case MEMONIC_cmovnp: case MEMONIC_cmovnl: case MEMONIC_cmovnle:
        case MEMONIC_mov:
        case MEMONIC_xchg: // 'op reg/m, reg' or 'op reg, reg/m'
        case MEMONIC_xadd: case MEMONIC_cmpxchg: // 'op reg/m, reg'
        case MEMONIC_add:  case MEMONIC_or: // 'op regm, imm8' or 'op regm, imm' or 'op regm, reg' or 'op reg, regm'
        case MEMONIC_adc:  case MEMONIC_sbb:
        case MEMONIC_and:  case MEMONIC_sub:
        case MEMONIC_xor:  case MEMONIC_cmp:
        case MEMONIC_bt:   case MEMONIC_bts: case MEMONIC_btr: case MEMONIC_btc: // 'op regm, reg' or 'op regm, imm8'
        case MEMONIC_bsf:  case MEMONIC_bsr: // 'op reg, regm'
        case MEMONIC_popcnt:{                // 'op reg, regm'
            
            //
            // Operands have already been _base-checked_ thus we can more or less throw any two operand,
            // gpr-operation in here.
            //
            
            if(asm_instruction->operands[1].kind == ASM_ARG_immediate){
                if(asm_instruction->operands[0].size == 0){
                    report_error(context, token, "Cannot infer operand size for memory operand.");
                }
                
                if(asm_instruction->operands[1].immediate < 128 || (s64)asm_instruction->operands[1].immediate > -127){
                    asm_instruction->operands[1].size = 1;
                }else{
                    
                    // size 1 can be infered above
                    if(asm_instruction->operands[0].size == 2) asm_instruction->operands[1].size = 2;
                    if(asm_instruction->operands[0].size == 4) asm_instruction->operands[1].size = 4;
                }
                
                // this is fine for now @cleanup: report warning if the immediate does not fit or if signness is not right
                break;
            }
            
            if(regm_index != -1 && asm_instruction->operands[regm_index].size == 0){
                smm other_index = (regm_index + 1) & 1;
                asm_instruction->operands[regm_index].size = asm_instruction->operands[other_index].size;
            }
            
            if(asm_instruction->operands[0].size != asm_instruction->operands[1].size){
                // @cleanup: should we try to perform a 'is_inline_asm_function_argument_that_needs_to_be_an_integer'?
                report_error(context, token, "Operand size mismatch. %lld vs %lld.", asm_instruction->operands[0].size, asm_instruction->operands[1].size);
            }
        }break;
        
        case MEMONIC_rol: case MEMONIC_ror: // 'op regm, imm8' or 'op regm, cl'
        case MEMONIC_rcl: case MEMONIC_rcr:
        case MEMONIC_shl: case MEMONIC_shr:
        case MEMONIC_sal: case MEMONIC_sar:{
            if(asm_instruction->operands[1].kind == ASM_ARG_register && asm_instruction->operands[1].reg != ASM_REGISTER_rcx){
                report_error(context, token, "The right side needs to be either 'cl' or an 8-bit integer.");
            }
        }break;
        
        // no need to do anything, can only have xmm register operands
        case MEMONIC_movlhps: case MEMONIC_movhlps: 
        
        //
        // Zero Operand Memonics
        //
        case MEMONIC_pause: case MEMONIC_sfence: case MEMONIC_lfence: case MEMONIC_mfence:
        case MEMONIC_int:   case MEMONIC_int3: case MEMONIC_ret:
        case MEMONIC_cpuid: case MEMONIC_xgetbv:
        case MEMONIC_rdtsc: case MEMONIC_rdtscp:
        case MEMONIC_movsb: case MEMONIC_movsw:                     case MEMONIC_movsq:
        case MEMONIC_stosb: case MEMONIC_stosw: case MEMONIC_stosd: case MEMONIC_stosq:{
            // nothing to do here!
        }break;
        
        case MEMONIC_seto:  case MEMONIC_setno:
        case MEMONIC_setc:  case MEMONIC_setnc:
        case MEMONIC_setz:  case MEMONIC_setnz:
        case MEMONIC_setbe: case MEMONIC_setnbe:
        case MEMONIC_sets:  case MEMONIC_setns:
        case MEMONIC_setp:  case MEMONIC_setnp:
        case MEMONIC_setl:  case MEMONIC_setnl:
        case MEMONIC_setle: case MEMONIC_setnle:{
            if(asm_instruction->operands[0].size == 0) asm_instruction->operands[0].size = 1;
        }break;
        
        
        case MEMONIC_neg: 
        case MEMONIC_inc: case MEMONIC_dec:
        case MEMONIC_movzx: case MEMONIC_movsx: case MEMONIC_movsxd:{
            if(asm_instruction->operands[0].size == 0){
                report_error(context, token, "Cannot infer operand size for memory operand.");
            }
        }break;
        
        case MEMONIC_cvtsi2ss: case MEMONIC_cvtsi2sd:{
            if(asm_instruction->operands[1].size == 0){ 
                report_error(context, token, "Cannot infer operand size for memory operand.");
            }
        }break;
        
        case MEMONIC_lea: case MEMONIC_clflush:{
            asm_instruction->operands[0].size = 4;
        }break;
        
        // no need to do anything, we made sure that these only take memory addresses and the size does not matter to us
         case MEMONIC_prefetch:{
            if(asm_instruction->operands[1].size == 0){
                asm_instruction->operands[1].size = 4;
            }
        }break;
        
        case MEMONIC_ldmxcsr: case MEMONIC_stmxcsr:        
        case MEMONIC_cmpxchg16b: case MEMONIC_cmpxchg8b:{
            if(asm_instruction->operands[0].size == 0){
                asm_instruction->operands[0].size = 4;
            }
        }break;
        
        //
        // SSE
        //
        case MEMONIC_vmovd: case MEMONIC_vmovq:
        case MEMONIC_movq: case MEMONIC_movd:{
            if(asm_instruction->operands[0].register_kind_when_loaded == asm_instruction->operands[1].register_kind_when_loaded){
                if(asm_instruction->operands[0].register_kind_when_loaded == REGISTER_KIND_gpr){
                    report_error(context, token, "One operand needs to be a XMM register.");
                }else{
                    report_error(context, token, "One operand needs to be a general purpose register or a memory location.");
                }
            }
            
            if(regm_index != -1 && asm_instruction->operands[regm_index].size == 0){
                if(memonic == MEMONIC_movq){
                    asm_instruction->operands[regm_index].size = 8;
                }else{
                    asm_instruction->operands[regm_index].size = 4;
                }
            }
        }break;
        
        case MEMONIC_aesdec: case MEMONIC_pshufb:
        case MEMONIC_movntps: // op xmm/m, xmm
        case MEMONIC_movntpd:
        case MEMONIC_movntdq:
        case MEMONIC_movups: case MEMONIC_movaps: // "op xmm/m, xmm" or "op xmm, xmm/m"
        case MEMONIC_movupd: case MEMONIC_movapd:
        case MEMONIC_movdqa: case MEMONIC_movdqu:{
            if(regm_index != -1 && asm_instruction->operands[regm_index].size == 0){
                asm_instruction->operands[regm_index].size = 16;
            }
        }break;
        
        case MEMONIC_vpmovmskb:
        case MEMONIC_pmovmskb:
        case MEMONIC_movmskps: case MEMONIC_movmskpd:{
            // we allow you to write 'pmovmskb rax, xmm0' even tho it only takes 'eax'.
            asm_instruction->operands[0].size = 4;
        }break;
        
        // op xmm, xmm/m32
        case MEMONIC_addss: case MEMONIC_subss:  case MEMONIC_mulss: case MEMONIC_divss:
        case MEMONIC_rcpss: case MEMONIC_sqrtss: case MEMONIC_maxss: case MEMONIC_minss:
        case MEMONIC_rsqrtss: case MEMONIC_comiss: case MEMONIC_ucomiss:
        
        case MEMONIC_cvtss2sd: case MEMONIC_cvtss2si: case MEMONIC_cvttss2si:
        case MEMONIC_cmpeqss:  case MEMONIC_cmpltss:  case MEMONIC_cmpless:  case MEMONIC_cmpunordss:
        case MEMONIC_cmpneqss: case MEMONIC_cmpnltss: case MEMONIC_cmpnless: case MEMONIC_cmpordss:{
            if(asm_instruction->operands[1].size == 0) asm_instruction->operands[1].size = 4;
        }break;
        
        case MEMONIC_cvtsd2ss: case MEMONIC_cvtsd2si: case MEMONIC_cvttsd2si:
        case MEMONIC_addsd:    case MEMONIC_subsd:    case MEMONIC_mulsd: case MEMONIC_divsd:
        case MEMONIC_sqrtsd:   case MEMONIC_maxsd:    case MEMONIC_minsd:
        case MEMONIC_comisd:   case MEMONIC_ucomisd:
        
        case MEMONIC_cmpeqsd:  case MEMONIC_cmpltsd:  case MEMONIC_cmplesd:  case MEMONIC_cmpunordsd:
        case MEMONIC_cmpneqsd: case MEMONIC_cmpnltsd: case MEMONIC_cmpnlesd: case MEMONIC_cmpordsd:
        {
            if(asm_instruction->operands[1].size == 0) asm_instruction->operands[1].size = 8;
        }break;
        
        case MEMONIC_movlpd: case MEMONIC_movlps:
        case MEMONIC_movhpd: case MEMONIC_movhps:{
            if(asm_instruction->operands[0].size == 0) asm_instruction->operands[0].size = 8;
            if(asm_instruction->operands[1].size == 0) asm_instruction->operands[1].size = 8;
            
            if(regm_index == -1){
                report_error(context, token, "One operand of '%.*s' has to be a memory location.", token->size, token->data);
            }
        }break;
        
        // op xmm, xmm/m
        
        case MEMONIC_cvtdq2ps:  case MEMONIC_cvtps2dq: 
        case MEMONIC_cvtdq2pd:  case MEMONIC_cvtpd2dq:
        
        case MEMONIC_cvttps2dq: case MEMONIC_cvttpd2dq:
        
        case MEMONIC_cvtpd2ps: case MEMONIC_cvtps2pd:
        
        case MEMONIC_unpckhps: case MEMONIC_unpckhpd:
        case MEMONIC_unpcklps: case MEMONIC_unpcklpd:
        
        case MEMONIC_pcmpgtb: case MEMONIC_pcmpgtw: case MEMONIC_pcmpgtd:
        // case MEMONIC_pcmpltb: case MEMONIC_pcmpltw: case MEMONIC_pcmpltd:
        
        case MEMONIC_pmaddwd:   case MEMONIC_pmullw:
        case MEMONIC_paddb:     case MEMONIC_paddw:     case MEMONIC_paddd:     case MEMONIC_paddq:
        case MEMONIC_psubb:     case MEMONIC_psubw:     case MEMONIC_psubd:     case MEMONIC_psubq:
        case MEMONIC_paddsb:    case MEMONIC_paddsw:
        case MEMONIC_psubsb:    case MEMONIC_psubsw:
        case MEMONIC_paddusb:   case MEMONIC_paddusw:
        case MEMONIC_psubusb:   case MEMONIC_psubusw:
        case MEMONIC_pavgb:     case MEMONIC_pavgw:
        
        case MEMONIC_punpcklbw: case MEMONIC_punpcklwd: case MEMONIC_punpckldq: case MEMONIC_punpcklqdq:
        case MEMONIC_punpckhbw: case MEMONIC_punpckhwd: case MEMONIC_punpckhdq: case MEMONIC_punpckhqdq:
        case MEMONIC_packssdw:  case MEMONIC_packsswb:  case MEMONIC_packuswb:
        case MEMONIC_pxor:      case MEMONIC_por:       case MEMONIC_pand:      case MEMONIC_pandn:
        
        case MEMONIC_addps: case MEMONIC_subps:  case MEMONIC_mulps: case MEMONIC_divps:
        case MEMONIC_rcpps: case MEMONIC_sqrtps: case MEMONIC_maxps: case MEMONIC_minps:
        case MEMONIC_rsqrtps:
        case MEMONIC_haddps:
        
        case MEMONIC_andps: case MEMONIC_andnps: case MEMONIC_xorps: case MEMONIC_orps:
        
        case MEMONIC_addpd: case MEMONIC_subpd:  case MEMONIC_mulpd: case MEMONIC_divpd:
        case MEMONIC_sqrtpd: case MEMONIC_maxpd: case MEMONIC_minpd:
        case MEMONIC_haddpd:
        
        case MEMONIC_cmpeqps:  case MEMONIC_cmpltps:  case MEMONIC_cmpleps:  case MEMONIC_cmpunordps:
        case MEMONIC_cmpneqps: case MEMONIC_cmpnltps: case MEMONIC_cmpnleps: case MEMONIC_cmpordps:
        
        case MEMONIC_cmpeqpd:  case MEMONIC_cmpltpd:  case MEMONIC_cmplepd:  case MEMONIC_cmpunordpd:
        case MEMONIC_cmpneqpd: case MEMONIC_cmpnltpd: case MEMONIC_cmpnlepd: case MEMONIC_cmpordpd:
        
        case MEMONIC_andpd: case MEMONIC_andnpd: case MEMONIC_xorpd:  case MEMONIC_orpd:
        
        case MEMONIC_pcmpeqb:   case MEMONIC_pcmpeqw:   case MEMONIC_pcmpeqd:
        case MEMONIC_pminub: case MEMONIC_pminuw:
        case MEMONIC_pmaxub: case MEMONIC_pmaxuw:
        case MEMONIC_pminsb: case MEMONIC_pminsw:
        case MEMONIC_pmaxsb: case MEMONIC_pmaxsw:
        
        case MEMONIC_ptest:
        
        // op xmm, xmm/m, imm8
        case MEMONIC_palignr:
        case MEMONIC_pcmpistri:
        case MEMONIC_pcmpestri:
        case MEMONIC_pshufd: case MEMONIC_pshufhw: case MEMONIC_pshuflw: case MEMONIC_shufps: case MEMONIC_shufpd:{
            if(asm_instruction->operands[1].size == 0){
                asm_instruction->operands[1].size = 16;
            }
            asm_instruction->operands[2].size = 1;
        }break;
        
        // 'op xmm, imm' or 'op xmm, xmm/m'
        case MEMONIC_psllw: case MEMONIC_pslld: case MEMONIC_psllq: case MEMONIC_pslldq:
        case MEMONIC_psrlw: case MEMONIC_psrld: case MEMONIC_psrlq: case MEMONIC_psrldq:
        case MEMONIC_psraw: case MEMONIC_psrad:{
            if(asm_instruction->operands[1].size == 0){
                asm_instruction->operands[1].size = 16;
            }
        }break;
        
        case MEMONIC_psraq:{
            report_error(context, token, "Instruction 'psraq' does not exist, consider using 'vpsraq' instead (requires AVX512F).");
        }break;
        
        // 'op xmm, xmm/m32' or 'op xmm, xmm/m64'
        case MEMONIC_movsd: case MEMONIC_movss:{
            if(regm_index != -1 && asm_instruction->operands[regm_index].size == 0){
                u32 size = (memonic == MEMONIC_movsd) ? 8 : 4;
                asm_instruction->operands[regm_index].size = size;
            }
        }break;
        
        case MEMONIC_pextrb:{
            asm_instruction->operands[0].size = 1; // we allow eax instead of al, as it zero extends anyway
        }break;
        case MEMONIC_pextrw:{
            asm_instruction->operands[0].size = 2; // we allow eax instead of ax, as it zero extends anyway
        }break;
        case MEMONIC_pextrd:{
            if(asm_instruction->operands[0].size == 0) asm_instruction->operands[0].size = 4;
        }break;
        case MEMONIC_pextrq:{
            if(asm_instruction->operands[0].size == 0) asm_instruction->operands[0].size = 8;
        }break;
        
        case MEMONIC_pinsrb:{
            asm_instruction->operands[1].size = 1; // we allow size reg32, because this instruction is r32/m8
        }break;
        case MEMONIC_pinsrw:{
            asm_instruction->operands[1].size = 2; // we allow size reg32, because this instruction is r32/m8
        }break;
        case MEMONIC_pinsrd:{
            if(asm_instruction->operands[1].size == 0) asm_instruction->operands[1].size = 4;
        }break;
        case MEMONIC_pinsrq:{
            if(asm_instruction->operands[1].size == 0) asm_instruction->operands[1].size = 8;
        }break;
        
        
        // 'vmovups xmm/m, xmm' or 'vmovups xmm, xmm/m' 'vmovups ymm/m, ymm' or 'vmovups ymm, ymm/m'
        case MEMONIC_vmovdqu:
        case MEMONIC_vmovdqa:
        case MEMONIC_vmovups:{
            if(regm_index != -1 && asm_instruction->operands[regm_index].size == 0){
                smm other_index = (regm_index + 1) & 1;
                asm_instruction->operands[regm_index].size = asm_instruction->operands[other_index].size;
            }
            
            if(asm_instruction->operands[0].size != asm_instruction->operands[1].size){
                report_error(context, token, "Operand size mismatch. %lld vs %lld.", asm_instruction->operands[0].size, asm_instruction->operands[1].size);
            }
        }break;
        
        case MEMONIC_vpcmpeqb:
        case MEMONIC_vpshufb:
        case MEMONIC_vshufps: case MEMONIC_vblendps: case MEMONIC_vperm2f128: // 'op ymm1, ymm2, ymm3/m256, imm8' or 'op xmm1, xmm2, xmm3/m128, imm8'
        case MEMONIC_vpminub: case MEMONIC_vpxor:
        case MEMONIC_vmulps:  case MEMONIC_vaddps:{ // 'op ymm1, ymm2, ymm3/m256' or 'op xmm1, xmm2, xmm3/m128'
            if(asm_instruction->operands[2].size == 0){ // @cleanup: regm_index ? 
                u32 size = (memonic == MEMONIC_movsd) ? 8 : 4;
                asm_instruction->operands[regm_index].size = size;
            }
            
            smm size = asm_instruction->operands[0].size;
            if(size != asm_instruction->operands[1].size || size != asm_instruction->operands[2].size){
                report_error(context, token, "Operand size mismatch. %lld vs %lld vs %lld.", size, asm_instruction->operands[1].size, asm_instruction->operands[2].size);
            }
        }break;
        
        // 'vop xmm, xmm/m' or 'vop ymm, ymm/m'
        case MEMONIC_vptest:{
            if(asm_instruction->operands[1].size == 0){
                asm_instruction->operands[1].size = asm_instruction->operands[0].size;
            }
            
            if(asm_instruction->operands[0].size != asm_instruction->operands[1].size){
                report_error(context, token, "Operand size mismatch. %lld vs %lld.", asm_instruction->operands[0].size, asm_instruction->operands[1].size);
            }
        }break;
        
        case MEMONIC_crc32:{
            if(asm_instruction->operands[0].size == 0){
                asm_instruction->operands[0].size = asm_instruction->operands[1].size == 8 ? 8 : 4;
            }
            // we allow any size on the lhs.
        }break;
        
        case MEMONIC_vinsertf128:{
            if(asm_instruction->operands[2].size == 0){
                asm_instruction->operands[2].size = 16;
            }
        }break;
        
        case MEMONIC_vmovss:{ // @incomplete: this has weird forms we only support one right now
            if(regm_index != -1 && asm_instruction->operands[regm_index].size == 0){
                asm_instruction->operands[regm_index].size = 4;
            }
        }break;
        
        case MEMONIC_return_from_inline_asm_function:{
            if(!context->in_inline_asm_function){
                report_error(context, token, "Cannot use hlc-specific memonic 'return' outside of a '__declspec(inline_asm)'-function.");
            }
            
            //
            // make sure the return type matches.
            //
            struct ast_function *function = context->current_function;
            struct ast_type *return_type = function->type->return_type;
            
            if(return_type == &globals.typedef_void){
                report_error(context, token, "A function of type 'void' cannot return a value.");
            }
            
            if(asm_instruction->operands[0].size != return_type->size){
                if(get_register_kind_for_type(return_type) == REGISTER_KIND_xmm && asm_instruction->operands[0].register_kind_when_loaded){
                    //
                    // This is fine, we allow you to return any 'xmm' thing for any other.
                    //
                    
                }else{
                    report_error(context, token, "Returning a value of size %lld, but the return type is on size %lld.", asm_instruction->operands[0].size, return_type->size);
                }
            }
            
            if(!peek_token(context, TOKEN_closed_curly)){
                report_error(context, token, "The hlc-specific 'return'-memonic has to be the last memonic in the function.");
            }
        }break;
        
        case MEMONIC_none:{
            report_error(context, token, "Unknown memonic in __asm__-block.");
        }break;
        
        default: break;
    }
    
    // Make sure the instruction has its size inferred.
    assert(regm_index == -1 || asm_instruction->operands[regm_index].size != 0);
    
    if(asm_instruction){
        asm_instruction->prefixes = context->current_asm_flags;
    }
    return asm_instruction;
}


func void parse_asm_block(struct context *context, struct ir_asm_block *asm_block){
    
    struct ir_skip *ir_skip = null;
    if(!context->in_inline_asm_function){
        // @HACK: skip all the ir that is in the block.
        ir_skip = push_struct(&context->ir_arena, struct ir_skip);
        ir_skip->base.kind = IR_skip;
    }
    
    // note they should always 'ballance', thus this scope _should_ end but lets be careful
    while(!peek_token_eat(context, TOKEN_closed_curly) && !peek_token(context, TOKEN_invalid)){
        
        struct asm_instruction *asm_instruction = parse_asm_instruction(context);
        
        if(!context->should_exit_statement && asm_instruction){
            sll_push_back(asm_block->instructions, asm_instruction);
        }else{
            if(context->should_exit_statement){
                //
                // Skip all tokens till we exit the block
                //
                while(!peek_token_eat(context, TOKEN_closed_curly) && !peek_token(context, TOKEN_invalid)){
                    next_token(context);
                }
            }
            break;
        }
    }
    
    
    if(ir_skip){
        ir_skip->size_to_skip = (u32)(arena_current(&context->ir_arena) - (u8*)ir_skip);
    }
}
