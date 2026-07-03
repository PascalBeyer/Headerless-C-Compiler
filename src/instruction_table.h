
enum memonic{
    MEMONIC_none,
    
    MEMONIC_lock_prefix,
    MEMONIC_repe_prefix,
    
    MEMONIC_mov,
    MEMONIC_movzx, MEMONIC_movsx, MEMONIC_movsxd,
    MEMONIC_xchg,
    MEMONIC_bswap,
    
    
    MEMONIC_jo,
    MEMONIC_jno,
    MEMONIC_jc,
    MEMONIC_jnc,
    MEMONIC_jz,
    MEMONIC_jnz,
    MEMONIC_jbe,
    MEMONIC_jnbe,
    MEMONIC_js,
    MEMONIC_jns,
    MEMONIC_jp,
    MEMONIC_jnp,
    MEMONIC_jl,
    MEMONIC_jnl,
    MEMONIC_jle,
    MEMONIC_jnle,
    
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
    
    MEMONIC_int1,
    MEMONIC_int3,
    MEMONIC_ud2,
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
    MEMONIC_syscall,
    
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
    MEMONIC_pmuldq,
    MEMONIC_pmuludq,
    
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
    MEMONIC_vpcmpeqw,
    
    MEMONIC_vpshufb,
    MEMONIC_vshufps,
    MEMONIC_vblendps,
    MEMONIC_vperm2f128,
    MEMONIC_vinsertf128,
    
    MEMONIC_vpsrlw, MEMONIC_vpsraw, MEMONIC_vpsllw,  // vpsrlw/vpsraw/vpsllw xmm, xmm, xmm/m128 | vpsrlw/vpsraw/vpsllw xmm, xmm, imm8
    MEMONIC_vpsrld, MEMONIC_vpsrad, MEMONIC_vpslld,  // vpsrld/vpsrad/vpslld xmm, xmm, xmm/m128 | vpsrld/vpsrad/vpslld xmm, xmm, imm8
    MEMONIC_vpsrlq, MEMONIC_vpsraq, MEMONIC_vpsllq,  // vpsrlq/------/vpsllq xmm, xmm, xmm/m128 | vpsrlq/------/vpsllq xmm, xmm, imm8
    MEMONIC_vpsrldq,                MEMONIC_vpslldq, //                                         | vpsrldq/----/vpslldq xmm, xmm, imm8
    
    MEMONIC_vpbroadcastw,
    
    MEMONIC_vpmovmskb,
    
    MEMONIC_crc32,
    
    MEMONIC_return_from_inline_asm_function,
    MEMONIC_label,
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
    
    ASM_OP_KIND_label = 0x10000,
    
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
    [MEMONIC_label] = {.memonic = const_string("<label>"), .amount_of_operands = 0}, // This is just here for the system not to complain.
    
    [MEMONIC_lock_prefix] = {.memonic = const_string("lock") },
    [MEMONIC_repe_prefix] = {.memonic = const_string("repe") },
    
    [MEMONIC_xgetbv] = {.memonic = const_string("xgetbv"), .amount_of_operands = 0 },
    [MEMONIC_cpuid]  = {.memonic = const_string("cpuid"),  .amount_of_operands = 0 },
    [MEMONIC_rdtsc]  = {.memonic = const_string("rdtsc"),  .amount_of_operands = 0 },
    [MEMONIC_rdtscp] = {.memonic = const_string("rdtscp"), .amount_of_operands = 0 },
    [MEMONIC_int1]   = {.memonic = const_string("int1"),   .amount_of_operands = 0 },
    [MEMONIC_int3]   = {.memonic = const_string("int3"),   .amount_of_operands = 0 },
    [MEMONIC_ud2]   = {.memonic = const_string("ud2"),   .amount_of_operands = 0 },
    [MEMONIC_ret]    = {.memonic = const_string("ret"),    .amount_of_operands = 0 },
    [MEMONIC_syscall] = {.memonic = const_string("syscall"), .amount_of_operands = 0},
    
    [MEMONIC_movsb]  = {.memonic = const_string("movsb"), .amount_of_operands = 0 },
    [MEMONIC_movsw]  = {.memonic = const_string("movsw"), .amount_of_operands = 0 },
    [MEMONIC_movsq]  = {.memonic = const_string("movsq"), .amount_of_operands = 0 },
    [MEMONIC_stosb]  = {.memonic = const_string("stosb"), .amount_of_operands = 0 },
    [MEMONIC_stosw]  = {.memonic = const_string("stosw"), .amount_of_operands = 0 },
    [MEMONIC_stosd]  = {.memonic = const_string("stosd"), .amount_of_operands = 0 },
    [MEMONIC_stosq]  = {.memonic = const_string("stosq"), .amount_of_operands = 0 },
    
    [MEMONIC_pause]  = {.memonic = const_string("pause"), .amount_of_operands = 0},
    
    [MEMONIC_jo] = {.memonic = const_string("jo"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },
    [MEMONIC_jno] = {.memonic = const_string("jno"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },
    [MEMONIC_jc] = {.memonic = const_string("jc"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },
    [MEMONIC_jnc] = {.memonic = const_string("jnc"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },
    [MEMONIC_jz] = {.memonic = const_string("jz"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jnz] = {.memonic = const_string("jnz"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },
    [MEMONIC_jbe] = {.memonic = const_string("jbe"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jnbe] = {.memonic = const_string("jnbe"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_js] = {.memonic = const_string("js"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jns] = {.memonic = const_string("jns"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jp] = {.memonic = const_string("jp"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jnp] = {.memonic = const_string("jnp"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jl] = {.memonic = const_string("jl"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jnl] = {.memonic = const_string("jnl"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jle] = {.memonic = const_string("jle"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    [MEMONIC_jnle] = {.memonic = const_string("jnle"), .amount_of_operands = 1, .operand_kind_flags[0] = ASM_OP_KIND_label },    
    
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
    
    [MEMONIC_pmuldq] = {.memonic = const_string("pmuldq"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 },
    
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
    [MEMONIC_vpcmpeqw]= {.memonic = const_string("vpcmpeqw"), .amount_of_operands = 3, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    
    [MEMONIC_vblendps] = {.memonic = const_string("vblendps"),  .amount_of_operands = 4, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256, .operand_kind_flags[3] = ASM_OP_KIND_imm8 },
    [MEMONIC_vshufps]  = {.memonic = const_string("vshufps"),   .amount_of_operands = 4, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256, .operand_kind_flags[3] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_vperm2f128]  = {.memonic = const_string("vperm2f128"),  .amount_of_operands = 4, .operand_kind_flags[0] = ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_ymmm256, .operand_kind_flags[3] = ASM_OP_KIND_imm8 },
    
    [MEMONIC_vinsertf128] = {.memonic = const_string("vinsertf128"), .amount_of_operands = 4, .operand_kind_flags[0] = ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_ymm, .operand_kind_flags[2] = ASM_OP_KIND_xmmm128, .operand_kind_flags[3] = ASM_OP_KIND_imm8 },
    
    // @incomplete: this one is weird, it can take 2 or three operands!
    [MEMONIC_vmovss] = {.memonic = const_string("vmovss"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm32 },
    
    [MEMONIC_vptest] = {.memonic = const_string("vptest"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128 | ASM_OP_KIND_ymmm256 },
    
    [MEMONIC_vpmovmskb] = {.memonic = const_string("vpmovmskb"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_reg32 | ASM_OP_KIND_reg64, .operand_kind_flags[1] = ASM_OP_KIND_xmm | ASM_OP_KIND_ymm },
    [MEMONIC_vpbroadcastw] = {.memonic = const_string("vpbroadcastw"), .amount_of_operands = 2, .operand_kind_flags[0] = ASM_OP_KIND_ymm, .operand_kind_flags[1] = ASM_OP_KIND_xmmm128},
    
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

#define register_is_extended(reg) ((reg) & 0xF8)
enum register_encoding{
    INVALID_REGISTER = -1, // used to say that there is no index register (remove me once we are relative to emit_locations)
    REGISTER_A  = 0, // accumulatior
    REGISTER_C  = 1, // count
    REGISTER_D  = 2, // data
    
    REGISTER_B  = 3, // base pointer to data
    REGISTER_SP = 4, // stack pointer
    REGISTER_BP = 5, // stack base pointer
    REGISTER_SI = 6, // source index or pointer to data
    REGISTER_DI = 7, // destination index or pointer to data
    
    REGISTER_R8  = 8 + 0,
    REGISTER_R9  = 8 + 1,
    REGISTER_R10 = 8 + 2,
    REGISTER_R11 = 8 + 3,
    REGISTER_R12 = 8 + 4,
    REGISTER_R13 = 8 + 5,
    REGISTER_R14 = 8 + 6,
    REGISTER_R15 = 8 + 7,
    
    REGISTER_XMM0 = 0,
    REGISTER_XMM1 = 1,
    REGISTER_XMM2 = 2,
    REGISTER_XMM3 = 3,
    REGISTER_XMM4 = 4,
    REGISTER_XMM5 = 5,
    REGISTER_XMM6 = 6,
    REGISTER_XMM7 = 7,
    
    REGISTER_XMM8  = 8 + 0,
    REGISTER_XMM9  = 8 + 1,
    REGISTER_XMM10 = 8 + 2,
    REGISTER_XMM11 = 8 + 3,
    REGISTER_XMM12 = 8 + 4,
    REGISTER_XMM13 = 8 + 5,
    REGISTER_XMM14 = 8 + 6,
    REGISTER_XMM15 = 8 + 7,
    
    REGISTER_SIB_EXTENSION = REGISTER_SP,
    
    
};

