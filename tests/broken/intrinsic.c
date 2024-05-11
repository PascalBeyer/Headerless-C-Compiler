// broken

#include <intrin.h>
#include <immintrin.h>
#include <emmintrin.h>
#include <xmmintrin.h>

int _fltused = 0;

void sse(){
	   __m128 a = _mm_set_ps1(1.0f);
   __m128 b = _mm_set_ps(1.0f, 2.0f, 3.0f, 4.0f);
   int i = 3;
   __int64 i64 = 4;
   float f = 123.0f, f0 = 1.0f, f1 = 2.0f, f2 = 3.0f, f3 = 4.0f;

   float f4[4] = {1.0f, 2.0f, 3.0f, 4.0f};

   b = _mm_cvtsi32_ss (a, i);
   b = _mm_cvtsi64_ss (a, i64);
   f = _mm_cvtss_f32(a);
   i = _mm_cvtss_si32 (a);
   i64 = _mm_cvtss_si64(a);
   i = _mm_cvtt_ss2si(a);
   i = _mm_cvttss_si32(a);
   i64 = _mm_cvttss_si64 (a);
   a = _mm_load_ps(f4);
   a = _mm_load_ps1(f4);
   a = _mm_load_ss(f4);
   a = _mm_load1_ps(f4);
   a = _mm_loadr_ps(f4);

    a = _mm_move_ss(a, b);
    a = _mm_movehl_ps(a, b);
    a = _mm_movelh_ps(a, b);
    i = _mm_movemask_ps(a);

    a = _mm_or_ps (a, b);
    a = _mm_rcp_ps (a);
    a = _mm_rcp_ss (a);
    a = _mm_rsqrt_ps (a);
    a = _mm_rsqrt_ss (a);
    a = _mm_set_ps(f0, f1, f2, f3);

    a = _mm_set_ss (f);
    a = _mm_set1_ps (f);
    _mm_setcsr (i);
    a = _mm_setr_ps (f0, f1, f2, f3);
    a = _mm_setzero_ps ();
    _mm_sfence ();

    a = _mm_sqrt_ps (a);
    a = _mm_sqrt_ss (a);

    _mm_store_ps1 (f4, a);
    _mm_store_ss (f4, a);
    _mm_store1_ps (f4, a);
    _mm_storer_ps (f4, a);

    a = _mm_sub_ps (a, b);
    a = _mm_sub_ss (a, b);

    i =  _mm_ucomieq_ss (a, b);
    i =  _mm_ucomige_ss (a, b);
    i =  _mm_ucomigt_ss (a, b);
    i =  _mm_ucomile_ss (a, b);
    i =  _mm_ucomilt_ss (a, b);
    i =  _mm_ucomineq_ss (a, b);

    a = _mm_undefined_ps();
    a = _mm_xor_ps(a, b);
}

void sse2(){
	    __m128d a = {0};
    __m128d b = {0};
    __m128i m128i = {0};
    int i = 0, i1 = 1, i2 = 2, i3 = 3, i4 = 4;
    __int64 i64 = 123;

    i = _mm_comieq_sd(a, b);
    i = _mm_comige_sd(a, b);
    i = _mm_comigt_sd(a, b);
    i = _mm_comile_sd(a, b);
    i = _mm_comilt_sd(a, b);
    i = _mm_comineq_sd(a, b);

    __m128 m128;
    m128 = _mm_cvtsd_ss(m128, a);
    a = _mm_cvtsi32_sd (a, i);
    m128i = _mm_cvtsi64_si128(i64);
    a = _mm_cvtsi64x_sd (b, i64);
    m128i = _mm_cvtsi64x_si128(i64);
    a = _mm_cvtss_sd(a, m128);

    m128i = _mm_cvttpd_epi32(a);
    m128i = _mm_cvttps_epi32(m128);

    i = _mm_cvttsd_si32(a);

    i = _mm_extract_epi16(m128i, 1);

    m128i = _mm_insert_epi16(m128i, i, 1);

    _mm_lfence();

   double f4[4] = {0};
    a = _mm_load_pd (f4);
    a = _mm_load_pd1 (f4);
    a = _mm_load_sd (f4);
    m128i = _mm_load_si128 (&m128i);
    a = _mm_load1_pd (f4);
    a = _mm_loadh_pd (a, f4);
    m128i = _mm_loadl_epi64 (&m128i);
    a = _mm_loadl_pd (a, f4);
    a = _mm_loadr_pd (f4);
    a = _mm_loadu_pd (f4);
    m128i = _mm_loadu_si128 (&m128i);
    m128i = _mm_loadu_si16 (f4);
    m128i = _mm_loadu_si32 (f4);
    m128i = _mm_loadu_si64 (f4);

    _mm_maskmoveu_si128(m128i, m128i, "hello");

    i = _mm_movemask_pd(a);

    __m128i m128ia = {0}, m128ib = {0};
    m128i = _mm_mul_epu32(m128ia, m128ib);
    m128i = _mm_sad_epu8(m128ia, m128ib);

    double d1 = 1.0, d2 = 2.0; 
    a = _mm_set_pd(d1, d2);
    b = _mm_set_pd1(d1);
    a = _mm_set_sd(d2);
    b = _mm_set1_pd(d1);

    m128i = _mm_setr_epi32(i1, i2, i3, i4);

    a = _mm_setr_pd(d1, d2);

    m128i = _mm_shuffle_epi32(m128ia, 3);
    a = _mm_shuffle_pd(a, b, 3);

    m128i = _mm_sll_epi16(m128ia, m128ib);

    m128i = _mm_slli_si128 (m128ia, 3);

    m128i = _mm_move_epi64(m128ia);

    a = _mm_move_sd(a, b);

    i = _mm_ucomieq_sd (a, b);
    i = _mm_ucomige_sd (a, b);
    i = _mm_ucomigt_sd (a, b);
    i = _mm_ucomile_sd (a, b);
    i = _mm_ucomilt_sd (a, b);
    i = _mm_ucomineq_sd (a, b);

    m128i = _mm_unpackhi_epi64(m128ia, m128ib);

    _mm_store_pd(f4, a);
    _mm_store_pd1(f4, a);
    _mm_store_sd(f4, a);
    _mm_store1_pd(f4, a);

    _mm_storeh_pd(f4, a);
    _mm_storel_epi64(&m128i, m128ia);
    _mm_storel_pd(f4, a);
    _mm_storer_pd(f4, a);
    _mm_storeu_pd(f4, a);

    _mm_storeu_si16(f4, m128i);
    _mm_storeu_si32(f4, m128i);
    _mm_storeu_si64(f4, m128i);
    
    _mm_stream_pd(f4, a);
    _mm_stream_si128(&m128i, m128ia);
    _mm_stream_si32(&i, i1);
    _mm_stream_si64(&i64, i64);

    a = _mm_sqrt_sd(a, b);
}


int main(){
    
    
    {   // microsoft
        unsigned char carry_in = 1;
        unsigned short a = 1337;
        unsigned short b = 666;
        unsigned short out;
        unsigned char carry_out = _addcarry_u16(carry_in, a, b, &out);
        
        __addgsqword(out, 11);
        
        unsigned long ulong;
        long _long = 123;
        
        __int64 int64 = 123;
        
        unsigned char bsf = _BitScanForward(&ulong, 123);
        
        bsf = _BitScanForward64(&ulong, 123);
        
        unsigned char bsr = _BitScanReverse(&ulong, 123);
        bsr = _BitScanReverse64(&ulong, 123);
        unsigned char bt = _bittest(&_long, _long);
        bt = _bittest64(&int64, int64);
        
        unsigned char btac = _bittestandcomplement(&_long, _long);
        btac = _bittestandcomplement64(&int64, int64);
        
        _long = _InterlockedCompareExchange(&_long, _long, _long);
        
        // _InterlockedAdd(&_long, _long);
    }
    
    // general support
    // void _mm_clflush (void const* p);
    // void _mm_lfence (void);
    // void _mm_mfence (void);
    
    {
        // load
        double _d[2] = {1.0, 2.0};
        float _f[4] = {1.0f, 2.0f, 3.0f, 4.0f};
        __m128d d;
        __m128  f;
        __m128i i;
        
        __m64 m[2] = {{1}, {2}};
        
        i = _mm_loadu_si128 (&i);      // F3 0F 6F  movdqu
        _mm_stream_ps(_f, f);          //    0F 2B  movntps
        (void)_mm_storeu_si128(&i, i); // F3 0F 7F  movdu
        
        d = _mm_load_pd     (_d); // simple load of the two doubles into the result
        //   movups xmm0, xmmword[_d]
        
        d = _mm_loadr_pd    (_d); // load the two doubles in reverse order
        //   movups xmm0, xmmword[_d]
        //   shufpd  xmm0, xmm0, 1
        
        d = _mm_loadu_pd    (_d); // load unaligned.
        //   movups xmm0, xmmword[_d]
        
        f = _mm_load_ps  (_f); // load
        //   movups xmm0, xmmword[_f]
        
        f = _mm_loadr_ps (_f); // load in reverse
        //   movups xmm0, xmmword[_f]
        //   shufps xmm0, xmm0, 0x1b
        
        f = _mm_loadu_ps (_f); // load unaligned
        //   movups xmm0, xmmword[_f]
        
        
        d = _mm_load_sd (_d); // load only the first into the upper element
        //   movsd xmm0,  qword[_d]
        //   zero other element?
        
        d = _mm_load1_pd  (_d); // load one double into both slots of the result
        //   movsd  xmm0, qword ptr[_d]
        //   shufpd xmm0, xmm0, 0
        
        f = _mm_load_ps1 (_f); // load into both
        //   movss  xmm0, dword[_f]
        //   shufps xmm0, xmm0, 0
        
        f = _mm_load1_ps (_f); // same as the one above
        //   movss  xmm0, dword[_f]
        //   shufps xmm0, xmm0, 0
        
        f = _mm_load_ss  (_f); // load just one into the upper 
        //   movss xmm0, dword[_f]
        //   zero other element?
        
        
        f = _mm_loadh_pi (f, m); // load the high, copy the low
        f = _mm_loadl_pi (f, m); // load the low, copy the high
        
        i = _mm_loadl_epi64 (&i); // load 64-bit integer from memory into the first element of the result.
        d = _mm_loadl_pd (d, _d); // load lower copy upper.
        d = _mm_loadh_pd (d, _d); // load upper copy lower.
        
        
        i = _mm_load_si128  (&i); // load 128-bits of integer data from memory.
        //   movdqa xmm0, xmmword[&i]
        
        
        //int _i[4] = {1, 2, 3, 4};
        //i = _mm_loadu_si16 (_i); // undefined
        //i = _mm_loadu_si64 (_i); // undefined
        //d = _mm_load_pd1   (_d); // undefined
        //i = _mm_loadu_si32 (_i); // undefined
    }
    
    {
        __m128  f;
        __m128d d;
        __m128i i;
        
        f = _mm_setzero_ps();    //    0F 57  xorps
        f = _mm_set_ss(1337.0f); // F3 0F 10  movss
        f = _mm_set_ps(1337.0f, 1337.0f, 1337.0f, 1337.0f);
        f = _mm_set1_ps(1337.0f);
        f = _mm_set_ps1(1.0f); // wait, why is this defined???
        
        float f1 = 0.0, f2 = 0.123, f3 = 0.1337, f4 = 1337.0;
        f = _mm_set_ss(f2);
        f = _mm_set_ps(f1, f2, f3, f4); // just do them one by one
        f = _mm_set1_ps(f2); // movss shufps, 0
        f = _mm_set_ps1(f2);
        
        d = _mm_setzero_pd();
        d = _mm_set_sd(1337.0);
        d = _mm_set_pd(1337.0, 1337.0);
        d = _mm_set1_pd(1.0);
        //d = _mm_set_pd1(1.0); // not defined
        
        f = _mm_setr_ps(0.1f, 0.2f, 0.3f, 0.4f);
        d = _mm_setr_pd(1.0, 2.0);
        
        double d1 = 0.0, d2 = 1.3;
        d = _mm_set_sd(d1);
        d = _mm_set_pd(d1, d2);        
        
        
        
        //volatile float asd = 123;
        //f = _mm_set_ps(asd, asd + 1.f, asd + 2.f, asd + 3.f);
        
        i = _mm_setzero_si128();
        
        i = _mm_set_epi8 (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
        i = _mm_set_epi16(0, 1, 2, 3, 4, 5, 6, 7);
        i = _mm_set_epi32(0, 1, 2, 3);
        //i = _mm_set_epi64(0, 1); // not defined
        i = _mm_set_epi64x(0, 1);

        char i0 = 0, i1 = 1, i2 = 2, i3 = 3, i4 = 4, i5 = 5, i6 = 6, i7 = 7, i8 = 8, i9 = 9, i10 = 10,
        i11 = 11, i12 = 12, i13 = 13, i14 = 14, i15 = 15;
        
        i = _mm_set_epi8 (i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15);
        i = _mm_set_epi16(i0, i1, i2, i3, i4, i5, i6, i7);
        i = _mm_set_epi32(i0, i1, i2, i3);
        //i = _mm_set_epi64(i0, i1); // not defined
        i = _mm_set_epi64x(i0, i1);
        
        
        i = _mm_setr_epi8(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
        i = _mm_setr_epi16(0, 1, 2, 3, 4, 5, 6, 7);
        i = _mm_setr_epi32(0, 1, 2, 3);
        //i = _mm_setr_epi64(0, 1); // not defined
        i = _mm_setr_epi64x(0, 1);
        
        i = _mm_set1_epi8  (1);
        i = _mm_set1_epi16 (1);
        i = _mm_set1_epi32 (1);
        //i = _mm_set1_epi64 (1); // not defined
        i = _mm_set1_epi64x(1);
        
        //_mm_cvtpd_pi32;  // not defined
        //_mm_cvttpd_pi32; // not defined
        //_mm_cvtpi32_pd;  // not defined
        
        int _i = 0;
        __int64 i64 = 0;
        double _d = 0.0;
        
        
        
        d   = _mm_cvtsi32_sd  (d, 0);   // F2    0F 2A cvtsi2sd
        d   = _mm_cvtsi64_sd  (d, i64); // F2 48 0F 2A cvtsi2sd
        d   = _mm_cvtsi64x_sd (d, i64); // F2 48 0F 2A cvtsi2sd
        
        _i  = _mm_cvttsd_si32    (d); // F2    0F 2C  cvttsd2si
        i64 = _mm_cvttsd_si64    (d); // F2 48 0F 2C  cvttsd2si
        i64 = _mm_cvttsd_si64x   (d); // F2 48 0F 2C  cvttsd2si
        
        _d  = _mm_cvtsd_f64      (d); // F2    0F 2D  cvtsd2si
        _i  = _mm_cvtsd_si32     (d); // F2 48 0F 2D  cvtsd2si
        i64 = _mm_cvtsd_si64     (d); // F2 48 0F 2D  cvtsd2si
        
        d   = _mm_cvtps_pd       (f); //       0F 5A  cvtps2pd
        f   = _mm_cvtpd_ps       (d); // 66    0F 5A  cvtpd2ps
        f   = _mm_cvtsd_ss    (f, d); // F2    0F 5A  cvtsd2ss
        d   = _mm_cvtss_sd    (d, f); // F3    0F 5A  cvtss2sd
        
        f   = _mm_cvtepi32_ps    (i); //       0F 5B  cvtdq2ps
        i   = _mm_cvtps_epi32    (f); // 66    0F 5B  cvtps2dq
        i   = _mm_cvttps_epi32   (f); // F3    0F 5B  cvttps2dq
        
        d   = _mm_cvtepi32_pd    (i); // F3    0F E6  cvtdq2pd
        i   = _mm_cvtpd_epi32    (d); // F2    0F E6  cvtpd2dq
        i   = _mm_cvttpd_epi32   (d); // 66    0F E6  cvttpd2dq
        
        i64 = _mm_cvtsd_si64x    (d);   // 66 0F 6F  movdqa
        _i  = _mm_cvtsi128_si32  (i);   // 66 0F 6F  movdqa
        i64 = _mm_cvtsi128_si64  (i);   // 66 0F 6F  movdqa
        i64 = _mm_cvtsi128_si64x (i);   // 66 0F 6F  movdqa
        i   = _mm_cvtsi32_si128  (1);   // mov eax, movq 1, xmm0, eax, movdqa
        i   = _mm_cvtsi64_si128  (i64); // mov xmm0, rax, movdqa
        i   = _mm_cvtsi64x_si128 (i64);
        
        f = _mm_castpd_ps    (d); // 0f 28  movaps
        i = _mm_castpd_si128 (d); // movdqa
        d = _mm_castps_pd    (f); // movaps
        i = _mm_castps_si128 (f); // movdqa
        d = _mm_castsi128_pd (i); // movaps
        f = _mm_castsi128_ps (i); // movaps
        
        
        _i = _mm_movemask_epi8 (i); // 66 0F D7  pmovmskb
        _i = _mm_movemask_pd   (d); // 66 0F 50  movmskpd
        _i = _mm_movemask_ps   (f); // 0F 50     movmskps
        
        //__m64 m = 0;
        //_i = _mm_movemask_pi8  (m); // undefined
        
        // all undefined
        //_m_pmovmskb;
        //_m_sad_epu8;
        //_m_psadbw;
        //_mm_sad_pu8;
        //_mm_movepi64_pi64;
        
        //i = _mm_move_epi64(i); // defined but whatevs
        //i = _mm_movpi64_epi64 (m); // undefined
        
    }
    
    {   // __m128 _mm_op_ss(__m128 a, __m128 b);
        __m128 a = _mm_set_ss(0.0f), b = _mm_set_ss(0.0f), c;
        
        // SSE
        c = _mm_add_ss      (a, b); // F3 0F 58  addss
        c = _mm_div_ss      (a, b); // F3 0F 5E  divss
        c = _mm_max_ss      (a, b); // F3 0F 5F  maxss
        c = _mm_min_ss      (a, b); // F3 0F 5D  minss
        c = _mm_move_ss     (a, b); // F3 0F 10  movss
        c = _mm_mul_ss      (a, b); // F3 0F 59  mulss
        c = _mm_sub_ss      (a, b); // F3 0F 5C  subss
        
        c = _mm_cmpeq_ss    (a, b); // F3 0F C2  cmpeqss
        c = _mm_cmpge_ss    (a, b); // F3 0F C2  cmpless ...
        c = _mm_cmpgt_ss    (a, b); // F3 0F C2  cmpltss ...
        c = _mm_cmple_ss    (a, b); // F3 0F C2  cmpless
        c = _mm_cmplt_ss    (a, b); // F3 0F C2  cmpltss
        c = _mm_cmpneq_ss   (a, b); // F3 0F C2  cmpneqss
        c = _mm_cmpnge_ss   (a, b); // F3 0F C2  cmpnless
        c = _mm_cmpnle_ss   (a, b); // F3 0F C2  cmpnless
        c = _mm_cmpnlt_ss   (a, b); // F3 0F C2  cmpnltss
        c = _mm_cmpord_ss   (a, b); // F3 0F C2  cmpordss
        c = _mm_cmpunord_ss (a, b); // F3 0F C2  cmpunords
        
        
        int i; // these are fucking horrible, they just do commis and then load that.
        i = _mm_comieq_ss   (a, b); // comiss, sete, setnp, and, movzx
        i = _mm_comige_ss   (a, b);
        i = _mm_comigt_ss   (a, b);
        i = _mm_comile_ss   (a, b);
        i = _mm_comilt_ss   (a, b);
        i = _mm_comineq_ss  (a, b);
        i = _mm_ucomieq_ss  (a, b);
        i = _mm_ucomige_ss  (a, b);
        i = _mm_ucomigt_ss  (a, b);
        i = _mm_ucomile_ss  (a, b);
        i = _mm_ucomilt_ss  (a, b);
        i = _mm_ucomineq_ss (a, b);
        
        // SSE4.1
        c = _mm_ceil_ss     (a, b); // 66 0F 3A  roundss,2
        c = _mm_floor_ss    (a, b); // 66 0F 3A  roundss,1
        
    }
    
    {   // __m128 _mm_op_ps(__m128 a, __m128 b);
        __m128 a = _mm_set_ss(0.0f), b = _mm_set_ss(0.0f), c;
        
        // SSE
        c = _mm_movehl_ps   (a, b); // 0F 12  movhlps
        c = _mm_unpacklo_ps (a, b); // 0F 14  unpcklps
        c = _mm_unpackhi_ps (a, b); // 0F 15  unpckhps
        c = _mm_movelh_ps   (a, b); // 0F 16  movlhps
        c = _mm_and_ps      (a, b); // 0F 54  andps
        c = _mm_andnot_ps   (a, b); // 0F 55  andnps
        c = _mm_or_ps       (a, b); // 0F 56  orps
        c = _mm_xor_ps      (a, b); // 0F 57  xorps
        c = _mm_add_ps      (a, b); // 0F 58  addps
        c = _mm_mul_ps      (a, b); // 0F 59  mulps
        c = _mm_sub_ps      (a, b); // 0F 5C  subp
        c = _mm_min_ps      (a, b); // 0F 5D  minps
        c = _mm_div_ps      (a, b); // 0F 5E  divps
        c = _mm_max_ps      (a, b); // 0F 5F  maxps

        //c = _mm_move_ps     (a, b); // undefined

        c = _mm_cmpeq_ps    (a, b); // 0F C2  cmpeqps
        c = _mm_cmpge_ps    (a, b); // 0F C2  cmpleps
        c = _mm_cmpgt_ps    (a, b); // 0F C2  cmpltps
        c = _mm_cmple_ps    (a, b); // 0F C2  cmpleps
        c = _mm_cmplt_ps    (a, b); // 0F C2  cmpltps
        c = _mm_cmpneq_ps   (a, b); // 0F C2  cmpneqps
        c = _mm_cmpnge_ps   (a, b); // 0F C2  cmpnleps
        c = _mm_cmpngt_ps   (a, b); // 0F C2  cmpnltps
        c = _mm_cmpnle_ps   (a, b); // 0F C2  cmpnleps
        c = _mm_cmpnlt_ps   (a, b); // 0F C2  cmpnltps
        c = _mm_cmpord_ps   (a, b); // 0F C2  cmpordps
        c = _mm_cmpunord_ps (a, b); // 0F C2  cmpunordps
        
        
        c = _mm_shuffle_ps(a, b, 0);
        
        // SSE3
        c = _mm_addsub_ps (a, b); // F2 0F D0  addsubps
        c = _mm_hadd_ps   (a, b); // F2 0F 7C  haddps
        c = _mm_hsub_ps   (a, b); // F2 0F 7D  hsubps
        
    }
    
    {   // __m128d _mm_op_sd(__m128d a, __m128d b);
        __m128d a = _mm_set_sd(0.0), b = _mm_set_sd(0.0), c;
        
        // SSE2
        c = _mm_add_sd      (a, b); // F2 0F 58  addsd
        c = _mm_move_sd     (a, b); // F2 0F 10  movsd
        c = _mm_sqrt_sd     (a, b); // F2 0F 51  sqrtsd
        c = _mm_div_sd      (a, b); // F2 0F 5E  divsd
        c = _mm_min_sd      (a, b); // F2 0F 5D  minsd
        c = _mm_max_sd      (a, b); // F2 0F 5F  maxsd
        c = _mm_mul_sd      (a, b); // F2 0F 59  mulsd
        c = _mm_sub_sd      (a, b); // F2 0F 5C  subsd
        
        c = _mm_cmpeq_sd    (a, b); // F2 0F C2 ... 00  cmpeqsd
        c = _mm_cmple_sd    (a, b); // F2 0F C2 ... 01  cmplesd
        c = _mm_cmplt_sd    (a, b); // F2 0F C2 ... 02  cmpltsd
        c = _mm_cmpunord_sd (a, b); // F2 0F C2 ... 03  cmpunordsd
        c = _mm_cmpneq_sd   (a, b); // F2 0F C2 ... 04  cmpneqsd
        c = _mm_cmpnle_sd   (a, b); // F2 0F C2 ... 05  cmpnlesd
        c = _mm_cmpnlt_sd   (a, b); // F2 0F C2 ... 06  cmpnltsd
        c = _mm_cmpord_sd   (a, b); // F2 0F C2 ... 07  cmpordsd
        
        // these just desugar into the above
        c = _mm_cmpgt_sd    (a, b); // F2 0F C2 ... 01  cmpltsd
        c = _mm_cmpge_sd    (a, b); // F2 0F C2 ... 02  cmplesd
        c = _mm_cmpngt_sd   (a, b); // F2 0F C2 ... 05  cmpnltsd
        c = _mm_cmpnge_sd   (a, b); // F2 0F C2 ... 06  cmpnlesd
        
        int i; // see the ss case
        i = _mm_comieq_sd   (a, b);
        i = _mm_comige_sd   (a, b);
        i = _mm_comigt_sd   (a, b);
        i = _mm_comile_sd   (a, b);
        i = _mm_comilt_sd   (a, b);
        i = _mm_comineq_sd  (a, b);
        i = _mm_ucomieq_sd  (a, b);
        i = _mm_ucomige_sd  (a, b);
        i = _mm_ucomigt_sd  (a, b);
        i = _mm_ucomile_sd  (a, b);
        i = _mm_ucomilt_sd  (a, b);
        i = _mm_ucomineq_sd (a, b);
        
        // SSE4.1
        c = _mm_ceil_sd  (a, b);  // roundsd, 2
        c = _mm_floor_sd (a, b);  // roundsd, 1
        
    }
    
    {   // __m128d _mm_op_pd(__m128d a, __m128d b);
        __m128d a = _mm_set_sd(0.0), b = _mm_set_sd(0.0), c;
        
        // SSE2:
        c = _mm_add_pd      (a, b); // 66 0F 58  addpd
        c = _mm_sub_pd      (a, b); // 66 0F 5C  subpd
        c = _mm_mul_pd      (a, b); // 66 0F 59  mulpd
        c = _mm_div_pd      (a, b); // 66 0F 5E  divpd
        c = _mm_min_pd      (a, b); // 66 0F 5D  minpd
        c = _mm_max_pd      (a, b); // 66 0F 5F  maxpd
        c = _mm_andnot_pd   (a, b); // 66 0F 55  andnpd
        c = _mm_unpackhi_pd (a, b); // 66 0F 15  unpckhpd
        c = _mm_unpacklo_pd (a, b); // 66 0F 14  unpcklpd
        c = _mm_and_pd      (a, b); // 0F 54 (and_ps)
        c = _mm_or_pd       (a, b); // 0F 56 (or_ps)
        c = _mm_xor_pd      (a, b); // 0F 57
        
        //c = _mm_move_pd(a, b); // undefined
        
        c = _mm_cmpeq_pd    (a, b); // 66 0F C2, other see above.
        c = _mm_cmpge_pd    (a, b);
        c = _mm_cmpgt_pd    (a, b);
        c = _mm_cmple_pd    (a, b);
        c = _mm_cmplt_pd    (a, b);
        c = _mm_cmpneq_pd   (a, b);
        c = _mm_cmpnge_pd   (a, b);
        c = _mm_cmpngt_pd   (a, b);
        c = _mm_cmpnle_pd   (a, b);
        c = _mm_cmpnlt_pd   (a, b);
        c = _mm_cmpord_pd   (a, b);
        c = _mm_cmpunord_pd (a, b);
        
        c = _mm_shuffle_pd(a, b, 0);
        
        // SSE3
        c = _mm_addsub_pd(a, b); // 66 0F D0  addsubpd
        c = _mm_hadd_pd(a, b);   // 66 0F 7C  haddpd
        c = _mm_hsub_pd(a, b);   // 66 0F 7D  hsubpd
        
    }
    
    
    {   // __m128i _mm_op_epi8(__m128i a, __m128i b);
        __m128i a = _mm_set1_epi64x(0), b = _mm_set1_epi64x(0), c;
        
        // SSE2
        c = _mm_unpacklo_epi8 (a, b); // 66 0F 60  punpcklbw
        c = _mm_unpackhi_epi8 (a, b); // 66 0F 68  punpckhbw
        c = _mm_subs_epi8     (a, b); // 66 0F E8  psubsb
        c = _mm_adds_epi8     (a, b); // 66 0F EC  paddsb
        c = _mm_sub_epi8      (a, b); // 66 0F F8  psubb
        c = _mm_add_epi8      (a, b); // 66 0F FC  paddb
        c = _mm_cmpeq_epi8    (a, b); // 66 0F 74  pcmpeqb
        
        c = _mm_cmpgt_epi8    (a, b); // 66 0F 64  pcmpgtb
        c = _mm_cmplt_epi8    (a, b); // 66 0F 64  pcmpgtb
        
        c = _mm_adds_epu8     (a, b); // 66 0f dc  paddusb
        c = _mm_subs_epu8     (a, b); // 66 0f d8  psubusb
        c = _mm_avg_epu8      (a, b); // 66 0f e0  pavgb
        c = _mm_max_epu8      (a, b); // 66 0f de  pmaxub
        c = _mm_min_epu8      (a, b); // 66 0f da  pminub
        c = _mm_sad_epu8      (a, b); // 66 0f f6  psadbw?
        
        // SSSE3
        c = _mm_shuffle_epi8  (a, b); // 66 0F 38 00  pshufb
        c = _mm_sign_epi8     (a, b); // 66 0F 38 08  psignb
        
        // SSE4.1
        c = _mm_min_epi8      (a, b); // 66 0F 38 38  pminsb
        c = _mm_max_epi8      (a, b); // 66 0F 38 3C  pmaxsb
    }
    
    {   // __m128i _mm_op_epi16(__m128i a, __m128i b);
        __m128i a = _mm_set1_epi64x(0), b = _mm_set1_epi64x(0), c;
        
        // SSE2
        c = _mm_unpacklo_epi16 (a, b); // 66 0F 61  punpcklwd
        c = _mm_packs_epi16    (a, b); // 66 0F 63  packsswb
        c = _mm_packus_epi16   (a, b); // 66 0F 67  packuswb
        c = _mm_unpackhi_epi16 (a, b); // 66 0F 69  punpckhwd
        c = _mm_mullo_epi16    (a, b); // 66 0F D5  pmullw
        c = _mm_mulhi_epi16    (a, b); // 66 0F E5  pmulhw
        c = _mm_subs_epi16     (a, b); // 66 0F E9  psubsw
        c = _mm_min_epi16      (a, b); // 66 0F EA  pminsw
        c = _mm_adds_epi16     (a, b); // 66 0F ED  paddsw
        c = _mm_max_epi16      (a, b); // 66 0F EE  pmaxsw
        c = _mm_madd_epi16     (a, b); // 66 0F F5  pmaddwd
        c = _mm_sub_epi16      (a, b); // 66 0F F9  psubw
        c = _mm_add_epi16      (a, b); // 66 0F FD  paddw
        c = _mm_cmpeq_epi16    (a, b); // 66 0F 75  pcmpeqw
        c = _mm_cmpgt_epi16    (a, b); // 66 0F 65  pcmpgtw
        c = _mm_cmplt_epi16    (a, b); // 66 0F 65  pcmpgtw ... switches the order
        c = _mm_sll_epi16      (a, b); // 66 0F F1  psllw
        c = _mm_sra_epi16      (a, b); // 66 0F E1  psraw
        c = _mm_srl_epi16      (a, b); // 66 0F D1  psrlw
        
        c = _mm_adds_epu16     (a, b); // 66 0f dd  paddusw
        c = _mm_avg_epu16      (a, b); // 66 0F E3  pavgw
        c = _mm_mulhi_epu16    (a, b); // 66 0F E4  pmulhuw
        c = _mm_subs_epu16     (a, b); // 66 0F D9  psubusw
        
        
        // last needs to be a constant expression
        int imm8 = _mm_extract_epi16 (a, 3); // 66 0F C5 ... 03  pextrw
        c = _mm_insert_epi16 (a, 1337, 3);   // 66 0F C4 ... 03  pinsrw
        
        
        c = _mm_srli_epi16 (a, imm8); // 66 0F D1  psrlw
        c = _mm_srai_epi16 (a, imm8); // 66 0F E1  psraw
        c = _mm_slli_epi16 (a, imm8); // 66 0F F1  psllw
        c = _mm_shufflehi_epi16(a, 0); // F3 0F 70
        c = _mm_shufflelo_epi16(a, 1); // F2 0F 70
        
        
        // SSSE3
        c = _mm_hadd_epi16     (a, b); // 66 0F 38 01  phaddw
        c = _mm_hadds_epi16    (a, b); // 66 0F 38 03  phaddsw
        c = _mm_maddubs_epi16  (a, b); // 66 0F 38 04  pmaddubsw
        c = _mm_hsub_epi16     (a, b); // 66 0F 38 05  phsubw
        c = _mm_hsubs_epi16    (a, b); // 66 0F 38 07  phsubsw
        c = _mm_sign_epi16     (a, b); // 66 0F 38 09  psignw
        c = _mm_mulhrs_epi16   (a, b); // 66 0F 38 0B  pmulhrsw
    }
    
    {   // __m128i _mm_op_epi32(__m128i a, __m128i b);
        __m128i a = _mm_set1_epi64x(0), b = _mm_set1_epi64x(0), c;
        
        // SSE2
        c = _mm_unpacklo_epi32 (a, b); // 66 0F 62  punpckldq
        c = _mm_unpackhi_epi32 (a, b); // 66 0F 6A  punpckhdq
        c = _mm_packs_epi32    (a, b); // 66 0F 6B  packssdw
        c = _mm_sub_epi32      (a, b); // 66 0F FA  psubd
        c = _mm_add_epi32      (a, b); // 66 0F FE  paddd
        c = _mm_cmpeq_epi32    (a, b); // 66 0F 76  pcmpeqd
        c = _mm_cmpgt_epi32    (a, b); // 66 0F 66  pcmpgtd
        c = _mm_cmplt_epi32    (a, b); // 66 0F 66  pcmpgtd
        c = _mm_sll_epi32      (a, b); // 66 0F F2  pslld
        c = _mm_sra_epi32      (a, b); // 66 0F E2  psrad
        c = _mm_srl_epi32      (a, b); // 66 0F D2  psrld
        
        int imm8 = 3;
        c = _mm_slli_epi32 (a, imm8); // 66 0F F2  pslld
        c = _mm_srai_epi32 (a, imm8); // 66 0F E2  psrad
        c = _mm_srli_epi32 (a, imm8); // 66 0F D2  psrld
        
        // SSSE3
        c = _mm_hadd_epi32     (a, b); // 66 0F 38 02  phaddd
        c = _mm_hsub_epi32     (a, b); // 66 0F 38 06  phsubd
        c = _mm_sign_epi32     (a, b); // 66 0F 38 0A  psignd
        
        // SSE4.1
        c = _mm_max_epi32      (a, b); // 66 0F 38 3D  pmaxsd
        c = _mm_min_epi32      (a, b); // 66 0F 38 39  pminsd
        c = _mm_mul_epi32      (a, b); // 66 0F 38 28  pmuldq
        c = _mm_mullo_epi32    (a, b); // 66 0F 38 40  pmulld
        c = _mm_packus_epi32   (a, b); // 66 0F 38 2B  packusdw
        
    }
    
    {   // __m128i _mm_op_epi64(__m128i a, __m128i b);
        __m128i a = _mm_set1_epi64x(0), b = _mm_set1_epi64x(0), c;
        
        // SSE2:
        c = _mm_add_epi64      (a, b); // 66 0F D4  paddq
        c = _mm_sll_epi64      (a, b); // 66 0F F3  psllq
        c = _mm_srl_epi64      (a, b); // 66 0F D3  psrlq
        c = _mm_sub_epi64      (a, b); // 66 0F FB  psubq
        c = _mm_unpackhi_epi64 (a, b); // 66 0F 6D  punpckhqdq
        c = _mm_unpacklo_epi64 (a, b); // 66 0F 6C  punpcklqdq
        
        int imm8 = 3;
        c = _mm_srli_epi64 (a, imm8); // 66 0F D3  psrlq
        c = _mm_slli_epi64 (a, imm8); // 66 0F F3  psllq
        //c = _mm_srai_epi64 (a, imm8); // undefined
        
        
        
        // SSE4.1
        c = _mm_cmpeq_epi64    (a, b); // 66 0F 38 29  pcmpeqq
        
        // SSE4.2
        c = _mm_cmpgt_epi64    (a, b); // 66 0F 38 37  pcmpgtq
    }
    
    {   // __m128i _mm_op_si128(__m128i a, __m128i b);
        __m128i a = _mm_set1_epi64x(0), b = _mm_set1_epi64x(0), c;
        
        // SSE2:
        c = _mm_and_si128    (a, b); // 66 0F DB  pand
        c = _mm_andnot_si128 (a, b); // 66 0F DF  pandn
        c = _mm_or_si128     (a, b); // 66 0F EB  por
        c = _mm_xor_si128    (a, b); // 66 0F EF  pxor
        
        //int imm8 = 3;
        // all undefined apperantly
        //c = _mm_slli_epi128 (a, imm8); 
        //c = _mm_srai_epi128 (a, imm8);
        //c = _mm_srli_epi128 (a, imm8);
        //c = _mm_bslli_si128 (a, imm8);
        //c = _mm_bsrli_si128 (a, imm8);
        _mm_maskmoveu_si128 (a, a, (char *)&a);
    }
    
    
    return 0;
}

