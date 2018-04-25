;original ideas: https://arxiv.org/pdf/1611.07612.pdf
;AVX2 Harley-Seal: https://github.com/WojciechMula/sse-popcount/blob/master/popcnt-avx2-harley-seal.cpp
;AVX512 Harley-Seal: https://github.com/WojciechMula/sse-popcount/blob/master/popcnt-avx512-harley-seal.cpp


.data
			  ;07                 0f                 17                 1f                 27                 2f                 37                 3f
lookup_7b_L	dq 0302020102010100h, 0403030203020201h, 0403030203020201h, 0504040304030302h, 0403030203020201h, 0504040304030302h, 0504040304030302h, 0605050405040403h
			  ;47                 4f                 57                 5f                 67                 6f                 77                 7f
lookup_7b_H	dq 0403030203020201h, 0504040304030302h, 0504040304030302h, 0605050405040403h, 0504040304030302h, 0605050405040403h, 0605050405040403h, 0706060506050504h

_0fmask		dq 0f0f0f0f0f0f0f0fh, 0f0f0f0f0f0f0f0fh, 0f0f0f0f0f0f0f0fh, 0f0f0f0f0f0f0f0fh
lookup		dq 0302020102010100h, 0403030203020201h

xmmTemp		xmmword ?, ?, ?, ?, ?, ?, ?, ?, ?, ?
ktemp		dq ?, ?, ? ,?

_5mask		dq 5555555555555555h
_3mask		dq 3333333333333333h
_ffmask		dq 00000000000000ffh

IACA_START_MARKER macro
		mov ebx, 111
		db 64h, 67h, 90h
endm

IACA_END_MARKER macro
		mov ebx, 222
		db 64h, 67h, 90h
endm

TRACE_START macro 
		;db 66h, 66h, 66h, 66h, 66h, 66h, 66h, 0Fh, 1Fh, 84h, 00h, 00h, 00h, 00h, 00h
endm

TRACE_STOP macro
		;db 66h, 66h, 66h, 66h, 66h, 66h, 66h, 0Fh, 1Fh, 84h, 00h, 00h, 00h, 00h, 01h
endm 

.code

; based on Intel Architecture Instruction Set Extensions Programming Reference
; 319433_015.pdf, p.23
CheckISA proc
; in   - void
; out  - eax[0] - RDTSC
;	   - eax[1] - RDTSCP
;	   - eax[2] - POPCNT
;	   - eax[3] - RDRAND
;	   - eax[4] - AVX2 & OS
;	   - eax[5] - AVX512F & OS
;	   - eax[6] - AVX512BW & AVX512F & OS
;	   - eax[7] - AVX512VL & AVX512F & OS
;	   - eax[8] - AVX512VBMI & AVX512F & OS
;	   - eax[9] - VPOPCNT & AVX512F & OS
;	   - eax[10]- BITALG & AVX512F & OS
; used - 
;	   - rax, rcx, rdx, rbx, r8, r9, r10, r11
		push			rbx
		push			rdx
		mov				eax, 1
		cpuid
		mov				r8d, ecx
		mov				r9d, edx
		xor				eax, eax
		bt  			ecx, 27					; check OSXSAVE flag
		jnc				no_osxsave

		mov				eax, 80000001h
		xor				ecx, ecx
		cpuid									; edx[27]: RDTSCP
		mov				r11d, edx

		mov				eax, 7
		xor				ecx, ecx
		cpuid									; ebx[30]: AVX512BW ebx[16]: AVX512F ebx[5]: AVX2
		mov				r10d, ecx				; ecx[14]: VPOPCNTDQ ecx[12]: BITALG

		xor				ecx, ecx				; specify 0 for XFEATURE_ENABLED_MASK register
		xgetbv									; result in EDX:EAX
		; edx, ecx free
		xor				ecx, ecx
		xor				edx, edx
		and				eax, 0e6h
		cmp				eax, 0e6h				; verify that XCR0[7:5] = ‘111b’ (OPMASK state, upper 256-bit of ZMM0-ZMM15 and 
												; ZMM16-ZMM31 state are enabled by OS) and that XCR0[2:1] = ‘11b’ (XMM state and YMM state are enabled by OS)
		sete			dl
		and				eax, 06h				; XCR0[2:1] = ‘11b’ (XMM state and YMM state are enabled by OS)
		cmp				eax, 06h
		sete			cl
		xor				eax, eax
		bt				r10d, 12				; check BITALG
		rcl				eax, 1
		bt				r10d, 14				; check VPOPCNTDQ
		rcl				eax, 1
		bt				r10d, 1					; check AVX512VBMI
		rcl				eax, 1
		bt				ebx, 31					; check AVX512VL
		rcl				eax, 1
		bt				ebx, 30					; check AVX512BW
		rcl				eax, 1

		xor				r10, r10
		bt				ebx, 16					; check AVX512F
		setc			r10b
		rcl				eax, 1

		and				edx, r10d
		imul			eax, edx				; clear w/o OS support

		bt				ebx, 5					; check AVX2 flag
		rcl				eax, 1
		imul			eax, ecx				; clear w/o OS support
no_osxsave:
		bt				r8d, 30					; check RDRAND flag
		rcl				eax, 1
		bt				r8d, 23					; check HW POPCNT flag
		rcl				eax, 1
		bt				r11d, 27				; check RDTSCP flag
		rcl				eax, 1
		bt				r9d, 1					; check RDTSC flag
		rcl				eax, 1

		pop				rdx
		pop				rbx
		ret
CheckISA endp

head macro savexmm, timemode
;		rax - CPUID dest
;		rcx - CPUID dest
;		rdx - CPUID dest
;		rbx - CPUID dest
;		rbp - TSC start
;		rsp - stack
;		rsi - buf
;		rdi - size
;		r8-r15 - empty

TRACE_START
		push			r15
		push			r14
		push			r13
		push			r12
		push			rbx
		push			rbp
		push			rsi
		push			rdi

		mov				rsi, rcx
		mov				rdi, rdx
IF savexmm
		movups			[xmmTemp + 00h], xmm6
		movups			[xmmTemp + 10h], xmm7
		movups			[xmmTemp + 20h], xmm8
		movups			[xmmTemp + 30h], xmm9
		movups			[xmmTemp + 40h], xmm10
		movups			[xmmTemp + 50h], xmm11
		movups			[xmmTemp + 60h], xmm12
		movups			[xmmTemp + 70h], xmm13
		movups			[xmmTemp + 80h], xmm14
		movups			[xmmTemp + 90h], xmm15
ENDIF
IF timemode
		xor				eax, eax
		cpuid
		rdtsc
		shl				rdx, 20h
		or				rdx, rax
		mov				rbp, rdx
ENDIF
endm

tail macro  savexmm, timemode
IF timemode
		rdtscp
		shl				rdx, 20h
		or				rdx, rax
		sub				rbp, rdx
		xor				eax, eax
		cpuid
		mov				rax, rbp
		neg				rax
ENDIF
IF savexmm
		movups			xmm6, [xmmTemp + 00h]
		movups			xmm7, [xmmTemp + 10h]
		movups			xmm8, [xmmTemp + 20h]
		movups			xmm9, [xmmTemp + 30h]
		movups			xmm10, [xmmTemp + 40h]
		movups			xmm11, [xmmTemp + 50h]
		movups			xmm12, [xmmTemp + 60h]
		movups			xmm13, [xmmTemp + 70h]
		movups			xmm14, [xmmTemp + 80h]
		movups			xmm15, [xmmTemp + 90h]
ENDIF
		pop				rdi
		pop				rsi
		pop				rbp
		pop				rbx
		pop				r12
		pop				r13
		pop				r14
		pop				r15
TRACE_STOP
		ret
endm

HWPopCntM macro
; in   - rax popcnt counter
;      - rsi memAddr
;      - rdi memLen
; out  - rax +popcnt
;      - rdi 0
; used -  rbx, rcx, rdx, r9, r10, r11, r12, r13
		mov				r13, 20h
		xor				ebx, ebx
		xor				ecx, ecx
		xor				edx, edx

HWPopCnt_loop_20h:
		cmp				rdi, r13
		jb				HWPopCnt_loop_20h_End
		popcnt			r9, [rsi + 00h]
		popcnt			r10, [rsi + 08h]
		popcnt			r11, [rsi + 10h]
		popcnt			r12, [rsi + 18h]
		add				rax, r9
		add				rbx, r10
		add				rcx, r11
		add				rdx, r12
		add				rsi, r13
		sub				rdi, r13
		jmp				HWPopCnt_loop_20h

HWPopCnt_loop_20h_End:
		add				rax, rbx
		add				rcx, rdx
		add				rax, rcx
		mov				r13, 8h

HWPopCnt_remain8:
		cmp				rdi, r13
		jb				HWPopCnt_remain1
		popcnt			rcx, [rsi]
		add				rax, rcx
		add				rsi, r13
		sub				rdi, r13
		jmp				HWPopCnt_remain8

HWPopCnt_remain1:
		test			rdi, rdi
		jz				HWPopCnt_ready
		movzx			ecx, byte ptr [rsi]
		popcnt			ecx, ecx
		add				rax, rcx
		inc				rsi
		dec				rdi
		jmp				HWPopCnt_remain1
HWPopCnt_ready:
endm

CollectYmm0 macro res
		vextracti128	xmm1, ymm0, 1
		vpaddq			xmm0, xmm0, xmm1
		vpextrq			rcx, xmm0, 1
		vmovq			res, xmm0
		add				res, rcx
endm

CollectZmm0 macro res
		vextracti64x4	ymm1, zmm0, 1
		vpaddq			ymm0, ymm0, ymm1
		CollectYmm0		res
endm

PopCntNHM_M macro PROCNAME, timemode
PROCNAME	proc
		head			0, timemode				;rbp TSCstart, rsi: buf, rdi: size

		xor				eax, eax
		HWPopCntM

		tail			0, timemode
PROCNAME endp
endm

AVX2CSA Macro h, l, a, b, c, t1, t2
		vpxor			t1, a, b
		vpand			t2, a, b
		vpxor			l, t1, c
		vpand			t1, t1, c
		vpor			h, t1, t2
endm

AVX2PopCntLT	Macro inBytes, lookup, t1, t2
		vpsrlw			t2, inBytes, 4
		vpand			t1, inBytes, ymmword ptr _0fmask
		vpand			t2, t2, ymmword ptr _0fmask
		vpshufb			t1, lookup, t1
		vpshufb			t2, lookup, t2
		vpaddb			t1, t1, t2
endm

VPopCntHSW_M macro PROCNAME, timemode
PROCNAME	proc
; in   - rax popcnt counter
;      - rsi memAddr
;      - rdi memLen
; out  - rax +popcnt
;      - rdi 0
; used - rcx, rdx, ymm0 - ymm15
		head			1, timemode	;rbp TSCstart, rsi: buf, rdi: size
;IACA_START_MARKER
		;vzeroall
		vpxor			ymm0, ymm0, ymm0				;ymm0 total
		vpxor			ymm1, ymm1, ymm1				;ymm1 ones
		vpxor			ymm2, ymm2, ymm2				;ymm2 twos
		vpxor			ymm3, ymm3, ymm3				;ymm3 fours
		vpxor			ymm4, ymm4, ymm4				;ymm4 eights
		vpxor			ymm5, ymm5, ymm5				;ymm5 sixteens
														;ymm6 twosA
														;ymm7 twosB
														;ymm8 foursA
														;ymm9 foursB
														;ymm10 eightsA
														;ymm11 eightsB
		vbroadcasti128	ymm12, xmmword ptr lookup		;pshufb
		vpxor			ymm13, ymm13, ymm13				;const0

		mov				rcx, 10h * 20h
align 16
loop_200h:
		cmp				rdi, rcx
		jb				loop_200h_End
		AVX2CSA			ymm6, ymm1, ymm1, [rsi], [rsi + 20h], ymm14, ymm15
		AVX2CSA			ymm7, ymm1, ymm1, [rsi + 40h], [rsi + 60h], ymm14, ymm15
			AVX2CSA			ymm8, ymm2, ymm2, ymm6, ymm7, ymm14, ymm15
		AVX2CSA			ymm6, ymm1, ymm1, [rsi + 80h], [rsi + 0A0h], ymm14, ymm15
		AVX2CSA			ymm7, ymm1, ymm1, [rsi + 0C0h], [rsi + 0E0h], ymm14, ymm15
			AVX2CSA			ymm9, ymm2, ymm2, ymm6, ymm7, ymm14, ymm15
				AVX2CSA			ymm10, ymm3, ymm3, ymm8, ymm9, ymm14, ymm15

		AVX2CSA			ymm6, ymm1, ymm1, [rsi + 100h], [rsi + 120h], ymm14, ymm15
		AVX2CSA			ymm7, ymm1, ymm1, [rsi + 140h], [rsi + 160h], ymm14, ymm15
			AVX2CSA			ymm8, ymm2, ymm2, ymm6, ymm7, ymm14, ymm15
		AVX2CSA			ymm6, ymm1, ymm1, [rsi + 180h], [rsi + 1A0h], ymm14, ymm15
		AVX2CSA			ymm7, ymm1, ymm1, [rsi + 1C0h], [rsi + 1E0h], ymm14, ymm15
			AVX2CSA			ymm9, ymm2, ymm2, ymm6, ymm7, ymm14, ymm15
				AVX2CSA			ymm11, ymm3, ymm3, ymm8, ymm9, ymm14, ymm15

					AVX2CSA			ymm5, ymm4, ymm4, ymm10, ymm11, ymm14, ymm15

		AVX2PopCntLT	ymm5, ymm12, ymm14, ymm15
		vpsadbw			ymm14, ymm14, ymm13
		vpaddq			ymm0, ymm0, ymm14

		add				rsi, rcx
		sub				rdi, rcx
		jmp				loop_200h

loop_200h_End:
		
		vpsllq			ymm0, ymm0, 4
		vpxor			ymm6, ymm6, ymm6
		AVX2PopCntLT	ymm4, ymm12, ymm14, ymm15
		vpsllw			ymm14, ymm14, 3
		vpaddb			ymm6, ymm6, ymm14

		AVX2PopCntLT	ymm3, ymm12, ymm14, ymm15
		vpsllw			ymm14, ymm14, 2
		vpaddb			ymm6, ymm6, ymm14

		AVX2PopCntLT	ymm2, ymm12, ymm14, ymm15
		vpsllw			ymm14, ymm14, 1
		vpaddb			ymm6, ymm6, ymm14

		AVX2PopCntLT	ymm1, ymm12, ymm14, ymm15
		vpaddb			ymm6, ymm6, ymm14

		vpsadbw			ymm6, ymm6, ymm13
		vpaddq			ymm0, ymm0, ymm6

		mov				rcx, 20h
align 16
remain_20h:
		cmp				rdi, rcx
		jb				collect
		vmovdqu			ymm6, ymmword ptr [rsi]
		AVX2PopCntLT	ymm6, ymm12, ymm14, ymm15
		vpsadbw			ymm14, ymm14, ymm13
		vpaddq			ymm0, ymm0, ymm14
		add				rsi, rcx
		sub				rdi, rcx
		jmp				remain_20h

collect:
		CollectYmm0		rax
		mov				rcx, 8h

align 16
remain8:
		cmp				rdi, rcx
		jb				remain1
		popcnt			rdx, [rsi]
		add				rax, rdx
		add				rsi, rcx
		sub				rdi, rcx
		jmp				remain8

align 16
remain1:
		test			rdi, rdi
		jz				ready
		movzx			edx, byte ptr [rsi]
		popcnt			ecx, edx
		add				rax, rcx
		inc				rsi
		dec				rdi
		jmp				remain1

;IACA_END_MARKER
ready:
		tail			1, timemode	
PROCNAME endp
endm

AVX512CSA Macro h, l, a, b
		vmovdqa64					h, l							;no uop
		vpternlogq					l, a, b, 96h					;P05
		vpternlogq					h, a, b, 0e8h					;P05
endm

AVX512CSA_Mem Macro h, l, a, b, t, disp
		AVX512CSA					h, l, a, b
		vmovdqu64					t, zmmword ptr [rsi + disp]		;P23
endm

AVX512CSA_2Level Macro ones, twos, t0, t1, t2, b0, b1, b2, b3, l0, l1, l2, disp
		AVX512CSA_Mem				t0, ones, b0, b1, l0, disp
		AVX512CSA_Mem				t1, ones, b2, b3, l1, disp + 40h
			AVX512CSA_Mem			t2, twos, t0, t1, l2, disp + 80h
endm

AVX512CSA_3Level Macro ones, twos, fours, t0, t1, t2, t3, t4, b0, b1, b2, b3, b4, b5, b6, b7, l0, l1, l2, l3, l4, l5, l6, disp
		AVX512CSA_2Level			ones, twos, t0, t1, t2, b0, b1, b2, b3, l0, l1, l2, disp
		AVX512CSA_2Level			ones, twos, t0, t1, t3, b4, b5, b6, b7, l3, l4, l5, disp + 0C0h
			AVX512CSA_Mem				t4, fours, t2, t3, l6, disp + 180h
endm

AVX512CSA_4Level Macro ones, twos, fours, eights, t0, t1, t2, t3, t4, t5, t6, b0, b1, b2, b3, b4, b5, b6, b7, l0, l1, l2, l3, l4, l5, l6, l7, disp
	AVX512CSA_3Level				ones, twos, fours, t0, t1, t2, t3, t4, b0, b1, b2, b3, b4, b5, b6, b7, l0, l1, l2, l3, l4, l5, l6, disp
		vmovdqu64						l7, zmmword ptr [rsi + disp + 1C0h]
	AVX512CSA_3Level				ones, twos, fours, t0, t1, t2, t3, t5, b0, b1, b2, b3, b4, b5, b6, b7, l0, l1, l2, l3, l4, l5, l6, disp + 200h
		AVX512CSA_Mem					t6, eights, t4, t5, l7, disp + 3C0h
endm


AVX512CSA_YMM Macro h, l, a, b
		vmovdqa64					h, l
		vpternlogq					l, a, b, 96h
		vpternlogq					h, a, b, 0e8h
endm

AVX512CSA_YMM_Mem Macro h, l, a, b, t, disp
		AVX512CSA_YMM				h, l, a, b
		vmovdqu64					t, ymmword ptr [rsi + disp]
endm

AVX512CSA_YMM_2Level Macro ones, twos, t0, t1, t2, b0, b1, b2, b3, l0, l1, l2, disp
		AVX512CSA_YMM_Mem			t0, ones, b0, b1, l0, disp
		AVX512CSA_YMM_Mem			t1, ones, b2, b3, l1, disp + 20h
			AVX512CSA_YMM_Mem		t2, twos, t0, t1, l2, disp + 40h
endm

AVX512CSA_YMM_3Level Macro ones, twos, fours, t0, t1, t2, t3, t4, b0, b1, b2, b3, b4, b5, b6, b7, l0, l1, l2, l3, l4, l5, l6, disp
		AVX512CSA_YMM_2Level		ones, twos, t0, t1, t2, b0, b1, b2, b3, l0, l1, l2, disp
		AVX512CSA_YMM_2Level		ones, twos, t0, t1, t3, b4, b5, b6, b7, l3, l4, l5, disp + 060h
			AVX512CSA_YMM_Mem			t4, fours, t2, t3, l6, disp + 0C0h
endm

AVX512CSA_YMM_4Level Macro ones, twos, fours, eights, t0, t1, t2, t3, t4, t5, t6, b0, b1, b2, b3, b4, b5, b6, b7, l0, l1, l2, l3, l4, l5, l6, l7, disp
	AVX512CSA_YMM_3Level				ones, twos, fours, t0, t1, t2, t3, t4, b0, b1, b2, b3, b4, b5, b6, b7, l0, l1, l2, l3, l4, l5, l6, disp
		vmovdqu64						l7, ymmword ptr [rsi + disp + 0E0h]
	AVX512CSA_YMM_3Level				ones, twos, fours, t0, t1, t2, t3, t5, b0, b1, b2, b3, b4, b5, b6, b7, l0, l1, l2, l3, l4, l5, l6, disp + 100h
		AVX512CSA_YMM_Mem					t6, eights, t4, t5, l7, disp + 1E0h
endm


AVX512PopCntReg Macro v, t1, t2, _5maskReg, _3maskReg, _fmaskReg, _ffmaskReg
		vpsrlq						t1, v, 1
		vpandq						t1, t1, _5maskReg
		vpsubq						t1, v, t1

		vpsrlq						t2, t1, 2
		vpandq						t1, t1, _3maskReg
		vpandq						t2, t2, _3maskReg
		vpaddq						t1, t1, t2

		vpsrlq						t2, t1, 4
		vpaddq						t1, t1, t2
		vpandq						t1, t1, _fmaskReg

		vpsrlq						t2, t1, 32
		vpaddq						t1, t1, t2		;4byte
		vpsrlq						t2, t1, 16
		vpaddq						t1, t1, t2		;2byte
		vpsrlq						t2, t1, 8
		vpaddq						t1, t1, t2		;1byte
		vpandq						t1, t1, _ffmaskReg
endm

AVX512PopCntLT_BW Macro inBytes, lookup, t1, t2, _0fmaskReg
		vpsrlw						t2, inBytes, 4				;P0			1-1
		vpandq						t1, inBytes, _0fmaskReg		;P5			1-1
		vpshufb						t1, lookup, t1				;P5			2-2
		vpandq						t2, t2, _0fmaskReg			;P0			2-2
		vpshufb						t2, lookup, t2				;P5			3-3
		vpaddb						t1, t1, t2					;P05		4-4
endm

AVX512PopCntLT_VBMI Macro inBytes, lookup1, lookup2, temp, temp2
		vpmovb2m					k1, inBytes					;P0			1-1 according to IACA3.00 & 336289-002.pdf
		vpermi2b					inBytes, lookup1, lookup2	;P5			1-7 ??? 7|2 latency supposed
		kmovq						[ktemp + 00h], k1			;P237 P4	2-6 the only k reg operation that doesn't use P0 and P5
		popcnt						temp, [ktemp + 00h]			;P237 P1	7-9 P0&P5 remain free

		;kmovq						temp2, k1					;P0			2-4 shorter latency, but it consumes P0
		;popcnt						temp, temp2					;P1			5-7
endm

VPopCntKNL_M macro PROCNAME, timemode
PROCNAME	proc
; in   - rax popcnt counter
;      - rsi memAddr
;      - rdi memLen
; out  - rax +popcnt
;      - rdi 0
; used - rbx, rcx, rdx, r9, r10, r11, r12, r13, zmm0 - zmm31
		head						1, timemode						;rbp TSCstart, rsi: buf, rdi: size
;IACA_START_MARKER

		vpxor						xmm0, xmm0, xmm0				;total; VEX zeroing: 4 bytes, EVEX: 6 bytes
		vpbroadcastq				zmm20, qword ptr _5mask			;5555555555555555h
		vpbroadcastq				zmm21, qword ptr _3mask			;3333333333333333h
		vpbroadcastq				zmm22, qword ptr _0fmask		;0f0f0f0f0f0f0f0fh
		vpbroadcastq				zmm23, qword ptr _ffmask		;00000000000000ffh

		mov							rcx, 20h * 40h * 2
		cmp							rdi, rcx
		jb							remain_400_fffh

		vpxor						xmm1, xmm1, xmm1				;ones
		vpxor						xmm2, xmm2, xmm2				;twos
		vpxor						xmm3, xmm3, xmm3				;fours
		vpxor						xmm4, xmm4, xmm4				;eights
		vpxor						xmm5, xmm5, xmm5				;sixteens
		vpxor						xmm6, xmm6, xmm6				;thirtytwos
		vpxor						xmm7, xmm7, xmm7				;sixtyfours
																	;zmm8 twosA
																	;zmm9 twosB
																	;zmm10 foursA
																	;zmm11 foursB
																	;zmm12 eightsA
																	;zmm13 eightsB
																	;zmm14 sixteensA
																	;zmm15 sixteensB
																	;zmm16 thirtytwosA
																	;zmm17 thirtytwosB

		vmovdqu64					zmm24, zmmword ptr [rsi]
		vmovdqu64					zmm25, zmmword ptr [rsi + 040h]
		vmovdqu64					zmm26, zmmword ptr [rsi + 080h]
		vmovdqu64					zmm27, zmmword ptr [rsi + 0C0h]
		vmovdqu64					zmm28, zmmword ptr [rsi + 100h]
		vmovdqu64					zmm29, zmmword ptr [rsi + 140h]
		vmovdqu64					zmm30, zmmword ptr [rsi + 180h]
		vmovdqu64					zmm31, zmmword ptr [rsi + 1C0h]

align 16
loop_1000h:
		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm14, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 200h

		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm15, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 600h

			AVX512CSA				zmm16, zmm5, zmm14, zmm15

		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm14, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 0A00h

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm12, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0E00h
		vmovdqu64					zmm31, zmmword ptr [rsi + 0FC0h]

		sub							rdi, rcx
		add							rsi, rcx
		cmp							rdi, rcx
		jb							loop_1000h_End

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm13, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0h
			AVX512CSA_Mem			zmm15, zmm4, zmm12, zmm13, zmm31, 1C0h
				AVX512CSA			zmm17, zmm5, zmm14, zmm15
					AVX512CSA		zmm7, zmm6, zmm16, zmm17

		AVX512PopCntReg				zmm7, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpaddq						zmm0, zmm0, zmm8
		jmp							loop_1000h

loop_1000h_End:
		AVX512CSA					zmm8, zmm1, zmm24, zmm25
		AVX512CSA					zmm9, zmm1, zmm26, zmm27
			AVX512CSA				zmm10, zmm2, zmm8, zmm9
		AVX512CSA					zmm8, zmm1, zmm28, zmm29
		AVX512CSA					zmm9, zmm1, zmm30, zmm31
			AVX512CSA				zmm11, zmm2, zmm8, zmm9
				AVX512CSA			zmm13, zmm3, zmm10, zmm11
					AVX512CSA		zmm15, zmm4, zmm12, zmm13
						AVX512CSA	zmm17, zmm5, zmm14, zmm15
						AVX512CSA	zmm7, zmm6, zmm16, zmm17

		AVX512PopCntReg				zmm7, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpaddq						zmm0, zmm0, zmm8
		vpsllq						zmm0, zmm0, 6

		AVX512PopCntReg				zmm6, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 5
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm5, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 4
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm4, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 3
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm3, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 2
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm2, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 1
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm1, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpaddq						zmm0, zmm0, zmm8

		test						rdi, rdi
		jz							collect
align 16
remain_400_fffh:
		mov							rcx, 10h * 40h
		cmp							rdi, rcx
		jb							remain_40_3ffh

		vpxor						xmm1, xmm1, xmm1				;ones
		vpxor						xmm2, xmm2, xmm2				;twos
		vpxor						xmm3, xmm3, xmm3				;fours
		vpxor						xmm4, xmm4, xmm4				;eights
		vpxor						xmm5, xmm5, xmm5				;sixteens
																	;zmm8 twosA
																	;zmm9 twosB
																	;zmm10 foursA
																	;zmm11 foursB
																	;zmm12 eightsA
																	;zmm13 eightsB

		vmovdqu64					zmm24, zmmword ptr [rsi]
		vmovdqu64					zmm25, zmmword ptr [rsi + 040h]
		vmovdqu64					zmm26, zmmword ptr [rsi + 080h]
		vmovdqu64					zmm27, zmmword ptr [rsi + 0C0h]
		vmovdqu64					zmm28, zmmword ptr [rsi + 100h]
		vmovdqu64					zmm29, zmmword ptr [rsi + 140h]
		vmovdqu64					zmm30, zmmword ptr [rsi + 180h]
		vmovdqu64					zmm31, zmmword ptr [rsi + 1C0h]

align 16
remain_400_fffh_Loop:

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm12, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0200h
		vmovdqu64					zmm31, zmmword ptr [rsi + 3C0h]

		sub							rdi, rcx
		add							rsi, rcx
		cmp							rdi, rcx
		jb							remain_400_fffh_End

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm13, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 000h
					AVX512CSA_Mem	zmm5, zmm4, zmm12, zmm13, zmm31, 1C0h

		AVX512PopCntReg				zmm5, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 4
		vpaddq						zmm0, zmm0, zmm8
		jmp							remain_400_fffh_Loop

remain_400_fffh_End:
		AVX512CSA					zmm8, zmm1, zmm24, zmm25
		AVX512CSA					zmm9, zmm1, zmm26, zmm27
			AVX512CSA				zmm10, zmm2, zmm8, zmm9
		AVX512CSA					zmm8, zmm1, zmm28, zmm29
		AVX512CSA					zmm9, zmm1, zmm30, zmm31
			AVX512CSA				zmm11, zmm2, zmm8, zmm9
				AVX512CSA			zmm13, zmm3, zmm10, zmm11
					AVX512CSA		zmm5, zmm4, zmm12, zmm13

		AVX512PopCntReg				zmm5, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 4
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm4, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 3
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm3, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 2
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm2, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpsllq						zmm8, zmm8, 1
		vpaddq						zmm0, zmm0, zmm8

		AVX512PopCntReg				zmm1, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpaddq						zmm0, zmm0, zmm8

		test						rdi, rdi
		jz							collect

remain_40_3ffh:
		vpxor						xmm8, xmm8, xmm8
		mov							rcx, 40h
align 16
remain_40_3ffh_Loop:
		cmp							rdi, rcx
		jb							remain_0_3fh
		vmovdqu64					zmm6, zmmword ptr [rsi]
		AVX512PopCntReg				zmm6, zmm8, zmm9, zmm20, zmm21, zmm22, zmm23
		vpaddq						zmm0, zmm0, zmm8

		add							rsi, rcx
		sub							rdi, rcx
		jmp							remain_40_3ffh_Loop

remain_0_3fh:
collect:
		CollectZmm0					rax

		HWPopCntM

		tail						1, timemode
PROCNAME endp
endm

VPopCntSKX_M macro PROCNAME, timemode
PROCNAME	proc
; in   - rax popcnt counter
;      - rsi memAddr
;      - rdi memLen
; out  - rax +popcnt
;      - rdi 0
; used - rcx, rdx, zmm0 - zmm31, k1
		head						1, timemode						;rbp TSCstart, rsi: buf, rdi: size
;IACA_START_MARKER

		vpxor						xmm0, xmm0, xmm0				;total
		vpxorq						zmm21, zmm21, zmm21				;const0
		vpbroadcastq				zmm22, qword ptr _0fmask		;0f0f..0f
		vbroadcasti32x4				zmm23, xmmword ptr lookup		;pshufb

		mov							rcx, 20h * 40h * 2
		cmp							rdi, rcx
		jb							remain_400_fffh

		vpxor						xmm1, xmm1, xmm1				;ones
		vpxor						xmm2, xmm2, xmm2				;twos
		vpxor						xmm3, xmm3, xmm3				;fours
		vpxor						xmm4, xmm4, xmm4				;eights
		vpxor						xmm5, xmm5, xmm5				;sixteens
		vpxor						xmm6, xmm6, xmm6				;thirtytwos
		vpxor						xmm7, xmm7, xmm7				;sixtyfours
																	;zmm8 twosA
																	;zmm9 twosB
																	;zmm10 foursA
																	;zmm11 foursB
																	;zmm12 eightsA
																	;zmm13 eightsB
																	;zmm14 sixteensA
																	;zmm15 sixteensB
																	;zmm16 thirtytwosA
																	;zmm17 thirtytwosB

		vmovdqu64					zmm24, zmmword ptr [rsi]
		vmovdqu64					zmm25, zmmword ptr [rsi + 040h]
		vmovdqu64					zmm26, zmmword ptr [rsi + 080h]
		vmovdqu64					zmm27, zmmword ptr [rsi + 0C0h]
		vmovdqu64					zmm28, zmmword ptr [rsi + 100h]
		vmovdqu64					zmm29, zmmword ptr [rsi + 140h]
		vmovdqu64					zmm30, zmmword ptr [rsi + 180h]
		vmovdqu64					zmm31, zmmword ptr [rsi + 1C0h]

align 16
loop_1000h:
		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm14, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 200h

		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm15, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 600h

			AVX512CSA				zmm16, zmm5, zmm14, zmm15

		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm14, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 0A00h

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm12, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0E00h
		vmovdqu64					zmm31, zmmword ptr [rsi + 0FC0h]

		sub							rdi, rcx
		add							rsi, rcx
		cmp							rdi, rcx
		jb							loop_1000h_End

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm13, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0h
			AVX512CSA_Mem			zmm15, zmm4, zmm12, zmm13, zmm31, 1C0h
				AVX512CSA			zmm17, zmm5, zmm14, zmm15
					AVX512CSA		zmm7, zmm6, zmm16, zmm17

		AVX512PopCntLT_BW			zmm7, zmm23, zmm8, zmm9, zmm22
		vpsadbw						zmm8, zmm8, zmm21
		vpaddq						zmm0, zmm0, zmm8
		jmp							loop_1000h

loop_1000h_End:
		AVX512CSA					zmm8, zmm1, zmm24, zmm25
		AVX512CSA					zmm9, zmm1, zmm26, zmm27
			AVX512CSA				zmm10, zmm2, zmm8, zmm9
		AVX512CSA					zmm8, zmm1, zmm28, zmm29
		AVX512CSA					zmm9, zmm1, zmm30, zmm31
			AVX512CSA				zmm11, zmm2, zmm8, zmm9
				AVX512CSA			zmm13, zmm3, zmm10, zmm11
					AVX512CSA		zmm15, zmm4, zmm12, zmm13
						AVX512CSA	zmm17, zmm5, zmm14, zmm15
						AVX512CSA	zmm7, zmm6, zmm16, zmm17

		AVX512PopCntLT_BW			zmm7, zmm23, zmm8, zmm9, zmm22
		vpaddb						zmm8, zmm8, zmm8
		vpsllq						zmm0, zmm0, 6

		AVX512PopCntLT_BW			zmm6, zmm23, zmm18, zmm19, zmm22
		vpaddb						zmm18, zmm18, zmm8
		vpsadbw						zmm18, zmm18, zmm21
		vpsllq						zmm18, zmm18, 5
		vpaddq						zmm0, zmm0, zmm18
		vpxor						xmm15, xmm15, xmm15

		AVX512PopCntLT_BW			zmm5, zmm23, zmm18, zmm19, zmm22
		vpsllw						zmm19, zmm18, 4
		vpaddb						zmm15, zmm15, zmm19

		AVX512PopCntLT_BW			zmm4, zmm23, zmm18, zmm19, zmm22
		vpsllw						zmm19, zmm18, 3
		vpaddb						zmm15, zmm15, zmm19

		AVX512PopCntLT_BW			zmm3, zmm23, zmm18, zmm19, zmm22
		vpsllw						zmm19, zmm18, 2
		vpaddb						zmm15, zmm15, zmm19

		AVX512PopCntLT_BW			zmm2, zmm23, zmm18, zmm19, zmm22
		vpsllw						zmm19, zmm18, 1
		vpaddb						zmm15, zmm15, zmm19

		AVX512PopCntLT_BW			zmm1, zmm23, zmm18, zmm19, zmm22
		vpaddb						zmm15, zmm15, zmm18
		vpsadbw						zmm15, zmm15, zmm21
		vpaddq						zmm0, zmm0, zmm15

		test						rdi, rdi
		jz							collect
align 16
remain_400_fffh:
		mov							rcx, 10h * 40h
		cmp							rdi, rcx
		jb							remain_100_3ffh

		vpxor						xmm1, xmm1, xmm1				;ones
		vpxor						xmm2, xmm2, xmm2				;twos
		vpxor						xmm3, xmm3, xmm3				;fours
		vpxor						xmm4, xmm4, xmm4				;eights
		vpxor						xmm5, xmm5, xmm5				;sixteens
																	;zmm8 twosA
																	;zmm9 twosB
																	;zmm10 foursA
																	;zmm11 foursB
																	;zmm12 eightsA
																	;zmm13 eightsB

		vmovdqu64					zmm24, zmmword ptr [rsi]
		vmovdqu64					zmm25, zmmword ptr [rsi + 040h]
		vmovdqu64					zmm26, zmmword ptr [rsi + 080h]
		vmovdqu64					zmm27, zmmword ptr [rsi + 0C0h]
		vmovdqu64					zmm28, zmmword ptr [rsi + 100h]
		vmovdqu64					zmm29, zmmword ptr [rsi + 140h]
		vmovdqu64					zmm30, zmmword ptr [rsi + 180h]
		vmovdqu64					zmm31, zmmword ptr [rsi + 1C0h]

align 16
remain_400_fffh_Loop:

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm12, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0200h
		vmovdqu64					zmm31, zmmword ptr [rsi + 3C0h]

		sub							rdi, rcx
		add							rsi, rcx
		cmp							rdi, rcx
		jb							remain_400_fffh_End

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm13, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 000h
					AVX512CSA_Mem	zmm5, zmm4, zmm12, zmm13, zmm31, 1C0h
		
		AVX512PopCntLT_BW			zmm5, zmm23, zmm8, zmm9, zmm22
		vpsadbw						zmm8, zmm8, zmm21
		vpsllq						zmm8, zmm8, 4
		vpaddq						zmm0, zmm0, zmm8

		jmp							remain_400_fffh_Loop

remain_400_fffh_End:
		AVX512CSA					zmm8, zmm1, zmm24, zmm25
		AVX512CSA					zmm9, zmm1, zmm26, zmm27
			AVX512CSA				zmm10, zmm2, zmm8, zmm9
		AVX512CSA					zmm8, zmm1, zmm28, zmm29
		AVX512CSA					zmm9, zmm1, zmm30, zmm31
			AVX512CSA				zmm11, zmm2, zmm8, zmm9
				AVX512CSA			zmm13, zmm3, zmm10, zmm11
					AVX512CSA		zmm5, zmm4, zmm12, zmm13

		vpxor						xmm15, xmm15, xmm15

		AVX512PopCntLT_BW			zmm5, zmm23, zmm18, zmm19, zmm22
		vpsllw						zmm19, zmm18, 4
		vpaddb						zmm15, zmm15, zmm19

		AVX512PopCntLT_BW			zmm4, zmm23, zmm18, zmm19, zmm22
		vpsllw						zmm19, zmm18, 3
		vpaddb						zmm15, zmm15, zmm19

		AVX512PopCntLT_BW			zmm3, zmm23, zmm18, zmm19, zmm22
		vpsllw						zmm19, zmm18, 2
		vpaddb						zmm15, zmm15, zmm19

		AVX512PopCntLT_BW			zmm2, zmm23, zmm18, zmm19, zmm22
		vpsllw						zmm19, zmm18, 1
		vpaddb						zmm15, zmm15, zmm19

		AVX512PopCntLT_BW			zmm1, zmm23, zmm18, zmm19, zmm22
		vpaddb						zmm15, zmm15, zmm18
		vpsadbw						zmm15, zmm15, zmm21
		vpaddq						zmm0, zmm0, zmm15

		test						rdi, rdi
		jz							collect
align 16
remain_100_3ffh:
		mov							rcx, 100h
		cmp							rdi, rcx
		jb							remain_40_ffh
		
		vmovdqu64					zmm1, zmmword ptr [rsi]
		vmovdqu64					zmm2, zmmword ptr [rsi + 040h]
		vmovdqu64					zmm3, zmmword ptr [rsi + 080h]
		vmovdqu64					zmm4, zmmword ptr [rsi + 0C0h]

		vpsrlw						zmm5, zmm1, 4					; 1- 1;P0
		vpandq						zmm1, zmm1, zmm22				; 1- 1;P5
		vpshufb						zmm1, zmm23, zmm1				; 2- 2;P5
		vpandq						zmm5, zmm5, zmm22				; 2- 2;P0
		vpshufb						zmm5, zmm23, zmm5				; 3- 3;P5
			vpsrlw						zmm6, zmm2, 4				; 3- 3;P0
		vpaddb						zmm1, zmm1, zmm5				; 4- 4;P05
			vpandq						zmm2, zmm2, zmm22			; 4- 4;P05
			vpshufb						zmm2, zmm23, zmm2			; 5- 5;P5
			vpandq						zmm6, zmm6, zmm22			; 5- 5;P0
			vpshufb						zmm6, zmm23, zmm6			; 6- 6;P5
				vpsrlw						zmm7, zmm3, 4			; 6- 6;P0
			vpaddb						zmm2, zmm2, zmm6			; 7- 7;P05
				vpandq						zmm3, zmm3, zmm22		; 7- 7;P05
				vpshufb						zmm3, zmm23, zmm3		; 8- 8;P5
				vpandq						zmm7, zmm7, zmm22		; 8- 8;P0
				vpshufb						zmm7, zmm23, zmm7		; 9- 9;P5
					vpsrlw						zmm8, zmm4, 4		; 9- 9;P0
				vpaddb						zmm3, zmm3, zmm7		;10-10;P05
					vpandq						zmm4, zmm4, zmm22	;10-10;P05
					vpshufb						zmm4, zmm23, zmm4	;11-11;P5
					vpandq						zmm8, zmm8, zmm22	;11-11;P0
					vpshufb						zmm8, zmm23, zmm8	;12-12;P5
			vpaddb					zmm1, zmm1, zmm2				;12-12;P0
				vpaddb					zmm1, zmm1, zmm3			;13-13;P05
					vpaddb					zmm4, zmm4, zmm8		;13-13;P05
			vpaddb					zmm1, zmm1, zmm4				;14-14;P05
		vpsadbw						zmm1, zmm1, zmm21				;15-17;P5
		vpaddq						zmm0, zmm0, zmm1				;18-18;P05

		add							rsi, rcx
		sub							rdi, rcx
		jmp							remain_100_3ffh

remain_40_ffh:
		vpxor						xmm8, xmm8, xmm8
		mov							rcx, 40h
align 16
remain_40_ffh_Loop:
		cmp							rdi, rcx
		jb							remain_0_3fh
		vmovdqu64					zmm6, zmmword ptr [rsi]
		vpsrlw						zmm7, zmm6, 4
		vpandq						zmm6, zmm6, zmm22
		vpandq						zmm7, zmm7, zmm22
		vpshufb						zmm6, zmm23, zmm6
		vpshufb						zmm7, zmm23, zmm7
		vpaddb						zmm6, zmm6, zmm7
		vpaddb						zmm8, zmm6, zmm8
		add							rsi, rcx
		sub							rdi, rcx
		jmp							remain_40_ffh_Loop

remain_0_3fh:
		test						rdi, rdi
		jz							collect_0_ffh
		mov							rdx, -1
		bzhi						rdx, rdx, rdi					;bzhi works in range 0-255
		kmovq						k1, rdx
		vmovdqu8					zmm6 {k1}{z}, zmmword ptr [rsi]
		vpsrlw						zmm7, zmm6, 4
		vpandq						zmm6, zmm6, zmm22
		vpandq						zmm7, zmm7, zmm22
		vpshufb						zmm6, zmm23, zmm6
		vpshufb						zmm7, zmm23, zmm7
		vpaddb						zmm6, zmm6, zmm7
		vpaddb						zmm8, zmm6, zmm8
collect_0_ffh:
		vpsadbw						zmm8, zmm8, zmm21
		vpaddq						zmm0, zmm0, zmm8
collect:
		CollectZmm0					rax

		tail						1, timemode
PROCNAME endp
endm

VPopCntSKX_YMM_M macro PROCNAME, timemode
PROCNAME	proc
; in   - rax popcnt counter
;      - rsi memAddr
;      - rdi memLen
; out  - rax +popcnt
;      - rdi 0
; used - rcx, rdx, ymm0 - ymm31, k1
		head							1, timemode						;rbp TSCstart, rsi: buf, rdi: size
;IACA_START_MARKER

		vpxor							ymm0, ymm0, ymm0				;total
		vpxorq							ymm21, ymm21, ymm21				;const0
		vpbroadcastq					ymm22, qword ptr _0fmask		;0f0f..0f
		vbroadcasti32x4					ymm23, xmmword ptr lookup		;pshufb

		mov								rcx, 20h * 20h * 2
		cmp								rdi, rcx
		jb								remain_200_7ffh

		vpxor							ymm1, ymm1, ymm1				;ones
		vpxor							ymm2, ymm2, ymm2				;twos
		vpxor							ymm3, ymm3, ymm3				;fours
		vpxor							ymm4, ymm4, ymm4				;eights
		vpxor							ymm5, ymm5, ymm5				;sixteens
		vpxor							ymm6, ymm6, ymm6				;thirtytwos
		vpxor							ymm7, ymm7, ymm7				;sixtyfours
																		;ymm8 twosA
																		;ymm9 twosB
																		;ymm10 foursA
																		;ymm11 foursB
																		;ymm12 eightsA
																		;ymm13 eightsB
																		;ymm14 sixteensA
																		;ymm15 sixteensB
																		;ymm16 thirtytwosA
																		;ymm17 thirtytwosB

		vmovdqu64						ymm24, ymmword ptr [rsi]
		vmovdqu64						ymm25, ymmword ptr [rsi + 020h]
		vmovdqu64						ymm26, ymmword ptr [rsi + 040h]
		vmovdqu64						ymm27, ymmword ptr [rsi + 060h]
		vmovdqu64						ymm28, ymmword ptr [rsi + 080h]
		vmovdqu64						ymm29, ymmword ptr [rsi + 0A0h]
		vmovdqu64						ymm30, ymmword ptr [rsi + 0C0h]
		vmovdqu64						ymm31, ymmword ptr [rsi + 0E0h]

align 16
loop_800h:
		AVX512CSA_YMM_4Level			ymm1, ymm2, ymm3, ymm4, ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, 100h

		AVX512CSA_YMM_4Level			ymm1, ymm2, ymm3, ymm4, ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm15, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, 300h

			AVX512CSA_YMM				ymm16, ymm5, ymm14, ymm15

		AVX512CSA_YMM_4Level			ymm1, ymm2, ymm3, ymm4, ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, 0500h

		AVX512CSA_YMM_3Level			ymm1, ymm2, ymm3, ymm8, ymm9, ymm10, ymm11, ymm12, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, 0700h
		vmovdqu64						ymm31, ymmword ptr [rsi + 07E0h]

		sub								rdi, rcx
		add								rsi, rcx
		cmp								rdi, rcx
		jb								loop_800h_End

		AVX512CSA_YMM_3Level			ymm1, ymm2, ymm3, ymm8, ymm9, ymm10, ymm11, ymm13, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, 0h
			AVX512CSA_YMM_Mem			ymm15, ymm4, ymm12, ymm13, ymm31, 0E0h
				AVX512CSA_YMM			ymm17, ymm5, ymm14, ymm15
					AVX512CSA_YMM		ymm7, ymm6, ymm16, ymm17

		AVX512PopCntLT_BW				ymm7, ymm23, ymm8, ymm9, ymm22
		vpsadbw							ymm8, ymm8, ymm21
		vpaddq							ymm0, ymm0, ymm8
		jmp								loop_800h

loop_800h_End:
		AVX512CSA_YMM					ymm8, ymm1, ymm24, ymm25
		AVX512CSA_YMM					ymm9, ymm1, ymm26, ymm27
			AVX512CSA_YMM				ymm10, ymm2, ymm8, ymm9
		AVX512CSA_YMM					ymm8, ymm1, ymm28, ymm29
		AVX512CSA_YMM					ymm9, ymm1, ymm30, ymm31
			AVX512CSA_YMM				ymm11, ymm2, ymm8, ymm9
				AVX512CSA_YMM			ymm13, ymm3, ymm10, ymm11
					AVX512CSA_YMM		ymm15, ymm4, ymm12, ymm13
						AVX512CSA_YMM	ymm17, ymm5, ymm14, ymm15
						AVX512CSA_YMM	ymm7, ymm6, ymm16, ymm17

		AVX512PopCntLT_BW				ymm7, ymm23, ymm8, ymm9, ymm22
		vpaddb							ymm8, ymm8, ymm8
		vpsllq							ymm0, ymm0, 6

		AVX512PopCntLT_BW				ymm6, ymm23, ymm18, ymm19, ymm22
		vpaddb							ymm18, ymm18, ymm8
		vpsadbw							ymm18, ymm18, ymm21
		vpsllq							ymm18, ymm18, 5
		vpaddq							ymm0, ymm0, ymm18
		vpxorq							ymm17, ymm17, ymm17

		AVX512PopCntLT_BW				ymm5, ymm23, ymm18, ymm19, ymm22
		vpsllw							ymm19, ymm18, 4
		vpaddb							ymm17, ymm17, ymm19

		AVX512PopCntLT_BW				ymm4, ymm23, ymm18, ymm19, ymm22
		vpsllw							ymm19, ymm18, 3
		vpaddb							ymm17, ymm17, ymm19

		AVX512PopCntLT_BW				ymm3, ymm23, ymm18, ymm19, ymm22
		vpsllw							ymm19, ymm18, 2
		vpaddb							ymm17, ymm17, ymm19

		AVX512PopCntLT_BW				ymm2, ymm23, ymm18, ymm19, ymm22
		vpsllw							ymm19, ymm18, 1
		vpaddb							ymm17, ymm17, ymm19

		AVX512PopCntLT_BW				ymm1, ymm23, ymm18, ymm19, ymm22
		vpaddb							ymm17, ymm17, ymm18
		vpsadbw							ymm17, ymm17, ymm21
		vpaddq							ymm0, ymm0, ymm17

		test							rdi, rdi
		jz								collect
align 16
remain_200_7ffh:
		mov								rcx, 10h * 20h
		cmp								rdi, rcx
		jb								remain_80_1ffh

		vpxorq							ymm1, ymm1, ymm1				;ones
		vpxorq							ymm2, ymm2, ymm2				;twos
		vpxorq							ymm3, ymm3, ymm3				;fours
		vpxorq							ymm4, ymm4, ymm4				;eights
		vpxorq							ymm5, ymm5, ymm5				;sixteens
																	;ymm8 twosA
																	;ymm9 twosB
																	;ymm10 foursA
																	;ymm11 foursB
																	;ymm12 eightsA
																	;ymm13 eightsB

		vmovdqu64						ymm24, ymmword ptr [rsi]
		vmovdqu64						ymm25, ymmword ptr [rsi + 020h]
		vmovdqu64						ymm26, ymmword ptr [rsi + 040h]
		vmovdqu64						ymm27, ymmword ptr [rsi + 060h]
		vmovdqu64						ymm28, ymmword ptr [rsi + 080h]
		vmovdqu64						ymm29, ymmword ptr [rsi + 0A0h]
		vmovdqu64						ymm30, ymmword ptr [rsi + 0C0h]
		vmovdqu64						ymm31, ymmword ptr [rsi + 0E0h]

align 16
remain_200_7ffh_Loop:

		AVX512CSA_YMM_3Level			ymm1, ymm2, ymm3, ymm8, ymm9, ymm10, ymm11, ymm12, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, 0100h
		vmovdqu64						ymm31, ymmword ptr [rsi + 1E0h]

		sub								rdi, rcx
		add								rsi, rcx
		cmp								rdi, rcx
		jb								remain_200_7ffh_End

		AVX512CSA_YMM_3Level			ymm1, ymm2, ymm3, ymm8, ymm9, ymm10, ymm11, ymm13, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, ymm31, ymm24, ymm25, ymm26, ymm27, ymm28, ymm29, ymm30, 000h
					AVX512CSA_YMM_Mem	ymm5, ymm4, ymm12, ymm13, ymm31, 0E0h
		
		AVX512PopCntLT_BW				ymm5, ymm23, ymm8, ymm9, ymm22
		vpsadbw							ymm8, ymm8, ymm21
		vpsllq							ymm8, ymm8, 4
		vpaddq							ymm0, ymm0, ymm8

		jmp								remain_200_7ffh_Loop

remain_200_7ffh_End:
		AVX512CSA_YMM					ymm8, ymm1, ymm24, ymm25
		AVX512CSA_YMM					ymm9, ymm1, ymm26, ymm27
			AVX512CSA_YMM				ymm10, ymm2, ymm8, ymm9
		AVX512CSA_YMM					ymm8, ymm1, ymm28, ymm29
		AVX512CSA_YMM					ymm9, ymm1, ymm30, ymm31
			AVX512CSA_YMM				ymm11, ymm2, ymm8, ymm9
				AVX512CSA_YMM			ymm13, ymm3, ymm10, ymm11
					AVX512CSA_YMM		ymm5, ymm4, ymm12, ymm13

		vpxorq							ymm17, ymm17, ymm17

		AVX512PopCntLT_BW				ymm5, ymm23, ymm18, ymm19, ymm22
		vpsllw							ymm19, ymm18, 4
		vpaddb							ymm17, ymm17, ymm19

		AVX512PopCntLT_BW				ymm4, ymm23, ymm18, ymm19, ymm22
		vpsllw							ymm19, ymm18, 3
		vpaddb							ymm17, ymm17, ymm19

		AVX512PopCntLT_BW				ymm3, ymm23, ymm18, ymm19, ymm22
		vpsllw							ymm19, ymm18, 2
		vpaddb							ymm17, ymm17, ymm19

		AVX512PopCntLT_BW				ymm2, ymm23, ymm18, ymm19, ymm22
		vpsllw							ymm19, ymm18, 1
		vpaddb							ymm17, ymm17, ymm19

		AVX512PopCntLT_BW				ymm1, ymm23, ymm18, ymm19, ymm22
		vpaddb							ymm17, ymm17, ymm18
		vpsadbw							ymm17, ymm17, ymm21
		vpaddq							ymm0, ymm0, ymm17

align 16
remain_80_1ffh:
		test							rdi, rdi
		jz								collect
		mov								rcx, 80h
align 16
remain_80_1ffh_Loop:
		cmp								rdi, rcx
		jb								remain_20_7fh
		
		vmovdqu64						ymm1, ymmword ptr [rsi]
		vmovdqu64						ymm2, ymmword ptr [rsi + 020h]
		vmovdqu64						ymm3, ymmword ptr [rsi + 040h]
		vmovdqu64						ymm4, ymmword ptr [rsi + 060h]

		vpsrlw							ymm5, ymm1, 4					; 1- 1;P0
		vpandq							ymm1, ymm1, ymm22				; 1- 1;P5
		vpshufb							ymm1, ymm23, ymm1				; 2- 2;P5
		vpandq							ymm5, ymm5, ymm22				; 2- 2;P0
		vpshufb							ymm5, ymm23, ymm5				; 3- 3;P5
			vpsrlw							ymm6, ymm2, 4				; 3- 3;P0
		vpaddb							ymm1, ymm1, ymm5				; 4- 4;P05
			vpandq							ymm2, ymm2, ymm22			; 4- 4;P05
			vpshufb							ymm2, ymm23, ymm2			; 5- 5;P5
			vpandq							ymm6, ymm6, ymm22			; 5- 5;P0
			vpshufb							ymm6, ymm23, ymm6			; 6- 6;P5
				vpsrlw							ymm7, ymm3, 4			; 6- 6;P0
			vpaddb							ymm2, ymm2, ymm6			; 7- 7;P05
				vpandq							ymm3, ymm3, ymm22		; 7- 7;P05
				vpshufb							ymm3, ymm23, ymm3		; 8- 8;P5
				vpandq							ymm7, ymm7, ymm22		; 8- 8;P0
				vpshufb							ymm7, ymm23, ymm7		; 9- 9;P5
					vpsrlw							ymm8, ymm4, 4		; 9- 9;P0
				vpaddb							ymm3, ymm3, ymm7		;10-10;P05
					vpandq							ymm4, ymm4, ymm22	;10-10;P05
					vpshufb							ymm4, ymm23, ymm4	;11-11;P5
					vpandq							ymm8, ymm8, ymm22	;11-11;P0
					vpshufb							ymm8, ymm23, ymm8	;12-12;P5
			vpaddb						ymm1, ymm1, ymm2				;12-12;P0
				vpaddb						ymm1, ymm1, ymm3			;13-13;P05
					vpaddb						ymm4, ymm4, ymm8		;13-13;P05
			vpaddb						ymm1, ymm1, ymm4				;14-14;P05
		vpsadbw							ymm1, ymm1, ymm21				;15-17;P5
		vpaddq							ymm0, ymm0, ymm1				;18-18;P05

		add								rsi, rcx
		sub								rdi, rcx
		jmp								remain_80_1ffh_Loop

remain_20_7fh:
		vpxor							ymm8, ymm8, ymm8
		mov								rcx, 20h
align 16
remain_20_7fh_Loop:
		cmp								rdi, rcx
		jb								remain_0_1fh
		vmovdqu64						ymm6, ymmword ptr [rsi]
		vpsrlw							ymm7, ymm6, 4
		vpandq							ymm6, ymm6, ymm22
		vpandq							ymm7, ymm7, ymm22
		vpshufb							ymm6, ymm23, ymm6
		vpshufb							ymm7, ymm23, ymm7
		vpaddb							ymm6, ymm6, ymm7
		vpaddb							ymm8, ymm6, ymm8
		add								rsi, rcx
		sub								rdi, rcx
		jmp								remain_20_7fh_Loop

remain_0_1fh:
		test							rdi, rdi
		jz								collect_0_7fh
		mov								rdx, -1
		bzhi							rdx, rdx, rdi					;bzhi works in range 0-255
		kmovq							k1, rdx
		vmovdqu8						ymm6 {k1}{z}, ymmword ptr [rsi]
		vpsrlw							ymm7, ymm6, 4
		vpandq							ymm6, ymm6, ymm22
		vpandq							ymm7, ymm7, ymm22
		vpshufb							ymm6, ymm23, ymm6
		vpshufb							ymm7, ymm23, ymm7
		vpaddb							ymm6, ymm6, ymm7
		vpaddb							ymm8, ymm6, ymm8
collect_0_7fh:
		vpsadbw							ymm8, ymm8, ymm21
		vpaddq							ymm0, ymm0, ymm8
collect:
		Collectymm0						rax

		tail							1, timemode
PROCNAME endp
endm

VPopCntCNL_M macro PROCNAME, timemode
PROCNAME	proc
; in   - rax popcnt counter
;      - rsi memAddr
;      - rdi memLen
; out  - rax +popcnt
;      - rdi 0
; used - rcx, rdx, rbx, r12, zmm0 - zmm31, k1-k4
		head						1, timemode						;rbp TSCstart, rsi: buf, rdi: size
;IACA_START_MARKER

		vpxor						xmm0, xmm0, xmm0				;total 7b
		vpxorq						zmm21, zmm21, zmm21				;const0
		vmovdqa64					zmm22, zmmword ptr lookup_7b_L	;vpermi2b
		vmovdqa64					zmm23, zmmword ptr lookup_7b_H	;vpermi2b
		xor							eax, eax						;total 1b
		xor							ebx, ebx
		mov							rcx, 20h * 40h * 2
		cmp							rdi, rcx
		jb							remain_400_fffh

		vpxor						xmm1, xmm1, xmm1				;ones
		vpxor						xmm2, xmm2, xmm2				;twos
		vpxor						xmm3, xmm3, xmm3				;fours
		vpxor						xmm4, xmm4, xmm4				;eights
		vpxor						xmm5, xmm5, xmm5				;sixteens
		vpxor						xmm6, xmm6, xmm6				;thirtytwos
		vpxor						xmm7, xmm7, xmm7				;sixtyfours
																	;zmm8 twosA
																	;zmm9 twosB
																	;zmm10 foursA
																	;zmm11 foursB
																	;zmm12 eightsA
																	;zmm13 eightsB
																	;zmm14 sixteensA
																	;zmm15 sixteensB
																	;zmm16 thirtytwosA
																	;zmm17 thirtytwosB

		vmovdqu64					zmm24, zmmword ptr [rsi]
		vmovdqu64					zmm25, zmmword ptr [rsi + 040h]
		vmovdqu64					zmm26, zmmword ptr [rsi + 080h]
		vmovdqu64					zmm27, zmmword ptr [rsi + 0C0h]
		vmovdqu64					zmm28, zmmword ptr [rsi + 100h]
		vmovdqu64					zmm29, zmmword ptr [rsi + 140h]
		vmovdqu64					zmm30, zmmword ptr [rsi + 180h]
		vmovdqu64					zmm31, zmmword ptr [rsi + 1C0h]

align 16
loop_1000h:
		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm14, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 200h

		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm15, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 600h

			AVX512CSA				zmm16, zmm5, zmm14, zmm15

		AVX512CSA_4Level			zmm1, zmm2, zmm3, zmm4, zmm8, zmm9, zmm10, zmm11, zmm12, zmm13, zmm14, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, 0A00h

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm12, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0E00h
		vmovdqu64					zmm31, zmmword ptr [rsi + 0FC0h]

		sub							rdi, rcx
		add							rsi, rcx
		cmp							rdi, rcx
		jb							loop_1000h_End

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm13, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0h
			AVX512CSA_Mem			zmm15, zmm4, zmm12, zmm13, zmm31, 1C0h
				AVX512CSA			zmm17, zmm5, zmm14, zmm15
					AVX512CSA		zmm7, zmm6, zmm16, zmm17

		AVX512PopCntLT_VBMI			zmm7, zmm22, zmm23, rdx, r12
		vpsadbw						zmm7, zmm7, zmm21
		vpaddq						zmm0, zmm0, zmm7
		add							rax, rdx
		jmp							loop_1000h

loop_1000h_End:
		AVX512CSA					zmm8, zmm1, zmm24, zmm25
		AVX512CSA					zmm9, zmm1, zmm26, zmm27
			AVX512CSA				zmm10, zmm2, zmm8, zmm9
		AVX512CSA					zmm8, zmm1, zmm28, zmm29
		AVX512CSA					zmm9, zmm1, zmm30, zmm31
			AVX512CSA				zmm11, zmm2, zmm8, zmm9
				AVX512CSA			zmm13, zmm3, zmm10, zmm11
					AVX512CSA		zmm15, zmm4, zmm12, zmm13
						AVX512CSA	zmm17, zmm5, zmm14, zmm15
						AVX512CSA	zmm7, zmm6, zmm16, zmm17

		AVX512PopCntLT_VBMI			zmm7, zmm22, zmm23, rdx, r12
		vpaddb						zmm7, zmm7, zmm7
		add							rdx, rdx
		vpsllq						zmm0, zmm0, 6
		shl							rax, 6

		AVX512PopCntLT_VBMI			zmm6, zmm22, zmm23, rbx, r12
		vpaddb						zmm7, zmm7, zmm6
		add							rdx, rbx
		vpsadbw						zmm7, zmm7, zmm21
		vpsllq						zmm7, zmm7, 5
		shl							rdx, 5
		vpaddq						zmm0, zmm0, zmm7
		add							rax, rdx

		vpxor						xmm15, xmm15, xmm15

		AVX512PopCntLT_VBMI			zmm5, zmm22, zmm23, rdx, r12
		vpsllw						zmm5, zmm5, 4
		shl							rdx, 4
		vpaddb						zmm15, zmm15, zmm5
		add							rax, rdx

		AVX512PopCntLT_VBMI			zmm4, zmm22, zmm23, rdx, r12
		vpsllw						zmm4, zmm4, 3
		lea							rax, [rax + rdx * 8]
		vpaddb						zmm15, zmm15, zmm4

		AVX512PopCntLT_VBMI			zmm3, zmm22, zmm23, rdx, r12
		vpsllw						zmm3, zmm3, 2
		lea							rax, [rax + rdx * 4]
		vpaddb						zmm15, zmm15, zmm3

		AVX512PopCntLT_VBMI			zmm2, zmm22, zmm23, rdx, r12
		vpsllw						zmm2, zmm2, 1
		lea							rax, [rax + rdx * 2]
		vpaddb						zmm15, zmm15, zmm2

		AVX512PopCntLT_VBMI			zmm1, zmm22, zmm23, rdx, r12
		vpaddb						zmm15, zmm15, zmm1
		add							rax, rdx
		vpsadbw						zmm15, zmm15, zmm21
		vpaddq						zmm0, zmm0, zmm15

		test						rdi, rdi
		jz							collect
align 16
remain_400_fffh:
		mov							rcx, 10h * 40h
		cmp							rdi, rcx
		jb							remain_100_3ffh

		vpxor						xmm1, xmm1, xmm1				;ones
		vpxor						xmm2, xmm2, xmm2				;twos
		vpxor						xmm3, xmm3, xmm3				;fours
		vpxor						xmm4, xmm4, xmm4				;eights
		vpxor						xmm5, xmm5, xmm5				;sixteens
																	;zmm8 twosA
																	;zmm9 twosB
																	;zmm10 foursA
																	;zmm11 foursB
																	;zmm12 eightsA
																	;zmm13 eightsB

		vmovdqu64					zmm24, zmmword ptr [rsi]
		vmovdqu64					zmm25, zmmword ptr [rsi + 040h]
		vmovdqu64					zmm26, zmmword ptr [rsi + 080h]
		vmovdqu64					zmm27, zmmword ptr [rsi + 0C0h]
		vmovdqu64					zmm28, zmmword ptr [rsi + 100h]
		vmovdqu64					zmm29, zmmword ptr [rsi + 140h]
		vmovdqu64					zmm30, zmmword ptr [rsi + 180h]
		vmovdqu64					zmm31, zmmword ptr [rsi + 1C0h]

align 16
remain_400_fffh_Loop:

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm12, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 0200h
		vmovdqu64					zmm31, zmmword ptr [rsi + 3C0h]

		sub							rdi, rcx
		add							rsi, rcx
		cmp							rdi, rcx
		jb							remain_400_fffh_End

		AVX512CSA_3Level			zmm1, zmm2, zmm3, zmm8, zmm9, zmm10, zmm11, zmm13, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, zmm31, zmm24, zmm25, zmm26, zmm27, zmm28, zmm29, zmm30, 000h
					AVX512CSA_Mem	zmm5, zmm4, zmm12, zmm13, zmm31, 1C0h
		
		AVX512PopCntLT_VBMI			zmm5, zmm22, zmm23, rdx, r12
		vpsadbw						zmm5, zmm5, zmm21
		vpsllq						zmm5, zmm5, 4
		shl							rdx, 4
		vpaddq						zmm0, zmm0, zmm5
		add							rax, rdx

		jmp							remain_400_fffh_Loop

remain_400_fffh_End:
		AVX512CSA					zmm8, zmm1, zmm24, zmm25
		AVX512CSA					zmm9, zmm1, zmm26, zmm27
			AVX512CSA				zmm10, zmm2, zmm8, zmm9
		AVX512CSA					zmm8, zmm1, zmm28, zmm29
		AVX512CSA					zmm9, zmm1, zmm30, zmm31
			AVX512CSA				zmm11, zmm2, zmm8, zmm9
				AVX512CSA			zmm13, zmm3, zmm10, zmm11
					AVX512CSA		zmm5, zmm4, zmm12, zmm13

		vpxor						xmm15, xmm15, xmm15

		AVX512PopCntLT_VBMI			zmm5, zmm22, zmm23, rdx, r12
		vpsllw						zmm5, zmm5, 4
		shl							rdx, 4
		vpaddb						zmm15, zmm15, zmm5
		add							rax, rdx

		AVX512PopCntLT_VBMI			zmm4, zmm22, zmm23, rdx, r12
		vpsllw						zmm4, zmm4, 3
		vpaddb						zmm15, zmm15, zmm4
		lea							rax, [rax + rdx * 8]

		AVX512PopCntLT_VBMI			zmm3, zmm22, zmm23, rdx, r12
		vpsllw						zmm3, zmm3, 2
		lea							rax, [rax + rdx * 4]
		vpaddb						zmm15, zmm15, zmm3

		AVX512PopCntLT_VBMI			zmm2, zmm22, zmm23, rdx, r12
		vpsllw						zmm2, zmm2, 1
		lea							rax, [rax + rdx * 2]
		vpaddb						zmm15, zmm15, zmm2

		AVX512PopCntLT_VBMI			zmm1, zmm22, zmm23, rdx, r12
		vpaddb						zmm15, zmm15, zmm1
		add							rax, rdx
		vpsadbw						zmm15, zmm15, zmm21
		vpaddq						zmm0, zmm0, zmm15

		test						rdi, rdi
		jz							collect

align 16
remain_100_3ffh:
		mov							rcx, 100h
		vpxor						xmm5, xmm5, xmm5
		vpxor						xmm6, xmm6, xmm6

align 16
remain_100_3ffh_Loop:
		cmp							rdi, rcx
		jb							remain_40_ffh
		
		vmovdqu64					zmm1, zmmword ptr [rsi]
		vmovdqu64					zmm2, zmmword ptr [rsi + 040h]
		vmovdqu64					zmm3, zmmword ptr [rsi + 080h]
		vmovdqu64					zmm4, zmmword ptr [rsi + 0C0h]

		vpmovb2m					k1, zmm1
		vpermi2b					zmm1, zmm22, zmm23
		vpmovb2m					k2, zmm2
		vpermi2b					zmm2, zmm22, zmm23
		vpmovb2m					k3, zmm3
		vpermi2b					zmm3, zmm22, zmm23
		vpmovb2m					k4, zmm4
		vpermi2b					zmm4, zmm22, zmm23

		kmovq						[ktemp + 00h], k1
		vpaddb						zmm1, zmm1, zmm2
		kmovq						[ktemp + 08h], k2
		vpaddb						zmm3, zmm3, zmm4
		kmovq						[ktemp + 10h], k3
		vpaddb						zmm5, zmm5, zmm1
		kmovq						[ktemp + 18h], k4
		vpaddb						zmm6, zmm6, zmm3
		popcnt						r8, [ktemp + 00h]
		popcnt						r9, [ktemp + 08h]
		popcnt						r10, [ktemp + 10h]
		popcnt						r11, [ktemp + 18h]

		add							r8, r9
		add							r10, r11
		add							r8, r10
		add							rax, r8

		add							rsi, rcx
		sub							rdi, rcx
		jmp							remain_100_3ffh_Loop

remain_40_ffh:
		vpsadbw						zmm5, zmm5, zmm21
		vpsadbw						zmm6, zmm6, zmm21
		vpaddq						zmm0, zmm0, zmm5
		vpaddq						zmm0, zmm0, zmm6

		vpxor						xmm8, xmm8, xmm8
		mov							rcx, 40h
align 16
remain_40_ffh_Loop:
		cmp							rdi, rcx
		jb							remain_0_3fh
		vmovdqu64					zmm6, zmmword ptr [rsi]
		vpmovb2m					k1, zmm6
		vpermi2b					zmm6, zmm22, zmm23
		kmovq						[ktemp + 00h], k1
		vpaddb						zmm8, zmm6, zmm8
		popcnt						rdx, [ktemp + 00h]
		add							rsi, rcx
		sub							rdi, rcx
		add							rax, rdx
		jmp							remain_40_ffh_Loop

remain_0_3fh:
		test						rdi, rdi
		jz							collect_0_ffh
		mov							rdx, -1
		bzhi						rdx, rdx, rdi
		kmovq						k1, rdx
		vmovdqu8					zmm6 {k1}{z}, zmmword ptr [rsi]
		vpmovb2m					k2, zmm6
		vpermi2b					zmm6, zmm22, zmm23
		kmovq						[ktemp + 00h], k2
		vpaddb						zmm8, zmm6, zmm8
		popcnt						rdx, [ktemp + 00h]
		add							rax, rdx

collect_0_ffh:
		vpsadbw						zmm8, zmm8, zmm21
		vpaddq						zmm0, zmm0, zmm8
align 16
collect:
		CollectZmm0					rbx
		add							rax, rbx

		tail						1, timemode
PROCNAME endp
endm

VPopCntKNM_M macro PROCNAME, timemode
PROCNAME	proc
; in   - rax popcnt counter
;      - rsi memAddr
;      - rdi memLen
; out  - rax +popcnt
;      - rdi 0
; used - rbx, rcx, rdx, r9, r10, r11, r12, r13, zmm0 - zmm8
		head								1, timemode					;rbp TSCstart, rsi: buf, rdi: size

		vpxor								xmm0, xmm0, xmm0
		xor									eax, eax
		mov									rcx, 200h

align 16
loop_200h:
		cmp									rdi, rcx
		jb									remain_40_1ffh
		db	62h, 0F2h, 0FDh, 48h, 55h, 0Eh								;vpopcntq	zmm1, zmmword ptr [rsi]
		db	62h, 0F2h, 0FDh, 48h, 55h, 56h, 01h							;vpopcntq	zmm2, zmmword ptr [rsi + 040h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 5Eh, 02h							;vpopcntq	zmm3, zmmword ptr [rsi + 080h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 66h, 03h							;vpopcntq	zmm4, zmmword ptr [rsi + 0C0h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 6Eh, 04h							;vpopcntq	zmm5, zmmword ptr [rsi + 100h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 76h, 05h							;vpopcntq	zmm6, zmmword ptr [rsi + 140h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 7Eh, 06h							;vpopcntq	zmm7, zmmword ptr [rsi + 180h]
		db	62h, 072h, 0FDh, 48h, 55h, 46h, 07h							;vpopcntq	zmm8, zmmword ptr [rsi + 1C0h]
		vpaddq								zmm1, zmm1, zmm2
		vpaddq								zmm3, zmm3, zmm4
		vpaddq								zmm5, zmm5, zmm6
		vpaddq								zmm7, zmm7, zmm8
		vpaddq								zmm1, zmm1, zmm3
		vpaddq								zmm5, zmm5, zmm7
		vpaddq								zmm1, zmm1, zmm5
		vpaddq								zmm0, zmm0, zmm1
		sub									rdi, rcx
		add									rsi, rcx
		jmp									loop_200h

align 16
remain_40_1ffh:
		mov									rcx, 40h
align 16
remain_40_1ffh_Loop:
		cmp									rdi, rcx
		jb									remain_0_3fh
		db	62h, 0F2h, 0FDh, 48h, 55h, 0Eh								;vpopcntq	zmm1, zmmword ptr [rsi]
		vpaddq								zmm0, zmm0, zmm1
		add									rsi, rcx
		sub									rdi, rcx
		jmp									remain_40_1ffh_Loop


remain_0_3fh:
		CollectZmm0							rax
		test								rdi, rdi
		jz									ready

		HWPopCntM
ready:
		tail								1, timemode
PROCNAME endp
endm

VPopCntICL_M macro PROCNAME, timemode
PROCNAME	proc
; in   - rax popcnt counter
;      - rsi memAddr
;      - rdi memLen
; out  - rax +popcnt
;      - rdi 0
; used - rcx, rdx, zmm0 - zmm8, k1
		head								1, timemode					;rbp TSCstart, rsi: buf, rdi: size

		vpxor								xmm0, xmm0, xmm0
		mov									rcx, 200h

align 16
loop_200h:
		cmp									rdi, rcx
		jb									remain_40_1ffh
		db	62h, 0F2h, 0FDh, 48h, 55h, 0Eh								;vpopcntq	zmm1, zmmword ptr [rsi]			;4 latency, .5 throughput assumed, like VPLZCNTQ
		db	62h, 0F2h, 0FDh, 48h, 55h, 56h, 01h							;vpopcntq	zmm2, zmmword ptr [rsi + 040h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 5Eh, 02h							;vpopcntq	zmm3, zmmword ptr [rsi + 080h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 66h, 03h							;vpopcntq	zmm4, zmmword ptr [rsi + 0C0h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 6Eh, 04h							;vpopcntq	zmm5, zmmword ptr [rsi + 100h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 76h, 05h							;vpopcntq	zmm6, zmmword ptr [rsi + 140h]
		db	62h, 0F2h, 0FDh, 48h, 55h, 7Eh, 06h							;vpopcntq	zmm7, zmmword ptr [rsi + 180h]
		db	62h, 072h, 0FDh, 48h, 55h, 46h, 07h							;vpopcntq	zmm8, zmmword ptr [rsi + 1C0h]
		vpaddq								zmm1, zmm1, zmm2
		vpaddq								zmm3, zmm3, zmm4
		vpaddq								zmm5, zmm5, zmm6
		vpaddq								zmm7, zmm7, zmm8
		vpaddq								zmm1, zmm1, zmm3
		vpaddq								zmm5, zmm5, zmm7
		vpaddq								zmm1, zmm1, zmm5
		vpaddq								zmm0, zmm0, zmm1
		add									rsi, rcx
		sub									rdi, rcx
		jmp									loop_200h

align 16
remain_40_1ffh:
		mov									rcx, 40h
align 16
remain_40_1ffh_Loop:
		cmp									rdi, rcx
		jb									remain_0_3fh
		db	62h, 0F2h, 0FDh, 48h, 55h, 0Eh								;vpopcntq	zmm1, zmmword ptr [rsi]
		vpaddq								zmm0, zmm0, zmm1
		add									rsi, rcx
		sub									rdi, rcx
		jmp									remain_40_1ffh_Loop

remain_0_3fh:
		test								rdi, rdi
		jz									collect
		mov									rdx, -1
		bzhi								rdx, rdx, rdi				;bzhi works in range 0-255
		kmovq								k1, rdx
		vpxor								xmm2, xmm2, xmm2
		db	62h, 0F2h, 7Dh, 0C9h, 54h, 0Eh								;vpopcntb	zmm1 {k1}{z}, zmmword ptr [rsi]
		vpsadbw								zmm1, zmm1, zmm2
		vpaddq								zmm0, zmm0, zmm1
		xor									rdi, rdi

collect:
		CollectZmm0							rax

		tail								1, timemode
PROCNAME endp
endm

PopCntNHM_M				PopCntNHM,				0
PopCntNHM_M				PopCntNHM_Timed,		1
VPopCntHSW_M			VPopCntHSW,				0
VPopCntHSW_M			VPopCntHSW_Timed,		1
VPopCntKNL_M			VPopCntKNL,				0
VPopCntKNL_M			VPopCntKNL_Timed,		1
VPopCntSKX_M			VPopCntSKX,				0
VPopCntSKX_M			VPopCntSKX_Timed,		1
VPopCntCNL_M			VPopCntCNL,				0
VPopCntCNL_M			VPopCntCNL_Timed,		1
VPopCntKNM_M			VPopCntKNM,				0
VPopCntKNM_M			VPopCntKNM_Timed,		1
VPopCntICL_M			VPopCntICL,				0
VPopCntICL_M			VPopCntICL_Timed,		1

VPopCntSKX_YMM_M		VPopCntSKX_YMM,			0
VPopCntSKX_YMM_M		VPopCntSKX_YMM_Timed,	1

end