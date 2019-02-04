; RUN: llc -march=amdgcn -mcpu=tonga -verify-machineinstrs < %s | FileCheck -enable-var-scope -check-prefix=CHECK %s

; CHECK-LABEL: {{^}}ret:
; CHECK: s_mov_b64 s[[MASK:\[[0-9]+:[0-9]+\]]], 1
; CHECK-NEXT: v_cndmask_b32_e64 v0, 0, 1.0, s[[MASK]]
define float @ret() #1 {
main_body:
  %w = call i1 @llvm.amdgcn.inverse.ballot(i64 1)
  %r = select i1 %w, float 1.0, float 0.0
  ret float %r
}

; make sure it works for things that wind up in VGPR's
; CHECK-LABEL: {{^}}vgpr:
; CHECK: v_readfirstlane_b32 s[[MASKLO:[0-9]+]], v0
; CHECK-NEXT: v_readfirstlane_b32 s[[MASKHI:[0-9]+]], v1
; CHECK-NEXT: v_cndmask_b32_e64 v0, 0, 1.0, s{{\[}}[[MASKLO]]:[[MASKHI]]{{\]}}
define float @vgpr(i64 %v0_1) {
  %inv = call i1 @llvm.amdgcn.inverse.ballot(i64 %v0_1)
  %r = select i1 %inv, float 1.0, float 0.0
  ret float %r
}

; CHECK-LABEL: {{^}}phi_uniform:
; CHECK: s_cmp_lg_u32 s2, 0
; CHECK: s_cbranch_scc0
; CHECK: v_cndmask_b32_e64 v0, 0, 1.0, s[0:1]
; CHECK: s_branch
; CHECK: s_add_u32 s[[MASKLO:[0-9]+]], s0, 1
; CHECK: s_addc_u32 s[[MASKHI:[0-9]+]], s1, 0
; CHECK: v_cndmask_b32_e64 v0, 0, 1.0, s{{\[}}[[MASKLO]]:[[MASKHI]]{{\]}}
define amdgpu_ps float @phi_uniform(i64 inreg %s0_1, i32 inreg %s2) {
  main_body:
  %cc = icmp ne i32 %s2, 0

  br i1 %cc, label %endif, label %if

  if:
  %tmp = add i64 %s0_1, 1
  br label %endif

  endif:
  %sel = phi i64 [ %s0_1, %main_body], [ %tmp, %if ]

  %inv = call i1 @llvm.amdgcn.inverse.ballot(i64 %sel)
  %r = select i1 %inv, float 1.0, float 0.0
  ret float %r
}

; CHECK-LABEL: {{^}}phi_divergent:
; CHECK: v_cmp_eq_u32_e32 vcc, 0, v0
; CHECK: v_mov_b32_e32 v[[VMASKLO:[0-9]+]], s0
; CHECK: v_mov_b32_e32 v[[VMASKHI:[0-9]+]], s1
; CHECK: s_and_saveexec
; CHECK: s_add_u32
; CHECK: s_addc_u32
; CHECK: v_mov_b32_e32 v[[VMASKLO]],
; CHECK: v_mov_b32_e32 v[[VMASKHI]],
; CHECK: s_or_b64 exec, exec,
; CHECK: v_readfirstlane_b32 s[[SMASKLO:[0-9]+]], v[[VMASKLO]]
; CHECK: v_readfirstlane_b32 s[[SMASKHI:[0-9]+]], v[[VMASKHI]]
; CHECK: v_cndmask_b32_e64 v0, 0, 1.0, s{{\[}}[[SMASKLO]]:[[SMASKHI]]{{\]}}
define amdgpu_ps float @phi_divergent(i32 %v0, i64 inreg %s0_1) {
  main_body:
  %cc = icmp ne i32 %v0, 0

  br i1 %cc, label %endif, label %if

  if:
  %tmp = add i64 %s0_1, 1
  br label %endif

  endif:
  %sel = phi i64 [ %s0_1, %main_body], [ %tmp, %if ]

  %inv = call i1 @llvm.amdgcn.inverse.ballot(i64 %sel)
  %r = select i1 %inv, float 1.0, float 0.0
  ret float %r
}

declare i1 @llvm.amdgcn.inverse.ballot(i64)
declare i64 @llvm.amdgcn.icmp.i32(i32, i32, i32)

