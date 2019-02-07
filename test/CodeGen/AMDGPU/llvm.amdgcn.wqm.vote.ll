; RUN: llc -march=amdgcn -mcpu=tonga -verify-machineinstrs < %s | FileCheck -enable-var-scope -check-prefix=CHECK %s

;CHECK-LABEL: {{^}}ret:
;CHECK: v_cmp_eq_u32_e32 [[CMP:[^,]+]], v0, v1
;CHECK: s_and_b64 [[AND:[^,]+]], [[CMP]], exec
;CHECK: s_wqm_b64 [[WQM:[^,]+]], [[AND]]
;CHECK: v_cndmask_b32_e64 v0, 0, 1.0, [[WQM]]
define amdgpu_ps float @ret(i32 %v0, i32 %v1) #1 {
main_body:
  %c = icmp eq i32 %v0, %v1
  %w = call i1 @llvm.amdgcn.wqm.vote(i1 %c)
  %r = select i1 %w, float 1.0, float 0.0
  ret float %r
}

;CHECK-LABEL: {{^}}true:
;CHECK: s_wqm_b64
define amdgpu_ps float @true() #1 {
main_body:
  %w = call i1 @llvm.amdgcn.wqm.vote(i1 true)
  %r = select i1 %w, float 1.0, float 0.0
  ret float %r
}

;CHECK-LABEL: {{^}}false:
;CHECK: s_wqm_b64
define amdgpu_ps float @false() #1 {
main_body:
  %w = call i1 @llvm.amdgcn.wqm.vote(i1 false)
  %r = select i1 %w, float 1.0, float 0.0
  ret float %r
}

;CHECK-LABEL: {{^}}kill:
;CHECK: v_cmp_eq_u32_e32 [[CMP:[^,]+]], v0, v1
;CHECK: s_and_b64 [[AND:[^,]+]], [[CMP]], exec
;CHECK: s_wqm_b64 [[WQM:[^,]+]], [[AND]]
;CHECK: s_and_b64 exec, exec, [[WQM]]
;CHECK: s_endpgm
define amdgpu_ps void @kill(i32 %v0, i32 %v1) #1 {
main_body:
  %c = icmp eq i32 %v0, %v1
  %w = call i1 @llvm.amdgcn.wqm.vote(i1 %c)
  call void @llvm.amdgcn.kill(i1 %w)
  ret void
}

;CHECK-LABEL: {{^}}phi_not:
;CHECK: s_xor_b64 [[NOT:[^,]+]], {{[^,]+}}, -1
;CHECK-NEXT: s_and_b64 [[AND:[^,]+]], [[NOT]], exec
;CHECK-NEXT: s_wqm_b64 [[WQM:[^,]+]], [[AND]]
;CHECK-NEXT: v_cndmask_b32_e64 v0, 0, 1.0, [[WQM]]
define amdgpu_ps float @phi_not(i32 %v0, i32 %v1, i32 %v2) {
  %cc = icmp eq i32 %v0, 0

  br i1 %cc, label %if, label %else

  if:
  %tmp1 = icmp ne i32 %v1, 1
  br label %endif

  else:
  %tmp2 = icmp eq i32 %v2, 2
  br label %endif

  endif:
  %sel = phi i1 [ %tmp1, %if ], [ %tmp2, %else ]
  %not = xor i1 %sel, true
  %w = call i1 @llvm.amdgcn.wqm.vote(i1 %not)
  %r = select i1 %w, float 1.0, float 0.0
  ret float %r
}

declare void @llvm.amdgcn.kill(i1) #1
declare i1 @llvm.amdgcn.wqm.vote(i1)

attributes #1 = { nounwind }
