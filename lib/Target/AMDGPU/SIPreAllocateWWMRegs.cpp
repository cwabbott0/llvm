//===-- SIPreAllocateWWMRegs.cpp - WWM Register Pre-allocation ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//


#include "AMDGPU.h"
#include "AMDGPUSubtarget.h"
#include "SIInstrInfo.h"
#include "SIRegisterInfo.h"
#include "SIMachineFunctionInfo.h"
#include "MCTargetDesc/AMDGPUMCTargetDesc.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/VirtRegMap.h"
#include "llvm/CodeGen/LiveInterval.h"
#include "llvm/CodeGen/LiveIntervals.h"
#include "llvm/CodeGen/LiveRegMatrix.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/RegisterClassInfo.h"

using namespace llvm;

#define DEBUG_TYPE "si-pre-allocate-wwm-regs"

namespace {

class SIPreAllocateWWMRegs : public MachineFunctionPass {
private:
  const SIRegisterInfo *TRI;
  MachineRegisterInfo *MRI;
  LiveIntervals *LIS;
  LiveRegMatrix *Matrix;
  VirtRegMap *VRM;
  RegisterClassInfo RegClassInfo;

  std::vector<unsigned> RegsToRewrite;

public:
  static char ID;

  SIPreAllocateWWMRegs() : MachineFunctionPass(ID) {
    initializeSIPreAllocateWWMRegsPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &MF) override;

  StringRef getPassName() const override { return "SI Pre-allocate WWM Registers"; }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<LiveIntervals>();
    AU.addPreserved<LiveIntervals>();
    AU.addRequired<VirtRegMap>();
    AU.addRequired<LiveRegMatrix>();
    AU.addPreserved<SlotIndexes>();
    AU.setPreservesCFG();
    MachineFunctionPass::getAnalysisUsage(AU);
  }

private:
  bool processDef(MachineOperand &MO);
  void rewriteRegs(MachineFunction &MF);
};

} // End anonymous namespace.

INITIALIZE_PASS_BEGIN(SIPreAllocateWWMRegs, DEBUG_TYPE,
                "SI Pre-allocate WWM Registers", false, false)
INITIALIZE_PASS_DEPENDENCY(LiveIntervals)
INITIALIZE_PASS_DEPENDENCY(VirtRegMap)
INITIALIZE_PASS_DEPENDENCY(LiveRegMatrix)
INITIALIZE_PASS_END(SIPreAllocateWWMRegs, DEBUG_TYPE,
                "SI Pre-allocate WWM Registers", false, false)

char SIPreAllocateWWMRegs::ID = 0;

char &llvm::SIPreAllocateWWMRegsID = SIPreAllocateWWMRegs::ID;

FunctionPass *llvm::createSIPreAllocateWWMRegsPass() {
  return new SIPreAllocateWWMRegs();
}

bool SIPreAllocateWWMRegs::processDef(MachineOperand &MO) {
  if (!MO.isReg())
    return false;

  unsigned Reg = MO.getReg();

  if (!TRI->isVGPR(*MRI, Reg))
    return false;

  if (TRI->isPhysicalRegister(Reg))
    return false;

  if (VRM->hasPhys(Reg))
    return false;

  LiveInterval &LI = LIS->getInterval(Reg);

  for (unsigned PhysReg : RegClassInfo.getOrder(MRI->getRegClass(Reg))) {
    if (!MRI->isPhysRegUsed(PhysReg) &&
        Matrix->checkInterference(LI, PhysReg) == LiveRegMatrix::IK_Free) {
      Matrix->assign(LI, PhysReg);
      assert(PhysReg != 0);
      RegsToRewrite.push_back(Reg);
      return true;
    }
  }

  assert(!"physreg not found for WWM expression");
}

void SIPreAllocateWWMRegs::rewriteRegs(MachineFunction &MF) {
  for (MachineBasicBlock &MBB : MF) {
    for (MachineInstr &MI : MBB) {
      for (MachineOperand &MO : MI.operands()) {
        if (!MO.isReg())
          continue;

        const unsigned VirtReg = MO.getReg();
        if (TRI->isPhysicalRegister(VirtReg))
          continue;

        if (!VRM->hasPhys(VirtReg))
          continue;

        unsigned PhysReg = VRM->getPhys(VirtReg);
        const unsigned SubReg = MO.getSubReg();
        if (SubReg != 0) {
          PhysReg = TRI->getSubReg(PhysReg, SubReg);
          MO.setSubReg(0);
        }

        MO.setReg(PhysReg);
        MO.setIsRenamable(false);
      }
    }
  }

  SIMachineFunctionInfo *MFI = MF.getInfo<SIMachineFunctionInfo>();

  for (unsigned Reg : RegsToRewrite) {
    LIS->removeInterval(Reg);

    unsigned PhysReg = VRM->getPhys(Reg);
    assert(PhysReg != 0);
    MFI->ReserveWWMRegister(PhysReg);
  }

  RegsToRewrite.clear();

  // Update the set of reserved registers to include WWM ones.
  MRI->freezeReservedRegs(MF);
}

bool SIPreAllocateWWMRegs::runOnMachineFunction(MachineFunction &MF) {
  LLVM_DEBUG(dbgs() << "SIPreAllocateWWMRegs: function " << MF.getName() << "\n");

  const GCNSubtarget &ST = MF.getSubtarget<GCNSubtarget>();

  const SIInstrInfo *TII = ST.getInstrInfo();
  TRI = &TII->getRegisterInfo();
  MRI = &MF.getRegInfo();

  LIS = &getAnalysis<LiveIntervals>();
  Matrix = &getAnalysis<LiveRegMatrix>();
  VRM = &getAnalysis<VirtRegMap>();

  RegClassInfo.runOnMachineFunction(MF);

  bool RegsAssigned = false;

  // We use a reverse post-order traversal of the control-flow graph to
  // guarantee that we visit definitions in dominance order. Since WWM
  // expressions are guaranteed to never involve phi nodes, and we can only
  // escape WWM through the special WWM instruction, this means that this is a
  // perfect elimination order, so we can never do any better.
  ReversePostOrderTraversal<MachineFunction*> RPOT(&MF);

  for (MachineBasicBlock *MBB : RPOT) {
    bool InWWM = false;
    for (MachineInstr &MI : *MBB) {
      if (MI.getOpcode() == AMDGPU::V_SET_INACTIVE_B32)
        RegsAssigned |= processDef(MI.getOperand(0));

      if (MI.getOpcode() == AMDGPU::S_OR_SAVEEXEC_B64 &&
          MI.getOperand(1).isImm() && MI.getOperand(1).getImm() == -1) {
        LLVM_DEBUG(dbgs() << "entering WWM region: " << MI << "\n");
        InWWM = true;
        continue;
      }

      if (MI.getOpcode() == AMDGPU::EXIT_WWM) {
        LLVM_DEBUG(dbgs() << "exiting WWM region: " << MI << "\n");
        InWWM = false;
      }

      if (!InWWM)
        continue;

      LLVM_DEBUG(dbgs() << "processing " << MI);

      for (MachineOperand &DefOpnd : MI.defs()) {
        RegsAssigned |= processDef(DefOpnd);
      }
    }
  }

  if (!RegsAssigned)
    return false;

  rewriteRegs(MF);
  return true;
}

