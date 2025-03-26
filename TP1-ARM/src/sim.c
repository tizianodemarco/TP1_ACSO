#include <stdio.h>
#include <assert.h>
#include <string.h>

#include <stdint.h>
#include "shell.h"  

/* execute one instruction here. You should use CURRENT_STATE and modify
    * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
    * access memory. 
    * */

// Definimos opcodes posibles
#define ADDS_IMM_OP 0b10110001   // bits [31:21] - 11 bits
#define ADDS_EXT_OP 0b10101011001
#define HLT_OP       0b11010100000   // bits [31:21] - 11 bits

void process_instruction() {
    uint32_t instr = mem_read_32(CURRENT_STATE.PC); 
    uint32_t opcode = 0;
    int matched = 0;

    // Intentamos con tamaños de opcode decrecientes (desde 11 hasta 6 bits)
    for (int size = 11; size >= 6; size--) {
        opcode = (instr >> (32 - size)) & ((1 << size) - 1);
        if (opcode == ADDS_IMM_OP || opcode == HLT_OP) {
            matched = 1;
            break;
        }
    }

    if (!matched) {
        printf("Instrucción no implementada: 0x%08X\n", instr);
        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
        return;
    }

    // ADDS (immediate)
    if (opcode == ADDS_IMM_OP) {
        uint32_t Rd = instr & 0b11111;
        uint32_t Rn = (instr >> 5) & 0b11111;
        uint32_t imm12 = (instr >> 10) & 0x111111111111; 
        uint32_t shift = (instr >> 22) & 0b11;


        if (shift == 00) {
            imm12 = (uint64_t)imm12;
        } 
        else if (shift == 01) {
            imm12 = ((uint64_t)imm12 << 12);
        }

        uint64_t operand1 = CURRENT_STATE.REGS[Rn];
        uint64_t result = operand1 + imm12;

        NEXT_STATE.REGS[Rd] = result;

        // Actualizamos flags
        NEXT_STATE.FLAG_Z = (result == 0);
        NEXT_STATE.FLAG_N = ((int64_t)result < 0);
    }

    // ADDS (extended register)
    if (opcode == ADDS_EXT_OP) {
        uint32_t Rd = instr & 0b11111;
        uint32_t Rn = (instr >> 5) & 0b11111;
        uint32_t imm3 = (instr >> 10) & 0b111;
        uint32_t option = (instr >> 13) & 0b111;
        uint32_t Rm = (instr >> 16) & 0b11111;

        uint64_t operand1 = CURRENT_STATE.REGS[Rn];
        uint64_t operand2 = CURRENT_STATE.REGS[Rm];

        switch (option) {
            case 0b000: operand2 = (uint8_t)operand2; break; // UXTB
            case 0b001: operand2 = (uint16_t)operand2; break; // UXTH
            case 0b010: operand2 = (uint32_t)operand2; break; // UXTW
            case 0b011: operand2 = (uint64_t)operand2; break; // UXTX / LSL
            case 0b100: operand2 = (int8_t)operand2; break; // SXTB
            case 0b101: operand2 = (int16_t)operand2; break; // SXTH
            case 0b110: operand2 = (int32_t)operand2; break; // SXTW
            case 0b111: operand2 = (int64_t)operand2; break; // SXTX
        }
        
        if (imm3 > 4) {
            return;
        }
        operand2 <<= imm3;

        uint64_t result = operand1 + operand2;
        NEXT_STATE.REGS[Rd] = result;

        NEXT_STATE.FLAG_Z = (result == 0);
        NEXT_STATE.FLAG_N = ((int64_t)result < 0);
    }

    // HLT
    if (opcode == HLT_OP) {
        RUN_BIT = 0;
    }

    // Avanzar PC
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

