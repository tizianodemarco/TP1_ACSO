#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include "shell.h" 
#include "memory.h"
#include <stddef.h>

// Definición de opcodes posibles
#define ADDS_IMM_OP 0b10110001   
#define ADDS_EXT_OP 0b10101011000
#define SUBS_EXT_OP 0b11101011000
#define HLT_OP 0b11010100010  
#define SUBS_IMM_OP  0b11110001
#define ANDS_SHIFTED_REG_OP 0b11101010000
#define EOR_SHIFTED_REG_OP  0b11001010000
#define ORR_SHIFTED_REG_OP  0b10101010000
#define B_OP 0b000101
#define BR_OP 0b11010110000
#define B_COND_OP 0b01010100
#define LS_OP 0b110100110          
#define STUR_OP 0b11111000000
#define STURB_OP 0b00111000000
#define STURH_OP 0b01111000000
#define LDUR_OP 0b11111000010
#define LDURB_OP 0b00111000010
#define LDURH_OP 0b01111000010
#define MOVZ_OP 0b1101001010
#define ADD_EXT_OP 0b10001011000
#define ADD_IMM_OP 0b10010001
#define MUL_OP 0b10011011000
#define CBZ_OP 0b10110100
#define CBNZ_OP 0b10110101

// Definición de funciones auxiliares
typedef enum { AND_OP, ORR_OP, EOR_OP } OpType;
void adds_subs_imm(uint32_t instr, bool add);
void adds_subs_ext(uint32_t instr, bool add);
void ands_eor_orr(uint32_t instr, OpType option, bool flags_update);
int64_t signExtend(uint64_t value, int bitWidth);
uint64_t mem_read_64(uint64_t address);
uint16_t mem_read_16(uint64_t address);
uint8_t mem_read_8(uint64_t address);
uint64_t ExtendReg(int Rm, int extend, int shift);

// Modificar la definición del opcode para ADD (Immediate)
// Usamos una máscara y un valor para comparar en lugar de un valor exacto
#define ADD_IMM_OP_MASK 0xFF000000
#define ADD_IMM_OP_VAL  0x91000000

void process_instruction(){
    uint32_t instr = mem_read_32(CURRENT_STATE.PC); 
    uint32_t opcode = 0;
    int matched = 0;

    // Chequeo del opcode input
    for (int size = 11; size >= 6; size--) {
        opcode = (instr >> (32 - size)) & ((1 << size) - 1);
        
        // Verificación dinámica para ADD (Immediate)
        if ((instr & ADD_IMM_OP_MASK) == ADD_IMM_OP_VAL) {
            opcode = ADD_IMM_OP;
            matched = 1;
            break;
        }
        
        if (opcode == ADDS_IMM_OP || opcode == HLT_OP || opcode == ADDS_EXT_OP || 
            opcode == SUBS_IMM_OP || opcode == SUBS_EXT_OP || opcode == ANDS_SHIFTED_REG_OP || 
            opcode == EOR_SHIFTED_REG_OP || opcode == ORR_SHIFTED_REG_OP || opcode == B_OP || 
            opcode == BR_OP || opcode == B_COND_OP || opcode == LS_OP || opcode == STUR_OP || 
            opcode == STURB_OP || opcode == STURH_OP || opcode == LDUR_OP || opcode == LDURH_OP || 
            opcode == LDURB_OP || opcode == MOVZ_OP || opcode == ADD_EXT_OP || 
            opcode == MUL_OP || opcode == CBZ_OP || opcode == CBNZ_OP) {
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
        adds_subs_imm(instr, true);
    }

    // SUBS (immediate)
    if (opcode == SUBS_IMM_OP){
        adds_subs_imm(instr, false);
    }

    // ADDS (extended register)
    if (opcode == ADDS_EXT_OP) {
        adds_subs_ext(instr, true);
    }

    // SUBS (extended register)
    if (opcode == SUBS_EXT_OP){
        adds_subs_ext(instr, false);
    }

    // HLT
    if (opcode == HLT_OP) {
        RUN_BIT = 0;
    }

    // ANDS (shifted register)
    if (opcode == ANDS_SHIFTED_REG_OP){
        ands_eor_orr(instr, AND_OP, true);
    }

    // EOR (shifted register)
    if (opcode == EOR_SHIFTED_REG_OP ){
        ands_eor_orr(instr, EOR_OP, false);
    }

    // ORR (shifted register)
    if (opcode == ORR_SHIFTED_REG_OP){
        ands_eor_orr(instr, ORR_OP, false);
    }

    // B target
    if (opcode == B_OP){
        int32_t imm26 = instr & 0x03FFFFFF;
        int64_t offset = ((int64_t)(imm26 << 6)) >> 4;
        NEXT_STATE.PC = CURRENT_STATE.PC + offset;
        return;
    }

    // BR X
    if (opcode == BR_OP){
        uint64_t Rn = (instr >> 5) & 0b11111;
        uint64_t target = CURRENT_STATE.REGS[Rn];
        NEXT_STATE.PC = target;
        return;
    }

    // B.COND 
    if (opcode == B_COND_OP){
        uint64_t cond = instr & 0b1111;
        uint32_t imm19 = (instr >> 5) & 0b1111111111111111111;    
        int64_t offset = ((int64_t)(imm19 << 2) << 45) >> 45;
        bool take_branch = false;

        switch (cond){
            case 0b0000:    // BEQ - Equal
                take_branch = CURRENT_STATE.FLAG_Z;
                break;

            case 0b0001:    // BNE - Not Equal
                take_branch = !CURRENT_STATE.FLAG_Z;
                break;

            case 0b1011:    // BLT - Less Than (simplificado a N=1)
                take_branch = CURRENT_STATE.FLAG_N;
                break;

            case 0b1010:    // BGE - Greater Than or Equal (simplificado a N=0)
                take_branch = !CURRENT_STATE.FLAG_N;
                break;

            case 0b1101:    // BLE - Less Than or Equal (simplificado a Z=1 || N=1)
                take_branch = CURRENT_STATE.FLAG_Z || CURRENT_STATE.FLAG_N;
                break;
                
            case 0b1100:    // BGT - Greater Than (simplificado a Z=0 && N=0)
                take_branch = !CURRENT_STATE.FLAG_Z && !CURRENT_STATE.FLAG_N;
                break;
        }

        if (take_branch) {
            NEXT_STATE.PC = CURRENT_STATE.PC + offset;
            return;
        }
    }

    // LSL/LSR
    if (opcode == LS_OP){
        uint32_t Rd = instr & 0b11111;        
        uint32_t Rn = (instr >> 5) & 0b11111;   
        uint32_t immr = (instr >> 16) & 0b111111;  
        uint32_t imms = (instr >> 10) & 0b111111;  

        uint64_t operand = CURRENT_STATE.REGS[Rn];
        uint64_t result = 0;

        if (imms + 1 == immr) {
            result = operand << (-immr % 64);  // LSL
        } else if (imms == 0b111111) {
            result = operand >> immr;  // LSR
        }
        NEXT_STATE.REGS[Rd] = result;
    }

    // STUR/STURB/STURH
    if (opcode == STUR_OP || opcode == STURB_OP || opcode == STURH_OP){
        uint64_t Rt = instr & 0b11111;
        uint64_t Rn = (instr >> 5) & 0b11111;
        uint64_t imm9 = (instr >> 12) & 0b111111111;
        int64_t offset = signExtend(imm9, 9);

        switch(opcode){
            case STUR_OP: {
                uint64_t base_address = CURRENT_STATE.REGS[Rn];
                uint64_t address = base_address + offset;
                uint64_t data = CURRENT_STATE.REGS[Rt];
                
                mem_write_32(address, data & 0xFFFFFFFF);         
                mem_write_32(address + 4, (data >> 32) & 0xFFFFFFFF); 
                break;
            }
            
            case STURB_OP: {
                uint64_t address = CURRENT_STATE.REGS[Rn] + offset;
                uint8_t byte = CURRENT_STATE.REGS[Rt] & 0b11111111; 
                uint32_t current = mem_read_32(address & ~0x3); 
                int shift = (address & 0x3) * 8;
                current &= ~(0xFF << shift);
                current |= (byte << shift);
                mem_write_32(address & ~0x3, current);
                break;
            }
            
            case STURH_OP: {
                uint64_t address = CURRENT_STATE.REGS[Rn] + offset;
                uint16_t halfword = CURRENT_STATE.REGS[Rt] & 0xFFFF;
                mem_write_32(address & ~0x3, 
                    (mem_read_32(address & ~0x3) & ~(0xFFFF << ((address & 0x3) * 8))) | 
                    (halfword << ((address & 0x3) * 8)));
                break;
            }
        }
    }

    // LDUR/LDURB/LDURH
    if (opcode == LDUR_OP || opcode == LDURB_OP || opcode == LDURH_OP) {
        uint32_t Rt = instr & 0b11111;        
        uint32_t Rn = (instr >> 5) & 0b11111;  
        int32_t imm9 = (instr >> 12) & 0x1FF;  
        // Extensión de signo para imm9
        imm9 = (imm9 & 0x100) ? (imm9 | 0xFFFFFF00) : imm9;

        uint64_t address = CURRENT_STATE.REGS[Rn];
        address += imm9;

        if (opcode == LDUR_OP) {
            // LDUR (64 bits)
            uint32_t lower = mem_read_32(address);
            uint32_t upper = mem_read_32(address + 4);
            uint64_t data = ((uint64_t)upper << 32) | lower;
            NEXT_STATE.REGS[Rt] = data;
        } 
        else if (opcode == LDURH_OP) {
            // LDURH (16 bits) - Extensión de cero
            uint16_t halfword = mem_read_16(address);
            NEXT_STATE.REGS[Rt] = (uint64_t)halfword;
        } 
        else if (opcode == LDURB_OP) {
            // LDURB (8 bits) - Extensión de cero
            uint8_t byte = mem_read_8(address);
            NEXT_STATE.REGS[Rt] = (uint64_t)byte;
        }
    }

    // MOVZ
    if (opcode == MOVZ_OP){
        uint32_t imm16 = (instr >> 5) & 0xFFFF; 
        uint32_t Rd = instr & 0b11111;
        NEXT_STATE.REGS[Rd] = (uint64_t)imm16;
    }

    // ADD (Extended Register)
    if (opcode == ADD_EXT_OP) {
        uint32_t Rd = instr & 0x1F;  
        uint32_t Rn = (instr >> 5) & 0x1F;  
        uint32_t Rm = (instr >> 16) & 0x1F; 
        
        uint64_t operand1 = CURRENT_STATE.REGS[Rn];
        uint64_t operand2 = CURRENT_STATE.REGS[Rm];
        
        uint64_t result = operand1 + operand2;
        NEXT_STATE.REGS[Rd] = result;
    }

    // ADD (Immediate)
    if (opcode == ADD_IMM_OP) {
        uint32_t Rd = instr & 0x1F; 
        uint32_t Rn = (instr >> 5) & 0x1F; 
        uint32_t imm12 = (instr >> 10) & 0xFFF; 
        uint32_t shift = (instr >> 22) & 0x3;

        uint64_t operand1 = CURRENT_STATE.REGS[Rn];
        uint64_t imm = 0;
        
        // Solo implementamos shift 00 (sin shift) y 01 (LSL #12)
        if (shift == 0) {
            imm = imm12;
        } 
        else if (shift == 1) {
            imm = ((uint64_t)imm12 << 12);
        }
        // No implementamos ReservedValue (shift == 2 o shift == 3)
        
        uint64_t result = operand1 + imm;
        NEXT_STATE.REGS[Rd] = result;
    }

    // MUL
    if (opcode == MUL_OP){
        uint32_t Rd = instr & 0b11111;
        uint32_t Rn = (instr >> 5) & 0b11111;
        uint32_t Rm = (instr >> 16) & 0b11111;

        uint64_t operand1 = CURRENT_STATE.REGS[Rn];
        uint64_t operand2 = CURRENT_STATE.REGS[Rm];
        
        uint64_t result = operand1 * operand2;
        
        NEXT_STATE.FLAG_Z = (result == 0);
        NEXT_STATE.FLAG_N = ((int64_t)result < 0);
        NEXT_STATE.REGS[Rd] = result;
    }

    // CBZ/CBNZ
    if (opcode == CBZ_OP || opcode == CBNZ_OP){
        uint32_t Rt = instr & 0x1F; 
        int32_t imm19 = (instr >> 5) & 0x7FFFF; 

        int64_t offset = signExtend(imm19 << 2, 21); 

        uint64_t operand1 = CURRENT_STATE.REGS[Rt]; 
        
        if (opcode == CBZ_OP) {  
            if (operand1 == 0) {
                NEXT_STATE.PC = CURRENT_STATE.PC + offset;
                return; 
            }
        } else if (opcode == CBNZ_OP) {  
            if (operand1 != 0) {
                NEXT_STATE.PC = CURRENT_STATE.PC + offset;
                return; 
            }
        }
    }

    // Avanzar PC
    NEXT_STATE.PC = CURRENT_STATE.PC + 4;
}

// Funciones auxiliares

void adds_subs_ext(uint32_t instr, bool add){
    uint32_t Rd = instr & 0b11111;
    uint32_t Rn = (instr >> 5) & 0b11111;
    uint32_t Rm = (instr >> 16) & 0b11111;

    uint64_t operand1 = CURRENT_STATE.REGS[Rn];
    uint64_t operand2 = CURRENT_STATE.REGS[Rm];
    
    uint64_t result = add ? (operand1 + operand2) : (operand1 - operand2);
    
    NEXT_STATE.FLAG_Z = (result == 0);
    NEXT_STATE.FLAG_N = ((int64_t)result < 0);

    if (!add & Rd == 0b11111){              // CMP (Extended Register)
        return;
    }
    NEXT_STATE.REGS[Rd] = result;
}

void adds_subs_imm(uint32_t instr, bool add){
    uint32_t Rd = instr & 0b11111;
    uint32_t Rn = (instr >> 5) & 0b11111;
    uint32_t imm12 = (instr >> 10) & 0b111111111111; 
    uint32_t shift = (instr >> 22) & 0b11;

    if (shift == 0b00) {
        imm12 = (uint64_t)imm12;
    } 
    else if (shift == 0b01) {
        imm12 = ((uint64_t)imm12 << 12);
    }
    uint64_t operand1 = CURRENT_STATE.REGS[Rn];

    uint64_t result = add ? (operand1 + imm12) : (operand1 - imm12);

    NEXT_STATE.FLAG_Z = (result == 0);
    NEXT_STATE.FLAG_N = ((int64_t)result < 0);
            
    if ((!add && Rd != 0b11111) || add){        // CMP (Immediate)
        NEXT_STATE.REGS[Rd] = result;
    }
}

void ands_eor_orr(uint32_t instr, OpType option, bool flags_update) {
    uint64_t Rd = instr & 0b11111;
    uint64_t Rn = (instr >> 5) & 0b11111;
    uint64_t Rm = (instr >> 16) & 0b11111;

    uint64_t operand1 = CURRENT_STATE.REGS[Rn];
    uint64_t operand2 = CURRENT_STATE.REGS[Rm];

    uint64_t result = 0; 

    switch (option) {
        case EOR_OP: result = operand1 ^ operand2; break;
        case ORR_OP: result = operand1 | operand2; break;
        case AND_OP: result = operand1 & operand2; break;
    }

    if (flags_update) {
        NEXT_STATE.FLAG_Z = (result == 0);
        NEXT_STATE.FLAG_N = ((int64_t)result < 0);
    }

    NEXT_STATE.REGS[Rd] = result;
}

int64_t signExtend(uint64_t value, int bitWidth) {
    int64_t mask = 1LL << (bitWidth - 1);
    return (value ^ mask) - mask;
}

uint64_t mem_read_64(uint64_t address) {
    uint64_t lower = mem_read_32(address);
    uint64_t upper = mem_read_32(address + 4);
    return (upper << 32) | lower;
}

uint16_t mem_read_16(uint64_t address) {
    uint32_t word = mem_read_32(address & ~3); 
    int offset = address & 3;

    if (offset == 3) {  // Caso problemático
        uint32_t next_word = mem_read_32((address & ~3) + 4);
        return ((word >> 24) & 0xFF) | ((next_word & 0xFF) << 8);
    }
    return (word >> (offset * 8)) & 0xFFFF;
}

uint8_t mem_read_8(uint64_t address) {
    uint32_t word = mem_read_32(address & ~3); 
    int offset = address & 3; 
    return (word >> (offset * 8)) & 0xFF;
}

uint64_t ExtendReg(int Rm, int extend, int shift) {
    uint64_t result;
    switch (extend) {
        case 0: result = (int64_t)(CURRENT_STATE.REGS[Rm]) << shift; break; 
        case 1: result = (uint64_t)(CURRENT_STATE.REGS[Rm]) << shift; break;  
        case 2: result = (CURRENT_STATE.REGS[Rm]) >> shift; break; 
        default:result = CURRENT_STATE.REGS[Rm]; break; 
    }
    return result;
}