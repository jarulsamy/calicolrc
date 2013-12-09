"""
This code based on:
http://www.daniweb.com/software-development/python/code/367871/
assembler-for-little-computer-3-lc-3-in-python

Order of BRanch flags relaxed, BR without flags interpreted as BRnzp
(always).
"""

from __future__ import print_function
from array import array
import os
import sys

def lc_hex(h):
    """ Format the value in the form xFFFF """
    return 'x%04X' % lc_bin(h)

def lc_bin(v):
    """ Truncate any extra bytes """
    return v & 0xFFFF

def sext(binary, bits):
    """ 
    Sign-extend the binary number, check the most significant
    bit 
    """
    neg = binary & (1 << (bits - 1))
    if neg:
        mask = 0
        for i in range(bits, 16):
            mask |= (0b1 << i)
        return (mask | binary)
    else:
        return binary

def plus(v1, v2):
    """
    Add two values together, return a positive or negative value.
    """
    v1 = v1 & 0b1111111111111111
    v2 = v2 & 0b1111111111111111
    if v1 & (0b1 << 15): # neg
        v1 = -(~(v1 - 1) & 0xFFFF)
    if v2 & (0b1 << 15): # neg
        v2 = -(~(v2 - 1) & 0xFFFF)
    return v1 + v2

class LC3(object):
    """
    The LC3 Computer. This object can assemble, disassemble, and execute
    LC3 programs.
    """
    reg_pos = [9, 6, 0]
    flags = {'n': 1 << 11, 'z': 1 << 10, 'p': 1 << 9}
    # instructions:
    instruction_info = {
        'ADD': 0b1 << 12,
        'AND': 0b0101 << 12,
        'BR': 0b0,
        'GETC': (0b1111 << 12) + 0x20,
        'HALT': (0b1111 << 12) + 0x25,
        'IN': (0b1111 << 12) + 0x23,
        'JMP': 0b1100 << 12,
        'JMPT': (0b1100000 << 9) + 1,
        'JSR': 0b01001 << 11,
        'JSRR': 0b010000 << 9,
        'LD': 0b0010 << 12,
        'LDI': 0b1010 << 12,
        'LDR': 0b0110 << 12,
        'LEA': 0b1110 << 12,
        'NOT': (0b1001 << 12) + 0b111111,
        'OUT': (0b1111 << 12) + 0x21,
        'PUTS': (0b1111 << 12) + 0x22,
        'PUTSP': (0b1111 << 12) + 0x24,
        'RET': 0b1100000111000000,
        'RTI': 0b1000 << 12,
        'RTT': 0b1100000111000001,
        'ST': 0b0011 << 12,
        'STI': 0b1011 << 12,
        'STR': 0b0111 << 12,
        'TRAP': 0b1111 << 12,
    }
    # bits of immediate mode field:
    immediate = {
        'ADD': 5,
        'AND': 5,
        'BR': 9,
        'GETC': 0,
        'HALT': 0,
        'IN': 0,
        'JMP': 0,
        'JMPT': 0,
        'JSR': 11,
        'JSRR': 0,
        'LD': 9,
        'LDI': 9,
        'LDR': 6,
        'LEA': 9,
        'NOT': 9,
        'OUT': 0,
        'PUTS': 0,
        'PUTSP': 0,
        'RET': 0,
        'RTI': 0,
        'RTT': 0,
        'ST': 9,
        'STI': 9,
        'STR': 6,
        'TRAP': 8,
        'UNDEFINED': 0,
    }
    # Based on C.2 and C.7 states, and 1 cycle for each memory read
    cycles = {
            0b0000: 5  + 1, # BR
            0b0001: 5  + 1, # ADD
            0b0010: 7  + 3, # LD, + 2 memory reads
            0b0011: 7  + 2, # ST, + 1 memory read, one store
            0b0100: 6  + 1, # JSR
            0b0101: 5  + 1, # AND
            0b0110: 7  + 2, # LDR
            0b0111: 7  + 3, # STR
            0b1000: 12 + 3, # RTI
            0b1001: 5  + 1, # NOT
            0b1010: 9  + 3, # LDI
            0b1011: 9  + 3, # STI
            0b1100: 5  + 1, # JMP and RET
            0b1101: 13 + 1, # RESERVED
            0b1110: 5  + 1, # LEA
            0b1111: 7  + 2, # TRAP
        }

    def __init__(self):
        # Functions for interpreting instructions:
        self.debug = False
        self.apply = {
            0b0000: self.BR,
            0b0001: self.ADD,
            0b0010: self.LD,
            0b0011: self.ST,
            0b0100: self.JSR,
            0b0101: self.AND,
            0b0110: self.LDR,
            0b0111: self.STR,
            0b1000: self.RTI,
            0b1001: self.NOT,
            0b1010: self.LDI,
            0b1011: self.STI,
            0b1100: self.JMP, # and RET
            0b1101: self.RESERVED,
            0b1110: self.LEA,
            0b1111: self.TRAP,
        }
        # Functions for formatting instructions:
        self.format = {
            0b0000: self.BR_format,
            0b0001: self.ADD_format,
            0b0010: self.LD_format,
            0b0011: self.ST_format,
            0b0100: self.JSR_format,
            0b0101: self.AND_format,
            0b0110: self.LDR_format,
            0b0111: self.STR_format,
            0b1000: self.RTI_format,
            0b1001: self.NOT_format,
            0b1010: self.LDI_format,
            0b1011: self.STI_format,
            0b1100: self.JMP_format, # and RET_format
            0b1101: self.RESERVED_format,
            0b1110: self.LEA_format,
            0b1111: self.TRAP_format,
        }
        self.orig = 0x3000
        self.initialize()

    #### The following allow different hardware implementations:
    #### memory, register, nzp, and pc can be implemented in different
    #### means.
    def initialize(self):
        self.source = {}
        self.cycle = 0
        self.cont = False
        self.instruction_count = 0
        self.set_pc(0x3000)
        self.immediate_mask = {}
        for im in self.immediate:
            self.immediate_mask[im] = (1 << self.immediate[im]) - 1
        self.instructions = self.instruction_info.keys()
        self.regs = dict(('R%1i' % r, r) for r in range(8))
        self.labels = {}
        self.label_location = {}
        self.register = {0:0, 1:0, 2:0, 3:0, 4:0, 5:0, 6:0, 7:0}
        self.reset_memory()
        self.reset_registers()
        self.load_os()
        self.reset_registers()

    def reset_memory(self):
        self.memory = array('i', [0] * (1 << 16))

    def load_os(self, filename=None):
        if filename is None:
            filename = os.path.join(os.path.dirname(__file__), "lc3os.asm")
        text = "".join(open(filename).readlines())
        orig = self.debug 
        self.debug = False
        self.assemble(text)
        self.set_memory(0xFE04, 0xFFFF) ## OS_DSR Display Ready
        self.set_memory(0xFE00, 0xFFFF) ## OS_KBSR Keyboard Ready
        self.set_pc(0x3000)
        self.debug = orig
        self.labels = {}
        self.label_location = {}
        self.source = {}

    def reset(self):
        ## FIXME: reload code
        self.initialize()

    def reset_registers(self):
        orig = self.debug
        self.debug = False
        for i in range(8):
            self.set_register(i, 0)
        self.set_nzp(0)
        self.set_pc(self.orig)
        self.debug = orig
        
    def _set_nzp(self, value):
        self.nzp = (int(value & (1 << 15) > 0), 
                    int(value == 0), 
                    int((value & (1 << 15) == 0) and value != 0))

    def set_nzp(self, value):
        self._set_nzp(value)
        if self.debug:
            print("    NZP <=", self.get_nzp())

    def get_nzp(self, register=None):
        if register is not None:
            return self.nzp[register]
        else:
            return self.nzp

    def get_pc(self):
        return self.pc

    def set_pc(self, value):
        self._set_pc(value)
        if self.debug:
            print("    PC <= %s" % lc_hex(value))

    def _set_pc(self, value):
        self.pc = value

    def increment_pc(self, value=1):
        self.set_pc(self.get_pc() + value)

    def get_register(self, position):
        return self.register[position]

    def _set_register(self, position, value):
        self.register[position] = value

    def set_register(self, position, value):
        self._set_register(position, value)
        if self.debug:
            print("    R%d <= %s" % (position, lc_hex(value)))

    def set_instruction(self, location, n, line):
        """
        Put n into memory[location]; also checks to to make sure represented 
        correctly.
        """
        self.set_memory(location, lc_bin(n))
        self.source[location] = line

    def get_memory(self, location):
        return self.memory[location]

    def _set_memory(self, location, value):
        self.memory[location] = value

    def set_memory(self, location, value):
        self._set_memory(location, value)
        if self.debug:
            print("    memory[%s] <= %s" % (lc_hex(location), lc_hex(value)))

    def memory_tofile(self, start, stop, f):
        self.memory[start:stop].tofile(f)

    def memory_byteswap(self):
        self.memory.byteswap()

    #### End of overridden methods

    def reset(self):
        self.set_pc(self.orig)
        self.reset_registers()
        
    def make_label(self, label):
        return label.replace(":", "").upper()

    def in_range(self, n, bits):
        """
        Is n in range? -2**bits <= n < 2**bits
        """
        return -(1 << (bits-1)) <= n < (1 << (bits-1))
   
    def get_mem_str(self, loc):
        return 'x{0:04X}: {1:016b} {1:04x} '.format(loc, self.get_memory(loc))

    def reg(self, s, n=1):
        return self.registers[s.rstrip(', ')] << self.reg_pos[n]

    def undefined(self, data):
        raise ValueError('Undefined Instruction')

    def valid_label(self, word):
        if word[0] == 'x' and word[1].isdigit():
            return False
        return (word[0].isalpha() and
                all(c.isalpha() or c.isdigit() or c in ['_', ':'] for c in word))

    def get_immediate(self, word, mask=0xFFFF):
        if (word.startswith('x') and
            all(n in '-0123456789abcdefgABCDEF' for n in word[1:])):
            if word[1] == "-":
                v = ((-int('0x' + word[2:], 0)) & mask)
                return v
            else:
                return int('0' + word, 0) & mask
        elif word.startswith('#'):
            return int(word[1:]) & mask
        else:
            try:
                return int(word) & mask
            except ValueError:
                # could be a label
                return

    def process_instruction(self, words, line_count, line):
        """
        Process ready split words from line and parse the line use
        put to show the instruction line without label values
        """
        found = ''
        if not words or words[0].startswith(';'):
            return
        elif '.FILL' in words:
            word = words[words.index('.FILL') + 1]
            try:
                self.set_instruction(self.get_pc(), int(word), line_count)
            except ValueError:
                value = self.get_immediate(word)
                if value is None:
                    if self.make_label(word) in self.label_location:
                        self.label_location[self.make_label(word)].append([self.get_pc(), 0xFFFF, 16])
                    else:
                        self.label_location[self.make_label(word)] = [[self.get_pc(), 0xFFFF, 16]]
                else:
                    self.set_memory(self.get_pc(), lc_bin(value))
                    self.source[self.get_pc()] = line_count
            if words[0] != '.FILL':
                self.labels[self.make_label(words[0])] = self.get_pc()
            self.increment_pc()
            return    
        elif '.ORIG' in words:
            self.set_pc(int('0' + words[1]
                            if words[1].startswith('x')
                            else words[1], 0))
            self.orig = self.get_pc()
            return
        elif '.STRINGZ' in words:
            if self.valid_label(words[0]):
                self.labels[self.make_label(words[0])] = self.get_pc()
            else:
                self.source[self.get_pc()] = line_count
                raise ValueError('No label for .STRINGZ in line for PC = %s: %s, line #%s' % (lc_hex(self.get_pc()), line, line_count))
            s = line.split('"')
            string1 = string = s[1]
            # rejoin if "  inside quotes
            for st in s[2:]:
                if string.endswith('\\'):
                    string += '"' + st
    
            # encode backslash to get special characters
            backslash = False
            for c in string:
                if not backslash:
                    if c == '\\':
                        if not backslash:
                            backslash = True
                            continue
                    m = ord(c)
                else:
                    if c in 'nblr':
                        m = ord(c) - 100
                    else:
                        # easiest to implement:
                        # anything else escaped is itself (unlike Python)
                        m = ord(c)
    
                    backslash = False
                self.set_instruction(self.get_pc(), m, line_count)
                self.increment_pc()
            self.set_instruction(self.get_pc(), 0, line_count)
            self.increment_pc()
            return
        elif '.BLKW' in words:
            self.labels[self.make_label(words[0])] = self.get_pc()
            value = self.get_immediate(words[-1])
            if value is None or value <= 0:
                self.source[self.get_pc()] = line_count
                raise ValueError('Bad .BLKW immediate: %s, %r' % (words[-1], value))
            self.increment_pc(value)
            return
        # -------------------------------------------------------------
        ind = -1
        if words[0].startswith('BR'):
            ind = 0
        elif words[1:] and words[1].startswith('BR'):
            ind = 1
        if ind >= 0 and len(words[ind]) <= 5:
            if all(c in self.flags for c in words[ind][2:].lower()):
                fl = 0
                # BR alone does not make sense so default to Branch always
                if words[ind] == 'BR':
                    words[ind] = 'BRnzp'
                for f in words[ind][2:].lower():
                    fl |= self.flags[f]
                words[ind] = 'BR'
        if words[0] in self.instructions:
            found = words[0]
        else:
            if self.valid_label(words[0]):
                self.labels[self.make_label(words[0])] = self.get_pc()
            else:
                self.source[self.get_pc()] = line_count
                raise ValueError('Invalid label %s in line %s, line #: %s' % (words[0], line, line_count))
            if len(words) < 2:
                return
            found = words[1] if words[1] in self.instructions else ''
        if not found:
            self.source[self.get_pc()] = line_count
            word = words[0]
            if len(words) > 1:
                raise ValueError('Not an instruction: %s' % line)
            else:
                if self.valid_label(word):
                    if self.make_label(word) in self.label_location:
                        self.label_location[self.make_label(word)].append([self.get_pc(), 0xFFFF, 16])
                    else:
                        self.label_location[self.make_label(word)] = [[self.get_pc(), 0xFFFF, 16]]
                else:
                    raise ValueError('Invalid label: %r, line: %s' % (word, line))
            return
    
        try:
            instruction = self.instruction_info[found]
        except KeyError:
            self.source[self.get_pc()] = line_count
            raise ValueError('Unknown: instruction "%s"' % found)
        else:
            if found == 'BR':
                instruction |= fl
            r = rc = 0
            rc += found == 'JMPT'
        
            for word in words[1:]:
                word = word.rstrip(',')
                if word in self.regs:
                    t = self.regs[word] << self.reg_pos[rc]
                    r |= t
                    rc += 1
                else:
                    value = self.get_immediate(word, self.immediate_mask[found])
                    if value is not None:
                        instruction |= value
                        if found in ('ADD', 'AND'):
                            instruction |= 1 << 5
                    elif word != found:
                        if self.valid_label(word):
                            if self.make_label(word) in self.label_location:
                                self.label_location[self.make_label(word)].append([self.get_pc(), self.immediate_mask[found], self.immediate[found]])
                            else:
                                self.label_location[self.make_label(word)] = [[self.get_pc(), self.immediate_mask[found], self.immediate[found]]]
                        else:
                            raise ValueError('Invalid label: %r, line: %s' % (word, line))
    
                instruction |= r
                if found == 'JMPT':
                    break
            self.set_instruction(self.get_pc(), instruction, line_count)
            self.increment_pc()
    
    def assemble(self, code):
        # processing the lines
        orig = self.debug
        self.debug = False
        line_count = 1
        for line in code.splitlines():
            # remove comments
            ## FIXME: can't do like this! Need a real parser:
            orig_line, line = line, line.split(';')[0] 
            # add space after comma to make sure registers are space separated also (not with strings)
            if '"' not in line:
                line = line.replace(',', ', ')
            # drop comments
            words = (line.split()) if ';' in line else line.split()
            if '.END' in words:
                break
            self.process_instruction(words, line_count, line)
            line_count += 1
         # second pass:
        for label, value in self.label_location.items():
            if label not in self.labels:
                self.source[self.get_pc()] = line_count
                self.debug = orig
                raise ValueError('Bad label: "%s"' % label)
            else:
                for ref, mask, bits in value:
                    current = self.labels[label] - ref - 1
                    # kludge for absolute addresses,
                    # but seems correct for some code (lc3os.asm)
                    if self.get_memory(ref) == 0: # not instruction -> absolute
                        self.set_memory(ref, self.labels[label])
                    elif not self.in_range(current, bits) :
                        self.source[self.get_pc()] = line_count
                        self.debug = orig
                        raise ValueError(("Not an instruction: %s, mask %s, offset %s,  %s, ref %s" %
                                (label,
                                bin(mask),
                                self.labels[label] - ref,
                                bin(self.labels[label]),
                                lc_hex(ref))))
                    else:
                        # FIXME: not sure what this is, but if we init
                        # memory first, it works ok
                        self.set_memory(ref, 
                                        plus(self.get_memory(ref), 
                                             lc_bin(mask & current)))
        self.set_pc(self.orig)
        self.debug = orig

    def handleDebug(self, lineno):
        pass

    def Info(self, string):
        print(string, end="")

    def run(self, reset=True):
        if reset:
            self.cycle = 0
            self.instruction_count = 0
        self.cont = True
        while self.cont:
            self.step()

    def step(self):
        self.handleDebug(self.source.get(self.get_pc(), -1))
        instruction = self.get_memory(self.get_pc())
        instr = (instruction >> 12) & 0xF
        #print("executing: %s..." % lc_hex(self.get_pc()))
        if self.debug:
            print(self.instruction_count, self.format[instr](instruction, self.get_pc()))
        self.increment_pc()
        self.instruction_count += 1
        self.cycle += self.cycles[instr]
        self.apply[instr](instruction)
        #self.dump_registers()

    def dump_registers(self):
        print("PC:", lc_hex(self.get_pc()))
        for r,v in zip("NZP", self.get_nzp()):
            print("%s: %s" % (r,v), end=" ")
        print()
        count = 1
        for key in range(8):
            print("R%d: %s" % (key, lc_hex(self.get_register(key))), end=" ")
            if count % 4 == 0:
                print()
            count += 1
        print("-" * 20)
    
    def dump(self, start=None, stop=None):
        if start is None:
            start = min(self.source.keys())
        if stop is None:
            stop = max(self.source.keys()) + 1
        for memory in range(start, stop):
            instruction = self.get_memory(memory)
            instr = (instruction >> 12) & 0xF
            label = self.lookup(memory, "")
            if label:
                label = label + ":"
            if (instr in self.format.keys()):
                print("%-10s %s - %s  %-41s [line: %s]" % (
                    label, lc_hex(memory), lc_hex(instruction), 
                    self.format[instr](instruction, memory), self.source.get(memory, "")))
            else:
                print("%-10s %s - [line: %s]" % (
                    label, lc_hex(memory), lc_hex(instruction)))

    def disassemble(self):
        start = min(self.source.keys())
        stop = max(self.source.keys()) + 1
        print("           .ORIG %s " % lc_hex(start))
        for memory in range(start, stop):
            instruction = self.get_memory(memory)
            instr = (instruction >> 12) & 0xF
            label = self.lookup(memory, "")
            if label:
                label = label + ":"
            print("%-10s %s" % (label, self.format[instr](instruction, memory)))
        print("           .END")

    def lookup(self, location, default=None):
        for label in self.labels:
            if self.labels[label] == location:
                return label
        if default is None:
            return location
        else:
            return default

    def STR(self, instruction):
        src = (instruction & 0b0000111000000000) >> 9
        base = (instruction & 0b0000000111000000) >> 6
        pc_offset6 = instruction & 0b0000000000111111
        self.set_memory(plus(self.get_register(base), sext(pc_offset6, 6)),
                        self.get_register(src))

    def STR_format(self, instruction, location):
        src = (instruction & 0b0000111000000000) >> 9
        base = (instruction & 0b0000000111000000) >> 6
        pc_offset6 = instruction & 0b0000000000111111
        return "STR R%d, R%d, %s" % (src, base, self.lookup(plus(location, sext(pc_offset6,6)) + 1))

    def RTI(self, instruction):
        if (self.psr & 0b1000000000000000):
            raise ValueError("priviledge mode exception")
        else:
            self.set_pc(self.get_memory(self.get_register(6))) # R6 is the SSP
            self.set_register(6, lc_bin(plus(self.get_register(6), 1)))
            temp = self.get_memory(self.get_register(6))
            self.set_register(6, lc_bin(plus(self.get_register(6), 1)))
            self.psr = temp

    def RTI_format(self, instruction, location):
        return "RTI"

    def NOT(self, instruction):
        dst = (instruction & 0b0000111000000000) >> 9
        src = (instruction & 0b0000000111000000) >> 6
        self.set_register(dst, lc_bin(~self.get_register(src)))
        self.set_nzp(self.get_register(dst))

    def NOT_format(self, instruction, location):
        dst = (instruction & 0b0000111000000000) >> 9
        src = (instruction & 0b0000000111000000) >> 6
        return "NOT R%d, R%d" % (dst, src)

    def LDI(self, instruction):
        dst = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        self.set_register(dst,self.get_memory(
            self.get_memory(
                plus(self.get_pc(), sext(pc_offset9, 9)))))
        self.set_nzp(self.get_register(dst))        

    def LDI_format(self, instruction, location):
        dst = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        return "LDI R%d, %s" % (dst, self.lookup(plus(sext(pc_offset9,9), location) + 1))

    def STI(self, instruction):
        src = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        memory = self.get_memory(plus(self.get_pc(), sext(pc_offset9,9)))
        self.set_memory(memory, self.get_register(src))
        ## Hook up, side effect display:
        if memory == 0xFE06: ## OS_DDR
            try:
                self.Info(chr(self.get_register(src)))
            except:
                raise ValueError("Value in R%d (%s) is not in range 0-255 (x00-xFF)" % (src, lc_hex(self.get_register(src))))
        
    def STI_format(self, instruction, location):
        dst = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        return "STI R%d, %s" % (dst, self.lookup(plus(sext(pc_offset9,9), location) + 1))

    def RESERVED(self, instruction):
        raise ValueError("attempt to execute reserved instruction")

    def RESERVED_format(self, instruction, location):
        return ";; RESERVED %s %s" % (lc_hex((instruction >> 12) & 0xF), 
                                      lc_hex(instruction & 0b0000111111111111))

    def LEA(self, instruction):
        dst = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        self.set_register(dst, lc_bin(plus(self.get_pc(), sext(pc_offset9,9))))
        self.set_nzp(self.get_register(dst))

    def LEA_format(self, instruction, location):
        dst = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        return "LEA R%d, %s" % (dst, self.lookup(plus(sext(pc_offset9,9), location) + 1))

    def TRAP(self, instruction):
        vector = instruction & 0b0000000011111111
        self.set_register(7, self.get_pc())
        self.set_pc(self.get_memory(vector))
        if vector == 0x20:
            from Myro import ask
            data = ask(["Enter a character"], "LC3 Character Input")
            if data["Enter a character"]:
                self.set_memory(0xFE02, ord(data["Enter a character"][0]))
            else:
                self.set_memory(0xFE02, 10) # CR
        elif vector == 0x21:
            pass
        elif vector == 0x22:
            pass
        elif vector == 0x23:
            pass
        elif vector == 0x24:
            pass
        elif vector == 0x25:
            self.cont = False
        else:
            raise ValueError("invalid TRAP vector: %s" % lc_hex(vector))

    def TRAP_format(self, instruction, location):
        vector = instruction & 0b0000000011111111
        if vector == 0x20:
            return "GETC"
        elif vector == 0x21:
            return "OUT"
        elif vector == 0x22:
            return "PUTS"
        elif vector == 0x23:
            return "IN"
        elif vector == 0x24:
            return "PUTSP"
        elif vector == 0x25:
            return "HALT"
        else:
            raise ValueError("invalid TRAP vector: %s" % lc_hex(vector))

    def BR(self, instruction):
        n = instruction & 0b0000100000000000
        z = instruction & 0b0000010000000000
        p = instruction & 0b0000001000000000
        pc_offset9 = instruction & 0b0000000111111111
        if (not any([n, z, p])):
            raise AttributeError("Attempting to execute data!")
        if (n and self.get_nzp(0) or 
            z and self.get_nzp(1) or 
            p and self.get_nzp(2)):
            self.set_pc(plus(self.get_pc(), sext(pc_offset9,9)))
            if self.debug:
                print("    True - branching to", lc_hex(self.get_pc()))
        else:
            if self.debug:
                print("    False - continuing...")

    def BR_format(self, instruction, location):
        n = instruction & 0b0000100000000000
        z = instruction & 0b0000010000000000
        p = instruction & 0b0000001000000000
        pc_offset9 = instruction & 0b0000000111111111
        instr = "BR"
        if n:
            instr += "n"
        if z:
            instr += "z"
        if p:
            instr += "p"
        return "%s %s" % (instr, self.lookup(plus(sext(pc_offset9,9), location) + 1))

    def LD(self, instruction):
        dst = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        self.set_register(dst, self.get_memory(plus(self.get_pc(), sext(pc_offset9,9))))
        self.set_nzp(self.get_register(dst))

    def LD_format(self, instruction, location):
        dst = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        return "LD R%d, %s" % (dst, self.lookup(plus(sext(pc_offset9,9), location) + 1))

    def LDR(self, instruction):
        dst = (instruction & 0b0000111000000000) >> 9
        base = (instruction & 0b0000000111000000) >> 6
        pc_offset6 = instruction & 0b0000000000111111
        self.set_register(dst, self.get_memory(plus(self.get_register(base), sext(pc_offset6,6))))
        self.set_nzp(self.get_register(dst))

    def LDR_format(self, instruction, location):
        dst = (instruction & 0b0000111000000000) >> 9
        base = (instruction & 0b0000000111000000) >> 6
        pc_offset6 = instruction & 0b0000000000111111
        return "LDR R%d, R%d, %s" % (dst, base, self.lookup(plus(sext(pc_offset6,6), location) + 1))

    def ST(self, instruction):
        src = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        self.set_memory(plus(self.get_pc(), sext(pc_offset9,9)), self.get_register(src))

    def ST_format(self, instruction, location):
        src = (instruction & 0b0000111000000000) >> 9
        pc_offset9 = instruction & 0b0000000111111111
        return "ST R%d, %s" % (src, self.lookup(plus(sext(pc_offset9,9), location) + 1))

    def JMP(self, instruction):
        base = (instruction & 0b0000000111000000) >> 6
        self.set_pc(self.get_register(base))

    def JMP_format(self, instruction, location):
        base = (instruction & 0b0000000111000000) >> 6
        if base == 7:
            return "RET"
        else:
            return "JMP R%d" % base

    def JSR(self, instruction):
        temp = self.get_pc()
        if (instruction & 0b0000100000000000): # JSR
            pc_offset11 = instruction & 0b0000011111111111
            self.set_pc(plus(self.get_pc(), sext(pc_offset11,11)))
        else:                                  # JSRR
            base = (instruction & 0b0000000111000000) >> 6
            self.set_pc(self.get_register(base))
        self.set_register(7, temp)

    def JSR_format(self, instruction, location):
        if (instruction & 0b0000100000000000): # JSR
            pc_offset11 = instruction & 0b0000011111111111
            return "JSR %s" % self.lookup(plus(sext(pc_offset11,11), location) + 1)
        else:                                  # JSRR
            base = (instruction & 0b0000000111000000) >> 6
            return "JSRR R%d" % base

    def ADD(self, instruction):
        dst = (instruction & 0b0000111000000000) >> 9
        sr1 = (instruction & 0b0000000111000000) >> 6
        if (instruction & 0b0000000000100000) == 0:
            sr2 = instruction & 0b0000000000000111
            self.set_register(dst, lc_bin(plus(self.get_register(sr1), 
                                               self.get_register(sr2))))
        else:
            imm5 = instruction & 0b0000000000011111
            self.set_register(dst, lc_bin(plus(self.get_register(sr1), sext(imm5, 5))))
        self.set_nzp(self.get_register(dst))

    def ADD_format(self, instruction, location):
        dst = (instruction & 0b0000111000000000) >> 9
        sr1 = (instruction & 0b0000000111000000) >> 6
        if (instruction & 0b0000000000100000):
            imm5 = instruction & 0b0000000000011111
            return "ADD R%d, R%d, #%s" % (dst, sr1, sext(imm5, 5))
        else:
            sr2 = instruction & 0b0000000000000111
            return "ADD R%d, R%d, R%d" % (dst, sr1, sr2)

    def AND(self, instruction):
        dst = (instruction & 0b0000111000000000) >> 9
        sr1 = (instruction & 0b0000000111000000) >> 6
        if (instruction & 0b0000000000100000) == 0:
            sr2 = instruction & 0b0000000000000111
            self.set_register(dst, self.get_register(sr1) & self.get_register(sr2))
        else:
            imm5 = instruction & 0b0000000000011111
            self.set_register(dst, self.get_register(sr1) & sext(imm5, 5))
        self.set_nzp(self.get_register(dst))

    def AND_format(self, instruction, location):
        dst = (instruction & 0b0000111000000000) >> 9
        sr1 = (instruction & 0b0000000111000000) >> 6
        if (instruction & 0b0000000000100000):
            imm5 = instruction & 0b0000000000011111
            return "AND R%d, R%d, #%s" % (dst, sr1, sext(imm5, 5))
        else:
            sr2 = instruction & 0b0000000000000111
            return "AND R%d, R%d, R%d" % (dst, sr1, sr2)

    def load(self, filename):
        self.filename = filename
        fp = file(filename)
        text = "".join(fp.readlines())
        fp.close()
        return text

    def execute(self, filename):
        text = self.load(filename)
        self.assemble(text)
        self.run()
        self.dump_registers()
        print("Instructions:", self.instruction_count)
        print("Cycles: %s (%s milliseconds)" % 
              (self.cycle, self.cycle * 1./2000000))

    def save(self, base):
        # producing output
        # symbol list for Simulators
        with open(base + '.sym', 'w') as f:
            print('''//Symbol Name		Page Address
//----------------	------------
//''', end='\t', file=f)
        
            print('\n//\t'.join('\t%-20s%4x' % (name, value)
                            for name, value in self.labels.items()), file=f)
        
        with open(base + '.bin', 'w') as f:
            print('{0:016b}'.format(self.orig), file=f)  # orig address
            print('\n'.join('{0:016b}'.format(self.get_memory(m)) for m in range(self.orig, self.get_pc())),
                    file=f)
    
        # object file for running in Simulator
        with open(base + '.obj', 'wb') as f:
            #do slice from right after code and write
            #(byteorder of 0 does not matter)
            self.set_memory(self.get_pc(), self.orig)
            self.memory_byteswap()
            self.memory_tofile(self.get_pc(), self.get_pc() + 1, f)
            self.memory_tofile(self.orig,self.get_pc(), f)
            self.memory_byteswap()


if __name__ == "__main__":
    machine = LC3()
    test_code = """
;;; Algorithm for the iteration x <- a x mod m
;;; using Schrage's method

	.ORIG x3000
	JSR Random
	HALT

;;; -----------------------------------------------------
;;; Memory X has next random number
Random: ST R7,BACK 		; save return location
	LD R0, M
	LD R1, A
	JSR Divide 		; R0 / R1
	;; q = m / a
	LD R0, QUOTIENT 	; R0 / R1
	ST R0, Q 	
	;; r = m mod a
	LD R0, REMAINDER 	; R0 mod R1
	ST R0, R
        ;; x / q
	LD R0, X
	LD R1, Q
	JSR Divide 		; R0 / R1
	LD R1, QUOTIENT
	ST R1, TEMP2
	LD R1, REMAINDER 	; x mod q
	ST R1, TEMP1
	;; x <-  a * (x mod q) - r * (x / q)
	;;      a * TEMP1 - r * TEMP2
	LD R0, A
	JSR Multiply 		; R2 <- R0 * R1
	ST R2, TEMP1
	;;      a * TEMP1 - r * TEMP2
	LD R0, R
	LD R1, TEMP2
	JSR Multiply 		; R2 <- r * TEMP2
	NOT R2,R2 		; -R2
	ADD R2,R2,#1
	ST R2, TEMP2 
	LD R1, TEMP1
	ADD R2, R2, R1 		; TEMP1 - TEMP2
TEST:	BRzp DONE 		; if x < 0 then
	LD R1, M
	ADD R2, R2, R1 		; x <- x + m
DONE:	ST R2, X
	LD R7, BACK 		; Restore return address
	RET
A:	.FILL #7       	;; a , the multiplicative constant is given
M:	.FILL #32767    ;; m = 2 ** 15 - 1, the modulus is given
X:	.FILL #10	;; x, the seed is given
R:	.FILL #0
Q:	.FILL #0
TEMP1:	.FILL #0
TEMP2:	.FILL #0
BACK:	.FILL #0

;;; -----------------------------------------------------
;;; R2 <- R0 * R1
;;; Also uses R3 to store SIGN
Multiply: AND R2,R2,#0
	  AND R3,R3,#0
	  ADD R0,R0,#0 		; compare R0
	  BRn MultNEG1
	  BR  MultCont
MultNEG1: NOT R3,R3 		; flip SIGN
	  NOT R0,R0
	  ADD R0,R0,#1
MultCONT: ADD R1,R1,#0 		; compare R1
	  BRn MultNEG2
	  BR MultInit
MultNEG2: NOT R3,R3 		; flip SIGN
	  NOT R1,R1
	  ADD R1,R1,#1
MultInit: ADD R0,R0,#0  	; have R0 set the condition codes
MultLoop: BRz MultDone
	  ADD R2,R2,R1
	  ADD R0,R0,#-1
	  BR MultLoop
MultDone: ADD R0,R3,#0
	  BRzp MultRet
	  NOT R2,R2
	  ADD R2,R2,#1
MultRet:  RET			; R2 has the sum

;;; -----------------------------------------------------
;;; R0 / R1
;;; Also uses R3 to store SIGN
;;;           R4 to store -R1
;;;           R5 is QUOTIENT
;;;           R6 is REMAINDER
;;;           R2 temp
Divide:   AND R3,R3,#0
	  ST R3, QUOTIENT
	  ST R3, REMAINDER
	  ADD R0,R0,#0 		; compare R0
	  BRn DivNEG1
	  BR  DivCont
DivNEG1:  NOT R3,R3 		; flip SIGN
	  NOT R0,R0
	  ADD R0,R0,#1
DivCONT:  ADD R1,R1,#0 		; compare R1
	  BRn DivNEG2
	  BR DivInit
DivNEG2:  NOT R3,R3 		; flip SIGN
	  NOT R1,R1
	  ADD R1,R1,#1
DivInit:  ADD R4,R1,#0
	  NOT R4,R4
	  ADD R4,R4,#1
DivLoop:  ADD R2,R0,R4  	; have R2 set the condition codes
	  BRn DivDone
	  ADD R0,R0,R4
	  LD R2,QUOTIENT
	  ADD R2,R2,#1
	  ST R2,QUOTIENT
	  BR DivLoop
DivDone:  ADD R3,R3,#0 		; Negative?
	  BRzp DivRet
	  LD R2,QUOTIENT 	; Yes, then negate R2
	  NOT R2,R2
	  ADD R2,R2,#1
	  ST R2,QUOTIENT
DivRet:	  ST R0,REMAINDER
	  RET			; R2 has the sum
QUOTIENT:	.FILL #0
REMAINDER:	.FILL #0
	.END
"""

    add_code = """
.ORIG 0x3000
   ADD R0,R0,#1
   HALT
.END
"""

    machine.assemble(test_code)
    #machine.disassemble()
    #machine.dump()
    machine.run()
    #machine.dump_registers()
    #machine.save("test_code")

