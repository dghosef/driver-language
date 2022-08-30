# Usage:
`./compile_driver driver_path`

# Intro
While writing drivers, I notice that I keep
making dumb mistakes like forgetting to put a dev_barrier or something. Also,
I notice that for the most part, drivers are pretty simple control-flow wise.
Many are just a series of memory reads and writes. This made me wonder if we could
possible do more static analysis of drivers. So I wrote a Domain Specific Language
for driver creation that compiles to C. The language has a few different statement types:

- `varname := int;` will create a variable and set its value to be an integer literal
- `varname := @addr;` will do the same but set its value to be the value at location addr
- `varname[start:end] := int;` will set bits from start to end inclusive to the integer literal
- `varname[start:end] := int;` will set bits from start to end inclusive to the param's value
- `@addr <- var;` will write `var` to `addr`
- `@addr[start:end] == val` will insert a *dynamic* check to see that the last write to addr in the bitrange [start, end] inclusive is equal to `val` and then for the purposes of static checking every statement will assume that the assertion is true. The purpose of this is because sometimes we need to make sure that previous function calls have left us in the correct state and it isn't possible to determine this @ compile time. 


The general form of a program in this language looks like:
```
driverName(paramOne: numbits, paramTwo: numbits ...) {
  statement1;
  statement2;
 ... 
}
```

It is a very simple/short language but it allows us to do some useful analysis
because we don't have to worry about nontermination/conditionals and the like. In addition to writing a program, users provide a specification consisting of a number of rules using prolog. The rules are:

- sync_rules, a mapping from addresses to list of addresses
  - the (key: (addr1, addr2, addr3 ...)) basically means "when you write/read at `key`, then we know for sure that anything written to `addr`, `addr2`, etc has been fully written. The purpose of this is so we can keep track of all the addresses that have been fully/partially written so we never try to read from a partially written address. This stops the mistake of, for example, forgetting to put dev barriers and stuff.
- write_dependencies, a map from address to list(address, start, end, bitarray).
  - the (key: [(address, start, end, bitarray)]) pairing means that in order to write to `key`, you must first have written `bitarray` to `address` in the bitrange [start:end]. Keep in mind that writes must be fully written to count. This is useful for, for example, making sure that you set the right gpio mode before turning on an LED
- readProperty, a map from address to array of length 32. Each element in the array is either 0, 1, l, or u. This is the behvavior when you read from the address. For example, the readproperty (123: [0, 0, 0, 0, 1, 1, 1, 1]) indicates that when you read from address 123, the upper 4 bits are always 0 and the lower 4 are always 1. When the readproperty is `l`, it indicates that the value from reading it will be the value of the last write to that bit. When the readproperty is `u`, it indicates that it is unknown what value will be read. For a few more examples:
  - the readProperty of any regular memory address - e.g. 0x15 - will be a list of 32 `l` values because you always read the last write.
  - the readProperty of the gpio pin output clear would be a bunch of `u` values because it is a write only address so reads are unpredictable
- write_instr is a map from address to function that takes in a variable name and produces a string of code of the target language that writes that variable to the address.
- read_instr is the same but an instructino for reading into the variable
  - Note; read and writes don't necessarily have to read or write to memory. For example, we can designate a read to address 15(for example) as our dev_barrier instruction and design our rules that way. This is how we can incorporate arbitrary code that is often necessary for drivers.
- must_sync is a list of addresses that must be sunk at the end of the driver

## The general process for chceking a program is
  -  Everytime we declare a variable, keep a bitarray of its value in a map
     -  If we get the variable from memory, use readProperties along with the record of the most recent write to memory for its value
  - When we update a variable, just replace its relevant bits in the variable map
    - When we update a variable with a parameter, just mark the relevant bits as 'unknown'
  - When we write, make sure that every write dependency in the address we are writing to is fulfilled and every address in the write dependency is fully written. After we write, we mark the address we write to as partially written.
  - Also, whenever we read or write, we look at the sync rules and update the relevant variables marked as partially written to fully written.
  - at the end of the driver, we check that everything in must_sync has been sunc
## Some improvements/stuff I would have liked to have known before I started
- Prolog is sorta annoying because it doesn't do any typechecking so it's hard to get right. I spent too long wrestling with it.
- Right now there are no error messages, just a success or faillure. In order to find the error, you have to run the prolog debugger and step through the compilation process.
- I should have maybe just made my language a small subset of c instead of adding random new syntax.

## Language Spec.
```c
X ^^ Y = replace all X[i] with Y[i] when X[i] == LastWrite
SyncStatus(action, Addr, S) = S[i/true for i in SyncRules(action, Addr)]
[H|T] is a function body where H is the first statement and T is a list of the remaining statements
[] is an empty function body
M is a map from addresses to last written values
V is a map from variable names to their values
S is a map from address to whether or not said address has been fully written
P is a map from parameters to bitlengths
A is a map of assumptions


M,V,S,P,A     |- H  : M',V',S',A'
M',V',S',P,A' |- T  : M'',V'',S'',A''
----------------------------------------------------------- Block
M,V,S,P,A |- [H|T] : M'',V'',S'',A''
eval = eval(H) + nl + eval(T)

Everything relevant in S is `yes`
----------------------------------------------------------- EmptyBlock
M,V,S,P,A |- [] : M,V,S,A
eval = ""

addr is a valid address
bitrange is a tuple of length 2
len(bitarray) == bitrange[1] - bitrange[0] + 1
M' = M[addr/M[addr].replace(bitrange, bitarray)]
S' = S[addr/true]
----------------------------------------------------------- Assume {}
M,V,S,P,A |- @addr[bitrange] == bitarray : M',V,S',A + [(addr, bitrange, bitarray)]
eval = ""

i is a word-sized integer constant
varname not in P
----------------------------------------------------------- VarDecl-Num
M,V,S,P,A |- `varname` := i; : M,V[varname/i],S,A
eval = "var `varname`:word = `i`"

Addr is a valid address
S' := SyncStatus(read, Addr, S)
S'[Addr] == true
val := ReadProperties[Addr] ^^ M[Addr]
varname not in P
----------------------------------------------------------- VarDecl-Addr
M,V,S,P,A |- `varname` := @Addr; : M,V[varname/val],S',A
eval = ReadRule[(Addr, varname)]["ADDR"/Addr, "VAR"/varname]

bitarray is a bitarray
bitrange is a length 2 tuple of ints
varname in V
varname not in P
len(b) == bitrange[1] - bitrange[0] + 1 # Bitranges are inclusive so must add 1
val := V[varname].replace(bitrange, b)
----------------------------------------------------------- VarUpdate-Num
M,V,S,P,A |- `varname`[bitrange] := bitarray; : M,V[varname/val],S,A
eval = "`varname` = replace_bits(`varname`, `bitrange`, `bitarray`)"
       

paramname in P
bitrange is a length 2 tuple of ints
varname in V
varname not in P
P[paramname].bitlen == bitrange[1] - bitrange[0] + 1 # Bitranges are inclusive so must add 1
val := V[varname].replace(bitrange, [U] * P[paramname]/bitlen)
----------------------------------------------------------- VarUpdate-Param
M,V,S,P,A |- `varname`[bitrange] := `paramname`; : M,V[varname/val],S,A
eval = "`varname` = replace_bits(`varname`, `bitrange`, `bitarray(paramname)`)"

Addr is a valid address
S' = SyncStatus(write, Addr, S)
forall (address, bitrange, bitarray) in WriteDependencies[Addr], 
  M[address].slice(bitrange) == bitarray && S'[address] == True
M' = M[Addr/V[varname]]
S'' = S'[Addr/False]
----------------------------------------------------------- Write
M,V,S,P,A |- @Addr <- `varname`; : M',V,S'',A
eval = WriteRule[(Addr, varname)]["ADDR"/Addr, "VAR"/varname] +
       "GlobalMemory.update(`Addr`, `varname`)"
```

I spent most of my project working on the actual language and not enough on writing
drivers. I did write uartinit. The code looks like:

```

uartinit() {
    gpiofunc := 0;
    gpiofunc[12:14] := 2;
    gpiofunc[15:17] := 2;
    @538968068 <- gpiofunc;
    devbarrier := @15;
    enb := @539054084;
    enb[0:0] := 1;
    @539054084 <- enb;
    cntl := @539054176;
    cntl[0:1] := 0;
    @539054176 <- cntl;
    iir := @539054152;
    iir[1:2] := 3;
    @539054152 <- iir;
    ier := @539054148;
    ier[0:1] := 0;
    @539054148 <- ier;
    lcr := @539054156;
    lcr[0:1] := 3;
    @539054156 <- lcr;
    baud := 270;
    @539054184 <- baud;
    cntll := @539054176;
    cntll[0:1] := 3;
    @539054176 <- cntll;


    devbarrierone := @15;
}
```

I made reading from address 15 emit a dev_barrier and the rest do the standard get32/put32.

Here are the rules I came up with:
```
% We have to sync all the gpio pins 
must_sync(Addr) :- Addr > 538968064.

% readProperty(Addr, Val) indicates what happens when you read Addr
readProperty(_, Lastwrites) :- duplicate_list(l, 32, Lastwrites).

% List of address that get synced when Addr is read
% Our dev barrier instruction syncs everything
sync_rules(_, Addr, Synced) :-
  Addr is 15,
  range(0, 2000000, Synced).
% reads/writes within the same device are guaranteed to be ordered.
% SDRam
sync_rules(_, Addr, Synced) :-
  Addr >= 0,
  Addr =< 536870912,
  range(0, 536870912, Synced).
% GPIO
sync_rules(_, Addr, Synced) :-
  Addr >= 538968064,
  Addr =< 538968220,
  range(538968064, 538968220, Synced).
% SPI/UART
sync_rules(_, Addr, Synced) :-
  Addr >= 539054080,
  Addr =< 539054292,
  range(539054080, 539054292, Synced).
write_dependencies(539054084, [[538968068, 15, 17, [0,1,0]], [538968068, 12, 14, [0,1,0]]]).
write_dependencies(539054176, [[539054084, 0, 0, [1]]]).
write_dependencies(539054152, [[539054176, 0, 1, [0, 0]]]).
write_dependencies(539054148, [[539054176, 0, 1, [0, 0]]]).
write_dependencies(539054156, [[539054176, 0, 1, [0, 0]]]).
write_dependencies(539054184, [[539054176, 0, 1, [0, 0]]]).

write_dependencies(A, []) :- \+ write_dependencies(A, [_|_]).
% In order to write to anny address > 1000, write 010 to bits 1-3 at 123
%write_dependencies(Addr, [[123, 1, 3, [1, 0, 1]]]) :- Addr > 1000.
read_inst(15, _, Instr) :-
  Instr = [dev_barrier, '(', ')', ';'].
read_inst(Addr, VarName, Instr) :-
  Instr = [long, VarName, '=', get_32, '(', Addr, ')', ';'].

write_inst(Addr, VarName, Instr) :-
  Instr = [put_32, '(', Addr, ',', VarName, ')', ';'].
```

These rules aren't complete, but they show how you would write a specification
for a device. The listed uartinit function compiles and works with the rest of the
uart code from my uart assignment. If you try to compile without, for example,
one of the reads from @15 or some of the writes, compilation will fail.