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
- `varname[start:end] := paramname;` will set bits from start to end inclusive to the param's value
- `@addr <- var;` will write `var` to `addr`
- `@addr[start:end] == val` will insert a *dynamic* check to see that the last write to addr in the bitrange [start, end] inclusive is equal to `val` and then for the purposes of static checking every statement will assume that the assertion is true. The purpose of this is because sometimes we need to make sure that previous function calls have left us in the correct state and it isn't possible to determine this @ compile time. 


The general form of a program in this language looks like:
```c
driverName(paramOne: numbits, paramTwo: numbits ...) {
  statement1;
  statement2;
 ... 
}
```

An example is:
```c
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

In addition to the program, you can add a specification that verifies your program obeys certain properties.
The specification is written in prolog. Knowledge of it is useful but not necessary to understand this. 
Specification rules we can add are: 

- `sync_rules`, a mapping from addresses to list of addresses
  - The purpose of this is so we can keep track of all the addresses that have been fully/partially written so we never try to read from a partially written address. This stops the mistake of, for example, forgetting to put dev barriers and stuff.
  - For example, the rule: `sync_rules(_, Addr, Synced) :- Addr is 15, range(0, 2000000, Synced).` roughly translates to "on writing to address 15, addresses 0 to 2000000 are fully written"
- write_dependencies, a map from address to list of (address, start, end, bitarray) tuples
  - Add rules that ensure that certain values have been written to certain addresses. This is useful for, for example, making sure that you set the right gpio mode before turning on an LED
  - For example, the rule: `write_dependencies(15, [[40, 0, 3, [1, 0, 1, 1]]]).` means that "it is an error if, on writing to address 15, bits 0-3 at address 40 aren't set to 1011"
- readProperty, a map from address to array of length 32. Each element in the array is either 0, 1, l, or u. This is the behvavior when you read from the address. For example, the readproperty (123: [0, 0, 0, 0, 1, 1, 1, 1]) indicates that when you read from address 123, the upper 4 bits are always 0 and the lower 4 are always 1. When the readproperty is `l`, it indicates that the value from reading it will be the value of the last write to that bit. When the readproperty is `u`, it indicates that it is unknown what value will be read. For a few more examples:
  - the readProperty of any regular memory address - e.g. 0x15 - will be a list of 32 `l` values because you always read the last write.
  - the readProperty of the gpio pin output clear would be a bunch of `u` values because it is a write only address so reads are unpredictable
- write_instr is a map from address to function that takes in a variable name and produces a string of code of the target language that writes that variable to the address.
- read_instr is the same but an instruction for reading into the variable
  - Note: read and writes don't necessarily have to read or write to memory. For example, we can designate a read to address 15(for example) as our dev_barrier instruction and design our rules that way. This is how we can incorporate arbitrary code that is often necessary for drivers.
- must_sync is a list of addresses that must be fully written at the end of the driver

A minimal example set of rules is shown [here](src/compiler/rules.pl) and an example driver for initializing UART on the raspberry pi is shown [here](test.d)