# synacor

Cracking the synacor challange from [synacor address](https://challenge.synacor.com/).

## VM

Run is interrupted on either user input or when a break/watch point is hit. Then
input is either interpreted by the VM debugger or passed to the running bin as user input. No protection is implemented for mixing VM commands with inputs, and invalid VM commands probably just die and kill the VM.

__All numbers are in hex without the leading 0x prefix unless indicated otherwise__

<dl>
<dt>exit (or ^D)</dt>
<dd>Stops the VM</dd>

<dt>regs</dt>
<dd>Prints current registers & IP</dd>

<dt>stack</dt>
<dd>Prints current stack</dd>

<dt>breaks</dt>
<dd>Prints current break points</dd>

<dt>watches</dt>
<dd>Prints current watch points</dd>

<dt>cont</dt>
<dd>Continues to run (without changing existing user input)</dd>

<dt>step</dt>
<dd>One step of the VM</dd>

<dt>disass [addr] [count] (count decimal)</dt>
<dd>disassembles code or data from addr taking count number of 16 bit words.</dd>

<dt>set [addr] [value]</dt>
<dd>writes address with value (0x8000 is r0)</dd>

<dt>break [addr]</dt>
<dd>sets breakpoint on addr (hit when IP == addr)</dd>

<dt>delete [addr]</dt>
<dd>deletes breakpoint of addr</dd>

<dt>watch [addr]</dt>
<dd>sets watchpoint on addr (hit when addr is accessed (800? for registers) except code fetches)</dd>

<dt>delwatch [addr]</dt>
<dd>deletes watchpoint of addr</dd>

<dt>jump [addr]</dt>
<dd>writes addr into the IP</dd>
</dl>

# SPOILER ALERT

__If you would like to work on this challange yourself, you have stop reading here..__

## Layout

The main game loop seems to reside in `ab6` - `b88` with a few method calls in there. By examining the calls and what happens in between I ended up with the following layout:

```
   ab6    |
          - output
   ad0    |
          - prints "What do you do?"
   ae5    |
          - reads input
   afa    |
          - game logic
   b08    |
          - game logic
   b14    |
          - game logic
   b31    |
          - prints response
   b86    |
```

One can give the program input, and examine how the state of the VM changes after each game loop. State change can happen in the register bank or in the memory bank.

### Location

 By moving around one can notice two memory writes that seem to reflect the location of the player:

```
  aac <- 912
  aad <- 0
```

Here we can assume that `912` (all numbers are in hex) is the code of the current location. By moving around we can discover more rooms:
`90d`, `917`, `91c` written into `aac` etc.

Testing the theory: indeed breaking on `b18` and setting the `aac` to one of those values and `aad` to 0 ( not sure why the latter is needed, but the binary does it so we can do it too ), allows us to jump to an arbitrary location.

### Inventory

Taking and dropping an item is even more interesting. Taking an item writes 0 to a new address, whereas dropping it writes the code of the current location of that address. So the implementation must be that we have a vector of items in memory, mapping items to rooms or 0 for inventory. Unfortunately the vector seems to be a structure instead of a pointer, so one has to figure out how large an item is. But 3 digit (no leading 0) location codes with leading 9 stand out easily in a memory dump from that region:

```
% disass a6e 50
DATA : 90d(ऍ) 1270(ተ) 471e(䜞) 472c(䜬) 935(व) 0 47a8(䞨) 47b0(䞰) 7fff(翿) 1315(ጕ) 4824(䠤) 4830(䠰) 7fff(翿) 1343(ፃ) 4888(䢈) 488c(䢌) 971(ॱ) 12bf(኿) 48ba(䢺) 48c3(䣃) 994(ঔ) 14f0(ᓰ) 4903(䤃) 4911(䤑) 9a9(঩) 1501(ᔁ) 4951(䥑) 495c(䥜) 9b3(঳) 1512(ᔒ) 49a4(䦤) 49b1(䦱) 9a4(ত) 1523(ᔣ) 4a09(䨉) 4a13(䨓) 9ae(ম) 1534(ᔴ) 4a55(䩕) 4a60(䩠) 99f(ট) 1545(ᕅ) 4aa9(䪩) 4ab7(䪷) 9b8(স) 0 4af8(䫸) 4afc(䫼) a3f(ਿ) 0
```

so `90d`, `935`, `971` etc are rooms, and if we write 0 to their memory location we obtain the item from that room.

```
% set a6e 0
% set a72 0
% set a7e 0
% set a82 0
% set a86 0
% set a8a 0
% set a8e 0
% set a92 0
% set a96 0
% set a9a 0
% inv
a9a <- 0
a96 <- 0
a92 <- 0
a8e <- 0
a8a <- 0
a86 <- 0
a82 <- 0
a7e <- 0
a72 <- 0
a6e <- 0
6577 <- 69
6578 <- 6e
6579 <- 76
6576 <- 3


6576 <- 3
6576 <- 3
6576 <- 0
Your inventory:
- tablet
- empty lantern
- can
- red coin
- corroded coin
- shiny coin
- concave coin
- blue coin
- teleporter
- business card
aad <- 90d

What do you do?
```

So I just picked up a bunch of items without ever moving out of the first location!
