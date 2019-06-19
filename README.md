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

Taking and dropping an item is even more interesting. Taking an item writes 0 to a new address, whereas dropping it writes the code of the current location to the same address. So the implementation must be that we have a vector of items in memory, mapping items to locations or 0 for inventory. Unfortunately the vector seems to hold structures instead of a mere pointer, so one has to figure out the memory span an item takes. But the 3 digit (no leading 0) location codes with leading 9 stand out easily in a memory dump from that region:

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

### The ruin challenge

Even though this simply gives us the teleporter, which we can already obtain by hacking the inventory, we can also crack the ruins challenge. We insert the coins into the round slots described in the riddle, and as we do so the coins become numbers in a formula that we have to make true. It's simply a permutations of the 5 numbers the coins are substituted for. A Ruby expression that gives the right order:

```
p [2, 9, 5, 3, 7].permutation.select { |(a, b, c, d, e)|  a + b * c**2 + d**3 - e == 399 }
```

### Teleporter

After using the teleporter we find a strange book that tells us some nonsense about the 8th register and its effect on teleporting.
So let's do it again, but this time having a watch on 8007.

```
What do you do?
% watch 8007
% use teleporter


watchpoint 8007 hit
% regs
ip : 154b
r0: a94
r1: 1545
r2: 3
r3: a
r4: 65
r5: 0
r6: 0
r7: 0
% disass 154b 20
154b    | JMPZ [r7] [15e5]
154e    | PUSH [r0]
1550    | PUSH [r1]
1552    | PUSH [r2]
1554    | SET r0 [70ac]
1557    | SET r1 [5fb]
155a    | ADD r2 [390] [1d7]
155e    | CALL [5b2]
1560    | POP r2
1562    | POP r1
1564    | POP r0
1566    | NOOP
1567    | NOOP
1568    | NOOP
1569    | NOOP
156a    | NOOP
156b    | SET r0 [4]
156e    | SET r1 [1]
1571    | CALL [178b]
1573    | EQ r1 [r0] [6]
1577    | JMPZ [r1] [15cb]
157a    | PUSH [r0]
157c    | PUSH [r1]
157e    | PUSH [r2]
1580    | SET r0 [7156]
1583    | SET r1 [5fb]
1586    | ADD r2 [ca4] [21d7]
158a    | CALL [5b2]
158c    | POP r2
158e    | POP r1
1590    | POP r0
[..]
```

So by some experimenting we can see that the jump at `154b` controls if the teleporter simply takes us to the normal location or does something else.
Since the normal path is jumping to `15e5` let's see what happens if we jump to `154e`.

```
% jump 154e
% cont
A strange, electronic voice is projected into your mind:

  "Unusual setting detected!  Starting confirmation process!  Estimated time to completion: 1 billion years."

watchpoint 8007 hit
% regs
ip : 179a
r0: 3
r1: 0
r2: 3
r3: a
r4: 65
r5: 0
r6: 0
r7: 0
```

By looking around 179a we find ourself in a weird recursive function that seems to start at `178b`.

```
% disass 178b 16
178b    | JMPNZ [r0] [1793]
178e    | ADD r0 [r1] [1]
1792    | RET
1793    | JMPNZ [r1] [17a0]
1796    | ADD r0 [r0] [7fff]
179a    | SET r1 [r7]          ; <- IP
179d    | CALL [178b]
179f    | RET
17a0    | PUSH [r0]
17a2    | ADD r1 [r1] [7fff]
17a6    | CALL [178b]
17a8    | SET r1 [r0]
17ab    | POP r0
17ad    | ADD r0 [r0] [7fff]
17b1    | CALL [178b]
17b3    | RET
```

A pseudo code translation might look like:

```
f(r0, r1)
{
  if r0 /= 0 {
    if r1 /= 0 {
      tmp = r0       // pushed on stack
      r1 = r1 - 1
      f(r0, r1)
      r1 = r0
      r0 = tmp       // pop
      r0 = r0 - 1
      f(r0, r1)
    }
    else {
      r0 = r0 - 1
      r1 = r7         // we are here when the watchpoint is hit
      f(r0, r1)
    }
  }
  else {
    r0 = r1 + 1
  }
}
```

Instead of cracking this function I tried to avoid calling it in the first place. When we were looking at `154e` the code that first checked r7, I spotted the address of f() function at

```
1571    | CALL [178b]
```

So at this point lets go again but with a break on 1571.

```
% delwatch 8007
% cont
A strange, electronic voice is projected into your mind:

  "Miscalibration detected!  Aborting teleportation!"

Nothing else seems to happen.


What do you do?
```

Now without stopping for `r7` access we need two break points, we want to take the non-normal (`r7 /= 0`) path and we want to avoid calling `178b`.


```
% break 1571
% break 154b
% use teleporter


breakpoint 154b hit
% disass 154b 2
154b    | JMPZ [r7] [15e5]
154e    | PUSH [r0]

% jump 154e
% cont
A strange, electronic voice is projected into your mind:

  "Unusual setting detected!  Starting confirmation process!  Estimated time to completion: 1 billion years."

breakpoint 1571 hit
% disass 1571 10
1571    | CALL [178b]
1573    | EQ r1 [r0] [6]
1577    | JMPZ [r1] [15cb]
157a    | PUSH [r0]
157c    | PUSH [r1]
157e    | PUSH [r2]
1580    | SET r0 [7156]
1583    | SET r1 [5fb]
1586    | ADD r2 [ca4] [21d7]
158a    | CALL [5b2]

% jump 1573
% set 8000 6
% set 8001 5
% cont
You wake up on a sandy beach with a slight headache.  The last thing you remember is activating that teleporter... but now you can't find it anywhere in your pack.  Someone seems to have drawn a message in the sand here:

    ************

It begins to rain.  The message washes away.  You take a deep breath and feel firmly grounded in reality as the effects of the teleportation wear off.

== Beach ==
This is a sandy beach in a cove on some tropical island.  It is raining.  The ocean is to your south, and heavy foliage is to your north; the beach extends west and east.

There are 3 exits:
- west
- east
- north

What do you do?
% 
```

I set `r0` to 6 because that's what the code checks, and `r1` to 5 because f() would return such value. I'm not sure if the latter is needed at all.

