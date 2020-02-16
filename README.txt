This source package contains three modules,
  EA.C
  HISTORY.C
  DATETIME.C

Only few programmers use EAs, because IBMs API is not very easy to use. EA.C provides a much easier to use
interface to this API without too many restrictions (The functions cannot write more than 1 EA at a time).

HISTORY.C uses this EA.C and writes history logs to the history setting of an object. I added this module
as a sample for the usage of EA.C.

DATETIME.C provides a date/time string in country specific formats. It is used by HISTORY.C

The modules are property of Noller&Breining software. They may be freely distributes without changes.
The modules may be compiled for static and dynamic linking.

K. Breining
Noller&Breining Software
