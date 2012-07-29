delay-srt
=========

delay-srt is a Racket (Scheme) program which increases/decreases the
delay of an SRT subtitle file.


Motivation
----------

I like to download .mkv video files together with .srt subtitle files.
However, it is often that the subtitle has an offset to the video.
Some players have the ability to adjust (increase or decrease) the
delay of subtitle, but some do not.  So I decided to write this
program as a solution and my first excersice in learning the Racket
language (http://racket-lang.org).


Usage
-----

In order to use this program, you need to download and install Racket
(http://racket-lang.org/download).

You can run the program as a script.  For example, the following
command increases the delay of 30 seconds to the subtitle file
terminator-3.srt.

```bash
racket delay-srt.rkt -i 0:0:30,000 terminator-3.srt > new.srt
```

In above example, the format of delay is
"hours:minutes:seconds,milliseconds".

You can also decrease the delay by substituting the parameter -i (or
--increase-delay) to -d (or --decrease-delay).

I practiced using the unit test framework of Racket in this project.
To run the unit test:

```bash
racket delay-srt-test.rkt
```

