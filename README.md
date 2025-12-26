This is a Morse code generator that I wrote a few years ago.  I did it back
about 2009, then I converted it to Scala when I learned that.  It's just been
laying around on my computer, so I thought it was time to put it to good use.
It has some good features -- a set of classes that generate various waveforms
that get used to generate the Morse waveforms that get played through the
speaker.  This includes limiting the rise and fall time of the pulses, to
eliminate the distracting clicks that some generators have.  It uses
javax.sound to render the waveforms to the computer's sound system, and jline
to do text I/O on the command line.

This morse player can be used in quiz mode or standalone mode, to generate
English words from a dictionary, or random words.  Characters, numbers, and
punctuation can be controlled by command-line switches.  Farnsworth spacing
is implemented, and the character speed and text speed can be independently
controlled.  I hope it proves helpful to people in training with Morse code.

Jonathan Keatley - KE5RT
