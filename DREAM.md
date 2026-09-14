# Dream

The README outlines what Lithium _is_ at the moment. In here, I'd like to let my imagination loose. Imagine no borders, no friction, infinite resources, instant output. What is the vision for Lithium? How did I conjure it up? Where does it come from? Where do I want it to go?

## Lisp machines

I learned Common Lisp over twenty years ago, not just as a language but as a lifestyle. "Look at all that accidental complexity around me!", I thought. "C with all its warts! Not to mention C++! Unix with its 'everything-is-a-file-but-only-so-far' stringly typed mindset!"

I learned that, years ago, there were machines programmed in Lisp all the way down. That they had specialized hardware to facilitate efficient implementations of Lisp. I spent a lot of time daydreaming about an alternative history where Lisp prevailed and Lisp-based operating systems for all architectures -- x86, x86-64, ARM, you name it -- became the norm.

Then Clojure came along and taught me about identity and state, about decomplection, about thinking in data. To this day I dream about a Clojure-based operating system. One where Clojure -- or a Clojure-based language, with extensions (what extensions?) that enable efficient implementations -- is a central piece, and everything is written in it, down to the kernel and device drivers.

Projects like Mezzano, Movitz, or Project Mage are my inspiration. So is Oberon (the OS); although not written in Lisp, it is small, self-contained, and all-in-one-language.

## DOS's minimalism

I am a child of DOS. I lived in DOS before learning Linux, and I steered clear of Windows 9x where I only could.

Now, DOS is not a nice operating system. It's a set of unelegant kludges bolted onto each other. It exposes only a bare minimum of an API, inviting (or forcing) the programmer to directly talk to the hardware. Real mode's memory model sucks.

But what I enjoyed about DOS was its minimalism. Three files were enough for a minimally functional DOS installation. Total size: less than 200 KB. You can put that on a floppy, leaving a lot of space for useful software!

Turbo Pascal, for instance: a similarly self-contained, blazing fast compiler, producing small binaries. It felt like an empty notebook. No unnecessary parts; imagination was the limit.

Some early Linux distros were similarly minimal. I fondly cherish in my memory names like "tomsrtbt" and "muLinux". I used to hand-compile my kernels with only what my machines needed.

## Things that could be but weren't

I'm thinking of Alexia Massalin's "Synthesis" kernel, with its notion of "quajects" and "runtime-synthesized code" for partial function application. It always seemed to me like something that could be implemented in a Lisp. Maybe something low-level that could help said Lisp run fast.

How does it rhyme with Clojure? I have no clear picture, but I'd like to experiment.

## Interaction with outside world

Is less important to me. In Lithium, I dream of the perfect harmony of ideas _within_, rather than its applicability for _outside_. The outside world is kludgy, verbose, full of competing standards.

For instance, I don't picture myself writing a full-blown Web browser in Lithium. Just the sheer size of the HTML standard, not to mention JS, DOM, etc, makes me shudder.

But one can't live in total isolation. I would like to have basic UTF-8 support, for example. Some I/O for HUD and networking. A TCP/IP stack.

## 16-bit?

I started out with targetting 16-bit x86 because it's simple. But there's more to it than that. The tiny memory model can be looked at as a 16-bit equivalent of a protected-mode 32/64-bit flat memory model; the segmentation is out of the picture. I'd like to see how much it's possible to fit in 64 KiB.

I once read in "Bajtek", quoting the original Polish:

> Kiedy na Spectrum coś działało zbyt wolno, to trzeba było zoptymalizować algorytm. Teraz, w epoce PC-tów, po prostu kupuje się szybszy komputer. I to ma być informatyka? Kochani, 48 KB to niezmierzona przestrzeń!

## Speaking of segmentation

Could it be used to facilitate system-wide garbage collection?

## Imagine

Imagine a modern Clojure-based operating system, for modern computers, but also for old hardware.

Imagine it fitting in one 1.44MB floppy.

Imagine a REPL as _the_ way to interact with the system, just like BASIC was _the_ way for 8-bit micros of yore.

Imagine the compiler as a central part of the system, not just an add-on. Imagine it being able to compile itself.

Imagine no distinction between a program and a function.

Imagine a way to explore less-well trodden paths in OS design.

## Conclusion

There is none, really. Dreams are rarely coherent or complete. But what a dream!
