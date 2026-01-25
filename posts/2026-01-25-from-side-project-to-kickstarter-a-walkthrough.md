---

title: "From Side Project to a Kickstarter for Kanjideck: A Walkthrough"

tags: kanjideck
description: "The full account of how I created Kanjideck, started a business, and launched a Kickstarter"

toc: true

---

I think this is my first non-Haskell related post on this blog! This time, I
want to walk through how I launched [Kanjideck](https://kanjideck.com/) [on
Kickstarter](https://www.kickstarter.com/projects/rromes/kanjideck) starting
from zero.

I want to go over the initial side project, how that turned into a more
ambitious idea, setting up a company, manufacturing and testing a physical
product, spreadsheets, setting up the digital infrastructure for the business,
marketing and ads, burn-out, the future, and reaching out for help as a solo
entrepreneur.

# The initial idea (August 2024)

In the summer of 2024, with nothing to do after having finished reading "Babel"
by R. F. Kuang (a book which happened to be about language and etymology and
that I thoroughly enjoyed), I decided to get back to learning Japanese.

Japanese is a challenging language to learn (at least in the West). Mostly,
that's due to Kanji, one of the three types of characters used in written
Japanese. [Hiragana](https://en.wikipedia.org/wiki/Hiragana) and
[Katakana](https://en.wikipedia.org/wiki/Katakana) are phonetic scripts, where
to each symbol corresponds exactly one sound, and there are about 50 of each
(these can be learnt in about a week). OTOH, there are about 2,136
[Kanji](https://en.wikipedia.org/wiki/Kanji) needed for fluency^[Listed by the
Japanese Ministry of Education as [Jōyō
kanji](https://en.wikipedia.org/wiki/J%C5%8Dy%C5%8D_kanji)]. Each Kanji is a
(potentially complex) symbol with one or more readings and one or more
meanings. Anyway, that means there are a lot of Kanji to memorize.

I'm a big fan of spaced repetition and [Anki](https://apps.ankiweb.net/). If
you don't know about spaced repetition, see ["How to Remember Anything
Forever-ish"](https://sr1.literatu.com). I had tried to study Kanji seriously
in the past, but the resources I was using at the time, despite using them
consistently (in Anki), weren't doing it for me. Perhaps surprisingly, there is
a big design space for Kanji learning resources. One particularly annoying
aspect of many resources is the use of mnemonics, which appeal to brute-force
more so than understanding. To me, they felt distracting and misleading, where
a completely made up story about the distinctive features of the character was
used rather than appealing to the much more useful etymological origins of the
character.

I guess I have a lot to say about why I think etymology-based learning for
Kanji is so useful, how it makes *understanding* Kanji possible (alongside
memorising them), and how this understanding makes learning new ones that much
easier. However, to stay on track, I'll just refer those curious to the
[Kanjideck Guide](https://kanjideck.com/guide) to read more about its
design^[There are more design questions: whether or not to use
[Rōmaji](https://pt.wikipedia.org/wiki/Rōmaji), what readings to include, what
examples, how to display the character, and the aesthetics of the design
themselves are important].

<!--I had quit studying Japanese in 2022 out of frustration with the resources I was using.-->
Resuming Japanese in 2024, I wanted to program my own Anki deck, compiling just
the right information for each Kanji card. I knew exactly what information I
found valuable, so it was just a matter of finding the right sources and
conjuring up the right HTML (Anki cards are rendered using HTML). I had a
working prototype within 2 days, and it looked gorgeous^[From here on I started daily driving this Anki deck and doing some tweaks to
the displayed information and to how it was laid out. A few weeks in it had
reached a fixpoint. I'm still using it today, every day!]:


Kanji for Talk             |  Kanji for Past
:-------------------------:|:-------------------------:
![Fig 1. A digital Kanjideck card for Talk](/images/kanjideck/back_79.webp)  |  ![Fig 2. A digital Kanjideck card for Past](/images/kanjideck/back_106.webp)

It looked so nice, in fact, that I thought "could I print them out?".



