---

title: "From Side Project to the Kickstarter for Kanjideck: A Walkthrough"

tags: kanjideck
description: "The full account of how I created Kanjideck, started a business, and launched a Kickstarter"

toc: true

---

I think this is my first non-Haskell related post on this blog! This time, I
want to walk through how I launched [Kanjideck](https://kanjideck.com/) [on
Kickstarter](https://www.kickstarter.com/projects/rromes/kanjideck) starting
from zero.

I want to go over the initial side project, how that turned into a more
ambitious idea, manufacturing and testing a physical product, setting up a
company, spreadsheets, setting up the digital infrastructure for the business,
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


Fig 1. Kanji: Talk             |  Fig 2. Kanji: Past
:-------------------------:|:-------------------------:
![Fig 1. A digital Kanjideck card for Talk](/images/kanjideck/back_79.webp)  |  ![Fig 2. A digital Kanjideck card for Past](/images/kanjideck/back_106.webp)

It looked so nice, in fact, that I thought *"what if"*?

## What if ?

The digital Anki deck was working fantastically well and the design turned out
beautiful. I wanted to print some cards into the physical realm and experiment
studying with a physical resource^[Physical books and studying with a notebook
is still my preference, even though in the long run Anki is unbeatable at
managing the large amount of cards to review. I still use Anki daily.].
Namely, I wanted to try the [Leitner
System](https://en.wikipedia.org/wiki/Leitner_system), a well-known system for
physical spaced repetition.

I thought that printing custom cards would require a lot of upfront orders
(this turned out to be false, see next section), and started entertaining the idea of
making the resource I had created more widely available and start a small
business^[Open-source is a big part of my life (including my day job), and even
though for now Kanjideck is available only in the [Kickstarter
page](https://www.kickstarter.com/projects/rromes/kanjideck), I intend to make
the source for building the digital version available as soon as I can clean it
up.].

I had heard about [Kickstarter](https://www.kickstarter.com/) in passing -- a
crowd-funding platform which allows people to raise money in pre-orders, but
only charges customers if there are enough pre-orders to kickstart the business.

I then decided I'd run a Kickstarter to sell my Japanese Kanji deck in both
physical and digital versions, and gather enough up-front orders to be able to
print the decks out at a reasonable price per unit.

I just needed a physical version of Kanjideck first! And a project title: Kanjideck.

# Manufacturing playing cards (September 2024)

I called a few local printing companies (location: Portugal), when I was first
trying to figure out how to print the cards. Without knowing exactly who to
call, none of those I contacted were able to do playing cards specifically, and
required bulk orders in any case.

Eventually, I stumbled upon
[MakePlayingCards.com](https://www.makeplayingcards.com/), a printing business
specialized in playing cards from whom I could buy a single copy of a custom
deck, and easily consult the prices of bulk orders. They do worldwide shipping
and fulfillment, mentioning fulfillment for Kickstarter by name.

Based on the digital version of Kanjideck I made for Anki using HTML, I
programatically generated a PDF per card with a headless browser through
[Playwright](https://playwright.dev/), and converted each card from PDF to a
high-resolution PNG (fit for printing) using
[ImageMagick](https://imagemagick.org).

![Fig 3. Webpage with HTML Kanjideck cards](/images/kanjideck/IMG_8050.webp)

I ordered a test copy of 60 Kanjideck cards using their single-copy custom
print feature on September 10th. That was about 15€ at the time.

<!-- ![Fig 4. Plastic-wrapped cards order](/images/kanjideck/order1.webp) -->

While waiting for the cards to arrive, I started learning the 3D modelling tool
[Blender](https://www.blender.org/). I had seen many Kickstarter videos which
used pretty amazing animations to showcase their product. I was decided to
attempt this myself. A few days in I was able to model the cards and render
them in some fairly OK scenes:

Fig 5. Blender render      |  Fig 6. Blender render
:-------------------------:|:-------------------------:
![Fig 5. Blender render of Kanjideck cards](/images/kanjideck/fifth_render.webp)  |  ![Fig 6. Another blender render of Kanjideck cards](/images/kanjideck/r9in.webp)

I kept improving with time, but eventually realized I was not going to be able
to do an animated video which looked good enough to serve as the video
showcasing the project on the Kickstarter page.

In the meantime, the cards arrived from the manufacturer. These were the very
first prototype:

Fig 7. First prototype     |  Fig 8. First prototype^[In this picture you may also find some scribbles that refer to a board game I tried to design using the Kanjideck cards exclusively! Nothing came out of it (yet?).]
:-------------------------:|:-------------------------:
![Fig 7. First prototype](/images/kanjideck/9455B77F-7ABB-441F-8841-01ECFA3A6A24.webp)  |  ![Fig 8. First prototype](/images/kanjideck/IMG_8121.webp)

I kept daily driving the Anki deck, improving the cards information and layout,
and designing the product. Two other important things I tackled at this time were:

- Meta cards for the physical deck, including a tutorial, reference cards for
  Hiragana and Katakana, a reference card for verb and adjective conjugation,
  and markers for physical spaced repetition (to make a kind of Leitner box
  using them to separate which cards are in which level)

- The box design to package the cards. The Blender skills I had developed ended
  up being very helpful when designing the deck boxes, as I was able to lay out
  my design on a 3D model with accurate dimensions and preview it.

The second order, on October 19th, included three copies of the same deck
of cards, properly packaged, using various different sizes/textures for the
cards and for the boxes. The price for printing a single deck of 90 cards with
a rigid box was close to 50€, without shipping. The values are much more
reasonable when buying at scale, but for that I need the backing through Kickstarter:

![Fig 9. Ordering different types of cards](/images/kanjideck/order2.webp)

This order proved invaluable to really get a feel for the different types available
and choose the best fit for the Japanese-style cards. The linen material came
on top for the cards, but not for the box. Here are the three packages when
they arrived^[Here you may also notice a typo in the boxes, using ぉ rather than を]:

![Fig 10. Three Kanjidecks just arrived](/images/kanjideck/IMG_8449.webp)


