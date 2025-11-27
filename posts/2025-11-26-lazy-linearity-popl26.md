---

title: "Lazy Linearity for a Core Functional Language (POPL 2026)"

tags: linear-types, laziness
description: "A brief announcement about my first paper published to the POPL conference"

toc: false

---

I'm very proud to announce that "Lazy Linearity for a Core Functional
Language", a paper by myself and [Bernardo
Toninho](https://web.tecnico.ulisboa.pt/bernardo.toninho/), will be published
at [POPL 26](https://popl26.sigplan.org/track/POPL-2026-popl-research-papers)!

The extended version of the paper, which includes all proofs, is available
here \[[arXiv](https://arxiv.org/abs/2511.10361), [PDF](/data/papers/popl26-extended-version-2511.10361v2.pdf)\].

**The short-ish story**: In 2023, for my Master's thesis, I reached out to [Arnaud
Spiwack](https://assert-false.science/arnaud/) to discuss how Linear Types had
been implemented in GHC. I wanted to research compiler optimisations made
possible by linearity. Arnaud was quick to tell me:

"*Well yes, but you can't!"*

*"Even though Haskell is linearly typed, Core isn't!"*^[Core is the
intermediate compiler language to which source Haskell is desugared and to
which optimisations are applied]

Linearity is ignored in Core because, as soon as it's optimised,
previously valid linear programs become invalid.
<!---->
It turns out that traditional linear type systems are too syntactic, or
*strict*, about understanding linearity -- but Haskell, regardless of linear
types, is lazily evaluated. Improving optimisations would have to wait.

Our paper presents a system which, in contrast, also accepts programs that can
only be understood as linear under non-strict evaluation. Including the vast
majority of optimised linear Core programs (with proofs!).

The key ideas of this paper were developed during my Master's, but it took a
few more years of on-and-off work (supported by my employer
[Well-Typed](https://well-typed.com/)) with Bernardo to crystalize the understanding of a
"lazy linearity" and strengthen the theoretical results.

Now, the proof of the pudding is in the eating. Go read it!

<br>
<p style="width:100%; text-align: center">**Abstract**<p>
<section style="margin-left: 2em; margin-right: 2em; margin-bottom: 2em;">
Traditionally, in linearly typed languages, consuming a linear resource is
synonymous with its syntactic occurrence in the program. However, under the
lens of non-strict evaluation, linearity can be further understood
semantically, where a syntactic occurrence of a resource does not necessarily
entail using that resource when the program is executed. While this distinction
has been largely unexplored, it turns out to be inescapable in Haskell's
optimising compiler, which heavily rewrites the source program in ways that
break syntactic linearity but preserve the program's semantics. We introduce
Linear Core, a novel system which accepts the lazy semantics of linearity
statically and is suitable for lazy languages such as the Core intermediate
language of the Glasgow Haskell Compiler. We prove that Linear Core is sound,
guaranteeing linear resource usage, and that multiple optimising
transformations preserve linearity in Linear Core while failing to do so in
Core. We have implemented Linear Core as a compiler plugin to validate the
system against linearity-heavy libraries, including linear-base.
</section>

