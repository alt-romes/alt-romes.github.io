---

title: "Haskell Debugger for GHC 9.14"

tags: debugger, haskell
description: "Announcing the brand new Haskell Debugger, which I've been working on for the past year. It is first compatible with GHC 9.14."

toc: false

---

This post was first published inline to the [Haskell
Discourse](https://discourse.haskell.org/), where there was some [discussion
about the debugger](https://discourse.haskell.org/t/the-haskell-debugger-for-ghc-9-14/13499):

# The Haskell Debugger for GHC 9.14

**The Haskell Debugger is ready to use with GHC-9.14!**

The installation, configuration, and talks can be found in [the official
website](https://well-typed.github.io/haskell-debugger). The tl;dr first step
is installing the debugger:

```bash
$ ghc --version # MUST BE GHC 9.14
The Glorious Glasgow Haskell Compilation System, version 9.14.1

$ cabal install haskell-debugger \
    --allow-newer=base,time,containers,ghc,ghc-bignum,template-haskell \
    --enable-executable-dynamic # ON WINDOWS, DO NOT PASS --enable-executable-dynamic
...

$ ~/.local/bin/hdb --version # VERIFY IT'S THE LATEST!
Haskell Debugger, version 0.11.0.0
```

The second step is configuring your editor to use the debugger via the **D**ebug **A**dapter **P**rotocol (DAP).
    - For VSCode, install [the haskell debugger extension](https://marketplace.visualstudio.com/items?itemName=Well-Typed.haskell-debugger-extension).
    - For Neovim, install `nvim-dap` and [configure it for haskell-debugger](https://codeberg.org/mfussenegger/nvim-dap/wiki/Debug-Adapter-installation#haskell-hdb)
    - For other editors, consult your DAP documentation and let others know how!

Bug reports and discussions are welcome in the [haskell-debugger issue
tracker](https://github.com/well-typed/haskell-debugger).

My MuniHac 2025 talk also walks through the installation, usage, and design of
the debugger. Do note much has been improved since the talk was given, and much
more will still improve.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/urYtE15ryA0?si=6LtSEfXfCOSJFG11" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## A little bit more info

The debugger work is sponsored by Mercury. It's the project in which I've spent
most of my full working days (for almost a full year now), with the invaluable
help from my team at Well-Typed.

The debugger is meant to work both on trivial files and on large and complex
codebases^[Although, for large codebases, the usability is still rough around
the edges because of possibly long bytecode compilation times, and library code
not being interpreted. We've made considerable progress to improve this with
the [bytecode artifacts work](https://discourse.haskell.org/t/rfc-introduce-a-serialisable-bytecode-format-and-corresponding-bytecode-way/12678)].
It is a GHC application so *all features are supported*. Like HLS, it also uses
`hie-bios` to automatically configure the session based on your cabal or stack
project.

Robustness is a main goal of the debugger. If anything doesn't work, or if you
have performance issues, or something crashes, please don't hesitate to submit
a bug. We've got a small but respectable testsuite, and have tested performance
by debugging GHC itself, but there's much still to be fixed and improved.

**Roadmap**: There's a lot to do. I'm currently working on callstacks and
multi-threaded support. Do let me know what features would be most important to
you, so I can also factor that into the future planning.
