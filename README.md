# PureScriptMaterials

- [PureScriptMaterials](#purescriptmaterials)
  - [Work In Progress](#work-in-progress)
  - [Chapters](#chapters)
  - [Learning PureScript](#learning-purescript)
    - [Resist the urge to try to learn exactly how everything works immediately](#resist-the-urge-to-try-to-learn-exactly-how-everything-works-immediately)
    - [Keep in mind that learning PureScript is about getting things done](#keep-in-mind-that-learning-purescript-is-about-getting-things-done)
    - [Category theory is a branch of mathematics](#category-theory-is-a-branch-of-mathematics)
    - [Asking questions is good](#asking-questions-is-good)
    - [Leave most of what you've heard at the door](#leave-most-of-what-youve-heard-at-the-door)
  - [Installing the build- and project management-tool `spago`](#installing-the-build--and-project-management-tool-spago)
  - [Jordan's PureScript Reference](#jordans-purescript-reference)

## Work In Progress

This repository is based on
[our HaskellMaterials repository](https://github.com/quanterall/HaskellMaterials) and is currently
in the process of being adapted. This means that a lot of material will be in a transitional state
and may outright just reference things from Haskell. Eventually this will stop being true and we
will have converted all the material to PureScript equivalents. Bear in mind that this is a process
that is likely to take some time.

## Chapters

The below documents contain information and examples about different topics. Generally speaking they
are readable from top to bottom in terms of the assumptions they make about knowledge level.

- [Values and functions, basic types](./basics/01-values-and-functions.md)
- [Composite datatypes and working with them](./basics/02-composite-datatypes.md)
- [Type classes](./basics/03-type-classes.md)
- ["Effectful" and `Effect`](./basics/04-effectful.md)
- [Error handling](./basics/05-error-handling.md)
- [JSON in PureScript](./basics/06-json-data.md)
- [Mutable variables in PureScript](./basics/07-mutable-variables.md)
- [Streaming with `Conduit`](./basics/08-streaming.md)
- [The Reader Monad and ReaderT Monad Transformer](./basics/09-readert.md)
- [`Has` constraints](./basics/10-has-constraints.md)
- [Capability constraints](./basics/11-capability-constraints.md)
- [Testing](./basics/12-testing.md)
- [Optics (lenses & prisms)](./basics/13-optics.md)
- [Parsing with Megaparsec](./basics/14-parsing-with-megaparsec.md)

## Learning PureScript

Learning PureScript is hard, much like learning any programming language. I've found that many people
seem to make it an unnecessarily hard one because they imagine there is more theoretical background
that you have to learn in order to use PureScript. This is for the most part a fiction that people
convince themselves of. PureScript is first and foremost a practical language meant to solve problems
and there is very little that you can get out of theoretical (math or programming language theory)
knowledge when it comes to getting things done in it.

There are some things I would like to stress to everyone who wants to learn PureScript:

### Resist the urge to try to learn exactly how everything works immediately

I've seen a lot of people try to learn too much of how PureScript works immediately and I can pretty
much guarantee that they never treated learning any other language like this. With the exception of
the surface syntax of a language like C, it's near impossible. Getting comfortable with a language
takes time and effort and PureScript is no exception.

Sometimes you have to use features that you aren't intimately familiar with in order to do things.
Once you practice using them enough your intuition will catch up.

**Don't read `Monad` tutorials, use things that implement the `Monad` type class and understand how
it's used in practice, that'll be more useful than wasting your time reading theoretical (and
usually misleading/wrong) explanations of what monads are.**

### Keep in mind that learning PureScript is about getting things done

Learning PureScript is not a badge of honor. Learning more advanced things is only useful if it helps
you get things done or helps you understand more. There is lots of machinery that's created in the
PureScript world that is basically not applicable to almost any application. Learning is fun, but you
know what PureScript has too little of? People showing cool projects they created that actually do
things.

It would be a failure for this material to have you walking way saying things like:

> PureScript made me think differently. I don't use it often but it shaped the way I think.

In short, this repository is meant to serve not only as a jumping off point for exploring PureScript,
but also for using it. With that in mind it's important to stop at key points in your PureScript
journey and apply the things you think you know in order to iron out the specifics of them, as well
as get a sense of what you are able to create with them.

### Category theory is a branch of mathematics

While it can be stimulating to learn it's not useful knowledge for getting things done in PureScript.
It is my experience that people who don't use PureScript will often misrepresent the usefulness of
theory. This applies to more things than category theory.

If you feel compelled to learn category theory, keep in mind that it is unlikely to further your
knowledge of PureScript in any meaningful way and that it is more likely to serve as a potential
motivator for learning about programming language theory.

If you actually want to learn PureScript, couple pointed pieces of theory with practical exercise and
then write programs that actually do things. You'll be infinitely more productive and better at
PureScript and programming in general than someone who spent their time on things like category theory.

### Asking questions is good

We learn when we bump up against things. We might not understand the behavior of some code or we
might not be able to express ourselves clearly yet. We might just wonder if maybe there's a better
way to do the thing we're doing because it feels awkward. Talking to other people about our code and
solutions is a very effective way to stimulate thinking deeply about these things.

If you have any questions/suggestions on the material or things not covered by the material (yet?)
you can pick any way to contact me (RocketChat, Discord, rickard.andersson@quanterall.com) and I'll
do my very best to answer, correct or otherwise handle it. Another avenue would be to create an issue
or submit a pull request with the changes and/or questions.

### Leave most of what you've heard at the door

It's not uncommon for people to have opinions on PureScript. It's only slightly less common for
people to share those opinions on the Internet. What you'll come to realize after using PureScript
is that most of those opinions (good and bad) don't necessarily match real life. PureScript is more
talked about than it is used and with that comes a certain "lore".

Since it's very common to talk about PureScript without actually knowing it people feel free to, for
lack of a better word, make up attributes and characteristics. Unless someone has provably used
PureScript for creating applications to solve problems, there is no reason to trust anything they say
about the language (This actually applies to all languages and programming in general, but it's
worth noting here specifically).

## Installing the build- and project management-tool `spago`

PureScript is blessed with having a standard tool that most, if not all, people use: `spago`. I'll
recommend installing with [`asdf`](https://asdf-vm.com) here, but you can explore other ways of
installing it if you want:

```bash
$ asdf plugin add purescript
...
$ asdf plugin add spago
...
```

When we've installed `purescript` and `spago` plug-ins in `asdf` we can list and install the latest
versions of those tools:

```bash
$ asdf list all purescript
...
0.15.6
0.15.6-1
0.15.7-0
0.15.7
0.15.7-1
0.15.7-2
0.15.7-3
0.15.7-4
0.15.7-5
0.15.7-6
0.15.7-7
0.15.7-8
0.15.7-9
$ asdf install purescript 0.15.7
...
$ asdf global purescript 0.15.7
...

0.20.7
0.20.8
0.20.9
$ asdf install spago 0.20.9
...
$ asdf global spago 0.20.9
```

Now we should be able to start a new project with `spago` in any new directory:

```bash
$ mkdir ~/code/purescript/my-new-project
...
$ cd ~/code/purescript/my-new-project
...
$ spago init
[info] Initializing a sample project or migrating an existing one..
[info] Updating package-set tag to "psc-0.15.3-20220712"
Fetching the new one and generating hashes.. (this might take some time)
[info] Generating new hashes for the package set file so it will be cached.. (this might take some time)
[info] Set up a local Spago project.
[info] Try running `spago build`
```

## Jordan's PureScript Reference

As this repository represents a more streamlined learning experience, it's helpful to know where to
go in order to get a more complete picture of language and FP concepts. There is [a brilliant
reference](https://jordanmartinez.github.io/purescript-jordans-reference-site/Preface.html) written
by [Jordan Martinez](https://github.com/JordanMartinez/) that one can use to look up concepts.
