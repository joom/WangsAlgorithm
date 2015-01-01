WangsAlgorithm [![Build Status](https://secure.travis-ci.org/joom/WangsAlgorithm.svg)](http://travis-ci.org/joom/WangsAlgorithm)
==============

A propositional theorem prover in Haskell, using [Wang's Algorithm](http://www.cs.bham.ac.uk/research/projects/poplog/doc/popteach/wang), based on the sequent calculus (LK). Reading [a Prolog implementation](https://github.com/benhuds/Prolog) helped me understand it better.

## Usage

In order to use or compile the program you need to have [Haskell](http://www.haskell.org/) installed.

After you cloning the repository, go to the repository folder and do

```bash
cabal build
```

Now you compiled the program. You can run it like this:

```bash
./dist/build/wang/wang
```

Then you can enter propositions to see the proofs of them. Here's an example proof:

```
?: [(p->q)&(p->r)] |- [p->(q&r)]
Before: [((p) ⊃ (q)) ∧ ((p) ⊃ (r))] ⊢ [(p) ⊃ ((q) ∧ (r))]
Rule:   AndLeft
-------------------
Before: [(p) ⊃ (q),(p) ⊃ (r)] ⊢ [(p) ⊃ ((q) ∧ (r))]
Rule:   ImpliesRight
-------------------
Before: [(p) ⊃ (q),(p) ⊃ (r),p] ⊢ [(q) ∧ (r)]
Rule:   AndRight
-------------------
First branch:
    Before: [(p) ⊃ (q),(p) ⊃ (r),p] ⊢ [q]
    Rule:   ImpliesLeft
    -------------------
    First branch:
        Before: [(p) ⊃ (r),p] ⊢ [p,q]
        Rule:   WeakeningLeft
        -------------------
        Before: [p] ⊢ [p,q]
        Rule:   WeakeningRight
        -------------------
        Before: [p] ⊢ [p]
        Rule:   Id
        -------------------
        End.

    -------------------
    Second branch:
        Before: [q,(p) ⊃ (r),p] ⊢ [q]
        Rule:   WeakeningLeft
        -------------------
        Before: [q,p] ⊢ [q]
        Rule:   WeakeningLeft
        -------------------
        Before: [q] ⊢ [q]
        Rule:   Id
        -------------------
        End.

    -------------------

-------------------
Second branch:
    Before: [(p) ⊃ (q),(p) ⊃ (r),p] ⊢ [r]
    Rule:   ImpliesLeft
    -------------------
    First branch:
        Before: [(p) ⊃ (r),p] ⊢ [p,r]
        Rule:   WeakeningLeft
        -------------------
        Before: [p] ⊢ [p,r]
        Rule:   WeakeningRight
        -------------------
        Before: [p] ⊢ [p]
        Rule:   Id
        -------------------
        End.

    -------------------
    Second branch:
        Before: [q,(p) ⊃ (r),p] ⊢ [r]
        Rule:   ImpliesLeft
        -------------------
        First branch:
            Before: [q,p] ⊢ [p,r]
            Rule:   WeakeningLeft
            -------------------
            Before: [p] ⊢ [p,r]
            Rule:   WeakeningRight
            -------------------
            Before: [p] ⊢ [p]
            Rule:   Id
            -------------------
            End.

        -------------------
        Second branch:
            Before: [r,q,p] ⊢ [r]
            Rule:   WeakeningLeft
            -------------------
            Before: [r,p] ⊢ [r]
            Rule:   WeakeningLeft
            -------------------
            Before: [r] ⊢ [r]
            Rule:   Id
            -------------------
            End.

        -------------------

    -------------------

-------------------
Proof completed.
```

If you want to run the tests, use this command:

```bash
cabal test
```

## License

The MIT License (MIT)

Copyright (c) 2014 Joomy Korkut
