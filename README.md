WangsAlgorithm [![Build Status](https://secure.travis-ci.org/joom/WangsAlgorithm.svg)](http://travis-ci.org/joom/WangsAlgorithm)
==============

A propositional theorem prover in Haskell, using Wang's Algorithm. Reading [a Prolog implementation](https://github.com/benhuds/Prolog) helped me understand it better.

## Usage

In order to use or compile the program you need to have [Haskell](http://www.haskell.org/) installed.

After you cloning the repository, go the repository folder and do

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
Rule: AndLeft
-------------------
Before: [(p) ⊃ (q),(p) ⊃ (r)] ⊢ [(p) ⊃ ((q) ∧ (r))]
Rule: ImpliesRight
-------------------
Before: [(p) ⊃ (q),(p) ⊃ (r),p] ⊢ [(q) ∧ (r)]
Rule: AndRight
-------------------
First branch:
    Before: [(p) ⊃ (q),(p) ⊃ (r),p] ⊢ [q]
    Rule: ImpliesLeft
    -------------------
    First branch:
        Before: [(p) ⊃ (r),p] ⊢ [p,q]
        Rule: IdStar
        -------------------
        End.

    -------------------
    Second branch:
        Before: [q,(p) ⊃ (r),p] ⊢ [q]
        Rule: IdStar
        -------------------
        End.

    -------------------

-------------------
Second branch:
    Before: [(p) ⊃ (q),(p) ⊃ (r),p] ⊢ [r]
    Rule: ImpliesLeft
    -------------------
    First branch:
        Before: [(p) ⊃ (r),p] ⊢ [p,r]
        Rule: IdStar
        -------------------
        End.

    -------------------
    Second branch:
        Before: [q,(p) ⊃ (r),p] ⊢ [r]
        Rule: ImpliesLeft
        -------------------
        First branch:
            Before: [q,p] ⊢ [p,r]
            Rule: IdStar
            -------------------
            End.

        -------------------
        Second branch:
            Before: [r,q,p] ⊢ [r]
            Rule: IdStar
            -------------------
            End.

        -------------------

    -------------------

-------------------
Proof completed.
```

## License

The MIT License (MIT)

Copyright (c) 2014 Joomy Korkut
