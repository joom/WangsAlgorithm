WangsAlgorithm [![Build Status](https://secure.travis-ci.org/joom/WangsAlgorithm.svg)](http://travis-ci.org/joom/WangsAlgorithm)
==============

A propositional theorem prover in Haskell, using [Wang's Algorithm](http://www.cs.bham.ac.uk/research/projects/poplog/doc/popteach/wang), based on the sequent calculus (LK). Reading [a Prolog implementation](https://github.com/benhuds/Prolog) helped me understand it better.

## Usage

In order to use or compile the program you need to have [Stack](http://haskellstack.org) installed.

After you cloning the repository, go to the repository folder and do

```bash
stack install
```

Now you installed the program. You can run it like this:

```bash
wang --sequent "[(p->q)&(p->r)] |- [p->(q&r)]" --backend Text
```

Or shortly:

```bash
wang -s "[(p->q)&(p->r)] |- [p->(q&r)]" -b Text
```

You can also use `LaTeX` for an output.

Here's an example text proof for that:

```
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

Here's the LaTeX output for the same sequent.

```
\begin{prooftree}
    \AxiomC{} \RightLabel{\scriptsize $I$}
    \UnaryInfC{$p\vdash p$} \RightLabel{\scriptsize $WR$}
    \UnaryInfC{$p\vdash p,q$} \RightLabel{\scriptsize $WL$}
    \UnaryInfC{$\left( p\supset r\right) ,p\vdash p,q$}
    \AxiomC{} \RightLabel{\scriptsize $I$}
    \UnaryInfC{$q\vdash q$} \RightLabel{\scriptsize $WL$}
    \UnaryInfC{$q,p\vdash q$} \RightLabel{\scriptsize $WL$}
    \UnaryInfC{$q,\left( p\supset r\right) ,p\vdash q$}
    \RightLabel{\scriptsize $\supset L$}
    \BinaryInfC{$\left( p\supset q\right) ,\left( p\supset
               r\right) ,p\vdash q$} \AxiomC{}
    \RightLabel{\scriptsize $I$} \UnaryInfC{$p\vdash p$}
    \RightLabel{\scriptsize $WR$} \UnaryInfC{$p\vdash p,r$}
    \RightLabel{\scriptsize $WL$}
    \UnaryInfC{$\left( p\supset r\right) ,p\vdash p,r$}
    \AxiomC{} \RightLabel{\scriptsize $I$}
    \UnaryInfC{$p\vdash p$} \RightLabel{\scriptsize $WR$}
    \UnaryInfC{$p\vdash p,r$} \RightLabel{\scriptsize $WL$}
    \UnaryInfC{$q,p\vdash p,r$} \AxiomC{}
    \RightLabel{\scriptsize $I$} \UnaryInfC{$r\vdash r$}
    \RightLabel{\scriptsize $WL$} \UnaryInfC{$r,p\vdash r$}
    \RightLabel{\scriptsize $WL$}
    \UnaryInfC{$r,q,p\vdash r$}
    \RightLabel{\scriptsize $\supset L$}
    \BinaryInfC{$q,\left( p\supset r\right) ,p\vdash r$}
    \RightLabel{\scriptsize $\supset L$}
    \BinaryInfC{$\left( p\supset q\right) ,\left( p\supset
               r\right) ,p\vdash r$}
    \RightLabel{\scriptsize $\wedge R$}
    \BinaryInfC{$\left( p\supset q\right) ,\left( p\supset
               r\right) ,p\vdash \left( q\wedge r\right) $}
    \RightLabel{\scriptsize $\supset R$}
    \UnaryInfC{$\left( p\supset q\right) ,\left( p\supset
              r\right) \vdash \left( p\supset \left( q\wedge
              r\right) \right) $}
    \RightLabel{\scriptsize $\wedge L$}
    \UnaryInfC{$\left( \left( p\supset q\right) \wedge
              \left( p\supset r\right) \right) \vdash \left(
              p\supset \left( q\wedge r\right) \right) $}
\end{prooftree}
```

If you want to run the tests, use this command:

```bash
stack test
```

## License

The MIT License (MIT)

Copyright (c) 2014 Joomy Korkut
