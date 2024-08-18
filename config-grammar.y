<letter> := <A> | <B> | <C> | <D> | <E> | <F> | <G> | <H> | <I> | <J> | <K> | <L> | <M> | <N> | <O> | <P> | <Q> | <R> | <S>  | <T> | <U> | <V> | <W> | <X> | <Y> | <Z> | <a> | <b> | <c> | <d> | <e> | <f> | <g> | <h> | <i> | <j> | <k> | <l>  | <m> | <n> | <o> | <p> | <q> | <r> | <s> | <t> | <u> | <v> | <w> | <x> | <y>

<symbol> := <.> | <:> | <;> | <(> | <)> | <'> | <"> | <#> | <@> | <+> | <|> | <`> | <[> | <{> | <&> | <=> | <}> | <]> | <*> | <!> | <%> | <$> | <~> | <\> | <-> | <_> | </> | <?> | <^>

<letterlist> := <letter> | <letter> <letterlist>

<character1> := <character> | <"> <'>

<character> := <letter> | <symbol> | <digit>

<digit> := <1> | <2> | <3> | <4> | <5> | <6> | <7> | <8> | <9> | <0>

<digitlist> := <digit> | <digit> <digitlist>

<characterlist> := <character> | <character> <characterlist>

<fractional_number> := <digitlist> | <digitlist> '.' <digitlist>

<opt_whitespace> := " " | " " <opt_whitespace>

<word> := {<opt_whitespace>}* <letterlist> {<opt_whitespace>}*

<keyword> := {<opt_whitespace>}* <letterlist> {<symbollist>}* {<opt_whitespace>}*
