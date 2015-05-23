> nfa1 = NFA {
>              sigma  = DS.fromList "ab",
>              states = DS.fromList $ fmap Node [0..3],
>              moves  = DS.fromList [
>                Move { from = Node 0, to = Node 0, sym = 'b' },
>                Move { from = Node 0, to = Node 1, sym = 'a' },
>              Lambda { from = Node 0, to = Node 2 },
>                Move { from = Node 1, to = Node 1, sym = 'a' },
>                Move { from = Node 1, to = Node 1, sym = 'b' },
>                Move { from = Node 1, to = Node 3, sym = 'b' },
>                Move { from = Node 2, to = Node 2, sym = 'a' },
>                Move { from = Node 2, to = Node 1, sym = 'b' },
>              Lambda { from = Node 2, to = Node 3 },
>                Move { from = Node 3, to = Node 3, sym = 'a' },
>                Move { from = Node 3, to = Node 2, sym = 'b' }
>              ],
>              initial = Node 0,
>              final = DS.fromList [ Node 4 , Node 0]

--sample' $ (arbitrary :: Gen NFA)
