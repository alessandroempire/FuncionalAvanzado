module Main where

import LogoMachine
import qualified Data.Sequence as DS

a = Seq $ DS.fromList [ 
        Pd, Fd 100,
        Pc "Red", Rt 90, Fd 100,
        Pc "Yellow", Rt 90, Fd 100, Rt 90, Fd 100,
        Pc "Blue", Rt 135, Fd 142,
        Pu, Rt 135, Fd 200,
        Pd, Pc "Green", Fd 10
    ]

b = Seq $ DS.fromList [ 
        Pd, Fd 100,
        Rt 90, Fd 100, Rt 90, Fd 100, Rt 90, Fd 100,
        Rt 135, Fd 142, Pu, Rt 135, Fd 200, Pd, Fd 10
    ]

c = Seq $ DS.fromList [ 
        Pd, Pc "Yellow",
        Rep 4 $ DS.fromList [Fd 100, Rt 90]
    ]
d = Seq $ DS.fromList [
        Pd, Pc "Yellow", 
        Rep 36 $ DS.fromList [
            Rep 4 $ DS.fromList [ Fd 200, Rt 90 ],
            Rt 10
        ]
    ]

e = Seq $ DS.fromList [ 
        Pd, Pc "Red", Say "Baz",
        Pu, Fd 50, Pd, Pc "Blue", Say "Bar",
        Pu, Fd 50, Pd, Pc "Yellow", Say "Baz"
	]

f = Seq $ DS.fromList [
        Lt 90, Fd 50, Rt 90, Fd 50, Pc "Yellow", Pd, Say "Foo",
        Pu, Rt 90, Fd 100, Pc "Red", Pd, Say "Bar",
        Pu, Rt 90, Fd 100, Pc "Blue", Pd, Say "Baz",
        Pu, Rt 90, Fd 100, Pc "Green", Pd, Say "Qux",
        Pu, Home, Pd, Pc "White", Pd, Say "Home",
        Fd 150, Rt 90, Fd 150, Rep 4 $ DS.fromList [ Rt 90, Fd 300 ]
    ]

main = runLogoProgram 400 400 "Test" a
