module AntiTable where



{- -- Geometric antiproduct Cayley table
gapTab :: CayleyTable
gapTab =
  [ ["0", "0", "0", "0", "e321", "0", "0", "0", "e12", "e31", "e23", "0", "e3", "e2", "e1", "e"]

  , ["0", "0", "0", "0", "-e23", "0", "0", "0", "-e2", "e3", "-e321", "0", "e31", "-e12", "e", "e1"]
  , ["0", "0", "0", "0", "-e31", "0", "0", "0", "e1", "-e321", "-e3", "0", "-e23", "e", "e12", "e2"]
  , ["0", "0", "0", "0", "-e12", "0", "0", "0", "-e321", "-e1", "e2", "0", "e", "e23", "-e31", "e3"]
  , ["-e321", "e23", "e31", "e12", "-e1234", "-e1", "-e2", "-e3", "e124", "e314", "e234", "e", "-e43", "-e42", "-e41", "e4"]

  , ["0", "0", "0", "0", "e1", "0", "0", "0", "-e31", "e12", "-e", "0", "-e2", "e3", "-e321", "e23"]
  , ["0", "0", "0", "0", "e2", "0", "0", "0", "e23", "-e", "-e12", "0", "e1", "-e321", "-e3", "e31"]
  , ["0", "0", "0", "0", "e3", "0", "0", "0", "-e", "-e23", "e31", "0", "-e321", "-e1", "e2", "e12"]
  , ["e12", "e2", "-e1", "-e321", "e124", "e31", "-e23", "-e", "-e1234", "-e41", "e42", "e3", "-e4", "-e234", "e314", "e43"]
  , ["e31", "-e3", "-e321", "e1", "e314", "-e12", "-e", "e23", "e41", "-e1234", "-e43", "e2", "e234", "-e4", "-e124", "e42"]
  , ["e23", "-e321", "e3", "-e2", "e234", "-e", "e12", "-e31", "-e42", "e43", "-e1234", "e1", "-e314", "e124", "-e4", "e41"]

  , ["0", "0", "0", "0", "-e", "0", "0", "0", "e3", "e2", "e1", "0", "-e12", "-e13", "-e23", "e321"]
  , ["-e3", "e31", "-e23", "-e", "-e43", "-e2", "e1", "e321", "-e4", "-e234", "e314", "e12", "e1234", "e41", "-e42", "e124"]
  , ["-e2", "-e12", "-e", "e23", "-e42", "e3", "e321", "-e1", "e234", "-e4", "-e124", "e31", "-e41", "e1234", "e43", "e314"]
  , ["-e1", "-e", "e12", "-e31", "-e41", "e321", "e3", "e2", "-e314", "e124", "-e4", "e23", "e42", "-e43", "e1234", "e234"]

  , ["e", "e1", "e2", "e3", "e4", "e23", "e31", "e12", "e43", "e42", "e41", "e321", "e124", "e314", "e234", "e1234"]
  ]

-- Exterior antiproduct Cayley table
xapTab :: CayleyTable
xapTab =
  [ ["0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "e"]

  , ["0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "e", "e1"]
  , ["0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "e", "0", "e2"]
  , ["0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "e", "0", "0", "e3"]
  , ["0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "e", "0", "0", "0", "e4"]

  , ["0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "-e", "0", "-e2", "e3", "0", "e23"]
  , ["0", "0", "0", "0", "0", "0", "0", "0", "0", "-e", "0", "0", "e1", "0", "-e3", "e31"]
  , ["0", "0", "0", "0", "0", "0", "0", "0", "-e", "0", "0", "0", "0", "-e1", "e2", "e12"]
  , ["0", "0", "0", "0", "0", "0", "0", "-e", "0", "0", "0", "e3", "-e4", "0", "0", "e43"]
  , ["0", "0", "0", "0", "0", "0", "-e", "0", "0", "0", "0", "e2", "0", "-e4", "0", "e42"]
  , ["0", "0", "0", "0", "0", "-e", "0", "0", "0", "0", "0", "e1", "0", "0", "-e4", "e41"]

  , ["0", "0", "0", "0", "-e", "0", "0", "0", "e3", "e2", "e1", "0", "-e12", "-e13", "-e23", "e321"]
  , ["0", "0", "0", "-e", "0", "-e2", "e1", "0", "-e4", "0", "0", "e12", "0", "e41", "-e42", "e124"]
  , ["0", "0", "-e", "0", "0", "e3", "0", "-e1", "0", "-e4", "0", "e31", "-e41", "0", "e43", "e314"]
  , ["0", "-e", "0", "0", "0", "0", "-e3", "e2", "0", "0", "-e4", "e23", "e42", "-e43", "0", "e234"]

  , ["e", "e1", "e2", "e3", "e4", "e23", "e31", "e12", "e43", "e42", "e41", "e321", "e124", "e314", "e234", "0"]
  ]
 -}
