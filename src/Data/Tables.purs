module Tables where

type Basis = Array String

type CayleyTable = Array (Array String)

basis :: Basis
basis = ["e", "e1", "e2", "e3", "e4", "e23", "e31", "e12", "e43", "e42", "e41", "e321", "e124", "e314", "e234", "e1234"]

scBasis :: Basis
scBasis = ["e"]

vecBasis :: Basis
vecBasis = ["e1", "e2", "e3", "e4"]

biBasis :: Basis
biBasis = ["e23", "e31", "e12", "e43", "e42", "e41"]

triBasis :: Basis
triBasis = ["e321", "e124", "e314", "e234"]

psBasis :: Basis
psBasis = ["e1234"]

moBasis :: Basis
moBasis = ["e", "e23", "e31", "e12"
          , "e43", "e42", "e41", "e1234"]

flBasis :: Basis
flBasis = ["e1", "e2", "e3", "e4"
          , "e321", "e124", "e314", "e234"]

-- Empty Array of Strings
emptyAS :: Basis
emptyAS = ["", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""]

tabsize = 16 :: Int

type NamedTable = {name :: String, tab:: CayleyTable}

tables :: Array NamedTable
tables =
  [ {name: "gp",  tab: gpTab}
  , {name: "xp",  tab: xpTab}
  , {name: "gap", tab: gapTab}
  , {name: "xap", tab: xapTab}
  ]

-- More tables.
type MoreTables =
  {name :: String, tab:: CayleyTable, x1:: Basis, x2:: Basis}

moreTables :: Array MoreTables
moreTables =
  -- Geometric product
  [ {name: "gp", tab: gpTab, x1: basis, x2: basis}

  -- Exterior products vectors
  , {name: "xpvv", tab: xpTab, x1: vecBasis, x2: vecBasis}
  , {name: "xpvb", tab: xpTab, x1: vecBasis, x2: biBasis}
  , {name: "xpvt", tab: xpTab, x1: vecBasis, x2: triBasis}
  -- Exterior products bivectors
  , {name: "xpbv", tab: xpTab, x1: biBasis, x2: vecBasis}
  , {name: "xpbb", tab: xpTab, x1: biBasis, x2: biBasis}
  , {name: "xpbt", tab: xpTab, x1: biBasis, x2: triBasis}
  -- Exterior products trivectors
  , {name: "xptv", tab: xpTab, x1: triBasis, x2: vecBasis}
  , {name: "xptb", tab: xpTab, x1: triBasis, x2: biBasis}
  , {name: "xptt", tab: xpTab, x1: triBasis, x2: triBasis}
  -- Exterior product others
  , {name: "xp", tab: xpTab, x1: basis, x2: basis}

  -- Exterior antiproducts vectors
  , {name: "xapvv", tab: xapTab, x1: vecBasis, x2: vecBasis}
  , {name: "xapvb", tab: xapTab, x1: vecBasis, x2: biBasis}
  , {name: "xapvt", tab: xapTab, x1: vecBasis, x2: triBasis}
  -- Exterior antiproducts bivectors
  , {name: "xapbv", tab: xapTab, x1: biBasis, x2: vecBasis}
  , {name: "xapbb", tab: xapTab, x1: biBasis, x2: biBasis}
  , {name: "xapbt", tab: xapTab, x1: biBasis, x2: triBasis}
  -- Exterior antiproducts trivectors
  , {name: "xaptv", tab: xapTab, x1: triBasis, x2: vecBasis}
  , {name: "xaptb", tab: xapTab, x1: triBasis, x2: biBasis}
  , {name: "xaptt", tab: xapTab, x1: triBasis, x2: triBasis}
  -- Exterior antiproduct multivectors
  , {name: "xap", tab: xapTab, x1: basis, x2: basis}

   -- Geometric antiproducts motor
  , {name: "gapmv", tab: gapTab, x1: moBasis, x2: vecBasis}
  , {name: "gapmb", tab: gapTab, x1: moBasis, x2: biBasis}
  , {name: "gapmt", tab: gapTab, x1: moBasis, x2: triBasis}
   -- Geometric antiproducts flector
  , {name: "gapfv", tab: gapTab, x1: flBasis, x2: vecBasis}
  , {name: "gapfb", tab: gapTab, x1: flBasis, x2: biBasis}
  , {name: "gapft", tab: gapTab, x1: flBasis, x2: triBasis}
  -- Geometric antiproducts multivec
  , {name: "gapgm", tab: gapTab, x1: basis, x2: moBasis}
  , {name: "gapgf", tab: gapTab, x1: basis, x2: flBasis}
  , {name: "gap", tab: gapTab, x1: basis, x2: basis}
  ]

-- Could replace "1" with "e", uniform with algorithm.
-- Geometric product Cayley Table
gpTab :: CayleyTable
gpTab =
  [ ["1",  "e1", "e2", "e3", "e4",              "e23", "e31", "e12", "e43", "e42", "e41",      "e321", "e124", "e314", "e234", "e1234"]

  , ["e1",   "1", "e12", "-e31", "-e41",        "-e321", "-e3", "e2", "e314", "-e124", "-e4",  "-e23", "-e42", "e43", "e1234", "e234"]
  , ["e2",   "-e12", "1", "e23", "-e42",        "e3", "-e321", "-e1", "-e234", "-e4", "e124",  "-e31", "e41", "e1234", "-e43", "e314"]
  , ["e3",   "e31", "-e23", "1", "-e43",        "-e2", "e1", "-e321", "-e4", "e234", "-e314",  "-e12", "e1234", "-e41", "e42", "e124"]
  , ["e4",   "e41", "e42", "e43", "0",          "e234", "e314", "e124", "0", "0", "0",         "e1234", "0", "0", "0",         "0"]

  , ["e23",   "-e321", "-e3", "e2", "e234",     "-1", "-e12", "e31", "e42", "-e43", "-e1234",  "e1", "e314", "-e124", "-e4",   "e41"]
  , ["e31",   "e3", "-e321", "-e1", "e314",     "e12", "-1", "-e23", "-e41", "-e1234", "e43",  "e2", "-e234", "-e4", "e124",   "e42"]
  , ["e12",   "-e2", "e1", "-e321", "e124",     "-e31", "e23", "-1", "-e1234", "e41", "-e42",  "e3", "-e4", "e234", "-e314",   "e43"]
  , ["e43",   "e314", "-e234", "e4", "0",       "-e42", "e41", "-e1234", "0", "0", "0",        "-e124", "0", "0", "0",         "0"]
  , ["e42",   "-e124", "e4", "e234", "0",       "e43", "-e1234", "-e41", "0", "0", "0",        "-e314", "0", "0", "0",         "0"]
  , ["e41",   "e4", "e124", "-e314", "0",       "-e1234", "-e43", "e42", "0", "0", "0",        "-e234", "0", "0", "0",         "0"]

  , ["e321",  "-e23", "-e31", "-e12", "-e1234", "e1", "e2", "e3", "e124", "e314", "e234",      "-1", "-e43", "-e42", "-e41",   "e4"]
  , ["e124",  "-e42", "e41", "-e1234", "0",     "-e314", "e234", "-e4", "0", "0", "0",         "e43", "0", "0", "0",           "0"]
  , ["e314",  "e43", "-e1234", "-e41", "0",     "e124", "-e4", "-e234", "0", "0", "0",         "e42", "0", "0", "0",           "0"]
  , ["e234",  "-e1234", "-e43", "e42", "0",     "-e4", "-e124", "e314", "0", "0", "0",         "e41", "0", "0", "0",           "0"]

  , ["e1234", "-e234", "-e314", "-e124", "0",   "e41", "e42", "e43", "0", "0", "0",            "-e4", "0", "0", "0",           "0"]
  ]

-- Exterior product Cayley Table
xpTab :: CayleyTable
xpTab =
  [ ["0",  "e1", "e2", "e3", "e4",              "e23", "e31", "e12", "e43", "e42", "e41",      "e321", "e124", "e314", "e234", "e1234"]

  , ["e1",  "0", "e12", "-e31", "-e41",         "-e321", "0", "0", "e314", "-e124", "0",       "0", "0", "0", "e1234",         "0"]
  , ["e2",  "-e12", "0", "e23", "-e42",         "0", "-e321", "0", "-e234", "0", "e124",       "0", "0", "e1234", "0",         "0"]
  , ["e3",  "e31", "-e23", "0", "-e43",         "0", "0", "-e321", "0", "e234", "-e314",       "0", "e1234", "0", "0",         "0"]
  , ["e4",  "e41", "e42", "e43", "0",           "e234", "e314", "e124", "0", "0", "0",         "e1234", "0", "0", "0",         "0"]

  , ["e23",  "-e321", "0", "0", "e234",         "0", "0", "0", "0", "0", "-e1234",             "0", "0", "0", "0",             "0"]
  , ["e31",  "0", "-e321", "0", "e314",         "0", "0", "0", "0", "-e1234", "0",             "0", "0", "0", "0",             "0"]
  , ["e12",  "0", "0", "-e321", "e124",         "0", "0", "0", "-e1234", "0", "0",             "0", "0", "0", "0",             "0"]
  , ["e43",  "e314", "-e234", "0", "0",         "0", "0", "-e1234", "0", "0", "0",             "0", "0", "0", "0",             "0"]
  , ["e42",  "-e124", "0", "e234", "0",         "0", "-e1234", "0", "0", "0", "0",             "0", "0", "0", "0",             "0"]
  , ["e41",  "0", "e124", "-e314", "0",         "-e1234", "0", "0", "0", "0", "0",             "0", "0", "0", "0",             "0"]

  , ["e321",  "0", "0", "0", "-e1234",          "0", "0", "0", "0", "0", "0",                  "0", "0", "0", "0",             "0"]
  , ["e124",  "0", "0", "-e1234", "0",          "0", "0", "0", "0", "0", "0",                  "0", "0", "0", "0",             "0"]
  , ["e314",  "0", "-e1234", "0", "0",          "0", "0", "0", "0", "0", "0",                  "0", "0", "0", "0",             "0"]
  , ["e234",  "-e1234", "0", "0", "0",          "0", "0", "0", "0", "0", "0",                  "0", "0", "0", "0",             "0"]

  , ["e1234", "0", "0", "0", "0",               "0", "0", "0", "0", "0", "0",                  "0", "0", "0", "0",             "0"]
  ]

-- Geometric antiproduct Cayley table
gapTab :: CayleyTable
gapTab =
  [ ["0",     "0", "0", "0", "e321",            "0", "0", "0", "e12", "e31", "e23",            "0", "e3", "e2", "e1",          "e"]

  , ["0",     "0", "0", "0", "-e23",            "0", "0", "0", "-e2", "e3", "-e321",           "0", "e31", "-e12", "e",        "e1"]
  , ["0",     "0", "0", "0", "-e31",            "0", "0", "0", "e1", "-e321", "-e3",           "0", "-e23", "e", "e12",        "e2"]
  , ["0",     "0", "0", "0", "-e12",            "0", "0", "0", "-e321", "-e1", "e2",           "0", "e", "e23", "-e31",        "e3"]
  , ["-e321", "e23", "e31", "e12", "-e1234",    "-e1", "-e2", "-e3", "e124", "e314", "e234",   "e", "-e43", "-e42", "-e41",    "e4"]

  , ["0",     "0", "0", "0", "e1",              "0", "0", "0", "-e31", "e12", "-e",            "0", "-e2", "e3", "-e321",      "e23"]
  , ["0",     "0", "0", "0", "e2",              "0", "0", "0", "e23", "-e", "-e12",            "0", "e1", "-e321", "-e3",      "e31"]
  , ["0",     "0", "0", "0", "e3",              "0", "0", "0", "-e", "-e23", "e31",            "0", "-e321", "-e1", "e2",      "e12"]
  , ["e12",   "e2", "-e1", "-e321", "e124",     "e31", "-e23", "-e", "-e1234", "-e41", "e42",  "e3", "-e4", "-e234", "e314",   "e43"]
  , ["e31",   "-e3", "-e321", "e1", "e314",     "-e12", "-e", "e23", "e41", "-e1234", "-e43",  "e2", "e234", "-e4", "-e124",   "e42"]
  , ["e23",   "-e321", "e3", "-e2", "e234",     "-e", "e12", "-e31", "-e42", "e43", "-e1234",  "e1", "-e314", "e124", "-e4",   "e41"]

  , ["0",     "0", "0", "0", "-e",              "0", "0", "0", "e3", "e2", "e1",               "0", "-e12", "-e31", "-e23",    "e321"]
  , ["-e3",   "e31", "-e23", "-e", "-e43",      "-e2", "e1", "e321", "-e4", "-e234", "e314",   "e12", "e1234", "e41", "-e42",  "e124"]
  , ["-e2",   "-e12", "-e", "e23", "-e42",      "e3", "e321", "-e1", "e234", "-e4", "-e124",   "e31", "-e41", "e1234", "e43",  "e314"]
  , ["-e1",   "-e", "e12", "-e31", "-e41",      "e321", "-e3", "e2", "-e314", "e124", "-e4",    "e23", "e42", "-e43", "e1234", "e234"]

  , ["e",     "e1", "e2", "e3", "e4",           "e23", "e31", "e12", "e43", "e42", "e41",      "e321", "e124", "e314", "e234", "e1234"]
  ]

-- Exterior antiproduct Cayley table
xapTab :: CayleyTable
xapTab =
  [ ["0",     "0", "0", "0", "0",               "0", "0", "0", "0", "0", "0",                  "0", "0", "0", "0",             "e"]

  , ["0",     "0", "0", "0", "0",               "0", "0", "0", "0", "0", "0",                  "0", "0", "0", "e",             "e1"]
  , ["0",     "0", "0", "0", "0",               "0", "0", "0", "0", "0", "0",                  "0", "0", "e", "0",             "e2"]
  , ["0",     "0", "0", "0", "0",               "0", "0", "0", "0", "0", "0",                  "0", "e", "0", "0",             "e3"]
  , ["0",     "0", "0", "0", "0",               "0", "0", "0", "0", "0", "0",                  "e", "0", "0", "0",             "e4"]

  , ["0",     "0", "0", "0", "0",               "0", "0", "0", "0", "0", "-e",                 "0", "-e2", "e3", "0",          "e23"]
  , ["0",     "0", "0", "0", "0",               "0", "0", "0", "0", "-e", "0",                 "0", "e1", "0", "-e3",          "e31"]
  , ["0",     "0", "0", "0", "0",               "0", "0", "0", "-e", "0", "0",                 "0", "0", "-e1", "e2",          "e12"]
  , ["0",     "0", "0", "0", "0",               "0", "0", "-e", "0", "0", "0",                 "e3", "-e4", "0", "0",          "e43"]
  , ["0",     "0", "0", "0", "0",               "0", "-e", "0", "0", "0", "0",                 "e2", "0", "-e4", "0",          "e42"]
  , ["0",     "0", "0", "0", "0",               "-e", "0", "0", "0", "0", "0",                 "e1", "0", "0", "-e4",          "e41"]

  , ["0",     "0", "0", "0", "-e",              "0", "0", "0", "e3", "e2", "e1",               "0", "-e12", "-e31", "-e23",    "e321"]
  , ["0",     "0", "0", "-e", "0",              "-e2", "e1", "0", "-e4", "0", "0",             "e12", "0", "e41", "-e42",      "e124"]
  , ["0",     "0", "-e", "0", "0",              "e3", "0", "-e1", "0", "-e4", "0",             "e31", "-e41", "0", "e43",      "e314"]
  , ["0",     "-e", "0", "0", "0",              "0", "-e3", "e2", "0", "0", "-e4",             "e23", "e42", "-e43", "0",      "e234"]

  , ["e",     "e1", "e2", "e3", "e4",           "e23", "e31", "e12", "e43", "e42", "e41",      "e321", "e124", "e314", "e234", "0"]
  ]
