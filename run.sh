ghc -o out/$1 days/$1.hs -odir out -hidir out \
    && ./out/$1