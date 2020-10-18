FOR /L %%A IN (1,1,100) DO (
    ghc generator.hs
    generator.exe
)
exit