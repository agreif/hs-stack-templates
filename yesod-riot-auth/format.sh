ormolu -m inplace \
       $(find src hs-generator \
	      -type f \
	      -name "*.hs" \
	      ! -path "src/Settings.hs" \
       )
