ormolu -m inplace \
       $(find src hs-generator/src \
	      -type f \
	      -name "*.hs" \
	      ! -path "src/Settings.hs" \
       )
