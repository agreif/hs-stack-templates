{ mkDerivation, aeson, base, blaze-html, bytestring
, case-insensitive, classy-prelude, classy-prelude-conduit
, classy-prelude-yesod, conduit, conduit-extra, containers
, data-default, directory, esqueleto, fast-logger, file-embed
, foreign-store, hjsmin, hpack, hspec, http-client-tls
, http-conduit, mime-mail, monad-control, monad-logger, MonadRandom
, persistent, persistent-postgresql, persistent-template
, pwstore-fast, safe, shakespeare, stdenv, template-haskell, text
, time, transformers, unordered-containers, vector, wai, wai-extra
, wai-logger, warp, yaml, yesod, yesod-auth, yesod-auth-hashdb
, yesod-core, yesod-form, yesod-static, yesod-test
}:
mkDerivation {
  pname = "my-project";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base blaze-html bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
    containers data-default directory esqueleto fast-logger file-embed
    foreign-store hjsmin http-client-tls http-conduit mime-mail
    monad-control monad-logger MonadRandom persistent
    persistent-postgresql persistent-template pwstore-fast safe
    shakespeare template-haskell text time transformers
    unordered-containers vector wai wai-extra wai-logger warp yaml
    yesod yesod-auth yesod-auth-hashdb yesod-core yesod-form
    yesod-static
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base blaze-html bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
    containers data-default directory esqueleto fast-logger file-embed
    foreign-store hjsmin http-client-tls http-conduit mime-mail
    monad-control monad-logger MonadRandom persistent
    persistent-postgresql persistent-template pwstore-fast safe
    shakespeare template-haskell text time transformers
    unordered-containers vector wai wai-extra wai-logger warp yaml
    yesod yesod-auth yesod-auth-hashdb yesod-core yesod-form
    yesod-static
  ];
  testHaskellDepends = [
    aeson base blaze-html bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit conduit-extra
    containers data-default directory esqueleto fast-logger file-embed
    foreign-store hjsmin hspec http-client-tls http-conduit mime-mail
    monad-control monad-logger MonadRandom persistent
    persistent-postgresql persistent-template pwstore-fast safe
    shakespeare template-haskell text time transformers
    unordered-containers vector wai wai-extra wai-logger warp yaml
    yesod yesod-auth yesod-auth-hashdb yesod-core yesod-form
    yesod-static yesod-test
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
