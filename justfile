cabal-update:
    wasm32-wasi-cabal update

repl target:
     wasm32-wasi-cabal repl {{target}}

all-repl:
    wasm32-wasi-cabal repl --enable-multi-repl all

ghciwatch:
    ghciwatch --command "wasm32-wasi-cabal repl --enable-multi-repl all" $(fd .hs --exec echo -n "--watch {//} ")

build exe:
    wasm32-wasi-cabal build exe:{{exe}}

# UNTESTED
reload exe: (bundle exe 'ln -sr')

generate-ffi exe:
    ./generate-jsffi.sh {{exe}}

bundle exe deploy='cp': (build exe) (generate-ffi exe)
    mkdir -p ./bundles/{{exe}}
    rm -rf ./bundles/{{exe}}/*
    {{deploy}} ./.jsffi/{{exe}}_ghc_wasm_jsffi.js ./bundles/{{exe}}/ghc_wasm_jsffi.js
    fd -I {{exe}}.wasm dist-newstyle --exec {{deploy}} {} ./bundles/{{exe}}/main.wasm
    {{deploy}} static/{{exe}}/* ./bundles/{{exe}}/

serve exe: (bundle exe 'ln -sr')
    http-server -c-1 --port 8001 ./bundles/{{exe}}

# UNTESTED
zip exe: (bundle exe)
    zip -rj {{exe}}.zip bundles/{{exe}}/*
    zip -d {{exe}}.zip '*~'

gild:
    fd .cabal --exec cabal-gild --io={}

mv-jfxr:
    mv ~/Downloads/*.jfxr static/ld59/

gen-jfxr-types:
    echo -e 'jsonTypeGenIO "static/ld59/Default 1.jfxr" "games/ld59/LD59/Jfxr/Types.hs" "LD59.Jfxr.Types" "JfxrDef" "jfxr"\n:q' | \
    wasm32-wasi-cabal repl json-type-gen
