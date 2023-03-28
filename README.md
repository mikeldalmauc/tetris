# tetris
Tetris game in elm

 # Compilar el código

 ```
 elm make src/Main.elm --output build/main.js
 ```
elm make elm/src/Main.elm --output build/tetris.js

elm make elm/src/Main.elm --output --optimize build/tetris.js

uglifyjs build/confeti.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output build/confeti.min.js
uglifyjs src/buscaminas.mjs --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output build/buscaminas.min.js

Si uglify no está disponible
npm install uglify-js --global

Para optimizar codigo para producción Leer: https://github.com/elm/compiler/blob/master/hints/optimize.md


# Instalar y guia de el
 https://guide.elm-lang.org/install/elm.html


# Arrancar la web
Live server sobre app.html 