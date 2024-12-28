(def document "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.2\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"100.0\" height=\"100.0\">
  <circle cx=\"50\" cy=\"50\" r=\"40\" stroke=\"indigo\" stroke-width=\"1\" fill=\"white\">
  </circle>
</svg>")

(spit "draws_out/babashka.svg" document)
