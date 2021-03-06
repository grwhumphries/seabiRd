
<!-- README.md is generated from README.Rmd. Please edit that file -->

# seabiRd

<!-- badges: start -->

<!-- badges: end -->

A ggplot2 color palette based on some colourful Seabirds\!

![](https://media.giphy.com/media/eJFGbhHyjCHHiXYgcl/giphy.gif)

The package includes palettes for 8 species and is based on
“asteves/tayloRswift”

## Installation

You can install the released version of seabiRd with:

``` r
remotes::install_github("grwhumphries/seabiRd")
```

## Examples

### Fill by discrete variables

``` r
ggplot(x)+
  geom_bar(aes(x=object,y=values,fill=object),stat="identity",color="black")+
  scale_fill_seabird(palette = "atlanticpuffin")+
  theme_minimal()
```

<img src="man/figures/README-example-default-1.png" width="100%" />

### Color by discrete variable

``` r
ggplot(penguins, aes(bill_depth_mm, bill_length_mm, color = species)) +
  geom_point(size = 4) +
  scale_color_seabird(palette = "wavedalbatross")+
  theme_minimal()
#> Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="man/figures/README-example-palette-1.png" width="100%" />

## Palettes

![`bluefootedbooby`](https://upload.wikimedia.org/wikipedia/commons/thumb/7/7a/Blue-footed_Booby_%284885194224%29.jpg/449px-Blue-footed_Booby_%284885194224%29.jpg)  
`bluefootedbooby` = “\#c3eaf1”, “\#dcd2d1”, “\#3f4555”, “\#89634c”,
“\#bdc09d”, “\#704d3d”

![`atlanticpuffin`](https://upload.wikimedia.org/wikipedia/commons/thumb/b/b7/Atlantic_Puffin.jpg/800px-Atlantic_Puffin.jpg)

`atlanticpuffin` = “\#e38643”, “\#100f19”, “\#f4f2f1”, “\#2c2a38”,
“\#c1012c”, “\#875309”

![`kingpenguin`](https://upload.wikimedia.org/wikipedia/commons/thumb/b/be/SGI-2016-South_Georgia_%28Fortuna_Bay%29%E2%80%93King_penguin_%28Aptenodytes_patagonicus%29_04.jpg/800px-SGI-2016-South_Georgia_%28Fortuna_Bay%29%E2%80%93King_penguin_%28Aptenodytes_patagonicus%29_04.jpg)

`kingpenguin` = “\#1d1a18”, “\#eaf3f1”, “\#3d373b”, “\#ea9200”,
“\#9db2bd”, “\#ee964d”

![`wavedalbatross`](https://upload.wikimedia.org/wikipedia/commons/thumb/2/22/Waved_Albatross_pair.jpg/1024px-Waved_Albatross_pair.jpg)

`wavedalbatross` = “\#e6e7e2”, “\#d9b20c”, “\#d4bb87”, “\#928e84”,
“\#504531”, “\#291914”

![`whiskeredauklet`](https://upload.wikimedia.org/wikipedia/commons/2/23/Whiskered_Auklet.jpg)

`whiskeredauklet` = “\#4e5051”, “\#c13214”, “\#a49899”, “\#151616”,
“\#a7a5ae”, “\#f4f2f1”

![`incatern`](https://upload.wikimedia.org/wikipedia/commons/thumb/d/dc/Larosterna_inca_%28Inca_Tern_-_Inkaseeschwalbe%29_Weltvogelpark_Walsrode_2012-015.jpg/1920px-Larosterna_inca_%28Inca_Tern_-_Inkaseeschwalbe%29_Weltvogelpark_Walsrode_2012-015.jpg)

`incatern` = “\#e92b19”, “\#e7ca2b”, “\#f4f2f1”, “\#757f8f”, “\#333842”,
“\#c74611”

![`chileanskua`](https://cdn.download.ams.birds.cornell.edu/api/v1/asset/115457111/1800)

`chileanskua` = “\#302b2b”, “\#543f35”, “\#f8dcad”, “\#8a563d”,
“\#a88b75”, “\#7a6356”

![`redleggedkittiwake`](https://cdn.download.ams.birds.cornell.edu/api/v1/asset/169492361/1800)

`redleggedkittiwake` = “\#ddd58d”, “\#f4f2f1”, “\#96979d”, “\#d5d7d8”,
“\#414038”, “\#ac2c22”

## Inspiration

This package was inspired by
[asteves/tayloRswift](https://github.com/asteves/tayloRswift).
