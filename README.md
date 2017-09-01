ggflags
-------

flag geom for ggplot2

``` r
library(ggflags)
```

    ## Loading required package: ggplot2

``` r
set.seed(1234)
d <- data.frame(x=rnorm(50), y=rnorm(50), 
                country=sample(c("ar","fr", "nz", "gb", "es", "ca"), 50, TRUE), 
                stringsAsFactors = FALSE)
ggplot(d, aes(x=x, y=y, country=country, size=x)) + 
  geom_flag() + 
  scale_country()
```

![](README_files/figure-markdown_github/demo-1.png)

The flags in this fork of `ggflags`, from EmojiOne <https://github.com/eosrei/emojione-color-font>, are used under the [Creative Commons Attribution 4.0](https://github.com/eosrei/emojione-color-font/blob/master/LICENSE.md) licence.

![](README_files/figure-markdown_github/flags.png)

(note: proof of principle only)

### To-do

[ ] Ditch PNGs and use smaller, rescalable SVGs (or another vector format)
