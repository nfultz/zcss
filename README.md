# zcss - css for lattice and ggplot2

Inspired by tkonopka's Rcss, this package provides similar functionality
for `ggplot2` and `lattice`; however, usage and implementation differs substantially.
Instead of prepending each function with Rcss, instead we generate wrapper 
objects in their own environment. 

This can be used in three ways:

```{r}
require(zcss)
style <- zcss("test.css")

# Invoke through the dollar
style$plot(mtcars$mpg, mtcars$hp)
style$abline(h=mean(mtcars$hp))
style$abline(v=mean(mtcars$mpg))

# Inside a with block
with(style, {
  plot(mtcars$mpg, mtcars$hp)
  abline(h=mean(mtcars$hp))
  abline(v=mean(mtcars$mpg))
})

#Attach a style
attach(style)

plot(mtcars$mpg, mtcars$hp)
abline(h=mean(mtcars$hp))
abline(v=mean(mtcars$mpg))

#compatible with ggplot2
ggplot(mtcars) + aes(mpg, hp) + style$geom_points() + style$theme()

```

