# What

The `themes` directory is special in that it does not directly contain configs,
except it contains further directories which contain *theme-specific* configs.

This started as a not so serious 'would be nice to have' idea, but has now
actually become something usable.

The idea was that it would be nice to be able to switch 'themes' *without
affecting the state of the repository*. By which I mean being able to change the
themes of applications without it making the git status 'dirty', i.e. there
would be no need to make a new commit and update the 'currently chosen theme'
everytime I wanted to switch themes. And that of course would allow switching
themes multiples times a day, compared to maybe a few times a week or month.

But what does it mean to change the 'theme' of an application? It predominantly
means to simply change the colours. As you can imagine, this depends on the
application and whether or not it provides a declarative way to set colours.

# How
