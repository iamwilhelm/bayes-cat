---
layout: post
title:  "Entity Component System in Elm"
date:   2016-05-22 10:30:25 -0700
categories: architecture
---
When I originally started working on an Entity Component System (ECS) in Elm, it
didn't seem like there was an easy way to do it. When you have a generic
entity that has a common interface, the usual tool to reach for in OOP programs
is polymorphism. But that doesn't really work in Elm, as there's no such thing.

Apparently, in Functional Programs, there are things called typeclasses that
can help you with that. But Elm doesn't include typeclasses, because it's
considered to be too complicated.

Hence, I came up with a different way to implement an ECS, with some help from
readings around the web, like [Anatomy of a Knockout][anatomy-of-knockout]

There are three different parts in an ECS:

  - Entity: In-game objects that contain an ID and a list of components
  - Component: Tiny pieces of state that you can put together to represent the state of an entity
  - System: These are functions that operate on different combination of states for desired behavior

The general idea is to compose functionality and state that you need instead of
using OOP inheritance in order to make the instances that you need.

{% highlight elm %}
-- entity.elm

type alias ID = Int

type alias Model = {
    id : ID
  , components : List Component
  }
{% endhighlight %}

### Try the demo

You can try the [v0.0.3 demo here][v0.0.3], and the [source is here][v0.0.3-src].

wasd to move the cat around.

[anatomy-of-knockout]: https://www.chris-granger.com/2012/12/11/anatomy-of-a-knockout/
[home]: https://iamwilhelm.github.io/bayes-cat
[quasi-ecs]: https://gist.github.com/TheSeamau5/8d7fac9f1937f0bab317
[hacky-ecs]: https://gist.github.com/TheSeamau5/ec9695232ae919185f4d
[v0.0.3]: /bayes-cat/versions/v0.0.3/index.html
[v0.0.3-src]: https://github.com/iamwilhelm/bayes-cat/tree/v0.0.3
