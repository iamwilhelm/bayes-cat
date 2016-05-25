---
layout: post
title:  "Collision Detection in Elm"
date:   2016-05-25 10:56:00 -0700
categories: architecture
---

*Note: this post talks about [version v0.0.4][v0.0.4-src]*

Collision detection is a core system in most interactive games. Otherwise, game
world entities would just pass through each other. The naive implementation
for collision detection is to compare every collidable entity with every other
collidable entity. This results in an O(n^2) algorithm, which is extremely slow
when you add even just a moderate amount of entities in the world. There are
other algorithms like [Quad Trees][quadtree] that can minimize the cost of
doing collision detection, but for [v0.0.4][v0.0.4], I just implemented the
naive version just to get started.

When a collision is detected in typical game loops for imperative languages,
both entities are updated right there and then. However, I decided to separate the
detection of the collision and updating the entities that collided.

### Other similar architectures

In [Redux][redux] and the [Elm Architecture][elm-architecture], the flow of data
and the update of state is easy to understand:

- The entire application state and a message to update the state is passed into the top level `#update` function.
- The update method peels off the top level message and passes the sub-component message into the sub-component `#update` method
- Until it gets to a leaf component, which then changes the state.
- If during the course of processing a message, an event occurs where we need to
change state, but doesn't belong in that message, we generate another command.
- That command gets sent back to the top of the `#update` function.

This architecture has some properties that make the code easy to understand.

1. The flow of messages is unidirectional.
2. The message routing is hierarchical, so we are updating a specific part
of the application state.
3. Each layer of application state, and the code that updates it only need to
worry about that layer. This makes components easy to compose.

### Back to collision detection

Here is the top-level `#update` method

[main.elm](https://github.com/iamwilhelm/bayes-cat/blob/v0.0.4/src/main.elm#L70)
{% highlight haskell %}
type Msg =
    SizeChange Window.Size
  | Tick Float
  | Player Entity.Cat.Msg
  | Egg Entity.Egg.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick dt ->
      (step dt model, Cmd.batch <| effects dt model)
    SizeChange size ->
      { model | size = size } ! []
    Player catMsg ->
      map (System.Control.control Entity.Role.Cat (Entity.Cat.reduce catMsg)) model ! []
    Egg eggMsg ->
      map (Entity.Egg.reduce eggMsg) model ! []
    NoOp ->
      model ! []
{% endhighlight %}

Focus on the `Tick` message. That's where we use `#step` to update the positions
 of all entities and `#effects` to generate the next commands that will be sent
 to the top of the update function again. It's inside `#effects` that we look for collisions.

In a collision, there are two entities that need to be updated. But a collision
detection occurs during a `Tick` message. If we update either one of the entities in the
`Tick` message, it breaks the convention of only updating the part of the
application state the message intends to update. That would unnecessarily complicate
the `#update` method.

[main.elm](https://github.com/iamwilhelm/bayes-cat/blob/v0.0.4/src/main.elm#L106)
{% highlight haskell %}
effects : Float -> Model -> List (Cmd Msg)
effects dt model =
  List.filterMap (System.Collision.detect interact) (pairs model.entities)
{% endhighlight %}

So instead we return a command to update the specific collided entities after
we detect a collision between entities. That way, the detection of the collision
is decoupled from the updating of the entities.

Only three things to note in `#effects`:

1. `#pairs` returns pairs of model entities. It's the naive implementation.
2. `System.Collision.detect` checks whether two entities are touching.
3. `#interact` is a top-level function because it needs access to top level message

[main.elm](https://github.com/iamwilhelm/bayes-cat/blob/v0.0.4/src/main.elm#L106)
{% highlight haskell %}
interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (role1, entity1) (role2, entity2) =
  case role1 of
    Entity.Role.Cat ->
      Cmd.map Player <| Entity.Cat.interact (role1, entity1) (role2, entity2)
    Entity.Role.Egg ->
      Cmd.map Egg <| Entity.Egg.interact (role1, entity1) (role2, entity2)
{% endhighlight %}

Like `#update`, where as you descend into sub-components, you strip off the
parent messages to pass child messages, you need to do the same thing with
`#interact`. That's why we use `Cmd.map` to put the parent message `Player` and
`Egg` back on top of the child message returned by `#Entity.Cat.interact` and
`#Entity.Egg.interact`.

In the entity instances itself, you now just need to worry about the messages at
the entity level. That was the single biggest realization. You can keep each
entity's `#interact` self contained, by deconstructng the messages
using `case` as you travel down the hierarchy of `#interact`s, and reconstructing
them as you travel up the hierarchy of `#interact`s.

[entity/cat.elm](https://github.com/iamwilhelm/bayes-cat/blob/v0.0.4/src/entity/cat.elm#L71)
{% highlight haskell %}
interact : (Entity.Role.Name, Entity.Model) -> (Entity.Role.Name, Entity.Model) -> Cmd Msg
interact (selfRole, self) (otherRole, other) =
  case otherRole of
    Entity.Role.Egg ->
      Task.perform never identity (Task.succeed Grow)
    _ ->
      Task.perform never identity (Task.succeed NoOp)
{% endhighlight %}

It's not intuitive from the docs and API references, but the way you create
Cmd msg is with tasks that always succeed: `Task.perform never identity (Task.succeed Grow)`

Also, `#interact` here is about what other entities do to the cat entity in the
event of a collision, not what the cat entity does to other entities. This is because
the top-level `#interact` will tack on the `Cat` parent message on top, so all
messages need to be about Cat.

[quadtree]: http://gamedevelopment.tutsplus.com/tutorials/quick-tip-use-quadtrees-to-detect-likely-collisions-in-2d-space--gamedev-374
[redux]: https://github.com/reactjs/redux
[elm-architecture]: http://guide.elm-lang.org/architecture/index.html
[v0.0.4]: /bayes-cat/versions/v0.0.4/index.html
[v0.0.4-src]: https://github.com/iamwilhelm/bayes-cat/tree/v0.0.4
