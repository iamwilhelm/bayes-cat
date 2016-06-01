---
layout: post
title:  "On Cameras and Transforms"
date:   2016-06-01 16:04:00 -0700
categories: graphics
---

When you first get started, you plot the world coordinates of the entities
directly. But this won't work when you have a camera that moves around
independently of the entities in the world. This is where affine transforms
come into play to make it much more manageable.

Affine Transforms are covered elsewhere on the web, so brush up on it, if you
haven't done so already.

To reiterate an important point: when affine transforms are multiplied together,
the right most matrix is applied first.

```
M = T * R * S
```

where `T = translation matrix` `R = rotation matrix` and `S = scaling matrix`.
We most typically get the affine transform of an entity by multiplying in this
order, because we don't want the transforms to affect each other. Reading right
to left, we apply scaling first, then rotation, and finally translation.

If done in a different order, then rotation and translation would affect scaling,
and it's not what you expect.

### What is a Camera?

The camera is modeled as an entity that isn't viewable. There was just a single
workaround: searching the list
of entities for the camera on every tick. For now, it's fine, but otherwise,
the camera is treated like any other entity.

### Renderer system will handle it

I had wondered if we needed a camera system. Systems should never refer or
import each other, to stay decoupled and easy to understand. As I worked through
it, the camera transform would be tightly coupled with the renderer system,
so I decided they were probably the same thing. Besides, all the camera system
was doing was providing the camera transform.

What does the renderer system look like now? Well, it still asks the entities
to render themselves, but it pulls out the responsibility of transforming to
the world coordinates from the entities themselves, and it's in the renderer now.

Entities now return a `Maybe (List Form)`, rather than a `Maybe Form`, because
you no longer have to group the forms. The renderer will do that for you, since
it has to do a `Collage.groupTransform` call anyway.

[entity/egg.elm](https://github.com/iamwilhelm/bayes-cat/blob/v0.0.6/src/entity/egg.elm#L72)
{% highlight haskell %}
view : Entity.Model -> Maybe (List Form)
view entity =
  Entity.getCorporeal entity
  |> Maybe.map (\corp ->
    [
      filled corp.color <| circle ((fst corp.dim) / 2)
    ]
  )
{% endhighlight %}

Below, you can see the the renderer still defers to a router to render the entity,
but it also handles the transforms, by calling `#totalTransform`. It also takes
the camera, which it needs to pass into `#totalTransform`

[system/renderer.elm](https://github.com/iamwilhelm/bayes-cat/blob/v0.0.6/src/system/renderer.elm#L15)
{% highlight haskell %}
render : Maybe Entity.Model -> Entity.Model -> Maybe Collage.Form
render camera entity =
  let
    entityForms = Entity.getRenderable entity `andThen` renderEntity entity
    tf = totalTransform camera entity
  in
    Maybe.map (Collage.groupTransform tf) entityForms
{% endhighlight %}

In `#totalTransform`, we change the coordinate system for the entities.

[system/renderer.elm](https://github.com/iamwilhelm/bayes-cat/blob/v0.0.6/src/system/renderer.elm#L35)
{% highlight haskell %}
totalTransform : Maybe Entity.Model -> Entity.Model -> Transform
totalTransform camera entity =
  (e2w entity) `multiply` (w2c camera) -- NOTE the multiplcation should be reversed

e2w : Entity.Model -> Transform
e2w entity =
  Entity.getSpatial entity
  |> Maybe.map Component.Spatial.transform
  |> Maybe.withDefault Transform.identity

w2c : Maybe Entity.Model -> Transform
w2c camera =
  camera `andThen` Entity.getSpatial
  |> Maybe.map Component.Spatial.invertedControlTransform
  |> Maybe.withDefault Transform.identity
{% endhighlight %}

When we get the form back from the entity, they're in the local coordinate
system. We transform them into the world coordinate system with `#e2w` transform.
Then we multiply it by the `#w2c` transform to transform it into the camera
coordinate system. Note that `#w2c` uses `#invertedControlTransform`, because
the frame of reference when you move the camera is the camera, so everything else
should move in the opposite direction of where you're hitting the keys.

** NOTE I believe there's a bug here. the order the multiplication should be
reversed, as I noted above. I just forgot to reverse them when I was trying something,
but this looked like it was working **

We purposely don't `groupTransform` all the way up, as that would be too slow.
We multiply all the transforms first, before applying it to the forms.

### Controlling the Camera

Controlling the camera leverages the mechanisms we already have in place. We use
the keyboardRouter in main.elm to send messages to the camera entity, which
receives messages, and changes the force, which then is run through the physics
system, like any other entity.

### Try the Demo

You can try the [v0.0.6 demo here][v0.0.6], and the [source is here][v0.0.6-src].

i,j,k,l to move camera. u, o to zoom in and out.

Known bugs:
- there's no friction on the camera movements so it doesn't slow down.
- Because the bounded floors apply to all entities, they also apply to the camera

[v0.0.6]: /bayes-cat/versions/v0.0.6/index.html
[v0.0.6-src]: https://github.com/iamwilhelm/bayes-cat/tree/v0.0.6
