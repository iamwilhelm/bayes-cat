---
layout: post
title:  "Timestep affecting physics"
date:   2016-05-27 20:20:00 -0700
categories: physics
---

For any physics system, when you integrate, there are problems. Got tips from
[Fix your timestep][fix-your-timestep]

{% highlight haskell %}
type Msg =
    SizeChange Window.Size
  | Tick Float
  | Simulate Float
  | Player Entity.Cat.Msg
  | Egg Entity.Egg.Msg
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    _ = 3 --Debug.log "update msg: " msg
  in
    case msg of
      Tick dt ->
        stablizeFrameRate dt model
      Simulate dt ->
        (step dt model, Cmd.batch <| effects dt model)
{% endhighlight %}

{% highlight haskell %}
stablizeFrameRate : Float -> Model -> (Model, Cmd Msg)
stablizeFrameRate frameTime model =
  let
    dt = min frameTime model.targetFrameRate
  in
    if frameTime <= 0.0 then
      model ! []
    else
      model ! [
        Task.perform never identity (Task.succeed (Tick <| frameTime - dt))
      , Task.perform never identity (Task.succeed (Simulate dt))
      ]
{% endhighlight %}

[fix-your-timestep]: http://gafferongames.com/game-physics/fix-your-timestep/
