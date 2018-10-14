open Reprocessing;

module MapString = Map.Make(String);

type fruitType =
  | Apple
  | Banana
  | Pineapple;

let fruitToFilename = (fruit: fruitType) =>
  switch (fruit) {
  | Apple => "apple"
  | Banana => "banana"
  | Pineapple => "pineapple"
  };

let fruits = [Apple, Banana, Pineapple];
let randomFruit = () =>
  List.nth(fruits, Utils.random(~min=0, ~max=List.length(fruits)));

type fruitSize =
  | Small
  | Medium
  | Big;

let randomSize = () =>
  switch (Utils.random(~min=0, ~max=3)) {
  | 0 => Small
  | 1 => Medium
  | 2 => Big
  | _ => Small
  };

let fruitSizeToInt = size =>
  switch (size) {
  | Small => 50
  | Medium => 100
  | Big => 150
  };

type stamp = {
  position: (int, int),
  fruitType,
  size: fruitSize,
};

type state = {
  bg: imageT,
  assetMap: MapString.t(imageT),
  stamps: list(stamp),
  currentFruit: fruitType,
  size: fruitSize,
};

let loadFruit = (fruit, env) =>
  Draw.loadImage(
    ~filename="./assets/" ++ fruitToFilename(fruit) ++ ".png",
    env,
  );

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  let bg = Draw.loadImage(~filename="./assets/background.png", env);
  let assetMap =
    fruits
    |> List.fold_left(
         (map, fruit) =>
           map
           |> MapString.add(fruitToFilename(fruit), loadFruit(fruit, env)),
         MapString.empty,
       );
  {bg, assetMap, stamps: [], currentFruit: Apple, size: Small};
};

let getFruitImage = (assetMap, fruit) =>
  assetMap |> MapString.find(fruitToFilename(fruit));

let getMousePositionCentered = (state, env) => {
  let (mx, my) = env |. Env.mouse;
  let imgPosition = (
    mx - fruitSizeToInt(state.size) / 2,
    my - fruitSizeToInt(state.size) / 2,
  );
  imgPosition;
};

let draw = (state, env) => {
  Draw.image(state.bg, ~pos=(0, 0), env);
  let getFruitImage = getFruitImage(state.assetMap);
  let _ =
    state.stamps
    |> List.map(stamp =>
         Draw.image(
           getFruitImage(stamp.fruitType),
           ~pos=stamp.position,
           ~width=fruitSizeToInt(stamp.size),
           ~height=fruitSizeToInt(stamp.size),
           env,
         )
       );
  let appleImg = getFruitImage(state.currentFruit);
  let imgPosition = getMousePositionCentered(state, env);
  Draw.tint({r: 0.8, g: 0.8, b: 0.8, a: 0.5}, env);
  Draw.image(
    appleImg,
    ~pos=imgPosition,
    ~width=fruitSizeToInt(state.size),
    ~height=fruitSizeToInt(state.size),
    env,
  );
  Draw.noTint(env);
  Draw.scale(~x=0.5, ~y=0.5, env);
  Draw.text(~body="Press 1, 2, 3 to change stamp size", ~pos=(10, 10), env);
  Draw.text(~body="Press A, B, P to change fruit type", ~pos=(10, 50), env);
  Draw.text(~body="Press R to clear drawing board", ~pos=(10, 90), env);
  Draw.scale(~x=1.5, ~y=1.5, env);
  Draw.text(~body="Click to stamp!", ~pos=(10, 90), env);
  state;
};

let mouseDown = (state, env) => {
  ...state,
  stamps: [
    {
      position: getMousePositionCentered(state, env),
      fruitType: state.currentFruit,
      size: state.size,
    },
    ...state.stamps,
  ],
  currentFruit: randomFruit(),
  size: randomSize(),
};

let keyPressed = (state, env) => {
  let newState = {
    ...state,
    currentFruit:
      switch (Env.keyCode(env)) {
      | A => Apple
      | B => Banana
      | P => Pineapple
      | _ => state.currentFruit
      },
    size:
      switch (Env.keyCode(env)) {
      | Num_1 => Small
      | Num_2 => Medium
      | Num_3 => Big
      | _ => state.size
      },
  };
  if (Env.keyCode(env) == R) {
    {...state, stamps: [], size: Small};
  } else {
    newState;
  };
};

run(~setup, ~draw, ~mouseDown, ~keyPressed, ());