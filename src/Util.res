open Belt
module type TwoCombSerchable = {
  type t
  let guard: (t, t) => bool
}

module MakeTwoCombsFinder = (Info: TwoCombSerchable) => {
  type arrT = array<Info.t>
  let find = (arr: arrT) => {
    arr->Array.reduce(list{}, (l, a) => {
      arr->Array.reduce(l, (l, b) => {Info.guard(a, b) ? l->List.add((a, b)) : l})
    })->List.toArray
  }
}

module IntFindable = {
    type t = int
    let guard = (x, y) => x + y == 2020 && x != y
}

module IntTwoCombFinder = MakeTwoCombsFinder(IntFindable)

module FloatFindable = {
    type t = float
    let guard = (x, y) => x +. y == 2020. && x != y
}

module FloatTwoCombFinder = MakeTwoCombsFinder(FloatFindable)



IntTwoCombFinder.find([1020, 10, 20, 40, 50, 1000])->Js.log

FloatTwoCombFinder.find([1020., 10., 20., 40., 50., 1000.])->Js.log