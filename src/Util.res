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

