let budget_list = list{1721, 979, 366, 299, 675, 1456}

let findSolution = (l: list<int>) => {
  l
  ->Belt.List.map(e => (e, l))
  ->Belt.List.map(pair => {
    let (e0, l) = pair
    let l0 = Belt.List.keep(l, e1 => e0 + e1 == 2020)
    (e0, l0)
  })
  ->Belt.List.keep(x => {
    let (_, l) = x
    l != list{}
  })
  ->Belt.List.map(p => {
    let (e, l) = p
    (e, Belt.Option.getExn(Belt.List.head(l)))
  })
  ->Belt.List.head
  ->Belt.Option.getExn
  ->(
    p => {
      let (e0, e1) = p
      e0 * e1
    }
  )
}
let result = Request.make(~url="res/day1.txt", ~responseType=Text, ())
->Future.get(Js.log)


/**
let readTextFile = %raw(`
  function (file){
  var rawFile = new XMLHttpRequest();
    rawFile.open("GET", file, false);
    rawFile.onreadystatechange = function ()
    {
        if(rawFile.readyState === 4)
        {
            if(rawFile.status === 200 || rawFile.status == 0)
            {
                var allText = rawFile.responseText;
            }
        }
    }
    rawFile.send(null);}
`)
let result = readTextFile("res/day1.txt")
Js.log(result)
**/

let ans = findSolution(budget_list)
Js.log(ans)
