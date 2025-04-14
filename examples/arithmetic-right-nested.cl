(*
  from random import choice, randint
  ops = "+*"
  
  def generate(n):
      if n == 0: return str(randint(0,10))
      return "((x <- " + str(randint(0,10)) + ") " + choice(ops) + " " + generate(n-1) + ");"
  
  return "let x: Int <- 1 in out_int(" + generate(1) + ")"
*)


class Main inherits IO {
    main(): Object {{
        let x: Int <- 1 in out_int((( x <- 4) * (( x <- 2) * (( x <- 7) + (( x <- 0) + (( x <- 8) + (( x <- 5) + (( x <- 5) * (( x <- 7) * (( x <- 8) * (( x <- 5) + 3)))))))))));
        let x: Int <- 1 in out_int(((x <- 1) + ((x <- 1) + ((x <- 0) + ((x <- 10) + ((x <- 1) + ((x <- 7) * ((x <- 10) * 10))))))));
        let x: Int <- 1 in out_int(((x <- 6) + ((x <- 9) + ((x <- 9) + ((x <- 7) + ((x <- 2) + 1))))));
        let x: Int <- 1 in out_int(((x <- 10) * ((x <- 1) * 4)));
    }};
};
