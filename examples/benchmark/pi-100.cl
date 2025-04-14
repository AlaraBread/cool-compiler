(* Weimer 2010 -- compute the decimal expansion of PI 
 * Warning -- the unoptimized assembly version of this can be quite
 * slow, even on modern hardware. 
 * 
 * taken from http://www.codecodex.com/wiki/Digits_of_pi_calculation#C
 *) 

class ListOfInt {
   isNil() : Bool { true };
   head()  : Int { { abort(); 0; } };
   get(index : Int)  : Int { { abort(); 0; } };
   set(index : Int, newval : Int)  : Object { { abort(); self; } };
   tail()  : ListOfInt { { abort(); self; } };
   cons(i : Int) : ListOfInt { (new ConsOfInt).init(i, self) };

};


class ConsOfInt inherits ListOfInt {
   car : Int;	-- The element in this list cell
   cdr : ListOfInt;	-- The rest of the list
   isNil() : Bool { false };
   head()  : Int { car };
   tail()  : ListOfInt { cdr };
   get(index : Int)  : Int { 
      if index <= 0 then head() 
      else tail().get(index - 1) fi
   } ; 
   set(index : Int, newval: Int)  : Object { 
      if index <= 0 then car <- newval
      else tail().set(index - 1, newval) fi
   } ; 
   init(i : Int, rest : ListOfInt) : ListOfInt {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };
};


class Main inherits IO {

   modulus(a : Int, b : Int) : Int { -- compute A % B
     let divided : Int <- a / b in
     let multiplied : Int <- divided * b in
     let remainder : Int <- a - multiplied in
     remainder
   } ;

   pi_precision(precision : Int) : Object {
      let 
        arr: ListOfInt <- new ListOfInt, 
        array_init : Int <- 2000,
        scale : Int <- 10000,
        carry : Int, 
        i : Int
      in {
      while (i <= precision) loop {
        arr <- arr.cons(array_init); 
        i <- i + 1 ; 
      } pool ; 
      i <- precision ;
      while (0 < i) loop let sum : Int, j : Int <- i in {
        while (0 < j) loop {
                sum <- (sum * j) + (scale * arr.get(j)) ;
                arr.set(j, modulus(sum,(j*2)-1));
                sum <- sum / ((j * 2)-1);
                j <- j - 1; 
        } pool ;
        let output : Int <- carry + sum/scale in {
                if (output < 10) then out_int(0) else self fi ; 
                if (output < 100) then out_int(0) else self fi ; 
                if (output < 1000) then out_int(0) else self fi ; 
                out_int(output);
        } ; 
        carry <- modulus(sum,scale); 
        i <- i - 14; 
      } pool ;
        }
   }; 

   main() : Object {
      {
         pi_precision(100); 
      }

   };

};

(*
        100 precision, us vs. them: 
  31415926535897932384626433832795
  31415926535897932384626433832787

        200 precision, us vs. them: 
  314159265358979323846264338327950288419716939937510582097494
  314159265358979323846264338327950288419716939937510582097493
  *)
