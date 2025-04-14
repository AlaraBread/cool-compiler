(* Weimer 2010 -- matrix-matrix multiply where the matrices are stored
 * as lists of lists of integers (because Cool doesn't have array types).
 *
 * This incarnation uses 5x5 matrices. 
 *) 

(* ListOfInt corresponds to one row of a matrix *) 
class ListOfInt {
   isNil() : Bool { true };
   head()  : Int { { abort(); 0; } };
   get(index : Int)  : Int { { abort(); 0; } };
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
   init(i : Int, rest : ListOfInt) : ListOfInt {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };
};

(* ListOfList corresponds to one entire matrix *) 
class ListOfList {
   isNil() : Bool { true };
   head()  : ListOfInt { { abort(); (new ListOfInt); } };
   get(index : Int)  : ListOfInt { { abort(); (new ListOfInt); } };
   tail()  : ListOfList { { abort(); self; } };
   cons(i : ListOfInt) : ListOfList { (new ConsOfList).init(i, self) };

};


class ConsOfList inherits ListOfList {
   car : ListOfInt;	-- The element in this list cell
   cdr : ListOfList;	-- The rest of the list
   isNil() : Bool { false };
   head()  : ListOfInt { car };
   tail()  : ListOfList { cdr };
   get(index : Int)  : ListOfInt { 
      if index <= 0 then head() 
      else tail().get(index - 1) fi
   } ; 
   init(i : ListOfInt, rest : ListOfList) : ListOfList {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };
};


class Main inherits IO {


   print_list(l : ListOfInt) : Object {
      if l.isNil() then out_string("\n")
                   else {
			   out_int(l.head());
			   out_string(" ");
			   print_list(l.tail());
		        }
      fi
   };

   print_list_of_list(l : ListOfList) : Object {
      if l.isNil() then out_string("\n")
                   else {
			   print_list(l.head());
			   print_list_of_list(l.tail());
		        }
      fi

   } ;


   main() : Object {
      let 
        a: ListOfList <- new ListOfList, 
        b: ListOfList <- new ListOfList,
        i : Int,
        j : Int,
        k : Int,
        dot_product : Int, 
        result_row : ListOfInt,
        result : ListOfList <- new ListOfList 
      in 
      {

                (* read in Matrix A (in psycho order) *) 
         i <- 4;
         while (0 <= i) loop {
           j <- 4; 
           result_row <- new ListOfInt ;
           while (0 <= j) loop { 
             result_row <- result_row.cons(in_int()); 
             j <- j - 1 ; 
           } pool ;
           i <- i - 1 ; 
           a <- a.cons(result_row); 
         } pool ; 

                (* read in Matrix B (in psycho order) *) 
         i <- 4;
         while (0 <= i) loop {
           j <- 4; 
           result_row <- new ListOfInt ;
           while (0 <= j) loop { 
             result_row <- result_row.cons(in_int()); 
             j <- j - 1 ; 
           } pool ;
           i <- i - 1 ; 
           b <- b.cons(result_row); 
         } pool ; 
               
         print_list_of_list(a); 

         out_string("matrix multipled by\n\n"); 

         print_list_of_list(b); 

         out_string("yields\n\n"); 

                (* Classical Matrix-Matrix Multiply  *) 
         i <- 4; 
         while (0 <= i) loop {
           j <- 4; 
           result_row <- new ListOfInt ;
           while (0 <= j) loop { 
             k <- 0;
             dot_product <- 0; 
             while (k <= 4) loop { 
               dot_product <- dot_product + 
                  a.get(i).get(k) * b.get(k).get(j) ; 
               k <- k + 1 ; 
             } pool ; 
             result_row <- result_row.cons(dot_product); 
             j <- j - 1 ; 
           } pool ;
           i <- i - 1 ; 
           result <- result.cons(result_row); 
         } pool  ;
         print_list_of_list(result); 
      }

   };

};



