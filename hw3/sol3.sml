(*hw 3*)
(**)
datatype pattern = Wildcard | Variable of string | UnitP | ConstP of int
                 | TupleP of pattern list | ConstructorP of string * pattern

datatype valu = Const of int | Unit |Tuple of valu list 
              | Constructor of string * valu


(*problem1*)
(*val check_pat = fn : pattern -> bool*)

fun make_a_pattern_list(Wildcard ,ls) = ls
  | make_a_pattern_list(Variable(s) ,ls) = s::ls 
  | make_a_pattern_list(UnitP,ls) = ls
  | make_a_pattern_list(ConstP(n), ls)= ls
  | make_a_pattern_list(TupleP([]), ls) = ls
  | make_a_pattern_list(TupleP(x::xs), ls) = make_a_pattern_list( TupleP(xs), make_a_pattern_list(x,ls) )
  | make_a_pattern_list(ConstructorP(s,p),ls)= make_a_pattern_list(p, ls)

fun check(x::[]) = true
	(*check_pat(Wildcard) 같이 Variable string 이 하나도 없을 경우는 어떤 결과가 나올지?*)
  | check([]) = true 
  | check(x::xs) = if (List.exists (fn y => x = y)  xs  )
                   then false
                   else check(xs)

fun check_pat(p) = check(make_a_pattern_list(p,[]))


 (*problem2*)


fun make_svList(f,x::xs)=if(isSome(f(x)))
                         then valOf(f(x))@make_svList(f, xs)
                         else make_svList(f, xs)
  | make_svList(f,[])=[]



fun match( v , Wildcard ) = SOME []
  | match( v , Variable(s)) = SOME [(s,v)]
  | match(Unit, UnitP) = SOME []
  | match(Const(n), ConstP(m)) = if(n = m) 
                                 then SOME [] 
                                 else NONE
(*vl 과 pl 이 길이가 같을때만 계산.*)
  | match(Tuple(vl),TupleP(pl)) = if(length(vl)=length(pl))
                                  then  let val vlpl = ListPair.zip(vl,pl) 
                                        in
                                                if ((List.filter (fn(v,p)=>not(match(v,p)=NONE))  vlpl) = vlpl )
                                                then SOME (make_svList(match, vlpl))
                                                else NONE
                                        end
                                  else NONE
  | match(Constructor(s1 ,v), ConstructorP(s2, p)) = if(s1 = s2)
                                                    then match(v,p)
                                                    else NONE
  | match( v , p ) = NONE



(*problem3*)
type name = string
datatype RSP = ROCK | SCISSORS | PAPER

datatype 'a strategy = Cons of 'a *(unit -> 'a strategy)

datatype tournament = PLAYER of name * (RSP strategy ref) 
                    | MATCH of tournament * tournament

fun onlyOne(one : RSP) = Cons(one , fn() => onlyOne(one))
fun alterTwo(one : RSP, two : RSP) = Cons(one,fn()=>alterTwo(two,one))
fun alterThree(one : RSP, two : RSP, three : RSP) = Cons(one , fn() => alterThree(two ,three , one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK , PAPER)
val sr = alterTwo(SCISSORS , ROCK)
val srp = alterThree(SCISSORS , ROCK , PAPER)

(*strategyREf 순서를 바꾸고 가위바위보 중 하나를 리턴한다.*)
fun next(strategyRef) = let val Cons(rsp , ft) = !strategyRef in
                                strategyRef := ft();
                                rsp
                        end                       
                        
(*비기면 0 , 왼쪽이 이기면 1 , 오른쪽이 이기면 2*)
fun rules(ROCK , ROCK) = 0
  | rules(ROCK , SCISSORS) = 1
  | rules(ROCK , PAPER) = 2
  | rules(SCISSORS , ROCK) = 2
  | rules(SCISSORS , SCISSORS) = 0
  | rules(SCISSORS , PAPER) = 1
  | rules(PAPER , ROCK) = 1
  | rules(PAPER , SCISSORS) = 2
  | rules(PAPER , PAPER) = 0
 

fun whosWinner(MATCH(MATCH(t1 , t2), MATCH(m1 ,m2))) = whosWinner(MATCH(whosWinner(MATCH(t1,t2)) , whosWinner(MATCH(m1,m2))))
  | whosWinner(MATCH(MATCH(t1 ,t2) , PLAYER(n , str_ref))) = whosWinner(MATCH(whosWinner(MATCH(t1,t2)), PLAYER(n , str_ref)))
  | whosWinner(MATCH(PLAYER(n , str_ref) ,MATCH(t1,t2))) = whosWinner(MATCH(PLAYER(n ,str_ref), whosWinner(MATCH(t1,t2))))
  | whosWinner(MATCH(PLAYER(n1 , ref1) , PLAYER(n2, ref2)))  = let val r =rules(next(ref1) , next(ref2))
                                                                in
                                                                  if(r = 1)
                                                                  then PLAYER(n1, ref1)
                                                                  else if(r = 2)
                                                                  then PLAYER(n2, ref2)
								  else
                                                                    whosWinner( MATCH(PLAYER(n1,ref1), PLAYER(n2,ref2)))
                                                               end

                                                
  
  
  
  
  
  
  
 
    
                                                                
  

