(*PL_HW#2_2015004693_양상헌*)

(*problem no.1*)
datatype expr = NUM of int
              | PLUS of int*int
              | MINUS of int*int

datatype formula = TRUE 
                 | FALSE
                 | NOT of formula
                 | ANDALSO of formula*formula
                 | ORELSE of formula*formula
                 | IMPLY of formula*formula
                 | LESS of expr*expr

(* expr 데이타 타입을 계산하는 함수expr -> int*)
fun fEval(NUM(i)) = i
  | fEval(PLUS(a,b)) = a+b
  | fEval(MINUS(c,d)) = c-d
	
(*LESS가 이해가 잘 안돼서, 앞쪽의 expr이 뒷쪽의 expr보다 더작으면 참, 아니면 거짓 으로 이해하고 진행함*)
(*formula -> bool*)
fun eval(TRUE) = true
  | eval(FALSE) = false
  | eval(NOT(i)) = not(eval(i))
  | eval(ANDALSO(a,b)) = eval(a) andalso eval(b)
  | eval(ORELSE(c,d)) = eval(c) orelse eval(d)
  | eval(IMPLY(e,f)) = if eval(e) then eval(f) else true
  | eval(LESS(ex1,ex2)) = if (fEval(ex1) < fEval(ex2)) then true else false


(*problem no2*)
type name = string

datatype metro = STATION of name
               | AREA of name*metro
               | CONNECT of metro*metro

fun checkWellDefined(a, []) = false
  | checkWellDefined(a, al) = if(a = hd(al))
                              then true
                              else checkWellDefined(a, tl(al))
				
(*밑에까지(station_name까지) 내려올때까지 있었던 area 이름을 스트링에 저장,
* 스태이션일경우 저장된 area의 이름들 중에 STATION의 이름이 있을경우 true
* 없을경우 false를 반환한다.*)
fun reachStationName(STATION(a),namelist) = checkWellDefined(a,namelist)
  | reachStationName(AREA(a,met),namelist) = reachStationName(met , a::namelist)
  | reachStationName(CONNECT(met1,met2),namelist) = reachStationName(met1,namelist) andalso reachStationName(met2,namelist)

fun checkMetro(m) = reachStationName(m,[])

(*problem no3*)

datatype 'a lazyList = nullList
                     | cons of 'a * (unit -> 'a lazyList) 

fun seq(first, last) = if(first = last) 
                       then cons(first, fn()=>nullList)
                       else cons(first, fn()=>seq(first+1, last))

fun infSeq(first) = cons(first , fn()=>infSeq(first+1))

fun firstN(nullList , 0) = []
  | firstN(cons(x, f) , 0) = []
  | firstN(nullList , n) = []
  | firstN(cons(x, f) , n) = x::firstN(f(),n-1)

fun Nth(nullList , n) = NONE
  | Nth(cons(x, f), 1) = SOME(x)
  | Nth(cons(x, f), n) = if(n<1) then NONE else Nth(f(),n-1)

fun filterMultiples(nullList , n) = nullList
  | filterMultiples(cons(x,ft) , 0) = cons(x,ft)
  | filterMultiples(cons(x,ft) , 1) = nullList
  | filterMultiples(cons(x,ft) , n) = if((x mod n) = 0)
                                     then filterMultiples(ft() , n)
                                     else cons(x , fn()=>filterMultiples(ft() , n))

(*sieve함수의 인자는 2부터시작하는 infinit seq lazylist*)
fun sieve(cons(x,ft)) = cons(x,fn()=>sieve(filterMultiples(cons(x,ft),x)))

fun prime() = sieve(infSeq(2))
