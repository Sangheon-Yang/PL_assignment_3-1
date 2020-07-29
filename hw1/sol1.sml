(*problem no.1 merge list*)

(*리스트가 둘다 nil 일때, 하나만 nil 일때, 둘다 nil이 아닐때*)
fun merge ( l1 : int list , l2 : int list) : int list =
	if (null(l1) andalso null(l2))(*두리스트가 다 널일때*)
	then []
	else if (null(l1) andalso not(null(l2)))(*첫번째리스트만 널이고 두번째가 널이아닐떄*)
	then hd(l2) :: merge( l1 , tl(l2) )
	else if(not(null(l1)) andalso (null(l2)))(*첫번재리스트는 널이 아니고 두번째가 널일때*)
	then hd(l1) :: merge( tl(l1) , l2 )
	else if ( hd(l1)<hd(l2) )(*두리스트다 널이 아니고 l1의 헤드가 더 작을때*)
	then hd(l1) :: merge( tl(l1) , l2 )
	else hd(l2) :: merge ( l1 , tl(l2))



(*problem no.2 reverse lists*)
	
(*첫번째 매개변수 리스트의 헤드를 하나씩 받아서 두번재 매개변수 리스트에 붙여서 역순의 리스트를 만드는 함수*)
(* ex) f([1,2,3] , [5,6,7])=>[3,2,1,5,6,7]*)
fun getHDtoLIST(x : int list , y : int list) : int list =
	if null(x)(*x가 널일때*)
	then y
	else getHDtoLIST( tl(x) , hd(x)::y )
	
(*getHDtoLIST 함수를 이용, 역순으로 바꾸고싶은 리스트를 첫번재 매개변수로하고 두번째 매개변수는 널 리스트로 함 *)
fun reverse (x : int list) : int list =
	getHDtoLIST(x,[])



(*problem no.3 sigma function*)
(* an example for inner used function f(x)=2x (int->int)*)
fun ffff ( x : int ) : int = 
	2*x
(* a sigma function*)
fun sigma ( a : int , b : int , ff : (int -> int) ) : int =
	if ( a > b )
	then 0
	else ff(a) + sigma( (a+1) , b , ff)



(*problem no.4 digit function*)

(*mod 연산자와 div 연산자를 사용하여 단위 숫자를 recursive 하게 입력받아 역순 리스트를 생성 *)
(*ex: 345 => [5,4,3]*)
fun numberTOdigitsLIST( x : int ) : int list =
	if ( x < 10 )
	then [x]
	else (x mod 10) :: numberTOdigitsLIST(x div 10)
	
(* numberTOdigitsLIST 함수와 reverse 함수를 사용하여 올바른 digit리스트를 생성 *)
(* ex: 345=>[5,4,3]=>[3,4,5] *)
fun digits(x: int) : int list =
	reverse(numberTOdigitsLIST(x))
	
	
	
(*problem no.5 additivePersistence & digitalRoot function*)

(*입력받은 리스트의 숫자를 다 더하는 함수*)
fun sumOFdigits (x : int list) : int =
	if (x = nil)
	then 0
	else hd(x) + sumOFdigits( tl(x) )
	 
(*횟수가 한번 늘어날때마다 1씩 증가 recursively, sumOFdigits&digits function 사용 *)
fun additivePersistence ( x : int ) : int =
	if (x<10)
	then 0
	else 1 + additivePersistence (sumOFdigits( digits(x) ))
	
(*입렵받은숫자의 digit을 10보다 작아질때까지 계속해서 더해주는 재귀함수*)
fun digitalRoot ( x : int ) : int =
	if(x<10)
	then x
	else digitalRoot(sumOFdigits( digits(x) ))






