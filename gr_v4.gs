first.(x,y)=x
second.(x,y)=y
getPaths.(x::xs,end,seq)|first.(x)==last.(seq)&&end==second.(x)  =seq++[second.(x)]
				|first.(x)==last.(seq) =getPaths.(xs,end,seq++[second.(x)])
				|otherwise =getPaths.(xs,end,seq)


---------------------------finding all paths from x to y-------------------------------------------------------
chk.(y,[])=[]
chk.(y,z::zs)|last.z==y =z::chk.(y,zs)
		|otherwise =chk.(y,zs)

notchk.(y,[])=[]
notchk.(y,z::zs)|last.z/=y =z::notchk.(y,zs)
                |otherwise =notchk.(y,zs)

mklist.(l,[])=[]
mklist.(l,x::xs)=(l++[x])::mklist.(l,xs)

findnbr.(x,g,l)=[second.y|y<-g,first.y==x,notElem.(second.y).l]++
	    [first.y|y<-g,second.y==x,notElem.(first.y).l]


getPath.(x,y,g,l)=chk.(y,mklist.(l,findnbr.(last.l,g,l)))++doForAll.(x,y,g,notchk.(y,mklist.(l,findnbr.(last.l,g,l))))

doForAll.(x,y,g,[])=[]
doForAll.(x,y,g,t::ts)=getPath.(x,y,g,t)++doForAll.(x,y,g,ts)


-------------------------------  finding all paths of whole graph ----------------------------------------------------
findVertex.([],lst)=lst
findVertex.(x::xs,lst)=findVertex.(xs,lst++[first.(x)|notElem.(first.x).lst]++[second.(x)|notElem.(second.x).lst])

mkpair.([])=[]
mkpair.(x::xs)=pair.(x,xs)++mkpair.(xs)

pair.(x,[])=[]
pair.(x,y::ys)=(x,y)::pair.(x,ys)

findPaths.(g,[])=[]
findPaths.(g,x::xs)=getPath.(first.x,second.x,g,[first.x])::findPaths.(g,xs)

findAllPaths.(g)=findPaths.(g,mkpair.(findVertex.(g,[]))) --finding all paths in graph 


---------------------------------  finding central vertex  (vertex which is common in all the paths)  --------------------------------------------------------------

findCentralVer.(g)=findCentralVertex.(findAllPaths.(g)) ------------main function
findCentralVertex.([])=[]
findCentralVertex.(x::xs)=intersection.(remove1stLastAll.x)::findCentralVertex.(xs)

remove1stLastAll.([])=[]
remove1stLastAll.(x::xs)=(remove1stLast.(x,0))::remove1stLastAll.(xs)

remove1stLast.([],t)=[]
remove1stLast.(x::xs,t)|t<2 =remove1stLast.(reverse.xs,t+1) |otherwise =x::xs

inter.([],_)=[]
inter.(x::xs,ys)|(filter.(==x).ys)/=[] =x::inter.(xs,ys)
                |otherwise =inter.(xs,ys)
intersection.(x::[])=x
intersection.(x::y::xs)=intersection.(inter.(x,y)::xs)

--------------------------------finding a Minimally Connected graph from any graph----------------------------------------


first:(Int,Int)->Int
first.(x,y)=x

second:(Int,Int)->Int
second.(x,y)=y

findAllAdjVert:(Int,[(Int,Int)])->[Int]
findAllAdjVert.(v,[])=[]
findAllAdjVert.(v,x::xs)|first.x==v =second.x::findAllAdjVert.(v,xs)
			|second.x==v =first.x::findAllAdjVert.(v,xs)
			|otherwise =findAllAdjVert.(v,xs)
chk.([],k)=False
chk.(x::xs,k)|x==k =True
	     |otherwise =chk.(xs,k)

--append:([Int],[Int])->[Int]
append.(xs,[])=xs
append.([],ys)=ys
append.(x::xs,ys)=x::append.(xs,ys)

notInVisitedList.([],_)=[]
notInVisitedList.(x::xs,ys)|(filter.(==x).ys)==[] =x::notInVisitedList.(xs,ys)
			   |otherwise =notInVisitedList.(xs,ys)
mkedges.(k,[])=[]
mkedges.(k,x::xs)=(k,x)::mkedges.(k,xs)

doForAll1.([],_,_,graph)=graph
doForAll1.(t::ts,x::xs,visitedlst,graph)|notInVisitedList.(findAllAdjVert.(t,x::xs),visitedlst) /= [] =f.(t,x::xs,visitedlst,graph)
					|otherwise =doForAll1.(ts,x::xs,visitedlst,graph)

f.(_,[],_,_)=[]
f.(k,x::xs,visitedlst,graph)=doForAll.(notInVisitedList.(findAllAdjVert.(k,x::xs),visitedlst),x::xs,append.(visitedlst,notInVisitedList.(findAllAdjVert.(k,x::xs),visitedlst)),append.(graph,mkedges.(k,notInVisitedList.(findAllAdjVert.(k,x::xs),visitedlst))))----------------Main function

