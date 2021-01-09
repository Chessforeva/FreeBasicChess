'' Chess logic with samples for FreeBASIC v.0.4.6
'' Board in variables, moves, FEN & PGN functions
'' A ready code for free usage in any type of project
'' no clock, no chess engine
'' http://chessforeva.blogspot.com


'' Global variables with initial settings

declare sub a_SAMPLES ()    '' Hello world of this, look at the end of code

dim shared as string c0_position, c0_become, c0_become_from_engine
dim shared as string c0_moveslist, c0_start_FEN, c0_fischer_cst
dim shared as string c0_PG_1, c0_PGN_header, PGN_text, c0_NAGs, c0_foundmove
dim shared as byte c0_side, c0_sidemoves
dim shared as byte c0_wKingmoved, c0_bKingmoved, c0_wLRockmoved, c0_wRRockmoved
dim shared as byte c0_bLRockmoved, c0_bRRockmoved, c0_w00, c0_b00, c0_fischer
dim shared as byte c0_errflag
dim shared as integer c0_lastmovepawn

const c0_false = 0
const c0_true = 1

function c0_not( b as byte) as byte
    return 1-b
end function

function c0_CurColor as string
    if(c0_sidemoves>0) then return "w"
    return "b"
end function


c0_position = ""
c0_side =1
c0_sidemoves = 1

c0_wKingmoved = c0_false
c0_bKingmoved = c0_false
c0_wLRockmoved = c0_false
c0_wRRockmoved = c0_false
c0_bLRockmoved = c0_false
c0_bRRockmoved = c0_false
c0_w00 = c0_false
c0_b00 = c0_false

c0_lastmovepawn = 0

c0_become = ""
c0_become_from_engine = ""

c0_moveslist = ""

c0_foundmove = ""

c0_start_FEN = ""
c0_fischer = c0_false
c0_fischer_cst = ""

c0_PG_1 = ""

c0_PGN_header = ""

c0_errflag = c0_false

PGN_text = ""
c0_NAGs = ""


'' Substitutes for main string functions

function Substr (s as string, at as integer, l as integer) as string
return mid(s,at+1,l)
end function

function SubstrAll (s as string, at as integer) as string
return mid(s,at+1)
end function

function sReplace (s as string, sf as string, st as string) as string
    dim a as integer
    dim l as integer = len(sf)
    dim s2 as string = s
    do
        a = Instr(s2, sf)
        if a>0 then
            s2 = Left(s2,a-1) + st + mid(s2,a+len(sf))
        end if
    loop until a<1
    return s2
end function



''
''  Handler.......... : onInit
''  To set initial settings to variables
''
sub onInit
''

c0_position = ""
c0_side =1
c0_sidemoves = 1

c0_wKingmoved = c0_false
c0_bKingmoved = c0_false
c0_wLRockmoved = c0_false
c0_wRRockmoved = c0_false
c0_bLRockmoved = c0_false
c0_bRRockmoved = c0_false
c0_w00 = c0_false
c0_b00 = c0_false

c0_lastmovepawn = 0

c0_become = ""
c0_become_from_engine = ""

c0_moveslist = ""

c0_foundmove = ""

c0_start_FEN = ""
c0_fischer = c0_false
c0_fischer_cst = ""


c0_PG_1 = ""

c0_PGN_header= ""

c0_errflag = c0_false

PGN_text = ""
c0_NAGs = ""

''
end sub
''

''
''  Function......... : IndexOf
''  Description...... : Searches substrings...
''
function IndexOf (s as string, p as string) as integer
''
dim as integer ret2=-1
dim as integer ret = Instr( s, p )

if (ret>0) then
	ret2=ret-1
end if

return ret2
''
end function
''

'' Slow search function (for sure - without patterns)
function IndexOfslow (s as string, p as string) as integer
''
dim as integer ret2=-1
dim as integer ret = Instr( s, p )

if (ret>0) then
	ret2=ret-1
end if

'' otherwise slow loop
 'dim as integer c0_i=0
 'while(c0_i<len(s))
 ' if( Substr (s, c0_i, len(p)) = p ) then
 '	ret2=c0_i
 '	break
 'end if
 'c0_i+=1
 'wend

return ret2
''
end function
''


'' Returns ASCII code of char at position
function byteAt ( s as string, n as integer ) as integer
''
return asc( mid(s,n+1,1) )
''
end function
''

''
''  Function......... : window_confirm
''  Description...... : window for piece promotion
''                      Not developed. Should be done in visual environment.
''                      Just returns Yes to set up new queen
''                       (99% cases or just don't promote :D)
''
''
function window_confirm ( ask_text as string ) as byte
''
return c0_true
''
end function
''


'' CHESS internal functions
''
''  Function......... : c0_conv52
''
function c0_conv52 ( c0_vertikali as integer, c0_horizontali as integer) as string
''
return (c0_vertikali) & str(c0_horizontali)
''
end function
''

''
''  Function......... : c0_convE2
''
function c0_convE2 ( c0_vertikali as integer, c0_horizontali as integer ) as string
''
return chr(96+c0_horizontali) & str ( c0_vertikali )
''
end function
''

''
''  Function......... : c0_convE777
''
function c0_convE777 ( c0_verthoriz as string ) as string
''
return chr(96+val( Substr(c0_verthoriz,1,1))) & Substr(c0_verthoriz,0,1)
''
end function
''

''
''  Function......... : c0_convH888
''
function c0_convH888 ( c0_at8 as string ) as string
''
dim as integer c0_8horiz=byteAt(c0_at8,0) - 96
dim as integer c0_8vert=val( Substr(c0_at8,1,1))
return str(c0_8vert) & str(c0_8horiz)
''
end function
''

''
''  Function......... : c0_add_piece
''  Description...... : add a piece at position...
''
''
sub c0_add_piece ( c0_pstring as string )
''
dim as string c0_1_at=Substr(c0_pstring,2,2)
dim as string c0_1_figure=Substr(c0_pstring,1,1)
dim as string c0_1_color=Substr(c0_pstring,0,1)

dim as string position = c0_position

if(IndexOf( position, c0_1_at)<0) then
    position = position & c0_pstring & ";"
    '' Here the 3D object could be created for the new piece...
    c0_position = position
end if

''
end sub
''

''
''  Function......... : c0_clear_at
''  Description...... : remove a piece from position...
''
''
sub c0_clear_at ( c0_1_at as string )
''
dim as integer c0_a=IndexOf(c0_position,c0_1_at)

if(c0_a>=0) then

 c0_position = Substr( c0_position,0,c0_a-2) & SubstrAll(c0_position,c0_a+3)

end if
''
end sub
''

''
''  Function......... : c0_is_empty
''  Description...... : (more internal)
''
function c0_is_empty ( c0_Zvert as integer, c0_Zhoriz as integer ) as byte
''
 dim as byte c0_good = c0_true
 if(c0_Zvert<1 or c0_Zvert>8 or c0_Zhoriz<1 or c0_Zhoriz>8) then
    c0_good=c0_false
 else

   dim as integer c0_pz2=IndexOf( c0_position, c0_convE2(c0_Zvert,c0_Zhoriz) )
   if(c0_pz2>=0) then
	c0_good=c0_false
   end if
 end if
 return c0_good
''
end function
''

''
''  Function......... : c0_is_emptyline
''  Description...... :
''
''
function c0_is_emptyline (c0_Zvert as integer, c0_Zhoriz as integer, _
    c0_Zvert2 as integer, c0_Zhoriz2 as integer) as byte
''

 dim as byte c0_good = c0_true
 dim as integer c0_DZvert=c0_Zvert2-c0_Zvert
 if(c0_DZvert<0) then
    c0_DZvert=-1
 else
  if(c0_DZvert>0) then
    c0_DZvert=1
  end if
 end if
 dim as integer c0_DZhoriz=c0_Zhoriz2-c0_Zhoriz
 if(c0_DZhoriz<0) then
    c0_DZhoriz=-1
 else
  if(c0_DZhoriz>0) then
    c0_DZhoriz=1
  end if
 end if
 dim as integer c0_PZvert=c0_Zvert+c0_DZvert
 dim as integer c0_PZhoriz=c0_Zhoriz+c0_DZhoriz
 while(c0_PZvert<>c0_Zvert2 or c0_PZhoriz<>c0_Zhoriz2)

	if( c0_is_empty( c0_PZvert, c0_PZhoriz ) = c0_false ) then
		 c0_good=c0_false
		 exit while
	end if
	c0_PZvert=c0_PZvert+c0_DZvert
	c0_PZhoriz=c0_PZhoriz+c0_DZhoriz
 wend
 return c0_good

''
end function
''


''
''  Function......... : c0_is_enemy
''  Description...... :
''
function c0_is_enemy (c0_Zvert as integer, c0_Zhoriz as integer, _
    c0_mycolor as string) as byte
''
 dim as byte c0_is_there =c0_false
 if(c0_Zvert>=1 and c0_Zvert<=8 and c0_Zhoriz>=1 and c0_Zhoriz<=8) then

   dim as integer c0_pz2=IndexOf( c0_position, c0_convE2(c0_Zvert,c0_Zhoriz) )

    if(c0_pz2>=0 and Substr( c0_position,c0_pz2-2,1)<>c0_mycolor) then
        c0_is_there=c0_true
    end if
 end if
 return c0_is_there
''
end function
''


''
''  Function......... : c0_just_move_piece
''  Description...... : move visualy a piece...
''
sub c0_just_move_piece ( c0_2_from as string, c0_2_to as string )
''

c0_clear_at( c0_2_to )
dim as integer c0_a=IndexOf( c0_position, c0_2_from )
if(c0_a>=0) then
	dim as string c0_2_figure = Substr( c0_position,c0_a-1,1)
	dim as string c0_2_color = Substr( c0_position,c0_a-2,1)
	c0_position = sReplace( c0_position, c0_2_from, c0_2_to )
''	c0_moves2do+=c0_2_from+c0_2_to
end if

''
end sub
''


''
''  Function......... : c0_moveto
''  Description...... : moves a piece (chess move)
''
sub c0_moveto (c0_from_at as string, c0_to_at as string, c0_draw as byte )
''

dim as integer c0_vert = val( Substr(c0_from_at,0,1))
dim as integer c0_horiz= val( Substr(c0_from_at,1,1))
dim as integer c0_vert2 = val( Substr(c0_to_at,0,1))
dim as integer c0_horiz2= val( Substr(c0_to_at,1,1))

dim as integer c0_p=IndexOf(c0_position, c0_convE2(c0_vert,c0_horiz) )

dim as string c0_color=Substr(c0_position,c0_p-2,1)
dim as string c0_figure=Substr(c0_position,c0_p-1,1)

dim as string save_c0_position=c0_position

c0_lastmovepawn = 0
if(c0_draw) then
	c0_become =""
end if


 if(c0_draw) then

	save_c0_position=c0_position
	c0_just_move_piece( c0_convE2(c0_vert, c0_horiz), c0_convE2(c0_vert2, c0_horiz2) )
	c0_position = save_c0_position
 end if

 dim as integer c0_p2=IndexOf(c0_position, c0_convE2(c0_vert2,c0_horiz2) )
 if(c0_p2>=0) then

   c0_position = Substr(c0_position,0,c0_p2-2) & SubstrAll(c0_position,c0_p2+3)

   if(c0_not(c0_wLRockmoved ) and c0_convE2(c0_vert2,c0_horiz2)="a1") then
	c0_wLRockmoved = c0_true
   end if
   if(c0_not(c0_wRRockmoved ) and c0_convE2(c0_vert2,c0_horiz2)="h1") then
	c0_wRRockmoved = c0_true
   end if
   if(c0_not(c0_bLRockmoved ) and c0_convE2(c0_vert2,c0_horiz2)="a8") then
	c0_bLRockmoved = c0_true
   end if
   if(c0_not(c0_bRRockmoved ) and c0_convE2(c0_vert2,c0_horiz2)="h8") then
	c0_bRRockmoved = c0_true
   end if

 else

    if(c0_figure="R") then

     if(c0_color="w") then

        if(c0_convE2(c0_vert,c0_horiz)="a1") then
		c0_wLRockmoved = c0_true
	end if
        if(c0_convE2(c0_vert,c0_horiz)="h1") then
		c0_wRRockmoved = c0_true
	end if

     else

        if(c0_convE2(c0_vert,c0_horiz)="a8") then
		c0_bLRockmoved = c0_true
	end if
        if(c0_convE2(c0_vert,c0_horiz)="h8") then
		c0_bRRockmoved = c0_true
	end if
     end if
    end if


   if(c0_figure="K") then

    if(c0_not( c0_wKingmoved ) and c0_color="w") then

	if(c0_convE2(c0_vert,c0_horiz)="e1" and c0_convE2(c0_vert2,c0_horiz2)="g1") then	'' 0-0

		if(c0_draw) then

            save_c0_position=c0_position
            c0_just_move_piece("h1","f1")
            c0_position = save_c0_position
		end if

		c0_position = sReplace( c0_position,"h1", "f1" )		'' Rf1
		c0_w00 = c0_true
		c0_become = "0"
	end if

	if(c0_convE2(c0_vert,c0_horiz)="e1" and c0_convE2(c0_vert2,c0_horiz2)="c1")	then '' 0-0-0

		if(c0_draw) then

            save_c0_position=c0_position
            c0_just_move_piece("a1","d1")
            c0_position = save_c0_position
		end if
		c0_position = sReplace( c0_position,"a1", "d1" )		'' Rd1
		c0_w00 = c0_true
		c0_become = "0"

    end if

	c0_wKingmoved = c0_true

   end if

    if( c0_not(c0_bKingmoved) and c0_color="b") then

	if(c0_convE2(c0_vert,c0_horiz)="e8" and c0_convE2(c0_vert2,c0_horiz2)="g8")	then '' 0-0

		if(c0_draw) then

            save_c0_position=c0_position
            c0_just_move_piece("h8","f8")
            c0_position = save_c0_position
		end if
		c0_position = sReplace( c0_position,"h8", "f8" )		'' Rf8
		c0_b00 = c0_true
		c0_become ="0"
	end if
	if(c0_convE2(c0_vert,c0_horiz)="e8" and c0_convE2(c0_vert2,c0_horiz2)="c8")	then '' 0-0-0

		if(c0_draw) then

            save_c0_position=c0_position
            c0_just_move_piece("a8","d8")
            c0_position = save_c0_position
		end if
		c0_position = sReplace( c0_position,"a8", "d8" )		'' Rd8
		c0_b00 = c0_true
		c0_become ="0"
	end if
	c0_bKingmoved = c0_true

    end if

 end if
end if

 if(c0_figure="p")	then	'' pawn

	 if(c0_vert2=8 or c0_vert2=1) then

		if(len(c0_become_from_engine)>0) then

		  c0_figure= c0_become_from_engine

		else

		 if(c0_draw) then

			 if(window_confirm("Promote a QUEEN?")) then

				c0_figure = "Q"

			 else
			  if(window_confirm("Then a ROOK?")) then

				c0_figure = "R"

			 else
			  if(window_confirm("Maybe a BISHOP?")) then

				c0_figure = "B"

			 else
			  if(window_confirm("Really a KNIGHT????")) then

				c0_figure = "N"

			 else

				''print("I know, You need a new QUEEN.")
				c0_figure = "Q"
			 end if
             end if
             end if
             end if

          else
            c0_figure="Q"
          end if
		end if


		if(c0_draw) then

			c0_become = c0_figure
																		'' just put in queue... (no,will be detected above in 3D)...
			''save_c0_position=c0_position
			''c0_moves2do+=c0_convE2(c0_vert2,c0_horiz2) + "=" + c0_become
			''c0_position=save_c0_position
		end if
		c0_position = sReplace( c0_position,"p" & c0_convE2(c0_vert,c0_horiz), _
            c0_figure & c0_convE2(c0_vert,c0_horiz) )

     end if

	 if(c0_p2<0 and c0_horiz<>c0_horiz2) then

		if(c0_draw) then

			save_c0_position=c0_position
			c0_clear_at( c0_convE2(c0_vert,c0_horiz2) )
			c0_position = save_c0_position
		end if
		dim as integer c0_p3=IndexOf( c0_position, c0_convE2(c0_vert,c0_horiz2) )
		c0_position = Substr( c0_position,0,c0_p3-2) & SubstrAll( c0_position,c0_p3+3)
	 end if
	 if((c0_vert=2 and c0_vert2=4) or (c0_vert=7 and c0_vert2=5)) then
		c0_lastmovepawn = c0_horiz
	 end if

	end if


 c0_position = sReplace( c0_position, c0_convE2(c0_vert,c0_horiz), c0_convE2(c0_vert2,c0_horiz2) )

 if(c0_draw) then

  c0_moveslist = c0_moveslist & c0_convE2(c0_vert,c0_horiz) & c0_convE2(c0_vert2,c0_horiz2)

  if(len(c0_become )>0) then

    c0_moveslist = c0_moveslist & "[" & c0_become & "]"
  end if
  ''c0_and_promote_or_castle()
 end if

''
end sub
''

''
''  Function......... : c0_fischer_cstl_move
''  Description...... : Does fischer movings for castling...
''
sub c0_fischer_cstl_move (c0_move7 as string, c0_draw as byte)
''

dim as string c0_king=""
dim as string c0_rook=""
dim as string c0_king2=""
dim as string c0_rook2=""

if(Substr(c0_move7,0,4)="00**") then

	if(c0_sidemoves>0) then
		c0_king=Substr( c0_fischer_cst, IndexOfslow(c0_fischer_cst,"{wK}")+4,4 )
		c0_rook=Substr( c0_fischer_cst, IndexOfslow(c0_fischer_cst,"{wRR}")+5,4 )
		c0_king2="wKg1"
		c0_rook2="wRf1"
		c0_wKingmoved = c0_true
		c0_wLRockmoved = c0_true
		c0_wRRockmoved = c0_true
		c0_w00 = c0_true
	else
		c0_king=Substr( c0_fischer_cst, IndexOfslow(c0_fischer_cst,"{bK}")+4,4 )
		c0_rook=Substr( c0_fischer_cst, IndexOfslow(c0_fischer_cst,"{bRR}")+5,4 )
		c0_king2="bKg8"
		c0_rook2="bRf8"
		c0_bKingmoved = c0_true
		c0_bLRockmoved = c0_true
		c0_bRRockmoved = c0_true
		c0_b00 = c0_true
	end if

else
  if(Substr(c0_move7,0,4)="000*") then

	 if(c0_sidemoves>0) then
		c0_king=Substr( c0_fischer_cst, IndexOfslow(c0_fischer_cst,"{wK}")+4,4 )
		c0_rook=Substr( c0_fischer_cst, IndexOfslow(c0_fischer_cst,"{wLR}")+5,4 )
		c0_king2="wKc1"
        	c0_rook2="wRd1"
		c0_wKingmoved = c0_true
        	c0_wLRockmoved = c0_true
        	c0_wRRockmoved = c0_true
        	c0_w00 = c0_true
	 else
        	c0_king=Substr( c0_fischer_cst, IndexOfslow(c0_fischer_cst,"{bK}")+4,4 )
		c0_rook=Substr( c0_fischer_cst, IndexOfslow(c0_fischer_cst,"{bLR}")+5,4 )
		c0_king2="bKc8"
        	c0_rook2="bRd8"
		c0_bKingmoved = c0_true
        	c0_bLRockmoved = c0_true
        	c0_bRRockmoved = c0_true
        	c0_b00 = c0_true
     	 end if

  else

	dim as string c0_from_at=Substr(c0_move7,0,2)
	dim as string c0_to_at=Substr(c0_move7,2,2)
	c0_moveto(c0_convH888(c0_from_at), c0_convH888(c0_to_at), c0_draw)

  end if
end if


if(len(c0_king)>0 and len(c0_rook)>0) then

	if(c0_draw) then
		c0_clear_at(Substr(c0_king,2,2))
		c0_clear_at(Substr(c0_rook,2,2))
		c0_add_piece(Substr(c0_king2,0,2)+Substr(c0_rook2,2,2))
		c0_moveto(c0_convH888(Substr(c0_rook2,2,2)), c0_convH888(Substr(c0_king2,2,2)), c0_draw)
		c0_add_piece(c0_rook2)
	else
		if( c0_not(c0_king=c0_king2)) then
            c0_position = sReplace(c0_position,c0_king,c0_king2)
        end if
		if( c0_not(c0_rook=c0_rook2)) then
            c0_position = sReplace(c0_position,c0_rook,c0_rook2)
        end if
	end if
	c0_lastmovepawn = 0;
end if

''
end sub
''

declare function c0_is_check_to_king (c0_ZKcolor as string) as byte
declare function c0_is_attacked_king_before_move (c0_Qfrom_at as string, _
    c0_Qto_at as string, c0_Qcolor as string) as byte

''
''  Function......... : c0_move_to
''  Description...... : To normally call move a piece...
''
''
sub c0_move_to ( c0_Zstr1 as string, c0_Zstr2 as string )
''
c0_moveto( c0_convH888(c0_Zstr1), c0_convH888(c0_Zstr2), c0_true )
c0_sidemoves = -c0_sidemoves
''
end sub
''

''
''  Function......... : c0_can_be_moved
''  Description...... : (internal) Can do a such move?
''
''
function c0_can_be_moved (c0_from_at as string, c0_to_at as string, _
    c0_just_move_or_eat as byte) as byte
''

 dim as byte c0_can = c0_false
 dim as integer c0_vert = val(Substr(c0_from_at,0,1))
 dim as integer c0_horiz= val(Substr(c0_from_at,1,1))
 dim as integer c0_vert2 = val(Substr(c0_to_at,0,1))
 dim as integer c0_horiz2= val(Substr(c0_to_at,1,1))

 dim as integer c0_p=IndexOf( c0_position, c0_convE2(c0_vert,c0_horiz) )

 if(c0_p>=0) then
			''[1]

 dim as string c0_color=Substr( c0_position,c0_p-2,1)
 dim as string c0_figure=Substr( c0_position,c0_p-1,1)

 if(c0_is_empty(c0_vert2,c0_horiz2) or c0_is_enemy(c0_vert2,c0_horiz2,c0_color)) then ''[2]

 dim as integer c0_Dvert=c0_vert2-c0_vert
 if(c0_Dvert<0) then
	c0_Dvert=-c0_Dvert
 end if
 dim as integer c0_Dhoriz=c0_horiz2-c0_horiz
 if(c0_Dhoriz<0) then
	c0_Dhoriz=-c0_Dhoriz
 end if



 if(c0_figure="p") then

	dim as integer c0_virziens
	if( c0_color="w" ) then
        c0_virziens=1
    else
        c0_virziens=-1
    end if
	if(c0_horiz2=c0_horiz) then

	  if( (c0_vert2=c0_vert+c0_virziens and _
        c0_is_empty(c0_vert2,c0_horiz2)) or _
	   (c0_color="w" and c0_vert2=4 and _
       c0_vert=2 and c0_is_empty(3,c0_horiz2) and _
       c0_is_empty(4,c0_horiz2)) or _
	   (c0_color="b" and c0_vert2=5 and _
       c0_vert=7 and c0_is_empty(5,c0_horiz2) and _
       c0_is_empty(6,c0_horiz2)) ) then
		c0_can = c0_true
      end if
	else

	  if( (c0_horiz2=c0_horiz+1 or c0_horiz2=c0_horiz-1) and _
            c0_vert2=c0_vert+c0_virziens) then
	    if(c0_is_enemy(c0_vert2,c0_horiz2,c0_color) or _
		 (c0_lastmovepawn=c0_horiz2 and _
			((c0_color="w" and c0_vert2=6) or _
            (c0_color="b" and c0_vert2=3)) ) ) then
        c0_can=c0_true
        end if
      end if
	end if

 end if

 if(c0_figure="N") then
	if( c0_Dvert+c0_Dhoriz=3 and c0_Dvert<>0 and c0_Dhoriz<>0 ) then
		c0_can=c0_true
	end if
 end if
 if(c0_figure="B") then
	if( (c0_Dvert>0 and c0_Dvert=c0_Dhoriz) and c0_is_emptyline(c0_vert,c0_horiz,c0_vert2,c0_horiz2)) then
        c0_can=c0_true
    end if
 end if
 if(c0_figure="R") then
	if( ((c0_Dvert=0 or c0_Dhoriz=0) and _
        c0_Dvert<>c0_Dhoriz) and _
        c0_is_emptyline(c0_vert,c0_horiz,c0_vert2,c0_horiz2)) then
    c0_can=c0_true
	end if
 end if
 if(c0_figure="Q") then
	if( (c0_Dvert=0 or c0_Dhoriz=0 or c0_Dvert=c0_Dhoriz) and _
        c0_is_emptyline(c0_vert,c0_horiz,c0_vert2,c0_horiz2)) then
    c0_can=c0_true
	end if
 end if

 if(c0_figure="K") then

	if((c0_Dvert=0 and c0_Dhoriz=1) or _
            (c0_Dhoriz=0 and c0_Dvert=1) or (c0_Dhoriz=1 and c0_Dvert=1)) then
        c0_can=c0_true
	else

	  if (c0_not(c0_just_move_or_eat) and c0_not( c0_fischer )) then
      if (c0_not(c0_is_check_to_king(c0_color))) then
      
		if(c0_color="w") then

		  if(c0_not( c0_wKingmoved ) and c0_vert=1 and _
                c0_horiz=5 and c0_vert2=1) then

			if( (c0_horiz2=7 and c0_not( c0_wRRockmoved ) and _
				c0_is_empty(1,6) and c0_is_empty(1,7) and _
				c0_not( c0_is_attacked_king_before_move("15", "16", c0_color)) and _
				c0_not( c0_is_attacked_king_before_move("15", "17", c0_color))) or _
			    (c0_horiz2=3 and c0_not( c0_wLRockmoved ) and _
				c0_is_empty(1,2) and c0_is_empty(1,3) and c0_is_empty(1,4) and _
				c0_not( c0_is_attacked_king_before_move("15", "14", c0_color)) and _
				c0_not( c0_is_attacked_king_before_move("15", "13", c0_color))) ) then
             c0_can=c0_true
            end if
           end if

		 else
		   if(c0_not( c0_bKingmoved ) and c0_vert=8 and c0_horiz=5 and c0_vert2=8) then

			if( (c0_horiz2=7 and c0_not( c0_bRRockmoved ) and _
				c0_is_empty(8,6) and c0_is_empty(8,7) and _
				c0_not( c0_is_attacked_king_before_move("85", "86", c0_color)) and _
				c0_not( c0_is_attacked_king_before_move("85", "87", c0_color))) or _
			    (c0_horiz2=3 and c0_not( c0_bLRockmoved ) and _
				c0_is_empty(8,2) and c0_is_empty(8,3) and c0_is_empty(8,4) and _
				c0_not( c0_is_attacked_king_before_move("85", "84", c0_color)) and _
				c0_not( c0_is_attacked_king_before_move("85", "83", c0_color))) ) then
              c0_can=c0_true
            end if
           end if

		 end if

      end if
      end if
    end if
 end if

 if(c0_not(c0_just_move_or_eat) and c0_can) then
  c0_can = c0_not( c0_is_attacked_king_before_move(c0_from_at, c0_to_at, c0_color))
 end if


 end if    ''[2]
 end if    ''[1]

 return c0_can

''
end function
''


''
''  Function......... : c0_is_check_to_king
''  Description...... :
''
''
function c0_is_check_to_king (c0_ZKcolor as string) as byte
''

 dim as byte c0_is_check=c0_false
 dim as integer c0_Zp=IndexOf( c0_position, (c0_ZKcolor & "K") )
 dim as integer c0_ZKhoriz=byteAt(Substr( c0_position,c0_Zp+2,1),0) - 96
 dim as integer c0_ZKvert=val(Substr( c0_position,c0_Zp+3,1))
 dim as string c0_ZK_at = str(c0_ZKvert) & str(c0_ZKhoriz)

 dim as integer c0_i=0
 while( len(c0_position)>c0_i)

	dim as string c0_Zcolor=Substr( c0_position,c0_i,1)
	dim as string c0_Zfigure=Substr( c0_position,c0_i+1,1)

	if(c0_Zcolor<>c0_ZKcolor) then

		 dim as integer c0_Zhoriz=byteAt(Substr( c0_position,c0_i+2,1),0) - 96
		 dim as integer c0_Zvert=val(Substr( c0_position,c0_i+3,1))
		 dim as string c0_Z_at = str(c0_Zvert) & str(c0_Zhoriz)

		 if(c0_can_be_moved( c0_Z_at, c0_ZK_at, c0_true)) then

			 c0_is_check=c0_true
			 exit while
		 end if
	end if
    c0_i+=5
  wend
 return c0_is_check


''
end function
''


''
''  Function......... : c0_is_attacked_king_before_move
''  Description...... :
''
''
function c0_is_attacked_king_before_move (c0_Qfrom_at as string, _
    c0_Qto_at as string, c0_Qcolor as string) as byte
''

  dim as byte c0_is_attack=c0_false

  dim as string c0_save_position=c0_position
  dim as integer c0_save_sidemoves=c0_sidemoves
  dim as byte c0_save_wKingmoved=c0_wKingmoved
  dim as byte c0_save_bKingmoved=c0_bKingmoved
  dim as byte c0_save_wLRockmoved=c0_wLRockmoved
  dim as byte c0_save_wRRockmoved=c0_wRRockmoved
  dim as byte c0_save_bLRockmoved=c0_bLRockmoved
  dim as byte c0_save_bRRockmoved=c0_bRRockmoved
  dim as byte c0_save_w00=c0_w00
  dim as byte c0_save_b00=c0_b00
  dim as string c0_save_become=c0_become

  dim as integer c0_save_lastmovepawn=c0_lastmovepawn

  c0_moveto(c0_Qfrom_at, c0_Qto_at, c0_false)
  c0_sidemoves = -c0_sidemoves

  if( c0_is_check_to_king(c0_Qcolor) ) then

	c0_is_attack=c0_true
  end if

  c0_position = c0_save_position
  c0_sidemoves = c0_save_sidemoves
  c0_wKingmoved = c0_save_wKingmoved
  c0_bKingmoved = c0_save_bKingmoved
  c0_wLRockmoved = c0_save_wLRockmoved
  c0_wRRockmoved = c0_save_wRRockmoved
  c0_bLRockmoved = c0_save_bLRockmoved
  c0_bRRockmoved = c0_save_bRRockmoved
  c0_lastmovepawn = c0_save_lastmovepawn
  c0_w00 = c0_save_w00
  c0_b00 = c0_save_b00
  c0_become = c0_save_become

  return c0_is_attack

''
end function
''


''
''  Function......... : c0_is_mate_to_king
''  Description...... :
''
''
function c0_is_mate_to_king (c0_ZKcolor as string, c0_just_mate as byte) as byte
''

 dim as byte c0_is_mate=c0_false
 if( c0_just_mate or c0_is_check_to_king(c0_ZKcolor) ) then

    dim as integer c0_i=0
    c0_is_mate=c0_true
    while(c0_is_mate and (len(c0_position)>c0_i))

	 dim as string c0_Zcolor=Substr( c0_position,c0_i,1)
	 if(c0_Zcolor=c0_ZKcolor) then

		 dim as integer c0_Zhoriz=byteAt(Substr( c0_position,c0_i+2,1),0) - 96
		 dim as integer c0_Zvert=val(Substr( c0_position,c0_i+3,1))
		 dim as string c0_Z_at = str(c0_Zvert) & str(c0_Zhoriz)
		 dim as integer c0_vi=1
         while(c0_is_mate and c0_vi<=8)
          dim as integer c0_vj=1
		  while(c0_is_mate and c0_vj<=8)

			dim as string c0_Z_to_at=str(c0_vi) & str(c0_vj)
			if(c0_can_be_moved( c0_Z_at, c0_Z_to_at, c0_false)) then

				 c0_is_mate=c0_false
				 exit while
			end if

          c0_vj+=1
          wend

         c0_vi+=1
         wend
	 end if

    c0_i+=5
    wend

 end if
 return c0_is_mate
''
end function
''

''
''  Function......... : c0_is_pate_to_king
''  Description...... :
''
''
function c0_is_pate_to_king (c0_ZWcolor as string) as byte
''
 dim as byte c0_is_pate=c0_true

 dim as integer c0_j=0
 while( c0_is_pate and len(c0_position)>c0_j)

	dim as string c0_Wcolor=Substr( c0_position,c0_j,1)
	if(c0_Wcolor=c0_ZWcolor) then

        dim as integer c0_Whoriz=byteAt(Substr( c0_position,c0_j+2,1),0) - 96
		dim as integer c0_Wvert=val(Substr( c0_position,c0_j+3,1))
		dim as string c0_W_at = str(c0_Wvert) & str(c0_Whoriz)
        dim as integer c0_wi=1
		while( c0_is_pate and c0_wi<=8 )
            dim as integer c0_wj=1
            while( c0_is_pate and c0_wj<=8 )

                dim as string c0_W_to_at=str(c0_wi) & str(c0_wj)
                if(c0_can_be_moved( c0_W_at, c0_W_to_at, c0_false)) then

                    c0_is_pate=c0_false
                    exit while
				end if
            c0_wj+=1
            wend
        c0_wi+=1
		wend
	end if
 c0_j+=5
 wend

 return c0_is_pate

''
end function
''


''
''  Function......... : c0_fischer_adjustmoved
''  Description...... : Adjust main variables after position is set...
''
''
sub c0_fischer_adjustmoved
''

if(IndexOfslow(c0_fischer_cst,"{bLR}")>=0 and IndexOfslow(c0_fischer_cst,"{bK}")>=0) then
    c0_bKingmoved = c0_false
    c0_bLRockmoved = c0_false
    c0_b00 = c0_false
end if
if(IndexOfslow(c0_fischer_cst,"{bRR}")>=0 and IndexOfslow(c0_fischer_cst,"{bK}")>=0) then
    c0_bKingmoved = c0_false
    c0_bRRockmoved = c0_false
    c0_b00 = c0_false
end if
if(IndexOfslow(c0_fischer_cst,"{wLR}")>=0 and IndexOfslow(c0_fischer_cst,"{wK}")>=0) then
    c0_wKingmoved = c0_false
    c0_wLRockmoved = c0_false
    c0_w00 = c0_false
end if
if(IndexOfslow(c0_fischer_cst,"{wRR}")>=0 and IndexOfslow(c0_fischer_cst,"{wK}")>=0) then
    c0_wKingmoved = c0_false
    c0_wRRockmoved = c0_false
    c0_w00 = c0_false
end if
''
end sub
''

''
''  Function......... : c0_fischer_cst_fCr
''  Description...... : Saves fischer movings for castling from Crafty standard...
''
function c0_fischer_cst_fCr (c0_move as string) as string
''

dim as string c0_ret8=""
if(c0_fischer) then
    if((len(c0_move)>4) and (Substr(c0_move,0,5)="O-O-O" or _
        Substr(c0_move,0,5)="0-0-0")) then
        c0_ret8="000*"
    else
        if((len(c0_move)>2) and (Substr(c0_move,0,3)="O-O" or _
            Substr(c0_move,0,3)="0-0")) then
            c0_ret8="00**"
        end if
    end if
end if
return c0_ret8

''
end function
''

''
''  Function......... : c0_fischer_cst_tCr
''  Description...... : Saves to Crafty standard...
''
function c0_fischer_cst_tCr (c0_move as string) as string
''

dim as string c0_ret8=""
if(c0_fischer) then

    if(Substr(c0_move,0,4)="000*") then
        c0_ret8="0-0-0"
    else
        if(Substr(c0_move,0,4)="00**") then
            c0_ret8="0-0"
        end if
    end if
end if
return c0_ret8

''
end function
''


''
''  Function......... : c0_fisch_castl_save
''  Description...... : Get castling settings into variable...
''
sub c0_fisch_castl_save (c0_fen_c as string, c0_fen_pos as string)
''

c0_fischer_cst = ""
dim as integer atW=IndexOf(c0_fen_pos,"wK")
dim as integer atB=IndexOf(c0_fen_pos,"bK")

if(atW>=0 and atB>=0) then

	c0_fischer_cst & =("{wK}" & Substr(c0_fen_pos,atW,5) & _
        "{bK}" & Substr(c0_fen_pos,atB,5))

    dim as integer c0_q8=1
	while(c0_q8<=16)

        dim as integer c0_99=0
        if(c0_q8<9) then
            c0_99= 96+c0_q8
        else
            c0_99=64+c0_q8-8
        end if
		dim as string c0_ch=chr(c0_99)

		dim as string c0_cl="w"
        if(c0_q8<9) then
            c0_cl= "b"
        end if

		dim as string c0_vt="1"
        if(c0_q8<9) then
            c0_vt="8"
        end if

        if(c0_q8<9) then
            c0_99=96+c0_q8-0
        else
            c0_99=96+c0_q8-8
        end if

	dim as string c0_hz=chr(c0_99)
	dim as string c0_rook=c0_cl & "R" & c0_hz & c0_vt & ";"

	if(IndexOfslow(c0_fen_c,c0_ch)>=0 and IndexOfslow(c0_fen_pos,c0_rook)>=0) then

	if(c0_q8<9) then

	  if(byteAt(Substr(c0_fen_pos,atB+2,1),0)>byteAt(c0_hz,0)) then
            		c0_fischer_cst &= "{bLR}"
	  else
            		c0_fischer_cst &= "{bRR}"
        end if

         else

		  if(byteAt(Substr(c0_fen_pos,atW+2,1),0)>byteAt(c0_hz,0)) then
            c0_fischer_cst &= "{wLR}"
		  else
            c0_fischer_cst &= "{wRR}"
          end if

         end if

		 c0_fischer_cst &= c0_rook
        end if

       c0_q8+=1
	wend

    c0_q8=0
	while(c0_q8<len(c0_fen_pos))

		dim as string c0_pc=Substr(c0_fen_pos,c0_q8+1,1)
		if(c0_pc="R") then

            dim as string c0_cl=Substr(c0_fen_pos,c0_q8,1)
            dim as string c0_hz=Substr(c0_fen_pos,c0_q8+2,1)
            dim as string c0_rook=Substr(c0_fen_pos,c0_q8,5)

            if(c0_cl="w") then

                if(IndexOfslow(c0_fischer_cst,"{wLR}")<0 and _
                    IndexOfslow(c0_fen_c,"Q")>=0 and _
                    byteAt(Substr(c0_fen_pos,atW+2,1),0)>byteAt(c0_hz,0)) then
                    c0_fischer_cst &= "{wLR}" & c0_rook
                else
                    if(IndexOfslow(c0_fischer_cst,"{wRR}")<0 and _
                        IndexOfslow(c0_fen_c,"K")>=0 and _
                        byteAt(Substr(c0_fen_pos,atW+2,1),0)<byteAt(c0_hz,0)) then
                        c0_fischer_cst &= "{wRR}" & c0_rook
                    end if
                end if

            else

                if(IndexOfslow(c0_fischer_cst,"{bLR}")<0 and _
                    IndexOfslow(c0_fen_c,"q")>=0 and _
                    byteAt(Substr(c0_fen_pos,atB+2,1),0)>byteAt(c0_hz,0)) then
                        c0_fischer_cst &= "{bLR}" & c0_rook
                else
                    if(IndexOfslow(c0_fischer_cst,"{bRR}")<0 and _
                        IndexOfslow(c0_fen_c,"k")>=0 and _
                        byteAt(Substr(c0_fen_pos,atB+2,1),0)<byteAt(c0_hz,0)) then
                        c0_fischer_cst &= "{bRR}" & c0_rook
                    end if
                end if

            end if

        end if

        c0_q8+=5
	wend

end if

''
end sub
''



''
''  Function......... : c0_set_board_situation
''  Description...... : Set board situation...
''
''
sub c0_set_board_situation ( c0_figlist as string, _
    c0_wK as byte, c0_wLR as byte, c0_wRR as byte, _
    c0_w_00 as byte, c0_bK as byte, c0_bLR as byte, _
    c0_bRR as byte, c0_b_00 as byte, c0_elpas as integer, _
    c0_ml as string, c0_s as integer )
''


c0_position = ""
dim as integer i=0
while(i<len(c0_figlist))

	c0_add_piece( Substr(c0_figlist,i,4) )
	i+=4
    if( i<len(c0_figlist) and Substr(c0_figlist,i,1)=";" ) then
        i+=1
    end if
wend

c0_wKingmoved = c0_wK
c0_bKingmoved = c0_bK
c0_wLRockmoved = c0_wLR
c0_wRRockmoved = c0_wRR
c0_bLRockmoved = c0_bLR
c0_bRRockmoved = c0_bRR
c0_w00 = c0_w_00
c0_b00 = c0_b_00

c0_lastmovepawn = c0_elpas

c0_become = ""
c0_become_from_engine = ""			'' just engine

c0_moveslist = c0_ml
c0_sidemoves = c0_s

''
end sub
''

'
''  Function......... : c0_set_FEN
''  Description...... : Sets position using FEN
''
sub c0_set_FEN (c0_fen_str as string)
''

dim as integer c0_vert7=8
dim as integer c0_horz7=1

dim as string c0_fs1=""
dim as string c0_fs2=""

dim as integer c0_i7=0
while(c0_i7<len(c0_fen_str))

    dim as string c0_ch7=Substr(c0_fen_str,c0_i7,1)
    if( c0_ch7=" " ) then
        exit while
    end if
    dim as integer c0_pusto=val(c0_ch7)
    if( c0_pusto>=1 and c0_pusto<=8) then
        dim as integer c0_j7=1
        while(c0_j7<=c0_pusto)
            c0_fs1 &= "."
            c0_j7+=1
        wend
    else
        c0_fs1 &= c0_ch7
    end if
    c0_i7+=1
wend

c0_fs1 &= (" " & SubstrAll(c0_fen_str,c0_i7))

c0_i7=0
while(c0_i7<len(c0_fs1))

    dim as string c0_ch7=Substr(c0_fs1,c0_i7,1)
    if( c0_ch7=" " ) then
        exit while
    end if

    dim as string c0_pos7 = chr(96+c0_horz7) & str(c0_vert7)
    dim as string c0_color7=" "
    if(c0_ch7="p" or c0_ch7="n" or c0_ch7="b" or c0_ch7="r" or c0_ch7="q" or c0_ch7="k" ) then
        c0_color7="b"
    end if
    if(c0_ch7="P" or c0_ch7="N" or c0_ch7="B" or c0_ch7="R" or c0_ch7="Q" or c0_ch7="K" ) then
        c0_color7="w"
    end if
    if(c0_color7<>" ") then

        if( c0_ch7="P" or  c0_ch7="p" ) then
            c0_ch7="p"
        else
            c0_ch7=UCase(c0_ch7)
        end if

        c0_fs2 &= (c0_color7 & c0_ch7 & c0_pos7 & ";")
    end if

    if(c0_ch7="/") then
        if(c0_horz7>1) then
            c0_vert7-=1
            c0_horz7=1
        end if
    else
        c0_horz7+=1
        if(c0_horz7>8) then
             c0_horz7=1
            c0_vert7-=1
        end if
    end if

    if(c0_vert7<1) then
        exit while
    end if

    c0_i7+=1
wend

while(c0_i7<len(c0_fs1))
    if( Substr(c0_fs1,c0_i7,1)=" " ) then
        exit while
    end if
    c0_i7+=1
wend

while(c0_i7<len(c0_fs1))
    if( Substr(c0_fs1,c0_i7,1)<>" "  ) then
        exit while
    end if
    c0_i7+=1
wend

'' which moves
dim as integer c0_side7move=1
if(c0_i7<len(c0_fs1) and Substr(c0_fs1,c0_i7,1)="b") then
    c0_side7move=-1
end if

while(c0_i7<len(c0_fs1))
    if( Substr(c0_fs1,c0_i7,1)=" " ) then
        exit while
    end if
    c0_i7+=1
wend

while(c0_i7<len(c0_fs1))
    if( Substr(c0_fs1,c0_i7,1)<>" "  ) then
        exit while
    end if
    c0_i7+=1
wend

'' castlings

dim as byte c0_wK7=c0_false
dim as byte c0_wRL7=c0_false
dim as byte c0_wRR7=c0_false
dim as byte c0_wcastl7=c0_false
dim as byte c0_bK7=c0_false
dim as byte c0_bRL7=c0_false
dim as byte c0_bRR7=c0_false
dim as byte c0_bcastl7=c0_false

dim as string c0_q7="-"
if(c0_i7<len(c0_fs1)) then

 c0_q7=SubstrAll(c0_fs1,c0_i7)
 dim as integer c0_at7=IndexOf(c0_q7," ")
 if( c0_at7>=0 ) then
    c0_q7=Substr(c0_q7,0,c0_at7)
 end if
end if

if( IndexOf(c0_q7,"K")<0 ) then
	c0_wRR7=c0_true
end if
if( IndexOf(c0_q7,"Q")<0 ) then
	c0_wRL7=c0_true
end if

if( IndexOf(c0_q7,"k")<0 ) then
	c0_bRR7=c0_true
end if
if( IndexOf(c0_q7,"q")<0 ) then
	c0_bRL7=c0_true
end if

if( IndexOfslow(c0_q7,"-")>=0 ) then
    c0_wK7=c0_true
    c0_bK7=c0_true
end if


c0_fisch_castl_save(c0_q7,c0_fs2)

while(c0_i7<len(c0_fs1))
    if( Substr(c0_fs1,c0_i7,1)=" " ) then
        exit while
    end if
    c0_i7+=1
wend

while(c0_i7<len(c0_fs1))
    if( Substr(c0_fs1,c0_i7,1)<>" "  ) then
        exit while
    end if
    c0_i7+=1
wend

'' en passant

c0_q7="-"
if(c0_i7<len(c0_fs1)) then
    c0_q7=Substr(c0_fs1,c0_i7,1)
end if

dim as integer c0_enpass7=0
if( IndexOfslow(c0_q7,"-")<0 ) then
    c0_enpass7=byteAt(c0_q7,0)-96
end if

while(c0_i7<len(c0_fs1))
    if( Substr(c0_fs1,c0_i7,1)=" " ) then
        exit while
    end if
    c0_i7+=1
wend

while(c0_i7<len(c0_fs1))
    if( Substr(c0_fs1,c0_i7,1)<>" "  ) then
        exit while
    end if
    c0_i7+=1
wend

'' remaining information is omitted

c0_set_board_situation( c0_fs2, c0_wK7, c0_wRL7, c0_wRR7, c0_wcastl7, c0_bK7, c0_bRL7, c0_bRR7, c0_bcastl7, c0_enpass7, c0_moveslist, c0_side7move )

''
end sub
''

''
''  Function......... : c0_set_start_position
''  Description...... : Set board situation...
''
sub c0_set_start_position ( c0_mlist as string )
''

if( len( c0_start_FEN ) >0 ) then
	c0_set_FEN( c0_start_FEN )
	if(c0_fischer) then
        c0_fischer_adjustmoved
    end if
else
					'' Just memorize position
	c0_position = "wpa2;wpb2;wpc2;wpd2;wpe2;wpf2;wpg2;wph2;" + _
		"wRa1;wNb1;wBc1;wQd1;wKe1;wBf1;wNg1;wRh1;" + _
		"bpa7;bpb7;bpc7;bpd7;bpe7;bpf7;bpg7;bph7;" + _
		"bRa8;bNb8;bBc8;bQd8;bKe8;bBf8;bNg8;bRh8;"
end if

c0_wKingmoved = c0_false
c0_bKingmoved = c0_false
c0_wLRockmoved = c0_false
c0_wRRockmoved = c0_false
c0_bLRockmoved = c0_false
c0_bRRockmoved = c0_false
c0_w00 = c0_false
c0_b00 = c0_false

c0_lastmovepawn = 0
c0_sidemoves = 1

c0_become = ""
c0_become_from_engine = ""

c0_moveslist = ""

if(len(c0_mlist)>0) then
    dim as integer c0_z=0
	while(c0_z<len(c0_mlist))

		dim as string c0_from_at=Substr(c0_mlist,c0_z,2)
		dim as string c0_to_at=Substr(c0_mlist,c0_z+2,2)
		if(c0_z+4<len(c0_mlist) and Substr(c0_mlist,c0_z+4,1)="[") then

			c0_become_from_engine = Substr(c0_mlist,c0_z+5,1)
			c0_z+=3

		else
            c0_become_from_engine = ""
        end if

		if(c0_fischer) then
            c0_fischer_cstl_move(c0_from_at & c0_to_at,c0_false)
		else
            c0_moveto(c0_convH888(c0_from_at), c0_convH888(c0_to_at), c0_false)
        end if
		c0_sidemoves = -c0_sidemoves
        c0_z+=4
    wend

	if( len(c0_start_FEN )>0 ) then

		c0_set_board_situation( c0_position, c0_wKingmoved, c0_wLRockmoved, _
            c0_wRRockmoved, c0_w00, c0_bKingmoved, c0_bLRockmoved, _
            c0_bRRockmoved, c0_b00, c0_lastmovepawn, c0_moveslist, c0_sidemoves )

	else

		dim as string c0_pos2=c0_position
		c0_position = ""
        dim as integer c0_q=0
        while(c0_q<len(c0_pos2))
            c0_add_piece(Substr(c0_pos2,c0_q,4))
            c0_q+=5
        wend
    end if

end if

c0_moveslist = c0_mlist

''
end sub
''

'' Functions with "_D_" in the middle are for usage above (from outside of this), they are for interface
''
''  Function......... : c0_D_is_check_to_king
''  Description...... : Is check to king?
''
function c0_D_is_check_to_king as byte
''
return c0_is_check_to_king(c0_CurColor())
''
end function
''

''
''  Function......... : c0_D_can_be_moved
''  Description...... : Is the chess move possible (legal by chess rules)?
''
function c0_D_can_be_moved (c0_Zstr1 as string, c0_Zstr2 as string) as byte
''
return c0_can_be_moved( c0_convH888(c0_Zstr1), c0_convH888(c0_Zstr2), c0_false)
''
end function
''

''
''  Function......... : c0_D_is_enemy
''  Description...... : Is an enemy piece on square?
''
function c0_D_is_enemy (c0_Zstr as string, c0_mycolor  as string) as byte
''
dim as string c0_Zs2=c0_convH888(c0_Zstr)
return c0_is_enemy( val(Substr(c0_Zs2,0,1)), val(Substr(c0_Zs2,1,1)), c0_mycolor)
''
end function
''

''
''  Function......... : c0_D_is_empty
''  Description...... : Is the square empty?
''
function c0_D_is_empty (c0_Zstr as string) as byte
''
dim as string c0_Zs2=c0_convH888(c0_Zstr)
return c0_is_empty( val(Substr(c0_Zs2,0,1)), val(Substr(c0_Zs2,1,1)))
''
end function
''

''
''  Function......... : c0_D_is_mate_to_king
''  Description...... : Is checkmate to king?
''
function c0_D_is_mate_to_king as byte
''
return c0_is_mate_to_king( c0_CurColor(), c0_false)
''
end function
''

''
''  Function......... : c0_D_is_stalemate_to_king
''  Description...... : Is stalemate to king?
''
function c0_D_is_stalemate_to_king as byte
''
dim as string c = c0_CurColor()
return c0_is_pate_to_king(c) and c0_not( c0_is_mate_to_king(c, c0_false))
''
end function
''

''
''  Function......... : c0_D_is_emptyline
''  Description...... : Is the line empty?
''
function c0_D_is_emptyline (c0_Zstr1 as string, c0_Zstr2 as string) as byte
''
dim as string c0_Zs1=c0_convH888(c0_Zstr1)
dim as string c0_Zs2=c0_convH888(c0_Zstr2)
return c0_is_emptyline( val(Substr(c0_Zs1,0,1)), val(Substr(c0_Zs1,1,1)) , _
 val(Substr(c0_Zs2,0,1)), val(Substr(c0_Zs2,1,1)))
''
end function
''

''
''  Function......... : c0_D_what_at
''  Description...... : What the piece on square?
''
function c0_D_what_at (c0_Zstr1 as string) as string
''
 dim as string c0_ret=""
 dim as integer c0_pz2=IndexOf( c0_position, c0_Zstr1 )
 if(c0_pz2>=0) then
    c0_ret=Substr( c0_position,c0_pz2-2,2)
 end if
 return c0_ret
''
end function
''

''  Function......... : c0_D_last_move_was
''  Description...... : What was the last move?
''
function c0_D_last_move_was as string
''

dim as string c0_ret=""
if( len(c0_moveslist)>0 ) then

 if (Substr( c0_moveslist, len(c0_moveslist)-1, 1 )="]" ) then
    c0_ret= Substr( c0_moveslist, len(c0_moveslist)-7, 7 )
 else
    c0_ret= Substr( c0_moveslist, len(c0_moveslist)-4, 4 )
 end if
end if
return c0_ret

''
end function
''



''
''  Function......... : c0_get_tag
''  Description...... : just get tag string
''
''
function c0_get_tag (s as string, t as string) as string
''

 dim as string ret=""
 dim as string ctg1="[" & t & "]"
 dim as string ctg2="[/" & t & "]"
 dim as string s2 = s
 dim as integer at1=IndexOfslow(s2,ctg1)
 if(at1>=0) then

	s2=SubstrAll(s2,at1+len(ctg1))
	at1=IndexOfslow(s2,ctg2)
	if(at1>=0) then
		ret=Substr(s2,0, at1)
	end if
 end if
 return ret
''
end function
''

''
''  Function......... : c0_ReplUrl
''  Description...... : sReplaces urls with html-links...
''
function c0_ReplUrl (s as string) as string
''

dim as string str2=s
do

 dim as string urls=""
 dim as integer at=IndexOfslow(str2,"http://")
 if(at>=0) then
    urls="HTTP://" & SubstrAll(str2,at+7)
 else

   at=IndexOfslow(str2,"https://")
   if(at>=0) then
        urls="HTTPS://" & SubstrAll(str2,at+8)
   end if
 end if

 if(len(urls)>0) then

   dim as integer at2=IndexOf(urls," ")
   if(at2>=0) then
        urls=Substr(urls,0,at2)
   end if

   str2=Substr(str2,0,at) & "<a href='" & urls & _
        "' target='blank' >link»</a>"  & SubstrAll(str2, at +len(urls))
  else

   exit do

  end if

loop

str2= sReplace( str2, "HTTP://", "http://" )
str2= sReplace( str2, "HTTPS://", "https://" )

return str2

''
end function
''

''
''  Function......... : c0_from_Crafty_standard
''  Description...... : Crafty notation (quite a standard)
''
function c0_from_Crafty_standard (c0_move as string, _
    c0_color47 as string) as string
''

c0_move=sReplace( c0_move, "ep", "" )
c0_move=sReplace( c0_move, "8Q", "8=Q" )
c0_move=sReplace( c0_move, "8R", "8=R" )
c0_move=sReplace( c0_move, "8B", "8=B" )
c0_move=sReplace( c0_move, "8N", "8=N" )
c0_move=sReplace( c0_move, "1Q", "1=Q" )
c0_move=sReplace( c0_move, "1R", "1=R" )
c0_move=sReplace( c0_move, "1B", "1=B" )
c0_move=sReplace( c0_move, "1N", "1=N" )

dim as string c0_becomes7=""
dim as integer c0_sh7=IndexOfslow(c0_move,"=")

dim as string c0_ret7=c0_fischer_cst_fCr(c0_move)

const c0_sk8="{ab}{ba}{bc}{cb}{cd}{dc}{de}{ed}{ef}{fe}{fg}{gf}{gh}{hg}"

if(len(c0_ret7)>0) then
    return c0_ret7
else
if(len(c0_move)>4 and (Substr(c0_move,0,5)="O-O-O" or Substr(c0_move,0,5)="0-0-0")) then

        if(c0_color47="w") then

		  if(IndexOf( c0_position,"wKc1")<0 and c0_can_be_moved( "15","13",c0_false) ) then
            c0_ret7="e1c1[0]"
          end if

        else

		  if(IndexOf( c0_position,"bKc8")<0 and c0_can_be_moved( "85","83",c0_false) ) then
            c0_ret7="e8c8[0]"
          end if

        end if
else
if(len(c0_move)>2 and (Substr(c0_move,0,3)="O-O" or Substr(c0_move,0,3)="0-0")) then

        if(c0_color47="w") then

		  if(IndexOf( c0_position,"wKg1")<0 and c0_can_be_moved( "15","17",c0_false) ) then
            c0_ret7="e1g1[0]"
          end if

        else

		  if(IndexOf( c0_position,"bKg8")<0 and c0_can_be_moved( "85","87",c0_false) ) then
            c0_ret7="e8g8[0]"
          end if

        end if

else
if( IndexOfslow(c0_sk8, Substr(c0_move,0,2))>=0 ) then

    dim as integer c0_Z81horiz=byteAt(c0_move,0) - 96
    dim as integer c0_Z82horiz=byteAt(c0_move,1) - 96

    dim as integer c0_i8=0
    while( len(c0_position)>c0_i8)

	dim as string c0_Z8color=Substr( c0_position,c0_i8,1)
	dim as string c0_Z8figure=Substr( c0_position,c0_i8+1,1)
	dim as integer c0_Z8horiz=byteAt(Substr( c0_position,c0_i8+2,1),0) - 96
	dim as integer c0_Z8vert=val(Substr( c0_position,c0_i8+3,1))
    dim as integer c0_Z82vert
    if(c0_color47="w") then
        c0_Z82vert=c0_Z8vert+1
    else
        c0_Z82vert=c0_Z8vert-1
    end if
	dim as string c0_Z8from_at72 = str(c0_Z8vert) & str(c0_Z8horiz)
	dim as string c0_Z8to_at72 = str(c0_Z82vert) & str(c0_Z82horiz)

	if(c0_Z8color=c0_color47 and c0_Z8figure="p" and c0_Z81horiz=c0_Z8horiz ) then

		if( c0_can_be_moved( c0_Z8from_at72, c0_Z8to_at72,c0_false) ) then

			c0_ret7=c0_convE777(c0_Z8from_at72) & c0_convE777(c0_Z8to_at72)
			exit while

        end if

    end if
    c0_i8+=5

    wend

	if(c0_sh7>=0) then

        c0_becomes7="[" & Substr(c0_move,c0_sh7+1,1) & "]"
	end if
    c0_ret7 &= c0_becomes7

else

 dim as integer c0_cp7=len(c0_move)

 dim as string c0_figure7=Substr(c0_move,0,1)
 if(c0_figure7="N" or c0_figure7="B" or c0_figure7="R" or c0_figure7="Q" or c0_figure7="K") then
    c0_move = SubstrAll(c0_move,1)

 else
    c0_figure7="p"
 end if

 if(c0_sh7>=0) then

	c0_becomes7="[" & Substr(c0_move,c0_sh7+1,1) & "]"
	c0_move = Substr(c0_move, 0, c0_sh7)
 end if
 c0_move=sReplace( c0_move, "+", "" )
 c0_move=sReplace( c0_move, "-", "" )
 c0_move=sReplace( c0_move, "x", "" )
 c0_move=sReplace( c0_move, "X", "" )
 c0_move=sReplace( c0_move, "#", "" )
 c0_move=sReplace( c0_move, "!", "" )
 c0_move=sReplace( c0_move, "?", "" )

 c0_cp7=len(c0_move)
 c0_cp7-=1
 dim as string c0_to_at7 = Substr(c0_move, c0_cp7-1,2)
 dim as integer c0_vert72= val(Substr(c0_move, c0_cp7,1))
 c0_cp7-=1
 dim as integer c0_horiz72= byteAt(Substr(c0_move, c0_cp7,1),0) - 96
 c0_cp7-=1
 dim as string c0_to_at72 = str(c0_vert72) & str(c0_horiz72)
 dim as integer c0_vert71=0
 dim as integer c0_horiz71=0

 if(c0_cp7>=0) then

    c0_vert71=val(Substr(c0_move,c0_cp7,1))
    if(c0_vert71<1 or c0_vert71>8) then
        c0_vert71=0
    else
        c0_cp7-=1
    end if

  else
    c0_vert71=0
  end if

 if(c0_cp7>=0) then

  c0_horiz71=byteAt(Substr(c0_move,c0_cp7,1),0) - 96
  c0_cp7-=1

  if(c0_horiz71<1 or c0_horiz71>8) then
    c0_horiz71=0
  end if

 else
    c0_horiz71=0
 end if

 dim as integer c0_i4=0
 while(len(c0_position)>c0_i4)

	dim as string c0_Z4color=Substr( c0_position,c0_i4,1)
	dim as string c0_Z4figure=Substr( c0_position,c0_i4+1,1)
	dim as integer c0_Z4horiz=byteAt(Substr( c0_position,c0_i4+2,1),0) - 96
	dim as integer c0_Z4vert=val(Substr( c0_position,c0_i4+3,1))
	dim as string c0_Z4from_at72 = str(c0_Z4vert) & str(c0_Z4horiz)
	dim as string c0_Z4from_at7 = Substr( c0_position,c0_i4+2,2)

	if(c0_Z4color=c0_color47 and c0_figure7=c0_Z4figure) then

		 if((c0_vert71=0 or c0_vert71=c0_Z4vert) and _
			(c0_horiz71=0 or c0_horiz71=c0_Z4horiz) ) then

				if( c0_can_be_moved( c0_Z4from_at72,c0_to_at72,c0_false)) then

					c0_ret7 = c0_Z4from_at7 & c0_to_at7 & c0_becomes7
					exit while
				end if
		 end if

    end if

    c0_i4+=5
 wend

end if
end if
end if
end if

return c0_ret7

''
end function
''

''
''  Function......... : c0_PG_parseString
''  Description...... : Parses own string for chess moves...
''                      Annotatations and comments is possible to add
''
function c0_PG_parseString (s as string) as byte
''

dim as byte f_error=c0_false
dim as integer gaj=1
dim as string move=""
dim as string color7="w"
dim as string resultl="[1:0][1-0][1 : 0][1 - 0][0:1][0-1][0 : 1][0 - 1][1/2][1 / 2] [0.5:0.5][1/2:1/2][1/2-1/2][1/2 - 1/2][1/2 : 1/2][*]"

dim as string commentv=""

c0_PG_1 = ""

if(len(c0_NAGs)=0) then
    'c0_NAGs_define
end if

dim as string c0_1save_position=c0_position
dim as integer c0_1save_sidemoves=c0_sidemoves
dim as byte c0_1save_wKingmoved=c0_wKingmoved
dim as byte c0_1save_bKingmoved=c0_bKingmoved
dim as byte c0_1save_wLRockmoved=c0_wLRockmoved
dim as byte c0_1save_wRRockmoved=c0_wRRockmoved
dim as byte c0_1save_bLRockmoved=c0_bLRockmoved
dim as byte c0_1save_bRRockmoved=c0_bRRockmoved
dim as byte c0_1save_w00=c0_w00
dim as byte c0_1save_b00=c0_b00
dim as string c0_1save_become=c0_become
dim as string c0_1save_become_from_engine=c0_become_from_engine
dim as integer c0_1save_lastmovepawn= c0_lastmovepawn
dim as string c0_1save_moveslist= c0_moveslist

if( len(c0_start_FEN)>0 ) then
    s= ( "{[FEN " & c0_start_FEN & "]} " ) & s
    if(c0_sidemoves<0) then
        color7="b"
    end if
else

    c0_position = "wpa2,wpb2,wpc2,wpd2,wpe2,wpf2,wpg2,wph2," & _
    "wRa1,wNb1,wBc1,wQd1,wKe1,wBf1,wNg1,wRh1," & _
    "bpa7,bpb7,bpc7,bpd7,bpe7,bpf7,bpg7,bph7," & _
    "bRa8,bNb8,bBc8,bQd8,bKe8,bBf8,bNg8,bRh8,"

    c0_moveslist = ""

    c0_wKingmoved = c0_false
    c0_bKingmoved = c0_false
    c0_wLRockmoved = c0_false
    c0_wRRockmoved = c0_false
    c0_bLRockmoved = c0_false
    c0_bRRockmoved = c0_false
    c0_w00 = c0_false
    c0_b00 = c0_false

    c0_lastmovepawn = 0
    c0_sidemoves = 1
end if

c0_become =""
c0_become_from_engine =""
dim as string c_v="0123456789"
dim as integer k=0
dim as string reminder=""

dim as integer st_gaj=1
dim as integer st_atq=IndexOfslow(s,".")-1

if(st_atq>=0) then

  dim as string st_s=""
  while(st_atq>=0)

	 dim as string st_c=Substr(s,st_atq,1)
	 if( IndexOfslow(c_v,st_c) < 0 ) then
		exit while
	 end if

	 st_s=st_c & st_s
     st_atq -=1
  wend

 if(len(st_s)>0) then
    st_gaj=val(st_s)
 end if

end if

dim as integer i=len(s)

while( i>0 )
    if( SubstrAll(s,i-1)<>" " ) then
        exit while
    end if
    i-=1
wend
s=Substr(s,0,i)

dim as integer atwas=-1
dim as integer atcnt=0
dim as string Nag=""
dim as string Nag_txt=""
dim as integer Nag_at2=0

i=0
while(i<len(s))

 if( atwas<i ) then
    atwas=i
    atcnt=0
 else
    if( atwas<=i ) then
      atcnt+=1
    end if
 end if

 if( atcnt>50 ) then
    print("Sorry, can't parse this PGN! Errors inside.")
    f_error=c0_true
    exit while
 end if

 dim as string c=Substr(s,i,1)

 while( c=" " and (i+1)<len(s) and Substr(s,i+1,1)=" " )
    i+=1
    c=Substr(s,i,1)
 wend

 if( c=" " and (i+1)<len(s) and IndexOfslow( "{([$", Substr(s,i+1,1) )>=0) then
    i+=1
    c=Substr(s,i,1)
 end if

 commentv=""

 if(c="$") then

	 Nag= Substr(s,i,3)
     k=0
	 while(k<len(Nag))

		c=Substr(Nag,k,1)
		if( IndexOfslow(c_v,c) < 0 ) then
            Nag=Substr(Nag,0,k)
            exit while
        end if
        k+=1
	 wend

	 if(len(Nag)>0) then

		Nag_txt=""
		Nag_at2 = IndexOfslow(c0_NAGs,"[" & Nag & "]")
		if(Nag_at2>=0) then

			Nag_txt = SubstrAll(c0_NAGs, Nag_at2+len(Nag)+3)
			Nag_txt = Substr(Nag_txt, 0, IndexOfslow(Nag_txt,"[")-1)

		else
            Nag_txt = "Nag:" & Nag
        end if
		s=Substr(s,0,i) &  "{" & "[" & Nag_txt & "]"  & "}"  & _
                SubstrAll(s,i+len(Nag)+1)
	  end if
	  c=Substr(s,i,1)
 end if

 if(c="{" or c="(") then

   dim as integer cc=1
   dim as string c1=")"
   if(c="{") then
	c1="}"
   end if

   commentv=c
   i+=1
   while(i<len(s) and cc>0)

	dim as string c2=Substr(s,i,1)
	commentv &= c2
	if(c2=c) then
		cc+=1
	end if
	if(c2=c1) then
		cc-=1
	end if
	if(i+1=len(s) and cc>0) then
        commentv &= c1
    end if
    i+=1
   wend

  if(len(commentv)>0) then

	do  'loop

	   dim as integer Nag_at=IndexOfslow(commentv,"$")
	   if( Nag_at<0) then
		exit do
	   end if
	   Nag= Substr(commentv,Nag_at+1,3)
       k=0
	   while(k<len(Nag))

		c=Substr(Nag,k,1)
		if( IndexOfslow(c_v,c) < 0 ) then
            Nag=Substr(Nag,0,k)
            exit while
        end if
		k+=1
       wend

	  if(len(Nag)>0) then

		Nag_txt=""
		Nag_at2 = IndexOfslow(c0_NAGs,"[" & Nag & "]")
		if(Nag_at2>=0) then

			Nag_txt = SubstrAll(c0_NAGs, Nag_at2+len(Nag)+3)
			Nag_txt = Substr(Nag_txt, 0, IndexOfslow(Nag_txt,"[")-1)

		else
            Nag_txt = "Nag:" & Nag
        end if

		commentv=Substr(commentv,0,Nag_at) & "[" & Nag_txt & "]"  & _
                SubstrAll(commentv,Nag_at +len(Nag)+1)

	 else
        exit do
	 end if
    loop
   end if

  if( color7="b" ) then
    dim as integer j=i
	while(j<i+15)

		dim as string pj1=Substr(s,j,1)
		if( IndexOfslow("{([$",pj1)>=0 ) then
			exit while
		end if
		if( Substr(s,j,3)="..." ) then
            i=j+3
            exit while
        end if
        j+=1
	wend

  end if
  i-=1

 else
  if( c="."  or (c=" " and color7="b"  ) ) then

     move=""
     while(i<len(s) and (Substr(s,i,1)=" " or Substr(s,i,1)="."))
        i+=1
     wend
     c=Substr(s,i,1)
     while(i<len(s))

        c=Substr(s,i,1)
        if( c=" "  ) then
            exit while
        end if
        move &= c
        i+=1
	 wend

     if(len(move)>0 and IndexOfslow(move,"Z0")<0) then

	 if(IndexOfslow(resultl,move)>=0 ) then

		exit while

	 else

	dim as string move2=c0_from_Crafty_standard(move,color7)

	if(len(move2)=0) then
        print ( "Can't parse this PGN! move:" & str(gaj) & "." & color7 & " " & move)
        print ( c0_position )

        f_error=c0_true
        exit while
    end if

	dim as integer from_horiz4=byteAt(Substr(move2,0,1),0) - 96
	dim as integer from_vert4=val(Substr(move2,1,1))
	dim as integer to_horiz4=byteAt(Substr(move2,2,1),0) - 96
	dim as integer to_vert4=val(Substr(move2,3,1))

	dim as string from_move = str(from_vert4) & str(from_horiz4)
	dim as string to_move = str(to_vert4) & str(to_horiz4)

	if(len(move2)>4 and Substr(move2,4,1)="[") then
        	c0_become_from_engine = Substr(move2,5,1)
	else
        c0_become_from_engine = "Q"
    end if

	if(c0_fischer) then
        c0_fischer_cstl_move(move2,c0_false)

	else
        c0_moveto(from_move, to_move, c0_false)
    end if
	c0_sidemoves = -c0_sidemoves

	c0_PG_1 &= move2

	c0_become_from_engine =""
	c0_become =""

	if( color7="w" ) then
		color7="b"
        i-=1
	else
		color7="w"
		gaj+=1
	end if

	 if( color7="w" and len(s)-i<10) then

		reminder = SubstrAll(s,i+1)
		while(len(reminder)>0 and Substr(reminder,0,1)=" ")
            reminder=SubstrAll(reminder,1)
        wend
		if(len(reminder)>0 and IndexOfslow(resultl,reminder)>=0 ) then
			exit while
		end if

	 end if

	 end if
	end if

 else

     if(len(s)-i<10) then

        reminder = SubstrAll(s,i)
        while(len(reminder)>0 and Substr(reminder,0,1)=" ")
            reminder=SubstrAll(reminder,1)
        wend
        if(len(reminder)>0 and IndexOfslow(resultl,reminder)>=0 ) then
            exit while
        end if

      end if
 end if
 end if

 i+=1

wend



c0_position = c0_1save_position
c0_sidemoves = c0_1save_sidemoves
c0_wKingmoved = c0_1save_wKingmoved
c0_bKingmoved = c0_1save_bKingmoved
c0_wLRockmoved = c0_1save_wLRockmoved
c0_wRRockmoved = c0_1save_wRRockmoved
c0_bLRockmoved = c0_1save_bLRockmoved
c0_bRRockmoved = c0_1save_bRRockmoved
c0_w00 = c0_1save_w00
c0_b00 = c0_1save_b00
c0_become = c0_1save_become
c0_become_from_engine = c0_1save_become_from_engine
c0_lastmovepawn = c0_1save_lastmovepawn
c0_moveslist = c0_1save_moveslist

return f_error

''
end function
''


''
''  Function......... : c0_PG_gettable

''  Description...... : In fact this part is not very used
''                      but can be advanced.
''
sub c0_PG_gettable
''

dim as string rc=""

dim as string Event_Name=""
dim as string Event_Site=""
dim as string Event_Date=""
dim as string Roundv=""
dim as string White=""
dim as string Black=""
dim as string Result=""
dim as string ECO=""
dim as string WhiteElo=""
dim as string BlackElo=""
dim as string Game_Date=""
dim as string Source_Date=""

dim as string AddInfo=""

dim as string htms=""

dim as string CR=( chr(13) &  chr(10) )

c0_PGN_header = ""

PGN_text = sReplace( PGN_text,"  ", " " )

dim as string str2=PGN_text

do  'loop

 dim as integer at2=IndexOfslow(str2,"[")


 if(at2<0) then
	exit do
 end if

 dim as integer at2_1=IndexOfslow(str2,"(")
 dim as integer at2_2=IndexOfslow(str2,"{")
 if((at2_1>=0 and at2_1<at2) or (at2_2>=0 and at2_2<at2)) then
	exit do
 end if

 dim as string buf2= SubstrAll(str2,at2+1)
 buf2= Substr(buf2,0, IndexOfslow(buf2,"]") )
 str2= SubstrAll(str2,at2+len(buf2)+2)


 c0_PGN_header &= buf2 & CR

 buf2= c0_ReplUrl(buf2)

 buf2= sReplace( buf2,"'","" )
 buf2= sReplace( buf2,chr (34),"" )
 ''buf2= sReplace( buf2,"–","-" )

 dim as string buf3=UCase(buf2)



 dim as integer at9 = IndexOf(buf3,"SETUP ")
 if(at9>=0 and at9<3) then
    c0_fischer = (Substr(buf2,at9+6,1)="1")
 end if

 dim as integer at3 = IndexOf(buf3,"FEN ")
 if(at3>=0 and at3<3) then

    if( len(c0_start_FEN)=0 ) then
      c0_start_FEN = SubstrAll(buf2,at3+4)
      c0_set_start_position("")
    end if

 else

    at3 = IndexOf(buf3,"EVENT ")
    if(at3>=0) then
        Event_Name=SubstrAll(buf2,at3+6)
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"SITE ")
        if(at3>=0) then
            Event_Site=SubstrAll(buf2,at3+5)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"DATE ")
        if(at3>=0 and at3<3) then
            Game_Date=SubstrAll(buf2,at3+5)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"ROUND ")
        if(at3>=0) then
            Roundv=SubstrAll(buf2,at3+6)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"WHITE ")
        if(at3>=0) then
            White=SubstrAll(buf2,at3+6)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"BLACK ")
        if(at3>=0) then
            Black=SubstrAll(buf2,at3+6)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"ECO ")
        if(at3>=0) then
            ECO=SubstrAll(buf2,at3+4)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"WHITEELO ")
        if(at3>=0) then
            WhiteElo=SubstrAll(buf2,at3+9)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"BLACKELO ")
        if(at3>=0) then
            BlackElo=SubstrAll(buf2,at3+9)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"EVENTDATE ")
        if(at3>=0) then
            Event_Date=SubstrAll(buf2,at3+10)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"SOURCEDATE ")
        if(at3>=0) then
            Source_Date=SubstrAll(buf2,at3+11)
        end if
    end if
    if(at3<0) then
        at3 = IndexOf(buf3,"RESULT ")
        if(at3>=0) then
            Result=SubstrAll(buf2,at3+7)
        end if
    end if
    if(at3<0) then
        if(len(AddInfo)>0) then
            AddInfo=AddInfo & "<BR>"
        end if
        AddInfo=AddInfo & buf2
    end if


 end if

loop

 str2= c0_ReplUrl(str2)


 c0_errflag = c0_PG_parseString(str2)

 if(c0_fischer and len(c0_fischer_cst )>0) then
    c0_fischer_adjustmoved
 end if

 dim as integer at3 = IndexOfslow(str2,"*")
 if(at3>=0) then
	Result="not finished"
 end if
 at3 = IndexOfslow(str2,"1/2")
 if(at3>=0) then
	Result="1/2-1/2"
 end if
 at3 = IndexOfslow(str2,"1-0")
 if(at3>=0) then
	Result="1:0"
 end if
 at3 = IndexOfslow(str2,"1:0")
 if(at3>=0) then
	Result="1:0"
 end if
 at3 = IndexOfslow(str2,"0-1")
 if(at3>=0) then
	Result="0:1"
 end if
 at3 = IndexOfslow(str2,"0:1")
 if(at3>=0) then
	Result="0:1"
 end if


''
end sub
''


'''''''''' PGN parsers...
''
''  Function......... : c0_get_moves_from_PGN

''  Description...... : Parses PGN moves from string variable to
''                      own string for chess moves...
''

''
function c0_get_moves_from_PGN (c0_PGN_str as string) as string
''

PGN_text = c0_PGN_str

c0_PG_gettable

if(c0_errflag ) then
    print ( "There was an error in PGN parsing!")
end if

return c0_PG_1

''
end function
''


''
''  Function......... : c0_to_Crafty_standard
''  Description...... :
''
function c0_to_Crafty_standard (c0_move as string, _
    c0_color47 as string) as string
''

dim as string c0_ret9=c0_fischer_cst_tCr(c0_move)
 if(len(c0_ret9)>0) then

	c0_fischer_cstl_move(c0_move,c0_false)
	c0_sidemoves = -c0_sidemoves
	return c0_ret9
 end if

 dim as string c0_pos9=c0_position
 dim as integer c0_at9=IndexOf( c0_position, Substr(c0_move,0,2) )
 dim as integer c0_at7=IndexOf( c0_position, Substr(c0_move,2,2) )
 c0_become_from_engine = ""
 if( len(c0_move)>4 ) then
    c0_become_from_engine = Substr(c0_move,5,1)
 end if

 if(c0_at9>=0 ) then

  dim as string c0_9figure=Substr( c0_position, c0_at9-1,1 )
  dim as string c0_9color=Substr( c0_position, c0_at9-2,1 )
  if( c0_9color=c0_color47 ) then

    dim as integer c0_Z4horiz=byteAt(Substr(c0_move,0,1),0) - 96
    dim as integer c0_Z4vert=val(Substr(c0_move,1,1))
    dim as string c0_Z4from_at72 = str(c0_Z4vert) & str(c0_Z4horiz)
    dim as integer c0_Z5horiz=byteAt(Substr(c0_move,2,1),0) - 96
    dim as integer c0_Z5vert=val(Substr(c0_move,3,1))
    dim as string c0_Z5to_at72 = str(c0_Z5vert) & str(c0_Z5horiz)

    if( len(c0_become_from_engine)=0 and c0_9figure="p" and (c0_Z5vert=8 or c0_Z5vert=1) ) then
        c0_become_from_engine = "Q"
    end if

    if( c0_can_be_moved( c0_Z4from_at72,c0_Z5to_at72,c0_false ) ) then

    if( c0_9figure<>"p" ) then

	dim as integer c0_figc9=0
    dim as integer c0_i4=0
    dim as string c0_Q4color
    dim as string c0_Q4figure
	while(len(c0_position)>c0_i4)

        c0_Q4color=Substr( c0_position,c0_i4,1)
        c0_Q4figure=Substr( c0_position,c0_i4+1,1)
        if(c0_Q4color=c0_color47 and c0_9figure=c0_Q4figure) then
            c0_figc9+=1
        end if
        c0_i4+=5
    wend

    c0_i4=0
	while(len(c0_position)>c0_i4)

        c0_Q4color=Substr( c0_position,c0_i4,1)
        c0_Q4figure=Substr( c0_position,c0_i4+1,1)
        dim as integer c0_Q4horiz=byteAt(Substr( c0_position,c0_i4+2,1),0) - 96
        dim as integer c0_Q4vert=val(Substr( c0_position,c0_i4+3,1))
        dim as string c0_Q4from_at72 = str(c0_Q4vert) & str(c0_Q4horiz)
        dim as string c0_Q4from_at7 = Substr( c0_position,c0_i4+2,2)

        if(c0_Q4color=c0_color47 and c0_9figure=c0_Q4figure and c0_Q4from_at7 <>Substr(c0_move,0,2) ) then

		 if( c0_can_be_moved( c0_Q4from_at72, c0_Z5to_at72,c0_false)) then

			if( c0_figc9 < 3 and c0_Z4horiz<>c0_Q4horiz ) then
				c0_ret9 &= Substr(c0_move,0,1)
			else
				c0_ret9 &= Substr(c0_move,0,2) & "-"
			end if
			exit while
		 end if
		end if
        c0_i4+=5
    wend

    end if

	c0_moveto( c0_Z4from_at72,c0_Z5to_at72,c0_false )
	c0_sidemoves = -c0_sidemoves

	if( c0_9figure="K" and c0_9color="w" and Substr(c0_move,0,4) = "e1g1" ) then
        c0_ret9="O-O"
	else
	 if( c0_9figure="K" and c0_9color="b" and Substr(c0_move,0,4) = "e8g8" ) then
        c0_ret9="O-O"
	else
	 if( c0_9figure="K" and c0_9color="w" and Substr(c0_move,0,4) = "e1c1" ) then
        c0_ret9="O-O-O"
	else
	 if( c0_9figure="K" and c0_9color="b" and Substr(c0_move,0,4) = "e8c8" ) then
        c0_ret9="O-O-O"
	else

        if (c0_9figure<>"p") then

            c0_ret9 = c0_9figure & c0_ret9
        end if

		if( len(c0_pos9) > len(c0_position) ) then

			 if( (len(c0_ret9)>0) and Substr( c0_ret9, len(c0_ret9)-1,1)="-" ) then
                c0_ret9=Substr(c0_ret9,0,len(c0_ret9)-1)
             end if
			 c0_ret9 &= "x"
		end if

		if( len(c0_ret9)>0 and Substr(c0_ret9,0,1)="x" ) then
            c0_ret9= Substr(c0_move,0,1) & c0_ret9
        end if

		c0_ret9 &= Substr(c0_move,2,2)

		if( len(c0_become_from_engine)>0 ) then
            c0_ret9 &= "=" & c0_become_from_engine
        end if
        dim as string c0_color49="w"
        if( c0_color47="w" ) then
            c0_color49="b"
        end if

		if( c0_is_mate_to_king( c0_color49, c0_true ) ) then
           c0_ret9 &= "#"
		else
            if( c0_is_check_to_king( c0_color49 ) ) then
                c0_ret9 &= "+"
            end if
        end if

        end if
        end if
        end if
        end if

      end if

   end if

 end if

return c0_ret9
''
end function
''



''
''  Function......... : c0_put_to_PGN
''  Description...... : To write moveslist to PGN string
''

function c0_put_to_PGN (c0_moves_str as string) as string
''

if( len(c0_moves_str)=0 ) then
    c0_moves_str=c0_moveslist
end if

c0_errflag = c0_false
dim as string c0_1save_position=c0_position
dim as integer c0_1save_sidemoves=c0_sidemoves
dim as byte c0_1save_wKingmoved=c0_wKingmoved
dim as byte c0_1save_bKingmoved=c0_bKingmoved
dim as byte c0_1save_wLRockmoved=c0_wLRockmoved
dim as byte c0_1save_wRRockmoved=c0_wRRockmoved
dim as byte c0_1save_bLRockmoved=c0_bLRockmoved
dim as byte c0_1save_bRRockmoved=c0_bRRockmoved
dim as byte c0_1save_w00=c0_w00
dim as byte c0_1save_b00=c0_b00
dim as string c0_1save_become=c0_become
dim as string c0_1save_become_from_engine=c0_become_from_engine
dim as integer c0_1save_lastmovepawn= c0_lastmovepawn
dim as string c0_1save_moveslist= c0_moveslist

if( len(c0_start_FEN)>0 ) then
    c0_set_FEN( c0_start_FEN )
    c0_fischer_adjustmoved()
else

    c0_position = "wpa2,wpb2,wpc2,wpd2,wpe2,wpf2,wpg2,wph2," & _
    "wRa1,wNb1,wBc1,wQd1,wKe1,wBf1,wNg1,wRh1," & _
    "bpa7,bpb7,bpc7,bpd7,bpe7,bpf7,bpg7,bph7," & _
    "bRa8,bNb8,bBc8,bQd8,bKe8,bBf8,bNg8,bRh8,"

    c0_moveslist = ""

    c0_wKingmoved = c0_false
    c0_bKingmoved = c0_false
    c0_wLRockmoved = c0_false
    c0_wRRockmoved = c0_false
    c0_bLRockmoved = c0_false
    c0_bRRockmoved = c0_false
    c0_w00 = c0_false
    c0_b00 = c0_false

    c0_lastmovepawn = 0
    c0_sidemoves = 1
end if

c0_become = ""
c0_become_from_engine = ""

dim as string c0_PGN_ret=""

dim as string Result=""

dim as string CR=( chr(13) &  chr(10) )
dim as integer c0_i7=0

dim as string c0_qh = c0_PGN_header
dim as string c0_move8=""
 
while(len ( c0_qh )>0 )

    dim as integer c0_at_q5=IndexOfslow( c0_qh, CR )
    dim as string c0_hl=Substr ( c0_qh, 0, c0_at_q5 )
	dim as string c0_q9=UCase (c0_hl)
    c0_qh = SubstrAll ( c0_qh, c0_at_q5+2 )
	dim as integer c0_at_q8=IndexOf( c0_q9, "FEN " )
	if(c0_at_q8<0 and c0_fischer ) then
        c0_at_q8=IndexOf( c0_q9, "SETUP " )
    end if

	if( c0_at_q8<0 or c0_at_q8>3 ) then

        c0_PGN_ret &= ( "[" & c0_hl & "]" & CR )
        dim as integer c0_at_q9=IndexOf( c0_q9, "RESULT " )
        if( c0_at_q9>=0 ) then

            Result=SubstrAll( c0_hl, c0_at_q9 + 7 )
            Result=sReplace( Result, "'", "" )
 		end if
	end if
 c0_i7+=1
wend

dim peka as string = chr(32)

if( len(c0_start_FEN )>0 ) then

	if(c0_fischer) then
        c0_PGN_ret &= "[SetUp " & peka & "1" & peka & "]" & CR
    end if
	c0_PGN_ret &= "[FEN "  & peka & c0_start_FEN & peka & "]" & CR
end if

if( len(c0_PGN_ret)>0 ) then
    c0_PGN_ret &= CR
end if

dim as integer c07_gaj=0
dim as string c07_col="b"
c0_i7=0
while(c0_i7< len(c0_moves_str))

if(c07_col="w") then
    c07_col="b"
else
    c07_col="w"
    c07_gaj+=1
end if

 c0_move8=Substr( c0_moves_str, c0_i7, 4 )
 c0_i7+=4
 if( c0_i7< len(c0_moves_str) and Substr( c0_moves_str, c0_i7, 1 )="[" ) then

	c0_move8 &= Substr( c0_moves_str, c0_i7, 3 )
	c0_i7+=3
 end if

 dim as string c0_move9=c0_to_Crafty_standard( c0_move8, c07_col )
 if( len(c0_move9)>0 ) then

    if( c07_col="w" ) then
        c0_PGN_ret &= str(c07_gaj) & ". "
    end if
	c0_PGN_ret &= c0_move9 & " "

 else
    c0_errflag = c0_true
    exit while
 end if

wend


if(c0_not( c0_errflag )) then
    c0_PGN_ret &= " "  & Result
end if

c0_position = c0_1save_position
c0_sidemoves = c0_1save_sidemoves
c0_wKingmoved = c0_1save_wKingmoved
c0_bKingmoved = c0_1save_bKingmoved
c0_wLRockmoved = c0_1save_wLRockmoved
c0_wRRockmoved = c0_1save_wRRockmoved
c0_bLRockmoved = c0_1save_bLRockmoved
c0_bRRockmoved = c0_1save_bRRockmoved
c0_w00 = c0_1save_w00
c0_b00 = c0_1save_b00
c0_become = c0_1save_become
c0_become_from_engine = c0_1save_become_from_engine
c0_lastmovepawn = c0_1save_lastmovepawn
c0_moveslist = c0_1save_moveslist

if(c0_errflag ) then
    print ( "Can't parse " & str(c07_gaj) & c07_col  & ":" & c0_move8);
end if

if( len(c0_start_FEN)>0 ) then

	c0_set_board_situation( c0_position, c0_wKingmoved, c0_wLRockmoved, _
        c0_wRRockmoved, c0_w00, c0_bKingmoved, c0_bLRockmoved, c0_bRRockmoved, _
        c0_b00, c0_lastmovepawn, c0_moveslist, c0_sidemoves )
end if

return c0_PGN_ret
''
end function
''

''
''  Function......... : c0_DCN
''  Description...... : Internal to get next possible moves
''
function c0_DCN (c0_D7from_at as string, _
    c0_Dvert_TX as integer, c0_Dhoriz_TX as integer, _
    c0_Dcntx as byte) as string
''

 dim as string c0_D7poss=""
 dim as string c0_c7K=""

 dim as integer saveD1sidemoves=c0_sidemoves
 dim as byte saveD1wKingmoved=c0_wKingmoved
 dim as byte saveD1bKingmoved=c0_bKingmoved
 dim as byte saveD1wLRockmoved=c0_wLRockmoved
 dim as byte saveD1wRRockmoved=c0_wRRockmoved
 dim as byte saveD1bLRockmoved=c0_bLRockmoved
 dim as byte saveD1bRRockmoved=c0_bRRockmoved
 dim as byte saveD1w00=c0_w00
 dim as byte saveD1b00=c0_b00
 dim as integer saveD1lastmovepawn=c0_lastmovepawn
 dim as string saveD1position=c0_position
 dim as string saveD1become=c0_become

 dim as integer c0_D7vert=val(Substr(c0_D7from_at,0,1))
 dim as integer c0_D7horiz=val(Substr(c0_D7from_at,1,1))

 dim as integer c0_Dj=0
 while(c0_Dj<c0_Dcntx)

   c0_D7vert=c0_D7vert+c0_Dvert_TX
   c0_D7horiz=c0_D7horiz+c0_Dhoriz_TX
   if(c0_D7vert>=1 and c0_D7vert<=8 and c0_D7horiz>=1 and c0_D7horiz<=8) then

	dim as string c0_D7to_at=str(c0_D7vert) & str(c0_D7horiz)

	if( c0_can_be_moved( c0_D7from_at, c0_D7to_at, c0_false ) ) then

		c0_foundmove  = c0_convE777( c0_D7from_at ) & c0_convE777( c0_D7to_at )
		c0_D7poss &= c0_foundmove & ","
	end if

	c0_wKingmoved = saveD1wKingmoved
	c0_bKingmoved = saveD1bKingmoved
	c0_wLRockmoved = saveD1wLRockmoved
	c0_wRRockmoved = saveD1wRRockmoved
	c0_bLRockmoved = saveD1bLRockmoved
	c0_bRRockmoved = saveD1bRRockmoved
	c0_w00 = saveD1w00
	c0_b00 = saveD1b00

	c0_lastmovepawn = saveD1lastmovepawn
	c0_position = saveD1position
	c0_sidemoves = saveD1sidemoves
	c0_become = saveD1become

   end if

   c0_Dj+=1
  wend

 return c0_D7poss

''
end function
''



''
''  Function......... : c0_get_next_moves
''  Description...... : Function to get a string-list of next possible moves
''
''
function c0_get_next_moves() as string
''

 dim as string c0_Dposs=""
 dim as integer c0_Da=0
 while( len(c0_position )>c0_Da)

	dim as string c0_Dcolor=Substr( c0_position,c0_Da,1)
	if((c0_sidemoves>0 and c0_Dcolor="w") or (c0_sidemoves<0 and c0_Dcolor="b")) then

		dim as string c0_Dfigure=Substr( c0_position,c0_Da+1,1)
		dim as integer c0_Dhoriz=byteAt(Substr( c0_position,c0_Da+2,1),0) - 96
		dim as integer c0_Dvert=val(Substr( c0_position,c0_Da+3,1))
		dim as string c0_Dfrom_move=str(c0_Dvert) & str(c0_Dhoriz)

		if(c0_Dfigure="p") then

			 c0_Dposs &= c0_DCN(c0_Dfrom_move,c0_sidemoves,0,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,(2*c0_sidemoves),0,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,c0_sidemoves,1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,c0_sidemoves,-1,1)
		end if
		if(c0_Dfigure="N") then

			 c0_Dposs &= c0_DCN(c0_Dfrom_move,2,1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,2,-1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,1,2,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,1,-2,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-1,2,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-1,-2,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-2,1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-2,-1,1)
		end if
		if(c0_Dfigure="B" or c0_Dfigure="Q") then

			 c0_Dposs &= c0_DCN(c0_Dfrom_move,1,1,8)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,1,-1,8)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-1,1,8)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-1,-1,8)
		end if
		if(c0_Dfigure="R" or c0_Dfigure="Q") then

			 c0_Dposs &= c0_DCN(c0_Dfrom_move,1,0,8)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-1,0,8)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,0,1,8)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,0,-1,8)
		end if
		if(c0_Dfigure="K") then

			 c0_Dposs &= c0_DCN(c0_Dfrom_move,1,1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,1,0,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,1,-1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,0,1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,0,-1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-1,1,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-1,0,1)
			 c0_Dposs &= c0_DCN(c0_Dfrom_move,-1,-1,1)
			 if((c0_Dcolor="w" and c0_Dfrom_move="15") or _
                (c0_Dcolor="b" and c0_Dfrom_move="85")) then

				 c0_Dposs &= c0_DCN(c0_Dfrom_move,0,-2,1)
				 c0_Dposs &= c0_DCN(c0_Dfrom_move,0,2,1)
			 end if
		end if
	end if
   c0_Da +=5
 wend
 return c0_Dposs

''
end function
''



''
''  Function......... : c0_get_FEN
''  Description...... : Gets FEN for current chess position
''
function c0_get_FEN as string
''

dim as integer c0_vert7=8
dim as integer c0_horz7=1
dim as string c0_fs1=""
dim as integer c0_em7=0
dim as integer c0_at7=0

c0_vert7=8
while(c0_vert7>=1)

    c0_horz7=1
    while(c0_horz7<=8)

        dim as string c0_pos7 = chr(96+c0_horz7) & str(c0_vert7)
        c0_at7=IndexOf( c0_position, c0_pos7 )
        if( c0_at7>=0 ) then

            if( c0_em7>0 ) then
                c0_fs1 &= str(c0_em7)
                c0_em7=0
            end if
            dim as string c0_ch7=Substr( c0_position, c0_at7-1, 1 )
            dim as string c0_color7=Substr( c0_position, c0_at7-2, 1 )
            if( c0_color7="w" ) then
                c0_fs1 &= UCase( c0_ch7 )
            else
                c0_fs1 &= LCase( c0_ch7 )
            end if
        else
            c0_em7+=1
        end if
        c0_horz7+=1
    wend

 if( c0_em7>0 ) then
    c0_fs1 &= str(c0_em7)
    c0_em7=0
 end if

 c0_vert7-=1
 if(c0_vert7<1) then
	exit while
 end if

 c0_fs1 &= "/"

wend

dim as string c0_col8="w"
if(c0_sidemoves<0) then
	c0_col8="b"
end if

c0_fs1 &= " " & c0_col8 & " "

if(  (c0_w00 or c0_wKingmoved or (c0_wLRockmoved and c0_wRRockmoved ))  and _
     (c0_b00 or c0_bKingmoved or (c0_bLRockmoved and c0_bRRockmoved )) ) then
        c0_fs1 &= "- "
else

  if( c0_not(c0_w00 or c0_wKingmoved ) and c0_not( c0_wLRockmoved )) then
    c0_fs1 &= "Q"
  end if
  if( c0_not(c0_w00 or c0_wKingmoved ) and c0_not( c0_wRRockmoved )) then
    c0_fs1 &= "K"
  end if
  if( c0_not(c0_b00 or c0_bKingmoved ) and c0_not( c0_bLRockmoved )) then
    c0_fs1 &= "q"
  end if
  if( c0_not(c0_b00 or c0_bKingmoved ) and c0_not( c0_bRRockmoved )) then
    c0_fs1 &= "k"
  end if
  c0_fs1 &= " "

end if

 dim as string c0_enpass7="-"

 if(c0_lastmovepawn>0) then

	dim as string c0_lmove7=Substr( c0_moveslist, len(c0_moveslist )-4, 4 )
	c0_vert7 = byteAt(c0_lmove7,1)

	if( Substr(c0_lmove7,0,1)=Substr(c0_lmove7,2,1) and _
		(byteAt(c0_lmove7,0)-96=c0_lastmovepawn ) and _
		 (( Substr(c0_lmove7,1,1)="7" and Substr(c0_lmove7,3,1)="5" ) or _
		  ( Substr(c0_lmove7,1,1)="2" and Substr(c0_lmove7,3,1)="4" )) ) then

	 c0_at7=IndexOf( c0_position, Substr(c0_lmove7, 2,2) )
	 if( c0_at7>=0 and Substr( c0_position, c0_at7-1,1 )="p" ) then

		c0_enpass7=Substr(c0_lmove7,0,1)
		if( Substr(c0_lmove7,1,1)="7" ) then
            c0_enpass7 &= "6"
        else
            c0_enpass7 &= "3"
        end if
	 end if
	end if
end if
c0_fs1 &= c0_enpass7 & " "

c0_fs1 &= "0 "		'' position repeating moves....

dim as integer c0_mcount7=2
dim as integer c0_i7=0
while( c0_i7<len(c0_moveslist ) )

	c0_i7+=4
	if((len(c0_moveslist )>c0_i7) and (Substr(c0_moveslist ,c0_i7,1)="[")) then
        c0_i7+=3
    end if
	c0_mcount7+=1
wend
c0_fs1 &= str( int(c0_mcount7/2)) & " "

return c0_fs1

''
end function
''



''
''  Function......... : c0_take_back
''  Description...... : Take back 1 move
''
sub c0_take_back
''

dim as string c0_movespre=""
if( len(c0_moveslist)>0 ) then

 if (Substr( c0_moveslist, len(c0_moveslist)-1, 1 )="]" ) then
    c0_movespre= Substr(c0_moveslist, 0, len(c0_moveslist)-7 )
 else
    c0_movespre= Substr(c0_moveslist, 0, len(c0_moveslist)-4 )
 end if
end if

c0_set_start_position( c0_movespre )

''
end sub
''

''
''  Function......... : a_SAMPLES
''  Description...... : Samples for this chess logic code
''
sub a_SAMPLES
''

    print("============================================")
    print("===== Routine a_SAMPLES of chess logic =====")
    print("============================================")


	c0_side =1					'' This side is white.   For black set -1
	c0_set_start_position("")		    '' Set the initial position...

    '' 1. Test for basic chess functions (ok)
    print( "Setting up the starting position" )
	c0_set_start_position("")
    print( c0_position )
    print( "FEN function : " & c0_get_FEN() )

	'' Make a first move e4...
	c0_move_to("e2","e4")
	print( "Position after e4:")
    print( c0_position )

	'' Show the last move made...
	print ( "Last move:" & c0_D_last_move_was() )

    '' switch sides
    c0_sidemoves = -c0_sidemoves
    print ( "All movements till now:" & c0_moveslist )

    '' To see possible movements...
	print( "Now possible moves:")
    print( c0_get_next_moves() )

    '' And take it back...
	c0_take_back()
	print ( "Position after takeback" )
    print( c0_position )
    c0_sidemoves = -c0_sidemoves

	''Other functions:
	''Is e2-e4 a legal move in current position...
	print( "Can move a2-a4? ")
    print( c0_D_can_be_moved("a2","a4") )

	''Is there stalemate to the white king right now? ("b"/"w"- the parameter)
    ''c0_set_FEN ("7K/5q2/7k/8/8/8/8/8 w - - 0 70")
	print( "White king in stalemate? ")
    print( c0_D_is_stalemate_to_king() )

	''Is there check to the white king right now?
	print( "Check to white king?")
    print( c0_D_is_check_to_king() )

	''Is there checkmate to the white king right now?
	print( "Is checkmate to white king?")
    print( c0_D_is_mate_to_king() )

	'' What a piece on the square g7?
	print( "Piece on g7:" & c0_D_what_at("g7") )

	'' Is the square g6 empty? (no piece on it)
	print( "Is empty square g6? ")
    print( c0_D_is_empty("g6") )

    '' FEN position setup test
    '' c0_start_FEN="7k/Q7/2P2K2/8/8/8/8/8 w - - 0 70"
    c0_set_start_position("")
    c0_set_FEN ("7k/Q7/2P2K2/8/8/8/8/8 w - - 0 70")

    print( "Position after setup FEN=7k/Q7/2P2K2/8/8/8/8/8 w - - 0 70")
    print( c0_position )

    c0_set_start_position("")

    '' 2.PGN functions test (ok):
    print("PGN -> moves")
	dim as string PGN0="1.d4 d5 2.c4 e6 {comment goes here} 3.Nf3 Nf6 4.g3 Be7 (4.h4 or variant) 5.Bg2 0-0 6.0-0 dxc4 7.Qc2 a6 8.Qxc4 b5 9.Qc2 Bb7 10.Bd2 Be4 11.Qc1 Bb7 12.Qc2 Ra7 13.Rc1 Be4 14.Qb3 Bd5 15.Qe3 Nbd7 16.Ba5 Bd6 17.Nc3 Bb7 18.Ng5 Bxg2 19.Kxg2 Qa8+ 20.Qf3 Qxf3+ 21.Kxf3 e5 22.e3 Be7 23.Ne2 Re8 24.Kg2 Nd5 25.Nf3 Bd6 26.dxe5 Nxe5 27.Nxe5 Rxe5 28.Nd4 Ra8 29.Nc6 Re6 30.Rc2 Nb6 31.b3 Kf8 32.Rd1 Ke8 33.Nd4 Rf6 34.e4 Rg6 35.e5 Be7 36.Rxc7 Nd5 37.Rb7 Bd8 38.Nf5 Nf4+ 39.Kf3 Bxa5 40.gxf4 Bb4 41.Rdd7 Rc8 42.Rxf7 Rc3+ 43.Ke4 1-0"
	dim as string mlist0 = c0_get_moves_from_PGN(PGN0)
    print (mlist0)

    print("moves -> PGN (reverse action)")
	dim as string PGN1=c0_put_to_PGN(mlist0)
    print (PGN1)

	''3.Fischerrandom support test (ok):
    print("Fischer-random  PGN -> moves")
	dim as string PGN3="[White Aronian, Levon][Black Rosa, Mike][Result 0:1][SetUp 1][FEN bbrkqnrn/pppppppp/8/8/8/8/PPPPPPPP/BBRKQNRN w GCgc - 0 0] 1. c4 e5 2. Nhg3 Nhg6 3. b3 f6 4. e3 b6 5. Qe2 Ne6 6. Qh5 Rh8 7. Nf5 Ne7 8. Qxe8+ Kxe8 9. N1g3 h5 10. Nxe7 Kxe7 11. d4 d6 12. h4 Kf7 13. d5 Nf8 14. f4 c6 15. fxe5 dxe5 16. e4 Bd6 17. Bd3 Ng6 18. O-O Nxh4 19. Be2 Ng6 20. Nf5 Bc5+ 21. Kh2 Nf4 22. Rc2 cxd5 23. exd5 h4 24. Bg4 Rce8 25. Bb2 g6 26. Nd4 exd4 27.Rxf4 Bd6 0-1"
	dim as string mlist3= c0_get_moves_from_PGN(PGN3)
    print (mlist3)

    print("moves -> PGN (reverse action)")
    dim as string PGN4=c0_put_to_PGN(mlist3)
    print (PGN4)

    '' clear it all
    c0_start_FEN = ""
    c0_set_start_position("")
    print ("Starting position...")

    print ("Welcome, have a nice day!")
    sleep

''
end sub
''

'' Call samples...
'a_SAMPLES ()
