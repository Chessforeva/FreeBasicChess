'' This declares interface to ExeHandler

dim shared add_process as function _
    (byval eName as String, byval eArgs as String) as integer
dim shared get_status as function _
    (byval eId as integer) as integer
dim shared get_stdout as function _
    (byval eId as integer, byval buffer as String) as integer
dim shared put_stdin as function _
    (byval eId as integer, byval dataStr as String) as integer
dim shared kill_process as function _
    (byval eId as integer) as integer
dim shared release_all as function () as integer

Dim dll_hndl As Any Ptr
dll_hndl=DyLibLoad("ExeHandler.dll")

'' find the addresses for procedures (case letters matters!)
add_process = DyLibSymbol( dll_hndl, "add_process" )
get_status = DyLibSymbol( dll_hndl, "get_status" )
get_stdout = DyLibSymbol( dll_hndl, "get_stdout" )
put_stdin = DyLibSymbol( dll_hndl, "put_stdin" )
kill_process = DyLibSymbol( dll_hndl, "kill_process" )
release_all = DyLibSymbol( dll_hndl, "release_all" )

'' Chess interface

' Crafty public variables
Dim shared CraftyID As integer = 0
Dim shared CraftyMove As string
Dim shared Crafty3reps As integer   ' detects 3 repetitions of position
Dim shared as integer Crafty_search_depth = 0, Crafty_search_time = 0

' Naum public variables
Dim shared NaumID As integer = 0
Dim shared NaumMove As string
Dim shared as integer Naum_search_depth = 0, Naum_search_time = 0

' Data buffer
Dim shared DataBuffer As string
DataBuffer = SPACE(64*1024)
Dim shared syX As string
Dim shared syI As integer

CraftyMove = ""

' chess board that comes from chess engines (not used)
Dim shared ChessBoard(8,8) As string
Dim shared ChessBoReady As integer = 0

' stores history of chess moves
Dim shared as string moveHist(200)
Dim shared as integer mvCnt = 0

sub StartNaum
    if( not( NaumID>0) ) then
        NaumID = add_process( exepath( ) & "\naum\naum.exe","")
        if(NaumID>0) then
            put_stdin(NaumID, _
                "board" + chr(10)+ _
                "xboard" + chr(10) )
        end if
    end if
end sub

sub StartCrafty
    if( not( CraftyID>0) ) then
        CraftyID = add_process( exepath( ) & "\crafty\crafty-22.0-win32.exe","")
        if(CraftyID>0) then
            put_stdin(CraftyID, _
                "log off"+chr(10) + _
                "display"+chr(10) + _
                "sd 4"+chr(10) + _
                "xboard"+chr(10) )
        end if
    end if
end sub

sub CloseCrafty
    if(CraftyID>0) then
        kill_process(CraftyID)
    end if
end sub

sub CloseNaum
    if(NaumID>0) then
        kill_process(NaumID)
    end if
end sub

sub CraftyGo
    if(CraftyID>0) then
        put_stdin(CraftyID,"go"+chr(10) + "display"+chr(10))
    end if
end sub
sub CraftyUciMove( mv as string)
    if(CraftyID>0) then
        CraftyMove = ""
        put_stdin(CraftyID,mv + chr(10)+ "display"+chr(10))
    end if
end sub
sub CraftyNewGame
    if(CraftyID>0) then
        put_stdin(CraftyID,"new"+chr(10) + "display"+chr(10))
    end if
end sub
sub CraftyUndo
    if(CraftyID>0) then
        put_stdin(CraftyID,"undo"+chr(10) + "display"+chr(10))
    end if
end sub
sub CraftySetStrength( sd as integer, st as integer )
    dim as string s = ""
    if(sd>0) then
        s = "sd " & str(sd)
        Crafty_search_depth = sd
    end if
    if(st>0) then
        s = "st " & str(st)
        Crafty_search_time = st
    end if  
    if(CraftyID>0) then
        put_stdin(CraftyID,s + chr(10) + "display"+chr(10))
    end if
end sub



sub NaumGo
    if(NaumID>0) then
        put_stdin(NaumID,"go"+chr(10)+ "board"+chr(10))
    end if
end sub
sub NaumUciMove( mv as string)
    if(NaumID>0) then
        NaumMove = ""
        put_stdin(NaumID,mv + chr(10) + "board"+chr(10))
    end if
end sub
sub NaumNewGame
    if(NaumID>0) then
        put_stdin(NaumID,"new"+chr(10)+ "board"+chr(10))
    end if
end sub
sub NaumUndo
    if(NaumID>0) then
        put_stdin(NaumID,"undo"+chr(10)+ "board"+chr(10))
    end if
end sub
sub NaumSetStrength( sd as integer, st as integer )
    dim as string s = ""
    if(sd>0) then
        s = "sd " & str(sd)
        Naum_search_depth = sd
    end if
    if(st>0) then
        s = "st " & str(st)
        Naum_search_time = st
    end if  
    if(NaumID>0) then
        put_stdin(NaumID,s + chr(10) + "display"+chr(10))
    end if
end sub


function LineOnly as String
    dim as String c, s = ""
    'read string till \r\n   
    while ( syI>0 and c<>chr(10) and c<>chr(13) and c<>chr(0) )
        s += c  'read string
        c = Left(syX,1)
        syX = Mid(syX,2)
        syI -= 1
    wend
    'skip \r\n
    c = Left(syX,1)
    while ( syI>0 and (c=chr(10) or c=chr(13) or c=chr(0)) )
        syX = Mid(syX,2)
        c = Left(syX,1)
        syI -= 1
    wend
    return s
end function

Sub CraftyReadBuffer
    dim as String w, pc,pp = ""
    dim as integer a, h, v
    if(CraftyID>0) then
        syI = get_stdout(CraftyID,DataBuffer)
        if(syI>0) then
            syX = Left(DataBuffer,syI)
            do
                w = LineOnly
                a = InStr(w,"move ")
                if( a>0 ) then
                    CraftyMove = trim( mid(w,a+5) )
                end if
                a = InStr(w,"1/2-1/2 {Drawn by 3-fold repetition}")
                if( a>0 ) then
                    Crafty3reps = 1
                end if                
                if(mid(w,8,1)="|") then
                    v = val( mid(w,5,1) )
                    if(v>0) then
                        for h = 1 to 8
                            a = 9 + ((h-1)*4)
                            pc = mid(w, a, 1)
                            pp = mid(w, a+1, 1)
                            select case( pc )
                            case "<"
                                pp = LCase(pp)
                            case " "
                                pp = " "
                            end select
                            ChessBoard(v,h) = pp
                            ChessBoReady += 1
                        next
                    end if
                end if
            Loop until (syI=0)
        end if
    end if
end sub

dim shared as integer NaumBoV = 8

Sub NaumReadBuffer
    dim as String w, pp = ""
    dim as integer a, h
    if(NaumID>0) then
        syI = get_stdout(NaumID,DataBuffer)
        if(syI>0) then
            syX = Left(DataBuffer,syI)
            h = 1
            do
                w = LineOnly
                if(Left(w,5)="move ") then
                    NaumMove = trim( mid(w,6) )
                end if
                a = 1
                for h = 1 to 8
                    if( Instr("pnbrqkPNBRQK.",mid(w,(h*2)-1,1))<=0 ) then
                        if( h<8 and mid(w,(h*2),1)<>" " ) then
                            a = 0
                        end if
                    end if
                next
                if(a>0) then
                    for h = 1 to 8
                        pp = mid(w,(h*2)-1,1)
                        if pp="." then pp = " "
                        ChessBoard(NaumBoV,h) = pp
                        ChessBoReady += 1
                    next
                    if NaumBoV = 1 then
                        NaumBoV = 8
                    else
                        NaumBoV -= 1
                    end if
                end if
            Loop until (syI=0)
        end if
    end if
end sub

