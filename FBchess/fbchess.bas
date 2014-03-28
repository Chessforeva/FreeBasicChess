'-----------------------------------
'  FREEbasic chess project based on
'  Crafty and Naum chess engines
'-----------------------------------

' jpg-drawing
#include once "image.bas"

' drawable letters
#include once "letters.bas"

' chess logic
#include once "c0_chess.bas"
#include once "chessdll.bas"

' music in background, fmod.dll required
#include once "music.bas"

dim shared as Integer mouseX, mouseY, mouseButtons, mouseRes
dim shared as byte fNew = 0, fMusic = 0, fTakeBack = 0


dim shared as String key
dim as string sMsg, sMsg0     'status Check+,Checkmate#,Stalemate on screen
sMsg0 = space(60) + chr(13) + space(60)

dim shared as integer GameOver = 0
dim shared as integer IS_KEYBOARD = 0
dim shared as integer keyb_V = 2, keyb_H = 5

'-----------------------------
const SCR_W = (70*8)+200
const SCR_H = (70*8)+2
const SCR_BPP = 32

const GAME_WITH_CRAFTY = 1
const GAME_WITH_NAUM = 2
const GAME_CRAFTY_VS_NAUM = 3

dim shared as integer GameMode
GameMode = GAME_WITH_CRAFTY ' default

'most graphics board functions
#include once "GUI.bas"
  
WindowTitle "FreeBASIC chess with Crafty and Naum"

screenres SCR_W, SCR_H, SCR_BPP

LoadImagesOfLetters 'prepare images


dim shared as string dragMOVE
dragMOVE = ""

' players 
const ItsME = 0
const NAUM = 1
const CRAFTY = 2

dim shared as integer White_Player, Black_Player
White_Player = ItsME
Black_Player = CRAFTY

sub GoChessEngine
    
    if( GameOver<1 ) then
    
        dim move as string = dragMOVE
        if( len(dragMOVE)>0 and len(c0_become)>0 _
                and Instr("NBRQnbrq",c0_become)>0 ) then
            move &= LCase( c0_become )
        end if
        if(((c0_sidemoves>0) and (White_Player = NAUM)) or _
            ((c0_sidemoves<0) and (Black_Player = NAUM))) then
            if( len(move)>0 ) then
                NaumUciMove( move )
                'shell("echo.To:Naum." + move)
            else
                NaumGo
            end if

        end if
        if(((c0_sidemoves>0) and (White_Player = CRAFTY)) or _
            ((c0_sidemoves<0) and (Black_Player = CRAFTY))) then
            if( len(move)>0 ) then  
                CraftyUciMove( move )
                'shell("echo.To:Crafty." + move)
            else
                CraftyGo
            end if
        end if
        if( GameMode = GAME_CRAFTY_VS_NAUM ) then
            sleep 500
        end if
    end if
end sub


sub PutPiece(V as integer, H as integer, p as string)
    dim x as integer = posX(H)
    dim y as integer = posY(V)
    dim as string c, bg, f
    if( ((H+V) MOD 2)>0 ) then bg = "w" else bg = "b"
    if( len(p)=0 ) then
        f = ""
    else
        f = left(p,1) & UCase( mid(p,2,1) )
    end if
    PutPieceImage( f & bg, x, y )
end sub

sub printBoard
    dim as integer V,H
    for V=1 to 8
        for H=1 to 8
            dim as string at_sq = c0_convE2( V, H )
            dim as string piece = c0_D_what_at( at_sq )
            PutPiece( V, H, piece )
                ' current cursor position
            if(len(dragMOVE)=2 and at_sq=dragMOVE) then
                PutCursor( V, H, 1 )
            end if
                ' last move
            dim as string lastm = "x"
            if(mvCnt>0) then
                lastm = mid( moveHist,3+((mvCnt-1)*5), 2 )
            end if
            if(mvCnt>0 and at_sq = lastm ) then
                PutCursor( V, H, 2 )
            end if
            
            if(IS_KEYBOARD and keyb_V = V and keyb_H = H) then
                PutCursor( V, H, 3 )
            end if
        next
    next
end sub

'Clicked on known square
sub dragClick(V as integer, H as integer)
    dim as string sq =  c0_convE2( V, H )
    
    if(len(dragMOVE)<4) then    'otherwise doing nothing
    
    if( len(dragMOVE)=2 ) then
        c0_become_from_engine = ""
        if( c0_D_can_be_moved(dragMOVE,sq) ) then
            if( mid( c0_D_what_at(dragMOVE), 2, 1 )="p" and (V=1 or V=8) ) then
                c0_become_from_engine = whichPromote( c0_CurColor() )
            end if
            dragMOVE &= sq
        end if
    end if
    
    if( len(dragMOVE)=4 ) then
        c0_move_to ( mid( dragMOVE, 1, 2 ), mid( dragMOVE, 3, 2 ) )
        mvCnt += 1
        moveHist += dragMOVE
        if( len(c0_become_from_engine)>0 ) then
            moveHist += c0_become_from_engine
        else
            moveHist += " "
        end if
        GoChessEngine
    else
        if( Instr( "," & c0_get_next_moves(), ("," & sq) )>0 ) then
            dragMOVE = sq 
        end if
    end if
    printBoard
            
    end if
end sub

sub mouseClickOnBoard
    dim as integer V,H,x0,y0
    if( GameOver<1 and _
                ((c0_sidemoves>0 and White_Player=ItsME) or _
                 (c0_sidemoves<0 and Black_Player=ItsME)) ) then
        for V=1 to 8
            for H=1 to 8
                x0 = posX(H)
                y0 = posY(V)
                if((mouseX>=x0) and (mouseX<x0+70) and _
                    (mouseY>=y0) and (mouseY<y0+70)) then
                    dragClick( V, H )
                end if
            next
        next
    end if
end sub

sub KeybCursor( dV as integer, dH as integer )
    if(IS_KEYBOARD) then
        keyb_V += dV * c0_side
        keyb_H += dH * c0_side
        if keyb_V < 1 then keyb_V = 1
        if keyb_H < 1 then keyb_H = 1
        if keyb_V > 8 then keyb_V = 8
        if keyb_H > 8 then keyb_H = 8  
        if( dV = 0 and dH = 0 ) then    ' 0,0 means Clicked
            dragClick( keyb_V, keyb_H )
        end if
    else
        IS_KEYBOARD = 1     'This sets keyboard cursor on
    end if
end sub

sub DispWhoIs
    dim as string s_ItsMe = "IT'S ME"
    dim as string s_Naum = "NAUM"
    dim as string s_Crafty = "CRAFTY"
    
    if(White_Player=ItsME) then DisplayName(1,s_ItsMe)
    if(White_Player=NAUM) then DisplayName(1,s_Naum)
    if(White_Player=CRAFTY) then DisplayName(1,s_Crafty)
    
    if(Black_Player=ItsME) then DisplayName(8,s_ItsMe)
    if(Black_Player=NAUM) then DisplayName(8,s_Naum)
    if(Black_Player=CRAFTY) then DisplayName(8,s_Crafty) 
end sub

sub AdjustGameMode
    dim as integer Other
    dim as integer Me = ItsME
    
    showNewGameWindow
    
    select case(GameMode)
    case GAME_WITH_CRAFTY
        Other = CRAFTY
    case GAME_WITH_NAUM
        Other = NAUM   
    case GAME_CRAFTY_VS_NAUM
        Other = NAUM
        Me = CRAFTY
    end select
    if ((White_Player = ItsME) or (Black_Player<>ItsME)) then
        White_Player = Me
        Black_Player = Other
    else
        Black_Player = Me
        White_Player = Other
    end if
    
    if((c0_side>0 and Black_Player = ItsME) or _
        (c0_side<0 and White_Player = ItsME) or _
        ((Black_Player<>ItsME) and (White_Player<>ItsME)) ) then
        dim as integer swapz = White_Player     'swap players if mistake
        White_Player = Black_Player
        Black_Player = swapz
    end if
        
end sub

GameOver = 0
c0_side =1					'' This side is white.   For black set -1
c0_set_start_position("")		    '' Set the initial position...
printBoard

'start both chess engines
StartNaum
StartCrafty

AdjustGameMode
printBoard

DrawInitialButtons
DispWhoIs
GoChessEngine
        
'The main loop
Do
    sleep 10    ' Let the CPU do other things
    key = Inkey$
    
    if (len(key)>0 ) then
        if(key = CHR(255) & "H") then   'Up
            KeybCursor( +1, 0 )
        end if
        if(key = CHR(255) & "K") then   'Left
            KeybCursor( 0, -1 )
        end if
        if(key = CHR(255) & "M") then   'Right
            KeybCursor( 0, +1 )
        end if
        if(key = CHR(255) & "P") then   'Down
            KeybCursor( -1, 0 )
        end if
        if( ASC(key) = 32 or ASC(key) = 13 ) then
            KeybCursor( 0, 0 )
        end if
        printBoard
        continue do
    end if    
    
    mouseRes  = GetMouse (mouseX, mouseY, , mouseButtons)
    if mouseRes = 0 then    '' otherwise no mouse available
        if mouseButtons And 1 then      'on left mouse button
            fNew = ifNewGameClicked
            if(ifMusicClicked) then
                fMusic = 1-fMusic
                DrawMusicButton
                if(fMusic<1) then
                    StopMusic
                end if
                sleep 500
            end if
            fTakeBack = ifTakeBackClicked
            if(ifAboutClicked) then
                showAboutWindow
                printBoard
            end if
            mouseClickOnBoard
        end if
    end if
       
    ' get incomming data from both chess engines
    NaumReadBuffer
    CraftyReadBuffer
    
    dim as string engineMoved = NaumMove & CraftyMove
    
    if( len(engineMoved)>0) then

        dim as string move = c0_from_Crafty_standard( engineMoved, c0_CurColor() )
        if len(move)>4 then
            c0_become_from_engine = UCase( mid(move,6,1) )
        else
            c0_become_from_engine = "Q"
        end if
        move = left(move,4)
        if Instr("NBR",c0_become_from_engine)>0 then
            move &= c0_become_from_engine
        end if       
        c0_move_to ( mid( move, 1, 2 ), mid( move, 3, 2 ) )
        mvCnt += 1
        moveHist += move
        if( len(move)<5 ) then
            moveHist += " "
        end if
        printBoard
        NaumMove = ""
        CraftyMove = ""
        dragMOVE = move
        GoChessEngine
        dragMOVE = ""
    end if

    sMsg = ""    
    if( c0_D_is_check_to_king() ) then
     if( c0_D_is_mate_to_king() ) then
        sMsg = "Checkmate#" + chr(13)
        if(c0_sidemoves>0) then sMsg+="0-1" else sMsg+="1-0"
        GameOver = 1
     else
        sMsg = "Check+"
     end if
    else
     if( c0_D_is_stalemate_to_king() ) then
        sMsg = "Stalemate! " + chr(13) + "1/2-1/2"
        GameOver = 1
     end if
    end if
    if( Crafty3reps ) then
        sMsg = "3-times" + chr(13) + "1/2-1/2"
        GameOver = 1
        Crafty3reps = 0
    end if

    if(GameOver<1) then
        PutLetterString( 580, 40, sMsg0 )   'clear
    end if
    PutLetterString( 580, 40, sMsg )    'display status of the game
    
    if(IsMusicOver and fMusic) then
        PlayMusic
    end if
    
    if (fNew and (GameOver or len(dragMOVE)<4)) then
        fNew = 0
        dragMOVE = ""
        NaumMove = ""
        mvCnt = 0
        moveHist = ""
        keyb_V = 9-keyb_V
        keyb_H = 9-keyb_H
        CraftyMove = ""
        GameOver = 0
        c0_side = -c0_side		    '' Swaps sides
        c0_set_start_position("")	'' Set the initial position...
        printBoard
        CraftyNewGame
        NaumNewGame
        dim as integer swapz = White_Player     'swap players
        White_Player = Black_Player
        Black_Player = swapz
        AdjustGameMode
        DispWhoIs
        GoChessEngine
        sleep 500
    end if
    
    if(fTakeBack and (GameOver or len(dragMOVE)<4) and _
            ( White_Player=ItsME or Black_Player=ItsME)) then
        fTakeBack = 0
        GameOver = 0
        dragMOVE = ""
        NaumMove = ""
        CraftyMove = ""
        if( mvCnt > 0 ) then
            c0_take_back()
            mvCnt -= 1
            if mvCnt>0 then
                moveHist = mid(moveHist,1,mvCnt*5)
            else
                moveHist = ""
            end if
            CraftyUndo
            NaumUndo
            if( mvCnt > 0 and _
                ((c0_sidemoves>0 and White_Player<>ItsME) or _
                 (c0_sidemoves<0 and Black_Player<>ItsME)) ) then
                c0_take_back()
                mvCnt -= 1
                if mvCnt>0 then
                    moveHist = mid(moveHist,1,mvCnt*5)
                else
                    moveHist = ""
                end if
                CraftyUndo
                NaumUndo
            end if
            GoChessEngine
        end if
        printBoard
        sleep 500
    end if
    ''If mouseButtons And 2 then key = chr(27)

    '' till ESC or Close Window
Loop until ( GameMode=0 ) or (ASC(key) = 27) or (key = chr(255) + "k")  

' close both engines
CloseNaum
CloseCrafty

CloseMusic
        