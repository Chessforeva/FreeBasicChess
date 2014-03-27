
' this file contains most drawings

const BLACK = RGB(0,0,0)
const WHITE = RGB(255,255,255)
const DARK_RED = RGB(128,0,0)
const RED = RGB(255,0,0)

function EscEntSpcKeyClick( key as string ) as byte
    dim as byte r = 0
    if ( len(key)>0 and _
        ((ASC(key) = 27) or (ASC(key) = 13) or (ASC(key) = 32) or _
            (key = chr(255) + "k")) ) then
        r = 1
    end if
    return r
end function

function posX(H as integer) as integer  'position X by horiz.a-h
    dim as integer H0 = (H-1)
    if ( c0_side<0 ) then H0 = (7-H0)
    return 1+ (70 * H0)
end function
function posY(V as integer) as integer  'position Y by vert.1-8
    dim as integer V0 = (8-V)
    if ( c0_side<0 ) then V0 = (7-V0)
    return 1+ (70 * V0)
end function

'Buttons
const NEW_GAME_N = 1
const TAKE_BACK_N = 2
const MUSIC_N = 3
const ABOUT_N = 4
const OK_N = 5
const START_N = 6
const YES_N = 7

const CRAFTY_PLY2_N = 10
const CRAFTY_PLY4_N = 11
const CRAFTY_PLY6_N = 12
const CRAFTY_PLY8_N = 13
const CRAFTY_SEC1_N = 14
const CRAFTY_SEC3_N = 15

const NAUM_PLY2_N = 20
const NAUM_PLY4_N = 21
const NAUM_PLY6_N = 22
const NAUM_PLY8_N = 23
const NAUM_SEC1_N = 24
const NAUM_SEC3_N = 25

const AUTOGAME_N = 30

const PROMO_Q_N = 40
const PROMO_R_N = 41
const PROMO_B_N = 42
const PROMO_N_N = 43

dim shared as integer ButtXY(50,4)   ' holds (x0y0)-(x2y2) of buttons

sub Set0Hlpr            ' set 0 to ButtXY
    dim as integer n,i
    for n=1 to 50
        for i=1 to 4
            ButtXY(n,i)=0
        next
    next
end sub

function roundCornHlpr( _       'helper function for rounding corners
        x0 as integer, y0 as integer, _
        dx as integer, dy as integer, _
        xx as integer, yy as integer ) as byte
    dim as integer x1 = x0+dx, y1 = y0+dy, r = dx^2
    if( ((dx>0) and (xx>x1)) or ((dx<0) and (xx<x1)) or _
        ((dy>0) and (yy>y1)) or ((dy<0) and (yy<y1)) ) then
        return 1
    end if
    return ( r>( ((x1-xx)^2) + ((y1-yy)^2) ) )
    return 0
end function

sub DrawButton(x as integer, y as integer, s as string, N as integer)
    dim as integer x0=x, y0=y, xi, yi
    dim as UInteger c
    ButtXY( N, 1 ) = x
    ButtXY( N, 2 ) = y
    x+=8
    y+=6
    PutLetterString( x, y, s )
    for xi=x0 to x+8
        for yi=y0 to y+6
            c = Point(xi,yi)
            if( c=BLACK ) then
                select case(N)
                case NEW_GAME_N, OK_N, START_N
                    c=RGB(62,206-((yi-y0)*3),69)
                case TAKE_BACK_N
                    c=RGB(119,83,225-((yi-y0)*3))
                case MUSIC_N
                    c=RGB(235-((yi-y0)*3),82,97)
                case ABOUT_N
                    c=RGB(223,236-((yi-y0)*3),47)
                case CRAFTY_PLY2_N to CRAFTY_SEC3_N
                    c=RGB(62,200-((N-CRAFTY_PLY2_N)*20),200-((yi-y0)*3))
                case NAUM_PLY2_N to NAUM_SEC3_N
                    c=RGB(253-((N-NAUM_PLY2_N)*20),103,200-((yi-y0)*3))
                end select
            else
                c=WHITE
            end if
            if( roundCornHlpr( x0,y0, 4, 4, xi,yi) and _
                roundCornHlpr( x+8,y0, -4, 4, xi,yi) and _
                roundCornHlpr( x0,y+6, 4, -4, xi,yi) and _
                roundCornHlpr( x+8,y+6, -4, -4, xi,yi) ) then
                Pset(xi,yi),c
            end if
        next
    next
    ButtXY( N, 3 ) = x+8
    ButtXY( N, 4 ) = y+6
end sub

'This puts cursor
sub PutCursor(V as integer, H as integer, ch as integer )
    dim x as integer = posX(H)
    dim y as integer = posY(V)
    dim as integer curCol = Color()
    select case(ch)
    case 1
        Color RGB(173, 31, 5)
    case 2
        Color RGB(5, 75, 180)
    case 3
        Color RGB(16, 86, 5)
    end select
    Line (x, y)-(x+70-1, y+70-1),, b
    Line (x+2, y+2)-(x+70-3, y+70-3),, b
    Paint (x+1, y+1)
    Color curCol
end sub

sub ClearPreButton( N as integer )
    dim as integer curCol = Color()
    Color BLACK
    Line (ButtXY( N, 1),ButtXY( N, 2))-(ButtXY( N, 3),ButtXY( N, 4)),,bf
    Color curCol
end sub

sub DrawMusicButton
    dim as string s
    if (fMusic) then s = "OFF" else s="ON"
    ClearPreButton(MUSIC_N)
    DrawButton( 580, 250, "  MUSIC " & s & " ", MUSIC_N )
end sub

Sub DrawInitialButtons
    
    DrawButton( 580, 150, " NEW GAME ", NEW_GAME_N )
    DrawButton( 580, 200, "TAKE BACK", TAKE_BACK_N )
    DrawMusicButton
    DrawButton( 580, 300, "    ABOUT    ", 4 )

end sub

function ifButtRect( N as integer ) as byte
    return ( mouseX>ButtXY(N,1) and mouseY>ButtXY(N,2) and _
            mouseX<ButtXY(N,3) and mouseY<ButtXY(N,4) )
end function

function ifNewGameClicked as byte
    return ifButtRect( NEW_GAME_N )
end function

function ifMusicClicked as byte
    return ifButtRect( MUSIC_N )
end function

function ifTakeBackClicked as byte
    return ifButtRect( TAKE_BACK_N )
end function

function ifAboutClicked as byte
    return ifButtRect( ABOUT_N )
end function

function ifOKClicked as byte
    return ifButtRect( OK_N )
end function

function ifStartClicked as byte
    return ifButtRect( START_N )
end function

function PromoPieceClicked as byte
    dim as byte i, r = 0
    for i=PROMO_Q_N TO PROMO_N_N  'buttons to set promo-piece
        if( ifButtRect( i ) ) then
            r = i
        end if
    next
    return r
end function

function LimitsChosen as byte
    dim as byte i, r = 0
    for i=10 TO 29  'buttons to set limits for chess engines
        if( ifButtRect( i ) ) then
            r = i
        end if
    next
    return r
end function

sub DisplayName( Vn as integer, msg as string )
    dim as integer x = 570, y= posy(Vn)+20, xi, yi
    dim as integer x0 = x, y0 = y
    dim as UInteger c
    PutLetterString( x, y, msg + SPACE(60) )
    for xi=x0 to x
        for yi=y0 to y
            c = Point(xi,yi)
            if( c<>BLACK ) then
                c=RGB(70,255-((yi-y0)*4),163)
                Pset(xi,yi),c
            end if
        next
    next
end sub

const BOX_x1 = 80
const BOX_y1 = (SCR_H/2) - 210
const BOX_x2 = BOX_x1 + 400
const BOX_y2 = BOX_y1 + 420

sub DrawBox( Ch as integer )
    dim as integer curCol = Color()
    dim as integer H = 0
    if (Ch=1) then H=60
    if (Ch=2) then H=100
    Color BLACK
    Line ( BOX_x1, BOX_y1+H )-( BOX_x2, BOX_y2-H ),,bf
    Color DARK_RED
    Line ( BOX_x1, BOX_y1+H )-( BOX_x2, BOX_y2-H ),,b
    Color curCol
end sub

sub PutPieceImage( f2 as string, x as integer, y as integer )
    dim as FB.IMAGE ptr imgPc
	imgPc = imageread_jpg(  exepath( ) & "\images\" & f2 & ".jpg", SCR_BPP )
    put (x,y), imgPc, pset
    imagedestroy( imgPc ) 
end sub

sub showAboutWindow
    DrawBox(1)
    PutLetterString( BOX_x1 + 20, BOX_y1 + 80, _
        "THIS IS FREEBASIC CHESS" + CHR(10) + _
        "PROJECT BY CHESSFOREVA." + CHR(10) + _
        "FREE SOURCE, 2014" + CHR(10) + CHR(10) + _
        "USES CHESS ENGINES" + CHR(10) + _
        "NAUM 2.0 AND CRAFTY 22.0" + CHR(10) + _
        "MADE BY EXPERTS." + CHR(10) + CHR(10) + _
        "FOR WEAK GAME LIMIT AI" + CHR(10) + _
        "CALC.DEPTH TO 2 MOVES." )
    DrawButton( BOX_x1 + 170, BOX_y1 + 300, "OK", OK_N )
    do
        sleep 50    ' Let the CPU do other things
        key = Inkey$
        mouseRes  = GetMouse (mouseX, mouseY, , mouseButtons)
        if mouseRes = 0 then    '' otherwise no mouse available
            if mouseButtons And 1 then      'on left mouse button
                if (ifOKClicked) then
                    key = chr(13)
                end if
            end if
        end if
        if(IsMusicOver and fMusic) then
            PlayMusic
        end if
    Loop until EscEntSpcKeyClick( key )
end sub

sub GameModeClicked
    if(mouseX>BOX_x1 and mouseX<BOX_x2) then
        if(mouseY>BOX_y1 and mouseY<BOX_y1+159) then
            GameMode = GAME_WITH_CRAFTY
        end if
        if(mouseY>BOX_y1+159 and mouseY<BOX_y1+303) then
            GameMode = GAME_WITH_NAUM
        end if
        if(mouseY>BOX_y1+303 and mouseY<BOX_y1+353) then
            GameMode = GAME_CRAFTY_VS_NAUM
        end if
    end if
end sub

sub DrawCursOnGame( ch as integer )
    dim as integer X,Y,X2,Y2
    dim as integer curCol = Color()
    
    select case(ch)
    case GAME_WITH_CRAFTY
            
        X = BOX_x1 + 10
        Y = BOX_y1 + 15
        X2 = X + 380
        Y2 = Y + 140
        
    case GAME_WITH_NAUM
        
        X = BOX_x1 + 10
        Y = BOX_y1 + 157
        X2 = X + 380
        Y2 = Y + 140
        
    case GAME_CRAFTY_VS_NAUM
        
        X = BOX_x1 + 10
        Y = BOX_y1 + 299
        X2 = X + 380
        Y2 = Y + 50
        
    end select
    
    if(GameMode=ch) then
        Color RGB(255,255,90)
    else
        Color BLACK
    end if

    Line (X, Y)-(X2, Y2),, b
    Line (X+3, Y+3)-(X2-4, Y2-4),, b
    Paint (X+1, Y+1)
 
    Color curCol

end sub

' set limits if user clicked on options
sub SetLimits
    select case( LimitsChosen() )
    case CRAFTY_PLY2_N
        CraftySetStrength(2,0)
    case CRAFTY_PLY4_N
        CraftySetStrength(4,0)
    case CRAFTY_PLY6_N
        CraftySetStrength(6,0)   
    case CRAFTY_PLY8_N
        CraftySetStrength(8,0)
    case CRAFTY_SEC1_N
        CraftySetStrength(0,1)
    case CRAFTY_SEC3_N
        CraftySetStrength(0,3)
    case NAUM_PLY2_N
        NaumSetStrength(2,0)
    case NAUM_PLY4_N
        NaumSetStrength(4,0)
    case NAUM_PLY6_N
        NaumSetStrength(6,0)   
    case NAUM_PLY8_N
        NaumSetStrength(8,0)
    case NAUM_SEC1_N
        NaumSetStrength(0,1)
    case NAUM_SEC3_N
        NaumSetStrength(0,3) 
    end select

end sub

'draws cursor on limits buttons
sub DrawButtonLine( N as integer, T as integer )
    dim as integer i
    dim as integer curCol = Color()

    if(T) then
        Color RED
    else
        Color BLACK
    end if
    
    Line (ButtXY( N, 1)-3, ButtXY( N, 2)-3)- _
            (ButtXY( N, 3)+3, ButtXY( N, 4)+3),,b
    Line (ButtXY( N, 1)-5, ButtXY( N, 2)-5)- _
            (ButtXY( N, 3)+5, ButtXY( N, 4)+5),,b
    Paint( ButtXY( N, 1)-4, ButtXY( N, 2)-4 )
    
    Color curCol
end sub

sub DrawLimits
    
    DrawButtonLine( CRAFTY_PLY2_N, Crafty_search_depth=2)
    DrawButtonLine( CRAFTY_PLY4_N, Crafty_search_depth=4)
    DrawButtonLine( CRAFTY_PLY6_N, Crafty_search_depth=6)
    DrawButtonLine( CRAFTY_PLY8_N, Crafty_search_depth=8)
    DrawButtonLine( CRAFTY_SEC1_N, Crafty_search_time=1)
    DrawButtonLine( CRAFTY_SEC3_N, Crafty_search_time=3)
    
    DrawButtonLine( NAUM_PLY2_N, Naum_search_depth=2)
    DrawButtonLine( NAUM_PLY4_N, Naum_search_depth=4)
    DrawButtonLine( NAUM_PLY6_N, Naum_search_depth=6)
    DrawButtonLine( NAUM_PLY8_N, Naum_search_depth=8)
    DrawButtonLine( NAUM_SEC1_N, Naum_search_time=1)
    DrawButtonLine( NAUM_SEC3_N, Naum_search_time=3)
    
end sub

sub showNewGameWindow
    
    dim as integer r = 0
    dim as string c=chr(10)

    DrawBox(0)    
    
    PutLetterString( BOX_x1 + 20, BOX_y1 + 30, _
        "PLAY WITH CRAFTY" & c & _
        "  LIMITS" & c & _
        "   CALC.MOVES   " & c & c & _
        "   SECONDS PER MOVE " & c & c & c & _
        "PLAY WITH NAUM  " & c & _
        "  LIMITS" & c & _
        "   CALC.MOVES   " & c & c & _
        "   SECONDS PER MOVE " & c & c & c & _
        "LET CRAFTY PLAY WITH NAUM")
    
    DrawButton( BOX_x1 + 213, BOX_y1 + 60, "2", CRAFTY_PLY2_N )
    DrawButton( BOX_x1 + 255, BOX_y1 + 60, "4", CRAFTY_PLY4_N )
    DrawButton( BOX_x1 + 297, BOX_y1 + 60, "6", CRAFTY_PLY6_N )
    DrawButton( BOX_x1 + 339, BOX_y1 + 60, "8", CRAFTY_PLY8_N )
    DrawButton( BOX_x1 + 292, BOX_y1 + 104, "1", CRAFTY_SEC1_N )
    DrawButton( BOX_x1 + 329, BOX_y1 + 104, "3", CRAFTY_SEC3_N )
    
    DrawButton( BOX_x1 + 213, BOX_y1 + 202, "2", NAUM_PLY2_N )
    DrawButton( BOX_x1 + 255, BOX_y1 + 202, "4", NAUM_PLY4_N )
    DrawButton( BOX_x1 + 297, BOX_y1 + 202, "6", NAUM_PLY6_N )
    DrawButton( BOX_x1 + 339, BOX_y1 + 202, "8", NAUM_PLY8_N )
    DrawButton( BOX_x1 + 292, BOX_y1 + 246, "1", NAUM_SEC1_N )
    DrawButton( BOX_x1 + 329, BOX_y1 + 246, "3", NAUM_SEC3_N )
    
    DrawButton( BOX_x1 + 160, BOX_y1 + 368, "START", START_N )
        
    do
        sleep 50    ' Let the CPU do other things
        key = Inkey$
        mouseRes  = GetMouse (mouseX, mouseY, , mouseButtons)
        if mouseRes = 0 then    '' otherwise no mouse available
            if mouseButtons And 1 then      'on left mouse button
                if(ifStartClicked) then r = 1
                GameModeClicked 'Crafty,Naum or autogame
                SetLimits       'options of chess engines
            end if
        end if
        
        if (len(key)>0 ) then
            if(key = CHR(255) & "H") then   'Up
                if(GameMode>GAME_WITH_CRAFTY) then
                    GameMode -= 1
                end if
            end if
            if(key = CHR(255) & "P") then   'Down
                if(GameMode<GAME_CRAFTY_VS_NAUM) then
                    GameMode += 1
                end if
            end if
        end if
    
        DrawCursOnGame(GAME_WITH_CRAFTY)
        DrawCursOnGame(GAME_WITH_NAUM)
        DrawCursOnGame(GAME_CRAFTY_VS_NAUM)
        
        DrawLimits
    
        if(IsMusicOver and fMusic) then
            PlayMusic
        end if
    Loop until (r>0) or EscEntSpcKeyClick( key )
end sub

sub setImgButton( s as string, N as integer, x as integer, y as integer )
    ButtXY(N,1)=x
    ButtXY(N,2)=y
    ButtXY(N,3)=x+70
    ButtXY(N,4)=y+70
    PutPieceImage( s, x, y )
end sub

function whichPromote( f as string ) as string
    dim as string r = ""
    dim as integer p = 0, promo = PROMO_Q_N
    DrawBox(2)
    PutLetterString( BOX_x1 + 20, BOX_y1 + 120, _
        "WHICH PIECE TO PROMOTE?")
    setImgButton( f & "Q" & "b", PROMO_Q_N, BOX_x1 + 30, BOX_y1 + 160 )
    setImgButton( f & "R" & "w", PROMO_R_N, BOX_x1 + 120, BOX_y1 + 160 )
    setImgButton( f & "B" & "b", PROMO_B_N, BOX_x1 + 210, BOX_y1 + 160 )
    setImgButton( f & "N" & "w", PROMO_N_N, BOX_x1 + 300, BOX_y1 + 160 )    
    
    DrawButton( BOX_x1 + 170, BOX_y1 + 262, "YES", YES_N )
    
    do
        sleep 50    ' Let the CPU do other things
        key = Inkey$
        mouseRes  = GetMouse (mouseX, mouseY, , mouseButtons)
        if mouseRes = 0 then    '' otherwise no mouse available
            if mouseButtons And 1 then      'on left mouse button
                p = PromoPieceClicked 'user clicked on piece
                if(p>0) then
                    promo = p
                end if
                if (ifOKClicked) then
                    key = chr(13)
                end if
            end if
        end if
        
        if (len(key)>0 ) then
            if(key = CHR(255) & "K") then   'Left
                if(promo>PROMO_Q_N) then
                    promo -= 1
                end if
            end if
            if(key = CHR(255) & "M") then   'Right
                if(promo<PROMO_N_N) then
                    promo += 1
                end if
            end if
        end if
 
        DrawButtonLine( PROMO_Q_N, promo=PROMO_Q_N)
        DrawButtonLine( PROMO_R_N, promo=PROMO_R_N)
        DrawButtonLine( PROMO_B_N, promo=PROMO_B_N)
        DrawButtonLine( PROMO_N_N, promo=PROMO_N_N)
            
        if(IsMusicOver and fMusic) then
            PlayMusic
        end if
    Loop until EscEntSpcKeyClick( key )
    
    select case(promo)
    case PROMO_Q_N
        r = "Q"
    case PROMO_R_N
        r = "R"
    case PROMO_B_N
        r = "B"
    case PROMO_N_N
        r = "N"
    end select
    return r
end function

