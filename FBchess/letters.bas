
' This draws old-style letters as bitmap images

'this was created by letters.htm tool
const LtrCnt=54
dim shared as integer LtrHeight=20
dim shared LtrData(LtrCnt*2) as ubyte = _
{ 32,6,33,8,34,8,35,17,39,8,40,12,41,12,42,14,43,14,44,9, _
45,11,46,7,47,17,48,16,49,10,50,16,51,16,52,16,53,16,54,16, _
55,16,56,16,57,16,58,7,59,9,61,12,63,16,65,16,66,16,67,16, _
68,16,69,14,70,14,71,16,72,16,73,8,74,16,75,16,76,14,77,20, _
78,16,79,16,80,16,81,16,82,16,83,18,84,16,85,16,86,16,87,19, _
88,16,89,16,90,16,92,17 }

const LtrImagesFolder = "\images\"

dim shared LtrImages(LtrCnt) As Any Ptr

sub LoadImagesOfLetters
    dim as integer i
    for i=0 to LtrCnt-1
        LtrImages(i+1) = Imagecreate( LtrData((i*2)+1), LtrHeight )
        Bload  exepath( ) & LtrImagesFolder & "let_" & _
            str( LtrData(i*2) ) & ".bmp", LtrImages(i+1)
    next
end sub

'LoadImagesOfLetters 'prepare images

sub PutLetterString(ByRef x as integer, ByRef y as integer, s as string)
    dim as string sB = UCase(s)
    dim as integer c, n,i,xi=x, xm=x
    for n=1 to len(sB)
        c = asc( mid(sB,n,1) )
        if(c=10 or c=13) then   'if next line
            xi = x
            y += LtrHeight
        else
            for i=0 to LtrCnt-1
                if( c = LtrData(i*2) ) then ' If there is such a letter
                    Put (xi,y), LtrImages(i+1), pset
                    xi+=LtrData((i*2)+1)
                    if( xm<xi ) then
                        xm = xi
                    end if
                end if
            next
        end if
    next
    if (xi>x) then
        y += LtrHeight
    end if
    x=xm
end sub

