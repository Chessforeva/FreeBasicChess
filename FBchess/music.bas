#include once "fmod.bi"

dim shared as byte MusicON = 1
dim shared as byte MusicReady = 0

If (FSOUND_GetVersion < FMOD_VERSION) Then
    Print "FMOD version " + Str(FMOD_VERSION) + " or greater required!"
    MusicON = 0
End If

If (MusicON and FSOUND_Init(44100, 4, 0) = 0) Then
    Print "Could not initialize FMOD"
    MusicON = 0
End If

if (MusicON) then
    FSOUND_Stream_SetBufferSize(50)
End If

Dim shared As FSOUND_STREAM Ptr MusicStream

sub OpenMusicFile( filename as string )
    if (MusicON) then
        MusicStream = FSOUND_Stream_Open(filename, FSOUND_MPEGACCURATE, 0, 0)
    end if

    If (MusicON and MusicStream = 0) Then
        Print "FMOD could not load '" & filename & "'"
        FSOUND_Close()
        MusicON = 0
    End If
end sub

dim shared as integer MusicPos, MusicPrePos = -1

sub PlayMusic
    if(MusicON) then
        if(MusicReady<1) then
            OpenMusicFile( exepath( ) & "\music\" & "suka_suka.ogg")
            MusicReady = 1
        end if

        FSOUND_Stream_Play(FSOUND_FREE, MusicStream)
        MusicPos = 0
        MusicPrePos = -1
    end if
end sub

function IsMusicOver as byte
    if(MusicON) then
        MusicPos = FSOUND_Stream_GetPosition(MusicStream)
        if(MusicPos = MusicPrePos) then
            return 1
        else
            If (MusicPos >= FSOUND_Stream_GetLength(MusicStream)) Then
                return 1
            end if
        end if
        MusicPrePos = MusicPos
    end if
    return 0
end function

sub StopMusic
    FSOUND_Stream_Stop(MusicStream)
end sub

sub CloseMusic
    FSOUND_Stream_Stop(MusicStream)
    FSOUND_Stream_Close(MusicStream)
    FSOUND_Close()
end sub

   
