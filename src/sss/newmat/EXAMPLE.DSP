# Microsoft Developer Studio Project File - Name="EXAMPLE" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=EXAMPLE - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "EXAMPLE.MAK".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "EXAMPLE.MAK" CFG="EXAMPLE - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "EXAMPLE - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "EXAMPLE - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "EXAMPLE - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "EXAMPLE - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "EXAMPLE - Win32 Release"
# Name "EXAMPLE - Win32 Debug"
# Begin Source File

SOURCE=.\BANDMAT.CPP
# End Source File
# Begin Source File

SOURCE=.\BOOLEAN.H
# End Source File
# Begin Source File

SOURCE=.\CHOLESKY.CPP
# End Source File
# Begin Source File

SOURCE=.\CONTROLW.H
# End Source File
# Begin Source File

SOURCE=.\EVALUE.CPP
# End Source File
# Begin Source File

SOURCE=.\EXAMPLE.CPP
# End Source File
# Begin Source File

SOURCE=.\FFT.CPP
# End Source File
# Begin Source File

SOURCE=.\HHOLDER.CPP
# End Source File
# Begin Source File

SOURCE=.\INCLUDE.H
# End Source File
# Begin Source File

SOURCE=.\JACOBI.CPP
# End Source File
# Begin Source File

SOURCE=..\marglik.cpp
# End Source File
# Begin Source File

SOURCE=.\MYEXCEPT.CPP
# End Source File
# Begin Source File

SOURCE=.\MYEXCEPT.H
# End Source File
# Begin Source File

SOURCE=.\NEWFFT.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT.H
# End Source File
# Begin Source File

SOURCE=.\NEWMAT1.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT2.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT3.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT4.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT5.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT6.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT7.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT8.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMAT9.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMATAP.H
# End Source File
# Begin Source File

SOURCE=.\NEWMATEX.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMATIO.H
# End Source File
# Begin Source File

SOURCE=.\NEWMATNL.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMATNL.H
# End Source File
# Begin Source File

SOURCE=.\NEWMATRC.H
# End Source File
# Begin Source File

SOURCE=.\NEWMATRM.CPP
# End Source File
# Begin Source File

SOURCE=.\NEWMATRM.H
# End Source File
# Begin Source File

SOURCE=.\PRECISIO.H
# End Source File
# Begin Source File

SOURCE=.\SOLUTION.CPP
# End Source File
# Begin Source File

SOURCE=.\SOLUTION.H
# End Source File
# Begin Source File

SOURCE=.\SORT.CPP
# End Source File
# Begin Source File

SOURCE=.\SUBMAT.CPP
# End Source File
# Begin Source File

SOURCE=.\SVD.CPP
# End Source File
# End Target
# End Project
