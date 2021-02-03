# $language = "VBScript"
# $interface = "1.0"

' for SecureCRT to configure ITG6 line profiles

PhonePrefix = "5788"
StartNumber = 693
Password = "opnet"
FirstLine = 1
LastLine = 48
bEnableLine = false

PhysicalLineId    = "8"
DirectoryNumberId = "4"
CallerIDNameId    = "5"
SIPAuthUserNameId = "9"
SIPAuthPasswordId = "10"
EnableLineId      = "3"

Dim re, colMatch
Set re = New RegExp

Function SendCmd( cmd, wait )
	crt.screen.Send(cmd+Chr(13))
	str = crt.Screen.ReadString("OK", "failed", wait)
	if crt.Screen.MatchIndex = 0 then crt.Quit
	SendCmd = str
End Function

Function SendCmdOK( cmd, wait )
	str = SendCmd( cmd, wait )
	if crt.Screen.MatchIndex <> 1 then crt.Quit
	SendCmdOK = str
End Function

Function ConfigCmd( cmd, wait )
	ConfigCmd = SendCmd( "config "+cmd, wait )
End Function

Function ConfigCmdOK( cmd, wait )
	ConfigCmdOK = SendCmdOK( "config "+cmd, wait )
End Function

Function ConfigLine( cmd, item, wait )
	ConfigLine = ConfigCmd( cmd+" 4/2"+item, wait )
End Function

Function ConfigLineOK( cmd, item, wait )
	ConfigLineOK = ConfigCmdOK( cmd+" 4/2"+item, wait )
End Function

Sub SetLine( profId, itemId, value )
	ConfigLineOK "set", "/"+profId+"/"+itemId+" "+value, 2
End Sub

Function GetUser( line )
	GetUser = PhonePrefix + CStr( StartNumber + line - 1 )
End Function

Sub AddLine( line )
	str = ConfigLineOK( "add", "", 2 )

	re.Pattern = "([0-9]+) added"
	Set colMatch = re.Execute( str )
	if colMatch.count = 0 then crt.Quit

	profId = colMatch.item(0).SubMatches(0)
	user = GetUser( line )
	SetLine profId, PhysicalLineId   , CStr(line)
	SetLine profId, DirectoryNumberId, user
	SetLine profId, CallerIDNameId   , user
	SetLine profId, SIPAuthUserNameId, user
	SetLine profId, SIPAuthPasswordId, Password
	if bEnableLine then
		SetLine profId, EnableLineId, "Enabled"
	else
		SetLine profId, EnableLineId, "Disabled"
	end if
End Sub

Sub Main

	crt.Screen.Synchronous = true
	crt.Screen.IgnoreEscape = true

	ConfigLine "del", "", 5
	For i = FirstLine to LastLine
		AddLine i
	Next
	ConfigLineOK "commit", "", 10
	SendCmdOK "commit", 5

	MsgBox "Done"

End Sub
