@if(0)==(0) echo off
:: URL: http://bit.ly/cyginst
:: �ŏI�X�V: 2017�N5��20�� 13:13:14
setlocal

REM --- �ҏW����Ƃ���(�������火) ---
set CYG_NAME=project1
set CYG_BITS=32
::set CYG_CATS=Archive,Python
set CYG_PKGS=procps,psmisc,tmux,zsh,emacs-w32,git,vim,curl,ruby,chere
set DT_ICONS=1
::set CYG_HOME=.
REM --- �ҏW����Ƃ���(�����܂Ł�) ---

REM --- �f�o�b�O/�J�X�^�}�C�Y(�������火) ---
set CYG_DEBUG=0
set CYG_SITE=http://mirrors.kernel.org/sourceware/cygwin/
set CYG_LANG=ja
set CYG_FONT=MS Gothic
set CYG_FONT_HEIGHT=12
REM --- �f�o�b�O/�J�X�^�}�C�Y(�����܂Ł�) ---

cscript.exe //nologo //E:JScript "%~f0"
call cyginst.bat SUBPROC
endlocal
pause
exit /b
goto :EOF
@end
var fso = new ActiveXObject("Scripting.FileSystemObject");
var SCRIPT_CURRENT_DIR = fso.getParentFolderName(WScript.ScriptFullName);
var url = "https://raw.githubusercontent.com/cyginst/cyginst-v1/master/cyginst.bat";
var fileName = SCRIPT_CURRENT_DIR + "\\cyginst.bat";
try {
  downloadFile(url, fileName);
} catch (e) {
  WScript.Echo("cyginst.bat ���_�E�����[�h�ł��܂���ł���");
}
WScript.Quit();
function downloadFile(url, fileName) {
  var StreamTypeEnum  = { adTypeBinary: 1, adTypeText: 2 };
  var SaveOptionsEnum = { adSaveCreateNotExist: 1, adSaveCreateOverWrite: 2 };
  var http = WScript.CreateObject("MSXML2.XMLHTTP");
  var strm = WScript.CreateObject("ADODB.Stream");
  http.Open("GET", url, false);
  http.Send();
  strm.Type = StreamTypeEnum.adTypeBinary;
  strm.Open();
  strm.Write(http.responseBody);
  strm.SaveToFile(fileName, SaveOptionsEnum.adSaveCreateOverWrite);
}
