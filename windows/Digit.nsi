!include "x64.nsh"

; HM NIS Edit Wizard helper defines
!define PRODUCT_NAME "Digit"
!define PRODUCT_ARCH "win64-x86_64"
!define EXE_NAME "${PRODUCT_NAME}.exe"
!define PRODUCT_VERSION "v3.0"
!define PRODUCT_PUBLISHER "Ovidio Y. Peña Rodríguez"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\${EXE_NAME}"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"

SetCompressor bzip2

BrandingText "© 2003-2026 Ovidio Y. Peña Rodríguez"

; MUI 1.67 compatible ------
!include "MUI.nsh"

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_ICON "${PRODUCT_NAME}.ico"
!define MUI_UNICON "Un${PRODUCT_NAME}.ico"

; Welcome page
!insertmacro MUI_PAGE_WELCOME
; Components page
!insertmacro MUI_PAGE_COMPONENTS
; Directory page
!insertmacro MUI_PAGE_DIRECTORY
; Instfiles page
!insertmacro MUI_PAGE_INSTFILES
; Finish page
!define MUI_FINISHPAGE_RUN "$INSTDIR\${EXE_NAME}"
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language files
!insertmacro MUI_LANGUAGE "English"

; Reserve files
!insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

; MUI end ------

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
OutFile "Install-${PRODUCT_NAME}-${PRODUCT_VERSION}-${PRODUCT_ARCH}.exe"
; Use the "Program Files" directory for 64 bits
InstallDir "$PROGRAMFILES64\${PRODUCT_NAME}"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show

Section "!${PRODUCT_NAME}" SEC01
  SetOutPath "$INSTDIR"
  SetOverwrite try
  File "..\build\release\${PRODUCT_ARCH}\${PRODUCT_NAME}.exe"
  CreateDirectory "$SMPROGRAMS\${PRODUCT_NAME}"
  CreateShortCut "$SMPROGRAMS\${PRODUCT_NAME}\${PRODUCT_NAME}.lnk" "$INSTDIR\${EXE_NAME}"
  SectionIn RO
SectionEnd

Section "Desktop link" SEC02
  CreateShortCut "$DESKTOP\${PRODUCT_NAME}.lnk" "$INSTDIR\${EXE_NAME}"
  SetOverwrite try
SectionEnd

Section "Link in the quick access toolbar" SEC03
  CreateShortCut "$QUICKLAUNCH\${PRODUCT_NAME}.lnk" "$INSTDIR\${EXE_NAME}"
  SetOverwrite try
SectionEnd

Section -AdditionalIcons
  CreateShortCut "$SMPROGRAMS\${PRODUCT_NAME}\Desinstalar.lnk" "$INSTDIR\uninst.exe"
SectionEnd

Section -Post
  ; Use the register for 64 bits
  SetRegView 64

  WriteUninstaller "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\${EXE_NAME}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\${EXE_NAME}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"
SectionEnd

; Section descriptions
!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SEC01} "Install the files for $(^Name)"
  !insertmacro MUI_DESCRIPTION_TEXT ${SEC02} "Create a link on the desktop"
  !insertmacro MUI_DESCRIPTION_TEXT ${SEC03} "Create a link on the quick access toolbar"
!insertmacro MUI_FUNCTION_DESCRIPTION_END


Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "The uninstall of $(^Name) was completed successfully."
FunctionEnd

Function un.onInit
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Are you sure you want to uninstall $(^Name) along with all of its components?" IDYES +2
  Abort
FunctionEnd

Section Uninstall
  Delete "$INSTDIR\uninst.exe"
  Delete "$INSTDIR\${EXE_NAME}"

  Delete "$SMPROGRAMS\${PRODUCT_NAME}\Uninstall.lnk"
  Delete "$QUICKLAUNCH\${PRODUCT_NAME}.lnk"
  Delete "$DESKTOP\${PRODUCT_NAME}.lnk"
  Delete "$SMPROGRAMS\${PRODUCT_NAME}\${PRODUCT_NAME}.lnk"

  RMDir "$SMPROGRAMS\${PRODUCT_NAME}"
  RMDir "$INSTDIR"

  ; Use the register for 64 bits
  SetRegView 64

  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"
  SetAutoClose true
SectionEnd
