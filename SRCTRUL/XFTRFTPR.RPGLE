000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400030808     FFTPINPUT  uf a e             disk    RENAME(FTPINPUT:FTPIN000)
000500030808     F                                     PREFIX(f_)
000600030808
000700030808     D prmusr          s             10
000800030808     D prmpwd          s             10
000900030808     D prmdir          s             20
001000030808     D prmlib          s             10
001100030808     D prmfil          s             10
001200030820     D prmmbr          s             10
001300030808     D prmnomfiltmp    s             30
001400030808     D prmnomfil       s             30
001500030808     D Wriga           s            256
001600030808
001700030808
001800030808
001900030808      /TITLE *** MAIN ***
002000030808     C*
002100030808     C* Costruzione 1° riga: utente e password
002200030808     C                   eval      Wriga = %trim(prmusr) + ' ' + %trim(prmpwd)
002300030808     C                   eval      f_FTPINPUT = Wriga
002400030808     C                   write     FTPIN000
002500030808     C*
002600030808     C* Costruzione 2° riga: cambio directory
002700030808     C                   eval      Wriga = 'CD' + ' ' + %trim(prmdir)
002800030808     C                   eval      f_FTPINPUT = Wriga
002900030808     C                   write     FTPIN000
003000030808     C*
003100030808     C* Costruzione 3° riga: put del libreria/file
003200030808     C                   eval      Wriga = 'PUT'+' '+%trim(prmlib)+'/'+
003300030820     C                                     %trim(prmfil)+'.'+%trim(prmmbr)+' '+
003400030820     C                                     %trim(prmnomfiltmp)
003500030808     C                   eval      f_FTPINPUT = Wriga
003600030808     C                   write     FTPIN000
003700030808     C*
003800030808     C* Costruzione 4° riga: rename del file a fine trasferimento
003900030808     C                   eval      Wriga = 'RENAME'+' '+%trim(prmnomfiltmp)+
004000030808     C                                     ' '+%trim(prmnomfil)
004100030808     C                   eval      f_FTPINPUT = Wriga
004200030808     C                   write     FTPIN000
004300030808     C*
004400030808     C* Costruzione 5° riga: uscita dalla sessione FTP
004500030808     C                   eval      Wriga = 'QUIT'
004600030808     C                   eval      f_FTPINPUT = Wriga
004700030808     C                   write     FTPIN000
004800030808     C*
004900030808     C                   seton                                        LR
005000030808
005100030808
005200030808
005300030808      /TITLE Operazioni iniziali.
005400030808     C     *inzsr        begsr
005500030808     C*
005600030808     C     *entry        plist
005700030808     C                   parm                    prmusr
005800030808     C                   parm                    prmpwd
005900030808     C                   parm                    prmdir
006000030808     C                   parm                    prmlib
006100030808     C                   parm                    prmfil
006200030820     C                   parm                    prmmbr
006300030808     C                   parm                    prmnomfiltmp
006400030808     C                   parm                    prmnomfil
006500030820     C*
006600030820     C* Testo il parametro relativo al membro, se vuoto assumo come valore il nome file
006700030820     C                   if        prmmbr = *blanks
006800030820     C                   eval      prmmbr = prmfil
006900030820     C                   endif
007000030808     C*
007100030808     C                   endsr
