000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300990421     fFNDCT00F  IP   E           K DISK
000400990421     fFNDCD01L  UF   E           K DISK
000500990420     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000600990420      * ---------------------------------------------------------
000700990420     c     *entry        PLIST
000800990422     c                   parm                    aggio             1
000900990420      *
001000990420     c     kca           klist
001100990420     c                   kfld                    dctaac
001200990420     c                   kfld                    dctfil
001300990420     c                   kfld                    dctnca
001400980625      * ---------------------------------------------------------
001500990421     c                   IF        dctdch > 0
001600990421     c     kca           setll     FNDCD000
001700990421     c     kca           reade     FNDCD000                               31
001800990420     c                   DOW       *IN31 = *off
001900990421     c                   IF        dcdatb <> 'A' and dcddch = 0
002000990421     c* aggiorno dcd
002100990422     c                   IF        aggio = 'S'
002200990421     c                   eval      dcddch = dctdch
002300990421     c                   eval      dcdcch = dctcch
002400990421     c                   update    fndcd000
002500990422     c*
002600990422     c                   else
002700990422     c*
002800990422     c                   except    DETTA
002900990420     c                   ENDIF
003000990421     c                   ENDIF
003100990421     c*
003200990421     c     kca           reade     FNDCd000                               31
003300990421     c*
003400990420     c                   ENDDO
003500990421     c*
003600990421     c                   ENDIF
003700990420      *------------------------------------------------------------
003800990420     OPRTF198   E            DETTA       2
003900990420     O                       dctaac               4
004000990420     O                       dctfil           +   1
004100990420     O                       dctnca        z  +   1
004200990421     O                       dctdch           +   2
004300990421     O                       DCTcch           +   2
004400990421     O                       dcddch           +   2
004500990421     O                       dcdcch           +   2
004600990421     O          E            respa       1
004700990421     O                       dctaac               4
004800990421     O                       dctfil           +   1
004900990421     O                       dctnca        z  +   1
005000990421     O                                        +   1 'Manca di responsabilità'
