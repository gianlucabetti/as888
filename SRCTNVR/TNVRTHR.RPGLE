000100000725     H DECEDIT('0,') DATEDIT(*YMD.) DATFMT(*ISO)
000200000725
000300000725      ******************************************************************
000400000725      *                                                                *
000500010614      *  TNVRTHR - Controllo TITAH00T
000600000725      *                                                                *
000700000725      ******************************************************************
000800991227
000900010614     FTITAH00T  IF   E             DISK
001000000725
001100010614     D RECtitah        S             12  0
001200000725     D KPJBA         E DS
001300000725
001400000725      *----------------------------------------------------------------*
001500000725      *                                                                *
001600000725      *  ROUTINE PRINCIPALE                                            *
001700000725      *                                                                *
001800000725      *----------------------------------------------------------------*
001900000725
002000000725     C     *entry        plist
002100000725     C                   parm                    KPJBA
002200000725
002300010614     C                   eval      RECtitah = 0
002400000725
002500000914      * Controllo FNSTB00F
002600000725
002700000725     C                   do        *hival
002800010614     C                   read      titah00t                               99
002900000725     C   99              leave
003000010614     C                   eval      RECtitah = RECtitah + 1
003100000725     C                   enddo
003200000725
003300000725
003400000725      * Invio Messaggio
003500000725
003600010614     c                   if        RECtitah>0
003700010614     C                   move      RECtitah      WRECtitah        12
003800010614     C     '0'           check     WRECtitah     ch                3 0
003900010614     C                   eval      WRECtitah  = %subst(WRECtitah:ch)
004000000725     C                   else
004100010614     C                   move(p)   '0'           WRECtitah
004200000725     C                   endif
004300010614     C                   eval      MSGTES = 'TNVRTHR - Controllo file'
004400010614     C                             + ' TITAH00T '
004500000725     C                   eval      MSGDET = 'Trovati '
004600010614     C                             + %trim(WRECtitah) + ' records'
004700010614     C                             + ' di TITAH '
004800000725     C                             + ' ('
004900000725     C                             + %trim(KNSIF)
005000000725     C                             + ')'
005100000725
005200000725     C                   exsr      INVIA
005300000725
005400000725      * Fine Programma
005500000725
005600000725     C                   seton                                        LR
005700000725
005800000725      *----------------------------------------------------------------*
005900000725      *                                                                *
006000000725      *  SUBROUTINE DI INVIO MESSAGGIO                                 *
006100000725      *                                                                *
006200000725      *----------------------------------------------------------------*
006300000725
006400000725     C     INVIA         begsr
006500000725
006600000725     C                   call      'TNSYM0C'
006700000725     C                   parm                    MSGTES           55
006800000725     C                   parm                    MSGDET          198
006900000725
007000000725     C                   endsr
