000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300121213     FAZCAE01L  IF   E           K DISK
000400020802     D
000500121211     D KPJBA         E DS
000600121211     D fnls29ds        DS
000700121211     d* Terminal di partenza
000800121211     D  I29TFP                        3  0
000900121211     c* Numero foglio viaggio: se = 0 tutti i fogli aperti del terminal
001000121211     D  I29nfv                        6  0
001100121211     D  o29err                        1
001200121211     D  o29msg                      240
001300121213     d Dataiso         s               d   datfmt(*iso) inz(*sys)
001400980528     I***************************************************************
001500121213     c                   movel     dataiso       dateu             8 0
001600980528     c**
001700121213     C* SOLO SE MEMBRO PIENO
001800121213     C     'P'           SETLL     AZCAE01L
001900121213     C     'P'           READE     AZCAE01L                               31
002000121213     C*
002100121213    1C     *IN31         DOWEQ     *OFF
002200121213     C*
002300121213    2C     CAETFP        IFEQ      CAETFE
002400121213     C     CAEATB        ANDEQ     *BLANKS
002500121213     C     dateu         ANDGE     CAEDDE
002600121213     C     dateu         ANDLE     CAEDSC
002700121213     C*
002800121211     c                   clear                   fnls29ds
002900121213     C                   EVAL      I29TFP=caetfe
003000121211     C                   MOVEL     FNLS29DS      KPJBU
003100121211     c                   call      'FNLS29R'
003200121211     c                   parm                    kpjbA
003300121213     c* se c'e' messaggio, inivio mail   a ced
003400121213     c                   movel     kpjbu         fnls29ds
003500121213     c
003600121213     c                   if        o29msg<>*blanks
003700121213     c                   eval      msgtes='Fogli Viaggio non corretti TFP: ' +
003800121213     c                             %editc(caetfe:'X')
003900121213     c                   eval      msgdet= o29msg
004000121213     C
004100121213     C                   eval      wrkEml= 'ced@brt.it'
004200121213     C                   call(e)   'TIS701C'
004300121213     C                   parm                    wrkEml          253
004400121213     C                   PARM                    MSGTES           44
004500121213     C                   PARM                    MSGDET         5000
004600121213     C*
004700121213     c                   endif
004800121213     c                   endif
004900121213     C*
005000121213     C     'P'           READE     AZCAE01L                               31
005100121213     c                   enddo
005200020322     C*
005300020322     C                   SETON                                        LR
