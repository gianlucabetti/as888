000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300051109     FFNARB05L  iF   E           K DISK
000400051109     FFNARt01L  iF   E           K DISK
000500051109     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
000600051109     F                                     formlen(66)
000700051109     F                                     formofl(63)
000800051109     D                 DS
000900051109     D  ARBAAS                 1      4  0
001000051109     D  ARBMGS                 5      8  0
001100051109     D  ARBDSP                 1      8  0
001200051109     d trul06ds      e ds
001300051109     d  lin                    1     90  0 dim(30)
001400051109     D KPJBA         E DS
001500051109     d Dataiso         s               d   datfmt(*iso)
001600051109     d DataEUR         s               d   datfmt(*EUR)
001700000602      *--------------------------------------------------
001800050111     C     *LOVAL        SETLL     FNARB05L                           21
001900050111     C                   READ      FNARB05L                               21
002000000601      *
002100051109    1C     *IN21         DOWEQ     *OFF
002200051109     c* elaboro  tfp=05
002300051109    2c                   if        arbtfp=05  and arbaas=2005 and arbmgs>
002400051109     c                             1015
002500051109     c* Elaboro solo le linee di arrivo di roma
002600051109     c     arblna        lookup    lin                                    30
002700051109    3c                   if        *in30
002800051109     c                   clear                   contmanca
002900051109     c* Leggo dettaglio colli per vedere se collo partito il 7/11
003000051109     c     karb          setll     fnart01l
003100051109     c     karb          reade     fnart01l
003200051109    4c                   dow       not %eof(fnart01l)
003300051109    5c                   if        artdfv=20051107  and artdam=0  and
003400051109     c                             artdet=0
003500051109     c                   add       1             contmanca         5 0
003600051109    5c                   endif
003700051109     c     karb          reade     fnart01l
003800051109    4c                   enddo
003900051109     c
004000051109    4c                   if        contmanca>0
004100051109     c                   exsr      stampa
004200051109    4c                   endif
004300051109     c
004400051109    3c                   endif
004500051109    2c                   endif
004600050111      *
004700050111     C                   READ      FNARB05L                               21
004800051109    1C                   ENDDO
004900000601      *
005000051109     c                   except    totale
005100950201     C                   SETON                                        LR
005200000601      *---------------------------------------------------------
005300000601      *   IMPOSTAI PUNTI DI ARRIVO (AD ORA VERONA E TORINO)
005400000601      *---------------------------------------------------------
005500050111     C     *INZSR        BEGSR
005600000601      *
005700000601     C     KARB          KLIST
005800000601     C                   KFLD                    ARBAAS
005900000601     C                   KFLD                    ARBLNP
006000000601     C                   KFLD                    ARBNRS
006100000601     C                   KFLD                    ARBNSP
006200051109     c
006300051109     c                   seton                                        of
006400051109     c
006500051109     c                   clear                   trul06ds
006600051109     c                   eval      d06tla = 'L'
006700051109     c                   eval      d06cod = '£1'
006800051109     c                   movel     063           d06key
006900051109     c                   movel     trul06ds      kpjbu
007000051109     c                   call      'TRUL06R'
007100051109     c                   parm                    kpjba
007200051109     c                   movel     kpjbu         trul06ds
007300051109     c
007400051109     c                   z-add     1             xx                3 0
007500051109     c     000           lookup    lin(xx)                                30
007600051109     c                   if        *in30
007700051109     c                   eval      lin(xx)=255
007800051109     c                   endif
007900051109     c
008000050111     C                   ENDSR
008100051109      *---------------------------------------------------------
008200051109      *   stampa bolla mancante
008300051109      *---------------------------------------------------------
008400051109     C     stampa        BEGSR
008500051109     C     *ISO          movel     arbdsp        dataiso
008600051109     c                   movel     dataiso       dataeur
008700051109     c     *eur          movel     dataeur       stadat            8 0
008800051109     c
008900051109     c                   movel     arblom        stalom           20
009000051109     c                   movel     arblod        stalod           25
009100051109     c                   if        arbnzm<>*blanks
009200051109     c                   movel     arbnzm        staprm            3
009300051109     c                   else
009400051109     c                   movel(P)  arbprm        staprm
009500051109     c                   endif
009600051109     c                   if        arbnzd<>*blanks
009700051109     c                   movel     arbnzd        staprd            3
009800051109     c                   else
009900051109     c                   movel(P)  arbprd        staprd
010000051109     c                   endif
010100051109     c
010200051109     c                   if        *inof
010300051109     c                   except    testa
010400051109     c                   setoff                                       of
010500051109     c                   endif
010600051109     c
010700051109     c                   except    detta
010800051109     c                   add       1             totbolle          9 0
010900051109     c                   add       contmanca     totcolli          9 0
011000051109     c
011100051109     C                   ENDSR
011200051109     Oprtf198   E            TESTA            01
011300051109     O                       *DATE               20 '  /  /    '
011400051109     O                                           80 'E L E N C O   S P E D I Z'
011500051109     O                                         +  0 ' I O N I   M A N C A N T I'
011600051109     O                                          180 'PAG.'
011700051109     O                       PAGE               190
011800051109     O          E            TESTA       2
011900051109     O                                         +  1 'S P E D I Z I O N E'
012000051109     O                                         +  2 'LNA'
012100051109     O                                           37 'DATA SPED.'
012200051109     O                                           80 'M  I  T  T  E  N  T  E'
012300051109     O                                          130 'D  E  S  T  I  N  A  T'
012400051109     O                                         +  0 '  A  R  I  O'
012500051109     O                                          178 'NATURA MERCE'
012600051109     O                                         +  5 'COLLI'
012700051109     O                                         +  2 'MANCANTI'
012800051109     O          E            DETTA       1
012900051109     O                       ARBAAS
013000051109     O                       ARBLNP            +  1
013100051109     O                       ARBNRS        z   +  1
013200051109     O                       ARBNSP        2   +  1
013300051109     O                       ARBlna            +  1
013400051109     O                       stadat            +  1 '  /  /    '
013500051109     O                       ARBrsm            +  2
013600051109     O                       stalom            +  1
013700051109     O                       staprm            +  1
013800051109     O                       ARBrsd            +  2
013900051109     O                       stalod            +  1
014000051109     O                       staprd            +  1
014100051109     O                       arbnas            +  1
014200051109     O                       arbncl        2   +  1
014300051109     O                                         +  1 '|'
014400051109     O                       contmanca     2   +  0
014500051109     O          E            totale      2
014600051109     O                                              'Totale bolle :'
014700051109     O                       totbolle      2   +  1
014800051109     O                                         + 10 'Totale colli mancanti:'
014900051109     O                       totcolli      2   +  1
