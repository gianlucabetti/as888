000100020325     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020325      *------------------------------------------------------------------------*
000300120522      * Modifica campo sulle unità  - UNADES -
000400020325      *------------------------------------------------------------------------*
000500020325
000600020325      *------------------------------------------------------------------------*
000700020325      * DATA BASE
000800020325      *------------------------------------------------------------------------*
000900120522     FUNtra01L  iF   E           K DISK
001000120522     FUNana07L  UF   E           K DISK
001100120522     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200020325
001300971013     D KPJBA         E DS
001400020325
001500971013     C*------------------------------------------------------------------------*
001600120522     c                   if        updatare='S'
001700120522     c                   seton                                        02
001800120522     c                   endif
001900120522     c                   except    testa
002000120522     c
002100120522     c     *loval        setll     untra01l
002200120522     c                   read      untra01l
002300120522    1c                   dow       not %eof(untra01l)
002400120522
002500120522     c                   setoff                                       01
002600120522     c                   clear                   unades
002700120522     c     tramat        setll     unana07l
002800120522     c     tramat        reade     unana07l
002900120522    2c                   dow       not %eof(unana07l)
003000120522
003100120522    3c                   if        traold = unades
003200120522     c                   seton                                        01
003300120522     c                   except    det
003400120522     c
003500120522    4c                   if        updatare='S'
003600120522     c                   eval      unades=tranew
003700120522     c                   update    unana000
003800120522    4c                   endif
003900120522     c
004000120522     c                   add       1             aggio             7 0
004100120522     c                   leave
004200120522    3c                   endif
004300120522
004400120522     c     tramat        reade     unana07l
004500120522    2c                   enddo
004600120522     c* se non ho aggiornato stampo -->
004700120522    2c                   if        not *in01
004800120522     c                   add       1             noaggio           7 0
004900120522     c                   except    det
005000120522    2c                   endif
005100120522     c
005200120522     c                   read      untra01l
005300120522    1c                   enddo
005400971013     C*
005500120522     c                   except    totale
005600971013     C                   SETON                                        LR
005700971013     C*------------------------------------------------------------------------*
005800971013     C* *INZSR - OPERAZIONI INIZIALI
005900971013     C*------------------------------------------------------------------------*
006000971013     C     *INZSR        BEGSR
006100971013     C*--------------------
006200971013     C* RICEVIMENTO PARAMETRI
006300971013     C*--------------------
006400971013     C     *ENTRY        PLIST
006500971013     C                   PARM                    KPJBA
006600120522     c                   movel     kpjbu         updatare          1
006700971013     C                   ENDSR
006800120522     O*------------------------------------------------------------------------*
006900120522     Oqsysprt   E            TESTA       2  1
007000120522     O                                        +  20 'Aggiornamento  " UNADES " '
007100120522     O                                        +   1 ' in UNANA00F'
007200120522     o              n02                       +  10 '*** S I M U L A ***'
007300120522     o               02                       +  10 '** A G G I O R N A **'
007400120522     O                       UDATE              130 '  /  /  '
007500120522     Oqsysprt   E            det         1
007600120522     o                                              ' Matricola/descrizione:'
007700120522     O                       tramat           +   2
007800120522     O                       unaDES           +   2
007900120522     o               01                          86 ' Aggiornato'
008000120522     o              N01                          90 ' NON Aggiornato'
008100120522     Oqsysprt   E            det         1
008200120522     o                                           41 ' OLD :'
008300120522     O                       traold              72
008400120522     o                                        +   4 ' NEW :'
008500120522     O                       tranew           +   2
008600120522     Oqsysprt   E            totale      3
008700120522     o                                              ' TOTALE AGGIORNATI    :'
008800120522     O                       AGGIO         2  +   1
008900120522     o                                        +   4 ' TOTALE NON AGGIORNATI:'
009000120522     O                       NOAGGIO       2  +   1
