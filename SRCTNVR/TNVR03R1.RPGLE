000100990614     h DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300990611     H* PREP. ARCHIVIO DETTAGLIO TARIFFE                                       *
000400980323     H*------------------------------------------------------------------------*
000500991117     FTITAD04L  iF   e           K DISK
000600990721     FTNTAM01l  if   e           k DISK
000700991117     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
000800991117     F                                     FORMLEN(66)
000900991117     F                                     FORMOFL(58)
001000990721      *
001100000209     D UT§DSE0F      E DS
001200991117     d PERC            s              5  2
001300990721      *
001400991117     c     KTAM          klist
001500991117     c                   kfld                    taMksc
001600991117     c                   kfld                    taMctr
001700991117     c                   kfld                    taMprg
001800000209     c
001900000209     C                   Z-ADD     1             CODUT
002000000209     C                   CALL      'X§PARUT'
002100000209     C                   PARM                    UT§DSE0F
002200000209     C                   MOVEL     RAGUT         RSUT             20
002300000209     c
002400000209     C                   TIME                    W0140            14 0
002500000209      * UDATE IN GGMMAAAA
002600000209     C                   MOVE      W0140         data              8 0
002700000209      *
002800990614      *
002900991117     C     *LOVAL        SETLL     TNTAM01L
003000991117     C                   READ      TNTAM01L                               30
003100991117     C     *IN30         DOWEQ     *OFF
003200000209     c                   movel     tamksc        w0040             4 0
003300991117     C**
003400991117     C     TAMATB        IFEQ      ' '
003500000209     c     w0040         andne     8888
003600000202     c                   if        tamctr>=000 and tamctr<=099 or
003700000202     c                             tamctr>=300 and tamctr<=399
003800991117     C     KTAM          CHAIN     TITAD04L                           31
003900991117     C**
004000991117     C     *IN31         DOWEQ     *OFF
004100991117     C     TADATB        IFEQ      ' '
004200000202     C     TADRPV        ANDGT     0
004300000202     C                   ADD       1             CONTRPV          10 0
004400000202     c                   except    stampa
004500000202     C                   SETON                                        31
004600991117     C                   ENDIF
004700000202     c*
004800000202     C  n31KTAM          READE     TITAD04L                               31
004900991117     C                   ENDDO
005000991117     C*
005100991117     C                   ADD       1             CONTTAR          10 0
005200991117     C                   ENDIF
005300000202     C                   ENDIF
005400991117     C**
005500991117     C                   READ      TNTAM01L                               30
005600991117     C                   ENDDO
005700991117     C* CALCOLO %
005800991117     C     CONTRPV       IFGT      0
005900991117     C                   EVAL      PERC=(CONTRPV*100)/CONTTAR
006000991117     C                   ENDIF
006100991117     C                   EXCEPT    TOTALE
006200991117     C**
006300991117     C                   SETON                                        LR
006400000202     OPRTF198   E            STAMPA      1
006500000202     O                       TAMKSC            +  2
006600000202     O                       TAMCTR            +  2
006700000202     O                       TAMPRG        2   +  2
006800000202     O                       TAMfie            +  2
006900000202     O                       TAMDDt            +  2 '    /  /  '
007000000202     O                       TAMDSt            +  2 '    /  /  '
007100000202     O                       TAdlnp            +  2
007200000202     O                       TAdcts            +  2
007300000202     O                       TAdsgl        2   +  2
007400000202     O                       TAdrpv        2   +  2
007500000202     O          E            TOTALE      2
007600000209     O                       RSUT
007700000209     O                                              'Tariffe con rapporto peso/'
007800000209     O                                              'volume presente in almeno '
007900000209     O                                              'uno scaglione     -'
008000000209     O                       data              +  5 '  /  /    '
008100000209     O          E            TOTALE      2
008200000209     O                                              'TOTALE TARIFFE QLI o KG (e'
008300000209     O                                              'scluse tariffe di cartello'
008400000209     O                                              '):'
008500000209     O                       CONTTAR       2   +  0
008600991117     O                                         +  4 'TARIFFE CON RPV:'
008700000209     O                       CONTRPV       2   +  0
008800000209     O                                         +  4 '  %'
008900991117     O                       PERC          4   +  2
