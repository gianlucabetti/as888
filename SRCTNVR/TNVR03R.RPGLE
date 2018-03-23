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
002700990614      *
002800991117     C     *LOVAL        SETLL     TNTAM01L
002900991117     C                   READ      TNTAM01L                               30
003000991117     C     *IN30         DOWEQ     *OFF
003100991117     C**
003200000209     c                   movel     tamksc        w0040             4 0
003300000209     C     TAMATB        IFEQ      ' '
003400000209     c     w0040         andne     8888
003500000202     c                   if        tamctr>=000 and tamctr<=099 or
003600000202     c                             tamctr>=300 and tamctr<=399
003700000202     c                   clear                   scaglio           1
003800991117     C     KTAM          CHAIN     TITAD04L                           31
003900991117     C**
004000991117     C     *IN31         DOWEQ     *OFF
004100000202     c                   z-add     tadsgl        w0050             5 0
004200991117     C     TADATB        IFEQ      ' '
004300000202     c*
004400000209     c     tadsgl        iflt      30
004500000202     C     TADRPV        ANDGT     0
004600000202     C                   ADD       1             CONTRPV          10 0
004700000202     c                   except    stampa
004800000202     C                   SETON                                        31
004900000202     c                   endif
005000000202     c*
005100000202     c     w0050         ifeq      99999
005200000202     C     TADRPV        andgt     0
005300000202     c     scaglio       andeq     ' '
005400000202     C                   ADD       1             CONTRPV          10 0
005500000202     c                   except    stampa
005600000202     C                   SETON                                        31
005700000202     C                   endif
005800000202     c                   movel     '1'           scaglio
005900991117     C                   ENDIF
006000000202     c*
006100000202     C  n31KTAM          READE     TITAD04L                               31
006200991117     C                   ENDDO
006300991117     C*
006400991117     C                   ADD       1             CONTTAR          10 0
006500991117     C                   ENDIF
006600000202     C                   ENDIF
006700991117     C**
006800991117     C                   READ      TNTAM01L                               30
006900991117     C                   ENDDO
007000991117     C* CALCOLO %
007100991117     C     CONTRPV       IFGT      0
007200991117     C                   EVAL      PERC=(CONTRPV*100)/CONTTAR
007300991117     C                   ENDIF
007400991117     C                   EXCEPT    TOTALE
007500991117     C**
007600991117     C                   SETON                                        LR
007700000202     OPRTF198   E            STAMPA      1
007800000202     O                       TAMKSC            +  2
007900000202     O                       TAMCTR            +  2
008000000202     O                       TAMPRG        2   +  2
008100000202     O                       TAMfie            +  2
008200000202     O                       TAMDDt            +  2 '    /  /  '
008300000202     O                       TAMDSt            +  2 '    /  /  '
008400000202     O                       TAdlnp            +  2
008500000202     O                       TAdcts            +  2
008600000202     O                       TAdsgl        2   +  2
008700000202     O                       TAdrpv        2   +  2
008800000202     O          E            TOTALE      2
008900000209     O                       RSUT
009000000209     O                                         +  2 'Tariffe con rapporto peso/'
009100000209     O                                              'volume presente in almeno '
009200000209     O                                              'uno scaglione < 30 o con u'
009300000209     O                                              'nico scaglione 99999     -'
009400000209     O                       data              +  5 '  /  /    '
009500000209     O          E            TOTALE      2
009600000209     O                                              'TOTALE TARIFFE QLI o KG (e'
009700000209     O                                              'scluse tariffe di cartello'
009800000209     O                                              '):'
009900000209     O                       CONTTAR       2   +  0
010000991117     O                                         +  4 'TARIFFE CON RPV:'
010100000209     O                       CONTRPV       2   +  0
010200000209     O                                         +  4 '  %'
010300991117     O                       PERC          4   +  2
