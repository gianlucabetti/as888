000100150429     hDECEDIT('0,') DATEDIT(*YMD.)
000101170607      *****************************************************************
000104170607     fTABEL00F  if   e           k disk
000200000000      *****************************************************************
000300160927      *    crea Bolle recupero x consegne con ISOLA o FUORI MISURA
000400000000      *****************************************************************
000500030116     d KPJBA         E DS
000600160927     D*
000700160926     D tnecf00f      e DS
000800160926     D tnefr00f      e DS
000900151113     D*
001000160926     D fieu18r1DS    e DS
001100160926     D*
001101170607     d DSct          e DS
001102170607     D*
001200151210     d Wdata8          DS
001300151210     d  dagiolav               1      8  0
001400151210     d  agiolav                9     16  0
001500151210     d  GioLav                17     21  0
001600160926      *
001700160926     D WrkSqlCmd       S           1024
001800151106      *****************************************************************
001900000000     D WLBDAT          DS
002000940926     D  G02DAT                 1      8  0
002100940926     D  G02INV                 9     16  0
002200940926     D  G02ERR                17     17
002300940926     D  G02TGI                18     22  0
002400160308      *
002500151106     D parDate         DS
002600151106     D   DaData                1      8  0
002700151106     D    aData                9     16  0
002800160923     D     Anno                9     12  0
002900160923     D     Mese               13     16  0
003000151106      *
003100151106     D DATA_eur        S               D   DATFMT(*eur)
003200151106     D DATA_iso        S               D   DATFMT(*iso)
003300030205      *
003400030116     D digits          C                   CONST('0123456789')
003500030116      *****************************************************************
003600000000     C     *ENTRY        PLIST
003700000000     C                   PARM                    KPJBA
003800160923     C                   eval      parDate = KPJBU
003900030120      *
003901170607     C     KTAB          KLIST
003904170607     C                   KFLD                    tblkut
003905170607     C                   KFLD                    tblcod
003906170607     C                   KFLD                    tblkey
003911170607      *
004000030825     C* Imposta la UDATE  da TIME
004100030825     C                   TIME                    W0140            14 0
004200030825     C                   MOVE      W0140         UDATE8            8 0
004300150429     C                   MOVE      *date         Oggi              8 0
004400030825     C*
004500030827     c                   movel     KNMUS         ALFA3             3
004600160926      *
004700160926     c                   exsr      istruz
004800160926      *
004900000000      *--------------------------------------------------------------*
005000160926      *
005100160926     C/EXEC SQL
005200160926     C+ PREPARE S1 FROM :WrkSqlCmd
005300160926     C/END-EXEC
005400160926      *
005500160926     C/EXEC SQL
005600160926     C+ DECLARE A1 CURSOR FOR S1
005700160926     C/END-EXEC
005800160926      *
005900160926     C/EXEC SQL
006000160926     C+ OPEN A1
006100160926     C/END-EXEC
006200160926      *
006300160926     C                   DOU       SqlCod <> 0
006400160926     C/EXEC SQL
006500160927     C+ FETCH NEXT FROM A1 INTO :efrRIF, :efrCAU, :efrDFT,
006600170606     C+       :ecfAAS, :ecfLNP, :ecfNRS, :ecfNSP, :ecfRIF, :ecfCTS
006700160926     C/END-EXEC
006800160926     C                   SELECT
006900160926     c*
007000160926     c* a fine file Totali x rotture
007100160926     C                   WHEN      SqlCod = 100
007200160926     c                   leave
007300160926     **
007400160926     C                   WHEN      SqlCod < 0
007500160926     C                   seton                                        H1
007600160926     c                   goto      fine
007700160926      *
007800160926     C                   OTHER
007900160926     c* DETTAGLIO
008000160927     c                   exsr      Chiama_Bolla
008100160926     C                   ENDSL
008200160926      *
008300160926     C                   ENDDO
008400160926     C/EXEC SQL
008500160926     C+ CLOSE A1
008600160926     C/END-EXEC
008700160926      *
008800160926     c     fine          tag
008900160926     C                   SETON                                        LR
009000160926     C**************************************************************************
009100160926     C* mi compongo l'istruzione sql
009200160926     C**************************************************************************
009300160926     C     istruz        BEGSR                                                  *
009400160926      **
009500160926     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
009600160926     C                   EVAL      WrkSqlCmd  =
009700160926     C                             'with ORIGINE as ( '                        +
009800160926     c                             ' SELECT efrRIF, efrCAU, efrDFT '           +
009900160926     c                             ' FROM tnefr00f WHERE efrntw=''DPD'' and '  +
010000160926     c                             ' efrrif like''084%'' and '                 +
010100160926     c                             ' substr(efrcau, 1, 1)=''N'' and '          +
010200160926     c                             ' (substr(efrcau, 2, 1)=''F'' or '          +
010300160926     c                             ' substr(efrcau, 3, 1)=''I'' ) and '        +
010400160926     c                             ' efrdft between ' + %editc(daDATA:'X')     +
010500160926     c                             ' and ' + %editc(aDATA:'X')                 +
010600160926     c                             ' GROUP BY efrrif, EFRCAU, efrdft ),'       +
010700160926     C                             '     LEGAME  as ( '                        +
010800160926     c                             ' SELECT ecfAAS, ecfLNP, ecfNRS, ecfNSP, '  +
010900170606     c                             '  ecfRIF, ecfCTS  FROM tnecf00f WHERE '    +
011000160926     c                             '  ecfgrp =''001'' and ecftbl=''F1'' and '  +
011100160926     c                             '  ecfntw=''DPD'' and ecfrif like ''084%'' '+
011200160926     c                             '  and ecfdfp between ' + %editc(daDATA:'X')+
011300160926     c                             ' and ' + %editc(aDATA:'X') + ')'           +
011400160926     c                             '  SELECT Origine.*, legame.*            '  +
011500160926     c                             '    FROM legame join origine            '  +
011600160926     c                             '      on ecfrif = efrrif                '  +
011700160926     c                             '      order by efrcau, ecfaas, ecflnp   '
011800160926      **
011900160926     c                   ENDSR
012000160926      * ?------------------------------------------------------------------ */
012100160926      **
012200151113      * ?------------------------------------------------------------------ */
012300160926     c     Chiama_BOLLA  BEGSR
012400160926      **
012500160927      * se ci sono sia Isola che Fuori Misura esegue 2 chiamate
012600160927      *    il flag deve essere impostato I=Isola o F=Fuori Misura
012700160927      **
012800160927      ** Isola
012900160927     c                   if        %subst(efrCAU:3:1)='I'
013000170606      **
013001170607     c                   eval         tblkut     = 1
013002170607     c                   eval         tblcod     = 'CT'
013003170607     c                   eval         tblkey     = ecfCTS
013004170607     c                   clear                   Bolla_Recupero    1
013008170607     c*
013009170607     C     KTAB          chain     tabel00f
013010170607     C                   if        %Found(tabel00F)
013011170607     c                   eval      dsCT = tbluni
013012170607     C                   eval      Bolla_Recupero = §CTISLCLR
013013170607     C                   endIF
013014170607     C
013100170606      ** CHIODO --> da pilotare in seguito con la tebella "CT"
013200170606      **
013300170606      **  non devono essere create le bolle di recupero ISOLE per
013400170606      **   i codici tassazione "Baleari" E9 e "Corsica" F6
013500170606      **
013600170607     c                   if        Bolla_Recupero <> 'N'
013700170606      **
013800160926     c                   clear                   fieu18r1DS
013900160926     c                   eval         I18AAS     = ecfAAS
014000160926     c                   eval         I18LNP     = ecfLNP
014100160926     c                   eval         I18NRS     = ecfNRS
014200160926     c                   eval         I18NSP     = ecfNSP
014300160926     c                   eval         I18PARCEL  = ecfRIF
014400160927     c                   eval         I18WEIGHT  = 0
014500160927     c                   eval         I18FCLR    = 'I'
014600160926     c                   call      'FIEU18R1'
014700160926     c                   parm                    kpjba
014800160927     c                   parm                    fieu18r1ds
014900170606      **
015000160927     c                   end
015100170606     c                   end
015200160926      *
015300160927      ** Fuori Misura
015400160927     c                   if        %subst(efrCAU:2:1)='F'
015500160927     c                   clear                   fieu18r1DS
015600160927     c                   eval         I18AAS     = ecfAAS
015700160927     c                   eval         I18LNP     = ecfLNP
015800160927     c                   eval         I18NRS     = ecfNRS
015900160927     c                   eval         I18NSP     = ecfNSP
016000160927     c                   eval         I18PARCEL  = ecfRIF
016100160927     c                   eval         I18WEIGHT  = 0
016200160927     c                   eval         I18FCLR    = 'F'
016300160927      **
016400160927     c                   call      'FIEU18R1'
016500160927     c                   parm                    kpjba
016600160927     c                   parm                    fieu18r1ds
016700160927     c                   end
016800151210      *
016900160926     c                   ENDSR
017000151210      * ?------------------------------------------------------------------ */
