000100020319      /TITLE stampa Rettifiche conteggi
000200011221
000300930111     H DECEDIT('0,') DATEDIT(*DMY.)
000400011221
000500121017     FTntbe01L  IF   E           K DISK
000600020319     Fazorg01l  IF   E           K DISK
000700020319     Fficn41d   CF   E             WORKSTN
000800011221     D                SDS
000900011221     D PgmNam            *PROC
001000020319     D ficn41ds      E DS
001100891030     D KPJBA         E DS
001200020318     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001300020318     D  LIN                    1     90  0
001400020318     D                                     DIM(30)                              FIL. COMODO
001500020318     D ut§dse        E DS                  extname(ut§dse0f)
001600020318     D CNCR80        E DS
001700020319     D ficn70ds      E DS
001800121017     D dvpocont      E DS
001900121017     d DATA_VPOlimite  s              8s 0
002000121017     d DATA_LIMITE     s              8s 0
002100121017     d dataiso         s               d   datfmt(*iso)
002200121017     d dataeur         s               d   datfmt(*eur)
002300020319     D daut          E DS
002400020318     D WLBDAT          DS
002500020318     D  G02DAT                 1      8  0
002600020318     D  G02INV                 9     16  0
002700020318     D  G02ERR                17     17
002800020318     D  G02TGI                18     22  0
002900020308     D datiso          S               d
003000021111     D meno5           S               d
003100020308     D L1              S              3  0 DIM(30)
003200020308     D L6              S              3  0 DIM(30)
003300921222     C                   EXSR      DEFCAM
003400921222     C     *INKC         DOWEQ     '0'
003500020318     c* primo video
003600011221     C                   IF        *IN99
003700011221     C                   EVAL      *IN99 = *OFF
003800011221     C                   WRITE     CA81D1
003900011221     C                   EVAL      *IN99 = *ON
004000011221     C                   ENDIF
004100020318     c* se chiamato da menù emetto la prima videata di richiesta parametri
004200921222     C                   EXFMT     CA81D1
004300020318     c* fine
004400020318     C                   IF        *INKC
004500020318     c                   leave
004600020318     C                   ENDIF
004700020318     c* controlli
004800921222     C                   EXSR      CTRD1
004900020318     c   99              iter
005000020319     c* conferma
005100020319     C                   if        *inkf
005200020322     c                   move      pov           §41fil
005300020321     c                   movel     dettv         §41det
005400020322     c* per la stampa mandato pagamento sempre dettaglio
005500020322     c                   if        dtfttv <> 0
005600020322     c                   movel     'S'           §41det
005700020322     c                   end
005800020319     c                   movel(p)  ficn41ds      kpjbu
005900020319     c     knmus         ifeq      *all'1'
006000020329     c                   call      'FICN42C'
006100020319     c                   parm                    kpjba
006200020319     c                   else
006300020322     C                   MOVEL     'FN42'        KCOAZ
006400020319     C                   CALL      'BCH10'
006500020319     C                   PARM                    KPJBA
006600020319     c                   end
006700020319     c                   leave
006800020319     C                   ENDIF
006900020318     C                   ENDDO
007000921222     C                   SETON                                        LR
007100921222     C**************************************************************************
007200921222     C* CONTROLLI VIDEO1
007300020308     C**************************************************************************
007400921223     C     CTRD1         BEGSR                                                  *
007500921224     C                   SETOFF                                       99
007600020308     C* Filiale Padroncino
007700020321     c* per i secondi livelli il punto operativo è obbligatorio
007800020321     C     pov           IFEQ      0
007900020701     c*    cdu           andne     simfel
008000020701     c     SimPOU        andne     simfel
008100020321     C                   SETON                                        0199
008200020321     C                   End
008300020308     C* Verifico esistenza filiale padroncino: se REM su £6, se SIMFEL
008400020308     C* su £1
008500020321     C     pov           IFne      0
008600020701     C*    REM           IFEQ      'REM'
008700020701     C*    REMFIL        ANDGT     *ZEROS
008800020701     C                   IF        SimTpP = '2' OR SimTpP = *BLANK
008900020308     C     pov           LOOKUP    L6                                     20
009000020308     C  n20              SETON                                        0199
009100040309     C*                  ELSE
009200040309     C*    pov           LOOKUP    L1                                     20
009300040309     C* N20              SETON                                        0199
009400020308     C                   END
009500020321     C                   END
009600020319     c* decodifico
009700020321     c                   if        pov <> 0
009800020319     c     pov           chain     azorg01l
009900020319     c                   if        %found(azorg01l)
010000020319     c                   movel     orgdes        podesv
010100020319     c                   end
010200020321     c                   else
010300020321     c                   movel     *blanks       podesv
010400020321     c                   end
010500020319     C* Data conferma
010600020319    1C     dtfttv        IFGT      0
010700020319     C                   Z-ADD     dtfttv        G02DAT
010800020318     C                   MOVEL     *BLANK        G02ERR
010900020318     C                   CALL      'XSRDA8'
011000020318     C                   PARM                    WLBDAT
011100020318    2C     G02ERR        IFEQ      '1'
011200121017    2C     g02inv        orgE      DATA_vpoLIMITE
011300121017    2C*****g02inv        orgt      20120930
011400020319     C                   SETON                                        0299
011500020319    2C                   Else
011600020319     C                   move      G02dat        dtfttv
011700020318     C                   move      G02INV        datiso
011800020319     c                   move      datiso        §41ddc
011900020319    1C                   ENDIF
012000020308    1C                   ENDIF
012100020319     C* Data dal
012200020319    1C     daldds        IFGT      0
012300020319     C                   Z-ADD     daldds        G02DAT
012400020319     C                   MOVEL     *BLANK        G02ERR
012500020319     C                   CALL      'XSRDA8'
012600020319     C                   PARM                    WLBDAT
012700020319    2C     G02ERR        IFEQ      '1'
012800020319     C                   SETON                                        0399
012900020319    2C                   Else
013000020319     C                   move      G02dat        daldds
013100020319     C                   move      G02INV        datiso
013200021111     c* se oggi -
013300021111     c                   if        datiso < meno5
013400021112     C                   SETON                                        0699
013500021111     c                   else
013600020319     c                   move      datiso        §41dds
013700020319    1C                   ENDIF
013800021111    1C                   ENDIF
013900020319    1C                   ENDIF
014000020319     C* Data al
014100020319    1C     aldds         IFGT      0
014200020319     C                   Z-ADD     aldds         G02DAT
014300020319     C                   MOVEL     *BLANK        G02ERR
014400020319     C                   CALL      'XSRDA8'
014500020319     C                   PARM                    WLBDAT
014600020319    2C     G02ERR        IFEQ      '1'
014700020319     C                   SETON                                        0499
014800020319    2C                   Else
014900020319     C                   move      G02dat        aldds
015000020319     C                   move      G02INV        datiso
015100020319     c                   move      datiso        §41ads
015200020319    1C                   ENDIF
015300020319    1C                   ENDIF
015400020321     C*
015500020321     C                   IF        §41ads < §41dds
015600020321     C                   SETON                                        049903
015700020321    1C                   ENDIF
015800020321     C*
015900020321     C                   IF        dtfttv<>0 and (daldds<>0 or aldds<>0)
016000020321     C                   SETON                                        049903
016100020321     C                   SETON                                        02
016200020321    1C                   ENDIF
016300021127     C                   IF        dtfttv=0 and (daldds=0 or aldds=0)
016400021127     C                   SETON                                        049903
016500021127    1C                   ENDIF
016600020322     C                   IF        dtfttv=0 and daldds=0 and aldds=0
016700020322     C                   SETON                                        049903
016800020322     C                   SETON                                        02
016900020322    1C                   ENDIF
017000020614     c* Controllo se abilitato alla stampa delle rettifiche
017100020701     c*                  movel     cdu           i70pog
017200020701     c                   Z-ADD     SimPOU        i70pog
017300020614     c                   move      pov           i70pge
017400020614     c                   movel     knmus         i70prf
017500020614     c                   call      'FICN70R'
017600020614     c                   parm                    ficn70ds
017700020614     c                   movel     o70uni        daut
017800020614     c                   if        dtfttv <> 0
017900020614     c                   if        §autret <> 'S'
018000020614     c                   seton                                        0599
018100020614     c                   end
018200020614     c                   else
018300020614     c                   if        §autcrs <> 'S' and §autret <> 'S'
018400020614     c                   seton                                        0599
018500020614     c                   end
018600020614     c                   end
018700011221     C*
018800921223     C                   ENDSR
018900020308     C**************************************************************************
019000921222     C* INIZIALIZZAZIONE
019100020308     C**************************************************************************
019200921223     C     DEFCAM        BEGSR                                                  *
019300121017      *
019400121017     C     Ktbe          KLIST
019500121017     C                   KFLD                    tbecod
019600121017     C                   KFLD                    tbeke1
019700121017     C                   KFLD                    tbeke2
019800121017      *
019900921224     C                   SETOFF                                       98
020000121017      *
020100921223     C     *ENTRY        PLIST                                                  *
020200921223     C                   PARM                    KPJBA                          *
020300020308     C                   Z-ADD     1             CODUT
020400020308     C                   CALL      'X§PARUT'
020500020308     C                   PARM                    UT§DSE
020600020322     C                   MOVEL     RAGUT         RSUT
020700020308     C                   MOVEL     REC80         CNCR80
020800020321     c                   clear                   ficn41ds
020900020308     C***
021000020308     C* CARICO TABELLA FILIALI GESTITE £1
021100020308     C***
021200020308     C                   CLEAR                   DSUL06
021300020308     C                   MOVE      '£1'          D06COD
021400020308     C                   MOVEL     SIMFEL        D06KEY
021500020308     C                   MOVEL     ' '           D06TLA
021600020308     C                   MOVEL     DSUL06        KPJBU
021700020308     C                   CALL      'TRUL06R'
021800020308     C                   PARM                    KPJBA
021900020308     C                   MOVEL     KPJBU         DSUL06
022000020308     C                   MOVEA     LIN           L1
022100020321     C                   EXSR      PUL01
022200020321     C***
022300020321     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
022400020321     C***
022500020321     C                   CLEAR                   DSUL06
022600020321     C                   MOVE      '£6'          D06COD
022700020701     C*                  MOVEL     CDU           D06KEY
022800020701     C                   MOVEL     SimPOU        D06KEY
022900020321     C                   MOVEL     'L'           D06TLA
023000020321     C                   MOVEL     DSUL06        KPJBU
023100020321     C                   CALL      'TRUL06R'
023200020321     C                   PARM                    KPJBA
023300020321     C                   MOVEL     KPJBU         DSUL06
023400020321     C                   MOVEA     LIN           L6
023500021111     C*                                                    *
023600021111     c                   move      *date         meno5
023700021111     c                   subdur    5:*d          meno5
023800021111     C*                                                    *
023900121017     c* tabbella VPO record CONT
024000121017     C                   movel     'VPO'         tbecod
024100121017     C                   MOVEL(P)  'CONT'        tbeke1
024200121017     C                   clear                   tbeke2
024300121017     C                   clear                   dvpocont
024400121017     C     KTbe          CHAIN     Tntbe01l
024500121017     c                   if        %found(tntbe01l)
024600121017      **
024700121017     C                   movel     tbeuni        dvpocont
024800121017     C                   movel     §VPODFIR      data_VPOlimite
024900121017      **
025000121017     c                   else
025100121017      ** x NON trovato imposta fisso questa data 30/09/2012
025200121017     c                   clear                   dvpocont
025300121017     C                   movel     20120930      data_VPOlimite
025400121017     c                   end
025500121017     C*
025600921223     C                   ENDSR
025700020308     C*---------------------------------------------------------------*
025800020308     C* PULISCO 1 FORMATO --------------------------------------------*
025900020308     C*---------------------------------------------------------------*
026000020308     C     PUL01         BEGSR
026100020308     C                   CLEAR                   pov
026200020319     C                   CLEAR                   podesv
026300020319     C                   CLEAR                   dtfttv
026400020319     C                   CLEAR                   daldds
026500020319     C                   CLEAR                   aldds
026600020701     C*                  Z-ADD     CDU           pov
026700020701     C                   Z-ADD     SimPOU        pov
026800020321     c                   movel     'N'           dettv
026900020308     C                   ENDSR
