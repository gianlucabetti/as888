000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      **************************************************************************
000300941011      *  Nome programma:  FNLSC3R
000400941011      *  Descrizione   :  Stampa statistica partenze: controllo entrata
000500000000      *                   Programma esecuzione (stampa)
000600000000      *  Data creazione:  24 NOV 1993
000700000000      **************************************************************************
000800941011      *  Il pgm puo' essere lanciato dal filtro FNLSC0R oppure tramite
000900000000      *  le funzioni giornaliere, insieme ad altre stampe.
001000000000      *  La statistica Controllo Entrate non deve essere elaborata in
001100000000      *  giorni festivi.
001200000000      **************************************************************************
001300950210      *  ??PASSAGGIO PARAMETRI:?
001400950216      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001500950210      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001600970207      *  DS1FPG   STATISTICA SELEZIONATA
001700950210      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001800950210      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001900950210      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
002000950210      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
002100950210      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
002200950210      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002300950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
002400950210      **************************************************************************
002500000000      *  Indicatori utilizzati:
002600000000      *  30   ON    Overflow del file di stampa
002700000000      *  31         Numerazione delle pagine in stampa
002800000000      *  32         In alcuni cicli
002900000000      *  33         Alcune chain o lokup
003000000000      *  34         Nell'elaborazione: spedizione non controllata
003100000000      *  35
003200010417      *  36   ON    BOLLA DISK C
003300950213      *  37         DS1RIS=' ' ;  solo elaborazione senza stampa tabulato
003400950213      *  39   ON    DS1FEL=0 ;  non richiesta area specifica
003500000000      *  40         Usato nel file di stampa
003600000000      *  41         Usato nel file di stampa
003700000000      *  42         Usato nel file di stampa
003800000000      *  43         Usato nel file di stampa
003900000000      *  44         Usato nel file di stampa
004000000000      *  45         Usato nel file di stampa
004100010417      *  98         DI COMODO
004200000000      *  99         Generico, utilizzabile a piacere
004300000000      **************************************************************************
004400941122     FAZCLN01L  IF   E           K DISK    USROPN
004500000000     FAZORG01L  IF   E           K DISK
004600000000     FCNACO00F  IF   E           K DISK
004700021203     F*FNAPD01L  IF   E           K DISK
004800021203     FFIAPD01L  IF   E           K DISK
004900020607     FFNBLP70L  IF   E           K DISK    USROPN
005000941011     FFNBLT01L  IF   E           K DISK
005100140618     FFNBRV05L  IF   E           K DISK
005200941011     FFNSCE01L  UF A E           K DISK
005300000000     FTABEL00F  IF   E           K DISK
005400021016     fTntbe01l  uf   e           k Disk
005500941011     FFNLSC3P   O    E             PRINTER OFLIND(*IN30)
005600941011     F                                     USROPN
005700000000      *
005800000000     D L1              S              3  0 DIM(30)
005900020418     D L6              S              3  0 DIM(30)
006000950213     D LV2             S              3  0 DIM(30)
006100000616     D LIN             S              3  0 DIM(30)
006200000616     D SVLIN           S              3  0 DIM(30)
006300950213     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
006400000000     D RA1             S             48    DIM(999)
006500000000     D RA2             S             48    DIM(999)
006600000000     D RA3             S             48    DIM(999)
006700010417     D RA4             S             48    DIM(999)
006800010417     D RA5             S             48    DIM(999)
006900010420     D RA6             S             48    DIM(999)
007000010420     D DSK             S             12    DIM(7) CTDATA PERRCD(1)
007100000000     D GME             S              1    DIM(31)
007200010417     D TMC             S              2    DIM(100)
007300010417     D TMT             S              1    DIM(100)
007400020418
007500020418     d sav_t06         S              3  0 dim(30)
007600020418
007700020418     d wc              s              3  0
007800021016
007900021016     d kTbeCod         s                   Like(TbeCod)
008000021016     d kTbeKe1         s                   Like(TbeKe1)
008100020418
008200021203     d kpdpdr          S                   LIKE(APDpdr)
008300021203     d kpdtip          S                   LIKE(APDtip)
008400050829     d klin            S                   LIKE(ds1lnp)
008500000000      *
008600000000     D KPJBA         E DS
008700000000     D DS3A          E DS
008800000000     D DS3L          E DS
008900040922     D*DS5C          E DS
009000970707     D DS5P          E DS
009100010412     D DS1B          E DS
009200010417     D DS3C          E DS
009300010417     D OG143         E DS
009400000000     D CNCR80        E DS
009500000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009600981123     D  TCU                  398    697
009700981123     D                                     DIM(50)
009800981123     D  KCU                  698    847P 0
009900981123     D                                     DIM(50)
010000981123     D                                     PACKEVEN
010100981123     D TCUDS           DS
010200981123     D  F1                     1      1
010300981123     D  F3                     3      3
010400981123     D  F2                     2      2
010500981123     D  F4                     4      4
010600981123     D  F56                    5      6
010700000000      * DS per il passaggio parametri
010800950213     D DSLS01        E DS
010900950213     D                 DS
011000941011     D* DATA INIZIALE AAAA/MM/GG
011100950213     D  DATADA                 1      8  0
011200950213     D  $GI                    7      8  0
011300950213     D  AAINI                  1      4  0
011400950213     D  MMINI                  5      6  0
011500950213     D  MGINI                  5      8  0
011600040922     D WLBDAT          DS                  INZ
011700040922     D  G02DAT                 1      8  0
011800040922     D  G02INV                 9     16  0
011900040922     D  G02ERR                17     17
012000040922     D  G02TGI                18     22  0
012100950213     D* DS PER TRUL06R - CARICAMENTO £X
012200950213     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012300950314     D  T06                    1     90  0
012400950314     D                                     DIM(30)
012500000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
012600000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
012700021016      * Ds date statistiche
012800021016     d dSdf          e ds
012900000000      *
013000000000     C     *ENTRY        PLIST
013100000000     C                   PARM                    KPJBA
013200950213     C                   MOVEL     KPJBU         DSLS01
013300950213     C                   Z-ADD     DS1ADA        DATADA
013400000000      *
013500000000     C     CNACOK        KLIST
013600000000     C                   KFLD                    ACOKUT
013700000000     C                   KFLD                    ACOKCC
013800000000     C                   KFLD                    ACOKSC
013900000204     C     FNBRVK        KLIST
014000000000     C                   KFLD                    BRVNPG
014100000000     C                   KFLD                    BRVLNP
014200000000     C                   KFLD                    BRVLNA
014300000000     C                   KFLD                    BRVNRS
014400000000     C                   KFLD                    BRVNSC
014500941011     C     FNBLTK        KLIST
014600000000     C                   KFLD                    BLPAAS
014700000000     C                   KFLD                    BLPLNP
014800000000     C                   KFLD                    BLPNRS
014900000000     C                   KFLD                    BLPNSP
015000941011     C     FLSC1K        KLIST
015100000000     C                   KFLD                    SCEFLE
015200000000     C                   KFLD                    SCELNP
015300941011     C     FNBLPK        KLIST
015400020607     C                   KFLD                    LIN(XX)
015500000000     C                   KFLD                    AAINI
015600000000     C                   KFLD                    MGINI
015700941122     C     AZCLNK        KLIST
015800000000     C                   KFLD                    SIMFEL
015900941122     C                   KFLD                    CLNTFA
016000941122     C                   KFLD                    AAINI
016100000000     C                   KFLD                    MMINI
016200021016
016300021016     c     Ktbe          Klist
016400021016     c                   Kfld                    kTbeCod
016500021016     c                   Kfld                    kTbeKe1
016600021203     C     Kfapd         klist
016700021203     C                   kfld                    kpdtip
016800021203     C                   kfld                    kpdpdr
016900050829     c     Ksce          Klist
017000050829     c                   Kfld                    ds1fel
017100050829     c                   Kfld                    klin
017200000000      *
017300000616     C     *LIKE         DEFINE    DS1RIS        SAVRIS
017400000616     C     *LIKE         DEFINE    BLPLNP        $FILPR
017500941011     C     *LIKE         DEFINE    BLPPDR        $PADPR
017600010417     C     *LIKE         DEFINE    KKEY          KEYSAV
017700010524     C*
017800010524     C* CONTROLLO SE DEVE STAMPARE A DETTAGLIO O A TOTALI
017900010524     C                   SETOFF                                           13
018000010524     C     DS1FL5        IFEQ      'S'
018100010524     C                   SETON                                        13
018200010524     C                   ENDIF
018300010524     C                   SETOFF                                       90
018400010417     C*
018500010417     C* CARICO TABELLA CTM DISK C
018600010417     C                   MOVE      '1B'          KCOD
018700010417     C                   Z-ADD     1             CODUT
018800010417     C                   CLEAR                   II
018900010417     C     KTAB2         SETLL     TABEL00F
019000010417     C     KTAB2         READE     TABEL00F                               98
019100010417     C     *IN98         DOWEQ     *OFF
019200010417     C                   MOVEL     TBLUNI        DS1B
019300010417     C     §1BDKC        IFEQ      'B'
019400010417     C     §1BDKC        OREQ      'C'
019500010417     C                   ADD       1             II                3 0
019600010417     C                   MOVEL     TBLKEY        TMC(II)
019700010417     C                   MOVEL     §1BDKC        TMT(II)
019800010417     C                   ENDIF
019900010417     C     KTAB2         READE     TABEL00F                               98
020000010417     C                   ENDDO
020100000000      **************************************************************************
020200000000      *              M A I N      L I N E
020300000000      **************************************************************************
020400950213     C                   Z-ADD     1             CODUT
020500000000      *
020600000000      * Reperimento tabella per trasmissione archivio statistiche
020700040922     C***                MOVE      '5C'          KCOD
020800040922     C***                MOVE      '1       '    KKEY
020900040922     C***  KTAB1         CHAIN     TABEL00F                           99
021000040922     C**N99              MOVEL     TBLUNI        DS5C
021100040922     C** 99              CLEAR                   DS5C
021200000000      *
021300000000      * Accendo il 37 se non desidero la stampa
021400950213     C     DS1RIS        COMP      *BLANKS                                37
021500000000      *
021600950213      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
021700950213     C                   EXSR      COMUNE
021800981123     C*
021900981123     C*--- RICERCA CAPOCONTI
022000981123     C                   DO        50            X                 2 0
022100981123     C                   MOVE      TCU(X)        TCUDS
022200981123     C     F56           CABNE     'CG'          END1
022300981123     C     F4            COMP      '1'                                    21
022400981123     C     F4            COMP      '2'                                    22
022500981123     C     F4            COMP      '3'                                    23
022600981123     C     F4            COMP      '6'                                    27
022700981123     C** 1 CLIENTI   21
022800981123     C** 2 FORNITORI 22
022900981123     C** 3 AGENTI    23
023000981123     C     F3            COMP      '0'                                242425
023100981123     C     F3            COMP      'I'                                    26
023200981123     C** 0 ITALIA   25
023300981123     C** 1 ESTERO   24
023400981123     ** I CAPO CONTO IVA
023500981123     C   21
023600981123     CAN 24              Z-ADD     KCU(X)        KCE               4 0
023700981123     C   21
023800981123     CAN 25              Z-ADD     KCU(X)        KCI               4 0
023900981123     C   22
024000981123     CAN 24              Z-ADD     KCU(X)        KFE               4 0
024100981123     C   22
024200981123     CAN 25              Z-ADD     KCU(X)        KFI               4 0
024300981123     C   23
024400981123     CAN 24              Z-ADD     KCU(X)        KAE               4 0
024500981123     C   23
024600981123     CAN 25              Z-ADD     KCU(X)        KAI               4 0
024700981123     C   26              Z-ADD     KCU(X)        KIVA              4 0
024800981123     C   27              Z-ADD     KCU(X)        KBNA              4 0
024900981123     C     END1          TAG
025000981123     C                   END
025100981123     C                   SETOFF                                       212223
025200981123     C                   SETOFF                                       242526
025300981123     C                   SETOFF                                       27
025400000000      *
025500000000      * Aggiorno la data ultima elaborazione
025600950213     C     DS1RIS        IFNE      'R'
025700021016
025800021016     c                   Eval      kTbeCod = 'SDF'
025900021016     c                   Movel(p)  Simfel        kTbeKe1
026000021016     c     Ktbe          Chain     Tntbe01l
026100021016     c                   If        %Found(Tntbe01l)
026200021016     c                   Movel     TbeUni        dSdf
026300021016     c                   If        §SdfEntP = *Zeros or §SdfEntP > Ds1Ada
026400021016     c                   Z-Add     Ds1Ada        §SdfEntP
026500021016     c                   EndIf
026600021016     c                   If        §SdfEntU < Ds1Ada
026700021016     c                   Z-Add     Ds1Ada        §SdfEntU
026800021016     c                   EndIf
026900021016     c                   Movel     dSdf          TbeUni
027000021016     c                   Update    Tntbe000
027100021016     c                   EndIf
027200000000     C                   ENDIF
027300000000      *
027400000000     C                   SETON                                        LR
027500020212     C*****************************************************************
027600020212     C*  PARTE COMUNE STAMPE GIORNALIERE
027700020212     C*
027800020212     C*  . REPERIMENTO 'SIMFEL'
027900020212     C*  . CARICAMENTO '£1'
028000020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
028100020212     C*  . CODA DI STAMPA ('£OUTQ')
028200020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
028300020212     C*  . MODULO PER LA STAMPA ('£MOD')
028400020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
028500020212     C*****************************************************************
028600020212     C     COMUNE        BEGSR
028700020212     C*
028800020212     C*****************************************************************
028900020212     C*       RIEPILOGO INDICATORI:
029000020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
029100020212     C* 11    -           STAMPA PER 1L
029200020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
029300020212     C*****************************************************************
029400020212     C                   Z-ADD     1             CODUT
029500020212     C*
029600020212     C* Reperimento parametri aziendali
029700020212     C                   CALL      'X§PARUT'
029800020212     C                   PARM                    UT§DSE
029900020212     C                   MOVEL     RAGUT         RSUT             20
030000020212     C                   MOVEL     REC80         CNCR80
030100020212     C*
030200020212     C* Chiave di accesso per il file tabelle
030300020212     C     KTAB1         KLIST
030400020212     C                   KFLD                    CODUT
030500020212     C                   KFLD                    KCOD
030600020212     C                   KFLD                    KKEY
030700020212     C     KTAB2         KLIST
030800020212     C                   KFLD                    CODUT
030900020212     C                   KFLD                    KCOD
031000020212     C*
031100020212     C     *LIKE         DEFINE    TBLCOD        KCOD
031200020212     C     *LIKE         DEFINE    TBLKEY        KKEY
031300020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
031400020212     C*
031500020212     C* DETERMINO DATA E ORA DI ESECUZIONE
031600020212     C                   TIME                    WTIME            14 0
031700020212     C                   MOVE      WTIME         WDATE             8 0
031800020212     C                   MOVEL     WTIME         UTIME             6 0
031900040922     c*
032000040922     C                   CLEAR                   WLBDAT
032100040922     C                   MOVE      Wtime         G02DAT
032200040922     C                   CALL      'XSRDA8'
032300040922     C                   PARM                    WLBDAT
032400040922     C                   Z-ADD     G02INV        dateu             8 0
032500020212     C*
032600020418     C* CARICO '£1'
032700020212     C                   CLEAR                   KPJBU
032800020212     C                   CLEAR                   DSUL06
032900020212     C                   MOVE      '£1'          D06COD
033000020212     C                   MOVEL     SIMFEL        D06KEY
033100050802     C                   MOVE      'S'           D06KEY
033200050802     C                   MOVEL     ds1ada        D06drf
033300020212     C                   MOVEL     DSUL06        KPJBU
033400020212     C                   CALL      'TRUL06R'
033500020212     C                   PARM                    KPJBA
033600020212     C                   MOVEL     KPJBU         DSUL06
033700020418     c                   movea     t06           sav_t06
033800020418
033900020418      * se in arrivo carico la £6
034000020418 b1  c                   if        ds1tbo = 'A'
034100020418     c                   clear                   wc
034200020418 b2  c                   do        30            wa
034300020422     c                   if        sav_t06(wa) = *zeros
034400020422     c                   leave
034500020422     c                   endif
034600020418     c                   clear                   kpjbu
034700020418     c                   clear                   dsul06
034800020418     c                   eval      d06cod = '£6'
034900020418     c                   movel     sav_t06(wa)   d06key
035000050802     C                   MOVE      'S'           D06KEY
035100020418     c                   eval      d06esc = 'S'
035200050802     C                   MOVEL     ds1ada        D06drf
035300020419     c                   movel     dsul06        kpjbu
035400020418     c                   call      'TRUL06R'
035500020418     c                   parm                    kpjba
035600020418     c                   movel     kpjbu         dsul06
035700020418 b3  c                   do        30            wb
035800020418     c                   if        t06(wb) = *zeros
035900020418     c                   leave
036000020418     c                   endif
036100020418     c                   add       1             wc
036200020418     c                   z-add     t06(wb)       l6(wc)
036300020418 e3  c                   enddo
036400020418 e2  c                   enddo
036500020212     C*
036600020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
036700020212     C                   Z-ADD     *ZERO         WB
036800020418 b2  C                   DO        29            WA
036900020418     C     sav_T06(WA)   LOOKUP    L6                                     30
037000020212     C  N30              ADD       1             WB
037100020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
037200020418 e2  C                   ENDDO
037300020418 x1  C                   ELSE
037400020418     C                   MOVEA     sav_T06       L1
037500020418 e1  C                   ENDIF
037600020212     C*
037700020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
037800020212     C                   MOVEA     L1            LIN
037900020212     C                   CLEAR                   L1
038000020212     C                   Z-ADD     *ZERO         WB                3 0
038100020212     C     1             DO        30            WPT               3 0
038200020212     C     LIN(WPT)      IFNE      0
038300020212     C                   CLEAR                   FNLSC4
038400020212     C                   Z-ADD     LIN(WPT)      IC4LIN
038500020212     C                   MOVE      'B'           IC4FLG
038600020212     C                   MOVEL     FNLSC4        KPJBU
038700020212     C                   CALL      'FNLSC4R1'
038800020212     C                   PARM                    KPJBA
038900050802     C                   PARM                    ds1ada
039000020212     C                   MOVEL     KPJBU         FNLSC4
039100020212     C     OC4ESE        IFEQ      'S'
039200020212     C                   ADD       1             WB
039300020212     C                   MOVE      LIN(WPT)      L1(WB)
039400020212     C                   ENDIF
039500020212     C                   ENDIF
039600020212     C                   ENDDO
039700070103     C*
039800070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
039900070103     C                   Z-ADD     *ZERO         WB                3 0
040000070103     C                   DO        29            WA                3 0
040100070103     C     L1(WA)        IFNE      *ZEROS
040200070103     C     L1(WA)        ANDNE     SIMFEL
040300070103     C                   ADD       1             WB
040400070103     C                   Z-ADD     L1(WA)        LV2(WB)
040500070103     C                   ENDIF
040600070103     C                   ENDDO
040700020212     C*
040800020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
040900020212     C*
041000020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
041100020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
041200020212     C* la stampa e' lanciata da menu
041300020212     C                   MOVEL     '3L'          KCOD
041400020212     C                   MOVEL     DS1PGM        KKEY
041500020212     C                   MOVE      DS1FPG        KKEY
041600020212     C     KTAB1         CHAIN     TABEL00F                           30
041700020212     C  N30              MOVEL     TBLUNI        DS3L
041800020212     C   30              MOVEL     'F'           §3LD1L
041900020212     C   30              MOVEL     ' '           §3LD2L
042000020212     C   30              MOVEL     '*STD    '    §3LDST
042100020212     C*
042200020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
042300020212     C                   MOVEL     §3LDST        £MOD             10
042400020212     C                   CLEAR                   LIN
042500020212     C*
042600020212     C* Verifico chi ha lanciato la stampa
042700020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
042800020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
042900020212     C* DS1FGS<>0  --->  si proviene da menu
043000020212     C     DS1FGS        IFEQ      0
043100020212     C*
043200020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
043300020212     C* lancio, voglio la stampa;  invece
043400020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
043500020212     C*
043600020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
043700020212     C* creare stampa per 1°lvl. (se stessa)
043800020212     C     DS1LNP        IFEQ      SIMFEL
043900020212     C*
044000020212     C* Il flag §3LD1L ha i seguenti significati:
044100020212     C*    ' ' = tutti i dati
044200020212     C*    '1' = solo filiale di 1' livello
044300020212     C     §3LD1L        IFEQ      '1'
044400020212     C                   SETON                                        11
044500020212     C                   END
044600020212     C*
044700020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
044800020212     C* nella stampa
044900020212     C*
045000020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
045100020212     C  N11              MOVEA     L1            LIN
045200020212     C   11              MOVEA     SIMFEL        LIN(1)
045300020212     C*
045400020212     C* IMPOSTO LA STAMPANTE:
045500020212     C* STAMPANTE TABULATI
045600020212     C                   MOVEL     '5P'          KCOD
045700020212     C                   MOVEL     'T'           W004A             4
045800020212     C                   MOVE      LIN(1)        W004A
045900020212     C                   MOVEL(P)  W004A         KKEY
046000020212     C     KTAB1         CHAIN     TABEL00F                           99
046100020212     C     *IN99         IFEQ      *OFF
046200020212     C                   MOVEL     TBLUNI        DS5P
046300020212     C                   MOVEL     §5POTQ        £OUTQ            10
046400020212     C                   END
046500020212     C*
046600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
046700020212     C                   EXSR      LEGGI
046800020212     C*
046900020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
047000020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
047100020212     C                   ELSE
047200020212     C*
047300020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
047400020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
047500020212     C*
047600020212     C* Il significato del fla §3LD2L e' il seguente:
047700020212     C*    ' ' = tutti i dati
047800020212     C*    'F' = per ogni 2' livello
047900020212     C     §3LD2L        IFEQ      'F'
048000020212     C                   SETON                                        14
048100020212     C                   ENDIF
048200020212     C*
048300020212     C                   Z-ADD     1             WB
048400020212     C                   MOVEA     *ZEROS        LIN
048500020212     C  N14              MOVEA     L1            LIN
048600020212     C*
048700020212     C* Lancio per tutti i 2° lvl
048800020212     C     DS1LNP        IFEQ      0
048900020212     C*
049000020212     C* LV2 contiene tutte le filiali di 2° lvl
049100020212     C     LV2(WB)       DOWNE     0
049200020212      *
049300020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
049400020212     C                   CLEAR                   FNLSC4
049500020212     C                   Z-ADD     LV2(WB)       IC4LIN
049600020212     C                   MOVEL     FNLSC4        KPJBU
049700020212     C                   CALL      'FNLSC4R1'
049800020212     C                   PARM                    KPJBA
049900050802     C                   PARM                    ds1ada
050000020212     C                   MOVEL     KPJBU         FNLSC4
050100020212      *
050200020212     C     OC4PT1        IFEQ      'S'
050300020212     C     §3LPT         ANDEQ     'N'
050400020212     C                   GOTO      NOLEGG
050500020212     C                   ENDIF
050600020212      *
050700020212     C     OC4EXP        IFEQ      'S'
050800020212     C     §3LEXP        ANDEQ     'N'
050900020212     C                   GOTO      NOLEGG
051000020212     C                   ENDIF
051100020212      *
051200020212     C     OC4DPD        IFEQ      'S'
051300020212     C     §3LDPD        ANDEQ     'N'
051400020212     C                   GOTO      NOLEGG
051500020212     C                   ENDIF
051600020212      *
051700020212     C     OC4BAR        IFEQ      'S'
051800020212     C     §3LBAR        ANDEQ     'N'
051900020212     C                   GOTO      NOLEGG
052000020212     C                   ENDIF
052100020212      *
052200020212     C     OC4FED        IFEQ      'S'
052300020212     C     §3LFED        ANDEQ     'N'
052400020212     C                   GOTO      NOLEGG
052500020212     C                   ENDIF
052600020212      *
052700020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
052800020212     C*  dati degli altri 2° lvl
052900020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
053000020212     C   14              MOVEL     LV2(WB)       LIN(1)
053100020212     C*
053200020212     C* IMPOSTO LA STAMPANTE
053300020212     C* STAMPANTE TABULATI
053400020212     C                   MOVEL     '5P'          KCOD
053500020212     C                   MOVEL     'T'           W004A             4
053600020212     C                   MOVE      LV2(WB)       W004A
053700020212     C                   MOVEL(P)  W004A         KKEY
053800020212     C     KTAB1         CHAIN     TABEL00F                           99
053900020212     C     *IN99         IFEQ      *OFF
054000020212     C                   MOVEL     TBLUNI        DS5P
054100020212     C                   MOVEL     §5POTQ        £OUTQ            10
054200020212     C                   END
054300020212     C*
054400020212     C* Compongo il modulo di stampa con il codice della filiale
054500020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
054600020212     C*
054700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
054800020212     C                   EXSR      LEGGI
054900020212     C*
055000020212     C     NOLEGG        TAG
055100020212     C*
055200020212     C                   ADD       1             WB
055300020212     C                   ENDDO
055400020212     C*
055500020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
055600020212     C                   ELSE
055700020212     C*
055800020212     C* Lancio solo per la 2° lvl richiesta
055900020212     C   14              MOVEL     DS1LNP        LIN(1)
056000020212     C*
056100020212     C* STAMPANTE
056200020212     C* STAMPANTE TABULATI
056300020212     C                   MOVEL     '5P'          KCOD
056400020212     C                   MOVEL     'T'           W004A             4
056500020212     C                   MOVE      DS1LNP        W004A
056600020212     C                   MOVEL(P)  W004A         KKEY
056700020212     C     KTAB1         CHAIN     TABEL00F                           99
056800020212     C     *IN99         IFEQ      *OFF
056900020212     C                   MOVEL     TBLUNI        DS5P
057000020212     C                   MOVEL     §5POTQ        £OUTQ            10
057100020212     C                   END
057200020212     C*
057300020212     C* MODULO STAMPA
057400020212     C******               MOVE DS1LNP    £MOD
057500020212     C*
057600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
057700020212     C                   EXSR      LEGGI
057800020212     C*
057900020212     C                   ENDIF
058000020212     C                   ENDIF
058100020212     C*
058200020212     C                   ELSE
058300020212     C*
058400020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
058500020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
058600020212     C     DS1LNP        IFEQ      0
058700020212     C                   MOVEA     L1            LIN
058800020212     C* ...altrimenti elaboro solo se stessa
058900020212     C                   ELSE
059000020212     C                   MOVEL     DS1LNP        LIN(1)
059100020212     C                   ENDIF
059200020212     C*
059300020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
059400020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
059500020212     C* PRENDO QUELLA PRESENTE IN KPJBA
059600020212     C     DS1FGS        IFEQ      SIMFEL
059700020212     C                   MOVEL     KQOTD         £OUTQ
059800030627     C**!!!              MOVEL     KQOTD         £OUTQF
059900020212     C                   ELSE
060000020212     C* STAMPANTE TABULATI
060100020212     C                   MOVEL     '5P'          KCOD
060200020212     C                   MOVEL     'T'           W004A             4
060300020212     C                   MOVE      DS1FGS        W004A
060400020212     C                   MOVEL(P)  W004A         KKEY
060500020212     C     KTAB1         CHAIN     TABEL00F                           99
060600020212     C     *IN99         IFEQ      *OFF
060700020212     C                   MOVEL     TBLUNI        DS5P
060800020212     C                   MOVEL     §5POTQ        £OUTQ            10
060900020212     C                   END
061000020212     C******               MOVE DS1FGS    £MOD             TIPO MODULO
061100020212     C                   ENDIF
061200020212     C*
061300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
061400020212     C                   EXSR      LEGGI
061500020212     C                   ENDIF
061600020212     C*
061700020212     C                   ENDSR
061800000000      **************************************************************************
061900000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
062000000000      **************************************************************************
062100000000     C     LEGGI         BEGSR
062200000616     C                   CLEAR                   SAVRIS
062300000000      *
062400000000      * Come prima cosa devo verificare se giornata festiva, poiche' in tal
062500000000      * caso non ha significato la statistica controllo entrate
062600000000      * solo per ELABORAZIONE o  per stampe giornaliere
062700000000      * Ho dovuto mettere il controllo qui e non nel main perche' serve SIMFEL
062800950213     C     DS1FGS        IFEQ      0
062900950213     C     DS1RIS        ORNE      'R'
063000941122     C                   OPEN      AZCLN01L
063100000000      *
063200941122     C                   Z-ADD     *ZERO         CLNTFA
063300941122     C     AZCLNK        CHAIN     AZCLN01L                           99
063400941122     C                   CLOSE     AZCLN01L
063500000000     C     *IN99         IFEQ      *OFF
063600941122     C                   MOVEA     CLNMAT        GME
063700000000     C     GME($GI)      CABEQ     'F'           ENDLEG
063800000000     C                   ENDIF
063900000000     C                   ENDIF
064000000000      *
064100000000      * Elimino dall'archivio statistiche eventuali vecchie registrazioni
064200000000      * Solo se richiesta elaborazione ('S'=STAMPA)
064300950213     C     DS1RIS        IFNE      'R'
064400000000     C                   Z-ADD     SIMFEL        SCEFLE
064500000616     C**
064600000616     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
064700000616     C                   CLEAR                   KPJBU
064800000616     C                   CLEAR                   DSUL06
064900000616     C                   MOVE      '£1'          D06COD
065000000616     C                   MOVEL     SIMFEL        D06KEY
065100050802     C                   MOVE      'S'           D06KEY
065200050802     C                   MOVEL     ds1ada        D06drf
065300000616     C                   MOVEL     DSUL06        KPJBU
065400000616     C                   CALL      'TRUL06R'
065500000616     C                   PARM                    KPJBA
065600000616     C                   MOVEL     KPJBU         DSUL06
065700000616     C**
065800000616     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
065900000616     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
066000000616     C*  CHE USO ALLA FINE
066100000616     C     DS1RIS        IFNE      ' '
066200000616     C                   MOVEA     LIN           SVLIN
066300000616     C                   MOVEL     DS1RIS        SAVRIS
066400000616     C                   CLEAR                   DS1RIS
066500000616     C                   ENDIF
066600000616     C                   MOVEA     T06           LIN
066700000000      *
066800000000      * Elimino solo le linee della LIN (che dovro' poi elaborare)
066900000000     C                   Z-ADD     1             XX
067000000000     C     LIN(XX)       DOWGT     *ZERO
067100000000      *
067200000000     C                   Z-ADD     LIN(XX)       SCELNP
067300941011     C     FLSC1K        SETLL     FNSCE01L
067400000000     C                   DO        *HIVAL
067500941011     C     FLSC1K        READE     FNSCE01L                               99
067600950213     C  N99SCEDRE        IFGE      DS1ADA
067700950213     C     SCEDRE        ANDLE     DS1AAL
067800941011     C                   DELETE    FNSCE000
067900000000     C                   ENDIF
068000000000     C  N99              ENDDO
068100000000      *
068200000000     C                   ADD       1             XX
068300000000     C                   ENDDO
068400000000     C                   ENDIF
068500000000      *
068600000000     C                   Z-ADD     *ZERO         $F                4 0
068700000000     C                   Z-ADD     *ZERO         $G                4 0
068800000000     C                   Z-ADD     *ZERO         $H                4 0
068900000000     C                   Z-ADD     *ZERO         $I                4 0
069000010417     C                   Z-ADD     *ZERO         $L                4 0
069100010420     C                   Z-ADD     *ZERO         $M                4 0
069200000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
069300950213      *
069400950213      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
069500950213     C                   MOVEA     £MOD          CMD(33)
069600950213     C                   MOVEA     £OUTQ         CMD(51)
069700000000      *
069800000000      * Eseguo OVRPRTF sul file di stampa
069900950213     C                   Z-ADD     82            LUNG             15 5
070000950213     C                   MOVEL     *BLANKS       COMMAN           82
070100000000     C                   MOVEA     CMD           COMMAN
070200000000     C                   CALL      'QCMDEXC'                            H1
070300000000     C                   PARM                    COMMAN
070400000000     C                   PARM                    LUNG
070500000000      *
070600000000     C                   Z-ADD     1             $C                3 0
070700941011     C  N37              OPEN      FNLSC3P
070800000000     C                   SETOFF                                       31
070900000000      *
071000000000      * Stampo la testata di stampa
071100941011     C  N37              WRITE     LSC3T1
071200941011     C  N37              WRITE     LSC3R1
071300941011     C  N37              WRITE     LSC3T2
071400941011     C  N37              WRITE     LSC3R1
071500000000     C  N37              SETOFF                                       30
071600000000     C                   Z-ADD     *ZERO         $PRIMA            1 0
071700000000     C                   Z-ADD     *ZERO         $PRIM1            1 0
071800000000     C                   Z-ADD     *ZERO         $FILPR
071900000000     C                   Z-ADD     *ZERO         $PADPR
072000000619     C* STAMPO POI DOPO
072100000619     C     SAVRIS        IFNE      ' '
072200000619     C                   SETON                                        37
072300000619     C                   ENDIF
072400000000      *
072500000000      * Se scelta solo la ristampa chiamo la routine apposita
072600990721    1C     DS1RIS        IFNE      'R'
072700020607     C                   OPEN      FNBLP70L
072800000000      *
072900000000      * Per ogni linea presente in LIN leggo le bolle
073000000000     C                   Z-ADD     1             XX                3 0
073100990721    3C     LIN(XX)       DOWNE     *ZERO
073200000000      *
073300000000      * Leggo il file delle bolle in partenza
073400000000     C                   SETOFF                                       32
073500020607     C     FNBLPK        SETLL     FNBLP70L
073600990721    3C                   DO        *HIVAL
073700020607     C     FNBLPK        READE     FNBLP70L                               32
073800000000      *
073900000000      * Escludo le bolle senza segnacolli (BLPNCD e BLPNCA) che, pero', non
074000000000      * hanno segnacolli non sequenziali (BLPFNS<>'S')
074100990721    4C  N32BLPNCD        IFGT      *ZERO
074200000000     C     BLPNCA        ORGT      *ZERO
074300000000     C     BLPFNS        OREQ      'S'
074400000000      *
074500000000      * Escludo le bolle in sospensione (data ritiro < data spedizione)
074600941011     C                   Z-ADD     BLPMGS        $DTSPE            8 0
074700000000     C                   MOVEL     BLPAAS        $DTSPE
074800960717     C*****      BLPDRT    IFGE $DTSPE                     DATA RITIRO
074900000000      *
075000000000      * Decodifico il codice bolla poiche' devo escludere i recuperi
075100950213     C                   MOVE      '3A'          KCOD
075200950213     C                   MOVE      *BLANKS       KKEY
075300950213     C                   MOVEL     BLPCBO        KKEY
075400950213     C     KTAB1         CHAIN     TABEL00F                           33
075500000000     C  N33TBLFLG        COMP      *BLANK                             3333
075600000000     C  N33              MOVEL     TBLUNI        DS3A
075700000000     C   33              CLEAR                   DS3A
075800990721    5C     §3ARBL        IFNE      'R'
075900990721     C* ESCLUDO LE BOLLE SENZA PADRONCINO
076000990721     C     BLPPDR        ANDGT     0
076100000000      *
076200000000      * Verifico se la bolla letta appartiene al padroncino precedente
076300990721    6C     BLPPDR        IFNE      $PADPR
076400990721    7C     $PRIMA        IFNE      *ZERO
076500000000     C                   EXSR      TOTPAD
076600000000     C                   EXSR      AZZPAD
076700990721    7C                   ENDIF
076800000000      *
076900000000      * Decodifico il codice padroncino
077000021203     C*    BLPPDR        CHAIN     FNAPD01L                           33
077100021205     C                   move      BlpPdr        kpdpdr
077200021203     C                   move      'A'           kpdtip
077300021203     C     kfapd         chain     fiapd01L                           33
077400020517     C*!N33APDATB        COMP      *BLANK                             3333
077500000000      *
077600000000     C                   Z-ADD     1             $PRIMA
077700941011     C                   Z-ADD     BLPPDR        $PADPR
077800000000      *
077900000000     C                   SETOFF                                       35
078000000000      *
078100990721    6C                   ENDIF
078200000000      *
078300000000      * Se cambia la filiale decodifico e stampo i totali
078400990721    6C     BLPLNP        IFNE      $FILPR
078500000000     C                   SETON                                        40
078600990721    7C     $PRIM1        IFNE      *ZERO
078700000000     C                   EXSR      TOTFIL
078800000000     C                   EXSR      AZZFIL
078900990721    7C                   ENDIF
079000000000     C                   Z-ADD     1             $PRIM1
079100000000     C                   Z-ADD     BLPLNP        $FILPR
079200000000      *
079300000000      * Decodifico il codice filiale
079400000000     C     BLPLNP        CHAIN     AZORG01L                           33
079500000000     C                   SETOFF                                       40
079600010417     C                   CLEAR                   OG143
079700010417     C  N33              MOVEL     ORGDE3        OG143
079800990721    6C                   ENDIF
079900000000      *
080000000000     C                   EXSR      ELABOR
080100000000      *
080200990721    5C                   ENDIF
080300960717     C*******              ENDIF                           BLPDRT >=$DTSPE
080400000000      *
080500990721    4C                   ENDIF
080600990721    3C  N32              ENDDO
080700000000      *
080800000000     C                   ADD       1             XX
080900990721    2C                   ENDDO
081000000619     C**
081100000619     C* MEMORIZZO ULTIMO TOTALE PADRONCINO
081200000622    2C     SAVRIS        IFNE      ' '
081300000622     C     $PRIMA        ANDNE     *ZERO
081400000619     C                   EXSR      TOTPAD
081500000619     C                   EXSR      AZZPAD
081600010411     C* Richiamo routine per totali di filiale per memorizzare su
081700010411     C* FNSCE i "NON CONTROLLATI"
081800010411     C                   EXSR      TOTFIL
081900000619    2C                   ENDIF
082000000616    1C                   ENDIF
082100000000      *
082200000616     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
082300000616     C     SAVRIS        IFEQ      'S'
082400000616     C                   MOVEL     'R'           DS1RIS
082500000616     C                   MOVEA     SVLIN         LIN
082600000619     C                   SETOFF                                       37
082700000616     C                   ENDIF
082800000616     C*
082900000000      * Se scelta la ristampa rileggo solamente l'archivio statistiche
083000000616     C     DS1RIS        IFNE      ' '
083100000000     C                   EXSR      RISTAM
083200990721    1C                   ENDIF
083300000000      *
083400000000      * Stampa terminata
083500000000      * Se non ho stampato nessun dettaglio emetto "STAMPA VUOTA"
083600000000     C     $ALMEN        IFEQ      *ZERO
083700941011     C  N37              WRITE     LSC3E1
083800000000     C                   ELSE
083900000000      *
084000000000      * Avanza l'ultimo padroncino ancora in memoria
084100000000     C                   EXSR      TOTPAD
084200000000      *
084300000000      * Emetto l'ultimo totale di filiale
084400000000     C                   EXSR      TOTFIL
084500000000      *
084600000000      * Stampo il totale generale solo se nella schiera da stampare (LIN) c'e'
084700000000      * la filiale elaboratore (SIMFEL
084800000000     C     SIMFEL        LOOKUP    LIN                                    99
084900000000     C   99              EXSR      TOTGEN
085000000000      *
085100000000     C                   ENDIF
085200941011     C  N37              WRITE     LSC3E2
085300000000      *
085400941011     C  N37              CLOSE     FNLSC3P
085500000000      *
085600000000      * Chiudo il file bolle, solo se eseguita l'elaborazione
085700950213     C     DS1RIS        IFNE      'R'
085800020607     C                   CLOSE     FNBLP70L
085900000000     C                   ENDIF
086000000000      *
086100000000     C     ENDLEG        ENDSR
086200000000      **************************************************************************
086300000000      * Elaborazione del record letto per poi stampare
086400000000      **************************************************************************
086500000000     C     ELABOR        BEGSR
086600000000      *
086700000000      * Memorizzo in una schiera la ragione sociale del cliente poiche'
086800000000      * piu' spedizioni ad un ugual cliente le conteggio assieme (nel cliente)
086900000000      *
087000000000      * Aggancio l'anagrafica clienti
087100000000     C                   MOVE      *BLANKS       ACORAG
087200000000     C                   MOVEL     BLPRSM        ACORAG
087300000000     C     ACORAG        IFEQ      *BLANKS
087400981123     C                   Z-ADD     CODUT         ACOKUT
087500981123     C                   Z-ADD     KCI           ACOKCC
087600000000     C                   Z-ADD     BLPKSC        ACOKSC
087700000000     C     CNACOK        CHAIN     CNACO00F                           33
087800000000     C   33              MOVE      *BLANKS       ACORAG
087900000000     C                   ENDIF
088000010417     C* 36 ON = BOLLA DISK C
088100010417     C                   Z-ADD     1             II
088200010417     C                   CLEAR                   WTMT
088300010417     C     BLPCTM        LOOKUP    TMC(II)                                36
088400010417     C   36              MOVE      TMT(II)       WTMT              1
088500010417     C* Import dpd
088600020218     C*!N36§OGDPD        IFEQ      'S'
088700020218     C                   if        §ogntw = 'DPD'
088800010417     C     BLPCCM        IFGT      *ZEROS
088900010417     C                   MOVEL(P)  BLPCCM        KKEY
089000010417     C                   ELSE
089100010417     C                   MOVEL(P)  BLPKSC        KKEY
089200010417     C                   ENDIF
089300010417     C     KEYSAV        IFNE      KKEY
089400010417     C                   MOVE      KKEY          KEYSAV
089500010417     C                   MOVEL     '3C'          KCOD
089600010417     C     KTAB1         CHAIN     TABEL00F                           98
089700010417     C  N98              MOVEL     TBLUNI        DS3C
089800010417     C   98              CLEAR                   DS3C
089900010417     C                   ENDIF
090000010417     C     §3CDKC        IFEQ      'S'
090100010417     C                   SETON                                        36
090200010417     C                   MOVEL     'C'           WTMT
090300010417     C                   ENDIF
090400010417     C                   ENDIF
090500000000      *
090600000000      * Verifico se la spedizione e' stata controllata
090700000000     C                   EXSR      CONBRV
090800000000     C     $COBRV        COMP      'N'                                    34
090900000000      *
091000000000     C  N34
091100000000     CANN35              SETON                                        35
091200000000      *
091300000000      * Testo il flag "Sigla operatore video"
091400000000     C     BLPSOP        IFNE      '§'
091500950510     C     BLPSOP        ANDNE     '%'
091600010417     C     *IN36         ANDEQ     *OFF
091700000000      *
091800000000      * Ho gia' incontrato il cliente per il "NO DISK" ?   (33ON = SI)
091900000000     C                   SETOFF                                       33
092000000000     C     ACORAG        LOOKUP    RA1                                    33
092100000000     C  N33              ADD       1             $I
092200000000     C  N33              MOVE      ACORAG        RA1($I)
092300000000      *                                                    Bollettato
092400000000     C                   ADD       1             BOSPN
092500000000     C                   ADD       BLPNCL        BOCON
092600000000     C  N33              ADD       1             BOCLN
092700000000      *                                                    Controllato
092800000000     C  N34              ADD       1             COSPN
092900000000     C  N33
093000000000     CANN34              ADD       1             COCLN
093100000000     C                   ELSE
093200000000     C     BLPNRS        IFGT      *ZERO
093300000000      *
093400000000      * Ho gia' incontrato il cliente per il "DISK B" ?    (33ON = SI)
093500000000     C                   SETOFF                                       33
093600000000     C     ACORAG        LOOKUP    RA2                                    33
093700000000     C  N33              ADD       1             $F
093800000000     C  N33              MOVE      ACORAG        RA2($F)
093900000000      *                                                    Bollettato
094000000000     C                   ADD       1             BOSPB
094100000000     C                   ADD       BLPNCL        BOCOB
094200000000     C  N33              ADD       1             BOCLB
094300000000      *                                                    Controllato
094400000000     C  N34              ADD       1             COSPB
094500000000     C  N33
094600000000     CANN34              ADD       1             COCLB
094700000000     C                   ELSE
094800010417     C     *IN36         IFEQ      *OFF
094900000000      *
095000000000      * Ho gia' incontrato il cliente per il "DISK A" ?    (33ON = SI)
095100000000     C                   SETOFF                                       33
095200000000     C     ACORAG        LOOKUP    RA3                                    33
095300000000     C  N33              ADD       1             $H
095400000000     C  N33              MOVE      ACORAG        RA3($H)
095500000000      *                                                    Bollettato
095600000000     C                   ADD       1             BOSPA
095700000000     C                   ADD       BLPNCL        BOCOA
095800000000     C  N33              ADD       1             BOCLA
095900000000      *                                                    Controllato
096000000000     C  N34              ADD       1             COSPA
096100000000     C  N33
096200000000     CANN34              ADD       1             COCLA
096300010417     C                   ELSE
096400010417     C* DISK C BARTOLINI
096500010417     C     WTMT          IFEQ      'B'
096600010420     C     BLPSOP        IFEQ      ':'
096700010420      * Ho gia' incontrato il cliente per il "DISK C" ?    (33ON = SI)
096800010420     C                   SETOFF                                       33
096900010420     C     ACORAG        LOOKUP    RA6                                    33
097000010420     C  N33              ADD       1             $M
097100010420     C  N33              MOVE      ACORAG        RA6($M)
097200010420      *                                                    Bollettato
097300010420     C                   ADD       1             BOSPCE
097400010420     C                   ADD       BLPNCL        BOCOCE
097500010420     C  N33              ADD       1             BOCLCE
097600010420      *                                                    Controllato
097700010420     C  N34              ADD       1             COSPCE
097800010420     C  N33
097900010420     CANN34              ADD       1             COCLCE
098000010420     C                   ELSE
098100010417      * Ho gia' incontrato il cliente per il "DISK C" ?    (33ON = SI)
098200010417     C                   SETOFF                                       33
098300010417     C     ACORAG        LOOKUP    RA4                                    33
098400010417     C  N33              ADD       1             $G
098500010417     C  N33              MOVE      ACORAG        RA4($G)
098600010417      *                                                    Bollettato
098700010417     C                   ADD       1             BOSPCB
098800010417     C                   ADD       BLPNCL        BOCOCB
098900010417     C  N33              ADD       1             BOCLCB
099000010417      *                                                    Controllato
099100010417     C  N34              ADD       1             COSPCB
099200010417     C  N33
099300010417     CANN34              ADD       1             COCLCB
099400010420     C                   ENDIF
099500010417     C                   ELSE
099600010417     C* DISK C CLIENTE
099700010417      * Ho gia' incontrato il cliente per il "DISK C" ?    (33ON = SI)
099800010417     C                   SETOFF                                       33
099900010417     C     ACORAG        LOOKUP    RA5                                    33
100000010417     C  N33              ADD       1             $L
100100010417     C  N33              MOVE      ACORAG        RA5($L)
100200010417      *                                                    Bollettato
100300010417     C                   ADD       1             BOSPCC
100400010417     C                   ADD       BLPNCL        BOCOCC
100500010417     C  N33              ADD       1             BOCLCC
100600010417      *                                                    Controllato
100700010417     C  N34              ADD       1             COSPCC
100800010417     C  N33
100900010417     CANN34              ADD       1             COCLCC
101000010417     C                   ENDIF
101100010417     C                   ENDIF
101200000000     C                   ENDIF
101300000000     C                   ENDIF
101400000000      *
101500000000     C     ENDELA        ENDSR
101600000000      **************************************************************************
101700000000      * Per verificare se una spedizione e' stata controllata devo accedere
101800140618      * al file FNBRV05L
101900000000      **************************************************************************
102000000000     C     CONBRV        BEGSR
102100000000      *
102200000000     C                   MOVE      'N'           $COBRV            1
102300000000      *
102400000000      * Nella bolla vi e' numerazione sequenziale o no ?
102500000000     C     BLPFNS        IFEQ      'S'
102600000000      *
102700941011      * Se non sequenziale devo leggere i colli (FNBLT01L)
102800941011     C     FNBLTK        SETLL     FNBLT01L
102900000000     C                   DO        *HIVAL
103000000000      *
103100941011     C     FNBLTK        READE     FNBLT01L                               34
103200000000     C     *IN34         IFEQ      *OFF
103300000000      *
103400140618      * Verifico se il collo e' stato controllato (chain su FNBRV05L)
103500000000     C                   Z-ADD     5             BRVNPG
103600000000     C                   Z-ADD     BLTFLS        BRVLNP
103700000000     C                   Z-ADD     BLTLNA        BRVLNA
103800000000     C                   Z-ADD     BLTNRS        BRVNRS
103900941011     C                   Z-ADD     BLTNSC        BRVNSC
104000140618     C     FNBRVK        CHAIN     FNBRV05L                           99
104100000000     C  N99              MOVE      'S'           $COBRV
104200000000      *
104300000000      * Testo il flag "Sigla operatore video"
104400000000     C     BLPSOP        IFNE      '§'
104500950510     C     BLPSOP        ANDNE     '%'
104600010417     C     *IN36         ANDEQ     *OFF
104700000000     C  N99              ADD       1             COCON
104800000000     C   99              ADD       1             NCCON
104900000000     C                   ELSE
105000000000     C     BRVNRS        IFGT      *ZERO
105100000000     C  N99              ADD       1             COCOB
105200000000     C   99              ADD       1             NCCOB
105300000000     C                   ELSE
105400010417     C     *IN36         IFEQ      *OFF
105500000000     C  N99              ADD       1             COCOA
105600000000     C   99              ADD       1             NCCOA
105700010417     C                   ELSE
105800010417     C* DISK C BARTOLINI
105900010417     C     WTMT          IFEQ      'B'
106000010420     C     BLPSOP        IFEQ      ':'
106100010420     C* A+C
106200010420     C  N99              ADD       1             COCOCE
106300010420     C   99              ADD       1             NCCOCE
106400010420     C                   ELSE
106500010420     C* SOLO "C" BAR
106600010417     C  N99              ADD       1             COCOCB
106700010417     C   99              ADD       1             NCCOCB
106800010420     C                   ENDIF
106900010417     C                   ELSE
107000010417     C* DISK C CLIENTE
107100010417     C  N99              ADD       1             COCOCC
107200010417     C   99              ADD       1             NCCOCC
107300010417     C                   ENDIF
107400010417     C                   ENDIF
107500000000     C                   ENDIF
107600000000     C                   ENDIF
107700000000      *
107800000000     C                   ENDIF
107900000000     C  N34              ENDDO
108000000000      *
108100000000     C                   ELSE
108200000000      *
108300000000      * Se il flag mi ha indicato segnacolli sequenziali non occorre andare su
108400941011      * FNBLT ma utilizzo i campi "segnacollo da" (BLPNCD) e "segnacollo a"
108500000000      * (BLPNCA)
108600000000     C     BLPNCA        IFEQ      *ZERO
108700000000     C     BLPNCD        ANDGT     *ZERO
108800000000     C                   Z-ADD     BLPNCD        BLPNCA
108900000000     C                   ENDIF
109000000000     C     BLPNCD        DO        BLPNCA        $E                8 0
109100000000      *
109200140618      * Verifico se il collo e' stato controllato (chain su FNBRV05L)
109300000000     C                   Z-ADD     5             BRVNPG
109400000000     C                   Z-ADD     BLPFLS        BRVLNP
109500000000     C                   Z-ADD     BLPLNA        BRVLNA
109600000000     C                   Z-ADD     BLPNRS        BRVNRS
109700000000     C                   Z-ADD     $E            BRVNSC
109800140618     C     FNBRVK        CHAIN     FNBRV05L                           99
109900000000     C  N99              MOVE      'S'           $COBRV
110000000000      *
110100000000      * Testo il flag "Sigla operatore video"
110200000000     C     BLPSOP        IFNE      '§'
110300950510     C     BLPSOP        ANDNE     '%'
110400010417     C     *IN36         ANDEQ     *OFF
110500000000     C  N99              ADD       1             COCON
110600000000     C                   ELSE
110700000000     C     BRVNRS        IFGT      *ZERO
110800000000     C  N99              ADD       1             COCOB
110900000000     C                   ELSE
111000010417     C     *IN36         IFEQ      *OFF
111100000000     C  N99              ADD       1             COCOA
111200010417     C                   ELSE
111300010417     C     WTMT          IFEQ      'B'
111400010420     C     BLPSOP        IFEQ      ':'
111500010420     C  N99              ADD       1             COCOCE
111600010420     C                   ELSE
111700010417     C  N99              ADD       1             COCOCB
111800010420     C                   ENDIF
111900010417     C                   ELSE
112000010417     C  N99              ADD       1             COCOCC
112100010417     C                   ENDIF
112200010417     C                   ENDIF
112300000000     C                   ENDIF
112400000000     C                   ENDIF
112500000000      *
112600000000     C                   ENDDO
112700000000      *
112800000000     C                   ENDIF
112900000000      *
113000000000     C                   ENDSR
113100000000      **************************************************************************
113200000000      * Stampo i totali per padroncino
113300000000      **************************************************************************
113400000000     C     TOTPAD        BEGSR
113500000000      *
113600000000      * Pulisco il record delle statistiche
113700950213     C     DS1RIS        IFNE      'R'
113800941011     C                   CLEAR                   FNSCE000
113900000000     C                   ENDIF
114000000000      *
114100000000      *  ? S T A M P A    D E T T A G L I O    P E R    P A D R O N C I N O ?
114200000000      *
114300000000      * Imposto degli indicatori per stampare o no alcuni campi
114400000000     C                   SETON                                        404142
114500000000     C                   SETOFF                                       43
114600000000      *
114700000000     C                   Z-ADD     $PADPR        CDPADP
114800941011     C                   MOVEL     APDRSC        DEPADP
114900000000     C                   Z-ADD     $FILPR        CDFILP
115000000000     C                   MOVEL     ORGDES        DEFILP
115100941012     C                   MOVEL     ORGDES        DEFI1P
115200000000      * --- ?DISK B? ---                                  ?    DISK B     ?
115300000000     C                   MOVE      DSK(1)        DISK
115400000000      *                                                    Bollettato
115500000000     C                   Z-ADD     BOSPB         BOSPEP
115600000000     C                   Z-ADD     BOCOB         BOCOLP
115700000000     C                   Z-ADD     BOCLB         BOCLIP
115800000000      *                                                    Controllato
115900000000     C                   Z-ADD     COSPB         COSPEP
116000000000     C                   Z-ADD     COCOB         COCOLP
116100000000     C                   Z-ADD     *ZERO         $PERCE
116200000000     C     BOCOB         IFNE      *ZERO
116300000000     C     COCOB         DIV(H)    BOCOB         $PERCE            7 4
116400000000     C                   ENDIF
116500000000     C     $PERCE        MULT(H)   100           COPE1P
116600000000     C                   Z-ADD     COCLB         COCLIP
116700000000     C                   Z-ADD     *ZERO         $PERCE
116800000000     C     BOCLB         IFNE      *ZERO
116900000000     C     COCLB         DIV(H)    BOCLB         $PERCE
117000000000     C                   ENDIF
117100000000     C     $PERCE        MULT(H)   100           COPE2P
117200000000      *
117300000000      * Stampo il padroncino solo se ha almeno 1 collo controllato
117400000000     C     COSPB         IFNE      *ZERO
117500000000     C     COSPA         ORNE      *ZERO
117600000000     C     COSPN         ORNE      *ZERO
117700010417     C     COSPCB        ORNE      *ZERO
117800010417     C     COSPCC        ORNE      *ZERO
117900010420     C     COSPCE        ORNE      *ZERO
118000000000     C                   EXSR      STADET
118100000000     C                   ENDIF
118200000000     C                   EXSR      CALTO1
118300000000      * --- ?DISK A? ---                                  ?    DISK A     ?
118400000000     C                   MOVE      DSK(2)        DISK
118500000000     C                   SETOFF                                       4041
118600000000     C                   MOVE      *BLANKS       DEPADP
118700000000      *                                                    Bollettato
118800000000     C                   Z-ADD     BOSPA         BOSPEP
118900000000     C                   Z-ADD     BOCOA         BOCOLP
119000000000     C                   Z-ADD     BOCLA         BOCLIP
119100000000      *                                                    Controllato
119200000000     C                   Z-ADD     COSPA         COSPEP
119300000000     C                   Z-ADD     COCOA         COCOLP
119400000000     C                   Z-ADD     *ZERO         $PERCE
119500000000     C     BOCOA         IFNE      *ZERO
119600000000     C     COCOA         DIV(H)    BOCOA         $PERCE
119700000000     C                   ENDIF
119800000000     C     $PERCE        MULT(H)   100           COPE1P
119900000000     C                   Z-ADD     COCLA         COCLIP
120000000000     C                   Z-ADD     *ZERO         $PERCE
120100000000     C     BOCLA         IFNE      *ZERO
120200000000     C     COCLA         DIV(H)    BOCLA         $PERCE
120300000000     C                   ENDIF
120400000000     C     $PERCE        MULT(H)   100           COPE2P
120500000000      *
120600000000      * Stampo il padroncino solo se ha almeno 1 collo controllato
120700000000     C     COSPB         IFNE      *ZERO
120800000000     C     COSPA         ORNE      *ZERO
120900000000     C     COSPN         ORNE      *ZERO
121000010417     C     COSPCB        ORNE      *ZERO
121100010417     C     COSPCC        ORNE      *ZERO
121200010420     C     COSPCE        ORNE      *ZERO
121300000000     C                   EXSR      STADET
121400000000     C                   ENDIF
121500000000     C                   EXSR      CALTO1
121600010420      * --- ?DISK C? ---                                  ?    DISK A+C   ?
121700010420     C                   MOVE      DSK(5)        DISK
121800010420      *                                                    Bollettato
121900010420     C                   Z-ADD     BOSPCE        BOSPEP
122000010420     C                   Z-ADD     BOCOCE        BOCOLP
122100010420     C                   Z-ADD     BOCLCE        BOCLIP
122200010420      *                                                    Controllato
122300010420     C                   Z-ADD     COSPCE        COSPEP
122400010420     C                   Z-ADD     COCOCE        COCOLP
122500010420     C                   Z-ADD     *ZERO         $PERCE
122600010420     C     BOCOCE        IFNE      *ZERO
122700010420     C     COCOCE        DIV(H)    BOCOCE        $PERCE
122800010420     C                   ENDIF
122900010420     C     $PERCE        MULT(H)   100           COPE1P
123000010420     C                   Z-ADD     COCLCE        COCLIP
123100010420     C                   Z-ADD     *ZERO         $PERCE
123200010420     C     BOCLCE        IFNE      *ZERO
123300010420     C     COCLCE        DIV(H)    BOCLCE        $PERCE
123400010420     C                   ENDIF
123500010420     C     $PERCE        MULT(H)   100           COPE2P
123600010420      *
123700010420      * Stampo il padroncino solo se ha almeno 1 collo controllato
123800010420     C     COSPB         IFNE      *ZERO
123900010420     C     COSPA         ORNE      *ZERO
124000010420     C     COSPN         ORNE      *ZERO
124100010420     C     COSPCB        ORNE      *ZERO
124200010420     C     COSPCC        ORNE      *ZERO
124300010420     C     COSPCE        ORNE      *ZERO
124400010420     C                   EXSR      STADET
124500010420     C                   ENDIF
124600010420     C                   EXSR      CALTO1
124700010420      * --- ?DISK C? ---                                  ?    DISK C B   ?
124800010420     C                   MOVE      DSK(7)        DISK
124900010417      *                                                    Bollettato
125000010417     C                   Z-ADD     BOSPCB        BOSPEP
125100010417     C                   Z-ADD     BOCOCB        BOCOLP
125200010417     C                   Z-ADD     BOCLCB        BOCLIP
125300010417      *                                                    Controllato
125400010417     C                   Z-ADD     COSPCB        COSPEP
125500010417     C                   Z-ADD     COCOCB        COCOLP
125600010417     C                   Z-ADD     *ZERO         $PERCE
125700010417     C     BOCOCB        IFNE      *ZERO
125800010417     C     COCOCB        DIV(H)    BOCOCB        $PERCE
125900010417     C                   ENDIF
126000010417     C     $PERCE        MULT(H)   100           COPE1P
126100010417     C                   Z-ADD     COCLCB        COCLIP
126200010417     C                   Z-ADD     *ZERO         $PERCE
126300010417     C     BOCLCB        IFNE      *ZERO
126400010417     C     COCLCB        DIV(H)    BOCLCB        $PERCE
126500010417     C                   ENDIF
126600010417     C     $PERCE        MULT(H)   100           COPE2P
126700010417      *
126800010417      * Stampo il padroncino solo se ha almeno 1 collo controllato
126900010417     C     COSPB         IFNE      *ZERO
127000010417     C     COSPA         ORNE      *ZERO
127100010417     C     COSPN         ORNE      *ZERO
127200010417     C     COSPCB        ORNE      *ZERO
127300010417     C     COSPCC        ORNE      *ZERO
127400010420     C     COSPCE        ORNE      *ZERO
127500010417     C                   EXSR      STADET
127600010417     C                   ENDIF
127700010417     C                   EXSR      CALTO1
127800010420      * --- ?DISK C? ---                                  ?    DISK C C   ?
127900010417     C                   MOVE      DSK(6)        DISK
128000010417      *                                                    Bollettato
128100010417     C                   Z-ADD     BOSPCC        BOSPEP
128200010417     C                   Z-ADD     BOCOCC        BOCOLP
128300010417     C                   Z-ADD     BOCLCC        BOCLIP
128400010417      *                                                    Controllato
128500010417     C                   Z-ADD     COSPCC        COSPEP
128600010417     C                   Z-ADD     COCOCC        COCOLP
128700010417     C                   Z-ADD     *ZERO         $PERCE
128800010417     C     BOCOCC        IFNE      *ZERO
128900010417     C     COCOCC        DIV(H)    BOCOCC        $PERCE
129000010417     C                   ENDIF
129100010417     C     $PERCE        MULT(H)   100           COPE1P
129200010417     C                   Z-ADD     COCLCC        COCLIP
129300010417     C                   Z-ADD     *ZERO         $PERCE
129400010417     C     BOCLCC        IFNE      *ZERO
129500010417     C     COCLCC        DIV(H)    BOCLCC        $PERCE
129600010417     C                   ENDIF
129700010417     C     $PERCE        MULT(H)   100           COPE2P
129800010417      *
129900010417      * Stampo il padroncino solo se ha almeno 1 collo controllato
130000010417     C     COSPB         IFNE      *ZERO
130100010417     C     COSPA         ORNE      *ZERO
130200010417     C     COSPN         ORNE      *ZERO
130300010417     C     COSPCB        ORNE      *ZERO
130400010417     C     COSPCC        ORNE      *ZERO
130500010420     C     COSPCE        ORNE      *ZERO
130600010417     C                   EXSR      STADET
130700010417     C                   ENDIF
130800010417     C                   EXSR      CALTO1
130900000000      * --- ?NO DISK? ---                                 ?    NO DISK    ?
131000000000     C                   MOVE      DSK(3)        DISK
131100000000      *                                                    Bollettato
131200000000     C                   Z-ADD     BOSPN         BOSPEP
131300000000     C                   Z-ADD     BOCON         BOCOLP
131400000000     C                   Z-ADD     BOCLN         BOCLIP
131500000000      *                                                    Controllato
131600000000     C                   Z-ADD     COSPN         COSPEP
131700000000     C                   Z-ADD     COCON         COCOLP
131800000000     C                   Z-ADD     *ZERO         $PERCE
131900000000     C     BOCON         IFNE      *ZERO
132000000000     C     COCON         DIV(H)    BOCON         $PERCE
132100000000     C                   ENDIF
132200000000     C     $PERCE        MULT(H)   100           COPE1P
132300000000     C                   Z-ADD     COCLN         COCLIP
132400000000     C                   Z-ADD     *ZERO         $PERCE
132500000000     C     BOCLN         IFNE      *ZERO
132600000000     C     COCLN         DIV(H)    BOCLN         $PERCE
132700000000     C                   ENDIF
132800000000     C     $PERCE        MULT(H)   100           COPE2P
132900000000      *
133000000000      * Stampo il padroncino solo se ha almeno 1 collo controllato
133100000000     C     COSPB         IFNE      *ZERO
133200000000     C     COSPA         ORNE      *ZERO
133300000000     C     COSPN         ORNE      *ZERO
133400010417     C     COSPCB        ORNE      *ZERO
133500010417     C     COSPCC        ORNE      *ZERO
133600010420     C     COSPCE        ORNE      *ZERO
133700000000     C                   EXSR      STADET
133800000000     C                   ENDIF
133900000000     C                   EXSR      CALTO1
134000000000      *
134100000000      * ---  ? S T A M P A    T O T A L I    P E R    P A D R O N C I N O ?  ---
134200000000     C                   MOVE      DSK(4)        DISK
134300000000      *                                                    Bollettato
134400000000     C                   Z-ADD     BOSPT         BOSPEP
134500000000     C                   Z-ADD     BOCOT         BOCOLP
134600000000     C                   Z-ADD     BOCLT         BOCLIP
134700000000      *                                                    Controllato
134800000000     C                   Z-ADD     COSPT         COSPEP
134900000000     C                   Z-ADD     COCOT         COCOLP
135000000000     C                   Z-ADD     *ZERO         $PERCE
135100000000     C     BOCOT         IFNE      *ZERO
135200000000     C     COCOT         DIV(H)    BOCOT         $PERCE
135300000000     C                   ENDIF
135400000000     C     $PERCE        MULT(H)   100           COPE1P
135500000000     C                   Z-ADD     COCLT         COCLIP
135600000000     C                   Z-ADD     *ZERO         $PERCE
135700000000     C     BOCLT         IFNE      *ZERO
135800000000     C     COCLT         DIV(H)    BOCLT         $PERCE
135900000000     C                   ENDIF
136000000000     C     $PERCE        MULT(H)   100           COPE2P
136100000000      *
136200000000      * Stampo il padroncino solo se ha almeno 1 collo controllato
136300000000     C     COSPB         IFNE      *ZERO
136400000000     C     COSPA         ORNE      *ZERO
136500000000     C     COSPN         ORNE      *ZERO
136600010417     C     COSPCB        ORNE      *ZERO
136700010417     C     COSPCC        ORNE      *ZERO
136800010420     C     COSPCE        ORNE      *ZERO
136900010524     C  N37
137000010524     CANN13              WRITE     LSC3R2
137100010524     C  N37
137200010524     CANN13              WRITE     LSC3D1
137300010524     C  N37
137400010524     CANN13              WRITE     LSC3R2
137500010524     C  N37
137600010524     CANN13              WRITE     LSC3R5
137700000000      *
137800000000     C                   ELSE
137900000000      *
138000000000      * Se non stampo il padroncino significa "NON CONTROLLATO", per cui lo
138100000000      * aggiungo nel prospetto "NON CONTROLLATI" (visto che non ho potuto farlo
138200000000      * prima)
138300950213     C     DS1RIS        IFNE      'R'
138400000000     C                   ADD       BOSPB         NCSPB
138500000000     C                   ADD       BOCOB         NCCOB
138600000000     C                   ADD       BOCLB         NCCLB
138700000000     C                   ADD       BOSPA         NCSPA
138800000000     C                   ADD       BOCOA         NCCOA
138900000000     C                   ADD       BOCLA         NCCLA
139000000000     C                   ADD       BOSPN         NCSPN
139100000000     C                   ADD       BOCON         NCCON
139200000000     C                   ADD       BOCLN         NCCLN
139300010417     C                   ADD       BOSPCB        NCSPCB
139400010417     C                   ADD       BOCOCB        NCCOCB
139500010417     C                   ADD       BOCLCB        NCCLCB
139600010417     C                   ADD       BOSPCC        NCSPCC
139700010417     C                   ADD       BOCOCC        NCCOCC
139800010417     C                   ADD       BOCLCC        NCCLCC
139900010420     C                   ADD       BOSPCE        NCSPCE
140000010420     C                   ADD       BOCOCE        NCCOCE
140100010420     C                   ADD       BOCLCE        NCCLCE
140200000000     C                   ENDIF
140300000000      *
140400000000     C                   ENDIF
140500000000      *
140600000000      * Aggiorno il numero padroncini bollettati per filiale
140700000000     C                   ADD       1             PABOF
140800000000      *
140900000000      * Aggiorno il numero padroncini controllati per filiale
141000000000     C     COSPT         IFGT      *ZERO
141100000000     C                   ADD       1             PACOF
141200000000     C                   ENDIF
141300000000      *
141400000000      * Scrivo, in archivio, il record della statistica (solo per elaborazione)
141500950213     C     DS1RIS        IFNE      'R'
141600000000     C                   Z-ADD     SIMFEL        SCEFLE
141700000000     C                   Z-ADD     $FILPR        SCELNP
141800941011     C                   Z-ADD     $PADPR        SCEPDR
141900950213     C                   Z-ADD     DS1ADA        SCEDRE
142000000000     C                   Z-ADD     BOSPB         SCEBSB
142100000000     C                   Z-ADD     BOCOB         SCEBCB
142200000000     C                   Z-ADD     BOCLB         SCEBLB
142300000000     C                   Z-ADD     COSPB         SCECSB
142400000000     C                   Z-ADD     COCOB         SCECCB
142500000000     C                   Z-ADD     COCLB         SCECLB
142600000000     C                   Z-ADD     BOSPA         SCEBSA
142700000000     C                   Z-ADD     BOCOA         SCEBCA
142800000000     C                   Z-ADD     BOCLA         SCEBLA
142900000000     C                   Z-ADD     COSPA         SCECSA
143000000000     C                   Z-ADD     COCOA         SCECCA
143100000000     C                   Z-ADD     COCLA         SCECLA
143200000000     C                   Z-ADD     BOSPN         SCEBSN
143300000000     C                   Z-ADD     BOCON         SCEBCN
143400000000     C                   Z-ADD     BOCLN         SCEBLN
143500000000     C                   Z-ADD     COSPN         SCECSN
143600000000     C                   Z-ADD     COCON         SCECCN
143700000000     C                   Z-ADD     COCLN         SCECLN
143800010420     C                   Z-ADD     BOSPCB        SCEBSC
143900010420     C                   Z-ADD     BOCOCB        SCEBCC
144000010420     C                   Z-ADD     BOCLCB        SCEBLC
144100010420     C                   Z-ADD     COSPCB        SCECSC
144200010420     C                   Z-ADD     COCOCB        SCECCC
144300010420     C                   Z-ADD     COCLCB        SCECLC
144400010420     C                   Z-ADD     BOSPCC        SCEBSK
144500010420     C                   Z-ADD     BOCOCC        SCEBCK
144600010420     C                   Z-ADD     BOCLCC        SCEBLK
144700010420     C                   Z-ADD     COSPCC        SCECSK
144800010420     C                   Z-ADD     COCOCC        SCECCK
144900010420     C                   Z-ADD     COCLCC        SCECLK
145000010420     C                   Z-ADD     BOSPCE        SCEBSE
145100010420     C                   Z-ADD     BOCOCE        SCEBCE
145200010420     C                   Z-ADD     BOCLCE        SCEBLE
145300010420     C                   Z-ADD     COSPCE        SCECSE
145400010420     C                   Z-ADD     COCOCE        SCECCE
145500010420     C                   Z-ADD     COCLCE        SCECLE
145600000000      *
145700000000      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
145800000000      * riempo il "flag di trasmissione" (SATFTR)
145900040922     C***                Z-ADD     *ZERO         SCEDTR
146000040922     C***  §5CSCE        IFEQ      'S'
146100000000     C                   MOVE      *BLANK        SCEFTR
146200040922     C***                ELSE
146300040922     C***                MOVE      'T'           SCEFTR
146400040922     C***                ENDIF
146500040922     c                   z-add     dateu         scedtr
146600000000      *
146700941011     C                   WRITE     FNSCE000
146800000000      *
146900000000     C                   ENDIF
147000000000      *
147100000000     C                   ENDSR
147200000000      **************************************************************************
147300000000      * Stampo i totali per filiale
147400000000      **************************************************************************
147500000000     C     TOTFIL        BEGSR
147600000000      *
147700000000      * ----------  ? S T A M P A    N O N    C O N T R O L L A T I ?  ---------
147800000000      *
147900000000     C                   SETOFF                                       414243
148000010524     C                   SETON                                        40
148100000000      *
148200000000     C     NCSPB         IFNE      *ZERO
148300000000     C     NCCOB         ORNE      *ZERO
148400000000     C     NCCLB         ORNE      *ZERO
148500000000     C     NCSPA         ORNE      *ZERO
148600000000     C     NCCOA         ORNE      *ZERO
148700000000     C     NCCLA         ORNE      *ZERO
148800000000     C     NCSPN         ORNE      *ZERO
148900000000     C     NCCON         ORNE      *ZERO
149000000000     C     NCCLN         ORNE      *ZERO
149100010417     C     NCSPCB        ORNE      *ZERO
149200010417     C     NCCOCB        ORNE      *ZERO
149300010417     C     NCCLCB        ORNE      *ZERO
149400010417     C     NCSPCC        ORNE      *ZERO
149500010417     C     NCCOCC        ORNE      *ZERO
149600010417     C     NCCLCC        ORNE      *ZERO
149700010420     C     NCSPCE        ORNE      *ZERO
149800010420     C     NCCOCE        ORNE      *ZERO
149900010420     C     NCCLCE        ORNE      *ZERO
150000000000     C     'NON CONT'    CAT       'ROLLAT':0    DEPADP
150100000000     C     DEPADP        CAT       'I':0         DEPADP
150200000000      * --- ?DISK B? ---                                  ?    DISK B     ?
150300000000     C                   MOVE      DSK(1)        DISK
150400000000     C                   Z-ADD     NCSPB         BOSPEP
150500000000     C                   Z-ADD     NCCOB         BOCOLP
150600000000     C                   Z-ADD     NCCLB         BOCLIP
150700000000     C                   EXSR      STADET
150800000000     C                   EXSR      CALTO2
150900000000      * --- ?DISK A? ---                                  ?    DISK A     ?
151000000000     C                   MOVE      *BLANKS       DEPADP
151100000000     C                   SETOFF                                       40
151200000000     C                   MOVE      DSK(2)        DISK
151300000000     C                   Z-ADD     NCSPA         BOSPEP
151400000000     C                   Z-ADD     NCCOA         BOCOLP
151500000000     C                   Z-ADD     NCCLA         BOCLIP
151600000000     C                   EXSR      STADET
151700000000     C                   EXSR      CALTO2
151800010417      * --- ?DISK C ? ---                                 ?    DISK C     ?
151900010420     C                   MOVE      DSK(5)        DISK
152000010420     C                   Z-ADD     NCSPCE        BOSPEP
152100010420     C                   Z-ADD     NCCOCE        BOCOLP
152200010420     C                   Z-ADD     NCCLCE        BOCLIP
152300010420     C                   EXSR      STADET
152400010420     C                   EXSR      CALTO2
152500010420     C*
152600010420     C                   MOVE      DSK(7)        DISK
152700010417     C                   Z-ADD     NCSPCB        BOSPEP
152800010417     C                   Z-ADD     NCCOCB        BOCOLP
152900010417     C                   Z-ADD     NCCLCB        BOCLIP
153000010417     C                   EXSR      STADET
153100010417     C                   EXSR      CALTO2
153200010417     C*
153300010417     C                   MOVE      DSK(6)        DISK
153400010417     C                   Z-ADD     NCSPCC        BOSPEP
153500010417     C                   Z-ADD     NCCOCC        BOCOLP
153600010417     C                   Z-ADD     NCCLCC        BOCLIP
153700010417     C                   EXSR      STADET
153800010417     C                   EXSR      CALTO2
153900000000      * --- ?NO DISK? ---                                 ?    NO DISK    ?
154000000000     C                   MOVE      DSK(3)        DISK
154100000000     C                   Z-ADD     NCSPN         BOSPEP
154200000000     C                   Z-ADD     NCCON         BOCOLP
154300000000     C                   Z-ADD     NCCLN         BOCLIP
154400000000     C                   EXSR      STADET
154500000000     C                   EXSR      CALTO2
154600000000      *
154700000000      * --- ?TOTALE PADRONCINI NON CONTROLLATI? ---       ?    TOTALE     ?
154800000000     C                   MOVE      DSK(4)        DISK
154900000000     C                   Z-ADD     NCSPT         BOSPEP
155000000000     C                   Z-ADD     NCCOT         BOCOLP
155100000000     C                   Z-ADD     NCCLT         BOCLIP
155200010524     C  N37
155300010524     CANN13              WRITE     LSC3R2
155400010524     C  N37
155500010524     CANN13              WRITE     LSC3D1
155600010524     C  N37
155700010524     CANN13              WRITE     LSC3R2
155800000000      *
155900000000      * Scrivo, in archivio, il record della statistica (solo per elaborazione)
156000950213     C     DS1RIS        IFNE      'R'
156100000000     C                   Z-ADD     SIMFEL        SCEFLE
156200000000     C                   Z-ADD     CDFILP        SCELNP
156300941011     C                   Z-ADD     *ZERO         SCEPDR
156400950213     C                   Z-ADD     DS1ADA        SCEDRE
156500000000     C                   Z-ADD     NCSPB         SCEBSB
156600000000     C                   Z-ADD     NCCOB         SCEBCB
156700000000     C                   Z-ADD     NCCLB         SCEBLB
156800000000     C                   Z-ADD     *ZERO         SCECSB
156900000000     C                   Z-ADD     *ZERO         SCECCB
157000000000     C                   Z-ADD     *ZERO         SCECLB
157100000000     C                   Z-ADD     NCSPA         SCEBSA
157200000000     C                   Z-ADD     NCCOA         SCEBCA
157300000000     C                   Z-ADD     NCCLA         SCEBLA
157400000000     C                   Z-ADD     *ZERO         SCECSA
157500000000     C                   Z-ADD     *ZERO         SCECCA
157600000000     C                   Z-ADD     *ZERO         SCECLA
157700000000     C                   Z-ADD     NCSPN         SCEBSN
157800000000     C                   Z-ADD     NCCON         SCEBCN
157900000000     C                   Z-ADD     NCCLN         SCEBLN
158000000000     C                   Z-ADD     *ZERO         SCECSN
158100000000     C                   Z-ADD     *ZERO         SCECCN
158200000000     C                   Z-ADD     *ZERO         SCECLN
158300010417     C                   Z-ADD     NCSPCB        SCEBSC
158400010417     C                   Z-ADD     NCCOCB        SCEBCC
158500010417     C                   Z-ADD     NCCLCB        SCEBLC
158600010417     C                   Z-ADD     *ZEROS        SCECSC
158700010417     C                   Z-ADD     *ZEROS        SCECCC
158800010417     C                   Z-ADD     *ZEROS        SCECLC
158900010417     C                   Z-ADD     NCSPCC        SCEBSK
159000010417     C                   Z-ADD     NCCOCC        SCEBCK
159100010417     C                   Z-ADD     NCCLCC        SCEBLK
159200010417     C                   Z-ADD     *ZEROS        SCECSK
159300010417     C                   Z-ADD     *ZEROS        SCECCK
159400010417     C                   Z-ADD     *ZEROS        SCECLK
159500010420     C                   Z-ADD     NCSPCE        SCEBSE
159600010420     C                   Z-ADD     NCCOCE        SCEBCE
159700010420     C                   Z-ADD     NCCLCE        SCEBLE
159800010420     C                   Z-ADD     *ZEROS        SCECSE
159900010420     C                   Z-ADD     *ZEROS        SCECCE
160000010420     C                   Z-ADD     *ZEROS        SCECLE
160100000000      *
160200000000      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
160300000000      * riempo il "flag di trasmissione" (SATFTR)
160400040922     c                   z-add     dateu         scedtr
160500040922     C***                Z-ADD     *ZERO         SCEDTR
160600040922     C***  §5CSCE        IFEQ      'S'
160700000000     C                   MOVE      *BLANK        SCEFTR
160800040922     C***                ELSE
160900040922     C***                MOVE      'T'           SCEFTR
161000040922     C***                ENDIF
161100000000      *
161200941011     C                   WRITE     FNSCE000
161300000000     C                   ENDIF
161400000000      *
161500000000     C                   ENDIF
161600000000      *
161700000000      * ----  ? S T A M P A    D E T T A G L I O    D I    F I L I A L E ?  ----
161800000000      *
161900000000      * Per prima cosa testo l'overflow
162000000000     C  N37              DO
162100941011     C   30              WRITE     LSC3T1
162200000000     C   30              SETOFF                                       30
162300000000      *
162400941011     C                   WRITE     LSC3R5
162500941011     C                   WRITE     LSC3R3
162600941011     C                   WRITE     LSC3T3
162700941011     C                   WRITE     LSC3R3
162800000000     C                   ENDDO
162900000000      *
163000000000     C                   MOVE      *BLANKS       DEPADP
163100000000     C                   SETOFF                                       4143
163200010524     C                   SETON                                        404290
163300000000      * --- ?DISK B? ---                                  ?    DISK B     ?
163400000000     C                   MOVE      DSK(1)        DISK
163500000000      *                                                    Bollettato
163600000000     C                   Z-ADD     B1SPF         BOSPEP
163700000000     C                   Z-ADD     B1COF         BOCOLP
163800000000     C                   Z-ADD     B1CLF         BOCLIP
163900000000      *                                                    Controllato
164000000000     C                   Z-ADD     C1SPF         COSPEP
164100000000     C                   Z-ADD     C1COF         COCOLP
164200000000     C                   Z-ADD     *ZERO         $PERCE
164300000000     C     B1COF         IFNE      *ZERO
164400000000     C     C1COF         DIV(H)    B1COF         $PERCE
164500000000     C                   ENDIF
164600000000     C     $PERCE        MULT(H)   100           COPE1P
164700000000     C                   Z-ADD     C1CLF         COCLIP
164800000000     C                   Z-ADD     *ZERO         $PERCE
164900000000     C     B1CLF         IFNE      *ZERO
165000000000     C     C1CLF         DIV(H)    B1CLF         $PERCE
165100000000     C                   ENDIF
165200000000     C     $PERCE        MULT(H)   100           COPE2P
165300000000     C                   EXSR      STADET
165400000000     C                   EXSR      CALTO3
165500000000      * --- ?DISK A? ---                                  ?    DISK A     ?
165600000000     C                   MOVE      DSK(2)        DISK
165700000000     C                   SETOFF                                       40
165800000000     C                   MOVE      *BLANKS       DEPADP
165900000000      *                                                    Bollettato
166000000000     C                   Z-ADD     B2SPF         BOSPEP
166100000000     C                   Z-ADD     B2COF         BOCOLP
166200000000     C                   Z-ADD     B2CLF         BOCLIP
166300000000      *                                                    Controllato
166400000000     C                   Z-ADD     C2SPF         COSPEP
166500000000     C                   Z-ADD     C2COF         COCOLP
166600000000     C                   Z-ADD     *ZERO         $PERCE
166700000000     C     B2COF         IFNE      *ZERO
166800000000     C     C2COF         DIV(H)    B2COF         $PERCE
166900000000     C                   ENDIF
167000000000     C     $PERCE        MULT(H)   100           COPE1P
167100000000     C                   Z-ADD     C2CLF         COCLIP
167200000000     C                   Z-ADD     *ZERO         $PERCE
167300000000     C     B2CLF         IFNE      *ZERO
167400000000     C     C2CLF         DIV(H)    B2CLF         $PERCE
167500000000     C                   ENDIF
167600000000     C     $PERCE        MULT(H)   100           COPE2P
167700000000     C                   EXSR      STADET
167800000000     C                   EXSR      CALTO3
167900010420      * --- ?DISK C ? ---                                 ?    DISK C     ?
168000010420     C                   MOVE      DSK(5)        DISK
168100010420      *                                                    Bollettato
168200010420     C                   Z-ADD     B5SPF         BOSPEP
168300010420     C                   Z-ADD     B5COF         BOCOLP
168400010420     C                   Z-ADD     B5CLF         BOCLIP
168500010420      *                                                    Controllato
168600010420     C                   Z-ADD     C5SPF         COSPEP
168700010420     C                   Z-ADD     C5COF         COCOLP
168800010420     C                   Z-ADD     *ZERO         $PERCE
168900010420     C     B5COF         IFNE      *ZERO
169000010420     C     C5COF         DIV(H)    B5COF         $PERCE
169100010420     C                   ENDIF
169200010420     C     $PERCE        MULT(H)   100           COPE1P
169300010420     C                   Z-ADD     C5CLF         COCLIP
169400010420     C                   Z-ADD     *ZERO         $PERCE
169500010420     C     B5CLF         IFNE      *ZERO
169600010420     C     C5CLF         DIV(H)    B5CLF         $PERCE
169700010420     C                   ENDIF
169800010420     C     $PERCE        MULT(H)   100           COPE2P
169900010420     C                   EXSR      STADET
170000010420     C                   EXSR      CALTO3
170100010420     C*
170200010420     C                   MOVE      DSK(7)        DISK
170300010417      *                                                    Bollettato
170400010420     C                   Z-ADD     B7SPF         BOSPEP
170500010420     C                   Z-ADD     B7COF         BOCOLP
170600010420     C                   Z-ADD     B7CLF         BOCLIP
170700010417      *                                                    Controllato
170800010420     C                   Z-ADD     C7SPF         COSPEP
170900010420     C                   Z-ADD     C7COF         COCOLP
171000010417     C                   Z-ADD     *ZERO         $PERCE
171100010420     C     B7COF         IFNE      *ZERO
171200010420     C     C7COF         DIV(H)    B7COF         $PERCE
171300010417     C                   ENDIF
171400010417     C     $PERCE        MULT(H)   100           COPE1P
171500010420     C                   Z-ADD     C7CLF         COCLIP
171600010417     C                   Z-ADD     *ZERO         $PERCE
171700010420     C     B7CLF         IFNE      *ZERO
171800010420     C     C7CLF         DIV(H)    B7CLF         $PERCE
171900010417     C                   ENDIF
172000010417     C     $PERCE        MULT(H)   100           COPE2P
172100010417     C                   EXSR      STADET
172200010417     C                   EXSR      CALTO3
172300010417     C*
172400010417     C                   MOVE      DSK(6)        DISK
172500010417      *                                                    Bollettato
172600010417     C                   Z-ADD     B6SPF         BOSPEP
172700010417     C                   Z-ADD     B6COF         BOCOLP
172800010417     C                   Z-ADD     B6CLF         BOCLIP
172900010417      *                                                    Controllato
173000010417     C                   Z-ADD     C6SPF         COSPEP
173100010417     C                   Z-ADD     C6COF         COCOLP
173200010417     C                   Z-ADD     *ZERO         $PERCE
173300010417     C     B6COF         IFNE      *ZERO
173400010417     C     C6COF         DIV(H)    B6COF         $PERCE
173500010417     C                   ENDIF
173600010417     C     $PERCE        MULT(H)   100           COPE1P
173700010417     C                   Z-ADD     C6CLF         COCLIP
173800010417     C                   Z-ADD     *ZERO         $PERCE
173900010417     C     B6CLF         IFNE      *ZERO
174000010417     C     C6CLF         DIV(H)    B6CLF         $PERCE
174100010417     C                   ENDIF
174200010417     C     $PERCE        MULT(H)   100           COPE2P
174300010417     C                   EXSR      STADET
174400010417     C                   EXSR      CALTO3
174500000000      * --- ?NO DISK? ---                                 ?    NO DISK    ?
174600000000     C                   MOVE      DSK(3)        DISK
174700000000      *                                                    Bollettato
174800000000     C                   Z-ADD     B3SPF         BOSPEP
174900000000     C                   Z-ADD     B3COF         BOCOLP
175000000000     C                   Z-ADD     B3CLF         BOCLIP
175100000000      *                                                    Controllato
175200000000     C                   Z-ADD     C3SPF         COSPEP
175300000000     C                   Z-ADD     C3COF         COCOLP
175400000000     C                   Z-ADD     *ZERO         $PERCE
175500000000     C     B3COF         IFNE      *ZERO
175600000000     C     C3COF         DIV(H)    B3COF         $PERCE
175700000000     C                   ENDIF
175800000000     C     $PERCE        MULT(H)   100           COPE1P
175900000000     C                   Z-ADD     C3CLF         COCLIP
176000000000     C                   Z-ADD     *ZERO         $PERCE
176100000000     C     B3CLF         IFNE      *ZERO
176200000000     C     C3CLF         DIV(H)    B3CLF         $PERCE
176300000000     C                   ENDIF
176400000000     C     $PERCE        MULT(H)   100           COPE2P
176500000000     C                   EXSR      STADET
176600000000     C                   EXSR      CALTO3
176700010524     C  N37              SETOFF                                       90
176800000000      *
176900000000      * -------  ? S T A M P A    T O T A L E    D I    F I L I A L E ?  -------
177000000000      *
177100000000     C                   SETOFF                                       4041
177200000000     C                   SETON                                        4243
177300000000      * --- ?TOTALE FILIALE? ---                          ?    TOTALE     ?
177400000000     C                   MOVE      DSK(4)        DISK
177500000000      *                                                    Bollettato
177600000000     C                   Z-ADD     BOSPF         BOSPEP
177700000000     C                   Z-ADD     BOCOF         BOCOLP
177800000000     C                   Z-ADD     BOCLF         BOCLIP
177900000000      *                                                    Controllato
178000000000     C                   Z-ADD     COSPF         COSPEP
178100000000     C                   Z-ADD     COCOF         COCOLP
178200000000     C                   Z-ADD     *ZERO         $PERCE
178300000000     C     BOCOF         IFNE      *ZERO
178400000000     C     COCOF         DIV(H)    BOCOF         $PERCE
178500000000     C                   ENDIF
178600000000     C     $PERCE        MULT(H)   100           COPE1P
178700000000     C                   Z-ADD     COCLF         COCLIP
178800000000     C                   Z-ADD     *ZERO         $PERCE
178900000000     C     BOCLF         IFNE      *ZERO
179000000000     C     COCLF         DIV(H)    BOCLF         $PERCE
179100000000     C                   ENDIF
179200000000     C     $PERCE        MULT(H)   100           COPE2P
179300000000      *
179400000000      * Stampo anche il totale padroncini bollettato e controllato
179500000000     C                   Z-ADD     PABOF         PABOLP
179600000000     C                   Z-ADD     PACOF         PACONP
179700000000     C                   Z-ADD     *ZERO         $PERCE
179800000000     C     PABOF         IFNE      *ZERO
179900000000     C     PACOF         DIV(H)    PABOF         $PERCE
180000000000     C                   ENDIF
180100000000     C     $PERCE        MULT(H)   100           PAPERP
180200000000      *
180300941011     C  N37              WRITE     LSC3R7
180400941011     C  N37              WRITE     LSC3D1
180500941011     C  N37              WRITE     LSC3R8
180600941011     C  N37              WRITE     LSC3R5
180700000000      *
180800000000     C                   ENDSR
180900000000      **************************************************************************
181000000000      * Stampo i totali generali
181100000000      **************************************************************************
181200000000     C     TOTGEN        BEGSR
181300000000      *
181400950213     C     DS1RIS        IFNE      *BLANKS
181500000000     C                   SETOFF                                       414243
181600000000     C                   SETOFF                                       40
181700010524     C                   SETON                                        90
181800000000      *
181900000000      * ------  ? S T A M P A    D E T T A G L I O    G E N E R A L E ?  -------
182000000000      *
182100010601      * Per prima cosa testo l'overflow
182200010601     C   30              WRITE     LSC3T1
182300010601     C   30              SETOFF                                       30
182400010601      *
182500941011     C                   WRITE     LSC3R5
182600941011     C                   WRITE     LSC3R4
182700941011     C                   WRITE     LSC3T4
182800941011     C                   WRITE     LSC3R4
182900000000      *
183000000000     C                   MOVE      *BLANKS       DEPADP
183100000000     C                   SETON                                        42
183200000000      * --- ?DISK B? ---                                  ?    DISK B     ?
183300000000     C                   MOVE      DSK(1)        DISK
183400000000      *                                                    Bollettato
183500000000     C                   Z-ADD     B1SPG         BOSPEP
183600000000     C                   Z-ADD     B1COG         BOCOLP
183700000000     C                   Z-ADD     B1CLG         BOCLIP
183800000000      *                                                    Controllato
183900000000     C                   Z-ADD     C1SPG         COSPEP
184000000000     C                   Z-ADD     C1COG         COCOLP
184100000000     C                   Z-ADD     *ZERO         $PERCE
184200000000     C     B1COG         IFNE      *ZERO
184300000000     C     C1COG         DIV(H)    B1COG         $PERCE
184400000000     C                   ENDIF
184500000000     C     $PERCE        MULT(H)   100           COPE1P
184600000000     C                   Z-ADD     C1CLG         COCLIP
184700000000     C                   Z-ADD     *ZERO         $PERCE
184800000000     C     B1CLG         IFNE      *ZERO
184900000000     C     C1CLG         DIV(H)    B1CLG         $PERCE
185000000000     C                   ENDIF
185100000000     C     $PERCE        MULT(H)   100           COPE2P
185200000000     C                   EXSR      STADET
185300000000     C                   EXSR      CALTO5
185400000000      * --- ?DISK A? ---                                  ?    DISK A     ?
185500000000     C                   MOVE      DSK(2)        DISK
185600000000     C                   SETOFF                                       4041
185700000000     C                   MOVE      *BLANKS       DEPADP
185800000000      *                                                    Bollettato
185900000000     C                   Z-ADD     B2SPG         BOSPEP
186000000000     C                   Z-ADD     B2COG         BOCOLP
186100000000     C                   Z-ADD     B2CLG         BOCLIP
186200000000      *                                                    Controllato
186300000000     C                   Z-ADD     C2SPG         COSPEP
186400000000     C                   Z-ADD     C2COG         COCOLP
186500000000     C                   Z-ADD     *ZERO         $PERCE
186600000000     C     B2COG         IFNE      *ZERO
186700000000     C     C2COG         DIV(H)    B2COG         $PERCE
186800000000     C                   ENDIF
186900000000     C     $PERCE        MULT(H)   100           COPE1P
187000000000     C                   Z-ADD     C2CLG         COCLIP
187100000000     C                   Z-ADD     *ZERO         $PERCE
187200000000     C     B2CLG         IFNE      *ZERO
187300000000     C     C2CLG         DIV(H)    B2CLG         $PERCE
187400000000     C                   ENDIF
187500000000     C     $PERCE        MULT(H)   100           COPE2P
187600000000     C                   EXSR      STADET
187700000000     C                   EXSR      CALTO5
187800010420      * --- ?DISK C ? ---                                 ?    DISK C     ?
187900010420     C                   MOVE      DSK(5)        DISK
188000010420      *                                                    Bollettato
188100010420     C                   Z-ADD     B5SPG         BOSPEP
188200010420     C                   Z-ADD     B5COG         BOCOLP
188300010420     C                   Z-ADD     B5CLG         BOCLIP
188400010420      *                                                    Controllato
188500010420     C                   Z-ADD     C5SPG         COSPEP
188600010420     C                   Z-ADD     C5COG         COCOLP
188700010420     C                   Z-ADD     *ZERO         $PERCE
188800010420     C     B5COG         IFNE      *ZERO
188900010420     C     C5COG         DIV(H)    B5COG         $PERCE
189000010420     C                   ENDIF
189100010420     C     $PERCE        MULT(H)   100           COPE1P
189200010420     C                   Z-ADD     C5CLG         COCLIP
189300010420     C                   Z-ADD     *ZERO         $PERCE
189400010420     C     B5CLG         IFNE      *ZERO
189500010420     C     C5CLG         DIV(H)    B5CLG         $PERCE
189600010420     C                   ENDIF
189700010420     C     $PERCE        MULT(H)   100           COPE2P
189800010420     C                   EXSR      STADET
189900010420     C                   EXSR      CALTO5
190000010420     C*
190100010420     C                   MOVE      DSK(7)        DISK
190200010417      *                                                    Bollettato
190300010420     C                   Z-ADD     B7SPG         BOSPEP
190400010420     C                   Z-ADD     B7COG         BOCOLP
190500010420     C                   Z-ADD     B7CLG         BOCLIP
190600010417      *                                                    Controllato
190700010420     C                   Z-ADD     C7SPG         COSPEP
190800010420     C                   Z-ADD     C7COG         COCOLP
190900010417     C                   Z-ADD     *ZERO         $PERCE
191000010420     C     B7COG         IFNE      *ZERO
191100010420     C     C7COG         DIV(H)    B7COG         $PERCE
191200010417     C                   ENDIF
191300010417     C     $PERCE        MULT(H)   100           COPE1P
191400010420     C                   Z-ADD     C7CLG         COCLIP
191500010417     C                   Z-ADD     *ZERO         $PERCE
191600010420     C     B7CLG         IFNE      *ZERO
191700010420     C     C7CLG         DIV(H)    B7CLG         $PERCE
191800010417     C                   ENDIF
191900010417     C     $PERCE        MULT(H)   100           COPE2P
192000010417     C                   EXSR      STADET
192100010417     C                   EXSR      CALTO5
192200010417     C*
192300010417     C                   MOVE      DSK(6)        DISK
192400010417      *                                                    Bollettato
192500010417     C                   Z-ADD     B6SPG         BOSPEP
192600010417     C                   Z-ADD     B6COG         BOCOLP
192700010417     C                   Z-ADD     B6CLG         BOCLIP
192800010417      *                                                    Controllato
192900010417     C                   Z-ADD     C6SPG         COSPEP
193000010417     C                   Z-ADD     C6COG         COCOLP
193100010417     C                   Z-ADD     *ZERO         $PERCE
193200010417     C     B6COG         IFNE      *ZERO
193300010417     C     C6COG         DIV(H)    B6COG         $PERCE
193400010417     C                   ENDIF
193500010417     C     $PERCE        MULT(H)   100           COPE1P
193600010417     C                   Z-ADD     C6CLG         COCLIP
193700010417     C                   Z-ADD     *ZERO         $PERCE
193800010417     C     B6CLG         IFNE      *ZERO
193900010417     C     C6CLG         DIV(H)    B6CLG         $PERCE
194000010417     C                   ENDIF
194100010417     C     $PERCE        MULT(H)   100           COPE2P
194200010417     C                   EXSR      STADET
194300010417     C                   EXSR      CALTO5
194400000000      * --- ?NO DISK? ---                                 ?    NO DISK    ?
194500000000     C                   MOVE      DSK(3)        DISK
194600000000      *                                                    Bollettato
194700000000     C                   Z-ADD     B3SPG         BOSPEP
194800000000     C                   Z-ADD     B3COG         BOCOLP
194900000000     C                   Z-ADD     B3CLG         BOCLIP
195000000000      *                                                    Controllato
195100000000     C                   Z-ADD     C3SPG         COSPEP
195200000000     C                   Z-ADD     C3COG         COCOLP
195300000000     C                   Z-ADD     *ZERO         $PERCE
195400000000     C     B3COG         IFNE      *ZERO
195500000000     C     C3COG         DIV(H)    B3COG         $PERCE
195600000000     C                   ENDIF
195700000000     C     $PERCE        MULT(H)   100           COPE1P
195800000000     C                   Z-ADD     C3CLG         COCLIP
195900000000     C                   Z-ADD     *ZERO         $PERCE
196000000000     C     B3CLG         IFNE      *ZERO
196100000000     C     C3CLG         DIV(H)    B3CLG         $PERCE
196200000000     C                   ENDIF
196300000000     C     $PERCE        MULT(H)   100           COPE2P
196400000000     C                   EXSR      STADET
196500000000     C                   EXSR      CALTO5
196600010524     C                   SETOFF                                       90
196700000000      *
196800000000      * -----  ? S T A M P A    T O T A L E    D E L    G E N E R A L E ?  -----
196900000000      *
197000000000      * --- ?TOTALE GENERALE ---                          ?    TOTALE     ?
197100000000     C                   MOVE      DSK(4)        DISK
197200000000      *                                                    Bollettato
197300000000     C                   Z-ADD     BOSPG         BOSPEP
197400000000     C                   Z-ADD     BOCOG         BOCOLP
197500000000     C                   Z-ADD     BOCLG         BOCLIP
197600000000      *                                                    Controllato
197700000000     C                   Z-ADD     COSPG         COSPEP
197800000000     C                   Z-ADD     COCOG         COCOLP
197900000000     C                   Z-ADD     *ZERO         $PERCE
198000000000     C     BOCOG         IFNE      *ZERO
198100000000     C     COCOG         DIV(H)    BOCOG         $PERCE
198200000000     C                   ENDIF
198300000000     C     $PERCE        MULT(H)   100           COPE1P
198400000000     C                   Z-ADD     COCLG         COCLIP
198500000000     C                   Z-ADD     *ZERO         $PERCE
198600000000     C     BOCLG         IFNE      *ZERO
198700000000     C     COCLG         DIV(H)    BOCLG         $PERCE
198800000000     C                   ENDIF
198900000000     C     $PERCE        MULT(H)   100           COPE2P
199000941011     C                   WRITE     LSC3R6
199100941011     C                   WRITE     LSC3D1
199200941011     C                   WRITE     LSC3R6
199300000000      *
199400000000      * ----------  ? S T A M P A    N O N    C O N T R O L L A T I ?  ---------
199500000000      *
199600000000     C*                    SETOF                     414243
199700000000     C*                    SETOF                     40
199800000000     C*          N1SPF     IFNE *ZERO
199900000000     C*          N1COF     ORNE *ZERO
200000000000     C*          N1CLF     ORNE *ZERO
200100000000     C*          N2SPF     ORNE *ZERO
200200000000     C*          N2COF     ORNE *ZERO
200300000000     C*          N2CLF     ORNE *ZERO
200400000000     C*          N3SPF     ORNE *ZERO
200500000000     C*          N3COF     ORNE *ZERO
200600000000     C*          N3CLF     ORNE *ZERO
200700000000     C*          'NON CONT'CAT  'ROLLAT':0DEPADP
200800000000     C*          DEPADP    CAT  'I':0     DEPADP
200900000000      * --- ?DISK B? ---                                  ?    DISK B     ?
201000000000     C*                    MOVE DSK,1     DISK
201100000000     C*                    Z-ADDN1SPF     BOSPEP           NR.SPEDIZIONI1F
201200000000     C*                    Z-ADDN1COF     BOCOLP           NR.COLLI     1F
201300000000     C*                    Z-ADDN1CLF     BOCLIP           NR.CLIENTI   1F
201400000000     C*                    EXSR STADET                     DETTAGLIO
201500000000     C*                    EXSR CALTO4                     SOMMA TOTALI
201600000000      * --- ?DISK A? ---                                  ?    DISK A     ?
201700000000     C*                    MOVE *BLANKS   DEPADP
201800000000     C*                    MOVE DSK,2     DISK
201900000000     C*                    Z-ADDN2SPF     BOSPEP           NR.SPEDIZIONI2F
202000000000     C*                    Z-ADDN2COF     BOCOLP           NR.COLLI     2F
202100000000     C*                    Z-ADDN2CLF     BOCLIP           NR.CLIENTI   2F
202200000000     C*                    EXSR STADET                     DETTAGLIO
202300000000     C*                    EXSR CALTO4                     SOMMA TOTALI
202400000000      * --- ?NO DISK? ---                                 ?    NO DISK    ?
202500000000     C*                    MOVE DSK,3     DISK
202600000000     C*                    Z-ADDN3SPF     BOSPEP           NR.SPEDIZIONI3F
202700000000     C*                    Z-ADDN3COF     BOCOLP           NR.COLLI     3F
202800000000     C*                    Z-ADDN3CLF     BOCLIP           NR.CLIENTI   3F
202900000000     C*                    EXSR STADET                     DETTAGLIO
203000000000     C*                    EXSR CALTO4                     SOMMA TOTALI
203100000000      *
203200000000      * --- ?TOTALE PADRONCINI NON CONTROLLATI? ---       ?    TOTALE     ?
203300000000     C*                    MOVE DSK,4     DISK
203400000000     C*                    Z-ADDNCSPF     BOSPEP           NR.SPEDIZIONI F
203500000000     C*                    Z-ADDNCCOF     BOCOLP           NR.COLLI      F
203600000000     C*                    Z-ADDNCCLF     BOCLIP           NR.CLIENTI    F
203700941011     C*                    WRITELSC3R2                     RIGA CORTA
203800941011     C*                    WRITELSC3D1                     DETTAGLIO
203900941011     C*                    WRITELSC3R2                     RIGA CORTA
204000000000     C*                    ENDIF
204100000000      *
204200000000     C                   ENDIF
204300000000      *
204400000000     C                   ENDSR
204500000000      **************************************************************************
204600000000      * Sommo i dati della riga di dettaglio stampata per avere i totali
204700000000      **************************************************************************
204800000000     C     CALTO1        BEGSR
204900000000      *
205000000000      * --  ? C A L C O L O    T O T A L I    P E R    P A D R O N C I N O ?  --
205100000000      *                                                    Bollettato
205200000000     C                   ADD       BOSPEP        BOSPT
205300000000     C                   ADD       BOCOLP        BOCOT
205400000000     C                   ADD       BOCLIP        BOCLT
205500000000      *                                                    Controllato
205600000000     C   42              ADD       COSPEP        COSPT
205700000000     C   42              ADD       COCOLP        COCOT
205800000000     C   42              ADD       COCLIP        COCLT
205900000000      *
206000000000      * Aggiorno i totali anche categoria per categoria
206100000000     C     DISK          IFEQ      DSK(1)
206200000000      * Totali filiale "DISK B" (1)                        Bollettato
206300000000     C                   ADD       BOSPEP        B1SPF
206400000000     C                   ADD       BOCOLP        B1COF
206500000000     C                   ADD       BOCLIP        B1CLF
206600000000      *                                                    Controllato
206700000000     C                   ADD       COSPEP        C1SPF
206800000000     C                   ADD       COCOLP        C1COF
206900000000     C                   ADD       COCLIP        C1CLF
207000000000     C                   ENDIF
207100000000      *
207200000000     C     DISK          IFEQ      DSK(2)
207300000000      * Totali filiale "DISK A" (2)                        Bollettato
207400000000     C                   ADD       BOSPEP        B2SPF
207500000000     C                   ADD       BOCOLP        B2COF
207600000000     C                   ADD       BOCLIP        B2CLF
207700000000      *                                                    Controllato
207800000000     C                   ADD       COSPEP        C2SPF
207900000000     C                   ADD       COCOLP        C2COF
208000000000     C                   ADD       COCLIP        C2CLF
208100000000     C                   ENDIF
208200010417     C     DISK          IFEQ      DSK(5)
208300010420      * Totali filiale "DISK A+C"                          Bollettato
208400010417     C                   ADD       BOSPEP        B5SPF
208500010417     C                   ADD       BOCOLP        B5COF
208600010417     C                   ADD       BOCLIP        B5CLF
208700010417      *                                                    Controllato
208800010417     C                   ADD       COSPEP        C5SPF
208900010417     C                   ADD       COCOLP        C5COF
209000010417     C                   ADD       COCLIP        C5CLF
209100010417     C                   ENDIF
209200010420     C     DISK          IFEQ      DSK(6)
209300010420      * Totali filiale "DISK C CLI"                        Bollettato
209400010420     C                   ADD       BOSPEP        B6SPF
209500010420     C                   ADD       BOCOLP        B6COF
209600010420     C                   ADD       BOCLIP        B6CLF
209700010420      *                                                    Controllato
209800010420     C                   ADD       COSPEP        C6SPF
209900010420     C                   ADD       COCOLP        C6COF
210000010420     C                   ADD       COCLIP        C6CLF
210100010420     C                   ENDIF
210200010420     C     DISK          IFEQ      DSK(7)
210300010420      * Totali filiale "DISK C BAR"                        Bollettato
210400010420     C                   ADD       BOSPEP        B7SPF
210500010420     C                   ADD       BOCOLP        B7COF
210600010420     C                   ADD       BOCLIP        B7CLF
210700010417      *                                                    Controllato
210800010420     C                   ADD       COSPEP        C7SPF
210900010420     C                   ADD       COCOLP        C7COF
211000010420     C                   ADD       COCLIP        C7CLF
211100010417     C                   ENDIF
211200000000      *
211300000000     C     DISK          IFEQ      DSK(3)
211400000000      * Totali filiale "NO DISK" (3)                       Bollettato
211500000000     C                   ADD       BOSPEP        B3SPF
211600000000     C                   ADD       BOCOLP        B3COF
211700000000     C                   ADD       BOCLIP        B3CLF
211800000000      *                                                    Controllato
211900000000     C                   ADD       COSPEP        C3SPF
212000000000     C                   ADD       COCOLP        C3COF
212100000000     C                   ADD       COCLIP        C3CLF
212200000000     C                   ENDIF
212300000000      *
212400000000     C                   ENDSR
212500000000      **************************************************************************
212600000000      * Sommo i dati della riga stampata come totali NON CONTROLLATI
212700000000      **************************************************************************
212800000000     C     CALTO2        BEGSR
212900000000      *
213000000000      * --  ? C A L C O L O    T O T A L E    N O N    C O N T R O L LA T I ?  -
213100000000      *                                                    Bollettato
213200000000     C                   ADD       BOSPEP        NCSPT
213300000000     C                   ADD       BOCOLP        NCCOT
213400000000     C                   ADD       BOCLIP        NCCLT
213500000000      *
213600000000     C     DISK          IFEQ      DSK(1)
213700000000      * Totali filiale "DISK B" (1)                        Bollettato
213800000000     C                   ADD       BOSPEP        N1SPF             7 0
213900000000     C                   ADD       BOCOLP        N1COF             7 0
214000000000     C                   ADD       BOCLIP        N1CLF             7 0
214100000000     C                   ENDIF
214200000000      *
214300000000     C     DISK          IFEQ      DSK(2)
214400000000      * Totali filiale "DISK A" (2)                        Bollettato
214500000000     C                   ADD       BOSPEP        N2SPF             7 0
214600000000     C                   ADD       BOCOLP        N2COF             7 0
214700000000     C                   ADD       BOCLIP        N2CLF             7 0
214800000000     C                   ENDIF
214900010417     C*
215000010417     C     DISK          IFEQ      DSK(5)
215100010420      * Totali filiale "DISK A+C BAR"                        Bollettato
215200010417     C                   ADD       BOSPEP        N5SPF             7 0
215300010417     C                   ADD       BOCOLP        N5COF             7 0
215400010417     C                   ADD       BOCLIP        N5CLF             7 0
215500010417     C                   ENDIF
215600010417     C*
215700010420     C     DISK          IFEQ      DSK(6)
215800010420      * Totali filiale "DISK C CLI"                          Bollettato
215900010417     C                   ADD       BOSPEP        N6SPF             7 0
216000010417     C                   ADD       BOCOLP        N6COF             7 0
216100010417     C                   ADD       BOCLIP        N6CLF             7 0
216200010417     C                   ENDIF
216300010420     C*
216400010420     C     DISK          IFEQ      DSK(7)
216500010420      * Totali filiale "DISK C BAR"                          Bollettato
216600010420     C                   ADD       BOSPEP        N7SPF             7 0
216700010420     C                   ADD       BOCOLP        N7COF             7 0
216800010420     C                   ADD       BOCLIP        N7CLF             7 0
216900010420     C                   ENDIF
217000000000      *
217100000000     C     DISK          IFEQ      DSK(3)
217200000000      * Totali filiale "NO DISK" (3)                       Bollettato
217300000000     C                   ADD       BOSPEP        N3SPF             7 0
217400000000     C                   ADD       BOCOLP        N3COF             7 0
217500000000     C                   ADD       BOCLIP        N3CLF             7 0
217600000000     C                   ENDIF
217700000000      *
217800000000     C                   ENDSR
217900000000      **************************************************************************
218000000000      * Sommo i dati della riga stampata come totali della filiale
218100000000      **************************************************************************
218200000000     C     CALTO3        BEGSR
218300000000      *
218400000000      * -----  ? C A L C O L O    T O T A L E    P E R    F I L I A L E ?  -----
218500000000      *                                                    Bollettato
218600000000     C                   ADD       BOSPEP        BOSPF
218700000000     C                   ADD       BOCOLP        BOCOF
218800000000     C                   ADD       BOCLIP        BOCLF
218900000000      *                                                    Controllato
219000000000     C                   ADD       COSPEP        COSPF
219100000000     C                   ADD       COCOLP        COCOF
219200000000     C                   ADD       COCLIP        COCLF
219300000000      *                                                    Padroncini
219400000619     C     DS1RIS        IFNE      ' '
219500000619     C     DISK          IFEQ      DSK(1)
219600000000      * Totali generali "DISK B" (1)                       Bollettato
219700000000     C                   ADD       BOSPEP        B1SPG             7 0
219800000000     C                   ADD       BOCOLP        B1COG             7 0
219900000000     C                   ADD       BOCLIP        B1CLG             7 0
220000000000      *                                                    Controllato
220100000000     C                   ADD       COSPEP        C1SPG             7 0
220200000000     C                   ADD       COCOLP        C1COG             7 0
220300000000     C                   ADD       COCLIP        C1CLG             7 0
220400000000     C                   ENDIF
220500000000      *
220600000000     C     DISK          IFEQ      DSK(2)
220700000000      * Totali generali "DISK A" (2)                       Bollettato
220800000000     C                   ADD       BOSPEP        B2SPG             7 0
220900000000     C                   ADD       BOCOLP        B2COG             7 0
221000000000     C                   ADD       BOCLIP        B2CLG             7 0
221100000000      *                                                    Controllato
221200000000     C                   ADD       COSPEP        C2SPG             7 0
221300000000     C                   ADD       COCOLP        C2COG             7 0
221400000000     C                   ADD       COCLIP        C2CLG             7 0
221500010417     C                   ENDIF
221600010417     C*
221700010417     C     DISK          IFEQ      DSK(5)
221800010420      * Totali generali "DISK A+C BAR"                     Bollettato
221900010417     C                   ADD       BOSPEP        B5SPG             7 0
222000010417     C                   ADD       BOCOLP        B5COG             7 0
222100010417     C                   ADD       BOCLIP        B5CLG             7 0
222200010417      *                                                    Controllato
222300010417     C                   ADD       COSPEP        C5SPG             7 0
222400010417     C                   ADD       COCOLP        C5COG             7 0
222500010417     C                   ADD       COCLIP        C5CLG             7 0
222600010417     C                   ENDIF
222700010417     C*
222800010417     C     DISK          IFEQ      DSK(6)
222900010417      * Totali generali "DISK C CLI" (6)                   Bollettato
223000010417     C                   ADD       BOSPEP        B6SPG             7 0
223100010417     C                   ADD       BOCOLP        B6COG             7 0
223200010417     C                   ADD       BOCLIP        B6CLG             7 0
223300010417      *                                                    Controllato
223400010417     C                   ADD       COSPEP        C6SPG             7 0
223500010417     C                   ADD       COCOLP        C6COG             7 0
223600010417     C                   ADD       COCLIP        C6CLG             7 0
223700010417     C                   ENDIF
223800010420     C*
223900010420     C     DISK          IFEQ      DSK(7)
224000010420      * Totali generali "DISK C BAR" (6)                   Bollettato
224100010420     C                   ADD       BOSPEP        B7SPG             7 0
224200010420     C                   ADD       BOCOLP        B7COG             7 0
224300010420     C                   ADD       BOCLIP        B7CLG             7 0
224400010420      *                                                    Controllato
224500010420     C                   ADD       COSPEP        C7SPG             7 0
224600010420     C                   ADD       COCOLP        C7COG             7 0
224700010420     C                   ADD       COCLIP        C7CLG             7 0
224800010420     C                   ENDIF
224900000000      *
225000000000     C     DISK          IFEQ      DSK(3)
225100000000      * Totali generali "NO DISK" (3)                      Bollettato
225200000000     C                   ADD       BOSPEP        B3SPG             7 0
225300000000     C                   ADD       BOCOLP        B3COG             7 0
225400000000     C                   ADD       BOCLIP        B3CLG             7 0
225500000000      *                                                    Controllato
225600000000     C                   ADD       COSPEP        C3SPG             7 0
225700000000     C                   ADD       COCOLP        C3COG             7 0
225800000000     C                   ADD       COCLIP        C3CLG             7 0
225900000000     C                   ENDIF
226000000619     C                   ENDIF
226100000000      *
226200000000     C                   ENDSR
226300000000      **************************************************************************
226400000000      * Sommo i dati della riga stampata come totali NON CONTROLLATI di filiale
226500000000      **************************************************************************
226600000000     C     CALTO4        BEGSR
226700000000      *
226800000000      * -------  ? CALCOLO  TOTALE  NON  CONTROLLATI  PER  FILIALE ?  ----------
226900000000      *                                                    Bollettato
227000000000     C                   ADD       BOSPEP        NCSPF             7 0
227100000000     C                   ADD       BOCOLP        NCCOF             7 0
227200000000     C                   ADD       BOCLIP        NCCLF             7 0
227300000000      *
227400000000     C                   ENDSR
227500000000      **************************************************************************
227600000000      * Sommo i dati della riga stampata come totali generali
227700000000      **************************************************************************
227800000000     C     CALTO5        BEGSR
227900000000      *
228000000000      * -------  ? C A L C O L O    T O T A L E    G E N E R A L E ?  ----------
228100000000      *                                                    Bollettato
228200000000     C                   ADD       BOSPEP        BOSPG             7 0
228300000000     C                   ADD       BOCOLP        BOCOG             7 0
228400000000     C                   ADD       BOCLIP        BOCLG             7 0
228500000000      *                                                    Controllato
228600000000     C                   ADD       COSPEP        COSPG             7 0
228700000000     C                   ADD       COCOLP        COCOG             7 0
228800000000     C                   ADD       COCLIP        COCLG             7 0
228900000000      *
229000000000     C                   ENDSR
229100000000      **************************************************************************
229200000000      * Stampo la riga di dettaglio testando l'overflow
229300000000      **************************************************************************
229400000000     C     STADET        BEGSR
229500000000      *
229600950213     C     DS1RIS        IFNE      *BLANKS
229700000000      *
229800000000      * Testo l'indicatore di overflow
229900000000     C     *IN30         IFEQ      *ON
230000941011     C                   WRITE     LSC3T1
230100941011     C                   WRITE     LSC3R1
230200941011     C                   WRITE     LSC3T2
230300941011     C                   WRITE     LSC3R1
230400000000      *
230500000000     C                   SETOFF                                       30
230600000000     C                   ENDIF
230700000000      *
230800000000     C     BOCOLP        COMP      *ZERO                              4444
230900000000     C     BOCLIP        COMP      *ZERO                              4545
231000000000      *
231100010524     C   90
231200010524     CORN13              WRITE     LSC3D1
231300000000      *
231400000000     C                   ENDIF
231500000000     C                   Z-ADD     1             $ALMEN
231600000000      *
231700000000     C                   ENDSR
231800000000      **************************************************************************
231900000000      * Azzeramento totali per padroncino
232000000000      **************************************************************************
232100000000     C     AZZPAD        BEGSR
232200000000      *
232300000000     C                   Z-ADD     *ZERO         BOSPN             7 0
232400000000     C                   Z-ADD     *ZERO         BOCON             7 0
232500000000     C                   Z-ADD     *ZERO         BOCLN             7 0
232600000000      *                                                    Controllato
232700000000     C                   Z-ADD     *ZERO         COSPN             7 0
232800000000     C                   Z-ADD     *ZERO         COCON             7 0
232900000000     C                   Z-ADD     *ZERO         COCLN             7 0
233000000000      *                                                    --- DISK B ----
233100000000      *                                                    Bollettato
233200000000     C                   Z-ADD     *ZERO         BOSPB             7 0
233300000000     C                   Z-ADD     *ZERO         BOCOB             7 0
233400000000     C                   Z-ADD     *ZERO         BOCLB             7 0
233500000000      *                                                    Controllato
233600000000     C                   Z-ADD     *ZERO         COSPB             7 0
233700000000     C                   Z-ADD     *ZERO         COCOB             7 0
233800000000     C                   Z-ADD     *ZERO         COCLB             7 0
233900000000      *                                                    --- DISK A ----
234000000000      *                                                    Bollettato
234100000000     C                   Z-ADD     *ZERO         BOSPA             7 0
234200000000     C                   Z-ADD     *ZERO         BOCOA             7 0
234300000000     C                   Z-ADD     *ZERO         BOCLA             7 0
234400000000      *                                                    Controllato
234500000000     C                   Z-ADD     *ZERO         COSPA             7 0
234600000000     C                   Z-ADD     *ZERO         COCOA             7 0
234700000000     C                   Z-ADD     *ZERO         COCLA             7 0
234800010417      *                                                    --- DISK C ----
234900010417      *                                                    Bollettato
235000010417     C                   Z-ADD     *ZERO         BOSPCB            7 0
235100010417     C                   Z-ADD     *ZERO         BOCOCB            7 0
235200010417     C                   Z-ADD     *ZERO         BOCLCB            7 0
235300010417      *                                                    Controllato
235400010417     C                   Z-ADD     *ZERO         COSPCB            7 0
235500010417     C                   Z-ADD     *ZERO         COCOCB            7 0
235600010417     C                   Z-ADD     *ZERO         COCLCB            7 0
235700010417      *                                                    Bollettato
235800010417     C                   Z-ADD     *ZERO         BOSPCC            7 0
235900010417     C                   Z-ADD     *ZERO         BOCOCC            7 0
236000010417     C                   Z-ADD     *ZERO         BOCLCC            7 0
236100010417      *                                                    Controllato
236200010417     C                   Z-ADD     *ZERO         COSPCC            7 0
236300010417     C                   Z-ADD     *ZERO         COCOCC            7 0
236400010417     C                   Z-ADD     *ZERO         COCLCC            7 0
236500010420      *                                                    Bollettato
236600010420     C                   Z-ADD     *ZERO         BOSPCE            7 0
236700010420     C                   Z-ADD     *ZERO         BOCOCE            7 0
236800010420     C                   Z-ADD     *ZERO         BOCLCE            7 0
236900010420      *                                                    Controllato
237000010420     C                   Z-ADD     *ZERO         COSPCE            7 0
237100010420     C                   Z-ADD     *ZERO         COCOCE            7 0
237200010420     C                   Z-ADD     *ZERO         COCLCE            7 0
237300000000      *                                                    --- TOTALI ----
237400000000     C                   Z-ADD     *ZERO         BOSPT             7 0
237500000000     C                   Z-ADD     *ZERO         BOCOT             7 0
237600000000     C                   Z-ADD     *ZERO         BOCLT             7 0
237700000000      *                                                    Controllato
237800000000     C                   Z-ADD     *ZERO         COSPT             7 0
237900000000     C                   Z-ADD     *ZERO         COCOT             7 0
238000000000     C                   Z-ADD     *ZERO         COCLT             7 0
238100000000      *
238200000000     C                   CLEAR                   RA1
238300000000     C                   CLEAR                   RA2
238400000000     C                   CLEAR                   RA3
238500010417     C                   CLEAR                   RA4
238600010417     C                   CLEAR                   RA5
238700010420     C                   CLEAR                   RA6
238800000000     C                   Z-ADD     *ZERO         $G
238900000000     C                   Z-ADD     *ZERO         $F
239000000000     C                   Z-ADD     *ZERO         $H
239100000000     C                   Z-ADD     *ZERO         $I
239200010417     C                   Z-ADD     *ZERO         $L
239300010420     C                   Z-ADD     *ZERO         $M
239400000000      *
239500000000     C                   ENDSR
239600000000      **************************************************************************
239700000000      * Azzeramento totali per filiale
239800000000      **************************************************************************
239900000000     C     AZZFIL        BEGSR
240000000000      *                                                    Non controllato
240100000000     C                   Z-ADD     *ZERO         NCSPA             7 0
240200000000     C                   Z-ADD     *ZERO         NCCOA             7 0
240300000000     C                   Z-ADD     *ZERO         NCCLA             7 0
240400010417     C                   Z-ADD     *ZERO         NCSPCB            7 0
240500010417     C                   Z-ADD     *ZERO         NCCOCB            7 0
240600010417     C                   Z-ADD     *ZERO         NCCLCB            7 0
240700010417     C                   Z-ADD     *ZERO         NCSPCC            7 0
240800010417     C                   Z-ADD     *ZERO         NCCOCC            7 0
240900010417     C                   Z-ADD     *ZERO         NCCLCC            7 0
241000010420     C                   Z-ADD     *ZERO         NCSPCE            7 0
241100010420     C                   Z-ADD     *ZERO         NCCOCE            7 0
241200010420     C                   Z-ADD     *ZERO         NCCLCE            7 0
241300000000     C                   Z-ADD     *ZERO         NCSPB             7 0
241400000000     C                   Z-ADD     *ZERO         NCCOB             7 0
241500000000     C                   Z-ADD     *ZERO         NCCLB             7 0
241600000000     C                   Z-ADD     *ZERO         NCSPN             7 0
241700000000     C                   Z-ADD     *ZERO         NCCON             7 0
241800000000     C                   Z-ADD     *ZERO         NCCLN             7 0
241900000000     C                   Z-ADD     *ZERO         NCSPT             7 0
242000000000     C                   Z-ADD     *ZERO         NCCOT             7 0
242100000000     C                   Z-ADD     *ZERO         NCCLT             7 0
242200000000      *
242300000000     C                   Z-ADD     *ZERO         PABOF             7 0
242400000000     C                   Z-ADD     *ZERO         PACOF             7 0
242500000000      *
242600010417     C                   Z-ADD     *ZERO         B1SPF             7 0
242700000000     C                   Z-ADD     *ZERO         B1COF             7 0
242800000000     C                   Z-ADD     *ZERO         B1CLF             7 0
242900000000     C                   Z-ADD     *ZERO         C1SPF             7 0
243000000000     C                   Z-ADD     *ZERO         C1COF             7 0
243100000000     C                   Z-ADD     *ZERO         C1CLF             7 0
243200000000     C                   Z-ADD     *ZERO         B2SPF             7 0
243300000000     C                   Z-ADD     *ZERO         B2COF             7 0
243400000000     C                   Z-ADD     *ZERO         B2CLF             7 0
243500000000     C                   Z-ADD     *ZERO         C2SPF             7 0
243600000000     C                   Z-ADD     *ZERO         C2COF             7 0
243700000000     C                   Z-ADD     *ZERO         C2CLF             7 0
243800000000     C                   Z-ADD     *ZERO         B3SPF             7 0
243900000000     C                   Z-ADD     *ZERO         B3COF             7 0
244000000000     C                   Z-ADD     *ZERO         B3CLF             7 0
244100000000     C                   Z-ADD     *ZERO         C3SPF             7 0
244200000000     C                   Z-ADD     *ZERO         C3COF             7 0
244300000000     C                   Z-ADD     *ZERO         C3CLF             7 0
244400010417     C                   Z-ADD     *ZERO         B5SPF             7 0
244500010417     C                   Z-ADD     *ZERO         B5COF             7 0
244600010417     C                   Z-ADD     *ZERO         B5CLF             7 0
244700010417     C                   Z-ADD     *ZERO         C5SPF             7 0
244800010417     C                   Z-ADD     *ZERO         C5COF             7 0
244900010417     C                   Z-ADD     *ZERO         C5CLF             7 0
245000010417     C                   Z-ADD     *ZERO         B6SPF             7 0
245100010417     C                   Z-ADD     *ZERO         B6COF             7 0
245200010417     C                   Z-ADD     *ZERO         B6CLF             7 0
245300010417     C                   Z-ADD     *ZERO         C6SPF             7 0
245400010417     C                   Z-ADD     *ZERO         C6COF             7 0
245500010417     C                   Z-ADD     *ZERO         C6CLF             7 0
245600010420     C                   Z-ADD     *ZERO         B7SPF             7 0
245700010420     C                   Z-ADD     *ZERO         B7COF             7 0
245800010420     C                   Z-ADD     *ZERO         B7CLF             7 0
245900010420     C                   Z-ADD     *ZERO         C7SPF             7 0
246000010420     C                   Z-ADD     *ZERO         C7COF             7 0
246100010420     C                   Z-ADD     *ZERO         C7CLF             7 0
246200000000     C                   Z-ADD     *ZERO         BOSPF             7 0
246300000000     C                   Z-ADD     *ZERO         BOCOF             7 0
246400000000     C                   Z-ADD     *ZERO         BOCLF             7 0
246500000000      *                                                    Controllato
246600000000     C                   Z-ADD     *ZERO         COSPF             7 0
246700000000     C                   Z-ADD     *ZERO         COCOF             7 0
246800000000     C                   Z-ADD     *ZERO         COCLF             7 0
246900000000      *
247000000000     C                   ENDSR
247100000000      **************************************************************************
247200000000      * I dati della stampa non vengono rigenerati, ma semplicemente viene letto
247300941011      * l'archivio statistiche (FNSCE00F) preparato con una elaborazione
247400000000      * precedente
247500000000      **************************************************************************
247600000000     C     RISTAM        BEGSR
247700000000      *
247800000000     C                   Z-ADD     *ZERO         $PRIM1
247900000000     C                   Z-ADD     *ZERO         $PRIMA
248000000000     C                   Z-ADD     *ZERO         $FILPR
248100000000     C                   Z-ADD     *ZERO         $PADPR
248200050829     c                   setoff                                       3839
248300000000      *
248400000000      * Pulisco i campi per l'elaborazione
248500000000     C                   EXSR      PULIZ
248600000000      *
248700000000      * Verifico se richiesta una specifica area
248800950213     C     DS1FEL        COMP      *ZERO                                  39
248900050829     c
249000050829     c                   if        ds1fel=0 and simfel>0
249100050829     c                   eval      ds1fel=simfel
249200050829     c                   setoff                                       39
249300050829     c                   endif
249400050829     c
249500050829     c                   if        lin(1)>0 and lin(2)=0
249600050829     c                   eval      klin=lin(1)
249700050829     c                   seton                                        38
249800050829     c                   endif
249900000000      *
250000000000     C                   SETON                                        43
250100050829     C  n38DS1FEL        SETLL     FNSCE01L
250200050829     C   38ksce          SETLL     FNSCE01L
250300000000     C                   DO        *HIVAL
250400050829     c                   if        *in38
250500050829     C     ksce          READE(N)  FNSCE01L                               33
250600050829     c                   else
250700050829     C  N39DS1FEL        READE(N)  FNSCE01L                               33
250800941011     C   39              READ(N)   FNSCE01L                               33
250900050829     c                   endif
251000000000      *
251100950213     C  N33SCEDRE        IFGE      DS1ADA
251200950213     C     SCEDRE        ANDLE     DS1AAL
251300000000      *
251400000000      * Solo le filiali presenti nella schiera LIN
251500050829     c                   if        not *in38 and *in39
251600000000     C     SCELNP        LOOKUP    LIN                                    99
251700050829     c                   else
251800050829     c                   seton                                        99
251900050829     c                   endif
252000050829     c
252100000000     C     *IN99         IFEQ      *ON
252200000000      *
252300000000      * Verifico il record appartiene al padroncino precedente
252400941011     C     SCEPDR        IFNE      $PADPR
252500000000      *
252600000000     C     $PRIMA        IFNE      *ZERO
252700000000     C     $PADPR        ANDNE     *ZERO
252800000000     C                   EXSR      TOTPAD
252900000000     C                   EXSR      AZZPAD
253000000000     C                   ENDIF
253100000000     C                   Z-ADD     1             $PRIMA
253200000000      *
253300000000      * Decodifico il codice padroncino
253400021203     C*    SCEPDR        CHAIN     FNAPD01L                           99
253500021205     C                   move      ScePdr        kpdpdr
253600021203     C                   move      'A'           kpdtip
253700021203     C     kfapd         chain     fiapd01L                           99
253800020517     C*!N99APDATB        COMP      *BLANK                             9999
253900941011     C   99              CLEAR                   APDRSC
254000000000      *
254100941011     C                   Z-ADD     SCEPDR        $PADPR
254200000000     C                   ENDIF
254300000000      *
254400000000      * Se cambia la filiale decodifico e stampo i totali
254500000000     C     SCELNP        IFNE      $FILPR
254600000000     C                   SETON                                        40
254700000000     C     $PRIM1        IFNE      *ZERO
254800000000     C                   EXSR      TOTFIL
254900000000     C                   EXSR      AZZFIL
255000000000     C                   ENDIF
255100000000     C                   Z-ADD     1             $PRIM1
255200000000     C                   Z-ADD     SCELNP        $FILPR
255300000000      *
255400000000      * Decodifico il codice filiale
255500000000     C     SCELNP        CHAIN     AZORG01L                           99
255600000000     C                   SETOFF                                       40
255700010417     C                   CLEAR                   OG143
255800010417     C  N99              MOVEL     ORGDE3        OG143
255900000000     C                   ENDIF
256000000000      *
256100000000      * Aggiorno i totali
256200000000     C                   EXSR      ELABO2
256300000000     C                   Z-ADD     1             $ALMEN
256400000000      *
256500000000     C                   ENDIF
256600000000      *
256700000000     C                   ENDIF
256800000000     C  N33              ENDDO
256900000000      *
257000000000     C                   ENDSR
257100000000      **************************************************************************
257200941011      * Avendo letto un record dal file statistiche (FNSCE00F) riempo
257300000000      * le schiere per stampare
257400000000      **************************************************************************
257500000000     C     ELABO2        BEGSR
257600000000      *
257700000000      * Se il cod.padroncino e' nullo significa: "NON CONTROLLATI"
257800941011     C     SCEPDR        IFNE      *ZERO
257900000000     C                   ADD       SCEBSB        BOSPB
258000000000     C                   ADD       SCEBCB        BOCOB
258100000000     C                   ADD       SCEBLB        BOCLB
258200000000     C                   ADD       SCECSB        COSPB
258300000000     C                   ADD       SCECCB        COCOB
258400000000     C                   ADD       SCECLB        COCLB
258500000000     C                   ADD       SCEBSA        BOSPA
258600000000     C                   ADD       SCEBCA        BOCOA
258700000000     C                   ADD       SCEBLA        BOCLA
258800000000     C                   ADD       SCECSA        COSPA
258900000000     C                   ADD       SCECCA        COCOA
259000000000     C                   ADD       SCECLA        COCLA
259100010417     C                   ADD       SCEBSC        BOSPCB
259200010417     C                   ADD       SCEBCC        BOCOCB
259300010417     C                   ADD       SCEBLC        BOCLCB
259400010417     C                   ADD       SCECSC        COSPCB
259500010417     C                   ADD       SCECCC        COCOCB
259600010417     C                   ADD       SCECLC        COCLCB
259700010417     C                   ADD       SCEBSK        BOSPCC
259800010417     C                   ADD       SCEBCK        BOCOCC
259900010417     C                   ADD       SCEBLK        BOCLCC
260000010417     C                   ADD       SCECSK        COSPCC
260100010417     C                   ADD       SCECCK        COCOCC
260200010417     C                   ADD       SCECLK        COCLCC
260300010420     C                   ADD       SCEBSE        BOSPCE
260400010420     C                   ADD       SCEBCE        BOCOCE
260500010420     C                   ADD       SCEBLE        BOCLCE
260600010420     C                   ADD       SCECSE        COSPCE
260700010420     C                   ADD       SCECCE        COCOCE
260800010420     C                   ADD       SCECLE        COCLCE
260900000000     C                   ADD       SCEBSN        BOSPN
261000000000     C                   ADD       SCEBCN        BOCON
261100000000     C                   ADD       SCEBLN        BOCLN
261200000000     C                   ADD       SCECSN        COSPN
261300000000     C                   ADD       SCECCN        COCON
261400000000     C                   ADD       SCECLN        COCLN
261500000000      *
261600000000     C                   ELSE
261700000000      *
261800000000     C                   ADD       SCEBSB        NCSPB
261900000000     C                   ADD       SCEBCB        NCCOB
262000000000     C                   ADD       SCEBLB        NCCLB
262100000000     C                   ADD       SCEBSA        NCSPA
262200000000     C                   ADD       SCEBCA        NCCOA
262300000000     C                   ADD       SCEBLA        NCCLA
262400010417     C                   ADD       SCEBSC        NCSPCB
262500010417     C                   ADD       SCEBCC        NCCOCB
262600010417     C                   ADD       SCEBLC        NCCLCB
262700010417     C                   ADD       SCEBSK        NCSPCC
262800010417     C                   ADD       SCEBCK        NCCOCC
262900010417     C                   ADD       SCEBLK        NCCLCC
263000010420     C                   ADD       SCEBSE        NCSPCE
263100010420     C                   ADD       SCEBCE        NCCOCE
263200010420     C                   ADD       SCEBLE        NCCLCE
263300000000     C                   ADD       SCEBSN        NCSPN
263400000000     C                   ADD       SCEBCN        NCCON
263500000000     C                   ADD       SCEBLN        NCCLN
263600000000      *
263700000000     C                   ENDIF
263800000000      *
263900000000     C                   ENDSR
264000000000      **************************************************************************
264100000000      * Pulizia dei campi per l'elaborazione della ristampa
264200000000      **************************************************************************
264300000000     C     PULIZ         BEGSR
264400000000      *
264500000000     C                   CLEAR                   BOSPB
264600000000     C                   CLEAR                   BOCOB
264700000000     C                   CLEAR                   BOCLB
264800000000     C                   CLEAR                   COSPB
264900000000     C                   CLEAR                   COCOB
265000000000     C                   CLEAR                   COCLB
265100000000     C                   CLEAR                   BOSPA
265200000000     C                   CLEAR                   BOCOA
265300000000     C                   CLEAR                   BOCLA
265400000000     C                   CLEAR                   COSPA
265500000000     C                   CLEAR                   COCOA
265600000000     C                   CLEAR                   COCLA
265700010417     C                   CLEAR                   BOSPCB
265800010417     C                   CLEAR                   BOCOCB
265900010417     C                   CLEAR                   BOCLCB
266000010417     C                   CLEAR                   COSPCB
266100010417     C                   CLEAR                   COCOCB
266200010417     C                   CLEAR                   COCLCB
266300010417     C                   CLEAR                   BOSPCC
266400010417     C                   CLEAR                   BOCOCC
266500010417     C                   CLEAR                   BOCLCC
266600010417     C                   CLEAR                   COSPCC
266700010417     C                   CLEAR                   COCOCC
266800010417     C                   CLEAR                   COCLCC
266900010420     C                   CLEAR                   BOSPCE
267000010420     C                   CLEAR                   BOCOCE
267100010420     C                   CLEAR                   BOCLCE
267200010420     C                   CLEAR                   COSPCE
267300010420     C                   CLEAR                   COCOCE
267400010420     C                   CLEAR                   COCLCE
267500000000     C                   CLEAR                   BOSPN
267600000000     C                   CLEAR                   BOCON
267700000000     C                   CLEAR                   BOCLN
267800000000     C                   CLEAR                   COSPN
267900000000     C                   CLEAR                   COCON
268000000000     C                   CLEAR                   COCLN
268100000000      *
268200000000     C                   CLEAR                   NCSPB
268300000000     C                   CLEAR                   NCCOB
268400000000     C                   CLEAR                   NCCLB
268500000000     C                   CLEAR                   NCSPA
268600000000     C                   CLEAR                   NCCOA
268700000000     C                   CLEAR                   NCCLA
268800010417     C                   CLEAR                   NCSPCB
268900010417     C                   CLEAR                   NCCOCB
269000010417     C                   CLEAR                   NCCLCB
269100010417     C                   CLEAR                   NCSPCC
269200010417     C                   CLEAR                   NCCOCC
269300010417     C                   CLEAR                   NCCLCC
269400010420     C                   CLEAR                   NCSPCE
269500010420     C                   CLEAR                   NCCOCE
269600010420     C                   CLEAR                   NCCLCE
269700000000     C                   CLEAR                   NCSPN
269800000000     C                   CLEAR                   NCCON
269900000000     C                   CLEAR                   NCCLN
270000000000      *
270100000000      * Per il totale filiale
270200000000     C                   CLEAR                   BOSPT
270300000000     C                   CLEAR                   BOCOT
270400000000     C                   CLEAR                   BOCLT
270500000000     C                   CLEAR                   COSPT
270600000000     C                   CLEAR                   COCOT
270700000000     C                   CLEAR                   COCLT
270800000000     C                   CLEAR                   B1SPF
270900000000     C                   CLEAR                   B1COF
271000000000     C                   CLEAR                   B1CLF
271100000000     C                   CLEAR                   C1SPF
271200000000     C                   CLEAR                   C1COF
271300000000     C                   CLEAR                   C1CLF
271400000000     C                   CLEAR                   B2SPF
271500000000     C                   CLEAR                   B2COF
271600000000     C                   CLEAR                   B2CLF
271700000000     C                   CLEAR                   C2SPF
271800000000     C                   CLEAR                   C2COF
271900000000     C                   CLEAR                   C2CLF
272000000000     C                   CLEAR                   B3SPF
272100000000     C                   CLEAR                   B3COF
272200000000     C                   CLEAR                   B3CLF
272300000000     C                   CLEAR                   C3SPF
272400000000     C                   CLEAR                   C3COF
272500000000     C                   CLEAR                   C3CLF
272600010417     C                   CLEAR                   B5SPF
272700010417     C                   CLEAR                   B5COF
272800010417     C                   CLEAR                   B5CLF
272900010417     C                   CLEAR                   C5SPF
273000010417     C                   CLEAR                   C5COF
273100010417     C                   CLEAR                   C5CLF
273200010417     C                   CLEAR                   B6SPF
273300010417     C                   CLEAR                   B6COF
273400010417     C                   CLEAR                   B6CLF
273500010417     C                   CLEAR                   C6SPF
273600010417     C                   CLEAR                   C6COF
273700010417     C                   CLEAR                   C6CLF
273800010420     C                   CLEAR                   B7SPF
273900010420     C                   CLEAR                   B7COF
274000010420     C                   CLEAR                   B7CLF
274100010420     C                   CLEAR                   C7SPF
274200010420     C                   CLEAR                   C7COF
274300010420     C                   CLEAR                   C7CLF
274400000000     C                   CLEAR                   BOSPF
274500000000     C                   CLEAR                   BOCOF
274600000000     C                   CLEAR                   BOCLF
274700000000     C                   CLEAR                   COSPF
274800000000     C                   CLEAR                   COCOF
274900000000     C                   CLEAR                   COCLF
275000000000     C                   CLEAR                   PABOF
275100000000     C                   CLEAR                   PACOF
275200000000      *
275300000000      * Per il totale non controllati
275400000000     C                   CLEAR                   NCSPT
275500000000     C                   CLEAR                   NCCOT
275600000000     C                   CLEAR                   NCCLT
275700000000     C                   CLEAR                   N1SPF
275800000000     C                   CLEAR                   N1COF
275900000000     C                   CLEAR                   N1CLF
276000000000     C                   CLEAR                   N2SPF
276100000000     C                   CLEAR                   N2COF
276200000000     C                   CLEAR                   N2CLF
276300000000     C                   CLEAR                   N3SPF
276400000000     C                   CLEAR                   N3COF
276500000000     C                   CLEAR                   N3CLF
276600010417     C                   CLEAR                   N5SPF
276700010417     C                   CLEAR                   N5COF
276800010417     C                   CLEAR                   N5CLF
276900010417     C                   CLEAR                   N6SPF
277000010417     C                   CLEAR                   N6COF
277100010417     C                   CLEAR                   N6CLF
277200010420     C                   CLEAR                   N7SPF
277300010420     C                   CLEAR                   N7COF
277400010420     C                   CLEAR                   N7CLF
277500000000      *
277600000000     C                   ENDSR
277700000000      **************************************************************************
277800010420** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
277900950213OVRPRTF FILE(FNLSC3P) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.entr.
278000950213')
278100000000** Descrizioni da emettere in stampa
278200000000DISK B
278300000000DISK A
278400000000NO DISK
278500000000TOTALI
278600010420DISK A+C BAR
278700010417DISK C CLI
278800010420DISK C BAR
