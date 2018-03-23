000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200000000      *****************************************************************
000300950117      *  Nome programma:  FNLSC5R
000400000000      *  Descrizione   :  Stampa statistica bollettato
000500000000      *                   Programma esecuzione (stampa)
000600000000      *  Data creazione:  04 GEN 1994
000700000000      *****************************************************************
000800950117      *  Il pgm puo' essere lanciato dal filtro FNLSC0R oppure tramite
000900000000      *  le funzioni giornaliere, insieme ad altre stampe
001000000000      *****************************************************************
001100950216      *  ??PASSAGGIO PARAMETRI:?
001200950216      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001300950216      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001400970207      *  DS1FPG   TIPO STASTISTICA
001500950216      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001600950216      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA
001700950216      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001800950216      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
001900950216      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
002000950216      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002100950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
002200950216      *****************************************************************
002300950213      *  N.B.: A seconda del parametro DS1RIS (stampa/ristampa) il programma
002400000000      *        segue due strade diverse per generare l'output:
002500000000      *        . se richiesta la STAMPA, ci sara' l'elaborazione dei dati con
002600950213      *          letture dai file bolle etc.; pero' le due date limite, DS1ADA
002700950213      *          e DS1AAL devono essere coincidenti;
002800000000      *          non possibile l'elaborazione in giorni festivi;
002900000000      *        . se richiesta la RISTAMPA, non ci sara' l'elaborazione ma la
003000990507      *          semplice lettura (e stampa) del file FNSBO00F, nel quale sono
003100000000      *          gia' presenti statistiche elaborate precedentemente; in questo
003200000000      *          caso si potra' chiedere un periodo dal-al.
003300000000      *****************************************************************
003400000000     FAZORG01L  IF   E           K DISK
003500010717     FFNBLP31L  IF   E           K DISK    USROPN
003600990507     FFNSBO01L  UF A E           K DISK
003700000000     FTABEL00F  IF   E           K DISK
003800970312     FAZCLN01L  IF   E           K DISK
003900021016     fTntbe01l  uf   e           k Disk
004000950117     FFNLSC5P   O    E             PRINTER OFLIND(*IN30)
004100950117     F                                     USROPN
004200000000      *
004300000000     D L1              S              3  0 DIM(30)
004400020418     D L6              S              3  0 DIM(30)
004500950213     D LV2             S              3  0 DIM(30)
004600000615     D LIN             S              3  0 DIM(30)
004700000615     D SVLIN           S              3  0 DIM(30)
004800950308     D CMD             S              1    DIM(82) CTDATA PERRCD(80)
004900970312     D GME             S              1    DIM(31)
005000000000      * Le schiere che seguono hanno quattro elementi:
005100000000      * il primo:    per la filiale
005200000000      * il secondo:  per i conto servizi di filiale
005300000000      * il terzo:    per i conto servizi dell'area
005400000000      * il quarto:   per i totali area
005500000000      * il quinto:   per i totali generali
005600000000      * il sesto:    per la filiale        (sospensione)
005700000000      * il settimo:  per i conto servizi   (sospensione)
005800000000     D COL             S              7  0 DIM(7)
005900000000     D PF3             S              9  1 DIM(7)
006000000000     D PO3             S              9  1 DIM(7)
006100000000     D PTA             S              9  1 DIM(7)
006200000000     D VOL             S              9  4 DIM(7)
006300000000     D SEF             S              7  0 DIM(7)
006400000000     D SEA             S              7  0 DIM(7)
006500000000     D SNF             S              7  0 DIM(7)
006600000000     D SNA             S              7  0 DIM(7)
006700000000     D SSF             S              7  0 DIM(7)
006800000000     D SSA             S              7  0 DIM(7)
006900000000     D SRF             S              7  0 DIM(7)
007000000000     D SRA             S              7  0 DIM(7)
007100020418
007200020418     d sav_t06         S              3  0 dim(30)
007300020418
007400020418     d wc              s              3  0
007500021016
007600021016     d kTbeCod         s                   Like(TbeCod)
007700021016     d kTbeKe1         s                   Like(TbeKe1)
007800050829     d klin            s                   Like(ds1lnp)
007900020418
008000000000      *
008100090604     d ds5e          e ds
008200000000     D KPJBA         E DS
008300000000     D DS3A          E DS
008400000000     D DS3I          E DS
008500000000     D DS3L          E DS
008600040922     D*DS5C          E DS
008700970707     D DS5P          E DS
008800950117     D DSQT1         E DS
008900000000     D CNCR80        E DS
009000000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009100950213     D* DS PER TRUL06R - CARICAMENTO £X
009200950213     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009300950314     D  T06                    1     90  0
009400950314     D                                     DIM(30)
009500000000      * DS per il passaggio parametri
009600950213     D DSLS01        E DS
009700970312     D  ANNODA                41     44  0
009800970312     D  MESEDA                45     46  0
009900970312     D  $GI                   47     48  0
010000950213     D                 DS
010100950117      * DATA INIZIALE AAAA/MM/GG (8 CARATTERI)
010200950213     D  DATADA                 1      8  0
010300950213     D  $ANNDA                 1      4  0
010400950213     D  $MGSDA                 5      8  0
010500000000      * DS per data di spedizione della bolla
010600000000     D                 DS
010700950117     D  $DATSP                 1      8  0
010800950117     D  BLPAAS                 1      4  0
010900950117     D  BLPMGS                 5      8  0
011000040922
011100040922     D WLBDAT          DS                  INZ
011200040922     D  G02DAT                 1      8  0
011300040922     D  G02INV                 9     16  0
011400040922     D  G02ERR                17     17
011500040922     D  G02TGI                18     22  0
011600000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
011700000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
011800021016      * Ds date statistiche
011900021016     d dSdf          e ds
012000000000      *
012100000000     C     *ENTRY        PLIST
012200000000     C                   PARM                    KPJBA
012300950213     C                   MOVEL     KPJBU         DSLS01
012400950213     C                   Z-ADD     DS1ADA        DATADA
012500000000      *
012600010717     C     FNBLPK        KLIST
012700000000     C                   KFLD                    BLPLNP
012800000000     C                   KFLD                    $ANNDA
012900000000     C                   KFLD                    $MGSDA
013000990507     C     FNSBOK        KLIST
013100000000     C                   KFLD                    SBOSOS
013200000000     C                   KFLD                    SBOFLE
013300000000     C                   KFLD                    SBOLNP
013400000000     C                   KFLD                    SBODRE
013500990507     C     FLSB1K        KLIST
013600000000     C                   KFLD                    SBOSOS
013700000000     C                   KFLD                    SBOFLE
013800000000     C                   KFLD                    SBOLNP
013900990507     C     FLSB2K        KLIST
014000000000     C                   KFLD                    SBOSOS
014100000000     C                   KFLD                    SBOFLE
014200050829     C     ksbo          KLIST
014300050829     C                   KFLD                    SBOSOS
014400050829     C                   KFLD                    SBOFLE
014500050829     C                   KFLD                    klin
014600050829     C                   KFLD                    sbodre
014700050829     C     ksbo2         KLIST
014800050829     C                   KFLD                    SBOSOS
014900050829     C                   KFLD                    SBOFLE
015000050829     C                   KFLD                    klin
015100970312     C*
015200970312     C     KCLN          KLIST
015300970312     C                   KFLD                    SIMFEL
015400970312     C                   KFLD                    $TFA
015500970312     C                   KFLD                    ANNODA
015600970312     C                   KFLD                    MESEDA
015700000000      *
015800000615     C     *LIKE         DEFINE    DS1RIS        SAVRIS
015900000615     C     *LIKE         DEFINE    BLPLNP        $FILPR
016000000000     C     *LIKE         DEFINE    BLPLNP        $FI2PR
016100950117     C     *LIKE         DEFINE    BLPCBO        $CBOPR
016200000000     C     *LIKE         DEFINE    §3ARBL        $RBL
016300970312     C     *LIKE         DEFINE    SIMFEL        $TFA
016400000000      *****************************************************************
016500000000      *              M A I N      L I N E
016600000000      *****************************************************************
016700000000     C                   Z-ADD     1             CODUT
016800000000      *
016900950117      * LEGGO LA DSQT1 PER IL RAPPORTO PESO/VOLUME
017000950213     C                   MOVEL     'QT'          KCOD
017100950213     C                   MOVEL     '1       '    KKEY
017200950213     C     KTAB1         CHAIN     TABEL00F                           99
017300950117     C  N99              MOVEL     TBLUNI        DSQT1
017400950117     C   99              CLEAR                   DSQT1
017500000000      *
017600000000      * Reperisco la tabella 3I per calcoli che serviranno poi
017700950213     C                   MOVEL     '3I'          KCOD
017800950213     C                   MOVEL     '1       '    KKEY
017900950213     C     KTAB1         CHAIN     TABEL00F                           99
018000000000     C  N99              MOVEL     TBLUNI        DS3I
018100000000     C   99              CLEAR                   DS3I
018200000000     C   99              Z-ADD     20            §3IARR
018300000000      *
018400000000      * Reperimento tabella per trasmissione archivio statistiche
018500040922     C***                MOVE      '5C'          KCOD
018600040922     C***                MOVE      '1       '    KKEY
018700040922     C***  KTAB1         CHAIN     TABEL00F                           99
018800040922     C**N99              MOVEL     TBLUNI        DS5C
018900040922     C***99              CLEAR                   DS5C
019000000000      *
019100090604      * carico le descrizioni fisse dei tipi servizio
019200090604      * chiodo la descrizione per ogni tipo servizio visto che la descrizione
019300090604      * è da stampare nella testata ed è fissa
019400090604      * le colonne dei tipi servizio da stampare sono
019500090604      * 'C + D' 'E'
019600090604     c                   clear                   ds5e
019700090604     c                   clear                   wdtspc
019800090604     c                   clear                   wdtspd
019900090604     c                   clear                   wdtspe
020000090604     c                   eval      kcod = '5E'
020100090604     c     ktab2         setll     tabel00f
020200090604     c                   do        *hival
020300090604     c     ktab2         reade     tabel00f
020400090604     c                   if        %eof(tabel00f)
020500090604     c                   leave
020600090604     c                   endif
020700090604     c                   eval      ds5e = tbluni
020800090604     c                   if        tblkey = 'C'
020900090604     c                   eval      wdtspc = §5ed08
021000090604     c                   endif
021100090604     c                   if        tblkey = 'D'
021200090604     c                   eval      wdtspd = §5ed08
021300090604     c                   endif
021400090604     c                   if        tblkey = 'E'
021500090604     c                   eval      wdtspe = §5ed08
021600090604     c                   endif
021700090604     c                   enddo
021800090604
021900000000     C                   MOVE      'RISTAMPA'    STRIP
022000950213     C     DS1RIS        IFNE      'R'
022100000000     C                   MOVE      'STAMPA  '    STRIP
022200010717     C                   OPEN      FNBLP31L
022300000000     C                   ENDIF
022400950213     C     DS1RIS        COMP      *BLANKS                                36
022500000000      *
022600000000      * Scrivo, in stampa, se selezionata filiale o area
022700000000     C                   MOVE      *BLANKS       STRIP1
022800950213     C     DS1LNP        IFEQ      *ZERO
022900000000     C                   MOVEL     'AREA'        STRIP1
023000950213     C                   MOVE      DS1FEL        STRIP1
023100000000     C                   ELSE
023200960926     C                   MOVEL     'P.O.'        STRIP1
023300950213     C                   MOVE      DS1LNP        STRIP1
023400000000     C                   ENDIF
023500000000      *
023600000000      * Se la data AL e' vuota la imposto come la data DAL
023700950213     C     DS1AAL        IFEQ      *ZERO
023800950213     C                   Z-ADD     DS1ADA        DS1AAL
023900950213     C                   Z-ADD     DS1GDA        DS1GAL
024000000000     C                   ENDIF
024100000000      *
024200000000      * Verifico se richiesto un solo giorno o un periodo dal-al
024300950213     C     DS1ADA        COMP      DS1AAL                                 44
024400000000      *
024500950213      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
024600950213     C                   EXSR      COMUNE
024700000000      *
024800000000     C                   SETON                                        LR
024900020212     C*****************************************************************
025000020212     C*  PARTE COMUNE STAMPE GIORNALIERE
025100020212     C*
025200020212     C*  . REPERIMENTO 'SIMFEL'
025300020212     C*  . CARICAMENTO '£1'
025400020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
025500020212     C*  . CODA DI STAMPA ('£OUTQ')
025600020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
025700020212     C*  . MODULO PER LA STAMPA ('£MOD')
025800020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
025900020212     C*****************************************************************
026000020212     C     COMUNE        BEGSR
026100020212     C*
026200020212     C*****************************************************************
026300020212     C*       RIEPILOGO INDICATORI:
026400020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
026500020212     C* 11    -           STAMPA PER 1L
026600020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
026700050829     C* 32 e 37 PER ristampa
026800020212     C*****************************************************************
026900020212     C                   Z-ADD     1             CODUT
027000020212     C*
027100020212     C* Reperimento parametri aziendali
027200020212     C                   CALL      'X§PARUT'
027300020212     C                   PARM                    UT§DSE
027400020212     C                   MOVEL     RAGUT         RSUT             20
027500020212     C                   MOVEL     REC80         CNCR80
027600020212     C*
027700020212     C* Chiave di accesso per il file tabelle
027800020212     C     KTAB1         KLIST
027900020212     C                   KFLD                    CODUT
028000020212     C                   KFLD                    KCOD
028100020212     C                   KFLD                    KKEY
028200020212     C     KTAB2         KLIST
028300020212     C                   KFLD                    CODUT
028400020212     C                   KFLD                    KCOD
028500021016
028600021016     c     Ktbe          Klist
028700021016     c                   Kfld                    kTbeCod
028800021016     c                   Kfld                    kTbeKe1
028900020212     C*
029000020212     C     *LIKE         DEFINE    TBLCOD        KCOD
029100020212     C     *LIKE         DEFINE    TBLKEY        KKEY
029200020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
029300020212     C*
029400020212     C* DETERMINO DATA E ORA DI ESECUZIONE
029500020212     C                   TIME                    WTIME            14 0
029600020212     C                   MOVE      WTIME         WDATE             8 0
029700020212     C                   MOVEL     WTIME         UTIME             6 0
029800040922     c*
029900040922     C                   CLEAR                   WLBDAT
030000040922     C                   MOVE      Wtime         G02DAT
030100040922     C                   CALL      'XSRDA8'
030200040922     C                   PARM                    WLBDAT
030300040922     C                   Z-ADD     G02INV        dateu             8 0
030400020212     C*
030500020418     C* CARICO '£1'
030600020212     C                   CLEAR                   KPJBU
030700020212     C                   CLEAR                   DSUL06
030800020212     C                   MOVE      '£1'          D06COD
030900020212     C                   MOVEL     SIMFEL        D06KEY
031000050802     C                   MOVE      'S'           D06KEY
031100050802     C                   MOVEL     ds1ada        D06drf
031200020212     C                   MOVEL     DSUL06        KPJBU
031300020212     C                   CALL      'TRUL06R'
031400020212     C                   PARM                    KPJBA
031500020212     C                   MOVEL     KPJBU         DSUL06
031600020418     c                   movea     t06           sav_t06
031700020418
031800020418      * se in arrivo carico la £6
031900020418 b1  c                   if        ds1tbo = 'A'
032000020418     c                   clear                   wc
032100020418 b2  c                   do        30            wa
032200020422     c                   if        sav_t06(wa) = *zeros
032300020422     c                   leave
032400020422     c                   endif
032500020418     c                   clear                   kpjbu
032600020418     c                   clear                   dsul06
032700020418     c                   eval      d06cod = '£6'
032800020418     c                   movel     sav_t06(wa)   d06key
032900050802     C                   MOVE      'S'           D06KEY
033000020418     c                   eval      d06esc = 'S'
033100050802     C                   MOVEL     ds1ada        D06drf
033200020419     c                   movel     dsul06        kpjbu
033300020418     c                   call      'TRUL06R'
033400020418     c                   parm                    kpjba
033500020418     c                   movel     kpjbu         dsul06
033600020418 b3  c                   do        30            wb
033700020418     c                   if        t06(wb) = *zeros
033800020418     c                   leave
033900020418     c                   endif
034000020418     c                   if        t06(wb) <> sav_t06(wa)
034100020418     c                   add       1             wc
034200020418     c                   z-add     t06(wb)       l6(wc)
034300020418     c                   endif
034400020418 e3  c                   enddo
034500020418 e2  c                   enddo
034600020418
034700020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
034800020212     C                   Z-ADD     *ZERO         WB
034900020418 b2  C                   DO        29            WA
035000020418     C     sav_T06(WA)   LOOKUP    L6                                     30
035100020212     C  N30              ADD       1             WB
035200020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
035300020418 e2  C                   ENDDO
035400020418 x1  C                   ELSE
035500020418     C                   MOVEA     sav_T06       L1
035600020418 e1  C                   ENDIF
035700020212     C*
035800020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
035900020212     C                   MOVEA     L1            LIN
036000020212     C                   CLEAR                   L1
036100020212     C                   Z-ADD     *ZERO         WB                3 0
036200020212     C     1             DO        30            WPT               3 0
036300020212     C     LIN(WPT)      IFNE      0
036400020212     C                   CLEAR                   FNLSC4
036500020212     C                   Z-ADD     LIN(WPT)      IC4LIN
036600020212     C                   MOVE      'B'           IC4FLG
036700020212     C                   MOVEL     FNLSC4        KPJBU
036800020212     C                   CALL      'FNLSC4R1'
036900020212     C                   PARM                    KPJBA
037000050802     C                   PARM                    ds1ada
037100020212     C                   MOVEL     KPJBU         FNLSC4
037200020212     C     OC4ESE        IFEQ      'S'
037300020212     C                   ADD       1             WB
037400020212     C                   MOVE      LIN(WPT)      L1(WB)
037500020212     C                   ENDIF
037600020212     C                   ENDIF
037700020212     C                   ENDDO
037800070103     C*
037900070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
038000070103     C                   Z-ADD     *ZERO         WB                3 0
038100070103     C                   DO        29            WA                3 0
038200070103     C     L1(WA)        IFNE      *ZEROS
038300070103     C     L1(WA)        ANDNE     SIMFEL
038400070103     C                   ADD       1             WB
038500070103     C                   Z-ADD     L1(WA)        LV2(WB)
038600070103     C                   ENDIF
038700070103     C                   ENDDO
038800020212     C*
038900020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
039000020212     C*
039100020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
039200020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
039300020212     C* la stampa e' lanciata da menu
039400020212     C                   MOVEL     '3L'          KCOD
039500020212     C                   MOVEL     DS1PGM        KKEY
039600020212     C                   MOVE      DS1FPG        KKEY
039700020212     C     KTAB1         CHAIN     TABEL00F                           30
039800020212     C  N30              MOVEL     TBLUNI        DS3L
039900020212     C   30              MOVEL     'F'           §3LD1L
040000020212     C   30              MOVEL     ' '           §3LD2L
040100020212     C   30              MOVEL     '*STD    '    §3LDST
040200020212     C*
040300020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
040400020212     C                   MOVEL     §3LDST        £MOD             10
040500020212     C                   CLEAR                   LIN
040600020212     C*
040700020212     C* Verifico chi ha lanciato la stampa
040800020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
040900020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
041000020212     C* DS1FGS<>0  --->  si proviene da menu
041100020212     C     DS1FGS        IFEQ      0
041200020212     C*
041300020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
041400020212     C* lancio, voglio la stampa;  invece
041500020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
041600020212     C*
041700020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
041800020212     C* creare stampa per 1°lvl. (se stessa)
041900020212     C     DS1LNP        IFEQ      SIMFEL
042000020212     C*
042100020212     C* Il flag §3LD1L ha i seguenti significati:
042200020212     C*    ' ' = tutti i dati
042300020212     C*    '1' = solo filiale di 1' livello
042400020212     C     §3LD1L        IFEQ      '1'
042500020212     C                   SETON                                        11
042600020212     C                   END
042700020212     C*
042800020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
042900020212     C* nella stampa
043000020212     C*
043100020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
043200020212     C  N11              MOVEA     L1            LIN
043300020212     C   11              MOVEA     SIMFEL        LIN(1)
043400020212     C*
043500020212     C* IMPOSTO LA STAMPANTE:
043600020212     C* STAMPANTE TABULATI
043700020212     C                   MOVEL     '5P'          KCOD
043800020212     C                   MOVEL     'T'           W004A             4
043900020212     C                   MOVE      LIN(1)        W004A
044000020212     C                   MOVEL(P)  W004A         KKEY
044100020212     C     KTAB1         CHAIN     TABEL00F                           99
044200020212     C     *IN99         IFEQ      *OFF
044300020212     C                   MOVEL     TBLUNI        DS5P
044400020212     C                   MOVEL     §5POTQ        £OUTQ            10
044500020212     C                   END
044600020212     C*
044700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
044800020212     C                   EXSR      LEGGI
044900020212     C*
045000020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
045100020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
045200020212     C                   ELSE
045300020212     C*
045400020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
045500020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
045600020212     C*
045700020212     C* Il significato del fla §3LD2L e' il seguente:
045800020212     C*    ' ' = tutti i dati
045900020212     C*    'F' = per ogni 2' livello
046000020212     C     §3LD2L        IFEQ      'F'
046100020212     C                   SETON                                        14
046200020212     C                   ENDIF
046300020212     C*
046400020212     C                   Z-ADD     1             WB
046500020212     C                   MOVEA     *ZEROS        LIN
046600020212     C  N14              MOVEA     L1            LIN
046700020212     C*
046800020212     C* Lancio per tutti i 2° lvl
046900020212     C     DS1LNP        IFEQ      0
047000020212     C*
047100020212     C* LV2 contiene tutte le filiali di 2° lvl
047200020212     C     LV2(WB)       DOWNE     0
047300020212      *
047400020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
047500020212     C                   CLEAR                   FNLSC4
047600020212     C                   Z-ADD     LV2(WB)       IC4LIN
047700020212     C                   MOVEL     FNLSC4        KPJBU
047800020212     C                   CALL      'FNLSC4R1'
047900020212     C                   PARM                    KPJBA
048000050802     C                   PARM                    ds1ada
048100020212     C                   MOVEL     KPJBU         FNLSC4
048200020212      *
048300020212     C     OC4PT1        IFEQ      'S'
048400020212     C     §3LPT         ANDEQ     'N'
048500020212     C                   GOTO      NOLEGG
048600020212     C                   ENDIF
048700020212      *
048800020212     C     OC4EXP        IFEQ      'S'
048900020212     C     §3LEXP        ANDEQ     'N'
049000020212     C                   GOTO      NOLEGG
049100020212     C                   ENDIF
049200020212      *
049300020212     C     OC4DPD        IFEQ      'S'
049400020212     C     §3LDPD        ANDEQ     'N'
049500020212     C                   GOTO      NOLEGG
049600020212     C                   ENDIF
049700020212      *
049800020212     C     OC4BAR        IFEQ      'S'
049900020212     C     §3LBAR        ANDEQ     'N'
050000020212     C                   GOTO      NOLEGG
050100020212     C                   ENDIF
050200020212      *
050300020212     C     OC4FED        IFEQ      'S'
050400020212     C     §3LFED        ANDEQ     'N'
050500020212     C                   GOTO      NOLEGG
050600020212     C                   ENDIF
050700020212      *
050800020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
050900020212     C*  dati degli altri 2° lvl
051000020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
051100020212     C   14              MOVEL     LV2(WB)       LIN(1)
051200020212     C*
051300020212     C* IMPOSTO LA STAMPANTE
051400020212     C* STAMPANTE TABULATI
051500020212     C                   MOVEL     '5P'          KCOD
051600020212     C                   MOVEL     'T'           W004A             4
051700020212     C                   MOVE      LV2(WB)       W004A
051800020212     C                   MOVEL(P)  W004A         KKEY
051900020212     C     KTAB1         CHAIN     TABEL00F                           99
052000020212     C     *IN99         IFEQ      *OFF
052100020212     C                   MOVEL     TBLUNI        DS5P
052200020212     C                   MOVEL     §5POTQ        £OUTQ            10
052300020212     C                   END
052400020212     C*
052500020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
052600020212     C                   EXSR      LEGGI
052700020212     C*
052800020212     C     NOLEGG        TAG
052900020212     C*
053000020212     C                   ADD       1             WB
053100020212     C                   ENDDO
053200020212     C*
053300020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
053400020212     C                   ELSE
053500020212     C*
053600020212     C* Lancio solo per la 2° lvl richiesta
053700020212     C   14              MOVEL     DS1LNP        LIN(1)
053800020212     C*
053900020212     C* STAMPANTE
054000020212     C* STAMPANTE TABULATI
054100020212     C                   MOVEL     '5P'          KCOD
054200020212     C                   MOVEL     'T'           W004A             4
054300020212     C                   MOVE      DS1LNP        W004A
054400020212     C                   MOVEL(P)  W004A         KKEY
054500020212     C     KTAB1         CHAIN     TABEL00F                           99
054600020212     C     *IN99         IFEQ      *OFF
054700020212     C                   MOVEL     TBLUNI        DS5P
054800020212     C                   MOVEL     §5POTQ        £OUTQ            10
054900020212     C                   END
055000020212     C*
055100020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
055200020212     C                   EXSR      LEGGI
055300020212     C*
055400020212     C                   ENDIF
055500020212     C                   ENDIF
055600020212     C*
055700020212     C                   ELSE
055800020212     C*
055900020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
056000020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
056100020212     C     DS1LNP        IFEQ      0
056200020212     C                   MOVEA     L1            LIN
056300020212     C* ...altrimenti elaboro solo se stessa
056400020212     C                   ELSE
056500020212     C                   MOVEL     DS1LNP        LIN(1)
056600020212     C                   ENDIF
056700020212     C*
056800020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
056900020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
057000020212     C* PRENDO QUELLA PRESENTE IN KPJBA
057100020212     C     DS1FGS        IFEQ      SIMFEL
057200020212     C                   MOVEL     KQOTD         £OUTQ
057300030627     C**!!!              MOVEL     KQOTD         £OUTQF
057400020212     C                   ELSE
057500020212     C* STAMPANTE TABULATI
057600020212     C                   MOVEL     '5P'          KCOD
057700020212     C                   MOVEL     'T'           W004A             4
057800020212     C                   MOVE      DS1FGS        W004A
057900020212     C                   MOVEL(P)  W004A         KKEY
058000020212     C     KTAB1         CHAIN     TABEL00F                           99
058100020212     C     *IN99         IFEQ      *OFF
058200020212     C                   MOVEL     TBLUNI        DS5P
058300020212     C                   MOVEL     §5POTQ        £OUTQ            10
058400020212     C                   END
058500020212     C                   ENDIF
058600020212     C*
058700020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
058800020212     C                   EXSR      LEGGI
058900020212     C                   ENDIF
059000020212     C*
059100020212     C                   ENDSR
059200000000      *****************************************************************
059300000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
059400000000      *****************************************************************
059500000000     C     LEGGI         BEGSR
059600000615     C                   CLEAR                   SAVRIS
059700000000      *
059800000000      * Se sono nelle stampe giornaliere non voglio diciture
059900950213     C     DS1FGS        IFEQ      *ZERO
060000000000     C     SIMFEL        ANDNE     *ZERO
060100000000     C                   MOVE      *BLANKS       STRIP
060200000000     C                   MOVE      *BLANKS       STRIP1
060300000000     C                   ENDIF
060400000000      *
060500000000      * Cancello dal file statistiche i record della giornata che vado a elabor.
060600950213     C     DS1RIS        IFNE      'R'
060700000615     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
060800000615     C                   CLEAR                   KPJBU
060900000615     C                   CLEAR                   DSUL06
061000000615     C                   MOVE      '£1'          D06COD
061100000615     C                   MOVEL     SIMFEL        D06KEY
061200050802     C                   MOVE      'S'           D06KEY
061300050802     C                   MOVEL     ds1ada        D06drf
061400000615     C                   MOVEL     DSUL06        KPJBU
061500000615     C                   CALL      'TRUL06R'
061600000615     C                   PARM                    KPJBA
061700000615     C                   MOVEL     KPJBU         DSUL06
061800000615     C**
061900000615     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
062000000615     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
062100000615     C*  CHE USO ALLA FINE
062200000615     C     DS1RIS        IFNE      ' '
062300000615     C                   MOVEA     LIN           SVLIN
062400000615     C                   MOVEL     DS1RIS        SAVRIS
062500000615     C                   CLEAR                   DS1RIS
062600000615     C                   ENDIF
062700000615     C                   MOVEA     T06           LIN
062800000615     C**
062900000000     C                   MOVE      *BLANKS       SBOSOS
063000000000     C                   DO        2
063100000000     C                   Z-ADD     SIMFEL        SBOFLE
063200950213     C                   Z-ADD     DS1ADA        SBODRE
063300000000      *
063400000000      * Elimino solo le linee della LIN (che dovro' poi elaborare)
063500000000     C                   Z-ADD     1             XX
063600000000     C     LIN(XX)       DOWGT     *ZERO
063700000000      *
063800000000     C                   Z-ADD     LIN(XX)       SBOLNP
063900950213     C                   Z-ADD     DS1ADA        SBODRE
064000990507     C     FNSBOK        SETLL     FNSBO01L
064100000000     C                   DO        *HIVAL
064200990507     C     FLSB1K        READE     FNSBO01L                               99
064300990507     C  N99SBODRE        COMP      DS1AAL                             99
064400990507     C  N99              DELETE    FNSBO000
064500000000     C  N99              ENDDO
064600000000      *
064700000000     C                   ADD       1             XX
064800000000     C                   ENDDO
064900000000      *
065000000000     C                   MOVE      'S'           SBOSOS
065100000000     C                   ENDDO
065200000000     C                   ENDIF
065300950308      *
065400950308      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
065500950308     C                   MOVEA     £MOD          CMD(33)
065600950308     C                   MOVEA     £OUTQ         CMD(51)
065700000000      *
065800000000      * Eseguo OVRPRTF sul file di stampa
065900950308     C                   Z-ADD     82            LUNG             15 5
066000950308     C                   MOVEL     *BLANKS       COMMAN           82
066100000000     C                   MOVEA     CMD           COMMAN
066200000000     C                   CALL      'QCMDEXC'                            H1
066300000000     C                   PARM                    COMMAN
066400000000     C                   PARM                    LUNG
066500000000      *
066600000000     C                   SETON                                        43
066700000000     C                   EXSR      STAINI
066800000000      *
066900950404      * L'elaborazione solo se scelta la stampa (DS1RIS='S')
067000000615    1C     DS1RIS        IFNE      'R'
067100970312     C* Verifico su calendario se giorno festivo e lo memorizzo
067200990507     C* (Mi serve per sapere il tipo record da memorizzare in FNSBO)
067300970312     C                   CLEAR                   $TFA
067400970312     C                   CLEAR                   WFES
067500970312     C     KCLN          CHAIN     AZCLN01L                           98
067600970312     C     *IN98         IFEQ      *OFF
067700970312     C                   MOVEA     CLNMAT        GME
067800970312     C                   MOVE      GME($GI)      WFES              1
067900970312     C                   END
068000970312     C*
068100000000     C                   Z-ADD     *ZERO         $PRIMA            1 0
068200000000     C                   Z-ADD     *ZERO         $FILPR
068300950117     C                   MOVE      *BLANKS       $CBOPR
068400000000      *
068500000000      * Considero solamente le filiali presenti in LIN
068600000000     C                   Z-ADD     1             XX                3 0
068700000000     C     LIN(XX)       DOWGT     *ZERO
068800000000      *
068900000000     C                   Z-ADD     LIN(XX)       BLPLNP
069000970312     C*
069100970312      * Al primissimo giro memorizzo la filiale come precedente
069200970312     C     $PRIMA        IFEQ      *ZERO
069300970312     C                   Z-ADD     BLPLNP        $FILPR
069400970312     C                   Z-ADD     1             $PRIMA
069500970312     C                   ENDIF
069600970312      *
069700970312      * Per prima cosa controllo la rottura per filiale
069800970312     C     BLPLNP        IFNE      $FILPR
069900970312     C                   Z-ADD     1             $X
070000970312     C                   EXSR      TOTALE
070100970312     C                   Z-ADD     BLPLNP        $FILPR
070200970312     C                   ENDIF
070300970312     C*
070400010717     C     FNBLPK        SETLL     FNBLP31L
070500000000      *
070600000000     C                   DO        *HIVAL
070700010717     C   44FNBLPK        READE     FNBLP31L                               33
070800010717     C  N44LIN(XX)       READE     FNBLP31L                               33
070900000000      *
071000000000      * Per diminuire le letture, quando supero la data finale cambio LIN,XX
071100950213     C  N33
071200950213     CANN44$DATSP        COMP      DS1AAL                             33
071300000000      *
071400950213     C  N33$DATSP        IFGE      DS1ADA
071500950213     C     $DATSP        ANDLE     DS1AAL
071600950126     C*
071700950126     C* ESCLUDO I TIPI BOLLA CHE INIZIANO CON '£' (QUESTO PER FREGARE
071800950126     C* WANDA CON LE SUE BOLLE FANTASMA);  ANCHE SE RISULTANO COME
071900950126     C* C/SERVIZI NON HANNO COLLI E NON DEVO CONSIDERARLE
072000950126     C                   MOVEL     BLPCBO        WUNO              1
072100950126     C     WUNO          IFNE      '£'
072200000000     C                   EXSR      ELABOR
072300950126     C                   ENDIF
072400000000     C                   ENDIF
072500000000      *
072600000000     C  N33              ENDDO
072700000000      *
072800000000     C                   ADD       1             XX
072900000000     C                   ENDDO
073000000000      *
073100000000     C                   Z-ADD     1             $X                1 0
073200000000     C                   EXSR      TOTALE
073300000615    1C                   ENDIF
073400000615     C*
073500000615     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
073600000615     C     SAVRIS        IFEQ      'S'
073700000616     C                   MOVEL     'R'           DS1RIS
073800000615     C                   MOVEA     SVLIN         LIN
073900000615     C                   SETON                                        43
074000000615     C                   EXSR      STAINI
074100000616      * CANCELLO I CAMPI DI TOTALE PERHCE' ADESSO LI RISOMMA
074200000616     C                   Z-ADD     1             $X
074300000616     C                   DO        7             $X
074400000616     C                   Z-ADD     *ZERO         COL($X)
074500000616     C                   Z-ADD     *ZERO         PF3($X)
074600000616     C                   Z-ADD     *ZERO         PO3($X)
074700000616     C                   Z-ADD     *ZERO         PTA($X)
074800000616     C                   Z-ADD     *ZERO         VOL($X)
074900000616     C                   Z-ADD     *ZERO         SEF($X)
075000000616     C                   Z-ADD     *ZERO         SEA($X)
075100000616     C                   Z-ADD     *ZERO         SNF($X)
075200000616     C                   Z-ADD     *ZERO         SNA($X)
075300000616     C                   Z-ADD     *ZERO         SSF($X)
075400000616     C                   Z-ADD     *ZERO         SSA($X)
075500000616     C                   Z-ADD     *ZERO         SRF($X)
075600000616     C                   Z-ADD     *ZERO         SRA($X)
075700000616     C                   ENDDO
075800000615     C                   ENDIF
075900000615     C*
076000000615      * STAMPA O RISTAMPA
076100000615     C     DS1RIS        IFNE      ' '
076200000000     C                   SETON                                        43
076300000000     C                   EXSR      RISTAM
076400000000     C                   EXSR      STAFIN
076500000000     C                   SETOFF                                       43
076600000000     C                   EXSR      STAINI
076700000000     C                   EXSR      RISTAM
076800000000     C                   ENDIF
076900000000      *
077000000000     C                   EXSR      STAFIN
077100000000      *
077200000000     C                   ENDSR
077300000000      *****************************************************************
077400000000      * Elaborazione del record-bolla letto
077500000000      *****************************************************************
077600000000     C     ELABOR        BEGSR
077700000000      *
077800000000      * Verifico se si tratta di c/servizio o recupero
077900950117     C     BLPCBO        IFNE      $CBOPR
078000950213     C                   MOVE      '3A'          KCOD
078100950213     C                   MOVE      *BLANKS       KKEY
078200950213     C                   MOVEL     BLPCBO        KKEY
078300950213     C     KTAB1         CHAIN     TABEL00F                           99
078400000000     C  N99              MOVEL     TBLUNI        DS3A
078500950117     C  N99              MOVE      BLPCBO        $CBOPR
078600000000     C   99              CLEAR                   DS3A
078700000000     C                   MOVE      §3ARBL        $RBL
078800000000     C                   MOVEL     §3ATB1        $FRAS             1
078900000000     C                   ENDIF
079000000000      *
079100000000      * Definisco il valore gli indici di schiera
079200000000     C                   Z-ADD     1             $Z                1 0
079300000000     C     $RBL          IFEQ      'C'
079400000000     C                   Z-ADD     2             $Z
079500000000     C                   ENDIF
079600000000     C     $Z            ADD       5             $Y                1 0
079700000000      *
079800000000      * Verifico se si tratta di bolla in sospensione
079900000000      * ( Data ritiro merce < Data spedizione )
080000000000     C     BLPDRT        COMP      $DATSP                               34
080100000000      *
080200000000      * Verifico se "Porto franco" o "Porto assegnato"
080300000000     C     $FRAS         COMP      'F'                                    35
080400000000      *
080500000000      * Colli, peso e volume non sono validi se bolla di recupero
080600000000     C     $RBL          IFNE      'R'
080700000000     C                   ADD       BLPNCL        COL($Z)
080800000000     C   34              ADD       BLPNCL        COL($Y)
080900021217      *
081000021217      * Occhio al peso nella bolla!!     Sulla bolla devo verificare i dati
081100021217      * misurati con la macchina smistacolli (BLPNCP=Numero colli rilevati,
081200021217      * BLPPKC=Peso rilevato)
081300050112     c                   if           BLPncp = BLPncl
081400021217     c                             or BLPpkc >= BLPpkf
081500021217     c                   z-add     BLPpkc        BLPpkf
081600021217e   2c                   endif
081700000000      *
081800950117     C     BLPPKF        IFLE      3500
081900950117     C                   ADD       BLPPKF        PF3($Z)
082000950117     C   34              ADD       BLPPKF        PF3($Y)
082100000000     C                   ELSE
082200950117     C                   ADD       BLPPKF        PO3($Z)
082300950117     C   34              ADD       BLPPKF        PO3($Y)
082400000000     C                   ENDIF
082500000000      *
082600000000      * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
082700000000      * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
082800950117      * BLPVLC=Volume rilevato)
082900050112     C     BLPNCR        ifeq      BLPNCL
083000950117     C                   Z-ADD     BLPVLC        BLPVLF
083100000000     C                   ELSE
083200950117     C     BLPVLC        IFGE      BLPVLF
083300950117     C                   Z-ADD     BLPVLC        BLPVLF
083400000000     C                   ENDIF
083500000000     C                   ENDIF
083600000000      *
083700000000      * Calcolo il peso tassabile della spedizione, no per i c/servizio ($Z=2)
083800000000      * e solo per bolle con peso<=3500 Kg.
083900000000     C     $Z            IFNE      2
084000950117     C     BLPPKF        ANDLE     3500
084100950117     C                   Z-ADD     BLPPKF        $PESTA            9 1
084200950117     C     BLPVLF        IFNE      0
084300950117     C     BLPVLF        MULT(H)   §QTKPM        $COMPK            9 0
084400000000      * Per il peso tassabile, devo usare i maggior peso tra il reale e quello
084500000000      * calcolato con il rapporto fisso aziendale
084600000000     C     $COMPK        IFGT      $PESTA
084700000000     C                   Z-ADD     $COMPK        $PESTA
084800000000     C                   ENDIF
084900000000     C                   ENDIF
085000000000      * Calcolo l'arrotondamento in base alla tabella 3I (campo §3IARR)
085100000000     C     $PESTA        DIV       §3IARR        $COMPK
085200000000     C                   MVR                     $RESTO            5 0
085300000000     C     $RESTO        IFGT      0
085400000000     C     $COMPK        MULT      §3IARR        $PESTA
085500000000     C                   ADD       §3IARR        $PESTA
085600000000     C                   ENDIF
085700000000     C                   ADD       $PESTA        PTA($Z)
085800000000     C   34              ADD       $PESTA        PTA($Y)
085900000000     C                   ENDIF
086000000000      *
086100950117     C                   ADD       BLPVLF        VOL($Z)
086200950117     C   34              ADD       BLPVLF        VOL($Y)
086300000000      *
086400000000     C     BLPTSP        IFEQ      'E'
086500000000     C   35              ADD       1             SEF($Z)
086600000000     C  N35              ADD       1             SEA($Z)
086700000000     C   35
086800000000     CAN 34              ADD       1             SEF($Y)
086900000000     C  N35
087000000000     CAN 34              ADD       1             SEA($Y)
087100000000     C                   ELSE
087200000000      *
087300950117     C     BLPTSP        IFEQ      'D'
087400000619     C     BLPTSP        OREQ      'P'
087500000619     C   35              ADD       1             SSF($Z)
087600000619     C  N35              ADD       1             SSA($Z)
087700000619     C   35
087800000619     CAN 34              ADD       1             SSF($Y)
087900000619     C  N35
088000000619     CAN 34              ADD       1             SSA($Y)
088100000000     C                   ELSE
088200000000      *
088300950117     C   35              ADD       1             SNF($Z)
088400950117     C  N35              ADD       1             SNA($Z)
088500950117     C   35
088600950117     CAN 34              ADD       1             SNF($Y)
088700950117     C  N35
088800950117     CAN 34              ADD       1             SNA($Y)
088900000000     C                   ENDIF
089000000000     C                   ENDIF
089100000000      *
089200000000     C                   ENDIF
089300000000      *
089400000000     C     $RBL          IFEQ      'R'
089500000000     C   35              ADD       1             SRF($Z)
089600000000     C  N35              ADD       1             SRA($Z)
089700000000     C   35
089800000000     CAN 34              ADD       1             SRF($Y)
089900000000     C  N35
090000000000     CAN 34              ADD       1             SRA($Y)
090100000000     C                   ENDIF
090200000000      *
090300000000     C                   ENDSR
090400000000      *****************************************************************
090500000000      * Calcolo, stampa e azzeramento dei totali per:
090600000000      *   se $X=1    filiale
090700000000      *   se $X=2    conto servizi per filiale
090800000000      *   se $X=3    conto servizi per area
090900000000      *   se $X=4    area
091000000000      *   se $X=5    totale generale
091100000000      *   se $X=6    filiale             (sospensione)
091200000000      *   se $X=7    conto servizi fil.  (sospensione)
091300000000      *****************************************************************
091400000000     C     TOTALE        BEGSR
091500000000      *
091600000000     C                   MOVE      *BLANKS       ORGDEP
091700000000     C     $X            IFEQ      1
091800000000     C                   Z-ADD     $FILPR        FILIAP
091900000000     C                   SETON                                        40
092000000000     C     FILIAP        CHAIN     AZORG01L                           99
092100000000     C  N99              MOVEL     ORGDES        ORGDEP
092200000000     C                   ENDIF
092300000000      *
092400000000     C     $X            IFEQ      3
092500000000     C                   SETOFF                                       40
092600000000     C                   Z-ADD     *ZERO         FILIAP
092700000000     C     'C / SERV'    CAT       'IZIO':0      ORGDEP
092800000000     C                   ENDIF
092900000000      *
093000000000     C     $X            IFEQ      4
093100950117     C  N36              WRITE     LSC5T3
093200000000     C                   SETOFF                                       40
093300000000     C                   Z-ADD     *ZERO         FILIAP
093400000000     C                   MOVE      *BLANKS       ORGDEP
093500000000     C                   MOVEL     'TOTALE'      ORGDEP
093600000000     C                   ENDIF
093700000000      *
093800000000      * Il TOTALE GENERALE lo stampo solo se sono in sede
093900000000     C     $X            IFEQ      5
094000000000     C     SIMFEL        ANDEQ     *ZERO
094100950117     C  N36              WRITE     LSC5T3
094200000000     C                   SETOFF                                       40
094300000000     C                   Z-ADD     *ZERO         FILIAP
094400000000     C     'TOTALE G'    CAT       'ENERAL':0    ORGDEP
094500000000     C     ORGDEP        CAT       'E':0         ORGDEP
094600000000     C                   ENDIF
094700000000      *
094800000000     C                   Z-ADD     COL($X)       COLLIP
094900000000     C                   Z-ADD     PF3($X)       PEF35P
095000000000     C                   Z-ADD     PO3($X)       PEO35P
095100000000     C     PF3($X)       ADD       PO3($X)       PETOTP
095200000000     C                   Z-ADD     PTA($X)       PETASP
095300000000     C                   Z-ADD     VOL($X)       VOLUMP
095400000000     C     SEF($X)       ADD       SEA($X)       SPESTP
095500000000     C                   Z-ADD     SEF($X)       SPESFP
095600000000     C                   Z-ADD     SEA($X)       SPESAP
095700000000     C     SNF($X)       ADD       SNA($X)       SPNOTP
095800000000     C                   Z-ADD     SNF($X)       SPNOFP
095900000000     C                   Z-ADD     SNA($X)       SPNOAP
096000000000     C     SSF($X)       ADD       SSA($X)       SPSTTP
096100000000     C                   Z-ADD     SSF($X)       SPSTFP
096200000000     C                   Z-ADD     SSA($X)       SPSTAP
096300000000     C     SPESTP        ADD       SPNOTP        SPTOTP
096400000000     C                   ADD       SPSTTP        SPTOTP
096500000000     C     SPESFP        ADD       SPNOFP        SPTOFP
096600000000     C                   ADD       SPSTFP        SPTOFP
096700000000     C     SPESAP        ADD       SPNOAP        SPTOAP
096800000000     C                   ADD       SPSTAP        SPTOAP
096900970312     C     COLLIP        IFGT      *ZEROS
097000000000     C     SRF($X)       ADD       SRA($X)       SPRETP
097100000000     C                   Z-ADD     SRF($X)       SPREFP
097200000000     C                   Z-ADD     SRA($X)       SPREAP
097300970312     C                   ELSE
097400970312     C                   MOVE      *ZEROS        SPRETP
097500970312     C                   MOVE      *ZEROS        SPREFP
097600970312     C                   MOVE      *ZEROS        SPREAP
097700970312     C                   END
097800000000      *
097900000000      * Non devo stampare il totale generale se non sono in sede
098000000000     C     $X            IFLT      5
098100000000     C     $X            OREQ      5
098200000000     C     SIMFEL        ANDEQ     *ZERO
098300000000     C                   EXSR      STAMPA
098400000000     C                   ENDIF
098500000000      *
098600000000      * In questa fase aggiorno le schiere per i totali, con il segg. criterio:
098700000000      *    se ho stampato la filiale ($X=1) aggiorno l'area
098800000000      *    se ho stampato i c/servizi ($X=3) aggiorno l'area
098900000000      *    se ho stampato i totali area ($X=4) aggiorno il totale genrale
099000000000     C     $X            IFLT      5
099100000000     C                   Z-ADD     4             $W                1 0
099200000000     C     $X            IFEQ      4
099300000000     C                   Z-ADD     5             $W
099400000000     C                   ENDIF
099500000000      *
099600000000     C                   ADD       COL($X)       COL($W)
099700000000     C                   ADD       PF3($X)       PF3($W)
099800000000     C                   ADD       PO3($X)       PO3($W)
099900000000     C                   ADD       PTA($X)       PTA($W)
100000000000     C                   ADD       VOL($X)       VOL($W)
100100000000     C                   ADD       SEF($X)       SEF($W)
100200000000     C                   ADD       SEA($X)       SEA($W)
100300000000     C                   ADD       SNF($X)       SNF($W)
100400000000     C                   ADD       SNA($X)       SNA($W)
100500000000     C                   ADD       SSF($X)       SSF($W)
100600000000     C                   ADD       SSA($X)       SSA($W)
100700970312     C     COL($W)       IFGT      *ZEROS
100800000000     C                   ADD       SRF($X)       SRF($W)
100900000000     C                   ADD       SRA($X)       SRA($W)
101000970312     C                   ELSE
101100970312     C                   MOVE      *ZEROS        SRF($W)
101200970312     C                   MOVE      *ZEROS        SRA($W)
101300970312     C                   END
101400000000     C                   ENDIF
101500000000      *
101600000000      * Se in stampa (elaborazione), scrivo anche nell'archivio statistiche
101700950213     C     DS1RIS        IFNE      'R'
101800000000     C     $X            ANDEQ     1
101900000000     C     *IN43         ANDEQ     *ON
102000000000     C                   EXSR      AGGIOR
102100000000     C                   ENDIF
102200000000      *
102300000000      * Azzeramento dei campi gia' stampati
102400000000     C                   Z-ADD     *ZERO         COL($X)
102500000000     C                   Z-ADD     *ZERO         PF3($X)
102600000000     C                   Z-ADD     *ZERO         PO3($X)
102700000000     C                   Z-ADD     *ZERO         PTA($X)
102800000000     C                   Z-ADD     *ZERO         VOL($X)
102900000000     C                   Z-ADD     *ZERO         SEF($X)
103000000000     C                   Z-ADD     *ZERO         SEA($X)
103100000000     C                   Z-ADD     *ZERO         SNF($X)
103200000000     C                   Z-ADD     *ZERO         SNA($X)
103300000000     C                   Z-ADD     *ZERO         SSF($X)
103400000000     C                   Z-ADD     *ZERO         SSA($X)
103500000000     C                   Z-ADD     *ZERO         SRF($X)
103600000000     C                   Z-ADD     *ZERO         SRA($X)
103700000000      *
103800000000      * Se azzero la filiale (nr.1), devo azzerare anche i suoi c/servizi (nr.2)
103900000000      * Ma prima li sommo dei totali c/servizi dell'area (nr.3)
104000000000     C     $X            IFEQ      1
104100000000     C                   ADD       COL(2)        COL(3)
104200000000     C                   ADD       PF3(2)        PF3(3)
104300000000     C                   ADD       PO3(2)        PO3(3)
104400000000     C                   ADD       PTA(2)        PTA(3)
104500000000     C                   ADD       VOL(2)        VOL(3)
104600000000     C                   ADD       SEF(2)        SEF(3)
104700000000     C                   ADD       SEA(2)        SEA(3)
104800000000     C                   ADD       SNF(2)        SNF(3)
104900000000     C                   ADD       SNA(2)        SNA(3)
105000000000     C                   ADD       SSF(2)        SSF(3)
105100000000     C                   ADD       SSA(2)        SSA(3)
105200970312     C     COL(3)        IFGT      *ZEROS
105300000000     C                   ADD       SRF(2)        SRF(3)
105400000000     C                   ADD       SRA(2)        SRA(3)
105500970312     C                   ELSE
105600970312     C                   MOVE      *ZEROS        SRF(3)
105700970312     C                   MOVE      *ZEROS        SRA(3)
105800970312     C                   END
105900000000      *
106000000000     C                   Z-ADD     *ZERO         COL(2)
106100000000     C                   Z-ADD     *ZERO         PF3(2)
106200000000     C                   Z-ADD     *ZERO         PO3(2)
106300000000     C                   Z-ADD     *ZERO         PTA(2)
106400000000     C                   Z-ADD     *ZERO         VOL(2)
106500000000     C                   Z-ADD     *ZERO         SEF(2)
106600000000     C                   Z-ADD     *ZERO         SEA(2)
106700000000     C                   Z-ADD     *ZERO         SNF(2)
106800000000     C                   Z-ADD     *ZERO         SNA(2)
106900000000     C                   Z-ADD     *ZERO         SSF(2)
107000000000     C                   Z-ADD     *ZERO         SSA(2)
107100000000     C                   Z-ADD     *ZERO         SRF(2)
107200000000     C                   Z-ADD     *ZERO         SRA(2)
107300000000      *
107400000000     C                   Z-ADD     *ZERO         COL(6)
107500000000     C                   Z-ADD     *ZERO         PF3(6)
107600000000     C                   Z-ADD     *ZERO         PO3(6)
107700000000     C                   Z-ADD     *ZERO         PTA(6)
107800000000     C                   Z-ADD     *ZERO         VOL(6)
107900000000     C                   Z-ADD     *ZERO         SEF(6)
108000000000     C                   Z-ADD     *ZERO         SEA(6)
108100000000     C                   Z-ADD     *ZERO         SNF(6)
108200000000     C                   Z-ADD     *ZERO         SNA(6)
108300000000     C                   Z-ADD     *ZERO         SSF(6)
108400000000     C                   Z-ADD     *ZERO         SSA(6)
108500000000     C                   Z-ADD     *ZERO         SRF(6)
108600000000     C                   Z-ADD     *ZERO         SRA(6)
108700000000      *
108800000000     C                   Z-ADD     *ZERO         COL(7)
108900000000     C                   Z-ADD     *ZERO         PF3(7)
109000000000     C                   Z-ADD     *ZERO         PO3(7)
109100000000     C                   Z-ADD     *ZERO         PTA(7)
109200000000     C                   Z-ADD     *ZERO         VOL(7)
109300000000     C                   Z-ADD     *ZERO         SEF(7)
109400000000     C                   Z-ADD     *ZERO         SEA(7)
109500000000     C                   Z-ADD     *ZERO         SNF(7)
109600000000     C                   Z-ADD     *ZERO         SNA(7)
109700000000     C                   Z-ADD     *ZERO         SSF(7)
109800000000     C                   Z-ADD     *ZERO         SSA(7)
109900000000     C                   Z-ADD     *ZERO         SRF(7)
110000000000     C                   Z-ADD     *ZERO         SRA(7)
110100000000     C                   ENDIF
110200000000      *
110300000000     C                   ENDSR
110400000000      *****************************************************************
110500000000      * A fine stampa, emetto gli ultimissimi totali e quello generale
110600000000      *****************************************************************
110700000000     C     STAINI        BEGSR
110800000000      *
110900950213     C     DS1RIS        IFNE      *BLANKS
111000950117     C                   OPEN      FNLSC5P
111100000000     C                   SETOFF                                       31
111200000000      * Stampo l'intestazione
111300950117     C                   WRITE     LSC5T1
111400950117     C                   WRITE     LSC5T3
111500000000     C                   SETOFF                                       30
111600000000      *
111700000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
111800000000     C                   ENDIF
111900000000      *
112000000000     C                   ENDSR
112100000000      *****************************************************************
112200000000      * A fine stampa, emetto gli ultimissimi totali e quello generale
112300000000      *****************************************************************
112400000000     C     STAFIN        BEGSR
112500000000      *
112600950213     C     DS1RIS        IFNE      *BLANKS
112700000000      *
112800000000      * Stampa terminata
112900000000     C     $ALMEN        IFEQ      *ZERO
113000950117     C                   WRITE     LSC5E1
113100000000     C                   ELSE
113200000000     C     3             DO        5             $X
113300000000     C                   EXSR      TOTALE
113400000000     C                   ENDDO
113500000000     C                   ENDIF
113600950117     C                   WRITE     LSC5E2
113700000000      *
113800950117     C                   CLOSE     FNLSC5P
113900000000      *
114000000000     C                   ENDIF
114100000000      *
114200000000      * Aggiorno la data ultima elaborazione
114300950213     C     DS1RIS        IFNE      'R'
114400021016
114500021016     c                   Eval      kTbeCod = 'SDF'
114600021016     c                   Movel(p)  Simfel        kTbeKe1
114700021016     c     Ktbe          Chain     Tntbe01l
114800021016     c                   If        %Found(Tntbe01l)
114900021016     c                   Movel     TbeUni        dSdf
115000021016     c                   If        §SdfBolP = *Zeros or §SdfBolP > Ds1Ada
115100021016     c                   Z-Add     Ds1Ada        §SdfBolP
115200021016     c                   EndIf
115300021016     c                   If        §SdfBolU < Ds1Aal
115400021016     c                   Z-Add     Ds1Aal        §SdfBolU
115500021016     c                   EndIf
115600021016     c                   Movel     dSdf          TbeUni
115700021016     c                   Update    Tntbe000
115800021016     c                   EndIf
115900000000     C                   ENDIF
116000000000      *
116100000000     C                   ENDSR
116200000000      *****************************************************************
116300000000      * Stampa del dettaglio con test per l'overflow
116400000000      *****************************************************************
116500000000     C     STAMPA        BEGSR
116600000000      *
116700970312    1C     DS1RIS        IFNE      *BLANKS
116800000000      *
116900000000      * Stampo solo se la riga ha dei valori (testo il nr.colli)
117000950929      *  o i colli C/S > 0
117100970312    2C     COLLIP        IFGT      *ZERO
117200970312     C     $X            OREQ      3
117300970312     C     COL(2)        ANDGT     0
117400000000      *
117500950117     C   30              WRITE     LSC5T1
117600950117     C   30              WRITE     LSC5T3
117700000000     C   30              SETOFF                                       30
117800950117     C                   WRITE     LSC5D1
117900970312    2C                   END
118000000000      *
118100970312    2C     COLLIP        IFGT      *ZEROS
118200970312     C     $ALMEN        ANDEQ     *ZERO
118300970312     C     COL(2)        ORGT      *ZEROS
118400970312     C     $ALMEN        ANDEQ     *ZERO
118500000000     C                   Z-ADD     1             $ALMEN
118600970312    2C                   ENDIF
118700000000      *
118800970312    1C                   ENDIF
118900000000      *
119000000000     C                   ENDSR
119100000000      *****************************************************************
119200000000      * Scrittura su archivio statistiche
119300000000      *****************************************************************
119400000000     C     AGGIOR        BEGSR
119500000000      *
119600000000     C                   Z-ADD     1             $W
119700000000     C     $W            DOUGT     7
119800000000      *
119900000000     C                   Z-ADD     SIMFEL        SBOFLE
120000000000     C                   Z-ADD     $FILPR        SBOLNP
120100950213     C                   Z-ADD     DS1ADA        SBODRE
120200000000     C                   MOVE      *BLANK        SBOSOS
120300000000     C                   MOVE      *BLANK        SBOTRE
120400000000     C     $W            IFEQ      2
120500000000     C     $W            OREQ      7
120600000000     C                   MOVE      'C'           SBOTRE
120700000000     C                   ENDIF
120800000000     C     $W            IFGT      5
120900000000     C                   MOVE      'S'           SBOSOS
121000000000     C                   ENDIF
121100000000      *
121200000000     C                   Z-ADD     COL($W)       SBOCOL
121300000000     C                   Z-ADD     PF3($W)       SBOPF3
121400000000     C                   Z-ADD     PO3($W)       SBOPO3
121500000000     C                   Z-ADD     PTA($W)       SBOPTA
121600000000     C                   Z-ADD     VOL($W)       SBOVOL
121700000000     C                   Z-ADD     SEF($W)       SBOSEF
121800000000     C                   Z-ADD     SEA($W)       SBOSEA
121900000000     C                   Z-ADD     SNF($W)       SBOSNF
122000000000     C                   Z-ADD     SNA($W)       SBOSNA
122100000000     C                   Z-ADD     SSF($W)       SBOSSF
122200000000     C                   Z-ADD     SSA($W)       SBOSSA
122300970312     C     COL($W)       IFGT      *ZEROS
122400000000     C                   Z-ADD     SRF($W)       SBOSRF
122500000000     C                   Z-ADD     SRA($W)       SBOSRA
122600970312     C                   ELSE
122700970312     C                   MOVE      *ZEROS        SBOSRF
122800970312     C                   MOVE      *ZEROS        SBOSRA
122900970312     C                   END
123000000000      *
123100000000      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
123200040922      * riempo il "flag di trasmissione" (SBOFTR)
123300040922     c                   z-add     dateu         sbodtr
123400040922     C***                Z-ADD     *ZERO         SBODTR
123500040922     C***  §5CSBO        IFEQ      'S'
123600000000     C                   MOVE      *BLANK        SBOFTR
123700040922     C***                ELSE
123800040922     C***                MOVE      'T'           SBOFTR
123900040922     C***                ENDIF
124000000000      *
124100970312     C* memorizzo record con sbotre='F' (festivo) se numero colli = 0
124200970312     C* e giorno elaborato festivo
124300970312     C     WFES          IFEQ      'F'
124400970312     C     COL($W)       ANDEQ     0
124500970312     C                   MOVE      'F'           SBOTRE
124600970312     C                   END
124700970312     C*
124800970312     C     $W            IFEQ      1
124900970312     C     COL($W)       ORGT      *ZERO
125000990507     C                   WRITE     FNSBO000
125100990519     C                   MOVEL     'S'           WTRASM            1
125200000000     C                   ENDIF
125300000000      *
125400000000      * Solo se ho registrato con $X=1 (coie' rottura per filiale), registro
125500000000      * anche per c/servizio ($X=2), per filiale con spspensione ($X=5) e
125600000000      * per c/servizio con sospensione ($X=6)
125700000000     C     $W            IFEQ      2
125800000000     C                   Z-ADD     6             $W
125900000000     C                   ELSE
126000000000     C                   ADD       1             $W
126100000000     C                   ENDIF
126200000000      *
126300000000     C                   ENDDO
126400000000      *
126500000000     C                   ENDSR
126600000000      *****************************************************************
126700990507      * RISTAMPA: senza elaborazione, solo lettura da file statistiche (FNSBO)
126800000000      * Tutto il ciclo di ristampa viene eseguito 2 volte, prima per le bolle
126900000000      * normali quindi per quelle in sospensione
127000000000      *****************************************************************
127100000000     C     RISTAM        BEGSR
127200000000      *
127300050829     c                   setoff                                       3237
127400000000      * Verifico se richiesta una specifica area
127500950213     C     DS1FEL        COMP      *ZERO                                  32
127600050829     c
127700050829     c                   if        ds1fel=0 and simfel>0
127800050829     c                   eval      ds1fel=simfel
127900050829     c                   setoff                                       32
128000050829     c                   endif
128100050829     c
128200050829     c                   if        lin(1)>0 and lin(2)=0
128300050829     c                   eval      klin=lin(1)
128400050829     c                   seton                                        37
128500050829     c                   endif
128600000000      *
128700000000     C                   Z-ADD     *ZERO         $PRIM1            1 0
128800000000     C                   Z-ADD     *ZERO         $FILPR
128900000000     C                   Z-ADD     *ZERO         $FI2PR
129000000000      *
129100000000     C   43              MOVE      *BLANK        SBOSOS
129200000000     C  N43              MOVE      'S'           SBOSOS
129300950213     C                   Z-ADD     DS1FEL        SBOFLE
129400950213     C                   Z-ADD     DS1ADA        SBODRE
129500000000     C                   Z-ADD     *ZERO         SBOLNP
129600000000      *
129700050829     C  n37FNSBOK        SETLL     FNSBO01L
129800050829     C   37ksbo          SETLL     FNSBO01L
129900950929    1C                   DO        *HIVAL
130000050829     c                   if        *in37
130100050830     C     ksbo2         reade(N)  FNSBO01L                               33
130200050829     c                   else
130300990507     C  N32FLSB2K        READE(N)  FNSBO01L                               33
130400990507     C   32SBOSOS        READE(N)  FNSBO01L                               33
130500050829     c                   endif
130600000000      *
130700990507    2C  N33SBODRE        IFGE      DS1ADA
130800990507     C     SBODRE        ANDLE     DS1AAL
130900000000      *
131000000000      * Solo le filiali presenti nella schiera LIN
131100050829     c                   if        not *in37 and *in32
131200000000     C     SBOLNP        LOOKUP    LIN                                    99
131300050829     c                   else
131400050829     c                   seton                                        99
131500050829     c                   endif
131600050829     c
131700950929    3C     *IN99         IFEQ      *ON
131800000000      *
131900000000      * Al primo giro memorizzo il "precedente"
132000950929    4C     $PRIM1        IFEQ      *ZERO
132100000000     C                   Z-ADD     SBOFLE        $FI2PR
132200000000     C                   Z-ADD     SBOLNP        $FILPR
132300000000     C                   Z-ADD     1             $PRIM1
132400950929    4C                   ENDIF
132500000000      *
132600000000      * Se cambia filiale stampo i totali per filiale
132700950929    4C     SBOTRE        IFEQ      *BLANK
132800950929     C     SBOLNP        ANDNE     $FILPR
132900000000     C                   Z-ADD     1             $X
133000000000     C                   EXSR      TOTALE
133100000000     C                   Z-ADD     SBOLNP        $FILPR
133200950929    4C                   ENDIF
133300000000      *
133400000000      * Se cambia area stampo i totali per c/servizio
133500950929    4C     SBOFLE        IFNE      $FI2PR
133600000000     C                   Z-ADD     3             $X
133700000000     C                   EXSR      TOTALE
133800000000     C                   Z-ADD     4             $X
133900000000     C                   EXSR      TOTALE
134000000000     C                   Z-ADD     SBOFLE        $FI2PR
134100950929    4C                   ENDIF
134200000000      *
134300000000     C                   EXSR      ELABO2
134400000000      *
134500950929    3C                   ENDIF
134600000000      *
134700950929    2C                   ENDIF
134800950929    1C  N33              ENDDO
134900000000     C                   Z-ADD     1             $X
135000000000     C                   EXSR      TOTALE
135100000000      *
135200000000     C     ENDRIS        ENDSR
135300000000      *****************************************************************
135400990507      * Avendo letto il file FNSBO00F memorizzo i campi del record nelle schiere
135500000000      *****************************************************************
135600000000     C     ELABO2        BEGSR
135700000000      *
135800000000      * Definisco il valore gli indici di schiera
135900000000     C                   Z-ADD     1             $Z
136000000000     C     SBOTRE        IFEQ      'C'
136100000000     C                   Z-ADD     2             $Z
136200000000     C                   ENDIF
136300000000      *
136400000000     C                   ADD       SBOCOL        COL($Z)
136500000000     C                   ADD       SBOPF3        PF3($Z)
136600000000     C                   ADD       SBOPO3        PO3($Z)
136700000000     C                   ADD       SBOPTA        PTA($Z)
136800000000     C                   ADD       SBOVOL        VOL($Z)
136900000000     C                   ADD       SBOSEF        SEF($Z)
137000000000     C                   ADD       SBOSEA        SEA($Z)
137100000000     C                   ADD       SBOSNF        SNF($Z)
137200000000     C                   ADD       SBOSNA        SNA($Z)
137300000000     C                   ADD       SBOSSF        SSF($Z)
137400000000     C                   ADD       SBOSSA        SSA($Z)
137500000000     C                   ADD       SBOSRF        SRF($Z)
137600000000     C                   ADD       SBOSRA        SRA($Z)
137700000000      *
137800000000     C                   ENDSR
137900000000      *****************************************************************
138000000000      *                                4         5         6         7         8
138100950308** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
138200950308OVRPRTF FILE(FNLSC5P) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.boll.
138300950308')
