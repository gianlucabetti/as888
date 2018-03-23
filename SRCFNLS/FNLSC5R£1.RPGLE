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
008100000000     D KPJBA         E DS
008200000000     D DS3A          E DS
008300000000     D DS3I          E DS
008400000000     D DS3L          E DS
008500040922     D*DS5C          E DS
008600970707     D DS5P          E DS
008700950117     D DSQT1         E DS
008800000000     D CNCR80        E DS
008900000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009000950213     D* DS PER TRUL06R - CARICAMENTO £X
009100950213     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009200950314     D  T06                    1     90  0
009300950314     D                                     DIM(30)
009400000000      * DS per il passaggio parametri
009500950213     D DSLS01        E DS
009600970312     D  ANNODA                41     44  0
009700970312     D  MESEDA                45     46  0
009800970312     D  $GI                   47     48  0
009900950213     D                 DS
010000950117      * DATA INIZIALE AAAA/MM/GG (8 CARATTERI)
010100950213     D  DATADA                 1      8  0
010200950213     D  $ANNDA                 1      4  0
010300950213     D  $MGSDA                 5      8  0
010400000000      * DS per data di spedizione della bolla
010500000000     D                 DS
010600950117     D  $DATSP                 1      8  0
010700950117     D  BLPAAS                 1      4  0
010800950117     D  BLPMGS                 5      8  0
010900040922
011000040922     D WLBDAT          DS                  INZ
011100040922     D  G02DAT                 1      8  0
011200040922     D  G02INV                 9     16  0
011300040922     D  G02ERR                17     17
011400040922     D  G02TGI                18     22  0
011500000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
011600000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
011700021016      * Ds date statistiche
011800021016     d dSdf          e ds
011900000000      *
012000000000     C     *ENTRY        PLIST
012100000000     C                   PARM                    KPJBA
012200950213     C                   MOVEL     KPJBU         DSLS01
012300950213     C                   Z-ADD     DS1ADA        DATADA
012400000000      *
012500010717     C     FNBLPK        KLIST
012600000000     C                   KFLD                    BLPLNP
012700000000     C                   KFLD                    $ANNDA
012800000000     C                   KFLD                    $MGSDA
012900990507     C     FNSBOK        KLIST
013000000000     C                   KFLD                    SBOSOS
013100000000     C                   KFLD                    SBOFLE
013200000000     C                   KFLD                    SBOLNP
013300000000     C                   KFLD                    SBODRE
013400990507     C     FLSB1K        KLIST
013500000000     C                   KFLD                    SBOSOS
013600000000     C                   KFLD                    SBOFLE
013700000000     C                   KFLD                    SBOLNP
013800990507     C     FLSB2K        KLIST
013900000000     C                   KFLD                    SBOSOS
014000000000     C                   KFLD                    SBOFLE
014100050829     C     ksbo          KLIST
014200050829     C                   KFLD                    SBOSOS
014300050829     C                   KFLD                    SBOFLE
014400050829     C                   KFLD                    klin
014500050829     C                   KFLD                    sbodre
014600050829     C     ksbo2         KLIST
014700050829     C                   KFLD                    SBOSOS
014800050829     C                   KFLD                    SBOFLE
014900050829     C                   KFLD                    klin
015000970312     C*
015100970312     C     KCLN          KLIST
015200970312     C                   KFLD                    SIMFEL
015300970312     C                   KFLD                    $TFA
015400970312     C                   KFLD                    ANNODA
015500970312     C                   KFLD                    MESEDA
015600000000      *
015700000615     C     *LIKE         DEFINE    DS1RIS        SAVRIS
015800000615     C     *LIKE         DEFINE    BLPLNP        $FILPR
015900000000     C     *LIKE         DEFINE    BLPLNP        $FI2PR
016000950117     C     *LIKE         DEFINE    BLPCBO        $CBOPR
016100000000     C     *LIKE         DEFINE    §3ARBL        $RBL
016200970312     C     *LIKE         DEFINE    SIMFEL        $TFA
016300000000      *****************************************************************
016400000000      *              M A I N      L I N E
016500000000      *****************************************************************
016600000000     C                   Z-ADD     1             CODUT
016700000000      *
016800950117      * LEGGO LA DSQT1 PER IL RAPPORTO PESO/VOLUME
016900950213     C                   MOVEL     'QT'          KCOD
017000950213     C                   MOVEL     '1       '    KKEY
017100950213     C     KTAB1         CHAIN     TABEL00F                           99
017200950117     C  N99              MOVEL     TBLUNI        DSQT1
017300950117     C   99              CLEAR                   DSQT1
017400000000      *
017500000000      * Reperisco la tabella 3I per calcoli che serviranno poi
017600950213     C                   MOVEL     '3I'          KCOD
017700950213     C                   MOVEL     '1       '    KKEY
017800950213     C     KTAB1         CHAIN     TABEL00F                           99
017900000000     C  N99              MOVEL     TBLUNI        DS3I
018000000000     C   99              CLEAR                   DS3I
018100000000     C   99              Z-ADD     20            §3IARR
018200000000      *
018300000000      * Reperimento tabella per trasmissione archivio statistiche
018400040922     C***                MOVE      '5C'          KCOD
018500040922     C***                MOVE      '1       '    KKEY
018600040922     C***  KTAB1         CHAIN     TABEL00F                           99
018700040922     C**N99              MOVEL     TBLUNI        DS5C
018800040922     C***99              CLEAR                   DS5C
018900000000      *
019000000000     C                   MOVE      'RISTAMPA'    STRIP
019100950213     C     DS1RIS        IFNE      'R'
019200000000     C                   MOVE      'STAMPA  '    STRIP
019300010717     C                   OPEN      FNBLP31L
019400000000     C                   ENDIF
019500950213     C     DS1RIS        COMP      *BLANKS                                36
019600000000      *
019700000000      * Scrivo, in stampa, se selezionata filiale o area
019800000000     C                   MOVE      *BLANKS       STRIP1
019900950213     C     DS1LNP        IFEQ      *ZERO
020000000000     C                   MOVEL     'AREA'        STRIP1
020100950213     C                   MOVE      DS1FEL        STRIP1
020200000000     C                   ELSE
020300960926     C                   MOVEL     'P.O.'        STRIP1
020400950213     C                   MOVE      DS1LNP        STRIP1
020500000000     C                   ENDIF
020600000000      *
020700000000      * Se la data AL e' vuota la imposto come la data DAL
020800950213     C     DS1AAL        IFEQ      *ZERO
020900950213     C                   Z-ADD     DS1ADA        DS1AAL
021000950213     C                   Z-ADD     DS1GDA        DS1GAL
021100000000     C                   ENDIF
021200000000      *
021300000000      * Verifico se richiesto un solo giorno o un periodo dal-al
021400950213     C     DS1ADA        COMP      DS1AAL                                 44
021500000000      *
021600950213      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
021700950213     C                   EXSR      COMUNE
021800000000      *
021900000000     C                   SETON                                        LR
022000020212     C*****************************************************************
022100020212     C*  PARTE COMUNE STAMPE GIORNALIERE
022200020212     C*
022300020212     C*  . REPERIMENTO 'SIMFEL'
022400020212     C*  . CARICAMENTO '£1'
022500020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
022600020212     C*  . CODA DI STAMPA ('£OUTQ')
022700020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
022800020212     C*  . MODULO PER LA STAMPA ('£MOD')
022900020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
023000020212     C*****************************************************************
023100020212     C     COMUNE        BEGSR
023200020212     C*
023300020212     C*****************************************************************
023400020212     C*       RIEPILOGO INDICATORI:
023500020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
023600020212     C* 11    -           STAMPA PER 1L
023700020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
023800050829     C* 32 e 37 PER ristampa
023900020212     C*****************************************************************
024000020212     C                   Z-ADD     1             CODUT
024100020212     C*
024200020212     C* Reperimento parametri aziendali
024300020212     C                   CALL      'X§PARUT'
024400020212     C                   PARM                    UT§DSE
024500020212     C                   MOVEL     RAGUT         RSUT             20
024600020212     C                   MOVEL     REC80         CNCR80
024700020212     C*
024800020212     C* Chiave di accesso per il file tabelle
024900020212     C     KTAB1         KLIST
025000020212     C                   KFLD                    CODUT
025100020212     C                   KFLD                    KCOD
025200020212     C                   KFLD                    KKEY
025300020212     C     KTAB2         KLIST
025400020212     C                   KFLD                    CODUT
025500020212     C                   KFLD                    KCOD
025600021016
025700021016     c     Ktbe          Klist
025800021016     c                   Kfld                    kTbeCod
025900021016     c                   Kfld                    kTbeKe1
026000020212     C*
026100020212     C     *LIKE         DEFINE    TBLCOD        KCOD
026200020212     C     *LIKE         DEFINE    TBLKEY        KKEY
026300020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
026400020212     C*
026500020212     C* DETERMINO DATA E ORA DI ESECUZIONE
026600020212     C                   TIME                    WTIME            14 0
026700020212     C                   MOVE      WTIME         WDATE             8 0
026800020212     C                   MOVEL     WTIME         UTIME             6 0
026900040922     c*
027000040922     C                   CLEAR                   WLBDAT
027100040922     C                   MOVE      Wtime         G02DAT
027200040922     C                   CALL      'XSRDA8'
027300040922     C                   PARM                    WLBDAT
027400040922     C                   Z-ADD     G02INV        dateu             8 0
027500020212     C*
027600020418     C* CARICO '£1'
027700020212     C                   CLEAR                   KPJBU
027800020212     C                   CLEAR                   DSUL06
027900020212     C                   MOVE      '£1'          D06COD
028000020212     C                   MOVEL     SIMFEL        D06KEY
028100050802     C                   MOVE      'S'           D06KEY
028200050802     C                   MOVEL     ds1ada        D06drf
028300020212     C                   MOVEL     DSUL06        KPJBU
028400020212     C                   CALL      'TRUL06R'
028500020212     C                   PARM                    KPJBA
028600020212     C                   MOVEL     KPJBU         DSUL06
028700020418     c                   movea     t06           sav_t06
028800020418
028900020418      * se in arrivo carico la £6
029000020418 b1  c                   if        ds1tbo = 'A'
029100020418     c                   clear                   wc
029200020418 b2  c                   do        30            wa
029300020422     c                   if        sav_t06(wa) = *zeros
029400020422     c                   leave
029500020422     c                   endif
029600020418     c                   clear                   kpjbu
029700020418     c                   clear                   dsul06
029800020418     c                   eval      d06cod = '£6'
029900020418     c                   movel     sav_t06(wa)   d06key
030000050802     C                   MOVE      'S'           D06KEY
030100020418     c                   eval      d06esc = 'S'
030200050802     C                   MOVEL     ds1ada        D06drf
030300020419     c                   movel     dsul06        kpjbu
030400020418     c                   call      'TRUL06R'
030500020418     c                   parm                    kpjba
030600020418     c                   movel     kpjbu         dsul06
030700020418 b3  c                   do        30            wb
030800020418     c                   if        t06(wb) = *zeros
030900020418     c                   leave
031000020418     c                   endif
031100020418     c                   if        t06(wb) <> sav_t06(wa)
031200020418     c                   add       1             wc
031300020418     c                   z-add     t06(wb)       l6(wc)
031400020418     c                   endif
031500020418 e3  c                   enddo
031600020418 e2  c                   enddo
031700020418
031800020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
031900020212     C                   Z-ADD     *ZERO         WB
032000020418 b2  C                   DO        29            WA
032100020418     C     sav_T06(WA)   LOOKUP    L6                                     30
032200020212     C  N30              ADD       1             WB
032300020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
032400020418 e2  C                   ENDDO
032500020418 x1  C                   ELSE
032600020418     C                   MOVEA     sav_T06       L1
032700020418 e1  C                   ENDIF
032800020212     C*
032900020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
033000020212     C                   MOVEA     L1            LIN
033100020212     C                   CLEAR                   L1
033200020212     C                   Z-ADD     *ZERO         WB                3 0
033300020212     C     1             DO        30            WPT               3 0
033400020212     C     LIN(WPT)      IFNE      0
033500020212     C                   CLEAR                   FNLSC4
033600020212     C                   Z-ADD     LIN(WPT)      IC4LIN
033700020212     C                   MOVE      'B'           IC4FLG
033800020212     C                   MOVEL     FNLSC4        KPJBU
033900020212     C                   CALL      'FNLSC4R1'
034000020212     C                   PARM                    KPJBA
034100050802     C                   PARM                    ds1ada
034200020212     C                   MOVEL     KPJBU         FNLSC4
034300020212     C     OC4ESE        IFEQ      'S'
034400020212     C                   ADD       1             WB
034500020212     C                   MOVE      LIN(WPT)      L1(WB)
034600020212     C                   ENDIF
034700020212     C                   ENDIF
034800020212     C                   ENDDO
034900070103     C*
035000070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
035100070103     C                   Z-ADD     *ZERO         WB                3 0
035200070103     C                   DO        29            WA                3 0
035300070103     C     L1(WA)        IFNE      *ZEROS
035400070103     C     L1(WA)        ANDNE     SIMFEL
035500070103     C                   ADD       1             WB
035600070103     C                   Z-ADD     L1(WA)        LV2(WB)
035700070103     C                   ENDIF
035800070103     C                   ENDDO
035900020212     C*
036000020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
036100020212     C*
036200020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
036300020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
036400020212     C* la stampa e' lanciata da menu
036500020212     C                   MOVEL     '3L'          KCOD
036600020212     C                   MOVEL     DS1PGM        KKEY
036700020212     C                   MOVE      DS1FPG        KKEY
036800020212     C     KTAB1         CHAIN     TABEL00F                           30
036900020212     C  N30              MOVEL     TBLUNI        DS3L
037000020212     C   30              MOVEL     'F'           §3LD1L
037100020212     C   30              MOVEL     ' '           §3LD2L
037200020212     C   30              MOVEL     '*STD    '    §3LDST
037300020212     C*
037400020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
037500020212     C                   MOVEL     §3LDST        £MOD             10
037600020212     C                   CLEAR                   LIN
037700020212     C*
037800020212     C* Verifico chi ha lanciato la stampa
037900020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
038000020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
038100020212     C* DS1FGS<>0  --->  si proviene da menu
038200020212     C     DS1FGS        IFEQ      0
038300020212     C*
038400020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
038500020212     C* lancio, voglio la stampa;  invece
038600020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
038700020212     C*
038800020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
038900020212     C* creare stampa per 1°lvl. (se stessa)
039000020212     C     DS1LNP        IFEQ      SIMFEL
039100020212     C*
039200020212     C* Il flag §3LD1L ha i seguenti significati:
039300020212     C*    ' ' = tutti i dati
039400020212     C*    '1' = solo filiale di 1' livello
039500020212     C     §3LD1L        IFEQ      '1'
039600020212     C                   SETON                                        11
039700020212     C                   END
039800020212     C*
039900020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
040000020212     C* nella stampa
040100020212     C*
040200020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
040300020212     C  N11              MOVEA     L1            LIN
040400020212     C   11              MOVEA     SIMFEL        LIN(1)
040500020212     C*
040600020212     C* IMPOSTO LA STAMPANTE:
040700020212     C* STAMPANTE TABULATI
040800020212     C                   MOVEL     '5P'          KCOD
040900020212     C                   MOVEL     'T'           W004A             4
041000020212     C                   MOVE      LIN(1)        W004A
041100020212     C                   MOVEL(P)  W004A         KKEY
041200020212     C     KTAB1         CHAIN     TABEL00F                           99
041300020212     C     *IN99         IFEQ      *OFF
041400020212     C                   MOVEL     TBLUNI        DS5P
041500020212     C                   MOVEL     §5POTQ        £OUTQ            10
041600020212     C                   END
041700020212     C*
041800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
041900020212     C                   EXSR      LEGGI
042000020212     C*
042100020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
042200020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
042300020212     C                   ELSE
042400020212     C*
042500020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
042600020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
042700020212     C*
042800020212     C* Il significato del fla §3LD2L e' il seguente:
042900020212     C*    ' ' = tutti i dati
043000020212     C*    'F' = per ogni 2' livello
043100020212     C     §3LD2L        IFEQ      'F'
043200020212     C                   SETON                                        14
043300020212     C                   ENDIF
043400020212     C*
043500020212     C                   Z-ADD     1             WB
043600020212     C                   MOVEA     *ZEROS        LIN
043700020212     C  N14              MOVEA     L1            LIN
043800020212     C*
043900020212     C* Lancio per tutti i 2° lvl
044000020212     C     DS1LNP        IFEQ      0
044100020212     C*
044200020212     C* LV2 contiene tutte le filiali di 2° lvl
044300020212     C     LV2(WB)       DOWNE     0
044400020212      *
044500020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
044600020212     C                   CLEAR                   FNLSC4
044700020212     C                   Z-ADD     LV2(WB)       IC4LIN
044800020212     C                   MOVEL     FNLSC4        KPJBU
044900020212     C                   CALL      'FNLSC4R1'
045000020212     C                   PARM                    KPJBA
045100050802     C                   PARM                    ds1ada
045200020212     C                   MOVEL     KPJBU         FNLSC4
045300020212      *
045400020212     C     OC4PT1        IFEQ      'S'
045500020212     C     §3LPT         ANDEQ     'N'
045600020212     C                   GOTO      NOLEGG
045700020212     C                   ENDIF
045800020212      *
045900020212     C     OC4EXP        IFEQ      'S'
046000020212     C     §3LEXP        ANDEQ     'N'
046100020212     C                   GOTO      NOLEGG
046200020212     C                   ENDIF
046300020212      *
046400020212     C     OC4DPD        IFEQ      'S'
046500020212     C     §3LDPD        ANDEQ     'N'
046600020212     C                   GOTO      NOLEGG
046700020212     C                   ENDIF
046800020212      *
046900020212     C     OC4BAR        IFEQ      'S'
047000020212     C     §3LBAR        ANDEQ     'N'
047100020212     C                   GOTO      NOLEGG
047200020212     C                   ENDIF
047300020212      *
047400020212     C     OC4FED        IFEQ      'S'
047500020212     C     §3LFED        ANDEQ     'N'
047600020212     C                   GOTO      NOLEGG
047700020212     C                   ENDIF
047800020212      *
047900020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
048000020212     C*  dati degli altri 2° lvl
048100020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
048200020212     C   14              MOVEL     LV2(WB)       LIN(1)
048300020212     C*
048400020212     C* IMPOSTO LA STAMPANTE
048500020212     C* STAMPANTE TABULATI
048600020212     C                   MOVEL     '5P'          KCOD
048700020212     C                   MOVEL     'T'           W004A             4
048800020212     C                   MOVE      LV2(WB)       W004A
048900020212     C                   MOVEL(P)  W004A         KKEY
049000020212     C     KTAB1         CHAIN     TABEL00F                           99
049100020212     C     *IN99         IFEQ      *OFF
049200020212     C                   MOVEL     TBLUNI        DS5P
049300020212     C                   MOVEL     §5POTQ        £OUTQ            10
049400020212     C                   END
049500020212     C*
049600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
049700020212     C                   EXSR      LEGGI
049800020212     C*
049900020212     C     NOLEGG        TAG
050000020212     C*
050100020212     C                   ADD       1             WB
050200020212     C                   ENDDO
050300020212     C*
050400020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
050500020212     C                   ELSE
050600020212     C*
050700020212     C* Lancio solo per la 2° lvl richiesta
050800020212     C   14              MOVEL     DS1LNP        LIN(1)
050900020212     C*
051000020212     C* STAMPANTE
051100020212     C* STAMPANTE TABULATI
051200020212     C                   MOVEL     '5P'          KCOD
051300020212     C                   MOVEL     'T'           W004A             4
051400020212     C                   MOVE      DS1LNP        W004A
051500020212     C                   MOVEL(P)  W004A         KKEY
051600020212     C     KTAB1         CHAIN     TABEL00F                           99
051700020212     C     *IN99         IFEQ      *OFF
051800020212     C                   MOVEL     TBLUNI        DS5P
051900020212     C                   MOVEL     §5POTQ        £OUTQ            10
052000020212     C                   END
052100020212     C*
052200020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
052300020212     C                   EXSR      LEGGI
052400020212     C*
052500020212     C                   ENDIF
052600020212     C                   ENDIF
052700020212     C*
052800020212     C                   ELSE
052900020212     C*
053000020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
053100020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
053200020212     C     DS1LNP        IFEQ      0
053300020212     C                   MOVEA     L1            LIN
053400020212     C* ...altrimenti elaboro solo se stessa
053500020212     C                   ELSE
053600020212     C                   MOVEL     DS1LNP        LIN(1)
053700020212     C                   ENDIF
053800020212     C*
053900020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
054000020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
054100020212     C* PRENDO QUELLA PRESENTE IN KPJBA
054200020212     C     DS1FGS        IFEQ      SIMFEL
054300020212     C                   MOVEL     KQOTD         £OUTQ
054400030627     C**!!!              MOVEL     KQOTD         £OUTQF
054500020212     C                   ELSE
054600020212     C* STAMPANTE TABULATI
054700020212     C                   MOVEL     '5P'          KCOD
054800020212     C                   MOVEL     'T'           W004A             4
054900020212     C                   MOVE      DS1FGS        W004A
055000020212     C                   MOVEL(P)  W004A         KKEY
055100020212     C     KTAB1         CHAIN     TABEL00F                           99
055200020212     C     *IN99         IFEQ      *OFF
055300020212     C                   MOVEL     TBLUNI        DS5P
055400020212     C                   MOVEL     §5POTQ        £OUTQ            10
055500020212     C                   END
055600020212     C                   ENDIF
055700020212     C*
055800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
055900020212     C                   EXSR      LEGGI
056000020212     C                   ENDIF
056100020212     C*
056200020212     C                   ENDSR
056300000000      *****************************************************************
056400000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
056500000000      *****************************************************************
056600000000     C     LEGGI         BEGSR
056700000615     C                   CLEAR                   SAVRIS
056800000000      *
056900000000      * Se sono nelle stampe giornaliere non voglio diciture
057000950213     C     DS1FGS        IFEQ      *ZERO
057100000000     C     SIMFEL        ANDNE     *ZERO
057200000000     C                   MOVE      *BLANKS       STRIP
057300000000     C                   MOVE      *BLANKS       STRIP1
057400000000     C                   ENDIF
057500000000      *
057600000000      * Cancello dal file statistiche i record della giornata che vado a elabor.
057700950213     C     DS1RIS        IFNE      'R'
057800000615     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
057900000615     C                   CLEAR                   KPJBU
058000000615     C                   CLEAR                   DSUL06
058100000615     C                   MOVE      '£1'          D06COD
058200000615     C                   MOVEL     SIMFEL        D06KEY
058300050802     C                   MOVE      'S'           D06KEY
058400050802     C                   MOVEL     ds1ada        D06drf
058500000615     C                   MOVEL     DSUL06        KPJBU
058600000615     C                   CALL      'TRUL06R'
058700000615     C                   PARM                    KPJBA
058800000615     C                   MOVEL     KPJBU         DSUL06
058900000615     C**
059000000615     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
059100000615     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
059200000615     C*  CHE USO ALLA FINE
059300000615     C     DS1RIS        IFNE      ' '
059400000615     C                   MOVEA     LIN           SVLIN
059500000615     C                   MOVEL     DS1RIS        SAVRIS
059600000615     C                   CLEAR                   DS1RIS
059700000615     C                   ENDIF
059800000615     C                   MOVEA     T06           LIN
059900000615     C**
060000000000     C                   MOVE      *BLANKS       SBOSOS
060100000000     C                   DO        2
060200000000     C                   Z-ADD     SIMFEL        SBOFLE
060300950213     C                   Z-ADD     DS1ADA        SBODRE
060400000000      *
060500000000      * Elimino solo le linee della LIN (che dovro' poi elaborare)
060600000000     C                   Z-ADD     1             XX
060700000000     C     LIN(XX)       DOWGT     *ZERO
060800000000      *
060900000000     C                   Z-ADD     LIN(XX)       SBOLNP
061000950213     C                   Z-ADD     DS1ADA        SBODRE
061100990507     C     FNSBOK        SETLL     FNSBO01L
061200000000     C                   DO        *HIVAL
061300990507     C     FLSB1K        READE     FNSBO01L                               99
061400990507     C  N99SBODRE        COMP      DS1AAL                             99
061500990507     C  N99              DELETE    FNSBO000
061600000000     C  N99              ENDDO
061700000000      *
061800000000     C                   ADD       1             XX
061900000000     C                   ENDDO
062000000000      *
062100000000     C                   MOVE      'S'           SBOSOS
062200000000     C                   ENDDO
062300000000     C                   ENDIF
062400950308      *
062500950308      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
062600950308     C                   MOVEA     £MOD          CMD(33)
062700950308     C                   MOVEA     £OUTQ         CMD(51)
062800000000      *
062900000000      * Eseguo OVRPRTF sul file di stampa
063000950308     C                   Z-ADD     82            LUNG             15 5
063100950308     C                   MOVEL     *BLANKS       COMMAN           82
063200000000     C                   MOVEA     CMD           COMMAN
063300000000     C                   CALL      'QCMDEXC'                            H1
063400000000     C                   PARM                    COMMAN
063500000000     C                   PARM                    LUNG
063600000000      *
063700000000     C                   SETON                                        43
063800000000     C                   EXSR      STAINI
063900000000      *
064000950404      * L'elaborazione solo se scelta la stampa (DS1RIS='S')
064100000615    1C     DS1RIS        IFNE      'R'
064200970312     C* Verifico su calendario se giorno festivo e lo memorizzo
064300990507     C* (Mi serve per sapere il tipo record da memorizzare in FNSBO)
064400970312     C                   CLEAR                   $TFA
064500970312     C                   CLEAR                   WFES
064600970312     C     KCLN          CHAIN     AZCLN01L                           98
064700970312     C     *IN98         IFEQ      *OFF
064800970312     C                   MOVEA     CLNMAT        GME
064900970312     C                   MOVE      GME($GI)      WFES              1
065000970312     C                   END
065100970312     C*
065200000000     C                   Z-ADD     *ZERO         $PRIMA            1 0
065300000000     C                   Z-ADD     *ZERO         $FILPR
065400950117     C                   MOVE      *BLANKS       $CBOPR
065500000000      *
065600000000      * Considero solamente le filiali presenti in LIN
065700000000     C                   Z-ADD     1             XX                3 0
065800000000     C     LIN(XX)       DOWGT     *ZERO
065900000000      *
066000000000     C                   Z-ADD     LIN(XX)       BLPLNP
066100970312     C*
066200970312      * Al primissimo giro memorizzo la filiale come precedente
066300970312     C     $PRIMA        IFEQ      *ZERO
066400970312     C                   Z-ADD     BLPLNP        $FILPR
066500970312     C                   Z-ADD     1             $PRIMA
066600970312     C                   ENDIF
066700970312      *
066800970312      * Per prima cosa controllo la rottura per filiale
066900970312     C     BLPLNP        IFNE      $FILPR
067000970312     C                   Z-ADD     1             $X
067100970312     C                   EXSR      TOTALE
067200970312     C                   Z-ADD     BLPLNP        $FILPR
067300970312     C                   ENDIF
067400970312     C*
067500010717     C     FNBLPK        SETLL     FNBLP31L
067600000000      *
067700000000     C                   DO        *HIVAL
067800010717     C   44FNBLPK        READE     FNBLP31L                               33
067900010717     C  N44LIN(XX)       READE     FNBLP31L                               33
068000000000      *
068100000000      * Per diminuire le letture, quando supero la data finale cambio LIN,XX
068200950213     C  N33
068300950213     CANN44$DATSP        COMP      DS1AAL                             33
068400000000      *
068500950213     C  N33$DATSP        IFGE      DS1ADA
068600950213     C     $DATSP        ANDLE     DS1AAL
068700950126     C*
068800950126     C* ESCLUDO I TIPI BOLLA CHE INIZIANO CON '£' (QUESTO PER FREGARE
068900950126     C* WANDA CON LE SUE BOLLE FANTASMA);  ANCHE SE RISULTANO COME
069000950126     C* C/SERVIZI NON HANNO COLLI E NON DEVO CONSIDERARLE
069100950126     C                   MOVEL     BLPCBO        WUNO              1
069200950126     C     WUNO          IFNE      '£'
069300000000     C                   EXSR      ELABOR
069400950126     C                   ENDIF
069500000000     C                   ENDIF
069600000000      *
069700000000     C  N33              ENDDO
069800000000      *
069900000000     C                   ADD       1             XX
070000000000     C                   ENDDO
070100000000      *
070200000000     C                   Z-ADD     1             $X                1 0
070300000000     C                   EXSR      TOTALE
070400000615    1C                   ENDIF
070500000615     C*
070600000615     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
070700000615     C     SAVRIS        IFEQ      'S'
070800000616     C                   MOVEL     'R'           DS1RIS
070900000615     C                   MOVEA     SVLIN         LIN
071000000615     C                   SETON                                        43
071100000615     C                   EXSR      STAINI
071200000616      * CANCELLO I CAMPI DI TOTALE PERHCE' ADESSO LI RISOMMA
071300000616     C                   Z-ADD     1             $X
071400000616     C                   DO        7             $X
071500000616     C                   Z-ADD     *ZERO         COL($X)
071600000616     C                   Z-ADD     *ZERO         PF3($X)
071700000616     C                   Z-ADD     *ZERO         PO3($X)
071800000616     C                   Z-ADD     *ZERO         PTA($X)
071900000616     C                   Z-ADD     *ZERO         VOL($X)
072000000616     C                   Z-ADD     *ZERO         SEF($X)
072100000616     C                   Z-ADD     *ZERO         SEA($X)
072200000616     C                   Z-ADD     *ZERO         SNF($X)
072300000616     C                   Z-ADD     *ZERO         SNA($X)
072400000616     C                   Z-ADD     *ZERO         SSF($X)
072500000616     C                   Z-ADD     *ZERO         SSA($X)
072600000616     C                   Z-ADD     *ZERO         SRF($X)
072700000616     C                   Z-ADD     *ZERO         SRA($X)
072800000616     C                   ENDDO
072900000615     C                   ENDIF
073000000615     C*
073100000615      * STAMPA O RISTAMPA
073200000615     C     DS1RIS        IFNE      ' '
073300000000     C                   SETON                                        43
073400000000     C                   EXSR      RISTAM
073500000000     C                   EXSR      STAFIN
073600000000     C                   SETOFF                                       43
073700000000     C                   EXSR      STAINI
073800000000     C                   EXSR      RISTAM
073900000000     C                   ENDIF
074000000000      *
074100000000     C                   EXSR      STAFIN
074200000000      *
074300000000     C                   ENDSR
074400000000      *****************************************************************
074500000000      * Elaborazione del record-bolla letto
074600000000      *****************************************************************
074700000000     C     ELABOR        BEGSR
074800000000      *
074900000000      * Verifico se si tratta di c/servizio o recupero
075000950117     C     BLPCBO        IFNE      $CBOPR
075100950213     C                   MOVE      '3A'          KCOD
075200950213     C                   MOVE      *BLANKS       KKEY
075300950213     C                   MOVEL     BLPCBO        KKEY
075400950213     C     KTAB1         CHAIN     TABEL00F                           99
075500000000     C  N99              MOVEL     TBLUNI        DS3A
075600950117     C  N99              MOVE      BLPCBO        $CBOPR
075700000000     C   99              CLEAR                   DS3A
075800000000     C                   MOVE      §3ARBL        $RBL
075900000000     C                   MOVEL     §3ATB1        $FRAS             1
076000000000     C                   ENDIF
076100000000      *
076200000000      * Definisco il valore gli indici di schiera
076300000000     C                   Z-ADD     1             $Z                1 0
076400000000     C     $RBL          IFEQ      'C'
076500000000     C                   Z-ADD     2             $Z
076600000000     C                   ENDIF
076700000000     C     $Z            ADD       5             $Y                1 0
076800000000      *
076900000000      * Verifico se si tratta di bolla in sospensione
077000000000      * ( Data ritiro merce < Data spedizione )
077100000000     C     BLPDRT        COMP      $DATSP                               34
077200000000      *
077300000000      * Verifico se "Porto franco" o "Porto assegnato"
077400000000     C     $FRAS         COMP      'F'                                    35
077500000000      *
077600000000      * Colli, peso e volume non sono validi se bolla di recupero
077700000000     C     $RBL          IFNE      'R'
077800000000     C                   ADD       BLPNCL        COL($Z)
077900000000     C   34              ADD       BLPNCL        COL($Y)
078000021217      *
078100021217      * Occhio al peso nella bolla!!     Sulla bolla devo verificare i dati
078200021217      * misurati con la macchina smistacolli (BLPNCP=Numero colli rilevati,
078300021217      * BLPPKC=Peso rilevato)
078400050112     c                   if           BLPncp = BLPncl
078500021217     c                             or BLPpkc >= BLPpkf
078600021217     c                   z-add     BLPpkc        BLPpkf
078700021217e   2c                   endif
078800000000      *
078900950117     C     BLPPKF        IFLE      3500
079000950117     C                   ADD       BLPPKF        PF3($Z)
079100950117     C   34              ADD       BLPPKF        PF3($Y)
079200000000     C                   ELSE
079300950117     C                   ADD       BLPPKF        PO3($Z)
079400950117     C   34              ADD       BLPPKF        PO3($Y)
079500000000     C                   ENDIF
079600000000      *
079700000000      * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
079800000000      * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
079900950117      * BLPVLC=Volume rilevato)
080000050112     C     BLPNCR        ifeq      BLPNCL
080100950117     C                   Z-ADD     BLPVLC        BLPVLF
080200000000     C                   ELSE
080300950117     C     BLPVLC        IFGE      BLPVLF
080400950117     C                   Z-ADD     BLPVLC        BLPVLF
080500000000     C                   ENDIF
080600000000     C                   ENDIF
080700000000      *
080800000000      * Calcolo il peso tassabile della spedizione, no per i c/servizio ($Z=2)
080900000000      * e solo per bolle con peso<=3500 Kg.
081000000000     C     $Z            IFNE      2
081100950117     C     BLPPKF        ANDLE     3500
081200950117     C                   Z-ADD     BLPPKF        $PESTA            9 1
081300950117     C     BLPVLF        IFNE      0
081400950117     C     BLPVLF        MULT(H)   §QTKPM        $COMPK            9 0
081500000000      * Per il peso tassabile, devo usare i maggior peso tra il reale e quello
081600000000      * calcolato con il rapporto fisso aziendale
081700000000     C     $COMPK        IFGT      $PESTA
081800000000     C                   Z-ADD     $COMPK        $PESTA
081900000000     C                   ENDIF
082000000000     C                   ENDIF
082100000000      * Calcolo l'arrotondamento in base alla tabella 3I (campo §3IARR)
082200000000     C     $PESTA        DIV       §3IARR        $COMPK
082300000000     C                   MVR                     $RESTO            5 0
082400000000     C     $RESTO        IFGT      0
082500000000     C     $COMPK        MULT      §3IARR        $PESTA
082600000000     C                   ADD       §3IARR        $PESTA
082700000000     C                   ENDIF
082800000000     C                   ADD       $PESTA        PTA($Z)
082900000000     C   34              ADD       $PESTA        PTA($Y)
083000000000     C                   ENDIF
083100000000      *
083200950117     C                   ADD       BLPVLF        VOL($Z)
083300950117     C   34              ADD       BLPVLF        VOL($Y)
083400000000      *
083500000000     C     BLPTSP        IFEQ      'E'
083600000000     C   35              ADD       1             SEF($Z)
083700000000     C  N35              ADD       1             SEA($Z)
083800000000     C   35
083900000000     CAN 34              ADD       1             SEF($Y)
084000000000     C  N35
084100000000     CAN 34              ADD       1             SEA($Y)
084200000000     C                   ELSE
084300000000      *
084400950117     C     BLPTSP        IFEQ      'D'
084500000619     C     BLPTSP        OREQ      'P'
084600000619     C   35              ADD       1             SSF($Z)
084700000619     C  N35              ADD       1             SSA($Z)
084800000619     C   35
084900000619     CAN 34              ADD       1             SSF($Y)
085000000619     C  N35
085100000619     CAN 34              ADD       1             SSA($Y)
085200000000     C                   ELSE
085300000000      *
085400950117     C   35              ADD       1             SNF($Z)
085500950117     C  N35              ADD       1             SNA($Z)
085600950117     C   35
085700950117     CAN 34              ADD       1             SNF($Y)
085800950117     C  N35
085900950117     CAN 34              ADD       1             SNA($Y)
086000000000     C                   ENDIF
086100000000     C                   ENDIF
086200000000      *
086300000000     C                   ENDIF
086400000000      *
086500000000     C     $RBL          IFEQ      'R'
086600000000     C   35              ADD       1             SRF($Z)
086700000000     C  N35              ADD       1             SRA($Z)
086800000000     C   35
086900000000     CAN 34              ADD       1             SRF($Y)
087000000000     C  N35
087100000000     CAN 34              ADD       1             SRA($Y)
087200000000     C                   ENDIF
087300000000      *
087400000000     C                   ENDSR
087500000000      *****************************************************************
087600000000      * Calcolo, stampa e azzeramento dei totali per:
087700000000      *   se $X=1    filiale
087800000000      *   se $X=2    conto servizi per filiale
087900000000      *   se $X=3    conto servizi per area
088000000000      *   se $X=4    area
088100000000      *   se $X=5    totale generale
088200000000      *   se $X=6    filiale             (sospensione)
088300000000      *   se $X=7    conto servizi fil.  (sospensione)
088400000000      *****************************************************************
088500000000     C     TOTALE        BEGSR
088600000000      *
088700000000     C                   MOVE      *BLANKS       ORGDEP
088800000000     C     $X            IFEQ      1
088900000000     C                   Z-ADD     $FILPR        FILIAP
089000000000     C                   SETON                                        40
089100000000     C     FILIAP        CHAIN     AZORG01L                           99
089200000000     C  N99              MOVEL     ORGDES        ORGDEP
089300000000     C                   ENDIF
089400000000      *
089500000000     C     $X            IFEQ      3
089600000000     C                   SETOFF                                       40
089700000000     C                   Z-ADD     *ZERO         FILIAP
089800000000     C     'C / SERV'    CAT       'IZIO':0      ORGDEP
089900000000     C                   ENDIF
090000000000      *
090100000000     C     $X            IFEQ      4
090200950117     C  N36              WRITE     LSC5T3
090300000000     C                   SETOFF                                       40
090400000000     C                   Z-ADD     *ZERO         FILIAP
090500000000     C                   MOVE      *BLANKS       ORGDEP
090600000000     C                   MOVEL     'TOTALE'      ORGDEP
090700000000     C                   ENDIF
090800000000      *
090900000000      * Il TOTALE GENERALE lo stampo solo se sono in sede
091000000000     C     $X            IFEQ      5
091100000000     C     SIMFEL        ANDEQ     *ZERO
091200950117     C  N36              WRITE     LSC5T3
091300000000     C                   SETOFF                                       40
091400000000     C                   Z-ADD     *ZERO         FILIAP
091500000000     C     'TOTALE G'    CAT       'ENERAL':0    ORGDEP
091600000000     C     ORGDEP        CAT       'E':0         ORGDEP
091700000000     C                   ENDIF
091800000000      *
091900000000     C                   Z-ADD     COL($X)       COLLIP
092000000000     C                   Z-ADD     PF3($X)       PEF35P
092100000000     C                   Z-ADD     PO3($X)       PEO35P
092200000000     C     PF3($X)       ADD       PO3($X)       PETOTP
092300000000     C                   Z-ADD     PTA($X)       PETASP
092400000000     C                   Z-ADD     VOL($X)       VOLUMP
092500000000     C     SEF($X)       ADD       SEA($X)       SPESTP
092600000000     C                   Z-ADD     SEF($X)       SPESFP
092700000000     C                   Z-ADD     SEA($X)       SPESAP
092800000000     C     SNF($X)       ADD       SNA($X)       SPNOTP
092900000000     C                   Z-ADD     SNF($X)       SPNOFP
093000000000     C                   Z-ADD     SNA($X)       SPNOAP
093100000000     C     SSF($X)       ADD       SSA($X)       SPSTTP
093200000000     C                   Z-ADD     SSF($X)       SPSTFP
093300000000     C                   Z-ADD     SSA($X)       SPSTAP
093400000000     C     SPESTP        ADD       SPNOTP        SPTOTP
093500000000     C                   ADD       SPSTTP        SPTOTP
093600000000     C     SPESFP        ADD       SPNOFP        SPTOFP
093700000000     C                   ADD       SPSTFP        SPTOFP
093800000000     C     SPESAP        ADD       SPNOAP        SPTOAP
093900000000     C                   ADD       SPSTAP        SPTOAP
094000970312     C     COLLIP        IFGT      *ZEROS
094100000000     C     SRF($X)       ADD       SRA($X)       SPRETP
094200000000     C                   Z-ADD     SRF($X)       SPREFP
094300000000     C                   Z-ADD     SRA($X)       SPREAP
094400970312     C                   ELSE
094500970312     C                   MOVE      *ZEROS        SPRETP
094600970312     C                   MOVE      *ZEROS        SPREFP
094700970312     C                   MOVE      *ZEROS        SPREAP
094800970312     C                   END
094900000000      *
095000000000      * Non devo stampare il totale generale se non sono in sede
095100000000     C     $X            IFLT      5
095200000000     C     $X            OREQ      5
095300000000     C     SIMFEL        ANDEQ     *ZERO
095400000000     C                   EXSR      STAMPA
095500000000     C                   ENDIF
095600000000      *
095700000000      * In questa fase aggiorno le schiere per i totali, con il segg. criterio:
095800000000      *    se ho stampato la filiale ($X=1) aggiorno l'area
095900000000      *    se ho stampato i c/servizi ($X=3) aggiorno l'area
096000000000      *    se ho stampato i totali area ($X=4) aggiorno il totale genrale
096100000000     C     $X            IFLT      5
096200000000     C                   Z-ADD     4             $W                1 0
096300000000     C     $X            IFEQ      4
096400000000     C                   Z-ADD     5             $W
096500000000     C                   ENDIF
096600000000      *
096700000000     C                   ADD       COL($X)       COL($W)
096800000000     C                   ADD       PF3($X)       PF3($W)
096900000000     C                   ADD       PO3($X)       PO3($W)
097000000000     C                   ADD       PTA($X)       PTA($W)
097100000000     C                   ADD       VOL($X)       VOL($W)
097200000000     C                   ADD       SEF($X)       SEF($W)
097300000000     C                   ADD       SEA($X)       SEA($W)
097400000000     C                   ADD       SNF($X)       SNF($W)
097500000000     C                   ADD       SNA($X)       SNA($W)
097600000000     C                   ADD       SSF($X)       SSF($W)
097700000000     C                   ADD       SSA($X)       SSA($W)
097800970312     C     COL($W)       IFGT      *ZEROS
097900000000     C                   ADD       SRF($X)       SRF($W)
098000000000     C                   ADD       SRA($X)       SRA($W)
098100970312     C                   ELSE
098200970312     C                   MOVE      *ZEROS        SRF($W)
098300970312     C                   MOVE      *ZEROS        SRA($W)
098400970312     C                   END
098500000000     C                   ENDIF
098600000000      *
098700000000      * Se in stampa (elaborazione), scrivo anche nell'archivio statistiche
098800950213     C     DS1RIS        IFNE      'R'
098900000000     C     $X            ANDEQ     1
099000000000     C     *IN43         ANDEQ     *ON
099100000000     C                   EXSR      AGGIOR
099200000000     C                   ENDIF
099300000000      *
099400000000      * Azzeramento dei campi gia' stampati
099500000000     C                   Z-ADD     *ZERO         COL($X)
099600000000     C                   Z-ADD     *ZERO         PF3($X)
099700000000     C                   Z-ADD     *ZERO         PO3($X)
099800000000     C                   Z-ADD     *ZERO         PTA($X)
099900000000     C                   Z-ADD     *ZERO         VOL($X)
100000000000     C                   Z-ADD     *ZERO         SEF($X)
100100000000     C                   Z-ADD     *ZERO         SEA($X)
100200000000     C                   Z-ADD     *ZERO         SNF($X)
100300000000     C                   Z-ADD     *ZERO         SNA($X)
100400000000     C                   Z-ADD     *ZERO         SSF($X)
100500000000     C                   Z-ADD     *ZERO         SSA($X)
100600000000     C                   Z-ADD     *ZERO         SRF($X)
100700000000     C                   Z-ADD     *ZERO         SRA($X)
100800000000      *
100900000000      * Se azzero la filiale (nr.1), devo azzerare anche i suoi c/servizi (nr.2)
101000000000      * Ma prima li sommo dei totali c/servizi dell'area (nr.3)
101100000000     C     $X            IFEQ      1
101200000000     C                   ADD       COL(2)        COL(3)
101300000000     C                   ADD       PF3(2)        PF3(3)
101400000000     C                   ADD       PO3(2)        PO3(3)
101500000000     C                   ADD       PTA(2)        PTA(3)
101600000000     C                   ADD       VOL(2)        VOL(3)
101700000000     C                   ADD       SEF(2)        SEF(3)
101800000000     C                   ADD       SEA(2)        SEA(3)
101900000000     C                   ADD       SNF(2)        SNF(3)
102000000000     C                   ADD       SNA(2)        SNA(3)
102100000000     C                   ADD       SSF(2)        SSF(3)
102200000000     C                   ADD       SSA(2)        SSA(3)
102300970312     C     COL(3)        IFGT      *ZEROS
102400000000     C                   ADD       SRF(2)        SRF(3)
102500000000     C                   ADD       SRA(2)        SRA(3)
102600970312     C                   ELSE
102700970312     C                   MOVE      *ZEROS        SRF(3)
102800970312     C                   MOVE      *ZEROS        SRA(3)
102900970312     C                   END
103000000000      *
103100000000     C                   Z-ADD     *ZERO         COL(2)
103200000000     C                   Z-ADD     *ZERO         PF3(2)
103300000000     C                   Z-ADD     *ZERO         PO3(2)
103400000000     C                   Z-ADD     *ZERO         PTA(2)
103500000000     C                   Z-ADD     *ZERO         VOL(2)
103600000000     C                   Z-ADD     *ZERO         SEF(2)
103700000000     C                   Z-ADD     *ZERO         SEA(2)
103800000000     C                   Z-ADD     *ZERO         SNF(2)
103900000000     C                   Z-ADD     *ZERO         SNA(2)
104000000000     C                   Z-ADD     *ZERO         SSF(2)
104100000000     C                   Z-ADD     *ZERO         SSA(2)
104200000000     C                   Z-ADD     *ZERO         SRF(2)
104300000000     C                   Z-ADD     *ZERO         SRA(2)
104400000000      *
104500000000     C                   Z-ADD     *ZERO         COL(6)
104600000000     C                   Z-ADD     *ZERO         PF3(6)
104700000000     C                   Z-ADD     *ZERO         PO3(6)
104800000000     C                   Z-ADD     *ZERO         PTA(6)
104900000000     C                   Z-ADD     *ZERO         VOL(6)
105000000000     C                   Z-ADD     *ZERO         SEF(6)
105100000000     C                   Z-ADD     *ZERO         SEA(6)
105200000000     C                   Z-ADD     *ZERO         SNF(6)
105300000000     C                   Z-ADD     *ZERO         SNA(6)
105400000000     C                   Z-ADD     *ZERO         SSF(6)
105500000000     C                   Z-ADD     *ZERO         SSA(6)
105600000000     C                   Z-ADD     *ZERO         SRF(6)
105700000000     C                   Z-ADD     *ZERO         SRA(6)
105800000000      *
105900000000     C                   Z-ADD     *ZERO         COL(7)
106000000000     C                   Z-ADD     *ZERO         PF3(7)
106100000000     C                   Z-ADD     *ZERO         PO3(7)
106200000000     C                   Z-ADD     *ZERO         PTA(7)
106300000000     C                   Z-ADD     *ZERO         VOL(7)
106400000000     C                   Z-ADD     *ZERO         SEF(7)
106500000000     C                   Z-ADD     *ZERO         SEA(7)
106600000000     C                   Z-ADD     *ZERO         SNF(7)
106700000000     C                   Z-ADD     *ZERO         SNA(7)
106800000000     C                   Z-ADD     *ZERO         SSF(7)
106900000000     C                   Z-ADD     *ZERO         SSA(7)
107000000000     C                   Z-ADD     *ZERO         SRF(7)
107100000000     C                   Z-ADD     *ZERO         SRA(7)
107200000000     C                   ENDIF
107300000000      *
107400000000     C                   ENDSR
107500000000      *****************************************************************
107600000000      * A fine stampa, emetto gli ultimissimi totali e quello generale
107700000000      *****************************************************************
107800000000     C     STAINI        BEGSR
107900000000      *
108000950213     C     DS1RIS        IFNE      *BLANKS
108100950117     C                   OPEN      FNLSC5P
108200000000     C                   SETOFF                                       31
108300000000      * Stampo l'intestazione
108400950117     C                   WRITE     LSC5T1
108500950117     C                   WRITE     LSC5T3
108600000000     C                   SETOFF                                       30
108700000000      *
108800000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
108900000000     C                   ENDIF
109000000000      *
109100000000     C                   ENDSR
109200000000      *****************************************************************
109300000000      * A fine stampa, emetto gli ultimissimi totali e quello generale
109400000000      *****************************************************************
109500000000     C     STAFIN        BEGSR
109600000000      *
109700950213     C     DS1RIS        IFNE      *BLANKS
109800000000      *
109900000000      * Stampa terminata
110000000000     C     $ALMEN        IFEQ      *ZERO
110100950117     C                   WRITE     LSC5E1
110200000000     C                   ELSE
110300000000     C     3             DO        5             $X
110400000000     C                   EXSR      TOTALE
110500000000     C                   ENDDO
110600000000     C                   ENDIF
110700950117     C                   WRITE     LSC5E2
110800000000      *
110900950117     C                   CLOSE     FNLSC5P
111000000000      *
111100000000     C                   ENDIF
111200000000      *
111300000000      * Aggiorno la data ultima elaborazione
111400950213     C     DS1RIS        IFNE      'R'
111500021016
111600021016     c                   Eval      kTbeCod = 'SDF'
111700021016     c                   Movel(p)  Simfel        kTbeKe1
111800021016     c     Ktbe          Chain     Tntbe01l
111900021016     c                   If        %Found(Tntbe01l)
112000021016     c                   Movel     TbeUni        dSdf
112100021016     c                   If        §SdfBolP = *Zeros or §SdfBolP > Ds1Ada
112200021016     c                   Z-Add     Ds1Ada        §SdfBolP
112300021016     c                   EndIf
112400021016     c                   If        §SdfBolU < Ds1Aal
112500021016     c                   Z-Add     Ds1Aal        §SdfBolU
112600021016     c                   EndIf
112700021016     c                   Movel     dSdf          TbeUni
112800021016     c                   Update    Tntbe000
112900021016     c                   EndIf
113000000000     C                   ENDIF
113100000000      *
113200000000     C                   ENDSR
113300000000      *****************************************************************
113400000000      * Stampa del dettaglio con test per l'overflow
113500000000      *****************************************************************
113600000000     C     STAMPA        BEGSR
113700000000      *
113800970312    1C     DS1RIS        IFNE      *BLANKS
113900000000      *
114000000000      * Stampo solo se la riga ha dei valori (testo il nr.colli)
114100950929      *  o i colli C/S > 0
114200970312    2C     COLLIP        IFGT      *ZERO
114300970312     C     $X            OREQ      3
114400970312     C     COL(2)        ANDGT     0
114500000000      *
114600950117     C   30              WRITE     LSC5T1
114700950117     C   30              WRITE     LSC5T3
114800000000     C   30              SETOFF                                       30
114900950117     C                   WRITE     LSC5D1
115000970312    2C                   END
115100000000      *
115200970312    2C     COLLIP        IFGT      *ZEROS
115300970312     C     $ALMEN        ANDEQ     *ZERO
115400970312     C     COL(2)        ORGT      *ZEROS
115500970312     C     $ALMEN        ANDEQ     *ZERO
115600000000     C                   Z-ADD     1             $ALMEN
115700970312    2C                   ENDIF
115800000000      *
115900970312    1C                   ENDIF
116000000000      *
116100000000     C                   ENDSR
116200000000      *****************************************************************
116300000000      * Scrittura su archivio statistiche
116400000000      *****************************************************************
116500000000     C     AGGIOR        BEGSR
116600000000      *
116700000000     C                   Z-ADD     1             $W
116800000000     C     $W            DOUGT     7
116900000000      *
117000000000     C                   Z-ADD     SIMFEL        SBOFLE
117100000000     C                   Z-ADD     $FILPR        SBOLNP
117200950213     C                   Z-ADD     DS1ADA        SBODRE
117300000000     C                   MOVE      *BLANK        SBOSOS
117400000000     C                   MOVE      *BLANK        SBOTRE
117500000000     C     $W            IFEQ      2
117600000000     C     $W            OREQ      7
117700000000     C                   MOVE      'C'           SBOTRE
117800000000     C                   ENDIF
117900000000     C     $W            IFGT      5
118000000000     C                   MOVE      'S'           SBOSOS
118100000000     C                   ENDIF
118200000000      *
118300000000     C                   Z-ADD     COL($W)       SBOCOL
118400000000     C                   Z-ADD     PF3($W)       SBOPF3
118500000000     C                   Z-ADD     PO3($W)       SBOPO3
118600000000     C                   Z-ADD     PTA($W)       SBOPTA
118700000000     C                   Z-ADD     VOL($W)       SBOVOL
118800000000     C                   Z-ADD     SEF($W)       SBOSEF
118900000000     C                   Z-ADD     SEA($W)       SBOSEA
119000000000     C                   Z-ADD     SNF($W)       SBOSNF
119100000000     C                   Z-ADD     SNA($W)       SBOSNA
119200000000     C                   Z-ADD     SSF($W)       SBOSSF
119300000000     C                   Z-ADD     SSA($W)       SBOSSA
119400970312     C     COL($W)       IFGT      *ZEROS
119500000000     C                   Z-ADD     SRF($W)       SBOSRF
119600000000     C                   Z-ADD     SRA($W)       SBOSRA
119700970312     C                   ELSE
119800970312     C                   MOVE      *ZEROS        SBOSRF
119900970312     C                   MOVE      *ZEROS        SBOSRA
120000970312     C                   END
120100000000      *
120200000000      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
120300040922      * riempo il "flag di trasmissione" (SBOFTR)
120400040922     c                   z-add     dateu         sbodtr
120500040922     C***                Z-ADD     *ZERO         SBODTR
120600040922     C***  §5CSBO        IFEQ      'S'
120700000000     C                   MOVE      *BLANK        SBOFTR
120800040922     C***                ELSE
120900040922     C***                MOVE      'T'           SBOFTR
121000040922     C***                ENDIF
121100000000      *
121200970312     C* memorizzo record con sbotre='F' (festivo) se numero colli = 0
121300970312     C* e giorno elaborato festivo
121400970312     C     WFES          IFEQ      'F'
121500970312     C     COL($W)       ANDEQ     0
121600970312     C                   MOVE      'F'           SBOTRE
121700970312     C                   END
121800970312     C*
121900970312     C     $W            IFEQ      1
122000970312     C     COL($W)       ORGT      *ZERO
122100990507     C                   WRITE     FNSBO000
122200990519     C                   MOVEL     'S'           WTRASM            1
122300000000     C                   ENDIF
122400000000      *
122500000000      * Solo se ho registrato con $X=1 (coie' rottura per filiale), registro
122600000000      * anche per c/servizio ($X=2), per filiale con spspensione ($X=5) e
122700000000      * per c/servizio con sospensione ($X=6)
122800000000     C     $W            IFEQ      2
122900000000     C                   Z-ADD     6             $W
123000000000     C                   ELSE
123100000000     C                   ADD       1             $W
123200000000     C                   ENDIF
123300000000      *
123400000000     C                   ENDDO
123500000000      *
123600000000     C                   ENDSR
123700000000      *****************************************************************
123800990507      * RISTAMPA: senza elaborazione, solo lettura da file statistiche (FNSBO)
123900000000      * Tutto il ciclo di ristampa viene eseguito 2 volte, prima per le bolle
124000000000      * normali quindi per quelle in sospensione
124100000000      *****************************************************************
124200000000     C     RISTAM        BEGSR
124300000000      *
124400050829     c                   setoff                                       3237
124500000000      * Verifico se richiesta una specifica area
124600950213     C     DS1FEL        COMP      *ZERO                                  32
124700050829     c
124800050829     c                   if        ds1fel=0 and simfel>0
124900050829     c                   eval      ds1fel=simfel
125000050829     c                   setoff                                       32
125100050829     c                   endif
125200050829     c
125300050829     c                   if        lin(1)>0 and lin(2)=0
125400050829     c                   eval      klin=lin(1)
125500050829     c                   seton                                        37
125600050829     c                   endif
125700000000      *
125800000000     C                   Z-ADD     *ZERO         $PRIM1            1 0
125900000000     C                   Z-ADD     *ZERO         $FILPR
126000000000     C                   Z-ADD     *ZERO         $FI2PR
126100000000      *
126200000000     C   43              MOVE      *BLANK        SBOSOS
126300000000     C  N43              MOVE      'S'           SBOSOS
126400950213     C                   Z-ADD     DS1FEL        SBOFLE
126500950213     C                   Z-ADD     DS1ADA        SBODRE
126600000000     C                   Z-ADD     *ZERO         SBOLNP
126700000000      *
126800050829     C  n37FNSBOK        SETLL     FNSBO01L
126900050829     C   37ksbo          SETLL     FNSBO01L
127000950929    1C                   DO        *HIVAL
127100050829     c                   if        *in37
127200050830     C     ksbo2         reade(N)  FNSBO01L                               33
127300050829     c                   else
127400990507     C  N32FLSB2K        READE(N)  FNSBO01L                               33
127500990507     C   32SBOSOS        READE(N)  FNSBO01L                               33
127600050829     c                   endif
127700000000      *
127800990507    2C  N33SBODRE        IFGE      DS1ADA
127900990507     C     SBODRE        ANDLE     DS1AAL
128000000000      *
128100000000      * Solo le filiali presenti nella schiera LIN
128200050829     c                   if        not *in37 and *in32
128300000000     C     SBOLNP        LOOKUP    LIN                                    99
128400050829     c                   else
128500050829     c                   seton                                        99
128600050829     c                   endif
128700050829     c
128800950929    3C     *IN99         IFEQ      *ON
128900000000      *
129000000000      * Al primo giro memorizzo il "precedente"
129100950929    4C     $PRIM1        IFEQ      *ZERO
129200000000     C                   Z-ADD     SBOFLE        $FI2PR
129300000000     C                   Z-ADD     SBOLNP        $FILPR
129400000000     C                   Z-ADD     1             $PRIM1
129500950929    4C                   ENDIF
129600000000      *
129700000000      * Se cambia filiale stampo i totali per filiale
129800950929    4C     SBOTRE        IFEQ      *BLANK
129900950929     C     SBOLNP        ANDNE     $FILPR
130000000000     C                   Z-ADD     1             $X
130100000000     C                   EXSR      TOTALE
130200000000     C                   Z-ADD     SBOLNP        $FILPR
130300950929    4C                   ENDIF
130400000000      *
130500000000      * Se cambia area stampo i totali per c/servizio
130600950929    4C     SBOFLE        IFNE      $FI2PR
130700000000     C                   Z-ADD     3             $X
130800000000     C                   EXSR      TOTALE
130900000000     C                   Z-ADD     4             $X
131000000000     C                   EXSR      TOTALE
131100000000     C                   Z-ADD     SBOFLE        $FI2PR
131200950929    4C                   ENDIF
131300000000      *
131400000000     C                   EXSR      ELABO2
131500000000      *
131600950929    3C                   ENDIF
131700000000      *
131800950929    2C                   ENDIF
131900950929    1C  N33              ENDDO
132000000000     C                   Z-ADD     1             $X
132100000000     C                   EXSR      TOTALE
132200000000      *
132300000000     C     ENDRIS        ENDSR
132400000000      *****************************************************************
132500990507      * Avendo letto il file FNSBO00F memorizzo i campi del record nelle schiere
132600000000      *****************************************************************
132700000000     C     ELABO2        BEGSR
132800000000      *
132900000000      * Definisco il valore gli indici di schiera
133000000000     C                   Z-ADD     1             $Z
133100000000     C     SBOTRE        IFEQ      'C'
133200000000     C                   Z-ADD     2             $Z
133300000000     C                   ENDIF
133400000000      *
133500000000     C                   ADD       SBOCOL        COL($Z)
133600000000     C                   ADD       SBOPF3        PF3($Z)
133700000000     C                   ADD       SBOPO3        PO3($Z)
133800000000     C                   ADD       SBOPTA        PTA($Z)
133900000000     C                   ADD       SBOVOL        VOL($Z)
134000000000     C                   ADD       SBOSEF        SEF($Z)
134100000000     C                   ADD       SBOSEA        SEA($Z)
134200000000     C                   ADD       SBOSNF        SNF($Z)
134300000000     C                   ADD       SBOSNA        SNA($Z)
134400000000     C                   ADD       SBOSSF        SSF($Z)
134500000000     C                   ADD       SBOSSA        SSA($Z)
134600000000     C                   ADD       SBOSRF        SRF($Z)
134700000000     C                   ADD       SBOSRA        SRA($Z)
134800000000      *
134900000000     C                   ENDSR
135000000000      *****************************************************************
135100000000      *                                4         5         6         7         8
135200950308** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
135300950308OVRPRTF FILE(FNLSC5P) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.boll.
135400950308')
