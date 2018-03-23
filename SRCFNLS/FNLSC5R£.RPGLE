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
001900090910      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
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
003600090910     FFiSBO01L  UF A E           K DISK
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
006500090910     D ShF             S              7  0 DIM(7)
006600090910     D ShA             S              7  0 DIM(7)
006700000000     D SNF             S              7  0 DIM(7)
006800000000     D SNA             S              7  0 DIM(7)
006900000000     D SSF             S              7  0 DIM(7)
007000000000     D SSA             S              7  0 DIM(7)
007100000000     D SRF             S              7  0 DIM(7)
007200000000     D SRA             S              7  0 DIM(7)
007300020418
007400020418     d sav_t06         S              3  0 dim(30)
007500020418
007600020418     d wc              s              3  0
007700021016
007800021016     d kTbeCod         s                   Like(TbeCod)
007900021016     d kTbeKe1         s                   Like(TbeKe1)
008000050829     d klin            s                   Like(ds1lnp)
008100020418
008200000000      *
008300090911     D TIBS02        E DS                  EXTNAME(TIBS02DS)
008400090911     d dvpodeco      e ds
008500090911     d
008600090604     d ds5e          e ds
008700000000     D KPJBA         E DS
008800000000     D DS3A          E DS
008900000000     D DS3I          E DS
009000000000     D DS3L          E DS
009100970707     D DS5P          E DS
009200950117     D DSQT1         E DS
009300000000     D CNCR80        E DS
009400000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
009500950213     D* DS PER TRUL06R - CARICAMENTO £X
009600950213     D DSUL06        E DS                  EXTNAME(TRUL06DS)
009700950314     D  T06                    1     90  0
009800950314     D                                     DIM(30)
009900000000      * DS per il passaggio parametri
010000950213     D DSLS01        E DS
010100970312     D  ANNODA                41     44  0
010200970312     D  MESEDA                45     46  0
010300970312     D  $GI                   47     48  0
010400950213     D                 DS
010500950117      * DATA INIZIALE AAAA/MM/GG (8 CARATTERI)
010600950213     D  DATADA                 1      8  0
010700950213     D  $ANNDA                 1      4  0
010800950213     D  $MGSDA                 5      8  0
010900000000      * DS per data di spedizione della bolla
011000000000     D                 DS
011100950117     D  $DATSP                 1      8  0
011200950117     D  BLPAAS                 1      4  0
011300950117     D  BLPMGS                 5      8  0
011400040922
011500040922     D WLBDAT          DS                  INZ
011600040922     D  G02DAT                 1      8  0
011700040922     D  G02INV                 9     16  0
011800040922     D  G02ERR                17     17
011900040922     D  G02TGI                18     22  0
012000000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
012100000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
012200021016      * Ds date statistiche
012300021016     d dSdf          e ds
012400000000      *
012500000000     C     *ENTRY        PLIST
012600000000     C                   PARM                    KPJBA
012700950213     C                   MOVEL     KPJBU         DSLS01
012800950213     C                   Z-ADD     DS1ADA        DATADA
012900000000      *
013000010717     C     FNBLPK        KLIST
013100000000     C                   KFLD                    BLPLNP
013200000000     C                   KFLD                    $ANNDA
013300000000     C                   KFLD                    $MGSDA
013400990507     C     FNSBOK        KLIST
013500000000     C                   KFLD                    SBOSOS
013600000000     C                   KFLD                    SBOFLE
013700000000     C                   KFLD                    SBOLNP
013800000000     C                   KFLD                    SBODRE
013900990507     C     FLSB1K        KLIST
014000000000     C                   KFLD                    SBOSOS
014100000000     C                   KFLD                    SBOFLE
014200000000     C                   KFLD                    SBOLNP
014300990507     C     FLSB2K        KLIST
014400000000     C                   KFLD                    SBOSOS
014500000000     C                   KFLD                    SBOFLE
014600050829     C     ksbo          KLIST
014700050829     C                   KFLD                    SBOSOS
014800050829     C                   KFLD                    SBOFLE
014900050829     C                   KFLD                    klin
015000050829     C                   KFLD                    sbodre
015100050829     C     ksbo2         KLIST
015200050829     C                   KFLD                    SBOSOS
015300050829     C                   KFLD                    SBOFLE
015400050829     C                   KFLD                    klin
015500970312     C*
015600970312     C     KCLN          KLIST
015700970312     C                   KFLD                    SIMFEL
015800970312     C                   KFLD                    $TFA
015900970312     C                   KFLD                    ANNODA
016000970312     C                   KFLD                    MESEDA
016100000000      *
016200000615     C     *LIKE         DEFINE    DS1RIS        SAVRIS
016300000615     C     *LIKE         DEFINE    BLPLNP        $FILPR
016400000000     C     *LIKE         DEFINE    BLPLNP        $FI2PR
016500950117     C     *LIKE         DEFINE    BLPCBO        $CBOPR
016600000000     C     *LIKE         DEFINE    §3ARBL        $RBL
016700970312     C     *LIKE         DEFINE    SIMFEL        $TFA
016800000000      *****************************************************************
016900000000      *              M A I N      L I N E
017000000000      *****************************************************************
017100000000     C                   Z-ADD     1             CODUT
017200000000      *
017300950117      * LEGGO LA DSQT1 PER IL RAPPORTO PESO/VOLUME
017400950213     C                   MOVEL     'QT'          KCOD
017500950213     C                   MOVEL     '1       '    KKEY
017600950213     C     KTAB1         CHAIN     TABEL00F                           99
017700950117     C  N99              MOVEL     TBLUNI        DSQT1
017800950117     C   99              CLEAR                   DSQT1
017900000000      *
018000000000      * Reperisco la tabella 3I per calcoli che serviranno poi
018100950213     C                   MOVEL     '3I'          KCOD
018200950213     C                   MOVEL     '1       '    KKEY
018300950213     C     KTAB1         CHAIN     TABEL00F                           99
018400000000     C  N99              MOVEL     TBLUNI        DS3I
018500000000     C   99              CLEAR                   DS3I
018600000000     C   99              Z-ADD     20            §3IARR
018700000000      *
018800090604      * carico le descrizioni fisse dei tipi servizio
018900090604      * chiodo la descrizione per ogni tipo servizio visto che la descrizione
019000090604      * è da stampare nella testata ed è fissa
019100090604      * le colonne dei tipi servizio da stampare sono
019200090911      * 'C + D' 'E' +"H"
019300090604     c                   clear                   ds5e
019400090604     c                   clear                   wdtspc
019500090604     c                   clear                   wdtspd
019600090604     c                   clear                   wdtspe
019700090604     c                   eval      kcod = '5E'
019800090604     c     ktab2         setll     tabel00f
019900090604     c                   do        *hival
020000090604     c     ktab2         reade     tabel00f
020100090604     c                   if        %eof(tabel00f)
020200090604     c                   leave
020300090604     c                   endif
020400090604     c                   eval      ds5e = tbluni
020500090604     c                   if        tblkey = 'C'
020600090604     c                   eval      wdtspc = §5ed08
020700090604     c                   endif
020800090604     c                   if        tblkey = 'D'
020900090604     c                   eval      wdtspd = §5ed08
021000090604     c                   endif
021100090604     c                   if        tblkey = 'E'
021200090604     c                   eval      wdtspe = §5ed08
021300090604     c                   endif
021400090910     c                   if        tblkey = 'H'
021500090910     c                   eval      wdtsph = §5ed08
021600090910     c                   endif
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
024400090911
024500090911      * Carico la VPODECO
024600090911     c                   clear                   tibs02
024700090911     c                   clear                   dvpodeco
024800090911     c                   eval      t02mod = 'C'
024900090911     c                   eval      t02sif = knsif
025000090911     c                   eval      t02cod = 'VPO'
025100090911     c                   eval      t02ke1 = 'DECO'
025200090911     c                   call      'TIBS02R'
025300090911     c                   parm                    kpjba
025400090911     c                   parm                    tibs02
025500090911     c                   if        t02err = *blanks
025600090911     c                   eval      dvpodeco = t02uni
025700090911     c                   else
025800090911     c                   eval      §vpotsph=20091001
025900090911     c                   endif
026000090911     c
026100000000      *
026200950213      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
026300950213     C                   EXSR      COMUNE
026400000000      *
026500000000     C                   SETON                                        LR
026600020212     C*****************************************************************
026700020212     C*  PARTE COMUNE STAMPE GIORNALIERE
026800020212     C*
026900020212     C*  . REPERIMENTO 'SIMFEL'
027000020212     C*  . CARICAMENTO '£1'
027100020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
027200020212     C*  . CODA DI STAMPA ('£OUTQ')
027300020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
027400020212     C*  . MODULO PER LA STAMPA ('£MOD')
027500020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
027600020212     C*****************************************************************
027700020212     C     COMUNE        BEGSR
027800020212     C*
027900020212     C*****************************************************************
028000020212     C*       RIEPILOGO INDICATORI:
028100020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
028200020212     C* 11    -           STAMPA PER 1L
028300020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
028400050829     C* 32 e 37 PER ristampa
028500020212     C*****************************************************************
028600020212     C                   Z-ADD     1             CODUT
028700020212     C*
028800020212     C* Reperimento parametri aziendali
028900020212     C                   CALL      'X§PARUT'
029000020212     C                   PARM                    UT§DSE
029100020212     C                   MOVEL     RAGUT         RSUT             20
029200020212     C                   MOVEL     REC80         CNCR80
029300020212     C*
029400020212     C* Chiave di accesso per il file tabelle
029500020212     C     KTAB1         KLIST
029600020212     C                   KFLD                    CODUT
029700020212     C                   KFLD                    KCOD
029800020212     C                   KFLD                    KKEY
029900020212     C     KTAB2         KLIST
030000020212     C                   KFLD                    CODUT
030100020212     C                   KFLD                    KCOD
030200021016
030300021016     c     Ktbe          Klist
030400021016     c                   Kfld                    kTbeCod
030500021016     c                   Kfld                    kTbeKe1
030600020212     C*
030700020212     C     *LIKE         DEFINE    TBLCOD        KCOD
030800020212     C     *LIKE         DEFINE    TBLKEY        KKEY
030900020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
031000020212     C*
031100020212     C* DETERMINO DATA E ORA DI ESECUZIONE
031200020212     C                   TIME                    WTIME            14 0
031300020212     C                   MOVE      WTIME         WDATE             8 0
031400020212     C                   MOVEL     WTIME         UTIME             6 0
031500040922     c*
031600040922     C                   CLEAR                   WLBDAT
031700040922     C                   MOVE      Wtime         G02DAT
031800040922     C                   CALL      'XSRDA8'
031900040922     C                   PARM                    WLBDAT
032000040922     C                   Z-ADD     G02INV        dateu             8 0
032100090911     c
032200090911     c* Se già decorrente tipo servizio H accendo indicatore 10 per
032300090911     c*  stampare anche le spedizioni H
032400090916     c                   if        ds1aal>=§vpotsph
032500090911     c                   seton                                        10
032600090911     c                   endif
032700020212     C*
032800020418     C* CARICO '£1'
032900020212     C                   CLEAR                   KPJBU
033000020212     C                   CLEAR                   DSUL06
033100020212     C                   MOVE      '£1'          D06COD
033200020212     C                   MOVEL     SIMFEL        D06KEY
033300050802     C                   MOVE      'S'           D06KEY
033400050802     C                   MOVEL     ds1ada        D06drf
033500020212     C                   MOVEL     DSUL06        KPJBU
033600020212     C                   CALL      'TRUL06R'
033700020212     C                   PARM                    KPJBA
033800020212     C                   MOVEL     KPJBU         DSUL06
033900020418     c                   movea     t06           sav_t06
034000020418
034100020418      * se in arrivo carico la £6
034200020418 b1  c                   if        ds1tbo = 'A'
034300020418     c                   clear                   wc
034400020418 b2  c                   do        30            wa
034500020422     c                   if        sav_t06(wa) = *zeros
034600020422     c                   leave
034700020422     c                   endif
034800020418     c                   clear                   kpjbu
034900020418     c                   clear                   dsul06
035000020418     c                   eval      d06cod = '£6'
035100020418     c                   movel     sav_t06(wa)   d06key
035200050802     C                   MOVE      'S'           D06KEY
035300020418     c                   eval      d06esc = 'S'
035400050802     C                   MOVEL     ds1ada        D06drf
035500020419     c                   movel     dsul06        kpjbu
035600020418     c                   call      'TRUL06R'
035700020418     c                   parm                    kpjba
035800020418     c                   movel     kpjbu         dsul06
035900020418 b3  c                   do        30            wb
036000020418     c                   if        t06(wb) = *zeros
036100020418     c                   leave
036200020418     c                   endif
036300020418     c                   if        t06(wb) <> sav_t06(wa)
036400020418     c                   add       1             wc
036500020418     c                   z-add     t06(wb)       l6(wc)
036600020418     c                   endif
036700020418 e3  c                   enddo
036800020418 e2  c                   enddo
036900020418
037000020418     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
037100020212     C                   Z-ADD     *ZERO         WB
037200020418 b2  C                   DO        29            WA
037300020418     C     sav_T06(WA)   LOOKUP    L6                                     30
037400020212     C  N30              ADD       1             WB
037500020418     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
037600020418 e2  C                   ENDDO
037700020418 x1  C                   ELSE
037800020418     C                   MOVEA     sav_T06       L1
037900020418 e1  C                   ENDIF
038000020212     C*
038100020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
038200020212     C                   MOVEA     L1            LIN
038300020212     C                   CLEAR                   L1
038400020212     C                   Z-ADD     *ZERO         WB                3 0
038500020212     C     1             DO        30            WPT               3 0
038600020212     C     LIN(WPT)      IFNE      0
038700020212     C                   CLEAR                   FNLSC4
038800020212     C                   Z-ADD     LIN(WPT)      IC4LIN
038900020212     C                   MOVE      'B'           IC4FLG
039000020212     C                   MOVEL     FNLSC4        KPJBU
039100020212     C                   CALL      'FNLSC4R1'
039200020212     C                   PARM                    KPJBA
039300050802     C                   PARM                    ds1ada
039400020212     C                   MOVEL     KPJBU         FNLSC4
039500020212     C     OC4ESE        IFEQ      'S'
039600020212     C                   ADD       1             WB
039700020212     C                   MOVE      LIN(WPT)      L1(WB)
039800020212     C                   ENDIF
039900020212     C                   ENDIF
040000020212     C                   ENDDO
040100070103     C*
040200070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
040300070103     C                   Z-ADD     *ZERO         WB                3 0
040400070103     C                   DO        29            WA                3 0
040500070103     C     L1(WA)        IFNE      *ZEROS
040600070103     C     L1(WA)        ANDNE     SIMFEL
040700070103     C                   ADD       1             WB
040800070103     C                   Z-ADD     L1(WA)        LV2(WB)
040900070103     C                   ENDIF
041000070103     C                   ENDDO
041100020212     C*
041200020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
041300020212     C*
041400020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
041500020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
041600020212     C* la stampa e' lanciata da menu
041700020212     C                   MOVEL     '3L'          KCOD
041800020212     C                   MOVEL     DS1PGM        KKEY
041900020212     C                   MOVE      DS1FPG        KKEY
042000020212     C     KTAB1         CHAIN     TABEL00F                           30
042100020212     C  N30              MOVEL     TBLUNI        DS3L
042200020212     C   30              MOVEL     'F'           §3LD1L
042300020212     C   30              MOVEL     ' '           §3LD2L
042400020212     C   30              MOVEL     '*STD    '    §3LDST
042500020212     C*
042600020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
042700020212     C                   MOVEL     §3LDST        £MOD             10
042800020212     C                   CLEAR                   LIN
042900020212     C*
043000020212     C* Verifico chi ha lanciato la stampa
043100020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
043200020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
043300020212     C* DS1FGS<>0  --->  si proviene da menu
043400020212     C     DS1FGS        IFEQ      0
043500020212     C*
043600020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
043700020212     C* lancio, voglio la stampa;  invece
043800020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
043900020212     C*
044000020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
044100020212     C* creare stampa per 1°lvl. (se stessa)
044200020212     C     DS1LNP        IFEQ      SIMFEL
044300020212     C*
044400020212     C* Il flag §3LD1L ha i seguenti significati:
044500020212     C*    ' ' = tutti i dati
044600020212     C*    '1' = solo filiale di 1' livello
044700020212     C     §3LD1L        IFEQ      '1'
044800020212     C                   SETON                                        11
044900020212     C                   END
045000020212     C*
045100020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
045200020212     C* nella stampa
045300020212     C*
045400020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
045500020212     C  N11              MOVEA     L1            LIN
045600020212     C   11              MOVEA     SIMFEL        LIN(1)
045700020212     C*
045800020212     C* IMPOSTO LA STAMPANTE:
045900020212     C* STAMPANTE TABULATI
046000020212     C                   MOVEL     '5P'          KCOD
046100020212     C                   MOVEL     'T'           W004A             4
046200020212     C                   MOVE      LIN(1)        W004A
046300020212     C                   MOVEL(P)  W004A         KKEY
046400020212     C     KTAB1         CHAIN     TABEL00F                           99
046500020212     C     *IN99         IFEQ      *OFF
046600020212     C                   MOVEL     TBLUNI        DS5P
046700020212     C                   MOVEL     §5POTQ        £OUTQ            10
046800020212     C                   END
046900020212     C*
047000020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
047100020212     C                   EXSR      LEGGI
047200020212     C*
047300020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
047400020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
047500020212     C                   ELSE
047600020212     C*
047700020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
047800020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
047900020212     C*
048000020212     C* Il significato del fla §3LD2L e' il seguente:
048100020212     C*    ' ' = tutti i dati
048200020212     C*    'F' = per ogni 2' livello
048300020212     C     §3LD2L        IFEQ      'F'
048400020212     C                   SETON                                        14
048500020212     C                   ENDIF
048600020212     C*
048700020212     C                   Z-ADD     1             WB
048800020212     C                   MOVEA     *ZEROS        LIN
048900020212     C  N14              MOVEA     L1            LIN
049000020212     C*
049100020212     C* Lancio per tutti i 2° lvl
049200020212     C     DS1LNP        IFEQ      0
049300020212     C*
049400020212     C* LV2 contiene tutte le filiali di 2° lvl
049500020212     C     LV2(WB)       DOWNE     0
049600020212      *
049700020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
049800020212     C                   CLEAR                   FNLSC4
049900020212     C                   Z-ADD     LV2(WB)       IC4LIN
050000020212     C                   MOVEL     FNLSC4        KPJBU
050100020212     C                   CALL      'FNLSC4R1'
050200020212     C                   PARM                    KPJBA
050300050802     C                   PARM                    ds1ada
050400020212     C                   MOVEL     KPJBU         FNLSC4
050500020212      *
050600020212     C     OC4PT1        IFEQ      'S'
050700020212     C     §3LPT         ANDEQ     'N'
050800020212     C                   GOTO      NOLEGG
050900020212     C                   ENDIF
051000020212      *
051100020212     C     OC4EXP        IFEQ      'S'
051200020212     C     §3LEXP        ANDEQ     'N'
051300020212     C                   GOTO      NOLEGG
051400020212     C                   ENDIF
051500020212      *
051600020212     C     OC4DPD        IFEQ      'S'
051700020212     C     §3LDPD        ANDEQ     'N'
051800020212     C                   GOTO      NOLEGG
051900020212     C                   ENDIF
052000020212      *
052100020212     C     OC4BAR        IFEQ      'S'
052200020212     C     §3LBAR        ANDEQ     'N'
052300020212     C                   GOTO      NOLEGG
052400020212     C                   ENDIF
052500020212      *
052600020212     C     OC4FED        IFEQ      'S'
052700020212     C     §3LFED        ANDEQ     'N'
052800020212     C                   GOTO      NOLEGG
052900020212     C                   ENDIF
053000020212      *
053100020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
053200020212     C*  dati degli altri 2° lvl
053300020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
053400020212     C   14              MOVEL     LV2(WB)       LIN(1)
053500020212     C*
053600020212     C* IMPOSTO LA STAMPANTE
053700020212     C* STAMPANTE TABULATI
053800020212     C                   MOVEL     '5P'          KCOD
053900020212     C                   MOVEL     'T'           W004A             4
054000020212     C                   MOVE      LV2(WB)       W004A
054100020212     C                   MOVEL(P)  W004A         KKEY
054200020212     C     KTAB1         CHAIN     TABEL00F                           99
054300020212     C     *IN99         IFEQ      *OFF
054400020212     C                   MOVEL     TBLUNI        DS5P
054500020212     C                   MOVEL     §5POTQ        £OUTQ            10
054600020212     C                   END
054700020212     C*
054800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
054900020212     C                   EXSR      LEGGI
055000020212     C*
055100020212     C     NOLEGG        TAG
055200020212     C*
055300020212     C                   ADD       1             WB
055400020212     C                   ENDDO
055500020212     C*
055600020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
055700020212     C                   ELSE
055800020212     C*
055900020212     C* Lancio solo per la 2° lvl richiesta
056000020212     C   14              MOVEL     DS1LNP        LIN(1)
056100020212     C*
056200020212     C* STAMPANTE
056300020212     C* STAMPANTE TABULATI
056400020212     C                   MOVEL     '5P'          KCOD
056500020212     C                   MOVEL     'T'           W004A             4
056600020212     C                   MOVE      DS1LNP        W004A
056700020212     C                   MOVEL(P)  W004A         KKEY
056800020212     C     KTAB1         CHAIN     TABEL00F                           99
056900020212     C     *IN99         IFEQ      *OFF
057000020212     C                   MOVEL     TBLUNI        DS5P
057100020212     C                   MOVEL     §5POTQ        £OUTQ            10
057200020212     C                   END
057300020212     C*
057400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
057500020212     C                   EXSR      LEGGI
057600020212     C*
057700020212     C                   ENDIF
057800020212     C                   ENDIF
057900020212     C*
058000020212     C                   ELSE
058100020212     C*
058200020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
058300020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
058400020212     C     DS1LNP        IFEQ      0
058500020212     C                   MOVEA     L1            LIN
058600020212     C* ...altrimenti elaboro solo se stessa
058700020212     C                   ELSE
058800020212     C                   MOVEL     DS1LNP        LIN(1)
058900020212     C                   ENDIF
059000020212     C*
059100020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
059200020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
059300020212     C* PRENDO QUELLA PRESENTE IN KPJBA
059400020212     C     DS1FGS        IFEQ      SIMFEL
059500020212     C                   MOVEL     KQOTD         £OUTQ
059600030627     C**!!!              MOVEL     KQOTD         £OUTQF
059700020212     C                   ELSE
059800020212     C* STAMPANTE TABULATI
059900020212     C                   MOVEL     '5P'          KCOD
060000020212     C                   MOVEL     'T'           W004A             4
060100020212     C                   MOVE      DS1FGS        W004A
060200020212     C                   MOVEL(P)  W004A         KKEY
060300020212     C     KTAB1         CHAIN     TABEL00F                           99
060400020212     C     *IN99         IFEQ      *OFF
060500020212     C                   MOVEL     TBLUNI        DS5P
060600020212     C                   MOVEL     §5POTQ        £OUTQ            10
060700020212     C                   END
060800020212     C                   ENDIF
060900020212     C*
061000020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
061100020212     C                   EXSR      LEGGI
061200020212     C                   ENDIF
061300020212     C*
061400020212     C                   ENDSR
061500000000      *****************************************************************
061600000000      * APRO IL FILE DI STAMPA ED INIZIO LE LETTURE
061700000000      *****************************************************************
061800000000     C     LEGGI         BEGSR
061900000615     C                   CLEAR                   SAVRIS
062000000000      *
062100000000      * Se sono nelle stampe giornaliere non voglio diciture
062200950213     C     DS1FGS        IFEQ      *ZERO
062300000000     C     SIMFEL        ANDNE     *ZERO
062400000000     C                   MOVE      *BLANKS       STRIP
062500000000     C                   MOVE      *BLANKS       STRIP1
062600000000     C                   ENDIF
062700000000      *
062800000000      * Cancello dal file statistiche i record della giornata che vado a elabor.
062900950213     C     DS1RIS        IFNE      'R'
063000000615     C* CARICO LA TABELLA '£1'ED IGNORO LA LIN PASSATA
063100000615     C                   CLEAR                   KPJBU
063200000615     C                   CLEAR                   DSUL06
063300000615     C                   MOVE      '£1'          D06COD
063400000615     C                   MOVEL     SIMFEL        D06KEY
063500050802     C                   MOVE      'S'           D06KEY
063600050802     C                   MOVEL     ds1ada        D06drf
063700000615     C                   MOVEL     DSUL06        KPJBU
063800000615     C                   CALL      'TRUL06R'
063900000615     C                   PARM                    KPJBA
064000000615     C                   MOVEL     KPJBU         DSUL06
064100000615     C**
064200000615     C* SE DEVO SOLO ELABORARE RICOPRO DIRETTAMENTE LA DS LIN
064300000615     C* SE DEVO ELABORARE E STAMPARE, MI SALVO LA LIN DI STAMPA
064400000615     C*  CHE USO ALLA FINE
064500000615     C     DS1RIS        IFNE      ' '
064600000615     C                   MOVEA     LIN           SVLIN
064700000615     C                   MOVEL     DS1RIS        SAVRIS
064800000615     C                   CLEAR                   DS1RIS
064900000615     C                   ENDIF
065000000615     C                   MOVEA     T06           LIN
065100000615     C**
065200000000     C                   MOVE      *BLANKS       SBOSOS
065300000000     C                   DO        2
065400000000     C                   Z-ADD     SIMFEL        SBOFLE
065500950213     C                   Z-ADD     DS1ADA        SBODRE
065600000000      *
065700000000      * Elimino solo le linee della LIN (che dovro' poi elaborare)
065800000000     C                   Z-ADD     1             XX
065900000000     C     LIN(XX)       DOWGT     *ZERO
066000000000      *
066100000000     C                   Z-ADD     LIN(XX)       SBOLNP
066200950213     C                   Z-ADD     DS1ADA        SBODRE
066300090910     C     FNSBOK        SETLL     FiSBO01L
066400000000     C                   DO        *HIVAL
066500090910     C     FLSB1K        READE     FiSBO01L                               99
066600990507     C  N99SBODRE        COMP      DS1AAL                             99
066700090910     C  N99              DELETE    FiSBO000
066800000000     C  N99              ENDDO
066900000000      *
067000000000     C                   ADD       1             XX
067100000000     C                   ENDDO
067200000000      *
067300000000     C                   MOVE      'S'           SBOSOS
067400000000     C                   ENDDO
067500000000     C                   ENDIF
067600950308      *
067700950308      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
067800950308     C                   MOVEA     £MOD          CMD(33)
067900950308     C                   MOVEA     £OUTQ         CMD(51)
068000000000      *
068100000000      * Eseguo OVRPRTF sul file di stampa
068200950308     C                   Z-ADD     82            LUNG             15 5
068300950308     C                   MOVEL     *BLANKS       COMMAN           82
068400000000     C                   MOVEA     CMD           COMMAN
068500000000     C                   CALL      'QCMDEXC'                            H1
068600000000     C                   PARM                    COMMAN
068700000000     C                   PARM                    LUNG
068800000000      *
068900000000     C                   SETON                                        43
069000000000     C                   EXSR      STAINI
069100000000      *
069200950404      * L'elaborazione solo se scelta la stampa (DS1RIS='S')
069300000615    1C     DS1RIS        IFNE      'R'
069400970312     C* Verifico su calendario se giorno festivo e lo memorizzo
069500990507     C* (Mi serve per sapere il tipo record da memorizzare in FNSBO)
069600970312     C                   CLEAR                   $TFA
069700970312     C                   CLEAR                   WFES
069800970312     C     KCLN          CHAIN     AZCLN01L                           98
069900970312     C     *IN98         IFEQ      *OFF
070000970312     C                   MOVEA     CLNMAT        GME
070100970312     C                   MOVE      GME($GI)      WFES              1
070200970312     C                   END
070300970312     C*
070400000000     C                   Z-ADD     *ZERO         $PRIMA            1 0
070500000000     C                   Z-ADD     *ZERO         $FILPR
070600950117     C                   MOVE      *BLANKS       $CBOPR
070700000000      *
070800000000      * Considero solamente le filiali presenti in LIN
070900000000     C                   Z-ADD     1             XX                3 0
071000000000     C     LIN(XX)       DOWGT     *ZERO
071100000000      *
071200000000     C                   Z-ADD     LIN(XX)       BLPLNP
071300970312     C*
071400970312      * Al primissimo giro memorizzo la filiale come precedente
071500970312     C     $PRIMA        IFEQ      *ZERO
071600970312     C                   Z-ADD     BLPLNP        $FILPR
071700970312     C                   Z-ADD     1             $PRIMA
071800970312     C                   ENDIF
071900970312      *
072000970312      * Per prima cosa controllo la rottura per filiale
072100970312     C     BLPLNP        IFNE      $FILPR
072200970312     C                   Z-ADD     1             $X
072300970312     C                   EXSR      TOTALE
072400970312     C                   Z-ADD     BLPLNP        $FILPR
072500970312     C                   ENDIF
072600970312     C*
072700010717     C     FNBLPK        SETLL     FNBLP31L
072800000000      *
072900000000     C                   DO        *HIVAL
073000010717     C   44FNBLPK        READE     FNBLP31L                               33
073100010717     C  N44LIN(XX)       READE     FNBLP31L                               33
073200000000      *
073300000000      * Per diminuire le letture, quando supero la data finale cambio LIN,XX
073400950213     C  N33
073500950213     CANN44$DATSP        COMP      DS1AAL                             33
073600000000      *
073700950213     C  N33$DATSP        IFGE      DS1ADA
073800950213     C     $DATSP        ANDLE     DS1AAL
073900950126     C*
074000950126     C* ESCLUDO I TIPI BOLLA CHE INIZIANO CON '£' (QUESTO PER FREGARE
074100950126     C* WANDA CON LE SUE BOLLE FANTASMA);  ANCHE SE RISULTANO COME
074200950126     C* C/SERVIZI NON HANNO COLLI E NON DEVO CONSIDERARLE
074300950126     C                   MOVEL     BLPCBO        WUNO              1
074400160607     c
074500160607     c* Escudo anche le cons anomale 7 per bolle chiuse in partenza
074600160607     c*  per merce mai affidata
074700950126     C     WUNO          IFNE      '£'
074800160607     c     blpcca        andne     '7'
074900000000     C                   EXSR      ELABOR
075000950126     C                   ENDIF
075100000000     C                   ENDIF
075200000000      *
075300000000     C  N33              ENDDO
075400000000      *
075500000000     C                   ADD       1             XX
075600000000     C                   ENDDO
075700000000      *
075800000000     C                   Z-ADD     1             $X                1 0
075900000000     C                   EXSR      TOTALE
076000000615    1C                   ENDIF
076100000615     C*
076200000615     C* SE DEVO ANCHE STAMPARE, LO FACCIO ORA
076300000615     C     SAVRIS        IFEQ      'S'
076400000616     C                   MOVEL     'R'           DS1RIS
076500000615     C                   MOVEA     SVLIN         LIN
076600000615     C                   SETON                                        43
076700000615     C                   EXSR      STAINI
076800000616      * CANCELLO I CAMPI DI TOTALE PERHCE' ADESSO LI RISOMMA
076900000616     C                   Z-ADD     1             $X
077000000616     C                   DO        7             $X
077100000616     C                   Z-ADD     *ZERO         COL($X)
077200000616     C                   Z-ADD     *ZERO         PF3($X)
077300000616     C                   Z-ADD     *ZERO         PO3($X)
077400000616     C                   Z-ADD     *ZERO         PTA($X)
077500000616     C                   Z-ADD     *ZERO         VOL($X)
077600000616     C                   Z-ADD     *ZERO         SEF($X)
077700000616     C                   Z-ADD     *ZERO         SEA($X)
077800090910     C                   Z-ADD     *ZERO         ShF($X)
077900090910     C                   Z-ADD     *ZERO         ShA($X)
078000000616     C                   Z-ADD     *ZERO         SNF($X)
078100000616     C                   Z-ADD     *ZERO         SNA($X)
078200000616     C                   Z-ADD     *ZERO         SSF($X)
078300000616     C                   Z-ADD     *ZERO         SSA($X)
078400000616     C                   Z-ADD     *ZERO         SRF($X)
078500000616     C                   Z-ADD     *ZERO         SRA($X)
078600000616     C                   ENDDO
078700000615     C                   ENDIF
078800000615     C*
078900000615      * STAMPA O RISTAMPA
079000000615     C     DS1RIS        IFNE      ' '
079100000000     C                   SETON                                        43
079200000000     C                   EXSR      RISTAM
079300000000     C                   EXSR      STAFIN
079400000000     C                   SETOFF                                       43
079500000000     C                   EXSR      STAINI
079600000000     C                   EXSR      RISTAM
079700000000     C                   ENDIF
079800000000      *
079900000000     C                   EXSR      STAFIN
080000000000      *
080100000000     C                   ENDSR
080200000000      *****************************************************************
080300000000      * Elaborazione del record-bolla letto
080400000000      *****************************************************************
080500000000     C     ELABOR        BEGSR
080600000000      *
080700000000      * Verifico se si tratta di c/servizio o recupero
080800950117     C     BLPCBO        IFNE      $CBOPR
080900950213     C                   MOVE      '3A'          KCOD
081000950213     C                   MOVE      *BLANKS       KKEY
081100950213     C                   MOVEL     BLPCBO        KKEY
081200950213     C     KTAB1         CHAIN     TABEL00F                           99
081300000000     C  N99              MOVEL     TBLUNI        DS3A
081400950117     C  N99              MOVE      BLPCBO        $CBOPR
081500000000     C   99              CLEAR                   DS3A
081600000000     C                   MOVE      §3ARBL        $RBL
081700000000     C                   MOVEL     §3ATB1        $FRAS             1
081800000000     C                   ENDIF
081900000000      *
082000000000      * Definisco il valore gli indici di schiera
082100000000     C                   Z-ADD     1             $Z                1 0
082200000000     C     $RBL          IFEQ      'C'
082300000000     C                   Z-ADD     2             $Z
082400000000     C                   ENDIF
082500000000     C     $Z            ADD       5             $Y                1 0
082600000000      *
082700000000      * Verifico se si tratta di bolla in sospensione
082800000000      * ( Data ritiro merce < Data spedizione )
082900000000     C     BLPDRT        COMP      $DATSP                               34
083000000000      *
083100000000      * Verifico se "Porto franco" o "Porto assegnato"
083200000000     C     $FRAS         COMP      'F'                                    35
083300000000      *
083400000000      * Colli, peso e volume non sono validi se bolla di recupero
083500000000     C     $RBL          IFNE      'R'
083600000000     C                   ADD       BLPNCL        COL($Z)
083700000000     C   34              ADD       BLPNCL        COL($Y)
083800021217      *
083900021217      * Occhio al peso nella bolla!!     Sulla bolla devo verificare i dati
084000021217      * misurati con la macchina smistacolli (BLPNCP=Numero colli rilevati,
084100021217      * BLPPKC=Peso rilevato)
084200050112     c                   if           BLPncp = BLPncl
084300021217     c                             or BLPpkc >= BLPpkf
084400021217     c                   z-add     BLPpkc        BLPpkf
084500021217e   2c                   endif
084600000000      *
084700950117     C     BLPPKF        IFLE      3500
084800950117     C                   ADD       BLPPKF        PF3($Z)
084900950117     C   34              ADD       BLPPKF        PF3($Y)
085000000000     C                   ELSE
085100950117     C                   ADD       BLPPKF        PO3($Z)
085200950117     C   34              ADD       BLPPKF        PO3($Y)
085300000000     C                   ENDIF
085400000000      *
085500000000      * Occhio al volume nella bolla!!   Sulla bolla devo verificare i dati
085600000000      * misurati con la macchina smistacolli (BLPNCR=Numero colli rilevati,
085700950117      * BLPVLC=Volume rilevato)
085800050112     C     BLPNCR        ifeq      BLPNCL
085900950117     C                   Z-ADD     BLPVLC        BLPVLF
086000000000     C                   ELSE
086100950117     C     BLPVLC        IFGE      BLPVLF
086200950117     C                   Z-ADD     BLPVLC        BLPVLF
086300000000     C                   ENDIF
086400000000     C                   ENDIF
086500000000      *
086600000000      * Calcolo il peso tassabile della spedizione, no per i c/servizio ($Z=2)
086700000000      * e solo per bolle con peso<=3500 Kg.
086800000000     C     $Z            IFNE      2
086900950117     C     BLPPKF        ANDLE     3500
087000950117     C                   Z-ADD     BLPPKF        $PESTA            9 1
087100950117     C     BLPVLF        IFNE      0
087200950117     C     BLPVLF        MULT(H)   §QTKPM        $COMPK            9 0
087300000000      * Per il peso tassabile, devo usare i maggior peso tra il reale e quello
087400000000      * calcolato con il rapporto fisso aziendale
087500000000     C     $COMPK        IFGT      $PESTA
087600000000     C                   Z-ADD     $COMPK        $PESTA
087700000000     C                   ENDIF
087800000000     C                   ENDIF
087900000000      * Calcolo l'arrotondamento in base alla tabella 3I (campo §3IARR)
088000000000     C     $PESTA        DIV       §3IARR        $COMPK
088100000000     C                   MVR                     $RESTO            5 0
088200000000     C     $RESTO        IFGT      0
088300000000     C     $COMPK        MULT      §3IARR        $PESTA
088400000000     C                   ADD       §3IARR        $PESTA
088500000000     C                   ENDIF
088600000000     C                   ADD       $PESTA        PTA($Z)
088700000000     C   34              ADD       $PESTA        PTA($Y)
088800000000     C                   ENDIF
088900000000      *
089000950117     C                   ADD       BLPVLF        VOL($Z)
089100950117     C   34              ADD       BLPVLF        VOL($Y)
089200000000      *
089300090910     c                   select
089400090910     c                   when      blptsp='E'
089500000000     C   35              ADD       1             SEF($Z)
089600000000     C  N35              ADD       1             SEA($Z)
089700000000     C   35
089800000000     CAN 34              ADD       1             SEF($Y)
089900000000     C  N35
090000000000     CAN 34              ADD       1             SEA($Y)
090100000000      *
090200090910     c                   when      blptsp='H'
090300090910     C   35              ADD       1             ShF($Z)
090400090910     C  N35              ADD       1             ShA($Z)
090500090910     C   35
090600090910     CAN 34              ADD       1             ShF($Y)
090700090910     C  N35
090800090910     CAN 34              ADD       1             ShA($Y)
090900090910      *
091000090910     c                   when      BLPTSP='D'
091100090910     C**** BLPTSP        IFEQ      'D'
091200090910     C**** BLPTSP        OREQ      'P'
091300000619     C   35              ADD       1             SSF($Z)
091400000619     C  N35              ADD       1             SSA($Z)
091500000619     C   35
091600000619     CAN 34              ADD       1             SSF($Y)
091700000619     C  N35
091800000619     CAN 34              ADD       1             SSA($Y)
091900090910     C                   other
092000000000      *
092100090910     c* Tipo servizio "C"
092200950117     C   35              ADD       1             SNF($Z)
092300950117     C  N35              ADD       1             SNA($Z)
092400950117     C   35
092500950117     CAN 34              ADD       1             SNF($Y)
092600950117     C  N35
092700950117     CAN 34              ADD       1             SNA($Y)
092800090910     C                   ENDsl
092900000000      *
093000000000     C                   ENDIF
093100000000      *
093200000000     C     $RBL          IFEQ      'R'
093300000000     C   35              ADD       1             SRF($Z)
093400000000     C  N35              ADD       1             SRA($Z)
093500000000     C   35
093600000000     CAN 34              ADD       1             SRF($Y)
093700000000     C  N35
093800000000     CAN 34              ADD       1             SRA($Y)
093900000000     C                   ENDIF
094000000000      *
094100000000     C                   ENDSR
094200000000      *****************************************************************
094300000000      * Calcolo, stampa e azzeramento dei totali per:
094400000000      *   se $X=1    filiale
094500000000      *   se $X=2    conto servizi per filiale
094600000000      *   se $X=3    conto servizi per area
094700000000      *   se $X=4    area
094800000000      *   se $X=5    totale generale
094900000000      *   se $X=6    filiale             (sospensione)
095000000000      *   se $X=7    conto servizi fil.  (sospensione)
095100000000      *****************************************************************
095200000000     C     TOTALE        BEGSR
095300000000      *
095400000000     C                   MOVE      *BLANKS       ORGDEP
095500000000     C     $X            IFEQ      1
095600000000     C                   Z-ADD     $FILPR        FILIAP
095700000000     C                   SETON                                        40
095800000000     C     FILIAP        CHAIN     AZORG01L                           99
095900000000     C  N99              MOVEL     ORGDES        ORGDEP
096000000000     C                   ENDIF
096100000000      *
096200000000     C     $X            IFEQ      3
096300000000     C                   SETOFF                                       40
096400000000     C                   Z-ADD     *ZERO         FILIAP
096500000000     C     'C / SERV'    CAT       'IZIO':0      ORGDEP
096600000000     C                   ENDIF
096700000000      *
096800000000     C     $X            IFEQ      4
096900090911     c  n36              if        not *in10
097000090911     C                   WRITE     LSC5T3
097100090911     c                   else
097200090911     C                   WRITE     LSC5T3n
097300090911     c                   endif
097400000000     C                   SETOFF                                       40
097500000000     C                   Z-ADD     *ZERO         FILIAP
097600000000     C                   MOVE      *BLANKS       ORGDEP
097700000000     C                   MOVEL     'TOTALE'      ORGDEP
097800000000     C                   ENDIF
097900000000      *
098000000000      * Il TOTALE GENERALE lo stampo solo se sono in sede
098100000000     C     $X            IFEQ      5
098200000000     C     SIMFEL        ANDEQ     *ZERO
098300090911     c  n36              if        not *in10
098400090911     C                   WRITE     LSC5T3
098500090911     c                   else
098600090911     C                   WRITE     LSC5T3n
098700090911     c                   endif
098800000000     C                   SETOFF                                       40
098900000000     C                   Z-ADD     *ZERO         FILIAP
099000000000     C     'TOTALE G'    CAT       'ENERAL':0    ORGDEP
099100000000     C     ORGDEP        CAT       'E':0         ORGDEP
099200000000     C                   ENDIF
099300000000      *
099400000000     C                   Z-ADD     COL($X)       COLLIP
099500000000     C                   Z-ADD     PF3($X)       PEF35P
099600000000     C                   Z-ADD     PO3($X)       PEO35P
099700000000     C     PF3($X)       ADD       PO3($X)       PETOTP
099800000000     C                   Z-ADD     PTA($X)       PETASP
099900000000     C                   Z-ADD     VOL($X)       VOLUMP
100000000000     C     SEF($X)       ADD       SEA($X)       SPESTP
100100000000     C                   Z-ADD     SEF($X)       SPESFP
100200000000     C                   Z-ADD     SEA($X)       SPESAP
100300090910     C     ShF($X)       ADD       ShA($X)       SPhSTP
100400090910     C                   Z-ADD     ShF($X)       SPhSFP
100500090910     C                   Z-ADD     ShA($X)       SPhSAP
100600000000     C     SNF($X)       ADD       SNA($X)       SPNOTP
100700000000     C                   Z-ADD     SNF($X)       SPNOFP
100800000000     C                   Z-ADD     SNA($X)       SPNOAP
100900000000     C     SSF($X)       ADD       SSA($X)       SPSTTP
101000000000     C                   Z-ADD     SSF($X)       SPSTFP
101100000000     C                   Z-ADD     SSA($X)       SPSTAP
101200000000     C     SPESTP        ADD       SPNOTP        SPTOTP
101300000000     C                   ADD       SPSTTP        SPTOTP
101400090914     C                   ADD       SPhsTP        SPTOTP
101500000000     C     SPESFP        ADD       SPNOFP        SPTOFP
101600000000     C                   ADD       SPSTFP        SPTOFP
101700090914     C                   ADD       SPhsFP        SPTOFP
101800000000     C     SPESAP        ADD       SPNOAP        SPTOAP
101900000000     C                   ADD       SPSTAP        SPTOAP
102000090914     C                   ADD       SPhsAP        SPTOAP
102100970312     C     COLLIP        IFGT      *ZEROS
102200000000     C     SRF($X)       ADD       SRA($X)       SPRETP
102300000000     C                   Z-ADD     SRF($X)       SPREFP
102400000000     C                   Z-ADD     SRA($X)       SPREAP
102500970312     C                   ELSE
102600970312     C                   MOVE      *ZEROS        SPRETP
102700970312     C                   MOVE      *ZEROS        SPREFP
102800970312     C                   MOVE      *ZEROS        SPREAP
102900970312     C                   END
103000000000      *
103100000000      * Non devo stampare il totale generale se non sono in sede
103200000000     C     $X            IFLT      5
103300000000     C     $X            OREQ      5
103400000000     C     SIMFEL        ANDEQ     *ZERO
103500000000     C                   EXSR      STAMPA
103600000000     C                   ENDIF
103700000000      *
103800000000      * In questa fase aggiorno le schiere per i totali, con il segg. criterio:
103900000000      *    se ho stampato la filiale ($X=1) aggiorno l'area
104000000000      *    se ho stampato i c/servizi ($X=3) aggiorno l'area
104100000000      *    se ho stampato i totali area ($X=4) aggiorno il totale genrale
104200000000     C     $X            IFLT      5
104300000000     C                   Z-ADD     4             $W                1 0
104400000000     C     $X            IFEQ      4
104500000000     C                   Z-ADD     5             $W
104600000000     C                   ENDIF
104700000000      *
104800000000     C                   ADD       COL($X)       COL($W)
104900000000     C                   ADD       PF3($X)       PF3($W)
105000000000     C                   ADD       PO3($X)       PO3($W)
105100000000     C                   ADD       PTA($X)       PTA($W)
105200000000     C                   ADD       VOL($X)       VOL($W)
105300000000     C                   ADD       SEF($X)       SEF($W)
105400000000     C                   ADD       SEA($X)       SEA($W)
105500090910     C                   ADD       ShF($X)       ShF($W)
105600090910     C                   ADD       ShA($X)       ShA($W)
105700000000     C                   ADD       SNF($X)       SNF($W)
105800000000     C                   ADD       SNA($X)       SNA($W)
105900000000     C                   ADD       SSF($X)       SSF($W)
106000000000     C                   ADD       SSA($X)       SSA($W)
106100970312     C     COL($W)       IFGT      *ZEROS
106200000000     C                   ADD       SRF($X)       SRF($W)
106300000000     C                   ADD       SRA($X)       SRA($W)
106400970312     C                   ELSE
106500970312     C                   MOVE      *ZEROS        SRF($W)
106600970312     C                   MOVE      *ZEROS        SRA($W)
106700970312     C                   END
106800000000     C                   ENDIF
106900000000      *
107000000000      * Se in stampa (elaborazione), scrivo anche nell'archivio statistiche
107100950213     C     DS1RIS        IFNE      'R'
107200000000     C     $X            ANDEQ     1
107300000000     C     *IN43         ANDEQ     *ON
107400000000     C                   EXSR      AGGIOR
107500000000     C                   ENDIF
107600000000      *
107700000000      * Azzeramento dei campi gia' stampati
107800000000     C                   Z-ADD     *ZERO         COL($X)
107900000000     C                   Z-ADD     *ZERO         PF3($X)
108000000000     C                   Z-ADD     *ZERO         PO3($X)
108100000000     C                   Z-ADD     *ZERO         PTA($X)
108200000000     C                   Z-ADD     *ZERO         VOL($X)
108300000000     C                   Z-ADD     *ZERO         SEF($X)
108400000000     C                   Z-ADD     *ZERO         SEA($X)
108500090910     C                   Z-ADD     *ZERO         ShF($X)
108600090910     C                   Z-ADD     *ZERO         ShA($X)
108700000000     C                   Z-ADD     *ZERO         SNF($X)
108800000000     C                   Z-ADD     *ZERO         SNA($X)
108900000000     C                   Z-ADD     *ZERO         SSF($X)
109000000000     C                   Z-ADD     *ZERO         SSA($X)
109100000000     C                   Z-ADD     *ZERO         SRF($X)
109200000000     C                   Z-ADD     *ZERO         SRA($X)
109300000000      *
109400000000      * Se azzero la filiale (nr.1), devo azzerare anche i suoi c/servizi (nr.2)
109500000000      * Ma prima li sommo dei totali c/servizi dell'area (nr.3)
109600000000     C     $X            IFEQ      1
109700000000     C                   ADD       COL(2)        COL(3)
109800000000     C                   ADD       PF3(2)        PF3(3)
109900000000     C                   ADD       PO3(2)        PO3(3)
110000000000     C                   ADD       PTA(2)        PTA(3)
110100000000     C                   ADD       VOL(2)        VOL(3)
110200000000     C                   ADD       SEF(2)        SEF(3)
110300000000     C                   ADD       SEA(2)        SEA(3)
110400090910     C                   ADD       ShF(2)        ShF(3)
110500090910     C                   ADD       ShA(2)        ShA(3)
110600000000     C                   ADD       SNF(2)        SNF(3)
110700000000     C                   ADD       SNA(2)        SNA(3)
110800000000     C                   ADD       SSF(2)        SSF(3)
110900000000     C                   ADD       SSA(2)        SSA(3)
111000970312     C     COL(3)        IFGT      *ZEROS
111100000000     C                   ADD       SRF(2)        SRF(3)
111200000000     C                   ADD       SRA(2)        SRA(3)
111300970312     C                   ELSE
111400970312     C                   MOVE      *ZEROS        SRF(3)
111500970312     C                   MOVE      *ZEROS        SRA(3)
111600970312     C                   END
111700000000      *
111800000000     C                   Z-ADD     *ZERO         COL(2)
111900000000     C                   Z-ADD     *ZERO         PF3(2)
112000000000     C                   Z-ADD     *ZERO         PO3(2)
112100000000     C                   Z-ADD     *ZERO         PTA(2)
112200000000     C                   Z-ADD     *ZERO         VOL(2)
112300000000     C                   Z-ADD     *ZERO         SEF(2)
112400000000     C                   Z-ADD     *ZERO         SEA(2)
112500090910     C                   Z-ADD     *ZERO         ShF(2)
112600090910     C                   Z-ADD     *ZERO         ShA(2)
112700000000     C                   Z-ADD     *ZERO         SNF(2)
112800000000     C                   Z-ADD     *ZERO         SNA(2)
112900000000     C                   Z-ADD     *ZERO         SSF(2)
113000000000     C                   Z-ADD     *ZERO         SSA(2)
113100000000     C                   Z-ADD     *ZERO         SRF(2)
113200000000     C                   Z-ADD     *ZERO         SRA(2)
113300000000      *
113400000000     C                   Z-ADD     *ZERO         COL(6)
113500000000     C                   Z-ADD     *ZERO         PF3(6)
113600000000     C                   Z-ADD     *ZERO         PO3(6)
113700000000     C                   Z-ADD     *ZERO         PTA(6)
113800000000     C                   Z-ADD     *ZERO         VOL(6)
113900000000     C                   Z-ADD     *ZERO         SEF(6)
114000000000     C                   Z-ADD     *ZERO         SEA(6)
114100090910     C                   Z-ADD     *ZERO         ShF(6)
114200090910     C                   Z-ADD     *ZERO         ShA(6)
114300000000     C                   Z-ADD     *ZERO         SNF(6)
114400000000     C                   Z-ADD     *ZERO         SNA(6)
114500000000     C                   Z-ADD     *ZERO         SSF(6)
114600000000     C                   Z-ADD     *ZERO         SSA(6)
114700000000     C                   Z-ADD     *ZERO         SRF(6)
114800000000     C                   Z-ADD     *ZERO         SRA(6)
114900000000      *
115000000000     C                   Z-ADD     *ZERO         COL(7)
115100000000     C                   Z-ADD     *ZERO         PF3(7)
115200000000     C                   Z-ADD     *ZERO         PO3(7)
115300000000     C                   Z-ADD     *ZERO         PTA(7)
115400000000     C                   Z-ADD     *ZERO         VOL(7)
115500000000     C                   Z-ADD     *ZERO         SEF(7)
115600000000     C                   Z-ADD     *ZERO         SEA(7)
115700090910     C                   Z-ADD     *ZERO         ShF(7)
115800090910     C                   Z-ADD     *ZERO         ShA(7)
115900000000     C                   Z-ADD     *ZERO         SNF(7)
116000000000     C                   Z-ADD     *ZERO         SNA(7)
116100000000     C                   Z-ADD     *ZERO         SSF(7)
116200000000     C                   Z-ADD     *ZERO         SSA(7)
116300000000     C                   Z-ADD     *ZERO         SRF(7)
116400000000     C                   Z-ADD     *ZERO         SRA(7)
116500000000     C                   ENDIF
116600000000      *
116700000000     C                   ENDSR
116800000000      *****************************************************************
116900090910      * Stampe iniziali
117000000000      *****************************************************************
117100000000     C     STAINI        BEGSR
117200000000      *
117300950213     C     DS1RIS        IFNE      *BLANKS
117400950117     C                   OPEN      FNLSC5P
117500000000     C                   SETOFF                                       31
117600000000      * Stampo l'intestazione
117700090911     c                   if        not *in10
117800950117     C                   WRITE     LSC5T1
117900950117     C                   WRITE     LSC5T3
118000090911     c                   else
118100090911     C                   WRITE     LSC5T1n
118200090911     C                   WRITE     LSC5T3n
118300090911     c                   endif
118400000000     C                   SETOFF                                       30
118500000000      *
118600000000     C                   Z-ADD     *ZERO         $ALMEN            1 0
118700000000     C                   ENDIF
118800000000      *
118900000000     C                   ENDSR
119000000000      *****************************************************************
119100000000      * A fine stampa, emetto gli ultimissimi totali e quello generale
119200000000      *****************************************************************
119300000000     C     STAFIN        BEGSR
119400000000      *
119500950213     C     DS1RIS        IFNE      *BLANKS
119600000000      *
119700000000      * Stampa terminata
119800000000     C     $ALMEN        IFEQ      *ZERO
119900950117     C                   WRITE     LSC5E1
120000000000     C                   ELSE
120100000000     C     3             DO        5             $X
120200000000     C                   EXSR      TOTALE
120300000000     C                   ENDDO
120400000000     C                   ENDIF
120500950117     C                   WRITE     LSC5E2
120600000000      *
120700950117     C                   CLOSE     FNLSC5P
120800000000      *
120900000000     C                   ENDIF
121000000000      *
121100000000      * Aggiorno la data ultima elaborazione
121200950213     C     DS1RIS        IFNE      'R'
121300021016
121400021016     c                   Eval      kTbeCod = 'SDF'
121500021016     c                   Movel(p)  Simfel        kTbeKe1
121600021016     c     Ktbe          Chain     Tntbe01l
121700021016     c                   If        %Found(Tntbe01l)
121800021016     c                   Movel     TbeUni        dSdf
121900021016     c                   If        §SdfBolP = *Zeros or §SdfBolP > Ds1Ada
122000021016     c                   Z-Add     Ds1Ada        §SdfBolP
122100021016     c                   EndIf
122200021016     c                   If        §SdfBolU < Ds1Aal
122300021016     c                   Z-Add     Ds1Aal        §SdfBolU
122400021016     c                   EndIf
122500021016     c                   Movel     dSdf          TbeUni
122600021016     c                   Update    Tntbe000
122700021016     c                   EndIf
122800000000     C                   ENDIF
122900000000      *
123000000000     C                   ENDSR
123100000000      *****************************************************************
123200000000      * Stampa del dettaglio con test per l'overflow
123300000000      *****************************************************************
123400000000     C     STAMPA        BEGSR
123500000000      *
123600970312    1C     DS1RIS        IFNE      *BLANKS
123700000000      *
123800000000      * Stampo solo se la riga ha dei valori (testo il nr.colli)
123900950929      *  o i colli C/S > 0
124000970312    2C     COLLIP        IFGT      *ZERO
124100970312     C     $X            OREQ      3
124200970312     C     COL(2)        ANDGT     0
124300000000      *
124400090911     c   30              if        not *in10
124500090911     C                   WRITE     LSC5T1
124600090911     C                   WRITE     LSC5T3
124700090911     C                   SETOFF                                       30
124800090911     c                   else
124900090911     C                   WRITE     LSC5T1n
125000090911     C                   WRITE     LSC5T3n
125100090911     C                   SETOFF                                       30
125200090911     c                   endif
125300090911
125400090911     c                   if        not *in10
125500090911     C                   WRITE     LSC5D1
125600090911     c                   else
125700090911     C                   WRITE     LSC5D1n
125800090911     c                   endif
125900090911     c
126000970312    2C                   END
126100000000      *
126200970312    2C     COLLIP        IFGT      *ZEROS
126300970312     C     $ALMEN        ANDEQ     *ZERO
126400970312     C     COL(2)        ORGT      *ZEROS
126500970312     C     $ALMEN        ANDEQ     *ZERO
126600000000     C                   Z-ADD     1             $ALMEN
126700970312    2C                   ENDIF
126800000000      *
126900970312    1C                   ENDIF
127000000000      *
127100000000     C                   ENDSR
127200000000      *****************************************************************
127300000000      * Scrittura su archivio statistiche
127400000000      *****************************************************************
127500000000     C     AGGIOR        BEGSR
127600000000      *
127700000000     C                   Z-ADD     1             $W
127800000000     C     $W            DOUGT     7
127900000000      *
128000000000     C                   Z-ADD     SIMFEL        SBOFLE
128100000000     C                   Z-ADD     $FILPR        SBOLNP
128200950213     C                   Z-ADD     DS1ADA        SBODRE
128300000000     C                   MOVE      *BLANK        SBOSOS
128400000000     C                   MOVE      *BLANK        SBOTRE
128500000000     C     $W            IFEQ      2
128600000000     C     $W            OREQ      7
128700000000     C                   MOVE      'C'           SBOTRE
128800000000     C                   ENDIF
128900000000     C     $W            IFGT      5
129000000000     C                   MOVE      'S'           SBOSOS
129100000000     C                   ENDIF
129200000000      *
129300000000     C                   Z-ADD     COL($W)       SBOCOL
129400000000     C                   Z-ADD     PF3($W)       SBOPF3
129500000000     C                   Z-ADD     PO3($W)       SBOPO3
129600000000     C                   Z-ADD     PTA($W)       SBOPTA
129700000000     C                   Z-ADD     VOL($W)       SBOVOL
129800000000     C                   Z-ADD     SEF($W)       SBOSEF
129900000000     C                   Z-ADD     SEA($W)       SBOSEA
130000090910     C                   Z-ADD     ShF($W)       SBOShF
130100090910     C                   Z-ADD     ShA($W)       SBOShA
130200000000     C                   Z-ADD     SNF($W)       SBOSNF
130300000000     C                   Z-ADD     SNA($W)       SBOSNA
130400000000     C                   Z-ADD     SSF($W)       SBOSSF
130500000000     C                   Z-ADD     SSA($W)       SBOSSA
130600970312     C     COL($W)       IFGT      *ZEROS
130700000000     C                   Z-ADD     SRF($W)       SBOSRF
130800000000     C                   Z-ADD     SRA($W)       SBOSRA
130900970312     C                   ELSE
131000970312     C                   MOVE      *ZEROS        SBOSRF
131100970312     C                   MOVE      *ZEROS        SBOSRA
131200970312     C                   END
131300000000      *
131400000000      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
131500040922      * riempo il "flag di trasmissione" (SBOFTR)
131600040922     c                   z-add     dateu         sbodtr
131700040922     C***                Z-ADD     *ZERO         SBODTR
131800040922     C***  §5CSBO        IFEQ      'S'
131900000000     C                   MOVE      *BLANK        SBOFTR
132000040922     C***                ELSE
132100040922     C***                MOVE      'T'           SBOFTR
132200040922     C***                ENDIF
132300000000      *
132400970312     C* memorizzo record con sbotre='F' (festivo) se numero colli = 0
132500970312     C* e giorno elaborato festivo
132600970312     C     WFES          IFEQ      'F'
132700970312     C     COL($W)       ANDEQ     0
132800970312     C                   MOVE      'F'           SBOTRE
132900970312     C                   END
133000970312     C*
133100970312     C     $W            IFEQ      1
133200970312     C     COL($W)       ORGT      *ZERO
133300090910     C                   WRITE     FiSBO000
133400990519     C                   MOVEL     'S'           WTRASM            1
133500000000     C                   ENDIF
133600000000      *
133700000000      * Solo se ho registrato con $X=1 (coie' rottura per filiale), registro
133800000000      * anche per c/servizio ($X=2), per filiale con spspensione ($X=5) e
133900000000      * per c/servizio con sospensione ($X=6)
134000000000     C     $W            IFEQ      2
134100000000     C                   Z-ADD     6             $W
134200000000     C                   ELSE
134300000000     C                   ADD       1             $W
134400000000     C                   ENDIF
134500000000      *
134600000000     C                   ENDDO
134700000000      *
134800000000     C                   ENDSR
134900000000      *****************************************************************
135000990507      * RISTAMPA: senza elaborazione, solo lettura da file statistiche (FNSBO)
135100000000      * Tutto il ciclo di ristampa viene eseguito 2 volte, prima per le bolle
135200000000      * normali quindi per quelle in sospensione
135300000000      *****************************************************************
135400000000     C     RISTAM        BEGSR
135500000000      *
135600050829     c                   setoff                                       3237
135700000000      * Verifico se richiesta una specifica area
135800950213     C     DS1FEL        COMP      *ZERO                                  32
135900050829     c
136000050829     c                   if        ds1fel=0 and simfel>0
136100050829     c                   eval      ds1fel=simfel
136200050829     c                   setoff                                       32
136300050829     c                   endif
136400050829     c
136500050829     c                   if        lin(1)>0 and lin(2)=0
136600050829     c                   eval      klin=lin(1)
136700050829     c                   seton                                        37
136800050829     c                   endif
136900000000      *
137000000000     C                   Z-ADD     *ZERO         $PRIM1            1 0
137100000000     C                   Z-ADD     *ZERO         $FILPR
137200000000     C                   Z-ADD     *ZERO         $FI2PR
137300000000      *
137400000000     C   43              MOVE      *BLANK        SBOSOS
137500000000     C  N43              MOVE      'S'           SBOSOS
137600950213     C                   Z-ADD     DS1FEL        SBOFLE
137700950213     C                   Z-ADD     DS1ADA        SBODRE
137800000000     C                   Z-ADD     *ZERO         SBOLNP
137900000000      *
138000090910     C  n37FNSBOK        SETLL     FiSBO01L
138100090910     C   37ksbo          SETLL     FiSBO01L
138200950929    1C                   DO        *HIVAL
138300050829     c                   if        *in37
138400090910     C     ksbo2         reade(N)  FiSBO01L                               33
138500050829     c                   else
138600090910     C  N32FLSB2K        READE(N)  FiSBO01L                               33
138700090910     C   32SBOSOS        READE(N)  FiSBO01L                               33
138800050829     c                   endif
138900000000      *
139000990507    2C  N33SBODRE        IFGE      DS1ADA
139100990507     C     SBODRE        ANDLE     DS1AAL
139200000000      *
139300000000      * Solo le filiali presenti nella schiera LIN
139400050829     c                   if        not *in37 and *in32
139500000000     C     SBOLNP        LOOKUP    LIN                                    99
139600050829     c                   else
139700050829     c                   seton                                        99
139800050829     c                   endif
139900050829     c
140000950929    3C     *IN99         IFEQ      *ON
140100000000      *
140200000000      * Al primo giro memorizzo il "precedente"
140300950929    4C     $PRIM1        IFEQ      *ZERO
140400000000     C                   Z-ADD     SBOFLE        $FI2PR
140500000000     C                   Z-ADD     SBOLNP        $FILPR
140600000000     C                   Z-ADD     1             $PRIM1
140700950929    4C                   ENDIF
140800000000      *
140900000000      * Se cambia filiale stampo i totali per filiale
141000950929    4C     SBOTRE        IFEQ      *BLANK
141100950929     C     SBOLNP        ANDNE     $FILPR
141200000000     C                   Z-ADD     1             $X
141300000000     C                   EXSR      TOTALE
141400000000     C                   Z-ADD     SBOLNP        $FILPR
141500950929    4C                   ENDIF
141600000000      *
141700000000      * Se cambia area stampo i totali per c/servizio
141800950929    4C     SBOFLE        IFNE      $FI2PR
141900000000     C                   Z-ADD     3             $X
142000000000     C                   EXSR      TOTALE
142100000000     C                   Z-ADD     4             $X
142200000000     C                   EXSR      TOTALE
142300000000     C                   Z-ADD     SBOFLE        $FI2PR
142400950929    4C                   ENDIF
142500000000      *
142600000000     C                   EXSR      ELABO2
142700000000      *
142800950929    3C                   ENDIF
142900000000      *
143000950929    2C                   ENDIF
143100950929    1C  N33              ENDDO
143200000000     C                   Z-ADD     1             $X
143300000000     C                   EXSR      TOTALE
143400000000      *
143500000000     C     ENDRIS        ENDSR
143600000000      *****************************************************************
143700990507      * Avendo letto il file FNSBO00F memorizzo i campi del record nelle schiere
143800000000      *****************************************************************
143900000000     C     ELABO2        BEGSR
144000000000      *
144100000000      * Definisco il valore gli indici di schiera
144200000000     C                   Z-ADD     1             $Z
144300000000     C     SBOTRE        IFEQ      'C'
144400000000     C                   Z-ADD     2             $Z
144500000000     C                   ENDIF
144600000000      *
144700000000     C                   ADD       SBOCOL        COL($Z)
144800000000     C                   ADD       SBOPF3        PF3($Z)
144900000000     C                   ADD       SBOPO3        PO3($Z)
145000000000     C                   ADD       SBOPTA        PTA($Z)
145100000000     C                   ADD       SBOVOL        VOL($Z)
145200000000     C                   ADD       SBOSEF        SEF($Z)
145300000000     C                   ADD       SBOSEA        SEA($Z)
145400090910     C                   ADD       SBOShF        ShF($Z)
145500090910     C                   ADD       SBOShA        ShA($Z)
145600000000     C                   ADD       SBOSNF        SNF($Z)
145700000000     C                   ADD       SBOSNA        SNA($Z)
145800000000     C                   ADD       SBOSSF        SSF($Z)
145900000000     C                   ADD       SBOSSA        SSA($Z)
146000000000     C                   ADD       SBOSRF        SRF($Z)
146100000000     C                   ADD       SBOSRA        SRA($Z)
146200000000      *
146300000000     C                   ENDSR
146400000000      *****************************************************************
146500000000      *                                4         5         6         7         8
146600950308** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
146700950308OVRPRTF FILE(FNLSC5P) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.boll.
146800950308')
