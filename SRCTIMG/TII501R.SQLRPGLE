000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200070316     h dftactgrp(*no) actgrp(*caller)
000300070316
000400110831      * TII501R *----------------------------------------------------*
000500110901      *     ?   - CARICA RIGHE ORDINE/articolo -  ?
000600070403      *--------------------------------------------------------------*
000700110831      * con la sottoclasse filiale tipo ordine
000800110831      * imposta le righe degli articoli caricabili
000900070227      *--------------------------------------------------------------*
001000110725      * Il n°d'ordine è univoco.
001100110725      *--------------------------------------------------------------*
001200940307      *  21           GENERICO OPERAZIONI I/O
001300940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001400070301      *  23           GENERICO Operazioni varie
001500940224      *  30           SFLDSP
001600940224      * N31           SFLCLR
001700940224      *  31           SFLDSPCTL
001800940224      *  32           SFLNXTCHG
001900940224      *  33           SFLEND
002000110908      *  35           pianificato Articolo SOSPESO
002100940224      *  39           OF PRTF
002200070302      *  40 <---> 49  DSPATR SU video
002300940608      *  Specificare l'uso dei singoli indicatori
002400070227      *  50           Emissione della window totale/parziale
002500070227      *  51 <---> 90  ERRORI SU VIDEO
002600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002700070829      *  98           ERRORE SPECIALE :
002800940224      *  99           INDIC. GENERALE DI ERRORE
002900940128     F*----------------------------------------------------*
003000070404      *  Output
003100110906     FTMORP02L  uF a E           K DISK    infds(orpds) usropn
003200080609      *
003300070404      *  Input
003400110826     FanTAB01l  if   e           k disk
003500110901     FtMAMP10l  if   e           k disk
003600110906     FTMOPP01l  if   e           k disk
003700110901      *
003800110901     Ftii501d   CF   E             WORKSTN
003900110725     F                                     SFILE(TII5S01:S1NRR)
004000940201     F                                     INFDS(DSFMT)
004100070305     D*----------------------------------------------------*
004200070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
004300070326     D*----------------------------------------------------*
004400940211     D* Passaggio Parametri
004500940211     D KPJBA         E DS
004600070227     D KPJBUs          s                   like(KPJBU)
004700110726     D*-------------
004800110826     d antab155      e ds
004900110826     d antab156      e ds
005000110901     d tii501ds      E ds
005100110824     D*-------------
005200940211     D DSFMT           DS           512
005300940506     D  $TASTO               369    369
005400940211     D  NRG                  370    370
005500940211     D  NCL                  371    371
005600940211     D  SFLNRR               378    379B 0
005700940127     D*-------------
005800940127     D* Reperimento nome PGM
005900940127     D STATUS         SDS           333
006000940127     D  DSPGM            *PROC
006100030113     D*-------------
006200110725     d orpds           ds
006300110725     d  orp_nrr              397    400b 0
006400070320     D*-------------
006500070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
006600110831     D S1NRR           S                   Like(C1rcd)
006700110831     D S1pag           S                   Like(C1rcd)
006800110831     D WSfl            S                   Like(C1nrr)
006900110831     D Wmax            S                   Like(C1rcd)
007000110831     D Wpag            S                   Like(C1rcd)
007100110831     D sav_WSfl        S                   Like(WSfl)
007200110831     D sav_Wmax        S                   Like(Wmax)
007300110831     D sav_Wpag        S                   Like(Wpag)
007400110831     D Wpagine         S                   Like(C1rcd)
007500111012     d sav_in11        s              1    INZ('0')
007600110824      *--------------------------
007700110826     D RigPag_SFl1     S              3  0 INZ(18)
007800070227      *--------------------------
007900110831     D sav_S1NRR       S                   Like(s1nrr)
008000101129      *--------------------------
008100110726     d dataiso         s               d   datfmt(*iso)
008200110825     d oggiiso         s               d   datfmt(*iso)
008300101129     d dataeur         s               d   datfmt(*eur)
008400070305      *--------------------------
008500070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008600070305     D WLBDAT          DS                  INZ
008700070305     D  G02DAT                 1      8  0
008800070227     D  G02INV                 9     16  0
008900070227     D  G02ERR                17     17
009000070227     D  G02TGI                18     22  0
009100070307      *
009200070307     D DATPAR          DS
009300070307     D  GIODAT                 1      8  0
009400070307     D  GIOINV                 9     16  0
009500070307     D  GIOTGI                17     21  0
009600070307      *
009700070307     d Wdata8          DS
009800070307     d  dadata                 1      8  0
009900070307     d  adata                  9     16  0
010000070307      *--------------------------------
010100070227     d AZUTEds       e ds                  extname(AZUTE00F)
010200070227     d DDatiUte      e ds
010300110725      *--------------------------
010400070320     d* Ds per dati organigramma
010500070404      *------
010600100505     d DTAISO          s               D
010700070227      *--------------------------
010800070320      *?  COSTANTI                                      ?
010900070320      *--------------------------
011000940506     D* Tasti di funzione
011100940506     D F01             C                   CONST(X'31')
011200940506     D F02             C                   CONST(X'32')
011300940506     D F03             C                   CONST(X'33')
011400940506     D F04             C                   CONST(X'34')
011500940506     D F05             C                   CONST(X'35')
011600940506     D F06             C                   CONST(X'36')
011700940506     D F07             C                   CONST(X'37')
011800940506     D F08             C                   CONST(X'38')
011900940506     D F09             C                   CONST(X'39')
012000940506     D F10             C                   CONST(X'3A')
012100940506     D F11             C                   CONST(X'3B')
012200070315     D F12             C                   CONST(X'3C')
012300940506     D F13             C                   CONST(X'B1')
012400940506     D F14             C                   CONST(X'B2')
012500940506     D F15             C                   CONST(X'B3')
012600940506     D F16             C                   CONST(X'B4')
012700940506     D F17             C                   CONST(X'B5')
012800940506     D F18             C                   CONST(X'B6')
012900940506     D F19             C                   CONST(X'B7')
013000940506     D F20             C                   CONST(X'B8')
013100940506     D F21             C                   CONST(X'B9')
013200940506     D F22             C                   CONST(X'BA')
013300940506     D F23             C                   CONST(X'BB')
013400940506     D F24             C                   CONST(X'BC')
013500940506     D ENTER           C                   CONST(X'F1')
013600940506     D ROLDWN          C                   CONST(X'F4')
013700940506     D ROLLUP          C                   CONST(X'F5')
013800070301     d digits          c                   '0123456789'
013900110831     d Pianificato     c                   'P'
014000110831     d aConsumo        c                   'C'
014100110831     d Classe_155      c                   '155'
014200110831     d SottoClas_156   c                   '156'
014300110831     d Acquisti        c                   'A'
014400110831     d Marketing       c                   'M'
014500111011      **********************************************************************
014600111011      * Constants
014700111011      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
014800111011      **********************************************************************
014900111011     D MaxKey          C                    2
015000111011$xxx D Descrizione     C                   '1'
015100111011$xxx D Codice          C                   '2'
015200111011     D Ascendente      C                   1
015300111011     D Discendente     C                   2
015400111011     D Carattere       C                   6
015500111011     D Put             C                   1
015600111011     D EndPut          C                   2
015700111011     D Get             C                   3
015800111011     D Numerico        C                   8
015900111011      **********************************************************************
016000111011      * Campi utili:
016100111011      *     RRN1       - Numero relativo di record del Subfile
016200111011      *     SizeList   - Dimensione della lista
016300111011      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
016400111011      **********************************************************************
016500111011     D Rrn1            S              5I 0
016600111011     D NotUsed         S             16A
016700111011     D ReturnSize      S              9B 0
016800111011     D SizeList        S              9B 0
016900111011     D RrnLast         S              5I 0
017000111011     D WrkSort         S              1
017100111011      **********************************************************************
017200111011      * Data Structures
017300111011      *     SflRcd1    - L'intero record del SFL da passare x l'ordinamento
017400111011      *     QLGSCB     - The sort request block for the QLGSORT API
017500111011      *     QLGSCB00   - The sort request block for the QLGSRTIO API
017600111011      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
017700111011      *     QUSEC      - Error structure for the QLGSORT API
017800111011      **********************************************************************
017900111011     D SflRcd1         DS
018000111011     D  S1MAT
018100111011     D  S1DES
018200111011     D  S1UMA
018300111011     D  S1QTA
018400111011     D  S1SELNOT
018500111011     D  S1NOT
018600111011     D  S1SELSOS
018700111011     D  S1SOSPESO
018800111011      *
018900111011     D  h1NRR
019000111011     D  h1INDIS
019100111011     D  h1SOSP
019200111011     D  h1PROTECT
019300111011     D  H1NOT
019400111011     D  H1SELSOS
019500111011     D  h1NOTA1
019600111011     D  h1NOTA2
019700111011     D  h1NOTA3
019800111011     D  h1NOTA4
019900111011     D  H1IN34
020000111011     D  H1IN36
020100111011     D  H1OBBLIG
020200111011     D  H1LVM
020300111011     D  H1LRO
020400111011     D  H1QTAMAX
020500111011     D  Selected1                     1A
020600111011      * ?------------------------------------------------------------------ */
020700111011      * ?set caratteri Maiuscoli/Minuscoli
020800111011     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
020900111011     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
021000111011      * ?------------------------------------------------------------------ */
021100111011      /COPY QSYSINC/QRPGLESRC,QLGSORT
021200111011     D QLGKL                         16    DIM(MaxKey)
021300111011     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
021400111011     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
021500111011     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
021600111011     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
021700111011
021800111011      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
021900111011      /COPY QSYSINC/QRPGLESRC,QUSEC
022000940127     C*----------------------------------------------------*
022100070419      *?     MAIN LINE PROGRAM                             ?
022200940127     C*----------------------------------------------------*
022300940223     C* inizializzazione variabili
022400940223     C                   EXSR      INZVAR
022500940223     C*
022600940223     C     $FINE         DOWEQ     *OFF
022700110823     C*
022800110826     C* Elenco Articoli da selezionare
022900110826     C     $GEST         CASEQ     'S1'          GESS1                          Lista Articoli
023000110725     C*
023100110725     C* Finestra x errori
023200110725     C     $GEST         CASEQ     'W1'          GESW1                          finestra errori
023300110726      *                                                                         errori su bolle
023400110726     C* Aggiorna il DataBase
023500110726     C     $GEST         CASEQ     'DB'          GESDB                          Aggiorna FILE
023600110726     C*
023700940117     C                   END
023800940117     C                   END
023900110901     C*
024000110901     C* restituisce la DS
024100110901     c                   eval      kpjbu = Tii501ds
024200940325     C* fine programma
024300940325     C                   SETON                                            LR
024400110824     C************************************************************
024500110824      *?  Emissione Finestra Errori                              ?
024600110824     C************************************************************
024700110824     C     GESW1         BEGSR
024800110824     C*
024900110824     C                   eval      $FINE = dopo_in_$FINE
025000110824     C                   eval      $GEST = sav$GEST
025100110824     c*
025200110824     c                   exfmt     TII5WER
025300110823     c*
025400110824     c* emesso l'errore torna al video che lo aveva generato
025500110824     c*  oppure va fuori dal programma  in base a $FINE e $GEST
025600110824     c*
025700110826     C                   ENDSR
025800110826     C/EJECT
025900110826      *---------------------------------------------------------------*
026000110826      *  ?  GESTIONE SFL di immissione righe di ordine               ?
026100110826      *---------------------------------------------------------------*
026200110826     C     GESS1         BEGSR
026300110826     C*
026400110826     C* inizializzazione videata
026500110826     C     $INZS1        IFEQ      *ON
026600110826     C                   EXSR      INZS1
026700110826     C                   MOVE      *OFF          $INZS1
026800110826     C                   ENDIF
026900110826     C*
027000110826     C* Inizializza e imposta piede videata
027100110826     C                   EXSR      INZZ1
027200110826     C*
027300110826     C* emissione piede videata
027400110826      * ?            *-----------------------*
027500110826     C                   WRITE     TII5Z01
027600110826      * ?            *-----------------------*
027700110826     C*
027800110826     C     WMAX          IFEQ      0
027900110826     C* Non ci sono records
028000110826      * ?            *-----------------------*
028100110826     C                   WRITE     TII5V01
028200110826      * ?            *-----------------------*
028300110826     c                   setoff                                       30
028400110826     C                   Else
028500110826     c                   seton                                        30
028600110826     c                   eval       sav_WSfl =  WSfl
028700110826     c                   eval       sav_Wpag =  Wpag
028800110826      *
028900110826     C     Wsfl          IFgt      0
029000110826     C                   Z-ADD     wsfl          C1RCD
029100110826     C                   Else
029200110826     C     Wpag          IFgt      0
029300110826     C                   Z-ADD     wpag          C1RCD
029400110826     C                   EndIF
029500110826     C                   EndIF
029600110826     C                   ENDIF
029700110826      *
029800110826      *  Emissione SFL dei GIRI
029900110826      * ?            *-----------------------*
030000110826     C                   EXFMT     TII5C01
030100110826      * ?            *-----------------------*
030200110826     c                   setoff                                       99
030300110826     C                   movel     'S1'          sav$GEST
030400110826      *
030500110826     C     C1NRR         IFNE      0
030600110826     C                   Z-ADD     C1NRR         WSFL
030700110826     C                   ENDIF
030800110826     C                   Z-ADD     SFLNRR        C1RCD
030900110826     C* Selezioni
0310001108261    C                   SELECT
031100111011     C* F11=Cambia Ordinamento del SFL
031200111011     C                   WHEN      $TASTO   =   F11
031300111011     c*********          move      *on           $inzs1
031400111011     C                   EXSR      F11_ordina1
031500110826     C* F3=Fine
031600110826     C     $TASTO        WHENEQ    F03
031700110826     C                   EXSR      F03_exit
031800110826      *
031900110826     C* F12=Ritorno
032000110826     C     $TASTO        WHENEQ    F12
032100110901     C                   EXSR      F12_return
032200110826      *
0323001108261O   C                   OTHER
032400110826     C* CONTROLLO testata
032500110826     C                   EXSR      CTRC1
032600110826     C* CONTROLLO subfile
032700110826     C     *IN99         IFEQ      *OFF
032800110826     C                   EXSR      CTRS1
032900110826     C                   END
033000110826      *
033100110826      * ? Scrive   l'ordine
033200110826     C     *IN99         IFEQ      *OFF
033300110826     C     $TASTO        andeq     F06
033400110826     c     ErrMsgZ       andEQ     *blank
033500110902     C                   if          WMAX > 0
033600110826     C                   MOVE      *ON           $WRTDB
033700110826     C                   movel     'DB'          $GEST
033800110901     c                   eval      I501KEYRET = F06
033900110902     c                   elseIf      WMAX = 0
034000110902     c                   eval      I501KEYRET = F06
034100110902     C                   MOVE      *ON           $FINE
034200110902     c                   end
034300110902     c                   end
034400110826      *
0345001108261-   C                   ENDSL
034600110826     C*
034700110826     C                   ENDSR
034800110826     C/EJECT
034900110826      *---------------------------------------------------------------*
035000110826      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
035100110826      *---------------------------------------------------------------*
035200110826     C     INZZ1         BEGSR
035300110826     C*
035400110826     C* Messaggio o tasti funzionali
035500110826     c                   clear                   z1dmsg
035600110826     c                   clear                   ErrmsgZ          80
035700110826     C*
035800110826      * imposta eventuali errori
035900110826     c                   eval      z1dmsg = ErrmsgZ
036000110826     C*
036100110826     C                   ENDSR
036200110826     C/EJECT
036300110826      *---------------------------------------------------------------*
036400110826      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
036500110826      *---------------------------------------------------------------*
036600110826     C     INZS1         BEGSR
036700110826     C*
036800110826     C* reset indicatori DSPATR
036900110826     c                   exsr      INZ_indERR
037000110826     C* pulizia SFL
037100110826     C                   SETOFF                                         3031
037200110826     C                   WRITE     TII5C01
037300110826     C                   SETON                                          31
037400110926     C*
037500110926      * ? Se Ordine Periodico deve caricare da file precaricato
037600110926     c                   if        I501TIPORD = Pianificato
037700110926     c                   setON                                        35
037800110926     c                   end
037900110826     C*
038000110826     C* Righe x Pagina del 1° SFL
038100110826     c                   z-add     RigPag_SFl1   s1pag
038200110826      *
038300110826     C* CARICAMENTO SFL totale
038400110826     C                   Z-ADD     0             S1NRR
038500110826     C                   Z-ADD     1             C1RCD
038600110826     C                   Z-ADD     0             WMAX
038700110826     C                   Z-ADD     0             Wsfl
038800110826     C* campi titolo
038900110826     C                   MOVEL     dsPGM         c1CPGM
039000110826     C                   MOVEL     knsif         c1nsif
039100110826     C                   MOVEL     knmus         c1nmus
039200110826     C                   MOVEL     RSUT          c1crsu
039300110826     C                   z-add     settimana     c1week
039400110901     C                   z-add     i501numord    C1NOR
039500110901     C                   z-add     i501dataor    C1DOR
039600110901     C                   move      I501SCLASS    C1scla
039700111012     C                   eval      C1FIL  = I501FILIAL
039800110908      *
039900110908     C                   movel     SottoClas_156 tabGRU
040000110908     C                   move      *all'0'       tabCOD
040100110908     C                   move      I501SCLASS    tabCOD
040200110908     c     kanTAB        chain     anTAB01l
040300110908     c                   if             %Found(anTAB01l)
040400110908     c                   eval      Antab156 = tabDES
040500110908     c                   eval      c1sclD   = DES156
040600110908     c                   end
040700110826      **----------------
040800110922      **
040900110901      * x sottoclasse articolo
041000110902     C                   move      I501sCLASS    ampSCM
041100110902     C                   eval        ampMAT    = *blank
041200110901     C     KtmAMP        setll     tmAMP10L
041300110901      **
041400110826      **----------------
041500110826     C* Carico il SFL
041600110826     C                   EXSR      ROLS1
041700110826     C*
041800110826     C                   Z-ADD     1             WPAG
041900111011     C*
042000111011     ** Al primo F11 ordino per Autista
042100111011     C                   EVAL      RrnLast = S1Nrr
042200111012     c                   if        sav_in11 <>*On
042300111011     C                   EVAL      WrkSort = Codice
042400111011     c                   exsr      F11_ordina1
042500111011     c                   else
042600111011     C                   EVAL      WrkSort = Descrizione
042700111011     c                   exsr      F11_ordina1
042800111012     c                   setoff                                       11
042900111011     c                   end
043000111011     C*
043100110826     c                   eval       sav_Wmax =  Wmax
043200110826     C*
043300110826     C                   ENDSR
043400110826     C/EJECT
043500110826      *---------------------------------------------------------------*
043600110826      *? CARICAMENTO PAGINA LISTA                                    ?
043700110826      *---------------------------------------------------------------*
043800110826     C     ROLS1         BEGSR
043900110826     C*
044000110826     C                   SETOFF                                       32
044100110901     C                   Z-ADD     1             Y
044200110826     C                   Z-ADD     WMAX          S1NRR
044300110826     C*
044400110826     C* Caricamento del Sfl x Giro
044500110826     c                   Exsr      Write_S1
044600110826     C*
044700110826     C                   Z-ADD     S1NRR         WMAX                 30
044800110826     C*
044900110826     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
045000110826     C     S1NRR         DIV       S1pag         PAGINE            4 0
045100110826     C                   MVR                     RESTO             3 0
045200110826     C     PAGINE        MULT      S1pag         C1RCD
0453001108261    C     RESTO         IFGT      0
045400110826     C                   ADD       1             C1RCD
0455001108261E   C                   ELSE
045600110826     C                   SUB       S1pag         C1RCD
045700110826     C                   ADD       1             C1RCD
0458001108261-   C                   ENDIF
045900110826     C*
046000110826     C                   ENDSR
046100110826      *---------------------------------------------------------------*
046200110826      *?  Scrive x Ogni rottura di GIRO                              ?
046300110826      *---------------------------------------------------------------*
046400110826     C     Write_S1      BEGSR
046500110926     C*
046600110926      **  apre TMORP per controllare se lo aveva già caricato
046700110926     c                   open      TMorp02l
046800110826      *
046900110826      *  Precarica da Ordine Periodico
047000110826     c                   exsr      Legge_rec1
047100110826      *
047200110826      * Finchè il giro è lo stesso
0473001109011    C     $EFILE        DowEQ     *OFF
047400110826     C                   clear                   TII5S01
047500110826      *
047600110826      * Solo x ordini mensili
047700110826     c                   exsr      carica_campi1
047800110826      *---------
047900110826     C                   ADD       1             S1NRR
048000110826     C                   ADD       1             Y
048100110826     C                   WRITE     TII5S01
048200110826      *---------
048300110826     c                   exsr      Legge_rec1
048400110826     c                   EndDO
048500110909     C*
048600110909     C*  chiude il file servito per il controllo
048700110909     c                   close     TMorp02l
048800110909     C*
048900110826     C                   ENDSR
049000110826      *---------------------------------------------------------------*
049100110826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
049200110826      *---------------------------------------------------------------*
049300110826     C     LEGGE_REC1    BEGSR
049400110826     C*
049500110826     C*  Per EoF deve uscire
049600110826     C*  Per Record Ok deve uscire riportando i dati
049700110826     C                   MOVEL     *OFF          $EFILE
049800110826     C                   MOVEL     *OFF          $RCDOK
049900110826     C*
0500001108261    C     $EFILE        DOUEQ     *ON
050100110826     C     $RCDOK        OREQ      *ON
050200110826     C*
050300110922     C*  per sottoClasse legge tutti i relativi articoli
050400110902     C                   move      I501sCLASS    ampSCM
050500110902     C     ampSCM        reade     tmAMP10L
050600110826      * Fine File
050700110922     C                   IF        %Eof(tmamp10l)
050800110901      **
050900110826     C                   MOVEL     *ON           $EFILE
051000110826     C                   MOVE      $EFILE        *IN33
051100110901      **
051200110826     C                   Else
051300110901      **
051400110826      * ?Controlla se la Spedizione è da considerare
051500110826     C                   MOVEL     *ON           $RCDOK
051600110826     C                   exsr      Check_RECord1
051700110826     C                   END
051800110826     C*
0519001108261-   C                   ENDDO
052000110922      **
052100110826     C                   ENDSR
052200110826      *---------------------------------------------------------------*
052300110826      *?   CONTROLLO il record letto se va bene                      ?
052400110826      *---------------------------------------------------------------*
052500110826     C     Check_Record1 BEGSR
052600110826     C*
052700110926     C*  controlli sull'articolo
052800110926     C*      Classe Merceologica
052900110926     c                   clear                   obbligatorio      1
053000110926     c                   clear                   facoltativo       1
053100110926     c                   clear                   a_consumo         1
053200110926     C*
053300110926     C                   movel     Classe_155    tabGRU
053400110926     C                   move      *all'0'       tabCOD
053500110926     C                   move      ampCLM        tabCOD
053600110926     c     kanTAB        chain     anTAB01l
053700110926     c                   if             %Found(anTAB01l)
053800110926     c                   eval      Antab155 = tabDES
053900110926     c                   eval         obbligatorio = PE1155v
054000110926     c                   eval         facoltativo  = PE2155v
054100110926     c                   eval         a_consumo    = PE3155v
054200110926     c                   else
054300110926     C                   MOVEL     *OFF          $RCDOK
054400110926     c                   goto      end_check1
054500110926     c                   end
054600110926     C*
054700110926     C* SottoClasse Merceologica
054800110926     c                   clear                   ufficiocomp       1
054900110926     c                   clear                   Filiale1LV        1
055000110926     c                   clear                   Filiale2LV        1
055100110926     c                   clear                   FilialeLOG        1
055200110926      *
055300110926     C                   movel     SottoClas_156 tabGRU
055400110926     C                   move      *all'0'       tabCOD
055500110926     C                   move      ampSCM        tabCOD
055600110926     c     kanTAB        chain     anTAB01l
055700110926     c                   if             %Found(anTAB01l)
055800110926     c                   eval      Antab156 = tabDES
055900110926     c                   eval         ufficiocomp  = GRU156v
056000110926     c                   eval         Filiale1LV   = FI1156v
056100110926     c                   eval         Filiale2LV   = FI2156v
056200110926     c                   eval         FilialeLOG   = FI3156v
056300110926     c                   else
056400110926     C                   MOVEL     *OFF          $RCDOK
056500110926     c                   goto      end_check1
056600110926     c                   end
056700110926     C*
056800110926     C*   Se l'ufficio competente o il livello non coincide
056900110926     c                   if        ufficiocomp <> I501uffCOM
057000110926     c                               or
057100110926     c                             I501livLPO = '1' and Filiale1LV <> *blank
057200110926     c                               or
057300110926     c                             I501livLPO = '2' and Filiale2LV <> *blank
057400110926     c                               or
057500110926     c                             I501livLPO = 'L' and FilialeLOG <> *blank
057600110926     C                   MOVEL     *OFF          $RCDOK
057700110926     c                   goto      end_check1
057800110926     c                   end
057900110926     C*
058000110926      **-------------
058100110926     C* Sprotegge campo quantità
058200110926     c                   setoFF                                           34
058300110926     C*
058400110926      * ? Se Ordine Periodico deve caricare da file precaricato
058500110926     c                   if        I501TIPORD = Pianificato
058600110926     C*
058700110926     C                   eval        OPPFIL    = I501FILIAL
058800110926     C                   eval        OPPUFF    = I501UFFCOM
058900110926      ** x FILIALE/UFFICIO Comp/Articolo
059000110926     C     KTMOPP        chain     TMOPP01L
059100111011     c                   if        not %Found(TMOPP01L)
059200111011     C                   clear                   OPPqta
059300111011     C                   clear                   OPPsosp
059400111013      * solo x Ordine Obbligatorio
059500111013     c                   if          Obbligatorio <> *blank
059600111013     C                   MOVEL     *OFF          $RCDOK
059700111013     c                   goto      end_check1
059800111013     c                   end
059900111011     c                   else
060000110926     c                   if        OPPQTA <> 0 and Obbligatorio <> *blank
060100110926      * Protegge la quantità
060200110926     c                   setON                                        34
060300110926     c                   end
060400110901     C*
060500110901     C*  se Scaduto o Sospeso ---- SALTA --------
060600111013     c                   if        OPPfinoAL < woggi8AMG or oppSOSP='A'
060700111013     c                             or oppqta = 0 and Obbligatorio <> *blank
060800110901     C                   MOVEL     *OFF          $RCDOK
060900110901     c                   goto      end_check1
061000110901     c                   end
061100110926     c                   end
061200110901     C*
061300110922     c                   end
061400110901     C*--------------
061500110901     C*   Finita Gestione dell'articolo
061600110901     c                   if         ampDFG > 0
061700110901     C                   MOVEL     *OFF          $RCDOK
061800110901     c                   goto      end_check1
061900110901     c                   end
062000110901     C*
062100110826     C     end_check1    ENDSR
062200110826      *---------------------------------------------------------------*
062300110826      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
062400110826      *---------------------------------------------------------------*
062500110826     C     Carica_campi1 BEGSR
062600110901     C*
062700111012     C*  se l'ordine è pianificato
062800111012     c                   if        I501TIPORD = Pianificato
062900111012     C*  articolo sospeso da sede
063000111012     c                   if        oppSOSP = 'A'
063100111012     c                   eval      h1sosp = 'A'
063200111012     c                   move(p)   '*Sosp'       s1sospeso
063300111012     c                   setoff                                       36
063400111012     c                   end
063500111012     c                   if        oppSOSP = 'S'
063600111012     c                   eval      s1selsos = 'S'
063700111012     c                   eval      h1selsos = 'S'
063800111012     c                   move(p)   'SOSP'        s1sospeso
063900111013     c                   setoff                                       36
064000111012     c                   end
064100111012     C*
064200111012     c                   z-add     OPPQTA        s1QTA
064300111013     C*
064400111012     c                   end
064500110908     C*
064600111012      ******     ragionare sul 36 acceso x obbligatorio con qtà a 0
064700111012      ******      per renderlo facoltativo
064800110901     C* se obbligatorio deve proteggere i campi selezionandolo
064900110901     C*  e accendendo il SFLNXTCHG
065000110908     c                   if        obbligatorio <> *blank
065100111013     c                   seton                                        323436
065200110908     c                   end
065300110909     C*----------
065400110909     C* SE ORDINE PIANIFICATO, CONTROLLA CHE NON SIA già stato inserito
065500110909     C*  in una precedente immissione nell'arco della settimana
065600110909     C*    e quindi imposta i dati registrati precedentemente
065700110909     C                   z-add     I501NUMORD    orpNOR
065800110909     C                   z-add     I501FILIAL    orpFIL
065900110909     C                   move      I501UFFCOM    orpUFF
066000111012     C                   move      ampMAT        orpMAT
066100111012      *
066200110909     c     kTMORP        chain(n)  TMORP02L
066300111013      *-
066400110926     c                   if        %Found(TMORP02l)
066500111013     c                   z-add     orpQTA        s1QTA
066600111013      *-
066700110926     c                   if           orpNOTE <> *blank
066800110909     c                   eval      s1not ='*'
066900110909     c                   move      '*'           h1not
067000110909     c                   eval      h1NOTA1 = %Subst(orpNOTE:01:60)
067100110909     c                   eval      h1NOTA2 = %Subst(orpNOTE:61:60)
067200110909     c                   eval      h1NOTA3 = %Subst(orpNOTE:121:60)
067300110909     c                   eval      h1NOTA4 = %Subst(orpNOTE:181:60)
067301111014     c                   end
067302111014      *-
067400111013     C*  articolo sospeso da sede
067500111013     c                   if        orpATT = 'A'
067600111013     c                   eval      h1sosp = 'A'
067700111013     c                   move(p)   '*Sosp'       s1sospeso
067800111013     c                   setoff                                       36
067900111013     c                   end
068000111013     c                   if        orpATT   = 'S'
068100111013     c                   eval      s1selsos = 'S'
068200111013     c                   eval      h1selsos = 'S'
068300111013     c                   move(p)   'SOSP'        s1sospeso
068400111013     c                   setoff                                       36
068500111013     c                   end
068600111013     C*
068800110909     c                   end
068900110909     C*
069000110826     C*  carica i campi e li protegge
069100110901     c                   eval      s1MAT =  ampMAT
069200110901     c                   eval      s1UMA =  ampUMA
069300110901     c                   eval      s1DES =  ampDMT
069400110901     c                   eval      h1LVM =  ampLVM
069500110901     c                   eval      h1LRO =  ampLRO
069600110908     c                   eval      h1qtaMAX = ampLEC
069700110901     C*
069800110926     C* Per quantità a zero anche se obbligatorio deve tenere sprotetto il campo
069900110926     c                   if          s1QTA = 0
070000111013     c                   setOFF                                       34
070100110926     c                   end
070200110926     C*
070300110926     c                   eval      h1PROTECT = *in34
070400110926     c                   eval      h1OBBLIG  = *in34
070500110926     c                   eval      H1in34 = *in34
070600110926     c                   eval      H1in36 = *in36
070700110909     C*
070800110826     C                   ENDSR
070900110826      *---------------------------------------------------------------*
071000110826      *?   CONTROLLO TESTATA LISTA                                   ?
071100110826      *---------------------------------------------------------------*
071200110826     C     CTRC1         BEGSR
071300110826     C*
071400110826     C                   MOVE      *OFF          *IN99
071500110826     C*
071600110826     C                   ENDSR
071700110826      *---------------------------------------------------------------*
071800110826      *?   CONTROLLO OPZIONI LISTA                                   ?
071900110826      *---------------------------------------------------------------*
072000110826     C     CTRS1         BEGSR
072100110826     C*
072200110826     C                   MOVEL     *OFF          $ESCI
072300110826     c                   clear                   ErrMsgZ
072400110826     C                   SETOFF                                       99
072500110826     C*
072600110826     C* Leggo il sfl solo se ci sono rcd
0727001108261    C     WMAX          IFGT      0
072800110826     C                   READC     TII5S01                                21
072900110826     C*
073000110826     C* esce se fine sfl o errore che richiede l'uscita
0731001108262    C     *IN21         DOWEQ     *OFF
073200110826     C     $ESCI         ANDEQ     *OFF
073300110826     C                   Z-ADD     S1NRR         C1RCD
073400110826     C* ctrl su riga
073500110826     C                   EXSR      RECS1
073600110826      *
073700110826     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0738001108263    C     *IN99         IFEQ      *ON
073900110826     C     $ESCI         OREQ      *ON
074000110826     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
074100110826     C* la ripristinerò a conclusione del ciclo di READC
074200110826     C                   MOVE      *OFF          $INZS1
0743001108263-   C                   ENDIF
074400110826     C*
074500110826      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
074501111014     c                   eval      H1in36 = *in36
074600110826     C                   MOVE      *ON           *IN32
074700110826     C                   UPDATE    TII5S01
074800110826     C*
074900110826     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0750001108263    C     $ESCI         IFEQ      *OFF
075100110826     C                   READC     TII5S01                                21
075200110826      *
075300110826     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0754001108264    C     *IN21         IFEQ      *ON
075500110826     C* calcolo pagina a cui deve posizionarsi
075600110826     c                   z-add     s1pag         Wpagine
075700110826     C                   EXSR      CLCPAG
0758001108264-   C                   ENDIF
0759001108263-   C                   ENDIF
076000110826     C*
0761001108262-   C                   ENDDO
076200110826     C*
0763001108261-   C                   ENDIF
076400110826     C* __________________________________________________
076500110826     C*  Controlli generali sulle scelte fatte
076600110909     c                   exsr      errori
076700110826     C* __________________________________________________
076800110826     C*
076900110826     C                   ENDSR
077000110826     C/EJECT
077100110826      *---------------------------------------------------------------*
077200110826      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
077300110826      *---------------------------------------------------------------*
077400110826     C     RECS1         BEGSR
077500110826     C*
077600110826     C* reset indicatori DSPATR
077700110826     c                   exsr      INZ_indERR
077800110909     C*
077900110909     c                   eval      *in36 = H1in36
078000110909     c                   eval      *in34 = H1in34
078100110826     C*
078200110909     C* Solo per ordini Pianificati
078300110909     c                   if        I501TIPORD = Pianificato
078400110909     C*
078500110912     C*  SOSPENSIONE ARTICOLO:
078600110912     C*  --------------------
078700110908     c                   clear                   s1sospeso
078800110908     C*
078900110908     c                   if        h1sosp <> *blank
079000111011     c                   move(p)   '*Sosp'       s1sospeso
079100110908     c                   else
079200110909     C*
079300110909     C*  se rilevata variazione sulla sospensione di qualche articolo
0794001109123    C     s1selSOS      ifEQ      'S'
079500111011     c                   move(p)   'SOSP'        s1sospeso
079600110912     c                   end
079700110912     C*
079800110909     c                   if          s1SELsos <> h1SELsos
079900110909     c                   eval         Err001 = errmsg(13)
080000110909     c                   eval         Err002 = errmsg(14)
080100110909     c                   eval         Err003 = *blank
080200110909     c                   eval      h1selsos = s1selSOS
080300110909     C                   MOVEL     *On           $ESCI
080400110909     c                   goTO      endSF1
080500110909     c                   end
080600110912     C*
080700110909     c                   end
080800110909     C*  --------------------
080900110909     c                   endif
081000110909     C* -----------
081100110901     c* se è un articolo obbligatorio
081200110826     C*  segnala l'errore
081300110901     c                   if        s1QTA = 0 and h1obblig = '1'
081400110909     c                   eval        err001 = 'Art.: ' + s1mat + ' qtà : 0 -
081500110909     c                               <--obbligatoria'
081600110909     c                   eval        err002 = ErrMsg(1)
081700110909     c                   setoN                                        99  57
081800110901     c                   goTO      endSF1
081900110901     c                   end
082000110908      **
082100110908      **  supera la quantità massima ordinabile
082200110908     c                   if        s1QTA > 0 and h1qtamax > 0 and
082300110908     c                             s1QTA > h1qtamax
082400110909     c                   eval        err001 = 'Art.: ' + s1mat + ' qtà : '
082500110909     c                             + s1UMA
082600110909     c                             + %editc(s1qta : '2')
082700110908     c                   eval        err002 = ErrMsg(2)
082800110909     c                   eval        err003 = '                    di : '
082900110909     c                             + s1UMA    + %editc(h1qtamax : '2')
083000110909     c                   setoN                                        99  57
083100110908     c                   goTO      endSF1
083200110908     c                   end
083300110901      ***
083400110901      ***   Imposta le Note
083500110908     c                   if        s1selNOT = 'N' and s1QTA = 0
083600110901     c                   eval        err002 = ErrMsg(1)
083700110909     c                   setoN                                        99
083800110901     c                   goTO      endSF1
083900110901     C*
084000110908     c                   elseif    s1selNOT = 'N'
084100110901     C*
084200110901     c                   eval      NOTMAT = s1MAT
084300110901     c                   eval      NOTDES = s1DES
084400110901     c                   eval      NOTQTA = s1qta
084500110901     c                   eval      NOT001 = h1nota1
084600110901     c                   eval      NOT002 = h1nota2
084700110901     c                   eval      NOT003 = h1nota3
084800110901     c                   eval      NOT004 = h1nota4
084900110901     C*
085000110901     c                   exfmt     tii5NOTE
085100110901     C*
085200110901     c                   eval               h1nota1 = NOT001
085300110901     c                   eval               h1nota2 = NOT002
085400110901     c                   eval               h1nota3 = NOT003
085500110901     c                   eval               h1nota4 = NOT004
085600110901     C*
085700110913     C                   eval      s1not = *blank
085800110913     C                   eval      s1selNOT = *blank
085900110901     c                   if        NOT001 <> *blank  or
086000110901     c                             NOT002 <> *blank  or
086100110901     c                             NOT003 <> *blank  or
086200110901     c                             NOT004 <> *blank
086300110901     C                   eval      s1not ='*'
086400110901     c                   end
086500110901     C*
086600110901     c                   endIF
086700110909     C*
086800110826     C     endSF1        ENDSR
086900070227     C/EJECT
087000110726      *---------------------------------------------------------------*
087100110726      *?  GESTIONE F03 x uscire LR                                   ?
087200110726      *---------------------------------------------------------------*
087300110726     C     F03_exit      BEGSR
087400110726     C* fine programma
087500110726     C                   MOVE      *ON           $FINE
087600110901     c                   eval      I501KEYRET = F03
087700110726      *
087800110726     C                   ENDSR
087900110726     C/EJECT
088000110901      *---------------------------------------------------------------*
088100110901      *?  GESTIONE F03 x uscire LR                                   ?
088200110901      *---------------------------------------------------------------*
088300110901     C     F12_return    BEGSR
088400110901     C* fine programma ritorno al video precedente
088500110901     C                   MOVE      *ON           $FINE
088600110901     c                   eval      I501KEYRET = F12
088700110901      *
088800110901     C                   ENDSR
088900110901     C/EJECT
089000070302      *---------------------------------------------------------------*
089100070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
089200070302      *---------------------------------------------------------------*
089300110901     C     GESDB         BEGSR
089400070306      **
089500110906     C                   open      TMORP02l
089600110902      **
089700110901     C                   z-add     0             S1NRR
089800110901      *  legge gli articoli selezionati
089900110901     c                   exsr      Legge_sfl1
0900001109011    C     $EFILE        DowEQ     *OFF
090100110901      * aggiorna il DATA BASE
090200110902     c                   if        s1qta > 0
090300110901     c                   exsr      carica_DataB
090400110902     c                   end
090500110902      *
090600110901     c                   exsr      Legge_sfl1
090700110901     c                   EndDO
090800070305     C*
090900110902     c                   eval      I501KEYRET = F06
090901111014     c                   if        I501TIPORD = Pianificato
090902111014     c                   eval      I501KEYRET = F12
090903111014     c                   end
091000110902     C                   MOVE      *ON           $FINE
091100110902     C*
091200110906     C                   close     TMORP02l
091300110902     C*
091400070302     C                   ENDSR
091500110901      *---------------------------------------------------------------*
091600110901      *  ?  GESTIONE data Base
091700110901      *---------------------------------------------------------------*
091800110901     C     Legge_Sfl1    BEGSR
091900110901     C*
092000110901     C                   ADD       1             S1NRR
092100110901     C     S1NRR         chain     tii5S01                            33
092200110901     C                   move      *in33         $EFILE
092300110901      **
092400110901     C                   ENDSR
092500110901      *---------------------------------------------------------------*
092600110901      *  ?  GESTIONE data Base
092700110901      *---------------------------------------------------------------*
092800110901     C     carica_DataB  BEGSR
092900110905     C*
093000110905     C                   z-add     I501NUMORD    orpNOR
093100110905     C                   z-add     I501FILIAL    orpFIL
093200110905     C                   move      I501UFFCOM    orpUFF
093300110905     C                   move      s1mat         orpMAT
093400110906     c     kTMORP        chain     TMORP02L
093500110901     C*
093600110906     c                   if        %Found(TMORP02l)
093700110908     c                   if        h1sosp ='A'
093800110908     c                   eval      orpATT = 'A'
093900110908     c                   else
094000110908     c                   if        s1selsos ='S'
094100110908     c                   eval      orpATT = 'S'
094200110909     c                   else
094300110909     c                   eval      orpATT = ' '
094400110909     c                   end
094500110908     c                   end
094600111013     c                   eval          ORPNOTE  = h1nota1 + h1nota2 +
094700111013     c                                            h1nota3 + h1nota4
094800110905     c                   eval          ORPQTA   = s1qta
094900110906     c                   update    TMORP000
095000110905     C*
095100110905     c                   else
095200110906     c                   clear                   TMORP000
095300110901     c                   eval          ORPUTE   = I501CODUTE
095400110901     c                   eval          ORPFIL   = I501FILIAL
095500110901     c                   eval          ORPUFF   = I501UFFCOM
095600110901     c                   eval          ORPNOR   = I501NUMORD
095700110902     C*
095800110902     C                   Z-ADD     I501DATAOR    G02DAT                         *AAAA/MM/GG
095900110902     C                   MOVEL     ' '           G02ERR
096000110902     C                   CALL      'XSRDA8'
096100110902     C                   PARM                    WLBDAT
096200110902     c                   Z-ADD     G02INV        ORPDOR
096300110902     C*
096400110909     c                   eval          ORPTOR   = *blank
096500110901     c                   eval          ORPQTA   = s1qta
096600110901     c                   eval          ORPMAT   = s1mat
096700110901     c                   move      I501WEEKAA    ORPANNO
096800110901     c                   movel     I501WEEKAA    ORPWEEK
096900110901     c                   eval          ORPPIAN  = I501TIPORD
097000110901     c                   eval          ORPCONF  = *blank
097100110901     c                   eval          ORPNOTE  = h1nota1 + h1nota2 +
097200110901     c                                            h1nota3 + h1nota4
097300110908     c                   if        h1sosp ='A'
097400110908     c                   eval      orpATT = 'A'
097500110908     c                   else
097600110908     c                   if        s1selsos ='S'
097700110908     c                   eval      orpATT = 'S'
097800110908     c                   end
097900110908     c                   end
098000110906     c                   write     TMORP000
098100110905     c                   end
098200110901     C*
098300110901     c                   eval      I501WRIUNO = 'S'
098400110901     C*
098500110901     C                   ENDSR
098600110825      *---------------------------------------------------------------*
098700110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
098800110825      *---------------------------------------------------------------*
098900110825     C     CLCPAG        BEGSR
099000110825     C* Input :
099100110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
099200110825     C*          cursore
099300110825     C* - Wpagine= numero rcd per pagina sfl
099400110825     C* Output :
099500110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
099600110825     C*
099700110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
099800110825     C                   MVR                     RESTO             3 0
099900110825     C     RESTO         IFGT      0
100000110825     C                   ADD       1             PAGINE
100100110825     C                   ENDIF
100200110825     C     PAGINE        MULT      Wpagine       WPAG
100300110825     C*
100400110825     C                   ENDSR
100500110825     C************************************************************
100600110825      *?  INIZIALIZZA indicatori di errore                           ?
100700110825     C************************************************************
100800110825     C     INZ_indERR    BEGSR
100900110825      *
101000110825     C                   move      *ALL'0'       IN5190           40
101100110825     C                   moveA     IN5190        *IN(51)
101200110825      *
101300110825     C                   ENDSR
101400070301     C/EJECT
101500110831     C************************************************************
101600070316      *  ?  OPERAZIONI INIZIALI      ?
101700110831     C************************************************************
101800940131     C     *INZSR        BEGSR
101900110726     C*
102000110901     C*    chiave per il ANTAB
102100110901     c     kanTAB        klist
102200110901     C                   KFLD                    tabGRU
102300110901     C                   KFLD                    tabCOD
102400110824     C*
102500110906     c     kTMOPP        klist
102600110906     C                   KFLD                    OPPFIL
102700110906     C                   KFLD                    OPPUFF
102800110922     C                   KFLD                    ampMAT
102900110901     C*
103000110901     c     ktmAMP        klist
103100110901     C                   KFLD                    ampSCM
103200110901     C                   KFLD                    ampMAT
103300110901     C*
103400110906     c     kTMORP        klist
103500110905     C                   KFLD                    orpNOR
103600110905     C                   KFLD                    orpFIL
103700110905     C                   KFLD                    orpUFF
103800110905     C                   KFLD                    orpMAT
103900110824     C*
104000030113     C* Reperimento parametri
104100030113     C     *ENTRY        PLIST
104200030113     C                   PARM                    KPJBA
104300110831     C                   EVAL      TII501DS = KPJBU
104400110726      *  Se è EDP
104500070308     c                   CLEAR                   se_EDP            1
104600070308     c                   if        %subst(Knmus:1:3) = 'EDP'
104700070308     c                   MOVE      'S'           se_EDP
104800070307     c                   end
104900070227
105000030113     C* Variabili per gestione videate
105100030113     C                   MOVE      *BLANK        $GEST             2
105200070305     C                   MOVE      *BLANK        sav$GEST          2
105300030113     C                   MOVE      *BLANK        $FINE             1
105400110824     C                   MOVE      *BLANK        dopo_in_$FINE     1
105500070227     C                   MOVE      *BLANK        $INZW1            1
105600070227     C                   MOVE      *BLANK        $INZS1            1
105700110726     C                   MOVE      *BLANK        $WRTdb            1
105800030113     C                   MOVE      *BLANK        $EFILE            1
105900030113     C                   MOVE      *BLANK        $ESCI             1
106000030113     C                   MOVE      *BLANK        $RCDOK            1
106100030113     C* Indici
106200030113     C                   Z-ADD     0             X                 3 0
106300030113     C                   Z-ADD     0             Y                 3 0
106400070227     C***
106500070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
106600070227     C                   TIME                    W0140            14 0
106700110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
106800110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
106900070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
107000070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
107100070227     C*
107200110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
107300070227     C                   MOVEL     *BLANK        G02ERR
107400070227     C                   CALL      'XSRDA8'
107500070227     C                   PARM                    WLBDAT
107600110901     c                   Z-ADD     G02INV        Woggi8AMG         8 0          *AAAA/MM/GG
107700110901     c                   eval      dataiso = %date(Woggi8AMG:*iso)
107800110824     c                   eval      dataeur = dataiso
107900110825     c                   eval      oggiiso = dataiso
108000110831     C                   MOVEL     I501WEEKAA    settimana         2 0
108100100505     C*
108200940117     C                   ENDSR
108300110831     C/EJECT
108400110831     C************************************************************
108500110831      *?  INIZIALIZZAZIONE VARIABILI                       ?
108600110831     C************************************************************
108700110831     C     INZVAR        BEGSR
108800110831     C*
108900110831     C* Variabili per gestione videate
109000110831     C                   MOVE      *OFF          $FINE
109100110831     C                   MOVE      *OFF          dopo_in_$FINE
109200110831     C                   MOVE      *OFF          $wrtDB
109300110831     C                   MOVE      *OFF          $INZW1
109400110831     C                   MOVE      *OFF          $EFILE
109500110831     C                   MOVE      *OFF          $ESCI
109600110831     C                   MOVE      *OFF          $RCDOK
109700110831     C* Prima di tutto Controlla Autorizzazioni ad Accedere al PGM
109800110901     C                   MOVE      *ON           $INZS1
109900110831     C                   MOVE      'S1'          $GEST
110000111011     c                   move      *blank        sav_in11
110100110831     C* Variabili appoggio
110200110831     C                   Z-ADD     1             WPAG
110300110831     c*
110400110831     C                   ENDSR
110500110825     C/EJECT
110600110726      * ?-------------------------------------------------------------*?
110700110826     C*   Routine degli errori da visualizzare sui SFL
110800110826      * ?-------------------------------------------------------------*?
110900110826     C     Errori        BEGSR
111000110826     C*
111100110901     c                   if        ErrMsgZ = *blank
111200110826      *
111300110826     C*  Messaggio di avviso
111400110826     C*  Messaggio di avviso    SE deve emettere un messaggio
111500110826     C*  Messaggio di avviso
111600110826     C*
111700110826     c                   if        err001 <> *blank or
111800110826     c                             err002 <> *blank or
111900110826     c                             err003 <> *blank
112000110826     c                   seton                                        99
112100110826      * ?            *-----------------------*
112200110826     C                   exfmt     TII5WER
112300110826      * ?            *-----------------------*
112400110909     c                   clear                   err001
112500110909     c                   clear                   err002
112600110909     c                   clear                   err003
112700110826     c                   end
112800110826     C*
112900110826     c                   end
113000110826     C*
113100110826     c                   ENDSR
113200111011     C/EJECT
113300111011      *---------------------------------------------------------------*
113400111011      *? GESTIONE F11 alterna l'ordinamento x codice o Descrizione   ?
113500111011      *---------------------------------------------------------------*
113600111011     C     F11_ordina1   BEGSR
113700111011     C*
113800111011      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
113900111011      * presente nel subfile -- Selected -- questo è aggiunto al record.
114000111011      * Il campo è usato per selezionare i records dando un ordine a quelli
114100111011      * selezionati davanti ai non selezionati.
114200111011     C                   CLEAR                   QLGSCB
114300111011     C                   CLEAR                   QLGSCB00
114400111011
114500111011     c                   eval      wsfl = 1
114600111011
114700111011     ** Gestione della scelta ordinamento.
114800111011     C                   SELECT
114900111011      *
115000111011      *?  Ordinamento per Autista.  ?
115100111011     C                   WHEN      WrkSort = Codice
115200111011     ** 1 campo chiave.
115300111011     C                   EVAL      QLGNBRK    = 1
115400111011     C                   EVAL      QLGSP      = 1
115500111011     C                   EVAL      QLGSS      = %SIZE(S1MAT)
115600111011     C                   EVAL      QLGDT      = Carattere
115700111011     C                   EVAL      QLGSO      = Ascendente
115800111011     C                   EVAL      QLGKL(1)   = QLGSKL
115900111011
116000111011     ** Al prossimo F11 ordino per
116100111011     C                   EVAL      WrkSort = Descrizione
116200111011
116300111011     C                   EVAL      *IN11 = *ON
116400111011     C                   EVAL      sav_in11 = *IN11
116500111011
116600111011      *?  Ordinamento per Descrizione  ?
116700111011     C                   WHEN      WrkSort = Descrizione
116800111011      *
116900111011     ** 1 campi chiave.  x Descrizione
117000111011     C                   EVAL      QLGNBRK    = 1
117100111011     C                   EVAL      QLGSP   = 1 + %SIZE(S1MAT)
117200111011     C                   EVAL      QLGSS    = %SIZE(S1DES)
117300111011     C                   EVAL      QLGDT    = Carattere
117400111011     C                   EVAL      QLGSO    = Ascendente
117500111011     C                   EVAL      QLGKL(1) = QLGSKL
117600111011     ** Al prossimo F11 ordino per
117700111011     C                   EVAL      WrkSort = Codice
117800111011
117900111011     C                   EVAL      *IN11 = *OFF
118000111011     C                   EVAL      sav_in11 = *IN11
118100111011
118200111011     ** Al prossimo F11 ordino per
118300111011     C                   EVAL      WrkSort = Codice
118400111011
118500111011     C                   ENDSL
118600111011
118700111011      * Load other sort parameters.
118800111011     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
118900111011     C                   EVAL      QLGRL     = %SIZE(SflRcd1) - 1
119000111011     C                   EVAL      QLGRT     = 8
119100111011     C                   EVAL      QLGOKL    = 80
119200111011     C                   EVAL      QLGLKE    = 16
119300111011     C                   EVAL      QLGLSS    = 290
119400111011
119500111011      * Initialize Sort I/O API fields.
119600111011     C                   EVAL      QLGRL00  = QLGRL
119700111011     C                   EVAL      QLGRC00  = 1
119800111011     C                   CLEAR                   QUSEI
119900111011     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
120000111011
120100111011      * First step - Initialize the sort routine.
120200111011     C                   CALL      'QLGSORT'
120300111011     C                   PARM                    QLGSCB
120400111011     C                   PARM                    NotUsed
120500111011     C                   PARM                    NotUsed
120600111011     C                   PARM                    SizeList
120700111011     C                   PARM                    ReturnSize
120800111011     C                   PARM                    QUSEC
120900111011
121000111011      * Next step - Write records to I/O routine.
121100111011     C                   EVAL      QLGRT00 = Put
121200111011
121300111011     C                   DO        RrnLast       S1nrr
121400111011
121500111011     C     S1nrr         CHAIN     TII5S01
121600111011
121700111011     ** Solo le righe con Selected = 'Y' sono riordinate,
121800111011     ** quindi per fare un ordinamento di tutte le righe
121900111011     ** metto 'Y' sempre.
122000111011     C                   EVAL      Selected1 = 'Y'
122100111011
122200111011     C                   CLEAR                   QUSEI
122300111011     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
122400111011      *
122500111011     C                   CALL      'QLGSRTIO'
122600111011     C                   PARM                    QLGSCB00
122700111011     C                   PARM                    SflRcd1
122800111011     C                   PARM                    NotUsed
122900111011     C                   PARM                    SizeList
123000111011     C                   PARM                    NotUsed
123100111011     C                   PARM                    QUSEC
123200111011
123300111011     C                   ENDDO
123400111011
123500111011      * Next step - Signal end of input, clear subfile for reload.
123600111011     C                   EVAL      QLGRT00 = EndPut
123700111011     C                   CLEAR                   QUSEI
123800111011     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
123900111011
124000111011     C                   CALL      'QLGSRTIO'
124100111011     C                   PARM                    QLGSCB00
124200111011     C                   PARM                    SflRcd1
124300111011     C                   PARM                    NotUsed
124400111011     C                   PARM                    SizeList
124500111011     C                   PARM                    NotUsed
124600111011     C                   PARM                    QUSEC
124700111011
124800111011     C* pulizia SFL
124900111011     C                   SETOFF                                         3031
125000111011     C                   WRITE     TII5C01
125100111011     C                   SETON                                          31
125200111011
125300111011      * Final step - Write the records back to the subfile.
125400111011     C                   EVAL      QLGRT00 = Get
125500111011
125600111011     C                   DO        RrnLast       s1nrr
125700111011     C                   CLEAR                   QUSEI
125800111011     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
125900111011     C                   CALL      'QLGSRTIO'
126000111011     C                   PARM                    QLGSCB00
126100111011     C                   PARM                    NotUsed
126200111011     C                   PARM                    SflRcd1
126300111011     C                   PARM                    QLGRL00
126400111011     C                   PARM                    NotUsed
126500111011     C                   PARM                    QUSEC
126600111013     C*
126700111013     c                   eval      *in36 = H1in36
126800111013     c                   eval      *in34 = H1in34
126900111011      *
127000111011     C                   WRITE     TII5S01
127100111011     C                   ENDDO
127200111011      *
127300111011     C     end_ordina1   ENDSR
127400111011     C/EJECT
127500110826      *---------------------------------------------------------------------------------
127600080609      *
127700070326** ErrMsg
127800110909L'Articolo non ha quantità o è obbligatorio.                                  01
127900110909L'Articolo ha superato la quantità massima ordinabile                         02
128000110909L'Articolo non ha superato la quantità minima di Riordino                     03
128100110831                                                                               04
128200110831                                                                               05
128300110831                                                                               06
128400110831                                                                               07
128500110831                                                                               08
128600110725                                                                               09
128700110725                                                                               10
128800110725                                                                               11
128900110725                                                                               12
129000111014Se si SOSPENDE un ARTICOLO occorre inserire le NOTE per                        13
129100111014   spiegare il motivo della sospensione nell'ordine.                           14
129200110725                                                                               15
129300110725                                                                               16
129400110725                                                                               17
129500110725                                                                               18
129600110726                                                                               19
129700110726                                                                               20
129800110726                                                                               21
129900110726                                                                               22
130000110726                                                                               23
130100110726                                                                               24
130200110726                                                                               25
130300110726                                                                               26
130400110726                                                                               27
130500110726                                                                               28
130600110726                                                                               29
130700110726                                                                               30
130800110726                                                                               31
130900110725                                                                               32
131000110726                                                                               33
131100110725                                                                               34
131200070829                                                                               35
131300070829                                                                               36
131400070829                                                                               37
131500070829                                                                               38
131600070829                                                                               39
131700110831                                                                               40
