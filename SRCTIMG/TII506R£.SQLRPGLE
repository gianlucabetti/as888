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
001901110908      *  35           pianificato Articolo SOSPESO
002000940224      *  39           OF PRTF
002100070302      *  40 <---> 49  DSPATR SU video
002200940608      *  Specificare l'uso dei singoli indicatori
002300070227      *  50           Emissione della window totale/parziale
002400070227      *  51 <---> 90  ERRORI SU VIDEO
002500940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002600070829      *  98           ERRORE SPECIALE :
002700940224      *  99           INDIC. GENERALE DI ERRORE
002800940128     F*----------------------------------------------------*
002900070404      *  Output
003000110906     FTMORP02L  uF a E           K DISK    infds(orpds) usropn
003100080609      *
003200070404      *  Input
003300110826     FanTAB01l  if   e           k disk
003400110901     FtMAMP10l  if   e           k disk
003500110906     FTMOPP01l  if   e           k disk
003600110901      *
003700110901     Ftii501d   CF   E             WORKSTN
003800110725     F                                     SFILE(TII5S01:S1NRR)
003900940201     F                                     INFDS(DSFMT)
004000070305     D*----------------------------------------------------*
004100070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
004200070326     D*----------------------------------------------------*
004300940211     D* Passaggio Parametri
004400940211     D KPJBA         E DS
004500070227     D KPJBUs          s                   like(KPJBU)
004600110726     D*-------------
004700110826     d antab155      e ds
004800110826     d antab156      e ds
004900110901     d tii501ds      E ds
005000110824     D*-------------
005100940211     D DSFMT           DS           512
005200940506     D  $TASTO               369    369
005300940211     D  NRG                  370    370
005400940211     D  NCL                  371    371
005500940211     D  SFLNRR               378    379B 0
005600940127     D*-------------
005700940127     D* Reperimento nome PGM
005800940127     D STATUS         SDS           333
005900940127     D  DSPGM            *PROC
006000030113     D*-------------
006100110725     d orpds           ds
006200110725     d  orp_nrr              397    400b 0
006300070320     D*-------------
006400070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
006500110831     D S1NRR           S                   Like(C1rcd)
006600110831     D S1pag           S                   Like(C1rcd)
006700110831     D WSfl            S                   Like(C1nrr)
006800110831     D Wmax            S                   Like(C1rcd)
006900110831     D Wpag            S                   Like(C1rcd)
007000110831     D sav_WSfl        S                   Like(WSfl)
007100110831     D sav_Wmax        S                   Like(Wmax)
007200110831     D sav_Wpag        S                   Like(Wpag)
007300110831     D Wpagine         S                   Like(C1rcd)
007400110824      *--------------------------
007500110826     D RigPag_SFl1     S              3  0 INZ(18)
007600070227      *--------------------------
007700110831     D sav_S1NRR       S                   Like(s1nrr)
007800101129      *--------------------------
007900110726     d dataiso         s               d   datfmt(*iso)
008000110825     d oggiiso         s               d   datfmt(*iso)
008100101129     d dataeur         s               d   datfmt(*eur)
008200070305      *--------------------------
008300070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008400070305     D WLBDAT          DS                  INZ
008500070305     D  G02DAT                 1      8  0
008600070227     D  G02INV                 9     16  0
008700070227     D  G02ERR                17     17
008800070227     D  G02TGI                18     22  0
008900070307      *
009000070307     D DATPAR          DS
009100070307     D  GIODAT                 1      8  0
009200070307     D  GIOINV                 9     16  0
009300070307     D  GIOTGI                17     21  0
009400070307      *
009500070307     d Wdata8          DS
009600070307     d  dadata                 1      8  0
009700070307     d  adata                  9     16  0
009800070307      *--------------------------------
009900070227     d AZUTEds       e ds                  extname(AZUTE00F)
010000070227     d DDatiUte      e ds
010100110725      *--------------------------
010200070320     d* Ds per dati organigramma
010300070404      *------
010400100505     d DTAISO          s               D
010500070227      *--------------------------
010600070320      *?  COSTANTI                                      ?
010700070320      *--------------------------
010800940506     D* Tasti di funzione
010900940506     D F01             C                   CONST(X'31')
011000940506     D F02             C                   CONST(X'32')
011100940506     D F03             C                   CONST(X'33')
011200940506     D F04             C                   CONST(X'34')
011300940506     D F05             C                   CONST(X'35')
011400940506     D F06             C                   CONST(X'36')
011500940506     D F07             C                   CONST(X'37')
011600940506     D F08             C                   CONST(X'38')
011700940506     D F09             C                   CONST(X'39')
011800940506     D F10             C                   CONST(X'3A')
011900940506     D F11             C                   CONST(X'3B')
012000070315     D F12             C                   CONST(X'3C')
012100940506     D F13             C                   CONST(X'B1')
012200940506     D F14             C                   CONST(X'B2')
012300940506     D F15             C                   CONST(X'B3')
012400940506     D F16             C                   CONST(X'B4')
012500940506     D F17             C                   CONST(X'B5')
012600940506     D F18             C                   CONST(X'B6')
012700940506     D F19             C                   CONST(X'B7')
012800940506     D F20             C                   CONST(X'B8')
012900940506     D F21             C                   CONST(X'B9')
013000940506     D F22             C                   CONST(X'BA')
013100940506     D F23             C                   CONST(X'BB')
013200940506     D F24             C                   CONST(X'BC')
013300940506     D ENTER           C                   CONST(X'F1')
013400940506     D ROLDWN          C                   CONST(X'F4')
013500940506     D ROLLUP          C                   CONST(X'F5')
013600070301     d digits          c                   '0123456789'
013700110831     d Pianificato     c                   'P'
013800110831     d aConsumo        c                   'C'
013900110831     d Classe_155      c                   '155'
014000110831     d SottoClas_156   c                   '156'
014100110831     d Acquisti        c                   'A'
014200110831     d Marketing       c                   'M'
014300940127     C*----------------------------------------------------*
014400070419      *?     MAIN LINE PROGRAM                             ?
014500940127     C*----------------------------------------------------*
014600110902     C*
014700940223     C* inizializzazione variabili
014800940223     C                   EXSR      INZVAR
014900940223     C*
015000940223     C     $FINE         DOWEQ     *OFF
015100110823     C*
015200110826     C* Elenco Articoli da selezionare
015300110826     C     $GEST         CASEQ     'S1'          GESS1                          Lista Articoli
015400110725     C*
015500110725     C* Finestra x errori
015600110725     C     $GEST         CASEQ     'W1'          GESW1                          finestra errori
015700110726      *                                                                         errori su bolle
015800110726     C* Aggiorna il DataBase
015900110726     C     $GEST         CASEQ     'DB'          GESDB                          Aggiorna FILE
016000110726     C*
016100940117     C                   END
016200940117     C                   END
016300110901     C*
016400110901     C* restituisce la DS
016500110901     c                   eval      kpjbu = Tii501ds
016600940325     C* fine programma
016700940325     C                   SETON                                            LR
016800110824     C************************************************************
016900110824      *?  Emissione Finestra Errori                              ?
017000110824     C************************************************************
017100110824     C     GESW1         BEGSR
017200110824     C*
017300110824     C                   eval      $FINE = dopo_in_$FINE
017400110824     C                   eval      $GEST = sav$GEST
017500110824     c*
017600110824     c                   exfmt     TII5WER
017700110823     c*
017800110824     c* emesso l'errore torna al video che lo aveva generato
017900110824     c*  oppure va fuori dal programma  in base a $FINE e $GEST
018000110824     c*
018100110826     C                   ENDSR
018200110826     C/EJECT
018300110826      *---------------------------------------------------------------*
018400110826      *  ?  GESTIONE SFL di immissione righe di ordine               ?
018500110826      *---------------------------------------------------------------*
018600110826     C     GESS1         BEGSR
018700110826     C*
018800110826     C* inizializzazione videata
018900110826     C     $INZS1        IFEQ      *ON
019000110826     C                   EXSR      INZS1
019100110826     C                   MOVE      *OFF          $INZS1
019200110826     C                   ENDIF
019300110826     C*
019400110826     C* Inizializza e imposta piede videata
019500110826     C                   EXSR      INZZ1
019600110826     C*
019700110826     C* emissione piede videata
019800110826      * ?            *-----------------------*
019900110826     C                   WRITE     TII5Z01
020000110826      * ?            *-----------------------*
020100110826     C*
020200110826     C     WMAX          IFEQ      0
020300110826     C* Non ci sono records
020400110826      * ?            *-----------------------*
020500110826     C                   WRITE     TII5V01
020600110826      * ?            *-----------------------*
020700110826     c                   setoff                                       30
020800110826     C                   Else
020900110826     c                   seton                                        30
021000110826     c                   eval       sav_WSfl =  WSfl
021100110826     c                   eval       sav_Wpag =  Wpag
021200110826      *
021300110826     C     Wsfl          IFgt      0
021400110826     C                   Z-ADD     wsfl          C1RCD
021500110826     C                   Else
021600110826     C     Wpag          IFgt      0
021700110826     C                   Z-ADD     wpag          C1RCD
021800110826     C                   EndIF
021900110826     C                   EndIF
022000110826     C                   ENDIF
022100110826      *
022200110826      *  Emissione SFL dei GIRI
022300110826      * ?            *-----------------------*
022400110826     C                   EXFMT     TII5C01
022500110826      * ?            *-----------------------*
022600110826     c                   setoff                                       99
022700110826     C                   movel     'S1'          sav$GEST
022800110826      *
022900110826     C     C1NRR         IFNE      0
023000110826     C                   Z-ADD     C1NRR         WSFL
023100110826     C                   ENDIF
023200110826     C                   Z-ADD     SFLNRR        C1RCD
023300110826     C* Selezioni
0234001108261    C                   SELECT
023500110826     C* F3=Fine
023600110826     C     $TASTO        WHENEQ    F03
023700110826     C                   EXSR      F03_exit
023800110826      *
023900110826     C* F12=Ritorno
024000110826     C     $TASTO        WHENEQ    F12
024100110901     C                   EXSR      F12_return
024200110826      *
0243001108261O   C                   OTHER
024400110826     C* CONTROLLO testata
024500110826     C                   EXSR      CTRC1
024600110826     C* CONTROLLO subfile
024700110826     C     *IN99         IFEQ      *OFF
024800110826     C                   EXSR      CTRS1
024900110826     C                   END
025000110826      *
025100110826      * ? Scrive   l'ordine
025200110826     C     *IN99         IFEQ      *OFF
025300110826     C     $TASTO        andeq     F06
025400110826     c     ErrMsgZ       andEQ     *blank
025500110902     C                   if          WMAX > 0
025600110826     C                   MOVE      *ON           $WRTDB
025700110826     C                   movel     'DB'          $GEST
025800110901     c                   eval      I501KEYRET = F06
025900110902     c                   elseIf      WMAX = 0
026000110902     c                   eval      I501KEYRET = F06
026100110902     C                   MOVE      *ON           $FINE
026200110902     c                   end
026300110902     c                   end
026400110826      *
0265001108261-   C                   ENDSL
026600110826     C*
026700110826     C                   ENDSR
026800110826     C/EJECT
026900110826      *---------------------------------------------------------------*
027000110826      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
027100110826      *---------------------------------------------------------------*
027200110826     C     INZZ1         BEGSR
027300110826     C*
027400110826     C* Messaggio o tasti funzionali
027500110826     c                   clear                   z1dmsg
027600110826     c                   clear                   ErrmsgZ          80
027700110826     C*
027800110826      * imposta eventuali errori
027900110826     c                   eval      z1dmsg = ErrmsgZ
028000110826     C*
028100110826     C                   ENDSR
028200110826     C/EJECT
028300110826      *---------------------------------------------------------------*
028400110826      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
028500110826      *---------------------------------------------------------------*
028600110826     C     INZS1         BEGSR
028700110826     C*
028800110826     C* reset indicatori DSPATR
028900110826     c                   exsr      INZ_indERR
029000110826     C* pulizia SFL
029100110826     C                   SETOFF                                         3031
029200110826     C                   WRITE     TII5C01
029300110826     C                   SETON                                          31
029400110826     C*
029500110826     C* Righe x Pagina del 1° SFL
029600110826     c                   z-add     RigPag_SFl1   s1pag
029700110826      *
029800110826     C* CARICAMENTO SFL totale
029900110826     C                   Z-ADD     0             S1NRR
030000110826     C                   Z-ADD     1             C1RCD
030100110826     C                   Z-ADD     0             WMAX
030200110826     C                   Z-ADD     0             Wsfl
030300110826     C* campi titolo
030400110826     C                   MOVEL     dsPGM         c1CPGM
030500110826     C                   MOVEL     knsif         c1nsif
030600110826     C                   MOVEL     knmus         c1nmus
030700110826     C                   MOVEL     RSUT          c1crsu
030800110826     C                   z-add     settimana     c1week
030900110901     C                   z-add     i501numord    C1NOR
031000110901     C                   z-add     i501dataor    C1DOR
031100110901     C                   move      I501SCLASS    C1scla
031101110908      *
031102110908     C                   movel     SottoClas_156 tabGRU
031103110908     C                   move      *all'0'       tabCOD
031104110908     C                   move      I501SCLASS    tabCOD
031105110908     c     kanTAB        chain     anTAB01l
031106110908     c                   if             %Found(anTAB01l)
031107110908     c                   eval      Antab156 = tabDES
031108110908     c                   eval      c1sclD   = DES156
031109110908     c                   end
031200110826      **----------------
031300110826     C* Posizionamento su file pilota
031400110901      **** se richiesto il Pianificato x precaricamento righe
031500110901     c                   if        I501TIPORD = Pianificato
031600110901      ***   SETLL x FILIALE/UFFICIO Competente
031700110906     C                   eval        OPPFIL    = I501FILIAL
031800110906     C                   eval        OPPUFF    = I501UFFCOM
031900110906     C     KTMOPP        setll     TMOPP01L
031901110909      **
031902110909      **  apre TMORP per controllare di non essere in aggiunta di altri records
031903110909     c                   open      TMorp02l
032000110901      **
032100110901     c                   else
032200110901      * altrimenti
032300110901      * x sottoclasse articolo
032400110901      **
032500110902     C                   move      I501sCLASS    ampSCM
032600110902     C                   eval        ampMAT    = *blank
032700110901     C     KtmAMP        setll     tmAMP10L
032800110901      **
032900110901     c                   end
033000110826      **----------------
033100110826     C* Carico il SFL
033200110826     C                   EXSR      ROLS1
033300110826     C*
033400110826     C                   Z-ADD     1             WPAG
033500110826     c                   eval       sav_Wmax =  Wmax
033600110826     C*
033700110826     C                   ENDSR
033800110826     C/EJECT
033900110826      *---------------------------------------------------------------*
034000110826      *? CARICAMENTO PAGINA LISTA                                    ?
034100110826      *---------------------------------------------------------------*
034200110826     C     ROLS1         BEGSR
034300110826     C*
034400110826     C                   SETOFF                                       32
034500110901     C                   Z-ADD     1             Y
034600110826     C                   Z-ADD     WMAX          S1NRR
034700110826     C*
034800110826     C* Caricamento del Sfl x Giro
034900110826     c                   Exsr      Write_S1
035000110826     C*
035100110826     C                   Z-ADD     S1NRR         WMAX                 30
035200110826     C*
035300110826     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
035400110826     C     S1NRR         DIV       S1pag         PAGINE            4 0
035500110826     C                   MVR                     RESTO             3 0
035600110826     C     PAGINE        MULT      S1pag         C1RCD
0357001108261    C     RESTO         IFGT      0
035800110826     C                   ADD       1             C1RCD
0359001108261E   C                   ELSE
036000110826     C                   SUB       S1pag         C1RCD
036100110826     C                   ADD       1             C1RCD
0362001108261-   C                   ENDIF
036300110826     C*
036400110826     C                   ENDSR
036500110826      *---------------------------------------------------------------*
036600110826      *?  Scrive x Ogni rottura di GIRO                              ?
036700110826      *---------------------------------------------------------------*
036800110826     C     Write_S1      BEGSR
036900110826      *
037000110826      *  Precarica da Ordine Periodico
037100110826     c                   exsr      Legge_rec1
037200110826      *
037300110826      * Finchè il giro è lo stesso
0374001109011    C     $EFILE        DowEQ     *OFF
037500110826     C                   clear                   TII5S01
037600110826      *
037700110826      * Solo x ordini mensili
037800110826     c                   exsr      carica_campi1
037900110826      *---------
038000110826     C                   ADD       1             S1NRR
038100110826     C                   ADD       1             Y
038200110826     C                   WRITE     TII5S01
038300110826      *---------
038400110826     c                   exsr      Legge_rec1
038500110826     C*
038600110826     c                   EndDO
038601110909     C*
038700110909     C*  chiude il file servito per il controllo
038701110909     c                   if        I501TIPORD = Pianificato
038702110909     c                   close     TMorp02l
038703110909     c                   end
038704110909     C*
038800110826     C                   ENDSR
038900110826      *---------------------------------------------------------------*
039000110826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
039100110826      *---------------------------------------------------------------*
039200110826     C     LEGGE_REC1    BEGSR
039300110826     C*
039400110826     C*  Per EoF deve uscire
039500110826     C*  Per Record Ok deve uscire riportando i dati
039600110826     C                   MOVEL     *OFF          $EFILE
039700110826     C                   MOVEL     *OFF          $RCDOK
039800110826     C*
0399001108261    C     $EFILE        DOUEQ     *ON
040000110826     C     $RCDOK        OREQ      *ON
040100110826     C*
040200110826      * ? Se Ordine Periodico deve caricare da file precaricato
040300110826     c****   LETTURA RECORD x Filiale
040400110901     c                   if        I501TIPORD = Pianificato
040401110908     c                   setON                                        35
040500110906     C     KTMOPP        reade     TMOPP01L
040504110909      **
040600110901     c                   else
040700110902     C                   move      I501sCLASS    ampSCM
040800110902     C     ampSCM        reade     tmAMP10L
040900110901     c                   end
041000110826      * Fine File
041100110906     C                   IF        %Eof(TMOPP01l) and I501TIPORD =Pianificato
041200110901     c                               or
041300110901     C                             %Eof(tmamp10l) and I501TIPORD<>Pianificato
041400110901      **
041500110826     C                   MOVEL     *ON           $EFILE
041600110826     C                   MOVE      $EFILE        *IN33
041700110901      **
041800110826     C                   Else
041900110901      **
042000110826      * ?Controlla se la Spedizione è da considerare
042100110826     C                   MOVEL     *ON           $RCDOK
042200110826     C                   exsr      Check_RECord1
042300110826     C                   END
042400110826     C*
0425001108261-   C                   ENDDO
042600110826     C*
042700110826     C                   ENDSR
042800110826      *---------------------------------------------------------------*
042900110826      *?   CONTROLLO il record letto se va bene                      ?
043000110826      *---------------------------------------------------------------*
043100110826     C     Check_Record1 BEGSR
043200110826     C*
043300110901     C*--------------
043400110901     C* se Pianificato
043500110901     c                   if        I501TIPORD = Pianificato
043600110901     C*
043700110901     C*  se Scaduto o Sospeso ---- SALTA --------
043800110908     c                   if        OPPfinoAL < woggi8AMG
044000110901     C                   MOVEL     *OFF          $RCDOK
044100110901     c                   goto      end_check1
044200110901     c                   end
044300110901     C*
044400110901     C*  Aggancia l'articolo
044500110902     C                   move      I501sCLASS    ampSCM
044600110906     c                   eval      ampMAT = OPPMAT
044700110901     C     KtmAMP        chain     tmAMP10L
044800110901     c                   if        not %Found(tmAMP10L)
044900110901     C                   MOVEL     *OFF          $RCDOK
045000110901     c                   goto      end_check1
045100110901     c                   end
045200110901     C*
045300110901     c                   end
045400110901     C*--------------
045500110901     C*   Finita Gestione dell'articolo
045600110901     c                   if         ampDFG > 0
045700110901     C                   MOVEL     *OFF          $RCDOK
045800110901     c                   goto      end_check1
045900110901     c                   end
046000110901     C*
046100110901     C*  controlli sull'articolo
046200110901     C*      Classe Merceologica
046300110901     c                   clear                   obbligatorio      1
046400110901     c                   clear                   facoltativo       1
046500110901     c                   clear                   a_consumo         1
046600110901     C*
046700110901     C                   movel     Classe_155    tabGRU
046800110901     C                   move      *all'0'       tabCOD
046900110901     C                   move      ampCLM        tabCOD
047000110901     c     kanTAB        chain     anTAB01l
047100110901     c                   if             %Found(anTAB01l)
047200110901     c                   eval      Antab155 = tabDES
047300110901     c                   eval         obbligatorio = PE1155v
047400110901     c                   eval         facoltativo  = PE2155v
047500110901     c                   eval         a_consumo    = PE3155v
047600110902     c                   else
047700110902     C                   MOVEL     *OFF          $RCDOK
047800110902     c                   goto      end_check1
047900110902     c                   end
048000110826     C*
048100110901     C* SottoClasse Merceologica
048200110901     c                   clear                   ufficiocomp       1
048300110901     c                   clear                   Filiale1LV        1
048400110901     c                   clear                   Filiale2LV        1
048500110901     c                   clear                   FilialeLOG        1
048600110901      *
048700110901     C                   movel     SottoClas_156 tabGRU
048800110901     C                   move      *all'0'       tabCOD
048900110901     C                   move      ampSCM        tabCOD
049000110901     c     kanTAB        chain     anTAB01l
049100110901     c                   if             %Found(anTAB01l)
049200110901     c                   eval      Antab156 = tabDES
049300110901     c                   eval         ufficiocomp  = GRU156v
049400110901     c                   eval         Filiale1LV   = FI1156v
049500110901     c                   eval         Filiale2LV   = FI2156v
049600110901     c                   eval         FilialeLOG   = FI3156v
049700110902     c                   else
049800110902     C                   MOVEL     *OFF          $RCDOK
049900110902     c                   goto      end_check1
050000110901     c                   end
050100110901     C*
050200110901     C*   Se l'ufficio competente o il livello non coincide
050300110901     c                   if        ufficiocomp <> I501uffCOM
050400110901     c                               or
050500110901     c                             I501livLPO = '1' and Filiale1LV <> *blank
050600110901     c                               or
050700110901     c                             I501livLPO = '2' and Filiale2LV <> *blank
050800110901     c                               or
050900110901     c                             I501livLPO = 'L' and FilialeLOG <> *blank
051000110901     C                   MOVEL     *OFF          $RCDOK
051100110901     c                   goto      end_check1
051200110901     c                   end
051300110901     C*
051400110901     C*
051500110826     C     end_check1    ENDSR
051600110826      *---------------------------------------------------------------*
051700110826      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
051800110826      *---------------------------------------------------------------*
051900110826     C     Carica_campi1 BEGSR
052000110901     C*
052001110908     c                   setoFF                                           34
052100110901     c                   eval      h1PROTECT = '0'
052200110901     c                   eval      h1OBBLIG  = '0'
052300110826     C*
052400110901     c                   if        I501TIPORD = Pianificato
052401110908     C*
052500110901     C* se obbligatorio deve proteggere i campi selezionandolo
052600110901     C*  e accendendo il SFLNXTCHG
052601110908     c                   if        obbligatorio <> *blank
052700110901     c                   eval      h1PROTECT = '1'
052800110901     c                   eval      h1OBBLIG  = '1'
052900110909     c                   seton                                        323436
052901110908     c                   end
052902110909     C*----------
052903110909     C* SE ORDINE PIANIFICATO, CONTROLLA CHE NON SIA già stato inserito
052904110909     C*  in una precedente immissione nell'arco della settimana
052905110909     C*    e quindi imposta i dati registrati precedentemente
052907110909     C                   z-add     I501NUMORD    orpNOR
052908110909     C                   z-add     I501FILIAL    orpFIL
052909110909     C                   move      I501UFFCOM    orpUFF
052910110909     C                   move      oppMAT        orpMAT
052911110909     c     kTMORP        chain(n)  TMORP02L
052912110909     c                   if        %Found(TMORP02l)      and
052913110909     c                             orpPIAN = Pianificato and
052914110909     c                             orpWEEK = Settimana
052915110909     c                   z-add     orpQTA        oppQTA
052916110909     c                   movel     orpATT        oppSOSP
052917110909     c                   if        orpNOTE <> *blank
052918110909     c                   eval      s1not ='*'
052919110909     c                   move      '*'           h1not
052920110909     c                   eval      h1NOTA1 = %Subst(orpNOTE:01:60)
052921110909     c                   eval      h1NOTA2 = %Subst(orpNOTE:61:60)
052922110909     c                   eval      h1NOTA3 = %Subst(orpNOTE:121:60)
052923110909     c                   eval      h1NOTA4 = %Subst(orpNOTE:181:60)
052924110909     c                   end
052925110909     c                   end
053101110908     C*
053102110908     C*  articolo sospeso da sede
053103110909     c                   if        oppSOSP = 'A'
053104110908     c                   eval      h1sosp = 'A'
053105110908     c                   move(p)   '*SOSP'       s1sospeso
053106110909     c                   setoff                                       36
053107110908     c                   end
053108110908     C*
053109110909     c                   if        oppSOSP = 'S'
053110110909     c                   eval      s1selsos = 'S'
053111110909     c                   eval      h1selsos = 'S'
053112110909     c                   move(p)   'SOSP'        s1sospeso
053113110909     c                   end
053114110909     C*
053115110908     c                   z-add     OPPQTA        s1QTA
053125110909     C*
053300110901     c                   end
053400110901     C*
053500110826     C*  carica i campi e li protegge
053600110901     c                   eval      s1MAT =  ampMAT
053700110901     c                   eval      s1UMA =  ampUMA
053800110901     c                   eval      s1DES =  ampDMT
053900110901     c                   eval      h1LVM =  ampLVM
054000110901     c                   eval      h1LRO =  ampLRO
054001110908     c                   eval      h1qtaMAX = ampLEC
054100110901     C*
054101110909     C*
054102110909     c                   eval       H1in36 = *in36
054103110909     c                   eval       H1in34 = *in34
054104110909     C*
054200110826     C                   ENDSR
054300110826      *---------------------------------------------------------------*
054400110826      *?   CONTROLLO TESTATA LISTA                                   ?
054500110826      *---------------------------------------------------------------*
054600110826     C     CTRC1         BEGSR
054700110826     C*
054800110826     C                   MOVE      *OFF          *IN99
054900110826     C*
055000110826     C                   ENDSR
055100110826      *---------------------------------------------------------------*
055200110826      *?   CONTROLLO OPZIONI LISTA                                   ?
055300110826      *---------------------------------------------------------------*
055400110826     C     CTRS1         BEGSR
055500110826     C*
055600110826     C                   MOVEL     *OFF          $ESCI
055700110826     c                   clear                   ErrMsgZ
055800110826     C                   SETOFF                                       99
055900110826     C*
056000110826     C* Leggo il sfl solo se ci sono rcd
0561001108261    C     WMAX          IFGT      0
056200110826     C                   READC     TII5S01                                21
056300110826     C*
056400110826     C* esce se fine sfl o errore che richiede l'uscita
0565001108262    C     *IN21         DOWEQ     *OFF
056600110826     C     $ESCI         ANDEQ     *OFF
056700110826     C                   Z-ADD     S1NRR         C1RCD
056800110826     C* ctrl su riga
056900110826     C                   EXSR      RECS1
057000110826      *
057100110826     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0572001108263    C     *IN99         IFEQ      *ON
057300110826     C     $ESCI         OREQ      *ON
057400110826     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
057500110826     C* la ripristinerò a conclusione del ciclo di READC
057600110826     C                   MOVE      *OFF          $INZS1
0577001108263-   C                   ENDIF
057800110826     C*
057900110826      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
058000110826     C                   MOVE      *ON           *IN32
058100110826     C                   UPDATE    TII5S01
058200110826     C*
058300110826     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0584001108263    C     $ESCI         IFEQ      *OFF
058500110826     C                   READC     TII5S01                                21
058600110826      *
058700110826     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0588001108264    C     *IN21         IFEQ      *ON
058900110826     C* calcolo pagina a cui deve posizionarsi
059000110826     c                   z-add     s1pag         Wpagine
059100110826     C                   EXSR      CLCPAG
0592001108264-   C                   ENDIF
0593001108263-   C                   ENDIF
059400110826     C*
0595001108262-   C                   ENDDO
059600110826     C*
0597001108261-   C                   ENDIF
059800110826     C* __________________________________________________
059900110826     C*  Controlli generali sulle scelte fatte
060000110909     c                   exsr      errori
060100110826     C* __________________________________________________
060200110826     C*
060300110826     C                   ENDSR
060400110826     C/EJECT
060500110826      *---------------------------------------------------------------*
060600110826      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
060700110826      *---------------------------------------------------------------*
060800110826     C     RECS1         BEGSR
060900110826     C*
061000110826     C* reset indicatori DSPATR
061100110826     c                   exsr      INZ_indERR
061101110909     C*
061102110909     c                   eval      *in36 = H1in36
061103110909     c                   eval      *in34 = H1in34
061200110826     C*
061201110909     C* Solo per ordini Pianificati
061203110909     c                   if        I501TIPORD = Pianificato
061204110909     C*
061205110912     C*  SOSPENSIONE ARTICOLO:
061206110912     C*  --------------------
061207110908     c                   clear                   s1sospeso
061209110908     C*
061210110908     c                   if        h1sosp <> *blank
061211110908     c                   move(p)   '*SOSP'       s1sospeso
061213110908     c                   else
061214110909     C*
061229110909     C*  se rilevata variazione sulla sospensione di qualche articolo
0612301109123    C     s1selSOS      ifEQ      'S'
061231110912     c                   move(p)   'SOSP'        s1sospeso
061232110912     c                   end
061233110912     C*
061234110909     c                   if          s1SELsos <> h1SELsos
061235110909     c                   eval         Err001 = errmsg(13)
061236110909     c                   eval         Err002 = errmsg(14)
061237110909     c                   eval         Err003 = *blank
061238110909     c                   eval      h1selsos = s1selSOS
061239110909     C                   MOVEL     *On           $ESCI
061240110909     c                   goTO      endSF1
061241110909     c                   end
061242110912     C*
061243110909     c                   end
061244110909     C*  --------------------
061245110909     c                   endif
061246110909     C* -----------
061300110901     c* se è un articolo obbligatorio
061400110826     C*  segnala l'errore
061500110901     c                   if        s1QTA = 0 and h1obblig = '1'
061600110909     c                   eval        err001 = 'Art.: ' + s1mat + ' qtà : 0 -
061602110909     c                               <--obbligatoria'
061603110909     c                   eval        err002 = ErrMsg(1)
061604110909     c                   setoN                                        99  57
061700110901     c                   goTO      endSF1
061800110901     c                   end
061801110908      **
061802110908      **  supera la quantità massima ordinabile
061803110908     c                   if        s1QTA > 0 and h1qtamax > 0 and
061804110908     c                             s1QTA > h1qtamax
061805110909     c                   eval        err001 = 'Art.: ' + s1mat + ' qtà : '
061806110909     c                             + s1UMA
061807110909     c                             + %editc(s1qta : '2')
061810110908     c                   eval        err002 = ErrMsg(2)
061811110909     c                   eval        err003 = '                    di : '
061813110909     c                             + s1UMA    + %editc(h1qtamax : '2')
061814110909     c                   setoN                                        99  57
061815110908     c                   goTO      endSF1
061816110908     c                   end
061900110901      ***
062000110901      ***   Imposta le Note
062100110908     c                   if        s1selNOT = 'N' and s1QTA = 0
062200110901     c                   eval        err002 = ErrMsg(1)
062201110909     c                   setoN                                        99
062300110901     c                   goTO      endSF1
062400110901     C*
062500110908     c                   elseif    s1selNOT = 'N'
062600110901     C*
062700110901     c                   eval      NOTMAT = s1MAT
062800110901     c                   eval      NOTDES = s1DES
062900110901     c                   eval      NOTQTA = s1qta
063000110901     c                   eval      NOT001 = h1nota1
063100110901     c                   eval      NOT002 = h1nota2
063200110901     c                   eval      NOT003 = h1nota3
063300110901     c                   eval      NOT004 = h1nota4
063400110901     C*
063500110901     c                   exfmt     tii5NOTE
063600110901     C*
063700110901     c                   eval               h1nota1 = NOT001
063800110901     c                   eval               h1nota2 = NOT002
063900110901     c                   eval               h1nota3 = NOT003
064000110901     c                   eval               h1nota4 = NOT004
064100110901     C*
064101110913     C                   eval      s1not = *blank
064102110913     C                   eval      s1selNOT = *blank
064200110901     c                   if        NOT001 <> *blank  or
064300110901     c                             NOT002 <> *blank  or
064400110901     c                             NOT003 <> *blank  or
064500110901     c                             NOT004 <> *blank
064600110901     C                   eval      s1not ='*'
064800110901     c                   end
064900110901     C*
065000110901     c                   endIF
065119110909     C*
065200110826     C     endSF1        ENDSR
065300070227     C/EJECT
065400110726      *---------------------------------------------------------------*
065500110726      *?  GESTIONE F03 x uscire LR                                   ?
065600110726      *---------------------------------------------------------------*
065700110726     C     F03_exit      BEGSR
065800110726     C* fine programma
065900110726     C                   MOVE      *ON           $FINE
066000110901     c                   eval      I501KEYRET = F03
066100110726      *
066200110726     C                   ENDSR
066300110726     C/EJECT
066400110901      *---------------------------------------------------------------*
066500110901      *?  GESTIONE F03 x uscire LR                                   ?
066600110901      *---------------------------------------------------------------*
066700110901     C     F12_return    BEGSR
066800110901     C* fine programma ritorno al video precedente
066900110901     C                   MOVE      *ON           $FINE
067000110901     c                   eval      I501KEYRET = F12
067100110901      *
067200110901     C                   ENDSR
067300110901     C/EJECT
067400070302      *---------------------------------------------------------------*
067500070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
067600070302      *---------------------------------------------------------------*
067700110901     C     GESDB         BEGSR
067800070306      **
067900110906     C                   open      TMORP02l
068000110902      **
068100110901     C                   z-add     0             S1NRR
068200110901      *  legge gli articoli selezionati
068300110901     c                   exsr      Legge_sfl1
0684001109011    C     $EFILE        DowEQ     *OFF
068500110901      * aggiorna il DATA BASE
068600110902     c                   if        s1qta > 0
068700110901     c                   exsr      carica_DataB
068800110902     c                   end
068900110902      *
069000110901     c                   exsr      Legge_sfl1
069100110901     c                   EndDO
069200070305     C*
069300110902     c                   eval      I501KEYRET = F06
069400110902     C                   MOVE      *ON           $FINE
069500110902     C*
069600110906     C                   close     TMORP02l
069700110902     C*
069800070302     C                   ENDSR
069900110901      *---------------------------------------------------------------*
070000110901      *  ?  GESTIONE data Base
070100110901      *---------------------------------------------------------------*
070200110901     C     Legge_Sfl1    BEGSR
070300110901     C*
070400110901     C                   ADD       1             S1NRR
070500110901     C     S1NRR         chain     tii5S01                            33
070600110901     C                   move      *in33         $EFILE
070700110901      **
070800110901     C                   ENDSR
070900110901      *---------------------------------------------------------------*
071000110901      *  ?  GESTIONE data Base
071100110901      *---------------------------------------------------------------*
071200110901     C     carica_DataB  BEGSR
071201110905     C*
071202110905     C                   z-add     I501NUMORD    orpNOR
071203110905     C                   z-add     I501FILIAL    orpFIL
071204110905     C                   move      I501UFFCOM    orpUFF
071205110905     C                   move      s1mat         orpMAT
071206110906     c     kTMORP        chain     TMORP02L
071300110901     C*
071301110906     c                   if        %Found(TMORP02l)
071302110908     c                   if        h1sosp ='A'
071303110908     c                   eval      orpATT = 'A'
071304110908     c                   else
071305110908     c                   if        s1selsos ='S'
071306110908     c                   eval      orpATT = 'S'
071307110909     c                   else
071308110909     c                   eval      orpATT = ' '
071309110909     c                   end
071310110908     c                   end
071311110905     c                   eval          ORPQTA   = s1qta
071312110906     c                   update    TMORP000
071313110905     C*
071314110905     c                   else
071400110906     c                   clear                   TMORP000
071500110901     c                   eval          ORPUTE   = I501CODUTE
071600110901     c                   eval          ORPFIL   = I501FILIAL
071700110901     c                   eval          ORPUFF   = I501UFFCOM
071800110901     c                   eval          ORPNOR   = I501NUMORD
071900110902     C*
072000110902     C                   Z-ADD     I501DATAOR    G02DAT                         *AAAA/MM/GG
072100110902     C                   MOVEL     ' '           G02ERR
072200110902     C                   CALL      'XSRDA8'
072300110902     C                   PARM                    WLBDAT
072400110902     c                   Z-ADD     G02INV        ORPDOR
072500110902     C*
072600110909     c                   eval          ORPTOR   = *blank
072700110901     c                   eval          ORPQTA   = s1qta
072800110901     c                   eval          ORPMAT   = s1mat
072900110901     c                   move      I501WEEKAA    ORPANNO
073000110901     c                   movel     I501WEEKAA    ORPWEEK
073100110901     c                   eval          ORPPIAN  = I501TIPORD
073200110901     c                   eval          ORPCONF  = *blank
073300110901     c                   eval          ORPNOTE  = h1nota1 + h1nota2 +
073400110901     c                                            h1nota3 + h1nota4
073401110908     c                   if        h1sosp ='A'
073402110908     c                   eval      orpATT = 'A'
073403110908     c                   else
073404110908     c                   if        s1selsos ='S'
073405110908     c                   eval      orpATT = 'S'
073406110908     c                   end
073407110908     c                   end
073500110906     c                   write     TMORP000
073501110905     c                   end
073600110901     C*
073700110901     c                   eval      I501WRIUNO = 'S'
073800110901     C*
073900110901     C                   ENDSR
074000110825      *---------------------------------------------------------------*
074100110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
074200110825      *---------------------------------------------------------------*
074300110825     C     CLCPAG        BEGSR
074400110825     C* Input :
074500110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
074600110825     C*          cursore
074700110825     C* - Wpagine= numero rcd per pagina sfl
074800110825     C* Output :
074900110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
075000110825     C*
075100110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
075200110825     C                   MVR                     RESTO             3 0
075300110825     C     RESTO         IFGT      0
075400110825     C                   ADD       1             PAGINE
075500110825     C                   ENDIF
075600110825     C     PAGINE        MULT      Wpagine       WPAG
075700110825     C*
075800110825     C                   ENDSR
075900110825     C************************************************************
076000110825      *?  INIZIALIZZA indicatori di errore                           ?
076100110825     C************************************************************
076200110825     C     INZ_indERR    BEGSR
076300110825      *
076400110825     C                   move      *ALL'0'       IN5190           40
076500110825     C                   moveA     IN5190        *IN(51)
076600110825      *
076700110825     C                   ENDSR
076800070301     C/EJECT
076900110831     C************************************************************
077000070316      *  ?  OPERAZIONI INIZIALI      ?
077100110831     C************************************************************
077200940131     C     *INZSR        BEGSR
077300110726     C*
077400110901     C*    chiave per il ANTAB
077500110901     c     kanTAB        klist
077600110901     C                   KFLD                    tabGRU
077700110901     C                   KFLD                    tabCOD
077800110824     C*
077900110906     c     kTMOPP        klist
078000110906     C                   KFLD                    OPPFIL
078100110906     C                   KFLD                    OPPUFF
078200110901     C*
078300110901     c     ktmAMP        klist
078400110901     C                   KFLD                    ampSCM
078500110901     C                   KFLD                    ampMAT
078600110901     C*
078700110906     c     kTMORP        klist
078800110905     C                   KFLD                    orpNOR
078801110905     C                   KFLD                    orpFIL
078802110905     C                   KFLD                    orpUFF
078803110905     C                   KFLD                    orpMAT
079100110824     C*
079200030113     C* Reperimento parametri
079300030113     C     *ENTRY        PLIST
079400030113     C                   PARM                    KPJBA
079500110831     C                   EVAL      TII501DS = KPJBU
079600110726      *  Se è EDP
079700070308     c                   CLEAR                   se_EDP            1
079800070308     c                   if        %subst(Knmus:1:3) = 'EDP'
079900070308     c                   MOVE      'S'           se_EDP
080000070307     c                   end
080100070227
080200030113     C* Variabili per gestione videate
080300030113     C                   MOVE      *BLANK        $GEST             2
080400070305     C                   MOVE      *BLANK        sav$GEST          2
080500030113     C                   MOVE      *BLANK        $FINE             1
080600110824     C                   MOVE      *BLANK        dopo_in_$FINE     1
080700070227     C                   MOVE      *BLANK        $INZW1            1
080800070227     C                   MOVE      *BLANK        $INZS1            1
080900110726     C                   MOVE      *BLANK        $WRTdb            1
081000030113     C                   MOVE      *BLANK        $EFILE            1
081100030113     C                   MOVE      *BLANK        $ESCI             1
081200030113     C                   MOVE      *BLANK        $RCDOK            1
081300030113     C* Indici
081400030113     C                   Z-ADD     0             X                 3 0
081500030113     C                   Z-ADD     0             Y                 3 0
081600070227     C***
081700070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
081800070227     C                   TIME                    W0140            14 0
081900110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
082000110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
082100070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
082200070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
082300070227     C*
082400110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
082500070227     C                   MOVEL     *BLANK        G02ERR
082600070227     C                   CALL      'XSRDA8'
082700070227     C                   PARM                    WLBDAT
082800110901     c                   Z-ADD     G02INV        Woggi8AMG         8 0          *AAAA/MM/GG
082900110901     c                   eval      dataiso = %date(Woggi8AMG:*iso)
083000110824     c                   eval      dataeur = dataiso
083100110825     c                   eval      oggiiso = dataiso
083200110831     C                   MOVEL     I501WEEKAA    settimana         2 0
083300100505     C*
083400940117     C                   ENDSR
083500110831     C/EJECT
083600110831     C************************************************************
083700110831      *?  INIZIALIZZAZIONE VARIABILI                       ?
083800110831     C************************************************************
083900110831     C     INZVAR        BEGSR
084000110831     C*
084100110831     C* Variabili per gestione videate
084200110831     C                   MOVE      *OFF          $FINE
084300110831     C                   MOVE      *OFF          dopo_in_$FINE
084400110831     C                   MOVE      *OFF          $wrtDB
084500110831     C                   MOVE      *OFF          $INZW1
084600110831     C                   MOVE      *OFF          $EFILE
084700110831     C                   MOVE      *OFF          $ESCI
084800110831     C                   MOVE      *OFF          $RCDOK
084900110831     C* Prima di tutto Controlla Autorizzazioni ad Accedere al PGM
085000110901     C                   MOVE      *ON           $INZS1
085100110831     C                   MOVE      'S1'          $GEST
085200110831     C* Variabili appoggio
085300110831     C                   Z-ADD     1             WPAG
085400110831     c*
085500110831     C                   ENDSR
085600110825     C/EJECT
085700110726      * ?-------------------------------------------------------------*?
085800110826     C*   Routine degli errori da visualizzare sui SFL
085900110826      * ?-------------------------------------------------------------*?
086000110826     C     Errori        BEGSR
086100110826     C*
086200110901     c                   if        ErrMsgZ = *blank
086800110826      *
086900110826     C*  Messaggio di avviso
087000110826     C*  Messaggio di avviso    SE deve emettere un messaggio
087100110826     C*  Messaggio di avviso
087200110826     C*
087300110826     c                   if        err001 <> *blank or
087400110826     c                             err002 <> *blank or
087500110826     c                             err003 <> *blank
087600110826     c                   seton                                        99
087700110826      * ?            *-----------------------*
087800110826     C                   exfmt     TII5WER
087900110826      * ?            *-----------------------*
087901110909     c                   clear                   err001
087902110909     c                   clear                   err002
087903110909     c                   clear                   err003
088000110826     c                   end
088100110826     C*
088200110826     c                   end
088300110826     C*
088400110826     c                   ENDSR
088500110826      *---------------------------------------------------------------------------------
088600080609      *
088700070326** ErrMsg
088800110909L'Articolo non ha quantità o è obbligatorio.                                  01
088900110909L'Articolo ha superato la quantità massima ordinabile                         02
089000110909L'Articolo non ha superato la quantità minima di Riordino                     03
089100110831                                                                               04
089200110831                                                                               05
089300110831                                                                               06
089400110831                                                                               07
089500110831                                                                               08
089600110725                                                                               09
089700110725                                                                               10
089800110725                                                                               11
089900110725                                                                               12
090000110909Se Sospeso o Tolto dalla Sospensione, spiegare nelle Note                      13
090100110909perchè l'articolo è sospeso o reintegrato nell'ordine.                         14
090200110725                                                                               15
090300110725                                                                               16
090400110725                                                                               17
090500110725                                                                               18
090600110726                                                                               19
090700110726                                                                               20
090800110726                                                                               21
090900110726                                                                               22
091000110726                                                                               23
091100110726                                                                               24
091200110726                                                                               25
091300110726                                                                               26
091400110726                                                                               27
091500110726                                                                               28
091600110726                                                                               29
091700110726                                                                               30
091800110726                                                                               31
091900110725                                                                               32
092000110726                                                                               33
092100110725                                                                               34
092200070829                                                                               35
092300070829                                                                               36
092400070829                                                                               37
092500070829                                                                               38
092600070829                                                                               39
092700110831                                                                               40
