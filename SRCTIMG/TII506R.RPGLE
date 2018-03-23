000100111024     H DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND')
000200111024     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300070316
000400110915      * TII506R *----------------------------------------------------*
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
003100110916     FTMORP02L  iF   E           K DISK    infds(orpds)
003200110916     FTMorp00f  uF   E             DISK    rename(tmORP000:tmorpfis) usropn
003300110920     FTMmoc00f  uF a E             DISK
003400111024      *   saldi
003500111024     Ftmsmp01L  UF A E           K DISK
003600111024      *
003700110920     Ftmlmp01L  if   e           k disk
003800110920     Ftmclp01L  if   e           K DISK
003900070404      *  Input
004000110826     FanTAB01l  if   e           k disk
004100110915     FtMAMP01l  if   e           k disk
004200110901      *
004300110915     Ftii506d   CF   E             WORKSTN
004400110725     F                                     SFILE(TII5S01:S1NRR)
004500940201     F                                     INFDS(DSFMT)
004600070305     D*----------------------------------------------------*
004700070829     D Errmsg          S             78    DIM(40) CTDATA PERRCD(1)             MSG DI ERRORE
004800070326     D*----------------------------------------------------*
004900940211     D* Passaggio Parametri
005000940211     D KPJBA         E DS
005100070227     D KPJBUs          s                   like(KPJBU)
005200110726     D*-------------
005300110826     d antab155      e ds
005400110915     d tii506ds      E ds
005500111024     D xsocds          DS          1000
005600111024     D soc001        E DS                  EXTNAME(xsoc001ds)
005700110824     D*-------------
005800940211     D DSFMT           DS           512
005900940506     D  $TASTO               369    369
006000110922     D  NRG                  370    370
006100940211     D  NCL                  371    371
006200940211     D  SFLNRR               378    379B 0
006300940127     D*-------------
006400940127     D* Reperimento nome PGM
006500940127     D STATUS         SDS           333
006600940127     D  DSPGM            *PROC
006700030113     D*-------------
006800110725     d orpds           ds
006900110725     d  orp_nrr              397    400b 0
007000070320     D*-------------
007100070316      * Variabili appoggio sempre presenti in tutte le anagrafiche
007200110831     D S1NRR           S                   Like(C1rcd)
007300110831     D S1pag           S                   Like(C1rcd)
007400110831     D WSfl            S                   Like(C1nrr)
007500110831     D Wmax            S                   Like(C1rcd)
007600110831     D Wpag            S                   Like(C1rcd)
007700110831     D sav_WSfl        S                   Like(WSfl)
007800110831     D sav_Wmax        S                   Like(Wmax)
007900110831     D sav_Wpag        S                   Like(Wpag)
008000110831     D Wpagine         S                   Like(C1rcd)
008100110919     d Conferma_Ordine...
008200110919     d                 S              1    INZ(' ')
008300110824      *--------------------------
008400110826     D RigPag_SFl1     S              3  0 INZ(18)
008500070227      *--------------------------
008600110831     D sav_S1NRR       S                   Like(s1nrr)
008700101129      *--------------------------
008800110726     d dataiso         s               d   datfmt(*iso)
008900110825     d oggiiso         s               d   datfmt(*iso)
009000101129     d dataeur         s               d   datfmt(*eur)
009100070305      *--------------------------
009200070305      * PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
009300070305     D WLBDAT          DS                  INZ
009400070305     D  G02DAT                 1      8  0
009500070227     D  G02INV                 9     16  0
009600070227     D  G02ERR                17     17
009700070227     D  G02TGI                18     22  0
009800070307      *
009900070307     D DATPAR          DS
010000070307     D  GIODAT                 1      8  0
010100070307     D  GIOINV                 9     16  0
010200070307     D  GIOTGI                17     21  0
010300070307      *
010400070307     d Wdata8          DS
010500070307     d  dadata                 1      8  0
010600070307     d  adata                  9     16  0
010700070307      *--------------------------------
010800070227     d AZUTEds       e ds                  extname(AZUTE00F)
010900070227     d DDatiUte      e ds
011000110725      *--------------------------
011100070320     d* Ds per dati organigramma
011200070404      *------
011300100505     d DTAISO          s               D
011400070227      *--------------------------
011500070320      *?  COSTANTI                                      ?
011600070320      *--------------------------
011700940506     D* Tasti di funzione
011800940506     D F01             C                   CONST(X'31')
011900940506     D F02             C                   CONST(X'32')
012000940506     D F03             C                   CONST(X'33')
012100940506     D F04             C                   CONST(X'34')
012200940506     D F05             C                   CONST(X'35')
012300940506     D F06             C                   CONST(X'36')
012400940506     D F07             C                   CONST(X'37')
012500940506     D F08             C                   CONST(X'38')
012600940506     D F09             C                   CONST(X'39')
012700940506     D F10             C                   CONST(X'3A')
012800940506     D F11             C                   CONST(X'3B')
012900070315     D F12             C                   CONST(X'3C')
013000940506     D F13             C                   CONST(X'B1')
013100940506     D F14             C                   CONST(X'B2')
013200940506     D F15             C                   CONST(X'B3')
013300940506     D F16             C                   CONST(X'B4')
013400940506     D F17             C                   CONST(X'B5')
013500940506     D F18             C                   CONST(X'B6')
013600940506     D F19             C                   CONST(X'B7')
013700940506     D F20             C                   CONST(X'B8')
013800940506     D F21             C                   CONST(X'B9')
013900940506     D F22             C                   CONST(X'BA')
014000940506     D F23             C                   CONST(X'BB')
014100940506     D F24             C                   CONST(X'BC')
014200940506     D ENTER           C                   CONST(X'F1')
014300940506     D ROLDWN          C                   CONST(X'F4')
014400940506     D ROLLUP          C                   CONST(X'F5')
014500070301     d digits          c                   '0123456789'
014600110831     d Pianificato     c                   'P'
014700110831     d aConsumo        c                   'C'
014800110831     d Classe_155      c                   '155'
014900110831     d Acquisti        c                   'A'
015000110831     d Marketing       c                   'M'
015100110919     d Conferma        c                   'C'
015200110919     d Revisione       c                   'R'
015201111026     d a_capo          c                   ':/N'
015300940127     C*----------------------------------------------------*
015400070419      *?     MAIN LINE PROGRAM                             ?
015500940127     C*----------------------------------------------------*
015600110902     C*
015700940223     C* inizializzazione variabili
015800940223     C                   EXSR      INZVAR
015900940223     C*
016000940223     C     $FINE         DOWEQ     *OFF
016100110823     C*
016200110826     C* Elenco Articoli da selezionare
016300110826     C     $GEST         CASEQ     'S1'          GESS1                          Lista Articoli
016400110725     C*
016500110725     C* Finestra x errori
016600110725     C     $GEST         CASEQ     'W1'          GESW1                          finestra errori
016700110726      *                                                                         errori su bolle
016800110726     C* Aggiorna il DataBase
016900110726     C     $GEST         CASEQ     'DB'          GESDB                          Aggiorna FILE
017000110726     C*
017100940117     C                   END
017200940117     C                   END
017300110901     C*
017400110901     C* restituisce la DS
017500110915     c                   eval      kpjbu = Tii506ds
017600940325     C* fine programma
017700940325     C                   SETON                                            LR
017800110824     C************************************************************
017900110824      *?  Emissione Finestra Errori                              ?
018000110824     C************************************************************
018100110824     C     GESW1         BEGSR
018200110824     C*
018300110824     C                   eval      $FINE = dopo_in_$FINE
018400110824     C                   eval      $GEST = sav$GEST
018500110824     c*
018600110824     c                   exfmt     TII5WER
018700110823     c*
018800110824     c* emesso l'errore torna al video che lo aveva generato
018900110824     c*  oppure va fuori dal programma  in base a $FINE e $GEST
019000110824     c*
019100110826     C                   ENDSR
019200110826     C/EJECT
019300110826      *---------------------------------------------------------------*
019400110826      *  ?  GESTIONE SFL di immissione righe di ordine               ?
019500110826      *---------------------------------------------------------------*
019600110826     C     GESS1         BEGSR
019700110826     C*
019800110826     C* inizializzazione videata
019900110826     C     $INZS1        IFEQ      *ON
020000110826     C                   EXSR      INZS1
020100110826     C                   MOVE      *OFF          $INZS1
020200110826     C                   ENDIF
020300110826     C*
020400110826     C* Inizializza e imposta piede videata
020500110826     C                   EXSR      INZZ1
020600110826     C*
020700110826     C* emissione piede videata
020800110826      * ?            *-----------------------*
020900110826     C                   WRITE     TII5Z01
021000110826      * ?            *-----------------------*
021100110826     C*
021200110826     C     WMAX          IFEQ      0
021300110826     C* Non ci sono records
021400110826      * ?            *-----------------------*
021500110826     C                   WRITE     TII5V01
021600110826      * ?            *-----------------------*
021700110826     c                   setoff                                       30
021800110826     C                   Else
021900110826     c                   seton                                        30
022000110826     c                   eval       sav_WSfl =  WSfl
022100110826     c                   eval       sav_Wpag =  Wpag
022200110826      *
022300110826     C     Wsfl          IFgt      0
022400110826     C                   Z-ADD     wsfl          C1RCD
022500110826     C                   Else
022600110826     C     Wpag          IFgt      0
022700110826     C                   Z-ADD     wpag          C1RCD
022800110826     C                   EndIF
022900110826     C                   EndIF
023000110826     C                   ENDIF
023100110926     C*
023200110926     c                   setoFF                                           35
023300110926     c                   if        orpPIAN = Pianificato
023400110926     c                   setON                                            35
023500110926     c                   end
023600110926     C*
023700110826      *
023800110826      *  Emissione SFL dei GIRI
023900110826      * ?            *-----------------------*
024000110826     C                   EXFMT     TII5C01
024100110826      * ?            *-----------------------*
024200110826     c                   setoff                                       99
024300110826     C                   movel     'S1'          sav$GEST
024400110826      *
024500110826     C     C1NRR         IFNE      0
024600110826     C                   Z-ADD     C1NRR         WSFL
024700110826     C                   ENDIF
024800110826     C                   Z-ADD     SFLNRR        C1RCD
024900110826     C* Selezioni
0250001108261    C                   SELECT
025100110826     C* F3=Fine
025200110826     C     $TASTO        WHENEQ    F03
025300110826     C                   EXSR      F03_exit
025400110826      *
025500110826     C* F12=Ritorno
025600110826     C     $TASTO        WHENEQ    F12
025700110901     C                   EXSR      F12_return
025800110826      *
0259001108261O   C                   OTHER
026000110826     C* CONTROLLO testata
026100110826     C                   EXSR      CTRC1
026200110826     C* CONTROLLO subfile
026300110826     C     *IN99         IFEQ      *OFF
026400110826     C                   EXSR      CTRS1
026500110826     C                   END
026600110826      *
026700110919      * ? Confermi
026800110919     C     $TASTO        ifeq      F06
026900110919      *
027000110826     C     *IN99         IFEQ      *OFF
027100110826     c     ErrMsgZ       andEQ     *blank
027200110919      *
027300110919      *  se richiesto anche la conferma dell'ordine
027400110919     c                   eval      Conferma_Ordine = 'S'
027500110919      *
027600110902     C                   if          WMAX > 0
027700110919      * ? Scrive   l'ordine
027800110826     C                   MOVE      *ON           $WRTDB
027900110826     C                   movel     'DB'          $GEST
028000110902     c                   elseIf      WMAX = 0
028100110902     C                   MOVE      *ON           $FINE
028200110902     c                   end
028300110919      *
028400110902     c                   end
028500110826      *
028600110919     c                   end
028700110919      *
0288001108261-   C                   ENDSL
028900110826     C*
029000110826     C                   ENDSR
029100110826     C/EJECT
029200110826      *---------------------------------------------------------------*
029300110826      *? INIZIALIZZAZIONE e IMPOSTA il PIEDE del SFL                 ?
029400110826      *---------------------------------------------------------------*
029500110826     C     INZZ1         BEGSR
029600110826     C*
029700110826     C* Messaggio o tasti funzionali
029800110826     c                   clear                   z1dmsg
029900110826     c                   clear                   ErrmsgZ          80
030000110826     C*
030100110826      * imposta eventuali errori
030200110826     c                   eval      z1dmsg = ErrmsgZ
030300110826     C*
030400110826     C                   ENDSR
030500110826     C/EJECT
030600110826      *---------------------------------------------------------------*
030700110826      *? INIZIALIZZAZIONE LISTA dei GIRI da selezionare              ?
030800110826      *---------------------------------------------------------------*
030900110826     C     INZS1         BEGSR
031000110826     C*
031100110826     C* reset indicatori DSPATR
031200110826     c                   exsr      INZ_indERR
031300110826     C* pulizia SFL
031400110826     C                   SETOFF                                         3031
031500110826     C                   WRITE     TII5C01
031600110826     C                   SETON                                          31
031700110826     C*
031800110826     C* Righe x Pagina del 1° SFL
031900110826     c                   z-add     RigPag_SFl1   s1pag
032000110826      *
032100110826     C* CARICAMENTO SFL totale
032200110826     C                   Z-ADD     0             S1NRR
032300110826     C                   Z-ADD     1             C1RCD
032400110826     C                   Z-ADD     0             WMAX
032500110826     C                   Z-ADD     0             Wsfl
032600110826     C* campi titolo
032700110826     C                   MOVEL     dsPGM         c1CPGM
032800110826     C                   MOVEL     knsif         c1nsif
032900110826     C                   MOVEL     knmus         c1nmus
033000110826     C                   MOVEL     RSUT          c1crsu
033100110826     C                   z-add     settimana     c1week
033200110915     C                   z-add     i506numord    C1NOR
033300110915     C                   z-add     i506dataor    C1DOR
033400110908      *
033500110826      **----------------
033600110826     C* Posizionamento su file pilota
033700110915      ***   SETLL x N.ORD/FILIALE/
033800110915     C                   eval        OrPFIL    = I506FILIAL
033900110915     C                   eval        OrPNOR    = C1NOR
034000110915     C     KTMOrP        setll     TMOrP02L
034100110826      **----------------
034200110826     C* Carico il SFL
034300110826     C                   EXSR      ROLS1
034400110826     C*
034500110826     C                   Z-ADD     1             WPAG
034600110826     c                   eval       sav_Wmax =  Wmax
034700110826     C*
034800110826     C                   ENDSR
034900110826     C/EJECT
035000110826      *---------------------------------------------------------------*
035100110826      *? CARICAMENTO PAGINA LISTA                                    ?
035200110826      *---------------------------------------------------------------*
035300110826     C     ROLS1         BEGSR
035400110826     C*
035500110826     C                   SETOFF                                       32
035600110901     C                   Z-ADD     1             Y
035700110826     C                   Z-ADD     WMAX          S1NRR
035800110826     C*
035900110826     C* Caricamento del Sfl x Giro
036000110826     c                   Exsr      Write_S1
036100110826     C*
036200110826     C                   Z-ADD     S1NRR         WMAX                 30
036300110826     C*
036400110826     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
036500110826     C     S1NRR         DIV       S1pag         PAGINE            4 0
036600110826     C                   MVR                     RESTO             3 0
036700110826     C     PAGINE        MULT      S1pag         C1RCD
0368001108261    C     RESTO         IFGT      0
036900110826     C                   ADD       1             C1RCD
0370001108261E   C                   ELSE
037100110826     C                   SUB       S1pag         C1RCD
037200110826     C                   ADD       1             C1RCD
0373001108261-   C                   ENDIF
037400110826     C*
037500110826     C                   ENDSR
037600110826      *---------------------------------------------------------------*
037700110826      *?  Scrive x Ogni rottura di GIRO                              ?
037800110826      *---------------------------------------------------------------*
037900110826     C     Write_S1      BEGSR
038000110826      *
038100110826      *  Precarica da Ordine Periodico
038200110826     c                   exsr      Legge_rec1
038300110826      *
038400110826      * Finchè il giro è lo stesso
0385001109011    C     $EFILE        DowEQ     *OFF
038600110826     C                   clear                   TII5S01
038700110826      *
038800110826      * Solo x ordini mensili
038900110826     c                   exsr      carica_campi1
039000110826      *---------
039100110826     C                   ADD       1             S1NRR
039200110826     C                   ADD       1             Y
039300110826     C                   WRITE     TII5S01
039400110826      *---------
039500110826     c                   exsr      Legge_rec1
039600110826     C*
039700110826     c                   EndDO
039800110909     C*
039900110826     C                   ENDSR
040000110826      *---------------------------------------------------------------*
040100110826      *?  LETTURA RCD ARCHIVIO PILOTA                                ?
040200110826      *---------------------------------------------------------------*
040300110826     C     LEGGE_REC1    BEGSR
040400110826     C*
040500110826     C*  Per EoF deve uscire
040600110826     C*  Per Record Ok deve uscire riportando i dati
040700110826     C                   MOVEL     *OFF          $EFILE
040800110826     C                   MOVEL     *OFF          $RCDOK
040900110826     C*
0410001108261    C     $EFILE        DOUEQ     *ON
041100110826     C     $RCDOK        OREQ      *ON
041200110826     C*
041300110826      * ? Se Ordine Periodico deve caricare da file precaricato
041400110826     c****   LETTURA RECORD x Filiale
041500110915     C     ktmORP        reade     tmORP02L
041600110826      * Fine File
041700110915     C                   if        %Eof(tmORP02L)
041800110826     C                   MOVEL     *ON           $EFILE
041900110826     C                   MOVE      $EFILE        *IN33
042000110826     C                   Else
042100110826      * ?Controlla se la Spedizione è da considerare
042200110826     C                   MOVEL     *ON           $RCDOK
042300110826     C                   exsr      Check_RECord1
042400110826     C                   END
042500110826     C*
0426001108261-   C                   ENDDO
042700110826     C*
042800110826     C                   ENDSR
042900110826      *---------------------------------------------------------------*
043000110826      *?   CONTROLLO il record letto se va bene                      ?
043100110826      *---------------------------------------------------------------*
043200110826     C     Check_Record1 BEGSR
043300110901     C*
043400110901     C*  Aggancia l'articolo
043500110915     c                   eval      ampMAT = orpMAT
043600110915     C     KtmAMP        chain     tmAMP01L
043700110915     c                   if        not %Found(tmAMP01L)
043800110901     C                   MOVEL     *OFF          $RCDOK
043900110901     c                   goto      end_check1
044000110901     c                   end
044100110901     C*--------------
044200110901     C*   Finita Gestione dell'articolo
044300110915     c                   if         ampDFG > 0
044400110915     c                                 or
044500110915     c                              orpCONF = *blank and orpPIAN <> Pianificato
044600110901     C                   MOVEL     *OFF          $RCDOK
044700110901     c                   goto      end_check1
044800110901     c                   end
044900110901     C*
045000110901     C*  controlli sull'articolo
045100110901     C*      Classe Merceologica
045200110901     c                   clear                   obbligatorio      1
045300110901     c                   clear                   facoltativo       1
045400110901     c                   clear                   a_consumo         1
045500110901     C*
045600110901     C                   movel     Classe_155    tabGRU
045700110901     C                   move      *all'0'       tabCOD
045800110901     C                   move      ampCLM        tabCOD
045900110901     c     kanTAB        chain     anTAB01l
046000110901     c                   if             %Found(anTAB01l)
046100110901     c                   eval      Antab155 = tabDES
046200110901     c                   eval         obbligatorio = PE1155v
046300110901     c                   eval         facoltativo  = PE2155v
046400110901     c                   eval         a_consumo    = PE3155v
046500110902     c                   else
046600110902     C                   MOVEL     *OFF          $RCDOK
046700110902     c                   goto      end_check1
046800110902     c                   end
046900110901     C*
047000110826     C     end_check1    ENDSR
047100110826      *---------------------------------------------------------------*
047200110826      *?  Carica gli articoli pianificati per il periodo x  Filiale  ?
047300110826      *---------------------------------------------------------------*
047400110826     C     Carica_campi1 BEGSR
047500110901     C*
047600110916     c                   z-add     orp_nrr       h1nrr
047700110908     c                   setoFF                                           34
047800110901     c                   eval      h1PROTECT = '0'
047900110901     c                   eval      h1OBBLIG  = '0'
048000110826     C*
048100110915     c                   if        orpPIAN = Pianificato
048200110908     C*
048300110901     C* se obbligatorio deve proteggere i campi selezionandolo
048400110901     C*  e accendendo il SFLNXTCHG
048500110908     c                   if        obbligatorio <> *blank
048600110901     c                   eval      h1PROTECT = '1'
048700110901     c                   eval      h1OBBLIG  = '1'
048800110909     c                   seton                                        323436
048900110908     c                   end
049000110908     C*
049100110908     C*  articolo sospeso da sede
049200110915     c                   if        orpATT  = 'A'
049300110908     c                   eval      h1sosp = 'A'
049400110926     c                   move(p)   'da_Sede'     s1sospeso
049500110909     c                   setoff                                       36
049600110908     c                   end
049700110908     C*
049800110915     c                   if        orpATT  = 'S'
049900110926     c******             eval      s1selsos = 'S'
050000110909     c                   eval      h1selsos = 'S'
050100110926     c                   move(p)   'da_Fil.'     s1sospeso
050200110909     c                   end
050300110909     C*
050400110901     c                   end
050500110916     C*----------
050600110916     c                   if        orpNOTE <> *blank
050700110916     c                   eval      s1not ='*'
050800110916     c                   move      '*'           h1not
050900110916     c                   eval      h1NOTA1 = %Subst(orpNOTE:01:60)
051000110916     c                   eval      h1NOTA2 = %Subst(orpNOTE:61:60)
051100110916     c                   eval      h1NOTA3 = %Subst(orpNOTE:121:60)
051200110916     c                   eval      h1NOTA4 = %Subst(orpNOTE:181:60)
051300110916     c                   eval      s1NOTA =h1Nota1 + h1NOTa2 +h1NOTa3
051400110916     c                   end
051500110901     C*
051600110826     C*  carica i campi e li protegge
051601111026     c                   eval      h1ute =  orpUTE
051602111026     c                   eval      h1fil =  orpFIL
051603111026     c                   eval      h1dor =  orpDOR
051604111026     c                   eval      h1nor =  orpNOR
051700110915     c                   eval      s1MAT =  orpMAT
051800110901     c                   eval      s1UMA =  ampUMA
051900110916     c                   eval      s1QTA =  orpQTA
052000110901     c                   eval      s1DES =  ampDMT
052100110901     c                   eval      h1LVM =  ampLVM
052200110901     c                   eval      h1LRO =  ampLRO
052300110908     c                   eval      h1qtaMAX = ampLEC
052400110909     C*
052500110909     c                   eval       H1in36 = *in36
052600110909     c                   eval       H1in34 = *in34
052700111024     c                   eval      c1FIL =  orpFIL
052800110909     C*
052900110826     C                   ENDSR
053000110826      *---------------------------------------------------------------*
053100110826      *?   CONTROLLO TESTATA LISTA                                   ?
053200110826      *---------------------------------------------------------------*
053300110826     C     CTRC1         BEGSR
053400110826     C*
053500110826     C                   MOVE      *OFF          *IN99
053600110826     C*
053700110826     C                   ENDSR
053800110826      *---------------------------------------------------------------*
053900110826      *?   CONTROLLO OPZIONI LISTA                                   ?
054000110826      *---------------------------------------------------------------*
054100110826     C     CTRS1         BEGSR
054200110826     C*
054300110826     C                   MOVEL     *OFF          $ESCI
054400110826     c                   clear                   ErrMsgZ
054500110826     C                   SETOFF                                       99
054600110826     C*
054700110826     C* Leggo il sfl solo se ci sono rcd
0548001108261    C     WMAX          IFGT      0
054900110826     C                   READC     TII5S01                                21
055000110826     C*
055100110826     C* esce se fine sfl o errore che richiede l'uscita
0552001108262    C     *IN21         DOWEQ     *OFF
055300110826     C     $ESCI         ANDEQ     *OFF
055400110826     C                   Z-ADD     S1NRR         C1RCD
055500110826     C* ctrl su riga
055600110826     C                   EXSR      RECS1
055700110826      *
055800110826     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0559001108263    C     *IN99         IFEQ      *ON
056000110826     C     $ESCI         OREQ      *ON
056100110826     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
056200110826     C* la ripristinerò a conclusione del ciclo di READC
056300110826     C                   MOVE      *OFF          $INZS1
0564001108263-   C                   ENDIF
056500110826     C*
056600110826      * ? Attivo sempre il SFLNXTCHG e aggiorno SFL
056700110826     C                   MOVE      *ON           *IN32
056800110826     C                   UPDATE    TII5S01
056900110826     C*
057000110826     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0571001108263    C     $ESCI         IFEQ      *OFF
057200110826     C                   READC     TII5S01                                21
057300110826      *
057400110826     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0575001108264    C     *IN21         IFEQ      *ON
057600110826     C* calcolo pagina a cui deve posizionarsi
057700110826     c                   z-add     s1pag         Wpagine
057800110826     C                   EXSR      CLCPAG
0579001108264-   C                   ENDIF
0580001108263-   C                   ENDIF
058100110826     C*
0582001108262-   C                   ENDDO
058300110826     C*
0584001108261-   C                   ENDIF
058500110826     C* __________________________________________________
058600110826     C*  Controlli generali sulle scelte fatte
058700110909     c                   exsr      errori
058800110826     C* __________________________________________________
058900110826     C*
059000110826     C                   ENDSR
059100110826     C/EJECT
059200110826      *---------------------------------------------------------------*
059300110826      *?   CONTROLLO CAMPI I/O RIGA LISTA                            ?
059400110826      *---------------------------------------------------------------*
059500110826     C     RECS1         BEGSR
059600110826     C*
059700110826     C* reset indicatori DSPATR
059800110826     c                   exsr      INZ_indERR
059900110909     C*
060000110909     c                   eval      *in36 = H1in36
060100110909     c                   eval      *in34 = H1in34
060200110826     C*
060300110912     C*  SOSPENSIONE ARTICOLO:
060400110912     C*  --------------------
060500110908     c                   clear                   s1sospeso
060600110908     C*
060700110908     c                   if        h1sosp <> *blank
060800110926     c                   move(p)   'da_Sede'     s1sospeso
060900110908     c                   else
061000110909     C*
061100110909     C*  se rilevata variazione sulla sospensione di qualche articolo
0612001109263    C     h1selSOS      ifEQ      'S'
061300110926     c                   move(p)   'da_Fil.'     s1sospeso
061400110912     c                   end
061600110912     C*
061800110909     c                   endif
061900110909     C* -----------
062000110901     c* se è un articolo obbligatorio
062100110826     C*  segnala l'errore
062200110901     c                   if        s1QTA = 0 and h1obblig = '1'
062300110909     c                   eval        err001 = 'Art.: ' + s1mat + ' qtà : 0 -
062400110909     c                               <--obbligatoria'
062500110909     c                   eval        err002 = ErrMsg(1)
062600110909     c                   setoN                                        99  57
062700110901     c                   goTO      endSF1
062800110901     c                   end
062900110908      **
063000110908      **  supera la quantità massima ordinabile
063100110908     c                   if        s1QTA > 0 and h1qtamax > 0 and
063200110908     c                             s1QTA > h1qtamax
063300110909     c                   eval        err001 = 'Art.: ' + s1mat + ' qtà : '
063400110909     c                             + s1UMA
063500110909     c                             + %editc(s1qta : '2')
063600110908     c                   eval        err002 = ErrMsg(2)
063700110909     c                   eval        err003 = '                    di : '
063800110909     c                             + s1UMA    + %editc(h1qtamax : '2')
063900110909     c                   setoN                                        99  57
064000110908     c                   goTO      endSF1
064100110908     c                   end
064200110901      ***
064300110919      ***   Legge le Note
064400110908     c                   if        s1selNOT = 'N' and s1QTA = 0
064500110901     c                   eval        err002 = ErrMsg(1)
064600110909     c                   setoN                                        99
064700110901     c                   goTO      endSF1
064800110901     C*
064900110908     c                   elseif    s1selNOT = 'N'
065000110901     C*
065100110901     c                   eval      NOTMAT = s1MAT
065200110901     c                   eval      NOTDES = s1DES
065300110901     c                   eval      NOTQTA = s1qta
065400110901     c                   eval      NOT001 = h1nota1
065500110901     c                   eval      NOT002 = h1nota2
065600110901     c                   eval      NOT003 = h1nota3
065700110901     c                   eval      NOT004 = h1nota4
065800110901     C*
065900110901     c                   exfmt     tii5NOTE
066000110901     C*
066100110901     c                   eval               h1nota1 = NOT001
066200110901     c                   eval               h1nota2 = NOT002
066300110901     c                   eval               h1nota3 = NOT003
066400110901     c                   eval               h1nota4 = NOT004
066500110916     c                   eval      s1NOTA =Not001 + NOT002 +NOT003
066600110901     C*
066700110913     C                   eval      s1not = *blank
066800110913     C                   eval      s1selNOT = *blank
066900110901     c                   if        NOT001 <> *blank  or
067000110901     c                             NOT002 <> *blank  or
067100110901     c                             NOT003 <> *blank  or
067200110901     c                             NOT004 <> *blank
067300110901     C                   eval      s1not ='*'
067400110901     c                   end
067500110901     C*
067600110901     c                   endIF
067701111026      ***
067702111026      ***   Per scrivere sulla mail delle Note sull'articolo
067703111026     c                   if        s1mail   = 'S'
067709111026     C*
067710111026     c                   eval      NOTMAT = s1MAT
067711111026     c                   eval      NOTDES = s1DES
067712111026     c                   eval      NOTQTA = s1qta
067713111026     c                   eval      NOT001 = h1mail1
067714111026     c                   eval      NOT002 = h1mail2
067715111026     c                   eval      NOT003 = h1mail3
067716111026     c                   eval      NOT004 = h1mail4
067717111026     C*
067718111026     c                   exfmt     tii5NOTE
067719111026     C*
067720111026     c                   eval               h1mail1 = NOT001
067721111026     c                   eval               h1mail2 = NOT002
067722111026     c                   eval               h1mail3 = NOT003
067723111026     c                   eval               h1mail4 = NOT004
067725111026     C*
067727111026     C                   eval      s1mail  = *blank
067728111026     c                   if          h1mail1 <> *blank  or
067729111026     c                               h1mail2 <> *blank  or
067730111026     c                               h1mail3 <> *blank  or
067731111026     c                               h1mail4 <> *blank
067732111026     C                   eval      s1mail='*'
067733111026     c                   end
067734111026     C*
067735111026     c                   endIF
067736111026     C*
067800110826     C     endSF1        ENDSR
067900070227     C/EJECT
068000110726      *---------------------------------------------------------------*
068100110726      *?  GESTIONE F03 x uscire LR                                   ?
068200110726      *---------------------------------------------------------------*
068300110726     C     F03_exit      BEGSR
068400110726     C* fine programma
068500110726     C                   MOVE      *ON           $FINE
068600110726      *
068700110726     C                   ENDSR
068800110726     C/EJECT
068900110901      *---------------------------------------------------------------*
069000110901      *?  GESTIONE F03 x uscire LR                                   ?
069100110901      *---------------------------------------------------------------*
069200110901     C     F12_return    BEGSR
069300110901     C* fine programma ritorno al video precedente
069400110901     C                   MOVE      *ON           $FINE
069500110901      *
069600110901     C                   ENDSR
069700110901     C/EJECT
069800070302      *---------------------------------------------------------------*
069900070320      *?  GESTIONE F06 x aggiornare il Data Base                     ?
070000070302      *---------------------------------------------------------------*
070100110901     C     GESDB         BEGSR
070200070306      **
070300110916     C                   open      TMORP00F
070301111026      *
070302111026     C                   movel     *blanks       Indirizzi       253
070303111026     C                   movel     *blanks       Oggetto          44
070304111026     C                   movel     *blanks       Messaggio      5000
070400110902      **
070500110901     C                   z-add     0             S1NRR
070600110901      *  legge gli articoli selezionati
070700110901     c                   exsr      Legge_sfl1
0708001109011    C     $EFILE        DowEQ     *OFF
070900110901      * aggiorna il DATA BASE
071000110919      *
071102111026     c                   if        h1ATT = *blank
071103111026     C* Legge la riga e la converte in TMMOC00F
071104111026     c                   exsr      scrive_TMMOC
071105111026      *
071106111026      * Si predispone per l'invio della Mail
071107111026     C                   if        s1mail='*'
071108111026     c                   exsr      prepara_mail
071109111026     c                   end
071110111026     c                   end
071200110902      *
071300110901     c                   exsr      Legge_sfl1
071400110901     c                   EndDO
071401111026      *
071500111026     C* Da per confermato l'ordine
071603111026     c                   exsr      aggior_DataB
071800110916     C                   close     TMORP00F
071801111026      *
071802111026     C*  se sono state caricate delle righe di mail deve generare la mail
071803111026     C                   if        Messaggio <> *blank
071804111026     c                   exsr      invio_mail
071805111026     c                   end
071900110902     C*
071901111026     C                   MOVE      *ON           $FINE
071902111026      *
072000070302     C                   ENDSR
072100110901      *---------------------------------------------------------------*
072200110901      *  ?  GESTIONE data Base
072300110901      *---------------------------------------------------------------*
072400110901     C     Legge_Sfl1    BEGSR
072500110901     C*
072600110901     C                   ADD       1             S1NRR
072700110901     C     S1NRR         chain     tii5S01                            33
072800110901     C                   move      *in33         $EFILE
072900110901      **
073000110901     C                   ENDSR
073100110825      *---------------------------------------------------------------*
073200110825      * ?  CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL   ?
073300110825      *---------------------------------------------------------------*
073400110825     C     CLCPAG        BEGSR
073500110825     C* Input :
073600110825     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
073700110825     C*          cursore
073800110825     C* - Wpagine= numero rcd per pagina sfl
073900110825     C* Output :
074000110825     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
074100110825     C*
074200110825     C     WSFL          DIV       Wpagine       PAGINE            4 0
074300110825     C                   MVR                     RESTO             3 0
074400110825     C     RESTO         IFGT      0
074500110825     C                   ADD       1             PAGINE
074600110825     C                   ENDIF
074700110825     C     PAGINE        MULT      Wpagine       WPAG
074800110825     C*
074900110825     C                   ENDSR
075000110825     C************************************************************
075100110825      *?  INIZIALIZZA indicatori di errore                           ?
075200110825     C************************************************************
075300110825     C     INZ_indERR    BEGSR
075400110825      *
075500110825     C                   move      *ALL'0'       IN5190           40
075600110825     C                   moveA     IN5190        *IN(51)
075700110825      *
075800110825     C                   ENDSR
075900070301     C/EJECT
076000110831     C************************************************************
076100070316      *  ?  OPERAZIONI INIZIALI      ?
076200110831     C************************************************************
076300940131     C     *INZSR        BEGSR
076400110726     C*
076500110901     C*    chiave per il ANTAB
076600110901     c     kanTAB        klist
076700110901     C                   KFLD                    tabGRU
076800110901     C                   KFLD                    tabCOD
076900110901     C*
077000110901     c     ktmAMP        klist
077100110901     C                   KFLD                    ampMAT
077200110920     C*
077300110920     C     KEYLIS        KLIST                                                  *
077400110920     C                   KFLD                    mocLIS                         *
077500110920     C                   KFLD                    mocMAT                         *
077600110920     C                   KFLD                    mocval                         *
077700110901     C*
077800110906     c     kTMORP        klist
077900110905     C                   KFLD                    orpNOR
078000110905     C                   KFLD                    orpFIL
078100111024     C*
078200111024     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
078300111024     C     KEYSAL        KLIST                                                  *
078400111024     C                   KFLD                    orpANNO                        *
078500111024     C                   KFLD                    mocMAG                         *
078600111024     C                   KFLD                    mocMAT                         *
078700110824     C*
078800030113     C* Reperimento parametri
078900030113     C     *ENTRY        PLIST
079000030113     C                   PARM                    KPJBA
079100110915     C                   EVAL      TII506DS = KPJBU
079200111024     C*
079300111024     C*---------- RICERCA DITTA :
079400111024     C                   MOVEL     'SOC001'      TIPXSC
079500111024     C                   MOVEL     *blank        SOCXSC
079600111024     C                   EXSR      REPSOC
079700111024     C     RTNXSC        IFNE      '1'
079800111024     C                   MOVEL     XSOCDS        SOC001
079900111024     C                   MOVEL     xscsoc        wsoc              3
080000111024     c                   end
080100110726      *  Se è EDP
080200070308     c                   CLEAR                   se_EDP            1
080300070308     c                   if        %subst(Knmus:1:3) = 'EDP'
080400070308     c                   MOVE      'S'           se_EDP
080500070307     c                   end
080600070227
080700030113     C* Variabili per gestione videate
080800030113     C                   MOVE      *BLANK        $GEST             2
080900070305     C                   MOVE      *BLANK        sav$GEST          2
081000030113     C                   MOVE      *BLANK        $FINE             1
081100110824     C                   MOVE      *BLANK        dopo_in_$FINE     1
081200070227     C                   MOVE      *BLANK        $INZW1            1
081300070227     C                   MOVE      *BLANK        $INZS1            1
081400110726     C                   MOVE      *BLANK        $WRTdb            1
081500030113     C                   MOVE      *BLANK        $EFILE            1
081600030113     C                   MOVE      *BLANK        $ESCI             1
081700030113     C                   MOVE      *BLANK        $RCDOK            1
081800030113     C* Indici
081900030113     C                   Z-ADD     0             X                 3 0
082000030113     C                   Z-ADD     0             Y                 3 0
082100070227     C***
082200070227     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
082300070227     C                   TIME                    W0140            14 0
082400110824     C                   MOVE      W0140         woggi             8 0          *GG/MM/AAAA
082500110824     C                   MOVE      W0140         Anno              4 0          *GG/MM/AAAA
082600070227     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
082700070319     C                   MOVEL     W0140         HHMMss            6 0          *ORA/MINUTI/sec
082800111024     c                   move      woggi         dataeur
082900111024     c                   move      dataeur       dataiso
083000111024     c                   move      dataiso       woggig            8 0
083100070227     C*
083200110824     C                   Z-ADD     woggi         G02DAT                         *GG/MM/AAAA
083300070227     C                   MOVEL     *BLANK        G02ERR
083400070227     C                   CALL      'XSRDA8'
083500070227     C                   PARM                    WLBDAT
083600110901     c                   Z-ADD     G02INV        Woggi8AMG         8 0          *AAAA/MM/GG
083700110901     c                   eval      dataiso = %date(Woggi8AMG:*iso)
083800110824     c                   eval      dataeur = dataiso
083900110825     c                   eval      oggiiso = dataiso
084000110915     C                   z-add     I506WEEK      settimana         2 0
084100110915     C                   z-add     I506ANNO      annoweek          4 0
084200100505     C*
084300940117     C                   ENDSR
084400110831     C/EJECT
084500110831     C************************************************************
084600110831      *?  INIZIALIZZAZIONE VARIABILI                       ?
084700110831     C************************************************************
084800110831     C     INZVAR        BEGSR
084900110831     C*
085000110831     C* Variabili per gestione videate
085100110831     C                   MOVE      *OFF          $FINE
085200110831     C                   MOVE      *OFF          dopo_in_$FINE
085300110831     C                   MOVE      *OFF          $wrtDB
085400110831     C                   MOVE      *OFF          $INZW1
085500110831     C                   MOVE      *OFF          $EFILE
085600110831     C                   MOVE      *OFF          $ESCI
085700110831     C                   MOVE      *OFF          $RCDOK
085800110831     C* Prima di tutto Controlla Autorizzazioni ad Accedere al PGM
085900110901     C                   MOVE      *ON           $INZS1
086000110831     C                   MOVE      'S1'          $GEST
086100110831     C* Variabili appoggio
086200110831     C                   Z-ADD     1             WPAG
086300110922     c                   setoff                                       37
086400110922     c                   if        i506pian = Pianificato
086500110926     c                   setoN                                        37
086600110922     c                   end
086700110919     c                   eval      Conferma_Ordine = ' '
086800110831     c*
086900110831     C                   ENDSR
087000110825     C/EJECT
087100110726      * ?-------------------------------------------------------------*?
087200110826     C*   Routine degli errori da visualizzare sui SFL
087300110826      * ?-------------------------------------------------------------*?
087400110826     C     Errori        BEGSR
087500110826     C*
087600110901     c                   if        ErrMsgZ = *blank
087700110826      *
087800110826     C*  Messaggio di avviso
087900110826     C*  Messaggio di avviso    SE deve emettere un messaggio
088000110826     C*  Messaggio di avviso
088100110826     C*
088200110826     c                   if        err001 <> *blank or
088300110826     c                             err002 <> *blank or
088400110826     c                             err003 <> *blank
088500110826     c                   seton                                        99
088600110826      * ?            *-----------------------*
088700110826     C                   exfmt     TII5WER
088800110826      * ?            *-----------------------*
088900110909     c                   clear                   err001
089000110909     c                   clear                   err002
089100110909     c                   clear                   err003
089200110826     c                   end
089300110826     C*
089400110826     c                   end
089500110826     C*
089600110826     c                   ENDSR
089700110919      *---------------------------------------------------------------*
089800110919      *  ?  GESTIONE data Base
089900110919      *---------------------------------------------------------------*
090000110919     C     aggior_DataB  BEGSR
094500110919     C*
094700110919      **----------------
094800110919     C* Posizionamento su file pilota
094900110919      ***   SETLL x N.ORD/FILIALE/
095000110919     C                   eval        OrPFIL    = I506FILIAL
095100110919     C                   eval        OrPNOR    = C1NOR
095200110919     C     KTMOrP        setll     TMOrP02L
095300110919     C     KTMOrP        reade     TMOrP02L
095400110919     c                   doW       not %Eof(TMOrP02L)
095500110919     C*
095900111024     C*  conferma il record letto
096000110919     c     orp_nrr       chain     TMORP00F
096100110919     c                   if        %Found(TMORP00F)
096200110926     c                   eval      orpCONF = 'C'
096300110926     c                   update    TMORPfis
096400111024     c                   exsr      aggSAL
096600110919     C                   END
096700110919     C*
096800110919     C     KTMOrP        reade     TMOrP02L
096900110919     C                   ENDdo
097100110919     C*
097200110919     C                   ENDSR
097300110919      *---------------------------------------------------------------------------------
097400110919     C*  trasferisce l'ordine Provvisorio in Ordine definitivo
097500110919      *---------------------------------------------------------------------------------
097600110919     C     Scrive_TMMOC  begSR
097700110919      **---
097800110919     C                   clear                   tmmoc000
097900111024      **---
098000111024     C                   MOVE      ' '           MOCATT
098100111026     C                   move      h1UTE         MOCUTE
098200111024     C                   Z-ADD     Woggi8AMG     MOCDMN
098300111026     C                   move      h1DOR         MOCDOR
098400111026     C                   Z-ADD     h1NOR         MOCNOR
098500111024     C                   MOVEl     '2 '          MOCTOR
098600111024     C                   MOVEl     'S'           MOCTRO
098700111026     C                   MOVE      s1MAT         MOCMAT
098800111026     C                   Z-ADD     s1QTA         MOCQTA
098900111024      **-----
099000111024     C*  Aggancia l'articolo
099100111026     c                   eval      ampMAT = s1MAT
099200111024     C     KtmAMP        chain     tmAMP01L
099300111024     c                   if        %Found(tmAMP01L)
099400111024     C                   MOVE      ampMAG        MOCMAG
099500111024     C                   MOVE      ampUMQ        MOCUMQ
099600111024     C                   MOVEL     ampDMT        MOCDMT
099700111024     c                   if         ampDFG > 0
099800111024     C                   MOVE      'A'           MOCatt
099900111024     c                   end
100000111024     c                   end
100100111024     C                   MOVEl     '1'           MOCTRG
100200111024     C                   MOVEl     'OR'          MOCFAS
100300111024      *--
100400111024     C                   move      *zeros        MOCDcp
100500111024     C                   move      *zeros        MOCDcr
100600111024     C                   MOVEL     *zeros        MOCCPG
100700111024     C                   Z-ADD     0             MOCAIV
100800111024     C                   Z-ADD     0             MOCNPN
100900111024     C                   move      *zeros        MOCDPC
101000111024     C                   Z-ADD     0             MOCNPC
101100111024     C                   MOVEL     *blanks       MOCSOS
101200111024     C                   MOVE      *BLANKS       MOCFLG
101300111024     C                   MOVEL     *BLANKS       MOCFLS
101400111024     C                   Z-ADD     0             MOCASS
101500111024      *--
101600111024     C                   MOVEL     'EUR'         MOCval
101700111024     C                   z-add     1             MOCcmb
101800111024     C                   MOVEl     '1  '         MOCLIS
101900110920     C                   MOVEL     '022'         MOCSPE
102000111024     C                   MOVEL     '2  '         MOCRES
102100111024     C                   MOVEl     '1  '         MOCIMB
102200111024      *--
102300111024     C                   MOVEl     '00465000'    MOCCLI
102400111026     C                   MOVE      h1FIL         MOCCLI
102500111024     c     moccli        chain     tmclp01l
102600111024     c                   if        %Found(tmclp01l)
102700111024     C                   MOVEL     CLPval        MOCval
102800111024     C                   MOVEl     CLPLIS        MOCLIS
102900111024     C                   MOVEL     CLPSPE        MOCSPE
103000111024     C                   MOVEL     CLPRES        MOCRES
103100111024     C                   MOVEl     CLPIMB        MOCIMB
103200111024     C                   MOVEL     CLPPCL        MOCPCL
103300111024     C                   MOVEl     CLPGPM        MOCGPM
103400111024     C                   MOVEl     CLPSTM        MOCSTM
103500111024     C                   Z-ADD     CLPSC1        MOCSC1
103600111024     C                   Z-ADD     CLPSC2        MOCSC2
103700111024      **
103800111024     C                   MOVE      *BLANKS       MOCFLG
103900110920     C                   MOVEL     *BLANKS       MOCFLS
104000111024     C                   Z-ADD     0             MOCASS
104100111024     c                   end
104200111024      *--
104300111024     C                   MOVE      *BLANKS       MOCESV
104400111024     C                   MOVE      *BLANKS       MOCFES
104500111024     C                   MOVE      *BLANKS       MOCTSR
104600111024     C                   Z-ADD     0             MOCPRZ
104700111024     C                   Z-ADD     0             MOCSC3
104800111024     C                   Z-ADD     1             MOCCLP
104900111024     C*--------------
105000111024     C     KEYLIS        chain     tmlmp01L
105100111024     c                   if        %Found(tmlmp01L)
105200111024     C     MOCCLP        IFEQ      1
105300111024     C                   Z-ADD     LMPVP1        MOCPRZ
105400111024     C                   END
105500111024     C     MOCCLP        IFEQ      2
105600111024     C                   Z-ADD     LMPVP2        MOCPRZ
105700111024     C                   END
105800111024     C     MOCCLP        IFEQ      3
105900111024     C                   Z-ADD     LMPVP3        MOCPRZ
106000111024     C                   END
106100111024     C     MOCCLP        IFEQ      4
106200111024     C                   Z-ADD     LMPVP4        MOCPRZ
106300111024     C                   END
106400111024     C     MOCCLP        IFEQ      5
106500111024     C                   Z-ADD     LMPVP5        MOCPRZ
106600111024     C                   END
106700111024     c                   end
106800111024     C*--------
106900111024     C                   MOVEL     *blank        MOCCOM
107000111024     C                   Z-ADD     0             MOCDBL
107100111024     C                   Z-ADD     0             MOCNBL
107200111024     C                   Z-ADD     0             MOCDFT
107300111024     C                   Z-ADD     0             MOCNFT
107400111024     C                   MOVE      *blank        MOCCDC
107500111024     C                   Z-ADD     0             MOCCRG
107600111024     C                   move      *zeros        MOCdrf
107700111024     C                   Z-ADD     0             MOCNRF
107800110920     C                   MOVEL     *blank        MOCLUO
107900110919      **---
108000110920     c                   write     tmmoc000
108100110919     C*
108200110919     C                   ENDSR
108300111024      *-------------------------------------------------------------------------*
108400111024     C**    AGGIORNAMENTO CAMPI SU SALDI M.P.                               ---**
108500111024      *-------------------------------------------------------------------------*
108600111024     CSR   AGGSAL        BEGSR
108700111024      *
108800111024     C     KEYSAL        CHAIN     tmsmp01L
108900111024      *
109000111024     C                   MOVE      ' '           SMPATT
109100111024     C                   move      xsccut        SMPUTE
109200111024     C                   Z-ADD     woggig        SMPDMN
109300111024     C                   Z-ADD     0             QTSALD           11 2
109400111024     C     MOCQTA        MULT(H)   AMPFCQ        QTSALD
109500111024      *
109600111024     C                   if        not %Found(tmsmp01L)
109700111024     C                   MOVE      orpANNO       SMPAAC
109800111024     C                   MOVE      mocMAG        SMPMAG
109900111024     C                   MOVE      mocMAT        SMPMAT
110000111024     C                   MOVE      AMPUMA        SMPUMA
110100111024     C                   Z-ADD     0             SMPPLF
110200111024     C                   Z-ADD     0             SMPPLV
110300111024     C                   Z-ADD     0             SMPQGI
110400111024     C                   Z-ADD     0             SMPQOR
110500111024     C                   Z-ADD     0             SMPQPR
110600111024     C                   Z-ADD     0             SMPQIM
110700111024     C                   Z-ADD     0             SMPQIP
110800111024     C                   Z-ADD     0             SMPQGF
110900111024     C                   Z-ADD     0             SMPQE1
111000111024     C                   Z-ADD     0             SMPQE2
111100111024     C                   Z-ADD     0             SMPQE3
111200111024     C                   Z-ADD     0             SMPQE4
111300111024     C                   Z-ADD     0             SMPQE5
111400111024     C                   Z-ADD     0             SMPQU1
111500111024     C                   Z-ADD     0             SMPQU2
111600111024     C                   Z-ADD     0             SMPQU3
111700111024     C                   Z-ADD     0             SMPQU4
111800111024     C                   Z-ADD     0             SMPQU5
111900111024     C                   Z-ADD     woggig        SMPDMS
112000111024     C                   Z-ADD     0             SMPNMS
112100111024     C                   Z-ADD     0             SMPPMC
112200111024     C                   MOVE      *BLANKS       SMPFLG
112300111024     C*
112400111024     C                   Z-ADD     QTSALD        SMPQPR
112500111024     C                   WRITE     tmsmp000
112600111024     C                   Else
112700111024     C*
112800111024     C                   ADD       QTSALD        SMPQPR
112900111024     C                   UPDATE    tmsmp000
113000111024     C                   END
113100111024     C*
113200111024     CSR   ENDSAL        ENDSR
113300111024      *-------------------------------------------------------------------------*
113400111024     C* Reperimento dati società
113500111024      *-------------------------------------------------------------------------*
113600111024     C     REPSOC        BEGSR
113700111024     C*
113800111024     C                   CALLB     'XSOC'
113900111024     C                   PARM                    TIPXSC            6
114000111024     C                   PARM                    SOCXSC            3
114100111024     C                   PARM                    CDSXSC            9 0
114200111024     C                   PARM                    MODXSC            3
114300111024     C                   PARM      *blanks       RTNXSC            1
114400111024     C                   PARM                    XSOCDS
114500111024     C                   PARM                    KPJBA
114600111024     C*
114700111024     C                   ENDSR
114801111026     c*==================================================================*
114802111026      * Prepara il Msg x E-mail
114803111026     c*==================================================================*
114804111026     c     Prepara_Mail  begsr
114805111026      *
114806111026     c                   eval      Messaggio =
114807111026     c                              %Trim(Messaggio)  + '/' + s1mat + ' ' +
114808111026     c                              %Trim(S1des)      + ' ' + s1UMA + ' ' +
114809111026     c                              %editc(s1qta:'2') + ' ' +
114810111026     c                             %Trim(h1mail1) + ' ' + %Trim(h1mail2) + ' ' +
114811111026     c                             %Trim(h1mail3) + ' ' + %Trim(h1mail4) + ' ' +
114813111026     c                              ' /'  + a_capo
114814111026      *
114815111026     c                   eval      Indirizzi = 'andrea.bertocchi@bartolini.it'
114816111026     c                   eval       Oggetto   = 'Ordine : ' + %editc(h1nor:'Z')
114817111026     c                              + ' del : ' + %editc(c1dor:'Y')
114818111026     c                              + ' di Riapprov.'
114819111026      *
114820111026     C                   ENDSR
114821111026     c*==================================================================*
114822111026      * Manda un Msg x E-mail
114823111026     c*==================================================================*
114824111026     c     Invio_Mail    begsr
114825111026      *
114826111026     C*   Invia_mail :
114827111026     C* Inizializzo variabili
114828111026     C                   movel     *blanks       wrkEml          253
114829111026     C                   movel     *blanks       wrkMsg         5000
114830111026     C                   movel     *blanks       wrkOgg           44
114831111026      *
114832111026     C* Valorizzo i campi della e-m@ail - indirizzo
114833111026     C                   eval      wrkEml = Indirizzi
114834111026     C                   eval      wrkOgg = Oggetto
114835111026     C                   eval      wrkMsg = Messaggio
114836111026      ********
114837111026     C                   call(e)   'TIS701C'
114838111026     C                   parm                    wrkEml
114839111026     C                   parm                    wrkOgg
114840111026     C                   parm                    wrkMsg
114841111026     C*
114842111026     C                   ENDSR
114843111026     ***********************************************************************
114900070326** ErrMsg
115000110909L'Articolo non ha quantità o è obbligatorio.                                  01
115100110909L'Articolo ha superato la quantità massima ordinabile                         02
115200110909L'Articolo non ha superato la quantità minima di Riordino                     03
115300110831                                                                               04
115400110831                                                                               05
115500110831                                                                               06
115600110831                                                                               07
115700110831                                                                               08
115800110725                                                                               09
115900110725                                                                               10
116000110725                                                                               11
116100110725                                                                               12
116200110909Se Sospeso o Tolto dalla Sospensione, spiegare nelle Note                      13
116300110909perchè l'articolo è sospeso o reintegrato nell'ordine.                         14
116400110725                                                                               15
116500110725                                                                               16
116600110725                                                                               17
116700110725                                                                               18
116800110726                                                                               19
116900110726                                                                               20
117000110726                                                                               21
117100110726                                                                               22
117200110726                                                                               23
117300110726                                                                               24
117400110726                                                                               25
117500110726                                                                               26
117600110726                                                                               27
117700110726                                                                               28
117800110726                                                                               29
117900110726                                                                               30
118000110726                                                                               31
118100110725                                                                               32
118200110726                                                                               33
118300110725                                                                               34
118400070829                                                                               35
118500070829                                                                               36
118600070829                                                                               37
118700070829                                                                               38
118800070829                                                                               39
118900110831                                                                               40
