000100170510     H*------------------------------------------------------------------------*
000200170510     H* Stampa movimenti GABBIE e BANCALI
000300170510     H*------------------------------------------------------------------------*
000400170510     H DECEDIT('0,') DATEDIT(*DMY.)
000500170510     F*------------------------------------------------------------------------*
000600170510     F* DATA BASE
000700170510     F*------------------------------------------------------------------------*
000800170510     FFNLGI2D   CF   E             WORKSTN INFDS(DEVDS1)
000900170510     F                                     SFILE(LGI2DMS:MSGULT)
001000170510     fAzcln01l  if   e           k disk
001100170510     D*------------------------------------------------------------------------*
001200170510     D* SCHIERE
001300170510     D*------------------------------------------------------------------------*
001400170510      *
001500170510     d gio             S              1    DIM(31)
001600170510     d fes             S              1    DIM(31)
001700170510      *
001800170510     D AnMeGi          DS
001900170510     d  Kanno                         4s 0
002000170510     d  Kmese                         2s 0
002100170510     d  gg                            2s 0
002200170510     D*
002300170510     D*------------
002400170510     D* MESSAGGI
002500170510     D*------------
002600170510     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002700170510     D*------------
002800170510     D* DESCRIZIONI TASTI FUNZIONALI
002900170510     D*------------
003000170510     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100170510     D*------------
003200170510     D* FUNZIONI DA CARICARE A VIDEO
003300170510     D*------------
003400170510     D FUV             S             19    DIM(24)
003500170510     D*-------------------
003600170510     D* FILIALI DEI CLIENTI DA VISUALIZZARE
003700170510     D*-------------------
003800170510     D L1              S              3    DIM(250) INZ(*zeros)
003900170510     D*------------------------------------------------------------------------*
004000170510     D* INPUT
004100170510     D*------------------------------------------------------------------------*
004200170510     D* RIDEFINIZIONE FUNZIONI VIDEO
004300170510     D*------------
004400170510     D FUDDS           DS
004500170510     D  FUDDE1                 1      1
004600170510     D  FUDFUN                 2      3
004700170510     D  FUDDE2                 4     19
004800170510     D  FUDFI0                20     34
004900170510     D  FUDATT                35     35
005000170510     D  FUDFI1                36     80
005100170510     D  FUDDES                 1     19
005200170510     D  FUDRIG                 1     80
005300170510     D*------------
005400170510     D* DS GESTIONE TASTI FUNZIONALI
005500170510     D*------------
005600170510     D DSKEY         E DS
005700170510     D DEVDS1          DS
005800170510     D  KEY                  369    369
005900170510     D*------------
006000170510     D* DS "XMSG" - GESTIONE MESSAGGI
006100170510     D*------------
006200170510     D DSMSG         E DS
006300170510     D*------------
006400170510     D* ARCHITETTURA
006500170510     D*------------
006600170510     D KPJBA         E DS
006700170510     D*------------------
006800170510     D* DS "XSRDA8" - INVERSIONE DATA (8)
006900170510     D*------------------
007000170510     D WLBDA8          DS                  INZ
007100170510     D  G08DAT                 1      8  0
007200170510     D  G08INV                 9     16  0
007300170510     D  G08ERR                17     17
007400170510     D  G08TGI                18     22  0
007500170510     d  dataiso        s               d   datfmt(*iso)
007600170510     d meno3GUdate     s              8s 0
007700170510     D*-------------------
007800170510     D* DS REPERIMENTO DATI UTENTE
007900170510     D*-------------------
008000170510     D TIBS34DS      E DS
008100170510     D DDATIUTE      E DS
008200170510     D AZUTEDS       E DS                  extname(AZUTE00F)
008300170510     D*------------------
008400170510     D* DS CARICAMENTO P.O. GESTITI
008500170510     D*-------------------
008600170510     D TRUL31DS      e ds                  INZ
008700170510     D*------------------
008800170510     D* PASSAGGIO PARAMETRI
008900170510     D*------------------
009000170510     D PARAM           DS                  INZ
009100170510     D  PARDAL                        8  0
009200170510     D  PARAL                         8  0
009300170510     D  PARFIL                        3  0
009400170510     D*
009500170510     D*------------------
009600170510     D* VARIABILI DI WRK
009700170510     D*------------------
009800170510     D SAVKPJBU                            LIKE(KPJBU)
009900170510     C*------------------------------------------------------------------------*
010000170510     C* MAIN LINES
010100170510     C*------------------------------------------------------------------------*
010200170510     C*
010300170510     C* NON CI SONO ERRORI IN ENTRATA
010400170510     C     ERRL00        IFEQ      '0'
010500170510     C*
010600170510     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010700170510     C     F03L00        DOUEQ     '1'
010800170510     C     F12L00        OREQ      '1'
010900170510     C     FINL00        OREQ      '1'
011000170510     C*
011100170510     C* EMETTE FORMATO VIDEO
011200170510     C                   EXSR      BUTFMR
011300170510     C*
011400170510     C* GESTIONE TASTI FUNZIONALI
011500170510     C                   EXSR      FUNCON
011600170510     C                   ENDDO
011700170510     C*
011800170510     C                   ENDIF
011900170510     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
012000170510     C                   MOVEL     PARAM         KPJBU
012100170510     C* FINE PGM
012200170510     C                   SETON                                        LR
012300170510     C*------------------------------------------------------------------------*
012400170510     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012500170510     C*------------------------------------------------------------------------*
012600170510     C     BUTFMR        BEGSR
012700170510     C*
012800170510     C* EMETTE LA TESTATA
012900170510     C                   WRITE     LGI2D01
013000170510     C*
013100170510     C* SPEDISCE EVENTUALE MESSAGGIO
013200170510     C                   EXSR      SNDMSG
013300170510     C*
013400170510     C* EMETTE LE FUNZIONI
013500170510     C                   WRITE     LGI2D02
013600170510     C*
013700170510     C* EMETTE IL VIDEO
013800170510     C                   EXFMT     LGI2D03
013900170510     C*
014000170510     C* REIMPOSTA FLAG DI ERRORE
014100170510     C                   MOVEL     '0'           ERRL00
014200170510     C*
014300170510     C                   ENDSR
014400170510     C*------------------------------------------------------------------------*
014500170510     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014600170510     C*------------------------------------------------------------------------*
014700170510     C     SNDMSG        BEGSR
014800170510     C*
014900170510     C* PULISCE IL SFILE MESSAGGI
015000170510     C                   MOVEL     '1'           *IN40
015100170510     C                   WRITE     LGI2DMC
015200170510     C                   MOVEL     '0'           *IN40
015300170510     C*
015400170510     C* VISUALIZZA IL MESSAGGIO
015500170510     C                   Z-ADD     *ZEROS        MSGULT            4 0
015600170510     C                   CALL      'XMSG'        XMSGPA
015700170510     C     DSMSER        IFEQ      *ZEROS
015800170510     C                   MOVEL     DSMSKE        MSGKEY
015900170510     C                   ENDIF
016000170510     C                   Z-ADD     1             MSGULT
016100170510     C                   WRITE     LGI2DMS
016200170510     C                   WRITE     LGI2DMC
016300170510     C*
016400170510     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016500170510     C                   MOVEL     *BLANKS       DSMSMS
016600170510     C*
016700170510     C                   ENDSR
016800170510     C*------------------------------------------------------------------------*
016900170510     C* ENTGES - GESTIONE ENTER -> INVIO
017000170510     C*------------------------------------------------------------------------*
017100170510     C     ENTGES        BEGSR
017200170510     C*
017300170510     C* CONTROLLA I CAMPI IMMESSI
017400170510     C                   EXSR      DECODI
017500170510     C*
017600170510     C                   ENDSR
017700170510     C*------------------------------------------------------------------------*
017800170510     C* F03GES - GESTIONE F03 -> FINE
017900170510     C*------------------------------------------------------------------------*
018000170510     C     F03GES        BEGSR
018100170510     C*
018200170510     C* IMPOSTA "PREMUTO F03"
018300170510     C                   MOVEL     '1'           F03L00
018400170510     C*
018500170510     C                   ENDSR
018600170510     C*------------------------------------------------------------------------*
018700170510     C* F06GES - GESTIONE F06 -> SOTTOMETTE RICHIESTA
018800170510     C*------------------------------------------------------------------------*
018900170510     C     F06GES        BEGSR
019000170510     C*
019100170510     C                   EXSR      DECODI
019200170510     C*
019300170510     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
019400170510     C     ERRL00        IFEQ      '0'
019500170510     C*
019600170510     C* LANCIO IL PROGRAMMA DI STAMPA MOVIMENTI
019700170510     C                   MOVEL     'LGI3'        KCOAZ
019800170510     C*
019900170510     C                   eval      savKPJBU = KPJBU
020000170510     C                   EXSR      VIDPAR
020100170510     C                   eval        KPJBU = PARAM
020200170510     C                   CALL      'BCH10'
020300170510     C********           CALL      'FNLGI3C'
020400170510     C                   PARM                    KPJBA
020500170510     C                   eval        KPJBU  = savKPJBU
020600170510     C*
020700170510     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020800170510     C                   MOVEL     '1'           F03L00
020900170510     C*
021000170510     C                   ENDIF
021100170510     C*
021200170510     C                   ENDSR
021300170510     C*------------------------------------------------------------------------*
021400170510     C* F12GES - GESTIONE F12 -> RITORNO
021500170510     C*------------------------------------------------------------------------*
021600170510     C     F12GES        BEGSR
021700170510     C*
021800170510     C* IMPOSTA "PREMUTO F12"
021900170510     C                   MOVEL     '1'           F12L00
022000170510     C*
022100170510     C                   ENDSR
022200170510     C*------------------------------------------------------------------------*
022300170510     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022400170510     C*------------------------------------------------------------------------*
022500170510     C     F24GES        BEGSR
022600170510     C*
022700170510     C                   EXSR      FUNGES
022800170510     C*
022900170510     C                   ENDSR
023000170510     C*------------------------------------------------------------------------*
023100170510     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023200170510     C*------------------------------------------------------------------------*
023300170510     C     DECODI        BEGSR
023400170510     C*
023500170510     C                   MOVEL     '0'           ERRL00
023600170510     C*
023700170510     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
023800170510     C                   SETOFF                                         8384
023900170510     C*
024000170510     C*--------------------
024100170510     C* CONTROLLO SE DATA DAL > 0 E DATA DETTAGLIO AL = 0 =>
024200170510     C* DATA DETTAGLIO AL = DATA DETTAGLIO DAL
024300170510     C*--------------------
024400170510     C                   IF        VIDDAL <> *zeros AND
024500170510     C                             VIDAL   = *zeros
024600170510     C                   EVAL      VIDAL = VIDDAL
024700170510     C                   ENDIF
024800170510     C*--------------------
024900170510     C* PERIODO DETTAGLIO MOVIMENTI
025000170510     C*--------------------
025100170510     C                   IF        VIDDAL = *zeros OR
025200170510     C                             VIDAL  = *zeros
025300170510     C                   MOVEL     '1'           ERRL00
025400170510     C                   SETON                                        8384
025500170510     C                   MOVEL     MSG(6)        DSMSMS
025600170510     C                   leavesr
025700170510     C                   ENDIF
025800170510     C*--------------------
025900170510     C* CONTROLLO DATA DETTAGLIO DAL
026000170510     C*--------------------
026100170510     C                   IF        VIDDAL <> *zeros
026200170510     C     *LIKE         DEFINE    VIDDAL        DALVID
026300170510     C                   Z-ADD     VIDDAL        WDA8
026400170510     C                   Z-ADD     *ZEROS        DA8W
026500170510     C                   EXSR      CHKDA8
026600170510     C                   Z-ADD     WDA8          VIDDAL
026700170510     C                   Z-ADD     DA8W          DALVID
026800170510     C     ERRL00        IFEQ      '1'
026900170510     C                   SETON                                        83
027000170510     C                   MOVEL     MSG(3)        DSMSMS
027100170510     C                   leavesr
027200170510     C                   ENDIF
027300170510     C                   ENDIF
027400170510     C*--------------------
027500170510     C* CONTROLLO DATA DETTAGLIO AL
027600170510     C*--------------------
027700170510     C                   IF        VIDAL  <> *zeros
027800170510     C     *LIKE         DEFINE    VIDAL         AALVID
027900170510     C                   Z-ADD     VIDAL         WDA8
028000170510     C                   Z-ADD     *ZEROS        DA8W
028100170510     C                   EXSR      CHKDA8
028200170510     C                   Z-ADD     WDA8          VIDAL
028300170510     C                   Z-ADD     DA8W          AALVID
028400170510     C     ERRL00        IFEQ      '1'
028500170510     C                   SETON                                        84
028600170510     C                   MOVEL     MSG(3)        DSMSMS
028700170510     C                   leavesr
028800170510     C                   ENDIF
028900170510     C                   ENDIF
029000170510     C*--------------------
029100170510     C* CONTROLLO DATA DETTAGLIO DAL > DATA DETTAGLIO AL
029200170510     C*--------------------
029300170510     C                   IF        dalvid > aalvid
029400170510     C                   MOVEL     '1'           ERRL00
029500170510     C                   SETON                                        8384
029600170510     C                   MOVEL     MSG(2)        DSMSMS
029700170510     C                   leavesr
029800170510     C                   ELSE
029900170510      **
030000170510     C                   IF        dalvid < 20170410
030100170510     C                   MOVEL     '1'           ERRL00
030200170510     C                   SETON                                        83
030300170510     C                   MOVEL     MSG(4)        DSMSMS
030400170510     C                   leavesr
030500170510     C                   ENDIF
030600170510      **
030700170510     C                   IF         aalvid > meno3GUdate
030800170510     C                   MOVEL     '1'           ERRL00
030900170510     C                   SETON                                        84
031000170510     C                   MOVEL     MSG(5)        DSMSMS
031100170510     C                   eval      dsMSMS =  %Trim(dsMSMS) +
031200170510     c                             %editc(gg:'X')      + '/' +
031300170510     c                             %editc(Kmese:'X')   + '/' +
031400170510     c                             %editc(Kanno:'X')
031500170510     C                   leavesr
031600170510     C                   ENDIF
031700170510      **
031800170510     C                   ENDIF
031900170510     C*--------------------
032000170510     C*
032100170510     C                   ENDSR
032200170510     C*------------------------------------------------------------------------*
032300170510     C* CHKDA8 - CONTROLLA LA DATA (8)
032400170510     C*------------------------------------------------------------------------*
032500170510     C     CHKDA8        BEGSR
032600170510     C*
032700170510     C     *LIKE         DEFINE    VIDDAL        WDA8
032800170510     C     *LIKE         DEFINE    VIDDAL        DA8W
032900170510     C*
033000170510     C                   Z-ADD     WDA8          G08DAT
033100170510     C                   Z-ADD     *ZEROS        G08INV
033200170510     C                   MOVEL     '0'           G08ERR
033300170510     C                   CALL      'XSRDA8'
033400170510     C                   PARM                    WLBDA8
033500170510     C                   Z-ADD     G08DAT        WDA8
033600170510     C                   Z-ADD     G08INV        DA8W
033700170510     C     G08ERR        IFEQ      '1'
033800170510     C                   MOVEL     '1'           ERRL00
033900170510     C                   ENDIF
034000170510     C*
034100170510     C                   ENDSR
034200170510     C*------------------------------------------------------------------------*
034300170510     C* CARTAB - CARICA LE TABELLE
034400170510     C*------------------------------------------------------------------------*
034500170510     C     CARTAB        BEGSR
034600170510     C*
034700170510     C* REPERISCE I DATI UTENTE
034800170510     C                   EXSR      REPDATIUTE
034900170510     C*
035000170510     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
035100170510     C                   CLEAR                   L1
035200170510     C                   CLEAR                   WAbi              2
035300170510     C*
035400170510     C                   IF        UTEAUT > *blanks
035500170510     C                   EVAL      WAbi = UTEAUT
035600170510     C                   ELSE
035700170510     C*
035800170510     C                   IF        DUTLPO = 'S'
035900170510     C                   EVAL      WAbi = 'AZ'
036000170510     C                   ENDIF
036100170510     C                   IF        DUTLPO = '1'
036200170510     C                   EVAL      WAbi = 'TP'
036300170510     C                   ENDIF
036400170510     C                   IF        DUTLPO = '2'
036500170510     C                   EVAL      WAbi = 'PO'
036600170510     C                   ENDIF
036700170510     C                   ENDIF
036800170510     C*
036900170510     C* Reperisce i P.O. gestibili dall'utente
037000170510     C                   CLEAR                   TRUL31DS
037100170510     C                   EVAL      I31abi = WAbi
037200170510     C                   EVAL      I31cpo = DUTPOU
037300170510     C                   CALL      'TRUL31R'
037400170510     C                   PARM                    KPJBA
037500170510     C                   PARM                    TRUL31DS
037600170510     C                   IF        O31pog > *zeros
037700170510     C                   MOVEA     O31pog        L1
037800170510     C                   ELSE
037900170510     C                   EVAL      FINL00 = '1'
038000170510     C                   ENDIF
038100170510     C*
038200170510     C                   ENDSR
038300170510     C*-------------------------------------------------------------------------
038400170510     C* REPERISCE I DATI UTENTE
038500170510     C*-------------------------------------------------------------------------
038600170510     C     REPDATIUTE    BEGSR
038700170510     C*
038800170510     C* INIZIALIZZA VARIABILI DI WRK
038900170510     C                   CLEAR                   TIBS34DS
039000170510     C                   CLEAR                   AZUTEDS
039100170510     C                   CLEAR                   DDATIUTE
039200170510     C*
039300170510     C     *DTAARA       DEFINE    §azute        azuteds
039400170510     C     *DTAARA       DEFINE    §datiute      ddatiute
039500170510     C                   IN(E)     *DTAARA
039600170510     C                   IF        %Error
039700170510     C                   EVAL      I34Tla = 'L'
039800170510     C                   CALL      'TIBS34R'
039900170510     C                   PARM                    Tibs34Ds
040000170510     C                   IN        *DTAARA
040100170510     C                   ENDIF
040200170510     C*
040300170510     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
040400170510     C                   MOVEL(P)  RSUT          DSFIRS           20
040500170510     C*
040600170510     C                   ENDSR
040700170510     C*------------------------------------------------------------------------*
040800170510     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
040900170510     C*------------------------------------------------------------------------*
041000170510     C     VIDPAR        BEGSR
041100170510     C*
041200170510     C                   Z-ADD     DALVID        PARDAL
041300170510     C                   Z-ADD     AALVID        PARAL
041400170510     C                   Z-ADD     0             PARFIL
041500170510     C*
041600170510     C                   ENDSR
041700170510     C*------------------------------------------------------------------------*
041800170510     C*  AZZVID - AZZERA O INIZIALIZZA TUTTI I CAMPI DEL FILE VIDEO
041900170510     C*------------------------------------------------------------------------*
042000170510     C     AZZVID        BEGSR
042100170510     C*
042200170511     C                   Z-ADD     dataGMA       VIDDAL
042300170511     c                   movel     '01'          VIDDAL
042400170511     C                   Z-ADD     dataGMA       VIDAL
042500170510     C                   SETOFF                                       55
042600170510     C                   SETOFF                                       56
042700170510     C                   SETON                                        92
042800170510     C*
042900170510     C                   ENDSR
043000170510     C*------------------------------------------------------------------------*
043100170510     C* *INZSR - OPERAZIONI INIZIALI
043200170510     C*------------------------------------------------------------------------*
043300170510     C     *INZSR        BEGSR
043400170510     C*
043500170510     C* RICEVIMENTO PARAMETRI
043600170510     C     *ENTRY        PLIST
043700170510     C                   PARM                    KPJBA
043800170510      *
043900170510     c     kcln          klist
044000170510     c                   kfld                    clntfp
044100170510     c                   kfld                    clntfa
044200170510     c                   kfld                    clnann
044300170510     c                   kfld                    clnmes
044400170510      *
044500170510     C*---
044600170510     C* CHIAVI DI LETTURA
044700170510     C*
044800170510     C* CARICAMENTO TABELLE
044900170510     C                   EXSR      CARTAB
045000170510     C*
045100170510     C* REPERISCO DATA E ORA CORRENTI
045200170510     C                   TIME                    WN14             14 0
045300170510     C                   MOVE      WN14          WN8               8 0
045400170510     C                   MOVEL     WN14          ORACOR            6 0
045500170510     C                   Z-ADD     WN8           G08DAT
045600170510     C                   Z-ADD     *ZEROS        G08INV
045700170510     C                   MOVEL     '0'           G08ERR
045800170510     C                   CALL      'XSRDA8'
045900170510     C                   PARM                    WLBDA8
046000170510     C                   Z-ADD     G08INV        dataAMG           8 0
046100170510     C*   data corrente
046200170510     c                   movel     dataAMG       dataiso
046300170510     c                   subdur    3:*d          dataiso
046400170510      ****
046500170510     c     riprova       tag
046600170510     c                   movel     dataiso       meno3GUdate
046700170510     C                   movel     meno3GUdate   AnMeGi
046800170510      *
046900170510      *controllo se udate è festivo non lancio l'elaborazione
047000170510      * ed esce subito
047100170510     c                   clear                   clntfa
047200170510     c                   clear                   clntfp
047300170510     c                   z-add     kanno         clnann
047400170510     c                   z-add     kmese         clnmes
047500170510     c     kcln          chain     azcln01l
047600170510     c                   if        %found(azcln01l)
047700170510     c                   movea     clnmat        gio
047800170510     c                   movea     clnpom        fes
047900170510     c                   end
048000170510      * esce subito se festivo o
048100170510      *  se sabato o domenica
048200170510     c                   if        gio(gg) = '6' or gio(gg) = '7' or
048300170510     c                             fes(gg) = 'F'
048400170510     c                   subdur    1:*d          dataiso
048500170510     c                   goto      riprova
048600170510     c                   end
048700170510      *
048800170511     C                   Z-ADD     0             G08DAT
048900170511     C                   Z-ADD     meno3GUdate   G08INV
049000170511     C                   MOVEL     '3'           G08ERR
049100170511     C                   CALL      'XSRDA8'
049200170511     C                   PARM                    WLBDA8
049300170511     C                   Z-ADD     G08DAT        dataGMA           8 0
049400170510     C*
049500170510     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
049600170510     C     XMSGPA        PLIST
049700170510     C                   PARM                    DSMSMS
049800170510     C                   PARM                    DSMSPG
049900170510     C                   PARM                    DSMSKE
050000170510     C                   PARM                    DSMSER
050100170510     C*
050200170510     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
050300170510     C                   CALL      'XKEY'
050400170510     C                   PARM                    DSKEY
050500170510     C*
050600170510     C* DEFINISCE LE VARIABILI DI PROGRAMMA
050700170510     C     *LIKE         DEFINE    FUDFUN        FUNKEY
050800170510     C*
050900170510     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
051000170510     C*   -NOME PGM CHIAMANTE
051100170510     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
051200170510     C                   MOVEL     'TILGI2R'     DSMSPG
051300170510     C                   MOVEL     'TILGI2R'     MSGQUE
051400170510     C                   MOVEL     *BLANKS       MSGKEY
051500170510     C                   MOVEL     *BLANKS       DSMSMS
051600170510     C*
051700170510     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
051800170510     C                   Z-ADD     0             FUNGRU            5 0
051900170510     C*
052000170510     C* GESTIONE TASTI FUNZIONALI A VIDEO
052100170510     C                   EXSR      FUNGES
052200170510     C*
052300170510     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03 E VARIABILI DI CONTROLLO
052400170510     C                   MOVEL     '0'           F12L00            1
052500170510     C                   MOVEL     '0'           F03L00            1
052600170510     C                   MOVEL     '0'           FINL00            1
052700170510     C                   MOVEL     '0'           ERRL00            1
052800170510     C                   MOVEL     '0'           FLFINE            1
052900170510     C*
053000170510     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
053100170510     C                   EXSR      AZZVID
053200170510     C*
053300170510     C                   ENDSR
053400170510     C*------------------------------------------------------------------------*
053500170510     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
053600170510     C*------------------------------------------------------------------------*
053700170510     C     FUNGES        BEGSR
053800170510     C*
053900170510     C* PULISCO CAMPI DI LAVORO
054000170510     C                   Z-ADD     0             W                 5 0
054100170510     C*
054200170510     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
054300170510     C* E CARICO TABELLA FUNZIONI ATTIVE
054400170510     C     1             DO        24            I
054500170510     C                   MOVEL     FUD(I)        FUDRIG
054600170510     C*
054700170510     C* PULIZIA FUNZIONI PRECEDENTI
054800170510     C                   MOVEL     *BLANKS       FUV(I)
054900170510     C*
055000170510     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
055100170510     C* (SALTA F24)
055200170510     C     FUDATT        IFEQ      'S'
055300170510     C     FUDFUN        ANDNE     '24'
055400170510     C                   ADD       1             W
055500170510     C                   MOVEL     FUDDES        FUV(W)
055600170510     C                   END
055700170510     C                   END
055800170510     C*
055900170510     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
056000170510     C* ALTRIMENTI DISATTIVO F24
056100170510     C     1             DO        24            I
056200170510     C                   MOVEL     FUD(I)        FUDRIG
056300170510     C*
056400170510     C     FUDFUN        IFEQ      '24'
056500170510     C     W             IFGT      6
056600170510     C                   MOVEL     'S'           FUDATT
056700170510     C                   ELSE
056800170510     C                   MOVEL     *BLANKS       FUDATT
056900170510     C                   END
057000170510     C                   MOVEL     FUDRIG        FUD(I)
057100170510     C                   END
057200170510     C                   END
057300170510     C*
057400170510     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
057500170510     C     W             DIV       6             I
057600170510     C                   MVR                     Z
057700170510     C*
057800170510     C     Z             IFNE      0
057900170510     C     I             ADD       1             Z
058000170510     C                   ELSE
058100170510     C                   Z-ADD     I             Z                 5 0
058200170510     C                   END
058300170510     C*
058400170510     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
058500170510     C                   ADD       1             FUNGRU
058600170510     C     FUNGRU        IFGT      Z
058700170510     C     FUNGRU        ORLT      1
058800170510     C                   Z-ADD     1             FUNGRU
058900170510     C                   END
059000170510     C*
059100170510     C* PRIMO GRUPPO DI 6 FUNZIONI
059200170510     C     FUNGRU        IFEQ      1
059300170510     C                   MOVEL     FUV(1)        FUN001
059400170510     C                   MOVEL     FUV(2)        FUN002
059500170510     C                   MOVEL     FUV(3)        FUN003
059600170510     C                   MOVEL     FUV(4)        FUN004
059700170510     C                   MOVEL     FUV(5)        FUN005
059800170510     C                   MOVEL     FUV(6)        FUN006
059900170510     C                   END
060000170510     C*
060100170510     C* SECONDO GRUPPO DI 6 FUNZIONI
060200170510     C     FUNGRU        IFEQ      2
060300170510     C                   MOVEL     FUV(7)        FUN001
060400170510     C                   MOVEL     FUV(8)        FUN002
060500170510     C                   MOVEL     FUV(9)        FUN003
060600170510     C                   MOVEL     FUV(10)       FUN004
060700170510     C                   MOVEL     FUV(11)       FUN005
060800170510     C                   MOVEL     FUV(12)       FUN006
060900170510     C                   END
061000170510     C*
061100170510     C* TERZO GRUPPO DI 6 FUNZIONI
061200170510     C     FUNGRU        IFEQ      3
061300170510     C                   MOVEL     FUV(13)       FUN001
061400170510     C                   MOVEL     FUV(14)       FUN002
061500170510     C                   MOVEL     FUV(15)       FUN003
061600170510     C                   MOVEL     FUV(16)       FUN004
061700170510     C                   MOVEL     FUV(17)       FUN005
061800170510     C                   MOVEL     FUV(18)       FUN006
061900170510     C                   END
062000170510     C*
062100170510     C* QUARTO GRUPPO DI 6 FUNZIONI
062200170510     C     FUNGRU        IFEQ      4
062300170510     C                   MOVEL     FUV(19)       FUN001
062400170510     C                   MOVEL     FUV(20)       FUN002
062500170510     C                   MOVEL     FUV(21)       FUN003
062600170510     C                   MOVEL     FUV(22)       FUN004
062700170510     C                   MOVEL     *BLANKS       FUN005
062800170510     C                   MOVEL     *BLANKS       FUN006
062900170510     C                   END
063000170510     C*
063100170510     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
063200170510     C                   MOVEL     *BLANKS       FUN007
063300170510     C                   MOVEL     *BLANKS       FUN008
063400170510     C*
063500170510     C     1             DO        24            I
063600170510     C                   MOVEL     FUD(I)        FUDRIG
063700170510     C     FUDFUN        IFEQ      '24'
063800170510     C     FUDATT        ANDEQ     'S'
063900170510     C                   MOVEL     FUDDES        FUN008
064000170510     C                   END
064100170510     C                   END
064200170510     C                   ENDSR
064300170510     C*------------------------------------------------------------------------*
064400170510     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
064500170510     C*------------------------------------------------------------------------*
064600170510     C     FUNCON        BEGSR
064700170510     C*
064800170510     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
064900170510     C                   EXSR      FUNVAR
065000170510     C*
065100170510     C* PULISCO CAMPI DI LAVORO
065200170510     C                   Z-ADD     0             I                 5 0
065300170510     C                   Z-ADD     0             W                 5 0
065400170510     C*
065500170510     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
065600170510     C     FUNKEY        IFNE      *BLANKS
065700170510     C     1             DO        24            I
065800170510     C                   MOVEL     FUD(I)        FUDRIG
065900170510     C*
066000170510     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
066100170510     C     FUNKEY        IFEQ      FUDFUN
066200170510     C     FUDATT        ANDEQ     'S'
066300170510     C                   Z-ADD     I             W
066400170510     C                   END
066500170510     C                   END
066600170510     C*
066700170510     C* FUNZIONE NON ATTIVA (ERRORE)
066800170510     C     W             IFEQ      0
066900170510     C*
067000170510     C* EMISSIONE MESSAGGIO DI ERRORE
067100170510     C                   MOVEL     MSG(1)        DSMSMS
067200170510     C*
067300170510     C                   END
067400170510     C                   END
067500170510     C*
067600170510     C* FUNZIONE ATTIVA (PROCEDI ...)
067700170510     C     W             IFNE      0
067800170510     C     FUNKEY        OREQ      *BLANKS
067900170510     C*
068000170510     C     FUNKEY        IFEQ      *BLANKS
068100170510     C                   EXSR      ENTGES
068200170510     C                   END
068300170510     C     FUNKEY        IFEQ      '03'
068400170510     C                   EXSR      F03GES
068500170510     C                   END
068600170510     C     FUNKEY        IFEQ      '06'
068700170510     C                   EXSR      F06GES
068800170510     C                   END
068900170510     C     FUNKEY        IFEQ      '12'
069000170510     C                   EXSR      F12GES
069100170510     C                   END
069200170510     C     FUNKEY        IFEQ      '24'
069300170510     C                   EXSR      F24GES
069400170510     C                   END
069500170510     C*
069600170510     C                   END
069700170510     C*
069800170510     C                   ENDSR
069900170510     C*------------------------------------------------------------------------*
070000170510     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
070100170510     C*------------------------------------------------------------------------*
070200170510     C     FUNVAR        BEGSR
070300170510     C*
070400170510     C* INIZIALIZZA VARIABILE -FUNKEY-
070500170510     C*
070600170510     C* FUNKEY=*BLANKS SIGNIFICA ENTER
070700170510     C     KEY           IFEQ      ENTER
070800170510     C                   MOVEL     *BLANKS       FUNKEY
070900170510     C                   END
071000170510     C     KEY           IFEQ      F1
071100170510     C                   MOVEL     '01'          FUNKEY
071200170510     C                   END
071300170510     C     KEY           IFEQ      F2
071400170510     C                   MOVEL     '02'          FUNKEY
071500170510     C                   END
071600170510     C     KEY           IFEQ      F3
071700170510     C                   MOVEL     '03'          FUNKEY
071800170510     C                   END
071900170510     C     KEY           IFEQ      F4
072000170510     C                   MOVEL     '04'          FUNKEY
072100170510     C                   END
072200170510     C     KEY           IFEQ      F5
072300170510     C                   MOVEL     '05'          FUNKEY
072400170510     C                   END
072500170510     C     KEY           IFEQ      F6
072600170510     C                   MOVEL     '06'          FUNKEY
072700170510     C                   END
072800170510     C     KEY           IFEQ      F7
072900170510     C                   MOVEL     '07'          FUNKEY
073000170510     C                   END
073100170510     C     KEY           IFEQ      F8
073200170510     C                   MOVEL     '08'          FUNKEY
073300170510     C                   END
073400170510     C     KEY           IFEQ      F9
073500170510     C                   MOVEL     '09'          FUNKEY
073600170510     C                   END
073700170510     C     KEY           IFEQ      F10
073800170510     C                   MOVEL     '10'          FUNKEY
073900170510     C                   END
074000170510     C     KEY           IFEQ      F11
074100170510     C                   MOVEL     '11'          FUNKEY
074200170510     C                   END
074300170510     C     KEY           IFEQ      F12
074400170510     C                   MOVEL     '12'          FUNKEY
074500170510     C                   END
074600170510     C     KEY           IFEQ      F13
074700170510     C                   MOVEL     '13'          FUNKEY
074800170510     C                   END
074900170510     C     KEY           IFEQ      F14
075000170510     C                   MOVEL     '14'          FUNKEY
075100170510     C                   END
075200170510     C     KEY           IFEQ      F15
075300170510     C                   MOVEL     '15'          FUNKEY
075400170510     C                   END
075500170510     C     KEY           IFEQ      F16
075600170510     C                   MOVEL     '16'          FUNKEY
075700170510     C                   END
075800170510     C     KEY           IFEQ      F17
075900170510     C                   MOVEL     '17'          FUNKEY
076000170510     C                   END
076100170510     C     KEY           IFEQ      F18
076200170510     C                   MOVEL     '18'          FUNKEY
076300170510     C                   END
076400170510     C     KEY           IFEQ      F19
076500170510     C                   MOVEL     '19'          FUNKEY
076600170510     C                   END
076700170510     C     KEY           IFEQ      F20
076800170510     C                   MOVEL     '20'          FUNKEY
076900170510     C                   END
077000170510     C     KEY           IFEQ      F21
077100170510     C                   MOVEL     '21'          FUNKEY
077200170510     C                   END
077300170510     C     KEY           IFEQ      F22
077400170510     C                   MOVEL     '22'          FUNKEY
077500170510     C                   END
077600170510     C     KEY           IFEQ      F23
077700170510     C                   MOVEL     '23'          FUNKEY
077800170510     C                   END
077900170510     C     KEY           IFEQ      F24
078000170510     C                   MOVEL     '24'          FUNKEY
078100170510     C                   END
078200170510     C     KEY           IFEQ      ROLLUP
078300170510     C                   MOVEL     'RU'          FUNKEY
078400170510     C                   END
078500170510     C     KEY           IFEQ      HELP
078600170510     C                   MOVEL     'HP'          FUNKEY
078700170510     C                   END
078800170510     C                   ENDSR
078900170510     C*------------------------------------------------------------------------*
079000170510     C* NOTE PER COMPILAZIONE SCHIERE
079100170510     C*------------------------------------------------------------------------*
079200170510     C* -MSG-
079300170510     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
079400170510     C* (NON TOCCARE MSG,1)
079500170510     C*
079600170510     C* -CMD-
079700170510     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
079800170510     C*
079900170510     C* -FUD-
080000170510     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
080100170510     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
080200170510     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
080300170510     C*  . POSSONO ESSERE IN ORDINE SPARSO
080400170510     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
080500170510     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
080600170510     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
080700170510     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
080800170510     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
080900170510     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
081000170510     C*
081100170510     C*------------------------------------------------------------------------*
081200170510** ======== SCHIERA: MSG  (MESSAGGI)                       ================
081300170510Tasto funzionale non gestito.                                                  1
081400170510Data iniziale maggiore di data finale.                                         2
081500170510Data errata o mancante                                                         3
081600170510Data  "DAL" deve essere Superiore al 10/4/2017 giorno di Partenza procedura    4
081700170510Data  "AL" non deve essere Superiore al :                                      5
081800170510FNLGI2R                                                                        6
081900170510FNLGI2R                                                                        7
082000170510FNLGI2R                                                                        8
082100170510FNLGI2R                                                                        9
082200170510FNLGI2R                                                                        1
082300170510FNLGI2R                                                                        1
082400170510FNLGI2R                                                                        1
082500170510FNLGI2R
082600170510FNLGI2R
082700170510FNLGI2R
082800170510FNLGI2R
082900170510FNLGI2R
083000170510FNLGI2R
083100170510FNLGI2R
083200170510FNLGI2R
083300170510** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
083400170510                   -        (S/N)=N                        funz.01¬
083500170510                   -        (S/N)=N                        funz.02¬
083600170510F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
083700170510                   -        (S/N)=N                        funz.04¬
083800170510                   -        (S/N)=N                        funz.05¬
083900170510F06=Lancio         -        (S/N)=S                        funz.06¬
084000170510                   -        (S/N)=N                        funz.07¬
084100170510                   -        (S/N)=N                        funz.08¬
084200170510                   -        (S/N)=N                        funz.09¬
084300170510                   -        (S/N)=N                        funz.10¬
084400170510                   -        (S/N)=N                        funz.11¬
084500170510F12=Ritorno        -        (S/N)=N                        funz.12¬
084600170510                   -        (S/N)=N                        funz.13¬
084700170510                   -        (S/N)=N                        funz.14¬
084800170510                   -        (S/N)=N                        funz.15¬
084900170510                   -        (S/N)=N                        funz.16¬
085000170510                   -        (S/N)=N                        funz.17¬
085100170510                   -        (S/N)=N                        funz.18¬
085200170510                   -        (S/N)=N                        funz.19¬
085300170510                   -        (S/N)=N                        funz.20¬
085400170510                   -        (S/N)=N                        funz.21¬
085500170510F22=Gestione stampe-        (S/N)=N                        funz.22¬
085600170510                   -        (S/N)=N                        funz.23¬
085700170510F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
