000100970526     H*------------------------------------------------------------------------*
000200030207     H* Gestione anagrafico GABBIE e BANCALI (AZAGB00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800021211     FTISIG2D   CF   E             WORKSTN INFDS(DEVDS1)
000900021211     F                                     SFILE(SIG2DMS:MSGULT)
001000021211     Fazagb01l  UF A E           K DISK
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D*------------
001500970526     D* DECODIFICHE
001600970526     D*------------
001700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* MESSAGGI
002000970526     D*------------
002100970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* DESCRIZIONI TASTI FUNZIONALI
002400970526     D*------------
002500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* FUNZIONI DA CARICARE A VIDEO
002800970526     D*------------
002900970526     D FUV             S             19    DIM(24)
003000970526     D*------------------------------------------------------------------------*
003100970526     D* INPUT
003200970526     D*------------------------------------------------------------------------*
003300970526     D*------------
003400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003500970526     D*------------
003600970526     D FUDDS           DS
003700970526     D  FUDDE1                 1      1
003800970526     D  FUDFUN                 2      3
003900970526     D  FUDDE2                 4     19
004000970526     D  FUDFI0                20     34
004100970526     D  FUDATT                35     35
004200970526     D  FUDFI1                36     80
004300970526     D  FUDDES                 1     19
004400970526     D  FUDRIG                 1     80
004500970526     D*------------
004600970526     D* DS GESTIONE TASTI FUNZIONALI
004700970526     D*------------
004800970526     D DSKEY         E DS
004900970526     D DEVDS1          DS
005000970526     D  KEY                  369    369
005100970526     D*------------
005200970526     D* DS "XMSG" - GESTIONE MESSAGGI
005300970526     D*------------
005400970526     D DSMSG         E DS
005500970528     D*------------------
005600970526     D* ARCHITETTURA
005700970526     D*------------
005800970526     D KPJBA         E DS
005900000407     D*------------------
006000000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
006100000407     D*------------------
006200000407     D WLBDA8          DS                  INZ
006300000407     D  G08DAT                 1      8  0
006400000407     D  G08INV                 9     16  0
006500000407     D  G08ERR                17     17
006600000407     D  G08TGI                18     22  0
006700030123     D*------------
006800030123     D* DS REPERIMENTO DATI UTENTE
006900030123     D*------------
007000030123     D TIBS34DS      E DS                                                       *Profili utente
007100030123     D DDATIUTE      E DS                                                       *Dati utente
007200030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007300030207     D*------------------
007400030207     D* DS REPERIMENTO / CONTROLLO DA TABELLE
007500030207     D*------------------
007600030207     D TIBS02DS      E DS
007700030207     D*------------------
007800030207     D* DS RIDEFINIZIONE TABELLA "TBA"
007900030207     D*------------------
008000030207     D DTBA          E DS
008100000407     D*------------------
008200000407     D* DS PASSAGGIO PARAMETRI
008300000407     D*------------------
008400000407     D PARAM           DS                  INZ
008500000420     D  POP1                   1      3
008600021211     D  PCOD                   4     18
008700021211     D  PMSG                  19     98
008800970526     C*------------------------------------------------------------------------*
008900970526     C* MAIN LINES
009000970526     C*------------------------------------------------------------------------*
009100970526     C*
009200970526     C* NON CI SONO ERRORI IN ENTRATA
009300021211     C     ERRL00        IFEQ      '0'
009400970526     C*
009500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009600021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009900970526     C*
010000970526     C* EMETTE FORMATO VIDEO
010100970526     C                   EXSR      BUTFMR
010200970526     C*
010300970526     C* GESTIONE TASTI FUNZIONALI
010400970526     C                   EXSR      FUNCON
010500021211     C                   ENDDO
010600970526     C*
010700021211     C                   ENDIF
010800021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
010900021211     C                   MOVEL     PARAM         KPJBU
011000970526     C* FINE PGM
011100970526     C                   SETON                                        LR
011200970526     C*------------------------------------------------------------------------*
011300970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011400970526     C*------------------------------------------------------------------------*
011500970526     C     BUTFMR        BEGSR
011600970526     C*
011700970526     C* EMETTE LA TESTATA
011800021211     C                   WRITE     SIG2D01
011900970526     C*
012000970526     C* SPEDISCE EVENTUALE MESSAGGIO
012100970526     C                   EXSR      SNDMSG
012200970526     C*
012300970526     C* EMETTE LE FUNZIONI
012400021211     C                   WRITE     SIG2D02
012500970526     C*
012600970526     C* EMETTE IL VIDEO
012700021211     C                   EXFMT     SIG2D03
012800970526     C*
012900970526     C* REIMPOSTA FLAG DI ERRORE
013000970526     C                   MOVEL     '0'           ERRL00
013100970526     C*
013200970526     C                   ENDSR
013300970526     C*------------------------------------------------------------------------*
013400970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013500970526     C*------------------------------------------------------------------------*
013600970526     C     SNDMSG        BEGSR
013700970526     C*
013800970526     C* PULISCE IL SFILE MESSAGGI
013900970526     C                   MOVEL     '1'           *IN40
014000021211     C                   WRITE     SIG2DMC
014100970526     C                   MOVEL     '0'           *IN40
014200970526     C*
014300970526     C* VISUALIZZA IL MESSAGGIO
014400970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014500970526     C                   CALL      'XMSG'        XMSGPA
014600021211     C     DSMSER        IFEQ      *ZEROS
014700970526     C                   MOVEL     DSMSKE        MSGKEY
014800021211     C                   ENDIF
014900970526     C                   Z-ADD     1             MSGULT
015000021211     C                   WRITE     SIG2DMS
015100021211     C                   WRITE     SIG2DMC
015200970526     C*
015300970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015400970526     C                   MOVEL     *BLANKS       DSMSMS
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* ENTGES - GESTIONE ENTER -> INVIO
015900970526     C*------------------------------------------------------------------------*
016000970526     C     ENTGES        BEGSR
016100970526     C*
016200970526     C* CONTROLLA I CAMPI IMMESSI
016300970526     C                   EXSR      DECODI
016400970526     C*
016500970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
016600021211     C     POP1          IFEQ      'O05'
016700021211     C                   MOVEL     *BLANKS       PMSG
016800970526     C                   MOVEL     '1'           FINL00
016900021211     C                   ENDIF
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* F03GES - GESTIONE F03 -> FINE
017400970526     C*------------------------------------------------------------------------*
017500970526     C     F03GES        BEGSR
017600970526     C*
017700970526     C* IMPOSTA "PREMUTO F03"
017800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F06GES        BEGSR
018500970526     C*
018600021211     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
018700021211     C     POP1          IFEQ      'F06'
018800021211     C                   EXSR      INSERI
018900021211     C                   ENDIF
019000021211     C     POP1          IFEQ      'O02'
019100021211     C                   EXSR      MODIFI
019200021211     C                   ENDIF
019300021211     C     POP1          IFEQ      'O03'
019400021211     C                   EXSR      COPIA
019500021211     C                   ENDIF
019600021211     C     POP1          IFEQ      'O04'
019700021211     C                   EXSR      ANNULL
019800021211     C                   ENDIF
019900021211     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* F12GES - GESTIONE F12 -> RITORNO
020300970526     C*------------------------------------------------------------------------*
020400970526     C     F12GES        BEGSR
020500970526     C*
020600970526     C* IMPOSTA "PREMUTO F12"
020700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F24GES        BEGSR
021400970526     C*
021500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021600970526     C*
021700970526     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
022000970526     C*------------------------------------------------------------------------*
022100970526     C     INSERI        BEGSR
022200970526     C*
022300970526     C* CONTROLLA I CAMPI IMMESSI
022400970526     C                   EXSR      DECODI
022500021211     C                   EXSR      DECODINS
022600970526     C*
022700970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
022800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022900021211     C                   CLEAR                   AZAGB000
023000021211     C                   EXSR      IMPCAM
023100021211     C                   WRITE     AZAGB000
023200000407     C                   MOVEL     MSG(2)        PMSG                           *MESSAGGIO
023300970602     C*
023400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
023500970602     C                   EXSR      AZZVID
023600021211     C                   ENDIF
023700970526     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* MODIFI - OPERAZIONI DI MODIFICA
024100970526     C*------------------------------------------------------------------------*
024200970526     C     MODIFI        BEGSR
024300970526     C*
024400970526     C* CONTROLLA I CAMPI IMMESSI
024500970526     C                   EXSR      DECODI
024600021211     C                   EXSR      DECODINS
024700970526     C*
024800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
024900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025000970619     C                   EXSR      LETCAS
025100970526     C*
025200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
025300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025400000407     C                   EXSR      IMPCAM
025500021211     C                   UPDATE    AZAGB000
025600000407     C                   MOVEL     MSG(3)        PMSG
025700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
025800021211     C                   ENDIF
025900021211     C                   ENDIF
026000970526     C*
026100970526     C                   ENDSR
026200021211     C*------------------------------------------------------------------------*
026300021211     C* COPIA - OPERAZIONI DI COPIA RECORD
026400021211     C*------------------------------------------------------------------------*
026500021211     C     COPIA         BEGSR
026600021211     C*
026700021211     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026800021211     C                   EXSR      LETCAS
026900021211     C*
027000021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027100021211     C*
027200021211     C* CONTROLLA I CAMPI IMMESSI
027300021211     C                   EXSR      DECODI
027400021211     C                   EXSR      DECODINS
027500021211     C*
027600021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027700021211     C                   CLEAR                   AZAGB000
027800021211     C                   EXSR      IMPCAM
027900021211     C                   WRITE     AZAGB000
028000021211     C                   MOVEL     MSG(5)        PMSG                           *MESSAGGIO
028100021211     C                   MOVEL     '1'           FINL00                         *FINE PGM
028200021211     C                   ENDIF
028300021211     C                   ENDIF
028400021211     C*
028500021211     C                   ENDSR
028600970526     C*------------------------------------------------------------------------*
028700970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
028800970526     C*------------------------------------------------------------------------*
028900970526     C     ANNULL        BEGSR
029000000411     C*
029100000411     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
029200000411     C                   EXSR      LETCAS
029300000411     C*
029400000411     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
029500000411     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029600021211     C                   DELETE    AZAGB000
029700000411     C                   MOVEL     MSG(4)        PMSG
029800000411     C                   MOVEL     '1'           FINL00                         *FINE PGM
029900000411     C                   ENDIF
030000000411     C*
030100970526     C                   ENDSR
030200970526     C*------------------------------------------------------------------------*
030300970619     C* LETCAS - LEGGE IL RECORD PERL'AGGIORNAMENTO
030400970526     C*------------------------------------------------------------------------*
030500970619     C     LETCAS        BEGSR
030600970526     C*
030700021211     C     PCOD          CHAIN     AZAGB01L                           99
030800021211     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
030900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
031000000407     C                   MOVEL     MSG(6)        PMSG                           *MESSAGGIO
031100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031200021211     C                   ENDIF
031300970526     C*
031400970526     C                   ENDSR
031500970526     C*------------------------------------------------------------------------*
031600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
031700970526     C*------------------------------------------------------------------------*
031800970526     C     DECODI        BEGSR
031900000407     C*
032000000407     C                   MOVEL     '0'           ERRL00
032100000407     C*
032200000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
032300000407     C                   SETOFF                                       808182
032400030207     C*--------------------
032500030612     C* CONTROLLO ESISTENZA CODICE ARTICOLO IN TABELLA "TBA" (SOLO SE DI TIPO BANCALE)
032600030207     C*--------------------
032700030207     C     ERRL00        IFEQ      '0'
032800030612     C     VIDTIP        IFEQ      'B'
032900030612     C     VIDCOD        ANDNE     *blanks
033000030207     C                   EXSR      CNTCOD
033100030207     C                   ENDIF
033200030207     C                   ENDIF
033300000407     C*--------------------
033400000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
033500000407     C*--------------------
033600000407     C     ERRL00        IFEQ      '0'
033700030207     C     VIDTIP        IFEQ      *blanks
033800000407     C                   MOVEL     '1'           ERRL00
033900000407     C                   SETON                                        80
034000000407     C                   ENDIF
034100021211     C     VIDCOD        IFEQ      *blanks
034200000407     C                   MOVEL     '1'           ERRL00
034300000407     C                   SETON                                        81
034400000407     C                   ENDIF
034500021211     C     VIDDES        IFEQ      *blanks
034600000407     C                   MOVEL     '1'           ERRL00
034700000407     C                   SETON                                        82
034800000407     C                   ENDIF
034900000407     C     ERRL00        IFEQ      '1'
035000000407     C                   MOVEL     MSG(12)       DSMSMS
035100000407     C                   ENDIF
035200000407     C                   ENDIF
035300000407     C*
035400970528     C                   ENDSR
035500021211     C*------------------------------------------------------------------------*
035600021211     C* DECODINS - CONTROLLA CAMPI E DECODIFICA X INSERIMENTO
035700021211     C*------------------------------------------------------------------------*
035800021211     C     DECODINS      BEGSR
035900021211     C*
036000021211     C                   MOVEL     '0'           ERRL00
036100021211     C*
036200021211     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
036300021211     C                   SETOFF                                       808182
036400021211     C*--------------------
036500021211     C* CONTROLLO GIA' ESISTENZA CODICE ARTICOLO
036600021211     C*--------------------
036700021211     C     ERRL00        IFEQ      '0'
036800021211     C     POP1          IFEQ      'F06'
036900021211     C     POP1          OREQ      'O03'
037000021211     C     VIDCOD        CHAIN     AZAGB01L                           99
037100021211     C     *IN99         IFEQ      *OFF
037200021211     C                   MOVEL     '1'           ERRL00
037300021211     C                   SETON                                        81
037400021211     C                   MOVEL     MSG(7)        DSMSMS
037500021211     C                   ENDIF
037600021211     C                   ENDIF
037700021211     C                   ENDIF
037800021211     C*
037900021211     C                   ENDSR
038000030207     C*------------------------------------------------------------------------*
038100030207     C* CNTCOD - CONTROLLA PRESENZA DEL CODICE ARTICOLO IN TABELLA "TBA"
038200030207     C*------------------------------------------------------------------------*
038300030207     C     CNTCOD        BEGSR
038400030207     C*
038500030207     C                   CLEAR                   TIBS02DS
038600030207     C*
038700030207     C     '?'           SCAN      VIDCOD                                 30
038800030207     C*
038900030207     C                   EVAL      T02TLA = 'L'
039000030207     C*  Ricerca
039100030207     C                   IF        *IN30 = *ON
039200030207     C                   MOVEL     'R'           T02MOD
039300030207     C                   ELSE
039400030207     C*  Controllo
039500030207     C                   MOVEL     'C'           T02MOD
039600030207     C                   MOVEL(P)  VIDCOD        T02KE1
039700030207     C                   ENDIF
039800030207     C                   MOVEL     'TBA'         T02COD
039900030207     C                   MOVEL     KNSIF         T02SIF
040000030207     C                   CALL      'TIBS02R'
040100030207     C                   PARM                    KPJBA
040200030207     C                   PARM                    TIBS02DS
040300030207     C*
040400030207     C                   IF        T02ERR = *blanks
040500030207     C                   MOVEL     T02KE1        VIDCOD
040600030207     C                   EVAL      DTBA = T02UNI
040700030207     C   30              EVAL      VIDDES = §TBADESC
040800030207     C                   ELSE
040900030207     C                   MOVEL     '1'           ERRL00
041000030207     C                   SETON                                        81
041100030207     C                   MOVEL     MSG(9)        DSMSMS
041200030207     C                   ENDIF
041300030207     C*
041400030207     C                   ENDSR
041500970526     C*------------------------------------------------------------------------*
041600970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
041700970526     C*------------------------------------------------------------------------*
041800970526     C     IREVID        BEGSR
041900000407     C*
042000021211     C                   MOVEL     AGBTIP        VIDTIP
042100021211     C                   MOVEL     AGBCOD        VIDCOD
042200021211     C                   MOVEL     AGBDES        VIDDES
042300970527     C*
042400970526     C                   ENDSR
042500970526     C*------------------------------------------------------------------------*
042600970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
042700970526     C*------------------------------------------------------------------------*
042800970526     C     AZZVID        BEGSR
042900000407     C*
043000021211     C                   MOVEL     *BLANKS       VIDTIP
043100021211     C                   MOVEL     *BLANKS       VIDCOD
043200021211     C                   MOVEL     *BLANKS       VIDDES
043300970526     C*
043400970526     C                   ENDSR
043500000407     C*------------------------------------------------------------------------*
043600000407     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
043700000407     C*------------------------------------------------------------------------*
043800000407     C     IMPCAM        BEGSR
043900000407     C*
044000021211     C                   MOVEL     VIDTIP        AGBTIP
044100021211     C                   MOVEL     VIDCOD        AGBCOD
044200021211     C                   MOVEL     VIDDES        AGBDES
044300000407     C*
044400000407     C                   ENDSR
044500970526     C*------------------------------------------------------------------------*
044600970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
044700970526     C*------------------------------------------------------------------------*
044800970526     C     IMPFUN        BEGSR
044900970526     C*
045000970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
045100021211     C     POP1          IFEQ      'O05'
045200970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
045300970526     C                   MOVEL     'N'           FUDATT
045400970526     C                   MOVEL     FUDRIG        FUD(6)
045500021211     C                   ENDIF
045600970526     C*
045700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
045800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
045900970526     C                   EXSR      FUNGES
046000970526     C*
046100000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
046200000407     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
046300000407     C                   EXSR      LETCAS
046400980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
046500980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
046600980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
046700000407     C                   ELSE                                                   *INSERIMENTO
046800970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
046900000407     C                   ENDIF
047000970526     C*
047100021211     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
047200021211     C     POP1          IFEQ      'O02'
047300021211     C                   MOVEL     DEC(1)        VIDFUN
047400021211     C                   ENDIF
047500021211     C     POP1          IFEQ      'O03'
047600021211     C                   MOVEL     DEC(5)        VIDFUN
047700021211     C                   ENDIF
047800021211     C     POP1          IFEQ      'O04'
047900021211     C                   MOVEL     DEC(2)        VIDFUN
048000021211     C                   ENDIF
048100021211     C     POP1          IFEQ      'O05'
048200021211     C                   MOVEL     DEC(3)        VIDFUN
048300021211     C                   ENDIF
048400021211     C     POP1          IFEQ      'F06'
048500021211     C                   MOVEL     DEC(4)        VIDFUN
048600021211     C                   ENDIF
048700021211     C*
048800021211     C                   ENDSR
048900030123     C*--------------------------------------------------------------------------------------------*
049000030123     C* REPERISCE I DATI UTENTE
049100030123     C*--------------------------------------------------------------------------------------------*
049200030123     C     REPDATIUTE    BEGSR
049300030123     C*
049400030123     C* INIZIALIZZA VARIABILI DI WRK
049500030123     C                   CLEAR                   TIBS34DS
049600030123     C                   CLEAR                   AZUTEDS
049700030123     C                   CLEAR                   DDATIUTE
049800030123     C*
049900030123     C     *DTAARA       DEFINE    §azute        azuteds
050000030123     C     *DTAARA       DEFINE    §datiute      ddatiute
050100030123     C                   IN(E)     *DTAARA
050200030123     C                   IF        %Error
050300030123     C                   EVAL      I34Tla = 'L'
050400030123     C                   CALL      'TIBS34R'
050500030123     C                   PARM                    Tibs34Ds
050600030123     C                   IN        *DTAARA
050700030123     C                   ENDIF
050800030123     C*
050900030123     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
051000030123     C                   MOVEL(P)  RSUT          DSFIRS           20
051100030123     C*
051200030123     C                   ENDSR
051300970526     C*------------------------------------------------------------------------*
051400970526     C* *INZSR - OPERAZIONI INIZIALI
051500970526     C*------------------------------------------------------------------------*
051600970526     C     *INZSR        BEGSR
051700970526     C*---
051800970526     C* RICEVIMENTO PARAMETRI
051900970526     C*---
052000970526     C     *ENTRY        PLIST
052100970612     C                   PARM                    KPJBA
052200000407     C                   MOVEL     KPJBU         PARAM
052300000407     C*
052400000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
052500000407     C     POP1          IFEQ      'O05'
052600000407     C                   SETON                                        90
052700000407     C                   ENDIF
052800000407     C     POP1          IFEQ      'O02'
052900000407     C                   SETON                                        91
053000000407     C                   ENDIF
053100030123     C*
053200030123     C* REPERIMENTO DATI UTENTE
053300030123     C                   EXSR      REPDATIUTE
053400000407     C*
053500000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
053600000407     C                   SETOFF                                       808182
053700970526     C*
053800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
053900970526     C     XMSGPA        PLIST
054000970526     C                   PARM                    DSMSMS
054100970526     C                   PARM                    DSMSPG
054200970526     C                   PARM                    DSMSKE
054300970526     C                   PARM                    DSMSER
054400970526     C*
054500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
054600970526     C                   CALL      'XKEY'
054700970526     C                   PARM                    DSKEY
054800970526     C*---
054900970526     C* VARIABILI NON RIFERITE AL DATA BASE
055000970526     C*---
055100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
055200970526     C*---
055300970526     C* VARIABILI DI CONTROLLO
055400970526     C*---
055500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
055600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
055800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
055900970526     C*
056000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
056100021211     C                   MOVEL     'TISIG2R'     DSMSPG
056200021211     C                   MOVEL     'TISIG2R'     MSGQUE
056300970526     C                   MOVEL     *BLANKS       MSGKEY
056400970526     C                   MOVEL     *BLANKS       DSMSMS
056500000407     C*
056600000407     C* CALCOLA LA DATA CORRENTE
056700000407     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
056800000407     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
056900000407     C                   Z-ADD     WN8           G08DAT
057000000407     C                   Z-ADD     *ZEROS        G08INV
057100000407     C                   MOVEL     '0'           G08ERR
057200000407     C                   CALL      'XSRDA8'
057300000407     C                   PARM                    WLBDA8
057400000407     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
057500970526     C*
057600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
057700970526     C                   EXSR      IMPFUN
057800970526     C*
057900970526     C                   ENDSR
058000970526     C*------------------------------------------------------------------------*
058100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
058200970526     C*------------------------------------------------------------------------*
058300970526     C     FUNGES        BEGSR
058400970526     C*
058500970526     C* PULISCO CAMPI DI LAVORO
058600970526     C                   Z-ADD     0             W                 5 0
058700970526     C*
058800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
058900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
059000970526     C     1             DO        24            I
059100970526     C                   MOVEL     FUD(I)        FUDRIG
059200970526     C*
059300970526     C* PULIZIA FUNZIONI PRECEDENTI
059400970526     C                   MOVEL     *BLANKS       FUV(I)
059500970526     C*
059600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
059700970526     C* (SALTA F24)
059800970526     C     FUDATT        IFEQ      'S'
059900970526     C     FUDFUN        ANDNE     '24'
060000970526     C                   ADD       1             W
060100970526     C                   MOVEL     FUDDES        FUV(W)
060200970526     C                   END
060300970526     C                   END                                                    DO WHILE
060400970526     C*
060500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
060600970526     C* ALTRIMENTI DISATTIVO F24
060700970526     C     1             DO        24            I
060800970526     C                   MOVEL     FUD(I)        FUDRIG
060900970526     C*
061000970526     C     FUDFUN        IFEQ      '24'
061100970526     C     W             IFGT      6
061200970526     C                   MOVEL     'S'           FUDATT
061300970526     C                   ELSE
061400970526     C                   MOVEL     *BLANKS       FUDATT
061500970526     C                   END                                                    W GT 8
061600970526     C                   MOVEL     FUDRIG        FUD(I)
061700970526     C                   END
061800970526     C                   END                                                    DO WHILE
061900970526     C*
062000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
062100970526     C     W             DIV       6             I
062200970526     C                   MVR                     Z
062300970526     C*
062400970526     C     Z             IFNE      0
062500970526     C     I             ADD       1             Z
062600970526     C                   ELSE
062700970526     C                   Z-ADD     I             Z                 5 0
062800970526     C                   END
062900970526     C*
063000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
063100970526     C                   ADD       1             FUNGRU
063200970526     C     FUNGRU        IFGT      Z
063300970526     C     FUNGRU        ORLT      1
063400970526     C                   Z-ADD     1             FUNGRU
063500970526     C                   END
063600970526     C*
063700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
063800970526     C     FUNGRU        IFEQ      1
063900970526     C                   MOVEL     FUV(1)        FUN001
064000970526     C                   MOVEL     FUV(2)        FUN002
064100970526     C                   MOVEL     FUV(3)        FUN003
064200970526     C                   MOVEL     FUV(4)        FUN004
064300970526     C                   MOVEL     FUV(5)        FUN005
064400970526     C                   MOVEL     FUV(6)        FUN006
064500970526     C                   END
064600970526     C*
064700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
064800970526     C     FUNGRU        IFEQ      2
064900970526     C                   MOVEL     FUV(7)        FUN001
065000970526     C                   MOVEL     FUV(8)        FUN002
065100970526     C                   MOVEL     FUV(9)        FUN003
065200970526     C                   MOVEL     FUV(10)       FUN004
065300970526     C                   MOVEL     FUV(11)       FUN005
065400970526     C                   MOVEL     FUV(12)       FUN006
065500970526     C                   END
065600970526     C*
065700970526     C* TERZO GRUPPO DI 6 FUNZIONI
065800970526     C     FUNGRU        IFEQ      3
065900970526     C                   MOVEL     FUV(13)       FUN001
066000970526     C                   MOVEL     FUV(14)       FUN002
066100970526     C                   MOVEL     FUV(15)       FUN003
066200970526     C                   MOVEL     FUV(16)       FUN004
066300970526     C                   MOVEL     FUV(17)       FUN005
066400970526     C                   MOVEL     FUV(18)       FUN006
066500970526     C                   END
066600970526     C*
066700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
066800970526     C     FUNGRU        IFEQ      4
066900970526     C                   MOVEL     FUV(19)       FUN001
067000970526     C                   MOVEL     FUV(20)       FUN002
067100970526     C                   MOVEL     FUV(21)       FUN003
067200970526     C                   MOVEL     FUV(22)       FUN004
067300970526     C                   MOVEL     *BLANKS       FUN005
067400970526     C                   MOVEL     *BLANKS       FUN006
067500970526     C                   END
067600970526     C*
067700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
067800970526     C                   MOVEL     *BLANKS       FUN007
067900970526     C                   MOVEL     *BLANKS       FUN008
068000970526     C*
068100970526     C     1             DO        24            I
068200970526     C                   MOVEL     FUD(I)        FUDRIG
068300970526     C     FUDFUN        IFEQ      '24'
068400970526     C     FUDATT        ANDEQ     'S'
068500970526     C                   MOVEL     FUDDES        FUN008
068600970526     C                   END
068700970526     C                   END                                                    DO
068800970526     C*
068900970526     C                   ENDSR
069000970526     C*------------------------------------------------------------------------*
069100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
069200970526     C*------------------------------------------------------------------------*
069300970526     C     FUNCON        BEGSR
069400970526     C*
069500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
069600970526     C                   EXSR      FUNVAR
069700970526     C*
069800970526     C* PULISCO CAMPI DI LAVORO
069900970526     C                   Z-ADD     0             I                 5 0
070000970526     C                   Z-ADD     0             W                 5 0
070100970526     C*
070200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
070300970526     C     FUNKEY        IFNE      *BLANKS
070400970526     C     1             DO        24            I
070500970526     C                   MOVEL     FUD(I)        FUDRIG
070600970526     C*
070700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
070800970526     C     FUNKEY        IFEQ      FUDFUN
070900970526     C     FUDATT        ANDEQ     'S'
071000970526     C                   Z-ADD     I             W
071100970526     C                   END
071200970526     C                   END                                                    DO WHILE
071300970526     C*
071400970526     C* FUNZIONE NON ATTIVA (ERRORE)
071500970526     C     W             IFEQ      0
071600970526     C*
071700970526     C* EMISSIONE MESSAGGIO DI ERRORE
071800970526     C                   MOVEL     MSG(1)        DSMSMS
071900970526     C*
072000970526     C                   END                                                    W=0
072100970526     C                   END                                                    FUNKEY NE *BLANKS
072200970526     C*
072300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
072400970526     C     W             IFNE      0
072500970526     C     FUNKEY        OREQ      *BLANKS
072600970526     C     FUNKEY        IFEQ      *BLANKS
072700970526     C                   EXSR      ENTGES
072800970526     C                   END
072900970526     C     FUNKEY        IFEQ      '03'
073000970526     C                   EXSR      F03GES
073100970526     C                   END
073200970526     C     FUNKEY        IFEQ      '06'
073300970526     C                   EXSR      F06GES
073400970526     C                   END
073500970526     C     FUNKEY        IFEQ      '12'
073600970526     C                   EXSR      F12GES
073700970526     C                   END
073800970526     C     FUNKEY        IFEQ      '24'
073900970526     C                   EXSR      F24GES
074000970526     C                   END
074100970526     C                   END
074200970526     C*
074300970526     C                   ENDSR
074400970526     C*------------------------------------------------------------------------*
074500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
074600970526     C*------------------------------------------------------------------------*
074700970526     C     FUNVAR        BEGSR
074800970526     C*
074900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
075000970526     C*
075100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
075200970526     C     KEY           IFEQ      ENTER
075300970526     C                   MOVEL     *BLANKS       FUNKEY
075400970526     C                   END
075500970526     C     KEY           IFEQ      F1
075600970526     C                   MOVEL     '01'          FUNKEY
075700970526     C                   END
075800970526     C     KEY           IFEQ      F2
075900970526     C                   MOVEL     '02'          FUNKEY
076000970526     C                   END
076100970526     C     KEY           IFEQ      F3
076200970526     C                   MOVEL     '03'          FUNKEY
076300970526     C                   END
076400970526     C     KEY           IFEQ      F4
076500970526     C                   MOVEL     '04'          FUNKEY
076600970526     C                   END
076700970526     C     KEY           IFEQ      F5
076800970526     C                   MOVEL     '05'          FUNKEY
076900970526     C                   END
077000970526     C     KEY           IFEQ      F6
077100970526     C                   MOVEL     '06'          FUNKEY
077200970526     C                   END
077300970526     C     KEY           IFEQ      F7
077400970526     C                   MOVEL     '07'          FUNKEY
077500970526     C                   END
077600970526     C     KEY           IFEQ      F8
077700970526     C                   MOVEL     '08'          FUNKEY
077800970526     C                   END
077900970526     C     KEY           IFEQ      F9
078000970526     C                   MOVEL     '09'          FUNKEY
078100970526     C                   END
078200970526     C     KEY           IFEQ      F10
078300970526     C                   MOVEL     '10'          FUNKEY
078400970526     C                   END
078500970526     C     KEY           IFEQ      F11
078600970526     C                   MOVEL     '11'          FUNKEY
078700970526     C                   END
078800970526     C     KEY           IFEQ      F12
078900970526     C                   MOVEL     '12'          FUNKEY
079000970526     C                   END
079100970526     C     KEY           IFEQ      F13
079200970526     C                   MOVEL     '13'          FUNKEY
079300970526     C                   END
079400970526     C     KEY           IFEQ      F14
079500970526     C                   MOVEL     '14'          FUNKEY
079600970526     C                   END
079700970526     C     KEY           IFEQ      F15
079800970526     C                   MOVEL     '15'          FUNKEY
079900970526     C                   END
080000970526     C     KEY           IFEQ      F16
080100970526     C                   MOVEL     '16'          FUNKEY
080200970526     C                   END
080300970526     C     KEY           IFEQ      F17
080400970526     C                   MOVEL     '17'          FUNKEY
080500970526     C                   END
080600970526     C     KEY           IFEQ      F18
080700970526     C                   MOVEL     '18'          FUNKEY
080800970526     C                   END
080900970526     C     KEY           IFEQ      F19
081000970526     C                   MOVEL     '19'          FUNKEY
081100970526     C                   END
081200970526     C     KEY           IFEQ      F20
081300970526     C                   MOVEL     '20'          FUNKEY
081400970526     C                   END
081500970526     C     KEY           IFEQ      F21
081600970526     C                   MOVEL     '21'          FUNKEY
081700970526     C                   END
081800970526     C     KEY           IFEQ      F22
081900970526     C                   MOVEL     '22'          FUNKEY
082000970526     C                   END
082100970526     C     KEY           IFEQ      F23
082200970526     C                   MOVEL     '23'          FUNKEY
082300970526     C                   END
082400970526     C     KEY           IFEQ      F24
082500970526     C                   MOVEL     '24'          FUNKEY
082600970526     C                   END
082700970526     C     KEY           IFEQ      ROLLUP
082800970526     C                   MOVEL     'RU'          FUNKEY
082900970526     C                   END
083000970526     C     KEY           IFEQ      HELP
083100970526     C                   MOVEL     'HP'          FUNKEY
083200970526     C                   END
083300970526     C                   ENDSR
083400970526     C*------------------------------------------------------------------------*
083500970526     C* NOTE COSTRUZIONE SCHIERE
083600970526     C*------------------------------------------------------------------------*
083700970526     C*
083800970526     C* -MSG-MESSAGGI
083900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
084000970526     C* (NON TOCCARE MSG,1 E MSG,2)
084100970526     C*
084200970526     C* -CMD-COMANDI
084300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
084400970526     C*
084500970526     C* -OPD-OPZIONI
084600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
084700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
084800970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
084900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
085000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
085100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
085200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
085300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
085400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
085500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
085600970526     C*
085700970526     C* -FUD-FUNZIONI
085800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
085900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
086000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
086100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
086200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
086300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
086400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
086500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
086600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
086700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
086800970526     O*------------------------------------------------------------------------*
086900970526** DEC - DECODIFICHE
087000970526    MODIFICA                                                      1
087100970526  ANNULLAMENTO                                                    2
087200970526VISUALIZZAZIONE                                                   3
087300021211  INSERIMENTO                                                     4
087400021211     COPIA                                                        5
087500970528** MSG - MESSAGGI
087600021211TISIG2R- Tasto funzionale non gestito                                          1
087700021211TISIG2R- Inserimento effettuato                                                2
087800021211TISIG2R- Modifica effettuata                                                   3
087900021211TISIG2R- Annullamento effettuato                                               4
088000021211TISIG2R- Copia effettuata                                                      5
088100021211TISIG2R- Richiesta funzione per record inesistente                             6
088200021211TISIG2R- Impossibile inserire, codice articolo già presente.                   7
088300021211TISIG2R- Richiesta funzione per record esistente                               8
088400030207TISIG2R- Codice articolo non presente in tabella TBA                           9
088500021211TISIG2R- 10                                                                    10
088600021211TISIG2R- 11                                                                    11
088700021211TISIG2R- Tutti i campi sono obbligatori                                        12
088800021211TISIG2R- 13                                                                    13
088900021211TISIG2R- 14                                                                    14
089000021211TISIG2R- 15                                                                    15
089100021211TISIG2R- 16                                                                    16
089200021211TISIG2R- 17                                                                    17
08930002121118                                                                             18
08940002121119                                                                             19
08950002121120                                                                             20
089600970526** FUD - FUNZIONI
089700970526                   -        (S/N)=N                        funz.01¬
089800970526                   -        (S/N)=N                        funz.02¬
089900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
090000970526                   -        (S/N)=N                        funz.04¬
090100970526                   -        (S/N)=N                        funz.05¬
090200000407F06=Conferma       -        (S/N)=S                        funz.06¬
090300970527                   -        (S/N)=N                        funz.07¬
090400970527                   -        (S/N)=N                        funz.08¬
090500970527                   -        (S/N)=N                        funz.09¬
090600970527                   -        (S/N)=N                        funz.10¬
090700970526                   -        (S/N)=N                        funz.11¬
090800970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
090900970526                   -        (S/N)=N                        funz.13¬
091000970526                   -        (S/N)=N                        funz.14¬
091100970526                   -        (S/N)=N                        funz.15¬
091200970526                   -        (S/N)=N                        funz.16¬
091300970526                   -        (S/N)=N                        funz.17¬
091400970526                   -        (S/N)=N                        funz.18¬
091500970526                   -        (S/N)=N                        funz.19¬
091600970526                   -        (S/N)=N                        funz.20¬
091700970526                   -        (S/N)=N                        funz.21¬
091800970526                   -        (S/N)=N                        funz.22¬
091900970526                   -        (S/N)=N                        funz.23¬
092000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
