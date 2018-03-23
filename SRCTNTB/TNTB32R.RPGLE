000100970526     H*------------------------------------------------------------------------*
000200030122     H* Gestione nuove tabelle: Tabella "GGB"
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030122     FTNTB32D   CF   E             WORKSTN INFDS(DEVDS1)
000900030122     F                                     SFILE(TB32DMS:MSGULT)
001000030122     Ftntbe01l  UF A E           K DISK
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
005500030122     D*------------
005600970526     D* ARCHITETTURA
005700970526     D*------------
005800970526     D KPJBA         E DS
005900030122     D*------------
006000030122     D* DS RIDEFINIZIONE NUOVA TABELLA "DGGB"
006100030122     D*------------
006200030122     D DGGB          E DS
006300030123     D*------------
006400030123     D* DS REPERIMENTO DATI UTENTE
006500030123     D*------------
006600030123     D TIBS34DS      E DS                                                       *Profili utente
006700030123     D DDATIUTE      E DS                                                       *Dati utente
006800030123     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900000407     D*------------------
007000000407     D* DS PASSAGGIO PARAMETRI
007100000407     D*------------------
007200000407     D PARAM           DS                  INZ
007300030123     D  POP1                   1      3
007400030123     D  PCAU                   4      7
007500030123     D  PDES                   8     42
007600030123     D  PMSG                  43    122
007700970526     C*------------------------------------------------------------------------*
007800970526     C* MAIN LINES
007900970526     C*------------------------------------------------------------------------*
008000970526     C*
008100970526     C* NON CI SONO ERRORI IN ENTRATA
008200021211     C     ERRL00        IFEQ      '0'
008300970526     C*
008400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008500021211     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
008800970526     C*
008900970526     C* EMETTE FORMATO VIDEO
009000970526     C                   EXSR      BUTFMR
009100970526     C*
009200970526     C* GESTIONE TASTI FUNZIONALI
009300970526     C                   EXSR      FUNCON
009400021211     C                   ENDDO
009500970526     C*
009600021211     C                   ENDIF
009700021211     C* AGGIORNO LA KPJBU X IL PASSAGGIO PARAMETRI
009800021211     C                   MOVEL     PARAM         KPJBU
009900970526     C* FINE PGM
010000970526     C                   SETON                                        LR
010100970526     C*------------------------------------------------------------------------*
010200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010300970526     C*------------------------------------------------------------------------*
010400970526     C     BUTFMR        BEGSR
010500970526     C*
010600970526     C* EMETTE LA TESTATA
010700030122     C                   WRITE     TB32D01
010800970526     C*
010900970526     C* SPEDISCE EVENTUALE MESSAGGIO
011000970526     C                   EXSR      SNDMSG
011100970526     C*
011200970526     C* EMETTE LE FUNZIONI
011300030122     C                   WRITE     TB32D02
011400970526     C*
011500970526     C* EMETTE IL VIDEO
011600030122     C                   EXFMT     TB32D03
011700970526     C*
011800970526     C* REIMPOSTA FLAG DI ERRORE
011900970526     C                   MOVEL     '0'           ERRL00
012000970526     C*
012100970526     C                   ENDSR
012200970526     C*------------------------------------------------------------------------*
012300970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
012400970526     C*------------------------------------------------------------------------*
012500970526     C     SNDMSG        BEGSR
012600970526     C*
012700970526     C* PULISCE IL SFILE MESSAGGI
012800970526     C                   MOVEL     '1'           *IN40
012900030122     C                   WRITE     TB32DMC
013000970526     C                   MOVEL     '0'           *IN40
013100970526     C*
013200970526     C* VISUALIZZA IL MESSAGGIO
013300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
013400970526     C                   CALL      'XMSG'        XMSGPA
013500021211     C     DSMSER        IFEQ      *ZEROS
013600970526     C                   MOVEL     DSMSKE        MSGKEY
013700021211     C                   ENDIF
013800970526     C                   Z-ADD     1             MSGULT
013900030122     C                   WRITE     TB32DMS
014000030122     C                   WRITE     TB32DMC
014100970526     C*
014200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
014300970526     C                   MOVEL     *BLANKS       DSMSMS
014400970526     C*
014500970526     C                   ENDSR
014600970526     C*------------------------------------------------------------------------*
014700970526     C* ENTGES - GESTIONE ENTER -> INVIO
014800970526     C*------------------------------------------------------------------------*
014900970526     C     ENTGES        BEGSR
015000970526     C*
015100970526     C* CONTROLLA I CAMPI IMMESSI
015200970526     C                   EXSR      DECODI
015300970526     C*
015400970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
015500021211     C     POP1          IFEQ      'O05'
015600021211     C                   MOVEL     *BLANKS       PMSG
015700970526     C                   MOVEL     '1'           FINL00
015800021211     C                   ENDIF
015900970526     C*
016000970526     C                   ENDSR
016100970526     C*------------------------------------------------------------------------*
016200970526     C* F03GES - GESTIONE F03 -> FINE
016300970526     C*------------------------------------------------------------------------*
016400970526     C     F03GES        BEGSR
016500970526     C*
016600970526     C* IMPOSTA "PREMUTO F03"
016700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016800970526     C*
016900970526     C                   ENDSR
017000970526     C*------------------------------------------------------------------------*
017100970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
017200970526     C*------------------------------------------------------------------------*
017300970526     C     F06GES        BEGSR
017400970526     C*
017500021211     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
017600021211     C     POP1          IFEQ      'F06'
017700021211     C                   EXSR      INSERI
017800021211     C                   ENDIF
017900021211     C     POP1          IFEQ      'O02'
018000021211     C                   EXSR      MODIFI
018100021211     C                   ENDIF
018200021211     C     POP1          IFEQ      'O04'
018300021211     C                   EXSR      ANNULL
018400021211     C                   ENDIF
018500030124     C     POP1          IFEQ      'O07'
018600030124     C                   EXSR      RIPRI
018700030124     C                   ENDIF
018800021211     C*
018900970526     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100970526     C* F12GES - GESTIONE F12 -> RITORNO
019200970526     C*------------------------------------------------------------------------*
019300970526     C     F12GES        BEGSR
019400970526     C*
019500970526     C* IMPOSTA "PREMUTO F12"
019600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019700970526     C*
019800970526     C                   ENDSR
019900970526     C*------------------------------------------------------------------------*
020000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020100970526     C*------------------------------------------------------------------------*
020200970526     C     F24GES        BEGSR
020300970526     C*
020400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
020500970526     C*
020600970526     C                   ENDSR
020700970526     C*------------------------------------------------------------------------*
020800970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
020900970526     C*------------------------------------------------------------------------*
021000970526     C     INSERI        BEGSR
021100970526     C*
021200970526     C* CONTROLLA I CAMPI IMMESSI
021300970526     C                   EXSR      DECODI
021400970526     C*
021500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
021600021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021700030122     C                   CLEAR                   DGGB
021800030122     C                   CLEAR                   TNTBE000
021900021211     C                   EXSR      IMPCAM
022000030123     C                   MOVEL(P)  'GGB'         tbeCOD
022100030128     C                   MOVEL(P)  'GG'          tbeAPL
022200030123     C                   MOVEL(P)  VIDCAU        tbeKE1
022300030123     C                   MOVEL(P)  DGGB          tbeUNI
022400030122     C                   WRITE     TNTBE000
022500000407     C                   MOVEL     MSG(2)        PMSG                           *MESSAGGIO
022600970602     C*
022700970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
022800970602     C                   EXSR      AZZVID
022900021211     C                   ENDIF
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* MODIFI - OPERAZIONI DI MODIFICA
023400970526     C*------------------------------------------------------------------------*
023500970526     C     MODIFI        BEGSR
023600970526     C*
023700970526     C* CONTROLLA I CAMPI IMMESSI
023800970526     C                   EXSR      DECODI
023900970526     C*
024000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
024100021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024200970619     C                   EXSR      LETCAS
024300970526     C*
024400970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024600000407     C                   EXSR      IMPCAM
024700030123     C                   MOVEL(P)  DGGB          tbeUNI
024800030123     C                   MOVEL     *blanks       tbeFTR
024900030122     C                   UPDATE    TNTBE000
025000000407     C                   MOVEL     MSG(3)        PMSG
025100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
025200021211     C                   ENDIF
025300021211     C                   ENDIF
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
025800970526     C*------------------------------------------------------------------------*
025900970526     C     ANNULL        BEGSR
026000000411     C*
026100000411     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
026200000411     C                   EXSR      LETCAS
026300000411     C*
026400000411     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026500000411     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026600030122     C                   MOVEL     'A'           tbeATB
026700030122     C                   MOVEL     *blanks       tbeFTR
026800030122     C                   UPDATE    TNTBE000
026900000411     C                   MOVEL     MSG(4)        PMSG
027000000411     C                   MOVEL     '1'           FINL00                         *FINE PGM
027100000411     C                   ENDIF
027200000411     C*
027300970526     C                   ENDSR
027400030124     C*------------------------------------------------------------------------*
027500030124     C* RIPRI - OPERAZIONI DI RIPRISTINO
027600030124     C*------------------------------------------------------------------------*
027700030124     C     RIPRI         BEGSR
027800030124     C*
027900030124     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
028000030124     C                   EXSR      LETCAS
028100030124     C*
028200030124     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028300030124     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028400030124     C                   MOVEL     ' '           tbeATB
028500030124     C                   MOVEL     *blanks       tbeFTR
028600030124     C                   UPDATE    TNTBE000
028700030124     C                   MOVEL     MSG(10)       PMSG
028800030124     C                   MOVEL     '1'           FINL00                         *FINE PGM
028900030124     C                   ENDIF
029000030124     C*
029100030124     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970619     C* LETCAS - LEGGE IL RECORD PERL'AGGIORNAMENTO
029400970526     C*------------------------------------------------------------------------*
029500970619     C     LETCAS        BEGSR
029600970526     C*
029700030122     C                   MOVEL     'GGB'         tbeCOD
029800030122     C                   MOVEL     PCAU          tbeKE1
029900030122     C*
030000030122     C     KEYTBE        CHAIN     TNTBE01L                           99
030100021211     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
030200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
030300000407     C                   MOVEL     MSG(6)        PMSG                           *MESSAGGIO
030400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
030500030122     C                   ELSE
030600030122     C                   EVAL      DGGB = tbeUNI
030700021211     C                   ENDIF
030800970526     C*
030900970526     C                   ENDSR
031000970526     C*------------------------------------------------------------------------*
031100970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
031200970526     C*------------------------------------------------------------------------*
031300970526     C     DECODI        BEGSR
031400000407     C*
031500000407     C                   MOVEL     '0'           ERRL00
031600000407     C*
031700000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
031800000407     C                   SETOFF                                       808182
031900030122     C                   SETOFF                                       838485
032000150216     C                   SETOFF                                       868788
032001150223     C                   SETOFF                                       89
032100030122     C*--------------------
032200030122     C* SE INDICATA CONTROLLO ESISTENZA CUSALE CORRELATA
032300030122     C*--------------------
032400030122     C                   IF        VIDCAC <> *blanks
032500030122     C                   MOVEL     'GGB'         tbeCOD
032600030122     C                   MOVEL     VIDCAC        tbeKE1
032700030122     C*
032800030122     C     KEYTBE        CHAIN     TNTBE01L                           99
032900030122     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
033000030122     C                   MOVEL     '1'           ERRL00                         *ERRORE
033100030122     C                   MOVEL     MSG(9)        DSMSMS                         *MESSAGGIO
033200030122     C                   SETON                                        86
033300030122     C                   ENDIF
033400030122     C                   ENDIF
033500000407     C*--------------------
033600000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
033700000407     C*--------------------
033800000407     C     ERRL00        IFEQ      '0'
033900030122     C     VIDTIM        IFEQ      *blanks
034000000407     C                   MOVEL     '1'           ERRL00
034100000407     C                   SETON                                        80
034200000407     C                   ENDIF
034300030122     C     VIDCAU        IFEQ      *blanks
034400000407     C                   MOVEL     '1'           ERRL00
034500000407     C                   SETON                                        81
034600000407     C                   ENDIF
034700021211     C     VIDDES        IFEQ      *blanks
034800000407     C                   MOVEL     '1'           ERRL00
034900000407     C                   SETON                                        82
035000000407     C                   ENDIF
035100030122     C     VIDSEG        IFEQ      *blanks
035200030128     C     VIDTIM        ANDNE     'S'
035300030122     C                   MOVEL     '1'           ERRL00
035400030122     C                   SETON                                        83
035500030122     C                   ENDIF
035600150210     C     VIDTIM        IFEQ      'M'
035700150210     C     VIDaut        ANDEQ     ' '
035800150210     C                   MOVEL     '1'           ERRL00
035900150210     C                   SETON                                        84
036000150210     C                   ENDIF
036100030122     C     VIDTIP        IFEQ      *blanks
036200030122     C                   MOVEL     '1'           ERRL00
036300030122     C                   SETON                                        87
036400030122     C                   ENDIF
036500000407     C     ERRL00        IFEQ      '1'
036600000407     C                   MOVEL     MSG(12)       DSMSMS
036700000407     C                   ENDIF
036800000407     C                   ENDIF
036801150223     c* % franchigia solo per clienti positivo
036802150223     C     ERRL00        IFEQ      '0'
036803150223     c     vidsco        andne     ' '
036804150223     C     VIDsco        IFEQ      'S'
036805150223     C     VIDTIP        andeq     'C'
036806150223     C     VIDseg        andeq     'E'
036807150223     C     VIDtim        andeq     'M'
036808150223     c                   else
036809150223     C                   SETON                                        89
036810150223     C                   MOVEL     MSG(11)       DSMSMS
036811150223     c                   end
036812150223     C                   ENDIF
036901150223     C*
037000970528     C                   ENDSR
037100970526     C*------------------------------------------------------------------------*
037200970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
037300970526     C*------------------------------------------------------------------------*
037400970526     C     IREVID        BEGSR
037500000407     C*
037600150304     C                   EVAL      VIDtsc = §GGBtsc
037700150216     C                   EVAL      VIDTIM = §GGBTIM
037800030122     C                   EVAL      VIDCAU = tbeKE1
037900030122     C                   EVAL      VIDDES = §GGBDES
038000030122     C                   EVAL      VIDSEG = §GGBSEG
038100030122     C                   EVAL      VIDAUT = §GGBAUT
038200150210     C                   EVAL      VIDLs01 = §GGBLs01
038201150223     C                   EVAL      VIDSCO = §GGBSCO
038300030122     C                   EVAL      VIDCAC = §GGBCAC
038400030122     C                   EVAL      VIDTIP = §GGBTIP
038500970527     C*
038600970526     C                   ENDSR
038700970526     C*------------------------------------------------------------------------*
038800970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
038900970526     C*------------------------------------------------------------------------*
039000970526     C     AZZVID        BEGSR
039100000407     C*
039200150304     C                   EVAL      VIDtsc = *blanks
039300150216     C                   EVAL      VIDTIM = *blanks
039400030122     C                   EVAL      VIDCAU = *blanks
039500030122     C                   EVAL      VIDDES = *blanks
039600030122     C                   EVAL      VIDSEG = *blanks
039700030122     C                   EVAL      VIDAUT = *blanks
039800150210     C                   EVAL      VIDLs01= *blanks
039801150223     C                   EVAL      VIDsco = *blanks
039900030122     C                   EVAL      VIDCAC = *blanks
040000030122     C                   EVAL      VIDTIP = *blanks
040100030122     C*
040200970526     C                   ENDSR
040300000407     C*------------------------------------------------------------------------*
040400000407     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
040500000407     C*------------------------------------------------------------------------*
040600000407     C     IMPCAM        BEGSR
040700000407     C*
040800150304     C                   EVAL      §GGBtsc = VIDtsc
040900150216     C                   EVAL      §GGBTIM = VIDTIM
041000030122     C                   EVAL      tbeKE1  = VIDCAU
041100030122     C                   EVAL      §GGBDES = VIDDES
041200030122     C                   EVAL      §GGBSEG = VIDSEG
041300030122     C                   EVAL      §GGBAUT = VIDAUT
041400150210     C                   EVAL      §GGBLs01= VIDLs01
041401150223     C                   EVAL      §GGBsco = VIDsco
041500030122     C                   EVAL      §GGBCAC = VIDCAC
041600030122     C                   EVAL      §GGBTIP = VIDTIP
041700000407     C*
041800000407     C                   ENDSR
041900970526     C*------------------------------------------------------------------------*
042000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
042100970526     C*------------------------------------------------------------------------*
042200970526     C     IMPFUN        BEGSR
042300970526     C*
042400970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
042500021211     C     POP1          IFEQ      'O05'
042600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
042700970526     C                   MOVEL     'N'           FUDATT
042800970526     C                   MOVEL     FUDRIG        FUD(6)
042900021211     C                   ENDIF
043000970526     C*
043100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
043200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
043300970526     C                   EXSR      FUNGES
043400970526     C*
043500000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
043600000407     C     POP1          IFNE      'F06'                                        *NO INSERIMENTO
043700000407     C                   EXSR      LETCAS
043800980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
043900980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
044000980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
044100000407     C                   ELSE                                                   *INSERIMENTO
044200970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
044300000407     C                   ENDIF
044400970526     C*
044500021211     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
044600021211     C     POP1          IFEQ      'O02'
044700021211     C                   MOVEL     DEC(1)        VIDFUN
044800021211     C                   ENDIF
044900021211     C     POP1          IFEQ      'O04'
045000021211     C                   MOVEL     DEC(2)        VIDFUN
045100021211     C                   ENDIF
045200021211     C     POP1          IFEQ      'O05'
045300021211     C                   MOVEL     DEC(3)        VIDFUN
045400021211     C                   ENDIF
045500030124     C     POP1          IFEQ      'O07'
045600030124     C                   MOVEL     DEC(5)        VIDFUN
045700030124     C                   ENDIF
045800021211     C     POP1          IFEQ      'F06'
045900021211     C                   MOVEL     DEC(4)        VIDFUN
046000021211     C                   ENDIF
046100021211     C*
046200021211     C                   ENDSR
046300030123     C*--------------------------------------------------------------------------------------------*
046400030123     C* REPERISCE I DATI UTENTE
046500030123     C*--------------------------------------------------------------------------------------------*
046600030123     C     REPDATIUTE    BEGSR
046700030123     C*
046800030123     C* INIZIALIZZA VARIABILI DI WRK
046900030123     C                   CLEAR                   TIBS34DS
047000030123     C                   CLEAR                   AZUTEDS
047100030123     C                   CLEAR                   DDATIUTE
047200030123     C*
047300030123     C     *DTAARA       DEFINE    §azute        azuteds
047400030123     C     *DTAARA       DEFINE    §datiute      ddatiute
047500030123     C                   IN(E)     *DTAARA
047600030123     C                   IF        %Error
047700030123     C                   EVAL      I34Tla = 'L'
047800030123     C                   CALL      'TIBS34R'
047900030123     C                   PARM                    Tibs34Ds
048000030123     C                   IN        *DTAARA
048100030123     C                   ENDIF
048200030123     C*
048300030123     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
048400030123     C                   MOVEL(P)  RSUT          DSFIRS           20
048500030123     C*
048600030123     C                   ENDSR
048700970526     C*------------------------------------------------------------------------*
048800970526     C* *INZSR - OPERAZIONI INIZIALI
048900970526     C*------------------------------------------------------------------------*
049000970526     C     *INZSR        BEGSR
049100970526     C*---
049200970526     C* RICEVIMENTO PARAMETRI
049300970526     C*---
049400970526     C     *ENTRY        PLIST
049500970612     C                   PARM                    KPJBA
049600000407     C                   MOVEL     KPJBU         PARAM
049700030123     C*
049800030123     C* REPERIMENTO DATI UTENTE
049900030123     C                   EXSR      REPDATIUTE
050000030122     C*---
050100030122     C* CHIAVI DI LETTURA
050200030122     C*---
050300030122     C* LETTURA tntbe01l - PARZIALE
050400030122     C     KEYTBE        KLIST
050500030122     C                   KFLD                    tbeCOD
050600030124     C                   KFLD                    tbeKE1
050700000407     C*
050800000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
050900000407     C     POP1          IFEQ      'O05'
051000000407     C                   SETON                                        90
051100000407     C                   ENDIF
051200000407     C     POP1          IFEQ      'O02'
051300000407     C                   SETON                                        91
051400000407     C                   ENDIF
051500970526     C*
051600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
051700970526     C     XMSGPA        PLIST
051800970526     C                   PARM                    DSMSMS
051900970526     C                   PARM                    DSMSPG
052000970526     C                   PARM                    DSMSKE
052100970526     C                   PARM                    DSMSER
052200970526     C*
052300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
052400970526     C                   CALL      'XKEY'
052500970526     C                   PARM                    DSKEY
052600970526     C*---
052700970526     C* VARIABILI NON RIFERITE AL DATA BASE
052800970526     C*---
052900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
053000970526     C*---
053100970526     C* VARIABILI DI CONTROLLO
053200970526     C*---
053300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
053400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
053500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
053600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
053700970526     C*
053800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
053900030122     C                   MOVEL     'TNTB32R'     DSMSPG
054000030122     C                   MOVEL     'TNTB32R'     MSGQUE
054100970526     C                   MOVEL     *BLANKS       MSGKEY
054200970526     C                   MOVEL     *BLANKS       DSMSMS
054300970526     C*
054400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
054500970526     C                   EXSR      IMPFUN
054600970526     C*
054700970526     C                   ENDSR
054800970526     C*------------------------------------------------------------------------*
054900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
055000970526     C*------------------------------------------------------------------------*
055100970526     C     FUNGES        BEGSR
055200970526     C*
055300970526     C* PULISCO CAMPI DI LAVORO
055400970526     C                   Z-ADD     0             W                 5 0
055500970526     C*
055600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
055700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
055800970526     C     1             DO        24            I
055900970526     C                   MOVEL     FUD(I)        FUDRIG
056000970526     C*
056100970526     C* PULIZIA FUNZIONI PRECEDENTI
056200970526     C                   MOVEL     *BLANKS       FUV(I)
056300970526     C*
056400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
056500970526     C* (SALTA F24)
056600970526     C     FUDATT        IFEQ      'S'
056700970526     C     FUDFUN        ANDNE     '24'
056800970526     C                   ADD       1             W
056900970526     C                   MOVEL     FUDDES        FUV(W)
057000970526     C                   END
057100970526     C                   END                                                    DO WHILE
057200970526     C*
057300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
057400970526     C* ALTRIMENTI DISATTIVO F24
057500970526     C     1             DO        24            I
057600970526     C                   MOVEL     FUD(I)        FUDRIG
057700970526     C*
057800970526     C     FUDFUN        IFEQ      '24'
057900970526     C     W             IFGT      6
058000970526     C                   MOVEL     'S'           FUDATT
058100970526     C                   ELSE
058200970526     C                   MOVEL     *BLANKS       FUDATT
058300970526     C                   END                                                    W GT 8
058400970526     C                   MOVEL     FUDRIG        FUD(I)
058500970526     C                   END
058600970526     C                   END                                                    DO WHILE
058700970526     C*
058800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
058900970526     C     W             DIV       6             I
059000970526     C                   MVR                     Z
059100970526     C*
059200970526     C     Z             IFNE      0
059300970526     C     I             ADD       1             Z
059400970526     C                   ELSE
059500970526     C                   Z-ADD     I             Z                 5 0
059600970526     C                   END
059700970526     C*
059800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
059900970526     C                   ADD       1             FUNGRU
060000970526     C     FUNGRU        IFGT      Z
060100970526     C     FUNGRU        ORLT      1
060200970526     C                   Z-ADD     1             FUNGRU
060300970526     C                   END
060400970526     C*
060500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
060600970526     C     FUNGRU        IFEQ      1
060700970526     C                   MOVEL     FUV(1)        FUN001
060800970526     C                   MOVEL     FUV(2)        FUN002
060900970526     C                   MOVEL     FUV(3)        FUN003
061000970526     C                   MOVEL     FUV(4)        FUN004
061100970526     C                   MOVEL     FUV(5)        FUN005
061200970526     C                   MOVEL     FUV(6)        FUN006
061300970526     C                   END
061400970526     C*
061500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
061600970526     C     FUNGRU        IFEQ      2
061700970526     C                   MOVEL     FUV(7)        FUN001
061800970526     C                   MOVEL     FUV(8)        FUN002
061900970526     C                   MOVEL     FUV(9)        FUN003
062000970526     C                   MOVEL     FUV(10)       FUN004
062100970526     C                   MOVEL     FUV(11)       FUN005
062200970526     C                   MOVEL     FUV(12)       FUN006
062300970526     C                   END
062400970526     C*
062500970526     C* TERZO GRUPPO DI 6 FUNZIONI
062600970526     C     FUNGRU        IFEQ      3
062700970526     C                   MOVEL     FUV(13)       FUN001
062800970526     C                   MOVEL     FUV(14)       FUN002
062900970526     C                   MOVEL     FUV(15)       FUN003
063000970526     C                   MOVEL     FUV(16)       FUN004
063100970526     C                   MOVEL     FUV(17)       FUN005
063200970526     C                   MOVEL     FUV(18)       FUN006
063300970526     C                   END
063400970526     C*
063500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
063600970526     C     FUNGRU        IFEQ      4
063700970526     C                   MOVEL     FUV(19)       FUN001
063800970526     C                   MOVEL     FUV(20)       FUN002
063900970526     C                   MOVEL     FUV(21)       FUN003
064000970526     C                   MOVEL     FUV(22)       FUN004
064100970526     C                   MOVEL     *BLANKS       FUN005
064200970526     C                   MOVEL     *BLANKS       FUN006
064300970526     C                   END
064400970526     C*
064500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
064600970526     C                   MOVEL     *BLANKS       FUN007
064700970526     C                   MOVEL     *BLANKS       FUN008
064800970526     C*
064900970526     C     1             DO        24            I
065000970526     C                   MOVEL     FUD(I)        FUDRIG
065100970526     C     FUDFUN        IFEQ      '24'
065200970526     C     FUDATT        ANDEQ     'S'
065300970526     C                   MOVEL     FUDDES        FUN008
065400970526     C                   END
065500970526     C                   END                                                    DO
065600970526     C*
065700970526     C                   ENDSR
065800970526     C*------------------------------------------------------------------------*
065900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
066000970526     C*------------------------------------------------------------------------*
066100970526     C     FUNCON        BEGSR
066200970526     C*
066300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
066400970526     C                   EXSR      FUNVAR
066500970526     C*
066600970526     C* PULISCO CAMPI DI LAVORO
066700970526     C                   Z-ADD     0             I                 5 0
066800970526     C                   Z-ADD     0             W                 5 0
066900970526     C*
067000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
067100970526     C     FUNKEY        IFNE      *BLANKS
067200970526     C     1             DO        24            I
067300970526     C                   MOVEL     FUD(I)        FUDRIG
067400970526     C*
067500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
067600970526     C     FUNKEY        IFEQ      FUDFUN
067700970526     C     FUDATT        ANDEQ     'S'
067800970526     C                   Z-ADD     I             W
067900970526     C                   END
068000970526     C                   END                                                    DO WHILE
068100970526     C*
068200970526     C* FUNZIONE NON ATTIVA (ERRORE)
068300970526     C     W             IFEQ      0
068400970526     C*
068500970526     C* EMISSIONE MESSAGGIO DI ERRORE
068600970526     C                   MOVEL     MSG(1)        DSMSMS
068700970526     C*
068800970526     C                   END                                                    W=0
068900970526     C                   END                                                    FUNKEY NE *BLANKS
069000970526     C*
069100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
069200970526     C     W             IFNE      0
069300970526     C     FUNKEY        OREQ      *BLANKS
069400970526     C     FUNKEY        IFEQ      *BLANKS
069500970526     C                   EXSR      ENTGES
069600970526     C                   END
069700970526     C     FUNKEY        IFEQ      '03'
069800970526     C                   EXSR      F03GES
069900970526     C                   END
070000970526     C     FUNKEY        IFEQ      '06'
070100970526     C                   EXSR      F06GES
070200970526     C                   END
070300970526     C     FUNKEY        IFEQ      '12'
070400970526     C                   EXSR      F12GES
070500970526     C                   END
070600970526     C     FUNKEY        IFEQ      '24'
070700970526     C                   EXSR      F24GES
070800970526     C                   END
070900970526     C                   END
071000970526     C*
071100970526     C                   ENDSR
071200970526     C*------------------------------------------------------------------------*
071300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
071400970526     C*------------------------------------------------------------------------*
071500970526     C     FUNVAR        BEGSR
071600970526     C*
071700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
071800970526     C*
071900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
072000970526     C     KEY           IFEQ      ENTER
072100970526     C                   MOVEL     *BLANKS       FUNKEY
072200970526     C                   END
072300970526     C     KEY           IFEQ      F1
072400970526     C                   MOVEL     '01'          FUNKEY
072500970526     C                   END
072600970526     C     KEY           IFEQ      F2
072700970526     C                   MOVEL     '02'          FUNKEY
072800970526     C                   END
072900970526     C     KEY           IFEQ      F3
073000970526     C                   MOVEL     '03'          FUNKEY
073100970526     C                   END
073200970526     C     KEY           IFEQ      F4
073300970526     C                   MOVEL     '04'          FUNKEY
073400970526     C                   END
073500970526     C     KEY           IFEQ      F5
073600970526     C                   MOVEL     '05'          FUNKEY
073700970526     C                   END
073800970526     C     KEY           IFEQ      F6
073900970526     C                   MOVEL     '06'          FUNKEY
074000970526     C                   END
074100970526     C     KEY           IFEQ      F7
074200970526     C                   MOVEL     '07'          FUNKEY
074300970526     C                   END
074400970526     C     KEY           IFEQ      F8
074500970526     C                   MOVEL     '08'          FUNKEY
074600970526     C                   END
074700970526     C     KEY           IFEQ      F9
074800970526     C                   MOVEL     '09'          FUNKEY
074900970526     C                   END
075000970526     C     KEY           IFEQ      F10
075100970526     C                   MOVEL     '10'          FUNKEY
075200970526     C                   END
075300970526     C     KEY           IFEQ      F11
075400970526     C                   MOVEL     '11'          FUNKEY
075500970526     C                   END
075600970526     C     KEY           IFEQ      F12
075700970526     C                   MOVEL     '12'          FUNKEY
075800970526     C                   END
075900970526     C     KEY           IFEQ      F13
076000970526     C                   MOVEL     '13'          FUNKEY
076100970526     C                   END
076200970526     C     KEY           IFEQ      F14
076300970526     C                   MOVEL     '14'          FUNKEY
076400970526     C                   END
076500970526     C     KEY           IFEQ      F15
076600970526     C                   MOVEL     '15'          FUNKEY
076700970526     C                   END
076800970526     C     KEY           IFEQ      F16
076900970526     C                   MOVEL     '16'          FUNKEY
077000970526     C                   END
077100970526     C     KEY           IFEQ      F17
077200970526     C                   MOVEL     '17'          FUNKEY
077300970526     C                   END
077400970526     C     KEY           IFEQ      F18
077500970526     C                   MOVEL     '18'          FUNKEY
077600970526     C                   END
077700970526     C     KEY           IFEQ      F19
077800970526     C                   MOVEL     '19'          FUNKEY
077900970526     C                   END
078000970526     C     KEY           IFEQ      F20
078100970526     C                   MOVEL     '20'          FUNKEY
078200970526     C                   END
078300970526     C     KEY           IFEQ      F21
078400970526     C                   MOVEL     '21'          FUNKEY
078500970526     C                   END
078600970526     C     KEY           IFEQ      F22
078700970526     C                   MOVEL     '22'          FUNKEY
078800970526     C                   END
078900970526     C     KEY           IFEQ      F23
079000970526     C                   MOVEL     '23'          FUNKEY
079100970526     C                   END
079200970526     C     KEY           IFEQ      F24
079300970526     C                   MOVEL     '24'          FUNKEY
079400970526     C                   END
079500970526     C     KEY           IFEQ      ROLLUP
079600970526     C                   MOVEL     'RU'          FUNKEY
079700970526     C                   END
079800970526     C     KEY           IFEQ      HELP
079900970526     C                   MOVEL     'HP'          FUNKEY
080000970526     C                   END
080100970526     C                   ENDSR
080200970526     C*------------------------------------------------------------------------*
080300970526     C* NOTE COSTRUZIONE SCHIERE
080400970526     C*------------------------------------------------------------------------*
080500970526     C*
080600970526     C* -MSG-MESSAGGI
080700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
080800970526     C* (NON TOCCARE MSG,1 E MSG,2)
080900970526     C*
081000970526     C* -CMD-COMANDI
081100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
081200970526     C*
081300970526     C* -OPD-OPZIONI
081400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
081500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
081600970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
081700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
081800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
081900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
082000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
082100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
082200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
082300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
082400970526     C*
082500970526     C* -FUD-FUNZIONI
082600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
082700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
082800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
082900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
083000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
083100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
083200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
083300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
083400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
083500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
083600970526     O*------------------------------------------------------------------------*
083700970526** DEC - DECODIFICHE
083800970526    MODIFICA                                                      1
083900970526  ANNULLAMENTO                                                    2
084000970526VISUALIZZAZIONE                                                   3
084100021211  INSERIMENTO                                                     4
084200030124  RIPRISTINO                                                      5
084300970528** MSG - MESSAGGI
084400030122TNTB32R- Tasto funzionale non gestito                                          1
084500030122TNTB32R- Inserimento effettuato                                                2
084600030122TNTB32R- Modifica effettuata                                                   3
084700030122TNTB32R- Annullamento effettuato                                               4
084800030122TNTB32R- Copia effettuata                                                      5
084900030122TNTB32R- Richiesta funzione per record inesistente                             6
085000030122TNTB32R- Impossibile inserire, codice causale già presente.                    7
085100030122TNTB32R- Richiesta funzione per record esistente                               8
085200030122TNTB32R- Causale correlata inesistente, inserirla e riproporre la richiesta    9
085300030124TNTB32R- Ripristino effettuato                                                 10
085400150223TNTB32R- % franchigia solo per movimento cliente, positivo, manuale            11
085500030122TNTB32R- Tutti i campi evidenziati sono obbligatori                            12
085600030122TNTB32R- 13                                                                    13
085700030122TNTB32R- 14                                                                    14
085800030122TNTB32R- 15                                                                    15
085900030122TNTB32R- 16                                                                    16
086000030122TNTB32R- 17                                                                    17
08610002121118                                                                             18
08620002121119                                                                             19
08630002121120                                                                             20
086400970526** FUD - FUNZIONI
086500970526                   -        (S/N)=N                        funz.01¬
086600970526                   -        (S/N)=N                        funz.02¬
086700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
086800970526                   -        (S/N)=N                        funz.04¬
086900970526                   -        (S/N)=N                        funz.05¬
087000000407F06=Conferma       -        (S/N)=S                        funz.06¬
087100970527                   -        (S/N)=N                        funz.07¬
087200970527                   -        (S/N)=N                        funz.08¬
087300970527                   -        (S/N)=N                        funz.09¬
087400970527                   -        (S/N)=N                        funz.10¬
087500970526                   -        (S/N)=N                        funz.11¬
087600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
087700970526                   -        (S/N)=N                        funz.13¬
087800970526                   -        (S/N)=N                        funz.14¬
087900970526                   -        (S/N)=N                        funz.15¬
088000970526                   -        (S/N)=N                        funz.16¬
088100970526                   -        (S/N)=N                        funz.17¬
088200970526                   -        (S/N)=N                        funz.18¬
088300970526                   -        (S/N)=N                        funz.19¬
088400970526                   -        (S/N)=N                        funz.20¬
088500970526                   -        (S/N)=N                        funz.21¬
088600970526                   -        (S/N)=N                        funz.22¬
088700970526                   -        (S/N)=N                        funz.23¬
088800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
