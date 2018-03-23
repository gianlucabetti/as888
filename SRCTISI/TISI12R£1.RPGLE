000100970526     H*------------------------------------------------------------------------*
000200000601     H* CAPPARIO - GESTIONE ECCEZIONI LINEE/ZONE - ROUTINE + ESTENSIONE X POSTE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970527     FTISI12D   CF   E             WORKSTN INFDS(DEVDS1)
000900970527     F                                     SFILE(SI12DMS:MSGULT)
001000970527     FTABEL00F  IF   E           K DISK
001100970527     FAZORG01L  IF   E           K DISK
001200970527     F*------------
001300970527     F* ECCEZIONI LINEE/ZONE
001400970527     F*------------
001500970527     FAZCPE01L  UF A E           K DISK
001600000601     FAZPPE01L  UF A E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* FUNZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D FUV             S             19    DIM(24)
003600970526     D*------------------------------------------------------------------------*
003700970526     D* INPUT
003800970526     D*------------------------------------------------------------------------*
003900970526     D*------------
004000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004100970526     D*------------
004200970526     D FUDDS           DS
004300970526     D  FUDDE1                 1      1
004400970526     D  FUDFUN                 2      3
004500970526     D  FUDDE2                 4     19
004600970526     D  FUDFI0                20     34
004700970526     D  FUDATT                35     35
004800970526     D  FUDFI1                36     80
004900970526     D  FUDDES                 1     19
005000970526     D  FUDRIG                 1     80
005100970526     D*------------
005200970526     D* DS GESTIONE TASTI FUNZIONALI
005300970526     D*------------
005400970526     D DSKEY         E DS
005500970526     D DEVDS1          DS
005600970526     D  KEY                  369    369
005700970526     D*------------
005800970526     D* DS "XMSG" - GESTIONE MESSAGGI
005900970526     D*------------
006000970526     D DSMSG         E DS
006100970626     D*------------------
006200970626     D* DS "TISI95R" - CONTROLLO CAP
006300970626     D*------------------
006400970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
006500970527     D*------------------
006600970527     D* DS NAZIONI
006700970527     D*------------------
006800970527     D DS15          E DS
006900970527     D*------------------
007000970527     D* DS ZONE DI CONSEGNA
007100970527     D*------------------
007200970527     D DS03          E DS
007300000613     D*------------
007400000613     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
007500000613     D*------------
007600000613     D OG143         E DS
007700970527     D*------------
007800970527     D* DS "TISI81R" - AGGIORNA UN CAP DAI SUOI ARCHIVI
007900970527     D*------------
008000970527     D DSSI81        E DS                  EXTNAME(TISI81DS)
008100971128     D*------------
008200971128     D* DS REPEIMENTO TERMINALs
008300971128     D*------------
008400971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
008500970526     D*------------
008600970526     D* DS DI PROCEDURA
008700970526     D*------------
008800970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
008900971128     D*------------------
009000971128     D* DS "XSRDA8" - CONTROLLA DATA (8)
009100971128     D*------------------
009200971128     D WLBDA8          DS                  INZ
009300971128     D  G08DAT                 1      8  0
009400971128     D  G08INV                 9     16  0
009500971128     D  G08ERR                17     17
009600971128     D  G08TGI                18     22  0
009700970526     D*------------
009800970526     D* ARCHITETTURA
009900970526     D*------------
010000970526     D KPJBA         E DS
010100060206      * - Parametri x Controllo profilo utenti
010200060206     d TIBS34ds      e ds
010300060206      * - Ds di riferimento al file esterno AZUTE00F
010400060206     d AZUTEds       e ds                  extname(AZUTE00F)
010500060206     d dUTE01        e ds
010600060206      * - Ds per dati organigramma
010700060206     d DDatiUte      e ds
010800970526     C*------------------------------------------------------------------------*
010900970526     C* MAIN LINES
011000970526     C*------------------------------------------------------------------------*
011100970526     C*
011200970526     C* NON CI SONO ERRORI IN ENTRATA
011300970526IF  1C     ERRL00        IFEQ      '0'
011400970526     C*
011500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011900970526     C*
012000970526     C* EMETTE FORMATO VIDEO
012100970526     C                   EXSR      BUTFMR
012200970526     C*
012300970526     C* GESTIONE TASTI FUNZIONALI
012400970526     C                   EXSR      FUNCON
012500970526E   2C                   ENDDO
012600970526     C*
012700970526     C* CI SONO ERRORI IN ENTRATA
012800970527X   1C                   ELSE
012900970526     C                   MOVEL     '1'           D00ERR
013000970526     C                   MOVEL     DSMSMS        D00MSG
013100970526E   1C                   ENDIF
013200970626     C*
013300970626     C* OPERAZIONI FINALI
013400970626     C                   EXSR      FINSR
013500970526     C* FINE PGM
013600970526     C                   SETON                                        LR
013700970526     C*------------------------------------------------------------------------*
013800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013900970526     C*------------------------------------------------------------------------*
014000970526     C     BUTFMR        BEGSR
014100970526     C*
014200970526     C* EMETTE LA TESTATA
014300970527     C                   WRITE     SI12D01
014400970526     C*
014500970526     C* SPEDISCE EVENTUALE MESSAGGIO
014600970526     C                   EXSR      SNDMSG
014700970526     C*
014800970526     C* EMETTE LE FUNZIONI
014900970527     C                   WRITE     SI12D02
015000970526     C*
015100970526     C* EMETTE IL VIDEO
015200970527     C                   EXFMT     SI12D03
015300970526     C*
015400970526     C* REIMPOSTA FLAG DI ERRORE
015500970526     C                   MOVEL     '0'           ERRL00
015600970526     C*
015700970526     C* REIMPOSTA INDICATORI DI ERRORE
015800000601     C                   SETOFF                                         5455
015900970526     C                   SETOFF                                         5859
016000970526     C                   SETOFF                                       606162
016100970526     C                   SETOFF                                       636465
016200970526     C                   SETOFF                                       666768
016300970526     C                   SETOFF                                       697071
016400970526     C                   SETOFF                                       727374
016500970526     C                   SETOFF                                       757677
016600970526     C                   SETOFF                                       787980
016700970526     C                   SETOFF                                       818283
016800970526     C                   SETOFF                                       848586
016900970526     C                   SETOFF                                       878889
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     SNDMSG        BEGSR
017600970526     C*
017700970526     C* PULISCE IL SFILE MESSAGGI
017800970526     C                   MOVEL     '1'           *IN40
017900970527     C                   WRITE     SI12DMC
018000970526     C                   MOVEL     '0'           *IN40
018100970526     C*
018200970526     C* VISUALIZZA IL MESSAGGIO
018300970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018400970526     C                   CALL      'XMSG'        XMSGPA
018500970526IF  1C     DSMSER        IFEQ      *ZEROS
018600970526     C                   MOVEL     DSMSKE        MSGKEY
018700970526E   1C                   ENDIF
018800970526     C                   Z-ADD     1             MSGULT
018900970527     C                   WRITE     SI12DMS
019000970527     C                   WRITE     SI12DMC
019100970526     C*
019200970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019300970526     C                   MOVEL     *BLANKS       DSMSMS
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* ENTGES - GESTIONE ENTER -> INVIO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     ENTGES        BEGSR
020000970526     C*
020100970526     C* CONTROLLA I CAMPI IMMESSI
020200970526     C                   EXSR      DECODI
020300970526     C*
020400970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
020500970526IF  1C     DEPOP1        IFEQ      'O05'
020600970526     C                   MOVEL     '1'           FINL00
020700970526E   1C                   ENDIF
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F03GES - GESTIONE F03 -> FINE
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F03GES        BEGSR
021400970526     C*
021500970526     C* IMPOSTA "PREMUTO F03"
021600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021700970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
022200970526     C*------------------------------------------------------------------------*
022300970526     C     F06GES        BEGSR
022400970526     C*
022500970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
022600970526IF  1C     DEPOP1        IFEQ      'F06'
022700970526     C                   EXSR      INSERI
022800970526E   1C                   ENDIF
022900970526IF  1C     DEPOP1        IFEQ      'O02'
023000970526     C                   EXSR      MODIFI
023100970526E   1C                   ENDIF
023200970526IF  1C     DEPOP1        IFEQ      'O04'
023300970526     C                   EXSR      ANNULL
023400970526E   1C                   ENDIF
023500970526IF  1C     DEPOP1        IFEQ      'O07'
023600970526     C                   EXSR      RIPRIS
023700970526E   1C                   ENDIF
023800970526     C*
023900970526     C                   ENDSR
024000970526     C*------------------------------------------------------------------------*
024100970526     C* F12GES - GESTIONE F12 -> RITORNO
024200970526     C*------------------------------------------------------------------------*
024300970526     C     F12GES        BEGSR
024400970526     C*
024500970526     C* IMPOSTA "PREMUTO F12"
024600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024700970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025200970526     C*------------------------------------------------------------------------*
025300970526     C     F24GES        BEGSR
025400970526     C*
025500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025600970526     C*
025700970526     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
026000970526     C*------------------------------------------------------------------------*
026100970526     C     INSERI        BEGSR
026200970526     C*
026300970526     C* CONTROLLA I CAMPI IMMESSI
026400970526     C                   EXSR      DECODI
026500970526     C*
026600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026800970527     C                   Z-ADD     D00VER        CPEVER                         *CHIAVE
026900970527     C                   MOVEL     VIDNAR        CPENAR
027000970527     C                   MOVEL     VIDCAP        CPECAP
027100970527     C                   MOVEL     VIDTFP        CPETFP
027200970527     C                   EXSR      IMPCPE                                       *ALTRI CAMPI
027300970527     C                   Z-ADD     *ZEROS        CPEDTR                         *DATA TRASMISSIONE
027400970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
027500970527     C                   MOVEL     *BLANKS       CPEATB                         *ANNULLAMENTO
027600970527     C                   WRITE     AZCPE000
027700970527     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
027800000601     C*
027900000601     C*--- CONSIDERAZIONI PER POSTE
028000000601     C                   EXSR      LETCPE
028100000601     C     *IN93         IFEQ      *OFF
028200000601     C                   EXSR      IMPPPE
028300000601     C                   ENDIF
028400000601     C*--- END POSTE
028500970527     C*
028600970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
028700970527     C                   Z-ADD     D00VER        D81VER
028800970527     C                   MOVEL     VIDNAR        D81NAR
028900970527     C                   MOVEL     VIDCAP        D81CAP
029000970527     C                   MOVEL     'E'           D81CHK
029100970527     C                   CALL      'TISI81R'
029200970527     C                   PARM                    DSSI81
029300970602     C*
029400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
029500970602     C                   EXSR      AZZVID
029600970526E   1C                   ENDIF
029700970526     C*
029800970526     C                   ENDSR
029900970526     C*------------------------------------------------------------------------*
030000970526     C* MODIFI - OPERAZIONI DI MODIFICA
030100970526     C*------------------------------------------------------------------------*
030200970526     C     MODIFI        BEGSR
030300970526     C*
030400970526     C* CONTROLLA I CAMPI IMMESSI
030500970526     C                   EXSR      DECODI
030600970526     C*
030700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030900970527     C                   EXSR      LETCPE
031000970526     C*
031100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
031200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031300970527     C                   EXSR      IMPCPE                                       *IMPOSTA I CAMPI
031400970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
031500970527     C                   MOVEL     *BLANKS       CPEATB                         *ANNULLAMENTO
031600970527     C                   UPDATE    AZCPE000
031700970526     C                   MOVEL     MSG(3)        D00MSG
031800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031900000601     C*
032000000601     C*--- CONSIDERAZIONI PER POSTE
032100000601     C     *IN93         IFEQ      *OFF
032200000601     C                   EXSR      IMPPPE
032300000601     C                   ENDIF
032400000601     C*--- END POSTE
032500970526E   2C                   ENDIF
032600970526E   1C                   ENDIF
032700970526     C*
032800970526     C                   ENDSR
032900970526     C*------------------------------------------------------------------------*
033000970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
033100970526     C*------------------------------------------------------------------------*
033200970526     C     ANNULL        BEGSR
033300970526     C*
033400970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
033500000602     C                   MOVEL     '0'           ANNDLT            1
033600970527     C                   EXSR      LETCPE
033700970527     C*
033800970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
033900970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034000970527IF  2C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
034100970527IF  3C     CPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
034200970527     C                   MOVEL     'A'           CPEATB                         *FLAG ANNULLAMENTO
034300970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
034400970527     C                   UPDATE    AZCPE000                                     *ANNULLA
034500970526X   3C                   ELSE                                                   *MAI TRASMESSO
034600970527     C                   DELETE    AZCPE000                                     *CANCELLA
034700000602     C                   MOVEL     '1'           ANNDLT
034800970526E   3C                   ENDIF
034900970526E   2C                   ENDIF
035000970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
035100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035200000601     C*
035300000601     C*--- ANNULLA ESTENSIONE ECCEZIONI X POSTE
035400000601     C     *IN93         IFEQ      *OFF
035500000601     C     *IN96         ANDEQ     *OFF
035600000601IF  2C     PPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
035700000601IF  3C     PPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
035800000601     C                   MOVEL     'A'           PPEATB                         *FLAG ANNULLAMENTO
035900000601     C                   MOVEL     *BLANKS       PPEFTR                         *FLAG TRASMISSIONE
036000000601     C                   UPDATE    AZPPE000                                     *ANNULLA
036100000601X   3C                   ELSE                                                   *MAI TRASMESSO
036200000602IF  4C     ANNDLT        IFEQ      '1'                                          *E'STATO DELETATO IL
036300000601     C                   DELETE    AZPPE000                                     *CANCELLA
036400000602     C                   ELSE
036500000602     C                   MOVEL     'A'           PPEATB                         *FLAG ANNULLAMENTO
036600000602     C                   MOVEL     *BLANKS       PPEFTR                         *FLAG TRASMISSIONE
036700000602     C                   UPDATE    AZPPE000                                     *ANNULLA
036800000602E   4C                   ENDIF
036900000601E   3C                   ENDIF
037000000601E   2C                   ENDIF
037100000601     C                   ENDIF
037200000601     C*--- END POSTE
037300970527     C*
037400970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
037500970527     C                   Z-ADD     D00VER        D81VER
037600970527     C                   MOVEL     VIDNAR        D81NAR
037700970527     C                   MOVEL     VIDCAP        D81CAP
037800970527     C                   MOVEL     'E'           D81CHK
037900970527     C                   CALL      'TISI81R'
038000970527     C                   PARM                    DSSI81
038100970526E   1C                   ENDIF
038200970526     C*
038300970526     C                   ENDSR
038400970526     C*------------------------------------------------------------------------*
038500970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
038600970526     C*------------------------------------------------------------------------*
038700970526     C     RIPRIS        BEGSR
038800970526     C*
038900970526     C* CONTROLLA I CAMPI IMMESSI
039000970526     C                   EXSR      DECODI
039100970526     C*
039200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
039300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039400970527     C                   EXSR      LETCPE
039500970526     C*
039600970526     C* IMPOSTA IL RECORD DA SCRIVERE
039700970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039800970527     C                   MOVEL     *BLANKS       CPEATB                         *FLAG ANNULLAMENTO
039900970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
040000970527     C                   UPDATE    AZCPE000                                     *ANNULLA
040100970526E   2C                   ENDIF
040200970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
040300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
040400000601     C*
040500000601     C*--- CONSIDERAZIONI PER POSTE
040600000601     C     *IN93         IFEQ      *OFF
040700000601     C     *IN96         IFEQ      *OFF
040800000601     C                   MOVEL     *BLANKS       PPEATB                         *FLAG ANNULLAMENTO
040900000601     C                   MOVEL     *BLANKS       PPEFTR                         *FLAG TRASMISSIONE
041000000601     C                   UPDATE    AZPPE000                                     *RIPRISTINA
041100000601     C                   ELSE
041200000601     C                   EXSR      IMPPPE
041300000601     C                   ENDIF
041400000601     C                   ENDIF
041500000601     C*--- END POSTE
041600970527     C*
041700970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
041800970527     C                   Z-ADD     D00VER        D81VER
041900970527     C                   MOVEL     VIDNAR        D81NAR
042000970527     C                   MOVEL     VIDCAP        D81CAP
042100970527     C                   MOVEL     'E'           D81CHK
042200970530     C                   CALL      'TISI81R'
042300970530     C                   PARM                    DSSI81
042400970526E   1C                   ENDIF
042500970526     C*
042600970526     C                   ENDSR
042700970526     C*------------------------------------------------------------------------*
042800970527     C* LETCPE - LEGGE IL RECORD PERL'AGGIORNAMENTO
042900970526     C*------------------------------------------------------------------------*
043000970527     C     LETCPE        BEGSR
043100970526     C*
043200000601     C     DEPOP1        IFNE      'F06'
043300970527     C                   Z-ADD     D00VER        KCEVER
043400980127     C                   MOVEL     D00NAR        KCENAR
043500980127     C                   MOVEL     D00CAP        KCECAP
043600980127     C                   MOVEL     D00TFP        KCETFP
043700970527     C     KEYCPE        CHAIN     AZCPE01L                           9998
043800970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
043900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
044000970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
044100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
044200970526E   1C                   ENDIF
044300970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
044400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
044500970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
044600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
044700970526E   1C                   ENDIF
044800000601     C                   ENDIF
044900000601     C*
045000000601     C* LETTURA SU ARCHIVI ESTENSIONE CAPPARIO X POSTE
045100000601     C     KEYCPE        CHAIN     AZPPE01L                           9697
045200000601IF  1C     *IN97         IFEQ      *ON                                          *ALLOCATO
045300000601     C                   MOVEL     '1'           ERRL00                         *ERRORE
045400000601     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
045500000601     C                   MOVEL     '1'           FINL00                         *FINE PGM
045600000601E   1C                   ENDIF
045700970526     C*
045800970526     C                   ENDSR
045900970526     C*------------------------------------------------------------------------*
046000970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
046100970526     C*------------------------------------------------------------------------*
046200970526     C     DECODI        BEGSR
046300970527     C*--------------------
046400970527     C* NAZIONE
046500970527     C*--------------------
046600970527     C     *LIKE         DEFINE    §15CPR        DEPCPR
046700970527IF  1C     ERRL00        IFEQ      '0'
046800970527     C                   MOVEL     *BLANKS       VDDNAR
046900970527     C                   MOVEL     VIDNAR        WNAR
047000970527     C                   MOVEL     *BLANKS       WDNAR
047100970527     C                   MOVEL     *BLANKS       WCPR
047200970527     C                   EXSR      CHKNAR
047300970527     C                   MOVEL     WNAR          VIDNAR
047400970527     C                   MOVEL     WDNAR         VDDNAR
047500970527     C                   MOVEL     WCPR          DEPCPR
047600970527IF  2C     ERRL00        IFEQ      '1'
047700970527     C                   SETON                                        60
047800970527     C                   MOVEL     MSG(11)       DSMSMS
047900970527E   2C                   ENDIF
048000970527E   1C                   ENDIF
048100970527     C*--------------------
048200970527     C* CAP
048300970527     C*--------------------
048400970527IF  1C     ERRL00        IFEQ      '0'
048500970527     C                   MOVEL     VIDNAR        WNAR
048600970527     C                   MOVEL     VIDCAP        WCAP
048700970527     C                   EXSR      CHKCAP
048800970527IF  2C     ERRL00        IFEQ      '1'
048900970527     C                   SETON                                        61
049000970527     C                   MOVEL     MSG(12)       DSMSMS
049100970527E   2C                   ENDIF
049200970527E   1C                   ENDIF
049300970626     C* CARICA DATI CAP
049400970527IF  1C     ERRL00        IFEQ      '0'
049500970626     C                   Z-ADD     O95CLA        VIDCLN
049600970626     C                   Z-ADD     O95CZC        VIDCZN
049700970626     C                   Z-ADD     O95CLO        VIDCLO
049800970626     C                   Z-ADD     O95CZO        VIDCZO
049900970626     C                   Z-ADD     O95CLK        VIDCKG
050000970626     C                   Z-ADD     O95CLM        VIDCMC
050100970626     C                   Z-ADD     O95CLS        VIDCLS
050200970626     C                   Z-ADD     O95CZS        VIDCZS
050300970626     C                   Z-ADD     O95CKS        VIDCKS
050400970626     C                   Z-ADD     O95CMS        VIDCMS
050500971118     C                   Z-ADD     O95CL2        VIDCL2
050600971118     C                   Z-ADD     O95CZ2        VIDCZ2
050700971118     C                   Z-ADD     O95CK2        VIDCK2
050800971118     C                   Z-ADD     O95CM2        VIDCM2
050900970527E   1C                   ENDIF
051000970527     C*--------------------
051100970527     C* TERMINAL PARTENZA
051200970527     C*--------------------
051300970527IF  1C     ERRL00        IFEQ      '0'
051400970527     C                   MOVEL     VIDTFP        WTFP
051500970527     C                   MOVEL     *BLANKS       WDTFP
051600970528     C                   EXSR      CHKTFX
051700970527     C                   MOVEL     WTFP          VIDTFP
051800970527     C                   MOVEL     WDTFP         VDDTFP
051900970527IF  2C     ERRL00        IFEQ      '1'
052000970527     C                   SETON                                        62
052100970527     C                   MOVEL     MSG(14)       DSMSMS
052200970527E   2C                   ENDIF
052300970527E   1C                   ENDIF
052400970527     C*--------------------
052500970527     C* NAZIONE + CAP + TERMINAL PARTENZA
052600970527     C*--------------------
052700970527     C* INSERIMENTO -> RECORD INESISTENTE
052800970527IF  1C     ERRL00        IFEQ      '0'
052900970527IF  2C     DEPOP1        IFEQ      'F06'
053000970527     C                   Z-ADD     D00VER        KCEVER
053100970527     C                   MOVEL     VIDNAR        KCENAR
053200970527     C                   MOVEL     VIDCAP        KCECAP
053300970527     C                   MOVEL     VIDTFP        KCETFP
053400970527     C     KEYCPE        CHAIN(N)  AZCPE01L                           99
053500970527IF  3C     *IN99         IFEQ      *OFF
053600970527     C                   SETON                                        606162
053700970527     C                   MOVEL     '1'           ERRL00
053800970527     C                   MOVEL     MSG(8)        DSMSMS
053900970527E   3C                   ENDIF
054000970527E   2C                   ENDIF
054100970527E   1C                   ENDIF
054200970527     C*--------------------
054300970606     C* LINEA ARRIVO STANDARD
054400970527     C*--------------------
054500970527IF  1C     ERRL00        IFEQ      '0'
054600970527     C                   MOVEL     VIDLNA        WLIN
054700970527     C                   MOVEL     *BLANKS       WDLIN
054800970527     C                   EXSR      CHKLIN
054900970527     C                   MOVEL     WLIN          VIDLNA
055000970527     C                   MOVEL     WDLIN         VDDLNA
055100970527IF  2C     ERRL00        IFEQ      '1'
055200970527     C                   SETON                                        63
055300970527     C                   MOVEL     MSG(15)       DSMSMS
055400970527E   2C                   ENDIF
055500970527E   1C                   ENDIF
055600970527     C*--------------------
055700970606     C* ZONA CONSEGNA STANDARD
055800970527     C*--------------------
055900970527IF  1C     ERRL00        IFEQ      '0'
056000970527     C                   MOVEL     VIDLNA        WLIN
056100970527     C                   Z-ADD     VIDZNC        WZNC
056200970527     C                   EXSR      CHKZNC
056300970527IF  2C     ERRL00        IFEQ      '1'
056400970527     C                   SETON                                        64
056500970527     C                   MOVEL     MSG(16)       DSMSMS
056600970527E   2C                   ENDIF
056700060206if  2c                   if        §03nuc = 'S'
056800060206     c                   movel     Msg(17)       DSmsms
056900060206     c                   eval      *in64  = *on
057000060206     c                   eval      ErrL00 = *on
057100060206e   2c                   endif
057200970527E   1C                   ENDIF
057300970606     C*--------------------
057400970912     C* LINEA ARRIVO SOTTO
057500970606     C*--------------------
057600970606IF  1C     ERRL00        IFEQ      '0'
057700970606     C                   MOVEL     *BLANKS       VDDLOS
057800970606IF  2C     VIDLOS        IFNE      *ZEROS
057900970606     C     VIDLOS        ANDNE     *BLANKS
058000970606     C                   MOVEL     VIDLOS        WLIN
058100970606     C                   MOVEL     *BLANKS       WDLIN
058200970606     C                   EXSR      CHKLIN
058300970606     C                   MOVEL     WLIN          VIDLOS
058400970606     C                   MOVEL     WDLIN         VDDLOS
058500970606IF  3C     ERRL00        IFEQ      '1'
058600970606     C                   SETON                                        67
058700970606     C                   MOVEL     MSG(15)       DSMSMS
058800970606E   3C                   ENDIF
058900970606E   2C                   ENDIF
059000970606E   1C                   ENDIF
059100970606     C*--------------------
059200970912     C* ZONA DI CONSEGNA SOTTO
059300970606     C*--------------------
059400970606IF  1C     ERRL00        IFEQ      '0'
059500971126IF  2C     VIDLOS        IFGT      *ZEROS
059600970606     C                   MOVEL     VIDLOS        WLIN
059700970606     C                   Z-ADD     VIDZOS        WZNC
059800970606     C                   EXSR      CHKZNC
059900970606IF  3C     ERRL00        IFEQ      '1'
060000970606     C                   SETON                                        68
060100970606     C                   MOVEL     MSG(16)       DSMSMS
060200970606E   3C                   ENDIF
060300060206if  3c                   if        §03nuc = 'S'
060400060206     c                   movel     Msg(17)       DSmsms
060500060206     c                   eval      *in68  = *on
060600060206     c                   eval      ErrL00 = *on
060700060206e   3c                   endif
060800970606E   2C                   ENDIF
060900970606E   1C                   ENDIF
061000970527     C*--------------------
061100970912     C* LINEA ARRIVO OLTRE
061200970527     C*--------------------
061300970527IF  1C     ERRL00        IFEQ      '0'
061400971126IF  2C     VIDLOL        IFNE      *ZEROS
061500971126     C     VIDLOL        ANDNE     *BLANKS
061600970527     C                   MOVEL     VIDLOL        WLIN
061700970527     C                   MOVEL     *BLANKS       WDLIN
061800970527     C                   EXSR      CHKLIN
061900970527     C                   MOVEL     WLIN          VIDLOL
062000970527     C                   MOVEL     WDLIN         VDDLOL
062100971126IF  3C     ERRL00        IFEQ      '1'
062200970527     C                   SETON                                        65
062300970527     C                   MOVEL     MSG(15)       DSMSMS
062400971126E   3C                   ENDIF
062500971126E   2C                   ENDIF
062600970527E   1C                   ENDIF
062700970527     C*--------------------
062800970912     C* ZONA CONSEGNA OLTRE
062900970527     C*--------------------
063000970527IF  1C     ERRL00        IFEQ      '0'
063100971126IF  2C     VIDLOL        IFGT      *ZEROS
063200970527     C                   MOVEL     VIDLOL        WLIN
063300970527     C                   Z-ADD     VIDZOL        WZNC
063400970527     C                   EXSR      CHKZNC
063500971126IF  3C     ERRL00        IFEQ      '1'
063600970527     C                   SETON                                        66
063700970527     C                   MOVEL     MSG(16)       DSMSMS
063800971126E   3C                   ENDIF
063900060206if  3c                   if        §03nuc = 'S'
064000060206     c                   movel     Msg(17)       DSmsms
064100060206     c                   eval      *in66  = *on
064200060206     c                   eval      ErrL00 = *on
064300060206e   3c                   endif
064400971126E   2C                   ENDIF
064500970527E   1C                   ENDIF
064600971118     C*--------------------
064700971118     C* LINEA ARRIVO OLTRE 2
064800971118     C*--------------------
064900971118IF  1C     ERRL00        IFEQ      '0'
065000971126IF  2C     VIDLO2        IFNE      *ZEROS
065100971126     C     VIDLO2        ANDNE     *BLANKS
065200971118     C                   MOVEL     VIDLO2        WLIN
065300971118     C                   MOVEL     *BLANKS       WDLIN
065400971118     C                   EXSR      CHKLIN
065500971118     C                   MOVEL     WLIN          VIDLO2
065600971118     C                   MOVEL     WDLIN         VDDLO2
065700971126IF  3C     ERRL00        IFEQ      '1'
065800971118     C                   SETON                                        69
065900971118     C                   MOVEL     MSG(15)       DSMSMS
066000971126E   3C                   ENDIF
066100971126E   2C                   ENDIF
066200971118E   1C                   ENDIF
066300971118     C*--------------------
066400971118     C* ZONA CONSEGNA OLTRE
066500971118     C*--------------------
066600971118IF  1C     ERRL00        IFEQ      '0'
066700971126IF  2C     VIDLO2        IFGT      *ZEROS
066800971118     C                   MOVEL     VIDLO2        WLIN
066900971118     C                   Z-ADD     VIDZO2        WZNC
067000971118     C                   EXSR      CHKZNC
067100971126IF  3C     ERRL00        IFEQ      '1'
067200971118     C                   SETON                                        70
067300971118     C                   MOVEL     MSG(16)       DSMSMS
067400971126E   3C                   ENDIF
067500060206if  2c                   if        §03nuc = 'S'
067600060206     c                   movel     Msg(17)       DSmsms
067700060206     c                   eval      *in70  = *on
067800060206     c                   eval      ErrL00 = *on
067900060206e   2c                   endif
068000971126E   2C                   ENDIF
068100971118E   1C                   ENDIF
068200970527     C*--------------------
068300970527     C* DECODIFICHE DATI CAPPARIO
068400970527     C*--------------------
068500970527     C* LINEA ARRIVO
068600970527IF  1C     ERRL00        IFEQ      '0'
068700970606IF  2C     VIDCLN        IFGT      *ZEROS
068800970527     C                   MOVEL     VIDCLN        WLIN
068900970527     C                   MOVEL     *BLANKS       WDLIN
069000970527     C                   EXSR      CHKLIN
069100970527     C                   MOVEL     WDLIN         VDDCLN
069200970606E   2C                   ENDIF
069300970606E   1C                   ENDIF
069400970912     C* LINEA ARRIVO SOTTO
069500970606IF  1C     ERRL00        IFEQ      '0'
069600970606IF  2C     VIDCLS        IFGT      *ZEROS
069700970606     C                   MOVEL     VIDCLS        WLIN
069800970606     C                   MOVEL     *BLANKS       WDLIN
069900970606     C                   EXSR      CHKLIN
070000970606     C                   MOVEL     WDLIN         VDDCLS
070100970606E   2C                   ENDIF
070200970606E   1C                   ENDIF
070300970912     C* LINEA ARRIVO OLTRE
070400970606IF  1C     ERRL00        IFEQ      '0'
070500970606IF  2C     VIDCLO        IFGT      *ZEROS
070600970527     C                   MOVEL     VIDCLO        WLIN
070700970527     C                   MOVEL     *BLANKS       WDLIN
070800970527     C                   EXSR      CHKLIN
070900970527     C                   MOVEL     WDLIN         VDDCLO
071000970606E   2C                   ENDIF
071100970606E   1C                   ENDIF
071200971118     C* LINEA ARRIVO OLTRE 2
071300971118IF  1C     ERRL00        IFEQ      '0'
071400971118IF  2C     VIDCL2        IFGT      *ZEROS
071500971118     C                   MOVEL     VIDCL2        WLIN
071600971118     C                   MOVEL     *BLANKS       WDLIN
071700971118     C                   EXSR      CHKLIN
071800971118     C                   MOVEL     WDLIN         VDDCL2
071900971118E   2C                   ENDIF
072000971118E   1C                   ENDIF
072100000601     C*
072200000601     C     ERRL00        IFEQ      '0'
072300000601     C                   EXSR      DECOPT
072400000601     C                   ENDIF
072500970527     C*
072600970527     C                   ENDSR
072700000601     C*------------------------------------------------------------------------*
072800000601     C* DECOPT - CONTROLLA CAMPI E DECODIFICA ESTENSIONI POSTE
072900000601     C*------------------------------------------------------------------------*
073000000601     C     DECOPT        BEGSR
073100000601     C*
073200000601     C*--------------------
073300000601     C* CONTROLLO NAZIONE X POSTE
073400000601     C*--------------------
073500000601     C     ERRL00        IFEQ      '0'
073600000601     C     VIDNAR        IFEQ      *BLANKS
073700000601     C                   SETOFF                                       93
073800000601     C                   ELSE
073900000601     C                   SETON                                        93
074000000601     C                   ENDIF
074100000601     C                   ENDIF
074200000601     C*
074300000601     C* I CONTROLLI SEGUENTI SONO SUBORDINATI ALLA VALIDITA' DELL'ESTENSIONE CAPPARIO X POSTE
074400000601     C*
074500000601     C     *IN93         IFEQ      *OFF
074600000601     C*--------------------
074700000601     C* LINEA ARRIVO POSTE
074800000601     C*--------------------
074900000601IF  1C     ERRL00        IFEQ      '0'
075000000601     C                   MOVEL     *BLANKS       VDPLNA
075100000601     C                   MOVEL     VIPLNA        WLIN
075200000601     C                   MOVEL     *BLANKS       WDLIN
075300000601     C                   EXSR      CHKLIN
075400000601     C                   MOVEL     WLIN          VIPLNA
075500000601     C                   MOVEL     WDLIN         VDPLNA
075600000601IF  2C     ERRL00        IFEQ      '1'
075700000601     C                   SETON                                        55
075800000601     C                   MOVEL     MSG(15)       DSMSMS
075900000601E   2C                   ENDIF
076000000601E   1C                   ENDIF
076100000601     C*--------------------
076200000601     C* ZONA DI CONSEGNA POSTE
076300000601     C*--------------------
076400000601IF  1C     ERRL00        IFEQ      '0'
076500000601     C                   MOVEL     VIPLNA        WLIN
076600000601     C                   Z-ADD     VIPZNC        WZNC
076700000601     C                   EXSR      CHKZNC
076800000601IF  2C     ERRL00        IFEQ      '1'
076900000601     C                   SETON                                        54
077000000601     C                   MOVEL     MSG(16)       DSMSMS
077100000601E   2C                   ENDIF
077200060206if  2c                   if        §03nuc = 'S'
077300060206     c                   movel     Msg(17)       DSmsms
077400060206     c                   eval      *in54  = *on
077500060206     c                   eval      ErrL00 = *on
077600060206e   2c                   endif
077700000601E   1C                   ENDIF
077800000601     C*
077900000601     C                   ENDIF
078000000601     C*
078100000601     C                   ENDSR
078200970527     C*------------------------------------------------------------------------*
078300970527     C* CHKNAR - CONTROLLA LA NAZIONE
078400970527     C*------------------------------------------------------------------------*
078500970527     C     CHKNAR        BEGSR
078600970527     C*
078700970527     C     *LIKE         DEFINE    VIDNAR        WNAR
078800970527     C     *LIKE         DEFINE    VDDNAR        WDNAR
078900970527     C     *LIKE         DEFINE    §15CPR        WCPR
079000970527     C*
079100970527     C* RICHIESTO ELENCO
079200970527     C                   SETOFF                                           28
079300970527     C     '?'           SCAN      WNAR                                   28
079400970527IF  1C     *IN28         IFEQ      *ON
079500020906     C                   Z-ADD     1             §KUT              1 0
079600970527     C                   MOVEL     '15'          §COD              2
079700970527     C                   MOVEL     *BLANKS       §KEY              8
079800970527     C                   MOVEL     *BLANKS       §DES             30
079900970527     C                   CALL      'X§TABER'
080000970527     C                   PARM                    §KUT
080100970527     C                   PARM                    §COD
080200970527     C                   PARM                    §KEY
080300970527     C                   PARM                    §DES
080400970527     C                   MOVEL     §KEY          WNAR
080500970527     C                   MOVEL     §DES          WDNAR
080600970527E   1C                   ENDIF
080700970527     C*
080800970527     C* CONTROLLA ESISTENZA CODICE
080900020906     C                   MOVEL     1             KTBKUT
081000970527     C                   MOVEL     '15'          KTBCOD
081100970527     C                   MOVEL(P)  WNAR          KTBKEY
081200970527     C     KEYTAB        CHAIN     TABEL00F                           99
081300970527IF  1C     *IN99         IFEQ      *OFF
081400970527     C     TBLFLG        ANDEQ     *BLANKS
081500970527     C                   MOVEL     TBLUNI        DS15
081600970527     C                   MOVEL     §15DES        WDNAR
081700970527     C                   MOVEL     §15CPR        WCPR
081800970527     C*
081900970527     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
082000970527IF  2C     VIDNAR        IFEQ      'I  '
082100970527     C                   MOVEL     '1'           ERRL00
082200970527     C                   MOVEL     *BLANKS       WDNAR
082300970527E   2C                   ENDIF
082400970527X   1C                   ELSE
082500970527     C                   MOVEL     '1'           ERRL00
082600970527E   1C                   ENDIF
082700970527     C*
082800970527     C                   ENDSR
082900970527     C*------------------------------------------------------------------------*
083000970527     C* CHKCAP - CONTROLLA IL CAP
083100970527     C*------------------------------------------------------------------------*
083200970527     C     CHKCAP        BEGSR
083300970527     C*
083400970527     C     *LIKE         DEFINE    VIDCAP        WCAP
083500970626     C*
083600970626     C* CONTROLLA IL CAP
083700970626     C                   CLEAR                   DSSI95
083800970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
083900970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
084000970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
084100970626     C                   MOVEL     WCAP          I95CAP                         *CAP
084200970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
084300970626     C                   CALL      'TISI95R'
084400970626     C                   PARM                    DSSI95
084500970626IF  1C     O95ERR        IFNE      *BLANKS
084600970626     C                   MOVEL     '1'           ERRL00
084700970626E   1C                   ENDIF
084800970527     C*
084900970527     C                   ENDSR
085000970527     C*------------------------------------------------------------------------*
085100970528     C* CHKTFX - CONTROLLA IL TERMINAL
085200970527     C*------------------------------------------------------------------------*
085300970528     C     CHKTFX        BEGSR
085400970527     C*
085500970527     C     *LIKE         DEFINE    VIDTFP        WTFP
085600970527     C     *LIKE         DEFINE    VDDTFP        WDTFP
085700970527     C*
085800970527     C* RICHIESTO ELENCO
085900970527     C                   SETOFF                                           28
086000970527     C     '?'           SCAN      WTFP                                   28
086100970527IF  1C     *IN28         IFEQ      *ON
086200970527     C                   MOVEL     *BLANKS       §COD1             3
086300970527     C                   MOVEL     *BLANKS       §TIP              1
086400970527     C                   MOVEL     *BLANKS       §DES1            25
086500971203     C                   MOVEL     *BLANKS       §PRA              3
086600971203     C                   CALL      'TNSD24R'
086700970527     C                   PARM                    §COD1
086800970527     C                   PARM                    §TIP
086900970527     C                   PARM                    §DES1
087000971203     C                   PARM                    §PRA
087100970527     C                   MOVEL     §COD1         WTFP
087200970527     C                   MOVEL     §DES1         WDTFP
087300970527E   1C                   ENDIF
087400970527     C*
087500970527     C* SOLO CARATTERI NUMERICI
087600970527     C                   SETOFF                                       28
087700970527     C                   TESTN                   WTFP                 28
087800970527IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
087900970527     C                   MOVEL     '1'           ERRL00
088000970527X   1C                   ELSE                                                   *E'NUMERO DA TESTN
088100970527     C                   MOVE      WTFP          W001              1
088200970527IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
088300970527     C                   MOVEL     '1'           ERRL00
088400970527X   2C                   ELSE                                                   *NUMERO
088500971128     C*
088600971128     C* REPERISCE IL TERMINAL
088700971128     C                   CLEAR                   DSLV55
088800971128     C                   MOVEL     'P'           D55TPT
088900971128     C                   MOVEL     WTFP          D55LIN
089000971128     C                   Z-ADD     DATCOR        D55DRF
089100971128     C                   CALL      'FNLV55R'
089200971128     C                   PARM                    DSLV55
089300971128IF  3C     D55ERR        IFNE      *BLANKS
089400971128     C                   MOVEL     WTFP          D55TFP
089500971128E   3C                   ENDIF
089600970527     C*
089700970527     C* CONTROLLA ESISTENZA CODICE
089800000613     C                   CLEAR                   OG143
089900970527     C                   MOVEL     WTFP          KORFIL
090000970527     C     KEYORG        CHAIN     AZORG01L                           99
090100000613IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
090200000613     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
090300000613     C     ORGFAG        ANDNE     'V'                                          *NON VARIA
090400000613     C                   MOVEL     ORGDE3        OG143
090500000613IF  4C     ORGFIL        IFEQ      D55TFP                                       *TERMINAL PARTENZA
090600020627     C     §OGNTW        OREQ      'PPT'                                        *FILIALE POSTE
090700000613     C                   MOVEL     ORGDES        WDTFP                          *DESCRIZIONE
090800000613X   4C                   ELSE
090900000613     C                   MOVEL     '1'           ERRL00                         *ERRORE
091000000613E   3C                   ENDIF
091100970527X   3C                   ELSE
091200000613     C                   MOVEL     '1'           ERRL00                         *ERRORE
091300970527E   3C                   ENDIF
091400970527E   2C                   ENDIF
091500970527E   1C                   ENDIF
091600970527     C*
091700970527     C                   ENDSR
091800970527     C*------------------------------------------------------------------------*
091900970527     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
092000970527     C*------------------------------------------------------------------------*
092100970527     C     CHKLIN        BEGSR
092200970527     C*
092300970527     C     *LIKE         DEFINE    VIDLNA        WLIN
092400970527     C     *LIKE         DEFINE    VDDLNA        WDLIN
092500970527     C*
092600970527     C* RICHIESTO ELENCO
092700970527     C                   SETOFF                                           28
092800970527     C     '?'           SCAN      WLIN                                   28
092900970527IF  1C     *IN28         IFEQ      *ON
093000970527     C                   MOVEL     *BLANKS       §COD1             3
093100970527     C                   MOVEL     *BLANKS       §TIP              1
093200970527     C                   MOVEL     *BLANKS       §DES1            25
093300971203     C                   MOVEL     *BLANKS       §PRA              3
093400980409     C                   CALL      'TNSD24R'
093500970527     C                   PARM                    §COD1
093600970527     C                   PARM                    §TIP
093700970527     C                   PARM                    §DES1
093800971203     C                   PARM                    §PRA
093900970527     C                   MOVEL     §COD1         WLIN
094000970527     C                   MOVEL     §DES1         WDLIN
094100970527E   1C                   ENDIF
094200970527     C*
094300970527     C* SOLO CARATTERI NUMERICI
094400970527     C                   SETOFF                                       28
094500970527     C                   TESTN                   WLIN                 28
094600970527IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
094700970527     C                   MOVEL     '1'           ERRL00
094800970527X   1C                   ELSE                                                   *E'NUMERO DA TESTN
094900970527     C                   MOVE      WLIN          W001              1
095000970527IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
095100970527     C                   MOVEL     '1'           ERRL00
095200970527X   2C                   ELSE                                                   *NUMERO
095300970527     C*
095400970527     C* CONTROLLA ESISTENZA CODICE
095500970527     C                   MOVEL     WLIN          KORFIL
095600970527     C     KEYORG        CHAIN     AZORG01L                           99
095700970527IF  3C     *IN99         IFEQ      *ON                                          *NON TROVATA
095800970527     C     ORGFVA        ORNE      *BLANKS                                      *ANNULLATA
095900970527     C     ORGFAG        OREQ      'V'                                          *VARIA
096000970527     C                   MOVEL     '1'           ERRL00
096100970527X   3C                   ELSE
096200970527     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
096300970527E   3C                   ENDIF
096400970527E   2C                   ENDIF
096500970527E   1C                   ENDIF
096600970527     C*
096700970527     C                   ENDSR
096800970527     C*------------------------------------------------------------------------*
096900970527     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
097000970527     C*------------------------------------------------------------------------*
097100970527     C     CHKZNC        BEGSR
097200970527     C*
097300970527     C     *LIKE         DEFINE    VIDZNC        WZNC
097400970527     C*
097500970527     C* CONTROLLA ESISTENZA CODICE
097600020906     C                   Z-ADD     1             KTBKUT
097700970527     C                   MOVEL     '03'          KTBCOD
097800970527     C                   MOVEL     WLIN          WA5               5
097900970527     C                   MOVE      WZNC          WA5
098000970527     C                   MOVEL(P)  WA5           KTBKEY
098100970527     C     KEYTAB        CHAIN     TABEL00F                           99
098200970527IF  1C     *IN99         IFEQ      *OFF
098300970527     C     TBLFLG        ANDEQ     *BLANKS
098400970527     C                   MOVEL     TBLUNI        DS03
098500970527X   1C                   ELSE
098600970527     C                   MOVEL     '1'           ERRL00
098700970527E   1C                   ENDIF
098800970527     C*
098900970527     C                   ENDSR
099000970526     C*------------------------------------------------------------------------*
099100970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
099200970526     C*------------------------------------------------------------------------*
099300970526     C     IREVID        BEGSR
099400970526     C* CAMPI
099500970527     C                   MOVEL     CPENAR        VIDNAR
099600970527     C                   MOVEL     CPECAP        VIDCAP
099700970527     C                   MOVEL     CPETFP        VIDTFP
099800970527     C                   MOVEL     CPELNA        VIDLNA
099900970527     C                   Z-ADD     CPEZNC        VIDZNC
100000970527     C                   MOVEL     CPELOL        VIDLOL
100100970527     C                   Z-ADD     CPEZOL        VIDZOL
100200970606     C                   MOVEL     CPELOS        VIDLOS
100300970606     C                   Z-ADD     CPEZOS        VIDZOS
100400971118     C                   MOVEL     CPELO2        VIDLO2
100500971118     C                   Z-ADD     CPEZO2        VIDZO2
100600000601     C*
100700000601     C*--- INPOSTAZIONI CAMPI DI ESTENSIONE CAPPARIO X POSTE
100800000601     C     CPELOS        IFEQ      *ZEROS
100900000601     C                   MOVEL     CPELNA        VIPLNA
101000000601     C                   Z-ADD     CPEZNC        VIPZNC
101100000601     C                   ELSE
101200000601     C                   MOVEL     CPELOS        VIPLNA
101300000601     C                   Z-ADD     CPEZOS        VIPZNC
101400000601     C                   ENDIF
101500000601     C* ... E COMUNQUE
101600000601     C                   MOVE      PPELNA        VIPLNA
101700000601     C                   Z-ADD     PPEZNC        VIPZNC
101800000601     C*--- END POSTE
101900970527     C*
102000970526     C                   ENDSR
102100970526     C*------------------------------------------------------------------------*
102200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
102300970526     C*------------------------------------------------------------------------*
102400970526     C     AZZVID        BEGSR
102500970526     C* CAMPI
102600970603     C                   MOVEL     D00NAR        VIDNAR
102700970603     C                   MOVEL     D00CAP        VIDCAP
102800970527     C                   MOVEL     *ZEROS        VIDTFP
102900970527     C                   MOVEL     *ZEROS        VIDLNA
103000970527     C                   Z-ADD     *ZEROS        VIDZNC
103100970527     C                   MOVEL     *ZEROS        VIDLOL
103200970527     C                   Z-ADD     *ZEROS        VIDZOL
103300970606     C                   MOVEL     *ZEROS        VIDLOS
103400970606     C                   Z-ADD     *ZEROS        VIDZOS
103500971118     C                   MOVEL     *ZEROS        VIDLO2
103600971118     C                   Z-ADD     *ZEROS        VIDZO2
103700000601     C                   MOVEL     *BLANKS       VIPLNA
103800000601     C                   Z-ADD     *ZEROS        VIPZNC
103900970526     C* DECODIFICHE
104000970527     C                   MOVEL     *BLANKS       VDDNAR
104100970527     C                   MOVEL     *BLANKS       VDDTFP
104200970527     C                   MOVEL     *BLANKS       VDDLNA
104300970527     C                   MOVEL     *BLANKS       VDDLOL
104400970606     C                   MOVEL     *BLANKS       VDDLOS
104500971118     C                   MOVEL     *BLANKS       VDDLO2
104600000601     C                   MOVEL     *BLANKS       VDPLNA
104700970527     C* CAMPI CAPPARIO
104800970527     C                   Z-ADD     *ZEROS        VIDCLN
104900970527     C                   Z-ADD     *ZEROS        VIDCZN
105000970606     C                   MOVEL     *BLANKS       VDDCLN
105100971118     C                   Z-ADD     *ZEROS        VIDCLO
105200970527     C                   Z-ADD     *ZEROS        VIDCZO
105300971118     C                   MOVEL     *BLANKS       VDDCLO
105400970606     C                   Z-ADD     *ZEROS        VIDCLS
105500970606     C                   Z-ADD     *ZEROS        VIDCZS
105600971118     C                   MOVEL     *BLANKS       VDDCLS
105700971118     C                   Z-ADD     *ZEROS        VIDCL2
105800971118     C                   Z-ADD     *ZEROS        VIDCZ2
105900971118     C                   MOVEL     *BLANKS       VDDCL2
106000970526     C*
106100970526     C                   ENDSR
106200970526     C*------------------------------------------------------------------------*
106300970527     C* IMPCPE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
106400970526     C*------------------------------------------------------------------------*
106500970527     C     IMPCPE        BEGSR
106600970526     C*
106700970527     C                   MOVEL     VIDLNA        CPELNA
106800970527     C                   Z-ADD     VIDZNC        CPEZNC
106900970527     C                   MOVEL     VIDLOL        CPELOL
107000970527     C                   Z-ADD     VIDZOL        CPEZOL
107100970606     C                   MOVEL     VIDLOS        CPELOS
107200970606     C                   Z-ADD     VIDZOS        CPEZOS
107300971118     C                   MOVEL     VIDLO2        CPELO2
107400971118     C                   Z-ADD     VIDZO2        CPEZO2
107500970526     C*
107600970526     C                   ENDSR
107700000601     C*------------------------------------------------------------------------*
107800000601     C* IMPPPE - IMPOSTA IL RECORD DA SCRIVERE/MODIFICARE SUL FILE ESTENSIONI ECCEZIONI X POSTE
107900000601     C*------------------------------------------------------------------------*
108000000601     C     IMPPPE        BEGSR
108100000601     C*
108200000601     C   96              Z-ADD     CPEVER        PPEVER
108300000601     C   96              MOVEL     CPENAR        PPENAR
108400000601     C   96              MOVEL     CPECAP        PPECAP
108500000601     C   96              MOVEL     CPETFP        PPETFP
108600000601     C                   MOVE      VIPLNA        PPELNA
108700000601     C                   Z-ADD     VIPZNC        PPEZNC
108800000601     C                   MOVEL     *BLANKS       PPEATB
108900000601     C                   MOVEL     *BLANKS       PPEFTR
109000000601     C   96              Z-ADD     *ZEROS        PPEDTR
109100000601     C   96              MOVEL     *BLANKS       PPEFL1
109200000601     C   96              WRITE     AZPPE000
109300000601     C  N96              UPDATE    AZPPE000
109400000601     C*
109500000601     C                   ENDSR
109600970526     C*------------------------------------------------------------------------*
109700970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
109800970526     C*------------------------------------------------------------------------*
109900970526     C     IMPFUN        BEGSR
110000970526     C*
110100970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
110200970526IF  1C     DEPOP1        IFEQ      'O05'
110300970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
110400970526     C                   MOVEL     'N'           FUDATT
110500970526     C                   MOVEL     FUDRIG        FUD(6)
110600970526E   1C                   ENDIF
110700970526     C*
110800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
110900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
111000970526     C                   EXSR      FUNGES
111100970526     C*
111200970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
111300970526     C*---
111400970526     C* 90 -> PROTEZIONE CHIAVE
111500970526     C* 91 -> PROTEZIONE TOTALE
111600970526     C*---
111700970527IF  1C     DEPOP0        IFEQ      'E02'                                        G E S T I O N E
111800970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
111900970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
112000970526     C                   SETOFF                                       91
112100970526E   2C                   ENDIF
112200970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
112300970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
112400970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
112500970526E   2C                   ENDIF
112600970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
112700970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
112800970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
112900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
113000970526     C                   SETON                                        91
113100970526E   2C                   ENDIF
113200970526X   1C                   ELSE                                                   *I N T E R R O G A
113300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
113400970526     C                   SETON                                        91
113500970526E   1C                   ENDIF
113600970526     C*
113700970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
113800970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
113900970527     C                   Z-ADD     D00VER        KCEVER
114000970527     C                   MOVEL     D00NAR        KCENAR
114100970527     C                   MOVEL     D00CAP        KCECAP
114200970527     C                   Z-ADD     D00TFP        KCETFP
114300000601     C     KEYCPE        CHAIN(N)  AZPPE01L                           99
114400970527     C     KEYCPE        CHAIN(N)  AZCPE01L                           99
114500970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
114600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
114700970526     C                   MOVEL     MSG(6)        DSMSMS
114800970526X   2C                   ELSE
114900970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
115000980128IF  3C     DEPOP1        IFEQ      'O05'
115100980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
115200980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
115300980128E   3C                   ENDIF
115400970526E   2C                   ENDIF
115500970526X   1C                   ELSE                                                   *INSERIMENTO
115600970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
115700970526E   1C                   ENDIF
115800970526     C*
115900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
116000970526IF  1C     DEPOP1        IFEQ      'O02'
116100970526     C                   MOVEL     DEC(1)        VIDFUN
116200970526E   1C                   ENDIF
116300970526IF  1C     DEPOP1        IFEQ      'O04'
116400970526     C                   MOVEL     DEC(2)        VIDFUN
116500970526E   1C                   ENDIF
116600970526IF  1C     DEPOP1        IFEQ      'O05'
116700970526     C                   MOVEL     DEC(3)        VIDFUN
116800970526E   1C                   ENDIF
116900970526IF  1C     DEPOP1        IFEQ      'O07'
117000970526     C                   MOVEL     DEC(4)        VIDFUN
117100970526E   1C                   ENDIF
117200970526IF  1C     DEPOP1        IFEQ      'F06'
117300970526     C                   MOVEL     DEC(5)        VIDFUN
117400970526E   1C                   ENDIF
117500970526     C*
117600970526     C                   ENDSR
117700970626     C*------------------------------------------------------------------------*
117800970626     C* FINSR - OPERAZIONI FINALI
117900970626     C*------------------------------------------------------------------------*
118000970626     C     FINSR         BEGSR
118100971128     C*
118200970704     C                   CLEAR                   DSSI95
118300970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
118400970626     C                   CALL      'TISI95R'
118500970626     C                   PARM                    DSSI95
118600971128     C*
118700971128     C                   CLEAR                   DSLV55
118800971128     C                   MOVEL     'C'           D55TLA
118900971128     C                   CALL      'FNLV55R'
119000971128     C                   PARM                    DSLV55
119100970626     C*
119200970626     C                   ENDSR
119300970526     C*------------------------------------------------------------------------*
119400970526     C* *INZSR - OPERAZIONI INIZIALI
119500970526     C*------------------------------------------------------------------------*
119600970526     C     *INZSR        BEGSR
119700970526     C*---
119800970526     C* RICEVIMENTO PARAMETRI
119900970526     C*---
120000970526     C     *ENTRY        PLIST
120100970612     C                   PARM                    KPJBA
120200970612     C                   PARM                    DSSI00
120300970526     C*
120400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
120500970526     C     XMSGPA        PLIST
120600970526     C                   PARM                    DSMSMS
120700970526     C                   PARM                    DSMSPG
120800970526     C                   PARM                    DSMSKE
120900970526     C                   PARM                    DSMSER
121000970526     C*
121100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
121200970526     C                   CALL      'XKEY'
121300970526     C                   PARM                    DSKEY
121400970526     C*---
121500970526     C* VARIABILI NON RIFERITE AL DATA BASE
121600970526     C*---
121700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
121800970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
121900970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
122000970526     C     *LIKE         DEFINE    D00OP0        WOP0
122100970526     C     *LIKE         DEFINE    D00OP1        WOP1
122200970526     C*---
122300970526     C* VARIABILI DI CONTROLLO
122400970526     C*---
122500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
122600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
122700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
122800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
122900970526     C*
123000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
123100970527     C                   MOVEL     'TISI12R'     DSMSPG
123200970527     C                   MOVEL     'TISI12R'     MSGQUE
123300970526     C                   MOVEL     *BLANKS       MSGKEY
123400970526     C                   MOVEL     *BLANKS       DSMSMS
123500970526     C*---
123600970526     C* VARIABILI RIFERITE AL DATA BASE
123700970526     C*---
123800970527     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
123900970527     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
124000970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
124100970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
124200970527     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
124300970527     C     *LIKE         DEFINE    CPENAR        KCENAR
124400970527     C     *LIKE         DEFINE    CPECAP        KCECAP
124500970527     C     *LIKE         DEFINE    CPETFP        KCETFP
124600970526     C*---
124700970526     C* CHIAVI DI LETTURA
124800970526     C*---
124900970527     C* LETTURA AZORG01L - COMPLETA
125000970527     C     KEYORG        KLIST
125100970528     C                   KFLD                    KORFIL                         *FILIALE
125200970526     C* LETTURA TABEL00F - COMPLETA
125300970526     C     KEYTAB        KLIST
125400970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
125500970526     C                   KFLD                    KTBCOD                         *TABELLA
125600970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
125700970527     C* LETTURA AZCPE01L - COMPLETA
125800970527     C     KEYCPE        KLIST
125900970527     C                   KFLD                    KCEVER                         *VERSIONE
126000970527     C                   KFLD                    KCENAR                         *NAZIONE
126100970527     C                   KFLD                    KCECAP                         *CAP
126200970527     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
126300970526     C*
126400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
126500970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
126600970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
126700971128     C*
126800971128     C* CALCOLA LA DATA CORRENTE
126900971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
127000971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
127100971128     C                   Z-ADD     WN8           G08DAT
127200971128     C                   Z-ADD     *ZEROS        G08INV
127300971128     C                   MOVEL     '0'           G08ERR
127400971128     C                   CALL      'XSRDA8'
127500971128     C                   PARM                    WLBDA8
127600971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
127700060206      *
127800060206      * REPERISCE I DATI DELL'UTENTE
127900060206     c                   exsr      RepDatiUte
128000060206     c                   movel     RSUT          DSfirs
128100970526     C*
128200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
128300970526     C                   EXSR      IMPFUN
128400970526     C*
128500970526     C                   ENDSR
128600060206      *---------------------------------------------------------------*
128700060206      * REPERISCE DATI UTENTE                                         *
128800060206      *---------------------------------------------------------------*
128900060206     c     RepDatiUte    BEGSR
129000060206      *
129100060206     c     *dtaara       define    §azute        AZUTEds
129200060206     c     *dtaara       define    §datiute      DDatiUte
129300060206      *
129400060206     c                   in(E)     *dtaara
129500060206     c                   IF        %ERROR or RSUT = *blanks
129600060206     c                   clear                   Tibs34Ds
129700060206     c                   call      'TIBS34R'
129800060206     c                   parm                    Tibs34Ds
129900060206     c                   in        *dtaara
130000060206     c                   ENDIF
130100060206      *
130200060206     c                   ENDSR
130300970526     C*------------------------------------------------------------------------*
130400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
130500970526     C*------------------------------------------------------------------------*
130600970526     C     FUNGES        BEGSR
130700970526     C*
130800970526     C* PULISCO CAMPI DI LAVORO
130900970526     C                   Z-ADD     0             W                 5 0
131000970526     C*
131100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
131200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
131300970526     C     1             DO        24            I
131400970526     C                   MOVEL     FUD(I)        FUDRIG
131500970526     C*
131600970526     C* PULIZIA FUNZIONI PRECEDENTI
131700970526     C                   MOVEL     *BLANKS       FUV(I)
131800970526     C*
131900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
132000970526     C* (SALTA F24)
132100970526     C     FUDATT        IFEQ      'S'
132200970526     C     FUDFUN        ANDNE     '24'
132300970526     C                   ADD       1             W
132400970526     C                   MOVEL     FUDDES        FUV(W)
132500970526     C                   END
132600970526     C                   END                                                    DO WHILE
132700970526     C*
132800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
132900970526     C* ALTRIMENTI DISATTIVO F24
133000970526     C     1             DO        24            I
133100970526     C                   MOVEL     FUD(I)        FUDRIG
133200970526     C*
133300970526     C     FUDFUN        IFEQ      '24'
133400970526     C     W             IFGT      6
133500970526     C                   MOVEL     'S'           FUDATT
133600970526     C                   ELSE
133700970526     C                   MOVEL     *BLANKS       FUDATT
133800970526     C                   END                                                    W GT 8
133900970526     C                   MOVEL     FUDRIG        FUD(I)
134000970526     C                   END
134100970526     C                   END                                                    DO WHILE
134200970526     C*
134300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
134400970526     C     W             DIV       6             I
134500970526     C                   MVR                     Z
134600970526     C*
134700970526     C     Z             IFNE      0
134800970526     C     I             ADD       1             Z
134900970526     C                   ELSE
135000970526     C                   Z-ADD     I             Z                 5 0
135100970526     C                   END
135200970526     C*
135300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
135400970526     C                   ADD       1             FUNGRU
135500970526     C     FUNGRU        IFGT      Z
135600970526     C     FUNGRU        ORLT      1
135700970526     C                   Z-ADD     1             FUNGRU
135800970526     C                   END
135900970526     C*
136000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
136100970526     C     FUNGRU        IFEQ      1
136200970526     C                   MOVEL     FUV(1)        FUN001
136300970526     C                   MOVEL     FUV(2)        FUN002
136400970526     C                   MOVEL     FUV(3)        FUN003
136500970526     C                   MOVEL     FUV(4)        FUN004
136600970526     C                   MOVEL     FUV(5)        FUN005
136700970526     C                   MOVEL     FUV(6)        FUN006
136800970526     C                   END
136900970526     C*
137000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
137100970526     C     FUNGRU        IFEQ      2
137200970526     C                   MOVEL     FUV(7)        FUN001
137300970526     C                   MOVEL     FUV(8)        FUN002
137400970526     C                   MOVEL     FUV(9)        FUN003
137500970526     C                   MOVEL     FUV(10)       FUN004
137600970526     C                   MOVEL     FUV(11)       FUN005
137700970526     C                   MOVEL     FUV(12)       FUN006
137800970526     C                   END
137900970526     C*
138000970526     C* TERZO GRUPPO DI 6 FUNZIONI
138100970526     C     FUNGRU        IFEQ      3
138200970526     C                   MOVEL     FUV(13)       FUN001
138300970526     C                   MOVEL     FUV(14)       FUN002
138400970526     C                   MOVEL     FUV(15)       FUN003
138500970526     C                   MOVEL     FUV(16)       FUN004
138600970526     C                   MOVEL     FUV(17)       FUN005
138700970526     C                   MOVEL     FUV(18)       FUN006
138800970526     C                   END
138900970526     C*
139000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
139100970526     C     FUNGRU        IFEQ      4
139200970526     C                   MOVEL     FUV(19)       FUN001
139300970526     C                   MOVEL     FUV(20)       FUN002
139400970526     C                   MOVEL     FUV(21)       FUN003
139500970526     C                   MOVEL     FUV(22)       FUN004
139600970526     C                   MOVEL     *BLANKS       FUN005
139700970526     C                   MOVEL     *BLANKS       FUN006
139800970526     C                   END
139900970526     C*
140000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
140100970526     C                   MOVEL     *BLANKS       FUN007
140200970526     C                   MOVEL     *BLANKS       FUN008
140300970526     C*
140400970526     C     1             DO        24            I
140500970526     C                   MOVEL     FUD(I)        FUDRIG
140600970526     C     FUDFUN        IFEQ      '24'
140700970526     C     FUDATT        ANDEQ     'S'
140800970526     C                   MOVEL     FUDDES        FUN008
140900970526     C                   END
141000970526     C                   END                                                    DO
141100970526     C*
141200970526     C                   ENDSR
141300970526     C*------------------------------------------------------------------------*
141400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
141500970526     C*------------------------------------------------------------------------*
141600970526     C     FUNCON        BEGSR
141700970526     C*
141800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
141900970526     C                   EXSR      FUNVAR
142000970526     C*
142100970526     C* PULISCO CAMPI DI LAVORO
142200970526     C                   Z-ADD     0             I                 5 0
142300970526     C                   Z-ADD     0             W                 5 0
142400970526     C*
142500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
142600970526     C     FUNKEY        IFNE      *BLANKS
142700970526     C     1             DO        24            I
142800970526     C                   MOVEL     FUD(I)        FUDRIG
142900970526     C*
143000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
143100970526     C     FUNKEY        IFEQ      FUDFUN
143200970526     C     FUDATT        ANDEQ     'S'
143300970526     C                   Z-ADD     I             W
143400970526     C                   END
143500970526     C                   END                                                    DO WHILE
143600970526     C*
143700970526     C* FUNZIONE NON ATTIVA (ERRORE)
143800970526     C     W             IFEQ      0
143900970526     C*
144000970526     C* EMISSIONE MESSAGGIO DI ERRORE
144100970526     C                   MOVEL     MSG(1)        DSMSMS
144200970526     C*
144300970526     C                   END                                                    W=0
144400970526     C                   END                                                    FUNKEY NE *BLANKS
144500970526     C*
144600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
144700970526     C     W             IFNE      0
144800970526     C     FUNKEY        OREQ      *BLANKS
144900970526     C     FUNKEY        IFEQ      *BLANKS
145000970526     C                   EXSR      ENTGES
145100970526     C                   END
145200970526     C     FUNKEY        IFEQ      '03'
145300970526     C                   EXSR      F03GES
145400970526     C                   END
145500970526     C     FUNKEY        IFEQ      '06'
145600970526     C                   EXSR      F06GES
145700970526     C                   END
145800970526     C     FUNKEY        IFEQ      '12'
145900970526     C                   EXSR      F12GES
146000970526     C                   END
146100970526     C     FUNKEY        IFEQ      '24'
146200970526     C                   EXSR      F24GES
146300970526     C                   END
146400970526     C                   END
146500970526     C*
146600970526     C                   ENDSR
146700970526     C*------------------------------------------------------------------------*
146800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
146900970526     C*------------------------------------------------------------------------*
147000970526     C     FUNVAR        BEGSR
147100970526     C*
147200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
147300970526     C*
147400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
147500970526     C     KEY           IFEQ      ENTER
147600970526     C                   MOVEL     *BLANKS       FUNKEY
147700970526     C                   END
147800970526     C     KEY           IFEQ      F1
147900970526     C                   MOVEL     '01'          FUNKEY
148000970526     C                   END
148100970526     C     KEY           IFEQ      F2
148200970526     C                   MOVEL     '02'          FUNKEY
148300970526     C                   END
148400970526     C     KEY           IFEQ      F3
148500970526     C                   MOVEL     '03'          FUNKEY
148600970526     C                   END
148700970526     C     KEY           IFEQ      F4
148800970526     C                   MOVEL     '04'          FUNKEY
148900970526     C                   END
149000970526     C     KEY           IFEQ      F5
149100970526     C                   MOVEL     '05'          FUNKEY
149200970526     C                   END
149300970526     C     KEY           IFEQ      F6
149400970526     C                   MOVEL     '06'          FUNKEY
149500970526     C                   END
149600970526     C     KEY           IFEQ      F7
149700970526     C                   MOVEL     '07'          FUNKEY
149800970526     C                   END
149900970526     C     KEY           IFEQ      F8
150000970526     C                   MOVEL     '08'          FUNKEY
150100970526     C                   END
150200970526     C     KEY           IFEQ      F9
150300970526     C                   MOVEL     '09'          FUNKEY
150400970526     C                   END
150500970526     C     KEY           IFEQ      F10
150600970526     C                   MOVEL     '10'          FUNKEY
150700970526     C                   END
150800970526     C     KEY           IFEQ      F11
150900970526     C                   MOVEL     '11'          FUNKEY
151000970526     C                   END
151100970526     C     KEY           IFEQ      F12
151200970526     C                   MOVEL     '12'          FUNKEY
151300970526     C                   END
151400970526     C     KEY           IFEQ      F13
151500970526     C                   MOVEL     '13'          FUNKEY
151600970526     C                   END
151700970526     C     KEY           IFEQ      F14
151800970526     C                   MOVEL     '14'          FUNKEY
151900970526     C                   END
152000970526     C     KEY           IFEQ      F15
152100970526     C                   MOVEL     '15'          FUNKEY
152200970526     C                   END
152300970526     C     KEY           IFEQ      F16
152400970526     C                   MOVEL     '16'          FUNKEY
152500970526     C                   END
152600970526     C     KEY           IFEQ      F17
152700970526     C                   MOVEL     '17'          FUNKEY
152800970526     C                   END
152900970526     C     KEY           IFEQ      F18
153000970526     C                   MOVEL     '18'          FUNKEY
153100970526     C                   END
153200970526     C     KEY           IFEQ      F19
153300970526     C                   MOVEL     '19'          FUNKEY
153400970526     C                   END
153500970526     C     KEY           IFEQ      F20
153600970526     C                   MOVEL     '20'          FUNKEY
153700970526     C                   END
153800970526     C     KEY           IFEQ      F21
153900970526     C                   MOVEL     '21'          FUNKEY
154000970526     C                   END
154100970526     C     KEY           IFEQ      F22
154200970526     C                   MOVEL     '22'          FUNKEY
154300970526     C                   END
154400970526     C     KEY           IFEQ      F23
154500970526     C                   MOVEL     '23'          FUNKEY
154600970526     C                   END
154700970526     C     KEY           IFEQ      F24
154800970526     C                   MOVEL     '24'          FUNKEY
154900970526     C                   END
155000970526     C     KEY           IFEQ      ROLLUP
155100970526     C                   MOVEL     'RU'          FUNKEY
155200970526     C                   END
155300970526     C     KEY           IFEQ      HELP
155400970526     C                   MOVEL     'HP'          FUNKEY
155500970526     C                   END
155600970526     C                   ENDSR
155700970526     C*------------------------------------------------------------------------*
155800970526     C* NOTE COSTRUZIONE SCHIERE
155900970526     C*------------------------------------------------------------------------*
156000970526     C*
156100970526     C* -MSG-MESSAGGI
156200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
156300970526     C* (NON TOCCARE MSG,1 E MSG,2)
156400970526     C*
156500970526     C* -CMD-COMANDI
156600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
156700970526     C*
156800970526     C* -OPD-OPZIONI
156900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
157000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
157100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
157200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
157300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
157400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
157500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
157600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
157700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
157800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
157900970526     C*
158000970526     C* -FUD-FUNZIONI
158100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
158200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
158300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
158400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
158500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
158600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
158700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
158800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
158900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
159000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
159100970526     O*------------------------------------------------------------------------*
159200970526** DEC - DECODIFICHE
159300970526    MODIFICA                                                      1
159400970526  ANNULLAMENTO                                                    2
159500970526VISUALIZZAZIONE                                                   3
159600970526   RIPRISTINO                                                     4
159700970526  INSERIMENTO                                                     5
159800970526** MSG - MESSAGGI
159900970527TISI12R- Tasto funzionale non gestito                                          1
160000970527TISI12R- Inserimento effettuato                                                2
160100970527TISI12R- Modifica effettuata                                                   3
160200970527TISI12R- Annullamento effettuato                                               4
160300970527TISI12R- Ripristino effettuato                                                 5
160400970527TISI12R- Richiesta funzione per record inesistente                             6
160500970527TISI12R- Record momentaneamente allocato: riprovare più tardi                  7
160600970527TISI12R- Richiesta funzione per record esistente                               8
160700000601TISI12R- 9
160800000601TISI12R- 10                                                                   10
160900970527TISI12R- Nazione errata                                                       11
161000970626TISI12R- CAP errato o inesistente                                             12
161100970626TISI12R- 13
161200970527TISI12R- Terminal partenza errato                                             14
161300970527TISI12R- Linea di arrivo errata                                               15
161400970527TISI12R- Zona di consegna errata                                              16
161500060206TISI12R- Zona di consegna NON abilitata                                       17
161600970527TISI12R- 18
161700970527TISI12R- 19
161800970527TISI12R- 20
161900970526** FUD - FUNZIONI
162000970526                   -        (S/N)=N                        funz.01¬
162100970526                   -        (S/N)=N                        funz.02¬
162200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
162300970526                   -        (S/N)=N                        funz.04¬
162400970526                   -        (S/N)=N                        funz.05¬
162500970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
162600970527                   -        (S/N)=N                        funz.07¬
162700970527                   -        (S/N)=N                        funz.08¬
162800970527                   -        (S/N)=N                        funz.09¬
162900970527                   -        (S/N)=N                        funz.10¬
163000970526                   -        (S/N)=N                        funz.11¬
163100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
163200970526                   -        (S/N)=N                        funz.13¬
163300970526                   -        (S/N)=N                        funz.14¬
163400970526                   -        (S/N)=N                        funz.15¬
163500970526                   -        (S/N)=N                        funz.16¬
163600970526                   -        (S/N)=N                        funz.17¬
163700970526                   -        (S/N)=N                        funz.18¬
163800970526                   -        (S/N)=N                        funz.19¬
163900970526                   -        (S/N)=N                        funz.20¬
164000970526                   -        (S/N)=N                        funz.21¬
164100970526                   -        (S/N)=N                        funz.22¬
164200970526                   -        (S/N)=N                        funz.23¬
164300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
