000100970526     H*------------------------------------------------------------------------*
000200061221     H* CAPPARIO - GESTIONE ECCEZIONI LINEE/ZONE
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
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900970526     D*------------
002000970526     D* DECODIFICHE
002100970526     D*------------
002200970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* MESSAGGI
002500970526     D*------------
002600970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* DESCRIZIONI TASTI FUNZIONALI
002900970526     D*------------
003000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* FUNZIONI DA CARICARE A VIDEO
003300970526     D*------------
003400970526     D FUV             S             19    DIM(24)
003500970526     D*------------------------------------------------------------------------*
003600970526     D* INPUT
003700970526     D*------------------------------------------------------------------------*
003800970526     D*------------
003900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004000970526     D*------------
004100970526     D FUDDS           DS
004200970526     D  FUDDE1                 1      1
004300970526     D  FUDFUN                 2      3
004400970526     D  FUDDE2                 4     19
004500970526     D  FUDFI0                20     34
004600970526     D  FUDATT                35     35
004700970526     D  FUDFI1                36     80
004800970526     D  FUDDES                 1     19
004900970526     D  FUDRIG                 1     80
005000970526     D*------------
005100970526     D* DS GESTIONE TASTI FUNZIONALI
005200970526     D*------------
005300970526     D DSKEY         E DS
005400970526     D DEVDS1          DS
005500970526     D  KEY                  369    369
005600970526     D*------------
005700970526     D* DS "XMSG" - GESTIONE MESSAGGI
005800970526     D*------------
005900970526     D DSMSG         E DS
006000970626     D*------------------
006100970626     D* DS "TISI95R" - CONTROLLO CAP
006200970626     D*------------------
006300970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
006400970527     D*------------------
006500970527     D* DS NAZIONI
006600970527     D*------------------
006700970527     D DS15          E DS
006800970527     D*------------------
006900970527     D* DS ZONE DI CONSEGNA
007000970527     D*------------------
007100970527     D DS03          E DS
007200000613     D*------------
007300000613     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
007400000613     D*------------
007500000613     D OG143         E DS
007600970527     D*------------
007700970527     D* DS "TISI81R" - AGGIORNA UN CAP DAI SUOI ARCHIVI
007800970527     D*------------
007900970527     D DSSI81        E DS                  EXTNAME(TISI81DS)
008000971128     D*------------
008100971128     D* DS REPEIMENTO TERMINALs
008200971128     D*------------
008300971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
008400970526     D*------------
008500970526     D* DS DI PROCEDURA
008600970526     D*------------
008700970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
008800971128     D*------------------
008900971128     D* DS "XSRDA8" - CONTROLLA DATA (8)
009000971128     D*------------------
009100971128     D WLBDA8          DS                  INZ
009200971128     D  G08DAT                 1      8  0
009300971128     D  G08INV                 9     16  0
009400971128     D  G08ERR                17     17
009500971128     D  G08TGI                18     22  0
009600970526     D*------------
009700970526     D* ARCHITETTURA
009800970526     D*------------
009900970526     D KPJBA         E DS
010000060206      * - Parametri x Controllo profilo utenti
010100060206     d TIBS34ds      e ds
010200060206      * - Ds di riferimento al file esterno AZUTE00F
010300060206     d AZUTEds       e ds                  extname(AZUTE00F)
010400060206     d dUTE01        e ds
010500060206      * - Ds per dati organigramma
010600060206     d DDatiUte      e ds
010700970526     C*------------------------------------------------------------------------*
010800970526     C* MAIN LINES
010900970526     C*------------------------------------------------------------------------*
011000970526     C*
011100970526     C* NON CI SONO ERRORI IN ENTRATA
011200970526IF  1C     ERRL00        IFEQ      '0'
011300970526     C*
011400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011800970526     C*
011900970526     C* EMETTE FORMATO VIDEO
012000970526     C                   EXSR      BUTFMR
012100970526     C*
012200970526     C* GESTIONE TASTI FUNZIONALI
012300970526     C                   EXSR      FUNCON
012400970526E   2C                   ENDDO
012500970526     C*
012600970526     C* CI SONO ERRORI IN ENTRATA
012700970527X   1C                   ELSE
012800970526     C                   MOVEL     '1'           D00ERR
012900970526     C                   MOVEL     DSMSMS        D00MSG
013000970526E   1C                   ENDIF
013100970626     C*
013200970626     C* OPERAZIONI FINALI
013300970626     C                   EXSR      FINSR
013400970526     C* FINE PGM
013500970526     C                   SETON                                        LR
013600970526     C*------------------------------------------------------------------------*
013700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
013800970526     C*------------------------------------------------------------------------*
013900970526     C     BUTFMR        BEGSR
014000970526     C*
014100970526     C* EMETTE LA TESTATA
014200970527     C                   WRITE     SI12D01
014300970526     C*
014400970526     C* SPEDISCE EVENTUALE MESSAGGIO
014500970526     C                   EXSR      SNDMSG
014600970526     C*
014700970526     C* EMETTE LE FUNZIONI
014800970527     C                   WRITE     SI12D02
014900970526     C*
015000970526     C* EMETTE IL VIDEO
015100970527     C                   EXFMT     SI12D03
015200970526     C*
015300970526     C* REIMPOSTA FLAG DI ERRORE
015400970526     C                   MOVEL     '0'           ERRL00
015500970526     C*
015600970526     C* REIMPOSTA INDICATORI DI ERRORE
015700970526     C                   SETOFF                                         5859
015800970526     C                   SETOFF                                       606162
015900970526     C                   SETOFF                                       636465
016000970526     C                   SETOFF                                       666768
016100970526     C                   SETOFF                                       697071
016200970526     C                   SETOFF                                       727374
016300970526     C                   SETOFF                                       757677
016400970526     C                   SETOFF                                       787980
016500970526     C                   SETOFF                                       818283
016600970526     C                   SETOFF                                       848586
016700970526     C                   SETOFF                                       878889
016800970526     C*
016900970526     C                   ENDSR
017000970526     C*------------------------------------------------------------------------*
017100970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017200970526     C*------------------------------------------------------------------------*
017300970526     C     SNDMSG        BEGSR
017400970526     C*
017500970526     C* PULISCE IL SFILE MESSAGGI
017600970526     C                   MOVEL     '1'           *IN40
017700970527     C                   WRITE     SI12DMC
017800970526     C                   MOVEL     '0'           *IN40
017900970526     C*
018000970526     C* VISUALIZZA IL MESSAGGIO
018100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
018200970526     C                   CALL      'XMSG'        XMSGPA
018300970526IF  1C     DSMSER        IFEQ      *ZEROS
018400970526     C                   MOVEL     DSMSKE        MSGKEY
018500970526E   1C                   ENDIF
018600970526     C                   Z-ADD     1             MSGULT
018700970527     C                   WRITE     SI12DMS
018800970527     C                   WRITE     SI12DMC
018900970526     C*
019000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019100970526     C                   MOVEL     *BLANKS       DSMSMS
019200970526     C*
019300970526     C                   ENDSR
019400970526     C*------------------------------------------------------------------------*
019500970526     C* ENTGES - GESTIONE ENTER -> INVIO
019600970526     C*------------------------------------------------------------------------*
019700970526     C     ENTGES        BEGSR
019800970526     C*
019900970526     C* CONTROLLA I CAMPI IMMESSI
020000970526     C                   EXSR      DECODI
020100970526     C*
020200970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
020300970526IF  1C     DEPOP1        IFEQ      'O05'
020400970526     C                   MOVEL     '1'           FINL00
020500970526E   1C                   ENDIF
020600970526     C*
020700970526     C                   ENDSR
020800970526     C*------------------------------------------------------------------------*
020900970526     C* F03GES - GESTIONE F03 -> FINE
021000970526     C*------------------------------------------------------------------------*
021100970526     C     F03GES        BEGSR
021200970526     C*
021300970526     C* IMPOSTA "PREMUTO F03"
021400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021500970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
021600970526     C*
021700970526     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
022000970526     C*------------------------------------------------------------------------*
022100970526     C     F06GES        BEGSR
022200970526     C*
022300970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
022400970526IF  1C     DEPOP1        IFEQ      'F06'
022500970526     C                   EXSR      INSERI
022600970526E   1C                   ENDIF
022700970526IF  1C     DEPOP1        IFEQ      'O02'
022800970526     C                   EXSR      MODIFI
022900970526E   1C                   ENDIF
023000970526IF  1C     DEPOP1        IFEQ      'O04'
023100970526     C                   EXSR      ANNULL
023200970526E   1C                   ENDIF
023300970526IF  1C     DEPOP1        IFEQ      'O07'
023400970526     C                   EXSR      RIPRIS
023500970526E   1C                   ENDIF
023600970526     C*
023700970526     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900970526     C* F12GES - GESTIONE F12 -> RITORNO
024000970526     C*------------------------------------------------------------------------*
024100970526     C     F12GES        BEGSR
024200970526     C*
024300970526     C* IMPOSTA "PREMUTO F12"
024400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024500970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
024600970526     C*
024700970526     C                   ENDSR
024800970526     C*------------------------------------------------------------------------*
024900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025000970526     C*------------------------------------------------------------------------*
025100970526     C     F24GES        BEGSR
025200970526     C*
025300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
025800970526     C*------------------------------------------------------------------------*
025900970526     C     INSERI        BEGSR
026000970526     C*
026100970526     C* CONTROLLA I CAMPI IMMESSI
026200970526     C                   EXSR      DECODI
026300970526     C*
026400970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026600970527     C                   Z-ADD     D00VER        CPEVER                         *CHIAVE
026700970527     C                   MOVEL     VIDNAR        CPENAR
026800970527     C                   MOVEL     VIDCAP        CPECAP
026900970527     C                   MOVEL     VIDTFP        CPETFP
027000970527     C                   EXSR      IMPCPE                                       *ALTRI CAMPI
027100970527     C                   Z-ADD     *ZEROS        CPEDTR                         *DATA TRASMISSIONE
027200970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
027300970527     C                   MOVEL     *BLANKS       CPEATB                         *ANNULLAMENTO
027400970527     C                   WRITE     AZCPE000
027500970527     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
027600970527     C*
027700970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
027800970527     C                   Z-ADD     D00VER        D81VER
027900970527     C                   MOVEL     VIDNAR        D81NAR
028000970527     C                   MOVEL     VIDCAP        D81CAP
028100970527     C                   MOVEL     'E'           D81CHK
028200970527     C                   CALL      'TISI81R'
028300970527     C                   PARM                    DSSI81
028400970602     C*
028500970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
028600970602     C                   EXSR      AZZVID
028700970526E   1C                   ENDIF
028800970526     C*
028900970526     C                   ENDSR
029000970526     C*------------------------------------------------------------------------*
029100970526     C* MODIFI - OPERAZIONI DI MODIFICA
029200970526     C*------------------------------------------------------------------------*
029300970526     C     MODIFI        BEGSR
029400970526     C*
029500970526     C* CONTROLLA I CAMPI IMMESSI
029600970526     C                   EXSR      DECODI
029700970526     C*
029800970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
029900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030000970527     C                   EXSR      LETCPE
030100970526     C*
030200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030400970527     C                   EXSR      IMPCPE                                       *IMPOSTA I CAMPI
030500970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
030600970527     C                   MOVEL     *BLANKS       CPEATB                         *ANNULLAMENTO
030700970527     C                   UPDATE    AZCPE000
030800970526     C                   MOVEL     MSG(3)        D00MSG
030900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031000000601     C*
031100970526E   2C                   ENDIF
031200970526E   1C                   ENDIF
031300970526     C*
031400970526     C                   ENDSR
031500970526     C*------------------------------------------------------------------------*
031600970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
031700970526     C*------------------------------------------------------------------------*
031800970526     C     ANNULL        BEGSR
031900970526     C*
032000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032100000602     C                   MOVEL     '0'           ANNDLT            1
032200970527     C                   EXSR      LETCPE
032300970527     C*
032400970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
032500970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032600970527IF  2C     CPEATB        IFEQ      *BLANKS                                      *NO ANNULLATI
032700970527IF  3C     CPEDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
032800970527     C                   MOVEL     'A'           CPEATB                         *FLAG ANNULLAMENTO
032900970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
033000970527     C                   UPDATE    AZCPE000                                     *ANNULLA
033100970526X   3C                   ELSE                                                   *MAI TRASMESSO
033200970527     C                   DELETE    AZCPE000                                     *CANCELLA
033300000602     C                   MOVEL     '1'           ANNDLT
033400970526E   3C                   ENDIF
033500970526E   2C                   ENDIF
033600970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
033700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033800970527     C*
033900970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
034000970527     C                   Z-ADD     D00VER        D81VER
034100970527     C                   MOVEL     VIDNAR        D81NAR
034200970527     C                   MOVEL     VIDCAP        D81CAP
034300970527     C                   MOVEL     'E'           D81CHK
034400970527     C                   CALL      'TISI81R'
034500970527     C                   PARM                    DSSI81
034600970526E   1C                   ENDIF
034700970526     C*
034800970526     C                   ENDSR
034900970526     C*------------------------------------------------------------------------*
035000970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
035100970526     C*------------------------------------------------------------------------*
035200970526     C     RIPRIS        BEGSR
035300970526     C*
035400970526     C* CONTROLLA I CAMPI IMMESSI
035500970526     C                   EXSR      DECODI
035600970526     C*
035700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
035800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035900970527     C                   EXSR      LETCPE
036000970526     C*
036100970526     C* IMPOSTA IL RECORD DA SCRIVERE
036200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036300970527     C                   MOVEL     *BLANKS       CPEATB                         *FLAG ANNULLAMENTO
036400970527     C                   MOVEL     *BLANKS       CPEFTR                         *FLAG TRASMISSIONE
036500970527     C                   UPDATE    AZCPE000                                     *ANNULLA
036600970526E   2C                   ENDIF
036700970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
036800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036900970527     C*
037000970527     C* AGGIORNA IL FLAG "ESISTENZA ECCEZIONI" SUL CAP
037100970527     C                   Z-ADD     D00VER        D81VER
037200970527     C                   MOVEL     VIDNAR        D81NAR
037300970527     C                   MOVEL     VIDCAP        D81CAP
037400970527     C                   MOVEL     'E'           D81CHK
037500970530     C                   CALL      'TISI81R'
037600970530     C                   PARM                    DSSI81
037700970526E   1C                   ENDIF
037800970526     C*
037900970526     C                   ENDSR
038000970526     C*------------------------------------------------------------------------*
038100970527     C* LETCPE - LEGGE IL RECORD PERL'AGGIORNAMENTO
038200970526     C*------------------------------------------------------------------------*
038300970527     C     LETCPE        BEGSR
038400970526     C*
038500000601     C     DEPOP1        IFNE      'F06'
038600970527     C                   Z-ADD     D00VER        KCEVER
038700980127     C                   MOVEL     D00NAR        KCENAR
038800980127     C                   MOVEL     D00CAP        KCECAP
038900980127     C                   MOVEL     D00TFP        KCETFP
039000970527     C     KEYCPE        CHAIN     AZCPE01L                           9998
039100970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
039200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
039300970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
039400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
039500970526E   1C                   ENDIF
039600970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
039700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
039800970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
039900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
040000970526E   1C                   ENDIF
040100000601     C                   ENDIF
040200970526     C*
040300970526     C                   ENDSR
040400970526     C*------------------------------------------------------------------------*
040500970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
040600970526     C*------------------------------------------------------------------------*
040700970526     C     DECODI        BEGSR
040800970527     C*--------------------
040900970527     C* NAZIONE
041000970527     C*--------------------
041100970527     C     *LIKE         DEFINE    §15CPR        DEPCPR
041200970527IF  1C     ERRL00        IFEQ      '0'
041300970527     C                   MOVEL     *BLANKS       VDDNAR
041400970527     C                   MOVEL     VIDNAR        WNAR
041500970527     C                   MOVEL     *BLANKS       WDNAR
041600970527     C                   MOVEL     *BLANKS       WCPR
041700970527     C                   EXSR      CHKNAR
041800970527     C                   MOVEL     WNAR          VIDNAR
041900970527     C                   MOVEL     WDNAR         VDDNAR
042000970527     C                   MOVEL     WCPR          DEPCPR
042100970527IF  2C     ERRL00        IFEQ      '1'
042200970527     C                   SETON                                        60
042300970527     C                   MOVEL     MSG(11)       DSMSMS
042400970527E   2C                   ENDIF
042500970527E   1C                   ENDIF
042600970527     C*--------------------
042700970527     C* CAP
042800970527     C*--------------------
042900970527IF  1C     ERRL00        IFEQ      '0'
043000970527     C                   MOVEL     VIDNAR        WNAR
043100970527     C                   MOVEL     VIDCAP        WCAP
043200970527     C                   EXSR      CHKCAP
043300970527IF  2C     ERRL00        IFEQ      '1'
043400970527     C                   SETON                                        61
043500970527     C                   MOVEL     MSG(12)       DSMSMS
043600970527E   2C                   ENDIF
043700970527E   1C                   ENDIF
043800970626     C* CARICA DATI CAP
043900970527IF  1C     ERRL00        IFEQ      '0'
044000970626     C                   Z-ADD     O95CLA        VIDCLN
044100970626     C                   Z-ADD     O95CZC        VIDCZN
044200970626     C                   Z-ADD     O95CLO        VIDCLO
044300970626     C                   Z-ADD     O95CZO        VIDCZO
044400970626     C                   Z-ADD     O95CLK        VIDCKG
044500970626     C                   Z-ADD     O95CLM        VIDCMC
044600970626     C                   Z-ADD     O95CLS        VIDCLS
044700970626     C                   Z-ADD     O95CZS        VIDCZS
044800970626     C                   Z-ADD     O95CKS        VIDCKS
044900970626     C                   Z-ADD     O95CMS        VIDCMS
045000971118     C                   Z-ADD     O95CL2        VIDCL2
045100971118     C                   Z-ADD     O95CZ2        VIDCZ2
045200971118     C                   Z-ADD     O95CK2        VIDCK2
045300971118     C                   Z-ADD     O95CM2        VIDCM2
045400970527E   1C                   ENDIF
045500970527     C*--------------------
045600970527     C* TERMINAL PARTENZA
045700970527     C*--------------------
045800970527IF  1C     ERRL00        IFEQ      '0'
045900970527     C                   MOVEL     VIDTFP        WTFP
046000970527     C                   MOVEL     *BLANKS       WDTFP
046100970528     C                   EXSR      CHKTFX
046200970527     C                   MOVEL     WTFP          VIDTFP
046300970527     C                   MOVEL     WDTFP         VDDTFP
046400970527IF  2C     ERRL00        IFEQ      '1'
046500970527     C                   SETON                                        62
046600970527     C                   MOVEL     MSG(14)       DSMSMS
046700970527E   2C                   ENDIF
046800970527E   1C                   ENDIF
046900970527     C*--------------------
047000970527     C* NAZIONE + CAP + TERMINAL PARTENZA
047100970527     C*--------------------
047200970527     C* INSERIMENTO -> RECORD INESISTENTE
047300970527IF  1C     ERRL00        IFEQ      '0'
047400970527IF  2C     DEPOP1        IFEQ      'F06'
047500970527     C                   Z-ADD     D00VER        KCEVER
047600970527     C                   MOVEL     VIDNAR        KCENAR
047700970527     C                   MOVEL     VIDCAP        KCECAP
047800970527     C                   MOVEL     VIDTFP        KCETFP
047900970527     C     KEYCPE        CHAIN(N)  AZCPE01L                           99
048000970527IF  3C     *IN99         IFEQ      *OFF
048100970527     C                   SETON                                        606162
048200970527     C                   MOVEL     '1'           ERRL00
048300970527     C                   MOVEL     MSG(8)        DSMSMS
048400970527E   3C                   ENDIF
048500970527E   2C                   ENDIF
048600970527E   1C                   ENDIF
048700970527     C*--------------------
048800970606     C* LINEA ARRIVO STANDARD
048900970527     C*--------------------
049000970527IF  1C     ERRL00        IFEQ      '0'
049100970527     C                   MOVEL     VIDLNA        WLIN
049200970527     C                   MOVEL     *BLANKS       WDLIN
049300970527     C                   EXSR      CHKLIN
049400970527     C                   MOVEL     WLIN          VIDLNA
049500970527     C                   MOVEL     WDLIN         VDDLNA
049600970527IF  2C     ERRL00        IFEQ      '1'
049700970527     C                   SETON                                        63
049800970527     C                   MOVEL     MSG(15)       DSMSMS
049900970527E   2C                   ENDIF
050000970527E   1C                   ENDIF
050100970527     C*--------------------
050200970606     C* ZONA CONSEGNA STANDARD
050300970527     C*--------------------
050400970527IF  1C     ERRL00        IFEQ      '0'
050500970527     C                   MOVEL     VIDLNA        WLIN
050600970527     C                   Z-ADD     VIDZNC        WZNC
050700970527     C                   EXSR      CHKZNC
050800970527IF  2C     ERRL00        IFEQ      '1'
050900970527     C                   SETON                                        64
051000970527     C                   MOVEL     MSG(16)       DSMSMS
051100970527E   2C                   ENDIF
051200060206if  2c                   if        §03nuc = 'S'
051300060206     c                   movel     Msg(17)       DSmsms
051400060206     c                   eval      *in64  = *on
051500060206     c                   eval      ErrL00 = *on
051600060206e   2c                   endif
051700970527E   1C                   ENDIF
051800970606     C*--------------------
051900970912     C* LINEA ARRIVO SOTTO
052000970606     C*--------------------
052100970606IF  1C     ERRL00        IFEQ      '0'
052200970606     C                   MOVEL     *BLANKS       VDDLOS
052300970606IF  2C     VIDLOS        IFNE      *ZEROS
052400970606     C     VIDLOS        ANDNE     *BLANKS
052500970606     C                   MOVEL     VIDLOS        WLIN
052600970606     C                   MOVEL     *BLANKS       WDLIN
052700970606     C                   EXSR      CHKLIN
052800970606     C                   MOVEL     WLIN          VIDLOS
052900970606     C                   MOVEL     WDLIN         VDDLOS
053000970606IF  3C     ERRL00        IFEQ      '1'
053100970606     C                   SETON                                        67
053200970606     C                   MOVEL     MSG(15)       DSMSMS
053300970606E   3C                   ENDIF
053400970606E   2C                   ENDIF
053500970606E   1C                   ENDIF
053600970606     C*--------------------
053700970912     C* ZONA DI CONSEGNA SOTTO
053800970606     C*--------------------
053900970606IF  1C     ERRL00        IFEQ      '0'
054000971126IF  2C     VIDLOS        IFGT      *ZEROS
054100970606     C                   MOVEL     VIDLOS        WLIN
054200970606     C                   Z-ADD     VIDZOS        WZNC
054300970606     C                   EXSR      CHKZNC
054400970606IF  3C     ERRL00        IFEQ      '1'
054500970606     C                   SETON                                        68
054600970606     C                   MOVEL     MSG(16)       DSMSMS
054700970606E   3C                   ENDIF
054800060206if  3c                   if        §03nuc = 'S'
054900060206     c                   movel     Msg(17)       DSmsms
055000060206     c                   eval      *in68  = *on
055100060206     c                   eval      ErrL00 = *on
055200060206e   3c                   endif
055300970606E   2C                   ENDIF
055400970606E   1C                   ENDIF
055500970527     C*--------------------
055600970912     C* LINEA ARRIVO OLTRE
055700970527     C*--------------------
055800970527IF  1C     ERRL00        IFEQ      '0'
055900971126IF  2C     VIDLOL        IFNE      *ZEROS
056000971126     C     VIDLOL        ANDNE     *BLANKS
056100970527     C                   MOVEL     VIDLOL        WLIN
056200970527     C                   MOVEL     *BLANKS       WDLIN
056300970527     C                   EXSR      CHKLIN
056400970527     C                   MOVEL     WLIN          VIDLOL
056500970527     C                   MOVEL     WDLIN         VDDLOL
056600971126IF  3C     ERRL00        IFEQ      '1'
056700970527     C                   SETON                                        65
056800970527     C                   MOVEL     MSG(15)       DSMSMS
056900971126E   3C                   ENDIF
057000971126E   2C                   ENDIF
057100970527E   1C                   ENDIF
057200970527     C*--------------------
057300970912     C* ZONA CONSEGNA OLTRE
057400970527     C*--------------------
057500970527IF  1C     ERRL00        IFEQ      '0'
057600971126IF  2C     VIDLOL        IFGT      *ZEROS
057700970527     C                   MOVEL     VIDLOL        WLIN
057800970527     C                   Z-ADD     VIDZOL        WZNC
057900970527     C                   EXSR      CHKZNC
058000971126IF  3C     ERRL00        IFEQ      '1'
058100970527     C                   SETON                                        66
058200970527     C                   MOVEL     MSG(16)       DSMSMS
058300971126E   3C                   ENDIF
058400060206if  3c                   if        §03nuc = 'S'
058500060206     c                   movel     Msg(17)       DSmsms
058600060206     c                   eval      *in66  = *on
058700060206     c                   eval      ErrL00 = *on
058800060206e   3c                   endif
058900971126E   2C                   ENDIF
059000970527E   1C                   ENDIF
059100971118     C*--------------------
059200971118     C* LINEA ARRIVO OLTRE 2
059300971118     C*--------------------
059400971118IF  1C     ERRL00        IFEQ      '0'
059500971126IF  2C     VIDLO2        IFNE      *ZEROS
059600971126     C     VIDLO2        ANDNE     *BLANKS
059700971118     C                   MOVEL     VIDLO2        WLIN
059800971118     C                   MOVEL     *BLANKS       WDLIN
059900971118     C                   EXSR      CHKLIN
060000971118     C                   MOVEL     WLIN          VIDLO2
060100971118     C                   MOVEL     WDLIN         VDDLO2
060200971126IF  3C     ERRL00        IFEQ      '1'
060300971118     C                   SETON                                        69
060400971118     C                   MOVEL     MSG(15)       DSMSMS
060500971126E   3C                   ENDIF
060600971126E   2C                   ENDIF
060700971118E   1C                   ENDIF
060800971118     C*--------------------
060900971118     C* ZONA CONSEGNA OLTRE
061000971118     C*--------------------
061100971118IF  1C     ERRL00        IFEQ      '0'
061200971126IF  2C     VIDLO2        IFGT      *ZEROS
061300971118     C                   MOVEL     VIDLO2        WLIN
061400971118     C                   Z-ADD     VIDZO2        WZNC
061500971118     C                   EXSR      CHKZNC
061600971126IF  3C     ERRL00        IFEQ      '1'
061700971118     C                   SETON                                        70
061800971118     C                   MOVEL     MSG(16)       DSMSMS
061900971126E   3C                   ENDIF
062000060206if  2c                   if        §03nuc = 'S'
062100060206     c                   movel     Msg(17)       DSmsms
062200060206     c                   eval      *in70  = *on
062300060206     c                   eval      ErrL00 = *on
062400060206e   2c                   endif
062500971126E   2C                   ENDIF
062600971118E   1C                   ENDIF
062700970527     C*--------------------
062800970527     C* DECODIFICHE DATI CAPPARIO
062900970527     C*--------------------
063000970527     C* LINEA ARRIVO
063100970527IF  1C     ERRL00        IFEQ      '0'
063200970606IF  2C     VIDCLN        IFGT      *ZEROS
063300970527     C                   MOVEL     VIDCLN        WLIN
063400970527     C                   MOVEL     *BLANKS       WDLIN
063500970527     C                   EXSR      CHKLIN
063600970527     C                   MOVEL     WDLIN         VDDCLN
063700970606E   2C                   ENDIF
063800970606E   1C                   ENDIF
063900970912     C* LINEA ARRIVO SOTTO
064000970606IF  1C     ERRL00        IFEQ      '0'
064100970606IF  2C     VIDCLS        IFGT      *ZEROS
064200970606     C                   MOVEL     VIDCLS        WLIN
064300970606     C                   MOVEL     *BLANKS       WDLIN
064400970606     C                   EXSR      CHKLIN
064500970606     C                   MOVEL     WDLIN         VDDCLS
064600970606E   2C                   ENDIF
064700970606E   1C                   ENDIF
064800970912     C* LINEA ARRIVO OLTRE
064900970606IF  1C     ERRL00        IFEQ      '0'
065000970606IF  2C     VIDCLO        IFGT      *ZEROS
065100970527     C                   MOVEL     VIDCLO        WLIN
065200970527     C                   MOVEL     *BLANKS       WDLIN
065300970527     C                   EXSR      CHKLIN
065400970527     C                   MOVEL     WDLIN         VDDCLO
065500970606E   2C                   ENDIF
065600970606E   1C                   ENDIF
065700971118     C* LINEA ARRIVO OLTRE 2
065800971118IF  1C     ERRL00        IFEQ      '0'
065900971118IF  2C     VIDCL2        IFGT      *ZEROS
066000971118     C                   MOVEL     VIDCL2        WLIN
066100971118     C                   MOVEL     *BLANKS       WDLIN
066200971118     C                   EXSR      CHKLIN
066300971118     C                   MOVEL     WDLIN         VDDCL2
066400971118E   2C                   ENDIF
066500971118E   1C                   ENDIF
066600970527     C*
066700970527     C                   ENDSR
066800970527     C*------------------------------------------------------------------------*
066900970527     C* CHKNAR - CONTROLLA LA NAZIONE
067000970527     C*------------------------------------------------------------------------*
067100970527     C     CHKNAR        BEGSR
067200970527     C*
067300970527     C     *LIKE         DEFINE    VIDNAR        WNAR
067400970527     C     *LIKE         DEFINE    VDDNAR        WDNAR
067500970527     C     *LIKE         DEFINE    §15CPR        WCPR
067600970527     C*
067700970527     C* RICHIESTO ELENCO
067800970527     C                   SETOFF                                           28
067900970527     C     '?'           SCAN      WNAR                                   28
068000970527IF  1C     *IN28         IFEQ      *ON
068100020906     C                   Z-ADD     1             §KUT              1 0
068200970527     C                   MOVEL     '15'          §COD              2
068300970527     C                   MOVEL     *BLANKS       §KEY              8
068400970527     C                   MOVEL     *BLANKS       §DES             30
068500970527     C                   CALL      'X§TABER'
068600970527     C                   PARM                    §KUT
068700970527     C                   PARM                    §COD
068800970527     C                   PARM                    §KEY
068900970527     C                   PARM                    §DES
069000970527     C                   MOVEL     §KEY          WNAR
069100970527     C                   MOVEL     §DES          WDNAR
069200970527E   1C                   ENDIF
069300970527     C*
069400970527     C* CONTROLLA ESISTENZA CODICE
069500020906     C                   MOVEL     1             KTBKUT
069600970527     C                   MOVEL     '15'          KTBCOD
069700970527     C                   MOVEL(P)  WNAR          KTBKEY
069800970527     C     KEYTAB        CHAIN     TABEL00F                           99
069900970527IF  1C     *IN99         IFEQ      *OFF
070000970527     C     TBLFLG        ANDEQ     *BLANKS
070100970527     C                   MOVEL     TBLUNI        DS15
070200970527     C                   MOVEL     §15DES        WDNAR
070300970527     C                   MOVEL     §15CPR        WCPR
070400970527     C*
070500970527     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
070600970527IF  2C     VIDNAR        IFEQ      'I  '
070700970527     C                   MOVEL     '1'           ERRL00
070800970527     C                   MOVEL     *BLANKS       WDNAR
070900970527E   2C                   ENDIF
071000970527X   1C                   ELSE
071100970527     C                   MOVEL     '1'           ERRL00
071200970527E   1C                   ENDIF
071300970527     C*
071400970527     C                   ENDSR
071500970527     C*------------------------------------------------------------------------*
071600970527     C* CHKCAP - CONTROLLA IL CAP
071700970527     C*------------------------------------------------------------------------*
071800970527     C     CHKCAP        BEGSR
071900970527     C*
072000970527     C     *LIKE         DEFINE    VIDCAP        WCAP
072100970626     C*
072200970626     C* CONTROLLA IL CAP
072300970626     C                   CLEAR                   DSSI95
072400970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
072500970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
072600970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
072700970626     C                   MOVEL     WCAP          I95CAP                         *CAP
072800970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
072900970626     C                   CALL      'TISI95R'
073000970626     C                   PARM                    DSSI95
073100970626IF  1C     O95ERR        IFNE      *BLANKS
073200970626     C                   MOVEL     '1'           ERRL00
073300970626E   1C                   ENDIF
073400970527     C*
073500970527     C                   ENDSR
073600970527     C*------------------------------------------------------------------------*
073700970528     C* CHKTFX - CONTROLLA IL TERMINAL
073800970527     C*------------------------------------------------------------------------*
073900970528     C     CHKTFX        BEGSR
074000970527     C*
074100970527     C     *LIKE         DEFINE    VIDTFP        WTFP
074200970527     C     *LIKE         DEFINE    VDDTFP        WDTFP
074300970527     C*
074400970527     C* RICHIESTO ELENCO
074500970527     C                   SETOFF                                           28
074600970527     C     '?'           SCAN      WTFP                                   28
074700970527IF  1C     *IN28         IFEQ      *ON
074800970527     C                   MOVEL     *BLANKS       §COD1             3
074900970527     C                   MOVEL     *BLANKS       §TIP              1
075000970527     C                   MOVEL     *BLANKS       §DES1            25
075100971203     C                   MOVEL     *BLANKS       §PRA              3
075200971203     C                   CALL      'TNSD24R'
075300970527     C                   PARM                    §COD1
075400970527     C                   PARM                    §TIP
075500970527     C                   PARM                    §DES1
075600971203     C                   PARM                    §PRA
075700970527     C                   MOVEL     §COD1         WTFP
075800970527     C                   MOVEL     §DES1         WDTFP
075900970527E   1C                   ENDIF
076000970527     C*
076100970527     C* SOLO CARATTERI NUMERICI
076200970527     C                   SETOFF                                       28
076300970527     C                   TESTN                   WTFP                 28
076400970527IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
076500970527     C                   MOVEL     '1'           ERRL00
076600970527X   1C                   ELSE                                                   *E'NUMERO DA TESTN
076700970527     C                   MOVE      WTFP          W001              1
076800970527IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
076900970527     C                   MOVEL     '1'           ERRL00
077000970527X   2C                   ELSE                                                   *NUMERO
077100971128     C*
077200971128     C* REPERISCE IL TERMINAL
077300971128     C                   CLEAR                   DSLV55
077400971128     C                   MOVEL     'P'           D55TPT
077500971128     C                   MOVEL     WTFP          D55LIN
077600971128     C                   Z-ADD     DATCOR        D55DRF
077700971128     C                   CALL      'FNLV55R'
077800971128     C                   PARM                    DSLV55
077900971128IF  3C     D55ERR        IFNE      *BLANKS
078000971128     C                   MOVEL     WTFP          D55TFP
078100971128E   3C                   ENDIF
078200970527     C*
078300970527     C* CONTROLLA ESISTENZA CODICE
078400000613     C                   CLEAR                   OG143
078500970527     C                   MOVEL     WTFP          KORFIL
078600970527     C     KEYORG        CHAIN     AZORG01L                           99
078700000613IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
078800000613     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
078900000613     C     ORGFAG        ANDNE     'V'                                          *NON VARIA
079000000613     C                   MOVEL     ORGDE3        OG143
079100000613IF  4C     ORGFIL        IFEQ      D55TFP                                       *TERMINAL PARTENZA
079200020627     C     §OGNTW        OREQ      'PPT'                                        *FILIALE POSTE
079300000613     C                   MOVEL     ORGDES        WDTFP                          *DESCRIZIONE
079400000613X   4C                   ELSE
079500000613     C                   MOVEL     '1'           ERRL00                         *ERRORE
079600000613E   3C                   ENDIF
079700970527X   3C                   ELSE
079800000613     C                   MOVEL     '1'           ERRL00                         *ERRORE
079900970527E   3C                   ENDIF
080000970527E   2C                   ENDIF
080100970527E   1C                   ENDIF
080200970527     C*
080300970527     C                   ENDSR
080400970527     C*------------------------------------------------------------------------*
080500970527     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
080600970527     C*------------------------------------------------------------------------*
080700970527     C     CHKLIN        BEGSR
080800970527     C*
080900970527     C     *LIKE         DEFINE    VIDLNA        WLIN
081000970527     C     *LIKE         DEFINE    VDDLNA        WDLIN
081100970527     C*
081200970527     C* RICHIESTO ELENCO
081300970527     C                   SETOFF                                           28
081400970527     C     '?'           SCAN      WLIN                                   28
081500970527IF  1C     *IN28         IFEQ      *ON
081600970527     C                   MOVEL     *BLANKS       §COD1             3
081700970527     C                   MOVEL     *BLANKS       §TIP              1
081800970527     C                   MOVEL     *BLANKS       §DES1            25
081900971203     C                   MOVEL     *BLANKS       §PRA              3
082000980409     C                   CALL      'TNSD24R'
082100970527     C                   PARM                    §COD1
082200970527     C                   PARM                    §TIP
082300970527     C                   PARM                    §DES1
082400971203     C                   PARM                    §PRA
082500970527     C                   MOVEL     §COD1         WLIN
082600970527     C                   MOVEL     §DES1         WDLIN
082700970527E   1C                   ENDIF
082800970527     C*
082900970527     C* SOLO CARATTERI NUMERICI
083000970527     C                   SETOFF                                       28
083100970527     C                   TESTN                   WLIN                 28
083200970527IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
083300970527     C                   MOVEL     '1'           ERRL00
083400970527X   1C                   ELSE                                                   *E'NUMERO DA TESTN
083500970527     C                   MOVE      WLIN          W001              1
083600970527IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
083700970527     C                   MOVEL     '1'           ERRL00
083800970527X   2C                   ELSE                                                   *NUMERO
083900970527     C*
084000970527     C* CONTROLLA ESISTENZA CODICE
084100970527     C                   MOVEL     WLIN          KORFIL
084200970527     C     KEYORG        CHAIN     AZORG01L                           99
084300970527IF  3C     *IN99         IFEQ      *ON                                          *NON TROVATA
084400970527     C     ORGFVA        ORNE      *BLANKS                                      *ANNULLATA
084500970527     C     ORGFAG        OREQ      'V'                                          *VARIA
084600970527     C                   MOVEL     '1'           ERRL00
084700970527X   3C                   ELSE
084800970527     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
084900970527E   3C                   ENDIF
085000970527E   2C                   ENDIF
085100970527E   1C                   ENDIF
085200970527     C*
085300970527     C                   ENDSR
085400970527     C*------------------------------------------------------------------------*
085500970527     C* CHKZNC - CONTROLLA LA ZONA DI CONSEGNA
085600970527     C*------------------------------------------------------------------------*
085700970527     C     CHKZNC        BEGSR
085800970527     C*
085900970527     C     *LIKE         DEFINE    VIDZNC        WZNC
086000970527     C*
086100970527     C* CONTROLLA ESISTENZA CODICE
086200020906     C                   Z-ADD     1             KTBKUT
086300970527     C                   MOVEL     '03'          KTBCOD
086400970527     C                   MOVEL     WLIN          WA5               5
086500970527     C                   MOVE      WZNC          WA5
086600970527     C                   MOVEL(P)  WA5           KTBKEY
086700970527     C     KEYTAB        CHAIN     TABEL00F                           99
086800970527IF  1C     *IN99         IFEQ      *OFF
086900970527     C     TBLFLG        ANDEQ     *BLANKS
087000970527     C                   MOVEL     TBLUNI        DS03
087100970527X   1C                   ELSE
087200970527     C                   MOVEL     '1'           ERRL00
087300970527E   1C                   ENDIF
087400970527     C*
087500970527     C                   ENDSR
087600970526     C*------------------------------------------------------------------------*
087700970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
087800970526     C*------------------------------------------------------------------------*
087900970526     C     IREVID        BEGSR
088000970526     C* CAMPI
088100970527     C                   MOVEL     CPENAR        VIDNAR
088200970527     C                   MOVEL     CPECAP        VIDCAP
088300970527     C                   MOVEL     CPETFP        VIDTFP
088400970527     C                   MOVEL     CPELNA        VIDLNA
088500970527     C                   Z-ADD     CPEZNC        VIDZNC
088600970527     C                   MOVEL     CPELOL        VIDLOL
088700970527     C                   Z-ADD     CPEZOL        VIDZOL
088800970606     C                   MOVEL     CPELOS        VIDLOS
088900970606     C                   Z-ADD     CPEZOS        VIDZOS
089000971118     C                   MOVEL     CPELO2        VIDLO2
089100971118     C                   Z-ADD     CPEZO2        VIDZO2
089200970527     C*
089300970526     C                   ENDSR
089400970526     C*------------------------------------------------------------------------*
089500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
089600970526     C*------------------------------------------------------------------------*
089700970526     C     AZZVID        BEGSR
089800970526     C* CAMPI
089900970603     C                   MOVEL     D00NAR        VIDNAR
090000970603     C                   MOVEL     D00CAP        VIDCAP
090100970527     C                   MOVEL     *ZEROS        VIDTFP
090200970527     C                   MOVEL     *ZEROS        VIDLNA
090300970527     C                   Z-ADD     *ZEROS        VIDZNC
090400970527     C                   MOVEL     *ZEROS        VIDLOL
090500970527     C                   Z-ADD     *ZEROS        VIDZOL
090600970606     C                   MOVEL     *ZEROS        VIDLOS
090700970606     C                   Z-ADD     *ZEROS        VIDZOS
090800971118     C                   MOVEL     *ZEROS        VIDLO2
090900971118     C                   Z-ADD     *ZEROS        VIDZO2
091000970526     C* DECODIFICHE
091100970527     C                   MOVEL     *BLANKS       VDDNAR
091200970527     C                   MOVEL     *BLANKS       VDDTFP
091300970527     C                   MOVEL     *BLANKS       VDDLNA
091400970527     C                   MOVEL     *BLANKS       VDDLOL
091500970606     C                   MOVEL     *BLANKS       VDDLOS
091600971118     C                   MOVEL     *BLANKS       VDDLO2
091700970527     C* CAMPI CAPPARIO
091800970527     C                   Z-ADD     *ZEROS        VIDCLN
091900970527     C                   Z-ADD     *ZEROS        VIDCZN
092000970606     C                   MOVEL     *BLANKS       VDDCLN
092100971118     C                   Z-ADD     *ZEROS        VIDCLO
092200970527     C                   Z-ADD     *ZEROS        VIDCZO
092300971118     C                   MOVEL     *BLANKS       VDDCLO
092400970606     C                   Z-ADD     *ZEROS        VIDCLS
092500970606     C                   Z-ADD     *ZEROS        VIDCZS
092600971118     C                   MOVEL     *BLANKS       VDDCLS
092700971118     C                   Z-ADD     *ZEROS        VIDCL2
092800971118     C                   Z-ADD     *ZEROS        VIDCZ2
092900971118     C                   MOVEL     *BLANKS       VDDCL2
093000970526     C*
093100970526     C                   ENDSR
093200970526     C*------------------------------------------------------------------------*
093300970527     C* IMPCPE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
093400970526     C*------------------------------------------------------------------------*
093500970527     C     IMPCPE        BEGSR
093600970526     C*
093700970527     C                   MOVEL     VIDLNA        CPELNA
093800970527     C                   Z-ADD     VIDZNC        CPEZNC
093900970527     C                   MOVEL     VIDLOL        CPELOL
094000970527     C                   Z-ADD     VIDZOL        CPEZOL
094100970606     C                   MOVEL     VIDLOS        CPELOS
094200970606     C                   Z-ADD     VIDZOS        CPEZOS
094300971118     C                   MOVEL     VIDLO2        CPELO2
094400971118     C                   Z-ADD     VIDZO2        CPEZO2
094500970526     C*
094600970526     C                   ENDSR
094700970526     C*------------------------------------------------------------------------*
094800970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
094900970526     C*------------------------------------------------------------------------*
095000970526     C     IMPFUN        BEGSR
095100970526     C*
095200970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
095300970526IF  1C     DEPOP1        IFEQ      'O05'
095400970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
095500970526     C                   MOVEL     'N'           FUDATT
095600970526     C                   MOVEL     FUDRIG        FUD(6)
095700970526E   1C                   ENDIF
095800970526     C*
095900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
096000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
096100970526     C                   EXSR      FUNGES
096200970526     C*
096300970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
096400970526     C*---
096500970526     C* 90 -> PROTEZIONE CHIAVE
096600970526     C* 91 -> PROTEZIONE TOTALE
096700970526     C*---
096800970527IF  1C     DEPOP0        IFEQ      'E02'                                        G E S T I O N E
096900970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
097000970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
097100970526     C                   SETOFF                                       91
097200970526E   2C                   ENDIF
097300970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
097400970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
097500970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
097600970526E   2C                   ENDIF
097700970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
097800970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
097900970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
098000970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
098100970526     C                   SETON                                        91
098200970526E   2C                   ENDIF
098300970526X   1C                   ELSE                                                   *I N T E R R O G A
098400970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
098500970526     C                   SETON                                        91
098600970526E   1C                   ENDIF
098700970526     C*
098800970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
098900970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
099000970527     C                   Z-ADD     D00VER        KCEVER
099100970527     C                   MOVEL     D00NAR        KCENAR
099200970527     C                   MOVEL     D00CAP        KCECAP
099300970527     C                   Z-ADD     D00TFP        KCETFP
099400970527     C     KEYCPE        CHAIN(N)  AZCPE01L                           99
099500970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
099600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
099700970526     C                   MOVEL     MSG(6)        DSMSMS
099800970526X   2C                   ELSE
099900970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
100000980128IF  3C     DEPOP1        IFEQ      'O05'
100100980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
100200980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
100300980128E   3C                   ENDIF
100400970526E   2C                   ENDIF
100500970526X   1C                   ELSE                                                   *INSERIMENTO
100600970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
100700970526E   1C                   ENDIF
100800970526     C*
100900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
101000970526IF  1C     DEPOP1        IFEQ      'O02'
101100970526     C                   MOVEL     DEC(1)        VIDFUN
101200970526E   1C                   ENDIF
101300970526IF  1C     DEPOP1        IFEQ      'O04'
101400970526     C                   MOVEL     DEC(2)        VIDFUN
101500970526E   1C                   ENDIF
101600970526IF  1C     DEPOP1        IFEQ      'O05'
101700970526     C                   MOVEL     DEC(3)        VIDFUN
101800970526E   1C                   ENDIF
101900970526IF  1C     DEPOP1        IFEQ      'O07'
102000970526     C                   MOVEL     DEC(4)        VIDFUN
102100970526E   1C                   ENDIF
102200970526IF  1C     DEPOP1        IFEQ      'F06'
102300970526     C                   MOVEL     DEC(5)        VIDFUN
102400970526E   1C                   ENDIF
102500970526     C*
102600970526     C                   ENDSR
102700970626     C*------------------------------------------------------------------------*
102800970626     C* FINSR - OPERAZIONI FINALI
102900970626     C*------------------------------------------------------------------------*
103000970626     C     FINSR         BEGSR
103100971128     C*
103200970704     C                   CLEAR                   DSSI95
103300970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
103400970626     C                   CALL      'TISI95R'
103500970626     C                   PARM                    DSSI95
103600971128     C*
103700971128     C                   CLEAR                   DSLV55
103800971128     C                   MOVEL     'C'           D55TLA
103900971128     C                   CALL      'FNLV55R'
104000971128     C                   PARM                    DSLV55
104100970626     C*
104200970626     C                   ENDSR
104300970526     C*------------------------------------------------------------------------*
104400970526     C* *INZSR - OPERAZIONI INIZIALI
104500970526     C*------------------------------------------------------------------------*
104600970526     C     *INZSR        BEGSR
104700970526     C*---
104800970526     C* RICEVIMENTO PARAMETRI
104900970526     C*---
105000970526     C     *ENTRY        PLIST
105100970612     C                   PARM                    KPJBA
105200970612     C                   PARM                    DSSI00
105300970526     C*
105400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
105500970526     C     XMSGPA        PLIST
105600970526     C                   PARM                    DSMSMS
105700970526     C                   PARM                    DSMSPG
105800970526     C                   PARM                    DSMSKE
105900970526     C                   PARM                    DSMSER
106000970526     C*
106100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
106200970526     C                   CALL      'XKEY'
106300970526     C                   PARM                    DSKEY
106400970526     C*---
106500970526     C* VARIABILI NON RIFERITE AL DATA BASE
106600970526     C*---
106700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
106800970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
106900970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
107000970526     C     *LIKE         DEFINE    D00OP0        WOP0
107100970526     C     *LIKE         DEFINE    D00OP1        WOP1
107200970526     C*---
107300970526     C* VARIABILI DI CONTROLLO
107400970526     C*---
107500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
107600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
107700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
107800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
107900970526     C*
108000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
108100970527     C                   MOVEL     'TISI12R'     DSMSPG
108200970527     C                   MOVEL     'TISI12R'     MSGQUE
108300970526     C                   MOVEL     *BLANKS       MSGKEY
108400970526     C                   MOVEL     *BLANKS       DSMSMS
108500970526     C*---
108600970526     C* VARIABILI RIFERITE AL DATA BASE
108700970526     C*---
108800970527     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
108900970527     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
109000970526     C     *LIKE         DEFINE    TBLCOD        KTBCOD
109100970526     C     *LIKE         DEFINE    TBLKEY        KTBKEY
109200970527     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
109300970527     C     *LIKE         DEFINE    CPENAR        KCENAR
109400970527     C     *LIKE         DEFINE    CPECAP        KCECAP
109500970527     C     *LIKE         DEFINE    CPETFP        KCETFP
109600970526     C*---
109700970526     C* CHIAVI DI LETTURA
109800970526     C*---
109900970527     C* LETTURA AZORG01L - COMPLETA
110000970527     C     KEYORG        KLIST
110100970528     C                   KFLD                    KORFIL                         *FILIALE
110200970526     C* LETTURA TABEL00F - COMPLETA
110300970526     C     KEYTAB        KLIST
110400970526     C                   KFLD                    KTBKUT                         *CODICE UTENTE
110500970526     C                   KFLD                    KTBCOD                         *TABELLA
110600970526     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
110700970527     C* LETTURA AZCPE01L - COMPLETA
110800970527     C     KEYCPE        KLIST
110900970527     C                   KFLD                    KCEVER                         *VERSIONE
111000970527     C                   KFLD                    KCENAR                         *NAZIONE
111100970527     C                   KFLD                    KCECAP                         *CAP
111200970527     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
111300970526     C*
111400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
111500970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
111600970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
111700971128     C*
111800971128     C* CALCOLA LA DATA CORRENTE
111900971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
112000971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
112100971128     C                   Z-ADD     WN8           G08DAT
112200971128     C                   Z-ADD     *ZEROS        G08INV
112300971128     C                   MOVEL     '0'           G08ERR
112400971128     C                   CALL      'XSRDA8'
112500971128     C                   PARM                    WLBDA8
112600971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
112700060206      *
112800060206      * REPERISCE I DATI DELL'UTENTE
112900060206     c                   exsr      RepDatiUte
113000060206     c                   movel     RSUT          DSfirs
113100970526     C*
113200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
113300970526     C                   EXSR      IMPFUN
113400970526     C*
113500970526     C                   ENDSR
113600060206      *---------------------------------------------------------------*
113700060206      * REPERISCE DATI UTENTE                                         *
113800060206      *---------------------------------------------------------------*
113900060206     c     RepDatiUte    BEGSR
114000060206      *
114100060206     c     *dtaara       define    §azute        AZUTEds
114200060206     c     *dtaara       define    §datiute      DDatiUte
114300060206      *
114400060206     c                   in(E)     *dtaara
114500060206     c                   IF        %ERROR or RSUT = *blanks
114600060206     c                   clear                   Tibs34Ds
114700060206     c                   call      'TIBS34R'
114800060206     c                   parm                    Tibs34Ds
114900060206     c                   in        *dtaara
115000060206     c                   ENDIF
115100060206      *
115200060206     c                   ENDSR
115300970526     C*------------------------------------------------------------------------*
115400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
115500970526     C*------------------------------------------------------------------------*
115600970526     C     FUNGES        BEGSR
115700970526     C*
115800970526     C* PULISCO CAMPI DI LAVORO
115900970526     C                   Z-ADD     0             W                 5 0
116000970526     C*
116100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
116200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
116300970526     C     1             DO        24            I
116400970526     C                   MOVEL     FUD(I)        FUDRIG
116500970526     C*
116600970526     C* PULIZIA FUNZIONI PRECEDENTI
116700970526     C                   MOVEL     *BLANKS       FUV(I)
116800970526     C*
116900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
117000970526     C* (SALTA F24)
117100970526     C     FUDATT        IFEQ      'S'
117200970526     C     FUDFUN        ANDNE     '24'
117300970526     C                   ADD       1             W
117400970526     C                   MOVEL     FUDDES        FUV(W)
117500970526     C                   END
117600970526     C                   END                                                    DO WHILE
117700970526     C*
117800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
117900970526     C* ALTRIMENTI DISATTIVO F24
118000970526     C     1             DO        24            I
118100970526     C                   MOVEL     FUD(I)        FUDRIG
118200970526     C*
118300970526     C     FUDFUN        IFEQ      '24'
118400970526     C     W             IFGT      6
118500970526     C                   MOVEL     'S'           FUDATT
118600970526     C                   ELSE
118700970526     C                   MOVEL     *BLANKS       FUDATT
118800970526     C                   END                                                    W GT 8
118900970526     C                   MOVEL     FUDRIG        FUD(I)
119000970526     C                   END
119100970526     C                   END                                                    DO WHILE
119200970526     C*
119300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
119400970526     C     W             DIV       6             I
119500970526     C                   MVR                     Z
119600970526     C*
119700970526     C     Z             IFNE      0
119800970526     C     I             ADD       1             Z
119900970526     C                   ELSE
120000970526     C                   Z-ADD     I             Z                 5 0
120100970526     C                   END
120200970526     C*
120300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
120400970526     C                   ADD       1             FUNGRU
120500970526     C     FUNGRU        IFGT      Z
120600970526     C     FUNGRU        ORLT      1
120700970526     C                   Z-ADD     1             FUNGRU
120800970526     C                   END
120900970526     C*
121000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
121100970526     C     FUNGRU        IFEQ      1
121200970526     C                   MOVEL     FUV(1)        FUN001
121300970526     C                   MOVEL     FUV(2)        FUN002
121400970526     C                   MOVEL     FUV(3)        FUN003
121500970526     C                   MOVEL     FUV(4)        FUN004
121600970526     C                   MOVEL     FUV(5)        FUN005
121700970526     C                   MOVEL     FUV(6)        FUN006
121800970526     C                   END
121900970526     C*
122000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
122100970526     C     FUNGRU        IFEQ      2
122200970526     C                   MOVEL     FUV(7)        FUN001
122300970526     C                   MOVEL     FUV(8)        FUN002
122400970526     C                   MOVEL     FUV(9)        FUN003
122500970526     C                   MOVEL     FUV(10)       FUN004
122600970526     C                   MOVEL     FUV(11)       FUN005
122700970526     C                   MOVEL     FUV(12)       FUN006
122800970526     C                   END
122900970526     C*
123000970526     C* TERZO GRUPPO DI 6 FUNZIONI
123100970526     C     FUNGRU        IFEQ      3
123200970526     C                   MOVEL     FUV(13)       FUN001
123300970526     C                   MOVEL     FUV(14)       FUN002
123400970526     C                   MOVEL     FUV(15)       FUN003
123500970526     C                   MOVEL     FUV(16)       FUN004
123600970526     C                   MOVEL     FUV(17)       FUN005
123700970526     C                   MOVEL     FUV(18)       FUN006
123800970526     C                   END
123900970526     C*
124000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
124100970526     C     FUNGRU        IFEQ      4
124200970526     C                   MOVEL     FUV(19)       FUN001
124300970526     C                   MOVEL     FUV(20)       FUN002
124400970526     C                   MOVEL     FUV(21)       FUN003
124500970526     C                   MOVEL     FUV(22)       FUN004
124600970526     C                   MOVEL     *BLANKS       FUN005
124700970526     C                   MOVEL     *BLANKS       FUN006
124800970526     C                   END
124900970526     C*
125000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
125100970526     C                   MOVEL     *BLANKS       FUN007
125200970526     C                   MOVEL     *BLANKS       FUN008
125300970526     C*
125400970526     C     1             DO        24            I
125500970526     C                   MOVEL     FUD(I)        FUDRIG
125600970526     C     FUDFUN        IFEQ      '24'
125700970526     C     FUDATT        ANDEQ     'S'
125800970526     C                   MOVEL     FUDDES        FUN008
125900970526     C                   END
126000970526     C                   END                                                    DO
126100970526     C*
126200970526     C                   ENDSR
126300970526     C*------------------------------------------------------------------------*
126400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
126500970526     C*------------------------------------------------------------------------*
126600970526     C     FUNCON        BEGSR
126700970526     C*
126800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
126900970526     C                   EXSR      FUNVAR
127000970526     C*
127100970526     C* PULISCO CAMPI DI LAVORO
127200970526     C                   Z-ADD     0             I                 5 0
127300970526     C                   Z-ADD     0             W                 5 0
127400970526     C*
127500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
127600970526     C     FUNKEY        IFNE      *BLANKS
127700970526     C     1             DO        24            I
127800970526     C                   MOVEL     FUD(I)        FUDRIG
127900970526     C*
128000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
128100970526     C     FUNKEY        IFEQ      FUDFUN
128200970526     C     FUDATT        ANDEQ     'S'
128300970526     C                   Z-ADD     I             W
128400970526     C                   END
128500970526     C                   END                                                    DO WHILE
128600970526     C*
128700970526     C* FUNZIONE NON ATTIVA (ERRORE)
128800970526     C     W             IFEQ      0
128900970526     C*
129000970526     C* EMISSIONE MESSAGGIO DI ERRORE
129100970526     C                   MOVEL     MSG(1)        DSMSMS
129200970526     C*
129300970526     C                   END                                                    W=0
129400970526     C                   END                                                    FUNKEY NE *BLANKS
129500970526     C*
129600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
129700970526     C     W             IFNE      0
129800970526     C     FUNKEY        OREQ      *BLANKS
129900970526     C     FUNKEY        IFEQ      *BLANKS
130000970526     C                   EXSR      ENTGES
130100970526     C                   END
130200970526     C     FUNKEY        IFEQ      '03'
130300970526     C                   EXSR      F03GES
130400970526     C                   END
130500970526     C     FUNKEY        IFEQ      '06'
130600970526     C                   EXSR      F06GES
130700970526     C                   END
130800970526     C     FUNKEY        IFEQ      '12'
130900970526     C                   EXSR      F12GES
131000970526     C                   END
131100970526     C     FUNKEY        IFEQ      '24'
131200970526     C                   EXSR      F24GES
131300970526     C                   END
131400970526     C                   END
131500970526     C*
131600970526     C                   ENDSR
131700970526     C*------------------------------------------------------------------------*
131800970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
131900970526     C*------------------------------------------------------------------------*
132000970526     C     FUNVAR        BEGSR
132100970526     C*
132200970526     C* INIZIALIZZA VARIABILE -FUNKEY-
132300970526     C*
132400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
132500970526     C     KEY           IFEQ      ENTER
132600970526     C                   MOVEL     *BLANKS       FUNKEY
132700970526     C                   END
132800970526     C     KEY           IFEQ      F1
132900970526     C                   MOVEL     '01'          FUNKEY
133000970526     C                   END
133100970526     C     KEY           IFEQ      F2
133200970526     C                   MOVEL     '02'          FUNKEY
133300970526     C                   END
133400970526     C     KEY           IFEQ      F3
133500970526     C                   MOVEL     '03'          FUNKEY
133600970526     C                   END
133700970526     C     KEY           IFEQ      F4
133800970526     C                   MOVEL     '04'          FUNKEY
133900970526     C                   END
134000970526     C     KEY           IFEQ      F5
134100970526     C                   MOVEL     '05'          FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F6
134400970526     C                   MOVEL     '06'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F7
134700970526     C                   MOVEL     '07'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F8
135000970526     C                   MOVEL     '08'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F9
135300970526     C                   MOVEL     '09'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      F10
135600970526     C                   MOVEL     '10'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      F11
135900970526     C                   MOVEL     '11'          FUNKEY
136000970526     C                   END
136100970526     C     KEY           IFEQ      F12
136200970526     C                   MOVEL     '12'          FUNKEY
136300970526     C                   END
136400970526     C     KEY           IFEQ      F13
136500970526     C                   MOVEL     '13'          FUNKEY
136600970526     C                   END
136700970526     C     KEY           IFEQ      F14
136800970526     C                   MOVEL     '14'          FUNKEY
136900970526     C                   END
137000970526     C     KEY           IFEQ      F15
137100970526     C                   MOVEL     '15'          FUNKEY
137200970526     C                   END
137300970526     C     KEY           IFEQ      F16
137400970526     C                   MOVEL     '16'          FUNKEY
137500970526     C                   END
137600970526     C     KEY           IFEQ      F17
137700970526     C                   MOVEL     '17'          FUNKEY
137800970526     C                   END
137900970526     C     KEY           IFEQ      F18
138000970526     C                   MOVEL     '18'          FUNKEY
138100970526     C                   END
138200970526     C     KEY           IFEQ      F19
138300970526     C                   MOVEL     '19'          FUNKEY
138400970526     C                   END
138500970526     C     KEY           IFEQ      F20
138600970526     C                   MOVEL     '20'          FUNKEY
138700970526     C                   END
138800970526     C     KEY           IFEQ      F21
138900970526     C                   MOVEL     '21'          FUNKEY
139000970526     C                   END
139100970526     C     KEY           IFEQ      F22
139200970526     C                   MOVEL     '22'          FUNKEY
139300970526     C                   END
139400970526     C     KEY           IFEQ      F23
139500970526     C                   MOVEL     '23'          FUNKEY
139600970526     C                   END
139700970526     C     KEY           IFEQ      F24
139800970526     C                   MOVEL     '24'          FUNKEY
139900970526     C                   END
140000970526     C     KEY           IFEQ      ROLLUP
140100970526     C                   MOVEL     'RU'          FUNKEY
140200970526     C                   END
140300970526     C     KEY           IFEQ      HELP
140400970526     C                   MOVEL     'HP'          FUNKEY
140500970526     C                   END
140600970526     C                   ENDSR
140700970526     C*------------------------------------------------------------------------*
140800970526     C* NOTE COSTRUZIONE SCHIERE
140900970526     C*------------------------------------------------------------------------*
141000970526     C*
141100970526     C* -MSG-MESSAGGI
141200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
141300970526     C* (NON TOCCARE MSG,1 E MSG,2)
141400970526     C*
141500970526     C* -CMD-COMANDI
141600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
141700970526     C*
141800970526     C* -OPD-OPZIONI
141900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
142000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
142100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
142200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
142300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
142400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
142500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
142600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
142700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
142800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
142900970526     C*
143000970526     C* -FUD-FUNZIONI
143100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
143200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
143300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
143400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
143500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
143600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
143700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
143800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
143900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
144000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
144100970526     O*------------------------------------------------------------------------*
144200970526** DEC - DECODIFICHE
144300970526    MODIFICA                                                      1
144400970526  ANNULLAMENTO                                                    2
144500970526VISUALIZZAZIONE                                                   3
144600970526   RIPRISTINO                                                     4
144700970526  INSERIMENTO                                                     5
144800970526** MSG - MESSAGGI
144900970527TISI12R- Tasto funzionale non gestito                                          1
145000970527TISI12R- Inserimento effettuato                                                2
145100970527TISI12R- Modifica effettuata                                                   3
145200970527TISI12R- Annullamento effettuato                                               4
145300970527TISI12R- Ripristino effettuato                                                 5
145400970527TISI12R- Richiesta funzione per record inesistente                             6
145500970527TISI12R- Record momentaneamente allocato: riprovare più tardi                  7
145600970527TISI12R- Richiesta funzione per record esistente                               8
145700000601TISI12R- 9
145800000601TISI12R- 10                                                                   10
145900970527TISI12R- Nazione errata                                                       11
146000970626TISI12R- CAP errato o inesistente                                             12
146100970626TISI12R- 13
146200970527TISI12R- Terminal partenza errato                                             14
146300970527TISI12R- Linea di arrivo errata                                               15
146400970527TISI12R- Zona di consegna errata                                              16
146500060206TISI12R- Zona di consegna NON abilitata                                       17
146600970527TISI12R- 18
146700970527TISI12R- 19
146800970527TISI12R- 20
146900970526** FUD - FUNZIONI
147000970526                   -        (S/N)=N                        funz.01¬
147100970526                   -        (S/N)=N                        funz.02¬
147200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
147300970526                   -        (S/N)=N                        funz.04¬
147400970526                   -        (S/N)=N                        funz.05¬
147500970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
147600970527                   -        (S/N)=N                        funz.07¬
147700970527                   -        (S/N)=N                        funz.08¬
147800970527                   -        (S/N)=N                        funz.09¬
147900970527                   -        (S/N)=N                        funz.10¬
148000970526                   -        (S/N)=N                        funz.11¬
148100970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
148200970526                   -        (S/N)=N                        funz.13¬
148300970526                   -        (S/N)=N                        funz.14¬
148400970526                   -        (S/N)=N                        funz.15¬
148500970526                   -        (S/N)=N                        funz.16¬
148600970526                   -        (S/N)=N                        funz.17¬
148700970526                   -        (S/N)=N                        funz.18¬
148800970526                   -        (S/N)=N                        funz.19¬
148900970526                   -        (S/N)=N                        funz.20¬
149000970526                   -        (S/N)=N                        funz.21¬
149100970526                   -        (S/N)=N                        funz.22¬
149200970526                   -        (S/N)=N                        funz.23¬
149300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
