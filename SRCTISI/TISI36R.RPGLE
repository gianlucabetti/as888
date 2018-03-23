000100970603     H*------------------------------------------------------------------------*
000200970603     H* CAPPARIO - GESTIONE SINONIMI LOCALITA' - ROUTINE
000300970603     H*------------------------------------------------------------------------*
000400970603     H DECEDIT('0,') DATEDIT(*DMY.)
000500970603     F*------------------------------------------------------------------------*
000600970603     F* DATA BASE
000700970603     F*------------------------------------------------------------------------*
000800970603     FTISI36D   CF   E             WORKSTN INFDS(DEVDS1)
000900970603     F                                     SFILE(SI36DMS:MSGULT)
001000970603     FTABEL00F  IF   E           K DISK
001100970603     F*------------
001200970603     F* SINONIMO LOCALITA'
001300970603     F*------------
001400970603     FAZCPS01L  UF A E           K DISK
001500060308     FAZCPS04L  IF   E           K DISK    PREFIX(w_) RENAME(azcps000:azcps004)
001600970603     F*------------
001700970603     F* LOCALITA'
001800970603     F*------------
001900970603     FAZCPL01L  IF   E           K DISK
002000970603     D*------------------------------------------------------------------------*
002100970603     D* SCHIERE
002200970603     D*------------------------------------------------------------------------*
002300970603     D*------------
002400970603     D* DECODIFICHE
002500970603     D*------------
002600970603     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002700970603     D*------------
002800970603     D* MESSAGGI
002900970603     D*------------
003000970603     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
003100970603     D*------------
003200970603     D* DESCRIZIONI TASTI FUNZIONALI
003300970603     D*------------
003400970603     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003500970603     D*------------
003600970603     D* FUNZIONI DA CARICARE A VIDEO
003700970603     D*------------
003800970603     D FUV             S             19    DIM(24)
003900970603     D*------------------------------------------------------------------------*
004000970603     D* INPUT
004100970603     D*------------------------------------------------------------------------*
004200970603     D*------------
004300970603     D* RIDEFINIZIONE FUNZIONI VIDEO
004400970603     D*------------
004500970603     D FUDDS           DS
004600970603     D  FUDDE1                 1      1
004700970603     D  FUDFUN                 2      3
004800970603     D  FUDDE2                 4     19
004900970603     D  FUDFI0                20     34
005000970603     D  FUDATT                35     35
005100970603     D  FUDFI1                36     80
005200970603     D  FUDDES                 1     19
005300970603     D  FUDRIG                 1     80
005400970603     D*------------
005500970603     D* DS GESTIONE TASTI FUNZIONALI
005600970603     D*------------
005700970603     D DSKEY         E DS
005800970603     D DEVDS1          DS
005900970603     D  KEY                  369    369
006000970603     D*------------
006100970603     D* DS "XMSG" - GESTIONE MESSAGGI
006200970603     D*------------
006300970603     D DSMSG         E DS
006400970603     D*------------------
006500970603     D* DS NAZIONI
006600970603     D*------------------
006700970603     D DS15          E DS
006800970626     D*------------------
006900970626     D* DS "TISI95R" - CONTROLLO CAP
007000970626     D*------------------
007100970626     D DSSI95        E DS                  EXTNAME(TISI95DS)
007200970603     D*------------
007300970603     D* DS "TISI82R" - AGGIORNA UNA LOCALITA' DAI SUOI ARCHIVI
007400970603     D*------------
007500970603     D DSSI82        E DS                  EXTNAME(TISI82DS)
007600970603     D*------------
007700970603     D* DS DI PROCEDURA
007800970603     D*------------
007900970603     D DSSI00        E DS                  EXTNAME(TISI00DS)
008000970603     D*------------
008100970603     D* ARCHITETTURA
008200970603     D*------------
008300970603     D KPJBA         E DS
008400970603     C*------------------------------------------------------------------------*
008500970603     C* MAIN LINES
008600970603     C*------------------------------------------------------------------------*
008700970603     C*
008800970603     C* NON CI SONO ERRORI IN ENTRATA
008900970603IF  1C     ERRL00        IFEQ      '0'
009000970603     C*
009100970603     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009200970603DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009300970603     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009400970603     C     FINL00        OREQ      '1'                                          *FINE PGM
009500970603     C*
009600970603     C* EMETTE FORMATO VIDEO
009700970603     C                   EXSR      BUTFMR
009800970603     C*
009900970603     C* GESTIONE TASTI FUNZIONALI
010000970603     C                   EXSR      FUNCON
010100970603E   2C                   ENDDO
010200970603     C*
010300970603     C* CI SONO ERRORI IN ENTRATA
010400970603X   1C                   ELSE
010500970603     C                   MOVEL     '1'           D00ERR
010600970603     C                   MOVEL     DSMSMS        D00MSG
010700970603E   1C                   ENDIF
010800970626     C*
010900970626     C* OPERAZIONI FINALI
011000970626     C                   EXSR      FINSR
011100970603     C* FINE PGM
011200970603     C                   SETON                                        LR
011300970603     C*------------------------------------------------------------------------*
011400970603     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011500970603     C*------------------------------------------------------------------------*
011600970603     C     BUTFMR        BEGSR
011700970603     C*
011800970603     C* EMETTE LA TESTATA
011900970603     C                   WRITE     SI36D01
012000970603     C*
012100970603     C* SPEDISCE EVENTUALE MESSAGGIO
012200970603     C                   EXSR      SNDMSG
012300970603     C*
012400970603     C* EMETTE LE FUNZIONI
012500970603     C                   WRITE     SI36D02
012600970603     C*
012700970603     C* EMETTE IL VIDEO
012800970603     C                   EXFMT     SI36D03
012900970603     C*
013000970603     C* REIMPOSTA FLAG DI ERRORE
013100970603     C                   MOVEL     '0'           ERRL00
013200970603     C*
013300970603     C* REIMPOSTA INDICATORI DI ERRORE
013400970603     C                   SETOFF                                         5859
013500970603     C                   SETOFF                                       606162
013600970603     C                   SETOFF                                       636465
013700970603     C                   SETOFF                                       666768
013800970603     C                   SETOFF                                       697071
013900970603     C                   SETOFF                                       727374
014000970603     C                   SETOFF                                       757677
014100970603     C                   SETOFF                                       787980
014200970603     C                   SETOFF                                       818283
014300970603     C                   SETOFF                                       848586
014400970603     C                   SETOFF                                       878889
014500970603     C*
014600970603     C                   ENDSR
014700970603     C*------------------------------------------------------------------------*
014800970603     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014900970603     C*------------------------------------------------------------------------*
015000970603     C     SNDMSG        BEGSR
015100970603     C*
015200970603     C* PULISCE IL SFILE MESSAGGI
015300970603     C                   MOVEL     '1'           *IN40
015400970603     C                   WRITE     SI36DMC
015500970603     C                   MOVEL     '0'           *IN40
015600970603     C*
015700970603     C* VISUALIZZA IL MESSAGGIO
015800970603     C                   Z-ADD     *ZEROS        MSGULT            4 0
015900970603     C                   CALL      'XMSG'        XMSGPA
016000970603IF  1C     DSMSER        IFEQ      *ZEROS
016100970603     C                   MOVEL     DSMSKE        MSGKEY
016200970603E   1C                   ENDIF
016300970603     C                   Z-ADD     1             MSGULT
016400970603     C                   WRITE     SI36DMS
016500970603     C                   WRITE     SI36DMC
016600970603     C*
016700970603     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016800970603     C                   MOVEL     *BLANKS       DSMSMS
016900970603     C*
017000970603     C                   ENDSR
017100970603     C*------------------------------------------------------------------------*
017200970603     C* ENTGES - GESTIONE ENTER -> INVIO
017300970603     C*------------------------------------------------------------------------*
017400970603     C     ENTGES        BEGSR
017500970603     C*
017600970603     C* CONTROLLA I CAMPI IMMESSI
017700970603     C                   EXSR      DECODI
017800970603     C*
017900970603     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
018000970603IF  1C     DEPOP1        IFEQ      'O05'
018100970603     C                   MOVEL     '1'           FINL00
018200970603E   1C                   ENDIF
018300970603     C*
018400970603     C                   ENDSR
018500970603     C*------------------------------------------------------------------------*
018600970603     C* F03GES - GESTIONE F03 -> FINE
018700970603     C*------------------------------------------------------------------------*
018800970603     C     F03GES        BEGSR
018900970603     C*
019000970603     C* IMPOSTA "PREMUTO F03"
019100970603     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019200970603     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
019300970603     C*
019400970603     C                   ENDSR
019500970603     C*------------------------------------------------------------------------*
019600970603     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019700970603     C*------------------------------------------------------------------------*
019800970603     C     F06GES        BEGSR
019900970603     C*
020000970603     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
020100970603IF  1C     DEPOP1        IFEQ      'F06'
020200970603     C                   EXSR      INSERI
020300970603E   1C                   ENDIF
020400970603IF  1C     DEPOP1        IFEQ      'O02'
020500970603     C                   EXSR      MODIFI
020600970603E   1C                   ENDIF
020700970603IF  1C     DEPOP1        IFEQ      'O04'
020800970603     C                   EXSR      ANNULL
020900970603E   1C                   ENDIF
021000970603IF  1C     DEPOP1        IFEQ      'O07'
021100970603     C                   EXSR      RIPRIS
021200970603E   1C                   ENDIF
021300970603     C*
021400970603     C                   ENDSR
021500970603     C*------------------------------------------------------------------------*
021600970603     C* F12GES - GESTIONE F12 -> RITORNO
021700970603     C*------------------------------------------------------------------------*
021800970603     C     F12GES        BEGSR
021900970603     C*
022000970603     C* IMPOSTA "PREMUTO F12"
022100970603     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022200970603     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
022300970603     C*
022400970603     C                   ENDSR
022500970603     C*------------------------------------------------------------------------*
022600970603     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022700970603     C*------------------------------------------------------------------------*
022800970603     C     F24GES        BEGSR
022900970603     C*
023000970603     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023100970603     C*
023200970603     C                   ENDSR
023300970603     C*------------------------------------------------------------------------*
023400970603     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
023500970603     C*------------------------------------------------------------------------*
023600970603     C     INSERI        BEGSR
023700970603     C*
023800970603     C* CONTROLLA I CAMPI IMMESSI
023900970603     C                   EXSR      DECODI
024000970603     C*
024100970603     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024200970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024300970603     C                   Z-ADD     D00VER        CPSVER                         *CHIAVE
024400970603     C                   MOVEL     VIDNAR        CPSNAR
024500970603     C                   MOVEL     VIDCAP        CPSCAP
024600970603     C                   MOVEL     VIDLOC        CPSLOC
024700970603     C                   MOVEL     VIDSIN        CPSSIN
024800970603     C                   EXSR      IMPCPS                                       *ALTRI CAMPI
024900970603     C                   Z-ADD     *ZEROS        CPSDTR                         *DATA TRASMISSIONE
025000970603     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
025100970603     C                   MOVEL     *BLANKS       CPSATB                         *ANNULLAMENTO
025200970603     C                   WRITE     AZCPS000
025300970603     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
025400970603     C*
025500970603     C* AGGIORNA IL FLAG "ESISTENZA SINONIMI" SULLA LOCALITA'
025600970603     C                   Z-ADD     D00VER        D82VER
025700970603     C                   MOVEL     VIDNAR        D82NAR
025800970603     C                   MOVEL     VIDCAP        D82CAP
025900970603     C                   MOVEL     VIDLOC        D82LOC
026000970603     C                   MOVEL     'S'           D82CHK
026100970603     C                   CALL      'TISI82R'
026200970603     C                   PARM                    DSSI82
026300970603     C*
026400970603     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
026500970603     C                   EXSR      AZZVID
026600001107     C                   MOVEL     *BLANKS       VIDSIN
026700970603E   1C                   ENDIF
026800970603     C*
026900970603     C                   ENDSR
027000970603     C*------------------------------------------------------------------------*
027100970603     C* MODIFI - OPERAZIONI DI MODIFICA
027200970603     C*------------------------------------------------------------------------*
027300970603     C     MODIFI        BEGSR
027400970603     C*
027500970603     C* CONTROLLA I CAMPI IMMESSI
027600970603     C                   EXSR      DECODI
027700970603     C*
027800970603     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
027900970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028000970603     C                   EXSR      LETCPS
028100970603     C*
028200970603     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028300970603IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028400970603     C                   EXSR      IMPCPS                                       *IMPOSTA I CAMPI
028500970603     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
028600970603     C                   MOVEL     *BLANKS       CPSATB                         *ANNULLAMENTO
028700970603     C                   UPDATE    AZCPS000
028800970603     C                   MOVEL     MSG(3)        D00MSG
028900970603     C                   MOVEL     '1'           FINL00                         *FINE PGM
029000970603E   2C                   ENDIF
029100970603     C*
029200970603     C* AGGIORNA IL FLAG "ESISTENZA SINONIMI" SULLA LOCALITA'
029300970603     C                   Z-ADD     D00VER        D82VER
029400970603     C                   MOVEL     VIDNAR        D82NAR
029500970603     C                   MOVEL     VIDCAP        D82CAP
029600970603     C                   MOVEL     VIDLOC        D82LOC
029700970603     C                   MOVEL     'S'           D82CHK
029800970603     C                   CALL      'TISI82R'
029900970603     C                   PARM                    DSSI82
030000970603E   1C                   ENDIF
030100970603     C*
030200970603     C                   ENDSR
030300970603     C*------------------------------------------------------------------------*
030400970603     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
030500970603     C*------------------------------------------------------------------------*
030600970603     C     ANNULL        BEGSR
030700970603     C*
030800970603     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030900970603     C                   EXSR      LETCPS
031000970603     C*
031100970603     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
031200970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031300970603IF  2C     CPSATB        IFEQ      *BLANKS                                      *NO ANNULLATI
031400970603IF  3C     CPSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
031500970603     C                   MOVEL     'A'           CPSATB                         *FLAG ANNULLAMENTO
031600970603     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
031700970603     C                   UPDATE    AZCPS000                                     *ANNULLA
031800970603X   3C                   ELSE                                                   *MAI TRASMESSO
031900970603     C                   DELETE    AZCPS000                                     *CANCELLA
032000970603E   3C                   ENDIF
032100970603E   2C                   ENDIF
032200970603     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
032300970603     C                   MOVEL     '1'           FINL00                         *FINE PGM
032400970603     C*
032500970603     C* AGGIORNA IL FLAG "ESISTENZA SINONIMI" SULLA LOCALITA'
032600970603     C                   Z-ADD     D00VER        D82VER
032700970603     C                   MOVEL     VIDNAR        D82NAR
032800970603     C                   MOVEL     VIDCAP        D82CAP
032900970603     C                   MOVEL     VIDLOC        D82LOC
033000970603     C                   MOVEL     'S'           D82CHK
033100970603     C                   CALL      'TISI82R'
033200970603     C                   PARM                    DSSI82
033300970603E   1C                   ENDIF
033400970603     C*
033500970603     C                   ENDSR
033600970603     C*------------------------------------------------------------------------*
033700970603     C* RIPRIS - OPERAZIONI DI RIPRISTINO
033800970603     C*------------------------------------------------------------------------*
033900970603     C     RIPRIS        BEGSR
034000970603     C*
034100970603     C* CONTROLLA I CAMPI IMMESSI
034200970603     C                   EXSR      DECODI
034300970603     C*
034400970603     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
034500970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034600970603     C                   EXSR      LETCPS
034700970603     C*
034800970603     C* IMPOSTA IL RECORD DA SCRIVERE
034900970603IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035000970603     C                   MOVEL     *BLANKS       CPSATB                         *FLAG ANNULLAMENTO
035100970603     C                   MOVEL     *BLANKS       CPSFTR                         *FLAG TRASMISSIONE
035200970603     C                   UPDATE    AZCPS000                                     *ANNULLA
035300970603E   2C                   ENDIF
035400970603     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
035500970603     C                   MOVEL     '1'           FINL00                         *FINE PGM
035600970603     C*
035700970603     C* AGGIORNA IL FLAG "ESISTENZA SINONIMI" SULLA LOCALITA'
035800970603     C                   Z-ADD     D00VER        D82VER
035900970603     C                   MOVEL     VIDNAR        D82NAR
036000970603     C                   MOVEL     VIDCAP        D82CAP
036100970603     C                   MOVEL     VIDLOC        D82LOC
036200970603     C                   MOVEL     'S'           D82CHK
036300970603     C                   CALL      'TISI82R'
036400970603     C                   PARM                    DSSI82
036500970603E   1C                   ENDIF
036600970603     C*
036700970603     C                   ENDSR
036800970603     C*------------------------------------------------------------------------*
036900970603     C* LETCPS - LEGGE IL RECORD PERL'AGGIORNAMENTO
037000970603     C*------------------------------------------------------------------------*
037100970603     C     LETCPS        BEGSR
037200970603     C*
037300970603     C                   Z-ADD     D00VER        KCSVER
037400980127     C                   MOVEL     D00NAR        KCSNAR
037500980127     C                   MOVEL     D00CAP        KCSCAP
037600980127     C                   MOVEL     D00LOC        KCSLOC
037700980127     C                   MOVEL     D00SIN        KCSSIN
037800970603     C     KEYCPS        CHAIN     AZCPS01L                           9998
037900970603IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
038000970603     C                   MOVEL     '1'           ERRL00                         *ERRORE
038100970603     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
038200970603     C                   MOVEL     '1'           FINL00                         *FINE PGM
038300970603E   1C                   ENDIF
038400970603IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
038500970603     C                   MOVEL     '1'           ERRL00                         *ERRORE
038600970603     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
038700970603     C                   MOVEL     '1'           FINL00                         *FINE PGM
038800970603E   1C                   ENDIF
038900970603     C*
039000970603     C                   ENDSR
039100970603     C*------------------------------------------------------------------------*
039200970603     C* DECODI - CONTROLLA CAMPI E DECODIFICA
039300970603     C*------------------------------------------------------------------------*
039400970603     C     DECODI        BEGSR
039500970603     C*--------------------
039600970603     C* NAZIONE
039700970603     C*--------------------
039800970603IF  1C     ERRL00        IFEQ      '0'
039900970603     C                   MOVEL     *BLANKS       VDDNAR
040000970603     C                   MOVEL     VIDNAR        WNAR
040100970603     C                   MOVEL     *BLANKS       WDNAR
040200970603     C                   EXSR      CHKNAR
040300970603     C                   MOVEL     WNAR          VIDNAR
040400970603     C                   MOVEL     WDNAR         VDDNAR
040500970603IF  2C     ERRL00        IFEQ      '1'
040600970603     C                   SETON                                        60
040700970603     C                   MOVEL     MSG(11)       DSMSMS
040800970603E   2C                   ENDIF
040900970603E   1C                   ENDIF
041000970603     C*--------------------
041100970603     C* CAP
041200970603     C*--------------------
041300970603IF  1C     ERRL00        IFEQ      '0'
041400970603     C                   MOVEL     VIDNAR        WNAR
041500970603     C                   MOVEL     VIDCAP        WCAP
041600970603     C                   EXSR      CHKCAP
041700970603IF  2C     ERRL00        IFEQ      '1'
041800970603     C                   SETON                                        61
041900970603     C                   MOVEL     MSG(12)       DSMSMS
042000970603E   2C                   ENDIF
042100970603E   1C                   ENDIF
042200970603     C*--------------------
042300970603     C* LOCALITA'
042400970603     C*--------------------
042500970603IF  1C     ERRL00        IFEQ      '0'
042600970603IF  2C     VIDLOC        IFEQ      *BLANKS
042700970603     C                   SETON                                        62
042800970603     C                   MOVEL     '1'           ERRL00
042900970603     C                   MOVEL     MSG(13)       DSMSMS
043000970603E   2C                   ENDIF
043100970603E   1C                   ENDIF
043200970603     C*--------------------
043300970603     C* NAZIONE + CAP + LOCALITA'
043400970603     C*--------------------
043500970603IF  1C     ERRL00        IFEQ      '0'
043600970603     C                   Z-ADD     D00VER        KCLVER
043700970603     C                   MOVEL     VIDNAR        KCLNAR
043800970603     C                   MOVEL     VIDCAP        KCLCAP
043900970603     C                   MOVEL     VIDLOC        KCLLOC
044000970603     C     KEYCPL        CHAIN     AZCPL01L                           99
044100970603IF  2C     *IN99         IFEQ      *ON
044200970603     C                   SETON                                        606162
044300970603     C                   MOVEL     '1'           ERRL00
044400970603     C                   MOVEL     MSG(14)       DSMSMS
044500970603E   2C                   ENDIF
044600970603E   1C                   ENDIF
044700970603     C*--------------------
044800970603     C* SINONIMO
044900970603     C*--------------------
045000970603IF  1C     ERRL00        IFEQ      '0'
045100970603IF  2C     VIDSIN        IFEQ      *BLANKS
045200970603     C                   SETON                                        63
045300970603     C                   MOVEL     '1'           ERRL00
045400970603     C                   MOVEL     MSG(15)       DSMSMS
045500970603X   2C                   ELSE
045600970603IF  3C     VIDSIN        IFEQ      VIDLOC
045700970603     C                   SETON                                        63
045800970603     C                   MOVEL     '1'           ERRL00
045900970603     C                   MOVEL     MSG(16)       DSMSMS
046000970603E   3C                   ENDIF
046100970603E   2C                   ENDIF
046200970603E   1C                   ENDIF
046300970603     C*--------------------
046400000609     C* NAZIONE + CAP + LOCALITA'+ SINONIMO  (CHIAVE)
046500970603     C*--------------------
046600970603     C* INSERIMENTO -> RECORD INESISTENTE
046700970603IF  1C     ERRL00        IFEQ      '0'
046800970603IF  2C     DEPOP1        IFEQ      'F06'
046900970603     C                   Z-ADD     D00VER        KCSVER
047000970603     C                   MOVEL     VIDNAR        KCSNAR
047100970603     C                   MOVEL     VIDCAP        KCSCAP
047200970603     C                   MOVEL     VIDLOC        KCSLOC
047300000609     C                   MOVEL     VIDSIN        KCSSIN
047400970603     C     KEYCPS        CHAIN(N)  AZCPS01L                           99
047500970603IF  3C     *IN99         IFEQ      *OFF
047600970603     C                   SETON                                        606162
047700970603     C                   SETON                                        63
047800970603     C                   MOVEL     '1'           ERRL00
047900970603     C                   MOVEL     MSG(8)        DSMSMS
048000970603E   3C                   ENDIF
048100970603E   2C                   ENDIF
048200970603E   1C                   ENDIF
048300060308     C*--------------------
048400060308     C* NAZIONE + CAP + SINONIMO
048500060308     C*--------------------
048600061124     C* INSERIMENTO -> SINONIMO INESISTENTE SU STESSO CAP COME LOCALITA
048700060308IF  1C     ERRL00        IFEQ      '0'
048800061124     C                   Z-ADD     D00VER        KCLVER
048900061124     C                   MOVEL     VIDNAR        KCLNAR
049000061124     C                   MOVEL     VIDCAP        KCLCAP
049100061124     C                   MOVEL     VIDSIN        KCLLOC
049200061030     C     KEYCPL        CHAIN     AZCPL01L                           99
049300061124IF  2C                   IF        %found(AZCPL01L)
049400080327IF  3C***                IF        CPLFI1 = *blanks
049500080327IF  3C                   IF        CPLATB = *blanks
049600060308     C                   SETON                                        606162
049700060308     C                   SETON                                        63
049800060308     C                   MOVEL     '1'           ERRL00
049900061124     C                   MOVEL     MSG(10)       DSMSMS
050000061124E   3C                   ENDIF
050100061124E   2C                   ENDIF
050200061030     C*
050300061124E   1C                   ENDIF
050400061124     C*--------------------
050500061124     C* NAZIONE + CAP + SINONIMO
050600061124     C*--------------------
050700061124     C* INSERIMENTO -> SINONIMO INESISTENTE SU STESSO CAP (A PRESCINDERE DALLA LOCALITA)
050800061124IF  1C     ERRL00        IFEQ      '0'
050900061124     C                   Z-ADD     D00VER        KCSVER
051000061124     C                   MOVEL     VIDNAR        KCSNAR
051100061124     C                   MOVEL     VIDCAP        KCSCAP
051200061124     C                   MOVEL     VIDSIN        KCSSIN
051300061124     C     KEYCPS_4      SETLL     AZCPS04L
051400061124IF  2C                   IF        %equal(AZCPS04L)
051500061124     C     KEYCPS_4      READE     AZCPS04L
051600061124     C                   DOW       not %eof(AZCPS04L)
051700061124     C*
051800061124     C* Se sinonimo nn già annullato verifico
051900061124IF  3C                   IF        CPSATB = *blanks
052000080327     C                   IF        VIDLOC <> CPSLOC
052100061124     C                   SETON                                        606162
052200061124     C                   SETON                                        63
052300061124     C                   MOVEL     '1'           ERRL00
052400061124     C                   MOVEL     MSG(9)        DSMSMS
052500061124     C                   LEAVE
052600080327     C                   ENDIF
052700061124E   3C                   ENDIF
052800061124     C     KEYCPS_4      READE     AZCPS04L
052900061124     C                   ENDDO
053000061124     C*
053100061124E   2C                   ENDIF
053200061124E   1C                   ENDIF
053300970603     C*
053400970603     C                   ENDSR
053500970603     C*------------------------------------------------------------------------*
053600970603     C* CHKNAR - CONTROLLA LA NAZIONE
053700970603     C*------------------------------------------------------------------------*
053800970603     C     CHKNAR        BEGSR
053900970603     C*
054000970603     C     *LIKE         DEFINE    VIDNAR        WNAR
054100970603     C     *LIKE         DEFINE    VDDNAR        WDNAR
054200970603     C*
054300970603     C* RICHIESTO ELENCO
054400970603     C                   SETOFF                                           28
054500970603     C     '?'           SCAN      WNAR                                   28
054600970603IF  1C     *IN28         IFEQ      *ON
054700020906     C                   Z-ADD     1             §KUT              1 0
054800970603     C                   MOVEL     '15'          §COD              2
054900970603     C                   MOVEL     *BLANKS       §KEY              8
055000970603     C                   MOVEL     *BLANKS       §DES             30
055100970603     C                   CALL      'X§TABER'
055200970603     C                   PARM                    §KUT
055300970603     C                   PARM                    §COD
055400970603     C                   PARM                    §KEY
055500970603     C                   PARM                    §DES
055600970603     C                   MOVEL     §KEY          WNAR
055700970603     C                   MOVEL     §DES          WDNAR
055800970603E   1C                   ENDIF
055900970603     C*
056000970603     C* CONTROLLA ESISTENZA CODICE
056100020906     C                   MOVEL     1             KTBKUT
056200970603     C                   MOVEL     '15'          KTBCOD
056300970603     C                   MOVEL(P)  WNAR          KTBKEY
056400970603     C     KEYTAB        CHAIN     TABEL00F                           99
056500970603IF  1C     *IN99         IFEQ      *OFF
056600970603     C     TBLFLG        ANDEQ     *BLANKS
056700970603     C                   MOVEL     TBLUNI        DS15
056800970603     C                   MOVEL     §15DES        WDNAR
056900970603     C*
057000970603     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
057100970603IF  2C     VIDNAR        IFEQ      'I  '
057200970603     C                   MOVEL     '1'           ERRL00
057300970603     C                   MOVEL     *BLANKS       WDNAR
057400970603E   2C                   ENDIF
057500970603X   1C                   ELSE
057600970603     C                   MOVEL     '1'           ERRL00
057700970603E   1C                   ENDIF
057800970603     C*
057900970603     C                   ENDSR
058000970603     C*------------------------------------------------------------------------*
058100970603     C* CHKCAP - CONTROLLA IL CAP
058200970603     C*------------------------------------------------------------------------*
058300970603     C     CHKCAP        BEGSR
058400970603     C*
058500970603     C     *LIKE         DEFINE    VIDCAP        WCAP
058600970626     C*
058700970626     C* CONTROLLA IL CAP
058800970626     C                   CLEAR                   DSSI95
058900970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
059000970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
059100970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
059200970626     C                   MOVEL     WCAP          I95CAP                         *CAP
059300970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
059400970626     C                   CALL      'TISI95R'
059500970626     C                   PARM                    DSSI95
059600970626IF  1C     O95ERR        IFNE      *BLANKS
059700970626     C                   MOVEL     '1'           ERRL00
059800970626E   1C                   ENDIF
059900970603     C*
060000970603     C                   ENDSR
060100970603     C*------------------------------------------------------------------------*
060200970603     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
060300970603     C*------------------------------------------------------------------------*
060400970603     C     IREVID        BEGSR
060500970603     C* CAMPI
060600970603     C                   MOVEL     CPSNAR        VIDNAR
060700970603     C                   MOVEL     CPSCAP        VIDCAP
060800970603     C                   MOVEL     CPSLOC        VIDLOC
060900970603     C                   MOVEL     CPSSIN        VIDSIN
061000970603     C*
061100970603     C                   ENDSR
061200970603     C*------------------------------------------------------------------------*
061300970603     C* AZZVID - AZZERA I CAMPI DEL VIDEO
061400970603     C*------------------------------------------------------------------------*
061500970603     C     AZZVID        BEGSR
061600970603     C* CAMPI
061700970603     C                   MOVEL     D00NAR        VIDNAR
061800970603     C                   MOVEL     D00CAP        VIDCAP
061900970603     C                   MOVEL     D00LOC        VIDLOC
062000001031     C                   MOVEL     D00SIN        VIDSIN
062100970603     C* DECODIFICHE
062200970603     C                   MOVEL     *BLANKS       VDDNAR
062300970603     C*
062400970603     C                   ENDSR
062500970603     C*------------------------------------------------------------------------*
062600970603     C* IMPCPS - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
062700970603     C*------------------------------------------------------------------------*
062800970603     C     IMPCPS        BEGSR
062900970603     C*
063000970603     C                   ENDSR
063100970603     C*------------------------------------------------------------------------*
063200970603     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
063300970603     C*------------------------------------------------------------------------*
063400970603     C     IMPFUN        BEGSR
063500970603     C*
063600970603     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
063700970603IF  1C     DEPOP1        IFEQ      'O05'
063800970603     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
063900970603     C                   MOVEL     'N'           FUDATT
064000970603     C                   MOVEL     FUDRIG        FUD(6)
064100970603E   1C                   ENDIF
064200970603     C*
064300970603     C* GESTIONE TASTI FUNZIONALI A VIDEO
064400970603     C                   Z-ADD     *ZEROS        FUNGRU            5 0
064500970603     C                   EXSR      FUNGES
064600970603     C*
064700970603     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
064800970603     C*---
064900970603     C* 90 -> PROTEZIONE CHIAVE
065000970603     C* 91 -> PROTEZIONE TOTALE
065100970603     C*---
065200970603IF  1C     DEPOP0        IFEQ      'S02'                                        G E S T I O N E
065300970603IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
065400970603     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
065500970603     C                   SETOFF                                       91
065600970603E   2C                   ENDIF
065700970603IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
065800970603     C                   SETON                                        90        *PROTEZIONE CHIAVE
065900970603     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
066000970603E   2C                   ENDIF
066100970603IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
066200970603     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
066300970603     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
066400970603     C                   SETON                                        90        *TUTTE LE PROTEZIONI
066500970603     C                   SETON                                        91
066600970603E   2C                   ENDIF
066700970603X   1C                   ELSE                                                   *I N T E R R O G A
066800970603     C                   SETON                                        90        *TUTTE LE PROTEZIONI
066900970603     C                   SETON                                        91
067000970603E   1C                   ENDIF
067100970603     C*
067200970603     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
067300970603IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
067400970603     C                   Z-ADD     D00VER        KCSVER
067500970603     C                   MOVEL     D00NAR        KCSNAR
067600970603     C                   MOVEL     D00CAP        KCSCAP
067700970603     C                   MOVEL     D00LOC        KCSLOC
067800970603     C                   MOVEL     D00SIN        KCSSIN
067900970603     C     KEYCPS        CHAIN(N)  AZCPS01L                           99
068000970603IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
068100970603     C                   MOVEL     '1'           ERRL00                         *ERRORE
068200970603     C                   MOVEL     MSG(6)        DSMSMS
068300970603X   2C                   ELSE
068400970603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
068500980128IF  3C     DEPOP1        IFEQ      'O05'
068600980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
068700980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
068800980128E   3C                   ENDIF
068900970603E   2C                   ENDIF
069000970603X   1C                   ELSE                                                   *INSERIMENTO
069100970603     C                   EXSR      AZZVID                                       *AZZERA CAMPI
069200970603E   1C                   ENDIF
069300970603     C*
069400970603     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
069500970603IF  1C     DEPOP1        IFEQ      'O02'
069600970603     C                   MOVEL     DEC(1)        VIDFUN
069700970603E   1C                   ENDIF
069800970603IF  1C     DEPOP1        IFEQ      'O04'
069900970603     C                   MOVEL     DEC(2)        VIDFUN
070000970603E   1C                   ENDIF
070100970603IF  1C     DEPOP1        IFEQ      'O05'
070200970603     C                   MOVEL     DEC(3)        VIDFUN
070300970603E   1C                   ENDIF
070400970603IF  1C     DEPOP1        IFEQ      'O07'
070500970603     C                   MOVEL     DEC(4)        VIDFUN
070600970603E   1C                   ENDIF
070700970603IF  1C     DEPOP1        IFEQ      'F06'
070800970603     C                   MOVEL     DEC(5)        VIDFUN
070900970603E   1C                   ENDIF
071000970603     C*
071100970603     C                   ENDSR
071200970626     C*------------------------------------------------------------------------*
071300970626     C* FINSR - OPERAZIONI FINALI
071400970626     C*------------------------------------------------------------------------*
071500970626     C     FINSR         BEGSR
071600970626     C*
071700970626     C* CHIUDE I FILE DEL "TISI95R"
071800970704     C                   CLEAR                   DSSI95
071900970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
072000970626     C                   CALL      'TISI95R'
072100970626     C                   PARM                    DSSI95
072200970626     C*
072300970626     C                   ENDSR
072400970603     C*------------------------------------------------------------------------*
072500970603     C* *INZSR - OPERAZIONI INIZIALI
072600970603     C*------------------------------------------------------------------------*
072700970603     C     *INZSR        BEGSR
072800970603     C*---
072900970603     C* RICEVIMENTO PARAMETRI
073000970603     C*---
073100970603     C     *ENTRY        PLIST
073200970612     C                   PARM                    KPJBA
073300970612     C                   PARM                    DSSI00
073400970603     C*
073500970603     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
073600970603     C     XMSGPA        PLIST
073700970603     C                   PARM                    DSMSMS
073800970603     C                   PARM                    DSMSPG
073900970603     C                   PARM                    DSMSKE
074000970603     C                   PARM                    DSMSER
074100970603     C*
074200970603     C* IMPOSTA LA DS -TASTI PREMUTI-
074300970603     C                   CALL      'XKEY'
074400970603     C                   PARM                    DSKEY
074500970603     C*---
074600970603     C* VARIABILI NON RIFERITE AL DATA BASE
074700970603     C*---
074800970603     C     *LIKE         DEFINE    FUDFUN        FUNKEY
074900970603     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
075000970603     C     *LIKE         DEFINE    D00OP1        DEPOP1
075100970603     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
075200970603     C     *LIKE         DEFINE    D00OP1        WOP1
075300970603     C     *LIKE         DEFINE    D00NAR        DNAR
075400970603     C     *LIKE         DEFINE    D00CAP        DCAP
075500970603     C     *LIKE         DEFINE    D00LOC        DLOC
075600970603     C*---
075700970603     C* VARIABILI DI CONTROLLO
075800970603     C*---
075900970603     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
076000970603     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
076100970603     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
076200970603     C                   MOVEL     '0'           ERRL00            1            *ERRORE
076300970603     C*
076400970603     C* IMPOSTA LA DS -INVIO MESSAGGI-
076500970603     C                   MOVEL     'TISI36R'     DSMSPG
076600970603     C                   MOVEL     'TISI36R'     MSGQUE
076700970603     C                   MOVEL     *BLANKS       MSGKEY
076800970603     C                   MOVEL     *BLANKS       DSMSMS
076900970603     C*---
077000970603     C* VARIABILI RIFERITE AL DATA BASE
077100970603     C*---
077200970603     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
077300970603     C     *LIKE         DEFINE    TBLCOD        KTBCOD
077400970603     C     *LIKE         DEFINE    TBLKEY        KTBKEY
077500970603     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
077600970603     C     *LIKE         DEFINE    CPSNAR        KCSNAR
077700970603     C     *LIKE         DEFINE    CPSCAP        KCSCAP
077800970603     C     *LIKE         DEFINE    CPSLOC        KCSLOC
077900970603     C     *LIKE         DEFINE    CPSSIN        KCSSIN
078000970603     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
078100970603     C     *LIKE         DEFINE    CPLNAR        KCLNAR
078200970603     C     *LIKE         DEFINE    CPLCAP        KCLCAP
078300970603     C     *LIKE         DEFINE    CPLLOC        KCLLOC
078400970603     C*---
078500970603     C* CHIAVI DI LETTURA
078600970603     C*---
078700970603     C* LETTURA TABEL00F - COMPLETA
078800970603     C     KEYTAB        KLIST
078900970603     C                   KFLD                    KTBKUT                         *CODICE UTENTE
079000970603     C                   KFLD                    KTBCOD                         *TABELLA
079100970603     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
079200970603     C* LETTURA AZCPS01L - COMPLETA
079300970603     C     KEYCPS        KLIST
079400970603     C                   KFLD                    KCSVER                         *VERSIONE
079500970603     C                   KFLD                    KCSNAR                         *NAZIONE
079600970603     C                   KFLD                    KCSCAP                         *CAP
079700970603     C                   KFLD                    KCSLOC                         *LOCALITA'
079800970603     C                   KFLD                    KCSSIN                         *SINONIMO LOCALITA'
079900060308     C* LETTURA AZCPS04L - COMPLETA
080000060308     C     KEYCPS_4      KLIST
080100060308     C                   KFLD                    KCSVER                         *VERSIONE
080200060308     C                   KFLD                    KCSNAR                         *NAZIONE
080300060308     C                   KFLD                    KCSCAP                         *CAP
080400060308     C                   KFLD                    KCSSIN                         *SINONIMO LOCALITA'
080500970603     C* LETTURA AZCPL01L - COMPLETA
080600970603     C     KEYCPL        KLIST
080700970603     C                   KFLD                    KCLVER                         *VERSIONE
080800970603     C                   KFLD                    KCLNAR                         *NAZIONE
080900970603     C                   KFLD                    KCLCAP                         *CAP
081000970603     C                   KFLD                    KCLLOC                         *LOCALITA'
081100970603     C*
081200970603     C* DEPOSITA LE OPZIONI DI ENTRATA
081300970603     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
081400970603     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
081500970603     C*
081600970603     C* IMPOSTA LE FUNZIONI DI INGRESSO
081700970603     C                   EXSR      IMPFUN
081800970603     C*
081900970603     C                   ENDSR
082000970603     C*------------------------------------------------------------------------*
082100970603     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
082200970603     C*------------------------------------------------------------------------*
082300970603     C     FUNGES        BEGSR
082400970603     C*
082500970603     C* PULISCO CAMPI DI LAVORO
082600970603     C                   Z-ADD     0             W                 5 0
082700970603     C*
082800970603     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
082900970603     C* E CARICO TABELLA FUNZIONI ATTIVE
083000970603     C     1             DO        24            I
083100970603     C                   MOVEL     FUD(I)        FUDRIG
083200970603     C*
083300970603     C* PULIZIA FUNZIONI PRECEDENTI
083400970603     C                   MOVEL     *BLANKS       FUV(I)
083500970603     C*
083600970603     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
083700970603     C* (SALTA F24)
083800970603     C     FUDATT        IFEQ      'S'
083900970603     C     FUDFUN        ANDNE     '24'
084000970603     C                   ADD       1             W
084100970603     C                   MOVEL     FUDDES        FUV(W)
084200970603     C                   END
084300970603     C                   END                                                    DO WHILE
084400970603     C*
084500970603     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
084600970603     C* ALTRIMENTI DISATTIVO F24
084700970603     C     1             DO        24            I
084800970603     C                   MOVEL     FUD(I)        FUDRIG
084900970603     C*
085000970603     C     FUDFUN        IFEQ      '24'
085100970603     C     W             IFGT      6
085200970603     C                   MOVEL     'S'           FUDATT
085300970603     C                   ELSE
085400970603     C                   MOVEL     *BLANKS       FUDATT
085500970603     C                   END                                                    W GT 8
085600970603     C                   MOVEL     FUDRIG        FUD(I)
085700970603     C                   END
085800970603     C                   END                                                    DO WHILE
085900970603     C*
086000970603     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
086100970603     C     W             DIV       6             I
086200970603     C                   MVR                     Z
086300970603     C*
086400970603     C     Z             IFNE      0
086500970603     C     I             ADD       1             Z
086600970603     C                   ELSE
086700970603     C                   Z-ADD     I             Z                 5 0
086800970603     C                   END
086900970603     C*
087000970603     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
087100970603     C                   ADD       1             FUNGRU
087200970603     C     FUNGRU        IFGT      Z
087300970603     C     FUNGRU        ORLT      1
087400970603     C                   Z-ADD     1             FUNGRU
087500970603     C                   END
087600970603     C*
087700970603     C* PRIMO GRUPPO DI 6 FUNZIONI
087800970603     C     FUNGRU        IFEQ      1
087900970603     C                   MOVEL     FUV(1)        FUN001
088000970603     C                   MOVEL     FUV(2)        FUN002
088100970603     C                   MOVEL     FUV(3)        FUN003
088200970603     C                   MOVEL     FUV(4)        FUN004
088300970603     C                   MOVEL     FUV(5)        FUN005
088400970603     C                   MOVEL     FUV(6)        FUN006
088500970603     C                   END
088600970603     C*
088700970603     C* SECONDO GRUPPO DI 6 FUNZIONI
088800970603     C     FUNGRU        IFEQ      2
088900970603     C                   MOVEL     FUV(7)        FUN001
089000970603     C                   MOVEL     FUV(8)        FUN002
089100970603     C                   MOVEL     FUV(9)        FUN003
089200970603     C                   MOVEL     FUV(10)       FUN004
089300970603     C                   MOVEL     FUV(11)       FUN005
089400970603     C                   MOVEL     FUV(12)       FUN006
089500970603     C                   END
089600970603     C*
089700970603     C* TERZO GRUPPO DI 6 FUNZIONI
089800970603     C     FUNGRU        IFEQ      3
089900970603     C                   MOVEL     FUV(13)       FUN001
090000970603     C                   MOVEL     FUV(14)       FUN002
090100970603     C                   MOVEL     FUV(15)       FUN003
090200970603     C                   MOVEL     FUV(16)       FUN004
090300970603     C                   MOVEL     FUV(17)       FUN005
090400970603     C                   MOVEL     FUV(18)       FUN006
090500970603     C                   END
090600970603     C*
090700970603     C* QUARTO GRUPPO DI 6 FUNZIONI
090800970603     C     FUNGRU        IFEQ      4
090900970603     C                   MOVEL     FUV(19)       FUN001
091000970603     C                   MOVEL     FUV(20)       FUN002
091100970603     C                   MOVEL     FUV(21)       FUN003
091200970603     C                   MOVEL     FUV(22)       FUN004
091300970603     C                   MOVEL     *BLANKS       FUN005
091400970603     C                   MOVEL     *BLANKS       FUN006
091500970603     C                   END
091600970603     C*
091700970603     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
091800970603     C                   MOVEL     *BLANKS       FUN007
091900970603     C                   MOVEL     *BLANKS       FUN008
092000970603     C*
092100970603     C     1             DO        24            I
092200970603     C                   MOVEL     FUD(I)        FUDRIG
092300970603     C     FUDFUN        IFEQ      '24'
092400970603     C     FUDATT        ANDEQ     'S'
092500970603     C                   MOVEL     FUDDES        FUN008
092600970603     C                   END
092700970603     C                   END                                                    DO
092800970603     C*
092900970603     C                   ENDSR
093000970603     C*------------------------------------------------------------------------*
093100970603     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
093200970603     C*------------------------------------------------------------------------*
093300970603     C     FUNCON        BEGSR
093400970603     C*
093500970603     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
093600970603     C                   EXSR      FUNVAR
093700970603     C*
093800970603     C* PULISCO CAMPI DI LAVORO
093900970603     C                   Z-ADD     0             I                 5 0
094000970603     C                   Z-ADD     0             W                 5 0
094100970603     C*
094200970603     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
094300970603     C     FUNKEY        IFNE      *BLANKS
094400970603     C     1             DO        24            I
094500970603     C                   MOVEL     FUD(I)        FUDRIG
094600970603     C*
094700970603     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
094800970603     C     FUNKEY        IFEQ      FUDFUN
094900970603     C     FUDATT        ANDEQ     'S'
095000970603     C                   Z-ADD     I             W
095100970603     C                   END
095200970603     C                   END                                                    DO WHILE
095300970603     C*
095400970603     C* FUNZIONE NON ATTIVA (ERRORE)
095500970603     C     W             IFEQ      0
095600970603     C*
095700970603     C* EMISSIONE MESSAGGIO DI ERRORE
095800970603     C                   MOVEL     MSG(1)        DSMSMS
095900970603     C*
096000970603     C                   END                                                    W=0
096100970603     C                   END                                                    FUNKEY NE *BLANKS
096200970603     C*
096300970603     C* FUNZIONE ATTIVA (PROCEDI ...)
096400970603     C     W             IFNE      0
096500970603     C     FUNKEY        OREQ      *BLANKS
096600970603     C     FUNKEY        IFEQ      *BLANKS
096700970603     C                   EXSR      ENTGES
096800970603     C                   END
096900970603     C     FUNKEY        IFEQ      '03'
097000970603     C                   EXSR      F03GES
097100970603     C                   END
097200970603     C     FUNKEY        IFEQ      '06'
097300970603     C                   EXSR      F06GES
097400970603     C                   END
097500970603     C     FUNKEY        IFEQ      '12'
097600970603     C                   EXSR      F12GES
097700970603     C                   END
097800970603     C     FUNKEY        IFEQ      '24'
097900970603     C                   EXSR      F24GES
098000970603     C                   END
098100970603     C                   END
098200970603     C*
098300970603     C                   ENDSR
098400970603     C*------------------------------------------------------------------------*
098500970603     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
098600970603     C*------------------------------------------------------------------------*
098700970603     C     FUNVAR        BEGSR
098800970603     C*
098900970603     C* INIZIALIZZA VARIABILE -FUNKEY-
099000970603     C*
099100970603     C* FUNKEY=*BLANKS SIGNIFICA ENTER
099200970603     C     KEY           IFEQ      ENTER
099300970603     C                   MOVEL     *BLANKS       FUNKEY
099400970603     C                   END
099500970603     C     KEY           IFEQ      F1
099600970603     C                   MOVEL     '01'          FUNKEY
099700970603     C                   END
099800970603     C     KEY           IFEQ      F2
099900970603     C                   MOVEL     '02'          FUNKEY
100000970603     C                   END
100100970603     C     KEY           IFEQ      F3
100200970603     C                   MOVEL     '03'          FUNKEY
100300970603     C                   END
100400970603     C     KEY           IFEQ      F4
100500970603     C                   MOVEL     '04'          FUNKEY
100600970603     C                   END
100700970603     C     KEY           IFEQ      F5
100800970603     C                   MOVEL     '05'          FUNKEY
100900970603     C                   END
101000970603     C     KEY           IFEQ      F6
101100970603     C                   MOVEL     '06'          FUNKEY
101200970603     C                   END
101300970603     C     KEY           IFEQ      F7
101400970603     C                   MOVEL     '07'          FUNKEY
101500970603     C                   END
101600970603     C     KEY           IFEQ      F8
101700970603     C                   MOVEL     '08'          FUNKEY
101800970603     C                   END
101900970603     C     KEY           IFEQ      F9
102000970603     C                   MOVEL     '09'          FUNKEY
102100970603     C                   END
102200970603     C     KEY           IFEQ      F10
102300970603     C                   MOVEL     '10'          FUNKEY
102400970603     C                   END
102500970603     C     KEY           IFEQ      F11
102600970603     C                   MOVEL     '11'          FUNKEY
102700970603     C                   END
102800970603     C     KEY           IFEQ      F12
102900970603     C                   MOVEL     '12'          FUNKEY
103000970603     C                   END
103100970603     C     KEY           IFEQ      F13
103200970603     C                   MOVEL     '13'          FUNKEY
103300970603     C                   END
103400970603     C     KEY           IFEQ      F14
103500970603     C                   MOVEL     '14'          FUNKEY
103600970603     C                   END
103700970603     C     KEY           IFEQ      F15
103800970603     C                   MOVEL     '15'          FUNKEY
103900970603     C                   END
104000970603     C     KEY           IFEQ      F16
104100970603     C                   MOVEL     '16'          FUNKEY
104200970603     C                   END
104300970603     C     KEY           IFEQ      F17
104400970603     C                   MOVEL     '17'          FUNKEY
104500970603     C                   END
104600970603     C     KEY           IFEQ      F18
104700970603     C                   MOVEL     '18'          FUNKEY
104800970603     C                   END
104900970603     C     KEY           IFEQ      F19
105000970603     C                   MOVEL     '19'          FUNKEY
105100970603     C                   END
105200970603     C     KEY           IFEQ      F20
105300970603     C                   MOVEL     '20'          FUNKEY
105400970603     C                   END
105500970603     C     KEY           IFEQ      F21
105600970603     C                   MOVEL     '21'          FUNKEY
105700970603     C                   END
105800970603     C     KEY           IFEQ      F22
105900970603     C                   MOVEL     '22'          FUNKEY
106000970603     C                   END
106100970603     C     KEY           IFEQ      F23
106200970603     C                   MOVEL     '23'          FUNKEY
106300970603     C                   END
106400970603     C     KEY           IFEQ      F24
106500970603     C                   MOVEL     '24'          FUNKEY
106600970603     C                   END
106700970603     C     KEY           IFEQ      ROLLUP
106800970603     C                   MOVEL     'RU'          FUNKEY
106900970603     C                   END
107000970603     C     KEY           IFEQ      HELP
107100970603     C                   MOVEL     'HP'          FUNKEY
107200970603     C                   END
107300970603     C                   ENDSR
107400970603     C*------------------------------------------------------------------------*
107500970603     C* NOTE COSTRUZIONE SCHIERE
107600970603     C*------------------------------------------------------------------------*
107700970603     C*
107800970603     C* -MSG-MESSAGGI
107900970603     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
108000970603     C* (NON TOCCARE MSG,1 E MSG,2)
108100970603     C*
108200970603     C* -CMD-COMANDI
108300970603     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
108400970603     C*
108500970603     C* -OPD-OPZIONI
108600970603     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
108700970603     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
108800970603     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
108900970603     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
109000970603     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
109100970603     C*  . POSSONO ESSERE IN ORDINE SPARSO
109200970603     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
109300970603     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
109400970603     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
109500970603     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
109600970603     C*
109700970603     C* -FUD-FUNZIONI
109800970603     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
109900970603     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
110000970603     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
110100970603     C*  . POSSONO ESSERE IN ORDINE SPARSO
110200970603     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
110300970603     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
110400970603     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
110500970603     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
110600970603     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
110700970603     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
110800970603     O*------------------------------------------------------------------------*
110900970603** DEC - DECODIFICHE
111000970603    MODIFICA                                                      1
111100970603  ANNULLAMENTO                                                    2
111200970603VISUALIZZAZIONE                                                   3
111300970603   RIPRISTINO                                                     4
111400970603  INSERIMENTO                                                     5
111500970603** MSG - MESSAGGI
111600970603TISI36R- Tasto funzionale non gestito                                          1
111700970603TISI36R- Inserimento effettuato                                                2
111800970603TISI36R- Modifica effettuata                                                   3
111900970603TISI36R- Annullamento effettuato                                               4
112000970603TISI36R- Ripristino effettuato                                                 5
112100970603TISI36R- Richiesta funzione per record inesistente                             6
112200970603TISI36R- Record momentaneamente allocato: riprovare più tardi                  7
112300970603TISI36R- Richiesta funzione per record esistente                               8
112400060308TISI36R- Sinonimo già presente sullo stesso CAP                                9
112500061124TISI36R- Sinonimo già presente come località sullo stesso CAP                 10
112600970603TISI36R- Nazione errata                                                       11
112700970603TISI36R- Cap errato                                                           12
112800970603TISI36R- Località errata                                                      13
112900970603TISI12R- Località inesistente                                                 14
113000970603TISI36R- Sinonimo errato                                                      15
113100970603TISI36R- Il sinonimo deve essere diverso dalla località                       16
113200970603TISI36R- 17
113300970603TISI36R- 18
113400970603TISI36R- 19
113500970603TISI36R- 20
113600970603** FUD - FUNZIONI
113700970603                   -        (S/N)=N                        funz.01¬
113800970603                   -        (S/N)=N                        funz.02¬
113900970603F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
114000970603                   -        (S/N)=N                        funz.04¬
114100970603                   -        (S/N)=N                        funz.05¬
114200970603F06=Aggiornamento  -        (S/N)=S                        funz.06¬
114300970603                   -        (S/N)=N                        funz.07¬
114400970603                   -        (S/N)=N                        funz.08¬
114500970603                   -        (S/N)=N                        funz.09¬
114600970603                   -        (S/N)=N                        funz.10¬
114700970603                   -        (S/N)=N                        funz.11¬
114800970603F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
114900970603                   -        (S/N)=N                        funz.13¬
115000970603                   -        (S/N)=N                        funz.14¬
115100970603                   -        (S/N)=N                        funz.15¬
115200970603                   -        (S/N)=N                        funz.16¬
115300970603                   -        (S/N)=N                        funz.17¬
115400970603                   -        (S/N)=N                        funz.18¬
115500970603                   -        (S/N)=N                        funz.19¬
115600970603                   -        (S/N)=N                        funz.20¬
115700970603                   -        (S/N)=N                        funz.21¬
115800970603                   -        (S/N)=N                        funz.22¬
115900970603                   -        (S/N)=N                        funz.23¬
116000970603F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
