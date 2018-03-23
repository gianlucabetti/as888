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
049400061124IF  3C                   IF        CPLFI1 = *blanks
049500060308     C                   SETON                                        606162
049600060308     C                   SETON                                        63
049700060308     C                   MOVEL     '1'           ERRL00
049800061124     C                   MOVEL     MSG(10)       DSMSMS
049900061124E   3C                   ENDIF
050000061124E   2C                   ENDIF
050100061030     C*
050200061124E   1C                   ENDIF
050300061124     C*--------------------
050400061124     C* NAZIONE + CAP + SINONIMO
050500061124     C*--------------------
050600061124     C* INSERIMENTO -> SINONIMO INESISTENTE SU STESSO CAP (A PRESCINDERE DALLA LOCALITA)
050700061124IF  1C     ERRL00        IFEQ      '0'
050800061124     C                   Z-ADD     D00VER        KCSVER
050900061124     C                   MOVEL     VIDNAR        KCSNAR
051000061124     C                   MOVEL     VIDCAP        KCSCAP
051100061124     C                   MOVEL     VIDSIN        KCSSIN
051200061124     C     KEYCPS_4      SETLL     AZCPS04L
051300061124IF  2C                   IF        %equal(AZCPS04L)
051400061124     C     KEYCPS_4      READE     AZCPS04L
051500061124     C                   DOW       not %eof(AZCPS04L)
051600061124     C*
051700061124     C* Se sinonimo nn già annullato verifico
051800061124IF  3C                   IF        CPSATB = *blanks
051900061124     C                   SETON                                        606162
052000061124     C                   SETON                                        63
052100061124     C                   MOVEL     '1'           ERRL00
052200061124     C                   MOVEL     MSG(9)        DSMSMS
052300061124     C                   LEAVE
052400061124E   3C                   ENDIF
052500061124     C     KEYCPS_4      READE     AZCPS04L
052600061124     C                   ENDDO
052700061124     C*
052800061124E   2C                   ENDIF
052900061124E   1C                   ENDIF
053000970603     C*
053100970603     C                   ENDSR
053200970603     C*------------------------------------------------------------------------*
053300970603     C* CHKNAR - CONTROLLA LA NAZIONE
053400970603     C*------------------------------------------------------------------------*
053500970603     C     CHKNAR        BEGSR
053600970603     C*
053700970603     C     *LIKE         DEFINE    VIDNAR        WNAR
053800970603     C     *LIKE         DEFINE    VDDNAR        WDNAR
053900970603     C*
054000970603     C* RICHIESTO ELENCO
054100970603     C                   SETOFF                                           28
054200970603     C     '?'           SCAN      WNAR                                   28
054300970603IF  1C     *IN28         IFEQ      *ON
054400020906     C                   Z-ADD     1             §KUT              1 0
054500970603     C                   MOVEL     '15'          §COD              2
054600970603     C                   MOVEL     *BLANKS       §KEY              8
054700970603     C                   MOVEL     *BLANKS       §DES             30
054800970603     C                   CALL      'X§TABER'
054900970603     C                   PARM                    §KUT
055000970603     C                   PARM                    §COD
055100970603     C                   PARM                    §KEY
055200970603     C                   PARM                    §DES
055300970603     C                   MOVEL     §KEY          WNAR
055400970603     C                   MOVEL     §DES          WDNAR
055500970603E   1C                   ENDIF
055600970603     C*
055700970603     C* CONTROLLA ESISTENZA CODICE
055800020906     C                   MOVEL     1             KTBKUT
055900970603     C                   MOVEL     '15'          KTBCOD
056000970603     C                   MOVEL(P)  WNAR          KTBKEY
056100970603     C     KEYTAB        CHAIN     TABEL00F                           99
056200970603IF  1C     *IN99         IFEQ      *OFF
056300970603     C     TBLFLG        ANDEQ     *BLANKS
056400970603     C                   MOVEL     TBLUNI        DS15
056500970603     C                   MOVEL     §15DES        WDNAR
056600970603     C*
056700970603     C* NON ACCETTA "I" COME NAZIONE ITALIA (DEVE ESSERE=' ')
056800970603IF  2C     VIDNAR        IFEQ      'I  '
056900970603     C                   MOVEL     '1'           ERRL00
057000970603     C                   MOVEL     *BLANKS       WDNAR
057100970603E   2C                   ENDIF
057200970603X   1C                   ELSE
057300970603     C                   MOVEL     '1'           ERRL00
057400970603E   1C                   ENDIF
057500970603     C*
057600970603     C                   ENDSR
057700970603     C*------------------------------------------------------------------------*
057800970603     C* CHKCAP - CONTROLLA IL CAP
057900970603     C*------------------------------------------------------------------------*
058000970603     C     CHKCAP        BEGSR
058100970603     C*
058200970603     C     *LIKE         DEFINE    VIDCAP        WCAP
058300970626     C*
058400970626     C* CONTROLLA IL CAP
058500970626     C                   CLEAR                   DSSI95
058600970626     C                   MOVEL     *BLANKS       I95TLA                         *TIPO LANCIO
058700970626     C                   MOVEL     '3'           I95TCN                         *CONTROLLO ESISTENZA
058800970626     C                   MOVEL     WNAR          I95NAR                         *NAZIONE
058900970626     C                   MOVEL     WCAP          I95CAP                         *CAP
059000970626     C                   Z-ADD     D00DRI        I95DAT                         *DATA RIFERIMENTO
059100970626     C                   CALL      'TISI95R'
059200970626     C                   PARM                    DSSI95
059300970626IF  1C     O95ERR        IFNE      *BLANKS
059400970626     C                   MOVEL     '1'           ERRL00
059500970626E   1C                   ENDIF
059600970603     C*
059700970603     C                   ENDSR
059800970603     C*------------------------------------------------------------------------*
059900970603     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
060000970603     C*------------------------------------------------------------------------*
060100970603     C     IREVID        BEGSR
060200970603     C* CAMPI
060300970603     C                   MOVEL     CPSNAR        VIDNAR
060400970603     C                   MOVEL     CPSCAP        VIDCAP
060500970603     C                   MOVEL     CPSLOC        VIDLOC
060600970603     C                   MOVEL     CPSSIN        VIDSIN
060700970603     C*
060800970603     C                   ENDSR
060900970603     C*------------------------------------------------------------------------*
061000970603     C* AZZVID - AZZERA I CAMPI DEL VIDEO
061100970603     C*------------------------------------------------------------------------*
061200970603     C     AZZVID        BEGSR
061300970603     C* CAMPI
061400970603     C                   MOVEL     D00NAR        VIDNAR
061500970603     C                   MOVEL     D00CAP        VIDCAP
061600970603     C                   MOVEL     D00LOC        VIDLOC
061700001031     C                   MOVEL     D00SIN        VIDSIN
061800970603     C* DECODIFICHE
061900970603     C                   MOVEL     *BLANKS       VDDNAR
062000970603     C*
062100970603     C                   ENDSR
062200970603     C*------------------------------------------------------------------------*
062300970603     C* IMPCPS - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
062400970603     C*------------------------------------------------------------------------*
062500970603     C     IMPCPS        BEGSR
062600970603     C*
062700970603     C                   ENDSR
062800970603     C*------------------------------------------------------------------------*
062900970603     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
063000970603     C*------------------------------------------------------------------------*
063100970603     C     IMPFUN        BEGSR
063200970603     C*
063300970603     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
063400970603IF  1C     DEPOP1        IFEQ      'O05'
063500970603     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
063600970603     C                   MOVEL     'N'           FUDATT
063700970603     C                   MOVEL     FUDRIG        FUD(6)
063800970603E   1C                   ENDIF
063900970603     C*
064000970603     C* GESTIONE TASTI FUNZIONALI A VIDEO
064100970603     C                   Z-ADD     *ZEROS        FUNGRU            5 0
064200970603     C                   EXSR      FUNGES
064300970603     C*
064400970603     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
064500970603     C*---
064600970603     C* 90 -> PROTEZIONE CHIAVE
064700970603     C* 91 -> PROTEZIONE TOTALE
064800970603     C*---
064900970603IF  1C     DEPOP0        IFEQ      'S02'                                        G E S T I O N E
065000970603IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
065100970603     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
065200970603     C                   SETOFF                                       91
065300970603E   2C                   ENDIF
065400970603IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
065500970603     C                   SETON                                        90        *PROTEZIONE CHIAVE
065600970603     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
065700970603E   2C                   ENDIF
065800970603IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
065900970603     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
066000970603     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
066100970603     C                   SETON                                        90        *TUTTE LE PROTEZIONI
066200970603     C                   SETON                                        91
066300970603E   2C                   ENDIF
066400970603X   1C                   ELSE                                                   *I N T E R R O G A
066500970603     C                   SETON                                        90        *TUTTE LE PROTEZIONI
066600970603     C                   SETON                                        91
066700970603E   1C                   ENDIF
066800970603     C*
066900970603     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
067000970603IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
067100970603     C                   Z-ADD     D00VER        KCSVER
067200970603     C                   MOVEL     D00NAR        KCSNAR
067300970603     C                   MOVEL     D00CAP        KCSCAP
067400970603     C                   MOVEL     D00LOC        KCSLOC
067500970603     C                   MOVEL     D00SIN        KCSSIN
067600970603     C     KEYCPS        CHAIN(N)  AZCPS01L                           99
067700970603IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
067800970603     C                   MOVEL     '1'           ERRL00                         *ERRORE
067900970603     C                   MOVEL     MSG(6)        DSMSMS
068000970603X   2C                   ELSE
068100970603     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
068200980128IF  3C     DEPOP1        IFEQ      'O05'
068300980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
068400980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
068500980128E   3C                   ENDIF
068600970603E   2C                   ENDIF
068700970603X   1C                   ELSE                                                   *INSERIMENTO
068800970603     C                   EXSR      AZZVID                                       *AZZERA CAMPI
068900970603E   1C                   ENDIF
069000970603     C*
069100970603     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
069200970603IF  1C     DEPOP1        IFEQ      'O02'
069300970603     C                   MOVEL     DEC(1)        VIDFUN
069400970603E   1C                   ENDIF
069500970603IF  1C     DEPOP1        IFEQ      'O04'
069600970603     C                   MOVEL     DEC(2)        VIDFUN
069700970603E   1C                   ENDIF
069800970603IF  1C     DEPOP1        IFEQ      'O05'
069900970603     C                   MOVEL     DEC(3)        VIDFUN
070000970603E   1C                   ENDIF
070100970603IF  1C     DEPOP1        IFEQ      'O07'
070200970603     C                   MOVEL     DEC(4)        VIDFUN
070300970603E   1C                   ENDIF
070400970603IF  1C     DEPOP1        IFEQ      'F06'
070500970603     C                   MOVEL     DEC(5)        VIDFUN
070600970603E   1C                   ENDIF
070700970603     C*
070800970603     C                   ENDSR
070900970626     C*------------------------------------------------------------------------*
071000970626     C* FINSR - OPERAZIONI FINALI
071100970626     C*------------------------------------------------------------------------*
071200970626     C     FINSR         BEGSR
071300970626     C*
071400970626     C* CHIUDE I FILE DEL "TISI95R"
071500970704     C                   CLEAR                   DSSI95
071600970626     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
071700970626     C                   CALL      'TISI95R'
071800970626     C                   PARM                    DSSI95
071900970626     C*
072000970626     C                   ENDSR
072100970603     C*------------------------------------------------------------------------*
072200970603     C* *INZSR - OPERAZIONI INIZIALI
072300970603     C*------------------------------------------------------------------------*
072400970603     C     *INZSR        BEGSR
072500970603     C*---
072600970603     C* RICEVIMENTO PARAMETRI
072700970603     C*---
072800970603     C     *ENTRY        PLIST
072900970612     C                   PARM                    KPJBA
073000970612     C                   PARM                    DSSI00
073100970603     C*
073200970603     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
073300970603     C     XMSGPA        PLIST
073400970603     C                   PARM                    DSMSMS
073500970603     C                   PARM                    DSMSPG
073600970603     C                   PARM                    DSMSKE
073700970603     C                   PARM                    DSMSER
073800970603     C*
073900970603     C* IMPOSTA LA DS -TASTI PREMUTI-
074000970603     C                   CALL      'XKEY'
074100970603     C                   PARM                    DSKEY
074200970603     C*---
074300970603     C* VARIABILI NON RIFERITE AL DATA BASE
074400970603     C*---
074500970603     C     *LIKE         DEFINE    FUDFUN        FUNKEY
074600970603     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
074700970603     C     *LIKE         DEFINE    D00OP1        DEPOP1
074800970603     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
074900970603     C     *LIKE         DEFINE    D00OP1        WOP1
075000970603     C     *LIKE         DEFINE    D00NAR        DNAR
075100970603     C     *LIKE         DEFINE    D00CAP        DCAP
075200970603     C     *LIKE         DEFINE    D00LOC        DLOC
075300970603     C*---
075400970603     C* VARIABILI DI CONTROLLO
075500970603     C*---
075600970603     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
075700970603     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
075800970603     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
075900970603     C                   MOVEL     '0'           ERRL00            1            *ERRORE
076000970603     C*
076100970603     C* IMPOSTA LA DS -INVIO MESSAGGI-
076200970603     C                   MOVEL     'TISI36R'     DSMSPG
076300970603     C                   MOVEL     'TISI36R'     MSGQUE
076400970603     C                   MOVEL     *BLANKS       MSGKEY
076500970603     C                   MOVEL     *BLANKS       DSMSMS
076600970603     C*---
076700970603     C* VARIABILI RIFERITE AL DATA BASE
076800970603     C*---
076900970603     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
077000970603     C     *LIKE         DEFINE    TBLCOD        KTBCOD
077100970603     C     *LIKE         DEFINE    TBLKEY        KTBKEY
077200970603     C     *LIKE         DEFINE    CPSVER        KCSVER                         *AZCPS00F
077300970603     C     *LIKE         DEFINE    CPSNAR        KCSNAR
077400970603     C     *LIKE         DEFINE    CPSCAP        KCSCAP
077500970603     C     *LIKE         DEFINE    CPSLOC        KCSLOC
077600970603     C     *LIKE         DEFINE    CPSSIN        KCSSIN
077700970603     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
077800970603     C     *LIKE         DEFINE    CPLNAR        KCLNAR
077900970603     C     *LIKE         DEFINE    CPLCAP        KCLCAP
078000970603     C     *LIKE         DEFINE    CPLLOC        KCLLOC
078100970603     C*---
078200970603     C* CHIAVI DI LETTURA
078300970603     C*---
078400970603     C* LETTURA TABEL00F - COMPLETA
078500970603     C     KEYTAB        KLIST
078600970603     C                   KFLD                    KTBKUT                         *CODICE UTENTE
078700970603     C                   KFLD                    KTBCOD                         *TABELLA
078800970603     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
078900970603     C* LETTURA AZCPS01L - COMPLETA
079000970603     C     KEYCPS        KLIST
079100970603     C                   KFLD                    KCSVER                         *VERSIONE
079200970603     C                   KFLD                    KCSNAR                         *NAZIONE
079300970603     C                   KFLD                    KCSCAP                         *CAP
079400970603     C                   KFLD                    KCSLOC                         *LOCALITA'
079500970603     C                   KFLD                    KCSSIN                         *SINONIMO LOCALITA'
079600060308     C* LETTURA AZCPS04L - COMPLETA
079700060308     C     KEYCPS_4      KLIST
079800060308     C                   KFLD                    KCSVER                         *VERSIONE
079900060308     C                   KFLD                    KCSNAR                         *NAZIONE
080000060308     C                   KFLD                    KCSCAP                         *CAP
080100060308     C                   KFLD                    KCSSIN                         *SINONIMO LOCALITA'
080200970603     C* LETTURA AZCPL01L - COMPLETA
080300970603     C     KEYCPL        KLIST
080400970603     C                   KFLD                    KCLVER                         *VERSIONE
080500970603     C                   KFLD                    KCLNAR                         *NAZIONE
080600970603     C                   KFLD                    KCLCAP                         *CAP
080700970603     C                   KFLD                    KCLLOC                         *LOCALITA'
080800970603     C*
080900970603     C* DEPOSITA LE OPZIONI DI ENTRATA
081000970603     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
081100970603     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
081200970603     C*
081300970603     C* IMPOSTA LE FUNZIONI DI INGRESSO
081400970603     C                   EXSR      IMPFUN
081500970603     C*
081600970603     C                   ENDSR
081700970603     C*------------------------------------------------------------------------*
081800970603     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
081900970603     C*------------------------------------------------------------------------*
082000970603     C     FUNGES        BEGSR
082100970603     C*
082200970603     C* PULISCO CAMPI DI LAVORO
082300970603     C                   Z-ADD     0             W                 5 0
082400970603     C*
082500970603     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
082600970603     C* E CARICO TABELLA FUNZIONI ATTIVE
082700970603     C     1             DO        24            I
082800970603     C                   MOVEL     FUD(I)        FUDRIG
082900970603     C*
083000970603     C* PULIZIA FUNZIONI PRECEDENTI
083100970603     C                   MOVEL     *BLANKS       FUV(I)
083200970603     C*
083300970603     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
083400970603     C* (SALTA F24)
083500970603     C     FUDATT        IFEQ      'S'
083600970603     C     FUDFUN        ANDNE     '24'
083700970603     C                   ADD       1             W
083800970603     C                   MOVEL     FUDDES        FUV(W)
083900970603     C                   END
084000970603     C                   END                                                    DO WHILE
084100970603     C*
084200970603     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
084300970603     C* ALTRIMENTI DISATTIVO F24
084400970603     C     1             DO        24            I
084500970603     C                   MOVEL     FUD(I)        FUDRIG
084600970603     C*
084700970603     C     FUDFUN        IFEQ      '24'
084800970603     C     W             IFGT      6
084900970603     C                   MOVEL     'S'           FUDATT
085000970603     C                   ELSE
085100970603     C                   MOVEL     *BLANKS       FUDATT
085200970603     C                   END                                                    W GT 8
085300970603     C                   MOVEL     FUDRIG        FUD(I)
085400970603     C                   END
085500970603     C                   END                                                    DO WHILE
085600970603     C*
085700970603     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
085800970603     C     W             DIV       6             I
085900970603     C                   MVR                     Z
086000970603     C*
086100970603     C     Z             IFNE      0
086200970603     C     I             ADD       1             Z
086300970603     C                   ELSE
086400970603     C                   Z-ADD     I             Z                 5 0
086500970603     C                   END
086600970603     C*
086700970603     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
086800970603     C                   ADD       1             FUNGRU
086900970603     C     FUNGRU        IFGT      Z
087000970603     C     FUNGRU        ORLT      1
087100970603     C                   Z-ADD     1             FUNGRU
087200970603     C                   END
087300970603     C*
087400970603     C* PRIMO GRUPPO DI 6 FUNZIONI
087500970603     C     FUNGRU        IFEQ      1
087600970603     C                   MOVEL     FUV(1)        FUN001
087700970603     C                   MOVEL     FUV(2)        FUN002
087800970603     C                   MOVEL     FUV(3)        FUN003
087900970603     C                   MOVEL     FUV(4)        FUN004
088000970603     C                   MOVEL     FUV(5)        FUN005
088100970603     C                   MOVEL     FUV(6)        FUN006
088200970603     C                   END
088300970603     C*
088400970603     C* SECONDO GRUPPO DI 6 FUNZIONI
088500970603     C     FUNGRU        IFEQ      2
088600970603     C                   MOVEL     FUV(7)        FUN001
088700970603     C                   MOVEL     FUV(8)        FUN002
088800970603     C                   MOVEL     FUV(9)        FUN003
088900970603     C                   MOVEL     FUV(10)       FUN004
089000970603     C                   MOVEL     FUV(11)       FUN005
089100970603     C                   MOVEL     FUV(12)       FUN006
089200970603     C                   END
089300970603     C*
089400970603     C* TERZO GRUPPO DI 6 FUNZIONI
089500970603     C     FUNGRU        IFEQ      3
089600970603     C                   MOVEL     FUV(13)       FUN001
089700970603     C                   MOVEL     FUV(14)       FUN002
089800970603     C                   MOVEL     FUV(15)       FUN003
089900970603     C                   MOVEL     FUV(16)       FUN004
090000970603     C                   MOVEL     FUV(17)       FUN005
090100970603     C                   MOVEL     FUV(18)       FUN006
090200970603     C                   END
090300970603     C*
090400970603     C* QUARTO GRUPPO DI 6 FUNZIONI
090500970603     C     FUNGRU        IFEQ      4
090600970603     C                   MOVEL     FUV(19)       FUN001
090700970603     C                   MOVEL     FUV(20)       FUN002
090800970603     C                   MOVEL     FUV(21)       FUN003
090900970603     C                   MOVEL     FUV(22)       FUN004
091000970603     C                   MOVEL     *BLANKS       FUN005
091100970603     C                   MOVEL     *BLANKS       FUN006
091200970603     C                   END
091300970603     C*
091400970603     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
091500970603     C                   MOVEL     *BLANKS       FUN007
091600970603     C                   MOVEL     *BLANKS       FUN008
091700970603     C*
091800970603     C     1             DO        24            I
091900970603     C                   MOVEL     FUD(I)        FUDRIG
092000970603     C     FUDFUN        IFEQ      '24'
092100970603     C     FUDATT        ANDEQ     'S'
092200970603     C                   MOVEL     FUDDES        FUN008
092300970603     C                   END
092400970603     C                   END                                                    DO
092500970603     C*
092600970603     C                   ENDSR
092700970603     C*------------------------------------------------------------------------*
092800970603     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
092900970603     C*------------------------------------------------------------------------*
093000970603     C     FUNCON        BEGSR
093100970603     C*
093200970603     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
093300970603     C                   EXSR      FUNVAR
093400970603     C*
093500970603     C* PULISCO CAMPI DI LAVORO
093600970603     C                   Z-ADD     0             I                 5 0
093700970603     C                   Z-ADD     0             W                 5 0
093800970603     C*
093900970603     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
094000970603     C     FUNKEY        IFNE      *BLANKS
094100970603     C     1             DO        24            I
094200970603     C                   MOVEL     FUD(I)        FUDRIG
094300970603     C*
094400970603     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
094500970603     C     FUNKEY        IFEQ      FUDFUN
094600970603     C     FUDATT        ANDEQ     'S'
094700970603     C                   Z-ADD     I             W
094800970603     C                   END
094900970603     C                   END                                                    DO WHILE
095000970603     C*
095100970603     C* FUNZIONE NON ATTIVA (ERRORE)
095200970603     C     W             IFEQ      0
095300970603     C*
095400970603     C* EMISSIONE MESSAGGIO DI ERRORE
095500970603     C                   MOVEL     MSG(1)        DSMSMS
095600970603     C*
095700970603     C                   END                                                    W=0
095800970603     C                   END                                                    FUNKEY NE *BLANKS
095900970603     C*
096000970603     C* FUNZIONE ATTIVA (PROCEDI ...)
096100970603     C     W             IFNE      0
096200970603     C     FUNKEY        OREQ      *BLANKS
096300970603     C     FUNKEY        IFEQ      *BLANKS
096400970603     C                   EXSR      ENTGES
096500970603     C                   END
096600970603     C     FUNKEY        IFEQ      '03'
096700970603     C                   EXSR      F03GES
096800970603     C                   END
096900970603     C     FUNKEY        IFEQ      '06'
097000970603     C                   EXSR      F06GES
097100970603     C                   END
097200970603     C     FUNKEY        IFEQ      '12'
097300970603     C                   EXSR      F12GES
097400970603     C                   END
097500970603     C     FUNKEY        IFEQ      '24'
097600970603     C                   EXSR      F24GES
097700970603     C                   END
097800970603     C                   END
097900970603     C*
098000970603     C                   ENDSR
098100970603     C*------------------------------------------------------------------------*
098200970603     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
098300970603     C*------------------------------------------------------------------------*
098400970603     C     FUNVAR        BEGSR
098500970603     C*
098600970603     C* INIZIALIZZA VARIABILE -FUNKEY-
098700970603     C*
098800970603     C* FUNKEY=*BLANKS SIGNIFICA ENTER
098900970603     C     KEY           IFEQ      ENTER
099000970603     C                   MOVEL     *BLANKS       FUNKEY
099100970603     C                   END
099200970603     C     KEY           IFEQ      F1
099300970603     C                   MOVEL     '01'          FUNKEY
099400970603     C                   END
099500970603     C     KEY           IFEQ      F2
099600970603     C                   MOVEL     '02'          FUNKEY
099700970603     C                   END
099800970603     C     KEY           IFEQ      F3
099900970603     C                   MOVEL     '03'          FUNKEY
100000970603     C                   END
100100970603     C     KEY           IFEQ      F4
100200970603     C                   MOVEL     '04'          FUNKEY
100300970603     C                   END
100400970603     C     KEY           IFEQ      F5
100500970603     C                   MOVEL     '05'          FUNKEY
100600970603     C                   END
100700970603     C     KEY           IFEQ      F6
100800970603     C                   MOVEL     '06'          FUNKEY
100900970603     C                   END
101000970603     C     KEY           IFEQ      F7
101100970603     C                   MOVEL     '07'          FUNKEY
101200970603     C                   END
101300970603     C     KEY           IFEQ      F8
101400970603     C                   MOVEL     '08'          FUNKEY
101500970603     C                   END
101600970603     C     KEY           IFEQ      F9
101700970603     C                   MOVEL     '09'          FUNKEY
101800970603     C                   END
101900970603     C     KEY           IFEQ      F10
102000970603     C                   MOVEL     '10'          FUNKEY
102100970603     C                   END
102200970603     C     KEY           IFEQ      F11
102300970603     C                   MOVEL     '11'          FUNKEY
102400970603     C                   END
102500970603     C     KEY           IFEQ      F12
102600970603     C                   MOVEL     '12'          FUNKEY
102700970603     C                   END
102800970603     C     KEY           IFEQ      F13
102900970603     C                   MOVEL     '13'          FUNKEY
103000970603     C                   END
103100970603     C     KEY           IFEQ      F14
103200970603     C                   MOVEL     '14'          FUNKEY
103300970603     C                   END
103400970603     C     KEY           IFEQ      F15
103500970603     C                   MOVEL     '15'          FUNKEY
103600970603     C                   END
103700970603     C     KEY           IFEQ      F16
103800970603     C                   MOVEL     '16'          FUNKEY
103900970603     C                   END
104000970603     C     KEY           IFEQ      F17
104100970603     C                   MOVEL     '17'          FUNKEY
104200970603     C                   END
104300970603     C     KEY           IFEQ      F18
104400970603     C                   MOVEL     '18'          FUNKEY
104500970603     C                   END
104600970603     C     KEY           IFEQ      F19
104700970603     C                   MOVEL     '19'          FUNKEY
104800970603     C                   END
104900970603     C     KEY           IFEQ      F20
105000970603     C                   MOVEL     '20'          FUNKEY
105100970603     C                   END
105200970603     C     KEY           IFEQ      F21
105300970603     C                   MOVEL     '21'          FUNKEY
105400970603     C                   END
105500970603     C     KEY           IFEQ      F22
105600970603     C                   MOVEL     '22'          FUNKEY
105700970603     C                   END
105800970603     C     KEY           IFEQ      F23
105900970603     C                   MOVEL     '23'          FUNKEY
106000970603     C                   END
106100970603     C     KEY           IFEQ      F24
106200970603     C                   MOVEL     '24'          FUNKEY
106300970603     C                   END
106400970603     C     KEY           IFEQ      ROLLUP
106500970603     C                   MOVEL     'RU'          FUNKEY
106600970603     C                   END
106700970603     C     KEY           IFEQ      HELP
106800970603     C                   MOVEL     'HP'          FUNKEY
106900970603     C                   END
107000970603     C                   ENDSR
107100970603     C*------------------------------------------------------------------------*
107200970603     C* NOTE COSTRUZIONE SCHIERE
107300970603     C*------------------------------------------------------------------------*
107400970603     C*
107500970603     C* -MSG-MESSAGGI
107600970603     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
107700970603     C* (NON TOCCARE MSG,1 E MSG,2)
107800970603     C*
107900970603     C* -CMD-COMANDI
108000970603     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
108100970603     C*
108200970603     C* -OPD-OPZIONI
108300970603     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
108400970603     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
108500970603     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
108600970603     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
108700970603     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
108800970603     C*  . POSSONO ESSERE IN ORDINE SPARSO
108900970603     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
109000970603     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
109100970603     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
109200970603     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
109300970603     C*
109400970603     C* -FUD-FUNZIONI
109500970603     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
109600970603     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
109700970603     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
109800970603     C*  . POSSONO ESSERE IN ORDINE SPARSO
109900970603     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
110000970603     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
110100970603     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
110200970603     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
110300970603     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
110400970603     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
110500970603     O*------------------------------------------------------------------------*
110600970603** DEC - DECODIFICHE
110700970603    MODIFICA                                                      1
110800970603  ANNULLAMENTO                                                    2
110900970603VISUALIZZAZIONE                                                   3
111000970603   RIPRISTINO                                                     4
111100970603  INSERIMENTO                                                     5
111200970603** MSG - MESSAGGI
111300970603TISI36R- Tasto funzionale non gestito                                          1
111400970603TISI36R- Inserimento effettuato                                                2
111500970603TISI36R- Modifica effettuata                                                   3
111600970603TISI36R- Annullamento effettuato                                               4
111700970603TISI36R- Ripristino effettuato                                                 5
111800970603TISI36R- Richiesta funzione per record inesistente                             6
111900970603TISI36R- Record momentaneamente allocato: riprovare più tardi                  7
112000970603TISI36R- Richiesta funzione per record esistente                               8
112100060308TISI36R- Sinonimo già presente sullo stesso CAP                                9
112200061124TISI36R- Sinonimo già presente come località sullo stesso CAP                 10
112300970603TISI36R- Nazione errata                                                       11
112400970603TISI36R- Cap errato                                                           12
112500970603TISI36R- Località errata                                                      13
112600970603TISI12R- Località inesistente                                                 14
112700970603TISI36R- Sinonimo errato                                                      15
112800970603TISI36R- Il sinonimo deve essere diverso dalla località                       16
112900970603TISI36R- 17
113000970603TISI36R- 18
113100970603TISI36R- 19
113200970603TISI36R- 20
113300970603** FUD - FUNZIONI
113400970603                   -        (S/N)=N                        funz.01¬
113500970603                   -        (S/N)=N                        funz.02¬
113600970603F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
113700970603                   -        (S/N)=N                        funz.04¬
113800970603                   -        (S/N)=N                        funz.05¬
113900970603F06=Aggiornamento  -        (S/N)=S                        funz.06¬
114000970603                   -        (S/N)=N                        funz.07¬
114100970603                   -        (S/N)=N                        funz.08¬
114200970603                   -        (S/N)=N                        funz.09¬
114300970603                   -        (S/N)=N                        funz.10¬
114400970603                   -        (S/N)=N                        funz.11¬
114500970603F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
114600970603                   -        (S/N)=N                        funz.13¬
114700970603                   -        (S/N)=N                        funz.14¬
114800970603                   -        (S/N)=N                        funz.15¬
114900970603                   -        (S/N)=N                        funz.16¬
115000970603                   -        (S/N)=N                        funz.17¬
115100970603                   -        (S/N)=N                        funz.18¬
115200970603                   -        (S/N)=N                        funz.19¬
115300970603                   -        (S/N)=N                        funz.20¬
115400970603                   -        (S/N)=N                        funz.21¬
115500970603                   -        (S/N)=N                        funz.22¬
115600970603                   -        (S/N)=N                        funz.23¬
115700970603F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
