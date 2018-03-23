000100970526     H*------------------------------------------------------------------------*
000200970528     H* CAPPARIO - GESTIONE TEMPI TRAZIONE - ROUTINE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970528     FTISI24D   CF   E             WORKSTN INFDS(DEVDS1)
000900970528     F                                     SFILE(SI24DMS:MSGULT)
001000970528     FAZORG01L  IF   E           K DISK
001100970528     FTABEL00F  IF   E           K DISK
001200970527     F*------------
001300970528     F* TEMPI TRAZIONE
001400970527     F*------------
001500970619     FAZCAS01L  UF A E           K DISK
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
006000970528     D*------------------
006100970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200970528     D*------------------
006300970528     D WLBDA8          DS                  INZ
006400970528     D  G08DAT                 1      8  0
006500970528     D  G08INV                 9     16  0
006600970528     D  G08ERR                17     17
006700970528     D  G08TGI                18     22  0
006800970528     D*------------
006900970528     D* DS TIPI SERVIZIO
007000970528     D*------------
007100970528     D DS5E          E DS
007200970526     D*------------
007300970526     D* DS DI PROCEDURA
007400970526     D*------------
007500970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
007600970526     D*------------
007700970526     D* ARCHITETTURA
007800970526     D*------------
007900970526     D KPJBA         E DS
008000970526     C*------------------------------------------------------------------------*
008100970526     C* MAIN LINES
008200970526     C*------------------------------------------------------------------------*
008300970526     C*
008400970526     C* NON CI SONO ERRORI IN ENTRATA
008500970526IF  1C     ERRL00        IFEQ      '0'
008600970526     C*
008700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009100970526     C*
009200970526     C* EMETTE FORMATO VIDEO
009300970526     C                   EXSR      BUTFMR
009400970526     C*
009500970526     C* GESTIONE TASTI FUNZIONALI
009600970526     C                   EXSR      FUNCON
009700970526E   2C                   ENDDO
009800970526     C*
009900970526     C* CI SONO ERRORI IN ENTRATA
010000970527X   1C                   ELSE
010100970526     C                   MOVEL     '1'           D00ERR
010200970526     C                   MOVEL     DSMSMS        D00MSG
010300970526E   1C                   ENDIF
010400970526     C* FINE PGM
010500970526     C                   SETON                                        LR
010600970526     C*------------------------------------------------------------------------*
010700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010800970526     C*------------------------------------------------------------------------*
010900970526     C     BUTFMR        BEGSR
011000970526     C*
011100970526     C* EMETTE LA TESTATA
011200970528     C                   WRITE     SI24D01
011300970526     C*
011400970526     C* SPEDISCE EVENTUALE MESSAGGIO
011500970526     C                   EXSR      SNDMSG
011600970526     C*
011700970526     C* EMETTE LE FUNZIONI
011800970528     C                   WRITE     SI24D02
011900970526     C*
012000970526     C* EMETTE IL VIDEO
012100970528     C                   EXFMT     SI24D03
012200970526     C*
012300970526     C* REIMPOSTA FLAG DI ERRORE
012400970526     C                   MOVEL     '0'           ERRL00
012500970526     C*
012600970526     C* REIMPOSTA INDICATORI DI ERRORE
012700970526     C                   SETOFF                                         5859
012800970526     C                   SETOFF                                       606162
012900970526     C                   SETOFF                                       636465
013000970526     C                   SETOFF                                       666768
013100970526     C                   SETOFF                                       697071
013200970526     C                   SETOFF                                       727374
013300970526     C                   SETOFF                                       757677
013400970526     C                   SETOFF                                       787980
013500970526     C                   SETOFF                                       818283
013600970526     C                   SETOFF                                       848586
013700970526     C                   SETOFF                                       878889
013800970526     C*
013900970526     C                   ENDSR
014000970526     C*------------------------------------------------------------------------*
014100970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
014200970526     C*------------------------------------------------------------------------*
014300970526     C     SNDMSG        BEGSR
014400970526     C*
014500970526     C* PULISCE IL SFILE MESSAGGI
014600970526     C                   MOVEL     '1'           *IN40
014700970528     C                   WRITE     SI24DMC
014800970526     C                   MOVEL     '0'           *IN40
014900970526     C*
015000970526     C* VISUALIZZA IL MESSAGGIO
015100970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
015200970526     C                   CALL      'XMSG'        XMSGPA
015300970526IF  1C     DSMSER        IFEQ      *ZEROS
015400970526     C                   MOVEL     DSMSKE        MSGKEY
015500970526E   1C                   ENDIF
015600970526     C                   Z-ADD     1             MSGULT
015700970528     C                   WRITE     SI24DMS
015800970528     C                   WRITE     SI24DMC
015900970526     C*
016000970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016100970526     C                   MOVEL     *BLANKS       DSMSMS
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* ENTGES - GESTIONE ENTER -> INVIO
016600970526     C*------------------------------------------------------------------------*
016700970526     C     ENTGES        BEGSR
016800970526     C*
016900970526     C* CONTROLLA I CAMPI IMMESSI
017000970526     C                   EXSR      DECODI
017100970526     C*
017200970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
017300970526IF  1C     DEPOP1        IFEQ      'O05'
017400970526     C                   MOVEL     '1'           FINL00
017500970526E   1C                   ENDIF
017600970526     C*
017700970526     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900970526     C* F03GES - GESTIONE F03 -> FINE
018000970526     C*------------------------------------------------------------------------*
018100970526     C     F03GES        BEGSR
018200970526     C*
018300970526     C* IMPOSTA "PREMUTO F03"
018400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018500970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
018600970526     C*
018700970526     C                   ENDSR
018800970526     C*------------------------------------------------------------------------*
018900970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019000970526     C*------------------------------------------------------------------------*
019100970526     C     F06GES        BEGSR
019200970526     C*
019300970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
019400970526IF  1C     DEPOP1        IFEQ      'F06'
019500970526     C                   EXSR      INSERI
019600970526E   1C                   ENDIF
019700970526IF  1C     DEPOP1        IFEQ      'O02'
019800970526     C                   EXSR      MODIFI
019900970526E   1C                   ENDIF
020000970526IF  1C     DEPOP1        IFEQ      'O04'
020100970526     C                   EXSR      ANNULL
020200970526E   1C                   ENDIF
020300970526IF  1C     DEPOP1        IFEQ      'O07'
020400970526     C                   EXSR      RIPRIS
020500970526E   1C                   ENDIF
020600970526     C*
020700970526     C                   ENDSR
020800970526     C*------------------------------------------------------------------------*
020900970526     C* F12GES - GESTIONE F12 -> RITORNO
021000970526     C*------------------------------------------------------------------------*
021100970526     C     F12GES        BEGSR
021200970526     C*
021300970526     C* IMPOSTA "PREMUTO F12"
021400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021500970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
021600970526     C*
021700970526     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022000970526     C*------------------------------------------------------------------------*
022100970526     C     F24GES        BEGSR
022200970526     C*
022300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022400970526     C*
022500970526     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
022800970526     C*------------------------------------------------------------------------*
022900970526     C     INSERI        BEGSR
023000970526     C*
023100970526     C* CONTROLLA I CAMPI IMMESSI
023200970526     C                   EXSR      DECODI
023300970526     C*
023400970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
023500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023600970619     C                   MOVEL     VIDPTA        CASPTA                         *CHIAVE
023700970619     C                   MOVEL     VIDLNP        CASLNP
023800970619     C                   MOVEL     VIDLNA        CASLNA
023900970619     C                   MOVEL     VIDTSP        CASTSP
024000970619     C                   Z-ADD     DDEVID        CASDDE
024100970619     C                   EXSR      IMPCAS                                       *ALTRI CAMPI
024200970619     C                   Z-ADD     *ZEROS        CASDTR                         *DATA TRASMISSIONE
024300970619     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
024400970619     C                   MOVEL     *BLANKS       CASATB                         *ANNULLAMENTO
024500970619     C                   WRITE     AZCAS000
024600970526     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
024700970602     C*
024800970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
024900970602     C                   EXSR      AZZVID
025000970526E   1C                   ENDIF
025100970526     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400970526     C* MODIFI - OPERAZIONI DI MODIFICA
025500970526     C*------------------------------------------------------------------------*
025600970526     C     MODIFI        BEGSR
025700970526     C*
025800970526     C* CONTROLLA I CAMPI IMMESSI
025900970526     C                   EXSR      DECODI
026000970526     C*
026100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
026200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026300970619     C                   EXSR      LETCAS
026400970526     C*
026500970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026700970619     C                   EXSR      IMPCAS                                       *IMPOSTA I CAMPI
026800970619     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
026900970619     C                   MOVEL     *BLANKS       CASATB                         *ANNULLAMENTO
027000970619     C                   UPDATE    AZCAS000
027100970526     C                   MOVEL     MSG(3)        D00MSG
027200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027300970526E   2C                   ENDIF
027400970526E   1C                   ENDIF
027500970526     C*
027600970526     C                   ENDSR
027700970526     C*------------------------------------------------------------------------*
027800970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
027900970526     C*------------------------------------------------------------------------*
028000970526     C     ANNULL        BEGSR
028100970526     C*
028200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
028300970619     C                   EXSR      LETCAS
028400970527     C*
028500970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028600970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028700970619IF  2C     CASATB        IFEQ      *BLANKS                                      *NO ANNULLATI
028800970619IF  3C     CASDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
028900970619     C                   MOVEL     'A'           CASATB                         *FLAG ANNULLAMENTO
029000970619     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
029100970619     C                   UPDATE    AZCAS000                                     *ANNULLA
029200970526X   3C                   ELSE                                                   *MAI TRASMESSO
029300970619     C                   DELETE    AZCAS000                                     *CANCELLA
029400970526E   3C                   ENDIF
029500970526E   2C                   ENDIF
029600970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
029700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029800970526E   1C                   ENDIF
029900970526     C*
030000970526     C                   ENDSR
030100970526     C*------------------------------------------------------------------------*
030200970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
030300970526     C*------------------------------------------------------------------------*
030400970526     C     RIPRIS        BEGSR
030500970526     C*
030600970526     C* CONTROLLA I CAMPI IMMESSI
030700970526     C                   EXSR      DECODI
030800970526     C*
030900970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
031000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031100970619     C                   EXSR      LETCAS
031200970526     C*
031300970526     C* IMPOSTA IL RECORD DA SCRIVERE
031400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031500970619     C                   MOVEL     *BLANKS       CASATB                         *FLAG ANNULLAMENTO
031600970619     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
031700970619     C                   UPDATE    AZCAS000                                     *ANNULLA
031800970526E   2C                   ENDIF
031900970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
032000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032100970526E   1C                   ENDIF
032200970526     C*
032300970526     C                   ENDSR
032400970526     C*------------------------------------------------------------------------*
032500970619     C* LETCAS - LEGGE IL RECORD PERL'AGGIORNAMENTO
032600970526     C*------------------------------------------------------------------------*
032700970619     C     LETCAS        BEGSR
032800970526     C*
032900980127     C                   MOVEL     D00SPT        KCSPTA
033000980127     C                   MOVEL     D00SLP        KCSLNP
033100980127     C                   MOVEL     D00SLA        KCSLNA
033200980127     C                   MOVEL     D00STS        KCSTSP
033300980127     C                   Z-ADD     D00SDD        KCSDDE
033400970619     C     KEYCAS        CHAIN     AZCAS01L                           9998
033500970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
033600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
033700970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
033800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033900970526E   1C                   ENDIF
034000970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
034100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034200970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
034300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
034400970526E   1C                   ENDIF
034500970526     C*
034600970526     C                   ENDSR
034700970526     C*------------------------------------------------------------------------*
034800970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
034900970526     C*------------------------------------------------------------------------*
035000970526     C     DECODI        BEGSR
035100970528     C*--------------------
035200970528     C* PARTENZA/TRAZIONE/ARRIVO
035300970528     C*--------------------
035400970528IF  1C     ERRL00        IFEQ      '0'
035500970528IF  2C     VIDPTA        IFEQ      'P'
035600970528     C     VIDPTA        OREQ      'T'
035700970528     C     VIDPTA        OREQ      'A'
035800970528X   2C                   ELSE
035900970528     C                   MOVEL     '1'           ERRL00
036000970528     C                   SETON                                        60
036100970528     C                   MOVEL     MSG(11)       DSMSMS
036200970528E   2C                   ENDIF
036300970528E   1C                   ENDIF
036400970528     C*--------------------
036500970528     C* TERMINAL / LINEA DI PARTENZA
036600970528     C*--------------------
036700970528IF  1C     ERRL00        IFEQ      '0'
036800970528     C                   MOVEL     *BLANKS       VDDLNP
036900970528IF  2C     VIDPTA        IFEQ      'P'                                          *OBBLIGATORIA
037000970528     C     VIDPTA        OREQ      'T'
037100970528     C                   MOVEL     VIDLNP        WLIN
037200970528     C                   MOVEL     *BLANKS       WDLIN
037300970528     C                   EXSR      CHKLIN
037400970528     C                   MOVEL     WLIN          VIDLNP
037500970528     C                   MOVEL     WDLIN         VDDLNP
037600970528IF  3C     ERRL00        IFEQ      '1'
037700970528     C                   SETON                                        61
037800970528     C                   MOVEL     MSG(12)       DSMSMS
037900970528E   3C                   ENDIF
038000970528X   2C                   ELSE                                                   *DA LASCIARE VUOTA
038100970528IF  3C     VIDLNP        IFGT      *ZEROS
038200970528     C                   SETON                                        61
038300970528     C                   MOVEL     MSG(18)       DSMSMS
038400970528E   3C                   ENDIF
038500970528E   2C                   ENDIF
038600970528E   1C                   ENDIF
038700970528     C*--------------------
038800970528     C* TERMINAL / LINEA DI ARRIVO
038900970528     C*--------------------
039000970528IF  1C     ERRL00        IFEQ      '0'
039100970528     C                   MOVEL     *BLANKS       VDDLNA
039200970528IF  2C     VIDPTA        IFEQ      'A'                                          *OBBLIGATORIA
039300970528     C     VIDPTA        OREQ      'T'
039400970528     C                   MOVEL     VIDLNA        WLIN
039500970528     C                   MOVEL     *BLANKS       WDLIN
039600970528     C                   EXSR      CHKLIN
039700970528     C                   MOVEL     WLIN          VIDLNA
039800970528     C                   MOVEL     WDLIN         VDDLNA
039900970528IF  3C     ERRL00        IFEQ      '1'
040000970528     C                   SETON                                        62
040100970528     C                   MOVEL     MSG(13)       DSMSMS
040200970528E   3C                   ENDIF
040300970528X   2C                   ELSE                                                   *DA LASCIARE VUOTA
040400970528IF  3C     VIDLNA        IFGT      *ZEROS
040500970528     C                   SETON                                        62
040600970528     C                   MOVEL     MSG(17)       DSMSMS
040700970528E   3C                   ENDIF
040800970528E   2C                   ENDIF
040900970528E   1C                   ENDIF
041000970528     C*--------------------
041100970528     C* TIPO SERVIZIO
041200970528     C*--------------------
041300970528IF  1C     ERRL00        IFEQ      '0'
041400970528     C                   MOVEL     *BLANKS       VDDTSP
041500970528     C                   MOVEL     VIDTSP        WTSP
041600970528     C                   MOVEL     *BLANKS       WDTSP
041700970528     C                   EXSR      CHKTSP
041800970528     C                   MOVEL     WTSP          VIDTSP
041900970528     C                   MOVEL     WDTSP         VDDTSP
042000970528IF  2C     ERRL00        IFEQ      '1'
042100970528     C                   SETON                                        63
042200970528     C                   MOVEL     MSG(16)       DSMSMS
042300970528E   2C                   ENDIF
042400970528E   1C                   ENDIF
042500970528     C*--------------------
042600970528     C* DATA DECORRENZA
042700970528     C*--------------------
042800970528     C     *LIKE         DEFINE    VIDDDE        DDEVID
042900970528IF  1C     ERRL00        IFEQ      '0'
043000970528     C                   Z-ADD     VIDDDE        WDA8
043100970528     C                   Z-ADD     *ZEROS        DA8W
043200970528     C                   EXSR      CHKDA8
043300970528     C                   Z-ADD     WDA8          VIDDDE
043400970528     C                   Z-ADD     DA8W          DDEVID
043500970528IF  2C     ERRL00        IFEQ      '1'
043600970528     C                   SETON                                        64
043700970528     C                   MOVEL     MSG(14)       DSMSMS
043800970528E   2C                   ENDIF
043900970528E   1C                   ENDIF
044000970528     C* INSERIMENTO -> RECORD INESISTENTE
044100970528IF  1C     ERRL00        IFEQ      '0'
044200970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
044300970619     C                   MOVEL     VIDPTA        KCSPTA
044400970619     C                   MOVEL     VIDLNP        KCSLNP
044500970619     C                   MOVEL     VIDLNA        KCSLNA
044600970619     C                   MOVEL     VIDTSP        KCSTSP
044700970619     C                   Z-ADD     DDEVID        KCSDDE
044800970619     C     KEYCAS        CHAIN(N)  AZCAS01L                           99
044900970528IF  3C     *IN99         IFEQ      *OFF
045000970528     C                   MOVEL     '1'           ERRL00
045100970528     C                   MOVEL     MSG(8)        DSMSMS
045200970528     C                   SETON                                        606162
045300970528     C                   SETON                                        6364
045400970528E   3C                   ENDIF
045500970528E   2C                   ENDIF
045600970528E   1C                   ENDIF
045700970528     C* INSERIMENTO/RIPRISTINO -> ULTIMA DATA DECORRENZA
045800970528IF  1C     ERRL00        IFEQ      '0'
045900970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
046000970528     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
046100970619     C                   MOVEL     VIDPTA        KCSPTA
046200970619     C                   MOVEL     VIDLNP        KCSLNP
046300970619     C                   MOVEL     VIDLNA        KCSLNA
046400970619     C                   MOVEL     VIDTSP        KCSTSP
046500970619     C     KE2CAS        SETLL     AZCAS01L
046600970619     C     KE2CAS        READE(N)  AZCAS01L                               99
046700970528DO  3C     *IN99         DOWEQ     *OFF
046800970619IF  4C     CASATB        IFEQ      *BLANKS
046900970619IF  5C     CASDDE        IFGT      DDEVID
047000970528     C                   SETON                                        606162
047100970528     C                   SETON                                        6364
047200970528     C                   MOVEL     MSG(19)       DSMSMS
047300970528     C                   MOVEL     '1'           ERRL00
047400970528     C                   LEAVE
047500970528E   5C                   ENDIF
047600970528E   4C                   ENDIF
047700970619     C     KE2CAS        READE(N)  AZCAS01L                               99
047800970528E   3C                   ENDDO
047900970528E   2C                   ENDIF
048000970528E   1C                   ENDIF
048100970528     C*--------------------
048200970528     C* TEMPO INTERNO
048300970528     C*--------------------
048400970528IF  1C     ERRL00        IFEQ      '0'
048500970528     C                   MOVEL     VIDTIN        WTEM
048600970528     C                   EXSR      CHKTEM
048700970528     C                   MOVEL     WTEM          VIDTIN
048800970528IF  2C     ERRL00        IFEQ      '1'
048900970528     C                   SETON                                        65
049000970528     C                   MOVEL     MSG(15)       DSMSMS
049100970528E   2C                   ENDIF
049200970528E   1C                   ENDIF
049300970528     C*--------------------
049400970528     C* TEMPO CLIENTE
049500970528     C*--------------------
049600970528IF  1C     ERRL00        IFEQ      '0'
049700970528     C                   MOVEL     VIDTCN        WTEM
049800970528     C                   EXSR      CHKTEM
049900970528     C                   MOVEL     WTEM          VIDTCN
050000970528IF  2C     ERRL00        IFEQ      '1'
050100970528     C                   SETON                                        66
050200970528     C                   MOVEL     MSG(15)       DSMSMS
050300970528E   2C                   ENDIF
050400970528E   1C                   ENDIF
050500970528     C*
050600970528     C                   ENDSR
050700970528     C*------------------------------------------------------------------------*
050800970528     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
050900970528     C*------------------------------------------------------------------------*
051000970528     C     CHKLIN        BEGSR
051100970528     C*
051200970528     C     *LIKE         DEFINE    VIDLNP        WLIN
051300970528     C     *LIKE         DEFINE    VDDLNP        WDLIN
051400970528     C*
051500970528     C* RICHIESTO ELENCO
051600970528     C                   SETOFF                                           28
051700970528     C     '?'           SCAN      WLIN                                   28
051800970528IF  1C     *IN28         IFEQ      *ON
051900970528     C                   MOVEL     *BLANKS       §COD1             3
052000970528     C                   MOVEL     *BLANKS       §TIP              1
052100970528     C                   MOVEL     *BLANKS       §DES1            25
052200971203     C                   MOVEL     *BLANKS       §PRA              3
052300971203     C                   CALL      'TNSD24R'
052400970528     C                   PARM                    §COD1
052500970528     C                   PARM                    §TIP
052600970528     C                   PARM                    §DES1
052700971203     C                   PARM                    §PRA
052800970528     C                   MOVEL     §COD1         WLIN
052900970528     C                   MOVEL     §DES1         WDLIN
053000970528E   1C                   ENDIF
053100970528     C*
053200970528     C* SOLO CARATTERI NUMERICI
053300970528     C                   SETOFF                                       28
053400970528     C                   TESTN                   WLIN                 28
053500970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
053600970528     C                   MOVEL     '1'           ERRL00
053700970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
053800970528     C                   MOVE      WLIN          W001              1
053900970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
054000970528     C                   MOVEL     '1'           ERRL00
054100970528X   2C                   ELSE                                                   *NUMERO
054200970528     C*
054300970528     C* CONTROLLA ESISTENZA CODICE
054400970528     C                   MOVEL     WLIN          KORFIL
054500970528     C     KEYORG        CHAIN     AZORG01L                           99
054600970528IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
054700970528     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
054800970528     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
054900970528     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
055000970528X   3C                   ELSE
055100970528     C                   MOVEL     '1'           ERRL00
055200970528E   3C                   ENDIF
055300970528E   2C                   ENDIF
055400970528E   1C                   ENDIF
055500970528     C*
055600970528     C                   ENDSR
055700970528     C*------------------------------------------------------------------------*
055800970528     C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
055900970528     C*------------------------------------------------------------------------*
056000970528     C     CHKTSP        BEGSR
056100970528     C*
056200970528     C     *LIKE         DEFINE    VIDTSP        WTSP
056300970528     C     *LIKE         DEFINE    VDDTSP        WDTSP
056400970528     C*
056500970528     C* RICHIESTO ELENCO
056600970528     C                   SETOFF                                           28
056700970528     C     '?'           SCAN      WTSP                                   28
056800970528IF  1C     *IN28         IFEQ      *ON
056900020906     C                   Z-ADD     1             §KUT              1 0
057000970528     C                   MOVEL     '5E'          §COD              2
057100970528     C                   MOVEL     *BLANKS       §KEY              8
057200970528     C                   MOVEL     *BLANKS       §DES             30
057300970528     C                   CALL      'X§TABER'
057400970528     C                   PARM                    §KUT
057500970528     C                   PARM                    §COD
057600970528     C                   PARM                    §KEY
057700970528     C                   PARM                    §DES
057800970528     C                   MOVEL     §KEY          WTSP
057900970528     C                   MOVEL     §DES          WDTSP
058000970528E   1C                   ENDIF
058100970528     C*
058200970528     C* CONTROLLA ESISTENZA CODICE
058300020906     C                   MOVEL     1             KTBKUT
058400970528     C                   MOVEL     '5E'          KTBCOD
058500970528     C                   MOVEL(P)  WTSP          KTBKEY
058600970528     C     KEYTAB        CHAIN     TABEL00F                           99
058700970528IF  1C     *IN99         IFEQ      *OFF
058800970528     C     TBLFLG        ANDEQ     *BLANKS
058900970528     C                   MOVEL     TBLUNI        DS5E
059000970528     C                   MOVEL     §5EDES        WDTSP
059100970528X   1C                   ELSE
059200970528     C                   MOVEL     '1'           ERRL00
059300970528E   1C                   ENDIF
059400970528     C*
059500970528     C                   ENDSR
059600970528     C*------------------------------------------------------------------------*
059700970528     C* CHKDA8 - CONTROLLA LA DATA (8)
059800970528     C*------------------------------------------------------------------------*
059900970528     C     CHKDA8        BEGSR
060000970528     C*
060100970528     C     *LIKE         DEFINE    VIDDDE        WDA8
060200970528     C     *LIKE         DEFINE    VIDDDE        DA8W
060300970528     C*
060400970528     C                   Z-ADD     WDA8          G08DAT
060500970528     C                   Z-ADD     *ZEROS        G08INV
060600970528     C                   MOVEL     '0'           G08ERR
060700970528     C                   CALL      'XSRDA8'
060800970528     C                   PARM                    WLBDA8
060900970528     C                   Z-ADD     G08DAT        WDA8
061000970528     C                   Z-ADD     G08INV        DA8W
061100970528IF  1C     G08ERR        IFEQ      '1'
061200970528     C                   MOVEL     '1'           ERRL00
061300970528E   1C                   ENDIF
061400970528     C*
061500970528     C                   ENDSR
061600970528     C*------------------------------------------------------------------------*
061700970528     C* CHKTEM - CONTROLLA IL TEMPO
061800970528     C*------------------------------------------------------------------------*
061900970528     C     CHKTEM        BEGSR
062000970528     C*
062100970528     C     *LIKE         DEFINE    VIDTIN        WTEM
062200970528     C*
062300970528     C* RICHIESTO ELENCO
062400970528     C                   SETOFF                                           28
062500970528     C     '?'           SCAN      WTEM                                   28
062600970528IF  1C     *IN28         IFEQ      *ON
062700020906     C                   Z-ADD     1             §KUT              1 0
062800970528     C                   MOVEL     '1W'          §COD              2
062900970528     C                   MOVEL     *BLANKS       §KEY              8
063000970528     C                   MOVEL     *BLANKS       §DES             30
063100970528     C                   CALL      'X§TABER'
063200970528     C                   PARM                    §KUT
063300970528     C                   PARM                    §COD
063400970528     C                   PARM                    §KEY
063500970528     C                   PARM                    §DES
063600970528     C                   MOVEL     §KEY          WTEM
063700970528E   1C                   ENDIF
063800970528     C*
063900970528     C* SOLO CARATTERI NUMERICI
064000970528     C                   SETOFF                                       28
064100970528     C                   TESTN                   WTEM                 28
064200970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
064300970528     C                   MOVEL     '1'           ERRL00
064400970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
064500970528     C                   MOVE      WTEM          W001              1
064600970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
064700970528     C                   MOVEL     '1'           ERRL00
064800970528X   2C                   ELSE                                                   *NUMERO
064900970528     C*
065000970528     C* CONTROLLA ESISTENZA CODICE
065100020906     C                   Z-ADD     1             KTBKUT
065200970528     C                   MOVEL     '1W'          KTBCOD
065300970528     C                   MOVEL(P)  WTEM          KTBKEY
065400970528     C     KEYTAB        CHAIN     TABEL00F                           99
065500970528IF  3C     *IN99         IFEQ      *ON                                          *NON TROVATA
065600970528     C     TBLFLG        ORNE      *BLANKS                                      *ANNULLATA
065700970528     C                   MOVEL     '1'           ERRL00
065800970528E   3C                   ENDIF
065900970528E   2C                   ENDIF
066000970528E   1C                   ENDIF
066100970528     C*
066200970528     C                   ENDSR
066300970526     C*------------------------------------------------------------------------*
066400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
066500970526     C*------------------------------------------------------------------------*
066600970526     C     IREVID        BEGSR
066700970526     C* CAMPI
066800970619     C                   MOVEL     CASPTA        VIDPTA
066900970619     C                   MOVEL     CASLNP        VIDLNP
067000970619     C                   MOVEL     CASLNA        VIDLNA
067100970619     C                   MOVEL     CASTSP        VIDTSP
067200970619     C                   Z-ADD     CASDDE        G08INV
067300970528     C                   Z-ADD     *ZEROS        G08DAT
067400970528     C                   MOVEL     '3'           G08ERR
067500970528     C                   CALL      'XSRDA8'
067600970528     C                   PARM                    WLBDA8
067700970528     C                   Z-ADD     G08DAT        VIDDDE
067800970619     C                   MOVEL     CASTIN        VIDTIN
067900970619     C                   MOVEL     CASTCN        VIDTCN
068000970527     C*
068100970526     C                   ENDSR
068200970526     C*------------------------------------------------------------------------*
068300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
068400970526     C*------------------------------------------------------------------------*
068500970526     C     AZZVID        BEGSR
068600970526     C* CAMPI
068700970528     C                   MOVEL     *BLANKS       VIDPTA
068800970528     C                   MOVEL     *ZEROS        VIDLNP
068900970528     C                   MOVEL     *ZEROS        VIDLNA
069000970528     C                   MOVEL     *BLANKS       VIDTSP
069100970528     C                   Z-ADD     *ZEROS        VIDDDE
069200970528     C                   MOVEL     *ZEROS        VIDTIN
069300970528     C                   MOVEL     *ZEROS        VIDTCN
069400970526     C* DECODIFICHE
069500970528     C                   MOVEL     *BLANKS       VDDLNP
069600970528     C                   MOVEL     *BLANKS       VDDLNA
069700970528     C                   MOVEL     *BLANKS       VDDTSP
069800970526     C*
069900970526     C                   ENDSR
070000970526     C*------------------------------------------------------------------------*
070100970619     C* IMPCAS - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
070200970526     C*------------------------------------------------------------------------*
070300970619     C     IMPCAS        BEGSR
070400970526     C*
070500970619     C                   MOVEL     VIDTIN        CASTIN
070600970619     C                   MOVEL     VIDTCN        CASTCN
070700970526     C*
070800970526     C                   ENDSR
070900970526     C*------------------------------------------------------------------------*
071000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
071100970526     C*------------------------------------------------------------------------*
071200970526     C     IMPFUN        BEGSR
071300970526     C*
071400970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
071500970526IF  1C     DEPOP1        IFEQ      'O05'
071600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
071700970526     C                   MOVEL     'N'           FUDATT
071800970526     C                   MOVEL     FUDRIG        FUD(6)
071900970526E   1C                   ENDIF
072000970526     C*
072100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
072200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
072300970526     C                   EXSR      FUNGES
072400970526     C*
072500970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
072600970526     C*---
072700970526     C* 90 -> PROTEZIONE CHIAVE
072800970526     C* 91 -> PROTEZIONE TOTALE
072900970526     C*---
073000970528IF  1C     DEPOP0        IFEQ      'Z02'                                        G E S T I O N E
073100970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
073200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
073300970526     C                   SETOFF                                       91
073400970526E   2C                   ENDIF
073500970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
073600970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
073700970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
073800970526E   2C                   ENDIF
073900970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
074000970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
074100970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
074200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
074300970526     C                   SETON                                        91
074400970526E   2C                   ENDIF
074500970526X   1C                   ELSE                                                   *I N T E R R O G A
074600970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
074700970526     C                   SETON                                        91
074800970526E   1C                   ENDIF
074900970526     C*
075000970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
075100970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
075200970619     C                   MOVEL     D00SPT        KCSPTA
075300970619     C                   Z-ADD     D00SLP        KCSLNP
075400970619     C                   Z-ADD     D00SLA        KCSLNA
075500970619     C                   MOVEL     D00STS        KCSTSP
075600970619     C                   Z-ADD     D00SDD        KCSDDE
075700970619     C     KEYCAS        CHAIN     AZCAS01L                           9998
075800970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
075900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
076000970526     C                   MOVEL     MSG(6)        DSMSMS
076100970526X   2C                   ELSE
076200980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
076300980128IF  3C     DEPOP1        IFEQ      'O05'
076400980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
076500980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
076600980128E   3C                   ENDIF
076700980128E   2C                   ENDIF
076800970526X   1C                   ELSE                                                   *INSERIMENTO
076900970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
077000970526E   1C                   ENDIF
077100970526     C*
077200970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
077300970526IF  1C     DEPOP1        IFEQ      'O02'
077400970526     C                   MOVEL     DEC(1)        VIDFUN
077500970526E   1C                   ENDIF
077600970526IF  1C     DEPOP1        IFEQ      'O04'
077700970526     C                   MOVEL     DEC(2)        VIDFUN
077800970526E   1C                   ENDIF
077900970526IF  1C     DEPOP1        IFEQ      'O05'
078000970526     C                   MOVEL     DEC(3)        VIDFUN
078100970526E   1C                   ENDIF
078200970526IF  1C     DEPOP1        IFEQ      'O07'
078300970526     C                   MOVEL     DEC(4)        VIDFUN
078400970526E   1C                   ENDIF
078500970526IF  1C     DEPOP1        IFEQ      'F06'
078600970526     C                   MOVEL     DEC(5)        VIDFUN
078700970526E   1C                   ENDIF
078800970526     C*
078900970526     C                   ENDSR
079000970526     C*------------------------------------------------------------------------*
079100970526     C* *INZSR - OPERAZIONI INIZIALI
079200970526     C*------------------------------------------------------------------------*
079300970526     C     *INZSR        BEGSR
079400970526     C*---
079500970526     C* RICEVIMENTO PARAMETRI
079600970526     C*---
079700970526     C     *ENTRY        PLIST
079800970612     C                   PARM                    KPJBA
079900970612     C                   PARM                    DSSI00
080000970526     C*
080100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
080200970526     C     XMSGPA        PLIST
080300970526     C                   PARM                    DSMSMS
080400970526     C                   PARM                    DSMSPG
080500970526     C                   PARM                    DSMSKE
080600970526     C                   PARM                    DSMSER
080700970526     C*
080800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
080900970526     C                   CALL      'XKEY'
081000970526     C                   PARM                    DSKEY
081100970526     C*---
081200970526     C* VARIABILI NON RIFERITE AL DATA BASE
081300970526     C*---
081400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
081500970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
081600970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
081700970526     C     *LIKE         DEFINE    D00OP0        WOP0
081800970526     C     *LIKE         DEFINE    D00OP1        WOP1
081900970526     C*---
082000970526     C* VARIABILI DI CONTROLLO
082100970526     C*---
082200970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
082300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
082400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
082500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
082600970526     C*
082700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
082800970528     C                   MOVEL     'TISI24R'     DSMSPG
082900970528     C                   MOVEL     'TISI24R'     MSGQUE
083000970526     C                   MOVEL     *BLANKS       MSGKEY
083100970526     C                   MOVEL     *BLANKS       DSMSMS
083200970526     C*---
083300970526     C* VARIABILI RIFERITE AL DATA BASE
083400970526     C*---
083500970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
083600970528     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
083700970528     C     *LIKE         DEFINE    TBLCOD        KTBCOD
083800970528     C     *LIKE         DEFINE    TBLKEY        KTBKEY
083900970619     C     *LIKE         DEFINE    CASPTA        KCSPTA                         *AZCAS00F
084000970619     C     *LIKE         DEFINE    CASLNP        KCSLNP
084100970619     C     *LIKE         DEFINE    CASLNA        KCSLNA
084200970619     C     *LIKE         DEFINE    CASTSP        KCSTSP
084300970619     C     *LIKE         DEFINE    CASDDE        KCSDDE
084400970526     C*---
084500970526     C* CHIAVI DI LETTURA
084600970526     C*---
084700970528     C* LETTURA AZORG01L - COMPLETA
084800970528     C     KEYORG        KLIST
084900970528     C                   KFLD                    KORFIL                         *FILIALE
085000970528     C* LETTURA TABEL00F - COMPLETA
085100970528     C     KEYTAB        KLIST
085200970528     C                   KFLD                    KTBKUT                         *UTENTE
085300970528     C                   KFLD                    KTBCOD                         *TABELLA
085400970528     C                   KFLD                    KTBKEY                         *CHIAVE
085500970619     C* LETTURA AZCAS01L - COMPLETA
085600970619     C     KEYCAS        KLIST
085700970619     C                   KFLD                    KCSPTA                         *PART/TRAZ/ARR
085800970619     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
085900970619     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
086000970619     C                   KFLD                    KCSTSP                         *TIPO SERVIZIO
086100970619     C                   KFLD                    KCSDDE                         *DECORRENZA
086200970619     C* LETTURA AZCAS01L - PARZIALE
086300970619     C     KE2CAS        KLIST
086400970619     C                   KFLD                    KCSPTA                         *PART/TRAS/ARR
086500970619     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
086600970619     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
086700970619     C                   KFLD                    KCSTSP                         *TIPO SERVIZIO
086800970619     C* LETTURA AZCAS01L - PARZIALE
086900970619     C     KE3CAS        KLIST
087000970619     C                   KFLD                    KCSPTA                         *PART/TRAS/ARR
087100970619     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
087200970619     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
087300970528     C*
087400970528     C* CALCOLA LA DATA CORRENTE
087500970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
087600970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
087700970528     C                   Z-ADD     WN8           G08DAT
087800970528     C                   Z-ADD     *ZEROS        G08INV
087900970528     C                   MOVEL     '0'           G08ERR
088000970528     C                   CALL      'XSRDA8'
088100970528     C                   PARM                    WLBDA8
088200970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
088300970526     C*
088400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
088500970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
088600970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
088700970526     C*
088800970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
088900970526     C                   EXSR      IMPFUN
089000970526     C*
089100970526     C                   ENDSR
089200970526     C*------------------------------------------------------------------------*
089300970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
089400970526     C*------------------------------------------------------------------------*
089500970526     C     FUNGES        BEGSR
089600970526     C*
089700970526     C* PULISCO CAMPI DI LAVORO
089800970526     C                   Z-ADD     0             W                 5 0
089900970526     C*
090000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
090100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
090200970526     C     1             DO        24            I
090300970526     C                   MOVEL     FUD(I)        FUDRIG
090400970526     C*
090500970526     C* PULIZIA FUNZIONI PRECEDENTI
090600970526     C                   MOVEL     *BLANKS       FUV(I)
090700970526     C*
090800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
090900970526     C* (SALTA F24)
091000970526     C     FUDATT        IFEQ      'S'
091100970526     C     FUDFUN        ANDNE     '24'
091200970526     C                   ADD       1             W
091300970526     C                   MOVEL     FUDDES        FUV(W)
091400970526     C                   END
091500970526     C                   END                                                    DO WHILE
091600970526     C*
091700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
091800970526     C* ALTRIMENTI DISATTIVO F24
091900970526     C     1             DO        24            I
092000970526     C                   MOVEL     FUD(I)        FUDRIG
092100970526     C*
092200970526     C     FUDFUN        IFEQ      '24'
092300970526     C     W             IFGT      6
092400970526     C                   MOVEL     'S'           FUDATT
092500970526     C                   ELSE
092600970526     C                   MOVEL     *BLANKS       FUDATT
092700970526     C                   END                                                    W GT 8
092800970526     C                   MOVEL     FUDRIG        FUD(I)
092900970526     C                   END
093000970526     C                   END                                                    DO WHILE
093100970526     C*
093200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
093300970526     C     W             DIV       6             I
093400970526     C                   MVR                     Z
093500970526     C*
093600970526     C     Z             IFNE      0
093700970526     C     I             ADD       1             Z
093800970526     C                   ELSE
093900970526     C                   Z-ADD     I             Z                 5 0
094000970526     C                   END
094100970526     C*
094200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
094300970526     C                   ADD       1             FUNGRU
094400970526     C     FUNGRU        IFGT      Z
094500970526     C     FUNGRU        ORLT      1
094600970526     C                   Z-ADD     1             FUNGRU
094700970526     C                   END
094800970526     C*
094900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
095000970526     C     FUNGRU        IFEQ      1
095100970526     C                   MOVEL     FUV(1)        FUN001
095200970526     C                   MOVEL     FUV(2)        FUN002
095300970526     C                   MOVEL     FUV(3)        FUN003
095400970526     C                   MOVEL     FUV(4)        FUN004
095500970526     C                   MOVEL     FUV(5)        FUN005
095600970526     C                   MOVEL     FUV(6)        FUN006
095700970526     C                   END
095800970526     C*
095900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
096000970526     C     FUNGRU        IFEQ      2
096100970526     C                   MOVEL     FUV(7)        FUN001
096200970526     C                   MOVEL     FUV(8)        FUN002
096300970526     C                   MOVEL     FUV(9)        FUN003
096400970526     C                   MOVEL     FUV(10)       FUN004
096500970526     C                   MOVEL     FUV(11)       FUN005
096600970526     C                   MOVEL     FUV(12)       FUN006
096700970526     C                   END
096800970526     C*
096900970526     C* TERZO GRUPPO DI 6 FUNZIONI
097000970526     C     FUNGRU        IFEQ      3
097100970526     C                   MOVEL     FUV(13)       FUN001
097200970526     C                   MOVEL     FUV(14)       FUN002
097300970526     C                   MOVEL     FUV(15)       FUN003
097400970526     C                   MOVEL     FUV(16)       FUN004
097500970526     C                   MOVEL     FUV(17)       FUN005
097600970526     C                   MOVEL     FUV(18)       FUN006
097700970526     C                   END
097800970526     C*
097900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
098000970526     C     FUNGRU        IFEQ      4
098100970526     C                   MOVEL     FUV(19)       FUN001
098200970526     C                   MOVEL     FUV(20)       FUN002
098300970526     C                   MOVEL     FUV(21)       FUN003
098400970526     C                   MOVEL     FUV(22)       FUN004
098500970526     C                   MOVEL     *BLANKS       FUN005
098600970526     C                   MOVEL     *BLANKS       FUN006
098700970526     C                   END
098800970526     C*
098900970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
099000970526     C                   MOVEL     *BLANKS       FUN007
099100970526     C                   MOVEL     *BLANKS       FUN008
099200970526     C*
099300970526     C     1             DO        24            I
099400970526     C                   MOVEL     FUD(I)        FUDRIG
099500970526     C     FUDFUN        IFEQ      '24'
099600970526     C     FUDATT        ANDEQ     'S'
099700970526     C                   MOVEL     FUDDES        FUN008
099800970526     C                   END
099900970526     C                   END                                                    DO
100000970526     C*
100100970526     C                   ENDSR
100200970526     C*------------------------------------------------------------------------*
100300970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
100400970526     C*------------------------------------------------------------------------*
100500970526     C     FUNCON        BEGSR
100600970526     C*
100700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
100800970526     C                   EXSR      FUNVAR
100900970526     C*
101000970526     C* PULISCO CAMPI DI LAVORO
101100970526     C                   Z-ADD     0             I                 5 0
101200970526     C                   Z-ADD     0             W                 5 0
101300970526     C*
101400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
101500970526     C     FUNKEY        IFNE      *BLANKS
101600970526     C     1             DO        24            I
101700970526     C                   MOVEL     FUD(I)        FUDRIG
101800970526     C*
101900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
102000970526     C     FUNKEY        IFEQ      FUDFUN
102100970526     C     FUDATT        ANDEQ     'S'
102200970526     C                   Z-ADD     I             W
102300970526     C                   END
102400970526     C                   END                                                    DO WHILE
102500970526     C*
102600970526     C* FUNZIONE NON ATTIVA (ERRORE)
102700970526     C     W             IFEQ      0
102800970526     C*
102900970526     C* EMISSIONE MESSAGGIO DI ERRORE
103000970526     C                   MOVEL     MSG(1)        DSMSMS
103100970526     C*
103200970526     C                   END                                                    W=0
103300970526     C                   END                                                    FUNKEY NE *BLANKS
103400970526     C*
103500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
103600970526     C     W             IFNE      0
103700970526     C     FUNKEY        OREQ      *BLANKS
103800970526     C     FUNKEY        IFEQ      *BLANKS
103900970526     C                   EXSR      ENTGES
104000970526     C                   END
104100970526     C     FUNKEY        IFEQ      '03'
104200970526     C                   EXSR      F03GES
104300970526     C                   END
104400970526     C     FUNKEY        IFEQ      '06'
104500970526     C                   EXSR      F06GES
104600970526     C                   END
104700970526     C     FUNKEY        IFEQ      '12'
104800970526     C                   EXSR      F12GES
104900970526     C                   END
105000970526     C     FUNKEY        IFEQ      '24'
105100970526     C                   EXSR      F24GES
105200970526     C                   END
105300970526     C                   END
105400970526     C*
105500970526     C                   ENDSR
105600970526     C*------------------------------------------------------------------------*
105700970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
105800970526     C*------------------------------------------------------------------------*
105900970526     C     FUNVAR        BEGSR
106000970526     C*
106100970526     C* INIZIALIZZA VARIABILE -FUNKEY-
106200970526     C*
106300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
106400970526     C     KEY           IFEQ      ENTER
106500970526     C                   MOVEL     *BLANKS       FUNKEY
106600970526     C                   END
106700970526     C     KEY           IFEQ      F1
106800970526     C                   MOVEL     '01'          FUNKEY
106900970526     C                   END
107000970526     C     KEY           IFEQ      F2
107100970526     C                   MOVEL     '02'          FUNKEY
107200970526     C                   END
107300970526     C     KEY           IFEQ      F3
107400970526     C                   MOVEL     '03'          FUNKEY
107500970526     C                   END
107600970526     C     KEY           IFEQ      F4
107700970526     C                   MOVEL     '04'          FUNKEY
107800970526     C                   END
107900970526     C     KEY           IFEQ      F5
108000970526     C                   MOVEL     '05'          FUNKEY
108100970526     C                   END
108200970526     C     KEY           IFEQ      F6
108300970526     C                   MOVEL     '06'          FUNKEY
108400970526     C                   END
108500970526     C     KEY           IFEQ      F7
108600970526     C                   MOVEL     '07'          FUNKEY
108700970526     C                   END
108800970526     C     KEY           IFEQ      F8
108900970526     C                   MOVEL     '08'          FUNKEY
109000970526     C                   END
109100970526     C     KEY           IFEQ      F9
109200970526     C                   MOVEL     '09'          FUNKEY
109300970526     C                   END
109400970526     C     KEY           IFEQ      F10
109500970526     C                   MOVEL     '10'          FUNKEY
109600970526     C                   END
109700970526     C     KEY           IFEQ      F11
109800970526     C                   MOVEL     '11'          FUNKEY
109900970526     C                   END
110000970526     C     KEY           IFEQ      F12
110100970526     C                   MOVEL     '12'          FUNKEY
110200970526     C                   END
110300970526     C     KEY           IFEQ      F13
110400970526     C                   MOVEL     '13'          FUNKEY
110500970526     C                   END
110600970526     C     KEY           IFEQ      F14
110700970526     C                   MOVEL     '14'          FUNKEY
110800970526     C                   END
110900970526     C     KEY           IFEQ      F15
111000970526     C                   MOVEL     '15'          FUNKEY
111100970526     C                   END
111200970526     C     KEY           IFEQ      F16
111300970526     C                   MOVEL     '16'          FUNKEY
111400970526     C                   END
111500970526     C     KEY           IFEQ      F17
111600970526     C                   MOVEL     '17'          FUNKEY
111700970526     C                   END
111800970526     C     KEY           IFEQ      F18
111900970526     C                   MOVEL     '18'          FUNKEY
112000970526     C                   END
112100970526     C     KEY           IFEQ      F19
112200970526     C                   MOVEL     '19'          FUNKEY
112300970526     C                   END
112400970526     C     KEY           IFEQ      F20
112500970526     C                   MOVEL     '20'          FUNKEY
112600970526     C                   END
112700970526     C     KEY           IFEQ      F21
112800970526     C                   MOVEL     '21'          FUNKEY
112900970526     C                   END
113000970526     C     KEY           IFEQ      F22
113100970526     C                   MOVEL     '22'          FUNKEY
113200970526     C                   END
113300970526     C     KEY           IFEQ      F23
113400970526     C                   MOVEL     '23'          FUNKEY
113500970526     C                   END
113600970526     C     KEY           IFEQ      F24
113700970526     C                   MOVEL     '24'          FUNKEY
113800970526     C                   END
113900970526     C     KEY           IFEQ      ROLLUP
114000970526     C                   MOVEL     'RU'          FUNKEY
114100970526     C                   END
114200970526     C     KEY           IFEQ      HELP
114300970526     C                   MOVEL     'HP'          FUNKEY
114400970526     C                   END
114500970526     C                   ENDSR
114600970526     C*------------------------------------------------------------------------*
114700970526     C* NOTE COSTRUZIONE SCHIERE
114800970526     C*------------------------------------------------------------------------*
114900970526     C*
115000970526     C* -MSG-MESSAGGI
115100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
115200970526     C* (NON TOCCARE MSG,1 E MSG,2)
115300970526     C*
115400970526     C* -CMD-COMANDI
115500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
115600970526     C*
115700970526     C* -OPD-OPZIONI
115800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
115900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
116000970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
116100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
116200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
116300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
116400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
116500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
116600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
116700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
116800970526     C*
116900970526     C* -FUD-FUNZIONI
117000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
117100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
117200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
117300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
117400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
117500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
117600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
117700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
117800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
117900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
118000970526     O*------------------------------------------------------------------------*
118100970526** DEC - DECODIFICHE
118200970526    MODIFICA                                                      1
118300970526  ANNULLAMENTO                                                    2
118400970526VISUALIZZAZIONE                                                   3
118500970526   RIPRISTINO                                                     4
118600970526  INSERIMENTO                                                     5
118700970528** MSG - MESSAGGI
118800970528TISI24R- Tasto funzionale non gestito                                          1
118900970528TISI24R- Inserimento effettuato                                                2
119000970528TISI24R- Modifica effettuata                                                   3
119100970528TISI24R- Annullamento effettuato                                               4
119200970528TISI24R- Ripristino effettuato                                                 5
119300970528TISI24R- Richiesta funzione per record inesistente                             6
119400970528TISI24R- Record momentaneamente allocato: riprovare più tardi                  7
119500970528TISI24R- Richiesta funzione per record esistente                               8
119600970528TISI24R- 9
119700970528TISI24R- 10
119800970528TISI24R- Immettere P=Partenza, T=Trazione, A=Arrivo                            11
119900970528TISI24R- Terminal / Linea partenza errato                                      12
120000970528TISI24R- Terminal / Linea arrivo errato                                        13
120100970528TISI24R- Data errata                                                           14
120200970528TISI24R- Tempo errato                                                          15
120300970528TISI24R- Tipo servizio errato                                                  16
120400970528TISI24R- Per tempo partenza non inserire la linea di arrivo                    17
120500970528TISI24R- Per tempo arrivo non inserire la linea di partenza                    18
120600970528TISI24R- Esiste già un tempo con data decorrenza superiore                     19
120700970528TISI24R- 20
120800970526** FUD - FUNZIONI
120900970526                   -        (S/N)=N                        funz.01¬
121000970526                   -        (S/N)=N                        funz.02¬
121100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
121200970526                   -        (S/N)=N                        funz.04¬
121300970526                   -        (S/N)=N                        funz.05¬
121400970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
121500970527                   -        (S/N)=N                        funz.07¬
121600970527                   -        (S/N)=N                        funz.08¬
121700970527                   -        (S/N)=N                        funz.09¬
121800970527                   -        (S/N)=N                        funz.10¬
121900970526                   -        (S/N)=N                        funz.11¬
122000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
122100970526                   -        (S/N)=N                        funz.13¬
122200970526                   -        (S/N)=N                        funz.14¬
122300970526                   -        (S/N)=N                        funz.15¬
122400970526                   -        (S/N)=N                        funz.16¬
122500970526                   -        (S/N)=N                        funz.17¬
122600970526                   -        (S/N)=N                        funz.18¬
122700970526                   -        (S/N)=N                        funz.19¬
122800970526                   -        (S/N)=N                        funz.20¬
122900970526                   -        (S/N)=N                        funz.21¬
123000970526                   -        (S/N)=N                        funz.22¬
123100970526                   -        (S/N)=N                        funz.23¬
123200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
