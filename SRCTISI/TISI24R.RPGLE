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
024500170313     c                   if        vidtsp<>' '
024600970619     C                   WRITE     AZCAS000
024700170313     c                   else
024800170313     c                   eval      castsp='E'
024900170313     C                   WRITE     AZCAS000
025000170313     c                   eval      castsp='C'
025100170313     C                   WRITE     AZCAS000
025200170313     c                   eval      castsp='D'
025300170313     C                   WRITE     AZCAS000
025400170313     c                   eval      castsp='H'
025500170313     C                   WRITE     AZCAS000
025600170313     c                   endif
025700970526     C                   MOVEL     MSG(2)        D00MSG                         *MESSAGGIO
025800970602     C*
025900970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
026000970602     C                   EXSR      AZZVID
026100970526E   1C                   ENDIF
026200970526     C*
026300970526     C                   ENDSR
026400970526     C*------------------------------------------------------------------------*
026500970526     C* MODIFI - OPERAZIONI DI MODIFICA
026600970526     C*------------------------------------------------------------------------*
026700970526     C     MODIFI        BEGSR
026800970526     C*
026900970526     C* CONTROLLA I CAMPI IMMESSI
027000970526     C                   EXSR      DECODI
027100970526     C*
027200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
027300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027400970619     C                   EXSR      LETCAS
027500970526     C*
027600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
027700970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027800970619     C                   EXSR      IMPCAS                                       *IMPOSTA I CAMPI
027900970619     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
028000970619     C                   MOVEL     *BLANKS       CASATB                         *ANNULLAMENTO
028100970619     C                   UPDATE    AZCAS000
028200970526     C                   MOVEL     MSG(3)        D00MSG
028300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
028400970526E   2C                   ENDIF
028500970526E   1C                   ENDIF
028600970526     C*
028700970526     C                   ENDSR
028800970526     C*------------------------------------------------------------------------*
028900970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
029000970526     C*------------------------------------------------------------------------*
029100970526     C     ANNULL        BEGSR
029200970526     C*
029300970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
029400970619     C                   EXSR      LETCAS
029500970527     C*
029600970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
029700970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029800970619IF  2C     CASATB        IFEQ      *BLANKS                                      *NO ANNULLATI
029900970619IF  3C     CASDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
030000970619     C                   MOVEL     'A'           CASATB                         *FLAG ANNULLAMENTO
030100970619     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
030200970619     C                   UPDATE    AZCAS000                                     *ANNULLA
030300970526X   3C                   ELSE                                                   *MAI TRASMESSO
030400970619     C                   DELETE    AZCAS000                                     *CANCELLA
030500970526E   3C                   ENDIF
030600970526E   2C                   ENDIF
030700970526     C                   MOVEL     MSG(4)        D00MSG                         *MESSAGGIO
030800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
030900970526E   1C                   ENDIF
031000970526     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
031400970526     C*------------------------------------------------------------------------*
031500970526     C     RIPRIS        BEGSR
031600970526     C*
031700970526     C* CONTROLLA I CAMPI IMMESSI
031800970526     C                   EXSR      DECODI
031900970526     C*
032000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032200970619     C                   EXSR      LETCAS
032300970526     C*
032400970526     C* IMPOSTA IL RECORD DA SCRIVERE
032500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032600970619     C                   MOVEL     *BLANKS       CASATB                         *FLAG ANNULLAMENTO
032700970619     C                   MOVEL     *BLANKS       CASFTR                         *FLAG TRASMISSIONE
032800970619     C                   UPDATE    AZCAS000                                     *ANNULLA
032900970526E   2C                   ENDIF
033000970526     C                   MOVEL     MSG(5)        D00MSG                         *MESSAGGIO
033100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033200970526E   1C                   ENDIF
033300970526     C*
033400970526     C                   ENDSR
033500970526     C*------------------------------------------------------------------------*
033600970619     C* LETCAS - LEGGE IL RECORD PERL'AGGIORNAMENTO
033700970526     C*------------------------------------------------------------------------*
033800970619     C     LETCAS        BEGSR
033900970526     C*
034000980127     C                   MOVEL     D00SPT        KCSPTA
034100980127     C                   MOVEL     D00SLP        KCSLNP
034200980127     C                   MOVEL     D00SLA        KCSLNA
034300980127     C                   MOVEL     D00STS        KCSTSP
034400980127     C                   Z-ADD     D00SDD        KCSDDE
034500970619     C     KEYCAS        CHAIN     AZCAS01L                           9998
034600970526IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
034700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034800970526     C                   MOVEL     MSG(7)        D00MSG                         *MESSAGGIO
034900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035000970526E   1C                   ENDIF
035100970526IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
035200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
035300970526     C                   MOVEL     MSG(6)        D00MSG                         *MESSAGGIO
035400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035500970526E   1C                   ENDIF
035600970526     C*
035700970526     C                   ENDSR
035800970526     C*------------------------------------------------------------------------*
035900970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
036000970526     C*------------------------------------------------------------------------*
036100970526     C     DECODI        BEGSR
036200970528     C*--------------------
036300970528     C* PARTENZA/TRAZIONE/ARRIVO
036400970528     C*--------------------
036500970528IF  1C     ERRL00        IFEQ      '0'
036600970528IF  2C     VIDPTA        IFEQ      'P'
036700970528     C     VIDPTA        OREQ      'T'
036800970528     C     VIDPTA        OREQ      'A'
036900970528X   2C                   ELSE
037000970528     C                   MOVEL     '1'           ERRL00
037100970528     C                   SETON                                        60
037200970528     C                   MOVEL     MSG(11)       DSMSMS
037300970528E   2C                   ENDIF
037400970528E   1C                   ENDIF
037500970528     C*--------------------
037600970528     C* TERMINAL / LINEA DI PARTENZA
037700970528     C*--------------------
037800970528IF  1C     ERRL00        IFEQ      '0'
037900970528     C                   MOVEL     *BLANKS       VDDLNP
038000970528IF  2C     VIDPTA        IFEQ      'P'                                          *OBBLIGATORIA
038100970528     C     VIDPTA        OREQ      'T'
038200970528     C                   MOVEL     VIDLNP        WLIN
038300970528     C                   MOVEL     *BLANKS       WDLIN
038400970528     C                   EXSR      CHKLIN
038500970528     C                   MOVEL     WLIN          VIDLNP
038600970528     C                   MOVEL     WDLIN         VDDLNP
038700970528IF  3C     ERRL00        IFEQ      '1'
038800970528     C                   SETON                                        61
038900970528     C                   MOVEL     MSG(12)       DSMSMS
039000970528E   3C                   ENDIF
039100970528X   2C                   ELSE                                                   *DA LASCIARE VUOTA
039200970528IF  3C     VIDLNP        IFGT      *ZEROS
039300970528     C                   SETON                                        61
039400970528     C                   MOVEL     MSG(18)       DSMSMS
039500970528E   3C                   ENDIF
039600970528E   2C                   ENDIF
039700970528E   1C                   ENDIF
039800970528     C*--------------------
039900970528     C* TERMINAL / LINEA DI ARRIVO
040000970528     C*--------------------
040100970528IF  1C     ERRL00        IFEQ      '0'
040200970528     C                   MOVEL     *BLANKS       VDDLNA
040300970528IF  2C     VIDPTA        IFEQ      'A'                                          *OBBLIGATORIA
040400970528     C     VIDPTA        OREQ      'T'
040500970528     C                   MOVEL     VIDLNA        WLIN
040600970528     C                   MOVEL     *BLANKS       WDLIN
040700970528     C                   EXSR      CHKLIN
040800970528     C                   MOVEL     WLIN          VIDLNA
040900970528     C                   MOVEL     WDLIN         VDDLNA
041000970528IF  3C     ERRL00        IFEQ      '1'
041100970528     C                   SETON                                        62
041200970528     C                   MOVEL     MSG(13)       DSMSMS
041300970528E   3C                   ENDIF
041400970528X   2C                   ELSE                                                   *DA LASCIARE VUOTA
041500970528IF  3C     VIDLNA        IFGT      *ZEROS
041600970528     C                   SETON                                        62
041700970528     C                   MOVEL     MSG(17)       DSMSMS
041800970528E   3C                   ENDIF
041900970528E   2C                   ENDIF
042000970528E   1C                   ENDIF
042100970528     C*--------------------
042200970528     C* TIPO SERVIZIO
042300970528     C*--------------------
042400970528IF  1C     ERRL00        IFEQ      '0'
042500170411     c
042600170411   1ac     vidtsp        ifne      ' '
042700970528     C                   MOVEL     *BLANKS       VDDTSP
042800970528     C                   MOVEL     VIDTSP        WTSP
042900970528     C                   MOVEL     *BLANKS       WDTSP
043000970528     C                   EXSR      CHKTSP
043100970528     C                   MOVEL     WTSP          VIDTSP
043200170411     C                   MOVEL(p)  WDTSP         VDDTSP
043300970528IF  2C     ERRL00        IFEQ      '1'
043400970528     C                   SETON                                        63
043500970528     C                   MOVEL     MSG(16)       DSMSMS
043600970528E   2C                   ENDIF
043700170411
043800170411     c                   else
043900170411     C                   MOVEL     *BLANKS       VDDTSP
044000170411     c                   eval      vddtsp='_=TUTTI I TIPI SERVIZIO'
044100170411E  1aC                   ENDIF
044200170411E   1C                   ENDIF
044300970528     C*--------------------
044400970528     C* DATA DECORRENZA
044500970528     C*--------------------
044600970528     C     *LIKE         DEFINE    VIDDDE        DDEVID
044700970528IF  1C     ERRL00        IFEQ      '0'
044800970528     C                   Z-ADD     VIDDDE        WDA8
044900970528     C                   Z-ADD     *ZEROS        DA8W
045000970528     C                   EXSR      CHKDA8
045100970528     C                   Z-ADD     WDA8          VIDDDE
045200970528     C                   Z-ADD     DA8W          DDEVID
045300970528IF  2C     ERRL00        IFEQ      '1'
045400970528     C                   SETON                                        64
045500970528     C                   MOVEL     MSG(14)       DSMSMS
045600970528E   2C                   ENDIF
045700970528E   1C                   ENDIF
045800970528     C* INSERIMENTO -> RECORD INESISTENTE
045900970528IF  1C     ERRL00        IFEQ      '0'
046000970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
046100970619     C                   MOVEL     VIDPTA        KCSPTA
046200970619     C                   MOVEL     VIDLNP        KCSLNP
046300970619     C                   MOVEL     VIDLNA        KCSLNA
046400970619     C                   MOVEL     VIDTSP        KCSTSP
046500970619     C                   Z-ADD     DDEVID        KCSDDE
046600170411    3c     vidtsp        ifne      ' '
046700970619     C     KEYCAS        CHAIN(N)  AZCAS01L                           99
046800170411IF  4C     *IN99         IFEQ      *OFF
046900970528     C                   MOVEL     '1'           ERRL00
047000970528     C                   MOVEL     MSG(8)        DSMSMS
047100970528     C                   SETON                                        606162
047200970528     C                   SETON                                        6364
047300170411E   4C                   ENDIF
047400170411c
047500170411   x3c                   else
047600170411     c                   z-add     1             xx                3 0
047700170411    4c                   dow       xx<=4 and ERRL00='0'
047800170411    5c                   select
047900170411     c                   when      xx=1
048000170411     C                   MOVEL     'E'           KCSTSP
048100170411     c                   when      xx=2
048200170411     C                   MOVEL     'H'           KCSTSP
048300170411     c                   when      xx=3
048400170411     C                   MOVEL     'C'           KCSTSP
048500170411     c                   when      xx=2
048600170411     C                   MOVEL     'D'           KCSTSP
048700170411    5c                   endsl
048800170411     c
048900170411     C     KEYCAS        CHAIN(N)  AZCAS01L                           99
049000170411IF  5C     *IN99         IFEQ      *OFF
049100170411     C                   MOVEL     '1'           ERRL00
049200170411     C                   MOVEL     MSG(8)        DSMSMS
049300170411     C                   SETON                                        606162
049400170411     C                   SETON                                        6364
049500170411E   5C                   ENDIF
049600170411     c
049700170411     c                   add       1             xx
049800170411    4c                   enddo
049900170411    3c                   endif
050000170411     c
050100970528E   2C                   ENDIF
050200970528E   1C                   ENDIF
050300170411     c
050400970528     C* INSERIMENTO/RIPRISTINO -> ULTIMA DATA DECORRENZA
050500970528IF  1C     ERRL00        IFEQ      '0'
050600970528IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
050700970528     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
050800970619     C                   MOVEL     VIDPTA        KCSPTA
050900970619     C                   MOVEL     VIDLNP        KCSLNP
051000970619     C                   MOVEL     VIDLNA        KCSLNA
051100970619     C                   MOVEL     VIDTSP        KCSTSP
051200170411
051300170411   2ac     vidtsp        ifne      ' '
051400970619     C     KE2CAS        SETLL     AZCAS01L
051500970619     C     KE2CAS        READE(N)  AZCAS01L                               99
051600970528DO  3C     *IN99         DOWEQ     *OFF
051700970619IF  4C     CASATB        IFEQ      *BLANKS
051800970619IF  5C     CASDDE        IFGT      DDEVID
051900970528     C                   SETON                                        606162
052000970528     C                   SETON                                        6364
052100970528     C                   MOVEL     MSG(19)       DSMSMS
052200970528     C                   MOVEL     '1'           ERRL00
052300970528     C                   LEAVE
052400970528E   5C                   ENDIF
052500970528E   4C                   ENDIF
052600970619     C     KE2CAS        READE(N)  AZCAS01L                               99
052700970528E   3C                   ENDDO
052800170411     c
052900170411  x2ac                   else
053000170411     c                   z-add     1             xx                3 0
053100170411   3ac                   dow       xx<=4 and ERRL00='0'
053200170411    4c                   select
053300170411     c                   when      xx=1
053400170411     C                   MOVEL     'E'           KCSTSP
053500170411     c                   when      xx=2
053600170411     C                   MOVEL     'H'           KCSTSP
053700170411     c                   when      xx=3
053800170411     C                   MOVEL     'C'           KCSTSP
053900170411     c                   when      xx=2
054000170411     C                   MOVEL     'D'           KCSTSP
054100170411    4c                   endsl
054200170411     c
054300170411     C     KE2CAS        SETLL     AZCAS01L
054400170411     C     KE2CAS        READE(N)  AZCAS01L                               99
054500170411DO  3C     *IN99         DOWEQ     *OFF
054600170411IF  4C     CASATB        IFEQ      *BLANKS
054700170411IF  5C     CASDDE        IFGT      DDEVID
054800170411     C                   SETON                                        606162
054900170411     C                   SETON                                        6364
055000170411     C                   MOVEL     MSG(19)       DSMSMS
055100170411     C                   MOVEL     '1'           ERRL00
055200170411     C                   LEAVE
055300170411E   5C                   ENDIF
055400170411E   4C                   ENDIF
055500170411     C     KE2CAS        READE(N)  AZCAS01L                               99
055600170411E   3C                   ENDDO
055700170411     c
055800170411     c                   add       1             xx
055900170411E  3aC                   ENDDO
056000170411   2ac                   endif
056100970528E   2C                   ENDIF
056200970528E   1C                   ENDIF
056300970528     C*--------------------
056400970528     C* TEMPO INTERNO
056500970528     C*--------------------
056600970528IF  1C     ERRL00        IFEQ      '0'
056700970528     C                   MOVEL     VIDTIN        WTEM
056800970528     C                   EXSR      CHKTEM
056900970528     C                   MOVEL     WTEM          VIDTIN
057000970528IF  2C     ERRL00        IFEQ      '1'
057100970528     C                   SETON                                        65
057200970528     C                   MOVEL     MSG(15)       DSMSMS
057300970528E   2C                   ENDIF
057400970528E   1C                   ENDIF
057500970528     C*--------------------
057600970528     C* TEMPO CLIENTE
057700970528     C*--------------------
057800970528IF  1C     ERRL00        IFEQ      '0'
057900970528     C                   MOVEL     VIDTCN        WTEM
058000970528     C                   EXSR      CHKTEM
058100970528     C                   MOVEL     WTEM          VIDTCN
058200970528IF  2C     ERRL00        IFEQ      '1'
058300970528     C                   SETON                                        66
058400970528     C                   MOVEL     MSG(15)       DSMSMS
058500970528E   2C                   ENDIF
058600970528E   1C                   ENDIF
058700970528     C*
058800970528     C                   ENDSR
058900970528     C*------------------------------------------------------------------------*
059000970528     C* CHKLIN - CONTROLLA LA LINEA DI ARRIVO
059100970528     C*------------------------------------------------------------------------*
059200970528     C     CHKLIN        BEGSR
059300970528     C*
059400970528     C     *LIKE         DEFINE    VIDLNP        WLIN
059500970528     C     *LIKE         DEFINE    VDDLNP        WDLIN
059600970528     C*
059700970528     C* RICHIESTO ELENCO
059800970528     C                   SETOFF                                           28
059900970528     C     '?'           SCAN      WLIN                                   28
060000970528IF  1C     *IN28         IFEQ      *ON
060100970528     C                   MOVEL     *BLANKS       §COD1             3
060200970528     C                   MOVEL     *BLANKS       §TIP              1
060300970528     C                   MOVEL     *BLANKS       §DES1            25
060400971203     C                   MOVEL     *BLANKS       §PRA              3
060500971203     C                   CALL      'TNSD24R'
060600970528     C                   PARM                    §COD1
060700970528     C                   PARM                    §TIP
060800970528     C                   PARM                    §DES1
060900971203     C                   PARM                    §PRA
061000970528     C                   MOVEL     §COD1         WLIN
061100970528     C                   MOVEL     §DES1         WDLIN
061200970528E   1C                   ENDIF
061300970528     C*
061400970528     C* SOLO CARATTERI NUMERICI
061500970528     C                   SETOFF                                       28
061600970528     C                   TESTN                   WLIN                 28
061700970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
061800970528     C                   MOVEL     '1'           ERRL00
061900970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
062000970528     C                   MOVE      WLIN          W001              1
062100970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
062200970528     C                   MOVEL     '1'           ERRL00
062300970528X   2C                   ELSE                                                   *NUMERO
062400970528     C*
062500970528     C* CONTROLLA ESISTENZA CODICE
062600970528     C                   MOVEL     WLIN          KORFIL
062700970528     C     KEYORG        CHAIN     AZORG01L                           99
062800970528IF  3C     *IN99         IFEQ      *OFF                                         *TROVATA
062900970528     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATA
063000970528     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
063100970528     C                   MOVEL     ORGDES        WDLIN                          *DESCRIZIONE
063200970528X   3C                   ELSE
063300970528     C                   MOVEL     '1'           ERRL00
063400970528E   3C                   ENDIF
063500970528E   2C                   ENDIF
063600970528E   1C                   ENDIF
063700970528     C*
063800970528     C                   ENDSR
063900970528     C*------------------------------------------------------------------------*
064000970528     C* CHKTSP - CONTROLLA IL TIPO SERVIZIO
064100970528     C*------------------------------------------------------------------------*
064200970528     C     CHKTSP        BEGSR
064300970528     C*
064400970528     C     *LIKE         DEFINE    VIDTSP        WTSP
064500970528     C     *LIKE         DEFINE    VDDTSP        WDTSP
064600970528     C*
064700970528     C* RICHIESTO ELENCO
064800970528     C                   SETOFF                                           28
064900970528     C     '?'           SCAN      WTSP                                   28
065000970528IF  1C     *IN28         IFEQ      *ON
065100020906     C                   Z-ADD     1             §KUT              1 0
065200970528     C                   MOVEL     '5E'          §COD              2
065300970528     C                   MOVEL     *BLANKS       §KEY              8
065400970528     C                   MOVEL     *BLANKS       §DES             30
065500970528     C                   CALL      'X§TABER'
065600970528     C                   PARM                    §KUT
065700970528     C                   PARM                    §COD
065800970528     C                   PARM                    §KEY
065900970528     C                   PARM                    §DES
066000970528     C                   MOVEL     §KEY          WTSP
066100970528     C                   MOVEL     §DES          WDTSP
066200970528E   1C                   ENDIF
066300970528     C*
066400970528     C* CONTROLLA ESISTENZA CODICE
066500020906     C                   MOVEL     1             KTBKUT
066600970528     C                   MOVEL     '5E'          KTBCOD
066700970528     C                   MOVEL(P)  WTSP          KTBKEY
066800970528     C     KEYTAB        CHAIN     TABEL00F                           99
066900970528IF  1C     *IN99         IFEQ      *OFF
067000970528     C     TBLFLG        ANDEQ     *BLANKS
067100970528     C                   MOVEL     TBLUNI        DS5E
067200970528     C                   MOVEL     §5EDES        WDTSP
067300970528X   1C                   ELSE
067400970528     C                   MOVEL     '1'           ERRL00
067500970528E   1C                   ENDIF
067600970528     C*
067700970528     C                   ENDSR
067800970528     C*------------------------------------------------------------------------*
067900970528     C* CHKDA8 - CONTROLLA LA DATA (8)
068000970528     C*------------------------------------------------------------------------*
068100970528     C     CHKDA8        BEGSR
068200970528     C*
068300970528     C     *LIKE         DEFINE    VIDDDE        WDA8
068400970528     C     *LIKE         DEFINE    VIDDDE        DA8W
068500970528     C*
068600970528     C                   Z-ADD     WDA8          G08DAT
068700970528     C                   Z-ADD     *ZEROS        G08INV
068800970528     C                   MOVEL     '0'           G08ERR
068900970528     C                   CALL      'XSRDA8'
069000970528     C                   PARM                    WLBDA8
069100970528     C                   Z-ADD     G08DAT        WDA8
069200970528     C                   Z-ADD     G08INV        DA8W
069300970528IF  1C     G08ERR        IFEQ      '1'
069400970528     C                   MOVEL     '1'           ERRL00
069500970528E   1C                   ENDIF
069600970528     C*
069700970528     C                   ENDSR
069800970528     C*------------------------------------------------------------------------*
069900970528     C* CHKTEM - CONTROLLA IL TEMPO
070000970528     C*------------------------------------------------------------------------*
070100970528     C     CHKTEM        BEGSR
070200970528     C*
070300970528     C     *LIKE         DEFINE    VIDTIN        WTEM
070400970528     C*
070500970528     C* RICHIESTO ELENCO
070600970528     C                   SETOFF                                           28
070700970528     C     '?'           SCAN      WTEM                                   28
070800970528IF  1C     *IN28         IFEQ      *ON
070900020906     C                   Z-ADD     1             §KUT              1 0
071000970528     C                   MOVEL     '1W'          §COD              2
071100970528     C                   MOVEL     *BLANKS       §KEY              8
071200970528     C                   MOVEL     *BLANKS       §DES             30
071300970528     C                   CALL      'X§TABER'
071400970528     C                   PARM                    §KUT
071500970528     C                   PARM                    §COD
071600970528     C                   PARM                    §KEY
071700970528     C                   PARM                    §DES
071800970528     C                   MOVEL     §KEY          WTEM
071900970528E   1C                   ENDIF
072000970528     C*
072100970528     C* SOLO CARATTERI NUMERICI
072200970528     C                   SETOFF                                       28
072300970528     C                   TESTN                   WTEM                 28
072400970528IF  1C     *IN28         IFEQ      *OFF                                         *NON E'NUMERO
072500970528     C                   MOVEL     '1'           ERRL00
072600970528X   1C                   ELSE                                                   *E'NUMERO DA TESTN
072700970528     C                   MOVE      WTEM          W001              1
072800970528IF  2C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
072900970528     C                   MOVEL     '1'           ERRL00
073000970528X   2C                   ELSE                                                   *NUMERO
073100970528     C*
073200970528     C* CONTROLLA ESISTENZA CODICE
073300020906     C                   Z-ADD     1             KTBKUT
073400970528     C                   MOVEL     '1W'          KTBCOD
073500970528     C                   MOVEL(P)  WTEM          KTBKEY
073600970528     C     KEYTAB        CHAIN     TABEL00F                           99
073700970528IF  3C     *IN99         IFEQ      *ON                                          *NON TROVATA
073800970528     C     TBLFLG        ORNE      *BLANKS                                      *ANNULLATA
073900970528     C                   MOVEL     '1'           ERRL00
074000970528E   3C                   ENDIF
074100970528E   2C                   ENDIF
074200970528E   1C                   ENDIF
074300970528     C*
074400970528     C                   ENDSR
074500970526     C*------------------------------------------------------------------------*
074600970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
074700970526     C*------------------------------------------------------------------------*
074800970526     C     IREVID        BEGSR
074900970526     C* CAMPI
075000970619     C                   MOVEL     CASPTA        VIDPTA
075100970619     C                   MOVEL     CASLNP        VIDLNP
075200970619     C                   MOVEL     CASLNA        VIDLNA
075300970619     C                   MOVEL     CASTSP        VIDTSP
075400970619     C                   Z-ADD     CASDDE        G08INV
075500970528     C                   Z-ADD     *ZEROS        G08DAT
075600970528     C                   MOVEL     '3'           G08ERR
075700970528     C                   CALL      'XSRDA8'
075800970528     C                   PARM                    WLBDA8
075900970528     C                   Z-ADD     G08DAT        VIDDDE
076000970619     C                   MOVEL     CASTIN        VIDTIN
076100970619     C                   MOVEL     CASTCN        VIDTCN
076200970527     C*
076300970526     C                   ENDSR
076400970526     C*------------------------------------------------------------------------*
076500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
076600970526     C*------------------------------------------------------------------------*
076700970526     C     AZZVID        BEGSR
076800970526     C* CAMPI
076900970528     C                   MOVEL     *BLANKS       VIDPTA
077000970528     C                   MOVEL     *ZEROS        VIDLNP
077100970528     C                   MOVEL     *ZEROS        VIDLNA
077200970528     C                   MOVEL     *BLANKS       VIDTSP
077300970528     C                   Z-ADD     *ZEROS        VIDDDE
077400970528     C                   MOVEL     *ZEROS        VIDTIN
077500970528     C                   MOVEL     *ZEROS        VIDTCN
077600970526     C* DECODIFICHE
077700970528     C                   MOVEL     *BLANKS       VDDLNP
077800970528     C                   MOVEL     *BLANKS       VDDLNA
077900970528     C                   MOVEL     *BLANKS       VDDTSP
078000170411     c                   eval      vddtsp='_=TUTTI I TIPI SERVIZIO'
078100970526     C*
078200970526     C                   ENDSR
078300970526     C*------------------------------------------------------------------------*
078400970619     C* IMPCAS - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
078500970526     C*------------------------------------------------------------------------*
078600970619     C     IMPCAS        BEGSR
078700970526     C*
078800970619     C                   MOVEL     VIDTIN        CASTIN
078900970619     C                   MOVEL     VIDTCN        CASTCN
079000970526     C*
079100970526     C                   ENDSR
079200970526     C*------------------------------------------------------------------------*
079300970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
079400970526     C*------------------------------------------------------------------------*
079500970526     C     IMPFUN        BEGSR
079600970526     C*
079700970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
079800970526IF  1C     DEPOP1        IFEQ      'O05'
079900970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
080000970526     C                   MOVEL     'N'           FUDATT
080100970526     C                   MOVEL     FUDRIG        FUD(6)
080200970526E   1C                   ENDIF
080300970526     C*
080400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
080500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
080600970526     C                   EXSR      FUNGES
080700970526     C*
080800970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
080900970526     C*---
081000970526     C* 90 -> PROTEZIONE CHIAVE
081100970526     C* 91 -> PROTEZIONE TOTALE
081200970526     C*---
081300970528IF  1C     DEPOP0        IFEQ      'Z02'                                        G E S T I O N E
081400970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
081500970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
081600970526     C                   SETOFF                                       91
081700970526E   2C                   ENDIF
081800970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
081900970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
082000970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
082100970526E   2C                   ENDIF
082200970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
082300970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
082400970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
082500970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
082600970526     C                   SETON                                        91
082700970526E   2C                   ENDIF
082800970526X   1C                   ELSE                                                   *I N T E R R O G A
082900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
083000970526     C                   SETON                                        91
083100970526E   1C                   ENDIF
083200970526     C*
083300970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
083400970526IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
083500970619     C                   MOVEL     D00SPT        KCSPTA
083600970619     C                   Z-ADD     D00SLP        KCSLNP
083700970619     C                   Z-ADD     D00SLA        KCSLNA
083800970619     C                   MOVEL     D00STS        KCSTSP
083900970619     C                   Z-ADD     D00SDD        KCSDDE
084000970619     C     KEYCAS        CHAIN     AZCAS01L                           9998
084100970526IF  2C     *IN99         IFEQ      *ON                                          *CAP NON TROVATO
084200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
084300970526     C                   MOVEL     MSG(6)        DSMSMS
084400970526X   2C                   ELSE
084500980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
084600980128IF  3C     DEPOP1        IFEQ      'O05'
084700980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
084800980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
084900980128E   3C                   ENDIF
085000980128E   2C                   ENDIF
085100970526X   1C                   ELSE                                                   *INSERIMENTO
085200970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
085300970526E   1C                   ENDIF
085400970526     C*
085500970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
085600970526IF  1C     DEPOP1        IFEQ      'O02'
085700970526     C                   MOVEL     DEC(1)        VIDFUN
085800970526E   1C                   ENDIF
085900970526IF  1C     DEPOP1        IFEQ      'O04'
086000970526     C                   MOVEL     DEC(2)        VIDFUN
086100970526E   1C                   ENDIF
086200970526IF  1C     DEPOP1        IFEQ      'O05'
086300970526     C                   MOVEL     DEC(3)        VIDFUN
086400970526E   1C                   ENDIF
086500970526IF  1C     DEPOP1        IFEQ      'O07'
086600970526     C                   MOVEL     DEC(4)        VIDFUN
086700970526E   1C                   ENDIF
086800970526IF  1C     DEPOP1        IFEQ      'F06'
086900970526     C                   MOVEL     DEC(5)        VIDFUN
087000970526E   1C                   ENDIF
087100970526     C*
087200970526     C                   ENDSR
087300970526     C*------------------------------------------------------------------------*
087400970526     C* *INZSR - OPERAZIONI INIZIALI
087500970526     C*------------------------------------------------------------------------*
087600970526     C     *INZSR        BEGSR
087700970526     C*---
087800970526     C* RICEVIMENTO PARAMETRI
087900970526     C*---
088000970526     C     *ENTRY        PLIST
088100970612     C                   PARM                    KPJBA
088200970612     C                   PARM                    DSSI00
088300970526     C*
088400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
088500970526     C     XMSGPA        PLIST
088600970526     C                   PARM                    DSMSMS
088700970526     C                   PARM                    DSMSPG
088800970526     C                   PARM                    DSMSKE
088900970526     C                   PARM                    DSMSER
089000970526     C*
089100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
089200970526     C                   CALL      'XKEY'
089300970526     C                   PARM                    DSKEY
089400970526     C*---
089500970526     C* VARIABILI NON RIFERITE AL DATA BASE
089600970526     C*---
089700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
089800970526     C     *LIKE         DEFINE    D00OP0        DEPOP0
089900970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
090000970526     C     *LIKE         DEFINE    D00OP0        WOP0
090100970526     C     *LIKE         DEFINE    D00OP1        WOP1
090200970526     C*---
090300970526     C* VARIABILI DI CONTROLLO
090400970526     C*---
090500970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
090600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
090700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
090800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
090900970526     C*
091000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
091100970528     C                   MOVEL     'TISI24R'     DSMSPG
091200970528     C                   MOVEL     'TISI24R'     MSGQUE
091300970526     C                   MOVEL     *BLANKS       MSGKEY
091400970526     C                   MOVEL     *BLANKS       DSMSMS
091500970526     C*---
091600970526     C* VARIABILI RIFERITE AL DATA BASE
091700970526     C*---
091800970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
091900970528     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
092000970528     C     *LIKE         DEFINE    TBLCOD        KTBCOD
092100970528     C     *LIKE         DEFINE    TBLKEY        KTBKEY
092200970619     C     *LIKE         DEFINE    CASPTA        KCSPTA                         *AZCAS00F
092300970619     C     *LIKE         DEFINE    CASLNP        KCSLNP
092400970619     C     *LIKE         DEFINE    CASLNA        KCSLNA
092500970619     C     *LIKE         DEFINE    CASTSP        KCSTSP
092600970619     C     *LIKE         DEFINE    CASDDE        KCSDDE
092700970526     C*---
092800970526     C* CHIAVI DI LETTURA
092900970526     C*---
093000970528     C* LETTURA AZORG01L - COMPLETA
093100970528     C     KEYORG        KLIST
093200970528     C                   KFLD                    KORFIL                         *FILIALE
093300970528     C* LETTURA TABEL00F - COMPLETA
093400970528     C     KEYTAB        KLIST
093500970528     C                   KFLD                    KTBKUT                         *UTENTE
093600970528     C                   KFLD                    KTBCOD                         *TABELLA
093700970528     C                   KFLD                    KTBKEY                         *CHIAVE
093800970619     C* LETTURA AZCAS01L - COMPLETA
093900970619     C     KEYCAS        KLIST
094000970619     C                   KFLD                    KCSPTA                         *PART/TRAZ/ARR
094100970619     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
094200970619     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
094300970619     C                   KFLD                    KCSTSP                         *TIPO SERVIZIO
094400970619     C                   KFLD                    KCSDDE                         *DECORRENZA
094500970619     C* LETTURA AZCAS01L - PARZIALE
094600970619     C     KE2CAS        KLIST
094700970619     C                   KFLD                    KCSPTA                         *PART/TRAS/ARR
094800970619     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
094900970619     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
095000970619     C                   KFLD                    KCSTSP                         *TIPO SERVIZIO
095100970619     C* LETTURA AZCAS01L - PARZIALE
095200970619     C     KE3CAS        KLIST
095300970619     C                   KFLD                    KCSPTA                         *PART/TRAS/ARR
095400970619     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
095500970619     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
095600970528     C*
095700970528     C* CALCOLA LA DATA CORRENTE
095800970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
095900970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
096000970528     C                   Z-ADD     WN8           G08DAT
096100970528     C                   Z-ADD     *ZEROS        G08INV
096200970528     C                   MOVEL     '0'           G08ERR
096300970528     C                   CALL      'XSRDA8'
096400970528     C                   PARM                    WLBDA8
096500970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
096600970526     C*
096700970526     C* DEPOSITA LE OPZIONI DI ENTRATA
096800970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
096900970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
097000970526     C*
097100970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
097200970526     C                   EXSR      IMPFUN
097300970526     C*
097400970526     C                   ENDSR
097500970526     C*------------------------------------------------------------------------*
097600970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
097700970526     C*------------------------------------------------------------------------*
097800970526     C     FUNGES        BEGSR
097900970526     C*
098000970526     C* PULISCO CAMPI DI LAVORO
098100970526     C                   Z-ADD     0             W                 5 0
098200970526     C*
098300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
098400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
098500970526     C     1             DO        24            I
098600970526     C                   MOVEL     FUD(I)        FUDRIG
098700970526     C*
098800970526     C* PULIZIA FUNZIONI PRECEDENTI
098900970526     C                   MOVEL     *BLANKS       FUV(I)
099000970526     C*
099100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
099200970526     C* (SALTA F24)
099300970526     C     FUDATT        IFEQ      'S'
099400970526     C     FUDFUN        ANDNE     '24'
099500970526     C                   ADD       1             W
099600970526     C                   MOVEL     FUDDES        FUV(W)
099700970526     C                   END
099800970526     C                   END                                                    DO WHILE
099900970526     C*
100000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
100100970526     C* ALTRIMENTI DISATTIVO F24
100200970526     C     1             DO        24            I
100300970526     C                   MOVEL     FUD(I)        FUDRIG
100400970526     C*
100500970526     C     FUDFUN        IFEQ      '24'
100600970526     C     W             IFGT      6
100700970526     C                   MOVEL     'S'           FUDATT
100800970526     C                   ELSE
100900970526     C                   MOVEL     *BLANKS       FUDATT
101000970526     C                   END                                                    W GT 8
101100970526     C                   MOVEL     FUDRIG        FUD(I)
101200970526     C                   END
101300970526     C                   END                                                    DO WHILE
101400970526     C*
101500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
101600970526     C     W             DIV       6             I
101700970526     C                   MVR                     Z
101800970526     C*
101900970526     C     Z             IFNE      0
102000970526     C     I             ADD       1             Z
102100970526     C                   ELSE
102200970526     C                   Z-ADD     I             Z                 5 0
102300970526     C                   END
102400970526     C*
102500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
102600970526     C                   ADD       1             FUNGRU
102700970526     C     FUNGRU        IFGT      Z
102800970526     C     FUNGRU        ORLT      1
102900970526     C                   Z-ADD     1             FUNGRU
103000970526     C                   END
103100970526     C*
103200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
103300970526     C     FUNGRU        IFEQ      1
103400970526     C                   MOVEL     FUV(1)        FUN001
103500970526     C                   MOVEL     FUV(2)        FUN002
103600970526     C                   MOVEL     FUV(3)        FUN003
103700970526     C                   MOVEL     FUV(4)        FUN004
103800970526     C                   MOVEL     FUV(5)        FUN005
103900970526     C                   MOVEL     FUV(6)        FUN006
104000970526     C                   END
104100970526     C*
104200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
104300970526     C     FUNGRU        IFEQ      2
104400970526     C                   MOVEL     FUV(7)        FUN001
104500970526     C                   MOVEL     FUV(8)        FUN002
104600970526     C                   MOVEL     FUV(9)        FUN003
104700970526     C                   MOVEL     FUV(10)       FUN004
104800970526     C                   MOVEL     FUV(11)       FUN005
104900970526     C                   MOVEL     FUV(12)       FUN006
105000970526     C                   END
105100970526     C*
105200970526     C* TERZO GRUPPO DI 6 FUNZIONI
105300970526     C     FUNGRU        IFEQ      3
105400970526     C                   MOVEL     FUV(13)       FUN001
105500970526     C                   MOVEL     FUV(14)       FUN002
105600970526     C                   MOVEL     FUV(15)       FUN003
105700970526     C                   MOVEL     FUV(16)       FUN004
105800970526     C                   MOVEL     FUV(17)       FUN005
105900970526     C                   MOVEL     FUV(18)       FUN006
106000970526     C                   END
106100970526     C*
106200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
106300970526     C     FUNGRU        IFEQ      4
106400970526     C                   MOVEL     FUV(19)       FUN001
106500970526     C                   MOVEL     FUV(20)       FUN002
106600970526     C                   MOVEL     FUV(21)       FUN003
106700970526     C                   MOVEL     FUV(22)       FUN004
106800970526     C                   MOVEL     *BLANKS       FUN005
106900970526     C                   MOVEL     *BLANKS       FUN006
107000970526     C                   END
107100970526     C*
107200970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
107300970526     C                   MOVEL     *BLANKS       FUN007
107400970526     C                   MOVEL     *BLANKS       FUN008
107500970526     C*
107600970526     C     1             DO        24            I
107700970526     C                   MOVEL     FUD(I)        FUDRIG
107800970526     C     FUDFUN        IFEQ      '24'
107900970526     C     FUDATT        ANDEQ     'S'
108000970526     C                   MOVEL     FUDDES        FUN008
108100970526     C                   END
108200970526     C                   END                                                    DO
108300970526     C*
108400970526     C                   ENDSR
108500970526     C*------------------------------------------------------------------------*
108600970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
108700970526     C*------------------------------------------------------------------------*
108800970526     C     FUNCON        BEGSR
108900970526     C*
109000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
109100970526     C                   EXSR      FUNVAR
109200970526     C*
109300970526     C* PULISCO CAMPI DI LAVORO
109400970526     C                   Z-ADD     0             I                 5 0
109500970526     C                   Z-ADD     0             W                 5 0
109600970526     C*
109700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
109800970526     C     FUNKEY        IFNE      *BLANKS
109900970526     C     1             DO        24            I
110000970526     C                   MOVEL     FUD(I)        FUDRIG
110100970526     C*
110200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
110300970526     C     FUNKEY        IFEQ      FUDFUN
110400970526     C     FUDATT        ANDEQ     'S'
110500970526     C                   Z-ADD     I             W
110600970526     C                   END
110700970526     C                   END                                                    DO WHILE
110800970526     C*
110900970526     C* FUNZIONE NON ATTIVA (ERRORE)
111000970526     C     W             IFEQ      0
111100970526     C*
111200970526     C* EMISSIONE MESSAGGIO DI ERRORE
111300970526     C                   MOVEL     MSG(1)        DSMSMS
111400970526     C*
111500970526     C                   END                                                    W=0
111600970526     C                   END                                                    FUNKEY NE *BLANKS
111700970526     C*
111800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
111900970526     C     W             IFNE      0
112000970526     C     FUNKEY        OREQ      *BLANKS
112100970526     C     FUNKEY        IFEQ      *BLANKS
112200970526     C                   EXSR      ENTGES
112300970526     C                   END
112400970526     C     FUNKEY        IFEQ      '03'
112500970526     C                   EXSR      F03GES
112600970526     C                   END
112700970526     C     FUNKEY        IFEQ      '06'
112800970526     C                   EXSR      F06GES
112900970526     C                   END
113000970526     C     FUNKEY        IFEQ      '12'
113100970526     C                   EXSR      F12GES
113200970526     C                   END
113300970526     C     FUNKEY        IFEQ      '24'
113400970526     C                   EXSR      F24GES
113500970526     C                   END
113600970526     C                   END
113700970526     C*
113800970526     C                   ENDSR
113900970526     C*------------------------------------------------------------------------*
114000970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
114100970526     C*------------------------------------------------------------------------*
114200970526     C     FUNVAR        BEGSR
114300970526     C*
114400970526     C* INIZIALIZZA VARIABILE -FUNKEY-
114500970526     C*
114600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
114700970526     C     KEY           IFEQ      ENTER
114800970526     C                   MOVEL     *BLANKS       FUNKEY
114900970526     C                   END
115000970526     C     KEY           IFEQ      F1
115100970526     C                   MOVEL     '01'          FUNKEY
115200970526     C                   END
115300970526     C     KEY           IFEQ      F2
115400970526     C                   MOVEL     '02'          FUNKEY
115500970526     C                   END
115600970526     C     KEY           IFEQ      F3
115700970526     C                   MOVEL     '03'          FUNKEY
115800970526     C                   END
115900970526     C     KEY           IFEQ      F4
116000970526     C                   MOVEL     '04'          FUNKEY
116100970526     C                   END
116200970526     C     KEY           IFEQ      F5
116300970526     C                   MOVEL     '05'          FUNKEY
116400970526     C                   END
116500970526     C     KEY           IFEQ      F6
116600970526     C                   MOVEL     '06'          FUNKEY
116700970526     C                   END
116800970526     C     KEY           IFEQ      F7
116900970526     C                   MOVEL     '07'          FUNKEY
117000970526     C                   END
117100970526     C     KEY           IFEQ      F8
117200970526     C                   MOVEL     '08'          FUNKEY
117300970526     C                   END
117400970526     C     KEY           IFEQ      F9
117500970526     C                   MOVEL     '09'          FUNKEY
117600970526     C                   END
117700970526     C     KEY           IFEQ      F10
117800970526     C                   MOVEL     '10'          FUNKEY
117900970526     C                   END
118000970526     C     KEY           IFEQ      F11
118100970526     C                   MOVEL     '11'          FUNKEY
118200970526     C                   END
118300970526     C     KEY           IFEQ      F12
118400970526     C                   MOVEL     '12'          FUNKEY
118500970526     C                   END
118600970526     C     KEY           IFEQ      F13
118700970526     C                   MOVEL     '13'          FUNKEY
118800970526     C                   END
118900970526     C     KEY           IFEQ      F14
119000970526     C                   MOVEL     '14'          FUNKEY
119100970526     C                   END
119200970526     C     KEY           IFEQ      F15
119300970526     C                   MOVEL     '15'          FUNKEY
119400970526     C                   END
119500970526     C     KEY           IFEQ      F16
119600970526     C                   MOVEL     '16'          FUNKEY
119700970526     C                   END
119800970526     C     KEY           IFEQ      F17
119900970526     C                   MOVEL     '17'          FUNKEY
120000970526     C                   END
120100970526     C     KEY           IFEQ      F18
120200970526     C                   MOVEL     '18'          FUNKEY
120300970526     C                   END
120400970526     C     KEY           IFEQ      F19
120500970526     C                   MOVEL     '19'          FUNKEY
120600970526     C                   END
120700970526     C     KEY           IFEQ      F20
120800970526     C                   MOVEL     '20'          FUNKEY
120900970526     C                   END
121000970526     C     KEY           IFEQ      F21
121100970526     C                   MOVEL     '21'          FUNKEY
121200970526     C                   END
121300970526     C     KEY           IFEQ      F22
121400970526     C                   MOVEL     '22'          FUNKEY
121500970526     C                   END
121600970526     C     KEY           IFEQ      F23
121700970526     C                   MOVEL     '23'          FUNKEY
121800970526     C                   END
121900970526     C     KEY           IFEQ      F24
122000970526     C                   MOVEL     '24'          FUNKEY
122100970526     C                   END
122200970526     C     KEY           IFEQ      ROLLUP
122300970526     C                   MOVEL     'RU'          FUNKEY
122400970526     C                   END
122500970526     C     KEY           IFEQ      HELP
122600970526     C                   MOVEL     'HP'          FUNKEY
122700970526     C                   END
122800970526     C                   ENDSR
122900970526     C*------------------------------------------------------------------------*
123000970526     C* NOTE COSTRUZIONE SCHIERE
123100970526     C*------------------------------------------------------------------------*
123200970526     C*
123300970526     C* -MSG-MESSAGGI
123400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
123500970526     C* (NON TOCCARE MSG,1 E MSG,2)
123600970526     C*
123700970526     C* -CMD-COMANDI
123800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
123900970526     C*
124000970526     C* -OPD-OPZIONI
124100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
124200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
124300970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
124400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
124500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
124600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
124700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
124800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
124900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
125000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
125100970526     C*
125200970526     C* -FUD-FUNZIONI
125300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
125400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
125500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
125600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
125700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
125800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
125900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
126000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
126100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
126200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
126300970526     O*------------------------------------------------------------------------*
126400970526** DEC - DECODIFICHE
126500970526    MODIFICA                                                      1
126600970526  ANNULLAMENTO                                                    2
126700970526VISUALIZZAZIONE                                                   3
126800970526   RIPRISTINO                                                     4
126900970526  INSERIMENTO                                                     5
127000970528** MSG - MESSAGGI
127100970528TISI24R- Tasto funzionale non gestito                                          1
127200970528TISI24R- Inserimento effettuato                                                2
127300970528TISI24R- Modifica effettuata                                                   3
127400970528TISI24R- Annullamento effettuato                                               4
127500970528TISI24R- Ripristino effettuato                                                 5
127600170411TISI24R- Richiesta funzione per record inesistente                             6
127700970528TISI24R- Record momentaneamente allocato: riprovare più tardi                  7
127800170411TISI24R- Richiesto inserimento per tempo di trazione già esistente             8
127900970528TISI24R- 9
128000970528TISI24R- 10
128100970528TISI24R- Immettere P=Partenza, T=Trazione, A=Arrivo                            11
128200970528TISI24R- Terminal / Linea partenza errato                                      12
128300970528TISI24R- Terminal / Linea arrivo errato                                        13
128400970528TISI24R- Data errata                                                           14
128500970528TISI24R- Tempo errato                                                          15
128600970528TISI24R- Tipo servizio errato                                                  16
128700970528TISI24R- Per tempo partenza non inserire la linea di arrivo                    17
128800970528TISI24R- Per tempo arrivo non inserire la linea di partenza                    18
128900970528TISI24R- Esiste già un tempo con data decorrenza superiore                     19
129000970528TISI24R- 20
129100970526** FUD - FUNZIONI
129200970526                   -        (S/N)=N                        funz.01¬
129300970526                   -        (S/N)=N                        funz.02¬
129400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
129500970526                   -        (S/N)=N                        funz.04¬
129600970526                   -        (S/N)=N                        funz.05¬
129700970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
129800970527                   -        (S/N)=N                        funz.07¬
129900970527                   -        (S/N)=N                        funz.08¬
130000970527                   -        (S/N)=N                        funz.09¬
130100970527                   -        (S/N)=N                        funz.10¬
130200970526                   -        (S/N)=N                        funz.11¬
130300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
130400970526                   -        (S/N)=N                        funz.13¬
130500970526                   -        (S/N)=N                        funz.14¬
130600970526                   -        (S/N)=N                        funz.15¬
130700970526                   -        (S/N)=N                        funz.16¬
130800970526                   -        (S/N)=N                        funz.17¬
130900970526                   -        (S/N)=N                        funz.18¬
131000970526                   -        (S/N)=N                        funz.19¬
131100970526                   -        (S/N)=N                        funz.20¬
131200970526                   -        (S/N)=N                        funz.21¬
131300970526                   -        (S/N)=N                        funz.22¬
131400970526                   -        (S/N)=N                        funz.23¬
131500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
