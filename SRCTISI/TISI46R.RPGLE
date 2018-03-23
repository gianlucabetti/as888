000100970526     H*------------------------------------------------------------------------*
000200970528     H* CAPPARIO - GESTIONE VERSIONI CAP - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970526     FTISI46D   CF   E             WORKSTN INFDS(DEVDS1)
000900970526     F                                     SFILE(SI46D05:SFLULT)
001000970526     F                                     SFILE(SI46DMS:MSGULT)
001100970526     F*------------
001200970526     F* VERSIONI
001300970526     F*------------
001400970526     FAZCPR01L  IF   E           K DISK
001500970526     FAZCPR02L  IF   E           K DISK
001600970526     F                                     RENAME(AZCPR000:AZCPR2)
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
002700970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* COMANDI
003000970526     D*------------
003100970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI OPZIONI
003400970526     D*------------
003500970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI TASTI FUNZIONALI
003800970526     D*------------
003900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* OPZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970526     D OPV             S             19    DIM(24)
004400970526     D*------------
004500970526     D* FUNZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970526     D FUV             S             19    DIM(24)
004800970526     D*------------------------------------------------------------------------*
004900970526     D* INPUT
005000970526     D*------------------------------------------------------------------------*
005100970526     D*------------
005200970526     D* RIDEFINIZIONE OPZIONI VIDEO
005300970526     D*------------
005400970526     D OPDDS           DS
005500970526     D  OPDOPZ                 1      2
005600970526     D  OPDDE1                 3     19
005700970526     D  OPDOP1                21     22
005800970526     D  OPDFI0                23     34
005900970526     D  OPDATT                35     35
006000970526     D  OPDFI1                36     80
006100970526     D  OPDDES                 1     19
006200970526     D  OPDRIG                 1     80
006300970526     D*------------
006400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006500970526     D*------------
006600970526     D FUDDS           DS
006700970526     D  FUDDE1                 1      1
006800970526     D  FUDFUN                 2      3
006900970526     D  FUDDE2                 4     19
007000970526     D  FUDFI0                20     34
007100970526     D  FUDATT                35     35
007200970526     D  FUDFI1                36     80
007300970526     D  FUDDES                 1     19
007400970526     D  FUDRIG                 1     80
007500970526     D*------------
007600970526     D* DS GESTIONE TASTI FUNZIONALI
007700970526     D*------------
007800970526     D DSKEY         E DS
007900970526     D DEVDS1          DS
008000970526     D  KEY                  369    369
008100970526     D*------------
008200970526     D* DS "XMSG" - GESTIONE MESSAGGI
008300970526     D*------------
008400970526     D DSMSG         E DS
008500970526     D*------------
008600020919     D* DS REPERIMENTO DATI UTENTE
008700970526     D*------------
008800020919     D TIBS34DS      E DS                                                       *Profili utente
008900020919     D DDATIUTE      E DS                                                       *Dati utente
009000020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009100970526     D*------------------
009200970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
009300970526     D*------------------
009400970526     D WLBDA8          DS                  INZ
009500970526     D  G08DAT                 1      8  0
009600970526     D  G08INV                 9     16  0
009700970526     D  G08ERR                17     17
009800970526     D  G08TGI                18     22  0
009900970526     D*------------
010000970526     D* DS DI PROCEDURA
010100970526     D*------------
010200970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010300970526     D*------------
010400970526     D* ARCHITETTURA
010500970526     D*------------
010600970526     D KPJBA         E DS
010700970715     D  UTENTE                 1      3
010800970526     C*------------------------------------------------------------------------*
010900970526     C* MAIN LINES
011000970526     C*------------------------------------------------------------------------*
011100970526     C*
011200970526     C* SE NON CI SONO ERRORI, PROSEGUE
011300970526IF  1C     ERRL00        IFNE      '1'
011400970526     C*
011500970526     C* CARICA IL SFILE
011600970526     C                   EXSR      POSSFL
011700970526     C*
011800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012200970526     C*
012300970526     C* REIMPOSTA ERRORE DI PROCERURA
012400970526     C                   MOVEL     '0'           ERRL00
012500970526     C*
012600970526     C* EMETTE IL SFILE
012700970526     C                   EXSR      OUTSFL
012800970526     C*
012900970526     C* GESTIONE TASTI FUNZIONALI
013000970526     C                   EXSR      FUNCON
013100970526     C*
013200970526E   2C                   ENDDO
013300970526E   1C                   ENDIF
013400970526     C* FINE PGM
013500970526     C                   SETON                                        LR
013600970526     C*------------------------------------------------------------------------*
013700970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013800970526     C*------------------------------------------------------------------------*
013900970526     C     F03GES        BEGSR
014000970526     C*
014100970526     C* IMPOSTA "PREMUTO F03"
014200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014300970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
014400970526     C*
014500970526     C                   ENDSR
014600970526     C*------------------------------------------------------------------------*
014700970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014800970526     C*------------------------------------------------------------------------*
014900970526     C     F05GES        BEGSR
015000970526     C*
015100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015200970526     C*
015300970526     C                   ENDSR
015400970526     C*------------------------------------------------------------------------*
015500970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015600970526     C*------------------------------------------------------------------------*
015700970526     C     F06GES        BEGSR
015800970526     C*
015900970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
016000020919IF  1C     DUTLPO        IFNE      'S'
016100970526     C                   MOVEL     '1'           ERRL00
016200970526     C                   MOVEL     MSG(3)        DSMSMS
016300970526E   1C                   ENDIF
016400970526     C*
016500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016600970602     C                   Z-ADD     *ZEROS        DDRI
016700970602     C                   Z-ADD     *ZEROS        DVER
016800970529     C                   MOVEL     DEPOP0        WOP0
016900970529     C                   MOVEL     'F06'         WOP1
017000970526     C                   MOVEL     'TISI47R'     WPGM              8
017100970526     C                   EXSR      LANPGM                                       *LANCIA PGM
017200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017300970526E   1C                   ENDIF
017400970526     C*
017500970526     C                   ENDSR
017600970528     C*------------------------------------------------------------------------*
017700970528     C* F08GES - GESTIONE F08 -> GESTIONE LOCALITA'
017800970528     C*------------------------------------------------------------------------*
017900970528     C     F08GES        BEGSR
018000970528     C*
018100980225     C                   Z-ADD     ULTDRI        DDRI
018200980225     C                   Z-ADD     ULTVER        DVER
018300970528     C                   MOVEL     DEPOP0        WOP0
018400970528     C                   MOVEL     'L'           WOP0
018500970528     C                   MOVEL     'F08'         WOP1
018600970528     C                   MOVEL     'TISI30R'     WPGM              8
018700970528     C                   EXSR      LANPGM
018800970528     C*
018900970528     C                   ENDSR
019000970528     C*------------------------------------------------------------------------*
019100970528     C* F09GES - GESTIONE F09 -> ECCEZIONI LINEE/ZONE
019200970528     C*------------------------------------------------------------------------*
019300970528     C     F09GES        BEGSR
019400970528     C*
019500980225     C                   Z-ADD     ULTDRI        DDRI
019600980225     C                   Z-ADD     ULTVER        DVER
019700970528     C                   MOVEL     DEPOP0        WOP0
019800970528     C                   MOVEL     'E'           WOP0
019900970528     C                   MOVEL     'F09'         WOP1
020000970528     C                   MOVEL     'TISI11R'     WPGM              8
020100970528     C                   EXSR      LANPGM
020200970528     C*
020300970528     C                   ENDSR
020400970528     C*------------------------------------------------------------------------*
020500970528     C* F10GES - GESTIONE F10 -> TEMPI DI CONSEGNA
020600970528     C*------------------------------------------------------------------------*
020700970528     C     F10GES        BEGSR
020800970528     C*
020900980225     C                   Z-ADD     ULTDRI        DDRI
021000980225     C                   Z-ADD     ULTVER        DVER
021100970528     C                   MOVEL     DEPOP0        WOP0
021200970528     C                   MOVEL     'T'           WOP0
021300970528     C                   MOVEL     'F10'         WOP1
021400970528     C                   MOVEL     'TISI41R'     WPGM              8
021500970528     C                   EXSR      LANPGM
021600970528     C*
021700970528     C                   ENDSR
021800970528     C*------------------------------------------------------------------------*
021900970528     C* F11GES - GESTIONE F11 -> CAP
022000970528     C*------------------------------------------------------------------------*
022100970528     C     F11GES        BEGSR
022200970603     C*
022300980225     C                   Z-ADD     ULTDRI        DDRI
022400980225     C                   Z-ADD     ULTVER        DVER
022500970528     C                   MOVEL     DEPOP0        WOP0
022600970528     C                   MOVEL     'C'           WOP0
022700970528     C                   MOVEL     'F11'         WOP1
022800970528     C                   MOVEL     'TISI02R'     WPGM              8
022900970528     C                   EXSR      LANPGM
023000970528     C*
023100970528     C                   ENDSR
023200970603     C*------------------------------------------------------------------------*
023300970603     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
023400970603     C*------------------------------------------------------------------------*
023500970603     C     F13GES        BEGSR
023600970603     C*
023700980225     C                   Z-ADD     ULTDRI        DDRI
023800980225     C                   Z-ADD     ULTVER        DVER
023900970603     C                   MOVEL     DEPOP0        WOP0
024000970603     C                   MOVEL     'S'           WOP0
024100970603     C                   MOVEL     'F13'         WOP1
024200970603     C                   MOVEL     'TISI35R'     WPGM              8
024300970603     C                   EXSR      LANPGM
024400970603     C*
024500970603     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* F12GES - GESTIONE F12 -> RITORNO
024800970526     C*------------------------------------------------------------------------*
024900970526     C     F12GES        BEGSR
025000970526     C*
025100970526     C* IMPOSTA "PREMUTO F12"
025200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
025300970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
025400970526     C*
025500970526     C                   ENDSR
025600970526     C*------------------------------------------------------------------------*
025700970526     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
025800970526     C*------------------------------------------------------------------------*
025900970526     C     F22GES        BEGSR
026000970526     C*
026100970526     C                   Z-ADD     80            LENGH            15 5
026200970526     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
026300970526     C                   PARM                    CMD(1)
026400970526     C                   PARM                    LENGH
026500970526     C*
026600970526     C                   ENDSR
026700970526     C*------------------------------------------------------------------------*
026800970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
026900970526     C*------------------------------------------------------------------------*
027000970526     C     F23GES        BEGSR
027100970526     C*
027200970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
027300970526     C*
027400970526     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
027700970526     C*------------------------------------------------------------------------*
027800970526     C     F24GES        BEGSR
027900970526     C*
028000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
028100970526     C*
028200970526     C                   ENDSR
028300970526     C*------------------------------------------------------------------------*
028400970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
028500970526     C*------------------------------------------------------------------------*
028600970526     C     RUPGES        BEGSR
028700970526     C*
028800970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
028900970526     C*
029000970526     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200970526     C* ENTGES - GESTIONE ENTER -> INVIO
029300970526     C*------------------------------------------------------------------------*
029400970526     C     ENTGES        BEGSR
029500970526     C*
029600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
029700970526     C                   EXSR      NEWPOS
029800970526     C*
029900970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
030000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030100970526IF  2C     SWPOSI        IFNE      0
030200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
030300970526X   2C                   ELSE
030400970526IF  3C     SFLULT        IFNE      0
030500970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
030600970526     C*
030700970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
030800970526IF  4C     SWMODI        IFNE      0
030900970526     C                   EXSR      POSSFL
031000970526E   4C                   ENDIF
031100970526E   3C                   ENDIF
031200970526E   2C                   ENDIF
031300970526E   1C                   ENDIF
031400970526     C*
031500970526     C                   ENDSR
031600970526     C*------------------------------------------------------------------------*
031700970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
031800970526     C*------------------------------------------------------------------------*
031900970526     C     O01GES        BEGSR
032000970526     C*
032100970526     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
032200970526     C                   Z-ADD     VIDVER        D00VER
032300970528     C                   Z-ADD     VIHDDE        D00DDE
032400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032500970526     C*
032600970526     C                   ENDSR
032700970526     C*------------------------------------------------------------------------*
032800970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
032900970526     C*------------------------------------------------------------------------*
033000970526     C     O02GES        BEGSR
033100970526     C*
033200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
033300970526IF  1C     VIDATB        IFEQ      'A'
033400970526     C                   MOVEL     '1'           ERRL00
033500970526     C                   MOVEL     MSG(4)        DSMSMS
033600970526E   1C                   ENDIF
033700970526     C* MODIFICA
033800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033900970602     C                   Z-ADD     VIHDDE        DDRI
034000970602     C                   Z-ADD     VIDVER        DVER
034100970529     C                   MOVEL     DEPOP0        WOP0
034200970526     C                   MOVEL     'O02'         WOP1
034300970526     C                   MOVEL     'TISI47R'     WPGM
034400970526     C                   EXSR      LANPGM
034500970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
034600970526E   1C                   ENDIF
034700970526     C*
034800970526     C                   ENDSR
034900970603     C*------------------------------------------------------------------------*
035000970723     C* O03GES - GESTIONE OPZIONE 3=DUPLICA CAPPARIO
035100970603     C*------------------------------------------------------------------------*
035200970603     C     O03GES        BEGSR
035300970603     C*
035400970603     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
035500970603IF  1C     VIDATB        IFEQ      'A'
035600970603     C                   MOVEL     '1'           ERRL00
035700970603     C                   MOVEL     MSG(4)        DSMSMS
035800970603E   1C                   ENDIF
035900970603     C* DUPLICA
036000970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036100970603     C                   Z-ADD     VIHDDE        DDRI
036200970603     C                   Z-ADD     VIDVER        DVER
036300970603     C                   MOVEL     DEPOP0        WOP0
036400970603     C                   MOVEL     'O02'         WOP1
036500970603     C                   MOVEL     'TISI48R'     WPGM
036600970603     C                   EXSR      LANPGM
036700970603     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
036800970603E   1C                   ENDIF
036900970603     C*
037000970603     C                   ENDSR
037100970526     C*------------------------------------------------------------------------*
037200970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
037300970526     C*------------------------------------------------------------------------*
037400970526     C     O04GES        BEGSR
037500970526     C*
037600970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
037700970526IF  1C     VIDATB        IFEQ      'A'
037800970526     C                   MOVEL     '1'           ERRL00
037900970526     C                   MOVEL     MSG(4)        DSMSMS
038000970526E   1C                   ENDIF
038100970526     C* ANNULLAMENTO
038200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038300970602     C                   Z-ADD     VIHDDE        DDRI
038400970602     C                   Z-ADD     VIDVER        DVER
038500970529     C                   MOVEL     DEPOP0        WOP0
038600970526     C                   MOVEL     'O04'         WOP1
038700970526     C                   MOVEL     'TISI47R'     WPGM
038800970526     C                   EXSR      LANPGM
038900970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
039000970526E   1C                   ENDIF
039100970526     C*
039200970526     C                   ENDSR
039300970526     C*------------------------------------------------------------------------*
039400970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
039500970526     C*------------------------------------------------------------------------*
039600970526     C     O05GES        BEGSR
039700970526     C*
039800970602     C                   Z-ADD     VIHDDE        DDRI
039900970602     C                   Z-ADD     VIDVER        DVER
040000970529     C                   MOVEL     DEPOP0        WOP0
040100970526     C                   MOVEL     'O05'         WOP1
040200970526     C                   MOVEL     'TISI47R'     WPGM
040300970526     C                   EXSR      LANPGM
040400970526     C*
040500970526     C                   ENDSR
040600970526     C*------------------------------------------------------------------------*
040700970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
040800970526     C*------------------------------------------------------------------------*
040900970526     C     O07GES        BEGSR
041000970526     C*
041100970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
041200970526IF  1C     VIDATB        IFNE      'A'
041300970526     C                   MOVEL     '1'           ERRL00
041400970526     C                   MOVEL     MSG(5)        DSMSMS
041500970526E   1C                   ENDIF
041600970526     C* RIPRISITINO
041700970528IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041800970602     C                   Z-ADD     VIHDDE        DDRI
041900970602     C                   Z-ADD     VIDVER        DVER
042000970529     C                   MOVEL     DEPOP0        WOP0
042100970526     C                   MOVEL     'O07'         WOP1
042200970526     C                   MOVEL     'TISI47R'     WPGM
042300970526     C                   EXSR      LANPGM
042400970526E   1C                   ENDIF
042500970526     C*
042600970526     C                   ENDSR
042700970528     C*------------------------------------------------------------------------*
042800970528     C* O08GES - GESTIONE OPZIONE 8=LOCALITA'
042900970528     C*------------------------------------------------------------------------*
043000970528     C     O08GES        BEGSR
043100970528     C*
043200970602     C                   Z-ADD     VIHDDE        DDRI
043300970602     C                   Z-ADD     VIDVER        DVER
043400970528     C                   MOVEL     DEPOP0        WOP0
043500970528     C                   MOVEL     'L'           WOP0
043600970528     C                   MOVEL     'O08'         WOP1
043700970528     C                   MOVEL     'TISI30R'     WPGM
043800970528     C                   EXSR      LANPGM
043900970528     C*
044000970528     C                   ENDSR
044100970528     C*------------------------------------------------------------------------*
044200970528     C* O09GES - GESTIONE OPZIONE 9=ECCEZIONI
044300970528     C*------------------------------------------------------------------------*
044400970528     C     O09GES        BEGSR
044500970528     C*
044600970602     C                   Z-ADD     VIHDDE        DDRI
044700970602     C                   Z-ADD     VIDVER        DVER
044800970528     C                   MOVEL     DEPOP0        WOP0
044900970528     C                   MOVEL     'E'           WOP0
045000970528     C                   MOVEL     'O09'         WOP1
045100970528     C                   MOVEL     'TISI11R'     WPGM
045200970528     C                   EXSR      LANPGM
045300970528     C*
045400970528     C                   ENDSR
045500970528     C*------------------------------------------------------------------------*
045600970528     C* O10GES - GESTIONE OPZIONE 10=TEMPI DI CONSEGNA
045700970528     C*------------------------------------------------------------------------*
045800970528     C     O10GES        BEGSR
045900970528     C*
046000970602     C                   Z-ADD     VIHDDE        DDRI
046100970602     C                   Z-ADD     VIDVER        DVER
046200970528     C                   MOVEL     DEPOP0        WOP0
046300970528     C                   MOVEL     'T'           WOP0
046400970528     C                   MOVEL     'O10'         WOP1
046500970528     C                   MOVEL     'TISI41R'     WPGM
046600970528     C                   EXSR      LANPGM
046700970528     C*
046800970528     C                   ENDSR
046900970528     C*------------------------------------------------------------------------*
047000970528     C* O11GES - GESTIONE OPZIONE 11=CAP
047100970528     C*------------------------------------------------------------------------*
047200970528     C     O11GES        BEGSR
047300970528     C*
047400970602     C                   Z-ADD     VIHDDE        DDRI
047500970602     C                   Z-ADD     VIDVER        DVER
047600970528     C                   MOVEL     DEPOP0        WOP0
047700970528     C                   MOVEL     'C'           WOP0
047800970528     C                   MOVEL     'O11'         WOP1
047900970528     C                   MOVEL     'TISI02R'     WPGM
048000970528     C                   EXSR      LANPGM
048100970528     C*
048200970528     C                   ENDSR
048300970603     C*------------------------------------------------------------------------*
048400970603     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
048500970603     C*------------------------------------------------------------------------*
048600970603     C     O13GES        BEGSR
048700970603     C*
048800970603     C                   Z-ADD     VIHDDE        DDRI
048900970603     C                   Z-ADD     VIDVER        DVER
049000970603     C                   MOVEL     DEPOP0        WOP0
049100970603     C                   MOVEL     'S'           WOP0
049200970603     C                   MOVEL     'O13'         WOP1
049300970603     C                   MOVEL     'TISI35R'     WPGM
049400970603     C                   EXSR      LANPGM
049500970603     C*
049600970603     C                   ENDSR
049700970526     C*------------------------------------------------------------------------*
049800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
049900970526     C*------------------------------------------------------------------------*
050000970526     C     NEWPOS        BEGSR
050100970526     C*
050200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
050300970526     C*         1= RICHIESTO POSIZIONAMENTO
050400970526     C                   Z-ADD     0             SWPOSI            7 0
050500970526     C*
050600970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
050700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
050800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
050900970526     C                   Z-ADD     0             SWMODI            1 0
051000970526     C*
051100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
051200970526IF  1C     VIDPVE        IFNE      DEPVER
051300970526     C     VIDPDD        ORNE      DEPDDE
051400970526     C     VIDPDR        ORNE      DEPDRI
051500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
051600970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
051700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
051800970526     C                   Z-ADD     VIDPVE        DEPVER                         *DEPOSITI
051900970526     C                   Z-ADD     VIDPDD        DEPDDE
052000970526     C                   Z-ADD     VIDPDR        DEPDRI
052100970526E   2C                   ENDIF
052200970526E   1C                   ENDIF
052300970526     C*
052400970526     C                   ENDSR
052500970526     C*------------------------------------------------------------------------*
052600970526     C* SETFIL - POSIZIONAMENTO RECORD
052700970526     C*------------------------------------------------------------------------*
052800970526     C     SETFIL        BEGSR
052900970526     C*
053000970526     C* POSIZIONAMENTO DEL FILE E PRIMA LETTURA IN BASE AL TIPO DI RICERCA
053100970526IF  1C     WTIPRI        IFEQ      'VER'                                        *PER VERSIONE
053200970526     C                   Z-ADD     VIDPVE        KCRVER
053300970527     C     KEYCPR        SETLL     AZCPR01L                           99
053400970526E   1C                   ENDIF
053500970526     C*
053600970526IF  1C     WTIPRI        IFEQ      'DEC'                                        *PER DECORRENZA
053700970526     C                   Z-ADD     PDDVID        KCRDDE
053800970527     C     KE2CPR        SETLL     AZCPR02L                           99
053900970526E   1C                   ENDIF
054000970526     C* LETTURA RECORD
054100970526IF  1C     *IN99         IFEQ      *OFF
054200970526     C                   EXSR      LETFIL
054300970526E   1C                   ENDIF
054400970526     C*
054500970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
054600970526IF  1C     *IN99         IFEQ      *ON
054700970526     C                   Z-ADD     1             SFLEOF
054800970526E   1C                   ENDIF
054900970526     C*
055000970526     C                   ENDSR
055100970526     C*------------------------------------------------------------------------*
055200970526     C* LETFIL - LETTURA RECORD
055300970526     C*------------------------------------------------------------------------*
055400970526     C     LETFIL        BEGSR
055500970526     C*
055600970526     C* LEGGE FINO A:
055700970526     C*     - FINE FILE
055800970526     C*     - TROVATO RECORD VALIDO
055900970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
056000970526DO  1C     *IN99         DOWEQ     *OFF
056100970526     C     WRECOK        ANDEQ     'N'
056200970526     C*
056300970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
056400970526IF  2C     WTIPRI        IFEQ      'VER'                                        *PER VERSIONE
056500970526     C                   READ      AZCPR01L                               99
056600970526E   2C                   ENDIF
056700970526IF  2C     WTIPRI        IFEQ      'DEC'                                        *PER DECORRENZA
056800970526     C                   READ      AZCPR02L                               99
056900970526E   2C                   ENDIF
057000970526     C*
057100970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
057200970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
057300970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
057400970530X   2C                   ELSE
057500970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
057600970530E   2C                   ENDIF
057700970526E   1C                   ENDDO
057800970526     C*
057900970526     C                   ENDSR
058000970526     C*------------------------------------------------------------------------*
058100970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
058200970526     C*------------------------------------------------------------------------*
058300970526     C     CHKREC        BEGSR
058400970526     C*
058500970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
058600970526     C*
058700970526     C* RECORD CON DECORRENZA MINORE DEL GIORNO RICHIESTO, ESCLUDE
058800970526IF  1C     PDRVID        IFGT      *ZEROS
058900970526IF  2C     PDRVID        IFGE      CPRDDE
059000970526     C     PDRVID        ANDLE     CPRDSC
059100970526X   2C                   ELSE
059200970526     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
059300970526E   2C                   ENDIF
059400970526E   1C                   ENDIF
059500970526     C*
059600970526     C                   ENDSR
059700970526     C*------------------------------------------------------------------------*
059800970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
059900970526     C*------------------------------------------------------------------------*
060000970526     C     IRESFL        BEGSR
060100970526     C*
060200970526     C                   MOVEL     *BLANKS       OPZKEY
060300970528     C                   Z-ADD     CPRDDE        VIHDDE
060400970526     C                   MOVEL     CPRATB        VIDATB
060500970526     C                   Z-ADD     CPRVER        VIDVER
060600970526     C                   Z-ADD     CPRDDE        G08INV
060700970526     C                   Z-ADD     *ZEROS        G08DAT
060800970526     C                   MOVEL     '3'           G08ERR
060900970526     C                   CALL      'XSRDA8'
061000970526     C                   PARM                    WLBDA8
061100970526     C                   Z-ADD     G08DAT        VIDDDE
061200970526     C                   Z-ADD     CPRDSC        G08INV
061300970526     C                   Z-ADD     *ZEROS        G08DAT
061400970526     C                   MOVEL     '3'           G08ERR
061500970526     C                   CALL      'XSRDA8'
061600970526     C                   PARM                    WLBDA8
061700970526     C                   Z-ADD     G08DAT        VIDDSC
061800970526     C                   MOVEL     CPRDES        VIDDES
061900970526     C*
062000970526     C                   ENDSR
062100970526     C*------------------------------------------------------------------------*
062200970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
062300970526     C*------------------------------------------------------------------------*
062400970526     C     CHKPAR        BEGSR
062500970526     C*
062600970526     C* REIMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
062700970526     C                   SETOFF                                       808182
062800970526     C*--------------------
062900970526     C* ORDINAMENTO
063000970526     C*--------------------
063100970526     C* DATA DECORRENZA
063200970526     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
063300970526     C                   Z-ADD     *ZEROS        PDDVID
063400970526IF  1C     ERRL00        IFEQ      '0'
063500970526IF  2C     VIDPDD        IFGT      *ZEROS
063600970526     C                   Z-ADD     VIDPDD        G08DAT
063700970526     C                   Z-ADD     *ZEROS        G08INV
063800970526     C                   MOVEL     '0'           G08ERR
063900970526     C                   CALL      'XSRDA8'
064000970526     C                   PARM                    WLBDA8
064100970526     C                   Z-ADD     G08DAT        VIDPDD
064200970526     C                   Z-ADD     G08INV        PDDVID
064300970526IF  3C     G08ERR        IFEQ      '1'
064400970526     C                   MOVEL     '1'           ERRL00
064500970526     C                   MOVEL     MSG(8)        DSMSMS
064600970526     C                   SETON                                        81
064700970526E   3C                   ENDIF
064800970526E   2C                   ENDIF
064900970526E   1C                   ENDIF
065000970526     C*--------------------
065100970526     C* SELEZIONI
065200970526     C*--------------------
065300970526     C* DATA RIFERIMENTO
065400970526     C     *LIKE         DEFINE    VIDPDR        PDRVID                         *DATA IN A/M/G
065500970526     C                   Z-ADD     *ZEROS        PDRVID
065600970526IF  1C     ERRL00        IFEQ      '0'
065700970526IF  2C     VIDPDR        IFGT      *ZEROS
065800970526     C                   Z-ADD     VIDPDR        G08DAT
065900970526     C                   Z-ADD     *ZEROS        G08INV
066000970526     C                   MOVEL     '0'           G08ERR
066100970526     C                   CALL      'XSRDA8'
066200970526     C                   PARM                    WLBDA8
066300970526     C                   Z-ADD     G08DAT        VIDPDR
066400970526     C                   Z-ADD     G08INV        PDRVID
066500970526IF  3C     G08ERR        IFEQ      '1'
066600970526     C                   MOVEL     '1'           ERRL00
066700970526     C                   MOVEL     MSG(8)        DSMSMS
066800970526     C                   SETON                                        82
066900970526E   3C                   ENDIF
067000970526E   2C                   ENDIF
067100970526E   1C                   ENDIF
067200970526     C*
067300970526     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER DECORRENZA
067400970526IF  1C     VIDPVE        IFEQ      *ZEROS
067500970526     C     VIDPDD        ANDEQ     *ZEROS
067600970526     C                   MOVEL     'DEC'         WTIPRI            3
067700970526E   1C                   ENDIF
067800970526     C*
067900970526     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
068000970526IF  1C     ERRL00        IFEQ      '0'
068100970526IF  2C     VIDPVE        IFGT      *ZEROS
068200970526     C     VIDPDD        ANDGT     *ZEROS
068300970526     C                   MOVEL     MSG(6)        DSMSMS
068400970526     C                   MOVEL     '1'           ERRL00
068500970526     C                   SETON                                        8081
068600970526E   2C                   ENDIF
068700970526E   1C                   ENDIF
068800970526     C*
068900970526     C* CONTROLLA IL TIPO RICERCA
069000970526IF  1C     ERRL00        IFEQ      '0'
069100970526IF  2C     VIDPVE        IFGT      *ZEROS
069200970526     C                   MOVEL     'VER'         WTIPRI
069300970526E   2C                   ENDIF
069400970526IF  2C     VIDPDD        IFNE      *ZEROS
069500970526     C                   MOVEL     'DEC'         WTIPRI
069600970526E   2C                   ENDIF
069700970526E   1C                   ENDIF
069800970526     C*
069900970526     C                   ENDSR
070000970526     C*------------------------------------------------------------------------*
070100970526     C* LANPGM - LANCIA UN PROGRAMMA
070200970526     C*------------------------------------------------------------------------*
070300970526     C     LANPGM        BEGSR
070400970526     C*
070500970526     C* IMPOSTA LA DS DI PROCEDURA
070600970528     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
070700970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
070800970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
070900970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
071000970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
071100970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
071200970602     C                   Z-ADD     DDRI          D00DRI                         *DATA RIFERIMENTO
071300970602     C                   Z-ADD     DVER          D00VER                         *VERSIONE
071400970530     C                   Z-ADD     *ZEROS        D00DDE                         *ALTRI CAMPI
071500970530     C                   MOVEL     *BLANKS       D00NAR
071600970530     C                   MOVEL     *BLANKS       D00CAP
071700970530     C                   Z-ADD     *ZEROS        D00TTC
071800970530     C                   MOVEL     *BLANKS       D00TSP
071900970530     C                   Z-ADD     *ZEROS        D00TFP
072000970530     C                   MOVEL     *BLANKS       D00LOC
072100970603     C                   MOVEL     *BLANKS       D00SIN
072200970530     C                   MOVEL     *BLANKS       D00EPA
072300970530     C                   Z-ADD     *ZEROS        D00ETP
072400970530     C                   Z-ADD     *ZEROS        D00ETA
072500970530     C                   Z-ADD     *ZEROS        D00EDD
072600970530     C                   MOVEL     *BLANKS       D00SPT
072700970530     C                   Z-ADD     *ZEROS        D00SLP
072800970530     C                   Z-ADD     *ZEROS        D00SLA
072900970530     C                   MOVEL     *BLANKS       D00STS
073000970530     C                   Z-ADD     *ZEROS        D00SDD
073100970526     C*
073200970526     C                   CALL      WPGM
073300970612     C                   PARM                    KPJBA
073400970612     C                   PARM                    DSSI00
073500970526     C*
073600970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
073700970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
073800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
073900970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
074000970526X   1C                   ELSE                                                   *NON PREMUTO F03
074100970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
074200970526E   1C                   ENDIF
074300970526     C*
074400970526     C                   ENDSR
074500970527     C*------------------------------------------------------------------------*
074600970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
074700970527     C*------------------------------------------------------------------------*
074800970527     C     CARTAB        BEGSR
074900970527     C*
075000970527     C                   ENDSR
075100970526     C*------------------------------------------------------------------------*
075200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
075300970526     C*------------------------------------------------------------------------*
075400970526     C     CHKFOP        BEGSR
075500970626     C*
075600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
075700970626IF  1C     DEPOP0        IFEQ      'R01'
075800970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
075900970626     C                   MOVEL     'S'           OPDATT
076000970626     C                   MOVEL     OPDRIG        OPD(1)
076100970626X   1C                   ELSE
076200970626     C* OPZIONI
076300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
076400970626     C                   MOVEL     'S'           OPDATT
076500970626     C                   MOVEL     OPDRIG        OPD(5)
076600970626     C                   MOVEL     OPD(8)        OPDRIG                         *LOCALITA'
076700970626     C                   MOVEL     'S'           OPDATT
076800970626     C                   MOVEL     OPDRIG        OPD(8)
076900970626     C                   MOVEL     OPD(9)        OPDRIG                         *ECCEZIONI
077000970626     C                   MOVEL     'S'           OPDATT
077100970626     C                   MOVEL     OPDRIG        OPD(9)
077200970626     C                   MOVEL     OPD(10)       OPDRIG                         *TEMPI CONSEGNA
077300970626     C                   MOVEL     'S'           OPDATT
077400970626     C                   MOVEL     OPDRIG        OPD(10)
077500970626     C                   MOVEL     OPD(11)       OPDRIG                         *CAP
077600970626     C                   MOVEL     'S'           OPDATT
077700970626     C                   MOVEL     OPDRIG        OPD(11)
077800970626     C                   MOVEL     OPD(13)       OPDRIG                         *SINONIMI
077900970626     C                   MOVEL     'S'           OPDATT
078000970626     C                   MOVEL     OPDRIG        OPD(13)
078100970626     C* FUNZIONI
078200970626     C                   MOVEL     FUD(8)        FUDRIG                         *LOCALITA'
078300970626     C                   MOVEL     'S'           FUDATT
078400970626     C                   MOVEL     FUDRIG        FUD(8)
078500970626     C                   MOVEL     FUD(9)        FUDRIG                         *ECCEZIONI
078600970626     C                   MOVEL     'S'           FUDATT
078700970626     C                   MOVEL     FUDRIG        FUD(9)
078800970626     C                   MOVEL     FUD(10)       FUDRIG                         *TEMPI CONSEGNA
078900970626     C                   MOVEL     'S'           FUDATT
079000970626     C                   MOVEL     FUDRIG        FUD(10)
079100970626     C                   MOVEL     FUD(11)       FUDRIG                         *CAP
079200970626     C                   MOVEL     'S'           FUDATT
079300970626     C                   MOVEL     FUDRIG        FUD(11)
079400970626     C                   MOVEL     FUD(13)       FUDRIG                         *SINONIMI
079500970626     C                   MOVEL     'S'           FUDATT
079600970626     C                   MOVEL     FUDRIG        FUD(13)
079700970626E   1C                   ENDIF
079800970626     C*
079900000602     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
080000970626IF  1C     DEPOP0        IFEQ      'R02'
080100970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
080200970626     C                   MOVEL     'S'           OPDATT
080300970626     C                   MOVEL     OPDRIG        OPD(2)
080400970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
080500970626     C                   MOVEL     'S'           OPDATT
080600970626     C                   MOVEL     OPDRIG        OPD(4)
080700010702     C*                    MOVELOPD,6     OPDRIG           *ALLINEAMENTO
080800010702     C*                    MOVEL'S'       OPDATT
080900010702     C*                    MOVELOPDRIG    OPD,6
081000970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
081100970626     C                   MOVEL     'S'           OPDATT
081200970626     C                   MOVEL     OPDRIG        OPD(7)
081300970715     C*
081400970715     C* SOLO SE EDP ABILITA DUPLICA E INSERIMENTO
081500970715IF  2C     UTENTE        IFEQ      'EDP'
081600970715     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
081700970715     C                   MOVEL     'S'           OPDATT
081800970715     C                   MOVEL     OPDRIG        OPD(3)
081900970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
082000970626     C                   MOVEL     'S'           FUDATT
082100970626     C                   MOVEL     FUDRIG        FUD(6)
082200970715E   2C                   ENDIF
082300970715E   1C                   ENDIF
082400970626     C*
082500000602     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO, ALLINEAMENTO
082600020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
082700970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
082800970626     C                   MOVEL     'N'           OPDATT
082900970626     C                   MOVEL     OPDRIG        OPD(4)
083000010702     C*                    MOVELOPD,6     OPDRIG           *NO ALLINEAMENTO
083100010702     C*                    MOVEL'N'       OPDATT
083200010702     C*                    MOVELOPDRIG    OPD,6
083300970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
083400970626     C                   MOVEL     'N'           OPDATT
083500970626     C                   MOVEL     OPDRIG        OPD(7)
083600970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
083700970626     C                   MOVEL     'N'           FUDATT
083800970626     C                   MOVEL     FUDRIG        FUD(6)
083900970626E   1C                   ENDIF
084000970526     C*
084100970526     C* GESTIONE OPZIONI A VIDEO
084200970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
084300970526     C                   EXSR      OPZGES
084400970526     C*
084500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
084600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
084700970526     C                   EXSR      FUNGES
084800970526     C*
084900970526     C                   ENDSR
085000020919     C*--------------------------------------------------------------------------------------------*
085100020919     C* REPERISCE I DATI UTENTE
085200020919     C*--------------------------------------------------------------------------------------------*
085300020919     C     REPDATIUTE    BEGSR
085400020919     C*
085500020919     C* INIZIALIZZA VARIABILI DI WRK
085600020919     C                   CLEAR                   TIBS34DS
085700020919     C                   CLEAR                   AZUTEDS
085800020919     C                   CLEAR                   DDATIUTE
085900020919     C*
086000020919     C     *DTAARA       DEFINE    §azute        azuteds
086100020919     C     *DTAARA       DEFINE    §datiute      ddatiute
086200020919     C                   IN(E)     *DTAARA
086300020919if  1C                   IF        %Error
086400020919     c                   EVAL      I34Tla = 'L'
086500020919     C                   CALL      'TIBS34R'
086600020919     C                   PARM                    Tibs34Ds
086700020919     C                   IN        *DTAARA
086800020919e   1C                   ENDIF
086900020919     C*
087000020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
087100020919     C                   MOVEL(P)  RSUT          DSFIRS           20
087200020919     C*
087300020919     C                   ENDSR
087400970526     C*------------------------------------------------------------------------*
087500970526     C* *INZSR - OPERAZIONI INIZIALI
087600970526     C*------------------------------------------------------------------------*
087700970526     C     *INZSR        BEGSR
087800970526     C*---
087900970526     C* RICEVIMENTO PARAMETRI
088000970526     C*---
088100970526     C     *ENTRY        PLIST
088200970612     C                   PARM                    KPJBA
088300970612     C                   PARM                    DSSI00
088400970526     C*
088500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
088600970526     C     XMSGPA        PLIST
088700970526     C                   PARM                    DSMSMS
088800970526     C                   PARM                    DSMSPG
088900970526     C                   PARM                    DSMSKE
089000970526     C                   PARM                    DSMSER
089100970526     C*
089200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
089300970526     C                   CALL      'XKEY'
089400970526     C                   PARM                    DSKEY
089500970526     C*---
089600970526     C* VARIABILI NON RIFERITE AL DATA BASE
089700970526     C*---
089800970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
089900970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
090000970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
090100970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
090200970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
090300970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
090400970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
090500970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
090600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
090700970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
090800970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
090900970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
091000970529     C     *LIKE         DEFINE    D00OP1        WOP1
091100970602     C     *LIKE         DEFINE    D00DRI        DDRI
091200970602     C     *LIKE         DEFINE    D00VER        DVER
091300970526     C*---
091400970526     C* VARIABILI DI CONTROLLO
091500970526     C*---
091600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
091700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
091800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
091900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
092000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
092100970526     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
092200970526     C*
092300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
092400970526     C                   MOVEL     'TISI46R'     DSMSPG
092500970526     C                   MOVEL     'TISI46R'     MSGQUE
092600970526     C                   MOVEL     *BLANKS       MSGKEY
092700970526     C                   MOVEL     *BLANKS       DSMSMS
092800970526     C                   MOVEL     *BLANKS       DSMSKE
092900970526     C                   Z-ADD     *ZEROS        DSMSER
093000970526     C*---
093100970526     C* VARIABILI RIFERITE AL DATA BASE
093200970526     C*---
093300970526     C     *LIKE         DEFINE    CPRVER        KCRVER                         *AZCPR00F
093400970526     C     *LIKE         DEFINE    CPRDDE        KCRDDE
093500970526     C     *LIKE         DEFINE    CPRVER        DEPVER                         *DEPOSITI
093600970526     C     *LIKE         DEFINE    CPRDDE        DEPDDE
093700970526     C     *LIKE         DEFINE    CPRDDE        DEPDRI
093800970526     C*---
093900970526     C* CHIAVI DI LETTURA
094000970526     C*---
094100970526     C* LETTURA AZCPR01L - COMPLETA
094200970527     C     KEYCPR        KLIST
094300970526     C                   KFLD                    KCRVER                         *VERSIONE
094400970526     C*
094500970526     C* LETTURA AZCPR02L - PARZIALE
094600970527     C     KE2CPR        KLIST
094700970526     C                   KFLD                    KCRDDE                         *DECORRENZA
094800970527     C*
094900970527     C* REPERIMENTO DATI UTENTE
095000020919     C                   EXSR      REPDATIUTE
095100970527     C*
095200970527     C* CARICA LE TABELLE OCCORRENTI
095300970527     C                   EXSR      CARTAB
095400970526     C*
095500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
095600970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
095700970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
095800970526     C*
095900970526     C* CALCOLA LA DATA CORRENTE
096000970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
096100970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
096200970526     C                   Z-ADD     WN8           G08DAT
096300970526     C                   Z-ADD     *ZEROS        G08INV
096400970526     C                   MOVEL     '0'           G08ERR
096500970526     C                   CALL      'XSRDA8'
096600970526     C                   PARM                    WLBDA8
096700970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
096800970526     C*
096900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
097000970526     C                   EXSR      CHKFOP
097100970526     C*
097200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
097300970528     C                   Z-ADD     *ZEROS        VIDPDR                         *DATA RIFERIMENTO
097400970528IF  1C     D00DRI        IFGT      *ZEROS
097500970526     C                   Z-ADD     D00DRI        G08INV
097600970526     C                   Z-ADD     *ZEROS        G08DAT
097700970526     C                   MOVEL     '3'           G08ERR
097800970526     C                   CALL      'XSRDA8'
097900970526     C                   PARM                    WLBDA8
098000970528     C                   Z-ADD     G08DAT        VIDPDR
098100970528E   1C                   ENDIF
098200970526     C                   EXSR      CHKPAR
098300970526IF  1C     ERRL00        IFEQ      '1'
098400970526     C                   MOVEL     '1'           D00ERR
098500970526     C                   MOVEL     DSMSMS        D00MSG
098600970526X   1C                   ELSE
098700970526     C                   Z-ADD     VIDPDR        DEPDRI                         *DEPOSITI
098800970526E   1C                   ENDIF
098900980225     C*
099000980225     C* REPERISCE L'ULTIMA VERSIONE
099100980225IF  1C     D00VER        IFEQ      *ZEROS
099200980225     C                   Z-ADD     DATCOR        P96DRI            8 0
099300980225     C                   Z-ADD     *ZEROS        P96VER            5 0
099400980225     C                   CALL      'TISI96R'
099500980225     C                   PARM                    P96DRI
099600980225     C                   PARM                    P96VER
099700980225     C                   Z-ADD     P96DRI        ULTDRI            8 0
099800980225     C                   Z-ADD     P96VER        ULTVER            5 0
099900980225E   1C                   ENDIF
100000970526     C*
100100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
100200970530IF  1C     DEPOP0        IFEQ      'R01'
100300970530     C                   MOVEL     DEC(3)        VIDING
100400970530E   1C                   ENDIF
100500970530IF  1C     DEPOP0        IFEQ      'R02'
100600970530     C                   MOVEL     DEC(1)        VIDING
100700970530E   1C                   ENDIF
100800970530IF  1C     DEPOP0        IFEQ      'R05'
100900970530     C                   MOVEL     DEC(2)        VIDING
101000970530E   1C                   ENDIF
101100970526     C*
101200970526     C                   ENDSR
101300970526     C*------------------------------------------------------------------------*
101400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101600970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
101700970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
101800970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
101900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102100970526     C*------------------------------------------------------------------------*
102200970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
102300970526     C*------------------------------------------------------------------------*
102400970526     C     POSSFL        BEGSR
102500970526     C*
102600970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
102700970526     C                   EXSR      CLRSFL
102800970526     C*
102900970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
103000970526     C                   EXSR      SETFIL
103100970526     C*
103200970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
103300970526     C     SFLEOF        IFEQ      0
103400970526     C                   EXSR      CARSFL
103500970526     C                   END
103600970526     C*
103700970526     C                   ENDSR
103800970526     C*------------------------------------------------------------------------*
103900970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
104000970526     C*------------------------------------------------------------------------*
104100970526     C     ENTATT        BEGSR
104200970526     C*
104300970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
104400970526     C                   Z-ADD     0             OPZOK             1 0
104500970526     C*
104600970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
104700970526     C                   Z-ADD     0             SFLVAR            5 0
104800970526     C*
104900970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
105000970526     C                   Z-ADD     0             SFLERR
105100970526     C*
105200970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
105300970526     C                   Z-ADD     SFLULT        WFLULT
105400970526     C*
105500970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
105600970526     C                   READC     SI46D05                                98
105700970526     C     *IN98         DOWEQ     '0'
105800970526     C*
105900970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
106000970526     C                   ADD       1             SFLVAR
106100970526     C*
106200970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
106300970526     C     OPZOK         IFEQ      0
106400970526     C*
106500970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
106600970526     C     OPZKEY        IFNE      *BLANKS
106700970526     C                   EXSR      OPZCON
106800970526     C                   END
106900970526     C*
107000970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
107100970526     C*            2. PULIZIA SCELTA IN SFL
107200970526     C     OPZOK         IFEQ      0
107300970526     C                   MOVEL     '0'           *IN41
107400970526     C                   MOVEL     *BLANKS       OPZKEY
107500970526     C                   ELSE
107600970526     C*
107700970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
107800970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
107900970526     C                   MOVEL     '1'           *IN41
108000970526     C                   Z-ADD     SFLULT        SFLERR
108100970526     C                   END
108200970526     C                   ELSE
108300970526     C*
108400970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
108500970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
108600970526     C                   MOVEL     '1'           *IN41
108700970526     C                   END                                                    OPZOK=0
108800970526     C*
108900970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
109000970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
109100970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
109200970526     C     OPZOK         IFNE      0
109300970526     C                   Z-ADD     SFLERR        SFLNBR
109400970526     C                   ELSE
109500970526     C                   Z-ADD     SFLULT        SFLNBR
109600970526     C                   END
109700970526     C*
109800970526     C* AGGIORNO RECORD SFL
109900970526     C                   UPDATE    SI46D05
110000970526     C*
110100970526     C                   READC     SI46D05                                98
110200970526     C                   END                                                    DO WHILE
110300970526     C*
110400970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
110500970526     C                   Z-ADD     WFLULT        SFLULT
110600970526     C*
110700970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
110800970526     C     SFLVAR        IFEQ      0
110900970526     C*---                 MOVEL'1'       FINL00
111000970526     C                   END
111100970526     C*
111200970526     C                   ENDSR
111300970526     C*------------------------------------------------------------------------*
111400970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
111500970526     C*------------------------------------------------------------------------*
111600970526     C     CLRSFL        BEGSR
111700970526     C*
111800970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
111900970526     C* (0=NON FINE FILE  1=FINE FILE)
112000970526     C                   Z-ADD     0             SFLEOF
112100970526     C*
112200970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
112300970526     C                   Z-ADD     0             SFLULT
112400970526     C*
112500970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
112600970526     C                   MOVEL     '1'           *IN40
112700970526     C                   WRITE     SI46D06
112800970526     C                   MOVEL     '0'           *IN40
112900970526     C*
113000970526     C                   ENDSR
113100970526     C*------------------------------------------------------------------------*
113200970526     C* CARSFL - CARICAMENTO SFL
113300970526     C*------------------------------------------------------------------------*
113400970526     C     CARSFL        BEGSR
113500970526     C*
113600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
113700970526     C     SFLULT        IFEQ      0
113800970526     C                   EXSR      SETFIL
113900970526     C                   END
114000970526     C*
114100970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
114200970526     C                   Z-ADD     SFLULT        WFLULT
114300970526     C*
114400970526     C* CICLO:
114500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
114600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
114700970526     C                   Z-ADD     1             SFLRIG
114800970526     C     SFLRIG        DOWLE     SFLPAG
114900970526     C     SFLEOF        ANDEQ     0
115000970526     C*
115100970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
115200970526     C                   MOVEL     *BLANKS       OPZKEY
115300970526     C*
115400970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
115500970526     C                   EXSR      IRESFL
115600970526     C*
115700970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
115800970526     C                   ADD       1             SFLULT
115900970526     C*
116000970526     C* SCRITTURA RECORD SFL
116100970526     C                   WRITE     SI46D05
116200970526     C                   ADD       1             SFLRIG
116300970526     C*
116400970526     C* LETTURA FILE DA CARICARE
116500970526     C                   EXSR      LETFIL
116600970526     C                   END                                                    DO WHILE
116700970526     C*
116800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
116900970526     C     WFLULT        IFNE      SFLULT
117000970526     C                   Z-ADD     WFLULT        SFLNBR
117100970526     C                   ADD       1             SFLNBR
117200970526     C                   END
117300970526     C*
117400970526     C                   ENDSR
117500970526     C*------------------------------------------------------------------------*
117600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
117700970526     C*------------------------------------------------------------------------*
117800970526     C     OUTSFL        BEGSR
117900970526     C*
118000970526     C* WRITE RECORD TESTATA VIDEO
118100970526     C                   WRITE     SI46D01
118200970526     C*
118300970526     C* VISUALIZZAZIONE MESSAGGIO
118400970526     C                   EXSR      SNDMSG
118500970526     C*
118600970526     C* WRITE RECORD OPZIONI VIDEO
118700970526     C                   WRITE     SI46D02
118800970526     C*
118900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
119000970526     C                   WRITE     SI46D03
119100970526     C*
119200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
119300970526     C                   WRITE     SI46D04
119400970526     C*
119500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
119600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
119700970526     C     SFLEOF        IFEQ      0
119800970526     C                   MOVEL     '0'           *IN42
119900970526     C                   ELSE
120000970526     C*
120100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
120200970526     C                   MOVEL     '1'           *IN42
120300970526     C                   END
120400970526     C*
120500970526     C* WRITE RECORD TASTI FUNZIONALI
120600970526     C                   WRITE     SI46D09
120700970526     C*
120800970526     C* CASO DI SFL VUOTO
120900970526     C     SFLULT        IFLE      0
121000970526     C*
121100970526     C*    - WRITE RECORD VUOTO
121200970526     C                   WRITE     SI46D0A
121300970526     C                   READ      SI46D0A                                99
121400970526     C*
121500970526     C                   ELSE
121600970526     C*
121700970526     C* CASO DI SFL CON RECORDS
121800970526     C*
121900970526     C*    - WRITE RECORD CONTROLLO SFL
122000970526     C                   WRITE     SI46D06
122100970526     C                   READ      SI46D06                                99
122200970526     C                   END
122300970526     C*
122400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
122500970526     C                   READ      SI46D03                                99
122600970526     C*
122700970526     C                   ENDSR
122800970526     C*------------------------------------------------------------------------*
122900970526     C* SNDMSG - MANDA MESSAGGIO
123000970526     C*------------------------------------------------------------------------*
123100970526     C     SNDMSG        BEGSR
123200970526     C*
123300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
123400970526     C                   MOVEL     '1'           *IN40
123500970526     C                   WRITE     SI46DMC
123600970526     C                   MOVEL     '0'           *IN40
123700970526     C*
123800970526     C* VISUALIZZAZIONE MESSAGGIO
123900970526     C                   Z-ADD     0             MSGULT
124000970526     C                   CALL      'XMSG'        XMSGPA
124100970526     C     DSMSER        IFEQ      0
124200970526     C                   MOVEL     DSMSKE        MSGKEY
124300970526     C                   END
124400970526     C                   Z-ADD     1             MSGULT
124500970526     C                   WRITE     SI46DMS                                      SFL MESS
124600970526     C                   WRITE     SI46DMC                                      CONTR MESS
124700970526     C*
124800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
124900970526     C                   MOVEL     *BLANKS       DSMSMS
125000970526     C*
125100970526     C                   ENDSR
125200970526     C*------------------------------------------------------------------------*
125300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
125400970526     C*------------------------------------------------------------------------*
125500970526     C     FUNCON        BEGSR
125600970526     C*
125700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
125800970526     C                   EXSR      FUNVAR
125900970526     C*
126000970526     C* PULISCO CAMPI DI LAVORO
126100970526     C                   Z-ADD     0             I                 5 0
126200970526     C                   Z-ADD     0             W                 5 0
126300970526     C*
126400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
126500970526     C     FUNKEY        IFNE      *BLANKS
126600970526     C     1             DO        24            I
126700970526     C                   MOVEL     FUD(I)        FUDRIG
126800970526     C*
126900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
127000970526     C     FUNKEY        IFEQ      FUDFUN
127100970526     C     FUDATT        ANDEQ     'S'
127200970526     C                   Z-ADD     I             W
127300970526     C                   END
127400970526     C                   END                                                    DO WHILE
127500970526     C*
127600970526     C* FUNZIONE NON ATTIVA (ERRORE)
127700970526     C     W             IFEQ      0
127800970526     C*
127900970526     C* EMISSIONE MESSAGGIO DI ERRORE
128000970526     C                   MOVEL     MSG(2)        DSMSMS
128100970526     C                   END                                                    W=0
128200970526     C                   END                                                    FUNKEY NE *BLANKS
128300970526     C*
128400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
128500970526     C     W             IFNE      0
128600970526     C     FUNKEY        OREQ      *BLANKS
128700970526     C*
128800970526     C     FUNKEY        IFEQ      *BLANKS
128900970526     C                   EXSR      ENTGES
129000970526     C                   END
129100970526     C     FUNKEY        IFEQ      '03'
129200970526     C                   EXSR      F03GES
129300970526     C                   END
129400970526     C     FUNKEY        IFEQ      '05'
129500970526     C                   EXSR      F05GES
129600970526     C                   END
129700970526     C     FUNKEY        IFEQ      '06'
129800970526     C                   EXSR      F06GES
129900970526     C                   END
130000970528     C     FUNKEY        IFEQ      '08'
130100970528     C                   EXSR      F08GES
130200970528     C                   END
130300970528     C     FUNKEY        IFEQ      '09'
130400970528     C                   EXSR      F09GES
130500970528     C                   END
130600970528     C     FUNKEY        IFEQ      '10'
130700970528     C                   EXSR      F10GES
130800970528     C                   END
130900970528     C     FUNKEY        IFEQ      '11'
131000970528     C                   EXSR      F11GES
131100970528     C                   END
131200970526     C     FUNKEY        IFEQ      '12'
131300970526     C                   EXSR      F12GES
131400970526     C                   END
131500970603     C     FUNKEY        IFEQ      '13'
131600970603     C                   EXSR      F13GES
131700970603     C                   END
131800970526     C     FUNKEY        IFEQ      '22'
131900970526     C                   EXSR      F22GES
132000970526     C                   END
132100970526     C     FUNKEY        IFEQ      '23'
132200970526     C                   EXSR      F23GES
132300970526     C                   END
132400970526     C     FUNKEY        IFEQ      '24'
132500970526     C                   EXSR      F24GES
132600970526     C                   END
132700970526     C     FUNKEY        IFEQ      'RU'
132800970526     C                   EXSR      RUPGES
132900970526     C                   END
133000970526     C*
133100970526     C                   END
133200970526     C*
133300970526     C                   ENDSR
133400970526     C*------------------------------------------------------------------------*
133500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
133600970526     C*------------------------------------------------------------------------*
133700970526     C     FUNVAR        BEGSR
133800970526     C*
133900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
134000970526     C     KEY           IFEQ      ENTER
134100970526     C                   MOVEL     *BLANKS       FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F1
134400970526     C                   MOVEL     '01'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F2
134700970526     C                   MOVEL     '02'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F3
135000970526     C                   MOVEL     '03'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F4
135300970526     C                   MOVEL     '04'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      F5
135600970526     C                   MOVEL     '05'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      F6
135900970526     C                   MOVEL     '06'          FUNKEY
136000970526     C                   END
136100970526     C     KEY           IFEQ      F7
136200970526     C                   MOVEL     '07'          FUNKEY
136300970526     C                   END
136400970526     C     KEY           IFEQ      F8
136500970526     C                   MOVEL     '08'          FUNKEY
136600970526     C                   END
136700970526     C     KEY           IFEQ      F9
136800970526     C                   MOVEL     '09'          FUNKEY
136900970526     C                   END
137000970526     C     KEY           IFEQ      F10
137100970526     C                   MOVEL     '10'          FUNKEY
137200970526     C                   END
137300970526     C     KEY           IFEQ      F11
137400970526     C                   MOVEL     '11'          FUNKEY
137500970526     C                   END
137600970526     C     KEY           IFEQ      F12
137700970526     C                   MOVEL     '12'          FUNKEY
137800970526     C                   END
137900970526     C     KEY           IFEQ      F13
138000970526     C                   MOVEL     '13'          FUNKEY
138100970526     C                   END
138200970526     C     KEY           IFEQ      F14
138300970526     C                   MOVEL     '14'          FUNKEY
138400970526     C                   END
138500970526     C     KEY           IFEQ      F15
138600970526     C                   MOVEL     '15'          FUNKEY
138700970526     C                   END
138800970526     C     KEY           IFEQ      F16
138900970526     C                   MOVEL     '16'          FUNKEY
139000970526     C                   END
139100970526     C     KEY           IFEQ      F17
139200970526     C                   MOVEL     '17'          FUNKEY
139300970526     C                   END
139400970526     C     KEY           IFEQ      F18
139500970526     C                   MOVEL     '18'          FUNKEY
139600970526     C                   END
139700970526     C     KEY           IFEQ      F19
139800970526     C                   MOVEL     '19'          FUNKEY
139900970526     C                   END
140000970526     C     KEY           IFEQ      F20
140100970526     C                   MOVEL     '20'          FUNKEY
140200970526     C                   END
140300970526     C     KEY           IFEQ      F21
140400970526     C                   MOVEL     '21'          FUNKEY
140500970526     C                   END
140600970526     C     KEY           IFEQ      F22
140700970526     C                   MOVEL     '22'          FUNKEY
140800970526     C                   END
140900970526     C     KEY           IFEQ      F23
141000970526     C                   MOVEL     '23'          FUNKEY
141100970526     C                   END
141200970526     C     KEY           IFEQ      F24
141300970526     C                   MOVEL     '24'          FUNKEY
141400970526     C                   END
141500970526     C     KEY           IFEQ      ROLLUP
141600970526     C                   MOVEL     'RU'          FUNKEY
141700970526     C                   END
141800970526     C     KEY           IFEQ      HELP
141900970526     C                   MOVEL     'HP'          FUNKEY
142000970526     C                   END
142100970526     C*
142200970526     C                   ENDSR
142300970526     C*------------------------------------------------------------------------*
142400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
142500970526     C*------------------------------------------------------------------------*
142600970526     C     FUNGES        BEGSR
142700970526     C*
142800970526     C* PULISCO CAMPI DI LAVORO
142900970526     C                   Z-ADD     0             W                 5 0
143000970526     C*
143100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
143200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
143300970526     C     1             DO        24            I
143400970526     C                   MOVEL     FUD(I)        FUDRIG
143500970526     C*
143600970526     C* PULIZIA FUNZIONI PRECEDENTI
143700970526     C                   MOVEL     *BLANKS       FUV(I)
143800970526     C*
143900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
144000970526     C* (SALTA F23 E F24)
144100970526     C     FUDATT        IFEQ      'S'
144200970526     C     FUDFUN        ANDNE     '23'
144300970526     C     FUDFUN        ANDNE     '24'
144400970526     C                   ADD       1             W
144500970526     C                   MOVEL     FUDDES        FUV(W)
144600970526     C                   END
144700970526     C                   END                                                    DO WHILE
144800970526     C*
144900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
145000970526     C* ALTRIMENTI DISATTIVO F24
145100970526     C     1             DO        24            I
145200970526     C                   MOVEL     FUD(I)        FUDRIG
145300970526     C*
145400970526     C     FUDFUN        IFEQ      '24'
145500970526     C     W             IFGT      6
145600970526     C                   MOVEL     'S'           FUDATT
145700970526     C                   ELSE
145800970526     C                   MOVEL     *BLANKS       FUDATT
145900970526     C                   END                                                    W GT 8
146000970526     C                   MOVEL     FUDRIG        FUD(I)
146100970526     C                   END
146200970526     C                   END                                                    DO WHILE
146300970526     C*
146400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
146500970526     C     W             DIV       6             I
146600970526     C                   MVR                     Z
146700970526     C*
146800970526     C     Z             IFNE      0
146900970526     C     I             ADD       1             Z
147000970526     C                   ELSE
147100970526     C                   Z-ADD     I             Z
147200970526     C                   END
147300970526     C*
147400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
147500970526     C                   ADD       1             FUNGRU
147600970526     C     FUNGRU        IFGT      Z
147700970526     C     FUNGRU        ORLT      1
147800970526     C                   Z-ADD     1             FUNGRU
147900970526     C                   END
148000970526     C*
148100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
148200970526     C     FUNGRU        IFEQ      1
148300970526     C                   MOVEL     FUV(1)        FUN001
148400970526     C                   MOVEL     FUV(2)        FUN002
148500970526     C                   MOVEL     FUV(3)        FUN003
148600970526     C                   MOVEL     FUV(4)        FUN004
148700970526     C                   MOVEL     FUV(5)        FUN005
148800970526     C                   MOVEL     FUV(6)        FUN006
148900970526     C                   END
149000970526     C*
149100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
149200970526     C     FUNGRU        IFEQ      2
149300970526     C                   MOVEL     FUV(7)        FUN001
149400970526     C                   MOVEL     FUV(8)        FUN002
149500970526     C                   MOVEL     FUV(9)        FUN003
149600970526     C                   MOVEL     FUV(10)       FUN004
149700970526     C                   MOVEL     FUV(11)       FUN005
149800970526     C                   MOVEL     FUV(12)       FUN006
149900970526     C                   END
150000970526     C*
150100970526     C* TERZO GRUPPO DI 6 FUNZIONI
150200970526     C     FUNGRU        IFEQ      3
150300970526     C                   MOVEL     FUV(13)       FUN001
150400970526     C                   MOVEL     FUV(14)       FUN002
150500970526     C                   MOVEL     FUV(15)       FUN003
150600970526     C                   MOVEL     FUV(16)       FUN004
150700970526     C                   MOVEL     FUV(17)       FUN005
150800970526     C                   MOVEL     FUV(18)       FUN006
150900970526     C                   END
151000970526     C*
151100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
151200970526     C     FUNGRU        IFEQ      4
151300970526     C                   MOVEL     FUV(19)       FUN001
151400970526     C                   MOVEL     FUV(20)       FUN002
151500970526     C                   MOVEL     FUV(21)       FUN003
151600970526     C                   MOVEL     FUV(22)       FUN004
151700970526     C                   MOVEL     *BLANKS       FUN005
151800970526     C                   MOVEL     *BLANKS       FUN006
151900970526     C                   END
152000970526     C*
152100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
152200970526     C                   MOVEL     *BLANKS       FUN007
152300970526     C                   MOVEL     *BLANKS       FUN008
152400970526     C*
152500970526     C     1             DO        24            I
152600970526     C                   MOVEL     FUD(I)        FUDRIG
152700970526     C     FUDFUN        IFEQ      '23'
152800970526     C     FUDATT        ANDEQ     'S'
152900970526     C                   MOVEL     FUDDES        FUN007
153000970526     C                   END
153100970526     C     FUDFUN        IFEQ      '24'
153200970526     C     FUDATT        ANDEQ     'S'
153300970526     C                   MOVEL     FUDDES        FUN008
153400970526     C                   END
153500970526     C                   END                                                    DO
153600970526     C*
153700970526     C                   ENDSR
153800970526     C*------------------------------------------------------------------------*
153900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
154000970526     C*------------------------------------------------------------------------*
154100970526     C     OPZGES        BEGSR
154200970526     C*
154300970526     C* PULISCO CAMPI DI LAVORO
154400970526     C                   Z-ADD     0             W                 5 0
154500970526     C*
154600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
154700970526     C* E CARICO TABELLA OPZIONI ATTIVE
154800970526     C     1             DO        24            I
154900970526     C                   MOVEL     OPD(I)        OPDRIG
155000970526     C*
155100970526     C* PULIZIA OPZIONI PRECEDENTI
155200970526     C                   MOVEL     *BLANKS       OPV(I)
155300970526     C*
155400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
155500970526     C     OPDATT        IFEQ      'S'
155600970526     C                   ADD       1             W
155700970526     C                   MOVEL     OPDDES        OPV(W)
155800970526     C                   END
155900970526     C                   END                                                    DO WHILE
156000970526     C*
156100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
156200970526     C* ALTRIMENTI DISATTIVO F23
156300970526     C     1             DO        24            I
156400970526     C                   MOVEL     FUD(I)        FUDRIG
156500970526     C*
156600970526     C     FUDFUN        IFEQ      '23'
156700970526     C     W             IFGT      8
156800970526     C                   MOVEL     'S'           FUDATT
156900970526     C                   ELSE
157000970526     C                   MOVEL     *BLANKS       FUDATT
157100970526     C                   END                                                    W GT 8
157200970526     C                   MOVEL     FUDRIG        FUD(I)
157300970526     C                   END
157400970526     C                   END                                                    DO WHILE
157500970526     C*
157600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
157700970526     C     W             DIV       8             I
157800970526     C                   MVR                     Z                 5 0
157900970526     C*
158000970526     C     Z             IFNE      0
158100970526     C     I             ADD       1             Z
158200970526     C                   ELSE
158300970526     C                   Z-ADD     I             Z
158400970526     C                   END
158500970526     C*
158600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
158700970526     C                   ADD       1             OPZGRU
158800970526     C     OPZGRU        IFGT      Z
158900970526     C     OPZGRU        ORLT      1
159000970526     C                   Z-ADD     1             OPZGRU
159100970526     C                   END
159200970526     C*
159300970526     C* PRIMO GRUPPO DI 8 OPZIONI
159400970526     C     OPZGRU        IFEQ      1
159500970526     C                   MOVEL     OPV(1)        OPZ001
159600970526     C                   MOVEL     OPV(2)        OPZ002
159700970526     C                   MOVEL     OPV(3)        OPZ003
159800970526     C                   MOVEL     OPV(4)        OPZ004
159900970526     C                   MOVEL     OPV(5)        OPZ005
160000970526     C                   MOVEL     OPV(6)        OPZ006
160100970526     C                   MOVEL     OPV(7)        OPZ007
160200970526     C                   MOVEL     OPV(8)        OPZ008
160300970526     C                   END
160400970526     C*
160500970526     C* SECONDO GRUPPO DI 8 OPZIONI
160600970526     C     OPZGRU        IFEQ      2
160700970526     C                   MOVEL     OPV(9)        OPZ001
160800970526     C                   MOVEL     OPV(10)       OPZ002
160900970526     C                   MOVEL     OPV(11)       OPZ003
161000970526     C                   MOVEL     OPV(12)       OPZ004
161100970526     C                   MOVEL     OPV(13)       OPZ005
161200970526     C                   MOVEL     OPV(14)       OPZ006
161300970526     C                   MOVEL     OPV(15)       OPZ007
161400970526     C                   MOVEL     OPV(16)       OPZ008
161500970526     C                   END
161600970526     C*
161700970526     C* TERZO GRUPPO DI 8 OPZIONI
161800970526     C     OPZGRU        IFEQ      3
161900970526     C                   MOVEL     OPV(17)       OPZ001
162000970526     C                   MOVEL     OPV(18)       OPZ002
162100970526     C                   MOVEL     OPV(19)       OPZ003
162200970526     C                   MOVEL     OPV(20)       OPZ004
162300970526     C                   MOVEL     OPV(21)       OPZ005
162400970526     C                   MOVEL     OPV(22)       OPZ006
162500970526     C                   MOVEL     OPV(23)       OPZ007
162600970526     C                   MOVEL     OPV(24)       OPZ008
162700970526     C                   END
162800970526     C*
162900970526     C                   ENDSR
163000970526     C*------------------------------------------------------------------------*
163100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
163200970526     C*------------------------------------------------------------------------*
163300970526     C     OPZCON        BEGSR
163400970526     C*
163500970526     C* PULISCO CAMPI DI LAVORO
163600970526     C                   Z-ADD     0             I                 5 0
163700970526     C                   Z-ADD     0             W                 5 0
163800970526     C*
163900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
164000970526     C     1             DO        24            I
164100970526     C                   MOVEL     OPD(I)        OPDRIG
164200970526     C*
164300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
164400970526     C     OPZKEY        IFEQ      OPDOPZ
164500970526     C     OPZKEY        OREQ      OPDOP1
164600970526     C*
164700970526     C     OPDATT        IFEQ      'S'
164800970526     C                   MOVEL     OPDOPZ        OPZKEY
164900970526     C                   Z-ADD     I             W
165000970526     C                   END
165100970526     C                   END
165200970526     C*
165300970526     C                   END                                                    DO WHILE
165400970526     C*
165500970526     C* OPZIONE NON ATTIVA (ERRORE)
165600970526     C     W             IFEQ      0
165700970526     C*
165800970526     C* OPZIONE NON OK
165900970526     C                   Z-ADD     1             OPZOK
166000970526     C*
166100970526     C* EMISSIONE MESSAGGIO DI ERRORE
166200970526     C                   MOVEL     MSG(1)        DSMSMS
166300970526     C                   ELSE
166400970526     C*
166500970526     C* OPZIONE ATTIVA (PROCEDI ...)
166600970526     C*
166700970526     C     OPZKEY        IFEQ      ' 1'
166800970526     C                   EXSR      O01GES
166900970526     C                   END
167000970526     C     OPZKEY        IFEQ      ' 2'
167100970526     C                   EXSR      O02GES
167200970526     C                   END
167300970603     C     OPZKEY        IFEQ      ' 3'
167400970603     C                   EXSR      O03GES
167500970603     C                   END
167600970526     C     OPZKEY        IFEQ      ' 4'
167700970526     C                   EXSR      O04GES
167800970526     C                   END
167900970526     C     OPZKEY        IFEQ      ' 5'
168000970526     C                   EXSR      O05GES
168100970526     C                   END
168200970526     C     OPZKEY        IFEQ      ' 7'
168300970526     C                   EXSR      O07GES
168400970526     C                   END
168500970528     C     OPZKEY        IFEQ      ' 8'
168600970528     C                   EXSR      O08GES
168700970528     C                   END
168800970528     C     OPZKEY        IFEQ      ' 9'
168900970528     C                   EXSR      O09GES
169000970528     C                   END
169100970528     C     OPZKEY        IFEQ      '10'
169200970528     C                   EXSR      O10GES
169300970528     C                   END
169400970528     C     OPZKEY        IFEQ      '11'
169500970528     C                   EXSR      O11GES
169600970528     C                   END
169700970603     C     OPZKEY        IFEQ      '13'
169800970603     C                   EXSR      O13GES
169900970603     C                   END
170000970526     C                   END                                                    OPZ.NON ATTIVA
170100970526     C*
170200970526     C                   ENDSR
170300970526     C*------------------------------------------------------------------------*
170400970526     C* NOTE COSTRUZIONE SCHIERE
170500970526     C*------------------------------------------------------------------------*
170600970526     C*
170700970526     C* -MSG-MESSAGGI
170800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
170900970526     C* (NON TOCCARE MSG,1 E MSG,2)
171000970526     C*
171100970526     C* -CMD-COMANDI
171200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
171300970526     C*
171400970526     C* -OPD-OPZIONI
171500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
171600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
171700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
171800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
171900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
172000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
172100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
172200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
172300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
172400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
172500970526     C*
172600970526     C* -FUD-FUNZIONI
172700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
172800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
172900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
173000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
173100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
173200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
173300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
173400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
173500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
173600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
173700970526     O*------------------------------------------------------------------------*
173800970526** DEC - DECODIFICHE
173900970526    GESTIONE                                                     1
174000970526INTERROGAZIONE                                                   2
174100970530     SCELTA                                                      3
1742009705264
1743009705265
174400970526** MSG - MESSAGGI
174500970526TISI46R- Opzione non gestita                                                   1
174600970526TISI46R- Tasto funzionale non gestito                                          2
174700970526TISI46R-
174800970526TISI46R- Record annullato. Consentito solo il ripristino                       4
174900970526TISI46R- Record non annullato. Impossibile ripristinare                        5
175000970526TISI46R- Accettato solo un parametro di inizio lista                           6
175100970526TISI46R- Errori nei parametri di ingresso                                      7
175200970526TISI46R- Data errata                                                           8
175300970526TISI46R- 9                                                                     9
175400970526TISI46R- 10                                                                   10
175500970526TISI46R- 11                                                                   11
175600970526TISI46R- 12                                                                   12
175700970526TISI46R- 13                                                                   13
175800970526TISI46R- 14                                                                   14
175900970526TISI46R- 15                                                                   15
176000970526TISI46R- 16                                                                   16
176100970526TISI46R- 17                                                                   17
176200970526TISI46R- 18                                                                   18
176300970526TISI46R- 19                                                                   19
176400970526TISI46R- 20                                                                   20
176500970526** CMD - COMANDI CL
176600970526WRKSPLF
176700970526
176800970526
176900970526
177000970526
177100970526** OPD - OPZIONI
177200970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
177300970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
177400970723 3=Duplica CAPPARIO 3 -     (S/N)=N                        opz. 03¬
177500970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
177600970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
177700020409                   -6 -     (S/N)=N                        opz. 06¬
177800970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
177900970626 8=Località        -8 -     (S/N)=N                        opz. 08¬
178000970626 9=Eccezioni       -9 -     (S/N)=N                        opz. 09¬
17810097062610=Tempi consegna  -10-     (S/N)=N                        opz. 10¬
17820097062611=Cap             -11-     (S/N)=N                        opz. 11¬
178300970526                   -  -     (S/N)=N                        opz. 12¬
17840097062613=Sinonimi        -13-     (S/N)=N                        opz. 13¬
178500970526                   -  -     (S/N)=N                        opz. 14¬
178600970526                   -  -     (S/N)=N                        opz. 15¬
178700970526                   -  -     (S/N)=N                        opz. 16¬
178800970526                   -  -     (S/N)=N                        opz. 17¬
178900970526                   -  -     (S/N)=N                        opz. 18¬
179000970526                   -  -     (S/N)=N                        opz. 19¬
179100970526                   -  -     (S/N)=N                        opz. 20¬
179200970526                   -  -     (S/N)=N                        opz. 21¬
179300970526                   -  -     (S/N)=N                        opz. 22¬
179400970526                   -  -     (S/N)=N                        opz. 23¬
179500970526                   -  -     (S/N)=N                        opz. 24¬
179600970526** FUD - FUNZIONI
179700970526                   -        (S/N)=N                        funz.01¬
179800970526                   -        (S/N)=N                        funz.02¬
179900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
180000970526                   -        (S/N)=N                        funz.04¬
180100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
180200970526F06=Inserimento    -        (S/N)=N                        funz.06¬
180300970526                   -        (S/N)=N                        funz.07¬
180400970626F08=Località       -        (S/N)=N                        funz.08¬
180500970626F09=Eccezioni      -        (S/N)=N                        funz.09¬
180600970626F10=Tempi consegna -        (S/N)=N                        funz.10¬
180700970626F11=Cap            -        (S/N)=N                        funz.11¬
180800970704F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
180900970626F13=Sinonimi       -        (S/N)=N                        funz.13¬
181000970603                   -        (S/N)=S                        funz.14¬
181100970526                   -        (S/N)=N                        funz.15¬
181200970526                   -        (S/N)=N                        funz.16¬
181300970526                   -        (S/N)=N                        funz.17¬
181400970526                   -        (S/N)=N                        funz.18¬
181500970627 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
181600970627 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
181700970526                   -        (S/N)=N                        funz.21¬
181800970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
181900970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
182000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
