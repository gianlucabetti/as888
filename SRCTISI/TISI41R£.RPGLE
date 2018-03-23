000100970526     H*------------------------------------------------------------------------*
000200970526     H* CAPPARIO - GESTIONE TEMPI DI CONSEGNA - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970526     FTISI41D   CF   E             WORKSTN INFDS(DEVDS1)
000900970526     F                                     SFILE(SI41D05:SFLULT)
001000970526     F                                     SFILE(SI41DMS:MSGULT)
001100970527     FTABEL00F  IF   E           K DISK
001200970526     F*------------
001300970526     F* TEMPI DI CONSEGNA
001400970526     F*------------
001500970527     FAZCPT01L  IF   E           K DISK
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
002600970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* DESCRIZIONI OPZIONI
002900970526     D*------------
003000970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* DESCRIZIONI TASTI FUNZIONALI
003300970526     D*------------
003400970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* OPZIONI DA CARICARE A VIDEO
003700970526     D*------------
003800970526     D OPV             S             19    DIM(24)
003900970526     D*------------
004000970526     D* FUNZIONI DA CARICARE A VIDEO
004100970526     D*------------
004200970527     D FUV             S             19    DIM(24)
004300970602     D*------------
004400970602     D* TIPI SERVIZIO
004500970602     D*------------
004600970602     D TSP             S              1    DIM(10)                              *CODICE
004700090605     D TDES            S              3    DIM(10)                              *DESCRIZIONE
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
009100970527     D*------------------
009200970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009300970527     D*------------------
009400970527     D WLBDA8          DS                  INZ
009500970527     D  G08DAT                 1      8  0
009600970527     D  G08INV                 9     16  0
009700970527     D  G08ERR                17     17
009800970527     D  G08TGI                18     22  0
009900970527     D*------------
010000970527     D* DS TIPI SERVIZIO
010100970527     D*------------
010200970527     D DS5E          E DS
010300970703     D*------------
010400970526     D* DS DI PROCEDURA
010500970526     D*------------
010600970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010700970526     D*------------
010800970526     D* ARCHITETTURA
010900970526     D*------------
011000970526     D KPJBA         E DS
011100970526     C*------------------------------------------------------------------------*
011200970526     C* MAIN LINES
011300970526     C*------------------------------------------------------------------------*
011400970526     C*
011500970526     C* SE NON CI SONO ERRORI, PROSEGUE
011600970526IF  1C     ERRL00        IFNE      '1'
011700970526     C*
011800970526     C* CARICA IL SFILE
011900970526     C                   EXSR      POSSFL
012000970526     C*
012100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012500970526     C*
012600970526     C* REIMPOSTA ERRORE DI PROCERURA
012700970526     C                   MOVEL     '0'           ERRL00
012800970526     C*
012900970526     C* EMETTE IL SFILE
013000970526     C                   EXSR      OUTSFL
013100970526     C*
013200970526     C* GESTIONE TASTI FUNZIONALI
013300970526     C                   EXSR      FUNCON
013400970526     C*
013500970526E   2C                   ENDDO
013600970527     C*
013700970527     C* CI SONO ERRORI IN ENTRATA
013800970527X   1C                   ELSE
013900970527     C                   MOVEL     '1'           D00ERR
014000970527     C                   MOVEL     DSMSMS        D00MSG
014100970526E   1C                   ENDIF
014200970526     C* FINE PGM
014300970526     C                   SETON                                        LR
014400970526     C*------------------------------------------------------------------------*
014500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014600970526     C*------------------------------------------------------------------------*
014700970526     C     F03GES        BEGSR
014800970526     C*
014900970526     C* IMPOSTA "PREMUTO F03"
015000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015100970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
015200970526     C*
015300970526     C                   ENDSR
015400970526     C*------------------------------------------------------------------------*
015500970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015600970526     C*------------------------------------------------------------------------*
015700970526     C     F05GES        BEGSR
015800970526     C*
015900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016000970526     C*
016100970526     C                   ENDSR
016200970526     C*------------------------------------------------------------------------*
016300970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016400970526     C*------------------------------------------------------------------------*
016500970526     C     F06GES        BEGSR
016600970526     C*
016700970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
016800020919IF  1C     DUTLPO        IFNE      'S'
016900970526     C                   MOVEL     '1'           ERRL00
017000970526     C                   MOVEL     MSG(3)        DSMSMS
017100970526E   1C                   ENDIF
017200970526     C*
017300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017400970602     C                   Z-ADD     *ZEROS        DTTC
017500970602     C                   MOVEL     *BLANKS       DTSP
017600970602     C                   MOVEL     DEPOP0        WOP0
017700970602     C                   MOVEL     'F06'         WOP1
017800970527     C                   MOVEL     'TISI42R'     WPGM              8
017900970526     C                   EXSR      LANPGM                                       *LANCIA PGM
018000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018100970526E   1C                   ENDIF
018200970526     C*
018300970526     C                   ENDSR
018400970526     C*------------------------------------------------------------------------*
018500970526     C* F12GES - GESTIONE F12 -> RITORNO
018600970526     C*------------------------------------------------------------------------*
018700970526     C     F12GES        BEGSR
018800970526     C*
018900970526     C* IMPOSTA "PREMUTO F12"
019000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019100970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
019200970526     C*
019300970526     C                   ENDSR
019400970526     C*------------------------------------------------------------------------*
019500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
019600970526     C*------------------------------------------------------------------------*
019700970526     C     F23GES        BEGSR
019800970526     C*
019900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
020000970526     C*
020100970526     C                   ENDSR
020200970526     C*------------------------------------------------------------------------*
020300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020400970526     C*------------------------------------------------------------------------*
020500970526     C     F24GES        BEGSR
020600970526     C*
020700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
021200970526     C*------------------------------------------------------------------------*
021300970526     C     RUPGES        BEGSR
021400970526     C*
021500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
021600970526     C*
021700970526     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900970526     C* ENTGES - GESTIONE ENTER -> INVIO
022000970526     C*------------------------------------------------------------------------*
022100970526     C     ENTGES        BEGSR
022200970526     C*
022300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
022400970526     C                   EXSR      NEWPOS
022500970526     C*
022600970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
022700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022800970526IF  2C     SWPOSI        IFNE      0
022900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
023000970526X   2C                   ELSE
023100970526IF  3C     SFLULT        IFNE      0
023200970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
023300970526     C*
023400970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
023500970526IF  4C     SWMODI        IFNE      0
023600970526     C                   EXSR      POSSFL
023700970526E   4C                   ENDIF
023800970526E   3C                   ENDIF
023900970526E   2C                   ENDIF
024000970526E   1C                   ENDIF
024100970526     C*
024200970526     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
024500970526     C*------------------------------------------------------------------------*
024600970526     C     O01GES        BEGSR
024700970526     C*
024800970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
024900970527     C                   Z-ADD     VIDTTC        D00TTC
025000970530     C                   MOVEL     VIDTSP        D00TSP
025100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
025200970526     C*
025300970526     C                   ENDSR
025400970526     C*------------------------------------------------------------------------*
025500970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
025600970526     C*------------------------------------------------------------------------*
025700970526     C     O02GES        BEGSR
025800970526     C*
025900970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
026000970526IF  1C     VIDATB        IFEQ      'A'
026100970526     C                   MOVEL     '1'           ERRL00
026200970526     C                   MOVEL     MSG(4)        DSMSMS
026300970526E   1C                   ENDIF
026400970527     C* MODIFICA
026500970526IF  1C     ERRL00        IFEQ      '0'
026600970602     C                   Z-ADD     VIDTTC        DTTC
026700970602     C                   MOVEL     VIDTSP        DTSP
026800970602     C                   MOVEL     DEPOP0        WOP0
026900970526     C                   MOVEL     'O02'         WOP1
027000970527     C                   MOVEL     'TISI42R'     WPGM
027100970526     C                   EXSR      LANPGM
027200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
027300970526E   1C                   ENDIF
027400970526     C*
027500970526     C                   ENDSR
027600970526     C*------------------------------------------------------------------------*
027700970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
027800970526     C*------------------------------------------------------------------------*
027900970526     C     O04GES        BEGSR
028000970526     C*
028100970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
028200970526IF  1C     VIDATB        IFEQ      'A'
028300970526     C                   MOVEL     '1'           ERRL00
028400970526     C                   MOVEL     MSG(4)        DSMSMS
028500970526E   1C                   ENDIF
028600970527     C* ANNULLAMENTO
028700970526IF  1C     ERRL00        IFEQ      '0'
028800970602     C                   Z-ADD     VIDTTC        DTTC
028900970602     C                   MOVEL     VIDTSP        DTSP
029000970602     C                   MOVEL     DEPOP0        WOP0
029100970526     C                   MOVEL     'O04'         WOP1
029200970527     C                   MOVEL     'TISI42R'     WPGM
029300970526     C                   EXSR      LANPGM
029400970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
029500970526E   1C                   ENDIF
029600970526     C*
029700970526     C                   ENDSR
029800970526     C*------------------------------------------------------------------------*
029900970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
030000970526     C*------------------------------------------------------------------------*
030100970526     C     O05GES        BEGSR
030200970526     C*
030300970602     C                   Z-ADD     VIDTTC        DTTC
030400970602     C                   MOVEL     VIDTSP        DTSP
030500970602     C                   MOVEL     DEPOP0        WOP0
030600970526     C                   MOVEL     'O05'         WOP1
030700970527     C                   MOVEL     'TISI42R'     WPGM
030800970526     C                   EXSR      LANPGM
030900970526     C*
031000970526     C                   ENDSR
031100970526     C*------------------------------------------------------------------------*
031200970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
031300970526     C*------------------------------------------------------------------------*
031400970526     C     O07GES        BEGSR
031500970526     C*
031600970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
031700970526IF  1C     VIDATB        IFNE      'A'
031800970526     C                   MOVEL     '1'           ERRL00
031900970526     C                   MOVEL     MSG(5)        DSMSMS
032000970526E   1C                   ENDIF
032100970526     C* RIPRISITINO
032200970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032300970602     C                   Z-ADD     VIDTTC        DTTC
032400970602     C                   MOVEL     VIDTSP        DTSP
032500970602     C                   MOVEL     DEPOP0        WOP0
032600970526     C                   MOVEL     'O07'         WOP1
032700970527     C                   MOVEL     'TISI42R'     WPGM
032800970526     C                   EXSR      LANPGM
032900970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
033000970526E   1C                   ENDIF
033100970526     C*
033200970526     C                   ENDSR
033300970526     C*------------------------------------------------------------------------*
033400970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
033500970526     C*------------------------------------------------------------------------*
033600970526     C     NEWPOS        BEGSR
033700970526     C*
033800970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
033900970526     C*         1= RICHIESTO POSIZIONAMENTO
034000970526     C                   Z-ADD     0             SWPOSI            7 0
034100970526     C*
034200970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
034300970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
034400970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
034500970526     C                   Z-ADD     0             SWMODI            1 0
034600970530     C*
034700970530     C* REIMPOSTA INDICATORI DI ERRORE
034800970530     C                   SETOFF                                       8081
034900970526     C*
035000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
035100970527IF  1C     VIDPTT        IFNE      DEPTTC
035200970527     C     VIDPTS        ORNE      DEPTSP
035300970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
035400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
035500970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
035600970527     C                   Z-ADD     VIDPTT        DEPTTC                         *DEPOSITI
035700970527     C                   MOVEL     VIDPTS        DEPTSP
035800970526E   2C                   ENDIF
035900970526E   1C                   ENDIF
036000970526     C*
036100970526     C                   ENDSR
036200970526     C*------------------------------------------------------------------------*
036300970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
036400970526     C*------------------------------------------------------------------------*
036500970526     C     SETFIL        BEGSR
036600970526     C*
036700970527     C* POSIZIONAMENTO SUL FILE
036800970527     C                   Z-ADD     D00VER        KCTVER
036900970527     C                   Z-ADD     VIDPTT        KCTTTC
037000970527     C     KEYCPT        SETLL     AZCPT01L                           99
037100970526     C* LETTURA RECORD
037200970526IF  1C     *IN99         IFEQ      *OFF
037300970526     C                   EXSR      LETFIL
037400970526E   1C                   ENDIF
037500970526     C*
037600970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
037700970526IF  1C     *IN99         IFEQ      *ON
037800970526     C                   Z-ADD     1             SFLEOF
037900970526E   1C                   ENDIF
038000970526     C*
038100970526     C                   ENDSR
038200970526     C*------------------------------------------------------------------------*
038300970526     C* LETFIL - LETTURA RECORD
038400970526     C*------------------------------------------------------------------------*
038500970526     C     LETFIL        BEGSR
038600970526     C*
038700970526     C* LEGGE FINO A:
038800970526     C*     - FINE FILE
038900970526     C*     - TROVATO RECORD VALIDO
039000970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
039100970526DO  1C     *IN99         DOWEQ     *OFF
039200970526     C     WRECOK        ANDEQ     'N'
039300970526     C*
039400970527     C* LETTURE SUCCESSIVE ALLA PRIMA
039500970527     C     KCTVER        READE     AZCPT01L                               99
039600970526     C*
039700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
039800970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
039900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
040000970530X   2C                   ELSE
040100970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
040200970530E   2C                   ENDIF
040300970526E   1C                   ENDDO
040400970526     C*
040500970526     C                   ENDSR
040600970526     C*------------------------------------------------------------------------*
040700970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
040800970526     C*------------------------------------------------------------------------*
040900970526     C     CHKREC        BEGSR
041000970526     C*
041100970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
041200970527     C*
041300970527     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
041400970527IF  1C     VIDPTS        IFNE      *BLANKS
041500970527IF  2C     VIDPTS        IFNE      CPTTSP
041600970527     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
041700970527E   2C                   ENDIF
041800970527E   1C                   ENDIF
041900970526     C*
042000970526     C                   ENDSR
042100970526     C*------------------------------------------------------------------------*
042200970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
042300970526     C*------------------------------------------------------------------------*
042400970526     C     IRESFL        BEGSR
042500970526     C*
042600970526     C                   MOVEL     *BLANKS       OPZKEY
042700970527     C                   MOVEL     CPTATB        VIDATB
042800970527     C                   Z-ADD     CPTTTC        VIDTTC
042900970527     C                   MOVEL     CPTDTC        VIDDTC
043000970527     C                   MOVEL     CPTTSP        VIDTSP
043100970527     C                   Z-ADD     CPT0IN        VID0IN
043200970527     C                   Z-ADD     CPT1IN        VID1IN
043300970527     C                   Z-ADD     CPT0CN        VID0CN
043400970527     C                   Z-ADD     CPT1CN        VID1CN
043500970527     C                   Z-ADD     CPTTIS        VIDTIS
043600970527     C                   Z-ADD     CPTTCS        VIDTCS
043700970527     C                   Z-ADD     CPTTLD        VIDTLD
043800970527     C* DECODIFICHE
043900970527     C                   SETOFF                                           28
044000970527     C                   Z-ADD     1             I
044100970527     C     CPTTSP        LOOKUP    TSP(I)                                 28
044200970527IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
044300970602     C                   MOVEL     TDES(I)       VIDDTS
044400970527X   1C                   ELSE                                                   *NON TROVATO
044500970527     C                   MOVEL     *BLANKS       VIDDTS
044600970527E   1C                   ENDIF
044700970526     C*
044800970526     C                   ENDSR
044900970526     C*------------------------------------------------------------------------*
045000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
045100970526     C*------------------------------------------------------------------------*
045200970526     C     CHKPAR        BEGSR
045300970526     C*
045400970526     C                   ENDSR
045500970526     C*------------------------------------------------------------------------*
045600970526     C* LANPGM - LANCIA UN PROGRAMMA
045700970526     C*------------------------------------------------------------------------*
045800970526     C     LANPGM        BEGSR
045900970526     C*
046000970526     C* IMPOSTA LA DS DI PROCEDURA
046100970602     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
046200970602     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
046300970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
046400970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
046500970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
046600970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
046700970602     C                   Z-ADD     DTTC          D00TTC                         *CODICE TEMPO
046800970602     C                   MOVEL     DTSP          D00TSP                         *TIPO SERVIZIO
046900970530     C                   Z-ADD     *ZEROS        D00TFP                         *ALTRI CAMPI
047000970530     C                   MOVEL     *BLANKS       D00LOC
047100970603     C                   MOVEL     *BLANKS       D00SIN
047200970530     C                   MOVEL     *BLANKS       D00EPA
047300970530     C                   Z-ADD     *ZEROS        D00ETP
047400970530     C                   Z-ADD     *ZEROS        D00ETA
047500970530     C                   Z-ADD     *ZEROS        D00EDD
047600970530     C                   MOVEL     *BLANKS       D00SPT
047700970530     C                   Z-ADD     *ZEROS        D00SLP
047800970530     C                   Z-ADD     *ZEROS        D00SLA
047900970530     C                   MOVEL     *BLANKS       D00STS
048000970530     C                   Z-ADD     *ZEROS        D00SDD
048100970526     C*
048200970526     C                   CALL      WPGM
048300970612     C                   PARM                    KPJBA
048400970612     C                   PARM                    DSSI00
048500970526     C*
048600970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
048700970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
048800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
048900970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
049000970526X   1C                   ELSE                                                   *NON PREMUTO F03
049100970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
049200970526E   1C                   ENDIF
049300970526     C*
049400970526     C                   ENDSR
049500970527     C*------------------------------------------------------------------------*
049600970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
049700970527     C*------------------------------------------------------------------------*
049800970527     C     CARTAB        BEGSR
049900970527     C*---
050000970527     C* TIPI SERVIZIO
050100970527     C*---
050200970527     C                   Z-ADD     *ZEROS        I                 5 0
050300970527     C                   MOVEL     *BLANKS       TSP
050400970527     C                   MOVEL     *BLANKS       DTSP
050500020919     C                   MOVEL     1             KTBKUT
050600970527     C                   MOVEL     '5E'          KTBCOD
050700970527     C     KEYTAB        SETLL     TABEL00F
050800970527     C     KEYTAB        READE     TABEL00F                               99
050900970527DO  1C     *IN99         DOWEQ     *OFF
051000970527IF  2C     TBLFLG        IFEQ      *BLANKS
051100970527     C                   ADD       1             I
051200970527     C                   MOVEL     TBLUNI        DS5E
051300970527     C                   MOVEL     TBLKEY        TSP(I)
051400090605     C                   MOVEL     §5ED03        TDES(I)
051500970527E   2C                   ENDIF
051600970527     C     KEYTAB        READE     TABEL00F                               99
051700970527E   1C                   ENDDO                                                  *FINE TABELLA
051800970527     C*
051900970527     C                   ENDSR
052000970526     C*------------------------------------------------------------------------*
052100970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
052200970526     C*------------------------------------------------------------------------*
052300970526     C     CHKFOP        BEGSR
052400970626     C*
052500970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
052600970626IF  1C     DEPOP0        IFEQ      'T01'
052700970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
052800970626     C                   MOVEL     'S'           OPDATT
052900970626     C                   MOVEL     OPDRIG        OPD(1)
053000970626X   1C                   ELSE
053100970626     C* OPZIONI
053200970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
053300970626     C                   MOVEL     'S'           OPDATT
053400970626     C                   MOVEL     OPDRIG        OPD(5)
053500970626     C* FUNZIONI
053600970626E   1C                   ENDIF
053700970626     C*
053800970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
053900970626IF  1C     DEPOP0        IFEQ      'T02'
054000970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
054100970626     C                   MOVEL     'S'           OPDATT
054200970626     C                   MOVEL     OPDRIG        OPD(2)
054300970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
054400970626     C                   MOVEL     'S'           OPDATT
054500970626     C                   MOVEL     OPDRIG        OPD(4)
054600970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
054700970626     C                   MOVEL     'S'           OPDATT
054800970626     C                   MOVEL     OPDRIG        OPD(7)
054900970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
055000970626     C                   MOVEL     'S'           FUDATT
055100970626     C                   MOVEL     FUDRIG        FUD(6)
055200970626E   1C                   ENDIF
055300970626     C*
055400970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
055500020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
055600970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
055700970626     C                   MOVEL     'N'           OPDATT
055800970626     C                   MOVEL     OPDRIG        OPD(4)
055900970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
056000970626     C                   MOVEL     'N'           OPDATT
056100970626     C                   MOVEL     OPDRIG        OPD(7)
056200970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
056300970626     C                   MOVEL     'N'           FUDATT
056400970626     C                   MOVEL     FUDRIG        FUD(6)
056500970626E   1C                   ENDIF
056600970526     C*
056700970526     C* GESTIONE OPZIONI A VIDEO
056800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
056900970526     C                   EXSR      OPZGES
057000970526     C*
057100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
057200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
057300970526     C                   EXSR      FUNGES
057400970526     C*
057500970526     C                   ENDSR
057600020919     C*--------------------------------------------------------------------------------------------*
057700020919     C* REPERISCE I DATI UTENTE
057800020919     C*--------------------------------------------------------------------------------------------*
057900020919     C     REPDATIUTE    BEGSR
058000020919     C*
058100020919     C* INIZIALIZZA VARIABILI DI WRK
058200020919     C                   CLEAR                   TIBS34DS
058300020919     C                   CLEAR                   AZUTEDS
058400020919     C                   CLEAR                   DDATIUTE
058500020919     C*
058600020919     C     *DTAARA       DEFINE    §azute        azuteds
058700020919     C     *DTAARA       DEFINE    §datiute      ddatiute
058800020919     C                   IN(E)     *DTAARA
058900020919if  1C                   IF        %Error
059000020919     c                   EVAL      I34Tla = 'L'
059100020919     C                   CALL      'TIBS34R'
059200020919     C                   PARM                    Tibs34Ds
059300020919     C                   IN        *DTAARA
059400020919e   1C                   ENDIF
059500020919     C*
059600020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
059700020919     C                   MOVEL(P)  RSUT          DSFIRS           20
059800020919     C*
059900020919     C                   ENDSR
060000970526     C*------------------------------------------------------------------------*
060100970526     C* *INZSR - OPERAZIONI INIZIALI
060200970526     C*------------------------------------------------------------------------*
060300970526     C     *INZSR        BEGSR
060400970526     C*---
060500970526     C* RICEVIMENTO PARAMETRI
060600970526     C*---
060700970526     C     *ENTRY        PLIST
060800970612     C                   PARM                    KPJBA
060900970612     C                   PARM                    DSSI00
061000970526     C*
061100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
061200970526     C     XMSGPA        PLIST
061300970526     C                   PARM                    DSMSMS
061400970526     C                   PARM                    DSMSPG
061500970526     C                   PARM                    DSMSKE
061600970526     C                   PARM                    DSMSER
061700970526     C*
061800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
061900970526     C                   CALL      'XKEY'
062000970526     C                   PARM                    DSKEY
062100970526     C*---
062200970526     C* VARIABILI NON RIFERITE AL DATA BASE
062300970526     C*---
062400970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
062500970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
062600970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
062700970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
062800970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
062900970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
063000970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
063100970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
063200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
063300970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
063400970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
063500970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
063600970529     C     *LIKE         DEFINE    D00OP1        WOP1
063700970602     C     *LIKE         DEFINE    D00TTC        DTTC
063800970602     C     *LIKE         DEFINE    D00TSP        DTSP
063900970526     C*---
064000970526     C* VARIABILI DI CONTROLLO
064100970526     C*---
064200970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
064300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
064400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
064500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
064600970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
064700970526     C                   Z-ADD     9             SFLPAG                         *N°RIGHE X PAG
064800970526     C*
064900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
065000970526     C                   MOVEL     'TISI41R'     DSMSPG
065100970526     C                   MOVEL     'TISI41R'     MSGQUE
065200970526     C                   MOVEL     *BLANKS       MSGKEY
065300970526     C                   MOVEL     *BLANKS       DSMSMS
065400970526     C                   MOVEL     *BLANKS       DSMSKE
065500970526     C                   Z-ADD     *ZEROS        DSMSER
065600970526     C*---
065700970526     C* VARIABILI RIFERITE AL DATA BASE
065800970526     C*---
065900970527     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
066000970527     C     *LIKE         DEFINE    TBLCOD        KTBCOD
066100970527     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
066200970527     C     *LIKE         DEFINE    CPTTTC        KCTTTC
066300970527     C     *LIKE         DEFINE    CPTTTC        DEPTTC                         *DEPOSITI
066400970527     C     *LIKE         DEFINE    CPTTSP        DEPTSP
066500970526     C*---
066600970526     C* CHIAVI DI LETTURA
066700970526     C*---
066800970527     C* LETTURA TABEL00F - PARZIALE
066900970527     C     KEYTAB        KLIST
067000970527     C                   KFLD                    KTBKUT                         *UTENTE
067100970527     C                   KFLD                    KTBCOD                         *TABELLA
067200970527     C* LETTURA AZCPT01L - PARZIALE
067300970527     C     KEYCPT        KLIST
067400970527     C                   KFLD                    KCTVER                         *VERSIONE
067500970527     C                   KFLD                    KCTTTC                         *CODICE TEMPO
067600970527     C*
067700970527     C* REPERIMENTO DATI UTENTE
067800020919     C                   EXSR      REPDATIUTE
067900970527     C*
068000970527     C* CARICA LE TABELLE OCCORRENTI
068100970527     C                   EXSR      CARTAB
068200970526     C*
068300970526     C* DEPOSITA LE OPZIONI DI ENTRATA
068400970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
068500970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
068600970526     C*
068700970526     C* CALCOLA LA DATA CORRENTE
068800970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
068900970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
069000970526     C                   Z-ADD     WN8           G08DAT
069100970526     C                   Z-ADD     *ZEROS        G08INV
069200970526     C                   MOVEL     '0'           G08ERR
069300970526     C                   CALL      'XSRDA8'
069400970526     C                   PARM                    WLBDA8
069500970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
069600970526     C*
069700970526     C* IMPOSTA I PARAMETRI DI INGRESSO
069800970526     C                   Z-ADD     D00DRI        G08INV
069900970526     C                   Z-ADD     *ZEROS        G08DAT
070000970526     C                   MOVEL     '3'           G08ERR
070100970526     C                   CALL      'XSRDA8'
070200970526     C                   PARM                    WLBDA8
070300970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
070400970527     C                   Z-ADD     D00TTC        VIDPTT                         *CODICE TEMPO
070500970527     C                   MOVEL     D00TSP        VIDPTS                         *TIPO SERVIZIO
070600970526     C                   EXSR      CHKPAR
070700970526IF  1C     ERRL00        IFEQ      '1'
070800970526     C                   MOVEL     '1'           D00ERR
070900970526     C                   MOVEL     DSMSMS        D00MSG
071000970526X   1C                   ELSE
071100970527     C                   Z-ADD     VIDPTT        DEPTTC                         *DEPOSITI
071200970527     C                   MOVEL     VIDPTS        DEPTSP
071300970526E   1C                   ENDIF
071400970703     C*
071500970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
071600970703IF  1C     D00VER        IFEQ      *ZEROS
071700970707     C                   Z-ADD     D00DRI        P96DRI            8 0
071800970707     C                   Z-ADD     *ZEROS        P96VER            5 0
071900970707     C                   CALL      'TISI96R'
072000970707     C                   PARM                    P96DRI
072100970707     C                   PARM                    P96VER
072200970707     C                   Z-ADD     P96VER        D00VER
072300970703E   1C                   ENDIF
072400970526     C*
072500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
072600970526     C                   EXSR      CHKFOP
072700970530     C*
072800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
072900970530IF  1C     DEPOP0        IFEQ      'T01'
073000970530     C                   MOVEL     DEC(3)        VIDING
073100970530E   1C                   ENDIF
073200970530IF  1C     DEPOP0        IFEQ      'T02'
073300970530     C                   MOVEL     DEC(1)        VIDING
073400970530E   1C                   ENDIF
073500970530IF  1C     DEPOP0        IFEQ      'T05'
073600970530     C                   MOVEL     DEC(2)        VIDING
073700970530E   1C                   ENDIF
073800970526     C*
073900970526     C                   ENDSR
074000970526     C*------------------------------------------------------------------------*
074100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
074400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
074500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
074600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074800970526     C*------------------------------------------------------------------------*
074900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
075000970526     C*------------------------------------------------------------------------*
075100970526     C     POSSFL        BEGSR
075200970526     C*
075300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
075400970526     C                   EXSR      CLRSFL
075500970526     C*
075600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
075700970526     C                   EXSR      SETFIL
075800970526     C*
075900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
076000970526     C     SFLEOF        IFEQ      0
076100970526     C                   EXSR      CARSFL
076200970526     C                   END
076300970526     C*
076400970526     C                   ENDSR
076500970526     C*------------------------------------------------------------------------*
076600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
076700970526     C*------------------------------------------------------------------------*
076800970526     C     ENTATT        BEGSR
076900970526     C*
077000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
077100970526     C                   Z-ADD     0             OPZOK             1 0
077200970526     C*
077300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
077400970526     C                   Z-ADD     0             SFLVAR            5 0
077500970526     C*
077600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
077700970526     C                   Z-ADD     0             SFLERR
077800970526     C*
077900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
078000970526     C                   Z-ADD     SFLULT        WFLULT
078100970526     C*
078200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
078300970526     C                   READC     SI41D05                                98
078400970526     C     *IN98         DOWEQ     '0'
078500970526     C*
078600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
078700970526     C                   ADD       1             SFLVAR
078800970526     C*
078900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
079000970526     C     OPZOK         IFEQ      0
079100970526     C*
079200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
079300970526     C     OPZKEY        IFNE      *BLANKS
079400970526     C                   EXSR      OPZCON
079500970526     C                   END
079600970526     C*
079700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
079800970526     C*            2. PULIZIA SCELTA IN SFL
079900970526     C     OPZOK         IFEQ      0
080000970526     C                   MOVEL     '0'           *IN41
080100970526     C                   MOVEL     *BLANKS       OPZKEY
080200970526     C                   ELSE
080300970526     C*
080400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
080500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
080600970526     C                   MOVEL     '1'           *IN41
080700970526     C                   Z-ADD     SFLULT        SFLERR
080800970526     C                   END
080900970526     C                   ELSE
081000970526     C*
081100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
081200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
081300970526     C                   MOVEL     '1'           *IN41
081400970526     C                   END                                                    OPZOK=0
081500970526     C*
081600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
081700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
081800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
081900970526     C     OPZOK         IFNE      0
082000970526     C                   Z-ADD     SFLERR        SFLNBR
082100970526     C                   ELSE
082200970526     C                   Z-ADD     SFLULT        SFLNBR
082300970526     C                   END
082400970526     C*
082500970526     C* AGGIORNO RECORD SFL
082600970526     C                   UPDATE    SI41D05
082700970526     C*
082800970526     C                   READC     SI41D05                                98
082900970526     C                   END                                                    DO WHILE
083000970526     C*
083100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
083200970526     C                   Z-ADD     WFLULT        SFLULT
083300970526     C*
083400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
083500970526     C     SFLVAR        IFEQ      0
083600970526     C*---                 MOVEL'1'       FINL00
083700970526     C                   END
083800970526     C*
083900970526     C                   ENDSR
084000970526     C*------------------------------------------------------------------------*
084100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
084200970526     C*------------------------------------------------------------------------*
084300970526     C     CLRSFL        BEGSR
084400970526     C*
084500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
084600970526     C* (0=NON FINE FILE  1=FINE FILE)
084700970526     C                   Z-ADD     0             SFLEOF
084800970526     C*
084900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
085000970526     C                   Z-ADD     0             SFLULT
085100970526     C*
085200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
085300970526     C                   MOVEL     '1'           *IN40
085400970526     C                   WRITE     SI41D06
085500970526     C                   MOVEL     '0'           *IN40
085600970526     C*
085700970526     C                   ENDSR
085800970526     C*------------------------------------------------------------------------*
085900970526     C* CARSFL - CARICAMENTO SFL
086000970526     C*------------------------------------------------------------------------*
086100970526     C     CARSFL        BEGSR
086200970526     C*
086300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
086400970526     C     SFLULT        IFEQ      0
086500970526     C                   EXSR      SETFIL
086600970526     C                   END
086700970526     C*
086800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
086900970526     C                   Z-ADD     SFLULT        WFLULT
087000970526     C*
087100970526     C* CICLO:
087200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
087300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
087400970526     C                   Z-ADD     1             SFLRIG
087500970526     C     SFLRIG        DOWLE     SFLPAG
087600970526     C     SFLEOF        ANDEQ     0
087700970526     C*
087800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
087900970526     C                   MOVEL     *BLANKS       OPZKEY
088000970526     C*
088100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
088200970526     C                   EXSR      IRESFL
088300970526     C*
088400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
088500970526     C                   ADD       1             SFLULT
088600970526     C*
088700970526     C* SCRITTURA RECORD SFL
088800970526     C                   WRITE     SI41D05
088900970526     C                   ADD       1             SFLRIG
089000970526     C*
089100970526     C* LETTURA FILE DA CARICARE
089200970526     C                   EXSR      LETFIL
089300970526     C                   END                                                    DO WHILE
089400970526     C*
089500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
089600970526     C     WFLULT        IFNE      SFLULT
089700970526     C                   Z-ADD     WFLULT        SFLNBR
089800970526     C                   ADD       1             SFLNBR
089900970526     C                   END
090000970526     C*
090100970526     C                   ENDSR
090200970526     C*------------------------------------------------------------------------*
090300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
090400970526     C*------------------------------------------------------------------------*
090500970526     C     OUTSFL        BEGSR
090600970526     C*
090700970526     C* WRITE RECORD TESTATA VIDEO
090800970526     C                   WRITE     SI41D01
090900970526     C*
091000970526     C* VISUALIZZAZIONE MESSAGGIO
091100970526     C                   EXSR      SNDMSG
091200970526     C*
091300970526     C* WRITE RECORD OPZIONI VIDEO
091400970526     C                   WRITE     SI41D02
091500970526     C*
091600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
091700970526     C                   WRITE     SI41D03
091800970526     C*
091900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
092000970526     C                   WRITE     SI41D04
092100970526     C*
092200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
092300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
092400970526     C     SFLEOF        IFEQ      0
092500970526     C                   MOVEL     '0'           *IN42
092600970526     C                   ELSE
092700970526     C*
092800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
092900970526     C                   MOVEL     '1'           *IN42
093000970526     C                   END
093100970526     C*
093200970526     C* WRITE RECORD TASTI FUNZIONALI
093300970526     C                   WRITE     SI41D09
093400970526     C*
093500970526     C* CASO DI SFL VUOTO
093600970526     C     SFLULT        IFLE      0
093700970526     C*
093800970526     C*    - WRITE RECORD VUOTO
093900970526     C                   WRITE     SI41D0A
094000970526     C                   READ      SI41D0A                                99
094100970526     C*
094200970526     C                   ELSE
094300970526     C*
094400970526     C* CASO DI SFL CON RECORDS
094500970526     C*
094600970526     C*    - WRITE RECORD CONTROLLO SFL
094700970526     C                   WRITE     SI41D06
094800970526     C                   READ      SI41D06                                99
094900970526     C                   END
095000970526     C*
095100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
095200970526     C                   READ      SI41D03                                99
095300970526     C*
095400970526     C                   ENDSR
095500970526     C*------------------------------------------------------------------------*
095600970526     C* SNDMSG - MANDA MESSAGGIO
095700970526     C*------------------------------------------------------------------------*
095800970526     C     SNDMSG        BEGSR
095900970526     C*
096000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
096100970526     C                   MOVEL     '1'           *IN40
096200970526     C                   WRITE     SI41DMC
096300970526     C                   MOVEL     '0'           *IN40
096400970526     C*
096500970526     C* VISUALIZZAZIONE MESSAGGIO
096600970526     C                   Z-ADD     0             MSGULT
096700970526     C                   CALL      'XMSG'        XMSGPA
096800970526     C     DSMSER        IFEQ      0
096900970526     C                   MOVEL     DSMSKE        MSGKEY
097000970526     C                   END
097100970526     C                   Z-ADD     1             MSGULT
097200970526     C                   WRITE     SI41DMS                                      SFL MESS
097300970526     C                   WRITE     SI41DMC                                      CONTR MESS
097400970526     C*
097500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
097600970526     C                   MOVEL     *BLANKS       DSMSMS
097700970526     C*
097800970526     C                   ENDSR
097900970526     C*------------------------------------------------------------------------*
098000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
098100970526     C*------------------------------------------------------------------------*
098200970526     C     FUNCON        BEGSR
098300970526     C*
098400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
098500970526     C                   EXSR      FUNVAR
098600970526     C*
098700970526     C* PULISCO CAMPI DI LAVORO
098800970526     C                   Z-ADD     0             I                 5 0
098900970526     C                   Z-ADD     0             W                 5 0
099000970526     C*
099100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
099200970526     C     FUNKEY        IFNE      *BLANKS
099300970526     C     1             DO        24            I
099400970526     C                   MOVEL     FUD(I)        FUDRIG
099500970526     C*
099600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
099700970526     C     FUNKEY        IFEQ      FUDFUN
099800970526     C     FUDATT        ANDEQ     'S'
099900970526     C                   Z-ADD     I             W
100000970526     C                   END
100100970526     C                   END                                                    DO WHILE
100200970526     C*
100300970526     C* FUNZIONE NON ATTIVA (ERRORE)
100400970526     C     W             IFEQ      0
100500970526     C*
100600970526     C* EMISSIONE MESSAGGIO DI ERRORE
100700970526     C                   MOVEL     MSG(2)        DSMSMS
100800970526     C                   END                                                    W=0
100900970526     C                   END                                                    FUNKEY NE *BLANKS
101000970526     C*
101100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
101200970526     C     W             IFNE      0
101300970526     C     FUNKEY        OREQ      *BLANKS
101400970526     C*
101500970526     C     FUNKEY        IFEQ      *BLANKS
101600970526     C                   EXSR      ENTGES
101700970526     C                   END
101800970526     C     FUNKEY        IFEQ      '03'
101900970526     C                   EXSR      F03GES
102000970526     C                   END
102100970526     C     FUNKEY        IFEQ      '05'
102200970526     C                   EXSR      F05GES
102300970526     C                   END
102400970526     C     FUNKEY        IFEQ      '06'
102500970526     C                   EXSR      F06GES
102600970526     C                   END
102700970526     C     FUNKEY        IFEQ      '12'
102800970526     C                   EXSR      F12GES
102900970526     C                   END
103000970526     C     FUNKEY        IFEQ      '23'
103100970526     C                   EXSR      F23GES
103200970526     C                   END
103300970526     C     FUNKEY        IFEQ      '24'
103400970526     C                   EXSR      F24GES
103500970526     C                   END
103600970526     C     FUNKEY        IFEQ      'RU'
103700970526     C                   EXSR      RUPGES
103800970526     C                   END
103900970526     C*
104000970526     C                   END
104100970526     C*
104200970526     C                   ENDSR
104300970526     C*------------------------------------------------------------------------*
104400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
104500970526     C*------------------------------------------------------------------------*
104600970526     C     FUNVAR        BEGSR
104700970526     C*
104800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
104900970526     C     KEY           IFEQ      ENTER
105000970526     C                   MOVEL     *BLANKS       FUNKEY
105100970526     C                   END
105200970526     C     KEY           IFEQ      F1
105300970526     C                   MOVEL     '01'          FUNKEY
105400970526     C                   END
105500970526     C     KEY           IFEQ      F2
105600970526     C                   MOVEL     '02'          FUNKEY
105700970526     C                   END
105800970526     C     KEY           IFEQ      F3
105900970526     C                   MOVEL     '03'          FUNKEY
106000970526     C                   END
106100970526     C     KEY           IFEQ      F4
106200970526     C                   MOVEL     '04'          FUNKEY
106300970526     C                   END
106400970526     C     KEY           IFEQ      F5
106500970526     C                   MOVEL     '05'          FUNKEY
106600970526     C                   END
106700970526     C     KEY           IFEQ      F6
106800970526     C                   MOVEL     '06'          FUNKEY
106900970526     C                   END
107000970526     C     KEY           IFEQ      F7
107100970526     C                   MOVEL     '07'          FUNKEY
107200970526     C                   END
107300970526     C     KEY           IFEQ      F8
107400970526     C                   MOVEL     '08'          FUNKEY
107500970526     C                   END
107600970526     C     KEY           IFEQ      F9
107700970526     C                   MOVEL     '09'          FUNKEY
107800970526     C                   END
107900970526     C     KEY           IFEQ      F10
108000970526     C                   MOVEL     '10'          FUNKEY
108100970526     C                   END
108200970526     C     KEY           IFEQ      F11
108300970526     C                   MOVEL     '11'          FUNKEY
108400970526     C                   END
108500970526     C     KEY           IFEQ      F12
108600970526     C                   MOVEL     '12'          FUNKEY
108700970526     C                   END
108800970526     C     KEY           IFEQ      F13
108900970526     C                   MOVEL     '13'          FUNKEY
109000970526     C                   END
109100970526     C     KEY           IFEQ      F14
109200970526     C                   MOVEL     '14'          FUNKEY
109300970526     C                   END
109400970526     C     KEY           IFEQ      F15
109500970526     C                   MOVEL     '15'          FUNKEY
109600970526     C                   END
109700970526     C     KEY           IFEQ      F16
109800970526     C                   MOVEL     '16'          FUNKEY
109900970526     C                   END
110000970526     C     KEY           IFEQ      F17
110100970526     C                   MOVEL     '17'          FUNKEY
110200970526     C                   END
110300970526     C     KEY           IFEQ      F18
110400970526     C                   MOVEL     '18'          FUNKEY
110500970526     C                   END
110600970526     C     KEY           IFEQ      F19
110700970526     C                   MOVEL     '19'          FUNKEY
110800970526     C                   END
110900970526     C     KEY           IFEQ      F20
111000970526     C                   MOVEL     '20'          FUNKEY
111100970526     C                   END
111200970526     C     KEY           IFEQ      F21
111300970526     C                   MOVEL     '21'          FUNKEY
111400970526     C                   END
111500970526     C     KEY           IFEQ      F22
111600970526     C                   MOVEL     '22'          FUNKEY
111700970526     C                   END
111800970526     C     KEY           IFEQ      F23
111900970526     C                   MOVEL     '23'          FUNKEY
112000970526     C                   END
112100970526     C     KEY           IFEQ      F24
112200970526     C                   MOVEL     '24'          FUNKEY
112300970526     C                   END
112400970526     C     KEY           IFEQ      ROLLUP
112500970526     C                   MOVEL     'RU'          FUNKEY
112600970526     C                   END
112700970526     C     KEY           IFEQ      HELP
112800970526     C                   MOVEL     'HP'          FUNKEY
112900970526     C                   END
113000970526     C*
113100970526     C                   ENDSR
113200970526     C*------------------------------------------------------------------------*
113300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
113400970526     C*------------------------------------------------------------------------*
113500970526     C     FUNGES        BEGSR
113600970526     C*
113700970526     C* PULISCO CAMPI DI LAVORO
113800970526     C                   Z-ADD     0             W                 5 0
113900970526     C*
114000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
114100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
114200970526     C     1             DO        24            I
114300970526     C                   MOVEL     FUD(I)        FUDRIG
114400970526     C*
114500970526     C* PULIZIA FUNZIONI PRECEDENTI
114600970526     C                   MOVEL     *BLANKS       FUV(I)
114700970526     C*
114800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
114900970526     C* (SALTA F23 E F24)
115000970526     C     FUDATT        IFEQ      'S'
115100970526     C     FUDFUN        ANDNE     '23'
115200970526     C     FUDFUN        ANDNE     '24'
115300970526     C                   ADD       1             W
115400970526     C                   MOVEL     FUDDES        FUV(W)
115500970526     C                   END
115600970526     C                   END                                                    DO WHILE
115700970526     C*
115800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
115900970526     C* ALTRIMENTI DISATTIVO F24
116000970526     C     1             DO        24            I
116100970526     C                   MOVEL     FUD(I)        FUDRIG
116200970526     C*
116300970526     C     FUDFUN        IFEQ      '24'
116400970526     C     W             IFGT      6
116500970526     C                   MOVEL     'S'           FUDATT
116600970526     C                   ELSE
116700970526     C                   MOVEL     *BLANKS       FUDATT
116800970526     C                   END                                                    W GT 8
116900970526     C                   MOVEL     FUDRIG        FUD(I)
117000970526     C                   END
117100970526     C                   END                                                    DO WHILE
117200970526     C*
117300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
117400970526     C     W             DIV       6             I
117500970526     C                   MVR                     Z
117600970526     C*
117700970526     C     Z             IFNE      0
117800970526     C     I             ADD       1             Z
117900970526     C                   ELSE
118000970526     C                   Z-ADD     I             Z
118100970526     C                   END
118200970526     C*
118300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
118400970526     C                   ADD       1             FUNGRU
118500970526     C     FUNGRU        IFGT      Z
118600970526     C     FUNGRU        ORLT      1
118700970526     C                   Z-ADD     1             FUNGRU
118800970526     C                   END
118900970526     C*
119000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
119100970526     C     FUNGRU        IFEQ      1
119200970526     C                   MOVEL     FUV(1)        FUN001
119300970526     C                   MOVEL     FUV(2)        FUN002
119400970526     C                   MOVEL     FUV(3)        FUN003
119500970526     C                   MOVEL     FUV(4)        FUN004
119600970526     C                   MOVEL     FUV(5)        FUN005
119700970526     C                   MOVEL     FUV(6)        FUN006
119800970526     C                   END
119900970526     C*
120000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
120100970526     C     FUNGRU        IFEQ      2
120200970526     C                   MOVEL     FUV(7)        FUN001
120300970526     C                   MOVEL     FUV(8)        FUN002
120400970526     C                   MOVEL     FUV(9)        FUN003
120500970526     C                   MOVEL     FUV(10)       FUN004
120600970526     C                   MOVEL     FUV(11)       FUN005
120700970526     C                   MOVEL     FUV(12)       FUN006
120800970526     C                   END
120900970526     C*
121000970526     C* TERZO GRUPPO DI 6 FUNZIONI
121100970526     C     FUNGRU        IFEQ      3
121200970526     C                   MOVEL     FUV(13)       FUN001
121300970526     C                   MOVEL     FUV(14)       FUN002
121400970526     C                   MOVEL     FUV(15)       FUN003
121500970526     C                   MOVEL     FUV(16)       FUN004
121600970526     C                   MOVEL     FUV(17)       FUN005
121700970526     C                   MOVEL     FUV(18)       FUN006
121800970526     C                   END
121900970526     C*
122000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
122100970526     C     FUNGRU        IFEQ      4
122200970526     C                   MOVEL     FUV(19)       FUN001
122300970526     C                   MOVEL     FUV(20)       FUN002
122400970526     C                   MOVEL     FUV(21)       FUN003
122500970526     C                   MOVEL     FUV(22)       FUN004
122600970526     C                   MOVEL     *BLANKS       FUN005
122700970526     C                   MOVEL     *BLANKS       FUN006
122800970526     C                   END
122900970526     C*
123000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
123100970526     C                   MOVEL     *BLANKS       FUN007
123200970526     C                   MOVEL     *BLANKS       FUN008
123300970526     C*
123400970526     C     1             DO        24            I
123500970526     C                   MOVEL     FUD(I)        FUDRIG
123600970526     C     FUDFUN        IFEQ      '23'
123700970526     C     FUDATT        ANDEQ     'S'
123800970526     C                   MOVEL     FUDDES        FUN007
123900970526     C                   END
124000970526     C     FUDFUN        IFEQ      '24'
124100970526     C     FUDATT        ANDEQ     'S'
124200970526     C                   MOVEL     FUDDES        FUN008
124300970526     C                   END
124400970526     C                   END                                                    DO
124500970526     C*
124600970526     C                   ENDSR
124700970526     C*------------------------------------------------------------------------*
124800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
124900970526     C*------------------------------------------------------------------------*
125000970526     C     OPZGES        BEGSR
125100970526     C*
125200970526     C* PULISCO CAMPI DI LAVORO
125300970526     C                   Z-ADD     0             W                 5 0
125400970526     C*
125500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
125600970526     C* E CARICO TABELLA OPZIONI ATTIVE
125700970526     C     1             DO        24            I
125800970526     C                   MOVEL     OPD(I)        OPDRIG
125900970526     C*
126000970526     C* PULIZIA OPZIONI PRECEDENTI
126100970526     C                   MOVEL     *BLANKS       OPV(I)
126200970526     C*
126300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
126400970526     C     OPDATT        IFEQ      'S'
126500970526     C                   ADD       1             W
126600970526     C                   MOVEL     OPDDES        OPV(W)
126700970526     C                   END
126800970526     C                   END                                                    DO WHILE
126900970526     C*
127000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
127100970526     C* ALTRIMENTI DISATTIVO F23
127200970526     C     1             DO        24            I
127300970526     C                   MOVEL     FUD(I)        FUDRIG
127400970526     C*
127500970526     C     FUDFUN        IFEQ      '23'
127600970526     C     W             IFGT      8
127700970526     C                   MOVEL     'S'           FUDATT
127800970526     C                   ELSE
127900970526     C                   MOVEL     *BLANKS       FUDATT
128000970526     C                   END                                                    W GT 8
128100970526     C                   MOVEL     FUDRIG        FUD(I)
128200970526     C                   END
128300970526     C                   END                                                    DO WHILE
128400970526     C*
128500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
128600970526     C     W             DIV       8             I
128700970526     C                   MVR                     Z                 5 0
128800970526     C*
128900970526     C     Z             IFNE      0
129000970526     C     I             ADD       1             Z
129100970526     C                   ELSE
129200970526     C                   Z-ADD     I             Z
129300970526     C                   END
129400970526     C*
129500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
129600970526     C                   ADD       1             OPZGRU
129700970526     C     OPZGRU        IFGT      Z
129800970526     C     OPZGRU        ORLT      1
129900970526     C                   Z-ADD     1             OPZGRU
130000970526     C                   END
130100970526     C*
130200970526     C* PRIMO GRUPPO DI 8 OPZIONI
130300970526     C     OPZGRU        IFEQ      1
130400970526     C                   MOVEL     OPV(1)        OPZ001
130500970526     C                   MOVEL     OPV(2)        OPZ002
130600970526     C                   MOVEL     OPV(3)        OPZ003
130700970526     C                   MOVEL     OPV(4)        OPZ004
130800970526     C                   MOVEL     OPV(5)        OPZ005
130900970526     C                   MOVEL     OPV(6)        OPZ006
131000970526     C                   MOVEL     OPV(7)        OPZ007
131100970526     C                   MOVEL     OPV(8)        OPZ008
131200970526     C                   END
131300970526     C*
131400970526     C* SECONDO GRUPPO DI 8 OPZIONI
131500970526     C     OPZGRU        IFEQ      2
131600970526     C                   MOVEL     OPV(9)        OPZ001
131700970526     C                   MOVEL     OPV(10)       OPZ002
131800970526     C                   MOVEL     OPV(11)       OPZ003
131900970526     C                   MOVEL     OPV(12)       OPZ004
132000970526     C                   MOVEL     OPV(13)       OPZ005
132100970526     C                   MOVEL     OPV(14)       OPZ006
132200970526     C                   MOVEL     OPV(15)       OPZ007
132300970526     C                   MOVEL     OPV(16)       OPZ008
132400970526     C                   END
132500970526     C*
132600970526     C* TERZO GRUPPO DI 8 OPZIONI
132700970526     C     OPZGRU        IFEQ      3
132800970526     C                   MOVEL     OPV(17)       OPZ001
132900970526     C                   MOVEL     OPV(18)       OPZ002
133000970526     C                   MOVEL     OPV(19)       OPZ003
133100970526     C                   MOVEL     OPV(20)       OPZ004
133200970526     C                   MOVEL     OPV(21)       OPZ005
133300970526     C                   MOVEL     OPV(22)       OPZ006
133400970526     C                   MOVEL     OPV(23)       OPZ007
133500970526     C                   MOVEL     OPV(24)       OPZ008
133600970526     C                   END
133700970526     C*
133800970526     C                   ENDSR
133900970526     C*------------------------------------------------------------------------*
134000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
134100970526     C*------------------------------------------------------------------------*
134200970526     C     OPZCON        BEGSR
134300970526     C*
134400970526     C* PULISCO CAMPI DI LAVORO
134500970526     C                   Z-ADD     0             I                 5 0
134600970526     C                   Z-ADD     0             W                 5 0
134700970526     C*
134800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
134900970526     C     1             DO        24            I
135000970526     C                   MOVEL     OPD(I)        OPDRIG
135100970526     C*
135200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
135300970526     C     OPZKEY        IFEQ      OPDOPZ
135400970526     C     OPZKEY        OREQ      OPDOP1
135500970526     C*
135600970526     C     OPDATT        IFEQ      'S'
135700970526     C                   MOVEL     OPDOPZ        OPZKEY
135800970526     C                   Z-ADD     I             W
135900970526     C                   END
136000970526     C                   END
136100970526     C*
136200970526     C                   END                                                    DO WHILE
136300970526     C*
136400970526     C* OPZIONE NON ATTIVA (ERRORE)
136500970526     C     W             IFEQ      0
136600970526     C*
136700970526     C* OPZIONE NON OK
136800970526     C                   Z-ADD     1             OPZOK
136900970526     C*
137000970526     C* EMISSIONE MESSAGGIO DI ERRORE
137100970526     C                   MOVEL     MSG(1)        DSMSMS
137200970526     C                   ELSE
137300970526     C*
137400970526     C* OPZIONE ATTIVA (PROCEDI ...)
137500970526     C*
137600970526     C     OPZKEY        IFEQ      ' 1'
137700970526     C                   EXSR      O01GES
137800970526     C                   END
137900970526     C     OPZKEY        IFEQ      ' 2'
138000970526     C                   EXSR      O02GES
138100970526     C                   END
138200970526     C     OPZKEY        IFEQ      ' 4'
138300970526     C                   EXSR      O04GES
138400970526     C                   END
138500970526     C     OPZKEY        IFEQ      ' 5'
138600970526     C                   EXSR      O05GES
138700970526     C                   END
138800970526     C     OPZKEY        IFEQ      ' 7'
138900970526     C                   EXSR      O07GES
139000970526     C                   END
139100970526     C                   END                                                    OPZ.NON ATTIVA
139200970526     C*
139300970526     C                   ENDSR
139400970526     C*------------------------------------------------------------------------*
139500970526     C* NOTE COSTRUZIONE SCHIERE
139600970526     C*------------------------------------------------------------------------*
139700970526     C*
139800970526     C* -MSG-MESSAGGI
139900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
140000970526     C* (NON TOCCARE MSG,1 E MSG,2)
140100970526     C*
140200970526     C* -CMD-COMANDI
140300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
140400970526     C*
140500970526     C* -OPD-OPZIONI
140600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
140700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
140800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
140900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
141000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
141100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
141200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
141300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
141400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
141500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
141600970526     C*
141700970526     C* -FUD-FUNZIONI
141800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
141900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
142000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
142100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
142200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
142300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
142400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
142500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
142600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
142700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
142800970526     O*------------------------------------------------------------------------*
142900970526** DEC - DECODIFICHE
143000970526    GESTIONE                                                     1
143100970526INTERROGAZIONE                                                   2
143200970530     SCELTA                                                      3
1433009705264
1434009705265
143500970526** MSG - MESSAGGI
143600970526TISI41R- Opzione non gestita                                                   1
143700970526TISI41R- Tasto funzionale non gestito                                          2
143800970526TISI41R- Non abilitato alla gestione di questo CAP                             3
143900970526TISI41R- Record annullato. Consentito solo il ripristino                       4
144000970526TISI41R- Record non annullato. Impossibile ripristinare                        5
144100970526TISI41R- Accettato solo un parametro di inizio lista                           6
144200970527TISI41R- 7                                                                     7
144300970527TISI41R- 8                                                                     8
144400970526TISI41R- 9                                                                     9
144500970526TISI41R- 10                                                                   10
144600970526TISI41R- 11                                                                   11
144700970526TISI41R- 12                                                                   12
144800970526TISI41R- 13                                                                   13
144900970526TISI41R- 14                                                                   14
145000970526TISI41R- 15                                                                   15
145100970526TISI41R- 16                                                                   16
145200970526TISI41R- 17                                                                   17
145300970526TISI41R- 18                                                                   18
145400970526TISI41R- 19                                                                   19
145500970526TISI41R- 20                                                                   20
145600970526** OPD - OPZIONI
145700970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
145800970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
145900970526                   -3 -     (S/N)=N                        opz. 03¬
146000970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
146100970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
146200970526                   -6 -     (S/N)=N                        opz. 06¬
146300970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
146400970527                   -8 -     (S/N)=N                        opz. 08¬
146500970527                   -9 -     (S/N)=N                        opz. 09¬
146600970527                   -10-     (S/N)=N                        opz. 10¬
146700970526                   -  -     (S/N)=N                        opz. 11¬
146800970526                   -  -     (S/N)=N                        opz. 12¬
146900970526                   -  -     (S/N)=N                        opz. 13¬
147000970526                   -  -     (S/N)=N                        opz. 14¬
147100970526                   -  -     (S/N)=N                        opz. 15¬
147200970526                   -  -     (S/N)=N                        opz. 16¬
147300970526                   -  -     (S/N)=N                        opz. 17¬
147400970526                   -  -     (S/N)=N                        opz. 18¬
147500970526                   -  -     (S/N)=N                        opz. 19¬
147600970526                   -  -     (S/N)=N                        opz. 20¬
147700970526                   -  -     (S/N)=N                        opz. 21¬
147800970526                   -  -     (S/N)=N                        opz. 22¬
147900970526                   -  -     (S/N)=N                        opz. 23¬
148000970526                   -  -     (S/N)=N                        opz. 24¬
148100970526** FUD - FUNZIONI
148200970526                   -        (S/N)=N                        funz.01¬
148300970526                   -        (S/N)=N                        funz.02¬
148400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
148500970526                   -        (S/N)=N                        funz.04¬
148600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
148700970526F06=Inserimento    -        (S/N)=N                        funz.06¬
148800970526                   -        (S/N)=N                        funz.07¬
148900970527                   -        (S/N)=N                        funz.08¬
149000970527                   -        (S/N)=N                        funz.09¬
149100970527                   -        (S/N)=N                        funz.10¬
149200970526                   -        (S/N)=N                        funz.11¬
149300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
149400970526                   -        (S/N)=N                        funz.13¬
149500970526                   -        (S/N)=N                        funz.14¬
149600970526                   -        (S/N)=N                        funz.15¬
149700970526                   -        (S/N)=N                        funz.16¬
149800970526                   -        (S/N)=N                        funz.17¬
149900970526                   -        (S/N)=N                        funz.18¬
150000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
150100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
150200970619                   -        (S/N)=S                        funz.21¬
150300970619                   -        (S/N)=N                        funz.22¬
150400970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
150500970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
