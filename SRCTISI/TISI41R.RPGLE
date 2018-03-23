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
009100170120     D DUTE01        E DS                                                       *Abilitazioni
009200970527     D*------------------
009300970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009400970527     D*------------------
009500970527     D WLBDA8          DS                  INZ
009600970527     D  G08DAT                 1      8  0
009700970527     D  G08INV                 9     16  0
009800970527     D  G08ERR                17     17
009900970527     D  G08TGI                18     22  0
010000970527     D*------------
010100970527     D* DS TIPI SERVIZIO
010200970527     D*------------
010300970527     D DS5E          E DS
010400970703     D*------------
010500970526     D* DS DI PROCEDURA
010600970526     D*------------
010700970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010800970526     D*------------
010900970526     D* ARCHITETTURA
011000970526     D*------------
011100970526     D KPJBA         E DS
011200170120     d
011300170120     d wabiute         s              1
011400970526     C*------------------------------------------------------------------------*
011500970526     C* MAIN LINES
011600970526     C*------------------------------------------------------------------------*
011700970526     C*
011800970526     C* SE NON CI SONO ERRORI, PROSEGUE
011900970526IF  1C     ERRL00        IFNE      '1'
012000970526     C*
012100970526     C* CARICA IL SFILE
012200970526     C                   EXSR      POSSFL
012300970526     C*
012400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012800970526     C*
012900970526     C* REIMPOSTA ERRORE DI PROCERURA
013000970526     C                   MOVEL     '0'           ERRL00
013100970526     C*
013200970526     C* EMETTE IL SFILE
013300970526     C                   EXSR      OUTSFL
013400970526     C*
013500970526     C* GESTIONE TASTI FUNZIONALI
013600970526     C                   EXSR      FUNCON
013700970526     C*
013800970526E   2C                   ENDDO
013900970527     C*
014000970527     C* CI SONO ERRORI IN ENTRATA
014100970527X   1C                   ELSE
014200970527     C                   MOVEL     '1'           D00ERR
014300970527     C                   MOVEL     DSMSMS        D00MSG
014400970526E   1C                   ENDIF
014500970526     C* FINE PGM
014600970526     C                   SETON                                        LR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014900970526     C*------------------------------------------------------------------------*
015000970526     C     F03GES        BEGSR
015100970526     C*
015200970526     C* IMPOSTA "PREMUTO F03"
015300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015400970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015900970526     C*------------------------------------------------------------------------*
016000970526     C     F05GES        BEGSR
016100970526     C*
016200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016300970526     C*
016400970526     C                   ENDSR
016500970526     C*------------------------------------------------------------------------*
016600970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016700970526     C*------------------------------------------------------------------------*
016800970526     C     F06GES        BEGSR
016900970526     C*
017000970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
017100020919IF  1C     DUTLPO        IFNE      'S'
017200970526     C                   MOVEL     '1'           ERRL00
017300970526     C                   MOVEL     MSG(3)        DSMSMS
017400970526E   1C                   ENDIF
017500970526     C*
017600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017700970602     C                   Z-ADD     *ZEROS        DTTC
017800970602     C                   MOVEL     *BLANKS       DTSP
017900970602     C                   MOVEL     DEPOP0        WOP0
018000970602     C                   MOVEL     'F06'         WOP1
018100970527     C                   MOVEL     'TISI42R'     WPGM              8
018200970526     C                   EXSR      LANPGM                                       *LANCIA PGM
018300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018400970526E   1C                   ENDIF
018500970526     C*
018600970526     C                   ENDSR
018700970526     C*------------------------------------------------------------------------*
018800970526     C* F12GES - GESTIONE F12 -> RITORNO
018900970526     C*------------------------------------------------------------------------*
019000970526     C     F12GES        BEGSR
019100970526     C*
019200970526     C* IMPOSTA "PREMUTO F12"
019300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019400970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
019500970526     C*
019600970526     C                   ENDSR
019700970526     C*------------------------------------------------------------------------*
019800970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
019900970526     C*------------------------------------------------------------------------*
020000970526     C     F23GES        BEGSR
020100970526     C*
020200970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F24GES        BEGSR
020900970526     C*
021000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
021500970526     C*------------------------------------------------------------------------*
021600970526     C     RUPGES        BEGSR
021700970526     C*
021800970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
021900970526     C*
022000970526     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* ENTGES - GESTIONE ENTER -> INVIO
022300970526     C*------------------------------------------------------------------------*
022400970526     C     ENTGES        BEGSR
022500970526     C*
022600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
022700970526     C                   EXSR      NEWPOS
022800970526     C*
022900970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
023000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023100970526IF  2C     SWPOSI        IFNE      0
023200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
023300970526X   2C                   ELSE
023400970526IF  3C     SFLULT        IFNE      0
023500970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
023600970526     C*
023700970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
023800970526IF  4C     SWMODI        IFNE      0
023900970526     C                   EXSR      POSSFL
024000970526E   4C                   ENDIF
024100970526E   3C                   ENDIF
024200970526E   2C                   ENDIF
024300970526E   1C                   ENDIF
024400970526     C*
024500970526     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
024800970526     C*------------------------------------------------------------------------*
024900970526     C     O01GES        BEGSR
025000970526     C*
025100970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
025200970527     C                   Z-ADD     VIDTTC        D00TTC
025300970530     C                   MOVEL     VIDTSP        D00TSP
025400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
025500970526     C*
025600970526     C                   ENDSR
025700970526     C*------------------------------------------------------------------------*
025800970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
025900970526     C*------------------------------------------------------------------------*
026000970526     C     O02GES        BEGSR
026100970526     C*
026200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
026300970526IF  1C     VIDATB        IFEQ      'A'
026400970526     C                   MOVEL     '1'           ERRL00
026500970526     C                   MOVEL     MSG(4)        DSMSMS
026600970526E   1C                   ENDIF
026700970527     C* MODIFICA
026800970526IF  1C     ERRL00        IFEQ      '0'
026900970602     C                   Z-ADD     VIDTTC        DTTC
027000970602     C                   MOVEL     VIDTSP        DTSP
027100970602     C                   MOVEL     DEPOP0        WOP0
027200970526     C                   MOVEL     'O02'         WOP1
027300970527     C                   MOVEL     'TISI42R'     WPGM
027400970526     C                   EXSR      LANPGM
027500970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
027600970526E   1C                   ENDIF
027700970526     C*
027800970526     C                   ENDSR
027900970526     C*------------------------------------------------------------------------*
028000970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
028100970526     C*------------------------------------------------------------------------*
028200970526     C     O04GES        BEGSR
028300970526     C*
028400970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
028500970526IF  1C     VIDATB        IFEQ      'A'
028600970526     C                   MOVEL     '1'           ERRL00
028700970526     C                   MOVEL     MSG(4)        DSMSMS
028800970526E   1C                   ENDIF
028900970527     C* ANNULLAMENTO
029000970526IF  1C     ERRL00        IFEQ      '0'
029100970602     C                   Z-ADD     VIDTTC        DTTC
029200970602     C                   MOVEL     VIDTSP        DTSP
029300970602     C                   MOVEL     DEPOP0        WOP0
029400970526     C                   MOVEL     'O04'         WOP1
029500970527     C                   MOVEL     'TISI42R'     WPGM
029600970526     C                   EXSR      LANPGM
029700970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
029800970526E   1C                   ENDIF
029900970526     C*
030000970526     C                   ENDSR
030100970526     C*------------------------------------------------------------------------*
030200970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
030300970526     C*------------------------------------------------------------------------*
030400970526     C     O05GES        BEGSR
030500970526     C*
030600970602     C                   Z-ADD     VIDTTC        DTTC
030700970602     C                   MOVEL     VIDTSP        DTSP
030800970602     C                   MOVEL     DEPOP0        WOP0
030900970526     C                   MOVEL     'O05'         WOP1
031000970527     C                   MOVEL     'TISI42R'     WPGM
031100970526     C                   EXSR      LANPGM
031200970526     C*
031300970526     C                   ENDSR
031400970526     C*------------------------------------------------------------------------*
031500970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
031600970526     C*------------------------------------------------------------------------*
031700970526     C     O07GES        BEGSR
031800970526     C*
031900970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
032000970526IF  1C     VIDATB        IFNE      'A'
032100970526     C                   MOVEL     '1'           ERRL00
032200970526     C                   MOVEL     MSG(5)        DSMSMS
032300970526E   1C                   ENDIF
032400970526     C* RIPRISITINO
032500970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032600970602     C                   Z-ADD     VIDTTC        DTTC
032700970602     C                   MOVEL     VIDTSP        DTSP
032800970602     C                   MOVEL     DEPOP0        WOP0
032900970526     C                   MOVEL     'O07'         WOP1
033000970527     C                   MOVEL     'TISI42R'     WPGM
033100970526     C                   EXSR      LANPGM
033200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
033300970526E   1C                   ENDIF
033400970526     C*
033500970526     C                   ENDSR
033600970526     C*------------------------------------------------------------------------*
033700970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
033800970526     C*------------------------------------------------------------------------*
033900970526     C     NEWPOS        BEGSR
034000970526     C*
034100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
034200970526     C*         1= RICHIESTO POSIZIONAMENTO
034300970526     C                   Z-ADD     0             SWPOSI            7 0
034400970526     C*
034500970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
034600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
034700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
034800970526     C                   Z-ADD     0             SWMODI            1 0
034900970530     C*
035000970530     C* REIMPOSTA INDICATORI DI ERRORE
035100970530     C                   SETOFF                                       8081
035200970526     C*
035300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
035400970527IF  1C     VIDPTT        IFNE      DEPTTC
035500970527     C     VIDPTS        ORNE      DEPTSP
035600970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
035700970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
035800970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
035900970527     C                   Z-ADD     VIDPTT        DEPTTC                         *DEPOSITI
036000970527     C                   MOVEL     VIDPTS        DEPTSP
036100970526E   2C                   ENDIF
036200970526E   1C                   ENDIF
036300970526     C*
036400970526     C                   ENDSR
036500970526     C*------------------------------------------------------------------------*
036600970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
036700970526     C*------------------------------------------------------------------------*
036800970526     C     SETFIL        BEGSR
036900970526     C*
037000970527     C* POSIZIONAMENTO SUL FILE
037100970527     C                   Z-ADD     D00VER        KCTVER
037200970527     C                   Z-ADD     VIDPTT        KCTTTC
037300970527     C     KEYCPT        SETLL     AZCPT01L                           99
037400970526     C* LETTURA RECORD
037500970526IF  1C     *IN99         IFEQ      *OFF
037600970526     C                   EXSR      LETFIL
037700970526E   1C                   ENDIF
037800970526     C*
037900970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
038000970526IF  1C     *IN99         IFEQ      *ON
038100970526     C                   Z-ADD     1             SFLEOF
038200970526E   1C                   ENDIF
038300970526     C*
038400970526     C                   ENDSR
038500970526     C*------------------------------------------------------------------------*
038600970526     C* LETFIL - LETTURA RECORD
038700970526     C*------------------------------------------------------------------------*
038800970526     C     LETFIL        BEGSR
038900970526     C*
039000970526     C* LEGGE FINO A:
039100970526     C*     - FINE FILE
039200970526     C*     - TROVATO RECORD VALIDO
039300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
039400970526DO  1C     *IN99         DOWEQ     *OFF
039500970526     C     WRECOK        ANDEQ     'N'
039600970526     C*
039700970527     C* LETTURE SUCCESSIVE ALLA PRIMA
039800970527     C     KCTVER        READE     AZCPT01L                               99
039900970526     C*
040000970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
040100970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
040200970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
040300970530X   2C                   ELSE
040400970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
040500970530E   2C                   ENDIF
040600970526E   1C                   ENDDO
040700970526     C*
040800970526     C                   ENDSR
040900970526     C*------------------------------------------------------------------------*
041000970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
041100970526     C*------------------------------------------------------------------------*
041200970526     C     CHKREC        BEGSR
041300970526     C*
041400970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
041500970527     C*
041600970527     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
041700970527IF  1C     VIDPTS        IFNE      *BLANKS
041800970527IF  2C     VIDPTS        IFNE      CPTTSP
041900970527     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042000970527E   2C                   ENDIF
042100970527E   1C                   ENDIF
042200970526     C*
042300970526     C                   ENDSR
042400970526     C*------------------------------------------------------------------------*
042500970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
042600970526     C*------------------------------------------------------------------------*
042700970526     C     IRESFL        BEGSR
042800970526     C*
042900970526     C                   MOVEL     *BLANKS       OPZKEY
043000970527     C                   MOVEL     CPTATB        VIDATB
043100970527     C                   Z-ADD     CPTTTC        VIDTTC
043200970527     C                   MOVEL     CPTDTC        VIDDTC
043300970527     C                   MOVEL     CPTTSP        VIDTSP
043400970527     C                   Z-ADD     CPT0IN        VID0IN
043500970527     C                   Z-ADD     CPT1IN        VID1IN
043600970527     C                   Z-ADD     CPT0CN        VID0CN
043700970527     C                   Z-ADD     CPT1CN        VID1CN
043800970527     C                   Z-ADD     CPTTIS        VIDTIS
043900970527     C                   Z-ADD     CPTTCS        VIDTCS
044000970527     C                   Z-ADD     CPTTLD        VIDTLD
044100970527     C* DECODIFICHE
044200970527     C                   SETOFF                                           28
044300970527     C                   Z-ADD     1             I
044400970527     C     CPTTSP        LOOKUP    TSP(I)                                 28
044500970527IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
044600970602     C                   MOVEL     TDES(I)       VIDDTS
044700970527X   1C                   ELSE                                                   *NON TROVATO
044800970527     C                   MOVEL     *BLANKS       VIDDTS
044900970527E   1C                   ENDIF
045000970526     C*
045100970526     C                   ENDSR
045200970526     C*------------------------------------------------------------------------*
045300970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
045400970526     C*------------------------------------------------------------------------*
045500970526     C     CHKPAR        BEGSR
045600970526     C*
045700970526     C                   ENDSR
045800970526     C*------------------------------------------------------------------------*
045900970526     C* LANPGM - LANCIA UN PROGRAMMA
046000970526     C*------------------------------------------------------------------------*
046100970526     C     LANPGM        BEGSR
046200970526     C*
046300970526     C* IMPOSTA LA DS DI PROCEDURA
046400970602     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
046500970602     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
046600970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
046700970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
046800970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
046900970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
047000970602     C                   Z-ADD     DTTC          D00TTC                         *CODICE TEMPO
047100970602     C                   MOVEL     DTSP          D00TSP                         *TIPO SERVIZIO
047200970530     C                   Z-ADD     *ZEROS        D00TFP                         *ALTRI CAMPI
047300970530     C                   MOVEL     *BLANKS       D00LOC
047400970603     C                   MOVEL     *BLANKS       D00SIN
047500970530     C                   MOVEL     *BLANKS       D00EPA
047600970530     C                   Z-ADD     *ZEROS        D00ETP
047700970530     C                   Z-ADD     *ZEROS        D00ETA
047800970530     C                   Z-ADD     *ZEROS        D00EDD
047900970530     C                   MOVEL     *BLANKS       D00SPT
048000970530     C                   Z-ADD     *ZEROS        D00SLP
048100970530     C                   Z-ADD     *ZEROS        D00SLA
048200970530     C                   MOVEL     *BLANKS       D00STS
048300970530     C                   Z-ADD     *ZEROS        D00SDD
048400970526     C*
048500970526     C                   CALL      WPGM
048600970612     C                   PARM                    KPJBA
048700970612     C                   PARM                    DSSI00
048800970526     C*
048900970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
049000970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
049100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
049200970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
049300970526X   1C                   ELSE                                                   *NON PREMUTO F03
049400970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
049500970526E   1C                   ENDIF
049600970526     C*
049700970526     C                   ENDSR
049800970527     C*------------------------------------------------------------------------*
049900970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
050000970527     C*------------------------------------------------------------------------*
050100970527     C     CARTAB        BEGSR
050200970527     C*---
050300970527     C* TIPI SERVIZIO
050400970527     C*---
050500970527     C                   Z-ADD     *ZEROS        I                 5 0
050600970527     C                   MOVEL     *BLANKS       TSP
050700970527     C                   MOVEL     *BLANKS       DTSP
050800020919     C                   MOVEL     1             KTBKUT
050900970527     C                   MOVEL     '5E'          KTBCOD
051000970527     C     KEYTAB        SETLL     TABEL00F
051100970527     C     KEYTAB        READE     TABEL00F                               99
051200970527DO  1C     *IN99         DOWEQ     *OFF
051300970527IF  2C     TBLFLG        IFEQ      *BLANKS
051400970527     C                   ADD       1             I
051500970527     C                   MOVEL     TBLUNI        DS5E
051600970527     C                   MOVEL     TBLKEY        TSP(I)
051700090605     C                   MOVEL     §5ED03        TDES(I)
051800970527E   2C                   ENDIF
051900970527     C     KEYTAB        READE     TABEL00F                               99
052000970527E   1C                   ENDDO                                                  *FINE TABELLA
052100970527     C*
052200970527     C                   ENDSR
052300970526     C*------------------------------------------------------------------------*
052400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
052500970526     C*------------------------------------------------------------------------*
052600970526     C     CHKFOP        BEGSR
052700170120     C*
052800170120     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
052900170120     C                   SETON                                        93
053000170120     C*
053100170120     C                   IF        wabiute = 'C'
053200170120     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
053300170120     C                   SETOFF                                       93
053400170120     C                   ENDIF
053500170120     c
053600170120     c* Se utente abilitato alla mantenzione futura, non può manutenzinare mai i tempi
053700170120     c                   if        wabiute='V'
053800170120     c                   eval      wabiute='I'
053900170120     C                   ENDIF
054000170120     c
054100970626     C*
054200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
054300970626IF  1C     DEPOP0        IFEQ      'T01'
054400970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
054500970626     C                   MOVEL     'S'           OPDATT
054600970626     C                   MOVEL     OPDRIG        OPD(1)
054700970626X   1C                   ELSE
054800970626     C* OPZIONI
054900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
055000970626     C                   MOVEL     'S'           OPDATT
055100970626     C                   MOVEL     OPDRIG        OPD(5)
055200970626     C* FUNZIONI
055300970626E   1C                   ENDIF
055400970626     C*
055500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
055600170120     c*  solo se abilitato
055700170120IF  1C  n93DEPOP0        IFEQ      'T02'
055800170120     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
055900170120     C                   MOVEL     'S'           OPDATT
056000170120     C                   MOVEL     OPDRIG        OPD(2)
056100170120     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
056200170120     C                   MOVEL     'S'           OPDATT
056300170120     C                   MOVEL     OPDRIG        OPD(4)
056400170120     c* il ripristino non esiste più
056500170120     C* n93              MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
056600170120     C* n93              MOVEL     'S'           OPDATT
056700170120     C* n93              MOVEL     OPDRIG        OPD(7)
056800170120
056900170120     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
057000170120     C                   MOVEL     'S'           FUDATT
057100170120     C                   MOVEL     FUDRIG        FUD(6)
057200970626E   1C                   ENDIF
057300970626     C*
057400970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
057500020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
057600970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
057700970626     C                   MOVEL     'N'           OPDATT
057800970626     C                   MOVEL     OPDRIG        OPD(4)
057900970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
058000970626     C                   MOVEL     'N'           OPDATT
058100970626     C                   MOVEL     OPDRIG        OPD(7)
058200970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
058300970626     C                   MOVEL     'N'           FUDATT
058400970626     C                   MOVEL     FUDRIG        FUD(6)
058500970626E   1C                   ENDIF
058600970526     C*
058700970526     C* GESTIONE OPZIONI A VIDEO
058800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
058900970526     C                   EXSR      OPZGES
059000970526     C*
059100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
059200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
059300970526     C                   EXSR      FUNGES
059400970526     C*
059500970526     C                   ENDSR
059600020919     C*--------------------------------------------------------------------------------------------*
059700020919     C* REPERISCE I DATI UTENTE
059800020919     C*--------------------------------------------------------------------------------------------*
059900020919     C     REPDATIUTE    BEGSR
060000020919     C*
060100020919     C* INIZIALIZZA VARIABILI DI WRK
060200020919     C                   CLEAR                   TIBS34DS
060300020919     C                   CLEAR                   AZUTEDS
060400020919     C                   CLEAR                   DDATIUTE
060500020919     C*
060600020919     C     *DTAARA       DEFINE    §azute        azuteds
060700020919     C     *DTAARA       DEFINE    §datiute      ddatiute
060800020919     C                   IN(E)     *DTAARA
060900020919if  1C                   IF        %Error
061000020919     c                   EVAL      I34Tla = 'L'
061100020919     C                   CALL      'TIBS34R'
061200020919     C                   PARM                    Tibs34Ds
061300020919     C                   IN        *DTAARA
061400020919e   1C                   ENDIF
061500020919     C*
061600020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
061700020919     C                   MOVEL(P)  RSUT          DSFIRS           20
061800170120     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
061900170120     C                   EVAL      DUTE01 = UTEFAF
062000020919     C*
062100020919     C                   ENDSR
062200970526     C*------------------------------------------------------------------------*
062300970526     C* *INZSR - OPERAZIONI INIZIALI
062400970526     C*------------------------------------------------------------------------*
062500970526     C     *INZSR        BEGSR
062600970526     C*---
062700970526     C* RICEVIMENTO PARAMETRI
062800970526     C*---
062900970526     C     *ENTRY        PLIST
063000970612     C                   PARM                    KPJBA
063100970612     C                   PARM                    DSSI00
063200970526     C*
063300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
063400970526     C     XMSGPA        PLIST
063500970526     C                   PARM                    DSMSMS
063600970526     C                   PARM                    DSMSPG
063700970526     C                   PARM                    DSMSKE
063800970526     C                   PARM                    DSMSER
063900970526     C*
064000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
064100970526     C                   CALL      'XKEY'
064200970526     C                   PARM                    DSKEY
064300970526     C*---
064400970526     C* VARIABILI NON RIFERITE AL DATA BASE
064500970526     C*---
064600970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
064700970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
064800970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
064900970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
065000970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
065100970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
065200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
065300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
065400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
065500970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
065600970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
065700970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
065800970529     C     *LIKE         DEFINE    D00OP1        WOP1
065900970602     C     *LIKE         DEFINE    D00TTC        DTTC
066000970602     C     *LIKE         DEFINE    D00TSP        DTSP
066100970526     C*---
066200970526     C* VARIABILI DI CONTROLLO
066300970526     C*---
066400970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
066500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
066600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
066700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
066800970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
066900970526     C                   Z-ADD     9             SFLPAG                         *N°RIGHE X PAG
067000970526     C*
067100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
067200970526     C                   MOVEL     'TISI41R'     DSMSPG
067300970526     C                   MOVEL     'TISI41R'     MSGQUE
067400970526     C                   MOVEL     *BLANKS       MSGKEY
067500970526     C                   MOVEL     *BLANKS       DSMSMS
067600970526     C                   MOVEL     *BLANKS       DSMSKE
067700970526     C                   Z-ADD     *ZEROS        DSMSER
067800970526     C*---
067900970526     C* VARIABILI RIFERITE AL DATA BASE
068000970526     C*---
068100970527     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
068200970527     C     *LIKE         DEFINE    TBLCOD        KTBCOD
068300970527     C     *LIKE         DEFINE    CPTVER        KCTVER                         *AZCPT00F
068400970527     C     *LIKE         DEFINE    CPTTTC        KCTTTC
068500970527     C     *LIKE         DEFINE    CPTTTC        DEPTTC                         *DEPOSITI
068600970527     C     *LIKE         DEFINE    CPTTSP        DEPTSP
068700970526     C*---
068800970526     C* CHIAVI DI LETTURA
068900970526     C*---
069000970527     C* LETTURA TABEL00F - PARZIALE
069100970527     C     KEYTAB        KLIST
069200970527     C                   KFLD                    KTBKUT                         *UTENTE
069300970527     C                   KFLD                    KTBCOD                         *TABELLA
069400970527     C* LETTURA AZCPT01L - PARZIALE
069500970527     C     KEYCPT        KLIST
069600970527     C                   KFLD                    KCTVER                         *VERSIONE
069700970527     C                   KFLD                    KCTTTC                         *CODICE TEMPO
069800970527     C*
069900970527     C* REPERIMENTO DATI UTENTE
070000020919     C                   EXSR      REPDATIUTE
070100170120     c* Imposto il flag della abilitazione
070200170120     c                   eval      wabiute=§utecap
070300970527     C*
070400970527     C* CARICA LE TABELLE OCCORRENTI
070500970527     C                   EXSR      CARTAB
070600970526     C*
070700970526     C* DEPOSITA LE OPZIONI DI ENTRATA
070800970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
070900970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
071000970526     C*
071100970526     C* CALCOLA LA DATA CORRENTE
071200970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
071300970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
071400970526     C                   Z-ADD     WN8           G08DAT
071500970526     C                   Z-ADD     *ZEROS        G08INV
071600970526     C                   MOVEL     '0'           G08ERR
071700970526     C                   CALL      'XSRDA8'
071800970526     C                   PARM                    WLBDA8
071900970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
072000970526     C*
072100970526     C* IMPOSTA I PARAMETRI DI INGRESSO
072200970526     C                   Z-ADD     D00DRI        G08INV
072300970526     C                   Z-ADD     *ZEROS        G08DAT
072400970526     C                   MOVEL     '3'           G08ERR
072500970526     C                   CALL      'XSRDA8'
072600970526     C                   PARM                    WLBDA8
072700970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
072800970527     C                   Z-ADD     D00TTC        VIDPTT                         *CODICE TEMPO
072900970527     C                   MOVEL     D00TSP        VIDPTS                         *TIPO SERVIZIO
073000970526     C                   EXSR      CHKPAR
073100970526IF  1C     ERRL00        IFEQ      '1'
073200970526     C                   MOVEL     '1'           D00ERR
073300970526     C                   MOVEL     DSMSMS        D00MSG
073400970526X   1C                   ELSE
073500970527     C                   Z-ADD     VIDPTT        DEPTTC                         *DEPOSITI
073600970527     C                   MOVEL     VIDPTS        DEPTSP
073700970526E   1C                   ENDIF
073800970703     C*
073900970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
074000970703IF  1C     D00VER        IFEQ      *ZEROS
074100970707     C                   Z-ADD     D00DRI        P96DRI            8 0
074200970707     C                   Z-ADD     *ZEROS        P96VER            5 0
074300970707     C                   CALL      'TISI96R'
074400970707     C                   PARM                    P96DRI
074500970707     C                   PARM                    P96VER
074600970707     C                   Z-ADD     P96VER        D00VER
074700970703E   1C                   ENDIF
074800970526     C*
074900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
075000970526     C                   EXSR      CHKFOP
075100970530     C*
075200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
075300970530IF  1C     DEPOP0        IFEQ      'T01'
075400970530     C                   MOVEL     DEC(3)        VIDING
075500970530E   1C                   ENDIF
075600970530IF  1C     DEPOP0        IFEQ      'T02'
075700970530     C                   MOVEL     DEC(1)        VIDING
075800970530E   1C                   ENDIF
075900970530IF  1C     DEPOP0        IFEQ      'T05'
076000970530     C                   MOVEL     DEC(2)        VIDING
076100970530E   1C                   ENDIF
076200970526     C*
076300970526     C                   ENDSR
076400970526     C*------------------------------------------------------------------------*
076500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
076700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
076800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
076900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
077000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
077100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
077200970526     C*------------------------------------------------------------------------*
077300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
077400970526     C*------------------------------------------------------------------------*
077500970526     C     POSSFL        BEGSR
077600970526     C*
077700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
077800970526     C                   EXSR      CLRSFL
077900970526     C*
078000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
078100970526     C                   EXSR      SETFIL
078200970526     C*
078300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
078400970526     C     SFLEOF        IFEQ      0
078500970526     C                   EXSR      CARSFL
078600970526     C                   END
078700970526     C*
078800970526     C                   ENDSR
078900970526     C*------------------------------------------------------------------------*
079000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
079100970526     C*------------------------------------------------------------------------*
079200970526     C     ENTATT        BEGSR
079300970526     C*
079400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
079500970526     C                   Z-ADD     0             OPZOK             1 0
079600970526     C*
079700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
079800970526     C                   Z-ADD     0             SFLVAR            5 0
079900970526     C*
080000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
080100970526     C                   Z-ADD     0             SFLERR
080200970526     C*
080300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
080400970526     C                   Z-ADD     SFLULT        WFLULT
080500970526     C*
080600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
080700970526     C                   READC     SI41D05                                98
080800970526     C     *IN98         DOWEQ     '0'
080900970526     C*
081000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
081100970526     C                   ADD       1             SFLVAR
081200970526     C*
081300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
081400970526     C     OPZOK         IFEQ      0
081500970526     C*
081600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
081700970526     C     OPZKEY        IFNE      *BLANKS
081800970526     C                   EXSR      OPZCON
081900970526     C                   END
082000970526     C*
082100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
082200970526     C*            2. PULIZIA SCELTA IN SFL
082300970526     C     OPZOK         IFEQ      0
082400970526     C                   MOVEL     '0'           *IN41
082500970526     C                   MOVEL     *BLANKS       OPZKEY
082600970526     C                   ELSE
082700970526     C*
082800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
082900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
083000970526     C                   MOVEL     '1'           *IN41
083100970526     C                   Z-ADD     SFLULT        SFLERR
083200970526     C                   END
083300970526     C                   ELSE
083400970526     C*
083500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
083600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
083700970526     C                   MOVEL     '1'           *IN41
083800970526     C                   END                                                    OPZOK=0
083900970526     C*
084000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
084100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
084200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
084300970526     C     OPZOK         IFNE      0
084400970526     C                   Z-ADD     SFLERR        SFLNBR
084500970526     C                   ELSE
084600970526     C                   Z-ADD     SFLULT        SFLNBR
084700970526     C                   END
084800970526     C*
084900970526     C* AGGIORNO RECORD SFL
085000970526     C                   UPDATE    SI41D05
085100970526     C*
085200970526     C                   READC     SI41D05                                98
085300970526     C                   END                                                    DO WHILE
085400970526     C*
085500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
085600970526     C                   Z-ADD     WFLULT        SFLULT
085700970526     C*
085800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
085900970526     C     SFLVAR        IFEQ      0
086000970526     C*---                 MOVEL'1'       FINL00
086100970526     C                   END
086200970526     C*
086300970526     C                   ENDSR
086400970526     C*------------------------------------------------------------------------*
086500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
086600970526     C*------------------------------------------------------------------------*
086700970526     C     CLRSFL        BEGSR
086800970526     C*
086900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
087000970526     C* (0=NON FINE FILE  1=FINE FILE)
087100970526     C                   Z-ADD     0             SFLEOF
087200970526     C*
087300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
087400970526     C                   Z-ADD     0             SFLULT
087500970526     C*
087600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
087700970526     C                   MOVEL     '1'           *IN40
087800970526     C                   WRITE     SI41D06
087900970526     C                   MOVEL     '0'           *IN40
088000970526     C*
088100970526     C                   ENDSR
088200970526     C*------------------------------------------------------------------------*
088300970526     C* CARSFL - CARICAMENTO SFL
088400970526     C*------------------------------------------------------------------------*
088500970526     C     CARSFL        BEGSR
088600970526     C*
088700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
088800970526     C     SFLULT        IFEQ      0
088900970526     C                   EXSR      SETFIL
089000970526     C                   END
089100970526     C*
089200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
089300970526     C                   Z-ADD     SFLULT        WFLULT
089400970526     C*
089500970526     C* CICLO:
089600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
089700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
089800970526     C                   Z-ADD     1             SFLRIG
089900970526     C     SFLRIG        DOWLE     SFLPAG
090000970526     C     SFLEOF        ANDEQ     0
090100970526     C*
090200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
090300970526     C                   MOVEL     *BLANKS       OPZKEY
090400970526     C*
090500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
090600970526     C                   EXSR      IRESFL
090700970526     C*
090800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
090900970526     C                   ADD       1             SFLULT
091000970526     C*
091100970526     C* SCRITTURA RECORD SFL
091200970526     C                   WRITE     SI41D05
091300970526     C                   ADD       1             SFLRIG
091400970526     C*
091500970526     C* LETTURA FILE DA CARICARE
091600970526     C                   EXSR      LETFIL
091700970526     C                   END                                                    DO WHILE
091800970526     C*
091900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
092000970526     C     WFLULT        IFNE      SFLULT
092100970526     C                   Z-ADD     WFLULT        SFLNBR
092200970526     C                   ADD       1             SFLNBR
092300970526     C                   END
092400970526     C*
092500970526     C                   ENDSR
092600970526     C*------------------------------------------------------------------------*
092700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
092800970526     C*------------------------------------------------------------------------*
092900970526     C     OUTSFL        BEGSR
093000970526     C*
093100970526     C* WRITE RECORD TESTATA VIDEO
093200970526     C                   WRITE     SI41D01
093300970526     C*
093400970526     C* VISUALIZZAZIONE MESSAGGIO
093500970526     C                   EXSR      SNDMSG
093600970526     C*
093700970526     C* WRITE RECORD OPZIONI VIDEO
093800970526     C                   WRITE     SI41D02
093900970526     C*
094000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
094100970526     C                   WRITE     SI41D03
094200970526     C*
094300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
094400970526     C                   WRITE     SI41D04
094500970526     C*
094600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
094700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
094800970526     C     SFLEOF        IFEQ      0
094900970526     C                   MOVEL     '0'           *IN42
095000970526     C                   ELSE
095100970526     C*
095200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
095300970526     C                   MOVEL     '1'           *IN42
095400970526     C                   END
095500970526     C*
095600970526     C* WRITE RECORD TASTI FUNZIONALI
095700970526     C                   WRITE     SI41D09
095800970526     C*
095900970526     C* CASO DI SFL VUOTO
096000970526     C     SFLULT        IFLE      0
096100970526     C*
096200970526     C*    - WRITE RECORD VUOTO
096300970526     C                   WRITE     SI41D0A
096400970526     C                   READ      SI41D0A                                99
096500970526     C*
096600970526     C                   ELSE
096700970526     C*
096800970526     C* CASO DI SFL CON RECORDS
096900970526     C*
097000970526     C*    - WRITE RECORD CONTROLLO SFL
097100970526     C                   WRITE     SI41D06
097200970526     C                   READ      SI41D06                                99
097300970526     C                   END
097400970526     C*
097500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
097600970526     C                   READ      SI41D03                                99
097700970526     C*
097800970526     C                   ENDSR
097900970526     C*------------------------------------------------------------------------*
098000970526     C* SNDMSG - MANDA MESSAGGIO
098100970526     C*------------------------------------------------------------------------*
098200970526     C     SNDMSG        BEGSR
098300970526     C*
098400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
098500970526     C                   MOVEL     '1'           *IN40
098600970526     C                   WRITE     SI41DMC
098700970526     C                   MOVEL     '0'           *IN40
098800970526     C*
098900970526     C* VISUALIZZAZIONE MESSAGGIO
099000970526     C                   Z-ADD     0             MSGULT
099100970526     C                   CALL      'XMSG'        XMSGPA
099200970526     C     DSMSER        IFEQ      0
099300970526     C                   MOVEL     DSMSKE        MSGKEY
099400970526     C                   END
099500970526     C                   Z-ADD     1             MSGULT
099600970526     C                   WRITE     SI41DMS                                      SFL MESS
099700970526     C                   WRITE     SI41DMC                                      CONTR MESS
099800970526     C*
099900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
100000970526     C                   MOVEL     *BLANKS       DSMSMS
100100970526     C*
100200970526     C                   ENDSR
100300970526     C*------------------------------------------------------------------------*
100400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
100500970526     C*------------------------------------------------------------------------*
100600970526     C     FUNCON        BEGSR
100700970526     C*
100800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
100900970526     C                   EXSR      FUNVAR
101000970526     C*
101100970526     C* PULISCO CAMPI DI LAVORO
101200970526     C                   Z-ADD     0             I                 5 0
101300970526     C                   Z-ADD     0             W                 5 0
101400970526     C*
101500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
101600970526     C     FUNKEY        IFNE      *BLANKS
101700970526     C     1             DO        24            I
101800970526     C                   MOVEL     FUD(I)        FUDRIG
101900970526     C*
102000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
102100970526     C     FUNKEY        IFEQ      FUDFUN
102200970526     C     FUDATT        ANDEQ     'S'
102300970526     C                   Z-ADD     I             W
102400970526     C                   END
102500970526     C                   END                                                    DO WHILE
102600970526     C*
102700970526     C* FUNZIONE NON ATTIVA (ERRORE)
102800970526     C     W             IFEQ      0
102900970526     C*
103000970526     C* EMISSIONE MESSAGGIO DI ERRORE
103100970526     C                   MOVEL     MSG(2)        DSMSMS
103200970526     C                   END                                                    W=0
103300970526     C                   END                                                    FUNKEY NE *BLANKS
103400970526     C*
103500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
103600970526     C     W             IFNE      0
103700970526     C     FUNKEY        OREQ      *BLANKS
103800970526     C*
103900970526     C     FUNKEY        IFEQ      *BLANKS
104000970526     C                   EXSR      ENTGES
104100970526     C                   END
104200970526     C     FUNKEY        IFEQ      '03'
104300970526     C                   EXSR      F03GES
104400970526     C                   END
104500970526     C     FUNKEY        IFEQ      '05'
104600970526     C                   EXSR      F05GES
104700970526     C                   END
104800970526     C     FUNKEY        IFEQ      '06'
104900970526     C                   EXSR      F06GES
105000970526     C                   END
105100970526     C     FUNKEY        IFEQ      '12'
105200970526     C                   EXSR      F12GES
105300970526     C                   END
105400970526     C     FUNKEY        IFEQ      '23'
105500970526     C                   EXSR      F23GES
105600970526     C                   END
105700970526     C     FUNKEY        IFEQ      '24'
105800970526     C                   EXSR      F24GES
105900970526     C                   END
106000970526     C     FUNKEY        IFEQ      'RU'
106100970526     C                   EXSR      RUPGES
106200970526     C                   END
106300970526     C*
106400970526     C                   END
106500970526     C*
106600970526     C                   ENDSR
106700970526     C*------------------------------------------------------------------------*
106800970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
106900970526     C*------------------------------------------------------------------------*
107000970526     C     FUNVAR        BEGSR
107100970526     C*
107200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
107300970526     C     KEY           IFEQ      ENTER
107400970526     C                   MOVEL     *BLANKS       FUNKEY
107500970526     C                   END
107600970526     C     KEY           IFEQ      F1
107700970526     C                   MOVEL     '01'          FUNKEY
107800970526     C                   END
107900970526     C     KEY           IFEQ      F2
108000970526     C                   MOVEL     '02'          FUNKEY
108100970526     C                   END
108200970526     C     KEY           IFEQ      F3
108300970526     C                   MOVEL     '03'          FUNKEY
108400970526     C                   END
108500970526     C     KEY           IFEQ      F4
108600970526     C                   MOVEL     '04'          FUNKEY
108700970526     C                   END
108800970526     C     KEY           IFEQ      F5
108900970526     C                   MOVEL     '05'          FUNKEY
109000970526     C                   END
109100970526     C     KEY           IFEQ      F6
109200970526     C                   MOVEL     '06'          FUNKEY
109300970526     C                   END
109400970526     C     KEY           IFEQ      F7
109500970526     C                   MOVEL     '07'          FUNKEY
109600970526     C                   END
109700970526     C     KEY           IFEQ      F8
109800970526     C                   MOVEL     '08'          FUNKEY
109900970526     C                   END
110000970526     C     KEY           IFEQ      F9
110100970526     C                   MOVEL     '09'          FUNKEY
110200970526     C                   END
110300970526     C     KEY           IFEQ      F10
110400970526     C                   MOVEL     '10'          FUNKEY
110500970526     C                   END
110600970526     C     KEY           IFEQ      F11
110700970526     C                   MOVEL     '11'          FUNKEY
110800970526     C                   END
110900970526     C     KEY           IFEQ      F12
111000970526     C                   MOVEL     '12'          FUNKEY
111100970526     C                   END
111200970526     C     KEY           IFEQ      F13
111300970526     C                   MOVEL     '13'          FUNKEY
111400970526     C                   END
111500970526     C     KEY           IFEQ      F14
111600970526     C                   MOVEL     '14'          FUNKEY
111700970526     C                   END
111800970526     C     KEY           IFEQ      F15
111900970526     C                   MOVEL     '15'          FUNKEY
112000970526     C                   END
112100970526     C     KEY           IFEQ      F16
112200970526     C                   MOVEL     '16'          FUNKEY
112300970526     C                   END
112400970526     C     KEY           IFEQ      F17
112500970526     C                   MOVEL     '17'          FUNKEY
112600970526     C                   END
112700970526     C     KEY           IFEQ      F18
112800970526     C                   MOVEL     '18'          FUNKEY
112900970526     C                   END
113000970526     C     KEY           IFEQ      F19
113100970526     C                   MOVEL     '19'          FUNKEY
113200970526     C                   END
113300970526     C     KEY           IFEQ      F20
113400970526     C                   MOVEL     '20'          FUNKEY
113500970526     C                   END
113600970526     C     KEY           IFEQ      F21
113700970526     C                   MOVEL     '21'          FUNKEY
113800970526     C                   END
113900970526     C     KEY           IFEQ      F22
114000970526     C                   MOVEL     '22'          FUNKEY
114100970526     C                   END
114200970526     C     KEY           IFEQ      F23
114300970526     C                   MOVEL     '23'          FUNKEY
114400970526     C                   END
114500970526     C     KEY           IFEQ      F24
114600970526     C                   MOVEL     '24'          FUNKEY
114700970526     C                   END
114800970526     C     KEY           IFEQ      ROLLUP
114900970526     C                   MOVEL     'RU'          FUNKEY
115000970526     C                   END
115100970526     C     KEY           IFEQ      HELP
115200970526     C                   MOVEL     'HP'          FUNKEY
115300970526     C                   END
115400970526     C*
115500970526     C                   ENDSR
115600970526     C*------------------------------------------------------------------------*
115700970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
115800970526     C*------------------------------------------------------------------------*
115900970526     C     FUNGES        BEGSR
116000970526     C*
116100970526     C* PULISCO CAMPI DI LAVORO
116200970526     C                   Z-ADD     0             W                 5 0
116300970526     C*
116400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
116500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
116600970526     C     1             DO        24            I
116700970526     C                   MOVEL     FUD(I)        FUDRIG
116800970526     C*
116900970526     C* PULIZIA FUNZIONI PRECEDENTI
117000970526     C                   MOVEL     *BLANKS       FUV(I)
117100970526     C*
117200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
117300970526     C* (SALTA F23 E F24)
117400970526     C     FUDATT        IFEQ      'S'
117500970526     C     FUDFUN        ANDNE     '23'
117600970526     C     FUDFUN        ANDNE     '24'
117700970526     C                   ADD       1             W
117800970526     C                   MOVEL     FUDDES        FUV(W)
117900970526     C                   END
118000970526     C                   END                                                    DO WHILE
118100970526     C*
118200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
118300970526     C* ALTRIMENTI DISATTIVO F24
118400970526     C     1             DO        24            I
118500970526     C                   MOVEL     FUD(I)        FUDRIG
118600970526     C*
118700970526     C     FUDFUN        IFEQ      '24'
118800970526     C     W             IFGT      6
118900970526     C                   MOVEL     'S'           FUDATT
119000970526     C                   ELSE
119100970526     C                   MOVEL     *BLANKS       FUDATT
119200970526     C                   END                                                    W GT 8
119300970526     C                   MOVEL     FUDRIG        FUD(I)
119400970526     C                   END
119500970526     C                   END                                                    DO WHILE
119600970526     C*
119700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
119800970526     C     W             DIV       6             I
119900970526     C                   MVR                     Z
120000970526     C*
120100970526     C     Z             IFNE      0
120200970526     C     I             ADD       1             Z
120300970526     C                   ELSE
120400970526     C                   Z-ADD     I             Z
120500970526     C                   END
120600970526     C*
120700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
120800970526     C                   ADD       1             FUNGRU
120900970526     C     FUNGRU        IFGT      Z
121000970526     C     FUNGRU        ORLT      1
121100970526     C                   Z-ADD     1             FUNGRU
121200970526     C                   END
121300970526     C*
121400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
121500970526     C     FUNGRU        IFEQ      1
121600970526     C                   MOVEL     FUV(1)        FUN001
121700970526     C                   MOVEL     FUV(2)        FUN002
121800970526     C                   MOVEL     FUV(3)        FUN003
121900970526     C                   MOVEL     FUV(4)        FUN004
122000970526     C                   MOVEL     FUV(5)        FUN005
122100970526     C                   MOVEL     FUV(6)        FUN006
122200970526     C                   END
122300970526     C*
122400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
122500970526     C     FUNGRU        IFEQ      2
122600970526     C                   MOVEL     FUV(7)        FUN001
122700970526     C                   MOVEL     FUV(8)        FUN002
122800970526     C                   MOVEL     FUV(9)        FUN003
122900970526     C                   MOVEL     FUV(10)       FUN004
123000970526     C                   MOVEL     FUV(11)       FUN005
123100970526     C                   MOVEL     FUV(12)       FUN006
123200970526     C                   END
123300970526     C*
123400970526     C* TERZO GRUPPO DI 6 FUNZIONI
123500970526     C     FUNGRU        IFEQ      3
123600970526     C                   MOVEL     FUV(13)       FUN001
123700970526     C                   MOVEL     FUV(14)       FUN002
123800970526     C                   MOVEL     FUV(15)       FUN003
123900970526     C                   MOVEL     FUV(16)       FUN004
124000970526     C                   MOVEL     FUV(17)       FUN005
124100970526     C                   MOVEL     FUV(18)       FUN006
124200970526     C                   END
124300970526     C*
124400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
124500970526     C     FUNGRU        IFEQ      4
124600970526     C                   MOVEL     FUV(19)       FUN001
124700970526     C                   MOVEL     FUV(20)       FUN002
124800970526     C                   MOVEL     FUV(21)       FUN003
124900970526     C                   MOVEL     FUV(22)       FUN004
125000970526     C                   MOVEL     *BLANKS       FUN005
125100970526     C                   MOVEL     *BLANKS       FUN006
125200970526     C                   END
125300970526     C*
125400970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
125500970526     C                   MOVEL     *BLANKS       FUN007
125600970526     C                   MOVEL     *BLANKS       FUN008
125700970526     C*
125800970526     C     1             DO        24            I
125900970526     C                   MOVEL     FUD(I)        FUDRIG
126000970526     C     FUDFUN        IFEQ      '23'
126100970526     C     FUDATT        ANDEQ     'S'
126200970526     C                   MOVEL     FUDDES        FUN007
126300970526     C                   END
126400970526     C     FUDFUN        IFEQ      '24'
126500970526     C     FUDATT        ANDEQ     'S'
126600970526     C                   MOVEL     FUDDES        FUN008
126700970526     C                   END
126800970526     C                   END                                                    DO
126900970526     C*
127000970526     C                   ENDSR
127100970526     C*------------------------------------------------------------------------*
127200970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
127300970526     C*------------------------------------------------------------------------*
127400970526     C     OPZGES        BEGSR
127500970526     C*
127600970526     C* PULISCO CAMPI DI LAVORO
127700970526     C                   Z-ADD     0             W                 5 0
127800970526     C*
127900970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
128000970526     C* E CARICO TABELLA OPZIONI ATTIVE
128100970526     C     1             DO        24            I
128200970526     C                   MOVEL     OPD(I)        OPDRIG
128300970526     C*
128400970526     C* PULIZIA OPZIONI PRECEDENTI
128500970526     C                   MOVEL     *BLANKS       OPV(I)
128600970526     C*
128700970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
128800970526     C     OPDATT        IFEQ      'S'
128900970526     C                   ADD       1             W
129000970526     C                   MOVEL     OPDDES        OPV(W)
129100970526     C                   END
129200970526     C                   END                                                    DO WHILE
129300970526     C*
129400970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
129500970526     C* ALTRIMENTI DISATTIVO F23
129600970526     C     1             DO        24            I
129700970526     C                   MOVEL     FUD(I)        FUDRIG
129800970526     C*
129900970526     C     FUDFUN        IFEQ      '23'
130000970526     C     W             IFGT      8
130100970526     C                   MOVEL     'S'           FUDATT
130200970526     C                   ELSE
130300970526     C                   MOVEL     *BLANKS       FUDATT
130400970526     C                   END                                                    W GT 8
130500970526     C                   MOVEL     FUDRIG        FUD(I)
130600970526     C                   END
130700970526     C                   END                                                    DO WHILE
130800970526     C*
130900970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
131000970526     C     W             DIV       8             I
131100970526     C                   MVR                     Z                 5 0
131200970526     C*
131300970526     C     Z             IFNE      0
131400970526     C     I             ADD       1             Z
131500970526     C                   ELSE
131600970526     C                   Z-ADD     I             Z
131700970526     C                   END
131800970526     C*
131900970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
132000970526     C                   ADD       1             OPZGRU
132100970526     C     OPZGRU        IFGT      Z
132200970526     C     OPZGRU        ORLT      1
132300970526     C                   Z-ADD     1             OPZGRU
132400970526     C                   END
132500970526     C*
132600970526     C* PRIMO GRUPPO DI 8 OPZIONI
132700970526     C     OPZGRU        IFEQ      1
132800970526     C                   MOVEL     OPV(1)        OPZ001
132900970526     C                   MOVEL     OPV(2)        OPZ002
133000970526     C                   MOVEL     OPV(3)        OPZ003
133100970526     C                   MOVEL     OPV(4)        OPZ004
133200970526     C                   MOVEL     OPV(5)        OPZ005
133300970526     C                   MOVEL     OPV(6)        OPZ006
133400970526     C                   MOVEL     OPV(7)        OPZ007
133500970526     C                   MOVEL     OPV(8)        OPZ008
133600970526     C                   END
133700970526     C*
133800970526     C* SECONDO GRUPPO DI 8 OPZIONI
133900970526     C     OPZGRU        IFEQ      2
134000970526     C                   MOVEL     OPV(9)        OPZ001
134100970526     C                   MOVEL     OPV(10)       OPZ002
134200970526     C                   MOVEL     OPV(11)       OPZ003
134300970526     C                   MOVEL     OPV(12)       OPZ004
134400970526     C                   MOVEL     OPV(13)       OPZ005
134500970526     C                   MOVEL     OPV(14)       OPZ006
134600970526     C                   MOVEL     OPV(15)       OPZ007
134700970526     C                   MOVEL     OPV(16)       OPZ008
134800970526     C                   END
134900970526     C*
135000970526     C* TERZO GRUPPO DI 8 OPZIONI
135100970526     C     OPZGRU        IFEQ      3
135200970526     C                   MOVEL     OPV(17)       OPZ001
135300970526     C                   MOVEL     OPV(18)       OPZ002
135400970526     C                   MOVEL     OPV(19)       OPZ003
135500970526     C                   MOVEL     OPV(20)       OPZ004
135600970526     C                   MOVEL     OPV(21)       OPZ005
135700970526     C                   MOVEL     OPV(22)       OPZ006
135800970526     C                   MOVEL     OPV(23)       OPZ007
135900970526     C                   MOVEL     OPV(24)       OPZ008
136000970526     C                   END
136100970526     C*
136200970526     C                   ENDSR
136300970526     C*------------------------------------------------------------------------*
136400970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
136500970526     C*------------------------------------------------------------------------*
136600970526     C     OPZCON        BEGSR
136700970526     C*
136800970526     C* PULISCO CAMPI DI LAVORO
136900970526     C                   Z-ADD     0             I                 5 0
137000970526     C                   Z-ADD     0             W                 5 0
137100970526     C*
137200970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
137300970526     C     1             DO        24            I
137400970526     C                   MOVEL     OPD(I)        OPDRIG
137500970526     C*
137600970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
137700970526     C     OPZKEY        IFEQ      OPDOPZ
137800970526     C     OPZKEY        OREQ      OPDOP1
137900970526     C*
138000970526     C     OPDATT        IFEQ      'S'
138100970526     C                   MOVEL     OPDOPZ        OPZKEY
138200970526     C                   Z-ADD     I             W
138300970526     C                   END
138400970526     C                   END
138500970526     C*
138600970526     C                   END                                                    DO WHILE
138700970526     C*
138800970526     C* OPZIONE NON ATTIVA (ERRORE)
138900970526     C     W             IFEQ      0
139000970526     C*
139100970526     C* OPZIONE NON OK
139200970526     C                   Z-ADD     1             OPZOK
139300970526     C*
139400970526     C* EMISSIONE MESSAGGIO DI ERRORE
139500970526     C                   MOVEL     MSG(1)        DSMSMS
139600970526     C                   ELSE
139700970526     C*
139800970526     C* OPZIONE ATTIVA (PROCEDI ...)
139900970526     C*
140000970526     C     OPZKEY        IFEQ      ' 1'
140100970526     C                   EXSR      O01GES
140200970526     C                   END
140300970526     C     OPZKEY        IFEQ      ' 2'
140400970526     C                   EXSR      O02GES
140500970526     C                   END
140600970526     C     OPZKEY        IFEQ      ' 4'
140700970526     C                   EXSR      O04GES
140800970526     C                   END
140900970526     C     OPZKEY        IFEQ      ' 5'
141000970526     C                   EXSR      O05GES
141100970526     C                   END
141200970526     C     OPZKEY        IFEQ      ' 7'
141300970526     C                   EXSR      O07GES
141400970526     C                   END
141500970526     C                   END                                                    OPZ.NON ATTIVA
141600970526     C*
141700970526     C                   ENDSR
141800970526     C*------------------------------------------------------------------------*
141900970526     C* NOTE COSTRUZIONE SCHIERE
142000970526     C*------------------------------------------------------------------------*
142100970526     C*
142200970526     C* -MSG-MESSAGGI
142300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
142400970526     C* (NON TOCCARE MSG,1 E MSG,2)
142500970526     C*
142600970526     C* -CMD-COMANDI
142700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
142800970526     C*
142900970526     C* -OPD-OPZIONI
143000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
143100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
143200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
143300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
143400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
143500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
143600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
143700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
143800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
143900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
144000970526     C*
144100970526     C* -FUD-FUNZIONI
144200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
144300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
144400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
144500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
144600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
144700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
144800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
144900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
145000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
145100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
145200970526     O*------------------------------------------------------------------------*
145300970526** DEC - DECODIFICHE
145400970526    GESTIONE                                                     1
145500970526INTERROGAZIONE                                                   2
145600970530     SCELTA                                                      3
1457009705264
1458009705265
145900970526** MSG - MESSAGGI
146000970526TISI41R- Opzione non gestita                                                   1
146100970526TISI41R- Tasto funzionale non gestito                                          2
146200970526TISI41R- Non abilitato alla gestione di questo CAP                             3
146300970526TISI41R- Record annullato. Consentito solo il ripristino                       4
146400970526TISI41R- Record non annullato. Impossibile ripristinare                        5
146500970526TISI41R- Accettato solo un parametro di inizio lista                           6
146600970527TISI41R- 7                                                                     7
146700970527TISI41R- 8                                                                     8
146800970526TISI41R- 9                                                                     9
146900970526TISI41R- 10                                                                   10
147000970526TISI41R- 11                                                                   11
147100970526TISI41R- 12                                                                   12
147200970526TISI41R- 13                                                                   13
147300970526TISI41R- 14                                                                   14
147400970526TISI41R- 15                                                                   15
147500970526TISI41R- 16                                                                   16
147600970526TISI41R- 17                                                                   17
147700970526TISI41R- 18                                                                   18
147800970526TISI41R- 19                                                                   19
147900970526TISI41R- 20                                                                   20
148000970526** OPD - OPZIONI
148100970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
148200970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
148300970526                   -3 -     (S/N)=N                        opz. 03¬
148400970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
148500970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
148600970526                   -6 -     (S/N)=N                        opz. 06¬
148700970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
148800970527                   -8 -     (S/N)=N                        opz. 08¬
148900970527                   -9 -     (S/N)=N                        opz. 09¬
149000970527                   -10-     (S/N)=N                        opz. 10¬
149100970526                   -  -     (S/N)=N                        opz. 11¬
149200970526                   -  -     (S/N)=N                        opz. 12¬
149300970526                   -  -     (S/N)=N                        opz. 13¬
149400970526                   -  -     (S/N)=N                        opz. 14¬
149500970526                   -  -     (S/N)=N                        opz. 15¬
149600970526                   -  -     (S/N)=N                        opz. 16¬
149700970526                   -  -     (S/N)=N                        opz. 17¬
149800970526                   -  -     (S/N)=N                        opz. 18¬
149900970526                   -  -     (S/N)=N                        opz. 19¬
150000970526                   -  -     (S/N)=N                        opz. 20¬
150100970526                   -  -     (S/N)=N                        opz. 21¬
150200970526                   -  -     (S/N)=N                        opz. 22¬
150300970526                   -  -     (S/N)=N                        opz. 23¬
150400970526                   -  -     (S/N)=N                        opz. 24¬
150500970526** FUD - FUNZIONI
150600970526                   -        (S/N)=N                        funz.01¬
150700970526                   -        (S/N)=N                        funz.02¬
150800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
150900970526                   -        (S/N)=N                        funz.04¬
151000970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
151100970526F06=Inserimento    -        (S/N)=N                        funz.06¬
151200970526                   -        (S/N)=N                        funz.07¬
151300970527                   -        (S/N)=N                        funz.08¬
151400970527                   -        (S/N)=N                        funz.09¬
151500970527                   -        (S/N)=N                        funz.10¬
151600970526                   -        (S/N)=N                        funz.11¬
151700970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
151800970526                   -        (S/N)=N                        funz.13¬
151900970526                   -        (S/N)=N                        funz.14¬
152000970526                   -        (S/N)=N                        funz.15¬
152100970526                   -        (S/N)=N                        funz.16¬
152200970526                   -        (S/N)=N                        funz.17¬
152300970526                   -        (S/N)=N                        funz.18¬
152400970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
152500970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
152600970619                   -        (S/N)=S                        funz.21¬
152700970619                   -        (S/N)=N                        funz.22¬
152800970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
152900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
