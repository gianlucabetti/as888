000100970526     H*------------------------------------------------------------------------*
000200970528     H* CAPPARIO - GESTIONE TEMPI TRAZIONE - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970528     FTISI23D   CF   E             WORKSTN INFDS(DEVDS1)
000900970528     F                                     SFILE(SI23D05:SFLULT)
001000970528     F                                     SFILE(SI23DMS:MSGULT)
001100970528     FAZORG01L  IF   E           K DISK
001200970528     FTABEL00F  IF   E           K DISK
001300970526     F*------------
001400970528     F* TEMPI TRAZIONE
001500970526     F*------------
001600970619     FAZCAS01L  IF   E           K DISK
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
002800970528     D*------------
002900970528     D* COMANDI
003000970528     D*------------
003100970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
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
004700970527     D FUV             S             19    DIM(24)
004800970528     D*------------
004900970528     D* ORGANIGRAMMA
005000970528     D*------------
005100971118     D ORG             S              3  0 DIM(999)                             *CODICE
005200971118     D DORG            S             30    DIM(999)                             *DESCRIZIONE
005300970528     D*------------
005400970528     D* TIPI SERVIZIO
005500970528     D*------------
005600970528     D TSP             S              1    DIM(10)                              *CODICE
005700090605     D DTSP            S              8    DIM(10)                              *DESCRIZIONE
005800970526     D*------------------------------------------------------------------------*
005900970526     D* INPUT
006000970526     D*------------------------------------------------------------------------*
006100970526     D*------------
006200970526     D* RIDEFINIZIONE OPZIONI VIDEO
006300970526     D*------------
006400970526     D OPDDS           DS
006500970526     D  OPDOPZ                 1      2
006600970526     D  OPDDE1                 3     19
006700970526     D  OPDOP1                21     22
006800970526     D  OPDFI0                23     34
006900970526     D  OPDATT                35     35
007000970526     D  OPDFI1                36     80
007100970526     D  OPDDES                 1     19
007200970526     D  OPDRIG                 1     80
007300970526     D*------------
007400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007500970526     D*------------
007600970526     D FUDDS           DS
007700970526     D  FUDDE1                 1      1
007800970526     D  FUDFUN                 2      3
007900970526     D  FUDDE2                 4     19
008000970526     D  FUDFI0                20     34
008100970526     D  FUDATT                35     35
008200970526     D  FUDFI1                36     80
008300970526     D  FUDDES                 1     19
008400970526     D  FUDRIG                 1     80
008500970526     D*------------
008600970526     D* DS GESTIONE TASTI FUNZIONALI
008700970526     D*------------
008800970526     D DSKEY         E DS
008900970526     D DEVDS1          DS
009000970526     D  KEY                  369    369
009100970526     D*------------
009200970526     D* DS "XMSG" - GESTIONE MESSAGGI
009300970526     D*------------
009400970526     D DSMSG         E DS
009500970526     D*------------
009600020919     D* DS REPERIMENTO DATI UTENTE
009700970526     D*------------
009800020919     D TIBS34DS      E DS                                                       *Profili utente
009900020919     D DDATIUTE      E DS                                                       *Dati utente
010000020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010100970527     D*------------------
010200970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
010300970527     D*------------------
010400970527     D WLBDA8          DS                  INZ
010500970527     D  G08DAT                 1      8  0
010600970527     D  G08INV                 9     16  0
010700970527     D  G08ERR                17     17
010800970527     D  G08TGI                18     22  0
010900970528     D*------------
011000970528     D* DS TIPI SERVIZIO
011100970528     D*------------
011200970528     D DS5E          E DS
011300970526     D*------------
011400970526     D* DS DI PROCEDURA
011500970526     D*------------
011600970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
011700970526     D*------------
011800970526     D* ARCHITETTURA
011900970526     D*------------
012000970526     D KPJBA         E DS
012100970526     C*------------------------------------------------------------------------*
012200970526     C* MAIN LINES
012300970526     C*------------------------------------------------------------------------*
012400970526     C*
012500970526     C* SE NON CI SONO ERRORI, PROSEGUE
012600970526IF  1C     ERRL00        IFNE      '1'
012700970526     C*
012800970526     C* CARICA IL SFILE
012900970526     C                   EXSR      POSSFL
013000970526     C*
013100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013500970526     C*
013600970526     C* REIMPOSTA ERRORE DI PROCERURA
013700970526     C                   MOVEL     '0'           ERRL00
013800970526     C*
013900970526     C* EMETTE IL SFILE
014000970526     C                   EXSR      OUTSFL
014100970526     C*
014200970526     C* GESTIONE TASTI FUNZIONALI
014300970526     C                   EXSR      FUNCON
014400970526     C*
014500970526E   2C                   ENDDO
014600970527     C*
014700970527     C* CI SONO ERRORI IN ENTRATA
014800970527X   1C                   ELSE
014900970527     C                   MOVEL     '1'           D00ERR
015000970527     C                   MOVEL     DSMSMS        D00MSG
015100970526E   1C                   ENDIF
015200970526     C* FINE PGM
015300970526     C                   SETON                                        LR
015400970526     C*------------------------------------------------------------------------*
015500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015600970526     C*------------------------------------------------------------------------*
015700970526     C     F03GES        BEGSR
015800970526     C*
015900970526     C* IMPOSTA "PREMUTO F03"
016000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016100970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016600970526     C*------------------------------------------------------------------------*
016700970526     C     F05GES        BEGSR
016800970526     C*
016900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     F06GES        BEGSR
017600970526     C*
017700970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
017800020919IF  1C     DUTLPO        IFNE      'S'
017900970526     C                   MOVEL     '1'           ERRL00
018000970526     C                   MOVEL     MSG(3)        DSMSMS
018100970526E   1C                   ENDIF
018200970526     C*
018300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018400970602     C                   MOVEL     *BLANKS       DSPT
018500970602     C                   Z-ADD     *ZEROS        DSLP
018600970602     C                   Z-ADD     *ZEROS        DSLA
018700970602     C                   MOVEL     *BLANKS       DSTS
018800970602     C                   Z-ADD     *ZEROS        DSDD
018900970602     C                   MOVEL     DEPOP0        WOP0
019000970529     C                   MOVEL     'F06'         WOP1
019100970528     C                   MOVEL     'TISI24R'     WPGM              8
019200970526     C                   EXSR      LANPGM                                       *LANCIA PGM
019300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019400970526E   1C                   ENDIF
019500970526     C*
019600970526     C                   ENDSR
019700971121     C*------------------------------------------------------------------------*
019800971121     C* F08GES - GESTIONE F08 -> AUTOMATISMI
019900971121     C*------------------------------------------------------------------------*
020000971121     C     F08GES        BEGSR
020100971121     C*
020200971121     C                   CALL      'TISI27R'
020300971121     C                   PARM                    KPJBA
020400971121     C*
020500971121     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F12GES - GESTIONE F12 -> RITORNO
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F12GES        BEGSR
021000970526     C*
021100970526     C* IMPOSTA "PREMUTO F12"
021200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021300970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
021400970526     C*
021500970526     C                   ENDSR
021600970528     C*------------------------------------------------------------------------*
021700970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021800970528     C*------------------------------------------------------------------------*
021900970528     C     F21GES        BEGSR
022000970528     C*
022100970612     C                   MOVEL     VIDPTA        DSPT
022200970612     C                   Z-ADD     VIDLNP        DSLP
022300970612     C                   Z-ADD     VIDLNA        DSLA
022400970612     C                   MOVEL     VIDTSP        DSTS
022500970612     C                   Z-ADD     VIHDDE        DSDD
022600970612     C                   MOVEL     DEPOP0        WOP0
022700970612     C                   MOVE      '03'          WOP0
022800970612     C                   MOVEL     'F21'         WOP1
022900970612     C                   MOVEL     'TISI25R'     WPGM              8
023000970612     C                   EXSR      LANPGM
023100970528     C*
023200970528     C                   ENDSR
023300970528     C*------------------------------------------------------------------------*
023400970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
023500970528     C*------------------------------------------------------------------------*
023600970528     C     F22GES        BEGSR
023700970528     C*
023800970528     C                   Z-ADD     80            LENGH            15 5
023900970528     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
024000970528     C                   PARM                    CMD(1)
024100970528     C                   PARM                    LENGH
024200970528     C*
024300970528     C                   ENDSR
024400970526     C*------------------------------------------------------------------------*
024500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
024600970526     C*------------------------------------------------------------------------*
024700970526     C     F23GES        BEGSR
024800970526     C*
024900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
025000970526     C*
025100970526     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025400970526     C*------------------------------------------------------------------------*
025500970526     C     F24GES        BEGSR
025600970526     C*
025700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025800970526     C*
025900970526     C                   ENDSR
026000970526     C*------------------------------------------------------------------------*
026100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
026200970526     C*------------------------------------------------------------------------*
026300970526     C     RUPGES        BEGSR
026400970526     C*
026500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
026600970526     C*
026700970526     C                   ENDSR
026800970526     C*------------------------------------------------------------------------*
026900970526     C* ENTGES - GESTIONE ENTER -> INVIO
027000970526     C*------------------------------------------------------------------------*
027100970526     C     ENTGES        BEGSR
027200970526     C*
027300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
027400970526     C                   EXSR      NEWPOS
027500970526     C*
027600970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
027700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027800970526IF  2C     SWPOSI        IFNE      0
027900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
028000970526X   2C                   ELSE
028100970526IF  3C     SFLULT        IFNE      0
028200970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
028300970526     C*
028400970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
028500970526IF  4C     SWMODI        IFNE      0
028600970526     C                   EXSR      POSSFL
028700970526E   4C                   ENDIF
028800970526E   3C                   ENDIF
028900970526E   2C                   ENDIF
029000970526E   1C                   ENDIF
029100970526     C*
029200970526     C                   ENDSR
029300970526     C*------------------------------------------------------------------------*
029400970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
029500970526     C*------------------------------------------------------------------------*
029600970526     C     O01GES        BEGSR
029700970526     C*
029800970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
029900970528     C                   MOVEL     VIDPTA        D00SPT
030000970528     C                   Z-ADD     VIDLNP        D00SLP
030100970528     C                   Z-ADD     VIDLNA        D00SLA
030200970528     C                   MOVEL     VIDTSP        D00STS
030300970528     C                   Z-ADD     VIHDDE        D00SDD
030400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
030500970526     C*
030600970526     C                   ENDSR
030700970526     C*------------------------------------------------------------------------*
030800970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
030900970526     C*------------------------------------------------------------------------*
031000970526     C     O02GES        BEGSR
031100970526     C*
031200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
031300970526IF  1C     VIDATB        IFEQ      'A'
031400970526     C                   MOVEL     '1'           ERRL00
031500970526     C                   MOVEL     MSG(4)        DSMSMS
031600970526E   1C                   ENDIF
031700970527     C* MODIFICA
031800970526IF  1C     ERRL00        IFEQ      '0'
031900970602     C                   MOVEL     VIDPTA        DSPT
032000970602     C                   Z-ADD     VIDLNP        DSLP
032100970602     C                   Z-ADD     VIDLNA        DSLA
032200970602     C                   MOVEL     VIDTSP        DSTS
032300970602     C                   Z-ADD     VIHDDE        DSDD
032400970602     C                   MOVEL     DEPOP0        WOP0
032500970526     C                   MOVEL     'O02'         WOP1
032600970528     C                   MOVEL     'TISI24R'     WPGM
032700970526     C                   EXSR      LANPGM
032800970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
032900970526E   1C                   ENDIF
033000970526     C*
033100970526     C                   ENDSR
033200970526     C*------------------------------------------------------------------------*
033300970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
033400970526     C*------------------------------------------------------------------------*
033500970526     C     O04GES        BEGSR
033600970526     C*
033700970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
033800970526IF  1C     VIDATB        IFEQ      'A'
033900970526     C                   MOVEL     '1'           ERRL00
034000970526     C                   MOVEL     MSG(4)        DSMSMS
034100970526E   1C                   ENDIF
034200970527     C* ANNULLAMENTO
034300970526IF  1C     ERRL00        IFEQ      '0'
034400970602     C                   MOVEL     VIDPTA        DSPT
034500970602     C                   Z-ADD     VIDLNP        DSLP
034600970602     C                   Z-ADD     VIDLNA        DSLA
034700970602     C                   MOVEL     VIDTSP        DSTS
034800970602     C                   Z-ADD     VIHDDE        DSDD
034900970602     C                   MOVEL     DEPOP0        WOP0
035000970526     C                   MOVEL     'O04'         WOP1
035100970528     C                   MOVEL     'TISI24R'     WPGM
035200970526     C                   EXSR      LANPGM
035300970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
035400970526E   1C                   ENDIF
035500970526     C*
035600970526     C                   ENDSR
035700970526     C*------------------------------------------------------------------------*
035800970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
035900970526     C*------------------------------------------------------------------------*
036000970526     C     O05GES        BEGSR
036100970526     C*
036200970602     C                   MOVEL     VIDPTA        DSPT
036300970602     C                   Z-ADD     VIDLNP        DSLP
036400970602     C                   Z-ADD     VIDLNA        DSLA
036500970602     C                   MOVEL     VIDTSP        DSTS
036600970602     C                   Z-ADD     VIHDDE        DSDD
036700970602     C                   MOVEL     DEPOP0        WOP0
036800970526     C                   MOVEL     'O05'         WOP1
036900970528     C                   MOVEL     'TISI24R'     WPGM
037000970526     C                   EXSR      LANPGM
037100970526     C*
037200970526     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
037500970526     C*------------------------------------------------------------------------*
037600970526     C     O07GES        BEGSR
037700970526     C*
037800970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
037900970526IF  1C     VIDATB        IFNE      'A'
038000970526     C                   MOVEL     '1'           ERRL00
038100970526     C                   MOVEL     MSG(5)        DSMSMS
038200970526E   1C                   ENDIF
038300970526     C* RIPRISITINO
038400970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
038500970602     C                   MOVEL     VIDPTA        DSPT
038600970602     C                   Z-ADD     VIDLNP        DSLP
038700970602     C                   Z-ADD     VIDLNA        DSLA
038800970602     C                   MOVEL     VIDTSP        DSTS
038900970602     C                   Z-ADD     VIHDDE        DSDD
039000970602     C                   MOVEL     DEPOP0        WOP0
039100970526     C                   MOVEL     'O07'         WOP1
039200970528     C                   MOVEL     'TISI24R'     WPGM
039300970526     C                   EXSR      LANPGM
039400970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
039500970526E   1C                   ENDIF
039600970526     C*
039700970526     C                   ENDSR
039800970526     C*------------------------------------------------------------------------*
039900970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
040000970526     C*------------------------------------------------------------------------*
040100970526     C     NEWPOS        BEGSR
040200970526     C*
040300970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
040400970526     C*         1= RICHIESTO POSIZIONAMENTO
040500970526     C                   Z-ADD     0             SWPOSI            7 0
040600970526     C*
040700970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
040800970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
040900970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
041000970526     C                   Z-ADD     0             SWMODI            1 0
041100970530     C*
041200970530     C* REIMPOSTA INDICATORI DI ERRORE
041300970530     C                   SETOFF                                       808182
041400970530     C                   SETOFF                                       83
041500970526     C*
041600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
041700970528IF  1C     VIDPPT        IFNE      DEPPTA
041800970528     C     VIDPLP        ORNE      DEPLNP
041900970528     C     VIDPLA        ORNE      DEPLNA
042000970528     C     VIDPTS        ORNE      DEPTSP
042100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
042200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
042300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
042400970528     C                   MOVEL     VIDPPT        DEPPTA                         *DEPOSITI
042500970528     C                   Z-ADD     VIDPLP        DEPLNP
042600970528     C                   Z-ADD     VIDPLA        DEPLNA
042700970528     C                   MOVEL     VIDPTS        DEPTSP
042800970526E   2C                   ENDIF
042900970526E   1C                   ENDIF
043000970526     C*
043100970526     C                   ENDSR
043200970526     C*------------------------------------------------------------------------*
043300970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
043400970526     C*------------------------------------------------------------------------*
043500970528     C     SETFIL        BEGSR
043600970526     C*
043700970528     C* POSIZIONAMENTO SUL FILE
043800970619     C                   MOVEL     VIDPPT        KCSPTA
043900970619     C                   Z-ADD     VIDPLP        KCSLNP
044000970619     C                   Z-ADD     VIDPLA        KCSLNA
044100970619     C     KEYCAS        SETLL     AZCAS01L                           99
044200970528     C*
044300970526     C* LETTURA RECORD
044400970526IF  1C     *IN99         IFEQ      *OFF
044500970526     C                   EXSR      LETFIL
044600970526E   1C                   ENDIF
044700970526     C*
044800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
044900970526IF  1C     *IN99         IFEQ      *ON
045000970526     C                   Z-ADD     1             SFLEOF
045100970526E   1C                   ENDIF
045200970526     C*
045300970526     C                   ENDSR
045400970526     C*------------------------------------------------------------------------*
045500970526     C* LETFIL - LETTURA RECORD
045600970526     C*------------------------------------------------------------------------*
045700970526     C     LETFIL        BEGSR
045800970526     C*
045900970526     C* LEGGE FINO A:
046000970526     C*     - FINE FILE
046100970526     C*     - TROVATO RECORD VALIDO
046200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
046300970526DO  1C     *IN99         DOWEQ     *OFF
046400970526     C     WRECOK        ANDEQ     'N'
046500970526     C*
046600970528     C* LETTURE SUCCESSIVE ALLA PRIMA
046700970619     C                   READ      AZCAS01L                               99
046800970528     C*
046900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
047000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
047100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
047200970530X   2C                   ELSE
047300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
047400970530E   2C                   ENDIF
047500970526E   1C                   ENDDO
047600970526     C*
047700970526     C                   ENDSR
047800970526     C*------------------------------------------------------------------------*
047900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
048000970526     C*------------------------------------------------------------------------*
048100970526     C     CHKREC        BEGSR
048200970526     C*
048300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
048400970528     C*
048500970528     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
048600970528IF  1C     VIDPTS        IFNE      *BLANKS
048700970619IF  2C     VIDPTS        IFNE      CASTSP
048800970528     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048900970528E   2C                   ENDIF
049000970528E   1C                   ENDIF
049100970526     C*
049200970526     C                   ENDSR
049300970526     C*------------------------------------------------------------------------*
049400970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
049500970526     C*------------------------------------------------------------------------*
049600970526     C     IRESFL        BEGSR
049700970526     C*
049800970526     C                   MOVEL     *BLANKS       OPZKEY
049900970619     C                   Z-ADD     CASDDE        VIHDDE
050000970619     C                   MOVEL     CASATB        VIDATB
050100970619     C                   MOVEL     CASPTA        VIDPTA
050200970619     C                   Z-ADD     CASLNP        VIDLNP
050300970619     C                   Z-ADD     CASLNA        VIDLNA
050400970619     C                   MOVEL     CASTSP        VIDTSP
050500970619     C                   Z-ADD     CASTIN        VIDTIN
050600970619     C                   Z-ADD     CASTCN        VIDTCN
050700970528     C* DECODIFICHE
050800970528     C                   SETOFF                                           28
050900970528     C                   Z-ADD     1             I
051000970619     C     CASLNP        LOOKUP    ORG(I)                                 28
051100970528IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
051200970528     C                   MOVEL     DORG(I)       VIDDLP
051300970528X   1C                   ELSE                                                   *NON TROVATO
051400970528     C                   MOVEL     *BLANKS       VIDDLP
051500970528E   1C                   ENDIF
051600970526     C*
051700970528     C                   SETOFF                                           28
051800970528     C                   Z-ADD     1             I
051900970619     C     CASLNA        LOOKUP    ORG(I)                                 28
052000970528IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
052100970528     C                   MOVEL     DORG(I)       VIDDLA
052200970528X   1C                   ELSE                                                   *NON TROVATO
052300970528     C                   MOVEL     *BLANKS       VIDDLA
052400970528E   1C                   ENDIF
052500970528     C*
052600970528     C                   SETOFF                                           28
052700970528     C                   Z-ADD     1             I
052800970619     C     CASTSP        LOOKUP    TSP(I)                                 28
052900970528IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
053000970528     C                   MOVEL     DTSP(I)       VIDDTS
053100970528X   1C                   ELSE                                                   *NON TROVATO
053200970528     C                   MOVEL     *BLANKS       VIDDTS
053300970528E   1C                   ENDIF
053400971121     C*
053500971121     C                   Z-ADD     CASDDE        G08INV
053600971121     C                   Z-ADD     *ZEROS        G08DAT
053700971121     C                   MOVEL     '3'           G08ERR
053800971121     C                   CALL      'XSRDA8'
053900971121     C                   PARM                    WLBDA8
054000971121     C                   Z-ADD     G08DAT        VIDDDE
054100970528     C*
054200970526     C                   ENDSR
054300970526     C*------------------------------------------------------------------------*
054400970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
054500970526     C*------------------------------------------------------------------------*
054600970526     C     CHKPAR        BEGSR
054700970526     C*
054800970526     C                   ENDSR
054900970526     C*------------------------------------------------------------------------*
055000970526     C* LANPGM - LANCIA UN PROGRAMMA
055100970526     C*------------------------------------------------------------------------*
055200970526     C     LANPGM        BEGSR
055300970526     C*
055400970526     C* IMPOSTA LA DS DI PROCEDURA
055500970602     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
055600970602     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
055700970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
055800970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
055900970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
056000970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
056100970602     C                   MOVEL     DSPT          D00SPT                         *PARTENZA/TRAZIONE/A
056200970602     C                   Z-ADD     DSLP          D00SLP                         *LINEA PARTENZA
056300970602     C                   Z-ADD     DSLA          D00SLA                         *LINEA ARRIVO
056400970602     C                   MOVEL     DSTS          D00STS                         *TIPO SERVIZIO
056500970602     C                   Z-ADD     DSDD          D00SDD                         *DECORRENZA
056600970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI DATI
056700970530     C                   MOVEL     *BLANKS       D00TSP
056800970530     C                   Z-ADD     *ZEROS        D00TFP
056900970530     C                   MOVEL     *BLANKS       D00LOC
057000970603     C                   MOVEL     *BLANKS       D00SIN
057100970530     C                   MOVEL     *BLANKS       D00EPA
057200970530     C                   Z-ADD     *ZEROS        D00ETP
057300970530     C                   Z-ADD     *ZEROS        D00ETA
057400970530     C                   Z-ADD     *ZEROS        D00EDD
057500970530     C*
057600970526     C                   CALL      WPGM
057700970612     C                   PARM                    KPJBA
057800970612     C                   PARM                    DSSI00
057900970526     C*
058000970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
058100970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
058200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
058300970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
058400970526X   1C                   ELSE                                                   *NON PREMUTO F03
058500970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
058600970526E   1C                   ENDIF
058700970526     C*
058800970526     C                   ENDSR
058900970527     C*------------------------------------------------------------------------*
059000970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
059100970527     C*------------------------------------------------------------------------*
059200970527     C     CARTAB        BEGSR
059300970528     C*---
059400970528     C* ORGANIGRAMMA
059500970528     C*---
059600970528     C                   Z-ADD     *ZEROS        I                 5 0
059700970528     C                   Z-ADD     *ZEROS        ORG
059800970528     C                   MOVEL     *BLANKS       DORG
059900970528     C     *LOVAL        SETLL     AZORG01L
060000970528     C                   READ      AZORG01L                               99
060100970528DO  1C     *IN99         DOWEQ     *OFF
060200970528IF  2C     ORGFVA        IFEQ      *BLANKS
060300970528     C     ORGFAG        ANDNE     'V'
060400970528     C                   ADD       1             I
060500970528     C                   Z-ADD     ORGFIL        ORG(I)
060600970528     C                   MOVEL     ORGDES        DORG(I)
060700970528E   2C                   ENDIF
060800970528     C                   READ      AZORG01L                               99
060900970528E   1C                   ENDDO                                                  *FINE ORGANIGRAMMA
061000970528     C*---
061100970528     C* TIPI SERVIZIO
061200970528     C*---
061300970528     C                   Z-ADD     *ZEROS        I                 5 0
061400970528     C                   MOVEL     *BLANKS       TSP
061500970528     C                   MOVEL     *BLANKS       DTSP
061600020919     C                   MOVEL     1             KTBKUT
061700970528     C                   MOVEL     '5E'          KTBCOD
061800970528     C     KEYTAB        SETLL     TABEL00F
061900970528     C     KEYTAB        READE     TABEL00F                               99
062000970528DO  1C     *IN99         DOWEQ     *OFF
062100970528IF  2C     TBLFLG        IFEQ      *BLANKS
062200970528     C                   ADD       1             I
062300970528     C                   MOVEL     TBLUNI        DS5E
062400970528     C                   MOVEL     TBLKEY        TSP(I)
062500090605     C                   MOVEL     §5ED08        DTSP(I)
062600970528E   2C                   ENDIF
062700970528     C     KEYTAB        READE     TABEL00F                               99
062800970528E   1C                   ENDDO                                                  *FINE TABELLA
062900970527     C*
063000970527     C                   ENDSR
063100970526     C*------------------------------------------------------------------------*
063200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
063300970526     C*------------------------------------------------------------------------*
063400970526     C     CHKFOP        BEGSR
063500970626     C*
063600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
063700970626IF  1C     DEPOP0        IFEQ      'Z01'
063800970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
063900970626     C                   MOVEL     'S'           OPDATT
064000970626     C                   MOVEL     OPDRIG        OPD(1)
064100970626X   1C                   ELSE
064200970626     C* OPZIONI
064300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
064400970626     C                   MOVEL     'S'           OPDATT
064500970626     C                   MOVEL     OPDRIG        OPD(5)
064600970626     C* FUNZIONI
064700970626E   1C                   ENDIF
064800970626     C*
064900970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
065000970626IF  1C     DEPOP0        IFEQ      'Z02'
065100970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
065200970626     C                   MOVEL     'S'           OPDATT
065300970626     C                   MOVEL     OPDRIG        OPD(2)
065400970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
065500970626     C                   MOVEL     'S'           OPDATT
065600970626     C                   MOVEL     OPDRIG        OPD(4)
065700970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
065800970626     C                   MOVEL     'S'           OPDATT
065900970626     C                   MOVEL     OPDRIG        OPD(7)
066000970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
066100970626     C                   MOVEL     'S'           FUDATT
066200970626     C                   MOVEL     FUDRIG        FUD(6)
066300971121     C                   MOVEL     FUD(8)        FUDRIG                         *AUTOMATISMO
066400971121     C                   MOVEL     'S'           FUDATT
066500971121     C                   MOVEL     FUDRIG        FUD(8)
066600970626E   1C                   ENDIF
066700970626     C*
066800970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
066900020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
067000970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
067100970626     C                   MOVEL     'N'           OPDATT
067200970626     C                   MOVEL     OPDRIG        OPD(4)
067300970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
067400970626     C                   MOVEL     'N'           OPDATT
067500970626     C                   MOVEL     OPDRIG        OPD(7)
067600970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
067700970626     C                   MOVEL     'N'           FUDATT
067800970626     C                   MOVEL     FUDRIG        FUD(6)
067900971121     C                   MOVEL     FUD(8)        FUDRIG                         *NO AUTOMATISMO
068000971121     C                   MOVEL     'N'           FUDATT
068100971121     C                   MOVEL     FUDRIG        FUD(8)
068200970626E   1C                   ENDIF
068300970526     C*
068400970526     C* GESTIONE OPZIONI A VIDEO
068500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
068600970526     C                   EXSR      OPZGES
068700970526     C*
068800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
068900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
069000970526     C                   EXSR      FUNGES
069100970526     C*
069200970526     C                   ENDSR
069300020919     C*--------------------------------------------------------------------------------------------*
069400020919     C* REPERISCE I DATI UTENTE
069500020919     C*--------------------------------------------------------------------------------------------*
069600020919     C     REPDATIUTE    BEGSR
069700020919     C*
069800020919     C* INIZIALIZZA VARIABILI DI WRK
069900020919     C                   CLEAR                   TIBS34DS
070000020919     C                   CLEAR                   AZUTEDS
070100020919     C                   CLEAR                   DDATIUTE
070200020919     C*
070300020919     C     *DTAARA       DEFINE    §azute        azuteds
070400020919     C     *DTAARA       DEFINE    §datiute      ddatiute
070500020919     C                   IN(E)     *DTAARA
070600020919if  1C                   IF        %Error
070700020919     c                   EVAL      I34Tla = 'L'
070800020919     C                   CALL      'TIBS34R'
070900020919     C                   PARM                    Tibs34Ds
071000020919     C                   IN        *DTAARA
071100020919e   1C                   ENDIF
071200020919     C*
071300020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
071400020919     C                   MOVEL(P)  RSUT          DSFIRS           20
071500020919     C*
071600020919     C                   ENDSR
071700970526     C*------------------------------------------------------------------------*
071800970526     C* *INZSR - OPERAZIONI INIZIALI
071900970526     C*------------------------------------------------------------------------*
072000970526     C     *INZSR        BEGSR
072100970526     C*---
072200970526     C* RICEVIMENTO PARAMETRI
072300970526     C*---
072400970526     C     *ENTRY        PLIST
072500970612     C                   PARM                    KPJBA
072600970612     C                   PARM                    DSSI00
072700970526     C*
072800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
072900970526     C     XMSGPA        PLIST
073000970526     C                   PARM                    DSMSMS
073100970526     C                   PARM                    DSMSPG
073200970526     C                   PARM                    DSMSKE
073300970526     C                   PARM                    DSMSER
073400970526     C*
073500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
073600970526     C                   CALL      'XKEY'
073700970526     C                   PARM                    DSKEY
073800970526     C*---
073900970526     C* VARIABILI NON RIFERITE AL DATA BASE
074000970526     C*---
074100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
074200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
074300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
074400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
074500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
074600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
074700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
074800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
074900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
075000970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
075100970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
075200970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
075300970529     C     *LIKE         DEFINE    D00OP1        WOP1
075400970602     C     *LIKE         DEFINE    D00SPT        DSPT
075500970602     C     *LIKE         DEFINE    D00SLP        DSLP
075600970602     C     *LIKE         DEFINE    D00SLA        DSLA
075700970602     C     *LIKE         DEFINE    D00STS        DSTS
075800970602     C     *LIKE         DEFINE    D00SDD        DSDD
075900970526     C*---
076000970526     C* VARIABILI DI CONTROLLO
076100970526     C*---
076200970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
076300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
076400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
076500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
076600970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
076700970528     C                   Z-ADD     9             SFLPAG                         *N°RIGHE X PAG
076800970526     C*
076900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
077000970528     C                   MOVEL     'TISI23R'     DSMSPG
077100970528     C                   MOVEL     'TISI23R'     MSGQUE
077200970526     C                   MOVEL     *BLANKS       MSGKEY
077300970526     C                   MOVEL     *BLANKS       DSMSMS
077400970526     C                   MOVEL     *BLANKS       DSMSKE
077500970526     C                   Z-ADD     *ZEROS        DSMSER
077600970526     C*---
077700970526     C* VARIABILI RIFERITE AL DATA BASE
077800970526     C*---
077900970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
078000970528     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
078100970528     C     *LIKE         DEFINE    TBLCOD        KTBCOD
078200970619     C     *LIKE         DEFINE    CASPTA        KCSPTA                         *AZCAS00F
078300970619     C     *LIKE         DEFINE    CASLNP        KCSLNP
078400970619     C     *LIKE         DEFINE    CASLNA        KCSLNA
078500970619     C     *LIKE         DEFINE    CASPTA        DEPPTA                         *DEPOSITI
078600970619     C     *LIKE         DEFINE    CASLNP        DEPLNP
078700970619     C     *LIKE         DEFINE    CASLNA        DEPLNA
078800970619     C     *LIKE         DEFINE    CASTSP        DEPTSP
078900970526     C*---
079000970526     C* CHIAVI DI LETTURA
079100970526     C*---
079200970528     C* LETTURA AZORG01L - COMPLETA
079300970528     C     KEYORG        KLIST
079400970528     C                   KFLD                    KORFIL                         *FILIALE
079500970528     C* LETTURA TABEL00F - PARZIALE
079600970528     C     KEYTAB        KLIST
079700970528     C                   KFLD                    KTBKUT                         *UTENTE
079800970528     C                   KFLD                    KTBCOD                         *TABELLA
079900970619     C* LETTURA AZCAS01L - PARZIALE
080000970619     C     KEYCAS        KLIST
080100970619     C                   KFLD                    KCSPTA                         *PARTENZA/ARRIVO
080200970619     C                   KFLD                    KCSLNP                         *LINEA PARTENZA
080300970619     C                   KFLD                    KCSLNA                         *LINEA ARRIVO
080400970527     C*
080500970527     C* REPERIMENTO DATI UTENTE
080600020919     C                   EXSR      REPDATIUTE
080700970527     C*
080800970527     C* CARICA LE TABELLE OCCORRENTI
080900970527     C                   EXSR      CARTAB
081000970526     C*
081100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
081200970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
081300970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
081400970526     C*
081500970526     C* CALCOLA LA DATA CORRENTE
081600970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
081700970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
081800970526     C                   Z-ADD     WN8           G08DAT
081900970526     C                   Z-ADD     *ZEROS        G08INV
082000970526     C                   MOVEL     '0'           G08ERR
082100970526     C                   CALL      'XSRDA8'
082200970526     C                   PARM                    WLBDA8
082300970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
082400970526     C*
082500970526     C* IMPOSTA I PARAMETRI DI INGRESSO
082600970528     C                   MOVEL     D00SPT        VIDPPT                         *PARTENZA/ARRIVO
082700970528     C                   Z-ADD     D00SLP        VIDPLP                         *LINEA PARTENZA
082800970528     C                   Z-ADD     D00SLA        VIDPLA                         *LINEA ARRIVO
082900970528     C                   MOVEL     D00STS        VIDPTS                         *TIPO SERVIZIO
083000970526     C                   EXSR      CHKPAR
083100970526IF  1C     ERRL00        IFEQ      '1'
083200970526     C                   MOVEL     '1'           D00ERR
083300970526     C                   MOVEL     DSMSMS        D00MSG
083400970526X   1C                   ELSE
083500970528     C                   MOVEL     VIDPPT        DEPPTA                         *DEPOSITI
083600970528     C                   Z-ADD     VIDPLP        DEPLNP
083700970528     C                   Z-ADD     VIDPLA        DEPLNA
083800970528     C                   MOVEL     VIDPTS        DEPTSP
083900970526E   1C                   ENDIF
084000970526     C*
084100970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
084200970526     C                   EXSR      CHKFOP
084300970530     C*
084400970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
084500970530IF  1C     DEPOP0        IFEQ      'Z01'
084600970530     C                   MOVEL     DEC(3)        VIDING
084700970530E   1C                   ENDIF
084800970530IF  1C     DEPOP0        IFEQ      'Z02'
084900970530     C                   MOVEL     DEC(1)        VIDING
085000970530E   1C                   ENDIF
085100970530IF  1C     DEPOP0        IFEQ      'Z05'
085200970530     C                   MOVEL     DEC(2)        VIDING
085300970530E   1C                   ENDIF
085400970526     C*
085500970526     C                   ENDSR
085600970526     C*------------------------------------------------------------------------*
085700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
085800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
085900970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
086000970529     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
086100970619     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
086200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
086300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
086400970526     C*------------------------------------------------------------------------*
086500970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
086600970526     C*------------------------------------------------------------------------*
086700970526     C     POSSFL        BEGSR
086800970526     C*
086900970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
087000970526     C                   EXSR      CLRSFL
087100970526     C*
087200970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
087300970526     C                   EXSR      SETFIL
087400970526     C*
087500970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
087600970526     C     SFLEOF        IFEQ      0
087700970526     C                   EXSR      CARSFL
087800970526     C                   END
087900970526     C*
088000970526     C                   ENDSR
088100970526     C*------------------------------------------------------------------------*
088200970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
088300970526     C*------------------------------------------------------------------------*
088400970526     C     ENTATT        BEGSR
088500970526     C*
088600970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
088700970526     C                   Z-ADD     0             OPZOK             1 0
088800970526     C*
088900970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
089000970526     C                   Z-ADD     0             SFLVAR            5 0
089100970526     C*
089200970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
089300970526     C                   Z-ADD     0             SFLERR
089400970526     C*
089500970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
089600970526     C                   Z-ADD     SFLULT        WFLULT
089700970526     C*
089800970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
089900970528     C                   READC     SI23D05                                98
090000970526     C     *IN98         DOWEQ     '0'
090100970526     C*
090200970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
090300970526     C                   ADD       1             SFLVAR
090400970526     C*
090500970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
090600970526     C     OPZOK         IFEQ      0
090700970526     C*
090800970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
090900970526     C     OPZKEY        IFNE      *BLANKS
091000970526     C                   EXSR      OPZCON
091100970526     C                   END
091200970526     C*
091300970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
091400970526     C*            2. PULIZIA SCELTA IN SFL
091500970526     C     OPZOK         IFEQ      0
091600970526     C                   MOVEL     '0'           *IN41
091700970526     C                   MOVEL     *BLANKS       OPZKEY
091800970526     C                   ELSE
091900970526     C*
092000970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
092100970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
092200970526     C                   MOVEL     '1'           *IN41
092300970526     C                   Z-ADD     SFLULT        SFLERR
092400970526     C                   END
092500970526     C                   ELSE
092600970526     C*
092700970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
092800970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
092900970526     C                   MOVEL     '1'           *IN41
093000970526     C                   END                                                    OPZOK=0
093100970526     C*
093200970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
093300970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
093400970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
093500970526     C     OPZOK         IFNE      0
093600970526     C                   Z-ADD     SFLERR        SFLNBR
093700970526     C                   ELSE
093800970526     C                   Z-ADD     SFLULT        SFLNBR
093900970526     C                   END
094000970526     C*
094100970526     C* AGGIORNO RECORD SFL
094200970528     C                   UPDATE    SI23D05
094300970526     C*
094400970528     C                   READC     SI23D05                                98
094500970526     C                   END                                                    DO WHILE
094600970526     C*
094700970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
094800970526     C                   Z-ADD     WFLULT        SFLULT
094900970526     C*
095000970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
095100970526     C     SFLVAR        IFEQ      0
095200970526     C*---                 MOVEL'1'       FINL00
095300970526     C                   END
095400970526     C*
095500970526     C                   ENDSR
095600970526     C*------------------------------------------------------------------------*
095700970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
095800970526     C*------------------------------------------------------------------------*
095900970526     C     CLRSFL        BEGSR
096000970526     C*
096100970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
096200970526     C* (0=NON FINE FILE  1=FINE FILE)
096300970526     C                   Z-ADD     0             SFLEOF
096400970526     C*
096500970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
096600970526     C                   Z-ADD     0             SFLULT
096700970526     C*
096800970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
096900970526     C                   MOVEL     '1'           *IN40
097000970528     C                   WRITE     SI23D06
097100970526     C                   MOVEL     '0'           *IN40
097200970526     C*
097300970526     C                   ENDSR
097400970526     C*------------------------------------------------------------------------*
097500970526     C* CARSFL - CARICAMENTO SFL
097600970526     C*------------------------------------------------------------------------*
097700970526     C     CARSFL        BEGSR
097800970526     C*
097900970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
098000970526     C     SFLULT        IFEQ      0
098100970526     C                   EXSR      SETFIL
098200970526     C                   END
098300970526     C*
098400970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
098500970526     C                   Z-ADD     SFLULT        WFLULT
098600970526     C*
098700970526     C* CICLO:
098800970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
098900970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
099000970526     C                   Z-ADD     1             SFLRIG
099100970526     C     SFLRIG        DOWLE     SFLPAG
099200970526     C     SFLEOF        ANDEQ     0
099300970526     C*
099400970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
099500970526     C                   MOVEL     *BLANKS       OPZKEY
099600970526     C*
099700970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
099800970526     C                   EXSR      IRESFL
099900970526     C*
100000970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
100100970526     C                   ADD       1             SFLULT
100200970526     C*
100300970526     C* SCRITTURA RECORD SFL
100400970528     C                   WRITE     SI23D05
100500970526     C                   ADD       1             SFLRIG
100600970526     C*
100700970526     C* LETTURA FILE DA CARICARE
100800970526     C                   EXSR      LETFIL
100900970526     C                   END                                                    DO WHILE
101000970526     C*
101100970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
101200970526     C     WFLULT        IFNE      SFLULT
101300970526     C                   Z-ADD     WFLULT        SFLNBR
101400970526     C                   ADD       1             SFLNBR
101500970526     C                   END
101600970526     C*
101700970526     C                   ENDSR
101800970526     C*------------------------------------------------------------------------*
101900970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
102000970526     C*------------------------------------------------------------------------*
102100970526     C     OUTSFL        BEGSR
102200970526     C*
102300970526     C* WRITE RECORD TESTATA VIDEO
102400970528     C                   WRITE     SI23D01
102500970526     C*
102600970526     C* VISUALIZZAZIONE MESSAGGIO
102700970526     C                   EXSR      SNDMSG
102800970526     C*
102900970526     C* WRITE RECORD OPZIONI VIDEO
103000970528     C                   WRITE     SI23D02
103100970526     C*
103200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
103300970528     C                   WRITE     SI23D03
103400970526     C*
103500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
103600970528     C                   WRITE     SI23D04
103700970526     C*
103800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
103900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
104000970526     C     SFLEOF        IFEQ      0
104100970526     C                   MOVEL     '0'           *IN42
104200970526     C                   ELSE
104300970526     C*
104400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
104500970526     C                   MOVEL     '1'           *IN42
104600970526     C                   END
104700970526     C*
104800970526     C* WRITE RECORD TASTI FUNZIONALI
104900970528     C                   WRITE     SI23D09
105000970526     C*
105100970529     C* CASO DI SFL VUOTO
105200970526     C     SFLULT        IFLE      0
105300970526     C*
105400970526     C*    - WRITE RECORD VUOTO
105500970528     C                   WRITE     SI23D0A
105600970528     C                   READ      SI23D0A                                99
105700970526     C*
105800970526     C                   ELSE
105900970526     C*
106000970529     C* CASO DI SFL CON RECORDS
106100970526     C*
106200970526     C*    - WRITE RECORD CONTROLLO SFL
106300970528     C                   WRITE     SI23D06
106400970528     C                   READ      SI23D06                                99
106500970526     C                   END
106600970526     C*
106700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
106800970528     C                   READ      SI23D03                                99
106900970526     C*
107000970526     C                   ENDSR
107100970526     C*------------------------------------------------------------------------*
107200970526     C* SNDMSG - MANDA MESSAGGIO
107300970526     C*------------------------------------------------------------------------*
107400970526     C     SNDMSG        BEGSR
107500970526     C*
107600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
107700970526     C                   MOVEL     '1'           *IN40
107800970528     C                   WRITE     SI23DMC
107900970526     C                   MOVEL     '0'           *IN40
108000970526     C*
108100970526     C* VISUALIZZAZIONE MESSAGGIO
108200970526     C                   Z-ADD     0             MSGULT
108300970526     C                   CALL      'XMSG'        XMSGPA
108400970526     C     DSMSER        IFEQ      0
108500970526     C                   MOVEL     DSMSKE        MSGKEY
108600970526     C                   END
108700970526     C                   Z-ADD     1             MSGULT
108800970528     C                   WRITE     SI23DMS                                      SFL MESS
108900970528     C                   WRITE     SI23DMC                                      CONTR MESS
109000970526     C*
109100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
109200970526     C                   MOVEL     *BLANKS       DSMSMS
109300970526     C*
109400970526     C                   ENDSR
109500970526     C*------------------------------------------------------------------------*
109600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
109700970526     C*------------------------------------------------------------------------*
109800970526     C     FUNCON        BEGSR
109900970526     C*
110000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
110100970526     C                   EXSR      FUNVAR
110200970526     C*
110300970526     C* PULISCO CAMPI DI LAVORO
110400970526     C                   Z-ADD     0             I                 5 0
110500970526     C                   Z-ADD     0             W                 5 0
110600970526     C*
110700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
110800970526     C     FUNKEY        IFNE      *BLANKS
110900970526     C     1             DO        24            I
111000970526     C                   MOVEL     FUD(I)        FUDRIG
111100970526     C*
111200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
111300970526     C     FUNKEY        IFEQ      FUDFUN
111400970526     C     FUDATT        ANDEQ     'S'
111500970526     C                   Z-ADD     I             W
111600970526     C                   END
111700970526     C                   END                                                    DO WHILE
111800970526     C*
111900970526     C* FUNZIONE NON ATTIVA (ERRORE)
112000970526     C     W             IFEQ      0
112100970526     C*
112200970526     C* EMISSIONE MESSAGGIO DI ERRORE
112300970526     C                   MOVEL     MSG(2)        DSMSMS
112400970526     C                   END                                                    W=0
112500970526     C                   END                                                    FUNKEY NE *BLANKS
112600970526     C*
112700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
112800970526     C     W             IFNE      0
112900970526     C     FUNKEY        OREQ      *BLANKS
113000970526     C*
113100970526     C     FUNKEY        IFEQ      *BLANKS
113200970526     C                   EXSR      ENTGES
113300970526     C                   END
113400970526     C     FUNKEY        IFEQ      '03'
113500970526     C                   EXSR      F03GES
113600970526     C                   END
113700970526     C     FUNKEY        IFEQ      '05'
113800970526     C                   EXSR      F05GES
113900970526     C                   END
114000970526     C     FUNKEY        IFEQ      '06'
114100970526     C                   EXSR      F06GES
114200970526     C                   END
114300971121     C     FUNKEY        IFEQ      '08'
114400971121     C                   EXSR      F08GES
114500971121     C                   END
114600970526     C     FUNKEY        IFEQ      '12'
114700970526     C                   EXSR      F12GES
114800970526     C                   END
114900970528     C     FUNKEY        IFEQ      '21'
115000970528     C                   EXSR      F21GES
115100970528     C                   END
115200970528     C     FUNKEY        IFEQ      '22'
115300970528     C                   EXSR      F22GES
115400970528     C                   END
115500970526     C     FUNKEY        IFEQ      '23'
115600970526     C                   EXSR      F23GES
115700970526     C                   END
115800970526     C     FUNKEY        IFEQ      '24'
115900970526     C                   EXSR      F24GES
116000970526     C                   END
116100970526     C     FUNKEY        IFEQ      'RU'
116200970526     C                   EXSR      RUPGES
116300970526     C                   END
116400970526     C*
116500970526     C                   END
116600970526     C*
116700970526     C                   ENDSR
116800970526     C*------------------------------------------------------------------------*
116900970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
117000970526     C*------------------------------------------------------------------------*
117100970526     C     FUNVAR        BEGSR
117200970526     C*
117300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
117400970526     C     KEY           IFEQ      ENTER
117500970526     C                   MOVEL     *BLANKS       FUNKEY
117600970526     C                   END
117700970526     C     KEY           IFEQ      F1
117800970526     C                   MOVEL     '01'          FUNKEY
117900970526     C                   END
118000970526     C     KEY           IFEQ      F2
118100970526     C                   MOVEL     '02'          FUNKEY
118200970526     C                   END
118300970526     C     KEY           IFEQ      F3
118400970526     C                   MOVEL     '03'          FUNKEY
118500970526     C                   END
118600970526     C     KEY           IFEQ      F4
118700970526     C                   MOVEL     '04'          FUNKEY
118800970526     C                   END
118900970526     C     KEY           IFEQ      F5
119000970526     C                   MOVEL     '05'          FUNKEY
119100970526     C                   END
119200970526     C     KEY           IFEQ      F6
119300970526     C                   MOVEL     '06'          FUNKEY
119400970526     C                   END
119500970526     C     KEY           IFEQ      F7
119600970526     C                   MOVEL     '07'          FUNKEY
119700970526     C                   END
119800970526     C     KEY           IFEQ      F8
119900970526     C                   MOVEL     '08'          FUNKEY
120000970526     C                   END
120100970526     C     KEY           IFEQ      F9
120200970526     C                   MOVEL     '09'          FUNKEY
120300970526     C                   END
120400970526     C     KEY           IFEQ      F10
120500970526     C                   MOVEL     '10'          FUNKEY
120600970526     C                   END
120700970526     C     KEY           IFEQ      F11
120800970526     C                   MOVEL     '11'          FUNKEY
120900970526     C                   END
121000970526     C     KEY           IFEQ      F12
121100970526     C                   MOVEL     '12'          FUNKEY
121200970526     C                   END
121300970526     C     KEY           IFEQ      F13
121400970526     C                   MOVEL     '13'          FUNKEY
121500970526     C                   END
121600970526     C     KEY           IFEQ      F14
121700970526     C                   MOVEL     '14'          FUNKEY
121800970526     C                   END
121900970526     C     KEY           IFEQ      F15
122000970526     C                   MOVEL     '15'          FUNKEY
122100970526     C                   END
122200970526     C     KEY           IFEQ      F16
122300970526     C                   MOVEL     '16'          FUNKEY
122400970526     C                   END
122500970526     C     KEY           IFEQ      F17
122600970526     C                   MOVEL     '17'          FUNKEY
122700970526     C                   END
122800970526     C     KEY           IFEQ      F18
122900970526     C                   MOVEL     '18'          FUNKEY
123000970526     C                   END
123100970526     C     KEY           IFEQ      F19
123200970526     C                   MOVEL     '19'          FUNKEY
123300970526     C                   END
123400970526     C     KEY           IFEQ      F20
123500970526     C                   MOVEL     '20'          FUNKEY
123600970526     C                   END
123700970526     C     KEY           IFEQ      F21
123800970526     C                   MOVEL     '21'          FUNKEY
123900970526     C                   END
124000970526     C     KEY           IFEQ      F22
124100970526     C                   MOVEL     '22'          FUNKEY
124200970526     C                   END
124300970526     C     KEY           IFEQ      F23
124400970526     C                   MOVEL     '23'          FUNKEY
124500970526     C                   END
124600970526     C     KEY           IFEQ      F24
124700970526     C                   MOVEL     '24'          FUNKEY
124800970526     C                   END
124900970526     C     KEY           IFEQ      ROLLUP
125000970526     C                   MOVEL     'RU'          FUNKEY
125100970526     C                   END
125200970526     C     KEY           IFEQ      HELP
125300970526     C                   MOVEL     'HP'          FUNKEY
125400970526     C                   END
125500970526     C*
125600970526     C                   ENDSR
125700970526     C*------------------------------------------------------------------------*
125800970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
125900970526     C*------------------------------------------------------------------------*
126000970526     C     FUNGES        BEGSR
126100970526     C*
126200970526     C* PULISCO CAMPI DI LAVORO
126300970526     C                   Z-ADD     0             W                 5 0
126400970526     C*
126500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
126600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
126700970526     C     1             DO        24            I
126800970526     C                   MOVEL     FUD(I)        FUDRIG
126900970526     C*
127000970526     C* PULIZIA FUNZIONI PRECEDENTI
127100970526     C                   MOVEL     *BLANKS       FUV(I)
127200970526     C*
127300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
127400970526     C* (SALTA F23 E F24)
127500970526     C     FUDATT        IFEQ      'S'
127600970526     C     FUDFUN        ANDNE     '23'
127700970526     C     FUDFUN        ANDNE     '24'
127800970526     C                   ADD       1             W
127900970526     C                   MOVEL     FUDDES        FUV(W)
128000970526     C                   END
128100970526     C                   END                                                    DO WHILE
128200970526     C*
128300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
128400970526     C* ALTRIMENTI DISATTIVO F24
128500970526     C     1             DO        24            I
128600970526     C                   MOVEL     FUD(I)        FUDRIG
128700970526     C*
128800970526     C     FUDFUN        IFEQ      '24'
128900970526     C     W             IFGT      6
129000970526     C                   MOVEL     'S'           FUDATT
129100970526     C                   ELSE
129200970526     C                   MOVEL     *BLANKS       FUDATT
129300970526     C                   END                                                    W GT 8
129400970526     C                   MOVEL     FUDRIG        FUD(I)
129500970526     C                   END
129600970526     C                   END                                                    DO WHILE
129700970526     C*
129800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
129900970526     C     W             DIV       6             I
130000970526     C                   MVR                     Z
130100970526     C*
130200970526     C     Z             IFNE      0
130300970526     C     I             ADD       1             Z
130400970526     C                   ELSE
130500970526     C                   Z-ADD     I             Z
130600970526     C                   END
130700970526     C*
130800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
130900970526     C                   ADD       1             FUNGRU
131000970526     C     FUNGRU        IFGT      Z
131100970526     C     FUNGRU        ORLT      1
131200970526     C                   Z-ADD     1             FUNGRU
131300970526     C                   END
131400970526     C*
131500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
131600970526     C     FUNGRU        IFEQ      1
131700970526     C                   MOVEL     FUV(1)        FUN001
131800970526     C                   MOVEL     FUV(2)        FUN002
131900970526     C                   MOVEL     FUV(3)        FUN003
132000970526     C                   MOVEL     FUV(4)        FUN004
132100970526     C                   MOVEL     FUV(5)        FUN005
132200970526     C                   MOVEL     FUV(6)        FUN006
132300970526     C                   END
132400970526     C*
132500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
132600970526     C     FUNGRU        IFEQ      2
132700970526     C                   MOVEL     FUV(7)        FUN001
132800970526     C                   MOVEL     FUV(8)        FUN002
132900970526     C                   MOVEL     FUV(9)        FUN003
133000970526     C                   MOVEL     FUV(10)       FUN004
133100970526     C                   MOVEL     FUV(11)       FUN005
133200970526     C                   MOVEL     FUV(12)       FUN006
133300970526     C                   END
133400970526     C*
133500970526     C* TERZO GRUPPO DI 6 FUNZIONI
133600970526     C     FUNGRU        IFEQ      3
133700970526     C                   MOVEL     FUV(13)       FUN001
133800970526     C                   MOVEL     FUV(14)       FUN002
133900970526     C                   MOVEL     FUV(15)       FUN003
134000970526     C                   MOVEL     FUV(16)       FUN004
134100970526     C                   MOVEL     FUV(17)       FUN005
134200970526     C                   MOVEL     FUV(18)       FUN006
134300970526     C                   END
134400970526     C*
134500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
134600970526     C     FUNGRU        IFEQ      4
134700970526     C                   MOVEL     FUV(19)       FUN001
134800970526     C                   MOVEL     FUV(20)       FUN002
134900970526     C                   MOVEL     FUV(21)       FUN003
135000970526     C                   MOVEL     FUV(22)       FUN004
135100970526     C                   MOVEL     *BLANKS       FUN005
135200970526     C                   MOVEL     *BLANKS       FUN006
135300970526     C                   END
135400970526     C*
135500970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
135600970526     C                   MOVEL     *BLANKS       FUN007
135700970526     C                   MOVEL     *BLANKS       FUN008
135800970526     C*
135900970526     C     1             DO        24            I
136000970526     C                   MOVEL     FUD(I)        FUDRIG
136100970526     C     FUDFUN        IFEQ      '23'
136200970526     C     FUDATT        ANDEQ     'S'
136300970526     C                   MOVEL     FUDDES        FUN007
136400970526     C                   END
136500970526     C     FUDFUN        IFEQ      '24'
136600970526     C     FUDATT        ANDEQ     'S'
136700970526     C                   MOVEL     FUDDES        FUN008
136800970526     C                   END
136900970526     C                   END                                                    DO
137000970526     C*
137100970526     C                   ENDSR
137200970526     C*------------------------------------------------------------------------*
137300970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
137400970526     C*------------------------------------------------------------------------*
137500970526     C     OPZGES        BEGSR
137600970526     C*
137700970526     C* PULISCO CAMPI DI LAVORO
137800970526     C                   Z-ADD     0             W                 5 0
137900970526     C*
138000970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
138100970526     C* E CARICO TABELLA OPZIONI ATTIVE
138200970526     C     1             DO        24            I
138300970526     C                   MOVEL     OPD(I)        OPDRIG
138400970526     C*
138500970526     C* PULIZIA OPZIONI PRECEDENTI
138600970526     C                   MOVEL     *BLANKS       OPV(I)
138700970526     C*
138800970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
138900970526     C     OPDATT        IFEQ      'S'
139000970526     C                   ADD       1             W
139100970526     C                   MOVEL     OPDDES        OPV(W)
139200970526     C                   END
139300970526     C                   END                                                    DO WHILE
139400970526     C*
139500970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
139600970526     C* ALTRIMENTI DISATTIVO F23
139700970526     C     1             DO        24            I
139800970526     C                   MOVEL     FUD(I)        FUDRIG
139900970526     C*
140000970526     C     FUDFUN        IFEQ      '23'
140100970526     C     W             IFGT      8
140200970526     C                   MOVEL     'S'           FUDATT
140300970526     C                   ELSE
140400970526     C                   MOVEL     *BLANKS       FUDATT
140500970526     C                   END                                                    W GT 8
140600970526     C                   MOVEL     FUDRIG        FUD(I)
140700970526     C                   END
140800970526     C                   END                                                    DO WHILE
140900970526     C*
141000970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
141100970526     C     W             DIV       8             I
141200970526     C                   MVR                     Z                 5 0
141300970526     C*
141400970526     C     Z             IFNE      0
141500970526     C     I             ADD       1             Z
141600970526     C                   ELSE
141700970526     C                   Z-ADD     I             Z
141800970526     C                   END
141900970526     C*
142000970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
142100970526     C                   ADD       1             OPZGRU
142200970526     C     OPZGRU        IFGT      Z
142300970526     C     OPZGRU        ORLT      1
142400970526     C                   Z-ADD     1             OPZGRU
142500970526     C                   END
142600970526     C*
142700970526     C* PRIMO GRUPPO DI 8 OPZIONI
142800970526     C     OPZGRU        IFEQ      1
142900970526     C                   MOVEL     OPV(1)        OPZ001
143000970526     C                   MOVEL     OPV(2)        OPZ002
143100970526     C                   MOVEL     OPV(3)        OPZ003
143200970526     C                   MOVEL     OPV(4)        OPZ004
143300970526     C                   MOVEL     OPV(5)        OPZ005
143400970526     C                   MOVEL     OPV(6)        OPZ006
143500970526     C                   MOVEL     OPV(7)        OPZ007
143600970526     C                   MOVEL     OPV(8)        OPZ008
143700970526     C                   END
143800970526     C*
143900970526     C* SECONDO GRUPPO DI 8 OPZIONI
144000970526     C     OPZGRU        IFEQ      2
144100970526     C                   MOVEL     OPV(9)        OPZ001
144200970526     C                   MOVEL     OPV(10)       OPZ002
144300970526     C                   MOVEL     OPV(11)       OPZ003
144400970526     C                   MOVEL     OPV(12)       OPZ004
144500970526     C                   MOVEL     OPV(13)       OPZ005
144600970526     C                   MOVEL     OPV(14)       OPZ006
144700970526     C                   MOVEL     OPV(15)       OPZ007
144800970526     C                   MOVEL     OPV(16)       OPZ008
144900970526     C                   END
145000970526     C*
145100970526     C* TERZO GRUPPO DI 8 OPZIONI
145200970526     C     OPZGRU        IFEQ      3
145300970526     C                   MOVEL     OPV(17)       OPZ001
145400970526     C                   MOVEL     OPV(18)       OPZ002
145500970526     C                   MOVEL     OPV(19)       OPZ003
145600970526     C                   MOVEL     OPV(20)       OPZ004
145700970526     C                   MOVEL     OPV(21)       OPZ005
145800970526     C                   MOVEL     OPV(22)       OPZ006
145900970526     C                   MOVEL     OPV(23)       OPZ007
146000970526     C                   MOVEL     OPV(24)       OPZ008
146100970526     C                   END
146200970526     C*
146300970526     C                   ENDSR
146400970526     C*------------------------------------------------------------------------*
146500970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
146600970526     C*------------------------------------------------------------------------*
146700970526     C     OPZCON        BEGSR
146800970526     C*
146900970526     C* PULISCO CAMPI DI LAVORO
147000970526     C                   Z-ADD     0             I                 5 0
147100970526     C                   Z-ADD     0             W                 5 0
147200970526     C*
147300970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
147400970526     C     1             DO        24            I
147500970526     C                   MOVEL     OPD(I)        OPDRIG
147600970526     C*
147700970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
147800970526     C     OPZKEY        IFEQ      OPDOPZ
147900970526     C     OPZKEY        OREQ      OPDOP1
148000970526     C*
148100970526     C     OPDATT        IFEQ      'S'
148200970526     C                   MOVEL     OPDOPZ        OPZKEY
148300970526     C                   Z-ADD     I             W
148400970526     C                   END
148500970526     C                   END
148600970526     C*
148700970526     C                   END                                                    DO WHILE
148800970526     C*
148900970526     C* OPZIONE NON ATTIVA (ERRORE)
149000970526     C     W             IFEQ      0
149100970526     C*
149200970526     C* OPZIONE NON OK
149300970526     C                   Z-ADD     1             OPZOK
149400970526     C*
149500970526     C* EMISSIONE MESSAGGIO DI ERRORE
149600970526     C                   MOVEL     MSG(1)        DSMSMS
149700970526     C                   ELSE
149800970526     C*
149900970526     C* OPZIONE ATTIVA (PROCEDI ...)
150000970526     C*
150100970526     C     OPZKEY        IFEQ      ' 1'
150200970526     C                   EXSR      O01GES
150300970526     C                   END
150400970526     C     OPZKEY        IFEQ      ' 2'
150500970526     C                   EXSR      O02GES
150600970526     C                   END
150700970526     C     OPZKEY        IFEQ      ' 4'
150800970526     C                   EXSR      O04GES
150900970526     C                   END
151000970526     C     OPZKEY        IFEQ      ' 5'
151100970526     C                   EXSR      O05GES
151200970526     C                   END
151300970526     C     OPZKEY        IFEQ      ' 7'
151400970526     C                   EXSR      O07GES
151500970526     C                   END
151600970526     C                   END                                                    OPZ.NON ATTIVA
151700970526     C*
151800970526     C                   ENDSR
151900970526     C*------------------------------------------------------------------------*
152000970526     C* NOTE COSTRUZIONE SCHIERE
152100970526     C*------------------------------------------------------------------------*
152200970526     C*
152300970526     C* -MSG-MESSAGGI
152400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
152500970526     C* (NON TOCCARE MSG,1 E MSG,2)
152600970526     C*
152700970526     C* -CMD-COMANDI
152800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
152900970526     C*
153000970526     C* -OPD-OPZIONI
153100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
153200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
153300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
153400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
153500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
153600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
153700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
153800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
153900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
154000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
154100970526     C*
154200970526     C* -FUD-FUNZIONI
154300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
154400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
154500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
154600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
154700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
154800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
154900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
155000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
155100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
155200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
155300970526     O*------------------------------------------------------------------------*
155400970526** DEC - DECODIFICHE
155500970526    GESTIONE                                                     1
155600970526INTERROGAZIONE                                                   2
155700970530     SCELTA                                                      3
1558009705264
1559009705265
156000970526** MSG - MESSAGGI
156100970528TISI23R- Opzione non gestita                                                   1
156200970528TISI23R- Tasto funzionale non gestito                                          2
156300970528TISI23R- Non abilitato alla gestione di questo CAP                             3
156400970528TISI23R- Record annullato. Consentito solo il ripristino                       4
156500970528TISI23R- Record non annullato. Impossibile ripristinare                        5
156600970528TISI23R- Accettato solo un parametro di inizio lista                           6
156700970528TISI23R- 7                                                                     7
156800970528TISI23R- 8                                                                     8
156900970528TISI23R- 9                                                                     9
157000970528TISI23R- 10                                                                   10
157100970530TISI23R- 11
157200970528TISI23R- 12                                                                   12
157300970528TISI23R- 13                                                                   13
157400970528TISI23R- 14                                                                   14
157500970528TISI23R- 15                                                                   15
157600970528TISI23R- 16                                                                   16
157700970528TISI23R- 17                                                                   17
157800970528TISI23R- 18                                                                   18
157900970528TISI23R- 19                                                                   19
158000970528TISI23R- 20                                                                   20
158100970528** CMD - COMANDI CL
158200970528WRKSPLF
158300970528
158400970528
158500970528
158600970528
158700970526** OPD - OPZIONI
158800970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
158900970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
159000970526                   -3 -     (S/N)=N                        opz. 03¬
159100970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
159200970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
159300970526                   -6 -     (S/N)=N                        opz. 06¬
159400970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
159500970527                   -8 -     (S/N)=N                        opz. 08¬
159600970527                   -9 -     (S/N)=N                        opz. 09¬
159700970527                   -10-     (S/N)=N                        opz. 10¬
159800970526                   -  -     (S/N)=N                        opz. 11¬
159900970526                   -  -     (S/N)=N                        opz. 12¬
160000970526                   -  -     (S/N)=N                        opz. 13¬
160100970526                   -  -     (S/N)=N                        opz. 14¬
160200970526                   -  -     (S/N)=N                        opz. 15¬
160300970526                   -  -     (S/N)=N                        opz. 16¬
160400970526                   -  -     (S/N)=N                        opz. 17¬
160500970526                   -  -     (S/N)=N                        opz. 18¬
160600970526                   -  -     (S/N)=N                        opz. 19¬
160700970526                   -  -     (S/N)=N                        opz. 20¬
160800970526                   -  -     (S/N)=N                        opz. 21¬
160900970526                   -  -     (S/N)=N                        opz. 22¬
161000970526                   -  -     (S/N)=N                        opz. 23¬
161100970526                   -  -     (S/N)=N                        opz. 24¬
161200970526** FUD - FUNZIONI
161300970526                   -        (S/N)=N                        funz.01¬
161400970526                   -        (S/N)=N                        funz.02¬
161500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
161600970526                   -        (S/N)=N                        funz.04¬
161700970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
161800970526F06=Inserimento    -        (S/N)=N                        funz.06¬
161900970526                   -        (S/N)=N                        funz.07¬
162000971121F08=Automatismi    -        (S/N)=N                        funz.08¬
162100970527                   -        (S/N)=N                        funz.09¬
162200970527                   -        (S/N)=N                        funz.10¬
162300970526                   -        (S/N)=N                        funz.11¬
162400970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
162500970526                   -        (S/N)=N                        funz.13¬
162600970526                   -        (S/N)=N                        funz.14¬
162700970526                   -        (S/N)=N                        funz.15¬
162800970526                   -        (S/N)=N                        funz.16¬
162900970526                   -        (S/N)=N                        funz.17¬
163000970526                   -        (S/N)=N                        funz.18¬
163100970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
163200970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
163300970528F21=Stampa tempi   -        (S/N)=S                        funz.21¬
163400970528F22=Gestione stampe-        (S/N)=S                        funz.22¬
163500970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
163600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
