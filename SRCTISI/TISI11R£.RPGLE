000100970526     H*------------------------------------------------------------------------*
000200970527     H* CAPPARIO - GESTIONE ECCEZIONI LINEE/ZONE - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970527     FTISI11D   CF   E             WORKSTN INFDS(DEVDS1)
000900970527     F                                     SFILE(SI11D05:SFLULT)
001000970527     F                                     SFILE(SI11DMS:MSGULT)
001100970527     FAZORG01L  IF   E           K DISK
001200970528     F*------------
001300970528     F* CAPPARIO
001400970528     F*------------
001500970528     FAZCPC01L  IF   E           K DISK
001600970526     F*------------
001700970527     F* ECCEZIONI LINEE/ZONE
001800970526     F*------------
001900970527     FAZCPE01L  IF   E           K DISK
002000970526     D*------------------------------------------------------------------------*
002100970526     D* SCHIERE
002200970526     D*------------------------------------------------------------------------*
002300970526     D*------------
002400970526     D* DECODIFICHE
002500970526     D*------------
002600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* MESSAGGI
002900970526     D*------------
003000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* DESCRIZIONI OPZIONI
003300970526     D*------------
003400970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* DESCRIZIONI TASTI FUNZIONALI
003700970526     D*------------
003800970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* OPZIONI DA CARICARE A VIDEO
004100970526     D*------------
004200970526     D OPV             S             19    DIM(24)
004300970526     D*------------
004400970526     D* FUNZIONI DA CARICARE A VIDEO
004500970526     D*------------
004600970527     D FUV             S             19    DIM(24)
004700970527     D*------------
004800970527     D* ORGANIGRAMMA
004900970527     D*------------
005000971118     D ORG             S              3  0 DIM(999)                             *CODICE
005100971118     D DORG            S             30    DIM(999)                             *DESCRIZIONE
005200970526     D*------------------------------------------------------------------------*
005300970526     D* INPUT
005400970526     D*------------------------------------------------------------------------*
005500970526     D*------------
005600970526     D* RIDEFINIZIONE OPZIONI VIDEO
005700970526     D*------------
005800970526     D OPDDS           DS
005900970526     D  OPDOPZ                 1      2
006000970526     D  OPDDE1                 3     19
006100970526     D  OPDOP1                21     22
006200970526     D  OPDFI0                23     34
006300970526     D  OPDATT                35     35
006400970526     D  OPDFI1                36     80
006500970526     D  OPDDES                 1     19
006600970526     D  OPDRIG                 1     80
006700970526     D*------------
006800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006900970526     D*------------
007000970526     D FUDDS           DS
007100970526     D  FUDDE1                 1      1
007200970526     D  FUDFUN                 2      3
007300970526     D  FUDDE2                 4     19
007400970526     D  FUDFI0                20     34
007500970526     D  FUDATT                35     35
007600970526     D  FUDFI1                36     80
007700970526     D  FUDDES                 1     19
007800970526     D  FUDRIG                 1     80
007900970526     D*------------
008000970526     D* DS GESTIONE TASTI FUNZIONALI
008100970526     D*------------
008200970526     D DSKEY         E DS
008300970526     D DEVDS1          DS
008400970526     D  KEY                  369    369
008500970526     D*------------
008600970526     D* DS "XMSG" - GESTIONE MESSAGGI
008700970526     D*------------
008800970526     D DSMSG         E DS
008900970526     D*------------
009000020919     D* DS REPERIMENTO DATI UTENTE
009100970526     D*------------
009200020919     D TIBS34DS      E DS                                                       *Profili utente
009300020919     D DDATIUTE      E DS                                                       *Dati utente
009400020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009500150423     D DUTE01        E DS                                                       *Abilitazioni
009600970527     D*------------------
009700970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009800970527     D*------------------
009900970527     D WLBDA8          DS                  INZ
010000970527     D  G08DAT                 1      8  0
010100970527     D  G08INV                 9     16  0
010200970527     D  G08ERR                17     17
010300970527     D  G08TGI                18     22  0
010400970526     D*------------
010500970526     D* DS DI PROCEDURA
010600970526     D*------------
010700970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010800970526     D*------------
010900970526     D* ARCHITETTURA
011000970526     D*------------
011100970526     D KPJBA         E DS
011200970526     C*------------------------------------------------------------------------*
011300970526     C* MAIN LINES
011400970526     C*------------------------------------------------------------------------*
011500970526     C*
011600970526     C* SE NON CI SONO ERRORI, PROSEGUE
011700970526IF  1C     ERRL00        IFNE      '1'
011800970526     C*
011900970526     C* CARICA IL SFILE
012000970526     C                   EXSR      POSSFL
012100970526     C*
012200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012600970526     C*
012700970526     C* REIMPOSTA ERRORE DI PROCERURA
012800970526     C                   MOVEL     '0'           ERRL00
012900970526     C*
013000970526     C* EMETTE IL SFILE
013100970526     C                   EXSR      OUTSFL
013200970526     C*
013300970526     C* GESTIONE TASTI FUNZIONALI
013400970526     C                   EXSR      FUNCON
013500970526     C*
013600970526E   2C                   ENDDO
013700970527     C*
013800970527     C* CI SONO ERRORI IN ENTRATA
013900970527X   1C                   ELSE
014000970527     C                   MOVEL     '1'           D00ERR
014100970527     C                   MOVEL     DSMSMS        D00MSG
014200970526E   1C                   ENDIF
014300970526     C* FINE PGM
014400970526     C                   SETON                                        LR
014500970526     C*------------------------------------------------------------------------*
014600970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014700970526     C*------------------------------------------------------------------------*
014800970526     C     F03GES        BEGSR
014900970526     C*
015000970526     C* IMPOSTA "PREMUTO F03"
015100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015200970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
015300970526     C*
015400970526     C                   ENDSR
015500970526     C*------------------------------------------------------------------------*
015600970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015700970526     C*------------------------------------------------------------------------*
015800970526     C     F05GES        BEGSR
015900970526     C*
016000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016100970526     C*
016200970526     C                   ENDSR
016300970526     C*------------------------------------------------------------------------*
016400970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016500970526     C*------------------------------------------------------------------------*
016600970526     C     F06GES        BEGSR
016700970526     C*
016800970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
016900021115IF  1C***  DUTLPO        IFNE      'S'
017000021115     C***                MOVEL     '1'           ERRL00
017100021115     C***                MOVEL     MSG(3)        DSMSMS
017200021115E   1C***                ENDIF
017300970526     C*
017400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017500970603IF  2C     D00NAR        IFNE      *BLANKS
017600970603     C     D00CAP        ORNE      *BLANKS
017700970603     C                   MOVEL     D00NAR        DNAR
017800970603     C                   MOVEL     D00CAP        DCAP
017900970603X   2C                   ELSE
018000970602     C                   MOVEL     *BLANKS       DNAR
018100970602     C                   MOVEL     *BLANKS       DCAP
018200970603E   2C                   ENDIF
018300970602     C                   Z-ADD     *ZEROS        DTFP
018400970602     C                   MOVEL     DEPOP0        WOP0
018500970529     C                   MOVEL     'F06'         WOP1
018600970527     C                   MOVEL     'TISI12R'     WPGM              8
018700970526     C                   EXSR      LANPGM                                       *LANCIA PGM
018800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018900970526E   1C                   ENDIF
019000970526     C*
019100970526     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300970526     C* F12GES - GESTIONE F12 -> RITORNO
019400970526     C*------------------------------------------------------------------------*
019500970526     C     F12GES        BEGSR
019600970526     C*
019700970526     C* IMPOSTA "PREMUTO F12"
019800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019900970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
020000970526     C*
020100970526     C                   ENDSR
020200970526     C*------------------------------------------------------------------------*
020300970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
020400970526     C*------------------------------------------------------------------------*
020500970526     C     F23GES        BEGSR
020600970526     C*
020700970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F24GES        BEGSR
021400970526     C*
021500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021600970526     C*
021700970526     C                   ENDSR
021800970526     C*------------------------------------------------------------------------*
021900970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
022000970526     C*------------------------------------------------------------------------*
022100970526     C     RUPGES        BEGSR
022200970526     C*
022300970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
022400970526     C*
022500970526     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* ENTGES - GESTIONE ENTER -> INVIO
022800970526     C*------------------------------------------------------------------------*
022900970526     C     ENTGES        BEGSR
023000970526     C*
023100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
023200970526     C                   EXSR      NEWPOS
023300970526     C*
023400970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
023500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023600970526IF  2C     SWPOSI        IFNE      0
023700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
023800970526X   2C                   ELSE
023900970526IF  3C     SFLULT        IFNE      0
024000970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
024100970526     C*
024200970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
024300970526IF  4C     SWMODI        IFNE      0
024400970526     C                   EXSR      POSSFL
024500970526E   4C                   ENDIF
024600970526E   3C                   ENDIF
024700970526E   2C                   ENDIF
024800970526E   1C                   ENDIF
024900970526     C*
025000970526     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
025300970526     C*------------------------------------------------------------------------*
025400970526     C     O01GES        BEGSR
025500970526     C*
025600970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
025700970527     C                   MOVEL     VIDNAR        D00NAR
025800970527     C                   MOVEL     VIDCAP        D00CAP
025900970527     C                   Z-ADD     VIDTFP        D00TFP
026000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
026100970526     C*
026200970526     C                   ENDSR
026300970526     C*------------------------------------------------------------------------*
026400970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
026500970526     C*------------------------------------------------------------------------*
026600970526     C     O02GES        BEGSR
026700970526     C*
026800970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
026900970526IF  1C     VIDATB        IFEQ      'A'
027000970526     C                   MOVEL     '1'           ERRL00
027100970526     C                   MOVEL     MSG(4)        DSMSMS
027200970526E   1C                   ENDIF
027300970527     C* MODIFICA
027400970704IF  1C     ERRL00        IFEQ      '0'
027500970602     C                   MOVEL     VIDNAR        DNAR
027600970602     C                   MOVEL     VIDCAP        DCAP
027700970602     C                   Z-ADD     VIDTFP        DTFP
027800970602     C                   MOVEL     DEPOP0        WOP0
027900970526     C                   MOVEL     'O02'         WOP1
028000970527     C                   MOVEL     'TISI12R'     WPGM
028100970526     C                   EXSR      LANPGM
028200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
028300970528E   1C                   ENDIF
028400970526     C*
028500970526     C                   ENDSR
028600970526     C*------------------------------------------------------------------------*
028700970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
028800970526     C*------------------------------------------------------------------------*
028900970526     C     O04GES        BEGSR
029000970526     C*
029100970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
029200970526IF  1C     VIDATB        IFEQ      'A'
029300970526     C                   MOVEL     '1'           ERRL00
029400970526     C                   MOVEL     MSG(4)        DSMSMS
029500970526E   1C                   ENDIF
029600970527     C* ANNULLAMENTO
029700970704IF  1C     ERRL00        IFEQ      '0'
029800970602     C                   MOVEL     VIDNAR        DNAR
029900970602     C                   MOVEL     VIDCAP        DCAP
030000970602     C                   Z-ADD     VIDTFP        DTFP
030100970602     C                   MOVEL     DEPOP0        WOP0
030200970526     C                   MOVEL     'O04'         WOP1
030300970527     C                   MOVEL     'TISI12R'     WPGM
030400970526     C                   EXSR      LANPGM
030500970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
030600970528E   1C                   ENDIF
030700970526     C*
030800970526     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
031100970526     C*------------------------------------------------------------------------*
031200970526     C     O05GES        BEGSR
031300970526     C*
031400970602     C                   MOVEL     VIDNAR        DNAR
031500970602     C                   MOVEL     VIDCAP        DCAP
031600970602     C                   Z-ADD     VIDTFP        DTFP
031700970602     C                   MOVEL     DEPOP0        WOP0
031800970526     C                   MOVEL     'O05'         WOP1
031900970527     C                   MOVEL     'TISI12R'     WPGM
032000970526     C                   EXSR      LANPGM
032100970526     C*
032200970526     C                   ENDSR
032300970526     C*------------------------------------------------------------------------*
032400970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
032500970526     C*------------------------------------------------------------------------*
032600970526     C     O07GES        BEGSR
032700970526     C*
032800970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
032900970526IF  1C     VIDATB        IFNE      'A'
033000970526     C                   MOVEL     '1'           ERRL00
033100970526     C                   MOVEL     MSG(5)        DSMSMS
033200970526E   1C                   ENDIF
033300970526     C* RIPRISITINO
033400970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033500970602     C                   MOVEL     VIDNAR        DNAR
033600970602     C                   MOVEL     VIDCAP        DCAP
033700970602     C                   Z-ADD     VIDTFP        DTFP
033800970602     C                   MOVEL     DEPOP0        WOP0
033900970526     C                   MOVEL     'O07'         WOP1
034000970527     C                   MOVEL     'TISI12R'     WPGM
034100970526     C                   EXSR      LANPGM
034200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
034300970528E   1C                   ENDIF
034400970526     C*
034500970526     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
034800970526     C*------------------------------------------------------------------------*
034900970526     C     NEWPOS        BEGSR
035000970526     C*
035100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
035200970526     C*         1= RICHIESTO POSIZIONAMENTO
035300970526     C                   Z-ADD     0             SWPOSI            7 0
035400970526     C*
035500970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
035600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
035700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
035800970526     C                   Z-ADD     0             SWMODI            1 0
035900970530     C*
036000970530     C* REIMPOSTA INDICATORI DI ERRORE
036100970530     C                   SETOFF                                       808182
036200970526     C*
036300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
036400970527IF  1C     VIDPNA        IFNE      DEPNAR
036500970527     C     VIDPCA        ORNE      DEPCAP
036600970527     C     VIDPTF        ORNE      DEPTFP
036700970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
036800970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
036900970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
037000970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
037100970527     C                   MOVEL     VIDPCA        DEPCAP
037200970527     C                   Z-ADD     VIDPTF        DEPTFP
037300970526E   2C                   ENDIF
037400970526E   1C                   ENDIF
037500970526     C*
037600970526     C                   ENDSR
037700970526     C*------------------------------------------------------------------------*
037800970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
037900970526     C*------------------------------------------------------------------------*
038000970526     C     SETFIL        BEGSR
038100970526     C*
038200970527     C* POSIZIONAMENTO SUL FILE
038300970527     C                   Z-ADD     D00VER        KCEVER
038400970527     C                   MOVEL     VIDPNA        KCENAR
038500970527     C                   MOVEL     VIDPCA        KCECAP
038600970527     C                   Z-ADD     VIDPTF        KCETFP
038700970527     C     KEYCPE        SETLL     AZCPE01L                           99
038800970526     C* LETTURA RECORD
038900970526IF  1C     *IN99         IFEQ      *OFF
039000970526     C                   EXSR      LETFIL
039100970526E   1C                   ENDIF
039200970526     C*
039300970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
039400970526IF  1C     *IN99         IFEQ      *ON
039500970526     C                   Z-ADD     1             SFLEOF
039600970526E   1C                   ENDIF
039700970526     C*
039800970526     C                   ENDSR
039900970526     C*------------------------------------------------------------------------*
040000970526     C* LETFIL - LETTURA RECORD
040100970526     C*------------------------------------------------------------------------*
040200970526     C     LETFIL        BEGSR
040300970526     C*
040400970526     C* LEGGE FINO A:
040500970526     C*     - FINE FILE
040600970526     C*     - TROVATO RECORD VALIDO
040700970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
040800970526DO  1C     *IN99         DOWEQ     *OFF
040900970526     C     WRECOK        ANDEQ     'N'
041000970526     C*
041100970527     C* LETTURE SUCCESSIVE ALLA PRIMA
041200970527     C     KCEVER        READE     AZCPE01L                               99
041300970526     C*
041400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
041500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
041600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
041700970530X   2C                   ELSE
041800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
041900970530E   2C                   ENDIF
042000970526E   1C                   ENDDO
042100970526     C*
042200970526     C                   ENDSR
042300970526     C*------------------------------------------------------------------------*
042400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
042500970526     C*------------------------------------------------------------------------*
042600970526     C     CHKREC        BEGSR
042700970526     C*
042800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
042900970530     C*
043000970530     C* SE RICHIESTO ORDINAMENTO PER CAP, ESCLUDE LE LOCALITA' NON DEL CAP
043100970530IF  1C     VIDPNA        IFNE      *BLANKS
043200970530     C     VIDPCA        ORNE      *BLANKS
043300970530IF  2C     VIDPNA        IFNE      CPENAR
043400970530     C     VIDPCA        ORNE      CPECAP
043500970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043600970530E   2C                   ENDIF
043700970530E   1C                   ENDIF
043800970526     C*
043900970526     C                   ENDSR
044000970526     C*------------------------------------------------------------------------*
044100970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
044200970526     C*------------------------------------------------------------------------*
044300970526     C     IRESFL        BEGSR
044400970526     C*
044500970526     C                   MOVEL     *BLANKS       OPZKEY
044600970527     C                   MOVEL     CPEATB        VIDATB
044700970528     C                   MOVEL     CPENAR        VIDNAR
044800970528     C                   MOVEL     CPECAP        VIDCAP
044900970527     C                   Z-ADD     CPETFP        VIDTFP
045000970527     C                   Z-ADD     CPELNA        VIDLNA
045100970527     C                   Z-ADD     CPEZNC        VIDZNC
045200970527     C                   Z-ADD     CPELOL        VIDLOL
045300970527     C                   Z-ADD     CPEZOL        VIDZOL
045400970606     C                   Z-ADD     CPELOS        VIDLOS
045500970606     C                   Z-ADD     CPEZOS        VIDZOS
045600970527     C* DECODIFICHE
045700970527     C                   SETOFF                                           28
045800970527     C                   Z-ADD     1             I
045900970527     C     CPETFP        LOOKUP    ORG(I)                                 28
046000970527IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
046100970527     C                   MOVEL     DORG(I)       VIDDTF
046200970527X   1C                   ELSE                                                   *NON TROVATO
046300970527     C                   MOVEL     *BLANKS       VIDDTF
046400970527E   1C                   ENDIF
046500970527     C*
046600970527     C                   SETOFF                                           28
046700970527     C                   Z-ADD     1             I
046800970527     C     CPELNA        LOOKUP    ORG(I)                                 28
046900970527IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
047000970527     C                   MOVEL     DORG(I)       VIDDLN
047100970527X   1C                   ELSE                                                   *NON TROVATO
047200970527     C                   MOVEL     *BLANKS       VIDDLN
047300970527E   1C                   ENDIF
047400970527     C*
047500970527     C                   SETOFF                                           28
047600970527     C                   Z-ADD     1             I
047700970527     C     CPELOL        LOOKUP    ORG(I)                                 28
047800970527IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
047900970527     C                   MOVEL     DORG(I)       VIDDLO
048000970527X   1C                   ELSE                                                   *NON TROVATO
048100970527     C                   MOVEL     *BLANKS       VIDDLO
048200970527E   1C                   ENDIF
048300970606     C*
048400970606     C                   SETOFF                                           28
048500970606     C                   Z-ADD     1             I
048600970606     C     CPELOS        LOOKUP    ORG(I)                                 28
048700970606IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
048800970606     C                   MOVEL     DORG(I)       VIDDLS
048900970606X   1C                   ELSE                                                   *NON TROVATO
049000970606     C                   MOVEL     *BLANKS       VIDDLS
049100970606E   1C                   ENDIF
049200970528     C* ALTRI DATI
049300970528     C                   Z-ADD     CPEVER        KCCVER
049400970528     C                   MOVEL     CPENAR        KCCNAR
049500970528     C                   MOVEL     CPECAP        KCCCAP
049600970528     C     KEYCPC        CHAIN     AZCPC01L                           99
049700970528IF  1C     *IN99         IFEQ      *OFF
049800970528     C                   Z-ADD     CPCLNA        VIHCLA
049900970528X   1C                   ELSE
050000970528     C                   Z-ADD     *ZEROS        VIHCLA
050100970528E   1C                   ENDIF
050200970526     C*
050300970526     C                   ENDSR
050400970526     C*------------------------------------------------------------------------*
050500970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
050600970526     C*------------------------------------------------------------------------*
050700970526     C     CHKPAR        BEGSR
050800970526     C*
050900970526     C                   ENDSR
051000970526     C*------------------------------------------------------------------------*
051100970526     C* LANPGM - LANCIA UN PROGRAMMA
051200970526     C*------------------------------------------------------------------------*
051300970526     C     LANPGM        BEGSR
051400970526     C*
051500970526     C* IMPOSTA LA DS DI PROCEDURA
051600970602     C                   MOVEL     WOP0          DEPOP0                         *LIVELLO PROCEDURA
051700970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
051800970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
051900970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
052000970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
052100970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
052200970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
052300970602     C                   MOVEL     DCAP          D00CAP                         *CAP
052400970602     C                   Z-ADD     DTFP          D00TFP                         *TERMINAL PARTENZA
052500970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
052600970530     C                   MOVEL     *BLANKS       D00TSP
052700970530     C                   MOVEL     *BLANKS       D00LOC
052800970603     C                   MOVEL     *BLANKS       D00SIN
052900970530     C                   MOVEL     *BLANKS       D00EPA
053000970530     C                   Z-ADD     *ZEROS        D00ETP
053100970530     C                   Z-ADD     *ZEROS        D00ETA
053200970530     C                   Z-ADD     *ZEROS        D00EDD
053300970530     C                   MOVEL     *BLANKS       D00SPT
053400970530     C                   Z-ADD     *ZEROS        D00SLP
053500970530     C                   Z-ADD     *ZEROS        D00SLA
053600970530     C                   MOVEL     *BLANKS       D00STS
053700970530     C                   Z-ADD     *ZEROS        D00SDD
053800970526     C*
053900970526     C                   CALL      WPGM
054000970612     C                   PARM                    KPJBA
054100970612     C                   PARM                    DSSI00
054200970526     C*
054300970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
054400970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
054500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
054600970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
054700970526X   1C                   ELSE                                                   *NON PREMUTO F03
054800970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
054900970526E   1C                   ENDIF
055000970526     C*
055100970526     C                   ENDSR
055200970527     C*------------------------------------------------------------------------*
055300970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
055400970527     C*------------------------------------------------------------------------*
055500970527     C     CARTAB        BEGSR
055600970527     C*---
055700970527     C* ORGANIGRAMMA
055800970527     C*---
055900970527     C                   Z-ADD     *ZEROS        I                 5 0
056000970527     C                   Z-ADD     *ZEROS        ORG
056100970527     C                   MOVEL     *BLANKS       DORG
056200970527     C     *LOVAL        SETLL     AZORG01L
056300970527     C                   READ      AZORG01L                               99
056400970527DO  1C     *IN99         DOWEQ     *OFF
056500970527IF  2C     ORGFVA        IFEQ      *BLANKS
056600970527     C     ORGFAG        ANDNE     'V'
056700970527     C                   ADD       1             I
056800970527     C                   Z-ADD     ORGFIL        ORG(I)
056900970527     C                   MOVEL     ORGDES        DORG(I)
057000970527E   2C                   ENDIF
057100970527     C                   READ      AZORG01L                               99
057200970527E   1C                   ENDDO                                                  *FINE ORGANIGRAMMA
057300970527     C*
057400970527     C                   ENDSR
057500970526     C*------------------------------------------------------------------------*
057600970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
057700970526     C*------------------------------------------------------------------------*
057800970526     C     CHKFOP        BEGSR
057900970626     C*
058000970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
058100970626IF  1C     DEPOP0        IFEQ      'E01'
058200970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
058300970626     C                   MOVEL     'S'           OPDATT
058400970626     C                   MOVEL     OPDRIG        OPD(1)
058500970626X   1C                   ELSE
058600970626     C* OPZIONI
058700970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
058800970626     C                   MOVEL     'S'           OPDATT
058900970626     C                   MOVEL     OPDRIG        OPD(5)
059000970626     C* FUNZIONI
059100970626E   1C                   ENDIF
059200150423     C                   IF        §UTECAP = 'I'  or §utecap='O'
059300150423     C* FORZO SOLO INTERROGAZIONE
059400150423     C                   EVAL      DEPOP1 = 'O05'
059500150423     C                   ENDIF
059600970626     C*
059700970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
059800970626IF  1C     DEPOP0        IFEQ      'E02'
059900150423     c     §utecap       andne     'I'
060000150423     c     §utecap       andne     'O'
060100970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
060200970626     C                   MOVEL     'S'           OPDATT
060300970626     C                   MOVEL     OPDRIG        OPD(2)
060400970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
060500970626     C                   MOVEL     'S'           OPDATT
060600970626     C                   MOVEL     OPDRIG        OPD(4)
060700970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
060800970626     C                   MOVEL     'S'           OPDATT
060900970626     C                   MOVEL     OPDRIG        OPD(7)
061000970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
061100970626     C                   MOVEL     'S'           FUDATT
061200970626     C                   MOVEL     FUDRIG        FUD(6)
061300970626E   1C                   ENDIF
061400970626     C*
061500970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
061600021115IF  1C***  DUTLPO        IFNE      'S'                                          *FILIALE
061700021115     C***                MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
061800021115     C***                MOVEL     'N'           OPDATT
061900021115     C***                MOVEL     OPDRIG        OPD(4)
062000021115     C***                MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
062100021115     C***                MOVEL     'N'           OPDATT
062200021115     C***                MOVEL     OPDRIG        OPD(7)
062300021115     C***                MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
062400021115     C***                MOVEL     'N'           FUDATT
062500021115     C***                MOVEL     FUDRIG        FUD(6)
062600021115E   1C***                ENDIF
062700970526     C*
062800970526     C* GESTIONE OPZIONI A VIDEO
062900970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
063000970526     C                   EXSR      OPZGES
063100970526     C*
063200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
063300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
063400970526     C                   EXSR      FUNGES
063500970526     C*
063600970526     C                   ENDSR
063700020919     C*--------------------------------------------------------------------------------------------*
063800020919     C* REPERISCE I DATI UTENTE
063900020919     C*--------------------------------------------------------------------------------------------*
064000020919     C     REPDATIUTE    BEGSR
064100020919     C*
064200020919     C* INIZIALIZZA VARIABILI DI WRK
064300020919     C                   CLEAR                   TIBS34DS
064400020919     C                   CLEAR                   AZUTEDS
064500020919     C                   CLEAR                   DDATIUTE
064600020919     C*
064700020919     C     *DTAARA       DEFINE    §azute        azuteds
064800020919     C     *DTAARA       DEFINE    §datiute      ddatiute
064900020919     C                   IN(E)     *DTAARA
065000020919if  1C                   IF        %Error
065100020919     c                   EVAL      I34Tla = 'L'
065200020919     C                   CALL      'TIBS34R'
065300020919     C                   PARM                    Tibs34Ds
065400020919     C                   IN        *DTAARA
065500020919e   1C                   ENDIF
065600150423     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
065700150423     C                   EVAL      DUTE01 = UTEFAF
065800020919     C*
065900020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
066000020919     C                   MOVEL(P)  RSUT          DSFIRS           20
066100020919     C*
066200020919     C                   ENDSR
066300970526     C*------------------------------------------------------------------------*
066400970526     C* *INZSR - OPERAZIONI INIZIALI
066500970526     C*------------------------------------------------------------------------*
066600970526     C     *INZSR        BEGSR
066700970526     C*---
066800970526     C* RICEVIMENTO PARAMETRI
066900970526     C*---
067000970526     C     *ENTRY        PLIST
067100970612     C                   PARM                    KPJBA
067200970612     C                   PARM                    DSSI00
067300970526     C*
067400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
067500970526     C     XMSGPA        PLIST
067600970526     C                   PARM                    DSMSMS
067700970526     C                   PARM                    DSMSPG
067800970526     C                   PARM                    DSMSKE
067900970526     C                   PARM                    DSMSER
068000970526     C*
068100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
068200970526     C                   CALL      'XKEY'
068300970526     C                   PARM                    DSKEY
068400970526     C*---
068500970526     C* VARIABILI NON RIFERITE AL DATA BASE
068600970526     C*---
068700970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
068800970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
068900970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
069000970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
069100970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
069200970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
069300970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
069400970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
069500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
069600970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
069700970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
069800970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
069900970529     C     *LIKE         DEFINE    D00OP1        WOP1
070000970602     C     *LIKE         DEFINE    D00NAR        DNAR
070100970602     C     *LIKE         DEFINE    D00CAP        DCAP
070200970602     C     *LIKE         DEFINE    D00TFP        DTFP
070300970526     C*---
070400970526     C* VARIABILI DI CONTROLLO
070500970526     C*---
070600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
070700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
070800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
070900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
071000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
071100970606     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
071200970526     C*
071300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
071400970527     C                   MOVEL     'TISI11R'     DSMSPG
071500970527     C                   MOVEL     'TISI11R'     MSGQUE
071600970526     C                   MOVEL     *BLANKS       MSGKEY
071700970526     C                   MOVEL     *BLANKS       DSMSMS
071800970526     C                   MOVEL     *BLANKS       DSMSKE
071900970526     C                   Z-ADD     *ZEROS        DSMSER
072000970526     C*---
072100970526     C* VARIABILI RIFERITE AL DATA BASE
072200970526     C*---
072300970527     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
072400970528     C     *LIKE         DEFINE    CPEVER        KCEVER                         *AZCPE00F
072500970527     C     *LIKE         DEFINE    CPENAR        KCENAR
072600970527     C     *LIKE         DEFINE    CPECAP        KCECAP
072700970527     C     *LIKE         DEFINE    CPETFP        KCETFP
072800970528     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
072900970528     C     *LIKE         DEFINE    CPCNAR        KCCNAR
073000970528     C     *LIKE         DEFINE    CPCCAP        KCCCAP
073100970527     C     *LIKE         DEFINE    CPENAR        DEPNAR                         *DEPOSITI
073200970527     C     *LIKE         DEFINE    CPECAP        DEPCAP
073300970527     C     *LIKE         DEFINE    CPETFP        DEPTFP
073400970526     C*---
073500970526     C* CHIAVI DI LETTURA
073600970526     C*---
073700970527     C* LETTURA AZORG01L - COMPLETA
073800970527     C     KEYORG        KLIST
073900970527     C                   KFLD                    KORFIL                         *FILIALE
074000970527     C* LETTURA AZCPE01L - COMPLETA
074100970527     C     KEYCPE        KLIST
074200970527     C                   KFLD                    KCEVER                         *VERSIONE
074300970527     C                   KFLD                    KCENAR                         *NAZIONE
074400970527     C                   KFLD                    KCECAP                         *CAP
074500970527     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
074600970528     C* LETTURA AZCPC01L - COMPLETA
074700970528     C     KEYCPC        KLIST
074800970528     C                   KFLD                    KCCVER                         *VERSIONE
074900970528     C                   KFLD                    KCCNAR                         *NAZIONE
075000970528     C                   KFLD                    KCCCAP                         *CAP
075100970527     C*
075200970527     C* REPERIMENTO DATI UTENTE
075300020919     C                   EXSR      REPDATIUTE
075400970527     C*
075500970527     C* CARICA LE TABELLE OCCORRENTI
075600970527     C                   EXSR      CARTAB
075700970526     C*
075800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
075900970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
076000970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
076100970526     C*
076200970526     C* CALCOLA LA DATA CORRENTE
076300970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
076400970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
076500970526     C                   Z-ADD     WN8           G08DAT
076600970526     C                   Z-ADD     *ZEROS        G08INV
076700970526     C                   MOVEL     '0'           G08ERR
076800970526     C                   CALL      'XSRDA8'
076900970526     C                   PARM                    WLBDA8
077000970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
077100970526     C*
077200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
077300970526     C                   Z-ADD     D00DRI        G08INV
077400970526     C                   Z-ADD     *ZEROS        G08DAT
077500970526     C                   MOVEL     '3'           G08ERR
077600970526     C                   CALL      'XSRDA8'
077700970526     C                   PARM                    WLBDA8
077800970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
077900970527     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
078000970527     C                   MOVEL     D00CAP        VIDPCA                         *CAP
078100970527     C                   Z-ADD     D00TFP        VIDPTF                         *TERMINAL PARTENZA
078200970526     C                   EXSR      CHKPAR
078300970526IF  1C     ERRL00        IFEQ      '1'
078400970526     C                   MOVEL     '1'           D00ERR
078500970526     C                   MOVEL     DSMSMS        D00MSG
078600970526X   1C                   ELSE
078700970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
078800970527     C                   MOVEL     VIDPCA        DEPCAP
078900970527     C                   Z-ADD     VIDTFP        DEPTFP
079000970526E   1C                   ENDIF
079100970703     C*
079200970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
079300970703IF  1C     D00VER        IFEQ      *ZEROS
079400970707     C                   Z-ADD     D00DRI        P96DRI            8 0
079500970707     C                   Z-ADD     *ZEROS        P96VER            5 0
079600970707     C                   CALL      'TISI96R'
079700970707     C                   PARM                    P96DRI
079800970707     C                   PARM                    P96VER
079900970707     C                   Z-ADD     P96VER        D00VER
080000970703E   1C                   ENDIF
080100970526     C*
080200970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
080300970526     C                   EXSR      CHKFOP
080400970526     C*
080500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
080600970530IF  1C     DEPOP0        IFEQ      'E01'
080700970530     C                   MOVEL     DEC(3)        VIDING
080800970530E   1C                   ENDIF
080900970530IF  1C     DEPOP0        IFEQ      'E02'
081000970530     C                   MOVEL     DEC(1)        VIDING
081100970530E   1C                   ENDIF
081200970530IF  1C     DEPOP0        IFEQ      'E05'
081300970530     C                   MOVEL     DEC(2)        VIDING
081400970530E   1C                   ENDIF
081500970526     C*
081600970526     C                   ENDSR
081700970526     C*------------------------------------------------------------------------*
081800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
081900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
082100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
082200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
082300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
082500970526     C*------------------------------------------------------------------------*
082600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
082700970526     C*------------------------------------------------------------------------*
082800970526     C     POSSFL        BEGSR
082900970526     C*
083000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
083100970526     C                   EXSR      CLRSFL
083200970526     C*
083300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
083400970526     C                   EXSR      SETFIL
083500970526     C*
083600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
083700970526     C     SFLEOF        IFEQ      0
083800970526     C                   EXSR      CARSFL
083900970526     C                   END
084000970526     C*
084100970526     C                   ENDSR
084200970526     C*------------------------------------------------------------------------*
084300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
084400970526     C*------------------------------------------------------------------------*
084500970526     C     ENTATT        BEGSR
084600970526     C*
084700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
084800970526     C                   Z-ADD     0             OPZOK             1 0
084900970526     C*
085000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
085100970526     C                   Z-ADD     0             SFLVAR            5 0
085200970526     C*
085300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
085400970526     C                   Z-ADD     0             SFLERR
085500970526     C*
085600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
085700970526     C                   Z-ADD     SFLULT        WFLULT
085800970526     C*
085900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
086000970527     C                   READC     SI11D05                                98
086100970526     C     *IN98         DOWEQ     '0'
086200970526     C*
086300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
086400970526     C                   ADD       1             SFLVAR
086500970526     C*
086600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
086700970526     C     OPZOK         IFEQ      0
086800970526     C*
086900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
087000970526     C     OPZKEY        IFNE      *BLANKS
087100970526     C                   EXSR      OPZCON
087200970526     C                   END
087300970526     C*
087400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
087500970526     C*            2. PULIZIA SCELTA IN SFL
087600970526     C     OPZOK         IFEQ      0
087700970526     C                   MOVEL     '0'           *IN41
087800970526     C                   MOVEL     *BLANKS       OPZKEY
087900970526     C                   ELSE
088000970526     C*
088100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
088200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
088300970526     C                   MOVEL     '1'           *IN41
088400970526     C                   Z-ADD     SFLULT        SFLERR
088500970526     C                   END
088600970526     C                   ELSE
088700970526     C*
088800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
088900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
089000970526     C                   MOVEL     '1'           *IN41
089100970526     C                   END                                                    OPZOK=0
089200970526     C*
089300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
089400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
089500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
089600970526     C     OPZOK         IFNE      0
089700970526     C                   Z-ADD     SFLERR        SFLNBR
089800970526     C                   ELSE
089900970526     C                   Z-ADD     SFLULT        SFLNBR
090000970526     C                   END
090100970526     C*
090200970526     C* AGGIORNO RECORD SFL
090300970527     C                   UPDATE    SI11D05
090400970526     C*
090500970527     C                   READC     SI11D05                                98
090600970526     C                   END                                                    DO WHILE
090700970526     C*
090800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
090900970526     C                   Z-ADD     WFLULT        SFLULT
091000970526     C*
091100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
091200970526     C     SFLVAR        IFEQ      0
091300970526     C*---                 MOVEL'1'       FINL00
091400970526     C                   END
091500970526     C*
091600970526     C                   ENDSR
091700970526     C*------------------------------------------------------------------------*
091800970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
091900970526     C*------------------------------------------------------------------------*
092000970526     C     CLRSFL        BEGSR
092100970526     C*
092200970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
092300970526     C* (0=NON FINE FILE  1=FINE FILE)
092400970526     C                   Z-ADD     0             SFLEOF
092500970526     C*
092600970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
092700970526     C                   Z-ADD     0             SFLULT
092800970526     C*
092900970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
093000970526     C                   MOVEL     '1'           *IN40
093100970527     C                   WRITE     SI11D06
093200970526     C                   MOVEL     '0'           *IN40
093300970526     C*
093400970526     C                   ENDSR
093500970526     C*------------------------------------------------------------------------*
093600970526     C* CARSFL - CARICAMENTO SFL
093700970526     C*------------------------------------------------------------------------*
093800970526     C     CARSFL        BEGSR
093900970526     C*
094000970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
094100970526     C     SFLULT        IFEQ      0
094200970526     C                   EXSR      SETFIL
094300970526     C                   END
094400970526     C*
094500970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
094600970526     C                   Z-ADD     SFLULT        WFLULT
094700970526     C*
094800970526     C* CICLO:
094900970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
095000970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
095100970526     C                   Z-ADD     1             SFLRIG
095200970526     C     SFLRIG        DOWLE     SFLPAG
095300970526     C     SFLEOF        ANDEQ     0
095400970526     C*
095500970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
095600970526     C                   MOVEL     *BLANKS       OPZKEY
095700970526     C*
095800970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
095900970526     C                   EXSR      IRESFL
096000970526     C*
096100970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
096200970526     C                   ADD       1             SFLULT
096300970526     C*
096400970526     C* SCRITTURA RECORD SFL
096500970527     C                   WRITE     SI11D05
096600970526     C                   ADD       1             SFLRIG
096700970526     C*
096800970526     C* LETTURA FILE DA CARICARE
096900970526     C                   EXSR      LETFIL
097000970526     C                   END                                                    DO WHILE
097100970526     C*
097200970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
097300970526     C     WFLULT        IFNE      SFLULT
097400970526     C                   Z-ADD     WFLULT        SFLNBR
097500970526     C                   ADD       1             SFLNBR
097600970526     C                   END
097700970526     C*
097800970526     C                   ENDSR
097900970526     C*------------------------------------------------------------------------*
098000970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
098100970526     C*------------------------------------------------------------------------*
098200970526     C     OUTSFL        BEGSR
098300970526     C*
098400970526     C* WRITE RECORD TESTATA VIDEO
098500970527     C                   WRITE     SI11D01
098600970526     C*
098700970526     C* VISUALIZZAZIONE MESSAGGIO
098800970526     C                   EXSR      SNDMSG
098900970526     C*
099000970526     C* WRITE RECORD OPZIONI VIDEO
099100970527     C                   WRITE     SI11D02
099200970526     C*
099300970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
099400970527     C                   WRITE     SI11D03
099500970526     C*
099600970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
099700970527     C                   WRITE     SI11D04
099800970526     C*
099900970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
100000970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
100100970526     C     SFLEOF        IFEQ      0
100200970526     C                   MOVEL     '0'           *IN42
100300970526     C                   ELSE
100400970526     C*
100500970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
100600970526     C                   MOVEL     '1'           *IN42
100700970526     C                   END
100800970526     C*
100900970526     C* WRITE RECORD TASTI FUNZIONALI
101000970527     C                   WRITE     SI11D09
101100970526     C*
101200970526     C* CASO DI SFL VUOTO
101300970526     C     SFLULT        IFLE      0
101400970526     C*
101500970526     C*    - WRITE RECORD VUOTO
101600970527     C                   WRITE     SI11D0A
101700970527     C                   READ      SI11D0A                                99
101800970526     C*
101900970526     C                   ELSE
102000970526     C*
102100970526     C* CASO DI SFL CON RECORDS
102200970526     C*
102300970526     C*    - WRITE RECORD CONTROLLO SFL
102400970527     C                   WRITE     SI11D06
102500970527     C                   READ      SI11D06                                99
102600970526     C                   END
102700970526     C*
102800970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
102900970527     C                   READ      SI11D03                                99
103000970526     C*
103100970526     C                   ENDSR
103200970526     C*------------------------------------------------------------------------*
103300970526     C* SNDMSG - MANDA MESSAGGIO
103400970526     C*------------------------------------------------------------------------*
103500970526     C     SNDMSG        BEGSR
103600970526     C*
103700970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
103800970526     C                   MOVEL     '1'           *IN40
103900970527     C                   WRITE     SI11DMC
104000970526     C                   MOVEL     '0'           *IN40
104100970526     C*
104200970526     C* VISUALIZZAZIONE MESSAGGIO
104300970526     C                   Z-ADD     0             MSGULT
104400970526     C                   CALL      'XMSG'        XMSGPA
104500970526     C     DSMSER        IFEQ      0
104600970526     C                   MOVEL     DSMSKE        MSGKEY
104700970526     C                   END
104800970526     C                   Z-ADD     1             MSGULT
104900970527     C                   WRITE     SI11DMS                                      SFL MESS
105000970527     C                   WRITE     SI11DMC                                      CONTR MESS
105100970526     C*
105200970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
105300970526     C                   MOVEL     *BLANKS       DSMSMS
105400970526     C*
105500970526     C                   ENDSR
105600970526     C*------------------------------------------------------------------------*
105700970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
105800970526     C*------------------------------------------------------------------------*
105900970526     C     FUNCON        BEGSR
106000970526     C*
106100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
106200970526     C                   EXSR      FUNVAR
106300970526     C*
106400970526     C* PULISCO CAMPI DI LAVORO
106500970526     C                   Z-ADD     0             I                 5 0
106600970526     C                   Z-ADD     0             W                 5 0
106700970526     C*
106800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
106900970526     C     FUNKEY        IFNE      *BLANKS
107000970526     C     1             DO        24            I
107100970526     C                   MOVEL     FUD(I)        FUDRIG
107200970526     C*
107300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
107400970526     C     FUNKEY        IFEQ      FUDFUN
107500970526     C     FUDATT        ANDEQ     'S'
107600970526     C                   Z-ADD     I             W
107700970526     C                   END
107800970526     C                   END                                                    DO WHILE
107900970526     C*
108000970526     C* FUNZIONE NON ATTIVA (ERRORE)
108100970526     C     W             IFEQ      0
108200970526     C*
108300970526     C* EMISSIONE MESSAGGIO DI ERRORE
108400970526     C                   MOVEL     MSG(2)        DSMSMS
108500970526     C                   END                                                    W=0
108600970526     C                   END                                                    FUNKEY NE *BLANKS
108700970526     C*
108800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
108900970526     C     W             IFNE      0
109000970526     C     FUNKEY        OREQ      *BLANKS
109100970526     C*
109200970526     C     FUNKEY        IFEQ      *BLANKS
109300970526     C                   EXSR      ENTGES
109400970526     C                   END
109500970526     C     FUNKEY        IFEQ      '03'
109600970526     C                   EXSR      F03GES
109700970526     C                   END
109800970526     C     FUNKEY        IFEQ      '05'
109900970526     C                   EXSR      F05GES
110000970526     C                   END
110100970526     C     FUNKEY        IFEQ      '06'
110200970526     C                   EXSR      F06GES
110300970526     C                   END
110400970526     C     FUNKEY        IFEQ      '12'
110500970526     C                   EXSR      F12GES
110600970526     C                   END
110700970526     C     FUNKEY        IFEQ      '23'
110800970526     C                   EXSR      F23GES
110900970526     C                   END
111000970526     C     FUNKEY        IFEQ      '24'
111100970526     C                   EXSR      F24GES
111200970526     C                   END
111300970526     C     FUNKEY        IFEQ      'RU'
111400970526     C                   EXSR      RUPGES
111500970526     C                   END
111600970526     C*
111700970526     C                   END
111800970526     C*
111900970526     C                   ENDSR
112000970526     C*------------------------------------------------------------------------*
112100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
112200970526     C*------------------------------------------------------------------------*
112300970526     C     FUNVAR        BEGSR
112400970526     C*
112500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
112600970526     C     KEY           IFEQ      ENTER
112700970526     C                   MOVEL     *BLANKS       FUNKEY
112800970526     C                   END
112900970526     C     KEY           IFEQ      F1
113000970526     C                   MOVEL     '01'          FUNKEY
113100970526     C                   END
113200970526     C     KEY           IFEQ      F2
113300970526     C                   MOVEL     '02'          FUNKEY
113400970526     C                   END
113500970526     C     KEY           IFEQ      F3
113600970526     C                   MOVEL     '03'          FUNKEY
113700970526     C                   END
113800970526     C     KEY           IFEQ      F4
113900970526     C                   MOVEL     '04'          FUNKEY
114000970526     C                   END
114100970526     C     KEY           IFEQ      F5
114200970526     C                   MOVEL     '05'          FUNKEY
114300970526     C                   END
114400970526     C     KEY           IFEQ      F6
114500970526     C                   MOVEL     '06'          FUNKEY
114600970526     C                   END
114700970526     C     KEY           IFEQ      F7
114800970526     C                   MOVEL     '07'          FUNKEY
114900970526     C                   END
115000970526     C     KEY           IFEQ      F8
115100970526     C                   MOVEL     '08'          FUNKEY
115200970526     C                   END
115300970526     C     KEY           IFEQ      F9
115400970526     C                   MOVEL     '09'          FUNKEY
115500970526     C                   END
115600970526     C     KEY           IFEQ      F10
115700970526     C                   MOVEL     '10'          FUNKEY
115800970526     C                   END
115900970526     C     KEY           IFEQ      F11
116000970526     C                   MOVEL     '11'          FUNKEY
116100970526     C                   END
116200970526     C     KEY           IFEQ      F12
116300970526     C                   MOVEL     '12'          FUNKEY
116400970526     C                   END
116500970526     C     KEY           IFEQ      F13
116600970526     C                   MOVEL     '13'          FUNKEY
116700970526     C                   END
116800970526     C     KEY           IFEQ      F14
116900970526     C                   MOVEL     '14'          FUNKEY
117000970526     C                   END
117100970526     C     KEY           IFEQ      F15
117200970526     C                   MOVEL     '15'          FUNKEY
117300970526     C                   END
117400970526     C     KEY           IFEQ      F16
117500970526     C                   MOVEL     '16'          FUNKEY
117600970526     C                   END
117700970526     C     KEY           IFEQ      F17
117800970526     C                   MOVEL     '17'          FUNKEY
117900970526     C                   END
118000970526     C     KEY           IFEQ      F18
118100970526     C                   MOVEL     '18'          FUNKEY
118200970526     C                   END
118300970526     C     KEY           IFEQ      F19
118400970526     C                   MOVEL     '19'          FUNKEY
118500970526     C                   END
118600970526     C     KEY           IFEQ      F20
118700970526     C                   MOVEL     '20'          FUNKEY
118800970526     C                   END
118900970526     C     KEY           IFEQ      F21
119000970526     C                   MOVEL     '21'          FUNKEY
119100970526     C                   END
119200970526     C     KEY           IFEQ      F22
119300970526     C                   MOVEL     '22'          FUNKEY
119400970526     C                   END
119500970526     C     KEY           IFEQ      F23
119600970526     C                   MOVEL     '23'          FUNKEY
119700970526     C                   END
119800970526     C     KEY           IFEQ      F24
119900970526     C                   MOVEL     '24'          FUNKEY
120000970526     C                   END
120100970526     C     KEY           IFEQ      ROLLUP
120200970526     C                   MOVEL     'RU'          FUNKEY
120300970526     C                   END
120400970526     C     KEY           IFEQ      HELP
120500970526     C                   MOVEL     'HP'          FUNKEY
120600970526     C                   END
120700970526     C*
120800970526     C                   ENDSR
120900970526     C*------------------------------------------------------------------------*
121000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
121100970526     C*------------------------------------------------------------------------*
121200970526     C     FUNGES        BEGSR
121300970526     C*
121400970526     C* PULISCO CAMPI DI LAVORO
121500970526     C                   Z-ADD     0             W                 5 0
121600970526     C*
121700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
121800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
121900970526     C     1             DO        24            I
122000970526     C                   MOVEL     FUD(I)        FUDRIG
122100970526     C*
122200970526     C* PULIZIA FUNZIONI PRECEDENTI
122300970526     C                   MOVEL     *BLANKS       FUV(I)
122400970526     C*
122500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
122600970526     C* (SALTA F23 E F24)
122700970526     C     FUDATT        IFEQ      'S'
122800970526     C     FUDFUN        ANDNE     '23'
122900970526     C     FUDFUN        ANDNE     '24'
123000970526     C                   ADD       1             W
123100970526     C                   MOVEL     FUDDES        FUV(W)
123200970526     C                   END
123300970526     C                   END                                                    DO WHILE
123400970526     C*
123500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
123600970526     C* ALTRIMENTI DISATTIVO F24
123700970526     C     1             DO        24            I
123800970526     C                   MOVEL     FUD(I)        FUDRIG
123900970526     C*
124000970526     C     FUDFUN        IFEQ      '24'
124100970526     C     W             IFGT      6
124200970526     C                   MOVEL     'S'           FUDATT
124300970526     C                   ELSE
124400970526     C                   MOVEL     *BLANKS       FUDATT
124500970526     C                   END                                                    W GT 8
124600970526     C                   MOVEL     FUDRIG        FUD(I)
124700970526     C                   END
124800970526     C                   END                                                    DO WHILE
124900970526     C*
125000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
125100970526     C     W             DIV       6             I
125200970526     C                   MVR                     Z
125300970526     C*
125400970526     C     Z             IFNE      0
125500970526     C     I             ADD       1             Z
125600970526     C                   ELSE
125700970526     C                   Z-ADD     I             Z
125800970526     C                   END
125900970526     C*
126000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
126100970526     C                   ADD       1             FUNGRU
126200970526     C     FUNGRU        IFGT      Z
126300970526     C     FUNGRU        ORLT      1
126400970526     C                   Z-ADD     1             FUNGRU
126500970526     C                   END
126600970526     C*
126700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
126800970526     C     FUNGRU        IFEQ      1
126900970526     C                   MOVEL     FUV(1)        FUN001
127000970526     C                   MOVEL     FUV(2)        FUN002
127100970526     C                   MOVEL     FUV(3)        FUN003
127200970526     C                   MOVEL     FUV(4)        FUN004
127300970526     C                   MOVEL     FUV(5)        FUN005
127400970526     C                   MOVEL     FUV(6)        FUN006
127500970526     C                   END
127600970526     C*
127700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
127800970526     C     FUNGRU        IFEQ      2
127900970526     C                   MOVEL     FUV(7)        FUN001
128000970526     C                   MOVEL     FUV(8)        FUN002
128100970526     C                   MOVEL     FUV(9)        FUN003
128200970526     C                   MOVEL     FUV(10)       FUN004
128300970526     C                   MOVEL     FUV(11)       FUN005
128400970526     C                   MOVEL     FUV(12)       FUN006
128500970526     C                   END
128600970526     C*
128700970526     C* TERZO GRUPPO DI 6 FUNZIONI
128800970526     C     FUNGRU        IFEQ      3
128900970526     C                   MOVEL     FUV(13)       FUN001
129000970526     C                   MOVEL     FUV(14)       FUN002
129100970526     C                   MOVEL     FUV(15)       FUN003
129200970526     C                   MOVEL     FUV(16)       FUN004
129300970526     C                   MOVEL     FUV(17)       FUN005
129400970526     C                   MOVEL     FUV(18)       FUN006
129500970526     C                   END
129600970526     C*
129700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
129800970526     C     FUNGRU        IFEQ      4
129900970526     C                   MOVEL     FUV(19)       FUN001
130000970526     C                   MOVEL     FUV(20)       FUN002
130100970526     C                   MOVEL     FUV(21)       FUN003
130200970526     C                   MOVEL     FUV(22)       FUN004
130300970526     C                   MOVEL     *BLANKS       FUN005
130400970526     C                   MOVEL     *BLANKS       FUN006
130500970526     C                   END
130600970526     C*
130700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
130800970526     C                   MOVEL     *BLANKS       FUN007
130900970526     C                   MOVEL     *BLANKS       FUN008
131000970526     C*
131100970526     C     1             DO        24            I
131200970526     C                   MOVEL     FUD(I)        FUDRIG
131300970526     C     FUDFUN        IFEQ      '23'
131400970526     C     FUDATT        ANDEQ     'S'
131500970526     C                   MOVEL     FUDDES        FUN007
131600970526     C                   END
131700970526     C     FUDFUN        IFEQ      '24'
131800970526     C     FUDATT        ANDEQ     'S'
131900970526     C                   MOVEL     FUDDES        FUN008
132000970526     C                   END
132100970526     C                   END                                                    DO
132200970526     C*
132300970526     C                   ENDSR
132400970526     C*------------------------------------------------------------------------*
132500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
132600970526     C*------------------------------------------------------------------------*
132700970526     C     OPZGES        BEGSR
132800970526     C*
132900970526     C* PULISCO CAMPI DI LAVORO
133000970526     C                   Z-ADD     0             W                 5 0
133100970526     C*
133200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
133300970526     C* E CARICO TABELLA OPZIONI ATTIVE
133400970526     C     1             DO        24            I
133500970526     C                   MOVEL     OPD(I)        OPDRIG
133600970526     C*
133700970526     C* PULIZIA OPZIONI PRECEDENTI
133800970526     C                   MOVEL     *BLANKS       OPV(I)
133900970526     C*
134000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
134100970526     C     OPDATT        IFEQ      'S'
134200970526     C                   ADD       1             W
134300970526     C                   MOVEL     OPDDES        OPV(W)
134400970526     C                   END
134500970526     C                   END                                                    DO WHILE
134600970526     C*
134700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
134800970526     C* ALTRIMENTI DISATTIVO F23
134900970526     C     1             DO        24            I
135000970526     C                   MOVEL     FUD(I)        FUDRIG
135100970526     C*
135200970526     C     FUDFUN        IFEQ      '23'
135300970526     C     W             IFGT      8
135400970526     C                   MOVEL     'S'           FUDATT
135500970526     C                   ELSE
135600970526     C                   MOVEL     *BLANKS       FUDATT
135700970526     C                   END                                                    W GT 8
135800970526     C                   MOVEL     FUDRIG        FUD(I)
135900970526     C                   END
136000970526     C                   END                                                    DO WHILE
136100970526     C*
136200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
136300970526     C     W             DIV       8             I
136400970526     C                   MVR                     Z                 5 0
136500970526     C*
136600970526     C     Z             IFNE      0
136700970526     C     I             ADD       1             Z
136800970526     C                   ELSE
136900970526     C                   Z-ADD     I             Z
137000970526     C                   END
137100970526     C*
137200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
137300970526     C                   ADD       1             OPZGRU
137400970526     C     OPZGRU        IFGT      Z
137500970526     C     OPZGRU        ORLT      1
137600970526     C                   Z-ADD     1             OPZGRU
137700970526     C                   END
137800970526     C*
137900970526     C* PRIMO GRUPPO DI 8 OPZIONI
138000970526     C     OPZGRU        IFEQ      1
138100970526     C                   MOVEL     OPV(1)        OPZ001
138200970526     C                   MOVEL     OPV(2)        OPZ002
138300970526     C                   MOVEL     OPV(3)        OPZ003
138400970526     C                   MOVEL     OPV(4)        OPZ004
138500970526     C                   MOVEL     OPV(5)        OPZ005
138600970526     C                   MOVEL     OPV(6)        OPZ006
138700970526     C                   MOVEL     OPV(7)        OPZ007
138800970526     C                   MOVEL     OPV(8)        OPZ008
138900970526     C                   END
139000970526     C*
139100970526     C* SECONDO GRUPPO DI 8 OPZIONI
139200970526     C     OPZGRU        IFEQ      2
139300970526     C                   MOVEL     OPV(9)        OPZ001
139400970526     C                   MOVEL     OPV(10)       OPZ002
139500970526     C                   MOVEL     OPV(11)       OPZ003
139600970526     C                   MOVEL     OPV(12)       OPZ004
139700970526     C                   MOVEL     OPV(13)       OPZ005
139800970526     C                   MOVEL     OPV(14)       OPZ006
139900970526     C                   MOVEL     OPV(15)       OPZ007
140000970526     C                   MOVEL     OPV(16)       OPZ008
140100970526     C                   END
140200970526     C*
140300970526     C* TERZO GRUPPO DI 8 OPZIONI
140400970526     C     OPZGRU        IFEQ      3
140500970526     C                   MOVEL     OPV(17)       OPZ001
140600970526     C                   MOVEL     OPV(18)       OPZ002
140700970526     C                   MOVEL     OPV(19)       OPZ003
140800970526     C                   MOVEL     OPV(20)       OPZ004
140900970526     C                   MOVEL     OPV(21)       OPZ005
141000970526     C                   MOVEL     OPV(22)       OPZ006
141100970526     C                   MOVEL     OPV(23)       OPZ007
141200970526     C                   MOVEL     OPV(24)       OPZ008
141300970526     C                   END
141400970526     C*
141500970526     C                   ENDSR
141600970526     C*------------------------------------------------------------------------*
141700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
141800970526     C*------------------------------------------------------------------------*
141900970526     C     OPZCON        BEGSR
142000970526     C*
142100970526     C* PULISCO CAMPI DI LAVORO
142200970526     C                   Z-ADD     0             I                 5 0
142300970526     C                   Z-ADD     0             W                 5 0
142400970526     C*
142500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
142600970526     C     1             DO        24            I
142700970526     C                   MOVEL     OPD(I)        OPDRIG
142800970526     C*
142900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
143000970526     C     OPZKEY        IFEQ      OPDOPZ
143100970526     C     OPZKEY        OREQ      OPDOP1
143200970526     C*
143300970526     C     OPDATT        IFEQ      'S'
143400970526     C                   MOVEL     OPDOPZ        OPZKEY
143500970526     C                   Z-ADD     I             W
143600970526     C                   END
143700970526     C                   END
143800970526     C*
143900970526     C                   END                                                    DO WHILE
144000970526     C*
144100970526     C* OPZIONE NON ATTIVA (ERRORE)
144200970526     C     W             IFEQ      0
144300970526     C*
144400970526     C* OPZIONE NON OK
144500970526     C                   Z-ADD     1             OPZOK
144600970526     C*
144700970526     C* EMISSIONE MESSAGGIO DI ERRORE
144800970526     C                   MOVEL     MSG(1)        DSMSMS
144900970526     C                   ELSE
145000970526     C*
145100970526     C* OPZIONE ATTIVA (PROCEDI ...)
145200970526     C*
145300970526     C     OPZKEY        IFEQ      ' 1'
145400970526     C                   EXSR      O01GES
145500970526     C                   END
145600970526     C     OPZKEY        IFEQ      ' 2'
145700970526     C                   EXSR      O02GES
145800970526     C                   END
145900970526     C     OPZKEY        IFEQ      ' 4'
146000970526     C                   EXSR      O04GES
146100970526     C                   END
146200970526     C     OPZKEY        IFEQ      ' 5'
146300970526     C                   EXSR      O05GES
146400970526     C                   END
146500970526     C     OPZKEY        IFEQ      ' 7'
146600970526     C                   EXSR      O07GES
146700970526     C                   END
146800970526     C                   END                                                    OPZ.NON ATTIVA
146900970526     C*
147000970526     C                   ENDSR
147100970526     C*------------------------------------------------------------------------*
147200970526     C* NOTE COSTRUZIONE SCHIERE
147300970526     C*------------------------------------------------------------------------*
147400970526     C*
147500970526     C* -MSG-MESSAGGI
147600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
147700970526     C* (NON TOCCARE MSG,1 E MSG,2)
147800970526     C*
147900970526     C* -CMD-COMANDI
148000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
148100970526     C*
148200970526     C* -OPD-OPZIONI
148300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
148400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
148500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
148600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
148700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
148800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
148900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
149000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
149100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
149200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
149300970526     C*
149400970526     C* -FUD-FUNZIONI
149500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
149600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
149700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
149800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
149900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
150000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
150100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
150200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
150300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
150400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
150500970526     O*------------------------------------------------------------------------*
150600970526** DEC - DECODIFICHE
150700970526    GESTIONE                                                     1
150800970526INTERROGAZIONE                                                   2
150900970530     SCELTA                                                      3
1510009705264
1511009705265
151200970526** MSG - MESSAGGI
151300970527TISI11R- Opzione non gestita                                                   1
151400970527TISI11R- Tasto funzionale non gestito                                          2
151500970704TISI11R- 3
151600970527TISI11R- Record annullato. Consentito solo il ripristino                       4
151700970527TISI11R- Record non annullato. Impossibile ripristinare                        5
151800970527TISI11R- Accettato solo un parametro di inizio lista                           6
151900970527TISI11R- 7                                                                     7
152000970527TISI11R- 8                                                                     8
152100970527TISI11R- 9                                                                     9
152200970527TISI11R- 10                                                                   10
152300970527TISI11R- 11                                                                   11
152400970527TISI11R- 12                                                                   12
152500970527TISI11R- 13                                                                   13
152600970527TISI11R- 14                                                                   14
152700970527TISI11R- 15                                                                   15
152800970527TISI11R- 16                                                                   16
152900970527TISI11R- 17                                                                   17
153000970527TISI11R- 18                                                                   18
153100970527TISI11R- 19                                                                   19
153200970527TISI11R- 20                                                                   20
153300970526** OPD - OPZIONI
153400970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
153500970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
153600970526                   -3 -     (S/N)=N                        opz. 03¬
153700970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
153800970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
153900970526                   -6 -     (S/N)=N                        opz. 06¬
154000970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
154100970527                   -8 -     (S/N)=N                        opz. 08¬
154200970527                   -9 -     (S/N)=N                        opz. 09¬
154300970527                   -10-     (S/N)=N                        opz. 10¬
154400970526                   -  -     (S/N)=N                        opz. 11¬
154500970526                   -  -     (S/N)=N                        opz. 12¬
154600970526                   -  -     (S/N)=N                        opz. 13¬
154700970526                   -  -     (S/N)=N                        opz. 14¬
154800970526                   -  -     (S/N)=N                        opz. 15¬
154900970526                   -  -     (S/N)=N                        opz. 16¬
155000970526                   -  -     (S/N)=N                        opz. 17¬
155100970526                   -  -     (S/N)=N                        opz. 18¬
155200970526                   -  -     (S/N)=N                        opz. 19¬
155300970526                   -  -     (S/N)=N                        opz. 20¬
155400970526                   -  -     (S/N)=N                        opz. 21¬
155500970526                   -  -     (S/N)=N                        opz. 22¬
155600970526                   -  -     (S/N)=N                        opz. 23¬
155700970526                   -  -     (S/N)=N                        opz. 24¬
155800970526** FUD - FUNZIONI
155900970526                   -        (S/N)=N                        funz.01¬
156000970526                   -        (S/N)=N                        funz.02¬
156100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
156200970526                   -        (S/N)=N                        funz.04¬
156300970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
156400970526F06=Inserimento    -        (S/N)=N                        funz.06¬
156500970526                   -        (S/N)=N                        funz.07¬
156600970527                   -        (S/N)=N                        funz.08¬
156700970527                   -        (S/N)=N                        funz.09¬
156800970527                   -        (S/N)=N                        funz.10¬
156900970526                   -        (S/N)=N                        funz.11¬
157000970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
157100970526                   -        (S/N)=N                        funz.13¬
157200970526                   -        (S/N)=N                        funz.14¬
157300970526                   -        (S/N)=N                        funz.15¬
157400970526                   -        (S/N)=N                        funz.16¬
157500970526                   -        (S/N)=N                        funz.17¬
157600970526                   -        (S/N)=N                        funz.18¬
157700970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
157800970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
157900970527                   -        (S/N)=N                        funz.21¬
158000970527                   -        (S/N)=N                        funz.22¬
158100970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
158200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
