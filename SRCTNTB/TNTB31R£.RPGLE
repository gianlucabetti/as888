000100970526     H*------------------------------------------------------------------------*
000200030122     H* Gestione nuove tabelle: Tabella "GGB"
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030122     FTNTB31D   CF   E             WORKSTN INFDS(DEVDS1)
000900030122     F                                     SFILE(TB31D05:SFLULT)
001000030122     F                                     SFILE(TB31DMS:MSGULT)
001100970526     F*------------
001200030122     Ftntbe01l  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970528     D*------------
002500970528     D* COMANDI
002600970528     D*------------
002700970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI OPZIONI
003000970526     D*------------
003100970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI TASTI FUNZIONALI
003400970526     D*------------
003500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* OPZIONI DA CARICARE A VIDEO
003800970526     D*------------
003900970526     D OPV             S             19    DIM(24)
004000970526     D*------------
004100970526     D* FUNZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970527     D FUV             S             19    DIM(24)
004400991108     D*------------
004500970526     D*------------------------------------------------------------------------*
004600970526     D* INPUT
004700970526     D*------------------------------------------------------------------------*
004800970526     D*------------
004900970526     D* RIDEFINIZIONE OPZIONI VIDEO
005000970526     D*------------
005100970526     D OPDDS           DS
005200970526     D  OPDOPZ                 1      2
005300970526     D  OPDDE1                 3     19
005400970526     D  OPDOP1                21     22
005500970526     D  OPDFI0                23     34
005600970526     D  OPDATT                35     35
005700970526     D  OPDFI1                36     80
005800970526     D  OPDDES                 1     19
005900970526     D  OPDRIG                 1     80
006000970526     D*------------
006100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006200970526     D*------------
006300970526     D FUDDS           DS
006400970526     D  FUDDE1                 1      1
006500970526     D  FUDFUN                 2      3
006600970526     D  FUDDE2                 4     19
006700970526     D  FUDFI0                20     34
006800970526     D  FUDATT                35     35
006900970526     D  FUDFI1                36     80
007000970526     D  FUDDES                 1     19
007100970526     D  FUDRIG                 1     80
007200970526     D*------------
007300970526     D* DS GESTIONE TASTI FUNZIONALI
007400970526     D*------------
007500970526     D DSKEY         E DS
007600970526     D DEVDS1          DS
007700970526     D  KEY                  369    369
007800970526     D*------------
007900970526     D* DS "XMSG" - GESTIONE MESSAGGI
008000970526     D*------------
008100970526     D DSMSG         E DS
008200970526     D*------------
008300020917     D* DS REPERIMENTO DATI UTENTE
008400970526     D*------------
008500020917     D TIBS34DS      E DS                                                       *Profili utente
008600020917     D DDATIUTE      E DS                                                       *Dati utente
008700020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008800030122     D*------------
008900030122     D* DS RIDEFINIZIONE NUOVA TABELLA "GGB"
009000030122     D*------------
009100030122     D DGGB          E DS
009200970526     D*------------
009300970526     D* ARCHITETTURA
009400970526     D*------------
009500970526     D KPJBA         E DS
009600021211     D*------------------
009700030122     D* PASSAGGIO PARAMETRI
009800021211     D*------------------
009900021211     D PARAM           DS                  INZ
010000030122     D  POP1                   1      3
010100030122     D  PCAU                   4      7
010200030123     D  PDES                   8     42
010300030123     D  PMSG                  43    122
010400970526     C*------------------------------------------------------------------------*
010500970526     C* MAIN LINES
010600970526     C*------------------------------------------------------------------------*
010700970526     C*
010800970526     C* SE NON CI SONO ERRORI, PROSEGUE
010900030122     C     ERRL00        IFNE      '1'
011000970526     C*
011100970526     C* CARICA IL SFILE
011200970526     C                   EXSR      POSSFL
011300970526     C*
011400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011500030122     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011800970526     C*
011900970526     C* REIMPOSTA ERRORE DI PROCERURA
012000970526     C                   MOVEL     '0'           ERRL00
012100970526     C*
012200970526     C* EMETTE IL SFILE
012300970526     C                   EXSR      OUTSFL
012400970526     C*
012500970526     C* GESTIONE TASTI FUNZIONALI
012600970526     C                   EXSR      FUNCON
012700970526     C*
012800030122     C                   ENDDO
012900970527     C*
013000030122     C                   ENDIF
013100991108     C*
013200970526     C* FINE PGM
013300970526     C                   SETON                                        LR
013400970526     C*------------------------------------------------------------------------*
013500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013600970526     C*------------------------------------------------------------------------*
013700970526     C     F03GES        BEGSR
013800970526     C*
013900970526     C* IMPOSTA "PREMUTO F03"
014000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014100970526     C*
014200970526     C                   ENDSR
014300970526     C*------------------------------------------------------------------------*
014400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014500970526     C*------------------------------------------------------------------------*
014600970526     C     F05GES        BEGSR
014700970526     C*
014800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
014900970526     C*
015000970526     C                   ENDSR
015100970526     C*------------------------------------------------------------------------*
015200030122     C* F06GES - GESTIONE F06 -> INSERIMENTO
015300970526     C*------------------------------------------------------------------------*
015400000407     C     F06GES        BEGSR
015500970526     C*
015600021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
015700021211     C     DUTLPO        IFNE      'S'
015800000407     C                   MOVEL     '1'           ERRL00
015900000407     C                   MOVEL     MSG(3)        DSMSMS
016000021211     C                   ENDIF
016100000407     C*
016200021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016300030122     C                   MOVEL     *blanks       PCAU
016400000407     C                   MOVEL     'F06'         POP1
016500000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
016600030122     C                   CALL      'TNTB32R'
016700000407     C                   PARM                    KPJBA
016800021211     C                   MOVEL     KPJBU         PARAM
016900000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
017000021211     C                   ENDIF
017100000407     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400991108     C* F12GES - GESTIONE F12 -> RITORNO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F12GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F12"
017900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018000970526     C*
018100970526     C                   ENDSR
018200970528     C*------------------------------------------------------------------------*
018300970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
018400970528     C*------------------------------------------------------------------------*
018500970528     C     F21GES        BEGSR
018600970612     C*
018700991117     C* NON GESTITA
018800970528     C*
018900970528     C                   ENDSR
019000970528     C*------------------------------------------------------------------------*
019100970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
019200970528     C*------------------------------------------------------------------------*
019300970528     C     F22GES        BEGSR
019400970528     C*
019500021211     C* NON GESTITA
019600970528     C*
019700970528     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
020000970526     C*------------------------------------------------------------------------*
020100970526     C     F23GES        BEGSR
020200970526     C*
020300970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F24GES        BEGSR
021000970526     C*
021100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
021600970526     C*------------------------------------------------------------------------*
021700970526     C     RUPGES        BEGSR
021800970526     C*
021900970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
022000970526     C*
022100970526     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300991108     C* ENTGES - GESTIONE ENTER -> INVIO
022400970526     C*------------------------------------------------------------------------*
022500970526     C     ENTGES        BEGSR
022600000407     C*
022700000407     C                   MOVEL     '0'           ERRL00
022800991108     C*
022900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
023000970526     C                   EXSR      NEWPOS
023100021211     C*
023200021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
023300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023400021211     C     SWPOSI        IFNE      0
023500021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
023600021211     C                   ELSE
023700021211     C     SFLULT        IFNE      0
023800021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
023900021211     C*
024000021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
024100021211     C     SWMODI        IFNE      0
024200021211     C                   EXSR      POSSFL
024300021211     C                   ENDIF
024400021211     C                   ENDIF
024500021211     C                   ENDIF
024600021211     C                   ENDIF
024700021211     C*
024800021211     C                   ENDSR
024900000420     C*------------------------------------------------------------------------*
025000000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
025100000420     C*------------------------------------------------------------------------*
025200000420     C     O01GES        BEGSR
025300000420     C*
025400000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
025500030122     C                   MOVEL     VIDCAU        PCAU
025600030123     C                   MOVEL     VIDDES        PDES
025700000420     C                   MOVEL     PARAM         KPJBU
025800000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
025900000420     C*
026000000420     C                   ENDSR
026100000407     C*------------------------------------------------------------------------*
026200000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
026300000407     C*------------------------------------------------------------------------*
026400000407     C     O02GES        BEGSR
026500000407     C*
026600021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
026700021211     C     DUTLPO        IFNE      'S'
026800021211     C                   MOVEL     '1'           ERRL00
026900021211     C                   MOVEL     MSG(3)        DSMSMS
027000021211     C                   ENDIF
027100021211     C*
027200021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027300030122     C                   MOVEL     VIDCAU        PCAU
027400000407     C                   MOVEL     'O02'         POP1
027500000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
027600030122     C                   CALL      'TNTB32R'
027700000407     C                   PARM                    KPJBA
027800021211     C                   MOVEL     KPJBU         PARAM
027900021211     C                   ENDIF
028000000407     C*
028100000407     C                   ENDSR
028200991112     C*------------------------------------------------------------------------*
028300021211     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
028400991112     C*------------------------------------------------------------------------*
028500991112     C     O04GES        BEGSR
028600021211     C*
028700021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
028800021211     C     DUTLPO        IFNE      'S'
028900021211     C                   MOVEL     '1'           ERRL00
029000021211     C                   MOVEL     MSG(3)        DSMSMS
029100021211     C                   ENDIF
029200991112     C*
029300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029400030122     C                   MOVEL     VIDCAU        PCAU
029500000407     C                   MOVEL     'O04'         POP1
029600000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
029700030122     C                   CALL      'TNTB32R'
029800000407     C                   PARM                    KPJBA
029900021211     C                   MOVEL     KPJBU         PARAM
030000000411     C                   ENDIF
030100991112     C*
030200991112     C                   ENDSR
030300991014     C*------------------------------------------------------------------------*
030400000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
030500970526     C*------------------------------------------------------------------------*
030600970526     C     O05GES        BEGSR
030700000407     C*
030800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030900030122     C                   MOVEL     VIDCAU        PCAU
031000000407     C                   MOVEL     'O05'         POP1
031100000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
031200030122     C                   CALL      'TNTB32R'
031300000407     C                   PARM                    KPJBA
031400021211     C                   ENDIF
031500000407     C*
031600970526     C                   ENDSR
031700030124     C*------------------------------------------------------------------------*
031800030124     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO RECORD
031900030124     C*------------------------------------------------------------------------*
032000030124     C     O07GES        BEGSR
032100030124     C*
032200030124     C* OPERAZIONE CONSENTITO SOLO IN SEDE
032300030124     C     DUTLPO        IFNE      'S'
032400030124     C                   MOVEL     '1'           ERRL00
032500030124     C                   MOVEL     MSG(3)        DSMSMS
032600030124     C                   ENDIF
032700030124     C*
032800030124     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032900030124     C                   MOVEL     VIDCAU        PCAU
033000030124     C                   MOVEL     'O07'         POP1
033100030124     C                   EXSR      PASPAR                                       *RICARICA SFILE
033200030124     C                   CALL      'TNTB32R'
033300030124     C                   PARM                    KPJBA
033400030124     C                   MOVEL     KPJBU         PARAM
033500030124     C                   ENDIF
033600030124     C*
033700030124     C                   ENDSR
033800991028     C*------------------------------------------------------------------------*
033900991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
034000991112     C*------------------------------------------------------------------------*
034100991112     C     PASPAR        BEGSR
034200991112     C*
034300000407     C                   MOVEL     PARAM         KPJBU
034400991112     C*
034500991112     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
034800970526     C*------------------------------------------------------------------------*
034900970526     C     NEWPOS        BEGSR
035000970526     C*
035100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
035200970526     C*         1= RICHIESTO POSIZIONAMENTO
035300970526     C                   Z-ADD     0             SWPOSI            7 0
035400970526     C*
035500991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
035600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
035700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
035800970526     C                   Z-ADD     0             SWMODI            1 0
035900970530     C*
036000970530     C* REIMPOSTA INDICATORI DI ERRORE
036100991108     C                   SETOFF                                       808182
036200991108     C                   SETOFF                                       83
036300970526     C*
036400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
036500030122     C     VIDTIM        IFNE      deptim
036600021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
036700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
036800030122     C                   MOVEL     VIDTIM        deptim                         *DEPOSITI
036900021211     C                   ENDIF
037000021211     C                   ENDIF
037100970526     C*
037200970526     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400021211     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
037500970526     C*------------------------------------------------------------------------*
037600970528     C     SETFIL        BEGSR
037700970526     C*
037800970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
037900030122     C                   MOVEL     'GGB'         tbeCOD
038000030122     C     KEYTBE        SETLL     tntbe01l                           99
038100980107     C*
038200021211     C* LETTURA RECORD
038300021211     C     *IN99         IFEQ      *OFF
038400970526     C                   EXSR      LETFIL
038500021211     C                   ENDIF
038600970526     C*
038700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
038800021211     C     *IN99         IFEQ      *ON
038900970526     C                   Z-ADD     1             SFLEOF
039000021211     C                   ENDIF
039100970526     C*
039200970526     C                   ENDSR
039300970526     C*------------------------------------------------------------------------*
039400970526     C* LETFIL - LETTURA RECORD
039500970526     C*------------------------------------------------------------------------*
039600970526     C     LETFIL        BEGSR
039700970526     C*
039800970526     C* LEGGE FINO A:
039900970526     C*     - FINE FILE
040000970526     C*     - TROVATO RECORD VALIDO
040100970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
040200021211     C     *IN99         DOWEQ     *OFF
040300970526     C     WRECOK        ANDEQ     'N'
040400970526     C*
040500970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
040600030122     C     KEYTBE        READE     tntbe01l                               99
040700970528     C*
040800970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
040900021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
041000970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
041100021211     C                   ELSE
041200030122     C                   EVAL      DGGB = tbeUNI
041300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
041400021211     C                   ENDIF
041500021211     C                   ENDDO
041600970526     C*
041700970526     C                   ENDSR
041800970526     C*------------------------------------------------------------------------*
041900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
042000970526     C*------------------------------------------------------------------------*
042100970526     C     CHKREC        BEGSR
042200970526     C*
042300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
042400030122     C*
042500030122     C* FILTRA SE RICHIESTO UN TIPO CAUSALE
042600030122     C     VIDTIM        IFEQ      *blanks
042700030122     C                   ELSE
042800030122     C     VIDTIM        IFNE      §GGBTIM
042900030122     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043000030122     C                   ENDIF
043100030122     C                   ENDIF
043200991108     C*
043300970526     C                   ENDSR
043400970526     C*------------------------------------------------------------------------*
043500000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
043600970526     C*------------------------------------------------------------------------*
043700970526     C     IRESFL        BEGSR
043800970526     C*
043900000407     C                   MOVEL     *blanks       OPZKEY
044000030122     C                   MOVEL     tbeATB        VIDATB
044100030122     C                   MOVEL     tbeKE1        VIDCAU
044200030122     C                   MOVEL     §GGBDES       VIDDES
044300991108     C*
044400970526     C                   ENDSR
044500970526     C*------------------------------------------------------------------------*
044600030122     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
044700970526     C*------------------------------------------------------------------------*
044800970526     C     CHKFOP        BEGSR
044900991112     C*
045000991112     C* OPZIONI
045100970626     C*
045200000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
045300021211     C     DEPOP0        IFEQ      'P01'
045400000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
045500000420     C                   MOVEL     'S'           OPDATT
045600000420     C                   MOVEL     OPDRIG        OPD(1)
045700021211     C                   ELSE
045800000420     C* OPZIONI
045900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
046000970626     C                   MOVEL     'S'           OPDATT
046100970626     C                   MOVEL     OPDRIG        OPD(5)
046200021211     C                   ENDIF
046300970626     C*
046400000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
046500021211     C     DEPOP0        IFEQ      'P02'
046600000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
046700970626     C                   MOVEL     'S'           OPDATT
046800000407     C                   MOVEL     OPDRIG        OPD(2)
046900021211     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
047000021211     C                   MOVEL     'S'           OPDATT
047100021211     C                   MOVEL     OPDRIG        OPD(3)
047200000411     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
047300000411     C                   MOVEL     'S'           OPDATT
047400000411     C                   MOVEL     OPDRIG        OPD(4)
047500991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
047600991112     C                   MOVEL     'S'           OPDATT
047700991112     C                   MOVEL     OPDRIG        OPD(5)
047800030124     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
047900030124     C                   MOVEL     'S'           OPDATT
048000030124     C                   MOVEL     OPDRIG        OPD(7)
048100021211     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
048200000407     C                   MOVEL     'S'           FUDATT
048300000407     C                   MOVEL     FUDRIG        FUD(6)
048400021211     C                   ENDIF
048500970626     C*
048600970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
048700021211     C     DUTLPO        IFNE      'S'                                          *FILIALE
048800000407     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
048900000407     C                   MOVEL     'N'           OPDATT
049000000407     C                   MOVEL     OPDRIG        OPD(2)
049100991112     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
049200991112     C                   MOVEL     'N'           OPDATT
049300991112     C                   MOVEL     OPDRIG        OPD(4)
049400030124     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
049500030124     C                   MOVEL     'N'           OPDATT
049600030124     C                   MOVEL     OPDRIG        OPD(7)
049700021211     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
049800021211     C                   MOVEL     'N'           OPDATT
049900021211     C                   MOVEL     FUDRIG        FUD(6)
050000021211     C                   ENDIF
050100970526     C*
050200970526     C* GESTIONE OPZIONI A VIDEO
050300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
050400970526     C                   EXSR      OPZGES
050500970526     C*
050600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
050700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
050800970526     C                   EXSR      FUNGES
050900970526     C*
051000970526     C                   ENDSR
051100020917     C*--------------------------------------------------------------------------------------------*
051200020917     C* REPERISCE I DATI UTENTE
051300020917     C*--------------------------------------------------------------------------------------------*
051400020917     C     REPDATIUTE    BEGSR
051500020917     C*
051600020917     C* INIZIALIZZA VARIABILI DI WRK
051700020917     C                   CLEAR                   TIBS34DS
051800020917     C                   CLEAR                   AZUTEDS
051900020917     C                   CLEAR                   DDATIUTE
052000020917     C*
052100020917     C     *DTAARA       DEFINE    §azute        azuteds
052200020917     C     *DTAARA       DEFINE    §datiute      ddatiute
052300020917     C                   IN(E)     *DTAARA
052400021211     C                   IF        %Error
052500021211     C                   EVAL      I34Tla = 'L'
052600020917     C                   CALL      'TIBS34R'
052700020917     C                   PARM                    Tibs34Ds
052800020917     C                   IN        *DTAARA
052900021211     C                   ENDIF
053000020917     C*
053100020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
053200020917     C                   MOVEL(P)  RSUT          DSFIRS           20
053300020917     C*
053400020917     C                   ENDSR
053500970526     C*------------------------------------------------------------------------*
053600970526     C* *INZSR - OPERAZIONI INIZIALI
053700970526     C*------------------------------------------------------------------------*
053800970526     C     *INZSR        BEGSR
053900970526     C*---
054000970526     C* RICEVIMENTO PARAMETRI
054100970526     C*---
054200970526     C     *ENTRY        PLIST
054300970612     C                   PARM                    KPJBA
054400021211     C                   MOVEL     KPJBU         PARAM
054500030123     C*
054600030123     C* EFFETTUO ALCUNE CONSIDERAZIONI SUI PARAMETRI DI ENTRATA
054700030123     C                   IF        POP1 = *blanks
054800030123     C                   EVAL      POP1 = 'P02'
054900030123     C                   ENDIF
055000030123     C                   IF        POP1 = 'P1M'
055100030123     C                   EVAL      VIDTIM = 'M'
055200030123     C                   EVAL      deptim = 'M'
055300030123     C                   EVAL      POP1 = 'P01'
055400030123     C                   SETON                                        90
055500030123     C                   ENDIF
055600021211     C                   MOVEL     POP1          DEPOP0            3
055700970526     C*
055800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
055900970526     C     XMSGPA        PLIST
056000970526     C                   PARM                    DSMSMS
056100970526     C                   PARM                    DSMSPG
056200970526     C                   PARM                    DSMSKE
056300970526     C                   PARM                    DSMSER
056400970526     C*
056500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
056600970526     C                   CALL      'XKEY'
056700970526     C                   PARM                    DSKEY
056800970526     C*---
056900970526     C* VARIABILI NON RIFERITE AL DATA BASE
057000970526     C*---
057100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
057200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
057300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
057400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
057500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
057600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
057700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
057800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
057900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
058000970526     C*---
058100030122     C     *LIKE         DEFINE    VIDTIM        deptim
058200991108     C*---
058300970526     C* VARIABILI DI CONTROLLO
058400970526     C*---
058500970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
058600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
058700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
058800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
058900970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
059000000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
059100970526     C*
059200970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
059300030122     C                   MOVEL     'TNTB31R'     DSMSPG
059400030122     C                   MOVEL     'TNTB31R'     MSGQUE
059500970526     C                   MOVEL     *BLANKS       MSGKEY
059600970526     C                   MOVEL     *BLANKS       DSMSMS
059700970526     C                   MOVEL     *BLANKS       DSMSKE
059800970526     C                   Z-ADD     *ZEROS        DSMSER
059900970526     C*---
060000970526     C* CHIAVI DI LETTURA
060100970526     C*---
060200030122     C* LETTURA tntbe01l - PARZIALE
060300030122     C     KEYTBE        KLIST
060400030122     C                   KFLD                    tbeCOD
060500991108     C*
060600970527     C* REPERIMENTO DATI UTENTE
060700020917     C                   EXSR      REPDATIUTE
060800991115     C*
060900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
061000970526     C                   EXSR      CHKFOP
061100970526     C*
061200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
061300021211     C     DEPOP0        IFEQ      'P01'
061400970530     C                   MOVEL     DEC(3)        VIDING
061500021211     C                   ENDIF
061600021211     C     DEPOP0        IFEQ      'P02'
061700970526     C                   MOVEL     DEC(1)        VIDING
061800021211     C                   ENDIF
061900021211     C     DEPOP0        IFEQ      'P05'
062000970526     C                   MOVEL     DEC(2)        VIDING
062100021211     C                   ENDIF
062200970526     C*
062300970526     C                   ENDSR
062400970526     C*------------------------------------------------------------------------*
062500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
062600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
062700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
062800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
062900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
063000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
063200970526     C*------------------------------------------------------------------------*
063300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
063400970526     C*------------------------------------------------------------------------*
063500970526     C     POSSFL        BEGSR
063600970526     C*
063700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
063800970526     C                   EXSR      CLRSFL
063900970526     C*
064000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
064100970526     C                   EXSR      SETFIL
064200970526     C*
064300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
064400970526     C     SFLEOF        IFEQ      0
064500970526     C                   EXSR      CARSFL
064600970526     C                   END
064700970526     C*
064800970526     C                   ENDSR
064900970526     C*------------------------------------------------------------------------*
065000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
065100970526     C*------------------------------------------------------------------------*
065200970526     C     ENTATT        BEGSR
065300970526     C*
065400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
065500970526     C                   Z-ADD     0             OPZOK             1 0
065600970526     C*
065700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
065800970526     C                   Z-ADD     0             SFLVAR            5 0
065900970526     C*
066000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
066100970526     C                   Z-ADD     0             SFLERR
066200970526     C*
066300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
066400970526     C                   Z-ADD     SFLULT        WFLULT
066500970526     C*
066600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
066700030122     C                   READC     TB31D05                                98
066800970526     C     *IN98         DOWEQ     '0'
066900970526     C*
067000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
067100970526     C                   ADD       1             SFLVAR
067200970526     C*
067300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
067400970526     C     OPZOK         IFEQ      0
067500970526     C*
067600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
067700970526     C     OPZKEY        IFNE      *BLANKS
067800970526     C                   EXSR      OPZCON
067900970526     C                   END
068000970526     C*
068100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
068200970526     C*            2. PULIZIA SCELTA IN SFL
068300970526     C     OPZOK         IFEQ      0
068400970526     C                   MOVEL     '0'           *IN41
068500970526     C                   MOVEL     *BLANKS       OPZKEY
068600970526     C                   ELSE
068700970526     C*
068800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
068900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
069000970526     C                   MOVEL     '1'           *IN41
069100970526     C                   Z-ADD     SFLULT        SFLERR
069200970526     C                   END
069300970526     C                   ELSE
069400970526     C*
069500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
069600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
069700970526     C                   MOVEL     '1'           *IN41
069800970526     C                   END                                                    OPZOK=0
069900970526     C*
070000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
070100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
070200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
070300970526     C     OPZOK         IFNE      0
070400970526     C                   Z-ADD     SFLERR        SFLNBR
070500970526     C                   ELSE
070600970526     C                   Z-ADD     SFLULT        SFLNBR
070700970526     C                   END
070800970526     C*
070900970526     C* AGGIORNO RECORD SFL
071000030122     C                   UPDATE    TB31D05
071100970526     C*
071200030122     C                   READC     TB31D05                                98
071300970526     C                   END                                                    DO WHILE
071400970526     C*
071500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
071600970526     C                   Z-ADD     WFLULT        SFLULT
071700970526     C*
071800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
071900970526     C     SFLVAR        IFEQ      0
072000970526     C                   END
072100970526     C*
072200991115     C     OPZKEY        IFNE      ' 5'
072300991115     C                   EXSR      F05GES
072400991115     C                   ENDIF
072500991112     C*
072600970526     C                   ENDSR
072700970526     C*------------------------------------------------------------------------*
072800970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
072900970526     C*------------------------------------------------------------------------*
073000970526     C     CLRSFL        BEGSR
073100970526     C*
073200970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
073300970526     C* (0=NON FINE FILE  1=FINE FILE)
073400970526     C                   Z-ADD     0             SFLEOF
073500970526     C*
073600970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
073700970526     C                   Z-ADD     0             SFLULT
073800970526     C*
073900970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
074000970526     C                   MOVEL     '1'           *IN40
074100030122     C                   WRITE     TB31D06
074200970526     C                   MOVEL     '0'           *IN40
074300970526     C*
074400970526     C                   ENDSR
074500970526     C*------------------------------------------------------------------------*
074600970526     C* CARSFL - CARICAMENTO SFL
074700970526     C*------------------------------------------------------------------------*
074800970526     C     CARSFL        BEGSR
074900970526     C*
075000970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
075100970526     C     SFLULT        IFEQ      0
075200970526     C                   EXSR      SETFIL
075300970526     C                   END
075400970526     C*
075500970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
075600970526     C                   Z-ADD     SFLULT        WFLULT
075700970526     C*
075800970526     C* CICLO:
075900970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
076000970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
076100970526     C                   Z-ADD     1             SFLRIG
076200970526     C     SFLRIG        DOWLE     SFLPAG
076300970526     C     SFLEOF        ANDEQ     0
076400970526     C*
076500970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
076600991116     C                   MOVEL     *BLANKS       OPZKEY
076700970526     C*
076800970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
076900970526     C                   EXSR      IRESFL
077000970526     C*
077100970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
077200970526     C                   ADD       1             SFLULT
077300970526     C*
077400970526     C* SCRITTURA RECORD SFL
077500030122     C                   WRITE     TB31D05
077600970526     C                   ADD       1             SFLRIG
077700970526     C*
077800970526     C* LETTURA FILE DA CARICARE
077900970526     C                   EXSR      LETFIL
078000970526     C                   END                                                    DO WHILE
078100970526     C*
078200970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
078300970526     C     WFLULT        IFNE      SFLULT
078400970526     C                   Z-ADD     WFLULT        SFLNBR
078500970526     C                   ADD       1             SFLNBR
078600970526     C                   END
078700970526     C*
078800970526     C                   ENDSR
078900970526     C*------------------------------------------------------------------------*
079000970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
079100970526     C*------------------------------------------------------------------------*
079200970526     C     OUTSFL        BEGSR
079300970526     C*
079400970526     C* WRITE RECORD TESTATA VIDEO
079500030122     C                   WRITE     TB31D01
079600970526     C*
079700970526     C* VISUALIZZAZIONE MESSAGGIO
079800970526     C                   EXSR      SNDMSG
079900970526     C*
080000970526     C* WRITE RECORD OPZIONI VIDEO
080100030122     C                   WRITE     TB31D02
080200970526     C*
080300970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
080400030122     C                   WRITE     TB31D03
080500970526     C*
080600970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
080700030122     C                   WRITE     TB31D04
080800970526     C*
080900970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
081000970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
081100970526     C     SFLEOF        IFEQ      0
081200970526     C                   MOVEL     '0'           *IN42
081300970526     C                   ELSE
081400970526     C*
081500970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
081600970526     C                   MOVEL     '1'           *IN42
081700970526     C                   END
081800970526     C*
081900970526     C* WRITE RECORD TASTI FUNZIONALI
082000030122     C                   WRITE     TB31D09
082100970526     C*
082200970526     C* CASO DI SFL VUOTO
082300970526     C     SFLULT        IFLE      0
082400970526     C*
082500970526     C*    - WRITE RECORD VUOTO
082600030122     C                   WRITE     TB31D0A
082700030122     C                   READ      TB31D0A                                99
082800970526     C*
082900970526     C                   ELSE
083000970526     C*
083100970526     C* CASO DI SFL CON RECORDS
083200970526     C*
083300970526     C*    - WRITE RECORD CONTROLLO SFL
083400030122     C                   WRITE     TB31D06
083500030122     C                   READ      TB31D06                                99
083600970526     C                   END
083700970526     C*
083800970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
083900030122     C                   READ      TB31D03                                99
084000970526     C*
084100970526     C                   ENDSR
084200970526     C*------------------------------------------------------------------------*
084300970526     C* SNDMSG - MANDA MESSAGGIO
084400970526     C*------------------------------------------------------------------------*
084500970526     C     SNDMSG        BEGSR
084600970526     C*
084700970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
084800970526     C                   MOVEL     '1'           *IN40
084900030122     C                   WRITE     TB31DMC
085000970526     C                   MOVEL     '0'           *IN40
085100970526     C*
085200970526     C* VISUALIZZAZIONE MESSAGGIO
085300970526     C                   Z-ADD     0             MSGULT
085400970526     C                   CALL      'XMSG'        XMSGPA
085500970526     C     DSMSER        IFEQ      0
085600970526     C                   MOVEL     DSMSKE        MSGKEY
085700970526     C                   END
085800970526     C                   Z-ADD     1             MSGULT
085900030122     C                   WRITE     TB31DMS                                      SFL MESS
086000030122     C                   WRITE     TB31DMC                                      CONTR MESS
086100970526     C*
086200970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
086300970526     C                   MOVEL     *BLANKS       DSMSMS
086400970526     C*
086500970526     C                   ENDSR
086600970526     C*------------------------------------------------------------------------*
086700970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
086800970526     C*------------------------------------------------------------------------*
086900970526     C     FUNCON        BEGSR
087000970526     C*
087100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
087200970526     C                   EXSR      FUNVAR
087300970526     C*
087400970526     C* PULISCO CAMPI DI LAVORO
087500970526     C                   Z-ADD     0             I                 5 0
087600970526     C                   Z-ADD     0             W                 5 0
087700970526     C*
087800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
087900970526     C     FUNKEY        IFNE      *BLANKS
088000970526     C     1             DO        24            I
088100970526     C                   MOVEL     FUD(I)        FUDRIG
088200970526     C*
088300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
088400970526     C     FUNKEY        IFEQ      FUDFUN
088500970526     C     FUDATT        ANDEQ     'S'
088600970526     C                   Z-ADD     I             W
088700970526     C                   END
088800970526     C                   END                                                    DO WHILE
088900970526     C*
089000970526     C* FUNZIONE NON ATTIVA (ERRORE)
089100970526     C     W             IFEQ      0
089200970526     C*
089300970526     C* EMISSIONE MESSAGGIO DI ERRORE
089400970526     C                   MOVEL     MSG(2)        DSMSMS
089500970526     C                   END                                                    W=0
089600970526     C                   END                                                    FUNKEY NE *BLANKS
089700970526     C*
089800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
089900970526     C     W             IFNE      0
090000970526     C     FUNKEY        OREQ      *BLANKS
090100970526     C*
090200970526     C     FUNKEY        IFEQ      *BLANKS
090300970526     C                   EXSR      ENTGES
090400970526     C                   END
090500970526     C     FUNKEY        IFEQ      '03'
090600970526     C                   EXSR      F03GES
090700970526     C                   END
090800970526     C     FUNKEY        IFEQ      '05'
090900970526     C                   EXSR      F05GES
091000970526     C                   END
091100970526     C     FUNKEY        IFEQ      '06'
091200970526     C                   EXSR      F06GES
091300970526     C                   END
091400970526     C     FUNKEY        IFEQ      '12'
091500970526     C                   EXSR      F12GES
091600970526     C                   END
091700970528     C     FUNKEY        IFEQ      '21'
091800970528     C                   EXSR      F21GES
091900970528     C                   END
092000970528     C     FUNKEY        IFEQ      '22'
092100970528     C                   EXSR      F22GES
092200970528     C                   END
092300970526     C     FUNKEY        IFEQ      '23'
092400970526     C                   EXSR      F23GES
092500970526     C                   END
092600970526     C     FUNKEY        IFEQ      '24'
092700970526     C                   EXSR      F24GES
092800970526     C                   END
092900970526     C     FUNKEY        IFEQ      'RU'
093000970526     C                   EXSR      RUPGES
093100970526     C                   END
093200970526     C*
093300970526     C                   END
093400970526     C*
093500970526     C                   ENDSR
093600970526     C*------------------------------------------------------------------------*
093700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
093800970526     C*------------------------------------------------------------------------*
093900970526     C     FUNVAR        BEGSR
094000970526     C*
094100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
094200970526     C     KEY           IFEQ      ENTER
094300970526     C                   MOVEL     *BLANKS       FUNKEY
094400970526     C                   END
094500970526     C     KEY           IFEQ      F1
094600970526     C                   MOVEL     '01'          FUNKEY
094700970526     C                   END
094800970526     C     KEY           IFEQ      F2
094900970526     C                   MOVEL     '02'          FUNKEY
095000970526     C                   END
095100970526     C     KEY           IFEQ      F3
095200970526     C                   MOVEL     '03'          FUNKEY
095300970526     C                   END
095400970526     C     KEY           IFEQ      F4
095500970526     C                   MOVEL     '04'          FUNKEY
095600970526     C                   END
095700970526     C     KEY           IFEQ      F5
095800970526     C                   MOVEL     '05'          FUNKEY
095900970526     C                   END
096000970526     C     KEY           IFEQ      F6
096100970526     C                   MOVEL     '06'          FUNKEY
096200970526     C                   END
096300970526     C     KEY           IFEQ      F7
096400970526     C                   MOVEL     '07'          FUNKEY
096500970526     C                   END
096600970526     C     KEY           IFEQ      F8
096700970526     C                   MOVEL     '08'          FUNKEY
096800970526     C                   END
096900970526     C     KEY           IFEQ      F9
097000970526     C                   MOVEL     '09'          FUNKEY
097100970526     C                   END
097200970526     C     KEY           IFEQ      F10
097300970526     C                   MOVEL     '10'          FUNKEY
097400970526     C                   END
097500970526     C     KEY           IFEQ      F11
097600970526     C                   MOVEL     '11'          FUNKEY
097700970526     C                   END
097800970526     C     KEY           IFEQ      F12
097900970526     C                   MOVEL     '12'          FUNKEY
098000970526     C                   END
098100970526     C     KEY           IFEQ      F13
098200970526     C                   MOVEL     '13'          FUNKEY
098300970526     C                   END
098400970526     C     KEY           IFEQ      F14
098500970526     C                   MOVEL     '14'          FUNKEY
098600970526     C                   END
098700970526     C     KEY           IFEQ      F15
098800970526     C                   MOVEL     '15'          FUNKEY
098900970526     C                   END
099000970526     C     KEY           IFEQ      F16
099100970526     C                   MOVEL     '16'          FUNKEY
099200970526     C                   END
099300970526     C     KEY           IFEQ      F17
099400970526     C                   MOVEL     '17'          FUNKEY
099500970526     C                   END
099600970526     C     KEY           IFEQ      F18
099700970526     C                   MOVEL     '18'          FUNKEY
099800970526     C                   END
099900970526     C     KEY           IFEQ      F19
100000970526     C                   MOVEL     '19'          FUNKEY
100100970526     C                   END
100200970526     C     KEY           IFEQ      F20
100300970526     C                   MOVEL     '20'          FUNKEY
100400970526     C                   END
100500970526     C     KEY           IFEQ      F21
100600970526     C                   MOVEL     '21'          FUNKEY
100700970526     C                   END
100800970526     C     KEY           IFEQ      F22
100900970526     C                   MOVEL     '22'          FUNKEY
101000970526     C                   END
101100970526     C     KEY           IFEQ      F23
101200970526     C                   MOVEL     '23'          FUNKEY
101300970526     C                   END
101400970526     C     KEY           IFEQ      F24
101500970526     C                   MOVEL     '24'          FUNKEY
101600970526     C                   END
101700970526     C     KEY           IFEQ      ROLLUP
101800970526     C                   MOVEL     'RU'          FUNKEY
101900970526     C                   END
102000970526     C     KEY           IFEQ      HELP
102100970526     C                   MOVEL     'HP'          FUNKEY
102200970526     C                   END
102300970526     C*
102400970526     C                   ENDSR
102500970526     C*------------------------------------------------------------------------*
102600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
102700970526     C*------------------------------------------------------------------------*
102800970526     C     FUNGES        BEGSR
102900970526     C*
103000970526     C* PULISCO CAMPI DI LAVORO
103100970526     C                   Z-ADD     0             W                 5 0
103200970526     C*
103300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
103400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
103500970526     C     1             DO        24            I
103600970526     C                   MOVEL     FUD(I)        FUDRIG
103700970526     C*
103800970526     C* PULIZIA FUNZIONI PRECEDENTI
103900970526     C                   MOVEL     *BLANKS       FUV(I)
104000970526     C*
104100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
104200970526     C* (SALTA F23 E F24)
104300970526     C     FUDATT        IFEQ      'S'
104400970526     C     FUDFUN        ANDNE     '23'
104500970526     C     FUDFUN        ANDNE     '24'
104600970526     C                   ADD       1             W
104700970526     C                   MOVEL     FUDDES        FUV(W)
104800970526     C                   END
104900970526     C                   END                                                    DO WHILE
105000970526     C*
105100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
105200970526     C* ALTRIMENTI DISATTIVO F24
105300970526     C     1             DO        24            I
105400970526     C                   MOVEL     FUD(I)        FUDRIG
105500970526     C*
105600970526     C     FUDFUN        IFEQ      '24'
105700970526     C     W             IFGT      6
105800970526     C                   MOVEL     'S'           FUDATT
105900970526     C                   ELSE
106000970526     C                   MOVEL     *BLANKS       FUDATT
106100970526     C                   END                                                    W GT 8
106200970526     C                   MOVEL     FUDRIG        FUD(I)
106300970526     C                   END
106400970526     C                   END                                                    DO WHILE
106500970526     C*
106600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
106700970526     C     W             DIV       6             I
106800970526     C                   MVR                     Z
106900970526     C*
107000970526     C     Z             IFNE      0
107100970526     C     I             ADD       1             Z
107200970526     C                   ELSE
107300970526     C                   Z-ADD     I             Z
107400970526     C                   END
107500970526     C*
107600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
107700970526     C                   ADD       1             FUNGRU
107800970526     C     FUNGRU        IFGT      Z
107900970526     C     FUNGRU        ORLT      1
108000970526     C                   Z-ADD     1             FUNGRU
108100970526     C                   END
108200970526     C*
108300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
108400970526     C     FUNGRU        IFEQ      1
108500970526     C                   MOVEL     FUV(1)        FUN001
108600970526     C                   MOVEL     FUV(2)        FUN002
108700970526     C                   MOVEL     FUV(3)        FUN003
108800970526     C                   MOVEL     FUV(4)        FUN004
108900970526     C                   MOVEL     FUV(5)        FUN005
109000970526     C                   MOVEL     FUV(6)        FUN006
109100970526     C                   END
109200970526     C*
109300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
109400970526     C     FUNGRU        IFEQ      2
109500970526     C                   MOVEL     FUV(7)        FUN001
109600970526     C                   MOVEL     FUV(8)        FUN002
109700970526     C                   MOVEL     FUV(9)        FUN003
109800970526     C                   MOVEL     FUV(10)       FUN004
109900970526     C                   MOVEL     FUV(11)       FUN005
110000970526     C                   MOVEL     FUV(12)       FUN006
110100970526     C                   END
110200970526     C*
110300970526     C* TERZO GRUPPO DI 6 FUNZIONI
110400970526     C     FUNGRU        IFEQ      3
110500970526     C                   MOVEL     FUV(13)       FUN001
110600970526     C                   MOVEL     FUV(14)       FUN002
110700970526     C                   MOVEL     FUV(15)       FUN003
110800970526     C                   MOVEL     FUV(16)       FUN004
110900970526     C                   MOVEL     FUV(17)       FUN005
111000970526     C                   MOVEL     FUV(18)       FUN006
111100970526     C                   END
111200970526     C*
111300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
111400970526     C     FUNGRU        IFEQ      4
111500970526     C                   MOVEL     FUV(19)       FUN001
111600970526     C                   MOVEL     FUV(20)       FUN002
111700970526     C                   MOVEL     FUV(21)       FUN003
111800970526     C                   MOVEL     FUV(22)       FUN004
111900970526     C                   MOVEL     *BLANKS       FUN005
112000970526     C                   MOVEL     *BLANKS       FUN006
112100970526     C                   END
112200970526     C*
112300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
112400970526     C                   MOVEL     *BLANKS       FUN007
112500970526     C                   MOVEL     *BLANKS       FUN008
112600970526     C*
112700970526     C     1             DO        24            I
112800970526     C                   MOVEL     FUD(I)        FUDRIG
112900970526     C     FUDFUN        IFEQ      '23'
113000970526     C     FUDATT        ANDEQ     'S'
113100970526     C                   MOVEL     FUDDES        FUN007
113200970526     C                   END
113300970526     C     FUDFUN        IFEQ      '24'
113400970526     C     FUDATT        ANDEQ     'S'
113500970526     C                   MOVEL     FUDDES        FUN008
113600970526     C                   END
113700970526     C                   END                                                    DO
113800970526     C*
113900970526     C                   ENDSR
114000970526     C*------------------------------------------------------------------------*
114100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
114200970526     C*------------------------------------------------------------------------*
114300970526     C     OPZGES        BEGSR
114400970526     C*
114500970526     C* PULISCO CAMPI DI LAVORO
114600970526     C                   Z-ADD     0             W                 5 0
114700970526     C*
114800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
114900970526     C* E CARICO TABELLA OPZIONI ATTIVE
115000991115     C     1             DO        24            I
115100970526     C                   MOVEL     OPD(I)        OPDRIG
115200970526     C*
115300970526     C* PULIZIA OPZIONI PRECEDENTI
115400970526     C                   MOVEL     *BLANKS       OPV(I)
115500970526     C*
115600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
115700970526     C     OPDATT        IFEQ      'S'
115800970526     C                   ADD       1             W
115900970526     C                   MOVEL     OPDDES        OPV(W)
116000970526     C                   END
116100970526     C                   END                                                    DO WHILE
116200970526     C*
116300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
116400970526     C* ALTRIMENTI DISATTIVO F23
116500991115     C     1             DO        24            I
116600970526     C                   MOVEL     FUD(I)        FUDRIG
116700970526     C*
116800970526     C     FUDFUN        IFEQ      '23'
116900970526     C     W             IFGT      8
117000970526     C                   MOVEL     'S'           FUDATT
117100970526     C                   ELSE
117200970526     C                   MOVEL     *BLANKS       FUDATT
117300970526     C                   END                                                    W GT 8
117400970526     C                   MOVEL     FUDRIG        FUD(I)
117500970526     C                   END
117600970526     C                   END                                                    DO WHILE
117700970526     C*
117800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
117900970526     C     W             DIV       8             I
118000970526     C                   MVR                     Z                 5 0
118100970526     C*
118200970526     C     Z             IFNE      0
118300970526     C     I             ADD       1             Z
118400970526     C                   ELSE
118500970526     C                   Z-ADD     I             Z
118600970526     C                   END
118700970526     C*
118800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
118900970526     C                   ADD       1             OPZGRU
119000970526     C     OPZGRU        IFGT      Z
119100970526     C     OPZGRU        ORLT      1
119200970526     C                   Z-ADD     1             OPZGRU
119300970526     C                   END
119400970526     C*
119500970526     C* PRIMO GRUPPO DI 8 OPZIONI
119600970526     C     OPZGRU        IFEQ      1
119700970526     C                   MOVEL     OPV(1)        OPZ001
119800970526     C                   MOVEL     OPV(2)        OPZ002
119900970526     C                   MOVEL     OPV(3)        OPZ003
120000970526     C                   MOVEL     OPV(4)        OPZ004
120100970526     C                   MOVEL     OPV(5)        OPZ005
120200970526     C                   MOVEL     OPV(6)        OPZ006
120300970526     C                   MOVEL     OPV(7)        OPZ007
120400970526     C                   MOVEL     OPV(8)        OPZ008
120500970526     C                   END
120600970526     C*
120700970526     C* SECONDO GRUPPO DI 8 OPZIONI
120800970526     C     OPZGRU        IFEQ      2
120900970526     C                   MOVEL     OPV(9)        OPZ001
121000970526     C                   MOVEL     OPV(10)       OPZ002
121100970526     C                   MOVEL     OPV(11)       OPZ003
121200970526     C                   MOVEL     OPV(12)       OPZ004
121300970526     C                   MOVEL     OPV(13)       OPZ005
121400970526     C                   MOVEL     OPV(14)       OPZ006
121500970526     C                   MOVEL     OPV(15)       OPZ007
121600970526     C                   MOVEL     OPV(16)       OPZ008
121700970526     C                   END
121800970526     C*
121900970526     C* TERZO GRUPPO DI 8 OPZIONI
122000970526     C     OPZGRU        IFEQ      3
122100970526     C                   MOVEL     OPV(17)       OPZ001
122200970526     C                   MOVEL     OPV(18)       OPZ002
122300970526     C                   MOVEL     OPV(19)       OPZ003
122400970526     C                   MOVEL     OPV(20)       OPZ004
122500970526     C                   MOVEL     OPV(21)       OPZ005
122600970526     C                   MOVEL     OPV(22)       OPZ006
122700970526     C                   MOVEL     OPV(23)       OPZ007
122800970526     C                   MOVEL     OPV(24)       OPZ008
122900970526     C                   END
123000970526     C*
123100970526     C                   ENDSR
123200970526     C*------------------------------------------------------------------------*
123300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
123400970526     C*------------------------------------------------------------------------*
123500970526     C     OPZCON        BEGSR
123600970526     C*
123700970526     C* PULISCO CAMPI DI LAVORO
123800970526     C                   Z-ADD     0             I                 5 0
123900970526     C                   Z-ADD     0             W                 5 0
124000970526     C*
124100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
124200970526     C     1             DO        24            I
124300970526     C                   MOVEL     OPD(I)        OPDRIG
124400970526     C*
124500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
124600970526     C     OPZKEY        IFEQ      OPDOPZ
124700970526     C     OPZKEY        OREQ      OPDOP1
124800970526     C*
124900970526     C     OPDATT        IFEQ      'S'
125000970526     C                   MOVEL     OPDOPZ        OPZKEY
125100970526     C                   Z-ADD     I             W
125200970526     C                   END
125300970526     C                   END
125400970526     C*
125500970526     C                   END                                                    DO WHILE
125600970526     C*
125700970526     C* OPZIONE NON ATTIVA (ERRORE)
125800970526     C     W             IFEQ      0
125900970526     C*
126000970526     C* OPZIONE NON OK
126100970526     C                   Z-ADD     1             OPZOK
126200970526     C*
126300970526     C* EMISSIONE MESSAGGIO DI ERRORE
126400970526     C                   MOVEL     MSG(1)        DSMSMS
126500970526     C                   ELSE
126600970526     C*
126700970526     C* OPZIONE ATTIVA (PROCEDI ...)
126800970526     C*
126900021211     C                   MOVEL     *BLANKS       PMSG
127000000420     C     OPZKEY        IFEQ      ' 1'
127100000420     C                   EXSR      O01GES
127200000420     C                   END
127300000407     C     OPZKEY        IFEQ      ' 2'
127400000407     C                   EXSR      O02GES
127500000407     C                   END
127600991110     C     OPZKEY        IFEQ      ' 4'
127700991110     C                   EXSR      O04GES
127800991014     C                   END
127900970526     C     OPZKEY        IFEQ      ' 5'
128000970526     C                   EXSR      O05GES
128100970526     C                   END
128200030124     C     OPZKEY        IFEQ      ' 7'
128300030124     C                   EXSR      O07GES
128400030124     C                   END
128500021211     C                   MOVEL     PMSG          DSMSMS
128600021211     C                   END                                                    OPZ.NON ATTIVA
128700970526     C*
128800970526     C                   ENDSR
128900970526     C*------------------------------------------------------------------------*
129000970526     C* NOTE COSTRUZIONE SCHIERE
129100970526     C*------------------------------------------------------------------------*
129200970526     C*
129300970526     C* -MSG-MESSAGGI
129400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
129500970526     C* (NON TOCCARE MSG,1 E MSG,2)
129600970526     C*
129700970526     C* -CMD-COMANDI
129800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
129900970526     C*
130000970526     C* -OPD-OPZIONI
130100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
130200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
130300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
130400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
130500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
130600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
130700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
130800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
130900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
131000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
131100970526     C*
131200970526     C* -FUD-FUNZIONI
131300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
131400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
131500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
131600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
131700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
131800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
131900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
132000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
132100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
132200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
132300991108     C*
132400991115     C* -STS-CODICI STATI DEI RECORD DI LOG
132500991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
132600991110     C* ================> IMPORTANTE <=================
132700991115     C* per codici di errore in -STS- indicare in -DES- sempre
132800991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
132900991110     C* ERR
133000991108     C*
133100970526     O*------------------------------------------------------------------------*
133200970526** DEC - DECODIFICHE
133300970526    GESTIONE                                                     1
133400000407 INTERROGAZIONE                                                  2
133500970530     SCELTA                                                      3
1336009705264
1337009705265
133800970526** MSG - MESSAGGI
133900030122TNTB31R- Opzione non gestita.                                                  1
134000030122TNTB31R- Tasto funzionale non gestito.                                         2
134100030122TNTB31R- Utente non abilitato all'inserimento.                                 3
134200030122TNTB31R- Impossibile annullare, record già annullato.                          4
134300030122TNTB31R- 5                                                                     5
134400030122TNTB31R- 6                                                                     6
134500030122TNTB31R- 7                                                                     7
134600030122TNTB31R- 8                                                                     8
134700030122TNTB31R- 9                                                                     9
134800030122TNTB31R- 10                                                                   10
134900030122TNTB31R- 11                                                                   11
135000030122TNTB31R- 12                                                                   12
135100030122TNTB31R- 13                                                                   13
135200030122TNTB31R- 14                                                                   14
135300030122TNTB31R- 15                                                                   15
135400030122TNTB31R- 16                                                                   16
135500030122TNTB31R- 17                                                                   17
135600030122TNTB31R- 18                                                                   18
135700030122TNTB31R- 19                                                                   19
135800030122TNTB31R- 20                                                                   20
135900970528** CMD - COMANDI CL
136000021211
136100021211
136200991110
136300970528
136400970528
136500970526** OPD - OPZIONI
136600000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
136700000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
136800030122                   -  -     (S/N)=N                        opz. 03¬
136900000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
137000000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
137100991115                   -  -     (S/N)=N                        opz. 06¬
137200030124 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
137300991110                   -  -     (S/N)=N                        opz. 08¬
137400991110                   -  -     (S/N)=N                        opz. 09¬
137500991110                   -  -     (S/N)=N                        opz. 10¬
137600000407                   -  -     (S/N)=N                        opz. 11¬
137700970526                   -  -     (S/N)=N                        opz. 12¬
137800970526                   -  -     (S/N)=N                        opz. 13¬
137900000407                   -  -     (S/N)=N                        opz. 14¬
138000000407                   -  -     (S/N)=N                        opz. 15¬
138100970526                   -  -     (S/N)=N                        opz. 16¬
138200970526                   -  -     (S/N)=N                        opz. 17¬
138300970526                   -  -     (S/N)=N                        opz. 18¬
138400970526                   -  -     (S/N)=N                        opz. 19¬
138500970526                   -  -     (S/N)=N                        opz. 20¬
138600970526                   -  -     (S/N)=N                        opz. 21¬
138700970526                   -  -     (S/N)=N                        opz. 22¬
138800970526                   -  -     (S/N)=N                        opz. 23¬
138900000407                   -  -     (S/N)=N                        opz. 25¬
139000970526** FUD - FUNZIONI
139100970526                   -        (S/N)=N                        funz.01¬
139200970526                   -        (S/N)=N                        funz.02¬
139300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
139400970526                   -        (S/N)=N                        funz.04¬
139500970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
139600000407F06=Inserimento    -        (S/N)=N                        funz.06¬
139700970526                   -        (S/N)=N                        funz.07¬
139800970527                   -        (S/N)=N                        funz.08¬
139900970527                   -        (S/N)=N                        funz.09¬
140000970527                   -        (S/N)=N                        funz.10¬
140100970526                   -        (S/N)=N                        funz.11¬
140200970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
140300991116                   -        (S/N)=N                        funz.13¬
140400970526                   -        (S/N)=N                        funz.14¬
140500970526                   -        (S/N)=N                        funz.15¬
140600970526                   -        (S/N)=N                        funz.16¬
140700970526                   -        (S/N)=N                        funz.17¬
140800970526                   -        (S/N)=N                        funz.18¬
140900970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
141000970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
141100000407                   -        (S/N)=N                        funz.21¬
141200991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
141300991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
141400991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
