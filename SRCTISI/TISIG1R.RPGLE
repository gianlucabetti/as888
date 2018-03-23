000100970526     H*------------------------------------------------------------------------*
000200021217     H* Gestione file anagrafico Gabbie e Bancali (AZAGB00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800021211     FTISIG1D   CF   E             WORKSTN INFDS(DEVDS1)
000900021211     F                                     SFILE(SIG1D05:SFLULT)
001000021211     F                                     SFILE(SIG1DMS:MSGULT)
001100970526     F*------------
001200021211     Fazagb01l  UF   E           K DISK
001300021211     Fazmgb02l  IF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D*------------
001800970526     D* DECODIFICHE
001900970526     D*------------
002000970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* MESSAGGI
002300970526     D*------------
002400970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500970528     D*------------
002600970528     D* COMANDI
002700970528     D*------------
002800970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* DESCRIZIONI OPZIONI
003100970526     D*------------
003200970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* DESCRIZIONI TASTI FUNZIONALI
003500970526     D*------------
003600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003700970526     D*------------
003800970526     D* OPZIONI DA CARICARE A VIDEO
003900970526     D*------------
004000970526     D OPV             S             19    DIM(24)
004100970526     D*------------
004200970526     D* FUNZIONI DA CARICARE A VIDEO
004300970526     D*------------
004400970527     D FUV             S             19    DIM(24)
004500991108     D*------------
004600970526     D*------------------------------------------------------------------------*
004700970526     D* INPUT
004800970526     D*------------------------------------------------------------------------*
004900970526     D*------------
005000970526     D* RIDEFINIZIONE OPZIONI VIDEO
005100970526     D*------------
005200970526     D OPDDS           DS
005300970526     D  OPDOPZ                 1      2
005400970526     D  OPDDE1                 3     19
005500970526     D  OPDOP1                21     22
005600970526     D  OPDFI0                23     34
005700970526     D  OPDATT                35     35
005800970526     D  OPDFI1                36     80
005900970526     D  OPDDES                 1     19
006000970526     D  OPDRIG                 1     80
006100970526     D*------------
006200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006300970526     D*------------
006400970526     D FUDDS           DS
006500970526     D  FUDDE1                 1      1
006600970526     D  FUDFUN                 2      3
006700970526     D  FUDDE2                 4     19
006800970526     D  FUDFI0                20     34
006900970526     D  FUDATT                35     35
007000970526     D  FUDFI1                36     80
007100970526     D  FUDDES                 1     19
007200970526     D  FUDRIG                 1     80
007300970526     D*------------
007400970526     D* DS GESTIONE TASTI FUNZIONALI
007500970526     D*------------
007600970526     D DSKEY         E DS
007700970526     D DEVDS1          DS
007800970526     D  KEY                  369    369
007900970526     D*------------
008000970526     D* DS "XMSG" - GESTIONE MESSAGGI
008100970526     D*------------
008200970526     D DSMSG         E DS
008300970526     D*------------
008400020917     D* DS REPERIMENTO DATI UTENTE
008500970526     D*------------
008600020917     D TIBS34DS      E DS                                                       *Profili utente
008700020917     D DDATIUTE      E DS                                                       *Dati utente
008800020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008900970527     D*------------------
009000991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
009100970527     D*------------------
009200970527     D WLBDA8          DS                  INZ
009300970527     D  G08DAT                 1      8  0
009400970527     D  G08INV                 9     16  0
009500970527     D  G08ERR                17     17
009600970527     D  G08TGI                18     22  0
009700970526     D*------------
009800991110     D* DS DI PROCEDURA
009900970526     D*------------
010000970526     D* ARCHITETTURA
010100970526     D*------------
010200970526     D KPJBA         E DS
010300021211     D*------------------
010400021211     D* PASSAGGIO PARAMETRI
010500021211     D*------------------
010600021211     D PARAM           DS                  INZ
010700021211     D  POP1                   1      3
010800021211     D  PCOD                   4     18
010900021211     D  PMSG                  19     98
011000970526     C*------------------------------------------------------------------------*
011100970526     C* MAIN LINES
011200970526     C*------------------------------------------------------------------------*
011300970526     C*
011400970526     C* SE NON CI SONO ERRORI, PROSEGUE
011500970526IF  1C     ERRL00        IFNE      '1'
011600970526     C*
011700970526     C* CARICA IL SFILE
011800970526     C                   EXSR      POSSFL
011900970526     C*
012000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012400970526     C*
012500970526     C* REIMPOSTA ERRORE DI PROCERURA
012600970526     C                   MOVEL     '0'           ERRL00
012700970526     C*
012800970526     C* EMETTE IL SFILE
012900970526     C                   EXSR      OUTSFL
013000970526     C*
013100970526     C* GESTIONE TASTI FUNZIONALI
013200970526     C                   EXSR      FUNCON
013300970526     C*
013400970526E   2C                   ENDDO
013500970527     C*
013600970526E   1C                   ENDIF
013700991108     C*
013800970526     C* FINE PGM
013900970526     C                   SETON                                        LR
014000970526     C*------------------------------------------------------------------------*
014100970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014200970526     C*------------------------------------------------------------------------*
014300970526     C     F03GES        BEGSR
014400970526     C*
014500970526     C* IMPOSTA "PREMUTO F03"
014600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014700970526     C*
014800970526     C                   ENDSR
014900970526     C*------------------------------------------------------------------------*
015000970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015100970526     C*------------------------------------------------------------------------*
015200970526     C     F05GES        BEGSR
015300970526     C*
015400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015500970526     C*
015600970526     C                   ENDSR
015700970526     C*------------------------------------------------------------------------*
015800970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015900970526     C*------------------------------------------------------------------------*
016000000407     C     F06GES        BEGSR
016100970526     C*
016200021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
016300021211     C     DUTLPO        IFNE      'S'
016400000407     C                   MOVEL     '1'           ERRL00
016500000407     C                   MOVEL     MSG(3)        DSMSMS
016600021211     C                   ENDIF
016700000407     C*
016800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016900021211     C                   MOVEL     *blanks       PCOD
017000000407     C                   MOVEL     'F06'         POP1
017100000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
017200021211     C                   CALL      'TISIG2R'
017300000407     C                   PARM                    KPJBA
017400021211     C                   MOVEL     KPJBU         PARAM
017500000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
017600021211     C                   ENDIF
017700000407     C*
017800970526     C                   ENDSR
017900970526     C*------------------------------------------------------------------------*
018000991108     C* F12GES - GESTIONE F12 -> RITORNO
018100970526     C*------------------------------------------------------------------------*
018200970526     C     F12GES        BEGSR
018300970526     C*
018400970526     C* IMPOSTA "PREMUTO F12"
018500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018600970526     C*
018700970526     C                   ENDSR
018800970528     C*------------------------------------------------------------------------*
018900970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
019000970528     C*------------------------------------------------------------------------*
019100970528     C     F21GES        BEGSR
019200970612     C*
019300991117     C* NON GESTITA
019400970528     C*
019500970528     C                   ENDSR
019600970528     C*------------------------------------------------------------------------*
019700970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
019800970528     C*------------------------------------------------------------------------*
019900970528     C     F22GES        BEGSR
020000970528     C*
020100021211     C* NON GESTITA
020200970528     C*
020300970528     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F23GES        BEGSR
020800970526     C*
020900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
021000970526     C*
021100970526     C                   ENDSR
021200970526     C*------------------------------------------------------------------------*
021300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021400970526     C*------------------------------------------------------------------------*
021500970526     C     F24GES        BEGSR
021600970526     C*
021700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021800970526     C*
021900970526     C                   ENDSR
022000970526     C*------------------------------------------------------------------------*
022100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
022200970526     C*------------------------------------------------------------------------*
022300970526     C     RUPGES        BEGSR
022400970526     C*
022500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
022600970526     C*
022700970526     C                   ENDSR
022800970526     C*------------------------------------------------------------------------*
022900991108     C* ENTGES - GESTIONE ENTER -> INVIO
023000970526     C*------------------------------------------------------------------------*
023100970526     C     ENTGES        BEGSR
023200000407     C*
023300000407     C                   MOVEL     '0'           ERRL00
023400991108     C*
023500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
023600970526     C                   EXSR      NEWPOS
023700021211     C*
023800021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
023900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024000021211     C     SWPOSI        IFNE      0
024100021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
024200021211     C                   ELSE
024300021211     C     SFLULT        IFNE      0
024400021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
024500021211     C*
024600021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
024700021211     C     SWMODI        IFNE      0
024800021211     C                   EXSR      POSSFL
024900021211     C                   ENDIF
025000021211     C                   ENDIF
025100021211     C                   ENDIF
025200021211     C                   ENDIF
025300021211     C*
025400021211     C                   ENDSR
025500000420     C*------------------------------------------------------------------------*
025600000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
025700000420     C*------------------------------------------------------------------------*
025800000420     C     O01GES        BEGSR
025900000420     C*
026000000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
026100021211     C                   MOVEL     VIDCOD        PCOD
026200000420     C                   MOVEL     PARAM         KPJBU
026300000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
026400000420     C*
026500000420     C                   ENDSR
026600000407     C*------------------------------------------------------------------------*
026700000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
026800000407     C*------------------------------------------------------------------------*
026900000407     C     O02GES        BEGSR
027000000407     C*
027100021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
027200021211     C     DUTLPO        IFNE      'S'
027300021211     C                   MOVEL     '1'           ERRL00
027400030123     C                   MOVEL     MSG(3)        PMSG
027500021211     C                   ENDIF
027600021211     C*
027700021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027800021211     C                   MOVEL     VIDCOD        PCOD
027900000407     C                   MOVEL     'O02'         POP1
028000000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
028100021211     C                   CALL      'TISIG2R'
028200000407     C                   PARM                    KPJBA
028300021211     C                   MOVEL     KPJBU         PARAM
028400021211     C                   ENDIF
028500000407     C*
028600000407     C                   ENDSR
028700021211     C*------------------------------------------------------------------------*
028800021211     C* O03GES - GESTIONE OPZIONE 3= COPIA
028900021211     C*------------------------------------------------------------------------*
029000021211     C     O03GES        BEGSR
029100021211     C*
029200021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
029300021211     C     DUTLPO        IFNE      'S'
029400021211     C                   MOVEL     '1'           ERRL00
029500030123     C                   MOVEL     MSG(3)        PMSG
029600021211     C                   ENDIF
029700021211     C*
029800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029900021211     C                   MOVEL     VIDCOD        PCOD
030000021211     C                   MOVEL     'O03'         POP1
030100021211     C                   EXSR      PASPAR                                       *RICARICA SFILE
030200021211     C                   CALL      'TISIG2R'
030300021211     C                   PARM                    KPJBA
030400021211     C                   MOVEL     KPJBU         PARAM
030500021211     C                   ENDIF
030600021211     C*
030700021211     C                   ENDSR
030800991112     C*------------------------------------------------------------------------*
030900021211     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
031000991112     C*------------------------------------------------------------------------*
031100991112     C     O04GES        BEGSR
031200021211     C*
031300021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
031400021211     C     DUTLPO        IFNE      'S'
031500021211     C                   MOVEL     '1'           ERRL00
031600030123     C                   MOVEL     MSG(3)        PMSG
031700021211     C                   ENDIF
031800991112     C*
031900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032000021211     C*
032100021211     C* VERIFICO CHE NN VI SIANO PRESENTI MOVIMENTO PER L'ARICOLO CHE SI VUOLE ANNULLARE
032200021211     C                   MOVEL     VIDCOD        KEYCOD
032300021211     C     KEYMGBP       SETLL     azmgb02l                               55
032400021211     C*
032500021211     C                   IF        *IN55 = *ON
032600030123     C                   MOVEL     MSG(4)        PMSG
032700021211     C                   ELSE
032800021211     C                   MOVEL     VIDCOD        PCOD
032900000407     C                   MOVEL     'O04'         POP1
033000000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
033100021211     C                   CALL      'TISIG2R'
033200000407     C                   PARM                    KPJBA
033300021211     C                   MOVEL     KPJBU         PARAM
033400021211     C                   ENDIF
033500000411     C                   ENDIF
033600991112     C*
033700991112     C                   ENDSR
033800991014     C*------------------------------------------------------------------------*
033900000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
034000970526     C*------------------------------------------------------------------------*
034100970526     C     O05GES        BEGSR
034200000407     C*
034300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034400021211     C                   MOVEL     VIDCOD        PCOD
034500000407     C                   MOVEL     'O05'         POP1
034600000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
034700021211     C                   CALL      'TISIG2R'
034800000407     C                   PARM                    KPJBA
034900021211     C                   ENDIF
035000000407     C*
035100970526     C                   ENDSR
035200991028     C*------------------------------------------------------------------------*
035300991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
035400991112     C*------------------------------------------------------------------------*
035500991112     C     PASPAR        BEGSR
035600991112     C*
035700000407     C                   MOVEL     PARAM         KPJBU
035800991112     C*
035900991112     C                   ENDSR
036000970526     C*------------------------------------------------------------------------*
036100970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
036200970526     C*------------------------------------------------------------------------*
036300970526     C     NEWPOS        BEGSR
036400970526     C*
036500970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
036600970526     C*         1= RICHIESTO POSIZIONAMENTO
036700970526     C                   Z-ADD     0             SWPOSI            7 0
036800970526     C*
036900991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037000970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037100970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037200970526     C                   Z-ADD     0             SWMODI            1 0
037300970530     C*
037400970530     C* REIMPOSTA INDICATORI DI ERRORE
037500991108     C                   SETOFF                                       808182
037600991108     C                   SETOFF                                       83
037700970526     C*
037800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
037900021211     C     VIDCO1        IFNE      depco1
038000021211     C     VIDTP2        ORNE      deptp2
038100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
038200021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
038300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
038400021211     C                   MOVEL     VIDCO1        depco1                         *DEPOSITI
038500021211     C                   MOVEL     VIDTP2        deptp2                         *DEPOSITI
038600021211     C                   ENDIF
038700021211     C                   ENDIF
038800970526     C*
038900970526     C                   ENDSR
039000970526     C*------------------------------------------------------------------------*
039100021211     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
039200970526     C*------------------------------------------------------------------------*
039300970528     C     SETFIL        BEGSR
039400970526     C*
039500970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
039600021211     C     VIDCO1        IFEQ      *blanks
039700021211     C                   MOVEL     *loval        KEYCOD
039800991108     C                   ELSE
039900021211     C                   MOVEL     VIDCO1        KEYCOD
040000991108     C                   ENDIF
040100021211     C     WTIPRI        IFEQ      'COD'
040200021211     C     KEYAGB        SETLL     azagb01l                           99
040300021211     C                   ENDIF
040400980107     C*
040500021211     C* LETTURA RECORD
040600021211     C     *IN99         IFEQ      *OFF
040700970526     C                   EXSR      LETFIL
040800021211     C                   ENDIF
040900970526     C*
041000970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
041100021211     C     *IN99         IFEQ      *ON
041200970526     C                   Z-ADD     1             SFLEOF
041300021211     C                   ENDIF
041400970526     C*
041500970526     C                   ENDSR
041600970526     C*------------------------------------------------------------------------*
041700970526     C* LETFIL - LETTURA RECORD
041800970526     C*------------------------------------------------------------------------*
041900970526     C     LETFIL        BEGSR
042000970526     C*
042100970526     C* LEGGE FINO A:
042200970526     C*     - FINE FILE
042300970526     C*     - TROVATO RECORD VALIDO
042400970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
042500021211     C     *IN99         DOWEQ     *OFF
042600970526     C     WRECOK        ANDEQ     'N'
042700970526     C*
042800970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
042900021211     C     WTIPRI        IFEQ      'COD'                                        *PER COD. ARTICOLO
043000021211     C                   READ      azagb01l                               99
043100021211     C                   ENDIF
043200970528     C*
043300970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043400021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
043500970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
043600021211     C                   ELSE
043700970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
043800021211     C                   ENDIF
043900021211     C                   ENDDO
044000970526     C*
044100970526     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
044400970526     C*------------------------------------------------------------------------*
044500970526     C     CHKREC        BEGSR
044600970526     C*
044700970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
044800021211     C* FILTRA SE RICHIESTO UN TIPO ARTICOLO
044900021211     C     VIDTP2        ifeq      *blanks
045000021211     C                   else
045100021211     C     agbTIP        ifne      VIDTP2
045200021211     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
045300021211     C                   endif
045400021211     C                   endif
045500991108     C*
045600970526     C                   ENDSR
045700970526     C*------------------------------------------------------------------------*
045800000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
045900970526     C*------------------------------------------------------------------------*
046000970526     C     IRESFL        BEGSR
046100970526     C*
046200000407     C                   MOVEL     *blanks       OPZKEY
046300021211     C                   MOVEL     agbTIP        VIDTIP
046400021211     C                   IF        agbTIP = 'G'
046500021211     C                   EVAL      VIDDTP = '(GABBIE)'
046600021211     C                   ENDIF
046700021211     C                   IF        agbTIP = 'B'
046800021211     C                   EVAL      VIDDTP = '(BANCALI)'
046900021211     C                   ENDIF
047000021211     C                   MOVEL     agbCOD        VIDCOD
047100021211     C                   MOVEL     agbDES        VIDDES
047200991108     C*
047300970526     C                   ENDSR
047400970526     C*------------------------------------------------------------------------*
047500991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
047600970526     C*------------------------------------------------------------------------*
047700970526     C     CHKPAR        BEGSR
047800970528     C*
047900021211     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (COD)
048000021211     C                   MOVEL     'COD'         WTIPRI            3
048100000407     C*
048200970526     C                   ENDSR
048300970526     C*------------------------------------------------------------------------*
048400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
048500970526     C*------------------------------------------------------------------------*
048600970526     C     CHKFOP        BEGSR
048700991112     C*
048800991112     C* OPZIONI
048900970626     C*
049000000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
049100021211     C     DEPOP0        IFEQ      'P01'
049200000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
049300000420     C                   MOVEL     'S'           OPDATT
049400000420     C                   MOVEL     OPDRIG        OPD(1)
049500021211     C                   ELSE
049600000420     C* OPZIONI
049700970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
049800970626     C                   MOVEL     'S'           OPDATT
049900970626     C                   MOVEL     OPDRIG        OPD(5)
050000021211     C                   ENDIF
050100970626     C*
050200000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
050300021211     C     DEPOP0        IFEQ      'P02'
050400000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
050500970626     C                   MOVEL     'S'           OPDATT
050600000407     C                   MOVEL     OPDRIG        OPD(2)
050700021211     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
050800021211     C                   MOVEL     'S'           OPDATT
050900021211     C                   MOVEL     OPDRIG        OPD(3)
051000000411     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
051100000411     C                   MOVEL     'S'           OPDATT
051200000411     C                   MOVEL     OPDRIG        OPD(4)
051300991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
051400991112     C                   MOVEL     'S'           OPDATT
051500991112     C                   MOVEL     OPDRIG        OPD(5)
051600021211     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
051700000407     C                   MOVEL     'S'           FUDATT
051800000407     C                   MOVEL     FUDRIG        FUD(6)
051900021211     C                   ENDIF
052000970626     C*
052100970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
052200021211     C     DUTLPO        IFNE      'S'                                          *FILIALE
052300000407     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
052400000407     C                   MOVEL     'N'           OPDATT
052500000407     C                   MOVEL     OPDRIG        OPD(2)
052600991112     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
052700991112     C                   MOVEL     'N'           OPDATT
052800991112     C                   MOVEL     OPDRIG        OPD(4)
052900021211     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
053000021211     C                   MOVEL     'N'           OPDATT
053100021211     C                   MOVEL     FUDRIG        FUD(6)
053200021211     C                   ENDIF
053300970526     C*
053400970526     C* GESTIONE OPZIONI A VIDEO
053500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
053600970526     C                   EXSR      OPZGES
053700970526     C*
053800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
053900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
054000970526     C                   EXSR      FUNGES
054100970526     C*
054200970526     C                   ENDSR
054300020917     C*--------------------------------------------------------------------------------------------*
054400020917     C* REPERISCE I DATI UTENTE
054500020917     C*--------------------------------------------------------------------------------------------*
054600020917     C     REPDATIUTE    BEGSR
054700020917     C*
054800020917     C* INIZIALIZZA VARIABILI DI WRK
054900020917     C                   CLEAR                   TIBS34DS
055000020917     C                   CLEAR                   AZUTEDS
055100020917     C                   CLEAR                   DDATIUTE
055200020917     C*
055300020917     C     *DTAARA       DEFINE    §azute        azuteds
055400020917     C     *DTAARA       DEFINE    §datiute      ddatiute
055500020917     C                   IN(E)     *DTAARA
055600021211     C                   IF        %Error
055700021211     C                   EVAL      I34Tla = 'L'
055800020917     C                   CALL      'TIBS34R'
055900020917     C                   PARM                    Tibs34Ds
056000020917     C                   IN        *DTAARA
056100021211     C                   ENDIF
056200020917     C*
056300020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
056400020917     C                   MOVEL(P)  RSUT          DSFIRS           20
056500020917     C*
056600020917     C                   ENDSR
056700970526     C*------------------------------------------------------------------------*
056800970526     C* *INZSR - OPERAZIONI INIZIALI
056900970526     C*------------------------------------------------------------------------*
057000970526     C     *INZSR        BEGSR
057100970526     C*---
057200970526     C* RICEVIMENTO PARAMETRI
057300970526     C*---
057400970526     C     *ENTRY        PLIST
057500970612     C                   PARM                    KPJBA
057600021211     C                   MOVEL     KPJBU         PARAM
057700021211     C                   MOVEL     POP1          DEPOP0            3
057800970526     C*
057900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
058000970526     C     XMSGPA        PLIST
058100970526     C                   PARM                    DSMSMS
058200970526     C                   PARM                    DSMSPG
058300970526     C                   PARM                    DSMSKE
058400970526     C                   PARM                    DSMSER
058500970526     C*
058600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
058700970526     C                   CALL      'XKEY'
058800970526     C                   PARM                    DSKEY
058900970526     C*---
059000970526     C* VARIABILI NON RIFERITE AL DATA BASE
059100970526     C*---
059200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
059300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
059400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
059500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
059600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
059700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
059800970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
059900970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
060000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
060100970526     C*---
060200021211     C     *LIKE         DEFINE    VIDCO1        depco1
060300021211     C     *LIKE         DEFINE    VIDTP2        deptp2
060400021211     C     *LIKE         DEFINE    AGBCOD        KEYCOD
060500991108     C*---
060600970526     C* VARIABILI DI CONTROLLO
060700970526     C*---
060800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
060900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
061000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
061100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
061200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
061300000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
061400970526     C*
061500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
061600021211     C                   MOVEL     'TISIG1R'     DSMSPG
061700021211     C                   MOVEL     'TISIG1R'     MSGQUE
061800970526     C                   MOVEL     *BLANKS       MSGKEY
061900970526     C                   MOVEL     *BLANKS       DSMSMS
062000970526     C                   MOVEL     *BLANKS       DSMSKE
062100970526     C                   Z-ADD     *ZEROS        DSMSER
062200970526     C*---
062300970526     C* CHIAVI DI LETTURA
062400970526     C*---
062500021211     C* LETTURA azagb01l - COMPLETA
062600021211     C     KEYAGB        KLIST
062700021211     C                   KFLD                    KEYCOD
062800021211     C* LETTURA azmgb02l - PARZIALE
062900021211     C     KEYMGBP       KLIST
063000021211     C                   KFLD                    KEYCOD
063100991108     C*
063200970527     C* REPERIMENTO DATI UTENTE
063300020917     C                   EXSR      REPDATIUTE
063400970526     C*
063500970526     C* IMPOSTA I PARAMETRI DI INGRESSO
063600970526     C                   EXSR      CHKPAR
063700991115     C*
063800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
063900970526     C                   EXSR      CHKFOP
064000970526     C*
064100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
064200021211     C     DEPOP0        IFEQ      'P01'
064300970530     C                   MOVEL     DEC(3)        VIDING
064400021211     C                   ENDIF
064500021211     C     DEPOP0        IFEQ      'P02'
064600970526     C                   MOVEL     DEC(1)        VIDING
064700021211     C                   ENDIF
064800021211     C     DEPOP0        IFEQ      'P05'
064900970526     C                   MOVEL     DEC(2)        VIDING
065000021211     C                   ENDIF
065100970526     C*
065200970526     C                   ENDSR
065300970526     C*------------------------------------------------------------------------*
065400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065600970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
065700970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
065800970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
065900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066100970526     C*------------------------------------------------------------------------*
066200970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
066300970526     C*------------------------------------------------------------------------*
066400970526     C     POSSFL        BEGSR
066500970526     C*
066600970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
066700970526     C                   EXSR      CLRSFL
066800970526     C*
066900970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
067000970526     C                   EXSR      SETFIL
067100970526     C*
067200970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
067300970526     C     SFLEOF        IFEQ      0
067400970526     C                   EXSR      CARSFL
067500970526     C                   END
067600970526     C*
067700970526     C                   ENDSR
067800970526     C*------------------------------------------------------------------------*
067900970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
068000970526     C*------------------------------------------------------------------------*
068100970526     C     ENTATT        BEGSR
068200970526     C*
068300970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
068400970526     C                   Z-ADD     0             OPZOK             1 0
068500970526     C*
068600970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
068700970526     C                   Z-ADD     0             SFLVAR            5 0
068800970526     C*
068900970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
069000970526     C                   Z-ADD     0             SFLERR
069100970526     C*
069200970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
069300970526     C                   Z-ADD     SFLULT        WFLULT
069400970526     C*
069500970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
069600021211     C                   READC     SIG1D05                                98
069700970526     C     *IN98         DOWEQ     '0'
069800970526     C*
069900970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
070000970526     C                   ADD       1             SFLVAR
070100970526     C*
070200970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
070300970526     C     OPZOK         IFEQ      0
070400970526     C*
070500970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
070600970526     C     OPZKEY        IFNE      *BLANKS
070700970526     C                   EXSR      OPZCON
070800970526     C                   END
070900970526     C*
071000970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
071100970526     C*            2. PULIZIA SCELTA IN SFL
071200970526     C     OPZOK         IFEQ      0
071300970526     C                   MOVEL     '0'           *IN41
071400970526     C                   MOVEL     *BLANKS       OPZKEY
071500970526     C                   ELSE
071600970526     C*
071700970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
071800970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
071900970526     C                   MOVEL     '1'           *IN41
072000970526     C                   Z-ADD     SFLULT        SFLERR
072100970526     C                   END
072200970526     C                   ELSE
072300970526     C*
072400970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
072500970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
072600970526     C                   MOVEL     '1'           *IN41
072700970526     C                   END                                                    OPZOK=0
072800970526     C*
072900970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
073000970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
073100970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
073200970526     C     OPZOK         IFNE      0
073300970526     C                   Z-ADD     SFLERR        SFLNBR
073400970526     C                   ELSE
073500970526     C                   Z-ADD     SFLULT        SFLNBR
073600970526     C                   END
073700970526     C*
073800970526     C* AGGIORNO RECORD SFL
073900021211     C                   UPDATE    SIG1D05
074000970526     C*
074100021211     C                   READC     SIG1D05                                98
074200970526     C                   END                                                    DO WHILE
074300970526     C*
074400970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
074500970526     C                   Z-ADD     WFLULT        SFLULT
074600970526     C*
074700970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
074800970526     C     SFLVAR        IFEQ      0
074900970526     C                   END
075000970526     C*
075100991115     C     OPZKEY        IFNE      ' 5'
075200991115     C                   EXSR      F05GES
075300991115     C                   ENDIF
075400991112     C*
075500970526     C                   ENDSR
075600970526     C*------------------------------------------------------------------------*
075700970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
075800970526     C*------------------------------------------------------------------------*
075900970526     C     CLRSFL        BEGSR
076000970526     C*
076100970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
076200970526     C* (0=NON FINE FILE  1=FINE FILE)
076300970526     C                   Z-ADD     0             SFLEOF
076400970526     C*
076500970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
076600970526     C                   Z-ADD     0             SFLULT
076700970526     C*
076800970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
076900970526     C                   MOVEL     '1'           *IN40
077000021211     C                   WRITE     SIG1D06
077100970526     C                   MOVEL     '0'           *IN40
077200970526     C*
077300970526     C                   ENDSR
077400970526     C*------------------------------------------------------------------------*
077500970526     C* CARSFL - CARICAMENTO SFL
077600970526     C*------------------------------------------------------------------------*
077700970526     C     CARSFL        BEGSR
077800970526     C*
077900970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
078000970526     C     SFLULT        IFEQ      0
078100970526     C                   EXSR      SETFIL
078200970526     C                   END
078300970526     C*
078400970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
078500970526     C                   Z-ADD     SFLULT        WFLULT
078600970526     C*
078700970526     C* CICLO:
078800970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
078900970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
079000970526     C                   Z-ADD     1             SFLRIG
079100970526     C     SFLRIG        DOWLE     SFLPAG
079200970526     C     SFLEOF        ANDEQ     0
079300970526     C*
079400970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
079500991116     C                   MOVEL     *BLANKS       OPZKEY
079600970526     C*
079700970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
079800970526     C                   EXSR      IRESFL
079900970526     C*
080000970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
080100970526     C                   ADD       1             SFLULT
080200970526     C*
080300970526     C* SCRITTURA RECORD SFL
080400021211     C                   WRITE     SIG1D05
080500970526     C                   ADD       1             SFLRIG
080600970526     C*
080700970526     C* LETTURA FILE DA CARICARE
080800970526     C                   EXSR      LETFIL
080900970526     C                   END                                                    DO WHILE
081000970526     C*
081100970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
081200970526     C     WFLULT        IFNE      SFLULT
081300970526     C                   Z-ADD     WFLULT        SFLNBR
081400970526     C                   ADD       1             SFLNBR
081500970526     C                   END
081600970526     C*
081700970526     C                   ENDSR
081800970526     C*------------------------------------------------------------------------*
081900970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
082000970526     C*------------------------------------------------------------------------*
082100970526     C     OUTSFL        BEGSR
082200970526     C*
082300970526     C* WRITE RECORD TESTATA VIDEO
082400021211     C                   WRITE     SIG1D01
082500970526     C*
082600970526     C* VISUALIZZAZIONE MESSAGGIO
082700970526     C                   EXSR      SNDMSG
082800970526     C*
082900970526     C* WRITE RECORD OPZIONI VIDEO
083000021211     C                   WRITE     SIG1D02
083100970526     C*
083200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
083300021211     C                   WRITE     SIG1D03
083400970526     C*
083500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
083600021211     C                   WRITE     SIG1D04
083700970526     C*
083800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
083900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
084000970526     C     SFLEOF        IFEQ      0
084100970526     C                   MOVEL     '0'           *IN42
084200970526     C                   ELSE
084300970526     C*
084400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
084500970526     C                   MOVEL     '1'           *IN42
084600970526     C                   END
084700970526     C*
084800970526     C* WRITE RECORD TASTI FUNZIONALI
084900021211     C                   WRITE     SIG1D09
085000970526     C*
085100970526     C* CASO DI SFL VUOTO
085200970526     C     SFLULT        IFLE      0
085300970526     C*
085400970526     C*    - WRITE RECORD VUOTO
085500021211     C                   WRITE     SIG1D0A
085600021211     C                   READ      SIG1D0A                                99
085700970526     C*
085800970526     C                   ELSE
085900970526     C*
086000970526     C* CASO DI SFL CON RECORDS
086100970526     C*
086200970526     C*    - WRITE RECORD CONTROLLO SFL
086300021211     C                   WRITE     SIG1D06
086400021211     C                   READ      SIG1D06                                99
086500970526     C                   END
086600970526     C*
086700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
086800021211     C                   READ      SIG1D03                                99
086900970526     C*
087000970526     C                   ENDSR
087100970526     C*------------------------------------------------------------------------*
087200970526     C* SNDMSG - MANDA MESSAGGIO
087300970526     C*------------------------------------------------------------------------*
087400970526     C     SNDMSG        BEGSR
087500970526     C*
087600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
087700970526     C                   MOVEL     '1'           *IN40
087800021211     C                   WRITE     SIG1DMC
087900970526     C                   MOVEL     '0'           *IN40
088000970526     C*
088100970526     C* VISUALIZZAZIONE MESSAGGIO
088200970526     C                   Z-ADD     0             MSGULT
088300970526     C                   CALL      'XMSG'        XMSGPA
088400970526     C     DSMSER        IFEQ      0
088500970526     C                   MOVEL     DSMSKE        MSGKEY
088600970526     C                   END
088700970526     C                   Z-ADD     1             MSGULT
088800021211     C                   WRITE     SIG1DMS                                      SFL MESS
088900021211     C                   WRITE     SIG1DMC                                      CONTR MESS
089000970526     C*
089100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
089200970526     C                   MOVEL     *BLANKS       DSMSMS
089300970526     C*
089400970526     C                   ENDSR
089500970526     C*------------------------------------------------------------------------*
089600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
089700970526     C*------------------------------------------------------------------------*
089800970526     C     FUNCON        BEGSR
089900970526     C*
090000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
090100970526     C                   EXSR      FUNVAR
090200970526     C*
090300970526     C* PULISCO CAMPI DI LAVORO
090400970526     C                   Z-ADD     0             I                 5 0
090500970526     C                   Z-ADD     0             W                 5 0
090600970526     C*
090700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
090800970526     C     FUNKEY        IFNE      *BLANKS
090900970526     C     1             DO        24            I
091000970526     C                   MOVEL     FUD(I)        FUDRIG
091100970526     C*
091200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
091300970526     C     FUNKEY        IFEQ      FUDFUN
091400970526     C     FUDATT        ANDEQ     'S'
091500970526     C                   Z-ADD     I             W
091600970526     C                   END
091700970526     C                   END                                                    DO WHILE
091800970526     C*
091900970526     C* FUNZIONE NON ATTIVA (ERRORE)
092000970526     C     W             IFEQ      0
092100970526     C*
092200970526     C* EMISSIONE MESSAGGIO DI ERRORE
092300970526     C                   MOVEL     MSG(2)        DSMSMS
092400970526     C                   END                                                    W=0
092500970526     C                   END                                                    FUNKEY NE *BLANKS
092600970526     C*
092700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
092800970526     C     W             IFNE      0
092900970526     C     FUNKEY        OREQ      *BLANKS
093000970526     C*
093100970526     C     FUNKEY        IFEQ      *BLANKS
093200970526     C                   EXSR      ENTGES
093300970526     C                   END
093400970526     C     FUNKEY        IFEQ      '03'
093500970526     C                   EXSR      F03GES
093600970526     C                   END
093700970526     C     FUNKEY        IFEQ      '05'
093800970526     C                   EXSR      F05GES
093900970526     C                   END
094000970526     C     FUNKEY        IFEQ      '06'
094100970526     C                   EXSR      F06GES
094200970526     C                   END
094300970526     C     FUNKEY        IFEQ      '12'
094400970526     C                   EXSR      F12GES
094500970526     C                   END
094600970528     C     FUNKEY        IFEQ      '21'
094700970528     C                   EXSR      F21GES
094800970528     C                   END
094900970528     C     FUNKEY        IFEQ      '22'
095000970528     C                   EXSR      F22GES
095100970528     C                   END
095200970526     C     FUNKEY        IFEQ      '23'
095300970526     C                   EXSR      F23GES
095400970526     C                   END
095500970526     C     FUNKEY        IFEQ      '24'
095600970526     C                   EXSR      F24GES
095700970526     C                   END
095800970526     C     FUNKEY        IFEQ      'RU'
095900970526     C                   EXSR      RUPGES
096000970526     C                   END
096100970526     C*
096200970526     C                   END
096300970526     C*
096400970526     C                   ENDSR
096500970526     C*------------------------------------------------------------------------*
096600970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
096700970526     C*------------------------------------------------------------------------*
096800970526     C     FUNVAR        BEGSR
096900970526     C*
097000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
097100970526     C     KEY           IFEQ      ENTER
097200970526     C                   MOVEL     *BLANKS       FUNKEY
097300970526     C                   END
097400970526     C     KEY           IFEQ      F1
097500970526     C                   MOVEL     '01'          FUNKEY
097600970526     C                   END
097700970526     C     KEY           IFEQ      F2
097800970526     C                   MOVEL     '02'          FUNKEY
097900970526     C                   END
098000970526     C     KEY           IFEQ      F3
098100970526     C                   MOVEL     '03'          FUNKEY
098200970526     C                   END
098300970526     C     KEY           IFEQ      F4
098400970526     C                   MOVEL     '04'          FUNKEY
098500970526     C                   END
098600970526     C     KEY           IFEQ      F5
098700970526     C                   MOVEL     '05'          FUNKEY
098800970526     C                   END
098900970526     C     KEY           IFEQ      F6
099000970526     C                   MOVEL     '06'          FUNKEY
099100970526     C                   END
099200970526     C     KEY           IFEQ      F7
099300970526     C                   MOVEL     '07'          FUNKEY
099400970526     C                   END
099500970526     C     KEY           IFEQ      F8
099600970526     C                   MOVEL     '08'          FUNKEY
099700970526     C                   END
099800970526     C     KEY           IFEQ      F9
099900970526     C                   MOVEL     '09'          FUNKEY
100000970526     C                   END
100100970526     C     KEY           IFEQ      F10
100200970526     C                   MOVEL     '10'          FUNKEY
100300970526     C                   END
100400970526     C     KEY           IFEQ      F11
100500970526     C                   MOVEL     '11'          FUNKEY
100600970526     C                   END
100700970526     C     KEY           IFEQ      F12
100800970526     C                   MOVEL     '12'          FUNKEY
100900970526     C                   END
101000970526     C     KEY           IFEQ      F13
101100970526     C                   MOVEL     '13'          FUNKEY
101200970526     C                   END
101300970526     C     KEY           IFEQ      F14
101400970526     C                   MOVEL     '14'          FUNKEY
101500970526     C                   END
101600970526     C     KEY           IFEQ      F15
101700970526     C                   MOVEL     '15'          FUNKEY
101800970526     C                   END
101900970526     C     KEY           IFEQ      F16
102000970526     C                   MOVEL     '16'          FUNKEY
102100970526     C                   END
102200970526     C     KEY           IFEQ      F17
102300970526     C                   MOVEL     '17'          FUNKEY
102400970526     C                   END
102500970526     C     KEY           IFEQ      F18
102600970526     C                   MOVEL     '18'          FUNKEY
102700970526     C                   END
102800970526     C     KEY           IFEQ      F19
102900970526     C                   MOVEL     '19'          FUNKEY
103000970526     C                   END
103100970526     C     KEY           IFEQ      F20
103200970526     C                   MOVEL     '20'          FUNKEY
103300970526     C                   END
103400970526     C     KEY           IFEQ      F21
103500970526     C                   MOVEL     '21'          FUNKEY
103600970526     C                   END
103700970526     C     KEY           IFEQ      F22
103800970526     C                   MOVEL     '22'          FUNKEY
103900970526     C                   END
104000970526     C     KEY           IFEQ      F23
104100970526     C                   MOVEL     '23'          FUNKEY
104200970526     C                   END
104300970526     C     KEY           IFEQ      F24
104400970526     C                   MOVEL     '24'          FUNKEY
104500970526     C                   END
104600970526     C     KEY           IFEQ      ROLLUP
104700970526     C                   MOVEL     'RU'          FUNKEY
104800970526     C                   END
104900970526     C     KEY           IFEQ      HELP
105000970526     C                   MOVEL     'HP'          FUNKEY
105100970526     C                   END
105200970526     C*
105300970526     C                   ENDSR
105400970526     C*------------------------------------------------------------------------*
105500970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
105600970526     C*------------------------------------------------------------------------*
105700970526     C     FUNGES        BEGSR
105800970526     C*
105900970526     C* PULISCO CAMPI DI LAVORO
106000970526     C                   Z-ADD     0             W                 5 0
106100970526     C*
106200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
106300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
106400970526     C     1             DO        24            I
106500970526     C                   MOVEL     FUD(I)        FUDRIG
106600970526     C*
106700970526     C* PULIZIA FUNZIONI PRECEDENTI
106800970526     C                   MOVEL     *BLANKS       FUV(I)
106900970526     C*
107000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
107100970526     C* (SALTA F23 E F24)
107200970526     C     FUDATT        IFEQ      'S'
107300970526     C     FUDFUN        ANDNE     '23'
107400970526     C     FUDFUN        ANDNE     '24'
107500970526     C                   ADD       1             W
107600970526     C                   MOVEL     FUDDES        FUV(W)
107700970526     C                   END
107800970526     C                   END                                                    DO WHILE
107900970526     C*
108000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
108100970526     C* ALTRIMENTI DISATTIVO F24
108200970526     C     1             DO        24            I
108300970526     C                   MOVEL     FUD(I)        FUDRIG
108400970526     C*
108500970526     C     FUDFUN        IFEQ      '24'
108600970526     C     W             IFGT      6
108700970526     C                   MOVEL     'S'           FUDATT
108800970526     C                   ELSE
108900970526     C                   MOVEL     *BLANKS       FUDATT
109000970526     C                   END                                                    W GT 8
109100970526     C                   MOVEL     FUDRIG        FUD(I)
109200970526     C                   END
109300970526     C                   END                                                    DO WHILE
109400970526     C*
109500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
109600970526     C     W             DIV       6             I
109700970526     C                   MVR                     Z
109800970526     C*
109900970526     C     Z             IFNE      0
110000970526     C     I             ADD       1             Z
110100970526     C                   ELSE
110200970526     C                   Z-ADD     I             Z
110300970526     C                   END
110400970526     C*
110500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
110600970526     C                   ADD       1             FUNGRU
110700970526     C     FUNGRU        IFGT      Z
110800970526     C     FUNGRU        ORLT      1
110900970526     C                   Z-ADD     1             FUNGRU
111000970526     C                   END
111100970526     C*
111200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
111300970526     C     FUNGRU        IFEQ      1
111400970526     C                   MOVEL     FUV(1)        FUN001
111500970526     C                   MOVEL     FUV(2)        FUN002
111600970526     C                   MOVEL     FUV(3)        FUN003
111700970526     C                   MOVEL     FUV(4)        FUN004
111800970526     C                   MOVEL     FUV(5)        FUN005
111900970526     C                   MOVEL     FUV(6)        FUN006
112000970526     C                   END
112100970526     C*
112200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
112300970526     C     FUNGRU        IFEQ      2
112400970526     C                   MOVEL     FUV(7)        FUN001
112500970526     C                   MOVEL     FUV(8)        FUN002
112600970526     C                   MOVEL     FUV(9)        FUN003
112700970526     C                   MOVEL     FUV(10)       FUN004
112800970526     C                   MOVEL     FUV(11)       FUN005
112900970526     C                   MOVEL     FUV(12)       FUN006
113000970526     C                   END
113100970526     C*
113200970526     C* TERZO GRUPPO DI 6 FUNZIONI
113300970526     C     FUNGRU        IFEQ      3
113400970526     C                   MOVEL     FUV(13)       FUN001
113500970526     C                   MOVEL     FUV(14)       FUN002
113600970526     C                   MOVEL     FUV(15)       FUN003
113700970526     C                   MOVEL     FUV(16)       FUN004
113800970526     C                   MOVEL     FUV(17)       FUN005
113900970526     C                   MOVEL     FUV(18)       FUN006
114000970526     C                   END
114100970526     C*
114200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
114300970526     C     FUNGRU        IFEQ      4
114400970526     C                   MOVEL     FUV(19)       FUN001
114500970526     C                   MOVEL     FUV(20)       FUN002
114600970526     C                   MOVEL     FUV(21)       FUN003
114700970526     C                   MOVEL     FUV(22)       FUN004
114800970526     C                   MOVEL     *BLANKS       FUN005
114900970526     C                   MOVEL     *BLANKS       FUN006
115000970526     C                   END
115100970526     C*
115200970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
115300970526     C                   MOVEL     *BLANKS       FUN007
115400970526     C                   MOVEL     *BLANKS       FUN008
115500970526     C*
115600970526     C     1             DO        24            I
115700970526     C                   MOVEL     FUD(I)        FUDRIG
115800970526     C     FUDFUN        IFEQ      '23'
115900970526     C     FUDATT        ANDEQ     'S'
116000970526     C                   MOVEL     FUDDES        FUN007
116100970526     C                   END
116200970526     C     FUDFUN        IFEQ      '24'
116300970526     C     FUDATT        ANDEQ     'S'
116400970526     C                   MOVEL     FUDDES        FUN008
116500970526     C                   END
116600970526     C                   END                                                    DO
116700970526     C*
116800970526     C                   ENDSR
116900970526     C*------------------------------------------------------------------------*
117000970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
117100970526     C*------------------------------------------------------------------------*
117200970526     C     OPZGES        BEGSR
117300970526     C*
117400970526     C* PULISCO CAMPI DI LAVORO
117500970526     C                   Z-ADD     0             W                 5 0
117600970526     C*
117700970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
117800970526     C* E CARICO TABELLA OPZIONI ATTIVE
117900991115     C     1             DO        24            I
118000970526     C                   MOVEL     OPD(I)        OPDRIG
118100970526     C*
118200970526     C* PULIZIA OPZIONI PRECEDENTI
118300970526     C                   MOVEL     *BLANKS       OPV(I)
118400970526     C*
118500970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
118600970526     C     OPDATT        IFEQ      'S'
118700970526     C                   ADD       1             W
118800970526     C                   MOVEL     OPDDES        OPV(W)
118900970526     C                   END
119000970526     C                   END                                                    DO WHILE
119100970526     C*
119200970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
119300970526     C* ALTRIMENTI DISATTIVO F23
119400991115     C     1             DO        24            I
119500970526     C                   MOVEL     FUD(I)        FUDRIG
119600970526     C*
119700970526     C     FUDFUN        IFEQ      '23'
119800970526     C     W             IFGT      8
119900970526     C                   MOVEL     'S'           FUDATT
120000970526     C                   ELSE
120100970526     C                   MOVEL     *BLANKS       FUDATT
120200970526     C                   END                                                    W GT 8
120300970526     C                   MOVEL     FUDRIG        FUD(I)
120400970526     C                   END
120500970526     C                   END                                                    DO WHILE
120600970526     C*
120700970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
120800970526     C     W             DIV       8             I
120900970526     C                   MVR                     Z                 5 0
121000970526     C*
121100970526     C     Z             IFNE      0
121200970526     C     I             ADD       1             Z
121300970526     C                   ELSE
121400970526     C                   Z-ADD     I             Z
121500970526     C                   END
121600970526     C*
121700970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
121800970526     C                   ADD       1             OPZGRU
121900970526     C     OPZGRU        IFGT      Z
122000970526     C     OPZGRU        ORLT      1
122100970526     C                   Z-ADD     1             OPZGRU
122200970526     C                   END
122300970526     C*
122400970526     C* PRIMO GRUPPO DI 8 OPZIONI
122500970526     C     OPZGRU        IFEQ      1
122600970526     C                   MOVEL     OPV(1)        OPZ001
122700970526     C                   MOVEL     OPV(2)        OPZ002
122800970526     C                   MOVEL     OPV(3)        OPZ003
122900970526     C                   MOVEL     OPV(4)        OPZ004
123000970526     C                   MOVEL     OPV(5)        OPZ005
123100970526     C                   MOVEL     OPV(6)        OPZ006
123200970526     C                   MOVEL     OPV(7)        OPZ007
123300970526     C                   MOVEL     OPV(8)        OPZ008
123400970526     C                   END
123500970526     C*
123600970526     C* SECONDO GRUPPO DI 8 OPZIONI
123700970526     C     OPZGRU        IFEQ      2
123800970526     C                   MOVEL     OPV(9)        OPZ001
123900970526     C                   MOVEL     OPV(10)       OPZ002
124000970526     C                   MOVEL     OPV(11)       OPZ003
124100970526     C                   MOVEL     OPV(12)       OPZ004
124200970526     C                   MOVEL     OPV(13)       OPZ005
124300970526     C                   MOVEL     OPV(14)       OPZ006
124400970526     C                   MOVEL     OPV(15)       OPZ007
124500970526     C                   MOVEL     OPV(16)       OPZ008
124600970526     C                   END
124700970526     C*
124800970526     C* TERZO GRUPPO DI 8 OPZIONI
124900970526     C     OPZGRU        IFEQ      3
125000970526     C                   MOVEL     OPV(17)       OPZ001
125100970526     C                   MOVEL     OPV(18)       OPZ002
125200970526     C                   MOVEL     OPV(19)       OPZ003
125300970526     C                   MOVEL     OPV(20)       OPZ004
125400970526     C                   MOVEL     OPV(21)       OPZ005
125500970526     C                   MOVEL     OPV(22)       OPZ006
125600970526     C                   MOVEL     OPV(23)       OPZ007
125700970526     C                   MOVEL     OPV(24)       OPZ008
125800970526     C                   END
125900970526     C*
126000970526     C                   ENDSR
126100970526     C*------------------------------------------------------------------------*
126200970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
126300970526     C*------------------------------------------------------------------------*
126400970526     C     OPZCON        BEGSR
126500970526     C*
126600970526     C* PULISCO CAMPI DI LAVORO
126700970526     C                   Z-ADD     0             I                 5 0
126800970526     C                   Z-ADD     0             W                 5 0
126900970526     C*
127000970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
127100970526     C     1             DO        24            I
127200970526     C                   MOVEL     OPD(I)        OPDRIG
127300970526     C*
127400970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
127500970526     C     OPZKEY        IFEQ      OPDOPZ
127600970526     C     OPZKEY        OREQ      OPDOP1
127700970526     C*
127800970526     C     OPDATT        IFEQ      'S'
127900970526     C                   MOVEL     OPDOPZ        OPZKEY
128000970526     C                   Z-ADD     I             W
128100970526     C                   END
128200970526     C                   END
128300970526     C*
128400970526     C                   END                                                    DO WHILE
128500970526     C*
128600970526     C* OPZIONE NON ATTIVA (ERRORE)
128700970526     C     W             IFEQ      0
128800970526     C*
128900970526     C* OPZIONE NON OK
129000970526     C                   Z-ADD     1             OPZOK
129100970526     C*
129200970526     C* EMISSIONE MESSAGGIO DI ERRORE
129300970526     C                   MOVEL     MSG(1)        DSMSMS
129400970526     C                   ELSE
129500970526     C*
129600970526     C* OPZIONE ATTIVA (PROCEDI ...)
129700970526     C*
129800021211     C                   MOVEL     *BLANKS       PMSG
129900000420     C     OPZKEY        IFEQ      ' 1'
130000000420     C                   EXSR      O01GES
130100000420     C                   END
130200000407     C     OPZKEY        IFEQ      ' 2'
130300000407     C                   EXSR      O02GES
130400000407     C                   END
130500021211     C     OPZKEY        IFEQ      ' 3'
130600021211     C                   EXSR      O03GES
130700021211     C                   END
130800991110     C     OPZKEY        IFEQ      ' 4'
130900991110     C                   EXSR      O04GES
131000991014     C                   END
131100970526     C     OPZKEY        IFEQ      ' 5'
131200970526     C                   EXSR      O05GES
131300970526     C                   END
131400030123     C                   MOVEL     PMSG          DSMSMS
131500021211     C                   END                                                    OPZ.NON ATTIVA
131600970526     C*
131700970526     C                   ENDSR
131800970526     C*------------------------------------------------------------------------*
131900970526     C* NOTE COSTRUZIONE SCHIERE
132000970526     C*------------------------------------------------------------------------*
132100970526     C*
132200970526     C* -MSG-MESSAGGI
132300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
132400970526     C* (NON TOCCARE MSG,1 E MSG,2)
132500970526     C*
132600970526     C* -CMD-COMANDI
132700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
132800970526     C*
132900970526     C* -OPD-OPZIONI
133000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
133100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
133200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
133300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
133400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
133500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
133600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
133700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
133800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
133900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
134000970526     C*
134100970526     C* -FUD-FUNZIONI
134200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
134300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
134400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
134500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
134600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
134700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
134800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
134900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
135000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
135100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
135200991108     C*
135300991115     C* -STS-CODICI STATI DEI RECORD DI LOG
135400991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
135500991110     C* ================> IMPORTANTE <=================
135600991115     C* per codici di errore in -STS- indicare in -DES- sempre
135700991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
135800991110     C* ERR
135900991108     C*
136000970526     O*------------------------------------------------------------------------*
136100970526** DEC - DECODIFICHE
136200970526    GESTIONE                                                     1
136300000407 INTERROGAZIONE                                                  2
136400970530     SCELTA                                                      3
1365009705264
1366009705265
136700970526** MSG - MESSAGGI
136800021211TISIG1R- Opzione non gestita.                                                  1
136900021211TISIG1R- Tasto funzionale non gestito.                                         2
137000030107TISIG1R- Utente non abilitato all'inserimento.                                 3
137100021211TISIG1R- Impossibile annullare, presenti movimenti su tale codice articolo.    4
137200021211TISIG1R- 5                                                                     5
137300021211TISIG1R- 6                                                                     6
137400021211TISIG1R- 7                                                                     7
137500021211TISIG1R- 8                                                                     8
137600021211TISIG1R- 9                                                                     9
137700021211TISIG1R- 10                                                                   10
137800021211TISIG1R- 11                                                                   11
137900021211TISIG1R- 12                                                                   12
138000021211TISIG1R- 13                                                                   13
138100021211TISIG1R- 14                                                                   14
138200021211TISIG1R- 15                                                                   15
138300021211TISIG1R- 16                                                                   16
138400021211TISIG1R- 17                                                                   17
138500021211TISIG1R- 18                                                                   18
138600021211TISIG1R- 19                                                                   19
138700021211TISIG1R- 20                                                                   20
138800970528** CMD - COMANDI CL
138900021211
139000021211
139100991110
139200970528
139300970528
139400970526** OPD - OPZIONI
139500000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
139600000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
139700021211 3=Copia           -3 -     (S/N)=N                        opz. 03¬
139800000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
139900000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
140000991115                   -  -     (S/N)=N                        opz. 06¬
140100021211                   -  -     (S/N)=N                        opz. 07¬
140200991110                   -  -     (S/N)=N                        opz. 08¬
140300991110                   -  -     (S/N)=N                        opz. 09¬
140400991110                   -  -     (S/N)=N                        opz. 10¬
140500000407                   -  -     (S/N)=N                        opz. 11¬
140600970526                   -  -     (S/N)=N                        opz. 12¬
140700970526                   -  -     (S/N)=N                        opz. 13¬
140800000407                   -  -     (S/N)=N                        opz. 14¬
140900000407                   -  -     (S/N)=N                        opz. 15¬
141000970526                   -  -     (S/N)=N                        opz. 16¬
141100970526                   -  -     (S/N)=N                        opz. 17¬
141200970526                   -  -     (S/N)=N                        opz. 18¬
141300970526                   -  -     (S/N)=N                        opz. 19¬
141400970526                   -  -     (S/N)=N                        opz. 20¬
141500970526                   -  -     (S/N)=N                        opz. 21¬
141600970526                   -  -     (S/N)=N                        opz. 22¬
141700970526                   -  -     (S/N)=N                        opz. 23¬
141800000407                   -  -     (S/N)=N                        opz. 25¬
141900970526** FUD - FUNZIONI
142000970526                   -        (S/N)=N                        funz.01¬
142100970526                   -        (S/N)=N                        funz.02¬
142200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
142300970526                   -        (S/N)=N                        funz.04¬
142400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
142500000407F06=Inserimento    -        (S/N)=N                        funz.06¬
142600970526                   -        (S/N)=N                        funz.07¬
142700970527                   -        (S/N)=N                        funz.08¬
142800970527                   -        (S/N)=N                        funz.09¬
142900970527                   -        (S/N)=N                        funz.10¬
143000970526                   -        (S/N)=N                        funz.11¬
143100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
143200991116                   -        (S/N)=N                        funz.13¬
143300970526                   -        (S/N)=N                        funz.14¬
143400970526                   -        (S/N)=N                        funz.15¬
143500970526                   -        (S/N)=N                        funz.16¬
143600970526                   -        (S/N)=N                        funz.17¬
143700970526                   -        (S/N)=N                        funz.18¬
143800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
143900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
144000000407                   -        (S/N)=N                        funz.21¬
144100991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
144200991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
144300991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
