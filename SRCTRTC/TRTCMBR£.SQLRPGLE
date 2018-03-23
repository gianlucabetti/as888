000100970526     H*------------------------------------------------------------------------*
000200141211     H* Mostra stato invio e-mail per Progressivo
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800141210     FTRTCMBD   CF   E             WORKSTN INFDS(DEVDS1)
000900141210     F                                     SFILE(TCMBS03:SFLULT)
001000141210     F                                     SFILE(TCMBDMS:MSGULT)
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400970526     D*------------
001500970526     D* DECODIFICHE
001600970526     D*------------
001700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* MESSAGGI
002000970526     D*------------
002100970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200970528     D*------------
002300970528     D* COMANDI
002400970528     D*------------
002500970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* DESCRIZIONI OPZIONI
002800970526     D*------------
002900970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI TASTI FUNZIONALI
003200970526     D*------------
003300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* OPZIONI DA CARICARE A VIDEO
003600970526     D*------------
003700970526     D OPV             S             19    DIM(24)
003800970526     D*------------
003900970526     D* FUNZIONI DA CARICARE A VIDEO
004000970526     D*------------
004100970527     D FUV             S             19    DIM(24)
004200991108     D*------------
004300970526     D*------------------------------------------------------------------------*
004400970526     D* INPUT
004500970526     D*------------------------------------------------------------------------*
004600970526     D*------------
004700970526     D* RIDEFINIZIONE OPZIONI VIDEO
004800970526     D*------------
004900970526     D OPDDS           DS
005000970526     D  OPDOPZ                 1      2
005100970526     D  OPDDE1                 3     19
005200970526     D  OPDOP1                21     22
005300970526     D  OPDFI0                23     34
005400970526     D  OPDATT                35     35
005500970526     D  OPDFI1                36     80
005600970526     D  OPDDES                 1     19
005700970526     D  OPDRIG                 1     80
005800970526     D*------------
005900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006000970526     D*------------
006100970526     D FUDDS           DS
006200970526     D  FUDDE1                 1      1
006300970526     D  FUDFUN                 2      3
006400970526     D  FUDDE2                 4     19
006500970526     D  FUDFI0                20     34
006600970526     D  FUDATT                35     35
006700970526     D  FUDFI1                36     80
006800970526     D  FUDDES                 1     19
006900970526     D  FUDRIG                 1     80
007000970526     D*------------
007100970526     D* DS GESTIONE TASTI FUNZIONALI
007200970526     D*------------
007300970526     D DSKEY         E DS
007400970526     D DEVDS1          DS
007500970526     D  KEY                  369    369
007600970526     D*------------
007700970526     D* DS "XMSG" - GESTIONE MESSAGGI
007800970526     D*------------
007900970526     D DSMSG         E DS
008000970526     D*------------
008100020917     D* DS REPERIMENTO DATI UTENTE
008200970526     D*------------
008300020917     D TIBS34DS      E DS                                                       *Profili utente
008400020917     D DDATIUTE      E DS                                                       *Dati utente
008500020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008600970526     D*------------
008700970526     D* ARCHITETTURA
008800970526     D*------------
008900970526     D KPJBA         E DS
009000120510     D Sav_kpjba       s                   like(kpjba)
009100050224     D*------------------
009200050224     D* DS D PROCEDURA X REPERIMENTO/IMPOSTAZIONE STATO PROGRESSIVO LOG
009300050224     D*------------------
009400050224     D TRTCM9DS      E DS
009500030128     D*------------------
009600030128     D* DS CONTROLLO DATA (8)
009700030128     D*------------------
009800030128     D WLBDA8          DS                  INZ
009900030128     D  G08DAT                 1      8  0
010000030128     D  G08INV                 9     16  0
010100030128     D  G08ERR                17     17
010200030128     D  G08TGI                18     22  0
010300021211     D*------------------
010400030509     D* PASSAGGIO PARAMETRI
010500021211     D*------------------
010600021211     D PARAM           DS                  INZ
010700141211     D  POP1                          3
010800141211     D  POP2                          1
010900141211     D  pIn_Prog                      7
011000141211     D  pIn_Mit                      40
011100141211     D  pOut_Msg                     80
011200050216     D*------------------
011300050216     D* VARIABILI D WRK
011400050216     D*------------------
011500050808     D wDataDA         S               D   DATFMT(*iso)
011600141211     d wSQL            s           2048    inz  varying
011700141211     D TIMAT00F      E DS
011800141211     D RcdTIMAT        DS
011900141211     D  wMATTO                             like(MATTO)
012000141211     D  wMATSTS                            like(MATSTS)
012100141211     D  wMATMSG                            like(MATMSG)
012200141211     D  wMATMOMINS                         like(MATMOMINS)
012300141211     D  wMATMOMSTS                         like(MATMOMSTS)
012400050216     D*------------------
012500050216     D* COSTANTI
012600050216     D*------------------
012700050216     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
012800050216     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
012900970526     C*------------------------------------------------------------------------*
013000970526     C* MAIN LINES
013100970526     C*------------------------------------------------------------------------*
013200100202
013300100202
013400100202     C*
013500100202     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013600100202     C
013700100202     C/EXEC SQL
013800100202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013900100202     C/END-EXEC
014000100202     C*
014100970526     C*
014200970526     C* SE NON CI SONO ERRORI, PROSEGUE
014300970526IF  1C     ERRL00        IFNE      '1'
014400970526     C*
014500970526     C* CARICA IL SFILE
014600970526     C                   EXSR      POSSFL
014700970526     C*
014800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015200970526     C*
015300970526     C* REIMPOSTA ERRORE DI PROCERURA
015400970526     C                   MOVEL     '0'           ERRL00
015500970526     C*
015600050114     C* EMETTE IL SFILE
015700970526     C                   EXSR      OUTSFL
015800970526     C*
015900970526     C* GESTIONE TASTI FUNZIONALI
016000970526     C                   EXSR      FUNCON
016100970526     C*
016200970526E   2C                   ENDDO
016300970527     C*
016400970526E   1C                   ENDIF
016500991108     C*
016600970526     C* FINE PGM
016700970526     C                   SETON                                        LR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017000970526     C*------------------------------------------------------------------------*
017100970526     C     F03GES        BEGSR
017200970526     C*
017300970526     C* IMPOSTA "PREMUTO F03"
017400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800120510     C* F04GES - GESTIONE F04 -> Delete log con prg=0
017900970526     C*------------------------------------------------------------------------*
018000120510     C     F04GES        BEGSR
018100970526     C*
018200120510     C* salvo kpjba prima di passarla
018300120510     C                   MOVEL     KPJBA         Sav_kpjba
018400120510     C* chiamata a pgm apposito
018500120510     C* (ha il video che chiede i parametri e non gliene servono altri)
018600120510     C                   CALL      'TRTCMAR'
018700120510     C                   PARM                    KPJBA
018800120510     C* ripristino kpjba
018900120510     C                   EVAL      KPJBA =       Sav_kpjba
019000970526     C*
019100970526     C                   ENDSR
019200120510     C*------------------------------------------------------------------------*
019300120510     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
019400120510     C*------------------------------------------------------------------------*
019500120510     C     F05GES        BEGSR
019600120510     C*
019700120510     C                   EXSR      POSSFL                                       *RICARICA SFILE
019800120510     C*
019900120510     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100030509     C* F12GES - GESTIONE F12 -> RITORNO
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F12GES        BEGSR
020400970526     C*
020500970526     C* IMPOSTA "PREMUTO F12"
020600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020700970526     C*
020800970526     C                   ENDSR
020900970528     C*------------------------------------------------------------------------*
021000970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021100970528     C*------------------------------------------------------------------------*
021200970528     C     F21GES        BEGSR
021300970612     C*
021400991117     C* NON GESTITA
021500970528     C*
021600970528     C                   ENDSR
021700970528     C*------------------------------------------------------------------------*
021800970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
021900970528     C*------------------------------------------------------------------------*
022000970528     C     F22GES        BEGSR
022100970528     C*
022200021211     C* NON GESTITA
022300970528     C*
022400970528     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F23GES        BEGSR
022900970526     C*
023000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023500970526     C*------------------------------------------------------------------------*
023600970526     C     F24GES        BEGSR
023700970526     C*
023800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024300970526     C*------------------------------------------------------------------------*
024400970526     C     RUPGES        BEGSR
024500970526     C*
024600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024700970526     C*
024800970526     C                   ENDSR
024900970526     C*------------------------------------------------------------------------*
025000991108     C* ENTGES - GESTIONE ENTER -> INVIO
025100970526     C*------------------------------------------------------------------------*
025200970526     C     ENTGES        BEGSR
025300000407     C*
025400000407     C                   MOVEL     '0'           ERRL00
025500991108     C*
025600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025700970526     C                   EXSR      NEWPOS
025800021211     C*
025900021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026000021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026100021211     C     SWPOSI        IFNE      0
026200021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
026300021211     C                   ELSE
026400021211     C     SFLULT        IFNE      0
026500021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026600021211     C*
026700021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026800021211     C     SWMODI        IFNE      0
026900021211     C                   EXSR      POSSFL
027000021211     C                   ENDIF
027100021211     C                   ENDIF
027200021211     C                   ENDIF
027300021211     C                   ENDIF
027400021211     C*
027500021211     C                   ENDSR
027600030509     C*------------------------------------------------------------------------*
027700030509     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
027800030509     C*------------------------------------------------------------------------*
027900030509     C     O02GES        BEGSR
028000030509     C*
028100030509     C*
028200030509     C                   ENDSR
028300991028     C*------------------------------------------------------------------------*
028400991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
028500991112     C*------------------------------------------------------------------------*
028600991112     C     PASPAR        BEGSR
028700991112     C*
028800000407     C                   MOVEL     PARAM         KPJBU
028900991112     C*
029000991112     C                   ENDSR
029100970526     C*------------------------------------------------------------------------*
029200970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
029300970526     C*------------------------------------------------------------------------*
029400970526     C     NEWPOS        BEGSR
029500970526     C*
029600970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
029700970526     C*         1= RICHIESTO POSIZIONAMENTO
029800970526     C                   Z-ADD     0             SWPOSI            7 0
029900970526     C*
030000991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
030100970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
030200970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
030300970526     C                   Z-ADD     0             SWMODI            1 0
030400970530     C*
030500970530     C* REIMPOSTA INDICATORI DI ERRORE
030600050111     C                   MOVEL     '0'           ERRPAR            1
030700970526     C*
030800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
030900030305     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
031000050111     C*
031100970526     C                   ENDSR
031200970526     C*------------------------------------------------------------------------*
031300021217     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
031400970526     C*------------------------------------------------------------------------*
031500970528     C     SETFIL        BEGSR
031600050808     C*
031700141211     c                   EVAL      wSQL =
031800141211     c                             'select +
031900141211     c                              MATTO, +
032000141211     c                              MATSTS, +
032100141211     c                              MATMSG, +
032200141211     c                              MATMOMINS, +
032300141211     c                              MATMOMSTS +
032400141211     c                              from timat00f +
032500141211     c                              where +
032600141211     c                              MATTRC = ''BM1'' and +
032700141211     c                              MATPROG = ''' +
032800141211     c                              D02Prog + ''''
032900141211      *
033000141211     C/EXEC SQL
033100141211     C+ PREPARE S1 FROM :wSQL
033200141211     C/END-EXEC
033300141211
033400141211     C/EXEC SQL
033500141211     C+ DECLARE A1 CURSOR FOR S1
033600141211     C/END-EXEC
033700141211
033800141211     C/EXEC SQL
033900141211     C+ OPEN A1
034000141211     C/END-EXEC
034100980107     C*
034200021211     C* LETTURA RECORD
034300970526     C                   EXSR      LETFIL
034400970526     C*
034500970526     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* LETFIL - LETTURA RECORD
034800970526     C*------------------------------------------------------------------------*
034900970526     C     LETFIL        BEGSR
035000970526     C*
035100141211     C* leggo finchè non trovo un rcd valido
035200141211     C* oppure sono finiti i rcd
035300141211     C                   MOVEL     'N'           WRECOK
035400141211     C                   DOU       SQLCod >= 0 and SQLCod <> 100 and
035500141211     C                             wRecOK = 'S'
035600141211     C                             OR
035700141211     C                             SFLEOF = 1
035800970526     C*
035900141211     C/EXEC SQL
036000141211     C+ FETCH NEXT FROM A1 INTO :RcdTIMAT
036100141211     C/END-EXEC
036200970528     C*
036300970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
036400141211     C                   IF        SQLCod < 0    or                             *ERRORE BLOCCANTE
036500141211     C                             SQLCod = 100                                 *FINE FILE
036600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
036700021211     C                   ELSE
036800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
036900021211     C                   ENDIF
037000021211     C                   ENDDO
037100970526     C*
037200970526     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400021217     C* CHKREC - CONTROLLA VALIDITA' RECORD
037500970526     C*------------------------------------------------------------------------*
037600970526     C     CHKREC        BEGSR
037700970526     C*
037800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
037900991108     C*
038000970526     C                   ENDSR
038100970526     C*------------------------------------------------------------------------*
038200000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
038300970526     C*------------------------------------------------------------------------*
038400970526     C     IRESFL        BEGSR
038500970526     C*
038600141211     C                   MOVEL     wMATTo        VIDTo
038700141211     C                   MOVEL     wMATSts       VIDSts
038800141211     C                   MOVEL     wMATMsg       VIDMsg
038900141211     C                   EVAL      VIDMomIns = wMATMomIns
039000141211     C                   EVAL      VIDMomSts = wMATMomSts
039100040818     C*
039200970526     C                   ENDSR
039300970526     C*------------------------------------------------------------------------*
039400991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
039500970526     C*------------------------------------------------------------------------*
039600970526     C     CHKPAR        BEGSR
039700021218     C*
039800021218     C* INIZIALIZZO FLAG GENERICO DI ERRORE SELEZIONI
039900030509     C                   MOVEL     '0'           W_ERR             1
040000000407     C*
040100970526     C                   ENDSR
040200970526     C*------------------------------------------------------------------------*
040300970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
040400970526     C*------------------------------------------------------------------------*
040500970526     C     CHKFOP        BEGSR
040600991112     C*
040700991112     C* OPZIONI
040800970626     C*
040900141210     C* non ci sono opzio perché il video è di sola visualizzazione, ma tutto rimane
041000141210     C* impostato come se fosse possibile averle
041100141210     C*
041200000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
041300021211     C     DEPOP0        IFEQ      'P01'
041400000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
041500000420     C                   MOVEL     'S'           OPDATT
041600000420     C                   MOVEL     OPDRIG        OPD(1)
041700021211     C                   ELSE
041800000420     C* OPZIONI
041900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
042000970626     C                   MOVEL     'S'           OPDATT
042100970626     C                   MOVEL     OPDRIG        OPD(5)
042200141210     C                   MOVEL     OPD(10)       OPDRIG                         *VISUAL.STATUS INVIO
042300141210     C                   MOVEL     'S'           OPDATT
042400141210     C                   MOVEL     OPDRIG        OPD(10)
042500021211     C                   ENDIF
042600970626     C*
042700000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
042800021211     C     DEPOP0        IFEQ      'P02'
042900991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
043000991112     C                   MOVEL     'S'           OPDATT
043100991112     C                   MOVEL     OPDRIG        OPD(5)
043200050111     C                   MOVEL     OPD(15)       OPDRIG                         *VISUALIZZA SPOOL
043300050111     C                   MOVEL     'S'           OPDATT
043400050111     C                   MOVEL     OPDRIG        OPD(15)
043500030509     C                   IF        %subst(KNMUS:1:3) = 'EDP'
043600030509     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
043700030509     C                   MOVEL     'S'           OPDATT
043800030509     C                   MOVEL     OPDRIG        OPD(2)
043900031210     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
044000031210     C                   MOVEL     'S'           OPDATT
044100031210     C                   MOVEL     OPDRIG        OPD(4)
044200031210     C                   MOVEL     OPD(6)        OPDRIG                         *RILASCIO
044300031210     C                   MOVEL     'S'           OPDATT
044400031210     C                   MOVEL     OPDRIG        OPD(6)
044500071001     C                   MOVEL     OPD(11)       OPDRIG                         *RIESECUZIONE SPOOL
044600071001     C                   MOVEL     'S'           OPDATT
044700071001     C                   MOVEL     OPDRIG        OPD(11)
044800120510     C                   MOVEL     FUD(04)       FUDRIG                         *DLT LOG CON PRG=0
044900120510     C                   MOVEL     'S'           FUDATT
045000120510     C                   MOVEL     FUDRIG        FUD(04)
045100021211     C                   ENDIF
045200030509     C                   ENDIF
045300970526     C*
045400970526     C* GESTIONE OPZIONI A VIDEO
045500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
045600970526     C                   EXSR      OPZGES
045700970526     C*
045800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
045900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
046000970526     C                   EXSR      FUNGES
046100970526     C*
046200970526     C                   ENDSR
046300020917     C*--------------------------------------------------------------------------------------------*
046400020917     C* REPERISCE I DATI UTENTE
046500020917     C*--------------------------------------------------------------------------------------------*
046600020917     C     REPDATIUTE    BEGSR
046700020917     C*
046800020917     C* INIZIALIZZA VARIABILI DI WRK
046900020917     C                   CLEAR                   TIBS34DS
047000020917     C                   CLEAR                   AZUTEDS
047100020917     C                   CLEAR                   DDATIUTE
047200020917     C*
047300020917     C     *DTAARA       DEFINE    §azute        azuteds
047400020917     C     *DTAARA       DEFINE    §datiute      ddatiute
047500020917     C                   IN(E)     *DTAARA
047600021211     C                   IF        %Error
047700021211     C                   EVAL      I34Tla = 'L'
047800020917     C                   CALL      'TIBS34R'
047900020917     C                   PARM                    Tibs34Ds
048000020917     C                   IN        *DTAARA
048100021211     C                   ENDIF
048200020917     C*
048300020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
048400020917     C                   MOVEL(P)  RSUT          DSFIRS           20
048500020917     C*
048600020917     C                   ENDSR
048700970526     C*------------------------------------------------------------------------*
048800970526     C* *INZSR - OPERAZIONI INIZIALI
048900970526     C*------------------------------------------------------------------------*
049000970526     C     *INZSR        BEGSR
049100970526     C*---
049200970526     C* RICEVIMENTO PARAMETRI
049300970526     C*---
049400970526     C     *ENTRY        PLIST
049500141211     C                   PARM                    Kpjba
049600141211
049700141211     C                   EVAL      Param = Kpjbu
049800141211     C                   EVAL      D02Prog = pIn_Prog
049900141211     C                   EVAL      D02Mit  = pIn_Mit
050000141210     C                   MOVEL     *blank        DEPOP0            3
050100050110     C*
050200050110     C* REPERIMENTO DATI UTENTE
050300050110     C                   EXSR      REPDATIUTE
050400050808     C*
050500050808     C* CALCOLA LA DATA CORRENTE
050600050808     C                   time                    wn14             14 0
050700050808     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
050800050808     C                   z-add     wn8           g08dat
050900050808     C                   z-add     *zeros        g08inv
051000050808     C                   movel     '0'           g08err
051100050808     C                   call      'XSRDA8'
051200050808     C                   parm                    wlbda8
051300050808     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
051400030604     C*
051500030604     C* INIZIALIZZO I VALORI DI DEFAULT A VIDEO
051600970526     C*
051700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
051800970526     C     XMSGPA        PLIST
051900970526     C                   PARM                    DSMSMS
052000970526     C                   PARM                    DSMSPG
052100970526     C                   PARM                    DSMSKE
052200970526     C                   PARM                    DSMSER
052300970526     C*
052400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
052500970526     C                   CALL      'XKEY'
052600970526     C                   PARM                    DSKEY
052700970526     C*---
052800970526     C* VARIABILI NON RIFERITE AL DATA BASE
052900970526     C*---
053000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
053100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
053200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
053300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
053400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
053500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
053600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
053700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
053800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
053900970526     C*---
054000021217     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
054100991108     C*---
054200970526     C* VARIABILI DI CONTROLLO
054300970526     C*---
054400970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
054500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
054600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
054700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
054800970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
054900050110     C                   Z-ADD     8             SFLPAG                         *N°RIGHE X PAG
055000970526     C*
055100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
055200141210     C                   MOVEL     'TRTCMBR'     DSMSPG
055300141210     C                   MOVEL     'TRTCMBR'     MSGQUE
055400970526     C                   MOVEL     *BLANKS       MSGKEY
055500970526     C                   MOVEL     *BLANKS       DSMSMS
055600970526     C                   MOVEL     *BLANKS       DSMSKE
055700970526     C                   Z-ADD     *ZEROS        DSMSER
055800970526     C*---
055900970526     C* CHIAVI DI LETTURA
056000970526     C*---
056100970526     C*
056200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
056300970526     C                   EXSR      CHKPAR
056400991115     C*
056500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
056600970526     C                   EXSR      CHKFOP
056700970526     C*
056800970526     C                   ENDSR
056900970526     C*------------------------------------------------------------------------*
057000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057200970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
057300970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
057400970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
057500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057700970526     C*------------------------------------------------------------------------*
057800970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
057900970526     C*------------------------------------------------------------------------*
058000970526     C     POSSFL        BEGSR
058100970526     C*
058200970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
058300970526     C                   EXSR      CLRSFL
058400970526     C*
058500970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
058600970526     C                   EXSR      SETFIL
058700970526     C*
058800970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
058900970526     C     SFLEOF        IFEQ      0
059000970526     C                   EXSR      CARSFL
059100030305     C                   ENDIF
059200970526     C*
059300970526     C                   ENDSR
059400970526     C*------------------------------------------------------------------------*
059500970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
059600970526     C*------------------------------------------------------------------------*
059700970526     C     ENTATT        BEGSR
059800970526     C*
059900970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
060000970526     C                   Z-ADD     0             OPZOK             1 0
060100970526     C*
060200970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
060300970526     C                   Z-ADD     0             SFLVAR            5 0
060400970526     C*
060500970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
060600970526     C                   Z-ADD     0             SFLERR
060700970526     C*
060800970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
060900970526     C                   Z-ADD     SFLULT        WFLULT
061000970526     C*
061100970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
061200141210     C                   READC     TCMBS03                                98
061300970526     C     *IN98         DOWEQ     '0'
061400970526     C*
061500970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
061600970526     C                   ADD       1             SFLVAR
061700970526     C*
061800970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
061900970526     C     OPZOK         IFEQ      0
062000970526     C*
062100970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
062200970526     C     OPZKEY        IFNE      *BLANKS
062300970526     C                   EXSR      OPZCON
062400970526     C                   END
062500970526     C*
062600970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
062700970526     C*            2. PULIZIA SCELTA IN SFL
062800970526     C     OPZOK         IFEQ      0
062900970526     C                   MOVEL     '0'           *IN41
063000970526     C                   MOVEL     *BLANKS       OPZKEY
063100970526     C                   ELSE
063200970526     C*
063300970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
063400970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
063500970526     C                   MOVEL     '1'           *IN41
063600970526     C                   Z-ADD     SFLULT        SFLERR
063700970526     C                   END
063800970526     C                   ELSE
063900970526     C*
064000970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
064100970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
064200970526     C                   MOVEL     '1'           *IN41
064300970526     C                   END                                                    OPZOK=0
064400970526     C*
064500970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
064600970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
064700970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
064800970526     C     OPZOK         IFNE      0
064900970526     C                   Z-ADD     SFLERR        SFLNBR
065000970526     C                   ELSE
065100970526     C                   Z-ADD     SFLULT        SFLNBR
065200970526     C                   END
065300970526     C*
065400970526     C* AGGIORNO RECORD SFL
065500141211     C                   UPDATE    TCMBS03
065600970526     C*
065700141211     C                   READC     TCMBS03                                98
065800970526     C                   END                                                    DO WHILE
065900970526     C*
066000970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
066100970526     C                   Z-ADD     WFLULT        SFLULT
066200970526     C*
066300970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
066400970526     C     SFLVAR        IFEQ      0
066500970526     C                   END
066600991112     C*
066700970526     C                   ENDSR
066800970526     C*------------------------------------------------------------------------*
066900970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
067000970526     C*------------------------------------------------------------------------*
067100970526     C     CLRSFL        BEGSR
067200970526     C*
067300970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
067400970526     C* (0=NON FINE FILE  1=FINE FILE)
067500970526     C                   Z-ADD     0             SFLEOF
067600970526     C*
067700970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
067800970526     C                   Z-ADD     0             SFLULT
067900970526     C*
068000141211     C* pulizia SFL
068100141211     C                   SETOFF                                         3031
068200141210     C                   WRITE     TCMBD03
068300141211     C                   SETON                                          31
068400970526     C*
068500970526     C                   ENDSR
068600970526     C*------------------------------------------------------------------------*
068700970526     C* CARSFL - CARICAMENTO SFL
068800970526     C*------------------------------------------------------------------------*
068900970526     C     CARSFL        BEGSR
069000970526     C*
069100141211     C* queste istruzioni fanno fare una doppia SETFIL all'inizio e quindi una doppia lettura
069200970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
069300141211     C*    SFLULT        IFEQ      0
069400141211     C*                  EXSR      SETFIL
069500141211     C*                  END
069600970526     C*
069700970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
069800970526     C                   Z-ADD     SFLULT        WFLULT
069900970526     C*
070000970526     C* CICLO:
070100970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
070200970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
070300970526     C                   Z-ADD     1             SFLRIG
070400030509     C     SFLRIG        DOWLE     SFLPAG
070500030509     C     SFLEOF        ANDEQ     0
070600970526     C*
070700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
070800991116     C                   MOVEL     *BLANKS       OPZKEY
070900970526     C*
071000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
071100970526     C                   EXSR      IRESFL
071200970526     C*
071300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
071400970526     C                   ADD       1             SFLULT
071500970526     C*
071600970526     C* SCRITTURA RECORD SFL
071700141210     C                   WRITE     TCMBS03
071800970526     C                   ADD       1             SFLRIG
071900970526     C*
072000970526     C* LETTURA FILE DA CARICARE
072100970526     C                   EXSR      LETFIL
072200970526     C                   END                                                    DO WHILE
072300970526     C*
072400970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
072500970526     C     WFLULT        IFNE      SFLULT
072600970526     C                   Z-ADD     WFLULT        SFLNBR
072700970526     C                   ADD       1             SFLNBR
072800970526     C                   END
072900970526     C*
073000970526     C                   ENDSR
073100970526     C*------------------------------------------------------------------------*
073200970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
073300970526     C*------------------------------------------------------------------------*
073400970526     C     OUTSFL        BEGSR
073500970526     C*
073600970526     C* WRITE RECORD TESTATA VIDEO
073700141210     C                   WRITE     TCMBD01
073800970526     C*
073900970526     C* VISUALIZZAZIONE MESSAGGIO
074000970526     C                   EXSR      SNDMSG
074100970526     C*
074200141211     C* finto una write del rcd con le opzioni per non dover cancellare tutta la gestione
074300141211     C                   IF        1 <> 1
074400141211     C                   WRITE     TCMBD04
074500141211     C                   ENDIF
074600141211     C*
074700141211     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
074800141211     C                   WRITE     TCMBD02
074900970526     C*
075000970526     C* WRITE RECORD TASTI FUNZIONALI
075100141210     C                   WRITE     TCMBD09
075200970526     C*
075300970526     C* CASO DI SFL VUOTO
075400970526     C     SFLULT        IFLE      0
075500970526     C*
075600141211     C                   SETOFF                                       30
075700141211     C*
075800141211     C*    - WRITE RECORD CONTROLLO SFL VUOTO
075900141211     C                   WRITE     TCMBD03
076000141211     C                   READ      TCMBD03
076100970526     C                   ELSE
076200970526     C*
076300970526     C* CASO DI SFL CON RECORDS
076400141211     C*
076500141211     C                   SETON                                        30
076600141211     C*
076700970526     C*    - WRITE RECORD CONTROLLO SFL
076800141210     C                   WRITE     TCMBD03
076900141210     C                   READ      TCMBD03
077000970526     C                   END
077100970526     C*
077200970526     C                   ENDSR
077300970526     C*------------------------------------------------------------------------*
077400970526     C* SNDMSG - MANDA MESSAGGIO
077500970526     C*------------------------------------------------------------------------*
077600970526     C     SNDMSG        BEGSR
077700970526     C*
077800970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
077900970526     C                   MOVEL     '1'           *IN40
078000141210     C                   WRITE     TCMBDMC
078100970526     C                   MOVEL     '0'           *IN40
078200970526     C*
078300970526     C* VISUALIZZAZIONE MESSAGGIO
078400970526     C                   Z-ADD     0             MSGULT
078500970526     C                   CALL      'XMSG'        XMSGPA
078600970526     C     DSMSER        IFEQ      0
078700970526     C                   MOVEL     DSMSKE        MSGKEY
078800970526     C                   END
078900970526     C                   Z-ADD     1             MSGULT
079000141210     C                   WRITE     TCMBDMS                                      SFL MESS
079100141210     C                   WRITE     TCMBDMC                                      CONTR MESS
079200970526     C*
079300970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
079400970526     C                   MOVEL     *BLANKS       DSMSMS
079500970526     C*
079600970526     C                   ENDSR
079700970526     C*------------------------------------------------------------------------*
079800970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
079900970526     C*------------------------------------------------------------------------*
080000970526     C     FUNCON        BEGSR
080100970526     C*
080200970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
080300970526     C                   EXSR      FUNVAR
080400970526     C*
080500970526     C* PULISCO CAMPI DI LAVORO
080600970526     C                   Z-ADD     0             I                 5 0
080700970526     C                   Z-ADD     0             W                 5 0
080800970526     C*
080900970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
081000970526     C     FUNKEY        IFNE      *BLANKS
081100970526     C     1             DO        24            I
081200970526     C                   MOVEL     FUD(I)        FUDRIG
081300970526     C*
081400970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
081500970526     C     FUNKEY        IFEQ      FUDFUN
081600970526     C     FUDATT        ANDEQ     'S'
081700970526     C                   Z-ADD     I             W
081800970526     C                   END
081900970526     C                   END                                                    DO WHILE
082000970526     C*
082100970526     C* FUNZIONE NON ATTIVA (ERRORE)
082200970526     C     W             IFEQ      0
082300970526     C*
082400970526     C* EMISSIONE MESSAGGIO DI ERRORE
082500970526     C                   MOVEL     MSG(2)        DSMSMS
082600970526     C                   END                                                    W=0
082700970526     C                   END                                                    FUNKEY NE *BLANKS
082800970526     C*
082900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
083000970526     C     W             IFNE      0
083100970526     C     FUNKEY        OREQ      *BLANKS
083200970526     C*
083300970526     C     FUNKEY        IFEQ      *BLANKS
083400970526     C                   EXSR      ENTGES
083500970526     C                   END
083600970526     C     FUNKEY        IFEQ      '03'
083700970526     C                   EXSR      F03GES
083800970526     C                   END
083900120510     C     FUNKEY        IFEQ      '04'
084000120510     C                   EXSR      F04GES
084100970526     C                   END
084200120510     C     FUNKEY        IFEQ      '05'
084300120510     C                   EXSR      F05GES
084400120510     C                   END
084500970526     C     FUNKEY        IFEQ      '12'
084600970526     C                   EXSR      F12GES
084700970526     C                   END
084800970528     C     FUNKEY        IFEQ      '21'
084900970528     C                   EXSR      F21GES
085000970528     C                   END
085100970528     C     FUNKEY        IFEQ      '22'
085200970528     C                   EXSR      F22GES
085300970528     C                   END
085400970526     C     FUNKEY        IFEQ      '23'
085500970526     C                   EXSR      F23GES
085600970526     C                   END
085700970526     C     FUNKEY        IFEQ      '24'
085800970526     C                   EXSR      F24GES
085900970526     C                   END
086000970526     C     FUNKEY        IFEQ      'RU'
086100970526     C                   EXSR      RUPGES
086200970526     C                   END
086300970526     C*
086400970526     C                   END
086500970526     C*
086600970526     C                   ENDSR
086700970526     C*------------------------------------------------------------------------*
086800970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
086900970526     C*------------------------------------------------------------------------*
087000970526     C     FUNVAR        BEGSR
087100970526     C*
087200030509     C* FUNKEY=*BLANKS TCMNIFICA ENTER
087300970526     C     KEY           IFEQ      ENTER
087400970526     C                   MOVEL     *BLANKS       FUNKEY
087500970526     C                   END
087600970526     C     KEY           IFEQ      F1
087700970526     C                   MOVEL     '01'          FUNKEY
087800970526     C                   END
087900970526     C     KEY           IFEQ      F2
088000970526     C                   MOVEL     '02'          FUNKEY
088100970526     C                   END
088200970526     C     KEY           IFEQ      F3
088300970526     C                   MOVEL     '03'          FUNKEY
088400970526     C                   END
088500970526     C     KEY           IFEQ      F4
088600970526     C                   MOVEL     '04'          FUNKEY
088700970526     C                   END
088800970526     C     KEY           IFEQ      F5
088900970526     C                   MOVEL     '05'          FUNKEY
089000970526     C                   END
089100970526     C     KEY           IFEQ      F6
089200970526     C                   MOVEL     '06'          FUNKEY
089300970526     C                   END
089400970526     C     KEY           IFEQ      F7
089500970526     C                   MOVEL     '07'          FUNKEY
089600970526     C                   END
089700970526     C     KEY           IFEQ      F8
089800970526     C                   MOVEL     '08'          FUNKEY
089900970526     C                   END
090000970526     C     KEY           IFEQ      F9
090100970526     C                   MOVEL     '09'          FUNKEY
090200970526     C                   END
090300970526     C     KEY           IFEQ      F10
090400970526     C                   MOVEL     '10'          FUNKEY
090500970526     C                   END
090600970526     C     KEY           IFEQ      F11
090700970526     C                   MOVEL     '11'          FUNKEY
090800970526     C                   END
090900970526     C     KEY           IFEQ      F12
091000970526     C                   MOVEL     '12'          FUNKEY
091100970526     C                   END
091200970526     C     KEY           IFEQ      F13
091300970526     C                   MOVEL     '13'          FUNKEY
091400970526     C                   END
091500970526     C     KEY           IFEQ      F14
091600970526     C                   MOVEL     '14'          FUNKEY
091700970526     C                   END
091800970526     C     KEY           IFEQ      F15
091900970526     C                   MOVEL     '15'          FUNKEY
092000970526     C                   END
092100970526     C     KEY           IFEQ      F16
092200970526     C                   MOVEL     '16'          FUNKEY
092300970526     C                   END
092400970526     C     KEY           IFEQ      F17
092500970526     C                   MOVEL     '17'          FUNKEY
092600970526     C                   END
092700970526     C     KEY           IFEQ      F18
092800970526     C                   MOVEL     '18'          FUNKEY
092900970526     C                   END
093000970526     C     KEY           IFEQ      F19
093100970526     C                   MOVEL     '19'          FUNKEY
093200970526     C                   END
093300970526     C     KEY           IFEQ      F20
093400970526     C                   MOVEL     '20'          FUNKEY
093500970526     C                   END
093600970526     C     KEY           IFEQ      F21
093700970526     C                   MOVEL     '21'          FUNKEY
093800970526     C                   END
093900970526     C     KEY           IFEQ      F22
094000970526     C                   MOVEL     '22'          FUNKEY
094100970526     C                   END
094200970526     C     KEY           IFEQ      F23
094300970526     C                   MOVEL     '23'          FUNKEY
094400970526     C                   END
094500970526     C     KEY           IFEQ      F24
094600970526     C                   MOVEL     '24'          FUNKEY
094700970526     C                   END
094800970526     C     KEY           IFEQ      ROLLUP
094900970526     C                   MOVEL     'RU'          FUNKEY
095000970526     C                   END
095100970526     C     KEY           IFEQ      HELP
095200970526     C                   MOVEL     'HP'          FUNKEY
095300970526     C                   END
095400970526     C*
095500970526     C                   ENDSR
095600970526     C*------------------------------------------------------------------------*
095700970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
095800970526     C*------------------------------------------------------------------------*
095900970526     C     FUNGES        BEGSR
096000970526     C*
096100970526     C* PULISCO CAMPI DI LAVORO
096200970526     C                   Z-ADD     0             W                 5 0
096300970526     C*
096400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
096500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
096600970526     C     1             DO        24            I
096700970526     C                   MOVEL     FUD(I)        FUDRIG
096800970526     C*
096900970526     C* PULIZIA FUNZIONI PRECEDENTI
097000970526     C                   MOVEL     *BLANKS       FUV(I)
097100970526     C*
097200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
097300970526     C* (SALTA F23 E F24)
097400970526     C     FUDATT        IFEQ      'S'
097500970526     C     FUDFUN        ANDNE     '23'
097600970526     C     FUDFUN        ANDNE     '24'
097700970526     C                   ADD       1             W
097800970526     C                   MOVEL     FUDDES        FUV(W)
097900970526     C                   END
098000970526     C                   END                                                    DO WHILE
098100970526     C*
098200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
098300970526     C* ALTRIMENTI DISATTIVO F24
098400970526     C     1             DO        24            I
098500970526     C                   MOVEL     FUD(I)        FUDRIG
098600970526     C*
098700970526     C     FUDFUN        IFEQ      '24'
098800970526     C     W             IFGT      6
098900970526     C                   MOVEL     'S'           FUDATT
099000970526     C                   ELSE
099100970526     C                   MOVEL     *BLANKS       FUDATT
099200970526     C                   END                                                    W GT 8
099300970526     C                   MOVEL     FUDRIG        FUD(I)
099400970526     C                   END
099500970526     C                   END                                                    DO WHILE
099600970526     C*
099700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
099800970526     C     W             DIV       6             I
099900970526     C                   MVR                     Z
100000970526     C*
100100970526     C     Z             IFNE      0
100200970526     C     I             ADD       1             Z
100300970526     C                   ELSE
100400970526     C                   Z-ADD     I             Z
100500970526     C                   END
100600970526     C*
100700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
100800970526     C                   ADD       1             FUNGRU
100900970526     C     FUNGRU        IFGT      Z
101000970526     C     FUNGRU        ORLT      1
101100970526     C                   Z-ADD     1             FUNGRU
101200970526     C                   END
101300970526     C*
101400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
101500970526     C     FUNGRU        IFEQ      1
101600970526     C                   MOVEL     FUV(1)        FUN001
101700970526     C                   MOVEL     FUV(2)        FUN002
101800970526     C                   MOVEL     FUV(3)        FUN003
101900970526     C                   MOVEL     FUV(4)        FUN004
102000970526     C                   MOVEL     FUV(5)        FUN005
102100970526     C                   MOVEL     FUV(6)        FUN006
102200970526     C                   END
102300970526     C*
102400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
102500970526     C     FUNGRU        IFEQ      2
102600970526     C                   MOVEL     FUV(7)        FUN001
102700970526     C                   MOVEL     FUV(8)        FUN002
102800970526     C                   MOVEL     FUV(9)        FUN003
102900970526     C                   MOVEL     FUV(10)       FUN004
103000970526     C                   MOVEL     FUV(11)       FUN005
103100970526     C                   MOVEL     FUV(12)       FUN006
103200970526     C                   END
103300970526     C*
103400970526     C* TERZO GRUPPO DI 6 FUNZIONI
103500970526     C     FUNGRU        IFEQ      3
103600970526     C                   MOVEL     FUV(13)       FUN001
103700970526     C                   MOVEL     FUV(14)       FUN002
103800970526     C                   MOVEL     FUV(15)       FUN003
103900970526     C                   MOVEL     FUV(16)       FUN004
104000970526     C                   MOVEL     FUV(17)       FUN005
104100970526     C                   MOVEL     FUV(18)       FUN006
104200970526     C                   END
104300970526     C*
104400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
104500970526     C     FUNGRU        IFEQ      4
104600970526     C                   MOVEL     FUV(19)       FUN001
104700970526     C                   MOVEL     FUV(20)       FUN002
104800970526     C                   MOVEL     FUV(21)       FUN003
104900970526     C                   MOVEL     FUV(22)       FUN004
105000970526     C                   MOVEL     *BLANKS       FUN005
105100970526     C                   MOVEL     *BLANKS       FUN006
105200970526     C                   END
105300970526     C*
105400970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
105500970526     C                   MOVEL     *BLANKS       FUN007
105600970526     C                   MOVEL     *BLANKS       FUN008
105700970526     C*
105800970526     C     1             DO        24            I
105900970526     C                   MOVEL     FUD(I)        FUDRIG
106000970526     C     FUDFUN        IFEQ      '23'
106100970526     C     FUDATT        ANDEQ     'S'
106200970526     C                   MOVEL     FUDDES        FUN007
106300970526     C                   END
106400970526     C     FUDFUN        IFEQ      '24'
106500970526     C     FUDATT        ANDEQ     'S'
106600970526     C                   MOVEL     FUDDES        FUN008
106700970526     C                   END
106800970526     C                   END                                                    DO
106900970526     C*
107000970526     C                   ENDSR
107100970526     C*------------------------------------------------------------------------*
107200970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
107300970526     C*------------------------------------------------------------------------*
107400970526     C     OPZGES        BEGSR
107500970526     C*
107600970526     C* PULISCO CAMPI DI LAVORO
107700970526     C                   Z-ADD     0             W                 5 0
107800970526     C*
107900970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
108000970526     C* E CARICO TABELLA OPZIONI ATTIVE
108100991115     C     1             DO        24            I
108200970526     C                   MOVEL     OPD(I)        OPDRIG
108300970526     C*
108400970526     C* PULIZIA OPZIONI PRECEDENTI
108500970526     C                   MOVEL     *BLANKS       OPV(I)
108600970526     C*
108700970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
108800970526     C     OPDATT        IFEQ      'S'
108900970526     C                   ADD       1             W
109000970526     C                   MOVEL     OPDDES        OPV(W)
109100970526     C                   END
109200970526     C                   END                                                    DO WHILE
109300970526     C*
109400970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
109500970526     C* ALTRIMENTI DISATTIVO F23
109600991115     C     1             DO        24            I
109700970526     C                   MOVEL     FUD(I)        FUDRIG
109800970526     C*
109900970526     C     FUDFUN        IFEQ      '23'
110000970526     C     W             IFGT      8
110100970526     C                   MOVEL     'S'           FUDATT
110200970526     C                   ELSE
110300970526     C                   MOVEL     *BLANKS       FUDATT
110400970526     C                   END                                                    W GT 8
110500970526     C                   MOVEL     FUDRIG        FUD(I)
110600970526     C                   END
110700970526     C                   END                                                    DO WHILE
110800970526     C*
110900970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
111000970526     C     W             DIV       8             I
111100970526     C                   MVR                     Z                 5 0
111200970526     C*
111300970526     C     Z             IFNE      0
111400970526     C     I             ADD       1             Z
111500970526     C                   ELSE
111600970526     C                   Z-ADD     I             Z
111700970526     C                   END
111800970526     C*
111900970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
112000970526     C                   ADD       1             OPZGRU
112100970526     C     OPZGRU        IFGT      Z
112200970526     C     OPZGRU        ORLT      1
112300970526     C                   Z-ADD     1             OPZGRU
112400970526     C                   END
112500970526     C*
112600970526     C* PRIMO GRUPPO DI 8 OPZIONI
112700970526     C     OPZGRU        IFEQ      1
112800970526     C                   MOVEL     OPV(1)        OPZ001
112900970526     C                   MOVEL     OPV(2)        OPZ002
113000970526     C                   MOVEL     OPV(3)        OPZ003
113100970526     C                   MOVEL     OPV(4)        OPZ004
113200970526     C                   MOVEL     OPV(5)        OPZ005
113300970526     C                   MOVEL     OPV(6)        OPZ006
113400970526     C                   MOVEL     OPV(7)        OPZ007
113500970526     C                   MOVEL     OPV(8)        OPZ008
113600970526     C                   END
113700970526     C*
113800970526     C* SECONDO GRUPPO DI 8 OPZIONI
113900970526     C     OPZGRU        IFEQ      2
114000970526     C                   MOVEL     OPV(9)        OPZ001
114100970526     C                   MOVEL     OPV(10)       OPZ002
114200970526     C                   MOVEL     OPV(11)       OPZ003
114300970526     C                   MOVEL     OPV(12)       OPZ004
114400970526     C                   MOVEL     OPV(13)       OPZ005
114500970526     C                   MOVEL     OPV(14)       OPZ006
114600970526     C                   MOVEL     OPV(15)       OPZ007
114700970526     C                   MOVEL     OPV(16)       OPZ008
114800970526     C                   END
114900970526     C*
115000970526     C* TERZO GRUPPO DI 8 OPZIONI
115100970526     C     OPZGRU        IFEQ      3
115200970526     C                   MOVEL     OPV(17)       OPZ001
115300970526     C                   MOVEL     OPV(18)       OPZ002
115400970526     C                   MOVEL     OPV(19)       OPZ003
115500970526     C                   MOVEL     OPV(20)       OPZ004
115600970526     C                   MOVEL     OPV(21)       OPZ005
115700970526     C                   MOVEL     OPV(22)       OPZ006
115800970526     C                   MOVEL     OPV(23)       OPZ007
115900970526     C                   MOVEL     OPV(24)       OPZ008
116000970526     C                   END
116100970526     C*
116200970526     C                   ENDSR
116300970526     C*------------------------------------------------------------------------*
116400970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
116500970526     C*------------------------------------------------------------------------*
116600970526     C     OPZCON        BEGSR
116700970526     C*
116800970526     C* PULISCO CAMPI DI LAVORO
116900970526     C                   Z-ADD     0             I                 5 0
117000970526     C                   Z-ADD     0             W                 5 0
117100970526     C*
117200970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
117300970526     C     1             DO        24            I
117400970526     C                   MOVEL     OPD(I)        OPDRIG
117500970526     C*
117600970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
117700970526     C     OPZKEY        IFEQ      OPDOPZ
117800970526     C     OPZKEY        OREQ      OPDOP1
117900970526     C*
118000970526     C     OPDATT        IFEQ      'S'
118100970526     C                   MOVEL     OPDOPZ        OPZKEY
118200970526     C                   Z-ADD     I             W
118300970526     C                   END
118400970526     C                   END
118500970526     C*
118600970526     C                   END                                                    DO WHILE
118700970526     C*
118800970526     C* OPZIONE NON ATTIVA (ERRORE)
118900970526     C     W             IFEQ      0
119000970526     C*
119100970526     C* OPZIONE NON OK
119200970526     C                   Z-ADD     1             OPZOK
119300970526     C*
119400970526     C* EMISSIONE MESSAGGIO DI ERRORE
119500970526     C                   MOVEL     MSG(1)        DSMSMS
119600970526     C                   ELSE
119700970526     C*
119800970526     C* OPZIONE ATTIVA (PROCEDI ...)
119900970526     C*
120000141211     C                   MOVEL     *BLANKS       pOut_Msg
120100030509     C     OPZKEY        IFEQ      ' 2'
120200030509     C                   EXSR      O02GES
120300030509     C                   END
120400141211     C                   MOVEL     pOut_Msg      DSMSMS
120500021211     C                   END                                                    OPZ.NON ATTIVA
120600970526     C*
120700970526     C                   ENDSR
120800970526     C*------------------------------------------------------------------------*
120900970526     C* NOTE COSTRUZIONE SCHIERE
121000970526     C*------------------------------------------------------------------------*
121100970526     C*
121200970526     C* -MSG-MESSAGGI
121300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
121400970526     C* (NON TOCCARE MSG,1 E MSG,2)
121500970526     C*
121600970526     C* -CMD-COMANDI
121700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
121800970526     C*
121900970526     C* -OPD-OPZIONI
122000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
122100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
122200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
122300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
122400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
122500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
122600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
122700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
122800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
122900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
123000970526     C*
123100970526     C* -FUD-FUNZIONI
123200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
123300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
123400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
123500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
123600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
123700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
123800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
123900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
124000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
124100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
124200991108     C*
124300991115     C* -STS-CODICI STATI DEI RECORD DI LOG
124400991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
124500991110     C* ================> IMPORTANTE <=================
124600991115     C* per codici di errore in -STS- indicare in -DES- sempre
124700991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
124800991110     C* ERR
124900991108     C*
125000970526     O*------------------------------------------------------------------------*
125100970526** DEC - DECODIFICHE
125200970526    GESTIONE                                                     1
125300000407 INTERROGAZIONE                                                  2
1254000305093
1255009705264
1256009705265
125700970526** MSG - MESSAGGI
125800141210TRTCMBR- Opzione non gestita.                                                  1
125900141210TRTCMBR- Tasto funzionale non gestito.                                         2
126000141210TRTCMBR- Non abilitato all inserimento in filiale.                             3
126100141210TRTCMBR- Utente non abilitato all'opzione richiesta.                           4
126200141210TRTCMBR- File di spool non trovato o inesistente, verificare.                  5
126300141210TRTCMBR- Specificare almeno 5 caratteri per la ricerca.                        6
126400141210TRTCMBR- Operazione non consentita su log ancora da elaborare.                 7
126500141210TRTCMBR- 8                                                                     8
126600141210TRTCMBR- 9                                                                     9
126700141210TRTCMBR- 10                                                                   10
126800141210TRTCMBR- 11                                                                   11
126900141210TRTCMBR- 12                                                                   12
127000141210TRTCMBR- 13                                                                   13
127100141210TRTCMBR- 14                                                                   14
127200141210TRTCMBR- 15                                                                   15
127300141210TRTCMBR- 16                                                                   16
127400141210TRTCMBR- 17                                                                   17
127500141210TRTCMBR- 18                                                                   18
127600141210TRTCMBR- 19                                                                   19
127700141210TRTCMBR- 20                                                                   20
127800970528** CMD - COMANDI CL
127900021211
128000021211
128100991110
128200970528
128300970528
128400970526** OPD - OPZIONI
128500030509                   -  -     (S/N)=N                        opz. 01¬
128600141210                   -  -     (S/N)=N                        opz. 02¬
128700030509                   -  -     (S/N)=N                        opz. 03¬
128800141210                   -  -     (S/N)=N                        opz. 04¬
128900141210                   -  -     (S/N)=N                        opz. 05¬
129000141210                   -  -     (S/N)=N                        opz. 06¬
129100021211                   -  -     (S/N)=N                        opz. 07¬
129200991110                   -  -     (S/N)=N                        opz. 08¬
129300991110                   -  -     (S/N)=N                        opz. 09¬
129400141210                   -  -     (S/N)=N                        opz. 10¬
129500141210                   -  -     (S/N)=N                        opz. 11¬
129600970526                   -  -     (S/N)=N                        opz. 12¬
129700970526                   -  -     (S/N)=N                        opz. 13¬
129800000407                   -  -     (S/N)=N                        opz. 14¬
129900141210                   -  -     (S/N)=N                        opz. 15¬
130000970526                   -  -     (S/N)=N                        opz. 16¬
130100970526                   -  -     (S/N)=N                        opz. 17¬
130200970526                   -  -     (S/N)=N                        opz. 18¬
130300970526                   -  -     (S/N)=N                        opz. 19¬
130400970526                   -  -     (S/N)=N                        opz. 20¬
130500970526                   -  -     (S/N)=N                        opz. 21¬
130600970526                   -  -     (S/N)=N                        opz. 22¬
130700970526                   -  -     (S/N)=N                        opz. 23¬
130800000407                   -  -     (S/N)=N                        opz. 25¬
130900970526** FUD - FUNZIONI
131000970526                   -        (S/N)=N                        funz.01¬
131100970526                   -        (S/N)=N                        funz.02¬
131200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
131300141210                   -        (S/N)=N                        funz.04¬
131400141210                   -        (S/N)=N                        funz.05¬
131500030509                   -        (S/N)=N                        funz.06¬
131600970526                   -        (S/N)=N                        funz.07¬
131700970527                   -        (S/N)=N                        funz.08¬
131800141210                   -        (S/N)=N                        funz.09¬
131900970527                   -        (S/N)=N                        funz.10¬
132000970526                   -        (S/N)=N                        funz.11¬
132100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
132200991116                   -        (S/N)=N                        funz.13¬
132300970526                   -        (S/N)=N                        funz.14¬
132400970526                   -        (S/N)=N                        funz.15¬
132500970526                   -        (S/N)=N                        funz.16¬
132600970526                   -        (S/N)=N                        funz.17¬
132700970526                   -        (S/N)=N                        funz.18¬
132800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
132900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
133000000407                   -        (S/N)=N                        funz.21¬
133100141210                   -        (S/N)=N                        funz.22¬
133200991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
133300991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
