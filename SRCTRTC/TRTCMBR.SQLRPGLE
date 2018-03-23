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
011200150417     D  pIn_AAS                       6
011300050216     D*------------------
011400050216     D* VARIABILI D WRK
011500050216     D*------------------
011600050808     D wDataDA         S               D   DATFMT(*iso)
011700141211     d wSQL            s           2048    inz  varying
011800141211     D TIMAT00F      E DS
011900141211     D RcdTIMAT        DS
012000141211     D  wMATTO                             like(MATTO)
012100141211     D  wMATSTS                            like(MATSTS)
012200141211     D  wMATMSG                            like(MATMSG)
012300141211     D  wMATMOMINS                         like(MATMOMINS)
012400141211     D  wMATMOMSTS                         like(MATMOMSTS)
012500050216     D*------------------
012600050216     D* COSTANTI
012700050216     D*------------------
012800050216     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
012900050216     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
013000970526     C*------------------------------------------------------------------------*
013100970526     C* MAIN LINES
013200970526     C*------------------------------------------------------------------------*
013300100202
013400100202
013500100202     C*
013600100202     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013700100202     C
013800100202     C/EXEC SQL
013900100202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014000100202     C/END-EXEC
014100100202     C*
014200970526     C*
014300970526     C* SE NON CI SONO ERRORI, PROSEGUE
014400970526IF  1C     ERRL00        IFNE      '1'
014500970526     C*
014600970526     C* CARICA IL SFILE
014700970526     C                   EXSR      POSSFL
014800970526     C*
014900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015300970526     C*
015400970526     C* REIMPOSTA ERRORE DI PROCERURA
015500970526     C                   MOVEL     '0'           ERRL00
015600970526     C*
015700050114     C* EMETTE IL SFILE
015800970526     C                   EXSR      OUTSFL
015900970526     C*
016000970526     C* GESTIONE TASTI FUNZIONALI
016100970526     C                   EXSR      FUNCON
016200970526     C*
016300970526E   2C                   ENDDO
016400970527     C*
016500970526E   1C                   ENDIF
016600991108     C*
016700970526     C* FINE PGM
016800970526     C                   SETON                                        LR
016900970526     C*------------------------------------------------------------------------*
017000970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017100970526     C*------------------------------------------------------------------------*
017200970526     C     F03GES        BEGSR
017300970526     C*
017400970526     C* IMPOSTA "PREMUTO F03"
017500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017600970526     C*
017700970526     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900120510     C* F04GES - GESTIONE F04 -> Delete log con prg=0
018000970526     C*------------------------------------------------------------------------*
018100120510     C     F04GES        BEGSR
018200970526     C*
018300120510     C* salvo kpjba prima di passarla
018400120510     C                   MOVEL     KPJBA         Sav_kpjba
018500120510     C* chiamata a pgm apposito
018600120510     C* (ha il video che chiede i parametri e non gliene servono altri)
018700120510     C                   CALL      'TRTCMAR'
018800120510     C                   PARM                    KPJBA
018900120510     C* ripristino kpjba
019000120510     C                   EVAL      KPJBA =       Sav_kpjba
019100970526     C*
019200970526     C                   ENDSR
019300120510     C*------------------------------------------------------------------------*
019400120510     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
019500120510     C*------------------------------------------------------------------------*
019600120510     C     F05GES        BEGSR
019700120510     C*
019800120510     C                   EXSR      POSSFL                                       *RICARICA SFILE
019900120510     C*
020000120510     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200030509     C* F12GES - GESTIONE F12 -> RITORNO
020300970526     C*------------------------------------------------------------------------*
020400970526     C     F12GES        BEGSR
020500970526     C*
020600970526     C* IMPOSTA "PREMUTO F12"
020700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020800970526     C*
020900970526     C                   ENDSR
021000970528     C*------------------------------------------------------------------------*
021100970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021200970528     C*------------------------------------------------------------------------*
021300970528     C     F21GES        BEGSR
021400970612     C*
021500991117     C* NON GESTITA
021600970528     C*
021700970528     C                   ENDSR
021800970528     C*------------------------------------------------------------------------*
021900970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
022000970528     C*------------------------------------------------------------------------*
022100970528     C     F22GES        BEGSR
022200970528     C*
022300021211     C* NON GESTITA
022400970528     C*
022500970528     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022800970526     C*------------------------------------------------------------------------*
022900970526     C     F23GES        BEGSR
023000970526     C*
023100970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023600970526     C*------------------------------------------------------------------------*
023700970526     C     F24GES        BEGSR
023800970526     C*
023900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024400970526     C*------------------------------------------------------------------------*
024500970526     C     RUPGES        BEGSR
024600970526     C*
024700970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100991108     C* ENTGES - GESTIONE ENTER -> INVIO
025200970526     C*------------------------------------------------------------------------*
025300970526     C     ENTGES        BEGSR
025400000407     C*
025500000407     C                   MOVEL     '0'           ERRL00
025600991108     C*
025700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025800970526     C                   EXSR      NEWPOS
025900021211     C*
026000021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026100021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026200021211     C     SWPOSI        IFNE      0
026300021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
026400021211     C                   ELSE
026500021211     C     SFLULT        IFNE      0
026600021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026700021211     C*
026800021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026900021211     C     SWMODI        IFNE      0
027000021211     C                   EXSR      POSSFL
027100021211     C                   ENDIF
027200021211     C                   ENDIF
027300021211     C                   ENDIF
027400021211     C                   ENDIF
027500021211     C*
027600021211     C                   ENDSR
027700030509     C*------------------------------------------------------------------------*
027800030509     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
027900030509     C*------------------------------------------------------------------------*
028000030509     C     O02GES        BEGSR
028100030509     C*
028200030509     C*
028300030509     C                   ENDSR
028400991028     C*------------------------------------------------------------------------*
028500991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
028600991112     C*------------------------------------------------------------------------*
028700991112     C     PASPAR        BEGSR
028800991112     C*
028900000407     C                   MOVEL     PARAM         KPJBU
029000991112     C*
029100991112     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
029400970526     C*------------------------------------------------------------------------*
029500970526     C     NEWPOS        BEGSR
029600970526     C*
029700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
029800970526     C*         1= RICHIESTO POSIZIONAMENTO
029900970526     C                   Z-ADD     0             SWPOSI            7 0
030000970526     C*
030100991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
030200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
030300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
030400970526     C                   Z-ADD     0             SWMODI            1 0
030500970530     C*
030600970530     C* REIMPOSTA INDICATORI DI ERRORE
030700050111     C                   MOVEL     '0'           ERRPAR            1
030800970526     C*
030900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
031000030305     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
031100050111     C*
031200970526     C                   ENDSR
031300970526     C*------------------------------------------------------------------------*
031400021217     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
031500970526     C*------------------------------------------------------------------------*
031600970528     C     SETFIL        BEGSR
031700050808     C*
031800141211     c                   EVAL      wSQL =
031900141211     c                             'select +
032000141211     c                              MATTO, +
032100141211     c                              MATSTS, +
032200141211     c                              MATMSG, +
032300141211     c                              MATMOMINS, +
032400141211     c                              MATMOMSTS +
032500141211     c                              from timat00f +
032600141211     c                              where +
032700141211     c                              MATTRC = ''BM1'' and +
032800141211     c                              MATPROG = ''' +
032900150417     c                              D02Prog + '''   and  +
033000150417     c                              MATAAS = ' +
033100150417     c                              D02AAS  + ''
033200141211      *
033300141211     C/EXEC SQL
033400141211     C+ PREPARE S1 FROM :wSQL
033500141211     C/END-EXEC
033600141211
033700141211     C/EXEC SQL
033800141211     C+ DECLARE A1 CURSOR FOR S1
033900141211     C/END-EXEC
034000141211
034100141211     C/EXEC SQL
034200141211     C+ OPEN A1
034300141211     C/END-EXEC
034400980107     C*
034500021211     C* LETTURA RECORD
034600970526     C                   EXSR      LETFIL
034700970526     C*
034800970526     C                   ENDSR
034900970526     C*------------------------------------------------------------------------*
035000970526     C* LETFIL - LETTURA RECORD
035100970526     C*------------------------------------------------------------------------*
035200970526     C     LETFIL        BEGSR
035300970526     C*
035400141211     C* leggo finchè non trovo un rcd valido
035500141211     C* oppure sono finiti i rcd
035600141211     C                   MOVEL     'N'           WRECOK
035700141211     C                   DOU       SQLCod >= 0 and SQLCod <> 100 and
035800141211     C                             wRecOK = 'S'
035900141211     C                             OR
036000141211     C                             SFLEOF = 1
036100970526     C*
036200141211     C/EXEC SQL
036300141211     C+ FETCH NEXT FROM A1 INTO :RcdTIMAT
036400141211     C/END-EXEC
036500970528     C*
036600970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
036700141211     C                   IF        SQLCod < 0    or                             *ERRORE BLOCCANTE
036800141211     C                             SQLCod = 100                                 *FINE FILE
036900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
037000021211     C                   ELSE
037100970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
037200021211     C                   ENDIF
037300021211     C                   ENDDO
037400970526     C*
037500970526     C                   ENDSR
037600970526     C*------------------------------------------------------------------------*
037700021217     C* CHKREC - CONTROLLA VALIDITA' RECORD
037800970526     C*------------------------------------------------------------------------*
037900970526     C     CHKREC        BEGSR
038000970526     C*
038100970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
038200991108     C*
038300970526     C                   ENDSR
038400970526     C*------------------------------------------------------------------------*
038500000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
038600970526     C*------------------------------------------------------------------------*
038700970526     C     IRESFL        BEGSR
038800970526     C*
038900141211     C                   MOVEL     wMATTo        VIDTo
039000141211     C                   MOVEL     wMATSts       VIDSts
039100141211     C                   MOVEL     wMATMsg       VIDMsg
039200141211     C                   EVAL      VIDMomIns = wMATMomIns
039300141211     C                   EVAL      VIDMomSts = wMATMomSts
039400040818     C*
039500970526     C                   ENDSR
039600970526     C*------------------------------------------------------------------------*
039700991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
039800970526     C*------------------------------------------------------------------------*
039900970526     C     CHKPAR        BEGSR
040000021218     C*
040100021218     C* INIZIALIZZO FLAG GENERICO DI ERRORE SELEZIONI
040200030509     C                   MOVEL     '0'           W_ERR             1
040300000407     C*
040400970526     C                   ENDSR
040500970526     C*------------------------------------------------------------------------*
040600970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
040700970526     C*------------------------------------------------------------------------*
040800970526     C     CHKFOP        BEGSR
040900991112     C*
041000991112     C* OPZIONI
041100970626     C*
041200141210     C* non ci sono opzio perché il video è di sola visualizzazione, ma tutto rimane
041300141210     C* impostato come se fosse possibile averle
041400141210     C*
041500000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
041600021211     C     DEPOP0        IFEQ      'P01'
041700000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
041800000420     C                   MOVEL     'S'           OPDATT
041900000420     C                   MOVEL     OPDRIG        OPD(1)
042000021211     C                   ELSE
042100000420     C* OPZIONI
042200970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
042300970626     C                   MOVEL     'S'           OPDATT
042400970626     C                   MOVEL     OPDRIG        OPD(5)
042500141210     C                   MOVEL     OPD(10)       OPDRIG                         *VISUAL.STATUS INVIO
042600141210     C                   MOVEL     'S'           OPDATT
042700141210     C                   MOVEL     OPDRIG        OPD(10)
042800021211     C                   ENDIF
042900970626     C*
043000000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
043100021211     C     DEPOP0        IFEQ      'P02'
043200991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
043300991112     C                   MOVEL     'S'           OPDATT
043400991112     C                   MOVEL     OPDRIG        OPD(5)
043500050111     C                   MOVEL     OPD(15)       OPDRIG                         *VISUALIZZA SPOOL
043600050111     C                   MOVEL     'S'           OPDATT
043700050111     C                   MOVEL     OPDRIG        OPD(15)
043800030509     C                   IF        %subst(KNMUS:1:3) = 'EDP'
043900030509     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
044000030509     C                   MOVEL     'S'           OPDATT
044100030509     C                   MOVEL     OPDRIG        OPD(2)
044200031210     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
044300031210     C                   MOVEL     'S'           OPDATT
044400031210     C                   MOVEL     OPDRIG        OPD(4)
044500031210     C                   MOVEL     OPD(6)        OPDRIG                         *RILASCIO
044600031210     C                   MOVEL     'S'           OPDATT
044700031210     C                   MOVEL     OPDRIG        OPD(6)
044800071001     C                   MOVEL     OPD(11)       OPDRIG                         *RIESECUZIONE SPOOL
044900071001     C                   MOVEL     'S'           OPDATT
045000071001     C                   MOVEL     OPDRIG        OPD(11)
045100120510     C                   MOVEL     FUD(04)       FUDRIG                         *DLT LOG CON PRG=0
045200120510     C                   MOVEL     'S'           FUDATT
045300120510     C                   MOVEL     FUDRIG        FUD(04)
045400021211     C                   ENDIF
045500030509     C                   ENDIF
045600970526     C*
045700970526     C* GESTIONE OPZIONI A VIDEO
045800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
045900970526     C                   EXSR      OPZGES
046000970526     C*
046100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
046200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
046300970526     C                   EXSR      FUNGES
046400970526     C*
046500970526     C                   ENDSR
046600020917     C*--------------------------------------------------------------------------------------------*
046700020917     C* REPERISCE I DATI UTENTE
046800020917     C*--------------------------------------------------------------------------------------------*
046900020917     C     REPDATIUTE    BEGSR
047000020917     C*
047100020917     C* INIZIALIZZA VARIABILI DI WRK
047200020917     C                   CLEAR                   TIBS34DS
047300020917     C                   CLEAR                   AZUTEDS
047400020917     C                   CLEAR                   DDATIUTE
047500020917     C*
047600020917     C     *DTAARA       DEFINE    §azute        azuteds
047700020917     C     *DTAARA       DEFINE    §datiute      ddatiute
047800020917     C                   IN(E)     *DTAARA
047900021211     C                   IF        %Error
048000021211     C                   EVAL      I34Tla = 'L'
048100020917     C                   CALL      'TIBS34R'
048200020917     C                   PARM                    Tibs34Ds
048300020917     C                   IN        *DTAARA
048400021211     C                   ENDIF
048500020917     C*
048600020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
048700020917     C                   MOVEL(P)  RSUT          DSFIRS           20
048800020917     C*
048900020917     C                   ENDSR
049000970526     C*------------------------------------------------------------------------*
049100970526     C* *INZSR - OPERAZIONI INIZIALI
049200970526     C*------------------------------------------------------------------------*
049300970526     C     *INZSR        BEGSR
049400970526     C*---
049500970526     C* RICEVIMENTO PARAMETRI
049600970526     C*---
049700970526     C     *ENTRY        PLIST
049800141211     C                   PARM                    Kpjba
049900141211
050000141211     C                   EVAL      Param = Kpjbu
050100141211     C                   EVAL      D02Prog = pIn_Prog
050200150417     C                   EVAL      D02AAS  = pIn_AAS
050300141211     C                   EVAL      D02Mit  = pIn_Mit
050400141210     C                   MOVEL     *blank        DEPOP0            3
050500050110     C*
050600050110     C* REPERIMENTO DATI UTENTE
050700050110     C                   EXSR      REPDATIUTE
050800050808     C*
050900050808     C* CALCOLA LA DATA CORRENTE
051000050808     C                   time                    wn14             14 0
051100050808     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
051200050808     C                   z-add     wn8           g08dat
051300050808     C                   z-add     *zeros        g08inv
051400050808     C                   movel     '0'           g08err
051500050808     C                   call      'XSRDA8'
051600050808     C                   parm                    wlbda8
051700050808     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
051800030604     C*
051900030604     C* INIZIALIZZO I VALORI DI DEFAULT A VIDEO
052000970526     C*
052100970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
052200970526     C     XMSGPA        PLIST
052300970526     C                   PARM                    DSMSMS
052400970526     C                   PARM                    DSMSPG
052500970526     C                   PARM                    DSMSKE
052600970526     C                   PARM                    DSMSER
052700970526     C*
052800970526     C* IMPOSTA LA DS -TASTI PREMUTI-
052900970526     C                   CALL      'XKEY'
053000970526     C                   PARM                    DSKEY
053100970526     C*---
053200970526     C* VARIABILI NON RIFERITE AL DATA BASE
053300970526     C*---
053400970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
053500970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
053600970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
053700970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
053800970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
053900970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
054000970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
054100970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
054200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
054300970526     C*---
054400021217     C     *LIKE         DEFINE    KPJBU         SAVKPJBU
054500991108     C*---
054600970526     C* VARIABILI DI CONTROLLO
054700970526     C*---
054800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
054900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
055100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
055200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
055300050110     C                   Z-ADD     8             SFLPAG                         *N°RIGHE X PAG
055400970526     C*
055500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
055600141210     C                   MOVEL     'TRTCMBR'     DSMSPG
055700141210     C                   MOVEL     'TRTCMBR'     MSGQUE
055800970526     C                   MOVEL     *BLANKS       MSGKEY
055900970526     C                   MOVEL     *BLANKS       DSMSMS
056000970526     C                   MOVEL     *BLANKS       DSMSKE
056100970526     C                   Z-ADD     *ZEROS        DSMSER
056200970526     C*---
056300970526     C* CHIAVI DI LETTURA
056400970526     C*---
056500970526     C*
056600970526     C* IMPOSTA I PARAMETRI DI INGRESSO
056700970526     C                   EXSR      CHKPAR
056800991115     C*
056900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
057000970526     C                   EXSR      CHKFOP
057100970526     C*
057200970526     C                   ENDSR
057300970526     C*------------------------------------------------------------------------*
057400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057600970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
057700970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
057800970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
057900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
058000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
058100970526     C*------------------------------------------------------------------------*
058200970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
058300970526     C*------------------------------------------------------------------------*
058400970526     C     POSSFL        BEGSR
058500970526     C*
058600970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
058700970526     C                   EXSR      CLRSFL
058800970526     C*
058900970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
059000970526     C                   EXSR      SETFIL
059100970526     C*
059200970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
059300970526     C     SFLEOF        IFEQ      0
059400970526     C                   EXSR      CARSFL
059500030305     C                   ENDIF
059600970526     C*
059700970526     C                   ENDSR
059800970526     C*------------------------------------------------------------------------*
059900970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
060000970526     C*------------------------------------------------------------------------*
060100970526     C     ENTATT        BEGSR
060200970526     C*
060300970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
060400970526     C                   Z-ADD     0             OPZOK             1 0
060500970526     C*
060600970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
060700970526     C                   Z-ADD     0             SFLVAR            5 0
060800970526     C*
060900970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
061000970526     C                   Z-ADD     0             SFLERR
061100970526     C*
061200970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
061300970526     C                   Z-ADD     SFLULT        WFLULT
061400970526     C*
061500970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
061600141210     C                   READC     TCMBS03                                98
061700970526     C     *IN98         DOWEQ     '0'
061800970526     C*
061900970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
062000970526     C                   ADD       1             SFLVAR
062100970526     C*
062200970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
062300970526     C     OPZOK         IFEQ      0
062400970526     C*
062500970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
062600970526     C     OPZKEY        IFNE      *BLANKS
062700970526     C                   EXSR      OPZCON
062800970526     C                   END
062900970526     C*
063000970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
063100970526     C*            2. PULIZIA SCELTA IN SFL
063200970526     C     OPZOK         IFEQ      0
063300970526     C                   MOVEL     '0'           *IN41
063400970526     C                   MOVEL     *BLANKS       OPZKEY
063500970526     C                   ELSE
063600970526     C*
063700970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
063800970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
063900970526     C                   MOVEL     '1'           *IN41
064000970526     C                   Z-ADD     SFLULT        SFLERR
064100970526     C                   END
064200970526     C                   ELSE
064300970526     C*
064400970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
064500970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
064600970526     C                   MOVEL     '1'           *IN41
064700970526     C                   END                                                    OPZOK=0
064800970526     C*
064900970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
065000970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
065100970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
065200970526     C     OPZOK         IFNE      0
065300970526     C                   Z-ADD     SFLERR        SFLNBR
065400970526     C                   ELSE
065500970526     C                   Z-ADD     SFLULT        SFLNBR
065600970526     C                   END
065700970526     C*
065800970526     C* AGGIORNO RECORD SFL
065900141211     C                   UPDATE    TCMBS03
066000970526     C*
066100141211     C                   READC     TCMBS03                                98
066200970526     C                   END                                                    DO WHILE
066300970526     C*
066400970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
066500970526     C                   Z-ADD     WFLULT        SFLULT
066600970526     C*
066700970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
066800970526     C     SFLVAR        IFEQ      0
066900970526     C                   END
067000991112     C*
067100970526     C                   ENDSR
067200970526     C*------------------------------------------------------------------------*
067300970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
067400970526     C*------------------------------------------------------------------------*
067500970526     C     CLRSFL        BEGSR
067600970526     C*
067700970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
067800970526     C* (0=NON FINE FILE  1=FINE FILE)
067900970526     C                   Z-ADD     0             SFLEOF
068000970526     C*
068100970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
068200970526     C                   Z-ADD     0             SFLULT
068300970526     C*
068400141211     C* pulizia SFL
068500141211     C                   SETOFF                                         3031
068600141210     C                   WRITE     TCMBD03
068700141211     C                   SETON                                          31
068800970526     C*
068900970526     C                   ENDSR
069000970526     C*------------------------------------------------------------------------*
069100970526     C* CARSFL - CARICAMENTO SFL
069200970526     C*------------------------------------------------------------------------*
069300970526     C     CARSFL        BEGSR
069400970526     C*
069500141211     C* queste istruzioni fanno fare una doppia SETFIL all'inizio e quindi una doppia lettura
069600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
069700141211     C*    SFLULT        IFEQ      0
069800141211     C*                  EXSR      SETFIL
069900141211     C*                  END
070000970526     C*
070100970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
070200970526     C                   Z-ADD     SFLULT        WFLULT
070300970526     C*
070400970526     C* CICLO:
070500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
070600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
070700970526     C                   Z-ADD     1             SFLRIG
070800030509     C     SFLRIG        DOWLE     SFLPAG
070900030509     C     SFLEOF        ANDEQ     0
071000970526     C*
071100970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
071200991116     C                   MOVEL     *BLANKS       OPZKEY
071300970526     C*
071400970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
071500970526     C                   EXSR      IRESFL
071600970526     C*
071700970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
071800970526     C                   ADD       1             SFLULT
071900970526     C*
072000970526     C* SCRITTURA RECORD SFL
072100141210     C                   WRITE     TCMBS03
072200970526     C                   ADD       1             SFLRIG
072300970526     C*
072400970526     C* LETTURA FILE DA CARICARE
072500970526     C                   EXSR      LETFIL
072600970526     C                   END                                                    DO WHILE
072700970526     C*
072800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
072900970526     C     WFLULT        IFNE      SFLULT
073000970526     C                   Z-ADD     WFLULT        SFLNBR
073100970526     C                   ADD       1             SFLNBR
073200970526     C                   END
073300970526     C*
073400970526     C                   ENDSR
073500970526     C*------------------------------------------------------------------------*
073600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
073700970526     C*------------------------------------------------------------------------*
073800970526     C     OUTSFL        BEGSR
073900970526     C*
074000970526     C* WRITE RECORD TESTATA VIDEO
074100141210     C                   WRITE     TCMBD01
074200970526     C*
074300970526     C* VISUALIZZAZIONE MESSAGGIO
074400970526     C                   EXSR      SNDMSG
074500970526     C*
074600141211     C* finto una write del rcd con le opzioni per non dover cancellare tutta la gestione
074700141211     C                   IF        1 <> 1
074800141211     C                   WRITE     TCMBD04
074900141211     C                   ENDIF
075000141211     C*
075100141211     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
075200141211     C                   WRITE     TCMBD02
075300970526     C*
075400970526     C* WRITE RECORD TASTI FUNZIONALI
075500141210     C                   WRITE     TCMBD09
075600970526     C*
075700970526     C* CASO DI SFL VUOTO
075800970526     C     SFLULT        IFLE      0
075900970526     C*
076000141211     C                   SETOFF                                       30
076100141211     C*
076200141211     C*    - WRITE RECORD CONTROLLO SFL VUOTO
076300141211     C                   WRITE     TCMBD03
076400141211     C                   READ      TCMBD03
076500970526     C                   ELSE
076600970526     C*
076700970526     C* CASO DI SFL CON RECORDS
076800141211     C*
076900141211     C                   SETON                                        30
077000141211     C*
077100970526     C*    - WRITE RECORD CONTROLLO SFL
077200141210     C                   WRITE     TCMBD03
077300141210     C                   READ      TCMBD03
077400970526     C                   END
077500970526     C*
077600970526     C                   ENDSR
077700970526     C*------------------------------------------------------------------------*
077800970526     C* SNDMSG - MANDA MESSAGGIO
077900970526     C*------------------------------------------------------------------------*
078000970526     C     SNDMSG        BEGSR
078100970526     C*
078200970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
078300970526     C                   MOVEL     '1'           *IN40
078400141210     C                   WRITE     TCMBDMC
078500970526     C                   MOVEL     '0'           *IN40
078600970526     C*
078700970526     C* VISUALIZZAZIONE MESSAGGIO
078800970526     C                   Z-ADD     0             MSGULT
078900970526     C                   CALL      'XMSG'        XMSGPA
079000970526     C     DSMSER        IFEQ      0
079100970526     C                   MOVEL     DSMSKE        MSGKEY
079200970526     C                   END
079300970526     C                   Z-ADD     1             MSGULT
079400141210     C                   WRITE     TCMBDMS                                      SFL MESS
079500141210     C                   WRITE     TCMBDMC                                      CONTR MESS
079600970526     C*
079700970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
079800970526     C                   MOVEL     *BLANKS       DSMSMS
079900970526     C*
080000970526     C                   ENDSR
080100970526     C*------------------------------------------------------------------------*
080200970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
080300970526     C*------------------------------------------------------------------------*
080400970526     C     FUNCON        BEGSR
080500970526     C*
080600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
080700970526     C                   EXSR      FUNVAR
080800970526     C*
080900970526     C* PULISCO CAMPI DI LAVORO
081000970526     C                   Z-ADD     0             I                 5 0
081100970526     C                   Z-ADD     0             W                 5 0
081200970526     C*
081300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
081400970526     C     FUNKEY        IFNE      *BLANKS
081500970526     C     1             DO        24            I
081600970526     C                   MOVEL     FUD(I)        FUDRIG
081700970526     C*
081800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
081900970526     C     FUNKEY        IFEQ      FUDFUN
082000970526     C     FUDATT        ANDEQ     'S'
082100970526     C                   Z-ADD     I             W
082200970526     C                   END
082300970526     C                   END                                                    DO WHILE
082400970526     C*
082500970526     C* FUNZIONE NON ATTIVA (ERRORE)
082600970526     C     W             IFEQ      0
082700970526     C*
082800970526     C* EMISSIONE MESSAGGIO DI ERRORE
082900970526     C                   MOVEL     MSG(2)        DSMSMS
083000970526     C                   END                                                    W=0
083100970526     C                   END                                                    FUNKEY NE *BLANKS
083200970526     C*
083300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
083400970526     C     W             IFNE      0
083500970526     C     FUNKEY        OREQ      *BLANKS
083600970526     C*
083700970526     C     FUNKEY        IFEQ      *BLANKS
083800970526     C                   EXSR      ENTGES
083900970526     C                   END
084000970526     C     FUNKEY        IFEQ      '03'
084100970526     C                   EXSR      F03GES
084200970526     C                   END
084300120510     C     FUNKEY        IFEQ      '04'
084400120510     C                   EXSR      F04GES
084500970526     C                   END
084600120510     C     FUNKEY        IFEQ      '05'
084700120510     C                   EXSR      F05GES
084800120510     C                   END
084900970526     C     FUNKEY        IFEQ      '12'
085000970526     C                   EXSR      F12GES
085100970526     C                   END
085200970528     C     FUNKEY        IFEQ      '21'
085300970528     C                   EXSR      F21GES
085400970528     C                   END
085500970528     C     FUNKEY        IFEQ      '22'
085600970528     C                   EXSR      F22GES
085700970528     C                   END
085800970526     C     FUNKEY        IFEQ      '23'
085900970526     C                   EXSR      F23GES
086000970526     C                   END
086100970526     C     FUNKEY        IFEQ      '24'
086200970526     C                   EXSR      F24GES
086300970526     C                   END
086400970526     C     FUNKEY        IFEQ      'RU'
086500970526     C                   EXSR      RUPGES
086600970526     C                   END
086700970526     C*
086800970526     C                   END
086900970526     C*
087000970526     C                   ENDSR
087100970526     C*------------------------------------------------------------------------*
087200970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
087300970526     C*------------------------------------------------------------------------*
087400970526     C     FUNVAR        BEGSR
087500970526     C*
087600030509     C* FUNKEY=*BLANKS TCMNIFICA ENTER
087700970526     C     KEY           IFEQ      ENTER
087800970526     C                   MOVEL     *BLANKS       FUNKEY
087900970526     C                   END
088000970526     C     KEY           IFEQ      F1
088100970526     C                   MOVEL     '01'          FUNKEY
088200970526     C                   END
088300970526     C     KEY           IFEQ      F2
088400970526     C                   MOVEL     '02'          FUNKEY
088500970526     C                   END
088600970526     C     KEY           IFEQ      F3
088700970526     C                   MOVEL     '03'          FUNKEY
088800970526     C                   END
088900970526     C     KEY           IFEQ      F4
089000970526     C                   MOVEL     '04'          FUNKEY
089100970526     C                   END
089200970526     C     KEY           IFEQ      F5
089300970526     C                   MOVEL     '05'          FUNKEY
089400970526     C                   END
089500970526     C     KEY           IFEQ      F6
089600970526     C                   MOVEL     '06'          FUNKEY
089700970526     C                   END
089800970526     C     KEY           IFEQ      F7
089900970526     C                   MOVEL     '07'          FUNKEY
090000970526     C                   END
090100970526     C     KEY           IFEQ      F8
090200970526     C                   MOVEL     '08'          FUNKEY
090300970526     C                   END
090400970526     C     KEY           IFEQ      F9
090500970526     C                   MOVEL     '09'          FUNKEY
090600970526     C                   END
090700970526     C     KEY           IFEQ      F10
090800970526     C                   MOVEL     '10'          FUNKEY
090900970526     C                   END
091000970526     C     KEY           IFEQ      F11
091100970526     C                   MOVEL     '11'          FUNKEY
091200970526     C                   END
091300970526     C     KEY           IFEQ      F12
091400970526     C                   MOVEL     '12'          FUNKEY
091500970526     C                   END
091600970526     C     KEY           IFEQ      F13
091700970526     C                   MOVEL     '13'          FUNKEY
091800970526     C                   END
091900970526     C     KEY           IFEQ      F14
092000970526     C                   MOVEL     '14'          FUNKEY
092100970526     C                   END
092200970526     C     KEY           IFEQ      F15
092300970526     C                   MOVEL     '15'          FUNKEY
092400970526     C                   END
092500970526     C     KEY           IFEQ      F16
092600970526     C                   MOVEL     '16'          FUNKEY
092700970526     C                   END
092800970526     C     KEY           IFEQ      F17
092900970526     C                   MOVEL     '17'          FUNKEY
093000970526     C                   END
093100970526     C     KEY           IFEQ      F18
093200970526     C                   MOVEL     '18'          FUNKEY
093300970526     C                   END
093400970526     C     KEY           IFEQ      F19
093500970526     C                   MOVEL     '19'          FUNKEY
093600970526     C                   END
093700970526     C     KEY           IFEQ      F20
093800970526     C                   MOVEL     '20'          FUNKEY
093900970526     C                   END
094000970526     C     KEY           IFEQ      F21
094100970526     C                   MOVEL     '21'          FUNKEY
094200970526     C                   END
094300970526     C     KEY           IFEQ      F22
094400970526     C                   MOVEL     '22'          FUNKEY
094500970526     C                   END
094600970526     C     KEY           IFEQ      F23
094700970526     C                   MOVEL     '23'          FUNKEY
094800970526     C                   END
094900970526     C     KEY           IFEQ      F24
095000970526     C                   MOVEL     '24'          FUNKEY
095100970526     C                   END
095200970526     C     KEY           IFEQ      ROLLUP
095300970526     C                   MOVEL     'RU'          FUNKEY
095400970526     C                   END
095500970526     C     KEY           IFEQ      HELP
095600970526     C                   MOVEL     'HP'          FUNKEY
095700970526     C                   END
095800970526     C*
095900970526     C                   ENDSR
096000970526     C*------------------------------------------------------------------------*
096100970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
096200970526     C*------------------------------------------------------------------------*
096300970526     C     FUNGES        BEGSR
096400970526     C*
096500970526     C* PULISCO CAMPI DI LAVORO
096600970526     C                   Z-ADD     0             W                 5 0
096700970526     C*
096800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
096900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
097000970526     C     1             DO        24            I
097100970526     C                   MOVEL     FUD(I)        FUDRIG
097200970526     C*
097300970526     C* PULIZIA FUNZIONI PRECEDENTI
097400970526     C                   MOVEL     *BLANKS       FUV(I)
097500970526     C*
097600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
097700970526     C* (SALTA F23 E F24)
097800970526     C     FUDATT        IFEQ      'S'
097900970526     C     FUDFUN        ANDNE     '23'
098000970526     C     FUDFUN        ANDNE     '24'
098100970526     C                   ADD       1             W
098200970526     C                   MOVEL     FUDDES        FUV(W)
098300970526     C                   END
098400970526     C                   END                                                    DO WHILE
098500970526     C*
098600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
098700970526     C* ALTRIMENTI DISATTIVO F24
098800970526     C     1             DO        24            I
098900970526     C                   MOVEL     FUD(I)        FUDRIG
099000970526     C*
099100970526     C     FUDFUN        IFEQ      '24'
099200970526     C     W             IFGT      6
099300970526     C                   MOVEL     'S'           FUDATT
099400970526     C                   ELSE
099500970526     C                   MOVEL     *BLANKS       FUDATT
099600970526     C                   END                                                    W GT 8
099700970526     C                   MOVEL     FUDRIG        FUD(I)
099800970526     C                   END
099900970526     C                   END                                                    DO WHILE
100000970526     C*
100100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
100200970526     C     W             DIV       6             I
100300970526     C                   MVR                     Z
100400970526     C*
100500970526     C     Z             IFNE      0
100600970526     C     I             ADD       1             Z
100700970526     C                   ELSE
100800970526     C                   Z-ADD     I             Z
100900970526     C                   END
101000970526     C*
101100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
101200970526     C                   ADD       1             FUNGRU
101300970526     C     FUNGRU        IFGT      Z
101400970526     C     FUNGRU        ORLT      1
101500970526     C                   Z-ADD     1             FUNGRU
101600970526     C                   END
101700970526     C*
101800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
101900970526     C     FUNGRU        IFEQ      1
102000970526     C                   MOVEL     FUV(1)        FUN001
102100970526     C                   MOVEL     FUV(2)        FUN002
102200970526     C                   MOVEL     FUV(3)        FUN003
102300970526     C                   MOVEL     FUV(4)        FUN004
102400970526     C                   MOVEL     FUV(5)        FUN005
102500970526     C                   MOVEL     FUV(6)        FUN006
102600970526     C                   END
102700970526     C*
102800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
102900970526     C     FUNGRU        IFEQ      2
103000970526     C                   MOVEL     FUV(7)        FUN001
103100970526     C                   MOVEL     FUV(8)        FUN002
103200970526     C                   MOVEL     FUV(9)        FUN003
103300970526     C                   MOVEL     FUV(10)       FUN004
103400970526     C                   MOVEL     FUV(11)       FUN005
103500970526     C                   MOVEL     FUV(12)       FUN006
103600970526     C                   END
103700970526     C*
103800970526     C* TERZO GRUPPO DI 6 FUNZIONI
103900970526     C     FUNGRU        IFEQ      3
104000970526     C                   MOVEL     FUV(13)       FUN001
104100970526     C                   MOVEL     FUV(14)       FUN002
104200970526     C                   MOVEL     FUV(15)       FUN003
104300970526     C                   MOVEL     FUV(16)       FUN004
104400970526     C                   MOVEL     FUV(17)       FUN005
104500970526     C                   MOVEL     FUV(18)       FUN006
104600970526     C                   END
104700970526     C*
104800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
104900970526     C     FUNGRU        IFEQ      4
105000970526     C                   MOVEL     FUV(19)       FUN001
105100970526     C                   MOVEL     FUV(20)       FUN002
105200970526     C                   MOVEL     FUV(21)       FUN003
105300970526     C                   MOVEL     FUV(22)       FUN004
105400970526     C                   MOVEL     *BLANKS       FUN005
105500970526     C                   MOVEL     *BLANKS       FUN006
105600970526     C                   END
105700970526     C*
105800970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
105900970526     C                   MOVEL     *BLANKS       FUN007
106000970526     C                   MOVEL     *BLANKS       FUN008
106100970526     C*
106200970526     C     1             DO        24            I
106300970526     C                   MOVEL     FUD(I)        FUDRIG
106400970526     C     FUDFUN        IFEQ      '23'
106500970526     C     FUDATT        ANDEQ     'S'
106600970526     C                   MOVEL     FUDDES        FUN007
106700970526     C                   END
106800970526     C     FUDFUN        IFEQ      '24'
106900970526     C     FUDATT        ANDEQ     'S'
107000970526     C                   MOVEL     FUDDES        FUN008
107100970526     C                   END
107200970526     C                   END                                                    DO
107300970526     C*
107400970526     C                   ENDSR
107500970526     C*------------------------------------------------------------------------*
107600970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
107700970526     C*------------------------------------------------------------------------*
107800970526     C     OPZGES        BEGSR
107900970526     C*
108000970526     C* PULISCO CAMPI DI LAVORO
108100970526     C                   Z-ADD     0             W                 5 0
108200970526     C*
108300970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
108400970526     C* E CARICO TABELLA OPZIONI ATTIVE
108500991115     C     1             DO        24            I
108600970526     C                   MOVEL     OPD(I)        OPDRIG
108700970526     C*
108800970526     C* PULIZIA OPZIONI PRECEDENTI
108900970526     C                   MOVEL     *BLANKS       OPV(I)
109000970526     C*
109100970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
109200970526     C     OPDATT        IFEQ      'S'
109300970526     C                   ADD       1             W
109400970526     C                   MOVEL     OPDDES        OPV(W)
109500970526     C                   END
109600970526     C                   END                                                    DO WHILE
109700970526     C*
109800970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
109900970526     C* ALTRIMENTI DISATTIVO F23
110000991115     C     1             DO        24            I
110100970526     C                   MOVEL     FUD(I)        FUDRIG
110200970526     C*
110300970526     C     FUDFUN        IFEQ      '23'
110400970526     C     W             IFGT      8
110500970526     C                   MOVEL     'S'           FUDATT
110600970526     C                   ELSE
110700970526     C                   MOVEL     *BLANKS       FUDATT
110800970526     C                   END                                                    W GT 8
110900970526     C                   MOVEL     FUDRIG        FUD(I)
111000970526     C                   END
111100970526     C                   END                                                    DO WHILE
111200970526     C*
111300970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
111400970526     C     W             DIV       8             I
111500970526     C                   MVR                     Z                 5 0
111600970526     C*
111700970526     C     Z             IFNE      0
111800970526     C     I             ADD       1             Z
111900970526     C                   ELSE
112000970526     C                   Z-ADD     I             Z
112100970526     C                   END
112200970526     C*
112300970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
112400970526     C                   ADD       1             OPZGRU
112500970526     C     OPZGRU        IFGT      Z
112600970526     C     OPZGRU        ORLT      1
112700970526     C                   Z-ADD     1             OPZGRU
112800970526     C                   END
112900970526     C*
113000970526     C* PRIMO GRUPPO DI 8 OPZIONI
113100970526     C     OPZGRU        IFEQ      1
113200970526     C                   MOVEL     OPV(1)        OPZ001
113300970526     C                   MOVEL     OPV(2)        OPZ002
113400970526     C                   MOVEL     OPV(3)        OPZ003
113500970526     C                   MOVEL     OPV(4)        OPZ004
113600970526     C                   MOVEL     OPV(5)        OPZ005
113700970526     C                   MOVEL     OPV(6)        OPZ006
113800970526     C                   MOVEL     OPV(7)        OPZ007
113900970526     C                   MOVEL     OPV(8)        OPZ008
114000970526     C                   END
114100970526     C*
114200970526     C* SECONDO GRUPPO DI 8 OPZIONI
114300970526     C     OPZGRU        IFEQ      2
114400970526     C                   MOVEL     OPV(9)        OPZ001
114500970526     C                   MOVEL     OPV(10)       OPZ002
114600970526     C                   MOVEL     OPV(11)       OPZ003
114700970526     C                   MOVEL     OPV(12)       OPZ004
114800970526     C                   MOVEL     OPV(13)       OPZ005
114900970526     C                   MOVEL     OPV(14)       OPZ006
115000970526     C                   MOVEL     OPV(15)       OPZ007
115100970526     C                   MOVEL     OPV(16)       OPZ008
115200970526     C                   END
115300970526     C*
115400970526     C* TERZO GRUPPO DI 8 OPZIONI
115500970526     C     OPZGRU        IFEQ      3
115600970526     C                   MOVEL     OPV(17)       OPZ001
115700970526     C                   MOVEL     OPV(18)       OPZ002
115800970526     C                   MOVEL     OPV(19)       OPZ003
115900970526     C                   MOVEL     OPV(20)       OPZ004
116000970526     C                   MOVEL     OPV(21)       OPZ005
116100970526     C                   MOVEL     OPV(22)       OPZ006
116200970526     C                   MOVEL     OPV(23)       OPZ007
116300970526     C                   MOVEL     OPV(24)       OPZ008
116400970526     C                   END
116500970526     C*
116600970526     C                   ENDSR
116700970526     C*------------------------------------------------------------------------*
116800970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
116900970526     C*------------------------------------------------------------------------*
117000970526     C     OPZCON        BEGSR
117100970526     C*
117200970526     C* PULISCO CAMPI DI LAVORO
117300970526     C                   Z-ADD     0             I                 5 0
117400970526     C                   Z-ADD     0             W                 5 0
117500970526     C*
117600970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
117700970526     C     1             DO        24            I
117800970526     C                   MOVEL     OPD(I)        OPDRIG
117900970526     C*
118000970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
118100970526     C     OPZKEY        IFEQ      OPDOPZ
118200970526     C     OPZKEY        OREQ      OPDOP1
118300970526     C*
118400970526     C     OPDATT        IFEQ      'S'
118500970526     C                   MOVEL     OPDOPZ        OPZKEY
118600970526     C                   Z-ADD     I             W
118700970526     C                   END
118800970526     C                   END
118900970526     C*
119000970526     C                   END                                                    DO WHILE
119100970526     C*
119200970526     C* OPZIONE NON ATTIVA (ERRORE)
119300970526     C     W             IFEQ      0
119400970526     C*
119500970526     C* OPZIONE NON OK
119600970526     C                   Z-ADD     1             OPZOK
119700970526     C*
119800970526     C* EMISSIONE MESSAGGIO DI ERRORE
119900970526     C                   MOVEL     MSG(1)        DSMSMS
120000970526     C                   ELSE
120100970526     C*
120200970526     C* OPZIONE ATTIVA (PROCEDI ...)
120300970526     C*
120400141211     C                   MOVEL     *BLANKS       pOut_Msg
120500030509     C     OPZKEY        IFEQ      ' 2'
120600030509     C                   EXSR      O02GES
120700030509     C                   END
120800141211     C                   MOVEL     pOut_Msg      DSMSMS
120900021211     C                   END                                                    OPZ.NON ATTIVA
121000970526     C*
121100970526     C                   ENDSR
121200970526     C*------------------------------------------------------------------------*
121300970526     C* NOTE COSTRUZIONE SCHIERE
121400970526     C*------------------------------------------------------------------------*
121500970526     C*
121600970526     C* -MSG-MESSAGGI
121700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
121800970526     C* (NON TOCCARE MSG,1 E MSG,2)
121900970526     C*
122000970526     C* -CMD-COMANDI
122100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
122200970526     C*
122300970526     C* -OPD-OPZIONI
122400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
122500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
122600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
122700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
122800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
122900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
123000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
123100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
123200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
123300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
123400970526     C*
123500970526     C* -FUD-FUNZIONI
123600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
123700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
123800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
123900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
124000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
124100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
124200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
124300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
124400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
124500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
124600991108     C*
124700991115     C* -STS-CODICI STATI DEI RECORD DI LOG
124800991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
124900991110     C* ================> IMPORTANTE <=================
125000991115     C* per codici di errore in -STS- indicare in -DES- sempre
125100991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
125200991110     C* ERR
125300991108     C*
125400970526     O*------------------------------------------------------------------------*
125500970526** DEC - DECODIFICHE
125600970526    GESTIONE                                                     1
125700000407 INTERROGAZIONE                                                  2
1258000305093
1259009705264
1260009705265
126100970526** MSG - MESSAGGI
126200141210TRTCMBR- Opzione non gestita.                                                  1
126300141210TRTCMBR- Tasto funzionale non gestito.                                         2
126400141210TRTCMBR- Non abilitato all inserimento in filiale.                             3
126500141210TRTCMBR- Utente non abilitato all'opzione richiesta.                           4
126600141210TRTCMBR- File di spool non trovato o inesistente, verificare.                  5
126700141210TRTCMBR- Specificare almeno 5 caratteri per la ricerca.                        6
126800141210TRTCMBR- Operazione non consentita su log ancora da elaborare.                 7
126900141210TRTCMBR- 8                                                                     8
127000141210TRTCMBR- 9                                                                     9
127100141210TRTCMBR- 10                                                                   10
127200141210TRTCMBR- 11                                                                   11
127300141210TRTCMBR- 12                                                                   12
127400141210TRTCMBR- 13                                                                   13
127500141210TRTCMBR- 14                                                                   14
127600141210TRTCMBR- 15                                                                   15
127700141210TRTCMBR- 16                                                                   16
127800141210TRTCMBR- 17                                                                   17
127900141210TRTCMBR- 18                                                                   18
128000141210TRTCMBR- 19                                                                   19
128100141210TRTCMBR- 20                                                                   20
128200970528** CMD - COMANDI CL
128300021211
128400021211
128500991110
128600970528
128700970528
128800970526** OPD - OPZIONI
128900030509                   -  -     (S/N)=N                        opz. 01¬
129000141210                   -  -     (S/N)=N                        opz. 02¬
129100030509                   -  -     (S/N)=N                        opz. 03¬
129200141210                   -  -     (S/N)=N                        opz. 04¬
129300141210                   -  -     (S/N)=N                        opz. 05¬
129400141210                   -  -     (S/N)=N                        opz. 06¬
129500021211                   -  -     (S/N)=N                        opz. 07¬
129600991110                   -  -     (S/N)=N                        opz. 08¬
129700991110                   -  -     (S/N)=N                        opz. 09¬
129800141210                   -  -     (S/N)=N                        opz. 10¬
129900141210                   -  -     (S/N)=N                        opz. 11¬
130000970526                   -  -     (S/N)=N                        opz. 12¬
130100970526                   -  -     (S/N)=N                        opz. 13¬
130200000407                   -  -     (S/N)=N                        opz. 14¬
130300141210                   -  -     (S/N)=N                        opz. 15¬
130400970526                   -  -     (S/N)=N                        opz. 16¬
130500970526                   -  -     (S/N)=N                        opz. 17¬
130600970526                   -  -     (S/N)=N                        opz. 18¬
130700970526                   -  -     (S/N)=N                        opz. 19¬
130800970526                   -  -     (S/N)=N                        opz. 20¬
130900970526                   -  -     (S/N)=N                        opz. 21¬
131000970526                   -  -     (S/N)=N                        opz. 22¬
131100970526                   -  -     (S/N)=N                        opz. 23¬
131200000407                   -  -     (S/N)=N                        opz. 25¬
131300970526** FUD - FUNZIONI
131400970526                   -        (S/N)=N                        funz.01¬
131500970526                   -        (S/N)=N                        funz.02¬
131600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
131700141210                   -        (S/N)=N                        funz.04¬
131800141210                   -        (S/N)=N                        funz.05¬
131900030509                   -        (S/N)=N                        funz.06¬
132000970526                   -        (S/N)=N                        funz.07¬
132100970527                   -        (S/N)=N                        funz.08¬
132200141210                   -        (S/N)=N                        funz.09¬
132300970527                   -        (S/N)=N                        funz.10¬
132400970526                   -        (S/N)=N                        funz.11¬
132500970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
132600991116                   -        (S/N)=N                        funz.13¬
132700970526                   -        (S/N)=N                        funz.14¬
132800970526                   -        (S/N)=N                        funz.15¬
132900970526                   -        (S/N)=N                        funz.16¬
133000970526                   -        (S/N)=N                        funz.17¬
133100970526                   -        (S/N)=N                        funz.18¬
133200970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
133300970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
133400000407                   -        (S/N)=N                        funz.21¬
133500141210                   -        (S/N)=N                        funz.22¬
133600991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
133700991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
