000100140123      *PARMS CLOSQLCSR(*ENDMOD)
000200050209      *PARMS DYNUSRPRF(*OWNER)
000300050209      *PARMS RDB(AS777)
000400050209      *PARMS USER(VAS777)
000500050209      *PARMS PASSWORD(wwwVAS)
000600050209      *PARMS DBGVIEW(*SOURCE)
000700970526     H*------------------------------------------------------------------------*
000800991116     H* Gestione file di log TIVTL01L - Strategi Track & Tracing
000900040927     H* RDB(AS777) USER(...) PASSWORD(...) DBGVIEW(*SOURCE) DYNUSRPRF(*OWNER)
001000090107      *
001100090107      *?           ATTENZIONE!!!!!!!!!       ?
001200090107      *?l'oggetto in OBJ è diverso da quello su SETRAS perchè il pgm va ?
001300141105      *?per forza compilato su Setras
001400130207      * passaggi per creare il pgm su SETRAS
001500130207      * 1) se modificato il DSPF, questo va compilato su AS888 in una libreria che esiste anche
001600130207      *    su SETRAS (GAITRAOBJ) e poi portato mediante SAVRSTOBJ sulla stessa
001700130207      * 2) controllare su SETRAS l'esistenza del DDM che punta alla DS TIVTL10:
001800130207      *    DSPDDMF TIVTL10F
001900130207      *    se non c'è, crearlo col cmd:
002000130207      *    CRTDDMF FILE(TIVTL10F) RMTFILE(STRATEBAR/TIVTL10F) RMTLOCNAME(as777 *IP)
002100130207      * 3) controllare su AS777 che l'utente con cui si accede al DB esista ancora
002200130207      * 4) eseguire su SETRAS il cmd CRTSQLRPGI con i parametri sopra elencati
002300161019      *    CRTSQLRPGI OBJ(GAITRAOBJ/TIS7GT12R) SRCFILE(GAITRASRC/SRCTIS7) SRCMBR(TIS7GT12R)
002400130218      *    RDB(AS777) USER(VAS777) PASSWORD() CLOSQLCSR(*ENDMOD) DYNUSRPRF(*OWNER)
002500161019      * 5) il cmd crea su AS777 l'SQLPKG TIS7GT12R per l'esecuzione dell'SQL
002600090107      *
002700970526     H*------------------------------------------------------------------------*
002800161019     H DECEDIT('0,') DATEDIT(*DMY/)
002900130207     h dftactgrp(*no)
003000130207     h bnddir('UBBNDDIR')
003100970526     F*------------------------------------------------------------------------*
003200970526     F* DATA BASE
003300970526     F*------------------------------------------------------------------------*
003400161012     FTIS7GT12D CF   E             WORKSTN INFDS(DEVDS1)
003500991221     F                                     SFILE(S7GTD05:SFLULT)
003600991221     F                                     SFILE(S7GTDMS:MSGULT)
003700040929
003800040929     D ConnectionAlreadyExists...
003900040929     D                 C                   -842
004000040929
004100970526     D*------------------------------------------------------------------------*
004200970526     D* SCHIERE
004300970526     D*------------------------------------------------------------------------*
004400970526     D*------------
004500970526     D* DECODIFICHE
004600970526     D*------------
004700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
004800970526     D*------------
004900970526     D* MESSAGGI
005000970526     D*------------
005100970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
005200970528     D*------------
005300970528     D* COMANDI
005400970528     D*------------
005500970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
005600970526     D*------------
005700970526     D* DESCRIZIONI OPZIONI
005800970526     D*------------
005900970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
006000970526     D*------------
006100970526     D* DESCRIZIONI TASTI FUNZIONALI
006200970526     D*------------
006300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
006400970526     D*------------
006500970526     D* OPZIONI DA CARICARE A VIDEO
006600970526     D*------------
006700970526     D OPV             S             19    DIM(24)
006800970526     D*------------
006900970526     D* FUNZIONI DA CARICARE A VIDEO
007000970526     D*------------
007100970527     D FUV             S             19    DIM(24)
007200970526     D*------------------------------------------------------------------------*
007300970526     D* INPUT
007400970526     D*------------------------------------------------------------------------*
007500970526     D*------------
007600970526     D* RIDEFINIZIONE OPZIONI VIDEO
007700970526     D*------------
007800970526     D OPDDS           DS
007900970526     D  OPDOPZ                 1      2
008000970526     D  OPDDE1                 3     19
008100970526     D  OPDOP1                21     22
008200970526     D  OPDFI0                23     34
008300970526     D  OPDATT                35     35
008400970526     D  OPDFI1                36     80
008500970526     D  OPDDES                 1     19
008600970526     D  OPDRIG                 1     80
008700970526     D*------------
008800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
008900970526     D*------------
009000970526     D FUDDS           DS
009100970526     D  FUDDE1                 1      1
009200970526     D  FUDFUN                 2      3
009300970526     D  FUDDE2                 4     19
009400970526     D  FUDFI0                20     34
009500970526     D  FUDATT                35     35
009600970526     D  FUDFI1                36     80
009700970526     D  FUDDES                 1     19
009800970526     D  FUDRIG                 1     80
009900970526     D*------------
010000970526     D* DS GESTIONE TASTI FUNZIONALI
010100970526     D*------------
010200970526     D DSKEY         E DS
010300970526     D DEVDS1          DS
010400970526     D  KEY                  369    369
010500970526     D*------------
010600970526     D* DS "XMSG" - GESTIONE MESSAGGI
010700970526     D*------------
010800970526     D DSMSG         E DS
010900970526     D*------------
011000020916     D* DS REPERIMENTO DATI UTENTE
011100970526     D*------------
011200020916     D TIBS34DS      E DS                                                       *Profili utente
011300020916     D DDATIUTE      E DS                                                       *Dati utente
011400020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
011500970527     D*------------------
011600991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
011700970527     D*------------------
011800970527     D WLBDA8          DS                  INZ
011900970527     D  G08DAT                 1      8  0
012000970527     D  G08INV                 9     16  0
012100970527     D  G08ERR                17     17
012200970527     D  G08TGI                18     22  0
012300970526     D*------------
012400991110     D* DS DI PROCEDURA
012500991116     D tis172        E DS                  EXTNAME(tis172dsi)
012600991116     D tis174        E DS                  EXTNAME(tis174dsi)
012700001103     D tis175        E DS                  EXTNAME(tis175dsi)
012800001114     D tis1W2        E DS                  EXTNAME(tis1W2ids)
012900030225     D tis1G2        E DS                  EXTNAME(tis1G2ids)
013000030225     D                                     PREFIX(g_)
013100970526     D*------------
013200970526     D* ARCHITETTURA
013300970526     D*------------
013400970526     D KPJBA         E DS
013500991108     D*------------------
013600991108     D* DS - CONTROLLO ED INVERSIONE VELOCE DATA (8)
013700991108     D*------------------
013800991108     D                 DS                  INZ
013900991108     D  GG                     1      2  0
014000991108     D  MM                     3      4  0
014100991108     D  AAAA                   5      8  0
014200991108     D  CHKDATA                1      8  0
014300991108     D*------------------
014400991108     D                 DS                  INZ
014500991108     D  AAINV                  1      4  0
014600991108     D  MMINV                  5      6  0
014700991108     D  GGINV                  7      8  0
014800991108     D  INVDATA                1      8  0
014900081229      *------------------
015000081229      * ds - controllo ora (6)
015100081229      *------------------
015200081229     d                 ds                  inz
015300081229     d  ora                    1      2  0
015400081229     d  min                    3      4  0
015500081229     d  sec                    5      6  0
015600081229     d  chkora                 1      6  0
015700991110     D*------------------
015800991110     D* DS - CONVERSIONE DATA IN FORMATO ALFA  (10)
015900991110     D*------------------
016000991110     D                 DS                  INZ
016100991110     D  GGA                    1      2  0
016200991110     D  SEP1                   3      3
016300991110     D  MMA                    4      5  0
016400991110     D  SEP2                   6      6
016500991110     D  AAAAA                  7     10  0
016600991110     D  DATAA                  1     10
016700991110     D*------------------
016800991110     D* DS - CONVERSIONE ORA IN FORMATO ALFA  (8)
016900991110     D*------------------
017000991110     D                 DS                  INZ
017100991110     D  HH                     1      2  0
017200991110     D  PP                     3      4  0
017300991110     D  SS                     5      6  0
017400991110     D  ORAN                   1      6  0
017500991110     D*------------------
017600991110     D                 DS                  INZ
017700991110     D  HHA                    1      2  0
017800991110     D  SEP3                   3      3
017900991110     D  PPA                    4      5  0
018000991110     D  SEP4                   6      6
018100991110     D  SSA                    7      8  0
018200991110     D  ORAA                   1      8
018300991108     D*------------------
018400991108     D* DS REPERIMENTO DATI UTENTE
018500991108     D*-------------------
018600991108     DBS69DS         E DS                  extname(TIBS69DS)
018700991108     DACODS          E DS                  extname(CNACO00F)
018800991108     DINDDS          E DS                  extname(CNIND00F)
018900991108     DCLPDS          E DS                  extname(CNCLP00F)
019000991108     DCLSDS          E DS                  extname(FNCLS00F)
019100040924     D TiVtl10F      E DS
019200040924     D                                     INZ
019300040924
019400161012     D wSQL            S           2048                                         SQL statement
019500040924     D                                     VARYING
019600161012
019700161012     D depDtD          S                   like(D03DtD)
019800161012     D depTmD          S                   like(D03TmD)
019900161012     D depDtA          S                   like(D03DtA)
020000161012     D depTmA          S                   like(D03TmA)
020100161012     D depRagg         S                   like(D03Ragg)
020200161013     D datiFetch       DS                  qualified
020300161012     D   Giorni                        d
020400161013     D   Num                          7s 0
020500161013     D colonna1SFL     DS                  dim(100) qualified
020600161013     D   Giorni                        d
020700161013     D   NSped                        7s 0
020800161013     D colonna2SFL     DS                  dim(100) qualified
020900161013     D   Giorni                        d
021000161017     D   NSpBC                        7s 0
021100161013     D colonna3SFL     DS                  dim(100) qualified
021200161013     D   Giorni                        d
021300161017     D   NSpBCL                       7s 0
021400161012
021500161012     D wColonna        S              1s 0
021600161013     D wI              S              3s 0
021700161012     D wTmStD          S               z
021800161012     D wTmStA          S               z
021900161013     D wTmStString     S             26a
022000161012     D DataErr         S              1s 0
022100161013     D DtDISO          s               d
022200161013     D TmDISO          s               t
022300161013     D DtAISO          s               d
022400161013     D TmAISO          s               t
022500161014     D wN              S              3s 0
022600161017     D wTotale         S               n
022700161013
022800161013     D DatCor          s              8s 0
022900130207
023000130207       // -?Reperimento NETA sistema AS/400 corrente?
023100130207     d currSysNeta     s              8a   inz
023200130207      /copy gaitrasrc/srcProtoPr,UBRTVNETA
023300040924
023400100107     C/EXEC SQL
023500100107     C+ SET OPTION COMMIT = *NONE
023600100107     C/END-EXEC
023700100107
023800970526     C*------------------------------------------------------------------------*
023900970526     C* MAIN LINES
024000970526     C*------------------------------------------------------------------------*
024100970526     C*
024200100107     C                   EVAL      *INLR = *ON
024300970526     C* SE NON CI SONO ERRORI, PROSEGUE
024400970526IF  1C     ERRL00        IFNE      '1'
024500970526     C*
024600970526     C* CARICA IL SFILE
024700970526     C                   EXSR      POSSFL
024800970526     C*
024900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
025000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
025100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
025200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
025300970526     C*
025400970526     C* REIMPOSTA ERRORE DI PROCERURA
025500970526     C                   MOVEL     '0'           ERRL00
025600970526     C*
025700970526     C* EMETTE IL SFILE
025800970526     C                   EXSR      OUTSFL
025900970526     C*
026000970526     C* GESTIONE TASTI FUNZIONALI
026100970526     C                   EXSR      FUNCON
026200970526     C*
026300970526E   2C                   ENDDO
026400970527     C*
026500970527     C* CI SONO ERRORI IN ENTRATA
026600970526E   1C                   ENDIF
026700991108     C*
026800991108     C                   EXSR      FINSR
026900970526     C* FINE PGM
027000040929     C                   RETURN
027100970526     C*------------------------------------------------------------------------*
027200970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
027300970526     C*------------------------------------------------------------------------*
027400970526     C     F03GES        BEGSR
027500970526     C*
027600970526     C* IMPOSTA "PREMUTO F03"
027700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
027800970526     C*
027900970526     C                   ENDSR
028000970526     C*------------------------------------------------------------------------*
028100970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
028200970526     C*------------------------------------------------------------------------*
028300970526     C     F05GES        BEGSR
028400970526     C*
028500970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
028600970526     C*
028700970526     C                   ENDSR
028800970526     C*------------------------------------------------------------------------*
028900970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
029000970526     C*------------------------------------------------------------------------*
029100970526     C     F06GES        BEGSR
029200970526     C*
029300991108     C* NON GESTITO
029400970526     C*
029500970526     C                   ENDSR
029600970526     C*------------------------------------------------------------------------*
029700991108     C* F12GES - GESTIONE F12 -> RITORNO
029800970526     C*------------------------------------------------------------------------*
029900970526     C     F12GES        BEGSR
030000970526     C*
030100970526     C* IMPOSTA "PREMUTO F12"
030200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
030300970526     C*
030400970526     C                   ENDSR
030500970526     C*------------------------------------------------------------------------*
030600970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
030700970526     C*------------------------------------------------------------------------*
030800970526     C     RUPGES        BEGSR
030900970526     C*
031000161017     C* caricando tutto il SFL subito, questa chiamata non serve
031100161017     C***                EXSR      CARSFL                                       *LEGGE DATABASE
031200970526     C*
031300970526     C                   ENDSR
031400970526     C*------------------------------------------------------------------------*
031500991108     C* ENTGES - GESTIONE ENTER -> INVIO
031600970526     C*------------------------------------------------------------------------*
031700970526     C     ENTGES        BEGSR
031800991108     C*
031900161012     C                   EVAL      DataErr = 0
032000161012     C*
032100991108     C*
032200991108     C     DATAERR       IFEQ      0
032300991108     C*
032400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
032500970526     C                   EXSR      NEWPOS
032600970526     C*
032700991108     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
032800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032900970526IF  2C     SWPOSI        IFNE      0
033000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
033100970526X   2C                   ELSE
033200970526IF  3C     SFLULT        IFNE      0
033300970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
033400970526     C*
033500970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
033600970526IF  4C     SWMODI        IFNE      0
033700970526     C                   EXSR      POSSFL
033800970526E   4C                   ENDIF
033900970526E   3C                   ENDIF
034000970526E   2C                   ENDIF
034100970526E   1C                   ENDIF
034200970526     C*
034300991108     C                   ENDIF
034400991108     C*
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
035500991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
035600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
035700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
035800970526     C                   Z-ADD     0             SWMODI            1 0
035900970530     C*
036000970530     C* REIMPOSTA INDICATORI DI ERRORE
036100991108     C                   SETOFF                                       808182
036200081229     C                   SETOFF                                       838485
036300970526     C*
036400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
036500161012     C     D03DtD        IFNE      depDtD
036600161012     C     D03TmD        ORNE      depTmD
036700161012     C     D03DtA        ORNE      depDtA
036800161012     C     D03TmA        ORNE      depTmA
036900161012     C     D03Ragg       ORNE      depRagg
037000970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
037100001103     C     ERRL00        IFEQ      '0'                                          *NO ERORI
037200970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
037300161012     C                   MOVEL     D03DtD        depDtD
037400161012     C                   MOVEL     D03TmD        depTmD
037500161012     C                   MOVEL     D03DtA        depDtA
037600161012     C                   MOVEL     D03TmA        depTmA
037700161012     C                   MOVEL     D03Ragg       depRagg
037800001103     C                   ENDIF
037900001103     C                   ENDIF
038000970526     C*
038100970526     C                   ENDSR
038200970526     C*------------------------------------------------------------------------*
038300970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
038400970526     C*------------------------------------------------------------------------*
038500991117     C     SETFIL        BEGSR
038600040924     C
038700161013     C* per ora c'è un solo caricamento da SQL
038800161013     C                   SELECT
038900161013IF  1C                   WHEN      WTipRi = 'SI '
039000161013     C* mi posiziono al primo elemento
039100161013     C                   EVAL      wI = 1
039200161014     C* se il primo elemento della colonna 1 è valorizzato, allora ci sono righe da scrivere
039300161014     C                   EVAL      *in99 = (Colonna1SFL(1).Giorni = *loval)
039400161014     C                   IF        *in99 = *off
039500161014     C                   EVAL      SFLEOF = 0
039600161014     C                   ELSE
039700161014     C                   EVAL      SFLEOF = 1
039800161014     C                   ENDIF
039900161013     C                   ENDSL
040000980107     C*
040100161014     C* MA PERCHÉ LO FA NELLA SETFIL LA LETTURA DEL RCD???
040200161014     C* LO TOLGO
040300161014     C*
040400970526     C* LETTURA RECORD
040500161014IF  1C***  *IN99         IFEQ      *OFF
040600161014     C***                EXSR      LETFIL
040700161014E   1C***                ENDIF
040800970526     C*
040900970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
041000970526IF  1C     *IN99         IFEQ      *ON
041100970526     C                   Z-ADD     1             SFLEOF
041200970526E   1C                   ENDIF
041300970526     C*
041400970526     C                   ENDSR
041500970526     C*------------------------------------------------------------------------*
041600970526     C* LETFIL - LETTURA RECORD
041700970526     C*------------------------------------------------------------------------*
041800970526     C     LETFIL        BEGSR
041900970526     C*
042000970526     C* LEGGE FINO A:
042100970526     C*     - FINE FILE
042200970526     C*     - TROVATO RECORD VALIDO
042300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
042400970526DO  1C     *IN99         DOWEQ     *OFF
042500970526     C     WRECOK        ANDEQ     'N'
042600970526     C*
042700970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
042800161013     C* per ora c'è un solo caricamento da SQL
042900161013     C                   SELECT
043000161013IF  1C                   WHEN      WTipRi = 'SI '
043100161013     C                   EXSR      LeggoSk
043200161013     C                   ENDSL
043300140123     C*
043400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
043600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
043700040924     C                   EXSR      CloseCursor
043800970530X   2C                   ELSE
043900970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
044000970530E   2C                   ENDIF
044100970526E   1C                   ENDDO
044200970526     C*
044300970526     C                   ENDSR
044400970526     C*------------------------------------------------------------------------*
044500970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
044600970526     C*------------------------------------------------------------------------*
044700161017     C     CHKREC        BEGSR
044800970526     C*
044900161017     C* se l'ordinamento SLF è quello dell'SQL
045000161017IF  1C                   IF        WTipRi = 'SI '
045100161012     C* leggendo con SQL, è già nello statement che faccio filtro
045200970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
045300161017     C* altrimenti il raggruppamento prevede di vedere solo il totale
045400161017     C* quindi se sono alla riga di totale
045500161017     C*                  IF        wTotale = *on
045600161017     C* considero il rcd valido
045700161017     C*                  MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
045800161017     C*                  ELSE
045900161017     C* altrimenti non lo considero valido
046000161017     C*                  MOVEL     'N'           WRECOK                         *VALIDITA' RECORD
046100161017     C*                  ENDIF
046200991108     C*
046300161017     C*                  ENDIF
046400161017     C*
046500161017     C                   ENDIF
046600161017     C*
046700970526     C                   ENDSR
046800970526     C*------------------------------------------------------------------------*
046900161013     C* IRESFL - IMPOSTA I CAMPI DEL SFILE
047000970526     C*------------------------------------------------------------------------*
047100970526     C     IRESFL        BEGSR
047200161013     C*
047300161013     C                   SELECT
047400161013     C                   WHEN      WTipRi = 'SI '
047500161017     C* se devo raggruppare per giorni
047600161017     C                   IF        D03Ragg = 'G'
047700161017     C* se devo scrivere le singole righe
047800161017     C                   IF        wTotale = *off
047900161019     C                   EVAL      D05ragg   = '  ' +
048000161019     C                              %char(
048100161019     C                              Colonna1SFL(SFLRig).Giorni : *dmy)
048200161013     C                   EVAL      D05NSped= Colonna1SFL(SFLRig).NSped
048300161013         //la prima DS contiene tutti i giorni in cui sono state interrogate le spedizioni
048400161013         //le altre no (difficile che non sia così ma non impossibile) per cui vanno cercate
048500161013         //le occorrenze con lo stesso giorno
048600161014     C                   EVAL      wN =
048700161014     C                                  %lookup(Colonna1SFL(SFLRIG).Giorni :
048800161017     C                                          Colonna2SFL(*).Giorni : 1 :
048900161014     C                                          %elem(Colonna2SFL))
049000161014     C                   IF        wN > 0
049100161014     C                   EVAL      D05NSpBC= Colonna2SFL(SFLRig).NSpBC
049200161017     C                   EVAL(H)   D05PSpBC= (D05NSpBC/D05NSped)*100
049300161014     C                   ELSE
049400161014     C                   EVAL      D05NSpBC= 0
049500161014     C                   EVAL      D05PSpBC= 0
049600161014     C                   ENDIF
049700161014     C                   EVAL      wN =
049800161014     C                                  %lookup(Colonna1SFL(SFLRIG).Giorni :
049900161017     C                                          Colonna3SFL(*).Giorni : 1 :
050000161014     C                                          %elem(Colonna2SFL))
050100161014     C                   IF        wN > 0
050200161014     C                   EVAL      D05NSpBCL= Colonna3SFL(SFLRig).NSpBCL
050300161017     C                   EVAL(H)   D05PSpBCL= (D05NSpBCL/D05NSpBC)*100
050400161014     C                   ELSE
050500161014     C                   EVAL      D05NSpBCL= 0
050600161014     C                   EVAL      D05PSpBCL= 0
050700161014     C                   ENDIF
050800161014     C                   EVAL      D05NSpBCS= D05NSpBC - D05NSpBCL
050900161017     C                   EVAL(H)   D05PSpBCS= (D05NSpBCS/D05NSpBC)*100
051000161017     C*
051100161017     C* incremento i totali
051200161017     C                   EVAL      D06NSped += D05NSped
051300161017     C                   EVAL      D06NSpBC += D05NSpBC
051400161017     C                   EVAL      D06NSpBCS+= D05NSpBCS
051500161017     C                   EVAL      D06NSpBCL+= D05NSpBCL
051600161017
051700161017     C* se devo scrivere il totale
051800161017     C                   ELSE
051900161017     C                   EVAL      D05Ragg = 'T O T A L E'
052000161017     C                   EVAL      D05NSped= D06NSped
052100161017     C                   EVAL      D05NSpBC= D06NSpBC
052200161017     C                   EVAL(H)   D05PSpBC= (D05NSpBC/D05NSped)*100
052300161017     C                   EVAL      D05NSpBCL= D06NSpBCL
052400161017     C                   EVAL(H)   D05PSpBCL= (D05NSpBCL/D05NSpBC)*100
052500161017     C                   EVAL      D05NSpBCS= D06NSpBC - D06NSpBCL
052600161017     C                   EVAL(H)   D05PSpBCS= (D05NSpBCS/D05NSpBC)*100
052700161017     C                   ENDIF
052800161017
052900161017     C* se devo raggruppare per tutto
053000161017     C                   ELSE
053100161017     C                   EVAL      D05Ragg = 'T O T A L E'
053200161017     C* sarà valorizzato solo il primo elemento delle schiere
053300161017     C                   EVAL      D05NSped= Colonna1SFL(1).NSped
053400161017     C                   EVAL      D05NSpBC= Colonna2SFL(1).NSpBC
053500161017     C                   EVAL(H)   D05PSpBC= (D05NSpBC/D05NSped)*100
053600161017     C                   EVAL      D05NSpBCL= Colonna3SFL(1).NSpBCL
053700161017     C                   EVAL(H)   D05PSpBCL= (D05NSpBCL/D05NSpBC)*100
053800161017     C                   EVAL      D05NSpBCS= D05NSpBC - D05NSpBCL
053900161017     C                   EVAL(H)   D05PSpBCS= (D05NSpBCS/D05NSpBC)*100
054000161017     C                   ENDIF
054100161017
054200161017     C                   ENDSL
054300161017     C*
054400970526     C                   ENDSR
054500001114     C*----------------------------------------------------*
054600001114     C*  CONTROLLO NUMERICITA' CAMPI
054700001114     C*----------------------------------------------------*
054800001114     C     CHKNUM        BEGSR
054900001114     C*
055000001114     C*
055100001114     C                   ENDSR
055200161013     C*------------------------------------------------------------------------*
055300161013     C* Inizializzo mappa dei parametri
055400161013     C*------------------------------------------------------------------------*
055500161013     C     InzD03        BEGSR
055600161013     C*
055700161013     C                   EVAL      D03DtD = 0
055800161013     C                   EVAL      D03TMD = 0
055900161013     C                   EVAL      D03DtA = 0
056000161013     C                   EVAL      D03TMA = 235959
056100161013     C                   EVAL      D03Ragg= 'G'
056200161014     C                   CLEAR                   Colonna1SFL
056300161014     C                   CLEAR                   Colonna2SFL
056400161014     C                   CLEAR                   Colonna3SFL
056500161013     C*
056600161013     C                   ENDSR
056700970526     C*------------------------------------------------------------------------*
056800991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
056900970526     C*------------------------------------------------------------------------*
057000970526     C     CHKPAR        BEGSR
057100970528     C*
057200161013     C* parto non caricando nulla
057300161013     C                   MOVEL     'NO '         WTIPRI            3
057400020708     C*
057500161013     C* controllo correttezza date DA/A
057600161013     C*
057700161013     C* CONTROLLA che la data DA  sia corretta formalmente
057800161013     C     ERRL00        IFEQ      '0'
057900161013     C                   IF        D03DtD   <> 0
058000161013     C                   MONITOR
058100161013     C* se il valore della data è > 6 cifre
058200161013     C                   IF        %len(%trim(%char(D03DtD  ))) > 6
058300161013     C* presumo sia scritta in ggmmaaaa
058400161013IF  5C                   EVAL      DtDISO = %date(D03DtD:*eur)
058500161013     C* se il valore della data è <= 6 cifre
058600161013     C                   ELSE
058700161013     C* presumo sia scritta in ggmmaa
058800161013IF  5C                   EVAL      DtDISO = %date(D03DtD:*dmy)
058900161013     C                   ENDIF
059000161013IF  2C                   ON-ERROR
059100161013     C                   MOVEL     '1'           ERRL00
059200161013     C                   SETON                                        81
059300161013     C                   MOVEL     MSG(12)       DSMSMS
059400161013     C                   LEAVESR
059500161013     C                   ENDMON
059600161013     C* rivalorizzo il video con la data in ggmmaaaa
059700161013     C                   EVAL      D03DtD = %dec(DtDISO:*eur)
059800161013     C                   ENDIF
059900161013     C                   ENDIF
060000161013     C*
060100161013     C* CONTROLLA che la data  A  sia corretta formalmente
060200161013     C     ERRL00        IFEQ      '0'
060300161013     C                   IF        D03DtA   <> 0
060400161013     C                   MONITOR
060500161013     C* se il valore della data è > 6 cifre
060600161013     C                   IF        %len(%trim(%char(D03DtA  ))) > 6
060700161013     C* presumo sia scritta in ggmmaaaa
060800161013IF  5C                   EVAL      DtAISO = %date(D03DtA:*eur)
060900161013     C* se il valore della data è <= 6 cifre
061000161013     C                   ELSE
061100161013     C* presumo sia scritta in ggmmaa
061200161013IF  5C                   EVAL      DtAISO = %date(D03DtA:*dmy)
061300161013     C                   ENDIF
061400161013IF  2C                   ON-ERROR
061500161013     C                   MOVEL     '1'           ERRL00
061600161013     C                   SETON                                        83
061700161013     C                   MOVEL     MSG(12)       DSMSMS
061800161013     C                   LEAVESR
061900161013     C                   ENDMON
062000161013     C* rivalorizzo il video con la data in ggmmaaaa
062100161013     C                   EVAL      D03DtA = %dec(DtAISO:*eur)
062200161013     C                   ENDIF
062300161013     C                   ENDIF
062400161013     C*
062500161013     C* CONTROLLA che la data DA  sia <= data A
062600161013     C     ERRL00        IFEQ      '0'
062700161013     C                   IF        DtDISO > DtAISO
062800161013     C                   SETON                                        8183
062900161013     C                   MOVEL     MSG(14)       DSMSMS
063000161013     C                   LEAVESR
063100161013     C                   ENDIF
063200161013     C                   ENDIF
063300161013     C*
063400161013     C* CONTROLLA che l'ora DA  sia corretta formalmente
063500161013     C     ERRL00        IFEQ      '0'
063600161013     C                   MONITOR
063700161013     C                   EVAL      TmDISO = %time(D03TmD)
063800161013     C                   ON-ERROR
063900161013     C                   MOVEL     '1'           ERRL00
064000161013     C                   SETON                                        82
064100161013     C                   MOVEL     MSG(13)       DSMSMS
064200161013     C                   LEAVESR
064300161013     C                   ENDMON
064400161013     C                   ENDIF
064500161013     C*
064600161013     C* CONTROLLA che l'ora  A  sia corretta formalmente
064700161013     C     ERRL00        IFEQ      '0'
064800161013     C                   MONITOR
064900161013     C                   EVAL      TmAISO = %time(D03TmA)
065000161013     C                   ON-ERROR
065100161013     C                   MOVEL     '1'           ERRL00
065200161013     C                   SETON                                        84
065300161013     C                   MOVEL     MSG(13)       DSMSMS
065400161013     C                   LEAVESR
065500161013     C                   ENDMON
065600161013     C                   ENDIF
065700161013     C*
065800161013     C*
065900161013     C     ERRL00        IFEQ      '0'
066000161013     C                   IF        D03DtD   <> 0  and
066100161013     C                             D03DtA   <> 0
066200161013     C                   EVAL       wTmStString = %char(DtDISO) + '-' +
066300161013     C                                            %char(TmDISO) + '.000000'
066400161013     C                   EVAL      wTmStD =
066500161013     C                              %timestamp(wTmStString)
066600161013     C                   EVAL       wTmStString = %char(DtAISO) + '-' +
066700161013     C                                            %char(TmAISO) + '.000000'
066800161013     C                   EVAL      wTmStA =
066900161013     C                              %timestamp(wTmStString)
067000161013     C                   ENDIF
067100161013     C                   ENDIF
067200161013     C*
067300161013IF  1C     ERRL00        IFEQ      '0'
067400161013     C                   SELECT
067500161013     C                   WHEN      D03DtD > 0 and
067600161013     C                             D03DtA > 0
067700161013     C                   MOVEL     'SI '         WTIPRI
067800161013     C* carico le DS multiple da cui reperire i dati per il SFL mediante SQL
067900161013     C* in modo che il flusso del pgm sia inalterato, solo che legge dalle DS invece che da file
068000161013     C                   EXSR      CaricaSkDaSQL
068100161014     C* in modo che il flusso del pgm sia inalterato, solo che legge dalle DS invece che da file
068200161013     C                   ENDSL
068300161013E   1C                   ENDIF
068400161013     C*
068500970526     C                   ENDSR
068600970526     C*------------------------------------------------------------------------*
068700970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
068800970526     C*------------------------------------------------------------------------*
068900970526     C     CHKFOP        BEGSR
069000991112     C*
069100991112     C* OPZIONI
069200970626     C*
069300970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
069400991112IF  1C     DEPOP0        IFEQ      'P05'
069500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
069600970626     C                   MOVEL     'S'           OPDATT
069700970626     C                   MOVEL     OPDRIG        OPD(5)
069800970626E   1C                   ENDIF
069900970626     C*
070000970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
070100970626IF  1C     DEPOP0        IFEQ      'P02'
070200991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
070300991112     C                   MOVEL     'S'           OPDATT
070400991112     C                   MOVEL     OPDRIG        OPD(5)
070500970626E   1C                   ENDIF
070600970626     C*
070700970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
070800020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
070900991110     C                   MOVEL     FUD(1)        FUDRIG                         *NO INSERIMENTO
071000970626     C                   MOVEL     'N'           FUDATT
071100991110     C                   MOVEL     FUDRIG        FUD(1)
071200991110     C                   MOVEL     FUD(11)       FUDRIG                         *NO INSERIMENTO
071300991110     C                   MOVEL     'N'           FUDATT
071400991110     C                   MOVEL     FUDRIG        FUD(11)
071500970626E   1C                   ENDIF
071600970526     C*
071700970526     C* GESTIONE OPZIONI A VIDEO
071800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
071900970526     C                   EXSR      OPZGES
072000970526     C*
072100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
072200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
072300970526     C                   EXSR      FUNGES
072400970526     C*
072500970526     C                   ENDSR
072600991108     C*------------------------------------------------------------------------*
072700991108     C* FINSR - OPERAZIONI FINALI
072800991108     C*------------------------------------------------------------------------*
072900991108     C     FINSR         BEGSR
073000991108     C*
073100991108     C* CHIUDE I FILE DEI PGM CHIAMATI
073200991108     C                   CLEAR                   BS69DS
073300991108     C                   CLEAR                   ACODS
073400991108     C                   CLEAR                   INDDS
073500991108     C                   CLEAR                   CLPDS
073600991108     C                   CLEAR                   CLSDS
073700991108     C                   MOVEL     'C'           I69TLA
073800991108     C                   MOVEL     KNSIF         I69SIF
073900991108     C                   CALL      'TIBS69R'
074000991108     C                   PARM                    BS69DS
074100991108     C                   PARM                    ACODS
074200991108     C                   PARM                    INDDS
074300991108     C                   PARM                    CLPDS
074400991108     C                   PARM                    CLSDS
074500040924     C*
074600040924     C                   EXSR      CloseCursor
074700040928     C                   EXSR      ConnectReset
074800991108     C*
074900991108     C                   ENDSR
075000020916     C*--------------------------------------------------------------------------------------------*
075100020916     C* REPERISCE I DATI UTENTE
075200020916     C*--------------------------------------------------------------------------------------------*
075300020916     C     REPDATIUTE    BEGSR
075400020916     C*
075500020916     C* INIZIALIZZA VARIABILI DI WRK
075600020916     C                   CLEAR                   TIBS34DS
075700020916     C                   CLEAR                   AZUTEDS
075800020916     C                   CLEAR                   DDATIUTE
075900020916     C*
076000020916     C     *DTAARA       DEFINE    §azute        azuteds
076100020916     C     *DTAARA       DEFINE    §datiute      ddatiute
076200020916     C                   IN(E)     *DTAARA
076300020916if  1C                   IF        %Error
076400020916     c                   EVAL      I34Tla = 'L'
076500020916     C                   CALL      'TIBS34R'
076600020916     C                   PARM                    Tibs34Ds
076700020916     C                   IN        *DTAARA
076800020916e   1C                   ENDIF
076900020916     C*
077000020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
077100020916     C                   MOVEL(P)  RSUT          DSFIRS           20
077200020916     C*
077300020916     C                   ENDSR
077400970526     C*------------------------------------------------------------------------*
077500970526     C* *INZSR - OPERAZIONI INIZIALI
077600970526     C*------------------------------------------------------------------------*
077700970526     C     *INZSR        BEGSR
077800970526     C*---
077900970526     C* RICEVIMENTO PARAMETRI
078000970526     C*---
078100970526     C     *ENTRY        PLIST
078200970612     C                   PARM                    KPJBA
078300991117     C                   movel     kpjbu         DEPOP0            3
078400970526     C*
078500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
078600970526     C     XMSGPA        PLIST
078700970526     C                   PARM                    DSMSMS
078800970526     C                   PARM                    DSMSPG
078900970526     C                   PARM                    DSMSKE
079000970526     C                   PARM                    DSMSER
079100970526     C*
079200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
079300970526     C                   CALL      'XKEY'
079400970526     C                   PARM                    DSKEY
079500970526     C*---
079600970526     C* VARIABILI NON RIFERITE AL DATA BASE
079700970526     C*---
079800970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
079900970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
080000970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
080100970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
080200970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
080300970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
080400970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
080500970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
080600970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
080700970526     C*---
080800970526     C* VARIABILI DI CONTROLLO
080900970526     C*---
081000970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
081100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
081200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
081300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
081400970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
081500130218     C                   Z-ADD     14            SFLPAG                         *N°RIGHE X PAG
081600991110     C                   MOVEL     '/'           SEP1                           *VALIDITA' RECORD
081700991110     C                   MOVEL     '/'           SEP2                           *VALIDITA' RECORD
081800991110     C                   MOVEL     ':'           SEP3                           *VALIDITA' RECORD
081900991110     C                   MOVEL     ':'           SEP4                           *VALIDITA' RECORD
082000161013     C* il SFL è vuoto sino a lettura
082100161013     C                   Z-ADD     1             SFLEOF
082200970526     C*
082300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
082400161012     C                   MOVEL     'TIS7GT12R'   DSMSPG
082500161012     C                   MOVEL     'TIS7GT12R'   MSGQUE
082600970526     C                   MOVEL     *BLANKS       MSGKEY
082700970526     C                   MOVEL     *BLANKS       DSMSMS
082800970526     C                   MOVEL     *BLANKS       DSMSKE
082900970526     C                   Z-ADD     *ZEROS        DSMSER
083000970526     C*---
083100970526     C* VARIABILI RIFERITE AL DATA BASE
083200970526     C*---
083300991108     C*
083400970527     C* REPERIMENTO DATI UTENTE
083500020916     C                   EXSR      REPDATIUTE
083600970526     C*
083700970526     C* CALCOLA LA DATA CORRENTE
083800970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
083900970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
084000161013     C                   eval      datcor = %dec(%date() : *ISO)
084100970526     C*
084200161013     C* imposto il dft su D03
084300161013     C                   EXSR      InzD03
084400161013     C*
084500161013     C* IMPOSTA I PARAMETRI DI INGRESSO
084600970526     C                   EXSR      CHKPAR
084700991115     C*
084800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
084900970526     C                   EXSR      CHKFOP
085000970526     C*
085100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
085200970530IF  1C     DEPOP0        IFEQ      'P01'
085300970530     C                   MOVEL     DEC(3)        VIDING
085400970530E   1C                   ENDIF
085500970530IF  1C     DEPOP0        IFEQ      'P02'
085600970526     C                   MOVEL     DEC(1)        VIDING
085700970530E   1C                   ENDIF
085800970530IF  1C     DEPOP0        IFEQ      'P05'
085900970526     C                   MOVEL     DEC(2)        VIDING
086000970526E   1C                   ENDIF
086100970526     C*
086200130207     C* se sono su AS888 non mi connetto a 777 perché non ci si riesce
086300130207     C                   IF        UBRTVNETA_Rtv( currSysNeta )=*zeros
086400130207     C                   IF        %subst (
086500130207     C                                            currSysNeta
086600130207     C                                                  : 1 : 5 )<>'AS888'
086700040924     C                   EXSR      ConnectTo
086800130207     C                   ENDIF
086900130207     C                   ENDIF
087000040924     C*
087100970526     C                   ENDSR
087200970526     C*------------------------------------------------------------------------*
087300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
087400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
087500970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
087600970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
087700970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
087800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
087900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
088000970526     C*------------------------------------------------------------------------*
088100970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
088200970526     C*------------------------------------------------------------------------*
088300970526     C     POSSFL        BEGSR
088400970526     C*
088500970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
088600970526     C                   EXSR      CLRSFL
088700970526     C*
088800970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
088900970526     C                   EXSR      SETFIL
089000970526     C*
089100970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
089200970526     C     SFLEOF        IFEQ      0
089300970526     C                   EXSR      CARSFL
089400970526     C                   END
089500970526     C*
089600970526     C                   ENDSR
089700970526     C*------------------------------------------------------------------------*
089800970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
089900970526     C*------------------------------------------------------------------------*
090000970526     C     ENTATT        BEGSR
090100970526     C*
090200970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
090300970526     C                   Z-ADD     0             OPZOK             1 0
090400970526     C*
090500970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
090600970526     C                   Z-ADD     0             SFLVAR            5 0
090700970526     C*
090800970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
090900970526     C                   Z-ADD     0             SFLERR
091000970526     C*
091100970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
091200970526     C                   Z-ADD     SFLULT        WFLULT
091300970526     C*
091400970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
091500991221     C                   READC     S7GTD05                                98
091600970526     C     *IN98         DOWEQ     '0'
091700970526     C*
091800970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
091900970526     C                   ADD       1             SFLVAR
092000970526     C*
092100970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
092200970526     C     OPZOK         IFEQ      0
092300970526     C*
092400970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
092500970526     C     OPZKEY        IFNE      *BLANKS
092600970526     C                   EXSR      OPZCON
092700970526     C                   END
092800970526     C*
092900970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
093000970526     C*            2. PULIZIA SCELTA IN SFL
093100970526     C     OPZOK         IFEQ      0
093200970526     C                   MOVEL     '0'           *IN41
093300970526     C                   MOVEL     *BLANKS       OPZKEY
093400970526     C                   ELSE
093500970526     C*
093600970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
093700970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
093800970526     C                   MOVEL     '1'           *IN41
093900970526     C                   Z-ADD     SFLULT        SFLERR
094000970526     C                   END
094100970526     C                   ELSE
094200970526     C*
094300970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
094400970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
094500970526     C                   MOVEL     '1'           *IN41
094600970526     C                   END                                                    OPZOK=0
094700970526     C*
094800970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
094900970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
095000970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
095100970526     C     OPZOK         IFNE      0
095200970526     C                   Z-ADD     SFLERR        SFLNBR
095300970526     C                   ELSE
095400970526     C                   Z-ADD     SFLULT        SFLNBR
095500970526     C                   END
095600970526     C*
095700970526     C* AGGIORNO RECORD SFL
095800991221     C                   UPDATE    S7GTD05
095900970526     C*
096000991221     C                   READC     S7GTD05                                98
096100970526     C                   END                                                    DO WHILE
096200970526     C*
096300970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
096400970526     C                   Z-ADD     WFLULT        SFLULT
096500970526     C*
096600970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
096700970526     C     SFLVAR        IFEQ      0
096800970526     C*---                 MOVEL'1'       FINL00
096900970526     C                   END
097000970526     C*
097100991112     C*
097200970526     C                   ENDSR
097300970526     C*------------------------------------------------------------------------*
097400970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
097500970526     C*------------------------------------------------------------------------*
097600970526     C     CLRSFL        BEGSR
097700970526     C*
097800970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
097900970526     C* (0=NON FINE FILE  1=FINE FILE)
098000161013     C***                Z-ADD     0             SFLEOF
098100970526     C*
098200970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
098300970526     C                   Z-ADD     0             SFLULT
098400970526     C*
098500970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
098600970526     C                   MOVEL     '1'           *IN40
098700991221     C                   WRITE     S7GTD06
098800970526     C                   MOVEL     '0'           *IN40
098900970526     C*
099000970526     C                   ENDSR
099100970526     C*------------------------------------------------------------------------*
099200970526     C* CARSFL - CARICAMENTO SFL
099300970526     C*------------------------------------------------------------------------*
099400970526     C     CARSFL        BEGSR
099500970526     C*
099600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
099700050124     C***  SFLULT        IFEQ      0
099800050124     C***                EXSR      SETFIL
099900050124     C***                END
100000970526     C*
100100970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
100200970526     C                   Z-ADD     SFLULT        WFLULT
100300970526     C*
100400970526     C* CICLO:
100500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
100600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
100700970526     C                   Z-ADD     1             SFLRIG
100800161014     C*
100900161014     C* LETTURA FILE DA CARICARE
101000161014     C                   EXSR      LETFIL
101100161014     C*
101200161013     C***  SFLRIG        DOWLE     SFLPAG
101300161013     C***  SFLEOF        ANDEQ     0
101400161013      // il SFL lo carico tutto, non a pagine
101500161013     C     SFLEOF        DOWEQ     0
101600970526     C*
101700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
101800991116     C                   MOVEL     *BLANKS       OPZKEY
101900970526     C*
102000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
102100970526     C                   EXSR      IRESFL
102200970526     C*
102300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
102400970526     C                   ADD       1             SFLULT
102500970526     C*
102600970526     C* SCRITTURA RECORD SFL
102700991221     C                   WRITE     S7GTD05
102800970526     C                   ADD       1             SFLRIG
102900970526     C*
103000970526     C* LETTURA FILE DA CARICARE
103100970526     C                   EXSR      LETFIL
103200970526     C                   END                                                    DO WHILE
103300970526     C*
103400970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
103500970526     C     WFLULT        IFNE      SFLULT
103600970526     C                   Z-ADD     WFLULT        SFLNBR
103700970526     C                   ADD       1             SFLNBR
103800970526     C                   END
103900970526     C*
104000970526     C                   ENDSR
104100970526     C*------------------------------------------------------------------------*
104200970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
104300970526     C*------------------------------------------------------------------------*
104400970526     C     OUTSFL        BEGSR
104500970526     C*
104600970526     C* WRITE RECORD TESTATA VIDEO
104700991221     C                   WRITE     S7GTD01
104800970526     C*
104900970526     C* VISUALIZZAZIONE MESSAGGIO
105000970526     C                   EXSR      SNDMSG
105100970526     C*
105200970526     C* WRITE RECORD OPZIONI VIDEO
105300991221     C                   WRITE     S7GTD02
105400970526     C*
105500970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
105600991221     C                   WRITE     S7GTD03
105700970526     C*
105800970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
105900991221     C                   WRITE     S7GTD04
106000970526     C*
106100970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
106200970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
106300970526     C     SFLEOF        IFEQ      0
106400970526     C                   MOVEL     '0'           *IN42
106500970526     C                   ELSE
106600970526     C*
106700970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
106800970526     C                   MOVEL     '1'           *IN42
106900970526     C                   END
107000970526     C*
107100970526     C* WRITE RECORD TASTI FUNZIONALI
107200991221     C                   WRITE     S7GTD09
107300970526     C*
107400970526     C* CASO DI SFL VUOTO
107500970526     C     SFLULT        IFLE      0
107600970526     C*
107700970526     C*    - WRITE RECORD VUOTO
107800991221     C                   WRITE     S7GTD0A
107900991221     C                   READ      S7GTD0A                                99
108000970526     C*
108100970526     C                   ELSE
108200970526     C*
108300970526     C* CASO DI SFL CON RECORDS
108400970526     C*
108500970526     C*    - WRITE RECORD CONTROLLO SFL
108600991221     C                   WRITE     S7GTD06
108700991221     C                   READ      S7GTD06                                99
108800970526     C                   END
108900970526     C*
109000970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
109100991221     C                   READ      S7GTD03                                99
109200970526     C*
109300970526     C                   ENDSR
109400970526     C*------------------------------------------------------------------------*
109500970526     C* SNDMSG - MANDA MESSAGGIO
109600970526     C*------------------------------------------------------------------------*
109700970526     C     SNDMSG        BEGSR
109800970526     C*
109900970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
110000970526     C                   MOVEL     '1'           *IN40
110100991221     C                   WRITE     S7GTDMC
110200970526     C                   MOVEL     '0'           *IN40
110300970526     C*
110400970526     C* VISUALIZZAZIONE MESSAGGIO
110500970526     C                   Z-ADD     0             MSGULT
110600970526     C                   CALL      'XMSG'        XMSGPA
110700970526     C     DSMSER        IFEQ      0
110800970526     C                   MOVEL     DSMSKE        MSGKEY
110900970526     C                   END
111000970526     C                   Z-ADD     1             MSGULT
111100991221     C                   WRITE     S7GTDMS                                      SFL MESS
111200991221     C                   WRITE     S7GTDMC                                      CONTR MESS
111300970526     C*
111400970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
111500970526     C                   MOVEL     *BLANKS       DSMSMS
111600970526     C*
111700970526     C                   ENDSR
111800130207     C*------------------------------------------------------------------------*
111900130207     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
112000130207     C*------------------------------------------------------------------------*
112100130207     C     FUNCON        BEGSR
112200130207     C*
112300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
112400970526     C                   EXSR      FUNVAR
112500970526     C*
112600970526     C* PULISCO CAMPI DI LAVORO
112700970526     C                   Z-ADD     0             I                 5 0
112800970526     C                   Z-ADD     0             W                 5 0
112900970526     C*
113000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
113100970526     C     FUNKEY        IFNE      *BLANKS
113200970526     C     1             DO        24            I
113300970526     C                   MOVEL     FUD(I)        FUDRIG
113400970526     C*
113500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
113600970526     C     FUNKEY        IFEQ      FUDFUN
113700970526     C     FUDATT        ANDEQ     'S'
113800970526     C                   Z-ADD     I             W
113900970526     C                   END
114000970526     C                   END                                                    DO WHILE
114100970526     C*
114200970526     C* FUNZIONE NON ATTIVA (ERRORE)
114300970526     C     W             IFEQ      0
114400970526     C*
114500970526     C* EMISSIONE MESSAGGIO DI ERRORE
114600970526     C                   MOVEL     MSG(2)        DSMSMS
114700970526     C                   END                                                    W=0
114800970526     C                   END                                                    FUNKEY NE *BLANKS
114900970526     C*
115000970526     C* FUNZIONE ATTIVA (PROCEDI ...)
115100970526     C     W             IFNE      0
115200970526     C     FUNKEY        OREQ      *BLANKS
115300970526     C*
115400970526     C     FUNKEY        IFEQ      *BLANKS
115500970526     C                   EXSR      ENTGES
115600970526     C                   END
115700991116     C     FUNKEY        IFEQ      '03'
115800991116     C                   EXSR      F03GES
115900991116     C                   END
116000970526     C     FUNKEY        IFEQ      '05'
116100970526     C                   EXSR      F05GES
116200970526     C                   END
116300991116     C     FUNKEY        IFEQ      '12'
116400991116     C                   EXSR      F12GES
116500991116     C                   END
116600970526     C     FUNKEY        IFEQ      'RU'
116700970526     C                   EXSR      RUPGES
116800970526     C                   END
116900970526     C*
117000970526     C                   END
117100970526     C*
117200970526     C                   ENDSR
117300970526     C*------------------------------------------------------------------------*
117400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
117500970526     C*------------------------------------------------------------------------*
117600970526     C     FUNVAR        BEGSR
117700970526     C*
117800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
117900970526     C     KEY           IFEQ      ENTER
118000970526     C                   MOVEL     *BLANKS       FUNKEY
118100970526     C                   END
118200970526     C     KEY           IFEQ      F1
118300970526     C                   MOVEL     '01'          FUNKEY
118400970526     C                   END
118500970526     C     KEY           IFEQ      F2
118600970526     C                   MOVEL     '02'          FUNKEY
118700970526     C                   END
118800970526     C     KEY           IFEQ      F3
118900970526     C                   MOVEL     '03'          FUNKEY
119000970526     C                   END
119100970526     C     KEY           IFEQ      F4
119200970526     C                   MOVEL     '04'          FUNKEY
119300970526     C                   END
119400970526     C     KEY           IFEQ      F5
119500970526     C                   MOVEL     '05'          FUNKEY
119600970526     C                   END
119700970526     C     KEY           IFEQ      F6
119800970526     C                   MOVEL     '06'          FUNKEY
119900970526     C                   END
120000970526     C     KEY           IFEQ      F7
120100970526     C                   MOVEL     '07'          FUNKEY
120200970526     C                   END
120300970526     C     KEY           IFEQ      F8
120400970526     C                   MOVEL     '08'          FUNKEY
120500970526     C                   END
120600970526     C     KEY           IFEQ      F9
120700970526     C                   MOVEL     '09'          FUNKEY
120800970526     C                   END
120900970526     C     KEY           IFEQ      F10
121000970526     C                   MOVEL     '10'          FUNKEY
121100970526     C                   END
121200970526     C     KEY           IFEQ      F11
121300970526     C                   MOVEL     '11'          FUNKEY
121400970526     C                   END
121500970526     C     KEY           IFEQ      F12
121600970526     C                   MOVEL     '12'          FUNKEY
121700970526     C                   END
121800970526     C     KEY           IFEQ      F13
121900970526     C                   MOVEL     '13'          FUNKEY
122000970526     C                   END
122100970526     C     KEY           IFEQ      F14
122200970526     C                   MOVEL     '14'          FUNKEY
122300970526     C                   END
122400970526     C     KEY           IFEQ      F15
122500970526     C                   MOVEL     '15'          FUNKEY
122600970526     C                   END
122700970526     C     KEY           IFEQ      F16
122800970526     C                   MOVEL     '16'          FUNKEY
122900970526     C                   END
123000970526     C     KEY           IFEQ      F17
123100970526     C                   MOVEL     '17'          FUNKEY
123200970526     C                   END
123300970526     C     KEY           IFEQ      F18
123400970526     C                   MOVEL     '18'          FUNKEY
123500970526     C                   END
123600970526     C     KEY           IFEQ      F19
123700970526     C                   MOVEL     '19'          FUNKEY
123800970526     C                   END
123900970526     C     KEY           IFEQ      F20
124000970526     C                   MOVEL     '20'          FUNKEY
124100970526     C                   END
124200970526     C     KEY           IFEQ      F21
124300970526     C                   MOVEL     '21'          FUNKEY
124400970526     C                   END
124500970526     C     KEY           IFEQ      F22
124600970526     C                   MOVEL     '22'          FUNKEY
124700970526     C                   END
124800970526     C     KEY           IFEQ      F23
124900970526     C                   MOVEL     '23'          FUNKEY
125000970526     C                   END
125100970526     C     KEY           IFEQ      F24
125200970526     C                   MOVEL     '24'          FUNKEY
125300970526     C                   END
125400970526     C     KEY           IFEQ      ROLLUP
125500970526     C                   MOVEL     'RU'          FUNKEY
125600970526     C                   END
125700970526     C     KEY           IFEQ      HELP
125800970526     C                   MOVEL     'HP'          FUNKEY
125900970526     C                   END
126000970526     C*
126100970526     C                   ENDSR
126200970526     C*------------------------------------------------------------------------*
126300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
126400970526     C*------------------------------------------------------------------------*
126500970526     C     FUNGES        BEGSR
126600970526     C*
126700970526     C* PULISCO CAMPI DI LAVORO
126800970526     C                   Z-ADD     0             W                 5 0
126900970526     C*
127000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
127100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
127200970526     C     1             DO        24            I
127300970526     C                   MOVEL     FUD(I)        FUDRIG
127400970526     C*
127500970526     C* PULIZIA FUNZIONI PRECEDENTI
127600970526     C                   MOVEL     *BLANKS       FUV(I)
127700970526     C*
127800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
127900970526     C* (SALTA F23 E F24)
128000970526     C     FUDATT        IFEQ      'S'
128100970526     C     FUDFUN        ANDNE     '23'
128200970526     C     FUDFUN        ANDNE     '24'
128300970526     C                   ADD       1             W
128400970526     C                   MOVEL     FUDDES        FUV(W)
128500970526     C                   END
128600970526     C                   END                                                    DO WHILE
128700970526     C*
128800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
128900970526     C* ALTRIMENTI DISATTIVO F24
129000970526     C     1             DO        24            I
129100970526     C                   MOVEL     FUD(I)        FUDRIG
129200970526     C*
129300970526     C     FUDFUN        IFEQ      '24'
129400970526     C     W             IFGT      6
129500970526     C                   MOVEL     'S'           FUDATT
129600970526     C                   ELSE
129700970526     C                   MOVEL     *BLANKS       FUDATT
129800970526     C                   END                                                    W GT 8
129900970526     C                   MOVEL     FUDRIG        FUD(I)
130000970526     C                   END
130100970526     C                   END                                                    DO WHILE
130200970526     C*
130300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
130400970526     C     W             DIV       6             I
130500970526     C                   MVR                     Z
130600970526     C*
130700970526     C     Z             IFNE      0
130800970526     C     I             ADD       1             Z
130900970526     C                   ELSE
131000970526     C                   Z-ADD     I             Z
131100970526     C                   END
131200970526     C*
131300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
131400970526     C                   ADD       1             FUNGRU
131500970526     C     FUNGRU        IFGT      Z
131600970526     C     FUNGRU        ORLT      1
131700970526     C                   Z-ADD     1             FUNGRU
131800970526     C                   END
131900970526     C*
132000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
132100970526     C     FUNGRU        IFEQ      1
132200970526     C                   MOVEL     FUV(1)        FUN001
132300970526     C                   MOVEL     FUV(2)        FUN002
132400970526     C                   MOVEL     FUV(3)        FUN003
132500970526     C                   MOVEL     FUV(4)        FUN004
132600970526     C                   MOVEL     FUV(5)        FUN005
132700970526     C                   MOVEL     FUV(6)        FUN006
132800970526     C                   END
132900970526     C*
133000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
133100970526     C     FUNGRU        IFEQ      2
133200970526     C                   MOVEL     FUV(7)        FUN001
133300970526     C                   MOVEL     FUV(8)        FUN002
133400970526     C                   MOVEL     FUV(9)        FUN003
133500970526     C                   MOVEL     FUV(10)       FUN004
133600970526     C                   MOVEL     FUV(11)       FUN005
133700970526     C                   MOVEL     FUV(12)       FUN006
133800970526     C                   END
133900970526     C*
134000970526     C* TERZO GRUPPO DI 6 FUNZIONI
134100970526     C     FUNGRU        IFEQ      3
134200970526     C                   MOVEL     FUV(13)       FUN001
134300970526     C                   MOVEL     FUV(14)       FUN002
134400970526     C                   MOVEL     FUV(15)       FUN003
134500970526     C                   MOVEL     FUV(16)       FUN004
134600970526     C                   MOVEL     FUV(17)       FUN005
134700970526     C                   MOVEL     FUV(18)       FUN006
134800970526     C                   END
134900970526     C*
135000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
135100970526     C     FUNGRU        IFEQ      4
135200970526     C                   MOVEL     FUV(19)       FUN001
135300970526     C                   MOVEL     FUV(20)       FUN002
135400970526     C                   MOVEL     FUV(21)       FUN003
135500970526     C                   MOVEL     FUV(22)       FUN004
135600970526     C                   MOVEL     *BLANKS       FUN005
135700970526     C                   MOVEL     *BLANKS       FUN006
135800970526     C                   END
135900970526     C*
136000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
136100970526     C                   MOVEL     *BLANKS       FUN007
136200970526     C                   MOVEL     *BLANKS       FUN008
136300970526     C*
136400970526     C     1             DO        24            I
136500970526     C                   MOVEL     FUD(I)        FUDRIG
136600970526     C     FUDFUN        IFEQ      '23'
136700970526     C     FUDATT        ANDEQ     'S'
136800970526     C                   MOVEL     FUDDES        FUN007
136900970526     C                   END
137000970526     C     FUDFUN        IFEQ      '24'
137100970526     C     FUDATT        ANDEQ     'S'
137200970526     C                   MOVEL     FUDDES        FUN008
137300970526     C                   END
137400970526     C                   END                                                    DO
137500970526     C*
137600970526     C                   ENDSR
137700970526     C*------------------------------------------------------------------------*
137800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
137900970526     C*------------------------------------------------------------------------*
138000970526     C     OPZGES        BEGSR
138100970526     C*
138200970526     C* PULISCO CAMPI DI LAVORO
138300970526     C                   Z-ADD     0             W                 5 0
138400970526     C*
138500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
138600970526     C* E CARICO TABELLA OPZIONI ATTIVE
138700991115     C     1             DO        24            I
138800970526     C                   MOVEL     OPD(I)        OPDRIG
138900970526     C*
139000970526     C* PULIZIA OPZIONI PRECEDENTI
139100970526     C                   MOVEL     *BLANKS       OPV(I)
139200970526     C*
139300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
139400970526     C     OPDATT        IFEQ      'S'
139500970526     C                   ADD       1             W
139600970526     C                   MOVEL     OPDDES        OPV(W)
139700970526     C                   END
139800970526     C                   END                                                    DO WHILE
139900970526     C*
140000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
140100970526     C* ALTRIMENTI DISATTIVO F23
140200991115     C     1             DO        24            I
140300970526     C                   MOVEL     FUD(I)        FUDRIG
140400970526     C*
140500970526     C     FUDFUN        IFEQ      '23'
140600970526     C     W             IFGT      8
140700970526     C                   MOVEL     'S'           FUDATT
140800970526     C                   ELSE
140900970526     C                   MOVEL     *BLANKS       FUDATT
141000970526     C                   END                                                    W GT 8
141100970526     C                   MOVEL     FUDRIG        FUD(I)
141200970526     C                   END
141300970526     C                   END                                                    DO WHILE
141400970526     C*
141500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
141600970526     C     W             DIV       8             I
141700970526     C                   MVR                     Z                 5 0
141800970526     C*
141900970526     C     Z             IFNE      0
142000970526     C     I             ADD       1             Z
142100970526     C                   ELSE
142200970526     C                   Z-ADD     I             Z
142300970526     C                   END
142400970526     C*
142500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
142600970526     C                   ADD       1             OPZGRU
142700970526     C     OPZGRU        IFGT      Z
142800970526     C     OPZGRU        ORLT      1
142900970526     C                   Z-ADD     1             OPZGRU
143000970526     C                   END
143100970526     C*
143200970526     C* PRIMO GRUPPO DI 8 OPZIONI
143300970526     C     OPZGRU        IFEQ      1
143400970526     C                   MOVEL     OPV(1)        OPZ001
143500970526     C                   MOVEL     OPV(2)        OPZ002
143600970526     C                   MOVEL     OPV(3)        OPZ003
143700970526     C                   MOVEL     OPV(4)        OPZ004
143800970526     C                   MOVEL     OPV(5)        OPZ005
143900970526     C                   MOVEL     OPV(6)        OPZ006
144000970526     C                   MOVEL     OPV(7)        OPZ007
144100970526     C                   MOVEL     OPV(8)        OPZ008
144200970526     C                   END
144300970526     C*
144400970526     C* SECONDO GRUPPO DI 8 OPZIONI
144500970526     C     OPZGRU        IFEQ      2
144600970526     C                   MOVEL     OPV(9)        OPZ001
144700970526     C                   MOVEL     OPV(10)       OPZ002
144800970526     C                   MOVEL     OPV(11)       OPZ003
144900970526     C                   MOVEL     OPV(12)       OPZ004
145000970526     C                   MOVEL     OPV(13)       OPZ005
145100970526     C                   MOVEL     OPV(14)       OPZ006
145200970526     C                   MOVEL     OPV(15)       OPZ007
145300970526     C                   MOVEL     OPV(16)       OPZ008
145400970526     C                   END
145500970526     C*
145600970526     C* TERZO GRUPPO DI 8 OPZIONI
145700970526     C     OPZGRU        IFEQ      3
145800970526     C                   MOVEL     OPV(17)       OPZ001
145900970526     C                   MOVEL     OPV(18)       OPZ002
146000970526     C                   MOVEL     OPV(19)       OPZ003
146100970526     C                   MOVEL     OPV(20)       OPZ004
146200970526     C                   MOVEL     OPV(21)       OPZ005
146300970526     C                   MOVEL     OPV(22)       OPZ006
146400970526     C                   MOVEL     OPV(23)       OPZ007
146500970526     C                   MOVEL     OPV(24)       OPZ008
146600970526     C                   END
146700970526     C*
146800970526     C                   ENDSR
146900970526     C*------------------------------------------------------------------------*
147000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
147100970526     C*------------------------------------------------------------------------*
147200970526     C     OPZCON        BEGSR
147300970526     C*
147400970526     C* PULISCO CAMPI DI LAVORO
147500970526     C                   Z-ADD     0             I                 5 0
147600970526     C                   Z-ADD     0             W                 5 0
147700970526     C*
147800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
147900970526     C     1             DO        24            I
148000970526     C                   MOVEL     OPD(I)        OPDRIG
148100970526     C*
148200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
148300970526     C     OPZKEY        IFEQ      OPDOPZ
148400970526     C     OPZKEY        OREQ      OPDOP1
148500970526     C*
148600970526     C     OPDATT        IFEQ      'S'
148700970526     C                   MOVEL     OPDOPZ        OPZKEY
148800970526     C                   Z-ADD     I             W
148900970526     C                   END
149000970526     C                   END
149100970526     C*
149200970526     C                   END                                                    DO WHILE
149300970526     C*
149400970526     C* OPZIONE NON ATTIVA (ERRORE)
149500970526     C     W             IFEQ      0
149600970526     C*
149700970526     C* OPZIONE NON OK
149800970526     C                   Z-ADD     1             OPZOK
149900970526     C*
150000970526     C* EMISSIONE MESSAGGIO DI ERRORE
150100970526     C                   MOVEL     MSG(1)        DSMSMS
150200970526     C                   ELSE
150300970526     C*
150400970526     C* OPZIONE ATTIVA (PROCEDI ...)
150500970526     C*
150600970526     C     OPZKEY        IFEQ      ' 5'
150700161012     C***                EXSR      O05GES
150800970526     C                   END
150900040927     C     OPZKEY        IFEQ      '15'
151000161012     C***                EXSR      O15GES
151100040927     C                   END
151200970526     C                   END                                                    OPZ.NON ATTIVA
151300130207     C*
151400970526     C                   ENDSR
151500040924
151600040924     ***********************************************************************************************
151700040924     **?
151800040924     **?Connessione al server web.
151900040924     **?
152000040924     ***********************************************************************************************
152100040924     C     ConnectTo     BEGSR
152200040929
152300040924     C/EXEC SQL
152400040924     C+ CONNECT TO AS777
152500040924     C/END-EXEC
152600100107     C                   IF        SQLCod = ConnectionAlreadyExists
152700100107     C/EXEC SQL
152800100107     C+ SET CONNECTION AS777
152900100107     C/END-EXEC
153000100107     C                   ELSEIF    SQLCod < 0
153100161012     C                   EVAL      DataErr = 1
153200161012     C                   EVAL      DSMSMS = MSG(3)
153300040929     C                   ENDIF
153400040924
153500040924     C                   ENDSR
153600040924
153700040924     ***********************************************************************************************
153800040924     **?
153900161012     ** Gli SQL da eseguire sono 3:
154000161012     ** - il primo estrae le spedizioni interrogate in tutti i giorni del periodo scelto
154100161012     ** - il secondo estrae le spedizioni interrogate mediante BRTCode in tutti i giorni del periodo
154200161012     **   scelto
154300161012     ** - il terzo estrae le spedizioni interrogate mediante BRTCode da link in tutti i giorni del
154400161012     **   periodo scelto
154500161012     ** Con questi 3 SQL riempio delle DS con occorrenze, in cui una colonna è sempre il giorno.
154600040924     **?
154700040924     ***********************************************************************************************
154800161013     C     CaricaSkDaSQL BEGSR
154900040924
155000161017     C                   CLEAR                   Colonna1SFL
155100161017     C                   CLEAR                   Colonna2SFL
155200161017     C                   CLEAR                   Colonna3SFL
155300161017     C                   EVAL      D06NSped  = 0
155400161017     C                   EVAL      D06NSpBC  = 0
155500161017     C                   EVAL      D06NSpBCS = 0
155600161017     C                   EVAL      D06NSpBCL = 0
155700161017
155800161013      // gli sql da eseguire sono 3 per riempire le 3 colonne
1559001610131f   C                   FOR       wColonna = 1 TO 3
156000161013
156100161013     ** Chiudo il cursore se già aperto.
156200161013     ** siccome gli SQL da eseguire sono 3 con lo stesso cursore, questo va chiuso ogni volta
156300161013     C                   IF        %LEN(wSQL) > 0
156400161013     C                   EXSR      CloseCursor
156500161013     C                   ENDIF
156600161013
156700161013      // estraggo il numero spedizioni interrogate
1568001610132s   C                   select
1569001610132w   C                   when      wColonna = 1
157000161018     C                   EVAL      wSQL =
157100161018     C                             'SELECT'
157200161018     C                   IF        D03Ragg = 'T'
157300161018     C                   EVAL      wSQL = %trim(wSQL) +
157400161018     C                             ' CURDATE(), +
157500161018     C                              SUM(CONTEGGIO_BOLLA) +
157600161018     C                              FROM ( +
157700161018     C                              SELECT'
157800161018     C                   ENDIF
157900161012     C                   EVAL      wSQL = %trim(wSQL) +
158000161012     C                             ' DATE(VTLRQSMMT) AS GIORNO, +
158100161012     C                              COUNT(DISTINCT SUBSTR(VTLRQSDTA, 12, 12) +
158200161012     C                              CONCAT SUBSTR(VTLRQSDTA, 41, 35)) +
158300161012     C                              AS CONTEGGIO_BOLLA +
158400161012     C                              FROM'
1585001610133s   C                   SELECT
1586001610133w   C                   WHEN      %subst(currSysNetA : 1 : 5) <> 'AS888'
158700161012     C                   EVAL      wSQL = %trim(wSQL) +
158800161012     C                             ' STRATEBAR/TIVTL10F'
1589001610133o   C                   OTHER
159000161012     C                   EVAL      wSQL = %trim(wSQL) +
159100161012     C                             ' TIVTL10F'
1592001610133e   C                   ENDSL
159300161012     C                   EVAL      wSQL = %trim(wSQL) +
159400161018     C                             ' WHERE +
159500161012     C                              VTLTIP = ''DB'' +
159600161012     C                              AND VTLRQSOPC = ''GETBOLLA'' +
159700161012     C                              AND LEFT(VTLSVRNAME, 7) <> ''CUSSINI'' +
159800161012     C                              AND VTLRQSMMT BETWEEN ''' +
159900161012     C                              %char(wTmStD) + ''' AND ''' +
160000161017     C                              %char(wTmStA) + ''' AND VTLSUN = 0 +
160100161017     C                              GROUP BY DATE(VTLRQSMMT)'
160200161018     C                   IF        D03Ragg = 'T'
160300161018     C                   EVAL      wSQL = %trim(wSQL) +
160400161018     C                             ') AS I'
160500161018     C                   ENDIF
160600161013
160700161013      // estraggo il numero spedizioni interrogate per BRTCode
1608001610132w   C                   when      wColonna = 2
160900161018     C                   EVAL      wSQL =
161000161018     C                             'SELECT'
161100161018     C                   IF        D03Ragg = 'T'
161200161018     C                   EVAL      wSQL = %trim(wSQL) +
161300161018     C                             ' CURDATE(), +
161400161018     C                              SUM(CONTEGGIO_BRTCODE) +
161500161018     C                              FROM ( +
161600161018     C                              SELECT'
161700161018     C                   ENDIF
161800161013     C                   EVAL      wSQL = %trim(wSQL) +
161900161013     C                             ' DATE(VTLRQSMMT) AS GIORNO, +
162000161013     C                              COUNT(DISTINCT SUBSTR(VTLRQSDTA, 75, 19)) +
162100161013     C                              AS CONTEGGIO_BRTCODE +
162200161013     C                              FROM'
1623001610133s   C                   SELECT
1624001610133w   C                   WHEN      %subst(currSysNetA : 1 : 5) <> 'AS888'
162500161013     C                   EVAL      wSQL = %trim(wSQL) +
162600161013     C                             ' STRATEBAR/TIVTL10F'
1627001610133o   C                   OTHER
162800161013     C                   EVAL      wSQL = %trim(wSQL) +
162900161018     C                             ' TIVTL10F'
1630001610133e   C                   ENDSL
163100161013     C                   EVAL      wSQL = %trim(wSQL) +
163200161013     C                             ' WHERE +
163300161013     C                              VTLTIP = ''IC'' +
163400161013     C                              AND VTLRQSOPC = ''GETISTRUCO'' +
163500161013     C                              AND LEFT(VTLSVRNAME, 7) <> ''CUSSINI'' +
163600161013     C                              AND SUBSTR(VTLRQSDTA , 75 , 19) <> '' '' +
163700161013     C                              AND VTLSUN = 0 +
163800161013     C                              AND VTLRQSMMT BETWEEN ''' +
163900161013     C                              %char(wTmStD) + ''' AND ''' +
164000161017     C                              %char(wTmStA) + ''' +
164100161017     C                               GROUP BY DATE(VTLRQSMMT)'
164200161018     C                   IF        D03Ragg = 'T'
164300161018     C                   EVAL      wSQL = %trim(wSQL) +
164400161018     C                             ') AS I'
164500161018     C                   ENDIF
164600161013
164700161013      // estraggo il numero spedizioni interrogate per BRTCode da link
1648001610132w   C                   when      wColonna = 3
164900161018     C                   EVAL      wSQL =
165000161018     C                             'SELECT'
165100161018     C                   IF        D03Ragg = 'T'
165200161018     C                   EVAL      wSQL = %trim(wSQL) +
165300161018     C                             ' CURDATE(), +
165400161018     C                              SUM(CONTEGGIO_BOLLE) +
165500161018     C                              FROM ( +
165600161018     C                              SELECT'
165700161018     C                   ENDIF
165800161013     C                   EVAL      wSQL = %trim(wSQL) +
165900161013     C                             ' DATE(VTLRQSMMT) AS GIORNO, +
166000161013     C                              COUNT(DISTINCT SUBSTR(VTLRQSDTA, 12, 12)) +
166100161013     C                              AS CONTEGGIO_BOLLE +
166200161013     C                              FROM'
1663001610133s   C                   SELECT
1664001610133w   C                   WHEN      %subst(currSysNetA : 1 : 5) <> 'AS888'
166500161013     C                   EVAL      wSQL = %trim(wSQL) +
166600161013     C                             ' STRATEBAR/TIVTL10F'
1667001610133o   C                   OTHER
166800161013     C                   EVAL      wSQL = %trim(wSQL) +
166900161013     C                             ' TIVTL10F'
1670001610133e   C                   ENDSL
167100161013     C                   EVAL      wSQL = %trim(wSQL) +
167200161013     C                             ' WHERE +
167300161013     C                              VTLTIP = ''DB'' +
167400161013     C                              AND VTLRQSOPC = ''GETBOLLA'' +
167500161013     C                              AND LEFT(VTLSVRNAME, 7) <> ''CUSSINI'' +
167600161013     C                              AND SUBSTR(VTLRQSDTA , 193 , 1) = ''a'' +
167700161013     C                              AND VTLRQSMMT BETWEEN ''' +
167800161013     C                              %char(wTmStD) + ''' AND ''' +
167900161017     C                              %char(wTmStA) + ''' +
168000161017     C                               GROUP BY DATE(VTLRQSMMT)'
168100161018     C                   IF        D03Ragg = 'T'
168200161018     C                   EVAL      wSQL = %trim(wSQL) +
168300161018     C                             ') AS I'
168400161018     C                   ENDIF
168500161017
1686001610132e   C                   ENDSL
168700040924
168800161012      // apro il primo cursore
168900161012     C                   EXSR      ExecSQL
169000161012
169100161012      // valorizzo la DS con occorrenze relativa alla colonna in esame
169200161012     C                   EXSR      FetchCursor
169300040924
1694001610131e   C                   ENDFOR
169500161013
169600161013     C                   ENDSR
169700040924
169800040924     ***********************************************************************************************
169900040924     **?
170000040924     **?
170100040924     **?
170200040924     ***********************************************************************************************
170300161012     C     ExecSQL       BEGSR
170400040924
170500161012     C/EXEC SQL
170600161012     C+ PREPARE S FROM :wSQL
170700161012     C/END-EXEC
170800161012
170900161012     C/EXEC SQL
171000161012     C+ DECLARE C CURSOR FOR S
171100161012     C/END-EXEC
171200161012
171300161012     C/EXEC SQL
171400161012     C+ OPEN C
171500161012     C/END-EXEC
171600161012
171700161012     C                   IF        SQLCod < 0
171800161012     C                   EVAL      DataErr = 1
171900161012     C                   EVAL      DSMSMS = MSG(3)
172000161012     C                   ENDIF
172100161012
172200161012     C                   ENDSR
172300161012
172400161012     ***********************************************************************************************
172500161013     **
172600161013     ** leggo il cursore
172700161013     **
172800161012     ***********************************************************************************************
172900161013      /free
173000161013
173100161013       begsr FetchCursor;
173200161012
173300161013         select;
173400161013          when wColonna = 1;
173500161013           wI = 0;
173600161013           DoU  SQLCode = 100 or SQLCode < 0;
173700161013             exec sql  fetch next  from C   into :datiFetch;
173800161013             // è possibile che non venga letto rcd
173900161013             // quindi non blocco il flusso
174000161013             select;
174100161013             // errore
174200161013             when SQLCode < 0;
174300161013               DataErr = 1;
174400161013               DSMSMS = MSG(3);
174500161013               // - Chiusura cursore (visto che esco con leavesr)
174600161013               exsr  CloseCursor;
174700161013               leavesr;
174800161013             // nessuna lettura
174900161013             when SQLCode = 100 and wI = 0;
175000161013               // non faccio nulla
175100161013             // lettura
175200161013             when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
175300161013               wI = wI + 1;
175400161013               Colonna1SFL(wI) = datiFetch;
175500161013             endsl;
175600161013           enddo;
175700161013
175800161013           // se ho caricato almeno un elemento, attivo il SFL
175900161013           if wI > 0;
176000161013             SFLEOF = 0;
176100161013             *in99 = *off;
176200161013           else;
176300161013             SFLEOF = 1;
176400161013             *in99 = *on;
176500161013           endif;
176600161013
176700161013          when wColonna = 2;
176800161013           wI = 0;
176900161013           DoU  SQLCode = 100 or SQLCode < 0;
177000161013             exec sql  fetch next  from C   into :datiFetch;
177100161013             // è possibile che non venga letto rcd
177200161013             // quindi non blocco il flusso
177300161013             select;
177400161013             // errore
177500161013             when SQLCode < 0;
177600161013               DataErr = 1;
177700161013               DSMSMS = MSG(3);
177800161013               // - Chiusura cursore (visto che esco con leavesr)
177900161013               exsr  CloseCursor;
178000161013               leavesr;
178100161013             // nessuna lettura
178200161013             when SQLCode = 100 and wI = 0;
178300161013               // non faccio nulla
178400161013             // lettura
178500161013             when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
178600161013               wI = wI + 1;
178700161013               Colonna2SFL(wI) = datiFetch;
178800161013
178900161013             other;
179000161013               // non faccio nulla
179100161013             endsl;
179200161013           EndDo;
179300161013
179400161013          when wColonna = 3;
179500161013           wI = 0;
179600161013           DoU  SQLCode = 100 or SQLCode < 0;
179700161013             exec sql  fetch next  from C   into :datiFetch;
179800161013             // è possibile che non venga letto rcd
179900161013             // quindi non blocco il flusso
180000161013             select;
180100161013             // errore
180200161013             when SQLCode < 0;
180300161013               DataErr = 1;
180400161013               DSMSMS = MSG(3);
180500161013               // - Chiusura cursore (visto che esco con leavesr)
180600161013               exsr  CloseCursor;
180700161013               leavesr;
180800161013             // nessuna lettura
180900161013             when SQLCode = 100 and wI = 0;
181000161013               // non faccio nulla
181100161013             // lettura
181200161013             when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
181300161013               wI = wI + 1;
181400161013               Colonna3SFL(wI) = datiFetch;
181500161013
181600161013             other;
181700161013               // non faccio nulla
181800161013             endsl;
181900161013           EndDo;
182000161013
182100161013         endsl;
182200161013
182300161013       endsr;
182400161013
182500161013     ***********************************************************************************************
182600161013     **
182700161013     ** leggo le DS multiple per valorizzare il SFL
182800161013     **
182900161013     ***********************************************************************************************
183000161013
183100161013       begsr leggoSk;
183200161013
183300161017         // se devo raggruppare per giorni
183400161017         // (se è stato richiesto il totale non devo emettere la riga di totale)
183500161017         if D03Ragg = 'G';
183600161017         // se è finita la colonna 1,
183700161017         *in99 = (Colonna1SFL(SFLRIG).Giorni = *loval);
183800161017         IF *in99 = *on;
183900161017           // se non ho ancora scritto il totale
184000161017           if wTotale = *off;
184100161017             // devo scrivere il totale
184200161017             *in99 = *off;
184300161017             EVAL SFLEOF = 0 ;
184400161017             wTotale = *on;
184500161017           // se ho già scritto il totale
184600161017           else;
184700161017             // devo uscire dal ciclo
184800161017             EVAL SFLEOF = 1 ;
184900161017             wTotale = *off;
185000161017           endif;
185100161017         ELSE;
185200161017           EVAL SFLEOF = 0 ;
185300161017           wTotale = *off;
185400161017         ENDIF;
185500161017         // se ho richiesto solo il totale
185600161017         else;
185700161017           // se è finita la colonna 1,
185800161017           *in99 = (Colonna1SFL(SFLRIG).Giorni = *loval);
185900161017           IF *in99 = *on;
186000161017             // devo uscire dal ciclo
186100161017             EVAL SFLEOF = 1 ;
186200161017             wTotale = *off;
186300161017           ENDIF;
186400161017         ENDIF;
186500161012
186600161013       endsr;
186700161013
186800161013      /end-free
186900040924
187000040924     ***********************************************************************************************
187100040924     **?
187200040929     **?Chiusura cursore.
187300040924     **?
187400040924     ***********************************************************************************************
187500040924     C     CloseCursor   BEGSR
187600040924
187700040924     C/EXEC SQL
187800161012     C+ CLOSE C
187900040924     C/END-EXEC
188000040924
188100040924     C                   ENDSR
188200040929
188300040929     ***********************************************************************************************
188400040929     **?
188500040929     **?Deconnessione dal server web.
188600040929     **?
188700040929     ***********************************************************************************************
188800040929     C     ConnectReset  BEGSR
188900040929
189000040929     C/EXEC SQL
189100040929     C+ DISCONNECT ALL
189200040929     C/END-EXEC
189300040929
189400040929     C/EXEC SQL
189500040929     C+ CONNECT RESET
189600040929     C/END-EXEC
189700040929
189800040929     C                   ENDSR
189900040924
190000970526     C*------------------------------------------------------------------------*
190100970526     C* NOTE COSTRUZIONE SCHIERE
190200970526     C*------------------------------------------------------------------------*
190300970526     C*
190400970526     C* -MSG-MESSAGGI
190500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
190600970526     C* (NON TOCCARE MSG,1 E MSG,2)
190700970526     C*
190800970526     C* -CMD-COMANDI
190900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
191000970526     C*
191100970526     C* -OPD-OPZIONI
191200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
191300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
191400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
191500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
191600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
191700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
191800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
191900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
192000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
192100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
192200970526     C*
192300970526     C* -FUD-FUNZIONI
192400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
192500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
192600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
192700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
192800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
192900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
193000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
193100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
193200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
193300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
193400991108     C*
193500991115     C* -STS-CODICI STATI DEI RECORD DI LOG
193600991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
193700991110     C* ================> IMPORTANTE <=================
193800991115     C* per codici di errore in -STS- indicare in -DES- sempre
193900991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
194000991110     C* ERR
194100991108     C*
194200970526     O*------------------------------------------------------------------------*
194300970526** DEC - DECODIFICHE
194400970526    GESTIONE                                                     1
194500161018 INTERROGAZIONE                                                  2
194600970530     SCELTA                                                      3
1947009705264
1948009705265
194900970526** MSG - MESSAGGI
195000161012TIS7GT12R- Opzione non gestita                                                 1
195100161012TIS7GT12R- Tasto funzionale non gestito                                        2
195200161012TIS7GT12R- Errore SQL nella FETCH                                              3
195300161012TIS7GT12R- 4
195400161012TIS7GT12R- 5
195500161012TIS7GT12R- 6
195600161012TIS7GT12R- 7                                                                   7
195700161012TIS7GT12R- 8                                                                   8
195800161012TIS7GT12R- 9                                                                   9
195900161012TIS7GT12R- 10                                                                 10
196000161012TIS7GT12R- 11
196100161012TIS7GT12R- Data errata                                                        12
196200161012TIS7GT12R- Ora errata                                                         13
196300161013TIS7GT12R- Data DA > Data A                                                   14
196400161012TIS7GT12R- 15                                                                 15
196500161012TIS7GT12R- 16                                                                 16
196600161012TIS7GT12R- 17                                                                 17
196700161012TIS7GT12R- 18                                                                 18
196800161012TIS7GT12R- 19                                                                 19
196900161012TIS7GT12R- 20                                                                 20
197000970528** CMD - COMANDI CL
197100991110DSPPFM FILE(
197200991110) MBR(
197300991110
197400970528
197500970528
197600970526** OPD - OPZIONI
197700991116                   -  -     (S/N)=N                        opz. 01¬
197800991110                   -  -     (S/N)=N                        opz. 02¬
197900991110                   -  -     (S/N)=N                        opz. 03¬
198000991116                   -  -     (S/N)=N                        opz. 04¬
198100161012                   -  -     (S/N)=N                        opz. 05¬
198200991115                   -  -     (S/N)=N                        opz. 06¬
198300991110                   -  -     (S/N)=N                        opz. 07¬
198400991110                   -  -     (S/N)=N                        opz. 08¬
198500991110                   -  -     (S/N)=N                        opz. 09¬
198600991110                   -  -     (S/N)=N                        opz. 10¬
198700991115                   -  -     (S/N)=N                        opz. 11¬
198800970526                   -  -     (S/N)=N                        opz. 12¬
198900970526                   -  -     (S/N)=N                        opz. 13¬
199000991116                   -  -     (S/N)=N                        opz. 14¬
199100161012                   -  -     (S/N)=N                        opz. 15¬
199200970526                   -  -     (S/N)=N                        opz. 16¬
199300970526                   -  -     (S/N)=N                        opz. 17¬
199400970526                   -  -     (S/N)=N                        opz. 18¬
199500970526                   -  -     (S/N)=N                        opz. 19¬
199600970526                   -  -     (S/N)=N                        opz. 20¬
199700970526                   -  -     (S/N)=N                        opz. 21¬
199800970526                   -  -     (S/N)=N                        opz. 22¬
199900970526                   -  -     (S/N)=N                        opz. 23¬
200000991116                   -  -     (S/N)=N                        opz. 25¬
200100970526** FUD - FUNZIONI
200200970526                   -        (S/N)=N                        funz.01¬
200300970526                   -        (S/N)=N                        funz.02¬
200400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
200500970526                   -        (S/N)=N                        funz.04¬
200600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
200700991108                   -        (S/N)=N                        funz.06¬
200800970526                   -        (S/N)=N                        funz.07¬
200900970527                   -        (S/N)=N                        funz.08¬
201000970527                   -        (S/N)=N                        funz.09¬
201100970527                   -        (S/N)=N                        funz.10¬
201200970526                   -        (S/N)=N                        funz.11¬
201300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
201400991116                   -        (S/N)=N                        funz.13¬
201500970526                   -        (S/N)=N                        funz.14¬
201600970526                   -        (S/N)=N                        funz.15¬
201700970526                   -        (S/N)=N                        funz.16¬
201800970526                   -        (S/N)=N                        funz.17¬
201900970526                   -        (S/N)=N                        funz.18¬
202000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
202100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
202200991116                   -        (S/N)=N                        funz.21¬
202300991116                   -        (S/N)=N                        funz.22¬
202400991116                   -        (S/N)=N  <== S-FISSO           funz.23¬
202500991116                   -        (S/N)=N  <== S-FISSO           funz.24¬
