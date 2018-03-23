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
002300130218      *    CRTSQLRPGI OBJ(GAITRAOBJ/TIS7GT10R) SRCFILE(GAITRASRC/SRCTIS7) SRCMBR(TIS7GT10R)
002400130218      *    RDB(AS777) USER(VAS777) PASSWORD() CLOSQLCSR(*ENDMOD) DYNUSRPRF(*OWNER)
002500130207      * 5) il cmd crea su AS777 l'SQLPKG TIS7GT10R per l'esecuzione dell'SQL
002600090107      *
002700970526     H*------------------------------------------------------------------------*
002800970526     H DECEDIT('0,') DATEDIT(*DMY.)
002900130207     h dftactgrp(*no)
003000130207     h bnddir('UBBNDDIR')
003100970526     F*------------------------------------------------------------------------*
003200970526     F* DATA BASE
003300970526     F*------------------------------------------------------------------------*
003400040927     FTIS7GT10D CF   E             WORKSTN INFDS(DEVDS1)
003500991221     F                                     SFILE(S7GTD05:SFLULT)
003600991221     F                                     SFILE(S7GTDMS:MSGULT)
003700130207     F                                     SFILE(S7GTD07:D07SFLULT)
003800970526     F*------------
003900040924     F*tivtl01l  IF   E           K DISK
004000040924     F*tivtl02l  IF   E           K DISK
004100040924     F*                                    RENAME(tivtl000:tivtlxxx)
004200040929
004300040929     D ConnectionAlreadyExists...
004400040929     D                 C                   -842
004500040929
004600970526     D*------------------------------------------------------------------------*
004700970526     D* SCHIERE
004800970526     D*------------------------------------------------------------------------*
004900970526     D*------------
005000970526     D* DECODIFICHE
005100970526     D*------------
005200970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
005300970526     D*------------
005400970526     D* MESSAGGI
005500970526     D*------------
005600970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
005700970528     D*------------
005800970528     D* COMANDI
005900970528     D*------------
006000970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
006100970526     D*------------
006200970526     D* DESCRIZIONI OPZIONI
006300970526     D*------------
006400970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
006500970526     D*------------
006600970526     D* DESCRIZIONI TASTI FUNZIONALI
006700970526     D*------------
006800970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
006900970526     D*------------
007000970526     D* OPZIONI DA CARICARE A VIDEO
007100970526     D*------------
007200970526     D OPV             S             19    DIM(24)
007300970526     D*------------
007400970526     D* FUNZIONI DA CARICARE A VIDEO
007500970526     D*------------
007600970527     D FUV             S             19    DIM(24)
007700991108     D*------------
007800991116     D* CODICI DI TIPO OPERAZIONI NEL LOG
007900991108     D*------------
008000141105     D TIP             S              2    DIM(17) CTDATA PERRCD(1) ASCEND
008100991108     D*------------
008200991108     D* DESCRIZIONI STATO DEI RECORD DI LOG
008300991108     D*------------
008400141105     D DES             S             25    DIM(17) CTDATA PERRCD(1)
008500130207     D*------------
008600130207     D* Codice e descrizione richieste
008700130207     D*------------
008800141118     D RQS             S             20    DIM(32) CTDATA PERRCD(1)
008900141118     D RQS_D           S             50    DIM(32) ALT(RQS)
009000130218     D RPY             S             20    DIM(14) CTDATA PERRCD(1)
009100130218     D RPY_D           S             50    DIM(14) ALT(RPY)
009200970526     D*------------------------------------------------------------------------*
009300970526     D* INPUT
009400970526     D*------------------------------------------------------------------------*
009500970526     D*------------
009600970526     D* RIDEFINIZIONE OPZIONI VIDEO
009700970526     D*------------
009800970526     D OPDDS           DS
009900970526     D  OPDOPZ                 1      2
010000970526     D  OPDDE1                 3     19
010100970526     D  OPDOP1                21     22
010200970526     D  OPDFI0                23     34
010300970526     D  OPDATT                35     35
010400970526     D  OPDFI1                36     80
010500970526     D  OPDDES                 1     19
010600970526     D  OPDRIG                 1     80
010700970526     D*------------
010800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
010900970526     D*------------
011000970526     D FUDDS           DS
011100970526     D  FUDDE1                 1      1
011200970526     D  FUDFUN                 2      3
011300970526     D  FUDDE2                 4     19
011400970526     D  FUDFI0                20     34
011500970526     D  FUDATT                35     35
011600970526     D  FUDFI1                36     80
011700970526     D  FUDDES                 1     19
011800970526     D  FUDRIG                 1     80
011900970526     D*------------
012000970526     D* DS GESTIONE TASTI FUNZIONALI
012100970526     D*------------
012200970526     D DSKEY         E DS
012300970526     D DEVDS1          DS
012400970526     D  KEY                  369    369
012500970526     D*------------
012600970526     D* DS "XMSG" - GESTIONE MESSAGGI
012700970526     D*------------
012800970526     D DSMSG         E DS
012900970526     D*------------
013000020916     D* DS REPERIMENTO DATI UTENTE
013100970526     D*------------
013200020916     D TIBS34DS      E DS                                                       *Profili utente
013300020916     D DDATIUTE      E DS                                                       *Dati utente
013400020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
013500970527     D*------------------
013600991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
013700970527     D*------------------
013800970527     D WLBDA8          DS                  INZ
013900970527     D  G08DAT                 1      8  0
014000970527     D  G08INV                 9     16  0
014100970527     D  G08ERR                17     17
014200970527     D  G08TGI                18     22  0
014300970526     D*------------
014400991110     D* DS DI PROCEDURA
014500991116     D tis172        E DS                  EXTNAME(tis172dsi)
014600991116     D tis174        E DS                  EXTNAME(tis174dsi)
014700001103     D tis175        E DS                  EXTNAME(tis175dsi)
014800001114     D tis1W2        E DS                  EXTNAME(tis1W2ids)
014900030225     D tis1G2        E DS                  EXTNAME(tis1G2ids)
015000030225     D                                     PREFIX(g_)
015100970526     D*------------
015200970526     D* ARCHITETTURA
015300970526     D*------------
015400970526     D KPJBA         E DS
015500991108     D*------------------
015600991108     D* DS - CONTROLLO ED INVERSIONE VELOCE DATA (8)
015700991108     D*------------------
015800991108     D                 DS                  INZ
015900991108     D  GG                     1      2  0
016000991108     D  MM                     3      4  0
016100991108     D  AAAA                   5      8  0
016200991108     D  CHKDATA                1      8  0
016300991108     D*------------------
016400991108     D                 DS                  INZ
016500991108     D  AAINV                  1      4  0
016600991108     D  MMINV                  5      6  0
016700991108     D  GGINV                  7      8  0
016800991108     D  INVDATA                1      8  0
016900081229      *------------------
017000081229      * ds - controllo ora (6)
017100081229      *------------------
017200081229     d                 ds                  inz
017300081229     d  ora                    1      2  0
017400081229     d  min                    3      4  0
017500081229     d  sec                    5      6  0
017600081229     d  chkora                 1      6  0
017700991110     D*------------------
017800991110     D* DS - CONVERSIONE DATA IN FORMATO ALFA  (10)
017900991110     D*------------------
018000991110     D                 DS                  INZ
018100991110     D  GGA                    1      2  0
018200991110     D  SEP1                   3      3
018300991110     D  MMA                    4      5  0
018400991110     D  SEP2                   6      6
018500991110     D  AAAAA                  7     10  0
018600991110     D  DATAA                  1     10
018700991110     D*------------------
018800991110     D* DS - CONVERSIONE ORA IN FORMATO ALFA  (8)
018900991110     D*------------------
019000991110     D                 DS                  INZ
019100991110     D  HH                     1      2  0
019200991110     D  PP                     3      4  0
019300991110     D  SS                     5      6  0
019400991110     D  ORAN                   1      6  0
019500991110     D*------------------
019600991110     D                 DS                  INZ
019700991110     D  HHA                    1      2  0
019800991110     D  SEP3                   3      3
019900991110     D  PPA                    4      5  0
020000991110     D  SEP4                   6      6
020100991110     D  SSA                    7      8  0
020200991110     D  ORAA                   1      8
020300991108     D*------------------
020400991108     D* DS REPERIMENTO DATI UTENTE
020500991108     D*-------------------
020600991108     DBS69DS         E DS                  extname(TIBS69DS)
020700991108     DACODS          E DS                  extname(CNACO00F)
020800991108     DINDDS          E DS                  extname(CNIND00F)
020900991108     DCLPDS          E DS                  extname(CNCLP00F)
021000991108     DCLSDS          E DS                  extname(FNCLS00F)
021100040924     D TiVtl10F      E DS
021200040924     D                                     INZ
021300040924
021400040924     D WrkRqsMmt1      S               Z                                        Momento iniziale
021500040924     D WrkRqsMmt2      S               Z                                        Momento finale
021600040924     D                                     INZ(*HIVAL)
021700040924     D WrkDatISO       S               D                                        Data *ISO
021800130218     D WrkOraISO       S               T                                        Ora *ISO
021900130218     D WrkOraHMSm      S             12                                         Ora HH.MM.SS.mmm
022000040924     D WrkSQLStm       S           2048                                         SQL statement
022100040924     D                                     VARYING
022200040924     D WrkSQLWhere     S           2048                                         SQL WHERE
022300040924     D                                     VARYING
022400040924     D                                     INZ('WHERE')
022500140123     D WrkSQLSELEZ     S           2048                                         SQL ORDER BY
022600040924     D                                     VARYING
022700140123     D                                     INZ(') select SELEZ.*, (select count-
022800140123     D                                     (*) from selez) from SELEZ ')
022900140123     D WrkSQLOrderBy   S           2048                                         SQL ORDER BY
023000140123     D                                     VARYING
023100140123     D                                     INZ('ORDER BY')
023200040927     D TastoUscita     S              1
023300130207     D SkScelta        S              3
023400130207     D wi              S              3s 0
023500130208     D totI            S              3s 0
023600140219     D wRQS            S             20
023700130207
023800130207       // -?Reperimento NETA sistema AS/400 corrente?
023900130207     d currSysNeta     s              8a   inz
024000130207      /copy gaitrasrc/srcProtoPr,UBRTVNETA
024100040924
024200100107     C/EXEC SQL
024300100107     C+ SET OPTION COMMIT = *NONE
024400100107     C/END-EXEC
024500100107
024600970526     C*------------------------------------------------------------------------*
024700970526     C* MAIN LINES
024800970526     C*------------------------------------------------------------------------*
024900970526     C*
025000100107     C                   EVAL      *INLR = *ON
025100970526     C* SE NON CI SONO ERRORI, PROSEGUE
025200970526IF  1C     ERRL00        IFNE      '1'
025300970526     C*
025400970526     C* CARICA IL SFILE
025500970526     C                   EXSR      POSSFL
025600970526     C*
025700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
025800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
025900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
026000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
026100970526     C*
026200970526     C* REIMPOSTA ERRORE DI PROCERURA
026300970526     C                   MOVEL     '0'           ERRL00
026400970526     C*
026500970526     C* EMETTE IL SFILE
026600970526     C                   EXSR      OUTSFL
026700970526     C*
026800970526     C* GESTIONE TASTI FUNZIONALI
026900970526     C                   EXSR      FUNCON
027000970526     C*
027100970526E   2C                   ENDDO
027200970527     C*
027300970527     C* CI SONO ERRORI IN ENTRATA
027400970526E   1C                   ENDIF
027500991108     C*
027600991108     C                   EXSR      FINSR
027700970526     C* FINE PGM
027800040929     C                   RETURN
027900040929     C*
028000040929     C     ErroreSQL     TAG
028100040929     C                   EVAL      *INH1 = *ON
028200100107     C                   EXSR      FINSR
028300970526     C*------------------------------------------------------------------------*
028400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
028500970526     C*------------------------------------------------------------------------*
028600970526     C     F03GES        BEGSR
028700970526     C*
028800970526     C* IMPOSTA "PREMUTO F03"
028900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
029000970526     C*
029100970526     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
029400970526     C*------------------------------------------------------------------------*
029500970526     C     F05GES        BEGSR
029600970526     C*
029700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
029800970526     C*
029900970526     C                   ENDSR
030000970526     C*------------------------------------------------------------------------*
030100970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
030200970526     C*------------------------------------------------------------------------*
030300970526     C     F06GES        BEGSR
030400970526     C*
030500991108     C* NON GESTITO
030600970526     C*
030700970526     C                   ENDSR
030800970526     C*------------------------------------------------------------------------*
030900991108     C* F12GES - GESTIONE F12 -> RITORNO
031000970526     C*------------------------------------------------------------------------*
031100970526     C     F12GES        BEGSR
031200970526     C*
031300970526     C* IMPOSTA "PREMUTO F12"
031400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
031500970526     C*
031600970526     C                   ENDSR
031700970526     C*------------------------------------------------------------------------*
031800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
031900970526     C*------------------------------------------------------------------------*
032000970526     C     RUPGES        BEGSR
032100970526     C*
032200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
032300970526     C*
032400970526     C                   ENDSR
032500970526     C*------------------------------------------------------------------------*
032600991108     C* ENTGES - GESTIONE ENTER -> INVIO
032700970526     C*------------------------------------------------------------------------*
032800970526     C     ENTGES        BEGSR
032900130207     C*
033000141105     C* se richiesta la scelta per "Op"
033100141105     C     '?'           SCAN      VIDTI1                                 98
033200130207     C                   IF        *in98 = *on
033300141105     C                   EVAL      SkScelta = 'TI1'
033400130207     C                   EXSR      SFLScelta
033500130207     C                   ENDIF
033600141105     C*
033700141105     C* se richiesta la scelta per "Richiesta"
033800141105     C     '?'           SCAN      VIDHS1                                 98
033900141105     C                   IF        *in98 = *on
034000141105     C                   EVAL      SkScelta = 'RQS'
034100141105     C                   EXSR      SFLScelta
034200141105     C                   ENDIF
034300130208     C*
034400130208     C* se richiesta la scelta per "Replay"
034500130208     C     '?'           SCAN      VIDRpyOpC                              98
034600130208     C                   IF        *in98 = *on
034700130208     C                   EVAL      SkScelta = 'RPY'
034800130208     C                   EXSR      SFLScelta
034900130208     C                   ENDIF
035000991108     C*
035100991108     C* CONTROLLO RAPIDO DATA (FORMATO GGMMAAAA) SE NO => ERRORE
035200991108     C*
035300040924     C                   RESET                   WrkRqsMmt1
035400040924     C                   RESET                   WrkRqsMmt2
035500991108     C                   Z-ADD     0             DATAERR           1 0
035600991108     C                   MOVEL     VIDdt2        CHKDATA
035700991108     C     CHKDATA       IFGT      0
035800991108     C     GG            IFLT      1
035900991108     C     GG            ORGT      31
036000991108     C                   Z-ADD     1             DATAERR
036100991108     C                   SETON                                        80
036200991108     C                   MOVEL     MSG(12)       DSMSMS
036300991108     C                   ENDIF
036400991108     C     MM            IFLT      1
036500991108     C     MM            ORGT      12
036600991108     C                   Z-ADD     1             DATAERR
036700991108     C                   SETON                                        80
036800991108     C                   MOVEL     MSG(12)       DSMSMS
036900991108     C                   ENDIF
037000040924     C                   IF        DataErr = 0
037100040924     C     *EUR          MOVE      VidDt2        WrkDatISO
037200040924     C                   MOVEL     WrkDatISO     WrkRqsMmt2
037300040924     C                   ENDIF
037400991108     C                   ENDIF
037500991108     C*
037600991108     C                   MOVEL     VIDdt1        CHKDATA
037700991108     C     CHKDATA       IFGT      0
037800991108     C     GG            IFLT      1
037900991108     C     GG            ORGT      31
038000991108     C                   Z-ADD     1             DATAERR
038100991108     C                   SETON                                        82
038200991108     C                   MOVEL     MSG(12)       DSMSMS
038300991108     C                   ENDIF
038400991108     C     MM            IFLT      1
038500991108     C     MM            ORGT      12
038600991108     C                   Z-ADD     1             DATAERR
038700991108     C                   SETON                                        82
038800991108     C                   MOVEL     MSG(12)       DSMSMS
038900991108     C                   ENDIF
039000040924     C                   IF        DataErr = 0
039100040924     C                   RESET                   WrkRqsMmt1
039200040924     C                   RESET                   WrkRqsMmt2
039300040924     C     *EUR          MOVE      VidDt1        WrkDatISO
039400040924     C                   MOVEL     WrkDatISO     WrkRqsMmt1
039500040924     C                   MOVEL     WrkDatISO     WrkRqsMmt2
039600040924     C                   ENDIF
039700991108     C                   ENDIF
039800081229      * se errore vado a fine della routine
039900081229     c                   if        dataerr <> 0
040000081229     c                   leavesr
040100081229     c                   endif
040200081229
040300081229      * se inserita l'ora deve esserci anche la data
040400081229     c                   if        vidor2 > 0 and viddt2 = 0
040500081229     c                   eval      *in80 = *on
040600081229     c                   eval      dsmsms = msg(12)
040700081229     c                   leavesr
040800081229     c                   endif
040900081229      * controllo ora (formato hhmmss)
041000081229     c                   if        vidor2 > 0
041100081229     c                   movel     vidor2        chkora
041200081229     c                   if        ora < 0 or ora > 23
041300081229     c                   eval      *in84 = *on
041400081229     c                   eval      dsmsms = msg(13)
041500081229     c                   leavesr
041600081229     c                   endif
041700081229     c                   if        min < 0 or min > 59
041800081229     c                   eval      *in84 = *on
041900081229     c                   eval      dsmsms = msg(13)
042000081229     c                   leavesr
042100081229     c                   endif
042200081229     c                   if        sec < 0 or sec > 59
042300081229     c                   eval      *in84 = *on
042400081229     c                   eval      dsmsms = msg(13)
042500081229     c                   leavesr
042600081229     c                   endif
042700081229     c     *eur          move      vidor2        wrkoraiso
042800081229     c                   move      wrkoraiso     wrkrqsmmt2
042900081229     c                   endif
043000991108     C*
043100991108     C     DATAERR       IFEQ      0
043200991108     C*
043300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
043400970526     C                   EXSR      NEWPOS
043500970526     C*
043600991108     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
043700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
043800970526IF  2C     SWPOSI        IFNE      0
043900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
044000970526X   2C                   ELSE
044100970526IF  3C     SFLULT        IFNE      0
044200970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
044300970526     C*
044400970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
044500970526IF  4C     SWMODI        IFNE      0
044600970526     C                   EXSR      POSSFL
044700970526E   4C                   ENDIF
044800970526E   3C                   ENDIF
044900970526E   2C                   ENDIF
045000970526E   1C                   ENDIF
045100970526     C*
045200991108     C                   ENDIF
045300991108     C*
045400970526     C                   ENDSR
045500970526     C*------------------------------------------------------------------------*
045600991110     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD DI LOG
045700970526     C*------------------------------------------------------------------------*
045800970526     C     O05GES        BEGSR
045900991110     C*
046000991116     C
046100040927     C                   SELECT
046200040927     C     VIDope        WHENEQ    'DB'
046300991221     C                   EXFMT     S7GTDWW1
046400040927     C     VIDope        WHENEQ    'RB'
046500991221     C                   EXFMT     S7GTDWW2
046600040927     C     VIDope        WHENEQ    'DM'
046700001103     C                   EXFMT     S7GTDWW3
046800040927     C     VIDope        WHENEQ    'T1'
046900001115     C                   EXFMT     S7GTDWW4
047000040927     C     VIDope        WHENEQ    'T2'
047100030225     C                   EXFMT     S7GTDWW5
047200040927     C                   OTHER
047300040927     C                   EXSR      O15Ges
047400040927     C                   ENDSL
047500001115     C
047600970526     C                   ENDSR
047700040927     C*------------------------------------------------------------------------*
047800040927     C* O15GES - GESTIONE OPZIONE 15=VISUALIZZAZIONE RECORD TIVTL10F.
047900040927     C*------------------------------------------------------------------------*
048000040927     C     O15GES        BEGSR
048100040927     C
048200040927     C                   CALL      'TIS7GT11R'
048300040927     C                   PARM                    D05TiVtl
048400040927     C                   PARM      *BLANK        TastoUscita
048500040927     C
048600040927     C                   ENDSR
048700970526     C*------------------------------------------------------------------------*
048800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
048900970526     C*------------------------------------------------------------------------*
049000970526     C     NEWPOS        BEGSR
049100970526     C*
049200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
049300970526     C*         1= RICHIESTO POSIZIONAMENTO
049400970526     C                   Z-ADD     0             SWPOSI            7 0
049500970526     C*
049600991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
049700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
049800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
049900970526     C                   Z-ADD     0             SWMODI            1 0
050000970530     C*
050100970530     C* REIMPOSTA INDICATORI DI ERRORE
050200991108     C                   SETOFF                                       808182
050300081229     C                   SETOFF                                       838485
050400970526     C*
050500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
050600001103     C     VIDks1        IFNE      depks1
050700001103     C     VIDdt1        ORNE      depdt1
050800050114     C     VIDti1        ORNE      depti1
050900130206     C     VIDTip        ORNE      depTip
051000130218     C     VIDApp        ORNE      depApp
051100140123     C     VIDLink       ORNE      depLink
051200130218     C     VIDClTIPAD    ORNE      depClTIPAD
051300050114     C     VIDhs1        ORNE      dephs1
051400130207     C     VIDRpyOpC     ORNE      depRpyOpC
051500050124     C     VIDsun        ORNE      depsun
051600001103     C     VIDdt2        ORNE      depdt2
051700081229     c     vidor2        orne      depor2
051800970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
051900001103     C     ERRL00        IFEQ      '0'                                          *NO ERORI
052000970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
052100991108     C                   MOVEL     VIDks1        DEPks1                         *DEPOSITI
052200991108     C                   MOVEL     VIDdt1        DEPdt1                         *DEPOSITI
052300050114     C                   MOVEL     VIDti1        DEPti1                         *DEPOSITI
052400130206     C                   MOVEL     VIDTip        DEPTip                         Web services S/N/
052500130218     C                   MOVEL     VIDApp        DEPApp                         App N/
052600140123     C                   MOVEL     VIDLink       DEPLink                        Link S/N/
052700130218     C                   MOVEL     VIDClTIPAD    DEPClTIPAD                     IP Addres
052800050114     C                   MOVEL     VIDhs1        DEPhs1                         *DEPOSITI
052900130207     C                   MOVEL     VIDRpyOpC     DEPRpyOpC                      Replay Op.Code
053000050124     C                   MOVEL     VIDsun        DEPsun                         *DEPOSITI
053100991108     C                   MOVEL     VIDdt2        DEPdt2                         *DEPOSITI
053200081229     c                   movel     vidor2        depor2
053300001103     C                   ENDIF
053400001103     C                   ENDIF
053500970526     C*
053600970526     C                   ENDSR
053700970526     C*------------------------------------------------------------------------*
053800970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
053900970526     C*------------------------------------------------------------------------*
054000991117     C     SETFIL        BEGSR
054100040924     C
054200040924     C                   EXSR      OpenCursor
054300970526     C*
054400970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
054500040924     C*    VIDdt2        IFEQ      *ZEROS
054600040924     C*                  MOVEL     *hival        KTLdat
054700040924     C*                  ELSE
054800040924     C*                  Z-ADD     VIDdt2        CHKDATA
054900040924     C*                  Z-ADD     AAAA          AAINV
055000040924     C*                  Z-ADD     MM            MMINV
055100040924     C*                  Z-ADD     GG            GGINV
055200040924     C*                  MOVEL     INVDATA       KTLdat
055300040924     C*                  ENDIF
055400040924     C*                  MOVEL     *hival        KTLora
055500040924IF  1C*    WTIPRI        IFEQ      'DAT'                                        *PER PARTENZA
055600040924     C*    KEYvtl1       SETLL     tivtl01l                           99
055700040924E   1C*                  ENDIF
055800040924IF  1C*    WTIPRI        IFEQ      'KSC'                                        *PER PARTENZA
055900040924     C*    KEYvtl2       SETLL     tivtl02l                           99
056000040924E   1C*                  ENDIF
056100980107     C*
056200970526     C* LETTURA RECORD
056300040924IF  1C*    *IN99         IFEQ      *OFF
056400970526     C                   EXSR      LETFIL
056500040924E   1C*                  ENDIF
056600970526     C*
056700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
056800970526IF  1C     *IN99         IFEQ      *ON
056900970526     C                   Z-ADD     1             SFLEOF
057000970526E   1C                   ENDIF
057100970526     C*
057200970526     C                   ENDSR
057300970526     C*------------------------------------------------------------------------*
057400970526     C* LETFIL - LETTURA RECORD
057500970526     C*------------------------------------------------------------------------*
057600970526     C     LETFIL        BEGSR
057700970526     C*
057800970526     C* LEGGE FINO A:
057900970526     C*     - FINE FILE
058000970526     C*     - TROVATO RECORD VALIDO
058100970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
058200970526DO  1C     *IN99         DOWEQ     *OFF
058300970526     C     WRECOK        ANDEQ     'N'
058400970526     C*
058500970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
058600040924IF  2C*    WTIPRI        IFEQ      'DAT'                                        *PER PARTENZA
058700040924     C*                  READ      tivtl01l                               99
058800040924E   2C*                  ENDIF
058900040924IF  2C*    WTIPRI        IFEQ      'KSC'                                        *PER PARTENZA
059000040924     C*                  READ      tivtl02l                               99
059100040924E   2C*                  ENDIF
059200040924     C*
059300040924     C                   EXSR      FetchCursor
059400140123     C*
059500970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
059600970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
059700970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
059800040924     C                   EXSR      CloseCursor
059900970530X   2C                   ELSE
060000970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
060100970530E   2C                   ENDIF
060200970526E   1C                   ENDDO
060300970526     C*
060400970526     C                   ENDSR
060500970526     C*------------------------------------------------------------------------*
060600970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
060700970526     C*------------------------------------------------------------------------*
060800970526     C     CHKREC        BEGSR
060900970526     C*
061000970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
061100001103     C*
061200001103     C*filtra se impostato codice cliente
061300040924     C*    vidks1        ifeq      *zeros
061400040924     C*    vidks1        oreq      *blank
061500040924     C*                  ELSE
061600040924     C*    vidks1        ifne      vtlksu
061700040924     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
061800040924     C*                  ENDIF
061900040924     C*                  ENDIF
062000991108     C*
062100001103     C*filtra se impostata data del log
062200040924     C*    viddt1        ifeq      *zeros
062300040924     C*                  ELSE
062400040924     C*                  Z-ADD     viddt1        CHKDATA
062500040924     C*                  Z-ADD     AAAA          AAINV
062600040924     C*                  Z-ADD     MM            MMINV
062700040924     C*                  Z-ADD     GG            GGINV
062800040924     C*    invdata       ifne      vtldat
062900040924     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
063000040924     C*                  ENDIF
063100040924     C*                  ENDIF
063200991108     C*
063300970526     C                   ENDSR
063400970526     C*------------------------------------------------------------------------*
063500001114 XXX C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
063600970526     C*------------------------------------------------------------------------*
063700970526     C     IRESFL        BEGSR
063800970526     C*
063900970526     C                   MOVEL     *BLANKS       OPZKEY
064000041129     C                   MOVE      VtlRpyMmt     WrkDatISO
064100130218     C                   MOVE      VtlRpyMmt     WrkOraISO
064200130218     C                   EVAL      WrkOraHMSm = %subst(%char(VTLRpyMMT):12:12)
064300040924     C*                  Z-ADD     vtldat        VIHdati
064400040924     C                   MOVE      WrkDatISO     VihDatI
064500040924     C*                  Z-ADD     vtldat        G08INV
064600040924     C*                  Z-ADD     *ZEROS        G08DAT
064700040924     C*                  MOVEL     '3'           G08ERR
064800040924     C*                  CALL      'XSRDA8'
064900040924     C*                  PARM                    WLBDA8
065000040924     C*                  Z-ADD     G08DAT        VIHdat
065100040924     C*                  Z-ADD     VIHdat        CHKDATA
065200040924     C     *EUR          MOVE      WrkDatISO     VihDat
065300040924     C*                  Z-ADD     GG            GGA
065400040924     C*                  Z-ADD     MM            MMA
065500040924     C*                  Z-ADD     AAAA          AAAAA
065600040924     C*                  MOVEL     DATAA         VIDdat
065700040924     C                   MOVE      WrkDatISO     VidDat
065800040924     C*                  z-add     vtlora        VIHora
065900130218     C                   MOVEL     WrkOraISO     VihOra
066000040924     C*                  z-add     VIHora        ORAN
066100040924     C*                  z-add     HH            HHA
066200040924     C*                  z-add     PP            PPA
066300040924     C*                  z-add     SS            SSA
066400040924     C*                  MOVEL     ORAA          VIDora
066500130218     C                   MOVE      WrkOraISO     VidOra
066600130218     C                   MOVEL     WrkOraHMSm    VidOra2
066700991116     C                   MOVEL     vtlksu        VIDksc
066800991116     C                   MOVE      vtlksu        VIPksc
066900001114     C* verifico che il codice cliente sia un valore numerico
067000001114     C                   EXSR      chknum
067100001114     C                   IF        PiInt=*on
067200001114     C* se OK reperimento "Ragione Sociale" del Cliente Unificante
067300991108     C                   CLEAR                   BS69DS
067400991108     C                   CLEAR                   ACODS
067500991108     C                   MOVEL     KNSIF         I69SIF
067600001114     C                   Z-ADD     PiVal         I69KAC
067700991108     C                   CALL      'TIBS69R'
067800991108     C                   PARM                    BS69DS
067900991108     C                   PARM                    ACODS
068000991108     C                   PARM                    INDDS
068100991108     C                   PARM                    CLPDS
068200991108     C                   PARM                    CLSDS
068300991108     C     O69ERR        IFNE      '1'
068400991108     C                   MOVEL     ACORAG        VIDRAG
068500991112     C                   MOVEL     ACORAG        VIHRAG
068600130218     C                   EVAL      VidIPAddr = VtlCltIPAd
068700991108     C                   ELSE
068800991108     C                   MOVEL     *ALL'*'       VIDRAG
068900991112     C                   MOVEL     *ALL'*'       VIHRAG
069000130218     C                   EVAL      VidIPAddr = *all'*'
069100991108     C                   ENDIF
069200001114     C                   ELSE
069300001114     C                   MOVEL     *ALL'*'       VIDRAG
069400001114     C                   MOVEL     *ALL'*'       VIHRAG
069500040924     C                   EVAL      VidRag = VtlCltIPAd
069600130218     C                   EVAL      VidIPAddr = VTLCltIPAd
069700001114     C                   ENDIF
069800991116     C                   MOVEL     vtltip        VIDope
069900991116     C* controllo in schiera codici e descrizioni tipo operazioni
070000991108     C                   SETOFF                                       28
070100991108     C                   Z-ADD     1             J                 2 0
070200991116     C     vtltip        LOOKUP    TIP(J)                                 28
070300991108     C     *IN28         IFEQ      *ON
070400991116     C                   MOVEL     DES(J)        DESOPE
070500001106     C                   ELSE
070600001106     C                   EVAL      DESOPE = '* Servizio non cod. *'
070700991108     C                   ENDIF
070800001103     C                   CLEAR                   tis172
070900001103     C                   CLEAR                   tis174
071000001103     C                   CLEAR                   tis175
071100991221     C     vtltip        IFEQ      'RB'
071200040924     C                   MOVEL     VtlRqsDta     tis172
071300001114     C                   MOVEL     rifi72        VIDrif
071400001114     C                   MOVEL     dini72        VIDdai
071500001114     C                   MOVEL     dfii72        VIDdaf
071600001114     C                   MOVEL     dsti72        VIDdst
071700991116     C                   ENDIF
071800991221     C     vtltip        IFEQ      'DB'
071900040924     C                   MOVEL     VtlRqsDta     tis174
072000001114     C                   MOVEL     nspedizi74    VIDspe
072100991116     C                   ENDIF
072200001103     C     vtltip        IFEQ      'DM'
072300040924     C                   MOVEL     VtlRqsDta     tis175
072400001114     C                   MOVE(p)   docmiti75     VIDrmn
072500001103     C                   ENDIF
072600001114     C     vtltip        IFEQ      'T1'
072700040924     C                   MOVEL     VtlRqsDta     tis1W2
072800001114     C                   MOVEL     din           VIDdai
072900001114     C                   MOVEL     dfi           VIDdaf
073000001114     C                   Z-ADD     lna           VIDPOA
073100001114     C                   MOVEL     prd           VIDPRA
073200001114     C                   MOVEL     dst           VIDDES
073300001114     C                   MOVEL     mit           VIDMIT
073400001114     C                   MOVEL     ckc           VIDCON
073500001114     C                   MOVEL     ckg           VIDGIA
073600001114     C                   MOVEL     cka           VIDALT
073700001114     C                   ENDIF
073800030225     C     vtltip        IFEQ      'T2'
073900040924     C                   MOVEL     VtlRqsDta     tis1G2
074000030225     C                   MOVEL     g_din         VIDg_dai
074100030225     C                   MOVEL     g_dfi         VIDg_daf
074200030225     C                   MOVEL     g_rif         VIDg_RIF
074300030225     C                   MOVEL     g_prd         VIDg_PRD
074400030225     C                   MOVEL     g_lod         VIDg_LOD
074500030225     C                   MOVEL     g_dst         VIDg_DST
074600030225     C                   MOVEL     g_mit         VIDg_MIT
074700030225     C                   MOVEL     g_ape         VIDg_APE
074800030225     C                   MOVEL     g_dis         VIDg_DIS
074900030225     C                   ENDIF
075000040924     C*                  EVAL      VIDESI = %trim(%subst(vtldsi:(256-10+1):10))
075100040924     C                   EVAL      VIDESI = VtlRpyOpC
075200040927     C                   EVAL      D05TiVtl = TiVtl10F
075300130218     C* reperisco descrizione Esito
075400130218     C                   SELECT
075500130218     C* se il codice è 0 o DONE va sostituito con OK
075600130218     C                   WHEN      VTLRpyOpC = '0' or
075700130218     C                             VTLRpyOpC = 'DONE'
075800130218     C                   EVAL      J=%lookup('OK':RPY)
075900130218     C                   IF        J <> 0
076000130218     C                   EVAL      VDDRpyOpC = RPY_D(J)
076100130218     C                   ELSE
076200130218     C                   EVAL      VDDRpyOpC = *all'*'
076300130218     C                   ENDIF
076400130218     C* se il codice è inizia con ERR sostituito con ERR*
076500130218     C                   WHEN      %subst(VTLRpyOpC:1:3) = 'ERR'
076600130218     C                   EVAL      J=%lookup('ERR*':RPY)
076700130218     C                   IF        J <> 0
076800130218     C                   EVAL      VDDRpyOpC = RPY_D(J)
076900130218     C                   ELSE
077000130218     C                   EVAL      VDDRpyOpC = *all'*'
077100130218     C                   ENDIF
077200130218     C* un tutti gli altri casi si prende il codice
077300130218     C                   OTHER
077400130218     C                   EVAL      J=%lookup(vtlRpyOpC:RPY)
077500130218     C                   IF        J <> 0
077600130218     C                   EVAL      VDDRpyOpC = RPY_D(J)
077700130218     C                   ELSE
077800130218     C                   EVAL      VDDRpyOpC = *all'*'
077900130218     C                   ENDIF
078000130218     C                   ENDSL
078100130218     C* reperisco descrizione Richiesta
078200140219     C* se la richiesta è 'DETTAGLIO' allora devo vedere cos'è il cod.operazione, perché T3 ha una
078300140219     C* descrizione diversa
078400140219     C                   SELECT
078500140219     C                   WHEN      VTLRqsOpC = 'DETTAGLIO' and
078600140219     C                             VTLTip = 'T3'
078700140219     C                   EVAL      wRQS = %trim(VTLRqsOpC) + %trim(VTLTip)
078800140219     C                   OTHER
078900140219     C                   EVAL      wRQS = VTLRqsOpC
079000140219     C                   ENDSL
079100140219     C                   EVAL      J=%lookup(wRQS:RQS)
079200130218     C                   IF        J <> 0
079300130218     C                   EVAL      VDDRqsOpC = RQS_D(J)
079400130218     C                   ELSE
079500130218     C                   EVAL      VDDRqsOpC = *all'*'
079600130218     C                   ENDIF
079700991108     C*
079800970526     C                   ENDSR
079900001114     C*----------------------------------------------------*
080000001114     C*  CONTROLLO NUMERICITA' CAMPI
080100001114     C*----------------------------------------------------*
080200001114     C     CHKNUM        BEGSR
080300001114     C*
080400001114     C                   call(e)   'ISNUMERIC'
080500001114     C                   PARM      VIDksc        PiStr            30
080600001114     C                   PARM      ','           PiDecChr          1
080700001114     C                   PARM      *ZEROS        PiVal            30 9
080800001114     C                   PARM      '0'           PiInt             1
080900001114     C                   PARM      '0'           PiNum             1
081000001114     C                   IF        %error
081100001114     C                   EVAL      PiInt=*off
081200001114     C                   ENDIF
081300001114     C*
081400001114     C                   ENDSR
081500970526     C*------------------------------------------------------------------------*
081600991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
081700970526     C*------------------------------------------------------------------------*
081800970526     C     CHKPAR        BEGSR
081900970528     C*
082000991117     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L
082100991117     C                   MOVEL     'DAT'         WTIPRI            3
082200020708     C*
082300970526     C                   ENDSR
082400970526     C*------------------------------------------------------------------------*
082500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
082600970526     C*------------------------------------------------------------------------*
082700970526     C     CHKFOP        BEGSR
082800991112     C*
082900991112     C* OPZIONI
083000970626     C*
083100970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
083200991112IF  1C     DEPOP0        IFEQ      'P05'
083300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
083400970626     C                   MOVEL     'S'           OPDATT
083500970626     C                   MOVEL     OPDRIG        OPD(5)
083600970626E   1C                   ENDIF
083700970626     C*
083800970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
083900970626IF  1C     DEPOP0        IFEQ      'P02'
084000991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
084100991112     C                   MOVEL     'S'           OPDATT
084200991112     C                   MOVEL     OPDRIG        OPD(5)
084300970626E   1C                   ENDIF
084400970626     C*
084500970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
084600020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
084700991110     C                   MOVEL     FUD(1)        FUDRIG                         *NO INSERIMENTO
084800970626     C                   MOVEL     'N'           FUDATT
084900991110     C                   MOVEL     FUDRIG        FUD(1)
085000991110     C                   MOVEL     FUD(11)       FUDRIG                         *NO INSERIMENTO
085100991110     C                   MOVEL     'N'           FUDATT
085200991110     C                   MOVEL     FUDRIG        FUD(11)
085300970626E   1C                   ENDIF
085400970526     C*
085500970526     C* GESTIONE OPZIONI A VIDEO
085600970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
085700970526     C                   EXSR      OPZGES
085800970526     C*
085900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
086000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
086100970526     C                   EXSR      FUNGES
086200970526     C*
086300970526     C                   ENDSR
086400991108     C*------------------------------------------------------------------------*
086500991108     C* FINSR - OPERAZIONI FINALI
086600991108     C*------------------------------------------------------------------------*
086700991108     C     FINSR         BEGSR
086800991108     C*
086900991108     C* CHIUDE I FILE DEI PGM CHIAMATI
087000991108     C                   CLEAR                   BS69DS
087100991108     C                   CLEAR                   ACODS
087200991108     C                   CLEAR                   INDDS
087300991108     C                   CLEAR                   CLPDS
087400991108     C                   CLEAR                   CLSDS
087500991108     C                   MOVEL     'C'           I69TLA
087600991108     C                   MOVEL     KNSIF         I69SIF
087700991108     C                   CALL      'TIBS69R'
087800991108     C                   PARM                    BS69DS
087900991108     C                   PARM                    ACODS
088000991108     C                   PARM                    INDDS
088100991108     C                   PARM                    CLPDS
088200991108     C                   PARM                    CLSDS
088300040924     C*
088400040924     C                   EXSR      CloseCursor
088500040928     C                   EXSR      ConnectReset
088600991108     C*
088700991108     C                   ENDSR
088800020916     C*--------------------------------------------------------------------------------------------*
088900020916     C* REPERISCE I DATI UTENTE
089000020916     C*--------------------------------------------------------------------------------------------*
089100020916     C     REPDATIUTE    BEGSR
089200020916     C*
089300020916     C* INIZIALIZZA VARIABILI DI WRK
089400020916     C                   CLEAR                   TIBS34DS
089500020916     C                   CLEAR                   AZUTEDS
089600020916     C                   CLEAR                   DDATIUTE
089700020916     C*
089800020916     C     *DTAARA       DEFINE    §azute        azuteds
089900020916     C     *DTAARA       DEFINE    §datiute      ddatiute
090000020916     C                   IN(E)     *DTAARA
090100020916if  1C                   IF        %Error
090200020916     c                   EVAL      I34Tla = 'L'
090300020916     C                   CALL      'TIBS34R'
090400020916     C                   PARM                    Tibs34Ds
090500020916     C                   IN        *DTAARA
090600020916e   1C                   ENDIF
090700020916     C*
090800020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
090900020916     C                   MOVEL(P)  RSUT          DSFIRS           20
091000020916     C*
091100020916     C                   ENDSR
091200970526     C*------------------------------------------------------------------------*
091300970526     C* *INZSR - OPERAZIONI INIZIALI
091400970526     C*------------------------------------------------------------------------*
091500970526     C     *INZSR        BEGSR
091600970526     C*---
091700970526     C* RICEVIMENTO PARAMETRI
091800970526     C*---
091900970526     C     *ENTRY        PLIST
092000970612     C                   PARM                    KPJBA
092100991117     C                   movel     kpjbu         DEPOP0            3
092200970526     C*
092300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
092400970526     C     XMSGPA        PLIST
092500970526     C                   PARM                    DSMSMS
092600970526     C                   PARM                    DSMSPG
092700970526     C                   PARM                    DSMSKE
092800970526     C                   PARM                    DSMSER
092900970526     C*
093000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
093100970526     C                   CALL      'XKEY'
093200970526     C                   PARM                    DSKEY
093300970526     C*---
093400970526     C* VARIABILI NON RIFERITE AL DATA BASE
093500970526     C*---
093600970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
093700970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
093800970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
093900970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
094000970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
094100130207     C     *LIKE         DEFINE    SFLNBR        D07SFLULT
094200970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
094300970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
094400970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
094500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
094600970526     C*---
094700991108     C     *LIKE         DEFINE    viddt2        Depdt2
094800991108     C     *LIKE         DEFINE    viddt1        Depdt1
094900081229     c     *like         define    vidor2        depor2
095000991108     C     *LIKE         DEFINE    vidks1        Depks1
095100050114     C     *LIKE         DEFINE    vidti1        Depti1
095200130206     C     *LIKE         DEFINE    vidTip        DepTip
095300130218     C     *LIKE         DEFINE    vidApp        DepApp
095400140123     C     *LIKE         DEFINE    vidLink       DepLink
095500130218     C     *LIKE         DEFINE    vidClTIPAD    DepClTIPAD
095600050114     C     *LIKE         DEFINE    vidhs1        Dephs1
095700130207     C     *LIKE         DEFINE    vidRpyOpC     DepRpyOpC
095800050124     C     *LIKE         DEFINE    vidsun        Depsun
095900040924     C*    *LIKE         DEFINE    vtldat        ktldat
096000040924     C*    *LIKE         DEFINE    vtlora        ktlora
096100040924     C*    *LIKE         DEFINE    vtlksu        ktlksc
096200991116     C*
096300970526     C* VARIABILI DI CONTROLLO
096400970526     C*---
096500970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
096600970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
096700970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
096800970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
096900970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
097000130218     C                   Z-ADD     14            SFLPAG                         *N°RIGHE X PAG
097100991110     C                   MOVEL     '/'           SEP1                           *VALIDITA' RECORD
097200991110     C                   MOVEL     '/'           SEP2                           *VALIDITA' RECORD
097300991110     C                   MOVEL     ':'           SEP3                           *VALIDITA' RECORD
097400991110     C                   MOVEL     ':'           SEP4                           *VALIDITA' RECORD
097500970526     C*
097600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
097700991116     C                   MOVEL     'TIS7GTR'     DSMSPG
097800991116     C                   MOVEL     'TIS7GTR'     MSGQUE
097900970526     C                   MOVEL     *BLANKS       MSGKEY
098000970526     C                   MOVEL     *BLANKS       DSMSMS
098100970526     C                   MOVEL     *BLANKS       DSMSKE
098200970526     C                   Z-ADD     *ZEROS        DSMSER
098300970526     C*---
098400970526     C* VARIABILI RIFERITE AL DATA BASE
098500970526     C*---
098600970526     C*---
098700970526     C* CHIAVI DI LETTURA
098800970526     C*---
098900991116     C* LETTURA tivtl01L -
099000040924     C*    KEYvtl1       KLIST
099100040924     C*                  KFLD                    KTLdat                         *PARTENZA/ARRIVO
099200040924     C*                  KFLD                    KTLora                         *PARTENZA/ARRIVO
099300040924     C*                  KFLD                    KTLksc                         *PARTENZA/ARRIVO
099400991117     C* LETTURA tivtl02L -
099500040924     C*    KEYvtl2       KLIST
099600040924     C*                  KFLD                    KTLksc                         *PARTENZA/ARRIVO
099700040924     C*                  KFLD                    KTLdat                         *PARTENZA/ARRIVO
099800040924     C*                  KFLD                    KTLora                         *PARTENZA/ARRIVO
099900991108     C*
100000970527     C* REPERIMENTO DATI UTENTE
100100020916     C                   EXSR      REPDATIUTE
100200970526     C*
100300970526     C* CALCOLA LA DATA CORRENTE
100400970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
100500970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
100600970526     C                   Z-ADD     WN8           G08DAT
100700970526     C                   Z-ADD     *ZEROS        G08INV
100800970526     C                   MOVEL     '0'           G08ERR
100900970526     C                   CALL      'XSRDA8'
101000970526     C                   PARM                    WLBDA8
101100970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
101200970526     C*
101300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
101400970526     C                   EXSR      CHKPAR
101500991115     C*
101600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
101700970526     C                   EXSR      CHKFOP
101800970526     C*
101900970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
102000970530IF  1C     DEPOP0        IFEQ      'P01'
102100970530     C                   MOVEL     DEC(3)        VIDING
102200970530E   1C                   ENDIF
102300970530IF  1C     DEPOP0        IFEQ      'P02'
102400970526     C                   MOVEL     DEC(1)        VIDING
102500970530E   1C                   ENDIF
102600970530IF  1C     DEPOP0        IFEQ      'P05'
102700970526     C                   MOVEL     DEC(2)        VIDING
102800970526E   1C                   ENDIF
102900970526     C*
103000130207     C* se sono su AS888 non mi connetto a 777 perché non ci si riesce
103100130207     C                   IF        UBRTVNETA_Rtv( currSysNeta )=*zeros
103200130207     C                   IF        %subst (
103300130207     C                                            currSysNeta
103400130207     C                                                  : 1 : 5 )<>'AS888'
103500040924     C                   EXSR      ConnectTo
103600130207     C                   ENDIF
103700130207     C                   ENDIF
103800040924     C*
103900970526     C                   ENDSR
104000970526     C*------------------------------------------------------------------------*
104100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
104400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
104500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
104600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
104800970526     C*------------------------------------------------------------------------*
104900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
105000970526     C*------------------------------------------------------------------------*
105100970526     C     POSSFL        BEGSR
105200970526     C*
105300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
105400970526     C                   EXSR      CLRSFL
105500970526     C*
105600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
105700970526     C                   EXSR      SETFIL
105800970526     C*
105900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
106000970526     C     SFLEOF        IFEQ      0
106100970526     C                   EXSR      CARSFL
106200970526     C                   END
106300970526     C*
106400970526     C                   ENDSR
106500970526     C*------------------------------------------------------------------------*
106600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
106700970526     C*------------------------------------------------------------------------*
106800970526     C     ENTATT        BEGSR
106900970526     C*
107000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
107100970526     C                   Z-ADD     0             OPZOK             1 0
107200970526     C*
107300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
107400970526     C                   Z-ADD     0             SFLVAR            5 0
107500970526     C*
107600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
107700970526     C                   Z-ADD     0             SFLERR
107800970526     C*
107900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
108000970526     C                   Z-ADD     SFLULT        WFLULT
108100970526     C*
108200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
108300991221     C                   READC     S7GTD05                                98
108400970526     C     *IN98         DOWEQ     '0'
108500970526     C*
108600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
108700970526     C                   ADD       1             SFLVAR
108800970526     C*
108900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
109000970526     C     OPZOK         IFEQ      0
109100970526     C*
109200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
109300970526     C     OPZKEY        IFNE      *BLANKS
109400970526     C                   EXSR      OPZCON
109500970526     C                   END
109600970526     C*
109700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
109800970526     C*            2. PULIZIA SCELTA IN SFL
109900970526     C     OPZOK         IFEQ      0
110000970526     C                   MOVEL     '0'           *IN41
110100970526     C                   MOVEL     *BLANKS       OPZKEY
110200970526     C                   ELSE
110300970526     C*
110400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
110500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
110600970526     C                   MOVEL     '1'           *IN41
110700970526     C                   Z-ADD     SFLULT        SFLERR
110800970526     C                   END
110900970526     C                   ELSE
111000970526     C*
111100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
111200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
111300970526     C                   MOVEL     '1'           *IN41
111400970526     C                   END                                                    OPZOK=0
111500970526     C*
111600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
111700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
111800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
111900970526     C     OPZOK         IFNE      0
112000970526     C                   Z-ADD     SFLERR        SFLNBR
112100970526     C                   ELSE
112200970526     C                   Z-ADD     SFLULT        SFLNBR
112300970526     C                   END
112400970526     C*
112500970526     C* AGGIORNO RECORD SFL
112600991221     C                   UPDATE    S7GTD05
112700970526     C*
112800991221     C                   READC     S7GTD05                                98
112900970526     C                   END                                                    DO WHILE
113000970526     C*
113100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
113200970526     C                   Z-ADD     WFLULT        SFLULT
113300970526     C*
113400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
113500970526     C     SFLVAR        IFEQ      0
113600970526     C*---                 MOVEL'1'       FINL00
113700970526     C                   END
113800970526     C*
113900991112     C*
114000970526     C                   ENDSR
114100970526     C*------------------------------------------------------------------------*
114200970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
114300970526     C*------------------------------------------------------------------------*
114400970526     C     CLRSFL        BEGSR
114500970526     C*
114600970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
114700970526     C* (0=NON FINE FILE  1=FINE FILE)
114800970526     C                   Z-ADD     0             SFLEOF
114900970526     C*
115000970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
115100970526     C                   Z-ADD     0             SFLULT
115200970526     C*
115300970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
115400970526     C                   MOVEL     '1'           *IN40
115500991221     C                   WRITE     S7GTD06
115600970526     C                   MOVEL     '0'           *IN40
115700970526     C*
115800970526     C                   ENDSR
115900970526     C*------------------------------------------------------------------------*
116000970526     C* CARSFL - CARICAMENTO SFL
116100970526     C*------------------------------------------------------------------------*
116200970526     C     CARSFL        BEGSR
116300970526     C*
116400970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
116500050124     C***  SFLULT        IFEQ      0
116600050124     C***                EXSR      SETFIL
116700050124     C***                END
116800970526     C*
116900970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
117000970526     C                   Z-ADD     SFLULT        WFLULT
117100970526     C*
117200970526     C* CICLO:
117300970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
117400970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
117500970526     C                   Z-ADD     1             SFLRIG
117600970526     C     SFLRIG        DOWLE     SFLPAG
117700970526     C     SFLEOF        ANDEQ     0
117800970526     C*
117900970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
118000991116     C                   MOVEL     *BLANKS       OPZKEY
118100970526     C*
118200970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
118300970526     C                   EXSR      IRESFL
118400970526     C*
118500970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
118600970526     C                   ADD       1             SFLULT
118700970526     C*
118800970526     C* SCRITTURA RECORD SFL
118900991221     C                   WRITE     S7GTD05
119000970526     C                   ADD       1             SFLRIG
119100970526     C*
119200970526     C* LETTURA FILE DA CARICARE
119300970526     C                   EXSR      LETFIL
119400970526     C                   END                                                    DO WHILE
119500970526     C*
119600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
119700970526     C     WFLULT        IFNE      SFLULT
119800970526     C                   Z-ADD     WFLULT        SFLNBR
119900970526     C                   ADD       1             SFLNBR
120000970526     C                   END
120100970526     C*
120200970526     C                   ENDSR
120300970526     C*------------------------------------------------------------------------*
120400970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
120500970526     C*------------------------------------------------------------------------*
120600970526     C     OUTSFL        BEGSR
120700970526     C*
120800970526     C* WRITE RECORD TESTATA VIDEO
120900991221     C                   WRITE     S7GTD01
121000970526     C*
121100970526     C* VISUALIZZAZIONE MESSAGGIO
121200970526     C                   EXSR      SNDMSG
121300970526     C*
121400970526     C* WRITE RECORD OPZIONI VIDEO
121500991221     C                   WRITE     S7GTD02
121600970526     C*
121700970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
121800991221     C                   WRITE     S7GTD03
121900970526     C*
122000970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
122100991221     C                   WRITE     S7GTD04
122200970526     C*
122300970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
122400970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
122500970526     C     SFLEOF        IFEQ      0
122600970526     C                   MOVEL     '0'           *IN42
122700970526     C                   ELSE
122800970526     C*
122900970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
123000970526     C                   MOVEL     '1'           *IN42
123100970526     C                   END
123200970526     C*
123300970526     C* WRITE RECORD TASTI FUNZIONALI
123400991221     C                   WRITE     S7GTD09
123500970526     C*
123600970526     C* CASO DI SFL VUOTO
123700970526     C     SFLULT        IFLE      0
123800970526     C*
123900970526     C*    - WRITE RECORD VUOTO
124000991221     C                   WRITE     S7GTD0A
124100991221     C                   READ      S7GTD0A                                99
124200970526     C*
124300970526     C                   ELSE
124400970526     C*
124500970526     C* CASO DI SFL CON RECORDS
124600970526     C*
124700970526     C*    - WRITE RECORD CONTROLLO SFL
124800991221     C                   WRITE     S7GTD06
124900991221     C                   READ      S7GTD06                                99
125000970526     C                   END
125100970526     C*
125200970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
125300991221     C                   READ      S7GTD03                                99
125400970526     C*
125500970526     C                   ENDSR
125600970526     C*------------------------------------------------------------------------*
125700970526     C* SNDMSG - MANDA MESSAGGIO
125800970526     C*------------------------------------------------------------------------*
125900970526     C     SNDMSG        BEGSR
126000970526     C*
126100970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
126200970526     C                   MOVEL     '1'           *IN40
126300991221     C                   WRITE     S7GTDMC
126400970526     C                   MOVEL     '0'           *IN40
126500970526     C*
126600970526     C* VISUALIZZAZIONE MESSAGGIO
126700970526     C                   Z-ADD     0             MSGULT
126800970526     C                   CALL      'XMSG'        XMSGPA
126900970526     C     DSMSER        IFEQ      0
127000970526     C                   MOVEL     DSMSKE        MSGKEY
127100970526     C                   END
127200970526     C                   Z-ADD     1             MSGULT
127300991221     C                   WRITE     S7GTDMS                                      SFL MESS
127400991221     C                   WRITE     S7GTDMC                                      CONTR MESS
127500970526     C*
127600970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
127700970526     C                   MOVEL     *BLANKS       DSMSMS
127800970526     C*
127900970526     C                   ENDSR
128000970526     C*------------------------------------------------------------------------*
128100130207     C* SFLSCELTA - SFL DI SCELTA
128200970526     C*------------------------------------------------------------------------*
128300130207     C     SFLSCelta     BEGSR
128400130207     C*
128500130207     C* CARICAMENTO SFL
128600130207     C*
128700130207     C* pulizia sfl
128800130208     C                   EVAL      D07SflUlt = 0
128900130208     C                   MOVEL     '1'           *IN45
129000130207     C                   WRITE     S7GTWW7
129100130208     C                   MOVEL     '0'           *IN45
129200970526     C*
129300130207     C* carico tutti gli elementi della schiera richiesta
129400130208     C                   SELECT
129500141105     C                   WHEN      SkScelta = 'TI1'
129600141105     C                   EVAL      totI = %elem(TIP)
129700141105     C                   WHEN      SkScelta = 'RQS'
129800141105     C                   EVAL      totI = %elem(RQS)
129900130208     C                   WHEN      SkScelta = 'RPY'
130000130208     C                   EVAL      totI = %elem(RPY)
130100130208     C                   ENDSL
130200130208     C                   FOR       wi = 1  BY 1 TO totI
130300130207     C*
130400130207     C                   SELECT
130500141105     C                   WHEN      SkScelta = 'TI1'
130600141105     C                   EVAL      D07Cod = TIP(wi)
130700141105     C                   EVAL      D07Des = DES(wi)
130800130207     C                   EVAL      D07OpzKey = *blank
130900141105     C                   WHEN      SkScelta = 'RQS'
131000141105     C                   EVAL      D07Cod = RQS(wi)
131100141105     C                   EVAL      D07Des = RQS_D(wi)
131200141105     C                   EVAL      D07OpzKey = *blank
131300130208     C                   WHEN      SkScelta = 'RPY'
131400130218     C                   EVAL      D07Cod = RPY(wi)
131500130218     C                   EVAL      D07Des = RPY_D(wi)
131600130208     C                   EVAL      D07OpzKey = *blank
131700130207     C                   ENDSL
131800130207     C*
131900130208     C                   EVAL      D07SflUlt = D07SflUlt + 1
132000130207     C                   WRITE     S7GTD07
132100130207     C*
132200130207     C                   ENDFOR
132300130208     C*
132400130208     C                   IF        D07SflUlt > 0
132500130208     C                   EVAL      SflNbrWW7 = 1
132600130208     C                   ENDIF
132700130207     C*
132800130207     C                   EXFMT     S7GTWW7
132900130207     C*
133000130207     C* LETTURA SCELTA
133100130207     C*
133200130207     C* il primo rcd scelto viene preso (e all'enter si esce)
133300130207     C                   READC     S7GTD07                                98
133400130207     C                   IF        *in98 = *off
133500130207     C                   SELECT
133600141105     C                   WHEN      SkScelta = 'TI1'
133700141105     C                   EVAL      VIDTI1    = D07Cod
133800141105     C                   WHEN      SkScelta = 'RQS'
133900141105     C                   EVAL      VIDHS1    = D07Cod
134000130208     C                   WHEN      SkScelta = 'RPY'
134100130208     C                   EVAL      VIDRpyOpC = D07Cod
134200130207     C                   ENDSL
134300130218     C* se non è stato richiesto niente, imposto la variabile richiesta con blank
134400130218     C                   ELSE
134500130218     C                   SELECT
134600141105     C                   WHEN      SkScelta = 'TI1'
134700141105     C                   EVAL      VIDTI1    = *blank
134800130218     C                   WHEN      SkScelta = 'RQS'
134900130218     C                   EVAL      VIDHS1    = *blank
135000130218     C                   WHEN      SkScelta = 'RPY'
135100130218     C                   EVAL      VIDRpyOpC = *blank
135200130218     C                   ENDSL
135300130218     C                   ENDIF
135400130207     C*
135500130207     C                   ENDSR
135600130207     C*------------------------------------------------------------------------*
135700130207     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
135800130207     C*------------------------------------------------------------------------*
135900130207     C     FUNCON        BEGSR
136000130207     C*
136100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
136200970526     C                   EXSR      FUNVAR
136300970526     C*
136400970526     C* PULISCO CAMPI DI LAVORO
136500970526     C                   Z-ADD     0             I                 5 0
136600970526     C                   Z-ADD     0             W                 5 0
136700970526     C*
136800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
136900970526     C     FUNKEY        IFNE      *BLANKS
137000970526     C     1             DO        24            I
137100970526     C                   MOVEL     FUD(I)        FUDRIG
137200970526     C*
137300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
137400970526     C     FUNKEY        IFEQ      FUDFUN
137500970526     C     FUDATT        ANDEQ     'S'
137600970526     C                   Z-ADD     I             W
137700970526     C                   END
137800970526     C                   END                                                    DO WHILE
137900970526     C*
138000970526     C* FUNZIONE NON ATTIVA (ERRORE)
138100970526     C     W             IFEQ      0
138200970526     C*
138300970526     C* EMISSIONE MESSAGGIO DI ERRORE
138400970526     C                   MOVEL     MSG(2)        DSMSMS
138500970526     C                   END                                                    W=0
138600970526     C                   END                                                    FUNKEY NE *BLANKS
138700970526     C*
138800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
138900970526     C     W             IFNE      0
139000970526     C     FUNKEY        OREQ      *BLANKS
139100970526     C*
139200970526     C     FUNKEY        IFEQ      *BLANKS
139300970526     C                   EXSR      ENTGES
139400970526     C                   END
139500991116     C     FUNKEY        IFEQ      '03'
139600991116     C                   EXSR      F03GES
139700991116     C                   END
139800970526     C     FUNKEY        IFEQ      '05'
139900970526     C                   EXSR      F05GES
140000970526     C                   END
140100991116     C     FUNKEY        IFEQ      '12'
140200991116     C                   EXSR      F12GES
140300991116     C                   END
140400970526     C     FUNKEY        IFEQ      'RU'
140500970526     C                   EXSR      RUPGES
140600970526     C                   END
140700970526     C*
140800970526     C                   END
140900970526     C*
141000970526     C                   ENDSR
141100970526     C*------------------------------------------------------------------------*
141200970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
141300970526     C*------------------------------------------------------------------------*
141400970526     C     FUNVAR        BEGSR
141500970526     C*
141600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
141700970526     C     KEY           IFEQ      ENTER
141800970526     C                   MOVEL     *BLANKS       FUNKEY
141900970526     C                   END
142000970526     C     KEY           IFEQ      F1
142100970526     C                   MOVEL     '01'          FUNKEY
142200970526     C                   END
142300970526     C     KEY           IFEQ      F2
142400970526     C                   MOVEL     '02'          FUNKEY
142500970526     C                   END
142600970526     C     KEY           IFEQ      F3
142700970526     C                   MOVEL     '03'          FUNKEY
142800970526     C                   END
142900970526     C     KEY           IFEQ      F4
143000970526     C                   MOVEL     '04'          FUNKEY
143100970526     C                   END
143200970526     C     KEY           IFEQ      F5
143300970526     C                   MOVEL     '05'          FUNKEY
143400970526     C                   END
143500970526     C     KEY           IFEQ      F6
143600970526     C                   MOVEL     '06'          FUNKEY
143700970526     C                   END
143800970526     C     KEY           IFEQ      F7
143900970526     C                   MOVEL     '07'          FUNKEY
144000970526     C                   END
144100970526     C     KEY           IFEQ      F8
144200970526     C                   MOVEL     '08'          FUNKEY
144300970526     C                   END
144400970526     C     KEY           IFEQ      F9
144500970526     C                   MOVEL     '09'          FUNKEY
144600970526     C                   END
144700970526     C     KEY           IFEQ      F10
144800970526     C                   MOVEL     '10'          FUNKEY
144900970526     C                   END
145000970526     C     KEY           IFEQ      F11
145100970526     C                   MOVEL     '11'          FUNKEY
145200970526     C                   END
145300970526     C     KEY           IFEQ      F12
145400970526     C                   MOVEL     '12'          FUNKEY
145500970526     C                   END
145600970526     C     KEY           IFEQ      F13
145700970526     C                   MOVEL     '13'          FUNKEY
145800970526     C                   END
145900970526     C     KEY           IFEQ      F14
146000970526     C                   MOVEL     '14'          FUNKEY
146100970526     C                   END
146200970526     C     KEY           IFEQ      F15
146300970526     C                   MOVEL     '15'          FUNKEY
146400970526     C                   END
146500970526     C     KEY           IFEQ      F16
146600970526     C                   MOVEL     '16'          FUNKEY
146700970526     C                   END
146800970526     C     KEY           IFEQ      F17
146900970526     C                   MOVEL     '17'          FUNKEY
147000970526     C                   END
147100970526     C     KEY           IFEQ      F18
147200970526     C                   MOVEL     '18'          FUNKEY
147300970526     C                   END
147400970526     C     KEY           IFEQ      F19
147500970526     C                   MOVEL     '19'          FUNKEY
147600970526     C                   END
147700970526     C     KEY           IFEQ      F20
147800970526     C                   MOVEL     '20'          FUNKEY
147900970526     C                   END
148000970526     C     KEY           IFEQ      F21
148100970526     C                   MOVEL     '21'          FUNKEY
148200970526     C                   END
148300970526     C     KEY           IFEQ      F22
148400970526     C                   MOVEL     '22'          FUNKEY
148500970526     C                   END
148600970526     C     KEY           IFEQ      F23
148700970526     C                   MOVEL     '23'          FUNKEY
148800970526     C                   END
148900970526     C     KEY           IFEQ      F24
149000970526     C                   MOVEL     '24'          FUNKEY
149100970526     C                   END
149200970526     C     KEY           IFEQ      ROLLUP
149300970526     C                   MOVEL     'RU'          FUNKEY
149400970526     C                   END
149500970526     C     KEY           IFEQ      HELP
149600970526     C                   MOVEL     'HP'          FUNKEY
149700970526     C                   END
149800970526     C*
149900970526     C                   ENDSR
150000970526     C*------------------------------------------------------------------------*
150100970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
150200970526     C*------------------------------------------------------------------------*
150300970526     C     FUNGES        BEGSR
150400970526     C*
150500970526     C* PULISCO CAMPI DI LAVORO
150600970526     C                   Z-ADD     0             W                 5 0
150700970526     C*
150800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
150900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
151000970526     C     1             DO        24            I
151100970526     C                   MOVEL     FUD(I)        FUDRIG
151200970526     C*
151300970526     C* PULIZIA FUNZIONI PRECEDENTI
151400970526     C                   MOVEL     *BLANKS       FUV(I)
151500970526     C*
151600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
151700970526     C* (SALTA F23 E F24)
151800970526     C     FUDATT        IFEQ      'S'
151900970526     C     FUDFUN        ANDNE     '23'
152000970526     C     FUDFUN        ANDNE     '24'
152100970526     C                   ADD       1             W
152200970526     C                   MOVEL     FUDDES        FUV(W)
152300970526     C                   END
152400970526     C                   END                                                    DO WHILE
152500970526     C*
152600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
152700970526     C* ALTRIMENTI DISATTIVO F24
152800970526     C     1             DO        24            I
152900970526     C                   MOVEL     FUD(I)        FUDRIG
153000970526     C*
153100970526     C     FUDFUN        IFEQ      '24'
153200970526     C     W             IFGT      6
153300970526     C                   MOVEL     'S'           FUDATT
153400970526     C                   ELSE
153500970526     C                   MOVEL     *BLANKS       FUDATT
153600970526     C                   END                                                    W GT 8
153700970526     C                   MOVEL     FUDRIG        FUD(I)
153800970526     C                   END
153900970526     C                   END                                                    DO WHILE
154000970526     C*
154100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
154200970526     C     W             DIV       6             I
154300970526     C                   MVR                     Z
154400970526     C*
154500970526     C     Z             IFNE      0
154600970526     C     I             ADD       1             Z
154700970526     C                   ELSE
154800970526     C                   Z-ADD     I             Z
154900970526     C                   END
155000970526     C*
155100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
155200970526     C                   ADD       1             FUNGRU
155300970526     C     FUNGRU        IFGT      Z
155400970526     C     FUNGRU        ORLT      1
155500970526     C                   Z-ADD     1             FUNGRU
155600970526     C                   END
155700970526     C*
155800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
155900970526     C     FUNGRU        IFEQ      1
156000970526     C                   MOVEL     FUV(1)        FUN001
156100970526     C                   MOVEL     FUV(2)        FUN002
156200970526     C                   MOVEL     FUV(3)        FUN003
156300970526     C                   MOVEL     FUV(4)        FUN004
156400970526     C                   MOVEL     FUV(5)        FUN005
156500970526     C                   MOVEL     FUV(6)        FUN006
156600970526     C                   END
156700970526     C*
156800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
156900970526     C     FUNGRU        IFEQ      2
157000970526     C                   MOVEL     FUV(7)        FUN001
157100970526     C                   MOVEL     FUV(8)        FUN002
157200970526     C                   MOVEL     FUV(9)        FUN003
157300970526     C                   MOVEL     FUV(10)       FUN004
157400970526     C                   MOVEL     FUV(11)       FUN005
157500970526     C                   MOVEL     FUV(12)       FUN006
157600970526     C                   END
157700970526     C*
157800970526     C* TERZO GRUPPO DI 6 FUNZIONI
157900970526     C     FUNGRU        IFEQ      3
158000970526     C                   MOVEL     FUV(13)       FUN001
158100970526     C                   MOVEL     FUV(14)       FUN002
158200970526     C                   MOVEL     FUV(15)       FUN003
158300970526     C                   MOVEL     FUV(16)       FUN004
158400970526     C                   MOVEL     FUV(17)       FUN005
158500970526     C                   MOVEL     FUV(18)       FUN006
158600970526     C                   END
158700970526     C*
158800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
158900970526     C     FUNGRU        IFEQ      4
159000970526     C                   MOVEL     FUV(19)       FUN001
159100970526     C                   MOVEL     FUV(20)       FUN002
159200970526     C                   MOVEL     FUV(21)       FUN003
159300970526     C                   MOVEL     FUV(22)       FUN004
159400970526     C                   MOVEL     *BLANKS       FUN005
159500970526     C                   MOVEL     *BLANKS       FUN006
159600970526     C                   END
159700970526     C*
159800970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
159900970526     C                   MOVEL     *BLANKS       FUN007
160000970526     C                   MOVEL     *BLANKS       FUN008
160100970526     C*
160200970526     C     1             DO        24            I
160300970526     C                   MOVEL     FUD(I)        FUDRIG
160400970526     C     FUDFUN        IFEQ      '23'
160500970526     C     FUDATT        ANDEQ     'S'
160600970526     C                   MOVEL     FUDDES        FUN007
160700970526     C                   END
160800970526     C     FUDFUN        IFEQ      '24'
160900970526     C     FUDATT        ANDEQ     'S'
161000970526     C                   MOVEL     FUDDES        FUN008
161100970526     C                   END
161200970526     C                   END                                                    DO
161300970526     C*
161400970526     C                   ENDSR
161500970526     C*------------------------------------------------------------------------*
161600970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
161700970526     C*------------------------------------------------------------------------*
161800970526     C     OPZGES        BEGSR
161900970526     C*
162000970526     C* PULISCO CAMPI DI LAVORO
162100970526     C                   Z-ADD     0             W                 5 0
162200970526     C*
162300970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
162400970526     C* E CARICO TABELLA OPZIONI ATTIVE
162500991115     C     1             DO        24            I
162600970526     C                   MOVEL     OPD(I)        OPDRIG
162700970526     C*
162800970526     C* PULIZIA OPZIONI PRECEDENTI
162900970526     C                   MOVEL     *BLANKS       OPV(I)
163000970526     C*
163100970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
163200970526     C     OPDATT        IFEQ      'S'
163300970526     C                   ADD       1             W
163400970526     C                   MOVEL     OPDDES        OPV(W)
163500970526     C                   END
163600970526     C                   END                                                    DO WHILE
163700970526     C*
163800970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
163900970526     C* ALTRIMENTI DISATTIVO F23
164000991115     C     1             DO        24            I
164100970526     C                   MOVEL     FUD(I)        FUDRIG
164200970526     C*
164300970526     C     FUDFUN        IFEQ      '23'
164400970526     C     W             IFGT      8
164500970526     C                   MOVEL     'S'           FUDATT
164600970526     C                   ELSE
164700970526     C                   MOVEL     *BLANKS       FUDATT
164800970526     C                   END                                                    W GT 8
164900970526     C                   MOVEL     FUDRIG        FUD(I)
165000970526     C                   END
165100970526     C                   END                                                    DO WHILE
165200970526     C*
165300970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
165400970526     C     W             DIV       8             I
165500970526     C                   MVR                     Z                 5 0
165600970526     C*
165700970526     C     Z             IFNE      0
165800970526     C     I             ADD       1             Z
165900970526     C                   ELSE
166000970526     C                   Z-ADD     I             Z
166100970526     C                   END
166200970526     C*
166300970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
166400970526     C                   ADD       1             OPZGRU
166500970526     C     OPZGRU        IFGT      Z
166600970526     C     OPZGRU        ORLT      1
166700970526     C                   Z-ADD     1             OPZGRU
166800970526     C                   END
166900970526     C*
167000970526     C* PRIMO GRUPPO DI 8 OPZIONI
167100970526     C     OPZGRU        IFEQ      1
167200970526     C                   MOVEL     OPV(1)        OPZ001
167300970526     C                   MOVEL     OPV(2)        OPZ002
167400970526     C                   MOVEL     OPV(3)        OPZ003
167500970526     C                   MOVEL     OPV(4)        OPZ004
167600970526     C                   MOVEL     OPV(5)        OPZ005
167700970526     C                   MOVEL     OPV(6)        OPZ006
167800970526     C                   MOVEL     OPV(7)        OPZ007
167900970526     C                   MOVEL     OPV(8)        OPZ008
168000970526     C                   END
168100970526     C*
168200970526     C* SECONDO GRUPPO DI 8 OPZIONI
168300970526     C     OPZGRU        IFEQ      2
168400970526     C                   MOVEL     OPV(9)        OPZ001
168500970526     C                   MOVEL     OPV(10)       OPZ002
168600970526     C                   MOVEL     OPV(11)       OPZ003
168700970526     C                   MOVEL     OPV(12)       OPZ004
168800970526     C                   MOVEL     OPV(13)       OPZ005
168900970526     C                   MOVEL     OPV(14)       OPZ006
169000970526     C                   MOVEL     OPV(15)       OPZ007
169100970526     C                   MOVEL     OPV(16)       OPZ008
169200970526     C                   END
169300970526     C*
169400970526     C* TERZO GRUPPO DI 8 OPZIONI
169500970526     C     OPZGRU        IFEQ      3
169600970526     C                   MOVEL     OPV(17)       OPZ001
169700970526     C                   MOVEL     OPV(18)       OPZ002
169800970526     C                   MOVEL     OPV(19)       OPZ003
169900970526     C                   MOVEL     OPV(20)       OPZ004
170000970526     C                   MOVEL     OPV(21)       OPZ005
170100970526     C                   MOVEL     OPV(22)       OPZ006
170200970526     C                   MOVEL     OPV(23)       OPZ007
170300970526     C                   MOVEL     OPV(24)       OPZ008
170400970526     C                   END
170500970526     C*
170600970526     C                   ENDSR
170700970526     C*------------------------------------------------------------------------*
170800970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
170900970526     C*------------------------------------------------------------------------*
171000970526     C     OPZCON        BEGSR
171100970526     C*
171200970526     C* PULISCO CAMPI DI LAVORO
171300970526     C                   Z-ADD     0             I                 5 0
171400970526     C                   Z-ADD     0             W                 5 0
171500970526     C*
171600970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
171700970526     C     1             DO        24            I
171800970526     C                   MOVEL     OPD(I)        OPDRIG
171900970526     C*
172000970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
172100970526     C     OPZKEY        IFEQ      OPDOPZ
172200970526     C     OPZKEY        OREQ      OPDOP1
172300970526     C*
172400970526     C     OPDATT        IFEQ      'S'
172500970526     C                   MOVEL     OPDOPZ        OPZKEY
172600970526     C                   Z-ADD     I             W
172700970526     C                   END
172800970526     C                   END
172900970526     C*
173000970526     C                   END                                                    DO WHILE
173100970526     C*
173200970526     C* OPZIONE NON ATTIVA (ERRORE)
173300970526     C     W             IFEQ      0
173400970526     C*
173500970526     C* OPZIONE NON OK
173600970526     C                   Z-ADD     1             OPZOK
173700970526     C*
173800970526     C* EMISSIONE MESSAGGIO DI ERRORE
173900970526     C                   MOVEL     MSG(1)        DSMSMS
174000970526     C                   ELSE
174100970526     C*
174200970526     C* OPZIONE ATTIVA (PROCEDI ...)
174300970526     C*
174400970526     C     OPZKEY        IFEQ      ' 5'
174500970526     C                   EXSR      O05GES
174600970526     C                   END
174700040927     C     OPZKEY        IFEQ      '15'
174800040927     C                   EXSR      O15GES
174900040927     C                   END
175000970526     C                   END                                                    OPZ.NON ATTIVA
175100130207     C*
175200970526     C                   ENDSR
175300040924
175400040924     ***********************************************************************************************
175500040924     **?
175600040924     **?Connessione al server web.
175700040924     **?
175800040924     ***********************************************************************************************
175900040924     C     ConnectTo     BEGSR
176000040929
176100040924     C/EXEC SQL
176200040924     C+ CONNECT TO AS777
176300040924     C/END-EXEC
176400100107     C                   IF        SQLCod = ConnectionAlreadyExists
176500100107     C/EXEC SQL
176600100107     C+ SET CONNECTION AS777
176700100107     C/END-EXEC
176800100107     C                   ELSEIF    SQLCod < 0
176900040929     C                   GOTO      ErroreSQL
177000040929     C                   ENDIF
177100040924
177200040924     C                   ENDSR
177300040924
177400040924     ***********************************************************************************************
177500040924     **?
177600040929     **?Apertura cursore.
177700040924     **?
177800040924     ***********************************************************************************************
177900040924     C     OpenCursor    BEGSR
178000040924
178100040924     **?Chiudo il cursore già aperto.
178200040924     C                   IF        %LEN(WrkSQLStm) > 0
178300040924     C                   EXSR      CloseCursor
178400040924     C                   ENDIF
178500040924
178600040924     **?Imposto la parte fissa dell'istruzione.
178700130207     C                   SELECT
178800130207     C                   WHEN      %subst(currSysNetA : 1 : 5) <> 'AS888'
178900140123     C                   EVAL      WrkSQLStm = 'with SELEZ as ( -
179000140123     C                             SELECT * FROM STRATEBAR/TIVTL10F'
179100130207     C                   OTHER
179200140123     C                   EVAL      WrkSQLStm = 'with SELEZ as ( -
179300140123     C                             SELECT * FROM TIVTL10F'
179400130207     C                   ENDSL
179500040924
179600040924     **?Selezione e posizionamento.
179700081229     C                   IF        VidDt2 <> 0 OR vidor2 <> 0
179800130206     C                                         OR VidDt1 <> 0
179900130206     C                                         OR (VidKS1 <> *BLANK
180000050114     C                              AND VidKS1 <> *ZERO) OR VidTI1 <> *BLANK
180100130206     C                              OR VidTip <> *BLANK OR VidHS1 <> *BLANK
180200130207     C                              OR VidRpyOpC <> *BLANK
180300130218     C                              OR VidApp <> *BLANK
180400140123     C                              OR VidLink <> *BLANK
180500130218     C                              OR VidClTIPAD <> *BLANK
180600050124     C                              OR (VidSUN <> *ZERO AND VidSUN <> *BLANK)
180700040924     C
180800040924     C                   RESET                   WrkSQLWhere
180900040924     C
181000040924     C                   IF        VidDt1 <> 0
181100040924     C                   IF        WrkSQLWhere <> 'WHERE'
181200040924     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
181300040924     C                   ENDIF
181400041129     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLRPYMMT -
181500040924     C                             BETWEEN '''  + %CHAR(WrkRqsMmt1)
181600040924     C                             + ''' AND ''' + %CHAR(WrkRqsMmt2) + ''''
181700040924     C                   ENDIF
181800040924     C
181900040924     C                   IF        VidKS1 <> *BLANK AND VidKS1 <> *ZERO
182000040924     C                   IF        WrkSQLWhere <> 'WHERE'
182100040924     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
182200040924     C                   ENDIF
182300040924     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLKSU = '''
182400040924     C                             + VidKS1 + ''''
182500040924     C                   ENDIF
182600040924     C
182700050114     C                   IF        VidTI1 <> *BLANK
182800040924     C                   IF        WrkSQLWhere <> 'WHERE'
182900040924     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
183000040924     C                   ENDIF
183100040924     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLTIP = '''
183200050114     C                             + VidTI1 + ''''
183300040924     C                   ENDIF
183400050114     C
183500130206     C                   IF        VidTip = 'S'
183600050114     C                   IF        WrkSQLWhere <> 'WHERE'
183700050114     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
183800050114     C                   ENDIF
183900130206     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLTip = +
184000130206     C                             ''WS'''
184100050114     C                   ENDIF
184200130206     C
184300130206     C                   IF        VidTip = 'N'
184400130206     C                   IF        WrkSQLWhere <> 'WHERE'
184500130206     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
184600130206     C                   ENDIF
184700130206     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLTip <> +
184800130206     C                             ''WS'''
184900130206     C                   ENDIF
185000130206     C
185100130218     C                   IF        VidApp = 'S'
185200130206     C                   IF        WrkSQLWhere <> 'WHERE'
185300130206     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
185400130206     C                   ENDIF
185500130206     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLCLTIPAD +
185600130218     C                             in ( +
185700130207     C                             ''192.168.249.67'' , +
185800130207     C                             ''192.168.249.68'' , +
185900130207     C                             ''192.168.251.67'' )'
186000130206     C                   ENDIF
186100130218     C
186200130218     C                   IF        VidApp = 'N'
186300130218     C                   IF        WrkSQLWhere <> 'WHERE'
186400130218     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
186500130218     C                   ENDIF
186600130218     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLCLTIPAD +
186700130218     C                             not in ( +
186800130218     C                             ''192.168.249.67'' , +
186900130218     C                             ''192.168.249.68'' , +
187000130218     C                             ''192.168.251.67'' )'
187100130218     C                   ENDIF
187200140123     C
187300140123     C                   IF        VidLink = 'S'
187400140123     C                   IF        WrkSQLWhere <> 'WHERE'
187500140123     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
187600140123     C                   ENDIF
187700140123     C                   EVAL      WrkSQLWhere = WrkSQLWhere +
187800141118     C                             ' (substr(VTLRQSDTA , 182 , 1)=''a'' +
187900141118     C                             or substr(VTLRQSDTA , 193 , 1)=''a'')'
188000140123     C                   ENDIF
188100140123     C
188200141105     C                   IF        VidLink = 'N'
188300140123     C                   IF        WrkSQLWhere <> 'WHERE'
188400140123     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
188500140123     C                   ENDIF
188600140123     C                   EVAL      WrkSQLWhere = WrkSQLWhere +
188700141118     C                             ' (substr(VTLRQSDTA , 182 , 1)<>''a'' +
188800141118     C                             and substr(VTLRQSDTA , 193 , 1)<>''a'')'
188900140123     C                   ENDIF
189000050114     C
189100050114     C                   IF        VidHS1 <> *BLANK
189200050114     C                   IF        WrkSQLWhere <> 'WHERE'
189300050114     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
189400050114     C                   ENDIF
189500130206     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLRQSOPC = '''
189600050114     C                             + VidHS1 + ''''
189700050114     C                   ENDIF
189800130218     C
189900130218     C                   IF        VidCltIPAd <> *BLANK
190000130218     C                   IF        WrkSQLWhere <> 'WHERE'
190100130218     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
190200130218     C                   ENDIF
190300130218     C                   EVAL      WrkSQLWhere = WrkSQLWhere +
190400130218     C                              ' VTLCLTIPAD like''' +
190500130218     C                              %trimr(VidCltIPAd) + '%'''
190600130218     C                   ENDIF
190700130207     C
190800130207     C                   IF        VidRpyOpC <> *BLANK
190900130207     C                   IF        WrkSQLWhere <> 'WHERE'
191000130207     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
191100130207     C                   ENDIF
191200130218     C                   SELECT
191300130218     C* se il codice è OK va tradotto con una lista con 0 e DONE
191400130218     C                   WHEN      VidRpyOpC = 'OK'
191500130218     C                   EVAL      WrkSQLWhere = WrkSQLWhere +
191600130218     C                             ' VTLRPYOPC in (''0'' , ''DONE'')'
191700130218     C* se il codice è ERR* va tradotto con un like ERR%
191800130218     C                   WHEN      VidRpyOpC = 'ERR*'
191900130218     C                   EVAL      WrkSQLWhere = WrkSQLWhere +
192000130218     C                             ' VTLRPYOPC like ''' + VidRpyOpC + '%'''
192100130218     C* un tutti gli altri casi si prende il codice
192200130218     C                   OTHER
192300130207     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLRPYOPC = '''
192400130207     C                             + VidRpyOpC + ''''
192500130218     C                   ENDSL
192600130207     C                   ENDIF
192700050124     C
192800050124     C                   IF        VidSUN <> *BLANK AND VidSUN <> *ZERO
192900050124     C                   IF        WrkSQLWhere <> 'WHERE'
193000050124     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
193100050124     C                   ENDIF
193200050124     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLSUN= '
193300050124     C                             + VidSUN
193400050124     C                   ENDIF
193500040924     C
193600040924     C                   IF        VidDt2 <> 0
193700040924     C                   IF        WrkSQLWhere <> 'WHERE'
193800040924     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' AND'
193900040924     C                   ENDIF
194000041129     C                   EVAL      WrkSQLWhere = WrkSQLWhere + ' VTLRPYMMT <= -
194100040924     C                             ''' + %CHAR(WrkRqsMmt2) + ''''
194200040924     C                   ENDIF
194300130207     C
194400130207     C                   EVAL      WrkSQLStm = WrkSQLStm + ' ' + WrkSQLWhere
194500130207     C
194600130207     C                   ENDIF
194700040924
194800140123     **?Select SELEZ.*
194900140123     C                   RESET                   WrkSQLSELEZ
195000140123     C                   EVAL      WrkSQLStm = WrkSQLStm + ' ' + WrkSQLSELEZ
195100140123
195200140123     **?Ordinamento
195300140123     C                   RESET                   WrkSQLOrderBy
195400040924     C
195500130206     C***                SELECT
195600041129     C                   EVAL      WrkSQLOrderBy = WrkSQLOrderBy + ' VTLRPYMMT -
195700040924     C                             DESC'
195800130206E   1C***                ENDSL
195900040924     C
196000040924     C                   EVAL      WrkSQLStm = WrkSQLStm + ' ' + WrkSQLOrderBy
196100040924
196200040924     C                   EVAL      WrkSQLStm = WrkSQLStm + ' FOR READ ONLY'
196300040924
196400040924     **?Chiedo a SQL le righe per una pagina di subfile.
196500050202     C***                EVAL      WrkSQLStm = WrkSQLStm + ' OPTIMIZE FOR '
196600050124     C***                          + %EDITC(SflPag:'Z') + ' ROWS'
196700050202     C***                          + ' 1 ROWS'
196800040924
196900040924     C/EXEC SQL
197000040924     C+ PREPARE TIVTL FROM :WrkSQLStm
197100040924     C/END-EXEC
197200040924
197300040924     C/EXEC SQL
197400140123     C+ DECLARE TIVTL CURSOR FOR TIVTL
197500040924     C/END-EXEC
197600040924
197700040924     C/EXEC SQL
197800040924     C+ OPEN TIVTL
197900040924     C/END-EXEC
198000140123
198100040929     C                   IF        SQLCod < 0
198200040929     C                   GOTO      ErroreSQL
198300040929     C                   ENDIF
198400040924
198500040924     C                   ENDSR
198600040924
198700040924     ***********************************************************************************************
198800040924     **?
198900040924     **?
199000040924     **?
199100040924     ***********************************************************************************************
199200040924     C     FetchCursor   BEGSR
199300040924
199400040924     C/EXEC SQL
199500140123     C+ FETCH NEXT FROM TIVTL INTO :TiVtl10F , :VIDCONT
199600040924     C/END-EXEC
199700040929
199800040929     C                   IF        SQLCod < 0
199900040929     C                   GOTO      ErroreSQL
200000040929     C                   ENDIF
200100040924
200200040929     C                   EVAL      *IN99 = (SQLCod = 100)
200300040924
200400040924     C                   ENDSR
200500040924
200600040924     ***********************************************************************************************
200700040924     **?
200800040929     **?Chiusura cursore.
200900040924     **?
201000040924     ***********************************************************************************************
201100040924     C     CloseCursor   BEGSR
201200040924
201300040924     C/EXEC SQL
201400040924     C+ CLOSE TIVTL
201500040924     C/END-EXEC
201600040924
201700040924     C                   ENDSR
201800040929
201900040929     ***********************************************************************************************
202000040929     **?
202100040929     **?Deconnessione dal server web.
202200040929     **?
202300040929     ***********************************************************************************************
202400040929     C     ConnectReset  BEGSR
202500040929
202600040929     C/EXEC SQL
202700040929     C+ DISCONNECT ALL
202800040929     C/END-EXEC
202900040929
203000040929     C/EXEC SQL
203100040929     C+ CONNECT RESET
203200040929     C/END-EXEC
203300040929
203400040929     C                   ENDSR
203500040924
203600970526     C*------------------------------------------------------------------------*
203700970526     C* NOTE COSTRUZIONE SCHIERE
203800970526     C*------------------------------------------------------------------------*
203900970526     C*
204000970526     C* -MSG-MESSAGGI
204100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
204200970526     C* (NON TOCCARE MSG,1 E MSG,2)
204300970526     C*
204400970526     C* -CMD-COMANDI
204500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
204600970526     C*
204700970526     C* -OPD-OPZIONI
204800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
204900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
205000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
205100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
205200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
205300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
205400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
205500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
205600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
205700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
205800970526     C*
205900970526     C* -FUD-FUNZIONI
206000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
206100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
206200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
206300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
206400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
206500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
206600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
206700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
206800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
206900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
207000991108     C*
207100991115     C* -STS-CODICI STATI DEI RECORD DI LOG
207200991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
207300991110     C* ================> IMPORTANTE <=================
207400991115     C* per codici di errore in -STS- indicare in -DES- sempre
207500991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
207600991110     C* ERR
207700991108     C*
207800970526     O*------------------------------------------------------------------------*
207900970526** DEC - DECODIFICHE
208000970526    GESTIONE                                                     1
208100970526INTERROGAZIONE                                                   2
208200970530     SCELTA                                                      3
2083009705264
2084009705265
208500970526** MSG - MESSAGGI
208600991116TIS7GTR- Opzione non gestita                                                   1
208700991116TIS7GTR- Tasto funzionale non gestito                                          2
208800030225TIS7GTR- 3
208900030225TIS7GTR- 4
209000030225TIS7GTR- 5
209100030225TIS7GTR- 6
209200991116TIS7GTR- 7                                                                     7
209300991116TIS7GTR- 8                                                                     8
209400991116TIS7GTR- 9                                                                     9
209500991116TIS7GTR- 10                                                                   10
209600991116TIS7GTR- 11
209700991116TIS7GTR- Data errata                                                          12
209800081229TIS7GTR- Ora errata                                                           13
209900991116TIS7GTR- 14                                                                   14
210000991116TIS7GTR- 15                                                                   15
210100991116TIS7GTR- 16                                                                   16
210200991116TIS7GTR- 17                                                                   17
210300991116TIS7GTR- 18                                                                   18
210400991116TIS7GTR- 19                                                                   19
210500991116TIS7GTR- 20                                                                   20
210600970528** CMD - COMANDI CL
210700991110DSPPFM FILE(
210800991110) MBR(
210900991110
211000970528
211100970528
211200970526** OPD - OPZIONI
211300991116                   -  -     (S/N)=N                        opz. 01¬
211400991110                   -  -     (S/N)=N                        opz. 02¬
211500991110                   -  -     (S/N)=N                        opz. 03¬
211600991116                   -  -     (S/N)=N                        opz. 04¬
211700040927 5=Visual. Dett.   -5 -     (S/N)=S                        opz. 05¬
211800991115                   -  -     (S/N)=N                        opz. 06¬
211900991110                   -  -     (S/N)=N                        opz. 07¬
212000991110                   -  -     (S/N)=N                        opz. 08¬
212100991110                   -  -     (S/N)=N                        opz. 09¬
212200991110                   -  -     (S/N)=N                        opz. 10¬
212300991115                   -  -     (S/N)=N                        opz. 11¬
212400970526                   -  -     (S/N)=N                        opz. 12¬
212500970526                   -  -     (S/N)=N                        opz. 13¬
212600991116                   -  -     (S/N)=N                        opz. 14¬
21270004092715=Dettagli        -15-     (S/N)=S                        opz. 15¬
212800970526                   -  -     (S/N)=N                        opz. 16¬
212900970526                   -  -     (S/N)=N                        opz. 17¬
213000970526                   -  -     (S/N)=N                        opz. 18¬
213100970526                   -  -     (S/N)=N                        opz. 19¬
213200970526                   -  -     (S/N)=N                        opz. 20¬
213300970526                   -  -     (S/N)=N                        opz. 21¬
213400970526                   -  -     (S/N)=N                        opz. 22¬
213500970526                   -  -     (S/N)=N                        opz. 23¬
213600991116                   -  -     (S/N)=N                        opz. 25¬
213700970526** FUD - FUNZIONI
213800970526                   -        (S/N)=N                        funz.01¬
213900970526                   -        (S/N)=N                        funz.02¬
214000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
214100970526                   -        (S/N)=N                        funz.04¬
214200970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
214300991108                   -        (S/N)=N                        funz.06¬
214400970526                   -        (S/N)=N                        funz.07¬
214500970527                   -        (S/N)=N                        funz.08¬
214600970527                   -        (S/N)=N                        funz.09¬
214700970527                   -        (S/N)=N                        funz.10¬
214800970526                   -        (S/N)=N                        funz.11¬
214900970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
215000991116                   -        (S/N)=N                        funz.13¬
215100970526                   -        (S/N)=N                        funz.14¬
215200970526                   -        (S/N)=N                        funz.15¬
215300970526                   -        (S/N)=N                        funz.16¬
215400970526                   -        (S/N)=N                        funz.17¬
215500970526                   -        (S/N)=N                        funz.18¬
215600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
215700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
215800991116                   -        (S/N)=N                        funz.21¬
215900991116                   -        (S/N)=N                        funz.22¬
216000991116                   -        (S/N)=N  <== S-FISSO           funz.23¬
216100991116                   -        (S/N)=N  <== S-FISSO           funz.24¬
216200991116** TIP - STATI DEI RECORD DI LOG
216300100107DB
216400100107DM
216500100107HB
216600141105IC
216700100107LI
216800100107LV
216900100107RB
217000100107SG
217100120308ST
217200100107TC
217300100107TM
217400100107TO
217500100107TR
217600100107T1
217700100107T2
217800100107T3
217900111025WS
218000100107** DES - STATI DEI RECORD DI LOG
218100100107DETTAGLIO BOLLA
218200100107RICERCA RIF. MITTENTE
218300141105HERBALIFE RIC. RIF. MITT.
218400141105ISTRUZIONI L/AVVISO
218500141105LOGIN IP CHECK
218600100107POD IMAGE
218700141105RICERCA BOLLE AVANZATA
218800100107STORIA GIACENZA
218900120504CALCOLO COSTO SPEDIZIONE
219000100107ELENCO COLLI
219100100107TEMPI CONSEGNA
219200141105ORM IMMISSIONE
219300141105DISPOSIZIONI GIACENZA
219400141105ESITI CONSEGNA
219500100107RINTRACCIA GIACENZE
219600141105TRACKING ORM
219700111025WEB SERVICE
219800130207** RQS - Codice e Descrizione Richieste
219900130218ALFA08              T&T - Ricerca sped. riferimento 8A
220000130218CHKIPLOGIN          Login
220100130218CHECKIMM            ORM - Immissione multipla
220200130218COLLISPED           T&T - Elenco colli
220300130218CONFERMA            ORM - Immissione
220400130207DETTAGLIO           T&T - Dettaglio spedizione
220500140219DETTAGLIOT3         T&T - Dettaglio ORM
220600130218DOCMIT              T&T - Ricerca con rif. mittente RMN
220700141118GETBOLLA            T&T - Dettaglio spedizione
220800141118GETISTRUCO          T&T - Ricerca/Check BRTcode
220900141022GETSTOGIA           T&T - Storia giacenza
221000130218GETTAMCALC          Calcolo costo spedizione
221100130207GETTEMPI            Tempi di consegna
221200130218INVIADISP           Disposizioni giacenza
221300130218LISTA               Lista ORM
221400130218LOAD                T&T - Ricerca spedizioni avanzata
221500130218LOAD                T&T - Ricerca stor.con RMN
221600130218LOADSPE             T&T - Rintraccia giacenze
221700130218LOADSPE             T&T - Esiti consegna
221800141118PUTISTRUCO          T&T - Immiss.Disp.Consegna
221900130218RTVIMG              T&T - POD image
222000130218RTVIMGXML           T&T - POD image XML
222100130218STAMSPE             T&T - Esiti consegna
222200130218WSERLO010W          WS Logistica - Avanzamento stato OC
222300140513WSERLO020W          WS Logistica - Situazione Articolo
222400130218WSER001000          GetDatiSpedizioneByIdSpedizione
222500130207WSER002000          GetIdSpedizioneByRMN
222600130218WSER003000          GetIdSpedizioneByRMA
222700130207WSER004000          GetIdSpedizioneByIdCollo
222800130218WSER005000          GetLegendaEsiti
222900130218WSER006000          GetLegendaEventi
223000140513WSER00700W          GetDatiSpedizioneByBRTCode
223100130208** RPY - Codice Replay
223200130218LOGIN               Login area utente
223300130218100                 Fine dati
2234001302182                   Lingua sconosciuta o non gestita. I dati restituiti in lingua ITA
223500130218OK                  Elaborazione eseguita con successo
223600130218-1                  Errore sconosciuto o non previsto
223700130218-3                  In questo momento non è possibile connettersi al database
223800130218-10                 ID spedizione non valido o non passato
223900130218-11                 Spedizione non trovata
224000130218-20                 Riferimento mittente numerico obbligatorio o non valido
224100130218-21                 ID cliente non valido o non passato
224200130218-22                 Riferimento non univoco
224300130218-23                 Riferimento mittente alfabetico obbligatorio
224400130218-30                 ID collo obbligatorio
224500130218ERR*                Errore
