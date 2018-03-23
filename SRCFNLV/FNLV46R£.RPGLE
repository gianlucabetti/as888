000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941216      * FNLV46R  *---------------------------------------------------*
000300901119      *         - STAMPA TERMINAL ITALIA
000400900515      *--------------------------------------------------------------*
000500901119     FAZORG04L  IF   E           K DISK
000600981203     FAZORG01L  IF   E           K DISK
000700981203     F                                     RENAME(AZORG:AZORG001)
000800940310     FCNACO00F  IF   E           K DISK
000900021120     FTNVIS05L  IF   E           K DISK    usropn
001000981203     FTNCPO01L  IF   E           K DISK
001100900523     FTABEL00F  IF   E           K DISK
001200000920     FLET017    O    F  124        PRINTER OFLIND(*INOF)
001300000920     F                                     USROPN
001400041217     f                                     infds(PrtInfo)
001500041216     F                                     FORMLEN(66)
001600041216     F                                     FORMOFL(63)
001700900515      *
001800041217      *   S C H I E R E
001900041217      *
002000060714     D CMD             S             80    DIM(4) CTDATA PERRCD(1)
002100000421     D ES              S             80    DIM(10) CTDATA PERRCD(1)
002200041217     d POG             S              3    DIM(250)
002300041217     d $Dett           s                   like(ds_Dett) dim(250) inz
002400070314     D LET1            S             21    DIM(2) CTDATA PERRCD(1)
002500070314     d let2            s             22    dim(2) ctdata perrcd(1)
002600070314     d let3            s             24    dim(2) ctdata perrcd(1)
002700070314     d let4            s             33    dim(2) ctdata perrcd(1)
002800041217      *
002900041217      *   S T R U T T U R E - D A T I
003000041217      *
003100041217      * - Parametri x Controllo profilo utenti
003200041217     d TIBS34ds      e ds
003300041217      * - Ds di riferimento al file esterno AZUTE00F
003400041217     d AZUTEds       e ds                  extname(AZUTE00F)
003500041217      * - Ds per dati organigramma
003600041217     d dDATIUTE      e ds
003700900514     D KPJBA         E DS
003800020213     d og143         e ds
003900960424     D OG148         E DS
004000900516      *
004100981113     D* PASSAGGIO DATI
004200981113     D DSLV45        E DS                  EXTNAME(FNLV45DS)
004300981203     D DSMK25        E DS                  EXTNAME(TRMK25DS)
004400981203     D DSCPS         E DS                  EXTNAME(TNCPS00F)
004500021108      *
004600021108     d Trul31ds      e ds                  INZ
004700041217      *
004800041217     d PrtInfo         ds
004900041217     d  CurLine              367    368I 0
005000041217      *
005100041217      * struttura-dati per memorizzare una singla riga di stampa nella
005200041217      *  schiera $Dett
005300041217     d ds_Dett         ds
005400041217     d   ORGdes
005500041217     d   ORGind
005600041217     d   ORGcpf
005700041217     d   ORGloc
005800041217     d   ORGpro
005900041217     d   ORGtel
006000041217     d   ORGfax
006100041217     d   ORGdea
006200041217     d   wOGntw                            like(§OGntw)
006300041217      *
006400041217      *   V A R I A B I L I
006500041217      *
006600041217      * - Indici di schiera
006700041217     d XX              s              3  0 inz
006800041217     d YY              s              3  0 inz
006900041217      * - Codice Regione x test "rottura"
007000041217     d SavCRE          s              3  0 inz
007100041217      * - Decodifica cod. regione
007200041217     d DesCRE          s             20    inz
007300041217      * - Num. giro x regione
007400041217     d Giro            s              1    inz
007500041217      * - Flag "Divisione" (da tab. 10/1)
007600041217     d DivSN           s              1    inz
007700041217      * - 1ª intestazione (da tab. 10/2)
007800041217     d INTES1          s             75    inz
007900041217      * - 2ª intestazione (da tab. 10/2)
008000041217     d INTES2          s             75    inz
008100041217      * - 3ª intestazione (da tab. 10/3)
008200041217     d INTES3          s             75    inz
008300041217      * - Campi di comodo
008400041217     d CDU             s              3  0 inz
008500041217     d Lung            s             15  5 inz
008600041217     d Comman          s            100    inz
008700041217     d Wcpo            s              3    inz
008800041217     d Wok             s              1    inz
008900041217     d w003a           s              3    inz
009000050104     c* - Costante di stampa
009100050104     D COS1            C                   CONST('     Per informazioni sulle n-
009200050104     D                                     azioni e località raggiunte dai serv-
009300050104     D                                     izi estero visitare il sito Internet-
009400050104     D                                      www.bartolini.it o ')
009500050104     D COS2            C                   CONST('     contattare l''assistenza-
009600050104     D                                      clienti.                           -
009700050104     D                                                                         -
009800050104     D                                                          ')
009900060630     d cos12           c                   const('     For information on the c-
010000060630     d                                     ountries and towns served by oversea-
010100060630     d                                     s services, please visit the Interne-
010200060724     d                                     t www.bartolini.it ')
010300060630     d cos22           c                   const('     or contact customer assi-
010400060724     d                                     stance.                             -
010500060612     d                                                                         -
010600060913     d                                                        ')
010700060804     d cos13           c                   const('     Pour tout renseignement -
010800060804     d                                     sur les pays et les communes desserv-
010900060804     d                                     ies par les services étrangers, veui-
011000060804     d                                     llez visiter le site')
011100060804     d cos23           c                   const('     Internet www.bartolini.i-
011200060913     d                                     t ou contacter le service clients.  -
011300060612     d                                                                         -
011400060612     d                                                         ')
011500060804     d cos14           c                   const('     Wenn Sie Informationen ü-
011600060804     d                                     ber die vom Auslandsservice bediente-
011700060804     d                                     n Länder wünschen, konsultieren Sie -
011800060804     d                                     die Website         ')
011900060804     d cos24           c                   const('     www.bartolini.it oder we-
012000060804     d                                     nden Sie sich direkt an unseren Kund-
012100060804     d                                     enservice.                          -
012200060612     d                                                         ')
012300050104      *
012400900515      *---------------------------------------------------------------*
012500000000     C     *ENTRY        PLIST
012600000000     C                   PARM                    KPJBA
012700981113     C                   PARM                    DSLV45
012800900516      *
012900041217     c     *dtaara       define    §azute        AZUTEds
013000041217     c     *dtaara       define    §datiute      dDATIUTE
013100041217     c                   in(E)     *dtaara
013200041217     c                   if        %ERROR or RSUT = *blanks
013300041217     c                   clear                   Tibs34Ds
013400041217     c                   call      'TIBS34R'
013500041217     c                   parm                    Tibs34Ds
013600041217     c                   in        *dtaara
013700041217     c                   endif
013800041217      *
013900021121      * per Simfel <> *zeros apro file visite e indirizzi
014000041217     C                   IF        DutLpo <> 'S'
014100021121     C                   open      TnVis05l
014200021121     C                   ENDIF
014300060612      * controllo in che lingua devo stampare
014400060612      * italiano
014500060613     c                   eval      *in11 = (i45lin = 'I')
014600060612      * inglese
014700060613     c                   eval      *in12 = (i45lin = '2')
014800060612      * francese
014900060613     c                   eval      *in13 = (i45lin = '3')
015000060612      * tedesco
015100060613     c                   eval      *in14 = (i45lin = '4')
015200900515      *---------------------------------------------------------------*
015300900515      * CHIAVI                                                        *
015400900515      *---------------------------------------------------------------*
015500041217      * ACCESSO X CODICE SU TABEL
015600901025     C     TABKEY        KLIST
015700041217     C                   KFLD                    TBLkut
015800901025     C                   KFLD                    COD               2
015900901025     C                   KFLD                    KEY               8
016000041217     c                   eval      TBLkut = 1
016100900515      *---------------------------------------------------------------*
016200900515      * FLUSSO PRINCIPALE                                             *
016300900515      *---------------------------------------------------------------*
016400041217      * IMPOSTAZIONI INIZIALI
016500940310     C                   EXSR      IMPO
016600041217      *
016700041217    1C                   SELECT
016800041217      *
016900041217      * CLIENTI
017000041217      *
017100041217    1C     I45AST        WHENEQ    'C'
017200940311     C     *LOVAL        SETLL     CNACO00F
017300940311     C                   READ      CNACO00F                               31
017400041217      *
017500041217    2C     *IN31         DOWEQ     *OFF
017600041217      * STAMPA I TERMINALS
017700940311     C                   EXSR      STAMPA
017800940311     C                   READ      CNACO00F                               31
017900041217    2C                   ENDDO
018000041217      *
018100041217      * VISITE
018200041217      *
018300041217    1C     I45AST        WHENEQ    'V'
018400941216     C     *LOVAL        SETLL     TNVIS000
018500941216     C                   READ      TNVIS000                               31
018600041217      *
018700041217    2C     *IN31         DOWEQ     *OFF
018800041217      * STAMPA I TERMINALS
018900940311     C                   EXSR      STAMPA
019000941216     C                   READ      TNVIS000                               31
019100041217    2C                   ENDDO
019200041217      *
019300041217      * POTENZIALI
019400041217      *
019500041217    1C     I45AST        WHENEQ    'P'
019600981203     C     *LOVAL        SETLL     TNCPO01L
019700981203     C                   READ      TNCPO01L                               31
019800041217      *
019900041217    2C     *IN31         DOWEQ     *OFF
020000041217      * Se non sono in sede controllo che il cliente sia dell'area
020100041217    3C     DutLpo        IFne      'S'
020200981203     C     SAVFLT        ANDNE     CPOFLT
020300981203     C                   MOVEL     CPOFLT        SAVFLT
020400981203    3C                   ENDIF
020500041217      *
020600041217     C                   movel     cpoflt        wcpo
020700021108     C     wcpo          lookup    POG                                    30
020800021108    4C                   IF        *in30 = *on
020900041217     C                   MOVEL     'S'           WOK
021000041217    4C                   ENDIF
021100041217      *
021200041217      * SE EFFETTUATA SELEZIONE PER COMMERCIALE STATO DATA STATO
021300041217      *  DEVO REPERIRE ULTIMO STATO DEL CLIENTE
021400041217      *  DEVE COMUNQUE AVERE ALMENO UNO STATO NELL'AZIENDA IN CUI SONO
021500981203     C                   CLEAR                   DSMK25
021600981203     C                   MOVEL     CPOCPO        I25CPO
021700981203     C                   MOVEL     CDUDIT        I25DIT
021800981203     C                   MOVEL     DSMK25        KPJBU
021900981203     C                   CALL      'TRMK25R'
022000981203     C                   PARM                    KPJBA
022100981203     C                   PARM                    DSCPS
022200981203     C                   MOVEL     KPJBU         DSMK25
022300041217      *
022400041217    4C     O25ERR        IFEQ      ' '
022500041217      * SE PARZIALIZZATO PER STATO
022600041217    5C     I45FST        IFNE      *BLANKS
022700981203     C     CPSFST        ANDNE     I45FST
022800981203     C                   CLEAR                   WOK
022900041217    5C                   ENDIF
023000041217      *
023100041217    5C     I45ADD        IFNE      0
023200981203     C     CPSDST        IFLT      I45ADD
023300981203     C     CPSDST        ORGT      I45ADA
023400981203     C                   CLEAR                   WOK
023500981203     C                   ENDIF
023600041217    5C                   ENDIF
023700041217      *
023800981203     C     I45CMM        IFNE      0
023900981203     C     CPSCMM        ANDNE     I45CMM
024000981203     C                   CLEAR                   WOK
024100981203     C                   ENDIF
024200041217   X4C                   ELSE
024300041217      *
024400041217      * SE NON TROVATO NESSUNO STATO TORNO A LEGGERE
024500981203     C                   CLEAR                   WOK
024600041217    4C                   ENDIF
024700041217      * STAMPA I TERMINALS
024800041217    4C     WOK           IFEQ      'S'
024900940311     C                   EXSR      STAMPA
025000041217    4C                   ENDIF
025100041217      *
025200981203     C                   READ      TNCPO01L                               31
025300041217    2C                   ENDDO
025400041217      *
025500041217      *
025600041217      * LIBERO
025700041217      *
025800041217    1C     I45AST        WHENEQ    'L'
025900940330     C                   EXSR      STAMPA
026000041217      *
026100041217    1C                   ENDSL
026200041217      *
026300981203     C                   CLEAR                   DSMK25
026400981203     C                   MOVEL     'C'           I25TLA
026500981203     C                   MOVEL     DSMK25        KPJBU
026600981203     C                   CALL      'TRMK25R'
026700981203     C                   PARM                    KPJBA
026800981203     C                   PARM                    DSCPS
026900041217      *
027000940311     C                   SETON                                        LR
027100041217      *
027200041217      * STAMPA TERMINALS ---------------------------------------------*
027300940311     C     STAMPA        BEGSR
027400041217     C                   Z-ADD     0             SAVCRE
027500041217      * STAMPO NUOVA TESTATA
027600901119     C                   EXCEPT    TESTA
027700041216     c                   if        INTES2 <> *blanks
027800980217     C  N80              EXCEPT    TES2
027900041216     c                   endif
028000041217      *
028100940311     C     *LOVAL        SETLL     AZORG
028200940311     C                   READ      AZORG                                  30
028300041217      *
028400940311    1C     *IN30         DOWEQ     *OFF
028500960424     C                   MOVEL     ORGDE8        OG148
028600041217      * SE PUNTO OPERATIVO NON ANNULLATO E SE E' FILIALE O AGENZIA
028700940311    2C     ORGFVA        IFEQ      *BLANKS
028800901121     C     ORGFAG        ANDNE     'V'
028900041217      *
029000041217      * CONTROLLO CAMBIO REGIONE
029100940311    3C     ORGCRE        IFNE      SAVCRE
029200041217      * DECODIFICO LA REGIONE
029300980402     C                   MOVEL     ORGCRE        KEY
029400980402     C     TABKEY        CHAIN     TABEL                              32
029500041217     C   32              MOVEL     *BLANKS       DESCRE
029600041217     C  N32              MOVEL     TBLUNI        DESCRE
029700980402     C                   MOVEL     ORGCRE        SAVCRE
029800041217      * SPENGO L'INDICATORE CHE MI CONDIZIONA LA STAMPA DELLA NUOVA REGIONE
029900980402     C                   SETOFF                                       91
030000041217      * FACCIO IL PRIMO GIRO DI PER LA PRIMA STAMPA
030100041217     C                   MOVE      '1'           GIRO
030200980402     C                   EXSR      DIVDUE
030300041217      * NEL CASO CI SIANO PIU' SUDDIVISIONI LEGGO PER ALTRE 2 VOLTE LA STESSA
030400041217      * REGIONE
030500980402    4C     *IN80         IFEQ      *ON
030600980331     C                   MOVEL     '2'           GIRO
030700980217     C                   EXSR      DIVDUE
030800980331     C                   MOVEL     '3'           GIRO
030900980331     C                   EXSR      DIVDUE
031000980402    4C                   END
031100980402     C     SAVCRE        SETGT     AZORG
031200980402    3C                   END
031300940311    2C                   ENDIF
031400041217      *
031500940311     C                   READ      AZORG                                  30
031600940311    1C                   ENDDO
031700041217      *
031800041217      * STAMPO I DATI RELATIVI ALL'ESTERO
031900041217      *
032000041217if  1c                   if            *inOF = *on
032100050104     c                             or  CurLine > 65 - 4
032200041217     c                   except    TESTA
032300041217     c                   eval      *inOF = *off
032400041217e   1c                   endif
032500050104     C                   EXCEPT    ESTER1
032600050104     C                   EXCEPT    ESTER2
032700041217      *
032800940311     C                   ENDSR
032900041217      *--------------------------------------------------------------*
033000041217      * CONTROLLLO VALIDITA' RECORD IN STAMPA -----------------------*
033100041217      *--------------------------------------------------------------*
033200980216     C     CTRREC        BEGSR
033300041217      *
033400980217     C                   SETOFF                                       70
033500041217      *  SIA CHE SIA IN BAR CHE IN SDI CONTROLLO I FLAGS RELATIVI ALLA
033600041217      * BARTOLINI SIA CHE SI TRATTI DI UNA STAMPA CON DIVISIONE CHE NON
033700041217      *  80 = OFF DIVISIONE NO
033800980331    2C     *IN80         IFEQ      *OFF
033900041217      * CONTROLLO FLAG ORGANIGRAMMA BARTOLINI CHE SIANO UGUALI A "S"
034000980331    3C     §OGBD1        IFNE      'S'
034100980217     C     §OGBD2        ORNE      'S'
034200980217     C                   SETON                                        70
034300980331    3C                   END
034400041217      *  80 = ON  DIVISIONE SI
034500980331   X2C                   ELSE
034600980331    3C     GIRO          IFEQ      '1'
034700041217      * SE E' IL PRIMO GIRO PER REGIONE CONTROLLO CHE PER BARTOLINI IL PRIMO
034800041217      * FLAG BARTOLINI SIA = S E IL 2° BLANK
034900980331     C     §OGBD1        IFEQ      'S'
035000980331     C     §OGBD2        ANDEQ     ' '
035100980331     C                   ELSE
035200980217     C                   SETON                                        70
035300980331     C                   END
035400980331    3C                   END
035500980331    3C     GIRO          IFEQ      '2'
035600041217      * SE E' IL SECONDO GIRO PER REGIONE CONTROLLO CHE PER BARTOLINI IL
035700041217      * SECONDO FLAG BARTOLINI SIA = S E IL 1° BLANK
035800980331     C     §OGBD2        IFEQ      'S'
035900980331     C     §OGBD1        ANDEQ     ' '
036000980331     C                   ELSE
036100980217     C                   SETON                                        70
036200980331     C                   END
036300980331    3C                   END
036400980331    3C     GIRO          IFEQ      '3'
036500041217      * SE E' IL TERZO GIRO PER REGIONE CONTROLLO CHE PER BARTOLINI IL
036600041217      * SECONDO FLAG BARTOLINI SIA = S E IL 1° = S
036700980331    4C     §OGBD2        IFEQ      'S'
036800980331     C     §OGBD1        ANDEQ     'S'
036900980331   X4C                   ELSE
037000980330     C                   SETON                                        70
037100980331    4C                   END
037200980331    3C                   END
037300980331    2C                   END
037400041217      *
037500980217     C                   ENDSR
037600041217      *--------------------------------------------------------------*
037700041217      * Elaboro i P.O. della regione in corso di elaborazione        *
037800041217      *--------------------------------------------------------------*
037900041217     c     DIVDUE        BEGSR
038000041217      *
038100041217      * Rirpistino l'indicatore per stampare l'intestazione della
038200041217      *   divisione 2/3
038300041217     c                   eval      *in90 = *off
038400041217      *
038500041217      * Pulisco la schiera in cui sto per memorizzare i dati di tutti
038600041217      *  i p.o. della regione che si sta elaborando: PRIMA di stampare
038700041217      *  il primo p.o. devo verificare che TUTTI i p.o di 'sta regione
038800041217      *  stiano in questa pagina; altrimenti salto PRIMA su pagina nuova.
038900041217     c                   clear                   xx
039000041217     c                   clear                   $Dett
039100041217      *
039200041217      * PRIMA conteggio le righe da stampare intabellandone i dati
039300041217      *
039400041217     c     SavCRE        setll     AZORG
039500041217     c                   read      AZORG
039600041217      *
039700041217do  1c                   DOW       NOT %eof(AZORG04L)
039800041217      *
039900041217     c                   movel     ORGde3        og143
040000041217     c                   eval      wOGntw =  §OGntw
040100041217     c                   movel     ORGde8        og148
040200041217      *
040300041217if  2c                   IF            ORGfva =  *blanks
040400041217     c                             and ORGfag <> 'V'
040500041217      *
040600041217if  3c                   if        ORGcre <> SavCRE
040700041217     c                   leave
040800041217e   3c                   endif
040900041217      *
041000041217      * Controllo validità records letti
041100041217     c                   exsr      CtrRec
041200041217      *
041300041217      * 70 = ON record NON valido
041400041217if  3c                   IF        *in70 = *off
041500041217      *
041600041217      * - Conteggio intestazione alla regione
041700041217if  4c                   if        *in91 = *off
041800041217     c                   add       1             xx
041900041217i    c                   movel     DesCRE        $Dett(xx)
042000041217     c                   eval      *in91 = *on
042100041217e   4c                   endif
042200041217      * - Varie intestazioni in relazione al giro che sto facendo
042300041217sel 4c  n90              select
042400041217w   4c                   when      Giro  = '1'
042500041217     c                   add       1             xx
042600041217     c   80              movel     Intes1        $Dett(xx)
042700041217     c                   eval      *in90 = *on
042800041217w   4c                   when      Giro  = '2'
042900041217     c                   add       1             xx
043000041217     c                   movel     Intes2        $Dett(xx)
043100041217     c                   eval      *in90 = *on
043200041217w   4c                   when      Giro  = '3'
043300041217     c                   add       1             xx
043400041217     c                   movel     Intes3        $Dett(xx)
043500041217     c                   eval      *in90 = *on
043600041217e   4c                   endsl
043700041217      * - Controllo se si tratta di un P.O. estero
043800041217      *   (per il quale dovrò stampare 2 righe)
043900041217if  4c                   if        §OGntw = 'DPD' or §OGntw = 'FED' or
044000041217     c                             §OGntw = 'EEX' or §OGntw = 'EUP'
044100041217     c                   add       1             xx
044200041217     c                   movel     ds_Dett       $Dett(xx)
044300041217e   4c                   endif
044400041217      * - Conteggio singolo dettaglio
044500041217     c                   add       1             xx
044600041217     c                   movel     ds_Dett       $Dett(xx)
044700041217      *
044800041217e   3c                   ENDIF
044900041217      *
045000041217e   2c                   ENDIF
045100041217      *
045200041217      * - Leggo record successivo
045300041217     c                   read      AZORG
045400041217e   1c                   ENDDO
045500041217      *
045600041217      * POI stampo le righe di cui ho appena intabellato i dati
045700041217      *
045800041217      * Rirpistino l'indicatore per stampare l'intestazione della
045900041217      *   regione
046000041217     c                   eval      *in91 = *off
046100041217      * Rirpistino l'indicatore per stampare l'intestazione della
046200041217      *   divisione 2/3
046300041217     c                   eval      *in90 = *off
046400041217      *
046500041217      * Se NON ci stanno sulla pagina corrente: salto alla successiva.
046600041217      * Se però dovessero comparire più di 60 p.o. per la regione: NON
046700041217      *   ci starebbero comunque in un'unica pagina, perciò... vaff...
046800041217      *   al conteggio dei p.o. da stampare in singola pagina !
046900041217      * - Testo l'overflow
047000041217if  1c                   if            *inOF = *on
047100041217     c                             or  CurLine + xx > 65
047200041217     c                             and           xx < 63
047300041217     c                   except    PAG
047400041217     c                   eval      *inOF = *off
047500041217e   1c                   endif
047600041217      *
047700041217do  1c                   DO        xx            yy
047800041217      *
047900041217sel 2c                   SELECT
048000041217      * - Stampo la regione
048100041217w   2c                   WHEN      yy    = 1
048200041217     c                   except    TES1
048300041217      * - Varie intestazioni in relazione al giro che sto facendo
048400041217w   2c                   WHEN      yy    = 2
048500041217sel 3c                   select
048600041217w   3c                   when      Giro  = '1'  and  *in80 = *on
048700041217     c                   except    TES2
048800041217     c                   eval      *in90 = *on
048900041217w   3c                   when      Giro  = '2'
049000041217     c                   except    TES3
049100041217     c                   eval      *in90 = *on
049200041217w   3c                   when      Giro  = '3'
049300041217     c                   except    TES4
049400041217     c                   eval      *in90 = *on
049500041217e   3c                   endsl
049600041217      * - Dettaglio
049700041217x   2c                   OTHER
049800041217     c                   movel     $Dett(yy)     ds_Dett
049900041217      * - - Testo l'overflow
050000041217if  1c                   if        *inOF
050100041217     c                   except    PAG
050200041217     c                   eval      *inOF = *off
050300041217e   1c                   endif
050400041217      * - - Controllo se si tratta di un P.O. estero
050500041217if  3c                   if        wOGntw = 'DPD' or wOGntw = 'FED' or
050600041217     c                             wOGntw = 'EEX' or wOGntw = 'EUP'
050700041217     c                   eval      *in50  = *on
050800041217x   3c                   else
050900041217     c                   eval      *in50  = *off
051000041217e   3c                   endif
051100041217     c                   except    DET
051200041217      *
051300041217e   2c                   ENDSL
051400041217      *
051500041217e   1c                   ENDDO
051600041217      *
051700041217     c                   ENDSR
051800041217      *
051900041217      * IMPOSTAZIONI INIZIALI ----------------------------------------*
052000940310     C     IMPO          BEGSR
052100021121      *
052200041217    1C     DutLpo        IFne      'S'
052300021108      *
052400021108      * Chiamo il trul31r per reperire schiera po
052500021108      * stesso raggruppamento po utente /stesso AS
052600021108     C                   CLEAR                   TRUL31DS
052700041214     C                   EVAL      I31ABI   = 'RA'
052800041217     C                   EVAL      I31CPO   = DutPou
052900021108     C                   CALL      'TRUL31R'
053000021108     C                   PARM                    KPJBA
053100021108     C                   PARM                    TRUL31DS
053200021108if   C                   IF        O31POG > *zeros
053300021108     C                   MOVEA     O31POG        POG
053400021108x    C                   ENDIF
053500981203     C     *LIKE         DEFINE    CPOFLT        SAVFLT
053600041217     C                   MOVEL     DutFel        CDU
053700981203     C*
053800041217     c     DutLpo        ifeq      '2'
053900041217     c     DutLpo        oreq      *blanks
054000041217     c                   z-add     DutPou        cdu
054100981203    2C                   ENDIF
054200020827      *
054300041217     C     DutLpo        IFEQ      'S'
054400020827     C                   MOVEL     *BLANKS       CDUDIT
054500020827     C                   ELSE
054600041217      * DIT DI CDU
054700981203     C     CDU           CHAIN     AZORG01L                           30
054800981203     C   30              MOVEL     *BLANKS       CDUDIT
054900981203     C  N30              MOVEL     ORGDIT        CDUDIT
055000020827     C                   ENDIF
055100981203    1C                   END
055200041217      *
055300020827     C     *LIKE         DEFINE    ORGDIT        CDUDIT
055400041217      *
055500041217      * AGGANCIO LA TABELLA "10" PER VEDERE SE LA STAMPA DEVE AVERE SIA LE
055600041217      * FILIALI SDI CHE QUELLE BARTOLINI  E PER RECUPERARE L'INTESTAZIONE
055700041217      * CHE DEVO METTERE IN STAMPA DEI PUNTI OPERATIVI
055800980213     C                   MOVEL     '10'          COD
055900980213     C                   MOVEL     *BLANKS       KEY
056000980213     C                   MOVEL     '1'           KEY
056100980213     C     TABKEY        CHAIN     TABEL                              32
056200980213     C   32              MOVEL     *BLANKS       TBLUNI
056300041217     C                   MOVEL     TBLUNI        DIVSN
056400041217      * SE LA DIVISIONE E' S = SI ACCENDO INDICATORE 80
056500980213     C     DIVSN         IFEQ      'S'
056600980213     C                   SETON                                        80
056700980213     C                   END
056800041217      * CARICO IN CAMPI DI COMODO LE DUE INTESTAZIONE PUNTI OPERATIVI
056900980213     C                   MOVEL     '2'           KEY
057000980213     C     TABKEY        CHAIN     TABEL                              32
057100980213     C   32              MOVEL     *BLANKS       TBLUNI
057200041217     C                   MOVEL     TBLUNI        INTES1
057300041217      * 2° INTESTAZIONE
057400980217     C                   MOVEL     '3'           KEY
057500980213     C     TABKEY        CHAIN     TABEL                              32
057600980213     C   32              MOVEL     *BLANKS       TBLUNI
057700041217     C                   MOVEL     TBLUNI        INTES2
057800041217      * 3° INTESTAZIONE
057900980330     C                   MOVEL     '4'           KEY
058000980330     C     TABKEY        CHAIN     TABEL                              32
058100980330     C   32              MOVEL     *BLANKS       TBLUNI
058200041217     C                   MOVEL     TBLUNI        INTES3
058300041217      *
058400041217      * OVRPRTF SU FILE PRINTER
058500041217      *
058600060714      * in lingua
058700041217     C                   MOVEL     I45NCO        W003A
058800060714     C**!!!              MOVEA     W003A         CMD(39)
058900060714     c   11              eval      %subst(cmd(1):39:3) = w003a
059000060714     c   12              eval      %subst(cmd(2):39:3) = w003a
059100060714     c   13              eval      %subst(cmd(3):39:3) = w003a
059200060714     c   14              eval      %subst(cmd(4):39:3) = w003a
059300940310      *
059400940310      * Eseguo OVRPRTF sul file di stampa
059500041217     C                   Z-ADD     100           LUNG
059600041217     C                   MOVEL     *BLANKS       COMMAN
059700060714     C**!!!              MOVEA     CMD           COMMAN
059800060714     c   11              eval      comman = cmd(1)
059900060714     c   12              eval      comman = cmd(2)
060000060714     c   13              eval      comman = cmd(3)
060100060714     c   14              eval      comman = cmd(4)
060200940310     C                   CALL      'QCMDEXC'                            H1
060300940310     C                   PARM                    COMMAN
060400940310     C                   PARM                    LUNG
060500000920     C                   OPEN      LET017
060600041217      * PREPARO LA CHIAVE PER LA DECODIFICA DELLE REGIONI
060700980213     C                   MOVEL     '14'          COD
060800940310     C                   ENDSR
060900041217      *
061000900517      *---------------------------------------------------------------*
061100041217      *
061200041216     OLET017    E            TESTA            09
061300070314     O               11      LET1(1)             70
061400070314     o               12      let2(1)             70
061500070314     o               13      let3(1)             70
061600060804     o               14      let4(1)             75
061700901119      *
061800901119     O          E            TESTA       0  0
061900070314     O               11      LET1(1)             70
062000070314     o               12      let2(1)             70
062100070314     o               13      let3(1)             70
062200060804     o               14      let4(1)             75
062300901122     O                       RSUT                25
062400041207     O                       I45GDS             101 '  /  /    '
062500900522      *
062600041216     O          E            TESTA       0  0
062700070314     O               11      LET1(2)             70
062800070314     o               12      let2(2)             70
062900070314     o               13      let3(2)             70
063000060804     o               14      let4(2)             75
063100901119      *
063200041216     O          E            TESTA       0  1
063300070314     O               11      LET1(2)             70
063400070314     o               12      let2(2)             70
063500070314     o               13      let3(2)             70
063600060804     o               14      let4(2)             75
063700980216      *
063800980219     O          E            TES1        1
063900980219     O                       DESCRE              25
064000980219      *
064100980219     O          E            TES1        0  0
064200980219     O                       DESCRE              25
064300041216     o          E            TES1        0  0
064400041216     o                                              '     ____________-
064500041216     o                                              __________________-
064600041216     o                                              __________________-
064700041216     o                                              __________________-
064800041216     o                                              __________________-
064900041216     o                                              __________________-
065000050104     o                                              ______________'
065100901119      *
065200980219     O          E            TES2        1
065300980219     O                       INTES1              80
065400900522      *
065500980216     O          E            TES2        0  0
065600980219     O                       INTES1              80
065700980216      *
065800980219     O          E            TES3        1
065900980219     O                       INTES2              80
066000980216      *
066100980216     O          E            TES3        0  0
066200980219     O                       INTES2              80
066300980330      *
066400980330     O          E            TES4        1
066500980330     O                       INTES3              80
066600980330      *
066700980330     O          E            TES4        0  0
066800980330     O                       INTES3              80
066900980217      *
067000980219     O          E            DET         1
067100051110     O                       ORGDES              27
067200051110     O                                           29 ':'
067300051110     O              N50      ORGIND              51
067400041207     o              n50      orgcpf           +   1
067500041207     o              n50      orgloc           +   1
067600041207     o              n50      orgpro           +   1
067700041207     o              n50                       +   2 'TEL.'
067800041207     o              n50      orgtel           +   1
067900041207     o              n50                       +   1 'FAX.'
068000041207     o              n50      orgfax           +   1
068100051110     O               50      ORGDEA              51
068200980217     O          E    50      DET         1
068300051110     O                       ORGIND              51
068400041207     O                       ORGCPF           +   1
068500041207     O                       ORGLOC           +   1
068600041207     O                       ORGPRO           +   1
068700041207     o                                        +   2 'TEL.'
068800041207     O                       ORGTEL           +   1
068900041207     o                                        +   1 'FAX.'
069000041207     O                       ORGFAX           +   1
069100901119      *
069200041216     O          E            PAG              08
069300901119     O                                              ' '
069400000407      *  ESTERO
069500000421     O          E            ESTE1       1
069600000407     O                       ES(1)               80
069700000421     O          E            ESTE1       0  0
069800000407     O                       ES(1)               80
069900000421     O          E            ESTE1       0  0
070000000407     O                       ES(2)               80
070100000421     O          E            ESTE1       1
070200000407     O                       ES(3)               80
070300000421     O          E            ESTE1       0  0
070400000407     O                       ES(3)               80
070500000421     O          E            ESTE1       2
070600000407     O                       ES(4)               80
070700000421     O          E            ESTE1       0  0
070800000421     O                       ES(4)               80
070900000421     O          E            ESTE1       1
071000000421     O                       ES(5)               80
071100000421     O          E            ESTE2       2
071200000421     O                       ES(6)               80
071300000421     O          E            ESTE2       0  0
071400000421     O                       ES(6)               80
071500000421     O          E            ESTE2       1
071600000421     O                       ES(7)               80
071700041207     O                                        +   0 'E KG. 31,5'
071800000421     O          E            ESTE2       2
071900000421     O                       ES(9)               80
072000070312     O                                        +   0 'ici commerciali delle fili'
072100070312     O                                              'ali sopra'
072200041207     O          E            ESTE2       1
072300070312     O                                           14 'elencate.'
072400050104     O          E            ESTEr1      3
072500060612     O               11      cos1               121
072600060612     O               12      cos12              121
072700060612     O               13      cos13              121
072800060612     O               14      cos14              121
072900050104     O          E            ESTEr2      1
073000060612     O               11      cos2               121
073100060612     O               12      cos22              121
073200060612     O               13      cos23              121
073300060612     O               14      cos24              121
073400000407      *
073500900517      *---------------------------------------------------------------*
073600940310** Comandi    567890123456789012345678901234567890123456789012345678901234567890
073700070312OVRPRTF FILE(LET017) CPI(16,7) COPIES(001)  USRDTA('Elenco Fil') FORMTYPE(LET1)
073800070314OVRPRTF FILE(LET017) CPI(16,7) COPIES(001)  USRDTA('Branches') FORMTYPE(LET1)
073900070314OVRPRTF FILE(LET017) CPI(16,7) COPIES(001)  USRDTA('List Fil.') FORMTYPE(LET1)
074000070314OVRPRTF FILE(LET017) CPI(16,7) COPIES(001)  USRDTA('Liste Fil.') FORMTYPE(LET1)
074100000407**  Estero    ES
074200000407     ESTERO
074300041216     ____________________
074400000407          EUROPA
074500070312          NETWORK D.P.D.       :  450 FILIALI
074600000421                                  PER SPEDIZIONI MONOCOLLO FINO A KG. 31,5
074700000421          NETWORK EUROEXPRESS  :  10 PARTNERS
074800041207                                  CON DISTRIBUZIONE CAPILLARE DI SPEDIZIONI OLTR
074900000421                                  OLTRE KG. 31,5
075000070312     Per maggiori informazioni su entrambi i network europei, contattare gli uff
075100070312     uffici commerciali delle Filiali sopra elencate.
075200050104**   LET1
075300070312ELENCO FILIALI ITALIA
075400070312_____________________
075500060612**   LET2
075600070314LIST OF BRANCHES-ITALY
075700070314______________________
075800060612**   LET3
075900070314LISTE DE FILIALES-ITALIE
076000070314________________________
076100060612**   LET4
076200070314LISTE MIT DEN FILIALEN IN ITALIEN
076300070314_________________________________
