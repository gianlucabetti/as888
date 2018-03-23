000100141219     /*END
000200950718     H*------------------------------------------------------------------------*
000300950718     H*             RICERCA E SCELTA CLIENTE - SELEZIONE MULTIPLA              *
000400950718     H* VIENE LANCIATO CON UN "?" E PERMETTE LA RICERCA E LA SELEZIONE DI UNO  *
000500950718     H* O PIU' CLIENTI (RICERCA PER CODICE O PER DESCRIZIONE) FRA QUELLI DEL   *
000600950718     H* SOTTOCONTO PASSATO O FRA TUTTI SE IL SOTTOCONTO E' UGUALE A ZERO.      *
000700950718     H*------------------------------------------------------------------------*
000800950718     H DECEDIT('0,') DATEDIT(*DMY.)
000900950718     F*------------------------------------------------------------------------*
001000950718     F* DATA BASE
001100950718     F*------------------------------------------------------------------------*
001200950718     FXCLID     CF   E             WORKSTN INFDS(DEVDS1)
001300950718     F                                     SFILE(XCLIDVS:SFLULT)
001400950718     F                                     SFILE(XCLIDVM:MSGULT)
001500950718     F*-----------
001600950718     F* KUT+KCC+KSC
001700950718     F*-----------
001800950718     FCNACO00F  IF   E           K DISK
001900960705     FTFNTC01L  IF   E           K DISK    USROPN
002000950718     F*-----------
002100961017     F* FTT+FLT+KCC+RAG
002200950718     F*-----------
002300961017     FCNACO04L  IF   E           K DISK
002400961017     F                                     RENAME(CNACO000:CNACO4)
002500100629
002600100629       // -?Anagrafica particolarità clienti?
002700100629     fCNCLP00F  if   e           k disk    usropn
002800100629
002900130909       // -?Anagrafica Commerciali?
003000130909     fAZCMM01L  if   e           k disk    usropn
003100100629
003200950718     D*------------------------------------------------------------------------*
003300950718     D* SCHIERE
003400950718     D*------------------------------------------------------------------------*
003500950718     D*-------------------
003600950718     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
003700950718     D*-------------------
003800950718     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003900950718     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
004000950718     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
004100950718     D FUN             S             40    DIM(31)
004200950718     D*-------------------
004300950718     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
004400950718     D*-------------------
004500950718     D FFU             S              6    DIM(31)
004600950718     D FDE             S             19    DIM(31)
004700950718     D FAT             S              1    DIM(31)
004800950718     D*-------------------
004900950718     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
005000950718     D*-------------------
005100950718     D OPD             S              2    DIM(24)
005200950718     D OPS             S              2    DIM(24)
005300950718     D ODE             S             19    DIM(24)
005400950718     D OAT             S              1    DIM(24)
005500950718     D*-------------------
005600950718     D* TABELLA DESCRIZIONI A VIDEO
005700950718     D*-------------------
005800950718     D FUV             S             19    DIM(8)
005900950718     D OPV             S             19    DIM(8)
006000950718     D*-------------------
006100950718     D* TABELLA TASTI FUNZIONALI PREMUTI
006200950718     D*-------------------
006300950718     D KEYS            S              1    DIM(40)
006400950718     D*-------------------
006500950718     D* CAPOCONTO / SOTTOCONTO DI RITORNO
006600950718     D*-------------------
006700950724     D KCC             S              4  0 DIM(30)
006800950724     D KSC             S              7  0 DIM(30)
006900950724     D*-------------------
007000950724     D* PER PASSAGGIO DATI
007100950724     D*-------------------
007200950724     D KCA             S              3    DIM(30)
007300950724     D KSA             S              4    DIM(30)
007400960705     D*-------------------
007500960705     D* COMANDI CLP
007600960705     D*-------------------
007700960705     D CMD1            S             35    DIM(1) CTDATA PERRCD(1)
007800961017     D*-------------------
007900961017     D* FILIALI DEI CLIENTI DA VISUALIZZARE
008000961017     D*-------------------
008100021113     D L1              S              3    DIM(250) INZ(*zeros)
008200100629       // -?Commerciali dei clienti da visualizzare?
008300141219     d c_Max_Cmm       c                   const(9000)
008400100629     d $Comm           s                   like(CLPage)
008500141219     d                                     dim(c_Max_Cmm)
008600100630     d                                     ascend
008700000821     D*------------------
008800000821     D* DS RICERCA CLIENTI UNIFICANTI
008900000821     D*------------------
009000000821     D BS10DS        E DS                  EXTNAME(TIBS10DS)
009100950718     D*------------------------------------------------------------------------*
009200950718     D* INPUT
009300950718     D*------------------------------------------------------------------------*
009400950718     D*---------------------
009500950718     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
009600950718     D*---------------------
009700950718     D DSOPZ         E DS
009800950718     D  OPZRIG                 1     40
009900950718     D*---------------------
010000950718     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
010100950718     D*---------------------
010200950718     D DSFUN         E DS
010300950718     D  FUNRIG                 1     40
010400950718     D*---------------------
010500950718     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
010600950718     D*---------------------
010700950718     D DSMSG         E DS
010800950718     D*---------------------
010900950718     D* DS PER GESTIONE TASTI FUNZIONALI
011000950718     D*---------------------
011100950718     D DSKEYS        E DS
011200950718     D  KEYRIG                 1     40
011300950718     D*---------------------
011400950718     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
011500950718     D*---------------------
011600950718     D                SDS
011700950718     D  USER                 254    263
011800950718     D*---------------------
011900950718     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
012000950718     D*---------------------
012100950718     D DEVDS1          DS
012200950718     D  KEY                  369    369
012300950718     D  CURSOR               370    371
012400950718     D  FRTSFL               378    379B 0
012500950718     D*---------------------
012600950718     D* DS DI PROCEDURA
012700950718     D*---------------------
012800950718     D DSXC          E DS                  EXTNAME(XCLIRDS)
012900971124     D*---------------------
013000971124     D* DS "XSRDA8" - CONTROLLA DATA (8)
013100971124     D*---------------------
013200971124     D WLBDA8          DS
013300971124     D  G08DAT                 1      8  0
013400971124     D  G08INV                 9     16  0
013500971124     D  G08ERR                17     17
013600971124     D  G08TGI                18     22  0
013700950724     D*---------------------
013800950724     D* DS PER PASSAGGIO CAMPI PACKED
013900950724     D*---------------------
014000950724     D                 DS
014100950724     D  DSKCN                  1      3P 0
014200950724     D  DSKCA                  1      3
014300950724     D  DSKSN                  4      7P 0
014400950724     D  DSKSA                  4      7
014500020905     D*-------------
014600020905     D* DS DI WRK
014700020905     D*-------------
014800020905     D TIBS34DS      E DS
014900020905     D DDATIUTE      E DS
015000020905     D AZUTEDS       E DS                  extname(AZUTE00F)
015100100713     d dute01        e ds
015200021113     D*------------------
015300021113     D* DS CARICAMENTO P.O. GESTITI
015400021113     D*-------------------
015500021113     D TRUL31DS      e ds                  INZ
015600020905     D*-------------
015700020905     D* DS DI ARCHITETTURA
015800020905     D*-------------
015900020905     D KPJBA         E DS
016000100629
016100100629       //--------------------------------------------------------------
016200100629       //?Definizione variabili.                                       ?
016300100629       //--------------------------------------------------------------
016400100629
016500100629       // -?Flags booleani?
016600100629     d $Xcomm          s               n
016700100629
016800100629       // -?Indici di schiera?
016900100629     d xx              s              5  0
017000100629
017100100629       //--------------------------------------------------------------
017200100629       //?Definizione key-list.                                        ?
017300100629       //--------------------------------------------------------------
017400100629
017500100629       // -?File CNCLP00F?
017600100629     d k03cnclp00    e ds                  extname(CNCLP00F : *key)
017700100629     d                                     prefix(k_)   inz
017800100629
017900950718     C*------------------------------------------------------------------------*
018000950718     C* MAIN LINE
018100950718     C*------------------------------------------------------------------------*
018200950718     C*
018300950718     C* CICLO FINO A CHE:
018400950718DO  1C     $FINE         DOUEQ     '1'
018500950718     C*
018600950718     C                   MOVEL     '0'           $ERR
018700950718     C*
018800950718     C* CONTROLLA GESTIONE VIDEO
018900950718IF  2C     $GES          IFEQ      'VS'
019000950718     C                   EXSR      GESVS
019100950718E   2C                   ENDIF
019200950718     C*
019300950718E   1C                   ENDDO
019400950718     C*
019500950718IF  1C     $F03          IFEQ      '1'
019600950718     C                   MOVEL     '1'           DXCF03
019700950718E   1C                   ENDIF
019800950724     C*
019900950724     C* COMPATTA I CODICI SELEZIONATI PER PASSARLI NELLA DS
020000950724DO  1C     1             DO        30            C
020100950724     C* CAPOCONTO
020200950724     C                   Z-ADD     KCC(C)        DSKCN
020300950724     C                   MOVE      DSKCA         KCA(C)
020400950724     C* SOTTOCONTO
020500950724     C                   Z-ADD     KSC(C)        DSKSN
020600950724     C                   MOVE      DSKSA         KSA(C)
020700950724E   1C                   ENDDO
020800950724     C                   MOVEA     KCA           DXCKCC
020900950724     C                   MOVEA     KSA           DXCKSC
021000950724     C*
021100971128     C* OPERAZIONI FINALI
021200971128     C                   EXSR      FINSR
021300971128     C*
021400950718     C                   SETON                                        LR
021500971128     C*------------------------------------------------------------------------*
021600971128     C* FINSR - OPERAZIONI FINALI
021700971128     C*------------------------------------------------------------------------*
021800971128     C     FINSR         BEGSR
021900971128     C*
022000971128     C*
022100971128     C                   ENDSR
022200950718     C*------------------------------------------------------------------------*
022300950718     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
022400950718     C*------------------------------------------------------------------------*
022500950718     C     CARVS         BEGSR
022600950718     C*
022700950718     C                   MOVEL     'VS'          $GES
022800950718     C*
022900950718     C                   EXSR      CAROPZ
023000950718     C                   EXSR      OUTOPZ
023100950718     C                   MOVEA     FUN2          FUN
023200950718     C                   EXSR      CARFUN
023300950718     C                   EXSR      OUTFUN
023400950718     C*
023500950718     C                   WRITE     XCLIDVT
023600950718     C                   WRITE     XCLIDVO
023700950718     C                   WRITE     XCLIDVF
023800950718     C*
023900950718     C                   EXSR      POSSFL
024000950718     C*
024100950718     C                   ENDSR
024200950718     C*------------------------------------------------------------------------*
024300950718     C* GESVS  - GESTIONE SUBFILE
024400950718     C*------------------------------------------------------------------------*
024500950718     C     GESVS         BEGSR
024600950718     C*
024700950718IF  1C     $TESVD        IFEQ      '1'
024800950718     C                   WRITE     XCLIDVT
024900950718     C                   WRITE     XCLIDVO
025000950718     C                   WRITE     XCLIDVF
025100950718E   1C                   ENDIF
025200950718     C*
025300950718     C                   EXSR      OUTSFL
025400950718     C*
025500950718     C                   EXSR      CONFUN
025600950718     C*
025700950718IF  1C     $ERR          IFEQ      '0'
025800950718SL  2C                   SELECT
025900950718     C                   OTHER
026000950718     C                   EXSR      FUNSFL
026100950718E   2C                   ENDSL
026200950718E   1C                   ENDIF
026300950718     C*
026400950718     C                   ENDSR
026500950718     C*------------------------------------------------------------------------*
026600950718     C*  GESOPZ - GESTIONE OPZIONI
026700950718     C*------------------------------------------------------------------------*
026800950718     C     GESOPZ        BEGSR
026900950718     C*
027000950718SL  1C                   SELECT
027100950718     C     VSCOPZ        WHENEQ    ' 1'
027200950718     C                   EXSR      O01GES
027300950718     C                   OTHER
027400950718     C                   MOVEL     MSG(4)        $MSG
027500950718     C                   EXSR      CARMSG
027600950718E   1C                   ENDSL
027700950718     C*
027800950718     C                   ENDSR
027900950718     C*------------------------------------------------------------------------*
028000950718     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
028100950718     C*------------------------------------------------------------------------*
028200950718     C     O01GES        BEGSR
028300950718     C*
028400950718IF  1C     $ERR          IFEQ      '0'
028500950721     C     C             ANDLT     30
028600950718     C                   ADD       1             C
028700950718     C                   MOVEL     VSCKCC        KCC(C)
028800950718     C                   MOVEL     VSCKSC        KSC(C)
028900950718     C                   MOVEL     '1'           $FINE
029000950718E   1C                   ENDIF
029100950718     C*
029200950718     C                   ENDSR
029300950718     C*------------------------------------------------------------------------*
029400950718     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
029500950718     C*------------------------------------------------------------------------*
029600950718     C     NEWPOS        BEGSR
029700950718     C*
029800950718     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
029900950718     C*        1 = RICHIESTO POSIZIONAMENTO
030000950718     C                   Z-ADD     0             SWPOSI            7 0
030100950718     C*
030200950718     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
030300950718     C*            NON OCCORRE RICARICARE IL SUBFILE
030400950718     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
030500950718     C                   Z-ADD     0             SWMODI            1 0
030600950718     C*
030700950718     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
030800950718IF  1C     VCCKSC        IFNE      DEPKSC
030900950718     C     VCCRAG        ORNE      DEPRAG
031000950718     C*
031100950718     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLA I PARAMETRI
031200950718     C                   EXSR      CHKPAR
031300950718     C*
031400950718     C* SE I PARAMETRI SONO OK, LI MEMORIZZA E IMPOSTA FLAG PARAMETRI MODIFICATI
031500950718IF  2C     $ERR          IFEQ      '0'
031600950718     C                   Z-ADD     1             SWPOSI
031700950718     C                   Z-ADD     VCCKSC        DEPKSC
031800950718     C                   MOVEL     VCCRAG        DEPRAG
031900950718E   2C                   ENDIF
032000950718E   1C                   ENDIF
032100950718     C*
032200950718     C                   ENDSR
032300950718     C*------------------------------------------------------------------------*
032400950718     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
032500950718     C*------------------------------------------------------------------------*
032600950718     C     CHKPAR        BEGSR
032700950718     C*
032800950718     C* REIMPOSTA INDICATORI EVIDENZAZIONE ERRORI SUI PARAMETRI
032900950718     C                   SETOFF                                       8081
033000950718     C*
033100950718     C* REIMPOSTA INDICATORI EVIDENZAZIONE ORDINAMENTO
033200950718     C                   SETOFF                                       5051
033300950718     C*--------------------
033400950718     C* ORDINAMENTO
033500950718     C*--------------------
033600950718     C* SE TUTTI I PARAMETRI SONO VUOTI, UTILIZZO CHIAVE "COD"
033700950718IF  1C     VCCKSC        IFEQ      *ZEROS
033800950718     C     VCCRAG        ANDEQ     *BLANKS
033900100629     C*//                MOVEL     'COD'         WTIPRI            3
034000100629     C                   MOVEL     'RAG'         WTIPRI            3
034100950718     C                   SETON                                        50
034200950718X   1C                   ELSE
034300950718     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
034400950718IF  2C     VCCKSC        IFNE      *ZEROS
034500950718     C     VCCRAG        ANDNE     *BLANKS
034600950718     C                   MOVEL     MSG(7)        $MSG
034700950718     C                   EXSR      CARMSG
034800950718     C                   MOVEL     '1'           $ERR
034900950718     C                   SETON                                        8081
035000950718X   2C                   ELSE
035100950718     C***
035200950718     C* INIZIO ELENCO DA  C O D I C E
035300950718     C***
035400950718IF  3C     VCCKSC        IFNE      *ZEROS
035500950718     C                   MOVEL     'COD'         WTIPRI
035600950718     C                   SETON                                        50
035700950718E   3C                   ENDIF
035800950718     C***
035900950718     C* INIZIO ELENCO DA  R A G I O N E   S O C I A L E
036000950718     C***
036100950718IF  3C     VCCRAG        IFNE      *BLANKS
036200950718     C                   MOVEL     'RAG'         WTIPRI
036300950718     C                   SETON                                        51
036400950718E   3C                   ENDIF
036500950718     C*
036600950718E   2C                   ENDIF
036700950718E   1C                   ENDIF
036800950718     C*
036900950718     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
037000950718     C* CURSORE SUL CAMPO DEL CONTROL
037100950718IF  1C     $ERR          IFEQ      '1'
037200950718     C                   MOVEL     '0'           *IN43
037300950718X   1C                   ELSE
037400950718     C                   MOVEL     '1'           *IN43
037500950718E   1C                   ENDIF
037600950718     C*
037700950718     C                   ENDSR
037800950718     C*------------------------------------------------------------------------*
037900950718     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
038000950718     C*------------------------------------------------------------------------*
038100950718     C     SETFIL        BEGSR
038200950718     C*
038300950718     C                   MOVEL     '1'           *IN43
038400950718     C*--------------------
038500950718     C* POSIZIONAMENTO PER  C O D I C E
038600950718     C*--------------------
038700950718IF  1C     WTIPRI        IFEQ      'COD'
038800020905     C                   Z-ADD     1             KACKUT
038900950718     C                   Z-ADD     DXCCAP        KACKCC
039000961017IF  2C     VCCKSC        IFGT      *ZEROS
039100950718     C                   Z-ADD     VCCKSC        KACKSC
039200961017X   2C                   ELSE
039300961017     C                   Z-ADD     *ZEROS        KACKSC
039400100629     c                   if        NOT $Xcomm
039500100629     C                   MOVE(P)   L1(1)         KACKSC
039600100629     c                   endif
039700961017E   2C                   ENDIF
039800950718     C     KEYACO        SETLL     CNACO00F                           99
039900961017E   1C                   ENDIF
040000950718     C*--------------------
040100950718     C* POSIZIONAMENTO PER  R A G I O N E   S O C I A L E
040200950718     C*--------------------
040300961017IF  1C     WTIPRI        IFEQ      'RAG'
040400020905     C                   Z-ADD     1             KACKUT
040500961017     C                   Z-ADD     DXCCAP        KACKCC
040600961017     C                   MOVEL     VCCRAG        KACRAG
040700961017     C     KE2ACO        SETLL     CNACO04L                           99
040800961017E   1C                   ENDIF
040900950718     C*
041000950718     C  N99              EXSR      LETFIL
041100950718     C*
041200950718     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
041300950718IF  1C     *IN99         IFEQ      *ON
041400950718     C                   Z-ADD     1             SFLEOF
041500950718E   1C                   ENDIF
041600950718     C*
041700950718     C                   ENDSR
041800950718     C*------------------------------------------------------------------------*
041900950718     C* LETFIL - LETTURA RECORD SUCESSIVO
042000950718     C*------------------------------------------------------------------------*
042100950718     C     LETFIL        BEGSR
042200950718     C*
042300950718     C* LEGGO FINO A:
042400950718     C*     - FINE FILE
042500950718     C*     - TROVATO RECORD VALIDO
042600950718     C                   MOVEL     'N'           WRECOK            1
042700950718DO  1C     *IN99         DOWEQ     *OFF
042800950718     C     WRECOK        ANDEQ     'N'
042900950718     C*--------------------
043000950718     C* LETTURA PER  C O D I C E
043100950718     C*--------------------
043200950718IF  2C     WTIPRI        IFEQ      'COD'
043300950718     C                   READ      CNACO00F                               99
043400961017E   2C                   ENDIF
043500950718     C*--------------------
043600950718     C* LETTURA PER  R A G I O N E   S O C I A L E
043700950718     C*--------------------
043800961017IF  2C     WTIPRI        IFEQ      'RAG'
043900961017     C                   READ      CNACO04L                               99
044000961017E   2C                   ENDIF
044100950718     C*
044200950718IF  2C     *IN99         IFEQ      *ON
044300950718     C                   Z-ADD     1             SFLEOF
044400950718X   2C                   ELSE
044500950718     C                   EXSR      CHKREC
044600950718E   2C                   ENDIF
044700950718     C*
044800950718E   1C                   ENDDO
044900950718     C*
045000950718     C                   ENDSR
045100950718     C*------------------------------------------------------------------------*
045200950718     C* CHKREC - CONTROLLA VALIDITA' RECORD
045300950718     C*------------------------------------------------------------------------*
045400950718     C     CHKREC        BEGSR
045500950718     C*
045600950718     C                   MOVEL     'S'           WRECOK
045700950718     C*
045800950718     C* CLIENTE ANNULLATO, ESCLUDE
045900950718IF  2C     ACOFLG        IFNE      *BLANKS
046000950718     C                   MOVEL     'N'           WRECOK
046100950718E   2C                   ENDIF
046200950718     C*
046300950718     C* SOTTOCONTO DIVERSO DA QUELLO RICHIESTO, ESCLUDE
046400950718IF  1C     DXCCAP        IFNE      *ZEROS
046500950718     C     DXCCAP        ANDNE     ACOKCC
046600950718     C                   MOVEL     'N'           WRECOK
046700950718E   1C                   ENDIF
046800950718     C*
046900950718     C* CAPOCONTO "FIL+0000", ESCLUDE
047000950718     C                   MOVE      ACOKSC        CAPOC             4 0
047100950718IF  1C     CAPOC         IFEQ      *ZEROS
047200960227     C                   MOVEL     'N'           WRECOK
047300950718E   1C                   ENDIF
047400961017     C*
047500961017     C* CLIENTI NON IN GESTIONE, ESCLUDE
047600020905IF  1C     DUTLPO        IFNE      'S'
047700021113     C                   MOVEL     ACOKSC        WN3               3
047800961017     C                   SETOFF                                           28
047900100629     C     WN3           LOOKUP    L1                                     28
048000100629      /free
048100100629           // -?Se cliente NON in gestione:?
048200100629           //  ?verifica se lo risulta il suo comm.le (o il relativo unificante)
048300100629           if  Not *in28   and   $Xcomm;
048400100630             clear  k03cnclp00;
048500100630             k_CLPkut = ACOkut;
048600100630             k_CLPkcc = ACOkcc;
048700100630             k_CLPksc = ACOksc;
048800100629             chain  %kds(k03cnclp00)  CNCLP000;
048900100629             *in28 = (%found(CNCLP00F)   and
049000100630                      CLPage > *zero     and
049100100629                      %lookup(CLPage : $Comm) > *zero);
049200100629           endif;
049300100629      /end-free
049400961017IF  2C     *IN28         IFEQ      *OFF
049500961017     C                   MOVEL     'N'           WRECOK
049600961017E   2C                   ENDIF
049700961017E   1C                   ENDIF
049800950718     C*
049900950718     C                   ENDSR
050000950718     C*------------------------------------------------------------------------*
050100950718     C* IRESFL - IMPOSTA I CAMPI DEL SUBFILE DAL FILE FISICO
050200950718     C*------------------------------------------------------------------------*
050300950718     C     IRESFL        BEGSR
050400950718     C*
050500960705     C                   Z-ADD     ACOKCC        VSCKCC
050600950718     C                   Z-ADD     ACOKSC        VSCKSC
050700950718     C                   MOVEL     ACORAG        VSCRAG
050800000821     C*
050900000821     C* REPERISCE IL CODICE PADRE DEI CLIENTI
051000000821     C                   CLEAR                   BS10DS
051100000821     C                   Z-ADD     DATCOR        D10DRF
051200050519     C                   MOVEL     INTLE         D10TLE
051300000821     C                   MOVEL     'P'           D10PAF
051400000821     C                   Z-ADD     ACOKSC        D10COD
051500000821     C                   CALL      'TIBS10R'
051600000821     C                   PARM                    BS10DS
051700000821IF  1C     D10ERR        IFEQ      *BLANKS
051800000821     C     D10COP        ANDGT     *ZEROS
051900000821     C                   Z-ADD     D10COP        VSCCOP
052000000821X   1C                   ELSE
052100000821     C                   Z-ADD     ACOKSC        VSCCOP
052200000821E   1C                   ENDIF
052300000905     C*
052400000905     C* IMPOSTA SE IL CLIENTE E' UNIFICANTE O NO
052500000905     C                   MOVEL     *BLANKS       VSCUNI
052600000905     c                   Z-ADD     VSCCOP        N7                7 0
052700000905IF  1C     N7            IFEQ      VSCKSC
052800000905     C                   MOVEL     'U'           VSCUNI
052900000905E   1C                   ENDIF
053000100427      * imposta se il cliente è bloccato o no
053100100427     c                   eval      VSCabl = ACOabl
053200130322      * Visualizzo sempre blocco "8" anche se il blocco sul file è "7"
053300130322     c                   IF        VSCabl = '7'
053400130322     c                   eval      VSCabl = '8'
053500130322     c                   ENDIF
053600000821     C*
053700000821     C* LEGGE LE NOTE DEI CLIENTI PER LA DESCRIZIONE
053800960611     C                   CLEAR                   VSCDS1
053900960611     C                   CLEAR                   VSCDS2
054000960705IF  1C     $ESI          IFEQ      '1'
054100960705     C                   CLEAR                   KNCNK1
054200960705     C                   MOVEL     'C'           KNCAPL
054300960705     C                   MOVEL     'DC'          KNCTNT
054400960705     C                   MOVEL     ACOKCC        KNCNK1
054500960705     C                   MOVE      ACOKSC        KNCNK1
054600010131
054700010131     C     KEYNTC        SETLL     TFNTC01L
054800010131     C                   do        *hival
054900010131     C     KEYNTC        READE     TFNTC01L                               98
055000010131     C   98              leave
055100010131IF  2C     ntcflt        ifne      'A'
055200010131     C                   MOVEL     NTCRNT        W058A            58
055300010131     C                   MOVEL     W058A         VSCDS1
055400010131     C                   MOVE      W058A         VSCDS2
055500010131     C                   leave
055600010131E   2C                   ENDIF
055700010131     C                   enddo
055800010131
055900960705E   1C                   ENDIF
056000950718     C*
056100950718     C                   ENDSR
056200961017     C*------------------------------------------------------------------------*
056300961017     C* CARTAB - CARICA LE TABELLE
056400961017     C*------------------------------------------------------------------------*
056500961017     C     CARTAB        BEGSR
056600020905     C*
056700020905     C* REPERISCE I DATI UTENTE
056800020905     C                   EXSR      REPDATIUTE
056900020905     C*
057000020905     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
057100020905     C                   CLEAR                   L1
057200021113     C                   CLEAR                   WAbi              2
057300021113     C*
057400021113     C* Se ci sono degli errori sull'abilitazione utente, esce
057500021113     C                   SELECT
057600021113     C*
057700021113     C* Se ho ricevuto in input una autorizzazione speciale => la gestisco
057800100713     C                   WHEN      DXCAUT > *blank and DXCAUT <> '*C'
057900021113     C                   EVAL      WAbi = DXCAUT
058000100713     C* Se ho ricevuto in input *C (nuovo CRM) passo autorizzazione gestione tar
058100100713     C                   WHEN      DXCAUT > *blank and DXCAUT =  '*C'
058200100713     C                   EVAL      WAbi = §utegtc
058300021113     C*
058400021113     C* ...se invece nn ricevo particolari autorizzazioni in input da gestire =>
058500021113     C* dell'utente (1° o 2° livello)
058600021113     C                   OTHER
058700021118     C                   IF        UTEAUT > *blanks
058800021118     C                   EVAL      WAbi = UTEAUT
058900021118     C                   ELSE
059000021118     C*
059100021113     C                   IF        DUTLPO = 'S'
059200021113     C                   EVAL      WAbi = 'AZ'
059300021113     C                   ENDIF
059400021113     C                   IF        DUTLPO = '1'
059500021113     C                   EVAL      WAbi = 'TP'
059600021113     C                   ENDIF
059700021113     C                   IF        DUTLPO = '2'
059800021113     C                   EVAL      WAbi = 'PO'
059900021113     C                   ENDIF
060000021118     C                   ENDIF
060100021113     C                   ENDSL
060200021113     C*
060300021113     C* Reperisce i P.O. gestibili dall'utente
060400021113     C                   CLEAR                   TRUL31DS
060500021113     C                   EVAL      I31abi = WAbi
060600021113     C                   EVAL      I31cpo = DUTpou
060700021113     C                   CALL      'TRUL31R'
060800021113     C                   PARM                    KPJBA
060900021113     C                   PARM                    TRUL31DS
061000021113     C                   IF        O31pog > *zeros
061100021113     C                   MOVEA     O31pog        L1
061200021113     C                   ELSE
061300021113     C                   EVAL      $FINE = '1'
061400021113     C                   ENDIF
061500100629      /free
061600100629
061700130909         // -?Preparazione dell'elenco dei commerciali:?
061800100629         //  ?1) di filiale abilitata?
061900100629         //  ?2) con comm.le unificante di filiale abilitata?
062000100629         //  ?(SE ricevuta apposita richiesta dal chiamante)?
062100100630         clear  $Comm;
062200100629         clear  xx;
062300100629
062400100629         IF  $Xcomm;
062500100629
062600130909           setll  ( *loval )  AZCMM000;
062700130909           read  AZCMM000;
062800130909           DoW  NOT %eof(AZCMM01L);
062900130909             if  %lookup( %subst( %editc( CMMcod : 'X' ) : 1 : 3 )
063000130909                          : L1 ) > *zero   or
063100130909                 %lookup( %subst( %editc( CMMuni : 'X' ) : 1 : 3 )
063200130909                          : L1 ) > *zero;
063300100629               xx += 1;
063400130909               $Comm(xx) = CMMcod;
063500100629             endif;
063600130909             read  AZCMM000;
063700100629           EndDo;
063800100629
063900100629           // -?Ordinamento crescente?
064000100630           DoW  xx < %elem($Comm);
064100100630             xx += 1;
064200100630             $Comm(xx) = *hival;
064300100630           EndDo;
064400100630           sorta  $Comm;
064500100629
064600100629         ENDIF;
064700100629
064800100629      /end-free
064900961017     C*
065000961017     C                   ENDSR
065100020905     C*-------------------------------------------------------------------------
065200020905     C* REPERISCE I DATI UTENTE
065300020905     C*-------------------------------------------------------------------------
065400020905     C     REPDATIUTE    BEGSR
065500020905     C*
065600020905     C* INIZIALIZZA VARIABILI DI WRK
065700020905     C                   CLEAR                   TIBS34DS
065800020905     C                   CLEAR                   AZUTEDS
065900020905     C                   CLEAR                   DDATIUTE
066000100713     C                   CLEAR                   Dute01
066100020905     C*
066200020905     C     *DTAARA       DEFINE    §azute        azuteds
066300020905     C     *DTAARA       DEFINE    §datiute      ddatiute
066400020905     C                   IN(E)     *DTAARA
066500020905if  1C                   IF        %Error
066600020905     c                   EVAL      I34Tla = 'L'
066700020905     C                   CALL      'TIBS34R'
066800020905     C                   PARM                    Tibs34Ds
066900020905     C                   IN        *DTAARA
067000020905e   1C                   ENDIF
067100100713     c*
067200100713     c                   Movel     UteFaf        Dute01
067300020905     C*
067400020905     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
067500020923     C                   MOVEL(P)  RSUT          DSFIRS
067600020905     C*
067700020905     C                   ENDSR
067800950718     C*------------------------------------------------------------------------*
067900961017     C* *INZSR - OPERAZIONI INIZIALI
068000950718     C*------------------------------------------------------------------------*
068100950718     C     *INZSR        BEGSR
068200950718     C*--------------------
068300950718     C* RICEVIMENTO PARAMETRI
068400950718     C*--------------------
068500950718     C     *ENTRY        PLIST
068600950721     C                   PARM                    DSXC
068700050519     C                   PARM                    PARMINTLE         2
068800120321     c                   parm                    ragsoc           48
068900050519     C*--------------------
069000050519     C* EFFETTUO SUBITO CONSIDERAZIONI SU NUOVI PARAMETRI EVENTUALMENTE NN PASSA
069100050519     C*--------------------
069200050519     C                   MOVEL     'ST'          INTLE             2
069300050519     C                   IF        %PARMS > 1
069400050519     C                   EVAL      INTLE = PARMINTLE
069500050519     C                   ENDIF
069600050519     C*
069700050519     C                   IF        INTLE = *blanks
069800050519     C                   EVAL      INTLE = 'ST'
069900050519     C                   ENDIF
070000120321      * se richiamato da SB48 (opzione subfile) non abilito la scelta
070100120321     c                   IF        dxcop0 = 'OPZ'
070200120321     c                   eval      *in46 = *on
070300120321     c                   eval      vccrag = ragsoc
070400120321     c                   ENDIF
070500050519     C*--------------------
070600050519     C* IMPOSTO SUBITO ALCUNDE DESCRIZIONI A VIDEO
070700050519     C*--------------------
070800050519     C                   EVAL      VDDTLE = '(Unificazione per tipo legame ' +
070900050519     C                                      INTLE + ')'
071000950718     C*--------------------
071100950718     C* CAMPI RIFERITI AL DATA BASE
071200950718     C*--------------------
071300950718     C     *LIKE         DEFINE    ACOKUT        KACKUT
071400950718     C     *LIKE         DEFINE    ACOKCC        KACKCC
071500950718     C     *LIKE         DEFINE    ACOKSC        KACKSC
071600961017     C     *LIKE         DEFINE    ACORAG        KACRAG
071700960705     C     *LIKE         DEFINE    NTCAPL        KNCAPL
071800960705     C     *LIKE         DEFINE    NTCNK1        KNCNK1
071900960705     C     *LIKE         DEFINE    NTCNK2        KNCNK2
072000960705     C     *LIKE         DEFINE    NTCTNT        KNCTNT
072100950718     C     *LIKE         DEFINE    ACOKSC        DEPKSC
072200950718     C     *LIKE         DEFINE    ACORAG        DEPRAG
072300020905     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU
072400950718     C*--------------------
072500950718     C* CHIAVI DI LETTURA
072600950718     C*--------------------
072700950718     C* LETTURA CNACO00F - COMPLETA
072800950718     C     KEYACO        KLIST
072900950718     C                   KFLD                    KACKUT
073000950718     C                   KFLD                    KACKCC
073100950718     C                   KFLD                    KACKSC
073200961017     C* LETTURA CNACO04L - COMPLETA
073300950718     C     KE2ACO        KLIST
073400961017     C                   KFLD                    KACKUT
073500961017     C                   KFLD                    KACKCC
073600961017     C                   KFLD                    KACRAG
073700960611     C* LETTURA TFNTC01L - COMPLETA
073800960705     C     KEYNTC        KLIST
073900960705     C                   KFLD                    KNCAPL
074000960705     C                   KFLD                    KNCNK1
074100960705     C                   KFLD                    KNCNK2
074200961017     C                   KFLD                    KNCTNT
074300950718     C*--------------------
074400950718     C* CAMPI DI CONTROLLO
074500950718     C*--------------------
074600950718     C                   Z-ADD     *ZEROS        C                 4 0
074700950718     C                   MOVEL     *ZEROS        KCC
074800950718     C                   MOVEL     *ZEROS        KSC
074900100629      /free
075000130909         // -?Verifica se richiamato in ricerca per filiale comm.le:
075100100629         //  ?memorizza il fatto di essere stato richiamato con?
075200100629         //  ?DXCAUT = "*C" ed imposta il valore standard per il?
075300100629         //  ?reperimento delle filiali gestibili dall'utente?
075400100629         //  ?(vedi TRUL31DS.I31ABI).?
075500100629         $Xcomm = (DXCaut = '*C');
075600100629         if  $Xcomm;
075700100713         //  clear  DXCaut;
075800130909           open  AZCMM01L;
075900100629           open  CNCLP00F;
076000100629         endif;
076100100629      /end-free
076200950718     C*
076300950718     C* ESEGUE OPERAZIONI INIZIALI STANDARD
076400950718     C                   MOVEL     'I'           $TGES             1
076500950718     C                   EXSR      INZSTD
076600950718     C*
076700950718     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
076800950718     C                   EXSR      INZSFL
076900971124     C*
077000971124     C* IMPOSTA LA DATA CORRENTE
077100971124     C                   TIME                    N14              14 0
077200971124     C                   MOVE      N14           N8                8 0
077300971124     C                   Z-ADD     N8            G08DAT
077400971124     C                   Z-ADD     *ZEROS        G08INV
077500971124     C                   MOVEL     '0'           G08ERR
077600971124     C                   CALL      'XSRDA8'
077700971124     C                   PARM                    WLBDA8
077800971124     C                   Z-ADD     G08INV        DATCOR            8 0
077900961017     C*
078000961017     C* CARICA LE TABELLE
078100961017     C                   EXSR      CARTAB
078200960705     C*
078300960705     C* CONTROLLA ESISTENZA DEL FILE NOTE DEL P.D.C.
078400960705     C                   Z-ADD     35            LUNG             15 5
078500960705     C                   MOVEL     *BLANKS       COMMAN           80
078600960705     C                   MOVEA     CMD1(1)       COMMAN
078700960705     C                   CALL      'QCMDEXC'                            98
078800960705     C                   PARM                    COMMAN
078900960705     C                   PARM                    LUNG
079000960705IF  1C     *IN98         IFEQ      *OFF
079100960705     C                   OPEN      TFNTC01L
079200960705     C                   MOVEL     '1'           $ESI              1
079300960705E   1C                   ENDIF
079400950718     C*
079500960705     C* IMPOSTAZIONI INIZIALI VIDEO
079600950718     C                   Z-ADD     DXCCAP        VCCKCC
079700950718     C*
079800130909     C* EMETTE VIDEATA SUBFILE
079900950718     C                   EXSR      CHKPAR
080000950718     C                   EXSR      CARVS
080100950718     C*
080200950718     C                   ENDSR
080300950718     C*------------------------------------------------------------------------*
080400950718     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080500950718     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080600950718     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
080700950718     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
080800950718     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
080900950718     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
081000950718     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
081100950718     C*------------------------------------------------------------------------*
081200950718     C* INZSTD - ROUTINE INIZIALE STANDARD
081300950718     C*------------------------------------------------------------------------*
081400950718     C     INZSTD        BEGSR
081500950718     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
081600950718     C                   CALL      'XKEYS'
081700950718     C                   PARM                    DSKEYS
081800950718     C                   MOVEA     KEYRIG        KEYS
081900950718     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
082000950718     C     XMSGPA        PLIST
082100950718     C                   PARM                    DSMSMS
082200950718     C                   PARM                    DSMSPG
082300950718     C                   PARM                    DSMSKE
082400950718     C                   PARM                    DSMSER
082500950718     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
082600950718     C                   MOVEL     'XCLIR  '     DSMSPG
082700950718     C                   MOVEL     'XCLIR  '     VMCQUE
082800950718     C                   MOVEL     *BLANKS       VMCKEY
082900950718     C                   MOVEL     *BLANKS       DSMSMS
083000950718     C* INIZIALIZZO FLAG CONTROLLO PGM
083100950718     C                   MOVEL     '0'           $ERR              1
083200950718     C                   MOVEL     '0'           $F03              1
083300950718     C                   MOVEL     *BLANKS       $GES              2
083400950718     C                   MOVEL     *BLANKS       $MSG             80
083500950718     C                   MOVEL     '0'           $FINE             1
083600950718     C                   MOVEL     '0'           $TESVD            1
083700020905     C*
083800950718     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
083900950718     C                   BITOFF    '01234567'    POSMI1            1
084000950718     C                   BITOFF    '01234567'    POSMI2            1
084100950718     C                   BITOFF    '01234567'    POSMA1            1
084200950718     C                   BITOFF    '01234567'    POSMA2            1
084300950718     C                   BITON     '34'          POSMI1            1
084400950718     C                   BITON     '7'           POSMI2            1
084500950718     C                   BITON     '34'          POSMA1            1
084600950718     C                   BITON     '0'           POSMA2            1
084700950718     C                   MOVE      POSMI2        POSMIN            2
084800950718     C                   MOVEL     POSMI1        POSMIN
084900950718     C                   MOVE      POSMA2        POSMAX            2
085000950718     C                   MOVEL     POSMA1        POSMAX
085100950718     C                   ENDSR
085200950718     C*------------------------------------------------------------------------*
085300950718     C* FUNSTD - LANCIA SUBROUTINE STANDARD
085400950718     C*------------------------------------------------------------------------*
085500950718     C     FUNSTD        BEGSR
085600950718     C                   SELECT
085700950718     C     KEY           WHENEQ    F3
085800950718     C                   EXSR      F03STD
085900950718     C     KEY           WHENEQ    F12
086000950718     C                   EXSR      F12STD
086100950718     C     KEY           WHENEQ    F24
086200950718     C                   EXSR      F24STD
086300950718     C                   OTHER
086400950718     C                   MOVEL     MSG(2)        $MSG
086500950718     C                   EXSR      CARMSG
086600950718     C                   ENDSL
086700950718     C                   ENDSR
086800950718     C*------------------------------------------------------------------------*
086900950718     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
087000950718     C*------------------------------------------------------------------------*
087100950718     C     F03STD        BEGSR
087200950718     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
087300950718     C                   MOVEL     '1'           $F03
087400950718     C* IMPOSTO FLAG DI FINE PROGRAMMA
087500950718     C                   MOVEL     '1'           $FINE
087600950718     C                   ENDSR
087700950718     C*------------------------------------------------------------------------*
087800950718     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
087900950718     C*------------------------------------------------------------------------*
088000950718     C     F12STD        BEGSR
088100950718     C* IMPOSTO FLAG DI FIN PGM
088200950718     C                   MOVEL     '1'           $FINE
088300950718     C                   ENDSR
088400950718     C*------------------------------------------------------------------------*
088500950718     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
088600950718     C*------------------------------------------------------------------------*
088700950718     C     F24STD        BEGSR
088800950718     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
088900950718     C                   EXSR      OUTFUN
089000950718     C                   ENDSR
089100950718     C*------------------------------------------------------------------------*
089200950718     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
089300950718     C*------------------------------------------------------------------------*
089400950718     C     INZSFL        BEGSR
089500950718     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
089600950718     C     *LIKE         DEFINE    VCCNBR        SFLPAG
089700950718     C     *LIKE         DEFINE    VCCNBR        SFLEOF
089800950718     C     *LIKE         DEFINE    VCCNBR        SFLERR
089900950718     C     *LIKE         DEFINE    VCCNBR        SFLULT
090000950718     C     *LIKE         DEFINE    VCCNBR        WFLULT
090100950718     C     *LIKE         DEFINE    VCCNBR        SFLRIG
090200950718     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
090300950718     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
090400950718     C                   Z-ADD     12            SFLPAG
090500950718     C                   ENDSR
090600950718     C*------------------------------------------------------------------------*
090700950718     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
090800950718     C*------------------------------------------------------------------------*
090900950718     C     FUNSFL        BEGSR
091000950718     C                   SELECT
091100950718     C     KEY           WHENEQ    ENTER
091200950718     C                   EXSR      ENTSFL
091300950718     C     KEY           WHENEQ    F3
091400950718     C                   EXSR      F03STD
091500950718     C     KEY           WHENEQ    F5
091600950718     C                   EXSR      F05STD
091700950718     C     KEY           WHENEQ    F12
091800950718     C                   EXSR      F12STD
091900950718     C     KEY           WHENEQ    F17
092000950718     C                   EXSR      F17STD
092100950718     C     KEY           WHENEQ    F18
092200950718     C                   EXSR      F18STD
092300950718     C     KEY           WHENEQ    F23
092400950718     C                   EXSR      F23STD
092500950718     C     KEY           WHENEQ    F24
092600950718     C                   EXSR      F24STD
092700950718     C     KEY           WHENEQ    ROLLUP
092800950718     C                   EXSR      RUPSTD
092900950718     C     KEY           WHENEQ    ROLLDN
093000950718     C                   EXSR      RDNSTD
093100950718     C                   OTHER
093200950718     C                   MOVEL     MSG(2)        $MSG
093300950718     C                   EXSR      CARMSG
093400950718     C                   ENDSL
093500950718     C                   ENDSR
093600950718     C*------------------------------------------------------------------------*
093700950718     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
093800950718     C*------------------------------------------------------------------------*
093900950718     C     F05STD        BEGSR
094000950718     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
094100950718     C                   EXSR      POSSFL
094200950718     C                   ENDSR
094300950718     C*------------------------------------------------------------------------*
094400950718     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
094500950718     C*------------------------------------------------------------------------*
094600950718     C     F17STD        BEGSR
094700950718     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
094800950718     C                   Z-ADD     1             VCCNBR
094900950718     C                   ENDSR
095000950718     C*------------------------------------------------------------------------*
095100950718     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
095200950718     C*------------------------------------------------------------------------*
095300950718     C     F18STD        BEGSR
095400950718     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
095500950718     C                   Z-ADD     SFLULT        VCCNBR
095600950718     C                   ENDSR
095700950718     C*------------------------------------------------------------------------*
095800950718     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
095900950718     C*------------------------------------------------------------------------*
096000950718     C     F23STD        BEGSR
096100950718     C* GESTIONE VISUALIZZAZIONE OPZIONI
096200950718     C                   EXSR      OUTOPZ
096300950718     C                   ENDSR
096400950718     C*------------------------------------------------------------------------*
096500950718     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
096600950718     C*------------------------------------------------------------------------*
096700950718     C     RUPSTD        BEGSR
096800950718     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
096900950718     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
097000950718     C     CURSOR        IFGE      POSMIN
097100950718     C     CURSOR        ANDLE     POSMAX
097200950718     C                   ELSE
097300950718     C                   EXSR      CARSFL
097400950718     C                   ENDIF
097500950718     C                   ENDSR
097600950718     C*------------------------------------------------------------------------*
097700950718     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
097800950718     C*------------------------------------------------------------------------*
097900950718     C     RDNSTD        BEGSR
098000950718     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
098100950718     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
098200950718     C     CURSOR        IFGE      POSMIN
098300950718     C     CURSOR        ANDLE     POSMAX
098400950718     C                   ELSE
098500950718     C* INVIO MESSAGGIO DI INIZIO ELENCO
098600950718     C                   MOVEL     '1'           $ERR
098700950718     C                   MOVEL     MSG(5)        $MSG
098800950718     C                   EXSR      CARMSG
098900950718     C                   ENDIF
099000950718     C                   ENDSR
099100950718     C*------------------------------------------------------------------------*
099200950718     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
099300950718     C*------------------------------------------------------------------------*
099400950718     C     ENTSFL        BEGSR
099500950718     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
099600950718     C                   EXSR      NEWPOS
099700950718     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
099800950718     C     $ERR          IFEQ      '0'
099900950718     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
100000950718     C     SWPOSI        IFNE      0
100100950718     C                   EXSR      POSSFL
100200950718     C                   ELSE
100300950718     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
100400950718     C     SFLULT        IFNE      0
100500950718     C                   EXSR      OPZSFL
100600950718     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
100700950718     C     SWMODI        IFNE      0
100800950718     C                   EXSR      POSSFL
100900950718     C                   ENDIF
101000950718     C                   ENDIF
101100950718     C                   ENDIF
101200950718     C                   ENDIF
101300950718     C                   ENDSR
101400950718     C*------------------------------------------------------------------------*
101500950718     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
101600950718     C*------------------------------------------------------------------------*
101700950718     C     POSSFL        BEGSR
101800950718     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
101900950718     C                   EXSR      CLRSFL
102000950718     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
102100950718     C                   EXSR      SETFIL
102200950718     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
102300950718     C     SFLEOF        IFEQ      0
102400950718     C                   EXSR      CARSFL
102500950718     C                   ENDIF
102600950718     C                   ENDSR
102700950718     C*------------------------------------------------------------------------*
102800950718     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
102900950718     C*------------------------------------------------------------------------*
103000950718     C     OPZSFL        BEGSR
103100950718     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
103200950718     C                   MOVEL     'S'           OPZOK             1
103300950718     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
103400950718     C                   Z-ADD     0             SFLVAR            5 0
103500950718     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
103600950718     C                   Z-ADD     0             SFLERR
103700950718     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
103800950718     C                   Z-ADD     SFLULT        WFLULT
103900950718     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
104000950718     C                   READC     XCLIDVS                                49
104100950718     C     *IN49         DOWEQ     '0'
104200950718     C     $ERR          ANDEQ     '0'
104300950718     C* ADD CONTATORE DI RIGHE SFL VARIATE
104400950718     C                   ADD       1             SFLVAR
104500950718     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
104600950718     C     OPZOK         IFEQ      'S'
104700950718     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
104800950718     C     VSCOPZ        IFNE      *BLANKS
104900950718     C                   EXSR      CONOPZ
105000950718     C     OPZOK         IFEQ      'S'
105100950718     C     $ERR          ANDEQ     '0'
105200950718     C                   EXSR      GESOPZ
105300950718     C                   ENDIF
105400950718     C                   ENDIF
105500950718     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
105600950718     C*            2. PULIZIA SCELTA IN SFL
105700950718     C     OPZOK         IFEQ      'S'
105800950718     C                   MOVEL     '0'           *IN41
105900950718     C                   MOVEL     *BLANKS       VSCOPZ
106000950718     C                   ELSE
106100950718     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
106200950718     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
106300950718     C                   MOVEL     '1'           *IN41
106400950718     C                   Z-ADD     SFLULT        SFLERR
106500950718     C                   ENDIF
106600950718     C                   ELSE
106700950718     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
106800950718     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
106900950718     C                   MOVEL     '1'           *IN41
107000950718     C                   ENDIF
107100950718     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
107200950718     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
107300950718     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
107400950718     C     OPZOK         IFNE      'S'
107500950718     C                   Z-ADD     SFLERR        VCCNBR
107600950718     C                   ELSE
107700950718     C                   Z-ADD     SFLULT        VCCNBR
107800950718     C                   ENDIF
107900950718     C* AGGIORNO RECORD SFL
108000950718     C                   UPDATE    XCLIDVS
108100950718     C     $ERR          IFEQ      '0'
108200950718     C                   READC     XCLIDVS                                49
108300950718     C                   ENDIF
108400950718     C                   ENDDO
108500950718     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
108600950718     C                   Z-ADD     WFLULT        SFLULT
108700950718     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
108800950718     C     SFLVAR        IFEQ      0
108900950718     C                   MOVEL     '1'           $FINE
109000950718     C                   ENDIF
109100950718     C                   ENDSR
109200950718     C*------------------------------------------------------------------------*
109300950718     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
109400950718     C*------------------------------------------------------------------------*
109500950718     C     CLRSFL        BEGSR
109600950718     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
109700950718     C* (0=NON FINE FILE  1=FINE FILE)
109800950718     C                   Z-ADD     0             SFLEOF
109900950718     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
110000950718     C                   Z-ADD     0             SFLULT
110100950718     C* ACCENSIONE INDICATORE PER CLEAR SFL
110200950718     C                   MOVEL     '1'           *IN40
110300950718     C                   WRITE     XCLIDVC
110400950718     C                   MOVEL     '0'           *IN40
110500950718     C                   ENDSR
110600950718     C*------------------------------------------------------------------------*
110700950718     C* CARSFL - CARICAMENTO SFL
110800950718     C*------------------------------------------------------------------------*
110900950718     C     CARSFL        BEGSR
111000950718     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
111100950718     C     SFLULT        IFEQ      0
111200950718     C                   EXSR      SETFIL
111300950718     C                   ENDIF
111400950718     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
111500950718     C                   Z-ADD     SFLULT        WFLULT
111600950718     C* CICLO:
111700950718     C*    -CARICO NUMERO DI RIGHE=SFLPAG
111800950718     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
111900950718     C                   Z-ADD     1             SFLRIG
112000950718     C     SFLRIG        DOWLE     SFLPAG
112100950718     C     SFLEOF        ANDEQ     0
112200950718     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
112300950718     C                   MOVEL     *BLANKS       VSCOPZ
112400950718     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
112500950718     C                   EXSR      IRESFL
112600950718     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
112700950718     C                   ADD       1             SFLULT
112800950718     C* SCRITTURA RECORD SFL
112900950718     C                   WRITE     XCLIDVS
113000950718     C                   ADD       1             SFLRIG
113100950718     C* LETTURA FILE DA CARICARE
113200950718     C                   EXSR      LETFIL
113300950718     C                   ENDDO
113400950718     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
113500950718     C     WFLULT        IFNE      SFLULT
113600950718     C                   Z-ADD     WFLULT        VCCNBR
113700950718     C                   ADD       1             VCCNBR
113800950718     C                   ENDIF
113900950718     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
114000950718     C     SFLEOF        IFEQ      1
114100950718     C     SFLULT        ANDGT     *ZEROS
114200950718     C                   MOVEL     '1'           $ERR
114300950718     C                   MOVEL     MSG(6)        $MSG
114400950718     C                   EXSR      CARMSG
114500950718     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
114600950718     C     SFLULT        CHAIN     XCLIDVS                            99
114700950718     C     *IN99         IFEQ      '0'
114800950718     C                   SETON                                        45
114900950718     C                   UPDATE    XCLIDVS
115000950718     C                   SETOFF                                       45
115100950718     C                   ENDIF
115200950718     C                   ENDIF
115300950718     C                   ENDSR
115400950718     C*------------------------------------------------------------------------*
115500950718     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
115600950718     C*------------------------------------------------------------------------*
115700950718     C     OUTSFL        BEGSR
115800950718     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
115900950718     C     MSGULT        IFNE      *ZEROS
116000950718     C                   EXSR      OUTMSG
116100950718     C                   ENDIF
116200950718     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
116300950718     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
116400950718     C     SFLEOF        IFEQ      0
116500950718     C                   MOVEL     '0'           *IN42
116600950718     C                   ELSE
116700950718     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
116800950718     C                   MOVEL     '1'           *IN42
116900950718     C                   ENDIF
117000950718     C* EMISSIONE SUBFILE
117100950718     C     SFLULT        IFLE      0
117200950718     C*  --> CASO DI SFL VUOTO
117300950718     C                   MOVEL     '0'           *IN43
117400950718     C                   WRITE     XCLIDVV
117500950718     C                   WRITE     XCLIDVC
117600950718     C                   READ      XCLIDVC                                99
117700950718     C                   ELSE
117800950718     C*  --> CASO DI SFL CON RECORDS
117900950718     C                   WRITE     XCLIDVC
118000950718     C                   READ      XCLIDVC                                99
118100950718     C                   ENDIF
118200950718     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
118300950718     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
118400950718     C                   Z-ADD     FRTSFL        VCCNBR
118500950718     C                   ENDSR
118600950718     C*------------------------------------------------------------------------*
118700950718     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
118800950718     C*          CODA MESSAGGI DEL PGM
118900950718     C*------------------------------------------------------------------------*
119000950718     C     CARMSG        BEGSR
119100950718     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
119200950718     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
119300950718     C     MSGULT        IFEQ      0
119400950718     C                   MOVEL     '1'           *IN40
119500950718     C                   WRITE     XCLIDVK
119600950718     C                   MOVEL     '0'           *IN40
119700950718     C                   ENDIF
119800950718     C* INVIA MESSAGGIO ALLA CODA DEL PGM
119900950718     C                   MOVEL     $MSG          DSMSMS
120000950718     C                   CALL      'XMSG'        XMSGPA
120100950718     C     DSMSER        IFEQ      0
120200950718     C                   MOVEL     DSMSKE        VMCKEY
120300950718     C                   ENDIF
120400950718     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
120500950718     C                   ADD       1             MSGULT            4 0
120600950718     C                   WRITE     XCLIDVM
120700950718     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
120800950718     C                   MOVEL     *BLANKS       $MSG
120900950718     C                   ENDSR
121000950718     C*------------------------------------------------------------------------*
121100950718     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
121200950718     C*------------------------------------------------------------------------*
121300950718     C     OUTMSG        BEGSR
121400950718     C* VISUALIZZAZIONE MESSAGGIO
121500950718     C                   MOVEL     '1'           *IN44
121600950718     C                   WRITE     XCLIDVK
121700950718     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
121800950718     C                   Z-ADD     *ZEROS        MSGULT
121900950718     C                   ENDSR
122000950718     C*------------------------------------------------------------------------*
122100950718     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
122200950718     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
122300950718     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
122400950718     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
122500950718     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
122600950718     C*------------------------------------------------------------------------*
122700950718     C     CARFUN        BEGSR
122800950718     C                   Z-ADD     *ZEROS        FUNULT            5 0
122900950718     C     1             DO        31            I                 5 0
123000950718     C                   MOVEL     FUN(I)        FUNRIG
123100950718     C                   MOVEL     FUNFUN        FFU(I)
123200950718     C                   MOVEL     FUNDES        FDE(I)
123300020905     C     DUTLPO        IFEQ      'S'
123400950718     C     $TGES         IFEQ      'G'
123500950718     C                   MOVEL     FUNASG        FAT(I)
123600950718     C                   ELSE
123700950718     C                   MOVEL     FUNASI        FAT(I)
123800950718     C                   ENDIF
123900950718     C                   ELSE
124000950718     C     $TGES         IFEQ      'G'
124100950718     C                   MOVEL     FUNAFG        FAT(I)
124200950718     C                   ELSE
124300950718     C                   MOVEL     FUNAFI        FAT(I)
124400950718     C                   ENDIF
124500950718     C                   ENDIF
124600950718     C     FAT(I)        IFEQ      'E'
124700950718     C                   MOVEL     USER          WEDP              3
124800950718     C     WEDP          IFEQ      'EDP'
124900950718     C                   MOVEL     'S'           FAT(I)
125000950718     C                   ELSE
125100950718     C                   MOVEL     'N'           FAT(I)
125200950718     C                   ENDIF
125300950718     C                   ENDIF
125400950718     C                   ENDDO
125500950718     C                   ENDSR
125600950718     C*------------------------------------------------------------------------*
125700950718     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
125800950718     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
125900950718     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
126000950718     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
126100950718     C*------------------------------------------------------------------------*
126200950718     C     OUTFUN        BEGSR
126300950718     C     1             DO        8             I
126400950718     C                   MOVEL     *BLANKS       FUV(I)
126500950718     C                   ENDDO
126600950718     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
126700950718     C                   Z-ADD     *ZEROS        FUNNAT            2 0
126800950718     C     1             DO        31            I
126900950718     C     FAT(I)        IFEQ      'S'
127000950718     C     FDE(I)        ANDNE     *BLANKS
127100950718     C                   ADD       1             FUNNAT
127200950718     C                   ENDIF
127300950718     C                   ENDDO
127400950718     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
127500950718     C     FUNNAT        IFGT      8
127600950718     C                   MOVEL     'S'           FAT(24)
127700950718     C                   ENDIF
127800950718     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
127900950718     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
128000950718     C                   Z-ADD     8             FUNVIS            5 0
128100950718     C     FAT(23)       IFEQ      'S'
128200950718     C                   SUB       1             FUNVIS
128300950718     C                   ENDIF
128400950718     C     FAT(24)       IFEQ      'S'
128500950718     C                   SUB       1             FUNVIS
128600950718     C                   ENDIF
128700950718     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
128800950718     C                   Z-ADD     *ZEROS        W                 5 0
128900950718     C                   ADD       1             FUNULT
129000950718     C     FUNULT        IFGT      31
129100950718     C                   Z-ADD     1             FUNULT
129200950718     C                   ENDIF
129300950718     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
129400950718     C     FUNULT        DO        31            I
129500950718     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
129600950718     C     W             IFLT      FUNVIS
129700950718     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
129800950718     C     FAT(I)        IFEQ      'S'
129900950718     C     FDE(I)        ANDNE     *BLANKS
130000950718     C     I             IFEQ      23
130100950718     C     I             OREQ      24
130200950718     C                   ELSE
130300950718     C                   ADD       1             W
130400950718     C                   MOVEL     FDE(I)        FUV(W)
130500950718     C                   ENDIF
130600950718     C                   ENDIF
130700950718     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
130800950718     C                   Z-ADD     I             FUNULT
130900950718     C                   ENDIF
131000950718     C                   ENDDO
131100950718     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
131200950718     C                   MOVEL     FUV(1)        VFD001
131300950718     C                   MOVEL     FUV(2)        VFD002
131400950718     C                   MOVEL     FUV(3)        VFD003
131500950718     C                   MOVEL     FUV(4)        VFD004
131600950718     C                   MOVEL     FUV(5)        VFD005
131700950718     C                   MOVEL     FUV(6)        VFD006
131800950718     C     FAT(23)       IFEQ      'N'
131900950718     C     FAT(24)       ANDEQ     'N'
132000950718     C                   MOVEL     FUV(7)        VFD007
132100950718     C                   MOVEL     FUV(8)        VFD008
132200950718     C                   ELSE
132300950718     C     FAT(23)       IFEQ      'S'
132400950718     C     FAT(24)       ANDEQ     'S'
132500950718     C                   MOVEL     FDE(23)       VFD007
132600950718     C                   MOVEL     FDE(24)       VFD008
132700950718     C                   ELSE
132800950718     C                   MOVEL     FUV(7)        VFD007
132900950718     C     FAT(23)       IFEQ      'S'
133000950718     C                   MOVEL     FDE(23)       VFD008
133100950718     C                   ELSE
133200950718     C                   MOVEL     FDE(24)       VFD008
133300950718     C                   ENDIF
133400950718     C                   ENDIF
133500950718     C                   ENDIF
133600950718     C                   MOVEL     '1'           $TESVD
133700950718     C                   ENDSR
133800950718     C*------------------------------------------------------------------------*
133900950718     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
134000950718     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
134100950718     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
134200950718     C*                   - ERRORE SE FUNZIONE NON ATTIVA
134300950718     C*------------------------------------------------------------------------*
134400950718     C     CONFUN        BEGSR
134500950718     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
134600950718     C     1             DO        31            I
134700950718     C     KEY           IFEQ      KEYS(I)
134800950718     C* CONTROLLO SE IL TASTO E' ATTIVO
134900950718     C     FAT(I)        IFNE      'S'
135000950718     C                   MOVEL     MSG(1)        $MSG
135100950718     C                   EXSR      CARMSG
135200950718     C                   MOVEL     '1'           $ERR
135300950718     C                   ENDIF
135400950718     C                   ENDIF
135500950718     C                   ENDDO
135600950718     C                   ENDSR
135700950718     C*------------------------------------------------------------------------*
135800950718     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
135900950718     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
136000950718     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
136100950718     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
136200950718     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
136300950718     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
136400950718     C*------------------------------------------------------------------------*
136500950718     C     CAROPZ        BEGSR
136600950718     C                   Z-ADD     *ZEROS        OPZULT            5 0
136700950718     C     1             DO        24            I
136800950718     C                   MOVEL     OPZ(I)        OPZRIG
136900950718     C                   MOVEL     OPZOPD        OPD(I)
137000950718     C                   MOVEL     OPZOPS        OPS(I)
137100950718     C                   MOVEL     OPZDES        ODE(I)
137200020905     C     DUTLPO        IFEQ      'S'
137300950718     C     $TGES         IFEQ      'G'
137400950718     C                   MOVEL     OPZASG        OAT(I)
137500950718     C                   ELSE
137600950718     C                   MOVEL     OPZASI        OAT(I)
137700950718     C                   ENDIF
137800950718     C                   ELSE
137900950718     C     $TGES         IFEQ      'G'
138000950718     C                   MOVEL     OPZAFG        OAT(I)
138100950718     C                   ELSE
138200950718     C                   MOVEL     OPZAFI        OAT(I)
138300950718     C                   ENDIF
138400950718     C                   ENDIF
138500950718     C     OAT(I)        IFEQ      'E'
138600950718     C                   MOVEL     USER          WEDP              3
138700950718     C     WEDP          IFEQ      'EDP'
138800950718     C                   MOVEL     'S'           OAT(I)
138900950718     C                   ELSE
139000950718     C                   MOVEL     'N'           OAT(I)
139100950718     C                   ENDIF
139200950718     C                   ENDIF
139300950718     C                   ENDDO
139400950718     C                   ENDSR
139500950718     C*------------------------------------------------------------------------*
139600950718     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
139700950718     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
139800950718     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
139900950718     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
140000950718     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
140100950718     C*------------------------------------------------------------------------*
140200950718     C     OUTOPZ        BEGSR
140300950718     C     1             DO        8             I
140400950718     C                   MOVEL     *BLANKS       OPV(I)
140500950718     C                   ENDDO
140600950718     C                   Z-ADD     *ZEROS        OPZNAT            2 0
140700950718     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
140800950718     C                   ADD       1             OPZULT
140900950718     C     OPZULT        IFGT      24
141000950718     C                   Z-ADD     1             OPZULT
141100950718     C                   END
141200950718     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
141300950718     C     1             DO        24            I
141400950718     C     OAT(I)        IFEQ      'S'
141500950718     C     ODE(I)        ANDNE     *BLANKS
141600950718     C                   ADD       1             OPZNAT
141700950718     C                   ENDIF
141800950718     C                   ENDDO
141900950718     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
142000950718     C                   Z-ADD     0             W
142100950718     C     OPZULT        DO        24            I
142200950718     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
142300950718     C     W             IFLT      8
142400950718     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
142500950718     C     OAT(I)        IFEQ      'S'
142600950718     C     ODE(I)        ANDNE     *BLANKS
142700950718     C                   ADD       1             W
142800950718     C                   MOVEL     ODE(I)        OPV(W)
142900950718     C                   ENDIF
143000950718     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
143100950718     C                   Z-ADD     I             OPZULT
143200950718     C                   ENDIF
143300950718     C                   ENDDO
143400950718     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
143500950718     C                   MOVEL     OPV(1)        VOD001
143600950718     C                   MOVEL     OPV(2)        VOD002
143700950718     C                   MOVEL     OPV(3)        VOD003
143800950718     C                   MOVEL     OPV(4)        VOD004
143900950718     C                   MOVEL     OPV(5)        VOD005
144000950718     C                   MOVEL     OPV(6)        VOD006
144100950718     C                   MOVEL     OPV(7)        VOD007
144200950718     C                   MOVEL     OPV(8)        VOD008
144300950718     C                   MOVEL     '1'           $TESVD
144400950718     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
144500950718     C     OPZNAT        IFGT      8
144600950718     C                   MOVEL     'S'           FAT(23)
144700950718     C                   ENDIF
144800950718     C                   ENDSR
144900950718     C*------------------------------------------------------------------------*
145000950718     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
145100950718     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
145200950718     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
145300950718     C*                   - ERRORE SE OPZIONE NON ATTIVA
145400950718     C*------------------------------------------------------------------------*
145500950718     C     CONOPZ        BEGSR
145600950718     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
145700950718     C                   MOVEL     *BLANKS       OPZOK             1
145800950718     C     1             DO        24            I                 5 0
145900950718     C     VSCOPZ        IFEQ      OPD(I)
146000950718     C     VSCOPZ        OREQ      OPS(I)
146100950718     C     OAT(I)        IFEQ      'S'
146200950718     C                   MOVEL     OPD(I)        VSCOPZ
146300950718     C                   MOVEL     'S'           OPZOK
146400950718     C                   ELSE
146500950718     C                   MOVEL     'N'           OPZOK             1
146600950718     C                   ENDIF
146700950718     C                   ENDIF
146800950718     C                   ENDDO
146900950718     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
147000950718     C     OPZOK         IFNE      'S'
147100950718     C                   MOVEL     MSG(3)        $MSG
147200950718     C                   EXSR      CARMSG
147300950718     C                   MOVEL     '1'           $ERR
147400950718     C                   MOVEL     'N'           OPZOK             1
147500950718     C                   ENDIF
147600950718     C                   ENDSR
147700960705** ======== SCHIERA: MSG  (MESSAGGI)                       ================
147800950718XCLIR  - Tasto funzionale non valido.                                          1
147900950718XCLIR  - Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
148000950718XCLIR  - Opzione non valida.                                                   3
148100950718XCLIR  - Opzione abilitata ma non gestita. Avvertire CED.                      4
148200950718XCLIR  - Inizio elenco.                                                        5
148300950718XCLIR  - Fine elenco.                                                          6
148400950718XCLIR  - Accettato solo un parametro di inizio elenco.                         7
148500950718XCLIR  - 8
148600950718XCLIR  - 9
148700950718XCLIR  - 10
148800950718XCLIR  - 11
148900950718XCLIR  - 12
149000950718XCLIR  - 13
149100950718XCLIR  - 14
149200950718XCLIR  - 15
149300950718XCLIR  - 16
149400950718XCLIR  - 17
149500950718XCLIR  - 18
149600950718XCLIR  - 19
149700950718XCLIR  - 20
149800950718** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
149900950718- 1-1 -1=Scelta           -    SS     SS
150000950718- 2-2 -                   -    NN     NN
150100950718- 3-3 -                   -    NN     NN
150200950718- 4-4 -                   -    NN     NN
150300950718- 5-5 -                   -    NN     NN
150400950718- 6-6 -                   -    NN     NN
150500950718- 7-7 -                   -    NN     NN
150600950718- 8-8 -                   -    NN     NN
150700950718- 9-9 -                   -    NN     NN
150800950718-10-  -                   -    NN     NN
150900950718-11-  -                   -    NN     NN
151000950718-12-  -                   -    NN     NN
151100950718-13-  -                   -    NN     NN
151200950718-14-  -                   -    NN     NN
151300950718-15-  -                   -    NN     NN
151400950718-16-  -                   -    NN     NN
151500950718-17-  -                   -    NN     NN
151600950718-18-  -                   -    NN     NN
151700950718-19-  -                   -    NN     NN
151800950718-20-  -                   -    NN     NN
151900950718-21-  -                   -    NN     NN
152000950718-22-  -                   -    NN     NN
152100950718-23-  -                   -    NN     NN
152200950718-24-  -                   -    NN     NN
152300950718** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
152400950718F1    -                   -    NN     NN
152500950718F2    -                   -    NN     NN
152600950718F3    -F3=Fine            -    SS     SS
152700950718F4    -                   -    NN     NN
152800950718F5    -F5=Rivisualizza    -    SS     SS
152900950718F6    -                   -    NN     NN
153000950718F7    -                   -    NN     NN
153100950718F8    -                   -    NN     NN
153200960611F9    -F9=Altri dati      -    SS     SS
153300950718F10   -                   -    NN     NN
153400950718F11   -                   -    NN     NN
153500950718F12   -F12=Ritorno        -    SS     SS
153600950718F13   -                   -    NN     NN
153700950718F14   -                   -    NN     NN
153800950718F15   -                   -    NN     NN
153900950718F16   -                   -    NN     NN
154000950718F17   -F17=Inizio dati    -    SS     SS
154100950718F18   -F18=Fine dati      -    SS     SS
154200950718F19   -                   -    NN     NN
154300950718F20   -                   -    NN     NN
154400950718F21   -                   -    NN     NN
154500950718F22   -                   -    NN     NN
154600950718F23   -F23=Altre opzioni  -    NN     NN
154700950718F24   -F24=Altri tasti    -    NN     NN
154800950718ENTER -                   -    SS     SS
154900950718ROLLUP-                   -    SS     SS
155000950718ROLLDN-                   -    SS     SS
155100950718HELP  -                   -    NN     NN
155200950718PRINT -                   -    NN     NN
155300950718BSPACE-                   -    NN     NN
155400950718CLEAR -                   -    NN     NN
155500960705** CMD1 - COMANDI CLP
155600960705CHKOBJ OBJ(TFNTC01L) OBJTYPE(*FILE)
