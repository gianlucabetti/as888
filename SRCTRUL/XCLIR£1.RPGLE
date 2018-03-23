000100950718     H*------------------------------------------------------------------------*
000200950718     H*             RICERCA E SCELTA CLIENTE - SELEZIONE MULTIPLA              *
000300950718     H* VIENE LANCIATO CON UN "?" E PERMETTE LA RICERCA E LA SELEZIONE DI UNO  *
000400950718     H* O PIU' CLIENTI (RICERCA PER CODICE O PER DESCRIZIONE) FRA QUELLI DEL   *
000500950718     H* SOTTOCONTO PASSATO O FRA TUTTI SE IL SOTTOCONTO E' UGUALE A ZERO.      *
000600950718     H*------------------------------------------------------------------------*
000700950718     H DECEDIT('0,') DATEDIT(*DMY.)
000800950718     F*------------------------------------------------------------------------*
000900950718     F* DATA BASE
001000950718     F*------------------------------------------------------------------------*
001100950718     FXCLID     CF   E             WORKSTN INFDS(DEVDS1)
001200950718     F                                     SFILE(XCLIDVS:SFLULT)
001300950718     F                                     SFILE(XCLIDVM:MSGULT)
001400950718     F*-----------
001500950718     F* KUT+KCC+KSC
001600950718     F*-----------
001700950718     FCNACO00F  IF   E           K DISK
001800960705     FTFNTC01L  IF   E           K DISK    USROPN
001900950718     F*-----------
002000961017     F* FTT+FLT+KCC+RAG
002100950718     F*-----------
002200961017     FCNACO04L  IF   E           K DISK
002300961017     F                                     RENAME(CNACO000:CNACO4)
002400100629
002500100629       // -?Anagrafica particolarità clienti?
002600100629     fCNCLP00F  if   e           k disk    usropn
002700100629
002800130909       // -?Anagrafica Commerciali?
002900130909     fAZCMM01L  if   e           k disk    usropn
003000100629
003100950718     D*------------------------------------------------------------------------*
003200950718     D* SCHIERE
003300950718     D*------------------------------------------------------------------------*
003400950718     D*-------------------
003500950718     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
003600950718     D*-------------------
003700950718     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003800950718     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
003900950718     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
004000950718     D FUN             S             40    DIM(31)
004100950718     D*-------------------
004200950718     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
004300950718     D*-------------------
004400950718     D FFU             S              6    DIM(31)
004500950718     D FDE             S             19    DIM(31)
004600950718     D FAT             S              1    DIM(31)
004700950718     D*-------------------
004800950718     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
004900950718     D*-------------------
005000950718     D OPD             S              2    DIM(24)
005100950718     D OPS             S              2    DIM(24)
005200950718     D ODE             S             19    DIM(24)
005300950718     D OAT             S              1    DIM(24)
005400950718     D*-------------------
005500950718     D* TABELLA DESCRIZIONI A VIDEO
005600950718     D*-------------------
005700950718     D FUV             S             19    DIM(8)
005800950718     D OPV             S             19    DIM(8)
005900950718     D*-------------------
006000950718     D* TABELLA TASTI FUNZIONALI PREMUTI
006100950718     D*-------------------
006200950718     D KEYS            S              1    DIM(40)
006300950718     D*-------------------
006400950718     D* CAPOCONTO / SOTTOCONTO DI RITORNO
006500950718     D*-------------------
006600950724     D KCC             S              4  0 DIM(30)
006700950724     D KSC             S              7  0 DIM(30)
006800950724     D*-------------------
006900950724     D* PER PASSAGGIO DATI
007000950724     D*-------------------
007100950724     D KCA             S              3    DIM(30)
007200950724     D KSA             S              4    DIM(30)
007300960705     D*-------------------
007400960705     D* COMANDI CLP
007500960705     D*-------------------
007600960705     D CMD1            S             35    DIM(1) CTDATA PERRCD(1)
007700961017     D*-------------------
007800961017     D* FILIALI DEI CLIENTI DA VISUALIZZARE
007900961017     D*-------------------
008000021113     D L1              S              3    DIM(250) INZ(*zeros)
008100100629       // -?Commerciali dei clienti da visualizzare?
008200100629     d $Comm           s                   like(CLPage)
008300100630     d                                     dim(5000)
008400100630     d                                     ascend
008500000821     D*------------------
008600000821     D* DS RICERCA CLIENTI UNIFICANTI
008700000821     D*------------------
008800000821     D BS10DS        E DS                  EXTNAME(TIBS10DS)
008900950718     D*------------------------------------------------------------------------*
009000950718     D* INPUT
009100950718     D*------------------------------------------------------------------------*
009200950718     D*---------------------
009300950718     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
009400950718     D*---------------------
009500950718     D DSOPZ         E DS
009600950718     D  OPZRIG                 1     40
009700950718     D*---------------------
009800950718     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
009900950718     D*---------------------
010000950718     D DSFUN         E DS
010100950718     D  FUNRIG                 1     40
010200950718     D*---------------------
010300950718     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
010400950718     D*---------------------
010500950718     D DSMSG         E DS
010600950718     D*---------------------
010700950718     D* DS PER GESTIONE TASTI FUNZIONALI
010800950718     D*---------------------
010900950718     D DSKEYS        E DS
011000950718     D  KEYRIG                 1     40
011100950718     D*---------------------
011200950718     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
011300950718     D*---------------------
011400950718     D                SDS
011500950718     D  USER                 254    263
011600950718     D*---------------------
011700950718     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
011800950718     D*---------------------
011900950718     D DEVDS1          DS
012000950718     D  KEY                  369    369
012100950718     D  CURSOR               370    371
012200950718     D  FRTSFL               378    379B 0
012300950718     D*---------------------
012400950718     D* DS DI PROCEDURA
012500950718     D*---------------------
012600950718     D DSXC          E DS                  EXTNAME(XCLIRDS)
012700971124     D*---------------------
012800971124     D* DS "XSRDA8" - CONTROLLA DATA (8)
012900971124     D*---------------------
013000971124     D WLBDA8          DS
013100971124     D  G08DAT                 1      8  0
013200971124     D  G08INV                 9     16  0
013300971124     D  G08ERR                17     17
013400971124     D  G08TGI                18     22  0
013500950724     D*---------------------
013600950724     D* DS PER PASSAGGIO CAMPI PACKED
013700950724     D*---------------------
013800950724     D                 DS
013900950724     D  DSKCN                  1      3P 0
014000950724     D  DSKCA                  1      3
014100950724     D  DSKSN                  4      7P 0
014200950724     D  DSKSA                  4      7
014300020905     D*-------------
014400020905     D* DS DI WRK
014500020905     D*-------------
014600020905     D TIBS34DS      E DS
014700020905     D DDATIUTE      E DS
014800020905     D AZUTEDS       E DS                  extname(AZUTE00F)
014900100713     d dute01        e ds
015000021113     D*------------------
015100021113     D* DS CARICAMENTO P.O. GESTITI
015200021113     D*-------------------
015300021113     D TRUL31DS      e ds                  INZ
015400020905     D*-------------
015500020905     D* DS DI ARCHITETTURA
015600020905     D*-------------
015700020905     D KPJBA         E DS
015800100629
015900100629       //--------------------------------------------------------------
016000100629       //?Definizione variabili.                                       ?
016100100629       //--------------------------------------------------------------
016200100629
016300100629       // -?Flags booleani?
016400100629     d $Xcomm          s               n
016500100629
016600100629       // -?Indici di schiera?
016700100629     d xx              s              5  0
016800100629
016900100629       //--------------------------------------------------------------
017000100629       //?Definizione key-list.                                        ?
017100100629       //--------------------------------------------------------------
017200100629
017300100629       // -?File CNCLP00F?
017400100629     d k03cnclp00    e ds                  extname(CNCLP00F : *key)
017500100629     d                                     prefix(k_)   inz
017600100629
017700950718     C*------------------------------------------------------------------------*
017800950718     C* MAIN LINE
017900950718     C*------------------------------------------------------------------------*
018000950718     C*
018100950718     C* CICLO FINO A CHE:
018200950718DO  1C     $FINE         DOUEQ     '1'
018300950718     C*
018400950718     C                   MOVEL     '0'           $ERR
018500950718     C*
018600950718     C* CONTROLLA GESTIONE VIDEO
018700950718IF  2C     $GES          IFEQ      'VS'
018800950718     C                   EXSR      GESVS
018900950718E   2C                   ENDIF
019000950718     C*
019100950718E   1C                   ENDDO
019200950718     C*
019300950718IF  1C     $F03          IFEQ      '1'
019400950718     C                   MOVEL     '1'           DXCF03
019500950718E   1C                   ENDIF
019600950724     C*
019700950724     C* COMPATTA I CODICI SELEZIONATI PER PASSARLI NELLA DS
019800950724DO  1C     1             DO        30            C
019900950724     C* CAPOCONTO
020000950724     C                   Z-ADD     KCC(C)        DSKCN
020100950724     C                   MOVE      DSKCA         KCA(C)
020200950724     C* SOTTOCONTO
020300950724     C                   Z-ADD     KSC(C)        DSKSN
020400950724     C                   MOVE      DSKSA         KSA(C)
020500950724E   1C                   ENDDO
020600950724     C                   MOVEA     KCA           DXCKCC
020700950724     C                   MOVEA     KSA           DXCKSC
020800950724     C*
020900971128     C* OPERAZIONI FINALI
021000971128     C                   EXSR      FINSR
021100971128     C*
021200950718     C                   SETON                                        LR
021300971128     C*------------------------------------------------------------------------*
021400971128     C* FINSR - OPERAZIONI FINALI
021500971128     C*------------------------------------------------------------------------*
021600971128     C     FINSR         BEGSR
021700971128     C*
021800971128     C*
021900971128     C                   ENDSR
022000950718     C*------------------------------------------------------------------------*
022100950718     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
022200950718     C*------------------------------------------------------------------------*
022300950718     C     CARVS         BEGSR
022400950718     C*
022500950718     C                   MOVEL     'VS'          $GES
022600950718     C*
022700950718     C                   EXSR      CAROPZ
022800950718     C                   EXSR      OUTOPZ
022900950718     C                   MOVEA     FUN2          FUN
023000950718     C                   EXSR      CARFUN
023100950718     C                   EXSR      OUTFUN
023200950718     C*
023300950718     C                   WRITE     XCLIDVT
023400950718     C                   WRITE     XCLIDVO
023500950718     C                   WRITE     XCLIDVF
023600950718     C*
023700950718     C                   EXSR      POSSFL
023800950718     C*
023900950718     C                   ENDSR
024000950718     C*------------------------------------------------------------------------*
024100950718     C* GESVS  - GESTIONE SUBFILE
024200950718     C*------------------------------------------------------------------------*
024300950718     C     GESVS         BEGSR
024400950718     C*
024500950718IF  1C     $TESVD        IFEQ      '1'
024600950718     C                   WRITE     XCLIDVT
024700950718     C                   WRITE     XCLIDVO
024800950718     C                   WRITE     XCLIDVF
024900950718E   1C                   ENDIF
025000950718     C*
025100950718     C                   EXSR      OUTSFL
025200950718     C*
025300950718     C                   EXSR      CONFUN
025400950718     C*
025500950718IF  1C     $ERR          IFEQ      '0'
025600950718SL  2C                   SELECT
025700950718     C                   OTHER
025800950718     C                   EXSR      FUNSFL
025900950718E   2C                   ENDSL
026000950718E   1C                   ENDIF
026100950718     C*
026200950718     C                   ENDSR
026300950718     C*------------------------------------------------------------------------*
026400950718     C*  GESOPZ - GESTIONE OPZIONI
026500950718     C*------------------------------------------------------------------------*
026600950718     C     GESOPZ        BEGSR
026700950718     C*
026800950718SL  1C                   SELECT
026900950718     C     VSCOPZ        WHENEQ    ' 1'
027000950718     C                   EXSR      O01GES
027100950718     C                   OTHER
027200950718     C                   MOVEL     MSG(4)        $MSG
027300950718     C                   EXSR      CARMSG
027400950718E   1C                   ENDSL
027500950718     C*
027600950718     C                   ENDSR
027700950718     C*------------------------------------------------------------------------*
027800950718     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
027900950718     C*------------------------------------------------------------------------*
028000950718     C     O01GES        BEGSR
028100950718     C*
028200950718IF  1C     $ERR          IFEQ      '0'
028300950721     C     C             ANDLT     30
028400950718     C                   ADD       1             C
028500950718     C                   MOVEL     VSCKCC        KCC(C)
028600950718     C                   MOVEL     VSCKSC        KSC(C)
028700950718     C                   MOVEL     '1'           $FINE
028800950718E   1C                   ENDIF
028900950718     C*
029000950718     C                   ENDSR
029100950718     C*------------------------------------------------------------------------*
029200950718     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
029300950718     C*------------------------------------------------------------------------*
029400950718     C     NEWPOS        BEGSR
029500950718     C*
029600950718     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
029700950718     C*        1 = RICHIESTO POSIZIONAMENTO
029800950718     C                   Z-ADD     0             SWPOSI            7 0
029900950718     C*
030000950718     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
030100950718     C*            NON OCCORRE RICARICARE IL SUBFILE
030200950718     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
030300950718     C                   Z-ADD     0             SWMODI            1 0
030400950718     C*
030500950718     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
030600950718IF  1C     VCCKSC        IFNE      DEPKSC
030700950718     C     VCCRAG        ORNE      DEPRAG
030800950718     C*
030900950718     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLA I PARAMETRI
031000950718     C                   EXSR      CHKPAR
031100950718     C*
031200950718     C* SE I PARAMETRI SONO OK, LI MEMORIZZA E IMPOSTA FLAG PARAMETRI MODIFICATI
031300950718IF  2C     $ERR          IFEQ      '0'
031400950718     C                   Z-ADD     1             SWPOSI
031500950718     C                   Z-ADD     VCCKSC        DEPKSC
031600950718     C                   MOVEL     VCCRAG        DEPRAG
031700950718E   2C                   ENDIF
031800950718E   1C                   ENDIF
031900950718     C*
032000950718     C                   ENDSR
032100950718     C*------------------------------------------------------------------------*
032200950718     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
032300950718     C*------------------------------------------------------------------------*
032400950718     C     CHKPAR        BEGSR
032500950718     C*
032600950718     C* REIMPOSTA INDICATORI EVIDENZAZIONE ERRORI SUI PARAMETRI
032700950718     C                   SETOFF                                       8081
032800950718     C*
032900950718     C* REIMPOSTA INDICATORI EVIDENZAZIONE ORDINAMENTO
033000950718     C                   SETOFF                                       5051
033100950718     C*--------------------
033200950718     C* ORDINAMENTO
033300950718     C*--------------------
033400950718     C* SE TUTTI I PARAMETRI SONO VUOTI, UTILIZZO CHIAVE "COD"
033500950718IF  1C     VCCKSC        IFEQ      *ZEROS
033600950718     C     VCCRAG        ANDEQ     *BLANKS
033700100629     C*//                MOVEL     'COD'         WTIPRI            3
033800100629     C                   MOVEL     'RAG'         WTIPRI            3
033900950718     C                   SETON                                        50
034000950718X   1C                   ELSE
034100950718     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
034200950718IF  2C     VCCKSC        IFNE      *ZEROS
034300950718     C     VCCRAG        ANDNE     *BLANKS
034400950718     C                   MOVEL     MSG(7)        $MSG
034500950718     C                   EXSR      CARMSG
034600950718     C                   MOVEL     '1'           $ERR
034700950718     C                   SETON                                        8081
034800950718X   2C                   ELSE
034900950718     C***
035000950718     C* INIZIO ELENCO DA  C O D I C E
035100950718     C***
035200950718IF  3C     VCCKSC        IFNE      *ZEROS
035300950718     C                   MOVEL     'COD'         WTIPRI
035400950718     C                   SETON                                        50
035500950718E   3C                   ENDIF
035600950718     C***
035700950718     C* INIZIO ELENCO DA  R A G I O N E   S O C I A L E
035800950718     C***
035900950718IF  3C     VCCRAG        IFNE      *BLANKS
036000950718     C                   MOVEL     'RAG'         WTIPRI
036100950718     C                   SETON                                        51
036200950718E   3C                   ENDIF
036300950718     C*
036400950718E   2C                   ENDIF
036500950718E   1C                   ENDIF
036600950718     C*
036700950718     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
036800950718     C* CURSORE SUL CAMPO DEL CONTROL
036900950718IF  1C     $ERR          IFEQ      '1'
037000950718     C                   MOVEL     '0'           *IN43
037100950718X   1C                   ELSE
037200950718     C                   MOVEL     '1'           *IN43
037300950718E   1C                   ENDIF
037400950718     C*
037500950718     C                   ENDSR
037600950718     C*------------------------------------------------------------------------*
037700950718     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA
037800950718     C*------------------------------------------------------------------------*
037900950718     C     SETFIL        BEGSR
038000950718     C*
038100950718     C                   MOVEL     '1'           *IN43
038200950718     C*--------------------
038300950718     C* POSIZIONAMENTO PER  C O D I C E
038400950718     C*--------------------
038500950718IF  1C     WTIPRI        IFEQ      'COD'
038600020905     C                   Z-ADD     1             KACKUT
038700950718     C                   Z-ADD     DXCCAP        KACKCC
038800961017IF  2C     VCCKSC        IFGT      *ZEROS
038900950718     C                   Z-ADD     VCCKSC        KACKSC
039000961017X   2C                   ELSE
039100961017     C                   Z-ADD     *ZEROS        KACKSC
039200100629     c                   if        NOT $Xcomm
039300100629     C                   MOVE(P)   L1(1)         KACKSC
039400100629     c                   endif
039500961017E   2C                   ENDIF
039600950718     C     KEYACO        SETLL     CNACO00F                           99
039700961017E   1C                   ENDIF
039800950718     C*--------------------
039900950718     C* POSIZIONAMENTO PER  R A G I O N E   S O C I A L E
040000950718     C*--------------------
040100961017IF  1C     WTIPRI        IFEQ      'RAG'
040200020905     C                   Z-ADD     1             KACKUT
040300961017     C                   Z-ADD     DXCCAP        KACKCC
040400961017     C                   MOVEL     VCCRAG        KACRAG
040500961017     C     KE2ACO        SETLL     CNACO04L                           99
040600961017E   1C                   ENDIF
040700950718     C*
040800950718     C  N99              EXSR      LETFIL
040900950718     C*
041000950718     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
041100950718IF  1C     *IN99         IFEQ      *ON
041200950718     C                   Z-ADD     1             SFLEOF
041300950718E   1C                   ENDIF
041400950718     C*
041500950718     C                   ENDSR
041600950718     C*------------------------------------------------------------------------*
041700950718     C* LETFIL - LETTURA RECORD SUCESSIVO
041800950718     C*------------------------------------------------------------------------*
041900950718     C     LETFIL        BEGSR
042000950718     C*
042100950718     C* LEGGO FINO A:
042200950718     C*     - FINE FILE
042300950718     C*     - TROVATO RECORD VALIDO
042400950718     C                   MOVEL     'N'           WRECOK            1
042500950718DO  1C     *IN99         DOWEQ     *OFF
042600950718     C     WRECOK        ANDEQ     'N'
042700950718     C*--------------------
042800950718     C* LETTURA PER  C O D I C E
042900950718     C*--------------------
043000950718IF  2C     WTIPRI        IFEQ      'COD'
043100950718     C                   READ      CNACO00F                               99
043200961017E   2C                   ENDIF
043300950718     C*--------------------
043400950718     C* LETTURA PER  R A G I O N E   S O C I A L E
043500950718     C*--------------------
043600961017IF  2C     WTIPRI        IFEQ      'RAG'
043700961017     C                   READ      CNACO04L                               99
043800961017E   2C                   ENDIF
043900950718     C*
044000950718IF  2C     *IN99         IFEQ      *ON
044100950718     C                   Z-ADD     1             SFLEOF
044200950718X   2C                   ELSE
044300950718     C                   EXSR      CHKREC
044400950718E   2C                   ENDIF
044500950718     C*
044600950718E   1C                   ENDDO
044700950718     C*
044800950718     C                   ENDSR
044900950718     C*------------------------------------------------------------------------*
045000950718     C* CHKREC - CONTROLLA VALIDITA' RECORD
045100950718     C*------------------------------------------------------------------------*
045200950718     C     CHKREC        BEGSR
045300950718     C*
045400950718     C                   MOVEL     'S'           WRECOK
045500950718     C*
045600950718     C* CLIENTE ANNULLATO, ESCLUDE
045700950718IF  2C     ACOFLG        IFNE      *BLANKS
045800950718     C                   MOVEL     'N'           WRECOK
045900950718E   2C                   ENDIF
046000950718     C*
046100950718     C* SOTTOCONTO DIVERSO DA QUELLO RICHIESTO, ESCLUDE
046200950718IF  1C     DXCCAP        IFNE      *ZEROS
046300950718     C     DXCCAP        ANDNE     ACOKCC
046400950718     C                   MOVEL     'N'           WRECOK
046500950718E   1C                   ENDIF
046600950718     C*
046700950718     C* CAPOCONTO "FIL+0000", ESCLUDE
046800950718     C                   MOVE      ACOKSC        CAPOC             4 0
046900950718IF  1C     CAPOC         IFEQ      *ZEROS
047000960227     C                   MOVEL     'N'           WRECOK
047100950718E   1C                   ENDIF
047200961017     C*
047300961017     C* CLIENTI NON IN GESTIONE, ESCLUDE
047400020905IF  1C     DUTLPO        IFNE      'S'
047500021113     C                   MOVEL     ACOKSC        WN3               3
047600961017     C                   SETOFF                                           28
047700100629     C     WN3           LOOKUP    L1                                     28
047800100629      /free
047900100629           // -?Se cliente NON in gestione:?
048000100629           //  ?verifica se lo risulta il suo comm.le (o il relativo unificante)
048100100629           if  Not *in28   and   $Xcomm;
048200100630             clear  k03cnclp00;
048300100630             k_CLPkut = ACOkut;
048400100630             k_CLPkcc = ACOkcc;
048500100630             k_CLPksc = ACOksc;
048600100629             chain  %kds(k03cnclp00)  CNCLP000;
048700100629             *in28 = (%found(CNCLP00F)   and
048800100630                      CLPage > *zero     and
048900100629                      %lookup(CLPage : $Comm) > *zero);
049000100629           endif;
049100100629      /end-free
049200961017IF  2C     *IN28         IFEQ      *OFF
049300961017     C                   MOVEL     'N'           WRECOK
049400961017E   2C                   ENDIF
049500961017E   1C                   ENDIF
049600950718     C*
049700950718     C                   ENDSR
049800950718     C*------------------------------------------------------------------------*
049900950718     C* IRESFL - IMPOSTA I CAMPI DEL SUBFILE DAL FILE FISICO
050000950718     C*------------------------------------------------------------------------*
050100950718     C     IRESFL        BEGSR
050200950718     C*
050300960705     C                   Z-ADD     ACOKCC        VSCKCC
050400950718     C                   Z-ADD     ACOKSC        VSCKSC
050500950718     C                   MOVEL     ACORAG        VSCRAG
050600000821     C*
050700000821     C* REPERISCE IL CODICE PADRE DEI CLIENTI
050800000821     C                   CLEAR                   BS10DS
050900000821     C                   Z-ADD     DATCOR        D10DRF
051000050519     C                   MOVEL     INTLE         D10TLE
051100000821     C                   MOVEL     'P'           D10PAF
051200000821     C                   Z-ADD     ACOKSC        D10COD
051300000821     C                   CALL      'TIBS10R'
051400000821     C                   PARM                    BS10DS
051500000821IF  1C     D10ERR        IFEQ      *BLANKS
051600000821     C     D10COP        ANDGT     *ZEROS
051700000821     C                   Z-ADD     D10COP        VSCCOP
051800000821X   1C                   ELSE
051900000821     C                   Z-ADD     ACOKSC        VSCCOP
052000000821E   1C                   ENDIF
052100000905     C*
052200000905     C* IMPOSTA SE IL CLIENTE E' UNIFICANTE O NO
052300000905     C                   MOVEL     *BLANKS       VSCUNI
052400000905     c                   Z-ADD     VSCCOP        N7                7 0
052500000905IF  1C     N7            IFEQ      VSCKSC
052600000905     C                   MOVEL     'U'           VSCUNI
052700000905E   1C                   ENDIF
052800100427      * imposta se il cliente è bloccato o no
052900100427     c                   eval      VSCabl = ACOabl
053000130322      * Visualizzo sempre blocco "8" anche se il blocco sul file è "7"
053100130322     c                   IF        VSCabl = '7'
053200130322     c                   eval      VSCabl = '8'
053300130322     c                   ENDIF
053400000821     C*
053500000821     C* LEGGE LE NOTE DEI CLIENTI PER LA DESCRIZIONE
053600960611     C                   CLEAR                   VSCDS1
053700960611     C                   CLEAR                   VSCDS2
053800960705IF  1C     $ESI          IFEQ      '1'
053900960705     C                   CLEAR                   KNCNK1
054000960705     C                   MOVEL     'C'           KNCAPL
054100960705     C                   MOVEL     'DC'          KNCTNT
054200960705     C                   MOVEL     ACOKCC        KNCNK1
054300960705     C                   MOVE      ACOKSC        KNCNK1
054400010131
054500010131     C     KEYNTC        SETLL     TFNTC01L
054600010131     C                   do        *hival
054700010131     C     KEYNTC        READE     TFNTC01L                               98
054800010131     C   98              leave
054900010131IF  2C     ntcflt        ifne      'A'
055000010131     C                   MOVEL     NTCRNT        W058A            58
055100010131     C                   MOVEL     W058A         VSCDS1
055200010131     C                   MOVE      W058A         VSCDS2
055300010131     C                   leave
055400010131E   2C                   ENDIF
055500010131     C                   enddo
055600010131
055700960705E   1C                   ENDIF
055800950718     C*
055900950718     C                   ENDSR
056000961017     C*------------------------------------------------------------------------*
056100961017     C* CARTAB - CARICA LE TABELLE
056200961017     C*------------------------------------------------------------------------*
056300961017     C     CARTAB        BEGSR
056400020905     C*
056500020905     C* REPERISCE I DATI UTENTE
056600020905     C                   EXSR      REPDATIUTE
056700020905     C*
056800020905     C* INIZIALIZZO LE VARIABILI DI WRK RELATIVE ALLE FILIALI IN GESTIONE
056900020905     C                   CLEAR                   L1
057000021113     C                   CLEAR                   WAbi              2
057100021113     C*
057200021113     C* Se ci sono degli errori sull'abilitazione utente, esce
057300021113     C                   SELECT
057400021113     C*
057500021113     C* Se ho ricevuto in input una autorizzazione speciale => la gestisco
057600100713     C                   WHEN      DXCAUT > *blank and DXCAUT <> '*C'
057700021113     C                   EVAL      WAbi = DXCAUT
057800100713     C* Se ho ricevuto in input *C (nuovo CRM) passo autorizzazione gestione tar
057900100713     C                   WHEN      DXCAUT > *blank and DXCAUT =  '*C'
058000100713     C                   EVAL      WAbi = §utegtc
058100021113     C*
058200021113     C* ...se invece nn ricevo particolari autorizzazioni in input da gestire =>
058300021113     C* dell'utente (1° o 2° livello)
058400021113     C                   OTHER
058500021118     C                   IF        UTEAUT > *blanks
058600021118     C                   EVAL      WAbi = UTEAUT
058700021118     C                   ELSE
058800021118     C*
058900021113     C                   IF        DUTLPO = 'S'
059000021113     C                   EVAL      WAbi = 'AZ'
059100021113     C                   ENDIF
059200021113     C                   IF        DUTLPO = '1'
059300021113     C                   EVAL      WAbi = 'TP'
059400021113     C                   ENDIF
059500021113     C                   IF        DUTLPO = '2'
059600021113     C                   EVAL      WAbi = 'PO'
059700021113     C                   ENDIF
059800021118     C                   ENDIF
059900021113     C                   ENDSL
060000021113     C*
060100021113     C* Reperisce i P.O. gestibili dall'utente
060200021113     C                   CLEAR                   TRUL31DS
060300021113     C                   EVAL      I31abi = WAbi
060400021113     C                   EVAL      I31cpo = DUTpou
060500021113     C                   CALL      'TRUL31R'
060600021113     C                   PARM                    KPJBA
060700021113     C                   PARM                    TRUL31DS
060800021113     C                   IF        O31pog > *zeros
060900021113     C                   MOVEA     O31pog        L1
061000021113     C                   ELSE
061100021113     C                   EVAL      $FINE = '1'
061200021113     C                   ENDIF
061300100629      /free
061400100629
061500130909         // -?Preparazione dell'elenco dei commerciali:?
061600100629         //  ?1) di filiale abilitata?
061700100629         //  ?2) con comm.le unificante di filiale abilitata?
061800100629         //  ?(SE ricevuta apposita richiesta dal chiamante)?
061900100630         clear  $Comm;
062000100629         clear  xx;
062100100629
062200100629         IF  $Xcomm;
062300100629
062400130909           setll  ( *loval )  AZCMM000;
062500130909           read  AZCMM000;
062600130909           DoW  NOT %eof(AZCMM01L);
062700130909             if  %lookup( %subst( %editc( CMMcod : 'X' ) : 1 : 3 )
062800130909                          : L1 ) > *zero   or
062900130909                 %lookup( %subst( %editc( CMMuni : 'X' ) : 1 : 3 )
063000130909                          : L1 ) > *zero;
063100100629               xx += 1;
063200130909               $Comm(xx) = CMMcod;
063300100629             endif;
063400130909             read  AZCMM000;
063500100629           EndDo;
063600100629
063700100629           // -?Ordinamento crescente?
063800100630           DoW  xx < %elem($Comm);
063900100630             xx += 1;
064000100630             $Comm(xx) = *hival;
064100100630           EndDo;
064200100630           sorta  $Comm;
064300100629
064400100629         ENDIF;
064500100629
064600100629      /end-free
064700961017     C*
064800961017     C                   ENDSR
064900020905     C*-------------------------------------------------------------------------
065000020905     C* REPERISCE I DATI UTENTE
065100020905     C*-------------------------------------------------------------------------
065200020905     C     REPDATIUTE    BEGSR
065300020905     C*
065400020905     C* INIZIALIZZA VARIABILI DI WRK
065500020905     C                   CLEAR                   TIBS34DS
065600020905     C                   CLEAR                   AZUTEDS
065700020905     C                   CLEAR                   DDATIUTE
065800100713     C                   CLEAR                   Dute01
065900020905     C*
066000020905     C     *DTAARA       DEFINE    §azute        azuteds
066100020905     C     *DTAARA       DEFINE    §datiute      ddatiute
066200020905     C                   IN(E)     *DTAARA
066300020905if  1C                   IF        %Error
066400020905     c                   EVAL      I34Tla = 'L'
066500020905     C                   CALL      'TIBS34R'
066600020905     C                   PARM                    Tibs34Ds
066700020905     C                   IN        *DTAARA
066800020905e   1C                   ENDIF
066900100713     c*
067000100713     c                   Movel     UteFaf        Dute01
067100020905     C*
067200020905     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
067300020923     C                   MOVEL(P)  RSUT          DSFIRS
067400020905     C*
067500020905     C                   ENDSR
067600950718     C*------------------------------------------------------------------------*
067700961017     C* *INZSR - OPERAZIONI INIZIALI
067800950718     C*------------------------------------------------------------------------*
067900950718     C     *INZSR        BEGSR
068000950718     C*--------------------
068100950718     C* RICEVIMENTO PARAMETRI
068200950718     C*--------------------
068300950718     C     *ENTRY        PLIST
068400950721     C                   PARM                    DSXC
068500050519     C                   PARM                    PARMINTLE         2
068600120321     c                   parm                    ragsoc           48
068700050519     C*--------------------
068800050519     C* EFFETTUO SUBITO CONSIDERAZIONI SU NUOVI PARAMETRI EVENTUALMENTE NN PASSA
068900050519     C*--------------------
069000050519     C                   MOVEL     'ST'          INTLE             2
069100050519     C                   IF        %PARMS > 1
069200050519     C                   EVAL      INTLE = PARMINTLE
069300050519     C                   ENDIF
069400050519     C*
069500050519     C                   IF        INTLE = *blanks
069600050519     C                   EVAL      INTLE = 'ST'
069700050519     C                   ENDIF
069800120321      * se richiamato da SB48 (opzione subfile) non abilito la scelta
069900120321     c                   IF        dxcop0 = 'OPZ'
070000120321     c                   eval      *in46 = *on
070100120321     c                   eval      vccrag = ragsoc
070200120321     c                   ENDIF
070300050519     C*--------------------
070400050519     C* IMPOSTO SUBITO ALCUNDE DESCRIZIONI A VIDEO
070500050519     C*--------------------
070600050519     C                   EVAL      VDDTLE = '(Unificazione per tipo legame ' +
070700050519     C                                      INTLE + ')'
070800950718     C*--------------------
070900950718     C* CAMPI RIFERITI AL DATA BASE
071000950718     C*--------------------
071100950718     C     *LIKE         DEFINE    ACOKUT        KACKUT
071200950718     C     *LIKE         DEFINE    ACOKCC        KACKCC
071300950718     C     *LIKE         DEFINE    ACOKSC        KACKSC
071400961017     C     *LIKE         DEFINE    ACORAG        KACRAG
071500960705     C     *LIKE         DEFINE    NTCAPL        KNCAPL
071600960705     C     *LIKE         DEFINE    NTCNK1        KNCNK1
071700960705     C     *LIKE         DEFINE    NTCNK2        KNCNK2
071800960705     C     *LIKE         DEFINE    NTCTNT        KNCTNT
071900950718     C     *LIKE         DEFINE    ACOKSC        DEPKSC
072000950718     C     *LIKE         DEFINE    ACORAG        DEPRAG
072100020905     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU
072200950718     C*--------------------
072300950718     C* CHIAVI DI LETTURA
072400950718     C*--------------------
072500950718     C* LETTURA CNACO00F - COMPLETA
072600950718     C     KEYACO        KLIST
072700950718     C                   KFLD                    KACKUT
072800950718     C                   KFLD                    KACKCC
072900950718     C                   KFLD                    KACKSC
073000961017     C* LETTURA CNACO04L - COMPLETA
073100950718     C     KE2ACO        KLIST
073200961017     C                   KFLD                    KACKUT
073300961017     C                   KFLD                    KACKCC
073400961017     C                   KFLD                    KACRAG
073500960611     C* LETTURA TFNTC01L - COMPLETA
073600960705     C     KEYNTC        KLIST
073700960705     C                   KFLD                    KNCAPL
073800960705     C                   KFLD                    KNCNK1
073900960705     C                   KFLD                    KNCNK2
074000961017     C                   KFLD                    KNCTNT
074100950718     C*--------------------
074200950718     C* CAMPI DI CONTROLLO
074300950718     C*--------------------
074400950718     C                   Z-ADD     *ZEROS        C                 4 0
074500950718     C                   MOVEL     *ZEROS        KCC
074600950718     C                   MOVEL     *ZEROS        KSC
074700100629      /free
074800130909         // -?Verifica se richiamato in ricerca per filiale comm.le:
074900100629         //  ?memorizza il fatto di essere stato richiamato con?
075000100629         //  ?DXCAUT = "*C" ed imposta il valore standard per il?
075100100629         //  ?reperimento delle filiali gestibili dall'utente?
075200100629         //  ?(vedi TRUL31DS.I31ABI).?
075300100629         $Xcomm = (DXCaut = '*C');
075400100629         if  $Xcomm;
075500100713         //  clear  DXCaut;
075600130909           open  AZCMM01L;
075700100629           open  CNCLP00F;
075800100629         endif;
075900100629      /end-free
076000950718     C*
076100950718     C* ESEGUE OPERAZIONI INIZIALI STANDARD
076200950718     C                   MOVEL     'I'           $TGES             1
076300950718     C                   EXSR      INZSTD
076400950718     C*
076500950718     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
076600950718     C                   EXSR      INZSFL
076700971124     C*
076800971124     C* IMPOSTA LA DATA CORRENTE
076900971124     C                   TIME                    N14              14 0
077000971124     C                   MOVE      N14           N8                8 0
077100971124     C                   Z-ADD     N8            G08DAT
077200971124     C                   Z-ADD     *ZEROS        G08INV
077300971124     C                   MOVEL     '0'           G08ERR
077400971124     C                   CALL      'XSRDA8'
077500971124     C                   PARM                    WLBDA8
077600971124     C                   Z-ADD     G08INV        DATCOR            8 0
077700961017     C*
077800961017     C* CARICA LE TABELLE
077900961017     C                   EXSR      CARTAB
078000960705     C*
078100960705     C* CONTROLLA ESISTENZA DEL FILE NOTE DEL P.D.C.
078200960705     C                   Z-ADD     35            LUNG             15 5
078300960705     C                   MOVEL     *BLANKS       COMMAN           80
078400960705     C                   MOVEA     CMD1(1)       COMMAN
078500960705     C                   CALL      'QCMDEXC'                            98
078600960705     C                   PARM                    COMMAN
078700960705     C                   PARM                    LUNG
078800960705IF  1C     *IN98         IFEQ      *OFF
078900960705     C                   OPEN      TFNTC01L
079000960705     C                   MOVEL     '1'           $ESI              1
079100960705E   1C                   ENDIF
079200950718     C*
079300960705     C* IMPOSTAZIONI INIZIALI VIDEO
079400950718     C                   Z-ADD     DXCCAP        VCCKCC
079500950718     C*
079600130909     C* EMETTE VIDEATA SUBFILE
079700950718     C                   EXSR      CHKPAR
079800950718     C                   EXSR      CARVS
079900950718     C*
080000950718     C                   ENDSR
080100950718     C*------------------------------------------------------------------------*
080200950718     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080300950718     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080400950718     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
080500950718     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
080600950718     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
080700950718     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080800950718     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080900950718     C*------------------------------------------------------------------------*
081000950718     C* INZSTD - ROUTINE INIZIALE STANDARD
081100950718     C*------------------------------------------------------------------------*
081200950718     C     INZSTD        BEGSR
081300950718     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
081400950718     C                   CALL      'XKEYS'
081500950718     C                   PARM                    DSKEYS
081600950718     C                   MOVEA     KEYRIG        KEYS
081700950718     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
081800950718     C     XMSGPA        PLIST
081900950718     C                   PARM                    DSMSMS
082000950718     C                   PARM                    DSMSPG
082100950718     C                   PARM                    DSMSKE
082200950718     C                   PARM                    DSMSER
082300950718     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
082400950718     C                   MOVEL     'XCLIR  '     DSMSPG
082500950718     C                   MOVEL     'XCLIR  '     VMCQUE
082600950718     C                   MOVEL     *BLANKS       VMCKEY
082700950718     C                   MOVEL     *BLANKS       DSMSMS
082800950718     C* INIZIALIZZO FLAG CONTROLLO PGM
082900950718     C                   MOVEL     '0'           $ERR              1
083000950718     C                   MOVEL     '0'           $F03              1
083100950718     C                   MOVEL     *BLANKS       $GES              2
083200950718     C                   MOVEL     *BLANKS       $MSG             80
083300950718     C                   MOVEL     '0'           $FINE             1
083400950718     C                   MOVEL     '0'           $TESVD            1
083500020905     C*
083600950718     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
083700950718     C                   BITOFF    '01234567'    POSMI1            1
083800950718     C                   BITOFF    '01234567'    POSMI2            1
083900950718     C                   BITOFF    '01234567'    POSMA1            1
084000950718     C                   BITOFF    '01234567'    POSMA2            1
084100950718     C                   BITON     '34'          POSMI1            1
084200950718     C                   BITON     '7'           POSMI2            1
084300950718     C                   BITON     '34'          POSMA1            1
084400950718     C                   BITON     '0'           POSMA2            1
084500950718     C                   MOVE      POSMI2        POSMIN            2
084600950718     C                   MOVEL     POSMI1        POSMIN
084700950718     C                   MOVE      POSMA2        POSMAX            2
084800950718     C                   MOVEL     POSMA1        POSMAX
084900950718     C                   ENDSR
085000950718     C*------------------------------------------------------------------------*
085100950718     C* FUNSTD - LANCIA SUBROUTINE STANDARD
085200950718     C*------------------------------------------------------------------------*
085300950718     C     FUNSTD        BEGSR
085400950718     C                   SELECT
085500950718     C     KEY           WHENEQ    F3
085600950718     C                   EXSR      F03STD
085700950718     C     KEY           WHENEQ    F12
085800950718     C                   EXSR      F12STD
085900950718     C     KEY           WHENEQ    F24
086000950718     C                   EXSR      F24STD
086100950718     C                   OTHER
086200950718     C                   MOVEL     MSG(2)        $MSG
086300950718     C                   EXSR      CARMSG
086400950718     C                   ENDSL
086500950718     C                   ENDSR
086600950718     C*------------------------------------------------------------------------*
086700950718     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
086800950718     C*------------------------------------------------------------------------*
086900950718     C     F03STD        BEGSR
087000950718     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
087100950718     C                   MOVEL     '1'           $F03
087200950718     C* IMPOSTO FLAG DI FINE PROGRAMMA
087300950718     C                   MOVEL     '1'           $FINE
087400950718     C                   ENDSR
087500950718     C*------------------------------------------------------------------------*
087600950718     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
087700950718     C*------------------------------------------------------------------------*
087800950718     C     F12STD        BEGSR
087900950718     C* IMPOSTO FLAG DI FIN PGM
088000950718     C                   MOVEL     '1'           $FINE
088100950718     C                   ENDSR
088200950718     C*------------------------------------------------------------------------*
088300950718     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
088400950718     C*------------------------------------------------------------------------*
088500950718     C     F24STD        BEGSR
088600950718     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
088700950718     C                   EXSR      OUTFUN
088800950718     C                   ENDSR
088900950718     C*------------------------------------------------------------------------*
089000950718     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
089100950718     C*------------------------------------------------------------------------*
089200950718     C     INZSFL        BEGSR
089300950718     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
089400950718     C     *LIKE         DEFINE    VCCNBR        SFLPAG
089500950718     C     *LIKE         DEFINE    VCCNBR        SFLEOF
089600950718     C     *LIKE         DEFINE    VCCNBR        SFLERR
089700950718     C     *LIKE         DEFINE    VCCNBR        SFLULT
089800950718     C     *LIKE         DEFINE    VCCNBR        WFLULT
089900950718     C     *LIKE         DEFINE    VCCNBR        SFLRIG
090000950718     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
090100950718     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
090200950718     C                   Z-ADD     12            SFLPAG
090300950718     C                   ENDSR
090400950718     C*------------------------------------------------------------------------*
090500950718     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
090600950718     C*------------------------------------------------------------------------*
090700950718     C     FUNSFL        BEGSR
090800950718     C                   SELECT
090900950718     C     KEY           WHENEQ    ENTER
091000950718     C                   EXSR      ENTSFL
091100950718     C     KEY           WHENEQ    F3
091200950718     C                   EXSR      F03STD
091300950718     C     KEY           WHENEQ    F5
091400950718     C                   EXSR      F05STD
091500950718     C     KEY           WHENEQ    F12
091600950718     C                   EXSR      F12STD
091700950718     C     KEY           WHENEQ    F17
091800950718     C                   EXSR      F17STD
091900950718     C     KEY           WHENEQ    F18
092000950718     C                   EXSR      F18STD
092100950718     C     KEY           WHENEQ    F23
092200950718     C                   EXSR      F23STD
092300950718     C     KEY           WHENEQ    F24
092400950718     C                   EXSR      F24STD
092500950718     C     KEY           WHENEQ    ROLLUP
092600950718     C                   EXSR      RUPSTD
092700950718     C     KEY           WHENEQ    ROLLDN
092800950718     C                   EXSR      RDNSTD
092900950718     C                   OTHER
093000950718     C                   MOVEL     MSG(2)        $MSG
093100950718     C                   EXSR      CARMSG
093200950718     C                   ENDSL
093300950718     C                   ENDSR
093400950718     C*------------------------------------------------------------------------*
093500950718     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
093600950718     C*------------------------------------------------------------------------*
093700950718     C     F05STD        BEGSR
093800950718     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
093900950718     C                   EXSR      POSSFL
094000950718     C                   ENDSR
094100950718     C*------------------------------------------------------------------------*
094200950718     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
094300950718     C*------------------------------------------------------------------------*
094400950718     C     F17STD        BEGSR
094500950718     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
094600950718     C                   Z-ADD     1             VCCNBR
094700950718     C                   ENDSR
094800950718     C*------------------------------------------------------------------------*
094900950718     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
095000950718     C*------------------------------------------------------------------------*
095100950718     C     F18STD        BEGSR
095200950718     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
095300950718     C                   Z-ADD     SFLULT        VCCNBR
095400950718     C                   ENDSR
095500950718     C*------------------------------------------------------------------------*
095600950718     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
095700950718     C*------------------------------------------------------------------------*
095800950718     C     F23STD        BEGSR
095900950718     C* GESTIONE VISUALIZZAZIONE OPZIONI
096000950718     C                   EXSR      OUTOPZ
096100950718     C                   ENDSR
096200950718     C*------------------------------------------------------------------------*
096300950718     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
096400950718     C*------------------------------------------------------------------------*
096500950718     C     RUPSTD        BEGSR
096600950718     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
096700950718     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
096800950718     C     CURSOR        IFGE      POSMIN
096900950718     C     CURSOR        ANDLE     POSMAX
097000950718     C                   ELSE
097100950718     C                   EXSR      CARSFL
097200950718     C                   ENDIF
097300950718     C                   ENDSR
097400950718     C*------------------------------------------------------------------------*
097500950718     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
097600950718     C*------------------------------------------------------------------------*
097700950718     C     RDNSTD        BEGSR
097800950718     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
097900950718     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
098000950718     C     CURSOR        IFGE      POSMIN
098100950718     C     CURSOR        ANDLE     POSMAX
098200950718     C                   ELSE
098300950718     C* INVIO MESSAGGIO DI INIZIO ELENCO
098400950718     C                   MOVEL     '1'           $ERR
098500950718     C                   MOVEL     MSG(5)        $MSG
098600950718     C                   EXSR      CARMSG
098700950718     C                   ENDIF
098800950718     C                   ENDSR
098900950718     C*------------------------------------------------------------------------*
099000950718     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
099100950718     C*------------------------------------------------------------------------*
099200950718     C     ENTSFL        BEGSR
099300950718     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
099400950718     C                   EXSR      NEWPOS
099500950718     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
099600950718     C     $ERR          IFEQ      '0'
099700950718     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
099800950718     C     SWPOSI        IFNE      0
099900950718     C                   EXSR      POSSFL
100000950718     C                   ELSE
100100950718     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
100200950718     C     SFLULT        IFNE      0
100300950718     C                   EXSR      OPZSFL
100400950718     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
100500950718     C     SWMODI        IFNE      0
100600950718     C                   EXSR      POSSFL
100700950718     C                   ENDIF
100800950718     C                   ENDIF
100900950718     C                   ENDIF
101000950718     C                   ENDIF
101100950718     C                   ENDSR
101200950718     C*------------------------------------------------------------------------*
101300950718     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
101400950718     C*------------------------------------------------------------------------*
101500950718     C     POSSFL        BEGSR
101600950718     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
101700950718     C                   EXSR      CLRSFL
101800950718     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
101900950718     C                   EXSR      SETFIL
102000950718     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
102100950718     C     SFLEOF        IFEQ      0
102200950718     C                   EXSR      CARSFL
102300950718     C                   ENDIF
102400950718     C                   ENDSR
102500950718     C*------------------------------------------------------------------------*
102600950718     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
102700950718     C*------------------------------------------------------------------------*
102800950718     C     OPZSFL        BEGSR
102900950718     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
103000950718     C                   MOVEL     'S'           OPZOK             1
103100950718     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
103200950718     C                   Z-ADD     0             SFLVAR            5 0
103300950718     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
103400950718     C                   Z-ADD     0             SFLERR
103500950718     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
103600950718     C                   Z-ADD     SFLULT        WFLULT
103700950718     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
103800950718     C                   READC     XCLIDVS                                49
103900950718     C     *IN49         DOWEQ     '0'
104000950718     C     $ERR          ANDEQ     '0'
104100950718     C* ADD CONTATORE DI RIGHE SFL VARIATE
104200950718     C                   ADD       1             SFLVAR
104300950718     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
104400950718     C     OPZOK         IFEQ      'S'
104500950718     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
104600950718     C     VSCOPZ        IFNE      *BLANKS
104700950718     C                   EXSR      CONOPZ
104800950718     C     OPZOK         IFEQ      'S'
104900950718     C     $ERR          ANDEQ     '0'
105000950718     C                   EXSR      GESOPZ
105100950718     C                   ENDIF
105200950718     C                   ENDIF
105300950718     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
105400950718     C*            2. PULIZIA SCELTA IN SFL
105500950718     C     OPZOK         IFEQ      'S'
105600950718     C                   MOVEL     '0'           *IN41
105700950718     C                   MOVEL     *BLANKS       VSCOPZ
105800950718     C                   ELSE
105900950718     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
106000950718     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
106100950718     C                   MOVEL     '1'           *IN41
106200950718     C                   Z-ADD     SFLULT        SFLERR
106300950718     C                   ENDIF
106400950718     C                   ELSE
106500950718     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
106600950718     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
106700950718     C                   MOVEL     '1'           *IN41
106800950718     C                   ENDIF
106900950718     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
107000950718     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
107100950718     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
107200950718     C     OPZOK         IFNE      'S'
107300950718     C                   Z-ADD     SFLERR        VCCNBR
107400950718     C                   ELSE
107500950718     C                   Z-ADD     SFLULT        VCCNBR
107600950718     C                   ENDIF
107700950718     C* AGGIORNO RECORD SFL
107800950718     C                   UPDATE    XCLIDVS
107900950718     C     $ERR          IFEQ      '0'
108000950718     C                   READC     XCLIDVS                                49
108100950718     C                   ENDIF
108200950718     C                   ENDDO
108300950718     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
108400950718     C                   Z-ADD     WFLULT        SFLULT
108500950718     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
108600950718     C     SFLVAR        IFEQ      0
108700950718     C                   MOVEL     '1'           $FINE
108800950718     C                   ENDIF
108900950718     C                   ENDSR
109000950718     C*------------------------------------------------------------------------*
109100950718     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
109200950718     C*------------------------------------------------------------------------*
109300950718     C     CLRSFL        BEGSR
109400950718     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
109500950718     C* (0=NON FINE FILE  1=FINE FILE)
109600950718     C                   Z-ADD     0             SFLEOF
109700950718     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
109800950718     C                   Z-ADD     0             SFLULT
109900950718     C* ACCENSIONE INDICATORE PER CLEAR SFL
110000950718     C                   MOVEL     '1'           *IN40
110100950718     C                   WRITE     XCLIDVC
110200950718     C                   MOVEL     '0'           *IN40
110300950718     C                   ENDSR
110400950718     C*------------------------------------------------------------------------*
110500950718     C* CARSFL - CARICAMENTO SFL
110600950718     C*------------------------------------------------------------------------*
110700950718     C     CARSFL        BEGSR
110800950718     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
110900950718     C     SFLULT        IFEQ      0
111000950718     C                   EXSR      SETFIL
111100950718     C                   ENDIF
111200950718     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
111300950718     C                   Z-ADD     SFLULT        WFLULT
111400950718     C* CICLO:
111500950718     C*    -CARICO NUMERO DI RIGHE=SFLPAG
111600950718     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
111700950718     C                   Z-ADD     1             SFLRIG
111800950718     C     SFLRIG        DOWLE     SFLPAG
111900950718     C     SFLEOF        ANDEQ     0
112000950718     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
112100950718     C                   MOVEL     *BLANKS       VSCOPZ
112200950718     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
112300950718     C                   EXSR      IRESFL
112400950718     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
112500950718     C                   ADD       1             SFLULT
112600950718     C* SCRITTURA RECORD SFL
112700950718     C                   WRITE     XCLIDVS
112800950718     C                   ADD       1             SFLRIG
112900950718     C* LETTURA FILE DA CARICARE
113000950718     C                   EXSR      LETFIL
113100950718     C                   ENDDO
113200950718     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
113300950718     C     WFLULT        IFNE      SFLULT
113400950718     C                   Z-ADD     WFLULT        VCCNBR
113500950718     C                   ADD       1             VCCNBR
113600950718     C                   ENDIF
113700950718     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
113800950718     C     SFLEOF        IFEQ      1
113900950718     C     SFLULT        ANDGT     *ZEROS
114000950718     C                   MOVEL     '1'           $ERR
114100950718     C                   MOVEL     MSG(6)        $MSG
114200950718     C                   EXSR      CARMSG
114300950718     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
114400950718     C     SFLULT        CHAIN     XCLIDVS                            99
114500950718     C     *IN99         IFEQ      '0'
114600950718     C                   SETON                                        45
114700950718     C                   UPDATE    XCLIDVS
114800950718     C                   SETOFF                                       45
114900950718     C                   ENDIF
115000950718     C                   ENDIF
115100950718     C                   ENDSR
115200950718     C*------------------------------------------------------------------------*
115300950718     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
115400950718     C*------------------------------------------------------------------------*
115500950718     C     OUTSFL        BEGSR
115600950718     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
115700950718     C     MSGULT        IFNE      *ZEROS
115800950718     C                   EXSR      OUTMSG
115900950718     C                   ENDIF
116000950718     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
116100950718     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
116200950718     C     SFLEOF        IFEQ      0
116300950718     C                   MOVEL     '0'           *IN42
116400950718     C                   ELSE
116500950718     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
116600950718     C                   MOVEL     '1'           *IN42
116700950718     C                   ENDIF
116800950718     C* EMISSIONE SUBFILE
116900950718     C     SFLULT        IFLE      0
117000950718     C*  --> CASO DI SFL VUOTO
117100950718     C                   MOVEL     '0'           *IN43
117200950718     C                   WRITE     XCLIDVV
117300950718     C                   WRITE     XCLIDVC
117400950718     C                   READ      XCLIDVC                                99
117500950718     C                   ELSE
117600950718     C*  --> CASO DI SFL CON RECORDS
117700950718     C                   WRITE     XCLIDVC
117800950718     C                   READ      XCLIDVC                                99
117900950718     C                   ENDIF
118000950718     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
118100950718     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
118200950718     C                   Z-ADD     FRTSFL        VCCNBR
118300950718     C                   ENDSR
118400950718     C*------------------------------------------------------------------------*
118500950718     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
118600950718     C*          CODA MESSAGGI DEL PGM
118700950718     C*------------------------------------------------------------------------*
118800950718     C     CARMSG        BEGSR
118900950718     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
119000950718     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
119100950718     C     MSGULT        IFEQ      0
119200950718     C                   MOVEL     '1'           *IN40
119300950718     C                   WRITE     XCLIDVK
119400950718     C                   MOVEL     '0'           *IN40
119500950718     C                   ENDIF
119600950718     C* INVIA MESSAGGIO ALLA CODA DEL PGM
119700950718     C                   MOVEL     $MSG          DSMSMS
119800950718     C                   CALL      'XMSG'        XMSGPA
119900950718     C     DSMSER        IFEQ      0
120000950718     C                   MOVEL     DSMSKE        VMCKEY
120100950718     C                   ENDIF
120200950718     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
120300950718     C                   ADD       1             MSGULT            4 0
120400950718     C                   WRITE     XCLIDVM
120500950718     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
120600950718     C                   MOVEL     *BLANKS       $MSG
120700950718     C                   ENDSR
120800950718     C*------------------------------------------------------------------------*
120900950718     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
121000950718     C*------------------------------------------------------------------------*
121100950718     C     OUTMSG        BEGSR
121200950718     C* VISUALIZZAZIONE MESSAGGIO
121300950718     C                   MOVEL     '1'           *IN44
121400950718     C                   WRITE     XCLIDVK
121500950718     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
121600950718     C                   Z-ADD     *ZEROS        MSGULT
121700950718     C                   ENDSR
121800950718     C*------------------------------------------------------------------------*
121900950718     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
122000950718     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
122100950718     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
122200950718     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
122300950718     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
122400950718     C*------------------------------------------------------------------------*
122500950718     C     CARFUN        BEGSR
122600950718     C                   Z-ADD     *ZEROS        FUNULT            5 0
122700950718     C     1             DO        31            I                 5 0
122800950718     C                   MOVEL     FUN(I)        FUNRIG
122900950718     C                   MOVEL     FUNFUN        FFU(I)
123000950718     C                   MOVEL     FUNDES        FDE(I)
123100020905     C     DUTLPO        IFEQ      'S'
123200950718     C     $TGES         IFEQ      'G'
123300950718     C                   MOVEL     FUNASG        FAT(I)
123400950718     C                   ELSE
123500950718     C                   MOVEL     FUNASI        FAT(I)
123600950718     C                   ENDIF
123700950718     C                   ELSE
123800950718     C     $TGES         IFEQ      'G'
123900950718     C                   MOVEL     FUNAFG        FAT(I)
124000950718     C                   ELSE
124100950718     C                   MOVEL     FUNAFI        FAT(I)
124200950718     C                   ENDIF
124300950718     C                   ENDIF
124400950718     C     FAT(I)        IFEQ      'E'
124500950718     C                   MOVEL     USER          WEDP              3
124600950718     C     WEDP          IFEQ      'EDP'
124700950718     C                   MOVEL     'S'           FAT(I)
124800950718     C                   ELSE
124900950718     C                   MOVEL     'N'           FAT(I)
125000950718     C                   ENDIF
125100950718     C                   ENDIF
125200950718     C                   ENDDO
125300950718     C                   ENDSR
125400950718     C*------------------------------------------------------------------------*
125500950718     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
125600950718     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
125700950718     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
125800950718     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
125900950718     C*------------------------------------------------------------------------*
126000950718     C     OUTFUN        BEGSR
126100950718     C     1             DO        8             I
126200950718     C                   MOVEL     *BLANKS       FUV(I)
126300950718     C                   ENDDO
126400950718     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
126500950718     C                   Z-ADD     *ZEROS        FUNNAT            2 0
126600950718     C     1             DO        31            I
126700950718     C     FAT(I)        IFEQ      'S'
126800950718     C     FDE(I)        ANDNE     *BLANKS
126900950718     C                   ADD       1             FUNNAT
127000950718     C                   ENDIF
127100950718     C                   ENDDO
127200950718     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
127300950718     C     FUNNAT        IFGT      8
127400950718     C                   MOVEL     'S'           FAT(24)
127500950718     C                   ENDIF
127600950718     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
127700950718     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
127800950718     C                   Z-ADD     8             FUNVIS            5 0
127900950718     C     FAT(23)       IFEQ      'S'
128000950718     C                   SUB       1             FUNVIS
128100950718     C                   ENDIF
128200950718     C     FAT(24)       IFEQ      'S'
128300950718     C                   SUB       1             FUNVIS
128400950718     C                   ENDIF
128500950718     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
128600950718     C                   Z-ADD     *ZEROS        W                 5 0
128700950718     C                   ADD       1             FUNULT
128800950718     C     FUNULT        IFGT      31
128900950718     C                   Z-ADD     1             FUNULT
129000950718     C                   ENDIF
129100950718     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
129200950718     C     FUNULT        DO        31            I
129300950718     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
129400950718     C     W             IFLT      FUNVIS
129500950718     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
129600950718     C     FAT(I)        IFEQ      'S'
129700950718     C     FDE(I)        ANDNE     *BLANKS
129800950718     C     I             IFEQ      23
129900950718     C     I             OREQ      24
130000950718     C                   ELSE
130100950718     C                   ADD       1             W
130200950718     C                   MOVEL     FDE(I)        FUV(W)
130300950718     C                   ENDIF
130400950718     C                   ENDIF
130500950718     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
130600950718     C                   Z-ADD     I             FUNULT
130700950718     C                   ENDIF
130800950718     C                   ENDDO
130900950718     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
131000950718     C                   MOVEL     FUV(1)        VFD001
131100950718     C                   MOVEL     FUV(2)        VFD002
131200950718     C                   MOVEL     FUV(3)        VFD003
131300950718     C                   MOVEL     FUV(4)        VFD004
131400950718     C                   MOVEL     FUV(5)        VFD005
131500950718     C                   MOVEL     FUV(6)        VFD006
131600950718     C     FAT(23)       IFEQ      'N'
131700950718     C     FAT(24)       ANDEQ     'N'
131800950718     C                   MOVEL     FUV(7)        VFD007
131900950718     C                   MOVEL     FUV(8)        VFD008
132000950718     C                   ELSE
132100950718     C     FAT(23)       IFEQ      'S'
132200950718     C     FAT(24)       ANDEQ     'S'
132300950718     C                   MOVEL     FDE(23)       VFD007
132400950718     C                   MOVEL     FDE(24)       VFD008
132500950718     C                   ELSE
132600950718     C                   MOVEL     FUV(7)        VFD007
132700950718     C     FAT(23)       IFEQ      'S'
132800950718     C                   MOVEL     FDE(23)       VFD008
132900950718     C                   ELSE
133000950718     C                   MOVEL     FDE(24)       VFD008
133100950718     C                   ENDIF
133200950718     C                   ENDIF
133300950718     C                   ENDIF
133400950718     C                   MOVEL     '1'           $TESVD
133500950718     C                   ENDSR
133600950718     C*------------------------------------------------------------------------*
133700950718     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
133800950718     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
133900950718     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
134000950718     C*                   - ERRORE SE FUNZIONE NON ATTIVA
134100950718     C*------------------------------------------------------------------------*
134200950718     C     CONFUN        BEGSR
134300950718     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
134400950718     C     1             DO        31            I
134500950718     C     KEY           IFEQ      KEYS(I)
134600950718     C* CONTROLLO SE IL TASTO E' ATTIVO
134700950718     C     FAT(I)        IFNE      'S'
134800950718     C                   MOVEL     MSG(1)        $MSG
134900950718     C                   EXSR      CARMSG
135000950718     C                   MOVEL     '1'           $ERR
135100950718     C                   ENDIF
135200950718     C                   ENDIF
135300950718     C                   ENDDO
135400950718     C                   ENDSR
135500950718     C*------------------------------------------------------------------------*
135600950718     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
135700950718     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
135800950718     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
135900950718     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
136000950718     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
136100950718     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
136200950718     C*------------------------------------------------------------------------*
136300950718     C     CAROPZ        BEGSR
136400950718     C                   Z-ADD     *ZEROS        OPZULT            5 0
136500950718     C     1             DO        24            I
136600950718     C                   MOVEL     OPZ(I)        OPZRIG
136700950718     C                   MOVEL     OPZOPD        OPD(I)
136800950718     C                   MOVEL     OPZOPS        OPS(I)
136900950718     C                   MOVEL     OPZDES        ODE(I)
137000020905     C     DUTLPO        IFEQ      'S'
137100950718     C     $TGES         IFEQ      'G'
137200950718     C                   MOVEL     OPZASG        OAT(I)
137300950718     C                   ELSE
137400950718     C                   MOVEL     OPZASI        OAT(I)
137500950718     C                   ENDIF
137600950718     C                   ELSE
137700950718     C     $TGES         IFEQ      'G'
137800950718     C                   MOVEL     OPZAFG        OAT(I)
137900950718     C                   ELSE
138000950718     C                   MOVEL     OPZAFI        OAT(I)
138100950718     C                   ENDIF
138200950718     C                   ENDIF
138300950718     C     OAT(I)        IFEQ      'E'
138400950718     C                   MOVEL     USER          WEDP              3
138500950718     C     WEDP          IFEQ      'EDP'
138600950718     C                   MOVEL     'S'           OAT(I)
138700950718     C                   ELSE
138800950718     C                   MOVEL     'N'           OAT(I)
138900950718     C                   ENDIF
139000950718     C                   ENDIF
139100950718     C                   ENDDO
139200950718     C                   ENDSR
139300950718     C*------------------------------------------------------------------------*
139400950718     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
139500950718     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
139600950718     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
139700950718     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
139800950718     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
139900950718     C*------------------------------------------------------------------------*
140000950718     C     OUTOPZ        BEGSR
140100950718     C     1             DO        8             I
140200950718     C                   MOVEL     *BLANKS       OPV(I)
140300950718     C                   ENDDO
140400950718     C                   Z-ADD     *ZEROS        OPZNAT            2 0
140500950718     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
140600950718     C                   ADD       1             OPZULT
140700950718     C     OPZULT        IFGT      24
140800950718     C                   Z-ADD     1             OPZULT
140900950718     C                   END
141000950718     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
141100950718     C     1             DO        24            I
141200950718     C     OAT(I)        IFEQ      'S'
141300950718     C     ODE(I)        ANDNE     *BLANKS
141400950718     C                   ADD       1             OPZNAT
141500950718     C                   ENDIF
141600950718     C                   ENDDO
141700950718     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
141800950718     C                   Z-ADD     0             W
141900950718     C     OPZULT        DO        24            I
142000950718     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
142100950718     C     W             IFLT      8
142200950718     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
142300950718     C     OAT(I)        IFEQ      'S'
142400950718     C     ODE(I)        ANDNE     *BLANKS
142500950718     C                   ADD       1             W
142600950718     C                   MOVEL     ODE(I)        OPV(W)
142700950718     C                   ENDIF
142800950718     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
142900950718     C                   Z-ADD     I             OPZULT
143000950718     C                   ENDIF
143100950718     C                   ENDDO
143200950718     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
143300950718     C                   MOVEL     OPV(1)        VOD001
143400950718     C                   MOVEL     OPV(2)        VOD002
143500950718     C                   MOVEL     OPV(3)        VOD003
143600950718     C                   MOVEL     OPV(4)        VOD004
143700950718     C                   MOVEL     OPV(5)        VOD005
143800950718     C                   MOVEL     OPV(6)        VOD006
143900950718     C                   MOVEL     OPV(7)        VOD007
144000950718     C                   MOVEL     OPV(8)        VOD008
144100950718     C                   MOVEL     '1'           $TESVD
144200950718     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
144300950718     C     OPZNAT        IFGT      8
144400950718     C                   MOVEL     'S'           FAT(23)
144500950718     C                   ENDIF
144600950718     C                   ENDSR
144700950718     C*------------------------------------------------------------------------*
144800950718     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
144900950718     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
145000950718     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
145100950718     C*                   - ERRORE SE OPZIONE NON ATTIVA
145200950718     C*------------------------------------------------------------------------*
145300950718     C     CONOPZ        BEGSR
145400950718     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
145500950718     C                   MOVEL     *BLANKS       OPZOK             1
145600950718     C     1             DO        24            I                 5 0
145700950718     C     VSCOPZ        IFEQ      OPD(I)
145800950718     C     VSCOPZ        OREQ      OPS(I)
145900950718     C     OAT(I)        IFEQ      'S'
146000950718     C                   MOVEL     OPD(I)        VSCOPZ
146100950718     C                   MOVEL     'S'           OPZOK
146200950718     C                   ELSE
146300950718     C                   MOVEL     'N'           OPZOK             1
146400950718     C                   ENDIF
146500950718     C                   ENDIF
146600950718     C                   ENDDO
146700950718     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
146800950718     C     OPZOK         IFNE      'S'
146900950718     C                   MOVEL     MSG(3)        $MSG
147000950718     C                   EXSR      CARMSG
147100950718     C                   MOVEL     '1'           $ERR
147200950718     C                   MOVEL     'N'           OPZOK             1
147300950718     C                   ENDIF
147400950718     C                   ENDSR
147500960705** ======== SCHIERA: MSG  (MESSAGGI)                       ================
147600950718XCLIR  - Tasto funzionale non valido.                                          1
147700950718XCLIR  - Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
147800950718XCLIR  - Opzione non valida.                                                   3
147900950718XCLIR  - Opzione abilitata ma non gestita. Avvertire CED.                      4
148000950718XCLIR  - Inizio elenco.                                                        5
148100950718XCLIR  - Fine elenco.                                                          6
148200950718XCLIR  - Accettato solo un parametro di inizio elenco.                         7
148300950718XCLIR  - 8
148400950718XCLIR  - 9
148500950718XCLIR  - 10
148600950718XCLIR  - 11
148700950718XCLIR  - 12
148800950718XCLIR  - 13
148900950718XCLIR  - 14
149000950718XCLIR  - 15
149100950718XCLIR  - 16
149200950718XCLIR  - 17
149300950718XCLIR  - 18
149400950718XCLIR  - 19
149500950718XCLIR  - 20
149600950718** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
149700950718- 1-1 -1=Scelta           -    SS     SS
149800950718- 2-2 -                   -    NN     NN
149900950718- 3-3 -                   -    NN     NN
150000950718- 4-4 -                   -    NN     NN
150100950718- 5-5 -                   -    NN     NN
150200950718- 6-6 -                   -    NN     NN
150300950718- 7-7 -                   -    NN     NN
150400950718- 8-8 -                   -    NN     NN
150500950718- 9-9 -                   -    NN     NN
150600950718-10-  -                   -    NN     NN
150700950718-11-  -                   -    NN     NN
150800950718-12-  -                   -    NN     NN
150900950718-13-  -                   -    NN     NN
151000950718-14-  -                   -    NN     NN
151100950718-15-  -                   -    NN     NN
151200950718-16-  -                   -    NN     NN
151300950718-17-  -                   -    NN     NN
151400950718-18-  -                   -    NN     NN
151500950718-19-  -                   -    NN     NN
151600950718-20-  -                   -    NN     NN
151700950718-21-  -                   -    NN     NN
151800950718-22-  -                   -    NN     NN
151900950718-23-  -                   -    NN     NN
152000950718-24-  -                   -    NN     NN
152100950718** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
152200950718F1    -                   -    NN     NN
152300950718F2    -                   -    NN     NN
152400950718F3    -F3=Fine            -    SS     SS
152500950718F4    -                   -    NN     NN
152600950718F5    -F5=Rivisualizza    -    SS     SS
152700950718F6    -                   -    NN     NN
152800950718F7    -                   -    NN     NN
152900950718F8    -                   -    NN     NN
153000960611F9    -F9=Altri dati      -    SS     SS
153100950718F10   -                   -    NN     NN
153200950718F11   -                   -    NN     NN
153300950718F12   -F12=Ritorno        -    SS     SS
153400950718F13   -                   -    NN     NN
153500950718F14   -                   -    NN     NN
153600950718F15   -                   -    NN     NN
153700950718F16   -                   -    NN     NN
153800950718F17   -F17=Inizio dati    -    SS     SS
153900950718F18   -F18=Fine dati      -    SS     SS
154000950718F19   -                   -    NN     NN
154100950718F20   -                   -    NN     NN
154200950718F21   -                   -    NN     NN
154300950718F22   -                   -    NN     NN
154400950718F23   -F23=Altre opzioni  -    NN     NN
154500950718F24   -F24=Altri tasti    -    NN     NN
154600950718ENTER -                   -    SS     SS
154700950718ROLLUP-                   -    SS     SS
154800950718ROLLDN-                   -    SS     SS
154900950718HELP  -                   -    NN     NN
155000950718PRINT -                   -    NN     NN
155100950718BSPACE-                   -    NN     NN
155200950718CLEAR -                   -    NN     NN
155300960705** CMD1 - COMANDI CLP
155400960705CHKOBJ OBJ(TFNTC01L) OBJTYPE(*FILE)
