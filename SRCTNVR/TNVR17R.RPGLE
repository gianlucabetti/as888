000100980323     H DECEDIT('0,') DATEDIT(*DMY.)
000200000915
000300000915      *-------------------------------------------------------------*
000400000915      *                                                             *
000500000915      *   ***  INGLOBO  ***   T A R I F F E   G I A C E N Z E       *
000600000915      *                                                             *
000700000915      *-------------------------------------------------------------*
000800000915
000900990923     FTNTAM01L  IF   E           K DISK
001000000915     FTITGC01L  UF A E           K DISK
001100990923     FTABEL00F  IF   E           K DISK
001200000918     FFNCLS01L  IF   E           K DISK
001300000918     FTNVR17P   O    E             PRINTER OFLIND(*IN55)
001400000915
001500980323     D L1              S              3  0 DIM(30)                              FIL.GESTITE
001600000918     D KPJBA         E DS                  INZ
001700000918     D DSTA01        E DS
001800000918     D DSCV          E DS
001900000918     D YEUR          E DS                  EXTNAME(YEURCODS)
002000980323     D CNCR80        E DS
002100980323     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200980323     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002300980323     D  LIN                    1     90  0
002400000915     D                                     DIM(30)                              FIL.COMODO
002500000915
002600000915      *-------------------------------------------------------------*
002700000915      *                                                             *
002800000915      *   OPERAZIONI INIZIALI                                       *
002900000915      *                                                             *
003000000915      *-------------------------------------------------------------*
003100000918
003200000918      *   Carico L1
003300000918
003400000918     C                   Z-ADD     1             CODUT             1 0
003500000918     C                   CALL      'X§PARUT'
003600000918     C                   PARM                    UT§DSE
003700000918     C                   MOVEL     REC80         CNCR80
003800000918     C                   MOVEL     RAGUT         RSUT
003900000918
004000000918     C                   CLEAR                   DSUL06
004100000918     C                   MOVE      '£1'          D06COD
004200000918     C                   MOVEL     SIMFEL        D06KEY
004300000918     C                   MOVEL     DSUL06        KPJBU
004400000918     C                   CALL      'TRUL06R'
004500000918     C                   PARM                    KPJBA
004600000918     C                   MOVEL     KPJBU         DSUL06
004700000918     C                   MOVEA     LIN           L1
004800000915
004900000918     C                   write     testa
005000000918     C                   setoff                                       55
005100000918
005200000915      *   Definisco il TITGC di cartello
005300000915
005400000915     C     *like         define    tgcatb        tgcatbCartello
005500000915     C     *like         define    tgcksc        tgckscCartello
005600000915     C     *like         define    tgcctr        tgcctrCartello
005700000915     C     *like         define    tgcprg        tgcprgCartello
005800000915     C     *like         define    tgcsgv        tgcsgvCartello
005900000915     C     *like         define    tgcsgr        tgcsgrCartello
006000000915     C     *like         define    tgcsgp        tgcsgpCartello
006100000915     C     *like         define    tgcsgd        tgcsgdCartello
006200000915     C     *like         define    tgcsg1        tgcsg1Cartello
006300000915     C     *like         define    tgcsg2        tgcsg2Cartello
006400000915     C     *like         define    tgcsg3        tgcsg3Cartello
006500000915     C     *like         define    tgcst1        tgcst1Cartello
006600000915     C     *like         define    tgcst2        tgcst2Cartello
006700000915     C     *like         define    tgcst3        tgcst3Cartello
006800000915     C     *like         define    tgcstm        tgcstmCartello
006900000915     C     *like         define    tgcrpv        tgcrpvCartello
007000000915     C     *like         define    tgcfaf        tgcfafCartello
007100000915     C     *like         define    tgcsgf        tgcsgfCartello
007200000915     C     *like         define    tgcggr        tgcggrCartello
007300000915     C     *like         define    tgctcm        tgctcmCartello
007400000915     C     *like         define    tgctfg        tgctfgCartello
007500000915     C     *like         define    tgcduv        tgcduvCartello
007600000915     C     *like         define    tgcftr        tgcftrCartello
007700000915     C     *like         define    tgcdtr        tgcdtrCartello
007800000915
007900000915     C     *like         define    tgcksc        keyksc
008000000915     C     *like         define    tgcctr        keyctr
008100000915     C     *like         define    tgcprg        keyprg
008200000918
008300000918     C     *like         define    tblcod        cod
008400000918     C     *like         define    tblkey        key
008500000915
008600000915      *   Accesso al Data Base
008700000915
008800000915     C     KeyTariffe    KLIST
008900000915     C                   KFLD                    Keyksc
009000000915     C                   KFLD                    Keyctr
009100000915     C                   KFLD                    Keyprg
009200000918
009300000918     C     KeyTabelle    KLIST
009400000918     C                   KFLD                    codut
009500000918     C                   KFLD                    cod
009600000918     C                   KFLD                    key
009700000915
009800000915      *   Ripresa dati tariffa di controllo
009900000915
010000000915     C                   z-add     8888830       keyksc
010100000915     C                   z-add     0             keyctr
010200000915     C                   z-add     5             keyprg
010300000915
010400000915     C     KeyTariffe    chain     titgc01l
010500000915     C                   if        %found(titgc01l)
010600000915     C                   if        tgcatb = ' '
010700000915
010800000915      *                  ==================
010900000915     C                   exsr      SRPIGLIA
011000000915      *                  ==================
011100000915
011200000915     C                   endif
011300000915     C                   endif
011400000915
011500000915      *-------------------------------------------------------------*
011600000915      *                                                             *
011700000915      *   ROUTINE PRINCIPALE                                        *
011800000915      *                                                             *
011900000915      *-------------------------------------------------------------*
012000000915
012100000915      *   Se sono riuscito a reperire la tariffa di cartello procedo
012200000915      *   all'inglobo nei clienti che non la hanno.
012300000915      *   *IN01 = *ON
012400000915
012500000915     C     *in01         ifeq      *on
012600000915     C     *loval        setll     tntam01l
012700000915
012800000915      *   Leggo il tariffario clienti
012900000915
013000000915     C                   do        *hival
013100000915     C                   read      tntam01l
013200000915
013300000915     C                   if        %eof(tntam01l)
013400000915     C                   leave
013500000915     C                   endif
013600000915
013700000915      *   Escludo gli 888
013800000915
013900000915     C                   movel     tamksc        W3alfanumerico    3
014000000915     C     W3alfanumericoifeq      '888'
014100000915     C                   iter
014200000915     C                   endif
014300000915
014400000915      *   Controllo se presente tariffa giacenza
014500000915
014600000915     C                   z-add     tamksc        keyksc
014700000915     C                   z-add     tamctr        keyctr
014800000918     C                   z-add     tamprg        keyprg
014900000918     C     KeyTariffe    chain     titgc01l
015000000915     C                   if        %found(titgc01l)
015100000915     C     tgcatb        ifne      *blanks
015200000915
015300000915      *   Se ho trovato tariffa ma annullata la aggiorno
015400000915      *   con i dati di quella cartello
015500000915
015600000915     C                   setoff                                       03        update
015700000915
015800000915      *                  ===================
015900000915     C                   exsr      SRINGLOBA
016000000915      *                  ===================
016100000915
016200000915     C                   endif
016300000915     C                   else
016400000915
016500000915      *   Se non ho trovato tariffa la scrivo
016600000915      *   con i dati di quella cartello
016700000915
016800000915     C                   seton                                        03        write
016900000915
017000000915      *                  ===================
017100000915     C                   exsr      SRINGLOBA
017200000915      *                  ===================
017300000915
017400000915     C                   endif
017500000915
017600000915
017700000915     C                   enddo
017800000915
017900000915     C                   endif
018000000915
018100000915      *   Se non sono riuscito a reperire la tariffa di cartello
018200000915      *   stampo errore
018300000915      *   *IN01 = *OFF
018400000915
018500000915      *                  ==================
018600000915     C  N01              exsr      SRERRORE
018700000915      *                  ==================
018800000915
018900000918     C                   write     finest
019000000918
019100000915     C                   eval      *inlr = *on
019200000915
019300000915      *-------------------------------------------------------------*
019400000915      *                                                             *
019500000915      *   SUBROUTINES                                               *
019600000915      *                                                             *
019700000915      *-------------------------------------------------------------*
019800000915
019900000915      *-------------------------------------------------------------*
020000000915      *   SRPIGLIA - Subroutine ripresa dati tariffa di cartello    *
020100000915      *-------------------------------------------------------------*
020200000915
020300000915     C     SRPIGLIA      begsr
020400000915     C                   seton                                        01
020500000915     C                   move      tgcatb        tgcatbCartello
020600000915     C                   move      tgcksc        tgckscCartello
020700000915     C                   move      tgcctr        tgcctrCartello
020800000915     C                   move      tgcprg        tgcprgCartello
020900000915     C                   move      tgcsgv        tgcsgvCartello
021000000915     C                   move      tgcsgr        tgcsgrCartello
021100000915     C                   move      tgcsgp        tgcsgpCartello
021200000915     C                   move      tgcsgd        tgcsgdCartello
021300000915     C                   move      tgcsg1        tgcsg1Cartello
021400000915     C                   move      tgcsg2        tgcsg2Cartello
021500000915     C                   move      tgcsg3        tgcsg3Cartello
021600000915     C                   move      tgcst1        tgcst1Cartello
021700000915     C                   move      tgcst2        tgcst2Cartello
021800000915     C                   move      tgcst3        tgcst3Cartello
021900000915     C                   move      tgcstm        tgcstmCartello
022000000915     C                   move      tgcrpv        tgcrpvCartello
022100000915     C                   move      tgcfaf        tgcfafCartello
022200000915     C                   move      tgcsgf        tgcsgfCartello
022300000915     C                   move      tgcggr        tgcggrCartello
022400000915     C                   move      tgctcm        tgctcmCartello
022500000915     C                   move      tgctfg        tgctfgCartello
022600000915     C                   move      tgcduv        tgcduvCartello
022700000915     C                   move      tgcftr        tgcftrCartello
022800000915     C                   move      tgcdtr        tgcdtrCartello
022900000915     C                   endsr
023000000915
023100000915      *-------------------------------------------------------------*
023200000915      *   SRMOLLA  - Subroutine rilascio dati tariffa di cartello   *
023300000915      *-------------------------------------------------------------*
023400000915
023500000915     C     SRMOLLA       begsr
023600000915     C                   move      tgcatbCartellotgcatb
023700000918
023800000918     C                   z-add     tgcsgvCartellocomodo
023900000918     C                   exsr      CONVERTI
024000000918     C                   z-add     comodo        tgcsgv
024100000918
024200000918     C                   z-add     tgcsgrCartellocomodo
024300000918     C                   exsr      CONVERTI
024400000918     C                   z-add     comodo        tgcsgr
024500000918
024600000918     C                   z-add     tgcsgpCartellocomodo
024700000918     C                   exsr      CONVERTI
024800000918     C                   z-add     comodo        tgcsgp
024900000918
025000000918     C                   z-add     tgcsgdCartellocomodo
025100000918     C                   exsr      CONVERTI
025200000918     C                   z-add     comodo        tgcsgd
025300000918
025400000915     C                   move      tgcsg1Cartellotgcsg1
025500000915     C                   move      tgcsg2Cartellotgcsg2
025600000915     C                   move      tgcsg3Cartellotgcsg3
025700000918
025800000918     C                   z-add     tgcst1Cartellocomodo
025900000918     C                   exsr      CONVERTI
026000000918     C                   z-add     comodo        tgcst1
026100000918
026200000918     C                   z-add     tgcst2Cartellocomodo
026300000918     C                   exsr      CONVERTI
026400000918     C                   z-add     comodo        tgcst2
026500000918
026600000918     C                   z-add     tgcst3Cartellocomodo
026700000918     C                   exsr      CONVERTI
026800000918     C                   z-add     comodo        tgcst3
026900000918
027000000918     C                   z-add     tgcstmCartellocomodo
027100000918     C                   exsr      CONVERTI
027200000918     C                   z-add     comodo        tgcstm
027300000918
027400000915     C                   move      tgcrpvCartellotgcrpv
027500000915     C                   move      tgcfafCartellotgcfaf
027600000915     C                   move      tgcsgfCartellotgcsgf
027700000915     C                   move      tgcggrCartellotgcggr
027800000915     C                   endsr
027900000915
028000000915      *-------------------------------------------------------------*
028100000915      *   SRERRORE - Subroutine stampa errore                       *
028200000915      *-------------------------------------------------------------*
028300000915
028400000915     C     SRERRORE      begsr
028500000918     C                   write     errore
028600000915     C                   endsr
028700000915
028800000915      *-------------------------------------------------------------*
028900000915      *   SRINGLOBA - Subroutine inglobo tariffe                    *
029000000915      *-------------------------------------------------------------*
029100000915
029200000915     C     SRINGLOBA     begsr
029300000918
029400000918     C                   eval      tgcduv = tamduv                              data ult.variaz.
029500000918     C                   move      tamksc        tgcksc
029600000918     C                   move      tamctr        tgcctr
029700000918     C                   move      tamprg        tgcprg
029800000918
029900000918      *   Controllo se società cliente è presente in L1
030000000918      *   ... se presente: *IN06 = *ON
030100000918
030200000918     C                   movel     tamksc        W3numerico        3 0
030300000918     C     W3numerico    lookup    L1                                     06
030400000918
030500000918      *   Se presente in L1 sflaggo per la trasmissione
030600000918
030700000918     C     *in06         ifeq      *on
030800000918     C                   clear                   tgcftr
030900000918     C                   clear                   tgcdtr
031000000918     C                   else
031100000918     C                   eval      tgcftr = tamftr
031200000918     C                   eval      tgcdtr = tamdtr
031300000918     C                   endif
031400000918
031500000918      *   Recupero flags da "FNCLS"
031600000918
031700000918     C                   clear                   tgctcm
031800000918     C                   clear                   tgctfg
031900000918     C     tamksc        chain     fncls01l
032000000918     C                   if        %found(fncls01l)
032100000918     C                   movel     clsflo        tgctfg
032200000918     C                   move      clsflo        tgctcm
032300000918     C                   endif
032400000918
032500000918     C                   move      ' '           aster
032600000918
032700000918     C     tgctfg        ifeq      ' '
032800000918     C                   eval      tgctfg = 'R'
032900000918     C                   move      '*'           aster
033000000918     C                   endif
033100000918
033200000918     C     tgctcm        ifeq      ' '
033300000918     C                   eval      tgctcm = 'P'
033400000918     C                   move      '*'           aster
033500000918     C                   endif
033600000918
033700000918      *   Recupero Valori divisa
033800000918
033900000918     C                   movel     tamflo        dsta01
034000000918     C     §tadiv        ifeq      *blanks
034100000918     C                   eval      §tadiv = 'ITL'
034200000918     C                   endif
034300000918
034400000918     C                   movel     'CV'          cod
034500000918     C                   movel(p)  §tadiv        key
034600000918     C                   clear                   dscv
034700000918     C     KeyTabelle    chain     tabel00f
034800000918     C                   if        %found(tabel00f)
034900000918     C                   movel     tbluni        dscv
035000000918     C                   endif
035100000918
035200000918     C                   setoff                                       12
035300000918     C                   eval      *in12=(§cvfdc = 'S')
035400000918
035500000918      *   Completamento e scrittura/aggiornamento record
035600000918
035700000918     C                   exsr      SRMOLLA
035800000918
035900000918     C  N03              UPDATE    titgc000
036000000918     C   03              WRITE     titgc000
036100000918
036200000918     C     *in06         ifeq      *on
036300000918     C   55              write     testa
036400000918     C   55              setoff                                       55
036500000918     C                   write     dett
036600000918     C                   endif
036700000918
036800000915     C                   endsr
036900000918
037000000918      *-------------------------------------------------------------*
037100000918      *   CONVERTI - Subroutine conversione importi                 *
037200000918      *-------------------------------------------------------------*
037300000918
037400000918     C     CONVERTI      begsr
037500000918
037600000918     C                   if        comodo > 0
037700000918     C                   clear                   YEUR
037800000918     C                   movel     'ITL'         YECDVI
037900000918     C                   movel     §TADIV        YECDVO
038000000918     C                   z-add     comodo        YECIMI
038100000918     C   12              move      '3'           YECDCO
038200000918     C  N12              move      '0'           YECDCO
038300000918
038400000918     C                   CALL      'YEURCO'
038500000918     C                   PARM                    YEUR
038600000918
038700000918     C     YECESI        ifeq      ' '
038800000918     C                   z-add     YECIMO        comodo           15 3
038900000918     C                   else
039000000918     C                   z-add     *ZEROS        comodo
039100000918     C                   endif
039200000918
039300000918     C                   endif
039400000918
039500000918     C                   endsr
