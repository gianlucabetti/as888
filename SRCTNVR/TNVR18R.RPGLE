000100980323     H DECEDIT('0,') DATEDIT(*DMY.)
000200000915
000300000915      *-------------------------------------------------------------*
000400000915      *                                                             *
000500000918      *   ***  INGLOBO  ***   TARIFFE GIACENZE IN OFFERTE           *
000600000915      *                                                             *
000700000915      *-------------------------------------------------------------*
000800000915
000900000918     FTNOFM01L  IF   E           K DISK
001000000918     FTNVIS05L  IF   E           K DISK
001100000918     FTITGC01L  IF   E           K DISK    RENAME(TITGC000:TITGC1)
001200000918     FTIOGC01L  UF A E           K DISK
001300990923     FTABEL00F  IF   E           K DISK
001400000918     FFNCLS01L  IF   E           K DISK
001500000918     FTNVR18P   O    E             PRINTER OFLIND(*IN55)
001600000915
001700000918     D DSTA01        E DS
001800000918     D DSCV          E DS
001900000918     D YEUR          E DS                  EXTNAME(YEURCODS)
002000980323     D CNCR80        E DS
002100980323     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200000915
002300000915      *-------------------------------------------------------------*
002400000915      *                                                             *
002500000915      *   OPERAZIONI INIZIALI                                       *
002600000915      *                                                             *
002700000915      *-------------------------------------------------------------*
002800000918
002900000918      *   Carico L1
003000000918
003100000918     C                   Z-ADD     1             CODUT             1 0
003200000918     C                   CALL      'X§PARUT'
003300000918     C                   PARM                    UT§DSE
003400000918     C                   MOVEL     REC80         CNCR80
003500000918     C                   MOVEL     RAGUT         RSUT
003600000915
003700000918     C                   write     testa
003800000918     C                   setoff                                       55
003900000918
004000000918      *   Definisco la tariffa di cartello
004100000915
004200000915     C     *like         define    tgcatb        tgcatbCartello
004300000915     C     *like         define    tgcksc        tgckscCartello
004400000915     C     *like         define    tgcctr        tgcctrCartello
004500000915     C     *like         define    tgcprg        tgcprgCartello
004600000915     C     *like         define    tgcsgv        tgcsgvCartello
004700000915     C     *like         define    tgcsgr        tgcsgrCartello
004800000915     C     *like         define    tgcsgp        tgcsgpCartello
004900000915     C     *like         define    tgcsgd        tgcsgdCartello
005000000915     C     *like         define    tgcsg1        tgcsg1Cartello
005100000915     C     *like         define    tgcsg2        tgcsg2Cartello
005200000915     C     *like         define    tgcsg3        tgcsg3Cartello
005300000915     C     *like         define    tgcst1        tgcst1Cartello
005400000915     C     *like         define    tgcst2        tgcst2Cartello
005500000915     C     *like         define    tgcst3        tgcst3Cartello
005600000915     C     *like         define    tgcstm        tgcstmCartello
005700000915     C     *like         define    tgcrpv        tgcrpvCartello
005800000915     C     *like         define    tgcfaf        tgcfafCartello
005900000915     C     *like         define    tgcsgf        tgcsgfCartello
006000000915     C     *like         define    tgcggr        tgcggrCartello
006100000915     C     *like         define    tgctcm        tgctcmCartello
006200000915     C     *like         define    tgctfg        tgctfgCartello
006300000915     C     *like         define    tgcduv        tgcduvCartello
006400000915     C     *like         define    tgcftr        tgcftrCartello
006500000915     C     *like         define    tgcdtr        tgcdtrCartello
006600000915
006700000915     C     *like         define    tgcksc        keyksc
006800000915     C     *like         define    tgcctr        keyctr
006900000915     C     *like         define    tgcprg        keyprg
007000000918
007100000918     C     *like         define    tblcod        cod
007200000918     C     *like         define    tblkey        key
007300000915
007400000915      *   Accesso al Data Base
007500000915
007600000915     C     KeyTariffe    KLIST
007700000915     C                   KFLD                    Keyksc
007800000915     C                   KFLD                    Keyctr
007900000915     C                   KFLD                    Keyprg
008000000918
008100000918     C     KeyTabelle    KLIST
008200000918     C                   KFLD                    codut
008300000918     C                   KFLD                    cod
008400000918     C                   KFLD                    key
008500000915
008600000915      *   Ripresa dati tariffa di controllo
008700000915
008800000915     C                   z-add     8888830       keyksc
008900000915     C                   z-add     0             keyctr
009000000915     C                   z-add     5             keyprg
009100000915
009200000915     C     KeyTariffe    chain     titgc01l
009300000915     C                   if        %found(titgc01l)
009400000915     C                   if        tgcatb = ' '
009500000915
009600000915      *                  ==================
009700000915     C                   exsr      SRPIGLIA
009800000915      *                  ==================
009900000915
010000000915     C                   endif
010100000915     C                   endif
010200000915
010300000915      *-------------------------------------------------------------*
010400000915      *                                                             *
010500000915      *   ROUTINE PRINCIPALE                                        *
010600000915      *                                                             *
010700000915      *-------------------------------------------------------------*
010800000915
010900000915      *   Se sono riuscito a reperire la tariffa di cartello procedo
011000000915      *   all'inglobo nei clienti che non la hanno.
011100000915      *   *IN01 = *ON
011200000915
011300000915     C     *in01         ifeq      *on
011400000918     C     *loval        setll     tnofm01l
011500000915
011600000915      *   Leggo il tariffario clienti
011700000915
011800000915     C                   do        *hival
011900000918     C                   read      tnofm01l
012000000915
012100000918     C                   if        %eof(tnofm01l)
012200000915     C                   leave
012300000915     C                   endif
012400000915
012500000915      *   Controllo se presente tariffa giacenza
012600000915
012700000915     C                   z-add     tamksc        keyksc
012800000915     C                   z-add     tamctr        keyctr
012900000918     C                   z-add     tamprg        keyprg
013000000918     C     KeyTariffe    chain     tiogc01l
013100000918     C                   if        %found(tiogc01l)
013200000915     C     tgcatb        ifne      *blanks
013300000915
013400000915      *   Se ho trovato tariffa ma annullata la aggiorno
013500000915      *   con i dati di quella cartello
013600000915
013700000915     C                   setoff                                       03        update
013800000915
013900000915      *                  ===================
014000000915     C                   exsr      SRINGLOBA
014100000915      *                  ===================
014200000915
014300000915     C                   endif
014400000915     C                   else
014500000915
014600000915      *   Se non ho trovato tariffa la scrivo
014700000915      *   con i dati di quella cartello
014800000915
014900000915     C                   seton                                        03        write
015000000915
015100000915      *                  ===================
015200000915     C                   exsr      SRINGLOBA
015300000915      *                  ===================
015400000915
015500000915     C                   endif
015600000915
015700000915
015800000915     C                   enddo
015900000915
016000000915     C                   endif
016100000915
016200000915      *   Se non sono riuscito a reperire la tariffa di cartello
016300000915      *   stampo errore
016400000915      *   *IN01 = *OFF
016500000915
016600000915      *                  ==================
016700000915     C  N01              exsr      SRERRORE
016800000915      *                  ==================
016900000915
017000000918     C                   write     finest
017100000918
017200000915     C                   eval      *inlr = *on
017300000915
017400000915      *-------------------------------------------------------------*
017500000915      *                                                             *
017600000915      *   SUBROUTINES                                               *
017700000915      *                                                             *
017800000915      *-------------------------------------------------------------*
017900000915
018000000915      *-------------------------------------------------------------*
018100000915      *   SRPIGLIA - Subroutine ripresa dati tariffa di cartello    *
018200000915      *-------------------------------------------------------------*
018300000915
018400000915     C     SRPIGLIA      begsr
018500000915     C                   seton                                        01
018600000915     C                   move      tgcatb        tgcatbCartello
018700000915     C                   move      tgcksc        tgckscCartello
018800000915     C                   move      tgcctr        tgcctrCartello
018900000915     C                   move      tgcprg        tgcprgCartello
019000000915     C                   move      tgcsgv        tgcsgvCartello
019100000915     C                   move      tgcsgr        tgcsgrCartello
019200000915     C                   move      tgcsgp        tgcsgpCartello
019300000915     C                   move      tgcsgd        tgcsgdCartello
019400000915     C                   move      tgcsg1        tgcsg1Cartello
019500000915     C                   move      tgcsg2        tgcsg2Cartello
019600000915     C                   move      tgcsg3        tgcsg3Cartello
019700000915     C                   move      tgcst1        tgcst1Cartello
019800000915     C                   move      tgcst2        tgcst2Cartello
019900000915     C                   move      tgcst3        tgcst3Cartello
020000000915     C                   move      tgcstm        tgcstmCartello
020100000915     C                   move      tgcrpv        tgcrpvCartello
020200000915     C                   move      tgcfaf        tgcfafCartello
020300000915     C                   move      tgcsgf        tgcsgfCartello
020400000915     C                   move      tgcggr        tgcggrCartello
020500000915     C                   move      tgctcm        tgctcmCartello
020600000915     C                   move      tgctfg        tgctfgCartello
020700000915     C                   move      tgcduv        tgcduvCartello
020800000915     C                   move      tgcftr        tgcftrCartello
020900000915     C                   move      tgcdtr        tgcdtrCartello
021000000915     C                   endsr
021100000915
021200000915      *-------------------------------------------------------------*
021300000915      *   SRMOLLA  - Subroutine rilascio dati tariffa di cartello   *
021400000915      *-------------------------------------------------------------*
021500000915
021600000915     C     SRMOLLA       begsr
021700000915     C                   move      tgcatbCartellotgcatb
021800000918
021900000918     C                   z-add     tgcsgvCartellocomodo
022000000918     C                   exsr      CONVERTI
022100000918     C                   z-add     comodo        tgcsgv
022200000918
022300000918     C                   z-add     tgcsgrCartellocomodo
022400000918     C                   exsr      CONVERTI
022500000918     C                   z-add     comodo        tgcsgr
022600000918
022700000918     C                   z-add     tgcsgpCartellocomodo
022800000918     C                   exsr      CONVERTI
022900000918     C                   z-add     comodo        tgcsgp
023000000918
023100000918     C                   z-add     tgcsgdCartellocomodo
023200000918     C                   exsr      CONVERTI
023300000918     C                   z-add     comodo        tgcsgd
023400000918
023500000915     C                   move      tgcsg1Cartellotgcsg1
023600000915     C                   move      tgcsg2Cartellotgcsg2
023700000915     C                   move      tgcsg3Cartellotgcsg3
023800000918
023900000918     C                   z-add     tgcst1Cartellocomodo
024000000918     C                   exsr      CONVERTI
024100000918     C                   z-add     comodo        tgcst1
024200000918
024300000918     C                   z-add     tgcst2Cartellocomodo
024400000918     C                   exsr      CONVERTI
024500000918     C                   z-add     comodo        tgcst2
024600000918
024700000918     C                   z-add     tgcst3Cartellocomodo
024800000918     C                   exsr      CONVERTI
024900000918     C                   z-add     comodo        tgcst3
025000000918
025100000918     C                   z-add     tgcstmCartellocomodo
025200000918     C                   exsr      CONVERTI
025300000918     C                   z-add     comodo        tgcstm
025400000918
025500000915     C                   move      tgcrpvCartellotgcrpv
025600000915     C                   move      tgcfafCartellotgcfaf
025700000915     C                   move      tgcsgfCartellotgcsgf
025800000915     C                   move      tgcggrCartellotgcggr
025900000915     C                   endsr
026000000915
026100000915      *-------------------------------------------------------------*
026200000915      *   SRERRORE - Subroutine stampa errore                       *
026300000915      *-------------------------------------------------------------*
026400000915
026500000915     C     SRERRORE      begsr
026600000918     C                   write     errore
026700000915     C                   endsr
026800000915
026900000915      *-------------------------------------------------------------*
027000000915      *   SRINGLOBA - Subroutine inglobo tariffe                    *
027100000915      *-------------------------------------------------------------*
027200000915
027300000915     C     SRINGLOBA     begsr
027400000918
027500000918     C                   eval      tgcduv = tamduv                              data ult.variaz.
027600000918     C                   move      tamksc        tgcksc
027700000918     C                   move      tamctr        tgcctr
027800000918     C                   move      tamprg        tgcprg
027900000918     C                   clear                   tgcftr
028000000918     C                   clear                   tgcdtr
028100000918
028200000918      *   Recupero flags da "FNCLS"
028300000918
028400000918     C                   clear                   tgctcm
028500000918     C                   clear                   tgctfg
028600000918     C     tamksc        chain     tnvis05l
028700000918     C                   if        %found(tnvis05l)
028800000918     C     visksc        chain     fncls01l
028900000918     C                   if        %found(fncls01l)
029000000918     C                   movel     clsflo        tgctfg
029100000918     C                   move      clsflo        tgctcm
029200000918     C                   endif
029300000918     C                   endif
029400000918
029500000918     C                   move      ' '           aster
029600000918
029700000918     C     tgctfg        ifeq      ' '
029800000918     C                   eval      tgctfg = 'R'
029900000918     C                   move      '*'           aster
030000000918     C                   endif
030100000918
030200000918     C     tgctcm        ifeq      ' '
030300000918     C                   eval      tgctcm = 'P'
030400000918     C                   move      '*'           aster
030500000918     C                   endif
030600000918
030700000918      *   Recupero Valori divisa
030800000918
030900000918     C                   movel     tamflo        dsta01
031000000918     C     §tadiv        ifeq      *blanks
031100000918     C                   eval      §tadiv = 'ITL'
031200000918     C                   endif
031300000918
031400000918     C                   movel     'CV'          cod
031500000918     C                   movel(p)  §tadiv        key
031600000918     C                   clear                   dscv
031700000918     C     KeyTabelle    chain     tabel00f
031800000918     C                   if        %found(tabel00f)
031900000918     C                   movel     tbluni        dscv
032000000918     C                   endif
032100000918
032200000918     C                   setoff                                       12
032300000918     C                   eval      *in12=(§cvfdc = 'S')
032400000918
032500000918      *   Completamento e scrittura/aggiornamento record
032600000918
032700000918     C                   exsr      SRMOLLA
032800000918
032900000918     C  N03              UPDATE    titgc000
033000000918     C   03              WRITE     titgc000
033100000918
033200000918     C   55              write     testa
033300000918     C   55              setoff                                       55
033400000918     C                   write     dett
033500000918
033600000915     C                   endsr
033700000918
033800000918      *-------------------------------------------------------------*
033900000918      *   CONVERTI - Subroutine conversione importi                 *
034000000918      *-------------------------------------------------------------*
034100000918
034200000918     C     CONVERTI      begsr
034300000918
034400000918     C                   if        comodo > 0
034500000918     C                   clear                   YEUR
034600000918     C                   movel     'ITL'         YECDVI
034700000918     C                   movel     §TADIV        YECDVO
034800000918     C                   z-add     comodo        YECIMI
034900000918     C   12              move      '3'           YECDCO
035000000918     C  N12              move      '0'           YECDCO
035100000918
035200000918     C                   CALL      'YEURCO'
035300000918     C                   PARM                    YEUR
035400000918
035500000918     C     YECESI        ifeq      ' '
035600000918     C                   z-add     YECIMO        comodo           15 3
035700000918     C                   else
035800000918     C                   z-add     *ZEROS        comodo
035900000918     C                   endif
036000000918
036100000918     C                   endif
036200000918
036300000918     C                   endsr
