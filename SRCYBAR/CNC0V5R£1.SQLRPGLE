000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300911106      * CNC0V5R                                                      *
000400900606      *                                                              *
000500911107      *        STAMPA DISTINTA C/ASSEGNI DIRETTI + DELET ASSGNI DIRET*
000600900606?     *--------------------------------------------------------------*
000700110914     Fcnass00f  uf   E             DISK
000800120220     Ffncss03l  uf   E           k DISK
000900110913     FAZORG01L  IF   E           K DISK
001000911106     FCNABI01L  IF   E           K DISK
001100931001     FTABEL00F  IF   E           K DISK
001200941020     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
001300941020     F                                     USROPN
001400900515      *
001500950208     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)            PRINTER OV
001600931001     D*-------------------------------------------------------------------*
001700931001     D* SCHIERA PER CARICAMENTO CLIENTI ABILITATI AI "BB"                 *
001800951019     D COD             S              7  0 DIM(200)                             COD.CLIENT
001900950123     D TCV             S              3    DIM(30)                              TAB. DIVIS
002000950123     D DCV             S             89    DIM(30)                              DSCV
002100931001     D*-------------------------------------------------------------------*
002200900515      *
002300110913     D fnarbds       E DS                  extname(fnarb00f)
002400110913     D fiar9ds       E DS                  extname(fiar900f)
002500120118     D tncsmds       E DS                  extname(tncsm00f)
002600110913     D cnassds       E DS                  extname(cnass00f)
002700110913     D CNCR80        E DS
002800941020     D WLBDA8          DS
002900941020     D  G08DAT                 1      8  0
003000941020     D  G08INV                 9     16  0
003100941020     D  G08ERR                17     17
003200941020     D  G08TGI                18     22  0
003300941020     D*
003400900514     D KPJBA         E DS
003500900514     D  LIBSYS                92    100
003600900514     D  MSGERR               453    502
003700900516      *
003800900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003900931001     D*
004000931001     D DSTM          E DS
004100900606      *
004200941021     D DSCV          E DS
004300941021      *
004400941021     D                SDS
004500941021     D  §PGM                   1     10
004600110914      *
004700911106     D PARAM           DS
004800911106     D  PFLE                   1      3
004900911106     D  PCUC                   4      6
005000911106     D  PNMI                   7     13
005100110914     D  PdMI                  14     21
005200110914      *
005300110913     d nrr             s              9  0
005400110914     D £tpi            s                   like(asstpi)
005500110914     D £tpa            s                   like(asstpa)
005600110914     D £dte            s                   like(assdte)
005700110914     D £nra            s                   like(assnra)
005800110914     D £abi            s                   like(assabi)
005900110914     D £cab            s                   like(asscab)
006000110914     D £vca            s                   like(assvca)
006100110914     D £cas            s                   like(asscas)
006200120125     D £key            s                   like(csskey)
006201120314     d rifass          ds
006202120314     D §nra                                like(assnra)
006203120314     D §dte                                like(assdte)
006204120314     D §abi                                like(assabi)
006205120314     D §cab                                like(asscab)
006206120314     D §key                                like(csskey)
006300110914     d §TOTbe          s              3  0
006400110914     d §TOTMV          s                   like(asscas)
006500110914     d §TOTEU          s                   like(asscas)
006600120222     d §TOTbes         s              3  0
006700120222     d §TOTMVs         s                   like(asscas)
006800120222     d §TOTEUs         s                   like(asscas)
006900120314     d §cas            s                   like(asscas)
007000120222     D §vca            s                   like(assvca)
007101120314     d savrif          s             38
007200900524      *
007300911106      * APRO FILE DI STAMPA
007400911106     C                   MOVEA     CMDP          CMDPX            60
007500911106     C                   Z-ADD     60            LUNCMD           15 5
007600950208     C                   CALL      'QCMDEXC'                            91
007700911106     C                   PARM                    CMDPX
007800911106     C                   PARM                    LUNCMD
007900941020     C                   OPEN      QSYSPRT
008000110908     c                   seton                                        of
008100120222     c* prima di cominciare calcolo il totale c/assegni perchè poi il
008200120222     c* CNASS sarà deletato, devo farlo prima perchè altrimenti potrei
008300120222     c* contare lo stesso c/assegno più volte nel caso di una spedizione
008400120222     c* con + assegni
008401120314     c                   exsr      srtotali
014000911106      *
014100110913     C/EXEC SQL
014200110913     C+ DECLARE b1 CURSOR FOR
014300110914     C+ select arbaas, arblnp, arbnrs, arbnsp, arblna, arbrsm, arbrmn,
014400110914     C+ arbccm, arbksc,
014500120118     C+         ifnull(csmtpi, asstpi) £tpi,
014600120118     C+         ifnull(csmtpa, asstpa) £tpa,
014700110914     C+         asscas, rrn(cnass00f),
014800120118     C+         ifnull(csmnra, assnra) £nra,
014900120118     C+         ifnull(csmdte, assdte) £dte,
015000120118     C+         ifnull(csmabi, assabi) £abi,
015100120118     C+         ifnull(csmcab, asscab) £cab,
015200120118     C+         ifnull(csmidc, asscas) £cas,
015300120125     C+         ifnull(csmdiv, assvca) £vca,
015400120125     C+         ifnull(csmkey, ' ') £key
015500110913     C+ from cnass00F join fnarb00f on
015600110913     C+ assaas = arbaas and arblnp=asslnp and
015700110913     C+ arbnrs=assnrs and arbnsp=assnsp
015800110913     C+ join fiar900f on
015900110913     C+ assaas = ar9aas and ar9lnp=asslnp and
016000110913     C+ ar9nrs=assnrs and ar9nsp=assnsp
016100120118     C+ left outer join tncsm00f on
016200120118     C+ assnra = csmkey
016300110913     C+ WHERE
016400110914     C+ asstpi = 'M' and assfle = :§fle
016500110913     C+ and arbnmi = :§nmi and arbdmi = :§dmi
016600110913     C+ and arbipp= :§fle
016700110914     C+ order by £cas, £abi, £cab, £nra, £dte
016800110914     C/END-EXEC
016900110913     C/EXEC SQL
017000110913     C+ OPEN b1
017100110913     C/END-EXEC
017200110913     C                   do        *hival
017300110913
017400110913     C/EXEC SQL
017500110913     C+ FETCH NEXT FROM b1 INTO
017600110914     C+ :arbaas, :arblnp, :arbnrs, :arbnsp, :arblna,
017700110914     C+ :arbrsm, :arbrmn, :arbccm, :arbksc,
017800110914     C+ :£tpi, :£tpa, :asscas, :nrr, :£nra, :£dte,
017900120125     C+ :£abi, :£cab, :£cas, :£vca, :£key
018000110913     C/END-EXEC
018100110913
018200110913     C                   select
018300110913     C                   when      sqlcod = 100
018400110913     c                   leave
018500110913     C                   WHEN      SqlCod < 0
018600110913     c                   if        sqlcod <> -913
018700110913     C                   seton                                        H1
018800110913     c                   end
018900110913     c                   seton                                        lr
019000110913     c                   return
019100110913     C                   other
019200110913     c                   seton                                        10
019300110913     c                   exsr      srstampa
019400110914     c     nrr           delete    cnass00f
019500120125     c* cancello l'eventuale record nel file delle spedizioni perchè in
019600120125     c* sede non serve inquanto verrà memorizzato nel TNCSB
019700120125     c                   if        £key <> ' '
019800120220     c     £key          setll     fncss03l
019900120125     c                   do        *hival
020000120220     c     £key          reade     fncss03l
020100120220     c                   if        %eof(fncss03l)
020200120125     c                   leave
020300120125     c                   end
020400120220     c                   delete    fncss000
020500120125     c                   enddo
020600120125     c                   end
020700110913     C                   endsl
020800110913     C                   enddo
020900110913     C/EXEC SQL
021000110913     C+ CLOSE b1
021100110913     C/END-EXEC
021200010928      *
021300110907     C     §TOTBE        IFNE      *ZEROS
021400011003     C     §TOTEU        ORNE      *ZEROS
021500941019     C     §TOTMV        ORNE      *ZEROS
021600911106     C                   EXCEPT    TOTALE
021700930521     C                   END
021800910521     C                   SETON                                        LR
021900110901     C**********************************************************************
022000120314     c     srtotali      begsr
022100110901     C**********************************************************************
022102120314     c                   clear                   §totmv
022103120314     c                   clear                   §toteu
022104120314     c                   clear                   §totbe
022105120314     c* assegni singoli
022106120314     C/EXEC SQL
022107120314     C+ DECLARE a1 CURSOR FOR
022108120315     C+ SELECT assvca, asscas, arbccm, arbksc from
022109120314     C+ cnass00F join fnarb00f on
022110120314     C+ assaas = arbaas and arblnp=asslnp and
022111120314     C+ arbnrs=assnrs and arbnsp=assnsp exception join fncss00f on
022112120314     C+ assaas = cssaao and arblnp=csslno and
022113120314     C+ arbnrs=cssnro and arbnsp=cssnso
022114120314     C+ WHERE asstpi = 'M' and assfle = :§fle and arbnmi = :§nmi
022115120314     C+ and arbdmi = :§dmi and arbipp= :§fle
022116120314     C/END-EXEC
022117120314     c*
022118120314     C/EXEC SQL
022119120314     C+ OPEN a1
022120120314     C/END-EXEC
022121120314     C                   do        *hival
022122120314
022123120314     C/EXEC SQL
022124120314     C+ FETCH NEXT FROM a1 INTO
022125120314     C+ :§vca, :§cas, :arbccm, : arbksc
022126120314     C/END-EXEC
022127120314
022128120314     C                   select
022129120314     C                   when      sqlcod = 100
022130120314     c                   leave
022131120314     C                   WHEN      SqlCod < 0
022132120314     c                   if        sqlcod <> -913
022133120314     C                   seton                                        H1
022134120314     c                   end
022135120314     c                   seton                                        lr
022136120314     c                   return
022137120314     C                   other
022138120314     C                   CLEAR                   DSCV
022139120314     C     §vca          IFNE      *BLANK
022140120314     C                   Z-ADD     1             X                 3 0
022141120314     C     §vca          LOOKUP    TCV(X)                                 22
022142120314     C   22              MOVEL     DCV(X)        DSCV
022143120314     C                   ELSE
022144120314     C                   MOVEL     'I'           §CVITA
022145120314     C                   ENDIF
022146120314     C*  CONTROLLO SE CLIENTE ABILITATO "BB" OPPURE NO
022147120314     C                   SETOFF                                       40
022149120314     C     §CVITA        IFEQ      'I'                                          LIRE
022150120314     C     §vca          OREQ      'EUR'
022151120314     C     arbccm        IFNE      0
022152120314     C     arbccm        LOOKUP    COD                                    40
022153120314     C                   ELSE
022154120314     C     arbksc        LOOKUP    COD                                    40
022155120314     C                   END
022156120314     C                   END
022157120314      * NON "BB"
022158120314     C     *IN40         IFEQ      '0'
022159120314     C                   Z-ADD     1             O                 4 0
022160120314     C     'EUR'         LOOKUP    TCV(O)                                 14
022161120314     C                   MOVEL     DCV(O)        DSCV
022162120314     c                   add       1             §totbe
022163120314     C     §vca          IFEQ      'EUR'
022164120314     C     *IN14         ANDEQ     *ON
022165120314     C     §CVDVA        ANDEQ     'S'
022166120314     C                   ADD       §cas          §TOTEU
022167120314     C                   ELSE
022168120314     C                   ADD       §cas          §TOTMV
022169120314     C                   ENDIF
022170120314     C                   END
022171120314     C                   endsl
022172120314     C                   enddo
022173120314     C/EXEC SQL
022174120314     C+ CLOSE a1
022175120314     C/END-EXEC
022176120314     c* assegni multipli
022177120314     C/EXEC SQL
022178120314     C+ DECLARE a2 CURSOR FOR
022179120314     C+ SELECT csmdiv, csmidc, arbccm, arbksc,
022180120314     C+ csmnra, csmdte, csmabi, csmcab from
022181120314     C+ cnass00F join fnarb00f on
022182120314     C+ assaas = arbaas and arblnp=asslnp and
022183120314     C+ arbnrs=assnrs and arbnsp=assnsp join tncsm00f on
022184120314     C+ assnra = csmkey
022185120314     C+ WHERE asstpi = 'M' and assfle = :§fle and arbnmi = :§nmi
022186120314     C+ and arbdmi = :§dmi and arbipp= :§fle
022187120314     c+ order by csmkey, csmnra, csmdte, csmabi, csmcab
022188120314     C/END-EXEC
022189120314     c*
022190120314     C/EXEC SQL
022191120314     C+ OPEN a2
022192120314     C/END-EXEC
022193120314     C                   do        *hival
022194120314
022195120314     C/EXEC SQL
022196120314     C+ FETCH NEXT FROM a2 INTO
022197120314     C+ :§vca, :§cas, :arbccm, :arbksc, :§nra, :§dte, :§abi, :§cab, :§key
022198120314     C/END-EXEC
022199120314
022200120314     C                   select
022201120314     C                   when      sqlcod = 100
022202120314     c                   leave
022203120314     C                   WHEN      SqlCod < 0
022204120314     c                   if        sqlcod <> -913
022205120314     C                   seton                                        H1
022206120314     c                   end
022207120314     c                   seton                                        lr
022208120314     c                   return
022209120314     C                   other
022210120314     c* l'assegno un
022211120314     c                   if        rifass = savrif
022212120314     c                   iter
022213120314     c                   else
022214120314     c                   eval      savrif = rifass
022215120314     c                   end
022216120314     C                   CLEAR                   DSCV
022217120314     C     §vca          IFNE      *BLANK
022218120314     C                   Z-ADD     1             X                 3 0
022219120314     C     §vca          LOOKUP    TCV(X)                                 22
022220120314     C   22              MOVEL     DCV(X)        DSCV
022221120314     C                   ELSE
022222120314     C                   MOVEL     'I'           §CVITA
022223120314     C                   ENDIF
022224120314     C*  CONTROLLO SE CLIENTE ABILITATO "BB" OPPURE NO
022225120314     C                   SETOFF                                       40
022226120314     C     §CVITA        IFEQ      'I'                                          LIRE
022227120314     C     §vca          OREQ      'EUR'
022228120314     C     arbccm        IFNE      0
022229120314     C     arbccm        LOOKUP    COD                                    40
022230120314     C                   ELSE
022231120314     C     arbksc        LOOKUP    COD                                    40
022232120314     C                   END
022233120314     C                   END
022234120314      * NON "BB"
022235120314     C     *IN40         IFEQ      '0'
022236120314     C                   Z-ADD     1             O                 4 0
022237120314     C     'EUR'         LOOKUP    TCV(O)                                 14
022238120314     C                   MOVEL     DCV(O)        DSCV
022239120314     c                   add       1             §totbe
022240120314     C     §vca          IFEQ      'EUR'
022241120314     C     *IN14         ANDEQ     *ON
022242120314     C     §CVDVA        ANDEQ     'S'
022243120314     C                   ADD       §cas          §TOTEU
022244120314     C                   ELSE
022245120314     C                   ADD       §cas          §TOTMV
022246120314     C                   ENDIF
022247120314     C                   END
022248120314     C                   endsl
022249120314     C                   enddo
022250120314     C/EXEC SQL
022251120314     C+ CLOSE a2
022252120314     C/END-EXEC
022253120314     c                   clear                   §totmvs
022254120314     c                   clear                   §toteus
022255120314     c                   clear                   §totbes
022256120314     C/EXEC SQL
022257120314     C+ DECLARE a3 CURSOR FOR
022258120314     C+ SELECT assvca, asscas, arbccm, arbksc from
022259120314     C+ cnass00F join fnarb00f on
022260120314     C+ assaas = arbaas and arblnp=asslnp and
022261120314     C+ arbnrs=assnrs and arbnsp=assnsp
022262120314     C+ WHERE asstpi = 'M' and assfle = :§fle and arbnmi = :§nmi
022263120314     C+ and arbdmi = :§dmi and arbipp= :§fle
022264120314     C/END-EXEC
022265120314     c*
022266120314     C/EXEC SQL
022267120314     C+ OPEN a3
022268120314     C/END-EXEC
022269120314     C                   do        *hival
022270120314
022271120314     C/EXEC SQL
022272120314     C+ FETCH NEXT FROM a3 INTO
022273120314     C+ :§vca, :§cas, :arbccm, :arbksc
022274120314     C/END-EXEC
022275120314
022276120314     C                   select
022277120314     C                   when      sqlcod = 100
022278120314     c                   leave
022279120314     C                   WHEN      SqlCod < 0
022280120314     c                   if        sqlcod <> -913
022281120314     C                   seton                                        H1
022282120314     c                   end
022283120314     c                   seton                                        lr
022284120314     c                   return
022285120314     C                   other
022286120314     C                   CLEAR                   DSCV
022287120314     C     §vca          IFNE      *BLANK
022288120314     C                   Z-ADD     1             X                 3 0
022289120314     C     §vca          LOOKUP    TCV(X)                                 22
022290120314     C   22              MOVEL     DCV(X)        DSCV
022291120314     C                   ELSE
022292120314     C                   MOVEL     'I'           §CVITA
022293120314     C                   ENDIF
022294120314     C*  CONTROLLO SE CLIENTE ABILITATO "BB" OPPURE NO
022295120314     C                   SETOFF                                       40
022296120314     C     §CVITA        IFEQ      'I'                                          LIRE
022297120314     C     §vca          OREQ      'EUR'
022298120314     C     arbccm        IFNE      0
022299120314     C     arbccm        LOOKUP    COD                                    40
022300120314     C                   ELSE
022301120314     C     arbksc        LOOKUP    COD                                    40
022302120314     C                   END
022303120314     C                   END
022304120314      * NON "BB"
022305120314     C     *IN40         IFEQ      '0'
022306120314     C                   Z-ADD     1             O                 4 0
022307120314     C     'EUR'         LOOKUP    TCV(O)                                 14
022308120314     C                   MOVEL     DCV(O)        DSCV
022309120314     c                   add       1             §totbes
022310120314     C     §vca          IFEQ      'EUR'
022311120314     C     *IN14         ANDEQ     *ON
022312120314     C     §CVDVA        ANDEQ     'S'
022313120314     C                   ADD       §cas          §TOTEUs
022314120314     C                   ELSE
022315120314     C                   ADD       §cas          §TOTMVs
022316120314     C                   ENDIF
022317120314     C                   END
022318120314     C                   endsl
022319120314     C                   enddo
022320120314     C/EXEC SQL
022321120314     C+ CLOSE a3
022322120314     C/END-EXEC
022323120314     c                   endsr
022324120314     C**********************************************************************
022325120314     c     srstampa      begsr
022326120314     C**********************************************************************
025900110913     c* campi in stampa
026000110914     C                   MOVEL     arbrsm        RSM20            20
026100110914     C                   Z-ADD     £dte          G08INV
026200110901     C                   MOVE      *ZEROS        G08DAT
026300110901     C                   MOVEL     '3'           G08ERR
026400110901     C                   CALL      'XSRDA8'
026500110901     C                   PARM                    WLBDA8
026600110901     C                   Z-ADD     G08DAT        GMADTE            8 0
026700110913     c*
026800110901     C     KABI          CHAIN     CNABI01L                           21
026900110901     C  N21ABIANN        COMP      *BLANK                             2121
027000110901     C  N21              MOVEL     ABIIST        §DESBA           20
027100110901     C   21              MOVEL     *ALL'?'       §DESBA
027200110913     c*
027300110901     c                   if        *inof
027400110901     C                   EXCEPT    TESTA
027500110908     C                   SETOff                                       of
027600110901     c                   end
027700110913     c*
027800110913     c                   if        *in10
027900110914     C     £cas          IFEQ      *ZEROS
028000110901     C                   EXCEPT    DETERR
028100110913     c                   else
028300110913     C                   EXCEPT    DETTA
028400110914     C     £cas          IFne      asscas
028500110914     C                   EXCEPT    DETca
028600110901     C                   END
028700110914     C                   END
028800110913     C                   END
028900110913     c                   setoff                                       10
029000110913     c*
029100110901     C                   ENDsr
029200110901     C**********************************************************************
029300941021     C     *INZSR        BEGSR
029400110901     C**********************************************************************
029500000000     C     *ENTRY        PLIST
029600000000     C                   PARM                    KPJBA
029700911106     C                   MOVEL     KPJBU         PARAM
029800911106     C                   MOVE      PFLE          §FLE              3 0
029900911106     C                   MOVE      PCUC          §CUC              3 0
030000911106     C                   MOVE      PNMI          §NMI              7 0
030100110914     c                   if        pdmi <> ' '
030200110914     C                   MOVE      PdMI          §dMI              8 0
030300110914     c                   else
030400110914     c                   clear                   §dmi
030500110914     c                   end
030600110914     C                   Z-ADD     §dmi          G08INV
030700110913     C                   MOVE      *ZEROS        G08DAT
030800110913     C                   MOVEL     '3'           G08ERR
030900110913     C                   CALL      'XSRDA8'
031000110913     C                   PARM                    WLBDA8
031100110913     C                   Z-ADD     G08DAT        GMADMI            8 0
031200900516      *
031300911106     C                   MOVEL     *ALL'-'       TRATT           132
031400000000     C                   Z-ADD     1             CODUT
031500900517     C                   CALL      'X§PARUT'
031600900517     C                   PARM                    UT§DSE
031700110914     C     CODUT         ifeq      -1
031800110914     c                   seton                                        lr
031900110914     c                   return
032000110914     c                   end
032100000000     C                   MOVEL     RAGUT         RSUT             20
032200971125     C                   MOVEL     RAGUT         RCONT            17
032300910708     C                   MOVEL     REC80         CNCR80
032400941020     C*§§
032500941020     C                   TIME                    WHHUDT           14 0
032600941020     C                   MOVEL     WHHUDT        ORA               6 0
032700941020     C                   MOVE      WHHUDT        G08DAT
032800941020     C                   MOVE      *ZEROS        G08INV
032900941020     C                   MOVEL     *BLANK        G08ERR
033000941020     C                   CALL      'XSRDA8'
033100941020     C                   PARM                    WLBDA8
033200941020     C                   Z-ADD     G08INV        AMGUDT            8 0
033300941020     C                   Z-ADD     G08DAT        GMAUDT            8 0
033400941020     C*§§
033500911031     C     §FLE          CHAIN     AZORG01L                           21
033600911106     C  N21              MOVEL     ORGDES        DFLE             20
033700911030     C*
033800900515      *---------------------------------------------------------------*
033900900515      * CHIAVI                                                        *
034000900515      *---------------------------------------------------------------*
034100911106     C     KABI          KLIST
034200110914     C                   KFLD                    £ABI
034300110914     C                   KFLD                    £CAB
034400931001     C*
034500931001     C*  DEFINISCO CHIAVE PER POSIZIONAMENTO SU TABEL00F
034600931001     C     KTAB          KLIST
034700931001     C                   KFLD                    TBLKUT
034800931001     C                   KFLD                    TBLCOD
034900931001     C*
035000931001     C                   Z-ADD     1             TBLKUT
035100931001     C                   MOVE      'TM'          TBLCOD
035200931001     C*
035300931001     C*  LETTURA RECORD TABELLA TM PER RICONOSCERE E MEMOR. IN SCHIERA
035400931001     C*  CLIENTI ABILITATI AL "BB"
035500931001     C                   Z-ADD     0             X
035600931005     C     KTAB          CHAIN     TABEL00F                           70
035700931001     C*
035800931001     C     *IN70         DOWEQ     '0'
035900931001     C                   MOVEL     TBLUNI        DSTM
036000931001     C     §TMFBB        IFEQ      'S'
036100940406     C     §TMFVE        ANDEQ     ' '
036200931001     C                   ADD       1             X
036300931005     C                   MOVEL     TBLKEY        COD(X)
036400931001     C                   END
036500931001     C     KTAB          READE     TABEL00F                               70
036600931001     C                   END
036700941021      *
036800941021      * CARICO TUTTI I CODICI VALUTA  TAB.CV
036900941021     C                   MOVEL     'CV'          TBLCOD
037000941021     C                   MOVE      *ZEROS        X
037100941021     C     KTAB          SETLL     TABEL00F
037200941021     C     *IN70         DOUEQ     '1'
037300941021     C     KTAB          READE     TABEL00F                               70
037400950123     C  N70TBLFLG        IFEQ      ' '
037500950123     C                   ADD       1             X
037600950123     C                   MOVEL     TBLKEY        TCV(X)
037700950123     C                   MOVEL     TBLUNI        DCV(X)
037800950123     C                   END
037900941021     C                   END
038000931001     C*
038100910521     C                   ENDSR
038200900606      *----------------------------------------------------------------
038300941021     OQSYSPRT   E            TESTA            02
038400911106     O                       RSUT                20
038500941021     O                       SIMFEL            +  1
038600120322     O                                           70 'INCASSI CONTRASSEGNI INTES'
038700120322     O                                         +  0 'TATI AL MITTENTE'
038800971125     O                       RCONT             +  1
038900941021     O                       §PGM              +  2
039000941021     O                       GMAUDT             122 '  /  /    '
039100911106     O                                          128 'PAG.'
039200911106     O                       PAGE               132
039300941021     O          E            TESTA            03
039400941021     O                       KNSIF             +  0
039500941021     O                       KNMUS             +  1
039600941021     O                       ORA                122 '  :  :  '
039700911106      *
039800941021     O          E            TESTA            04
039900911106     O                                         +  0 'DISTINTA N°'
040000911106     O                       §NMI          Z   +  1
040100911106     O                                         +  2 'DEL'
040200941020     O                       GMADMI            +  2 '  /  /    '
040300120322     O                                         +  2 'FILIALE'
040400911106     O                       §FLE          Z   +  2
040500911106     O                       DFLE              +  2
040600911106     O                                         +  2 'OPERATORE'
040700911106     O                       §CUC          Z   +  2
040800911106     O                       KNMUS             +  2
040900911106      *
041000911106     O          E            TESTA       0
041100911106     O                                         +  0 'DISTINTA N°'
041200911106     O                       §NMI          Z   +  1
041300911106     O                                         +  2 'DEL'
041400941020     O                       GMADMI            +  2 '  /  /    '
041500120322     O                                         +  2 'FILIALE'
041600911106     O                       §FLE          Z   +  2
041700911106     O                       DFLE              +  2
041800911106     O                                         +  2 'OPERATORE'
041900911106     O                       §CUC          Z   +  2
042000911106     O                       KNMUS             +  2
042100911106      *
042200941021     O          E            TESTA            05
042300911106     O                       TRATT             +  0
042400911106      *
042500941021     O          E            TESTA            06
042600941020     O                                            4 'Par.'
042700941020     O                                            8 'Arr.'
042800941020     O                                           13 'Anno'
042900941020     O                                           24 'N° Spediz.'
043000950119     O                                           37 'Data assegno'
043100950119     O                                           49 'Importo'
043200950119     O                                           57 'Tp.'
043300950119     O                                           61 'Div'
043400941020     O                                         +  1 'Mittente'
043500941020     O                                           95 'Riferim.'
043600941020     O                                          109 'N° Assegno'
043700941020     O                                          115 'Banca'
043800911106      *
043900941021     O          E            TESTA            07
044000911106     O                       TRATT             +  0
044100911106      *
044200911106     O          E            DETTA       2
044300110914     O                       arblnp        Z      4
044400110914     O                       arblna        Z      8
044500110914     O                       arbaas        Z     13
044600110914     O                       arbnrs        Z     16
044700110914     O                       arbnsp        Z     24
044800941020     O                       GMADTE              35 '  /  /    '
044900110914     O                       £cas          2     53
045000110914     O                       £TPA                55
045100110914     O                       £TPI                57
045200110914     O                       £VCA                61
045300941020     O                       RSM20               82
045400941020     O                       ARBRMN        Z     98
045500110914     O                       £nra               109
045600941020     O                       §DESBA             130
045700110914     O          E            DETca       1
045800110914     O                       asscas        2     53
045801120322     O                                           61 '<--'
045802120322     O                                         +  1 'Importo c/ass.'
045900110914     O          E            DETERR      1
046000930409     O                                         +  1 'ATT.NE LA PRECEDENTE'
046100930409     O                                         +  1 'SPEDIZIONE HA INCASSO'
046200930409     O                                         +  1 'ANOMALO: AVVISARE C.E.D.'
046300930409     O                                         +  1 'SEDE !!!!!'
046400911106      *
046500911106     O          E            TOTALE      2
046600911106     O                       TRATT             +  0
046700911106      *
046800060414     O          E            TOTALE      2
046900991018     O                                         + 70 'Totale assegni'
047000991018     O                                         +  0 ' Mittente EURO'
047100991115     O                       §TOTEU        2    125
047200941019     O          E            TOTALE      1
047300941019     O                                         + 70 'Totale assegni'
047400941019     O                                         +  0 ' Mittente Val.'
047500991115     O                       §TOTMV        2    125
047600991018     O          E            TOTALE      1
047700110914     O                                         + 70 'Numero assegni'
047800110907     O                       §TOTBE        2    125
047900120222     O          E            TOTALE      3
048000120222     O                                         + 70 'Totale spedizioni'
048100120222     O                                         +  0 ' Mittente EURO'
048200120222     O                       §TOTEUS       2    125
048300120222     O          E            TOTALE      1
048400120222     O                                         + 70 'Totale spedizioni'
048500120222     O                                         +  0 ' Mittente Val.'
048600120222     O                       §TOTMVS       2    125
048700120222     O          E            TOTALE      1
048800120222     O                                         + 70 'Numero spedizioni'
048900120222     O                       §TOTBES       2    125
049000950208**  CMDP
049100950208OVRPRTF FILE(QSYSPRT) USRDTA(Ch.Op_Mitt) SAVE(*YES)
