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
006300120314     d rifass          ds
006400120314     D §nra                                like(assnra)
006500120314     D §dte                                like(assdte)
006600120314     D §abi                                like(assabi)
006700120314     D §cab                                like(asscab)
006800120314     D §key                                like(csskey)
006900110914     d §TOTbe          s              3  0
007000110914     d §TOTMV          s                   like(asscas)
007100110914     d §TOTEU          s                   like(asscas)
007200120222     d §TOTbes         s              3  0
007300120222     d §TOTMVs         s                   like(asscas)
007400120222     d §TOTEUs         s                   like(asscas)
007500120314     d §cas            s                   like(asscas)
007600120222     D §vca            s                   like(assvca)
007700120314     d savrif          s             38
007800900524      *
007900911106      * APRO FILE DI STAMPA
008000911106     C                   MOVEA     CMDP          CMDPX            60
008100911106     C                   Z-ADD     60            LUNCMD           15 5
008200950208     C                   CALL      'QCMDEXC'                            91
008300911106     C                   PARM                    CMDPX
008400911106     C                   PARM                    LUNCMD
008500941020     C                   OPEN      QSYSPRT
008600110908     c                   seton                                        of
008700120222     c* prima di cominciare calcolo il totale c/assegni perchè poi il
008800120222     c* CNASS sarà deletato, devo farlo prima perchè altrimenti potrei
008900120222     c* contare lo stesso c/assegno più volte nel caso di una spedizione
009000120222     c* con + assegni
009100120314     c                   exsr      srtotali
009200911106      *
009300110913     C/EXEC SQL
009400110913     C+ DECLARE b1 CURSOR FOR
009500110914     C+ select arbaas, arblnp, arbnrs, arbnsp, arblna, arbrsm, arbrmn,
009600110914     C+ arbccm, arbksc,
009700120118     C+         ifnull(csmtpi, asstpi) £tpi,
009800120118     C+         ifnull(csmtpa, asstpa) £tpa,
009900110914     C+         asscas, rrn(cnass00f),
010000120118     C+         ifnull(csmnra, assnra) £nra,
010100120118     C+         ifnull(csmdte, assdte) £dte,
010200120118     C+         ifnull(csmabi, assabi) £abi,
010300120118     C+         ifnull(csmcab, asscab) £cab,
010400120118     C+         ifnull(csmidc, asscas) £cas,
010500120125     C+         ifnull(csmdiv, assvca) £vca,
010600120125     C+         ifnull(csmkey, ' ') £key
010700110913     C+ from cnass00F join fnarb00f on
010800110913     C+ assaas = arbaas and arblnp=asslnp and
010900110913     C+ arbnrs=assnrs and arbnsp=assnsp
011000110913     C+ join fiar900f on
011100110913     C+ assaas = ar9aas and ar9lnp=asslnp and
011200110913     C+ ar9nrs=assnrs and ar9nsp=assnsp
011300120118     C+ left outer join tncsm00f on
011400120118     C+ assnra = csmkey
011500110913     C+ WHERE
011600110914     C+ asstpi = 'M' and assfle = :§fle
011700110913     C+ and arbnmi = :§nmi and arbdmi = :§dmi
011800110913     C+ and arbipp= :§fle
011900110914     C+ order by £cas, £abi, £cab, £nra, £dte
012000110914     C/END-EXEC
012100110913     C/EXEC SQL
012200110913     C+ OPEN b1
012300110913     C/END-EXEC
012400110913     C                   do        *hival
012500110913
012600110913     C/EXEC SQL
012700110913     C+ FETCH NEXT FROM b1 INTO
012800110914     C+ :arbaas, :arblnp, :arbnrs, :arbnsp, :arblna,
012900110914     C+ :arbrsm, :arbrmn, :arbccm, :arbksc,
013000110914     C+ :£tpi, :£tpa, :asscas, :nrr, :£nra, :£dte,
013100120125     C+ :£abi, :£cab, :£cas, :£vca, :£key
013200110913     C/END-EXEC
013300110913
013400110913     C                   select
013500110913     C                   when      sqlcod = 100
013600110913     c                   leave
013700110913     C                   WHEN      SqlCod < 0
013800110913     c                   if        sqlcod <> -913
013900110913     C                   seton                                        H1
014000110913     c                   end
014100110913     c                   seton                                        lr
014200110913     c                   return
014300110913     C                   other
014400110913     c                   seton                                        10
014500110913     c                   exsr      srstampa
014600110914     c     nrr           delete    cnass00f
014700120125     c* cancello l'eventuale record nel file delle spedizioni perchè in
014800120125     c* sede non serve inquanto verrà memorizzato nel TNCSB
014900120125     c                   if        £key <> ' '
015000120801     C/EXEC SQL
015100120801     C+ DELETE FROM FNCSS03L WHERE csskey = :£key
015200120801     C/END-EXEC
015300120125     c                   end
015400110913     C                   endsl
015500110913     C                   enddo
015600110913     C/EXEC SQL
015700110913     C+ CLOSE b1
015800110913     C/END-EXEC
015900010928      *
016000110907     C     §TOTBE        IFNE      *ZEROS
016100011003     C     §TOTEU        ORNE      *ZEROS
016200941019     C     §TOTMV        ORNE      *ZEROS
016300911106     C                   EXCEPT    TOTALE
016400930521     C                   END
016500910521     C                   SETON                                        LR
016600110901     C**********************************************************************
016700120314     c     srtotali      begsr
016800110901     C**********************************************************************
016900120314     c                   clear                   §totmv
017000120314     c                   clear                   §toteu
017100120314     c                   clear                   §totbe
017200120314     c* assegni singoli
017300120314     C/EXEC SQL
017400120314     C+ DECLARE a1 CURSOR FOR
017500120315     C+ SELECT assvca, asscas, arbccm, arbksc from
017600120314     C+ cnass00F join fnarb00f on
017700120314     C+ assaas = arbaas and arblnp=asslnp and
017800120314     C+ arbnrs=assnrs and arbnsp=assnsp exception join fncss00f on
017900120314     C+ assaas = cssaao and arblnp=csslno and
018000120314     C+ arbnrs=cssnro and arbnsp=cssnso
018100120314     C+ WHERE asstpi = 'M' and assfle = :§fle and arbnmi = :§nmi
018200120314     C+ and arbdmi = :§dmi and arbipp= :§fle
018300120314     C/END-EXEC
018400120314     c*
018500120314     C/EXEC SQL
018600120314     C+ OPEN a1
018700120314     C/END-EXEC
018800120314     C                   do        *hival
018900120314
019000120314     C/EXEC SQL
019100120314     C+ FETCH NEXT FROM a1 INTO
019200120314     C+ :§vca, :§cas, :arbccm, : arbksc
019300120314     C/END-EXEC
019400120314
019500120314     C                   select
019600120314     C                   when      sqlcod = 100
019700120314     c                   leave
019800120314     C                   WHEN      SqlCod < 0
019900120314     c                   if        sqlcod <> -913
020000120314     C                   seton                                        H1
020100120314     c                   end
020200120314     c                   seton                                        lr
020300120314     c                   return
020400120314     C                   other
020500120314     C                   CLEAR                   DSCV
020600120314     C     §vca          IFNE      *BLANK
020700120314     C                   Z-ADD     1             X                 3 0
020800120314     C     §vca          LOOKUP    TCV(X)                                 22
020900120314     C   22              MOVEL     DCV(X)        DSCV
021000120314     C                   ELSE
021100120314     C                   MOVEL     'I'           §CVITA
021200120314     C                   ENDIF
021300120314     C*  CONTROLLO SE CLIENTE ABILITATO "BB" OPPURE NO
021400120314     C                   SETOFF                                       40
021500120314     C     §CVITA        IFEQ      'I'                                          LIRE
021600120314     C     §vca          OREQ      'EUR'
021700120314     C     arbccm        IFNE      0
021800120314     C     arbccm        LOOKUP    COD                                    40
021900120314     C                   ELSE
022000120314     C     arbksc        LOOKUP    COD                                    40
022100120314     C                   END
022200120314     C                   END
022300120314      * NON "BB"
022400120314     C     *IN40         IFEQ      '0'
022500120314     C                   Z-ADD     1             O                 4 0
022600120314     C     'EUR'         LOOKUP    TCV(O)                                 14
022700120314     C                   MOVEL     DCV(O)        DSCV
022800120314     c                   add       1             §totbe
022900120314     C     §vca          IFEQ      'EUR'
023000120314     C     *IN14         ANDEQ     *ON
023100120314     C     §CVDVA        ANDEQ     'S'
023200120314     C                   ADD       §cas          §TOTEU
023300120314     C                   ELSE
023400120314     C                   ADD       §cas          §TOTMV
023500120314     C                   ENDIF
023600120314     C                   END
023700120314     C                   endsl
023800120314     C                   enddo
023900120314     C/EXEC SQL
024000120314     C+ CLOSE a1
024100120314     C/END-EXEC
024200120314     c* assegni multipli
024300120314     C/EXEC SQL
024400120314     C+ DECLARE a2 CURSOR FOR
024500120314     C+ SELECT csmdiv, csmidc, arbccm, arbksc,
024600120314     C+ csmnra, csmdte, csmabi, csmcab from
024700120314     C+ cnass00F join fnarb00f on
024800120314     C+ assaas = arbaas and arblnp=asslnp and
024900120314     C+ arbnrs=assnrs and arbnsp=assnsp join tncsm00f on
025000120314     C+ assnra = csmkey
025100120314     C+ WHERE asstpi = 'M' and assfle = :§fle and arbnmi = :§nmi
025200120314     C+ and arbdmi = :§dmi and arbipp= :§fle
025300120314     c+ order by csmkey, csmnra, csmdte, csmabi, csmcab
025400120314     C/END-EXEC
025500120314     c*
025600120314     C/EXEC SQL
025700120314     C+ OPEN a2
025800120314     C/END-EXEC
025900120314     C                   do        *hival
026000120314
026100120314     C/EXEC SQL
026200120314     C+ FETCH NEXT FROM a2 INTO
026300120314     C+ :§vca, :§cas, :arbccm, :arbksc, :§nra, :§dte, :§abi, :§cab, :§key
026400120314     C/END-EXEC
026500120314
026600120314     C                   select
026700120314     C                   when      sqlcod = 100
026800120314     c                   leave
026900120314     C                   WHEN      SqlCod < 0
027000120314     c                   if        sqlcod <> -913
027100120314     C                   seton                                        H1
027200120314     c                   end
027300120314     c                   seton                                        lr
027400120314     c                   return
027500120314     C                   other
027600120314     c* l'assegno un
027700120314     c                   if        rifass = savrif
027800120314     c                   iter
027900120314     c                   else
028000120314     c                   eval      savrif = rifass
028100120314     c                   end
028200120314     C                   CLEAR                   DSCV
028300120314     C     §vca          IFNE      *BLANK
028400120314     C                   Z-ADD     1             X                 3 0
028500120314     C     §vca          LOOKUP    TCV(X)                                 22
028600120314     C   22              MOVEL     DCV(X)        DSCV
028700120314     C                   ELSE
028800120314     C                   MOVEL     'I'           §CVITA
028900120314     C                   ENDIF
029000120314     C*  CONTROLLO SE CLIENTE ABILITATO "BB" OPPURE NO
029100120314     C                   SETOFF                                       40
029200120314     C     §CVITA        IFEQ      'I'                                          LIRE
029300120314     C     §vca          OREQ      'EUR'
029400120314     C     arbccm        IFNE      0
029500120314     C     arbccm        LOOKUP    COD                                    40
029600120314     C                   ELSE
029700120314     C     arbksc        LOOKUP    COD                                    40
029800120314     C                   END
029900120314     C                   END
030000120314      * NON "BB"
030100120314     C     *IN40         IFEQ      '0'
030200120314     C                   Z-ADD     1             O                 4 0
030300120314     C     'EUR'         LOOKUP    TCV(O)                                 14
030400120314     C                   MOVEL     DCV(O)        DSCV
030500120314     c                   add       1             §totbe
030600120314     C     §vca          IFEQ      'EUR'
030700120314     C     *IN14         ANDEQ     *ON
030800120314     C     §CVDVA        ANDEQ     'S'
030900120314     C                   ADD       §cas          §TOTEU
031000120314     C                   ELSE
031100120314     C                   ADD       §cas          §TOTMV
031200120314     C                   ENDIF
031300120314     C                   END
031400120314     C                   endsl
031500120314     C                   enddo
031600120314     C/EXEC SQL
031700120314     C+ CLOSE a2
031800120314     C/END-EXEC
031900120314     c                   clear                   §totmvs
032000120314     c                   clear                   §toteus
032100120314     c                   clear                   §totbes
032200120314     C/EXEC SQL
032300120314     C+ DECLARE a3 CURSOR FOR
032400120314     C+ SELECT assvca, asscas, arbccm, arbksc from
032500120314     C+ cnass00F join fnarb00f on
032600120314     C+ assaas = arbaas and arblnp=asslnp and
032700120314     C+ arbnrs=assnrs and arbnsp=assnsp
032800120314     C+ WHERE asstpi = 'M' and assfle = :§fle and arbnmi = :§nmi
032900120314     C+ and arbdmi = :§dmi and arbipp= :§fle
033000120314     C/END-EXEC
033100120314     c*
033200120314     C/EXEC SQL
033300120314     C+ OPEN a3
033400120314     C/END-EXEC
033500120314     C                   do        *hival
033600120314
033700120314     C/EXEC SQL
033800120314     C+ FETCH NEXT FROM a3 INTO
033900120314     C+ :§vca, :§cas, :arbccm, :arbksc
034000120314     C/END-EXEC
034100120314
034200120314     C                   select
034300120314     C                   when      sqlcod = 100
034400120314     c                   leave
034500120314     C                   WHEN      SqlCod < 0
034600120314     c                   if        sqlcod <> -913
034700120314     C                   seton                                        H1
034800120314     c                   end
034900120314     c                   seton                                        lr
035000120314     c                   return
035100120314     C                   other
035200120314     C                   CLEAR                   DSCV
035300120314     C     §vca          IFNE      *BLANK
035400120314     C                   Z-ADD     1             X                 3 0
035500120314     C     §vca          LOOKUP    TCV(X)                                 22
035600120314     C   22              MOVEL     DCV(X)        DSCV
035700120314     C                   ELSE
035800120314     C                   MOVEL     'I'           §CVITA
035900120314     C                   ENDIF
036000120314     C*  CONTROLLO SE CLIENTE ABILITATO "BB" OPPURE NO
036100120314     C                   SETOFF                                       40
036200120314     C     §CVITA        IFEQ      'I'                                          LIRE
036300120314     C     §vca          OREQ      'EUR'
036400120314     C     arbccm        IFNE      0
036500120314     C     arbccm        LOOKUP    COD                                    40
036600120314     C                   ELSE
036700120314     C     arbksc        LOOKUP    COD                                    40
036800120314     C                   END
036900120314     C                   END
037000120314      * NON "BB"
037100120314     C     *IN40         IFEQ      '0'
037200120314     C                   Z-ADD     1             O                 4 0
037300120314     C     'EUR'         LOOKUP    TCV(O)                                 14
037400120314     C                   MOVEL     DCV(O)        DSCV
037500120314     c                   add       1             §totbes
037600120314     C     §vca          IFEQ      'EUR'
037700120314     C     *IN14         ANDEQ     *ON
037800120314     C     §CVDVA        ANDEQ     'S'
037900120314     C                   ADD       §cas          §TOTEUs
038000120314     C                   ELSE
038100120314     C                   ADD       §cas          §TOTMVs
038200120314     C                   ENDIF
038300120314     C                   END
038400120314     C                   endsl
038500120314     C                   enddo
038600120314     C/EXEC SQL
038700120314     C+ CLOSE a3
038800120314     C/END-EXEC
038900120314     c                   endsr
039000120314     C**********************************************************************
039100120314     c     srstampa      begsr
039200120314     C**********************************************************************
039300110913     c* campi in stampa
039400110914     C                   MOVEL     arbrsm        RSM20            20
039500110914     C                   Z-ADD     £dte          G08INV
039600110901     C                   MOVE      *ZEROS        G08DAT
039700110901     C                   MOVEL     '3'           G08ERR
039800110901     C                   CALL      'XSRDA8'
039900110901     C                   PARM                    WLBDA8
040000110901     C                   Z-ADD     G08DAT        GMADTE            8 0
040100110913     c*
040200110901     C     KABI          CHAIN     CNABI01L                           21
040300120627     C* N21ABIANN        COMP      *BLANK                             2121
040400110901     C  N21              MOVEL     ABIIST        §DESBA           20
040500110901     C   21              MOVEL     *ALL'?'       §DESBA
040600110913     c*
040700110901     c                   if        *inof
040800110901     C                   EXCEPT    TESTA
040900110908     C                   SETOff                                       of
041000110901     c                   end
041100110913     c*
041200110913     c                   if        *in10
041300110914     C     £cas          IFEQ      *ZEROS
041400110901     C                   EXCEPT    DETERR
041500110913     c                   else
041600110913     C                   EXCEPT    DETTA
041700110914     C     £cas          IFne      asscas
041800110914     C                   EXCEPT    DETca
041900110901     C                   END
042000110914     C                   END
042100110913     C                   END
042200110913     c                   setoff                                       10
042300110913     c*
042400110901     C                   ENDsr
042500110901     C**********************************************************************
042600941021     C     *INZSR        BEGSR
042700110901     C**********************************************************************
042800000000     C     *ENTRY        PLIST
042900000000     C                   PARM                    KPJBA
043000911106     C                   MOVEL     KPJBU         PARAM
043100911106     C                   MOVE      PFLE          §FLE              3 0
043200911106     C                   MOVE      PCUC          §CUC              3 0
043300911106     C                   MOVE      PNMI          §NMI              7 0
043400110914     c                   if        pdmi <> ' '
043500110914     C                   MOVE      PdMI          §dMI              8 0
043600110914     c                   else
043700110914     c                   clear                   §dmi
043800110914     c                   end
043900110914     C                   Z-ADD     §dmi          G08INV
044000110913     C                   MOVE      *ZEROS        G08DAT
044100110913     C                   MOVEL     '3'           G08ERR
044200110913     C                   CALL      'XSRDA8'
044300110913     C                   PARM                    WLBDA8
044400110913     C                   Z-ADD     G08DAT        GMADMI            8 0
044500900516      *
044600911106     C                   MOVEL     *ALL'-'       TRATT           132
044700000000     C                   Z-ADD     1             CODUT
044800900517     C                   CALL      'X§PARUT'
044900900517     C                   PARM                    UT§DSE
045000110914     C     CODUT         ifeq      -1
045100110914     c                   seton                                        lr
045200110914     c                   return
045300110914     c                   end
045400000000     C                   MOVEL     RAGUT         RSUT             20
045500971125     C                   MOVEL     RAGUT         RCONT            17
045600910708     C                   MOVEL     REC80         CNCR80
045700941020     C*§§
045800941020     C                   TIME                    WHHUDT           14 0
045900941020     C                   MOVEL     WHHUDT        ORA               6 0
046000941020     C                   MOVE      WHHUDT        G08DAT
046100941020     C                   MOVE      *ZEROS        G08INV
046200941020     C                   MOVEL     *BLANK        G08ERR
046300941020     C                   CALL      'XSRDA8'
046400941020     C                   PARM                    WLBDA8
046500941020     C                   Z-ADD     G08INV        AMGUDT            8 0
046600941020     C                   Z-ADD     G08DAT        GMAUDT            8 0
046700941020     C*§§
046800911031     C     §FLE          CHAIN     AZORG01L                           21
046900911106     C  N21              MOVEL     ORGDES        DFLE             20
047000911030     C*
047100900515      *---------------------------------------------------------------*
047200900515      * CHIAVI                                                        *
047300900515      *---------------------------------------------------------------*
047400911106     C     KABI          KLIST
047500110914     C                   KFLD                    £ABI
047600110914     C                   KFLD                    £CAB
047700931001     C*
047800931001     C*  DEFINISCO CHIAVE PER POSIZIONAMENTO SU TABEL00F
047900931001     C     KTAB          KLIST
048000931001     C                   KFLD                    TBLKUT
048100931001     C                   KFLD                    TBLCOD
048200931001     C*
048300931001     C                   Z-ADD     1             TBLKUT
048400931001     C                   MOVE      'TM'          TBLCOD
048500931001     C*
048600931001     C*  LETTURA RECORD TABELLA TM PER RICONOSCERE E MEMOR. IN SCHIERA
048700931001     C*  CLIENTI ABILITATI AL "BB"
048800931001     C                   Z-ADD     0             X
048900931005     C     KTAB          CHAIN     TABEL00F                           70
049000931001     C*
049100931001     C     *IN70         DOWEQ     '0'
049200931001     C                   MOVEL     TBLUNI        DSTM
049300931001     C     §TMFBB        IFEQ      'S'
049400940406     C     §TMFVE        ANDEQ     ' '
049500931001     C                   ADD       1             X
049600931005     C                   MOVEL     TBLKEY        COD(X)
049700931001     C                   END
049800931001     C     KTAB          READE     TABEL00F                               70
049900931001     C                   END
050000941021      *
050100941021      * CARICO TUTTI I CODICI VALUTA  TAB.CV
050200941021     C                   MOVEL     'CV'          TBLCOD
050300941021     C                   MOVE      *ZEROS        X
050400941021     C     KTAB          SETLL     TABEL00F
050500941021     C     *IN70         DOUEQ     '1'
050600941021     C     KTAB          READE     TABEL00F                               70
050700950123     C  N70TBLFLG        IFEQ      ' '
050800950123     C                   ADD       1             X
050900950123     C                   MOVEL     TBLKEY        TCV(X)
051000950123     C                   MOVEL     TBLUNI        DCV(X)
051100950123     C                   END
051200941021     C                   END
051300931001     C*
051400910521     C                   ENDSR
051500900606      *----------------------------------------------------------------
051600941021     OQSYSPRT   E            TESTA            02
051700911106     O                       RSUT                20
051800170705     O******                 SIMFEL            +  1
051900170705     O                                           60 'INCASSI CONTRASSEGNI INTES'
052000120322     O                                         +  0 'TATI AL MITTENTE'
052100971125     O                       RCONT             +  1
052200941021     O                       §PGM              +  2
052300941021     O                       GMAUDT             122 '  /  /    '
052400911106     O                                          128 'PAG.'
052500911106     O                       PAGE               132
052600941021     O          E            TESTA            03
052700941021     O                       KNSIF             +  0
052800941021     O                       KNMUS             +  1
052900941021     O                       ORA                122 '  :  :  '
053000911106      *
053100941021     O          E            TESTA            04
053200911106     O                                         +  0 'DISTINTA N°'
053300911106     O                       §NMI          Z   +  1
053400911106     O                                         +  2 'DEL'
053500941020     O                       GMADMI            +  2 '  /  /    '
053600120322     O                                         +  2 'FILIALE'
053700911106     O                       §FLE          Z   +  2
053800911106     O                       DFLE              +  2
053900911106     O                                         +  2 'OPERATORE'
054000911106     O                       §CUC          Z   +  2
054100911106     O                       KNMUS             +  2
054200911106      *
054300911106     O          E            TESTA       0
054400911106     O                                         +  0 'DISTINTA N°'
054500911106     O                       §NMI          Z   +  1
054600911106     O                                         +  2 'DEL'
054700941020     O                       GMADMI            +  2 '  /  /    '
054800120322     O                                         +  2 'FILIALE'
054900911106     O                       §FLE          Z   +  2
055000911106     O                       DFLE              +  2
055100911106     O                                         +  2 'OPERATORE'
055200911106     O                       §CUC          Z   +  2
055300911106     O                       KNMUS             +  2
055400911106      *
055500941021     O          E            TESTA            05
055600911106     O                       TRATT             +  0
055700911106      *
055800941021     O          E            TESTA            06
055900941020     O                                            4 'Par.'
056000941020     O                                            8 'Arr.'
056100941020     O                                           13 'Anno'
056200941020     O                                           24 'N° Spediz.'
056300950119     O                                           37 'Data assegno'
056400950119     O                                           49 'Importo'
056500950119     O                                           57 'Tp.'
056600950119     O                                           61 'Div'
056700941020     O                                         +  1 'Mittente'
056800941020     O                                           95 'Riferim.'
056900941020     O                                          109 'N° Assegno'
057000941020     O                                          115 'Banca'
057100911106      *
057200941021     O          E            TESTA            07
057300911106     O                       TRATT             +  0
057400911106      *
057500911106     O          E            DETTA       2
057600110914     O                       arblnp        Z      4
057700110914     O                       arblna        Z      8
057800110914     O                       arbaas        Z     13
057900110914     O                       arbnrs        Z     16
058000110914     O                       arbnsp        Z     24
058100941020     O                       GMADTE              35 '  /  /    '
058200110914     O                       £cas          2     53
058300110914     O                       £TPA                55
058400110914     O                       £TPI                57
058500110914     O                       £VCA                61
058600941020     O                       RSM20               82
058700941020     O                       ARBRMN        Z     98
058800110914     O                       £nra               109
058900941020     O                       §DESBA             130
059000110914     O          E            DETca       1
059100110914     O                       asscas        2     53
059200120322     O                                           61 '<--'
059300120322     O                                         +  1 'Importo c/ass.'
059400110914     O          E            DETERR      1
059500930409     O                                         +  1 'ATT.NE LA PRECEDENTE'
059600930409     O                                         +  1 'SPEDIZIONE HA INCASSO'
059700930409     O                                         +  1 'ANOMALO: AVVISARE C.E.D.'
059800930409     O                                         +  1 'SEDE !!!!!'
059900911106      *
060000911106     O          E            TOTALE      2
060100911106     O                       TRATT             +  0
060200911106      *
060300060414     O          E            TOTALE      2
060400991018     O                                         + 70 'Totale assegni'
060500991018     O                                         +  0 ' Mittente EURO'
060600991115     O                       §TOTEU        2    125
060700941019     O          E            TOTALE      1
060800941019     O                                         + 70 'Totale assegni'
060900941019     O                                         +  0 ' Mittente Val.'
061000991115     O                       §TOTMV        2    125
061100991018     O          E            TOTALE      1
061200110914     O                                         + 70 'Numero assegni'
061300110907     O                       §TOTBE        2    125
061400120222     O          E            TOTALE      3
061500120222     O                                         + 70 'Totale spedizioni'
061600120222     O                                         +  0 ' Mittente EURO'
061700120222     O                       §TOTEUS       2    125
061800120222     O          E            TOTALE      1
061900120222     O                                         + 70 'Totale spedizioni'
062000120222     O                                         +  0 ' Mittente Val.'
062100120222     O                       §TOTMVS       2    125
062200120222     O          E            TOTALE      1
062300120222     O                                         + 70 'Numero spedizioni'
062400120222     O                       §TOTBES       2    125
062500950208**  CMDP
062600950208OVRPRTF FILE(QSYSPRT) USRDTA(Ch.Op_Mitt) SAVE(*YES)
