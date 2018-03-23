000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300090224      * FNLRF7R7*                                                    *
000400950315      *---------*                                                    *
000500090227      *         - spedizioni squadrate in controllo aut. -           *
000600090227      *         - part. cons. NO PARZIALI                -           *
000700000426      *                                                              *
000800090227      *  Il programma elenca le spedizioni parz. messe in consegna   *
000900090227      *  con part.NO PARZIALI, elaborate in fase di quadratura       *
001000090227      *  autotrasportatore, oppure anche se non parziali ma con anom.*
001100950315      *--------------------------------------------------------------*
001200090224     FFNLRf7D7  CF   E             WORKSTN SFILE(LRf7S01:NRR3)
001300090227     F                                     SFILE(LRf7S06:NRR6)
001400090302     ffnarb01l  if   e           k disk    usropn
001500090302     f                                     extfile(fileovr)
001600090302     ffiapd01l  if   e           k disk
001700090302     ftabel00f  if   e           k disk
001800090402     D CMD             S             70    DIM(9) CTDATA PERRCD(1)              ERRORI
001900090302     D PAR             S              2    DIM(50)
002000090302     D PARC            S              2    DIM(50)
002100090302     D cli             S              7  0 DIM(100)
002200090302     D ncli            S              3  0
002300090302     D WrkSqlCmd       S           1024
002400090302     d fileovr         s             20
002500090302     d dtaiso          s               d
002600090302     d dtaeur          s               d   datfmt(*eur)
002700090302     D TIBS55        E DS                  EXTNAME(TIBS55DS)
002800950315     D KPJBA         E DS
002900080515     D ddatiute      e ds
003000080515     D azuteds       e ds                  extname(AZUTE00F)
003100090302     D fnlv50ds      E DS                  inz
003200080515     D tibs34ds      E DS                  inz
003300090302     D ds7R          e ds
003400090302     D ds7U          e ds
003500090302     dtibs69ds       e ds
003600090302     dcnaco          e ds                  extname(cnaco00f)
003700090302     dcnind          e ds                  extname(cnind00f)
003800090302     dcnclp          e ds                  extname(cnclp00f)
003900090302     dFncls          e ds                  extname(Fncls00f)
004000080701      * DS per richiamo pgm FNLR36R
004100080701     D DSLR36          DS
004200080701     D  P36AAS                14     17  0
004300080701     D  P36LNP                18     20  0
004400080701     D  P36NRS                21     22  0
004500080701     D  P36NSP                23     29  0
004600080701     D  P36F03                30     30
004700080701     D  P36FLG                31     31
004800090302     D wf7ds           DS
004900090302     D  waas                   1      4  0
005000090302     D  wlnp                   5      7  0
005100090302     D  wnrs                   8      9  0
005200090302     D  wnsp                  10     16  0
005300090302     D  wfgs                  17     19  0
005400090302     D  wnfv                  20     25  0
005500090302     D  wpdc                  26     32  0
005600090302     D  wtim                  33     46  0
005700090302     D  wminnsc               47     53  0
005800090302     D  wmaxnsc               54     60  0
005900090302     D WLBDAT          DS
006000090302     D  G02DAT                 1      8  0
006100090302     D  G02INV                 9     16  0
006200090302     D  G02ERR                17     17
006300090302     D  G02TGI                18     22  0
006400090302      *---------------------------------------------------------------*
006500950315      * FLUSSO PRINCIPALE                                             *
006600950315      *---------------------------------------------------------------*
006700090224     c                   exsr      inzd01
006800090224     c                   do        *hival
006900090224     c                   exfmt     lrf7d01
007000090224      *  Fine Lavoro
007100090224     C     *INKC         IFEQ      '1'
007200090224     C                   leave
007300090224     C                   END
007400090224     c* video parametri
007500090224     c                   exsr      srd01
007600090224     c   28              iter
007700090227     c* parzializzazione clienti
007800090302     c                   clear                   cli
007900090302     c                   clear                   ncli
008000090227     c     *inkh         ifeq      '1'
008100090227     c                   exsr      srs06
008200090227     c                   end
008300090224     c* subfile errori
008400090224     c                   exsr      srs01
008500090224     C     *INKC         IFEQ      '1'
008600090224     C                   leave
008700090224     C                   END
008800090224     c                   enddo
008900090224     c                   seton                                        lr
009000090224     C*--------------------------------------------------------------*
009100090224     C*  Inizializzazione parametri prima videata
009200090224     C*--------------------------------------------------------------*
009300090224     C     INZD01        BEGSR
009400090224     C*
009500090224     C                   SETOFF                                       284043
009600090224     C                   SETOFF                                       4445
009700090302     c                   if        dutpou = 046
009800090302     c                   clear                   v1clnp
009900090302     c                   else
010000090302     C                   MOVEL     dutpou        V1Clnp
010100090302     c                   end
010200090224     c                   move      *date         v1cdta
010300090224     c                   clear                   v1cora
010400090227     c                   eval      v1ccan = 'S'
010500090227     c                   eval      v1csqua= 'S'
010600090224     C*
010700090224     C                   ENDSR
010800090224     C*--------------------------------------------------------------*
010900090224     C*  Controlli prima videata
011000090224     C*--------------------------------------------------------------*
011100090224     C     srd01         BEGSR
011200090224     c                   setoff                                       284043
011300090224     c                   setoff                                       44
011400090227     c                   do
011500090227     C* filiale partenza obbligatoria per filiale
011600090302     c                   if        v1clnp = 0 and dutpou <> 046
011700090227     C                   SETON                                        2840
011800090227     C                   eval      $MSG  = 'Filiale partenza obbligatoria'
011900090227     c                   iter
012000090227     c                   end
012100090304     c                   if        v1clnp <> 0 and dutpou <> 046
012200090302     C                   CLEAR                   FNLV50ds
012300090224     C                   MOVEL     KNMUS         D50PRU
012400090302     C                   MOVEL     V1Clnp        D50FGS
012500090224     C                   CALL      'FNLV50R'
012600090302     C                   PARM                    FNLV50ds
012700090224     C     D50ERR        IFNE      ' '
012800090224     C                   SETON                                        2840
012900090224     C                   MOVEL     D50MSG        $MSG
013000090224     C                   leave
013100090302     C                   ENDIF
013200090224     C                   ENDIF
013300090224     C*  Data elaborazione
013400090224     C     V1CDta        IFEQ      0
013500090224     C                   SETON                                        4428
013600090224     C                   eval      $MSG  = 'Data elaborazione errata'
013700090224     C                   leave
013800090224     C                   ELSE
013900090224     C                   MOVE      V1CDta        G02DAT
014000090224     C                   MOVEL     *BLANKS       G02ERR
014100090224     C                   CALL      'XSRDA8'
014200090224     C                   PARM                    WLBDAT
014300090224     C     G02ERR        IFEQ      '1'
014400090224     C                   SETON                                        2844
014500090224     C                   eval      $MSG  = 'Data elaborazione errata'
014600090224     C                   leave
014700090224     C                   END
014800090224     C                   END
014900090302     C                   movel     g02inv        WDta              8 0
015000090224     C                   Z-ADD     G02DAT        V1CDta
015100090227     c* errore se scelta no squadrate e no anomalie
015200090227     c                   if        v1csqua = 'N' and v1ccan = 'N'
015300090227     C                   SETON                                        28
015400090227     C                   eval      $MSG  = 'Scegliere almeno uno dei -
015500090227     C                             due tipi spedizioni'
015600090227     C                   leave
015700090227     c                   end
015800090224     C                   ENDdo
015900090224     C*
016000090224     C                   ENDSR
016100090224      *---------------------------------------------------------------*
016200090224     c     srs01         begsr
016300090224      *---------------------------------------------------------------*
016400090227     C*  Caricamento subfile anomalie e squadrature
016500080508     C                   EXSR      INZS01
016600090302     c*  Se il subfile delle squadrature
016700090224     C     NRR3          IFeq      0
016800090302     c                   setoff                                       20
016900080526     C                   END
017000090224     c*
017100080512     C                   Z-ADD     1             NRR3
017200950315      *  Loop gestione subfile degli errori
017300080508     C                   DO        *hival
017400080508      *
017500090224     C                   WRITE     LRf7Z01
017600090224     C                   EXFMT     LRf7C01
017700080508      *  Fine Lavoro
017800080508     C     *INKC         IFEQ      '1'
017900080508     C                   leave
018000080508     C                   END
018100080508      *  Guida
018200080711     C     *INKL         IFEQ      '1'
018300080508     C                   leave
018400080508     C                   END
018500080701      *  Interrogazione bolle
018600080701     C     *INKG         IFEQ      '1'
018700090224     c                   clear                   dslr36
018800080701     C                   MOVEL     DSLR36        KPJBU
018900080701     C                   CALL      'FNLR36R'
019000080701     C                   PARM                    KPJBA
019100080701     C                   END
019200080508      *  Loop di controlli
019300131126     C   20              EXSR      CTR01
019400080508     C                   ENDdo
019500080508     c*
019600090224     c                   endsr
019700950317      *---------------------------------------------------------------*
019800950317      * Gestione videata subfile errori                               *
019900950317      *---------------------------------------------------------------*
020000080508     C     INZS01        BEGSR
020100090224     C                   SETOFF                                       2021
020200090224     C                   WRITE     LRf7C01
020300950317     C                   SETON                                        2021
020400080512     C                   Z-ADD     0             NRR3              4 0
020500090302     c                   movel     wdta          comela           14
020600090227     c                   move      v1cora        comela
020700090227     c* compongo la stringa sql
020800090227     c                   exsr      sristruz
020900090227     C/EXEC SQL
021000090227     C+ PREPARE S1 FROM :WrkSqlCmd
021100090227     C/END-EXEC
021200090227
021300090227     C/EXEC SQL
021400090227     C+ DECLARE A1 CURSOR FOR S1
021500090227     C/END-EXEC
021600090227
021700090227     C/EXEC SQL
021800090227     C+ OPEN A1
021900090227     C/END-EXEC
022000090224
022100090224     C                   DOU       SqlCod <> 0
022200090224     C/EXEC SQL
022300090227     C+ FETCH NEXT FROM A1 INTO :wf7ds
022400090224     C/END-EXEC
022500090224     c*
022600090224     C                   SELECT
022700090224     C                   WHEN      SqlCod = 0
022800090227     c*
022900090227     c     karb          chain     fnarb01l
023000090304     c                   if        not %found(fnarb01l)
023100090304     c                   clear                   arbksc
023200090304     c                   clear                   arbrsm
023300090304     c                   clear                   arbrsd
023400090304     c                   end
023500090302     c* verifico se scelta parz.clienti
023600090302     c                   setoff                                       11
023700090302     c                   if        ncli <> 0
023800090302     c     arbksc        lookup    cli                                    11
023900090302     c                   if        not *in11
024000090302     c                   iter
024100090302     c                   end
024200090302     c                   end
024300090224     c*
024400090224     c     kapd          chain     fiapd01l
024500090304     c                   if        not %found(fiapd01l)
024600090304     c                   clear                   apdrsf
024700090304     c                   end
024800090302     c* se conta > 1 vuol dire che ho + record per spedizione e quindi
024900090302     c* ho anomalie a livello di colli
025000090302     c                   clear                   v3ccan
025100090302     c                   if        wmaxnsc <> 0 and v1ccan = 'S'
025200090302     c                   eval      v3ccan = 'S'
025300090302     c                   end
025400090302     c                   if        wmaxnsc = 0 and v1ccan = 'S'
025500090302     c                   eval      v3ccan = 'N'
025600090302     c                   end
025700090302     c* ho squadrature a livello di spedizione
025800090302     c                   clear                   v3csqua
025900090302     c                   if        wminnsc = 0 and v1csqua = 'S'
026000090302     c                   eval      v3csqua = 'S'
026100090302     c                   end
026200090302     c                   if        wminnsc <> 0 and v1csqua = 'S'
026300090302     c                   eval      v3csqua = 'N'
026400090302     c                   end
026500090302     c                   eval      v3caas = waas
026600090302     c                   eval      v3clnp = wlnp
026700090302     c                   eval      v3cnrs = wnrs
026800090302     c                   eval      v3cnsp = wnsp
026900090302     c                   eval      v3cfgs = wfgs
027000090302     c                   eval      v3cnfv = wnfv
027100090302     c                   movel     wtim          com08             8 0
027200090302     c                   movel     com08         dtaiso
027300090302     c                   movel     dtaiso        dtaeur
027400090302     c                   movel     dtaeur        v3cdta
027500090302     c                   move      wtim          v3cora
027600090227     c                   eval      v3crsd = arbrsd
027700090227     c                   eval      v3crsm = arbrsm
027800090302     c                   eval      v3dpdr = apdrsf
027900090227     c
028000090224     C                   ADD       1             NRR3
028100090224     C                   WRITE     LRf7S01
028200090224     C                   other
028300090224     c                   leave
028400090224     c                   endsl
028500090302     c                   enddo
028600090302     C/EXEC SQL
028700090302     C+ CLOSE A1
028800090302     C/END-EXEC
028900090302     c*
029000090227     c                   endsr
029100090302      *---------------------------------------------------------------*
029200090302      *  istruzione sql
029300090302      *---------------------------------------------------------------*
029400090302     C     sristruz      BEGSR
029500090302     C                   EVAL      %SUBST(CMD(3): 31: 14) = COMELA
029600090302     c                   eval      wrksqlcmd = cmd(1) + ' ' +
029700090302     c                             cmd(2) + ' ' + cmd(3)
029800090227     c* filiale partenza
029900090227     c                   if        v1clnp <> 0
030000090302     C                   MOVEL     V1CLNP        COMLNP            3
030100090302     C                   EVAL      %SUBST(CMD(6): 14: 3) = COMLNP
030200090302     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
030300090302     c                             ' ' + cmd(6)
030400090227     c                   end
030500090402     c* serie bolle
030600090402     c                   if        v1cnrs <> 0
030700090402     C                   MOVEL     V1Cnrs        COMnrs            3
030800090402     C                   EVAL      %SUBST(CMD(9): 14: 3) = COMnrs
030900090402     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
031000090402     c                             ' ' + cmd(9)
031100090402     c                   end
031200090227     c* bolle con squadratura e no anomalie
031300090227     c                   if        v1csqua = 'S' and v1ccan = 'N'
031400090302     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
031500090302     c                             ' ' + cmd(7)
031600090227     c                   end
031700090227     c* bolle non squadrate ma con anomalie
031800090227     c                   if        v1csqua = 'N' and v1ccan = 'S'
031900090302     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
032000090302     c                             ' ' + cmd(8)
032100090227     c                   end
032200090302     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
032300090302     c                             ' ' + cmd(4) + ' ' + cmd(5)
032400090227     c                   endsr
032500080508      *---------------------------------------------------------------*
032600080508      *  Loop di errori  subfile errori                               *
032700080508      *---------------------------------------------------------------*
032800080509     C     CTR01         BEGSR
032900950315      *
033000090224     C                   READC     LRf7S01                                32
033100980325      *
033200950317     C     *IN32         DOWEQ     '0'
033300080509      *  Interrogazione bolla
033400090302     C                   if        v3csce = 'V'
033500950317     C                   MOVEL     '1'           P36FLG
033600950317     C                   Z-ADD     V3CAAS        P36AAS
033700950317     C                   Z-ADD     V3CLNP        P36LNP
033800950317     C                   Z-ADD     V3CNSP        P36NSP
033900950317     C                   Z-ADD     V3CNRS        P36NRS
034000080509     C                   MOVEL(P)  DSLR36        KPJBU
034100950317     C                   CALL      'FNLR36R'
034200080701     c                   PARM                    KPJBA
034300080730     c                   MOVEL     ' '           v3CSCE
034400090302     c                   end
034500980325      *  Aggiorno subfile
034600950320     C                   SETON                                        22
034700090224     C                   UPDATE    LRf7s01
034800950320     C                   SETOFF                                       22
034900090224     C                   READC     LRf7S01                                32
035000080509     C                   ENDdo
035100080715     c*
035200080715     c                   z-add     1             nrr3
035300950317     C*
035400950317     C                   ENDSR
035500090227      *---------------------------------------------------------------*
035600090227     c     srs06         begsr
035700090227      *---------------------------------------------------------------*
035800090227     C*  Caricamento subfile parz. clienti
035900090227     C                   EXSR      INZS06
036000090227     c*
036100090227     C                   Z-ADD     1             NRR6
036200090227      *  Loop gestione subfile
036300090227     C                   DO        *hival
036400090227      *
036500090227     C                   WRITE     LRf7Z06
036600090227     C                   EXFMT     LRf7C06
036700090227      *  Guida
036800090227     C     *INKL         IFEQ      '1'
036900090227     C                   leave
037000090227     C                   END
037100090227      *  Loop di controlli
037200090227     C                   EXSR      CTR06
037300090302     c   28              iter
037400090227     C                   ENDdo
037500090227     c*
037600090227     c                   endsr
037700090227      *---------------------------------------------------------------*
037800090227      * Gestione subfile parz. clienti                                *
037900090227      *---------------------------------------------------------------*
038000090227     C     INZS06        BEGSR
038100090302     C                   SETOFF                                       626327
038200090302     C                   SETOFF                                       28
038300090227     C                   WRITE     LRf7C06
038400090227     C                   SETON                                        6263
038500090227     C                   Z-ADD     0             NRR6              4 0
038600090227     c                   do        100
038700090227     c                   clear                   SFWCLI
038800090227     c                   clear                   SFWCLID
038900090227     C                   ADD       1             NRR6
039000090227     C                   WRITE     LRf7S06
039100090227     c                   enddo
039200090302     c                   endsr
039300090227      *---------------------------------------------------------------*
039400090227      *  Loop di controllo subfile parz. clienti                      *
039500090227      *---------------------------------------------------------------*
039600090227     C     CTR06         BEGSR
039700090302     c                   setoff                                       28
039800090227      *
039900090227     C                   READC     LRf7S06                                32
040000090227      *
040100090227     C     *IN32         DOWEQ     '0'
040200090302     c                   setoff                                       27
040300090227     c*
040400090227     C     '?'           SCAN      sfwCLI                                 33
040500090227     C     *IN33         IFEQ      '1'
040600090227     C                   SETON                                        90
040700090227     C                   CALL      'TRTB71R1'
040800090227     C                   PARM                    KNSIF
040900090227     C                   PARM                    PCLI              8
041000090227     C                   PARM                    PRAG             25
041100090227     C                   MOVEL     PCLI          sfwCLI
041200090302     C                   MOVEL     Prag          sfwCLId
041300090302     c                   end
041400090302     c*
041500090302     c                   if        sfwcli <> *zero and sfwcli <> ' '
041600090302     C                   CLEAR                   TIBS69ds
041700090302     C                   CLEAR                   CNACO
041800090302     C                   CLEAR                   CNIND
041900090302     C                   CLEAR                   CNCLP
042000090302     C                   CLEAR                   FNCLS
042100090302     C                   MOVEL     ' '           I69TLA
042200090302     C                   MOVEL     KNSIF         I69SIF
042300090302     C                   MOVE      sfwcli        I69KAC
042400090302     C                   MOVE      sfwcli        I69Kcp
042500090302     C                   CALL      'TIBS69R'
042600090302     C                   PARM                    TIBS69ds
042700090302     C                   PARM                    CNACO
042800090302     C                   PARM                    CNIND
042900090302     C                   PARM                    CNCLP
043000090302     C                   PARM                    FNCLS
043100090302     C                   if        o69err <>*blanks
043200090302     C                   SETON                                        2728
043300090302     C                   eval      $MSG6 = 'Cliente errato'
043400090302     C                   EVAL      sfwclid = *ALL'?'
043500090302     C                   ELSE
043600090302     C                   MOVEL     ACORAG        sfwclid
043700090302     c                   end
043800090302     c                   else
043900090302     c                   clear                   sfwclid
044000090302     c                   end
044100090227     c* verifico se il cliente è NO PARZ.
044200090302     c                   if        sfwcli <> *zero and sfwcli <> ' '
044300090227     c                   movel(p)  '7U'          tblcod
044400090302     c                   eval      tblkey = sfwcli + 'R'
044500090227     c     ktab          chain     tabel00f
044600090227     c                   if        not %found(tabel00f)
044700090227     C                   SETON                                        2728
044800090302     C                   eval      $MSG6= 'Cliente non abilitato alla -
044900090227     C                             partic.Cons.'
045000090227     c                   else
045100090227     c                   movel     tbluni        ds7u
045200090227     c                   movea     §7ucpa        parC
045300090227     c                   clear                   ok                1 0
045400090302     c                   do        30            xx                2 0
045500090227     c     parc(xx)      lookup    par                                    11
045600090227     c                   if        *in11
045700090227     c                   eval      ok = 1
045800090302     c                   add       1             ncli
045900090302     c                   movel     sfwcli        cli(ncli)
046000090227     c                   leave
046100090227     c                   end
046200090227     c                   enddo
046300090227     c                   if        ok = 0
046400090227     C                   SETON                                        2728
046500090302     C                   eval      $MSG6= 'Cliente non abilitato alla -
046600090227     C                             partic.Cons.'
046700090227     c                   end
046800090227     c                   end
046900090302     c                   end
047000090227      *  Aggiorno subfile
047100090302     C                   SETOn                                        23
047200090302     C                   UPDATE    LRf7s06
047300090302     C                   SETOFF                                       23
047400090302     C                   READC     LRf7S06                                32
047500090227     C                   ENDdo
047600090227     c*
047700090227     c                   z-add     1             nrr6
047800090227     C*
047900090227     C                   ENDSR
048000080509     C*-----------------------------------------------------*
048100080509     C     *INZSR        BEGSR
048200080509     C*-----------------------------------------------------*
048300080508      *
048400080508     C     *ENTRY        PLIST
048500090302     C                   PARM                    kpjba
048600080515     C* Decodifica Elaboratore
048700080515     c     *dtaara       define    §azute        azuteds
048800080515     c     *dtaara       define    §datiute      ddatiute
048900080515     C                   in(E)     *dtaara
049000080515     C                   IF        %Error  or  RSUT = *blanks
049100080515     C                   call      'TIBS34R'
049200080515     C                   parm                    Tibs34Ds
049300080515     C                   in        *dtaara
049400080515     c                   ENDIF
049500090304     c     dutpou        comp      046                                    46
049600090302     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
049700090302     C* da/a cui spedire
049800090302     C                   CLEAR                   TIBS55
049900090302     C                   MOVEL     'L'           I50TLA
050000090302     C                   MOVEL     KNSIF         I50PSI
050100090302     C                   MOVEL     dutpou        I50PPO
050200090302     C                   MOVEL     '001'         I50APO
050300090302     C                   CALL      'TIBS55R'
050400090302     C                   PARM                    TIBS55
050500090302     c                   eval      fileovr=o50ala + '/FNARB01L'
050600090302     c                   open      fnarb01l
050700090302     c     kapd          klist
050800090302     c                   kfld                    apdtip
050900090302     c                   kfld                    wpdc
051000090302     c                   eval      apdtip = 'A'
051100090302     c     karb          klist
051200090302     c                   kfld                    waas
051300090302     c                   kfld                    wlnp
051400090302     c                   kfld                    wnrs
051500090302     c                   kfld                    wnsp
051600090302     c     ktab          klist
051700090302     c                   kfld                    tblkut
051800090302     c                   kfld                    tblcod
051900090302     c                   kfld                    tblkey
052000090227     c* carico part. consegne NO Parz.
052100090227     C                   Z-ADD     0             X                 3 0
052200090227     C                   Z-ADD     1             tblkut
052300090227     C                   eval      tblcod = '7R'
052400090227     C     ktab          setll     Tabel00f
052500090227     C                   DO        *hival
052600090227     C     ktab          READE     TABEL00F
052700090227     c                   if        %eof(tabel00f)
052800090227     c                   leave
052900090227     c                   end
053000090227     C                   MOVEL     TBLUNI        DS7R
053100090227     c                   if        §7r8cp = 'N'
053200090227     C                   ADD       1             X
053300090227     C                   MOVEL     TBlkey        par(X)
053400090227     c                   end
053500090227     C                   ENDdo
053600080515     c*
053700080509     C                   ENDSR
053800090227**
053900090302SELECT WF7AAS, WF7LNP, WF7NRS, WF7NSP, WF7FGS, WF7NFV, wf7pdc,
054000090302max(WF7TIM), min(wf7nsc), max(wf7nsc)
054100090303from fnlrf70f WHERE WF7tim >= 00000000000000
054200090302GROUP BY WF7AAS, WF7LNP, WF7NRS, WF7NSP, WF7FGS, WF7NFV, wf7pdc
054300090302ORDER BY WF7FGS, WF7NFV, WF7AAS, WF7LNP, WF7NRS, WF7NSP, wf7pdc
054400090302and WF7LNP = 000
054500090302and WF7nsc = 0
054600090302and WF7nsc <> 0
054700090402and WF7nrs = 00
