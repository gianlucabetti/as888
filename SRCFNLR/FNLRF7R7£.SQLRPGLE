000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300090224      * FNLRF7R7*                                                    *
000400950315      *---------*                                                    *
000500090227      *         - spedizioni squadrate in controllo aut. -           *
000501090227      *         - part. cons. NO PARZIALI                -           *
000600000426      *                                                              *
000700090227      *  Il programma elenca le spedizioni parz. messe in consegna   *
000800090227      *  con part.NO PARZIALI, elaborate in fase di quadratura       *
000900090227      *  autotrasportatore, oppure anche se non parziali ma con anom.*
001800950315      *--------------------------------------------------------------*
001900090224     FFNLRf7D7  CF   E             WORKSTN SFILE(LRf7S01:NRR3)
001901090227     F                                     SFILE(LRf7S06:NRR6)
002006090302     ffnarb01l  if   e           k disk    usropn
002008090302     f                                     extfile(fileovr)
002603090302     ffiapd01l  if   e           k disk
002604090302     ftabel00f  if   e           k disk
002700090402     D CMD             S             70    DIM(9) CTDATA PERRCD(1)              ERRORI
002701090302     D PAR             S              2    DIM(50)
002702090302     D PARC            S              2    DIM(50)
002703090302     D cli             S              7  0 DIM(100)
002704090302     D ncli            S              3  0
002705090302     D WrkSqlCmd       S           1024
002706090302     d fileovr         s             20
002707090302     d dtaiso          s               d
002708090302     d dtaeur          s               d   datfmt(*eur)
002709090302     D TIBS55        E DS                  EXTNAME(TIBS55DS)
004100950315     D KPJBA         E DS
004117080515     D ddatiute      e ds
004118080515     D azuteds       e ds                  extname(AZUTE00F)
004119090302     D fnlv50ds      E DS                  inz
004120080515     D tibs34ds      E DS                  inz
004121090302     D ds7R          e ds
004122090302     D ds7U          e ds
004123090302     dtibs69ds       e ds
004124090302     dcnaco          e ds                  extname(cnaco00f)
004125090302     dcnind          e ds                  extname(cnind00f)
004126090302     dcnclp          e ds                  extname(cnclp00f)
004127090302     dFncls          e ds                  extname(Fncls00f)
004128080701      * DS per richiamo pgm FNLR36R
004129080701     D DSLR36          DS
004130080701     D  P36AAS                14     17  0
004131080701     D  P36LNP                18     20  0
004132080701     D  P36NRS                21     22  0
004133080701     D  P36NSP                23     29  0
004134080701     D  P36F03                30     30
004135080701     D  P36FLG                31     31
004138090302     D wf7ds           DS
004139090302     D  waas                   1      4  0
004140090302     D  wlnp                   5      7  0
004141090302     D  wnrs                   8      9  0
004142090302     D  wnsp                  10     16  0
004143090302     D  wfgs                  17     19  0
004144090302     D  wnfv                  20     25  0
004145090302     D  wpdc                  26     32  0
004146090302     D  wtim                  33     46  0
004147090302     D  wminnsc               47     53  0
004148090302     D  wmaxnsc               54     60  0
004149090302     D WLBDAT          DS
004150090302     D  G02DAT                 1      8  0
004151090302     D  G02INV                 9     16  0
004152090302     D  G02ERR                17     17
004153090302     D  G02TGI                18     22  0
004700090302      *---------------------------------------------------------------*
004800950315      * FLUSSO PRINCIPALE                                             *
004900950315      *---------------------------------------------------------------*
004901090224     c                   exsr      inzd01
004902090224     c                   do        *hival
004903090224     c                   exfmt     lrf7d01
004904090224      *  Fine Lavoro
004905090224     C     *INKC         IFEQ      '1'
004906090224     C                   leave
004907090224     C                   END
004908090224     c* video parametri
004909090224     c                   exsr      srd01
004910090224     c   28              iter
004911090227     c* parzializzazione clienti
004912090302     c                   clear                   cli
004913090302     c                   clear                   ncli
004914090227     c     *inkh         ifeq      '1'
004915090227     c                   exsr      srs06
004916090227     c                   end
004917090224     c* subfile errori
004918090224     c                   exsr      srs01
004919090224     C     *INKC         IFEQ      '1'
004920090224     C                   leave
004921090224     C                   END
004922090224     c                   enddo
004923090224     c                   seton                                        lr
004924090224     C*--------------------------------------------------------------*
004925090224     C*  Inizializzazione parametri prima videata
004926090224     C*--------------------------------------------------------------*
004927090224     C     INZD01        BEGSR
004928090224     C*
004929090224     C                   SETOFF                                       284043
004930090224     C                   SETOFF                                       4445
004931090302     c                   if        dutpou = 046
004932090302     c                   clear                   v1clnp
004933090302     c                   else
004935090302     C                   MOVEL     dutpou        V1Clnp
004936090302     c                   end
004937090224     c                   move      *date         v1cdta
004938090224     c                   clear                   v1cora
004939090227     c                   eval      v1ccan = 'S'
004940090227     c                   eval      v1csqua= 'S'
004943090224     C*
004944090224     C                   ENDSR
005003090224     C*--------------------------------------------------------------*
005004090224     C*  Controlli prima videata
005005090224     C*--------------------------------------------------------------*
005006090224     C     srd01         BEGSR
005007090224     c                   setoff                                       284043
005008090224     c                   setoff                                       44
005009090227     c                   do
005010090227     C* filiale partenza obbligatoria per filiale
005011090302     c                   if        v1clnp = 0 and dutpou <> 046
005012090227     C                   SETON                                        2840
005013090227     C                   eval      $MSG  = 'Filiale partenza obbligatoria'
005014090227     c                   iter
005015090227     c                   end
005016090304     c                   if        v1clnp <> 0 and dutpou <> 046
005017090302     C                   CLEAR                   FNLV50ds
005018090224     C                   MOVEL     KNMUS         D50PRU
005019090302     C                   MOVEL     V1Clnp        D50FGS
005020090224     C                   CALL      'FNLV50R'
005021090302     C                   PARM                    FNLV50ds
005022090224     C     D50ERR        IFNE      ' '
005023090224     C                   SETON                                        2840
005024090224     C                   MOVEL     D50MSG        $MSG
005025090224     C                   leave
005026090302     C                   ENDIF
005027090224     C                   ENDIF
005044090224     C*  Data elaborazione
005045090224     C     V1CDta        IFEQ      0
005046090224     C                   SETON                                        4428
005047090224     C                   eval      $MSG  = 'Data elaborazione errata'
005048090224     C                   leave
005049090224     C                   ELSE
005050090224     C                   MOVE      V1CDta        G02DAT
005051090224     C                   MOVEL     *BLANKS       G02ERR
005052090224     C                   CALL      'XSRDA8'
005053090224     C                   PARM                    WLBDAT
005054090224     C     G02ERR        IFEQ      '1'
005055090224     C                   SETON                                        2844
005056090224     C                   eval      $MSG  = 'Data elaborazione errata'
005057090224     C                   leave
005058090224     C                   END
005059090224     C                   END
005060090302     C                   movel     g02inv        WDta              8 0
005062090224     C                   Z-ADD     G02DAT        V1CDta
005063090227     c* errore se scelta no squadrate e no anomalie
005064090227     c                   if        v1csqua = 'N' and v1ccan = 'N'
005065090227     C                   SETON                                        28
005066090227     C                   eval      $MSG  = 'Scegliere almeno uno dei -
005067090227     C                             due tipi spedizioni'
005068090227     C                   leave
005069090227     c                   end
005136090224     C                   ENDdo
005137090224     C*
005138090224     C                   ENDSR
005139090224      *---------------------------------------------------------------*
005140090224     c     srs01         begsr
005141090224      *---------------------------------------------------------------*
005500090227     C*  Caricamento subfile anomalie e squadrature
005600080508     C                   EXSR      INZS01
005601090302     c*  Se il subfile delle squadrature
005603090224     C     NRR3          IFeq      0
005604090302     c                   setoff                                       20
005607080526     C                   END
005608090224     c*
005620080512     C                   Z-ADD     1             NRR3
005700950315      *  Loop gestione subfile degli errori
005800080508     C                   DO        *hival
005801080508      *
005802090224     C                   WRITE     LRf7Z01
005803090224     C                   EXFMT     LRf7C01
005808080508      *  Fine Lavoro
005809080508     C     *INKC         IFEQ      '1'
005812080508     C                   leave
005813080508     C                   END
005814080508      *  Guida
005815080711     C     *INKL         IFEQ      '1'
005817080508     C                   leave
005818080508     C                   END
005819080701      *  Interrogazione bolle
005820080701     C     *INKG         IFEQ      '1'
005821090224     c                   clear                   dslr36
005827080701     C                   MOVEL     DSLR36        KPJBU
005828080701     C                   CALL      'FNLR36R'
005829080701     C                   PARM                    KPJBA
005830080701     C                   END
005831080508      *  Loop di controlli
005832080508     C                   EXSR      CTR01
006200080508     C                   ENDdo
006201080508     c*
006202090224     c                   endsr
007300950317      *---------------------------------------------------------------*
007400950317      * Gestione videata subfile errori                               *
007500950317      *---------------------------------------------------------------*
007600080508     C     INZS01        BEGSR
007800090224     C                   SETOFF                                       2021
007900090224     C                   WRITE     LRf7C01
008000950317     C                   SETON                                        2021
008400080512     C                   Z-ADD     0             NRR3              4 0
008401090302     c                   movel     wdta          comela           14
008402090227     c                   move      v1cora        comela
008403090227     c* compongo la stringa sql
008404090227     c                   exsr      sristruz
008405090227     C/EXEC SQL
008406090227     C+ PREPARE S1 FROM :WrkSqlCmd
008407090227     C/END-EXEC
008408090227
008409090227     C/EXEC SQL
008410090227     C+ DECLARE A1 CURSOR FOR S1
008411090227     C/END-EXEC
008412090227
008413090227     C/EXEC SQL
008414090227     C+ OPEN A1
008415090227     C/END-EXEC
008482090224
008483090224     C                   DOU       SqlCod <> 0
008484090224     C/EXEC SQL
008485090227     C+ FETCH NEXT FROM A1 INTO :wf7ds
008486090224     C/END-EXEC
008487090224     c*
008488090224     C                   SELECT
008489090224     C                   WHEN      SqlCod = 0
008490090227     c*
008494090227     c     karb          chain     fnarb01l
008495090304     c                   if        not %found(fnarb01l)
008496090304     c                   clear                   arbksc
008497090304     c                   clear                   arbrsm
008498090304     c                   clear                   arbrsd
008499090304     c                   end
008500090302     c* verifico se scelta parz.clienti
008501090302     c                   setoff                                       11
008502090302     c                   if        ncli <> 0
008503090302     c     arbksc        lookup    cli                                    11
008504090302     c                   if        not *in11
008505090302     c                   iter
008506090302     c                   end
008507090302     c                   end
008508090224     c*
008509090224     c     kapd          chain     fiapd01l
008510090304     c                   if        not %found(fiapd01l)
008511090304     c                   clear                   apdrsf
008514090304     c                   end
031418090302     c* se conta > 1 vuol dire che ho + record per spedizione e quindi
031419090302     c* ho anomalie a livello di colli
031420090302     c                   clear                   v3ccan
031421090302     c                   if        wmaxnsc <> 0 and v1ccan = 'S'
031423090302     c                   eval      v3ccan = 'S'
031425090302     c                   end
031426090302     c                   if        wmaxnsc = 0 and v1ccan = 'S'
031427090302     c                   eval      v3ccan = 'N'
031428090302     c                   end
031429090302     c* ho squadrature a livello di spedizione
031430090302     c                   clear                   v3csqua
031431090302     c                   if        wminnsc = 0 and v1csqua = 'S'
031432090302     c                   eval      v3csqua = 'S'
031433090302     c                   end
031434090302     c                   if        wminnsc <> 0 and v1csqua = 'S'
031435090302     c                   eval      v3csqua = 'N'
031436090302     c                   end
031437090302     c                   eval      v3caas = waas
031438090302     c                   eval      v3clnp = wlnp
031439090302     c                   eval      v3cnrs = wnrs
031440090302     c                   eval      v3cnsp = wnsp
031441090302     c                   eval      v3cfgs = wfgs
031442090302     c                   eval      v3cnfv = wnfv
031443090302     c                   movel     wtim          com08             8 0
031444090302     c                   movel     com08         dtaiso
031445090302     c                   movel     dtaiso        dtaeur
031446090302     c                   movel     dtaeur        v3cdta
031447090302     c                   move      wtim          v3cora
031448090227     c                   eval      v3crsd = arbrsd
031449090227     c                   eval      v3crsm = arbrsm
031450090302     c                   eval      v3dpdr = apdrsf
031451090227     c
031452090224     C                   ADD       1             NRR3
031453090224     C                   WRITE     LRf7S01
031454090224     C                   other
031455090224     c                   leave
031456090224     c                   endsl
031457090302     c                   enddo
031458090302     C/EXEC SQL
031459090302     C+ CLOSE A1
031460090302     C/END-EXEC
031461090302     c*
031462090227     c                   endsr
031471090302      *---------------------------------------------------------------*
031472090302      *  istruzione sql
031473090302      *---------------------------------------------------------------*
031474090302     C     sristruz      BEGSR
031475090302     C                   EVAL      %SUBST(CMD(3): 31: 14) = COMELA
031476090302     c                   eval      wrksqlcmd = cmd(1) + ' ' +
031477090302     c                             cmd(2) + ' ' + cmd(3)
031478090227     c* filiale partenza
031479090227     c                   if        v1clnp <> 0
031480090302     C                   MOVEL     V1CLNP        COMLNP            3
031481090302     C                   EVAL      %SUBST(CMD(6): 14: 3) = COMLNP
031482090302     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
031483090302     c                             ' ' + cmd(6)
031484090227     c                   end
031485090402     c* serie bolle
031486090402     c                   if        v1cnrs <> 0
031487090402     C                   MOVEL     V1Cnrs        COMnrs            3
031488090402     C                   EVAL      %SUBST(CMD(9): 14: 3) = COMnrs
031489090402     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
031490090402     c                             ' ' + cmd(9)
031491090402     c                   end
031492090227     c* bolle con squadratura e no anomalie
031493090227     c                   if        v1csqua = 'S' and v1ccan = 'N'
031494090302     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
031495090302     c                             ' ' + cmd(7)
031496090227     c                   end
031497090227     c* bolle non squadrate ma con anomalie
031498090227     c                   if        v1csqua = 'N' and v1ccan = 'S'
031499090302     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
031500090302     c                             ' ' + cmd(8)
031501090227     c                   end
031502090302     c                   eval      wrksqlcmd = %trim(wrksqlcmd) +
031503090302     c                             ' ' + cmd(4) + ' ' + cmd(5)
031504090227     c                   endsr
045715080508      *---------------------------------------------------------------*
045716080508      *  Loop di errori  subfile errori                               *
045717080508      *---------------------------------------------------------------*
045718080509     C     CTR01         BEGSR
045720950315      *
045900090224     C                   READC     LRf7S01                                32
046000980325      *
046100950317     C     *IN32         DOWEQ     '0'
050803080509      *  Interrogazione bolla
050804090302     C                   if        v3csce = 'V'
051000950317     C                   MOVEL     '1'           P36FLG
051100950317     C                   Z-ADD     V3CAAS        P36AAS
051200950317     C                   Z-ADD     V3CLNP        P36LNP
051300950317     C                   Z-ADD     V3CNSP        P36NSP
051400950317     C                   Z-ADD     V3CNRS        P36NRS
051500080509     C                   MOVEL(P)  DSLR36        KPJBU
051600950317     C                   CALL      'FNLR36R'
051700080701     c                   PARM                    KPJBA
051701080730     c                   MOVEL     ' '           v3CSCE
051702090302     c                   end
056900980325      *  Aggiorno subfile
057000950320     C                   SETON                                        22
057200090224     C                   UPDATE    LRf7s01
057300950320     C                   SETOFF                                       22
057301090224     C                   READC     LRf7S01                                32
058200080509     C                   ENDdo
058201080715     c*
058202080715     c                   z-add     1             nrr3
059800950317     C*
059900950317     C                   ENDSR
059901090227      *---------------------------------------------------------------*
059902090227     c     srs06         begsr
059903090227      *---------------------------------------------------------------*
059904090227     C*  Caricamento subfile parz. clienti
059905090227     C                   EXSR      INZS06
059911090227     c*
059912090227     C                   Z-ADD     1             NRR6
059913090227      *  Loop gestione subfile
059914090227     C                   DO        *hival
059915090227      *
059916090227     C                   WRITE     LRf7Z06
059917090227     C                   EXFMT     LRf7C06
059922090227      *  Guida
059923090227     C     *INKL         IFEQ      '1'
059924090227     C                   leave
059925090227     C                   END
059933090227      *  Loop di controlli
059934090227     C                   EXSR      CTR06
059935090302     c   28              iter
059936090227     C                   ENDdo
059937090227     c*
059938090227     c                   endsr
059939090227      *---------------------------------------------------------------*
059940090227      * Gestione subfile parz. clienti                                *
059941090227      *---------------------------------------------------------------*
059942090227     C     INZS06        BEGSR
059943090302     C                   SETOFF                                       626327
059944090302     C                   SETOFF                                       28
059945090227     C                   WRITE     LRf7C06
059946090227     C                   SETON                                        6263
059947090227     C                   Z-ADD     0             NRR6              4 0
059948090227     c                   do        100
059949090227     c                   clear                   SFWCLI
059950090227     c                   clear                   SFWCLID
059988090227     C                   ADD       1             NRR6
059989090227     C                   WRITE     LRf7S06
059990090227     c                   enddo
059992090302     c                   endsr
059993090227      *---------------------------------------------------------------*
059994090227      *  Loop di controllo subfile parz. clienti                      *
059995090227      *---------------------------------------------------------------*
059996090227     C     CTR06         BEGSR
059997090302     c                   setoff                                       28
060000090227      *
060001090227     C                   READC     LRf7S06                                32
060002090227      *
060003090227     C     *IN32         DOWEQ     '0'
060004090302     c                   setoff                                       27
060005090227     c*
060006090227     C     '?'           SCAN      sfwCLI                                 33
060007090227     C     *IN33         IFEQ      '1'
060008090227     C                   SETON                                        90
060009090227     C                   CALL      'TRTB71R1'
060010090227     C                   PARM                    KNSIF
060011090227     C                   PARM                    PCLI              8
060012090227     C                   PARM                    PRAG             25
060013090227     C                   MOVEL     PCLI          sfwCLI
060014090302     C                   MOVEL     Prag          sfwCLId
060015090302     c                   end
060016090302     c*
060017090302     c                   if        sfwcli <> *zero and sfwcli <> ' '
060018090302     C                   CLEAR                   TIBS69ds
060019090302     C                   CLEAR                   CNACO
060020090302     C                   CLEAR                   CNIND
060021090302     C                   CLEAR                   CNCLP
060022090302     C                   CLEAR                   FNCLS
060023090302     C                   MOVEL     ' '           I69TLA
060024090302     C                   MOVEL     KNSIF         I69SIF
060025090302     C                   MOVE      sfwcli        I69KAC
060026090302     C                   MOVE      sfwcli        I69Kcp
060027090302     C                   CALL      'TIBS69R'
060028090302     C                   PARM                    TIBS69ds
060029090302     C                   PARM                    CNACO
060030090302     C                   PARM                    CNIND
060031090302     C                   PARM                    CNCLP
060032090302     C                   PARM                    FNCLS
060033090302     C                   if        o69err <>*blanks
060034090302     C                   SETON                                        2728
060035090302     C                   eval      $MSG6 = 'Cliente errato'
060036090302     C                   EVAL      sfwclid = *ALL'?'
060037090302     C                   ELSE
060038090302     C                   MOVEL     ACORAG        sfwclid
060039090302     c                   end
060040090302     c                   else
060041090302     c                   clear                   sfwclid
060042090302     c                   end
060054090227     c* verifico se il cliente è NO PARZ.
060055090302     c                   if        sfwcli <> *zero and sfwcli <> ' '
060056090227     c                   movel(p)  '7U'          tblcod
060058090302     c                   eval      tblkey = sfwcli + 'R'
060059090227     c     ktab          chain     tabel00f
060060090227     c                   if        not %found(tabel00f)
060061090227     C                   SETON                                        2728
060062090302     C                   eval      $MSG6= 'Cliente non abilitato alla -
060063090227     C                             partic.Cons.'
060064090227     c                   else
060065090227     c                   movel     tbluni        ds7u
060066090227     c                   movea     §7ucpa        parC
060067090227     c                   clear                   ok                1 0
060068090302     c                   do        30            xx                2 0
060069090227     c     parc(xx)      lookup    par                                    11
060070090227     c                   if        *in11
060078090227     c                   eval      ok = 1
060079090302     c                   add       1             ncli
060080090302     c                   movel     sfwcli        cli(ncli)
060081090227     c                   leave
060082090227     c                   end
060083090227     c                   enddo
060084090227     c                   if        ok = 0
060085090227     C                   SETON                                        2728
060086090302     C                   eval      $MSG6= 'Cliente non abilitato alla -
060087090227     C                             partic.Cons.'
060088090227     c                   end
060089090227     c                   end
060090090302     c                   end
060091090227      *  Aggiorno subfile
060092090302     C                   SETOn                                        23
060093090302     C                   UPDATE    LRf7s06
060094090302     C                   SETOFF                                       23
060095090302     C                   READC     LRf7S06                                32
060096090227     C                   ENDdo
060097090227     c*
060098090227     c                   z-add     1             nrr6
060099090227     C*
060100090227     C                   ENDSR
060257080509     C*-----------------------------------------------------*
060258080509     C     *INZSR        BEGSR
060259080509     C*-----------------------------------------------------*
060260080508      *
060261080508     C     *ENTRY        PLIST
060262090302     C                   PARM                    kpjba
060263080515     C* Decodifica Elaboratore
060264080515     c     *dtaara       define    §azute        azuteds
060265080515     c     *dtaara       define    §datiute      ddatiute
060266080515     C                   in(E)     *dtaara
060267080515     C                   IF        %Error  or  RSUT = *blanks
060268080515     C                   call      'TIBS34R'
060269080515     C                   parm                    Tibs34Ds
060270080515     C                   in        *dtaara
060271080515     c                   ENDIF
060272090304     c     dutpou        comp      046                                    46
060273090302     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
060274090302     C* da/a cui spedire
060275090302     C                   CLEAR                   TIBS55
060276090302     C                   MOVEL     'L'           I50TLA
060277090302     C                   MOVEL     KNSIF         I50PSI
060278090302     C                   MOVEL     dutpou        I50PPO
060279090302     C                   MOVEL     '001'         I50APO
060280090302     C                   CALL      'TIBS55R'
060281090302     C                   PARM                    TIBS55
060282090302     c                   eval      fileovr=o50ala + '/FNARB01L'
060283090302     c                   open      fnarb01l
060284090302     c     kapd          klist
060285090302     c                   kfld                    apdtip
060286090302     c                   kfld                    wpdc
060287090302     c                   eval      apdtip = 'A'
060288090302     c     karb          klist
060289090302     c                   kfld                    waas
060290090302     c                   kfld                    wlnp
060291090302     c                   kfld                    wnrs
060292090302     c                   kfld                    wnsp
060293090302     c     ktab          klist
060294090302     c                   kfld                    tblkut
060295090302     c                   kfld                    tblcod
060296090302     c                   kfld                    tblkey
060376090227     c* carico part. consegne NO Parz.
060378090227     C                   Z-ADD     0             X                 3 0
060379090227     C                   Z-ADD     1             tblkut
060380090227     C                   eval      tblcod = '7R'
060381090227     C     ktab          setll     Tabel00f
060382090227     C                   DO        *hival
060383090227     C     ktab          READE     TABEL00F
060384090227     c                   if        %eof(tabel00f)
060385090227     c                   leave
060386090227     c                   end
060387090227     C                   MOVEL     TBLUNI        DS7R
060388090227     c                   if        §7r8cp = 'N'
060389090227     C                   ADD       1             X
060390090227     C                   MOVEL     TBlkey        par(X)
060391090227     c                   end
060392090227     C                   ENDdo
060393080515     c*
060394080509     C                   ENDSR
071100090227**
071200090302SELECT WF7AAS, WF7LNP, WF7NRS, WF7NSP, WF7FGS, WF7NFV, wf7pdc,
071301090302max(WF7TIM), min(wf7nsc), max(wf7nsc)
071302090303from fnlrf70f WHERE WF7tim >= 00000000000000
071500090302GROUP BY WF7AAS, WF7LNP, WF7NRS, WF7NSP, WF7FGS, WF7NFV, wf7pdc
071600090302ORDER BY WF7FGS, WF7NFV, WF7AAS, WF7LNP, WF7NRS, WF7NSP, wf7pdc
071701090302and WF7LNP = 000
071800090302and WF7nsc = 0
071900090302and WF7nsc <> 0
072000090402and WF7nrs = 00
