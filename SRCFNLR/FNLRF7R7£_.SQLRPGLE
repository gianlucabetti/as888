000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300090224      * FNLRF7R7*                                                    *
000400950315      *---------*                                                    *
000500090224      *         - spedizioni squadrate in contropllo aut -           *
000600000426      *                                                              *
000700090224      *  Il programma elenca le spedizioni squadrate con particol.   *
000800090224      *  consegna NO PARZIALI, elaborate in fase di quadratura       *
000900090224      *  autotrasportatore                                           *
001800950315      *--------------------------------------------------------------*
001900090224     FFNLRf7D7  CF   E             WORKSTN SFILE(LRf7S01:NRR3)
002006080707     ffnarb01l  if   e           k disk
002007080805     ffidst01l  if   e           k disk
002014080707     ftabel00f  if   e           k disk
002016080710     fazorg01l  if   e           k disk
002017090224     fwfnlrf7l  if   e           k disk
002400950320      *----------------------------------------------------------------*
002500950320      *    s c h i e r e                                               *
002600950320      *----------------------------------------------------------------*
002700980325     D ERR             S             70    DIM(7) CTDATA PERRCD(1)              ERRORI
004100950315     D KPJBA         E DS
004101080508     D fnlr53ds7     E DS
004103080728     D fidg46ds      E DS
004104080509     D fipctcetds    E DS
004105080707     D fipcdcedds    E DS
004106080707     D fnarbds       E DS                  extname(FNARB00F)
004107080707     D fipctds       E DS                  extname(Fipct00F)
004108080526     D fnarbsds      E DS                  prefix(£) extname(FNARBS0F)
004109080707     d trul06ds      e ds
004110080707     d  lin£                   1     90  0 dim(30)
004111080526     d ksped           ds
004112080526     d  kaas                          4  0
004113080526     d  klnp                          3  0
004114080526     d  knrs                          2  0
004115080526     d  knsp                          7  0
004116080515     D ddatiute      e ds
004117080515     D azuteds       e ds                  extname(AZUTE00F)
004118080515     D tibs34ds      E DS                  inz
004119080707     D ds2a          e ds
004120080711     D decd          e ds
004123080701      * DS per richiamo pgm FNLR36R
004124080701     D DSLR36          DS
004125080701     D  P36AAS                14     17  0
004126080701     D  P36LNP                18     20  0
004127080701     D  P36NRS                21     22  0
004128080701     D  P36NSP                23     29  0
004129080701     D  P36F03                30     30
004130080701     D  P36FLG                31     31
004700950315      *---------------------------------------------------------------*
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
004911090224     c* subfile errori
004912090224     c                   exsr      srs01
004913090224     C     *INKC         IFEQ      '1'
004914090224     C                   leave
004915090224     C                   END
004916090224     c                   enddo
004917090224     c                   seton                                        lr
004918090224     C*--------------------------------------------------------------*
004919090224     C*  Inizializzazione parametri prima videata
004920090224     C*--------------------------------------------------------------*
004921090224     C     INZD01        BEGSR
004922090224     C*
004923090224     C                   SETOFF                                       284043
004924090224     C                   SETOFF                                       4445
004935090224     C                   MOVEL     dutpou        V1CFIL
004936090224     c                   move      *date         v1cddc
004937090224     c                   move      *date         v1cdta
004938090224     c                   clear                   v1cora
004939090224     c                   clear                   v1ccan
004943090224     C*
004944090224     C                   ENDSR
005003090224     C*--------------------------------------------------------------*
005004090224     C*  Controlli prima videata
005005090224     C*--------------------------------------------------------------*
005006090224     C     srd01         BEGSR
005007090224     c                   setoff                                       284043
005008090224     c                   setoff                                       44
005009090224     C* filiale partenza
005010090224     C                   CLEAR                   FNLV50
005011090224     C                   MOVEL     KNMUS         D50PRU
005012090224     C                   MOVEL     V1CFIL        D50FGS
005013090224     C                   CALL      'FNLV50R'
005014090224     C                   PARM                    FNLV50
005015090224     C     D50ERR        IFNE      ' '
005016090224     C                   SETON                                        2840
005017090224     C                   MOVEL     D50MSG        $MSG
005018090224     C                   leave
005019090224     C                   ENDIF
005020090224     C*  Data distinta
005021090224     C     V1CDDC        IFEQ      0
005022090224     C                   SETON                                        4328
005023090224     C                   eval      $MSG  = 'Data distinta errata'
005024090224     C                   leave
005025090224     C                   ELSE
005026090224     C                   MOVE      V1CDDC        G02DAT
005027090224     C                   MOVEL     *BLANKS       G02ERR
005028090224     C                   CALL      'XSRDA8'
005029090224     C                   PARM                    WLBDAT
005030090224     C     G02ERR        IFEQ      '1'
005031090224     C                   SETON                                        2843
005032090224     C                   eval      $MSG  = 'Data distinta errata'
005033090224     C                   leave
005034090224     C                   END
005035090224     C                   END
005036090224     C                   Z-ADD     G02INV        WDDT
005037090224     C                   Z-ADD     G02DAT        V1CDDC
005038090224     C*  Data elaborazione
005039090224     C     V1CDta        IFEQ      0
005040090224     C                   SETON                                        4428
005042090224     C                   eval      $MSG  = 'Data elaborazione errata'
005043090224     C                   leave
005044090224     C                   ELSE
005045090224     C                   MOVE      V1CDta        G02DAT
005046090224     C                   MOVEL     *BLANKS       G02ERR
005047090224     C                   CALL      'XSRDA8'
005048090224     C                   PARM                    WLBDAT
005049090224     C     G02ERR        IFEQ      '1'
005050090224     C                   SETON                                        2844
005051090224     C                   eval      $MSG  = 'Data elaborazione errata'
005052090224     C                   leave
005053090224     C                   END
005054090224     C                   END
005055090224     C                   movel     v1cora        WDta
005056090224     C                   move      g02dat        WDta
005057090224     C                   Z-ADD     G02DAT        V1CDta
005136090224     C                   ENDdo
005137090224     C*
005138090224     C                   ENDSR
005139090224      *---------------------------------------------------------------*
005140090224     c     srs01         begsr
005141090224      *---------------------------------------------------------------*
005500080508     C*  Caricamento subfile errori
005600080508     C                   EXSR      INZS01
005601080526     c*  Se il subfile degli errori è vuoto forzo la scrittura
005602080526     c*  del file di lavoro fnarbs0f ed esco
005603090224     C     NRR3          IFeq      0
005604090224     c                   seton                                        21
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
008401090224     C/EXEC SQL
008402090224     C+ DECLARE A3 CURSOR FOR SELECT * FROM wfnlrf7f WHERE WF7LNP
008403090224     C+ =:v1clnp and WF7DDC =:wddc and WF7TIM>=:wdta
008407090224     C/END-EXEC
008410090224
008411090224     C/EXEC SQL
008412090224     C+ OPEN A3
008413090224     C/END-EXEC
008414090224
008415090224     C                   DOU       SqlCod <> 0
008416090224     C/EXEC SQL
008417090224     C+ FETCH NEXT FROM A3 INTO :wf7ds
008418090224     C/END-EXEC
008419090224     c*
008420090224     C                   SELECT
008421090224     C                   WHEN      SqlCod = 0
008425080805     c* aggancio la distinta per reperire codice autista
008426080805     c     kdst          chain     fidst01l
008427090224     c*
008428090224     c     kapd          chain     fiapd01l
008429090224     c*
031410090224     c     karb          chain     fnarb01l
031411090224     c*
031412090224     c                   clear                   tbluni
031413090224     C     wf7CAN        IFNE      ' '
031414090224     C                   MOVEL     '3E'          tblCOD
031415090224     C                   MOVEL(P)  wf7CAN        tblKEY
031416090224     C     KTAB          CHAIN     TABEL
031417090224     C                   END
031418090224     c*
031419090224     c                   eval      v3caas = wf7aas
031420090224     c                   eval      v3clnp = wf7lnp
031421090224     c                   eval      v3cnrs = wf7nrs
031422090224     c                   eval      v3cnsp = wf7nsp
031423090224     c                   eval      v3dpdr = apdrsf
031424090224     c                   eval      v3cfgs = wf7fgs
031425090224     c                   eval      v3cnfv = wf7nfv
031426090224     c                   eval      v3cnsc = wf7nsc
031427090224     c                   eval      v3cmsg = wf7not
031429090224     c                   movel     wf7tim        v3cora
031430090224     c                   move      wf7tim        v3cdta
031431090224     C                   MOVEL     TBLUNI        V3DCAN
031432090224     C                   ADD       1             NRR3
031433090224     C                   WRITE     LRf7S01
031434090224     C                   other
031435090224     c                   leave
031436090224     c                   endsl
045715080508      *---------------------------------------------------------------*
045716080508      *  Loop di errori  subfile errori                               *
045717080508      *---------------------------------------------------------------*
045718080509     C     CTR01         BEGSR
045720950315      *
045900090224     C                   READC     LRf7S01                                32
046000980325      *
046100950317     C     *IN32         DOWEQ     '0'
046203080704     c*
047010080701     c                   select
050803080509      *  Interrogazione bolla
050804080512     C                   when      v3csce = 'V'
051000950317     C                   MOVEL     '1'           P36FLG
051100950317     C                   Z-ADD     V3CAAS        P36AAS
051200950317     C                   Z-ADD     V3CLNP        P36LNP
051300950317     C                   Z-ADD     V3CNSP        P36NSP
051400950317     C                   Z-ADD     V3CNRS        P36NRS
051500080509     C                   MOVEL(P)  DSLR36        KPJBU
051600950317     C                   CALL      'FNLR36R'
051700080701     c                   PARM                    KPJBA
051701080730     c                   MOVEL     ' '           v3CSCE
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
060257080509     C*-----------------------------------------------------*
060258080509     C     *INZSR        BEGSR
060259080509     C*-----------------------------------------------------*
060260080508      *
060261080508     C     *ENTRY        PLIST
060262080707     C                   PARM                    fnlr53ds7
060263080515     C* Decodifica Elaboratore
060264080515     c     *dtaara       define    §azute        azuteds
060265080515     c     *dtaara       define    §datiute      ddatiute
060266080515     C                   in(E)     *dtaara
060267080515     C                   IF        %Error  or  RSUT = *blanks
060268080515     C                   call      'TIBS34R'
060269080515     C                   parm                    Tibs34Ds
060270080515     C                   in        *dtaara
060271080515     c                   ENDIF
060272080515     c*
060273080515     c     kfvv          klist
060274080707     c                   kfld                    knpg4
060275080515     c                   kfld                    knfvv
060276080701     c                   kfld                    d537fgsi
060277080805     c     kdst          klist
060278080805     c                   kfld                    knpg4
060279080805     c                   kfld                    knfv
060280080805     c                   kfld                    d537fgsi
060281080515     c     kfnart        klist
060282080515     c                   kfld                    brvlnp
060283080515     c                   kfld                    brvlna
060284080515     c                   kfld                    brvnrs
060285080515     c                   kfld                    brvnsc
060286080707     c     klbl          klist
060287080707     c                   kfld                    artaas
060288080707     c                   kfld                    artlnp
060289080707     c                   kfld                    artnrs
060290080707     c                   kfld                    artnsp
060291080707     c     karb          klist
060292080526     c                   kfld                    kaas
060293080526     c                   kfld                    klnp
060294080526     c                   kfld                    knrs
060295080526     c                   kfld                    knsp
060296080707     c     karbd         klist
060297080707     c                   kfld                    v3caas
060298080707     c                   kfld                    v3clnp
060299080707     c                   kfld                    v3cnrs
060300080707     c                   kfld                    v3cnsp
060301080707     c     kpcd          klist
060302080805     c                   kfld                    dstfgs
060303080805     c                   kfld                    dstnfv
060304080805     c                   kfld                    dstpdr
060305080805     c                   kfld                    pcdtrd
060306080707     c                   kfld                    kaas
060307080707     c                   kfld                    klnp
060308080707     c                   kfld                    knrs
060309080707     c                   kfld                    knsp
060310080805     c                   eval      pcdtrd = 'CED'
060311080707     c     kpct          klist
060312080805     c                   kfld                    dstfgs
060313080805     c                   kfld                    dstnfv
060314080805     c                   kfld                    dstpdr
060315080805     c                   kfld                    pcttrd
060316080707     c                   kfld                    kaas
060317080707     c                   kfld                    klnp
060318080707     c                   kfld                    knrs
060319080707     c                   kfld                    knsp
060320080707     c                   eval      pcttrd = 'CET'
060321081117     c     kpct01        klist
060326081117     c                   kfld                    kaas
060327081117     c                   kfld                    klnp
060328081117     c                   kfld                    knrs
060329081117     c                   kfld                    knsp
060330081117     c                   kfld                    pcttrd
060331080515     c     kfib1         klist
060332080515     c                   kfld                    knpg
060333080515     c                   kfld                    knfv
060334080701     c                   kfld                    d537fgsi
060335080515
060336080515     c     kfib3         klist
060337080515     c                   kfld                    knpg8
060338080515     c                   kfld                    brvlnp
060339080515     c                   kfld                    brvlna
060340080515     c                   kfld                    brvnrs
060341080515     c                   kfld                    brvnsc
060342080515     c                   kfld                    brvfgs
060343080515     c     kfib3b        klist
060344080707     c                   kfld                    knpg4
060345080515     c                   kfld                    brvlnp
060346080515     c                   kfld                    brvlna
060347080515     c                   kfld                    brvnrs
060348080515     c                   kfld                    brvnsc
060349080515     c                   kfld                    brvfgs
060350080707     c     kdisarb       klist
060351080707     c                   kfld                    arbifp
060352080707     c                   kfld                    arbndc
060353080707     c     kdispct       klist
060354080805     c                   kfld                    dstfgs
060355080805     c                   kfld                    dstnfv
060356080805     c                   kfld                    dstpdr
060357080707     c                   kfld                    pcttrd
060358080707     c     ktab          klist
060359080707     c                   kfld                    TBLKUT
060360080707     c                   kfld                    TBLCOD
060361080707     c                   kfld                    TBLKEY
060362080707     c                   eval      tblkut = 1
060363080707     c                   eval      tblcod = '2A'
060364080711     c     ktbe          klist
060365080711     c                   kfld                    TBeCOD
060366080711     c                   kfld                    TBeKE1
060367080711     c                   eval      tbecod = 'ECD'
060368080515     C*  Caricamento £1
060369080515     c                   clear                   trul06ds
060370080515     c                   eval      d06cod = '£1'
060371080515     c                   movel     dutpou        d06key
060372080515     c                   movel     trul06ds      kpjbu
060373080515     c                   call      'TRUL06R'
060374080515     c                   parm                    kpjba
060375080515     c                   movel     kpjbu         trul06ds
060376080515     c*
060377080509     C                   ENDSR
071100080515
