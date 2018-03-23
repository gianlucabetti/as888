000100031105     H DECEDIT('0,') DATEDIT(*YMD.)
000200031105
000300081211      //****************************************************************
000400081211      //                                                               *
000500081211      //       AGGIORNA TARIFFE CON ADDIZIONALE                        *
000600081211      //                                                               *
000700081211      //****************************************************************
000900031105
001600081211     FTNTAM01L  UF   E           K DISK
002400031105
002500081201     FTabel00f  if   e           k DISK
002600081211      //------------------------------------------------------------------------*
002700031105
007300031105
007400061128     D Dsta01        e ds
007500031105     D kpjba         e ds
007501081211     D wfautds       e ds                  extname(WFAUT00F)
007503081211
007504081211     D W_wfaclf        s                   like(wfaclf)
007505081211     D W_wfactrv       s                   like(wfactrv)
007507081211     D W_wfaprgn       s                   like(wfaprgn)
007508081211     D W_wfappan       s                   like(wfappan)
007510081211
007511081211     D Kcod            S                   like(TBLCOD)
007512081211     D Kkey            S                   like(TBLKEY)
007513081211
007515081211
007516081211     D perce           DS                  INZ
007517081211     D  perce1                 1      5  2
007518081211     D  perce2                 6     10  2
007519081211
007520081211
007521081211
007522081211
007600081211      //---------------------------------------------------------------
007601081211      //?Definizione key-list.
007602081211      //---------------------------------------------------------------
007603081211
007604081211      // - File TNTAM01L
007605081211     d k03tam01      e ds                  extname(TNTAM01L:*key)
007608081211     d                                     inz
007700081211      //--------------------------------------------------------------------------------------------
007800081211      //------------------------------------------------------------------------*
007900081211      // Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008000081128     c
008001081211      /FREE
008100081211       EXEC SQL
008200081211        SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD;
008400081211       //
008500081211       EXEC SQL
008600081211        declare C1 cursor for
008700081211        SELECT wfaclf, wfactrv, wfaprgn, wfappan  FROM EDPAUMENTI/WFaut00F
008800081211        WHERE WFATCL = 'C' and wfafauc = 'A' and wfappan = 3,4
008900081211        order BY  wfaclf, wfactrv, wfaprgn
009000081211        for update of wfappan, wfafauc ;
009001081211
009002081211
009300081211       EXEC SQL
009400081211        open C1;
009600081211
009700081211       EXEC SQL
009800081211        Fetch C1 into :W_wfaclf , :W_wfactrv, :W_wfaprgn, :W_wfappan;
010000081211       //
010100081128
010200081211       //
010300081211       dow sqlcod = *zeros;
010400031105
010500081211         //
010600081211         // Aggancio Tntam01l
010602081211
010603081211               TAMksc = W_wfaclf  ;
010604081211               TAMctr = W_wfactrv ;
010605081211               TAMprg = W_wfaprgn ;
010606081211
010608081211         chain %kds(k03tam01 : 3) TNTAM01L;
010609081211
010610081211       //  se esiste in TNTAM
010611081211         If  %found(TNTAM01L) and tamatb = ' '
010612081211
010614081211       //  verifico se è stata modificata la % addizionale
010615081211             and tamppa = W_Wfappan ;
010616081211               tamppa = perce2;
019100081211               update tntam04;
019200081211       // aggiorno i dati sul file di lavoro dell'aumento
019201081211       EXEC SQL
019206081211         UPDATE edpaumenti/wfaut00f SET wfappan = :tamppa , wfafauc = 'M'
019208081211           WHERE CURRENT OF C1 ;
019209081211
019210081211         ENDIF;
024600081211
024601081211       EXEC SQL
024701081211        Fetch C1 into :W_wfaclf , :W_wfactrv, :W_wfaprgn, :W_wfappan;
025000081211         //
025100081211       enddo;
025200051021
025300081211       //
025400081211       EXEC SQL
025500081211        close C1;
025700081211
025800081201
025900081211       *INlr = *ON;
026000081201
062700081211       //****************************************************************
062800081211       // ROUTINE INIZIALE
062900081211       //****************************************************************
063000081211       BEGSR *INZSR;
063100031105
063101081211      /END-FREE
063200031105     C     *ENTRY        PLIST
063300031105     C                   PARM                    KPJBA
063400081211      //
065400081211      //
065404081211     c     keytab        klist
065405081211     c                   kfld                    codut
065406081211     c                   kfld                    kcod
065407081211     c                   kfld                    kkey
065408081211      *
065409081211      * reperisce data del giorno
065410081211     C                   TIME                    W0140
065411081211      * UDATE IN GGMMAAAA
065412081211     C                   MOVE      W0140         WDTGIO
065413081211      * UDATE IN AAAAMMGG
065414081211     C                   Z-ADD     WDTGIO        G02DAT
065415081211     C                   MOVEL     *BLANK        G02ERR
065416081211     C                   CALL      'XSRDA8'
065417081211     C                   PARM                    WLBDAT
065418081211     C                   Z-ADD     G02INV        §oggi
065419081211
065420081211      * cerco le percentuali di aumento
065421081211     c                   clear                   perce
065422081211     c                   move(p)   '2008'        kkey
065423081211     c                   movel(p)  '%A'          kcod
065424081211     c     Keytab        chain     tabel00f
065425081211     c                   If        %found(tabel00f)
065426081211     c                   movel     tbluni        perce
065427081211     c                   endif
065428081211     C                   endsr
067400081211       endsr;
