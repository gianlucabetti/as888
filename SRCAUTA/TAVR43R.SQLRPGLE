000100081212     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200031105
000300081211      //****************************************************************
000400081211      //                                                               *
000500081211      //       AGGIORNA TARIFFE CON ADDIZIONALE                        *
000600081211      //                                                               *
000700081211      //****************************************************************
000800031105
000900081211     FTNTAM01L  UF   E           K DISK
001000031105
001100081201     FTabel00f  if   e           k DISK
001200081211      //------------------------------------------------------------------------*
001300031105
001400031105
001500061128     D Dsta01        e ds
001600031105     D kpjba         e ds
001700081211     D wfautds       e ds                  extname(WFAUT00F)
001800081211
001900081211     D W_wfaclf        s                   like(wfaclf)
002000081211     D W_wfactrv       s                   like(wfactrv)
002100081211     D W_wfaprgn       s                   like(wfaprgn)
002200081211     D W_wfappan       s                   like(wfappan)
002300081211
002400081211     D Kcod            S                   like(TBLCOD)
002500081211     D Kkey            S                   like(TBLKEY)
002600081211     D codut           s              1  0
002700081211
002800081211
002900081211     D perce           DS                  INZ
003000081211     D  perce1                 1      5  2
003100081211     D  perce2                 6     10  2
003200081211
003300081211
003400081211
003500081211
003600081211      //---------------------------------------------------------------
003700081211      //?Definizione key-list.
003800081211      //---------------------------------------------------------------
003900081211
004000081211      // - File TNTAM01L
004100081211     d k03tam01      e ds                  extname(TNTAM01L:*key)
004200081211     d                                     inz
004300081211      //--------------------------------------------------------------------------------------------
004400081211      //------------------------------------------------------------------------*
004500081211      // Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004600081128     c
004700081211      /FREE
004800081211       EXEC SQL
004900081211        SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD;
005000081211       //
005100081211       EXEC SQL
005200081211        declare C1 cursor for
005300081211        SELECT wfaclf, wfactrv, wfaprgn, wfappan  FROM EDPAUMENTI/WFaut00F
005400081211        WHERE WFATCL = 'C' and wfafauc = 'A' and wfappan = 3,4
005500081211        order BY  wfaclf, wfactrv, wfaprgn
005600081211        for update of wfappan, wfafauc ;
005700081211
005800081211
005900081211       EXEC SQL
006000081211        open C1;
006100081211
006200081211       EXEC SQL
006300081211        Fetch C1 into :W_wfaclf , :W_wfactrv, :W_wfaprgn, :W_wfappan;
006400081211       //
006500081128
006600081211       //
006700081211       dow sqlcod = *zeros;
006800031105
006900081211         //
007000081211         // Aggancio Tntam01l
007100081211
007200081211               TAMksc = W_wfaclf  ;
007300081211               TAMctr = W_wfactrv ;
007400081211               TAMprg = W_wfaprgn ;
007500081211
007600081211         chain %kds(k03tam01 : 3) TNTAM01L;
007700081211
007800081211       //  se esiste in TNTAM
007900081211         If  %found(TNTAM01L) and tamatb = ' '
008000081211
008100081211       //  verifico se è stata modificata la % addizionale
008200081211             and tamppa = W_Wfappan ;
008300081211               tamppa = perce2;
008400081211               update tntam000;
008500081211       // aggiorno i dati sul file di lavoro dell'aumento
008600081211       EXEC SQL
008700081211         UPDATE edpaumenti/wfaut00f SET wfappan = :tamppa , wfafauc = 'M'
008800081211           WHERE CURRENT OF C1 ;
008900081211
009000081211         ENDIF;
009100081211
009200081211       EXEC SQL
009300081211        Fetch C1 into :W_wfaclf , :W_wfactrv, :W_wfaprgn, :W_wfappan;
009400081211         //
009500081211       enddo;
009600051021
009700081211       //
009800081211       EXEC SQL
009900081211        close C1;
010000081211
010100081201
010200081211       *INlr = *ON;
010300081201
010400081211       //****************************************************************
010500081211       // ROUTINE INIZIALE
010600081211       //****************************************************************
010700081211       BEGSR *INZSR;
010800031105
010900081211      /END-FREE
011000031105     C     *ENTRY        PLIST
011100031105     C                   PARM                    KPJBA
011200081211      //
011300081211      //
011400081211     c     keytab        klist
011500081211     c                   kfld                    codut
011600081211     c                   kfld                    kcod
011700081211     c                   kfld                    kkey
011800081211
011900081211      * cerco le percentuali di aumento
012000081211     c                   z-add     1             codut
012100081211     c                   clear                   perce
012200081211     c                   move(p)   '2008'        kkey
012300081211     c                   movel(p)  '%A'          kcod
012400081211     c     Keytab        chain     tabel00f
012500081211     c                   If        %found(tabel00f)
012600081211     c                   movel     tbluni        perce
012700081211     c                   endif
012800081211     C                   endsr
