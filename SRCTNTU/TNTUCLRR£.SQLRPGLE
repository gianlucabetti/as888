000400030115
000500051201      /TITLE verifica se per un determinato moltiplicatore, gg ci sono
000501051201      /TITLE personalizzazioni cliente
000602051201     dtntuclids      e ds
000610051207     d cli                                 overlay(tntuclids:20)
000611051213     D                                     DIM(100)
000612051213     D                                     like(§cliclii)
000614051207     D row             DS
000615051213     D                                     OCCURS(100)
000616051207     D                                     INZ
000617051213     D  rowcli                             like(§cliclii)
000618051207     d x               s              5i 0
002000030115
005100051129     C     *ENTRY        PLIST
005200051201     C                   PARM                    tntuclids
005201051207     **?Il chiamante mi dice di chiudere.
005202051207     c                   if        §cliesi = '9'
005204051207     C                   exsr      srfine
005205051207     C                   ENDIF
005206051207     **?Al ricevimento di una nuova richiesta chiudo il cursore già aperto
005207051207     **?e preparo la nuova istruzione SQL.
005208051207     C                   IF        §cliesi = '2'
005211051207     C                   EXSR      CloseLista
005212051207     C                   ENDIF
005213051201     c                   clear                   cli
005214051201     c                   clear                   x
005305051207     c*
005306051213     c                   if        §cliclii= 0
005307051130     C/EXEC SQL
005308051207     C+ DECLARE A1 CURSOR FOR SELECT tutcli FROM tntua00f join tntut00f
005309051207     C+ on tuaatt = tutatt and tuaprg = tutprg WHERE tuaatb = ' ' and
005310051207     C+ tutatb = ' ' and tuamult = :§climult and :§clidta between tuadti
005311051207     C+ and tuadtf and :§clidta between tutdti and tutdtf and tutcli <> 0
005312051214     C+ GROUP BY tutcli FOR READ ONLY
005317051130     C/END-EXEC
005318051207     c                   else
005319051207     C/EXEC SQL
005320051207     C+ DECLARE B1 CURSOR FOR SELECT tutcli FROM tntua00f join tntut00f
005321051207     C+ on tuaatt = tutatt and tuaprg = tutprg WHERE tuaatb = ' ' and
005322051207     C+ tutatb = ' ' and tuamult = :§climult and :§clidta between tuadti
005323051207     C+ and tuadtf and :§clidta between tutdti and tutdtf and tutcli
005324051214     C+ =:§cliclii GROUP BY tutcli FOR READ ONLY
005325051207     C/END-EXEC
005326051201     c                   end
005327051130
005328051213     c                   if        §cliclii= 0
005329051207     C/EXEC SQL
005330051207     C+ OPEN A1
005331051207     C/END-EXEC
005332051207     c                   else
005333051130     C/EXEC SQL
005334051130     C+ OPEN B1
005335051130     C/END-EXEC
005336051207     c                   end
005337051207
005338051213     c                   if        §cliclii= 0
005339051207     C/EXEC SQL
005340051213     C+ FETCH NEXT FROM A1 FOR 100 ROWS INTO :row
005341051207     C/END-EXEC
005342051207     c                   else
005343051130     C/EXEC SQL
005344051213     C+ FETCH NEXT FROM B1 FOR 100 ROWS INTO :row
005345051130     C/END-EXEC
005346051207     c                   end
005347051130     C                   SELECT
005348051130     c*
005349051130     C                   WHEN      SqlCod = 100
005350051207     C                   EXSR      srfine
005351051130     C                   WHEN      SqlCod < 0
005352051130     C                   seton                                        H1
005353051130     c                   exsr      srfine
005354051130     c                   other
005355051207     **?20 ricorrenze. Il numero è contenuto nella variabile SQLER3.
005356051207     **?Scorro la DS multipla e riempio le schiere.
005357051207     C                   DO        sqlEr3        x
005358051207     C     x             OCCUR     row
005359051207     C                   EVAL      cli(x) = rowcli
005360051207     C                   ENDDO
005364051130     C                   ENDSL
005365051207     c* imposto il ritorno
005367051213     c                   if        sqler3 < 100
005368051207     c                   eval      §cliesi = '9'
005369051207     c                   exsr      srfine
005370051207     c                   else
005371051207     c                   eval      §cliesi = '2'
005372051207     c                   end
005373051130      *
005500051129     C**************************************************************************
005600051129     C* esco con codice d'errore
005700051129     C**************************************************************************
005800051129     C     srfine        BEGSR
005801051207     c                   exsr      closelista
007100051129     c                   seton                                        lr
007200051129     c                   return
007300051129     c*
007400051129     c                   endsr
007500051207     ***********************************************************************
007600051207     **?
007700051207     **?Chiusura cursore.
007800051207     **?
007900051207     ***********************************************************************
008000051207     C     CloseLista    BEGSR
008100051207
008101051213     c                   if        §cliclii= 0
008200051207     C/EXEC SQL
008300051207     C+ CLOSE A1
008400051207     C/END-EXEC
008401051207     c                   else
008402051207     C/EXEC SQL
008403051207     C+ CLOSE B1
008404051207     C/END-EXEC
008405051207     c                   end
008500051207
008600051207     C                   ENDSR
008700051207
