000400030115
000500051130      /TITLE Calcola ultima data elaborazione
000602051213     dtntudtads      e ds
000607051130     d dta             s               d   inz
002000030115
005100051129     C     *ENTRY        PLIST
005200051213     C                   PARM                    tntudtads
005304051130     c* reperisco la data ultima di elaborazione
005305051130     C/EXEC SQL
005306051130     C+ DECLARE B1 CURSOR FOR SELECT ifnull(max( TUDDTA ), '0001-01-01')
005307051130     C+ FROM tntud00f WHERE TUDMULT = :t05mult
005308051130     C/END-EXEC
005309051130
005310051130     C/EXEC SQL
005311051130     C+ OPEN B1
005312051130     C/END-EXEC
005313051130     C                   DO        *hival
005314051130     C/EXEC SQL
005315051130     C+ FETCH NEXT FROM B1 INTO :dta
005316051130     C/END-EXEC
005317051130     C                   SELECT
005318051130     c*
005319051130     C                   WHEN      SqlCod = 100
005320051130     c                   leave
005321051130     C                   WHEN      SqlCod < 0
005322051130     C                   seton                                        H1
005323051130     c                   exsr      srfine
005324051130     c                   other
005326051130     c                   movel     dta           t05dti
005327051130     c                   leave
005328051130     C                   ENDSL
005329051130      *
005330051130     C                   ENDDO
005331051130     C/EXEC SQL
005332051130     C+ CLOSE B1
005333051130     C/END-EXEC
005334051130     c                   exsr      srfine
005500051129     C**************************************************************************
005600051129     C* esco con codice d'errore
005700051129     C**************************************************************************
005800051129     C     srfine        BEGSR
007100051129     c                   seton                                        lr
007200051129     c                   return
007300051129     c*
007400051129     c                   endsr
