000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090109     H* TNVRA10R0 *--------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800081110     D esito           s             10i 0
000900081110     D FGT_REC         s              9s 0
001000931202     D KPJBA         E DS
001100900521     C*****************************************************************
001200081209     C                   SETON                                        LR
001300081230      **
001400081230     c                   if        Aggiorna ='S'
001500020418     C*
001600081230     C/EXEC SQL
001700090108     C+ UPDATE FIFGT00F as u SET u.FGTPRG = u.fgtPRG * 10
001800090109     C+  where  u.fgttsr in ('C', 'R', 'G') and
001900090109     C+         u.fgtsml concat digits(u.fgtpdr) in (
002000090108     C+  select a.fgtsml concat digits(a.fgtpdr)
002100090108     C+    from fifgt00f as a where a.fgtrap > 0 )
002200081110     C/END-EXEC
002300081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
002400081110     c                   if        sqlCODE < 0
002500081110     c                   eval      esito = Sqlcode
002600081110     c                   dump(a)
002700081110     c                   return
002800081110     c                   end
002900081230     C/EXEC SQL
003000081230     C+ UPDATE FIFPT00F SET FPTPRG = fptPRG * 10
003100090108     C+  where  fptsml concat digits(fptpdr) in (
003200090108     C+  select a.fgtsml concat digits(a.fgtpdr)
003300090109     C+    from fifgt00f as a where a.fgtrap > 0
003400090109     C+     and a.fgttsr in ('C', 'R', 'G') )
003500081230     C/END-EXEC
003600081230     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
003700081230     c                   if        sqlCODE < 0
003800081230     c                   eval      esito = Sqlcode
003900081230     c                   dump(a)
004000081230     c                   return
004100081230     c                   end
004200081230     C/EXEC SQL
004300081230     C+ UPDATE FIFPD00F SET FPDPRG = fpdPRG * 10
004400090108     C+  where  fpdsml concat digits(fpdpdr) in (
004500090108     C+  select a.fgtsml concat digits(a.fgtpdr)
004600090109     C+    from fifgt00f as a where a.fgtrap > 0
004700090109     C+     and a.fgttsr in ('C', 'R', 'G') )
004800081230     C/END-EXEC
004900081230     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
005000081230     c                   if        sqlCODE < 0
005100081230     c                   eval      esito = Sqlcode
005200081230     c                   dump(a)
005300081230     c                   return
005400081230     c                   end
005500081230     C*
005600081230     c                   end
005700081230     C*
005800081110     C     FINE          TAG
005900081110     c                   return
006000081209     C*------------------------------------------------------------------------*
006100081209     C* *INZSR - OPERAZIONI INIZIALI
006200081209     C*------------------------------------------------------------------------*
006300081209     C     *INZSR        BEGSR
006400081209      *
006500081209     C     *ENTRY        PLIST
006600081209     C                   PARM                    AGGIORNA          1
006700081209     C*
006800081209     c                   clear                   esito
006900081209      *
007000081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
007100081209     C                   clear                   UDATE_8           8 0
007200081209     C                   MOVEL     *DATE         UDATE_8
007300081209     C*
007400081209     c                   endSR
