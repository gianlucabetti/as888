000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200170203      *------------------------------------------------------------------------*
000300170203     faitra00f  uf   e             disk
000400170203     ffiapd00f  uf   e             disk
000500170203      *------------------------------------------------------------------------*
000600170203     d RRNFIAPD        s              9  0
000700170203     d RRNAITRA        s              9  0
000800170203      *------------------------------------------------------------------------*
000900170203     C/EXEC SQL
001000170203     C+ Declare A1 Cursor for
001100170203     c+  with
001200170203     c+     ANAGAPD as (
001300170203     c+  SELECT int(substr(apdflr, 9, 7)) massa,
001400170203     c+  rrn(fiapd00f) as RRN_APD, fiapd00f.* FROM fiapd00f
001500170203     c+  WHERE APDTIP ='A' and APDATB =' '
001600170203     c+  and int(substr(apdflr, 9, 7)) < 260000),
001700170203     c+     AUTCITTA as (
001800170203     c+  SELECT rrn(aitra00f) as RRN_TRA, aitra00f.*
001900170203     c+  FROM aitra00f WHERE TRAANN<>'A' and TRADIN < 20170101
002000170203     c+  and tradfi=0 and traKAU>0 and traBPT=0),
002100170203     c+     AUTMISTI as (
002200170203     c+  SELECT rrn(aitra00f) as RRN_TRA, aitra00f.*
002300170203     c+  FROM aitra00f WHERE TRAANN<>'A' and TRADIN < 20170101
002400170203     c+  and tradfi=0 and traKAU>0 and traBPT>0),
002500170203     c+     VALORGEN as (
002600170203     c+  SELECT * FROM FIFTT00f
002700170203     c+  WHERE FTTDDC between 20170101 and 20170131)
002800170203     c+      select rrn_APD, rrn_TRA from ANAGAPD join AUTCITTA
002900170203     c+      on apdPDR=traKAU
003000170203     c+      where apdpdr not in ( select fttpdr from VALORGEN)
003100170203     C/END-EXEC
003200170203      *          apertura cursore
003300170203     C/EXEC SQL
003400170203     C+ OPEN A1
003500170203     C/END-EXEC
003600170203     C                   DOU       SqlCod = 100 OR SqlCod < 0
003700170203      *          lettura cursore
003800170203     C/EXEC SQL
003900170203     C+ Fetch Next From A1 Into :RRNFIAPD, :RRNAITRA
004000170203     C/END-EXEC
004100170203     C                   SELECT
004200170203      * EoF
004300170203     C                   WHEN      SqlCod = 100
004400170203     c                   leave
004500170203      *
004600170203      * Lettura dei records
004700170203     C                   WHEN      SqlCod >= 0
004800170203      *
004900170203      *  esegue aggiornamento
005000170203     c     RRNFIAPD      chain     fiapd00f
005100170203     c                   if        %found(fiapd00f)
005200170203     c                   eval      apdSNU = 'S'
005300170203     c                   update    fiapd000
005400170203     c                   end
005500170203      *
005600170203     c     RRNAITRA      chain     aitra00f
005700170203     c                   if        %found(aitra00f)
005800170206     c                   eval      tradfi = 20170205
005900170203     c                   eval      tradp4 = 99991231
006000170203     c                   update    aitra000
006100170203     c                   end
006200170203      *
006300170203     C                   OTHER
006400170203      *   Errori
006500170203     C                   EXSR      SQLError
006600170203     c                   leave
006700170203      *
006800170203     C                   ENDSL
006900170203      *
007000170203     C                   ENDDO
007100170203     C/EXEC SQL
007200170203     C+ Close A1
007300170203     C/END-EXEC
007400170203     C*
007500170203     C     FINE          TAG
007600170203     C                   SETON                                        LR
007700170203      **********************************************************************
007800170203      **********************************************************************
007900170203     c     SQLError      begsr
008000170203      *
008100170203     c                   endsr
008200170203      **********************************************************************
