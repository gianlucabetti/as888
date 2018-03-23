000100940224     H*--------------------------------------------------------------*
000200050908     FWXpdr00F  o  a E             DISK
000300050908     FTITAS30C  iF   E           K DISK
000400030926
000500050908     d codPDR          s              7  0
000600050908     d codTSR          s              1
000700050908     d codNDC          s              7  0
000800050908     d codDDC          s              8  0
000900050908     d codAAS          s              4  0
001000050908     d codLNP          s              3  0
001100050908     d codNRS          s              2  0
001200050908     d codNSP          s              7  0
001300050908     d codCOS          s              9  3
001400030205     C*---------------------------------------------------------------*
001500050908     C/EXEC SQL
001600050908     C+ DELETE FROM WXPDR00F
001700050908     C/END-EXEC
001800050908      **
001900050908     C/EXEC SQL
002000050908     C+ Declare A1 Cursor for
002100050908     c+ SELECT FCEPDR, FCETSR, FCENDC, FCEDDC, FCEAAS, FCELNP,
002200050908     c+ FCENRS, FCENSP, decimal(sum(FCEICE), 9, 3) costo
002300050908     c+ FROM fifce00f
002400050908     c+ WHERE fceddc in
002500050908     c+ (20050702, 20050709, 20050716,
002600050908     c+  20050723, 20050730, 20050903)
002700050908     c+ GROUP BY FCEPDR, FCETSR, FCENDC, FCEDDC, FCEAAS,
002800050908     c+  FCELNP, FCENRS, FCENSP
002900050908     C/END-EXEC
003000050908      *          apertura cursore
003100050908     C/EXEC SQL
003200050908     C+ OPEN A1
003300050908     C/END-EXEC
003400050908     C                   DOU       SqlCod <> 0
003500050908      *
003600050908      *          lettura cursore
003700050908     C/EXEC SQL
003800050908     C+ FETCH NEXT FROM A1 INTO :codPDR, :codTSR, :codNDC, :codDDC,
003900050908     C+ :codAAS, :codLNP, :codNRS, :codNSP, :codCOS
004000050908     C/END-EXEC
004100050908     C                   SELECT
004200050908     C                   WHEN      SqlCod = 100
004300050908     c* a fine file
004400050908     c                   leave
004500050908     c* x errori
004600050908     C                   WHEN      SqlCod < 0
004700050908     C                   seton                                        H1
004800050908     c                   goto      fine_ciclo
004900050908     c*
005000050908     C                   OTHER
005100050908     c*
005200050908      * solo se ci sono dei costi
005300050908     c                   if        codCOS >0
005400050908
005500050908     c* dettaglio aggancia TITAS30C
005600050908     c                   clear                   wxpdr000
005700050908     c
005800050908     c                   z-add     codPDR        wxpPDR
005900050908     c                   MOVE      codTSR        wxpTSR
006000050908     c                   z-add     codNDC        wxpNDC
006100050908     c                   z-add     codDDC        wxpDDC
006200050908     c                   z-add     codAAS        wxpAAS
006300050908     c                   z-add     codLNP        wxpLNP
006400050908     c                   z-add     codNRS        wxpNRS
006500050908     c                   z-add     codNSP        wxpNSP
006600050908     c                   z-add     codCOS        wxpCOS
006700050908     c
006800050908     c     ktas          chain     titas30c
006900050908     c                   if        %Found(titas30c)
007000050908     c                   z-add     tasncl        wxpncl
007100050908     c                   z-add     tasvlb        wxpvlb
007200050908     c                   z-add     taspkb        wxppkb
007300050908     c                   movel     tasaas        wxpdsp
007400050908     c                   move      tasmgs        wxpdsp
007500050908     c                   end
007600050908     c                   write     wxpdr000
007700050908      *
007800050908     c                   end
007900050908      *
008000050908     C                   ENDSL
008100050908      *
008200050908     C                   ENDDO
008300030926      *
008400050908     c     fine_ciclo    tag
008500050908     C/EXEC SQL
008600050908     C+ Close A1
008700050908     C/END-EXEC
008800000000     C                   SETON                                        LR
008900030926      **-------------------------------------------------------------**
009000050908     c     ktas          klist
009100050908     c                   kfld                    wxpaas
009200050908     c                   kfld                    wxplnp
009300050908     c                   kfld                    wxpnrs
009400050908     c                   kfld                    wxpnsp
009500050908      **-------------------------------------------------------------**
