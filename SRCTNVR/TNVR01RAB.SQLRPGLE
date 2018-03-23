000100940224     H*--------------------------------------------------------------*
000200050908     FWXpdr00F  o  a E             DISK
000300050908     FTITAS30C  iF   E           K DISK
000400050912     Ffiftd05l  iF   E           K DISK
000500030926
000600050908     d codPDR          s              7  0
000700050908     d codTSR          s              1
000800050908     d codNDC          s              7  0
000900050908     d codDDC          s              8  0
001000050908     d codAAS          s              4  0
001100050908     d codLNP          s              3  0
001200050908     d codNRS          s              2  0
001300050908     d codNSP          s              7  0
001400050908     d codCOS          s              9  3
001500030205     C*---------------------------------------------------------------*
001600050908     C/EXEC SQL
001700050908     C+ DELETE FROM WXPDR00F
001800050908     C/END-EXEC
001900050908      **
002000050908     C/EXEC SQL
002100050908     C+ Declare A1 Cursor for
002200050908     c+ SELECT FCEPDR, FCETSR, FCENDC, FCEDDC, FCEAAS, FCELNP,
002300050908     c+ FCENRS, FCENSP, decimal(sum(FCEICE), 9, 3) costo
002400050908     c+ FROM fifce00f
002500050908     c+ WHERE fceddc in
002600050908     c+ (20050702, 20050709, 20050716,
002700050909     c+  20050723, 20050730, 20050903)  and
002800050909     c+ digits(fceNDC)concat(digits(fceDDC))concat(digits(fcePDR)) in
002900050909     c+ (select
003000050909     c+ digits(fTTNDC)concat(digits(fTTDDC))concat(digits(fttPDR))
003100050909     c+ from fiftt00f)
003200050908     c+ GROUP BY FCEPDR, FCETSR, FCENDC, FCEDDC, FCEAAS,
003300050908     c+  FCELNP, FCENRS, FCENSP
003400050909     c+ ORDER BY FCEPDR, FCETSR, FCENDC, FCEDDC, FCEAAS,
003500050909     c+  FCELNP, FCENRS, FCENSP
003600050908     C/END-EXEC
003700050908      *          apertura cursore
003800050908     C/EXEC SQL
003900050908     C+ OPEN A1
004000050908     C/END-EXEC
004100050908     C                   DOU       SqlCod <> 0
004200050908      *
004300050908      *          lettura cursore
004400050908     C/EXEC SQL
004500050908     C+ FETCH NEXT FROM A1 INTO :codPDR, :codTSR, :codNDC, :codDDC,
004600050908     C+ :codAAS, :codLNP, :codNRS, :codNSP, :codCOS
004700050908     C/END-EXEC
004800050908     C                   SELECT
004900050908     C                   WHEN      SqlCod = 100
005000050908     c* a fine file
005100050908     c                   leave
005200050908     c* x errori
005300050908     C                   WHEN      SqlCod < 0
005400050908     C                   seton                                        H1
005500050908     c                   goto      fine_ciclo
005600050908     c*
005700050908     C                   OTHER
005800050908     c*
005900050909     c*************      if        codCOS >0
006000050908
006100050908     c* dettaglio aggancia TITAS30C
006200050908     c                   clear                   wxpdr000
006300050908     c
006400050908     c                   z-add     codPDR        wxpPDR
006500050908     c                   MOVE      codTSR        wxpTSR
006600050908     c                   z-add     codNDC        wxpNDC
006700050908     c                   z-add     codDDC        wxpDDC
006800050908     c                   z-add     codAAS        wxpAAS
006900050908     c                   z-add     codLNP        wxpLNP
007000050908     c                   z-add     codNRS        wxpNRS
007100050908     c                   z-add     codNSP        wxpNSP
007200050908     c                   z-add     codCOS        wxpCOS
007300050908     c
007400050908     c     ktas          chain     titas30c
007500050908     c                   if        %Found(titas30c)
007600050908     c                   movel     tasaas        wxpdsp
007700050908     c                   move      tasmgs        wxpdsp
007800050908     c                   end
007900050912     c
008000050912     c     kftd          chain     fiftd05l
008100050912     c                   if        %Found(fiftd05l)
008200050912     c                   z-add     ftdncl        wxpncl
008300050912     c                   z-add     ftdvlu        wxpvlb
008400050912     c                   z-add     ftdpkl        wxppkb
008500050912     c                   end
008600050912     c
008700050908     c                   write     wxpdr000
008800050908      *
008900050909     c*************      end
009000050908      *
009100050908     C                   ENDSL
009200050908      *
009300050908     C                   ENDDO
009400030926      *
009500050908     c     fine_ciclo    tag
009600050908     C/EXEC SQL
009700050908     C+ Close A1
009800050908     C/END-EXEC
009900000000     C                   SETON                                        LR
010000030926      **-------------------------------------------------------------**
010100050912     c     ktas          klist
010200050912     c                   kfld                    wxpaas
010300050912     c                   kfld                    wxplnp
010400050912     c                   kfld                    wxpnrs
010500050912     c                   kfld                    wxpnsp
010600050912     c     kftd          klist
010700050912     c                   kfld                    wxpPDR
010800050912     c                   kfld                    wxpTSR
010900050912     c                   kfld                    wxpNDC
011000050912     c                   kfld                    wxpDDC
011100050908     c                   kfld                    wxpaas
011200050908     c                   kfld                    wxplnp
011300050908     c                   kfld                    wxpnrs
011400050908     c                   kfld                    wxpnsp
011500050908      **-------------------------------------------------------------**
