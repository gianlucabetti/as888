000100160912      /TITLE Pulizia File CONTROLLO FATTURE PARTNER (IN SEDE)
000200160912      * Deve girare in ambiente di SEDE
000300160912      *
000400160714     ftabel00f  if   e           k disk
000500160715      *
000600160714     D ds5a4         E DS
000700160715      *-------------------
000800160715     D W0140           S             14  0
000900160715     D WORA            S              6  0
001000160715     D WDTGIO          S              8  0
001100160715     D DATEU           S              8  0
001200160715     D DATA_eur        S               D   DATFMT(*eur)
001300160715     D DATA_iso        S               D   DATFMT(*iso)
001400160715      *
001500160715      * ?================================================================== */
001600160714     C                   seton                                        lr
001700160714      *
001800160714     c     ktab          klist
001900160715     c                   kfld                    tblkut
002000160715     c                   kfld                    tblcod
002100160714     c                   kfld                    tblkey
002200160714      *
002300160715     c                   z-add     1             tblkut
002400160714     c                   clear                   ds5a4
002500160714     c                   movel     '5A'          tblcod
002600160714     c                   movel     '4'           tblkey
002700160714     c     ktab          chain     tabel00f
002800160714     c                   if        %found(tabel00f) and tblflg = *blanks
002900160714     c                   movel     tbluni        ds5a4
003000160714     c                   endif
003100160715      *
003200160715      *------------------
003300160715      * Recupero data e ora
003400160715     C                   TIME                    WORA
003500160715     C                   TIME                    W0140
003600160715      * UDATE IN GGMMAAAA
003700160715     C                   MOVE      W0140         WDTGIO
003800160715      * UDATE IN AAAAMMGG
003900160715     C     *eur          MOVEL     WDTGIO        DATA_eur
004000160715     C     *iso          MOVEL     DATA_eur      dateu
004100161118      *
004200161118      * in GIORNI in sospensione su EDIVAB
004300161118     C                   MOVEL     DATA_eur      DATA_iso
004400161118     c                   subdur    §5aDPDsos:*d  DATA_iso
004500161118     C                   MOVEL     DATA_iso      dataDPDSOS        8 0
004600161118      *
004700160714      *
004800161121      * PULIZIA EDIVAT0F  "DPD_IN_SOSPESO"  non presenti più i VAB
004900161121      *
005000161121     C/EXEC SQL
005100161121     C+   delete   FROM filtragru/edivat0f
005200161121     C+      WHERE
005300161121     C+           VATCMR  = '§DPD_IN_SOSPESO'   and
005400161121     C+    digits(vataas) concat digits(vatlnp) concat
005500161121     C+    digits(vatnrs) concat digits(vatnsp)
005600161121     c+          in (select
005700161121     C+    digits(vabaas) concat digits(vablnp) concat
005800161121     C+    digits(vabnrs) concat digits(vabnsp)
005900161121     C+      from
006000161121     C+    filtragru/edivab0f
006100161121     C+      WHERE
006200161121     C+    VAbCMR = '§DPD_IN_SOSPESO' and VABDCM < :dataDPDsos )
006300161121     C/END-EXEC
006400161012      *
006500161013      *
006600161118      * PULIZIA EDIVAB0F  "DPD_IN_SOSPESO"
006700161118      *
006800161013     C/EXEC SQL
006900161013     C+   DELETE from filtragru/edivab0f
007000161013     C+    WHERE
007100161121     C+   VABCMR = '§DPD_IN_SOSPESO' and  VABDCM < :dataDPDsos
007200161121     C+
007300161013     C/END-EXEC
007400161013      *
007500161013      *
007600160714     c                   RETURN
007700160715      * --------------------------------------------------------------------
