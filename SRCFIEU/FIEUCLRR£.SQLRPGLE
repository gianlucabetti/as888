000100160912      /TITLE Pulizia File CONTROLLO FATTURE PARTNER (IN SEDE)
000200160912      * Deve girare in ambiente di SEDE
000300160912      *
000400160714     ftabel00f  if   e           k disk
000500160715      *
000600160714     D KPJBA         E DS
000700160715      *
000800160714     D ds5a4         E DS
000900160715      *-------------------
001000160715     D W0140           S             14  0
001100160715     D WORA            S              6  0
001200160715     D WDTGIO          S              8  0
001300160715     D DATEU           S              8  0
001400160715     D DATA_eur        S               D   DATFMT(*eur)
001500160715     D DATA_iso        S               D   DATFMT(*iso)
001600160715      *
001700160715      * ?================================================================== */
001800160912     C     *entry        plist
001900160912     C                   parm                    kpjba
002000160912     C*
002100160714     C                   seton                                        lr
002200160714      *
002300160714     c     ktab          klist
002400160715     c                   kfld                    tblkut
002500160715     c                   kfld                    tblcod
002600160714     c                   kfld                    tblkey
002700160714      *
002800160715     c                   z-add     1             tblkut
002900160714     c                   clear                   ds5a4
003000160714     c                   movel     '5A'          tblcod
003100160714     c                   movel     '4'           tblkey
003200160714     c     ktab          chain     tabel00f
003300160714     c                   if        %found(tabel00f) and tblflg = *blanks
003400160714     c                   movel     tbluni        ds5a4
003500160714     c                   endif
003600160715      *
003700160715      *------------------
003800160715      * Recupero data e ora
003900160715     C                   TIME                    WORA
004000160715     C                   TIME                    W0140
004100160715      * UDATE IN GGMMAAAA
004200160715     C                   MOVE      W0140         WDTGIO
004300160715      * UDATE IN AAAAMMGG
004400160715     C     *eur          MOVEL     WDTGIO        DATA_eur
004500160715     C     *iso          MOVEL     DATA_eur      dateu
004600161118      *
004700161118      * in GIORNI in sospensione su EDIVAB
004800161118     C                   MOVEL     DATA_eur      DATA_iso
004900161118     c                   subdur    §5aDPDsos:*d  DATA_iso
005000161118     C                   MOVEL     DATA_iso      dataDPDSOS        8 0
005100161118      *
005200160714      *
005300161121      * PULIZIA EDIVAT0F  "DPD_IN_SOSPESO"  non presenti più i VAB
005400161121      *
005500161121     C/EXEC SQL
005600161121     C+   delete   FROM filtragru/edivat0f
005700161121     C+      WHERE
005800161121     C+           VATCMR  = '§DPD_IN_SOSPESO'   and
005900161121     C+    digits(vataas) concat digits(vatlnp) concat
006000161121     C+    digits(vatnrs) concat digits(vatnsp)
006100161121     c+          in (select
006200161121     C+    digits(vabaas) concat digits(vablnp) concat
006300161121     C+    digits(vabnrs) concat digits(vabnsp)
006400161121     C+      from
006500161121     C+    filtragru/edivab0f
006600161121     C+      WHERE
006700161121     C+    VAbCMR = '§DPD_IN_SOSPESO' and VABDCM < :dataDPDsos )
006800161121     C/END-EXEC
006900161012      *
007000161013      *
007100161118      * PULIZIA EDIVAB0F  "DPD_IN_SOSPESO"
007200161118      *
007300161013     C/EXEC SQL
007400161013     C+   DELETE from filtragru/edivab0f
007500161013     C+    WHERE
007600161121     C+   VABCMR = '§DPD_IN_SOSPESO' and  VABDCM < :dataDPDsos
007700161121     C+
007800161013     C/END-EXEC
007900161013      *
008000161013      *
008100161013      *
008200160714     c                   RETURN
008300000714
008400160715      * --------------------------------------------------------------------
