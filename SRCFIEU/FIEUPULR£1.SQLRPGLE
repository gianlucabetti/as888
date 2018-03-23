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
004600160912      * in MESI
004700160912     C                   MOVEL     DATA_eur      DATA_iso
004800160912     c                   subdur    §5acfDPD:*m   DATA_iso
004900160912     C                   MOVEL     DATA_iso      dataDPD           8 0
005000160912     C                   MOVE      '01'          dataDPD
005100160912      * x File Originale CLEARING (Barbara)
005200160912     C                   MOVEL     DATA_iso      data10aDPD       10
005300160912     C                   MOVEL     data10aDPD    data7aDPD         7            '2016-09'
005400160912      * in MESI
005500160912     C                   MOVEL     DATA_eur      DATA_iso
005600160912     c                   subdur    §5acfFED:*m   DATA_iso
005700160912     C                   MOVEL     DATA_iso      dataFED           8 0
005800160912     C                   MOVE      '01'          dataFED
005900160912     C                   MOVE      dataFED       datalfaFED        8
006000160912      * in MESI
006100160912     C                   MOVEL     DATA_eur      DATA_iso
006200160912     c                   subdur    §5acfEEX:*m   DATA_iso
006300160912     C                   MOVEL     DATA_iso      dataEEX           8 0
006400160912     C                   MOVE      '01'          dataEEX
006500160912      *
006600160912      * PULIZIA File Controllo Fatture Partner
006700160715      *
006800160912      *  FILE STORICO ricezioni da FEDEX Fatture originali tracciato FEDEX
006900160715     C/EXEC SQL
007000160715     C+    DELETE
007100160715     C+    from fifeis0w where  rrn(fifeis0w) < (
007200160715     C+
007300160715     C+    SELECT               rrn(fifeis0w)
007400160715     C+    FROM fifeis0w WHERE WRKDAT like 'HA1INVOICE%'
007500160715     C+           and substr(wrkdat, 57, 8) in (
007600160715     C+
007700160715     C+    SELECT max(substr(wrkdat, 57, 8))
007800160715     C+    FROM fifeis0w WHERE WRKDAT like 'HA1INVOICE%'
007900160912     C+           and substr(wrkdat, 57, 8) < :datalfaFED )
008000160715     C+    )
008100160715     C/END-EXEC
008200160715      *
008300160715      *
008400160715      * PULIZIA File Controllo Fatture PArtner
008500160715      *
008600160715     C/EXEC SQL
008700160912     C+    DELETE  from        tnefr00f
008800160912     C+      where efrntw = 'DPD'
008900160912     C+        and efrdft < :dataDPD
009000160715     C/END-EXEC
009100160912      *
009200160912     C/EXEC SQL
009300160912     C+    DELETE  from        tnefr00f
009400160912     C+      where efrntw = 'FED'
009500160912     C+        and efrdft < :dataFED
009600160912     C/END-EXEC
009700160912      *
009800160912     C/EXEC SQL
009900160912     C+    DELETE  from        tnefr00f
010000160912     C+      where efrntw not in ('DPD', 'FED')
010100160912     C+        and efrdft < :dataEEX
010200160912     C/END-EXEC
010300160715      *
010400160715      *
010500160715      *
010600160715     C/EXEC SQL
010700160912     C+    DELETE   from       tnecf00f
010800160912     C+      where  ecfNTW = 'DPD' and
010900160916     C+            (ecfdcm > 0     and
011000160916     C+             ecfdcm < :dataDPD )
011100160718     C+               or
011200160916     C+             ecfNTW = 'DPD' and
011300160718     C+            (ecfdfp > 0     and
011400160912     C+             ecfdfp < :dataDPD )
011500160715     C/END-EXEC
011600160912      *
011700160912     C/EXEC SQL
011800160912     C+    DELETE   from       tnecf00f
011900160912     C+      where  ecfNTW = 'FED' and
012000160912     C+            (ecfaas > 0     and
012100160912     C+             ecfaas*10000+ecfmgs < :dataFED )
012200160912     C+               or
012300160916     C+             ecfNTW = 'FED' and
012400160912     C+            (ecfdfp > 0     and
012500160912     C+             ecfdfp < :dataFED )
012600160912     C/END-EXEC
012700160715      *
012800160912     C/EXEC SQL
012900160912     C+    DELETE   from       tnecf00f
013000160912     C+      where  ecfNTW not in ('DPD', 'FED') and
013100160912     C+            (ecfaas > 0     and
013200160912     C+             ecfaas*10000+ecfmgs < :dataEEX )
013300160912     C+               or
013400160916     C+             ecfNTW not in ('DPD', 'FED') and
013500160912     C+            (ecfdfp > 0     and
013600160912     C+             ecfdfp < :dataEEX )
013700160912     C/END-EXEC
013800160912      *
013900160715      *
014000160715      *
014100160715     C/EXEC SQL
014200160912     C+    DELETE   FROM       wfdpdba0f
014300160912     C+       WHERE DBAYMO < :data7aDPD
014400160715     C/END-EXEC
014500160715      *
014600160714      *
014700160714      * PULIZIA TIDP300F
014800160714     C/EXEC SQL
014900160715     C+  DELETE   from  TIDP300f where
015000160714     C+  digits(dp3aas) concat  digits(dp3lnp) concat digits(dp3nrs) concat
015100160714     C+  digits(dp3nsp)  not in (select
015200160714     C+  digits(blpaas) concat  digits(blplnp) concat digits(blpnrs) concat
015300160714     C+  digits(blpnsp)  from filtra201/fnblp00f)
015400160714     C+       and
015500160714     C+  digits(dp3aas) concat  digits(dp3lnp) concat digits(dp3nrs) concat
015600160714     C+  digits(dp3nsp)  not in (select
015700160714     C+  digits(arbaas) concat  digits(arblnp) concat digits(arbnrs) concat
015800160714     C+  digits(arbnsp)  from filtra201/fnarb00f)
015900160714     C+       and
016000160714     C+  digits(dp3aas) concat  digits(dp3lnp) concat digits(dp3nrs) concat
016100160714     C+  digits(dp3nsp)  not in (select
016200160714     C+  digits(vabaas) concat  digits(vablnp) concat digits(vabnrs) concat
016300160714     C+  digits(vabnsp)  from filtragru/fivab00f)
016400160714     C+       and
016500160714     C+  digits(dp3aas) concat  digits(dp3lnp) concat digits(dp3nrs) concat
016600160714     C+  digits(dp3nsp)  not in (select
016700160714     C+  digits(vabaas) concat  digits(vablnp) concat digits(vabnrs) concat
016800160714     C+  digits(vabnsp)  from filtragru/edivab0f)
016900160714     C+       and
017000160714     C+  digits(dp3aas) concat  digits(dp3lnp) concat digits(dp3nrs) concat
017100160714     C+  digits(dp3nsp)  not in (select
017200160714     C+  digits(ECFaas) concat  digits(ECFlnp) concat digits(ECFnrs) concat
017300160714     C+  digits(ECFnsp)  from tnECF00f where ECFntw='DPD')
017400160714     C+       and
017500160913     C+  DP3PRN not in (select substr(ECfrif, 1, 14 )
017600160913     C+    from tnECF00f where ECFntw='DPD')
017700160913     C+       and
017800160913     C+  DP3PRN not in (select substr(EFRrif, 1, 14 )
017900160913     C+    from tnEFR00f where EFRntw='DPD')
018000160919     C+       and
018100160919     C+  DP3DTSPE > 0 and DP3DTSPE < :dataDPD
018200160714     C/END-EXEC
018300160714      *
018400161012      *
018500161012      * PULIZIA TIDP200F
018600161012     C/EXEC SQL
018700161012     C+  DELETE   from  TIDP200f where
018800161012     C+   I2PARCEL not in
018900161012     C+    (select dp3prn from tidp300f)
019000161012     C+       and
019100161012     C+   I2PARCEL not in
019200161012     C+    (select substr(digits(blprmn), 2, 14) from filtra201/fnblp00f)
019300161012     C+       and
019400161012     C+   I2PARCEL not in
019500161012     C+    (select substr(digits(vabrmn), 2, 14) from  fivab00f)
019600161012     C+       and
019700161012     C+   I2PARCEL not in
019800161012     C+    (select substr(digits(vabrmn), 2, 14) from  edivab0f)
019900161012     C+       and
020000161012     C+   I2PARCEL not in
020100161012     C+    (select substr(efrRIF, 1, 14) from tnefr00F where EFRntw='DPD')
020200161012     C+       and
020300161012     C+   I2PARCEL not in
020400161012     C+    (select substr(ecfRIF, 1, 14) from tnecf00f where ECFntw='DPD')
020500161012     C+       and
020600161012     C+  DEC(INT(I2DATTIM/1000000), 8, 0) < :dataDPD  and I2DATTIM > 0
020700161012     C/END-EXEC
020800161012      *
020900161013      *
021000161013      * PULIZIA EDIVAB0F  "DPD_IN_SOSPESO"   prima della data di pulizia DPD
021100161013      *
021200161013     C/EXEC SQL
021300161013     C+   DELETE from filtragru/edivab0f
021400161013     C+    WHERE
021500161013     C+          VABCMR = '§DPD_IN_SOSPESO' and VABDCM < :dataDPD
021600161013     C/END-EXEC
021700161013      *
021800161013      *
021900161013      * PULIZIA EDIVAT0F  "DPD_IN_SOSPESO"  non presenti più i VAB
022000161013      *
022100161013     C/EXEC SQL
022200161013     C+   delete   FROM filtragru/edivat0f
022300161013     C+      WHERE
022400161013     C+           VATCMR  = '§DPD_IN_SOSPESO'   and
022500161013     C+    digits(vataas) concat digits(vatlnp) concat
022600161013     C+    digits(vatnrs) concat digits(vatnsp)
022700161013     c+      not in (select
022800161013     C+    digits(vabaas) concat digits(vablnp) concat
022900161013     C+    digits(vabnrs) concat digits(vabnsp)
023000161013     C+      from
023100161013     C+    filtragrpf/edivab0f
023200161013     C+      WHERE
023300161013     C+    VAbCMR = '§DPD_IN_SOSPESO' )
023400161013     C/END-EXEC
023500161013      *
023600160714     c                   RETURN
023700000714
023800160715      * --------------------------------------------------------------------
