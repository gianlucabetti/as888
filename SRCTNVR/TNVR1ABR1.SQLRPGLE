000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300050517     fWFFMA00F  uf a E           K DISK    UsrOPN
000400050511     f                                     prefix(M_)
000500050524     ffiapd01l  if   E           K DISK
000600050511     fTNVR1ABp  o    E             PRINTER OFLIND(*in66)
000700050510      **-----------------------------------------------------------------***
000800040820     d KPJBA         E DS
000900050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001000040820     D*
001100050510     D DsPassaggio   e DS                  EXTNAME(TNVR1ABds)
001200050510     D fiftt00f      e DS
001300050510     D azorg00f      e DS
001400040820     D*
001500040820     D WLBDAT          DS
001600040820     D  G02DAT                 1      8  0
001700040820     D  G02INV                 9     16  0
001800040820     D  G02ERR                17     17
001900040820     D  G02TGI                18     22  0
002000040820      *
002100040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002200040826     D  almeno_uno     s              1
002300040820      *
002400050510     D  FLD_Distr      s                   like(orgfl3)
002500050510     D  FLD_Area       s                   like(orgcar)
002600050510     D  FLD_Fgs        s                   like(fttfgs)
002700050510     D  FLD_Padr       s                   like(fttpdr)
002800050510     D  FLD_TotVal     s             11  3
002900050510     D  FLD_GioLav     s              5  0
003000050510     D  FLD_MedPadr    s             11  3
003100050510      *
003200050511     D  TOT_Dist       s                   like(M_FmeDist)
003300050511     D  TOT_Area       s                   like(M_FmeArea)
003400050511     D  TOT_POper      s                   like(M_FmePOper)
003500050510     D  TOT_SumMed     s             11  3
003600050510     D  TOT_NumAut     s              5  0
003700050510     D  TOT_MedGiorPO  s             11  3
003800050510      *
003900050511     D*-------------
004000050511     D* Reperimento nome PGM
004100050511     D STATUS         SDS           333
004200050511     D  DSPGM            *PROC
004300050511     D*-------------
004400050511      *
004500040820     D digits          C                   CONST('0123456789')
004600050511      * ?------------------------------------------------------------------ */
004700050511      * ? In questa Fase si rilevano i dati per singolo Autista
004800050511      * ? dai Conteggi Padroncini prendendo i costi x giornata.
004900050511      * ? Poi si possono individuare, contandole, le giornate che
005000050511      * ? l'Autista ha lavorato.
005100050511      * ? Con il Totale x Autista diviso le sue giornate lavorate
005200050511      * ? si ha il costo medio di ogni singolo Autista
005300050511      * ? e scrivo un record sul File Statistico.
005400050510      **
005500050510      **  Il File è organizzato x Distretto/Area/P.O./Autista
005600050510      **   i cui dati sono:
005700050510      **     TOTALE VALORE x AUTISTA (di tutti i Giorni Lavorati)
005800050510      **     TOTALE Giorni LAVORATI
005900050510      **     MEDIA Giornaliera x Autista
006000050510      **           ottenuta come TOT_VALORE diviso TOT_Giorni_LAVORATI
006100050510      **  Con questa base si ottengono i valori x Singola Giornata Autista.
006200050511      * ?------------------------------------------------------------------ */
006300050510     C/EXEC SQL
006400050510     C+ Declare A1 Cursor for
006500050510     C+ SELECT orgfl3, orgcar, fttfgs, fttpdr,
006600050510     C+   decimal(sum(Tot_Costo), 11, 3) as Tot_Valore,
006700050510     C+   decimal(count(*), 5, 0) as Num_Giorni,
006800050510     C+   decimal(sum(TOT_Costo)/count(*), 11, 3) as Media_Giornata
006900050510     C+ FROM (
007000050510     C+ SELECT orgfl3, orgcar, fttfgs, FTTPDR, fttddc,
007100050510     C+ decimal(sum(FTTITT + FTTITA + FTTTPE + FTTTBN + FTTTIM + FTTMNT),
007200050510     C+ 11, 3) as Tot_Costo FROM FIFTT00F join AZORG01L on FTTFGS=ORGFIL
007300050517     C+ WHERE ftttsr =' ' and fttfvl = 'C' and fttNFF <> 999999999
007400050510     C+   AND fttddc between :DADATA  and :ADATA
007500050510     C+ GROUP BY  orgfl3, orgcar, fttpdr, fttddc, fttfgs
007600050510     C+ ) as A
007700050510     C+ GROUP BY  orgfl3, orgcar, fttfgs, fttpdr
007800050510     C+ ORDER BY  orgfl3, orgcar, fttfgs, fttpdr
007900050510     C/END-EXEC
008000050510      *          apertura cursore
008100050510     C/EXEC SQL
008200050510     C+ OPEN A1
008300050510     C/END-EXEC
008400050510     c                   clear                   almeno_uno
008500050510      **
008600050510     C                   DOU       SqlCod <> 0
008700050510      **         lettura cursore
008800050510     C/EXEC SQL
008900050510     C+ Fetch Next From A1 Into :FLD_Distr, :FLD_Area, :FLD_Fgs,
009000050510     C+       :FLD_Padr, :FLD_TotVal, :FLD_GioLav, :FLD_MedPadr
009100050510     C/END-EXEC
009200050510     C                   SELECT
009300050510     **
009400050510     C                   WHEN      SqlCod = 100
009500050510     c* a fine file
009600050510     c                   if        almeno_uno = 'S'
009700050510     c                   else
009800050510     c                   end
009900050510     c                   leave
010000050510     c*
010100050510     C                   WHEN      SqlCod < 0
010200050510     C                   seton                                        H1
010300050510     c                   goto      fine
010400050510     c*
010500050510     C                   OTHER
010600050510     **
010700050510     c* DETTAGLIO
010800050510     c* riga   dettaglio
010900050510     c                   exsr      Wri_rig_det
011000050510     c*
011100050510     C                   ENDSL
011200050510
011300050510     C                   ENDDO
011400050510     C/EXEC SQL
011500050510     C+ Close A1
011600050510     C/END-EXEC
011700050510     c*
011800050510     c*  Chiude il File per eseguire SQL finale di totalizzazione
011900050517     c                   Close     WFFMA00F
012000050510     c* Totali
012100050510     c                   exsr      totalizza
012200050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
012300040823      *
012400041007     c     fine          tag
012500020326     C                   SETON                                        LR
012600050511     ?****************************************************************************
012700050511      *?    Scrive Riga File di Work
012800050511     ?****************************************************************************
012900050510     c     Wri_rig_det   begsr
013000050510      *
013100050511     c                   move      'S'           almeno_uno
013200050524      *
013300050510     c                   clear                   WFFME000
013400050510      *
013500050524     c                   eval      APDTIP = 'A'
013600050524     c                   eval      APDPDR =  FLD_Padr
013700050524     c     K_aut         chain     fiapd01l
013800050524     c                   if        %Found(fiapd01l)
013900050524     c                   eval      M_FMEPESO   = apdPKL
014000050524     c                   eval      M_FMEVOLUME = apdVLU
014100050524     c                   end
014200050511     c                   eval      M_FMEDIST   = FLD_Distr
014300050511     c                   eval      M_FMEAREA   = FLD_Area
014400050511     c                   eval      M_FMEPOPER  = FLD_Fgs
014500050511     c                   eval      M_FMEAUTIST = FLD_Padr
014600050511     c                   eval      M_FMETOTVAL = FLD_TotVal
014700050511     c                   eval      M_FMENGILAV = FLD_GioLav
014800050511     c                   eval      M_FMEMEDGIO = FLD_MedPadr
014900050511     c                   eval      M_FMEDADATA = pasdti
015000050511     c                   eval      M_FMEADATA  = pasdtf
015100050510      *
015200050510     c                   write     WFFME000
015300050510      *
015400050510     c                   endsr
015500050511     ?****************************************************************************
015600050511      *?   Totali da  File di Work
015700050511     ?****************************************************************************
015800050510     c     Totalizza     begsr
015900050510      *
016000050517     c                   Open      WFFMA00F
016100050511      * ?------------------------------------------------------------------ */
016200050511      **  Con il file Ricavato Precedentemente x Autista è possibile
016300050511      **   totalizzare e ricavare le medie x P.O..
016400050511      **   Con il totale di tutti gli Autisti del P.O. diviso il numero
016500050510      **   stesso degli Autisti del P.O. si ottiene il valore medio
016600050510      **   a giornata del P.O.
016700050511      * ?------------------------------------------------------------------ */
016800050510     C/EXEC SQL
016900050510     C+ Declare B2 Cursor for
017000050510     C+  SELECT FMEDIST, FMEAREA, FMEPOPER,
017100050510     C+  decimal(sum(FMEMEDGIO), 11, 3) as TOT_PO_MED,
017200050510     C+  decimal( COUNT(*), 5, 0) as NUM_AUT_PO,
017300050510     C+  decimal(sum(FMEMEDGIO)/COUNT(*), 11, 3) as MEDIA_PO
017400050517     C+  FROM WFFMA00F
017500050510     C+  GROUP BY FMEDIST, FMEAREA, FMEPOPER
017600050510     C+  ORDER BY FMEDIST, FMEAREA, FMEPOPER
017700050510     C/END-EXEC
017800050510      *          apertura cursore
017900050510     C/EXEC SQL
018000050510     C+ OPEN B2
018100050510     C/END-EXEC
018200050510     c                   clear                   almeno_uno
018300050510      **
018400050510     C                   DOU       SqlCod <> 0
018500050510      **         lettura cursore
018600050510     C/EXEC SQL
018700050510     C+ Fetch Next From B2 Into :TOT_Dist, :TOT_Area, :TOT_POper,
018800050510     C+       :TOT_SumMed, :TOT_NumAut, :TOT_MedGiorPO
018900050510     C/END-EXEC
019000050510     C                   SELECT
019100050510     **
019200050510     C                   WHEN      SqlCod = 100
019300050510     c* a fine file
019400050510     c                   if        almeno_uno = 'S'
019500050510     c                   else
019600050511     c                   if        pasLST = 'S'
019700050511     c                   exsr      PRT_NO_Dati
019800050511     c                   endIF
019900050510     c                   end
020000050510     c                   leave
020100050510     c*
020200050510     C                   WHEN      SqlCod < 0
020300050510     C                   seton                                        H2
020400050510     c                   goto      fine_SR
020500050510     c*
020600050510     C                   OTHER
020700050510     **
020800050510     c* DETTAGLIO
020900050510     c* riga   dettaglio
021000050510     c                   exsr      Agg_rig_det
021100050511     c                   if        pasLST = 'S'
021200050511     c                   exsr      PRT_rig_det
021300050511     c                   endIF
021400050510     c*
021500050510     C                   ENDSL
021600050510
021700050510     C                   ENDDO
021800050510     C/EXEC SQL
021900050510     C+ Close B2
022000050510     C/END-EXEC
022100050510     c
022200050510     c     fine_SR       tag
022300050517     c                   Close     WFFMA00F
022400050510      *
022500050510     c                   endsr
022600050511     ?****************************************************************************
022700050511      *?   Stampa      File di Work Una riga x P.O.
022800050511     ?****************************************************************************
022900050511     c     PRT_rig_det   begsr
023000050511      *
023100050511     c                   if        *in66
023200050511      *                 * ------------------ *
023300050511     c                   write     testa
023400050511      *                 * ------------------ *
023500050511     c                   setoff                                       66
023600050511     c                   end
023700050511      *
023800050511     c                   eval      PDist   = TOT_Dist
023900050511     C                   eval      PArea   = TOT_Area
024000050511     C                   eval      PPOper  = TOT_POper
024100050511     c                   eval      PNumAut = TOT_NumAut
024200050511     c                   eval      PMedia  = TOT_MedGiorPO
024300050511      *
024400050511      *                 * ------------------ *
024500050511     c                   write     dettagli
024600050511      *                 * ------------------ *
024700050511      *
024800050511     c                   endsr
024900050511     ?****************************************************************************
025000050511      *?   Aggiorna    File di Work
025100050511     ?****************************************************************************
025200050511     c     Agg_rig_det   begsr
025300050511      *
025400050511     c                   move      'S'           almeno_uno
025500050511      *
025600050517     C     K_MED         setll     WFFMA00F
025700050517     C     K_MED         reade     WFFMA00F
025800050511      *
025900050517     c                   dow       not %Eof(WFFMA00F)
026000050511      *
026100050511     c                   eval      M_FMENUMAUT = TOT_NumAut
026200050511     c                   eval      M_FMEMEDPO  = TOT_MedGiorPO
026300050511      *
026400050511     c                   update    WFFME000
026500050511      *
026600050517     C     K_MED         reade     WFFMA00F
026700050511     c                   enddo
026800050510      *
026900050510     c                   endsr
027000050511     ?****************************************************************************
027100050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
027200050511     ?****************************************************************************
027300050511     c     PRT_NO_Dati   begsr
027400050511      *
027500050511     c                   if        *in66
027600050511      *                 * ------------------ *
027700050511     c                   write     testa
027800050511      *                 * ------------------ *
027900050511     c                   setoff                                       66
028000050511     c                   end
028100050511      *
028200050511      *                 * ------------------ *
028300050511     c                   write     nodati
028400050511      *                 * ------------------ *
028500050511      *
028600050511     c                   endsr
028700050511     ?****************************************************************************
028800050511      *?   INIZIALIZZAZIONE
028900050511     ?****************************************************************************
029000040820     C     *inzsr        BEGSR
029100040820      **
029200050511     C     K_MED         KLIST
029300050511     C                   Kfld                    TOT_Dist
029400050511     C                   Kfld                    TOT_Area
029500050511     C                   Kfld                    TOT_POper
029600050524      **
029700050524     C     K_aut         KLIST
029800050524     C                   Kfld                    APDTIP
029900050524     C                   Kfld                    APDPDR
030000050511      **
030100040820     C     *ENTRY        PLIST
030200040820     C                   PARM                    KPJBA
030300050510     c                   movel     kpjbu         DsPassaggio
030400050511      **
030500050511     C                   Z-ADD     1             CODUT
030600050511     C                   CALL      'X§PARUT'
030700050511     C                   PARM                    UT§DSE
030800050511     C                   MOVEL     RAGUT         RSUT             20
030900041103      *
031000040820      *  imposta UDATE di elaborazione
031100040820     C                   move      *date         G02DAT
031200040820     C                   MOVE      *ZEROS        G02INV
031300040820     C                   MOVE      *BLANKS       G02ERR
031400040820     C                   CALL      'XSRDA8'
031500040820     C                   PARM                    WLBDAT
031600040820     C                   Z-ADD     G02INV        data_oggi
031700050511     C                   move      G02dat        day
031800040820     C                   TIME                    W0140            14 0
031900050511     C                   MOVEl     W0140         ore
032000040823      **
032100050510     c                   move      PASdti        dadata            8 0
032200050510     c                   move      PASdtf        adata             8 0
032300040823      **
032400050511     c                   seton                                        66
032500040825     c                   clear                   prima_volta       1
032600040826     c                   clear                   almeno_uno
032700050511     C                   MOVEL     DSPGM         NOMPGM
032800050511      **
032900050511     C                   move      *zeros        G02DAT
033000050511     C                   MOVE      dadata        G02INV
033100050511     C                   MOVE      '3'           G02ERR
033200050511     C                   CALL      'XSRDA8'
033300050511     C                   PARM                    WLBDAT
033400050511     C                   Z-ADD     G02Dat        PDadata
033500050511      **
033600050511     C                   move      *zeros        G02DAT
033700050511     C                   MOVE      adata         G02INV
033800050511     C                   MOVE      '3'           G02ERR
033900050511     C                   CALL      'XSRDA8'
034000050511     C                   PARM                    WLBDAT
034100050511     C                   Z-ADD     G02dat        PAdata
034200040820     c*
034300050517     c                   Open      WFFMA00F
034400050510     c*
034500040820     C                   ENDSR
034600050511     ?****************************************************************************
