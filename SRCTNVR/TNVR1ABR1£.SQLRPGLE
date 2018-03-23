000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300050517     fWFFMA00F  uf a E           K DISK    UsrOPN
000400050511     f                                     prefix(M_)
000500050511     fTNVR1ABp  o    E             PRINTER OFLIND(*in66)
000600050510      **-----------------------------------------------------------------***
000700040820     d KPJBA         E DS
000800050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000900040820     D*
001000050510     D DsPassaggio   e DS                  EXTNAME(TNVR1ABds)
001100050510     D fiftt00f      e DS
001200050510     D azorg00f      e DS
001300040820     D*
001400040820     D WLBDAT          DS
001500040820     D  G02DAT                 1      8  0
001600040820     D  G02INV                 9     16  0
001700040820     D  G02ERR                17     17
001800040820     D  G02TGI                18     22  0
001900040820      *
002000040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002100040826     D  almeno_uno     s              1
002200040820      *
002300050510     D  FLD_Distr      s                   like(orgfl3)
002400050510     D  FLD_Area       s                   like(orgcar)
002500050510     D  FLD_Fgs        s                   like(fttfgs)
002600050510     D  FLD_Padr       s                   like(fttpdr)
002700050510     D  FLD_TotVal     s             11  3
002800050510     D  FLD_GioLav     s              5  0
002900050510     D  FLD_MedPadr    s             11  3
003000050510      *
003100050511     D  TOT_Dist       s                   like(M_FmeDist)
003200050511     D  TOT_Area       s                   like(M_FmeArea)
003300050511     D  TOT_POper      s                   like(M_FmePOper)
003400050510     D  TOT_SumMed     s             11  3
003500050510     D  TOT_NumAut     s              5  0
003600050510     D  TOT_MedGiorPO  s             11  3
003700050510      *
003800050511     D*-------------
003900050511     D* Reperimento nome PGM
004000050511     D STATUS         SDS           333
004100050511     D  DSPGM            *PROC
004200050511     D*-------------
004300050511      *
004400040820     D digits          C                   CONST('0123456789')
004500050511      * ?------------------------------------------------------------------ */
004600050511      * ? In questa Fase si rilevano i dati per singolo Autista
004700050511      * ? dai Conteggi Padroncini prendendo i costi x giornata.
004800050511      * ? Poi si possono individuare, contandole, le giornate che
004900050511      * ? l'Autista ha lavorato.
005000050511      * ? Con il Totale x Autista diviso le sue giornate lavorate
005100050511      * ? si ha il costo medio di ogni singolo Autista
005200050511      * ? e scrivo un record sul File Statistico.
005300050510      **
005400050510      **  Il File è organizzato x Distretto/Area/P.O./Autista
005500050510      **   i cui dati sono:
005600050510      **     TOTALE VALORE x AUTISTA (di tutti i Giorni Lavorati)
005700050510      **     TOTALE Giorni LAVORATI
005800050510      **     MEDIA Giornaliera x Autista
005900050510      **           ottenuta come TOT_VALORE diviso TOT_Giorni_LAVORATI
006000050510      **  Con questa base si ottengono i valori x Singola Giornata Autista.
006100050511      * ?------------------------------------------------------------------ */
006200050510     C/EXEC SQL
006300050510     C+ Declare A1 Cursor for
006400050510     C+ SELECT orgfl3, orgcar, fttfgs, fttpdr,
006500050510     C+   decimal(sum(Tot_Costo), 11, 3) as Tot_Valore,
006600050510     C+   decimal(count(*), 5, 0) as Num_Giorni,
006700050510     C+   decimal(sum(TOT_Costo)/count(*), 11, 3) as Media_Giornata
006800050510     C+ FROM (
006900050510     C+ SELECT orgfl3, orgcar, fttfgs, FTTPDR, fttddc,
007000050510     C+ decimal(sum(FTTITT + FTTITA + FTTTPE + FTTTBN + FTTTIM + FTTMNT),
007100050510     C+ 11, 3) as Tot_Costo FROM FIFTT00F join AZORG01L on FTTFGS=ORGFIL
007200050517     C+ WHERE ftttsr =' ' and fttfvl = 'C' and fttNFF <> 999999999
007300050510     C+   AND fttddc between :DADATA  and :ADATA
007400050510     C+ GROUP BY  orgfl3, orgcar, fttpdr, fttddc, fttfgs
007500050510     C+ ) as A
007600050510     C+ GROUP BY  orgfl3, orgcar, fttfgs, fttpdr
007700050510     C+ ORDER BY  orgfl3, orgcar, fttfgs, fttpdr
007800050510     C/END-EXEC
007900050510      *          apertura cursore
008000050510     C/EXEC SQL
008100050510     C+ OPEN A1
008200050510     C/END-EXEC
008300050510     c                   clear                   almeno_uno
008400050510      **
008500050510     C                   DOU       SqlCod <> 0
008600050510      **         lettura cursore
008700050510     C/EXEC SQL
008800050510     C+ Fetch Next From A1 Into :FLD_Distr, :FLD_Area, :FLD_Fgs,
008900050510     C+       :FLD_Padr, :FLD_TotVal, :FLD_GioLav, :FLD_MedPadr
009000050510     C/END-EXEC
009100050510     C                   SELECT
009200050510     **
009300050510     C                   WHEN      SqlCod = 100
009400050510     c* a fine file
009500050510     c                   if        almeno_uno = 'S'
009600050510     c                   else
009700050510     c                   end
009800050510     c                   leave
009900050510     c*
010000050510     C                   WHEN      SqlCod < 0
010100050510     C                   seton                                        H1
010200050510     c                   goto      fine
010300050510     c*
010400050510     C                   OTHER
010500050510     **
010600050510     c* DETTAGLIO
010700050510     c* riga   dettaglio
010800050510     c                   exsr      Wri_rig_det
010900050510     c*
011000050510     C                   ENDSL
011100050510
011200050510     C                   ENDDO
011300050510     C/EXEC SQL
011400050510     C+ Close A1
011500050510     C/END-EXEC
011600050510     c*
011700050510     c*  Chiude il File per eseguire SQL finale di totalizzazione
011800050517     c                   Close     WFFMA00F
011900050510     c* Totali
012000050510     c                   exsr      totalizza
012100050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
012200040823      *
012300041007     c     fine          tag
012400020326     C                   SETON                                        LR
012500050511     ?****************************************************************************
012600050511      *?    Scrive Riga File di Work
012700050511     ?****************************************************************************
012800050510     c     Wri_rig_det   begsr
012900050510      *
013000050511     c                   move      'S'           almeno_uno
013100050511      *
013200050510     c                   clear                   WFFME000
013300050510      *
013400050511     c                   eval      M_FMEDIST   = FLD_Distr
013500050511     c                   eval      M_FMEAREA   = FLD_Area
013600050511     c                   eval      M_FMEPOPER  = FLD_Fgs
013700050511     c                   eval      M_FMEAUTIST = FLD_Padr
013800050511     c                   eval      M_FMETOTVAL = FLD_TotVal
013900050511     c                   eval      M_FMENGILAV = FLD_GioLav
014000050511     c                   eval      M_FMEMEDGIO = FLD_MedPadr
014100050511     c                   eval      M_FMEDADATA = pasdti
014200050511     c                   eval      M_FMEADATA  = pasdtf
014300050510      *
014400050510     c                   write     WFFME000
014500050510      *
014600050510     c                   endsr
014700050511     ?****************************************************************************
014800050511      *?   Totali da  File di Work
014900050511     ?****************************************************************************
015000050510     c     Totalizza     begsr
015100050510      *
015200050517     c                   Open      WFFMA00F
015300050511      * ?------------------------------------------------------------------ */
015400050511      **  Con il file Ricavato Precedentemente x Autista è possibile
015500050511      **   totalizzare e ricavare le medie x P.O..
015600050511      **   Con il totale di tutti gli Autisti del P.O. diviso il numero
015700050510      **   stesso degli Autisti del P.O. si ottiene il valore medio
015800050510      **   a giornata del P.O.
015900050511      * ?------------------------------------------------------------------ */
016000050510     C/EXEC SQL
016100050510     C+ Declare B2 Cursor for
016200050510     C+  SELECT FMEDIST, FMEAREA, FMEPOPER,
016300050510     C+  decimal(sum(FMEMEDGIO), 11, 3) as TOT_PO_MED,
016400050510     C+  decimal( COUNT(*), 5, 0) as NUM_AUT_PO,
016500050510     C+  decimal(sum(FMEMEDGIO)/COUNT(*), 11, 3) as MEDIA_PO
016600050517     C+  FROM WFFMA00F
016700050510     C+  GROUP BY FMEDIST, FMEAREA, FMEPOPER
016800050510     C+  ORDER BY FMEDIST, FMEAREA, FMEPOPER
016900050510     C/END-EXEC
017000050510      *          apertura cursore
017100050510     C/EXEC SQL
017200050510     C+ OPEN B2
017300050510     C/END-EXEC
017400050510     c                   clear                   almeno_uno
017500050510      **
017600050510     C                   DOU       SqlCod <> 0
017700050510      **         lettura cursore
017800050510     C/EXEC SQL
017900050510     C+ Fetch Next From B2 Into :TOT_Dist, :TOT_Area, :TOT_POper,
018000050510     C+       :TOT_SumMed, :TOT_NumAut, :TOT_MedGiorPO
018100050510     C/END-EXEC
018200050510     C                   SELECT
018300050510     **
018400050510     C                   WHEN      SqlCod = 100
018500050510     c* a fine file
018600050510     c                   if        almeno_uno = 'S'
018700050510     c                   else
018800050511     c                   if        pasLST = 'S'
018900050511     c                   exsr      PRT_NO_Dati
019000050511     c                   endIF
019100050510     c                   end
019200050510     c                   leave
019300050510     c*
019400050510     C                   WHEN      SqlCod < 0
019500050510     C                   seton                                        H2
019600050510     c                   goto      fine_SR
019700050510     c*
019800050510     C                   OTHER
019900050510     **
020000050510     c* DETTAGLIO
020100050510     c* riga   dettaglio
020200050510     c                   exsr      Agg_rig_det
020300050511     c                   if        pasLST = 'S'
020400050511     c                   exsr      PRT_rig_det
020500050511     c                   endIF
020600050510     c*
020700050510     C                   ENDSL
020800050510
020900050510     C                   ENDDO
021000050510     C/EXEC SQL
021100050510     C+ Close B2
021200050510     C/END-EXEC
021300050510     c
021400050510     c     fine_SR       tag
021500050517     c                   Close     WFFMA00F
021600050510      *
021700050510     c                   endsr
021800050511     ?****************************************************************************
021900050511      *?   Stampa      File di Work Una riga x P.O.
022000050511     ?****************************************************************************
022100050511     c     PRT_rig_det   begsr
022200050511      *
022300050511     c                   if        *in66
022400050511      *                 * ------------------ *
022500050511     c                   write     testa
022600050511      *                 * ------------------ *
022700050511     c                   setoff                                       66
022800050511     c                   end
022900050511      *
023000050511     c                   eval      PDist   = TOT_Dist
023100050511     C                   eval      PArea   = TOT_Area
023200050511     C                   eval      PPOper  = TOT_POper
023300050511     c                   eval      PNumAut = TOT_NumAut
023400050511     c                   eval      PMedia  = TOT_MedGiorPO
023500050511      *
023600050511      *                 * ------------------ *
023700050511     c                   write     dettagli
023800050511      *                 * ------------------ *
023900050511      *
024000050511     c                   endsr
024100050511     ?****************************************************************************
024200050511      *?   Aggiorna    File di Work
024300050511     ?****************************************************************************
024400050511     c     Agg_rig_det   begsr
024500050511      *
024600050511     c                   move      'S'           almeno_uno
024700050511      *
024800050517     C     K_MED         setll     WFFMA00F
024900050517     C     K_MED         reade     WFFMA00F
025000050511      *
025100050517     c                   dow       not %Eof(WFFMA00F)
025200050511      *
025300050511     c                   eval      M_FMENUMAUT = TOT_NumAut
025400050511     c                   eval      M_FMEMEDPO  = TOT_MedGiorPO
025500050511      *
025600050511     c                   update    WFFME000
025700050511      *
025800050517     C     K_MED         reade     WFFMA00F
025900050511     c                   enddo
026000050510      *
026100050510     c                   endsr
026200050511     ?****************************************************************************
026300050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
026400050511     ?****************************************************************************
026500050511     c     PRT_NO_Dati   begsr
026600050511      *
026700050511     c                   if        *in66
026800050511      *                 * ------------------ *
026900050511     c                   write     testa
027000050511      *                 * ------------------ *
027100050511     c                   setoff                                       66
027200050511     c                   end
027300050511      *
027400050511      *                 * ------------------ *
027500050511     c                   write     nodati
027600050511      *                 * ------------------ *
027700050511      *
027800050511     c                   endsr
027900050511     ?****************************************************************************
028000050511      *?   INIZIALIZZAZIONE
028100050511     ?****************************************************************************
028200040820     C     *inzsr        BEGSR
028300040820      **
028400050511     C     K_MED         KLIST
028500050511     C                   Kfld                    TOT_Dist
028600050511     C                   Kfld                    TOT_Area
028700050511     C                   Kfld                    TOT_POper
028800050511      **
028900040820     C     *ENTRY        PLIST
029000040820     C                   PARM                    KPJBA
029100050510     c                   movel     kpjbu         DsPassaggio
029200050511      **
029300050511     C                   Z-ADD     1             CODUT
029400050511     C                   CALL      'X§PARUT'
029500050511     C                   PARM                    UT§DSE
029600050511     C                   MOVEL     RAGUT         RSUT             20
029700041103      *
029800040820      *  imposta UDATE di elaborazione
029900040820     C                   move      *date         G02DAT
030000040820     C                   MOVE      *ZEROS        G02INV
030100040820     C                   MOVE      *BLANKS       G02ERR
030200040820     C                   CALL      'XSRDA8'
030300040820     C                   PARM                    WLBDAT
030400040820     C                   Z-ADD     G02INV        data_oggi
030500050511     C                   move      G02dat        day
030600040820     C                   TIME                    W0140            14 0
030700050511     C                   MOVEl     W0140         ore
030800040823      **
030900050510     c                   move      PASdti        dadata            8 0
031000050510     c                   move      PASdtf        adata             8 0
031100040823      **
031200050511     c                   seton                                        66
031300040825     c                   clear                   prima_volta       1
031400040826     c                   clear                   almeno_uno
031500050511     C                   MOVEL     DSPGM         NOMPGM
031600050511      **
031700050511     C                   move      *zeros        G02DAT
031800050511     C                   MOVE      dadata        G02INV
031900050511     C                   MOVE      '3'           G02ERR
032000050511     C                   CALL      'XSRDA8'
032100050511     C                   PARM                    WLBDAT
032200050511     C                   Z-ADD     G02Dat        PDadata
032300050511      **
032400050511     C                   move      *zeros        G02DAT
032500050511     C                   MOVE      adata         G02INV
032600050511     C                   MOVE      '3'           G02ERR
032700050511     C                   CALL      'XSRDA8'
032800050511     C                   PARM                    WLBDAT
032900050511     C                   Z-ADD     G02dat        PAdata
033000040820     c*
033100050517     c                   Open      WFFMA00F
033200050510     c*
033300040820     C                   ENDSR
033400050511     ?****************************************************************************
