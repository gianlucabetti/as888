000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300050517     fWFFMA00F  uf a E           K DISK    UsrOPN
000400050511     f                                     prefix(M_)
000500050524     ffiapd01l  if   E           K DISK
000600050829-->  fTISE85p   o    E             PRINTER OFLIND(*in66)
000700050510      **-----------------------------------------------------------------***
000800040820     d KPJBA         E DS
000900050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001000050905     D dapdflr       E DS
001100040820     D*
001200050829-->  D DsPassaggio   e DS                  EXTNAME(TISE85ds)
001300050510     D fiftt00f      e DS
001400050510     D azorg00f      e DS
001500040820     D*
001600040820     D WLBDAT          DS
001700040820     D  G02DAT                 1      8  0
001800040820     D  G02INV                 9     16  0
001900040820     D  G02ERR                17     17
002000040820     D  G02TGI                18     22  0
002100040820      *
002200040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002300040826     D  almeno_uno     s              1
002400040820      *
002500050510     D  FLD_Distr      s                   like(orgfl3)
002600050510     D  FLD_Area       s                   like(orgcar)
002700050510     D  FLD_Fgs        s                   like(fttfgs)
002800050510     D  FLD_Padr       s                   like(fttpdr)
002900050905     D  FLD_Spediz     s              7  0
003000050510     D  FLD_TotVal     s             11  3
003100050510     D  FLD_GioLav     s              5  0
003200050510     D  FLD_MedPadr    s             11  3
003300050510      *
003400050511     D  TOT_Dist       s                   like(M_FmeDist)
003500050511     D  TOT_Area       s                   like(M_FmeArea)
003600050511     D  TOT_POper      s                   like(M_FmePOper)
003700050510     D  TOT_SumMed     s             11  3
003800050510     D  TOT_NumAut     s              5  0
003900050510     D  TOT_MedGiorPO  s             11  3
004000050510      *
004100050511     D*-------------
004200050511     D* Reperimento nome PGM
004300050511     D STATUS         SDS           333
004400050511     D  DSPGM            *PROC
004500050511     D*-------------
004600050511      *
004700040820     D digits          C                   CONST('0123456789')
004800050511      * ?------------------------------------------------------------------ */
004900050511      * ? In questa Fase si rilevano i dati per singolo Autista
005000050511      * ? dai Conteggi Padroncini prendendo i costi x giornata.
005100050511      * ? Poi si possono individuare, contandole, le giornate che
005200050511      * ? l'Autista ha lavorato.
005300050511      * ? Con il Totale x Autista diviso le sue giornate lavorate
005400050511      * ? si ha il costo medio di ogni singolo Autista
005500050511      * ? e scrivo un record sul File Statistico.
005600050510      **
005700050510      **  Il File è organizzato x Distretto/Area/P.O./Autista
005800050510      **   i cui dati sono:
005900050510      **     TOTALE VALORE x AUTISTA (di tutti i Giorni Lavorati)
006000050510      **     TOTALE Giorni LAVORATI
006100050510      **     MEDIA Giornaliera x Autista
006200050510      **           ottenuta come TOT_VALORE diviso TOT_Giorni_LAVORATI
006300050510      **  Con questa base si ottengono i valori x Singola Giornata Autista.
006400050511      * ?------------------------------------------------------------------ */
006500050510     C/EXEC SQL
006600050510     C+ Declare A1 Cursor for
006700050510     C+ SELECT orgfl3, orgcar, fttfgs, fttpdr,
006800050905     C+   decimal(sum(Sped_gior),  7, 0) as Tot_Sped,
006900050905     C+   decimal(sum(Tot_Costo), 11, 3) as Tot_Valore,
007000050510     C+   decimal(count(*), 5, 0) as Num_Giorni,
007100050510     C+   decimal(sum(TOT_Costo)/count(*), 11, 3) as Media_Giornata
007200050510     C+ FROM (
007300050510     C+ SELECT orgfl3, orgcar, fttfgs, FTTPDR, fttddc,
007400050905     C+ decimal(sum(fttset), 7, 0) as Sped_gior,
007500050510     C+ decimal(sum(FTTITT + FTTITA + FTTTPE + FTTTBN + FTTTIM + FTTMNT),
007600050510     C+ 11, 3) as Tot_Costo FROM FIFTT00F join AZORG01L on FTTFGS=ORGFIL
007700050517     C+ WHERE ftttsr =' ' and fttfvl = 'C' and fttNFF <> 999999999
007800050510     C+   AND fttddc between :DADATA  and :ADATA
007900050510     C+ GROUP BY  orgfl3, orgcar, fttpdr, fttddc, fttfgs
008000050510     C+ ) as A
008100050510     C+ GROUP BY  orgfl3, orgcar, fttfgs, fttpdr
008200050510     C+ ORDER BY  orgfl3, orgcar, fttfgs, fttpdr
008300050510     C/END-EXEC
008400050510      *          apertura cursore
008500050510     C/EXEC SQL
008600050510     C+ OPEN A1
008700050510     C/END-EXEC
008800050510     c                   clear                   almeno_uno
008900050510      **
009000050510     C                   DOU       SqlCod <> 0
009100050510      **         lettura cursore
009200050510     C/EXEC SQL
009300050905     C+ FETCH NEXT FROM A1 INTO :FLD_Distr, :FLD_Area, :FLD_Fgs,
009400050905     C+ :FLD_Padr, :FLD_Spediz, :FLD_TotVal, :FLD_GioLav, :FLD_MedPadr
009500050510     C/END-EXEC
009600050510     C                   SELECT
009700050510     **
009800050510     C                   WHEN      SqlCod = 100
009900050510     c* a fine file
010000050510     c                   if        almeno_uno = 'S'
010100050510     c                   else
010200050510     c                   end
010300050510     c                   leave
010400050510     c*
010500050510     C                   WHEN      SqlCod < 0
010600050510     C                   seton                                        H1
010700050510     c                   goto      fine
010800050510     c*
010900050510     C                   OTHER
011000050510     **
011100050510     c* DETTAGLIO
011200050510     c* riga   dettaglio
011300050510     c                   exsr      Wri_rig_det
011400050510     c*
011500050510     C                   ENDSL
011600050510
011700050510     C                   ENDDO
011800050510     C/EXEC SQL
011900050510     C+ Close A1
012000050510     C/END-EXEC
012100050510     c*
012200050510     c*  Chiude il File per eseguire SQL finale di totalizzazione
012300050517     c                   Close     WFFMA00F
012400050510     c* Totali
012500050510     c                   exsr      totalizza
012600050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
012700040823      *
012800041007     c     fine          tag
012900020326     C                   SETON                                        LR
013000050511     ?****************************************************************************
013100050511      *?    Scrive Riga File di Work
013200050511     ?****************************************************************************
013300050510     c     Wri_rig_det   begsr
013400050510      *
013500050511     c                   move      'S'           almeno_uno
013600050524      *
013700050510     c                   clear                   WFFME000
013800050510      *
013900050524     c                   eval      APDTIP = 'A'
014000050524     c                   eval      APDPDR =  FLD_Padr
014100050524     c     K_aut         chain     fiapd01l
014200050524     c                   if        %Found(fiapd01l)
014300050905     c                   movel     apdflr        dapdflr
014400050905      * è alfanumerico --> campo filler
014500050905     c                   if        §apdKM <> *blank
014600050905     c                   move      §apdKM        M_FMKMETRI
014700050905     c                   end
014800050905      *
014900050524     c                   eval      M_FMEPESO   = apdPKL
015000050524     c                   eval      M_FMEVOLUME = apdVLU
015100050524     c                   end
015200050511     c                   eval      M_FMEDIST   = FLD_Distr
015300050511     c                   eval      M_FMEAREA   = FLD_Area
015400050511     c                   eval      M_FMEPOPER  = FLD_Fgs
015500050511     c                   eval      M_FMEAUTIST = FLD_Padr
015600050905     c                   eval      M_FMSPEDIZ  = FLD_Spediz
015700050511     c                   eval      M_FMETOTVAL = FLD_TotVal
015800050511     c                   eval      M_FMENGILAV = FLD_GioLav
015900050511     c                   eval      M_FMEMEDGIO = FLD_MedPadr
016000050511     c                   eval      M_FMEDADATA = pasdti
016100050511     c                   eval      M_FMEADATA  = pasdtf
016200050510      *
016300050510     c                   write     WFFME000
016400050510      *
016500050510     c                   endsr
016600050511     ?****************************************************************************
016700050511      *?   Totali da  File di Work
016800050511     ?****************************************************************************
016900050510     c     Totalizza     begsr
017000050510      *
017100050517     c                   Open      WFFMA00F
017200050511      * ?------------------------------------------------------------------ */
017300050511      **  Con il file Ricavato Precedentemente x Autista è possibile
017400050511      **   totalizzare e ricavare le medie x P.O..
017500050511      **   Con il totale di tutti gli Autisti del P.O. diviso il numero
017600050510      **   stesso degli Autisti del P.O. si ottiene il valore medio
017700050510      **   a giornata del P.O.
017800050511      * ?------------------------------------------------------------------ */
017900050510     C/EXEC SQL
018000050510     C+ Declare B2 Cursor for
018100050510     C+  SELECT FMEDIST, FMEAREA, FMEPOPER,
018200050510     C+  decimal(sum(FMEMEDGIO), 11, 3) as TOT_PO_MED,
018300050510     C+  decimal( COUNT(*), 5, 0) as NUM_AUT_PO,
018400050510     C+  decimal(sum(FMEMEDGIO)/COUNT(*), 11, 3) as MEDIA_PO
018500050517     C+  FROM WFFMA00F
018600050510     C+  GROUP BY FMEDIST, FMEAREA, FMEPOPER
018700050510     C+  ORDER BY FMEDIST, FMEAREA, FMEPOPER
018800050510     C/END-EXEC
018900050510      *          apertura cursore
019000050510     C/EXEC SQL
019100050510     C+ OPEN B2
019200050510     C/END-EXEC
019300050510     c                   clear                   almeno_uno
019400050510      **
019500050510     C                   DOU       SqlCod <> 0
019600050510      **         lettura cursore
019700050510     C/EXEC SQL
019800050510     C+ Fetch Next From B2 Into :TOT_Dist, :TOT_Area, :TOT_POper,
019900050510     C+       :TOT_SumMed, :TOT_NumAut, :TOT_MedGiorPO
020000050510     C/END-EXEC
020100050510     C                   SELECT
020200050510     **
020300050510     C                   WHEN      SqlCod = 100
020400050510     c* a fine file
020500050510     c                   if        almeno_uno = 'S'
020600050510     c                   else
020700050511     c                   if        pasLST = 'S'
020800050511     c                   exsr      PRT_NO_Dati
020900050511     c                   endIF
021000050510     c                   end
021100050510     c                   leave
021200050510     c*
021300050510     C                   WHEN      SqlCod < 0
021400050510     C                   seton                                        H2
021500050510     c                   goto      fine_SR
021600050510     c*
021700050510     C                   OTHER
021800050510     **
021900050510     c* DETTAGLIO
022000050510     c* riga   dettaglio
022100050510     c                   exsr      Agg_rig_det
022200050511     c                   if        pasLST = 'S'
022300050511     c                   exsr      PRT_rig_det
022400050511     c                   endIF
022500050510     c*
022600050510     C                   ENDSL
022700050510
022800050510     C                   ENDDO
022900050510     C/EXEC SQL
023000050510     C+ Close B2
023100050510     C/END-EXEC
023200050510     c
023300050510     c     fine_SR       tag
023400050517     c                   Close     WFFMA00F
023500050510      *
023600050510     c                   endsr
023700050511     ?****************************************************************************
023800050511      *?   Stampa      File di Work Una riga x P.O.
023900050511     ?****************************************************************************
024000050511     c     PRT_rig_det   begsr
024100050511      *
024200050511     c                   if        *in66
024300050511      *                 * ------------------ *
024400050511     c                   write     testa
024500050511      *                 * ------------------ *
024600050511     c                   setoff                                       66
024700050511     c                   end
024800050511      *
024900050511     c                   eval      PDist   = TOT_Dist
025000050511     C                   eval      PArea   = TOT_Area
025100050511     C                   eval      PPOper  = TOT_POper
025200050511     c                   eval      PNumAut = TOT_NumAut
025300050511     c                   eval      PMedia  = TOT_MedGiorPO
025400050511      *
025500050511      *                 * ------------------ *
025600050511     c                   write     dettagli
025700050511      *                 * ------------------ *
025800050511      *
025900050511     c                   endsr
026000050511     ?****************************************************************************
026100050511      *?   Aggiorna    File di Work
026200050511     ?****************************************************************************
026300050511     c     Agg_rig_det   begsr
026400050511      *
026500050511     c                   move      'S'           almeno_uno
026600050511      *
026700050517     C     K_MED         setll     WFFMA00F
026800050517     C     K_MED         reade     WFFMA00F
026900050511      *
027000050517     c                   dow       not %Eof(WFFMA00F)
027100050511      *
027200050511     c                   eval      M_FMENUMAUT = TOT_NumAut
027300050511     c                   eval      M_FMEMEDPO  = TOT_MedGiorPO
027400050511      *
027500050511     c                   update    WFFME000
027600050511      *
027700050517     C     K_MED         reade     WFFMA00F
027800050511     c                   enddo
027900050510      *
028000050510     c                   endsr
028100050511     ?****************************************************************************
028200050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
028300050511     ?****************************************************************************
028400050511     c     PRT_NO_Dati   begsr
028500050511      *
028600050511     c                   if        *in66
028700050511      *                 * ------------------ *
028800050511     c                   write     testa
028900050511      *                 * ------------------ *
029000050511     c                   setoff                                       66
029100050511     c                   end
029200050511      *
029300050511      *                 * ------------------ *
029400050511     c                   write     nodati
029500050511      *                 * ------------------ *
029600050511      *
029700050511     c                   endsr
029800050511     ?****************************************************************************
029900050511      *?   INIZIALIZZAZIONE
030000050511     ?****************************************************************************
030100040820     C     *inzsr        BEGSR
030200040820      **
030300050511     C     K_MED         KLIST
030400050511     C                   Kfld                    TOT_Dist
030500050511     C                   Kfld                    TOT_Area
030600050511     C                   Kfld                    TOT_POper
030700050524      **
030800050524     C     K_aut         KLIST
030900050524     C                   Kfld                    APDTIP
031000050524     C                   Kfld                    APDPDR
031100050511      **
031200040820     C     *ENTRY        PLIST
031300040820     C                   PARM                    KPJBA
031400050510     c                   movel     kpjbu         DsPassaggio
031500050511      **
031600050511     C                   Z-ADD     1             CODUT
031700050511     C                   CALL      'X§PARUT'
031800050511     C                   PARM                    UT§DSE
031900050511     C                   MOVEL     RAGUT         RSUT             20
032000041103      *
032100040820      *  imposta UDATE di elaborazione
032200040820     C                   move      *date         G02DAT
032300040820     C                   MOVE      *ZEROS        G02INV
032400040820     C                   MOVE      *BLANKS       G02ERR
032500040820     C                   CALL      'XSRDA8'
032600040820     C                   PARM                    WLBDAT
032700040820     C                   Z-ADD     G02INV        data_oggi
032800050511     C                   move      G02dat        day
032900040820     C                   TIME                    W0140            14 0
033000050511     C                   MOVEl     W0140         ore
033100040823      **
033200050510     c                   move      PASdti        dadata            8 0
033300050510     c                   move      PASdtf        adata             8 0
033400040823      **
033500050511     c                   seton                                        66
033600040825     c                   clear                   prima_volta       1
033700040826     c                   clear                   almeno_uno
033800050511     C                   MOVEL     DSPGM         NOMPGM
033900050511      **
034000050511     C                   move      *zeros        G02DAT
034100050511     C                   MOVE      dadata        G02INV
034200050511     C                   MOVE      '3'           G02ERR
034300050511     C                   CALL      'XSRDA8'
034400050511     C                   PARM                    WLBDAT
034500050511     C                   Z-ADD     G02Dat        PDadata
034600050511      **
034700050511     C                   move      *zeros        G02DAT
034800050511     C                   MOVE      adata         G02INV
034900050511     C                   MOVE      '3'           G02ERR
035000050511     C                   CALL      'XSRDA8'
035100050511     C                   PARM                    WLBDAT
035200050511     C                   Z-ADD     G02dat        PAdata
035300040820     c*
035400050517     c                   Open      WFFMA00F
035500050510     c*
035600040820     C                   ENDSR
035700050511     ?****************************************************************************
