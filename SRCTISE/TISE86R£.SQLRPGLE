000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300050517     fWFFMA00F  uf a E           K DISK    UsrOPN
000400050511     f                                     prefix(M_)
000500050524     ffiapd01l  if   E           K DISK
000600051111     ffiftt02l  if   E           K DISK
000700050829-->  fTISE85p   o    E             PRINTER OFLIND(*in66)
000800050510      **-----------------------------------------------------------------***
000900040820     d KPJBA         E DS
001000050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001100110511     D CNCR42        E DS
001200050905     D dapdflr       E DS
001300040820     D*
001400050829-->  D DsPassaggio   e DS                  EXTNAME(TISE85ds)
001500050510     D fiftt00f      e DS
001600050510     D azorg00f      e DS
001700040820     D*
001800040820     D WLBDAT          DS
001900040820     D  G02DAT                 1      8  0
002000040820     D  G02INV                 9     16  0
002100040820     D  G02ERR                17     17
002200040820     D  G02TGI                18     22  0
002300040820      *
002400040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002500040826     D  almeno_uno     s              1
002600040820      *
002700050510     D  FLD_Distr      s                   like(orgfl3)
002800050510     D  FLD_Area       s                   like(orgcar)
002900050510     D  FLD_Fgs        s                   like(fttfgs)
003000050510     D  FLD_Padr       s                   like(fttpdr)
003100050905     D  FLD_Spediz     s              7  0
003200051111     D  FLD_Ritiri     s              7  0
003300051111     D  FLD_Consegne   s              7  0
003400050510     D  FLD_TotVal     s             11  3
003500050510     D  FLD_GioLav     s              5  0
003600050510     D  FLD_MedPadr    s             11  3
003700050510      *
003800050511     D  TOT_Dist       s                   like(M_FmeDist)
003900050511     D  TOT_Area       s                   like(M_FmeArea)
004000050511     D  TOT_POper      s                   like(M_FmePOper)
004100050510     D  TOT_SumMed     s             11  3
004200050510     D  TOT_NumAut     s              5  0
004300050510     D  TOT_MedGiorPO  s             11  3
004400050510      *
004500050511     D*-------------
004600050511     D* Reperimento nome PGM
004700050511     D STATUS         SDS           333
004800050511     D  DSPGM            *PROC
004900050511     D*-------------
005000050511      *
005100040820     D digits          C                   CONST('0123456789')
005200050511      * ?------------------------------------------------------------------ */
005300050511      * ? In questa Fase si rilevano i dati per singolo Autista
005400050511      * ? dai Conteggi Padroncini prendendo i costi x giornata.
005500050511      * ? Poi si possono individuare, contandole, le giornate che
005600050511      * ? l'Autista ha lavorato.
005700050511      * ? Con il Totale x Autista diviso le sue giornate lavorate
005800050511      * ? si ha il costo medio di ogni singolo Autista
005900050511      * ? e scrivo un record sul File Statistico.
006000050510      **
006100050510      **  Il File è organizzato x Distretto/Area/P.O./Autista
006200050510      **   i cui dati sono:
006300050510      **     TOTALE VALORE x AUTISTA (di tutti i Giorni Lavorati)
006400050510      **     TOTALE Giorni LAVORATI
006500050510      **     MEDIA Giornaliera x Autista
006600050510      **           ottenuta come TOT_VALORE diviso TOT_Giorni_LAVORATI
006700050510      **  Con questa base si ottengono i valori x Singola Giornata Autista.
006800050511      * ?------------------------------------------------------------------ */
006900050510     C/EXEC SQL
007000050510     C+ Declare A1 Cursor for
007100050510     C+ SELECT orgfl3, orgcar, fttfgs, fttpdr,
007200050905     C+   decimal(sum(Sped_gior),  7, 0) as Tot_Sped,
007300050905     C+   decimal(sum(Tot_Costo), 11, 3) as Tot_Valore,
007400050510     C+   decimal(count(*), 5, 0) as Num_Giorni,
007500050510     C+   decimal(sum(TOT_Costo)/count(*), 11, 3) as Media_Giornata
007600050510     C+ FROM (
007700050510     C+ SELECT orgfl3, orgcar, fttfgs, FTTPDR, fttddc,
007800050905     C+ decimal(sum(fttset), 7, 0) as Sped_gior,
007900081007     C+ decimal(sum(FTTITT + FTTITA + FTTTPE + FTTTBN + FTTTIM + FTTMNT
008000081007     C+       + FTTPCAR), 11, 3) as Tot_Costo
008100081007     C+  FROM FIFTT00F join AZORG01L on FTTFGS=ORGFIL
008200050517     C+ WHERE ftttsr =' ' and fttfvl = 'C' and fttNFF <> 999999999
008300050510     C+   AND fttddc between :DADATA  and :ADATA
008400050510     C+ GROUP BY  orgfl3, orgcar, fttpdr, fttddc, fttfgs
008500050510     C+ ) as A
008600050510     C+ GROUP BY  orgfl3, orgcar, fttfgs, fttpdr
008700050510     C+ ORDER BY  orgfl3, orgcar, fttfgs, fttpdr
008800050510     C/END-EXEC
008900050510      *          apertura cursore
009000050510     C/EXEC SQL
009100050510     C+ OPEN A1
009200050510     C/END-EXEC
009300050510     c                   clear                   almeno_uno
009400050510      **
009500050510     C                   DOU       SqlCod <> 0
009600050510      **         lettura cursore
009700050510     C/EXEC SQL
009800050905     C+ FETCH NEXT FROM A1 INTO :FLD_Distr, :FLD_Area, :FLD_Fgs,
009900050905     C+ :FLD_Padr, :FLD_Spediz, :FLD_TotVal, :FLD_GioLav, :FLD_MedPadr
010000050510     C/END-EXEC
010100050510     C                   SELECT
010200050510     **
010300050510     C                   WHEN      SqlCod = 100
010400050510     c* a fine file
010500050510     c                   if        almeno_uno = 'S'
010600050510     c                   else
010700050510     c                   end
010800050510     c                   leave
010900050510     c*
011000050510     C                   WHEN      SqlCod < 0
011100050510     C                   seton                                        H1
011200050510     c                   goto      fine
011300050510     c*
011400050510     C                   OTHER
011500050510     **
011600050510     c* DETTAGLIO
011700050510     c* riga   dettaglio
011800050510     c                   exsr      Wri_rig_det
011900050510     c*
012000050510     C                   ENDSL
012100050510
012200050510     C                   ENDDO
012300050510     C/EXEC SQL
012400050510     C+ Close A1
012500050510     C/END-EXEC
012600050510     c*
012700050510     c*  Chiude il File per eseguire SQL finale di totalizzazione
012800050517     c                   Close     WFFMA00F
012900050510     c* Totali
013000050510     c                   exsr      totalizza
013100050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
013200040823      *
013300041007     c     fine          tag
013400020326     C                   SETON                                        LR
013500050511     ?****************************************************************************
013600050511      *?    Scrive Riga File di Work
013700050511     ?****************************************************************************
013800050510     c     Wri_rig_det   begsr
013900050510      *
014000050511     c                   move      'S'           almeno_uno
014100050524      *
014200050510     c                   clear                   WFFME000
014300050510      *
014400050524     c                   eval      APDTIP = 'A'
014500050524     c                   eval      APDPDR =  FLD_Padr
014600050524     c     K_aut         chain     fiapd01l
014700050524     c                   if        %Found(fiapd01l)
014800050905     c                   movel     apdflr        dapdflr
014900050905      * è alfanumerico --> campo filler
015000050905     c                   if        §apdKM <> *blank
015100050905     c                   move      §apdKM        M_FMKMETRI
015200050905     c                   end
015300050905      *
015400050524     c                   eval      M_FMEPESO   = apdPKL
015500050524     c                   eval      M_FMEVOLUME = apdVLU
015600050524     c                   end
015700050511     c                   eval      M_FMEDIST   = FLD_Distr
015800050511     c                   eval      M_FMEAREA   = FLD_Area
015900050511     c                   eval      M_FMEPOPER  = FLD_Fgs
016000050511     c                   eval      M_FMEAUTIST = FLD_Padr
016100050905     c                   eval      M_FMSPEDIZ  = FLD_Spediz
016200051111      **
016300051111     c                   clear                   FLD_Ritiri
016400051111     c                   clear                   FLD_Consegne
016500051111      **
016600051111     c                   eval      fttfvl = 'C'
016700051111     c                   eval      fttpdr = APDPDR
016800051111     c                   eval      fttddc = dadata
016900051111      **
017000051111     C     K_FTT         setll     fiftt02l
017100051111     C     K_FTT_Pdr     reade     fiftt02l
017200051111      **
017300051111     c                   dow       not %EoF(fiftt02l)
017400051111      **  se supera la data limite del periodo selezionato
017500051111     c                   if        fttddc > adata
017600051111     c                   leave
017700051111     c                   end
017800051111      **
017900051111     c                   select
018000051111     c                   when      ftttsr = 'C'
018100051111     c                   add       fttSET        FLD_Consegne
018200051111     c                   when      ftttsr = 'R'
018300051111     c                   add       fttSET        FLD_Ritiri
018400051111     c                   endSL
018500051111      **
018600051111     C     K_FTT_Pdr     reade     fiftt02l
018700051111     c                   endDO
018800051111      **
018900051111     c                   eval      M_FMSPERIT  = FLD_Ritiri
019000051111     c                   eval      M_FMSPECON  = FLD_Consegne
019100051111      **
019200050511     c                   eval      M_FMETOTVAL = FLD_TotVal
019300050511     c                   eval      M_FMENGILAV = FLD_GioLav
019400050511     c                   eval      M_FMEMEDGIO = FLD_MedPadr
019500050511     c                   eval      M_FMEDADATA = pasdti
019600050511     c                   eval      M_FMEADATA  = pasdtf
019700050510      *
019800050510     c                   write     WFFME000
019900050510      *
020000050510     c                   endsr
020100050511     ?****************************************************************************
020200050511      *?   Totali da  File di Work
020300050511     ?****************************************************************************
020400050510     c     Totalizza     begsr
020500050510      *
020600050517     c                   Open      WFFMA00F
020700050511      * ?------------------------------------------------------------------ */
020800050511      **  Con il file Ricavato Precedentemente x Autista è possibile
020900050511      **   totalizzare e ricavare le medie x P.O..
021000050511      **   Con il totale di tutti gli Autisti del P.O. diviso il numero
021100050510      **   stesso degli Autisti del P.O. si ottiene il valore medio
021200050510      **   a giornata del P.O.
021300050511      * ?------------------------------------------------------------------ */
021400050510     C/EXEC SQL
021500050510     C+ Declare B2 Cursor for
021600050510     C+  SELECT FMEDIST, FMEAREA, FMEPOPER,
021700050510     C+  decimal(sum(FMEMEDGIO), 11, 3) as TOT_PO_MED,
021800050510     C+  decimal( COUNT(*), 5, 0) as NUM_AUT_PO,
021900050510     C+  decimal(sum(FMEMEDGIO)/COUNT(*), 11, 3) as MEDIA_PO
022000050517     C+  FROM WFFMA00F
022100050510     C+  GROUP BY FMEDIST, FMEAREA, FMEPOPER
022200050510     C+  ORDER BY FMEDIST, FMEAREA, FMEPOPER
022300050510     C/END-EXEC
022400050510      *          apertura cursore
022500050510     C/EXEC SQL
022600050510     C+ OPEN B2
022700050510     C/END-EXEC
022800050510     c                   clear                   almeno_uno
022900050510      **
023000050510     C                   DOU       SqlCod <> 0
023100050510      **         lettura cursore
023200050510     C/EXEC SQL
023300050510     C+ Fetch Next From B2 Into :TOT_Dist, :TOT_Area, :TOT_POper,
023400050510     C+       :TOT_SumMed, :TOT_NumAut, :TOT_MedGiorPO
023500050510     C/END-EXEC
023600050510     C                   SELECT
023700050510     **
023800050510     C                   WHEN      SqlCod = 100
023900050510     c* a fine file
024000050510     c                   if        almeno_uno = 'S'
024100050510     c                   else
024200050511     c                   if        pasLST = 'S'
024300050511     c                   exsr      PRT_NO_Dati
024400050511     c                   endIF
024500050510     c                   end
024600050510     c                   leave
024700050510     c*
024800050510     C                   WHEN      SqlCod < 0
024900050510     C                   seton                                        H2
025000050510     c                   goto      fine_SR
025100050510     c*
025200050510     C                   OTHER
025300050510     **
025400050510     c* DETTAGLIO
025500050510     c* riga   dettaglio
025600050510     c                   exsr      Agg_rig_det
025700050511     c                   if        pasLST = 'S'
025800050511     c                   exsr      PRT_rig_det
025900050511     c                   endIF
026000050510     c*
026100050510     C                   ENDSL
026200050510
026300050510     C                   ENDDO
026400050510     C/EXEC SQL
026500050510     C+ Close B2
026600050510     C/END-EXEC
026700050510     c
026800050510     c     fine_SR       tag
026900050517     c                   Close     WFFMA00F
027000050510      *
027100050510     c                   endsr
027200050511     ?****************************************************************************
027300050511      *?   Stampa      File di Work Una riga x P.O.
027400050511     ?****************************************************************************
027500050511     c     PRT_rig_det   begsr
027600050511      *
027700050511     c                   if        *in66
027800050511      *                 * ------------------ *
027900050511     c                   write     testa
028000050511      *                 * ------------------ *
028100050511     c                   setoff                                       66
028200050511     c                   end
028300050511      *
028400050511     c                   eval      PDist   = TOT_Dist
028500050511     C                   eval      PArea   = TOT_Area
028600050511     C                   eval      PPOper  = TOT_POper
028700050511     c                   eval      PNumAut = TOT_NumAut
028800050511     c                   eval      PMedia  = TOT_MedGiorPO
028900050511      *
029000050511      *                 * ------------------ *
029100050511     c                   write     dettagli
029200050511      *                 * ------------------ *
029300050511      *
029400050511     c                   endsr
029500050511     ?****************************************************************************
029600050511      *?   Aggiorna    File di Work
029700050511     ?****************************************************************************
029800050511     c     Agg_rig_det   begsr
029900050511      *
030000050511     c                   move      'S'           almeno_uno
030100050511      *
030200050517     C     K_MED         setll     WFFMA00F
030300050517     C     K_MED         reade     WFFMA00F
030400050511      *
030500050517     c                   dow       not %Eof(WFFMA00F)
030600050511      *
030700050511     c                   eval      M_FMENUMAUT = TOT_NumAut
030800050511     c                   eval      M_FMEMEDPO  = TOT_MedGiorPO
030900050511      *
031000050511     c                   update    WFFME000
031100050511      *
031200050517     C     K_MED         reade     WFFMA00F
031300050511     c                   enddo
031400050510      *
031500050510     c                   endsr
031600050511     ?****************************************************************************
031700050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
031800050511     ?****************************************************************************
031900050511     c     PRT_NO_Dati   begsr
032000050511      *
032100050511     c                   if        *in66
032200050511      *                 * ------------------ *
032300050511     c                   write     testa
032400050511      *                 * ------------------ *
032500050511     c                   setoff                                       66
032600050511     c                   end
032700050511      *
032800050511      *                 * ------------------ *
032900050511     c                   write     nodati
033000050511      *                 * ------------------ *
033100050511      *
033200050511     c                   endsr
033300050511     ?****************************************************************************
033400050511      *?   INIZIALIZZAZIONE
033500050511     ?****************************************************************************
033600040820     C     *inzsr        BEGSR
033700040820      **
033800050511     C     K_MED         KLIST
033900050511     C                   Kfld                    TOT_Dist
034000050511     C                   Kfld                    TOT_Area
034100050511     C                   Kfld                    TOT_POper
034200050524      **
034300050524     C     K_aut         KLIST
034400050524     C                   Kfld                    APDTIP
034500050524     C                   Kfld                    APDPDR
034600051111      **
034700051111     C     K_FTT         KLIST
034800051111     C                   Kfld                    fttPDR
034900051111     C                   Kfld                    fttFVL
035000051111     C                   Kfld                    fttDDC
035100051111      **
035200051111     C     K_FTT_Pdr     KLIST
035300051111     C                   Kfld                    fttPDR
035400051111     C                   Kfld                    fttFVL
035500050511      **
035600040820     C     *ENTRY        PLIST
035700040820     C                   PARM                    KPJBA
035800050510     c                   movel     kpjbu         DsPassaggio
035900050511      **
036000050511     C                   Z-ADD     1             CODUT
036100050511     C                   CALL      'X§PARUT'
036200050511     C                   PARM                    UT§DSE
036300110511     C*******            MOVEL     RAGUT         RSUT             20
036400110511     C                   MOVEL     REC42         CNCR42
036500110511     C                   MOVEL     simRGS        RSUT             20
036600041103      *
036700040820      *  imposta UDATE di elaborazione
036800040820     C                   move      *date         G02DAT
036900040820     C                   MOVE      *ZEROS        G02INV
037000040820     C                   MOVE      *BLANKS       G02ERR
037100040820     C                   CALL      'XSRDA8'
037200040820     C                   PARM                    WLBDAT
037300040820     C                   Z-ADD     G02INV        data_oggi
037400050511     C                   move      G02dat        day
037500040820     C                   TIME                    W0140            14 0
037600050511     C                   MOVEl     W0140         ore
037700040823      **
037800050510     c                   move      PASdti        dadata            8 0
037900050510     c                   move      PASdtf        adata             8 0
038000040823      **
038100050511     c                   seton                                        66
038200040825     c                   clear                   prima_volta       1
038300040826     c                   clear                   almeno_uno
038400050511     C                   MOVEL     DSPGM         NOMPGM
038500050511      **
038600050511     C                   move      *zeros        G02DAT
038700050511     C                   MOVE      dadata        G02INV
038800050511     C                   MOVE      '3'           G02ERR
038900050511     C                   CALL      'XSRDA8'
039000050511     C                   PARM                    WLBDAT
039100050511     C                   Z-ADD     G02Dat        PDadata
039200050511      **
039300050511     C                   move      *zeros        G02DAT
039400050511     C                   MOVE      adata         G02INV
039500050511     C                   MOVE      '3'           G02ERR
039600050511     C                   CALL      'XSRDA8'
039700050511     C                   PARM                    WLBDAT
039800050511     C                   Z-ADD     G02dat        PAdata
039900040820     c*
040000050517     c                   Open      WFFMA00F
040100050510     c*
040200040820     C                   ENDSR
040300050511     ?****************************************************************************
