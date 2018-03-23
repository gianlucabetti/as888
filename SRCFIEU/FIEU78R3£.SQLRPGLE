000100161298      ***********************************************************************
000200080617      * il programma legge tutte le bolle di Reso DPD, cerca la bolla mamma
000300080617      * quindi agginuge sulla mamma gli importi della figlia (reso) e poi
000400080617      * cancella il record di reso.
000500080617      ***********************************************************************
000600981216     H DECEDIT('0,') DATEDIT(*DMY.)
000700161298      *---------------------------------------------------------------------*
000800080617     FFnLBL01L  iF   E           k DISK
000900080617     FTNECF00F  uF   E             DISK
001000080617     FTNECF02L  uF   E           k DISK    rename(TNECF000:TNECF2)
001100080617      *
001200080617     D KPJBA         E DS
001300030620      *
001400080617     D ecfAAS_SQ       S                   like(ecfAAS)
001500080617     D ecfLNP_SQ       S                   like(ecfLNP)
001600080617     D ecfNRS_SQ       S                   like(ecfNRS)
001700080617     D ecfNSP_SQ       S                   like(ecfNSP)
001800080617     D ecfMGS_SQ       S                   like(ecfMGS)
001900080617     D ecfRIF_SQ       S                   like(ecfRIF)
002000080617     D ecfDFP_SQ       S                   like(ecfDFP)
002100080617     D ecfGRP_SQ       S                   like(ecfGRP)
002200080617     D ecfIMP_SQ       S                   like(ecfIMP)
002300080617     D ecfIMR_SQ       S                   like(ecfIMR)
002400080617     D ecfIMC_SQ       S                   like(ecfIMC)
002500080617     D ecfRRN_SQ       S              9s 0
002600030620      *---------------------------------------------------------------------*
002700080617      *
002800080617      *---------------------------------------------------------------------*
002900030624     C     *entry        plist
003000030624     C                   parm                    kpjba
003100030624      *
003200080617     c     klbl1         klist
003300080617     c                   kfld                    ecfAas_sq
003400080617     c                   kfld                    ecfLnp_sq
003500080617     c                   kfld                    ecfNrs_sq
003600080617     c                   kfld                    ecfNsp_sq
003700040830      *
003800080617     c     klbl1_1       klist
003900080617     c                   kfld                    lblAAP
004000080617     c                   kfld                    lblLPP
004100080617     c                   kfld                    lblNRP
004200080617     c                   kfld                    lblNSP
004300080617      *
004400080617      * x aggancio Bolla Originale (mamma)
004500080617     c     kECF2         klist
004600080617     c                   kfld                    lblAAo
004700080617     c                   kfld                    lblLPo
004800080617     c                   kfld                    lblNRo
004900080617     c                   kfld                    lblNSo
005000080617     c                   kfld                    ecfGRP_SQ
005100080617     C/EXEC SQL
005200080617     C+ DECLARE A1 CURSOR FOR
005300080617     C+ SELECT ECFAAS, ECFLNP, ECFNRS, ECFNSP, ECFMGS, ECFRIF, ECFDFP,
005400080617     C+ ECFGRP, ECFIMP, ECFIMR, ECFIMC, rrn(tnecf00f)
005500080617     C+ FROM tnecf00f
005600080617     C+ WHERE ecfntw='DPD' and ecffbr='S' and ecfdtg=0
005700080617     C+ ORDER BY ecfaas, ecfmgs
005800080617     C/END-EXEC
005900080617      *
006000080617      * Esegue un'estrazione di tutte le bolle Rese
006100080617     C/EXEC SQL
006200080617     C+ OPEN A1
006300080617     C/END-EXEC
006400080617     C                   DOU       SqlCod <> 0
006500080617     C/EXEC SQL
006600080617     C+ FETCH NEXT FROM A1 INTO :ECFAAS_SQ, :ECFLNP_SQ, :ECFNRS_SQ,
006700080617     C+ :ECFNSP_SQ, :ECFMGS_SQ, :ECFRIF_SQ, :ECFDFP_SQ, :ECFGRP_SQ,
006800080617     C+ :ECFIMP_SQ, :ECFIMR_SQ, :ECFIMC_SQ, :ECFRRN_SQ
006900080617     C/END-EXEC
007000080617     C                   SELECT
007100080617     c* a fine file
007200080617     C                   WHEN      SqlCod = 100
007300080617     c                   leave
007400080617     c* se errori
007500080617     C                   WHEN      SqlCod < 0
007600080617     C                   seton                                        H1
007700080617     c                   goto      fine
007800080617     c*
007900080617     C                   OTHER
008000080617     c* DETTAGLIO
008100080617     c                   exsr      Exec_Dettaglio
008200080617     c*
008300080617     C                   ENDSL
008400080617
008500080617     C                   ENDDO
008600080617     C/EXEC SQL
008700080617     C+ CLOSE A1
008800080617     C/END-EXEC
008900080617     c     FINE          tag
009000080617     c                   seton                                        LR
009100030627      *----------------------------------------------------------------
009200030627      * imposta i campi per la scrittura
009300030627      *----------------------------------------------------------------
009400080617     c     Exec_DettaglioBegsr
009500040830      *
009600080617      *  Esegue la ricerca della bolla originale su cui aggiungere
009700080617      *   gli importi del reso.
009800080617     c                   exsr      Cerca_la_mamma
009900040902     **
010000080617      *  Se ha trovato la Bolla mamma a cui attribuire gli importi
010100080617      *  potrà in seguito cancellare il record di Reso Letto con SQL
010200080617     c                   if        Mamma_trovata = 'S'
010300080617     c                   clear                   ECF_mamma         1
010400080617     c     kECF2         chain     tnECF02L
010500080617     **
010600080617     c                   if        %Found(tnECF02L)
010700080617     c                   move      'S'           ECF_mamma
010800080617     ** aggiunge gli importi sulla bolla mamma
010900080617     c                   eval      ecfIMP= ecfIMP + ecfIMP_SQ
011000080617     c                   eval      ecfIMR= ecfIMR + ecfIMR_SQ
011100080617     c                   eval      ecfIMC= ecfIMC + ecfIMC_SQ
011200080617     c                   if        ecfDfp = 0
011300080617     c                   eval      ecfDfp = ecfDfp_sq
011400080619     c                   if        ecfDfp > 0
011500080619     c                   move(p)   '1'           ecfNfp
011600080619     c                   endIf
011700080617     c                   end
011800080617     c                   update    tnecf2
011900080617      *
012000080617     c                   End
012100080617      *
012200080617      * Quindi con il Nr.relativo di record va a cancellare
012300080617      *  se è stato fatto l'aggiornamento sommando gli importi oppure
012400080617      *  se il record è fra quelli da NON considerare.
012500080617     c                   if        ECF_mamma = 'S' or
012600080617     c                             ecfGRP_SQ = 'NO '
012700080617      *
012800080617     c     ecfRRN_SQ     chain     tnECF00F
012900080617     c                   if        %Found(tnECF00f)
013000080617     c                   delete    tnecf000
013100080617     c                   End
013200080617      *
013300080617     c                   End
013400080617     **
013500080617     c                   End
013600030627      *
013700030627     c                   Endsr
013800030627      *----------------------------------------------------------------
013900080617      *  x le bolle di reso risale la catena fino alla bolla Originale
014000080617      *----------------------------------------------------------------
014100080617     c     Cerca_la_MammaBegsr
014200080617      *
014300080617     c                   clear                   mamma_trovata     1
014400080617      * Con la chiave bolla di TITAS
014500080617      *  aggancia la figlia per cercare la madre.
014600080617     c     klbl1         chain     fnLBL01l
014700080617     c                   If        %Found(fnLBL01l)
014800080617      *
014900080617      *  se la bolla originale equivale alla madre
015000080617     c                   if        lblAAo = lblAAp and
015100080617     c                             lblLPo = lblLPp and
015200080617     c                             lblNRo = lblNRp and
015300080617     c                             lblNSo = lblNSp
015400080617      *  esce dalla routine
015500080617     c                   move      'S'           mamma_trovata
015600080617     c                   LEAVESR
015700080617     c                   end
015800080617      *
015900080617      * altrimenti prosegue alla ricerca della bolla mamma che sia l'originale
016000080617      *
016100080617     c     cerca_ancora  tag
016200080617      *
016300080617     c     klbl1_1       chain     fnLBL01l
016400080617     c                   If        %Found(fnLBL01l)
016500080617      *
016600080617      *  se la bolla originale equivale alla madre
016700080617     c                   if        lblAAo = lblAAp and
016800080617     c                             lblLPo = lblLPp and
016900080617     c                             lblNRo = lblNRp and
017000080617     c                             lblNSo = lblNSp
017100080617      *  esce dalla routine
017200080617     c                   move      'S'           mamma_trovata
017300080617     c                   LEAVESR
017400080617     c                   elSe
017500080617     c                   goTO      cerca_ancora
017600080617     c                   end
017700080617      *
017800080617     c                   elSe
017900080617     c                   endIf
018000080617      *
018100080617     c                   endIf
018200080617      *
018300080617     c                   Endsr
018400080617      *----------------------------------------------------------------
018500050907     c     *inzsr        Begsr
018600050907      *
018700050907      *
018800050907     c                   Endsr
018900050907      *----------------------------------------------------------------
