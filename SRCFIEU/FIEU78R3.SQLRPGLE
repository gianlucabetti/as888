000100161298      ***********************************************************************
000200080617      * il programma legge tutte le bolle di Reso DPD, cerca la bolla mamma
000300160721      * quindi aggiunge sulla FIGLIA gli importi della mamma per i RESI
000400080617      ***********************************************************************
000500981216     H DECEDIT('0,') DATEDIT(*DMY.)
000600161298      *---------------------------------------------------------------------*
000700080617     FFnLBL01L  iF   E           k DISK
000800080617      *
000900080617     D KPJBA         E DS
001000160721     D tnecf00f      E DS
001100160721      *
001200160721     D parDate         DS
001300160721     D   DaData                1      8  0
001400160721     D    aData                9     16  0
001500160721     D    aAnno                9     12  0
001600160721     D    aMesG               13     16  0
001700160721     D   TpNtw                17     19
001800160721     D   TpData               20     20
001900160721      *
002000160721     D savAAo          S                   like(lblAAo)
002100160721     D savLPo          S                   like(lblLPo)
002200160721     D savNRo          S                   like(lblNRo)
002300160721     D savNSo          S                   like(lblNSo)
002400160721     D savRIF          S                   like(ecfRIF)
002500160721     D savGRP          S                   like(ecfGRP)
002600160721     D savNRREC        S              9s 0
002700030620      *
002800080617     D ecfAAS_SQ       S                   like(ecfAAS)
002900080617     D ecfLNP_SQ       S                   like(ecfLNP)
003000080617     D ecfNRS_SQ       S                   like(ecfNRS)
003100080617     D ecfNSP_SQ       S                   like(ecfNSP)
003200080617     D ecfMGS_SQ       S                   like(ecfMGS)
003300080617     D ecfRIF_SQ       S                   like(ecfRIF)
003400080617     D ecfDFP_SQ       S                   like(ecfDFP)
003500080617     D ecfGRP_SQ       S                   like(ecfGRP)
003600080617     D ecfIMP_SQ       S                   like(ecfIMP)
003700080617     D ecfIMR_SQ       S                   like(ecfIMR)
003800080617     D ecfIMC_SQ       S                   like(ecfIMC)
003900080617     D ecfRRN_SQ       S              9s 0
004000030620      *---------------------------------------------------------------------*
004100080617      *
004200080617      *---------------------------------------------------------------------*
004300030624     C     *entry        plist
004400030624     C                   parm                    kpjba
004500160721     c                   movel     kpjbu         pardate
004600030624      *
004700080617     C/EXEC SQL
004800080617     C+ DECLARE A1 CURSOR FOR
004900080617     C+ SELECT ECFAAS, ECFLNP, ECFNRS, ECFNSP, ECFMGS, ECFRIF, ECFDFP,
005000080617     C+ ECFGRP, ECFIMP, ECFIMR, ECFIMC, rrn(tnecf00f)
005100080617     C+ FROM tnecf00f
005200160721     C+ WHERE ecfntw='DPD' and ecffbr='S' and ecfdtg=0  and
005300160721     C+       ecfdcm between :dadata and :adata
005400080617     C+ ORDER BY ecfaas, ecfmgs
005500080617     C/END-EXEC
005600080617      *
005700080617      * Esegue un'estrazione di tutte le bolle Rese
005800080617     C/EXEC SQL
005900080617     C+ OPEN A1
006000080617     C/END-EXEC
006100080617     C                   DOU       SqlCod <> 0
006200080617     C/EXEC SQL
006300080617     C+ FETCH NEXT FROM A1 INTO :ECFAAS_SQ, :ECFLNP_SQ, :ECFNRS_SQ,
006400080617     C+ :ECFNSP_SQ, :ECFMGS_SQ, :ECFRIF_SQ, :ECFDFP_SQ, :ECFGRP_SQ,
006500080617     C+ :ECFIMP_SQ, :ECFIMR_SQ, :ECFIMC_SQ, :ECFRRN_SQ
006600080617     C/END-EXEC
006700080617     C                   SELECT
006800080617     c* a fine file
006900080617     C                   WHEN      SqlCod = 100
007000080617     c                   leave
007100080617     c* se errori
007200080617     C                   WHEN      SqlCod < 0
007300080617     C                   seton                                        H1
007400080617     c                   goto      fine
007500080617     c*
007600080617     C                   OTHER
007700080617     c* DETTAGLIO
007800080617     c                   exsr      Exec_Dettaglio
007900080617     c*
008000080617     C                   ENDSL
008100080617
008200080617     C                   ENDDO
008300080617     C/EXEC SQL
008400080617     C+ CLOSE A1
008500080617     C/END-EXEC
008600080617     c     FINE          tag
008700080617     c                   seton                                        LR
008800030627      *----------------------------------------------------------------
008900030627      * imposta i campi per la scrittura
009000030627      *----------------------------------------------------------------
009100080617     c     Exec_DettaglioBegsr
009200040830      *
009300160721      *  Esegue la ricerca della bolla mamma per gli importi da aggiungere
009400160721      *   sulla figlia.
009500080617     c                   exsr      Cerca_la_mamma
009600040902     **
009700160721      *  Se ha trovato la Bolla mamma da cui prendere gli importi
009800080617     c                   if        Mamma_trovata = 'S'
009900160721     c                   z-add     ecfRRN_SQ     savNRREC
010000160721     c                   movel     ecfRIF_SQ     savRIF
010100160721     c                   movel     ecfGRP_SQ     savGRP
010200080617     **
010300160721     **   Aggancia la Mamma e aggiorna la FIGLIA
010400160721     c                   call      'FIEU78R4'
010500160721     c                   parm                    savAAo
010600160721     c                   parm                    savLPo
010700160721     c                   parm                    savNRo
010800160721     c                   parm                    savNSo
010900160721     c                   parm                    savRIF
011000160721     c                   parm                    savGRP
011100160721     c                   parm                    savNRREC
011200160721     **
011300080617     c                   End
011400030627      *
011500030627     c                   Endsr
011600030627      *----------------------------------------------------------------
011700080617      *  x le bolle di reso risale la catena fino alla bolla Originale
011800080617      *----------------------------------------------------------------
011900080617     c     Cerca_la_MammaBegsr
012000080617      *
012100080617     c                   clear                   mamma_trovata     1
012200080617      * Con la chiave bolla di TITAS
012300160721      *  ag§ancia la figlia per cercare la madre.
012400080617     c     klbl1         chain     fnLBL01l
012500080617     c                   If        %Found(fnLBL01l)
012600160721     c                   move      'S'           mamma_trovata
012700080617      *
012800160721      *   bolla originale equivale alla madre
012900160721     c                   eval      savAAo = lblAAo
013000160721     c                   eval      savLPo = lblLPo
013100160721     c                   eval      savNRo = lblNRo
013200160721     c                   eval      savNSo = lblNSo
013300080617     c                   end
013400080617      *
013500080617     c                   Endsr
013600080617      *----------------------------------------------------------------
013700050907     c     *inzsr        Begsr
013800050907      *
013900160721     c     klbl1         klist
014000160721     c                   kfld                    ecfAas_sq
014100160721     c                   kfld                    ecfLnp_sq
014200160721     c                   kfld                    ecfNrs_sq
014300160721     c                   kfld                    ecfNsp_sq
014400160721      *
014500050907     c                   Endsr
014600050907      *----------------------------------------------------------------
