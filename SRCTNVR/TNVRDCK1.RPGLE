000100040625     H DECEDIT('0,') DATEDIT(*DMY.)
000200040625      * ---------------------------------------------------------
000300040709     fWRKDCK0F  up   E             DISK
000400040709     fFNDCT01l  uf   E           K DISK
000500040709     fFNDCK01L  UF A E           K DISK
000600040712     fFIRIG00F  o  A E             DISK
000700040625     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000800040625      * ---------------------------------------------------------
000900040712
001000040625      * ---------------------------------------------------------
001100040625
001200040709      * verifico il flag di errore nel file  WRKFLG
001300040709      * "0" bolla figlia nel file fndct di filiale aggiorno FNDCK con figlia e FNDCT con mamma e
001400040709      *     richiesta di generazione per la sola mamma
001500040709      * "2" unica bolla figlia aggiorno FNDCK
001600040709      * "3" e wrkaas > 0 aggiorno FNDCK
001700040709
001800040709     c                   if        wrkflg = '0' or
001900040709     c                             wrkflg = '2' or
002000040712     c                             (wrkflg = '3' and wrkaas > 0)
002100040709      * verifico se esiste già FNDCK
002200040709      *
002300040709     c     Kca           chain     fndck01l
002400040709    2c                   if        not %found(fndck01l)
002500040709     c                   clear                   fndck000
002600040709     c                   eval      dckaac = wrkaac
002700040709     c                   eval      dckfil = wrkfil
002800040709     c                   eval      dcknca = wrknca
002900040709     c                   eval      dckaas = wrkaas
003000040709     c                   eval      dcklnp = wrklnp
003100040709     c                   eval      dcknrs = wrknrs
003200040709     c                   eval      dcknsp = wrknsp
003300040709     c                   write     fndck000
003400040709     c                   endif
003500040709
003600040709      * aggiorno il flag di wrkdck con "9" di elaborato
003700040709     c                   eval      wrkflg = '9'
003800040709     c                   update    wrkdck00
003900040709     c                   endif
004000040709      * Spedizione mamma valorizzata aggancio fndct ed aggiorno
004100040709      * scrivo firig00f per la richiesta di rigenerazione
004200040709      *
004300040709     c                   If        wrkaam > 0
004400040709      * aggancio fndct01
004500040709     c     Kca           chain     fndct01l
004600040712    2c                   if        %found(fndct01l)
004700040709     c                   eval      dctaas = wrkaam
004800040709     c                   eval      dctlnp = wrklnm
004900040709     c                   eval      dctnrs = wrknrm
005000040709     c                   eval      dctnsp = wrknsm
005100040709     c                   update    fndct000
005200040709     c                   endif
005300040709      * scrittura del firig00f
005400040712     c                   clear                   firig000
005500040712     C                   Z-ADD     wrkaam        rigAAS
005600040712     C                   Z-ADD     wrklnm        rigLNP
005700040712     C                   Z-ADD     wrknrm        rigNRS
005800040712     C                   Z-ADD     wrknsm        rigNSP
005900040712     C                   Z-ADD     wrklam        rigLNA
006000040712     C                   MOVEL     'D'           rigCAR
006100040709      * trasmetto solo in partenza
006200040712     C                   MOVEL     'P'           rigTRA
006300040712     C                   Z-ADD     46            rigPOR
006400040712     C                   MOVEL     'EDPGA'       rigPRU
006500040709      * metto queste date fasulle
006600040712     C                   Z-ADD     20011111      rigDIM
006700040712     C                   Z-ADD     1111          rigHIM
006800040709      *
006900040712     C                   write     firig000
007000040709      *
007100040709     c                   endif
007200040712      *
007300040709      *
007400980722      *****************************************************************
007500980623      *  ROUTINE INIZIALE
007600980722      *****************************************************************
007700951009     C     *INZSR        BEGSR
007800990412
007900980625      *
008000980721      *
008100040630     c     kca           klist
008200040709     c                   kfld                    wrkaac
008300040709     c                   kfld                    wrkfil
008400040709     c                   kfld                    wrknca
008500040630      *
008600981224      *
008700951009     C                   ENDSR
008800980611      *------------------------------------------------------------
