000100121004      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200121004     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300121004
000400121004      *---------------------------------------------------------------*
000500121004
000600121004     FWFSER00F  IF   E             DISK
000700121004     fTitas40c  if   e           k disk
000800121004     fNdreg04l  if   e           k disk
000900121004     fNdiva01l  if   e           k disk
001000121004     FYNNCT00F  O    E             DISK
001100121004     FYNNCR00F  O    E             DISK
001200121004      *
001300121004      // - Flags booleani
001400121004     d $End            s               n   inz(*off)
001500121004      // - campi di comodo
001600121004     d Societa         s              3    inz('201')
001700121004     d Contabilita     s              2    inz('CG')
001800121004     d serie           s              4
001900121004     d nreg            s              9  0
002000121004
002100121004      *---------------------------------------------------------------*
002200121004     d  XNMDAT         s               d   datfmt(*iso)
002300121004      **********************************************************************************************
002400121004      *
002500121004     c                   exsr      repnum
002600121004      *
002700121004Do  1c                   Do        *hival
002800121004      *
002900121004     c                   eval      $end = *off
003000121004      *
003100121004     c                   read      wfser00f
003200121004      *
003300121004     c                   if        %eof(wfser00f)
003400121004     c                   leave
003500121004     c                   endif
003600121004      * per ogni fattura scrivo una testata
003700121004     c                   exsr      SR_YNNCT
003800121004      * se mi ritorna $end acceso c'è stato un errore Leggola fattura usccessiva
003900121004     c                   If        $end = *on
004000121004     c                   iter
004100121004     c                   Endif
004200121004      * per ogni fattura elaboro il dettaglio
004300121004     c                   exsr      SR_YNMCR
004400121004      *
004500121004     c                   Enddo
004600121004
004700121004     c                   seton                                        lr
004800121004      *----------------------------------------------------*
004900121004      * Scrittura testata nota di accredito
005000121004      *----------------------------------------------------*
005100121004     c     Sr_Ynnct      begsr
005200121004      *
005300121004      * per ogni fattura vado a reperire i movimenti contabili relativi
005400121004      * agganciando il file NDREG04L con numero fattura
005500121004     c     K_ndreg       klist
005600121004     c                   kfld                    societa
005700121004     c                   kfld                    contabilita
005800121004     c                   kfld                    xnmdat
005900121004     c                   kfld                    nreg
006000121004     c                   kfld                    serie
006100121004
006200121004     c                   move      serdft        xnmdat
006300121004     c                   z-add     sernft        nreg
006400121004     c                   if        serfiv = '996'
006500121004     c                   eval      serie = '2996'
006600121004     c                   endif
006700121004     c                   if        serfiv = '998'
006800121004     c                   eval      serie = '2998'
006900121004     c                   endif
007000121004
007100121004     c     K_ndreg       chain     ndreg04l
007200121004      * se non trovata la registrazione vado alla fattura successiva
007300121004     c                   if        not %found(ndreg04l)
007400121004     c                   eval      $end  = *on
007500121004     c                   leavesr
007600121004     c                   Endif
007700121004      *
007800121004      * per ogni fattura vado a reperire i movimenti iva relativi
007900121004      * agganciando il file NDIVA01L con numero assoluto di registrazione
008000121004     c     K_ndiva       klist
008100121004     c                   kfld                    regsys
008200121004     c                   kfld                    regnrasreg
008300121004
008400121004     c     K_ndiva       setll     ndiva01l
008500121004      * se non trovata la registrazione IVA vado alla fattura successiva
008600121004     c                   if        not %equal
008700121004     c                   eval      $end  = *on
008800121004     c                   leavesr
008900121004     c                   Endif
009000121004     c                   clear                   ynnct000
009100121004      * numero assoluto interno numeratore YNA
009200121004     c                   eval      num_int = num_int + 1
009300121004     c                   eval      nctnrasreg = num_int
009400121004     c                   eval      nctsocieta = societa
009500121004     c                   move      20121007      nctdin
009600121004     c                   move      '2997'        nctserreg
009700121004     c                   move      '997'         nctlbi
009800121004     c                   movel     sercfa        fil               3 0          po richiedente
009900121004     c                   eval      nctfil = fil
010000121004     c**!!**             eval      nctfil = 046                                 po richiedente 046
010100121004     c                   eval      nctddc = ntcdim
010200121004     c                   eval      nctndc = nctnrasreg
010300121004     c                   eval      nctserdoc = 'YNA'
010400121004     c                   eval      nctdivisa  = regdivisa
010500121004     c                   eval      nctcambio  = regcambio
010600121004     c                   eval      ncttnr = 'S'
010700121004     c                   eval      nctdrp = regdtdoc
010800121004     c                   eval      nctdrp = regnrdoc
010900121004     c                   eval      nctimr = regimporto
011000121004     c                   eval      nctimpord = nctimr
011100121004     c                   eval      nctdivisap = regdivisa
011200121004     c                   eval      nctcambiop = regcambio
011300121004
011400121004     c                   do        *hival
011500121004     c     K_ndiva       reade     ndiva01l
011600121004
011700121004     c                   if        %eof(ndiva01l)
011800121004     c                   leave
011900121004     c                   endif
012000121004     c
012100121004     c                   clear                   ynnct000
012200121004     c
012300121004      *----------------------------------------------------*
012400121004      * Reperisco numeratore
012500121004      *----------------------------------------------------*
012600121004     c     repnum        begsr
012700121004      * imposto "1" per reperire il numero e non aggiornare in fase di test (da modificare )
012800121004     C                   MOVE      '1'           XNMRIC            1
012900121004     C**!!**             MOVE      '2'           XNMRIC            1
013000121004     c                   MOVE      '201'         XNMSOC            3
013100121004     c                   MOVE      *BLANKS       XNMUNI            8
013200121004     c                   MOVE      'CG'          XNMCTB            2
013300121004     c                   MOVE      *BLANKS       XNMKEY            8
013400121004     c                   MOVE      'YNA'         XNMSER            4
013500121004     c                   MOVE      *OFF          XNMCMT            1
013600121004     c                   MOVE      *off          XNMIVA            1
013700121004     c                   MOVE      *OFF          XNMREG            1
013800121004     c                   MOVE      '0'           XNMerr            1
013900121004      *
014000121004     c                   CALLB     'XNMR'        PNMR
014100121004      *
014200121004     c     xnmerr        ifne      '0'
014300121004     c                   seton                                        h5
014400121004     c                   end
014500121004     C                   z-add     xnmnum        num_int           9 0
014600121004      *
014700121004     c                   endsr
