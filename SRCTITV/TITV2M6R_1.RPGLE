000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200121024     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500081007     Ftivawwwt  O    E             DISK
000600991027
000700080924     D fnvac00t      e ds
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100081007
001200081007
001300081007     D ds_out_cli      ds
001400121017     D  do_filler1                    1    inz
001500121017     D  do_RMA                        8    inz
001600121017     D  do_CBO                        2    inz
001700121017     D  do_DataSped                   6    inz(*zeros)
001800121017     D  do_filler2                    1    inz
001900121017     D  do_RSD                       30    inz
002000121017     D  do_DCM                        6    inz(*zeros)
002100121017     D  do_IDcorriere                10    inz('4510')
002200021007
002300121024
002400121024     D*------------------
002500121024     D* LINKING A DEFINIZIONI ESTERNE
002600121024     D*------------------
002700121024     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002800121024     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002900121024
003000121024
003100021007
003200991027     C                   EXSR      traduci
003300921023     C                   SETON                                        LR
003400991027
003500081007
003600081007
003700991027     C     traduci       BEGSR
003800991027     C*
003900121017     C                   read      TIVGDTMP
004000121017     C                   dow       not %eof(TIVGDTMP)
004100080924     C*
004200121024     C                   setoff                                       31
004300121017     C                   eval      fnvac00t = vgdDTA
004400121024     C*
004500121024     C* Se richiesto nei parametri traduzione solo spedizione valide ai fini della spedizione =>
004600121024     C                   if        %subst(prmppt:1:1) = 'O'
004700121024     C*
004800121024     C* Inizializzo i campi d wrk
004900121024     C                   movel     *blanks       wEsito1           1
005000121024     C*
005100121024     C* Chiamata metodo GetLblTyp
005200121024     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
005300121024     C                                                vacAAS
005400121024     C                                               :vacLNP
005500121024     C                                               :vacNRS
005600121024     C                                               :vacNSP
005700121024     C                                               :pOutLblTyp
005800121024     C                                               :pOutAnnoBO
005900121024     C                                               :pOutLineaParBO
006000121024     C                                               :pOutSerieBO
006100121024     C                                               :pOutNumSpedBO
006200121024     C                                               :pOutDcmBO
006300121024     C                                               :pOutCcaBO
006400121024     C                                               :pOutRblBO))
006500121024     C*
006600121024     C* Se trattasi d bolla originale proseguo con le verifiche
006700121024     C                   if        pOutLblTyp <> 'O'
006800121024     C                   seton                                        31
006900121024     C                   endif
007000121024     C                   endif
007100121024     C*
007200121019     C                   reset                   ds_out_cli
007300121017     C                   eval      do_RMA      = vacRMA
007400121017     C                   eval      do_CBO      = vacCBO
007500121017     C                   eval      do_DataSped = %subst(%char(vacAAS*10000+
007600121017     C                                           vacMGS):3:6)
007700121017     C                   eval      do_RSD      = vacRSD
007800121017     C                   eval      do_DCM      = %subst(%char(vacDCM):3:6)
007900121017     C*
008000121024     C  N31              exsr      WRIOUT
008100080613     C*
008200121017     C                   delete    TIVGD000
008300121017     C                   read      TIVGDTMP
008400121017     C*
008500121017     C                   enddo
008600991027     C*
008700991027     C                   eval      wrkesito = '0'
008800991027     C*
008900910830     C                   ENDSR
009000090225
009100090225
009200090225
009300090225     C     WRIOUT        BEGSR
009400090225     C*
009500090225     C                   eval      vawDTA = ds_out_cli
009600090225     C*
009700090225     C                   WRITE     TIVAW000
009800090225     C*
009900090225     C                   ENDSR
010000991027
010100081007
010200081007
010300991027      /TITLE Operazioni iniziali.
010400991027     C     *inzsr        BEGSR
010500991027     C*
010600121024     C     *entry        PLIST
010700991027     C                   parm                    prmppt
010800991027     C     wrkesito      parm      wrkesito      prmesito
010900090225     C*
011000991027     C                   ENDSR
