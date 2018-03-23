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
002200170516     D  do_fAPPUNTAM                  1    inz
002300170516     D  do_fGIAC                      1    inz
002400021007
002500121024
002600121024     D*------------------
002700121024     D* LINKING A DEFINIZIONI ESTERNE
002800121024     D*------------------
002900121024     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003000121024     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003100121024
003200121024
003300021007
003400991027     C                   EXSR      traduci
003500921023     C                   SETON                                        LR
003600991027
003700081007
003800081007
003900991027     C     traduci       BEGSR
004000991027     C*
004100121017     C                   read      TIVGDTMP
004200121017     C                   dow       not %eof(TIVGDTMP)
004300080924     C*
004400121024     C                   setoff                                       31
004500121017     C                   eval      fnvac00t = vgdDTA
004600121024     C*
004700121024     C* Se richiesto nei parametri traduzione solo spedizione valide ai fini della spedizione =>
004800121024     C                   if        %subst(prmppt:1:1) = 'O'
004900121024     C*
005000121024     C* Inizializzo i campi d wrk
005100121024     C                   movel     *blanks       wEsito1           1
005200121024     C*
005300121024     C* Chiamata metodo GetLblTyp
005400121024     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
005500121024     C                                                vacAAS
005600121024     C                                               :vacLNP
005700121024     C                                               :vacNRS
005800121024     C                                               :vacNSP
005900121024     C                                               :pOutLblTyp
006000121024     C                                               :pOutAnnoBO
006100121024     C                                               :pOutLineaParBO
006200121024     C                                               :pOutSerieBO
006300121024     C                                               :pOutNumSpedBO
006400121024     C                                               :pOutDcmBO
006500121024     C                                               :pOutCcaBO
006600121024     C                                               :pOutRblBO))
006700121024     C*
006800121024     C* Se trattasi d bolla originale proseguo con le verifiche
006900121024     C                   if        pOutLblTyp <> 'O'
007000121024     C                   seton                                        31
007100121024     C                   endif
007200121024     C                   endif
007300121024     C*
007400121019     C                   reset                   ds_out_cli
007500121112     C                   eval      do_RMA      = %trim(%subst(vacRMA:3))
007600121112     C                   eval      do_CBO      = %trim(%subst(vacRMA:1:2))
007700121017     C                   eval      do_DataSped = %subst(%char(vacAAS*10000+
007800121017     C                                           vacMGS):3:6)
007900121017     C                   eval      do_RSD      = vacRSD
008000121017     C                   eval      do_DCM      = %subst(%char(vacDCM):3:6)
008100170516     C                   if        vacTC1 = 'A' or vacTC2 = 'A'
008200170516     C                   eval      do_fAPPUNTAM = 'A'
008300170516     C                   endif
008400170516     C                   if        vacDAG > *zeros
008500170516     C                   eval      do_fGIAC = 'G'
008600170516     C                   endif
008700121128     C*
008800121128     C* FORZATURE CLIENTE
008900121128     C*
009000121128     C* Il cliente richiede unicamente le spedizioni con codice bolla <> 'B'
009100121128     C                   if        %trim(do_CBO) = 'B'
009200121128     C                   seton                                        31
009300121128     C                   endif
009400121128     C*
009500121128     C* Il cliente richiede unicamente le spedizioni consegnate ok
009600121128     C                   if        vacDCM > *zeros AND
009700121128     C                             (vacCCA = *blanks OR vacCCA = '1')
009800121128     C                   else
009900121128     C                   seton                                        31
010000121128     C                   endif
010100121017     C*
010200121024     C  N31              exsr      WRIOUT
010300080613     C*
010400121017     C                   delete    TIVGD000
010500121017     C                   read      TIVGDTMP
010600121017     C*
010700121017     C                   enddo
010800991027     C*
010900991027     C                   eval      wrkesito = '0'
011000991027     C*
011100910830     C                   ENDSR
011200090225
011300090225
011400090225
011500090225     C     WRIOUT        BEGSR
011600090225     C*
011700090225     C                   eval      vawDTA = ds_out_cli
011800090225     C*
011900090225     C                   WRITE     TIVAW000
012000090225     C*
012100090225     C                   ENDSR
012200991027
012300081007
012400081007
012500991027      /TITLE Operazioni iniziali.
012600991027     C     *inzsr        BEGSR
012700991027     C*
012800121024     C     *entry        PLIST
012900991027     C                   parm                    prmppt
013000991027     C     wrkesito      parm      wrkesito      prmesito
013100090225     C*
013200991027     C                   ENDSR
