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
007300121112     C                   eval      do_RMA      = %trim(%subst(vacRMA:3))
007400121112     C                   eval      do_CBO      = %trim(%subst(vacRMA:1:2))
007500121017     C                   eval      do_DataSped = %subst(%char(vacAAS*10000+
007600121017     C                                           vacMGS):3:6)
007700121017     C                   eval      do_RSD      = vacRSD
007800121017     C                   eval      do_DCM      = %subst(%char(vacDCM):3:6)
007900121128     C*
008000121128     C* FORZATURE CLIENTE
008100121128     C*
008200121128     C* Il cliente richiede unicamente le spedizioni con codice bolla <> 'B'
008300121128     C                   if        %trim(do_CBO) = 'B'
008400121128     C                   seton                                        31
008500121128     C                   endif
008600121128     C*
008700121128     C* Il cliente richiede unicamente le spedizioni consegnate ok
008800121128     C                   if        vacDCM > *zeros AND
008900121128     C                             (vacCCA = *blanks OR vacCCA = '1')
009000121128     C                   else
009100121128     C                   seton                                        31
009200121128     C                   endif
009300121017     C*
009400121024     C  N31              exsr      WRIOUT
009500080613     C*
009600121017     C                   delete    TIVGD000
009700121017     C                   read      TIVGDTMP
009800121017     C*
009900121017     C                   enddo
010000991027     C*
010100991027     C                   eval      wrkesito = '0'
010200991027     C*
010300910830     C                   ENDSR
010400090225
010500090225
010600090225
010700090225     C     WRIOUT        BEGSR
010800090225     C*
010900090225     C                   eval      vawDTA = ds_out_cli
011000090225     C*
011100090225     C                   WRITE     TIVAW000
011200090225     C*
011300090225     C                   ENDSR
011400991027
011500081007
011600081007
011700991027      /TITLE Operazioni iniziali.
011800991027     C     *inzsr        BEGSR
011900991027     C*
012000121024     C     *entry        PLIST
012100991027     C                   parm                    prmppt
012200991027     C     wrkesito      parm      wrkesito      prmesito
012300090225     C*
012400991027     C                   ENDSR
