000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000629
000400090320     Ftivgdtmp  uF   E             DISK
000500110504     Ftivawwwt  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000991027
001100110718     D*-------------------
001200110718     D* DS OUTOUT X DOWNLOAD
001300110718     D*-------------------
001400110718     D DS_OUTDWL       DS
001500110718     D  O_WFPTIP                      1A
001600110718     D  O_WFPCOD                     11S 0
001700110718     D  O_WFPPRO                      2A
001800110718     D  O_WFPCAP                      9A
001900110718     D  O_WFPNAZ                      3A
002000110718     D  O_WFPEML                     60A
002100110718     D  O_WFPABC                      1A
002200110718     D  O_WFPFIL                      3S 0
002300110718     D  O_WFPCOM                      7S 0
002400110718     D  O_WFPDEC                     60A
002500110718     D  O_WFPEMC                     60A
002600110718     D  O_WFPDES                     20A
002700110718     D  O_WFPINF                     20A
002800110718     D  O_WFPCPF                      5S 0
002900110718     D  O_WFPLOF                     20A
003000110718     D  O_WFPPRF                      2A
003100110718     D  O_WFPTEF                     13A
003200110718     D  O_WFPFAF                     13A
003300110909     D  O_WFPARE                      3S 0
003400010131
003500010131
003600991027     C                   exsr      traduci
003700991027     C                   seton                                        lr
003800000629
003900110503
004000000629
004100000629     C     traduci       begsr
004200000629     C*
004300090320     C                   READ      tivgdtmp
004400000629     C                   dow       not %eof
004500110503     C*
004600100729     C*
004700110718     C                   clear                   DS_OUTDWL
004800110718     C                   movel     vgddta        DS_OUTDWL
004900110718     C*
005000110718     C* Inizializzazione flag d ok a download
005100110718     C                   setoff                                       31
005200010528     C*
005300110718     C* Applico filtri richiesti
005400110718     C                   if        O_WFPNAZ = *blanks AND
005500110718     C                             O_WFPEML > *blanks
005600110718     C                   else
005700110718     C                   seton                                        31
005800110704     C                   endif
005900110902     C*
006000110902     C* Forzature su provincie
006100110902     C                   select
006200110902     C                   when      %trim(O_WFPPRO) = 'FO'
006300110902     C                   eval      O_WFPPRO = 'FC'
006400110902     C                   when      %trim(O_WFPPRO) = 'PS'
006500110902     C                   eval      O_WFPPRO = 'PU'
006600110905     C                   endsl
006700110905     C*
006800110905     C                   select
006900110902     C                   when      %trim(O_WFPPRF) = 'FO'
007000110902     C                   eval      O_WFPPRF = 'FC'
007100110902     C                   when      %trim(O_WFPPRF) = 'PS'
007200110902     C                   eval      O_WFPPRF = 'PU'
007300110902     C                   endsl
007400000629     C*
007500110718     C                   eval      vawDTA =
007600110718     C                                    '"'+   %trim(O_WFPTIP)+'",'     +
007700110718     C                                    %trim(%editc(O_WFPCOD:'3'))+',' +
007800110718     C                                    '"'+   %trim(O_WFPPRO)+'",'     +
007900110718     C                                    '"'+   %trim(O_WFPCAP)+'",'     +
008000110718     C                                    '"'+   %trim(O_WFPNAZ)+'",'     +
008100110718     C                                    '"'+   %trim(O_WFPEML)+'",'     +
008200110718     C                                    '"'+   %trim(O_WFPABC)+'",'     +
008300110718     C                                    %trim(%editc(O_WFPFIL:'3'))+',' +
008400110718     C                                    %trim(%editc(O_WFPCOM:'3'))+',' +
008500110718     C                                    '"'+   %trim(O_WFPDEC)+'",'     +
008600120307     C                                    '"'+%trim(O_WFPEMC)+'@brt.it",'+
008700110718     C                                    '"'+   %trim(O_WFPDES)+'",'     +
008800110718     C                                    '"'+   %trim(O_WFPINF)+'",'     +
008900110718     C                                    %trim(%editc(O_WFPCPF:'X'))+',' +
009000110718     C                                    '"'+   %trim(O_WFPLOF)+'",'     +
009100110718     C                                    '"'+   %trim(O_WFPPRF)+'",'     +
009200110718     C                                    '"'+   %trim(O_WFPTEF)+'",'     +
009300110909     C                                    '"'+   %trim(O_WFPFAF)+'",'     +
009400110909     C                                    %trim(%editc(O_WFPARE:'3'))
009500110503     C*
009600110718     C  N31              WRITE     TIVAW000
009700090320     C                   delete    tivgd000
009800000629     C*
009900090320     C                   READ      tivgdtmp
010000000629     C                   ENDdo
010100000629     C*
010200000629     C                   eval      wrkesito = '0'
010300000629     C*
010400000629     C                   ENDSR
010500010131
010600000629
010700991027
010800991027      /TITLE Operazioni iniziali.
010900991027     C     *inzsr        BEGSR
011000991027     C*
011100991027     C     *ENTRY        PLIST
011200991027     C                   parm                    prmppt
011300991027     C     wrkesito      parm      wrkesito      prmesito
011400991027     C*
011500991027     C                   ENDSR
