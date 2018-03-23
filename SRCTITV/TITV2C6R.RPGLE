000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500011123     Ffnvapwwt  O    E             DISK    rename(fnvap000:fnvapaaa)
000600090323     F                                     prefix(A_) commit
000700110209     FTIVGD00F  O    E             DISK    prefix(w_) rename(TIVGD000:TIVGDWWW)
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200050909     D fnvapds       e ds                  extname(fnvap00t)
001300991027
001400991027     C                   EXSR      traduci
001500921023     C                   SETON                                        LR
001600991027
001700991027     C     traduci       BEGSR
001800991027     C*
001900050909     C                   READ      TIVGDTMP
002000991027     C                   dow       not %eof
002100050909     C                   eval      fnvapds = vgdDTA
002200991027     C*
002300011123     C                   MOVE      vapIDC        A_vapIDC
002400011123     C                   MOVE      vapPOE        A_vapPOE
002500011123     C                   MOVE      vapDPE        A_vapDPE
002600011123     C                   MOVE      vapNSR        A_vapNSR
002700011123     C                   MOVE      vapNOR        A_vapNOR
002800011123     C                   MOVE      vapNRV        A_vapNRV
002900011123     C                   MOVE      vapRFA        A_vapRFA
003000011123     C                   MOVE      vapPOG        A_vapPOG
003100011123     C                   MOVE      vapDPG        A_vapDPG
003200011123     C                   MOVE      vapDAE        A_vapDAE
003300011123     C                   MOVE      vapORE        A_vapORE
003400011123     C                   MOVE      vapFAR        A_vapFAR
003500011123     C                   MOVE      vapDFA        A_vapDFA
003600011123     C                   MOVE      vapCAR        A_vapCAR
003700011123     C                   MOVE      vapDCA        A_vapDCA
003800011123     C                   MOVE      vapNOT        A_vapNOT
003900030120     C*
004000030120     C* A fine campo forzo il carattere "." a fine campo x delimitare esattamente la fine del record
004100030120     C                   MOVE      '.'           A_vapNOT
004200930409     C*
004300011123     C                   WRITE     FNVAPaaa
004400110209     C                   EXSR      wriVGD
004500050909     C                   DELETE    TIVGD000
004600991027     C*
004700050909     C                   READ      TIVGDTMP
004800991027     C                   ENDdo
004900991027     C*
005000991027     C                   eval      wrkesito = '0'
005100991027     C*
005200910830     C                   ENDSR
005300110209
005400110209
005500110209
005600110209      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
005700110209     C     wriVGD        BEGSR
005800110209     C*
005900110209     C                   clear                   tivgdwww
006000110209     C                   eval      w_vgdDTA = vgdDTA
006100120509     C                   eval      w_vgdTIP = 'PW'
006200110209     C                   eval      w_vgdKSU = vgdKSU
006300110209     C                   eval      w_vgdTSC = 'WW'
006400110209     C                   eval      w_vgdDAT = datcor
006500110209     C                   eval      w_vgdPGM = 'TITV2C6R'
006600110209     C                   write     tivgdwww
006700110209     C*
006800110209     C                   ENDSR
006900110209     C*------------------------------------------------------------------------*
007000110209
007100110209
007200991027
007300991027      /TITLE Operazioni iniziali.
007400991027     C     *inzsr        BEGSR
007500991027     C*
007600991027     C     *ENTRY        PLIST
007700991027     C                   parm                    prmppt
007800991027     C     wrkesito      parm      wrkesito      prmesito
007900110209     C*
008000110209     C                   z-add     *zeros        datcor            8 0
008100110209     C                   eval      datcor = %dec(%date() : *ISO)
008200991027     C*
008300991027     C                   ENDSR
