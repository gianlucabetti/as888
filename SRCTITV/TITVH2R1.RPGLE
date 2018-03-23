000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600050121     Ffnorn02l  IF   E           K DISK
000700090505     Ffnora01l  IF   E           K DISK
000800141023     Ftitas30c  IF   E           K DISK
000900991027
001000050121     D fnvapaaa      e ds                  extname(fnvapwwt) prefix(A_)
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400050909     D fnvapds       e ds                  extname(fnvap00t)
001500141023
001600141023     D ds_ORMRC        ds
001700141023     D  rc_prefix                     1
001800141023     D  rc_aas2                       2  0
001900141023     D  rc_lnp                        3  0
002000141023     D  rc_nrs                        2  0
002100141023     D  rc_nsp                        7  0
002200991027
002300991027     C                   EXSR      traduci
002400921023     C                   SETON                                        LR
002500991027
002600991027     C     traduci       BEGSR
002700991027     C*
002800050909     C                   READ      TIVGDTMP
002900991027     C                   dow       not %eof
003000050909     C                   eval      fnvapds = vgdDTA
003100991027     C*
003200141023     C* Forzatura in caso di ORM "RC"
003300141023     C                   clear                   ds_ORMRC
003400141023     C                   if        vapNSR > *zeros AND
003500141023     C                             %subst(vapRFA:1:1) = 'B'
003600141023     C                   eval      ds_ORMRC = vapRFA
003700141023     C                   eval      tasAAS = rc_aas2 + 2000
003800141023     C                   eval      tasLNP = rc_lnp
003900141023     C                   eval      tasNRS = rc_nrs
004000141023     C                   eval      tasNSP = rc_nsp
004100141023     C     KEYtas30_P    chain     titas30c
004200141023     C                   if        %found(titas30c)
004300141023     C                   eval      vapRFA = %trim(%char(tasRMN))
004400141023     C                   endif
004500141023     C                   endif
004600141023     C*
004700011123     C                   MOVE      vapIDC        A_vapIDC
004800011123     C                   MOVE      vapPOE        A_vapPOE
004900011123     C                   MOVE      vapDPE        A_vapDPE
005000011123     C                   MOVE      vapNSR        A_vapNSR
005100011123     C                   MOVE      vapNOR        A_vapNOR
005200011123     C                   MOVE      vapNRV        A_vapNRV
005300011123     C                   MOVE      vapRFA        A_vapRFA
005400011123     C                   MOVE      vapPOG        A_vapPOG
005500011123     C                   MOVE      vapDPG        A_vapDPG
005600011123     C                   MOVE      vapDAE        A_vapDAE
005700011123     C                   MOVE      vapORE        A_vapORE
005800011123     C                   MOVE      vapFAR        A_vapFAR
005900011123     C                   MOVE      vapDFA        A_vapDFA
006000011123     C                   MOVE      vapCAR        A_vapCAR
006100011123     C                   MOVE      vapDCA        A_vapDCA
006200011123     C                   MOVE      vapNOT        A_vapNOT
006300050121     C*
006400050121     C* Inizializzazione campi e variabili d out
006500050121     C                   CLEAR                   tivaw000
006600050124     C*
006700050124     C* Inizializzo la variabile contenete la posizione fino a cui considerare il buffer del VAP
006800050124     C                   Z-ADD     177           wPosVAP           3 0
006900050124     C*
007000050124     C* Valorizzo il campo flat d output con il buffer proveniente dal file FNVAP
007100050124     C* (tutto il tracciato tranne l'ultimo campo ovvero le note)
007200050124     C                   EVAL      vawDTA = %subst(fnvapaaa:1:wPosVAP)
007300050121     C*
007400050121     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
007500090505     C     KEYorn02_P    SETLL     fnorn02l
007600050121     C                   IF        %equal(fnorn02l)
007700050121     C* Inizializzo contatore x ritorno max 4 note
007800050124     C                   Z-ADD     1             wContaORN         1 0
007900050121     C* Lettura note
008000090505     C     KEYorn02_P    READE     fnorn02l
008100050121     C                   DOW       not %eof(fnorn02l) AND
008200050124     C                             wContaORN <= 4
008300050121     C* Solo note con data/ora > data/ora esecuzione fase relativa
008400050121     C                   IF        orndai*1000000+ornori >=
008500050121     C                             vapdae*1000000+vapore
008600050124     C* Aggiungo al buffer del VAP la nota fase ORM corrente
008700050124     C                   EVAL      vawDTA = %subst(vawDTA:1:wPosVAP+
008800050124     C                             (35*(wContaORN-1))) + ornnot
008900050124     C                   ADD       1             wContaORN
009000050121     C                   ENDIF
009100050121     C*
009200090505     C     KEYorn02_P    READE     fnorn02l
009300090505     C                   ENDDO
009400050121     C                   ENDIF
009500090505     C*
009600090505     C* Aggancio il file FNORA x reperire eventuali: persona da contattare e note appuntamento
009700090505     C* (solo x fasi 100)
009800090505     C                   IF        vapFAR = 100
009900090505     C     KEYora01_P    SETLL     fnora01l
010000090505     C                   IF        %equal(fnora01l)
010100090511     C     KEYora01_P    READE     fnora01l
010200090505     C                   EVAL      vawDTA = %subst(vawDTA:1:wPosVAP+
010300090505     C                             (35*4)) + oraREF + oraNOTE
010400090505     C                   ENDIF
010500090505     C                   ENDIF
010600030120     C*
010700030120     C* A fine campo forzo il carattere "." a fine campo x delimitare esattamente la fine del record
010800050121     C                   MOVE      '.'           vawDTA
010900930409     C*
011000050121     C                   WRITE     TIVAW000
011100050909     C                   DELETE    TIVGD000
011200991027     C*
011300050909     C                   READ      TIVGDTMP
011400991027     C                   ENDdo
011500991027     C*
011600991027     C                   eval      wrkesito = '0'
011700991027     C*
011800910830     C                   ENDSR
011900991027
012000991027      /TITLE Operazioni iniziali.
012100991027     C     *inzsr        BEGSR
012200991027     C*
012300991027     C     *ENTRY        PLIST
012400991027     C                   parm                    prmppt
012500991027     C     wrkesito      parm      wrkesito      prmesito
012600050121     C*
012700050121     C* Definizione chiavi
012800050121     C*
012900050121     C* File: FNORN02L
013000090505     C     KEYorn02_P    KLIST
013100050121     C                   KFLD                    VAPPOE
013200050121     C                   KFLD                    VAPNSR
013300050121     C                   KFLD                    VAPNOR
013400050121     C                   KFLD                    VAPNRV
013500050121     C                   KFLD                    VAPFAR
013600090505     C*
013700090505     C* File: FNORA01L
013800090505     C     KEYora01_P    KLIST
013900090505     C                   KFLD                    VAPPOE
014000090505     C                   KFLD                    VAPNSR
014100090505     C                   KFLD                    VAPNOR
014200090505     C                   KFLD                    VAPNRV
014300090505     C                   KFLD                    VAPFAR
014400141023     C*
014500141023     C* File: TITAS30C
014600141023     C     KEYtas30_P    KLIST
014700141023     C                   KFLD                    TASAAS
014800141023     C                   KFLD                    TASLNP
014900141023     C                   KFLD                    TASNRS
015000141023     C                   KFLD                    TASNSP
015100991027     C*
015200991027     C                   ENDSR
