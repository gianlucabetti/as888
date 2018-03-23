000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600050121     Ffnorn02l  IF   E           K DISK
000700090505     Ffnora01l  IF   E           K DISK
000800991027
000900050121     D fnvapaaa      e ds                  extname(fnvapwwt) prefix(A_)
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300050909     D fnvapds       e ds                  extname(fnvap00t)
001400991027
001500991027     C                   EXSR      traduci
001600921023     C                   SETON                                        LR
001700991027
001800991027     C     traduci       BEGSR
001900991027     C*
002000050909     C                   READ      TIVGDTMP
002100991027     C                   dow       not %eof
002200050909     C                   eval      fnvapds = vgdDTA
002300991027     C*
002400011123     C                   MOVE      vapIDC        A_vapIDC
002500011123     C                   MOVE      vapPOE        A_vapPOE
002600011123     C                   MOVE      vapDPE        A_vapDPE
002700011123     C                   MOVE      vapNSR        A_vapNSR
002800011123     C                   MOVE      vapNOR        A_vapNOR
002900011123     C                   MOVE      vapNRV        A_vapNRV
003000011123     C                   MOVE      vapRFA        A_vapRFA
003100011123     C                   MOVE      vapPOG        A_vapPOG
003200011123     C                   MOVE      vapDPG        A_vapDPG
003300011123     C                   MOVE      vapDAE        A_vapDAE
003400011123     C                   MOVE      vapORE        A_vapORE
003500011123     C                   MOVE      vapFAR        A_vapFAR
003600011123     C                   MOVE      vapDFA        A_vapDFA
003700011123     C                   MOVE      vapCAR        A_vapCAR
003800011123     C                   MOVE      vapDCA        A_vapDCA
003900011123     C                   MOVE      vapNOT        A_vapNOT
004000050121     C*
004100050121     C* Inizializzazione campi e variabili d out
004200050121     C                   CLEAR                   tivaw000
004300050124     C*
004400050124     C* Inizializzo la variabile contenete la posizione fino a cui considerare il buffer del VAP
004500050124     C                   Z-ADD     177           wPosVAP           3 0
004600050124     C*
004700050124     C* Valorizzo il campo flat d output con il buffer proveniente dal file FNVAP
004800050124     C* (tutto il tracciato tranne l'ultimo campo ovvero le note)
004900050124     C                   EVAL      vawDTA = %subst(fnvapaaa:1:wPosVAP)
005000050121     C*
005100050121     C* Aggancio il file FNORN x reperire le eventuali note fasi da ritornare al cliente
005200090505     C     KEYorn02_P    SETLL     fnorn02l
005300050121     C                   IF        %equal(fnorn02l)
005400050121     C* Inizializzo contatore x ritorno max 4 note
005500050124     C                   Z-ADD     1             wContaORN         1 0
005600050121     C* Lettura note
005700090505     C     KEYorn02_P    READE     fnorn02l
005800050121     C                   DOW       not %eof(fnorn02l) AND
005900050124     C                             wContaORN <= 4
006000050121     C* Solo note con data/ora > data/ora esecuzione fase relativa
006100050121     C                   IF        orndai*1000000+ornori >=
006200050121     C                             vapdae*1000000+vapore
006300050124     C* Aggiungo al buffer del VAP la nota fase ORM corrente
006400050124     C                   EVAL      vawDTA = %subst(vawDTA:1:wPosVAP+
006500050124     C                             (35*(wContaORN-1))) + ornnot
006600050124     C                   ADD       1             wContaORN
006700050121     C                   ENDIF
006800050121     C*
006900090505     C     KEYorn02_P    READE     fnorn02l
007000090505     C                   ENDDO
007100050121     C                   ENDIF
007200090505     C*
007300090505     C* Aggancio il file FNORA x reperire eventuali: persona da contattare e note appuntamento
007400090505     C* (solo x fasi 100)
007500090505     C                   IF        vapFAR = 100
007600090505     C     KEYora01_P    SETLL     fnora01l
007700090505     C                   IF        %equal(fnora01l)
007800090511     C     KEYora01_P    READE     fnora01l
007900090505     C                   EVAL      vawDTA = %subst(vawDTA:1:wPosVAP+
008000090505     C                             (35*4)) + oraREF + oraNOTE
008100090505     C                   ENDIF
008200090505     C                   ENDIF
008300030120     C*
008400030120     C* A fine campo forzo il carattere "." a fine campo x delimitare esattamente la fine del record
008500050121     C                   MOVE      '.'           vawDTA
008600930409     C*
008700050121     C                   WRITE     TIVAW000
008800050909     C                   DELETE    TIVGD000
008900991027     C*
009000050909     C                   READ      TIVGDTMP
009100991027     C                   ENDdo
009200991027     C*
009300991027     C                   eval      wrkesito = '0'
009400991027     C*
009500910830     C                   ENDSR
009600991027
009700991027      /TITLE Operazioni iniziali.
009800991027     C     *inzsr        BEGSR
009900991027     C*
010000991027     C     *ENTRY        PLIST
010100991027     C                   parm                    prmppt
010200991027     C     wrkesito      parm      wrkesito      prmesito
010300050121     C*
010400050121     C* Definizione chiavi
010500050121     C*
010600050121     C* File: FNORN02L
010700090505     C     KEYorn02_P    KLIST
010800050121     C                   KFLD                    VAPPOE
010900050121     C                   KFLD                    VAPNSR
011000050121     C                   KFLD                    VAPNOR
011100050121     C                   KFLD                    VAPNRV
011200050121     C                   KFLD                    VAPFAR
011300090505     C*
011400090505     C* File: FNORA01L
011500090505     C     KEYora01_P    KLIST
011600090505     C                   KFLD                    VAPPOE
011700090505     C                   KFLD                    VAPNSR
011800090505     C                   KFLD                    VAPNOR
011900090505     C                   KFLD                    VAPNRV
012000090505     C                   KFLD                    VAPFAR
012100991027     C*
012200991027     C                   ENDSR
