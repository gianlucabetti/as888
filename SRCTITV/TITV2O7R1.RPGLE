000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500130319     Ftivawwwt  UF A E             DISK
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000991027
001100130319
001200130319     C                   exsr      chkRECFILMBR
001300060804     C                   exsr      traduci
001400060804     C                   seton                                        LR
001500130319
001600130319
001700130319     C     chkRECFILMBR  BEGSR
001800130319     C*
001900130319     C                   READ      TIVAWWWT                               55
002000130319     C*
002100130319     C                   ENDSR
002200130319
002300060804
002400991027     C     traduci       BEGSR
002500130319     C*
002600130319     C* Se ancora non presente inserisco testata ORM
002700130319     C                   if        *in55
002800130319     C                   clear                   tivaw000
002900130319     C                   eval      vawDTA =
003000130319     C                             'VAOPOE'+wSepCampi+'VAOTOR'+wSepCampi+
003100130319     C                             'VAOTCO'+wSepCampi+'VAODAO'+wSepCampi+
003200130319     C                             'VAOOAO'+wSepCampi+'VAOCOR'+wSepCampi+
003300130319     C                             'VAORSR'+wSepCampi+'VAOINR'+wSepCampi+
003400130319     C                             'VAOCAR'+wSepCampi+'VAOLOR'+wSepCampi+
003500130319     C                             'VAOPRR'+wSepCampi+'VAONAR'+wSepCampi+
003600171019     C                             'VAOCRC'+wSepCampi+'VAOORR'+wSepCampi+
003700130319     C                             'VAONAM'+wSepCampi+'VAONCL'+wSepCampi+
003800130319     C                             'VAOPKG'+wSepCampi+'VAOPAG'+wSepCampi+
003900130319     C                             'VAOKSC'+wSepCampi+'VAODDT'+wSepCampi+
004000130319     C                             'VAOCTR'+wSepCampi+'VAODAR'+wSepCampi+
004100130319     C                             'VAORFA'+wSepCampi+'VAOFLO'
004200130319     C                   WRITE     TIVAW000
004300130319     C                   endif
004400991027     C*
004500060804     C                   read      TIVGDTMP
004600991027     C                   dow       not %eof
004700050121     C*
004800050121     C* Inizializzazione campi e variabili d out
004900060804     C                   clear                   tivaw000
005000060804     C*
005100060804     C* Valorizzo il buffer del file d output
005200080130     C                   eval      vawDTA = vgdDTA
005300930409     C*
005400050121     C                   WRITE     TIVAW000
005500121023     C*
005600050909     C                   DELETE    TIVGD000
005700050909     C                   READ      TIVGDTMP
005800991027     C                   ENDdo
005900991027     C*
006000991027     C                   eval      wrkesito = '0'
006100991027     C*
006200910830     C                   ENDSR
006300060804
006400130319
006500991027
006600991027      /TITLE Operazioni iniziali.
006700991027     C     *inzsr        BEGSR
006800991027     C*
006900991027     C     *ENTRY        PLIST
007000991027     C                   parm                    prmppt
007100991027     C     wrkesito      parm      wrkesito      prmesito
007200130319     C*
007300130319     C* Verifico se richiesto un particolare carattere d separatore campi
007400130319     C                   IF        %subst(prmppt:1:1) <> *blanks
007500130319     C                   EVAL      wSepCampi = %subst(prmppt:1:1)
007600130319     C                   ELSE
007700130319     C                   MOVEL     ','           wSepCampi         1
007800130319     C                   ENDIF
007900991027     C*
008000991027     C                   ENDSR
