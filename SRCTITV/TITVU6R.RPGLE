000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700060804     D dtllso        e ds
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100060804     D wNum11_8        s             11  8 inz
001200060804     D wAlfa11         s             11    inz
001300060804     D wSegno          s              1    inz
001400060804     D wSepDec         s              1    inz(',')
001500070112     D wRecOut         s           1024    varying
001600991027
001700060804     C                   exsr      traduci
001800060804     C                   seton                                        LR
001900991027
002000060804
002100991027     C     traduci       BEGSR
002200991027     C*
002300060804     C                   read      TIVGDTMP
002400991027     C                   dow       not %eof
002500060804     C                   eval      dtllso = vgdDTA
002600050121     C*
002700050121     C* Inizializzazione campi e variabili d out
002800060804     C                   clear                   tivaw000
002900060804     C*
003000060804     C                   eval      wRecOut = %subst(vgdDTA:1:157)
003100060804     C*
003200060804     C* Effettuo determinate considerazioni x portare nella forma ASCII editata con segno
003300060804     C* le coordinale geo-stazionarie
003400060804     C                   if        §tllsoXLO < *zeros
003500060804     C                   eval      wNum11_8 = %abs(§tllsoXLO)
003600060804     C                   eval      wSegno = '-'
003700060804     C                   else
003800060804     C                   eval      wNum11_8 = §tllsoXLO
003900060804     C                   eval      wSegno = *blanks
004000060804     C                   endif
004100060804     C                   move(p)   wNum11_8      wAlfa11
004200060804     C                   eval      wRecOut = wRecOut + wSegno +
004300060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
004400060804     C                                       %subst(wAlfa11:4:8)
004500060804     C*
004600060804     C                   if        §tllsoYLA < *zeros
004700060804     C                   eval      wNum11_8 = %abs(§tllsoYLA)
004800060804     C                   eval      wSegno = '-'
004900060804     C                   else
005000060804     C                   eval      wNum11_8 = §tllsoYLA
005100060804     C                   eval      wSegno = *blanks
005200060804     C                   endif
005300060804     C                   move(p)   wNum11_8      wAlfa11
005400060804     C                   eval      wRecOut = wRecOut + wSegno +
005500060804     C                                       %subst(wAlfa11:1:3) + wSepDec +
005600060804     C                                       %subst(wAlfa11:4:8)
005700070112     C*
005800070112     C* Quindi proseguo con il resto dei dati
005900070112     C                   eval      wRecOut = %subst(wRecOut:1:183) +
006000070406     C                             %subst(vgdDTA:180:%len(DTLLSO)-180+1)
006100060804     C*
006200060804     C* Valorizzo il buffer del file d output
006300060804     C                   eval      vawdta = wRecOut
006400930409     C*
006500050121     C                   WRITE     TIVAW000
006600050909     C                   DELETE    TIVGD000
006700991027     C*
006800050909     C                   READ      TIVGDTMP
006900991027     C                   ENDdo
007000991027     C*
007100991027     C                   eval      wrkesito = '0'
007200991027     C*
007300910830     C                   ENDSR
007400060804
007500991027
007600991027      /TITLE Operazioni iniziali.
007700991027     C     *inzsr        BEGSR
007800991027     C*
007900991027     C     *ENTRY        PLIST
008000991027     C                   parm                    prmppt
008100991027     C     wrkesito      parm      wrkesito      prmesito
008200991027     C*
008300991027     C                   ENDSR
