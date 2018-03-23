000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090122     Ftivgdtmp  UF   E             DISK
000500030113     FtnvashsewtO    E             DISK
000600030113     FTITAS30C  IF   E           K DISK
000700991027
000800090122     D tnvasds       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030115     D wrkCAS          s             18  5
001300020729
001400991027
001500991027     C                   EXSR      traduci
001600921023     C                   SETON                                        LR
001700991027
001800991027     C     traduci       BEGSR
001900991027     C*
002000090122     C                   READ      tivgdtmp
002100090122     C                   DOW       not %eof(tivgdtmp)
002200090122     C                   movel     vgddta        tnvasds
002300020508     C*
002400020508     C* Innanzitutto inizializzo il formato record del file tradotto
002500030113     C                   CLEAR                   TNVASHSE
002600030113     C*
002700030113     C* Poi "chaino" la bolla x reperire alcune informazioni mancanti nel file dei contrassegni
002800030113     C     KEYVAS        CHAIN     TITAS30C
002900030113     C                   IF        %found(TITAS30C)
003000030113     C*
003100030113     C* Quindi effettuo la traduzione vera e propria:
003200030113     C* - lettera di vettura
003300030113     C                   MOVEL     tasLNP        HSENLDV
003400030113     C                   MOVE      tasNSP        HSENLDV
003500030113     C* - data spedizione
003600030113     C                   MOVEL     tasAAS        HSEDSP
003700030113     C                   MOVE      tasMGS        HSEDSP
003800030113     C* - ragione sociale destinatario
003900030113     C                   MOVEL     tasRSD        HSERSD
004000030113     C* - riferimento mittente
004100030113     C                   MOVEL     vasRMA        HSERMA
004200030113     C* - importo contrassegno in lire (o EURO senza decimali)
004300030113     C                   MOVE      vasCAS        HSECASLIRE
004400030113     C* - data valuta
004500030113     C                   MOVE      vasDVA        HSEDVA
004600030113     C* - divisa contrassegno
004700030113     C                   MOVEL     vasVCA        HSEVCA
004800030113     C* - importo contrassegno in EURO
004900030115     C                   EVAL      wrkCAS = vasCAS
005000030115     C                   EVAL      HSECAS = %editw(wrkCAS:'             ,     ')
005100020508     C*
005200030113     C                   WRITE     TNVASHSE
005300090122     C                   DELETE    tivgd000
005400030113     C                   ENDIF
005500020508     C*
005600090122     C                   READ      tivgdtmp
005700030113     C                   ENDDO
005800991027     C*
005900030113     C                   EVAL      wrkesito = '0'
006000991027     C*
006100910830     C                   ENDSR
006200020508
006300020508
006400020508
006500991027
006600991027      /TITLE Operazioni iniziali.
006700991027     C     *inzsr        BEGSR
006800991027     C*
006900991027     C     *ENTRY        PLIST
007000030113     C                   PARM                    prmppt
007100030113     C     wrkesito      PARM      wrkesito      prmesito
007200030113     C*
007300030113     C* Definizione chiavi di lettura
007400030113     C*
007500030113     C* - TITAS30C: PARZIALE
007600030113     C     KEYVAS        KLIST
007700030113     C                   KFLD                    vasAAS
007800030113     C                   KFLD                    vasLNP
007900030113     C                   KFLD                    vasNRS
008000030113     C                   KFLD                    vasNSP
008100991027     C*
008200991027     C                   ENDSR
