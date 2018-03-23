000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090122     Ftivgdtmp  UF   E             DISK
000500130506     Ftivawwwt  O    E             DISK
000600030113     FTITAS30C  IF   E           K DISK
000700991027
000800090122     D tnvasds       e ds                  extname(tnvas00t)
000900130506     D titv2n8vs     e ds
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300030115     D wrkCAS          s             18  5
001400020729
001500991027
001600991027     C                   EXSR      traduci
001700921023     C                   SETON                                        LR
001800991027
001900991027     C     traduci       BEGSR
002000991027     C*
002100090122     C                   READ      tivgdtmp
002200090122     C                   DOW       not %eof(tivgdtmp)
002300090122     C                   movel     vgddta        tnvasds
002400020508     C*
002500020508     C* Innanzitutto inizializzo il formato record del file tradotto
002600130506     C                   CLEAR                   titv2n8vs
002700030113     C*
002800030113     C* Poi "chaino" la bolla x reperire alcune informazioni mancanti nel file dei contrassegni
002900030113     C     KEYVAS        CHAIN     TITAS30C
003000030113     C                   IF        %found(TITAS30C)
003100030113     C*
003200030113     C* Quindi effettuo la traduzione vera e propria:
003300030113     C* - lettera di vettura
003400030113     C                   MOVEL     tasLNP        HSENLDV
003500030113     C                   MOVE      tasNSP        HSENLDV
003600030113     C* - data spedizione
003700030113     C                   MOVEL     tasAAS        HSEDSP
003800030113     C                   MOVE      tasMGS        HSEDSP
003900030113     C* - ragione sociale destinatario
004000030113     C                   MOVEL     tasRSD        HSERSD
004100030113     C* - riferimento mittente
004200030113     C                   MOVEL     vasRMA        HSERMA
004300030113     C* - importo contrassegno in lire (o EURO senza decimali)
004400030113     C                   MOVE      vasCAS        HSECASLIRE
004500030113     C* - data valuta
004600030113     C                   MOVE      vasDVA        HSEDVA
004700030113     C* - divisa contrassegno
004800030113     C                   MOVEL     vasVCA        HSEVCA
004900030113     C* - importo contrassegno in EURO
005000030115     C                   EVAL      wrkCAS = vasCAS
005100030115     C                   EVAL      HSECAS = %editw(wrkCAS:'             ,     ')
005200020508     C*
005300130506     C                   EVAL      vawdta = titv2n8vs
005400130506     C*
005500130506     C                   WRITE     TIVAW000
005600090122     C                   DELETE    tivgd000
005700030113     C                   ENDIF
005800020508     C*
005900090122     C                   READ      tivgdtmp
006000030113     C                   ENDDO
006100991027     C*
006200030113     C                   EVAL      wrkesito = '0'
006300991027     C*
006400910830     C                   ENDSR
006500020508
006600020508
006700020508
006800991027
006900991027      /TITLE Operazioni iniziali.
007000991027     C     *inzsr        BEGSR
007100991027     C*
007200991027     C     *ENTRY        PLIST
007300030113     C                   PARM                    prmppt
007400030113     C     wrkesito      PARM      wrkesito      prmesito
007500030113     C*
007600030113     C* Definizione chiavi di lettura
007700030113     C*
007800030113     C* - TITAS30C: PARZIALE
007900030113     C     KEYVAS        KLIST
008000030113     C                   KFLD                    vasAAS
008100030113     C                   KFLD                    vasLNP
008200030113     C                   KFLD                    vasNRS
008300030113     C                   KFLD                    vasNSP
008400991027     C*
008500991027     C                   ENDSR
