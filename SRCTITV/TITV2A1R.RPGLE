000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030709     FTITAS30C  IF   E           K DISK
000500030709     FTITAA30C  IF   E           K DISK
000600090325     Ftivgdtmp  uF   E             DISK
000700100209     FTIVAWWWT  UF A E             DISK    commit
000800030709     D*------------------
000900030709     D* DS REPERIMENTO DATI CLIENTE
001000030709     D*-------------------
001100030709     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001200030709     D ACODS         E DS                  EXTNAME(CNACO00F)
001300030709     D INDDS         E DS                  EXTNAME(CNIND00F)
001400030709     D CLPDS         E DS                  EXTNAME(CNCLP00F)
001500030709     D CLSDS         E DS                  EXTNAME(FNCLS00F)
001600991027
001700991027     D prmppt          s             50
001800991027     D prmesito        s              1
001900991027     D wrkesito        s                   like(prmesito)
002000090325     D tivafds       e ds                  extname(tivaf00t)
002100090325     D dsvafa        e ds                  extname(tivafwwt1)
002200070316     D sepFld          s              1
002300991027
002400030908     C                   EXSR      chkRECFILMBR
002500991027     C                   EXSR      traduci
002600921023     C                   SETON                                        LR
002700030908
002800030908     C     chkRECFILMBR  BEGSR
002900030908     C*
003000100209     C                   READ      TIVAWWWT                               55
003100030908     C*
003200030908     C                   ENDSR
003300991027
003400991027     C     traduci       BEGSR
003500030325     C*
003600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
003700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
003800100209     C                   EVAL      VAWDTA  = '"VAFAAS"'+sepFld+
003900070316     C                                       '"VAFLNP"'+sepFld+
004000070316     C                                       '"VAFNRS"'+sepFld+
004100070316     C                                       '"VAFNSP"'+sepFld+
004200070316     C                                       '"VAFTBL"'+sepFld+
004300070316     C                                       '"VAFMGS"'+sepFld+
004400070316     C                                       '"VAFKSC"'+sepFld+
004500070316     C                                       '"VAFLNA"'+sepFld+
004600070316     C                                       '"VAFNCL"'+sepFld+
004700070316     C                                       '"VAFPKB"'+sepFld+
004800070316     C                                       '"VAFVLF"'+sepFld+
004900070316     C                                       '"VAFQFT"'+sepFld+
005000070316     C                                       '"VAFPOR"'+sepFld+
005100070316     C                                       '"VAFSV1"'+sepFld+
005200070316     C                                       '"VAFVA1"'+sepFld+
005300070316     C                                       '"VAFSV2"'+sepFld+
005400070316     C                                       '"VAFVA2"'+sepFld+
005500070316     C                                       '"VAFSV3"'+sepFld+
005600070316     C                                       '"VAFVA3"'+sepFld+
005700070316     C                                       '"VAFSV4"'+sepFld+
005800070316     C                                       '"VAFVA4"'+sepFld+
005900070316     C                                       '"VAFSV5"'+sepFld+
006000070316     C                                       '"VAFVA5"'+sepFld+
006100070316     C                                       '"VAFSV6"'+sepFld+
006200070316     C                                       '"VAFVA6"'+sepFld+
006300070316     C                                       '"VAFSV7"'+sepFld+
006400070316     C                                       '"VAFVA7"'+sepFld+
006500070316     C                                       '"VAFSV8"'+sepFld+
006600070316     C                                       '"VAFVA8"'+sepFld+
006700070316     C                                       '"VAFVAX"'+sepFld+
006800070316     C                                       '"VAFIMV"'+sepFld+
006900070316     C                                       '"VAFDFT"'+sepFld+
007000070316     C                                       '"VAFNFT"'+sepFld+
007100070316     C                                       '"VAFFIV"'+sepFld+
007200070316     C                                       '"VAFDIV"'+sepFld+
007300070316     C                                       '"VAFDRT"'+sepFld+
007400070316     C                                       '"VAFRMN"'+sepFld+
007500070316     C                                       '"VAFRMA"'+sepFld+
007600070316     C                                       '"VAFRMX"'+sepFld+
007700070316     C                                       '"VAFFTC"'+sepFld+
007800070316     C                                       '"VAFTC2"'+sepFld+
007900070316     C                                       '"VAFTSP"'+sepFld+
008000070316     C                                       '"VAFFAP"'+sepFld+
008100070316     C                                       '"VAFFIN"'+sepFld+
008200070316     C                                       '"VAFCTR"'+sepFld+
008300070316     C                                       '"VAFNAS"'+sepFld+
008400070316     C                                       '"VAFCTS"'+sepFld+
008500070316     C                                       '"VAFCAD"'+sepFld+
008600070316     C                                       '"VAFLOD"'+sepFld+
008700070316     C                                       '"VAFPRD"'+sepFld+
008800070316     C                                       '"VAFNZD"'+sepFld+
008900070316     C                                       '"VAFVAS"'+sepFld+
009000070316     C                                       '"VAFIAS"'+sepFld+
009100070316     C                                       '"VAFFPC"'+sepFld+
009200070316     C                                       '"VAFPKC"'+sepFld+
009300070316     C                                       '"VAFFVC"'+sepFld+
009400070316     C                                       '"VAFVLC"'+sepFld+
009500070316     C                                       '"VAFRSD"'+sepFld+
009600070316     C                                       '"VAFIND"'+sepFld+
009700070316     C                                       '"VAFRSM"'+sepFld+
009800070316     C                                       '"VAFINM"'+sepFld+
009900070316     C                                       '"VAFCAM"'+sepFld+
010000070316     C                                       '"VAFLOM"'+sepFld+
010100070316     C                                       '"VAFPRM"'+sepFld+
010200070316     C                                       '"VAFNZM"'+sepFld+
010300070316     C                                       '"VAFRMO"'+sepFld+
010400070316     C                                       '"VAFCMO"'+sepFld+
010500070316     C                                       '"VAFNMO"'+sepFld+
010600100915     C                                       '"VAFPVL"'+sepFld+
010601100915     C                                       '"PALLET"'
010700030325     C*
010800100209     C                   WRITE     TIVAW000
010900030325     C                   ENDIF
011000991027     C*
011100090325     C                   READ      tivgdtmp
011200090325     C                   DOW       not %eof
011300090325     c                   movel     vgddta        tivafds
011400041220     C*
011500041220     C* Inizializzo i campi "estensione"
011600041220     C                   EVAL      VHFRSD = *blanks
011700041220     C                   EVAL      VHFIND = *blanks
011800041220     C                   EVAL      VHFRSM = *blanks
011900041220     C                   EVAL      VHFINM = *blanks
012000041220     C                   EVAL      VHFCAM = *blanks
012100041220     C                   EVAL      VHFLOM = *blanks
012200041220     C                   EVAL      VHFPRM = *blanks
012300041220     C                   EVAL      VHFNZM = *blanks
012400041220     C                   EVAL      VHFRMO = *blanks
012500041220     C                   EVAL      VHFCMO = *blanks
012600041220     C                   EVAL      VHFNMO = *blanks
012700060905     C                   Z-ADD     *zeros        wPVL              7 1
012800030709     C*
012900030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
013000030709     C     KEYtas30C     CHAIN     titas30c
013100030709     C                   IF        %found(titas30c)
013200030709     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
013300030709     C                   EVAL      VHFRSD = TASRSD
013400030709     C                   EVAL      VHFIND = TASIND
013500060905     C                   EVAL      wPVL = TASPVL
013600030709     C* X reperire i dati del mittente verifico il codice del cliente mittente:
013700030709     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
013800030709     C                   MOVE(P)   tasCCM        wCLI              4 0
013900030709     C                   IF        wCLI =    0 OR
014000030709     C                             wCLI = 8888 OR
014100030709     C                             wCLI = 9999
014200030709     C                   EVAL      taaTRC = 'M'
014300030709     C     KEYtaa30C     CHAIN     titaa30c
014400030709     C                   IF        %found(titaa30c)
014500030709     C                   EVAL      VHFRSM = TAARSC
014600030709     C                   EVAL      VHFINM = TAAIND
014700030709     C                   EVAL      VHFCAM = TAACAP
014800030709     C                   EVAL      VHFLOM = TAALOC
014900030709     C                   EVAL      VHFPRM = TAAPRV
015000070219     C                   EVAL      VHFNZM = TAANAZ
015100030709     C                   ENDIF
015200030709     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
015300030709     C                   ELSE
015400030709     C                   CLEAR                   BS69DS
015500030709     C                   CLEAR                   ACODS
015600030709     C                   CLEAR                   INDDS
015700030709     C                   CLEAR                   CLPDS
015800030709     C                   CLEAR                   CLSDS
015900030709     C                   Z-ADD     tasCCM        I69KAC
016000030709     C                   Z-ADD     tasCCM        I69KIN
016100030709     C                   CALL      'TIBS69R'
016200030709     C                   PARM                    BS69DS
016300030709     C                   PARM                    ACODS
016400030709     C                   PARM                    INDDS
016500030709     C                   PARM                    CLPDS
016600030709     C                   PARM                    CLSDS
016700030709     C     O69ERR        IFNE      '1'
016800030709     C                   EVAL      VHFRSM = ACORAG
016900030709     C                   EVAL      VHFINM = INDVIA
017000030709     C                   MOVEL(P)  INDCAP        VHFCAM
017100030709     C                   EVAL      VHFLOM = INDCIT
017200030709     C                   EVAL      VHFPRM = INDPRV
017300030709     C                   EVAL      VHFNZM = INDSTA
017400030709     C                   ENDIF
017500030709     C*
017600030709     C                   ENDIF
017700030709     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
017800030709     C                   EVAL      taaTRC = 'O'
017900030709     C     KEYtaa30C     CHAIN     titaa30c
018000030709     C                   IF        %found(titaa30c)
018100030709     C                   EVAL      VHFRMO = TAARSC
018200030709     C                   EVAL      VHFCMO = TAACAP
018300030709     C                   EVAL      VHFNMO = TAANAZ
018400030709     C                   ENDIF
018500030709     C                   ENDIF
018600030325     C*
018700100209     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+sepFld+
018800070316     C                                       %trim(%editc(VAFLNP:'Q'))+sepFld+
018900070316     C                                       %trim(%editc(VAFNRS:'Q'))+sepFld+
019000070316     C                                       %trim(%editc(VAFNSP:'Q'))+sepFld+
019100070316     C                                          '"'+%trim(VAFTBL)+'"'+sepFld+
019200070316     C                                       %trim(%editc(VAFMGS:'Q'))+sepFld+
019300070316     C                                       %trim(%editc(VAFKSC:'Q'))+sepFld+
019400070316     C                                       %trim(%editc(VAFLNA:'Q'))+sepFld+
019500070316     C                                       %trim(%editc(VAFNCL:'Q'))+sepFld+
019600070316     C                                       %trim(%editc(VAFPKB:'Q'))+sepFld+
019700070316     C                                       %trim(%editc(VAFVLF:'Q'))+sepFld+
019800070316     C                                       %trim(%editc(VAFQFT:'Q'))+sepFld+
019900070316     C                                       %trim(%editc(VAFPOR:'Q'))+sepFld+
020000070316     C                                          '"'+%trim(VAFSV1)+'"'+sepFld+
020100070316     C                                       %trim(%editc(VAFVA1:'Q'))+sepFld+
020200070316     C                                          '"'+%trim(VAFSV2)+'"'+sepFld+
020300070316     C                                       %trim(%editc(VAFVA2:'Q'))+sepFld+
020400070316     C                                          '"'+%trim(VAFSV3)+'"'+sepFld+
020500070316     C                                       %trim(%editc(VAFVA3:'Q'))+sepFld+
020600070316     C                                          '"'+%trim(VAFSV4)+'"'+sepFld+
020700070316     C                                       %trim(%editc(VAFVA4:'Q'))+sepFld+
020800070316     C                                          '"'+%trim(VAFSV5)+'"'+sepFld+
020900070316     C                                       %trim(%editc(VAFVA5:'Q'))+sepFld+
021000070316     C                                          '"'+%trim(VAFSV6)+'"'+sepFld+
021100070316     C                                       %trim(%editc(VAFVA6:'Q'))+sepFld+
021200070316     C                                          '"'+%trim(VAFSV7)+'"'+sepFld+
021300070316     C                                       %trim(%editc(VAFVA7:'Q'))+sepFld+
021400070316     C                                          '"'+%trim(VAFSV8)+'"'+sepFld+
021500070316     C                                       %trim(%editc(VAFVA8:'Q'))+sepFld+
021600070316     C                                       %trim(%editc(VAFVAX:'Q'))+sepFld+
021700070316     C                                       %trim(%editc(VAFIMV:'Q'))+sepFld+
021800070316     C                                       %trim(%editc(VAFDFT:'Q'))+sepFld+
021900070316     C                                       %trim(%editc(VAFNFT:'Q'))+sepFld+
022000070316     C                                       %trim(%editc(VAFFIV:'Q'))+sepFld+
022100070316     C                                          '"'+%trim(VAFDIV)+'"'+sepFld+
022200070316     C                                       %trim(%editc(VAFDRT:'Q'))+sepFld+
022300070316     C                                       %trim(%editc(VAFRMN:'Q'))+sepFld+
022400070316     C                                          '"'+%trim(VAFRMA)+'"'+sepFld+
022500070316     C                                          '"'+%trim(VAFRMX)+'"'+sepFld+
022600070316     C                                          '"'+%trim(VAFFTC)+'"'+sepFld+
022700070316     C                                          '"'+%trim(VAFTC2)+'"'+sepFld+
022800070316     C                                          '"'+%trim(VAFTSP)+'"'+sepFld+
022900070316     C                                          '"'+%trim(VAFFAP)+'"'+sepFld+
023000070316     C                                          '"'+%trim(VAFFIN)+'"'+sepFld+
023100070316     C                                       %trim(%editc(VAFCTR:'Q'))+sepFld+
023200070316     C                                          '"'+%trim(VAFNAS)+'"'+sepFld+
023300070316     C                                          '"'+%trim(VAFCTS)+'"'+sepFld+
023400070316     C                                          '"'+%trim(VAFCAD)+'"'+sepFld+
023500070316     C                                          '"'+%trim(VAFLOD)+'"'+sepFld+
023600070316     C                                          '"'+%trim(VAFPRD)+'"'+sepFld+
023700070316     C                                          '"'+%trim(VAFNZD)+'"'+sepFld+
023800070316     C                                          '"'+%trim(VAFVAS)+'"'+sepFld+
023900070316     C                                       %trim(%editc(VAFIAS:'Q'))+sepFld+
024000070316     C                                          '"'+%trim(VAFFPC)+'"'+sepFld+
024100070316     C                                       %trim(%editc(VAFPKC:'Q'))+sepFld+
024200070316     C                                          '"'+%trim(VAFFVC)+'"'+sepFld+
024300070316     C                                       %trim(%editc(VAFVLC:'Q'))+sepFld+
024400070316     C                                          '"'+%trim(VHFRSD)+'"'+sepFld+
024500070316     C                                          '"'+%trim(VHFIND)+'"'+sepFld+
024600070316     C                                          '"'+%trim(VHFRSM)+'"'+sepFld+
024700070316     C                                          '"'+%trim(VHFINM)+'"'+sepFld+
024800070316     C                                          '"'+%trim(VHFCAM)+'"'+sepFld+
024900070316     C                                          '"'+%trim(VHFLOM)+'"'+sepFld+
025000070316     C                                          '"'+%trim(VHFPRM)+'"'+sepFld+
025100070316     C                                          '"'+%trim(VHFNZM)+'"'+sepFld+
025200070316     C                                          '"'+%trim(VHFRMO)+'"'+sepFld+
025300070316     C                                          '"'+%trim(VHFCMO)+'"'+sepFld+
025400070316     C                                          '"'+%trim(VHFNMO)+'"'+sepFld+
025500100915     C                                       %trim(%editc(wPVL:'Q')) +sepFld+
025501100915     C                                       %trim(%editc(TASNCL:'Q'))
025600930409     C*
025700100209     C                   WRITE     TIVAW000
025800090325     C                   delete    tivgd000
025900991027     C*
026000090325     C                   READ      tivgdtmp
026100030325     C                   ENDDO
026200991027     C*
026300030325     C                   EVAL      wrkesito = '0'
026400991027     C*
026500910830     C                   ENDSR
026600991027
026700991027      /TITLE Operazioni iniziali.
026800991027     C     *inzsr        BEGSR
026900991027     C*
027000991027     C     *ENTRY        PLIST
027100991027     C                   parm                    prmppt
027200991027     C     wrkesito      parm      wrkesito      prmesito
027300070316     C*
027400070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
027500070316     C                   eval      sepFld = %subst(prmppt:2:1)
027600070316     C                   if        sepFld = *blanks
027700070316     C                   eval      sepFld = ','
027800070316     C                   endif
027900030709     C*
028000030709     C* Definizione chiavi
028100030709     C*
028200030709     C* Chiave su TITAS30C - Completa
028300030709     C     KEYtas30C     KLIST
028400030709     C                   KFLD                    vafAAS
028500030709     C                   KFLD                    vafLNP
028600030709     C                   KFLD                    vafNRS
028700030709     C                   KFLD                    vafNSP
028800030709     C                   KFLD                    vafTBL
028900030709     C*
029000030709     C* Chiave su TITAA30C - Completa
029100030709     C     KEYtaa30C     KLIST
029200030709     C                   KFLD                    vafAAS
029300030709     C                   KFLD                    vafLNP
029400030709     C                   KFLD                    vafNRS
029500030709     C                   KFLD                    vafNSP
029600030709     C                   KFLD                    taaTRC
029700991027     C*
029800991027     C                   ENDSR
