000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030709     FTITAS30C  IF   E           K DISK
000500030709     FTITAA30C  IF   E           K DISK
000600090423     FTITA430C  IF   E           K DISK
000700090423     FFNORM01L  IF   E           K DISK
000800090325     Ftivgdtmp  uF   E             DISK
000900090423     FTIVAWWWT  UF A E             DISK    commit
001000030709     D*------------------
001100030709     D* DS REPERIMENTO DATI CLIENTE
001200030709     D*-------------------
001300030709     D BS69DS        E DS                  EXTNAME(TIBS69DS)
001400030709     D ACODS         E DS                  EXTNAME(CNACO00F)
001500030709     D INDDS         E DS                  EXTNAME(CNIND00F)
001600030709     D CLPDS         E DS                  EXTNAME(CNCLP00F)
001700030709     D CLSDS         E DS                  EXTNAME(FNCLS00F)
001800991027
001900991027     D prmppt          s             50
002000991027     D prmesito        s              1
002100991027     D wrkesito        s                   like(prmesito)
002200090325     D tivafds       e ds                  extname(tivaf00t)
002300090325     D dsvafa        e ds                  extname(tivafawt)
002400070316     D sepFld          s              1
002500090423     D dta4m         e ds
002600071205
002700071205     D wPKB            s              8
002800071205     D wVLF            s              6
002900071205     D wQFT            s             14
003000071205     D wPOR            s             12
003100071205     D wVA1            s             12
003200071205     D wVA2            s             12
003300071205     D wVA3            s             12
003400071205     D wVA4            s             12
003500071205     D wVA5            s             12
003600071205     D wVA6            s             12
003700071205     D wVA7            s             12
003800071205     D wVA8            s             12
003900071205     D wVAX            s             12
004000071205     D wIMV            s             14
004100071205     D wIAS            s             14
004200071205     D wPKC            s              8
004300071205     D wVLC            s              6
004400991027
004500030908     C                   EXSR      chkRECFILMBR
004600991027     C                   EXSR      traduci
004700921023     C                   SETON                                        LR
004800030908
004900030908     C     chkRECFILMBR  BEGSR
005000030908     C*
005100090423     C                   READ      TIVAWWWT                               55
005200030908     C*
005300030908     C                   ENDSR
005400991027
005500090423
005600090423
005700991027     C     traduci       BEGSR
005800030325     C*
005900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
006000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
006100090423     C                   EVAL      VAWDTA  = '"VAFAAS"'+sepFld+
006200070316     C                                       '"VAFLNP"'+sepFld+
006300070316     C                                       '"VAFNRS"'+sepFld+
006400070316     C                                       '"VAFNSP"'+sepFld+
006500070316     C                                       '"VAFTBL"'+sepFld+
006600070316     C                                       '"VAFMGS"'+sepFld+
006700070316     C                                       '"VAFKSC"'+sepFld+
006800070316     C                                       '"VAFLNA"'+sepFld+
006900070316     C                                       '"VAFNCL"'+sepFld+
007000070316     C                                       '"VAFPKB"'+sepFld+
007100070316     C                                       '"VAFVLF"'+sepFld+
007200070316     C                                       '"VAFQFT"'+sepFld+
007300070316     C                                       '"VAFPOR"'+sepFld+
007400070316     C                                       '"VAFSV1"'+sepFld+
007500070316     C                                       '"VAFVA1"'+sepFld+
007600070316     C                                       '"VAFSV2"'+sepFld+
007700070316     C                                       '"VAFVA2"'+sepFld+
007800070316     C                                       '"VAFSV3"'+sepFld+
007900070316     C                                       '"VAFVA3"'+sepFld+
008000070316     C                                       '"VAFSV4"'+sepFld+
008100070316     C                                       '"VAFVA4"'+sepFld+
008200070316     C                                       '"VAFSV5"'+sepFld+
008300070316     C                                       '"VAFVA5"'+sepFld+
008400070316     C                                       '"VAFSV6"'+sepFld+
008500070316     C                                       '"VAFVA6"'+sepFld+
008600070316     C                                       '"VAFSV7"'+sepFld+
008700070316     C                                       '"VAFVA7"'+sepFld+
008800070316     C                                       '"VAFSV8"'+sepFld+
008900070316     C                                       '"VAFVA8"'+sepFld+
009000070316     C                                       '"VAFVAX"'+sepFld+
009100070316     C                                       '"VAFIMV"'+sepFld+
009200070316     C                                       '"VAFDFT"'+sepFld+
009300070316     C                                       '"VAFNFT"'+sepFld+
009400070316     C                                       '"VAFFIV"'+sepFld+
009500070316     C                                       '"VAFDIV"'+sepFld+
009600070316     C                                       '"VAFDRT"'+sepFld+
009700070316     C                                       '"VAFRMN"'+sepFld+
009800070316     C                                       '"VAFRMA"'+sepFld+
009900070316     C                                       '"VAFRMX"'+sepFld+
010000070316     C                                       '"VAFFTC"'+sepFld+
010100070316     C                                       '"VAFTC2"'+sepFld+
010200070316     C                                       '"VAFTSP"'+sepFld+
010300070316     C                                       '"VAFFAP"'+sepFld+
010400070316     C                                       '"VAFFIN"'+sepFld+
010500070316     C                                       '"VAFCTR"'+sepFld+
010600070316     C                                       '"VAFNAS"'+sepFld+
010700070316     C                                       '"VAFCTS"'+sepFld+
010800070316     C                                       '"VAFCAD"'+sepFld+
010900070316     C                                       '"VAFLOD"'+sepFld+
011000070316     C                                       '"VAFPRD"'+sepFld+
011100070316     C                                       '"VAFNZD"'+sepFld+
011200070316     C                                       '"VAFVAS"'+sepFld+
011300070316     C                                       '"VAFIAS"'+sepFld+
011400070316     C                                       '"VAFFPC"'+sepFld+
011500070316     C                                       '"VAFPKC"'+sepFld+
011600070316     C                                       '"VAFFVC"'+sepFld+
011700070316     C                                       '"VAFVLC"'+sepFld+
011800070316     C                                       '"VAFRSD"'+sepFld+
011900070316     C                                       '"VAFIND"'+sepFld+
012000070316     C                                       '"VAFRSM"'+sepFld+
012100070316     C                                       '"VAFINM"'+sepFld+
012200070316     C                                       '"VAFCAM"'+sepFld+
012300070316     C                                       '"VAFLOM"'+sepFld+
012400070316     C                                       '"VAFPRM"'+sepFld+
012500070316     C                                       '"VAFNZM"'+sepFld+
012600070316     C                                       '"VAFRMO"'+sepFld+
012700070316     C                                       '"VAFCMO"'+sepFld+
012800090423     C                                       '"VAFNMO"'+sepFld+
012900090423     C                                       '"TASDCM"'+sepFld+
013000090423     C                                       '"TASCCA"'+sepFld+
013100090423     C                                       '"TASFGC"'
013200030325     C*
013300090423     C                   WRITE     TIVAW000
013400030325     C                   ENDIF
013500991027     C*
013600090325     C                   READ      tivgdtmp
013700090325     C                   DOW       not %eof
013800090325     c                   movel     vgddta        tivafds
013900041220     C*
014000041220     C* Inizializzo i campi "estensione"
014100041220     C                   EVAL      VHFRSD = *blanks
014200041220     C                   EVAL      VHFIND = *blanks
014300041220     C                   EVAL      VHFRSM = *blanks
014400041220     C                   EVAL      VHFINM = *blanks
014500041220     C                   EVAL      VHFCAM = *blanks
014600041220     C                   EVAL      VHFLOM = *blanks
014700041220     C                   EVAL      VHFPRM = *blanks
014800041220     C                   EVAL      VHFNZM = *blanks
014900041220     C                   EVAL      VHFRMO = *blanks
015000041220     C                   EVAL      VHFCMO = *blanks
015100041220     C                   EVAL      VHFNMO = *blanks
015200030709     C*
015300030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
015400030709     C     KEYtas30C     CHAIN     titas30c
015500030709     C                   IF        %found(titas30c)
015600030709     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
015700030709     C                   EVAL      VHFRSD = TASRSD
015800030709     C                   EVAL      VHFIND = TASIND
015900030709     C* X reperire i dati del mittente verifico il codice del cliente mittente:
016000030709     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
016100030709     C                   MOVE(P)   tasCCM        wCLI              4 0
016200030709     C                   IF        wCLI =    0 OR
016300030709     C                             wCLI = 8888 OR
016400030709     C                             wCLI = 9999
016500030709     C                   EVAL      taaTRC = 'M'
016600030709     C     KEYtaa30C     CHAIN     titaa30c
016700030709     C                   IF        %found(titaa30c)
016800030709     C                   EVAL      VHFRSM = TAARSC
016900030709     C                   EVAL      VHFINM = TAAIND
017000030709     C                   EVAL      VHFCAM = TAACAP
017100030709     C                   EVAL      VHFLOM = TAALOC
017200030709     C                   EVAL      VHFPRM = TAAPRV
017300030709     C                   EVAL      VHFNZM = TAANAZ
017400030709     C                   ENDIF
017500030709     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
017600030709     C                   ELSE
017700030709     C                   CLEAR                   BS69DS
017800030709     C                   CLEAR                   ACODS
017900030709     C                   CLEAR                   INDDS
018000030709     C                   CLEAR                   CLPDS
018100030709     C                   CLEAR                   CLSDS
018200030709     C                   Z-ADD     tasCCM        I69KAC
018300030709     C                   Z-ADD     tasCCM        I69KIN
018400030709     C                   CALL      'TIBS69R'
018500030709     C                   PARM                    BS69DS
018600030709     C                   PARM                    ACODS
018700030709     C                   PARM                    INDDS
018800030709     C                   PARM                    CLPDS
018900030709     C                   PARM                    CLSDS
019000030709     C     O69ERR        IFNE      '1'
019100030709     C                   EVAL      VHFRSM = ACORAG
019200030709     C                   EVAL      VHFINM = INDVIA
019300030709     C                   MOVEL(P)  INDCAP        VHFCAM
019400030709     C                   EVAL      VHFLOM = INDCIT
019500030709     C                   EVAL      VHFPRM = INDPRV
019600030709     C                   EVAL      VHFNZM = INDSTA
019700030709     C                   ENDIF
019800030709     C*
019900030709     C                   ENDIF
020000090423     C*
020100030709     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
020200030709     C                   EVAL      taaTRC = 'O'
020300030709     C     KEYtaa30C     CHAIN     titaa30c
020400030709     C                   IF        %found(titaa30c)
020500030709     C                   EVAL      VHFRMO = TAARSC
020600030709     C                   EVAL      VHFCMO = TAACAP
020700030709     C                   EVAL      VHFNMO = TAANAZ
020800030709     C                   ENDIF
020900090423     C*
021000090423     C* X reperire il codice ordinante dell'orm legato alla bolla
021100090423     C* aggancio il TITA4 con tipo record = 'M':
021200090423     C                   EVAL      ta4TRC = 'M'
021300090423     C     KEYta430C     CHAIN     tita430c
021400090423     C                   IF        %found(tita430c)
021500090423     C                   EVAL      DTA4M = TA4NOT
021600090423     C*
021700090423     C* Quindi aggancio l'orm legato
021800090423     C     KEYorm01C     CHAIN     fnorm01l
021900090423     C                   IF        %found(fnorm01l)
022000090424     C                   MOVEL(P)  ormCOR        wCOR              7 0
022100090424     C                   MOVE(P)   wCOR          vafRMX
022200090423     C                   ENDIF
022300090423     C                   ENDIF
022400090423     C*
022500101014     C* Eseguo forzature rispetto a pesi/volumi rilevati e raltivi flag tipi
022600090423     C*
022700090423     C* - volume rilevato VLC
022800090423     C                   IF        tasVLC > *zeros
022900090423     C                   EVAL      vafVLC = tasVLC
023000090423     C                   IF        tasNCL = tasNCR
023100090423     C                   EVAL      vafFVC = 'T'
023200090423     C                   ELSE
023300090423     C                   EVAL      vafFVC = 'Z'
023400101022     C***                IF        vafVLC < vafVLF
023500101022     C                   IF        vafVLC < tasVLB
023600090424     C                   EVAL      vafVLC = *zeros
023700090424     C                   ENDIF
023800090423     C                   ENDIF
023900090423     C                   ENDIF
024000101014     C                   IF        vafVLF > vafVLC and
024100101014     C                             vafVLF > tasVLB
024200101014     C                   EVAL      vafVLC = vafVLF
024300101014     C                   ENDIF
024400090423     C*
024500090423     C* - peso rilevato PKC
024600090424     C                   IF        tasPKC > *zeros
024700090423     C                   EVAL      vafPKC = tasPKC
024800090423     C                   IF        tasNCL = tasNCR
024900090423     C                   EVAL      vafFPC = 'T'
025000090423     C                   ELSE
025100090423     C                   EVAL      vafFPC = 'Z'
025200101022     C***                IF        vafPKC < vafPKB
025300101022     C                   IF        vafPKC < tasPKB
025400090424     C                   EVAL      vafPKC = *zeros
025500090424     C                   ENDIF
025600090423     C                   ENDIF
025700090423     C                   ENDIF
025800101014     C                   IF        vafPKB > vafPKC AND
025900101014     C                             vafPKB > tasPKB
026000101014     C                   EVAL      vafPKC = vafPKB
026100101014     C                   ENDIF
026200140915     C*
026300071205     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
026400071205     C  N60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 , '))
026500071205     C  N60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 ,   '))
026600071205     C  N60              EVAL      wQFT = %trim(%editw(VAFQFT:'        0 ,   '))
026700071205     C  N60              EVAL      wPOR = %trim(%editw(VAFPOR:'      0 ,   '))
026800071205     C  N60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 ,   '))
026900071205     C  N60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 ,   '))
027000071205     C  N60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 ,   '))
027100071205     C  N60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 ,   '))
027200071205     C  N60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 ,   '))
027300071205     C  N60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 ,   '))
027400071205     C  N60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 ,   '))
027500071205     C  N60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 ,   '))
027600071205     C  N60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 ,   '))
027700071205     C  N60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 ,   '))
027800071205     C  N60              EVAL      wIAS = %trim(%editw(VAFIAS:'        0 ,   '))
027900071205     C  N60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 , '))
028000071205     C  N60              EVAL      wVLC = %trim(%editw(VAFVLC:'0 ,   '))
028100071205     C   60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 . '))
028200071205     C   60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 .   '))
028300071205     C   60              EVAL      wQFT = %trim(%editw(VAFQFT:'        0 .   '))
028400071205     C   60              EVAL      wPOR = %trim(%editw(VAFPOR:'      0 .   '))
028500071205     C   60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 .   '))
028600071205     C   60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 .   '))
028700071205     C   60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 .   '))
028800071205     C   60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 .   '))
028900071205     C   60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 .   '))
029000071205     C   60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 .   '))
029100071205     C   60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 .   '))
029200071205     C   60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 .   '))
029300071205     C   60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 .   '))
029400071205     C   60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 .   '))
029500071205     C   60              EVAL      wIAS = %trim(%editw(VAFIAS:'        0 .   '))
029600071205     C   60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 . '))
029700071205     C   60              EVAL      wVLC = %trim(%editw(VAFVLC:'0 .   '))
029800030325     C*
029900090423     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+sepFld+
030000070316     C                                       %trim(%editc(VAFLNP:'Q'))+sepFld+
030100070316     C                                       %trim(%editc(VAFNRS:'Q'))+sepFld+
030200070316     C                                       %trim(%editc(VAFNSP:'Q'))+sepFld+
030300070316     C                                          '"'+%trim(VAFTBL)+'"'+sepFld+
030400070316     C                                       %trim(%editc(VAFMGS:'Q'))+sepFld+
030500070316     C                                       %trim(%editc(VAFKSC:'Q'))+sepFld+
030600070316     C                                       %trim(%editc(VAFLNA:'Q'))+sepFld+
030700070316     C                                       %trim(%editc(VAFNCL:'Q'))+sepFld+
030800071205     C                                                %trim(wPKB)+sepFld+
030900071205     C                                                %trim(wVLF)+sepFld+
031000071205     C                                                %trim(wQFT)+sepFld+
031100071205     C                                                %trim(wPOR)+sepFld+
031200070316     C                                          '"'+%trim(VAFSV1)+'"'+sepFld+
031300071205     C                                                %trim(wVA1)+sepFld+
031400070316     C                                          '"'+%trim(VAFSV2)+'"'+sepFld+
031500071205     C                                                %trim(wVA2)+sepFld+
031600070316     C                                          '"'+%trim(VAFSV3)+'"'+sepFld+
031700071205     C                                                %trim(wVA3)+sepFld+
031800070316     C                                          '"'+%trim(VAFSV4)+'"'+sepFld+
031900071205     C                                                %trim(wVA4)+sepFld+
032000070316     C                                          '"'+%trim(VAFSV5)+'"'+sepFld+
032100071205     C                                                %trim(wVA5)+sepFld+
032200070316     C                                          '"'+%trim(VAFSV6)+'"'+sepFld+
032300071205     C                                                %trim(wVA6)+sepFld+
032400070316     C                                          '"'+%trim(VAFSV7)+'"'+sepFld+
032500071205     C                                                %trim(wVA7)+sepFld+
032600070316     C                                          '"'+%trim(VAFSV8)+'"'+sepFld+
032700071205     C                                                %trim(wVA8)+sepFld+
032800071205     C                                                %trim(wVAX)+sepFld+
032900071205     C                                                %trim(wIMV)+sepFld+
033000070316     C                                       %trim(%editc(VAFDFT:'Q'))+sepFld+
033100070316     C                                       %trim(%editc(VAFNFT:'Q'))+sepFld+
033200070316     C                                       %trim(%editc(VAFFIV:'Q'))+sepFld+
033300070316     C                                          '"'+%trim(VAFDIV)+'"'+sepFld+
033400070316     C                                       %trim(%editc(VAFDRT:'Q'))+sepFld+
033500070316     C                                       %trim(%editc(VAFRMN:'Q'))+sepFld+
033600070316     C                                          '"'+%trim(VAFRMA)+'"'+sepFld+
033700070316     C                                          '"'+%trim(VAFRMX)+'"'+sepFld+
033800070316     C                                          '"'+%trim(VAFFTC)+'"'+sepFld+
033900070316     C                                          '"'+%trim(VAFTC2)+'"'+sepFld+
034000070316     C                                          '"'+%trim(VAFTSP)+'"'+sepFld+
034100070316     C                                          '"'+%trim(VAFFAP)+'"'+sepFld+
034200070316     C                                          '"'+%trim(VAFFIN)+'"'+sepFld+
034300070316     C                                       %trim(%editc(VAFCTR:'Q'))+sepFld+
034400070316     C                                          '"'+%trim(VAFNAS)+'"'+sepFld+
034500070316     C                                          '"'+%trim(VAFCTS)+'"'+sepFld+
034600070316     C                                          '"'+%trim(VAFCAD)+'"'+sepFld+
034700070316     C                                          '"'+%trim(VAFLOD)+'"'+sepFld+
034800070316     C                                          '"'+%trim(VAFPRD)+'"'+sepFld+
034900070316     C                                          '"'+%trim(VAFNZD)+'"'+sepFld+
035000070316     C                                          '"'+%trim(VAFVAS)+'"'+sepFld+
035100071205     C                                                %trim(wIAS)+sepFld+
035200070316     C                                          '"'+%trim(VAFFPC)+'"'+sepFld+
035300071205     C                                                %trim(wPKC)+sepFld+
035400070316     C                                          '"'+%trim(VAFFVC)+'"'+sepFld+
035500071205     C                                                %trim(wVLC)+sepFld+
035600070316     C                                          '"'+%trim(VHFRSD)+'"'+sepFld+
035700070316     C                                          '"'+%trim(VHFIND)+'"'+sepFld+
035800070316     C                                          '"'+%trim(VHFRSM)+'"'+sepFld+
035900070316     C                                          '"'+%trim(VHFINM)+'"'+sepFld+
036000070316     C                                          '"'+%trim(VHFCAM)+'"'+sepFld+
036100070316     C                                          '"'+%trim(VHFLOM)+'"'+sepFld+
036200070316     C                                          '"'+%trim(VHFPRM)+'"'+sepFld+
036300070316     C                                          '"'+%trim(VHFNZM)+'"'+sepFld+
036400070316     C                                          '"'+%trim(VHFRMO)+'"'+sepFld+
036500070316     C                                          '"'+%trim(VHFCMO)+'"'+sepFld+
036600090423     C                                          '"'+%trim(VHFNMO)+'"'+sepFld+
036700090423     C                                       %trim(%editc(TASDCM:'Q'))+sepFld+
036800090423     C                                          '"'+%trim(TASCCA)+'"'+sepFld+
036900090423     C                                          '"'+%trim(TASFGC)+'"'
037000930409     C*
037100090423     C                   WRITE     TIVAW000
037200090423     C*
037300090423     C                   ENDIF
037400090423     C*
037500090325     C                   delete    tivgd000
037600991027     C*
037700090325     C                   READ      tivgdtmp
037800030325     C                   ENDDO
037900991027     C*
038000030325     C                   EVAL      wrkesito = '0'
038100991027     C*
038200910830     C                   ENDSR
038300090423
038400090423
038500991027
038600991027      /TITLE Operazioni iniziali.
038700991027     C     *inzsr        BEGSR
038800991027     C*
038900991027     C     *ENTRY        PLIST
039000991027     C                   parm                    prmppt
039100991027     C     wrkesito      parm      wrkesito      prmesito
039200070316     C*
039300070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
039400070316     C                   eval      sepFld = %subst(prmppt:2:1)
039500070316     C                   if        sepFld = *blanks
039600070316     C                   eval      sepFld = ','
039700070316     C                   endif
039800071205     C*
039900071205     C* Verifico se richiesto un particolare carattere d separatore decimale
040000071205     C                   SETON                                        60
040100071205     C                   IF        %subst(prmppt:3:1) = ','
040200071205     C                   SETOFF                                       60
040300071205     C                   ENDIF
040400030709     C*
040500030709     C* Definizione chiavi
040600030709     C*
040700030709     C* Chiave su TITAS30C - Completa
040800030709     C     KEYtas30C     KLIST
040900030709     C                   KFLD                    vafAAS
041000030709     C                   KFLD                    vafLNP
041100030709     C                   KFLD                    vafNRS
041200030709     C                   KFLD                    vafNSP
041300030709     C                   KFLD                    vafTBL
041400030709     C*
041500030709     C* Chiave su TITAA30C - Completa
041600030709     C     KEYtaa30C     KLIST
041700030709     C                   KFLD                    vafAAS
041800030709     C                   KFLD                    vafLNP
041900030709     C                   KFLD                    vafNRS
042000030709     C                   KFLD                    vafNSP
042100030709     C                   KFLD                    taaTRC
042200090423     C*
042300090423     C* Chiave su TITA430C - Completa
042400090423     C     KEYta430C     KLIST
042500090423     C                   KFLD                    vafAAS
042600090423     C                   KFLD                    vafLNP
042700090423     C                   KFLD                    vafNRS
042800090423     C                   KFLD                    vafNSP
042900090423     C                   KFLD                    ta4TRC
043000090423     C*
043100090423     C* Chiave su FNORM01L - Completa
043200090423     C     KEYorm01C     KLIST
043300090423     C                   KFLD                    §TA4MPOE
043400090423     C                   KFLD                    §TA4MNSR
043500090423     C                   KFLD                    §TA4MNOR
043600090423     C                   KFLD                    §TA4MNRV
043700991027     C*
043800991027     C                   ENDSR
