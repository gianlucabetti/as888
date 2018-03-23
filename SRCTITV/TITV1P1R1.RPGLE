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
006500150722     C                                       '"VAFRMN"'+sepFld+
006600150722     C                                       '"VAFRMA"'+sepFld+
006700150722     C                                       '"VAFRMX"'+sepFld+
006800070316     C                                       '"VAFNCL"'+sepFld+
006900070316     C                                       '"VAFPKB"'+sepFld+
007000070316     C                                       '"VAFVLF"'+sepFld+
007100070316     C                                       '"VAFFPC"'+sepFld+
007200070316     C                                       '"VAFPKC"'+sepFld+
007300070316     C                                       '"VAFFVC"'+sepFld+
007400150722     C                                       '"VAFVLC"'+sepFld
007500030325     C*
007600090423     C                   WRITE     TIVAW000
007700030325     C                   ENDIF
007800991027     C*
007900090325     C                   READ      tivgdtmp
008000090325     C                   DOW       not %eof
008100090325     c                   movel     vgddta        tivafds
008200041220     C*
008300041220     C* Inizializzo i campi "estensione"
008400041220     C                   EVAL      VHFRSD = *blanks
008500041220     C                   EVAL      VHFIND = *blanks
008600041220     C                   EVAL      VHFRSM = *blanks
008700041220     C                   EVAL      VHFINM = *blanks
008800041220     C                   EVAL      VHFCAM = *blanks
008900041220     C                   EVAL      VHFLOM = *blanks
009000041220     C                   EVAL      VHFPRM = *blanks
009100041220     C                   EVAL      VHFNZM = *blanks
009200041220     C                   EVAL      VHFRMO = *blanks
009300041220     C                   EVAL      VHFCMO = *blanks
009400041220     C                   EVAL      VHFNMO = *blanks
009500030709     C*
009600030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
009700030709     C     KEYtas30C     CHAIN     titas30c
009800030709     C                   IF        %found(titas30c)
009900030709     C* Se trovo la bolla valorizzo i dati del destinatario nn già presenti sul TIVAF
010000030709     C                   EVAL      VHFRSD = TASRSD
010100030709     C                   EVAL      VHFIND = TASIND
010200030709     C* X reperire i dati del mittente verifico il codice del cliente mittente:
010300030709     C* - se = 0 oppure 8888/9999 => aggancio il TITAA30C con tipo record = 'M'
010400030709     C                   MOVE(P)   tasCCM        wCLI              4 0
010500030709     C                   IF        wCLI =    0 OR
010600030709     C                             wCLI = 8888 OR
010700030709     C                             wCLI = 9999
010800030709     C                   EVAL      taaTRC = 'M'
010900030709     C     KEYtaa30C     CHAIN     titaa30c
011000030709     C                   IF        %found(titaa30c)
011100030709     C                   EVAL      VHFRSM = TAARSC
011200030709     C                   EVAL      VHFINM = TAAIND
011300030709     C                   EVAL      VHFCAM = TAACAP
011400030709     C                   EVAL      VHFLOM = TAALOC
011500030709     C                   EVAL      VHFPRM = TAAPRV
011600030709     C                   EVAL      VHFNZM = TAANAZ
011700030709     C                   ENDIF
011800030709     C* - altrimenti (trattasi d cliente codificato) tramite il TIBS69
011900030709     C                   ELSE
012000030709     C                   CLEAR                   BS69DS
012100030709     C                   CLEAR                   ACODS
012200030709     C                   CLEAR                   INDDS
012300030709     C                   CLEAR                   CLPDS
012400030709     C                   CLEAR                   CLSDS
012500030709     C                   Z-ADD     tasCCM        I69KAC
012600030709     C                   Z-ADD     tasCCM        I69KIN
012700030709     C                   CALL      'TIBS69R'
012800030709     C                   PARM                    BS69DS
012900030709     C                   PARM                    ACODS
013000030709     C                   PARM                    INDDS
013100030709     C                   PARM                    CLPDS
013200030709     C                   PARM                    CLSDS
013300030709     C     O69ERR        IFNE      '1'
013400030709     C                   EVAL      VHFRSM = ACORAG
013500030709     C                   EVAL      VHFINM = INDVIA
013600030709     C                   MOVEL(P)  INDCAP        VHFCAM
013700030709     C                   EVAL      VHFLOM = INDCIT
013800030709     C                   EVAL      VHFPRM = INDPRV
013900030709     C                   EVAL      VHFNZM = INDSTA
014000030709     C                   ENDIF
014100030709     C*
014200030709     C                   ENDIF
014300090423     C*
014400030709     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O':
014500030709     C                   EVAL      taaTRC = 'O'
014600030709     C     KEYtaa30C     CHAIN     titaa30c
014700030709     C                   IF        %found(titaa30c)
014800030709     C                   EVAL      VHFRMO = TAARSC
014900030709     C                   EVAL      VHFCMO = TAACAP
015000030709     C                   EVAL      VHFNMO = TAANAZ
015100030709     C                   ENDIF
015200090423     C*
015300090423     C* X reperire il codice ordinante dell'orm legato alla bolla
015400090423     C* aggancio il TITA4 con tipo record = 'M':
015500090423     C                   EVAL      ta4TRC = 'M'
015600090423     C     KEYta430C     CHAIN     tita430c
015700090423     C                   IF        %found(tita430c)
015800090423     C                   EVAL      DTA4M = TA4NOT
015900090423     C*
016000090423     C* Quindi aggancio l'orm legato
016100090423     C     KEYorm01C     CHAIN     fnorm01l
016200090423     C                   IF        %found(fnorm01l)
016300090424     C                   MOVEL(P)  ormCOR        wCOR              7 0
016400090424     C                   MOVE(P)   wCOR          vafRMX
016500090423     C                   ENDIF
016600090423     C                   ENDIF
016700090423     C*
016800101014     C* Eseguo forzature rispetto a pesi/volumi rilevati e raltivi flag tipi
016900090423     C*
017000090423     C* - volume rilevato VLC
017100090423     C                   IF        tasVLC > *zeros
017200090423     C                   EVAL      vafVLC = tasVLC
017300090423     C                   IF        tasNCL = tasNCR
017400090423     C                   EVAL      vafFVC = 'T'
017500090423     C                   ELSE
017600090423     C                   EVAL      vafFVC = 'Z'
017700101022     C***                IF        vafVLC < vafVLF
017800101022     C                   IF        vafVLC < tasVLB
017900090424     C                   EVAL      vafVLC = *zeros
018000090424     C                   ENDIF
018100090423     C                   ENDIF
018200090423     C                   ENDIF
018300101014     C                   IF        vafVLF > vafVLC and
018400101014     C                             vafVLF > tasVLB
018500101014     C                   EVAL      vafVLC = vafVLF
018600101014     C                   ENDIF
018700090423     C*
018800090423     C* - peso rilevato PKC
018900090424     C                   IF        tasPKC > *zeros
019000090423     C                   EVAL      vafPKC = tasPKC
019100090423     C                   IF        tasNCL = tasNCR
019200090423     C                   EVAL      vafFPC = 'T'
019300090423     C                   ELSE
019400090423     C                   EVAL      vafFPC = 'Z'
019500101022     C***                IF        vafPKC < vafPKB
019600101022     C                   IF        vafPKC < tasPKB
019700090424     C                   EVAL      vafPKC = *zeros
019800090424     C                   ENDIF
019900090423     C                   ENDIF
020000090423     C                   ENDIF
020100101014     C                   IF        vafPKB > vafPKC AND
020200101014     C                             vafPKB > tasPKB
020300101014     C                   EVAL      vafPKC = vafPKB
020400101014     C                   ENDIF
020500140915     C*
020600071205     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
020700071205     C  N60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 , '))
020800071205     C  N60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 ,   '))
020900071205     C  N60              EVAL      wQFT = %trim(%editw(VAFQFT:'        0 ,   '))
021000071205     C  N60              EVAL      wPOR = %trim(%editw(VAFPOR:'      0 ,   '))
021100071205     C  N60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 ,   '))
021200071205     C  N60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 ,   '))
021300071205     C  N60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 ,   '))
021400071205     C  N60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 ,   '))
021500071205     C  N60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 ,   '))
021600071205     C  N60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 ,   '))
021700071205     C  N60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 ,   '))
021800071205     C  N60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 ,   '))
021900071205     C  N60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 ,   '))
022000071205     C  N60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 ,   '))
022100071205     C  N60              EVAL      wIAS = %trim(%editw(VAFIAS:'        0 ,   '))
022200071205     C  N60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 , '))
022300071205     C  N60              EVAL      wVLC = %trim(%editw(VAFVLC:'0 ,   '))
022400071205     C   60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 . '))
022500071205     C   60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 .   '))
022600071205     C   60              EVAL      wQFT = %trim(%editw(VAFQFT:'        0 .   '))
022700071205     C   60              EVAL      wPOR = %trim(%editw(VAFPOR:'      0 .   '))
022800071205     C   60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 .   '))
022900071205     C   60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 .   '))
023000071205     C   60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 .   '))
023100071205     C   60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 .   '))
023200071205     C   60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 .   '))
023300071205     C   60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 .   '))
023400071205     C   60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 .   '))
023500071205     C   60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 .   '))
023600071205     C   60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 .   '))
023700071205     C   60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 .   '))
023800071205     C   60              EVAL      wIAS = %trim(%editw(VAFIAS:'        0 .   '))
023900071205     C   60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 . '))
024000071205     C   60              EVAL      wVLC = %trim(%editw(VAFVLC:'0 .   '))
024100150722     C*
024200150722     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+sepFld+
024300150722     C                                       %trim(%editc(VAFLNP:'Q'))+sepFld+
024400150722     C                                       %trim(%editc(VAFNRS:'Q'))+sepFld+
024500150722     C                                       %trim(%editc(VAFNSP:'Q'))+sepFld+
024600150722     C                                       %trim(%editc(VAFRMN:'Q'))+sepFld+
024700150722     C                                          '"'+%trim(VAFRMA)+'"'+sepFld+
024800150722     C                                          '"'+%trim(VAFRMX)+'"'+sepFld+
024900150722     C                                       %trim(%editc(VAFNCL:'Q'))+sepFld+
025000150722     C                                                %trim(wPKB)+sepFld+
025100150722     C                                                %trim(wVLF)+sepFld+
025200150722     C                                          '"'+%trim(VAFFPC)+'"'+sepFld+
025300150722     C                                                %trim(wPKC)+sepFld+
025400150722     C                                          '"'+%trim(VAFFVC)+'"'+sepFld+
025500150722     C                                                %trim(wVLC)+sepFld
025600150722     C*
025700150722     C                   WRITE     TIVAW000
025800090423     C*
025900090423     C                   ENDIF
026000090423     C*
026100090325     C                   delete    tivgd000
026200991027     C*
026300090325     C                   READ      tivgdtmp
026400030325     C                   ENDDO
026500991027     C*
026600030325     C                   EVAL      wrkesito = '0'
026700991027     C*
026800910830     C                   ENDSR
026900090423
027000090423
027100991027
027200991027      /TITLE Operazioni iniziali.
027300991027     C     *inzsr        BEGSR
027400991027     C*
027500991027     C     *ENTRY        PLIST
027600991027     C                   parm                    prmppt
027700991027     C     wrkesito      parm      wrkesito      prmesito
027800070316     C*
027900070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
028000070316     C                   eval      sepFld = %subst(prmppt:2:1)
028100070316     C                   if        sepFld = *blanks
028200070316     C                   eval      sepFld = ','
028300070316     C                   endif
028400071205     C*
028500071205     C* Verifico se richiesto un particolare carattere d separatore decimale
028600071205     C                   SETON                                        60
028700071205     C                   IF        %subst(prmppt:3:1) = ','
028800071205     C                   SETOFF                                       60
028900071205     C                   ENDIF
029000030709     C*
029100030709     C* Definizione chiavi
029200030709     C*
029300030709     C* Chiave su TITAS30C - Completa
029400030709     C     KEYtas30C     KLIST
029500030709     C                   KFLD                    vafAAS
029600030709     C                   KFLD                    vafLNP
029700030709     C                   KFLD                    vafNRS
029800030709     C                   KFLD                    vafNSP
029900030709     C                   KFLD                    vafTBL
030000030709     C*
030100030709     C* Chiave su TITAA30C - Completa
030200030709     C     KEYtaa30C     KLIST
030300030709     C                   KFLD                    vafAAS
030400030709     C                   KFLD                    vafLNP
030500030709     C                   KFLD                    vafNRS
030600030709     C                   KFLD                    vafNSP
030700030709     C                   KFLD                    taaTRC
030800090423     C*
030900090423     C* Chiave su TITA430C - Completa
031000090423     C     KEYta430C     KLIST
031100090423     C                   KFLD                    vafAAS
031200090423     C                   KFLD                    vafLNP
031300090423     C                   KFLD                    vafNRS
031400090423     C                   KFLD                    vafNSP
031500090423     C                   KFLD                    ta4TRC
031600090423     C*
031700090423     C* Chiave su FNORM01L - Completa
031800090423     C     KEYorm01C     KLIST
031900090423     C                   KFLD                    §TA4MPOE
032000090423     C                   KFLD                    §TA4MNSR
032100090423     C                   KFLD                    §TA4MNOR
032200090423     C                   KFLD                    §TA4MNRV
032300991027     C*
032400991027     C                   ENDSR
