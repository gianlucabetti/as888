000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400100224     Fazorg01l  IF   E           K DISK
000500090325     Ftivgdtmp  uF   E             DISK
000600100209     FTIVAWWWT  UF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100090325     D tivafds       e ds                  extname(tivaf00t)
001200070316     D sepFld          s              1
001300071205
001400071205     D wPKB            s              8
001500071205     D wVLF            s              6
001600071205     D wVA1            s             12
001700071205     D wVA2            s             12
001800071205     D wVA3            s             12
001900071205     D wVA4            s             12
002000071205     D wVA5            s             12
002100071205     D wVA6            s             12
002200071205     D wVA7            s             12
002300071205     D wVA8            s             12
002400071205     D wVAX            s             12
002500071205     D wIMV            s             14
002600071205     D wPKC            s              8
002700991027
002800100224
002900100224
003000030908     C                   EXSR      chkRECFILMBR
003100991027     C                   EXSR      traduci
003200921023     C                   SETON                                        LR
003300030908
003400100224
003500030908     C     chkRECFILMBR  BEGSR
003600030908     C*
003700100209     C                   READ      TIVAWWWT                               55
003800030908     C*
003900030908     C                   ENDSR
004000100224
004100991027
004200991027     C     traduci       BEGSR
004300030325     C*
004400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004500090427     C* (nomi campi)
004600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004700100209     C                   EVAL      VAWDTA =
004800090427     C                             '"VAFLNP"'                           +sepFld+
004900090427     C                             '"VAFNRS"'                           +sepFld+
005000090427     C                             '"VAFNSP"'                           +sepFld+
005100090427     C                             '"VAFMGS"'                           +sepFld+
005200090427     C                             '"VAFKSC"'                           +sepFld+
005300090427     C                             '"VAFLNA"'                           +sepFld+
005400090427     C                             '"VAFNCL"'                           +sepFld+
005500090427     C                             '"VAFPKB"'                           +sepFld+
005600090427     C                             '"VAFVLF"'                           +sepFld+
005700090427     C                             '"VAFSV1"'                           +sepFld+
005800090427     C                             '"VAFVA1"'                           +sepFld+
005900090427     C                             '"VAFSV2"'                           +sepFld+
006000090427     C                             '"VAFVA2"'                           +sepFld+
006100090427     C                             '"VAFSV3"'                           +sepFld+
006200090427     C                             '"VAFVA3"'                           +sepFld+
006300090427     C                             '"VAFSV4"'                           +sepFld+
006400090427     C                             '"VAFVA4"'                           +sepFld+
006500090427     C                             '"VAFSV5"'                           +sepFld+
006600090427     C                             '"VAFVA5"'                           +sepFld+
006700090427     C                             '"VAFSV6"'                           +sepFld+
006800090427     C                             '"VAFVA6"'                           +sepFld+
006900090427     C                             '"VAFSV7"'                           +sepFld+
007000090427     C                             '"VAFVA7"'                           +sepFld+
007100090427     C                             '"VAFSV8"'                           +sepFld+
007200090427     C                             '"VAFVA8"'                           +sepFld+
007300090427     C                             '"VAFVAX"'                           +sepFld+
007400090427     C                             '"VAFIMV"'                           +sepFld+
007500090427     C                             '"VAFDFT"'                           +sepFld+
007600090427     C                             '"VAFNFT"'                           +sepFld+
007700090427     C                             '"VAFDRT"'                           +sepFld+
007800090427     C                             '"VAFRMN"'                           +sepFld+
007900090427     C                             '"VAFTSP"'                           +sepFld+
008000090427     C                             '"VAFCAD"'                           +sepFld+
008100090427     C                             '"VAFLOD"'                           +sepFld+
008200100224     C                             '"VAFPRP"'                           +sepFld+
008300090427     C                             '"VAFPRD"'                           +sepFld+
008400090427     C                             '"VAFNZD"'                           +sepFld+
008500090427     C                             '"VAFFPC"'                           +sepFld+
008600100224     C                             '"VAFPKC"'
008700030325     C*
008800100209     C                   WRITE     TIVAW000
008900030325     C                   ENDIF
009000090427     C*
009100090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009200090427     C* (descrizione campi)
009300090427     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
009400100209     C                   EVAL      VAWDTA =
009500090427     C                             '"P.O. PARTENZA"'                    +sepFld+
009600090427     C                             '"NUMERO SERIE"'                     +sepFld+
009700100209     C                             '"NUMERO SPED."'                     +sepFld+
009800100209     C                             '"MESE/GIORNO SPED."'                +sepFld+
009900090427     C                             '"CODICE CLIENTE"'                   +sepFld+
010000090427     C                             '"P.O. ARRIVO"'                      +sepFld+
010100090427     C                             '"NUMERO COLLI"'                     +sepFld+
010200090427     C                             '"PESO BOLLETTATO IN KG."'           +sepFld+
010300090427     C                             '"VOLUME DA FATTURARE"'              +sepFld+
010400090427     C                             '"SIGLA VARIA 1"'                    +sepFld+
010500090427     C                             '"VARIA 1"'                          +sepFld+
010600090427     C                             '"SIGLA VARIA 2"'                    +sepFld+
010700090427     C                             '"VARIA 2"'                          +sepFld+
010800090427     C                             '"SIGLA VARIA 3"'                    +sepFld+
010900090427     C                             '"VARIA 3"'                          +sepFld+
011000090427     C                             '"SIGLA VARIA 4"'                    +sepFld+
011100090427     C                             '"VARIA 4"'                          +sepFld+
011200090427     C                             '"SIGLA VARIA 5"'                    +sepFld+
011300090427     C                             '"VARIA 5"'                          +sepFld+
011400090427     C                             '"SIGLA VARIA 6"'                    +sepFld+
011500090427     C                             '"VARIA 6"'                          +sepFld+
011600090427     C                             '"SIGLA VARIA 7"'                    +sepFld+
011700090427     C                             '"VARIA 7"'                          +sepFld+
011800090427     C                             '"SIGLA VARIA 8"'                    +sepFld+
011900090427     C                             '"VARIA 8"'                          +sepFld+
012000090427     C                             '"CUMULATIVO ALTRE VARIE"'           +sepFld+
012100090427     C                             '"IMPONIBILE IVA"'                   +sepFld+
012200090427     C                             '"DATA FATTURA (AMG)"'               +sepFld+
012300090427     C                             '"NUMERO FATTURA"'                   +sepFld+
012400090427     C                             '"DATA RITIRO (AMG)"'                +sepFld+
012500090427     C                             '"RIFERIM.MITTENTE NUMERICO"'        +sepFld+
012600100209     C                             '"TIPO SERVIZIO SPED."'              +sepFld+
012700090427     C                             '"CAP DESTINATARIO"'                 +sepFld+
012800090427     C                             '"LOCALITA DESTINATARIO"'            +sepFld+
012900100224     C                             '"PROVINCIA PARTENZA"'               +sepFld+
013000090427     C                             '"PROVINCIA DESTINATARIO"'           +sepFld+
013100090427     C                             '"NAZIONE DESTINATARIO"'             +sepFld+
013200090427     C                             '"TIPO PESO RILEVATO"'               +sepFld+
013300100224     C                             '"PESO RILEVATO IN KG."'
013400090427     C*
013500100209     C                   WRITE     TIVAW000
013600090427     C                   ENDIF
013700991027     C*
013800090325     C                   READ      tivgdtmp
013900090325     C                   DOW       not %eof
014000100224     C                   movel     vgddta        tivafds
014100100224     C*
014200100224     C                   movel     *blank        wPRP              2
014300030709     C*
014400100224     C* Reperisco i dati NON presenti sul TIVAF00T
014500100224     C     vafLNP        CHAIN     azorg01l
014600100224     C                   IF        %found(azorg01l)
014700100224     C                   EVAL      wPRP = orgPRO
014800100224     C                   ENDIF
014900071205     C*
015000071205     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
015100071205     C  N60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 , '))
015200071205     C  N60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 ,   '))
015300071205     C  N60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 ,   '))
015400071205     C  N60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 ,   '))
015500071205     C  N60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 ,   '))
015600071205     C  N60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 ,   '))
015700071205     C  N60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 ,   '))
015800071205     C  N60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 ,   '))
015900071205     C  N60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 ,   '))
016000071205     C  N60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 ,   '))
016100071205     C  N60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 ,   '))
016200071205     C  N60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 ,   '))
016300071205     C  N60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 , '))
016400071205     C   60              EVAL      wPKB = %trim(%editw(VAFPKB:'    0 . '))
016500071205     C   60              EVAL      wVLF = %trim(%editw(VAFVLF:'0 .   '))
016600071205     C   60              EVAL      wVA1 = %trim(%editw(VAFVA1:'      0 .   '))
016700071205     C   60              EVAL      wVA2 = %trim(%editw(VAFVA2:'      0 .   '))
016800071205     C   60              EVAL      wVA3 = %trim(%editw(VAFVA3:'      0 .   '))
016900071205     C   60              EVAL      wVA4 = %trim(%editw(VAFVA4:'      0 .   '))
017000071205     C   60              EVAL      wVA5 = %trim(%editw(VAFVA5:'      0 .   '))
017100071205     C   60              EVAL      wVA6 = %trim(%editw(VAFVA6:'      0 .   '))
017200071205     C   60              EVAL      wVA7 = %trim(%editw(VAFVA7:'      0 .   '))
017300071205     C   60              EVAL      wVA8 = %trim(%editw(VAFVA8:'      0 .   '))
017400071205     C   60              EVAL      wVAX = %trim(%editw(VAFVAX:'      0 .   '))
017500071205     C   60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 .   '))
017600071205     C   60              EVAL      wPKC = %trim(%editw(VAFPKC:'    0 . '))
017700100224     C*
017800100224     C                   EVAL      VAWDTA  =
017900070316     C                                       %trim(%editc(VAFLNP:'Q'))+sepFld+
018000070316     C                                       %trim(%editc(VAFNRS:'Q'))+sepFld+
018100070316     C                                       %trim(%editc(VAFNSP:'Q'))+sepFld+
018200070316     C                                       %trim(%editc(VAFMGS:'Q'))+sepFld+
018300070316     C                                       %trim(%editc(VAFKSC:'Q'))+sepFld+
018400070316     C                                       %trim(%editc(VAFLNA:'Q'))+sepFld+
018500070316     C                                       %trim(%editc(VAFNCL:'Q'))+sepFld+
018600100224     C                                                %trim(wPKB)     +sepFld+
018700100224     C                                                %trim(wVLF)     +sepFld+
018800100224     C                                          '"'+%trim(VAFSV1)+'"' +sepFld+
018900100224     C                                                %trim(wVA1)     +sepFld+
019000100224     C                                          '"'+%trim(VAFSV2)+'"' +sepFld+
019100100224     C                                                %trim(wVA2)     +sepFld+
019200100224     C                                          '"'+%trim(VAFSV3)+'"' +sepFld+
019300100224     C                                                %trim(wVA3)     +sepFld+
019400100224     C                                          '"'+%trim(VAFSV4)+'"' +sepFld+
019500100224     C                                                %trim(wVA4)     +sepFld+
019600100224     C                                          '"'+%trim(VAFSV5)+'"' +sepFld+
019700100224     C                                                %trim(wVA5)     +sepFld+
019800100224     C                                          '"'+%trim(VAFSV6)+'"' +sepFld+
019900100224     C                                                %trim(wVA6)     +sepFld+
020000100224     C                                          '"'+%trim(VAFSV7)+'"' +sepFld+
020100100224     C                                                %trim(wVA7)     +sepFld+
020200100224     C                                          '"'+%trim(VAFSV8)+'"' +sepFld+
020300100224     C                                                %trim(wVA8)     +sepFld+
020400100224     C                                                %trim(wVAX)     +sepFld+
020500100224     C                                                %trim(wIMV)     +sepFld+
020600070316     C                                       %trim(%editc(VAFDFT:'Q'))+sepFld+
020700070316     C                                       %trim(%editc(VAFNFT:'Q'))+sepFld+
020800070316     C                                       %trim(%editc(VAFDRT:'Q'))+sepFld+
020900070316     C                                       %trim(%editc(VAFRMN:'Q'))+sepFld+
021000100224     C                                          '"'+%trim(VAFTSP)+'"' +sepFld+
021100100224     C                                          '"'+%trim(VAFCAD)+'"' +sepFld+
021200100224     C                                          '"'+%trim(VAFLOD)+'"' +sepFld+
021300100224     C                                            '"'+%trim(wPRP)+'"' +sepFld+
021400100224     C                                          '"'+%trim(VAFPRD)+'"' +sepFld+
021500100224     C                                          '"'+%trim(VAFNZD)+'"' +sepFld+
021600100224     C                                          '"'+%trim(VAFFPC)+'"' +sepFld+
021700100224     C                                                %trim(wPKC)
021800930409     C*
021900100209     C                   WRITE     TIVAW000
022000090325     C                   delete    tivgd000
022100991027     C*
022200090325     C                   READ      tivgdtmp
022300030325     C                   ENDDO
022400991027     C*
022500030325     C                   EVAL      wrkesito = '0'
022600991027     C*
022700910830     C                   ENDSR
022800991027
022900991027      /TITLE Operazioni iniziali.
023000991027     C     *inzsr        BEGSR
023100991027     C*
023200991027     C     *ENTRY        PLIST
023300991027     C                   parm                    prmppt
023400991027     C     wrkesito      parm      wrkesito      prmesito
023500070316     C*
023600070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
023700070316     C                   eval      sepFld = %subst(prmppt:2:1)
023800070316     C                   if        sepFld = *blanks
023900070316     C                   eval      sepFld = ','
024000070316     C                   endif
024100071205     C*
024200071205     C* Verifico se richiesto un particolare carattere d separatore decimale
024300071205     C                   SETON                                        60
024400071205     C                   IF        %subst(prmppt:3:1) = ','
024500071205     C                   SETOFF                                       60
024600071205     C                   ENDIF
024700030709     C*
024800030709     C* Definizione chiavi
024900100224     C*
025000030709     C*
025100991027     C*
025200991027     C                   ENDSR
