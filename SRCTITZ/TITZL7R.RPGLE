000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090720     H DFTACTGRP(*NO) BNDDIR('UBRTVNETA') ACTGRP('BARTVAS')
000300171130      *
000400171130      * PARTICOLARITA':
000500171130      *
000600171130      * copiato dal TIS7TC3R3 e poi personalizzato
000700171130      *
000800991027
000900080924     Ftivgdtmp  uF   E             DISK
001000070426     FTITAS30C  IF   E           K DISK
001100090825     FFNBLT01L  IF   E           K DISK    usropn
001200090720     F                                     extfile(LibFileBLT01)
001300090720     FTITAH30C  IF   E           K DISK
001400090720     FTIVAWWWT  uF A E             DISK    commit
001500991027
001600991027     D prmppt          s             50
001700991027     D prmesito        s              1
001800991027     D wrkesito        s                   like(prmesito)
001900030325
002000030331     D dsvac         e ds                  extname(fnvac00t)
002100051007     D wIAS            s             14
002200051007     D wPKB            s              8
002300051007     D wVLB            s              6
002400051007     D wQFT            s             14
002500051007     D wCAS            s             14
002600090720     D wNNC            s             35
002700991027
002800090720     D LibFileBLT01    s             21A   inz
002900090720     D currSysNeta     s              8A   inz('*NULL')
003000090720
003100090720
003200090720     D*------------------
003300090720     D* LINKING A DEFINIZIONI ESTERNE
003400090720     D*------------------
003500090720     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
003600090720     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
003700090720
003800090720
003900090720     C*
004000090720     C* Reperisco il sistema AS/400 corrente
004100090720     C                   callp     UBRTVNETA_Rtv(currSysNeta)
004200090720     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
004300090720     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
004400090720     C                   else
004500090720     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
004600090720     C                   endif
004700090720     C*
004800090720     C* Apertura file "overraidati"
004900090720     C                   if        not %open(FNBLT01L)
005000090720     C                   open      FNBLT01L
005100090720     C                   endif
005200090720     C*
005300090720     C* Effettuo traduzione
005400030908     C                   EXSR      chkRECFILMBR
005500991027     C                   EXSR      traduci
005600090720     C*
005700090720     C* Chiusura file "overraidati"
005800090720     C                   if        %open(FNBLT01L)
005900090720     C                   close     FNBLT01L
006000090720     C                   endif
006100090720     C*
006200921023     C                   SETON                                        LR
006300090720
006400090720
006500030908
006600030908     C     chkRECFILMBR  BEGSR
006700030908     C*
006800090720     C                   READ      TIVAW000                               55
006900030908     C*
007000030908     C                   ENDSR
007100090720
007200090720
007300991027
007400991027     C     traduci       BEGSR
007500051007     C*
007600051007     C* Verifico se richiesto un particolare carattere d separatore campi
007700051007     C                   IF        %subst(prmppt:3:1) <> *blanks
007800051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
007900051007     C                   ELSE
008000171130     C                   MOVEL     ';'           wSepCampi         1
008100051007     C                   ENDIF
008200051007     C*
008300051007     C* Verifico se richiesto un particolare carattere d separatore decimale
008400051007     C                   SETON                                        60
008500051007     C                   IF        %subst(prmppt:4:1) = ','
008600051007     C                   SETOFF                                       60
008700051007     C                   ENDIF
008800030325     C*
008900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
009100171130     C                   EVAL      VAWDTA  = '"SAPDN"'+wSepCampi+
009200171130     C                                       '"EVENT"'+wSepCampi+
009300171130     C                                       '"BEGIN_PLAN_DATE"'+wSepCampi+
009400171130     C                                       '"END_PLAN_DATE"'+wSepCampi+
009500171130     C                                       '"BEGIN_IST_DATE"'+wSepCampi+
009600171130     C                                       '"END_IST_DATE"'+wSepCampi+
009700171130     C                                       '" "'+wSepCampi+
009800171130     C                                       '"timezone_plan"'+wSepCampi+
009900171130     C                                       '"timezone_act"'+wSepCampi+
010000171130     C                                       '"handlingUnit"'
010100070426     C*
010200070426     C                   ENDIF
010300030325     C*
010400090720     C                   WRITE     TIVAW000
010500030325     C                   ENDIF
010600991027     C*
010700080924     C                   READ      tivgdtmp
010800080924     C                   DOW       not %eof(tivgdtmp)
010900090720     C                   movel     vgddta        dsvac
011000090720     C*
011100090720     C                   CLEAR                   wNNC
011200090720     C*
011300090720     C* Aggancio il dettaglio colli
011400090720     C                   EVAL      tahTRC = 'C'
011500090720     C     KEYtah30_C    SETLL     titah30c
011600090720     C                   IF        %equal(titah30c)
011700090720     C     KEYtah30_C    READE     titah30c
011800090720     C                   DOW       not %eof(titah30c)
011900090720     C                   EVAL      wNNC = tahNOT
012000090720     C*
012100090720     C* Se riesco considero la data consegna merce d ogni singolo collo
012200090720     C                   EVAL      bltNSC = tahNSC
012300090720     C     KEYblt01_C    CHAIN     fnblt01l
012400090720     C                   IF        %found(fnblt01l)
012500090720     C                   IF        bltDCM > *zeros
012600090720     C                   MOVE      bltDCM        VACDCM
012700090720     C                   ENDIF
012800090720     C                   ENDIF
012900090720     C*
013000090720     C* Valorizzo il buffer d output
013100090720     C                   EXSR      ValBuffOut
013200090720     C*
013300090720     C     KEYtah30_C    READE     titah30c
013400090720     C                   ENDDO
013500090720     C                   ELSE
013600090720     C*
013700090720     C* Valorizzo il buffer d output
013800090720     C                   EXSR      ValBuffOut
013900090720     C*
014000090720     C                   ENDIF
014100090720     C*
014200090720     C* Proseguo con la lettura del file
014300080924     C                   DELETE    tivgd000
014400080924     C                   READ      tivgdtmp
014500090720     C*
014600030325     C                   ENDDO
014700991027     C*
014800030325     C                   EVAL      wrkesito = '0'
014900991027     C*
015000910830     C                   ENDSR
015100090720
015200090720
015300090720
015400090720     C     ValBuffOut    BEGSR
015500090720     C*
015600171201     C* scrivo un rcd per ogni evento, se questo si è verificato (ho la data)
015700171201     C*
015800171201     C* data consegna richiesta per la spedizione
015900171201     C                   IF        VACDCR > 0
016000171201     C                   EVAL      VAWDTA  =%trim(VACRMA)+wSepCampi+
016100171130     C                                      '007'+wSepCampi+
016200171130      *                                      yyyymmddhhmmss
016300171130     C                                      '00000000000000'+wSepCampi+
016400171130     C                                      '00000000000000'+wSepCampi+
016500171130     C                                      '00000000000000'+wSepCampi+
016600171130     C                                      '00000000000000'+wSepCampi+
016700171130     C                                      %trim(%editc(VACDCR:'Q'))+wSepCampi+
016800171130     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
016900171130     C                                      'CET'+wSepCampi+
017000171130     C                                      'CET'+wSepCampi+
017100171130     C                                      camponuovo
017200171201     C*
017300171201     C                   WRITE     TIVAW000
017400171201     C                   ENDIF
017500171130     C*
017600171201     C* data consegna singolo collo
017700171201     C                   IF        VACDCM > 0
017800171130     C                   EVAL      VAWDTA  =%trim(VACRMA)+wSepCampi+
017900171130     C                                      '008'+wSepCampi+
018000171130      *                                      yyyymmddhhmmss
018100171130     C                                      '00000000000000'+wSepCampi+
018200171130     C                                      '00000000000000'+wSepCampi+
018300171130     C                                      '00000000000000'+wSepCampi+
018400171130     C                                      '00000000000000'+wSepCampi+
018500171130     C                                      %trim(%editc(VACDCM:'Q'))+wSepCampi+
018600171130     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
018700171130     C                                      'CET'+wSepCampi+
018800171130     C                                      'CET'+wSepCampi+
018900171130     C                                      camponuovo
019000171201     C*
019100171201     C                   WRITE     TIVAW000
019200171201     C                   ENDIF
019300171130     C*
019400171201     C* data giacenza spedizione
019500171201     C                   IF        VACDAG > 0
019600171130     C                   EVAL      VAWDTA  =%trim(VACRMA)+wSepCampi+
019700171130     C                                      '016'+wSepCampi+
019800171130      *                                      yyyymmddhhmmss
019900171130     C                                      '00000000000000'+wSepCampi+
020000171130     C                                      '00000000000000'+wSepCampi+
020100171130     C                                      '00000000000000'+wSepCampi+
020200171130     C                                      '00000000000000'+wSepCampi+
020300171130     C                                      %trim(%editc(VACDAG:'Q'))+
020400171130     C                                      '0000'+wSepCampi+
020500171130     C                                      'CET'+wSepCampi+
020600171130     C                                      'CET'+wSepCampi+
020700171130     C                                      camponuovo
020800171201     C*
020900171201     C                   WRITE     TIVAW000
021000171201     C                   ENDIF
021100090720     C*
021200090720     C                   ENDSR
021300991027
021400090720
021500090720
021600991027      /TITLE Operazioni iniziali.
021700991027     C     *inzsr        BEGSR
021800991027     C*
021900991027     C     *ENTRY        PLIST
022000991027     C                   parm                    prmppt
022100991027     C     wrkesito      parm      wrkesito      prmesito
022200070426     C*
022300070426     C* Definizione chiavi
022400070426     C*
022500090720     C* Chiave su TITAS30C - Parziale
022600090720     C     KEYtas30_P    KLIST
022700070426     C                   KFLD                    vacAAS
022800070426     C                   KFLD                    vacLNP
022900070426     C                   KFLD                    vacNRS
023000070426     C                   KFLD                    vacNSP
023100090720     C*
023200090720     C* Chiave su TITAH30C - completa
023300090720     C     KEYtah30_C    KLIST
023400090720     C                   KFLD                    vacAAS
023500090720     C                   KFLD                    vacLNP
023600090720     C                   KFLD                    vacNRS
023700090720     C                   KFLD                    vacNSP
023800090720     C                   KFLD                    tahTRC
023900090720     C*
024000090720     C* Chiave su FNBLT01L - completa
024100090720     C     KEYblt01_C    KLIST
024200090720     C                   KFLD                    vacAAS
024300090720     C                   KFLD                    vacLNP
024400090720     C                   KFLD                    vacNRS
024500090720     C                   KFLD                    vacNSP
024600090720     C                   KFLD                    bltNSC
024700991027     C*
024800991027     C                   ENDSR
