000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090720     H DFTACTGRP(*NO) BNDDIR('UBRTVNETA') ACTGRP('BARTVAS')
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500090720     FTABEL00F  IF   E           K DISK
000600070426     FTITAS30C  IF   E           K DISK
000700090720     FTITA430C  IF   E           K DISK
000800090720     FFNBLT01L  IF   E           K DISK
000900090720     F                                     extfile(LibFileBLT01)
001000090720     F                                     usropn
001100090720     FTITAH30C  IF   E           K DISK
001200090720     FTIGCP51L  IF   E           K DISK
001300090720     FTIGNP01L  IF   E           K DISK
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
002700090720     D wIND            s             35
002800090720     D wCAD            s              9
002900090720     D wLOD            s             35
003000090720     D wRD2            s             35
003100090720     D wCMC            s              3
003200090720     D wDMC            s             75
003300090720     D wNOT            s             50
003400090720     D wDUR            s              8  0
003500100910     D wNOTE1          s                   like(TA4NOT)
003600100910     D wNOTE2          s                   like(TA4NOT)
003700090720
003800090720     D*-----------
003900090720     D* DEFINIZIONE DS / SCHIERE CODICI EVENTO
004000090720     D*-----------
004100090720     D ds2a          e ds
004200090720     D sk2Acod         s              3    dim(500)
004300090720     D sk2Ades         s             75    dim(500)
004400090720     D i               s              4  0 inz
004500090720
004600991027
004700090720     D LibFileBLT01    s             21A   inz
004800090720     D currSysNeta     s              8A   inz('*NULL')
004900090720
005000090720
005100090720     D*------------------
005200090720     D* LINKING A DEFINIZIONI ESTERNE
005300090720     D*------------------
005400090720     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
005500090720     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
005600090720
005700090720
005800090720     C*
005900090720     C* Reperisco il sistema AS/400 corrente
006000090720     C                   callp     UBRTVNETA_Rtv(currSysNeta)
006100090720     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
006200090720     C                   eval      LibFileBLT01 = 'FILTRA201/FNBLT01L'
006300090720     C                   else
006400090720     C                   eval      LibFileBLT01 = 'FILTRAPRD/FNBLT01L'
006500090720     C                   endif
006600090720     C*
006700090720     C* Apertura file "overraidati"
006800090720     C                   if        not %open(FNBLT01L)
006900090720     C                   open      FNBLT01L
007000090720     C                   endif
007100090720     C*
007200090720     C* Effettuo traduzione
007300090720     C                   EXSR      cartab
007400030908     C                   EXSR      chkRECFILMBR
007500991027     C                   EXSR      traduci
007600090720     C*
007700090720     C* Chiusura file "overraidati"
007800090720     C                   if        %open(FNBLT01L)
007900090720     C                   close     FNBLT01L
008000090720     C                   endif
008100090720     C*
008200921023     C                   SETON                                        LR
008300090720
008400090720
008500030908
008600030908     C     chkRECFILMBR  BEGSR
008700030908     C*
008800090720     C                   READ      TIVAW000                               55
008900030908     C*
009000030908     C                   ENDSR
009100090720
009200090720
009300991027
009400991027     C     traduci       BEGSR
009500051007     C*
009600051007     C* Verifico se richiesto un particolare carattere d separatore campi
009700051007     C                   IF        %subst(prmppt:3:1) <> *blanks
009800051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
009900051007     C                   ELSE
010000051007     C                   MOVEL     ','           wSepCampi         1
010100051007     C                   ENDIF
010200051007     C*
010300051007     C* Verifico se richiesto un particolare carattere d separatore decimale
010400051007     C                   SETON                                        60
010500051007     C                   IF        %subst(prmppt:4:1) = ','
010600051007     C                   SETOFF                                       60
010700051007     C                   ENDIF
010800030325     C*
010900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
011000030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
011100090720     C                   EVAL      VAWDTA  = '"VACAAS"'+wSepCampi+
011200051007     C                                       '"VACLNP"'+wSepCampi+
011300051007     C                                       '"VACNRS"'+wSepCampi+
011400051007     C                                       '"VACNSP"'+wSepCampi+
011500051007     C                                       '"VACMGS"'+wSepCampi+
011600051007     C                                       '"VACCBO"'+wSepCampi+
011700051007     C                                       '"VACLNA"'+wSepCampi+
011800051007     C                                       '"VACRSD"'+wSepCampi+
011900051007     C                                       '"VACPRD"'+wSepCampi+
012000051007     C                                       '"VACGC1"'+wSepCampi+
012100051007     C                                       '"VACGC2"'+wSepCampi+
012200051007     C                                       '"VACCTR"'+wSepCampi+
012300051007     C                                       '"VACCTS"'+wSepCampi+
012400051007     C                                       '"VACFTM"'+wSepCampi+
012500051007     C                                       '"VACFIN"'+wSepCampi+
012600051007     C                                       '"VACFAP"'+wSepCampi+
012700051007     C                                       '"VACTSP"'+wSepCampi+
012800051007     C                                       '"VACIAS"'+wSepCampi+
012900051007     C                                       '"VACVAS"'+wSepCampi+
013000051007     C                                       '"VACNAS"'+wSepCampi+
013100051007     C                                       '"VACNCL"'+wSepCampi+
013200051007     C                                       '"VACPKB"'+wSepCampi+
013300051007     C                                       '"VACVLB"'+wSepCampi+
013400051007     C                                       '"VACQFT"'+wSepCampi+
013500051007     C                                       '"VACTIC"'+wSepCampi+
013600051007     C                                       '"VACCAS"'+wSepCampi+
013700051007     C                                       '"VACVCA"'+wSepCampi+
013800051007     C                                       '"VACCCM"'+wSepCampi+
013900051007     C                                       '"VACRMN"'+wSepCampi+
014000051007     C                                       '"VACRMA"'+wSepCampi+
014100051007     C                                       '"VACRMO"'+wSepCampi+
014200051007     C                                       '"VACFFD"'+wSepCampi+
014300051007     C                                       '"VACTCR"'+wSepCampi+
014400051007     C                                       '"VACDCR"'+wSepCampi+
014500051007     C                                       '"VACHCR"'+wSepCampi+
014600051007     C                                       '"VACDCM"'+wSepCampi+
014700051007     C                                       '"VACHMC"'+wSepCampi+
014800051007     C                                       '"VACTC1"'+wSepCampi+
014900051007     C                                       '"VACTC2"'+wSepCampi+
015000051007     C                                       '"VACCCA"'+wSepCampi+
015100051007     C                                       '"VACDLA"'+wSepCampi+
015200090720     C                                       '"VACDAG"'+wSepCampi+
015300090720     C                                      '"SEGNAC."'+wSepCampi+
015400090720     C                                       '"VACRD2"'+wSepCampi+
015500070426     C                                       '"VACIND"'+wSepCampi+
015600070426     C                                       '"VACCAD"'+wSepCampi+
015700090720     C                                       '"VACLOD"'+wSepCampi+
015800090720     C                                       '"VAGCMC"'+wSepCampi+
015900090720     C                                       '"VAGDMC"'+wSepCampi+
016000090720     C                                       '"VAGNOT"'+wSepCampi+
016100100913     C                                       '"VAGDUR"'+wSepCampi+
016200100913     C                                       '"VABNOT"'+wSepCampi+
016300100913     C                                       '"VABNT2"'
016400091028     C                   WRITE     TIVAW000
016500070426     C                   ENDIF
016600991027     C*
016700080924     C                   READ      tivgdtmp
016800080924     C                   DOW       not %eof(tivgdtmp)
016900100122     C*
017000100122     C     '"':' '       xlate     vgddta        vgddta
017100100122     C*
017200090720     C                   movel     vgddta        dsvac
017300090720     C*
017400090720     C                   CLEAR                   wNNC
017500090720     C*
017600090720     C* Aggancio il dettaglio colli
017700090720     C                   EVAL      tahTRC = 'C'
017800090720     C     KEYtah30_C    SETLL     titah30c
017900090720     C                   IF        %equal(titah30c)
018000090720     C     KEYtah30_C    READE     titah30c
018100090720     C                   DOW       not %eof(titah30c)
018200090720     C                   EVAL      wNNC = tahNOT
018300090720     C*
018400090720     C* Se riesco considero la data consegna merce d ogni singolo collo
018500090720     C                   EVAL      bltNSC = tahNSC
018600090720     C     KEYblt01_C    CHAIN     fnblt01l
018700090720     C                   IF        %found(fnblt01l)
018800090720     C                   IF        bltDCM > *zeros
018900090720     C                   MOVE      bltDCM        VACDCM
019000090720     C                   ENDIF
019100090720     C                   ENDIF
019200090720     C*
019300090720     C* Valorizzo il buffer d output
019400090720     C                   EXSR      ValBuffOut
019500090720     C*
019600090720     C     KEYtah30_C    READE     titah30c
019700090720     C                   ENDDO
019800090720     C                   ELSE
019900090720     C*
020000090720     C* Valorizzo il buffer d output
020100090720     C                   EXSR      ValBuffOut
020200090720     C*
020300090720     C                   ENDIF
020400090720     C*
020500090720     C* Proseguo con la lettura del file
020600080924     C                   DELETE    tivgd000
020700080924     C                   READ      tivgdtmp
020800090720     C*
020900030325     C                   ENDDO
021000991027     C*
021100030325     C                   EVAL      wrkesito = '0'
021200991027     C*
021300910830     C                   ENDSR
021400090720
021500090720
021600090720
021700090720     C     ValBuffOut    BEGSR
021800090720     C*
021900090720     C* Eseguo operazioni x reperimento informazioni mancanti
022000090720     C                   EXSR      RepDati
022100090720     C*
022200090720     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
022300090720     C  N60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 ,   '))
022400090720     C   60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 .   '))
022500090720     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
022600090720     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
022700090720     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
022800090720     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
022900090720     C  N60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 ,   '))
023000090720     C   60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 .   '))
023100090720     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
023200090720     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
023300090720     C*
023400090720     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
023500090720     C                   IF        %subst(prmppt:2:1) = 'D'
023600090720     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+wSepCampi+
023700090720     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
023800090720     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
023900090720     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
024000090720     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
024100090720     C                                         '"'+%trim(VACCBO)+'"' +wSepCampi+
024200090720     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
024300090720     C                                         '"'+%trim(VACRSD)+'"' +wSepCampi+
024400090720     C                                         '"'+%trim(VACPRD)+'"' +wSepCampi+
024500090720     C                                         '"'+%trim(VACGC1)+'"' +wSepCampi+
024600090720     C                                         '"'+%trim(VACGC2)+'"' +wSepCampi+
024700090720     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
024800090720     C                                         '"'+%trim(VACCTS)+'"' +wSepCampi+
024900090720     C                                         '"'+%trim(VACFTM)+'"' +wSepCampi+
025000090720     C                                         '"'+%trim(VACFIN)+'"' +wSepCampi+
025100090720     C                                         '"'+%trim(VACFAP)+'"' +wSepCampi+
025200090720     C                                         '"'+%trim(VACTSP)+'"' +wSepCampi+
025300090720     C                                                    %trim(wIAS)+wSepCampi+
025400090720     C                                         '"'+%trim(VACVAS)+'"' +wSepCampi+
025500090720     C                                         '"'+%trim(VACNAS)+'"' +wSepCampi+
025600090720     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
025700090720     C                                                    %trim(wPKB)+wSepCampi+
025800090720     C                                                    %trim(wVLB)+wSepCampi+
025900090720     C                                                    %trim(wQFT)+wSepCampi+
026000090720     C                                         '"'+%trim(VACTIC)+'"' +wSepCampi+
026100090720     C                                                    %trim(wCAS)+wSepCampi+
026200090720     C                                         '"'+%trim(VACVCA)+'"' +wSepCampi+
026300090720     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
026400090720     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
026500090720     C                                         '"'+%trim(VACRMA)+'"' +wSepCampi+
026600090720     C                                         '"'+%trim(VACRMO)+'"' +wSepCampi+
026700090720     C                                         '"'+%trim(VACFFD)+'"' +wSepCampi+
026800090720     C                                         '"'+%trim(VACTCR)+'"' +wSepCampi+
026900090720     C                             %trim(%editw(VACDCR:'    /  /  '))+wSepCampi+
027000090720     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
027100090720     C                             %trim(%editw(VACDCM:'    /  /  '))+wSepCampi+
027200090720     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
027300090720     C                                         '"'+%trim(VACTC1)+'"' +wSepCampi+
027400090720     C                                         '"'+%trim(VACTC2)+'"' +wSepCampi+
027500090720     C                                         '"'+%trim(VACCCA)+'"' +wSepCampi+
027600090720     C                             %trim(%editw(VACDLA:'    /  /  '))+wSepCampi+
027700090720     C                             %trim(%editw(VACDAG:'    /  /  '))+wSepCampi+
027800090720     C                                            '"'+%trim(wNNC)+'"'+wSepCampi+
027900090720     C                                            '"'+%trim(wRD2)+'"'+wSepCampi+
028000090720     C                                            '"'+%trim(wIND)+'"'+wSepCampi+
028100090720     C                                            '"'+%trim(wCAD)+'"'+wSepCampi+
028200090720     C                                            '"'+%trim(wLOD)+'"'+wSepCampi+
028300090720     C                                            '"'+%trim(wCMC)+'"'+wSepCampi+
028400090720     C                                            '"'+%trim(wDMC)+'"'+wSepCampi+
028500090720     C                                            '"'+%trim(wNOT)+'"'+wSepCampi+
028600100910     C                               %trim(%editw(wDUR:'    /  /  '))+wSepCampi+
028700100910     C                                          '"'+%trim(wNOTE1)+'"'+wSepCampi+
028800100910     C                                          '"'+%trim(wNOTE2)+'"'
028900090720     C                   ELSE
029000090720     C*
029100090720     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+wSepCampi+
029200090720     C                                      %trim(%editc(VACLNP:'Q'))+wSepCampi+
029300090720     C                                      %trim(%editc(VACNRS:'Q'))+wSepCampi+
029400090720     C                                      %trim(%editc(VACNSP:'Q'))+wSepCampi+
029500090720     C                                      %trim(%editc(VACMGS:'Q'))+wSepCampi+
029600090720     C                                          '"'+%trim(VACCBO)+'"'+wSepCampi+
029700090720     C                                      %trim(%editc(VACLNA:'Q'))+wSepCampi+
029800090720     C                                          '"'+%trim(VACRSD)+'"'+wSepCampi+
029900090720     C                                          '"'+%trim(VACPRD)+'"'+wSepCampi+
030000090720     C                                          '"'+%trim(VACGC1)+'"'+wSepCampi+
030100090720     C                                          '"'+%trim(VACGC2)+'"'+wSepCampi+
030200090720     C                                      %trim(%editc(VACCTR:'Q'))+wSepCampi+
030300090720     C                                          '"'+%trim(VACCTS)+'"'+wSepCampi+
030400090720     C                                          '"'+%trim(VACFTM)+'"'+wSepCampi+
030500090720     C                                          '"'+%trim(VACFIN)+'"'+wSepCampi+
030600090720     C                                          '"'+%trim(VACFAP)+'"'+wSepCampi+
030700090720     C                                          '"'+%trim(VACTSP)+'"'+wSepCampi+
030800090720     C                                                    %trim(wIAS)+wSepCampi+
030900090720     C                                          '"'+%trim(VACVAS)+'"'+wSepCampi+
031000090720     C                                          '"'+%trim(VACNAS)+'"'+wSepCampi+
031100090720     C                                      %trim(%editc(VACNCL:'Q'))+wSepCampi+
031200090720     C                                                    %trim(wPKB)+wSepCampi+
031300090720     C                                                    %trim(wVLB)+wSepCampi+
031400090720     C                                                    %trim(wQFT)+wSepCampi+
031500090720     C                                          '"'+%trim(VACTIC)+'"'+wSepCampi+
031600090720     C                                                    %trim(wCAS)+wSepCampi+
031700090720     C                                          '"'+%trim(VACVCA)+'"'+wSepCampi+
031800090720     C                                      %trim(%editc(VACCCM:'Q'))+wSepCampi+
031900090720     C                                      %trim(%editc(VACRMN:'Q'))+wSepCampi+
032000090720     C                                          '"'+%trim(VACRMA)+'"'+wSepCampi+
032100090720     C                                          '"'+%trim(VACRMO)+'"'+wSepCampi+
032200090720     C                                          '"'+%trim(VACFFD)+'"'+wSepCampi+
032300090720     C                                          '"'+%trim(VACTCR)+'"'+wSepCampi+
032400090720     C                                      %trim(%editc(VACDCR:'Q'))+wSepCampi+
032500090720     C                                      %trim(%editc(VACHCR:'Q'))+wSepCampi+
032600090720     C                                      %trim(%editc(VACDCM:'Q'))+wSepCampi+
032700090720     C                                      %trim(%editc(VACHMC:'Q'))+wSepCampi+
032800090720     C                                          '"'+%trim(VACTC1)+'"'+wSepCampi+
032900090720     C                                          '"'+%trim(VACTC2)+'"'+wSepCampi+
033000090720     C                                          '"'+%trim(VACCCA)+'"'+wSepCampi+
033100090720     C                                      %trim(%editc(VACDLA:'Q'))+wSepCampi+
033200090720     C                                      %trim(%editc(VACDAG:'Q'))+wSepCampi+
033300090720     C                                            '"'+%trim(wNNC)+'"'+wSepCampi+
033400090720     C                                            '"'+%trim(wRD2)+'"'+wSepCampi+
033500090720     C                                            '"'+%trim(wIND)+'"'+wSepCampi+
033600090720     C                                            '"'+%trim(wCAD)+'"'+wSepCampi+
033700090720     C                                            '"'+%trim(wLOD)+'"'+wSepCampi+
033800090720     C                                            '"'+%trim(wCMC)+'"'+wSepCampi+
033900090720     C                                            '"'+%trim(wDMC)+'"'+wSepCampi+
034000090720     C                                            '"'+%trim(wNOT)+'"'+wSepCampi+
034100100910     C                                        %trim(%editc(wDUR:'Q'))+wSepCampi+
034200100910     C                                          '"'+%trim(wNOTE1)+'"'+wSepCampi+
034300100910     C                                          '"'+%trim(wNOTE2)+'"'
034400090720     C                   ENDIF
034500090720     C*
034600090720     C                   WRITE     TIVAW000
034700090720     C*
034800090720     C                   ENDSR
034900090720
035000090720
035100090720
035200090720     C     RepDati       BEGSR
035300090720     C*
035400090720     C* Inizializzo i campi "estensione"
035500090720     C                   EVAL      wIND = *blanks
035600090720     C                   EVAL      wCAD = *blanks
035700090720     C                   EVAL      wLOD = *blanks
035800090720     C                   EVAL      wRD2 = *blanks
035900090720     C                   EVAL      wCMC = *blanks
036000090720     C                   EVAL      wDMC = *blanks
036100090720     C                   EVAL      wNOT = *blanks
036200090720     C                   EVAL      wDUR = *zeros
036300100910     C                   EVAL      wNOTE1 = *blanks
036400100910     C                   EVAL      wNOTE2 = *blanks
036500090720     C*
036600090720     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
036700090720     C     KEYtas30_P    CHAIN     titas30c
036800090720     C                   IF        %found(titas30c)
036900090720     C*
037000090720     C                   EVAL      wIND = tasIND
037100090720     C                   EVAL      wCAD = tasCAD
037200090720     C                   EVAL      wLOD = tasLOD
037300090720     C*
037400090720     C* Reperisco la ragione sociale 2
037500090720     C                   EVAL      ta4TRC = 'D'
037600090720     C     KEYta430_C    CHAIN     tita430c
037700090720     C                   IF        %found(tita430c)
037800090720     C                   EVAL      wRD2 = TA4NOT
037900090720     C                   ENDIF
038000100910     C*
038100100910     C* Reperisco le note partenza 1 e 2
038200100910     C                   EVAL      ta4TRC = '8'
038300100910     C     KEYta430_C    CHAIN     tita430c
038400100910     C                   IF        %found(tita430c)
038500100910     C                   EVAL      wNOTE1 = TA4NOT
038600100910     C                   ENDIF
038700100910     C*
038800100910     C                   EVAL      ta4TRC = '9'
038900100910     C     KEYta430_C    CHAIN     tita430c
039000100910     C                   IF        %found(tita430c)
039100100910     C                   EVAL      wNOTE2 = TA4NOT
039200100910     C                   ENDIF
039300090720     C*
039400090720     C* Aggancio la giacenza + recente
039500090720     C                   EVAL      gcpFRG = *zeros
039600090720     C     KEYgcp51_P    chain     tigcp51l
039700090720     C                   if        %found(tigcp51l)
039800090720     C                   eval      wCMC = gcpCMC
039900090720     C                   eval      wDUR = gcpDUR
040000090720     C*
040100090720     C* reperisco la descrizione della causale giacenza
040200090720     C                   z-add     1             i
040300091019     C     wCMC          lookup    sk2Acod(i)                             54
040400090720     C                   if        %found
040500090720     C                   eval      wDMC = sk2Ades(i)
040600090720     C                   endif
040700090720     C*
040800090720     C* Aggancio le note giacenza della giacenza corrente
040900090720     C                   EVAL      gnpNPR = 1
041000090720     C                   EVAL      gnpFAS = 10
041100090720     C     KEYgnp01_P    chain     tignp01l
041200090720     C                   if        %found(tignp01l)
041300090720     C                   eval      wNOT = gnpDMC
041400090720     C                   endif
041500090720     C*
041600090720     C                   endif
041700090720     C                   ENDIF
041800090720     C*
041900090720     C                   ENDSR
042000090720
042100090720
042200090720
042300090720
042400090720     C     cartab        BEGSR
042500090720     C*
042600090720     C* Carico causali evento
042700090720     C                   clear                   sk2Acod
042800090720     C                   clear                   sk2Ades
042900090720     C                   eval      tblKUT = 1
043000090720     C                   eval      tblCOD = '2A'
043100090720     C     KEYtab_P      setll     tabel00f
043200090720     C     KEYtab_P      reade     tabel00f
043300090720     C                   dow       not %eof(tabel00f)
043400090720     C                   add       1             i
043500090720     C                   movel(p)  tblUNI        ds2a
043600090720     C                   eval      sk2Acod(i) = tblKEY
043700090720     C                   eval      sk2Ades(i) = §2ades
043800090720     C     KEYtab_P      reade     tabel00f
043900090720     C                   enddo
044000090720     C*
044100090720     C                   ENDSR
044200991027
044300090720
044400090720
044500991027      /TITLE Operazioni iniziali.
044600991027     C     *inzsr        BEGSR
044700991027     C*
044800991027     C     *ENTRY        PLIST
044900991027     C                   parm                    prmppt
045000991027     C     wrkesito      parm      wrkesito      prmesito
045100070426     C*
045200070426     C* Definizione chiavi
045300090720     C*
045400090720     C* Chiave su TABEL00F - Parziale
045500090720     C     KEYtab_P      KLIST
045600090720     C                   KFLD                    tblKUT
045700090720     C                   KFLD                    tblCOD
045800070426     C*
045900090720     C* Chiave su TITAS30C - Parziale
046000090720     C     KEYtas30_P    KLIST
046100070426     C                   KFLD                    vacAAS
046200070426     C                   KFLD                    vacLNP
046300070426     C                   KFLD                    vacNRS
046400070426     C                   KFLD                    vacNSP
046500090720     C*
046600090720     C* Chiave su TITAH30C - Completa
046700090720     C     KEYtah30_C    KLIST
046800090720     C                   KFLD                    vacAAS
046900090720     C                   KFLD                    vacLNP
047000090720     C                   KFLD                    vacNRS
047100090720     C                   KFLD                    vacNSP
047200090720     C                   KFLD                    tahTRC
047300090720     C*
047400090720     C* Chiave su TITA430C - Completa
047500090720     C     KEYta430_C    KLIST
047600090720     C                   KFLD                    vacAAS
047700090720     C                   KFLD                    vacLNP
047800090720     C                   KFLD                    vacNRS
047900090720     C                   KFLD                    vacNSP
048000090720     C                   KFLD                    ta4TRC
048100090720     C*
048200090720     C* Chiave su FNBLT01L - Completa
048300090720     C     KEYblt01_C    KLIST
048400090720     C                   KFLD                    vacAAS
048500090720     C                   KFLD                    vacLNP
048600090720     C                   KFLD                    vacNRS
048700090720     C                   KFLD                    vacNSP
048800090720     C                   KFLD                    bltNSC
048900090720     C*
049000090720     C* TIGCP51L - Parziale
049100090720     C     KEYgcp51_P    KLIST
049200090720     C                   KFLD                    vacAAS
049300090720     C                   KFLD                    vacLNP
049400090720     C                   KFLD                    vacNRS
049500090720     C                   KFLD                    vacNSP
049600090720     C                   KFLD                    gcpFRG
049700090720     C*
049800090720     C* TIGNP01L - Parziale
049900090720     C     KEYgnp01_P    KLIST
050000090720     C                   KFLD                    gcpAGC
050100090720     C                   KFLD                    gcpFGC
050200090720     C                   KFLD                    gcpNGC
050300090720     C                   KFLD                    gcpFRG
050400090720     C                   KFLD                    gnpNPR
050500090720     C                   KFLD                    gnpFAS
050600991027     C*
050700991027     C                   ENDSR
