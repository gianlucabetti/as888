000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090323     FTIVGDTMP  uF   E             DISK
000500100706     FTIVAWWWT  uF A E             DISK    commit
000600100706     FtitaA30c  if   E           k DISK
000700100706     Fcnaco00f  if   E           k DISK
000800100706     Fcnind00f  if   E           k DISK
000900100706     Ftita432c  iF   E           K DISK    rename(tita4000:tita4032)
001000100706     F                                     rename(tita4P00:tita4P32)
001100100706     Ftitas30c  iF   E           K DISK
001200161122     Ftiori05l  iF   E           K DISK
001300161122     Ffnacr01l  iF   E           K DISK
001400161128     Ffiar531c  iF   E           K DISK
001500100706
001600991027
001700991027     D prmppt          s             50
001800991027     D prmesito        s              1
001900991027     D wrkesito        s                   like(prmesito)
002000090323     D fnvapds       e ds                  extname(fnvap00t)
002100161128     D DAR5BNB       e ds
002200100706
002300100706
002400100706     D wPKB            s              8    inz
002500100706
002600161128     D wPRGBOL         s              3  0
002700161122     D wTAARSC         s                   like(TAARSC)
002800161122     D wTAAIND         s                   like(TAAIND)
002900161122     D wTAACAP         s                   like(TAACAP)
003000161122     D wTAALOC         s                   like(TAALOC)
003100161122     D wTAAPRV         s                   like(TAAPRV)
003200161122     D wTAANAZ         s                   like(TAANAZ)
003300161122     D wTASRSD         s                   like(TASRSD)
003400161122     D wTASIND         s                   like(TASIND)
003500161122     D wTASCAD         s                   like(TASCAD)
003600161122     D wTASLOD         s                   like(TASLOD)
003700161122     D wTASPRD         s                   like(TASPRD)
003800161122     D wTASNZD         s                   like(TASNZD)
003900161122     D wTASNCL         s                   like(TASNCL)
004000161122     D wTASPKB         s                   like(TASPKB)
004100161122     D wVAONCL         s                   like(VAONCL)
004200100706
004300100706
004400100706     D*------------
004500100706     D* DS di ridefinizione chiave esterna ORM
004600100706     D*------------
004700100706     D                 DS
004800100706     D x_vapPOE                       3  0
004900100706     D x_vapNSR                       2  0
005000100706     D x_vapNOR                       7  0
005100100706     D x_vapNRV                       2  0
005200100706     D keyORM                  1     14
005300100706
005400100706
005500991027
005600030908     C                   EXSR      chkRECFILMBR
005700991027     C                   EXSR      traduci
005800921023     C                   SETON                                        LR
005900100706
006000100706
006100030908
006200030908     C     chkRECFILMBR  BEGSR
006300100706     C*
006400100706     C                   READ      TIVAWWWT                               55
006500030908     C*
006600030908     C                   ENDSR
006700100706
006800100706
006900991027
007000991027     C     traduci       BEGSR
007100051007     C*
007200051007     C* Verifico se richiesto un particolare carattere d separatore campi
007300051007     C                   IF        %subst(prmppt:3:1) <> *blanks
007400051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
007500051007     C                   ELSE
007600100706     C                   MOVEL     ';'           wSepCampi         1
007700051007     C                   ENDIF
007800051007     C*
007900051007     C* Verifico se richiesto un particolare carattere d separatore decimale
008000051007     C                   SETON                                        60
008100100706     C                   IF        %subst(prmppt:4:1) = ','
008200100706     C                   SETOFF                                       60
008300051007     C                   ENDIF
008400030325     C*
008500030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
008700161128     C                   EVAL      VAWDTA  = '"PRGBOL"'+wSepCampi+
008800161128     C                                       '"VAPIDC"'+wSepCampi+
008900060719     C                                       '"VAPPOE"'+wSepCampi+
009000060719     C                                       '"VAPDPE"'+wSepCampi+
009100060719     C                                       '"VAPNSR"'+wSepCampi+
009200060719     C                                       '"VAPNOR"'+wSepCampi+
009300060719     C                                       '"VAPNRV"'+wSepCampi+
009400060719     C                                       '"VAPRFA"'+wSepCampi+
009500060719     C                                       '"VAPPOG"'+wSepCampi+
009600060719     C                                       '"VAPDPG"'+wSepCampi+
009700060719     C                                       '"VAPDAE"'+wSepCampi+
009800060719     C                                       '"VAPORE"'+wSepCampi+
009900060719     C                                       '"VAPFAR"'+wSepCampi+
010000060719     C                                       '"VAPDFA"'+wSepCampi+
010100060719     C                                       '"VAPCAR"'+wSepCampi+
010200060719     C                                       '"VAPDCA"'+wSepCampi+
010300100706     C                                       '"VAPNOT"'+wSepCampi+
010400100706     C                                       '"MITRSC"'+wSepCampi+
010500100706     C                                       '"MITIND"'+wSepCampi+
010600100706     C                                       '"MITCAP"'+wSepCampi+
010700100706     C                                       '"MITLOC"'+wSepCampi+
010800100706     C                                       '"MITPRV"'+wSepCampi+
010900161122     C                                       '"MITNAZ"'+wSepCampi+
011000100706     C                                       '"DESRSD"'+wSepCampi+
011100100706     C                                       '"DESIND"'+wSepCampi+
011200100706     C                                       '"DESCAD"'+wSepCampi+
011300100706     C                                       '"DESLOD"'+wSepCampi+
011400100706     C                                       '"DESPRD"'+wSepCampi+
011500100706     C                                       '"DESNZD"'+wSepCampi+
011600161122     C                                       '"VAONCL"'+wSepCampi+
011700100706     C                                       '"BOLNCL"'+wSepCampi+
011800100706     C                                       '"BOLPKB"'
011900030325     C*
012000100706     C                   WRITE     TIVAW000
012100030325     C                   ENDIF
012200991027     C*
012300090323     C                   READ      TIVGDTMP
012400090323     C                   DOW       not %eof(TIVGDTMP)
012500090323     C                   eval      fnvapds = vgdDTA
012600120127     C*
012700120127     C                   setoff                                       40
012800120127     C*
012900120127     C
013000120127     C                   if        %subst(prmppt:5:4) <> *blanks
013100120127     C                   if        %subst(prmppt:5:1) = 'S' and
013200161128     C                             %dec(%subst(prmppt:5+1:3):3:0) <> VAPFAR
013300120127     C                   seton                                        40
013400120127     C                   endif
013500120127     C*
013600120127     C                   if        %subst(prmppt:5:1) = 'E' and
013700120127     C                             %dec(%subst(prmppt:5+1:3):3:0) = VAPFAR
013800120127     C                   seton                                        40
013900120127     C                   endif
014000120127     C                   endif
014100120127     C*
014200120127     C* Restituisco i dati solo se rispondono ai requisiti richiesti nei parametri
014300120127     C                   if        not *in40
014400161128     C*
014500161128     C* Inizializzo indicatore di scrittura
014600161128     C                   setoff                                       35
014700100706     C*
014800100706     C* Reperisco alcuni dati direttamente dalla bolla legata al ritiro
014900161122     C                   exsr      inzREP
015000161122     C                   exsr      repORI
015100161122     C                   exsr      repTAS
015200161128     C*
015300161128     C* Se non già scritto almeno un record => scarico buffer "in sospeso"
015400161128     C  N35              exsr      valOUT
015500161128     C*
015600120127     C                   endif
015700120127     C*
015800090323     C                   DELETE    TIVGD000
015900991027     C*
016000090323     C                   READ      TIVGDTMP
016100030325     C                   ENDDO
016200991027     C*
016300030325     C                   EVAL      wrkesito = '0'
016400991027     C*
016500910830     C                   ENDSR
016600161122
016700161122
016800161122
016900161122      /TITLE inizializzazione Reperimenti successivi
017000161122     C     INZREP        BEGSR
017100161122     C*
017200161128     C                   clear                   wPRGBOL
017300161122     C                   clear                   wTAARSC
017400161122     C                   clear                   wTAAIND
017500161122     C                   clear                   wTAACAP
017600161122     C                   clear                   wTAALOC
017700161122     C                   clear                   wTAAPRV
017800161122     C                   clear                   wTAANAZ
017900161122     C                   clear                   wTASRSD
018000161122     C                   clear                   wTASIND
018100161122     C                   clear                   wTASCAD
018200161122     C                   clear                   wTASLOD
018300161122     C                   clear                   wTASPRD
018400161122     C                   clear                   wTASNZD
018500161128     C                   clear                   wTASNCL
018600161128     C                   clear                   wTASPKB
018700161128     C                   clear                   wVAONCL
018800161122     C*
018900161122     C                   ENDSR
019000100706
019100100706
019200100706
019300100706      /TITLE Reperimento dati dalle bolle
019400161122     C     REPTAS        BEGSR
019500161201     C*
019600161201     C                   SETOFF                                       45
019700100706     C*
019800100706     C* Effettuo prima lettura (e implicitamente posizionamento) "fuori ciclo"
019900100706     C                   EVAL      x_vapPOE = vapPOE
020000100706     C                   EVAL      x_vapNSR = vapNSR
020100100706     C                   EVAL      x_vapNOR = vapNOR
020200100706     C                   EVAL      x_vapNRV = vapNRV
020300100706     C                   EVAL      ta4n14 = keyORM
020400100706     C     KEYta432_C    CHAIN     tita432c
020500100706     C                   IF        %found(tita432c)
020600100706     C                   DOW       not %eof(tita432c)
020700100706     C     KEYtas30_P    CHAIN     titas30c
020800100706     C                   IF        %found(titas30c)
020900161201     C                   SETON                                        45
021000100706     C                   eval      wTASRSD = TASRSD
021100100706     C                   eval      wTASIND = TASIND
021200100706     C                   eval      wTASCAD = TASCAD
021300100706     C                   eval      wTASLOD = TASLOD
021400100706     C                   eval      wTASPRD = TASPRD
021500100706     C                   eval      wTASNZD = TASNZD
021600100706     C                   eval      wTASNCL = TASNCL
021700100706     C                   eval      wTASPKB = TASPKB
021800100706     C*
021900100706     C* Reperisco i dati del mittente ...
022000100706     C                   exsr      repTAA
022100161128     C*
022200161128     C* Reperisco i dati dei colli originali ...
022300161128     C                   exsr      repAR5
022400161128     C*
022500161128     C* Valorizzazione e scarico buffer di output
022600161128     C                   exsr      valOUT
022700100706     C*
022800100706     C                   ENDIF
022900100706     C     KEYta432_C    READE     tita432c
023000100706     C                   ENDDO
023100100706     C                   ENDIF
023200100706     C*
023300100706     C                   ENDSR
023400100706
023500100706
023600100706
023700100706     C*
023800100706     C     REPTAA        BEGSR
023900100706     C*
024000100706     C                   movel     *blanks       wTIPCCM           4
024100100706     C*
024200100706     C                   move(p)   tasCCM        wTIPCCM
024300100706     C                   if        wTIPCCM = '8888' or
024400100706     C                             wTIPCCM = '9999'
024500100706     C*
024600100706     C                   eval      taaTRC = 'M'
024700100706     C*
024800100706     C* Posizionamento diretto
024900100706     C     KEYtaA30_C    chain     titaA30c
025000100706     C                   if        %found(titaA30c)
025100100706     C                   movel     taaRSC        wTAARSC
025200100706     C                   movel     taaIND        wTAAIND
025300100706     C                   movel     taaCAP        wTAACAP
025400100706     C                   movel     taaLOC        wTAALOC
025500100706     C                   movel     taaPRV        wTAAPRV
025600100706     C                   endif
025700100706     C*
025800100706     C                   else
025900100706     C                   eval      acokut = 1
026000100706     C     KEYaco00_C    chain     cnaco00f
026100100706     C                   if        %found(cnaco00f)
026200100706     C                   movel     acoRAG        wTAARSC
026300100706     C     KEYind00_C    chain     cnind00f
026400100706     C                   if        %found(cnind00f)
026500100706     C                   movel     indVIA        wTAAIND
026600100706     C                   movel     indCAP        wTAACAP
026700100706     C                   movel     indCIT        wTAALOC
026800100706     C                   movel     indPRV        wTAAPRV
026900100706     C                   endif
027000100706     C                   endif
027100100706     C                   endif
027200100706     C*
027300100706     C                   ENDSR
027400161128
027500161128
027600161128
027700161128     C*
027800161128     C     REPAR5        BEGSR
027900161128     C*
028000161128     C                   clear                   DAR5BNB
028100161128     C                   eval      ar5TRD = 'BNB'
028200161128     C     KEYar531_P    chain     fiar531c
028300161128     C                   if        %found(fiar531c)
028400161128     C                   movel     ar5UNI        DAR5BNB
028500161128     C                   if        §ar5BCOR > *zeros
028600161128     C                   eval      wTASNCL = §ar5BCOR                           Colli originali
028700161128     C                   endif
028800161128     C                   endif
028900161128     C*
029000161128     C                   ENDSR
029100161122
029200161122
029300161122
029400161122      /TITLE Reperimento dati da file ORI
029500161122     C     REPORI        BEGSR
029600161122     C*
029700161122     C                   setoff                                       20
029800161122     C                   clear                   tiori000
029900161122     C*
030000161122     C* Solo x orm confermati
030100161122     C                   if        vapPOE > *zeros AND
030200161128     C                             vapNOR > *zeros
030300161122     C*
030400161122     C     KEYori05_C    CHAIN     tiori05l
030500161122     C                   IF        %found(tiori05l) AND vaoATB = *blanks
030600161122     C                   seton                                        20
030700161122     C*
030800161122     C* Reperisco i dati "orignali"
030900161122     C                   eval      wTAARSC = vaoRSR
031000161122     C                   eval      wTAAIND = vaoINR
031100161122     C                   eval      wTAACAP = vaoCAR
031200161122     C                   eval      wTAALOC = vaoLOR
031300161122     C                   eval      wTAAPRV = vaoPRR
031400161122     C                   eval      wTAANAZ = vaoNAR
031500161122     C                   eval      wTASRSD = vaoRSC
031600161122     C                   eval      wTASIND = vaoINC
031700161122     C                   eval      wTASCAD = vaoCAC
031800161122     C                   eval      wTASLOD = vaoLOC
031900161122     C                   eval      wTASPRD = vaoPRC
032000161122     C                   eval      wTASNZD = vaoNAC
032100161122     C                   eval      wVAONCL = vaoNCL
032200161128     C                   eval      wVAONCL = wVAONCL + vaoBNC
032300161122     C*
032400161122     C* Reperisco i dati del mittente ...
032500161122     C                   IF        vaoCRA > 0
032600161122     C                   z-add     vaoCRA        wCRO             10 0
032700161122     C                   exsr      repACR
032800161122     C   30              eval      wTAARSC = acrRSR
032900161122     C   30              eval      wTAAIND = acrINR
033000161122     C   30              eval      wTAACAP = acrCAR
033100161122     C   30              eval      wTAALOC = acrLOR
033200161122     C   30              eval      wTAAPRV = acrPRR
033300161122     C   30              eval      wTAANAZ = acrNAR
033400161122     C                   ENDIF
033500161122     C*
033600161122     C* Reperisco i dati del destinatario ...
033700161122     C                   IF        vaoCRC > 0
033800161122     C                   z-add     vaoCRC        wCRO             10 0
033900161122     C                   exsr      repACR
034000161122     C   30              eval      wTASRSD = acrRSR
034100161122     C   30              eval      wTASIND = acrINR
034200161122     C   30              eval      wTASCAD = acrCAR
034300161122     C   30              eval      wTASLOD = acrLOR
034400161122     C   30              eval      wTASPRD = acrPRR
034500161122     C   30              eval      wTASNZD = acrNAR
034600161122     C                   ENDIF
034700161122     C*
034800161122     C                   ENDIF
034900161122     C*
035000161122     C                   endif
035100161122     C*
035200161122     C                   ENDSR
035300161122
035400161122
035500161122
035600161122     C*
035700161122     C     REPACR        BEGSR
035800161122     C*
035900161122     C                   setoff                                       30
036000161122     C*
036100161122     C* Posizionamento diretto
036200161122     C     wCRO          chain     fnacr01l
036300161122     C                   if        %found(fnacr01l)
036400161122     C                   seton                                        30
036500161122     C                   endif
036600161122     C*
036700161122     C                   ENDSR
036800161128
036900161128
037000161128
037100161128     C*
037200161128     C     VALOUT        BEGSR
037300161128     C*
037400161128     C* Incremento contatore di bolla abbinarta al ORM corrente
037500161201     C   45              add       1             wPRGBOL
037600161128     C*
037700161128     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
037800161128     C  N60              EVAL      wPKB = %trim(%editw(wTASPKB:'    0 , '))
037900161128     C   60              EVAL      wPKB = %trim(%editw(wTASPKB:'    0 . '))
038000161128     C*
038100161128     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
038200161128     C                   IF        %subst(prmppt:2:1) = 'D'
038300161128     C                   EVAL      VAWDTA  =
038400161128     C                                     %trim(%editc(wPRGBOL:'4'))+wSepCampi+
038500161128     C                                         '"'+%trim(VAPIDC)+'"' +wSepCampi+
038600161128     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
038700161128     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
038800161128     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
038900161128     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
039000161128     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
039100161128     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
039200161128     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
039300161128     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
039400161128     C                             %trim(%editw(VAPDAE:'    /  /  '))+wSepCampi+
039500161128     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
039600161128     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
039700161128     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
039800161128     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
039900161128     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
040000161128     C                                         '"'+%trim(VAPNOT)+'"'
040100161128     C                   ELSE
040200161128     C*
040300161128     C                   EVAL      VAWDTA  =
040400161128     C                                     %trim(%editc(wPRGBOL:'4'))+wSepCampi+
040500161128     C                                         '"'+%trim(VAPIDC)+'"' +wSepCampi+
040600161128     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
040700161128     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
040800161128     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
040900161128     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
041000161128     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
041100161128     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
041200161128     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
041300161128     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
041400161128     C                                      %trim(%editc(VAPDAE:'Q'))+wSepCampi+
041500161128     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
041600161128     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
041700161128     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
041800161128     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
041900161128     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
042000161128     C                                         '"'+%trim(VAPNOT)+'"'
042100161128     C*
042200161128     C                   ENDIF
042300161128     C*
042400161128     C* Dati aggiuntivi...
042500161128     C                   EVAL      VAWDTA  = %trim(VAWDTA)           +wSepCampi+
042600161128     C                                        '"'+%trim(wTAARSC)+'"' +wSepCampi+
042700161128     C                                        '"'+%trim(wTAAIND)+'"' +wSepCampi+
042800161128     C                                        '"'+%trim(wTAACAP)+'"' +wSepCampi+
042900161128     C                                        '"'+%trim(wTAALOC)+'"' +wSepCampi+
043000161128     C                                        '"'+%trim(wTAAPRV)+'"' +wSepCampi+
043100161128     C                                        '"'+%trim(wTAANAZ)+'"' +wSepCampi+
043200161128     C                                        '"'+%trim(wTASRSD)+'"' +wSepCampi+
043300161128     C                                        '"'+%trim(wTASIND)+'"' +wSepCampi+
043400161128     C                                        '"'+%trim(wTASCAD)+'"' +wSepCampi+
043500161128     C                                        '"'+%trim(wTASLOD)+'"' +wSepCampi+
043600161128     C                                        '"'+%trim(wTASPRD)+'"' +wSepCampi+
043700161128     C                                        '"'+%trim(wTASNZD)+'"' +wSepCampi+
043800161128     C                                     %trim(%editc(wVAONCL:'Q'))+wSepCampi+
043900161128     C                                     %trim(%editc(wTASNCL:'Q'))+wSepCampi+
044000161128     C                                               %trim(wPKB)
044100161128     C*
044200161128     C                   WRITE     TIVAW000
044300161128     C                   seton                                        35
044400161128     C*
044500161128     C                   ENDSR
044600100706
044700991027
044800100706
044900991027      /TITLE Operazioni iniziali.
045000991027     C     *inzsr        BEGSR
045100991027     C*
045200991027     C     *ENTRY        PLIST
045300991027     C                   parm                    prmppt
045400991027     C     wrkesito      parm      wrkesito      prmesito
045500100706     C*
045600100706     C                   eval      acokcc = 151
045700100706     C*
045800100706     C     KEYtas30_P    KLIST
045900100706     C                   KFLD                    ta4AAS
046000100706     C                   KFLD                    ta4LNP
046100100706     C                   KFLD                    ta4NRS
046200100706     C                   KFLD                    ta4NSP
046300100706     C*
046400100706     C     KEYta432_C    KLIST
046500100706     C                   KFLD                    ta4N14
046600100706     C*
046700100706     C     KEYtaA30_C    KLIST
046800100706     C                   KFLD                    tasaas
046900100706     C                   KFLD                    taslnp
047000100706     C                   KFLD                    tasnrs
047100100706     C                   KFLD                    tasnsp
047200100706     C                   KFLD                    taatrc
047300161128     C*
047400161128     C     KEYar531_P    KLIST
047500161128     C                   KFLD                    tasaas
047600161128     C                   KFLD                    taslnp
047700161128     C                   KFLD                    tasnrs
047800161128     C                   KFLD                    tasnsp
047900161128     C                   KFLD                    ar5TRD
048000100706     C*
048100100706     C     KEYaco00_C    KLIST
048200100706     C                   KFLD                    acokut
048300100706     C                   KFLD                    acokcc
048400100706     C                   KFLD                    tasccm
048500100706     C*
048600100706     C     KEYind00_C    KLIST
048700100706     C                   KFLD                    acokut
048800100706     C                   KFLD                    acokcc
048900100706     C                   KFLD                    tasccm
049000161122     C*
049100161122     C     KEYori05_C    KLIST
049200161122     C                   KFLD                    vapPOE
049300161122     C                   KFLD                    vapNSR
049400161122     C                   KFLD                    vapNOR
049500161122     C                   KFLD                    vapNRV
049600991027     C*
049700991027     C                   ENDSR
