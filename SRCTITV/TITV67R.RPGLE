000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200161206     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300130227
000400130227      * VALORI STBPRS (PROVENIENZA STATO)
000500130227      * '1' = VAB (ACCETTAZIONE BOLLE)
000600130227      * '2' = EVB (EVENTI)
000700130227      * '3' = GCP (GIACENZE)
000800130227      * '4' = VAC (STATI CONSEGNA)
000900130227      * '5' = DCT (DANNI)
001000130227      * '6' = VAP (ORM)
001100130227      * '7' = PCT (PDA)
001200160223      * '8' = DETTAGLI COLLI
001300130227      *
001400130227
001500991027
001600090320     FTNTBE01L  UF A E           K DISK    commit
001700111031     FTITAS30C  IF   E           K DISK
001800041015     FFNDCT02L  IF   E           K DISK
001900120424     FFIAR531C  IF   E           K DISK
002000031014     FFNEVB01L  IF   E           K DISK
002100110929     FFNEVB04L  IF   E           K DISK    rename(FNEVB000:FNEVB004)
002200111031     F                                     prefix(evb4_)
002300111103     FFNARB01L  IF   E           K DISK    usropn
002400111103     F                                     extfile(LibFileARB01)
002500160211     FFIPCT03L  IF   E           K DISK    usropn
002600160211     F                                     extfile(LibFilePCT03)
002700050310     FTIGCP51L  IF   E           K DISK
002800100806     FFNORM01L  IF   E           K DISK
002900080924     FTIVGDTMP  UF   E             DISK
003000121008     FTISTB04L  UF   E           K DISK    prefix(i_) rename(tistb000:tistb004)
003100121008     F                                     commit
003200121008     FTISTB06L  UF A E             DISK    commit
003300160223     FTISTBD0F  UF A E             DISK    commit
003400031103
003500031103     D****
003600031103     D* DEFINIZIONE DS ESTERNE
003700031103     D****
003800031103     D DSSTB         E DS
003900120424     D DAR5GEN       E DS
004000160223     D DSTBDNPG5     E DS
004100041015     D fndctds       E DS                  EXTNAME(fndct00f)
004200041015     D fndctds_w     E DS                  EXTNAME(fndct00f) PREFIX(w_)
004300080924     D fnvac00t      E DS
004400110927     D fnvag00t      E DS
004500090320     D fnvab00t      E DS
004600100806     D fnvap00t      E DS
004700130227     D fipctds       E DS                  EXTNAME(fipct00f)
004800130227     D fipctds_w     E DS                  EXTNAME(fipct00f) PREFIX(w_)
004900130227     D fipctcetds    E DS
005000121008     D tistbds       E DS                  EXTNAME(tistb00f)
005100121008     D tistbds_i     E DS                  EXTNAME(tistb00f) PREFIX(i_)
005200081112
005300081112     D****
005400081112     D* DS RIDEFINIZIONE FLAG OPERATIVI FILE TISTB
005500081112     D****
005600081112     D DS_STBFOP       DS
005700081112     D  STB_BOR                       1    inz
005800111031     D  STB_CCA                       1    inz
005900111031     D  STB_FILLER                   18    inz
006000030924
006100031103     D****
006200031103     D* VARIABILI RELATIVE ALLA TRADUZIONE
006300031103     D****
006400030924     D prmppt          S             50
006500030924     D prmesito        S              1
006600030924     D wrkesito        S                   like(prmesito)
006700030924
006800031103     D****
006900031103     D* VARIABILI RIFERITE AL DATA-BASE
007000031103     D****
007100121010     D wDAS            S                   like(stbDAS) inz
007200121010     D kORS            S                   like(stbORS) inz
007300121010     D wORS            S              6  0              inz
007400121010     D wTIS            S                   like(stbTIS) inz
007500121010     D wPRS            S                   like(stbPRS) inz
007600121010     D wCOS            S                   like(stbCOS) inz
007700121010     D wFRG            S                   like(gcpFRG) inz
007800121010     D wAAS            S                   like(stbAAS) inz
007900121010     D wLNP            S                   like(stbLNP) inz
008000121010     D wLNA            S                   like(stbLNP) inz
008100121010     D wNRS            S                   like(stbNRS) inz
008200121010     D wNSP            S                   like(stbNSP) inz
008300121010     D wRMN            S                   like(stbRMN) inz
008400121010     D wRMA            S                   like(stbRMA) inz
008500121010     D wKSC            S                   like(stbKSC) inz
008600121010     D wDSP            S                   like(stbDSP) inz
008700121010     D wNDC            S                   like(tasNDC) inz
008800170926     D wTC1            S                   like(vacTC1) inz
008900170926     D wTC2            S                   like(vacTC2) inz
009000170926     D wDCR            S                   like(vacDCR) inz
009100170926     D wHCR            S                   like(vacHCR) inz
009200160211     D wPCTCMC         S                   like(§PCTCMC) inz
009300031103
009400031103     D****
009500031103     D* VARIABILI DI WRK
009600031103     D****
009700031103     D wDESTATO        S             70
009800130227
009900111103
010000111103     D LibFileARB01    s             21A   inz
010100160211     D LibFilePCT03    s             21A   inz
010200111103     D currSysNeta     s              8A   inz('*NULL')
010300111103
010400031103
010500031103     D****
010600031103     D* DS RIDEFINIZIONE PARAMETRI TRADUZIONE RICEVUTI IN INPUT
010700031103     D****
010800031103     D DSPPT           DS
010900031103     D  w§PPTTIP                      2
011000031104     D  w§PPTKSU                      7  0
011100121008     D  w§PPTUPD                      1
011200130227     D  w§PPTPDA                      1
011300160223     D  w§PPTCOLLI                    1
011400081112
011500081112
011600081112     D*------------------
011700081112     D* LINKING A DEFINIZIONI ESTERNE
011800081112     D*------------------
011900081112     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
012000090430     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
012100111103     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
012200111103     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
012300160223     D/COPY GAITRASRC/SRCPROTOPI,UBCOL00R
012400031031
012500081112
012600081112
012700031031     C***********
012800030924     C* MAIN/
012900031031     C***********
013000111103     C*
013100111103     C* Reperisco il sistema AS/400 corrente
013200111103     C                   callp     UBRTVNETA_Rtv(currSysNeta)
013300111103     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
013400111103     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
013500160211     C                   eval      LibFilePCT03 = 'FILTRA201/FIPCT03L'
013600111103     C                   else
013700111103     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
013800160211     C                   eval      LibFilePCT03 = 'FILTRAPRD/FIPCT03L'
013900111103     C                   endif
014000111103     C*
014100111103     C* Apertura file "overraidati"
014200111103     C                   if        not %open(FNARB01L)
014300111103     C                   open      FNARB01L
014400111103     C                   endif
014500160211     C                   if        not %open(FIPCT03L)
014600160211     C                   open      FIPCT03L
014700130227     C                   endif
014800031103     C*
014900031103     C* Eseguo routine d elaborazione
015000031103     C                   EXSR      verticalizza
015100111103     C*
015200111103     C* Chiusura file "overraidati"
015300111103     C                   if        %open(FNARB01L)
015400111103     C                   close     FNARB01L
015500111103     C                   endif
015600160211     C                   if        %open(FIPCT03L)
015700160211     C                   close     FIPCT03L
015800130227     C                   endif
015900090323     C*
016000090323     C* Sancisco il commit
016100090323     C                   COMMIT
016200031103     C*
016300921023     C                   SETON                                        LR
016400031103
016500031103
016600031103
016700031103     C     verticalizza  BEGSR
016800031103     C*
016900031103     C                   IF        w§PPTTIP = 'BT'
017000031103     C                   EXSR      verticalizBT
017100031103     C                   ENDIF
017200031103     C                   IF        w§PPTTIP = 'VC'
017300110926     C                   EXSR      verticalizALL
017400031103     C                   ENDIF
017500110926     C                   IF        w§PPTTIP = 'VG'
017600110926     C                   EXSR      verticalizALL
017700110926     C                   ENDIF
017800100806     C                   IF        w§PPTTIP = 'VP'
017900100806     C                   EXSR      verticalizVP
018000100806     C                   ENDIF
018100130227     C                   IF        w§PPTTIP = 'DA'
018200130227     C                   EXSR      verticalizDA
018300130227     C                   ENDIF
018400031103     C*
018500031103     C                   ENDSR
018600031103
018700031103
018800031103
018900031103     C     verticalizBT  BEGSR
019000031103     C*
019100090320     C                   READ      TIVGDTMP
019200161201     C                   DOW       not %eof(TIVGDTMP)
019300120727     C*
019400120727     C* Ignoro record "*VOID"
019500120727     C                   IF        %trim(vgdDTA) = '*VOID'
019600120727     C                   ELSE
019700090320     C*
019800090320     C                   EVAL      fnvab00t = vgdDTA
019900031106     C*
020000031106     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
020100031106     C                   EVAL      wAAS = vabAAS
020200031106     C                   EVAL      wLNP = vabLNP
020300110927     C                   EVAL      wLNA = vabLNA
020400031106     C                   EVAL      wNRS = vabNRS
020500031106     C                   EVAL      wNSP = vabNSP
020600050224     C                   EVAL      wRMN = vabRMN
020700050224     C                   EVAL      wRMA = vabRMA
020800031106     C                   EVAL      wKSC = vabCCM
020900111031     C*
021000111031     C* Valorizzo subito la data della bolla
021100111031     C                   EVAL      wDSP = vabAAS*10000+vabMGS
021200031103     C*
021300031103     C* X ogni bolla verticalizzo lo stato (da file FNVAB00T) nel file TISTB
021400050202     C                   EXSR      EXEVAB
021500160223     C*
021600161130     C* X ogni bolla verticalizzo le informazioni relative ai singoli colli
021700160223     C                   EXSR      EXECOLLI
021800120727     C*
021900120727     C                   ENDIF
022000031103     C*
022100090320     C                   DELETE    TIVGD000
022200090320     C*
022300090320     C                   READ      TIVGDTMP
022400031103     C                   ENDDO
022500031103     C*
022600031103     C                   EVAL      wrkesito = '0'
022700031103     C*
022800031103     C                   ENDSR
022900991027
023000030924
023100030924
023200110926     C     verticalizALL BEGSR
023300991027     C*
023400080924     C                   READ      TIVGDTMP
023500080924     C                   DOW       not %eof(TIVGDTMP)
023600120727     C*
023700120727     C* Ignoro record "*VOID"
023800120727     C                   IF        %trim(vgdDTA) = '*VOID'
023900120727     C                   ELSE
024000080924     C*
024100110927     C                   SELECT
024200110927     C                   WHEN      w§PPTTIP = 'VC'
024300110927     C                   EVAL      fnvac00t = vgdDTA
024400110927     C*
024500110927     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
024600110927     C                   EVAL      wAAS = vacAAS
024700110927     C                   EVAL      wLNP = vacLNP
024800110927     C                   EVAL      wLNA = vacLNA
024900110927     C                   EVAL      wNRS = vacNRS
025000110927     C                   EVAL      wNSP = vacNSP
025100110927     C                   EVAL      wRMN = vacRMN
025200110927     C                   EVAL      wRMA = vacRMA
025300110927     C                   EVAL      wKSC = vacCCM
025400170926     C                   EVAL      wTC1 = vacTC1
025500170926     C                   EVAL      wTC2 = vacTC2
025600170926     C                   EVAL      wDCR = vacDCR
025700170926     C                   EVAL      wHCR = vacHCR
025800111031     C*
025900111031     C* Valorizzo subito la data della bolla
026000111031     C                   EVAL      wDSP = vacAAS*10000+vacMGS
026100120703     C*
026200120703     C* Aggancio sempre anche la bolla (in arrivo o eventualmente in sede)
026300120703     C                   EVAL      wNDC = *zeros
026400120703     C     KEYarb01C     CHAIN     fnarb01l
026500120703     C                   IF        %found(fnarb01l)
026600120703     C                   EVAL      wNDC = arbNDC
026700161118     C                   EVAL      wLNA = arbLNA
026800161118     C                   EVAL      wRMN = arbRMN
026900161118     C                   EVAL      wRMA = arbRMA
027000170926     C                   EVAL      wTC1 = arbTC1
027100170926     C                   EVAL      wTC2 = arbTC2
027200170926     C                   EVAL      wDCR = arbDCR
027300170926     C                   EVAL      wHCR = arbHCR
027400161123     C                   IF        wKSC = *zeros
027500161123     C                   IF        arbCCM > *zeros
027600161118     C                   EVAL      wKSC = arbCCM
027700161123     C                   ELSE
027800161123     C                   EVAL      wKSC = arbKSC
027900161123     C                   ENDIF
028000161123     C                   ENDIF
028100161118     C                   EVAL      wDSP = arbAAS*10000+arbMGS
028200120703     C                   ELSE
028300120703     C     KEYtas30P     CHAIN     TITAS30C
028400120703     C                   IF        %found(TITAS30C)
028500120703     C                   EVAL      wNDC = tasNDC
028600161118     C                   EVAL      wLNA = tasLNA
028700161118     C                   EVAL      wRMN = tasRMN
028800170926     C                   EVAL      wTC1 = tasFTC
028900170926     C                   EVAL      wTC2 = tasTC2
029000170926     C                   EVAL      wDCR = tasDCR
029100170926     C                   EVAL      wHCR = tasHCR
029200161123     C                   IF        wKSC = *zeros
029300161123     C                   IF        tasCCM > *zeros
029400161123     C                   EVAL      wKSC = tasCCM
029500161123     C                   ELSE
029600161123     C                   EVAL      wKSC = tasKSC
029700161123     C                   ENDIF
029800161123     C                   ENDIF
029900161118     C                   EVAL      wDSP = tasAAS*10000+tasMGS
030000120703     C                   ENDIF
030100120703     C                   ENDIF
030200120424     C*
030300120424     C* X ogni bolla verticalizzo i dati da file FIAR500F - trk GEN
030400120424     C                   EXSR      EXEAR5GEN
030500110927     C*
030600110927     C* X ogni bolla verticalizzo gli eventi (da file FNEVB01L) nel file TISTB
030700110927     C                   EXSR      EXEEVB
030800110927     C*
030900110927     C* X ogni bolla verticalizzo le giacenze (da file TIGCP51L) nel file TISTB
031000110927     C                   EXSR      EXEGCP
031100110927     C*
031200110927     C* X ogni bolla verticalizzo i danni (da file FNDCT00T) nel file TISTB
031300110927     C                   EXSR      EXEDCT
031400110927     C*
031500110927     C* X ogni bolla verticalizzo lo stato (da file FNVAC00T) nel file TISTB
031600110927     C                   EXSR      EXEVAC
031700160223     C*
031800160223     C* X ogni bolla verticalizzo le informazioni relative ai singoli colli
031900160223     C                   EXSR      EXECOLLI
032000170926     C*
032100170926     C* X ogni bolla verticalizzo le informazioni del appuntamento
032200170926     C                   EXSR      EXEAPP
032300110927     C*
032400110927     C                   WHEN      w§PPTTIP = 'VG'
032500110927     C                   EVAL      fnvag00t = vgdDTA
032600110927     C*
032700110927     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
032800110927     C                   EVAL      wAAS = vagAAS
032900110927     C                   EVAL      wLNP = vagLNP
033000110927     C                   EVAL      wLNA = vagLNA
033100110927     C                   EVAL      wNRS = vagNRS
033200110927     C                   EVAL      wNSP = vagNSP
033300110927     C                   EVAL      wRMN = vagRMN
033400110927     C                   EVAL      wRMA = vagRMA
033500111020     C                   EVAL      wKSC = vagSCM
033600111031     C*
033700111031     C* Valorizzo subito la data della bolla
033800111031     C     KEYtas30P     CHAIN     TITAS30C
033900111031     C                   IF        %found(TITAS30C)
034000111031     C                   EVAL      wDSP = tasAAS*10000+tasMGS
034100111031     C                   ENDIF
034200110927     C*
034300110927     C* X ogni bolla verticalizzo gli eventi (da file FNEVB01L) nel file TISTB
034400110927     C                   EXSR      EXEEVB
034500110927     C*
034600110927     C* X ogni bolla verticalizzo le giacenze (da file TIGCP51L) nel file TISTB
034700110927     C                   EXSR      EXEGCP
034800170926     C*
034900170926     C* X ogni bolla verticalizzo le informazioni del appuntamento
035000170926     C                   EXSR      EXEAPP
035100110927     C*
035200110927     C                   ENDSL
035300120727     C*
035400120727     C                   ENDIF
035500031031     C*
035600080924     C                   DELETE    TIVGD000
035700031031     C*
035800080924     C                   READ      TIVGDTMP
035900031031     C                   ENDDO
036000031031     C*
036100031103     C                   EVAL      wrkesito = '0'
036200031031     C*
036300031031     C                   ENDSR
036400100806
036500100806
036600100806
036700100806     C     verticalizVP  BEGSR
036800100806     C*
036900100806     C                   READ      TIVGDTMP
037000100806     C                   DOW       not %eof(TIVGDTMP)
037100120727     C*
037200120727     C* Ignoro record "*VOID"
037300120727     C                   IF        %trim(vgdDTA) = '*VOID'
037400120727     C                   ELSE
037500100806     C*
037600100806     C                   EVAL      fnvap00t = vgdDTA
037700100806     C*
037800100806     C* Valorizzo subito la data ritiro orm
037900100806     C     KEYorm01      CHAIN     fnorm01l
038000100806     C                   IF        %found(fnorm01l)
038100100806     C                   EVAL      wDSP = ormDAR
038200100806     C*
038300100806     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
038400100806     C                   MOVEL     ormDAR        wAAS
038500100806     C                   EVAL      wLNP = vapPOE
038600100806     C                   EVAL      wNRS = vapNSR
038700100806     C                   EVAL      wNSP = vapNOR
038800100806     C                   EVAL      wRMN = %dec(%editc(vapPOE:'X')+
038900100806     C                                         %editc(vapNSR:'X')+
039000100806     C                                         %editc(vapNOR:'X')+
039100100806     C                                         %editc(vapNRV:'X'):14:0)
039200100806     C                   EVAL      wRMA = vapRFA
039300100806     C                   MOVEL     ormCOR        wKSC
039400100806     C*
039500100806     C* X ogni orm verticalizzo lo stato (da file FNVAP00T) nel file TISTB
039600100806     C                   EXSR      EXEVAP
039700100806     C
039800100806     C                   ENDIF
039900100806     C*
040000120727     C                   ENDIF
040100120727     C*
040200100806     C                   DELETE    TIVGD000
040300100806     C*
040400100806     C                   READ      TIVGDTMP
040500100806     C                   ENDDO
040600100806     C*
040700100806     C                   EVAL      wrkesito = '0'
040800100806     C*
040900100806     C                   ENDSR
041000130227
041100130227
041200130227
041300130227     C     verticalizDA  BEGSR
041400130227     C*
041500130227     C                   READ      TIVGDTMP
041600130227     C                   DOW       not %eof(TIVGDTMP)
041700130227     C*
041800130227     C* Ignoro record "*VOID"
041900130227     C                   IF        %trim(vgdDTA) = '*VOID'
042000130227     C                   ELSE
042100130227     C*
042200130227     C                   EVAL      fnvac00t = vgdDTA
042300130227     C*
042400130227     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
042500130227     C                   EVAL      wAAS = vacAAS
042600130227     C                   EVAL      wLNP = vacLNP
042700130227     C                   EVAL      wLNA = vacLNA
042800130227     C                   EVAL      wNRS = vacNRS
042900130227     C                   EVAL      wNSP = vacNSP
043000130227     C                   EVAL      wRMN = vacRMN
043100130227     C                   EVAL      wRMA = vacRMA
043200130227     C                   EVAL      wKSC = vacCCM
043300130227     C*
043400130227     C* Valorizzo subito la data della bolla
043500130227     C                   EVAL      wDSP = vacAAS*10000+vacMGS
043600130227     C*
043700160223     C* X ogni bolla verticalizzo lo stato (da file FIPCT03L) nel file TISTB
043800130227     C                   EXSR      EXEPCT
043900160223     C*
044000160223     C* X ogni bolla verticalizzo le informazioni relative ai singoli colli
044100160223     C                   EXSR      EXECOLLI
044200130227     C*
044300130227     C                   ENDIF
044400130227     C*
044500130227     C                   DELETE    TIVGD000
044600130227     C*
044700130227     C                   READ      TIVGDTMP
044800130227     C                   ENDDO
044900130227     C*
045000130227     C                   EVAL      wrkesito = '0'
045100130227     C*
045200130227     C                   ENDSR
045300031103
045400031103
045500031103
045600041014     C     EXEVAB        BEGSR
045700111031     C*
045800111031     C* Inizializzo DS flag operativi
045900111031     C                   CLEAR                   DS_STBFOP
046000040322     C*
046100040322     C* Imposto subito alcuni valori
046200040322     C                   EVAL      wDAS = vabAAS*10000+vabMGS
046300131014     C***                EVAL      wORS = *zeros
046400131024     C                   EVAL      wORS = 210000
046500031103     C*
046600031103     C* Dal FNVAB reperisco solo gli stati di presa in carico bolla, x cui:
046700031103     C*
046800031103     C****
046900031103     C* stato INCARICO1 (bolla presa in carico; con LNP=LNA)
047000031103     C****
047100031103     C* - condizioni:
047200031103     C*   LNP = LNA
047300031103     C*
047400031103     C* Verifico le condizione che soddisfano lo stato INCARICO1
047500031103     C                   IF        vabLNP = vabLNA
047600031103     C                   EVAL      wTIS = '9'
047700031103     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
047800031103     C                                        '(LNP=LNA)'
047900041014     C                   EVAL      wPRS = '1'
048000031103     C                   EVAL      wCOS = 'INCARICO1'
048100090430     C*
048200031103     C                   CLEAR                   tistb000
048300121008     C                   CLEAR                   tistbds
048400031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
048500031103     C                   EVAL      stbSTS = '1'
048600031103     C                   EXSR      WRIREC
048700031103     C                   ENDIF
048800031103     C****
048900031103     C*
049000031103     C****
049100031103     C* stato INCARICO2 (bolla presa in carico; con LNP<>LNA)
049200031103     C****
049300031103     C* - condizioni:
049400031103     C*   LNP <> LNA
049500031103     C*
049600031103     C* Verifico le condizione che soddisfano lo stato INCARICO2
049700031106     C                   IF        vabLNP <> vabLNA
049800031103     C                   EVAL      wTIS = '9'
049900031103     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
050000031103     C                                        '(LNP<>LNA)'
050100041014     C                   EVAL      wPRS = '1'
050200031103     C                   EVAL      wCOS = 'INCARICO2'
050300031103     C*
050400031103     C* - inizializzo il buffer del file stati bolle (TISTB)
050500031103     C                   CLEAR                   tistb000
050600121008     C                   CLEAR                   tistbds
050700031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
050800031103     C                   EVAL      stbSTS = '1'
050900031103     C                   EXSR      WRIREC
051000031103     C                   ENDIF
051100031103     C****
051200031103     C*
051300031103     C                   ENDSR
051400031031
051500031031
051600031031
051700031031     C     EXEEVB        BEGSR
051800031031     C*
051900031031     C* Reperisco gli eventi relativi alla bolla corrente
052000031014     C     KEYevb01P     SETLL     fnevb01l
052100031014     C                   IF        %equal(fnevb01l)
052200031014     C     KEYevb01P     READE     fnevb01l
052300031031     C                   DOW       not %eof(fnevb01l)
052400111103     C*
052500111103     C                   SETOFF                                       50
052600031031     C*
052700031031     C* Se evento NN annullato lo considero
052800031031     C                   IF        evbATB = *blanks
052900111031     C*
053000111031     C* Inizializzo DS flag operativi
053100111031     C                   CLEAR                   DS_STBFOP
053200111031     C*
053300111031     C* Verifico se trattasi d evento derivato da bolla figlia
053400111031     C*
053500111031     C* Chiamata metodo GetLblTyp
053600111031     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
053700111031     C                                                wAAS
053800111031     C                                               :wLNP
053900111031     C                                               :wNRS
054000111031     C                                               :wNSP
054100111031     C                                               :pOutLblTyp
054200111031     C                                               :pOutAnnoBO
054300111031     C                                               :pOutLineaParBO
054400111031     C                                               :pOutSerieBO
054500111031     C                                               :pOutNumSpedBO
054600111031     C                                               :pOutDcmBO
054700111031     C                                               :pOutCcaBO
054800111031     C                                               :pOutRblBO))
054900111031     C*
055000111031     C                   if        wEsito1 = '0'
055100111103     C*
055200111103     C* Se trattasi d reso della bolla originale => forzo inserimento stato di reso
055300111103     C                   if        pOutLblTyp = 'O' AND
055400111103     C                             pOutCcaBO = '2'
055500111103     C                   EVAL      STB_BOR = 'O'                                * bolla orig.
055600111103     C*
055700111103     C* Leggo la bolla in arrivo (no SEDE !!!)
055800111103     C     KEYarb01C     chain     fnarb01l
055900111103     C                   if        %found(fnarb01l)
056000111103     C                   EVAL      wTIS = '1'
056100111103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON RESO'
056200111103     C                   EVAL      wDAS = arbDCM
056300121010     C                   EVAL      wORS = arbHMC*100
056400111103     C                   EVAL      wPRS = '4'
056500111103     C                   EVAL      wCOS = 'RESO'
056600111103     C*
056700111103     C* - inizializzo il buffer del file stati bolle (TISTB)
056800111103     C                   CLEAR                   tistb000
056900121008     C                   CLEAR                   tistbds
057000111103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
057100111103     C                   EVAL      stbSTS = '9'
057200111103     C                   EXSR      WRIREC
057300111103     C*
057400111103     C                   else
057500111103     C                   SETON                                        50
057600111103     C                   endif
057700111103     C                   endif
057800111103     C*
057900111031     C                   if        pOutLblTyp = 'F'
058000111031     C                   EVAL      STB_BOR = 'F'                                * bolla Figlia
058100111031     C                   else
058200111103     C                   EVAL      STB_BOR = 'O'                                * bolla orig.
058300111031     C*
058400111031     C* Chiamata metodo GetLastChild
058500111031     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
058600111031     C                                                wAAS
058700111031     C                                               :wLNP
058800111031     C                                               :wNRS
058900111031     C                                               :wNSP
059000111031     C                                               :pOutAnnoFI
059100111031     C                                               :pOutLineaParFI
059200111031     C                                               :pOutSerieFI
059300111031     C                                               :pOutNumSpedFI
059400111031     C                                               :pOutDcmFI
059500111031     C                                               :pOutCcaFI))
059600111031     C                   if        wEsito2 = '0'
059700111031     C*
059800111031     C* Aggancio l'evento relativo della bolla figlia
059900111031     C                   EVAL      evb4_EVBAAS = pOutAnnoFI
060000111031     C                   EVAL      evb4_EVBLNP = pOutLineaParFI
060100111031     C                   EVAL      evb4_EVBNRS = pOutSerieFI
060200111031     C                   EVAL      evb4_EVBNSP = pOutNumSpedFI
060300111031     C                   EVAL      evb4_EVBCEV = evbCEV
060400111031     C                   EVAL      evb4_EVBDEV = evbDEV
060500111031     C*
060600111031     C     KEYevb04P     CHAIN     fnevb04l
060700111031     C                   IF        %found(fnevb04l)
060800111031     C                   IF        evb4_EVBNOT = evbNOT
060900111031     C                   EVAL      STB_BOR = 'F'                                * bolla Figlia
061000111031     C                   EVAL      STB_CCA = pOutCcaBO                          * CCA bolla orig.
061100111031     C                   ENDIF
061200111031     C                   ENDIF
061300111031     C*
061400111031     C                   endif
061500111031     C                   endif
061600111031     C                   endif
061700031031     C*
061800050224     C* Costruisco la chiave sul file TISTB04L
061900121019     C***                IF        evbDEV*10000+evbHEV >
062000121019     C***                          evbDTV*10000+evbORV
062100121012     C                   EVAL      wDAS = evbDEV
062200121019     C                   EVAL      wORS = evbHEV*100
062300121019     C***                ELSE
062400121019     C***                EVAL      wDAS = evbDTV
062500121019     C***                EVAL      wORS = evbORV
062600121019     C***                ENDIF
062700031103     C                   EVAL      wTIS = '1'
062800031103     C                   EVAL      wPRS = '2'
062900031103     C                   EVAL      wCOS = evbCEV
063000031031     C*
063100031031     C* - inizializzo il buffer del file stati bolle (TISTB)
063200031031     C                   CLEAR                   tistb000
063300121008     C                   CLEAR                   tistbds
063400031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
063500031031     C                   EVAL      stbSTS = '2'
063600031103     C                   EXSR      WRIREC
063700031031     C                   ENDIF
063800031031     C*
063900031031     C* Proseguo la lettura degli eventi x la bolla corrente
064000031014     C     KEYevb01P     READE     fnevb01l
064100031014     C                   ENDDO
064200031031     C                   ENDIF
064300991027     C*
064400910830     C                   ENDSR
064500031031
064600031031
064700031031
064800031031     C     EXEGCP        BEGSR
064900031031     C*
065000050310     C* Costruisco la chiave sul file TIGCP51L
065100031031     C                   EVAL      wFRG = *zeros
065200031031     C*
065300031104     C* Reperisco le giacenze relative alla bolla corrente
065400050310     C     KEYgcp51      SETLL     tigcp51l
065500050310     C                   IF        %equal(tigcp51l)
065600050310     C     KEYgcp51      READE     tigcp51l
065700050310     C                   DOW       not %eof(tigcp51l)
065800110929     C*
065900121010     C                   EVAL      wORS = 110000
066000031031     C*
066100031104     C* Se giacenza NN annullata la considero
066200031031     C                   IF        gcpATB = *blanks
066300111031     C*
066400111031     C* Inizializzo DS flag operativi
066500111031     C                   CLEAR                   DS_STBFOP
066600110929     C*
066700110929     C* Aggancio l'evento di apertura giacenza correlato
066800111031     C                   EVAL      evb4_EVBAAS = gcpAAS
066900111031     C                   EVAL      evb4_EVBLNP = gcpLNP
067000111031     C                   EVAL      evb4_EVBNRS = gcpNRS
067100111031     C                   EVAL      evb4_EVBNSP = gcpNSP
067200111031     C                   EVAL      evb4_EVBCEV = gcpCMC
067300111031     C                   EVAL      evb4_EVBDEV = gcpDUR
067400111031     C*
067500110929     C     KEYevb04P     CHAIN     fnevb04l
067600110929     C                   IF        %found(fnevb04l)
067700121008     C***                EVAL      wORS = evb4_evbHEV + 1
067800121019     C***                EVAL      wORS = %dec(%time(evbORV)+%minutes(1))
067900121019     C                   EVAL      wORS = %dec(%time(evbHEV*100)+%minutes(1))
068000110929     C                   ENDIF
068100121009     C*
068200121010     C                   Z-ADD     wORS          wORSgcp           6 0
068300031031     C*
068400050310     C* Effettuo considerazioni sul file TIGCP x detrminare il codice stato da attribuire
068500031103     C* alla bolla:
068600031031     C*
068700031031     C****
068800031103     C* stato GIACNODIS1 (giacenza aperta ma in attesa d disposizioni del cliente; con LNP=LNA)
068900031031     C****
069000031031     C* - condizioni:
069100050310     C*   data aper./riaper. giacenza > 0 AND
069200031031     C*   data disposizioni mittente (gcpDDM) = 0 AND
069300050614     C*   fase giacenza (gcpFAS) < 30 (ex = 020) AND
069400031031     C*   LNP = LNA
069500031103     C                   EVAL      wDAS = gcpDUR
069600031031     C*
069700031031     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
069800050614     C                   IF        wDAS   > *zeros AND
069900050614     C                             gcpDDM = *zeros AND
070000050614     C                             gcpFAS < 030    AND
070100110927     C                             wLNP = wLNA
070200031103     C                   EVAL      wTIS = '9'
070300031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA MA IN ATTESA ' +
070400031103     C                                        'DI DISPOSIZIONI DEL CLIENTE '  +
070500031103     C                                        '(LNP=LNA)'
070600031103     C                   EVAL      wPRS = '3'
070700031103     C                   EVAL      wCOS = 'GIACNODIS1'
070800121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
070900031031     C*
071000031031     C* - inizializzo il buffer del file stati bolle (TISTB)
071100031031     C                   CLEAR                   tistb000
071200121008     C                   CLEAR                   tistbds
071300031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
071400110927     C                   EVAL      stbSTS = '1'
071500031103     C                   EXSR      WRIREC
071600031031     C                   ENDIF
071700031031     C****
071800031031     C*
071900031031     C****
072000031103     C* stato GIACNODIS2 (giacenza aperta ma in attesa d disposizioni del cliente; con LNP<>LNA)
072100031031     C****
072200031031     C* - condizioni:
072300050310     C*   data aper./riaper. giacenza > 0 AND
072400031031     C*   data disposizioni mittente (gcpDDM) = 0 AND
072500050614     C*   fase giacenza (gcpFAS) < 30 (ex = 020) AND
072600031031     C*   LNP <> LNA
072700031103     C                   EVAL      wDAS = gcpDUR
072800031031     C*
072900031031     C* Verifico le condizione che soddisfano lo stato GIACNODIS2
073000050614     C                   IF        wDAS    > *zeros AND
073100050614     C                             gcpDDM  = *zeros AND
073200050614     C                             gcpFAS  < 030    AND
073300110927     C                             wLNP <> wLNA
073400031103     C                   EVAL      wTIS = '9'
073500031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA MA IN ATTESA ' +
073600031103     C                                        'DI DISPOSIZIONI DEL CLIENTE '  +
073700031103     C                                        '(LNP<>LNA)'
073800031103     C                   EVAL      wPRS = '3'
073900031103     C                   EVAL      wCOS = 'GIACNODIS2'
074000121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
074100031031     C*
074200031031     C* - inizializzo il buffer del file stati bolle (TISTB)
074300031031     C                   CLEAR                   tistb000
074400121008     C                   CLEAR                   tistbds
074500031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
074600110927     C                   EVAL      stbSTS = '1'
074700031103     C                   EXSR      WRIREC
074800031031     C                   ENDIF
074900031031     C****
075000031031     C*
075100031031     C****
075200031103     C* stato GIACSIDIS1 (giacenza aperta e disposizioni del cliente pervenute; con LNP=LNA)
075300031031     C****
075400031031     C* - condizioni:
075500050310     C*   data aper./riaper. giacenza > 0 AND
075600031031     C*   data disposizioni mittente (gcpDDM) > 0 AND
075700050614     C*   fase giacenza (gcpFAS) >= 030 AND
075800031031     C*   LNP = LNA
075900110927     C***                EVAL      wDAS = gcpDUR
076000110927     C                   EVAL      wDAS = gcpDDM
076100031031     C*
076200031031     C* Verifico le condizione che soddisfano lo stato GIACSIDIS1
076300050614     C                   IF        wDAS    > *zeros AND
076400050614     C                             gcpDDM  > *zeros AND
076500050614     C                             gcpFAS >= 030    AND
076600110927     C                             wLNP  = wLNA
076700031103     C                   EVAL      wTIS = '9'
076800031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA E DISPOSIZIONI' +
076900031103     C                                        ' DEL CLIENTE PERVENUTE '        +
077000031103     C                                        '(LNP=LNA)'
077100031103     C                   EVAL      wPRS = '3'
077200050614     C                   EVAL      wCOS = 'GIACSIDIS1'
077300121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
077400031031     C*
077500031031     C* Verifico se tale stato è già presente nel file TISTB
077600121010     C                   EVAL      kORS = %int(wORS/100)
077700050224     C     KEYstb04      CHAIN     tistb04l
077800031031     C*
077900031031     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
078000121008     C                   IF        not %found(tistb04l)
078100050614     C*
078200050614     C* Verifico anche se precedente stato analogo 'GIACNODIS' è già stato dato
078300050614     C* se non già scritto => scrivo anche quello
078400050614     C                   EVAL      wCOS = 'GIACNODIS1'
078500121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
078600050614     C* Verifico se tale stato è già presente nel file TISTB
078700121010     C                   EVAL      kORS = %int(wORS/100)
078800050614     C     KEYstb04      CHAIN     tistb04l
078900050614     C                   IF        not %found(tistb04l)
079000050614     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
079100050614     C                   CLEAR                   tistb000
079200121008     C                   CLEAR                   tistbds
079300110927     C                   EVAL      wDAS = gcpDUR
079400110927     C                   EVAL      stbSTS = '1'
079500050614     C                   EXSR      WRIREC
079600050614     C                   ENDIF
079700050614     C*
079800031031     C* - inizializzo il buffer del file stati bolle (TISTB)
079900031031     C                   CLEAR                   tistb000
080000121008     C                   CLEAR                   tistbds
080100050614     C                   EVAL      wCOS = 'GIACSIDIS1'
080200121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
080300031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
080400110927     C                   EVAL      wDAS = gcpDDM
080500031031     C                   EVAL      stbSTS = '2'
080600031103     C                   EXSR      WRIREC
080700031031     C                   ENDIF
080800031031     C                   ENDIF
080900031031     C****
081000031031     C*
081100031031     C****
081200031103     C* stato GIACSIDIS2 (giacenza aperta e disposizioni del cliente pervenute; con LNP<>LNA)
081300031031     C****
081400031031     C* - condizioni:
081500050310     C*   data aper./riaper. giacenza > 0 AND
081600031031     C*   data disposizioni mittente (gcpDDM) > 0 AND
081700050614     C*   fase giacenza (gcpFAS) >= 030 AND
081800031031     C*   LNP <> LNA
081900110927     C***                EVAL      wDAS = gcpDUR
082000110927     C                   EVAL      wDAS = gcpDDM
082100031031     C*
082200031031     C* Verifico le condizione che soddisfano lo stato GIACSIDIS2
082300050614     C                   IF        wDAS    > *zeros AND
082400050614     C                             gcpDDM  > *zeros AND
082500050614     C                             gcpFAS >= 030    AND
082600110927     C                             wLNP <> wLNA
082700031103     C                   EVAL      wTIS = '9'
082800031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA E DISPOSIZIONI' +
082900031103     C                                        ' DEL CLIENTE PERVENUTE '        +
083000031103     C                                        '(LNP<>LNA)'
083100031103     C                   EVAL      wPRS = '3'
083200031103     C                   EVAL      wCOS = 'GIACSIDIS2'
083300121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
083400031031     C*
083500031031     C* Verifico se tale stato è già presente nel file TISTB
083600121010     C                   EVAL      kORS = %int(wORS/100)
083700121008     C     KEYstb04      CHAIN     tistb04l
083800031031     C*
083900031031     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
084000050224     C                   IF        not %found(tistb04l)
084100050614     C*
084200050614     C* Verifico anche se precedente stato analogo 'GIACNODIS' è già stato dato
084300050614     C* se non già scritto => scrivo anche quello
084400050614     C                   EVAL      wCOS = 'GIACNODIS2'
084500121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
084600050614     C* Verifico se tale stato è già presente nel file TISTB
084700121010     C                   EVAL      kORS = %int(wORS/100)
084800050614     C     KEYstb04      CHAIN     tistb04l
084900050614     C                   IF        not %found(tistb04l)
085000050614     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
085100050614     C                   CLEAR                   tistb000
085200121008     C                   CLEAR                   tistbds
085300110927     C                   EVAL      wDAS = gcpDUR
085400110927     C                   EVAL      stbSTS = '1'
085500050614     C                   EXSR      WRIREC
085600050614     C                   ENDIF
085700050614     C*
085800031031     C* - inizializzo il buffer del file stati bolle (TISTB)
085900031031     C                   CLEAR                   tistb000
086000121008     C                   CLEAR                   tistbds
086100050614     C                   EVAL      wCOS = 'GIACSIDIS2'
086200121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
086300031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
086400110927     C                   EVAL      wDAS = gcpDDM
086500031031     C                   EVAL      stbSTS = '2'
086600031103     C                   EXSR      WRIREC
086700031031     C                   ENDIF
086800031031     C                   ENDIF
086900110926     C****
087000110927     C*
087100110927     C****
087200110927     C* stato apertura GIACENZA NO DISPOSIZIONI (standard)
087300110927     C****
087400110927     C* - condizioni:
087500110927     C*   data chiusura giacenza > 0
087600110927     C                   EVAL      wDAS = gcpDUR
087700110927     C*
087800110928     C* Verifico le condizione che soddisfano lo stato
087900110927     C                   IF        wDAS   > *zeros
088000110927     C                   EVAL      wTIS = '1'
088100110927     C                   EVAL      wPRS = '3'
088200110928     C                   EVAL      wCOS = 'GIACAPE'+gcpCMC
088300121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(1))
088400110927     C*
088500110927     C* - inizializzo il buffer del file stati bolle (TISTB)
088600110927     C                   CLEAR                   tistb000
088700121008     C                   CLEAR                   tistbds
088800110927     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
088900110927     C                   EVAL      stbSTS = '1'
089000110927     C                   EXSR      WRIREC
089100110927     C                   ENDIF
089200110927     C****
089300110927     C*
089400110927     C****
089500110927     C* stato apertura GIACENZA SI DISPOSIZIONI (standard)
089600110927     C****
089700110927     C* - condizioni:
089800110927     C*   data chiusura giacenza > 0
089900110927     C                   EVAL      wDAS = gcpDDM
090000110927     C*
090100110928     C* Verifico le condizione che soddisfano lo stato
090200110927     C                   IF        wDAS   > *zeros
090300110927     C                   EVAL      wTIS = '1'
090400110927     C                   EVAL      wPRS = '3'
090500110927     C                   EVAL      wCOS = 'GIACDIS'+gcpCMC
090600121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
090700110927     C*
090800110927     C* - inizializzo il buffer del file stati bolle (TISTB)
090900110927     C                   CLEAR                   tistb000
091000121008     C                   CLEAR                   tistbds
091100110927     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
091200110927     C                   EVAL      stbSTS = '2'
091300110927     C                   EXSR      WRIREC
091400110927     C                   ENDIF
091500110927     C****
091600110926     C*
091700110926     C****
091800110926     C* stato chiusura GIACENZA (standard)
091900110926     C****
092000110926     C* - condizioni:
092100110926     C*   data chiusura giacenza > 0
092200110926     C                   EVAL      wDAS = gcpDCG
092300110926     C*
092400110928     C* Verifico le condizione che soddisfano lo stato
092500110926     C                   IF        wDAS   > *zeros
092600110926     C                   EVAL      wTIS = '1'
092700110926     C                   EVAL      wPRS = '3'
092800110926     C                   EVAL      wCOS = 'GIACEND'+gcpCFG
092900121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(4))
093000110926     C*
093100110926     C* - inizializzo il buffer del file stati bolle (TISTB)
093200110926     C                   CLEAR                   tistb000
093300121008     C                   CLEAR                   tistbds
093400110926     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
093500111025     C                   EVAL      stbSTS = '2'
093600110926     C                   EXSR      WRIREC
093700110926     C                   ENDIF
093800031031     C****
093900031031     C                   ENDIF
094000031031     C*
094100031031     C* Proseguo la lettura delle giacenze x la bolla corrente
094200050310     C     KEYgcp51      READE     tigcp51l
094300031031     C                   ENDDO
094400031031     C                   ENDIF
094500031031     C*
094600031031     C                   ENDSR
094700041015
094800041015
094900041015
095000041015     C     EXEDCT        BEGSR
095100041015     C*
095200041015     C* Reperisco le C/A presenti relative alla bolla corrente
095300041015     C     KEYdct02      SETLL     fndct02l
095400041015     C                   IF        %equal(fndct02l)
095500041207     C*
095600041207     C* Inizializzo la ds d salvataggio
095700041207     C                   CLEAR                   fndctds_w
095800041207     C*
095900041207     C* Ciclo d lettura
096000041015     C     KEYdct02      READE     fndct02l
096100041015     C                   DOW       not %eof(fndct02l)
096200041015     C*
096300041015     C* Se C/A NN annullata la considero
096400041015     C                   IF        dctATB = *blanks
096500111031     C*
096600111031     C* Inizializzo DS flag operativi
096700111031     C                   CLEAR                   DS_STBFOP
096800041015     C*
096900041015     C* Salvo il buffer del record
097000041015     C                   EVAL      fndctds_w = fndctds
097100041015     C                   ENDIF
097200041015     C*
097300041015     C* Proseguo la lettura delle C/A x la bolla corrente
097400041015     C     KEYdct02      READE     fndct02l
097500041015     C                   ENDDO
097600041015     C*
097700041015     C* AL termine della lettura d tutte le C/A sulla bolla corrente considero l'ultima...
097800041015     C*
097900050224     C* Costruisco la chiave sul file TISTB04L
098000041015     C                   EVAL      wDAS = w_dctAAC*10000 + w_dctMGC
098100041015     C                   EVAL      wORS = *zeros
098200041015     C                   EVAL      wTIS = '1'
098300041015     C                   EVAL      wPRS = '5'
098400041015     C                   EVAL      wCOS = w_dctTAD
098500041015     C*
098600041015     C* - inizializzo il buffer del file stati bolle (TISTB)
098700041015     C                   CLEAR                   tistb000
098800121008     C                   CLEAR                   tistbds
098900041015     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
099000041015     C                   EVAL      stbSTS = '2'
099100041015     C                   EXSR      WRIREC
099200041015     C*
099300041015     C                   ENDIF
099400041015     C*
099500041015     C                   ENDSR
099600170926
099700170926
099800170926
099900170926     C     EXEAPP        BEGSR
100000170926     C*
100100170926     C****
100200170926     C* stato APPUNT (concordato appuntamento)
100300170926     C****
100400170926     C* - condizioni:
100500170926     C*   1ma o 2nd consegna particolare (xxxTC1 / xxxTC2) indicanti appuntamento
100600170926     C*   data consegna richiesta (xxxDCR) valorizzata
100700170926     C*
100800170926     C* Verifico le condizione che soddisfano lo stato APPUNT
100900170926     C                   IF        (wTC1='A' or wTC2='A') and wDCR>*zeros
101000170926     C                   EVAL      wTIS = '1'
101100170926     C                   EVAL      wDESTATO = 'APPUNTAMENTO CONCORDATO'
101200170926     C                   EVAL      wDAS = wDCR
101300170926     C                   EVAL      wORS = wHCR*100
101400170926     C                   EVAL      wPRS = '4'
101500170926     C                   EVAL      wCOS = 'APPUNT'
101600170926     C*
101700170926     C* - inizializzo il buffer del file stati bolle (TISTB)
101800170926     C                   CLEAR                   tistb000
101900170926     C                   CLEAR                   tistbds
102000170926     C*
102100170926     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
102200170926     C                   EVAL      stbSTS = '2'
102300170926     C                   EXSR      WRIREC
102400170926     C                   ENDIF
102500170926     C*
102600170926     C                   ENDSR
102700031031
102800031031
102900031031
103000041207     C     EXEVAC        BEGSR
103100111031     C*
103200111031     C* Inizializzo DS flag operativi
103300111031     C                   CLEAR                   DS_STBFOP
103400041014     C*
103500041014     C****
103600041014     C* stato INCARICO1 (bolla presa in carico; con LNP=LNA)
103700041014     C****
103800041014     C* - condizioni:
103900041014     C*   LNP = LNA
104000041014     C*
104100041014     C* Verifico le condizione che soddisfano lo stato INCARICO1
104200041014     C                   IF        vacLNP = vacLNA
104300041014     C                   EVAL      wTIS = '9'
104400041014     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
104500041014     C                                        '(LNP=LNA)'
104600041207     C                   EVAL      wDAS = vacAAS*10000+vacMGS
104700131030     C                   EVAL      wORS = 210000
104800110926     C                   EVAL      wPRS = '4'
104900041014     C                   EVAL      wCOS = 'INCARICO1'
105000041014     C*
105100041014     C* - inizializzo il buffer del file stati bolle (TISTB)
105200041014     C                   CLEAR                   tistb000
105300121008     C                   CLEAR                   tistbds
105400041014     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
105500041014     C                   EVAL      stbSTS = '1'
105600041014     C                   EXSR      WRIREC
105700041014     C                   ENDIF
105800041014     C****
105900041014     C*
106000041014     C****
106100041014     C* stato INCARICO2 (bolla presa in carico; con LNP<>LNA)
106200041014     C****
106300041014     C* - condizioni:
106400041014     C*   LNP <> LNA
106500041014     C*
106600041014     C* Verifico le condizione che soddisfano lo stato INCARICO2
106700041014     C                   IF        vacLNP <> vacLNA
106800041014     C                   EVAL      wTIS = '9'
106900041014     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
107000041014     C                                        '(LNP<>LNA)'
107100041207     C                   EVAL      wDAS = vacAAS*10000+vacMGS
107200131030     C                   EVAL      wORS = 210000
107300110926     C                   EVAL      wPRS = '4'
107400041014     C                   EVAL      wCOS = 'INCARICO2'
107500041014     C*
107600041014     C* - inizializzo il buffer del file stati bolle (TISTB)
107700041014     C                   CLEAR                   tistb000
107800121008     C                   CLEAR                   tistbds
107900041014     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
108000041014     C                   EVAL      stbSTS = '1'
108100041014     C                   EXSR      WRIREC
108200041014     C                   ENDIF
108300040205     C****
108400040205     C* stato CONSOK (bolla chiusa con consegna OK; generica STANDARD)
108500040205     C****
108600040205     C* - condizioni:
108700040205     C*   data consegna reale (vacDCM) > 0 AND
108800040205     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
108900040205     C*
109000040205     C* Verifico le condizione che soddisfano lo stato CONSOK
109100041207     C                   IF        vacDCM  > *zeros                   AND
109200120713     C                             (vacCCA = *blanks OR vacCCA = '1')
109300120713     C***                          AND wNDC   <> *all'9'
109400040205     C                   EVAL      wTIS = '1'
109500040205     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
109600041207     C                   EVAL      wDAS = vacDCM
109700121010     C                   EVAL      wORS = vacHMC*100
109800110926     C                   EVAL      wPRS = '4'
109900040205     C                   EVAL      wCOS = 'CONSOK'
110000040205     C*
110100040205     C* - inizializzo il buffer del file stati bolle (TISTB)
110200040205     C                   CLEAR                   tistb000
110300121008     C                   CLEAR                   tistbds
110400040205     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
110500040205     C                   EVAL      stbSTS = '9'
110600040205     C                   EXSR      WRIREC
110700040205     C                   ENDIF
110800040205     C*
110900031031     C****
111000031103     C* stato CONSOK1 (bolla chiusa con consegna OK; con LNP=LNA)
111100031031     C****
111200031031     C* - condizioni:
111300031031     C*   data consegna reale (vacDCM) > 0 AND
111400031031     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
111500031031     C*   LNP = LNA
111600031031     C*
111700031031     C* Verifico le condizione che soddisfano lo stato CONSOK1
111800041207     C                   IF        vacDCM  > *zeros                   AND
111900031031     C                             (vacCCA = *blanks OR vacCCA = '1') AND
112000120713     C                             vacLNP = vacLNA
112100120713     C***                          AND wNDC   <> *all'9'
112200031103     C                   EVAL      wTIS = '9'
112300031103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK ' +
112400031103     C                                        '(LNP=LNA)'
112500041207     C                   EVAL      wDAS = vacDCM
112600121010     C                   EVAL      wORS = vacHMC*100
112700110926     C                   EVAL      wPRS = '4'
112800031103     C                   EVAL      wCOS = 'CONSOK1'
112900031031     C*
113000031031     C* - inizializzo il buffer del file stati bolle (TISTB)
113100031031     C                   CLEAR                   tistb000
113200121008     C                   CLEAR                   tistbds
113300031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
113400031031     C                   EVAL      stbSTS = '9'
113500031103     C                   EXSR      WRIREC
113600031031     C                   ENDIF
113700031031     C****
113800031031     C*
113900031031     C****
114000031103     C* stato CONSOK2 (bolla chiusa con consegna OK; con LNP<>LNA)
114100031031     C****
114200031031     C* - condizioni:
114300031031     C*   data consegna reale (vacDCM) > 0 AND
114400031031     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
114500031031     C*   LNP <> LNA
114600031031     C*
114700120620     C* Verifico le condizione che soddisfano lo stato CONSOK2
114800041207     C                   IF        vacDCM  > *zeros                   AND
114900031031     C                             (vacCCA = *blanks OR vacCCA = '1') AND
115000120713     C                             vacLNP <> vacLNA
115100120713     C***                          AND wNDC   <> *all'9'
115200031103     C                   EVAL      wTIS = '9'
115300031103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK ' +
115400031103     C                                        '(LNP<>LNA)'
115500041207     C                   EVAL      wDAS = vacDCM
115600121010     C                   EVAL      wORS = vacHMC*100
115700110926     C                   EVAL      wPRS = '4'
115800031103     C                   EVAL      wCOS = 'CONSOK2'
115900031031     C*
116000031031     C* - inizializzo il buffer del file stati bolle (TISTB)
116100031031     C                   CLEAR                   tistb000
116200121008     C                   CLEAR                   tistbds
116300031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
116400031031     C                   EVAL      stbSTS = '9'
116500031103     C                   EXSR      WRIREC
116600031031     C                   ENDIF
116700031031     C****
116800140624     C*
116900040205     C****
117000040205     C* stato RESO (bolla chiusa con reso; generica STANDARD)
117100040205     C****
117200040205     C* - condizioni:
117300040205     C*   data consegna reale (vacDCM) > 0 AND
117400040205     C*   codice consegna anomala (vacCCA) = '2'
117500040205     C*
117600040205     C* Verifico le condizione che soddisfano lo stato RESO
117700041207     C                   IF        vacDCM > *zeros                    AND
117800040205     C                             vacCCA = '2'
117900040205     C                   EVAL      wTIS = '1'
118000040205     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON RESO'
118100041207     C                   EVAL      wDAS = vacDCM
118200121010     C  N50              EVAL      wORS = vacHMC*100
118300110926     C                   EVAL      wPRS = '4'
118400040205     C                   EVAL      wCOS = 'RESO'
118500040205     C*
118600040205     C* - inizializzo il buffer del file stati bolle (TISTB)
118700040205     C                   CLEAR                   tistb000
118800121008     C                   CLEAR                   tistbds
118900040205     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
119000040205     C                   EVAL      stbSTS = '9'
119100040205     C                   EXSR      WRIREC
119200040205     C                   ENDIF
119300100201     C*
119400100201     C****
119500100201     C* stato AVARIA (bolla chiusa con avaria; generica STANDARD)
119600100201     C****
119700100201     C* - condizioni:
119800100201     C*   data consegna reale (vacDCM) > 0 AND
119900100204     C*   codice consegna anomala (vacCCA) = '6'
120000100201     C*
120100111025     C* Verifico le condizione che soddisfano lo stato AVARIA
120200100201     C                   IF        vacDCM > *zeros                    AND
120300100201     C                             vacCCA = '6'
120400100201     C                   EVAL      wTIS = '1'
120500100201     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON AVARIA'
120600100201     C                   EVAL      wDAS = vacDCM
120700121010     C                   EVAL      wORS = vacHMC*100
120800110926     C                   EVAL      wPRS = '4'
120900100201     C                   EVAL      wCOS = 'AVARIA'
121000100201     C*
121100100201     C* - inizializzo il buffer del file stati bolle (TISTB)
121200100201     C                   CLEAR                   tistb000
121300121008     C                   CLEAR                   tistbds
121400100201     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
121500100201     C                   EVAL      stbSTS = '9'
121600100201     C                   EXSR      WRIREC
121700100201     C                   ENDIF
121800100204     C*
121900100204     C****
122000100204     C* stato ANOMALIA (bolla chiusa con anomalia; generica STANDARD)
122100100204     C****
122200100204     C* - condizioni:
122300100204     C*   data consegna reale (vacDCM) > 0 AND
122400100204     C*   codice consegna anomala (vacCCA) = '5'
122500100204     C*
122600111025     C* Verifico le condizione che soddisfano lo stato ANOMALIA
122700100204     C                   IF        vacDCM > *zeros                    AND
122800100204     C                             vacCCA = '5'
122900100204     C                   EVAL      wTIS = '1'
123000100204     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON ANOMALIA'
123100100204     C                   EVAL      wDAS = vacDCM
123200121010     C                   EVAL      wORS = vacHMC*100
123300110926     C                   EVAL      wPRS = '4'
123400100204     C                   EVAL      wCOS = 'ANOMALIA'
123500100204     C*
123600100204     C* - inizializzo il buffer del file stati bolle (TISTB)
123700100204     C                   CLEAR                   tistb000
123800121008     C                   CLEAR                   tistbds
123900100204     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
124000100204     C                   EVAL      stbSTS = '9'
124100100204     C                   EXSR      WRIREC
124200100204     C                   ENDIF
124300120712     C****
124400120712     C* stato MERCE MAI AFFIDATA (bolla chiusa con anomalia; generica STANDARD)
124500120712     C****
124600120712     C* - condizioni:
124700120712     C*   data consegna reale (vacDCM) > 0 AND
124800120712     C*   codice consegna anomala (vacCCA) = '7'
124900120712     C*
125000120712     C* Verifico le condizione che soddisfano lo stato ANOMALIA
125100120712     C                   IF        vacDCM > *zeros                    AND
125200120712     C                             vacCCA = '7'
125300120712     C                   EVAL      wTIS = '1'
125400120712     C                   EVAL      wDESTATO = 'BOLLA CHIUSA MERCE MAI AFFIDATA'
125500120712     C                   EVAL      wDAS = vacDCM
125600121010     C                   EVAL      wORS = vacHMC*100
125700120712     C                   EVAL      wPRS = '4'
125800120712     C                   EVAL      wCOS = 'COLNOTAFF'
125900120712     C*
126000120712     C* - inizializzo il buffer del file stati bolle (TISTB)
126100120712     C                   CLEAR                   tistb000
126200121008     C                   CLEAR                   tistbds
126300120712     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
126400120712     C                   EVAL      stbSTS = '9'
126500120712     C                   EXSR      WRIREC
126600120712     C                   ENDIF
126700140624     C*
126800140624     C****
126900140624     C* stato MERCE DISTRUTTA (bolla chiusa come merce da distruggere; generica STANDARD)
127000140624     C****
127100140624     C* - condizioni:
127200140624     C*   data consegna reale (vacDCM) > 0 AND
127300140624     C*   codice consegna anomala (vacCCA) = 'S'
127400140624     C*
127500170802     C* Verifico le condizione che soddisfano lo stato MERCE DISTRUTTA
127600140624     C                   IF        vacDCM > *zeros                    AND
127700140624     C                             vacCCA = 'S'
127800140624     C                   EVAL      wTIS = '1'
127900140624     C                   EVAL      wDESTATO = 'BOLLA CHIUSA - MERCE DISTRUTTA'
128000140624     C                   EVAL      wDAS = vacDCM
128100140624     C  N50              EVAL      wORS = vacHMC*100
128200140624     C                   EVAL      wPRS = '4'
128300140624     C                   EVAL      wCOS = 'MDISTRUTTA'
128400140624     C*
128500140624     C* - inizializzo il buffer del file stati bolle (TISTB)
128600140624     C                   CLEAR                   tistb000
128700140624     C                   CLEAR                   tistbds
128800140624     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
128900140624     C                   EVAL      stbSTS = '9'
129000140624     C                   EXSR      WRIREC
129100140624     C                   ENDIF
129200161114     C****
129300161114     C* stato CONSRIC (bolla con data consegna richiesta)
129400161114     C****
129500161114     C* - condizioni:
129600161114     C*   data consegna richiesta (vacDCR) > tutte altre date:
129700161114     C*      data consegna merce  (vacDCM)
129800161114     C*      data lasciato avviso (vacDLA)
129900161114     C*      data giacenza        (vacDAG)
130000161114     C*
130100161114     C* Verifico le condizione che soddisfano lo stato CONSOK
130200161114     C                   IF        vacDCR  > vacDCM AND
130300161114     C                             vacDCR  > vacDLA AND
130400161114     C                             vacDCR  > vacDAG
130500161114     C                   EVAL      wTIS = '1'
130600161114     C                   EVAL      wDESTATO = 'DATA CONSEGNA RICHIESTA'
130700161114     C                   EVAL      wDAS = vacDCR
130800161114     C                   EVAL      wORS = vacHCR*100
130900161114     C                   EVAL      wPRS = '4'
131000161114     C                   EVAL      wCOS = 'CONSRIC'
131100161114     C*
131200161114     C* - inizializzo il buffer del file stati bolle (TISTB)
131300161114     C                   CLEAR                   tistb000
131400161114     C                   CLEAR                   tistbds
131500161114     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
131600161114     C                   EVAL      stbSTS = '2'
131700161114     C                   EXSR      WRIREC
131800161114     C                   ENDIF
131900031031     C*
132000031031     C                   ENDSR
132100100806
132200100806
132300100806
132400100806     C     EXEVAP        BEGSR
132500111031     C*
132600111031     C* Inizializzo DS flag operativi
132700111031     C                   CLEAR                   DS_STBFOP
132800100806     C*
132900100806     C* Imposto subito alcuni valori
133000100806     C                   EVAL      wDAS = vapDAE
133100121010     C                   EVAL      wORS = vapORE
133200100806     C*
133300100806     C                   EVAL      wTIS = '1'
133400100806     C                   EVAL      wPRS = '6'
133500100806     C                   EVAL      wCOS = 'O' + %editc(vapFAR:'X') + vapCAR
133600100806     C*
133700100806     C                   CLEAR                   tistb000
133800121008     C                   CLEAR                   tistbds
133900100806     C*
134000100806     C                   EVAL      STB_BOR    = 'P'                             * ORM
134100100806     C                   EVAL      STB_FILLER = vapIDC
134200100806     C*
134300100806     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
134400100806     C                   EVAL      stbSTS = '1'
134500100806     C                   EXSR      WRIREC
134600100806     C*
134700100806     C                   ENDSR
134800120424
134900120424
135000120424
135100120424     C     EXEAR5GEN     BEGSR
135200120424     C*
135300120424     C* Inizializzo la ds d salvataggio
135400120424     C                   CLEAR                   DAR5GEN
135500120424     C*
135600120424     C* Aggancio il tipo record 'GEN'
135700120427     C                   EVAL      ar5TRD = 'GEN'
135800120424     C     KEYar531P     CHAIN     fiar531c
135900120424     C                   IF        %found(fiar531c)
136000120424     C*
136100120424     C* Se record NN annullato lo considero
136200120424     C                   IF        ar5ATB = *blanks
136300120424     C*
136400120424     C* Inizializzo DS flag operativi
136500120424     C                   CLEAR                   DS_STBFOP
136600120424     C*
136700120424     C* Salvo il buffer del record
136800120424     C                   EVAL      DAR5GEN = ar5UNI
136900120424     C*
137000120424     C* Costruisco la chiave sul file TISTB04L
137100120427     C                   IF        §AR5ARRDT <> *blanks
137200120427     C                   EVAL      wDAS = %dec(§AR5ARRDT:8:0)
137300120427     C                   ENDIF
137400120427     C                   IF        §AR5ARRHM <> *blanks
137500121010     C                   EVAL      wORS = %dec(§AR5ARRHM:4:0)*100
137600120427     C                   ENDIF
137700120424     C                   EVAL      wTIS = '1'
137800120424     C                   EVAL      wPRS = '4'
137900120424     C                   EVAL      wCOS = 'ARRPRICOL'
138000120424     C*
138100120424     C* - inizializzo il buffer del file stati bolle (TISTB)
138200120424     C                   CLEAR                   tistb000
138300121008     C                   CLEAR                   tistbds
138400120424     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
138500120424     C                   EVAL      stbSTS = '2'
138600120424     C                   EXSR      WRIREC
138700120424     C*
138800120424     C                   ENDIF
138900120424     C                   ENDIF
139000120424     C*
139100120424     C                   ENDSR
139200130227
139300130227
139400130227
139500130227     C     EXEPCT        BEGSR
139600130227     C*
139700130228     C* Innanzitutto verifico se il cliente di scambio dati è abilitato agli stati PDA
139800130227     C                   IF        w§PPTPDA = 'S'
139900130227     C*
140000130227     C* Inizializzo la ds d salvataggio
140100130227     C                   CLEAR                   fipctds_w
140200160208     C                   CLEAR                   fipctcetds
140300160208     C*
140400160208     C* Inizializzo il buffer del file stati bolle (TISTB)
140500160208     C                   CLEAR                   tistb000
140600160208     C                   CLEAR                   tistbds
140700160211     C                   EVAL      wPCTCMC = *loval
140800130227     C*
140900130227     C* Reperisco l'ultimo stato CET dagli eventi PDA
141000130227     C                   EVAL      pctTRD = 'CET'
141100160211     C     KEYpct03P     SETLL     fipct03l
141200160211     C                   IF        %equal(fipct03l)
141300160211     C     KEYpct03P     READE     fipct03l
141400160211     C                   DOW       not %eof(fipct03l)
141500130227     C*
141600130227     C* Se stato NN annullato lo considero
141700130227     C                   IF        pctATB = *blanks
141800130227     C*
141900130227     C* Salvo il buffer corrente
142000130227     C                   EVAL      fipctds_w = fipctds
142100130227     C*
142200130227     C* Verifico ultimo esito da PDA
142300130227     C                   EVAL      fipctcetds = w_pctDATI
142400160211     C*
142500160211     C* Verifico rottura di codice per codice evento consegna
142600160211     C* e considero solamente l'ultimo (a parità di codice evento)
142700160211     C                   IF        §PCTCMC <> wPCTCMC
142800130227     C*
142900130227     C* Se spedizione consegnata => inserisco stato 'CONSOK'
143000160211     C                   IF        §PCTCMC = *blanks
143100130227     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
143200130227     C                   EVAL      wCOS = 'CONSOK'
143300130304     C                   EVAL      stbSTS = '9'
143400130227     C                   ELSE
143500160211     C                   EVAL      wCOS = §PCTCMC
143600130304     C                   EVAL      stbSTS = '2'
143700130227     C                   ENDIF
143800130227     C*
143900130227     C* Valorizzo il buffer del record si stato nel TISTB
144000130227     C                   EVAL      wTIS = '1'
144100130227     C                   EVAL      wDAS = §PCTDATA
144200130227     C                   EVAL      wORS = §PCTORA
144300130227     C                   EVAL      wPRS = '7'
144400160208     C*
144500160208     C* Valorizzo e scarico il buffer del file stati bolle (TISTB)
144600130227     C                   EXSR      WRIREC
144700160211     C*
144800160211     C* Salvo il nuovo codice di rottura
144900160211     C                   EVAL      wPCTCMC = §PCTCMC
145000160211     C*
145100160211     C                   ENDIF
145200160211     C                   ENDIF
145300160211     C*
145400160211     C     KEYpct03P     READE     fipct03l
145500160211     C                   ENDDO
145600130227     C*
145700130228     C* Se non trovo la spedizione negli status PDA => verifico in FNARB
145800160208     C***                ELSE
145900130228     C*
146000160208     C* Se no stato PDA => aggancio la bolla IN ARRIVO
146100160208     C***  KEYarb01C     CHAIN     fnarb01l
146200160208     C***                if        %found(fnarb01l)
146300130228     C*
146400130228     C* Chiamata metodo GetLastChild
146500160208     C***                movel     *blanks       wEsito2           1
146600160208     C***                eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
146700160208     C***                                             wAAS
146800160208     C***                                            :wLNP
146900160208     C***                                            :wNRS
147000160208     C***                                            :wNSP
147100160208     C***                                            :pOutAnnoFI
147200160208     C***                                            :pOutLineaParFI
147300160208     C***                                            :pOutSerieFI
147400160208     C***                                            :pOutNumSpedFI
147500160208     C***                                            :pOutDcmFI
147600160208     C***                                            :pOutCcaFI))
147700160208     C***                if        wEsito2 = '0'
147800130228     C*
147900130228     C* Considero solo se spedizione (ultima bolla figlia) consegnata
148000160208     C***                if        pOutDcmFI > *zeros   AND
148100160208     C***                          pOutCcaFI = *blanks
148200160208     C***                EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
148300160208     C***                EVAL      wCOS = 'CONSOK'
148400130228     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
148500160208     C***                EVAL      stbSTS = '9'
148600160208     C***                EXSR      WRIREC
148700160208     C***                endif
148800130228     C*
148900130228     C* Se non ha bolle figlie verifico i dati sulla bolla stessa
149000160208     C***                else
149100130228     C*
149200130228     C* Considero solo se spedizione consegnata
149300160208     C***                if        arbDCM > *zeros   AND
149400160208     C***                          arbCCA = *blanks
149500160208     C***                EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
149600160208     C***                EVAL      wCOS = 'CONSOK'
149700130228     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
149800160208     C***                EVAL      stbSTS = '9'
149900160208     C***                EXSR      WRIREC
150000160208     C***                endif
150100130228     C*
150200160208     C***                endif
150300130228     C*
150400160208     C***                ENDIF
150500130227     C                   ENDIF
150600130227     C*
150700130227     C                   ENDIF
150800130227     C*
150900130227     C                   ENDSR
151000031031
151100160223
151200160223
151300160223     C     EXECOLLI      BEGSR
151400160223     C*
151500160223     C* Innanzitutto verifico se il cliente di scambio dati è abilitato alle info colli
151600160223     C                   IF        w§PPTCOLLI = 'S'
151700160223     C*
151800160223     C* Inizializzo il buffer del file stati bolle (TISTB)
151900160223     C                   CLEAR                   tistb000
152000160223     C                   CLEAR                   tistbds
152100160223     C*
152200160223     C* Reperisco le informazioni relative ai colli
152300160705     C                   EVAL      UBCOLOPZ     = 'FV5'
152400160223     C                   EVAL      UBCOLFLGOPE  = *blanks
152500160223     C                   EVAL      UBCOLTLA     = *blanks
152600160223     C                   EVAL      UBCOLAAS     = wAAS
152700160223     C                   EVAL      UBCOLLNP     = wLNP
152800160223     C                   EVAL      UBCOLNRS     = wNRS
152900160223     C                   EVAL      UBCOLNSP     = wNSP
153000160223     C                   CLEAR                   UBCOLERR
153100160223     C*
153200160223     C* Richiamo l'aopposito driver
153300160223     C                   CALL      'UBCOL00R'
153400160223     C                   PARM                    UBCOLOPZ
153500160223     C                   PARM                    UBCOLFLGOPE
153600160223     C                   PARM                    UBCOLTLA
153700160223     C                   PARM                    UBCOLAAS
153800160223     C                   PARM                    UBCOLLNP
153900160223     C                   PARM                    UBCOLNRS
154000160223     C                   PARM                    UBCOLNSP
154100160223     C                   PARM                    UBCOLNCL
154200160223     C                   PARM                    UBCOLTIP
154300160223     C                   PARM                    UBCOLDCM
154400160223     C                   PARM                    UBCOLHMC
154500160223     C                   PARM                    UBCOLSKBRT
154600160223     C                   PARM                    UBCOLSKCLI
154700160223     C                   PARM                    UBCOLSKDCM
154800160223     C                   PARM                    UBCOLSKHMC
154900160223     C                   PARM                    UBCOLSKDFS
155000160223     C                   PARM                    UBCOLSKHMS
155100160223     C                   PARM                    UBCOLSKVUC
155200160223     C                   PARM                    UBCOLSKPUC
155300160223     C                   PARM                    UBCOLERR
155400160223     C*
155500160223     C* Valorizzo l'output solo se l'esito è OK
155600160223     C                   IF        UBCOLERR = 0
155700160223     C*
155800160223     C* Ciclo per tutti i colli della spedizione
155900160224     C                   Z-ADD     1             wIdxCollo         5 0
156000160223     C                   DOW       wIdxCollo <= UBCOLNCL
156100160223     C*
156200160223     C* Routine per valorizzazione SPUNTA ENTRATA (5)
156300160223     C                   EXSR      EXESPUNTENT
156400160223     C*
156500160223     C                   ADD       1             wIdxCollo
156600160223     C                   ENDDO
156700160223     C*
156800160223     C                   ENDIF
156900160223     C                   ENDIF
157000160223     C*
157100160223     C                   ENDSR
157200160223
157300160223
157400160223
157500160223     C     EXESPUNTENT   BEGSR
157600160223     C*
157700160223     C* Valorizzo il buffer del record di stato nel TISTB
157800160223     C                   EVAL      wDESTATO = 'SPUNTA ENTRARA (5)'
157900160223     C                   EVAL      wCOS = 'SPUNTNPG5'
158000160627     C                   EVAL      wDAS = UBCOLSKDFS(1)
158100160627     C                   EVAL      wORS = UBCOLSKHMS(1)
158200160223     C                   EVAL      wTIS = '1'
158300160223     C                   EVAL      wPRS = '8'
158400160223     C                   CLEAR                   DSTBDNPG5
158500160223     C*
158600160223     C* Per il primo collo inserisco testata
158700160223     C                   IF        wIdxCollo  = 1
158800160223     C                   EXSR      WRIREC
158900160223     C                   ENDIF
159000160223     C*
159100160223     C* Per i colli inserisco anche le info di dettaglio TISTBD
159200160223     C                   CLEAR                   TISTBD00
159300160223     C                   EVAL      STBDKSU = STBKSU
159400160223     C                   EVAL      STBDAAS = STBAAS
159500160223     C                   EVAL      STBDLNP = STBLNP
159600160223     C                   EVAL      STBDNRS = STBNRS
159700160223     C                   EVAL      STBDNSP = STBNSP
159800160223     C                   EVAL      STBDDAS = STBDAS
159900160223     C                   EVAL      STBDORS = STBORS
160000160223     C                   EVAL      STBDCOS = STBCOS
160100160223     C                   EVAL      STBDTIS = 'BRT'
160200160223     C                   EVAL      STBDSEG = UBCOLSKBRT(wIdxCollo)
160300160223     C                   EVAL      STBDDAD = UBCOLSKDFS(wIdxCollo)
160400160224     C                   EVAL      STBDORD = UBCOLSKHMS(wIdxCollo)
160500160223     C                   MOVEL     datcor        stbDDAO
160600160223     C                   MOVE      oracor        stbDDAO
160700160223     C                   EVAL      §STBDNPG51 = UBCOLSKCLI(wIdxCollo)
160800160223     C                   EVAL      STBDDTA = DSTBDNPG5
160900160223     C*
161000160223     C                   WRITE(e)  TISTBD00
161100160223     C*
161200160223     C                   ENDSR
161300031031
161400031031
161500031031
161600031103     C     WRIREC        BEGSR
161700090430     C*
161800090430     C* Valorizzo il buffer del record del file stati bolla (TISTB)
161900090430     C                   EVAL      stbKSU = w§PPTKSU
162000090430     C                   EVAL      stbKSC = wKSC
162100090430     C                   EVAL      stbAAS = wAAS
162200090430     C                   EVAL      stbLNP = wLNP
162300090430     C                   EVAL      stbNRS = wNRS
162400090430     C                   EVAL      stbNSP = wNSP
162500090430     C                   EVAL      stbRMN = wRMN
162600090430     C                   EVAL      stbRMA = wRMA
162700090430     C                   EVAL      stbDSP = wDSP
162800090430     C                   EVAL      stbDAS = wDAS
162900121010     C                   EVAL      stbORS = %int(wORS/100)
163000090430     C                   EVAL      stbTIS = wTIS
163100090430     C                   EVAL      stbPRS = wPRS
163200090430     C                   EVAL      stbCOS = wCOS
163300090430     C* Valorizzo la data/ora d scrittura dello stato dell'evento
163400130314     C                   CLEAR                   stbDAOR
163500090430     C                   MOVEL     datcor        stbDAOR
163600090430     C                   MOVE      oracor        stbDAOR
163700100806     C*
163800100806     C                   IF        w§PPTTIP <> 'VP'
163900081112     C*
164000081112     C* Verifico se trattasi d bolla originale oppure figlia
164100081112     C                   movel     *blanks       wEsito1           1
164200090430     C                   movel     *blanks       wEsito2           1
164300081112     C*
164400081112     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
164500081112     C                                                STBAAS
164600081112     C                                               :STBLNP
164700081112     C                                               :STBNRS
164800081112     C                                               :STBNSP
164900090430     C                                               :pOutLblTyp
165000090430     C                                               :pOutAnnoBO
165100090430     C                                               :pOutLineaParBO
165200090430     C                                               :pOutSerieBO
165300090430     C                                               :pOutNumSpedBO
165400090430     C                                               :pOutDcmBO
165500090430     C                                               :pOutCcaBO
165600090430     C                                               :pOutRblBO))
165700081112     C*
165800111103     C                   IF        STB_BOR = *blanks
165900111103     C                   EVAL      STB_BOR = pOutLblTyp                         * bolla Originale
166000111103     C                   ENDIF
166100111103     C*
166200090430     C                   IF        pOutLblTyp = 'O'
166300090430     C*
166400111103     C* Se è uno stato d chiusura spedizione
166500111025     C                   if        stbSTS = '9'
166600090430     C* Chiamata metodo GetLastChild
166700090430     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
166800090430     C                                                STBAAS
166900090430     C                                               :STBLNP
167000090430     C                                               :STBNRS
167100090430     C                                               :STBNSP
167200090430     C                                               :pOutAnnoFI
167300090430     C                                               :pOutLineaParFI
167400090430     C                                               :pOutSerieFI
167500090430     C                                               :pOutNumSpedFI
167600090430     C                                               :pOutDcmFI
167700090430     C                                               :pOutCcaFI))
167800090430     C* Se ultima figlia consegnata allora overrido i dati consegna della bolla originale relativa
167900090430     C                   if        pOutDcmFI > *zeros
168000090430     C                   if        pOutCcaFI = '2'
168100090430     C                   eval      stbCOS = 'RESO'
168200111025     C                   eval      wCOS   = 'RESO'
168300111103     C                   if        wDAS   = pOutDcmFI
168400090430     C                   eval      stbDAS = pOutDcmFI
168500090430     C                   eval      wDAS   = pOutDcmFI
168600130228     C                   endif
168700090430     C                   endif
168800100201     C                   if        pOutCcaFI = '6'
168900100201     C                   eval      stbCOS = 'AVARIA'
169000111025     C                   eval      wCOS   = 'AVARIA'
169100111103     C                   IF        STB_BOR = *blanks
169200100201     C                   eval      stbDAS = pOutDcmFI
169300100201     C                   eval      wDAS   = pOutDcmFI
169400111103     C                   ENDIF
169500100201     C                   endif
169600090430     C                   endif
169700111031     C*
169800111025     C                   endif
169900111031     C*
170000081112     C                   ENDIF
170100100806     C*
170200100806     C                   ENDIF
170300090430     C*
170400081112     C                   EVAL      stbFOP = DS_STBFOP
170500121010     C*
170600121010     C* **** FORZATURE "FINALI" ****
170700121012     C*
170800121012     C* - MIC
170900121010     C                   IF         stbPRS = '2'    and
171000121010     C                             (stbCOS = 'MIC'  or
171100121010     C                              stbCOS = 'NIC') and
171200121012     C                              stbORS >= 1200
171300170517     C                   EVAL      wORS   = 1159*100
171400121010     C                   EVAL      stbORS = 1159
171500121010     C                   ENDIF
171600090430     C*
171700090430     C* Verifico se tale stato è già presente nel file TISTB
171800121010     C                   EVAL      kORS = %int(wORS/100)
171900121008     C     KEYstb04      CHAIN     tistb04l
172000090430     C*
172100090430     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
172200121008     C                   IF        not %found(tistb04l)
172300081112     C*
172400031103     C* Scarico il buffer del file stati bolle (TISTB)
172500121008     C                   WRITE     tistb000
172600121008     C*
172700121008     C                   ELSE
172800121008     C                   IF        w§PPTUPD = 'U'
172900121008     C                   EVAL      tistbds_i = tistbds
173000121008     C                   UPDATE    tistb004
173100121008     C                   ENDIF
173200090430     C*
173300090430     C                   ENDIF
173400031103     C*
173500031103     C* Dopo la scrittura dello stato bolla corrente, se trattasi d stato bolla PERSONALIZZATO
173600031103     C* (campo STBTIS='9') verifico che il codice stato attribuito
173700031103     C* sia presente in tabella "STATI BOLLA RICAVATI" (Tab. "STB")
173800031103     C                   IF        stbTIS = '9'
173900031103     C                   EVAL      tbeCOD = 'STB'
174000031103     C                   EVAL      tbeKE1 = stbCOS
174100031103     C     KEYtbe01P     SETLL     tntbe01l
174200031103     C                   IF        %equal(tntbe01l)
174300031103     C                   ELSE
174400031103     C                   CLEAR                   DSSTB
174500031103     C                   EVAL      §STBDES = wDESTATO
174600031103     C                   EVAL      tbeUNI = DSSTB
174700031103     C                   WRITE     tntbe000
174800031103     C                   ENDIF
174900031103     C                   ENDIF
175000031031     C*
175100031031     C                   ENDSR
175200031031
175300031031
175400991027
175500991027      /TITLE Operazioni iniziali.
175600991027     C     *inzsr        BEGSR
175700991027     C*
175800991027     C     *ENTRY        PLIST
175900991027     C                   parm                    prmppt
176000991027     C     wrkesito      parm      wrkesito      prmesito
176100031103     C*
176200031103     C* Ridefinisco subito i parametri d traduzione
176300031103     C                   EVAL      DSPPT = prmppt
176400040308     C*
176500040308     C* Calcola la data corrente
176600040308     C                   time                    wn14             14 0
176700040308     C                   movel     wn14          oracor            6 0          *ORA  (6) IN H/M/S
176800100713     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
176900100713     C                   eval      datcor = %dec(%date() : *ISO)
177000030709     C*
177100030709     C* Definizione chiavi
177200030924     C*
177300031103     C* Chiave su TNTBE01L - Parziale
177400031103     C     KEYtbe01P     KLIST
177500031103     C                   KFLD                    tbeCOD
177600031103     C                   KFLD                    tbeKE1
177700031014     C* Chiave su FNEVB01L - Parziale
177800031014     C     KEYevb01P     KLIST
177900031106     C                   KFLD                    wAAS
178000031106     C                   KFLD                    wLNP
178100031106     C                   KFLD                    wNRS
178200031106     C                   KFLD                    wNSP
178300110929     C* Chiave su FNEVB04L - Parziale
178400110929     C     KEYevb04P     KLIST
178500111031     C                   KFLD                    evb4_EVBAAS
178600111031     C                   KFLD                    evb4_EVBLNP
178700111031     C                   KFLD                    evb4_EVBNRS
178800111031     C                   KFLD                    evb4_EVBNSP
178900111031     C                   KFLD                    evb4_EVBCEV
179000111031     C                   KFLD                    evb4_EVBDEV
179100111031     C* Chiave su TIGCP51L - Completa
179200050310     C     KEYgcp51      KLIST
179300031106     C                   KFLD                    wAAS
179400031106     C                   KFLD                    wLNP
179500031106     C                   KFLD                    wNRS
179600031106     C                   KFLD                    wNSP
179700031031     C                   KFLD                    wFRG
179800041015     C* Chiave su FNDCT02L - Completa
179900041015     C     KEYdct02      KLIST
180000041015     C                   KFLD                    wAAS
180100041015     C                   KFLD                    wLNP
180200041015     C                   KFLD                    wNRS
180300041015     C                   KFLD                    wNSP
180400120424     C* Chiave su FIAR531C - Parziale
180500120424     C     KEYar531P     KLIST
180600120424     C                   KFLD                    wAAS
180700120424     C                   KFLD                    wLNP
180800120424     C                   KFLD                    wNRS
180900120424     C                   KFLD                    wNSP
181000120424     C                   KFLD                    ar5TRD
181100111031     C* Chiave su TITAS30C - Parziale
181200111031     C     KEYtas30P     KLIST
181300111031     C                   KFLD                    wAAS
181400111031     C                   KFLD                    wLNP
181500111031     C                   KFLD                    wNRS
181600111031     C                   KFLD                    wNSP
181700050224     C* Chiave su TISTB04L - Completa
181800050224     C     KEYstb04      KLIST
181900050224     C                   KFLD                    w§PPTKSU
182000031106     C                   KFLD                    wAAS
182100031106     C                   KFLD                    wLNP
182200031106     C                   KFLD                    wNRS
182300031106     C                   KFLD                    wNSP
182400031103     C                   KFLD                    wDAS
182500121010     C                   KFLD                    kORS
182600031103     C                   KFLD                    wCOS
182700130305     C                   KFLD                    wPRS
182800100806     C* Chiave su FNORM01L - Completa
182900100806     C     KEYorm01      KLIST
183000100806     C                   KFLD                    vapPOE
183100100806     C                   KFLD                    vapNSR
183200100806     C                   KFLD                    vapNOR
183300100806     C                   KFLD                    vapNRV
183400111103     C* Chiave su FNARB01L - Completa
183500111103     C     KEYarb01C     KLIST
183600111103     C                   KFLD                    wAAS
183700111103     C                   KFLD                    wLNP
183800111103     C                   KFLD                    wNRS
183900111103     C                   KFLD                    wNSP
184000160211     C* Chiave su FIPCT03L - Parziale
184100160211     C     KEYpct03P     KLIST
184200130227     C                   KFLD                    wAAS
184300130227     C                   KFLD                    wLNP
184400130227     C                   KFLD                    wNRS
184500130227     C                   KFLD                    wNSP
184600130227     C                   KFLD                    pctTRD
184700111103     C*
184800111103     C                   ENDSR
184900130227     C***
