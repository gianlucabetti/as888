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
055100170802     C*
055200170802     C* Se trattasi d reso della bolla figlia => forzo inserimento stato di "INRESO"
055300170802     C                   if        pOutLblTyp = 'F' AND
055400170802     C                             pOutCcaBO = '2'
055500170802     C                   EVAL      STB_BOR = 'F'                                * bolla figl.
055600170802     C*
055700170802     C* Leggo la bolla in arrivo (no SEDE !!!)
055800170802     C     KEYarb01C     chain     fnarb01l
055900170802     C                   if        %found(fnarb01l)
056000170802     C                   EVAL      wTIS = '1'
056100170802     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON RESO'
056200170802     C                   EVAL      wDAS = arbDCM
056300170802     C                   EVAL      wORS = arbHMC*100
056400170802     C                   EVAL      wPRS = '4'
056500170802     C                   EVAL      wCOS = 'INRESO'
056600170802     C*
056700170802     C* - inizializzo il buffer del file stati bolle (TISTB)
056800170802     C                   CLEAR                   tistb000
056900170802     C                   CLEAR                   tistbds
057000170802     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
057100170802     C                   EVAL      stbSTS = '2'
057200170802     C                   EXSR      WRIREC
057300170802     C*
057400170802     C                   else
057500170802     C                   SETON                                        50
057600170802     C                   endif
057700170802     C                   endif
057800111103     C*
057900111103     C* Se trattasi d reso della bolla originale => forzo inserimento stato di reso
058000111103     C                   if        pOutLblTyp = 'O' AND
058100111103     C                             pOutCcaBO = '2'
058200111103     C                   EVAL      STB_BOR = 'O'                                * bolla orig.
058300111103     C*
058400111103     C* Leggo la bolla in arrivo (no SEDE !!!)
058500111103     C     KEYarb01C     chain     fnarb01l
058600111103     C                   if        %found(fnarb01l)
058700111103     C                   EVAL      wTIS = '1'
058800111103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON RESO'
058900111103     C                   EVAL      wDAS = arbDCM
059000121010     C                   EVAL      wORS = arbHMC*100
059100111103     C                   EVAL      wPRS = '4'
059200111103     C                   EVAL      wCOS = 'RESO'
059300111103     C*
059400111103     C* - inizializzo il buffer del file stati bolle (TISTB)
059500111103     C                   CLEAR                   tistb000
059600121008     C                   CLEAR                   tistbds
059700111103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
059800111103     C                   EVAL      stbSTS = '9'
059900111103     C                   EXSR      WRIREC
060000111103     C*
060100111103     C                   else
060200111103     C                   SETON                                        50
060300111103     C                   endif
060400111103     C                   endif
060500111103     C*
060600111031     C                   if        pOutLblTyp = 'F'
060700111031     C                   EVAL      STB_BOR = 'F'                                * bolla Figlia
060800111031     C                   else
060900111103     C                   EVAL      STB_BOR = 'O'                                * bolla orig.
061000111031     C*
061100111031     C* Chiamata metodo GetLastChild
061200111031     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
061300111031     C                                                wAAS
061400111031     C                                               :wLNP
061500111031     C                                               :wNRS
061600111031     C                                               :wNSP
061700111031     C                                               :pOutAnnoFI
061800111031     C                                               :pOutLineaParFI
061900111031     C                                               :pOutSerieFI
062000111031     C                                               :pOutNumSpedFI
062100111031     C                                               :pOutDcmFI
062200111031     C                                               :pOutCcaFI))
062300111031     C                   if        wEsito2 = '0'
062400111031     C*
062500111031     C* Aggancio l'evento relativo della bolla figlia
062600111031     C                   EVAL      evb4_EVBAAS = pOutAnnoFI
062700111031     C                   EVAL      evb4_EVBLNP = pOutLineaParFI
062800111031     C                   EVAL      evb4_EVBNRS = pOutSerieFI
062900111031     C                   EVAL      evb4_EVBNSP = pOutNumSpedFI
063000111031     C                   EVAL      evb4_EVBCEV = evbCEV
063100111031     C                   EVAL      evb4_EVBDEV = evbDEV
063200111031     C*
063300111031     C     KEYevb04P     CHAIN     fnevb04l
063400111031     C                   IF        %found(fnevb04l)
063500111031     C                   IF        evb4_EVBNOT = evbNOT
063600111031     C                   EVAL      STB_BOR = 'F'                                * bolla Figlia
063700111031     C                   EVAL      STB_CCA = pOutCcaBO                          * CCA bolla orig.
063800111031     C                   ENDIF
063900111031     C                   ENDIF
064000111031     C*
064100111031     C                   endif
064200111031     C                   endif
064300111031     C                   endif
064400031031     C*
064500050224     C* Costruisco la chiave sul file TISTB04L
064600121019     C***                IF        evbDEV*10000+evbHEV >
064700121019     C***                          evbDTV*10000+evbORV
064800121012     C                   EVAL      wDAS = evbDEV
064900121019     C                   EVAL      wORS = evbHEV*100
065000121019     C***                ELSE
065100121019     C***                EVAL      wDAS = evbDTV
065200121019     C***                EVAL      wORS = evbORV
065300121019     C***                ENDIF
065400031103     C                   EVAL      wTIS = '1'
065500031103     C                   EVAL      wPRS = '2'
065600031103     C                   EVAL      wCOS = evbCEV
065700031031     C*
065800031031     C* - inizializzo il buffer del file stati bolle (TISTB)
065900031031     C                   CLEAR                   tistb000
066000121008     C                   CLEAR                   tistbds
066100031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
066200031031     C                   EVAL      stbSTS = '2'
066300031103     C                   EXSR      WRIREC
066400031031     C                   ENDIF
066500031031     C*
066600031031     C* Proseguo la lettura degli eventi x la bolla corrente
066700031014     C     KEYevb01P     READE     fnevb01l
066800031014     C                   ENDDO
066900031031     C                   ENDIF
067000991027     C*
067100910830     C                   ENDSR
067200031031
067300031031
067400031031
067500031031     C     EXEGCP        BEGSR
067600031031     C*
067700050310     C* Costruisco la chiave sul file TIGCP51L
067800031031     C                   EVAL      wFRG = *zeros
067900031031     C*
068000031104     C* Reperisco le giacenze relative alla bolla corrente
068100050310     C     KEYgcp51      SETLL     tigcp51l
068200050310     C                   IF        %equal(tigcp51l)
068300050310     C     KEYgcp51      READE     tigcp51l
068400050310     C                   DOW       not %eof(tigcp51l)
068500110929     C*
068600121010     C                   EVAL      wORS = 110000
068700031031     C*
068800031104     C* Se giacenza NN annullata la considero
068900031031     C                   IF        gcpATB = *blanks
069000111031     C*
069100111031     C* Inizializzo DS flag operativi
069200111031     C                   CLEAR                   DS_STBFOP
069300110929     C*
069400110929     C* Aggancio l'evento di apertura giacenza correlato
069500111031     C                   EVAL      evb4_EVBAAS = gcpAAS
069600111031     C                   EVAL      evb4_EVBLNP = gcpLNP
069700111031     C                   EVAL      evb4_EVBNRS = gcpNRS
069800111031     C                   EVAL      evb4_EVBNSP = gcpNSP
069900111031     C                   EVAL      evb4_EVBCEV = gcpCMC
070000111031     C                   EVAL      evb4_EVBDEV = gcpDUR
070100111031     C*
070200110929     C     KEYevb04P     CHAIN     fnevb04l
070300110929     C                   IF        %found(fnevb04l)
070400121008     C***                EVAL      wORS = evb4_evbHEV + 1
070500121019     C***                EVAL      wORS = %dec(%time(evbORV)+%minutes(1))
070600121019     C                   EVAL      wORS = %dec(%time(evbHEV*100)+%minutes(1))
070700110929     C                   ENDIF
070800121009     C*
070900121010     C                   Z-ADD     wORS          wORSgcp           6 0
071000031031     C*
071100050310     C* Effettuo considerazioni sul file TIGCP x detrminare il codice stato da attribuire
071200031103     C* alla bolla:
071300031031     C*
071400031031     C****
071500031103     C* stato GIACNODIS1 (giacenza aperta ma in attesa d disposizioni del cliente; con LNP=LNA)
071600031031     C****
071700031031     C* - condizioni:
071800050310     C*   data aper./riaper. giacenza > 0 AND
071900031031     C*   data disposizioni mittente (gcpDDM) = 0 AND
072000050614     C*   fase giacenza (gcpFAS) < 30 (ex = 020) AND
072100031031     C*   LNP = LNA
072200031103     C                   EVAL      wDAS = gcpDUR
072300031031     C*
072400031031     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
072500050614     C                   IF        wDAS   > *zeros AND
072600050614     C                             gcpDDM = *zeros AND
072700050614     C                             gcpFAS < 030    AND
072800110927     C                             wLNP = wLNA
072900031103     C                   EVAL      wTIS = '9'
073000031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA MA IN ATTESA ' +
073100031103     C                                        'DI DISPOSIZIONI DEL CLIENTE '  +
073200031103     C                                        '(LNP=LNA)'
073300031103     C                   EVAL      wPRS = '3'
073400031103     C                   EVAL      wCOS = 'GIACNODIS1'
073500121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
073600031031     C*
073700031031     C* - inizializzo il buffer del file stati bolle (TISTB)
073800031031     C                   CLEAR                   tistb000
073900121008     C                   CLEAR                   tistbds
074000031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
074100110927     C                   EVAL      stbSTS = '1'
074200031103     C                   EXSR      WRIREC
074300031031     C                   ENDIF
074400031031     C****
074500031031     C*
074600031031     C****
074700031103     C* stato GIACNODIS2 (giacenza aperta ma in attesa d disposizioni del cliente; con LNP<>LNA)
074800031031     C****
074900031031     C* - condizioni:
075000050310     C*   data aper./riaper. giacenza > 0 AND
075100031031     C*   data disposizioni mittente (gcpDDM) = 0 AND
075200050614     C*   fase giacenza (gcpFAS) < 30 (ex = 020) AND
075300031031     C*   LNP <> LNA
075400031103     C                   EVAL      wDAS = gcpDUR
075500031031     C*
075600031031     C* Verifico le condizione che soddisfano lo stato GIACNODIS2
075700050614     C                   IF        wDAS    > *zeros AND
075800050614     C                             gcpDDM  = *zeros AND
075900050614     C                             gcpFAS  < 030    AND
076000110927     C                             wLNP <> wLNA
076100031103     C                   EVAL      wTIS = '9'
076200031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA MA IN ATTESA ' +
076300031103     C                                        'DI DISPOSIZIONI DEL CLIENTE '  +
076400031103     C                                        '(LNP<>LNA)'
076500031103     C                   EVAL      wPRS = '3'
076600031103     C                   EVAL      wCOS = 'GIACNODIS2'
076700121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
076800031031     C*
076900031031     C* - inizializzo il buffer del file stati bolle (TISTB)
077000031031     C                   CLEAR                   tistb000
077100121008     C                   CLEAR                   tistbds
077200031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
077300110927     C                   EVAL      stbSTS = '1'
077400031103     C                   EXSR      WRIREC
077500031031     C                   ENDIF
077600031031     C****
077700031031     C*
077800031031     C****
077900031103     C* stato GIACSIDIS1 (giacenza aperta e disposizioni del cliente pervenute; con LNP=LNA)
078000031031     C****
078100031031     C* - condizioni:
078200050310     C*   data aper./riaper. giacenza > 0 AND
078300031031     C*   data disposizioni mittente (gcpDDM) > 0 AND
078400050614     C*   fase giacenza (gcpFAS) >= 030 AND
078500031031     C*   LNP = LNA
078600110927     C***                EVAL      wDAS = gcpDUR
078700110927     C                   EVAL      wDAS = gcpDDM
078800031031     C*
078900031031     C* Verifico le condizione che soddisfano lo stato GIACSIDIS1
079000050614     C                   IF        wDAS    > *zeros AND
079100050614     C                             gcpDDM  > *zeros AND
079200050614     C                             gcpFAS >= 030    AND
079300110927     C                             wLNP  = wLNA
079400031103     C                   EVAL      wTIS = '9'
079500031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA E DISPOSIZIONI' +
079600031103     C                                        ' DEL CLIENTE PERVENUTE '        +
079700031103     C                                        '(LNP=LNA)'
079800031103     C                   EVAL      wPRS = '3'
079900050614     C                   EVAL      wCOS = 'GIACSIDIS1'
080000121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
080100031031     C*
080200031031     C* Verifico se tale stato è già presente nel file TISTB
080300121010     C                   EVAL      kORS = %int(wORS/100)
080400050224     C     KEYstb04      CHAIN     tistb04l
080500031031     C*
080600031031     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
080700121008     C                   IF        not %found(tistb04l)
080800050614     C*
080900050614     C* Verifico anche se precedente stato analogo 'GIACNODIS' è già stato dato
081000050614     C* se non già scritto => scrivo anche quello
081100050614     C                   EVAL      wCOS = 'GIACNODIS1'
081200121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
081300050614     C* Verifico se tale stato è già presente nel file TISTB
081400121010     C                   EVAL      kORS = %int(wORS/100)
081500050614     C     KEYstb04      CHAIN     tistb04l
081600050614     C                   IF        not %found(tistb04l)
081700050614     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
081800050614     C                   CLEAR                   tistb000
081900121008     C                   CLEAR                   tistbds
082000110927     C                   EVAL      wDAS = gcpDUR
082100110927     C                   EVAL      stbSTS = '1'
082200050614     C                   EXSR      WRIREC
082300050614     C                   ENDIF
082400050614     C*
082500031031     C* - inizializzo il buffer del file stati bolle (TISTB)
082600031031     C                   CLEAR                   tistb000
082700121008     C                   CLEAR                   tistbds
082800050614     C                   EVAL      wCOS = 'GIACSIDIS1'
082900121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
083000031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
083100110927     C                   EVAL      wDAS = gcpDDM
083200031031     C                   EVAL      stbSTS = '2'
083300031103     C                   EXSR      WRIREC
083400031031     C                   ENDIF
083500031031     C                   ENDIF
083600031031     C****
083700031031     C*
083800031031     C****
083900031103     C* stato GIACSIDIS2 (giacenza aperta e disposizioni del cliente pervenute; con LNP<>LNA)
084000031031     C****
084100031031     C* - condizioni:
084200050310     C*   data aper./riaper. giacenza > 0 AND
084300031031     C*   data disposizioni mittente (gcpDDM) > 0 AND
084400050614     C*   fase giacenza (gcpFAS) >= 030 AND
084500031031     C*   LNP <> LNA
084600110927     C***                EVAL      wDAS = gcpDUR
084700110927     C                   EVAL      wDAS = gcpDDM
084800031031     C*
084900031031     C* Verifico le condizione che soddisfano lo stato GIACSIDIS2
085000050614     C                   IF        wDAS    > *zeros AND
085100050614     C                             gcpDDM  > *zeros AND
085200050614     C                             gcpFAS >= 030    AND
085300110927     C                             wLNP <> wLNA
085400031103     C                   EVAL      wTIS = '9'
085500031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA E DISPOSIZIONI' +
085600031103     C                                        ' DEL CLIENTE PERVENUTE '        +
085700031103     C                                        '(LNP<>LNA)'
085800031103     C                   EVAL      wPRS = '3'
085900031103     C                   EVAL      wCOS = 'GIACSIDIS2'
086000121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
086100031031     C*
086200031031     C* Verifico se tale stato è già presente nel file TISTB
086300121010     C                   EVAL      kORS = %int(wORS/100)
086400121008     C     KEYstb04      CHAIN     tistb04l
086500031031     C*
086600031031     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
086700050224     C                   IF        not %found(tistb04l)
086800050614     C*
086900050614     C* Verifico anche se precedente stato analogo 'GIACNODIS' è già stato dato
087000050614     C* se non già scritto => scrivo anche quello
087100050614     C                   EVAL      wCOS = 'GIACNODIS2'
087200121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
087300050614     C* Verifico se tale stato è già presente nel file TISTB
087400121010     C                   EVAL      kORS = %int(wORS/100)
087500050614     C     KEYstb04      CHAIN     tistb04l
087600050614     C                   IF        not %found(tistb04l)
087700050614     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
087800050614     C                   CLEAR                   tistb000
087900121008     C                   CLEAR                   tistbds
088000110927     C                   EVAL      wDAS = gcpDUR
088100110927     C                   EVAL      stbSTS = '1'
088200050614     C                   EXSR      WRIREC
088300050614     C                   ENDIF
088400050614     C*
088500031031     C* - inizializzo il buffer del file stati bolle (TISTB)
088600031031     C                   CLEAR                   tistb000
088700121008     C                   CLEAR                   tistbds
088800050614     C                   EVAL      wCOS = 'GIACSIDIS2'
088900121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
089000031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
089100110927     C                   EVAL      wDAS = gcpDDM
089200031031     C                   EVAL      stbSTS = '2'
089300031103     C                   EXSR      WRIREC
089400031031     C                   ENDIF
089500031031     C                   ENDIF
089600110926     C****
089700110927     C*
089800110927     C****
089900110927     C* stato apertura GIACENZA NO DISPOSIZIONI (standard)
090000110927     C****
090100110927     C* - condizioni:
090200110927     C*   data chiusura giacenza > 0
090300110927     C                   EVAL      wDAS = gcpDUR
090400110927     C*
090500110928     C* Verifico le condizione che soddisfano lo stato
090600110927     C                   IF        wDAS   > *zeros
090700110927     C                   EVAL      wTIS = '1'
090800110927     C                   EVAL      wPRS = '3'
090900110928     C                   EVAL      wCOS = 'GIACAPE'+gcpCMC
091000121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(1))
091100110927     C*
091200110927     C* - inizializzo il buffer del file stati bolle (TISTB)
091300110927     C                   CLEAR                   tistb000
091400121008     C                   CLEAR                   tistbds
091500110927     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
091600110927     C                   EVAL      stbSTS = '1'
091700110927     C                   EXSR      WRIREC
091800110927     C                   ENDIF
091900110927     C****
092000110927     C*
092100110927     C****
092200110927     C* stato apertura GIACENZA SI DISPOSIZIONI (standard)
092300110927     C****
092400110927     C* - condizioni:
092500110927     C*   data chiusura giacenza > 0
092600110927     C                   EVAL      wDAS = gcpDDM
092700110927     C*
092800110928     C* Verifico le condizione che soddisfano lo stato
092900110927     C                   IF        wDAS   > *zeros
093000110927     C                   EVAL      wTIS = '1'
093100110927     C                   EVAL      wPRS = '3'
093200110927     C                   EVAL      wCOS = 'GIACDIS'+gcpCMC
093300121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
093400110927     C*
093500110927     C* - inizializzo il buffer del file stati bolle (TISTB)
093600110927     C                   CLEAR                   tistb000
093700121008     C                   CLEAR                   tistbds
093800110927     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
093900110927     C                   EVAL      stbSTS = '2'
094000110927     C                   EXSR      WRIREC
094100110927     C                   ENDIF
094200110927     C****
094300110926     C*
094400110926     C****
094500110926     C* stato chiusura GIACENZA (standard)
094600110926     C****
094700110926     C* - condizioni:
094800110926     C*   data chiusura giacenza > 0
094900110926     C                   EVAL      wDAS = gcpDCG
095000110926     C*
095100110928     C* Verifico le condizione che soddisfano lo stato
095200110926     C                   IF        wDAS   > *zeros
095300110926     C                   EVAL      wTIS = '1'
095400110926     C                   EVAL      wPRS = '3'
095500110926     C                   EVAL      wCOS = 'GIACEND'+gcpCFG
095600121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(4))
095700110926     C*
095800110926     C* - inizializzo il buffer del file stati bolle (TISTB)
095900110926     C                   CLEAR                   tistb000
096000121008     C                   CLEAR                   tistbds
096100110926     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
096200111025     C                   EVAL      stbSTS = '2'
096300110926     C                   EXSR      WRIREC
096400110926     C                   ENDIF
096500031031     C****
096600031031     C                   ENDIF
096700031031     C*
096800031031     C* Proseguo la lettura delle giacenze x la bolla corrente
096900050310     C     KEYgcp51      READE     tigcp51l
097000031031     C                   ENDDO
097100031031     C                   ENDIF
097200031031     C*
097300031031     C                   ENDSR
097400041015
097500041015
097600041015
097700041015     C     EXEDCT        BEGSR
097800041015     C*
097900041015     C* Reperisco le C/A presenti relative alla bolla corrente
098000041015     C     KEYdct02      SETLL     fndct02l
098100041015     C                   IF        %equal(fndct02l)
098200041207     C*
098300041207     C* Inizializzo la ds d salvataggio
098400041207     C                   CLEAR                   fndctds_w
098500041207     C*
098600041207     C* Ciclo d lettura
098700041015     C     KEYdct02      READE     fndct02l
098800041015     C                   DOW       not %eof(fndct02l)
098900041015     C*
099000041015     C* Se C/A NN annullata la considero
099100041015     C                   IF        dctATB = *blanks
099200111031     C*
099300111031     C* Inizializzo DS flag operativi
099400111031     C                   CLEAR                   DS_STBFOP
099500041015     C*
099600041015     C* Salvo il buffer del record
099700041015     C                   EVAL      fndctds_w = fndctds
099800041015     C                   ENDIF
099900041015     C*
100000041015     C* Proseguo la lettura delle C/A x la bolla corrente
100100041015     C     KEYdct02      READE     fndct02l
100200041015     C                   ENDDO
100300041015     C*
100400041015     C* AL termine della lettura d tutte le C/A sulla bolla corrente considero l'ultima...
100500041015     C*
100600050224     C* Costruisco la chiave sul file TISTB04L
100700041015     C                   EVAL      wDAS = w_dctAAC*10000 + w_dctMGC
100800041015     C                   EVAL      wORS = *zeros
100900041015     C                   EVAL      wTIS = '1'
101000041015     C                   EVAL      wPRS = '5'
101100041015     C                   EVAL      wCOS = w_dctTAD
101200041015     C*
101300041015     C* - inizializzo il buffer del file stati bolle (TISTB)
101400041015     C                   CLEAR                   tistb000
101500121008     C                   CLEAR                   tistbds
101600041015     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
101700041015     C                   EVAL      stbSTS = '2'
101800041015     C                   EXSR      WRIREC
101900041015     C*
102000041015     C                   ENDIF
102100041015     C*
102200041015     C                   ENDSR
102300170926
102400170926
102500170926
102600170926     C     EXEAPP        BEGSR
102700170926     C*
102800170926     C****
102900170926     C* stato APPUNT (concordato appuntamento)
103000170926     C****
103100170926     C* - condizioni:
103200170926     C*   1ma o 2nd consegna particolare (xxxTC1 / xxxTC2) indicanti appuntamento
103300170926     C*   data consegna richiesta (xxxDCR) valorizzata
103400170926     C*
103500170926     C* Verifico le condizione che soddisfano lo stato APPUNT
103600170926     C                   IF        (wTC1='A' or wTC2='A') and wDCR>*zeros
103700170926     C                   EVAL      wTIS = '1'
103800170926     C                   EVAL      wDESTATO = 'APPUNTAMENTO CONCORDATO'
103900170926     C                   EVAL      wDAS = wDCR
104000170926     C                   EVAL      wORS = wHCR*100
104100170926     C                   EVAL      wPRS = '4'
104200170926     C                   EVAL      wCOS = 'APPUNT'
104300170926     C*
104400170926     C* - inizializzo il buffer del file stati bolle (TISTB)
104500170926     C                   CLEAR                   tistb000
104600170926     C                   CLEAR                   tistbds
104700170926     C*
104800170926     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
104900170926     C                   EVAL      stbSTS = '2'
105000170926     C                   EXSR      WRIREC
105100170926     C                   ENDIF
105200170926     C*
105300170926     C                   ENDSR
105400031031
105500031031
105600031031
105700041207     C     EXEVAC        BEGSR
105800111031     C*
105900111031     C* Inizializzo DS flag operativi
106000111031     C                   CLEAR                   DS_STBFOP
106100041014     C*
106200041014     C****
106300041014     C* stato INCARICO1 (bolla presa in carico; con LNP=LNA)
106400041014     C****
106500041014     C* - condizioni:
106600041014     C*   LNP = LNA
106700041014     C*
106800041014     C* Verifico le condizione che soddisfano lo stato INCARICO1
106900041014     C                   IF        vacLNP = vacLNA
107000041014     C                   EVAL      wTIS = '9'
107100041014     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
107200041014     C                                        '(LNP=LNA)'
107300041207     C                   EVAL      wDAS = vacAAS*10000+vacMGS
107400131030     C                   EVAL      wORS = 210000
107500110926     C                   EVAL      wPRS = '4'
107600041014     C                   EVAL      wCOS = 'INCARICO1'
107700041014     C*
107800041014     C* - inizializzo il buffer del file stati bolle (TISTB)
107900041014     C                   CLEAR                   tistb000
108000121008     C                   CLEAR                   tistbds
108100041014     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
108200041014     C                   EVAL      stbSTS = '1'
108300041014     C                   EXSR      WRIREC
108400041014     C                   ENDIF
108500041014     C****
108600041014     C*
108700041014     C****
108800041014     C* stato INCARICO2 (bolla presa in carico; con LNP<>LNA)
108900041014     C****
109000041014     C* - condizioni:
109100041014     C*   LNP <> LNA
109200041014     C*
109300041014     C* Verifico le condizione che soddisfano lo stato INCARICO2
109400041014     C                   IF        vacLNP <> vacLNA
109500041014     C                   EVAL      wTIS = '9'
109600041014     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
109700041014     C                                        '(LNP<>LNA)'
109800041207     C                   EVAL      wDAS = vacAAS*10000+vacMGS
109900131030     C                   EVAL      wORS = 210000
110000110926     C                   EVAL      wPRS = '4'
110100041014     C                   EVAL      wCOS = 'INCARICO2'
110200041014     C*
110300041014     C* - inizializzo il buffer del file stati bolle (TISTB)
110400041014     C                   CLEAR                   tistb000
110500121008     C                   CLEAR                   tistbds
110600041014     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
110700041014     C                   EVAL      stbSTS = '1'
110800041014     C                   EXSR      WRIREC
110900041014     C                   ENDIF
111000040205     C****
111100040205     C* stato CONSOK (bolla chiusa con consegna OK; generica STANDARD)
111200040205     C****
111300040205     C* - condizioni:
111400040205     C*   data consegna reale (vacDCM) > 0 AND
111500040205     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
111600040205     C*
111700040205     C* Verifico le condizione che soddisfano lo stato CONSOK
111800041207     C                   IF        vacDCM  > *zeros                   AND
111900120713     C                             (vacCCA = *blanks OR vacCCA = '1')
112000120713     C***                          AND wNDC   <> *all'9'
112100040205     C                   EVAL      wTIS = '1'
112200040205     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
112300041207     C                   EVAL      wDAS = vacDCM
112400121010     C                   EVAL      wORS = vacHMC*100
112500110926     C                   EVAL      wPRS = '4'
112600040205     C                   EVAL      wCOS = 'CONSOK'
112700040205     C*
112800040205     C* - inizializzo il buffer del file stati bolle (TISTB)
112900040205     C                   CLEAR                   tistb000
113000121008     C                   CLEAR                   tistbds
113100040205     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
113200040205     C                   EVAL      stbSTS = '9'
113300040205     C                   EXSR      WRIREC
113400040205     C                   ENDIF
113500040205     C*
113600031031     C****
113700031103     C* stato CONSOK1 (bolla chiusa con consegna OK; con LNP=LNA)
113800031031     C****
113900031031     C* - condizioni:
114000031031     C*   data consegna reale (vacDCM) > 0 AND
114100031031     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
114200031031     C*   LNP = LNA
114300031031     C*
114400031031     C* Verifico le condizione che soddisfano lo stato CONSOK1
114500041207     C                   IF        vacDCM  > *zeros                   AND
114600031031     C                             (vacCCA = *blanks OR vacCCA = '1') AND
114700120713     C                             vacLNP = vacLNA
114800120713     C***                          AND wNDC   <> *all'9'
114900031103     C                   EVAL      wTIS = '9'
115000031103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK ' +
115100031103     C                                        '(LNP=LNA)'
115200041207     C                   EVAL      wDAS = vacDCM
115300121010     C                   EVAL      wORS = vacHMC*100
115400110926     C                   EVAL      wPRS = '4'
115500031103     C                   EVAL      wCOS = 'CONSOK1'
115600031031     C*
115700031031     C* - inizializzo il buffer del file stati bolle (TISTB)
115800031031     C                   CLEAR                   tistb000
115900121008     C                   CLEAR                   tistbds
116000031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
116100031031     C                   EVAL      stbSTS = '9'
116200031103     C                   EXSR      WRIREC
116300031031     C                   ENDIF
116400031031     C****
116500031031     C*
116600031031     C****
116700031103     C* stato CONSOK2 (bolla chiusa con consegna OK; con LNP<>LNA)
116800031031     C****
116900031031     C* - condizioni:
117000031031     C*   data consegna reale (vacDCM) > 0 AND
117100031031     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
117200031031     C*   LNP <> LNA
117300031031     C*
117400120620     C* Verifico le condizione che soddisfano lo stato CONSOK2
117500041207     C                   IF        vacDCM  > *zeros                   AND
117600031031     C                             (vacCCA = *blanks OR vacCCA = '1') AND
117700120713     C                             vacLNP <> vacLNA
117800120713     C***                          AND wNDC   <> *all'9'
117900031103     C                   EVAL      wTIS = '9'
118000031103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK ' +
118100031103     C                                        '(LNP<>LNA)'
118200041207     C                   EVAL      wDAS = vacDCM
118300121010     C                   EVAL      wORS = vacHMC*100
118400110926     C                   EVAL      wPRS = '4'
118500031103     C                   EVAL      wCOS = 'CONSOK2'
118600031031     C*
118700031031     C* - inizializzo il buffer del file stati bolle (TISTB)
118800031031     C                   CLEAR                   tistb000
118900121008     C                   CLEAR                   tistbds
119000031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
119100031031     C                   EVAL      stbSTS = '9'
119200031103     C                   EXSR      WRIREC
119300031031     C                   ENDIF
119400031031     C****
119500140624     C*
119600040205     C****
119700040205     C* stato RESO (bolla chiusa con reso; generica STANDARD)
119800040205     C****
119900040205     C* - condizioni:
120000040205     C*   data consegna reale (vacDCM) > 0 AND
120100040205     C*   codice consegna anomala (vacCCA) = '2'
120200040205     C*
120300040205     C* Verifico le condizione che soddisfano lo stato RESO
120400041207     C                   IF        vacDCM > *zeros                    AND
120500040205     C                             vacCCA = '2'
120600040205     C                   EVAL      wTIS = '1'
120700040205     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON RESO'
120800041207     C                   EVAL      wDAS = vacDCM
120900121010     C  N50              EVAL      wORS = vacHMC*100
121000110926     C                   EVAL      wPRS = '4'
121100040205     C                   EVAL      wCOS = 'RESO'
121200040205     C*
121300040205     C* - inizializzo il buffer del file stati bolle (TISTB)
121400040205     C                   CLEAR                   tistb000
121500121008     C                   CLEAR                   tistbds
121600040205     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
121700040205     C                   EVAL      stbSTS = '9'
121800040205     C                   EXSR      WRIREC
121900040205     C                   ENDIF
122000100201     C*
122100100201     C****
122200100201     C* stato AVARIA (bolla chiusa con avaria; generica STANDARD)
122300100201     C****
122400100201     C* - condizioni:
122500100201     C*   data consegna reale (vacDCM) > 0 AND
122600100204     C*   codice consegna anomala (vacCCA) = '6'
122700100201     C*
122800111025     C* Verifico le condizione che soddisfano lo stato AVARIA
122900100201     C                   IF        vacDCM > *zeros                    AND
123000100201     C                             vacCCA = '6'
123100100201     C                   EVAL      wTIS = '1'
123200100201     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON AVARIA'
123300100201     C                   EVAL      wDAS = vacDCM
123400121010     C                   EVAL      wORS = vacHMC*100
123500110926     C                   EVAL      wPRS = '4'
123600100201     C                   EVAL      wCOS = 'AVARIA'
123700100201     C*
123800100201     C* - inizializzo il buffer del file stati bolle (TISTB)
123900100201     C                   CLEAR                   tistb000
124000121008     C                   CLEAR                   tistbds
124100100201     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
124200100201     C                   EVAL      stbSTS = '9'
124300100201     C                   EXSR      WRIREC
124400100201     C                   ENDIF
124500100204     C*
124600100204     C****
124700100204     C* stato ANOMALIA (bolla chiusa con anomalia; generica STANDARD)
124800100204     C****
124900100204     C* - condizioni:
125000100204     C*   data consegna reale (vacDCM) > 0 AND
125100100204     C*   codice consegna anomala (vacCCA) = '5'
125200100204     C*
125300111025     C* Verifico le condizione che soddisfano lo stato ANOMALIA
125400100204     C                   IF        vacDCM > *zeros                    AND
125500100204     C                             vacCCA = '5'
125600100204     C                   EVAL      wTIS = '1'
125700100204     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON ANOMALIA'
125800100204     C                   EVAL      wDAS = vacDCM
125900121010     C                   EVAL      wORS = vacHMC*100
126000110926     C                   EVAL      wPRS = '4'
126100100204     C                   EVAL      wCOS = 'ANOMALIA'
126200100204     C*
126300100204     C* - inizializzo il buffer del file stati bolle (TISTB)
126400100204     C                   CLEAR                   tistb000
126500121008     C                   CLEAR                   tistbds
126600100204     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
126700100204     C                   EVAL      stbSTS = '9'
126800100204     C                   EXSR      WRIREC
126900100204     C                   ENDIF
127000120712     C****
127100120712     C* stato MERCE MAI AFFIDATA (bolla chiusa con anomalia; generica STANDARD)
127200120712     C****
127300120712     C* - condizioni:
127400120712     C*   data consegna reale (vacDCM) > 0 AND
127500120712     C*   codice consegna anomala (vacCCA) = '7'
127600120712     C*
127700120712     C* Verifico le condizione che soddisfano lo stato ANOMALIA
127800120712     C                   IF        vacDCM > *zeros                    AND
127900120712     C                             vacCCA = '7'
128000120712     C                   EVAL      wTIS = '1'
128100120712     C                   EVAL      wDESTATO = 'BOLLA CHIUSA MERCE MAI AFFIDATA'
128200120712     C                   EVAL      wDAS = vacDCM
128300121010     C                   EVAL      wORS = vacHMC*100
128400120712     C                   EVAL      wPRS = '4'
128500120712     C                   EVAL      wCOS = 'COLNOTAFF'
128600120712     C*
128700120712     C* - inizializzo il buffer del file stati bolle (TISTB)
128800120712     C                   CLEAR                   tistb000
128900121008     C                   CLEAR                   tistbds
129000120712     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
129100120712     C                   EVAL      stbSTS = '9'
129200120712     C                   EXSR      WRIREC
129300120712     C                   ENDIF
129400140624     C*
129500140624     C****
129600140624     C* stato MERCE DISTRUTTA (bolla chiusa come merce da distruggere; generica STANDARD)
129700140624     C****
129800140624     C* - condizioni:
129900140624     C*   data consegna reale (vacDCM) > 0 AND
130000140624     C*   codice consegna anomala (vacCCA) = 'S'
130100140624     C*
130200170802     C* Verifico le condizione che soddisfano lo stato MERCE DISTRUTTA
130300140624     C                   IF        vacDCM > *zeros                    AND
130400140624     C                             vacCCA = 'S'
130500140624     C                   EVAL      wTIS = '1'
130600140624     C                   EVAL      wDESTATO = 'BOLLA CHIUSA - MERCE DISTRUTTA'
130700140624     C                   EVAL      wDAS = vacDCM
130800140624     C  N50              EVAL      wORS = vacHMC*100
130900140624     C                   EVAL      wPRS = '4'
131000140624     C                   EVAL      wCOS = 'MDISTRUTTA'
131100140624     C*
131200140624     C* - inizializzo il buffer del file stati bolle (TISTB)
131300140624     C                   CLEAR                   tistb000
131400140624     C                   CLEAR                   tistbds
131500140624     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
131600140624     C                   EVAL      stbSTS = '9'
131700140624     C                   EXSR      WRIREC
131800140624     C                   ENDIF
131900161114     C****
132000161114     C* stato CONSRIC (bolla con data consegna richiesta)
132100161114     C****
132200161114     C* - condizioni:
132300161114     C*   data consegna richiesta (vacDCR) > tutte altre date:
132400161114     C*      data consegna merce  (vacDCM)
132500161114     C*      data lasciato avviso (vacDLA)
132600161114     C*      data giacenza        (vacDAG)
132700161114     C*
132800161114     C* Verifico le condizione che soddisfano lo stato CONSOK
132900161114     C                   IF        vacDCR  > vacDCM AND
133000161114     C                             vacDCR  > vacDLA AND
133100161114     C                             vacDCR  > vacDAG
133200161114     C                   EVAL      wTIS = '1'
133300161114     C                   EVAL      wDESTATO = 'DATA CONSEGNA RICHIESTA'
133400161114     C                   EVAL      wDAS = vacDCR
133500161114     C                   EVAL      wORS = vacHCR*100
133600161114     C                   EVAL      wPRS = '4'
133700161114     C                   EVAL      wCOS = 'CONSRIC'
133800161114     C*
133900161114     C* - inizializzo il buffer del file stati bolle (TISTB)
134000161114     C                   CLEAR                   tistb000
134100161114     C                   CLEAR                   tistbds
134200161114     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
134300161114     C                   EVAL      stbSTS = '2'
134400161114     C                   EXSR      WRIREC
134500161114     C                   ENDIF
134600031031     C*
134700031031     C                   ENDSR
134800100806
134900100806
135000100806
135100100806     C     EXEVAP        BEGSR
135200111031     C*
135300111031     C* Inizializzo DS flag operativi
135400111031     C                   CLEAR                   DS_STBFOP
135500100806     C*
135600100806     C* Imposto subito alcuni valori
135700100806     C                   EVAL      wDAS = vapDAE
135800121010     C                   EVAL      wORS = vapORE
135900100806     C*
136000100806     C                   EVAL      wTIS = '1'
136100100806     C                   EVAL      wPRS = '6'
136200100806     C                   EVAL      wCOS = 'O' + %editc(vapFAR:'X') + vapCAR
136300100806     C*
136400100806     C                   CLEAR                   tistb000
136500121008     C                   CLEAR                   tistbds
136600100806     C*
136700100806     C                   EVAL      STB_BOR    = 'P'                             * ORM
136800100806     C                   EVAL      STB_FILLER = vapIDC
136900100806     C*
137000100806     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
137100100806     C                   EVAL      stbSTS = '1'
137200100806     C                   EXSR      WRIREC
137300100806     C*
137400100806     C                   ENDSR
137500120424
137600120424
137700120424
137800120424     C     EXEAR5GEN     BEGSR
137900120424     C*
138000120424     C* Inizializzo la ds d salvataggio
138100120424     C                   CLEAR                   DAR5GEN
138200120424     C*
138300120424     C* Aggancio il tipo record 'GEN'
138400120427     C                   EVAL      ar5TRD = 'GEN'
138500120424     C     KEYar531P     CHAIN     fiar531c
138600120424     C                   IF        %found(fiar531c)
138700120424     C*
138800120424     C* Se record NN annullato lo considero
138900120424     C                   IF        ar5ATB = *blanks
139000120424     C*
139100120424     C* Inizializzo DS flag operativi
139200120424     C                   CLEAR                   DS_STBFOP
139300120424     C*
139400120424     C* Salvo il buffer del record
139500120424     C                   EVAL      DAR5GEN = ar5UNI
139600120424     C*
139700120424     C* Costruisco la chiave sul file TISTB04L
139800120427     C                   IF        §AR5ARRDT <> *blanks
139900120427     C                   EVAL      wDAS = %dec(§AR5ARRDT:8:0)
140000120427     C                   ENDIF
140100120427     C                   IF        §AR5ARRHM <> *blanks
140200121010     C                   EVAL      wORS = %dec(§AR5ARRHM:4:0)*100
140300120427     C                   ENDIF
140400120424     C                   EVAL      wTIS = '1'
140500120424     C                   EVAL      wPRS = '4'
140600120424     C                   EVAL      wCOS = 'ARRPRICOL'
140700120424     C*
140800120424     C* - inizializzo il buffer del file stati bolle (TISTB)
140900120424     C                   CLEAR                   tistb000
141000121008     C                   CLEAR                   tistbds
141100120424     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
141200120424     C                   EVAL      stbSTS = '2'
141300120424     C                   EXSR      WRIREC
141400120424     C*
141500120424     C                   ENDIF
141600120424     C                   ENDIF
141700120424     C*
141800120424     C                   ENDSR
141900130227
142000130227
142100130227
142200130227     C     EXEPCT        BEGSR
142300130227     C*
142400130228     C* Innanzitutto verifico se il cliente di scambio dati è abilitato agli stati PDA
142500130227     C                   IF        w§PPTPDA = 'S'
142600130227     C*
142700130227     C* Inizializzo la ds d salvataggio
142800130227     C                   CLEAR                   fipctds_w
142900160208     C                   CLEAR                   fipctcetds
143000160208     C*
143100160208     C* Inizializzo il buffer del file stati bolle (TISTB)
143200160208     C                   CLEAR                   tistb000
143300160208     C                   CLEAR                   tistbds
143400160211     C                   EVAL      wPCTCMC = *loval
143500130227     C*
143600130227     C* Reperisco l'ultimo stato CET dagli eventi PDA
143700130227     C                   EVAL      pctTRD = 'CET'
143800160211     C     KEYpct03P     SETLL     fipct03l
143900160211     C                   IF        %equal(fipct03l)
144000160211     C     KEYpct03P     READE     fipct03l
144100160211     C                   DOW       not %eof(fipct03l)
144200130227     C*
144300130227     C* Se stato NN annullato lo considero
144400130227     C                   IF        pctATB = *blanks
144500130227     C*
144600130227     C* Salvo il buffer corrente
144700130227     C                   EVAL      fipctds_w = fipctds
144800130227     C*
144900130227     C* Verifico ultimo esito da PDA
145000130227     C                   EVAL      fipctcetds = w_pctDATI
145100160211     C*
145200160211     C* Verifico rottura di codice per codice evento consegna
145300160211     C* e considero solamente l'ultimo (a parità di codice evento)
145400160211     C                   IF        §PCTCMC <> wPCTCMC
145500130227     C*
145600130227     C* Se spedizione consegnata => inserisco stato 'CONSOK'
145700160211     C                   IF        §PCTCMC = *blanks
145800130227     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
145900130227     C                   EVAL      wCOS = 'CONSOK'
146000130304     C                   EVAL      stbSTS = '9'
146100130227     C                   ELSE
146200160211     C                   EVAL      wCOS = §PCTCMC
146300130304     C                   EVAL      stbSTS = '2'
146400130227     C                   ENDIF
146500130227     C*
146600130227     C* Valorizzo il buffer del record si stato nel TISTB
146700130227     C                   EVAL      wTIS = '1'
146800130227     C                   EVAL      wDAS = §PCTDATA
146900130227     C                   EVAL      wORS = §PCTORA
147000130227     C                   EVAL      wPRS = '7'
147100160208     C*
147200160208     C* Valorizzo e scarico il buffer del file stati bolle (TISTB)
147300130227     C                   EXSR      WRIREC
147400160211     C*
147500160211     C* Salvo il nuovo codice di rottura
147600160211     C                   EVAL      wPCTCMC = §PCTCMC
147700160211     C*
147800160211     C                   ENDIF
147900160211     C                   ENDIF
148000160211     C*
148100160211     C     KEYpct03P     READE     fipct03l
148200160211     C                   ENDDO
148300130227     C*
148400130228     C* Se non trovo la spedizione negli status PDA => verifico in FNARB
148500160208     C***                ELSE
148600130228     C*
148700160208     C* Se no stato PDA => aggancio la bolla IN ARRIVO
148800160208     C***  KEYarb01C     CHAIN     fnarb01l
148900160208     C***                if        %found(fnarb01l)
149000130228     C*
149100130228     C* Chiamata metodo GetLastChild
149200160208     C***                movel     *blanks       wEsito2           1
149300160208     C***                eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
149400160208     C***                                             wAAS
149500160208     C***                                            :wLNP
149600160208     C***                                            :wNRS
149700160208     C***                                            :wNSP
149800160208     C***                                            :pOutAnnoFI
149900160208     C***                                            :pOutLineaParFI
150000160208     C***                                            :pOutSerieFI
150100160208     C***                                            :pOutNumSpedFI
150200160208     C***                                            :pOutDcmFI
150300160208     C***                                            :pOutCcaFI))
150400160208     C***                if        wEsito2 = '0'
150500130228     C*
150600130228     C* Considero solo se spedizione (ultima bolla figlia) consegnata
150700160208     C***                if        pOutDcmFI > *zeros   AND
150800160208     C***                          pOutCcaFI = *blanks
150900160208     C***                EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
151000160208     C***                EVAL      wCOS = 'CONSOK'
151100130228     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
151200160208     C***                EVAL      stbSTS = '9'
151300160208     C***                EXSR      WRIREC
151400160208     C***                endif
151500130228     C*
151600130228     C* Se non ha bolle figlie verifico i dati sulla bolla stessa
151700160208     C***                else
151800130228     C*
151900130228     C* Considero solo se spedizione consegnata
152000160208     C***                if        arbDCM > *zeros   AND
152100160208     C***                          arbCCA = *blanks
152200160208     C***                EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
152300160208     C***                EVAL      wCOS = 'CONSOK'
152400130228     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
152500160208     C***                EVAL      stbSTS = '9'
152600160208     C***                EXSR      WRIREC
152700160208     C***                endif
152800130228     C*
152900160208     C***                endif
153000130228     C*
153100160208     C***                ENDIF
153200130227     C                   ENDIF
153300130227     C*
153400130227     C                   ENDIF
153500130227     C*
153600130227     C                   ENDSR
153700031031
153800160223
153900160223
154000160223     C     EXECOLLI      BEGSR
154100160223     C*
154200160223     C* Innanzitutto verifico se il cliente di scambio dati è abilitato alle info colli
154300160223     C                   IF        w§PPTCOLLI = 'S'
154400160223     C*
154500160223     C* Inizializzo il buffer del file stati bolle (TISTB)
154600160223     C                   CLEAR                   tistb000
154700160223     C                   CLEAR                   tistbds
154800160223     C*
154900160223     C* Reperisco le informazioni relative ai colli
155000160705     C                   EVAL      UBCOLOPZ     = 'FV5'
155100160223     C                   EVAL      UBCOLFLGOPE  = *blanks
155200160223     C                   EVAL      UBCOLTLA     = *blanks
155300160223     C                   EVAL      UBCOLAAS     = wAAS
155400160223     C                   EVAL      UBCOLLNP     = wLNP
155500160223     C                   EVAL      UBCOLNRS     = wNRS
155600160223     C                   EVAL      UBCOLNSP     = wNSP
155700160223     C                   CLEAR                   UBCOLERR
155800160223     C*
155900160223     C* Richiamo l'aopposito driver
156000160223     C                   CALL      'UBCOL00R'
156100160223     C                   PARM                    UBCOLOPZ
156200160223     C                   PARM                    UBCOLFLGOPE
156300160223     C                   PARM                    UBCOLTLA
156400160223     C                   PARM                    UBCOLAAS
156500160223     C                   PARM                    UBCOLLNP
156600160223     C                   PARM                    UBCOLNRS
156700160223     C                   PARM                    UBCOLNSP
156800160223     C                   PARM                    UBCOLNCL
156900160223     C                   PARM                    UBCOLTIP
157000160223     C                   PARM                    UBCOLDCM
157100160223     C                   PARM                    UBCOLHMC
157200160223     C                   PARM                    UBCOLSKBRT
157300160223     C                   PARM                    UBCOLSKCLI
157400160223     C                   PARM                    UBCOLSKDCM
157500160223     C                   PARM                    UBCOLSKHMC
157600160223     C                   PARM                    UBCOLSKDFS
157700160223     C                   PARM                    UBCOLSKHMS
157800160223     C                   PARM                    UBCOLSKVUC
157900160223     C                   PARM                    UBCOLSKPUC
158000160223     C                   PARM                    UBCOLERR
158100160223     C*
158200160223     C* Valorizzo l'output solo se l'esito è OK
158300160223     C                   IF        UBCOLERR = 0
158400160223     C*
158500160223     C* Ciclo per tutti i colli della spedizione
158600160224     C                   Z-ADD     1             wIdxCollo         5 0
158700160223     C                   DOW       wIdxCollo <= UBCOLNCL
158800160223     C*
158900160223     C* Routine per valorizzazione SPUNTA ENTRATA (5)
159000160223     C                   EXSR      EXESPUNTENT
159100160223     C*
159200160223     C                   ADD       1             wIdxCollo
159300160223     C                   ENDDO
159400160223     C*
159500160223     C                   ENDIF
159600160223     C                   ENDIF
159700160223     C*
159800160223     C                   ENDSR
159900160223
160000160223
160100160223
160200160223     C     EXESPUNTENT   BEGSR
160300160223     C*
160400160223     C* Valorizzo il buffer del record di stato nel TISTB
160500160223     C                   EVAL      wDESTATO = 'SPUNTA ENTRARA (5)'
160600160223     C                   EVAL      wCOS = 'SPUNTNPG5'
160700160627     C                   EVAL      wDAS = UBCOLSKDFS(1)
160800160627     C                   EVAL      wORS = UBCOLSKHMS(1)
160900160223     C                   EVAL      wTIS = '1'
161000160223     C                   EVAL      wPRS = '8'
161100160223     C                   CLEAR                   DSTBDNPG5
161200160223     C*
161300160223     C* Per il primo collo inserisco testata
161400160223     C                   IF        wIdxCollo  = 1
161500160223     C                   EXSR      WRIREC
161600160223     C                   ENDIF
161700160223     C*
161800160223     C* Per i colli inserisco anche le info di dettaglio TISTBD
161900160223     C                   CLEAR                   TISTBD00
162000160223     C                   EVAL      STBDKSU = STBKSU
162100160223     C                   EVAL      STBDAAS = STBAAS
162200160223     C                   EVAL      STBDLNP = STBLNP
162300160223     C                   EVAL      STBDNRS = STBNRS
162400160223     C                   EVAL      STBDNSP = STBNSP
162500160223     C                   EVAL      STBDDAS = STBDAS
162600160223     C                   EVAL      STBDORS = STBORS
162700160223     C                   EVAL      STBDCOS = STBCOS
162800160223     C                   EVAL      STBDTIS = 'BRT'
162900160223     C                   EVAL      STBDSEG = UBCOLSKBRT(wIdxCollo)
163000160223     C                   EVAL      STBDDAD = UBCOLSKDFS(wIdxCollo)
163100160224     C                   EVAL      STBDORD = UBCOLSKHMS(wIdxCollo)
163200160223     C                   MOVEL     datcor        stbDDAO
163300160223     C                   MOVE      oracor        stbDDAO
163400160223     C                   EVAL      §STBDNPG51 = UBCOLSKCLI(wIdxCollo)
163500160223     C                   EVAL      STBDDTA = DSTBDNPG5
163600160223     C*
163700160223     C                   WRITE(e)  TISTBD00
163800160223     C*
163900160223     C                   ENDSR
164000031031
164100031031
164200031031
164300031103     C     WRIREC        BEGSR
164400090430     C*
164500090430     C* Valorizzo il buffer del record del file stati bolla (TISTB)
164600090430     C                   EVAL      stbKSU = w§PPTKSU
164700090430     C                   EVAL      stbKSC = wKSC
164800090430     C                   EVAL      stbAAS = wAAS
164900090430     C                   EVAL      stbLNP = wLNP
165000090430     C                   EVAL      stbNRS = wNRS
165100090430     C                   EVAL      stbNSP = wNSP
165200090430     C                   EVAL      stbRMN = wRMN
165300090430     C                   EVAL      stbRMA = wRMA
165400090430     C                   EVAL      stbDSP = wDSP
165500090430     C                   EVAL      stbDAS = wDAS
165600121010     C                   EVAL      stbORS = %int(wORS/100)
165700090430     C                   EVAL      stbTIS = wTIS
165800090430     C                   EVAL      stbPRS = wPRS
165900090430     C                   EVAL      stbCOS = wCOS
166000090430     C* Valorizzo la data/ora d scrittura dello stato dell'evento
166100130314     C                   CLEAR                   stbDAOR
166200090430     C                   MOVEL     datcor        stbDAOR
166300090430     C                   MOVE      oracor        stbDAOR
166400100806     C*
166500100806     C                   IF        w§PPTTIP <> 'VP'
166600081112     C*
166700081112     C* Verifico se trattasi d bolla originale oppure figlia
166800081112     C                   movel     *blanks       wEsito1           1
166900090430     C                   movel     *blanks       wEsito2           1
167000081112     C*
167100081112     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
167200081112     C                                                STBAAS
167300081112     C                                               :STBLNP
167400081112     C                                               :STBNRS
167500081112     C                                               :STBNSP
167600090430     C                                               :pOutLblTyp
167700090430     C                                               :pOutAnnoBO
167800090430     C                                               :pOutLineaParBO
167900090430     C                                               :pOutSerieBO
168000090430     C                                               :pOutNumSpedBO
168100090430     C                                               :pOutDcmBO
168200090430     C                                               :pOutCcaBO
168300090430     C                                               :pOutRblBO))
168400081112     C*
168500111103     C                   IF        STB_BOR = *blanks
168600111103     C                   EVAL      STB_BOR = pOutLblTyp                         * bolla Originale
168700111103     C                   ENDIF
168800111103     C*
168900090430     C                   IF        pOutLblTyp = 'O'
169000090430     C*
169100111103     C* Se è uno stato d chiusura spedizione
169200111025     C                   if        stbSTS = '9'
169300090430     C* Chiamata metodo GetLastChild
169400090430     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
169500090430     C                                                STBAAS
169600090430     C                                               :STBLNP
169700090430     C                                               :STBNRS
169800090430     C                                               :STBNSP
169900090430     C                                               :pOutAnnoFI
170000090430     C                                               :pOutLineaParFI
170100090430     C                                               :pOutSerieFI
170200090430     C                                               :pOutNumSpedFI
170300090430     C                                               :pOutDcmFI
170400090430     C                                               :pOutCcaFI))
170500090430     C* Se ultima figlia consegnata allora overrido i dati consegna della bolla originale relativa
170600090430     C                   if        pOutDcmFI > *zeros
170700090430     C                   if        pOutCcaFI = '2'
170800090430     C                   eval      stbCOS = 'RESO'
170900111025     C                   eval      wCOS   = 'RESO'
171000111103     C                   if        wDAS   = pOutDcmFI
171100090430     C                   eval      stbDAS = pOutDcmFI
171200090430     C                   eval      wDAS   = pOutDcmFI
171300130228     C                   endif
171400090430     C                   endif
171500100201     C                   if        pOutCcaFI = '6'
171600100201     C                   eval      stbCOS = 'AVARIA'
171700111025     C                   eval      wCOS   = 'AVARIA'
171800111103     C                   IF        STB_BOR = *blanks
171900100201     C                   eval      stbDAS = pOutDcmFI
172000100201     C                   eval      wDAS   = pOutDcmFI
172100111103     C                   ENDIF
172200100201     C                   endif
172300090430     C                   endif
172400111031     C*
172500111025     C                   endif
172600111031     C*
172700081112     C                   ENDIF
172800100806     C*
172900100806     C                   ENDIF
173000090430     C*
173100081112     C                   EVAL      stbFOP = DS_STBFOP
173200121010     C*
173300121010     C* **** FORZATURE "FINALI" ****
173400121012     C*
173500121012     C* - MIC
173600121010     C                   IF         stbPRS = '2'    and
173700121010     C                             (stbCOS = 'MIC'  or
173800121010     C                              stbCOS = 'NIC') and
173900121012     C                              stbORS >= 1200
174000170517     C                   EVAL      wORS   = 1159*100
174100121010     C                   EVAL      stbORS = 1159
174200121010     C                   ENDIF
174300090430     C*
174400090430     C* Verifico se tale stato è già presente nel file TISTB
174500121010     C                   EVAL      kORS = %int(wORS/100)
174600121008     C     KEYstb04      CHAIN     tistb04l
174700090430     C*
174800090430     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
174900121008     C                   IF        not %found(tistb04l)
175000081112     C*
175100031103     C* Scarico il buffer del file stati bolle (TISTB)
175200121008     C                   WRITE     tistb000
175300121008     C*
175400121008     C                   ELSE
175500121008     C                   IF        w§PPTUPD = 'U'
175600121008     C                   EVAL      tistbds_i = tistbds
175700121008     C                   UPDATE    tistb004
175800121008     C                   ENDIF
175900090430     C*
176000090430     C                   ENDIF
176100031103     C*
176200031103     C* Dopo la scrittura dello stato bolla corrente, se trattasi d stato bolla PERSONALIZZATO
176300031103     C* (campo STBTIS='9') verifico che il codice stato attribuito
176400031103     C* sia presente in tabella "STATI BOLLA RICAVATI" (Tab. "STB")
176500031103     C                   IF        stbTIS = '9'
176600031103     C                   EVAL      tbeCOD = 'STB'
176700031103     C                   EVAL      tbeKE1 = stbCOS
176800031103     C     KEYtbe01P     SETLL     tntbe01l
176900031103     C                   IF        %equal(tntbe01l)
177000031103     C                   ELSE
177100031103     C                   CLEAR                   DSSTB
177200031103     C                   EVAL      §STBDES = wDESTATO
177300031103     C                   EVAL      tbeUNI = DSSTB
177400031103     C                   WRITE     tntbe000
177500031103     C                   ENDIF
177600031103     C                   ENDIF
177700031031     C*
177800031031     C                   ENDSR
177900031031
178000031031
178100991027
178200991027      /TITLE Operazioni iniziali.
178300991027     C     *inzsr        BEGSR
178400991027     C*
178500991027     C     *ENTRY        PLIST
178600991027     C                   parm                    prmppt
178700991027     C     wrkesito      parm      wrkesito      prmesito
178800031103     C*
178900031103     C* Ridefinisco subito i parametri d traduzione
179000031103     C                   EVAL      DSPPT = prmppt
179100040308     C*
179200040308     C* Calcola la data corrente
179300040308     C                   time                    wn14             14 0
179400040308     C                   movel     wn14          oracor            6 0          *ORA  (6) IN H/M/S
179500100713     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179600100713     C                   eval      datcor = %dec(%date() : *ISO)
179700030709     C*
179800030709     C* Definizione chiavi
179900030924     C*
180000031103     C* Chiave su TNTBE01L - Parziale
180100031103     C     KEYtbe01P     KLIST
180200031103     C                   KFLD                    tbeCOD
180300031103     C                   KFLD                    tbeKE1
180400031014     C* Chiave su FNEVB01L - Parziale
180500031014     C     KEYevb01P     KLIST
180600031106     C                   KFLD                    wAAS
180700031106     C                   KFLD                    wLNP
180800031106     C                   KFLD                    wNRS
180900031106     C                   KFLD                    wNSP
181000110929     C* Chiave su FNEVB04L - Parziale
181100110929     C     KEYevb04P     KLIST
181200111031     C                   KFLD                    evb4_EVBAAS
181300111031     C                   KFLD                    evb4_EVBLNP
181400111031     C                   KFLD                    evb4_EVBNRS
181500111031     C                   KFLD                    evb4_EVBNSP
181600111031     C                   KFLD                    evb4_EVBCEV
181700111031     C                   KFLD                    evb4_EVBDEV
181800111031     C* Chiave su TIGCP51L - Completa
181900050310     C     KEYgcp51      KLIST
182000031106     C                   KFLD                    wAAS
182100031106     C                   KFLD                    wLNP
182200031106     C                   KFLD                    wNRS
182300031106     C                   KFLD                    wNSP
182400031031     C                   KFLD                    wFRG
182500041015     C* Chiave su FNDCT02L - Completa
182600041015     C     KEYdct02      KLIST
182700041015     C                   KFLD                    wAAS
182800041015     C                   KFLD                    wLNP
182900041015     C                   KFLD                    wNRS
183000041015     C                   KFLD                    wNSP
183100120424     C* Chiave su FIAR531C - Parziale
183200120424     C     KEYar531P     KLIST
183300120424     C                   KFLD                    wAAS
183400120424     C                   KFLD                    wLNP
183500120424     C                   KFLD                    wNRS
183600120424     C                   KFLD                    wNSP
183700120424     C                   KFLD                    ar5TRD
183800111031     C* Chiave su TITAS30C - Parziale
183900111031     C     KEYtas30P     KLIST
184000111031     C                   KFLD                    wAAS
184100111031     C                   KFLD                    wLNP
184200111031     C                   KFLD                    wNRS
184300111031     C                   KFLD                    wNSP
184400050224     C* Chiave su TISTB04L - Completa
184500050224     C     KEYstb04      KLIST
184600050224     C                   KFLD                    w§PPTKSU
184700031106     C                   KFLD                    wAAS
184800031106     C                   KFLD                    wLNP
184900031106     C                   KFLD                    wNRS
185000031106     C                   KFLD                    wNSP
185100031103     C                   KFLD                    wDAS
185200121010     C                   KFLD                    kORS
185300031103     C                   KFLD                    wCOS
185400130305     C                   KFLD                    wPRS
185500100806     C* Chiave su FNORM01L - Completa
185600100806     C     KEYorm01      KLIST
185700100806     C                   KFLD                    vapPOE
185800100806     C                   KFLD                    vapNSR
185900100806     C                   KFLD                    vapNOR
186000100806     C                   KFLD                    vapNRV
186100111103     C* Chiave su FNARB01L - Completa
186200111103     C     KEYarb01C     KLIST
186300111103     C                   KFLD                    wAAS
186400111103     C                   KFLD                    wLNP
186500111103     C                   KFLD                    wNRS
186600111103     C                   KFLD                    wNSP
186700160211     C* Chiave su FIPCT03L - Parziale
186800160211     C     KEYpct03P     KLIST
186900130227     C                   KFLD                    wAAS
187000130227     C                   KFLD                    wLNP
187100130227     C                   KFLD                    wNRS
187200130227     C                   KFLD                    wNSP
187300130227     C                   KFLD                    pctTRD
187400111103     C*
187500111103     C                   ENDSR
187600130227     C***
