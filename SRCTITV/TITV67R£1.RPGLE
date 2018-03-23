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
008800160211     D wPCTCMC         S                   like(§PCTCMC) inz
008900031103
009000031103     D****
009100031103     D* VARIABILI DI WRK
009200031103     D****
009300031103     D wDESTATO        S             70
009400130227
009500111103
009600111103     D LibFileARB01    s             21A   inz
009700160211     D LibFilePCT03    s             21A   inz
009800111103     D currSysNeta     s              8A   inz('*NULL')
009900111103
010000031103
010100031103     D****
010200031103     D* DS RIDEFINIZIONE PARAMETRI TRADUZIONE RICEVUTI IN INPUT
010300031103     D****
010400031103     D DSPPT           DS
010500031103     D  w§PPTTIP                      2
010600031104     D  w§PPTKSU                      7  0
010700121008     D  w§PPTUPD                      1
010800130227     D  w§PPTPDA                      1
010900160223     D  w§PPTCOLLI                    1
011000081112
011100081112
011200081112     D*------------------
011300081112     D* LINKING A DEFINIZIONI ESTERNE
011400081112     D*------------------
011500081112     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
011600090430     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
011700111103     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
011800111103     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
011900160223     D/COPY GAITRASRC/SRCPROTOPI,UBCOL00R
012000031031
012100081112
012200081112
012300031031     C***********
012400030924     C* MAIN/
012500031031     C***********
012600111103     C*
012700111103     C* Reperisco il sistema AS/400 corrente
012800111103     C                   callp     UBRTVNETA_Rtv(currSysNeta)
012900111103     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
013000111103     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
013100160211     C                   eval      LibFilePCT03 = 'FILTRA201/FIPCT03L'
013200111103     C                   else
013300111103     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
013400160211     C                   eval      LibFilePCT03 = 'FILTRAPRD/FIPCT03L'
013500111103     C                   endif
013600111103     C*
013700111103     C* Apertura file "overraidati"
013800111103     C                   if        not %open(FNARB01L)
013900111103     C                   open      FNARB01L
014000111103     C                   endif
014100160211     C                   if        not %open(FIPCT03L)
014200160211     C                   open      FIPCT03L
014300130227     C                   endif
014400031103     C*
014500031103     C* Eseguo routine d elaborazione
014600031103     C                   EXSR      verticalizza
014700111103     C*
014800111103     C* Chiusura file "overraidati"
014900111103     C                   if        %open(FNARB01L)
015000111103     C                   close     FNARB01L
015100111103     C                   endif
015200160211     C                   if        %open(FIPCT03L)
015300160211     C                   close     FIPCT03L
015400130227     C                   endif
015500090323     C*
015600090323     C* Sancisco il commit
015700090323     C                   COMMIT
015800031103     C*
015900921023     C                   SETON                                        LR
016000031103
016100031103
016200031103
016300031103     C     verticalizza  BEGSR
016400031103     C*
016500031103     C                   IF        w§PPTTIP = 'BT'
016600031103     C                   EXSR      verticalizBT
016700031103     C                   ENDIF
016800031103     C                   IF        w§PPTTIP = 'VC'
016900110926     C                   EXSR      verticalizALL
017000031103     C                   ENDIF
017100110926     C                   IF        w§PPTTIP = 'VG'
017200110926     C                   EXSR      verticalizALL
017300110926     C                   ENDIF
017400100806     C                   IF        w§PPTTIP = 'VP'
017500100806     C                   EXSR      verticalizVP
017600100806     C                   ENDIF
017700130227     C                   IF        w§PPTTIP = 'DA'
017800130227     C                   EXSR      verticalizDA
017900130227     C                   ENDIF
018000031103     C*
018100031103     C                   ENDSR
018200031103
018300031103
018400031103
018500031103     C     verticalizBT  BEGSR
018600031103     C*
018700090320     C                   READ      TIVGDTMP
018800161201     C                   DOW       not %eof(TIVGDTMP)
018900120727     C*
019000120727     C* Ignoro record "*VOID"
019100120727     C                   IF        %trim(vgdDTA) = '*VOID'
019200120727     C                   ELSE
019300090320     C*
019400090320     C                   EVAL      fnvab00t = vgdDTA
019500031106     C*
019600031106     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
019700031106     C                   EVAL      wAAS = vabAAS
019800031106     C                   EVAL      wLNP = vabLNP
019900110927     C                   EVAL      wLNA = vabLNA
020000031106     C                   EVAL      wNRS = vabNRS
020100031106     C                   EVAL      wNSP = vabNSP
020200050224     C                   EVAL      wRMN = vabRMN
020300050224     C                   EVAL      wRMA = vabRMA
020400031106     C                   EVAL      wKSC = vabCCM
020500111031     C*
020600111031     C* Valorizzo subito la data della bolla
020700111031     C                   EVAL      wDSP = vabAAS*10000+vabMGS
020800031103     C*
020900031103     C* X ogni bolla verticalizzo lo stato (da file FNVAB00T) nel file TISTB
021000050202     C                   EXSR      EXEVAB
021100160223     C*
021200161130     C* X ogni bolla verticalizzo le informazioni relative ai singoli colli
021300160223     C                   EXSR      EXECOLLI
021400120727     C*
021500120727     C                   ENDIF
021600031103     C*
021700090320     C                   DELETE    TIVGD000
021800090320     C*
021900090320     C                   READ      TIVGDTMP
022000031103     C                   ENDDO
022100031103     C*
022200031103     C                   EVAL      wrkesito = '0'
022300031103     C*
022400031103     C                   ENDSR
022500991027
022600030924
022700030924
022800110926     C     verticalizALL BEGSR
022900991027     C*
023000080924     C                   READ      TIVGDTMP
023100080924     C                   DOW       not %eof(TIVGDTMP)
023200120727     C*
023300120727     C* Ignoro record "*VOID"
023400120727     C                   IF        %trim(vgdDTA) = '*VOID'
023500120727     C                   ELSE
023600080924     C*
023700110927     C                   SELECT
023800110927     C                   WHEN      w§PPTTIP = 'VC'
023900110927     C                   EVAL      fnvac00t = vgdDTA
024000110927     C*
024100110927     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
024200110927     C                   EVAL      wAAS = vacAAS
024300110927     C                   EVAL      wLNP = vacLNP
024400110927     C                   EVAL      wLNA = vacLNA
024500110927     C                   EVAL      wNRS = vacNRS
024600110927     C                   EVAL      wNSP = vacNSP
024700110927     C                   EVAL      wRMN = vacRMN
024800110927     C                   EVAL      wRMA = vacRMA
024900110927     C                   EVAL      wKSC = vacCCM
025000111031     C*
025100111031     C* Valorizzo subito la data della bolla
025200111031     C                   EVAL      wDSP = vacAAS*10000+vacMGS
025300120703     C*
025400120703     C* Aggancio sempre anche la bolla (in arrivo o eventualmente in sede)
025500120703     C                   EVAL      wNDC = *zeros
025600120703     C     KEYarb01C     CHAIN     fnarb01l
025700120703     C                   IF        %found(fnarb01l)
025800120703     C                   EVAL      wNDC = arbNDC
025900161118     C                   EVAL      wLNA = arbLNA
026000161118     C                   EVAL      wRMN = arbRMN
026100161118     C                   EVAL      wRMA = arbRMA
026200161123     C                   IF        wKSC = *zeros
026300161123     C                   IF        arbCCM > *zeros
026400161118     C                   EVAL      wKSC = arbCCM
026500161123     C                   ELSE
026600161123     C                   EVAL      wKSC = arbKSC
026700161123     C                   ENDIF
026800161123     C                   ENDIF
026900161118     C                   EVAL      wDSP = arbAAS*10000+arbMGS
027000120703     C                   ELSE
027100120703     C     KEYtas30P     CHAIN     TITAS30C
027200120703     C                   IF        %found(TITAS30C)
027300120703     C                   EVAL      wNDC = tasNDC
027400161118     C                   EVAL      wLNA = tasLNA
027500161118     C                   EVAL      wRMN = tasRMN
027600161123     C                   IF        wKSC = *zeros
027700161123     C                   IF        tasCCM > *zeros
027800161123     C                   EVAL      wKSC = tasCCM
027900161123     C                   ELSE
028000161123     C                   EVAL      wKSC = tasKSC
028100161123     C                   ENDIF
028200161123     C                   ENDIF
028300161118     C                   EVAL      wDSP = tasAAS*10000+tasMGS
028400120703     C                   ENDIF
028500120703     C                   ENDIF
028600120424     C*
028700120424     C* X ogni bolla verticalizzo i dati da file FIAR500F - trk GEN
028800120424     C                   EXSR      EXEAR5GEN
028900110927     C*
029000110927     C* X ogni bolla verticalizzo gli eventi (da file FNEVB01L) nel file TISTB
029100110927     C                   EXSR      EXEEVB
029200110927     C*
029300110927     C* X ogni bolla verticalizzo le giacenze (da file TIGCP51L) nel file TISTB
029400110927     C                   EXSR      EXEGCP
029500110927     C*
029600110927     C* X ogni bolla verticalizzo i danni (da file FNDCT00T) nel file TISTB
029700110927     C                   EXSR      EXEDCT
029800110927     C*
029900110927     C* X ogni bolla verticalizzo lo stato (da file FNVAC00T) nel file TISTB
030000110927     C                   EXSR      EXEVAC
030100160223     C*
030200160223     C* X ogni bolla verticalizzo le informazioni relative ai singoli colli
030300160223     C                   EXSR      EXECOLLI
030400110927     C*
030500110927     C                   WHEN      w§PPTTIP = 'VG'
030600110927     C                   EVAL      fnvag00t = vgdDTA
030700110927     C*
030800110927     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
030900110927     C                   EVAL      wAAS = vagAAS
031000110927     C                   EVAL      wLNP = vagLNP
031100110927     C                   EVAL      wLNA = vagLNA
031200110927     C                   EVAL      wNRS = vagNRS
031300110927     C                   EVAL      wNSP = vagNSP
031400110927     C                   EVAL      wRMN = vagRMN
031500110927     C                   EVAL      wRMA = vagRMA
031600111020     C                   EVAL      wKSC = vagSCM
031700111031     C*
031800111031     C* Valorizzo subito la data della bolla
031900111031     C     KEYtas30P     CHAIN     TITAS30C
032000111031     C                   IF        %found(TITAS30C)
032100111031     C                   EVAL      wDSP = tasAAS*10000+tasMGS
032200111031     C                   ENDIF
032300110927     C*
032400110927     C* X ogni bolla verticalizzo gli eventi (da file FNEVB01L) nel file TISTB
032500110927     C                   EXSR      EXEEVB
032600110927     C*
032700110927     C* X ogni bolla verticalizzo le giacenze (da file TIGCP51L) nel file TISTB
032800110927     C                   EXSR      EXEGCP
032900110927     C*
033000110927     C                   ENDSL
033100120727     C*
033200120727     C                   ENDIF
033300031031     C*
033400080924     C                   DELETE    TIVGD000
033500031031     C*
033600080924     C                   READ      TIVGDTMP
033700031031     C                   ENDDO
033800031031     C*
033900031103     C                   EVAL      wrkesito = '0'
034000031031     C*
034100031031     C                   ENDSR
034200100806
034300100806
034400100806
034500100806     C     verticalizVP  BEGSR
034600100806     C*
034700100806     C                   READ      TIVGDTMP
034800100806     C                   DOW       not %eof(TIVGDTMP)
034900120727     C*
035000120727     C* Ignoro record "*VOID"
035100120727     C                   IF        %trim(vgdDTA) = '*VOID'
035200120727     C                   ELSE
035300100806     C*
035400100806     C                   EVAL      fnvap00t = vgdDTA
035500100806     C*
035600100806     C* Valorizzo subito la data ritiro orm
035700100806     C     KEYorm01      CHAIN     fnorm01l
035800100806     C                   IF        %found(fnorm01l)
035900100806     C                   EVAL      wDSP = ormDAR
036000100806     C*
036100100806     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
036200100806     C                   MOVEL     ormDAR        wAAS
036300100806     C                   EVAL      wLNP = vapPOE
036400100806     C                   EVAL      wNRS = vapNSR
036500100806     C                   EVAL      wNSP = vapNOR
036600100806     C                   EVAL      wRMN = %dec(%editc(vapPOE:'X')+
036700100806     C                                         %editc(vapNSR:'X')+
036800100806     C                                         %editc(vapNOR:'X')+
036900100806     C                                         %editc(vapNRV:'X'):14:0)
037000100806     C                   EVAL      wRMA = vapRFA
037100100806     C                   MOVEL     ormCOR        wKSC
037200100806     C*
037300100806     C* X ogni orm verticalizzo lo stato (da file FNVAP00T) nel file TISTB
037400100806     C                   EXSR      EXEVAP
037500100806     C
037600100806     C                   ENDIF
037700100806     C*
037800120727     C                   ENDIF
037900120727     C*
038000100806     C                   DELETE    TIVGD000
038100100806     C*
038200100806     C                   READ      TIVGDTMP
038300100806     C                   ENDDO
038400100806     C*
038500100806     C                   EVAL      wrkesito = '0'
038600100806     C*
038700100806     C                   ENDSR
038800130227
038900130227
039000130227
039100130227     C     verticalizDA  BEGSR
039200130227     C*
039300130227     C                   READ      TIVGDTMP
039400130227     C                   DOW       not %eof(TIVGDTMP)
039500130227     C*
039600130227     C* Ignoro record "*VOID"
039700130227     C                   IF        %trim(vgdDTA) = '*VOID'
039800130227     C                   ELSE
039900130227     C*
040000130227     C                   EVAL      fnvac00t = vgdDTA
040100130227     C*
040200130227     C* Imposto subito la chiave bolla relativamente al file da tradurre corrente
040300130227     C                   EVAL      wAAS = vacAAS
040400130227     C                   EVAL      wLNP = vacLNP
040500130227     C                   EVAL      wLNA = vacLNA
040600130227     C                   EVAL      wNRS = vacNRS
040700130227     C                   EVAL      wNSP = vacNSP
040800130227     C                   EVAL      wRMN = vacRMN
040900130227     C                   EVAL      wRMA = vacRMA
041000130227     C                   EVAL      wKSC = vacCCM
041100130227     C*
041200130227     C* Valorizzo subito la data della bolla
041300130227     C                   EVAL      wDSP = vacAAS*10000+vacMGS
041400130227     C*
041500160223     C* X ogni bolla verticalizzo lo stato (da file FIPCT03L) nel file TISTB
041600130227     C                   EXSR      EXEPCT
041700160223     C*
041800160223     C* X ogni bolla verticalizzo le informazioni relative ai singoli colli
041900160223     C                   EXSR      EXECOLLI
042000130227     C*
042100130227     C                   ENDIF
042200130227     C*
042300130227     C                   DELETE    TIVGD000
042400130227     C*
042500130227     C                   READ      TIVGDTMP
042600130227     C                   ENDDO
042700130227     C*
042800130227     C                   EVAL      wrkesito = '0'
042900130227     C*
043000130227     C                   ENDSR
043100031103
043200031103
043300031103
043400041014     C     EXEVAB        BEGSR
043500111031     C*
043600111031     C* Inizializzo DS flag operativi
043700111031     C                   CLEAR                   DS_STBFOP
043800040322     C*
043900040322     C* Imposto subito alcuni valori
044000040322     C                   EVAL      wDAS = vabAAS*10000+vabMGS
044100131014     C***                EVAL      wORS = *zeros
044200131024     C                   EVAL      wORS = 210000
044300031103     C*
044400031103     C* Dal FNVAB reperisco solo gli stati di presa in carico bolla, x cui:
044500031103     C*
044600031103     C****
044700031103     C* stato INCARICO1 (bolla presa in carico; con LNP=LNA)
044800031103     C****
044900031103     C* - condizioni:
045000031103     C*   LNP = LNA
045100031103     C*
045200031103     C* Verifico le condizione che soddisfano lo stato INCARICO1
045300031103     C                   IF        vabLNP = vabLNA
045400031103     C                   EVAL      wTIS = '9'
045500031103     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
045600031103     C                                        '(LNP=LNA)'
045700041014     C                   EVAL      wPRS = '1'
045800031103     C                   EVAL      wCOS = 'INCARICO1'
045900090430     C*
046000031103     C                   CLEAR                   tistb000
046100121008     C                   CLEAR                   tistbds
046200031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
046300031103     C                   EVAL      stbSTS = '1'
046400031103     C                   EXSR      WRIREC
046500031103     C                   ENDIF
046600031103     C****
046700031103     C*
046800031103     C****
046900031103     C* stato INCARICO2 (bolla presa in carico; con LNP<>LNA)
047000031103     C****
047100031103     C* - condizioni:
047200031103     C*   LNP <> LNA
047300031103     C*
047400031103     C* Verifico le condizione che soddisfano lo stato INCARICO2
047500031106     C                   IF        vabLNP <> vabLNA
047600031103     C                   EVAL      wTIS = '9'
047700031103     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
047800031103     C                                        '(LNP<>LNA)'
047900041014     C                   EVAL      wPRS = '1'
048000031103     C                   EVAL      wCOS = 'INCARICO2'
048100031103     C*
048200031103     C* - inizializzo il buffer del file stati bolle (TISTB)
048300031103     C                   CLEAR                   tistb000
048400121008     C                   CLEAR                   tistbds
048500031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
048600031103     C                   EVAL      stbSTS = '1'
048700031103     C                   EXSR      WRIREC
048800031103     C                   ENDIF
048900031103     C****
049000031103     C*
049100031103     C                   ENDSR
049200031031
049300031031
049400031031
049500031031     C     EXEEVB        BEGSR
049600031031     C*
049700031031     C* Reperisco gli eventi relativi alla bolla corrente
049800031014     C     KEYevb01P     SETLL     fnevb01l
049900031014     C                   IF        %equal(fnevb01l)
050000031014     C     KEYevb01P     READE     fnevb01l
050100031031     C                   DOW       not %eof(fnevb01l)
050200111103     C*
050300111103     C                   SETOFF                                       50
050400031031     C*
050500031031     C* Se evento NN annullato lo considero
050600031031     C                   IF        evbATB = *blanks
050700111031     C*
050800111031     C* Inizializzo DS flag operativi
050900111031     C                   CLEAR                   DS_STBFOP
051000111031     C*
051100111031     C* Verifico se trattasi d evento derivato da bolla figlia
051200111031     C*
051300111031     C* Chiamata metodo GetLblTyp
051400111031     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
051500111031     C                                                wAAS
051600111031     C                                               :wLNP
051700111031     C                                               :wNRS
051800111031     C                                               :wNSP
051900111031     C                                               :pOutLblTyp
052000111031     C                                               :pOutAnnoBO
052100111031     C                                               :pOutLineaParBO
052200111031     C                                               :pOutSerieBO
052300111031     C                                               :pOutNumSpedBO
052400111031     C                                               :pOutDcmBO
052500111031     C                                               :pOutCcaBO
052600111031     C                                               :pOutRblBO))
052700111031     C*
052800111031     C                   if        wEsito1 = '0'
052900170802     C*
053000170802     C* Se trattasi d reso della bolla figlia => forzo inserimento stato di "INRESO"
053100170802     C                   if        pOutLblTyp = 'F' AND
053200170802     C                             pOutCcaBO = '2'
053300170802     C                   EVAL      STB_BOR = 'F'                                * bolla figl.
053400170802     C*
053500170802     C* Leggo la bolla in arrivo (no SEDE !!!)
053600170802     C     KEYarb01C     chain     fnarb01l
053700170802     C                   if        %found(fnarb01l)
053800170802     C                   EVAL      wTIS = '1'
053900170802     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON RESO'
054000170802     C                   EVAL      wDAS = arbDCM
054100170802     C                   EVAL      wORS = arbHMC*100
054200170802     C                   EVAL      wPRS = '4'
054300170802     C                   EVAL      wCOS = 'INRESO'
054400170802     C*
054500170802     C* - inizializzo il buffer del file stati bolle (TISTB)
054600170802     C                   CLEAR                   tistb000
054700170802     C                   CLEAR                   tistbds
054800170802     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
054900170802     C                   EVAL      stbSTS = '2'
055000170802     C                   EXSR      WRIREC
055100170802     C*
055200170802     C                   else
055300170802     C                   SETON                                        50
055400170802     C                   endif
055500170802     C                   endif
055600111103     C*
055700111103     C* Se trattasi d reso della bolla originale => forzo inserimento stato di reso
055800111103     C                   if        pOutLblTyp = 'O' AND
055900111103     C                             pOutCcaBO = '2'
056000111103     C                   EVAL      STB_BOR = 'O'                                * bolla orig.
056100111103     C*
056200111103     C* Leggo la bolla in arrivo (no SEDE !!!)
056300111103     C     KEYarb01C     chain     fnarb01l
056400111103     C                   if        %found(fnarb01l)
056500111103     C                   EVAL      wTIS = '1'
056600111103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON RESO'
056700111103     C                   EVAL      wDAS = arbDCM
056800121010     C                   EVAL      wORS = arbHMC*100
056900111103     C                   EVAL      wPRS = '4'
057000111103     C                   EVAL      wCOS = 'RESO'
057100111103     C*
057200111103     C* - inizializzo il buffer del file stati bolle (TISTB)
057300111103     C                   CLEAR                   tistb000
057400121008     C                   CLEAR                   tistbds
057500111103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
057600111103     C                   EVAL      stbSTS = '9'
057700111103     C                   EXSR      WRIREC
057800111103     C*
057900111103     C                   else
058000111103     C                   SETON                                        50
058100111103     C                   endif
058200111103     C                   endif
058300111103     C*
058400111031     C                   if        pOutLblTyp = 'F'
058500111031     C                   EVAL      STB_BOR = 'F'                                * bolla Figlia
058600111031     C                   else
058700111103     C                   EVAL      STB_BOR = 'O'                                * bolla orig.
058800111031     C*
058900111031     C* Chiamata metodo GetLastChild
059000111031     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
059100111031     C                                                wAAS
059200111031     C                                               :wLNP
059300111031     C                                               :wNRS
059400111031     C                                               :wNSP
059500111031     C                                               :pOutAnnoFI
059600111031     C                                               :pOutLineaParFI
059700111031     C                                               :pOutSerieFI
059800111031     C                                               :pOutNumSpedFI
059900111031     C                                               :pOutDcmFI
060000111031     C                                               :pOutCcaFI))
060100111031     C                   if        wEsito2 = '0'
060200111031     C*
060300111031     C* Aggancio l'evento relativo della bolla figlia
060400111031     C                   EVAL      evb4_EVBAAS = pOutAnnoFI
060500111031     C                   EVAL      evb4_EVBLNP = pOutLineaParFI
060600111031     C                   EVAL      evb4_EVBNRS = pOutSerieFI
060700111031     C                   EVAL      evb4_EVBNSP = pOutNumSpedFI
060800111031     C                   EVAL      evb4_EVBCEV = evbCEV
060900111031     C                   EVAL      evb4_EVBDEV = evbDEV
061000111031     C*
061100111031     C     KEYevb04P     CHAIN     fnevb04l
061200111031     C                   IF        %found(fnevb04l)
061300111031     C                   IF        evb4_EVBNOT = evbNOT
061400111031     C                   EVAL      STB_BOR = 'F'                                * bolla Figlia
061500111031     C                   EVAL      STB_CCA = pOutCcaBO                          * CCA bolla orig.
061600111031     C                   ENDIF
061700111031     C                   ENDIF
061800111031     C*
061900111031     C                   endif
062000111031     C                   endif
062100111031     C                   endif
062200031031     C*
062300050224     C* Costruisco la chiave sul file TISTB04L
062400121019     C***                IF        evbDEV*10000+evbHEV >
062500121019     C***                          evbDTV*10000+evbORV
062600121012     C                   EVAL      wDAS = evbDEV
062700121019     C                   EVAL      wORS = evbHEV*100
062800121019     C***                ELSE
062900121019     C***                EVAL      wDAS = evbDTV
063000121019     C***                EVAL      wORS = evbORV
063100121019     C***                ENDIF
063200031103     C                   EVAL      wTIS = '1'
063300031103     C                   EVAL      wPRS = '2'
063400031103     C                   EVAL      wCOS = evbCEV
063500031031     C*
063600031031     C* - inizializzo il buffer del file stati bolle (TISTB)
063700031031     C                   CLEAR                   tistb000
063800121008     C                   CLEAR                   tistbds
063900031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
064000031031     C                   EVAL      stbSTS = '2'
064100031103     C                   EXSR      WRIREC
064200031031     C                   ENDIF
064300031031     C*
064400031031     C* Proseguo la lettura degli eventi x la bolla corrente
064500031014     C     KEYevb01P     READE     fnevb01l
064600031014     C                   ENDDO
064700031031     C                   ENDIF
064800991027     C*
064900910830     C                   ENDSR
065000031031
065100031031
065200031031
065300031031     C     EXEGCP        BEGSR
065400031031     C*
065500050310     C* Costruisco la chiave sul file TIGCP51L
065600031031     C                   EVAL      wFRG = *zeros
065700031031     C*
065800031104     C* Reperisco le giacenze relative alla bolla corrente
065900050310     C     KEYgcp51      SETLL     tigcp51l
066000050310     C                   IF        %equal(tigcp51l)
066100050310     C     KEYgcp51      READE     tigcp51l
066200050310     C                   DOW       not %eof(tigcp51l)
066300110929     C*
066400121010     C                   EVAL      wORS = 110000
066500031031     C*
066600031104     C* Se giacenza NN annullata la considero
066700031031     C                   IF        gcpATB = *blanks
066800111031     C*
066900111031     C* Inizializzo DS flag operativi
067000111031     C                   CLEAR                   DS_STBFOP
067100110929     C*
067200110929     C* Aggancio l'evento di apertura giacenza correlato
067300111031     C                   EVAL      evb4_EVBAAS = gcpAAS
067400111031     C                   EVAL      evb4_EVBLNP = gcpLNP
067500111031     C                   EVAL      evb4_EVBNRS = gcpNRS
067600111031     C                   EVAL      evb4_EVBNSP = gcpNSP
067700111031     C                   EVAL      evb4_EVBCEV = gcpCMC
067800111031     C                   EVAL      evb4_EVBDEV = gcpDUR
067900111031     C*
068000110929     C     KEYevb04P     CHAIN     fnevb04l
068100110929     C                   IF        %found(fnevb04l)
068200121008     C***                EVAL      wORS = evb4_evbHEV + 1
068300121019     C***                EVAL      wORS = %dec(%time(evbORV)+%minutes(1))
068400121019     C                   EVAL      wORS = %dec(%time(evbHEV*100)+%minutes(1))
068500110929     C                   ENDIF
068600121009     C*
068700121010     C                   Z-ADD     wORS          wORSgcp           6 0
068800031031     C*
068900050310     C* Effettuo considerazioni sul file TIGCP x detrminare il codice stato da attribuire
069000031103     C* alla bolla:
069100031031     C*
069200031031     C****
069300031103     C* stato GIACNODIS1 (giacenza aperta ma in attesa d disposizioni del cliente; con LNP=LNA)
069400031031     C****
069500031031     C* - condizioni:
069600050310     C*   data aper./riaper. giacenza > 0 AND
069700031031     C*   data disposizioni mittente (gcpDDM) = 0 AND
069800050614     C*   fase giacenza (gcpFAS) < 30 (ex = 020) AND
069900031031     C*   LNP = LNA
070000031103     C                   EVAL      wDAS = gcpDUR
070100031031     C*
070200031031     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
070300050614     C                   IF        wDAS   > *zeros AND
070400050614     C                             gcpDDM = *zeros AND
070500050614     C                             gcpFAS < 030    AND
070600110927     C                             wLNP = wLNA
070700031103     C                   EVAL      wTIS = '9'
070800031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA MA IN ATTESA ' +
070900031103     C                                        'DI DISPOSIZIONI DEL CLIENTE '  +
071000031103     C                                        '(LNP=LNA)'
071100031103     C                   EVAL      wPRS = '3'
071200031103     C                   EVAL      wCOS = 'GIACNODIS1'
071300121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
071400031031     C*
071500031031     C* - inizializzo il buffer del file stati bolle (TISTB)
071600031031     C                   CLEAR                   tistb000
071700121008     C                   CLEAR                   tistbds
071800031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
071900110927     C                   EVAL      stbSTS = '1'
072000031103     C                   EXSR      WRIREC
072100031031     C                   ENDIF
072200031031     C****
072300031031     C*
072400031031     C****
072500031103     C* stato GIACNODIS2 (giacenza aperta ma in attesa d disposizioni del cliente; con LNP<>LNA)
072600031031     C****
072700031031     C* - condizioni:
072800050310     C*   data aper./riaper. giacenza > 0 AND
072900031031     C*   data disposizioni mittente (gcpDDM) = 0 AND
073000050614     C*   fase giacenza (gcpFAS) < 30 (ex = 020) AND
073100031031     C*   LNP <> LNA
073200031103     C                   EVAL      wDAS = gcpDUR
073300031031     C*
073400031031     C* Verifico le condizione che soddisfano lo stato GIACNODIS2
073500050614     C                   IF        wDAS    > *zeros AND
073600050614     C                             gcpDDM  = *zeros AND
073700050614     C                             gcpFAS  < 030    AND
073800110927     C                             wLNP <> wLNA
073900031103     C                   EVAL      wTIS = '9'
074000031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA MA IN ATTESA ' +
074100031103     C                                        'DI DISPOSIZIONI DEL CLIENTE '  +
074200031103     C                                        '(LNP<>LNA)'
074300031103     C                   EVAL      wPRS = '3'
074400031103     C                   EVAL      wCOS = 'GIACNODIS2'
074500121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
074600031031     C*
074700031031     C* - inizializzo il buffer del file stati bolle (TISTB)
074800031031     C                   CLEAR                   tistb000
074900121008     C                   CLEAR                   tistbds
075000031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
075100110927     C                   EVAL      stbSTS = '1'
075200031103     C                   EXSR      WRIREC
075300031031     C                   ENDIF
075400031031     C****
075500031031     C*
075600031031     C****
075700031103     C* stato GIACSIDIS1 (giacenza aperta e disposizioni del cliente pervenute; con LNP=LNA)
075800031031     C****
075900031031     C* - condizioni:
076000050310     C*   data aper./riaper. giacenza > 0 AND
076100031031     C*   data disposizioni mittente (gcpDDM) > 0 AND
076200050614     C*   fase giacenza (gcpFAS) >= 030 AND
076300031031     C*   LNP = LNA
076400110927     C***                EVAL      wDAS = gcpDUR
076500110927     C                   EVAL      wDAS = gcpDDM
076600031031     C*
076700031031     C* Verifico le condizione che soddisfano lo stato GIACSIDIS1
076800050614     C                   IF        wDAS    > *zeros AND
076900050614     C                             gcpDDM  > *zeros AND
077000050614     C                             gcpFAS >= 030    AND
077100110927     C                             wLNP  = wLNA
077200031103     C                   EVAL      wTIS = '9'
077300031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA E DISPOSIZIONI' +
077400031103     C                                        ' DEL CLIENTE PERVENUTE '        +
077500031103     C                                        '(LNP=LNA)'
077600031103     C                   EVAL      wPRS = '3'
077700050614     C                   EVAL      wCOS = 'GIACSIDIS1'
077800121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
077900031031     C*
078000031031     C* Verifico se tale stato è già presente nel file TISTB
078100121010     C                   EVAL      kORS = %int(wORS/100)
078200050224     C     KEYstb04      CHAIN     tistb04l
078300031031     C*
078400031031     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
078500121008     C                   IF        not %found(tistb04l)
078600050614     C*
078700050614     C* Verifico anche se precedente stato analogo 'GIACNODIS' è già stato dato
078800050614     C* se non già scritto => scrivo anche quello
078900050614     C                   EVAL      wCOS = 'GIACNODIS1'
079000121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
079100050614     C* Verifico se tale stato è già presente nel file TISTB
079200121010     C                   EVAL      kORS = %int(wORS/100)
079300050614     C     KEYstb04      CHAIN     tistb04l
079400050614     C                   IF        not %found(tistb04l)
079500050614     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
079600050614     C                   CLEAR                   tistb000
079700121008     C                   CLEAR                   tistbds
079800110927     C                   EVAL      wDAS = gcpDUR
079900110927     C                   EVAL      stbSTS = '1'
080000050614     C                   EXSR      WRIREC
080100050614     C                   ENDIF
080200050614     C*
080300031031     C* - inizializzo il buffer del file stati bolle (TISTB)
080400031031     C                   CLEAR                   tistb000
080500121008     C                   CLEAR                   tistbds
080600050614     C                   EVAL      wCOS = 'GIACSIDIS1'
080700121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
080800031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
080900110927     C                   EVAL      wDAS = gcpDDM
081000031031     C                   EVAL      stbSTS = '2'
081100031103     C                   EXSR      WRIREC
081200031031     C                   ENDIF
081300031031     C                   ENDIF
081400031031     C****
081500031031     C*
081600031031     C****
081700031103     C* stato GIACSIDIS2 (giacenza aperta e disposizioni del cliente pervenute; con LNP<>LNA)
081800031031     C****
081900031031     C* - condizioni:
082000050310     C*   data aper./riaper. giacenza > 0 AND
082100031031     C*   data disposizioni mittente (gcpDDM) > 0 AND
082200050614     C*   fase giacenza (gcpFAS) >= 030 AND
082300031031     C*   LNP <> LNA
082400110927     C***                EVAL      wDAS = gcpDUR
082500110927     C                   EVAL      wDAS = gcpDDM
082600031031     C*
082700031031     C* Verifico le condizione che soddisfano lo stato GIACSIDIS2
082800050614     C                   IF        wDAS    > *zeros AND
082900050614     C                             gcpDDM  > *zeros AND
083000050614     C                             gcpFAS >= 030    AND
083100110927     C                             wLNP <> wLNA
083200031103     C                   EVAL      wTIS = '9'
083300031104     C                   EVAL      wDESTATO = 'GIACENZA APERTA E DISPOSIZIONI' +
083400031103     C                                        ' DEL CLIENTE PERVENUTE '        +
083500031103     C                                        '(LNP<>LNA)'
083600031103     C                   EVAL      wPRS = '3'
083700031103     C                   EVAL      wCOS = 'GIACSIDIS2'
083800121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
083900031031     C*
084000031031     C* Verifico se tale stato è già presente nel file TISTB
084100121010     C                   EVAL      kORS = %int(wORS/100)
084200121008     C     KEYstb04      CHAIN     tistb04l
084300031031     C*
084400031031     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
084500050224     C                   IF        not %found(tistb04l)
084600050614     C*
084700050614     C* Verifico anche se precedente stato analogo 'GIACNODIS' è già stato dato
084800050614     C* se non già scritto => scrivo anche quello
084900050614     C                   EVAL      wCOS = 'GIACNODIS2'
085000121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(2))
085100050614     C* Verifico se tale stato è già presente nel file TISTB
085200121010     C                   EVAL      kORS = %int(wORS/100)
085300050614     C     KEYstb04      CHAIN     tistb04l
085400050614     C                   IF        not %found(tistb04l)
085500050614     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
085600050614     C                   CLEAR                   tistb000
085700121008     C                   CLEAR                   tistbds
085800110927     C                   EVAL      wDAS = gcpDUR
085900110927     C                   EVAL      stbSTS = '1'
086000050614     C                   EXSR      WRIREC
086100050614     C                   ENDIF
086200050614     C*
086300031031     C* - inizializzo il buffer del file stati bolle (TISTB)
086400031031     C                   CLEAR                   tistb000
086500121008     C                   CLEAR                   tistbds
086600050614     C                   EVAL      wCOS = 'GIACSIDIS2'
086700121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
086800031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
086900110927     C                   EVAL      wDAS = gcpDDM
087000031031     C                   EVAL      stbSTS = '2'
087100031103     C                   EXSR      WRIREC
087200031031     C                   ENDIF
087300031031     C                   ENDIF
087400110926     C****
087500110927     C*
087600110927     C****
087700110927     C* stato apertura GIACENZA NO DISPOSIZIONI (standard)
087800110927     C****
087900110927     C* - condizioni:
088000110927     C*   data chiusura giacenza > 0
088100110927     C                   EVAL      wDAS = gcpDUR
088200110927     C*
088300110928     C* Verifico le condizione che soddisfano lo stato
088400110927     C                   IF        wDAS   > *zeros
088500110927     C                   EVAL      wTIS = '1'
088600110927     C                   EVAL      wPRS = '3'
088700110928     C                   EVAL      wCOS = 'GIACAPE'+gcpCMC
088800121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(1))
088900110927     C*
089000110927     C* - inizializzo il buffer del file stati bolle (TISTB)
089100110927     C                   CLEAR                   tistb000
089200121008     C                   CLEAR                   tistbds
089300110927     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
089400110927     C                   EVAL      stbSTS = '1'
089500110927     C                   EXSR      WRIREC
089600110927     C                   ENDIF
089700110927     C****
089800110927     C*
089900110927     C****
090000110927     C* stato apertura GIACENZA SI DISPOSIZIONI (standard)
090100110927     C****
090200110927     C* - condizioni:
090300110927     C*   data chiusura giacenza > 0
090400110927     C                   EVAL      wDAS = gcpDDM
090500110927     C*
090600110928     C* Verifico le condizione che soddisfano lo stato
090700110927     C                   IF        wDAS   > *zeros
090800110927     C                   EVAL      wTIS = '1'
090900110927     C                   EVAL      wPRS = '3'
091000110927     C                   EVAL      wCOS = 'GIACDIS'+gcpCMC
091100121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(3))
091200110927     C*
091300110927     C* - inizializzo il buffer del file stati bolle (TISTB)
091400110927     C                   CLEAR                   tistb000
091500121008     C                   CLEAR                   tistbds
091600110927     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
091700110927     C                   EVAL      stbSTS = '2'
091800110927     C                   EXSR      WRIREC
091900110927     C                   ENDIF
092000110927     C****
092100110926     C*
092200110926     C****
092300110926     C* stato chiusura GIACENZA (standard)
092400110926     C****
092500110926     C* - condizioni:
092600110926     C*   data chiusura giacenza > 0
092700110926     C                   EVAL      wDAS = gcpDCG
092800110926     C*
092900110928     C* Verifico le condizione che soddisfano lo stato
093000110926     C                   IF        wDAS   > *zeros
093100110926     C                   EVAL      wTIS = '1'
093200110926     C                   EVAL      wPRS = '3'
093300110926     C                   EVAL      wCOS = 'GIACEND'+gcpCFG
093400121010     C                   EVAL      wORS = %dec(%time(wORSgcp)+%minutes(4))
093500110926     C*
093600110926     C* - inizializzo il buffer del file stati bolle (TISTB)
093700110926     C                   CLEAR                   tistb000
093800121008     C                   CLEAR                   tistbds
093900110926     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
094000111025     C                   EVAL      stbSTS = '2'
094100110926     C                   EXSR      WRIREC
094200110926     C                   ENDIF
094300031031     C****
094400031031     C                   ENDIF
094500031031     C*
094600031031     C* Proseguo la lettura delle giacenze x la bolla corrente
094700050310     C     KEYgcp51      READE     tigcp51l
094800031031     C                   ENDDO
094900031031     C                   ENDIF
095000031031     C*
095100031031     C                   ENDSR
095200041015
095300041015
095400041015
095500041015     C     EXEDCT        BEGSR
095600041015     C*
095700041015     C* Reperisco le C/A presenti relative alla bolla corrente
095800041015     C     KEYdct02      SETLL     fndct02l
095900041015     C                   IF        %equal(fndct02l)
096000041207     C*
096100041207     C* Inizializzo la ds d salvataggio
096200041207     C                   CLEAR                   fndctds_w
096300041207     C*
096400041207     C* Ciclo d lettura
096500041015     C     KEYdct02      READE     fndct02l
096600041015     C                   DOW       not %eof(fndct02l)
096700041015     C*
096800041015     C* Se C/A NN annullata la considero
096900041015     C                   IF        dctATB = *blanks
097000111031     C*
097100111031     C* Inizializzo DS flag operativi
097200111031     C                   CLEAR                   DS_STBFOP
097300041015     C*
097400041015     C* Salvo il buffer del record
097500041015     C                   EVAL      fndctds_w = fndctds
097600041015     C                   ENDIF
097700041015     C*
097800041015     C* Proseguo la lettura delle C/A x la bolla corrente
097900041015     C     KEYdct02      READE     fndct02l
098000041015     C                   ENDDO
098100041015     C*
098200041015     C* AL termine della lettura d tutte le C/A sulla bolla corrente considero l'ultima...
098300041015     C*
098400050224     C* Costruisco la chiave sul file TISTB04L
098500041015     C                   EVAL      wDAS = w_dctAAC*10000 + w_dctMGC
098600041015     C                   EVAL      wORS = *zeros
098700041015     C                   EVAL      wTIS = '1'
098800041015     C                   EVAL      wPRS = '5'
098900041015     C                   EVAL      wCOS = w_dctTAD
099000041015     C*
099100041015     C* - inizializzo il buffer del file stati bolle (TISTB)
099200041015     C                   CLEAR                   tistb000
099300121008     C                   CLEAR                   tistbds
099400041015     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
099500041015     C                   EVAL      stbSTS = '2'
099600041015     C                   EXSR      WRIREC
099700041015     C*
099800041015     C                   ENDIF
099900041015     C*
100000041015     C                   ENDSR
100100031031
100200031031
100300031031
100400041207     C     EXEVAC        BEGSR
100500111031     C*
100600111031     C* Inizializzo DS flag operativi
100700111031     C                   CLEAR                   DS_STBFOP
100800041014     C*
100900041014     C****
101000041014     C* stato INCARICO1 (bolla presa in carico; con LNP=LNA)
101100041014     C****
101200041014     C* - condizioni:
101300041014     C*   LNP = LNA
101400041014     C*
101500041014     C* Verifico le condizione che soddisfano lo stato INCARICO1
101600041014     C                   IF        vacLNP = vacLNA
101700041014     C                   EVAL      wTIS = '9'
101800041014     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
101900041014     C                                        '(LNP=LNA)'
102000041207     C                   EVAL      wDAS = vacAAS*10000+vacMGS
102100131030     C                   EVAL      wORS = 210000
102200110926     C                   EVAL      wPRS = '4'
102300041014     C                   EVAL      wCOS = 'INCARICO1'
102400041014     C*
102500041014     C* - inizializzo il buffer del file stati bolle (TISTB)
102600041014     C                   CLEAR                   tistb000
102700121008     C                   CLEAR                   tistbds
102800041014     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
102900041014     C                   EVAL      stbSTS = '1'
103000041014     C                   EXSR      WRIREC
103100041014     C                   ENDIF
103200041014     C****
103300041014     C*
103400041014     C****
103500041014     C* stato INCARICO2 (bolla presa in carico; con LNP<>LNA)
103600041014     C****
103700041014     C* - condizioni:
103800041014     C*   LNP <> LNA
103900041014     C*
104000041014     C* Verifico le condizione che soddisfano lo stato INCARICO2
104100041014     C                   IF        vacLNP <> vacLNA
104200041014     C                   EVAL      wTIS = '9'
104300041014     C                   EVAL      wDESTATO = 'BOLLA PRESA IN CARICO ' +
104400041014     C                                        '(LNP<>LNA)'
104500041207     C                   EVAL      wDAS = vacAAS*10000+vacMGS
104600131030     C                   EVAL      wORS = 210000
104700110926     C                   EVAL      wPRS = '4'
104800041014     C                   EVAL      wCOS = 'INCARICO2'
104900041014     C*
105000041014     C* - inizializzo il buffer del file stati bolle (TISTB)
105100041014     C                   CLEAR                   tistb000
105200121008     C                   CLEAR                   tistbds
105300041014     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
105400041014     C                   EVAL      stbSTS = '1'
105500041014     C                   EXSR      WRIREC
105600041014     C                   ENDIF
105700040205     C****
105800040205     C* stato CONSOK (bolla chiusa con consegna OK; generica STANDARD)
105900040205     C****
106000040205     C* - condizioni:
106100040205     C*   data consegna reale (vacDCM) > 0 AND
106200040205     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
106300040205     C*
106400040205     C* Verifico le condizione che soddisfano lo stato CONSOK
106500041207     C                   IF        vacDCM  > *zeros                   AND
106600120713     C                             (vacCCA = *blanks OR vacCCA = '1')
106700120713     C***                          AND wNDC   <> *all'9'
106800040205     C                   EVAL      wTIS = '1'
106900040205     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
107000041207     C                   EVAL      wDAS = vacDCM
107100121010     C                   EVAL      wORS = vacHMC*100
107200110926     C                   EVAL      wPRS = '4'
107300040205     C                   EVAL      wCOS = 'CONSOK'
107400040205     C*
107500040205     C* - inizializzo il buffer del file stati bolle (TISTB)
107600040205     C                   CLEAR                   tistb000
107700121008     C                   CLEAR                   tistbds
107800040205     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
107900040205     C                   EVAL      stbSTS = '9'
108000040205     C                   EXSR      WRIREC
108100040205     C                   ENDIF
108200040205     C*
108300031031     C****
108400031103     C* stato CONSOK1 (bolla chiusa con consegna OK; con LNP=LNA)
108500031031     C****
108600031031     C* - condizioni:
108700031031     C*   data consegna reale (vacDCM) > 0 AND
108800031031     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
108900031031     C*   LNP = LNA
109000031031     C*
109100031031     C* Verifico le condizione che soddisfano lo stato CONSOK1
109200041207     C                   IF        vacDCM  > *zeros                   AND
109300031031     C                             (vacCCA = *blanks OR vacCCA = '1') AND
109400120713     C                             vacLNP = vacLNA
109500120713     C***                          AND wNDC   <> *all'9'
109600031103     C                   EVAL      wTIS = '9'
109700031103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK ' +
109800031103     C                                        '(LNP=LNA)'
109900041207     C                   EVAL      wDAS = vacDCM
110000121010     C                   EVAL      wORS = vacHMC*100
110100110926     C                   EVAL      wPRS = '4'
110200031103     C                   EVAL      wCOS = 'CONSOK1'
110300031031     C*
110400031031     C* - inizializzo il buffer del file stati bolle (TISTB)
110500031031     C                   CLEAR                   tistb000
110600121008     C                   CLEAR                   tistbds
110700031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
110800031031     C                   EVAL      stbSTS = '9'
110900031103     C                   EXSR      WRIREC
111000031031     C                   ENDIF
111100031031     C****
111200031031     C*
111300031031     C****
111400031103     C* stato CONSOK2 (bolla chiusa con consegna OK; con LNP<>LNA)
111500031031     C****
111600031031     C* - condizioni:
111700031031     C*   data consegna reale (vacDCM) > 0 AND
111800031031     C*   codice consegna anomala (vacCCA) = ' ' OR = '1'
111900031031     C*   LNP <> LNA
112000031031     C*
112100120620     C* Verifico le condizione che soddisfano lo stato CONSOK2
112200041207     C                   IF        vacDCM  > *zeros                   AND
112300031031     C                             (vacCCA = *blanks OR vacCCA = '1') AND
112400120713     C                             vacLNP <> vacLNA
112500120713     C***                          AND wNDC   <> *all'9'
112600031103     C                   EVAL      wTIS = '9'
112700031103     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK ' +
112800031103     C                                        '(LNP<>LNA)'
112900041207     C                   EVAL      wDAS = vacDCM
113000121010     C                   EVAL      wORS = vacHMC*100
113100110926     C                   EVAL      wPRS = '4'
113200031103     C                   EVAL      wCOS = 'CONSOK2'
113300031031     C*
113400031031     C* - inizializzo il buffer del file stati bolle (TISTB)
113500031031     C                   CLEAR                   tistb000
113600121008     C                   CLEAR                   tistbds
113700031103     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
113800031031     C                   EVAL      stbSTS = '9'
113900031103     C                   EXSR      WRIREC
114000031031     C                   ENDIF
114100031031     C****
114200140624     C*
114300040205     C****
114400040205     C* stato RESO (bolla chiusa con reso; generica STANDARD)
114500040205     C****
114600040205     C* - condizioni:
114700040205     C*   data consegna reale (vacDCM) > 0 AND
114800040205     C*   codice consegna anomala (vacCCA) = '2'
114900040205     C*
115000040205     C* Verifico le condizione che soddisfano lo stato RESO
115100041207     C                   IF        vacDCM > *zeros                    AND
115200040205     C                             vacCCA = '2'
115300040205     C                   EVAL      wTIS = '1'
115400040205     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON RESO'
115500041207     C                   EVAL      wDAS = vacDCM
115600121010     C  N50              EVAL      wORS = vacHMC*100
115700110926     C                   EVAL      wPRS = '4'
115800040205     C                   EVAL      wCOS = 'RESO'
115900040205     C*
116000040205     C* - inizializzo il buffer del file stati bolle (TISTB)
116100040205     C                   CLEAR                   tistb000
116200121008     C                   CLEAR                   tistbds
116300040205     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
116400040205     C                   EVAL      stbSTS = '9'
116500040205     C                   EXSR      WRIREC
116600040205     C                   ENDIF
116700100201     C*
116800100201     C****
116900100201     C* stato AVARIA (bolla chiusa con avaria; generica STANDARD)
117000100201     C****
117100100201     C* - condizioni:
117200100201     C*   data consegna reale (vacDCM) > 0 AND
117300100204     C*   codice consegna anomala (vacCCA) = '6'
117400100201     C*
117500111025     C* Verifico le condizione che soddisfano lo stato AVARIA
117600100201     C                   IF        vacDCM > *zeros                    AND
117700100201     C                             vacCCA = '6'
117800100201     C                   EVAL      wTIS = '1'
117900100201     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON AVARIA'
118000100201     C                   EVAL      wDAS = vacDCM
118100121010     C                   EVAL      wORS = vacHMC*100
118200110926     C                   EVAL      wPRS = '4'
118300100201     C                   EVAL      wCOS = 'AVARIA'
118400100201     C*
118500100201     C* - inizializzo il buffer del file stati bolle (TISTB)
118600100201     C                   CLEAR                   tistb000
118700121008     C                   CLEAR                   tistbds
118800100201     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
118900100201     C                   EVAL      stbSTS = '9'
119000100201     C                   EXSR      WRIREC
119100100201     C                   ENDIF
119200100204     C*
119300100204     C****
119400100204     C* stato ANOMALIA (bolla chiusa con anomalia; generica STANDARD)
119500100204     C****
119600100204     C* - condizioni:
119700100204     C*   data consegna reale (vacDCM) > 0 AND
119800100204     C*   codice consegna anomala (vacCCA) = '5'
119900100204     C*
120000111025     C* Verifico le condizione che soddisfano lo stato ANOMALIA
120100100204     C                   IF        vacDCM > *zeros                    AND
120200100204     C                             vacCCA = '5'
120300100204     C                   EVAL      wTIS = '1'
120400100204     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON ANOMALIA'
120500100204     C                   EVAL      wDAS = vacDCM
120600121010     C                   EVAL      wORS = vacHMC*100
120700110926     C                   EVAL      wPRS = '4'
120800100204     C                   EVAL      wCOS = 'ANOMALIA'
120900100204     C*
121000100204     C* - inizializzo il buffer del file stati bolle (TISTB)
121100100204     C                   CLEAR                   tistb000
121200121008     C                   CLEAR                   tistbds
121300100204     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
121400100204     C                   EVAL      stbSTS = '9'
121500100204     C                   EXSR      WRIREC
121600100204     C                   ENDIF
121700120712     C****
121800120712     C* stato MERCE MAI AFFIDATA (bolla chiusa con anomalia; generica STANDARD)
121900120712     C****
122000120712     C* - condizioni:
122100120712     C*   data consegna reale (vacDCM) > 0 AND
122200120712     C*   codice consegna anomala (vacCCA) = '7'
122300120712     C*
122400120712     C* Verifico le condizione che soddisfano lo stato ANOMALIA
122500120712     C                   IF        vacDCM > *zeros                    AND
122600120712     C                             vacCCA = '7'
122700120712     C                   EVAL      wTIS = '1'
122800120712     C                   EVAL      wDESTATO = 'BOLLA CHIUSA MERCE MAI AFFIDATA'
122900120712     C                   EVAL      wDAS = vacDCM
123000121010     C                   EVAL      wORS = vacHMC*100
123100120712     C                   EVAL      wPRS = '4'
123200120712     C                   EVAL      wCOS = 'COLNOTAFF'
123300120712     C*
123400120712     C* - inizializzo il buffer del file stati bolle (TISTB)
123500120712     C                   CLEAR                   tistb000
123600121008     C                   CLEAR                   tistbds
123700120712     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
123800120712     C                   EVAL      stbSTS = '9'
123900120712     C                   EXSR      WRIREC
124000120712     C                   ENDIF
124100140624     C*
124200140624     C****
124300140624     C* stato MERCE DISTRUTTA (bolla chiusa come merce da distruggere; generica STANDARD)
124400140624     C****
124500140624     C* - condizioni:
124600140624     C*   data consegna reale (vacDCM) > 0 AND
124700140624     C*   codice consegna anomala (vacCCA) = 'S'
124800140624     C*
124900170802     C* Verifico le condizione che soddisfano lo stato MERCE DISTRUTTA
125000140624     C                   IF        vacDCM > *zeros                    AND
125100140624     C                             vacCCA = 'S'
125200140624     C                   EVAL      wTIS = '1'
125300140624     C                   EVAL      wDESTATO = 'BOLLA CHIUSA - MERCE DISTRUTTA'
125400140624     C                   EVAL      wDAS = vacDCM
125500140624     C  N50              EVAL      wORS = vacHMC*100
125600140624     C                   EVAL      wPRS = '4'
125700140624     C                   EVAL      wCOS = 'MDISTRUTTA'
125800140624     C*
125900140624     C* - inizializzo il buffer del file stati bolle (TISTB)
126000140624     C                   CLEAR                   tistb000
126100140624     C                   CLEAR                   tistbds
126200140624     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
126300140624     C                   EVAL      stbSTS = '9'
126400140624     C                   EXSR      WRIREC
126500140624     C                   ENDIF
126600161114     C****
126700161114     C* stato CONSRIC (bolla con data consegna richiesta)
126800161114     C****
126900161114     C* - condizioni:
127000161114     C*   data consegna richiesta (vacDCR) > tutte altre date:
127100161114     C*      data consegna merce  (vacDCM)
127200161114     C*      data lasciato avviso (vacDLA)
127300161114     C*      data giacenza        (vacDAG)
127400161114     C*
127500161114     C* Verifico le condizione che soddisfano lo stato CONSOK
127600161114     C                   IF        vacDCR  > vacDCM AND
127700161114     C                             vacDCR  > vacDLA AND
127800161114     C                             vacDCR  > vacDAG
127900161114     C                   EVAL      wTIS = '1'
128000161114     C                   EVAL      wDESTATO = 'DATA CONSEGNA RICHIESTA'
128100161114     C                   EVAL      wDAS = vacDCR
128200161114     C                   EVAL      wORS = vacHCR*100
128300161114     C                   EVAL      wPRS = '4'
128400161114     C                   EVAL      wCOS = 'CONSRIC'
128500161114     C*
128600161114     C* - inizializzo il buffer del file stati bolle (TISTB)
128700161114     C                   CLEAR                   tistb000
128800161114     C                   CLEAR                   tistbds
128900161114     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
129000161114     C                   EVAL      stbSTS = '2'
129100161114     C                   EXSR      WRIREC
129200161114     C                   ENDIF
129300031031     C*
129400031031     C                   ENDSR
129500100806
129600100806
129700100806
129800100806     C     EXEVAP        BEGSR
129900111031     C*
130000111031     C* Inizializzo DS flag operativi
130100111031     C                   CLEAR                   DS_STBFOP
130200100806     C*
130300100806     C* Imposto subito alcuni valori
130400100806     C                   EVAL      wDAS = vapDAE
130500121010     C                   EVAL      wORS = vapORE
130600100806     C*
130700100806     C                   EVAL      wTIS = '1'
130800100806     C                   EVAL      wPRS = '6'
130900100806     C                   EVAL      wCOS = 'O' + %editc(vapFAR:'X') + vapCAR
131000100806     C*
131100100806     C                   CLEAR                   tistb000
131200121008     C                   CLEAR                   tistbds
131300100806     C*
131400100806     C                   EVAL      STB_BOR    = 'P'                             * ORM
131500100806     C                   EVAL      STB_FILLER = vapIDC
131600100806     C*
131700100806     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
131800100806     C                   EVAL      stbSTS = '1'
131900100806     C                   EXSR      WRIREC
132000100806     C*
132100100806     C                   ENDSR
132200120424
132300120424
132400120424
132500120424     C     EXEAR5GEN     BEGSR
132600120424     C*
132700120424     C* Inizializzo la ds d salvataggio
132800120424     C                   CLEAR                   DAR5GEN
132900120424     C*
133000120424     C* Aggancio il tipo record 'GEN'
133100120427     C                   EVAL      ar5TRD = 'GEN'
133200120424     C     KEYar531P     CHAIN     fiar531c
133300120424     C                   IF        %found(fiar531c)
133400120424     C*
133500120424     C* Se record NN annullato lo considero
133600120424     C                   IF        ar5ATB = *blanks
133700120424     C*
133800120424     C* Inizializzo DS flag operativi
133900120424     C                   CLEAR                   DS_STBFOP
134000120424     C*
134100120424     C* Salvo il buffer del record
134200120424     C                   EVAL      DAR5GEN = ar5UNI
134300120424     C*
134400120424     C* Costruisco la chiave sul file TISTB04L
134500120427     C                   IF        §AR5ARRDT <> *blanks
134600120427     C                   EVAL      wDAS = %dec(§AR5ARRDT:8:0)
134700120427     C                   ENDIF
134800120427     C                   IF        §AR5ARRHM <> *blanks
134900121010     C                   EVAL      wORS = %dec(§AR5ARRHM:4:0)*100
135000120427     C                   ENDIF
135100120424     C                   EVAL      wTIS = '1'
135200120424     C                   EVAL      wPRS = '4'
135300120424     C                   EVAL      wCOS = 'ARRPRICOL'
135400120424     C*
135500120424     C* - inizializzo il buffer del file stati bolle (TISTB)
135600120424     C                   CLEAR                   tistb000
135700121008     C                   CLEAR                   tistbds
135800120424     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
135900120424     C                   EVAL      stbSTS = '2'
136000120424     C                   EXSR      WRIREC
136100120424     C*
136200120424     C                   ENDIF
136300120424     C                   ENDIF
136400120424     C*
136500120424     C                   ENDSR
136600130227
136700130227
136800130227
136900130227     C     EXEPCT        BEGSR
137000130227     C*
137100130228     C* Innanzitutto verifico se il cliente di scambio dati è abilitato agli stati PDA
137200130227     C                   IF        w§PPTPDA = 'S'
137300130227     C*
137400130227     C* Inizializzo la ds d salvataggio
137500130227     C                   CLEAR                   fipctds_w
137600160208     C                   CLEAR                   fipctcetds
137700160208     C*
137800160208     C* Inizializzo il buffer del file stati bolle (TISTB)
137900160208     C                   CLEAR                   tistb000
138000160208     C                   CLEAR                   tistbds
138100160211     C                   EVAL      wPCTCMC = *loval
138200130227     C*
138300130227     C* Reperisco l'ultimo stato CET dagli eventi PDA
138400130227     C                   EVAL      pctTRD = 'CET'
138500160211     C     KEYpct03P     SETLL     fipct03l
138600160211     C                   IF        %equal(fipct03l)
138700160211     C     KEYpct03P     READE     fipct03l
138800160211     C                   DOW       not %eof(fipct03l)
138900130227     C*
139000130227     C* Se stato NN annullato lo considero
139100130227     C                   IF        pctATB = *blanks
139200130227     C*
139300130227     C* Salvo il buffer corrente
139400130227     C                   EVAL      fipctds_w = fipctds
139500130227     C*
139600130227     C* Verifico ultimo esito da PDA
139700130227     C                   EVAL      fipctcetds = w_pctDATI
139800160211     C*
139900160211     C* Verifico rottura di codice per codice evento consegna
140000160211     C* e considero solamente l'ultimo (a parità di codice evento)
140100160211     C                   IF        §PCTCMC <> wPCTCMC
140200130227     C*
140300130227     C* Se spedizione consegnata => inserisco stato 'CONSOK'
140400160211     C                   IF        §PCTCMC = *blanks
140500130227     C                   EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
140600130227     C                   EVAL      wCOS = 'CONSOK'
140700130304     C                   EVAL      stbSTS = '9'
140800130227     C                   ELSE
140900160211     C                   EVAL      wCOS = §PCTCMC
141000130304     C                   EVAL      stbSTS = '2'
141100130227     C                   ENDIF
141200130227     C*
141300130227     C* Valorizzo il buffer del record si stato nel TISTB
141400130227     C                   EVAL      wTIS = '1'
141500130227     C                   EVAL      wDAS = §PCTDATA
141600130227     C                   EVAL      wORS = §PCTORA
141700130227     C                   EVAL      wPRS = '7'
141800160208     C*
141900160208     C* Valorizzo e scarico il buffer del file stati bolle (TISTB)
142000130227     C                   EXSR      WRIREC
142100160211     C*
142200160211     C* Salvo il nuovo codice di rottura
142300160211     C                   EVAL      wPCTCMC = §PCTCMC
142400160211     C*
142500160211     C                   ENDIF
142600160211     C                   ENDIF
142700160211     C*
142800160211     C     KEYpct03P     READE     fipct03l
142900160211     C                   ENDDO
143000130227     C*
143100130228     C* Se non trovo la spedizione negli status PDA => verifico in FNARB
143200160208     C***                ELSE
143300130228     C*
143400160208     C* Se no stato PDA => aggancio la bolla IN ARRIVO
143500160208     C***  KEYarb01C     CHAIN     fnarb01l
143600160208     C***                if        %found(fnarb01l)
143700130228     C*
143800130228     C* Chiamata metodo GetLastChild
143900160208     C***                movel     *blanks       wEsito2           1
144000160208     C***                eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
144100160208     C***                                             wAAS
144200160208     C***                                            :wLNP
144300160208     C***                                            :wNRS
144400160208     C***                                            :wNSP
144500160208     C***                                            :pOutAnnoFI
144600160208     C***                                            :pOutLineaParFI
144700160208     C***                                            :pOutSerieFI
144800160208     C***                                            :pOutNumSpedFI
144900160208     C***                                            :pOutDcmFI
145000160208     C***                                            :pOutCcaFI))
145100160208     C***                if        wEsito2 = '0'
145200130228     C*
145300130228     C* Considero solo se spedizione (ultima bolla figlia) consegnata
145400160208     C***                if        pOutDcmFI > *zeros   AND
145500160208     C***                          pOutCcaFI = *blanks
145600160208     C***                EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
145700160208     C***                EVAL      wCOS = 'CONSOK'
145800130228     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
145900160208     C***                EVAL      stbSTS = '9'
146000160208     C***                EXSR      WRIREC
146100160208     C***                endif
146200130228     C*
146300130228     C* Se non ha bolle figlie verifico i dati sulla bolla stessa
146400160208     C***                else
146500130228     C*
146600130228     C* Considero solo se spedizione consegnata
146700160208     C***                if        arbDCM > *zeros   AND
146800160208     C***                          arbCCA = *blanks
146900160208     C***                EVAL      wDESTATO = 'BOLLA CHIUSA CON CONSEGNA OK'
147000160208     C***                EVAL      wCOS = 'CONSOK'
147100130228     C* - valorizzo e scarico il buffer del file stati bolle (TISTB)
147200160208     C***                EVAL      stbSTS = '9'
147300160208     C***                EXSR      WRIREC
147400160208     C***                endif
147500130228     C*
147600160208     C***                endif
147700130228     C*
147800160208     C***                ENDIF
147900130227     C                   ENDIF
148000130227     C*
148100130227     C                   ENDIF
148200130227     C*
148300130227     C                   ENDSR
148400031031
148500160223
148600160223
148700160223     C     EXECOLLI      BEGSR
148800160223     C*
148900160223     C* Innanzitutto verifico se il cliente di scambio dati è abilitato alle info colli
149000160223     C                   IF        w§PPTCOLLI = 'S'
149100160223     C*
149200160223     C* Inizializzo il buffer del file stati bolle (TISTB)
149300160223     C                   CLEAR                   tistb000
149400160223     C                   CLEAR                   tistbds
149500160223     C*
149600160223     C* Reperisco le informazioni relative ai colli
149700160705     C                   EVAL      UBCOLOPZ     = 'FV5'
149800160223     C                   EVAL      UBCOLFLGOPE  = *blanks
149900160223     C                   EVAL      UBCOLTLA     = *blanks
150000160223     C                   EVAL      UBCOLAAS     = wAAS
150100160223     C                   EVAL      UBCOLLNP     = wLNP
150200160223     C                   EVAL      UBCOLNRS     = wNRS
150300160223     C                   EVAL      UBCOLNSP     = wNSP
150400160223     C                   CLEAR                   UBCOLERR
150500160223     C*
150600160223     C* Richiamo l'aopposito driver
150700160223     C                   CALL      'UBCOL00R'
150800160223     C                   PARM                    UBCOLOPZ
150900160223     C                   PARM                    UBCOLFLGOPE
151000160223     C                   PARM                    UBCOLTLA
151100160223     C                   PARM                    UBCOLAAS
151200160223     C                   PARM                    UBCOLLNP
151300160223     C                   PARM                    UBCOLNRS
151400160223     C                   PARM                    UBCOLNSP
151500160223     C                   PARM                    UBCOLNCL
151600160223     C                   PARM                    UBCOLTIP
151700160223     C                   PARM                    UBCOLDCM
151800160223     C                   PARM                    UBCOLHMC
151900160223     C                   PARM                    UBCOLSKBRT
152000160223     C                   PARM                    UBCOLSKCLI
152100160223     C                   PARM                    UBCOLSKDCM
152200160223     C                   PARM                    UBCOLSKHMC
152300160223     C                   PARM                    UBCOLSKDFS
152400160223     C                   PARM                    UBCOLSKHMS
152500160223     C                   PARM                    UBCOLSKVUC
152600160223     C                   PARM                    UBCOLSKPUC
152700160223     C                   PARM                    UBCOLERR
152800160223     C*
152900160223     C* Valorizzo l'output solo se l'esito è OK
153000160223     C                   IF        UBCOLERR = 0
153100160223     C*
153200160223     C* Ciclo per tutti i colli della spedizione
153300160224     C                   Z-ADD     1             wIdxCollo         5 0
153400160223     C                   DOW       wIdxCollo <= UBCOLNCL
153500160223     C*
153600160223     C* Routine per valorizzazione SPUNTA ENTRATA (5)
153700160223     C                   EXSR      EXESPUNTENT
153800160223     C*
153900160223     C                   ADD       1             wIdxCollo
154000160223     C                   ENDDO
154100160223     C*
154200160223     C                   ENDIF
154300160223     C                   ENDIF
154400160223     C*
154500160223     C                   ENDSR
154600160223
154700160223
154800160223
154900160223     C     EXESPUNTENT   BEGSR
155000160223     C*
155100160223     C* Valorizzo il buffer del record di stato nel TISTB
155200160223     C                   EVAL      wDESTATO = 'SPUNTA ENTRARA (5)'
155300160223     C                   EVAL      wCOS = 'SPUNTNPG5'
155400160627     C                   EVAL      wDAS = UBCOLSKDFS(1)
155500160627     C                   EVAL      wORS = UBCOLSKHMS(1)
155600160223     C                   EVAL      wTIS = '1'
155700160223     C                   EVAL      wPRS = '8'
155800160223     C                   CLEAR                   DSTBDNPG5
155900160223     C*
156000160223     C* Per il primo collo inserisco testata
156100160223     C                   IF        wIdxCollo  = 1
156200160223     C                   EXSR      WRIREC
156300160223     C                   ENDIF
156400160223     C*
156500160223     C* Per i colli inserisco anche le info di dettaglio TISTBD
156600160223     C                   CLEAR                   TISTBD00
156700160223     C                   EVAL      STBDKSU = STBKSU
156800160223     C                   EVAL      STBDAAS = STBAAS
156900160223     C                   EVAL      STBDLNP = STBLNP
157000160223     C                   EVAL      STBDNRS = STBNRS
157100160223     C                   EVAL      STBDNSP = STBNSP
157200160223     C                   EVAL      STBDDAS = STBDAS
157300160223     C                   EVAL      STBDORS = STBORS
157400160223     C                   EVAL      STBDCOS = STBCOS
157500160223     C                   EVAL      STBDTIS = 'BRT'
157600160223     C                   EVAL      STBDSEG = UBCOLSKBRT(wIdxCollo)
157700160223     C                   EVAL      STBDDAD = UBCOLSKDFS(wIdxCollo)
157800160224     C                   EVAL      STBDORD = UBCOLSKHMS(wIdxCollo)
157900160223     C                   MOVEL     datcor        stbDDAO
158000160223     C                   MOVE      oracor        stbDDAO
158100160223     C                   EVAL      §STBDNPG51 = UBCOLSKCLI(wIdxCollo)
158200160223     C                   EVAL      STBDDTA = DSTBDNPG5
158300160223     C*
158400160223     C                   WRITE(e)  TISTBD00
158500160223     C*
158600160223     C                   ENDSR
158700031031
158800031031
158900031031
159000031103     C     WRIREC        BEGSR
159100090430     C*
159200090430     C* Valorizzo il buffer del record del file stati bolla (TISTB)
159300090430     C                   EVAL      stbKSU = w§PPTKSU
159400090430     C                   EVAL      stbKSC = wKSC
159500090430     C                   EVAL      stbAAS = wAAS
159600090430     C                   EVAL      stbLNP = wLNP
159700090430     C                   EVAL      stbNRS = wNRS
159800090430     C                   EVAL      stbNSP = wNSP
159900090430     C                   EVAL      stbRMN = wRMN
160000090430     C                   EVAL      stbRMA = wRMA
160100090430     C                   EVAL      stbDSP = wDSP
160200090430     C                   EVAL      stbDAS = wDAS
160300121010     C                   EVAL      stbORS = %int(wORS/100)
160400090430     C                   EVAL      stbTIS = wTIS
160500090430     C                   EVAL      stbPRS = wPRS
160600090430     C                   EVAL      stbCOS = wCOS
160700090430     C* Valorizzo la data/ora d scrittura dello stato dell'evento
160800130314     C                   CLEAR                   stbDAOR
160900090430     C                   MOVEL     datcor        stbDAOR
161000090430     C                   MOVE      oracor        stbDAOR
161100100806     C*
161200100806     C                   IF        w§PPTTIP <> 'VP'
161300081112     C*
161400081112     C* Verifico se trattasi d bolla originale oppure figlia
161500081112     C                   movel     *blanks       wEsito1           1
161600090430     C                   movel     *blanks       wEsito2           1
161700081112     C*
161800081112     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
161900081112     C                                                STBAAS
162000081112     C                                               :STBLNP
162100081112     C                                               :STBNRS
162200081112     C                                               :STBNSP
162300090430     C                                               :pOutLblTyp
162400090430     C                                               :pOutAnnoBO
162500090430     C                                               :pOutLineaParBO
162600090430     C                                               :pOutSerieBO
162700090430     C                                               :pOutNumSpedBO
162800090430     C                                               :pOutDcmBO
162900090430     C                                               :pOutCcaBO
163000090430     C                                               :pOutRblBO))
163100081112     C*
163200111103     C                   IF        STB_BOR = *blanks
163300111103     C                   EVAL      STB_BOR = pOutLblTyp                         * bolla Originale
163400111103     C                   ENDIF
163500111103     C*
163600090430     C                   IF        pOutLblTyp = 'O'
163700090430     C*
163800111103     C* Se è uno stato d chiusura spedizione
163900111025     C                   if        stbSTS = '9'
164000090430     C* Chiamata metodo GetLastChild
164100090430     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
164200090430     C                                                STBAAS
164300090430     C                                               :STBLNP
164400090430     C                                               :STBNRS
164500090430     C                                               :STBNSP
164600090430     C                                               :pOutAnnoFI
164700090430     C                                               :pOutLineaParFI
164800090430     C                                               :pOutSerieFI
164900090430     C                                               :pOutNumSpedFI
165000090430     C                                               :pOutDcmFI
165100090430     C                                               :pOutCcaFI))
165200090430     C* Se ultima figlia consegnata allora overrido i dati consegna della bolla originale relativa
165300090430     C                   if        pOutDcmFI > *zeros
165400090430     C                   if        pOutCcaFI = '2'
165500090430     C                   eval      stbCOS = 'RESO'
165600111025     C                   eval      wCOS   = 'RESO'
165700111103     C                   if        wDAS   = pOutDcmFI
165800090430     C                   eval      stbDAS = pOutDcmFI
165900090430     C                   eval      wDAS   = pOutDcmFI
166000130228     C                   endif
166100090430     C                   endif
166200100201     C                   if        pOutCcaFI = '6'
166300100201     C                   eval      stbCOS = 'AVARIA'
166400111025     C                   eval      wCOS   = 'AVARIA'
166500111103     C                   IF        STB_BOR = *blanks
166600100201     C                   eval      stbDAS = pOutDcmFI
166700100201     C                   eval      wDAS   = pOutDcmFI
166800111103     C                   ENDIF
166900100201     C                   endif
167000090430     C                   endif
167100111031     C*
167200111025     C                   endif
167300111031     C*
167400081112     C                   ENDIF
167500100806     C*
167600100806     C                   ENDIF
167700090430     C*
167800081112     C                   EVAL      stbFOP = DS_STBFOP
167900121010     C*
168000121010     C* **** FORZATURE "FINALI" ****
168100121012     C*
168200121012     C* - MIC
168300121010     C                   IF         stbPRS = '2'    and
168400121010     C                             (stbCOS = 'MIC'  or
168500121010     C                              stbCOS = 'NIC') and
168600121012     C                              stbORS >= 1200
168700170517     C                   EVAL      wORS   = 1159*100
168800121010     C                   EVAL      stbORS = 1159
168900121010     C                   ENDIF
169000090430     C*
169100090430     C* Verifico se tale stato è già presente nel file TISTB
169200121010     C                   EVAL      kORS = %int(wORS/100)
169300121008     C     KEYstb04      CHAIN     tistb04l
169400090430     C*
169500090430     C* SOLO se stato NN ancora presente in archivio TISTB procedo con l'inserimento
169600121008     C                   IF        not %found(tistb04l)
169700081112     C*
169800031103     C* Scarico il buffer del file stati bolle (TISTB)
169900121008     C                   WRITE     tistb000
170000121008     C*
170100121008     C                   ELSE
170200121008     C                   IF        w§PPTUPD = 'U'
170300121008     C                   EVAL      tistbds_i = tistbds
170400121008     C                   UPDATE    tistb004
170500121008     C                   ENDIF
170600090430     C*
170700090430     C                   ENDIF
170800031103     C*
170900031103     C* Dopo la scrittura dello stato bolla corrente, se trattasi d stato bolla PERSONALIZZATO
171000031103     C* (campo STBTIS='9') verifico che il codice stato attribuito
171100031103     C* sia presente in tabella "STATI BOLLA RICAVATI" (Tab. "STB")
171200031103     C                   IF        stbTIS = '9'
171300031103     C                   EVAL      tbeCOD = 'STB'
171400031103     C                   EVAL      tbeKE1 = stbCOS
171500031103     C     KEYtbe01P     SETLL     tntbe01l
171600031103     C                   IF        %equal(tntbe01l)
171700031103     C                   ELSE
171800031103     C                   CLEAR                   DSSTB
171900031103     C                   EVAL      §STBDES = wDESTATO
172000031103     C                   EVAL      tbeUNI = DSSTB
172100031103     C                   WRITE     tntbe000
172200031103     C                   ENDIF
172300031103     C                   ENDIF
172400031031     C*
172500031031     C                   ENDSR
172600031031
172700031031
172800991027
172900991027      /TITLE Operazioni iniziali.
173000991027     C     *inzsr        BEGSR
173100991027     C*
173200991027     C     *ENTRY        PLIST
173300991027     C                   parm                    prmppt
173400991027     C     wrkesito      parm      wrkesito      prmesito
173500031103     C*
173600031103     C* Ridefinisco subito i parametri d traduzione
173700031103     C                   EVAL      DSPPT = prmppt
173800040308     C*
173900040308     C* Calcola la data corrente
174000040308     C                   time                    wn14             14 0
174100040308     C                   movel     wn14          oracor            6 0          *ORA  (6) IN H/M/S
174200100713     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
174300100713     C                   eval      datcor = %dec(%date() : *ISO)
174400030709     C*
174500030709     C* Definizione chiavi
174600030924     C*
174700031103     C* Chiave su TNTBE01L - Parziale
174800031103     C     KEYtbe01P     KLIST
174900031103     C                   KFLD                    tbeCOD
175000031103     C                   KFLD                    tbeKE1
175100031014     C* Chiave su FNEVB01L - Parziale
175200031014     C     KEYevb01P     KLIST
175300031106     C                   KFLD                    wAAS
175400031106     C                   KFLD                    wLNP
175500031106     C                   KFLD                    wNRS
175600031106     C                   KFLD                    wNSP
175700110929     C* Chiave su FNEVB04L - Parziale
175800110929     C     KEYevb04P     KLIST
175900111031     C                   KFLD                    evb4_EVBAAS
176000111031     C                   KFLD                    evb4_EVBLNP
176100111031     C                   KFLD                    evb4_EVBNRS
176200111031     C                   KFLD                    evb4_EVBNSP
176300111031     C                   KFLD                    evb4_EVBCEV
176400111031     C                   KFLD                    evb4_EVBDEV
176500111031     C* Chiave su TIGCP51L - Completa
176600050310     C     KEYgcp51      KLIST
176700031106     C                   KFLD                    wAAS
176800031106     C                   KFLD                    wLNP
176900031106     C                   KFLD                    wNRS
177000031106     C                   KFLD                    wNSP
177100031031     C                   KFLD                    wFRG
177200041015     C* Chiave su FNDCT02L - Completa
177300041015     C     KEYdct02      KLIST
177400041015     C                   KFLD                    wAAS
177500041015     C                   KFLD                    wLNP
177600041015     C                   KFLD                    wNRS
177700041015     C                   KFLD                    wNSP
177800120424     C* Chiave su FIAR531C - Parziale
177900120424     C     KEYar531P     KLIST
178000120424     C                   KFLD                    wAAS
178100120424     C                   KFLD                    wLNP
178200120424     C                   KFLD                    wNRS
178300120424     C                   KFLD                    wNSP
178400120424     C                   KFLD                    ar5TRD
178500111031     C* Chiave su TITAS30C - Parziale
178600111031     C     KEYtas30P     KLIST
178700111031     C                   KFLD                    wAAS
178800111031     C                   KFLD                    wLNP
178900111031     C                   KFLD                    wNRS
179000111031     C                   KFLD                    wNSP
179100050224     C* Chiave su TISTB04L - Completa
179200050224     C     KEYstb04      KLIST
179300050224     C                   KFLD                    w§PPTKSU
179400031106     C                   KFLD                    wAAS
179500031106     C                   KFLD                    wLNP
179600031106     C                   KFLD                    wNRS
179700031106     C                   KFLD                    wNSP
179800031103     C                   KFLD                    wDAS
179900121010     C                   KFLD                    kORS
180000031103     C                   KFLD                    wCOS
180100130305     C                   KFLD                    wPRS
180200100806     C* Chiave su FNORM01L - Completa
180300100806     C     KEYorm01      KLIST
180400100806     C                   KFLD                    vapPOE
180500100806     C                   KFLD                    vapNSR
180600100806     C                   KFLD                    vapNOR
180700100806     C                   KFLD                    vapNRV
180800111103     C* Chiave su FNARB01L - Completa
180900111103     C     KEYarb01C     KLIST
181000111103     C                   KFLD                    wAAS
181100111103     C                   KFLD                    wLNP
181200111103     C                   KFLD                    wNRS
181300111103     C                   KFLD                    wNSP
181400160211     C* Chiave su FIPCT03L - Parziale
181500160211     C     KEYpct03P     KLIST
181600130227     C                   KFLD                    wAAS
181700130227     C                   KFLD                    wLNP
181800130227     C                   KFLD                    wNRS
181900130227     C                   KFLD                    wNSP
182000130227     C                   KFLD                    pctTRD
182100111103     C*
182200111103     C                   ENDSR
182300130227     C***
