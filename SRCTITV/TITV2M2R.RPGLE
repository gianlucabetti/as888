000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400090325     Ftivgdtmp  uF   E             DISK
000500100209     FTIVAWWWT  UF A E             DISK    commit
000600120803     FFNARB01L  IF   E           K DISK    usropn
000700120803     F                                     extfile(LibFileARB01)
000800120803     FFNART01L  IF   E           K DISK    usropn
000900120803     F                                     extfile(LibFileART01)
001000131128     FFIAR901L  IF   E           K DISK    usropn
001100131128     F                                     extfile(LibFileAR901)
001200120803     FFNBRV07L  IF   E           K DISK    usropn
001300120803     F                                     extfile(LibFileBRV07)
001400120807     FTIGCP51L  IF   E           K DISK
001500120803
001600120803
001700120803     D LibFileARB01    s             21A   inz
001800120803     D LibFileART01    s             21A   inz
001900131128     D LibFileAR901    s             21A   inz
002000120803     D LibFileBRV07    s             21A   inz
002100120803     D currSysNeta     s              8A   inz('*NULL')
002200991027
002300991027     D prmppt          s             50
002400991027     D prmesito        s              1
002500991027     D wrkesito        s                   like(prmesito)
002600110913     D fnvacds       e ds                  extname(fnvac00t)
002700120803     D sepFld          s              1    inz(';')
002800111216
002900120921     D skSGN           s             15    inz dim(500)
003000120921     D jSGN            s              3  0 inz
003100120921
003200120803     D wCCM            s                   like(vacCCM) inz
003300120803     D wAAS            s                   like(vacAAS) inz
003400120803     D wLNP            s                   like(vacLNP) inz
003500120803     D wNRS            s                   like(vacNRS) inz
003600120803     D wNSP            s                   like(vacNSP) inz
003700120803     D wRMO            s                   like(vacRMO) inz
003800120803     D wRMN            s                   like(vacRMN) inz
003900120803     D wRMA            s                   like(vacRMA) inz
004000120803     D wRSD            s                   like(vacRSD) inz
004100120803     D wPRD            s                   like(vacPRD) inz
004200120925     D wNZD            s                   like(arbNZD) inz
004300120803     D wLNA            s                   like(vacLNA) inz
004400120803     D wNCL            s                   like(vacNCL) inz
004500131128     D wTIC            s                   like(ar9TIC) inz
004600131128     D wNDC            s                   like(arbNDC) inz
004700131128     D wDDC            s                   like(arbDDC) inz
004800120803     D wDSP            s             10                 inz
004900120803     D wDCR            s             10                 inz
005000120803     D wDCP            s             10                 inz
005100120809     D wINGIAC         s              1                 inz
005200120809     D wDIROTT         s              1                 inz
005300120809     D wCAOPEN         s              1                 inz
005400120809
005500120809
005600120809     D pInTpChiamDCT   S              2S 0
005700120809     D pOutFNDCT_DS  E DS                  extname(FNDCT02L)
005800120809     D pOutEsitoDCT    S              1A
005900110913
006000110913
006100110913     D*------------------
006200110913     D* LINKING A DEFINIZIONI ESTERNE
006300110913     D*------------------
006400120803     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006500120803     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006600111222     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
006700111222     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
006800120803     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
006900120803     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
007000110913
007100110810
007200991027
007300120803     C*
007400120803     C* Reperisco il sistema AS/400 corrente
007500120803     C                   callp     UBRTVNETA_Rtv(currSysNeta)
007600120803     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
007700120803     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
007800120803     C                   eval      LibFileART01 = 'FILTRA201/FNART01L'
007900131128     C                   eval      LibFileAR901 = 'FILTRA201/FIAR901L'
008000120803     C                   eval      LibFileBRV07 = 'FILTRA201/FNBRV07L'
008100120803     C                   else
008200120803     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
008300120803     C                   eval      LibFileART01 = 'FILTRAPRD/FNART01L'
008400131128     C                   eval      LibFileAR901 = 'FILTRAPRD/FIAR901L'
008500120803     C                   eval      LibFileBRV07 = 'FILTRAPRD/FNBRV07L'
008600120803     C                   endif
008700120803     C*
008800120803     C* Apertura file "overraidati"
008900120803     C                   if        not %open(FNARB01L)
009000120803     C                   open      FNARB01L
009100120803     C                   endif
009200120803     C                   if        not %open(FNART01L)
009300120803     C                   open      FNART01L
009400120803     C                   endif
009500131128     C                   if        not %open(FIAR901L)
009600131128     C                   open      FIAR901L
009700131128     C                   endif
009800120803     C                   if        not %open(FNBRV07L)
009900120803     C                   open      FNBRV07L
010000120803     C                   endif
010100120803     C*
010200120803     C* Esecuzione operazioni di elaborazione
010300030908     C                   EXSR      chkRECFILMBR
010400991027     C                   EXSR      traduci
010500120803     C*
010600120803     C* Chiusura file "overraidati"
010700120803     C                   if        %open(FNARB01L)
010800120803     C                   close     FNARB01L
010900120803     C                   endif
011000120803     C                   if        %open(FNART01L)
011100120803     C                   close     FNART01L
011200120803     C                   endif
011300131128     C                   if        %open(FIAR901L)
011400131128     C                   close     FIAR901L
011500131128     C                   endif
011600120803     C                   if        %open(FNBRV07L)
011700120803     C                   close     FNBRV07L
011800120803     C                   endif
011900120803     C*
012000921023     C                   SETON                                        LR
012100030908
012200110810
012300110810
012400030908     C     chkRECFILMBR  BEGSR
012500030908     C*
012600100209     C                   READ      TIVAWWWT                               55
012700120803     C*
012800120803     C* Se vuoto inserisco riga di intestazione
012900120803     C                   IF        *IN55
013000120803     C                   EVAL      VAWDTA  =
013100120810     C                              'CLI. MITTENTE'         +sepFld+
013200120810     C                              'ANNO SPED.'            +sepFld+
013300120810     C                              'LINEA PARTENZA'        +sepFld+
013400120810     C                              'SERIE SPED.'           +sepFld+
013500120810     C                              'NUM. SPED.'            +sepFld+
013600120810     C                              'MITT. ORIGINALE'       +sepFld+
013700120810     C                              'RIF. MITT. NUM.'       +sepFld+
013800120810     C                              'RIF. MITT. ALFA'       +sepFld+
013900120810     C                              'RAG. SOC. DEST.'       +sepFld+
014000120810     C                              'PROV. DEST'            +sepFld+
014100120925     C                              'NAZIONE DEST.'         +sepFld+
014200120810     C                              'LINEA ARRIVO'          +sepFld+
014300120810     C                              'COLLI SPED.'           +sepFld+
014400120810     C                              'DATA SPED.'            +sepFld+
014500120810     C                              'DATA CONS. RICH.'      +sepFld+
014600120810     C                              'DATA CONS. PARZ.'      +sepFld+
014700120810     C                              'IN GIAC.'              +sepFld+
014800120810     C                              'AVUTO DIR.'            +sepFld+
014900120921     C                              'C.A. APERTA'           +sepFld+
015000131128     C                              'SEGNACOLLO BRT'        +sepFld+
015100131128     C                              'TIPO INCASSO C/ASS.'   +sepFld+
015200131128     C                              'NUM. DISTINTA CONS.'   +sepFld+
015300131128     C                              'DATA DISTINTA CONS.'   +sepFld
015400120803     C                   WRITE     TIVAW000
015500120803     C                   ENDIF
015600030908     C*
015700030908     C                   ENDSR
015800110810
015900110810
016000991027
016100991027     C     traduci       BEGSR
016200991027     C*
016300090325     C                   READ      tivgdtmp
016400090325     C                   DOW       not %eof
016500120803     C                   movel     vgddta        fnvacds
016600120803     C*
016700120803     C* Inizializzazione variabili x buffer di output
016800120803     C                   clear                   wCCM
016900120803     C                   clear                   wAAS
017000120803     C                   clear                   wLNP
017100120803     C                   clear                   wNRS
017200120803     C                   clear                   wNSP
017300120803     C                   clear                   wRMO
017400120803     C                   clear                   wRMN
017500120803     C                   clear                   wRMA
017600120803     C                   clear                   wRSD
017700120803     C                   clear                   wPRD
017800120925     C                   clear                   wNZD
017900120803     C                   clear                   wLNA
018000120803     C                   clear                   wNCL
018100120803     C                   clear                   wDSP
018200120803     C                   clear                   wDCR
018300120803     C                   clear                   wDCP
018400131128     C                   clear                   wTIC
018500131128     C                   clear                   wNDC
018600131128     C                   clear                   wDDC
018700120809     C                   clear                   wINGIAC
018800120809     C                   clear                   wDIROTT
018900120809     C                   clear                   wCAOPEN
019000110913     C*
019100110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
019200110913     C                   MOVEL     *blanks       wEsito1           1
019300110913     C                   MOVEL     *blanks       wEsito2           1
019400110913     C*
019500110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019600110913     C                                                vacAAS
019700110913     C                                               :vacLNP
019800110913     C                                               :vacNRS
019900110913     C                                               :vacNSP
020000110913     C                                               :pOutLblTyp
020100110913     C                                               :pOutAnnoBO
020200110913     C                                               :pOutLineaParBO
020300110913     C                                               :pOutSerieBO
020400110913     C                                               :pOutNumSpedBO
020500110913     C                                               :pOutDcmBO
020600110913     C                                               :pOutCcaBO
020700110913     C                                               :pOutRblBO))
020800111109     C*
020900111109     C* Ritorno solamente le bolle valide come spedizione
021000111223     C                   IF        pOutLblTyp = 'O' AND pOutRblBO = 'N'
021100111220     C*
021200111220     C* Reperisco i dati della spedizione
021300120803     C                   SETON                                        51
021400111220     C                   EXSR      rtvDatiBolla
021500120803     C*
021600120803     C                   IF        not *in51
021700120803     C*
021800120809     C* Valorizzo il buffer d output
021900120803     C*
022000120803     C                   eval      wCCM = vacCCM
022100120803     C                   eval      wAAS = arbAAS
022200120803     C                   eval      wLNP = arbLNP
022300120803     C                   eval      wNRS = arbNRS
022400120803     C                   eval      wNSP = arbNSP
022500120803     C                   eval      wRMO = arbRMO
022600120803     C                   eval      wRMN = arbRMN
022700120803     C                   eval      wRMA = arbRMA
022800120803     C                   eval      wRSD = arbRSD
022900120803     C                   eval      wPRD = arbPRD
023000120925     C                   eval      wNZD = arbNZD
023100120803     C                   eval      wLNA = arbLNA
023200120803     C                   eval      wNCL = arbNCL
023300131128     C                   eval      wNDC = arbNDC
023400131128     C                   eval      wDDC = arbDDC
023500120809     C*
023600120809     C* Verifico se occorso dirottamento
023700120809     C                   if        vacCCA = '1'
023800120809     C                   eval      wDIROTT = 'S'
023900120809     C                   endif
024000120803     C*
024100120803     C* Formattazione date
024200120803     C*
024300120803     C                   eval      wDSP =
024400120803     C                              UBFMTDATE_Convert(
024500120803     C                                  %editc(arbAAS*10000+arbMGS:'4') :
024600120803     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
024700120803     C*
024800120803     C                   eval      wDCR =
024900120803     C                              UBFMTDATE_Convert(
025000120803     C                                  %editc(arbDCR:'4') :
025100120803     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
025200120803     C*
025300120803     C                   eval      wDCP =
025400120803     C                              UBFMTDATE_Convert(
025500120803     C                                  %editc(arbDCP:'4') :
025600120803     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
025700120921     C*
025800120921     C* Ciclo x ogni segnacollo mancante o non in consegna
025900120921     C                   z-add     1             jSGN
026000120921     C                   dow       jSGN <= %elem(skSGN)
026100120921     C*
026200120921     C                   if        jSGN = 1
026300120921     C                   eval      VAWDTA  =
026400120921     C                                     %editc(wCCM:'X')             +sepFld+
026500120921     C                                     %editc(wAAS:'X')             +sepFld+
026600120921     C                                     %editc(wLNP:'X')             +sepFld+
026700120921     C                                     %editc(wNRS:'X')             +sepFld+
026800120921     C                                     %editc(wNSP:'X')             +sepFld+
026900120921     C                                      %trim(wRMO)                 +sepFld+
027000120921     C                                     %editc(wRMN:'X')             +sepFld+
027100120921     C                                      %trim(wRMA)                 +sepFld+
027200120921     C                                      %trim(wRSD)                 +sepFld+
027300120921     C                                      %trim(wPRD)                 +sepFld+
027400120925     C                                      %trim(wNZD)                 +sepFld+
027500120921     C                                     %editc(wLNA:'X')             +sepFld+
027600120921     C                                     %editc(wNCL:'X')             +sepFld+
027700120921     C                                      %trim(wDSP)                 +sepFld+
027800120921     C                                      %trim(wDCR)                 +sepFld+
027900120921     C                                      %trim(wDCP)                 +sepFld+
028000120921     C                                      %trim(wINGIAC)              +sepFld+
028100120921     C                                      %trim(wDIROTT)              +sepFld+
028200120921     C                                      %trim(wCAOPEN)              +sepFld+
028300131128     C                                      ''''+%trim(skSGN(jSGN))     +sepFld+
028400131128     C                                      %trim(wTIC)                 +sepFld+
028500131128     C                                     %editc(wNDC:'X')             +sepFld+
028600131128     C                                     %editc(wDDC:'X')             +sepFld
028700120921     C                   write     TIVAW000
028800120921     C                   else
028900120921     C                   if        skSGN(jSGN) = *blanks
029000120921     C                   leave
029100120921     C                   else
029200120921     C                   eval      VAWDTA  =
029300120921     C                                     ' ' +sepFld+ ' ' +sepFld+
029400120921     C                                     ' ' +sepFld+ ' ' +sepFld+
029500120921     C                                     ' ' +sepFld+ ' ' +sepFld+
029600120921     C                                     ' ' +sepFld+ ' ' +sepFld+
029700120921     C                                     ' ' +sepFld+ ' ' +sepFld+
029800120921     C                                     ' ' +sepFld+ ' ' +sepFld+
029900120921     C                                     ' ' +sepFld+ ' ' +sepFld+
030000120921     C                                     ' ' +sepFld+ ' ' +sepFld+
030100120921     C                                     ' ' +sepFld+ ' ' +sepFld+
030200120921     C                                     ''''+%trim(skSGN(jSGN))      +sepFld
030300120921     C                   write     TIVAW000
030400120921     C                   endif
030500120921     C                   endif
030600120921     C                   add       1             jSGN
030700120921     C                   enddo
030800111109     C*
030900111109     C                   ENDIF
031000120803     C                   ENDIF
031100110810     C*
031200090325     C                   delete    tivgd000
031300991027     C*
031400090325     C                   READ      tivgdtmp
031500030325     C                   ENDDO
031600991027     C*
031700030325     C                   EVAL      wrkesito = '0'
031800991027     C*
031900910830     C                   ENDSR
032000111216
032100111216
032200111216
032300111216     C     rtvDatiBolla  BEGSR
032400120921     C*
032500120921     C                   clear                   skSGN
032600120921     C                   z-add     *zeros        jSGN
032700120803     C*
032800120803     C* Aggancio la bolla in filiale arrivo
032900120803     C     KEYarb01_C    chain     fnarb01l
033000120803     C                   if        %found(fnarb01l)
033100131128     C*
033200131128     C* Aggancio la bolla in filiale arrivo
033300131128     C     KEYarb01_C    chain     fiar901l
033400131128     C                   if        %found(fiar901l)
033500131128     C                   eval      wTIC = ar9TIC
033600131128     C                   else
033700131128     C                   eval      wTIC = '--'
033800131128     C                   endif
033900120807     C*
034000120809     C* Aggancio l'ultima giacenza
034100120807     C                   eval      gcpFRG = *zeros
034200120807     C     KEYgcp51      chain     tigcp51l
034300120807     C                   if        %found(tigcp51l)
034400120807     C*
034500120809     C* Verifico se bolla attualmente in giacenza
034600120807     C                   if        gcpATB = *blanks AND gcpFAS < 40
034700120809     C                   eval      wINGIAC = 'S'
034800120809     C                   endif
034900120809     C                   endif
035000120809     C*
035100120809     C* Reperisco l'ultimo danno ("valido ai fini internet")
035200120809     C                   call      'UBRTVDCT'
035300120809     C                   parm      vacAAS        pInAnno
035400120809     C                   parm      vacLNP        pInLineaPar
035500120809     C                   parm      vacNRS        pInSerie
035600120809     C                   parm      vacNSP        pInNumSped
035700120809     C                   parm      01            pInTpChiamDCT
035800120809     C                   parm                    pOutFNDCT_DS
035900120809     C                   parm                    pOutEsitoDCT
036000120809     C*
036100120809     C* Ritorno se attualmente c'è una C.A. aperta
036200120809     C                   if        pOutEsitoDCT = '1'
036300120809     C                   if        dctDCH = *zeros
036400120809     C                   eval      wCAOPEN = 'S'
036500120809     C                   endif
036600120809     C                   endif
036700120803     C*
036800120803     C* Inizializzo il contatore di spunte 4
036900120803     C                   z-add     *zeros        wContaSpunte_4    5 0
037000120803     C*
037100120803     C* Verifico le spunte
037200120803     C*
037300120803     C* - 1° scorro FNART01L (segnacolli) della bolla corrente
037400120803     C     KEYart01_P    setll     fnart01l
037500120921     C                   if        %equal(fnart01l)
037600120803     C     KEYart01_P    reade     fnart01l
037700120803     C                   dow       not %eof(fnart01l)
037800120921     C*
037900120921     C* Inizializzo a false la presenza fisica del collo
038000120921     C                   movel     'N'           wSegnacOK         1
038100120803     C*
038200120803     C* - 2° scorro FNBRV07L (spunte) del segnacollo bolla corrente
038300120803     C     KEYbrv07_P    setll     fnbrv07l
038400120921     C                   if        %equal(fnbrv07l)
038500120803     C     KEYbrv07_P    reade     fnbrv07l
038600120803     C                   dow       not %eof(fnbrv07l)
038700120803     C*
038800120803     C* Verifico unicamente l'esistenza delle spunte 4
038900120803     C                   if        brvNPG = 4
039000120803     C                   add       1             wContaSpunte_4
039100120921     C                   movel     'S'           wSegnacOK
039200120803     C                   endif
039300120803     C     KEYbrv07_P    reade     fnbrv07l
039400120803     C                   enddo
039500120921     C                   endif
039600120921     C*
039700120921     C* Se segnacollo non OK => collezione x essere restituito
039800120921     C                   if        wSegnacOK = 'N'
039900120921     C                   add       1             jSGN
040000120921     C                   eval      skSGN(jSGN) = %editc(artFLS:'X')+
040100120921     C                                           %editc(artLNA:'X')+
040200120921     C                                           %editc(artNRS:'X')+
040300120921     C                                           %editc(artNSC:'X')
040400120921     C                   endif
040500120803     C*
040600120803     C     KEYart01_P    reade     fnart01l
040700120803     C                   enddo
040800120803     C                   endif
040900120803     C*
041000120803     C* Considerazioni su quante spunte 4 sono state trovate
041100120803     C                   if        wContaSpunte_4 <> arbNCL
041200120803     C                   setoff                                       51
041300120803     C                   endif
041400120803     C*
041500120803     C                   endif
041600111221     C*
041700111216     C                   ENDSR
041800991027
041900110810
042000110810
042100991027      /TITLE Operazioni iniziali.
042200991027     C     *inzsr        BEGSR
042300991027     C*
042400991027     C     *ENTRY        PLIST
042500991027     C                   parm                    prmppt
042600991027     C     wrkesito      parm      wrkesito      prmesito
042700120803     C*
042800120803     C* Definizione chiavi
042900120803     C*
043000120803     C* Chiave su FNARB01L - Completa
043100120803     C     KEYarb01_C    KLIST
043200120803     C                   KFLD                    vacAAS
043300120803     C                   KFLD                    vacLNP
043400120803     C                   KFLD                    vacNRS
043500120803     C                   KFLD                    vacNSP
043600120807     C*
043700120807     C* Chiave su TIGCP51L - Completa
043800120807     C     KEYgcp51      KLIST
043900120807     C                   KFLD                    arbAAS
044000120807     C                   KFLD                    arbLNP
044100120807     C                   KFLD                    arbNRS
044200120807     C                   KFLD                    arbNSP
044300120807     C                   KFLD                    gcpFRG
044400120803     C*
044500120803     C* Chiave su FNART01L - Parziale
044600120803     C     KEYart01_P    KLIST
044700120803     C                   KFLD                    arbAAS
044800120803     C                   KFLD                    arbLNP
044900120803     C                   KFLD                    arbNRS
045000120803     C                   KFLD                    arbNSP
045100120803     C*
045200120803     C* Chiave su FNBRV07L - Parziale
045300120803     C     KEYbrv07_P    KLIST
045400120803     C                   KFLD                    artFLS
045500120803     C                   KFLD                    artLNA
045600120803     C                   KFLD                    artNRS
045700120803     C                   KFLD                    artNSC
045800030709     C*
045900991027     C                   ENDSR
