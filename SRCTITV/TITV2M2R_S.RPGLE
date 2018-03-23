000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400090325     Ftivgdtmp  uF   E             DISK
000500100209     FTIVAWWWT  UF A E             DISK    commit
000600120803     FFNARB01L  IF   E           K DISK    usropn
000700120803     F                                     extfile(LibFileARB01)
000800120803     FFNART01L  IF   E           K DISK    usropn
000900120803     F                                     extfile(LibFileART01)
001000120803     FFNBRV07L  IF   E           K DISK    usropn
001100120803     F                                     extfile(LibFileBRV07)
001200120807     FTIGCP51L  IF   E           K DISK
001300120803
001400120803
001500120803     D LibFileARB01    s             21A   inz
001600120803     D LibFileART01    s             21A   inz
001700120803     D LibFileBRV07    s             21A   inz
001800120803     D currSysNeta     s              8A   inz('*NULL')
001900991027
002000991027     D prmppt          s             50
002100991027     D prmesito        s              1
002200991027     D wrkesito        s                   like(prmesito)
002300110913     D fnvacds       e ds                  extname(fnvac00t)
002400120803     D sepFld          s              1    inz(';')
002500111216
002600120921     D skSGN           s             15    inz dim(500)
002700120921     D jSGN            s              3  0 inz
002800120921
002900120803     D wCCM            s                   like(vacCCM) inz
003000120803     D wAAS            s                   like(vacAAS) inz
003100120803     D wLNP            s                   like(vacLNP) inz
003200120803     D wNRS            s                   like(vacNRS) inz
003300120803     D wNSP            s                   like(vacNSP) inz
003400120803     D wRMO            s                   like(vacRMO) inz
003500120803     D wRMN            s                   like(vacRMN) inz
003600120803     D wRMA            s                   like(vacRMA) inz
003700120803     D wRSD            s                   like(vacRSD) inz
003800120803     D wPRD            s                   like(vacPRD) inz
003900120925     D wNZD            s                   like(arbNZD) inz
004000120803     D wLNA            s                   like(vacLNA) inz
004100120803     D wNCL            s                   like(vacNCL) inz
004200120803     D wDSP            s             10                 inz
004300120803     D wDCR            s             10                 inz
004400120803     D wDCP            s             10                 inz
004500120809     D wINGIAC         s              1                 inz
004600120809     D wDIROTT         s              1                 inz
004700120809     D wCAOPEN         s              1                 inz
004800120809
004900120809
005000120809     D pInTpChiamDCT   S              2S 0
005100120809     D pOutFNDCT_DS  E DS                  extname(FNDCT02L)
005200120809     D pOutEsitoDCT    S              1A
005300110913
005400110913
005500110913     D*------------------
005600110913     D* LINKING A DEFINIZIONI ESTERNE
005700110913     D*------------------
005800120803     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005900120803     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006000111222     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
006100111222     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
006200120803     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
006300120803     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
006400110913
006500110810
006600991027
006700120803     C*
006800120803     C* Reperisco il sistema AS/400 corrente
006900120803     C                   callp     UBRTVNETA_Rtv(currSysNeta)
007000120803     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
007100120803     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
007200120803     C                   eval      LibFileART01 = 'FILTRA201/FNART01L'
007300120803     C                   eval      LibFileBRV07 = 'FILTRA201/FNBRV07L'
007400120803     C                   else
007500120803     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
007600120803     C                   eval      LibFileART01 = 'FILTRAPRD/FNART01L'
007700120803     C                   eval      LibFileBRV07 = 'FILTRAPRD/FNBRV07L'
007800120803     C                   endif
007900120803     C*
008000120803     C* Apertura file "overraidati"
008100120803     C                   if        not %open(FNARB01L)
008200120803     C                   open      FNARB01L
008300120803     C                   endif
008400120803     C                   if        not %open(FNART01L)
008500120803     C                   open      FNART01L
008600120803     C                   endif
008700120803     C                   if        not %open(FNBRV07L)
008800120803     C                   open      FNBRV07L
008900120803     C                   endif
009000120803     C*
009100120803     C* Esecuzione operazioni di elaborazione
009200030908     C                   EXSR      chkRECFILMBR
009300991027     C                   EXSR      traduci
009400120803     C*
009500120803     C* Chiusura file "overraidati"
009600120803     C                   if        %open(FNARB01L)
009700120803     C                   close     FNARB01L
009800120803     C                   endif
009900120803     C                   if        %open(FNART01L)
010000120803     C                   close     FNART01L
010100120803     C                   endif
010200120803     C                   if        %open(FNBRV07L)
010300120803     C                   close     FNBRV07L
010400120803     C                   endif
010500120803     C*
010600921023     C                   SETON                                        LR
010700030908
010800110810
010900110810
011000030908     C     chkRECFILMBR  BEGSR
011100030908     C*
011200100209     C                   READ      TIVAWWWT                               55
011300120803     C*
011400120803     C* Se vuoto inserisco riga di intestazione
011500120803     C                   IF        *IN55
011600120803     C                   EVAL      VAWDTA  =
011700120810     C                              'CLI. MITTENTE'         +sepFld+
011800120810     C                              'ANNO SPED.'            +sepFld+
011900120810     C                              'LINEA PARTENZA'        +sepFld+
012000120810     C                              'SERIE SPED.'           +sepFld+
012100120810     C                              'NUM. SPED.'            +sepFld+
012200120810     C                              'MITT. ORIGINALE'       +sepFld+
012300120810     C                              'RIF. MITT. NUM.'       +sepFld+
012400120810     C                              'RIF. MITT. ALFA'       +sepFld+
012500120810     C                              'RAG. SOC. DEST.'       +sepFld+
012600120810     C                              'PROV. DEST'            +sepFld+
012700120925     C                              'NAZIONE DEST.'         +sepFld+
012800120810     C                              'LINEA ARRIVO'          +sepFld+
012900120810     C                              'COLLI SPED.'           +sepFld+
013000120810     C                              'DATA SPED.'            +sepFld+
013100120810     C                              'DATA CONS. RICH.'      +sepFld+
013200120810     C                              'DATA CONS. PARZ.'      +sepFld+
013300120810     C                              'IN GIAC.'              +sepFld+
013400120810     C                              'AVUTO DIR.'            +sepFld+
013500120921     C                              'C.A. APERTA'           +sepFld+
013600120921     C                              'SEGNACOLLO BRT'        +sepFld
013700120803     C                   WRITE     TIVAW000
013800120803     C                   ENDIF
013900030908     C*
014000030908     C                   ENDSR
014100110810
014200110810
014300991027
014400991027     C     traduci       BEGSR
014500991027     C*
014600090325     C                   READ      tivgdtmp
014700090325     C                   DOW       not %eof
014800120803     C                   movel     vgddta        fnvacds
014900120803     C*
015000120803     C* Inizializzazione variabili x buffer di output
015100120803     C                   clear                   wCCM
015200120803     C                   clear                   wAAS
015300120803     C                   clear                   wLNP
015400120803     C                   clear                   wNRS
015500120803     C                   clear                   wNSP
015600120803     C                   clear                   wRMO
015700120803     C                   clear                   wRMN
015800120803     C                   clear                   wRMA
015900120803     C                   clear                   wRSD
016000120803     C                   clear                   wPRD
016100120925     C                   clear                   wNZD
016200120803     C                   clear                   wLNA
016300120803     C                   clear                   wNCL
016400120803     C                   clear                   wDSP
016500120803     C                   clear                   wDCR
016600120803     C                   clear                   wDCP
016700120809     C                   clear                   wINGIAC
016800120809     C                   clear                   wDIROTT
016900120809     C                   clear                   wCAOPEN
017000110913     C*
017100110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
017200110913     C                   MOVEL     *blanks       wEsito1           1
017300110913     C                   MOVEL     *blanks       wEsito2           1
017400110913     C*
017500110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
017600110913     C                                                vacAAS
017700110913     C                                               :vacLNP
017800110913     C                                               :vacNRS
017900110913     C                                               :vacNSP
018000110913     C                                               :pOutLblTyp
018100110913     C                                               :pOutAnnoBO
018200110913     C                                               :pOutLineaParBO
018300110913     C                                               :pOutSerieBO
018400110913     C                                               :pOutNumSpedBO
018500110913     C                                               :pOutDcmBO
018600110913     C                                               :pOutCcaBO
018700110913     C                                               :pOutRblBO))
018800111109     C*
018900111109     C* Ritorno solamente le bolle valide come spedizione
019000111223     C                   IF        pOutLblTyp = 'O' AND pOutRblBO = 'N'
019100111220     C*
019200111220     C* Reperisco i dati della spedizione
019300120803     C                   SETON                                        51
019400111220     C                   EXSR      rtvDatiBolla
019500120803     C*
019600120803     C                   IF        not *in51
019700120803     C*
019800120809     C* Valorizzo il buffer d output
019900120803     C*
020000120803     C                   eval      wCCM = vacCCM
020100120803     C                   eval      wAAS = arbAAS
020200120803     C                   eval      wLNP = arbLNP
020300120803     C                   eval      wNRS = arbNRS
020400120803     C                   eval      wNSP = arbNSP
020500120803     C                   eval      wRMO = arbRMO
020600120803     C                   eval      wRMN = arbRMN
020700120803     C                   eval      wRMA = arbRMA
020800120803     C                   eval      wRSD = arbRSD
020900120803     C                   eval      wPRD = arbPRD
021000120925     C                   eval      wNZD = arbNZD
021100120803     C                   eval      wLNA = arbLNA
021200120803     C                   eval      wNCL = arbNCL
021300120809     C*
021400120809     C* Verifico se occorso dirottamento
021500120809     C                   if        vacCCA = '1'
021600120809     C                   eval      wDIROTT = 'S'
021700120809     C                   endif
021800120803     C*
021900120803     C* Formattazione date
022000120803     C*
022100120803     C                   eval      wDSP =
022200120803     C                              UBFMTDATE_Convert(
022300120803     C                                  %editc(arbAAS*10000+arbMGS:'4') :
022400120803     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
022500120803     C*
022600120803     C                   eval      wDCR =
022700120803     C                              UBFMTDATE_Convert(
022800120803     C                                  %editc(arbDCR:'4') :
022900120803     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
023000120803     C*
023100120803     C                   eval      wDCP =
023200120803     C                              UBFMTDATE_Convert(
023300120803     C                                  %editc(arbDCP:'4') :
023400120803     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
023500120921     C*
023600120921     C* Ciclo x ogni segnacollo mancante o non in consegna
023700120921     C                   z-add     1             jSGN
023800120921     C                   dow       jSGN <= %elem(skSGN)
023900120921     C*
024000120921     C                   if        jSGN = 1
024100120921     C                   eval      VAWDTA  =
024200120921     C                                     %editc(wCCM:'X')             +sepFld+
024300120921     C                                     %editc(wAAS:'X')             +sepFld+
024400120921     C                                     %editc(wLNP:'X')             +sepFld+
024500120921     C                                     %editc(wNRS:'X')             +sepFld+
024600120921     C                                     %editc(wNSP:'X')             +sepFld+
024700120921     C                                      %trim(wRMO)                 +sepFld+
024800120921     C                                     %editc(wRMN:'X')             +sepFld+
024900120921     C                                      %trim(wRMA)                 +sepFld+
025000120921     C                                      %trim(wRSD)                 +sepFld+
025100120921     C                                      %trim(wPRD)                 +sepFld+
025200120925     C                                      %trim(wNZD)                 +sepFld+
025300120921     C                                     %editc(wLNA:'X')             +sepFld+
025400120921     C                                     %editc(wNCL:'X')             +sepFld+
025500120921     C                                      %trim(wDSP)                 +sepFld+
025600120921     C                                      %trim(wDCR)                 +sepFld+
025700120921     C                                      %trim(wDCP)                 +sepFld+
025800120921     C                                      %trim(wINGIAC)              +sepFld+
025900120921     C                                      %trim(wDIROTT)              +sepFld+
026000120921     C                                      %trim(wCAOPEN)              +sepFld+
026100120921     C                                      ''''+%trim(skSGN(jSGN))     +sepFld
026200120921     C                   write     TIVAW000
026300120921     C                   else
026400120921     C                   if        skSGN(jSGN) = *blanks
026500120921     C                   leave
026600120921     C                   else
026700120921     C                   eval      VAWDTA  =
026800120921     C                                     ' ' +sepFld+ ' ' +sepFld+
026900120921     C                                     ' ' +sepFld+ ' ' +sepFld+
027000120921     C                                     ' ' +sepFld+ ' ' +sepFld+
027100120921     C                                     ' ' +sepFld+ ' ' +sepFld+
027200120921     C                                     ' ' +sepFld+ ' ' +sepFld+
027300120921     C                                     ' ' +sepFld+ ' ' +sepFld+
027400120921     C                                     ' ' +sepFld+ ' ' +sepFld+
027500120921     C                                     ' ' +sepFld+ ' ' +sepFld+
027600120921     C                                     ' ' +sepFld+ ' ' +sepFld+
027700120921     C                                     ''''+%trim(skSGN(jSGN))      +sepFld
027800120921     C                   write     TIVAW000
027900120921     C                   endif
028000120921     C                   endif
028100120921     C                   add       1             jSGN
028200120921     C                   enddo
028300111109     C*
028400111109     C                   ENDIF
028500120803     C                   ENDIF
028600110810     C*
028700090325     C                   delete    tivgd000
028800991027     C*
028900090325     C                   READ      tivgdtmp
029000030325     C                   ENDDO
029100991027     C*
029200030325     C                   EVAL      wrkesito = '0'
029300991027     C*
029400910830     C                   ENDSR
029500111216
029600111216
029700111216
029800111216     C     rtvDatiBolla  BEGSR
029900120921     C*
030000120921     C                   clear                   skSGN
030100120921     C                   z-add     *zeros        jSGN
030200120803     C*
030300120803     C* Aggancio la bolla in filiale arrivo
030400120803     C     KEYarb01_C    chain     fnarb01l
030500120803     C                   if        %found(fnarb01l)
030600120807     C*
030700120809     C* Aggancio l'ultima giacenza
030800120807     C                   eval      gcpFRG = *zeros
030900120807     C     KEYgcp51      chain     tigcp51l
031000120807     C                   if        %found(tigcp51l)
031100120807     C*
031200120809     C* Verifico se bolla attualmente in giacenza
031300120807     C                   if        gcpATB = *blanks AND gcpFAS < 40
031400120809     C                   eval      wINGIAC = 'S'
031500120809     C                   endif
031600120809     C                   endif
031700120809     C*
031800120809     C* Reperisco l'ultimo danno ("valido ai fini internet")
031900120809     C                   call      'UBRTVDCT'
032000120809     C                   parm      vacAAS        pInAnno
032100120809     C                   parm      vacLNP        pInLineaPar
032200120809     C                   parm      vacNRS        pInSerie
032300120809     C                   parm      vacNSP        pInNumSped
032400120809     C                   parm      01            pInTpChiamDCT
032500120809     C                   parm                    pOutFNDCT_DS
032600120809     C                   parm                    pOutEsitoDCT
032700120809     C*
032800120809     C* Ritorno se attualmente c'è una C.A. aperta
032900120809     C                   if        pOutEsitoDCT = '1'
033000120809     C                   if        dctDCH = *zeros
033100120809     C                   eval      wCAOPEN = 'S'
033200120809     C                   endif
033300120809     C                   endif
033400120803     C*
033500120803     C* Inizializzo il contatore di spunte 4
033600120803     C                   z-add     *zeros        wContaSpunte_4    5 0
033700120803     C*
033800120803     C* Verifico le spunte
033900120803     C*
034000120803     C* - 1° scorro FNART01L (segnacolli) della bolla corrente
034100120803     C     KEYart01_P    setll     fnart01l
034200120921     C                   if        %equal(fnart01l)
034300120803     C     KEYart01_P    reade     fnart01l
034400120803     C                   dow       not %eof(fnart01l)
034500120921     C*
034600120921     C* Inizializzo a false la presenza fisica del collo
034700120921     C                   movel     'N'           wSegnacOK         1
034800120803     C*
034900120803     C* - 2° scorro FNBRV07L (spunte) del segnacollo bolla corrente
035000120803     C     KEYbrv07_P    setll     fnbrv07l
035100120921     C                   if        %equal(fnbrv07l)
035200120803     C     KEYbrv07_P    reade     fnbrv07l
035300120803     C                   dow       not %eof(fnbrv07l)
035400120803     C*
035500120803     C* Verifico unicamente l'esistenza delle spunte 4
035600120803     C                   if        brvNPG = 4
035700120803     C                   add       1             wContaSpunte_4
035800120921     C                   movel     'S'           wSegnacOK
035900120803     C                   endif
036000120803     C     KEYbrv07_P    reade     fnbrv07l
036100120803     C                   enddo
036200120921     C                   endif
036300120921     C*
036400120921     C* Se segnacollo non OK => collezione x essere restituito
036500120921     C                   if        wSegnacOK = 'N'
036600120921     C                   add       1             jSGN
036700120921     C                   eval      skSGN(jSGN) = %editc(artFLS:'X')+
036800120921     C                                           %editc(artLNA:'X')+
036900120921     C                                           %editc(artNRS:'X')+
037000120921     C                                           %editc(artNSC:'X')
037100120921     C                   endif
037200120803     C*
037300120803     C     KEYart01_P    reade     fnart01l
037400120803     C                   enddo
037500120803     C                   endif
037600120803     C*
037700120803     C* Considerazioni su quante spunte 4 sono state trovate
037800120803     C                   if        wContaSpunte_4 <> arbNCL
037900120803     C                   setoff                                       51
038000120803     C                   endif
038100120803     C*
038200120803     C                   endif
038300111221     C*
038400111216     C                   ENDSR
038500991027
038600110810
038700110810
038800991027      /TITLE Operazioni iniziali.
038900991027     C     *inzsr        BEGSR
039000991027     C*
039100991027     C     *ENTRY        PLIST
039200991027     C                   parm                    prmppt
039300991027     C     wrkesito      parm      wrkesito      prmesito
039400120803     C*
039500120803     C* Definizione chiavi
039600120803     C*
039700120803     C* Chiave su FNARB01L - Completa
039800120803     C     KEYarb01_C    KLIST
039900120803     C                   KFLD                    vacAAS
040000120803     C                   KFLD                    vacLNP
040100120803     C                   KFLD                    vacNRS
040200120803     C                   KFLD                    vacNSP
040300120807     C*
040400120807     C* Chiave su TIGCP51L - Completa
040500120807     C     KEYgcp51      KLIST
040600120807     C                   KFLD                    arbAAS
040700120807     C                   KFLD                    arbLNP
040800120807     C                   KFLD                    arbNRS
040900120807     C                   KFLD                    arbNSP
041000120807     C                   KFLD                    gcpFRG
041100120803     C*
041200120803     C* Chiave su FNART01L - Parziale
041300120803     C     KEYart01_P    KLIST
041400120803     C                   KFLD                    arbAAS
041500120803     C                   KFLD                    arbLNP
041600120803     C                   KFLD                    arbNRS
041700120803     C                   KFLD                    arbNSP
041800120803     C*
041900120803     C* Chiave su FNBRV07L - Parziale
042000120803     C     KEYbrv07_P    KLIST
042100120803     C                   KFLD                    artFLS
042200120803     C                   KFLD                    artLNA
042300120803     C                   KFLD                    artNRS
042400120803     C                   KFLD                    artNSC
042500030709     C*
042600991027     C                   ENDSR
