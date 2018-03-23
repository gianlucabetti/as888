000100121009     H DECEDIT('0,') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120427
000400120712     FTITAS30C  IF   E           K DISK
000500120808     FTITA430C  IF   E           K DISK
000600120808     FFNORM01L  IF   E           K DISK
000700121016     FTABEL00F  IF   E           K DISK
000800120712     FTISTB03L  UF   E           K DISK    commit
000900090323     FTIVAWWWT  UF A E             DISK    commit
001000120727     FTIVGD00F  O    E             DISK
001100120427
001200030924
001300041015     D*-----------
001400041015     D* VARIABILI D WRK
001500041015     D*-----------
001600031103     D prmppi          S             50
001700030924     D prmesito        S              1
001800030924     D wrkesito        S                   like(prmesito)
001900121009     D skRMN           S                   like(stbRMN) dim(64000)
002000121009     D*skOUT           S           1254    dim(10000) inz
002100121009     D skOUT           S            234    dim(64000) inz
002200121009     D iOUT            S              6  0 inz
002300121009     D wTyRec          S              1    inz
002400121009     D wFillerL        S            600    inz
002500121009     D wFillerH        S           1020    inz
002600121016     D sk2Alavv        s             10    DIM(500)
002700121019     D sk2Agiac        s             10    DIM(500)
002800110929
002900041015
003000041015     D*-----------
003100041015     D* DEFINIZIONE DS ESTERNE
003200041015     D*-----------
003300120712     D TITV2L3DF     E DS
003400120712     D TITV2L3DH     E DS
003500120712     D TITV2L3DL     E DS
003600120808     D DSBL4M        E DS
003700121016     D DS2A          E DS
003800120712
003900031103
004000031103     D*-----------
004100031103     D* VARIABILI RIFERITE AL DATA-BASE
004200031103     D*-----------
004300080618     D wKstbKSU        S                   like(stbKSU)
004400080618     D wKstbFTR        S                   like(stbFTR)
004500030924
004600110103
004700031103
004800031103     D*-----------
004900031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005000031103     D*-----------
005100031103     D DSPPI           DS
005200120727     D  w§PPITIP                      2
005300080618     D  w§PPIKSU                      7  0
005400120808     D  w§PPITIP2                     2
005500120808     D  w§PPIENV                      1
005600031103
005700110928
005800110928     D*------------------
005900110928     D* LINKING A DEFINIZIONI ESTERNE
006000110928     D*------------------
006100110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006200110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006300110928
006400030924
006500030325
006600030924     C* MAIN/
006700121016     C*
006800121016     C* Caricamento tabelle
006900121016     C                   EXSR      cartab
007000041015     C*
007100041015     C* Esegue traduzione
007200991027     C                   EXSR      traduci
007300120809     C*
007400120809     C* Scarico i dati d output
007500120809     C                   EXSR      wrirec
007600110929     C*
007700921023     C                   SETON                                        LR
007800111121
007900111121
008000120712
008100030924
008200991027     C     traduci       BEGSR
008300120712     C*
008400120809     C                   setoff                                       40
008500120712     C                   setoff                                       50
008600120727     C                   setoff                                       51
008700121010     C                   setoff                                       52
008800120712     C*
008900120712     C* 1° => Record di file-header
009000120712     C                   clear                   TITV2L3DF
009100120712     C                   eval      F§REGTYPE  = 'F'
009200120712     C                   eval      F§FILETYPE = 'CDLCF'
009300120712     C                   eval      F§CARRCD   = 'BARTOLINI_TR'
009400120712     C                   eval      F§CRTDT    = %char(%dec(%date() : *ISO))
009500120712     C                   eval      F§CRTTM    = %editc(%dec(%time() : *ISO):'X')
009600120712     C                   eval      F§COLLECT  = 'DELI'
009700120712     C*
009800120809     C* Popolo la schiera degli output
009900120809     C                   eval      vawDTA = TITV2L3DF
010000121009     C                   eval      wTyRec = 'F'
010100120809     C                   if        not *in50
010200120809     C                   z-add     1             iOUT
010300121009     C                   eval      skOUT(iOUT) = wTyRec+vawDTA
010400120809     C                   endif
010500120712     C*
010600120712     C* 2° => Record di shipment-header
010700120719     C*
010800120719     C* Inizializzo il contatore di spedizioni elaborate
010900121009     C                   z-add     0             skRMNidx          6 0
011000031103     C*
011100031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
011200031103     C* di traduzione (trattasi d "post-traduzione")
011300120427     C                   eval      wKstbKSU = w§PPIKSU
011400120427     C                   eval      wKstbFTR = *blanks
011500120427     C     KEYstb03P     setll     tistb03l
011600120427     C                   if        %equal(tistb03l)
011700120427     C     KEYstb03P     reade     tistb03l
011800991027     C*
011900031103     C* Ciclo x tuttii record del cliente da trasmettere
012000120427     C                   dow       not %eof(tistb03l)
012100080618     C*
012200120427     C                   seton                                        50
012300121010     C                   setoff                                       52
012400120808     C                   setoff                                       56
012500031103     C*
012600031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
012700031103     C* deve restituire al cliente nel file tradotto
012800120427     C                   if        stbFTR = *blanks                             * ridondante
012900110103     C*
013000120427     C                   setoff                                       50
013100121113     C*
013200121113     C* Considero UNICAMENTE le spedizioni aventi:
013300121113     C* - LNP = 089
013400121114     C**** - NRS = 03
013500121113     C* - RMN > 24.000.000
013600121114     C* - RMA = 'LOGHISTES SAP' or 'PRIVALIA_BCN'
013700121113     C                   if        stbLNP = 089      AND
013800121114     C***                             stbNRS = 03       AND
013900121114     C                             stbRMN > 24000000 AND
014000130527     C                             (stbRMA = 'LOGHISTES SAP'   OR
014100130527     C                              stbRMA = 'PRIVALIA_BCN'    OR
014200130527     C                              stbRMA = 'ECILOG'          OR
014300130527     C                              stbRMA = 'ITALIAN STOCK @' OR
014400130527     C                              stbRMA = 'IS_PRIVALIA_BCN')
014500120719     C*
014600120719     C* Verifico che nel medesimo "giro" (ovvero file) non sia presente più volte lo stesso RMN
014700120719     C     stbRMN        lookup    skRMN                                  55
014800120727     C                   if        %found
014900120727     C                   seton                                        51
015000121010     C                   seton                                        52
015100120727     C                   else
015200121012     C*
015300121012     C* Verifico immediatamente data/ora stato corrente => se "futura" skippo completamente
015400121012     C                   if        stbDAS*10000+stbORS >
015500121012     C                             %int((%dec(%date():*ISO)*1000000+
015600121012     C                             %dec(%time()))/100)
015700121012     C                   seton                                        52
015800121012     C                   else
015900120719     C*
016000120719     C* Salvo il RMN corrente
016100120719     C                   add       1             skRMNidx
016200120719     C                   eval      skRMN(skRMNidx) = stbRMN
016300120808     C*
016400120808     C* Inizio a comporre il bufffer d output "comune" (sia alle distributions che ai return)
016500120808     C                   clear                   TITV2L3DH
016600120808     C                   eval      H§REGTYPE  = 'H'
016700120808     C                   eval      H§ACTDT    = %editc(stbDAS:'X')
016800120808     C                   eval      H§ACTTM    = %trim(%editw(stbORS:'0    00'))
016900120808     C*
017000120808     C* Per stati provenientio dalle bolle ...
017100120808     C                   if        stbPRS <> '6'
017200031103     C*
017300031103     C* Innanzitutto chaino la bolla direttamente su TITAS
017400120427     C     KEYtas30P     chain     titas30c
017500120427     C                   if        %found(titas30c)
017600121016     C*
017700121016     C* Verifico se evento lasciato avviso
017800121019     C                   movel     *blanks       wTIPEVE           1
017900121016     C                   if        stbPRS = '2'
018000121016     C                   z-add     1             i                 3 0
018100121016     C     stbCOS        lookup    sk2Alavv(i)                            55
018200121016     C                   if        %found
018300121019     C                   movel     'A'           wTIPEVE
018400121016     C                   endif
018500121019     C                   z-add     1             ii                3 0
018600121019     C     stbCOS        lookup    sk2Agiac(ii)                           55
018700121019     C                   if        %found
018800121019     C                   movel     'G'           wTIPEVE
018900121019     C                   endif
019000121016     C                   endif
019100120427     C*
019200120808     C                   eval      H§CARRACC  = '0896769'
019300120808     C                   eval      H§CARRCUS  = '0896769'
019400120808     C                   eval      H§REF_1    = '00000'+%editc(stbRMN:'X')
019500120808     C                   eval      H§REF_2    = H§REF_1
019600121004     C                   eval      H§TRACKNR  = %editc(tasLNP:'X') +
019700121004     C                                          %editc(tasNRS:'X') +
019800121004     C                                          %editc(tasNSP:'X')
019900121004     C                   eval      H§EXPNR    = %editc(tasLNP:'X') +
020000121004     C                                          %editc(tasNRS:'X') +
020100121004     C                                          %editc(tasNSP:'X')
020200121004     C***                eval      H§TRACKNR  = stbRMA
020300121004     C***                eval      H§EXPNR    = stbRMA
020400120712     C                   eval      H§TOTPACK  = %editc(tasNCL:'X')
020500120712     C                   eval      H§TOTWT    = %trim(
020600120712     C                                  %editw(tasPKB:'0      . 00'))
020700120712     C                   eval      H§TOTVOL   = %trim(
020800120712     C                                  %editw(tasVLB:'000000000  .   '))
020900120808     C*
021000120427     C                   select
021100120427     C*
021200121107     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
021300121019     C***                eval      H§STS = '00'
021400121019     C***                eval      H§STS = '02'
021500121107     C                   eval      H§STS = '04'
021600121019     C***                seton                                        50
021700120712     C*
021800120712     C                   when      %subst(stbCOS:1:9) = 'COLNOTAFF'
021900120712     C                   eval      H§STS = '01'
022000120427     C*
022100121019     C***                when      %subst(stbCOS:1:9) = 'ARRPRICOL'
022200121019     C***                eval      H§STS = '02'
022300121019     C***                if        stbDAS = *zeros
022400121019     C***                seton                                        50
022500121019     C***                endif
022600120712     C*
022700121107     C                   when      %trim(stbCOS) = 'MIC'
022800121107     C                   eval      H§STS = '04'
022900121107     C*
023000121107     C                   when      %trim(stbCOS) = 'RIC'
023100121107     C                   eval      H§STS = '04'
023200121016     C*
023300121019     C                   when      wTIPEVE = 'A'
023400121016     C                   eval      H§STS = '04'
023500121019     C                   eval      H§INC = 'S40'
023600121019     C*
023700121019     C                   when      wTIPEVE = 'G' AND (
023800121019     C                             %trim(stbCOS) = '001' or
023900121019     C                             %trim(stbCOS) = '002' or
024000121019     C                             %trim(stbCOS) = '003' or
024100121019     C                             %trim(stbCOS) = '004' or
024200121019     C                             %trim(stbCOS) = '005' or
024300121019     C                             %trim(stbCOS) = '006' or
024400121019     C                             %trim(stbCOS) = '008' or
024500121019     C                             %trim(stbCOS) = '009' or
024600121019     C                             %trim(stbCOS) = '012' or
024700121019     C                             %trim(stbCOS) = '016' or
024800121019     C                             %trim(stbCOS) = '026' or
024900121019     C                             %trim(stbCOS) = '035' or
025000121019     C                             %trim(stbCOS) = '037'     )
025100121019     C                   eval      H§STS = '04'
025200121019     C                   eval      H§INC = 'S10'
025300121019     C*
025400121019     C                   when      wTIPEVE = 'G' AND (
025500121019     C                             %trim(stbCOS) = '019' or
025600121019     C                             %trim(stbCOS) = '021' or
025700121019     C                             %trim(stbCOS) = '022' or
025800121019     C                             %trim(stbCOS) = '028'     )
025900121019     C                   eval      H§STS = '04'
026000121019     C                   eval      H§INC = 'S30'
026100121016     C*
026200121016     C                   when      %subst(stbCOS:1:7) = 'GIACAPE'
026300121016     C                   eval      H§STS = '04'
026400121019     C                   eval      H§INC = 'S50'
026500120712     C*
026600120712     C                   when      %subst(stbCOS:1:4) = 'RESO'
026700120712     C                   eval      H§STS = '06'
026800120712     C*
026900120712     C                   when      %trim(stbCOS) = 'CONSOK'
027000120712     C                   eval      H§STS = '07'
027100120427     C*
027200120427     C                   other
027300120712     C                   eval      H§STS = '??'
027400120712     C                   seton                                        50
027500120712     C*
027600120427     C                   endsl
027700120808     C*
027800120808     C* Reperisco la chiave del ORM legato alla bolla corrente
027900120808     C                   clear                   dsbl4m
028000120808     C                   eval      ta4TRC = 'M'
028100120808     C     KEYta430C     chain     tita430c
028200120808     C                   if        %found(tita430C)
028300120808     C                   if        ta4NOT <> *blanks
028400120808     C                   eval      dsbl4m = ta4NOT
028500120808     C*
028600120808     C* Aggancio l'ORM
028700120808     C     KEYorm01C     chain     fnorm01l
028800120808     C                   if        %found(fnorm01l)
028900120808     C                   if        ormATB = *blanks
029000120808     C*
029100120808     C* X questo caso considero solamente gli ORM singoli e pervenuti da file
029200120813     C                   if        ormTOR = 'S' and ormTCO = 'F' and
029300120813     C                             ormRFA = stbRMA
029400120808     C                   seton                                        56
029500120808     C*
029600120808     C* Se trattasi di "return" verifico in base allo stato se ritornare o meno il dato al cliente
029700120808     C                   select
029800120808     C*
029900121019     C***                when      H§STS = '00'
030000121019     C***                seton                                        50
030100120808     C*
030200120808     C                   when      H§STS = '07'
030300120808     C                   eval      H§STS = '09'
030400121019     C*
030500121019     C                   other
030600121019     C                   seton                                        50
030700120808     C*
030800120808     C                   endsl
030900120808     C*
031000120808     C                   eval      H§REF_1    = '00000'+ormRFA
031100120808     C*
031200120808     C* Se no ambiente di test valorizzo anche il REF 2
031300120808     C                   if        w§PPIENV = *blanks
031400120808     C                   eval      H§REF_2    = ormNAM
031500120808     C                   endif
031600120808     C*
031700120808     C                   endif
031800120808     C                   endif
031900120808     C                   endif
032000120808     C*
032100120808     C                   endif
032200120808     C                   endif
032300120808     C*
032400120808     C                   else
032500120808     C                   seton                                        50
032600120808     C                   endif
032700120808     C*
032800120808     C                   endif
032900120808     C*
033000120808     C* Per stati provenineti dagli ORM ...
033100120808     C                   if        stbPRS = '6'
033200120808     C*
033300120808     C                   seton                                        50
033400120808     C                   clear                   dsbl4m
033500120808     C                   move      stbRMN        wKeyORM          14 0
033600120808     C                   movel     wKeyORM       dsbl4m
033700120808     C*
033800120808     C* Aggancio l'ORM
033900120808     C     KEYorm01C     chain     fnorm01l
034000120808     C                   if        %found(fnorm01l)
034100120808     C                   if        ormATB = *blanks
034200120808     C*
034300120808     C* X questo caso considero solamente gli ORM singoli e pervenuti da file
034400120808     C                   if        ormTOR = 'S' and ormTCO = 'F'
034500120808     C                   setoff                                       50
034600120808     C                   seton                                        56
034700120808     C*
034800120808     C                   eval      H§CARRACC  = '0894539'
034900120808     C                   eval      H§CARRCUS  = '0894539'
035000120808     C                   eval      H§REF_1    = '00000'+ormRFA
035100120809     C*
035200120809     C* Se no ambiente di test valorizzo anche il REF 2
035300120809     C                   if        w§PPIENV = *blanks
035400120809     C                   eval      H§REF_2    = ormNAM
035500120809     C                   endif
035600120809     C*
035700121004     C                   eval      H§TRACKNR  = *zeros
035800121004     C                   eval      H§EXPNR    = *zeros
035900121004     C***                eval      H§TRACKNR  = ormRFA
036000121004     C***                eval      H§EXPNR    = ormRFA
036100120808     C                   eval      H§TOTPACK  = %editc(ormNCL:'X')
036200120808     C                   eval      H§TOTWT    = %trim(
036300120808     C                                  %editw(ormPKG:'0      . 00'))
036400120808     C                   eval      H§TOTVOL   = %trim(
036500120808     C                                  %editw(ormVLM:'000000000  .   '))
036600120808     C*
036700120808     C* In caso di "return" ancora in fase "ORM" restituisco solamente le fasi :
036800120809     C* 000, 050, 999
036900120808     C                   select
037000120808     C*
037100120808     C                   when      stbCOS = 'O000'
037200121019     C***                eval      H§STS = '00'
037300121019     C                   eval      H§STS = '02'
037400120808     C*
037500121019     C***                when      stbCOS = 'O050'
037600121019     C***                eval      H§STS = '00'
037700121019     C***                eval      H§STS = '02'
037800120808     C*
037900120808     C                   when      stbCOS = 'O999'
038000120808     C                   eval      H§STS = '08'
038100120808     C*
038200120808     C                   other
038300120808     C                   seton                                        50
038400120808     C*
038500120808     C                   endsl
038600120808     C*
038700120808     C                   endif
038800120808     C                   endif
038900120808     C                   endif
039000120808     C*
039100120808     C                   endif
039200080618     C*
039300080618     C* Scarico il buffer d output
039400120712     C                   eval      vawDTA = TITV2L3DH
039500121009     C                   eval      wTyRec = 'H'
039600120809     C  N50              exsr      INSREC
039700120712     C*
039800120712     C* 3° => Record di shipment-pack (solo se record di testata ok)
039900120808     C                   if        not *in50 and not *in56
040000120712     C                   clear                   TITV2L3DL
040100120712     C                   eval      L§REGTYPE  = 'L'
040200120712     C                   eval      L§ACTDT    = H§ACTDT
040300120712     C                   eval      L§ACTTM    = H§ACTTM
040400120712     C                   eval      L§REF_1    = H§REF_1
040500120712     C                   eval      L§STS      = H§STS
040600120712     C                   eval      L§PACKID   = *all'0'
040700120712     C                   eval      L§PCKWT    = '000000.000'
040800120712     C                   eval      L§PCKVOL   = '0000000000.000'
040900120712     C                   eval      L§PCKNR    = *all'0'
041000120712     C*
041100120712     C* Scarico il buffer d output
041200120712     C                   eval      vawDTA = TITV2L3DL
041300121009     C                   eval      wTyRec = 'L'
041400120809     C  N50              exsr      INSREC
041500120712     C                   endif
041600121012     C*
041700121012     C                   endif
041800031103     C*
041900031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
042000120427     C                   eval      stbFTR = 'T'
042100121010     C  N52              update    tistb000
042200121113     C*
042300121113     C                   endif
042400120712     C*
042500120712     C                   endif
042600120719     C*
042700120719     C                   endif
042800991027     C*
042900120427     C     KEYstb03P     reade     tistb03l
043000120427     C                   setoff                                       50
043100120427     C                   enddo
043200120427     C                   endif
043300120727     C*
043400120727     C* Se "spia" accesa => inserisco un record "*VOID"
043500120727     C                   if        *in51
043600120727     C                   clear                   tivgd000
043700120808     C                   eval      vgdTIP = w§PPITIP
043800120727     C                   eval      vgdKSU = '0'+%editc(w§PPIKSU:'X')
043900120727     C                   eval      vgdTSC = 'WW'
044000120727     C                   eval      vgdDAT = %dec(%date() : *ISO)
044100120727     C                   eval      vgdPGM = 'TITV2L3R6'
044200120727     C                   eval      vgdDTA = '*VOID'
044300120727     C                   write     tivgd000
044400120727     C                   endif
044500991027     C*
044600120427     C                   eval      wrkesito = '0'
044700991027     C*
044800910830     C                   ENDSR
044900031103
045000031103
045100031103
045200120809     C     insrec        BEGSR
045300120808     C*
045400120808     C* A seconda che stia considerando "distributions" o "return" => gestisco diversamente
045500120808     C                   if        *in56
045600120808     C                   clear                   tivgd000
045700120808     C                   eval      vgdTIP = w§PPITIP2
045800120808     C                   eval      vgdKSU = '0'+%editc(w§PPIKSU:'X')
045900120808     C                   eval      vgdTSC = 'WW'
046000120808     C                   eval      vgdDAT = %dec(%date() : *ISO)
046100120808     C                   eval      vgdPGM = 'TITV2L3R6'
046200121009     C                   eval      vgdDTA = vawDTA
046300120808     C                   write     tivgd000
046400120808     C*
046500120808     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
046600120808     C                   else
046700110928     C*
046800120427     C* Considero solamente le bolle originali
046900111103     C                   movel     *blanks       wEsito1           1
047000111103     C*
047100111103     C* Chiamata metodo GetLblTyp
047200111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
047300111103     C                                                stbAAS
047400111103     C                                               :stbLNP
047500111103     C                                               :stbNRS
047600111103     C                                               :stbNSP
047700111103     C                                               :pOutLblTyp
047800111103     C                                               :pOutAnnoBO
047900111103     C                                               :pOutLineaParBO
048000111103     C                                               :pOutSerieBO
048100111103     C                                               :pOutNumSpedBO
048200111103     C                                               :pOutDcmBO
048300111103     C                                               :pOutCcaBO
048400111103     C                                               :pOutRblBO))
048500111103     C*
048600120427     C                   if        pOutLblTyp = 'O' and
048700120427     C                             not *IN50                                    * ridondante
048800120809     C*
048900120809     C* Popolo la schiera degli output
049000120809     C                   seton                                        40
049100120809     C                   add       1             iOUT
049200121009     C                   eval      skOUT(iOUT) = wTyRec+vawDTA
049300120809     C*
049400120427     C                   endif
049500031103     C*
049600120808     C                   endif
049700120808     C*
049800031103     C                   ENDSR
049900120809
050000120809
050100120809
050200120809     C     wrirec        BEGSR
050300120809     C*
050400120809     C* Solo se presenti anche record d spedizione => scarico lo stack di buffer d output
050500120809     C                   if        *in40
050600120809     C                   eval      iOUT = 1
050700120809     C                   dow       iOUT <= %elem(skOUT) AND
050800120809     C                             skOUT(iOUT) <> *blanks
050900121009     C                   eval      wTyRec = %subst(skOUT(iOUT):1:1)
051000121009     C*
051100121009     C                   select
051200121009     C                   when      wTyRec = 'F'
051300121010     C                   eval      vawDTA = %subst(skOUT(iOUT):2)
051400121009     C                   when      wTyRec = 'L'
051500121009     C                   eval      vawDTA = %subst(skOUT(iOUT):2)+wFillerL
051600121009     C                   when      wTyRec = 'H'
051700121009     C                   eval      vawDTA = %subst(skOUT(iOUT):2)+wFillerH
051800121009     C                   endsl
051900121009     C*
052000121009     C***                eval      vawDTA = skOUT(iOUT)
052100120809     C                   write     tivaw000
052200120809     C                   add       1             iOUT
052300120809     C                   enddo
052400120809     C                   endif
052500120809     C*
052600120809     C                   ENDSR
052700031103
052800121016
052900121016     C     cartab        BEGSR
053000121016     C*
053100121016     C* Carico causali evento divise x categoria (A=Lavv G=Giac)
053200121016     C                   clear                   sk2Alavv
053300121016     C                   z-add     *zeros        i
053400121019     C                   z-add     *zeros        ii
053500121016     C                   eval      tblKUT = 1
053600121016     C                   eval      tblCOD = '2A'
053700121016     C     KEYtab00P     setll     tabel00f
053800121016     C     KEYtab00P     reade     tabel00f
053900121016     C                   dow       not %eof(tabel00f)
054000121016     C                   movel(p)  tblUNI        ds2a
054100121016     C                   if        §2aFTC = 'A'
054200121016     C                   add       1             i
054300121016     C                   eval      sk2Alavv(i)  = tblKEY
054400121016     C                   endif
054500121019     C                   if        §2aFTC = 'G'
054600121019     C                   add       1             ii
054700121019     C                   eval      sk2Agiac(ii) = tblKEY
054800121019     C                   endif
054900121016     C     KEYtab00P     reade     tabel00f
055000121016     C                   enddo
055100121016     C*
055200121016     C                   ENDSR
055300121016
055400031103
055500991027
055600991027      /TITLE Operazioni iniziali.
055700991027     C     *inzsr        BEGSR
055800991027     C*
055900991027     C     *ENTRY        PLIST
056000031103     C                   parm                    prmppi
056100991027     C     wrkesito      parm      wrkesito      prmesito
056200031103     C*
056300031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
056400031103     C                   EVAL      DSPPI = prmppi
056500030709     C*
056600030709     C* Definizione chiavi
056700110927     C*
056800110103     C* Chiave su TISTB03L - Parziale
056900110103     C     KEYstb03P     KLIST
057000031104     C                   KFLD                    wKstbKSU
057100031103     C                   KFLD                    wKstbFTR
057200031103     C*
057300031103     C* Chiave su TITAS30C - Parziale
057400031103     C     KEYtas30P     KLIST
057500031103     C                   KFLD                    stbAAS
057600031103     C                   KFLD                    stbLNP
057700031103     C                   KFLD                    stbNRS
057800031103     C                   KFLD                    stbNSP
057900121016     C*
058000121016     C* Chiave su TABEL00F - Parziale
058100121016     C     KEYtab00P     KLIST
058200121016     C                   KFLD                    tblKUT
058300121016     C                   KFLD                    tblCOD
058400120808     C*
058500120808     C* Chiave su TITA430C - Completa
058600120808     C     KEYta430C     KLIST
058700120808     C                   KFLD                    stbAAS
058800120808     C                   KFLD                    stbLNP
058900120808     C                   KFLD                    stbNRS
059000120808     C                   KFLD                    stbNSP
059100120808     C                   KFLD                    ta4TRC
059200120808     C*
059300120808     C* Chiave su FNORM01L - Completa
059400120808     C     KEYorm01C     KLIST
059500120808     C                   KFLD                    §B4POE
059600120808     C                   KFLD                    §B4NSR
059700120808     C                   KFLD                    §B4NOR
059800120808     C                   KFLD                    §B4NRV
059900991027     C*
060000991027     C                   ENDSR
