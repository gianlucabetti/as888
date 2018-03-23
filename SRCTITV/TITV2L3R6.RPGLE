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
001900130807     D skRMN           S                   like(stbRMN) dim(1000000)
002000130807     D skOUT           S              1    dim(419324) inz
002100130807     D skOUT1          S             40    dim(419324) inz
002200130807     D skOUT2          S             40    dim(419324) inz
002300130807     D skOUT3          S             40    dim(419324) inz
002400130807     D skOUT4          S             40    dim(419324) inz
002500130807     D skOUT5          S             40    dim(419324) inz
002600130807     D skOUT6          S             33    dim(419324) inz
002700121009     D iOUT            S              6  0 inz
002800130807     D wTyRec          S              1    inz
002900130807     D wFillerL        S            600    inz
003000130807     D wFillerH        S           1020    inz
003100130807     D wBufferStd      S            234    inz
003200130807     D sk2Alavv        S             10    DIM(500)
003300130807     D sk2Agiac        S             10    DIM(500)
003400110929
003500041015
003600041015     D*-----------
003700041015     D* DEFINIZIONE DS ESTERNE
003800041015     D*-----------
003900120712     D TITV2L3DF     E DS
004000120712     D TITV2L3DH     E DS
004100120712     D TITV2L3DL     E DS
004200120808     D DSBL4M        E DS
004300121016     D DS2A          E DS
004400120712
004500031103
004600031103     D*-----------
004700031103     D* VARIABILI RIFERITE AL DATA-BASE
004800031103     D*-----------
004900080618     D wKstbKSU        S                   like(stbKSU)
005000080618     D wKstbFTR        S                   like(stbFTR)
005100030924
005200110103
005300031103
005400031103     D*-----------
005500031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005600031103     D*-----------
005700031103     D DSPPI           DS
005800120727     D  w§PPITIP                      2
005900080618     D  w§PPIKSU                      7  0
006000120808     D  w§PPITIP2                     2
006100120808     D  w§PPIENV                      1
006200031103
006300110928
006400110928     D*------------------
006500110928     D* LINKING A DEFINIZIONI ESTERNE
006600110928     D*------------------
006700110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006800110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006900110928
007000030924
007100030325
007200030924     C* MAIN/
007300121016     C*
007400121016     C* Caricamento tabelle
007500121016     C                   EXSR      cartab
007600041015     C*
007700041015     C* Esegue traduzione
007800991027     C                   EXSR      traduci
007900120809     C*
008000120809     C* Scarico i dati d output
008100120809     C                   EXSR      wrirec
008200110929     C*
008300921023     C                   SETON                                        LR
008400111121
008500111121
008600120712
008700030924
008800991027     C     traduci       BEGSR
008900120712     C*
009000120809     C                   setoff                                       40
009100120712     C                   setoff                                       50
009200120727     C                   setoff                                       51
009300121010     C                   setoff                                       52
009400120712     C*
009500120712     C* 1° => Record di file-header
009600120712     C                   clear                   TITV2L3DF
009700120712     C                   eval      F§REGTYPE  = 'F'
009800120712     C                   eval      F§FILETYPE = 'CDLCF'
009900120712     C                   eval      F§CARRCD   = 'BARTOLINI_TR'
010000120712     C                   eval      F§CRTDT    = %char(%dec(%date() : *ISO))
010100120712     C                   eval      F§CRTTM    = %editc(%dec(%time() : *ISO):'X')
010200120712     C                   eval      F§COLLECT  = 'DELI'
010300120712     C*
010400120809     C* Popolo la schiera degli output
010500120809     C                   eval      vawDTA = TITV2L3DF
010600121009     C                   eval      wTyRec = 'F'
010700120809     C                   if        not *in50
010800120809     C                   z-add     1             iOUT
010900130807     C                   eval      skOUT(iOUT)  = wTyRec
011000130807     C                   eval      skOUT1(iOUT) = %subst(vawDTA:001:40)
011100130807     C                   eval      skOUT2(iOUT) = %subst(vawDTA:041:40)
011200130807     C                   eval      skOUT3(iOUT) = %subst(vawDTA:081:40)
011300130807     C                   eval      skOUT4(iOUT) = %subst(vawDTA:121:40)
011400130807     C                   eval      skOUT5(iOUT) = %subst(vawDTA:161:40)
011500130807     C                   eval      skOUT6(iOUT) = %subst(vawDTA:201:33)
011600120809     C                   endif
011700120712     C*
011800120712     C* 2° => Record di shipment-header
011900120719     C*
012000120719     C* Inizializzo il contatore di spedizioni elaborate
012100130807     C                   z-add     0             skRMNidx          7 0
012200031103     C*
012300031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
012400031103     C* di traduzione (trattasi d "post-traduzione")
012500120427     C                   eval      wKstbKSU = w§PPIKSU
012600120427     C                   eval      wKstbFTR = *blanks
012700120427     C     KEYstb03P     setll     tistb03l
012800120427     C                   if        %equal(tistb03l)
012900120427     C     KEYstb03P     reade     tistb03l
013000991027     C*
013100031103     C* Ciclo x tuttii record del cliente da trasmettere
013200120427     C                   dow       not %eof(tistb03l)
013300080618     C*
013400120427     C                   seton                                        50
013500121010     C                   setoff                                       52
013600120808     C                   setoff                                       56
013700031103     C*
013800031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
013900031103     C* deve restituire al cliente nel file tradotto
014000120427     C                   if        stbFTR = *blanks                             * ridondante
014100110103     C*
014200120427     C                   setoff                                       50
014300121113     C*
014400121113     C* Considero UNICAMENTE le spedizioni aventi:
014500121113     C* - LNP = 089
014600121114     C**** - NRS = 03
014700121113     C* - RMN > 24.000.000
014800121114     C* - RMA = 'LOGHISTES SAP' or 'PRIVALIA_BCN'
014900121113     C                   if        stbLNP = 089      AND
015000121114     C***                             stbNRS = 03       AND
015100121114     C                             stbRMN > 24000000 AND
015200130527     C                             (stbRMA = 'LOGHISTES SAP'   OR
015300130527     C                              stbRMA = 'PRIVALIA_BCN'    OR
015400130527     C                              stbRMA = 'ECILOG'          OR
015500130527     C                              stbRMA = 'ITALIAN STOCK @' OR
015600130527     C                              stbRMA = 'IS_PRIVALIA_BCN')
015700120719     C*
015800120719     C* Verifico che nel medesimo "giro" (ovvero file) non sia presente più volte lo stesso RMN
015900120719     C     stbRMN        lookup    skRMN                                  55
016000120727     C                   if        %found
016100120727     C                   seton                                        51
016200121010     C                   seton                                        52
016300120727     C                   else
016400121012     C*
016500121012     C* Verifico immediatamente data/ora stato corrente => se "futura" skippo completamente
016600121012     C                   if        stbDAS*10000+stbORS >
016700121012     C                             %int((%dec(%date():*ISO)*1000000+
016800121012     C                             %dec(%time()))/100)
016900121012     C                   seton                                        52
017000121012     C                   else
017100120719     C*
017200120719     C* Salvo il RMN corrente
017300120719     C                   add       1             skRMNidx
017400120719     C                   eval      skRMN(skRMNidx) = stbRMN
017500120808     C*
017600120808     C* Inizio a comporre il bufffer d output "comune" (sia alle distributions che ai return)
017700120808     C                   clear                   TITV2L3DH
017800120808     C                   eval      H§REGTYPE  = 'H'
017900120808     C                   eval      H§ACTDT    = %editc(stbDAS:'X')
018000120808     C                   eval      H§ACTTM    = %trim(%editw(stbORS:'0    00'))
018100120808     C*
018200120808     C* Per stati provenientio dalle bolle ...
018300120808     C                   if        stbPRS <> '6'
018400031103     C*
018500031103     C* Innanzitutto chaino la bolla direttamente su TITAS
018600120427     C     KEYtas30P     chain     titas30c
018700120427     C                   if        %found(titas30c)
018800121016     C*
018900121016     C* Verifico se evento lasciato avviso
019000121019     C                   movel     *blanks       wTIPEVE           1
019100121016     C                   if        stbPRS = '2'
019200121016     C                   z-add     1             i                 3 0
019300121016     C     stbCOS        lookup    sk2Alavv(i)                            55
019400121016     C                   if        %found
019500121019     C                   movel     'A'           wTIPEVE
019600121016     C                   endif
019700121019     C                   z-add     1             ii                3 0
019800121019     C     stbCOS        lookup    sk2Agiac(ii)                           55
019900121019     C                   if        %found
020000121019     C                   movel     'G'           wTIPEVE
020100121019     C                   endif
020200121016     C                   endif
020300120427     C*
020400120808     C                   eval      H§CARRACC  = '0896769'
020500120808     C                   eval      H§CARRCUS  = '0896769'
020600120808     C                   eval      H§REF_1    = '00000'+%editc(stbRMN:'X')
020700120808     C                   eval      H§REF_2    = H§REF_1
020800121004     C                   eval      H§TRACKNR  = %editc(tasLNP:'X') +
020900121004     C                                          %editc(tasNRS:'X') +
021000121004     C                                          %editc(tasNSP:'X')
021100121004     C                   eval      H§EXPNR    = %editc(tasLNP:'X') +
021200121004     C                                          %editc(tasNRS:'X') +
021300121004     C                                          %editc(tasNSP:'X')
021400121004     C***                eval      H§TRACKNR  = stbRMA
021500121004     C***                eval      H§EXPNR    = stbRMA
021600120712     C                   eval      H§TOTPACK  = %editc(tasNCL:'X')
021700120712     C                   eval      H§TOTWT    = %trim(
021800120712     C                                  %editw(tasPKB:'0      . 00'))
021900120712     C                   eval      H§TOTVOL   = %trim(
022000120712     C                                  %editw(tasVLB:'000000000  .   '))
022100120808     C*
022200120427     C                   select
022300120427     C*
022400121107     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
022500121019     C***                eval      H§STS = '00'
022600121019     C***                eval      H§STS = '02'
022700121107     C                   eval      H§STS = '04'
022800121019     C***                seton                                        50
022900120712     C*
023000120712     C                   when      %subst(stbCOS:1:9) = 'COLNOTAFF'
023100120712     C                   eval      H§STS = '01'
023200120427     C*
023300121019     C***                when      %subst(stbCOS:1:9) = 'ARRPRICOL'
023400121019     C***                eval      H§STS = '02'
023500121019     C***                if        stbDAS = *zeros
023600121019     C***                seton                                        50
023700121019     C***                endif
023800120712     C*
023900121107     C                   when      %trim(stbCOS) = 'MIC'
024000121107     C                   eval      H§STS = '04'
024100121107     C*
024200121107     C                   when      %trim(stbCOS) = 'RIC'
024300121107     C                   eval      H§STS = '04'
024400121016     C*
024500121019     C                   when      wTIPEVE = 'A'
024600121016     C                   eval      H§STS = '04'
024700121019     C                   eval      H§INC = 'S40'
024800121019     C*
024900121019     C                   when      wTIPEVE = 'G' AND (
025000121019     C                             %trim(stbCOS) = '001' or
025100121019     C                             %trim(stbCOS) = '002' or
025200121019     C                             %trim(stbCOS) = '003' or
025300121019     C                             %trim(stbCOS) = '004' or
025400121019     C                             %trim(stbCOS) = '005' or
025500121019     C                             %trim(stbCOS) = '006' or
025600121019     C                             %trim(stbCOS) = '008' or
025700121019     C                             %trim(stbCOS) = '009' or
025800121019     C                             %trim(stbCOS) = '012' or
025900121019     C                             %trim(stbCOS) = '016' or
026000121019     C                             %trim(stbCOS) = '026' or
026100121019     C                             %trim(stbCOS) = '035' or
026200121019     C                             %trim(stbCOS) = '037'     )
026300121019     C                   eval      H§STS = '04'
026400121019     C                   eval      H§INC = 'S10'
026500121019     C*
026600121019     C                   when      wTIPEVE = 'G' AND (
026700121019     C                             %trim(stbCOS) = '019' or
026800121019     C                             %trim(stbCOS) = '021' or
026900121019     C                             %trim(stbCOS) = '022' or
027000121019     C                             %trim(stbCOS) = '028'     )
027100121019     C                   eval      H§STS = '04'
027200121019     C                   eval      H§INC = 'S30'
027300121016     C*
027400121016     C                   when      %subst(stbCOS:1:7) = 'GIACAPE'
027500121016     C                   eval      H§STS = '04'
027600121019     C                   eval      H§INC = 'S50'
027700120712     C*
027800120712     C                   when      %subst(stbCOS:1:4) = 'RESO'
027900120712     C                   eval      H§STS = '06'
028000120712     C*
028100120712     C                   when      %trim(stbCOS) = 'CONSOK'
028200120712     C                   eval      H§STS = '07'
028300120427     C*
028400120427     C                   other
028500120712     C                   eval      H§STS = '??'
028600120712     C                   seton                                        50
028700120712     C*
028800120427     C                   endsl
028900120808     C*
029000120808     C* Reperisco la chiave del ORM legato alla bolla corrente
029100120808     C                   clear                   dsbl4m
029200120808     C                   eval      ta4TRC = 'M'
029300120808     C     KEYta430C     chain     tita430c
029400120808     C                   if        %found(tita430C)
029500120808     C                   if        ta4NOT <> *blanks
029600120808     C                   eval      dsbl4m = ta4NOT
029700120808     C*
029800120808     C* Aggancio l'ORM
029900120808     C     KEYorm01C     chain     fnorm01l
030000120808     C                   if        %found(fnorm01l)
030100120808     C                   if        ormATB = *blanks
030200120808     C*
030300120808     C* X questo caso considero solamente gli ORM singoli e pervenuti da file
030400120813     C                   if        ormTOR = 'S' and ormTCO = 'F' and
030500120813     C                             ormRFA = stbRMA
030600120808     C                   seton                                        56
030700120808     C*
030800120808     C* Se trattasi di "return" verifico in base allo stato se ritornare o meno il dato al cliente
030900120808     C                   select
031000120808     C*
031100121019     C***                when      H§STS = '00'
031200121019     C***                seton                                        50
031300120808     C*
031400120808     C                   when      H§STS = '07'
031500120808     C                   eval      H§STS = '09'
031600121019     C*
031700121019     C                   other
031800121019     C                   seton                                        50
031900120808     C*
032000120808     C                   endsl
032100120808     C*
032200120808     C                   eval      H§REF_1    = '00000'+ormRFA
032300120808     C*
032400120808     C* Se no ambiente di test valorizzo anche il REF 2
032500120808     C                   if        w§PPIENV = *blanks
032600120808     C                   eval      H§REF_2    = ormNAM
032700120808     C                   endif
032800120808     C*
032900120808     C                   endif
033000120808     C                   endif
033100120808     C                   endif
033200120808     C*
033300120808     C                   endif
033400120808     C                   endif
033500120808     C*
033600120808     C                   else
033700120808     C                   seton                                        50
033800120808     C                   endif
033900120808     C*
034000120808     C                   endif
034100120808     C*
034200120808     C* Per stati provenineti dagli ORM ...
034300120808     C                   if        stbPRS = '6'
034400120808     C*
034500120808     C                   seton                                        50
034600120808     C                   clear                   dsbl4m
034700120808     C                   move      stbRMN        wKeyORM          14 0
034800120808     C                   movel     wKeyORM       dsbl4m
034900120808     C*
035000120808     C* Aggancio l'ORM
035100120808     C     KEYorm01C     chain     fnorm01l
035200120808     C                   if        %found(fnorm01l)
035300120808     C                   if        ormATB = *blanks
035400120808     C*
035500120808     C* X questo caso considero solamente gli ORM singoli e pervenuti da file
035600120808     C                   if        ormTOR = 'S' and ormTCO = 'F'
035700120808     C                   setoff                                       50
035800120808     C                   seton                                        56
035900120808     C*
036000120808     C                   eval      H§CARRACC  = '0894539'
036100120808     C                   eval      H§CARRCUS  = '0894539'
036200120808     C                   eval      H§REF_1    = '00000'+ormRFA
036300120809     C*
036400120809     C* Se no ambiente di test valorizzo anche il REF 2
036500120809     C                   if        w§PPIENV = *blanks
036600120809     C                   eval      H§REF_2    = ormNAM
036700120809     C                   endif
036800120809     C*
036900121004     C                   eval      H§TRACKNR  = *zeros
037000121004     C                   eval      H§EXPNR    = *zeros
037100121004     C***                eval      H§TRACKNR  = ormRFA
037200121004     C***                eval      H§EXPNR    = ormRFA
037300120808     C                   eval      H§TOTPACK  = %editc(ormNCL:'X')
037400120808     C                   eval      H§TOTWT    = %trim(
037500120808     C                                  %editw(ormPKG:'0      . 00'))
037600120808     C                   eval      H§TOTVOL   = %trim(
037700120808     C                                  %editw(ormVLM:'000000000  .   '))
037800120808     C*
037900120808     C* In caso di "return" ancora in fase "ORM" restituisco solamente le fasi :
038000120809     C* 000, 050, 999
038100120808     C                   select
038200120808     C*
038300120808     C                   when      stbCOS = 'O000'
038400121019     C***                eval      H§STS = '00'
038500121019     C                   eval      H§STS = '02'
038600120808     C*
038700121019     C***                when      stbCOS = 'O050'
038800121019     C***                eval      H§STS = '00'
038900121019     C***                eval      H§STS = '02'
039000120808     C*
039100120808     C                   when      stbCOS = 'O999'
039200120808     C                   eval      H§STS = '08'
039300120808     C*
039400120808     C                   other
039500120808     C                   seton                                        50
039600120808     C*
039700120808     C                   endsl
039800120808     C*
039900120808     C                   endif
040000120808     C                   endif
040100120808     C                   endif
040200120808     C*
040300120808     C                   endif
040400080618     C*
040500080618     C* Scarico il buffer d output
040600120712     C                   eval      vawDTA = TITV2L3DH
040700121009     C                   eval      wTyRec = 'H'
040800120809     C  N50              exsr      INSREC
040900120712     C*
041000120712     C* 3° => Record di shipment-pack (solo se record di testata ok)
041100120808     C                   if        not *in50 and not *in56
041200120712     C                   clear                   TITV2L3DL
041300120712     C                   eval      L§REGTYPE  = 'L'
041400120712     C                   eval      L§ACTDT    = H§ACTDT
041500120712     C                   eval      L§ACTTM    = H§ACTTM
041600120712     C                   eval      L§REF_1    = H§REF_1
041700120712     C                   eval      L§STS      = H§STS
041800120712     C                   eval      L§PACKID   = *all'0'
041900120712     C                   eval      L§PCKWT    = '000000.000'
042000120712     C                   eval      L§PCKVOL   = '0000000000.000'
042100120712     C                   eval      L§PCKNR    = *all'0'
042200120712     C*
042300120712     C* Scarico il buffer d output
042400120712     C                   eval      vawDTA = TITV2L3DL
042500121009     C                   eval      wTyRec = 'L'
042600120809     C  N50              exsr      INSREC
042700120712     C                   endif
042800121012     C*
042900121012     C                   endif
043000031103     C*
043100031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
043200120427     C                   eval      stbFTR = 'T'
043300121010     C  N52              update    tistb000
043400121113     C*
043500121113     C                   endif
043600120712     C*
043700120712     C                   endif
043800120719     C*
043900120719     C                   endif
044000991027     C*
044100120427     C     KEYstb03P     reade     tistb03l
044200120427     C                   setoff                                       50
044300120427     C                   enddo
044400120427     C                   endif
044500120727     C*
044600120727     C* Se "spia" accesa => inserisco un record "*VOID"
044700120727     C                   if        *in51
044800120727     C                   clear                   tivgd000
044900120808     C                   eval      vgdTIP = w§PPITIP
045000120727     C                   eval      vgdKSU = '0'+%editc(w§PPIKSU:'X')
045100120727     C                   eval      vgdTSC = 'WW'
045200120727     C                   eval      vgdDAT = %dec(%date() : *ISO)
045300120727     C                   eval      vgdPGM = 'TITV2L3R6'
045400120727     C                   eval      vgdDTA = '*VOID'
045500120727     C                   write     tivgd000
045600120727     C                   endif
045700991027     C*
045800120427     C                   eval      wrkesito = '0'
045900991027     C*
046000910830     C                   ENDSR
046100031103
046200031103
046300031103
046400120809     C     insrec        BEGSR
046500120808     C*
046600120808     C* A seconda che stia considerando "distributions" o "return" => gestisco diversamente
046700120808     C                   if        *in56
046800120808     C                   clear                   tivgd000
046900120808     C                   eval      vgdTIP = w§PPITIP2
047000120808     C                   eval      vgdKSU = '0'+%editc(w§PPIKSU:'X')
047100120808     C                   eval      vgdTSC = 'WW'
047200120808     C                   eval      vgdDAT = %dec(%date() : *ISO)
047300120808     C                   eval      vgdPGM = 'TITV2L3R6'
047400121009     C                   eval      vgdDTA = vawDTA
047500120808     C                   write     tivgd000
047600120808     C*
047700120808     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
047800120808     C                   else
047900110928     C*
048000120427     C* Considero solamente le bolle originali
048100111103     C                   movel     *blanks       wEsito1           1
048200111103     C*
048300111103     C* Chiamata metodo GetLblTyp
048400111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
048500111103     C                                                stbAAS
048600111103     C                                               :stbLNP
048700111103     C                                               :stbNRS
048800111103     C                                               :stbNSP
048900111103     C                                               :pOutLblTyp
049000111103     C                                               :pOutAnnoBO
049100111103     C                                               :pOutLineaParBO
049200111103     C                                               :pOutSerieBO
049300111103     C                                               :pOutNumSpedBO
049400111103     C                                               :pOutDcmBO
049500111103     C                                               :pOutCcaBO
049600111103     C                                               :pOutRblBO))
049700111103     C*
049800120427     C                   if        pOutLblTyp = 'O' and
049900120427     C                             not *IN50                                    * ridondante
050000120809     C*
050100120809     C* Popolo la schiera degli output
050200120809     C                   seton                                        40
050300120809     C                   add       1             iOUT
050400130807     C                   eval      skOUT(iOUT)  = wTyRec
050500130807     C                   eval      skOUT1(iOUT) = %subst(vawDTA:001:40)
050600130807     C                   eval      skOUT2(iOUT) = %subst(vawDTA:041:40)
050700130807     C                   eval      skOUT3(iOUT) = %subst(vawDTA:081:40)
050800130807     C                   eval      skOUT4(iOUT) = %subst(vawDTA:121:40)
050900130807     C                   eval      skOUT5(iOUT) = %subst(vawDTA:161:40)
051000130807     C                   eval      skOUT6(iOUT) = %subst(vawDTA:201:33)
051100120809     C*
051200120427     C                   endif
051300031103     C*
051400120808     C                   endif
051500120808     C*
051600031103     C                   ENDSR
051700120809
051800120809
051900120809
052000120809     C     wrirec        BEGSR
052100120809     C*
052200120809     C* Solo se presenti anche record d spedizione => scarico lo stack di buffer d output
052300120809     C                   if        *in40
052400120809     C                   eval      iOUT = 1
052500120809     C                   dow       iOUT <= %elem(skOUT) AND
052600120809     C                             skOUT(iOUT) <> *blanks
052700130807     C                   eval      wTyRec     = skOUT(iOUT)
052800130807     C                   eval      wBufferStd = skOUT1(iOUT) +
052900130807     C                                          skOUT2(iOUT) +
053000130807     C                                          skOUT3(iOUT) +
053100130807     C                                          skOUT4(iOUT) +
053200130807     C                                          skOUT5(iOUT) +
053300130807     C                                          skOUT6(iOUT)
053400121009     C*
053500121009     C                   select
053600121009     C                   when      wTyRec = 'F'
053700130807     C                   eval      vawDTA = wBufferStd
053800121009     C                   when      wTyRec = 'L'
053900130807     C                   eval      vawDTA = wBufferStd + wFillerL
054000121009     C                   when      wTyRec = 'H'
054100130807     C                   eval      vawDTA = wBufferStd + wFillerH
054200121009     C                   endsl
054300121009     C*
054400120809     C                   write     tivaw000
054500120809     C                   add       1             iOUT
054600120809     C                   enddo
054700120809     C                   endif
054800120809     C*
054900120809     C                   ENDSR
055000031103
055100121016
055200121016     C     cartab        BEGSR
055300121016     C*
055400121016     C* Carico causali evento divise x categoria (A=Lavv G=Giac)
055500121016     C                   clear                   sk2Alavv
055600121016     C                   z-add     *zeros        i
055700121019     C                   z-add     *zeros        ii
055800121016     C                   eval      tblKUT = 1
055900121016     C                   eval      tblCOD = '2A'
056000121016     C     KEYtab00P     setll     tabel00f
056100121016     C     KEYtab00P     reade     tabel00f
056200121016     C                   dow       not %eof(tabel00f)
056300121016     C                   movel(p)  tblUNI        ds2a
056400121016     C                   if        §2aFTC = 'A'
056500121016     C                   add       1             i
056600121016     C                   eval      sk2Alavv(i)  = tblKEY
056700121016     C                   endif
056800121019     C                   if        §2aFTC = 'G'
056900121019     C                   add       1             ii
057000121019     C                   eval      sk2Agiac(ii) = tblKEY
057100121019     C                   endif
057200121016     C     KEYtab00P     reade     tabel00f
057300121016     C                   enddo
057400121016     C*
057500121016     C                   ENDSR
057600121016
057700031103
057800991027
057900991027      /TITLE Operazioni iniziali.
058000991027     C     *inzsr        BEGSR
058100991027     C*
058200991027     C     *ENTRY        PLIST
058300031103     C                   parm                    prmppi
058400991027     C     wrkesito      parm      wrkesito      prmesito
058500031103     C*
058600031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
058700031103     C                   EVAL      DSPPI = prmppi
058800030709     C*
058900030709     C* Definizione chiavi
059000110927     C*
059100110103     C* Chiave su TISTB03L - Parziale
059200110103     C     KEYstb03P     KLIST
059300031104     C                   KFLD                    wKstbKSU
059400031103     C                   KFLD                    wKstbFTR
059500031103     C*
059600031103     C* Chiave su TITAS30C - Parziale
059700031103     C     KEYtas30P     KLIST
059800031103     C                   KFLD                    stbAAS
059900031103     C                   KFLD                    stbLNP
060000031103     C                   KFLD                    stbNRS
060100031103     C                   KFLD                    stbNSP
060200121016     C*
060300121016     C* Chiave su TABEL00F - Parziale
060400121016     C     KEYtab00P     KLIST
060500121016     C                   KFLD                    tblKUT
060600121016     C                   KFLD                    tblCOD
060700120808     C*
060800120808     C* Chiave su TITA430C - Completa
060900120808     C     KEYta430C     KLIST
061000120808     C                   KFLD                    stbAAS
061100120808     C                   KFLD                    stbLNP
061200120808     C                   KFLD                    stbNRS
061300120808     C                   KFLD                    stbNSP
061400120808     C                   KFLD                    ta4TRC
061500120808     C*
061600120808     C* Chiave su FNORM01L - Completa
061700120808     C     KEYorm01C     KLIST
061800120808     C                   KFLD                    §B4POE
061900120808     C                   KFLD                    §B4NSR
062000120808     C                   KFLD                    §B4NOR
062100120808     C                   KFLD                    §B4NRV
062200991027     C*
062300991027     C                   ENDSR
