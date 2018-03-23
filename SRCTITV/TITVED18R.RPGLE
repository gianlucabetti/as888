000100160523     H DECEDIT('0.') DATEDIT(*DMY.)
000200160523     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400170217     FTIVGDTMP  IF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600160523
000700160523     D*-----------
000800160523     D* SCHIERE A PROGRAMMA
000900160523     D*-----------
001000160530     D wXML_ROWSHEAD   S             90    DIM(50) CTDATA PERRCD(1)
001100160523     D wXML_ROWSFOOT   S             90    DIM(10) CTDATA PERRCD(1)
001200991027
001300991027     D prmppt          s             50
001400991027     D prmesito        s              1
001500991027     D wrkesito        s                   like(prmesito)
001600160523     D i               s              3  0 inz
001700160523     D chKeyValDlm     s              1    inz('|')
001800160523     D chFmtDlm        s              1    inz(',')
001900170217     D wTFP            s              3    inz(*blanks)
002000160523     D wCPRVER         s              5    inz(*all'0')
002100160523     D wCPRDDEora      s             20    inz(*blanks)
002200160523     D wDatOraCor      s             20    inz(*blanks)
002300171017     D wProdName       s             25    inz(*blanks)
002400140708
002500160315     D*
002600160315     D DSCAPSIMPLE     DS                  qualified inz
002700160315     D  ORGFIL                        3  0
002800160315     D  CPRVER                        5  0
002900160315     D  CPRDDE                        8  0
003000160315     D  AMBITO                        5
003100160315     D  CPCNAR                        3
003200160315     D  CPCPRV                        2
003300160315     D  CPCCAP                        9
003400160315     D  CPLLOC                       37
003500160315     D  CPCLNA                        3  0
003600160315     D  CPCZNC                        2  0
003700160315     D  CAETFE                        3  0
003800170217     D  CAETFP                        3
003900170217     D*
004000170217     D SAVCAPSIMPLE    S             72    inz
004100160523
004200160523
004300160523     D*-------------------
004400160523     D* Variabili d wrk x la chiamata al *pgm esterno d "bonifica" dati
004500160523     D*-------------------
004600160523     D TxtInOut        S           2048
004700160523     D ElencoChar      S            256
004800160523     D TipoElenco      S              1
004900160523     D CharSost        S              1
005000160523     D UpperCase       S              1
005100160523     D ChkNull         S              1
005200160523     D CharNull        S              1
005300160523     D Esito           S              1
005400140708
005500160523
005600160523     D*------------------
005700160523     D* LINKING A DEFINIZIONI ESTERNE
005800160523     D*------------------
005900160523     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
006000160523     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
006100160523
006200140708
006300991027
006400060804     C                   exsr      traduci
006500060804     C                   seton                                        LR
006600140708
006700991027
006800060804
006900991027     C     traduci       BEGSR
007000991027     C*
007100160315     C* Verifico i parametri di personalizzazinoe formato output
007200160523     C                   if        %subst(prmppt:1:1) <> *blanks
007300160523     C                   eval      chKeyValDlm = %subst(prmppt:1:1)
007400160523     C                   endif
007500160523     C                   if        %subst(prmppt:2:1) <> *blanks
007600160523     C                   eval      chFmtDlm = %trim(%subst(prmppt:2:1))
007700160315     C                   endif
007800160523     C                   if        %subst(prmppt:3:1)  = 'S'
007900160523     C                   seton                                        55
008000160523     C                   else
008100160523     C                   setoff                                       55
008200160523     C                   endif
008300170217     C                   if        %subst(prmppt:4:3) <> *blanks
008400170217     C                   eval      wTFP = %subst(prmppt:4:3)
008500170217     C                   endif
008600171017     C                   select
008700171017     C                   when      %subst(prmppt:7:1) = '1'
008800171017     C                   eval      wProdName = 'AmazonSimplyRouteSTD'
008900171017     C                   when      %subst(prmppt:7:1) = '2'
009000171017     C                   eval      wProdName = 'AmazonSimplyRouteVRET'
009100171017     C                   other
009200171017     C                   eval      wProdName = 'AmazonSimplyRouteSTD'
009300171017     C                   endsl
009400160315     C*
009500160523     C* Prima lettura fuori ciclo
009600060804     C                   read      TIVGDTMP
009700160523     C                   eval      DSCAPSIMPLE = vgdDTA
009800160523     C*
009900171017     C* Scrivo le righe di intestazione-infrastruttura xml
010000160523     C                   exsr      wriHeader
010100160523     C*
010200160523     C* Scorro tutto il cappario e lo converto in XML
010300991027     C                   dow       not %eof
010400160523     C*
010500160523     C* Scrivo le righe di dettglio dati
010600160523     C                   exsr      wriBody
010700121023     C*
010800160523     C                   read      TIVGDTMP
010900160523     C                   enddo
011000160523     C*
011100171017     C* Scrivo le righe di piede-infrastruttura xml
011200160523     C                   exsr      wriFooter
011300991027     C*
011400991027     C                   eval      wrkesito = '0'
011500991027     C*
011600910830     C                   ENDSR
011700160523
011800160523
011900160523
012000160523     C     wriHeader     BEGSR
012100160523     C*
012200160523     C* Compongo le variabili in sostituzione delle costanti figurative
012300160523     C                   eval      wCPRVER = %editc(DSCAPSIMPLE.CPRVER:'X')
012400160523     C                   eval      wCPRDDEora =
012500160523     C                              UBFMTDATE_Convert(
012600160523     C                              %editc(DSCAPSIMPLE.CPRDDE:'X')+'000000' :
012700160523     C                              'YYYYMMDDHHMISS' : 'YYYY-MM-DDTHH:MI:SSZ')
012800160523     C*
012900160523     C                   eval      i = 1
013000160523     C                   dow       i < %elem(wXML_ROWSHEAD)
013100160523     C*
013200160523     C                   if        wXML_ROWSHEAD(i) <> *blanks
013300160523     C*
013400160523     C* Effettuo le sostituzioni delle costanti figurative
013500160523     C                   eval      vawDTA = wXML_ROWSHEAD(i)
013600160523     C                   eval      vawDTA =
013700160523     C                               %scanrpl('§CPRVER§':wCPRVER:vawDTA)
013800160523     C                   eval      vawDTA =
013900160523     C                               %scanrpl('§DATORACOR§':wDatOraCor:vawDTA)
014000160523     C                   eval      vawDTA =
014100160523     C                               %scanrpl('§CPRDDEORA§':wCPRDDEora:vawDTA)
014200171017     C                   eval      vawDTA =
014300171018     C                               %scanrpl('§PRODNAME§':
014400171018     C                                        %trim(wProdName):vawDTA)
014500160523     C*
014600160523     C                   write     TIVAW000
014700160523     C                   endif
014800160523     C*
014900160523     C                   eval      i = i + 1
015000160523     C                   enddo
015100160523     C*
015200160523     C                   ENDSR
015300160523
015400160523
015500160523
015600160523     C     wriFooter     BEGSR
015700160523     C*
015800160523     C                   eval      i = 1
015900160523     C                   dow       i < %elem(wXML_ROWSFOOT)
016000160523     C*
016100160523     C                   if        wXML_ROWSFOOT(i) <> *blanks
016200160523     C                   eval      vawDTA = wXML_ROWSFOOT(i)
016300160523     C                   write     TIVAW000
016400160523     C                   endif
016500160523     C*
016600160523     C                   eval      i = i + 1
016700160523     C                   enddo
016800160523     C*
016900160523     C                   ENDSR
017000160523
017100160523
017200160523
017300160523     C     wriBody       BEGSR
017400160523     C*
017500160523     C* Inizializzazione campi e variabili d out
017600160523     C                   clear                   tivaw000
017700160523     C*
017800160523     C                   eval      DSCAPSIMPLE = vgdDTA
017900170217     C*
018000170217     C* Eseguo verifica record corrente
018100170217     C                   exsr      chkRec
018200170217     C                   if        not *in31
018300160530     C*
018400160530     C* Forzo 'IT' per indicare Italia
018500160530     C                   if        DSCAPSIMPLE.CPCNAR = *blanks
018600160530     C                   eval      DSCAPSIMPLE.CPCNAR = 'IT'
018700160530     C                   endif
018800160523     C*
018900160523     C* Se richiesto nei parametri di input =>
019000160523     C* ... bonifico il buffer d output da caratteri NON consentiti
019100160523     C                   if        *in55
019200160523     C                   eval      TxtInOut = DSCAPSIMPLE.CPLLOC
019300160530     C                   exsr      exeBon
019400160523     C                   eval      DSCAPSIMPLE.CPLLOC = TxtInOut
019500160523     C                   endif
019600160523     C*
019700160523     C                   eval      vawDTA =
019800160523     C                             '<Entry>'+
019900160523     C                             %trim(DSCAPSIMPLE.CPCNAR)          +chFmtDlm+
020000160523     C                             %trim(DSCAPSIMPLE.CPCCAP)          +chFmtDlm+
020100160530     C                             %trim(DSCAPSIMPLE.CPLLOC)
020200160530     C                             +chKeyValDlm+
020300160523     C                             %editc(DSCAPSIMPLE.CAETFE:'X')     +chFmtDlm+
020400160523     C                             %editc(DSCAPSIMPLE.CPCLNA:'X')     +chFmtDlm+
020500160530     C                             %editc(DSCAPSIMPLE.CPCZNC:'X')     +chFmtDlm+
020600160530     C                             %editc(DSCAPSIMPLE.ORGFIL:'X')
020700160523     C                             +'</Entry>'
020800160523     C*
020900160530     C                   write     TIVAW000
021000160530     C*
021100160530     C                   endif
021200160523     C*
021300160523     C                   ENDSR
021400170217
021500170217
021600170217
021700170217      /TITLE Verifica validità recoord corrente
021800170217     C     chkRec        BEGSR
021900170217     C*
022000170217     C                   setoff                                       31
022100170217     C*
022200170217     C* Se richiesto nei parametri specifico Termina di Partenza verifico e filtro
022300170217     C                   if        not *in31 AND wTFP <> *blanks
022400170217     C                   if        DSCAPSIMPLE.CAETFP = wTFP OR
022500170217     C                             DSCAPSIMPLE.CAETFP = *zeros
022600170217     C                   else
022700170217     C                   seton                                        31
022800170217     C                   endif
022900170217     C                   endif
023000170217     C*
023100170217     C* Ritorno il record di cappario solamente se la località è valorizzata
023200170217     C                   if        not *in31
023300170217     C                   if        DSCAPSIMPLE.CPLLOC <> *blanks
023400170217     C                   else
023500170217     C                   seton                                        31
023600170217     C                   endif
023700170217     C                   endif
023800170217     C*
023900170217     C* Verifico rottura di codice a livello di località
024000170217     C                   if        not *in31
024100170217     C                   if        %subst(DSCAPSIMPLE:1:%size(SAVCAPSIMPLE)) <>
024200170217     C                             SAVCAPSIMPLE
024300170217     C                   eval      SAVCAPSIMPLE =
024400170217     C                             %subst(DSCAPSIMPLE:1:%size(SAVCAPSIMPLE))
024500170217     C                   else
024600170217     C                   seton                                        31
024700170217     C                   endif
024800170217     C                   endif
024900170217     C*
025000170217     C                   ENDSR
025100160523
025200160523
025300160523
025400160523      /TITLE "Bonifica" dati
025500160523     C     exeBon        BEGSR
025600160523     C*
025700160523     C* Imposto i parametri x il *pgm d "bonifica"
025800160523     C                   EVAL      ElencoChar = 'QWERTYUIOPASDFGHJKLZXCVBNMqwe'+
025900160523     C                                          'rtyuiopasdfghjklzxcvbnm'      +
026000160523     C                                          '1234567890.-_''"'
026100160523     C                   EVAL      TipoElenco = *blanks
026200160523     C                   EVAL      CharSost   = *blanks
026300160523     C                   EVAL      UpperCase  = *blanks
026400160523     C                   EVAL      ChkNull    = '1'
026500160523     C                   EVAL      CharNull   = *blanks
026600160523     C                   EVAL      Esito      = *blanks
026700160523     C*
026800160523     C* Effettuo la chiamata al *pgm d "bonifica"
026900160523     C                   CALL      'XCHKCHAR'
027000160523     C                   PARM                    TxtInOut
027100160523     C                   PARM                    ElencoChar
027200160523     C                   PARM                    TipoElenco
027300160523     C                   PARM                    CharSost
027400160523     C                   PARM                    UpperCase
027500160523     C                   PARM                    ChkNull
027600160523     C                   PARM                    CharNull
027700160523     C                   PARM                    Esito
027800160523     C*
027900160523     C                   ENDSR
028000060804
028100160523
028200991027
028300991027      /TITLE Operazioni iniziali.
028400991027     C     *inzsr        BEGSR
028500991027     C*
028600991027     C     *ENTRY        PLIST
028700991027     C                   parm                    prmppt
028800991027     C     wrkesito      parm      wrkesito      prmesito
028900160523     C*
029000160523     C* Reperisco la data/ora corrente
029100160523     C                   eval      wDatOraCor =
029200160523     C                              UBFMTDATE_Convert(
029300160523     C                              %subst(%trim(%char(
029400160523     C                                     %dec(%timestamp:*ISO))):1:14) :
029500160523     C                              'YYYYMMDDHHMISS' : 'YYYY-MM-DDTHH:MI:SSZ')
029600991027     C*
029700991027     C                   ENDSR
029800160523** wXML_ROWSHEAD
029900160523<?xml version="1.0" encoding="UTF-8" standalone="no"?>                                     1
030000160523<RoutingUpdate>                                                                            2
030100160523<CarrierName>BRT</CarrierName>                                                             3
030200160523<UpdateParameters>                                                                         4
030300160523<Version>§CPRVER§</Version>                                                                5
030400160523<CreationDate>§DATORACOR§</CreationDate>                                                   6
030500160523<EffectiveDate>§CPRDDEORA§</EffectiveDate>                                                 7
030600160523<ExpiryDate>2099-12-31T23:59:59Z</ExpiryDate>                                              8
030700160523</UpdateParameters>                                                                        9
030800160523<RoutingSpecification>                                                                    10
030900171017<ProductName>§PRODNAME§</ProductName>                                                     11
031000160523<FormatDefinition delimiter="|">                                                          12
031100160523<KeyDefinition delimiter=",">                                                             13
031200160530<Component>                                                                               14
031300160530<Name>BRTCountryCode</Name>                                                               15
031400160530<Type>destCountryCode</Type>                                                              16
031500160530</Component>                                                                              17
031600160530<Component>                                                                               18
031700160530<Name>PostalCode</Name>                                                                   19
031800160531<Type>destPostalCode</Type>                                                               20
031900160530</Component>                                                                              21
032000160530<Component>                                                                               22
032100160530<Name>Town</Name>                                                                         23
032200160530<Type>destCity</Type>                                                                     24
032300160530</Component>                                                                              25
032400160530</KeyDefinition>                                                                          26
032500160530<ValueDefinition delimiter=",">                                                           27
032600160530<Component>                                                                               28
032700160530<Name>ArrivalHubID</Name>                                                                 29
032800160530<Type>ThreeDigitAlphaNumeric</Type>                                                       30
032900160530</Component>                                                                              31
033000160530<Component>                                                                               32
033100160530<Name>ArrivalDepotID</Name>                                                               33
033200160530<Type>ThreeDigitAlphaNumeric</Type>                                                       34
033300160530</Component>                                                                              35
033400160530<Component>                                                                               36
033500160530<Name>ArrivalDeliveryZone</Name>                                                          37
033600160530<Type>TwoDigitAlphaNumeric</Type>                                                         38
033700160530</Component>                                                                              39
033800160530<Component>                                                                               40
033900160530<Name>DepartureHubID</Name>                                                               41
034000160530<Type>ThreeDigitAlphaNumeric</Type>                                                       42
034100160530</Component>                                                                              43
034200160530</ValueDefinition>                                                                        44
034300160530</FormatDefinition>                                                                       45
034400160530<RoutingData>                                                                             46
034500160530<Complete>                                                                                47
034600160530                                                                                          48
034700160530                                                                                          49
034800160530                                                                                          50
034900160523** wXML_ROWSFOOT
035000160614<Entry>DEFAULT|789,789,00,166</Entry>                                                      1
035100160527</Complete>                                                                                2
035200160527</RoutingData>                                                                             3
035300160527</RoutingSpecification>                                                                    4
035400160527</RoutingUpdate>                                                                           5
035500160527                                                                                           6
035600160527                                                                                           7
035700160523                                                                                           8
035800160523                                                                                           9
035900160523                                                                                          10
