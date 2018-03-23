000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND)
000300000000     H*PARMS CVTOPT(*DATETIME)
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*---------------------------------------------------------------*H
000600000000     F* NDCR82R   *      ESTRAZIONE ARCHIVI PER MOD. 770 GENERALE     *
000700000000     F*---------------------------------------------------------------*
000800020417     FND77007L  IF   E           K DISK
000900000000     FND77010F  O  A E             DISK
001000000000     F*-
001100000000     D F0D             S            200    DIM(9)
001200000000     D SCK             S             24    DIM(75)
001300000000     D CAM             S              1    DIM(16)
001400000000     D C15T            S             15    DIM(03)
001500000000     D*------------
001600000000     D DatMin          C                   CONST('0001-01-01')
001700000000     D*------------
001800000000     D KPJBA         E DS
001900000000     D*------------
002000020418     D GGDS          E DS                  EXTNAME(ND77000HDS) INZ
002100000000     D*-------------
002200000000     D ndcr80ds      E DS
002300000000     D*-------------
002400000000     D* Dati di ambiente ottenuti da XSOC
002500000000     D SOC001        E DS                  EXTNAME(XSOC001DS)
002600000000     D*-------------
002700000000     D* DS Interna per dati di output di XSOC
002800000000     D XSOCDS          DS          1000
002900000000     D*-------------
003000000000     D* Definizioni parametri driver soggetti
003100000000     D DVARIC          S              1
003200000000     D DVASOCIETA      S              3
003300000000     D DVAUNITA        S              8
003400000000     D DVADTRIF        S               D
003500000000     D DVASTRUTT       S             10
003600000000     D DVALENOUT       S              9B 0
003700000000     D DVASNATURA      S              1
003800000000     D DVACODICE       S              8
003900000000     D DVALINEAV       S              3
004000000000     D DVAFILIALE      S              3  0
004100000000     D DVASOGG         S              8
004200000000     D DVATPIND        S              2
004300000000     D DVACDIND        S              3
004400000000     D DVAOUTPUT       S           4000
004500000000     D DVAERRORE       S              1
004600000000     D*------------
004700000000     D* DS esterna per dati soggetto
004800000000     D DVASOG        E DS
004900000000     D*-------------
005000000000     D* DS esterna per decodifica conto
005100000000     D ND002DS       E DS
005200000000     D*-------------
005300000000     D* Definizioni parametri standard chiamate moduli
005400000000     D Esito           S              1
005500000000     D GesMsg          S              1
005600000000     D LenOut          S              9B 0
005700000000     D Struttura       S             10
005800000000     D*------------
005900000000     D N77CON          DS
006000000000     D  N77KCC                 1      6
006100000000     D  N77KSC                 7     14
006200000000     D*-------------
006300000000     D AMG             DS
006400000000     D  AMGAnno                1      4
006500000000     D  T1                     5      5
006600000000     D  AMGMese                6      7
006700000000     D  T2                     8      8
006800000000     D  AMGGiorno              9     10
006900000000     D*-------------
007000000000     D                 DS
007100000000     D  PEGIO                  1      2  0
007200000000     D  PEMES                  3      4  0
007300000000     D  PEANN                  5      8  0
007400000000     D  DATNAS                 1      8  0
007500000000     D                 DS
007600000000     D  QUADRO                 1      2
007700000000     D  RIGO                   3      5  0
007701150515R186 D  ID                     6      6
007703150515R186 D  COLO                   7      8  0
007800150515R186 X* COLO                   6      8  0
007900000000     D  CODICE                 1      8
008000000000     D                 DS
008100000000     D  CAMPO                  1     16
008200000000     D  C15I                  17     61
008300000000     D    C15                        15    Dim(03) Overlay(C15I)
008400000000     D  C60                    1     61
008500020417     D*-------------
008600130530R110 D CampoWrk        DS
008700130530 "   D CampoInt                1     14
008800130530 "   D CampoDec               15     16
008900130530 "   D*
009000130530 "   D CampoIntero     DS
009100130530 "   D  Int                    1     13
009200130530 "   D  virgola               14     14
009300130530 "   D  Dec                   15     16
009400130530R110 D*-------------
009500020417     D SavCdf          S                   Like(N77Cdf)
009600020423     D SavCde          S                   Like(N77Cde)
009700020417     D SavRCdf         S                   Like(N77RCdf)
009800020418     D*-------------
009900020418     D WrtAnag         S              1    Inz('0')
010000000000     C*----------------------------------------------------*
010100000000     C*- Operazioni iniziali e scrittura Angrafica Azienda
010200100512     C                   Exsr      INZVAR
010300000000     C*
010400130611R122 C**** Ex R110       Eval      TroncR80 = *Off                                                 C
010500130530R110 C*
010600000000     C* Posizionamento con
010700000000     C                   MOVEL     PER1R80       ANNO              4 0
010800000000     C                   Z-ADD     ANNO          N77ACF
010900020417     C                   MOVEL     'AU'          N77QUA
011000020417     C     KEY770        SETLL     ND77007L
011100020417     C     KEY770        READE     ND77007L                               21
011200100512     C  N21              Exsr      SalvaKey
011300000000     C*
011400100512     C                   DoW       *In21 = *off
011500020418     C*
011600020423     C* A rottura codice fiscale (o società fusione)...
011700100512     C                   If        N77Cdf <> SavCdf   Or
011800020423     C                             N77Cde <> SavCde   Or
011900020423     C                             N77RCdf <> SavRCdf
012000020418     C*   scarico il record
012100100512     C                   Exsr      WrSc
012200020418     C*   salvo la nuova chiave
012300100512     C                   Exsr      SalvaKey
012400020418     C*   operazioni iniziali nuovo percipiente
012500020418     C                   Move      *Off          WrtAnag
012600020419     C                   Movel     '        '    SCK(1)
012700020419     C                   Z-Add     0             Sc
012701150515R186 C                   Move      '0'           ScId
012800020419     C                   Z-Add     0             ScCol
012900020418     C                   Z-Add     0             ScRig
013000020418     C                   Add       1             ScMod
013100020418     C                   ENDIF
013200000000     C*
013300000000     C* Scrittura Anagrafiche e importi percipienti
013400100512     C                   Exsr      WRTAND
013500000000     C*
013600020417     C     KEY770        READE     ND77007L                               21
013700000000     C                   EndDo
013800000000     C*
013900000000     C     SC            CASGT     1             WRSC
014000000000     C                   ENDCS
014100000000     C*
014200000000     C     FINE          TAG
014300000000     C                   SETON                                        LR
014400020417     C*----------------------------------------------------*
014500020417     C* SALVATAGGIO CHIAVE PER ROTTURA
014600020417     C*----------------------------------------------------*
014700020417     C     SALVAKEY      BEGSR
014800020417     C*
014900020417     C                   Move      N77Cdf        SavCdf
015000020423     C                   Move      N77Cde        SavCde
015100020417     C                   Move      N77RCdf       SavRCdf
015200020417     C*
015300020417     C                   ENDSR
015400000000     C*----------------------------------------------------*
015500000000     C*-
015600000000     C* SCRITTURA RECORD FILE TRACCIATO MINISTERIALE
015700000000     C*-
015800000000     C*----------------------------------------------------*
015900000000     C     WRTAND        BEGSR
016000000000     C*
016100000000     C*              -------------------
016200020417     C*              SCRITTURA QUADRO AU
016300000000     C*              -------------------
016400000000     C*
016500020415     C                   MOVEL     'AU'          QUADRO
016600020418     C*
016700010403     C                   If        SCRIG         = 999
016800020418     C                   Z-Add     0             SCRIG
016900020418     C                   EndIf
017000020418     C*
017100020418     C* scrivo la parte anagrafica UNA SOLA VOLTA per ogni percipiente
017200020418     C*
017300020418     C                   If        WrtAnag       = *Off
017400020418     C*-------------------------
017500020418     C* inizio parte anagrafica
017600020418     C*-------------------------
017700020418     C*
017800020418     C                   Move      *On           WrtAnag
017900020418     C*
018000020418     C* solo all'inizio del record
018100020418     C                   If        SCRIG         = 0
018200020418     C* CODICE FISCALE DEL SOSTITUTO DI IMPOSTA
018300120517R102 C****               Z-ADD     1             SCCOL
018400120517R102 C                   Z-ADD     0             SCCOL
018401150515R186 C                   Movel     '0'           SCID
018500020418     C                   MOVEL     N77RCDF       C60
018600020418     c                   if        c60           <> *blanks
018700100512     C                   Exsr      WSC
018800020418     c                   endif
018900020418     C                   EndIf
019000150515R186 C* Vale per tutte le righe anagrafiche successive
019001150515R186 C                   Movel     'A'           SCID
019100020418     C* CODICE FISCALE
019200000000     C                   ADD       1             SCRIG
019300000000     C                   Z-ADD     1             SCCOL
019400000000     C                   MOVEL     N77CDF        C60
019500100512     c                   If        c60           <> *blanks
019600100512     C                   Exsr      WSC
019700100512     c                   EndIf
019800000000     C* RAGIONE SOCIALE - COGNOME
019900000000     C                   ADD       1             SCCOL
020000000000     C                   MOVEL     N77COG        C60
020100100512     C                   Exsr      WSC
020200000000     C* NOME
020300000000     C                   ADD       1             SCCOL
020400000000     C     N77PFG        IFEQ      'F'
020500000000     C                   MOVEL     N77NOM        C60
020600000000     C                   ENDIF
020700000000     c                   if        c60           <> *blanks
020800100512     C                   Exsr      WSC
020900000000     c                   endif
021000000000     C* SESSO
021100000000     C                   ADD       1             SCCOL
021200000000     C                   MOVEL     N77SES        C60
021300000000     c                   if        c60           <> *blanks
021400100512     C                   Exsr      WSC
021500000000     c                   endif
021600000000     C* DATA DI NASCITA
021700000000     C                   ADD       1             SCCOL
021800000000     C                   IF        N77PFG        = 'F'
021900000000     C                   IF        N77DNC        <> DatMin
022000000000     C                   MOVE      N77DNC        AMG
022100000000     C                   MOVE      AMGAnno       PEANN
022200000000     C                   MOVE      AMGMese       PEMES
022300000000     C                   MOVE      AMGGiorno     PEGIO
022400000000     C                   Else
022500000000     C                   MOVE      *zeros        Datnas
022600000000     C                   Endif
022700010619C1478c                   If        DatNas <> 0
022800000000     C                   MOVE      DATNAS        CAMPO
022900010619C1478C                   EndIf
023000000000     C                   ElSe
023100000000     C                   MOVE      *blanks       CAMPO
023200000000     C                   Endif
023300010417C1478c                   if        campo         <> *blanks  and
023400010417C1478c                             campo         <> *zeros
023500000000     c                   if        c60           <> *blanks
023600100512     C                   Exsr      WSC
023700000000     c                   endif
023800010417C1478c                   endif
023900000000     C* COMUNE O STATO ESTERO DI NASCITA
024000000000     C                   ADD       1             SCCOL
024100000000     C                   MOVEL     N77LNC        C60
024200000000     c                   if        c60           <> *blanks
024300100512     C                   Exsr      WSC
024400000000     c                   endif
024500000000     C* PROVINCIA DI NASCITA
024600000000     C                   ADD       1             SCCOL
024700000000     C                   MOVEL     N77PNC        C60
024800000000     c                   if        c60           <> *blanks
024900100512     C                   Exsr      WSC
025000000000     c                   endif
025001150515R186 C* CATEGORIE PARTICOLARI
025002150515  "  C                   ADD       1             SCCOL
025003150515  "  C                   MOVEL     N77CTGPRT     C60
025004150515  "  c                   if        c60           <> *blanks
025005150515  "  C                   Exsr      WSC
025006150515  "  c                   endif
025007150515  "  C* EVENTI ECCEZIONALI
025008150515  "  C                   ADD       1             SCCOL
025009150515  "  C                   MOVE      N77EVENTI     CAMPO
025010150515  "  c                   if        c60           <> *blanks
025011150515  "  C                   Exsr      WSC
025012150515R186 C                   endif
025100000000     C* COMUNE DOMICILIO FISCALE
025200150515R186 X****               ADD       1             SCCOL
025201150515R186 C                   Z-ADD     20            SCCOL
025300000000     C                   MOVEL     N77CIT        C60
025400000000     c                   if        c60           <> *blanks
025500100512     C                   Exsr      WSC
025600000000     c                   endif
025700010409     C* PROVINCIA DOMICILIO FISCALE
025800000000     C                   ADD       1             SCCOL
025900000000     C                   MOVEL     N77PRV        C60
026000000000     c                   if        c60           <> *blanks
026100100512     C                   Exsr      WSC
026200000000     c                   endif
026300150515R186 C**** ex C1478 * REGIONE
026400150515R186 C**** ex C1478       ADD       1             SCCOL
026500140509R148 C**** ex C1478      Z-add     N77Reg        IMPORT
026600140509R148 C**** ex C1478      MOVE      IMPORT        CAMPO
026700140509R148 C**** ex C1478      Exsr      ABLANK
026800150515R186 C**** ex R148       if        N77Reg        <> 0
026900150515  "  C**** ex R148       MOVE      N77Reg        CAMPO
027000150515  "  C**** ex C1478      if        c60           <> *blanks
027100150515  "  C**** ex C1478      Exsr      WSC
027200150515  "  C**** ex C1478      endif
027300150515R186 c**** ex R148       endif
027400140508R148 C* Comune
027500140508  "  C                   ADD       1             SCCOL
027600140508  "  C                   MOVEL     N77COM        C60
027700140508  "  c                   if        c60           <> *blanks
027800140508  "  C                   Exsr      WSC
027900140508R148 c                   endif
028000150515R186 C**** INDIRIZZO
028100150515  "  C****               ADD       1             SCCOL
028200150515  "  C****               MOVEL     N77VIA        C60
028300150515  "  C****               if        c60           <> *blanks
028400150515  "  C****               Exsr      WSC
028500150515  "  C****               endif
028600150515  "  C**** ex C2014 EREDE
028700150515  "  C**** ex C2014      ADD       1             SCCOL
028800150515  "  C**** ex C2014      If        N77Erede      <> *blanks
028900150515R186 C**** ex C2014      MOVE      '1'           Campo
029000080201C2078C****               Else
029100080201C2078C****               MOVE      '0'           Campo
029200080201C2078C****               EndiF
029300150515R186 C**** ex C2014      if        c60           <> *blanks
029400150515  "  C**** ex C2014      Exsr      WSC
029500150515  "  C**** ex C2014      endif
029600150515  "  C**** ex C2078      endif
029700150515  "  C**** ex C2014  EVENTI ECCEZIONALI
029800150515  "  C**** ex C2014        ADD       1             SCCOL
029900150515  "  C**** ex C2014        MOVE      N77EVENTI     CAMPO
030000150515  "  C**** ex C2014        if        c60           <> *blanks
030100150515  "  C**** ex C2014        Exsr      WSC
030200150515R186 C**** ex C2014        endif
030300000000     C* NON RESIDENTI
030400010403C1478C****               ADD       1             SCCOL
030500010403  "  C**** N77NUM        IFNE      *BLANK
030600010403  "  C****               MOVE      '1'           CAMPO
030700120502  "  C****               ELSE
030800120502  "  C****               MOVE      '0'           CAMPO
030900120502  "  C****               ENDIF
031000010403  "  C****               if        c60           <> *blanks
031100100512  "  C****               Exsr      WSC
031200010403  "  C****               endif
031300010403C1478C****               endif
031400010403     C* STATO ESTERO DI NASCITA
031500010403C1478C****               ADD       1             SCCOL
031600010403  "  C****               MOVEL     N77SER        C60
031700010403  "  c****               if        c60           <> *blanks
031800100512  "  C****               Exsr      WSC
031900010403C1478c****               endif
032000000000     C* CODICE DI IDENTIFICAZIONE FISCALE ESTERO
032100150515R186 X****               ADD       1             SCCOL
032101150515R186 C                   Z-ADD     40            SCCOL
032200000000     C                   MOVEL     N77CDE        C60
032300000000     c                   if        c60           <> *blanks
032400100512     C                   Exsr      WSC
032500000000     c                   endif
032600020418     C* LOCALITA' DI RESIDENZA ESTERA
032700020418     C                   ADD       1             SCCOL
032800020418     C                   MOVEL     N77RCIT       C60
032900020418     c                   if        c60           <> *blanks
033000100512     C                   Exsr      WSC
033100020418     c                   endif
033200020418     C* INDIRIZZO ESTERO
033300020418     C                   ADD       1             SCCOL
033400020419     C                   MOVEL     N77RVIA       C60
033500020418     c                   if        c60           <> *blanks
033600100512     C                   Exsr      WSC
033700020418     c                   endif
033800020418     C* COD. STATO ESTERO
033900020418     C                   ADD       1             SCCOL
034000020418     C                   MOVEL     N77CSE        CODEST            3
034100020418     C                   MOVE      CODEST        CAMPO
034200020418     c                   if        c60           <> *blanks
034300100512     C                   Exsr      WSC
034400020418     c                   endif
034500020419     C*
034600020419     C*-----------------------
034700020419     C* fine parte anagrafica
034800020419     C*-----------------------
034900020419     C*
035000020419     C                   ELSE
035100020418     C*
035200020419     C* se avevo già scritto la parte anagrafica, incremento
035300020419     C* il contatore di riga per gli altri dati
035400020419     C                   ADD       1             SCRIG
035500020419     C*
035600020418     C                   ENDIF
035601150515R186 C*
035602150515R186 C                   Movel     '0'           SCID
035700020418     C*
035800000000     C* CAUSALE
035900070420C2014X****               Z-ADD     16            SCCOL
036000140523R148 C**** ex C2014      Z-ADD     18            SCCOL
036100150515R186 C**** ex R148       Z-ADD     19            SCCOL
036101150515R186 C                   Z-ADD     1             SCCOL
036200000000     C                   MOVEL     N77C77        C60
036300000000     c                   if        c60           <> *blanks
036400100512     C                   Exsr      WSC
036500000000     c                   endif
036600010403C1478C* ANNO
036700030725C1478C                   ADD       1             SCCOL
036800030725D1407X*ex c1478          Z-add     N77Anno       IMPORT
036900030725  "  C                   Z-add     N77Anno       IMPORTZ
037000030725D1407C                   MOVE      IMPORTZ       CAMPO
037100100512C1478C                   Exsr      ABLANK
037200010403  "  c                   if        c60           <> *blanks
037300100512  "  C                   Exsr      WSC
037400010403C1478c                   endif
037500020514C1478C* ANTICIPAZIONE
037600020418  "  C                   ADD       1             SCCOL
037700020418  "  c                   If        N77Tit        <> *blanks
037800080201  "  C                   MOVE      '1'           Campo
037900080201  "  c                   if        c60           <> *blanks
038000100512  "  C                   Exsr      WSC
038100080201  "  c                   endif
038200080201C1478c                   endif
038300150515     C* AMMONTARE LORDO (CASELLA 21)
038400000000     C                   ADD       1             SCCOL
038500000000     C                   z-add     N77ITC        IMPORT
038600020612D1143C     TroncR80      IfEq      *On
038700020612  "  C                   Z-Add     IMPORT        IMPORTZ          13 0
038800020607  "  C                   MOVE      IMPORTZ       CAMPO
038900020607D1143C                   Else
039000000000     C                   MOVE      IMPORT        CAMPO
039100130530R110 C                   EXSR      WSCINT
039200020607D1143C                   EndIf
039300100512     C                   Exsr      ABLANK
039400000000     c                   if        c60           <> *blanks
039500100512     C                   Exsr      WSC
039600000000     c                   endif
039700150515     C*- Casella 22    = SOMME NON SOGGETTE A RITENUTA CONVENZIONALE
039800020514     C                   SETOFF                                       202122
039900020514     C     N77NUM        COMP      *BLANK                             2020
040000020514     C   20N77RAC        COMP      0                                      21
040100020514     C   20N77IMN        COMP      0                                  22
040200130219     C* SOMME NON SOGGETTE A RIT. PER REGIME CONVENZIONALE
040300000000     C                   Z-ADD     0             IMPORT
040400000000     C                   ADD       1             SCCOL
040500020514     C* STRANIERO SENZA RITENUTE E CON IMP. NON SOGGETTTO
040600000000     C   20
040700000000     CAN 21
040800000000     CAN 22N77IMN        ADD       N77SNS        IMPORT
040900020612D1143C     TroncR80      IfEq      *On
041000020612  "  C                   Z-Add     IMPORT        IMPORTZ
041100020607  "  C                   MOVE      IMPORTZ       CAMPO
041200020607D1143C                   Else
041300000000     C                   MOVE      IMPORT        CAMPO
041400130530R110 C                   EXSR      WSCINT
041500020607D1143C                   EndIf
041600100512     C                   Exsr      ABLANK
041700000000     c                   if        c60           <> *blanks
041800100512     C                   Exsr      WSC
041900000000     c                   endif
042000140508R110 C* CODICE
042100130219  "  C                   ADD       1             SCCOL
042200130219  "  c                   If        N77CODICE     <> *blanks
042300130219  "  C                   MOVE      N77CODICE     Campo
042400130219  "  c                   if        c60           <> *blanks
042500130219  "  C                   Exsr      WSC
042600130219  "  c                   EndIf
042700130219  "  c                   EndIf
042800130219R110 c*
042900150515     C* ALTRE SOMME NON SOGGETTE A RITENUTA (24)
043000020514     C                   Z-ADD     0             IMPORT
043100130219     C                   ADD       1             SCCOL
043200020514     C* ITALIA
043300020514     C  N20N77IMN        ADD       N77SNS        IMPORT           13 2
043400020514     C* STRANIERO SENZA RITENUTE SENZA IMP. NON SOGGETTO
043500020514     C   20
043600020514     CAN 21
043700020514     CANN22N77IMN        ADD       N77SNS        IMPORT
043800020514     C* STRANIERO CON RITENUTE
043900020514     C   20
044000020514     CANN21N77IMN        ADD       N77SNS        IMPORT
044100020612D1143C     TroncR80      IfEq      *On
044200020612  "  C                   Z-Add     IMPORT        IMPORTZ
044300020607  "  C                   MOVE      IMPORTZ       CAMPO
044400020607D1143C                   Else
044500020514     C                   MOVE      IMPORT        CAMPO
044600130530R110 C                   EXSR      WSCINT
044700020607D1143C                   EndIf
044800100512     C                   Exsr      ABLANK
044900020514     c                   if        c60           <> *blanks
045000100512     C                   Exsr      WSC
045100020514     c                   endif
045200130219     C* IMPONIBILE (25)
045300000000     C                   ADD       1             SCCOL
045400000000     C                   z-add     N77IMS        Import
045500020612D1143C     TroncR80      IfEq      *On
045600020612  "  C                   Z-Add     IMPORT        IMPORTZ
045700020607  "  C                   MOVE      IMPORTZ       CAMPO
045800020607D1143C                   Else
045900000000     C                   MOVE      Import        CAMPO
046000130530R110 C                   EXSR      WSCINT
046100020607D1143C                   EndIf
046200100512     C                   Exsr      ABLANK
046300000000     c                   if        c60           <> *blanks
046400100512     C                   Exsr      WSC
046500000000     c                   endif
046600130219C1770X*
046700130219C1770C* RITENUTE A TITOLO D'ACCONTO (26)
046800000000     C                   ADD       1             SCCOL
046900000000     C                   z-add     N77RAC        Import
047000020612D1143C     TroncR80      IfEq      *On
047100020612  "  C                   Z-Add     IMPORT        IMPORTZ
047200020607  "  C                   MOVE      IMPORTZ       CAMPO
047300020607D1143C                   Else
047400000000     C                   MOVE      Import        CAMPO
047500130530R110 C                   EXSR      WSCINT
047600020607D1143C                   EndIf
047700100512     C                   Exsr      ABLANK
047800000000     c                   if        c60           <> *blanks
047900100512     C                   Exsr      WSC
048000000000     c                   endif
048100040622C1770C*
048200130219  "  C* RITENUTE A TITOLO D'IMPOSTA (27)
048300040622  "  C                   ADD       1             SCCOL
048400040622  "  C                   Z-ADD     N77RIm        Import
048500040622  "  C*
048600040622  "  C                   if        TroncR80    = *On
048700040622  "  C                   Z-Add     Import        ImportZ
048800040622  "  C                   Move      ImportZ       Campo
048900040622  "  C                   else
049000130530C1770C                   Move      Import        Campo
049100130530R110 C                   EXSR      WSCINT
049200130530C1770C                   endif
049300040622  "  C*
049400100512  "  C                   Exsr      ABlank
049500040622  "  C                   if        C60        <> *Blanks
049600100512  "  C                   Exsr      WSC
049700040622  "  C                   endif
049800040622C1770C*
049900130219C1770C* RITENUTE SOSPESE (28)
050000000000     C                   ADD       1             SCCOL
050100000000     C                   z-add     N77NET        IMPORT
050200020612D1143C     TroncR80      IfEq      *On
050300020612  "  C                   Z-Add     IMPORT        IMPORTZ
050400020607  "  C                   MOVE      IMPORTZ       CAMPO
050500020607D1143C                   Else
050600000000     C                   MOVE      IMport        CAMPO
050700130530R110 C                   EXSR      WSCINT
050800020607D1143C                   EndIf
050900100512     C                   Exsr      ABLANK
051000000000     c                   if        c60           <> *blanks
051100100512     C                   Exsr      WSC
051200000000     c                   endif
051300040622C1770C*
051400130219C1770C* ADD.REGIONALE A TITOLO D'ACCONTO (29)
051500040622C1478C                   ADD       1             SCCOL
051600010403  "  C                   If        N77ARG   <> 0
051700020607C1478C                   z-add     N77ARG        IMPORT
051800020612D1143C     TroncR80      IfEq      *On
051900020612  "  C                   Z-Add     IMPORT        IMPORTZ
052000020607  "  C                   MOVE      IMPORTZ       CAMPO
052100020607D1143C                   Else
052200020607C1478C                   MOVE      IMport        CAMPO
052300130530R110 C                   EXSR      WSCINT
052400020607D1143C                   EndIf
052500100512C1478C                   Exsr      ABLANK
052600010403  "  c                   if        c60           <> *blanks
052700100512  "  C                   Exsr      WSC
052800010403  "  c                   endif
052900010403C1478c                   endif
053000040622C1770C*
053100130219  "  C* ADD.REGIONALE A TITOLO D'IMPOSTA (30)
053200040622  "  C                   ADD       1             SCCOL
053300040622  "  C                   Z-ADD     N77ARI        Import
053400040622  "  C*
053500040622  "  C                   if        TroncR80    = *On
053600040622  "  C                   Z-Add     Import        ImportZ
053700040622  "  C                   Move      ImportZ       Campo
053800040622  "  C                   else
053900130530C1770C                   Move      Import        Campo
054000130530R110 C                   EXSR      WSCINT
054100130530C1770C                   endif
054200040622  "  C*
054300100512  "  C                   Exsr      ABlank
054400040622  "  C                   if        C60        <> *Blanks
054500100512  "  C                   Exsr      WSC
054600040622  "  C                   endif
054700040622C1770C*
054800130219C1770C* ADD.REGIONALE SOSPESA (31)
054900040622C1478C                   ADD       1             SCCOL
055000010403  "  C                   If        N77ARS   <> 0
055100020607C1478C                   z-add     N77ARS        IMPORT
055200020612D1143C     TroncR80      IfEq      *On
055300020612  "  C                   Z-Add     IMPORT        IMPORTZ
055400020607  "  C                   MOVE      IMPORTZ       CAMPO
055500020607D1143C                   Else
055600020607C1478C                   MOVE      IMport        CAMPO
055700130530R110 C                   EXSR      WSCINT
055800020607D1143C                   EndIf
055900100512C1478C                   Exsr      ABLANK
056000010403  "  c                   if        c60           <> *blanks
056100100512  "  C                   Exsr      WSC
056200010403  "  c                   endif
056300010403C1478c                   endif
056400140508R148 C*
056500140508  "  C* Add.Comunale a titolo di acconto
056600140508  "  C                   ADD       1             SCCOL
056700140508  "  C                   If        N77ACTA  <> 0
056800140508  "  C                   z-add     N77ACTA       IMPORT
056900140508  "  C     TroncR80      IfEq      *On
057000140508  "  C                   Z-Add     IMPORT        IMPORTZ
057100140508  "  C                   MOVE      IMPORTZ       CAMPO
057200140508  "  C                   Else
057300140508  "  C                   MOVE      IMport        CAMPO
057400140508  "  C                   EXSR      WSCINT
057500140508  "  C                   EndIf
057600140508  "  C                   Exsr      ABLANK
057700140508  "  C                   if        c60           <> *blanks
057800140508  "  C                   Exsr      WSC
057900140508  "  C                   endif
058000140508  "  C                   endif
058100140508  "  C*
058200140508  "  C* Add.Comunale a titolo di imposta
058300140508  "  C                   ADD       1             SCCOL
058400140508  "  C                   If        N77ACG   <> 0
058500140508  "  C                   z-add     N77ACG        IMPORT
058600140508  "  C     TroncR80      IfEq      *On
058700140508  "  C                   Z-Add     IMPORT        IMPORTZ
058800140508  "  C                   MOVE      IMPORTZ       CAMPO
058900140508  "  C                   Else
059000140508  "  C                   MOVE      IMport        CAMPO
059100140508  "  C                   EXSR      WSCINT
059200140508  "  C                   EndIf
059300140508  "  C                   Exsr      ABLANK
059400140508  "  C                   if        c60           <> *blanks
059500140508  "  C                   Exsr      WSC
059600140508  "  C                   endif
059700140508  "  C                   endif
059800140508  "  C*
059900140508  "  C* Add.Comunale sospesa
060000140508  "  C                   ADD       1             SCCOL
060100140508  "  C                   If        N77ACS   <> 0
060200140508  "  C                   z-add     N77ACS        IMPORT
060300140508  "  C     TroncR80      IfEq      *On
060400140508  "  C                   Z-Add     IMPORT        IMPORTZ
060500140508  "  C                   MOVE      IMPORTZ       CAMPO
060600140508  "  C                   Else
060700140508  "  C                   MOVE      IMport        CAMPO
060800140508  "  C                   EXSR      WSCINT
060900140508  "  C                   EndIf
061000140508  "  C                   Exsr      ABLANK
061100140508  "  C                   if        c60           <> *blanks
061200140508  "  C                   Exsr      WSC
061300140508  "  C                   endif
061400140508  "  C                   endif
061500140508R148 C*
061600040622C1770C*
061700130219C1770C* IMPONIBILE ANNI PREC. (32)
061800040622C1478C                   ADD       1             SCCOL
061900010403  "  C                   If        N77IMSAP <> 0
062000020607C1478C                   z-add     N77IMSAP      IMPORT
062100020612D1143C     TroncR80      IfEq      *On
062200020612  "  C                   Z-Add     IMPORT        IMPORTZ
062300020607  "  C                   MOVE      IMPORTZ       CAMPO
062400020607D1143C                   Else
062500130611C1478C                   MOVE      IMPORT        CAMPO
062600130530R110 C                   EXSR      WSCINT
062700020607D1143C                   EndIf
062800100512C1478C                   Exsr      ABLANK
062900010403  "  c                   if        c60           <> *blanks
063000100512  "  C                   Exsr      WSC
063100010403  "  c                   endif
063200010403C1478c                   endif
063300040622C1770C*
063400130219C1770C* RITENUTE ANNI PREC. (33)
063500040622C1478C                   ADD       1             SCCOL
063600010403  "  C                   If        N77RACAP <> 0
063700020607C1478C                   z-add     N77RACAP      IMPORT
063800020612D1143C     TroncR80      IfEq      *On
063900020612  "  C                   Z-Add     IMPORT        IMPORTZ
064000020607  "  C                   MOVE      IMPORTZ       CAMPO
064100020607D1143C                   Else
064200130611C1478C                   MOVE      IMPORT        CAMPO
064300130530R110 C                   EXSR      WSCINT
064400020607D1143C                   EndIf
064500100512C1478C                   Exsr      ABLANK
064600010403  "  c                   if        c60           <> *blanks
064700100512  "  C                   Exsr      WSC
064800010403  "  c                   endif
064900010403C1478c                   endif
065000130219C1846C* CTB PREV CARICO DEL DATORE LAVORO - Soggetto Erogante (34)
065100050623  "  C                   ADD       1             SCCOL
065200050623  "  C                   If        N77CTBDL <> 0
065300050623  "  C                   z-add     N77CTBDL      IMPORT
065400050623  "  C     TroncR80      IfEq      *On
065500050623  "  C                   Z-Add     IMPORT        IMPORTZ
065600050623  "  C                   MOVE      IMPORTZ       CAMPO
065700050623  "  C                   Else
065800130611C1846C                   MOVE      IMPORT        CAMPO
065900130530R110 C                   EXSR      WSCINT
066000130530C1846C                   EndIf
066100100512  "  C                   Exsr      ABLANK
066200050623  "  c                   if        c60           <> *blanks
066300100512  "  C                   Exsr      WSC
066400050704  "  c                   endif
066500050704  "  c                   endif
066600130219  "  C* CTB PREV CARICO LAVORATORE - Percipiente (35)
066700050623  "  C                   ADD       1             SCCOL
066800050623  "  C                   If        N77CTBLV <> 0
066900050623  "  C                   z-add     N77CTBLV      IMPORT
067000050623  "  C     TroncR80      IfEq      *On
067100050623  "  C                   Z-Add     IMPORT        IMPORTZ
067200050623  "  C                   MOVE      IMPORTZ       CAMPO
067300050623  "  C                   Else
067400130611C1846C                   MOVE      IMPORT        CAMPO
067500130530R110 C                   EXSR      WSCINT
067600130530C1846C                   EndIf
067700100512  "  C                   Exsr      ABLANK
067800050623  "  c                   if        c60           <> *blanks
067900100512  "  C                   Exsr      WSC
068000050623  "  c                   endif
068100050623C1846c                   endif
068200130219C1943C* Spese rimborsate  (36)
068300060622  "  C                   ADD       1             SCCOL
068400060622  "  C                   If        N77SpeRimb <> 0
068500060622  "  C                   z-add     N77SpeRimb    IMPORT
068600060622  "  C     TroncR80      IfEq      *On
068700060622  "  C                   Z-Add     IMPORT        IMPORTZ
068800060622  "  C                   MOVE      IMPORTZ       CAMPO
068900060622  "  C                   Else
069000130611C1943C                   MOVE      IMPORT        CAMPO
069100130530R110 C                   EXSR      WSCINT
069200130530C1943C                   EndIf
069300100512  "  C                   Exsr      ABLANK
069400060622  "  c                   if        c60           <> *blanks
069500100512  "  C                   Exsr      WSC
069600060622  "  c                   endif
069700060622  "  c                   endif
069800130219  "  C* Ritenute rimborsate  (37)
069900060622  "  C                   ADD       1             SCCOL
070000060622  "  C                   If        N77RitRimb  <> 0
070100060622  "  C                   z-add     N77RitRimb    IMPORT
070200060622  "  C     TroncR80      IfEq      *On
070300060622  "  C                   Z-Add     IMPORT        IMPORTZ
070400060622  "  C                   MOVE      IMPORTZ       CAMPO
070500060622  "  C                   Else
070600130611C1943C                   MOVE      IMPORT        CAMPO
070700130530R110 C                   EXSR      WSCINT
070800130530C1943C                   EndIf
070900100512  "  C                   Exsr      ABLANK
071000060622  "  c                   if        c60           <> *blanks
071100100512  "  C                   Exsr      WSC
071200060622  "  c                   endif
071300060622C1943c                   endif
071400130219C2014C* Somme corrisposte prima della data fallimento (38)
071500070420  "  C                   ADD       1             SCCOL
071600070420  "  C                   If        N77ImpPrFa  <> 0
071700070420  "  C                   z-add     N77ImpPrFa    IMPORT
071800070420  "  C     TroncR80      IfEq      *On
071900070420  "  C                   Z-Add     IMPORT        IMPORTZ
072000070420  "  C                   MOVE      IMPORTZ       CAMPO
072100070420  "  C                   Else
072200130611C2014C                   MOVE      IMPORT        CAMPO
072300130530R110 C                   EXSR      WSCINT
072400130530C2014C                   EndIf
072500100512  "  C                   Exsr      ABLANK
072600070420  "  c                   if        c60           <> *blanks
072700100512  "  C                   Exsr      WSC
072800070420  "  c                   endif
072900070420  "  c                   endif
073000130219  "  C* Somme corrisposte dal curatore/commissario  (39)
073100070420  "  C                   ADD       1             SCCOL
073200070420  "  C                   If        N77ImpCuCo  <> 0
073300070420  "  C                   z-add     N77ImpCuCo    IMPORT
073400070420  "  C     TroncR80      IfEq      *On
073500070420  "  C                   Z-Add     IMPORT        IMPORTZ
073600070420  "  C                   MOVE      IMPORTZ       CAMPO
073700070420  "  C                   Else
073800130611C2014C                   MOVE      IMPORT        CAMPO
073900130530R110 C                   EXSR      WSCINT
074000130530C2014C                   EndIf
074100100512  "  C                   Exsr      ABLANK
074200070420  "  c                   if        c60           <> *blanks
074300100512  "  C                   Exsr      WSC
074400070420  "  c                   endif
074500070420C2014c                   endif
074600100517R024 C*------------------------------------------------------
074700100517R024 C* REDDITi EROGATI DA ALTRI SOGGETTI
074800100517R024 C*
074900110318R024 C* Totale reddito percepito                    (39)
075000110318R055 C*** ex R024        ADD       1             SCCOL
075100110318  "  C*** ex R024        If        N77ASIMP    <> 0
075200110318  "  C*** ex R024        z-add     N77ASIMP      IMPORT
075300110318  "  C*** ex R024   TroncR80      IfEq      *On
075400110318  "  C*** ex R024        Z-Add     IMPORT        IMPORTZ
075500110318  "  C*** ex R024        MOVE      IMPORTZ       CAMPO
075600110318  "  C*** ex R024        Else
075700110318  "  C*** ex R024        MOVE      Import        CAMPO
075800110318  "  C*** ex R024        EndIf
075900110318  "  C*** ex R024        Exsr      ABLANK
076000110318  "  c*** ex R024        if        c60           <> *blanks
076100110318  "  C*** ex R024        Exsr      WSC
076200110318  "  c*** ex R024        endif
076300110318  "  c*** ex R024        endif
076400130219R055 C* Causale (40)
076500150521R186 X**** Ex R055       ADD       1             SCCOL
076501150521R186 C                   Z-ADD     51            SCCOL
076600150521R055 C                   MOVEL     N77CauAs      C60
076700110318  "  c                   if        c60           <> *blanks
076800110318  "  C                   Exsr      WSC
076900110318R055 c                   EndIf
077000110318R024 C*
077100130219  "  C* Codice Fiscale (41)                         (40)
077200100517  "  C                   ADD       1             SCCOL
077300100517  "  C                   MOVEL     N77ASCDF      C60
077400100517  "  c                   if        c60           <> *blanks
077500100517  "  C                   Exsr      WSC
077600100517  "  c                   endif
077700100517  "  C*
077800130219  "  C* Imponibile (42)
077900100517  "  C                   ADD       1             SCCOL
078000100517  "  C                   If        N77ASIMS    <> 0
078100100517  "  C                   z-add     N77ASIMS      IMPORT
078200100517  "  C     TroncR80      IfEq      *On
078300100517  "  C                   Z-Add     IMPORT        IMPORTZ
078400100517  "  C                   MOVE      IMPORTZ       CAMPO
078500100517  "  C                   Else
078600130530R024 C                   MOVE      Import        CAMPO
078700130530R110 C                   EXSR      WSCINT
078800130530R024 C                   EndIf
078900100517  "  C                   Exsr      ABLANK
079000100517  "  c                   if        c60           <> *blanks
079100100517  "  C                   Exsr      WSC
079200100517  "  c                   endif
079300100517  "  c                   endif
079400100517  "  C*
079500130219  "  C* Ritenute a titolo di acconto (43)
079600100517  "  C                   ADD       1             SCCOL
079700100517  "  C                   If        N77ASRAC    <> 0
079800100517  "  C                   z-add     N77ASRAC      IMPORT
079900100517  "  C     TroncR80      IfEq      *On
080000100517  "  C                   Z-Add     IMPORT        IMPORTZ
080100100517  "  C                   MOVE      IMPORTZ       CAMPO
080200100517  "  C                   Else
080300130530R024 C                   MOVE      Import        CAMPO
080400130530R110 C                   EXSR      WSCINT
080500130530R024 C                   EndIf
080600100517  "  C                   Exsr      ABLANK
080700100517  "  c                   if        c60           <> *blanks
080800100517  "  C                   Exsr      WSC
080900100517  "  c                   endif
081000100517  "  c                   endif
081100100517  "  C*
081200130219  "  C* Ritenute a titolo d'imposta (44)
081300100517  "  C                   ADD       1             SCCOL
081400100517  "  C                   If        N77ASRIM    <> 0
081500100517  "  C                   z-add     N77ASRIM      IMPORT
081600100517  "  C     TroncR80      IfEq      *On
081700100517  "  C                   Z-Add     IMPORT        IMPORTZ
081800100517  "  C                   MOVE      IMPORTZ       CAMPO
081900100517  "  C                   Else
082000130530R024 C                   MOVE      Import        CAMPO
082100130530R110 C                   EXSR      WSCINT
082200130530R024 C                   EndIf
082300100517  "  C                   Exsr      ABLANK
082400100517  "  c                   if        c60           <> *blanks
082500100517  "  C                   Exsr      WSC
082600100517  "  c                   endif
082700100517  "  c                   endif
082800100517  "  C*
082900130219  "  C* Importo ritenute sospese (45)
083000100517  "  C                   ADD       1             SCCOL
083100100517  "  C                   If        N77ASRSO    <> 0
083200100517  "  C                   z-add     N77ASRSO      IMPORT
083300100517  "  C     TroncR80      IfEq      *On
083400100517  "  C                   Z-Add     IMPORT        IMPORTZ
083500100517  "  C                   MOVE      IMPORTZ       CAMPO
083600100517  "  C                   Else
083700130530R024 C                   MOVE      Import        CAMPO
083800130530R110 C                   EXSR      WSCINT
083900130530R024 C                   EndIf
084000100517  "  C                   Exsr      ABLANK
084100100517  "  c                   if        c60           <> *blanks
084200100517  "  C                   Exsr      WSC
084300100517  "  c                   endif
084400100517  "  c                   endif
084500100517  "  C*
084600130219  "  C* Add.Regionale a titolo di acconto (46)
084700100517  "  C                   ADD       1             SCCOL
084800100517  "  C                   If        N77ASARA    <> 0
084900100517  "  C                   z-add     N77ASARA      IMPORT
085000100517  "  C     TroncR80      IfEq      *On
085100100517  "  C                   Z-Add     IMPORT        IMPORTZ
085200100517  "  C                   MOVE      IMPORTZ       CAMPO
085300100517  "  C                   Else
085400130530R024 C                   MOVE      Import        CAMPO
085500130530R110 C                   EXSR      WSCINT
085600130530R024 C                   EndIf
085700100517  "  C                   Exsr      ABLANK
085800100517  "  c                   if        c60           <> *blanks
085900100517  "  C                   Exsr      WSC
086000100517  "  c                   endif
086100100517  "  c                   endif
086200100517  "  C*
086300130219  "  C* Add.Regionale a titolo d'imposta (47)
086400100517  "  C                   ADD       1             SCCOL
086500100517  "  C                   If        N77ASARI    <> 0
086600100517  "  C                   z-add     N77ASARI      IMPORT
086700100517  "  C     TroncR80      IfEq      *On
086800100517  "  C                   Z-Add     IMPORT        IMPORTZ
086900100517  "  C                   MOVE      IMPORTZ       CAMPO
087000100517  "  C                   Else
087100130530R024 C                   MOVE      Import        CAMPO
087200130530R110 C                   EXSR      WSCINT
087300130530R024 C                   EndIf
087400100517  "  C                   Exsr      ABLANK
087500100517  "  c                   if        c60           <> *blanks
087600100517  "  C                   Exsr      WSC
087700100517  "  c                   endif
087800100517  "  c                   endif
087900100517  "  C*
088000130219  "  C* Add.Regionale sospesa (48)
088100100517  "  C                   ADD       1             SCCOL
088200100517  "  C                   If        N77ASARS    <> 0
088300100517  "  C                   z-add     N77ASARS      IMPORT
088400100517  "  C     TroncR80      IfEq      *On
088500100517  "  C                   Z-Add     IMPORT        IMPORTZ
088600100517  "  C                   MOVE      IMPORTZ       CAMPO
088700100517  "  C                   Else
088800130530R024 C                   MOVE      Import        CAMPO
088900130530R110 C                   EXSR      WSCINT
089000130530R024 C                   EndIf
089100100517  "  C                   Exsr      ABLANK
089200100517  "  c                   if        c60           <> *blanks
089300100517  "  C                   Exsr      WSC
089400100517  "  c                   endif
089500100517R024 c                   endif
089600140508R148 C*
089700140508  "  C* Add.Comunale a titolo di acconto
089800140508  "  C                   ADD       1             SCCOL
089900140508  "  C                   If        N77ASACA <> 0
090000140508  "  C                   z-add     N77ASACA      IMPORT
090100140508  "  C     TroncR80      IfEq      *On
090200140508  "  C                   Z-Add     IMPORT        IMPORTZ
090300140508  "  C                   MOVE      IMPORTZ       CAMPO
090400140508  "  C                   Else
090500140508  "  C                   MOVE      IMport        CAMPO
090600140508  "  C                   EXSR      WSCINT
090700140508  "  C                   EndIf
090800140508  "  C                   Exsr      ABLANK
090900140508  "  C                   if        c60           <> *blanks
091000140508  "  C                   Exsr      WSC
091100140508  "  C                   endif
091200140508  "  C                   endif
091300140508  "  C*
091400140508  "  C* Add.Comunale a titolo di imposta
091500140508  "  C                   ADD       1             SCCOL
091600140508  "  C                   If        N77ASACI <> 0
091700140508  "  C                   z-add     N77ASACI      IMPORT
091800140508  "  C     TroncR80      IfEq      *On
091900140508  "  C                   Z-Add     IMPORT        IMPORTZ
092000140508  "  C                   MOVE      IMPORTZ       CAMPO
092100140508  "  C                   Else
092200140508  "  C                   MOVE      IMport        CAMPO
092300140508  "  C                   EXSR      WSCINT
092400140508  "  C                   EndIf
092500140508  "  C                   Exsr      ABLANK
092600140508  "  C                   if        c60           <> *blanks
092700140508  "  C                   Exsr      WSC
092800140508  "  C                   endif
092900140508  "  C                   endif
093000140508  "  C*
093100140508  "  C* Add.Comunale sospesa
093200140508  "  C                   ADD       1             SCCOL
093300140508  "  C                   If        N77ASACS <> 0
093400140508  "  C                   z-add     N77ASACS      IMPORT
093500140508  "  C     TroncR80      IfEq      *On
093600140508  "  C                   Z-Add     IMPORT        IMPORTZ
093700140508  "  C                   MOVE      IMPORTZ       CAMPO
093800140508  "  C                   Else
093900140508  "  C                   MOVE      IMport        CAMPO
094000140508  "  C                   EXSR      WSCINT
094100140508  "  C                   EndIf
094200140508  "  C                   Exsr      ABLANK
094300140508  "  C                   if        c60           <> *blanks
094400140508  "  C                   Exsr      WSC
094500140508  "  C                   endif
094600140508  "  C                   endif
094700140508R148 C*
094800020418     C*
094900010611nnn  C     SCk(1)        IFeq      *blanks
095000020418n    C                   Z-ADD     0             SCRIG
095100010611n    C                   ADD       1             SCMOD
095200020419n    C****               else
095300020419     C**** SCRIG         IFEQ      6
095400020419     C**** SCCOL         ANDEQ     28
095500100512     C****               Exsr      WRSC
095600020419     C****               Z-ADD     0             SCRIG
095700020419     C****               ADD       1             SCMOD
095800020419     C****               ENDIF
095900010611     C                   ENDIF
096000000000     C*
096100000000     C                   ENDSR
096200000000     C*----------------------------------------------------------------
096300000000     C*
096400000000     C* TOGLIE ZERI NON SIGNIFICATIVI DAL CAMPO
096500000000     C*
096600000000     C*----------------------------------------------------------------
096700000000     C     ABLANK        BEGSR
096800000000     C*
096900000000     C                   MOVEA     CAMPO         CAM
097000000000     C*
097100000000     C                   DO        16            X                 2 0
097200000000     C     CAM(X)        IFEQ      ' '
097300000000     C     CAM(X)        OREQ      '0'
097400000000     C                   MOVE      ' '           CAM(X)
097500000000     C                   ELSE
097600000000     C                   LEAVE
097700000000     C                   ENDIF
097800000000     C                   ENDDO
097900130530R110 C*
098000130530     C                   If        Cam(13)       = ' '  and
098100130530     C                             Cam(14)       = ','  and
098200130530     C                             Cam(15)       <>' '
098300130530     C                   Eval      Cam(13)       = '0'
098400130530R110 C                   EndIf
098500130530     C*
098600000000     C                   MOVEA     CAM           CAMPO
098700000000     C*
098800000000     C                   ENDSR
098900000000     C*----------------------------------------------------------------
099000000000     C*
099100020417     C* PREPARA SCHIERA QUADRO AU
099200000000     C*
099300000000     C*----------------------------------------------------------------
099400000000     C     WSC           BEGSR
099500000000     C*
099501150515R186 C                   MOVEL     SCID          ID
099600000000     C                   Z-ADD     SCCOL         COLO
099700000000     C                   Z-ADD     SCRIG         RIGO
099800000531     C                   ADD       1             SC
099900000000     C                   MOVEL     CODICE        SCK(SC)
100000000000     C                   MOVE      CAMPO         SCK(SC)
100100000000     C                   MOVEL     *BLANK        CAMPO
100200000000     C*
100300000000     C* SE RIEMPIO LA SCHIERA CREO RECORD
100400000531     C     SC            CASEQ     75            WRSC
100500000531     C                   ENDCS
100600000000     C*
100700000000     C* CONTROLLO SE CI SONO DATI DOPO LA 17° POSIZIONE
100800000000     C                   DO        3             XY                1 0
100900000000     C                   If        C15(XY) <> *blANKS
101000000000     C                   ADD       1             SC
101100000000     C                   MOVEL     CODICE        SCK(SC)
101200000000     C                   MOVEL     '+'           C16              16
101300000000     C                   MOVE      C15(XY)       C16
101400000000     C                   MOVE      C16           SCK(SC)
101500000000     C     SC            CASEQ     75            WRSC
101600000000     C                   ENDCS
101700000000     C                   ENDIF
101800000000     C                   ENDDO
101900000000     C*
102000000000     C                   Clear                   C60
102100000000     C*
102200000000     C* SE RAGGIUNGO IL NUMERO MAX DI RIGHE INCREMENTO MODULO
102300000000     C*
102400020419     C**** SCRIG         IFEQ      6
102500020419     C**** SCCOL         ANDEQ     28
102600100512     C****               Exsr      WRSC
102700020419     C****               Z-ADD     0             SCRIG
102800020419     C****               ADD       1             SCMOD
102900020419     C****               Z-add     0             SC
103000020419     C****               MOVEL     '        '    SCK(1)
103100020419     C****               ENDIF
103200000000     C*
103300000000     C                   ENDSR
103400130530R110 C*----------------------------------------------------------------
103500130530 "   C* Tratta campo importo
103600130530 "   C*----------------------------------------------------------------
103700130530 "   C     WSCINT        BEGSR
103800130530 "   C*
103900130530 "   C                   If        Import        <> 0
104000130530 "   C                   Move      Campo         CampoWRk
104100130530 "   C                   Move      CampoInt      Int
104200130530 "   C                   Move      ','           Virgola
104300130530 "   C                   Move      CampoDec      Dec
104400130530 "   C                   Move      CampoIntero   Campo
104500130530 "   C                   EndIf
104600130530 "   C*
104700130530R110 C                   ENDSR
104800000000     C*----------------------------------------------------------------
104900000000     C*
105000000000     C* SCRIVE RECORD SC
105100000000     C*
105200000000     C*----------------------------------------------------------------
105300000000     C     WRSC          BEGSR
105400000000     C*
105500020417     C                   MOVE      'H'           M710TR
105600000000     C                   MOVEL     XSCCDF        M710CF
105700000000     C                   Z-ADD     SCMOD         M710PM
105800020418     C                   MOVEL     SAVCDF        M710CP
105900130530     C                   MOVEA     SCK           F0D
106000000000     C                   MOVEA     F0D(1)        F01
106100000000     C                   MOVEA     F0D(2)        F02
106200000000     C                   MOVEA     F0D(3)        F03
106300000000     C                   MOVEA     F0D(4)        F04
106400000000     C                   MOVEA     F0D(5)        F05
106500000000     C                   MOVEA     F0D(6)        F06
106600000000     C                   MOVEA     F0D(7)        F07
106700000000     C                   MOVEA     F0D(8)        F08
106800000000     C                   MOVEA     F0D(9)        F09
106900000000     C                   MOVE      GGDS          M71REC
107000000000     C                   WRITE     M77010
107100000000     C                   Z-ADD     0             SC
107200000000     C                   CLEAR                   GGDS
107300000000     C                   CLEAR                   SCK
107400000000     C                   CLEAR                   F0D
107500000000     C                   CLEAR                   M71REC
107600000000     C*
107700000000     C                   ENDSR
107800000000     C*----------------------------------------------------*
107900000000     C* SUBROUTINE OPERAZIONI INIZIALI E SCRITTURA         *
108000000000     C* ARCHIVIO ANAGRAFICA AZIENDA                        *
108100000000     C*----------------------------------------------------*
108200000000     C     INZVAR        BEGSR
108300000000     C*-
108400000000     C     *ENTRY        PLIST
108500000000     C                   PARM                    KPJBA
108600000000     C*
108700000000     C                   movel     kpjbu         ndcr80ds
108800000000     C* --
108900020417     C     KEY770        KLIST
109000000000     C                   KFLD                    XSCSOC
109100000000     C                   KFLD                    N77ACF
109200020417     C                   KFLD                    N77QUA
109300000000     C*-
109400000000     C* Chiamata a xsoc
109500000000     C                   move      socR80        SOCXSC
109600000000     C                   MOVEL     'SOC001'      TIPXSC
109700000000     C                   CALLb     'XSOC'
109800000000     C                   PARM                    TIPXSC            6
109900000000     C                   PARM                    SOCXSC            3
110000000000     C                   PARM                    CDSXSC            9 0
110100000000     C                   PARM                    MODXSC            3
110200000000     C                   PARM      *BLANKS       RTNXSC            1
110300000000     C                   PARM                    XSOCDS
110400000000     C                   PARM                    KPJBA
110500000000     C     RTNXSC        IFNE      '1'
110600000000     C                   MOVEL     XSOCDS        SOC001
110700000000     C                   ELSE
110800000000     C                   GOTO      FINE
110900000000     C                   ENDIF
111000000000     C                   MOVE      *BLANK        CAMPO            16
111100000000     C                   MOVE      *BLANK        C60
111200000000     C                   MOVEL     '        '    SCK(1)
111300000000     C*
111400020417     C* CAMPI QUADRO AU
111500000000     C*
111600020418     C                   Z-ADD     0             SCRIG             3 0
111700150515R186 X**                 Z-ADD     0             SCCOL             3 0
111701150515R186 C                   Move      '0'           SCID              1
111702150515R186 C                   Z-ADD     0             SCCOL             2 0
111800000000     C                   Z-ADD     0             SC                3 0
111900000000     C                   Z-ADD     1             SCMOD             8 0
112000000000     C*
112100020419     C                   Move      *Off          WrtAnag
112200020419     C*
112300000000     C                   ENDSR
112400000000     C************************************************************
112500000000     C* REPERIMENTO DATI da ANPDC
112600000000     C************************************************************
112700000000     C     READANPDC     BEGSR
112800000000     C*
112900000000     C                   CLEAR                   ND002DS
113000000000     C                   EVAL      LenOut = %Size(ND002ds)
113100000000     C                   CallB     'NDMVC002'
113200000000     C                   PARM                    socR80
113300000000     C                   PARM                    N77KCC
113400000000     C                   PARM                    N77KSC
113500000000     C                   PARM                    *omit
113600000000     C                   PARM      *OFF          GesMsg
113700000000     C                   PARM      *ON           Esito
113800000000     C                   PARM      'ND002DS'     Struttura
113900000000     C                   PARM                    ND002DS
114000000000     C                   PARM                    LenOut
114100000000     C*
114200000000     C                   ENDSR
114300000000     C***************************************************************
114400000000     C* CONTROLLO SOGGETTI
114500000000     C***************************************************************
114600000000     C     CTRSOG        BEGSR
114700000000     C*
114800000000     C                   MOVE      '1'           DVARIC
114900000000     C                   MOVE      XSCSOC        DVASOCIETA
115000000000     C                   move      *blank        DVASTRUTT
115100000000     C                   MOVEL     'DVASOG'      DVASTRUTT
115200000000     C                   EVAL      DVALENOUT = %SIZE(DVASOG)
115300000000     C                   MOVE      sogg002       DVASOGG
115400000000     C                   CALLB     'NDDVASOG'
115500000000     C                   PARM                    DVARIC
115600000000     C                   PARM                    DVASOCIETA
115700000000     C                   PARM                    DVAUNITA
115800000000     C                   PARM                    DVASTRUTT
115900000000     C                   PARM                    DVADTRIF
116000000000     C                   PARM                    DVALENOUT
116100000000     C                   PARM                    DVASNATURA
116200000000     C                   PARM                    *omit
116300000000     C                   PARM                    DVALINEAV
116400000000     C                   PARM                    DVAFILIALE
116500000000     C                   PARM                    DVASOGG
116600000000     C                   PARM                    DVATPIND
116700000000     C                   PARM                    DVACDIND
116800000000     C                   PARM                    DVAERRORE
116900000000     C                   PARM                    DVAOUTPUT
117000000000     C     DVAERRORE     IFEQ      *OFF
117100000000     C                   EVAL      %SUBST(DVASOG:1:DVALENOUT)
117200000000     C                              = %SUBST(DVAOUTPUT:1:DVALENOUT)
117300000000     C                   ENDIF
117400000000     C*
117500000000     C                   ENDSR
