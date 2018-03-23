000100000000     H*PARMS DFTACTGRP(*NO)
000200150108     H*PARMS BNDDIR(PJXBND PJCBND)
000300000000     H*PARMS ACTGRP(QILE)
000400000000     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000500000000     H*PARMS RDB(*NONE) DYNUSRPRF(*OWNER)
000600000000     H*****************************************************************
000700000000     H DECEDIT('0,') DATEDIT(*DMY.)
000800000000     H*****************************************************************
000900000000     H*
001000141230      *                Estrazione dichiarazioni d'intento
001100141230      *                     fornitori
001200000000     H*           --------------------------------------
001300150105      *
001400150105      *   FLUSSO PRINCIPALE
001500150105      *
001600150105      *  - lettura delle dichiarazioni d'intento inviate ai fornitori
001700150105      *    in ordine di data di protocollo interno
001800150105      *
001900150105      *  - selezione delle dichiarazioni che hanno:
002000150105      *     1 - data di protocollo interno relativa all'anno
002100150105      *         indicato a video
002200150105      *
002300150105      *  - memorizzo movimento di dichiarazione nel file SDGDIE00F
002400150105      *
002500150105      *  - lettura nuovo record dichiarazione d'intento
002600150105      *
002700150105      *  - fine file dichiarazioni VAI A FINE
002800000000     H*
002900000000     F*****************************************************************
003000000000     F*------------
003100000000     F* Dati anagrafici aggiuntivi
003201160530R224 X**** Key: Sogg/DtFiVl/Sys/NrAsInt      UNIQUE
003202160530R224 F* Key: Sogg/DtFiVl/Sys/NrAsInt/Dest      UNIQUE
003300000000     FSDGSO301L IF   E           K DISK
003400150112     F*------------
003500150112     F* Dich.Int.- Estensione
003600150112     F* Key: Soc/Cf/DtInter/NrInter       UNIQUE
003700150112     FSDGDIF01L IF   E           K DISK
003800130826     F*------------
003900130826     F* Movimenti estratti
004000150108     F* Key: Soc/DtInter/NrInter
004100150108     FSDGDIE01L IF   E           K DISK    USROPN
004200150108     F                                     RENAME(SDGDIE000:SDGDIE01)
004300150108     F*------------
004400150108     F* Movimenti estratti
004500150108     FSDGDIE00F UF A E             DISK    USROPN
004600000000     F*------------
004700000000     F* Segnalazioni - Errori e avvertimenti
004800141230     FSDGSJ52P1 O    E             PRINTER  OFLIND(*In41)
004900000000     F*-------------
005000000000     F* Avvertimenti per dati fiscali non impostati
005100141230     FSDGSJ52P3 O    E             PRINTER  OFLIND(*In43)
005200000000     D*****************************************************************
005300000000     D* Passaggio Parametri
005400000000     D KPJBA         E DS
005500000000     D*-------------------------------------------------------*
005600000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
005700150105     D    SocSogg                     8    overlay(XSCFGO:31)
005800000000     D*-------------
005900000000     D* DS Interna per dati di output di SDGSOCDR
006000000000     D XSOCDS          DS          1000
006100000000     D*-------------
006200000000     D* DS Esterna gestione messaggi
006300000000     D SDGMSGDS      E DS                  INZ
006400000000     D  STS          E                     EXTFLD(MSGSTS)
006500000000     D                                     DIM(333)
006600000000     D  JBA          E                     EXTFLD(MSGJBA)
006700000000     D                                     DIM(502)
006701160530R224 D*----------------
006702160530 "   D* Valori assunti
006703160530 "   D XMPEDS        E DS                  INZ
006704160530 "   D XMPEDS1       E DS                  INZ
006705160530 "   D*-------------
006706160530 "   D DSDatiAna       DS                  INZ
006712160530R224 D $Dest                          2A
006800000000     D*-------------------------------------------------------*
006900000000     D* Reperimento nome PGM
007000000000     D STATUS         SDS           333
007100000000     D  DSPGM            *Proc
007200000000     D  Utente               254    263
007300000000     D*-------------------------------------------------------*
007400000000     D* Parametri che ricevo dal lancio
007500141230     D SDGSJ52DS     E DS
007600141230     D  ERRJ52       E                     INZ('0')
007700000000     D*-------------------------------------------------------*
007800000000     D* Ds x fetch
007900150105     D ANDICDS       E DS                  Occurs(1000) Extname(ANDIC00F)
008000000000     D*----------
008100000000     D StringaSql      DS          5000
008200141230     D SqlDIE          S           5000
008300141230     D SqlDIC          S           5000
008400000000     D*----------
008500000000     D* Indice DsMultipla
008600000000     D $Ind            S              4  0
008700000000     D*---------
008800141230     D* per prtf SDGSJ52P1 - errori
008900000000     D $TpErr1         S              2A   Inz
009000000000     D*------------------
009100000000     D* Controlli validità legende
009200000000     D X07ValDS      E DS
009300000000     D*-------------
009400130903     D* Tabella tipologie natura giuridica
009500130903     D ANGG16DS      E DS
009600150113     D*-------------
009700150113     D* Descrizione operazione per dich.intento
009800150113     D ANGG17DS      E DS
009900000000     D*---------
010000150105     D ANDIC00F      E DS                  INZ
010100000000     D                                     Prefix(IV2:3)
010200000000     D*---------
010300000000     D* Definizioni parametri standard chiamate moduli
010400000000     D $Esito          S              1
010500000000     D $GesMsg         S              1
010600000000     D $LenOut         S              9B 0
010700000000     D $Struttura      S             10
010800000000     D $CodOper        S              1
010900000000     D $Output         S           4000
011000000000     D $Refresh        S              1
011100000000     D $Uso            S              1
011200130918     D WStato          S                   Like(XScSta )
011300000000     D*---------
011400000000     D* dati conto
011500000000     D ND002DS       E DS
011600000000     D*---------
011700000000     D* Variazioni estremi fiscali
011800000000     D ANCPI00F      E DS
011900000000     D*-------------
012000000000     D* Driver estremi fiscali (ANCPI00F)
012100000000     D DVPRic          S              1
012200000000     D DVPSocieta      S              3
012300000000     D DVPUnita        S              8
012400000000     D DVPStrutt       S             10
012500000000     D DVPDtRif        S               D
012600000000     D DVPLenOut       S              9B 0
012700000000     D DVPSogg         S              8
012800000000     D DVPErrore       S              1
012900000000     D DVPOutput       S           4000
013000000000     D*---------
013100000000     D* Dati soggetti
013200000000     D DVASog        E DS
013300000000     D* Dati fornitori
013400000000     D DVAFor        E DS
013500000000     D*---------
013600000000     D* driver soggetti
013700000000     D DVARic          S              1
013800000000     D DVASocieta      S              3
013900000000     D DVAUnita        S              8
014000000000     D DVADtRif        S               D
014100000000     D DVAStrutt       S             10
014200000000     D DVALenOut       S              9B 0
014300000000     D DVASnatura      S              1
014400000000     D DVACodice       S              8
014500000000     D DVALineav       S              3
014600000000     D DVAFiliale      S              3  0
014700000000     D DVASogg         S              8
014800000000     D DVATpInd        S              2
014900000000     D DVACdInd        S              3
015000000000     D DVAOutput       S           4000
015100000000     D DVAErrore       S              1
015200000000     D*---------
015300000000     D* Richiamo a SDGTABDR
015400000000     D SDGTABDS      E DS
015500000000     D*---------
015600000000     D* Contabilità
015700000000     D ANGG84DS      E DS                  INZ
015800130926     D*-------------
015900130926     D* Passaggio Parametri al PGM di recupero dati
016000130926     D* anagrafici
016100150105     D**** SDGSJ53DS     E DS                  INZ
016200000000     D*---------
016300000000     D* Campi di lavoro
016400141230     D $ExistDIE       S              1
016500141230     D $WriteDIE       S              1
016600141230     D $AnnullaDIE     S              1
016700130826     D $PrimaStp1      S              1    INZ(*On)
016800000000     D $PrimaStp3      S              1    INZ(*On)
016900000000     D $OkConto        S              1    INZ('1')
017000150113     D $OkDIC          S              1    INZ('1')
017100000000     D $AnnConto       S              1    INZ('0')
017200141230     D $AnnDIC         S              1    INZ('0')
017300141230     D $TpAnn          S                   Like(DIETpAnn) INZ(*Blank)
017400130903     D $NatGiur        S                   Like(DVFNatGiur)
017500000000     D*---------
017600000000     D* reperimento dati fiscali anagrafici
017700141230     D SAVKsc          S                   Like(DICKsc     )
017800141230     D SAVDtInter      S                   Like(DICDtInter)
017900000000     D*---------
018000000000     D* segnalazione  dati fiscali mancanti
018100141230     D SAVKsc2         S                   Like(DICKsc    )
018200141230     D SAVDtInte2      S                   Like(DICDtInter)
018300000000     D*---------
018400141230     D $TpNSog         S                   Like(DIETpN  )
018500141230     D $CdFiscSog      S                   Like(DIECdFisc )
018600141230     D $PartIVASog     S                   Like(DIEPartIVA)
018700141230     D $CogRagSog      S                   Like(DIECogRag)
018800141230     D $NomeSog        S                   Like(DIENome )
018900150108     D $SexSog         S                   Like(DIESex  )
019000000000     D*------
019100141230     D $TpN            S                   Like(DIETpN  )
019200141230     D $CdFisc         S                   Like(DIECdFisc )
019300141230     D $PartIVA        S                   Like(DIEPartIVA)
019400141230     D $CogRag         S                   Like(DIECogRag)
019500141230     D $Nome           S                   Like(DIENome )
019600150108     D $Sex            S                   Like(DIESex  )
019700000000     D*-------------
019800000000     D WSogg           S                   Like(DVASogg)
019900000000     D WKsc            S                   Like(DVACodice)
020000150108     D WKcc            S              6A
020100000000     D WTpInd          S                   Like(DVATpInd)
020200000000     D WCdInd          S                   Like(DVACdInd)
020300000000     D WSNatura        S                   Like(DVASNatura)
020400141230     D $FineDIC        S              1    Inz(*Off)
020500141230     D $FineDIE        S              1    Inz(*Off)
020600000000     D $FineScan       S              1    Inz(*Off)
020700000000     D $CampoScan      S             50A
020800000000     D*---------
020900000000     D DataDs          DS
021000000000     D  AnnoDs                        4A
021100000000     D   Tratt1                       1A   Inz('-')
021200000000     D  MeseDs                        2A
021300000000     D   Tratt2                       1A   Inz('-')
021400000000     D  GiornoDs                      2A
021500000000     D*----------
021600000000     D $Data10A1       S             10
021700000000     D $Data10A2       S             10
021800111018     D $DataAnno       S               D
021900000000     D $Data           S               D
022000000000     D $Anno           S              4A
022100000000     D*-------------
022200000000     D UDateISO        S               D    datfmt(*iso)
022300150120      *--------------------------------------------------------------------
022400150120      *  - Codice: codice in cui eliminare gli zeri non significativi a sx
022500150120      *  - Len: lunghezza effettiva del codice
022600150120      *--------------------------------------------------------------------
022700150120     D SkCod           S              1A   DIM(6)
022800150120     D WCod            S              6A
022900150120     D WNrInter        S                   Like(DIENrInter)
023000000000     C******************************************************
023100000000     C*                MAIN LINE PROGRAM
023200000000     C******************************************************
023300000000     C*
023400000000     C                   EXSR      INZVAR
023500000000     C*
023600141230     C                   EXSR      CicloDIC
023700000000     C*
023800000000     C                   EXSR      ENDPGM
023900110902     C*
024000000000     C************************************************************
024100000000     C* Fine programma
024200000000     C************************************************************
024300000000     C     ENDPGM        BEGSR
024400000000     C*
024500000000     C                   EXSR      CloseSql
024600000000     C*
024700141230     C                   MoveL     SDGSJ52DS     KPJBU
024800000000     C*
024900141230     C                   if        %open (SDGSJ52P1)
025000141230     C                   Close     SDGSJ52P1
025100110720     C                   endif
025200000000     C                   Eval       *InLR      = *On
025300000000     C*
025400000000     C                   Return
025500000000     C*
025600000000     C                   ENDSR
025700000000     C/EJECT
025800000000     C************************************************************
025900150105     C* Ciclo di lettura di ANDIC00F
026000000000     C************************************************************
026100141230     C     CicloDIC      BEGSR
026200000000     C*
026300141230     C                   EXSR      RepSqlDIC
026400000000     C*
026500141230     C                   EXSR      OpenSqlDIC
026600000000     C*
026700141230     C                   EXSR      ExeSqlDIC
026800000000     C*
026900000000     C                   ENDSR
027000000000     C/EJECT
027100000000     C************************************************************
027200150105     C* Reperimento stringa sql su ANDIC00F
027300000000     C************************************************************
027400141230     C     RepSqlDIC     BEGSR
027500000000     C*
027600150105     C* SELECT * FROM ANDIC00F
027700141230     C* WHERE DICSOCIETA='XXX'
027800141230     C*  AND DICCF    = 'F'
027801170825R246 C*  AND (DICANNO = ANNOJ52
027802170825R246 C*      OR (DICANNO = 0
027900141230     C*  AND DICDTINTER BETWEEN 'ANNOJ52-01-01' AND 'ANNOJ52-12-31'
027901170825R246 C*      )  )
028000150113     C*
028100000000     C* -------------------------------------------------------------------
028200150113     C*  [se fornitore impostato]
028300150113     C*  and DICKsc   = KscJ52
028400150113     C*
028500150113     C* -------------------------------------------------------------------
028600150113     C*  [se tipo operazione impostato]
028700150113     C*  and DICTpOpe = ??????
028800150113     C*
028900150113     C* -------------------------------------------------------------------
029000150113     C*  [se numero interno  impostato]
029100150113     C*  AND DICNRINTER BETWEEN NrInte1J52 and NRInte2J52
029200150113     C*
029300000000     C* -------------------------------------------------------------------
029400141230     C* ORDER BY DICKSC, DICDTINTER, DICNRINTER
029500141230     C* FOR FETCH ONLY
029600000000     C*
029700141230     C                   Clear                   SqlDIC
029800000000     C*
029900141230     C                   Eval      SqlDIC =
030000150105     C                              'SELECT * FROM ANDIC00F ' +
030100141230     C                             ' WHERE DICSOCIETA=' + '''' +
030200141230     C                             SocietaJ52 + ''''
030300150126R163 C*
030400150126 "   C                   Eval      SqlDIC      = %trimr(SqlDIC)+
030500150126 "   C                             ' AND DICCf    =' + '''' +
030600150126R163 C                             'F'    + ''''
030700000000     C*
030701170825R246 C*  AND (DICANNO = ANNOJ52
030702170825 "   C*      OR (DICANNO = 0
030703170825 "   C*  AND (DICDTINTER BETWEEN 'ANNOJ52-01-01' AND 'ANNOJ52-12-31')
030704170825 "   C*      )  )
030705170825 "   C                   Move(P)   AnnoJ52       WAnnoAlfa         4
030706170825 "   C                   Eval      SqlDIC      = %trimr(SqlDIC) +
030707170825 "   C                             ' AND (DICAnno=' + '''' +
030708170825 "   C                             WAnnoAlfa + ''''
030709170825 "   C*
030710170825 "   C                   Move(P)   0             WAnnoAlfa         4
030711170825 "   C                   Eval      SqlDIC      = %trimr(SqlDIC) +
030712170825 "   C                             '  OR (DICAnno=' + '''' +
030713170825 "   C                             WAnnoAlfa + ''''
030714170825R246 C*
030800000000     C                   EXSR      RepPeriodo
030900000000     C*
031000141230     C                   Eval      SqlDIC      = %trimr(SqlDIC) +
031100141230     C                             ' AND (DICDtInter'
031200000000     C*
031300141230     C                   Eval      SqlDIC      = %trimr(SqlDIC) +
031400000000     C                             ' BETWEEN ''' + $Data10A1 +
031500000000     C                             ''' AND ''' + $Data10A2 + ''''
031600000000     C                             + ' ) '
031601170825R246 C*
031602170825 "   C                   Eval      SqlDIC      = %trimr(SqlDIC) +
031603170825R246 C                             ' )  ) '
031700150113     C*
031800150113     C                   if        KscJ52     <> *Blank
031900150113     C                   Eval      SqlDIC      = %trimr(SqlDIC)+
032000150113     C                             ' AND DICKsc   =' + '''' +
032100150113     C                             KscJ52 + ''''
032200150113     C                   endif
032300150113     C*
032400150113     C                   Select
032500150113     C* operazione specifica
032600150113     C                   When      TpOpeJ52    = '1'
032700150113     C                   Eval      SqlDIC      = %trimr(SqlDIC)+
032800150113     C                             ' AND DICOpera =' + '''' +
032900150113     C                             '1'    + ''''
033000150113     C* operazione fino a importo
033100150113     C                   When      TpOpeJ52    = '2'
033101170825R246 C                   Eval      SqlDIC      = %trimr(SqlDIC)+
033102170825 "   C                             ' AND DICOpera <>' + '''' +
033103170825R246 C                             '1'    + ''''
033200150113     C                   Eval      SqlDIC      = %trimr(SqlDIC)+
033300150113     C                             ' AND DICAnno <>' +
033400150113     C                             '0'
033500150113     C                   Eval      SqlDIC      = %trimr(SqlDIC)+
033600150113     C                             ' AND DICImporto <>' +
033700150113     C                             '0'
033800150113     C* operazioni nel periodo
033900150113     C                   When      TpOpeJ52    = '3'
034000150113     C                   Eval      $Data10A1   = '0001-01-01'
034100150113     C                   Eval      SqlDIC      = %trimr(SqlDIC) +
034200150113     C                             ' AND DICDtInDIC >' + '''' +
034300150113     C                             $Data10A1 + ''''
034400150113     C                   endSl
034500150113     C*
034600150113     C* and DICNrInter >= NrInte1J52
034700150120     C                   Move(P)   NrInte1J52    WNrAlfa           6
034800150113     C                   CAT       'and':1       SQLDIC
034900150113     C                   CAT       'DICNrInter':1SQLDIC
035000150113     C                   CAT       '>=':0        SQLDIC
035100150113     C                   CAT       WNrAlfa:0     SQLDIC
035200150113     C*
035300150113     C* and DICNrInter <= NrInte2J52
035400150113     C                   if        NrInte2J52 <> 0
035500150120     C                   Move(P)   NrInte2J52    WNrAlfa
035600150113     C                   CAT       'and':1       SQLDIC
035700150113     C                   CAT       'DICNrInter':1SQLDIC
035800150113     C                   CAT       '<=':0        SQLDIC
035900150113     C                   CAT       WNrAlfa:0     SQLDIC
036000150113     C                   endif
036100000000     C*
036200141230     C                   Eval      SqlDIC      = %trimr(SqlDIC)+
036300141230     C                               ' ORDER BY DICKSC, '+
036400141230     C                               ' DICDTINTER, DICNRINTER'
036500000000     C*
036600000000     C* For Fetch Only
036700141230     C                   Eval      SqlDIC      = %trimr(SqlDIC)+
036800000000     C                               ' FOR FETCH ONLY'
036900000000     C*
037000000000     C                   ENDSR
037100000000     C/EJECT
037200000000     C******************************************************
037300000000     C* Reperisco le date di inizio e fine periodo
037400000000     C******************************************************
037500000000     C     RepPeriodo    BEGSR
037600000000     C*
037700000000     C                   Reset                   DataDs
037800000000     C                   Clear                   $Data10A1
037900000000     C*
038000111018     C* limite "da"
038100141230     C                   MoveL     AnnoJ52       AnnoDs
038200000000     C                   MoveL     '01'          MeseDs
038300000000     C                   MoveL     '01'          GiornoDs
038400000000     C                   Move      DataDs        $Data10A1
038500000000     C*
038600111018     C* limite "a"
038700000000     C                   Reset                   DataDs
038800000000     C                   Clear                   $Data10A2
038900111018     C*
039000141230     C                   MoveL     AnnoJ52       AnnoDs
039100130821     C                   MoveL     '12'          MeseDs
039200130821     C                   MoveL     '31'          GiornoDs
039300130821     C                   Move      DataDs        $Data10A2
039400000000     C*
039500111018     C* fine anno
039600111018     C                   Reset                   DataDs
039700111018     C                   Clear                   $DataAnno
039800111018     C*
039900141230     C                   MoveL     AnnoJ52       AnnoDs
040000111018     C                   MoveL     '12'          MeseDs
040100111018     C                   MoveL     '31'          GiornoDs
040200111018     C                   Move      DataDs        $DataAnno
040300111018     C*
040400000000     C                   ENDSR
040500000000     C/EJECT
040600000000     C******************************************************
040700000000     C* Open cursore sql
040800000000     C******************************************************
040900141230     C     OpenSqlDIC    BEGSR
041000000000     C*
041100000000     C/EXEC SQL
041200141230     C+     PREPARE S1 FROM :SqlDIC
041300000000     C/END-EXEC
041400000000     C*
041500000000     C/EXEC SQL
041600000000     C+     Declare A1 cursor for S1
041700000000     C/END-EXEC
041800000000     C*
041900000000     C/EXEC SQL
042000000000     C+     OPEN A1
042100000000     C/END-EXEC
042200000000     C*
042300000000     C                   ENDSR
042400000000     C/EJECT
042500000000     C************************************************************
042600000000     C* Eseguo lettura dei movimenti IVA
042700000000     C************************************************************
042800141230     C     ExeSqlDIC     BEGSR
042900000000     C*
043000141230     C                   Move      *Off          $FineDIC
043100000000     C                   Z-Add     0             $Ind
043200000000     C                   Z-Add     0             SqlEr3
043300000000     C*
043400141230     C                   DoW       $FineDIC    = *Off
043500000000     C*
043600141230     C                   EXSR      FetchDIC
043700000000     C*
043800000000     C                   Select
043900000000     C*-----------------------
044000000000     C* OK
044100000000     C                   When      SqlCod     >= 0     and
044200000000     C                             SqlCod     <> 100
044300000000     C*
044400150105     C     $Ind          OCCUR     ANDICDS
044500000000     C*
044600141230     C                   EXSR      ElabDIC
044700000000     C*
044800000000     C*-----------------------
044900000000     C* fine lettura
045000000000     C                   When      SqlCod      = 100
045100000000     C*
045200141230     C                   Move      *On           $FineDIC
045300000000     C*
045400000000     C                   Other
045500000000     C*
045600000000     C*-----------------------
045700000000     C* si è verificato un errore
045800000000     C                   Eval      $TpErr1     = '99'
045900141230     C                   Eval      StringaSql  = SqlDIC
046000000000     C                   EXSR      StpErr1
046100141230     C                   Eval      ERRJ52      = '1'
046200000000     C*
046300141230     C                   Move      *On           $FineDIC
046400000000     C                   endsl
046500000000     C*
046600000000     C                   endDo
046700000000     C*
046800000000     C                   ENDSR
046900000000     C/EJECT
047000000000     C******************************************************
047100000000     C* Eseguo lettura
047200000000     C******************************************************
047300141230     C     FetchDIC      BEGSR
047400000000     C*
047500000000     C     $Ind          Ifeq      SqlEr3
047600000000     C/EXEC SQL
047700150105     C+     FETCH A1 FOR 1000 ROWS INTO :ANDICDS
047800000000     C/END-EXEC
047900000000     C                   Z-ADD     1             $Ind
048000000000     C                   else
048100000000     C                   ADD       1             $Ind
048200000000     C                   endif
048300000000     C*
048400000000     C                   ENDSR
048500000000     C/EJECT
048600000000     C********************************************************************
048700141230     C* Elaboro lettere di intento da inviare a fornitori
048800000000     C********************************************************************
048900141230     C     ElabDIC       BEGSR
049000150112     C*
049100150112     C                   Eval      $WriteDIE   = *On
049200130826     C*
049300150112     C* verifico se la lettera è stata inserita/modificata manualmente
049400150112     C* nei movimenti estratti
049500141230     C                   EXSR      CtrExistDIE2
049600141230     C                   if        $WriteDIE   = *On
049700130826     C*
049800130826     C                   EXSR      ChkConto
049900000000     C*
050000000000     C*-------------------
050100141230     C                   if        $OkConto    = *On
050200150105     C                   EXSR      ChkANDIC
050300150113     C                   Eval      $WriteDIE   = $OkDIC
050400110902     C*
050500110902     C                   else
050600150113     C                   eval      $WriteDIE   = *Off
050700110902     C                   endif
050800150113     C*
050900150113     C                   endif
051000130826     C*
051100130826     C* ---------------
051200141230     C                   if        $WriteDIE   = *ON
051300150105     C* verifico se la lettera era già stata estratta precedentemente
051400141230     C                   EXSR      CtrExistDIE
051500141230     C                   endif
051600130826     C*
051700150113     C* ---------------
051800150105     C                   if        $WriteDIE   = *On
051900150105     C* SCRIVO SDGDIE00F da ANDIC
052000141230     C                   EXSR      GesDIE_DIC
052100130826     C*
052200130826     C                   endif
052300130826     C*
052400130826     C                   ENDSR
052500130826     C/EJECT
052600150112     C************************************************************
052700150112     C* Controllo se registrazione inserita/modificata manualmente
052800150112     C************************************************************
052900150112     C     CtrExistDIE2  BEGSR
053000150112     C*
053100150112     C                   Eval      $ExistDIE   = *Off
053200150112     C*
053300150112     C                   MoveL     DICSocieta    DIESocieta
053400150112     C                   MoveL     DICDtInter    DIEDtInter
053500150120     C                   MoveL(P)  DICNrInter    WCod
053600150120     C                   EXSR      TogliZero
053700150120     C                   Eval      DIENrInter  = WNrInter
053800150112     C     K03DIE01      SETLL     SDGDIE01L                              21
053900150112     C     K03DIE01      READE     SDGDIE01L                              21
054000150112     C                   Eval      $ExistDIE   = Not(*In21)
054100150112     C*
054200150112     C                   Eval      $FineDIE    = *In21
054300150112     C                   DoW       $FineDIE    = *Off
054400150112     C*
054500150112     C                   EXSR      GestExistDIE
054600150112     C*
054700150112     C     K03DIE01      READE     SDGDIE01L                              21
054800150112     C                   Eval      $FineDIE    = *In21
054900150112     C                   endDo
055000150112     C*
055100150112     C                   ENDSR
055200150112     C/EJECT
055300130826     C************************************************************
055400150120     C* Tolgo gli zeri e metto *Blank
055500130826     C************************************************************
055600150120     C     TogliZero     BEGSR
055700130826     C*
055800150120     C                   If        WCod        > *Blank
055900150120     C*
056000150120     C* Sostituisco gli zeri a sx non significativi con dei blank
056100150120     C                   MoveA     WCod          SkCod
056200150120     C                   Do        6             $X                2 0
056300150120     C                   If        SkCod($X)='0'
056400150120     C                   Move      *Blank        SkCod($X)
056500150120     C                   Else
056600150120     C                   Leave
056700150120     C                   EndIf
056800150120     C                   EndDo
056900150120     C                   MoveA     SkCod         WCod
057000150120     C*
057100150120     C* Elimino i blank a dx
057200150120     C                   Move(P)   WCod          WNrInter
057300150120     C*
057400150120     C                   EndIf
057500150120     C*
057600150120     C                   ENDSR
057700150120     C/EJECT
057800150120     C************************************************************
057900150120     c* Gestione documento trovato nei dati estratti
058000150120     C************************************************************
058100150120     C     GestExistDIE  BEGSR
058200150120     C*
058300130826     C* se è manuale, non faccio nulla
058400141230     C                   if        DIEOrigine  = 'M'
058500141230     C                   Eval      $WriteDIE   = *Off
058600141230     C                   Eval      $FineDIE    = *On
058700130826     C                   endif
058800130826     C*
058900130826     C                   ENDSR
059000130826     C/EJECT
059100000000     C************************************************************
059200000000     C* Controllo dati conto
059300000000     C************************************************************
059400000000     C     ChkConto      BEGSR
059500000000     C*
059600141230     C                   IF        DICKsc     <> SAVKsc
059700000000     C*
059800141230     C                   Move      DICKsc        SAVKsc
059900000000     C*
060000000000     C                   EXSR      InzConto
060100150108     C*
060200150108     C                   CALLB     'XCAPCLIFOR'
060300150108     C                   PARM                    XSCSOC
060400150108     C                   PARM      'F     '      WKcc
060500150108     C                   PARM      DICKsc        WKsc
060600000000     C*
060700000000     C                   Eval      $Data       = UDateIso
060800000000     C                   EXSR      CallMVC002
060900000000     C*
061000000000     C                   if        $Esito      = *On
061100000000     C* annullo il conto
061200000000     C                   Eval      $AnnConto   = *On
061300000000     C                   if        $TpAnn      = *Blank
061400150112     C                   Eval      $TpAnn      = '01'
061500000000     C* stampo avvertimento per mancato reperimento dati conto
061600000000     C                   EXSR      StpErr1
061700000000     C                   endif
061800000000     C                   endif
061900000000     C*
062000000000     C*----------------------
062100000000     C*
062200000000     C                   if        $OkConto    = *On
062300000000     C* reperimento dati fiscali dipendenti solo dal conto
062400000000     C                   EXSR      RepDati_Conto
062500000000     C                   endif
062600000000     C*
062700000000     C*----------------------
062800000000     C*
062900000000     C                   if        $OkConto    = *On
063000000000     C                   EXSR      SelConto
063100000000     C                   endif
063200000000     C*
063300000000     C*----------------------
063400000000     C*
063500000000     C                   ENDIF
063600000000     C*
063700000000     C                   ENDSR
063800000000     C/EJECT
063900140526     C************************************************************
064000000000     C* Inizializzo variabili appoggio per dati conto cli/for
064100000000     C************************************************************
064200000000     C     InzConto      BEGSR
064300000000     C*
064400000000     C                   Eval      $OkConto    = *On
064500000000     C                   Eval      $AnnConto   = *Off
064600000000     C                   Eval      $TpAnn      = *Blank
064700000000     C*
064800130903     C                   Clear                   $TpNSog
064900000000     C                   Clear                   $CdFiscSog
065000000000     C                   Clear                   $PartIVASog
065100000000     C                   Clear                   $CogRagSog
065200000000     C                   Clear                   $NomeSog
065300000000     C*
065400130903     C                   Clear                   $TpN
065500000000     C                   Clear                   $CdFisc
065600000000     C                   Clear                   $PartIVA
065700000000     C                   Clear                   $CogRag
065800130903     C                   Clear                   $Nome
065900000000     C*
066000150105     C* SAVDtInter serve per reperire i dati fiscali
066100000000     C* storicizzati per il soggetto legato al conto
066200150105     C                   Move      *Loval        SAVDtInter
066300000000     C*
066400150105     C* SAVDtInte2 serve per stampare i dati fiscali
066500000000     C* non valorizzati sul soggetto legato al conto
066600150105     C                   Move      *Loval        SAVDtInte2
066700000000     C*
066800000000     C                   ENDSR
066900000000     C/EJECT
067000000000     C***********************************************************
067100000000     C* Driver per controllo conto
067200000000     C***********************************************************
067300000000     C     CallMvc002    BEGSR
067400000000     C*
067500000000     C                   Clear                   ND002DS
067600000000     C*
067700000000     C                   Eval      $LenOut    =  %Size(ND002DS)
067800000000     C*
067900000000     C                   CallB     'NDMVC002'
068000000000     C                   PARM                    XScSoc
068100000000     C                   PARM                    WKCC
068200000000     C                   PARM                    WKSC
068300000000     C                   PARM                    $Data
068400000000     C                   PARM      *Off          $GesMsg
068500000000     C                   PARM      *Off          $Esito
068600000000     C                   PARM      'ND002DS   '  $Struttura
068700000000     C                   PARM                    ND002DS
068800000000     C                   PARM                    $LenOut
068900000000     C*
069000000000     C                   ENDSR
069100000000     C/EJECT
069200000000     C************************************************************
069300000000     C* Reperimento dati fiscali del conto
069400000000     C************************************************************
069500000000     C     RepDati_Conto BEGSR
069600000000     C*
069700000000     C                   Eval      WSogg       = Sogg002
069800000000     C                   Eval      WSNatura    = SNatura002
069900130826     C                   Eval      WTpInd      = *Blank
070000130826     C                   Eval      WCdInd      = *Blank
070100000000     C*
070200000000     C                   select
070300000000     C                   when      WSNatura    = 'F'
070400000000     C                   MoveL     'DVAFOR    '  DVAStrutt
070500000000     C                   Eval      DVALenout   = %Size(DVAFor)
070600000000     C                   endsl
070700000000     C*
070800000000     C                   EXSR      CallDvaSog
070900000000     C*
071000000000     C                   if        DVAErrore  <> *Off
071100000000     C* annullo il conto
071200000000     C                   Eval      $AnnConto   = *On
071300000000     C                   if        $TpAnn      = *Blank
071400150112     C                   Eval      $TpAnn      = '01'
071500000000     C                   EXSR      StpErr1
071600000000     C                   endif
071700000000     C*
071800000000     C                   else
071900000000     C*
072000000000     C                   Select
072100000000     C                   when      SNatura002  = 'F'
072200000000     C                   EXSR      RieDvaFor
072300000000     C                   endSl
072400000000     C*
072500000000     C                   endif
072600000000     C*
072700000000     C                   ENDSR
072800000000     C/EJECT
072900000000     C************************************************************
073000000000     C* Chiamata al driver dei soggetti
073100000000     C************************************************************
073200000000     C     CallDvaSog    BEGSR
073300000000     C*
073400000000     C                   CALLB     'NDDVASOG'
073500000000     C                   PARM      '1'           DVARic
073600000000     C                   PARM      XScSoc        DVASocieta
073700000000     C                   PARM      *Blank        DVAUnita
073800000000     C                   PARM                    DVAStrutt
073900000000     C                   PARM                    DVADtRif
074000000000     C                   PARM                    DVALenOut
074100000000     C                   PARM      WSNatura      DVASNatura
074200000000     C                   PARM      WKsc          DVACodice
074300000000     C                   PARM      *Blank        DVALineaV
074400000000     C                   PARM      *Zero         DVAFiliale
074500000000     C                   PARM      WSogg         DVASogg
074600000000     C                   PARM      WTpInd        DVATpInd
074700000000     C                   PARM      WCdInd        DVACdInd
074800000000     C                   PARM      *Off          DVAErrore
074900000000     C                   PARM                    DVAOutPut
075000000000     C*
075100000000     C                   ENDSR
075200000000     C/EJECT
075300000000     C************************************************************
075400000000     C* Dati del fornitore
075500000000     C************************************************************
075600000000     C     RieDvaFor     BEGSR
075700000000     C*
075800000000     C                   Eval      %subst(DvaFor:1:DVALenout)
075900000000     C                              = %subst(DVAOutput:1:DVALenout)
076000000000     C*
076100130903     C                   Eval      $NatGiur    = DVFNatGiur
076200130903     C                   EXSR      RepTpN
076300130903     C                   if        XTbErr      = '0'
076400130903     C                   MoveL     XTbUni        ANGG16DS
076500150108     C*
076600150108     C                   Select
076700150108     C                   When      TpNG16      = '3'
076800150108     C                   Eval      $TpNSog     = '2'
076900150108     C*
077000150108     C* in caso di Dogana non importo il tipo di natura giuridica
077100150108     C                   When      AlleIVA002  = '02'
077200150108     C                   Eval      $TpNSog     = *Blank
077300150108     C*
077400150108     C                   Other
077500150108     C                   Eval      $TpNSog     = TpNG16
077600150108     C                   endSl
077700150108     C*
077800130903     C                   endif
077900130903     C*
078000000000     C                   MoveL     DVFPartIva    $PartIVASog
078100000000     C                   MoveL     DVFCdFisc     $CdFiscSog
078200000000     C                   MoveL     DVFDes        $CogRagSog
078300000000     C                   MoveL     *Blank        $NomeSog
078400150108     C                   MoveL     DVFSex        $SexSog
078500000000     C*
078600000000     C*
078700000000     C* verifico se sono presenti i dati aggiuntivi anagrafici
078800000000     C* se esistono sovrascrivo i dati appena reperiti
078900000000     C                   Eval      SO3Sogg     = WSogg
079000000000     C                   Eval      SO3DtFiVl   = *Loval
079100000000     C                   Eval      SO3Sys      = 0
079200000000     C                   Eval      SO3NrAsInt  = 0
079300000000     C                   EXSR      RepSO3_SOG
079400000000     C*
079500000000     C                   ENDSR
079600000000     C/EJECT
079700000000     C********************************************************************
079800130903     C* Reperisco la tipologia della natura giuridica
079900000000     C********************************************************************
080000130903     C     RepTpN        BEGSR
080100000000     C*
080200130903     C                   Clear                   SDGTABDS
080300130903     C*
080400130903     C                   Eval      XTbRic      = '1'
080500130903     C                   Eval      XTbAzi      = XScSoc
080600130903     C                   Eval      XTbKey      = *Zero
080700130903     C                   Eval      XTbCod      = 'G16'
080800130903     C                   Move      $NatGiur      XTbKey
080900130903     C*
081000130903     C                   CALL      'SDGTABDR'
081100130903     C                   PARM                    SDGTABDS
081200130903     C*
081300130903     C                   if        XTbErr      = '1'
081400130903     C                   Eval      $AnnConto   = *On
081500130903     C                   if        $TpAnn      = *Blank
081600150112     C                   Eval      $TpAnn      = '02'
081700130903     C                   EXSR      StpErr1
081800130903     C                   endif
081900130903     C                   endif
082000130903     C*
082100130903     C                   ENDSR
082200130903     C/EJECT
082300130903     C********************************************************************
082400130903     C* Reperisco i dati anagrafici aggiuntivi
082500130903     C********************************************************************
082600130903     C     RepSO3_SOG    BEGSR
082700130903     C*
082800160530R224 X**** K04SO301      CHAIN     SDGSO301L                          21
082801160530R224 C                   EXSR      ChainSDGSO3
082802160530R224 C*
082900000000     C                   if        *In21       = *Off
083000000000     C                   MoveL(P)  SO3CdFisc     $CdFiscSog
083100000000     C                   MoveL(P)  SO3PartIva    $PartIVASog
083200000000     C                   MoveL(P)  SO3CogRag     $CogRagSog
083300000000     C                   MoveL(P)  SO3Nome       $NomeSog
083400150108     C                   MoveL(P)  SO3Sex        $SexSog
083500000000     C                   endif
083600000000     C*
083700000000     C                   ENDSR
083800000000     C/EJECT
083801160530R224 C************************************************************
083802160530 "   C* Lettura dati aggiuntivi anagrafici
083803160530 "   C************************************************************
083804160530R224 C     ChainSDGSO3   BEGSR
083805160530     C*
083806160530     C                   Eval      SO3Dest     = $Dest
083818160530     C*
083819160530     C     K05SO301      CHAIN     SDGSO301L                          21
083820160530     C*
083821160530     C* se non trovato, provo con la destinazione Generica
083822160530     C                   if        *In21       = *On     and
083823160530     C                             SO3Dest    <> *Blank
083824160530     C                   Eval      SO3Dest     = *Blank
083825160530     C     K05SO301      CHAIN     SDGSO301L                          21
083826160530     C                   endif
083827160530     C*
083828160530R224 C                   ENDSR
083829160530R224 C/EJECT
083900000000     C************************************************************
084000000000     C* Selezioni sul conto
084100000000     C************************************************************
084200000000     C     SelConto      BEGSR
084300000000     C*
084400000000     C                   Select
084500000000     C*
084600000000     C* il soggetto non rientra nelle selezioni, scarto il conto
084700141230     C                   When      Sogg002    <> SoggJ52  and
084800141230     C                             SoggJ52    <> *Blank
084900130826     C                   Eval      $OkConto    = *Off
085000130826     C*
085100130826     C                   endSl
085200000000     C*
085300000000     C                   ENDSR
085400000000     C/EJECT
085500130826     C******************************************************
085600141230     C* Controllo lettera di intento
085700000000     C******************************************************
085800150105     C     ChkANDIC      BEGSR
085900000000     C*
086000150105     C                   EXSR      InzANDIC
086100150112     C*
086200150112     C* verifico se la lettera era già stata estratta precedentemente
086300150112     C                   EXSR      CtrSDGDIF
086400150105     C*
086500150105     C* reperimento dati fiscali dipendenti dalla lettera
086600150105     C* di intento
086700150105     C                   EXSR      RepDati_DIC
086800000000     C*
086900150113     C                   EXSR      SelANDIC
087000150113     C*
087100000000     C                   ENDSR
087200000000     C/EJECT
087300000000     C******************************************************
087400141230     c* Inizializzo le variabili per righe DIC
087500000000     C******************************************************
087600150105     C     InzANDIC      BEGSR
087700000000     C*
087800000000     C* se il movimento è annullato , estraggo cmq le righe
087900000000     C* IVA del periodo e le scrivo annullate
088000141230     C                   Eval      $AnnDIC     = $AnnConto
088100141230     C                   if        $AnnDIC    <> *On
088200000000     C                   Eval      $TpAnn      = *Blank
088300000000     C                   endif
088400000000     C*
088500000000     C                   ENDSR
088600000000     C/EJECT
088700150112     C************************************************************
088800150112     C* Controllo se registrazione già estratta
088900150112     C************************************************************
089000150112     C     CtrSDGDIF     BEGSR
089100150112     C*
089200150112     C                   Eval      DIFSocieta  = DICSocieta
089300150112     C                   Eval      DIFCF       = DICCF
089400150112     C                   Eval      DIFDtInter  = DICDtInter
089500150112     C                   Eval      DIFNrInter  = DICNrInter
089600150112     C     K04DIF01      CHAIN     SDGDIF01L                          21
089700150112     C*
089800150112     C                   if        *In21       = *Off
089900150112     C*
090000150112     C                   if        DIFInviato  = *On
090100150112     C*
090200150112     C                   Eval      $AnnDIC     = *On
090300150112     C                   if        $TpAnn      = *Blank
090400150112     C                   Eval      $TpAnn      = '03'
090500150112     C                   EXSR      StpErr1
090600150112     C                   endif
090700150112     C*
090800150112     C                   endif
090900150112     C*
091000150112     C                   endif
091100150112     C*
091200150112     C                   ENDSR
091300150112     C/EJECT
091400150112     C************************************************************
091500150112     C* Reperimento dati fiscali dipendenti dalla lettera
091600150112     C************************************************************
091700150112     C     RepDati_DIC   BEGSR
091800150112     C*
091900150112     C                   Select
092000150112     C                   When      Tecnico002 <> '2'     and
092100150112     C                             DICDtInter <> SAVDtInter
092200150112     C                   EXSR      RepANCPI
092300150112     C                   Move      DICDtInter    SAVDtInter
092400150112     C*
092500150112     C                   endSl
092600150112     C*
092700150112     C                   ENDSR
092800150112     C/EJECT
092900150112     C************************************************************
093000150112     C* Reperimento dati anagrafici da dati storicizzati
093100150112     C************************************************************
093200150112     C     RepANCPI      BEGSR
093300150112     C*
093400150112     C*  inizializzo i dati anagrafici con quelli del soggetto,
093500150112     C*  verranno sostituiti da quelli specifici se reperiti
093600150112     C                   MoveL(P)  $TpNSog       $TpN
093700150112     C                   MoveL(P)  $CdFiscSog    $CdFisc
093800150112     C                   MoveL(P)  $PartIVASog   $PartIVA
093900150112     C                   MoveL(P)  $CogRagSog    $CogRag
094000150112     C                   MoveL(P)  $NomeSog      $Nome
094100150112     C                   MoveL(P)  $SexSog       $Sex
094200150112     C*
094300150112     C                   Eval      DVPLenOut = %size(ANCPI00F)
094400150112     C*
094500150112     C                   CALLB     'NDDVACPI'
094600150112     C                   PARM      '2'           DVPRic
094700150112     C                   PARM      DICSocieta    DVPSocieta
094800150112     C                   PARM      *Blank        DVPUnita
094900150112     C                   PARM      'ANCPI     '  DVPStrutt
095000150112     C                   PARM      DICDtInter    DVPDtRif
095100150112     C                   PARM                    DVPLenOut
095200150112     C                   PARM      Sogg002       DVPSogg
095300150112     C                   PARM      *Off          DVPErrore
095400150112     C                   PARM                    DVPOutPut
095500150112     C*
095600150112     C                   if        DVPErrore   = *Off
095700150112     C                   Eval      %subst(ANCPI00F:1:DVPLenout)
095800150112     C                              = %subst(DVPOutput:1:DVPLenout)
095900150112     C*
096000150112     C* trovati dati storicizzati alla data documento, sovrascrivo
096100150112     C* quelli validi attualmente per il soggetto
096200150112     C                   Eval      $NatGiur    = CPINatGiur
096300150112     C                   EXSR      RepTpN
096400150112     C                   if        XTbErr      = '0'
096500150112     C                   MoveL     XTbUni        ANGG16DS
096600150112     C*
096700150112     C                   Select
096800150112     C                   When      TpNG16      = '3'
096900150112     C                   Eval      $TpN        = '2'
097000150112     C*
097100150112     C* in caso di Dogana non importo il tipo di natura giuridica
097200150112     C                   When      AlleIVA002  = '02'
097300150112     C                   Eval      $TpN        = *Blank
097400150112     C*
097500150112     C                   Other
097600150112     C                   Eval      $TpN        = TpNG16
097700150112     C                   endSl
097800150112     C*
097900150112     C                   endif
098000150112     C*
098100150112     C                   MoveL(P)  CPICdFisc     $CdFisc
098200150112     C                   MoveL(P)  CPIPartIva    $PartIVA
098300150112     C                   MoveL(P)  CPIDesSog1    $CogRag
098400150112     C*
098500150112     C* se il soggetto è persona fisica i seguenti dati NON cambiano
098600150112     C* mai, percui se reperiti per il soggetto vanno comunque sempre
098700150112     C* bene
098800150112     C*******            MoveL     *Blank        $Nome
098900150112     C*******            MoveL     *Blank        $Sex
099000150112     C*
099100150112     C*
099200150112     C* verifico se sono presenti i dati aggiuntivi anagrafici
099300150112     C* se esistono sovrascrivo i dati appena reperiti
099400150112     C                   Eval      SO3Sogg     = CPISogg
099500150112     C                   Eval      SO3DtFiVl   = CPIDtFiVL
099600150112     C                   Eval      SO3Sys      = 0
099700150112     C                   Eval      SO3NrAsInt  = 0
099800150112     C                   EXSR      RepSO3_2
099900150112     C*
100000150112     C                   endif
100100150112     C*
100200150112     C* se non ci sono dati storicizzati alla data, non è un problema
100300150112     C* significa solo che sono validi quelli impostati sul soggetto
100400150112     C*
100500150112     C                   ENDSR
100600150112     C/EJECT
100700150112     C********************************************************************
100800150112     C* Reperisco i dati anagrafici aggiuntivi
100900150112     C********************************************************************
101000150112     C     RepSO3_2      BEGSR
101100150112     C*
101200160530R224 X**** K04SO301      CHAIN     SDGSO301L                          21
101201160530R224 C                   EXSR      ChainSDGSO3
101202160530R224 C*
101300150112     C                   if        *In21       = *Off
101400150112     C                   MoveL(P)  SO3CdFisc     $CdFisc
101500150112     C                   MoveL(P)  SO3PartIva    $PartIVA
101600150112     C                   MoveL(P)  SO3CogRag     $CogRag
101700150112     C                   MoveL(P)  SO3Nome       $Nome
101800150112     C                   MoveL(P)  SO3Sex        $Sex
101900150112     C                   endif
102000150112     C*
102100150112     C                   ENDSR
102200150112     C/EJECT
102300150113     C********************************************************************
102400150113     C* Seleziono dati sul movimento
102500150113     C********************************************************************
102600150113     C     SelANDIC      BEGSR
102700150113     C*
102800150113     C                   Eval      $OkDIC      = *On
102900150113     C*
103000150113     C                   Select
103100150113     C*
103200150113     C                   When      $PartIVA   <> PartIVAJ52 and
103300150113     C                             PartIVAJ52 <> *Blank
103400150113     C                   Eval      $OkDIC      = *Off
103500150113     C*
103600150113     C                   When      $CdFisc    <> CdFiscJ52  and
103700150113     C                             CdFiscJ52  <> *Blank
103800150113     C                   Eval      $OkDIC      = *Off
103900150113     C*
104000150113     C                   endSl
104100150113     C*
104200150113     C                   ENDSR
104300150113     C/EJECT
104400150112     C************************************************************
104500150112     C* Controllo se registrazione già estratta e attiva
104600150112     C************************************************************
104700150112     C     CtrExistDIE   BEGSR
104800150112     C*
104900150112     C                   Eval      $ExistDIE   = *Off
105000150112     C                   Eval      $AnnullaDIE = *Off
105100150112     C*
105200150112     C                   MoveL     DICSocieta    DIESocieta
105300150112     C                   MoveL     DICDtInter    DIEDtInter
105400150120     C                   MoveL(P)  DICNrInter    WCod
105500150120     C                   EXSR      TogliZero
105600150120     C                   Eval      DIENrInter  = WNrInter
105700150112     C     K03DIE01      SETLL     SDGDIE01L                              21
105800150112     C     K03DIE01      READE     SDGDIE01L                              21
105900150112     C                   Eval      $ExistDIE   = Not(*In21)
106000150112     C                   Eval      $FineDIE    = *In21
106100150112     C                   DoW       $FineDIE    = *Off
106200150112     C*
106300150112     C                   EXSR      GestExistDIE
106400150112     C*
106500150112     C     K03DIE01      READE     SDGDIE01L                              21
106600150112     C                   Eval      $FineDIE    = *In21
106700150112     C                   endDo
106800150112     C*
106900150112     C*
107000150112     C* trovato, lo annullo
107100150112     C                   if        $ExistDIE   = *On       and
107200150112     C                             $AnnullaDIE = *On
107300150112     C                   Eval      $AnnDIC     = *On
107400150112     C                   if        $TpAnn      = *Blank
107500150112     C                   Eval      $TpAnn      = '04'
107600150112     C                   EXSR      StpErr1
107700150112     C                   endif
107800150112     C                   endif
107900150112     C*
108000150112     C                   ENDSR
108100150112     C/EJECT
108200000000     C************************************************************
108300150105     C* Scrivo SDGDIE00F a dettaglio di ANDIC00F
108400000000     C************************************************************
108500150105     C     GesDIE_DIC    BEGSR
108600150126R163 C*
108700150126 "   C* nel caso in cui il fornitore sia persona fisica
108800150126 "   C* e manchi il nome, annullo il movimento
108900150126 "   C                   if        $Tpn        = '1'      and
109000150126 "   C                             $Nome       = *Blank
109100150126 "   C*
109200150126 "   C                   Eval      $AnnDIC     = *On
109300150126 "   C                   if        $TpAnn      = *Blank
109400150126 "   C                   Eval      $TpAnn      = '01'
109500150126 "   C                   EXSR      StpErr1
109600150126 "   C                   endif
109700150126R163 C                   endif
109800000000     C*
109900000000     C                   EXSR      ChkDatiFisc
110000000000     C*
110100141230     C                   EXSR      RieSDGDIE
110200150108     C                   WRITE     SDGDIE000
110300000000     C*
110400000000     C                   ENDSR
110500000000     C/EJECT
110600000000     C********************************************************************
110700000000     C* Controllo se presenti i dati fiscali obbligatori
110800000000     C********************************************************************
110900000000     C     ChkDatiFisc   BEGSR
111000000000     C*
111100150105     C* nel caso in cui non si tratti di Dogana, Part.Iva o
111200000000     C* codice fiscale devono essere valorizzati
111300130918     C                   if        $PartIVA    = *Blank   and
111400130918     C                             $CdFisc     = *Blank   and
111500150108     C                             AlleIVA002 <> '02'
111600000000     C*
111700141230     C                   Eval      $AnnDIC     = *On
111800000000     C                   if        $TpAnn      = *Blank
111900150112     C                   Eval      $TpAnn      = '05'
112000000000     C                   EXSR      StpErr1
112100000000     C                   endif
112200000000     C*
112300000000     C*
112400000000     C                   Select
112500000000     C*
112600141230     C* conto normale: stampo solo a cambio di data di protocollo
112700000000     C                   When      Tecnico002 <> '2'          and
112800141230     C                             (DICDtInter <> SAVDtInte2   or
112900141230     C                              DICKsc     <> SAVKsc2          )
113000000000     C*
113100141230     C                   Move      DICDtInter    SAVDtInte2
113200141230     C                   MoveL     DICKsc        SAVKsc2
113300000000     C                   EXSR      StpNoFisc
113400141230     C*
113500141230     C* conto tecnico
113600141230     C                   When      Tecnico002 = '2'
113700141230     C                   EXSR      StpNoFisc
113800000000     C*
113900000000     C                   endsl
114000000000     C*
114100000000     C                   endif
114200000000     C*
114300000000     C                   ENDSR
114400000000     C/EJECT
114500000000     C************************************************************
114600150105     C* Riempio campi di SDGDIE00F
114700000000     C************************************************************
114800141230     C     RieSDGDIE     BEGSR
114900000000     C*
115000150108     C                   Clear                   SDGDIE000
115100150108     C*
115200150108     C                   CALLB     'XCAPCLIFOR'
115300150108     C                   PARM                    XSCSOC
115400150108     C                   PARM      'F     '      WKcc
115500150108     C                   PARM      DICKsc        WKsc
115600150108     C*
115700150105     C                   Eval      $Data       = DICDtInter
115800000000     C                   EXSR      CallMVC002
115900141230     C                   Eval      DIEUTEIMM   = XScCUt
116000141230     C                   Eval      DIEDTIMM    = UDateIso
116100141230     C                   Eval      DIEUTEULMO  = XScCUt
116200141230     C                   Eval      DIEDTULMO   = UDateIso
116300000000     C*
116400130821     C* Automatica
116500141230     C                   Eval      DIEORIGINE  = 'A'
116600141230     C                   Eval      DIESOCIETA  = XScSoc
116700150108     C*
116800141230     C                   Eval      DIEAnno     = AnnoJ52
116900150108     C*
117000150108     C                   Eval      DIEDtInter  = DICDtInter
117100150120     C                   MoveL(P)  DICNrInter    WCod
117200150120     C                   EXSR      TogliZero
117300150120     C                   Eval      DIENrInter  = WNrInter
117400150108     C*
117500150108     C* segnalo se è Dogana
117600150108     C                   if        AlleIVA002  = '02'
117700150108     C                   Eval      DIEDogana   = '1'
117800150108     C                   else
117900150108     C                   Eval      DIEDogana   = '0'
118000150108     C                   endif
118100150108     C*
118200141230     C                   Eval      DIESOGG     = Sogg002
118300150108     C                   Eval      DIEKSC      = DICKsc
118400150108     C                   Eval      DIETpN      = $TpN
118500000000     C*
118600141230     C                   Eval      DIEPARTIVA  = $PartIVA
118700141230     C                   Eval      DIECDFISC   = $CdFisc
118800000000     C*
118900000000     C                   Eval      $CampoScan  = $CogRag
119000000000     C                   EXSR      TolgoApice
119100000000     C                   Eval      $CogRag     = $CampoScan
119200141230     C                   Eval      DIECOGRAG   = $CogRag
119300000000     C*
119400000000     C                   Eval      $CampoScan  = $Nome
119500000000     C                   EXSR      TolgoApice
119600000000     C                   Eval      $Nome       = $CampoScan
119700141230     C                   Eval      DIENOME     = $Nome
119800000000     C*
119900150108     C                   Eval      DIESex      = $Sex
120000150108     C*
120100150108     C* reperisco il tipo di operazione dichiarata
120200150108     C                   if        DICOpera    = *On
120300150108     C* una sola operazione specifica
120400150108     C                   Eval      DIETpOpe    = '1'
120500150108     C                   else
120600150108     C                   if        DICAnno    <> 0     and
120700150108     C                             DICImporto <> 0
120800150108     C* operazioni effettuate fino a concorrenza
120900150108     C                   Eval      DIETpOpe    = '2'
121000150108     C                   else
121100150108     C* operazioni nel periodo
121200150108     C                   Eval      DIETpOpe    = '3'
121300150108     C                   endif
121400150108     C                   endif
121500150108     C*
121600150108     C* se è Dogana, ammessa una sola operazione specifica
121601150414R179 C* o Operazioni fino a concorrenza di importo (dal 13/04/15)
121700150108     C                   if        AlleIVA002  = '02'  and
121800150108     C                             DIETpOpe   <> '1'
121801150414R179 C                             and
121802150414R179 C                             DIETpOpe   <> '2'
121900150108     C*
122000150108     C                   Eval      $AnnDIC     = *On
122100150108     C                   if        $TpAnn      = *Blank
122200150112     C                   Eval      $TpAnn      = '06'
122300150108     C                   EXSR      StpErr1
122400150108     C                   endif
122500150108     C*
122600150108     C                   endif
122700150108     C*
122800150108     C                   Eval      DIEImporto  = DICImporto
122900150108     C*
123000150108     C                   Eval      DIEDtInDic  = DICDtInDic
123100150108     C                   Eval      DIEDtFiDic  = DICDtFiDic
123200150108     C*
123300130926     C* reperisco i dati anagrafici del primo record
123400130926     C* inserito con stessa partita IVA e codice fiscale
123500141230     C                   if        DIEPartIVA <> *Blank   or
123600141230     C                             DIECdFisc  <> *Blank
123700130926     C                   EXSR      RepAnagraf
123800130926     C                   endif
123900150113     C*
124000150113     C* reperisco la eventuale descrizione della merce
124100150113     C                   Clear                   SDGTABDS
124200150113     C                   Clear                   ANGG17DS
124300150113     C*
124400150113     C                   MOVE      XSCCUT        XTBUTE
124500150113     C                   MOVE      '1'           XTBRIC
124600150113     C                   MOVE      XSCSOC        XTBAZI
124700150113     C                   MOVE      *ZERO         XTBKEY
124800150113     C                   MOVE      'G17'         XTBCOD
124900150113     C                   MOVE      DICOPI        XTBKEY
125000150113     C                   CALL      'SDGTABDR'
125100150113     C                   PARM                    SDGTABDS
125200150113     C                   if        XTBERR      = *Off
125300150113     C                   MOVEL     XTBUNI        ANGG17DS
125400150113     C                   Eval      DIEDescr    = R01G17 +
125500150113     C                                           R02G17
125600150113     C                   endif
125700130822     C*
125800141230     C                   if        $AnnDIC     = *On
125900141230     C                   Eval      DIEANN      = *On
126000141230     C                   Eval      DIETPANN    = $TpAnn
126100150113     C                   else
126200150113     C                   Eval      DIEANN      = *Off
126300150113     C                   Eval      DIETPANN    = *Blank
126400000000     C                   endif
126500000000     C*
126600000000     C                   ENDSR
126700000000     C/EJECT
126800000000     C************************************************************
126900000000     C* Tolgo l'apice dai campi descrittivi
127000000000     C************************************************************
127100000000     C     TolgoApice    BEGSR
127200000000     C*
127300000000     C                   Eval      $FineScan   = *Off
127400000000     C*
127500000000     C                   DoW       $FineScan   = *Off
127600000000     C                   Clear                   PosIni            4 0
127700000000     C     ''''          Scan      $CampoScan    PosIni                   22
127800000000     C     *IN22         IFEQ      *On
127900000000     C                   Eval      %SubSt($CampoScan:Posini:1)=' '
128000000000     C                   else
128100000000     C                   Eval      $FineScan   = *On
128200000000     C                   endif
128300000000     C                   endDo
128400000000     C*
128500000000     C                   ENDSR
128600000000     C/EJECT
128700130822     C************************************************************
128800130926     C* Reperiti dati anagrafici a parità di partita IVA/cd fiscale
128900130822     C************************************************************
129000130926     C     RepAnagraf    BEGSR
129100130926     C*
129200150105     C******             Clear                   SDGSJ53DS
129300130926     C*
129400150108     C******             Eval      SOCIETAJ53  = XScSoc
129500150108     C******             Eval      ANNOJ53     = DIEAnno
129600150108     C******             Eval      PartIVAJ53  = DIEPartIVA
129700150108     C******             Eval      CdFiscGJ3   = DIECdFisc
129800130926     C*
129900150108     C******             CALL      'SDGSJ53R'
130000150108     C******             PARM                    KPJBA
130100150108     C******             PARM                    SDGSJ53DS
130200130926     C*
130300150108     C******             if        FoundJ53    = *On
130400130926     C*
130500150108     C******             Eval      DIECOGRAG   = COGRAGJ53
130600150108     C******             Eval      DIENOME     = NOMEJ53
130700130926     C*
130800150108     C******             endif
130900130926     C*
131000130926     C                   ENDSR
131100130926     C/EJECT
131200000000     C************************************************************
131300000000     C* INIZIALIZZAZIONE VARIABILI
131400000000     C************************************************************
131500000000     C     INZVAR        BEGSR
131600000000     C*
131700141230     C                   Eval      SDGSJ52DS   = KPJBU
131800000000     C*
131900000000     C*-------------------
132000141230     C* cancello SDGDIE00F in base alle selezioni effettuate
132100141230     C                   EXSR      DelSDGDIE
132200000000     C*
132300150108     C                   OPEN      SDGDIE00F
132400150108     C                   OPEN      SDGDIE01L
132500130826     C*
132600000000     C                   ENDSR
132700000000     C/EJECT
132800000000     C************************************************************
132900141230     C* Cancellazione  SDGDIE00F
133000000000     C************************************************************
133100141230     C     DelSDGDIE     BEGSR
133200000000     C*
133300000000     C* cancello solo i record inseriti automaticamente
133400141230     C                   EXSR      RepSQLDIE
133500000000     C*
133600141230     C                   EXSR      ExeSQLDIE
133700000000     C*
133800000000     C* Riorganizzazione membro
133900000000     C                   Clear                   CmdQcmd
134000141230     C                   Eval      CmdQcmd='RGZPFM FILE(SDGDIE00F)'
134100000000     C                   call      'QCMDEXC'                            28
134200000000     C                   PARM                    CmdQcmd         160
134300000000     C                   PARM      80            Len              15 5
134400000000     C*
134500000000     C                   ENDSR
134600000000     C/EJECT
134700000000     C************************************************************
134800141230     C* REPERIMENTO SCHIERA PER SQLDIE
134900000000     C************************************************************
135000141230     C     RepSQLDIE     BEGSR
135100000000     C*
135200141230     C                   Clear                   SqlDIE
135300000000     C*
135400141230     C* Delete from SDGDIE00F
135500141230     C                   CAT       'DELETE':1    SqlDIE
135600141230     C                   CAT       'from':1      SqlDIE
135700141230     C                   CAT       'SDGDIE00F':1 SqlDIE
135800000000     C*
135900000000     C* SELEZIONI
136000141230     C                   CAT       'WHERE':1     SqlDIE
136100141230     C                   EXSR      WhereDIE
136200000000     C*
136300000000     C                   ENDSR
136400000000     C/EJECT
136500000000     C******************************************************
136600141230     C* Selezioni su SDGDIE00F
136700000000     C******************************************************
136800141230     C     WhereDIE      BEGSR
136900000000     C*
137000141230     C* DIESocieta = SocietaJ52
137100141230     C                   CAT       'DIESocieta':1SQLDIE
137200141230     C                   CAT       '=''':0       SQLDIE
137300141230     C                   CAT       SocietaJ52:0  SQLDIE
137400141230     C                   CAT       '''':0        SQLDIE
137500150112     C*
137600150112     C* and DIEAnno = AnnoJ52
137700150112     C                   MoveL     AnnoJ52       $Anno
137800150112     C                   CAT       'and':1       SQLDIE
137900150112     C                   CAT       'DIEAnno':1   SQLDIE
138000150112     C                   CAT       '=':0         SQLDIE
138100150112     C                   CAT       $Anno:0       SQLDIE
138200000000     C*
138300150112     C* cancello tutti gli automatici e tutti i manuali
138400150112     C* senza tenere conto dei filtri (conta solo la
138500150112     C* società e l'anno)
138600150112     C                   if        DeleteJ52   = '3'
138700150112     C*
138800150112     C                   else
138900150112     C*
139000150112     C* cancello solo gli automatici se è richiesto di
139100150112     C* non cancellare i manuali
139200150112     C                   if        DeleteJ52   = '1'
139300150112     C* DIEOrigine = 'A'
139400150112     C                   CAT       'and':1       SQLDIE
139500150112     C                   CAT       'DIEOrigine':1SQLDIE
139600150112     C                   CAT       '=''':0       SQLDIE
139700150112     C                   CAT       'A''':0       SQLDIE
139800150112     C                   endif
139900000000     C*
140000141230     C* and DIEPartIVA = PartIVAJ52
140100141230     C                   if        PartIVAJ52 <> *Blank
140200141230     C                   CAT       'and':1       SQLDIE
140300141230     C                   CAT       'DIEPartIVA':1SQLDIE
140400141230     C                   CAT       '=''':0       SQLDIE
140500141230     C                   CAT       PartIVAJ52:0  SQLDIE
140600141230     C                   CAT       '''':0        SQLDIE
140700000000     C                   endif
140800000000     C*
140900141230     C* and DIECdFisc = CdFiscJ52
141000141230     C                   if        CdFiscJ52  <> *Blank
141100141230     C                   CAT       'and':1       SQLDIE
141200141230     C                   CAT       'DIECdFisc':1 SQLDIE
141300141230     C                   CAT       '=''':0       SQLDIE
141400141230     C                   CAT       CdFiscJ52:0   SQLDIE
141500141230     C                   CAT       '''':0        SQLDIE
141600000000     C                   endif
141700000000     C*
141800141230     C* and DIESogg = SoggJ52
141900141230     C                   if        SoggJ52    <> *Blank
142000141230     C                   CAT       'and':1       SQLDIE
142100141230     C                   CAT       'DIESogg':1   SQLDIE
142200141230     C                   CAT       '=''':0       SQLDIE
142300141230     C                   CAT       SoggJ52:0     SQLDIE
142400141230     C                   CAT       '''':0        SQLDIE
142500000000     C                   endif
142600000000     C*
142700141230     C* and DIEKsc = KscJ52
142800141230     C                   if        KscJ52     <> *Blank
142900141230     C                   CAT       'and':1       SQLDIE
143000141230     C                   CAT       'DIEKsc':1    SQLDIE
143100141230     C                   CAT       '=''':0       SQLDIE
143200141230     C                   CAT       KscJ52:0      SQLDIE
143300141230     C                   CAT       '''':0        SQLDIE
143400000000     C                   endif
143500150112     C*
143600150112     C* and DIETpOpe = TpOpeJ52
143700150112     C                   if        TpOpeJ52   <> *Blank
143800150112     C                   CAT       'and':1       SQLDIE
143900150112     C                   CAT       'DIETpOpe':1  SQLDIE
144000150112     C                   CAT       '=''':0       SQLDIE
144100150112     C                   CAT       TpOpeJ52:0    SQLDIE
144200150112     C                   CAT       '''':0        SQLDIE
144300150112     C                   endif
144400150112     C*
144500150112     C* and DIENrInter >= NrInte1J52
144600150120     C                   MoveL(P)  NrInte1J52    WCod
144700150120     C                   EXSR      TogliZero
144800150112     C                   CAT       'and':1       SQLDIE
144900150112     C                   CAT       'DIENrInter':1SQLDIE
145000150120     C                   CAT       '>=''':0      SQLDIE
145100150120     C                   CAT       WNrInter:0    SQLDIE
145200150120     C                   CAT       '''':0        SQLDIE
145300150112     C*
145400150112     C* and DIENrInter < NrInte2J52
145500150112     C                   if        NrInte2J52 <> 0
145600150120     C                   MoveL(P)  NrInte2J52    WCod
145700150120     C                   EXSR      TogliZero
145800150112     C                   CAT       'and':1       SQLDIE
145900150112     C                   CAT       'DIENrInter':1SQLDIE
146000150209R167 X****               CAT       '<''':0       SQLDIE
146100150130R167 C                   CAT       '<=''':0      SQLDIE
146200150120     C                   CAT       WNrInter:0    SQLDIE
146300150120     C                   CAT       '''':0        SQLDIE
146400150112     C                   endif
146500150112     C*
146600150112     C                   endif
146700141230     C*
146800000000     C                   ENDSR
146900000000     C/EJECT
147000000000     C******************************************************
147100141230     C* ESECUZIONE SQL per SDGDIE
147200000000     C******************************************************
147300141230     C     EXESQLDIE     BEGSR
147400000000     C*
147500000000     C/EXEC SQL
147600141230     C+     EXECUTE IMMEDIATE :SQLDIE
147700000000     C/END-EXEC
147800000000     C*
147900000000     C                   if        SQLCOD      < 0
148000000000     C                   CALL      'X66CHGJOB'
148100000000     C*
148200141230     C                   Eval      StringaSql  = SQLDIE
148300000000     C                   Eval      $TpErr1     = '99'
148400000000     C                   EXSR      StpErr1
148500141230     C                   Eval      ERRJ52      = '1'
148600000000     C                   endif
148700000000     C*
148800000000     C                   ENDSR
148900000000     C/EJECT
149000000000     C********************************************************************
149100000000     C* Close Sql fetch
149200000000     C********************************************************************
149300000000     C     CloseSql      BEGSR
149400000000     C*
149500000000     C/EXEC SQL
149600000000     C+     Close A1
149700000000     C/END-EXEC
149800000000     C*
149900000000     C                   ENDSR
150000000000     C/EJECT
150100000000     C************************************************************
150200000000     C* OPERAZIONI INIZIALI
150300000000     C************************************************************
150400000000     C     *INZSR        BEGSR
150500000000     C*
150600000000     C     *ENTRY        PLIST
150700000000     C                   PARM                    KPJBA
150800000000     C*
150900000000     C     *DMY          Move      UDate         UDateISO
151000000000     C*
151100141230     C                   Eval      SDGSJ52DS   = KPJBU
151200000000     C*
151300110720     C                   EXSR      REPSOC
151400000000     C*
151500000000     C*-------------------
151600000000     C* valorizzo i dati di testata comuni a tutte le stampe
151700000000     C                   MoveL     KNSIF         NOMSIF
151800000000     C                   MoveL     XSCDSI        NOMDIT
151900000000     C                   MoveL     DSPGM         NOMPGM
152000000000     C                   CALL      'XNETA'                              29
152100000000     C                   PARM                    NOMSYS
152200000000     C*
152300000000     C                   EXSR      RieParam
152400000000     C*
152500000000     C                   EXSR      StpErr1_Inz
152501160530R224 C*
152502160530R224 C                   EXSR      RepDatiAna
152600000000     C*
152700000000     C                   ENDSR
152800000000     C/EJECT
152900000000     C************************************************************
153000000000     C* Reperimento dati società
153100000000     C************************************************************
153200000000     C     REPSOC        BEGSR
153300000000     C*
153400000000     C                   CALL      'SDGSOCDR'
153500000000     C                   PARM      'SOC001'      TIPXSC            6
153600141230     C                   PARM      SocietaJ52    SOCXSC            3
153700000000     C                   PARM                    CDSXSC            9 0
153800000000     C                   PARM                    MODXSC            3
153900000000     C                   PARM      *BLANKS       RTNXSC            1
154000000000     C                   PARM                    XSOCDS
154100000000     C                   PARM                    KPJBA
154200000000     C*
154300000000     C                   if        RtnXsc     <> '1'
154400000000     C                   MoveL     XSOCDS        SOC001
154500000000     C                   else
154600000000     C                   Eval      $TpErr1     = '02'
154700110720     C***************    EXSR      StpErr1
154800141230     C                   Eval      ERRJ52      = '1'
154900000000     C                   EXSR      ENDPGM
155000000000     C                   endif
155100000000     C*
155200000000     C                   ENDSR
155300000000     C/EJECT
155301160530R224 C************************************************************
155302160530 "   C* REPERIMENTO dati anagafici aggiuntivi
155303160530 "   C************************************************************
155304160530R224 C     RepDatiAna    BegSr
155305160530     C*
155306160530     C* preparo i parametri di chiamata al driver
155307160530     C                   Clear                   XMPEDS
155308160530     C                   EVAL      TpChiamata = '01'
155309160530     C                   EVAL      NChiavi = 5
155310160530     C                   EVAL      VLogica = 2
155311160530     C*
155312160530     C* preparo le chiavi per la chiamata al driver
155313160530     C                   Clear                   XMPEDS1
155314160530     C                   MoveL     XSCSOC        DSMPESOC
155315160530     C                   MoveL     'SDGSJ51R A'  DSMPEPGM
155316160530     C                   MoveL     *blanks       DSMPEPRF
155317160530     C* Si reperiscono i dati di default a seconda delle modalita con
155318160530     C* cui il pgm e usato
155319160530     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
155320160530     C* MPETIP = 'PV'  pgm richiamato da menu
155321160530     C* MPETIP = 'PB'  x lancio Batch
155322160530     C                   MoveL     'PB'          DSMPETIP
155323160530     C*
155324160530     C                   CALLB     'XMPE'
155325160530     C                   PARM                    XMPEDS
155326160530     C                   PARM                    XMPEDS1
155327160530     C*
155328160530     C     Risultato     IfEq      '0'
155329160530     C                   MoveL     DSMPEPAR      DSDatiAna
155330160530     C                   EndIf
155331160530     C*
155332160530R224 C                   EndSr
155400000000     C************************************************************
155500000000     C* Imposto i parametri di elaborazione
155600000000     C************************************************************
155700000000     C     RieParam      BEGSR
155800000000     C*
155900141230     C                   Eval      PAAnno      = AnnoJ52
156000000000     C*
156100141230     C                   Eval      PAPartIVA   = PartIVAJ52
156200141230     C                   Eval      PACdFisc    = CdFiscJ52
156300000000     C*
156400141230     C                   Eval      PASogg      = SoggJ52
156500000000     C                   EXSR      RepSoggD
156600000000     C*
156700141230     C                   Eval      PAKsc       = KscJ52
156800000000     C                   EXSR      RepKscD
156900000000     C*
157000150112     C                   Eval      PATpOpe     = TpOpeJ52
157100150112     C                   EXSR      RepTpOpeD
157200150112     C*
157300150112     C                   Eval      PANrInte1   = NrInte1J52
157400150112     C                   Eval      PANrInte2   = NrInte2J52
157500150112     C*
157600150112     C                   Eval      PADelete    = DeleteJ52
157700150112     C                   EXSR      RepDeleteD
157800110720     C*
157900150112     C                   Eval      PAOn1       = XScOn
158000150112     C                   Eval      PAOff1      = XScOff
158100141230     C                   if        StpAnnJ52   = *On
158200110720     C                   Eval      PAStpAnn    = XScOn
158300110720     C                   else
158400110720     C                   Eval      PAStpAnn    = XScOff
158500110720     C                   endif
158600000000     C*
158700000000     C                   ENDSR
158800000000     C/EJECT
158900000000     C************************************************************
159000000000     C* Reperisco la descrizione del soggetto
159100000000     C************************************************************
159200000000     C     RepSoggD      BEGSR
159300000000     C*
159400000000     C                   Clear                   PASoggD
159500000000     C*
159600141230     C                   if        SoggJ52    <> *Blank
159700000000     C*
159800141230     C                   Eval      WSogg       = SoggJ52
159900000000     C                   Eval      WKsc        = *Blank
160000000000     C                   Eval      WKcc        = *Blank
160100000000     C                   Eval      WSNatura    = *Blank
160200000000     C                   Eval      WTpInd      = *Blank
160300000000     C                   Eval      WCdInd      = *Blank
160400000000     C                   MoveL     'DVASOG    '  DVAStrutt
160500000000     C                   Eval      DVALENOUT   = %size(DvaSog)
160600000000     C*
160700000000     C                   EXSR      CallDvaSog
160800000000     C*
160900000000     C                   if        DVAErrore  <> '1'
161000000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
161100000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
161200000000     C*
161300000000     C                   MoveL     DVSDes        PASoggD
161400000000     C                   else
161500000000     C                   MoveL     *All'?'       PASoggD
161600000000     C                   endif
161700000000     C*
161800000000     C                   endif
161900000000     C*
162000000000     C                   ENDSR
162100000000     C/EJECT
162200000000     C************************************************************
162300000000     C* Reperisco la descrizione del conto
162400000000     C************************************************************
162500000000     C     RepKscD       BEGSR
162600000000     C*
162700000000     C                   Clear                   PAKscD
162800000000     C*
162900150108     C                   if        KscJ52     <> *Blank
163000150108     C*
163100150108     C                   CALLB     'XCAPCLIFOR'
163200150108     C                   PARM                    XSCSOC
163300150108     C                   PARM      'F     '      WKcc
163400150108     C                   PARM      KscJ52        WKsc
163500150108     C*
163600000000     C                   Eval      $Data       = UDateIso
163700000000     C*
163800000000     C                   EXSR      CallMVC002
163900000000     C*
164000000000     C                   if        $Esito     <> *On
164100000000     C                   MoveL     DesBrev002    PAKscD
164200000000     C                   else
164300000000     C                   MoveL     *All'?'       PAKscD
164400000000     C                   endif
164500000000     C*
164600000000     C                   endif
164700000000     C*
164800000000     C                   ENDSR
164900000000     C/EJECT
165000150112     C***********************************************************
165100150112     C* Controllo tipo operazione
165200150112     C***********************************************************
165300150112     C     RepTpOpeD     BEGSR
165400150112     C*
165500150112     C                   Clear                   PATpOpeD
165600150112     C*
165700150112     C                   Clear                   X07ValDs
165800150112     C*
165900150112     C                   Move      '1'           X07Ric
166000150112     C                   Move      *On           X07All
166100150112     C                   Movel     'SDGSJ52S '   X07TRc
166200150112     C                   Movel     'TPOPEJ52 '   X07Cam
166300150112     C                   Movel     TpOpeJ52      X07VAL
166400150112     C*
166500150112     C                   Callb     'X07VALR'
166600150112     C                   Parm                    X07ValDs
166700150112     C*
166800150112     C     X07ERR        IFEQ      *Off
166900150112     C                   MOVEL     X07DES        PATpOpeD
167000150112     C                   endif
167100150112     C*
167200150112     C                   ENDSR
167300150112     C/EJECT
167400150112     C***********************************************************
167500150112     C* Controllo tipo cancellazione movimenti
167600150112     C***********************************************************
167700150112     C     RepDeleteD    BEGSR
167800150112     C*
167900150112     C                   Clear                   PADeleteD
168000150112     C*
168100150112     C                   Clear                   X07ValDs
168200150112     C*
168300150112     C                   Move      '1'           X07Ric
168400150112     C                   Move      *On           X07All
168500150112     C                   Movel     'SDGSJ52S '   X07TRc
168600150112     C                   Movel     'DELETEJ52'   X07Cam
168700150112     C                   Movel     DeleteJ52     X07VAL
168800150112     C*
168900150112     C                   Callb     'X07VALR'
169000150112     C                   Parm                    X07ValDs
169100150112     C*
169200150112     C     X07ERR        IFEQ      *Off
169300150112     C                   MOVEL     X07DES        PADeleteD
169400150112     C                   endif
169500150112     C*
169600150112     C                   ENDSR
169700150112     C/EJECT
169800000000     C******************************************************
169900141230     C* Preparo dati 'fissi' (SDGSJ52P1)     (solo I° volta)
170000000000     C******************************************************
170100000000     C     StpErr1_Inz   BEGSR
170200000000     C*
170300000000     C* Overflow
170400000000     C                   if        *In41       = *On        or
170500000000     C                             $PrimaStp1  = *On
170600000000     C* Testata
170700141230     C                   WRITE     SJ52T11
170800000000     C*
170900000000     C                   if        $PrimaStp1  = *On
171000000000     C* Parametri di elaborazione
171100141230     C                   WRITE     SJ52PA1
171200000000     C                   endif
171300000000     C*
171400000000     C* Intestazione colonne
171500000000     C                   if        $TpErr1     = *Blank
171600141230     C                   WRITE     SJ52T21
171700000000     C                   endif
171800000000     C*
171900000000     C                   Eval      *In41       = *Off
172000000000     C                   Eval      $PrimaStp1  = *Off
172100000000     C                   endif
172200000000     C*
172300000000     C                   ENDSR
172400000000     C/EJECT
172500000000     C************************************************************
172600141230     C* Stampa errori e avvertimenti (SDGSJ52P1)
172700000000     C************************************************************
172800000000     C     StpErr1       BEGSR
172900000000     C*
173000000000     C                   EXSR      StpErr1_Inz
173100000000     C*
173200000000     C                   Select
173300000000     C*
173400000000     C* Stampa riga di avvertimento per movimento annullato.
173500000000     C                   When      $TpAnn     <> *Blank
173600110720     C*
173700141230     C                   if        StpAnnJ52   = *On
173800000000     C                   Eval      AVVTpAnn    = $TpAnn
173900000000     C                   EXSR      RepTpAnnD
174000141230     C                   WRITE     SJ52PAnn
174100110720     C                   EXSR      StpErr1_Dati
174200110720     C                   endif
174300000000     C*
174400000000     C*  ERRORE: elaborazione interrotta. Codice società di collegamento
174500000000     C*   non valido. Codice società =
174600000000     C                   When      $TpErr1     = '02'
174700141230     C                   Eval      P102Soc     = SocietaJ52
174800141230     C                   WRITE     SJ52P102
174900000000     C*
175000000000     C*
175100000000     C*  ERRORE: elaborazione interrotta causa errori nella stringa sql.
175200000000     C*  Codice errore SQL 99999 nell'esecuzione della seguente stringa:
175300000000     C                   When      $TpErr1     = '99'
175400000000     C                   Eval      P199SqlCd   = SqlCod
175500000000     C                   Eval      P199SQL1    = %subst(StringaSql:1 :197)
175600000000     C                   Eval      P199SQL2    = %subst(StringaSql:198 :197)
175700000000     C                   Eval      P199SQL3    = %subst(StringaSql:395 :197)
175800000000     C                   Eval      P199SQL4    = %subst(StringaSql:595 :197)
175900000000     C                   Eval      P199SQL5    = %subst(StringaSql:792 :197)
176000141230     C                   WRITE     SJ52P199
176100000000     C*
176200000000     C                   OTHER
176300000000     C*  ERR . Avvertimento/errore non codificato.
176400141230     C                   WRITE     SJ52P1XX
176500000000     C*
176600000000     C                   endSl
176700000000     C*
176800000000     C                   ENDSR
176900000000     C/EJECT
177000000000     C******************************************************
177100000000     C* Stampo i riferimenti del movimento
177200000000     C******************************************************
177300000000     C     StpErr1_Dati  BEGSR
177400000000     C*
177500141230     C                   Clear                   SJ52DA1
177600000000     C*
177700150108     C                   MoveL     DICKsc        DA1Ksc
177800150108     C*
177900150108     C                   CALLB     'XCAPCLIFOR'
178000150108     C                   PARM                    XSCSOC
178100150108     C                   PARM      'F     '      WKcc
178200150108     C                   PARM      DICKsc        WKsc
178300150108     C*
178400150108     C                   Eval      $Data       = DICDtInter
178500000000     C                   EXSR      CallMVC002
178600000000     C                   MoveL     Sogg002       DA1Sogg
178700000000     C*
178800150108     C                   if        DICDtInter <> *Loval
178900150108     C     *JobRun       Move      DICDtInter    DA1DtInter
179000000000     C                   endif
179100150108     C                   Z-Add     DICNrInter    DA1NrInter
179200000000     C*
179300141230     C                   WRITE     SJ52DA1
179400000000     C*
179500000000     C                   ENDSR
179600000000     C/EJECT
179700000000     C************************************************************
179800000000     C* Reperisco la descrizione del tipo annullamento
179900000000     C************************************************************
180000000000     C     RepTpAnnD     BEGSR
180100000000     C*
180200000000     C                   Clear                   X07VALDS
180300000000     C*
180400000000     C                   Move      '1'           X07Ric
180500141230     C                   MoveL     'SDGDIE000 '  X07Trc
180600141230     C                   MoveL     'DIETPANN  '  X07Cam
180700000000     C                   MoveL     $TpAnn        X07Val
180800000000     C*
180900000000     C                   CALLB     'X07VALR'
181000000000     C                   PARM                    X07VALDS
181100000000     C*
181200000000     C                   if        X07ERR      = '0'
181300000000     C                   MoveL     X07Des        AVVTpAnnD
181400000000     C                   else
181500000000     C                   MoveL     *All'?'       AVVTpAnnD
181600000000     C                   endif
181700000000     C*
181800000000     C                   ENDSR
181900000000     C/EJECT
182000000000     C******************************************************
182100141230     C* Preparo dati 'fissi' stampa (SDGSJ52P3) (solo I° volta)
182200000000     C******************************************************
182300000000     C     StpNoFisc_Inz BEGSR
182400000000     C*
182500000000     C* Overflow
182600000000     C                   if        *In43       = *On        or
182700000000     C                             $PrimaStp3  = *On
182800000000     C* Testata
182900141230     C                   WRITE     SJ52T13
183000000000     C*
183100000000     C                   if        $PrimaStp3  = *On
183200000000     C* Parametri di elaborazione
183300141230     C                   WRITE     SJ52PA3
183400000000     C                   endif
183500000000     C*
183600000000     C* Intestazione colonne
183700141230     C                   WRITE     SJ52T23
183800000000     C*
183900000000     C                   Eval      *In43       = *Off
184000000000     C                   Eval      $PrimaStp3  = *Off
184100000000     C                   endif
184200000000     C*
184300000000     C                   ENDSR
184400000000     C/EJECT
184500000000     C********************************************************************
184600141230     C* Stampa dati fiscali non valorizzati (SDGSJ52P3)
184700000000     C********************************************************************
184800000000     C     StpNoFisc     BEGSR
184900000000     C*
185000000000     C                   EXSR      StpNoFisc_Inz
185100000000     C*
185200141230     C                   Clear                   SJ52P301
185300150108     C*
185400150108     C                   CALLB     'XCAPCLIFOR'
185500150108     C                   PARM                    XSCSOC
185600150108     C                   PARM      'F     '      WKcc
185700150108     C                   PARM      DICKsc        WKsc
185800150108     C*
185900150108     C                   Eval      $Data       = DICDtInter
186000000000     C                   EXSR      CallMVC002
186100150108     C*
186200150108     C                   MoveL     DICKsc        P301Ksc
186300150108     C*
186400000000     C                   MoveL     Sogg002       P301Sogg
186500000000     C*
186600000000     C                   Move      Descr002      P301Descr
186700000000     C*
186800150108     C                   if        DICDtInter <> *loval
186900150108     C     *JobRun       Move      DICDtInter    P301DtInte
187000000000     C                   endif
187100000000     C*
187200000000     C                   MoveL     $PartIVA      P301PartIV
187300000000     C                   MoveL     $CdFisc       P301CdFisc
187400000000     C*
187500141230     C                   WRITE     SJ52P301
187600000000     C*
187700000000     C                   ENDSR
187800000000     C/EJECT
187900000000     C************************************************************
188000000000     C* DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
188100000000     C************************************************************
188200000000     C     DefCam        BEGSR
188300000000     C*
188400130826     C*
188500150108     C*  SDGDIE01L
188600150108     C     K03DIE01      KLIST
188700150108     C                   KFLD                    DIESocieta
188800150108     C                   KFLD                    DIEDtInter
188900150108     C                   KFLD                    DIENrInter
189000150112     C*
189100150112     C*
189200150112     C*  SDGDIF01L
189300150112     C     K04DIF01      KLIST
189400150112     C                   KFLD                    DIFSocieta
189500150112     C                   KFLD                    DIFCF
189600150112     C                   KFLD                    DIFDtInter
189700150112     C                   KFLD                    DIFNrInter
189800130826     C*
189900000000     C*
190000000000     C* SDGSO301L
190101160530R224 X**** K04SO301      KLIST
190102160530R224 C     K05SO301      KLIST
190200000000     C                   KFLD                    SO3Sogg
190300000000     C                   KFLD                    SO3DtFiVL
190400000000     C                   KFLD                    SO3Sys
190500000000     C                   KFLD                    SO3NrAsint
190501160530R224 C                   KFLD                    SO3Dest
190600000000     C*
190700000000     C                   ENDSR
190800000000     C/EJECT
