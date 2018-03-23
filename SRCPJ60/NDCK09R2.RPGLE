000100020905     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200020905     H*PARMS BNDDIR(PJXBND PJCBND)
000300020906     H*PARMS CVTOPT(*NONE)
000400020905     H DECEDIT('0,') DATEDIT(*DMY.)
000500020905     F*--------------------------------------------------------------*
000600020905     F*            RICEZIONE MOVIMENTI PORTAFOGLIO PASSIVO           *
000700020905     F*--------------------------------------------------------------*
000800020906     F*
000900020905     F*----------
001000020906     FNDRI200F  O    E           K DISK
001100020905     FANSIF03L  IF   E           K DISK    RENAME(ANSIF000:ANSIF03)
001200020917     FNDCSR03L  IF   E           K DISK
001300020918     FCBIAVV    UF   F  120        DISK
001400020916     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN11)
001500020917     D*-------------
001600020917     D* Prototipazione delle procedure Euro
001700020917     D/COPY *LIBL/SRCCPY,PJX002PR
001800020905     D*----------
001900020906     D* Passaggio Parametri
002000020905     D KPJBA         E DS
002100020905     D NDCK09DS      E DS
002200020906     D*-------------
002300020906     D* Dati di ambiente ottenuti da XSOC
002400020906     D SOC001        E DS                  EXTNAME(XSOC001DS)
002500020906     D*-------------
002600020906     D* DS Interna per dati di output di XSOC
002700020906     D XSOCDS          DS          1000
002800020905     D*----------
002900020906     D* Reperimento nome PGM
003000020905     D STATUS         SDS           333
003100020905     D  DSPGM            *PROC
003200020905     D  PARMS            *PARMS
003300020918     D*-------------
003400020918     D* Parametri per XPAR
003500020918     D  XPASOC         S              3
003600020918     D  XPAPAR         S              8
003700020918     D  XPAOUT         S           2000
003800020918     D  XPAERR         S              1
003900020918     D  XPARIC         S              1
004000020918     D*-------------
004100020918     D* Parametri gestione home banking eff.passivi
004200020918     D NdcK08DS      E DS
004300020916     D*-------------
004400020917     D* ds tracciato ANSIF00F
004500020916     D AnSifDS       E DS                  ExtName(ANSIF00F)
004600020916     D*----------
004700020916     D* controllo ABI/CAB
004800020916     D X59ABIDS      E DS                  Inz
004900020917     D*-------------
005000020917     D* Definizioni parametri standard chiamate moduli
005100020917     D Esito           S              1
005200020917     D NoCtrlScos      S              1
005300020917     D DatiGest        S              1
005400020905     D*----------
005500020916     D NumProg         S              7S 0
005600020917     D ImportD         S             13S 2
005700020916     D Abi             S              5A
005800020917     D Cab             S              5A
005900020916     D ProgCar         S                   Like(Ri2ProgCar)
006000020917     D ZDateISO        S               D
006100020916     D*----------
006200020917     D* Variabli di work
006300020916     D WIn0109         S              9A
006400020916     D WDat60          S              6S 0
006500020918     D WRecDS          DS           120
006600020905     D*----------
006700020916     D* Costante per controllo "caratteri solo numerici"
006800020916     D Digits          C                   '1234567890'
006900020905     I*------------------------------------------------------------*
007000020905     I* DESCRIZIONE DEL FILE                                       *
007100020905     I*------------------------------------------------------------*
007200020906     I* Inizio file AV
007300020916     ICBIAVV    AA  01    2 CA    3 CV
007400020905     I                                  4    8  MITAV
007500020906     I                                  9   13  SIAAV
007600020906     I                                 14   19  DTCAV
007700020905     I                                 20   39  NMSAV
007800020905     I                                 40   45  FI1AV
007900020905     I                                 46  113  FI2AV
008000020905     I                                114  114  CDIAV
008100020905     I                                115  115  FI3AV
008200020905     I                                116  120  ABPAV
008300020919     I                                  1  120  RECAV
008400020905     I* Tipo record 10
008500020905     I          BB  02    2 C1    3 C0
008600020906     I                                  4   10  NPR10
008700020905     I                                 11   16  FI110
008800020906     I                                 17   22  DTA10
008900020906     I                                 23   28  DTS10
009000020906     I                                 29   33  CAU10
009100020906     I                                 34   46  IML10
009200020905     I                                 47   47  SEG10
009300020906     I                                 48   52  ABI10
009400020906     I                                 53   57  CAB10
009500020905     I                                 58   77  FI210
009600020905     I                                 78   91  FI310
009700020905     I                                 92   96  COA10
009800020905     I                                 97  119  FI410
009900020905     I                                120  120  CDI10
010000020919     I                                  1  120  REC10
010100020906     I* Tipo record 20 (non gestito)
010200020905     I          CC  03    2 C2    3 C0
010300020919     I                                  1  120  REC20
010400020919     I* Tipo record 30
010500020905     I          DD  04    2 C3    3 C0
010600020919     I                                  4   10  NPR30
010700020919     I                                 11  100  DEC30
010800040923D1660I                                 11   50  DEC30S
010900020919     I                                  1  120  REC30
011000030506     I* Tipo record 50
011100020905     I          EE  05    2 C5    3 C0
011200030506     I                                  1  120  REC50
011300030506c1653I                                 11  100  RFO50
011400040217C1724I                                101  116  IVA50
011500020905     I* Tipo record 70
011600020905     I          FF  06    2 C7    3 C0
011700020906     I                                  4   10  NPR70
011800020905     I                                 11   15  FI170
011900020905     I                                 16   51  NRA70
012000020906     I                                 52   53  TPE70
012100020906     I                                 54   54  FLV70
012200020905     I                                 55  112  FI270
012300020905     I                                113  120  KCO70
012400020918     I                                  1  120  REC70
012500020906     I* Fine file EF (non gestito)
012600020905     I          GG  07    2 CE    3 CF
012700020919     I                                  1  120  RECEF
012800020905     I          ZZ  22
012900020905     I*------------------------------------------------------------*
013000020906     C*****************************************************************
013100020906     C* MAIN LINE PROGRAM                                             *
013200020906     C*****************************************************************
013300020906     C*
013400020906     C                   ExSr      InzVar
013500020906     C*
013600020906     C                   ExSr      ElabFile
013700020906     C*
013800020906     C                   ExSr      EndPgm
013900020906      /EJECT
014000020906     C************************************************************
014100020906     C* FINE PROGRAMMA
014200020906     C************************************************************
014300020906     C     ENDPGM        BEGSR
014400020906     C*
014500020906     C                   MOVEL     NDCK09DS      KPJBU
014600020906     C*
014700020906     C                   SETON                                            LR
014800020906     C                   RETURN
014900020906     C*
015000020906     C                   ENDSR
015100020906     C/EJECT
015200020906     C************************************************************
015300020906     C* ELABORAZIONE FILE
015400020906     C************************************************************
015500020906     C     ELABFILE      BEGSR
015600020916     C*
015700020916     C                   ExSr      RedCBI
015800020906     C*
015900020906     C     *In21         DoWEq     *Off
016000020906     C*
016100020906     C     *In01         CasEq     *On           GESRKAV
016200020906     C     *In02         CasEq     *On           GESRK10
016300020919     C     *In04         CasEq     *On           GESRK30
016400020906     C     *In06         CasEq     *On           GESRK70
016500020906     C                   EndCS
016600020916     C*
016700020916     C                   ExSr      RedCBI
016800020906     C*
016900020906     C                   EndDo
017000020906     C*
017100020906     C                   ENDSR
017200020906     C/EJECT
017300020916     C************************************************************
017400020916     C* LETTURA RECORD
017500020916     C************************************************************
017600020916     C     RedCBI        BEGSR
017700020916     C*
017800020916     C* resetto gli indicatori eventualmente accesi in precedenza
017900020916     C* (la nuova read non li spegne)
018000020916     C                   Move      *All'0'       WIn0109
018100020916     C                   MoveA     WIn0109       *In(01)
018200020916     C*
018300020916     C                   Read      CBIAVV                                 21
018400020916     C*
018500020916     C                   ENDSR
018600020916     C/EJECT
018700020906     C************************************************************
018800020906     C* GESTIONE RECORD "AV"
018900020906     C************************************************************
019000020906     C     GESRKAV       BEGSR
019100080213D2345C                   move      *off          ErrAV1            1
019200020906     C*
019300020916     C* tramite Cod.SIA reperisco la Società
019400020916     C                   Movel     SiaAV         SifCdSIA
019500020916     C     SifCdSIA      Chain     AnSIF03L                           21
019600020916     C     *In21         IfEq      *On
019700020916     C                   Clear                   AnSifDS
019800080213D2345C                   move      *on           ErrAV1
019900080213  "  x** 11              Except    Testa
020000080213  "  x** 11              SetOff                                       11
020100080213D2345x**                 Except    Riga1
020200020916     C                   EndIf
020300020906     C*
020400020906     C                   ENDSR
020500020906     C/EJECT
020600020906     C************************************************************
020700020906     C* GESTIONE RECORD "10"
020800020906     C************************************************************
020900020906     C     GESRK10       BEGSR
021000080213D2345C                   move      *off          Err101            1
021100080213D2345C                   move      *off          Err102            1
021200080213D2345C                   move      *off          Err103            1
021300020918     C*
021400020918     C* con il record '10' ' inizio un avviso
021500020918     C                   ExSr      InzAvv
021600040923     C*
021700040923D1660C* controllo numericità importo (spostate qui le specifiche)
021800040923  "  C     Digits        Check     IML10                                  21
021900040923  "  C                   If        *In21 = *Off
022000040923  "  C                   Move      IML10         ImportD
022100040923  "  C                   Else
022200040923  "  C                   Z-Add     0             ImportD
022300040923D1660C                   EndIf
022400020916     C*
022500020916     C* controllo abi
022600020916     C                   Reset                   X59AbiDs
022700020916     C                   Movel     *Blank        X59Soc
022800020916     C                   Movel     Abi10         X59Abi
022900020916     C                   Call      'X59ABI'
023000020916     C                   Parm                    X59AbiDs
023100020916     C                   If        X59Err  = '0'
023200020916     C                   Move      X59Abi        Abi
023300020916     C                   Else
023400080213D2345C                   move      *on           Err101
023500080213  "  x** 11              Except    Testa
023600080213  "  x** 11              SetOff                                       11
023700080213D2345x**                 Except    Riga2
023800020916     C                   EndIf
023900020917     C*
024000020917     C* cab
024100020917     C                   Movel     Cab10         Cab
024200020916     C*
024300020916     C* tramite abi/cab reperisco banca/linea associate
024400021029     c                   clear                   OkAbi             1
024500040921D1660c                   clear                   CsrBanca
024600040921D1660c                   clear                   CsrLinea
024700050610C1843C                   move      SifSocieta    CsrSocieta
024800020917     C                   Movel     Abi           CsrABI
024900020917     C                   Movel     Cab           CsrCAB
025000040927D1660X**   K02Csr03      setll     NdCsr03L
025100050610C1843X**   K02Csr03      setll     NdCsr03L                               21
025200050610C1843c     K03Csr03      setll     NdCsr03L                               21
025300050610D1660C                   If        *in21 <> *on
025400080213D2345C                   move      *on           Err102
025500080213  "  X** 11              Except    Testa
025600080213  "  X** 11              SetOff                                       11
025700080213D2345X**                 Except    Riga6
025800080213D1660c                   endif
025900040927D1660C                   If        *in21  = *on
026000021029     c                   DO        *hival
026100050610C1843x**   K02Csr03      reade     NdCsr000                               21
026200050610C1843C     K03Csr03      reade     NdCsr000                               21
026300021029     c   21              leave
026400021029     c                   If        CsrAbiEfp=*on
026500021029     c                   eval      OkAbi=*on
026600021029     c                   leave
026700021029     C                   EndIf
026800021029     c                   enddo
026900021029     c                   if        OkAbi <> *on
027000080213D2345C                   move      *on           Err103
027100080213  "  x***11              Except    Testa
027200080213D2345x***11              SetOff                                       11
027300040927D1660x******             Except    Riga6
027400080213D2345x******             Except    Riga6A
027500021029     c                   endif
027600040927D1660c                   endif
027700020916     C*
027800020916     C* controllo numericità progressivo
027900020916     C     Digits        Check     NPR10                                  21
028000020916     C                   If        *In21 = *Off
028100020916     C                   Move      NPR10         NumProg
028200020916     C                   Else
028300020916     C                   Z-Add     0             NumProg
028400020916     C                   EndIf
028500020916     C*
028600020906     C                   ENDSR
028700020906     C/EJECT
028800020919     C************************************************************
028900020919     C* GESTIONE RECORD "30"
029000020919     C************************************************************
029100020919     C     GESRK30       BEGSR
029200020919     C*
029300020919     C*
029400020919     C                   ENDSR
029500040217     C/EJECT
029600020906     C************************************************************
029700020906     C* GESTIONE RECORD "70"
029800020906     C************************************************************
029900020906     C     GESRK70       BEGSR
030000020918     C*
030100080213D2345C* stampo prima tutte le segnalazioni di errore. se sono arrivata
030200080213  "  c* qui significa che i dati letti non sono stati passati su ndri2
030300080213  "  c* perchè c'è il record 70 e allora stampo gli errori
030400080213  "  C* con il record '70' termina un avviso
030500080213D2345c                   Exsr      StampaErr
030600080213     c*
030700020918     C                   ExSr      FinAvv
030800020906     C*
030900020906     C                   ENDSR
031000020906     C/EJECT
031100020918     C************************************************************
031200020918     C* OPERAZIONI INIZIALI AVVISO
031300020918     C************************************************************
031400020918     C     InzAvv        BEGSR
031500020918     C*
031600020918     C                   Clear                   NumProg
031700020918     C                   Clear                   ImportD
031800020918     C                   Clear                   Abi
031900020918     C                   Clear                   Cab
032000020918     C*
032100020918     C                   ENDSR
032200020918     C/EJECT
032300080213D2345C************************************************************
032400080213     C* Stampa segnalazioni di errore
032500080213D2345C************************************************************
032600080213     C     StampaErr     BEGSR
032700080213     C* errore record AV
032800080213     c                   if        ErrAV1 = *on
032900080213     C   11              Except    Testa
033000080213     C   11              SetOff                                       11
033100080213     C                   Except    Riga1
033200080213     c                   move      *off          ErrAV1
033300080213     c                   endif
033400080213     c*
033500080213     C* errori record 10
033600080213     c                   if        Err101 = *on
033700080213     C   11              Except    Testa
033800080213     C   11              SetOff                                       11
033900080213     C                   Except    Riga2
034000080213     c                   move      *off          Err101
034100080213     c                   endif
034200080213     c                   if        Err102 = *on
034300080213     C   11              Except    Testa
034400080213     C   11              SetOff                                       11
034500080213     C                   Except    Riga6
034600080213     c                   move      *off          Err102
034700080213     c                   endif
034800080213     c                   if        Err103 = *on
034900080213     C   11              Except    Testa
035000080213     C   11              SetOff                                       11
035100080213     c                   Except    Riga6A
035200080213     c                   move      *off          Err103
035300080213     c                   endif
035400080213     C*
035500080213D2345C                   ENDSR
035600020918     C************************************************************
035700020918     C* OPERAZIONI FINALI AVVISO
035800020918     C************************************************************
035900020918     C     FinAvv        BEGSR
036000020918     C*
036100020918     C* riempio il record con i dati reperiti
036200020918     C                   ExSr      RieRI2
036300020918     C*
036400020918     C* se ho i dati essenziali valorizzati
036500020918     C     RI2SIA        IfNE      *Zeros
036600020918     C     RI2SIA        AndNE     *Blanks
036700020918     C     RI2Societa    AndNE     *Blanks
036800020918     C     RI2Abi        AndNE     *Blanks
036900020918     C     RI2Cab        AndNE     *Blanks
037000020918     C     RI2DtSca      AndNE     *Loval
037100020918     C     RI2ImportD    AndNE     0
037200020918     C     RI2Importo    AndNE     0
037300040921D1660x***  RI2Banca      AndNE     *Blanks
037400040921D1660x***  RI2Linea      AndNE     *Blanks
037500020918     C* scrivo il record di NDRI200F
037600020918     C                   Add       1             ProgCar
037700020918     C                   Z-Add     ProgCar       RI2PROGCAR
037800020918     C                   Write     NDRI2000
037900020918     C* cancello il riferimento '70' dal record in modo che se per
038000020918     C* errore dovesse essere richiesta una nuova ricezione del
038100020918     C* file, i dati non vengano nuovamente importati
038200020918     C                   Eval      %SubSt(Rec70: 2: 2) = '--'
038300020918     C                   Move      Rec70         WRecDS
038400020918     C                   Update    CbiAvv        WRecDS
038500020918     C                   EndIf
038600020918     C*
038700020918     C                   ENDSR
038800020918     C/EJECT
038900020906     C************************************************************
039000020906     C* RIEMPIMENTO RECORD RI2
039100020906     C************************************************************
039200020906     C     RIERI2        BEGSR
039300020916     C*
039400020916     C                   Clear                   NdRi2000
039500020906     C*
039600020916     C                   Z-Add     NRCK09        RI2NRCAR
039700020906     C*
039800020916     C                   Movel     SifSocieta    RI2SOCIETA
039900020916     C*
040000020916     C                   Movel     Abi           RI2ABI
040100020917     C                   Movel     Cab           RI2CAB
040200020916     C                   Movel     SifCdSIA      RI2SIA
040300020917     C                   Movel     CsrBanca      RI2BANCA
040400020917     C                   Movel     CsrLinea      RI2LINEA
040500020916     C                   Movel     ZDateIso      RI2DTCAR
040600020916     C*
040700020916     C* Flusso = 'EFP' + le ultime 7 cifre del carico
040800020916     C                   Move      NrCK09        RI2FLUSSO
040900020916     C                   Movel     'EFP'         RI2FLUSSO
041000020916     C*
041100020916     C                   Move      DTCAV         WDat60
041200020916     C     *Dmy          Test(d)                 WDat60                 21
041300020916     C     *In21         IfEq      *Off
041400020916     C     *Dmy          Movel     WDat60        RI2DTCRE
041500020916     C                   EndIf
041600020916     C                   Move      DTA10         WDat60
041700020916     C     *Dmy          Test(d)                 WDat60                 21
041800020916     C     *In21         IfEq      *Off
041900040921     C     *Dmy          Movel     WDat60        RI2DTAVV
042000040921D1660C                   EndIf
042100090504>>>>>C*                  Move      DTS10         WDat60
042200040921D1660X***                EndIf
042300090504>>>>>C                   Move      DTS10         WDat60
042400020916     C     *Dmy          Test(d)                 WDat60                 21
042500020916     C     *In21         IfEq      *Off
042600020916     C     *Dmy          Movel     WDat60        RI2DTSCA
042700020917     C                   Else
042800020917     C   11              Except    Testa
042900020917     C   11              SetOff                                       11
043000020917     C                   Except    Riga3
043100020916     C                   EndIf
043200020916     C*
043300020918     C                   Movel     DivisaK08     RI2DIVISA
043400020917   b1C     ImportD       IfNE      0
043500020917     C                   Z-Add     ImportD       RI2IMPORTD
043600020917   b2C                   If        RI2Divisa <> XScDiv
043700020917     C                   Reset                   PJX00202DS
043800020917     C                   Eval      ImporD0202  = Ri2ImportD
043900020917     C                   Eval      Divisa0202  = Ri2Divisa
044000040921D1660X***                Eval      DtRif0202   = Ri2DtAvv
044100040921D1660C                   Eval      DtRif0202   = Ri2DtCar
044200020917     C                   ExSr      RepImpMC
044300020917   b3C                   If        Esito       = *Off
044400020917     C                   Eval      Ri2Importo  = ImporM0202
044500020917   x3C                   Else
044600020917     C                   Z-Add     0             RI2IMPORTO
044700020917     C   11              Except    Testa
044800020917     C   11              SetOff                                       11
044900020917     C                   Except    Riga5
045000020917   e3C                   EndIf
045100020917     C                   Else
045200020917     C                   Z-Add     ImportD       RI2IMPORTO
045300020917   e2C                   EndIf
045400020917   x1C                   Else
045500020917     C                   Z-Add     0             RI2IMPORTD
045600020917     C                   Z-Add     0             RI2IMPORTO
045700020917     C   11              Except    Testa
045800020917     C   11              SetOff                                       11
045900020917     C                   Except    Riga4
046000020917   e1C                   EndIf
046100020917     C*
046200020916     C                   Movel     Seg10         RI2SEGNO
046300020916     C*
046400020916     C                   Movel     NMSAV         RI2SUPPORT
046500020916     C                   Z-Add     NumProg       RI2NUMPROG
046600020919     C                   Movel     DEC30         RI2DESCR
046700030506C1653c                   move      Rfo50         RI2RIFOP
046800040217C1724c                   move      IVA50         RI2CDFIVA
046900020916     C                   Movel     NRA70         RI2NUMAVV
047000020916     C                   Movel     TPE70         RI2TPEFF
047100020916     C                   Movel     FLV70         RI2VISTA
047200020916     C*
047300020916     C                   Movel     *Loval        RI2DTRIC
047400020916     C                   Movel     *Off          RI2AVVISA
047500020916     C                   Z-Add     0             RI2SYS
047600020916     C                   Z-Add     0             RI2NRASREG
047700020916     C                   Z-Add     0             RI2NREFF
047800020916     C                   Movel     *Blanks       RI2FILLER
047900020916     C                   Movel     *Loval        RI2DATA
048000020906     C*
048100020906     C                   ENDSR
048200020906     C/EJECT
048300020917     C*********************************************************
048400020917     C* Reperisco il controvalore in m.c.
048500020917     C*********************************************************
048600020917     C     RepImpMC      BEGSR
048700020917     C*
048800020917     C                   Eval      DivMC0202   = XScDiv
048900020917     C                   Eval      NoCtrlScos  = *On
049000020917     C                   Eval      DatiGest    = *Off
049100020917     C                   CallP     X0202TestC(PJX00202DS :
049200020917     C                                        %Size(PJX00202DS) :
049300020917     C                                        *Off : Esito :
049400020917     C                                        NoCtrlScos : DatiGest)
049500020917     C*
049600020917     C                   ENDSR
049700020917     C/EJECT
049800020906     C************************************************************
049900020906     C* OPERAZIONI INIZIALI
050000020906     C************************************************************
050100020906     C     *INZSR        BEGSR
050200020906     C*
050300020906     C     *ENTRY        PLIST
050400020906     C                   PARM                    KPJBA
050500020906     C*
050600020906     C     *DMY          Move      Udate         ZDateISO
050700020906     C     *JobRun       Move      ZDateISO      ZDate             6 0
050800020916     C*
050900020916     C                   Time                    Ora               6 0
051000020906     C*
051100020906     C                   ENDSR
051200020906     C/EJECT
051300020906     C************************************************************
051400020906     C* INIZIALIZZAZIONE VARIABILI
051500020906     C************************************************************
051600020906     C     INZVAR        BEGSR
051700020906     C*
051800020916     C* dati da PGM chiamante
051900020906     C                   MOVEL     KPJBU         NDCK09DS
052000020906     C*
052100020916     C* reperimento società
052200020906     C                   MOVEL     'SOC001'      TIPXSC
052300020906     C                   MOVEL     SocK09        SOCXSC
052400020906     C                   EXSR      REPSOC
052500020906     C     RTNXSC        IFNE      '1'
052600020906     C                   MOVEL     XSOCDS        SOC001
052700020906     C                   ELSE
052800020906     C                   EXSR      ENDPGM
052900020906     C                   ENDIF
053000020918     C*
053100020918     C* reperimento parametri gestione modulo home banking Eff. Passivi
053200020918     C*
053300020918     C                   MOVEL     *BLANK        XPAERR
053400020918     C                   MOVEL     *BLANK        XPARIC
053500020918     C                   MOVEL     XScSoc        XPASOC
053600020918     C                   MOVEL(P)  'HBEFFPAS'    XPAPAR
053700020918     C                   MOVEL     *BLANK        XPAOUT
053800020918     C                   EXSR      REPPAR
053900020918     C     XPAERR        IFEQ      '0'
054000020918     C                   MOVEL     XPAOUT        NDCK08DS
054100020918     C                   ELSE
054200020918     C                   SETON                                        LR
054300020918     C                   RETURN
054400020918     C                   ENDIF
054500020916     C*
054600020916     C* valorizzazione campi univoci testate
054700020916     C                   MOVEL     KNSIF         NOMSIF           10
054800020916     C                   MOVEL     XSCDSI        NOMDIT           20
054900020916     C                   MOVEL     DSPGM         NOMPGM           10
055000020916     C* reperisco nome sistema informatico
055100020916     C                   CALL      'XNETA'                              21
055200020916     C                   PARM                    NOMSYS            8
055300020918     C*
055400020918     C* accensione indicatore di overflow per stampa prima testata
055500020918     C                   SetOn                                        11
055600020906     C*
055700020906     C* inizializzo il progressivo carico
055800020906     C                   Z-Add     0             ProgCar
055900020906     C*
056000020906     C                   ENDSR
056100020906     C/EJECT
056200020918     C************************************************************
056300020918     C* REPERIMENTO PARAMETRI GESTIONE
056400020918     C************************************************************
056500020918     C     REPPAR        BEGSR
056600020918     C*
056700020918     C                   CALLB     'XPAR'
056800020918     C                   PARM                    XPASOC
056900020918     C                   PARM                    XPAPAR
057000020918     C                   PARM                    XPAOUT
057100020918     C                   PARM                    XPAERR
057200020918     C                   PARM                    XPARIC
057300020918     C*
057400020918     C                   ENDSR
057500020918     C/EJECT
057600020917     C************************************************************
057700020917     C* DEFINIZIONE KLIST
057800020917     C************************************************************
057900020917     C     DefKList      BEGSR
058000020917     C*
058100050610C1843X***  K02Csr03      KList
058200050610C1843C     K03Csr03      KList
058300020917     C                   KFld                    CsrABI
058400020917     C                   KFld                    CsrCAB
058500050610C1843C                   KFld                    CsrSocieta
058600020917     C*
058700020917     C                   ENDSR
058800020917     C/EJECT
058900020906     C************************************************************
059000020906     C* REPERIMENTO DATI SOCIETA'
059100020906     C************************************************************
059200020906     C     REPSOC        BEGSR
059300020906     C*
059400020906     C                   CALLB     'XSOC'
059500020906     C                   PARM                    TIPXSC            6
059600020906     C                   PARM                    SOCXSC            3
059700020906     C                   PARM                    CDSXSC            9 0
059800020906     C                   PARM                    MODXSC            3
059900020906     C                   PARM      *BLANKS       RTNXSC            1
060000020906     C                   PARM                    XSOCDS
060100020906     C                   PARM                    KPJBA
060200020906     C*
060300020906     C                   ENDSR
060400020906     C/EJECT
060500020916      *
060600020916     OQSYSPRT   E            TESTA            02
060700020916     O                       NOMDIT              21
060800020916     O                                           35 '***---'
060900020916     O                                         +  0 '------------------------'
061000020916     O                                         +  0 '------------------------'
061100020916     O                                         +  0 '--***'
061200020916     O                       KNMUS              110
061300020916     O                       ZDATE         Y    120
061400020916     O                                          126 'Pag:'
061500020916     O                       PAGE          Z    131
061600020916     O          E            TESTA            03
061700020916     O                       NOMSYS               9                             000010
061800020916     O                       NOMSIF              20                             000010
061900020916     O                                           30 '*'
062000020916     O                                         + 11 ' LISTA ERRORI AVVISATURA'
062100020916     O                                         +  1 'EFFETTI '
062200020916     O                                         + 13 '*'
062300020916     O                       NOMPGM             110
062400020916     O                       ORA                120 '  :  :  '
062500020916     O          E            TESTA          2 04
062600020916     O                                           35 '***---'
062700020916     O                                         +  0 '------------------------'
062800020916     O                                         +  0 '------------------------'
062900020916     O                                         +  0 '--***'
063000020916     O                                          110 'gruppo PRO'
063100020916     O          E            RIGA1          1
063200020917     O                                         +  1 'ERRORE - Codice SIA:'
063300020916     O                       SIAAV             +  1
063400020917     O                                         +  5 'non esiste in'
063500020917     O                                         +  1 'Anagrafico Società'''
063600020916     O          E            RIGA2          1
063700020917     O                                         +  1 'ERRORE - Codice ABI:'
063800020916     O                       ABI10             +  1
063900020917     O                                         +  5 'non esiste in tabella'
064000020917     O          E            RIGA3          1
064100020917     O                                         +  1 'ERRORE - Data scadenza:'
064200020917     O                                         +  5 'non valorizzata'
064300040923D1660O                                         +  5 'importo'
064400040923  "  O                       IMPORTD       2   +  1
064500040923  "  O                                         +  2 'forn.'
064600040923D1660O                       DEC30S            +  1
064700020917     O          E            RIGA4          1
064800020917     O                                         +  1 'ERRORE - Importo'
064900020917     O                                         +  5 'non valorizzato'
065000040923D1660O                                         +  5 'importo'
065100040923  "  O                       IMPORTD       2   +  1
065200040923  "  O                                         +  2 'forn.'
065300040923D1660O                       DEC30S            +  1
065400020917     O          E            RIGA5          1
065500040923     O                                         +  1 'ERRORE-Per divisa:'
065600020917     O                       RI2DIVISA         +  1
065700020917     O                                         +  5 'non reperito controvalore'
065800040923D1660O                                         +  5 'importo'
065900040923  "  O                       IMPORTD       2   +  1
066000040923  "  O                                         +  2 'forn.'
066100040923D1660O                       DEC30S            +  1
066200020917     O          E            RIGA6          1
066300020917     O                                         +  1 'ERRORE - per ABI/CAB:'
066400020917     O                       ABI10             +  1
066500020917     O                       CAB10             +  1
066600020917     O                                         +  5 'manca associazione banca'
066700040923D1660O                                         +  5 'importo'
066800040927  "  O                       IMPORTD       2   +  1
066900040927  "  O          E            RIGA6A         1
067000040927  "  O                                         +  1 'ERRORE - per ABI/CAB:'
067100040927  "  O                       ABI10             +  1
067200040927  "  O                       CAB10             +  1
067300040927  "  O                                         +  5 'linea non abilitata'
067400040927  "  O                                         +  5 'importo'
067500040927D1660O                       IMPORTD       2   +  1
067600020916      /EJECT
