000100000000     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND SDG005) ACTGRP(*CALLER)
000200000000     H*PARMS OPTION(*NOXREF *NODEBUGIO)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      ********************************************************************
000500000000      *
000600000000      *         ------------------------------------------------
000700140328      *                    Acquisti di San Marino
000800140328      *                   Comunicazione Polivalente
000900140328      *                   Creazione file telematico
001000000000      *         ------------------------------------------------
001100000000      *
001200000000     F*----------------------------------------------------------------
001300000000     F* File contenente i dati da trasmettere all'agenzia
001400000000     FSDGETW00F UF A E             DISK
001500000000     F                                     InfDs(InfDsETW)
001600000000     FSDGETW01L IF   E           K DISK    RENAME(SDGETW000:SDGETW01)
001700000000     F*-------------
001800000000     FSDGETE00F O    E           K DISK    UsrOpn
001900000000     F*-------------
002000000000     F* Dati anagrafici aggiuntivi
002101160524R224 X**** Key: Sogg/DtFiVl/Sys/NrAsInt      UNIQUE
002102160524R224 F* Key: Sogg/DtFiVl/Sys/NrAsInt/Dest      UNIQUE
002200000000     FSDGSO301L IF   E           K DISK
002300000000     F*-------------
002400000000     FANSIF01L  IF   E           K DISK
002500000000     F*-------------
002600000000     F* Printer file errori
002700000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002800000000     D*-------------
002900000000     D* Prototipo per la procedura Euro
003000000000     D/COPY *LIBL/SRCCPY,PJX002PR
003100000000     D*-------------
003200000000     D InfDsETW        DS           512
003300000000     D  NrrEtw               397    400B 0
003400000000     D*-------------
003500000000     D* campo di comodo per calcolo importo EURO arrotondato
003600000000     D W30S15          S             30S15
003700000000     D*----------------
003800000000     D* Parametro di base
003900000000     D XPAOUT          DS          2000
004000000000     D*----------------
004100000000     D* PARAMETRI ESTERNI
004200000000     D KPJBA         E DS
004300000000     D*----------------
004400000000     D* Valori assunti
004500000000     D XMPEDS        E DS                  INZ
004600000000     D XMPEDS1       E DS                  INZ
004700000000     D DSInterm        DS                  INZ
004800000000     D $FisInt                             Like(G61BCFINT  )
004900000000     D $CafInt                        5A
005000000000     D $ImpTrs                        1A
005100000000     D $ImpDta                       10A
005200000000     D $FISSOG                       16A
005300000000     D $CARSOG                        2A
005400000000     D $DTISOG                       10A
005500000000     D $DTFSOG                       10A
005600000000     D $COGSOG                       24A
005700000000     D $NOMSOG                       20A
005800000000     D $SEXRSOG                       1A
005900000000     D $DTNSOG                       10A
006000000000     D $COMSOG                       40A
006100000000     D $PRVSOG                        2A
006200000000     D $RAGSOG                       24A
006201160609R224 D*-------------
006202160609 "   D DSContrib       DS                  INZ
006203160609R224 D $Dest                          2A
006300000000     D*---------
006400000000     D $Data           S               D
006500000000     D $Data10A1       S             10
006600000000     D $Data10A2       S             10
006700000000     D*---------
006800000000     D DataDs          DS
006900000000     D  AnnoDs                        4A
007000000000     D   Tratt1                       1A   Inz('-')
007100000000     D  MeseDs                        2A
007200000000     D   Tratt2                       1A   Inz('-')
007300000000     D  GiornoDs                      2A
007400000000     D*-------------
007500000000     D $CdFiscSog      S                   Like(SO3CdFisc  )
007600000000     D $CdIVASog       S                   Like(SO3PartIva )
007700000000     D $CogRagSog      S                   Like(SO3CogRag  )
007800000000     D $NomeSog        S                   Like(SO3Nome    )
007900000000     D $DtNascSog      S                   Like(SO3DtNasc  )
008000000000     D $LocNascSog     S                   Like(SO3LocNasc )
008100000000     D $PrvNascSog     S                   Like(SO3PrvNasc )
008200000000     D $StatoSog       S                   Like(SO3Stato   )
008300000000     D $StaFedSog      S                   Like(SO3StaFed  )
008400000000     D $LocalitSog     S                   Like(SO3Localit )
008500000000     D $IndirESog      S                   Like(SO3IndirE  )
008600000000     D $SexSog         S                   Like(SO3Sex     )
008700000000     D*----------------
008800000000     D* Parametri ricevuti
008900140328     D SDGSG64DS     E DS
008901160208R195 D*-------------
008902160208 "   D* Passaggio Parametri al PGM di stampa di lavoro
008903160208 "   D SDGSG61DS     E DS
008904160208R195 D  ERRG61       E                     INZ('0')
009000000000     D*-------------
009100000000     D* Ds tracciati appoggio
009200000000     D SDGSG61DSA    E DS
009300000000     D SDGSG61DSB    E DS
009400000000     D SDGSG61DSC    E DS
009500000000     D SDGSG61DSD    E DS
009600000000     D SDGSG61DSE    E DS
009700000000     D SDGSG61DSZ    E DS
009800000000     D*-------------
009900000000     D* Ds tracciati record
010000000000     D SDG005BLDS    E DS
010100000000     D SDG005DFDS    E DS
010200000000     D SDG005FADS    E DS
010300000000     D SDG005FEDS    E DS
010400000000     D SDG005FNDS    E DS
010500000000     D SDG005FRDS    E DS
010600000000     D SDG005NEDS    E DS
010700000000     D SDG005NRDS    E DS
010800000000     D SDG005SADS    E DS
010900000000     D SDG005SEDS    E DS
011000140328     D SDG005TUDS    E DS
011100140328     D SDG005TADS    E DS
011200000000     D*-------------
011300000000     D* Parametri modulo SDGSPECF
011400000000     D SDGPA08DS     E DS
011500000000     D*-------------
011600000000     D* Dati di ambiente ottenuti da SDGSOCDR
011700000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
011800000000     D*-------------
011900000000     D* DS Interna per dati di output di SDGSOCDR
012000000000     D XSOCDS          S           1000
012100000000     D*----------------
012200000000     D* Reperimento nome PGM
012300000000     D STATUS         SDS           333
012400000000     D  DSPGM            *PROC
012500000000     D*----------------
012600000000     D* DS modulo dati
012700000000     D SDG00502DS    E DS                  INZ
012800000000     D*
012900000000     D SDG00592DS    E DS                  INZ
013000000000     D*----------------
013100000000     D* Richiamo a SDGTABDR
013200000000     D SDGTABDS      E DS                  inz
013300000000     D*----------------
013400000000     D* tabelle delle nature giuridiche
013500000000     D ANGG16DS      E DS                  INZ
013600000000     D*-------------
013700000000     D* Stati
013800000000     D ANGG12DS      E DS                                    INZ
013900000000     D*------------
014000000000     D* tabella stati BL
014100000000     D SDGHD1DS      E DS                  INZ
014200000000     D*------------
014300000000     D* Immagine SDGETW00F
014400000000     D SDGETWDS      E DS                  ExtName(SDGETW00F)
014500000000     D*----------------
014600000000     D* campi di lavoro
014700000000     D $NrRec          S              9S 0 Inz(*Zero)
014800000000     D $NrRecTot       S              9S 0 Inz(*Zero)
014900000000     D $TpRec          S              1A
015000000000     D DataISO         S               D
015100000000     D TipRec          S              1A
015200000000     D*----------------
015300000000     D Importo19_3     S             19S 3
015400000000     D Importo16_0     S             16S 0
015500000000     D Importo16A      S             16A
015600000000     D Importo20_0     S             20S 0
015700000000     D Importo20A      S             20A
015800000000     D Importo0A       S             16A   Inz('               0')
015900000000     D*-------------
016000000000     D UDateISO        S               D
016100000000     D $TpErrore       S              2A
016200000000     D $Errori         S              1A   Inz('0')
016300000000     D $FineETW        S              1A   Inz('0')
016400000000     D $FineSCF        S              1A   Inz('0')
016500000000     D $FineEP3        S              1A   Inz('0')
016600000000     D NrEPW           S              9S 0
016700000000     D Errore          S              1A
016800000000     D $FlgNeg         S              1A   Inz('0')
016900000000     D*-------------
017000000000     D WrKNatGiu       S                   Like(XscNcr    )
017100000000     D UICSocieta      S                   Like(CODUIC0592)
017200000000     D WrkStato        S                   Like(STATO0592 )
017300000000     D WSavQua         S                   Like(Quadro0592)
017400000000     D*----------------
017500000000     D* Ds flag AnSif
017600000000     D                 DS
017700000000     D  SocSogg               31     38
017800000000     D  WrkFlgOpe              1     50
017900000000     D*----------------
018000000000     D* Ds per nome file
018100000000     D                 DS
018200000000     D  WAnno                  5      8  0
018300000000     D  WPrgInvio              9     10  0
018400000000     D  WNumero                1     10
018500000000     D*----------------
018600000000     D* variabili per invio multiplo
018700000000     D $MultiInvio     S              1A   Inz(*Off)
018800000000     D $MaxRighe       S              6S 0 Inz(0)
018900000000     D $NrInvii        S              5S 0 Inz(0)
019000000000     D $ProgrInvio     S              5S 0 Inz(0)
019100000000     D $KByte          C                   Const(1024)
019200000000     D LenEPW          S              9B 0
019300000000     D*-------------
019400000000     D WImpInp         S                   Like(ImporD0202)
019500000000     D WImpOut         S                   Like(ImporD0202)
019600000000     D*-------------
019700000000     D DtAMG           DS
019800000000     D  AMGAnno                1      4
019900000000     D  T1                     5      5
020000000000     D  AMGMese                6      7
020100000000     D  T2                     8      8
020200000000     D  AMGGiorno              9     10
020300000000     D*-------------
020400000000     D DtGMA           DS
020500000000     D  GMAGiorno              1      2  0
020600000000     D  GMAMese                3      4  0
020700000000     D  GMAAnno                5      8  0
020800000000     D*-------------
020900000000     D WrkDt           S              8A
021000000000     D WrkProgMod      S              7S 0 Inz(*Zero)
021100000000     D*---------------------------------------------------------------*
021200000000     D DatMin          C                   CONST('0001-01-01')
021300140401     D CostFile        C                   CONST('Acq.San Marino')
021400000000     D*----------------
021500000000     D* Nr moduli
021600000000     D NrModBL         S              7S 0 Inz(*Zero)
021700000000     D NrModSE         S              7S 0 Inz(*Zero)
021800000000     D*----------------
021900000000     D NrETW           S              7S 0 Inz(*Zero)
022000000000     D*
022100000000     D TotRec          S              7S 0 Inz(*Zero)
022200000000     D*
022300000000     D Cnt_C           S              7S 0 Inz(*Zero)
022400000000     D Cnt_D           S              7S 0 Inz(*Zero)
022500000000     D Cnt_Righe       S              7S 0 Inz(*Zero)
022600000000     D*
022700000000     D CntBL           S              7S 0 Inz(*Zero)
022800000000     D CntSE           S              7S 0 Inz(*Zero)
022900000000     D Prog_BL         S              3S 0 Inz(*Zero)
023000000000     D Prog_SE         S              3S 0 Inz(*Zero)
023100140328     D ProgMod         S              7S 0 Inz(*Zero)
023200000000     D ProgMod_BL      S              3S 0 Inz(*Zero)
023300000000     D ProgMod_SE      S              3S 0 Inz(*Zero)
023400000000     D*----------------
023500140328     D WrkImport01     S              9S 0 Inz(*Zero)
023600140328     D WrkImport02     S              9S 0 Inz(*Zero)
023700140328     D WrkImport03     S              9S 0 Inz(*Zero)
023800140328     D WrkImport04     S              9S 0 Inz(*Zero)
023900140328     D WrkImport05     S              9S 0 Inz(*Zero)
024000140328     D WrkImport06     S              9S 0 Inz(*Zero)
024100140328     D WrkImport07     S              9S 0 Inz(*Zero)
024200140328     D WrkImport08     S              9S 0 Inz(*Zero)
024300140328     D WrkImport09     S              9S 0 Inz(*Zero)
024400140328     D WrkImport10     S              9S 0 Inz(*Zero)
024500140328     D WrkTotale       S              9S 0 Inz(*Zero)
024600140328     D*----------------
024700140328     D* indentificativi campo
024800140328     D Conta_Rigo      S              3S 0 Inz(*Zero)
024900140328     D Conta_Colo      S              3S 0 Inz(*Zero)
025000140328     D*--
025100000000     D* tabella per i dati NON posizionali dei record 1 e 2 (1680 caratt.)
025200000000     D Max$I           C                   CONST(100)
025300000000     D Skiera          S             24    DIM(Max$I)
025400000000     D $I              S              3S 0
025500000000     D*--
025600000000     D Sk_Campo        S              1    DIM(Max$X)
025700000000     D Max$X           C                   CONST(61)
025800000000     D $X              S              3S 0
025900000000     D*--
026000000000     D* ogni elemento della tabella Skiera è costituito da:
026100000000     D* - 1 campo-codice di 8 caratteri;
026200000000     D* - 1 campo-valore di 16 caratteri.
026300000000     D*
026400000000     D                 DS
026500000000     D  Cod_Elenco             1      2
026600000000     D  Cod_Rigo               3      5  0
026700000000     D  Cod_Colo               6      8  0
026800000000     D  Campo_Codice           1      8
026900000000     D*
027000000000     D                 DS
027100000000     D  Campo_Valore           1     16
027200000000     D  Caratt_45             17     61
027300000000     D*
027400000000     D*----------------
027500000000     D* questa schiera serve per togliere gli zeri non significativi
027600000000     D* alle caselle che contengono degli importi dei campi posizionali
027700000000     D*
027800000000     D Max$Z           C                   CONST(16)
027900000000     D $Z              S              3S 0
028000000000     D*
028100000000     D Sk_Importo      S              1    DIM(Max$Z)
028200000000     D*----------------
028300000000     D                 DS
028400000000     D  Campo                  1     16
028500000000     D  C15I                  17     61
028600000000     D    C15                        15    Dim(03) Overlay(C15I)
028700000000     D  C60                    1     61
028800000000     D*---------------------------------------------------------------*
028900000000     C*  CICLO PRINCIPALE DEL PROGRAMMA                               *
029000000000     C*---------------------------------------------------------------*
029100000000     C*
029200000000     C                   Exsr      INZVAR
029300000000     C*
029400000000     C* Param.Modulo
029500000000     C                   Exsr      RepParMod
029600000000     C*
029700000000     C* Dati Rappresentante
029800000000     C                   Exsr      RepDatInter
029900000000     C*
030000000000     C* Azzeramento
030100000000     C                   Exsr      AzzeraProg
030200000000     C*
030300000000     C*-----------------------------------------------
030400000000     C* LETTURA DATI
030500000000     C*
030600000000     C* Inizializzo chiamata modulo di lettura
030700000000     C                   Exsr      Inz0502R
030800000000     C*
030900000000     C* Elaborazione dati per preparazione invio
031000000000     C                   Exsr      ElaboraDati
031100000000     C*
031200000000     C* Determino nr invii
031300000000     C                   Exsr      RepNrInvii
031400000000     C*
031500000000     C                   If        $NrInvii      > 1
031600000000     C                   Eval      $MultiInvio    = *On
031700000000     C                   ElSe
031800000000     C                   Eval      $MultiInvio    = *Off
031900000000     C                   EndIf
032000000000     C*
032100000000     C*-----------------------------------------------
032200000000     C*
032300000000     C* ELABORAZIONE DATI per SCRITTURA TELEMATICO ---
032400000000     C*
032500000000     C*
032600000000     C* Se ho record allora continuo
032700000000a1   C                   If        $NrRecTot     > 0
032800000000     C*
032900000000     C* Mi posiziono all'inizio di ETW
033000000000     C                   Eval      $FineETW    = *Off
033100000000     C                   Exsr      PosizETW
033200000000     C*
033300000000     C* Unico invio  o multi invio
033400000000a2   C                   If        $MultiInvio    = *Off
033500000000     C                   Z-Add     1             $ProgrInvio
033600000000     C                   Exsr      GesSingInv
033700000000x2   C                   ElSe
033800000000     C                   Exsr      GesMultInv
033900000000e2   C                   EndIf
034000000000     C*
034100000000     C*
034200000000e1   C                   EndIf
034300000000     C*
034400000000     C                   Exsr      EndPgm
034500000000     C*
034600000000     C******************************************************
034700000000     C* Elaboro dati di invio
034800000000     C******************************************************
034900000000     C     ElaboraDati   BegSr                                                  *
035000000000     C*
035100000000     C                   Exsr      LetturaSCF
035200000000     C*
035300000000     C                   Move      QUADRO0592    WSavQua
035400000000     C*
035500000000     C                   DOW       $FineSCF    = *Off
035600000000     C*
035700000000     C                   Exsr      OpeBreak
035800000000     C*
035900000000     C                   Exsr      ElaboraSCF
036000000000     C*
036100000000     C                   Exsr      LetturaSCF
036200000000     C*
036300000000     C                   EndDo                                                  *
036400000000     C*
036500000000     C                   Exsr      OpeBreak
036600000000     C*
036700000000     C                   Endsr
036800000000     C******************************************************
036900000000     C* Rottura
037000000000     C******************************************************
037100000000     C     OpeBreak      BegSr                                                  *
037200000000     C*
037300000000     C                   If        QUADRO0592    <> WSavQua    or
037400000000     C                             $FineSCF      =  '1'
037500000000     C*
037600000000     C                   Select
037700000000     C*
037800000000     C* Aggregato
037900140328     C****               When      WSavQua       =  'XX'  and
038000140328     C****                         Prog_XX > 0
038100140328     C****               EXSR      WrtDati
038200140328     C****               EXSR      InzSkiera
038300140328     C*
038400140328     C* Analitico
038500140328     C*
038600000000     C                   When      WSavQua       =  'SE'  and
038700000000     C                             Prog_SE > 0
038800000000     C                   EXSR      WrtDati
038900000000     C                   EXSR      InzSkiera
039000000000     C*
039100000000     C                   EndSl
039200000000     C*
039300000000     C                   Z-ADD     0             WRKPRGMOD         7 0
039400000000     C*
039500000000     C                   EndIf
039600000000     C*
039700000000     C                   Move      QUADRO0592    WSavQua
039800000000     C*
039900000000     C                   EndSr
040000000000     C******************************************************
040100000000     C* Ciclo dei dati estratti
040200000000     C******************************************************
040300000000     C     LetturaSCF    BegSr                                                  *
040400000000     C*
040500000000     C                   CALLb     'SDG00502R'
040600000000     C                   PARM                    KPJBA
040700000000     C                   PARM                    SDG00502DS
040800000000     C                   PARM                    SDG00592DS
040900000000     C                   PARM                    SDG005FADS
041000000000     C                   PARM                    SDG005SADS
041100000000     C                   PARM                    SDG005BLDS
041200000000     C                   PARM                    SDG005FEDS
041300000000     C                   PARM                    SDG005FRDS
041400000000     C                   PARM                    SDG005NEDS
041500000000     C                   PARM                    SDG005NRDS
041600000000     C                   PARM                    SDG005DFDS
041700000000     C                   PARM                    SDG005FNDS
041800000000     C                   PARM                    SDG005SEDS
041900000000R136 C                   PARM                    SDG005TUDS
042000000000     C                   PARM                    SDG005TADS
042100000000     C*
042200000000     C                   Eval      RIC0502     = '1'
042300000000     C*
042400000000     C                   MOVE      END0502       $FineSCF
042500000000     C*
042600000000     C                   EndSR
042700000000     C******************************************************
042800000000     C* Elabora record letti
042900000000     C******************************************************
043000000000     C     ElaboraSCF    BegSr                                                  *
043100140328     C*
043200140328     C                   Move      *blanks       Errore
043300140401     C*  Va esclusivamente compilato il quadro SE
043400140401     C*  per gli acquisti di San Marino
043500000000     C*
043600140328     C                   Select
043700140401     C*
043800000000     C                   When      QUADRO0592    =  'SE'
043900000000     C                   Add       1             CntSE
044000000000     C                   Exsr      GesRec_SE
044100000000     C*
044200000000     C                   Other
044300000000     C*
044400000000     C                   Move      '1'           Errore
044500000000     c                   Exsr      ElaErrori
044600140401     C*
044700000000     C                   EndSl
044800000000     C*
044900000000     C                   EndSR
045000000000     C******************************************************
045100000000     C* Calcola nr moduli
045200000000     C******************************************************
045300000000     C     Calc_NrMod    BegSr                                                  *
045400000000     C*
045500000000     C                   Z-Add     0             NrModSE
045600000000     C*
045700000000     C* SE
045800000000     C                   If        CntSE         > 0
045900000000     C     CntSE         Div       3             NrModSE
046000000000     C                   MVR                     RestoMod          5 3
046100000000     C                   If        RestoMod      > 0
046200000000     C                   Add       1             NrModSE
046300000000     C                   EndIf
046400000000     C                   EndIf
046500000000     C*
046600140401     C                   Eval      TotRec       = NrModSE
046700140328     C*
046800140328     C                   EndSR
046900000000     C******************************************************
047000000000     C* Tipo Record SE
047100000000     C******************************************************
047200000000     C     GesRec_SE     BegSr                                                  *
047300000000     C*
047400000000     C                   Add       1             Prog_SE
047500000000     C*
047600000000     C*-------------------------------------------
047700000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
047800000000     C*
047900000000     C* 1) Cognome
048000000000     C                   If        COGRAG05SE    <> *blanks
048100000000     C                   Z-Add     Prog_SE       Conta_Rigo
048200000000     C                   Z-Add     1             Conta_Colo
048300000000     C                   Clear                   Campo_Valore
048400000000     C                   MoveL     COGRAG05SE    Campo_Valore
048500000000     C                   MoveL     COGRAG05SE    C60
048600000000     C                   if        Campo_Valore <> *Blanks
048700000000     C                   Exsr      RieSkieraAlf
048800000000     C                   EndIf
048900000000     C                   EndIf
049000000000     C*
049100000000     C* 2) Nome
049200000000     C                   If        NOME05SE      <> *Blanks
049300000000     C                   Z-Add     Prog_SE       Conta_Rigo
049400000000     C                   Z-Add     2             Conta_Colo
049500000000     C                   Clear                   Campo_Valore
049600000000     C                   MoveL     NOME05SE      Campo_Valore
049700000000     C                   MoveL     NOME05SE      C60
049800000000     C                   if        Campo_Valore <> *Blanks
049900000000     C                   Exsr      RieSkieraAlf
050000000000     C                   EndIf
050100000000     C                   EndIf
050200000000     C*
050300000000     C* 3) Data di nascita
050400000000     C                   If        DTNASC05SE    <> *Loval
050500000000     C                   Move      DTNASC05SE    DtAMG
050600000000     C                   Move      AMGAnno       GMAAnno
050700000000     C                   Move      AMGMese       GMAMese
050800000000     C                   Move      AMGGiorno     GMAGiorno
050900000000     C                   Z-Add     Prog_SE       Conta_Rigo
051000000000     C                   Z-Add     3             Conta_Colo
051100000000     C                   Clear                   Campo_Valore
051200000000     C                   Move      DtGMA         Campo_Valore
051300000000     C                   if        Campo_Valore <> *Blanks
051400000000     C                   Exsr      RieSkiera
051500000000     C                   EndIf
051600000000     C                   EndIf
051700000000     C*
051800000000     C* 4) Comune o Stato estero di nascita
051900000000     C                   If        LOCNAS05SE    <> *Blanks
052000000000     C                   Z-Add     Prog_SE       Conta_Rigo
052100000000     C                   Z-Add     4             Conta_Colo
052200000000     C                   Clear                   Campo_Valore
052300000000     C                   MoveL     LOCNAS05SE    Campo_Valore
052400000000     C                   if        Campo_Valore <> *Blanks
052500000000     C                   Exsr      RieSkiera
052600000000     C                   EndIf
052700000000     C                   EndIf
052800000000     C*
052900000000     C* 5) Provincia
053000000000     C                   If        PRVNAS05SE    <> *Blanks
053100000000     C                   Z-Add     Prog_SE       Conta_Rigo
053200000000     C                   Z-Add     5             Conta_Colo
053300000000     C                   Clear                   Campo_Valore
053400000000     C                   MoveL     PRVNAS05SE    Campo_Valore
053500000000     C                   If        COGRAG05SE <> *blanks  and
053600000000     C                             UICSocieta <> UIC105SE
053700000000     C                   Clear                   Campo_Valore
053800000000     C                   MoveL     'EE'          Campo_Valore
053900000000     C                   EndIf
054000000000     C                   if        Campo_Valore <> *Blanks
054100000000     C                   Exsr      RieSkiera
054200000000     C                   EndIf
054300000000     C                   EndIf
054400000000     C*
054500000000     C* 6) Codice stato estero
054600000000     C                   If        UIC105SE      <> 0
054700000000     C                   Z-Add     Prog_SE       Conta_Rigo
054800000000     C                   Z-Add     6             Conta_Colo
054900000000     C                   Clear                   Campo_Valore
055000000000     C                   Move      UIC105SE      Campo_Valore
055100000000     C                   if        Campo_Valore <> *Blanks
055200000000     C                   Exsr      RieSkiera
055300000000     C                   EndIf
055400000000     C                   EndIf
055500000000     C*
055600000000     C* 7) Denominazione / ragione sociale
055700000000     C                   If        RAGSOC05SE    <> *blanks
055800000000     C                   Z-Add     Prog_SE       Conta_Rigo
055900000000     C                   Z-Add     7             Conta_Colo
056000000000     C                   Clear                   Campo_Valore
056100000000     C                   MoveL     RAGSOC05SE    Campo_Valore
056200000000     C                   MoveL     RAGSOC05SE    C60
056300000000     C                   if        Campo_Valore <> *Blanks
056400000000     C                   Exsr      RieSkieraAlf
056500000000     C                   EndIf
056600000000     C                   EndIf
056700000000     C*
056800000000     C* 8) Citta sede legale
056900000000     C                   If        CITTAE05SE    <> *blanks
057000000000     C                   Z-Add     Prog_SE       Conta_Rigo
057100000000     C                   Z-Add     8             Conta_Colo
057200000000     C                   Clear                   Campo_Valore
057300000000     C                   MoveL     CITTAE05SE    Campo_Valore
057400000000     C                   MoveL     CITTAE05SE    C60
057500000000     C                   if        Campo_Valore <> *Blanks
057600000000     C                   Exsr      RieSkieraAlf
057700000000     C                   EndIf
057800000000     C                   EndIf
057900000000     C*
058000000000     C* 9) Codice stato estero
058100000000     C                   If        UIC205SE      <> 0
058200000000     C                   Z-Add     Prog_SE       Conta_Rigo
058300000000     C                   Z-Add     9             Conta_Colo
058400000000     C                   Clear                   Campo_Valore
058500000000     C                   Move      UIC205SE      Campo_Valore
058600000000     C                   if        Campo_Valore <> *Blanks
058700000000     C                   Exsr      RieSkiera
058800000000     C                   EndIf
058900000000     C                   EndIf
059000000000     C*
059100000000     C* 10) Indirizzo estero sede legale
059200000000     C                   If        INDIRE05SE    <> *blanks
059300000000     C                   Z-Add     Prog_SE       Conta_Rigo
059400000000     C                   Z-Add     10            Conta_Colo
059500000000     C                   Clear                   Campo_Valore
059600000000     C                   MoveL     INDIRE05SE    Campo_Valore
059700000000     C                   MoveL     INDIRE05SE    C60
059800000000     C                   if        Campo_Valore <> *Blanks
059900000000     C                   Exsr      RieSkieraAlf
060000000000     C                   EndIf
060100000000     C                   EndIf
060200000000     C*
060300140121     C* 11) Codice identificativo IVA (solo se San Marino)
060400140328     C                   If        UIC205SE      =  037
060500140121     C                   If        CIVA05SE      <> *Loval
060600000000     C                   Z-Add     Prog_SE       Conta_Rigo
060700000000     C                   Z-Add     11            Conta_Colo
060800000000     C                   Clear                   Campo_Valore
060900000000     C                   MoveL     CIVA05SE      Campo_Valore
061000000000     C                   if        Campo_Valore <> *Blanks
061100000000     C                   Exsr      RieSkiera
061200000000     C                   EndIf
061300000000     C                   EndIf
061400140328     C                   EndIf
061500000000     C*
061600000000     C* 12) Data emissione del documento fattura
061700000000     C                   If        DTDOC05SE     <> *Loval
061800000000     C                   Move      DTDOC05SE     DtAMG
061900000000     C                   Move      AMGAnno       GMAAnno
062000000000     C                   Move      AMGMese       GMAMese
062100000000     C                   Move      AMGGiorno     GMAGiorno
062200000000     C                   Z-Add     Prog_SE       Conta_Rigo
062300000000     C                   Z-Add     12            Conta_Colo
062400000000     C                   Clear                   Campo_Valore
062500000000     C                   Move      DtGMA         Campo_Valore
062600000000     C                   if        Campo_Valore <> *Blanks
062700000000     C                   Exsr      RieSkiera
062800000000     C                   EndIf
062900000000     C                   EndIf
063000000000     C*
063100000000     C* 13) Data di registrazione fattura
063200000000     C                   If        DTREG05SE     <> *Loval
063300000000     C                   Move      DTREG05SE     DtAMG
063400000000     C                   Move      AMGAnno       GMAAnno
063500000000     C                   Move      AMGMese       GMAMese
063600000000     C                   Move      AMGGiorno     GMAGiorno
063700000000     C                   Z-Add     Prog_SE       Conta_Rigo
063800000000     C                   Z-Add     13            Conta_Colo
063900000000     C                   Clear                   Campo_Valore
064000000000     C                   Move      DtGMA         Campo_Valore
064100000000     C                   if        Campo_Valore <> *Blanks
064200000000     C                   Exsr      RieSkiera
064300000000     C                   EndIf
064400000000     C                   EndIf
064500000000     C*
064600000000     C* 14) Nr.Fattura
064700000000     C                   If        NRDOC05SE     <> 0
064800000000     C                   Z-Add     Prog_SE       Conta_Rigo
064900000000     C                   Z-Add     14            Conta_Colo
065000000000     C                   Clear                   Campo_Valore
065100000000     C                   MoveL     NRDOC05SE     Campo_Valore
065200000000     C                   if        Campo_Valore <> *Blanks
065300000000     C                   Exsr      RieSkiera
065400000000     C                   EndIf
065500000000     C                   EndIf
065600000000     C*
065700000000     C* 15) Importo
065800000000     C                   If        IMPORT05SE    <> 0
065900000000     C                   Z-Add     Prog_SE       Conta_Rigo
066000000000     C                   Z-Add     15            Conta_Colo
066100000000     C                   Clear                   Campo_Valore
066200000000     C                   Z-Add     IMPORT05SE    Importo16_0
066300000000     C                   If        IMPORT05SE   <   0
066400000000     C                   Eval      Importo16_0  = Importo16_0 * -1
066500000000     C                   EndIf
066600000000     C                   Exsr      Ablank2
066700000000     C                   if        Campo_Valore <> *Blanks
066800000000     C                   Exsr      RieSkiera
066900000000     C                   EndIf
067000000000     C                   EndIf
067100000000     C*
067200000000     C* 16) Imposta
067300000000     C                   If        IVA05SE       <> 0
067400000000     C                   Z-Add     Prog_SE       Conta_Rigo
067500000000     C                   Z-Add     16            Conta_Colo
067600000000     C                   Clear                   Campo_Valore
067700000000     C                   Z-Add     IVA05SE       Importo16_0
067800000000     C                   If        IVA05SE      <   0
067900000000     C                   Eval      Importo16_0  = Importo16_0 * -1
068000000000     C                   EndIf
068100000000     C                   Exsr      Ablank2
068200000000     C                   if        Campo_Valore <> *Blanks
068300000000     C                   Exsr      RieSkiera
068400000000     C                   EndIf
068500000000     C                   EndIf
068600000000     C*
068700000000     C* 17) Conferma importo
068800000000     C                   If        IMPORT05SE     > 999999
068900000000     C                   Z-Add     Prog_SE       Conta_Rigo
069000000000     C                   Z-Add     17            Conta_Colo
069100000000     C                   Clear                   Campo_Valore
069200000000     C                   Move      '1'           Campo_Valore
069300000000     C                   Exsr      Ablank
069400000000     C                   if        Campo_Valore <> *Blanks
069500000000     C                   Exsr      RieSkiera
069600000000     C                   EndIf
069700000000     C                   EndIf
069800000000     C*
069900000000     C                   IF        Prog_SE       = 3
070000000000     C                   Z-Add     0             Prog_SE
070100000000     C                   Add       1             ProgMod_SE
070200000000     C                   Z-Add     ProgMod_SE    WrkProgMod
070300000000     C                   EXSR      WrtDati
070400000000     C                   EXSR      InzSkiera
070500000000     C                   EndIf
070600000000     C*
070700000000     C                   EndSR
070800000000     C******************************************************
070900000000     C* Tipo Record TA
071000000000     C******************************************************
071100000000     C     GesRec_TA     BegSr                                                  *
071200000000     C*
071300000000     C*
071400000000     C*
071500000000     C*  SE  acquisti servizi da non residenti
071600000000     C                   If        NRRISE05TA    <> 0
071700000000     C                   Z-Add     10            Conta_Rigo
071800000000     C                   Z-Add     1             Conta_Colo
071900000000     C                   Clear                   Campo_Valore
072000000000     C                   Z-Add     NRRISE05TA    Importo16_0
072100000000     C                   Exsr      Ablank2
072200000000     C                   if        Campo_Valore <> *Blanks
072300000000     C                   Exsr      RieSkiera
072400000000     C                   EndIf
072500000000     C                   EndIf
072600000000     C*
072700000000     C                   EndSR
072800000000     C******************************************************
072900000000     C* Riempimento skiera per i dati non posizionali
073000000000     C******************************************************
073100000000     C     RieSkiera     BEGSR
073200000000     C*
073300000000     C                   Z-ADD     Conta_Rigo    Cod_Rigo
073400000000     C                   Z-ADD     Conta_Colo    Cod_Colo
073500000000     C*
073600000000     C                   ADD       1             $I
073700000000     C                   MoveL     Campo_Codice  Skiera($I)
073800000000     C                   MoveL     WSavQua       Skiera($I)
073900000000     C                   Move      Campo_Valore  Skiera($I)
074000000000     C                   Clear                   Campo_Valore
074100000000     C*
074200000000     C                   ENDSR                                                  *
074300000000      /EJECT
074400000000     C******************************************************
074500000000     C* Tratta campi alfanumerici
074600000000     C******************************************************
074700000000     C     RieSkieraAlf  BEGSR
074800000000     C*
074900000000     C                   Z-ADD     Conta_Rigo    Cod_Rigo
075000000000     C                   Z-ADD     Conta_Colo    Cod_Colo
075100000000     C*
075200000000     C                   Add       1             $I
075300000000     C                   MoveL     Campo_Codice  Skiera($I)
075400000000     C                   MoveL     WSavQua       Skiera($I)
075500000000     C                   Move      Campo_Valore  Skiera($I)
075600000000     C                   Clear                   Campo_Valore
075700000000     C*
075800000000     C                   Clear                   Campo
075900000000     C*
076000000000     C* Controllo se ci sono dati dopo la 17° posizione
076100000000     C                   Do        3             XY                1 0
076200000000     C*
076300000000     C                   If        C15(XY) <> *blANKS
076400000000     C                   Add       1             $I
076500000000     C                   MOVEL     '+'           C16              16
076600000000     C                   MOVE      C15(XY)       C16
076700000000     C                   MoveL     Campo_Codice  Skiera($I)
076800000000     C                   MoveL     WSavQua       Skiera($I)
076900000000     C                   Move      C16           Skiera($I)
077000000000     C                   EndIf
077100000000     C*
077200000000     C                   EndDo
077300000000     C*
077400000000     C                   Clear                   C60
077500000000     C*
077600000000     C                   ENDSR
077700000000     C******************************************************
077800000000     C* Toglie gli zeri non significativi al Campo_Valore
077900000000     C******************************************************
078000000000     C     ABlank        BEGSR
078100000000     C*
078200000000     C                   MoveA     Campo_Valore  Sk_Campo
078300000000     C*
078400000000     C                   DO        Max$X         $X
078500000000     C*
078600000000     C                   if        Sk_Campo($X) = ' '   or
078700000000     C                             Sk_Campo($X) = '0'
078800000000     C                   Move      ' '           Sk_Campo($X)
078900000000     C                   else
079000000000     C                   LEAVE
079100000000     C                   endif
079200000000     C*
079300000000     C                   ENDDO
079400000000     C*
079500000000     C                   MoveA     Sk_Campo      Campo_Valore
079600000000     C*
079700000000     C                   ENDSR                                                  *
079800000000      /EJECT
079900000000     C******************************************************
080000000000     C* Toglie gli zeri non significativi nelle caselle con
080100000000     C* importo dei campi posizionali
080200000000     C******************************************************
080300000000     C     ABlank2       BEGSR
080400000000     C*
080500000000     C                   Move      Importo16_0   Importo16A
080600000000     C                   MoveA     Importo16A    SK_Importo
080700000000     C*
080800000000     C                   DO        Max$Z         $Z
080900000000     C*
081000000000     C                   if        Sk_Importo($Z) = ' '   or
081100000000     C                             Sk_Importo($Z) = '0'
081200000000     C                   Move      ' '           Sk_Importo($Z)
081300000000     C                   else
081400000000     C                   If        $FlgNeg       = '1'
081500000000     C                   Eval      Sk_Importo($Z-1) = '-'
081600000000     C                   Eval      $FlgNeg       = '0'
081700000000     C                   endif
081800000000     C                   LEAVE
081900000000     C                   endif
082000000000     C*
082100000000     C                   ENDDO
082200000000     C*
082300000000     C                   MoveA     Sk_Importo    Campo_Valore
082400000000     C*
082500000000     C                   ENDSR                                                  *
082600000000      /EJECT
082700000000     C******************************************************
082800000000     C* Scrittura record tipo 1 = clienti / 2 = fornitori
082900000000     C******************************************************
083000000000     C     WrtDati       BEGSR
083100000000     C*
083200000000     C                   Clear                   SDGETW000
083300000000     C*
083400000000     C                   Move      XSCSOC        ETWSocieta
083500000000     C                   Move      WSavQua       ETWTRE
083600000000     C                   Add       1             WRKPRGMOD
083700000000     C                   Z-Add     WRKPRGMOD     ETWPRGMOD
083800000000     C                   Movea     Skiera        ETWDAT
083900140328     C                   MoveL     'D'           ETWTRC
084000140328     C*
084100140328     C                   Add       1             $NrRecTot
084200140328     C                   WRITE     SDGETW000
084300000000     C                   FEOD      SDGETW00F
084400000000     C*
084500000000     C                   ENDSR
084600000000     C******************************************************
084700000000     C* Gestione singolo invio
084800000000     C******************************************************
084900000000     C     GesSingInv    BegSr                                                  *
085000000000     C*
085100000000     C                   Exsr      RepNomeF
085200000000     C                   Exsr      GesFilTel
085300000000     C*
085400000000     C                   OPEN      SDGETE00F
085500000000     C*
085600000000     C* Record A  testata della fornitura
085700000000     C                   Exsr      WrtTpRecA
085800000000     C*
085900000000     C* Record B  testata della fornitura
086000000000     C                   Exsr      WrtTpRecB
086100000000     C*
086200000000     C* Leggo SDGETW per scrivere SDGETW
086300000000a1   C                   DOW       $FineETW    = *Off
086400000000     C*
086500000000     C                   Exsr      LetturaETW
086600000000     C*
086700000000e1   C                   EndDo                                                  *
086800000000     C*
086900000000     C* Record E di coda della fornitura
087000000000     C                   Exsr      WrtTpRecE
087100000000     C*
087200000000     C* Record Z di coda della fornitura
087300000000     C                   Exsr      WrtTpRecZ
087400000000     C*
087500000000     C                   Exsr      IscrCatF
087501151117R207 C*
087502151117  "  C                   If        StpLG64   <> *Off
087503151117  "  C                   Exsr      StampaFile
087504151117R207 C                   EndIf
087600000000     C*
087700000000     C                   Close     SDGETE00F
087800000000     C*
087900000000     C                   EndSR                                                  *
088000000000     C******************************************************
088100000000     C* Gestione invio multiplo
088200000000     C******************************************************
088300000000     C     GesMultInv    BegSr                                                  *
088400000000     C*
088500000000     C*
088600000000a1   C                   DOW       $FineETW    = *Off
088700000000     C*
088800000000     C                   Add       1             $ProgrInvio
088900000000     C                   Z-Add     0             Cnt_Righe
089000000000     C*
089100000000     C                   Exsr      RepNomeF
089200000000     C                   Exsr      GesFilTel
089300000000     C*
089400000000     C                   OPEN      SDGETE00F
089500000000     C*
089600000000     C* Record A  testata della fornitura
089700000000     C                   Exsr      WrtTpRecA
089800000000     C*
089900000000     C* Record B  testata della fornitura
090000000000     C                   Exsr      WrtTpRecB
090100000000     C*
090200000000     C* seconda lettura per scrivere il multifile
090300000000a2   C                   DOW       $FineETW    = *Off  and
090400140328     C                             (Cnt_Righe    <  MaxRecG64 and
090500140328     C                              MaxRecG64   > 0            )
090600000000     C*
090700000000     C* Leggo SDGETW per scrivere SDGETE
090800140328a1   C                   If        TipInvG64   <> 2
090900000000     C*
091000000000     C                   Exsr      LetturaETW
091100000000     C*
091200000000e1   C                   EndIf                                                  *
091300000000     C*
091400000000e2   C                   EndDo                                                  *
091500000000     C*
091600000000     C* Record E di coda della fornitura
091700000000     C                   Exsr      WrtTpRecE
091800000000     C*
091900000000     C* Record Z di coda della fornitura
092000000000     C                   Exsr      WrtTpRecZ
092100000000     C*
092200000000     C                   Exsr      IscrCatF
092300000000     C*
092400151117     C                   If        StpLG64   <> *Off
092500000000     C                   Exsr      StampaFile
092600000000     C                   EndIf
092700000000     C*
092800000000     C                   Close     SDGETE00F
092900000000     C*
093000000000e1   C                   EndDo                                                  *
093100000000     C*
093200000000     C                   EndSR                                                  *
093300000000     C************************************************************
093400000000     C* Reperimento numero max di invii
093500000000     C************************************************************
093600000000     C     RepNrInvii    BegSr
093700000000     C*
093800000000     C                   if        $NrRecTot   > 0
093900000000     C                   Z-Add     $NrRecTot     WImpInp
094000140328     C                   CallP     X0201MultC(WImpInp: 1: MaxRecG64:
094100000000     C                                        '+': '0': 0: WImpOut)
094200000000     C                   Z-Add     WImpOut       $NrInvii
094300000000     C                   EndIf
094400000000     C*
094500000000     C                   EndSR                                                  *
094600000000     C******************************************************
094700000000     C* Riempimento dati per t.r. A
094800000000     C******************************************************
094900000000     C     WrtTpRecA     BegSr                                                  *
095000000000     C*
095100000000     C                   Clear                   SDGETE000
095200000000     C                   Clear                   SDGSG61DSA
095300000000     C*
095400000000     C*-----------------------------------
095500000000     C* DATI IDENTIFICATIVI DELLA FORNITURA
095600000000     C*
095700000000     C*-
095800000000     C* Tipo record
095900000000     C                   MoveL     'A'           G61ATR
096000000000     C*-
096100000000     C* Codice identificativo della fornitura
096200000000     C                   MoveL     'NSP00'       G61AFOR
096300000000     C*-
096400000000     C* Tipo fornitore
096500000000     C*      "01" Soggetti che inviano la propria
096600000000     C*      "10" Intermediari
096700000000     C                   MoveL     '01'          G61ATFO
096800000000     C                   If        $FisInt       <> *blanks
096900000000     C                   MoveL     '10'          G61ATFO
097000000000     C                   EndIF
097100000000     C*-
097200000000     C* Codice fiscale del contribuente
097300000000     C                   MoveL     XScCdF        G61ACFI
097400140328     C                   If        $FisInt       <> *blanks
097500140328     C                   Move      $FisInt       G61ACFI
097600140328     C                   EndIF
097700140328     C*-
097800140328     C* Progressivo invio
097900140328     C                   If        $MultiInvio    = *Off
098000140328     C                   Z-Add     0             G61APRGINV
098100140328     C                   ElSe
098200140328     C                   Z-Add     $ProgrInvio   G61APRGINV
098300140328     C                   EndIF
098400140328     C* Num.Tot.Invii
098500140328     C                   If        $MultiInvio    = *Off
098600140328     C                   Z-Add     0             G61ATOTINV
098700000000     C                   ElSe
098800000000     C                   Z-Add     $NrInvii      G61ATOTINV
098900000000     C                   EndIF
099000000000     C*
099100000000     C*-----------------------------------
099200000000     C*-
099300000000     C* Carattere di controllo
099400000000     C                   MoveL     'A'           G61AK1
099500000000     C*-
099600000000     C* Elementi di fine riga
099700000000     C* Impostare i valori esadecimali '0D' e '0A'
099800000000     C* (caratteri ASCI 'CR' ed 'LF')
099900000000     C*
100000000000     C                   Eval      G61AK2  = *blanks
100100000000     C                   Eval      G61AK2  = X'0D25'
100200000000     C*
100300000000     C                   MoveL     SDGSG61DSA    ETEREC
100400000000     C                   WRITE     SDGETE000
100500000000     C                   FEOD      SDGETE00F
100600000000     C*
100700000000     C                   EndSR                                                  *
100800000000     C******************************************************
100900000000     C* Riempimento dati per t.r. B
101000000000     C******************************************************
101100000000     C     WrtTpRecB     BegSr                                                  *
101200000000     C*
101300000000     C                   Clear                   SDGETE000
101400000000     C                   Clear                   SDGSG61DSB
101500000000     C*
101600000000     C* Tipo record
101700000000     C                   MoveL     'B'           G61BTR
101800000000     C*-
101900000000     C* Codice fiscale del contribuente
102000000000     C                   MoveL     XScCdF        G61BCFI
102100000000     C* Progressivo modulo
102200000000     C                   Z-Add     1             G61BPRGMOD
102300000000     C*-
102400000000     C* Identificativo produttore software
102500000000     C                   MoveL     IDPRODA08     G61BIPS
102600000000     C*-
102700000000     C* Tipo Comunicazione
102800140328     C                   Move      '0'           G61BCORD
102900140328     C                   Move      '0'           G61BCSOS
103000140328     C                   Move      '0'           G61BCANN
103100000000     C*-
103200000000     C                   Select
103300140328     C                   When      TIPINVG64     = 0
103400000000     C                   Move      '1'           G61BCORD
103500140328     C                   When      TIPINVG64     = 1
103600000000     C                   Move      '1'           G61BCSOS
103700140328     C                   When      TIPINVG64     = 2
103800000000     C                   Move      '1'           G61BCSOS
103900000000     C                   EndSl
104000000000     C*-
104100140328     C                   Z-Add     PROTOCG64     G61BPROT
104200140328     C                   Z-Add     PROTODG64     G61BPROTD
104300000000     C*-
104400140328     C                   Move      '0'           G61BAGG
104500000000     C                   Move      '1'           G61BANA
104600000000     C*-
104700140328     C                   Move      '0'           G61BQFA
104800140328     C                   Move      '0'           G61BQSA
104900140328     C                   Move      '0'           G61BQBL
105000140328     C                   Move      '0'           G61BQFE
105100140328     C                   Move      '0'           G61BQFR
105200140328     C                   Move      '0'           G61BQNE
105300140328     C                   Move      '0'           G61BQNR
105400140328     C                   Move      '0'           G61BQDF
105500140328     C                   Move      '0'           G61BQFN
105600140328     C                   Move      '0'           G61BQSE
105700140328     C                   Move      '0'           G61BQTU
105800000000     C                   If        CntSE         > 0
105900000000     C                   Move      '1'           G61BQSE
106000140328     C                   EndIf
106100140328     C*
106200140328     C                   Move      '1'           G61BQTA
106300140328     C*
106400140328     C*-
106500140328     C* Partita IVA del contribuente
106600140328     C                   MoveL     XScIVA        G61BPIVA
106700000000     C*
106800000000     C* Codice attivita
106900000000     C                   MoveL     XScATT        G61BCAT
107000000000     C*
107100000000     C* Telefono
107200000000     C                   MoveL     *all'0'       G61BTEL
107300000000     C*
107400000000     C* Fax
107500000000     C                   Move      *all'0'       G61BFAX
107600000000     C*
107700000000     C* e-mail
107800140328     C                   MoveL     *blanks       G61BEMAIL
107900140328     C*
108000140328     C                   Move      *all'0'       G61BDTNASC
108100140328     C                   Move      *all'0'       G61BDTNAS1
108200140328     C                   Move      *all'0'       G61BDTINP
108300140328     C                   Move      *all'0'       G61BDTFIP
108400140328     C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
108500140328     C*
108600140328     C* reperisco la natura giuridica del contribuente
108700140328     C                   MoveL     XScNCR        WrkNatGiu
108800000000     C                   Exsr      RepNatGiu
108900000000     C*
109000000000     C                   Select
109100000000     C*-
109200000000     C*-----------------------
109300000000     C* PERSONA FISICA
109400000000     C* da impostare solo nel caso di persona fisica
109500000000     C                   When      TPNG16      = '1'
109600000000     C*
109700000000     C                   Exsr      RepPersFis
109800000000     C*-
109900000000     C* Cognome
110000000000     C                   MoveL     $CogRagSog    G61BCOGNO
110100000000     C*-
110200000000     C* Nome
110300000000     C                   MoveL     $NomeSog      G61BNOME
110400000000     C*-
110500000000     C* Sesso
110600000000     C                   If        $SexSog       ='1'
110700000000     C                   MoveL     'M'           G61BSESSO
110800000000     C                   Else
110900000000     C                   MoveL     'F'           G61BSESSO
111000000000     C                   EndIf
111100000000     C*-
111200000000     C* Data di nascita
111300000000     C                   If        $DtNascSog    <> *Loval
111400000000     C                   Move      $DtNascSog    DtAMG
111500000000     C                   Move      AMGAnno       GMAAnno
111600000000     C                   Move      AMGMese       GMAMese
111700000000     C                   Move      AMGGiorno     GMAGiorno
111800000000     C                   Move      DtGMA         G61BDTNASC
111900000000     C                   Else
112000000000     C                   Move      *zeros        G61BDTNASC
112100000000     C                   EndIf
112200000000     C*-
112300000000     C* Comune o Stato estero di nascita
112400000000     C                   MoveL     $LocNascSog   G61BLUONAS
112500000000     C*-
112600000000     C* Provincia di nascita
112700000000     C                   MoveL     $PrvNascSog   G61BPRVNAS
112800000000     C*-----------------------
112900000000     C* PERSONA GIURIDICA
113000000000     C* da impostare solo nel caso di persona non fisica
113100000000     C                   When      TPNG16      = '2'  or
113200140328     C                             TPNG16      = '3'
113300140328     C*-
113400140328     C* Denominazione
113500140328     C                   MoveL     XSCRGS        G61BDENOMI
113600140328     C*
113700140328     C                   EndSl
113800140328     C*-
113900140328     C* Anno
114000140328     C                   Move      AnnoG64       G61BANNO
114100000000     C*-
114200000000     C* Mese
114300000000     C                   Move      *blanks       G61BMESE
114400140328     C* Mese
114500140328     C                   Move(P)   *All'0'       G61BMese
114600140328     C                   Move      MeseG64       G61BMESE
114700000000     C*-
114800000000     C*-----------------------------------
114900000000     C* DATI SOGGETTO CHE SI IMPEGNA ALLA TRASMISSIONE
115000000000     C* (soggetto che effettua la comunicazione, se diverso dal soggetto
115100000000     C*  a cui si riferisce la comunicazione)
115200000000     C*
115300000000     C* Cod.Fiscale
115400000000     C                   Move      *blanks       G61BCDFIS1
115500000000     C* Se significativo
115600000000     C                   If        $FisSog       <> *blanks
115700000000     C* Cod.Fiscale
115800000000     C                   Move      $FisSog       G61BCDFIS1
115900000000     C* Cod.Carica
116000000000     C                   Move      $CarSog       G61BCARR1
116100000000     C* Data inizio procedura
116200000000     C                   If        $DtiSog       <> '0001-01-01'
116300000000     C                   Move      $DtiSog       DtAMG
116400000000     C                   Exsr      ValorizDTA
116500000000     C                   Eval      G61BDTINP    = WrkDt
116600000000     C                   EndIf
116700000000     C*-
116800000000     C* Data fine procedura
116900000000     C                   If        $DtfSog       <> '0001-01-01'
117000000000     C                   Move      $DtfSog       DtAMG
117100000000     C                   Exsr      ValorizDTA
117200000000     C                   Eval      G61BDTFIP    = WrkDt
117300000000     C                   EndIf
117400000000     C*-
117500000000     C* Persona Fisica
117600000000     c                   If        $COGSOG       <> *blanks
117700000000     C                   Eval      G61BCOGNO1   = $CogSOG
117800000000     C                   Eval      G61BNOME1    = $NomSOG
117900000000     C                   Eval      G61BSESSO1   = $SEXRSOG
118000000000     C                   If        $DtnSog       <> *blanks
118100000000     C                   Move      $DtnSog       DtAMG
118200000000     C                   Exsr      ValorizDTA
118300000000     C                   Eval      G61BDTNAS1   = WrkDt
118400000000     C                   EndIf
118500000000     C                   Eval      G61BLUONA1   = $COMSOG
118600000000     C                   Eval      G61BPRVNA1   = $PRVSOG
118700000000     C                   EndIf
118800000000     C*-
118900000000     C* Persona Giuridica
119000000000     c                   If        $RagSOG       <> *blanks
119100000000     C                   Eval      G61BDENOM1   = $RAGSOG
119200000000     C                   EndIf
119300000000     C*-
119400000000     C                   EndIf
119500000000     C*-----------------------------------
119600000000     C* DATI SOGGETTO CHE SI IMPEGNA ALLA TRASMISSIONE (Intermediario)
119700000000     C*
119800000000     C* Cod.Fisc.Intermediario
119900000000     C                   Move      *blanks       G61BCFINT
120000000000     C* Nr CAAF
120100000000     C                   Z-Add     0             G61BCAAF
120200000000     C* Impegno alla presentazione
120300000000     C                   Z-Add     0             G61BIMPE
120400000000     C* Data dell'Impegno alla presentazione
120500000000     C                   Move      *all'0'       G61BDTIMPE
120600000000     C*
120700000000     C* Se significativo
120800000000     C                   If        $FisInt       <> *blanks
120900000000     C* Cod.Fisc.Intermediario
121000000000     C                   Move      $FisInt       G61BCFINT
121100000000     C* Nr CAAF
121200000000     C                   Move      $CafInt       G61BCAAF
121300000000     C* Impegno alla presentazione
121400000000     C                   Move      $ImpTrs       G61BIMPE
121500000000     C* Data dell'Impegno alla presentazione
121600000000     C                   Move      $ImpDta       DtAMG
121700000000     C                   Exsr      ValorizDTA
121800000000     C                   Eval      G61BDTIMPE   = WrkDt
121900000000     C                   EndIf
122000000000     C*-----------------------------------
122100000000     C*-
122200000000     C* Carattere di controllo
122300000000     C                   MoveL     'A'           G61BK1
122400000000     C*-
122500000000     C* Elementi di fine riga
122600000000     C* Impostare i valori esadecimali '0D' e '0A'
122700000000     C* (caratteri ASCI 'CR' ed 'LF')
122800000000     C*
122900000000     C                   Eval      G61BK2 =  *blanks
123000000000     C                   Eval      G61BK2 =  X'0D25'
123100000000     C*
123200000000     C                   MoveL     SDGSG61DSB    ETEREC
123300000000     C                   WRITE     SDGETE000
123400000000     C                   FEOD      SDGETE00F
123500000000     C*
123600000000     C                   EndSR                                                  *
123700000000     C******************************************************
123800000000     C* Scrittura record t.r. D
123900000000     C******************************************************
124000000000     C     WrtTpRecD     BegSr
124100000000     C*
124200000000     C                   ADD       1             CNT_D
124300000000     C                   ADD       1             Cnt_Righe
124400000000     C*
124500000000     C                   Clear                   SDGSG61DSD
124600000000     C* Tipo Record
124700000000     C                   Eval      G61DTR       = 'D'
124800000000     C* Cod.Fiscale
124900000000     C                   Eval      G61DCFI      = XScCdF
125000000000     C*
125100000000     C* Progressivo modulo
125200000000     C                   Eval      G61DPRGMOD   = ETWPRGMOD
125300000000     C*
125400000000     C* Identificativo produttore software
125500000000     C                   MoveL     IDPRODA08     G61DIPS
125600000000     C*-
125700000000     C                   MoveL     ETWDAT        G61DDATI
125800000000     C*-
125900000000     C* Carattere di controllo
126000000000     C                   MoveL     'A'           G61DK1
126100000000     C*-
126200000000     C* Elementi di fine riga
126300000000     C* Impostare i valori esadecimali '0D' e '0A'
126400000000     C* (caratteri ASCI 'CR' ed 'LF')
126500000000     C*
126600000000     C                   Eval      G61DK2  = *blanks
126700000000     C                   Eval      G61DK2  = X'0D25'
126800000000     C*
126900000000     C*----
127000000000     C                   Clear                   SDGETE000
127100000000     C                   MoveL     SDGSG61DSD    ETEREC
127200000000     C*----
127300000000     C                   WRITE     SDGETE000
127400000000     C                   FEOD      SDGETE00F
127500000000     C*
127600000000     C                   EndSR
127700000000     C******************************************************
127800000000     C* Riempimento dati per t.r. E
127900000000     C******************************************************
128000000000     C     WrtTpRecE     BegSr                                                  *
128100000000     C*
128200000000     C                   Clear                   SDGSG61DSE
128300000000     C* Tipo Record
128400000000     C                   Eval      G61ETR       = 'E'
128500000000     C* Cod.Fiscale
128600000000     C                   Eval      G61ECFI      = XscCDf
128700000000     C*
128800000000     C* Progressivo modulo
128900000000     C                   Eval      G61EPRGMOD   = 1
129000000000     C*
129100000000     C* Identificativo produttore software
129200000000     C                   MoveL     IDPRODA08     G61EIPS
129300000000     C*-
129400000000     C                   Clear                   Skiera
129500000000     C                   Eval      WSavQua  ='TA'
129600000000     C*-
129700000000     C                   Exsr      GesRec_TA
129800000000     C                   Movea     Skiera        G61EDATI
129900000000     C*-
130000000000     C* Carattere di controllo
130100000000     C                   MoveL     'A'           G61EK1
130200000000     C*-
130300000000     C* Elementi di fine riga
130400000000     C* Impostare i valori esadecimali '0D' e '0A'
130500000000     C* (caratteri ASCI 'CR' ed 'LF')
130600000000     C*
130700000000     C                   Eval      G61EK2  = *blanks
130800000000     C                   Eval      G61EK2  = X'0D25'
130900000000     C*
131000000000     C*----
131100000000     C                   Clear                   SDGETE000
131200000000     C                   MoveL     SDGSG61DSE    ETEREC
131300000000     C*----
131400000000     C                   WRITE     SDGETE000
131500000000     C                   FEOD      SDGETE00F
131600000000     C*
131700000000     C                   EndSR
131800000000     C******************************************************
131900000000     C* Riempimento dati per t.r. Z
132000000000     C******************************************************
132100000000     C     WrtTpRecZ     BegSr                                                  *
132200000000     C*
132300000000     C                   Clear                   SDGSG61DSZ
132400000000     C* Tipo Record
132500000000     C                   Eval      G61ZTR       = 'Z'
132600000000     C*-
132700000000     C                   Z-ADD     1             G61ZNB
132800000000     C*-
132900000000     C                   Z-ADD     CNT_C         G61ZNC
133000000000     C*-
133100000000     C                   Z-ADD     CNT_D         G61ZND
133200000000     C*-
133300000000     C                   Z-ADD     1             G61ZNE
133400000000     C*-
133500000000     C* Carattere di controllo
133600000000     C                   MoveL     'A'           G61ZK1
133700000000     C*-
133800000000     C* Elementi di fine riga
133900000000     C* Impostare i valori esadecimali '0D' e '0A'
134000000000     C* (caratteri ASCI 'CR' ed 'LF')
134100000000     C*
134200000000     C                   Eval      G61ZK2  = *blanks
134300000000     C                   Eval      G61ZK2  = X'0D25'
134400000000     C*
134500000000     C                   MoveL     SDGSG61DSZ    ETEREC
134600000000     C                   WRITE     SDGETE000
134700000000     C                   FEOD      SDGETE00F
134800000000     C*
134900000000     C                   EndSR                                                  *
135000000000     C******************************************************
135100000000     C* Posizionamento SDGEP3
135200000000     C******************************************************
135300000000     C     PosizETW      BegSr                                                  *
135400000000     C*
135500000000     C     K01ETW01      SETLL     SDGETW01L                              23
135600000000     C*
135700000000     C                   EndSR                                                  *
135800000000     C******************************************************
135900000000     C* Leggo SDGETW e scrivo il record telematico SDGETE
136000000000     C******************************************************
136100000000     C     LetturaETW    BegSr                                                  *
136200000000     C*
136300000000     C                   READ      SDGETW01L                              23
136400000000     C                   Eval      $FineETW    = *in23
136500000000     C*
136600140401     C                   If        $FineETW    = *Off
136700000000     C                   Exsr      WrtTpRecD
136800140401     C                   EndIf                                                  *
136900000000     C*
137000000000     C                   EndSR                                                  *
137100000000     C******************************************************
137200000000     C* Valorizzazione data per telematico
137300000000     C******************************************************
137400000000     C     ValorizDTA    BegSr                                                  *
137500000000     C*
137600000000     C                   Clear                   WrkDt
137700000000     C*
137800000000     C                   Move      AMGAnno       GMAAnno
137900000000     C                   Move      AMGMese       GMAMese
138000000000     C                   Move      AMGGiorno     GMAGiorno
138100000000     C*
138200000000     C                   Eval      WrkDt        = DtGMA
138300000000     C*
138400000000     C                   EndSR
138500000000     C******************************************************
138600000000     C* Reperimento della natura giuridica
138700000000     C******************************************************
138800000000     C     RepNatGiu     BegSr                                                  *
138900000000     C*
139000000000     C                   Reset                   SDGTABDS
139100000000     C*
139200000000     C                   MOVE      '1'           XTBRIC
139300140328     C                   MOVE      SOCIETAG64    XTBAZI
139400000000     C                   MOVE      *ZERO         XTBKEY
139500000000     C                   MOVE      'G16'         XTBCOD
139600000000     C                   MOVE      WrkNatGiu     XTBKEY
139700000000     C                   CALL      'SDGTABDR'
139800000000     C                   PARM                    SDGTABDS
139900000000     C                   if        XTBERR      = *Off
140000000000     C                   MoveL     XTBUNI        ANGG16DS
140100000000     C                   EndIf
140200000000     C*
140300000000     C                   EndSR                                                  *
140400000000     C******************************************************
140500000000     C* Reperimento dati stato
140600000000     C******************************************************
140700000000     C     RepStato      BegSr                                                  *
140800000000     C*
140900000000     C                   Reset                   SDGTABDS
141000000000     C*
141100000000     C                   MOVE      '1'           XTBRIC
141200140328     C                   MOVE      SOCIETAG64    XTBAZI
141300000000     C                   MOVE      *ZERO         XTBKEY
141400000000     C                   MOVE      'G12'         XTBCOD
141500000000     C                   MOVE      WrkStato      XTBKEY
141600000000     C                   CALL      'SDGTABDR'
141700000000     C                   PARM                    SDGTABDS
141800000000     C                   if        XTBERR      = *Off
141900000000     C                   MoveL     XTBUNI        ANGG12DS
142000000000     C                   EndIf
142100000000     C*
142200000000     C                   EndSR                                                  *
142300000000     C************************************************************
142400000000     C* Reperimento dati società
142500000000     C************************************************************
142600000000     C     RepSoc        BegSr
142700000000     C*
142800000000     C                   CALL      'SDGSOCDR'
142900000000     C                   PARM                    TIPXSC            6
143000000000     C                   PARM                    SOCXSC            3
143100000000     C                   PARM                    CDSXSC            9 0
143200000000     C                   PARM                    MODXSC            3
143300000000     C                   PARM      *Blanks       RTNXSC            1
143400000000     C                   PARM                    XSOCDS
143500000000     C                   PARM                    KPJBA
143600000000     C*
143700000000     C                   EndSR
143800000000     C************************************************************
143900000000     C* Reperimento nome file per invio operazioni iva
144000000000     C************************************************************
144100000000     C     RepNomeF      BegSr
144200000000     C*
144300000000     C                   Eval      WNumero     = *All'0'
144400140328     C                   Z-Add     ANNOG64       WAnno
144500140401     C                   Z-Add     MESEG64       WPrgInvio
144600000000     C                   Move      WNumero       $Numero
144700000000     C*
144800000000     C                   CALL      'X35NOMFR'
144900140328     C                   PARM                    SOCIETAG64
145000000000     C                   PARM      *Blank        $Unita            8
145100140401     C                   PARM      'SM'          $CdScarico        2
145200000000     C                   PARM                    $Numero           9
145300000000     C                   PARM                    $File            10
145400000000     C                   PARM                    $Libreria        10
145500000000     C                   PARM                    $Errore           1
145600000000     C*
145700000000     C                   if        $Errore    <> '0'
145800000000     C                   Eval      Errore   = '2'
145900000000     C                   Exsr      ElaErrori
146000000000     C                   Exsr      EndPgm
146100000000     C                   EndIf
146200000000     C*
146300000000     C                   EndSR
146400000000     C************************************************************
146500000000     C* Reperisci dati persona fisica attraverso soggetto
146600000000     C************************************************************
146700000000     C     RepPersFis    BegSr
146800000000     C*
146900000000     C                   Clear                   $CdFiscSog
147000000000     C                   Clear                   $CdIVASog
147100000000     C                   Clear                   $CogRagSog
147200000000     C                   Clear                   $NomeSog
147300000000     C                   Clear                   $DtNascSog
147400000000     C                   Clear                   $LocNascSog
147500000000     C                   Clear                   $PrvNascSog
147600000000     C                   Clear                   $StatoSog
147700000000     C                   Clear                   $StaFedSog
147800000000     C                   Clear                   $LocalitSog
147900000000     C                   Clear                   $IndirESog
148000000000     C                   Clear                   $SexSog
148100000000     C*
148200000000     C                   Eval      SO3Sogg     = SocSogg
148300000000     C                   Move      *Loval        SO3DtFiVl
148400000000     C                   Eval      SO3Sys      = 0
148500000000     C                   Eval      SO3NrAsInt  = 0
148600000000     C*
148701160524R224 X**** K04SO301      CHAIN     SDGSO301L                          21
148702160524R224 C                   EXSR      ChainSDGSO3
148703160524R224 C*
148800000000     C                   If        *In21       = *Off
148900000000     C                   MoveL(P)  SO3CdFisc     $CdFiscSog
149000000000     C                   MoveL(P)  SO3PartIva    $CdIVASog
149100000000     C                   MoveL(P)  SO3CogRag     $CogRagSog
149200000000     C                   MoveL(P)  SO3Nome       $NomeSog
149300000000     C                   Move      SO3DtNasc     $DtNascSog
149400000000     C                   MoveL(P)  SO3LocNasc    $LocNascSog
149500000000     C                   MoveL(P)  SO3PrvNasc    $PrvNascSog
149600000000     C                   MoveL(P)  SO3Stato      $StatoSog
149700000000     C                   MoveL(P)  SO3StaFed     $StaFedSog
149800000000     C                   MoveL(P)  SO3Localit    $LocalitSog
149900000000     C                   MoveL(P)  SO3IndirE     $IndirESog
150000000000     C                   MoveL     SO3Sex        $SexSog
150100000000     C                   EndIf
150200000000     C*
150300000000     C                   EndSR
150301160524R224 C************************************************************
150302160524 "   C* Lettura dati aggiuntivi anagrafici
150303160524 "   C************************************************************
150304160524R224 C     ChainSDGSO3   BEGSR
150305160524     C*
150309160609     C                   Eval      SO3Dest     = $Dest
150316160524     C*
150317160524     C     K05SO301      CHAIN     SDGSO301L                          21
150318160524     C*
150319160524     C* se non trovato, provo con la destinazione Generica
150320160524     C                   if        *In21       = *On     and
150321160524     C                             SO3Dest    <> *Blank
150322160524     C                   Eval      SO3Dest     = *Blank
150323160524     C     K05SO301      CHAIN     SDGSO301L                          21
150324160524     C                   endif
150325160524     C*
150326160524R224 C                   ENDSR
150327160524R224 C/EJECT
150400000000     C************************************************************
150500000000     C* Gestione file per invio telematico
150600000000     C************************************************************
150700000000     C     GesFilTel     BegSr
150800000000     C*
150900140328     C                   Move      ANNOG64       AnnoA             4
151000140401     C                   Move      MeseG64       MeseA             2
151100000000     C                   MoveL     AnnoA         $DesFileI
151200000000     C                   Move      UDateIso      $DtAlfa
151300000000     C*
151400000000     C* Creo già la descrizione del file
151500000000     C                   Clear                   $DesFileO
151600140401     C     AnnoA         Cat       '/'           $DesFileO
151700140401     C                   Cat       MeseA:0       $DesFileO
151800000000     C                   Cat       CostFile:1    $DesFileO
151900000000     C                   Cat       '- Soc.':1    $DesFileO
152000000000     C                   Cat       XscSoc :1     $DesFileO
152100000000     C                   Cat       'del':1       $DesFileO
152200000000     C                   Cat       $DtAlfa:1     $DesFileO
152300000000     C*
152400140328     C                   Eval      $Libreria   = LIBG64
152500000000     C                   Eval      $Errore     = *Off
152600000000     C*
152700000000     C                   CALL      'SDGSG61C1'
152800000000     C                   PARM                    $File
152900000000     C                   PARM                    $Libreria
153000000000     C                   PARM                    WTpDist           2
153100000000     C                   PARM                    $DtAlfa          10
153200000000     C                   PARM                    $DesFileI         8
153300000000     C                   PARM                    $DesFileO        50
153400000000     C                   PARM                    $Errore           1
153500000000     C*
153600000000     C                   if        $Errore    <> '0'
153700000000     C                   Eval      Errore   = '3'
153800000000     C                   Exsr      ElaErrori
153900000000     C                   Exsr      EndPgm
154000000000     C                   EndIf
154100000000     C*
154200140328     C                   Eval      FILETELG64  = $FILE
154300000000     C*
154400000000     C                   EndSR
154500000000     C************************************************************
154600000000     C* Iscrizione a catalogo del file scarico
154700000000     C************************************************************
154800000000     C     IscrCatF      BegSr
154900000000     C*
155000000000     C
155100000000     C                   Eval      WNumero     = *All'0'
155200140328     C                   Z-Add     ANNOG64       WAnno
155300140401     C                   Z-Add     MESEG64       WPrgInvio
155400000000     C                   Move      WNumero       $Numero
155500000000     C*
155600000000     C                   Move      UDateIso      $DtAlfa
155700000000     C*
155800000000     C                   Call      'X36CATFR'
155900140328     C                   PARM                    SOCIETAG64
156000000000     C                   PARM      *Blank        $Unita
156100000000     C                   PARM                    $File
156200000000     C                   PARM                    $Libreria
156300000000     C                   PARM                    $DesFileO
156400140401     C                   PARM      'SM'          $CdScarico        2
156500000000     C                   PARM      'P'           $TpSupp           1
156600000000     C                   PARM      *Blank        $NmSupp          10
156700000000     C                   PARM      '4'           $TpDist           1
156800000000     C                   PARM                    $DtAlfa          10
156900000000     C                   PARM                    $Numero           9
157000000000     C                   PARM      *Blank        $DesBan          44
157100000000     C                   PARM                    KNmus
157200000000     C                   PARM      '0'           Sospeso           1
157300000000     C                   PARM      '1'           Video             1
157400000000     C*
157500000000     C                   EndSR
157600000000     C************************************************************
157700000000     C* Inizializzazioni variabili
157800000000     C************************************************************
157900000000     C     InzVar        BegSr
158000000000     C*
158100140328     C                   MoveL     KPJBU         SDGSG64DS
158200000000     C*
158300140328     C                   Eval      ERRG64      = *Off
158400140328     C                   Eval      MAXRECG64   = MAXRECG64 -4
158500000000     C*
158600000000     C                   MoveL     'SOC001'      TIPXSC
158700140328     C                   MoveL     SOCIETAG64    SOCXSC
158800000000     C*
158900000000     C                   Exsr      RepSoc
159000000000     C*
159100000000     C                   If        RTNXSC     <> '1'
159200000000     C                   MoveL     XSOCDS        SOC001
159300000000     C                   ElSe
159400000000     C                   Eval      $Errori     = *On
159500000000     C                   Exsr      EndPgm
159600000000     C                   EndIf
159700000000     C*
159800000000     C* se definito limite max per il file da creare
159900000000     C* definisco i parametri per l'invio multiplo
160000140328     C                   if        MAXDIMG64   > 0   or
160100140328     C                             MAXRECG64   > 0
160200000000     C****               Exsr      DefLimiti
160300000000     C                   EndIf
160400000000     C*
160500000000     C                   Move      XscSoc        SIFSocieta
160600000000     C     K01Sif01      Chain     AnSif01L                           21
160700000000     C  N21              Move      SifFlgOpe     WrkFlgOpe
160800000000     C   21              Move      *Blanks       SocSogg
160900000000     C*
161000000000     C* reperisco DATFMT del job
161100000000     C                   CallB     'XDATFMT'
161200000000     C                   Parm                    $DatFmt           4
161300000000     C*
161400000000     C                   Setoff                                       77
161500000000     C*
161600000000     C* Recupera stato UIC della societa
161700000000     C                   Move      XscSta        WrkStato
161800000000     C                   Exsr      RepStato
161900000000     C                   Move      CODUICG12     UICSocieta
162000000000     C*
162100000000     C                   SetOn                                        OF
162101160609R224 C*
162102160609R224 C                   EXSR      RepDatiCon
162200000000     C*
162300000000     C                   EndSR                                                  *
162400000000     C*
162401160609R224 C************************************************************
162402160609 "   C* REPERIMENTO dati anagafici aggiuntivi contribuente
162403160609 "   C************************************************************
162404160609R224 C     RepDatiCon    BegSr
162405160609     C*
162406160609     C* preparo i parametri di chiamata al driver
162407160609     C                   Clear                   XMPEDS
162408160609     C                   EVAL      TpChiamata = '01'
162409160609     C                   EVAL      NChiavi = 5
162410160609     C                   EVAL      VLogica = 2
162411160609     C*
162412160609     C* preparo le chiavi per la chiamata al driver
162413160609     C                   Clear                   XMPEDS1
162414160609     C                   MoveL     XSCSOC        DSMPESOC
162415160609     C                   MoveL     'SDGSG60R  '  DSMPEPGM
162416160609     C*
162418160609     C* Acquisti da San Marino
162420160609     C                   Move      'SC'          DSMPEPGM
162427160609     C*
162428160609     C                   MoveL     *Blanks       DSMPEPRF
162429160609     C* Si reperiscono i dati di default a seconda delle modalita con
162430160609     C* cui il pgm e usato
162431160609     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
162432160609     C* MPETIP = 'PV'  pgm richiamato da menu
162433160609     C* MPETIP = 'PB'  x lancio Batch
162434160609     C                   MoveL     'PB'          DSMPETIP
162435160609     C*
162436160609     C                   CALLB     'XMPE'
162437160609     C                   PARM                    XMPEDS
162438160609     C                   PARM                    XMPEDS1
162439160609     C*
162440160609     C     Risultato     IfEq      '0'
162441160609     C                   MoveL     DSMPEPAR      DSContrib
162442160609     C                   EndIf
162443160609     C*
162444160609R224 C                   EndSr
162500000000     C******************************************************
162600000000     C* Inizializzo campi per la gestione della tabella
162700000000     C******************************************************
162800000000     C     InzSkiera     BEGSR                                                  *
162900000000     C*
163000000000     C                   Reset                   Skiera
163100000000     C                   Z-ADD     0             $I
163200000000     C                   Z-ADD     0             Conta_Rigo
163300000000     C                   Z-ADD     0             Conta_Colo
163400000000     C*
163500000000     C                   ENDSR
163600000000     C/EJECT
163700000000     C************************************************************
163800000000     C* Azzeramento progressivi
163900000000     C************************************************************
164000000000     C     AzzeraProg    BegSr
164100000000     C*
164200000000     C                   Z-Add     0             NrETW
164300000000     C*
164400000000     C                   Z-Add     0             Cnt_C
164500000000     C                   Z-Add     0             Cnt_D
164600000000     C*
164700000000     C                   Z-Add     0             CntSE
164800140328     C                   Z-Add     0             ProgMod
164900000000     C                   Z-Add     0             Prog_SE
165000000000     C                   Z-Add     0             ProgMod_SE
165100000000     C*
165200000000     C                   Z-Add     0             $NrRecTot
165300000000     C*
165400000000     C                   Z-ADD     0             WRKPRGMOD         7 0
165500000000     C*
165600000000     C                   EndSR                                                  *
165700000000     C************************************************************
165800000000     C* Parametri per invio multiplo
165900000000     C************************************************************
166000000000     C     DefLimiti     BegSr
166100000000     C*
166200140328     C                   if        MAXDIMG64   > 0
166300000000     C*
166400000000     C* dimensione max 3MG = 3 * 1024 KB * 1024 byte = 3.145.728 byte
166500000000     C* diviso 1800 byte (=caratteri per record)= 1747,63 record circa
166600000000     C* meno 2 record (0 testa, 9 coda) = 1745 righe dati
166700000000     C* se presenti più di 1745 righe chiamo pgm per creazione
166800000000     C* di più file per multi invio
166900000000     C*
167000140328     C                   Z-Add     MAXDIMG64     WImpInp
167100000000     C*
167200000000     C                   Eval      LenEPW = %size(SDGETWDS)
167300000000     C*
167400000000     C                   CallP     X0201MultC(WImpInp:
167500000000     C                                        ($KByte * $KByte):
167600000000     C                                        LenEPW:
167700000000     C                                        '-': '0': 0: WImpOut)
167800000000     C*
167900000000     C                   Eval      $MaxRighe   = WImpOut - 3
168000000000     C*
168100000000     C                   EndIf
168200000000     C*
168300000000     C                   EndSR
168400000000     C************************************************************
168500000000     C* OPERAZIONI INIZIALI
168600000000     C************************************************************
168700000000     C     *INZSR        BegSr
168800000000     C*
168900000000     C* Reperimento parametri
169000000000     C*
169100000000     C     *ENTRY        PLIST                                                  *
169200000000     C                   PARM                    KPJBA                          *
169300000000     C*
169400000000     C     *DMY          Move      Udate         UDateISO
169500000000     C*
169600000000     C                   EndSR
169700000000     C************************************************************
169800000000     C* DEFINIZIONE KLIST
169900000000     C************************************************************
170000000000     C     DefKlist      BegSr
170100000000     C*
170200000000     C* KLIST
170300000000     C*
170400000000     C* SDGSo301L
170401160524R224 X**** K04SO301      KLIST
170402160524R224 C     K05SO301      KLIST
170600000000     C                   KFLD                    SO3Sogg
170700000000     C                   KFLD                    SO3DtFiVL
170800000000     C                   KFLD                    SO3Sys
170900000000     C                   KFLD                    SO3NrAsint
170901160524R224 C                   KFLD                    SO3Dest
171000000000     C*
171100000000     C* SDGETW01L
171200000000     C     K01ETW01      KLIST
171300000000     C                   KFLD                    ETWSocieta
171400000000     C*
171500000000     C* AnSif01L
171600000000     C     K01SIF01      KLIST
171700000000     C                   KFLD                    SIFSocieta
171800000000     C*
171900000000     C                   EndSR
172000000000     C************************************************************
172100140401     C* Reperimento parametri di setup per spesometro
172200000000     C************************************************************
172300000000     C     RepParMod     BEGSR
172400000000     C*
172500000000     C                   Clear                   SDGPA08DS
172600000000     C*
172700000000     C                   CALLB     'XPAR'
172800000000     C                   Parm                    XScSoc
172900000000     C                   Parm      'SDGSPECF'    XPaPar            8
173000000000     C                   Parm                    XPaOut         2000
173100000000     C                   Parm      *OFF          XPaErr            1
173200000000     C                   Parm      ' '           XPaRic            1
173300000000     C*
173400000000     C                   if        XPaErr     <> *On
173500000000     C                   MoveL     XPaOut        SDGPA08DS
173600000000     C                   EndIf
173700000000     C*
173800000000     C                   EndSR
173900000000     C************************************************************
174000000000     C* Reperimento dati Intermediario
174100000000     C************************************************************
174200000000     C     RepDatInter   BEGSR
174300000000     C*
174400000000     C* preparo i parametri di chiamata al driver
174500000000     C                   Clear                   XMPEDS
174600000000     C                   EVAL      TpChiamata = '01'
174700000000     C                   EVAL      NChiavi = 5
174800000000     C                   EVAL      VLogica = 2
174900000000     C*
175000000000     C* preparo le chiavi per la chiamata al driver
175100000000     C                   Clear                   XMPEDS1
175200000000     C                   MoveL     XSCSOC        DSMPESOC
175300000000     C                   MoveL     'SDGSG60R R'  DSMPEPGM
175400140422R133 C****               Move      'R'           DSMPEPGM
175500140422R133 C                   Move      'S'           DSMPEPGM
175600000000     C                   MoveL     *blanks       DSMPEPRF
175700000000     C* Si reperiscono i dati di default a seconda delle modalità con
175800000000     C* cui il pgm è usato
175900000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
176000000000     C* MPETIP = 'PV'  pgm richiamato da menù
176100000000     C* MPETIP = 'PB'  x lancio Batch
176200000000     C                   MoveL     'PB'          DSMPETIP
176300000000     C*
176400000000     C                   CALLB     'XMPE'
176500000000     C                   PARM                    XMPEDS
176600000000     C                   PARM                    XMPEDS1
176700000000     C*
176800000000     C     Risultato     IfEq      '0'
176900000000     C                   MoveL     DSMPEPAR      DSInterm
177000000000     C                   EndIf
177100000000     C*
177200000000     C                   EndSR
177300000000     C************************************************************
177400000000     C* inz ds
177500000000     C************************************************************
177600000000     C     Inz0502R      BegSr
177700140328     C*
177800140328     C* Preparo dati per sql
177900140328     C                   RESET                   SDG00502DS
178000140328     C*
178100140328     C                   Eval      Soc0502     = SOCIETAG64
178200140328     C                   Eval      Anno0502    = ANNOG64
178300140328     C                   if        NoteVarA08  = *Blank     or
178400140328     C                             NoteVarA08  = '1'
178500140328     C                   Eval      NoteVa0502  = '1'
178600140328     C                   else
178700140328     C                   Eval      NoteVa0502  = '2'
178800140328     C                   endif
178900140328     C                   Eval      Opz0502     = '05'
179000140328     C                   Eval      TpRott0502  = '01'
179100140328     C                   Eval      Rott0502    = '02'
179200140328     C                   Eval      Ric0502     = '0'
179300140328     C                   Eval      TpList0502  = '02'
179400140328     C*
179500140328     C* chiesti solo gli attivi
179600140328     C                   Eval      Ann0502     = '2'
179700140328     C                   Eval      TpAnn0502   = *Hival
179800000000     C                   Eval      TpN0502     = *Hival
179900000000     C                   Eval      TpNr0502    = *Hival
180000000000     C*
180100140328     C                   Eval      TpCom0502    = 'S'
180200140328     C                   Eval      Anno0502     = ANNOG64
180300140328     C                   Eval      Mese0502     = MESEG64
180400140328     C*
180500140328     C                   Eval      CF0502      = *Hival
180600140328     C*
180700140328     C                   MoveL     *Hival        Sogg0502
180800140328     C                   MoveL     *Hival        Causal0502
180900140328     C*
181000000000     C                   Eval      Stato0502    = *Hival
181100000000     C*
181200000000     C                   Eval      PIva0502     = *Hival
181300000000     C                   Eval      CdFisc0502   = *Hival
181400000000     C*
181500000000     C                   Eval      Quadro0502   = *Hival
181600000000     C                   Eval      TpOpe0502    = *Hival
181700000000     C*
181800000000     C                   Eval      CogRag0502   = *Hival
181900000000     C                   Eval      Nome0502     = *Hival
182000000000     C                   Eval      DtNasc0502   = *Hival
182100000000     C                   Eval      LocNas0502   = *Hival
182200000000     C                   Eval      PrvNas0502   = *Hival
182300000000     C                   Eval      CodUIC0502   = *Hival
182400000000     C                   Eval      Locali0502   = *Hival
182500000000     C                   Eval      IndirE0502   = *Hival
182600000000     C*
182700000000     C                   MoveL     *Hival        COGRAR0502
182800000000     C                   MoveL     *Hival        NOMER0502
182900000000     C                   MoveL     *Hival        DTNASR0502
183000000000     C                   MoveL     *Hival        LOCNAR0502
183100000000     C                   MoveL     *Hival        PRVNAR0502
183200000000     C                   MoveL     *Hival        STATOR0502
183300000000     C                   MoveL     *Hival        CODUIR0502
183400000000     C                   MoveL     *Hival        LOCALR0502
183500000000     C                   MoveL     *Hival        INDIER0502
183600000000     C*
183700000000     C                   MoveL     *Hival        PIVAR0502
183800000000     C                   MoveL     *Hival        CDFISR0502
183900000000     C                   MoveL     *Hival        TPOPER0502
184000000000     C                   MoveL     *Hival        NATOPR0502
184100000000     C                   MoveL     *Hival        LEASIR0502
184200000000     C                   MoveL     *Hival        DTREGR0502
184300000000     C                   MoveL     *Hival        NRREGR0502
184400000000     C                   MoveL     *Hival        SERRER0502
184500000000     C*
184600140328     C                   EndSR
184700140328     C************************************************************
184800140328     C* Calcola totale importi rigo quadri
184900140328     C************************************************************
185000140328     C     CalcWrkTotale BegSr
185100140328     C*
185200140328     C                   Eval      WrkTotale     = WrkImport01 +
185300140328     C                                             WrkImport02 +
185400140328     C                                             WrkImport03 +
185500140328     C                                             WrkImport04 +
185600140328     C                                             WrkImport05 +
185700140328     C                                             WrkImport06 +
185800140328     C                                             WrkImport07 +
185900140328     C                                             WrkImport08 +
186000131206     C                                             WrkImport09 +
186100131206     C                                             WrkImport10
186200140328     C*
186300140328     C                   EndSR
186400140328     C************************************************************
186500140328     C* Azzera importi pre totale
186600140328     C************************************************************
186700140328     C     AzzeraWrkImp  Begsr
186800140328     C*
186900140328     C                   Z-Add     0             WrkTotale
187000140328     C                   Z-Add     0             WrkImport01
187100140328     C                   Z-Add     0             WrkImport02
187200140328     C                   Z-Add     0             WrkImport03
187300140328     C                   Z-Add     0             WrkImport04
187400140328     C                   Z-Add     0             WrkImport05
187500140328     C                   Z-Add     0             WrkImport06
187600131206     C                   Z-Add     0             WrkImport07
187700131206     C                   Z-Add     0             WrkImport08
187800131206     C                   Z-Add     0             WrkImport09
187900131206     C                   Z-Add     0             WrkImport10
188000140328     C*
188100140328     C                   EndSR
188200140328     C************************************************************
188300131206     C* Stampa File telematico
188400131206     C************************************************************
188500131206     C     Stampafile    BegSr
188600131206     C*
188700160208R195 X****               MoveL     SDGSG64DS     KPJBU
188701160208R195 C*
188702160208 "   C                   Reset                   SDGSG61DS
188703160208 "   C*
188705160208 "   C                   Eval      SocietaG61  = SocietaG64
188706160208 "   C                   Eval      AnnoG61     = AnnoG64
188707160208 "   C*--------          Eval                  = MeseG64
188708160208 "   C                   Eval      TipInvG61   = TipInvG64
188709160208 "   C                   Eval      ProtocG61   = ProtocG64
188710160208 "   C                   Eval      ProtoDG61   = ProtoDG64
188711160208 "   C                   Eval      MaxDimG61   = MaxDimG64
188712160208 "   C                   Eval      MaxRecG61   = MaxRecG64
188713160208 "   C                   Eval      FileTelG61  = FileTelG64
188714160208 "   C**********         Eval      FileG61     =
188715160208 "   C                   Eval      LibG61      = LibG64
188716160208 "   C                   Eval      StpLG61     = StpLG64
188720160208 "   C*
188721160208 "   C* Acquisti da San Marino
188723160208 "   C                   Eval      TpComG61    = 'S'
188726160208R195 C*
188727160208R195 C                   MoveL     SDGSG61DS     KPJBU
188800000000     C*
188900151117R207 X***                CALL      'SDGSG61R1'
188901151117R207 C                   CALL      'SDGSG61R2'
189000000000     C                   PARM                    KPJBA                          *
189100000000     C*
189200000000     C                   EndSR
189300000000     C************************************************************
189400000000     C* FINE PROGRAMMA
189500000000     C************************************************************
189600000000     C     EndPgm        BegSr
189700000000     C*
189800000000     C                   if        $Errori     = *On
189900140328     C                   Eval      ERRG64      = '1'
190000000000     C                   EndIf
190100000000     C*
190200140328     C                   MoveL     SDGSG64DS     KPJBU
190300000000     C                   SETON                                        LR
190400000000     C*
190500000000     C                   RETURN
190600000000     C*
190700000000     C                   EndSR
190800000000     C************************************************************
190900000000     C* Elabora Errori
191000000000     C************************************************************
191100000000     C     ElaErrori     BEGSR
191200000000     C*
191300140328     C*
191400000000     C                   EndSr
191500000000     C************************************************************
