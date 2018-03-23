000100000000     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND SDG005) ACTGRP(*CALLER)
000200000000     H*PARMS OPTION(*NOXREF *NODEBUGIO)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      ********************************************************************
000500000000      *
000600000000      *         ------------------------------------------------
000700000000      *                   Comunicazione Operazioni Iva
000800000000      *                    Creazione file telematico
000900000000      *         ------------------------------------------------
001000000000      *
001100000000     F*----------------------------------------------------------------
001200000000     F* File contenente i dati da trasmettere all'agenzia
001300000000     FSDGETW00F UF A E             DISK
001400000000     F                                     InfDs(InfDsETW)
001500000000     FSDGETW01L IF   E           K DISK    RENAME(SDGETW000:SDGETW01)
001600000000     F*-------------
001700000000     FSDGETE00F O    E           K DISK    UsrOpn
001800000000     F*-------------
001900000000     F* Dati anagrafici aggiuntivi
001902160524R224 X**** Key: Sogg/DtFiVl/Sys/NrAsInt      UNIQUE
001903160524R224 F* Key: Sogg/DtFiVl/Sys/NrAsInt/Dest      UNIQUE
002100000000     FSDGSO301L IF   E           K DISK
002200000000     F*-------------
002300000000     FANSIF01L  IF   E           K DISK
002400000000     F*-------------
002500000000     F* Printer file errori
002600000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002700000000     D*-------------
002800000000     D* Prototipo per la procedura Euro
002900000000     D/COPY *LIBL/SRCCPY,PJX002PR
003000000000     D*-------------
003100000000     D InfDsETW        DS           512
003200000000     D  NrrEtw               397    400B 0
003300000000     D*-------------
003400000000     D* campo di comodo per calcolo importo EURO arrotondato
003500000000     D W30S15          S             30S15
003600000000     D*----------------
003700000000     D* Parametro di base
003800000000     D XPAOUT          DS          2000
003900000000     D*----------------
004000000000     D* PARAMETRI ESTERNI
004100000000     D KPJBA         E DS
004200000000     D*----------------
004300000000     D* Valori assunti
004400000000     D XMPEDS        E DS                  INZ
004500000000     D XMPEDS1       E DS                  INZ
004600000000     D DSInterm        DS                  INZ
004700000000     D $FisInt                             Like(G61BCFINT  )
004800000000     D $CafInt                        5A
004900000000     D $ImpTrs                        1A
005000000000     D $ImpDta                       10A
005100000000     D $FISSOG                       16A
005200000000     D $CARSOG                        2A
005300000000     D $DTISOG                       10A
005400000000     D $DTFSOG                       10A
005500000000     D $COGSOG                       24A
005600000000     D $NOMSOG                       20A
005700000000     D $SEXRSOG                       1A
005800000000     D $DTNSOG                       10A
005900000000     D $COMSOG                       40A
006000000000     D $PRVSOG                        2A
006100000000     D $RAGSOG                       24A
006101160609R224 D*-------------
006102160609 "   D DSContrib       DS                  INZ
006103160609R224 D $Dest                          2A
006200000000     D*---------
006300000000     D $Data           S               D
006400000000     D $Data10A1       S             10
006500000000     D $Data10A2       S             10
006600000000     D*---------
006700000000     D DataDs          DS
006800000000     D  AnnoDs                        4A
006900000000     D   Tratt1                       1A   Inz('-')
007000000000     D  MeseDs                        2A
007100000000     D   Tratt2                       1A   Inz('-')
007200000000     D  GiornoDs                      2A
007300000000     D*-------------
007400000000     D $CdFiscSog      S                   Like(SO3CdFisc  )
007500000000     D $CdIVASog       S                   Like(SO3PartIva )
007600000000     D $CogRagSog      S                   Like(SO3CogRag  )
007700000000     D $NomeSog        S                   Like(SO3Nome    )
007800000000     D $DtNascSog      S                   Like(SO3DtNasc  )
007900000000     D $LocNascSog     S                   Like(SO3LocNasc )
008000000000     D $PrvNascSog     S                   Like(SO3PrvNasc )
008100000000     D $StatoSog       S                   Like(SO3Stato   )
008200000000     D $StaFedSog      S                   Like(SO3StaFed  )
008300000000     D $LocalitSog     S                   Like(SO3Localit )
008400000000     D $IndirESog      S                   Like(SO3IndirE  )
008500000000     D $SexSog         S                   Like(SO3Sex     )
008600000000     D*----------------
008700000000     D* Parametri ricevuti
008800000000     D SDGSG61DS     E DS
008900000000     D*-------------
009000000000     D* Ds tracciati appoggio
009100000000     D SDGSG61DSA    E DS
009200000000     D SDGSG61DSB    E DS
009300000000     D SDGSG61DSC    E DS
009400000000     D SDGSG61DSD    E DS
009500000000     D SDGSG61DSE    E DS
009600000000     D SDGSG61DSZ    E DS
009700000000     D*-------------
009800000000     D* Ds tracciati record
009900000000     D SDG005BLDS    E DS
010000000000     D SDG005DFDS    E DS
010100000000     D SDG005FADS    E DS
010200000000     D SDG005FEDS    E DS
010300000000     D SDG005FNDS    E DS
010400000000     D SDG005FRDS    E DS
010500000000     D SDG005NEDS    E DS
010600000000     D SDG005NRDS    E DS
010700000000     D SDG005SADS    E DS
010800000000     D SDG005SEDS    E DS
010900000000R136 D SDG005TUDS    E DS
011000000000     D SDG005TADS    E DS
011100000000     D*-------------
011200000000     D* Parametri modulo SDGSPECF
011300000000     D SDGPA08DS     E DS
011400160202R195 D*-------------
011500160202 "   D* Parametri modulo SDBLACK2
011600160202R195 D SDGPA11DS     E DS
011700000000     D*-------------
011800000000     D* Dati di ambiente ottenuti da SDGSOCDR
011900000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
012000000000     D*-------------
012100000000     D* DS Interna per dati di output di SDGSOCDR
012200000000     D XSOCDS          S           1000
012300000000     D*----------------
012400000000     D* Reperimento nome PGM
012500000000     D STATUS         SDS           333
012600000000     D  DSPGM            *PROC
012700000000     D*----------------
012800000000     D* DS modulo dati
012900000000     D SDG00502DS    E DS                  INZ
013000000000     D*
013100000000     D SDG00592DS    E DS                  INZ
013200000000     D*----------------
013300000000     D* Richiamo a SDGTABDR
013400000000     D SDGTABDS      E DS                  inz
013500000000     D*----------------
013600000000     D* tabelle delle nature giuridiche
013700000000     D ANGG16DS      E DS                  INZ
013800000000     D*-------------
013900000000     D* Stati
014000000000     D ANGG12DS      E DS                                    INZ
014100000000     D*------------
014200000000     D* tabella stati BL
014300000000     D SDGHD1DS      E DS                  INZ
014400000000     D*------------
014500000000     D* Immagine SDGETW00F
014600000000     D SDGETWDS      E DS                  ExtName(SDGETW00F)
014700000000     D*----------------
014800000000     D* campi di lavoro
014900000000     D $NrRec          S              9S 0 Inz(*Zero)
015000000000     D $NrRecTot       S              9S 0 Inz(*Zero)
015100000000     D $TpRec          S              1A
015200000000     D DataISO         S               D
015300000000     D TipRec          S              1A
015400160202R195 D $Formato        S                   Like(FormatoA08)
015500160202 "   D $IdProd         S                   Like(IdProdA08 )
015600160202R195 D $NoteVar        S                   Like(NoteVarA08)
015700000000     D*----------------
015800000000     D Importo19_3     S             19S 3
015900000000     D Importo16_0     S             16S 0
016000000000     D Importo16A      S             16A
016100000000     D Importo20_0     S             20S 0
016200000000     D Importo20A      S             20A
016300000000     D Importo0A       S             16A   Inz('               0')
016400000000     D*-------------
016500000000     D UDateISO        S               D
016600000000     D $TpErrore       S              2A
016700000000     D $Errori         S              1A   Inz('0')
016800000000     D $FineETW        S              1A   Inz('0')
016900000000     D $FineSCF        S              1A   Inz('0')
017000000000     D $FineEP3        S              1A   Inz('0')
017100000000     D NrEPW           S              9S 0
017200000000     D Errore          S              1A
017300000000     D $FlgNeg         S              1A   Inz('0')
017400000000     D*-------------
017500000000     D WrKNatGiu       S                   Like(XscNcr    )
017600000000     D UICSocieta      S                   Like(CODUIC0592)
017700000000     D WrkStato        S                   Like(STATO0592 )
017800000000     D WSavQua         S                   Like(Quadro0592)
017900000000     D*----------------
018000000000     D* Ds flag AnSif
018100000000     D                 DS
018200000000     D  SocSogg               31     38
018300000000     D  WrkFlgOpe              1     50
018400000000     D*----------------
018500000000     D* Ds per nome file
018600000000     D                 DS
018700000000     D  WAnno                  5      8  0
018800000000     D  WPrgInvio              9     10  0
018900000000     D  WNumero                1     10
019000000000     D*----------------
019100000000     D* variabili per invio multiplo
019200000000     D $MultiInvio     S              1A   Inz(*Off)
019300000000     D $MaxRighe       S              6S 0 Inz(0)
019400000000     D $NrInvii        S              5S 0 Inz(0)
019500000000     D $ProgrInvio     S              5S 0 Inz(0)
019600000000     D $KByte          C                   Const(1024)
019700000000     D LenEPW          S              9B 0
019800000000     D*-------------
019900000000     D WImpInp         S                   Like(ImporD0202)
020000000000     D WImpOut         S                   Like(ImporD0202)
020100000000     D*-------------
020200000000     D DtAMG           DS
020300000000     D  AMGAnno                1      4
020400000000     D  T1                     5      5
020500000000     D  AMGMese                6      7
020600000000     D  T2                     8      8
020700000000     D  AMGGiorno              9     10
020800000000     D*-------------
020900000000     D DtGMA           DS
021000000000     D  GMAGiorno              1      2  0
021100000000     D  GMAMese                3      4  0
021200000000     D  GMAAnno                5      8  0
021300000000     D*-------------
021400000000     D WrkDt           S              8A
021500000000     D WrkProgMod      S              7S 0 Inz(*Zero)
021600000000     D*---------------------------------------------------------------*
021700000000     D DatMin          C                   CONST('0001-01-01')
021800000000     D CostFile        C                   CONST('Spesometro')
021900160204R195 D CostFileB       C                   CONST('Black List annuale')
022000000000     D*----------------
022100000000     D* Nr moduli
022200000000     D NrModFA         S              7S 0 Inz(*Zero)
022300000000     D NrModSA         S              7S 0 Inz(*Zero)
022400000000     D NrModBL         S              7S 0 Inz(*Zero)
022500000000     D NrModFE         S              7S 0 Inz(*Zero)
022600000000     D NrModFR         S              7S 0 Inz(*Zero)
022700000000     D NrModNE         S              7S 0 Inz(*Zero)
022800000000     D NrModNR         S              7S 0 Inz(*Zero)
022900000000     D NrModDF         S              7S 0 Inz(*Zero)
023000000000     D NrModFN         S              7S 0 Inz(*Zero)
023100000000     D NrModSE         S              7S 0 Inz(*Zero)
023200000000     D NrModTU         S              7S 0 Inz(*Zero)
023300000000     D*----------------
023400000000     D NrETW           S              7S 0 Inz(*Zero)
023500000000     D*
023600000000     D TotRec          S              7S 0 Inz(*Zero)
023700000000     D*
023800000000     D Cnt_C           S              7S 0 Inz(*Zero)
023900000000     D Cnt_D           S              7S 0 Inz(*Zero)
024000000000     D Cnt_Righe       S              7S 0 Inz(*Zero)
024100000000     D*
024200000000     D CntFA           S              7S 0 Inz(*Zero)
024300000000     D CntSA           S              7S 0 Inz(*Zero)
024400000000     D CntBL           S              7S 0 Inz(*Zero)
024500000000     D CntFE           S              7S 0 Inz(*Zero)
024600000000     D CntFR           S              7S 0 Inz(*Zero)
024700000000     D CntNE           S              7S 0 Inz(*Zero)
024800000000     D CntNR           S              7S 0 Inz(*Zero)
024900000000     D CntDF           S              7S 0 Inz(*Zero)
025000000000     D CntFN           S              7S 0 Inz(*Zero)
025100000000     D CntSE           S              7S 0 Inz(*Zero)
025200000000R136 D CntTU           S              7S 0 Inz(*Zero)
025300000000     D Prog_FA         S              3S 0 Inz(*Zero)
025400000000     D Prog_SA         S              3S 0 Inz(*Zero)
025500000000     D Prog_BL         S              3S 0 Inz(*Zero)
025600000000     D Prog_FE         S              3S 0 Inz(*Zero)
025700000000     D Prog_FR         S              3S 0 Inz(*Zero)
025800000000     D Prog_NE         S              3S 0 Inz(*Zero)
025900000000     D Prog_NR         S              3S 0 Inz(*Zero)
026000000000     D Prog_DF         S              3S 0 Inz(*Zero)
026100000000     D Prog_FN         S              3S 0 Inz(*Zero)
026200000000     D Prog_SE         S              3S 0 Inz(*Zero)
026300000000R136 D Prog_TU         S              3S 0 Inz(*Zero)
026400000000R139 D ProgMod         S              7S 0 Inz(*Zero)
026500000000     D ProgMod_FA      S              3S 0 Inz(*Zero)
026600000000     D ProgMod_SA      S              3S 0 Inz(*Zero)
026700000000     D ProgMod_BL      S              3S 0 Inz(*Zero)
026800000000     D ProgMod_FE      S              3S 0 Inz(*Zero)
026900000000     D ProgMod_FR      S              3S 0 Inz(*Zero)
027000000000     D ProgMod_NE      S              3S 0 Inz(*Zero)
027100000000     D ProgMod_NR      S              3S 0 Inz(*Zero)
027200000000     D ProgMod_DF      S              3S 0 Inz(*Zero)
027300000000     D ProgMod_FN      S              3S 0 Inz(*Zero)
027400000000     D ProgMod_SE      S              3S 0 Inz(*Zero)
027500000000R136 D ProgMod_TU      S              3S 0 Inz(*Zero)
027600000000     D*----------------
027700131206R141 D WrkImport01     S              9S 0 Inz(*Zero)
027800131206R141 D WrkImport02     S              9S 0 Inz(*Zero)
027900131206R141 D WrkImport03     S              9S 0 Inz(*Zero)
028000131206R141 D WrkImport04     S              9S 0 Inz(*Zero)
028100131206R141 D WrkImport05     S              9S 0 Inz(*Zero)
028200131206R141 D WrkImport06     S              9S 0 Inz(*Zero)
028300131206R141 D WrkImport07     S              9S 0 Inz(*Zero)
028400131206R141 D WrkImport08     S              9S 0 Inz(*Zero)
028500131206R141 D WrkImport09     S              9S 0 Inz(*Zero)
028600131206R141 D WrkImport10     S              9S 0 Inz(*Zero)
028700131206R141 D WrkTotale       S              9S 0 Inz(*Zero)
028800131206R141 D*----------------
028900140414R147 D Togli_Fa        S              7S 0 Inz(*Zero)
029000150306 "   D Togli_BL        S              7S 0 Inz(*Zero)
029100150309 "   D Numero_FA       S              7S 0 Inz(*Zero)
029200150309 "   D Numero_BL       S              7S 0 Inz(*Zero)
029300140414R147 D*----------------
029400000000     D* indentificativi campo
029500000000     D Conta_Rigo      S              3S 0 Inz(*Zero)
029600000000     D Conta_Colo      S              3S 0 Inz(*Zero)
029700000000     D*--
029800000000     D* tabella per i dati NON posizionali dei record 1 e 2 (1680 caratt.)
029900000000     D Max$I           C                   CONST(100)
030000000000     D Skiera          S             24    DIM(Max$I)
030100000000     D $I              S              3S 0
030200000000     D*--
030300000000     D Sk_Campo        S              1    DIM(Max$X)
030400000000     D Max$X           C                   CONST(61)
030500000000     D $X              S              3S 0
030600000000     D*--
030700000000     D* ogni elemento della tabella Skiera è costituito da:
030800000000     D* - 1 campo-codice di 8 caratteri;
030900000000     D* - 1 campo-valore di 16 caratteri.
031000000000     D*
031100000000     D                 DS
031200000000     D  Cod_Elenco             1      2
031300000000     D  Cod_Rigo               3      5  0
031400000000     D  Cod_Colo               6      8  0
031500000000     D  Campo_Codice           1      8
031600000000     D*
031700000000     D                 DS
031800000000     D  Campo_Valore           1     16
031900000000     D  Caratt_45             17     61
032000000000     D*
032100000000     D*----------------
032200000000     D* questa schiera serve per togliere gli zeri non significativi
032300000000     D* alle caselle che contengono degli importi dei campi posizionali
032400000000     D*
032500000000     D Max$Z           C                   CONST(16)
032600000000     D $Z              S              3S 0
032700000000     D*
032800000000     D Sk_Importo      S              1    DIM(Max$Z)
032900000000     D*----------------
033000000000     D                 DS
033100000000     D  Campo                  1     16
033200000000     D  C15I                  17     61
033300000000     D    C15                        15    Dim(03) Overlay(C15I)
033400150312     D  C60                    1     61
033500150312R177 D*----------------
033600150312 "   D LettoRigo1      S              1A   Inz(*Off )
033700150312 "   D LettoRigo2      S              1A   Inz(*Off )
033800150312 "   D LettoRigo3      S              1A   Inz(*Off )
033900150312 "   D LettoRigo4      S              1A   Inz(*Off )
034000150312 "   D LettoRigo5      S              1A   Inz(*Off )
034100150312 "   D LettoRigo6      S              1A   Inz(*Off )
034200150312 "   D LettoRigo7      S              1A   Inz(*Off )
034300150312 "   D LettoRigo8      S              1A   Inz(*Off )
034400150312 "   D LettoRigo9      S              1A   Inz(*Off )
034500150312 "   D LettoRigo10     S              1A   Inz(*Off )
034600150316 "   D $NrQuaFA        S              7S 0 Inz(0    )
034700150316 "   D $NrQuaSA        S              7S 0 Inz(0    )
034800150316 "   D $NrQuaBL1       S              7S 0 Inz(0    )
034900150316 "   D $NrQuaBL2       S              7S 0 Inz(0    )
035000150316 "   D $NrQuaBL3       S              7S 0 Inz(0    )
035100150316 "   D $NrQuaFE1       S              7S 0 Inz(0    )
035200150316 "   D $NrQuaFE2       S              7S 0 Inz(0    )
035300150316 "   D $NrQuaFR1       S              7S 0 Inz(0    )
035400150316 "   D $NrQuaFR2       S              7S 0 Inz(0    )
035500150316 "   D $NrQuaNE        S              7S 0 Inz(0    )
035600150316 "   D $NrQuaNR        S              7S 0 Inz(0    )
035700150316 "   D $NrQuaDF        S              7S 0 Inz(0    )
035800150316 "   D $NrQuaFN        S              7S 0 Inz(0    )
035900150316 "   D $NrQuaSE        S              7S 0 Inz(0    )
036000150316R177 D $NrQuaTU        S              7S 0 Inz(0    )
036100000000     D*---------------------------------------------------------------*
036200000000     C*  CICLO PRINCIPALE DEL PROGRAMMA                               *
036300000000     C*---------------------------------------------------------------*
036400000000     C*
036500000000     C                   Exsr      INZVAR
036600000000     C*
036700000000     C* Param.Modulo
036800160202R195 C*
036900160202 "   C                   Select
037000160202 "   C* se Spesometro
037100160202R195 C                   When      TpComG61    = 'E'
037200000000     C                   Exsr      RepParMod
037300160202R195 C*
037400160202 "   C* se Black List Annuale
037500160202 "   C                   When      TpComG61    = 'B'
037600160202 "   C                   EXSR      RepSDBLACK2
037700160202 "   C*
037800160202R195 C                   endSl
037900000000     C*
038000000000     C* Dati Rappresentante
038100000000     C                   Exsr      RepDatInter
038200000000     C*
038300000000     C* Azzeramento
038400000000     C                   Exsr      AzzeraProg
038500000000     C*
038600000000     C*-----------------------------------------------
038700000000     C* LETTURA DATI
038800000000     C*
038900000000     C* Inizializzo chiamata modulo di lettura
039000000000     C                   Exsr      Inz0502R
039100000000     C*
039200000000     C* Elaborazione dati per preparazione invio
039300000000     C                   Exsr      ElaboraDati
039400000000     C*
039500000000     C* Determino nr invii
039600000000     C                   Exsr      RepNrInvii
039700000000     C*
039800000000     C                   If        $NrInvii      > 1
039900000000     C                   Eval      $MultiInvio    = *On
040000000000     C                   ElSe
040100000000     C                   Eval      $MultiInvio    = *Off
040200000000     C                   EndIf
040300000000     C*
040400000000     C*-----------------------------------------------
040500000000     C*
040600000000     C* ELABORAZIONE DATI per SCRITTURA TELEMATICO ---
040700000000     C*
040800000000     C*
040900000000     C* Se ho record allora continuo
041000000000a1   C                   If        $NrRecTot     > 0
041100000000     C*
041200000000     C* Mi posiziono all'inizio di ETW
041300000000     C                   Eval      $FineETW    = *Off
041400000000     C                   Exsr      PosizETW
041500000000     C*
041600000000     C* Unico invio  o multi invio
041700000000a2   C                   If        $MultiInvio    = *Off
041800000000     C                   Z-Add     1             $ProgrInvio
041900000000     C                   Exsr      GesSingInv
042000000000x2   C                   ElSe
042100000000     C                   Exsr      GesMultInv
042200000000e2   C                   EndIf
042300000000     C*
042400000000     C*
042500000000e1   C                   EndIf
042600000000     C*
042700000000     C                   Exsr      EndPgm
042800000000     C*
042900000000     C******************************************************
043000000000     C* Elaboro dati di invio
043100000000     C******************************************************
043200000000     C     ElaboraDati   BegSr                                                  *
043300000000     C*
043400000000     C                   Exsr      LetturaSCF
043500000000     C*
043600000000     C                   Move      QUADRO0592    WSavQua
043700000000     C*
043800000000     C                   DOW       $FineSCF    = *Off
043900000000     C*
044000000000     C                   Exsr      OpeBreak
044100000000     C*
044200000000     C                   Exsr      ElaboraSCF
044300000000     C*
044400000000     C                   Exsr      LetturaSCF
044500000000     C*
044600000000     C                   EndDo                                                  *
044700000000     C*
044800000000     C                   Exsr      OpeBreak
044900000000     C*
045000000000     C                   Endsr
045100000000     C******************************************************
045200000000     C* Rottura
045300000000     C******************************************************
045400000000     C     OpeBreak      BegSr                                                  *
045500000000     C*
045600000000     C                   If        QUADRO0592    <> WSavQua    or
045700000000     C                             $FineSCF      =  '1'
045800000000     C*
045900000000     C                   Select
046000000000     C*
046100000000     C* Aggregato
046200000000     C                   When      WSavQua       =  'FA'  and
046300000000     C                             Prog_FA > 0
046400000000     C                   EXSR      WrtDati
046500000000     C                   EXSR      InzSkiera
046600000000     C*
046700000000     C                   When      WSavQua       =  'SA'  and
046800000000     C                             Prog_SA > 0
046900000000     C                   EXSR      WrtDati
047000000000     C                   EXSR      InzSkiera
047100000000R136 C*
047200000000R136 C                   When      WSavQua       =  'TU'  and
047300000000R136 C                             Prog_TU > 0
047400000000R136 C                   EXSR      WrtDati
047500000000R136 C                   EXSR      InzSkiera
047600000000     C*
047700000000     C* Analitico
047800000000     C                   When      WSavQua       =  'FE'  and
047900000000     C                             Prog_FE > 0
048000000000     C                   EXSR      WrtDati
048100000000     C                   EXSR      InzSkiera
048200000000     C*
048300000000     C                   When      WSavQua       =  'FR'  and
048400000000     C                             Prog_FR > 0
048500000000     C                   EXSR      WrtDati
048600000000     C                   EXSR      InzSkiera
048700000000     C*
048800000000     C                   When      WSavQua       =  'NE'  and
048900000000     C                             Prog_NE > 0
049000000000     C                   EXSR      WrtDati
049100000000     C                   EXSR      InzSkiera
049200000000     C*
049300000000     C                   When      WSavQua       =  'NR'  and
049400000000     C                             Prog_NR > 0
049500000000     C                   EXSR      WrtDati
049600000000     C                   EXSR      InzSkiera
049700000000     C*
049800000000     C                   When      WSavQua       =  'DF'  and
049900000000     C                             Prog_DF > 0
050000000000     C                   EXSR      WrtDati
050100000000     C                   EXSR      InzSkiera
050200000000     C*
050300000000     C                   When      WSavQua       =  'FN'  and
050400000000     C                             Prog_FN > 0
050500000000     C                   EXSR      WrtDati
050600000000     C                   EXSR      InzSkiera
050700000000     C*
050800000000     C                   When      WSavQua       =  'SE'  and
050900000000     C                             Prog_SE > 0
051000000000     C                   EXSR      WrtDati
051100000000     C                   EXSR      InzSkiera
051200000000     C*
051300000000     C                   EndSl
051400000000     C*
051500000000     C                   Z-ADD     0             WRKPRGMOD         7 0
051600000000     C*
051700000000     C                   EndIf
051800000000     C*
051900000000     C                   Move      QUADRO0592    WSavQua
052000000000     C*
052100000000     C                   EndSr
052200000000     C******************************************************
052300000000     C* Ciclo dei dati estratti
052400000000     C******************************************************
052500000000     C     LetturaSCF    BegSr                                                  *
052600000000     C*
052700000000     C                   CALLb     'SDG00502R'
052800000000     C                   PARM                    KPJBA
052900000000     C                   PARM                    SDG00502DS
053000000000     C                   PARM                    SDG00592DS
053100000000     C                   PARM                    SDG005FADS
053200000000     C                   PARM                    SDG005SADS
053300000000     C                   PARM                    SDG005BLDS
053400000000     C                   PARM                    SDG005FEDS
053500000000     C                   PARM                    SDG005FRDS
053600000000     C                   PARM                    SDG005NEDS
053700000000     C                   PARM                    SDG005NRDS
053800000000     C                   PARM                    SDG005DFDS
053900000000     C                   PARM                    SDG005FNDS
054000000000     C                   PARM                    SDG005SEDS
054100000000R136 C                   PARM                    SDG005TUDS
054200000000     C                   PARM                    SDG005TADS
054300000000     C*
054400000000     C                   Eval      RIC0502     = '1'
054500000000     C*
054600000000     C                   MOVE      END0502       $FineSCF
054700000000     C*
054800000000     C                   EndSR
054900000000     C******************************************************
055000000000     C* Elabora record letti
055100000000     C******************************************************
055200000000     C     ElaboraSCF    BegSr                                                  *
055300000000     C*
055400000000     C                   Move      *blanks       Errore
055500000000     C*
055600000000     C                   Select
055700000000     C* Aggregato
055800160202R195 X****               When      FORMATOA08    =  'A'
055900160202R195 C                   When      $Formato      =  'A'
056000000000     C*
056100000000     C                   Select
056200000000     C                   When      QUADRO0592    =  'FA'
056300000000     C                   Move      'FA'          Cod_Elenco
056400131206R141 C****               Add       1             CntFA
056500000000     C                   Exsr      GesRec_FA
056600000000     C*
056700000000     C                   When      QUADRO0592    =  'SA'
056800131206R141 C****               Add       1             CntSA
056900000000     C                   Exsr      GesRec_SA
057000000000     C*
057100000000     C                   When      QUADRO0592    =  'BL'
057200131206R141 C****               Add       1             CntBL
057300000000     C                   Exsr      GesRec_BL
057400000000     C*
057500000000R136 C                   When      QUADRO0592    =  'TU'
057600000000R136 C                   Add       1             CntTU
057700000000R136 C                   Exsr      GesRec_TU
057800000000R136 C*
057900000000     C                   EndSl
058000000000     C*
058100000000     C* Analitico
058200160202R195 X****               When      FORMATOA08    =  'D'
058300160202R195 C                   When      $Formato      =  'D'
058400000000     C*
058500000000     C                   Select
058600000000     C                   When      QUADRO0592    =  'FE'
058700131206     C                   Add       1             CntFE
058800000000     C                   Exsr      GesRec_FE
058900000000     C*
059000000000     C                   When      QUADRO0592    =  'FR'
059100131206     C                   Add       1             CntFR
059200000000     C                   Exsr      GesRec_FR
059300000000     C*
059400000000     C                   When      QUADRO0592    =  'NE'
059500000000     C                   Add       1             CntNE
059600000000     C                   Exsr      GesRec_NE
059700000000     C*
059800000000     C                   When      QUADRO0592    =  'NR'
059900000000     C                   Add       1             CntNR
060000000000     C                   Exsr      GesRec_NR
060100000000     C*
060200000000     C                   When      QUADRO0592    =  'DF'
060300000000     C                   Add       1             CntDF
060400000000     C                   Exsr      GesRec_DF
060500000000     C*
060600000000     C                   When      QUADRO0592    =  'FN'
060700000000     C                   Add       1             CntFN
060800000000     C                   Exsr      GesRec_FN
060900000000     C*
061000000000     C                   When      QUADRO0592    =  'SE'
061100000000     C                   Add       1             CntSE
061200000000     C                   Exsr      GesRec_SE
061300000000R136 C*
061400000000R136 C                   When      QUADRO0592    =  'TU'
061500000000R136 C                   Add       1             CntTU
061600000000R136 C                   Exsr      GesRec_TU
061700000000     C*
061800000000     C                   EndSl
061900000000     C*
062000000000     C                   Other
062100000000     C*
062200000000     C                   Move      '1'           Errore
062300000000     c                   Exsr      ElaErrori
062400000000     C                   EndSl
062500000000     C*
062600000000     C                   EndSR
062700000000     C******************************************************
062800000000     C* Calcola nr moduli
062900000000     C******************************************************
063000000000     C     Calc_NrMod    BegSr                                                  *
063100000000     C*
063200000000     C                   Z-Add     0             NrModFA
063300000000     C                   Z-Add     0             NrModSA
063400000000     C                   Z-Add     0             NrModBL
063500000000     C*
063600000000     C                   Z-Add     0             NrModFE
063700000000     C                   Z-Add     0             NrModFR
063800000000     C                   Z-Add     0             NrModNE
063900000000     C                   Z-Add     0             NrModNR
064000000000     C                   Z-Add     0             NrModDF
064100000000     C                   Z-Add     0             NrModFN
064200000000     C                   Z-Add     0             NrModSE
064300000000     C                   Z-Add     0             NrModTU
064400000000     C*
064500000000     C* FA
064600000000     C                   If        CntFA         > 0
064700000000     C     CntFA         Div       3             NrModFA
064800000000     C                   MVR                     RestoMod          5 3
064900000000     C                   If        RestoMod      > 0
065000000000     C                   Add       1             NrModFa
065100000000     C                   EndIf
065200000000     C                   EndIf
065300000000     C*
065400000000     C* SA
065500000000     C                   If        CntSA         > 0
065600000000     C     CntSA         Div       10            NrModSA
065700000000     C                   MVR                     RestoMod          5 3
065800000000     C                   If        RestoMod      > 0
065900000000     C                   Add       1             NrModSA
066000000000     C                   EndIf
066100000000     C                   EndIf
066200000000     C*
066300000000     C* BL
066400000000     C                   If        CntBL         > 0
066500000000     C                   Z-Add     CntBL         NrModBL
066600000000     C                   EndIf
066700000000     C*
066800000000     C* FE
066900000000     C                   If        CntFE         > 0
067000000000     C     CntFE         Div       6             NrModFE
067100000000     C                   MVR                     RestoMod          5 3
067200000000     C                   If        RestoMod      > 0
067300000000     C                   Add       1             NrModFE
067400000000     C                   EndIf
067500000000     C                   EndIf
067600000000     C*
067700000000     C* FR
067800000000     C                   If        CntFR         > 0
067900000000     C     CntFR         Div       6             NrModFR
068000000000     C                   MVR                     RestoMod          5 3
068100000000     C                   If        RestoMod      > 0
068200000000     C                   Add       1             NrModFR
068300000000     C                   EndIf
068400000000     C                   EndIf
068500000000     C*
068600000000     C* NE
068700000000     C                   If        CntNE         > 0
068800000000     C     CntNE         Div       10            NrModNE
068900000000     C                   MVR                     RestoMod          5 3
069000000000     C                   If        RestoMod      > 0
069100000000     C                   Add       1             NrModNE
069200000000     C                   EndIf
069300000000     C                   EndIf
069400000000     C*
069500000000     C* NR
069600000000     C                   If        CntNR         > 0
069700000000     C     CntNR         Div       10            NrModNR
069800000000     C                   MVR                     RestoMod          5 3
069900000000     C                   If        RestoMod      > 0
070000000000     C                   Add       1             NrModNR
070100000000     C                   EndIf
070200000000     C                   EndIf
070300000000     C*
070400000000     C* DF
070500000000     C                   If        CntDF         > 0
070600000000     C     CntDF         Div       10            NrModDF
070700000000     C                   MVR                     RestoMod          5 3
070800000000     C                   If        RestoMod      > 0
070900000000     C                   Add       1             NrModDF
071000000000     C                   EndIf
071100000000     C                   EndIf
071200000000     C*
071300000000     C* FN
071400000000     C                   If        CntFN         > 0
071500000000     C     CntFN         Div       3             NrModFN
071600000000     C                   MVR                     RestoMod          5 3
071700000000     C                   If        RestoMod      > 0
071800000000     C                   Add       1             NrModFN
071900000000     C                   EndIf
072000000000     C                   EndIf
072100000000     C*
072200000000     C* SE
072300000000     C                   If        CntSE         > 0
072400000000     C     CntSE         Div       3             NrModSE
072500000000     C                   MVR                     RestoMod          5 3
072600000000     C                   If        RestoMod      > 0
072700000000     C                   Add       1             NrModSE
072800000000     C                   EndIf
072900000000     C                   EndIf
073000000000R136 C*
073100000000 "   C* TU
073200000000 "   C                   If        CntTU         > 0
073300000000 "   C     CntTU         Div       3             NrModTU
073400000000 "   C                   MVR                     RestoMod          5 3
073500000000 "   C                   If        RestoMod      > 0
073600000000 "   C                   Add       1             NrModTU
073700000000 "   C                   EndIf
073800000000R136 C                   EndIf
073900000000     C*
074000000000     C                   Eval      TotRec       = NrModFA +
074100000000     C                                            NrModSA +
074200000000     C                                            NrModBL +
074300000000     C                                            NrModFE +
074400000000     C                                            NrModFR +
074500000000     C                                            NrModNE +
074600000000     C                                            NrModNR +
074700000000     C                                            NrModDF +
074800000000     C                                            NrModFN +
074900000000R136 C                                            NrModTU +
075000000000     C                                            NrModSE
075100000000     C*
075200000000     C                   EndSR
075300000000     C******************************************************
075400000000     C* Tipo Record FA
075500000000     C******************************************************
075600000000     C     GesRec_FA     BegSr                                                  *
075700000000     C*
075800131206R141 C                   Exsr      CheCk_FA
075900131206R141 C*
076000150306R147 X**** Ex R141       If        WrkTotale    <> 0
076100150306R147XC***********
076200150306 "  XC***********        If        ((IMPATT05FA  <> 0   or
076300150306 "  XC***********                   IVAATT05FA  <> 0   or
076400150306 "  XC***********                   MARGIA05FA  <> 0   or
076500150306 "  XC***********                   IMPNDE05FA  <> 0   or
076600150306 "  XC***********                   IVANDE05FA  <> 0)  and
076700150306 "  XC***********                   ImpAttOk    = *On)
076800150306 "  XC***********                                      Or
076900150306 "  XC***********                  ((IMPPAS05FA  <> 0   or
077000150306 "  XC***********                   IVAPAS05FA  <> 0   or
077100150306 "  XC***********                   MARGIP05FA  <> 0   or
077200150306 "  XC***********                   IMPNCR05FA  <> 0   or
077300150306 "  XC***********                   IVANCR05FA  <> 0)  and
077400150306R147XC***********                   ImpPasOk    = *On)
077500150306R147 C*
077600150309 "   C                   if        ImpAttOk    = *On   or
077700150309R147 C                             ImpPasOk    = *On
077800131206R141 C*
077900131206R141 C                   Add       1             CntFA
078000000000     C                   Add       1             Prog_FA
078100000000     C*
078200000000     C*-------------------------------------------
078300000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
078400000000     C*
078500000000     C* 1) Partita Iva
078600000000     C                   If        PIVA05FA      <> *blanks
078700000000R135 C                             and DOCRIE05FA <> '1'
078800000000     C                   Z-Add     Prog_FA       Conta_Rigo
078900000000     C                   Z-Add     1             Conta_Colo
079000000000     C                   Clear                   Campo_Valore
079100000000     C                   MoveL     PIVA05FA      Campo_Valore
079200000000     C                   if        Campo_Valore <> *Blanks
079300000000     C                   Exsr      RieSkiera
079400000000     C                   EndIf
079500000000     C                   else
079600000000     C*
079700000000     C* 2) Codice fiscale
079800000000     C                   If        CDFISC05FA      <> *blanks
079900000000R135 C                             and DOCRIE05FA  <> '1'
080000000000R135 C                             and PIVA05FA    = *blanks
080100000000     C                   Z-Add     Prog_FA       Conta_Rigo
080200000000     C                   Z-Add     2             Conta_Colo
080300000000     C                   Clear                   Campo_Valore
080400000000     C                   MoveL     CDFISC05FA    Campo_Valore
080500000000     C                   if        Campo_Valore <> *Blanks
080600000000     C                   Exsr      RieSkiera
080700000000     C                   EndIf
080800000000     C                   EndIf
080900000000     C                   EndIf
081000000000R135 C*
081100150420R135 C* 3) Documento riepilogativo
081200000000R135 C                   If        DOCRIE05FA    =  '1'
081300000000R135 C                   Z-Add     Prog_FA       Conta_Rigo
081400000000 "   C                   Z-Add     3             Conta_Colo
081500000000 "   C                   Clear                   Campo_Valore
081600000000 "   C                   Move      DOCRIE05FA    Campo_Valore
081700000000 "   C                   if        Campo_Valore <> *Blanks
081800000000 "   C                   Exsr      RieSkiera
081900000000 "   C                   EndIf
082000000000R135 C                   EndIf
082100000000     C*
082200150420R178 C                   If        ImpAttOk    = *On
082300000000     C* 3) Nr.Operaz.Attive aggregate
082400131120     C                   If        NROPEA05FA    <> 0
082500000000     C                   Z-Add     Prog_FA       Conta_Rigo
082600000000R135 C****               Z-Add     3             Conta_Colo
082700000000R135 C                   Z-Add     4             Conta_Colo
082800000000     C                   Clear                   Campo_Valore
082900000000     C                   Z-Add     NROPEA05FA    Importo16_0
083000000000     C                   Exsr      Ablank2
083100000000     C                   if        Campo_Valore <> *Blanks
083200000000     C                   Exsr      RieSkiera
083300000000     C                   EndIf
083400000000     C                   EndIf
083500150420R178 C                   EndIf
083600000000     C*
083700150420R178 C                   If        ImpPasOk    = *On
083800000000     C* 4) Nr.Operaz.Passive aggregate
083900000000     C                   If        NROPEP05FA    <> 0
084000000000     C                   Z-Add     Prog_FA       Conta_Rigo
084100000000R135 C****               Z-Add     4             Conta_Colo
084200000000R135 C                   Z-Add     5             Conta_Colo
084300000000     C                   Clear                   Campo_Valore
084400000000     C                   Z-Add     NROPEP05FA    Importo16_0
084500000000     C                   Exsr      Ablank2
084600000000     C                   if        Campo_Valore <> *Blanks
084700000000     C                   Exsr      RieSkiera
084800000000     C                   EndIf
084900000000     C                   EndIf
085000150420R178 C                   EndIf
085100000000     C*
085200150420R178 C                   If        ImpAttOk    = *On
085300000000     C* 5) Noleggio Leasing
085400000000R135 C****               If        LEASIN05FA    <> '0'
085500000000R135 C                   If        LEASIN05FA    <> *blanks
085600000000R139 C                             and LEASIN05FA    <> '0'
085700000000     C                   Z-Add     Prog_FA       Conta_Rigo
085800000000R135 C****               Z-Add     5             Conta_Colo
085900000000R135 C                   Z-Add     6             Conta_Colo
086000000000     C                   Clear                   Campo_Valore
086100000000R135 C****               Move      LEASIN05FA    Campo_Valore
086200000000R135 C                   MoveL     LEASIN05FA    Campo_Valore
086300000000     C                   if        Campo_Valore <> *Blanks
086400000000     C                   Exsr      RieSkiera
086500000000     C                   EndIf
086600000000     C                   EndIf
086700150420R178 C                   EndIf
086800000000     C*
086900000000     C*--- OPERAZIONI ATTIVE
087000000000     C*
087100150309R147 C                   If        ImpAttOk    = *On
087200000000     C* 6) Totale operaz.Imponibili, non imponibili ed esenti
087300131120     C                   If        IMPATT05FA    <> 0
087400000000     C                   Z-Add     Prog_FA       Conta_Rigo
087500000000R135 C****               Z-Add     6             Conta_Colo
087600000000R135 C                   Z-Add     7             Conta_Colo
087700000000     C                   Clear                   Campo_Valore
087800000000     C                   Z-Add     IMPATT05FA    Importo16_0
087900000000     C                   Exsr      Ablank2
088000000000     C                   if        Campo_Valore <> *Blanks
088100000000     C                   Exsr      RieSkiera
088200000000     C                   EndIf
088300000000     C                   EndIf
088400000000     C*
088500000000     C* 7) Totale imposta
088600000000     C                   If        IVAATT05FA    <> 0
088700000000     C                   Z-Add     Prog_FA       Conta_Rigo
088800000000R135 C****               Z-Add     7             Conta_Colo
088900000000R135 C                   Z-Add     8             Conta_Colo
089000000000     C                   Clear                   Campo_Valore
089100000000     C                   Z-Add     IVAATT05FA    Importo16_0
089200000000     C                   Exsr      Ablank2
089300000000     C                   if        Campo_Valore <> *Blanks
089400000000     C                   Exsr      RieSkiera
089500000000     C                   EndIf
089600000000     C                   EndIf
089700000000     C*
089800000000     C* 8) Totale operazioni con IVA non esposta
089900000000     C                   If        MARGIA05FA    <> 0
090000000000     C                   Z-Add     Prog_FA       Conta_Rigo
090100000000R135 C****               Z-Add     8             Conta_Colo
090200000000R135 C                   Z-Add     9             Conta_Colo
090300000000     C                   Clear                   Campo_Valore
090400000000     C                   Z-Add     MARGIA05FA    Importo16_0
090500000000     C                   Exsr      Ablank2
090600000000     C                   if        Campo_Valore <> *Blanks
090700000000     C                   Exsr      RieSkiera
090800000000     C                   EndIf
090900000000     C                   EndIf
091000000000     C*
091100000000     C* 9) Totale note variazione a debito controparte
091200000000     C                   If        IMPNDE05FA    <> 0
091300000000     C                   Z-Add     Prog_FA       Conta_Rigo
091400000000R135 C****               Z-Add     9             Conta_Colo
091500000000R135 C                   Z-Add     10            Conta_Colo
091600000000     C                   Clear                   Campo_Valore
091700000000     C                   Z-Add     IMPNDE05FA    Importo16_0
091800000000     C                   If        IMPNDE05FA   <   0
091900000000     C                   Eval      Importo16_0  = Importo16_0 * -1
092000000000     C                   EndIf
092100000000     C                   Exsr      Ablank2
092200000000     C                   if        Campo_Valore <> *Blanks
092300000000     C                   Exsr      RieSkiera
092400000000     C                   EndIf
092500000000     C                   EndIf
092600000000     C*
092700000000     C* 10) Totale imposta sulle note variazione a debito
092800000000     C                   If        IVANDE05FA    <> 0
092900000000     C                   Z-Add     Prog_FA       Conta_Rigo
093000000000R135 C****               Z-Add     10            Conta_Colo
093100000000R135 C                   Z-Add     11            Conta_Colo
093200000000     C                   Clear                   Campo_Valore
093300000000     C                   Z-Add     IVANDE05FA    Importo16_0
093400000000     C                   If        IVANDE05FA   <   0
093500000000     C                   Eval      Importo16_0  = Importo16_0 * -1
093600000000     C                   EndIf
093700000000     C                   Exsr      Ablank2
093800000000     C                   if        Campo_Valore <> *Blanks
093900000000     C                   Exsr      RieSkiera
094000000000     C                   EndIf
094100000000     C                   EndIf
094200000000     C*
094300150309R147 C                   endif
094400140414R147 C*
094500000000     C*--- OPERAZIONI PASSIVE
094600000000     C*
094700150306R147 C                   If        ImpPasOk    = *On
094800000000     C* 11) Totale operaz.Imponibili, non imponibili ed esenti
094900000000     C                   If        IMPPAS05FA    <> 0
095000000000     C                   Z-Add     Prog_FA       Conta_Rigo
095100000000R135 C****               Z-Add     11            Conta_Colo
095200000000R135 C                   Z-Add     12            Conta_Colo
095300000000     C                   Clear                   Campo_Valore
095400000000     C                   Z-Add     IMPPAS05FA    Importo16_0
095500000000     C                   Exsr      Ablank2
095600000000     C                   if        Campo_Valore <> *Blanks
095700000000     C                   Exsr      RieSkiera
095800000000     C                   EndIf
095900000000     C                   EndIf
096000000000     C*
096100000000     C* 12) Totale imposta
096200000000     C                   If        IVAPAS05FA    <> 0
096300000000     C                   Z-Add     Prog_FA       Conta_Rigo
096400000000R135 C****               Z-Add     12            Conta_Colo
096500000000R135 C                   Z-Add     13            Conta_Colo
096600000000     C                   Clear                   Campo_Valore
096700000000     C                   Z-Add     IVAPAS05FA    Importo16_0
096800000000     C                   Exsr      Ablank2
096900000000     C                   if        Campo_Valore <> *Blanks
097000000000     C                   Exsr      RieSkiera
097100000000     C                   EndIf
097200000000     C                   EndIf
097300000000     C*
097400000000     C* 13) Totale operazioni con IVA non esposta
097500000000     C                   If        MARGIP05FA    <> 0
097600000000     C                   Z-Add     Prog_FA       Conta_Rigo
097700000000R135 C****               Z-Add     13            Conta_Colo
097800000000R135 C                   Z-Add     14            Conta_Colo
097900000000     C                   Clear                   Campo_Valore
098000000000     C                   Z-Add     MARGIP05FA    Importo16_0
098100000000     C                   Exsr      Ablank2
098200000000     C                   if        Campo_Valore <> *Blanks
098300000000     C                   Exsr      RieSkiera
098400000000     C                   EndIf
098500000000     C                   EndIf
098600000000     C*
098700000000     C* 14) Totale note variazione a debito controparte
098800000000     C                   If        IMPNCR05FA    <> 0
098900000000     C                   Z-Add     Prog_FA       Conta_Rigo
099000000000R135 C****               Z-Add     14            Conta_Colo
099100000000R135 C                   Z-Add     15            Conta_Colo
099200000000     C                   Clear                   Campo_Valore
099300000000     C                   Z-Add     IMPNCR05FA    Importo16_0
099400000000     C                   If        IMPNCR05FA   <   0
099500000000     C                   Eval      Importo16_0  = Importo16_0 * -1
099600000000     C                   EndIf
099700000000     C                   Exsr      Ablank2
099800000000     C                   if        Campo_Valore <> *Blanks
099900000000     C                   Exsr      RieSkiera
100000000000     C                   EndIf
100100000000     C                   EndIf
100200000000     C*
100300000000     C* 15) Totale imposta sulle note variazione a debito
100400000000     C                   If        IVANCR05FA    <> 0
100500000000     C                   Z-Add     Prog_FA       Conta_Rigo
100600000000R135 C****               Z-Add     15            Conta_Colo
100700000000R135 C                   Z-Add     16            Conta_Colo
100800000000     C                   Clear                   Campo_Valore
100900000000     C                   Z-Add     IVANCR05FA    Importo16_0
101000000000     C                   If        IVANCR05FA   <   0
101100000000     C                   Eval      Importo16_0  = Importo16_0 * -1
101200000000     C                   EndIf
101300000000     C                   Exsr      Ablank2
101400000000     C                   if        Campo_Valore <> *Blanks
101500000000     C                   Exsr      RieSkiera
101600000000     C                   EndIf
101700000000     C                   EndIf
101800000000     C*
101900150309R147 C                   endif
102000140414R147 C*
102100000000     C                   IF        Prog_FA       = 3
102200000000     C                   Z-Add     0             Prog_FA
102300000000     C                   Add       1             ProgMod_FA
102400000000     C                   Z-Add     ProgMod_FA    WrkProgMod
102500000000     C                   EXSR      WrtDati
102600000000     C                   EXSR      InzSkiera
102700000000     C                   EndIf
102800000000     C*
102900131206R141 C                   EndIf
103000131206R141 C*
103100000000     C                   EndSR
103200131206R141 C******************************************************
103300131206     C* Check Import FA
103400131206R141 C******************************************************
103500131206     C     Check_Fa      BegSr                                                  *
103600000000     C*
103700140414     C                   Exsr      AzzeraWrkImp
103800140414     C*
103900150306R147 X****               Z-Add     IMPATT05FA    WrkImport01
104000150306 "   X****               Z-Add     IVAATT05FA    WrkImport02
104100150306R147 X****               Z-Add     MARGIA05FA    WrkImport03
104200150306     C                   Z-Add     IMPNDE05FA    WrkImport04
104300150306     C                   Z-Add     IVANDE05FA    WrkImport05
104400150306R147 X****               Z-Add     IMPPAS05FA    WrkImport06
104500150306 "   X****               Z-Add     IVAPAS05FA    WrkImport07
104600150306R147 X****               Z-Add     MARGIP05FA    WrkImport08
104700150306     C                   Z-Add     IMPNCR05FA    WrkImport09
104800150306     C                   Z-Add     IVANCR05FA    WrkImport10
104900131206     C*
105000150306R147 X****               Exsr      CalcWrkTotale
105100131206     C*
105200150306R147 C                   Move      *On           ImpAttOk          1
105300150306 "   C                   Move      *On           ImpPasOk          1
105400150306 "   C*
105500150309 "   C                   If        WrkImport04 < 0
105600150309 "   C                   Eval      WrkImport04 = WrkImport04 * -1
105700150306 "   C                   EndIf
105800150309 "   C                   If        WrkImport05 < 0
105900150309 "   C                   Eval      WrkImport05 = WrkImport05 * -1
106000150306 "   C                   EndIf
106100150309 "   C                   If        WrkImport09 < 0
106200150309 "   C                   Eval      WrkImport09 = WrkImport09 * -1
106300150306 "   C                   EndIf
106400150309 "   C                   If        WrkImport10 < 0
106500150309 "   C                   Eval      WrkImport10 = WrkImport10 * -1
106600150306 "   C                   EndIf
106700150306 "   C*
106800150309 "   C                   If        IMPATT05FA  < 1        and
106900150309 "   C                             IVAATT05FA  < 1        and
107000150309 "   C                             MARGIA05FA  < 1        and
107100150309 "   C                             WrkImport04 < 1        and
107200150309 "   C                             WrkImport05 < 1
107300150306 "   C                   Move      *Off          ImpAttOk
107400150306 "   C                   EndIf
107500150306 "   C*
107600150309 "   C                   If        IMPPAS05FA  < 1        and
107700150309 "   C                             IVAPAS05FA  < 1        and
107800150309 "   C                             MARGIP05FA  < 1        and
107900150309 "   C                             WrkImport09 < 1        and
108000150309 "   C                             WrkImport10 < 1
108100150306 "   C                   Move      *Off          ImpPasOk
108200150306 "   C                   EndIf
108300150306 "   C*
108400150309 "   C                   if        ImpAttOk   <> *On and
108500150306 "   C                             ImpPasOk   <> *On
108600150309 "   C                   Eval      Togli_FA    = Togli_FA + 1
108700150309 "   C                   else
108800150309 "   C                   Eval      Numero_FA   = Numero_FA +1
108900150309 "   C                   endif
109000140414R147 C*
109100131206R141 C                   EndSR
109200131206     C******************************************************
109300131206     C* Tipo Record SA
109400131206     C******************************************************
109500131206     C     GesRec_SA     BegSr                                                  *
109600131206     C*
109700131206R141 C                   Exsr      CheCk_SA
109800131206R141 C*
109900131206R141 C                   If        WrkTotale    <> 0
110000131206R141 C*
110100131206R141 C                   Add       1             CntSA
110200000000     C                   Add       1             Prog_SA
110300000000     C*
110400000000     C*-------------------------------------------
110500000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
110600000000     C*
110700000000     C* 1) Codice fiscale
110800000000     C                   If        CDFISC05SA    <> *blanks
110900000000     C                   Z-Add     Prog_SA       Conta_Rigo
111000000000     C                   Z-Add     1             Conta_Colo
111100000000     C                   Clear                   Campo_Valore
111200000000     C                   MoveL     CDFISC05SA    Campo_Valore
111300000000     C                   if        Campo_Valore <> *Blanks
111400000000     C                   Exsr      RieSkiera
111500000000     C                   EndIf
111600000000     C                   EndIf
111700000000     C*
111800000000     C* 2) Nr.Operazioni
111900000000     C                   If        NROPE05SA     <> 0
112000000000     C                   Z-Add     Prog_SA       Conta_Rigo
112100000000     C                   Z-Add     2             Conta_Colo
112200000000     C                   Clear                   Campo_Valore
112300000000     C                   Z-Add     NROPE05SA     Importo16_0
112400000000     C                   Exsr      Ablank2
112500000000     C                   if        Campo_Valore <> *Blanks
112600000000     C                   Exsr      RieSkiera
112700000000     C                   EndIf
112800000000     C                   EndIf
112900000000     C*
113000000000     C* 3) Importo complessivo
113100000000     C                   If        IMPORT05SA    <> 0
113200000000     C                   Z-Add     Prog_SA       Conta_Rigo
113300000000     C                   Z-Add     3             Conta_Colo
113400000000     C                   Clear                   Campo_Valore
113500000000     C                   Z-Add     IMPORT05SA    Importo16_0
113600000000     C                   Exsr      Ablank2
113700000000     C                   if        Campo_Valore <> *Blanks
113800000000     C                   Exsr      RieSkiera
113900000000     C                   EndIf
114000000000     C                   EndIf
114100000000     C*
114200000000     C* 4) Noleggio Leasing
114300000000     C                   If        LEASIN05SA    <> *blanks
114400000000R139 C                             and LEASIN05SA    <> '0'
114500000000     C                   Z-Add     Prog_SA       Conta_Rigo
114600000000     C                   Z-Add     4             Conta_Colo
114700000000     C                   Clear                   Campo_Valore
114800000000     C                   MoveL     LEASIN05SA    Campo_Valore
114900000000     C                   if        Campo_Valore <> *Blanks
115000000000     C                   Exsr      RieSkiera
115100000000     C                   EndIf
115200000000     C                   EndIf
115300000000     C*
115400000000     C                   IF        Prog_SA       = 10
115500000000     C                   Z-Add     0             Prog_SA
115600000000     C                   Add       1             ProgMod_SA
115700000000     C                   Z-Add     ProgMod_SA    WrkProgMod
115800000000     C                   EXSR      WrtDati
115900000000     C                   EXSR      InzSkiera
116000000000     C                   EndIf
116100000000     C*
116200131206R141 C                   EndIf
116300131206R141 C*
116400000000     C                   EndSR
116500131206R141 C******************************************************
116600131206     C* Check Import SA
116700131206R141 C******************************************************
116800131206     C     Check_Sa      BegSr                                                  *
116900131206     C*
117000131206     C                   Exsr      AzzeraWrkImp
117100131206     C*
117200131206     C                   Z-Add     IMPORT05SA    WrkImport01
117300131206     C*
117400150420     C                   Eval      WrkTotale     = WrkImport01
117500131206     C*
117600150420R178 X****               Exsr      CalcWrkTotale
117700131206     C*
117800131206R141 C                   EndSR
117900000000     C******************************************************
118000000000     C* Tipo Record BL
118100000000     C******************************************************
118200000000     C     GesRec_BL     BegSr                                                  *
118300000000     C*
118400131206R141 C                   Exsr      CheCk_BL
118500131206R141 C*
118600140414R147 C**** Ex R141       If        WrkTotale    <> 0
118700150306R147XC*************      If        ((IMPATT05BL  <> 0    or
118800150306 "  XC*************                 IVAATT05BL   <> 0)   and
118900150306 "  XC*************                 ImpAttOk     = *On)
119000150306 "  XC*************                                    Or
119100150306 "  XC*************                ((IMPPAS05BL  <> 0   or
119200150306 "  XC*************                 IVAPAS05BL  <> 0)   and
119300150306 "  XC*************                 ImpPasOk    = *On)
119400150306R147XC*************
119500150306R147 C*
119600150309 "   C                   if        ImpAttOk    = *On  or
119700150309R147 C                             ImpPasOk    = *On
119800131206R141 C*
119900131206R141 C                   Add       1             CntBL
120000000000     C                   Z-Add     1             Prog_BL
120100000000     C*
120200000000     C*-------------------------------------------
120300000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
120400000000     C*
120500000000     C* BL001
120600000000     C*
120700000000     C* 1) Cognome
120800000000     C                   If        COGRAG05BL    <> *blanks
120900000000     C                   Z-Add     Prog_BL       Conta_Rigo
121000000000     C                   Z-Add     1             Conta_Colo
121100000000     C                   Clear                   Campo_Valore
121200000000     C                   MoveL     COGRAG05BL    Campo_Valore
121300000000     C                   MoveL     COGRAG05BL    C60
121400000000     C                   if        Campo_Valore <> *Blanks
121500000000     C                   Exsr      RieSkieraAlf
121600000000     C                   EndIf
121700000000     C                   EndIf
121800000000     C*
121900000000     C* 2) Nome
122000000000     C                   If        NOME05BL      <> *blanks
122100000000     C                   Z-Add     Prog_BL       Conta_Rigo
122200000000     C                   Z-Add     2             Conta_Colo
122300000000     C                   Clear                   Campo_Valore
122400000000     C                   MoveL     NOME05BL      Campo_Valore
122500000000     C                   MoveL     NOME05BL      C60
122600000000     C                   if        Campo_Valore <> *Blanks
122700000000     C                   Exsr      RieSkieraAlf
122800000000     C                   EndIf
122900000000     C                   EndIf
123000000000     C*
123100000000     C* 3) Data di nascita
123200000000     C                   If        DTNASC05BL    <> *Loval
123300000000     C                   Move      DTNASC05BL    DtAMG
123400000000     C                   Move      AMGAnno       GMAAnno
123500000000     C                   Move      AMGMese       GMAMese
123600000000     C                   Move      AMGGiorno     GMAGiorno
123700000000     C                   Z-Add     Prog_BL       Conta_Rigo
123800000000     C                   Z-Add     3             Conta_Colo
123900000000     C                   Clear                   Campo_Valore
124000000000     C                   Move      DtGMA         Campo_Valore
124100000000     C                   if        Campo_Valore <> *Blanks
124200000000     C                   Exsr      RieSkiera
124300000000     C                   EndIf
124400000000     C                   EndIf
124500000000     C*
124600000000     C* 4) Comune o Stato estero di nascita
124700000000     C                   If        LOCNAS05BL    <> *blanks
124800000000     C                   Z-Add     Prog_BL       Conta_Rigo
124900000000     C                   Z-Add     4             Conta_Colo
125000000000     C                   Clear                   Campo_Valore
125100000000     C                   MoveL     LOCNAS05BL    Campo_Valore
125200000000     C                   MoveL     LOCNAS05BL    C60
125300000000     C                   if        Campo_Valore <> *Blanks
125400000000     C                   Exsr      RieSkieraAlf
125500000000     C                   EndIf
125600000000     C                   EndIf
125700000000     C*
125800000000     C* 5) Provincia
125900000000     C                   If        PRVNAS05BL    <> *blanks
126000000000     C                   Z-Add     Prog_BL       Conta_Rigo
126100000000     C                   Z-Add     5             Conta_Colo
126200000000     C                   Clear                   Campo_Valore
126300000000     C                   MoveL     PRVNAS05BL    Campo_Valore
126400000000     C                   If        COGRAG05BL <> *blanks  and
126500000000     C                             UICSocieta <> UIC105BL
126600000000     C                   Clear                   Campo_Valore
126700000000     C                   MoveL     'EE'          Campo_Valore
126800000000     C                   EndIf
126900000000     C                   if        Campo_Valore <> *Blanks
127000000000     C                   Exsr      RieSkiera
127100000000     C                   EndIf
127200000000     C                   EndIf
127300000000     C*
127400000000     C* 6) Codice stato estero
127500000000     C                   If        UIC105BL      <> 0
127600000000     C                   Z-Add     Prog_BL       Conta_Rigo
127700000000     C                   Z-Add     6             Conta_Colo
127800000000     C                   Clear                   Campo_Valore
127900000000     C                   Move      UIC105BL      Campo_Valore
128000000000     C                   if        Campo_Valore <> *Blanks
128100000000     C                   Exsr      RieSkiera
128200000000     C                   EndIf
128300000000     C                   EndIf
128400000000     C*
128500000000     C* 7) Denominazione / ragione sociale
128600000000     C                   If        RAGSOC05BL    <> *blanks
128700000000     C                   Z-Add     Prog_BL       Conta_Rigo
128800000000     C                   Z-Add     7             Conta_Colo
128900000000     C                   Clear                   Campo_Valore
129000000000     C                   MoveL     RAGSOC05BL    Campo_Valore
129100000000     C                   MoveL     RAGSOC05BL    C60
129200000000     C                   if        Campo_Valore <> *Blanks
129300000000     C                   Exsr      RieSkieraAlf
129400000000     C                   EndIf
129500000000     C                   EndIf
129600000000     C*
129700000000     C* 8) Citta sede legale
129800000000     C                   If        CITTAE05BL    <> *blanks
129900000000     C                   Z-Add     Prog_BL       Conta_Rigo
130000000000     C                   Z-Add     8             Conta_Colo
130100000000     C                   Clear                   Campo_Valore
130200000000     C                   MoveL     CITTAE05BL    Campo_Valore
130300000000     C                   MoveL     CITTAE05BL    C60
130400000000     C                   if        Campo_Valore <> *Blanks
130500000000     C                   Exsr      RieSkieraAlf
130600000000     C                   EndIf
130700000000     C                   EndIf
130800000000     C*
130900000000     C* 9) Codice stato estero
131000000000     C                   If        UIC205BL      <> 0
131100000000     C                   Z-Add     Prog_BL       Conta_Rigo
131200000000     C                   Z-Add     9             Conta_Colo
131300000000     C                   Clear                   Campo_Valore
131400000000     C                   Move      UIC205BL      Campo_Valore
131500000000     C                   if        Campo_Valore <> *Blanks
131600000000     C                   Exsr      RieSkiera
131700000000     C                   EndIf
131800000000     C                   EndIf
131900000000     C*
132000000000     C* 10) Indirizzo estero sede legale
132100000000     C                   If        INDIRE05BL    <> *blanks
132200000000     C                   Z-Add     Prog_BL       Conta_Rigo
132300000000     C                   Z-Add     10            Conta_Colo
132400000000     C                   Clear                   Campo_Valore
132500000000     C                   MoveL     INDIRE05BL    Campo_Valore
132600000000     C                   MoveL     INDIRE05BL    C60
132700000000     C                   if        Campo_Valore <> *Blanks
132800000000     C                   Exsr      RieSkieraAlf
132900000000     C                   EndIf
133000000000     C                   EndIf
133100000000     C*
133200000000     C* BL002
133300000000     C                   Add       1             Prog_BL
133400000000     C* 1) Codice identificativo
133500000000     C                   If        CIVA05BL      <> *blanks
133600000000     C                   Z-Add     Prog_BL       Conta_Rigo
133700000000     C                   Z-Add     1             Conta_Colo
133800000000     C                   Clear                   Campo_Valore
133900000000     C                   MoveL     CIVA05BL      Campo_Valore
134000000000     C                   MoveL     CIVA05BL      C60
134100000000     C                   if        Campo_Valore <> *Blanks
134200000000     C                   Exsr      RieSkieraAlf
134300000000     C                   EndIf
134400000000     C                   EndIf
134500000000     C*
134600000000     C* 2) Operazioni con paesi a fiscalità privilegiata
134700000000     C                   If        OPEBL05BL     <> *blanks
134800000000     C                   Z-Add     Prog_BL       Conta_Rigo
134900000000     C                   Z-Add     2             Conta_Colo
135000000000     C                   Clear                   Campo_Valore
135100000000     C                   Move      OPEBL05BL     Campo_Valore
135200000000     C                   Exsr      Ablank
135300000000     C                   if        Campo_Valore <> *Blanks
135400000000     C                   Exsr      RieSkiera
135500000000     C                   EndIf
135600000000     C                   EndIf
135700000000     C*
135800000000     C* 3) Operazioni con soggetti non residenti
135900000000     C                   If        OPENRE05BL    <> *blanks
136000000000     C                   Z-Add     Prog_BL       Conta_Rigo
136100000000     C                   Z-Add     3             Conta_Colo
136200000000     C                   Clear                   Campo_Valore
136300000000     C                   Move      OPENRE05BL    Campo_Valore
136400000000     C                   Exsr      Ablank
136500000000     C                   if        Campo_Valore <> *Blanks
136600000000     C                   Exsr      RieSkiera
136700000000     C                   EndIf
136800000000     C                   EndIf
136900000000     C*
137000000000     C* 4) Acquisti di servizi da soggetti non residenti
137100000000     C                   If        ACQSRV05BL    <> *blanks
137200000000     C                   Z-Add     Prog_BL       Conta_Rigo
137300000000     C                   Z-Add     4             Conta_Colo
137400000000     C                   Clear                   Campo_Valore
137500000000     C                   Move      ACQSRV05BL    Campo_Valore
137600000000     C                   Exsr      Ablank
137700000000     C                   if        Campo_Valore <> *Blanks
137800000000     C                   Exsr      RieSkiera
137900000000     C                   EndIf
138000000000     C                   EndIf
138100000000     C*
138200000000     C* BL003
138300160202R195 C* Operazioni imponibili, non imponibili ed esenti
138400150309R147 C                   If        ImpAttOk    = *On
138500140414R147 C*
138600000000     C                   Add       1             Prog_BL
138700000000     C*
138800000000     C* 1) Importo complessivo
138900160202R195 C*
139000160202R195 C* BL003001
139100000000     C                   If        IMPATT05BL    <> 0
139200000000     C                   Z-Add     Prog_BL       Conta_Rigo
139300000000     C                   Z-Add     1             Conta_Colo
139400000000     C                   Clear                   Campo_Valore
139500000000     C                   Z-Add     IMPATT05BL    Importo16_0
139600000000     C                   If        IMPATT05BL   <   0
139700000000     C                   Eval      Importo16_0  = Importo16_0 * -1
139800000000     C                   EndIf
139900000000     C                   Exsr      Ablank2
140000000000     C                   if        Campo_Valore <> *Blanks
140100000000     C                   Exsr      RieSkiera
140200000000     C                   EndIf
140300000000     C                   EndIf
140400000000     C*
140500000000     C* 2) Imposta
140600160202R195 C*
140700160202R195 C* BL003002
140800000000     C                   If        IVAATT05BL    <> 0
140900000000     C                   Z-Add     Prog_BL       Conta_Rigo
141000000000     C                   Z-Add     2             Conta_Colo
141100000000     C                   Clear                   Campo_Valore
141200000000     C                   Z-Add     IVAATT05BL    Importo16_0
141300000000     C                   If        IVAATT05BL   <   0
141400000000     C                   Eval      Importo16_0  = Importo16_0 * -1
141500000000     C                   EndIf
141600000000     C                   Exsr      Ablank2
141700000000     C                   if        Campo_Valore <> *Blanks
141800000000     C                   Exsr      RieSkiera
141900000000     C                   EndIf
142000000000     C                   EndIf
142100160202     C*
142200160202R195 C* Black list annuale
142300160202 "   C                   if        TpComG61    = 'B'
142400160202 "   C*
142500160202 "   C* BL004 - Operazioni non soggette ad IVA
142600160202 "   C                   Z-Add     4             Prog_BL
142700160202 "   C*
142800160202 "   C* 1) Cessione di beni - Importo complessivo
142900160202 "   C*
143000160202 "   C* BL004001
143100160202 "   C                   If        NOIBAT05BL    <> 0
143200160202 "   C                   Z-Add     Prog_BL       Conta_Rigo
143300160202 "   C                   Z-Add     1             Conta_Colo
143400160202 "   C                   Clear                   Campo_Valore
143500160202 "   C                   Z-Add     NOIBAT05BL    Importo16_0
143600160202 "   C                   If        NOIBAT05BL   <   0
143700160202 "   C                   Eval      Importo16_0  = Importo16_0 * -1
143800160202 "   C                   EndIf
143900160202 "   C                   Exsr      Ablank2
144000160202 "   C                   if        Campo_Valore <> *Blanks
144100160202 "   C                   Exsr      RieSkiera
144200160202 "   C                   EndIf
144300160202 "   C                   EndIf
144400160202 "   C*
144500160202 "   C* 2) Prestazioni di servizi - Importo complessivo
144600160202 "   C*
144700160202 "   C* BL004002
144800160202 "   C                   If        NOISAT05BL    <> 0
144900160202 "   C                   Z-Add     Prog_BL       Conta_Rigo
145000160202 "   C                   Z-Add     2             Conta_Colo
145100160202 "   C                   Clear                   Campo_Valore
145200160202 "   C                   Z-Add     NOISAT05BL    Importo16_0
145300160202 "   C                   If        NOISAT05BL   <   0
145400160202 "   C                   Eval      Importo16_0  = Importo16_0 * -1
145500160202 "   C                   EndIf
145600160202 "   C                   Exsr      Ablank2
145700160202 "   C                   if        Campo_Valore <> *Blanks
145800160202 "   C                   Exsr      RieSkiera
145900160202 "   C                   EndIf
146000160202 "   C                   EndIf
146100160202 "   C*
146200160202 "   C*
146300160202 "   C* BL005 - Note di variazione
146400160202 "   C                   Z-Add     5             Prog_BL
146500160202 "   C*
146600160202 "   C* 1) Importo complessivo
146700160202 "   C*
146800160202 "   C* BL005001
146900160202 "   C                   If        IMPNAT05BL    <> 0
147000160202 "   C                   Z-Add     Prog_BL       Conta_Rigo
147100160202 "   C                   Z-Add     1             Conta_Colo
147200160202 "   C                   Clear                   Campo_Valore
147300160202 "   C                   Z-Add     IMPNAT05BL    Importo16_0
147400160202 "   C                   If        IMPNAT05BL   <   0
147500160202 "   C                   Eval      Importo16_0  = Importo16_0 * -1
147600160202 "   C                   EndIf
147700160202 "   C                   Exsr      Ablank2
147800160202 "   C                   if        Campo_Valore <> *Blanks
147900160202 "   C                   Exsr      RieSkiera
148000160202 "   C                   EndIf
148100160202 "   C                   EndIf
148200160202 "   C*
148300160202 "   C* 2) Imposta
148400160202 "   C*
148500160202 "   C* BL005002
148600160202 "   C                   If        IVANAT05BL    <> 0
148700160202 "   C                   Z-Add     Prog_BL       Conta_Rigo
148800160202 "   C                   Z-Add     2             Conta_Colo
148900160202 "   C                   Clear                   Campo_Valore
149000160202 "   C                   Z-Add     IVANAT05BL    Importo16_0
149100160202 "   C                   If        IVANAT05BL   <   0
149200160202 "   C                   Eval      Importo16_0  = Importo16_0 * -1
149300160202 "   C                   EndIf
149400160202 "   C                   Exsr      Ablank2
149500160202 "   C                   if        Campo_Valore <> *Blanks
149600160202 "   C                   Exsr      RieSkiera
149700160202 "   C                   EndIf
149800160202 "   C                   EndIf
149900160202 "   C*
150000160202R195 C                   endif
150100160202     C*
150200150309R147 C                   endif
150300140414R147 C*
150400000000     C* BL006
150500160202R195 C* Operazioni imponibili, non imponibili ed esenti
150600150309R147 C                   If        ImpPasOk    = *On
150700140414R147 C*
150800000000     C                   Z-Add     6             Prog_BL
150900000000     C*
151000000000     C* 1) Importo complessivo
151100160202R195 C*
151200160202R195 C* BL006001
151300000000     C                   If        IMPPAS05BL    <> 0
151400000000     C                   Z-Add     Prog_BL       Conta_Rigo
151500000000     C                   Z-Add     1             Conta_Colo
151600000000     C                   Clear                   Campo_Valore
151700000000     C                   Z-Add     IMPPAS05BL    Importo16_0
151800000000     C                   If        IMPPAS05BL   <   0
151900000000     C                   Eval      Importo16_0  = Importo16_0 * -1
152000000000     C                   EndIf
152100000000     C                   Exsr      Ablank2
152200000000     C                   if        Campo_Valore <> *Blanks
152300000000     C                   Exsr      RieSkiera
152400000000     C                   EndIf
152500000000     C                   EndIf
152600000000     C*
152700000000     C* 2) Imposta
152800160202R195 C*
152900160202R195 C* BL006002
153000000000     C                   If        IVAPAS05BL    <> 0
153100000000     C                   Z-Add     Prog_BL       Conta_Rigo
153200000000     C                   Z-Add     2             Conta_Colo
153300000000     C                   Clear                   Campo_Valore
153400000000     C                   Z-Add     IVAPAS05BL    Importo16_0
153500000000     C                   If        IVAPAS05BL   <   0
153600000000     C                   Eval      Importo16_0  = Importo16_0 * -1
153700000000     C                   EndIf
153800000000     C                   Exsr      Ablank2
153900000000     C                   if        Campo_Valore <> *Blanks
154000000000     C                   Exsr      RieSkiera
154100000000     C                   EndIf
154200000000     C                   EndIf
154300160202R195 C*
154400160202 "   C* Black list annuale
154500160202 "   C                   if        TpComG61    = 'B'
154600160202 "   C*
154700160202 "   C* BL007 - Operazioni non soggette ad IVA
154800160202 "   C                   Z-Add     7             Prog_BL
154900160202 "   C*
155000160202 "   C* 1) Importo complessivo
155100160202 "   C*
155200160202 "   C* BL007001
155300160202 "   C                   If        NOIVAP05BL    <> 0
155400160202 "   C                   Z-Add     Prog_BL       Conta_Rigo
155500160202 "   C                   Z-Add     1             Conta_Colo
155600160202 "   C                   Clear                   Campo_Valore
155700160202 "   C                   Z-Add     NOIVAP05BL    Importo16_0
155800160202 "   C                   If        NOIVAP05BL   <   0
155900160202 "   C                   Eval      Importo16_0  = Importo16_0 * -1
156000160202 "   C                   EndIf
156100160202 "   C                   Exsr      Ablank2
156200160202 "   C                   if        Campo_Valore <> *Blanks
156300160202 "   C                   Exsr      RieSkiera
156400160202 "   C                   EndIf
156500160202 "   C                   EndIf
156600160202 "   C*
156700160202 "   C*
156800160202 "   C* BL008 - Note di variazione
156900160202 "   C                   Z-Add     8             Prog_BL
157000160202 "   C*
157100160202 "   C* 1) Importo complessivo
157200160202 "   C*
157300160202 "   C* BL008001
157400160202 "   C                   If        IMPNPA05BL    <> 0
157500160202 "   C                   Z-Add     Prog_BL       Conta_Rigo
157600160202 "   C                   Z-Add     1             Conta_Colo
157700160202 "   C                   Clear                   Campo_Valore
157800160202 "   C                   Z-Add     IMPNPA05BL    Importo16_0
157900160202 "   C                   If        IMPNPA05BL   <   0
158000160202 "   C                   Eval      Importo16_0  = Importo16_0 * -1
158100160202 "   C                   EndIf
158200160202 "   C                   Exsr      Ablank2
158300160202 "   C                   if        Campo_Valore <> *Blanks
158400160202 "   C                   Exsr      RieSkiera
158500160202 "   C                   EndIf
158600160202 "   C                   EndIf
158700160202 "   C*
158800160202 "   C* 2) Imposta
158900160202 "   C*
159000160202 "   C* BL008002
159100160202 "   C                   If        IVANPA05BL    <> 0
159200160202 "   C                   Z-Add     Prog_BL       Conta_Rigo
159300160202 "   C                   Z-Add     2             Conta_Colo
159400160202 "   C                   Clear                   Campo_Valore
159500160202 "   C                   Z-Add     IVANPA05BL    Importo16_0
159600160202 "   C                   If        IVANPA05BL   <   0
159700160202 "   C                   Eval      Importo16_0  = Importo16_0 * -1
159800160202 "   C                   EndIf
159900160202 "   C                   Exsr      Ablank2
160000160202 "   C                   if        Campo_Valore <> *Blanks
160100160202 "   C                   Exsr      RieSkiera
160200160202 "   C                   EndIf
160300160202 "   C                   EndIf
160400160202 "   C*
160500160202R195 C                   endif
160600140414     C*
160700150309R147 C                   endif
160800140414R147 C*
160900000000     C                   Add       1             ProgMod_BL
161000000000     C                   Z-Add     ProgMod_BL    WrkProgMod
161100000000     C                   EXSR      WrtDati
161200000000     C                   EXSR      InzSkiera
161300000000     C*
161400131206R141 C                   EndIf
161500131206R141 C*
161600000000     C                   EndSR
161700131206R141 C******************************************************
161800131206     C* Check Import BL
161900131206R141 C******************************************************
162000131206     C     Check_BL      BegSr                                                  *
162100131206     C*
162200131206     C                   Exsr      AzzeraWrkImp
162300131206     C*
162400140414R147 C****               Z-Add     IMPATT05BL    WrkImport01
162500140414R147 C****               Z-Add     IVAATT05BL    WrkImport02
162600140414R147 C****               Z-Add     IMPPAS05BL    WrkImport03
162700140414R147 C****               Z-Add     IVAPAS05BL    WrkImport04
162800131206     C*
162900140414R147 C****               Exsr      CalcWrkTotale
163000131206     C*
163100150306R147 C                   Move      *On           ImpAttOk          1
163200150306 "   C                   Move      *On           ImpPasOk          1
163300150306 "   C*
163400150306 "   C*
163500150306 "   C                   If        IMPATT05BL  < 1        and
163600160329R147 C                             IVAATT05BL  < 1
163601160329R2204C                             and
163602160329 "   C                             NOIBAT05BL  < 1        and
163603160329 "   C                             NOISAT05BL  < 1        and
163604160329 "   C                             IMPNAT05BL  < 1        and
163605160329R2204C                             IVANAT05BL  < 1
163700160329R147 C                   Move      *Off          ImpAttOk
163800150306 "   C                   EndIf
163900150306 "   C*
164000150306 "   C                   If        IMPPAS05BL  < 1        and
164100160329R147 C                             IVAPAS05BL  < 1
164101160329R2204C                             and
164102160329 "   C                             NOIVAP05BL  < 1        and
164103160329 "   C                             IMPNPA05BL  < 1        and
164104160329R2204C                             IVANPA05BL  < 1
164200160329R147 C                   Move      *Off          ImpPasOk
164300150306 "   C                   EndIf
164400150306 "   C*
164500150306 "   C                   If        ImpAttOk    = *Off  and
164600150306 "   C                             ImpPasOk    = *Off
164700150306 "   C                   Eval      Togli_BL    = Togli_BL + 1
164800150309 "   C                   else
164900150309 "   C                   Eval      Numero_BL   = Numero_BL + 1
165000150306R147 C                   EndIf
165100131206     C*
165200131206R141 C                   EndSR
165300000000     C******************************************************
165400000000     C* Tipo Record FE
165500000000     C******************************************************
165600000000     C     GesRec_FE     BegSr                                                  *
165700000000     C*
165800150420R178 C                   If        IMPORT05FE  > 1    or
165900150420R178 C                             IVA05FE     > 1
166000000000     C                   Add       1             Prog_FE
166100000000     C*
166200000000     C*-------------------------------------------
166300000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
166400000000     C*
166500000000     C* 1) Partita Iva
166600000000  a1 C                   If        PIVA05FE       <> *blanks
166700000000R135 C                             and DOCRIE05FE <> '1'
166800000000     C                   Z-Add     Prog_FE       Conta_Rigo
166900000000     C                   Z-Add     1             Conta_Colo
167000000000     C                   Clear                   Campo_Valore
167100000000     C                   MoveL     PIVA05FE      Campo_Valore
167200000000  a2 C                   if        Campo_Valore <> *Blanks
167300000000     C                   Exsr      RieSkiera
167400000000  e2 C                   EndIf
167500000000  x1 C                   else
167600000000     C*
167700000000     C* 2) Codice fiscale
167800000000R135 C                   If        CDFISC05FE     <> *blanks
167900000000R135 C                             and DOCRIE05FE <> '1'
168000000000R135 C                             and PIVA05FE    = *blanks
168100000000     C                   Z-Add     Prog_FE       Conta_Rigo
168200000000     C                   Z-Add     2             Conta_Colo
168300000000     C                   Clear                   Campo_Valore
168400000000     C                   MoveL     CDFISC05FE    Campo_Valore
168500000000  a3 C                   if        Campo_Valore <> *Blanks
168600000000     C                   Exsr      RieSkiera
168700000000  e3 C                   EndIf
168800000000  e2 C                   EndIf
168900000000R135 C                   EndIf
169000000000     C*
169100000000     C* 3) Documento riepilogativo
169200000000     C                   If        DOCRIE05FE    = '1'
169300000000     C                   Z-Add     Prog_FE       Conta_Rigo
169400000000     C                   Z-Add     3             Conta_Colo
169500000000     C                   Clear                   Campo_Valore
169600000000     C                   Move      DOCRIE05FE    Campo_Valore
169700000000     C                   Exsr      Ablank
169800000000     C                   if        Campo_Valore <> *Blanks
169900000000     C                   Exsr      RieSkiera
170000000000     C                   EndIf
170100000000     C                   EndIf
170200000000     C*
170300000000     C* 4) Iva non esposta in fattura (regime del margine)
170400000000     C                   If        MARGIN05FE    <> *blanks
170500000000     C                   Z-Add     Prog_FE       Conta_Rigo
170600000000     C                   Z-Add     4             Conta_Colo
170700000000     C                   Clear                   Campo_Valore
170800000000     C                   Move      MARGIN05FE    Campo_Valore
170900000000     C                   Exsr      Ablank
171000000000     C                   if        Campo_Valore <> *Blanks
171100000000     C                   Exsr      RieSkiera
171200000000     C                   EndIf
171300000000     C                   EndIf
171400000000     C*
171500000000     C* 5) Noleggio
171600000000     C                   If        LEASIN05FE    <> *blanks  and
171700000000     C                             LEASIN05FE    <> '0'
171800000000     C                   Z-Add     Prog_FE       Conta_Rigo
171900000000     C                   Z-Add     5             Conta_Colo
172000000000     C                   Clear                   Campo_Valore
172100000000     C                   MoveL     LEASIN05FE    Campo_Valore
172200000000     C                   Exsr      Ablank
172300000000     C                   if        Campo_Valore <> *Blanks
172400000000     C                   Exsr      RieSkiera
172500000000     C                   EndIf
172600000000     C                   EndIf
172700000000     C*
172800000000     C* 6) Autofatture
172900000000     C                   If        AUTOFA05FE    <> *blanks
173000000000     C                   Z-Add     Prog_FE       Conta_Rigo
173100000000     C                   Z-Add     6             Conta_Colo
173200000000     C                   Clear                   Campo_Valore
173300000000     C                   Move      AUTOFA05FE    Campo_Valore
173400000000     C                   Exsr      Ablank
173500000000     C                   if        Campo_Valore <> *Blanks
173600000000     C                   Exsr      RieSkiera
173700000000     C                   EndIf
173800000000     C                   EndIf
173900000000     C*
174000000000     C* 7) Data del docuemnto
174100000000     C                   If        DTDOC05FE     <> *Loval
174200000000     C                   Move      DTDOC05FE     DtAMG
174300000000     C                   Move      AMGAnno       GMAAnno
174400000000     C                   Move      AMGMese       GMAMese
174500000000     C                   Move      AMGGiorno     GMAGiorno
174600000000     C                   Z-Add     Prog_FE       Conta_Rigo
174700000000     C                   Z-Add     7             Conta_Colo
174800000000     C                   Clear                   Campo_Valore
174900000000     C                   Move      DtGMA         Campo_Valore
175000000000     C                   if        Campo_Valore <> *Blanks
175100000000     C                   Exsr      RieSkiera
175200000000     C                   EndIf
175300000000     C                   EndIf
175400000000     C*
175500000000     C* 8) Data di registrazione
175600000000     C                   If        DTREG05FE     <> *Loval
175700000000     C                   Move      DTREG05FE     DtAMG
175800000000     C                   Move      AMGAnno       GMAAnno
175900000000     C                   Move      AMGMese       GMAMese
176000000000     C                   Move      AMGGiorno     GMAGiorno
176100000000     C                   Z-Add     Prog_FE       Conta_Rigo
176200000000     C                   Z-Add     8             Conta_Colo
176300000000     C                   Clear                   Campo_Valore
176400000000     C                   Move      DtGMA         Campo_Valore
176500000000     C                   if        Campo_Valore <> *Blanks
176600000000     C                   Exsr      RieSkiera
176700000000     C                   EndIf
176800000000     C                   EndIf
176900000000     C*
177000000000     C* 9) Nr.fatture / documento riepilogativo
177100000000     C                   If        NRDOC05FE     <> 0
177200000000     C                   Z-Add     Prog_FE       Conta_Rigo
177300000000     C                   Z-Add     9             Conta_Colo
177400000000     C                   Clear                   Campo_Valore
177500000000     C                   MoveL     NRDOC05FE     Campo_Valore
177600000000     C                   if        Campo_Valore <> *Blanks
177700000000     C                   Exsr      RieSkiera
177800000000     C                   EndIf
177900000000     C                   EndIf
178000000000     C*
178100000000     C* 10) Importo
178200000000     C                   If        IMPORT05FE    >= 0
178300000000     C                   Z-Add     Prog_FE       Conta_Rigo
178400000000     C                   Z-Add     10            Conta_Colo
178500000000     C                   Clear                   Campo_Valore
178600000000     C                   Z-Add     IMPORT05FE    Importo16_0
178700000000     C                   Exsr      Ablank2
178800000000     C                   if        Campo_Valore <> *Blanks
178900000000     C                   Exsr      RieSkiera
179000000000     C                   EndIf
179100000000     C                   EndIf
179200000000     C*
179300000000     C* 11) Imposta
179400000000     C                   If        IVA05FE       >= 0
179500000000     C                   Z-Add     Prog_FE       Conta_Rigo
179600000000     C                   Z-Add     11            Conta_Colo
179700000000     C                   Clear                   Campo_Valore
179800000000     C                   Z-Add     IVA05FE       Importo16_0
179900000000     C                   Exsr      Ablank2
180000000000     C                   if        Campo_Valore <> *Blanks
180100000000     C                   Exsr      RieSkiera
180200000000     C                   EndIf
180300000000     C                   EndIf
180400000000     C*
180500000000     C* 12) Conferma importo
180600000000     C                   If        IMPORT05FE     > 999999
180700000000     C                   Z-Add     Prog_FE       Conta_Rigo
180800000000     C                   Z-Add     12            Conta_Colo
180900000000     C                   Clear                   Campo_Valore
181000000000     C                   Move      '1'           Campo_Valore
181100000000     C                   Exsr      Ablank
181200000000     C                   if        Campo_Valore <> *Blanks
181300000000     C                   Exsr      RieSkiera
181400000000     C                   EndIf
181500000000     C                   EndIf
181600000000     C*
181700000000     C                   IF        Prog_FE       = 6
181800000000     C                   Z-Add     0             Prog_FE
181900000000     C                   Add       1             ProgMod_FE
182000000000     C                   Z-Add     ProgMod_FE    WrkProgMod
182100000000     C                   EXSR      WrtDati
182200000000     C                   EXSR      InzSkiera
182300000000     C                   EndIf
182400000000     C*
182500150420R178 C                   EndIf
182600150420R178 C*
182700000000     C                   EndSR
182800000000     C******************************************************
182900000000     C* Tipo Record FR
183000000000     C******************************************************
183100000000     C     GesRec_FR     BegSr                                                  *
183200000000     C*
183300150420R178 C                   If        IMPORT05FR  > 1    or
183400150420R178 C                             IVA05FR     > 1
183500000000     C                   Add       1             Prog_FR
183600000000     C*
183700000000     C*-------------------------------------------
183800000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
183900000000     C*
184000000000     C* 1) Partita Iva
184100000000     C                   If        PIVA05FR      <> *blanks
184200000000R135 C                             and DOCRIE05FR  <> '1'
184300000000     C                   Z-Add     Prog_FR       Conta_Rigo
184400000000     C                   Z-Add     1             Conta_Colo
184500000000     C                   Clear                   Campo_Valore
184600000000     C                   MoveL     PIVA05FR      Campo_Valore
184700000000     C                   if        Campo_Valore <> *Blanks
184800000000     C                   Exsr      RieSkiera
184900000000     C                   EndIf
185000000000     C                   EndIf
185100000000     C*
185200000000     C* 2) Documento riepilogativo
185300000000     C                   If        DOCRIE05FR    = '1'
185400000000     C                   Z-Add     Prog_FR       Conta_Rigo
185500000000     C                   Z-Add     2             Conta_Colo
185600000000     C                   Clear                   Campo_Valore
185700000000     C                   Move      DOCRIE05FR    Campo_Valore
185800000000     C                   Exsr      Ablank
185900000000     C                   if        Campo_Valore <> *Blanks
186000000000     C                   Exsr      RieSkiera
186100000000     C                   EndIf
186200000000     C                   EndIf
186300000000     C*
186400000000     C* 3) Data del documento
186500000000     C                   If        DTDOC05FR     <> *Loval
186600000000     C                   Move      DTDOC05FR     DtAMG
186700000000     C                   Move      AMGAnno       GMAAnno
186800000000     C                   Move      AMGMese       GMAMese
186900000000     C                   Move      AMGGiorno     GMAGiorno
187000000000     C                   Z-Add     Prog_FR       Conta_Rigo
187100000000     C                   Z-Add     3             Conta_Colo
187200000000     C                   Clear                   Campo_Valore
187300000000     C                   Move      DtGMA         Campo_Valore
187400000000     C                   if        Campo_Valore <> *Blanks
187500000000     C                   Exsr      RieSkiera
187600000000     C                   EndIf
187700000000     C                   EndIf
187800000000     C*
187900000000     C* 4) Data di registrazione
188000000000     C                   If        DTREG05FR     <> *Loval
188100000000     C                   Move      DTREG05FR     DtAMG
188200000000     C                   Move      AMGAnno       GMAAnno
188300000000     C                   Move      AMGMese       GMAMese
188400000000     C                   Move      AMGGiorno     GMAGiorno
188500000000     C                   Z-Add     Prog_FR       Conta_Rigo
188600000000     C                   Z-Add     4             Conta_Colo
188700000000     C                   Clear                   Campo_Valore
188800000000     C                   Move      DtGMA         Campo_Valore
188900000000     C                   if        Campo_Valore <> *Blanks
189000000000     C                   Exsr      RieSkiera
189100000000     C                   EndIf
189200000000     C                   EndIf
189300000000     C*
189400000000     C* 5) Iva non esposta in fattura (regime del margine)
189500000000     C                   If        MARGIN05FR    <> *blanks
189600000000     C                   Z-Add     Prog_FR       Conta_Rigo
189700000000     C                   Z-Add     5             Conta_Colo
189800000000     C                   Clear                   Campo_Valore
189900000000     C                   Move      MARGIN05FR    Campo_Valore
190000000000     C                   Exsr      Ablank
190100000000     C                   if        Campo_Valore <> *Blanks
190200000000     C                   Exsr      RieSkiera
190300000000     C                   EndIf
190400000000     C                   EndIf
190500000000     C*
190600000000     C* 6) Reverse charge
190700000000     C                   If        REVERS05FR    <> *blanks
190800000000     C                   Z-Add     Prog_FR       Conta_Rigo
190900000000     C                   Z-Add     6             Conta_Colo
191000000000     C                   Clear                   Campo_Valore
191100000000     C                   Move      REVERS05FR    Campo_Valore
191200000000     C                   Exsr      Ablank
191300000000     C                   if        Campo_Valore <> *Blanks
191400000000     C                   Exsr      RieSkiera
191500000000     C                   EndIf
191600000000     C                   EndIf
191700000000     C*
191800000000     C* 7) Autofatture
191900000000     C                   If        AUTOFA05FR    <> *blanks
192000000000     C                   Z-Add     Prog_FR       Conta_Rigo
192100000000     C                   Z-Add     7             Conta_Colo
192200000000     C                   Clear                   Campo_Valore
192300000000     C                   Move      AUTOFA05FR    Campo_Valore
192400000000     C                   Exsr      Ablank
192500000000     C                   if        Campo_Valore <> *Blanks
192600000000     C                   Exsr      RieSkiera
192700000000     C                   EndIf
192800000000     C                   EndIf
192900000000     C*
193000000000     C* 8) Importo
193100000000     C                   If        IMPORT05FR    >= 0
193200000000     C                   Z-Add     Prog_FR       Conta_Rigo
193300000000     C                   Z-Add     8             Conta_Colo
193400000000     C                   Clear                   Campo_Valore
193500000000     C                   Z-Add     IMPORT05FR    Importo16_0
193600000000     C                   Exsr      Ablank2
193700000000     C                   if        Campo_Valore <> *Blanks
193800000000     C                   Exsr      RieSkiera
193900000000     C                   EndIf
194000000000     C                   EndIf
194100000000     C*
194200000000     C* 9) Imposta
194300000000     C                   If        IVA05FR       >= 0
194400000000     C                   Z-Add     Prog_FR       Conta_Rigo
194500000000     C                   Z-Add     9             Conta_Colo
194600000000     C                   Clear                   Campo_Valore
194700000000     C                   Z-Add     IVA05FR       Importo16_0
194800000000     C                   Exsr      Ablank2
194900000000     C                   if        Campo_Valore <> *Blanks
195000000000     C                   Exsr      RieSkiera
195100000000     C                   EndIf
195200000000     C                   EndIf
195300000000     C*
195400000000     C* 10) Conferma importo
195500000000     C                   If        IMPORT05FR     > 999999
195600000000     C                   Z-Add     Prog_FR       Conta_Rigo
195700000000     C                   Z-Add     10            Conta_Colo
195800000000     C                   Clear                   Campo_Valore
195900000000     C                   Move      '1'           Campo_Valore
196000000000     C                   Exsr      Ablank
196100000000     C                   if        Campo_Valore <> *Blanks
196200000000     C                   Exsr      RieSkiera
196300000000     C                   EndIf
196400000000     C                   EndIf
196500000000     C*
196600000000     C                   IF        Prog_FR       = 6
196700000000     C                   Z-Add     0             Prog_FR
196800000000     C                   Add       1             ProgMod_FR
196900000000     C                   Z-Add     ProgMod_FR    WrkProgMod
197000000000     C                   EXSR      WrtDati
197100000000     C                   EXSR      InzSkiera
197200000000     C                   EndIf
197300000000     C*
197400150420R178 C                   EndIf
197500150420R178 C*
197600000000     C                   EndSR
197700000000     C******************************************************
197800000000     C* Tipo Record NE
197900000000     C******************************************************
198000000000     C     GesRec_NE     BegSr                                                  *
198100000000     C*
198200150420R178 C                   Eval      WrkImport01 = IMPORT05NE
198300150420 "   C                   Eval      WrkImport02 = IVA05NE
198400150420 "   C                   If        WrkImport01 <> 0    or
198500150420R178 C                             WrkImport02 <> 0
198600000000     C                   Eval      $FlgNeg       = '0'
198700000000     C                   Add       1             Prog_NE
198800000000     C*
198900000000     C*-------------------------------------------
199000000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
199100000000     C*
199200000000     C* 1) Partita Iva
199300000000     C                   If        PIVA05NE      <> *blanks
199400000000     C                   Z-Add     Prog_NE       Conta_Rigo
199500000000     C                   Z-Add     1             Conta_Colo
199600000000     C                   Clear                   Campo_Valore
199700000000     C                   MoveL     PIVA05NE      Campo_Valore
199800000000     C                   if        Campo_Valore <> *Blanks
199900000000     C                   Exsr      RieSkiera
200000000000     C                   EndIf
200100000000     C                   else
200200000000     C*
200300000000     C* 2) Codice fiscale
200400000000     C                   Z-Add     Prog_NE       Conta_Rigo
200500000000     C                   Z-Add     2             Conta_Colo
200600000000     C                   Clear                   Campo_Valore
200700000000     C                   MoveL     CDFISC05NE    Campo_Valore
200800000000     C                   if        Campo_Valore <> *Blanks
200900000000     C                   Exsr      RieSkiera
201000000000     C                   EndIf
201100000000     C                   EndIf
201200000000     C*
201300000000     C* 3) Data emissione nota
201400000000     C                   If        DTDOC05NE     <> *Loval
201500000000     C                   Move      DTDOC05NE     DtAMG
201600000000     C                   Move      AMGAnno       GMAAnno
201700000000     C                   Move      AMGMese       GMAMese
201800000000     C                   Move      AMGGiorno     GMAGiorno
201900000000     C                   Z-Add     Prog_NE       Conta_Rigo
202000000000     C                   Z-Add     3             Conta_Colo
202100000000     C                   Clear                   Campo_Valore
202200000000     C                   Move      DtGMA         Campo_Valore
202300000000     C                   if        Campo_Valore <> *Blanks
202400000000     C                   Exsr      RieSkiera
202500000000     C                   EndIf
202600000000     C                   EndIf
202700000000     C*
202800000000     C* 4) Data di registrazione
202900000000     C                   If        DTREG05NE     <> *Loval
203000000000     C                   Move      DTREG05NE     DtAMG
203100000000     C                   Move      AMGAnno       GMAAnno
203200000000     C                   Move      AMGMese       GMAMese
203300000000     C                   Move      AMGGiorno     GMAGiorno
203400000000     C                   Z-Add     Prog_NE       Conta_Rigo
203500000000     C                   Z-Add     4             Conta_Colo
203600000000     C                   Clear                   Campo_Valore
203700000000     C                   Move      DtGMA         Campo_Valore
203800000000     C                   if        Campo_Valore <> *Blanks
203900000000     C                   Exsr      RieSkiera
204000000000     C                   EndIf
204100000000     C                   EndIf
204200000000     C*
204300000000     C* 5) Nr.Nota
204400000000     C                   If        NRDOC05NE     <> 0
204500000000     C                   Z-Add     Prog_NE       Conta_Rigo
204600000000     C                   Z-Add     5             Conta_Colo
204700000000     C                   Clear                   Campo_Valore
204800000000     C                   MoveL     NRDOC05NE     Campo_Valore
204900000000     C                   if        Campo_Valore <> *Blanks
205000000000     C                   Exsr      RieSkiera
205100000000     C                   EndIf
205200000000     C                   EndIf
205300000000     C*
205400000000     C* 6) Importo
205500000000     C                   If        IMPORT05NE   <>  0
205600000000     C                   Z-Add     Prog_NE       Conta_Rigo
205700000000     C                   Z-Add     6             Conta_Colo
205800000000     C                   Clear                   Campo_Valore
205900000000     C                   Z-Add     IMPORT05NE    Importo16_0
206000000000     C                   If        IMPORT05NE   <   0
206100000000     C                   Eval      Importo16_0  = Importo16_0 * -1
206200000000     C                   Eval      $FlgNeg       = '1'
206300000000     C                   EndIf
206400000000     C                   Exsr      Ablank2
206500000000     C                   if        Campo_Valore <> *Blanks
206600000000     C                   Exsr      RieSkiera
206700000000     C                   EndIf
206800000000     C                   EndIf
206900000000     C*
207000000000     C* 7) Imposta
207100000000     C                   If        IVA05NE      <>  0
207200000000     C                   Z-Add     Prog_NE       Conta_Rigo
207300000000     C                   Z-Add     7             Conta_Colo
207400000000     C                   Clear                   Campo_Valore
207500000000     C                   Z-Add     IVA05NE       Importo16_0
207600000000     C                   If        IVA05NE      <   0
207700000000     C                   Eval      Importo16_0  = Importo16_0 * -1
207800000000     C                   Eval      $FlgNeg       = '1'
207900000000     C                   EndIf
208000000000     C                   Exsr      Ablank2
208100000000     C                   if        Campo_Valore <> *Blanks
208200000000     C                   Exsr      RieSkiera
208300000000     C                   EndIf
208400000000     C                   EndIf
208500000000     C*
208600000000     C* 8) Conferma importo
208700000000     C                   If        IMPORT05NE     > 999999
208800000000     C                   Z-Add     Prog_NE       Conta_Rigo
208900000000     C                   Z-Add     8             Conta_Colo
209000000000     C                   Clear                   Campo_Valore
209100000000     C                   Move      '1'           Campo_Valore
209200000000     C                   Exsr      Ablank
209300000000     C                   if        Campo_Valore <> *Blanks
209400000000     C                   Exsr      RieSkiera
209500000000     C                   EndIf
209600000000     C                   EndIf
209700000000     C*
209800000000     C                   IF        Prog_NE       = 10
209900000000     C                   Z-Add     0             Prog_NE
210000000000     C                   Add       1             ProgMod_NE
210100000000     C                   Z-Add     ProgMod_NE    WrkProgMod
210200000000     C                   EXSR      WrtDati
210300000000     C                   EXSR      InzSkiera
210400000000     C                   EndIf
210500000000     C*
210600150420R178 C                   EndIf
210700150420R178 C*
210800000000     C                   EndSR
210900000000     C******************************************************
211000000000     C* Tipo Record NR
211100000000     C******************************************************
211200000000     C     GesRec_NR     BegSr                                                  *
211300000000     C*
211400150420R178 C                   Eval      WrkImport01 = IMPORT05NR
211500150420 "   C                   Eval      WrkImport02 = IVA05NR
211600150420 "   C                   If        WrkImport01 <> 0    or
211700150420R178 C                             WrkImport02 <> 0
211800000000     C                   Eval      $FlgNeg       = '0'
211900000000     C                   Add       1             Prog_NR
212000000000     C*
212100000000     C*-------------------------------------------
212200000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
212300000000     C*
212400000000     C* 1) Partita Iva
212500000000     C                   If        PIVA05NR      <> *blanks
212600000000     C                   Z-Add     Prog_NR       Conta_Rigo
212700000000     C                   Z-Add     1             Conta_Colo
212800000000     C                   Clear                   Campo_Valore
212900000000     C                   MoveL     PIVA05NR      Campo_Valore
213000000000     C                   if        Campo_Valore <> *Blanks
213100000000     C                   Exsr      RieSkiera
213200000000     C                   EndIf
213300000000     C                   EndIf
213400000000     C*
213500000000     C* 2) Data del documento
213600000000     C                   If        DTDOC05NR     <> *Loval
213700000000     C                   Move      DTDOC05NR     DtAMG
213800000000     C                   Move      AMGAnno       GMAAnno
213900000000     C                   Move      AMGMese       GMAMese
214000000000     C                   Move      AMGGiorno     GMAGiorno
214100000000     C                   Z-Add     Prog_NR       Conta_Rigo
214200000000     C                   Z-Add     2             Conta_Colo
214300000000     C                   Clear                   Campo_Valore
214400000000     C                   Move      DtGMA         Campo_Valore
214500000000     C                   if        Campo_Valore <> *Blanks
214600000000     C                   Exsr      RieSkiera
214700000000     C                   EndIf
214800000000     C                   EndIf
214900000000     C*
215000000000     C* 3) Data di registrazione
215100000000     C                   If        DTREG05NR     <> *Loval
215200000000     C                   Move      DTREG05NR     DtAMG
215300000000     C                   Move      AMGAnno       GMAAnno
215400000000     C                   Move      AMGMese       GMAMese
215500000000     C                   Move      AMGGiorno     GMAGiorno
215600000000     C                   Z-Add     Prog_NR       Conta_Rigo
215700000000     C                   Z-Add     3             Conta_Colo
215800000000     C                   Clear                   Campo_Valore
215900000000     C                   Move      DtGMA         Campo_Valore
216000000000     C                   if        Campo_Valore <> *Blanks
216100000000     C                   Exsr      RieSkiera
216200000000     C                   EndIf
216300000000     C                   EndIf
216400000000     C*
216500000000     C* 4) Importo
216600000000     C                   If        IMPORT05NR   <>  0
216700000000     C                   Z-Add     Prog_NR       Conta_Rigo
216800000000     C                   Z-Add     4             Conta_Colo
216900000000     C                   Clear                   Campo_Valore
217000000000     C                   Z-Add     IMPORT05NR    Importo16_0
217100000000     C                   If        IMPORT05NR   <   0
217200000000     C                   Eval      Importo16_0  = Importo16_0 * -1
217300000000     C                   Eval      $FlgNeg       = '1'
217400000000     C                   EndIf
217500000000     C                   Exsr      Ablank2
217600000000     C                   if        Campo_Valore <> *Blanks
217700000000     C                   Exsr      RieSkiera
217800000000     C                   EndIf
217900000000     C                   EndIf
218000000000     C*
218100000000     C* 5) Imposta
218200000000     C                   If        IVA05NR      <>  0
218300000000     C                   Z-Add     Prog_NR       Conta_Rigo
218400000000     C                   Z-Add     5             Conta_Colo
218500000000     C                   Clear                   Campo_Valore
218600000000     C                   Z-Add     IVA05NR       Importo16_0
218700000000     C                   If        IVA05NR      <   0
218800000000     C                   Eval      Importo16_0  = Importo16_0 * -1
218900000000     C                   Eval      $FlgNeg       = '1'
219000000000     C                   EndIf
219100000000     C                   Exsr      Ablank2
219200000000     C                   if        Campo_Valore <> *Blanks
219300000000     C                   Exsr      RieSkiera
219400000000     C                   EndIf
219500000000     C                   EndIf
219600000000     C*
219700000000     C* 6) Conferma importo
219800000000     C                   If        IMPORT05NR     > 999999
219900000000     C                   Z-Add     Prog_NR       Conta_Rigo
220000000000     C                   Z-Add     6             Conta_Colo
220100000000     C                   Clear                   Campo_Valore
220200000000     C                   Move      '1'           Campo_Valore
220300000000     C                   Exsr      Ablank
220400000000     C                   if        Campo_Valore <> *Blanks
220500000000     C                   Exsr      RieSkiera
220600000000     C                   EndIf
220700000000     C                   EndIf
220800000000     C*
220900000000     C                   IF        Prog_NR       = 10
221000000000     C                   Z-Add     0             Prog_NR
221100000000     C                   Add       1             ProgMod_NR
221200000000     C                   Z-Add     ProgMod_NR    WrkProgMod
221300000000     C                   EXSR      WrtDati
221400000000     C                   EXSR      InzSkiera
221500000000     C                   EndIf
221600000000     C*
221700150420R178 C                   EndIf
221800150420R178 C*
221900000000     C                   EndSR
222000000000     C******************************************************
222100000000     C* Tipo Record DF
222200000000     C******************************************************
222300000000     C     GesRec_DF     BegSr                                                  *
222400000000     C*
222500150420R178 C                   If        IMPORT05DF >= 1
222600000000     C                   Add       1             Prog_DF
222700000000     C*
222800000000     C*-------------------------------------------
222900000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
223000000000     C*
223100000000     C* 1) Codice fiscale
223200000000     C                   If        CDFISC05DF    <> *blanks
223300000000     C                   Z-Add     Prog_DF       Conta_Rigo
223400000000     C                   Z-Add     1             Conta_Colo
223500000000     C                   Clear                   Campo_Valore
223600000000     C                   MoveL     CDFISC05DF    Campo_Valore
223700000000     C                   if        Campo_Valore <> *Blanks
223800000000     C                   Exsr      RieSkiera
223900000000     C                   EndIf
224000000000     C                   EndIf
224100000000     C*
224200000000     C* 2) Data operazione
224300000000     C                   If        DTREG05DF     <> *Loval
224400000000     C                   Move      DTREG05DF     DtAMG
224500000000     C                   Move      AMGAnno       GMAAnno
224600000000     C                   Move      AMGMese       GMAMese
224700000000     C                   Move      AMGGiorno     GMAGiorno
224800000000     C                   Z-Add     Prog_DF       Conta_Rigo
224900000000     C                   Z-Add     2             Conta_Colo
225000000000     C                   Clear                   Campo_Valore
225100000000     C                   Move      DtGMA         Campo_Valore
225200000000     C                   if        Campo_Valore <> *Blanks
225300000000     C                   Exsr      RieSkiera
225400000000     C                   EndIf
225500000000     C                   EndIf
225600000000     C*
225700000000     C* 3) Importo
225800000000     C                   If        IMPORT05DF    >= 0
225900000000     C                   Z-Add     Prog_DF       Conta_Rigo
226000000000     C                   Z-Add     3             Conta_Colo
226100000000     C                   Clear                   Campo_Valore
226200000000     C                   Z-Add     IMPORT05DF    Importo16_0
226300000000     C                   Exsr      Ablank2
226400000000     C                   if        Campo_Valore <> *Blanks
226500000000     C                   Exsr      RieSkiera
226600000000     C                   EndIf
226700000000     C                   EndIf
226800000000     C*
226900000000     C* 4) Noleggio Leasing
227000000000     C                   If        LEASIN05DF    <> *blanks and
227100000000     C                             LEASIN05DF    <> '0'
227200000000     C                   Z-Add     Prog_DF       Conta_Rigo
227300000000     C                   Z-Add     4             Conta_Colo
227400000000     C                   Clear                   Campo_Valore
227500000000R135 C****               Move      '1'           Campo_Valore
227600000000R135 C                   MoveL     LEASIN05DF    Campo_Valore
227700000000     C                   if        Campo_Valore <> *Blanks
227800000000     C                   Exsr      RieSkiera
227900000000     C                   EndIf
228000000000     C                   EndIf
228100000000     C*
228200000000     C* 5) Conferma importo
228300000000     C                   If        IMPORT05DF     > 999999
228400000000     C                   Z-Add     Prog_DF       Conta_Rigo
228500000000     C                   Z-Add     5             Conta_Colo
228600000000     C                   Clear                   Campo_Valore
228700000000     C                   Move      '1'           Campo_Valore
228800000000     C                   Exsr      Ablank
228900000000     C                   if        Campo_Valore <> *Blanks
229000000000     C                   Exsr      RieSkiera
229100000000     C                   EndIf
229200000000     C                   EndIf
229300000000     C*
229400000000     C                   IF        Prog_DF       = 10
229500000000     C                   Z-Add     0             Prog_DF
229600000000     C                   Add       1             ProgMod_DF
229700000000     C                   Z-Add     ProgMod_DF    WrkProgMod
229800000000     C                   EXSR      WrtDati
229900000000     C                   EXSR      InzSkiera
230000000000     C                   EndIf
230100000000     C*
230200150420R178 C                   EndIf
230300150420R178 C*
230400000000     C                   EndSR
230500000000     C******************************************************
230600000000     C* Tipo Record FN
230700000000     C******************************************************
230800000000     C     GesRec_FN     BegSr                                                  *
230900000000     C*
231000150420R178 C                   Eval      WrkImport01 = IMPORT05FN
231100150420 "   C                   Eval      WrkImport02 = IVA05FN
231200150420 "   C                   If        WrkImport01 <> 0    or
231300150420R178 C                             WrkImport02 <> 0
231400000000     C                   Add       1             Prog_FN
231500000000     C*
231600000000     C*-------------------------------------------
231700000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
231800000000     C*
231900000000     C* 1) Cognome
232000000000     C                   If        COGRAG05FN    <> *blanks
232100000000     C                   Z-Add     Prog_FN       Conta_Rigo
232200000000     C                   Z-Add     1             Conta_Colo
232300000000     C                   Clear                   Campo_Valore
232400000000     C                   MoveL     COGRAG05FN    Campo_Valore
232500000000     C                   MoveL     COGRAG05FN    C60
232600000000     C                   if        Campo_Valore <> *Blanks
232700000000     C                   Exsr      RieSkieraAlf
232800000000     C                   EndIf
232900000000     C                   EndIf
233000000000     C*
233100000000     C* 2) Nome
233200000000     C                   If        NOME05FN      <> *Blanks
233300000000     C                   Z-Add     Prog_FN       Conta_Rigo
233400000000     C                   Z-Add     2             Conta_Colo
233500000000     C                   Clear                   Campo_Valore
233600000000     C                   MoveL     NOME05FN      Campo_Valore
233700000000     C                   MoveL     NOME05FN      C60
233800000000     C                   if        Campo_Valore <> *Blanks
233900000000     C                   Exsr      RieSkieraAlf
234000000000     C                   EndIf
234100000000     C                   EndIf
234200000000     C*
234300000000     C* 3) Data di nascita
234400000000     C                   If        DTNASC05FN    <> *Loval
234500000000     C                   Move      DTNASC05FN    DtAMG
234600000000     C                   Move      AMGAnno       GMAAnno
234700000000     C                   Move      AMGMese       GMAMese
234800000000     C                   Move      AMGGiorno     GMAGiorno
234900000000     C                   Z-Add     Prog_FN       Conta_Rigo
235000000000     C                   Z-Add     3             Conta_Colo
235100000000     C                   Clear                   Campo_Valore
235200000000     C                   Move      DtGMA         Campo_Valore
235300000000     C                   if        Campo_Valore <> *Blanks
235400000000     C                   Exsr      RieSkiera
235500000000     C                   EndIf
235600000000     C                   EndIf
235700000000     C*
235800000000     C* 4) Comune o Stato estero di nascita
235900000000     C                   If        LOCNAS05FN    <> *Blanks
236000000000     C                   Z-Add     Prog_FN       Conta_Rigo
236100000000     C                   Z-Add     4             Conta_Colo
236200000000     C                   Clear                   Campo_Valore
236300000000     C                   MoveL     LOCNAS05FN    Campo_Valore
236400000000     C                   MoveL     LOCNAS05FN    C60
236500000000     C                   if        Campo_Valore <> *Blanks
236600000000     C                   Exsr      RieSkieraAlf
236700000000     C                   EndIf
236800000000     C                   EndIf
236900000000     C*
237000000000     C* 5) Provincia
237100000000     C                   If        PRVNAS05FN    <> *Blanks
237200000000     C                   Z-Add     Prog_FN       Conta_Rigo
237300000000     C                   Z-Add     5             Conta_Colo
237400000000     C                   Clear                   Campo_Valore
237500000000     C                   MoveL     PRVNAS05FN    Campo_Valore
237600000000     C                   If        COGRAG05FN <> *blanks  and
237700000000     C                             UICSocieta <> UIC105FN
237800000000     C                   Clear                   Campo_Valore
237900000000     C                   MoveL     'EE'          Campo_Valore
238000000000     C                   EndIf
238100000000     C                   if        Campo_Valore <> *Blanks
238200000000     C                   Exsr      RieSkiera
238300000000     C                   EndIf
238400000000     C                   EndIf
238500000000     C*
238600000000     C* 6) Codice stato estero
238700000000     C                   If        UIC105FN      <> 0
238800000000     C                   Z-Add     Prog_FN       Conta_Rigo
238900000000     C                   Z-Add     6             Conta_Colo
239000000000     C                   Clear                   Campo_Valore
239100000000     C                   Move      UIC105FN      Campo_Valore
239200000000     C                   if        Campo_Valore <> *Blanks
239300000000     C                   Exsr      RieSkiera
239400000000     C                   EndIf
239500000000     C                   EndIf
239600000000     C*
239700000000     C* 7) Denominazione / ragione sociale
239800000000     C                   If        RAGSOC05FN    <> *blanks
239900000000     C                   Z-Add     Prog_FN       Conta_Rigo
240000000000     C                   Z-Add     7             Conta_Colo
240100000000     C                   Clear                   Campo_Valore
240200000000     C                   MoveL     RAGSOC05FN    Campo_Valore
240300000000     C                   MoveL     RAGSOC05FN    C60
240400000000     C                   if        Campo_Valore <> *Blanks
240500000000     C                   Exsr      RieSkieraAlf
240600000000     C                   EndIf
240700000000     C                   EndIf
240800000000     C*
240900000000     C* 8) Citta sede legale
241000000000     C                   If        CITTAE05FN    <> *blanks
241100000000     C                   Z-Add     Prog_FN       Conta_Rigo
241200000000     C                   Z-Add     8             Conta_Colo
241300000000     C                   Clear                   Campo_Valore
241400000000     C                   MoveL     CITTAE05FN    Campo_Valore
241500000000     C                   MoveL     CITTAE05FN    C60
241600000000     C                   if        Campo_Valore <> *Blanks
241700000000     C                   Exsr      RieSkieraAlf
241800000000     C                   EndIf
241900000000     C                   EndIf
242000000000     C*
242100000000     C* 9) Codice stato estero
242200000000     C                   If        UIC205FN      <> 0
242300000000     C                   Z-Add     Prog_FN       Conta_Rigo
242400000000     C                   Z-Add     9             Conta_Colo
242500000000     C                   Clear                   Campo_Valore
242600000000     C                   Move      UIC205FN      Campo_Valore
242700000000     C                   if        Campo_Valore <> *Blanks
242800000000     C                   Exsr      RieSkiera
242900000000     C                   EndIf
243000000000     C                   EndIf
243100000000     C*
243200000000     C* 10) Indirizzo estero sede legale
243300000000     C                   If        INDIRE05FN    <> *blanks
243400000000     C                   Z-Add     Prog_FN       Conta_Rigo
243500000000     C                   Z-Add     10            Conta_Colo
243600000000     C                   Clear                   Campo_Valore
243700000000     C                   MoveL     INDIRE05FN    Campo_Valore
243800000000     C                   MoveL     INDIRE05FN    C60
243900000000     C                   if        Campo_Valore <> *Blanks
244000000000     C                   Exsr      RieSkieraAlf
244100000000     C                   EndIf
244200000000     C                   EndIf
244300000000     C*
244400000000     C* 11) Data emissione del documento fattura
244500000000     C                   If        DTDOC05FN     <> *Loval
244600000000     C                   Move      DTDOC05FN     DtAMG
244700000000     C                   Move      AMGAnno       GMAAnno
244800000000     C                   Move      AMGMese       GMAMese
244900000000     C                   Move      AMGGiorno     GMAGiorno
245000000000     C                   Z-Add     Prog_FN       Conta_Rigo
245100000000     C                   Z-Add     11            Conta_Colo
245200000000     C                   Clear                   Campo_Valore
245300000000     C                   Move      DtGMA         Campo_Valore
245400000000     C                   if        Campo_Valore <> *Blanks
245500000000     C                   Exsr      RieSkiera
245600000000     C                   EndIf
245700000000     C                   EndIf
245800000000     C*
245900000000     C* 12) Data di registrazione fattura
246000000000     C                   If        DTREG05FN     <> *Loval
246100000000     C                   Move      DTREG05FN     DtAMG
246200000000     C                   Move      AMGAnno       GMAAnno
246300000000     C                   Move      AMGMese       GMAMese
246400000000     C                   Move      AMGGiorno     GMAGiorno
246500000000     C                   Z-Add     Prog_FN       Conta_Rigo
246600000000     C                   Z-Add     12            Conta_Colo
246700000000     C                   Clear                   Campo_Valore
246800000000     C                   Move      DtGMA         Campo_Valore
246900000000     C                   if        Campo_Valore <> *Blanks
247000000000     C                   Exsr      RieSkiera
247100000000     C                   EndIf
247200000000     C                   EndIf
247300000000     C*
247400000000     C* 13) Nr.Fattura
247500000000     C                   If        NRDOC05FN     <> 0
247600000000     C                   Z-Add     Prog_FN       Conta_Rigo
247700000000     C                   Z-Add     13            Conta_Colo
247800000000     C                   Clear                   Campo_Valore
247900000000     C                   MoveL     NRDOC05FN     Campo_Valore
248000000000     C****               Exsr      Ablank
248100000000     C                   if        Campo_Valore <> *Blanks
248200000000     C                   Exsr      RieSkiera
248300000000     C                   EndIf
248400000000     C                   EndIf
248500000000     C*
248600000000     C* 14) Noleggio Leasing
248700000000     C                   If        LEASIN05FN    <> *blanks  and
248800000000     C                             LEASIN05FN    <> '0'
248900000000     C                   Z-Add     Prog_FN       Conta_Rigo
249000000000     C                   Z-Add     14            Conta_Colo
249100000000     C                   Clear                   Campo_Valore
249200000000     C                   MoveL     LEASIN05FN    Campo_Valore
249300000000     C                   if        Campo_Valore <> *Blanks
249400000000     C                   Exsr      RieSkiera
249500000000     C                   EndIf
249600000000     C                   EndIf
249700000000     C*
249800000000     C* 15) Importo
249900000000     C                   If        IMPORT05FN    <> 0
250000000000     C                   If        IMPORT05FN    <  0
250100000000     C                   Eval      IMPORT05FN  = IMPORT05FN * -1
250200000000     C                   EndIf
250300000000     C                   Z-Add     Prog_FN       Conta_Rigo
250400000000     C                   Z-Add     15            Conta_Colo
250500000000     C                   Clear                   Campo_Valore
250600000000     C                   Z-Add     IMPORT05FN    Importo16_0
250700000000     C                   Exsr      Ablank2
250800000000     C                   if        Campo_Valore <> *Blanks
250900000000     C                   Exsr      RieSkiera
251000000000     C                   EndIf
251100000000     C                   EndIf
251200000000     C*
251300000000     C* 16) Imposta
251400000000     C                   If        IVA05FN       <> 0
251500000000     C                   If        IVA05FN       <  0
251600000000     C                   Eval      IVA05FN     = IVA05FN    * -1
251700000000     C                   EndIf
251800000000     C                   Z-Add     Prog_FN       Conta_Rigo
251900000000     C                   Z-Add     16            Conta_Colo
252000000000     C                   Clear                   Campo_Valore
252100000000     C                   Z-Add     IVA05FN       Importo16_0
252200000000     C                   Exsr      Ablank2
252300000000     C                   if        Campo_Valore <> *Blanks
252400000000     C                   Exsr      RieSkiera
252500000000     C                   EndIf
252600000000     C                   EndIf
252700000000     C*
252800000000     C* 17) Conferma importo
252900000000     C                   If        IMPORT05FN     > 999999
253000000000     C                   Z-Add     Prog_FN       Conta_Rigo
253100000000     C                   Z-Add     17            Conta_Colo
253200000000     C                   Clear                   Campo_Valore
253300000000     C                   Move      '1'           Campo_Valore
253400000000     C                   Exsr      Ablank
253500000000     C                   if        Campo_Valore <> *Blanks
253600000000     C                   Exsr      RieSkiera
253700000000     C                   EndIf
253800000000     C                   EndIf
253900000000     C*
254000000000     C                   IF        Prog_FN       = 3
254100000000     C                   Z-Add     0             Prog_FN
254200000000     C                   Add       1             ProgMod_FN
254300000000     C                   Z-Add     ProgMod_FN    WrkProgMod
254400000000     C                   EXSR      WrtDati
254500000000     C                   EXSR      InzSkiera
254600000000     C                   EndIf
254700000000     C*
254800150420R178 C                   EndIf
254900150420R178 C*
255000000000     C                   EndSR
255100000000     C******************************************************
255200000000     C* Tipo Record SE
255300000000     C******************************************************
255400000000     C     GesRec_SE     BegSr                                                  *
255500000000     C*
255600150420R178 C                   Eval      WrkImport01 = IMPORT05SE
255700150420 "   C                   Eval      WrkImport02 = IVA05SE
255800150420 "   C                   If        WrkImport01 <> 0    or
255900150420R178 C                             WrkImport02 <> 0
256000000000     C                   Add       1             Prog_SE
256100000000     C*
256200000000     C*-------------------------------------------
256300000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
256400000000     C*
256500000000     C* 1) Cognome
256600000000     C                   If        COGRAG05SE    <> *blanks
256700000000     C                   Z-Add     Prog_SE       Conta_Rigo
256800000000     C                   Z-Add     1             Conta_Colo
256900000000     C                   Clear                   Campo_Valore
257000000000     C                   MoveL     COGRAG05SE    Campo_Valore
257100000000     C                   MoveL     COGRAG05SE    C60
257200000000     C                   if        Campo_Valore <> *Blanks
257300000000     C                   Exsr      RieSkieraAlf
257400000000     C                   EndIf
257500000000     C                   EndIf
257600000000     C*
257700000000     C* 2) Nome
257800000000     C                   If        NOME05SE      <> *Blanks
257900000000     C                   Z-Add     Prog_SE       Conta_Rigo
258000000000     C                   Z-Add     2             Conta_Colo
258100000000     C                   Clear                   Campo_Valore
258200000000     C                   MoveL     NOME05SE      Campo_Valore
258300000000     C                   MoveL     NOME05SE      C60
258400000000     C                   if        Campo_Valore <> *Blanks
258500000000     C                   Exsr      RieSkieraAlf
258600000000     C                   EndIf
258700000000     C                   EndIf
258800000000     C*
258900000000     C* 3) Data di nascita
259000000000     C                   If        DTNASC05SE    <> *Loval
259100000000     C                   Move      DTNASC05SE    DtAMG
259200000000     C                   Move      AMGAnno       GMAAnno
259300000000     C                   Move      AMGMese       GMAMese
259400000000     C                   Move      AMGGiorno     GMAGiorno
259500000000     C                   Z-Add     Prog_SE       Conta_Rigo
259600000000     C                   Z-Add     3             Conta_Colo
259700000000     C                   Clear                   Campo_Valore
259800000000     C                   Move      DtGMA         Campo_Valore
259900000000     C                   if        Campo_Valore <> *Blanks
260000000000     C                   Exsr      RieSkiera
260100000000     C                   EndIf
260200000000     C                   EndIf
260300000000     C*
260400000000     C* 4) Comune o Stato estero di nascita
260500000000     C                   If        LOCNAS05SE    <> *Blanks
260600000000     C                   Z-Add     Prog_SE       Conta_Rigo
260700000000     C                   Z-Add     4             Conta_Colo
260800000000     C                   Clear                   Campo_Valore
260900000000     C                   MoveL     LOCNAS05SE    Campo_Valore
261000000000     C                   if        Campo_Valore <> *Blanks
261100000000     C                   Exsr      RieSkiera
261200000000     C                   EndIf
261300000000     C                   EndIf
261400000000     C*
261500000000     C* 5) Provincia
261600000000     C                   If        PRVNAS05SE    <> *Blanks
261700000000     C                   Z-Add     Prog_SE       Conta_Rigo
261800000000     C                   Z-Add     5             Conta_Colo
261900000000     C                   Clear                   Campo_Valore
262000000000     C                   MoveL     PRVNAS05SE    Campo_Valore
262100000000     C                   If        COGRAG05SE <> *blanks  and
262200000000     C                             UICSocieta <> UIC105SE
262300000000     C                   Clear                   Campo_Valore
262400000000     C                   MoveL     'EE'          Campo_Valore
262500000000     C                   EndIf
262600000000     C                   if        Campo_Valore <> *Blanks
262700000000     C                   Exsr      RieSkiera
262800000000     C                   EndIf
262900000000     C                   EndIf
263000000000     C*
263100000000     C* 6) Codice stato estero
263200000000     C                   If        UIC105SE      <> 0
263300000000     C                   Z-Add     Prog_SE       Conta_Rigo
263400000000     C                   Z-Add     6             Conta_Colo
263500000000     C                   Clear                   Campo_Valore
263600000000     C                   Move      UIC105SE      Campo_Valore
263700000000     C                   if        Campo_Valore <> *Blanks
263800000000     C                   Exsr      RieSkiera
263900000000     C                   EndIf
264000000000     C                   EndIf
264100000000     C*
264200000000     C* 7) Denominazione / ragione sociale
264300000000     C                   If        RAGSOC05SE    <> *blanks
264400000000     C                   Z-Add     Prog_SE       Conta_Rigo
264500000000     C                   Z-Add     7             Conta_Colo
264600000000     C                   Clear                   Campo_Valore
264700000000     C                   MoveL     RAGSOC05SE    Campo_Valore
264800000000     C                   MoveL     RAGSOC05SE    C60
264900000000     C                   if        Campo_Valore <> *Blanks
265000000000     C                   Exsr      RieSkieraAlf
265100000000     C                   EndIf
265200000000     C                   EndIf
265300000000     C*
265400000000     C* 8) Citta sede legale
265500000000     C                   If        CITTAE05SE    <> *blanks
265600000000     C                   Z-Add     Prog_SE       Conta_Rigo
265700000000     C                   Z-Add     8             Conta_Colo
265800000000     C                   Clear                   Campo_Valore
265900000000     C                   MoveL     CITTAE05SE    Campo_Valore
266000000000     C                   MoveL     CITTAE05SE    C60
266100000000     C                   if        Campo_Valore <> *Blanks
266200000000     C                   Exsr      RieSkieraAlf
266300000000     C                   EndIf
266400000000     C                   EndIf
266500000000     C*
266600000000     C* 9) Codice stato estero
266700000000     C                   If        UIC205SE      <> 0
266800000000     C                   Z-Add     Prog_SE       Conta_Rigo
266900000000     C                   Z-Add     9             Conta_Colo
267000000000     C                   Clear                   Campo_Valore
267100000000     C                   Move      UIC205SE      Campo_Valore
267200000000     C                   if        Campo_Valore <> *Blanks
267300000000     C                   Exsr      RieSkiera
267400000000     C                   EndIf
267500000000     C                   EndIf
267600000000     C*
267700000000     C* 10) Indirizzo estero sede legale
267800000000     C                   If        INDIRE05SE    <> *blanks
267900000000     C                   Z-Add     Prog_SE       Conta_Rigo
268000000000     C                   Z-Add     10            Conta_Colo
268100000000     C                   Clear                   Campo_Valore
268200000000     C                   MoveL     INDIRE05SE    Campo_Valore
268300000000     C                   MoveL     INDIRE05SE    C60
268400000000     C                   if        Campo_Valore <> *Blanks
268500000000     C                   Exsr      RieSkieraAlf
268600000000     C                   EndIf
268700000000     C                   EndIf
268800000000     C*
268900140121     C* 11) Codice identificativo IVA (solo se San Marino)
269000140121R145 C                   If        UIC205SE      =  037
269100150421R178 C                             or
269200150421R178 C                             UIC105SE      =  037
269300140121     C                   If        CIVA05SE      <> *Loval
269400000000     C                   Z-Add     Prog_SE       Conta_Rigo
269500000000     C                   Z-Add     11            Conta_Colo
269600000000     C                   Clear                   Campo_Valore
269700000000     C                   MoveL     CIVA05SE      Campo_Valore
269800000000     C                   if        Campo_Valore <> *Blanks
269900000000     C                   Exsr      RieSkiera
270000000000     C                   EndIf
270100000000     C                   EndIf
270200140121R145 C                   EndIf
270300000000     C*
270400000000     C* 12) Data emissione del documento fattura
270500000000     C                   If        DTDOC05SE     <> *Loval
270600000000     C                   Move      DTDOC05SE     DtAMG
270700000000     C                   Move      AMGAnno       GMAAnno
270800000000     C                   Move      AMGMese       GMAMese
270900000000     C                   Move      AMGGiorno     GMAGiorno
271000000000     C                   Z-Add     Prog_SE       Conta_Rigo
271100000000     C                   Z-Add     12            Conta_Colo
271200000000     C                   Clear                   Campo_Valore
271300000000     C                   Move      DtGMA         Campo_Valore
271400000000     C                   if        Campo_Valore <> *Blanks
271500000000     C                   Exsr      RieSkiera
271600000000     C                   EndIf
271700000000     C                   EndIf
271800000000     C*
271900000000     C* 13) Data di registrazione fattura
272000000000     C                   If        DTREG05SE     <> *Loval
272100000000     C                   Move      DTREG05SE     DtAMG
272200000000     C                   Move      AMGAnno       GMAAnno
272300000000     C                   Move      AMGMese       GMAMese
272400000000     C                   Move      AMGGiorno     GMAGiorno
272500000000     C                   Z-Add     Prog_SE       Conta_Rigo
272600000000     C                   Z-Add     13            Conta_Colo
272700000000     C                   Clear                   Campo_Valore
272800000000     C                   Move      DtGMA         Campo_Valore
272900000000     C                   if        Campo_Valore <> *Blanks
273000000000     C                   Exsr      RieSkiera
273100000000     C                   EndIf
273200000000     C                   EndIf
273300000000     C*
273400000000     C* 14) Nr.Fattura
273500000000     C                   If        NRDOC05SE     <> 0
273600000000     C                   Z-Add     Prog_SE       Conta_Rigo
273700000000     C                   Z-Add     14            Conta_Colo
273800000000     C                   Clear                   Campo_Valore
273900000000     C                   MoveL     NRDOC05SE     Campo_Valore
274000000000     C****               Exsr      Ablank
274100000000     C                   if        Campo_Valore <> *Blanks
274200000000     C                   Exsr      RieSkiera
274300000000     C                   EndIf
274400000000     C                   EndIf
274500000000     C*
274600000000     C* 15) Importo
274700000000     C                   If        IMPORT05SE    <> 0
274800000000     C                   Z-Add     Prog_SE       Conta_Rigo
274900000000     C                   Z-Add     15            Conta_Colo
275000000000     C                   Clear                   Campo_Valore
275100000000     C                   Z-Add     IMPORT05SE    Importo16_0
275200000000     C                   If        IMPORT05SE   <   0
275300000000     C                   Eval      Importo16_0  = Importo16_0 * -1
275400000000     C                   EndIf
275500000000     C                   Exsr      Ablank2
275600000000     C                   if        Campo_Valore <> *Blanks
275700000000     C                   Exsr      RieSkiera
275800000000     C                   EndIf
275900000000     C                   EndIf
276000000000     C*
276100000000     C* 16) Imposta
276200000000     C                   If        IVA05SE       <> 0
276300000000     C                   Z-Add     Prog_SE       Conta_Rigo
276400000000     C                   Z-Add     16            Conta_Colo
276500000000     C                   Clear                   Campo_Valore
276600000000     C                   Z-Add     IVA05SE       Importo16_0
276700000000     C                   If        IVA05SE      <   0
276800000000     C                   Eval      Importo16_0  = Importo16_0 * -1
276900000000     C                   EndIf
277000000000     C                   Exsr      Ablank2
277100000000     C                   if        Campo_Valore <> *Blanks
277200000000     C                   Exsr      RieSkiera
277300000000     C                   EndIf
277400000000     C                   EndIf
277500000000     C*
277600000000     C* 17) Conferma importo
277700000000     C                   If        IMPORT05SE     > 999999
277800000000     C                   Z-Add     Prog_SE       Conta_Rigo
277900000000     C                   Z-Add     17            Conta_Colo
278000000000     C                   Clear                   Campo_Valore
278100000000     C                   Move      '1'           Campo_Valore
278200000000     C                   Exsr      Ablank
278300000000     C                   if        Campo_Valore <> *Blanks
278400000000     C                   Exsr      RieSkiera
278500000000     C                   EndIf
278600000000     C                   EndIf
278700000000     C*
278800000000     C                   IF        Prog_SE       = 3
278900000000     C                   Z-Add     0             Prog_SE
279000000000     C                   Add       1             ProgMod_SE
279100000000     C                   Z-Add     ProgMod_SE    WrkProgMod
279200000000     C                   EXSR      WrtDati
279300000000     C                   EXSR      InzSkiera
279400000000     C                   EndIf
279500000000     C*
279600150420R178 C                   EndIf
279700150420R178 C*
279800000000     C                   EndSR
279900000000R136 C******************************************************
280000000000     C* Tipo Record TU
280100000000R136 C******************************************************
280200000000     C     GesRec_TU     BegSr                                                  *
280300000000     C*
280400150420R178 C                   Eval      WrkImport01 = IMPORT05TU
280500150420 "   C                   Eval      WrkImport02 = IVA05TU
280600150420 "   C                   If        WrkImport01 <> 0    or
280700150420R178 C                             WrkImport02 <> 0
280800000000     C                   Add       1             Prog_TU
280900000000     C*
281000000000     C*-------------------------------------------
281100000000     C* DATI IDENTIFICATIVI DEL CLIENTE/FORNITORE
281200000000     C*
281300000000     C* 1) Cognome
281400000000     C                   If        COGRAG05TU    <> *blanks
281500000000     C                   Z-Add     Prog_TU       Conta_Rigo
281600000000     C                   Z-Add     1             Conta_Colo
281700000000     C                   Clear                   Campo_Valore
281800000000     C                   MoveL     COGRAG05TU    Campo_Valore
281900000000     C                   MoveL     COGRAG05TU    C60
282000000000     C                   if        Campo_Valore <> *Blanks
282100000000     C                   Exsr      RieSkieraAlf
282200000000     C                   EndIf
282300000000     C                   EndIf
282400000000     C*
282500000000     C* 2) Nome
282600000000     C                   If        NOME05TU      <> *Blanks
282700000000     C                   Z-Add     Prog_TU       Conta_Rigo
282800000000     C                   Z-Add     2             Conta_Colo
282900000000     C                   Clear                   Campo_Valore
283000000000     C                   MoveL     NOME05TU      Campo_Valore
283100000000     C                   MoveL     NOME05TU      C60
283200000000     C                   if        Campo_Valore <> *Blanks
283300000000     C                   Exsr      RieSkieraAlf
283400000000     C                   EndIf
283500000000     C                   EndIf
283600000000     C*
283700000000     C* 3) Data di nascita
283800000000     C                   If        DTNASC05TU    <> *Loval
283900000000     C                   Move      DTNASC05TU    DtAMG
284000000000     C                   Move      AMGAnno       GMAAnno
284100000000     C                   Move      AMGMese       GMAMese
284200000000     C                   Move      AMGGiorno     GMAGiorno
284300000000     C                   Z-Add     Prog_TU       Conta_Rigo
284400000000     C                   Z-Add     3             Conta_Colo
284500000000     C                   Clear                   Campo_Valore
284600000000     C                   Move      DtGMA         Campo_Valore
284700000000     C                   if        Campo_Valore <> *Blanks
284800000000     C                   Exsr      RieSkiera
284900000000     C                   EndIf
285000000000     C                   EndIf
285100000000     C*
285200000000     C* 4) Comune o Stato estero di nascita
285300000000     C                   If        LOCNAS05TU    <> *Blanks
285400000000     C                   Z-Add     Prog_TU       Conta_Rigo
285500000000     C                   Z-Add     4             Conta_Colo
285600000000     C                   Clear                   Campo_Valore
285700000000     C                   MoveL     LOCNAS05TU    Campo_Valore
285800000000     C                   MoveL     LOCNAS05TU    C60
285900000000     C                   if        Campo_Valore <> *Blanks
286000000000     C                   Exsr      RieSkieraAlf
286100000000     C                   EndIf
286200000000     C                   EndIf
286300000000     C*
286400000000     C* 5) Provincia
286500000000     C                   If        PRVNAS05TU    <> *Blanks
286600000000     C                   Z-Add     Prog_TU       Conta_Rigo
286700000000     C                   Z-Add     5             Conta_Colo
286800000000     C                   Clear                   Campo_Valore
286900000000     C                   MoveL     PRVNAS05TU    Campo_Valore
287000000000     C                   If        UICSocieta <> UIC05TU
287100000000     C                   Clear                   Campo_Valore
287200000000     C                   MoveL     'EE'          Campo_Valore
287300000000     C                   EndIf
287400000000     C                   if        Campo_Valore <> *Blanks
287500000000     C                   Exsr      RieSkiera
287600000000     C                   EndIf
287700000000     C                   EndIf
287800000000     C*
287900000000     C* 6) Citta residenza
288000000000     C                   If        CITTAE05TU    <> *blanks
288100000000     C                   Z-Add     Prog_TU       Conta_Rigo
288200000000     C                   Z-Add     6             Conta_Colo
288300000000     C                   Clear                   Campo_Valore
288400000000     C                   MoveL     CITTAE05TU    Campo_Valore
288500000000     C                   MoveL     CITTAE05TU    C60
288600000000     C                   if        Campo_Valore <> *Blanks
288700000000     C                   Exsr      RieSkieraAlf
288800000000     C                   EndIf
288900000000     C                   EndIf
289000000000     C*
289100000000     C* 7) Codice stato estero
289200000000     C                   If        UIC05TU       <> 0
289300000000     C                   Z-Add     Prog_TU       Conta_Rigo
289400000000     C                   Z-Add     7             Conta_Colo
289500000000     C                   Clear                   Campo_Valore
289600000000     C                   Move      UIC05TU       Campo_Valore
289700000000     C                   if        Campo_Valore <> *Blanks
289800000000     C                   Exsr      RieSkiera
289900000000     C                   EndIf
290000000000     C                   EndIf
290100000000     C*
290200000000     C*
290300000000     C* 8) Indirizzo estero residenza
290400000000     C                   If        INDIRE05TU    <> *blanks
290500000000     C                   Z-Add     Prog_TU       Conta_Rigo
290600000000     C                   Z-Add     8             Conta_Colo
290700000000     C                   Clear                   Campo_Valore
290800000000     C                   MoveL     INDIRE05TU    Campo_Valore
290900000000     C                   MoveL     INDIRE05TU    C60
291000000000     C                   if        Campo_Valore <> *Blanks
291100000000     C                   Exsr      RieSkieraAlf
291200000000     C                   EndIf
291300000000     C                   EndIf
291400000000     C*
291500000000     C* 9) Data emissione del documento fattura
291600000000     C                   If        DTDOC05TU     <> *Loval
291700000000     C                   Move      DTDOC05TU     DtAMG
291800000000     C                   Move      AMGAnno       GMAAnno
291900000000     C                   Move      AMGMese       GMAMese
292000000000     C                   Move      AMGGiorno     GMAGiorno
292100000000     C                   Z-Add     Prog_TU       Conta_Rigo
292200000000     C                   Z-Add     9             Conta_Colo
292300000000     C                   Clear                   Campo_Valore
292400000000     C                   Move      DtGMA         Campo_Valore
292500000000     C                   if        Campo_Valore <> *Blanks
292600000000     C                   Exsr      RieSkiera
292700000000     C                   EndIf
292800000000     C                   EndIf
292900000000     C*
293000000000     C* 10) Data di registrazione fattura
293100000000     C                   If        DTREG05TU     <> *Loval
293200000000     C                   Move      DTREG05TU     DtAMG
293300000000     C                   Move      AMGAnno       GMAAnno
293400000000     C                   Move      AMGMese       GMAMese
293500000000     C                   Move      AMGGiorno     GMAGiorno
293600000000     C                   Z-Add     Prog_TU       Conta_Rigo
293700000000     C                   Z-Add     10            Conta_Colo
293800000000     C                   Clear                   Campo_Valore
293900000000     C                   Move      DtGMA         Campo_Valore
294000000000     C                   if        Campo_Valore <> *Blanks
294100000000     C                   Exsr      RieSkiera
294200000000     C                   EndIf
294300000000     C                   EndIf
294400000000     C*
294500000000     C* 11) Nr.Fattura
294600000000     C                   If        NRDOC05TU     <> 0
294700000000     C                   Z-Add     Prog_TU       Conta_Rigo
294800000000     C                   Z-Add     11            Conta_Colo
294900000000     C                   Clear                   Campo_Valore
295000000000     C                   MoveL     NRDOC05TU     Campo_Valore
295100000000     C****               Exsr      Ablank
295200000000     C                   if        Campo_Valore <> *Blanks
295300000000     C                   Exsr      RieSkiera
295400000000     C                   EndIf
295500000000     C                   EndIf
295600000000     C*
295700000000     C* 12) Importo
295800000000     C                   If        IMPORT05TU    <> 0
295900000000     C                   If        IMPORT05TU    <  0
296000000000     C                   Eval      IMPORT05TU  = IMPORT05TU * -1
296100000000     C                   EndIf
296200000000     C                   Z-Add     Prog_TU       Conta_Rigo
296300000000     C                   Z-Add     12            Conta_Colo
296400000000     C                   Clear                   Campo_Valore
296500000000     C                   Z-Add     IMPORT05TU    Importo16_0
296600000000     C                   Exsr      Ablank2
296700000000     C                   if        Campo_Valore <> *Blanks
296800000000     C                   Exsr      RieSkiera
296900000000     C                   EndIf
297000000000     C                   EndIf
297100000000     C*
297200000000     C* 13) Imposta
297300000000     C                   If        IVA05TU       <> 0
297400000000     C                   If        IVA05TU       <  0
297500000000     C                   Eval      IVA05TU     = IVA05TU    * -1
297600000000     C                   EndIf
297700000000     C                   Z-Add     Prog_TU       Conta_Rigo
297800000000     C                   Z-Add     13            Conta_Colo
297900000000     C                   Clear                   Campo_Valore
298000000000     C                   Z-Add     IVA05TU       Importo16_0
298100000000     C                   Exsr      Ablank2
298200000000     C                   if        Campo_Valore <> *Blanks
298300000000     C                   Exsr      RieSkiera
298400000000     C                   EndIf
298500000000     C                   EndIf
298600000000     C*
298700000000     C* 14) Conferma importo
298800000000     C                   If        IMPORT05TU     > 999999
298900000000     C                   Z-Add     Prog_TU       Conta_Rigo
299000000000     C                   Z-Add     14            Conta_Colo
299100000000     C                   Clear                   Campo_Valore
299200000000     C                   Move      '1'           Campo_Valore
299300000000     C                   Exsr      Ablank
299400000000     C                   if        Campo_Valore <> *Blanks
299500000000     C                   Exsr      RieSkiera
299600000000     C                   EndIf
299700000000     C                   EndIf
299800000000     C*
299900000000     C                   IF        Prog_TU       = 3
300000000000     C                   Z-Add     0             Prog_TU
300100000000     C                   Add       1             ProgMod_TU
300200000000     C                   Z-Add     ProgMod_TU    WrkProgMod
300300000000     C                   EXSR      WrtDati
300400000000     C                   EXSR      InzSkiera
300500000000     C                   EndIf
300600000000     C*
300700150420R178 C                   EndIf
300800150420R178 C*
300900000000R136 C                   EndSR
301000000000     C******************************************************
301100000000     C* Tipo Record TA
301200000000     C******************************************************
301300000000     C     GesRec_TA     BegSr                                                  *
301400000000     C*
301500000000     C*  FA
301600150309R147 X****               If        NRCPFA05TA    <> 0
301700150312R177 X*ex R147           If        Numero_FA  <> 0
301800150312R177 C                   If        $NrQuaFA   <> 0
301900000000     C                   Z-Add     1             Conta_Rigo
302000000000     C                   Z-Add     1             Conta_Colo
302100000000     C                   Clear                   Campo_Valore
302200150309R147 C****               Eval      NRCPFA05TA  = NRCPFA05TA - Togli_Fa
302300150309R147 X****               Z-Add     NRCPFA05TA    Importo16_0
302400150312R147 X*ex R147           Z-Add     Numero_FA     Importo16_0
302500150312R177 C                   Z-Add     $NrQuaFA      Importo16_0
302600000000     C                   Exsr      Ablank2
302700000000     C                   if        Campo_Valore <> *Blanks
302800000000     C                   Exsr      RieSkiera
302900000000     C                   EndIf
303000000000     C                   EndIf
303100000000     C*
303200000000     C*  SA
303300150312R177 X****               If        NRCLSA05TA    <> 0
303400150312R177 C                   If        $NrQuaSA    <> 0
303500000000     C                   Z-Add     2             Conta_Rigo
303600000000     C                   Z-Add     1             Conta_Colo
303700000000     C                   Clear                   Campo_Valore
303800150312R177 X****               Z-Add     NRCLSA05TA    Importo16_0
303900150312R177 C                   Z-Add     $NrQuaSA      Importo16_0
304000000000     C                   Exsr      Ablank2
304100000000     C                   if        Campo_Valore <> *Blanks
304200000000     C                   Exsr      RieSkiera
304300000000     C                   EndIf
304400000000     C                   EndIf
304500000000     C*
304600000000     C*  BL
304700150312R177 C* Residenti in Paesi con fiscalità privilegiata
304800150312R177 X****               If        NRBLBL05TA    <> 0
304900150312R177 C                   If        $NrQuaBL1    <> 0
305000000000     C                   Z-Add     3             Conta_Rigo
305100000000     C                   Z-Add     1             Conta_Colo
305200000000     C                   Clear                   Campo_Valore
305300150312R177 X****               Z-Add     NRBLBL05TA    Importo16_0
305400150312R177 C                   Z-Add     $NrQuaBL1     Importo16_0
305500000000     C                   Exsr      Ablank2
305600000000     C                   if        Campo_Valore <> *Blanks
305700000000     C                   Exsr      RieSkiera
305800000000     C                   EndIf
305900000000     C                   EndIf
306000000000     C*
306100000000     C*  BL
306200150312R177 C* Residenti esteri
306300150309R147 X****               If        NRNRBL05TA    <> 0
306400150312R177 X*ex R147           If        Numero_BL     <> 0
306500150312R177 C                   If        $NrQuaBL2    <> 0
306600000000     C                   Z-Add     3             Conta_Rigo
306700000000     C                   Z-Add     2             Conta_Colo
306800000000     C                   Clear                   Campo_Valore
306900150309R147 C****               Eval      NRNRBL05TA  = NRNRBL05TA - Togli_BL
307000150309R147 X****               Z-Add     NRNRBL05TA    Importo16_0
307100150312R177 X*ex R147           Z-Add     Numero_BL     Importo16_0
307200150312R177 C                   Z-Add     $NrQuaBL2     Importo16_0
307300000000     C                   Exsr      Ablank2
307400000000     C                   if        Campo_Valore <> *Blanks
307500000000     C                   Exsr      RieSkiera
307600000000     C                   EndIf
307700000000     C                   EndIf
307800000000     C*
307900000000     C*  BL
308000150312R177 C* Acquisti di servizi da residenti esteri
308100150312R177 X****               If        NRSRBL05TA    <> 0
308200150312R177 C                   If        $NrQuaBL3    <> 0
308300000000     C                   Z-Add     3             Conta_Rigo
308400000000     C                   Z-Add     3             Conta_Colo
308500000000     C                   Clear                   Campo_Valore
308600150312R177 X****               Z-Add     NRSRBL05TA    Importo16_0
308700150312R177 C                   Z-Add     $NrQuaBL3     Importo16_0
308800000000     C                   Exsr      Ablank2
308900000000     C                   if        Campo_Valore <> *Blanks
309000000000     C                   Exsr      RieSkiera
309100000000     C                   EndIf
309200000000     C                   EndIf
309300000000     C*
309400000000     C*  FE  fatture emesse
309500150312R177 X****               If        NRFEFE05TA    <> 0
309600150312R177 C                   If        $NrQuaFE1    <> 0
309700000000     C                   Z-Add     4             Conta_Rigo
309800000000     C                   Z-Add     1             Conta_Colo
309900000000     C                   Clear                   Campo_Valore
310000150312R177 X****               Z-Add     NRFEFE05TA    Importo16_0
310100150312R177 C                   Z-Add     $NrQuaFE1     Importo16_0
310200000000     C                   Exsr      Ablank2
310300000000     C                   if        Campo_Valore <> *Blanks
310400000000     C                   Exsr      RieSkiera
310500000000     C                   EndIf
310600000000     C                   EndIf
310700000000     C*
310800000000     C*  FE  doc riepilogativi
310900150312R177 X****               If        NRRIFE05TA    <> 0
311000150312R177 C                   If        $NrQuaFE2    <> 0
311100000000     C                   Z-Add     4             Conta_Rigo
311200000000     C                   Z-Add     2             Conta_Colo
311300000000     C                   Clear                   Campo_Valore
311400150312R177 X****               Z-Add     NRRIFE05TA    Importo16_0
311500150312R177 C                   Z-Add     $NrQuaFE2     Importo16_0
311600000000     C                   Exsr      Ablank2
311700000000     C                   if        Campo_Valore <> *Blanks
311800000000     C                   Exsr      RieSkiera
311900000000     C                   EndIf
312000000000     C                   EndIf
312100000000     C*
312200000000     C*  FR  fatture ricevute
312300150312R177 X****               If        NRFRFR05TA    <> 0
312400150312R177 C                   If        $NrQuaFR1    <> 0
312500000000     C                   Z-Add     5             Conta_Rigo
312600000000     C                   Z-Add     1             Conta_Colo
312700000000     C                   Clear                   Campo_Valore
312800150312R177 X****               Z-Add     NRFRFR05TA    Importo16_0
312900150312R177 C                   Z-Add     $NrQuaFR1     Importo16_0
313000000000     C                   Exsr      Ablank2
313100000000     C                   if        Campo_Valore <> *Blanks
313200000000     C                   Exsr      RieSkiera
313300000000     C                   EndIf
313400000000     C                   EndIf
313500000000     C*
313600000000     C*  FR  doc riepilogativi
313700150312R177 X****               If        NRRIFR05TA    <> 0
313800150312R177 C                   If        $NrQuaFR2    <> 0
313900000000     C                   Z-Add     5             Conta_Rigo
314000000000     C                   Z-Add     2             Conta_Colo
314100000000     C                   Clear                   Campo_Valore
314200150312R177 X****               Z-Add     NRRIFR05TA    Importo16_0
314300150312R177 C                   Z-Add     $NrQuaFR2     Importo16_0
314400000000     C                   Exsr      Ablank2
314500000000     C                   if        Campo_Valore <> *Blanks
314600000000     C                   Exsr      RieSkiera
314700000000     C                   EndIf
314800000000     C                   EndIf
314900000000     C*
315000000000     C*  NE  note emesse
315100150312R177 X****               If        NRNONE05TA    <> 0
315200150312R177 C                   If        $NrQuaNE     <> 0
315300000000     C                   Z-Add     6             Conta_Rigo
315400000000     C                   Z-Add     1             Conta_Colo
315500000000     C                   Clear                   Campo_Valore
315600150312R177 X****               Z-Add     NRNONE05TA    Importo16_0
315700150312R177 C                   Z-Add     $NrQuaNE      Importo16_0
315800000000     C                   Exsr      Ablank2
315900000000     C                   if        Campo_Valore <> *Blanks
316000000000     C                   Exsr      RieSkiera
316100000000     C                   EndIf
316200000000     C                   EndIf
316300000000     C*
316400000000     C*  NE  note ricevute
316500150312R177 X****               If        NRNONR05TA    <> 0
316600150312R177 C                   If        $NrQuaNR     <> 0
316700000000     C                   Z-Add     7             Conta_Rigo
316800000000     C                   Z-Add     1             Conta_Colo
316900000000     C                   Clear                   Campo_Valore
317000150312R177 X****               Z-Add     NRNONR05TA    Importo16_0
317100150312R177 C                   Z-Add     $NrQuaNR      Importo16_0
317200000000     C                   Exsr      Ablank2
317300000000     C                   if        Campo_Valore <> *Blanks
317400000000     C                   Exsr      RieSkiera
317500000000     C                   EndIf
317600000000     C                   EndIf
317700000000     C*
317800000000     C*  DF  operazioni senza fattura
317900000000R135 X****               If        NRCLDF05TA    <> 0
318000150312R177 X*ex R135           If        NRRIDF05TA    <> 0
318100150312R177 C                   If        $NrQuaDF     <> 0
318200000000     C                   Z-Add     8             Conta_Rigo
318300000000     C                   Z-Add     1             Conta_Colo
318400000000     C                   Clear                   Campo_Valore
318500000000R135 X****               Z-Add     NRCLDF05TA    Importo16_0
318600150312R177 X*ex R135           Z-Add     NRRIDF05TA    Importo16_0
318700150312R177 C                   Z-Add     $NrQuaDF      Importo16_0
318800000000     C                   Exsr      Ablank2
318900000000     C                   if        Campo_Valore <> *Blanks
319000000000     C                   Exsr      RieSkiera
319100000000     C                   EndIf
319200000000     C                   EndIf
319300000000     C*
319400000000     C*  FN  operazioni con soggetti non residenti
319500150312R177 X****               If        NRRIFN05TA    <> 0
319600150312R177 C                   If        $NrQuaFN     <> 0
319700000000     C                   Z-Add     9             Conta_Rigo
319800000000     C                   Z-Add     1             Conta_Colo
319900000000     C                   Clear                   Campo_Valore
320000150312R177 X****               Z-Add     NRRIFN05TA    Importo16_0
320100150312R177 C                   Z-Add     $NrQuaFN      Importo16_0
320200000000     C                   Exsr      Ablank2
320300000000     C                   if        Campo_Valore <> *Blanks
320400000000     C                   Exsr      RieSkiera
320500000000     C                   EndIf
320600000000     C                   EndIf
320700000000     C*
320800000000     C*  SE  acquisti servizi da non residenti
320900150312R177 X****               If        NRRISE05TA    <> 0
321000150312R177 C                   If        $NrQuaSE     <> 0
321100000000     C                   Z-Add     10            Conta_Rigo
321200000000     C                   Z-Add     1             Conta_Colo
321300000000     C                   Clear                   Campo_Valore
321400150312R177 X***                Z-Add     NRRISE05TA    Importo16_0
321500150312R177 C                   Z-Add     $NrQuaSE      Importo16_0
321600000000     C                   Exsr      Ablank2
321700000000     C                   if        Campo_Valore <> *Blanks
321800000000     C                   Exsr      RieSkiera
321900000000     C                   EndIf
322000000000     C                   EndIf
322100000000     C*
322200000000     C*
322300000000     C*  TU  operazioni legate al turismo
322400150312R177 X****               If        NRCLTU05TA    <> 0
322500150312R177 C                   If        $NrQuaTU     <> 0
322600000000     C                   Z-Add     11            Conta_Rigo
322700000000     C                   Z-Add     1             Conta_Colo
322800000000     C                   Clear                   Campo_Valore
322900150312R177 X****               Z-Add     NRCLTU05TA    Importo16_0
323000150312R177 C                   Z-Add     $NrQuaTU      Importo16_0
323100000000     C                   Exsr      Ablank2
323200000000     C                   if        Campo_Valore <> *Blanks
323300000000     C                   Exsr      RieSkiera
323400000000     C                   EndIf
323500000000     C                   EndIf
323600000000     C*
323700000000     C                   EndSR
323800000000     C******************************************************
323900000000     C* Riempimento skiera per i dati non posizionali
324000000000     C******************************************************
324100000000     C     RieSkiera     BEGSR
324200000000     C*
324300000000     C                   Z-ADD     Conta_Rigo    Cod_Rigo
324400000000     C                   Z-ADD     Conta_Colo    Cod_Colo
324500000000     C*
324600000000     C                   ADD       1             $I
324700000000     C                   MoveL     Campo_Codice  Skiera($I)
324800000000     C                   MoveL     WSavQua       Skiera($I)
324900000000     C                   Move      Campo_Valore  Skiera($I)
325000000000     C                   Clear                   Campo_Valore
325100000000     C*
325200000000     C                   ENDSR                                                  *
325300000000      /EJECT
325400000000     C******************************************************
325500000000     C* Tratta campi alfanumerici
325600000000     C******************************************************
325700000000     C     RieSkieraAlf  BEGSR
325800000000     C*
325900000000     C                   Z-ADD     Conta_Rigo    Cod_Rigo
326000000000     C                   Z-ADD     Conta_Colo    Cod_Colo
326100000000     C*
326200000000     C                   Add       1             $I
326300000000     C                   MoveL     Campo_Codice  Skiera($I)
326400000000     C                   MoveL     WSavQua       Skiera($I)
326500000000     C                   Move      Campo_Valore  Skiera($I)
326600000000     C                   Clear                   Campo_Valore
326700000000     C*
326800000000     C                   Clear                   Campo
326900000000     C*
327000000000     C* Controllo se ci sono dati dopo la 17° posizione
327100000000     C                   Do        3             XY                1 0
327200000000     C*
327300000000     C                   If        C15(XY) <> *blANKS
327400000000     C                   Add       1             $I
327500000000     C                   MOVEL     '+'           C16              16
327600000000     C                   MOVE      C15(XY)       C16
327700000000     C                   MoveL     Campo_Codice  Skiera($I)
327800000000     C                   MoveL     WSavQua       Skiera($I)
327900000000     C                   Move      C16           Skiera($I)
328000000000     C                   EndIf
328100000000     C*
328200000000     C                   EndDo
328300000000     C*
328400000000     C                   Clear                   C60
328500000000     C*
328600000000     C                   ENDSR
328700000000     C******************************************************
328800000000     C* Toglie gli zeri non significativi al Campo_Valore
328900000000     C******************************************************
329000000000     C     ABlank        BEGSR
329100000000     C*
329200000000     C                   MoveA     Campo_Valore  Sk_Campo
329300000000     C*
329400000000     C                   DO        Max$X         $X
329500000000     C*
329600000000     C                   if        Sk_Campo($X) = ' '   or
329700000000     C                             Sk_Campo($X) = '0'
329800000000     C                   Move      ' '           Sk_Campo($X)
329900000000     C                   else
330000000000     C                   LEAVE
330100000000     C                   endif
330200000000     C*
330300000000     C                   ENDDO
330400000000     C*
330500000000     C                   MoveA     Sk_Campo      Campo_Valore
330600000000     C*
330700000000     C                   ENDSR                                                  *
330800000000      /EJECT
330900000000     C******************************************************
331000000000     C* Toglie gli zeri non significativi nelle caselle con
331100000000     C* importo dei campi posizionali
331200000000     C******************************************************
331300000000     C     ABlank2       BEGSR
331400000000     C*
331500000000     C                   Move      Importo16_0   Importo16A
331600000000     C                   MoveA     Importo16A    SK_Importo
331700000000     C*
331800000000     C                   DO        Max$Z         $Z
331900000000     C*
332000000000     C                   if        Sk_Importo($Z) = ' '   or
332100000000     C                             Sk_Importo($Z) = '0'
332200000000     C                   Move      ' '           Sk_Importo($Z)
332300000000     C                   else
332400000000     C                   If        $FlgNeg       = '1'
332500000000     C                   Eval      Sk_Importo($Z-1) = '-'
332600000000     C                   Eval      $FlgNeg       = '0'
332700000000     C                   endif
332800000000     C                   LEAVE
332900000000     C                   endif
333000000000     C*
333100000000     C                   ENDDO
333200000000     C*
333300000000     C                   MoveA     Sk_Importo    Campo_Valore
333400000000     C*
333500000000     C                   ENDSR                                                  *
333600000000      /EJECT
333700000000     C******************************************************
333800000000     C* Scrittura record tipo 1 = clienti / 2 = fornitori
333900000000     C******************************************************
334000000000     C     WrtDati       BEGSR
334100000000     C*
334200000000     C                   Clear                   SDGETW000
334300000000     C*
334400000000     C                   Move      XSCSOC        ETWSocieta
334500000000     C                   Move      WSavQua       ETWTRE
334600000000     C                   Add       1             WRKPRGMOD
334700000000     C                   Z-Add     WRKPRGMOD     ETWPRGMOD
334800000000     C                   Movea     Skiera        ETWDAT
334900160202R195 C*
335000160202 "   X*ex R140           If        FORMATOA08    =  'A'   and
335100160202R195 C                   If        $Formato      =  'A'   and
335200000000R140 C                             WSavQua       <> 'TU'
335300000000R140 C                   MoveL     'C'           ETWTRC
335400000000R140 C                   ElSe
335500000000R140 C                   MoveL     'D'           ETWTRC
335600000000R140 C                   EndIf
335700000000     C*
335800000000     C                   Add       1             $NrRecTot
335900131211Î    C                   WRITE     SDGETW000
336000000000     C                   FEOD      SDGETW00F
336100000000     C*
336200000000     C                   ENDSR
336300000000     C******************************************************
336400000000     C* Gestione singolo invio
336500000000     C******************************************************
336600000000     C     GesSingInv    BegSr                                                  *
336700000000     C*
336800150312R177 C                   EXSR      InzNrQuadri
336900000000     C                   Exsr      RepNomeF
337000000000     C                   Exsr      GesFilTel
337100000000     C*
337200000000     C                   OPEN      SDGETE00F
337300000000     C*
337400000000     C* Record A  testata della fornitura
337500000000     C                   Exsr      WrtTpRecA
337600000000     C*
337700000000     C* Record B  testata della fornitura
337800000000     C                   Exsr      WrtTpRecB
337900000000     C*
338000000000     C* Leggo SDGETW per scrivere SDGETW
338100000000a1   C                   DOW       $FineETW    = *Off
338200000000     C*
338300000000     C                   Exsr      LetturaETW
338400000000     C*
338500000000e1   C                   EndDo                                                  *
338600000000     C*
338700000000     C* Record E di coda della fornitura
338800000000     C                   Exsr      WrtTpRecE
338900000000     C*
339000000000     C* Record Z di coda della fornitura
339100000000     C                   Exsr      WrtTpRecZ
339200000000     C*
339300000000     C                   Exsr      IscrCatF
339400000000     C*
339500000000     C                   Close     SDGETE00F
339600000000     C*
339700000000     C                   EndSR                                                  *
339800000000     C******************************************************
339900000000     C* Gestione invio multiplo
340000000000     C******************************************************
340100000000     C     GesMultInv    BegSr                                                  *
340200000000     C*
340300000000     C*
340400000000a1   C                   DOW       $FineETW    = *Off
340500150309R147 C                             and
340600150309R147 C                             $ProgrInvio < $NrInvii
340700000000     C*
340800000000     C                   Add       1             $ProgrInvio
340900000000     C                   Z-Add     0             Cnt_Righe
341000150312R177 C*
341100150312 "   C                   Z-Add     0             Cnt_C
341200150312 "   C                   Z-Add     0             Cnt_D
341300150312R177 C                   EXSR      InzNrQuadri
341400000000     C*
341500000000     C                   Exsr      RepNomeF
341600000000     C                   Exsr      GesFilTel
341700000000     C*
341800000000     C                   OPEN      SDGETE00F
341900000000     C*
342000000000     C* Record A  testata della fornitura
342100000000     C                   Exsr      WrtTpRecA
342200000000     C*
342300000000     C* Record B  testata della fornitura
342400000000     C                   Exsr      WrtTpRecB
342500000000     C*
342600000000     C* seconda lettura per scrivere il multifile
342700000000a2   C                   DOW       $FineETW    = *Off  and
342800000000     C                             (Cnt_Righe    <  MaxRecG61 and
342900000000     C                              MaxRecG61   > 0            )
343000000000     C*
343100000000     C* Leggo SDGETW per scrivere SDGETE
343200000000a1   C                   If        TipInvG61   <> 2
343300000000     C*
343400000000     C                   Exsr      LetturaETW
343500000000     C*
343600000000e1   C                   EndIf                                                  *
343700000000     C*
343800000000e2   C                   EndDo                                                  *
343900000000     C*
344000000000     C* Record E di coda della fornitura
344100000000     C                   Exsr      WrtTpRecE
344200000000     C*
344300000000     C* Record Z di coda della fornitura
344400000000     C                   Exsr      WrtTpRecZ
344500000000     C*
344600000000     C                   Exsr      IscrCatF
344700000000     C*
344800150309     C                   If        StpLG61   <> *Off
344900150309R147 C                             and
345000150309 "   C                             $ProgrInvio < $NrInvii
345100150309R147 C* la stampa dell'ultimo file la faccio fare al SDGSG61C
345200000000     C                   Exsr      StampaFile
345300000000     C                   EndIf
345400000000     C*
345500000000     C                   Close     SDGETE00F
345600000000     C*
345700000000e1   C                   EndDo                                                  *
345800000000     C*
345900000000     C                   EndSR                                                  *
346000150312R177 C************************************************************
346100150312 "   C* Inizializzazione numeratori per conteggio quadri
346200150312 "   C************************************************************
346300150312R177 C     InzNrQuadri   BegSr
346400150312     C*
346500150312     C                   Eval      LettoRigo1  = *Off
346600150312     C                   Eval      LettoRigo2  = *Off
346700150312     C                   Eval      LettoRigo3  = *Off
346800150312     C                   Eval      LettoRigo4  = *Off
346900150312     C                   Eval      LettoRigo5  = *Off
347000150312     C                   Eval      LettoRigo6  = *Off
347100150312     C                   Eval      LettoRigo7  = *Off
347200150312     C                   Eval      LettoRigo8  = *Off
347300150312     C                   Eval      LettoRigo9  = *Off
347400150312     C                   Eval      LettoRigo10 = *Off
347500150312     C                   Eval      $NrQuaFA    = 0
347600150312     C                   Eval      $NrQuaSA    = 0
347700150312     C                   Eval      $NrQuaBL1    = 0
347800150312     C                   Eval      $NrQuaBL2    = 0
347900150312     C                   Eval      $NrQuaBL3    = 0
348000150312     C                   Eval      $NrQuaFE1   = 0
348100150312     C                   Eval      $NrQuaFE2   = 0
348200150312     C                   Eval      $NrQuaFR1   = 0
348300150312     C                   Eval      $NrQuaFR2   = 0
348400150312     C                   Eval      $NrQuaNE    = 0
348500150312     C                   Eval      $NrQuaNR    = 0
348600150312     C                   Eval      $NrQuaDF    = 0
348700150312     C                   Eval      $NrQuaFN    = 0
348800150312     C                   Eval      $NrQuaSE    = 0
348900150312     C                   Eval      $NrQuaTU     = 0
349000150312     C*
349100150312R177 C                   ENDSR
349200150312R177 C/EJECT
349300000000     C************************************************************
349400000000     C* Reperimento numero max di invii
349500000000     C************************************************************
349600000000     C     RepNrInvii    BegSr
349700000000     C*
349800000000     C                   if        $NrRecTot   > 0
349900000000     C                   Z-Add     $NrRecTot     WImpInp
350000000000     C                   CallP     X0201MultC(WImpInp: 1: MaxRecG61:
350100000000     C                                        '+': '0': 0: WImpOut)
350200000000     C                   Z-Add     WImpOut       $NrInvii
350300000000     C                   EndIf
350400000000     C*
350500000000     C                   EndSR                                                  *
350600000000     C******************************************************
350700000000     C* Riempimento dati per t.r. A
350800000000     C******************************************************
350900000000     C     WrtTpRecA     BegSr                                                  *
351000000000     C*
351100000000     C                   Clear                   SDGETE000
351200000000     C                   Clear                   SDGSG61DSA
351300000000     C*
351400000000     C*-----------------------------------
351500000000     C* DATI IDENTIFICATIVI DELLA FORNITURA
351600000000     C*
351700000000     C*-
351800000000     C* Tipo record
351900000000     C                   MoveL     'A'           G61ATR
352000000000     C*-
352100000000     C* Codice identificativo della fornitura
352200000000     C                   MoveL     'NSP00'       G61AFOR
352300000000     C*-
352400000000     C* Tipo fornitore
352500000000     C*      "01" Soggetti che inviano la propria
352600000000     C*      "10" Intermediari
352700000000     C                   MoveL     '01'          G61ATFO
352800000000     C                   If        $FisInt       <> *blanks
352900000000     C                   MoveL     '10'          G61ATFO
353000000000     C                   EndIF
353100000000     C*-
353200000000     C* Codice fiscale del contribuente
353300000000     C                   MoveL     XScCdF        G61ACFI
353400000000R139 C                   If        $FisInt       <> *blanks
353500000000R139 C                   Move      $FisInt       G61ACFI
353600000000R139 C                   EndIF
353700000000     C*-
353800000000     C* Progressivo invio
353900000000R139 C                   If        $MultiInvio    = *Off
354000000000R139 C                   Z-Add     0             G61APRGINV
354100000000R139 C                   ElSe
354200000000     C                   Z-Add     $ProgrInvio   G61APRGINV
354300000000R139 C                   EndIF
354400000000     C* Num.Tot.Invii
354500000000     C                   If        $MultiInvio    = *Off
354600000000R139 C****               Z-Add     1             G61ATOTINV
354700000000R139 C                   Z-Add     0             G61ATOTINV
354800000000     C                   ElSe
354900000000     C                   Z-Add     $NrInvii      G61ATOTINV
355000000000     C                   EndIF
355100000000     C*
355200000000     C*-----------------------------------
355300000000     C*-
355400000000     C* Carattere di controllo
355500000000     C                   MoveL     'A'           G61AK1
355600000000     C*-
355700000000     C* Elementi di fine riga
355800000000     C* Impostare i valori esadecimali '0D' e '0A'
355900000000     C* (caratteri ASCI 'CR' ed 'LF')
356000000000     C*
356100000000     C                   Eval      G61AK2  = *blanks
356200000000     C                   Eval      G61AK2  = X'0D25'
356300000000     C*
356400000000     C                   MoveL     SDGSG61DSA    ETEREC
356500000000     C                   WRITE     SDGETE000
356600000000     C                   FEOD      SDGETE00F
356700000000     C*
356800000000     C                   EndSR                                                  *
356900000000     C******************************************************
357000000000     C* Riempimento dati per t.r. B
357100000000     C******************************************************
357200000000     C     WrtTpRecB     BegSr                                                  *
357300000000     C*
357400000000     C                   Clear                   SDGETE000
357500000000     C                   Clear                   SDGSG61DSB
357600000000     C*
357700000000     C* Tipo record
357800000000     C                   MoveL     'B'           G61BTR
357900000000     C*-
358000000000     C* Codice fiscale del contribuente
358100000000     C                   MoveL     XScCdF        G61BCFI
358200000000     C* Progressivo modulo
358300000000     C                   Z-Add     1             G61BPRGMOD
358400000000     C*-
358500000000     C* Identificativo produttore software
358600160202R195 X****               MoveL     IDPRODA08     G61BIPS
358700160202R195 C                   MoveL     $IdProd       G61BIPS
358800000000     C*-
358900000000     C* Tipo Comunicazione
359000000000R139 C****               Clear                   G61BCORD
359100000000  "  C****               Clear                   G61BCSOS
359200000000  "  C****               Clear                   G61BCANN
359300000000  "  C                   Move      '0'           G61BCORD
359400000000  "  C                   Move      '0'           G61BCSOS
359500000000R139 C                   Move      '0'           G61BCANN
359600000000     C*-
359700000000     C                   Select
359800000000     C                   When      TIPINVG61     = 0
359900000000     C                   Move      '1'           G61BCORD
360000000000     C                   When      TIPINVG61     = 1
360100000000     C                   Move      '1'           G61BCSOS
360200000000     C                   When      TIPINVG61     = 2
360300000000     C                   Move      '1'           G61BCSOS
360400000000     C                   EndSl
360500000000     C*-
360600000000     C                   Z-Add     PROTOCG61     G61BPROT
360700000000     C                   Z-Add     PROTODG61     G61BPROTD
360800000000     C*-
360900000000R139 C                   Move      '0'           G61BAGG
361000000000R139 C                   Move      '0'           G61BANA
361100160202R195 X****               If        FORMATOA08    = 'A'
361200160202R195 C*
361300160204R195 C                   If        $Formato      = 'A'
361400000000     C                   Move      '1'           G61BAGG
361500000000     C                   ElSe
361600000000     C                   Move      '1'           G61BANA
361700000000     C                   EndIf
361800000000     C*-
361900000000R139 C                   Move      '0'           G61BQFA
362000000000 "   C                   Move      '0'           G61BQSA
362100000000 "   C                   Move      '0'           G61BQBL
362200000000 "   C                   Move      '0'           G61BQFE
362300000000 "   C                   Move      '0'           G61BQFR
362400000000 "   C                   Move      '0'           G61BQNE
362500000000 "   C                   Move      '0'           G61BQNR
362600000000 "   C                   Move      '0'           G61BQDF
362700000000 "   C                   Move      '0'           G61BQFN
362800000000 "   C                   Move      '0'           G61BQSE
362900000000R139 C                   Move      '0'           G61BQTU
363000000000     C                   If        CntFA         > 0
363100000000     C                   Move      '1'           G61BQFA
363200000000     C                   Endif
363300000000     C                   If        CntSA         > 0
363400000000     C                   Move      '1'           G61BQSA
363500000000     C                   EndIf
363600000000     C                   If        CntBL         > 0
363700000000     C                   Move      '1'           G61BQBL
363800000000     C                   EndIf
363900000000     C                   If        CntFE         > 0
364000000000     C                   Move      '1'           G61BQFE
364100000000     C                   EndIf
364200000000     C                   If        CntFR         > 0
364300000000     C                   Move      '1'           G61BQFR
364400000000     C                   EndIf
364500000000     C                   If        CntNE         > 0
364600000000     C                   Move      '1'           G61BQNE
364700000000     C                   EndIf
364800000000     C                   If        CntNR         > 0
364900000000     C                   Move      '1'           G61BQNR
365000000000     C                   EndIf
365100000000     C                   If        CntDF         > 0
365200000000     C                   Move      '1'           G61BQDF
365300000000     C                   EndIf
365400000000     C                   If        CntFN         > 0
365500000000     C                   Move      '1'           G61BQFN
365600000000     C                   EndIf
365700000000     C                   If        CntSE         > 0
365800000000     C                   Move      '1'           G61BQSE
365900000000     C                   EndIf
366000000000     C****               If        CntTU         > 0
366100000000     C****               Move      '1'           G61BQTU
366200000000     C****               EndIf
366300000000R136 C                   If        CntTU         > 0
366400000000R136 C                   Move      '1'           G61BQTU
366500000000R136 C                   EndIf
366600000000     C*
366700000000     C                   Move      '1'           G61BQTA
366800000000     C*
366900000000     C*-
367000000000     C* Partita IVA del contribuente
367100000000     C                   MoveL     XScIVA        G61BPIVA
367200000000     C*
367300000000     C* Codice attivita
367400000000     C                   MoveL     XScATT        G61BCAT
367500000000     C*
367600000000     C* Telefono
367700000000     C                   MoveL     *all'0'       G61BTEL
367800000000     C*
367900000000     C* Fax
368000000000     C                   Move      *all'0'       G61BFAX
368100000000     C*
368200000000     C* e-mail
368300000000     C                   MoveL     *blanks       G61BEMAIL
368400000000     C*
368500000000R138 C                   Move      *all'0'       G61BDTNASC
368600000000R138 C                   Move      *all'0'       G61BDTNAS1
368700000000R138 C                   Move      *all'0'       G61BDTINP
368800000000R138 C                   Move      *all'0'       G61BDTFIP
368900000000     C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
369000000000     C*
369100000000     C* reperisco la natura giuridica del contribuente
369200000000     C                   MoveL     XScNCR        WrkNatGiu
369300000000     C                   Exsr      RepNatGiu
369400000000     C*
369500000000     C                   Select
369600000000     C*-
369700000000     C*-----------------------
369800000000     C* PERSONA FISICA
369900000000     C* da impostare solo nel caso di persona fisica
370000000000     C                   When      TPNG16      = '1'
370100000000     C*
370200000000     C                   Exsr      RepPersFis
370300000000     C*-
370400000000     C* Cognome
370500000000     C                   MoveL     $CogRagSog    G61BCOGNO
370600000000     C*-
370700000000     C* Nome
370800000000     C                   MoveL     $NomeSog      G61BNOME
370900000000     C*-
371000000000     C* Sesso
371100000000     C                   If        $SexSog       ='1'
371200000000     C                   MoveL     'M'           G61BSESSO
371300000000     C                   Else
371400000000     C                   MoveL     'F'           G61BSESSO
371500000000     C                   EndIf
371600000000     C*-
371700000000     C* Data di nascita
371800000000     C                   If        $DtNascSog    <> *Loval
371900000000     C                   Move      $DtNascSog    DtAMG
372000000000     C                   Move      AMGAnno       GMAAnno
372100000000     C                   Move      AMGMese       GMAMese
372200000000     C                   Move      AMGGiorno     GMAGiorno
372300000000     C                   Move      DtGMA         G61BDTNASC
372400000000     C                   Else
372500000000     C                   Move      *zeros        G61BDTNASC
372600000000     C                   EndIf
372700000000     C*-
372800000000     C* Comune o Stato estero di nascita
372900000000     C                   MoveL     $LocNascSog   G61BLUONAS
373000000000     C*-
373100000000     C* Provincia di nascita
373200000000     C                   MoveL     $PrvNascSog   G61BPRVNAS
373300000000     C*-----------------------
373400000000     C* PERSONA GIURIDICA
373500000000     C* da impostare solo nel caso di persona non fisica
373600000000     C                   When      TPNG16      = '2'  or
373700000000     C                             TPNG16      = '3'
373800000000     C*-
373900000000     C* Denominazione
374000000000     C                   MoveL     XSCRGS        G61BDENOMI
374100131113R141 C*
374200131113R141 C                   EndSl
374300000000     C*-
374400000000     C* Anno
374500000000     C                   Move      AnnoG61       G61BANNO
374600000000     C*-
374700000000     C* Mese
374800000000     C                   Move      *blanks       G61BMESE
374900000000     C*
375000131113R141 X****               EndSl
375100000000     C*-
375200000000     C*-----------------------------------
375300000000     C* DATI SOGGETTO CHE SI IMPEGNA ALLA TRASMISSIONE
375400000000     C* (soggetto che effettua la comunicazione, se diverso dal soggetto
375500000000     C*  a cui si riferisce la comunicazione)
375600000000     C*
375700000000     C* Cod.Fiscale
375800000000     C                   Move      *blanks       G61BCDFIS1
375900000000     C* Se significativo
376000000000     C                   If        $FisSog       <> *blanks
376100000000     C* Cod.Fiscale
376200000000     C                   Move      $FisSog       G61BCDFIS1
376300000000     C* Cod.Carica
376400000000     C                   Move      $CarSog       G61BCARR1
376500000000     C* Data inizio procedura
376600000000     C                   If        $DtiSog       <> '0001-01-01'
376700000000     C                   Move      $DtiSog       DtAMG
376800000000     C                   Exsr      ValorizDTA
376900000000     C                   Eval      G61BDTINP    = WrkDt
377000000000     C                   EndIf
377100000000     C*-
377200000000     C* Data fine procedura
377300000000     C                   If        $DtfSog       <> '0001-01-01'
377400000000     C                   Move      $DtfSog       DtAMG
377500000000     C                   Exsr      ValorizDTA
377600000000     C                   Eval      G61BDTFIP    = WrkDt
377700000000     C                   EndIf
377800000000     C*-
377900000000     C* Persona Fisica
378000000000     c                   If        $COGSOG       <> *blanks
378100000000     C                   Eval      G61BCOGNO1   = $CogSOG
378200000000     C                   Eval      G61BNOME1    = $NomSOG
378300000000     C                   Eval      G61BSESSO1   = $SEXRSOG
378400000000     C                   If        $DtnSog       <> *blanks
378500000000     C                   Move      $DtnSog       DtAMG
378600000000     C                   Exsr      ValorizDTA
378700000000     C                   Eval      G61BDTNAS1   = WrkDt
378800000000     C                   EndIf
378900000000     C                   Eval      G61BLUONA1   = $COMSOG
379000000000     C                   Eval      G61BPRVNA1   = $PRVSOG
379100000000     C                   EndIf
379200000000     C*-
379300000000     C* Persona Giuridica
379400000000     c                   If        $RagSOG       <> *blanks
379500000000     C                   Eval      G61BDENOM1   = $RAGSOG
379600000000     C                   EndIf
379700000000     C*-
379800000000     C                   EndIf
379900000000     C*-----------------------------------
380000000000     C* DATI SOGGETTO CHE SI IMPEGNA ALLA TRASMISSIONE (Intermediario)
380100000000     C*
380200000000     C* Cod.Fisc.Intermediario
380300000000     C                   Move      *blanks       G61BCFINT
380400000000     C* Nr CAAF
380500000000     C                   Z-Add     0             G61BCAAF
380600000000     C* Impegno alla presentazione
380700000000     C                   Z-Add     0             G61BIMPE
380800000000     C* Data dell'Impegno alla presentazione
380900000000     C                   Move      *all'0'       G61BDTIMPE
381000000000     C*
381100000000     C* Se significativo
381200000000     C                   If        $FisInt       <> *blanks
381300000000     C* Cod.Fisc.Intermediario
381400000000     C                   Move      $FisInt       G61BCFINT
381500000000     C* Nr CAAF
381600000000     C                   Move      $CafInt       G61BCAAF
381700000000     C* Impegno alla presentazione
381800000000     C                   Move      $ImpTrs       G61BIMPE
381900000000     C* Data dell'Impegno alla presentazione
382000000000     C                   Move      $ImpDta       DtAMG
382100000000     C                   Exsr      ValorizDTA
382200000000     C                   Eval      G61BDTIMPE   = WrkDt
382300000000     C                   EndIf
382400000000     C*-----------------------------------
382500000000     C*-
382600000000     C* Carattere di controllo
382700000000     C                   MoveL     'A'           G61BK1
382800000000     C*-
382900000000     C* Elementi di fine riga
383000000000     C* Impostare i valori esadecimali '0D' e '0A'
383100000000     C* (caratteri ASCI 'CR' ed 'LF')
383200000000     C*
383300000000     C                   Eval      G61BK2 =  *blanks
383400000000     C                   Eval      G61BK2 =  X'0D25'
383500000000     C*
383600000000     C                   MoveL     SDGSG61DSB    ETEREC
383700000000     C                   WRITE     SDGETE000
383800000000     C                   FEOD      SDGETE00F
383900000000     C*
384000000000     C                   EndSR                                                  *
384100000000     C******************************************************
384200000000     C* Scrittura record t.r. C
384300000000     C******************************************************
384400000000     C     WrtTpRecC     BegSr
384500000000     C*
384600000000R140 C****               ADD       1             CNT_C
384700000000     C                   ADD       1             Cnt_Righe
384800000000     C*
384900000000     C                   Clear                   SDGSG61DSC
385000000000     C* Tipo Record
385100000000     C                   Eval      G61CTR       = 'C'
385200000000R140 C                   If        ETWTRE       = 'TU'
385300000000R140 C                   Eval      G61CTR       = 'D'
385400000000R140 C                   ADD       1             CNT_D
385500000000R140 C                   ElSe
385600000000R140 C                   ADD       1             CNT_C
385700000000R140 C                   EndIf
385800000000     C* Cod.Fiscale
385900000000     C                   Eval      G61CCFI      = XScCdF
386000000000     C*
386100000000     C* Progressivo modulo
386200000000     C                   Eval      G61CPRGMOD   = ETWPRGMOD
386300000000     C*
386400000000     C* Identificativo produttore software
386500160202R195 X****               MoveL     IDPRODA08     G61CIPS
386600160202R195 C                   MoveL     $IdProd       G61CIPS
386700000000     C*-
386800000000     C                   MoveL     ETWDAT        G61CDATI
386900150312R177 C*
387000150312 "   C* recupero i dati presenti nel record per contare i quadri
387100150312 "   C                   EXSR      ContaQuadri
387200150312R177 C
387300000000     C*-
387400000000     C* Carattere di controllo
387500000000     C                   MoveL     'A'           G61CK1
387600000000     C*-
387700000000     C* Elementi di fine riga
387800000000     C* Impostare i valori esadecimali '0D' e '0A'
387900000000     C* (caratteri ASCI 'CR' ed 'LF')
388000000000     C*
388100000000     C                   Eval      G61CK2  = *blanks
388200000000     C                   Eval      G61CK2  = X'0D25'
388300000000     C*
388400000000     C*----
388500000000     C                   Clear                   SDGETE000
388600000000     C                   MoveL     SDGSG61DSC    ETEREC
388700000000     C*----
388800000000     C                   WRITE     SDGETE000
388900000000     C                   FEOD      SDGETE00F
389000000000     C*
389100000000     C                   EndSR
389200150312R177 C******************************************************
389300150312 "   C* Conteggio quadri FA, BL e SA
389400150312 "   C******************************************************
389500150312R177 C     ContaQuadri   BEGSR
389600150312     C*
389700150312     C                   Eval      LettoRigo1  = *Off
389800150312     C                   Eval      LettoRigo2  = *Off
389900150312     C                   Eval      LettoRigo3  = *Off
390000150312     C                   Eval      LettoRigo4  = *Off
390100150312     C                   Eval      LettoRigo5  = *Off
390200150312     C                   Eval      LettoRigo6  = *Off
390300150312     C                   Eval      LettoRigo7  = *Off
390400150312     C                   Eval      LettoRigo8  = *Off
390500150312     C                   Eval      LettoRigo9  = *Off
390600150312     C                   Eval      LettoRigo10 = *Off
390700150312     C*
390800150312     C                   Movea     ETWDAT        Skiera
390900150312     C                   Eval      $I          = 1
391000150312     C*
391100150312     C                   DOW       $I         <= Max$I       and
391200150312     C                             Skiera($I) <> *Blank
391300150312     C*
391400150312     C                   MoveL     Skiera($I)    Campo_Codice
391500150312     C*
391600150312     C                   Select
391700150312     C*--------------
391800150312     C* FATTURE/NOTE NAZIONALI
391900150312     C                   When      Cod_Elenco  = 'FA'
392000150312     C*
392100150312     C                   Select
392200150312     C                   When      Cod_Rigo    = 1        and
392300150312     C                             LettoRigo1  = *Off
392400150312     C                   Eval      $NrQuaFA    = $NrQuaFA + 1
392500150312     C                   Eval      LettoRigo1  = *On
392600150312     C*
392700150312     C                   When      Cod_Rigo    = 2        and
392800150312     C                             LettoRigo2  = *Off
392900150312     C                   Eval      $NrQuaFA    = $NrQuaFA + 1
393000150312     C                   Eval      LettoRigo2  = *On
393100150312     C*
393200150312     C                   When      Cod_Rigo    = 3        and
393300150312     C                             LettoRigo3  = *Off
393400150312     C                   Eval      $NrQuaFA    = $NrQuaFA + 1
393500150312     C                   Eval      LettoRigo3  = *On
393600150312     C                   endSl
393700150312     C*
393800150312     C*--------------
393900150312     C* CORRISPETTIVI (FATTURE O SCONTRINI)
394000150312     C                   When      Cod_Elenco  = 'SA'
394100150312     C*
394200150312     C                   Select
394300150312     C                   When      Cod_Rigo    = 1        and
394400150312     C                             LettoRigo1  = *Off
394500150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
394600150312     C                   Eval      LettoRigo1  = *On
394700150312     C*
394800150312     C                   When      Cod_Rigo    = 2        and
394900150312     C                             LettoRigo2  = *Off
395000150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
395100150312     C                   Eval      LettoRigo2  = *On
395200150312     C*
395300150312     C                   When      Cod_Rigo    = 3        and
395400150312     C                             LettoRigo3  = *Off
395500150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
395600150312     C                   Eval      LettoRigo3  = *On
395700150312     C*
395800150312     C                   When      Cod_Rigo    = 4        and
395900150312     C                             LettoRigo4  = *Off
396000150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
396100150312     C                   Eval      LettoRigo4  = *On
396200150312     C*
396300150312     C                   When      Cod_Rigo    = 5        and
396400150312     C                             LettoRigo5  = *Off
396500150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
396600150312     C                   Eval      LettoRigo5  = *On
396700150312     C*
396800150312     C                   When      Cod_Rigo    = 6        and
396900150312     C                             LettoRigo6  = *Off
397000150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
397100150312     C                   Eval      LettoRigo6  = *On
397200150312     C*
397300150312     C                   When      Cod_Rigo    = 7        and
397400150312     C                             LettoRigo7  = *Off
397500150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
397600150312     C                   Eval      LettoRigo7  = *On
397700150312     C*
397800150312     C                   When      Cod_Rigo    = 8        and
397900150312     C                             LettoRigo8  = *Off
398000150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
398100150312     C                   Eval      LettoRigo8  = *On
398200150312     C*
398300150312     C                   When      Cod_Rigo    = 9        and
398400150312     C                             LettoRigo9  = *Off
398500150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
398600150312     C                   Eval      LettoRigo9  = *On
398700150312     C*
398800150312     C                   When      Cod_Rigo    = 10       and
398900150312     C                             LettoRigo10 = *Off
399000150312     C                   Eval      $NrQuaSA    = $NrQuaSA + 1
399100150312     C                   Eval      LettoRigo10 = *On
399200150312     C                   endSl
399300150312     C*
399400150312     C*--------------
399500150312     C* FATTURE ESTERE E/O BLACK LIST
399600150312     C                   When      Cod_Elenco  = 'BL'
399700150312     C*
399800150312     C                   Select
399900150312     C                   When      Cod_Rigo    = 2        and
400000150312     C                             LettoRigo2  = *Off
400100150312     C*
400200150312     C                   Select
400300150312     C*
400400150312     C* 2) Operazioni con paesi a fiscalità privilegiata
400500150312     C                   When      Cod_Colo    = 2
400600160303R195 X****               Eval      $NrQuaBL1   = 0
400700160303R195 C                   Eval      $NrQuaBL1   = $NrQuaBL1  + 1
400800150312     C                   Eval      LettoRigo2  = *On
400900150312     C*               /-------------
401000150312     C*
401100150312     C* 3) Operazioni con soggetti non residenti
401200150312     C                   When      Cod_Colo    = 3
401300150312     C                   Eval      $NrQuaBL2   = $NrQuaBL2  + 1
401400150312     C                   Eval      LettoRigo2  = *On
401500150312     C*
401600150312     C* 4) Acquisti di servizi da soggetti non residenti
401700150312     C                   When      Cod_Colo    = 4
401800150312     C                   Eval      $NrQuaBL3   = $NrQuaBL3  + 1
401900150312     C                   Eval      LettoRigo2  = *On
402000150312     C                   endSl
402100150312     C*
402200150312     C                   endSl
402300150312     C*
402400150312     C*--------------
402500150312     C* Fatture emesse e Documenti riepilogativi (operazioni attive)
402600150312     C                   When      Cod_Elenco  = 'FE'
402700150312     C*
402800150312     C                   Select
402900150312     C                   When      Cod_Rigo    = 1        and
403000150312     C                             LettoRigo1  = *Off
403100150312     C                   if        Cod_Colo    = 3
403200150312     C                   Eval      $NrQuaFE2   = $NrQuaFE2+ 1
403300150312     C                   else
403400150312     C                   Eval      $NrQuaFE1   = $NrQuaFE1+ 1
403500150312     C                   endif
403600150312     C                   Eval      LettoRigo1  = *On
403700150312     C*
403800150312     C                   When      Cod_Rigo    = 2        and
403900150312     C                             LettoRigo2  = *Off
404000150312     C                   if        Cod_Colo    = 3
404100150312     C                   Eval      $NrQuaFE2   = $NrQuaFE2+ 1
404200150312     C                   else
404300150312     C                   Eval      $NrQuaFE1   = $NrQuaFE1+ 1
404400150312     C                   endif
404500150312     C                   Eval      LettoRigo2  = *On
404600150312     C*
404700150312     C                   When      Cod_Rigo    = 3        and
404800150312     C                             LettoRigo3  = *Off
404900150312     C                   if        Cod_Colo    = 3
405000150312     C                   Eval      $NrQuaFE2   = $NrQuaFE2+ 1
405100150312     C                   else
405200150312     C                   Eval      $NrQuaFE1   = $NrQuaFE1+ 1
405300150312     C                   endif
405400150312     C                   Eval      LettoRigo3  = *On
405500150312     C*
405600150312     C                   When      Cod_Rigo    = 4        and
405700150312     C                             LettoRigo4  = *Off
405800150312     C                   if        Cod_Colo    = 3
405900150312     C                   Eval      $NrQuaFE2   = $NrQuaFE2+ 1
406000150312     C                   else
406100150312     C                   Eval      $NrQuaFE1   = $NrQuaFE1+ 1
406200150312     C                   endif
406300150312     C                   Eval      LettoRigo4  = *On
406400150312     C*
406500150312     C                   When      Cod_Rigo    = 5        and
406600150312     C                             LettoRigo5  = *Off
406700150312     C                   if        Cod_Colo    = 3
406800150312     C                   Eval      $NrQuaFE2   = $NrQuaFE2+ 1
406900150312     C                   else
407000150312     C                   Eval      $NrQuaFE1   = $NrQuaFE1+ 1
407100150312     C                   endif
407200150312     C                   Eval      LettoRigo5  = *On
407300150312     C*
407400150312     C                   When      Cod_Rigo    = 6        and
407500150312     C                             LettoRigo6  = *Off
407600150312     C                   if        Cod_Colo    = 3
407700150312     C                   Eval      $NrQuaFE2   = $NrQuaFE2+ 1
407800150312     C                   else
407900150312     C                   Eval      $NrQuaFE1   = $NrQuaFE1+ 1
408000150312     C                   endif
408100150312     C                   Eval      LettoRigo6  = *On
408200150312     C                   endSl
408300150312     C*
408400150312     C*--------------
408500150312     C* Fatture ricevute e Documenti riepilogativi (Operazioni passive)
408600150312     C                   When      Cod_Elenco  = 'FR'
408700150312     C*
408800150312     C                   Select
408900150312     C                   When      Cod_Rigo    = 1        and
409000150312     C                             LettoRigo1  = *Off
409100150312     C                   if        Cod_Colo    = 2
409200150312     C                   Eval      $NrQuaFR2   = $NrQuaFR2+ 1
409300150312     C                   else
409400150312     C                   Eval      $NrQuaFR1   = $NrQuaFR1+ 1
409500150312     C                   endif
409600150312     C                   Eval      LettoRigo1  = *On
409700150312     C*
409800150312     C                   When      Cod_Rigo    = 2        and
409900150312     C                             LettoRigo2  = *Off
410000150312     C                   if        Cod_Colo    = 2
410100150312     C                   Eval      $NrQuaFR2   = $NrQuaFR2+ 1
410200150312     C                   else
410300150312     C                   Eval      $NrQuaFR1   = $NrQuaFR1+ 1
410400150312     C                   endif
410500150312     C                   Eval      LettoRigo2  = *On
410600150312     C*
410700150312     C                   When      Cod_Rigo    = 3        and
410800150312     C                             LettoRigo3  = *Off
410900150312     C                   if        Cod_Colo    = 2
411000150312     C                   Eval      $NrQuaFR2   = $NrQuaFR2+ 1
411100150312     C                   else
411200150312     C                   Eval      $NrQuaFR1   = $NrQuaFR1+ 1
411300150312     C                   endif
411400150312     C                   Eval      LettoRigo3  = *On
411500150312     C*
411600150312     C                   When      Cod_Rigo    = 4        and
411700150312     C                             LettoRigo4  = *Off
411800150312     C                   if        Cod_Colo    = 2
411900150312     C                   Eval      $NrQuaFR2   = $NrQuaFR2+ 1
412000150312     C                   else
412100150312     C                   Eval      $NrQuaFR1   = $NrQuaFR1+ 1
412200150312     C                   endif
412300150312     C                   Eval      LettoRigo4  = *On
412400150312     C*
412500150312     C                   When      Cod_Rigo    = 5        and
412600150312     C                             LettoRigo5  = *Off
412700150312     C                   if        Cod_Colo    = 2
412800150312     C                   Eval      $NrQuaFR2   = $NrQuaFR2+ 1
412900150312     C                   else
413000150312     C                   Eval      $NrQuaFR1   = $NrQuaFR1+ 1
413100150312     C                   endif
413200150312     C                   Eval      LettoRigo5  = *On
413300150312     C*
413400150312     C                   When      Cod_Rigo    = 6        and
413500150312     C                             LettoRigo6  = *Off
413600150312     C                   if        Cod_Colo    = 2
413700150312     C                   Eval      $NrQuaFR2   = $NrQuaFR2+ 1
413800150312     C                   else
413900150312     C                   Eval      $NrQuaFR1   = $NrQuaFR1+ 1
414000150312     C                   endif
414100150312     C                   Eval      LettoRigo6  = *On
414200150312     C                   endSl
414300150312     C*
414400150312     C*--------------
414500150312     C* Note di variazione emesse
414600150312     C                   When      Cod_Elenco  = 'NE'
414700150312     C*
414800150312     C                   Select
414900150312     C                   When      Cod_Rigo    = 1        and
415000150312     C                             LettoRigo1  = *Off
415100150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
415200150312     C                   Eval      LettoRigo1  = *On
415300150312     C*
415400150312     C                   When      Cod_Rigo    = 2        and
415500150312     C                             LettoRigo2  = *Off
415600150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
415700150312     C                   Eval      LettoRigo2  = *On
415800150312     C*
415900150312     C                   When      Cod_Rigo    = 3        and
416000150312     C                             LettoRigo3  = *Off
416100150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
416200150312     C                   Eval      LettoRigo3  = *On
416300150312     C*
416400150312     C                   When      Cod_Rigo    = 4        and
416500150312     C                             LettoRigo4  = *Off
416600150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
416700150312     C                   Eval      LettoRigo4  = *On
416800150312     C*
416900150312     C                   When      Cod_Rigo    = 5        and
417000150312     C                             LettoRigo5  = *Off
417100150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
417200150312     C                   Eval      LettoRigo5  = *On
417300150312     C*
417400150312     C                   When      Cod_Rigo    = 6        and
417500150312     C                             LettoRigo6  = *Off
417600150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
417700150312     C                   Eval      LettoRigo6  = *On
417800150312     C*
417900150312     C                   When      Cod_Rigo    = 7        and
418000150312     C                             LettoRigo7  = *Off
418100150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
418200150312     C                   Eval      LettoRigo7  = *On
418300150312     C*
418400150312     C                   When      Cod_Rigo    = 8        and
418500150312     C                             LettoRigo8  = *Off
418600150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
418700150312     C                   Eval      LettoRigo8  = *On
418800150312     C*
418900150312     C                   When      Cod_Rigo    = 9        and
419000150312     C                             LettoRigo9  = *Off
419100150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
419200150312     C                   Eval      LettoRigo9  = *On
419300150312     C*
419400150312     C                   When      Cod_Rigo    = 10       and
419500150312     C                             LettoRigo10 = *Off
419600150312     C                   Eval      $NrQuaNE    = $NrQuaNE + 1
419700150312     C                   Eval      LettoRigo10 = *On
419800150312     C                   endSl
419900150312     C*
420000150312     C*--------------
420100150312     C* Note di variazioni ricevute
420200150312     C                   When      Cod_Elenco  = 'NR'
420300150312     C*
420400150312     C                   Select
420500150312     C                   When      Cod_Rigo    = 1        and
420600150312     C                             LettoRigo1  = *Off
420700150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
420800150312     C                   Eval      LettoRigo1  = *On
420900150312     C*
421000150312     C                   When      Cod_Rigo    = 2        and
421100150312     C                             LettoRigo2  = *Off
421200150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
421300150312     C                   Eval      LettoRigo2  = *On
421400150312     C*
421500150312     C                   When      Cod_Rigo    = 3        and
421600150312     C                             LettoRigo3  = *Off
421700150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
421800150312     C                   Eval      LettoRigo3  = *On
421900150312     C*
422000150312     C                   When      Cod_Rigo    = 4        and
422100150312     C                             LettoRigo4  = *Off
422200150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
422300150312     C                   Eval      LettoRigo4  = *On
422400150312     C*
422500150312     C                   When      Cod_Rigo    = 5        and
422600150312     C                             LettoRigo5  = *Off
422700150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
422800150312     C                   Eval      LettoRigo5  = *On
422900150312     C*
423000150312     C                   When      Cod_Rigo    = 6        and
423100150312     C                             LettoRigo6  = *Off
423200150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
423300150312     C                   Eval      LettoRigo6  = *On
423400150312     C*
423500150312     C                   When      Cod_Rigo    = 7        and
423600150312     C                             LettoRigo7  = *Off
423700150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
423800150312     C                   Eval      LettoRigo7  = *On
423900150312     C*
424000150312     C                   When      Cod_Rigo    = 8        and
424100150312     C                             LettoRigo8  = *Off
424200150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
424300150312     C                   Eval      LettoRigo8  = *On
424400150312     C*
424500150312     C                   When      Cod_Rigo    = 9        and
424600150312     C                             LettoRigo9  = *Off
424700150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
424800150312     C                   Eval      LettoRigo9  = *On
424900150312     C*
425000150312     C                   When      Cod_Rigo    = 10       and
425100150312     C                             LettoRigo10 = *Off
425200150312     C                   Eval      $NrQuaNR    = $NrQuaNR + 1
425300150312     C                   Eval      LettoRigo10 = *On
425400150312     C                   endSl
425500150312     C*
425600150312     C*--------------
425700150312     C* Operazioni senza fattura
425800150312     C                   When      Cod_Elenco  = 'DF'
425900150312     C*
426000150312     C                   Select
426100150312     C                   When      Cod_Rigo    = 1        and
426200150312     C                             LettoRigo1  = *Off
426300150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
426400150312     C                   Eval      LettoRigo1  = *On
426500150312     C*
426600150312     C                   When      Cod_Rigo    = 2        and
426700150312     C                             LettoRigo2  = *Off
426800150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
426900150312     C                   Eval      LettoRigo2  = *On
427000150312     C*
427100150312     C                   When      Cod_Rigo    = 3        and
427200150312     C                             LettoRigo3  = *Off
427300150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
427400150312     C                   Eval      LettoRigo3  = *On
427500150312     C*
427600150312     C                   When      Cod_Rigo    = 4        and
427700150312     C                             LettoRigo4  = *Off
427800150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
427900150312     C                   Eval      LettoRigo4  = *On
428000150312     C*
428100150312     C                   When      Cod_Rigo    = 5        and
428200150312     C                             LettoRigo5  = *Off
428300150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
428400150312     C                   Eval      LettoRigo5  = *On
428500150312     C*
428600150312     C                   When      Cod_Rigo    = 6        and
428700150312     C                             LettoRigo6  = *Off
428800150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
428900150312     C                   Eval      LettoRigo6  = *On
429000150312     C*
429100150312     C                   When      Cod_Rigo    = 7        and
429200150312     C                             LettoRigo7  = *Off
429300150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
429400150312     C                   Eval      LettoRigo7  = *On
429500150312     C*
429600150312     C                   When      Cod_Rigo    = 8        and
429700150312     C                             LettoRigo8  = *Off
429800150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
429900150312     C                   Eval      LettoRigo8  = *On
430000150312     C*
430100150312     C                   When      Cod_Rigo    = 9        and
430200150312     C                             LettoRigo9  = *Off
430300150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
430400150312     C                   Eval      LettoRigo9  = *On
430500150312     C*
430600150312     C                   When      Cod_Rigo    = 10       and
430700150312     C                             LettoRigo10 = *Off
430800150312     C                   Eval      $NrQuaDF    = $NrQuaDF + 1
430900150312     C                   Eval      LettoRigo10 = *On
431000150312     C                   endSl
431100150312     C*
431200150312     C*--------------
431300150312     C* Operazioni con soggetti non residenti (Operazioni attive)
431400150312     C                   When      Cod_Elenco  = 'FN'
431500150312     C*
431600150312     C                   Select
431700150312     C                   When      Cod_Rigo    = 1        and
431800150312     C                             LettoRigo1  = *Off
431900150312     C                   Eval      $NrQuaFN    = $NrQuaFN + 1
432000150312     C                   Eval      LettoRigo1  = *On
432100150312     C*
432200150312     C                   When      Cod_Rigo    = 2        and
432300150312     C                             LettoRigo2  = *Off
432400150312     C                   Eval      $NrQuaFN    = $NrQuaFN + 1
432500150312     C                   Eval      LettoRigo2  = *On
432600150312     C*
432700150312     C                   When      Cod_Rigo    = 3        and
432800150312     C                             LettoRigo3  = *Off
432900150312     C                   Eval      $NrQuaFN    = $NrQuaFN + 1
433000150312     C                   Eval      LettoRigo3  = *On
433100150312     C                   endSl
433200150312     C*
433300150312     C*--------------
433400150312     C* Acquisti di servizi da non residenti e Acquisti da operatori di San Marino
433500150312     C                   When      Cod_Elenco  = 'SE'
433600150312     C*
433700150312     C                   Select
433800150312     C                   When      Cod_Rigo    = 1        and
433900150312     C                             LettoRigo1  = *Off
434000150312     C                   Eval      $NrQuaSE    = $NrQuaSE + 1
434100150312     C                   Eval      LettoRigo1  = *On
434200150312     C*
434300150312     C                   When      Cod_Rigo    = 2        and
434400150312     C                             LettoRigo2  = *Off
434500150312     C                   Eval      $NrQuaSE    = $NrQuaSE + 1
434600150312     C                   Eval      LettoRigo2  = *On
434700150312     C*
434800150312     C                   When      Cod_Rigo    = 3        and
434900150312     C                             LettoRigo3  = *Off
435000150312     C                   Eval      $NrQuaSE    = $NrQuaSE + 1
435100150312     C                   Eval      LettoRigo3  = *On
435200150312     C                   endSl
435300150312     C*
435400150312     C*--------------
435500150312     C* OPERAZIONI LEGATE AL TURISMO
435600150312     C                   When      Cod_Elenco  = 'TU'
435700150312     C*
435800150312     C                   Select
435900150312     C                   When      Cod_Rigo    = 1        and
436000150312     C                             LettoRigo1  = *Off
436100150312     C                   Eval      $NrQuaTU    = $NrQuaTU + 1
436200150312     C                   Eval      LettoRigo1  = *On
436300150312     C*
436400150312     C                   When      Cod_Rigo    = 2        and
436500150312     C                             LettoRigo2  = *Off
436600150312     C                   Eval      $NrQuaTU    = $NrQuaTU + 1
436700150312     C                   Eval      LettoRigo2  = *On
436800150312     C*
436900150312     C                   When      Cod_Rigo    = 3        and
437000150312     C                             LettoRigo3  = *Off
437100150312     C                   Eval      $NrQuaTU    = $NrQuaTU + 1
437200150312     C                   Eval      LettoRigo3  = *On
437300150312     C                   endSl
437400150312     C*
437500150312     C                   endSl
437600150312     C*
437700150312     C                   Add       1             $I
437800150312     C*
437900150312     C                   endDO
438000150312     C*
438100150312     C                   EXSR      InzSkiera
438200150312     C*
438300150312R177 C                   ENDSR
438400150312R177 C/EJECT
438500000000     C******************************************************
438600000000     C* Scrittura record t.r. D
438700000000     C******************************************************
438800000000     C     WrtTpRecD     BegSr
438900000000     C*
439000000000     C                   ADD       1             CNT_D
439100000000     C                   ADD       1             Cnt_Righe
439200000000     C*
439300000000     C                   Clear                   SDGSG61DSD
439400000000     C* Tipo Record
439500000000     C                   Eval      G61DTR       = 'D'
439600000000     C* Cod.Fiscale
439700000000     C                   Eval      G61DCFI      = XScCdF
439800000000     C*
439900000000     C* Progressivo modulo
440000000000     C                   Eval      G61DPRGMOD   = ETWPRGMOD
440100000000     C*
440200000000     C* Identificativo produttore software
440300160202R195 X****               MoveL     IDPRODA08     G61DIPS
440400160202R195 C                   MoveL     $IdProd       G61DIPS
440500000000     C*-
440600000000     C                   MoveL     ETWDAT        G61DDATI
440700150312R177 C*
440800150312 "   C* recupero i dati presenti nel record per contare i quadri
440900150312 "   C                   EXSR      ContaQuadri
441000150312R177 C
441100000000     C*-
441200000000     C* Carattere di controllo
441300000000     C                   MoveL     'A'           G61DK1
441400000000     C*-
441500000000     C* Elementi di fine riga
441600000000     C* Impostare i valori esadecimali '0D' e '0A'
441700000000     C* (caratteri ASCI 'CR' ed 'LF')
441800000000     C*
441900000000     C                   Eval      G61DK2  = *blanks
442000000000     C                   Eval      G61DK2  = X'0D25'
442100000000     C*
442200000000     C*----
442300000000     C                   Clear                   SDGETE000
442400000000     C                   MoveL     SDGSG61DSD    ETEREC
442500000000     C*----
442600000000     C                   WRITE     SDGETE000
442700000000     C                   FEOD      SDGETE00F
442800000000     C*
442900000000     C                   EndSR
443000000000     C******************************************************
443100000000     C* Riempimento dati per t.r. E
443200000000     C******************************************************
443300000000     C     WrtTpRecE     BegSr                                                  *
443400000000     C*
443500000000     C                   Clear                   SDGSG61DSE
443600000000     C* Tipo Record
443700000000     C                   Eval      G61ETR       = 'E'
443800000000     C* Cod.Fiscale
443900000000     C                   Eval      G61ECFI      = XscCDf
444000000000     C*
444100000000     C* Progressivo modulo
444200000000     C                   Eval      G61EPRGMOD   = 1
444300000000     C*
444400000000     C* Identificativo produttore software
444500160202R195 X****               MoveL     IDPRODA08     G61EIPS
444600160202R195 C                   MoveL     $IdProd       G61EIPS
444700000000     C*-
444800000000     C                   Clear                   Skiera
444900000000     C                   Eval      WSavQua  ='TA'
445000000000     C*-
445100000000     C                   Exsr      GesRec_TA
445200000000     C                   Movea     Skiera        G61EDATI
445300000000     C*-
445400000000     C* Carattere di controllo
445500000000     C                   MoveL     'A'           G61EK1
445600000000     C*-
445700000000     C* Elementi di fine riga
445800000000     C* Impostare i valori esadecimali '0D' e '0A'
445900000000     C* (caratteri ASCI 'CR' ed 'LF')
446000000000     C*
446100000000     C                   Eval      G61EK2  = *blanks
446200000000     C                   Eval      G61EK2  = X'0D25'
446300000000     C*
446400000000     C*----
446500000000     C                   Clear                   SDGETE000
446600000000     C                   MoveL     SDGSG61DSE    ETEREC
446700000000     C*----
446800000000     C                   WRITE     SDGETE000
446900000000     C                   FEOD      SDGETE00F
447000000000     C*
447100000000     C                   EndSR
447200000000     C******************************************************
447300000000     C* Riempimento dati per t.r. Z
447400000000     C******************************************************
447500000000     C     WrtTpRecZ     BegSr                                                  *
447600000000     C*
447700000000     C                   Clear                   SDGSG61DSZ
447800000000     C* Tipo Record
447900000000     C                   Eval      G61ZTR       = 'Z'
448000000000     C*-
448100000000     C                   Z-ADD     1             G61ZNB
448200000000     C*-
448300000000     C                   Z-ADD     CNT_C         G61ZNC
448400000000     C*-
448500000000     C                   Z-ADD     CNT_D         G61ZND
448600000000     C*-
448700000000     C                   Z-ADD     1             G61ZNE
448800000000     C*-
448900000000     C* Carattere di controllo
449000000000     C                   MoveL     'A'           G61ZK1
449100000000     C*-
449200000000     C* Elementi di fine riga
449300000000     C* Impostare i valori esadecimali '0D' e '0A'
449400000000     C* (caratteri ASCI 'CR' ed 'LF')
449500000000     C*
449600000000     C                   Eval      G61ZK2  = *blanks
449700000000     C                   Eval      G61ZK2  = X'0D25'
449800000000     C*
449900000000     C                   MoveL     SDGSG61DSZ    ETEREC
450000000000     C                   WRITE     SDGETE000
450100000000     C                   FEOD      SDGETE00F
450200000000     C*
450300000000     C                   EndSR                                                  *
450400000000     C******************************************************
450500000000     C* Posizionamento SDGEP3
450600000000     C******************************************************
450700000000     C     PosizETW      BegSr                                                  *
450800000000     C*
450900000000     C     K01ETW01      SETLL     SDGETW01L                              23
451000000000     C*
451100000000     C                   EndSR                                                  *
451200000000     C******************************************************
451300000000     C* Leggo SDGETW e scrivo il record telematico SDGETE
451400000000     C******************************************************
451500000000     C     LetturaETW    BegSr                                                  *
451600000000     C*
451700000000     C                   READ      SDGETW01L                              23
451800000000     C                   Eval      $FineETW    = *in23
451900000000     C*
452000000000     C                   Select
452100000000     C*
452200160202R195 X****               When      FormatoA08    = 'A'   and
452300160202R195 C                   When      $Formato      = 'A'   and
452400000000     C                             *in23         = *Off
452500000000     C                   Exsr      WrtTpRecC
452600160202     C*
452700160202R195 X****               When      FormatoA08    = 'D'   and
452800160204R195 C                   When      $Formato      = 'D'   and
452900000000     C                             *in23         = *Off
453000000000     C                   Exsr      WrtTpRecD
453100000000     C*
453200000000     C                   EndSl
453300000000     C*
453400000000     C                   EndSR                                                  *
453500000000     C******************************************************
453600000000     C* Valorizzazione data per telematico
453700000000     C******************************************************
453800000000     C     ValorizDTA    BegSr                                                  *
453900000000     C*
454000000000     C                   Clear                   WrkDt
454100000000     C*
454200000000     C                   Move      AMGAnno       GMAAnno
454300000000     C                   Move      AMGMese       GMAMese
454400000000     C                   Move      AMGGiorno     GMAGiorno
454500000000     C*
454600000000     C                   Eval      WrkDt        = DtGMA
454700000000     C*
454800000000     C                   EndSR
454900000000     C******************************************************
455000000000     C* Reperimento della natura giuridica
455100000000     C******************************************************
455200000000     C     RepNatGiu     BegSr                                                  *
455300000000     C*
455400000000     C                   Reset                   SDGTABDS
455500000000     C*
455600000000     C                   MOVE      '1'           XTBRIC
455700000000     C                   MOVE      SocietaG61    XTBAZI
455800000000     C                   MOVE      *ZERO         XTBKEY
455900000000     C                   MOVE      'G16'         XTBCOD
456000000000     C                   MOVE      WrkNatGiu     XTBKEY
456100000000     C                   CALL      'SDGTABDR'
456200000000     C                   PARM                    SDGTABDS
456300000000     C                   if        XTBERR      = *Off
456400000000     C                   MoveL     XTBUNI        ANGG16DS
456500000000     C                   EndIf
456600000000     C*
456700000000     C                   EndSR                                                  *
456800000000     C******************************************************
456900000000     C* Reperimento dati stato
457000000000     C******************************************************
457100000000     C     RepStato      BegSr                                                  *
457200000000     C*
457300000000     C                   Reset                   SDGTABDS
457400000000     C*
457500000000     C                   MOVE      '1'           XTBRIC
457600000000     C                   MOVE      SocietaG61    XTBAZI
457700000000     C                   MOVE      *ZERO         XTBKEY
457800000000     C                   MOVE      'G12'         XTBCOD
457900000000     C                   MOVE      WrkStato      XTBKEY
458000000000     C                   CALL      'SDGTABDR'
458100000000     C                   PARM                    SDGTABDS
458200000000     C                   if        XTBERR      = *Off
458300000000     C                   MoveL     XTBUNI        ANGG12DS
458400000000     C                   EndIf
458500000000     C*
458600000000     C                   EndSR                                                  *
458700000000     C************************************************************
458800000000     C* Reperimento dati società
458900000000     C************************************************************
459000000000     C     RepSoc        BegSr
459100000000     C*
459200000000     C                   CALL      'SDGSOCDR'
459300000000     C                   PARM                    TIPXSC            6
459400000000     C                   PARM                    SOCXSC            3
459500000000     C                   PARM                    CDSXSC            9 0
459600000000     C                   PARM                    MODXSC            3
459700000000     C                   PARM      *Blanks       RTNXSC            1
459800000000     C                   PARM                    XSOCDS
459900000000     C                   PARM                    KPJBA
460000000000     C*
460100000000     C                   EndSR
460200000000     C************************************************************
460300000000     C* Reperimento nome file per invio operazioni iva
460400000000     C************************************************************
460500000000     C     RepNomeF      BegSr
460600000000     C*
460700000000     C                   Eval      WNumero     = *All'0'
460800000000     C                   Z-Add     AnnoG61       WAnno
460900000000     C                   Z-Add     $ProgrInvio   WPrgInvio
461000000000     C                   Move      WNumero       $Numero
461100160204R195 C*
461200160204 "   C* Spesometro
461300160204 "   C                   if        TpComG61    = 'E'
461400160204 "   C                   Move      'OP'          $CdScarico        2
461500160204 "   C                   else
461600160204 "   C* Black list annuale
461700160204 "   C                   Move      'BA'          $CdScarico
461800160204R195 C                   endif
461900000000     C*
462000000000     C                   CALL      'X35NOMFR'
462100000000     C                   PARM                    SocietaG61
462200000000     C                   PARM      *Blank        $Unita            8
462300160204R195 X****               PARM      'OP'          $CdScarico        2
462400160204R195 C                   PARM                    $CdScarico
462500000000     C                   PARM                    $Numero           9
462600000000     C                   PARM                    $File            10
462700000000     C                   PARM                    $Libreria        10
462800000000     C                   PARM                    $Errore           1
462900000000     C*
463000000000     C                   if        $Errore    <> '0'
463100000000     C                   Eval      Errore   = '2'
463200000000     C                   Exsr      ElaErrori
463300000000     C                   Exsr      EndPgm
463400000000     C                   EndIf
463500000000     C*
463600000000     C                   EndSR
463700000000     C************************************************************
463800000000     C* Reperisci dati persona fisica attraverso soggetto
463900000000     C************************************************************
464000000000     C     RepPersFis    BegSr
464100000000     C*
464200000000     C                   Clear                   $CdFiscSog
464300000000     C                   Clear                   $CdIVASog
464400000000     C                   Clear                   $CogRagSog
464500000000     C                   Clear                   $NomeSog
464600000000     C                   Clear                   $DtNascSog
464700000000     C                   Clear                   $LocNascSog
464800000000     C                   Clear                   $PrvNascSog
464900000000     C                   Clear                   $StatoSog
465000000000     C                   Clear                   $StaFedSog
465100000000     C                   Clear                   $LocalitSog
465200000000     C                   Clear                   $IndirESog
465300000000     C                   Clear                   $SexSog
465400000000     C*
465500000000     C                   Eval      SO3Sogg     = SocSogg
465600000000     C                   Move      *Loval        SO3DtFiVl
465700000000     C                   Eval      SO3Sys      = 0
465800000000     C                   Eval      SO3NrAsInt  = 0
465900000000     C*
466001160524R224 X**** K04SO301      CHAIN     SDGSO301L                          21
466002160524R224 C                   EXSR      ChainSDGSO3
466003160524R224 C*
466100000000     C                   If        *In21       = *Off
466200000000     C                   MoveL(P)  SO3CdFisc     $CdFiscSog
466300000000     C                   MoveL(P)  SO3PartIva    $CdIVASog
466400000000     C                   MoveL(P)  SO3CogRag     $CogRagSog
466500000000     C                   MoveL(P)  SO3Nome       $NomeSog
466600000000     C                   Move      SO3DtNasc     $DtNascSog
466700000000     C                   MoveL(P)  SO3LocNasc    $LocNascSog
466800000000     C                   MoveL(P)  SO3PrvNasc    $PrvNascSog
466900000000     C                   MoveL(P)  SO3Stato      $StatoSog
467000000000     C                   MoveL(P)  SO3StaFed     $StaFedSog
467100000000     C                   MoveL(P)  SO3Localit    $LocalitSog
467200000000     C                   MoveL(P)  SO3IndirE     $IndirESog
467300000000     C                   MoveL     SO3Sex        $SexSog
467400000000     C                   EndIf
467500000000     C*
467600000000     C                   EndSR
467601160524R224 C************************************************************
467602160524 "   C* Lettura dati aggiuntivi anagrafici
467603160524 "   C************************************************************
467604160524R224 C     ChainSDGSO3   BEGSR
467618160609     C*
467619160609     C                   Eval      SO3Dest     = $Dest
467620160524     C*
467621160524     C     K05SO301      CHAIN     SDGSO301L                          21
467622160524     C*
467623160524     C* se non trovato, provo con la destinazione Generica
467624160524     C                   if        *In21       = *On     and
467625160524     C                             SO3Dest    <> *Blank
467626160524     C                   Eval      SO3Dest     = *Blank
467627160524     C     K05SO301      CHAIN     SDGSO301L                          21
467628160524     C                   endif
467629160524     C*
467630160524R224 C                   ENDSR
467631160524R224 C/EJECT
467700000000     C************************************************************
467800000000     C* Gestione file per invio telematico
467900000000     C************************************************************
468000000000     C     GesFilTel     BegSr
468100000000     C*
468200000000     C                   Move      AnnoG61       AnnoA             4
468300000000     C                   MoveL     AnnoA         $DesFileI
468400000000     C                   Move      UDateIso      $DtAlfa
468500000000     C*
468600000000     C* Creo già la descrizione del file
468700000000     C                   Clear                   $DesFileO
468800000000     C     'Anno'        Cat       AnnoA:1       $DesFileO
468900160204R195 C*
469000160204 "   C* Spesometro
469100160204R195 C                   if        TpComG61    = 'E'
469200000000     C                   Cat       CostFile:1    $DesFileO
469300160204R195 C                   else
469400160204 "   C* Black List annuale
469500160204 "   C                   Cat       CostFileB:1   $DesFileO
469600160204 "   C                   endif
469700160204R195 C*
469800000000     C                   Cat       '- Soc.':1    $DesFileO
469900000000     C                   Cat       XscSoc :1     $DesFileO
470000000000     C                   Cat       'del':1       $DesFileO
470100000000     C                   Cat       $DtAlfa:1     $DesFileO
470200000000     C*
470300000000     C                   Eval      $Libreria   = LibG61
470400000000     C                   Eval      $Errore     = *Off
470500000000     C*
470600000000     C                   CALL      'SDGSG61C1'
470700000000     C                   PARM                    $File
470800000000     C                   PARM                    $Libreria
470900000000     C                   PARM                    WTpDist           2
471000000000     C                   PARM                    $DtAlfa          10
471100000000     C                   PARM                    $DesFileI         8
471200000000     C                   PARM                    $DesFileO        50
471300000000     C                   PARM                    $Errore           1
471400000000     C*
471500000000     C                   if        $Errore    <> '0'
471600000000     C                   Eval      Errore   = '3'
471700000000     C                   Exsr      ElaErrori
471800000000     C                   Exsr      EndPgm
471900000000     C                   EndIf
472000000000     C*
472100000000     C                   Eval      FILETELG61  = $FILE
472200000000     C*
472300000000     C                   EndSR
472400000000     C************************************************************
472500000000     C* Iscrizione a catalogo del file scarico
472600000000     C************************************************************
472700000000     C     IscrCatF      BegSr
472800000000     C*
472900000000     C
473000000000     C                   Eval      WNumero     = *All'0'
473100000000     C                   Z-Add     AnnoG61       WAnno
473200000000     C                   Z-Add     $ProgrInvio   WPrgInvio
473300000000     C                   Move      WNumero       $Numero
473400000000     C*
473500000000     C                   Move      UDateIso      $DtAlfa
473600160204R195 C*
473700160204 "   C* Spesometro
473800160204 "   C                   if        TpComG61    = 'E'
473900160204 "   C                   Move      'OP'          $CdScarico        2
474000160204 "   C                   else
474100160204 "   C* Black list annuale
474200160204 "   C                   Move      'BA'          $CdScarico
474300160204R195 C                   endif
474400000000     C*
474500000000     C                   Call      'X36CATFR'
474600000000     C                   PARM                    SocietaG61
474700000000     C                   PARM      *Blank        $Unita
474800000000     C                   PARM                    $File
474900000000     C                   PARM                    $Libreria
475000000000     C                   PARM                    $DesFileO
475100160204R195 X****               PARM      'OP'          $CdScarico        2
475200160204R195 C                   PARM                    $CdScarico
475300000000     C                   PARM      'P'           $TpSupp           1
475400000000     C                   PARM      *Blank        $NmSupp          10
475500000000     C                   PARM      '4'           $TpDist           1
475600000000     C                   PARM                    $DtAlfa          10
475700000000     C                   PARM                    $Numero           9
475800000000     C                   PARM      *Blank        $DesBan          44
475900000000     C                   PARM                    KNmus
476000000000     C                   PARM      '0'           Sospeso           1
476100000000     C                   PARM      '1'           Video             1
476200000000     C*
476300000000     C                   EndSR
476400000000     C************************************************************
476500000000     C* Inizializzazioni variabili
476600000000     C************************************************************
476700000000     C     InzVar        BegSr
476800000000     C*
476900000000     C                   MoveL     KPJBU         SDGSG61DS
477000000000     C*
477100000000     C                   Eval      ERRG61      = *Off
477200000000     C                   Eval      MaxRecG61   = MaxRecG61 -4
477300000000     C*
477400000000     C                   MoveL     'SOC001'      TIPXSC
477500000000     C                   MoveL     SOCIETAG61    SOCXSC
477600000000     C*
477700000000     C                   Exsr      RepSoc
477800000000     C*
477900000000     C                   If        RTNXSC     <> '1'
478000000000     C                   MoveL     XSOCDS        SOC001
478100000000     C                   ElSe
478200000000     C                   Eval      $Errori     = *On
478300000000     C                   Exsr      EndPgm
478400000000     C                   EndIf
478500000000     C*
478600000000     C* se definito limite max per il file da creare
478700000000     C* definisco i parametri per l'invio multiplo
478800000000     C                   if        MaxDimG61   > 0   or
478900000000     C                             MaxRecG61   > 0
479000000000     C****               Exsr      DefLimiti
479100000000     C                   EndIf
479200000000     C*
479300000000     C                   Move      XscSoc        SIFSocieta
479400000000     C     K01Sif01      Chain     AnSif01L                           21
479500000000     C  N21              Move      SifFlgOpe     WrkFlgOpe
479600000000     C   21              Move      *Blanks       SocSogg
479700000000     C*
479800000000     C* reperisco DATFMT del job
479900000000     C                   CallB     'XDATFMT'
480000000000     C                   Parm                    $DatFmt           4
480100000000     C*
480200000000     C                   Setoff                                       77
480300000000     C*
480400000000     C* Recupera stato UIC della societa
480500000000     C                   Move      XscSta        WrkStato
480600000000     C                   Exsr      RepStato
480700000000     C                   Move      CODUICG12     UICSocieta
480800000000     C*
480900000000     C                   SetOn                                        OF
480901160609R224 C*
480902160609R224 C                   EXSR      RepDatiCon
481000000000     C*
481100000000     C                   EndSR                                                  *
481200000000     C*
481201160609R224 C************************************************************
481202160609 "   C* REPERIMENTO dati anagafici aggiuntivi contribuente
481203160609 "   C************************************************************
481204160609R224 C     RepDatiCon    BegSr
481205160609     C*
481206160609     C* preparo i parametri di chiamata al driver
481207160609     C                   Clear                   XMPEDS
481208160609     C                   EVAL      TpChiamata = '01'
481209160609     C                   EVAL      NChiavi = 5
481210160609     C                   EVAL      VLogica = 2
481211160609     C*
481212160609     C* preparo le chiavi per la chiamata al driver
481213160609     C                   Clear                   XMPEDS1
481214160609     C                   MoveL     XSCSOC        DSMPESOC
481215160609     C                   MoveL     'SDGSG60R  '  DSMPEPGM
481216160609     C*
481217160609     C                   Select
481218160609     C* Spesometro
481219160609     C                   When      TpComG61    = 'E'
481220160609     C                   Move      'EC'          DSMPEPGM
481225160609     C*
481226160609     C* Black List
481227160609     C                   When      TpComG61    = 'B'
481228160609     C                   Move      'BC'          DSMPEPGM
481229160609     C*
481230160609     C                   endsl
481231160609     C*
481232160609     C                   MoveL     *Blanks       DSMPEPRF
481233160609     C* Si reperiscono i dati di default a seconda delle modalita con
481234160609     C* cui il pgm e usato
481235160609     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
481236160609     C* MPETIP = 'PV'  pgm richiamato da menu
481237160609     C* MPETIP = 'PB'  x lancio Batch
481238160609     C                   MoveL     'PB'          DSMPETIP
481239160609     C*
481240160609     C                   CALLB     'XMPE'
481241160609     C                   PARM                    XMPEDS
481242160609     C                   PARM                    XMPEDS1
481243160609     C*
481244160609     C     Risultato     IfEq      '0'
481245160609     C                   MoveL     DSMPEPAR      DSContrib
481246160609     C                   EndIf
481247160609     C*
481248160609R224 C                   EndSr
481300000000     C******************************************************
481400000000     C* Inizializzo campi per la gestione della tabella
481500000000     C******************************************************
481600000000     C     InzSkiera     BEGSR                                                  *
481700000000     C*
481800000000     C                   Reset                   Skiera
481900000000     C                   Z-ADD     0             $I
482000000000     C                   Z-ADD     0             Conta_Rigo
482100000000     C                   Z-ADD     0             Conta_Colo
482200000000     C*
482300000000     C                   ENDSR
482400000000     C/EJECT
482500000000     C************************************************************
482600000000     C* Azzeramento progressivi
482700000000     C************************************************************
482800000000     C     AzzeraProg    BegSr
482900000000     C*
483000000000     C                   Z-Add     0             NrETW
483100000000     C*
483200000000     C                   Z-Add     0             Cnt_C
483300000000     C                   Z-Add     0             Cnt_D
483400000000     C*
483500000000     C                   Z-Add     0             CntFA
483600000000     C                   Z-Add     0             CntSA
483700000000     C                   Z-Add     0             CntBL
483800000000     C                   Z-Add     0             CntFE
483900000000     C                   Z-Add     0             CntFR
484000000000     C                   Z-Add     0             CntNE
484100000000     C                   Z-Add     0             CntNR
484200000000     C                   Z-Add     0             CntDF
484300000000     C                   Z-Add     0             CntFN
484400000000     C                   Z-Add     0             CntSE
484500000000R136 C                   Z-Add     0             CntTU
484600000000     C*
484700000000R139 C                   Z-Add     0             ProgMod
484800000000     C                   Z-Add     0             Prog_FA
484900000000     C                   Z-Add     0             Prog_SA
485000000000     C                   Z-Add     0             Prog_BL
485100000000     C                   Z-Add     0             Prog_FE
485200000000     C                   Z-Add     0             Prog_FR
485300000000     C                   Z-Add     0             Prog_NE
485400000000     C                   Z-Add     0             Prog_NR
485500000000     C                   Z-Add     0             Prog_DF
485600000000     C                   Z-Add     0             Prog_FN
485700000000     C                   Z-Add     0             Prog_SE
485800000000R136 C                   Z-Add     0             Prog_TU
485900000000     C*
486000000000     C                   Z-Add     0             ProgMod_FA
486100000000     C                   Z-Add     0             ProgMod_SA
486200000000     C                   Z-Add     0             ProgMod_BL
486300000000     C                   Z-Add     0             ProgMod_FE
486400000000     C                   Z-Add     0             ProgMod_FR
486500000000     C                   Z-Add     0             ProgMod_NE
486600000000     C                   Z-Add     0             ProgMod_NR
486700000000     C                   Z-Add     0             ProgMod_DF
486800000000     C                   Z-Add     0             ProgMod_FN
486900000000     C                   Z-Add     0             ProgMod_SE
487000000000R136 C                   Z-Add     0             ProgMod_TU
487100000000     C*
487200000000     C                   Z-Add     0             $NrRecTot
487300000000     C*
487400000000     C                   Z-ADD     0             WRKPRGMOD         7 0
487500000000     C*
487600140414R147 C                   Z-Add     0             Togli_FA
487700150306 "   C                   Z-Add     0             Togli_BL
487800150309 "   C                   Z-Add     0             Numero_FA
487900150309 "   C                   Z-Add     0             Numero_BL
488000140414R147 C*
488100000000     C                   EndSR                                                  *
488200000000     C************************************************************
488300000000     C* Parametri per invio multiplo
488400000000     C************************************************************
488500000000     C     DefLimiti     BegSr
488600000000     C*
488700000000     C                   if        MaxDimG61   > 0
488800000000     C*
488900000000     C* dimensione max 3MG = 3 * 1024 KB * 1024 byte = 3.145.728 byte
489000000000     C* diviso 1800 byte (=caratteri per record)= 1747,63 record circa
489100000000     C* meno 2 record (0 testa, 9 coda) = 1745 righe dati
489200000000     C* se presenti più di 1745 righe chiamo pgm per creazione
489300000000     C* di più file per multi invio
489400000000     C*
489500000000     C                   Z-Add     MaxDimG61     WImpInp
489600000000     C*
489700000000     C                   Eval      LenEPW = %size(SDGETWDS)
489800000000     C*
489900000000     C                   CallP     X0201MultC(WImpInp:
490000000000     C                                        ($KByte * $KByte):
490100000000     C                                        LenEPW:
490200000000     C                                        '-': '0': 0: WImpOut)
490300000000     C*
490400000000     C                   Eval      $MaxRighe   = WImpOut - 3
490500000000     C*
490600000000     C                   EndIf
490700000000     C*
490800000000     C                   EndSR
490900000000     C************************************************************
491000000000     C* OPERAZIONI INIZIALI
491100000000     C************************************************************
491200000000     C     *INZSR        BegSr
491300000000     C*
491400000000     C* Reperimento parametri
491500000000     C*
491600000000     C     *ENTRY        PLIST                                                  *
491700000000     C                   PARM                    KPJBA                          *
491800000000     C*
491900000000     C     *DMY          Move      Udate         UDateISO
492000000000     C*
492100000000     C                   EndSR
492200000000     C************************************************************
492300000000     C* DEFINIZIONE KLIST
492400000000     C************************************************************
492500000000     C     DefKlist      BegSr
492600000000     C*
492700000000     C* KLIST
492800000000     C*
492900000000     C* SDGSo301L
492901160524R224 X**** K04SO301      KLIST
492902160524R224 C     K05SO301      KLIST
493100000000     C                   KFLD                    SO3Sogg
493200000000     C                   KFLD                    SO3DtFiVL
493300000000     C                   KFLD                    SO3Sys
493400000000     C                   KFLD                    SO3NrAsint
493401160524R224 C                   KFLD                    SO3Dest
493500000000     C*
493600000000     C* SDGETW01L
493700000000     C     K01ETW01      KLIST
493800000000     C                   KFLD                    ETWSocieta
493900000000     C*
494000000000     C* AnSif01L
494100000000     C     K01SIF01      KLIST
494200000000     C                   KFLD                    SIFSocieta
494300000000     C*
494400000000     C                   EndSR
494500000000     C************************************************************
494600000000     C* Reperimento parametri di setup per Black list
494700000000     C************************************************************
494800000000     C     RepParMod     BEGSR
494900000000     C*
495000000000     C                   Clear                   SDGPA08DS
495100000000     C*
495200000000     C                   CALLB     'XPAR'
495300000000     C                   Parm                    XScSoc
495400000000     C                   Parm      'SDGSPECF'    XPaPar            8
495500000000     C                   Parm                    XPaOut         2000
495600000000     C                   Parm      *OFF          XPaErr            1
495700000000     C                   Parm      ' '           XPaRic            1
495800000000     C*
495900000000     C                   if        XPaErr     <> *On
496000000000     C                   MoveL     XPaOut        SDGPA08DS
496100160202R195 C*
496200160202 "   C                   Eval      $Formato    = FormatoA08
496300160202 "   C                   Eval      $IdProd     = IdProdA08
496400160202 "   C                   if        NoteVarA08  = *Blank     or
496500160202 "   C                             NoteVarA08  = '1'
496600160202 "   C                   Eval      $NoteVar    = '1'
496700160202 "   C                   else
496800160202 "   C                   Eval      $NoteVar    = '2'
496900160202 "   C                   endif
497000160202R195 C*
497100000000     C                   EndIf
497200000000     C*
497300000000     C                   EndSR
497400160202R195 C************************************************************
497500160202 "   C* Reperimento parametri di setup per Black List
497600160202 "   C************************************************************
497700160202R195 C     RepSDBLACK2   BEGSR
497800160202     C*
497900160202     C                   Clear                   SDGPA11DS
498000160202     C*
498100160202     C                   CALLB     'XPAR'
498200160202     C                   Parm                    XScSoc
498300160202     C                   Parm      'SDBLACK2'    XPaPar            8
498400160202     C                   Parm                    XPaOut         2000
498500160202     C                   Parm      *OFF          XPaErr            1
498600160202     C                   Parm      ' '           XPaRic            1
498700160202     C*
498800160202     C                   if        XPaErr     <> *On
498900160202     C                   MoveL     XPaOut        SDGPA11DS
499000160202     C*
499100160202     C                   Eval      $Formato    = 'A'
499200160202     C                   Eval      $IdProd     = IdProdA11
499300160202     C                   Eval      $NoteVar    = NoteVarA11
499400160202     C*
499500160202     C                   endif
499600160202     C*
499700160202R195 C                   ENDSR
499800160202R195 C/EJECT
499900000000     C************************************************************
500000000000     C* Reperimento dati Intermediario
500100000000     C************************************************************
500200000000     C     RepDatInter   BEGSR
500300000000     C*
500400000000     C* preparo i parametri di chiamata al driver
500500000000     C                   Clear                   XMPEDS
500600000000     C                   EVAL      TpChiamata = '01'
500700000000     C                   EVAL      NChiavi = 5
500800000000     C                   EVAL      VLogica = 2
500900000000     C*
501000000000     C* preparo le chiavi per la chiamata al driver
501100000000     C                   Clear                   XMPEDS1
501200000000     C                   MoveL     XSCSOC        DSMPESOC
501300000000     C                   MoveL     'SDGSG60R R'  DSMPEPGM
501400160202R195 C*
501500160202 "   C                   Select
501600160202 "   C* Spesometro
501700160202R195 C                   When      TpComG61    = 'E'
501800000000     C                   Move      'R'           DSMPEPGM
501900160202R195 C*
502000160202 "   C* Black list annuale da 2016
502100160202 "   C                   When      TpComG61    = 'B'
502200160226 "   C                   Move      'B'           DSMPEPGM
502300160202 "   C*
502400160202 "   C                   endSl
502500160202R195 C*
502600000000     C                   MoveL     *blanks       DSMPEPRF
502700000000     C* Si reperiscono i dati di default a seconda delle modalità con
502800000000     C* cui il pgm è usato
502900000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
503000000000     C* MPETIP = 'PV'  pgm richiamato da menù
503100000000     C* MPETIP = 'PB'  x lancio Batch
503200000000     C                   MoveL     'PB'          DSMPETIP
503300000000     C*
503400000000     C                   CALLB     'XMPE'
503500000000     C                   PARM                    XMPEDS
503600000000     C                   PARM                    XMPEDS1
503700000000     C*
503800000000     C     Risultato     IfEq      '0'
503900000000     C                   MoveL     DSMPEPAR      DSInterm
504000000000     C                   EndIf
504100000000     C*
504200000000     C                   EndSR
504300000000     C************************************************************
504400000000     C* inz ds
504500000000     C************************************************************
504600000000     C     Inz0502R      BegSr
504700000000     C*
504800000000     C* Preparo dati per sql
504900000000     C                   RESET                   SDG00502DS
505000000000     C*
505100000000     C                   Eval      Soc0502     = SocietaG61
505200000000     C                   Eval      Anno0502    = AnnoG61
505300160202R195 X*ex R141           if        NoteVarA08  = *Blank     or
505400160202 "   X*ex R141                     NoteVarA08  = '1'
505500160202 "   X*ex R141           Eval      NoteVa0502  = '1'
505600160202 "   X*ex R141           else
505700160202 "   X*ex R141           Eval      NoteVa0502  = '2'
505800160202R195 X*ex R141           endif
505900160202R195 C*
506000160202 "   C                   Eval      NoteVa0502  = $NoteVar
506100160202R195 C*
506200000000     C                   Eval      Opz0502     = '05'
506300000000     C                   Eval      TpRott0502  = '01'
506400000000     C                   Eval      Rott0502    = '02'
506500000000     C                   Eval      Ric0502     = '0'
506600000000     C                   Eval      TpList0502  = '02'
506700000000     C*
506800000000R067 C* chiesti solo gli attivi
506900000000R067 C                   Eval      Ann0502     = '2'
507000000000     C                   Eval      TpAnn0502   = *Hival
507100000000     C                   Eval      TpN0502     = *Hival
507200000000     C                   Eval      TpNr0502    = *Hival
507300000000     C*
507400160202R195 X****               Eval      TpCom0502    = 'E'
507500160202R195 C                   Eval      TpCom0502    = TpComG61
507600160202     C*
507700000000     C                   Eval      Anno0502     = AnnoG61
507800000000     C                   Eval      Mese0502     = 0
507900000000     C*
508000000000     C                   Eval      CF0502      = *Hival
508100000000     C*
508200000000     C                   MoveL     *Hival        Sogg0502
508300131206R141 C                   MoveL     *Hival        Causal0502
508400000000     C*
508500000000     C                   Eval      Stato0502    = *Hival
508600000000     C*
508700000000     C                   Eval      PIva0502     = *Hival
508800000000     C                   Eval      CdFisc0502   = *Hival
508900000000     C*
509000000000     C                   Eval      Quadro0502   = *Hival
509100000000     C                   Eval      TpOpe0502    = *Hival
509200000000     C*
509300000000     C                   Eval      CogRag0502   = *Hival
509400000000     C                   Eval      Nome0502     = *Hival
509500000000     C                   Eval      DtNasc0502   = *Hival
509600000000     C                   Eval      LocNas0502   = *Hival
509700000000     C                   Eval      PrvNas0502   = *Hival
509800000000     C                   Eval      CodUIC0502   = *Hival
509900000000     C                   Eval      Locali0502   = *Hival
510000000000     C                   Eval      IndirE0502   = *Hival
510100000000     C*
510200000000     C                   MoveL     *Hival        COGRAR0502
510300000000     C                   MoveL     *Hival        NOMER0502
510400000000     C                   MoveL     *Hival        DTNASR0502
510500000000     C                   MoveL     *Hival        LOCNAR0502
510600000000     C                   MoveL     *Hival        PRVNAR0502
510700000000     C                   MoveL     *Hival        STATOR0502
510800000000     C                   MoveL     *Hival        CODUIR0502
510900000000     C                   MoveL     *Hival        LOCALR0502
511000000000     C                   MoveL     *Hival        INDIER0502
511100000000     C*
511200000000     C                   MoveL     *Hival        PIVAR0502
511300000000     C                   MoveL     *Hival        CDFISR0502
511400000000     C                   MoveL     *Hival        TPOPER0502
511500000000     C                   MoveL     *Hival        NATOPR0502
511600000000     C                   MoveL     *Hival        LEASIR0502
511700000000     C                   MoveL     *Hival        DTREGR0502
511800000000     C                   MoveL     *Hival        NRREGR0502
511900000000     C                   MoveL     *Hival        SERRER0502
512000000000     C*
512100000000     C                   EndSR
512200131206R141 C************************************************************
512300131206     C* Calcola totale importi rigo quadri
512400131206R141 C************************************************************
512500131206     C     CalcWrkTotale BegSr
512600000000     C*
512700131206     C                   Eval      WrkTotale     = WrkImport01 +
512800131206     C                                             WrkImport02 +
512900131206     C                                             WrkImport03 +
513000131206     C                                             WrkImport04 +
513100131206     C                                             WrkImport05 +
513200131206     C                                             WrkImport06 +
513300131206     C                                             WrkImport07 +
513400131206     C                                             WrkImport08 +
513500131206     C                                             WrkImport09 +
513600131206     C                                             WrkImport10
513700131206     C*
513800131206R141 C                   EndSR
513900131206R141 C************************************************************
514000131206     C* Azzera importi pre totale
514100131206R141 C************************************************************
514200131206     C     AzzeraWrkImp  Begsr
514300131206     C*
514400131206     C                   Z-Add     0             WrkTotale
514500131206     C                   Z-Add     0             WrkImport01
514600131206     C                   Z-Add     0             WrkImport02
514700131206     C                   Z-Add     0             WrkImport03
514800131206     C                   Z-Add     0             WrkImport04
514900131206     C                   Z-Add     0             WrkImport05
515000131206     C                   Z-Add     0             WrkImport06
515100131206     C                   Z-Add     0             WrkImport07
515200131206     C                   Z-Add     0             WrkImport08
515300131206     C                   Z-Add     0             WrkImport09
515400131206     C                   Z-Add     0             WrkImport10
515500131206     C*
515600131206R141 C                   EndSR
515700131206     C************************************************************
515800131206     C* Stampa File telematico
515900131206     C************************************************************
516000131206     C     Stampafile    BegSr
516100131206     C*
516200160204R195 C* Stampa di lavoro
516300160204R195 C                   if        StplG61       ='1'
516400000000     C                   MoveL     SDGSG61DS     KPJBU
516500000000     C*
516600150309R147 X****               CALL      'SDGSG61R1'
516700150309R147 C                   CALL      'SDGSG61R2'
516800000000     C                   PARM                    KPJBA                          *
516900160204R195 C                   endif
517000160204 "   C*
517100160204 "   C* Stampa su modulo (solo per Black List)
517200160204 "   C                   if        StplG61       ='2'
517300160204 "   C                   MoveL     SDGSG61DS     KPJBU
517400160204 "   C*
517500160204 "   C                   CALL      'SDGSG61R3'
517600160204 "   C                   PARM                    KPJBA
517700160204R195 C                   endif
517800160204     C*
517900000000     C*
518000000000     C                   EndSR
518100000000     C************************************************************
518200000000     C* FINE PROGRAMMA
518300000000     C************************************************************
518400000000     C     EndPgm        BegSr
518500000000     C*
518600000000     C                   if        $Errori     = *On
518700000000     C                   Eval      ERRG61      = '1'
518800000000     C                   EndIf
518900000000     C*
519000000000     C                   MoveL     SDGSG61DS     KPJBU
519100000000     C                   SETON                                        LR
519200000000     C*
519300000000     C                   RETURN
519400000000     C*
519500000000     C                   EndSR
519600000000     C************************************************************
519700000000     C* Elabora Errori
519800000000     C************************************************************
519900000000     C     ElaErrori     BEGSR
520000000000     C*
520100000000     C                   EndSr
520200000000     C************************************************************
