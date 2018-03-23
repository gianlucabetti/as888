000100000000     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(*CALLER)
000200000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*DATETIME)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      ********************************************************************
000500000000      *
000600000000      *         ------------------------------------------------
000700000000      *                            CU 2015
000800000000      *                   Creazione file telematico
000900000000      *         ------------------------------------------------
001000000000      *
001100000000     F*----------------------------------------------------------------
001200150305R172 X****FND77007L  IF   E           K DISK
001300150305R172 FND77007L  UF   E           K DISK
001400000000     F*----------------
001500000000     F* File contenente i dati da trasmettere all'agenzia
001600000000     FSDGCUW00F UF A E             DISK    UsrOpn
001700000000     F                                     InfDs(INFO00)
001800000000     F*-------------
001900000000     F* Dati anagrafici aggiuntivi
002000160530R224 X**** Key: Sogg/DtFiVl/Sys/NrAsInt      UNIQUE
002100160530R224 F* Key: Sogg/DtFiVl/Sys/NrAsInt/Dest      UNIQUE
002200000000     FSDGSO301L IF   E           K DISK
002300000000     F*-------------
002400000000     FANSIF01L  IF   E           K DISK
002500000000     D*-------------
002600000000     D* Prototipo per la procedura Euro
002700000000     D/COPY *LIBL/SRCCPY,PJX002PR
002800000000     D*----------------
002900000000     D* Parametro di base
003000000000     D XPAOUT          DS          2000
003100000000     D*----------------
003200000000     D* PARAMETRI ESTERNI
003300000000     D KPJBA         E DS
003400000000     D*----------------
003500000000     D* Valori assunti
003600000000     D XMPEDS        E DS                  INZ
003700000000     D XMPEDS1       E DS                  INZ
003800000000     D DSInterm        DS                  INZ
003900000000     D $IdProd                       16A
004000000000     D $FisInt                       16A
004100000000     D $ImpTrs                        1A
004200000000     D $FirInt                       30A
004300000000     D $ImpDta                       10A
004400000000     D $FisSog                       16A
004500000000     D $CarSog                        2A
004600000000     D $CogSog                       24A
004700000000     D $NomSog                       20A
004800000000     D $FisDic                       11A
004900000000     D $DtFirDic                     10A
005000000000     D $FirDic                       30A
005100000000     D*-------------
005200000000     D DSInterm2       DS                  INZ
005300000000     D $Telefon                      12A
005400000000     D $Email                        69A
005500160530R224 D*-------------
005600160607 "   D DSSostit        DS                  INZ
005700160607R224 D $DestSos                       2A   Inz(*Blank)
005800000000     D*----------------
005900000000     D* Parametri ricevuti
006000000000     D SDGSK61DS     E DS
006100000000     D*-------------
006200000000     D* Dati di ambiente ottenuti da SDGSOCDR
006300000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
006400000000     D*-------------
006500000000     D* DS Interna per dati di output di SDGSOCDR
006600000000     D XSOCDS          S           1000
006700000000     D*----------------
006800000000     D* Reperimento nome PGM
006900000000     D STATUS         SDS           333
007000000000     D  DSPGM            *PROC
007100000000     D*----------------
007200000000     D* Richiamo a SDGTABDR
007300000000     D SDGTABDS      E DS                  inz
007400000000     D*----------------
007500000000     D* tabelle delle nature giuridiche
007600000000     D ANGG16DS      E DS                  INZ
007700000000     D*------------
007800000000     D* Immagine SDGCUW00F
007900000000     D SDGCUWDS      E DS                  ExtName(SDGCUW00F)
008000000000     D*------------
008100000000     D* Ds tracciati appoggio
008200000000     D SDGSK61DSA    E DS
008300000000     D SDGSK61DSB    E DS
008400000000     D SDGSK61DSD    E DS
008500000000     D SDGSK61DSH    E DS
008600000000     D SDGSK61DSZ    E DS
008700000000     D*-------------
008800000000     D Info00          DS           512
008900000000     D  NrrCuw               397    400B 0
009000000000     D*----------------
009100150305R172 X**** Cnt_Righe       S              7S 0 Inz(*Zero)
009200150305R172 D Cnt_Righe       S              7S 0 Inz(1)
009300000000     D*
009400000000     D Cnt_D           S              7S 0 Inz(*Zero)
009500000000     D CntDA           S              7S 0 Inz(*Zero)
009600000000     D Prog_DA         S              3S 0 Inz(*Zero)
009700000000     D*
009800000000     D Cnt_H           S              7S 0 Inz(*Zero)
009900000000     D CntAU           S              7S 0 Inz(*Zero)
010000000000     D Prog_AU         S              3S 0 Inz(*Zero)
010100000000     D*----------------
010200000000     D* Salvataggio KEY
010300000000     D SavKey          DS
010400000000     D SavCdf                              Like(N77Cdf    )
010500000000     D SavCde                              Like(N77Cde    )
010600000000     D SavC77                              Like(N77C77    )
010700160208R211 D SavOSCDF1                           Like(N77OSCDF1 )
010800160208R211 D SavOSCDF2                           Like(N77OSCDF2 )
010900160208R211 D SavOSCDF3                           Like(N77OSCDF3 )
011000000000     D
011100000000     D* Nuova       KEY
011200000000     D NewKey          DS
011300000000     D N77Cdf
011400000000     D N77Cde
011500000000     D N77C77
011600160208R211 D N77OSCDF1
011700160208R211 D N77OSCDF2
011800160208R211 D N77OSCDF3
011900000000     D*----------------
012000000000     D* campi di lavoro
012100000000     D $NrRecTot       S              9S 0 Inz(*Zero)
012200000000     D SavNrrCuw       S              9S 0 Inz(*Zero)
012300150306R172 D SavProtCea      S                   Like(N77ProtCea)
012400000000     D*----------------
012500000000     D WImp004         S             19S 3
012600000000     D WImp005         S             19S 3
012700150224R169 D WImp0051        S             19S 3
012800000000     D WImp006         S             19S 3
012900000000     D WImp007         S             19S 3
013000150224R169 D WImp0071        S             19S 3
013100000000     D WImp008         S             19S 3
013200000000     D WImp009         S             19S 3
013300000000     D WImp010         S             19S 3
013400000000     D WImp011         S             19S 3
013500000000     D WImp012         S             19S 3
013600000000     D WImp013         S             19S 3
013700000000     D WImp014         S             19S 3
013800000000     D WImp015         S             19S 3
013900000000     D WImp016         S             19S 3
014000000000     D WImp017         S             19S 3
014100000000     D WImp018         S             19S 3
014200000000     D WImp019         S             19S 3
014300000000     D WImp020         S             19S 3
014400000000     D WImp021         S             19S 3
014500000000     D WImp022         S             19S 3
014600000000     D WImp023         S             19S 3
014700160129R211 D WImp034         S             19S 3
014800160129  "  D WImp035         S             19S 3
014900160129  "  D WImp037         S             19S 3
015000160129  "  D WImp038         S             19S 3
015100160129  "  D WImp039         S             19S 3
015200160129  "  D WImp041         S             19S 3
015300160129  "  D WImp042         S             19S 3
015400160129  "  D WImp053         S             19S 3
015500160129  "  D WImp054         S             19S 3
015600160129  "  D WImp055         S             19S 3
015700160129  "  D WImp056         S             19S 3
015800160129  "  D WImp057         S             19S 3
015900160129  "  D WImp058         S             19S 3
016000160129  "  D WImp059         S             19S 3
016100160129  "  D WImp060         S             19S 3
016200160129  "  D WImp061         S             19S 3
016300160129  "  D WImp062         S             19S 3
016400160129  "  D WImp102         S             19S 3
016500170125R211 D WImp103         S             19S 3
016600170125R250 D WImp104         S             19S 3
016700170125R211 D WImp106         S             19S 3
016800170125R211 D WImp107         S             19S 3
016900170125R250 D WImp108         S             19S 3
017000170125R211 D WImp131         S             19S 3
017100160129  "  D WImp132         S             19S 3
017200160129  "  D WImp133         S             19S 3
017300160129  "  D WImp134         S             19S 3
017400160129  "  D WImp135         S             19S 3
017500160129  "  D WImp136         S             19S 3
017600160129  "  D WImp137         S             19S 3
017700160201  "  D WImp138         S             19S 3
017800160201  "  D*----------------
017900160201  "  D* Campi alfabetici da salvare
018000160201  "  D*  Dati previdenziali
018100160201  "  D WDpCdf          S                   Like(N77DpCdf)
018200160201  "  D WDpDen          S                   Like(N77DpDen)
018300160201  "  D WDpCenP         S                   Like(N77DpCenP)
018400160201  "  D WDpCazi         S                   Like(N77DpCazi)
018500160201  "  D WDpCate         S                   Like(N77DpCate)
018600160201  "  D*  Altri contributi
018700160201  "  D WDpAcon         S                   Like(N77DpAcon)
018800160201  "  D*  Redditi erogati da altri soggetti
018900160201  "  D WAsCdf          S                   Like(N77AsCdf)
019000160201  "  D*  Casi particolari operazioni straordinarie
019100160201  "  D WOsCdf1         S                   Like(N77OsCdf1)
019200160201  "  D WOsCdf2         S                   Like(N77OsCdf2)
019300160201  "  D WOsCdf3         S                   Like(N77OsCdf3)
019400160201  "  D*  Somme liquidate a seguito di pignoramento presso terzi
019500170125R211 D WF2Cdf          S                   Like(N77F2Cdf)
019600170125R250 X*** ex r211 WF2Rino         S                   Like(N77F2Rino)
019700170125R211 D WF2Cdf2         S                   Like(N77F2Cdf2)
019800170125R250 X*** ex r211 WF2Rino2        S                   Like(N77F2Rino2)
019900000000     D*----------------
020000000000     D WImporto        S             19S 3
020100000000     D Importo16_0     S             16S 0
020200000000     D Importo16_2     S             16S 2
020300000000     D Importo16A      S             16A
020400000000     D $FlgNeg         S              1A   Inz('0')
020500150309R172 D $RcdOk          S              1A
020600000000     D*-------------
020700000000     D CampoWrk        DS
020800000000     D CampoInt                1     14
020900000000     D CampoDec               15     16
021000000000     D*
021100000000     D CampoIntero     DS
021200000000     D  Int                    1     13
021300000000     D  virgola               14     14
021400000000     D  Dec                   15     16
021500000000     D*-------------
021600000000     D SavCodice       S                   Like(N77Codice )
021700000000     D UDateISO        S               D
021800000000     D $TpErrore       S              2A
021900000000     D SoloPrimaVolta  S              1A   Inz('1')
022000000000     D $Errori         S              1A   Inz('0')
022100000000     D $Fine770        S              1A   Inz('0')
022200000000     D $FlDatiAna      S              1A   Inz('0')
022300000000     D $FlDatiImp      S              1A   Inz('0')
022400000000     D*-------------
022500000000     D $CdFiscSog      S                   Like(SO3CdFisc )
022600000000     D $CdIVASog       S                   Like(SO3PartIva )
022700000000     D $CogRagSog      S                   Like(SO3CogRag)
022800000000     D $NomeSog        S                   Like(SO3Nome )
022900000000     D $DtNascSog      S                   Like(SO3DtNasc)
023000000000     D $LocNascSog     S                   Like(SO3LocNasc)
023100000000     D $PrvNascSog     S                   Like(SO3PrvNasc)
023200000000     D $StatoSog       S                   Like(SO3Stato   )
023300000000     D $StaFedSog      S                   Like(SO3StaFed  )
023400000000     D $LocalitSog     S                   Like(SO3Localit)
023500000000     D $IndirESog      S                   Like(SO3IndirE  )
023600000000     D $SexSog         S              1A
023700000000     D*------
023800000000     D $CdFisc         S                   Like(SO3CdFisc)
023900000000     D $PartIVA        S                   Like(SO3PartIva)
024000000000     D WrKNatGiu       S              2A
024100000000     D WSavQuaD        S              2A
024200000000     D WSavQuaH        S              2A
024300000000     D*----------------
024400000000     D* Ds flag ansif
024500000000     D                 DS
024600000000     D  SocSogg               31     38
024700000000     D  WrkFlgOpe              1     50
024800000000     D*----------------
024900000000     D* Ds per nome file
025000000000     D                 DS
025100000000     D  WAnno                  3      6  0
025200000000     D  WPer                   7      8
025300000000     D  WPrgInvio              9     10  0
025400000000     D  WNumero                1     10
025500000000     D*--
025600000000     D* ogni elemento della tabella Skiera è costituito da:
025700000000     D* - 1 campo-codice di 8 caratteri;
025800000000     D* - 1 campo-valore di 16 caratteri.
025900000000     D*
026000000000     D                 DS
026100000000     D  Cod_Elenco             1      2
026200000000     D  Cod_Rigo               3      5  0
026300000000     D  Cod_Colo               6      8  0
026400000000     D  Campo_Codice           1      8
026500000000     D*
026600000000     D                 DS
026700000000     D  Campo                  1     16
026800000000     D  C15I                  17     61
026900000000     D    C15                        15    Dim(03) Overlay(C15I)
027000000000     D  C60                    1     61
027100000000     D*-------------
027200000000     D* Elementi della variabile CAMPO
027300000000     D Conta_Rigo      S              3S 0 Inz(*Zero)
027400000000     D Conta_Colo      S              3S 0 Inz(*Zero)
027500000000     D*-------------
027600000000     D* tabella per i dati NON posizionali del record D
027700000000     D Max$I           C                   CONST(100)
027800000000     D Skiera          S             24    DIM(Max$I)
027900000000     D $I              S              3S 0
028000000000     D*-------------
028100000000     D* tabella per i dati NON posizionali del record H
028200000000     D Max$H           C                   CONST(100)
028300000000     D SkieraH         S             24    DIM(Max$H)
028400000000     D $H              S              3S 0
028500000000     D*--
028600000000     D Sk_Campo        S              1    DIM(Max$X)
028700000000     D Max$X           C                   CONST(61)
028800000000     D $X              S              3S 0
028900000000     D*----------------
029000000000     D* questa schiera serve per togliere gli zeri non significativi
029100000000     D* alle caselle che contengono degli importi dei campi posizionali
029200000000     D Max$Z           C                   CONST(16)
029300000000     D $Z              S              3S 0
029400000000     D*
029500000000     D Sk_Importo      S              1    DIM(Max$Z)
029600000000     D*---------------------------------------------------
029700000000     D* Elemento tabella Skiera :
029800000000     D* campo 16 caratteri (valore)
029900000000     D                 DS
030000000000     D  Campo_Valore           1     16
030100000000     D  Caratt_45             17     61
030200000000     D*-------------
030300000000     D* variabili per invio multiplo
030400000000     D $MultiInvio     S              1A   Inz(*Off)
030500000000     D $NrInvii        S              5S 0 Inz(0)
030600000000     D $ProgrInvio     S              5S 0 Inz(0)
030700000000     D*-------------
030800000000     D WImpInp         S                   Like(ImporD0202)
030900000000     D WImpOut         S                   Like(ImporD0202)
031000000000     D*-------------
031100000000     D DtAMG           DS
031200000000     D  AMGAnno                1      4
031300000000     D  T1                     5      5
031400000000     D  AMGMese                6      7
031500000000     D  T2                     8      8
031600000000     D  AMGGiorno              9     10
031700000000     D*-------------
031800000000     D DtGMA           DS
031900000000     D  GMAGiorno              1      2  0
032000000000     D  GMAMese                3      4  0
032100000000     D  GMAAnno                5      8  0
032200000000     D*-------------
032300000000     D WrkDt           S              8A
032400000000     D*---------------------------------------------------------------*
032500000000     D DatMin          C                   CONST('0001-01-01')
032600000000     D CostFile        C                   CONST('Certificaz. Unica')
032700000000     C******************************************************
032800000000     C*                MAIN LINE PROGRAM
032900000000     C******************************************************
033000000000     C*
033100000000     C                   EXSR      INZVAR
033200000000     C*
033300000000     C* Dati Rappresentante
033400000000     C                   EXSR      RepDatInter
033500000000     C                   Exsr      RepDatInter2
033600000000     C*
033700000000     C* Azzeramento
033800000000     C                   EXSR      AzzeraProg
033900000000     C*
034000000000     C*-----------------------------------------------
034100000000     C* LETTURA DATI
034200000000     C*
034300000000     C* Elaborazione dati per preparazione invio
034400000000     C                   EXSR      ElaboraDati
034500000000     C*
034600000000     C* Determino nr invii
034700000000     C                   EXSR      RepNrInvii
034800000000     C*
034900000000     C                   if        $NrInvii      > 1
035000000000     C                   Eval      $MultiInvio    = *On
035100000000     C                   else
035200000000     C                   Eval      $MultiInvio    = *Off
035300000000     C                   endif
035400000000     C*
035500000000     C*-----------------------------------------------
035600000000     C*
035700000000     C* ELABORAZIONE DATI per SCRITTURA TELEMATICO ---
035800000000     C*
035900000000     C*
036000000000     C* Se ho record allora continuo
036100000000     C                   if        $NrRecTot     > 0
036200000000     C*
036300000000     C* Azzeramento
036400000000     C                   EXSR      AzzeraProg
036500000000     C*
036600000000     C* Unico invio  o multi invio
036700000000     C                   if        $MultiInvio    = *Off
036800000000     C                   Z-Add     1             $ProgrInvio
036900000000     C                   EXSR      GesSingInv
037000000000     C                   else
037100000000     C                   EXSR      GesMultInv
037200000000     C                   endif
037300000000     C*
037400000000     C                   endif
037500000000     C*
037600000000     C                   EXSR      ENDPGM
037700000000     C*
037800000000     C/EJECT
037900000000     C************************************************************
038000000000     C* FINE PROGRAMMA
038100000000     C************************************************************
038200000000     C     ENDPGM        BEGSR
038300000000     C*
038400000000     C                   if        $Errori     = *On
038500000000     C                   Eval      ERRK61      = '1'
038600000000     C                   endif
038700000000     C*
038800000000     C                   MoveL     SDGSK61DS     KPJBU
038900000000     C                   SETON                                        LR
039000000000     C*
039100000000     C                   RETURN
039200000000     C*
039300000000     C                   ENDSR
039400000000     C/EJECT
039500000000     C************************************************************
039600000000     C* Reperimento dati Intermediario
039700000000     C************************************************************
039800000000     C     RepDatInter   BEGSR
039900000000     C*
040000000000     C* preparo i parametri di chiamata al driver
040100000000     C                   Clear                   XMPEDS
040200000000     C                   EVAL      TpChiamata = '01'
040300000000     C                   EVAL      NChiavi = 5
040400000000     C                   EVAL      VLogica = 2
040500000000     C*
040600000000     C* preparo le chiavi per la chiamata al driver
040700000000     C                   Clear                   XMPEDS1
040800000000     C                   MoveL     XSCSOC        DSMPESOC
040900000000     C                   MoveL     'SDGSK60R R'  DSMPEPGM
041000000000     C                   MoveL     *Blanks       DSMPEPRF
041100000000     C* Si reperiscono i dati di default a seconda delle modalità con
041200000000     C* cui il pgm è usato
041300000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
041400000000     C* MPETIP = 'PV'  pgm richiamato da menù
041500000000     C* MPETIP = 'PB'  x lancio Batch
041600000000     C                   MoveL     'PB'          DSMPETIP
041700000000     C*
041800000000     C                   CALLB     'XMPE'
041900000000     C                   PARM                    XMPEDS
042000000000     C                   PARM                    XMPEDS1
042100000000     C*
042200000000     C     Risultato     IfEq      '0'
042300000000     C                   MoveL     DSMPEPAR      DSInterm
042400000000     C                   endif
042500000000     C*
042600000000     C                   ENDSR
042700000000     C/EJECT
042800000000     C************************************************************
042900000000     C* Reperimento dati Intermediario
043000000000     C************************************************************
043100000000     C     RepDatInter2  BEGSR
043200000000     C*
043300000000     C* preparo i parametri di chiamata al driver
043400000000     C                   Clear                   XMPEDS
043500000000     C                   EVAL      TpChiamata = '01'
043600000000     C                   EVAL      NChiavi = 5
043700000000     C                   EVAL      VLogica = 2
043800000000     C*
043900000000     C* preparo le chiavi per la chiamata al driver
044000000000     C                   Clear                   XMPEDS1
044100000000     C                   MoveL     XSCSOC        DSMPESOC
044200000000     C                   MoveL     'SDGSK60R S'  DSMPEPGM
044300000000     C                   Move      'S'           DSMPEPGM
044400000000     C                   MoveL     *blanks       DSMPEPRF
044500000000     C* Si reperiscono i dati di default a seconda delle modalità con
044600000000     C* cui il pgm è usato
044700000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
044800000000     C* MPETIP = 'PV'  pgm richiamato da menù
044900000000     C* MPETIP = 'PB'  x lancio Batch
045000000000     C                   MoveL     'PB'          DSMPETIP
045100000000     C*
045200000000     C                   CALLB     'XMPE'
045300000000     C                   PARM                    XMPEDS
045400000000     C                   PARM                    XMPEDS1
045500000000     C*
045600000000     C     Risultato     IfEq      '0'
045700000000     C                   MoveL     DSMPEPAR      DSInterm2
045800000000     C                   EndIf
045900000000     C*
046000000000     C                   EndSR
046100000000     C************************************************************
046200000000     C* Azzeramento progressivi
046300000000     C************************************************************
046400000000     C     AzzeraProg    BEGSR
046500000000     C*
046600000000     C                   Z-Add     0             Cnt_D
046700000000     C                   Z-Add     0             Cnt_H
046800000000     C*
046900000000     C                   Z-Add     0             CntDA
047000000000     C                   Z-Add     0             CntAU
047100000000     C*
047200000000     C                   Z-Add     0             Prog_DA
047300000000     C                   Z-Add     0             Prog_AU
047400000000     C*
047500000000     C                   Z-Add     0             $NrRecTot
047600000000     C*
047700000000     C                   ENDSR
047800000000     C/EJECT
047900000000     C******************************************************
048000000000     C* Elaboro dati di invio per contare il numero di record
048100000000     C******************************************************
048200000000     C     ElaboraDati   BEGSR
048300000000     C*
048400000000     C                   EXSR      Inz770
048500000000     C                   EXSR      Set770
048600000000     C*
048700000000     C* Conto quanti record per calcolare il numero di invii
048800000000     C                   DOW       $Fine770    = *Off
048900000000     C                   EXSR      Lettura770
049000000000     C*
049100000000     C                   if        N77Escludi <> '1'
049200150306R172 C                             and
049300150309 "   C                             (ProtocK61  =  0 and
049400150309 "   C                              ProtoDK61  =  0 or
049500150309R172 C                              $RcdOk = *on)
049600150316R172 C                             or $Fine770  = *On
049700000000     C****                         and N77Cde  =  *blank
049800000000     C****                         and N77Num  =  *blank
049900000000     C                   if        SoloPrimaVolta = *On
050000000000     C                   EXSR      ElaboraKey
050100000000     C                   endif
050200000000     C*
050300000000     C                   if        SavKey   <> NewKey  or
050400000000     C                             $Fine770  = *On
050500000000     C                   Add       1             $NrRecTot
050600000000     C                   Add       1             CntDA
050700000000     C                   Add       1             CntAU
050800000000     C                   EXSR      ElaboraKey
050900000000     C                   endif
051000000000     C                   endif
051100000000     C*
051200000000     C                   endDo
051300000000     C*
051400000000     C                   ENDSR
051500000000     C/EJECT
051600000000     C******************************************************
051700000000     C* Inizializzo campi
051800000000     C******************************************************
051900000000     C     Inz770        BEGSR
052000000000     C*
052100000000     C                   Eval      $Fine770    = *Off
052200000000     C                   Clear                   NewKey
052300000000     C*
052400000000     C                   Eval      WImp004    =  0
052500000000     C                   Eval      WImp005    =  0
052600150224R169 C                   Eval      WImp0051   =  0
052700000000     C                   Eval      WImp006    =  0
052800000000     C                   Eval      WImp007    =  0
052900150224R169 C                   Eval      WImp0071   =  0
053000000000     C                   Eval      WImp008    =  0
053100000000     C                   Eval      WImp009    =  0
053200000000     C                   Eval      WImp010    =  0
053300000000     C                   Eval      WImp011    =  0
053400000000     C                   Eval      WImp012    =  0
053500000000     C                   Eval      WImp013    =  0
053600000000     C                   Eval      WImp014    =  0
053700000000     C                   Eval      WImp015    =  0
053800000000     C                   Eval      WImp016    =  0
053900000000     C                   Eval      WImp017    =  0
054000000000     C                   Eval      WImp018    =  0
054100000000     C                   Eval      WImp019    =  0
054200000000     C                   Eval      WImp020    =  0
054300000000     C                   Eval      WImp021    =  0
054400160129R211 X***                Eval      WImp022    =  0
054500160129R211 X***                Eval      WImp023    =  0
054600160129R211 C                   Eval      WImp034    =  0
054700160129  "  C                   Eval      WImp035    =  0
054800160129  "  C                   Eval      WImp037    =  0
054900160129  "  C                   Eval      WImp038    =  0
055000160129  "  C                   Eval      WImp039    =  0
055100160129  "  C                   Eval      WImp041    =  0
055200160129  "  C                   Eval      WImp042    =  0
055300160129  "  C                   Eval      WImp053    =  0
055400160129  "  C                   Eval      WImp054    =  0
055500160129  "  C                   Eval      WImp055    =  0
055600160129  "  C                   Eval      WImp056    =  0
055700160129  "  C                   Eval      WImp057    =  0
055800160129  "  C                   Eval      WImp058    =  0
055900160129  "  C                   Eval      WImp059    =  0
056000160129  "  C                   Eval      WImp060    =  0
056100160129  "  C                   Eval      WImp061    =  0
056200160129  "  C                   Eval      WImp062    =  0
056300160129  "  C                   Eval      WImp102    =  0
056400170125R211 C                   Eval      WImp103    =  0
056500170125R250 C                   Eval      WImp104    =  0
056600170125R211 C                   Eval      WImp106    =  0
056700170125R211 C                   Eval      WImp107    =  0
056800170125R250 C                   Eval      WImp108    =  0
056900170125R211 C                   Eval      WImp131    =  0
057000160129  "  C                   Eval      WImp132    =  0
057100160129  "  C                   Eval      WImp133    =  0
057200160129  "  C                   Eval      WImp134    =  0
057300160129  "  C                   Eval      WImp135    =  0
057400160129  "  C                   Eval      WImp136    =  0
057500160129  "  C                   Eval      WImp137    =  0
057600160201  "  C                   Eval      WImp138    =  0
057700160201  "  C*
057800160201  "  C                   Clear                   WDpCdf
057900160201  "  C                   Clear                   WDpDen
058000160201  "  C                   Clear                   WDpCenP
058100160201  "  C                   Clear                   WDpCazi
058200160201  "  C                   Clear                   WDpCate
058300160201  "  C                   Clear                   WDpAcon
058400160201  "  C                   Clear                   WAsCdf
058500160201  "  C                   Clear                   WOsCdf1
058600160201  "  C                   Clear                   WOsCdf2
058700160201  "  C                   Clear                   WOsCdf3
058800170125r211 C                   Clear                   WF2Cdf
058900170125R250 X*** ex r211        Clear                   WF2Rino
059000170125r211 C                   Clear                   WF2Cdf2
059100170125R250 X*** ex r211        Clear                   WF2Rino2
059200000000     C*
059300000000     C                   ENDSR
059400000000     C/EJECT
059500000000     C************************************************************
059600000000     C* inz ds
059700000000     C************************************************************
059800000000     C     Set770        BEGSR
059900000000     C*
060000000000     C                   Eval      SoloPrimaVolta = *On
060100000000     C*
060200000000     C                   Eval      N77Societa  = SocietaK61
060300000000     C                   Eval      N77Acf      = AnnoK61
060400000000     C     K0277007      setll     Nd77007l
060500000000     C*
060600000000     C                   ENDSR
060700000000     C/EJECT
060800000000     C******************************************************
060900000000     C* Ciclo dei dati estratti
061000000000     C******************************************************
061100000000     C     Lettura770    BEGSR
061200000000     C*
061300000000     C     K0277007      reade     Nd77007l
061400000000     C                   if        %eof(Nd77007l)
061500000000     C                   eval      $Fine770  = *On
061600000000     C                   endif
061700150309R172 C*
061800150309R172 C                   exsr      Sel770
061900000000     C*
062000000000     C                   ENDSR
062100000000     C/EJECT
062200150309R172 C******************************************************
062300150309 "   C* Selezione dei record
062400150309R172 C******************************************************
062500150309     C     Sel770        BEGSR
062600150309     C*
062700150309     C                   eval      $RcdOk = *on
062800150309     C*
062900150309     C                   if        ProtocK61  <> 0 and
063000150309     C                             ProtoDK61  =  0 and
063100150309     C                             N77Protoc  <> ProtocK61
063200150309     C                   eval      $RcdOk      = *off
063300150309     C                   endif
063400150309     C*
063500150309     C                   if        ProtocK61  <> 0          and
063600150309     C                             ProtoDK61  <> 0          and
063700150309     C                             (N77Protoc  <> ProtocK61 or
063800150309     C                              N77ProtCea <> ProtoDK61)
063900150309     C                   eval      $RcdOk      = *off
064000150309     C                   endif
064100170307R2523C*
064200170307  "  C                   if        ProtocK61  =  0 and
064300170307  "  C                             ProtoDK61  =  0 and
064400170307  "  C                             N77Protoc  <> 0
064500170307  "  C                   eval      $RcdOk      = *off
064600170307R2523C                   endif
064700150309     C*
064800150309R172 C                   ENDSR
064900150309R172 C/EJECT
065000150305R172 C******************************************************
065100150305 "   C* Aggiorna record 770 con progressivo certificazione
065200150305R172 C******************************************************
065300150305     C     Aggiorna770   BEGSR
065400150305     C*
065500150305     C                   if        $Fine770  = *Off
065600150305     C                   z-add     Cnt_Righe     N77ProtCei
065700150305     C                   update    Nd770000
065800150305     C                   endif
065900150305     C*
066000150305R172 C                   ENDSR
066100150305R172 C/EJECT
066200000000     C******************************************************
066300000000     C* Salvataggio chiavi
066400000000     C******************************************************
066500000000     C     ElaboraKey    BEGSR
066600000000     C*
066700000000     C                   eval      SavCdf      = N77Cdf
066800000000     C                   eval      SavCde      = N77Cde
066900000000     C                   eval      SavC77      = N77C77
067000160208R211 C                   eval      SavOSCDF1   = N77OSCDF1
067100160208R211 C                   eval      SavOSCDF2   = N77OSCDF2
067200160208R211 C                   eval      SavOSCDF3   = N77OSCDF3
067300000000     C                   eval      SavCodice   = N77Codice
067400150306R172 C                   eval      SavProtCea  = N77ProtCea
067500000000     C*
067600000000     C                   Eval      SoloPrimaVolta = *Off
067700000000     C*
067800000000     C                   ENDSR
067900000000     C/EJECT
068000000000     C************************************************************
068100000000     C* Reperimento numero max di invii
068200000000     C************************************************************
068300000000     C     RepNrInvii    BEGSR
068400000000     C*
068500000000     C                   if        $NrRecTot   > 0
068600000000     C                   Z-Add     $NrRecTot     WImpInp
068700000000     C                   CallP     X0201MultC(WImpInp: 1: MaxRecK61:
068800000000     C                                        '+': '0': 0: WImpOut)
068900000000     C                   Z-Add     WImpOut       $NrInvii
069000000000     C                   endif
069100000000     C*
069200000000     C                   ENDSR
069300000000     C/EJECT
069400000000     C******************************************************
069500000000     C* Gestione singolo invio
069600000000     C******************************************************
069700000000     C     GesSingInv    BEGSR
069800000000     C*
069900000000     C                   EXSR      RepNomeF
070000000000     C                   EXSR      GesFilTel
070100000000     C*
070200000000     C                   OPEN      SDGCUW00F
070300000000     C*
070400000000     C* Record A  testata della fornitura
070500000000     C                   EXSR      WrtTpRecA
070600000000     C*
070700000000     C* Record B  testata della fornitura
070800000000     C                   EXSR      WrtTpRecB
070900000000     C*
071000000000     C                   EXSR      Inz770
071100000000     C                   EXSR      Set770
071200000000     C*
071300000000     C                   DOW       $Fine770    = *Off
071400000000     C                   EXSR      Lettura770
071500000000     C*
071600000000     C                   if        N77Escludi <> '1'
071700150306R172 C                             and
071800170308R2523x*** ex r172                  (ProtocK61  =  0 and
071900170308R2523x*** ex r172                   ProtoDK61  =  0 or
072000170308R2523x*** ex r172                   $RcdOk = *on)
072100170308R2523C                             $rcdOk = *on
072200150310R172 C                             or $Fine770  = *On
072300000000     C****                         and N77Cde  =  *blank
072400000000     C****                         and N77Num  =  *blank
072500000000     C                   if        SoloPrimaVolta = *On
072600000000     C                   EXSR      ElaboraKey
072700000000     C                   endif
072800000000     C*
072900000000     C                   if        SavKey        <> NewKey  or
073000000000     C                             $Fine770  = *On
073100000000     C                   if        $I         <> 0
073200000000     C*
073300000000     C                   EXSR      WrtTpRecD
073400000000     C                   EXSR      InzSkieraD
073500000000     C*
073600150305R172 C                   if        TipInvK61 <> 2
073700000000     C                   EXSR      WrtTpRecH
073800000000     C                   EXSR      InzSkieraH
073900150305R172 C                   endif
074000000000     C*
074100000000     C                   Add       1             $NrRecTot
074200000000     C                   Add       1             CntDA
074300000000     C                   Add       1             CntAU
074400000000     C                   EXSR      ElaboraKey
074500000000     C                   endif
074600000000     C                   endif
074700000000     C*
074800000000     C                   EXSR      ElaboraRecD
074900000000     C*
075000150305R172 C                   if        TipInvK61 <> 2
075100000000     C                   EXSR      ElaboraRecH
075200150305R172 C                   endif
075300150305R172 C*
075400150305R172 C                   EXSR      Aggiorna770
075500000000     C                   endif
075600000000     C*
075700000000     C                   enddo
075800000000     C*
075900000000     C* Aggiorna record B con totale comunicazioni relative
076000000000     C* a certificazioni lavoro autonomo
076100000000     C                   EXSR      UpdTpRecB
076200000000     C*
076300000000     C* Record Z di coda della fornitura
076400000000     C                   EXSR      WrtTpRecZ
076500000000     C*
076600000000     C                   EXSR      IscrCatF
076700000000     C*
076800000000     C                   CLOSE     SDGCUW00F
076900000000     C*
077000000000     C                   ENDSR
077100000000     C/EJECT
077200000000     C************************************************************
077300000000     C* Reperimento nome file
077400000000     C************************************************************
077500000000     C     RepNomeF      BEGSR
077600000000     C*
077700000000     C                   Eval      WNumero     = *All'0'
077800000000     C                   Z-Add     AnnoK61       WAnno
077900000000     C                   Z-Add     $ProgrInvio   WPrgInvio
078000000000     C                   Move      WNumero       $Numero
078100000000     C*
078200000000     C                   CALL      'X35NOMFR'
078300000000     C                   PARM                    SocietaK61
078400000000     C                   PARM      *Blank        $Unita            8
078500000000     C                   PARM      'CU'          $CdScarico        2
078600000000     C                   PARM                    $Numero           9
078700000000     C                   PARM                    $File            10
078800000000     C                   PARM                    $Libreria        10
078900000000     C                   PARM                    $Errore           1
079000000000     C*
079100000000     C                   if        $Errore    <> '0'
079200000000     C                   Eval      $TpErrore   = '10'
079300000000     C                   EXSR      StpErrore
079400000000     C                   EXSR      ENDPGM
079500000000     C                   endif
079600000000     C*
079700000000     C                   ENDSR
079800000000     C/EJECT
079900000000     C************************************************************
080000000000     C* Gestione file per invio telematico
080100000000     C************************************************************
080200000000     C     GesFilTel     BEGSR
080300000000     C*
080400000000     C* Creo già la descrizione del file
080500000000     C                   Clear                   $DesFileO
080600000000     C*
080700000000     C                   Move      AnnoK61       AnnoA             4
080800000000     C                   MoveL     AnnoA         $DesFileO
080900000000     C*
081000000000     C                   Cat       CostFile:1    $DesFileO
081100000000     C                   Cat       XScSoc :1     $DesFileO
081200000000     C*
081300000000     C                   Move      UDateIso      $DtAlfa
081400000000     C*
081500000000     C                   Eval      $Libreria   = LibK61
081600000000     C                   Eval      $Errore     = *Off
081700000000     C*
081800000000     C                   CALL      'SDGSK61C1'
081900000000     C                   PARM                    $File
082000000000     C                   PARM                    $Libreria
082100000000     C                   PARM                    WTpDist           2
082200000000     C                   PARM                    $DtAlfa          10
082300000000     C                   PARM                    $DesFileI         8
082400000000     C                   PARM                    $DesFileO        50
082500000000     C                   PARM                    $Errore           1
082600000000     C*
082700000000     C                   if        $Errore    <> '0'
082800000000     C                   Eval      $TpErrore   = '11'
082900000000     C                   EXSR      StpErrore
083000000000     C                   EXSR      ENDPGM
083100000000     C                   endif
083200000000     C*
083300000000     C                   ENDSR
083400000000     C/EJECT
083500000000     C******************************************************
083600000000     C* Riempimento dati per t.r. A
083700000000     C******************************************************
083800000000     C     WrtTpRecA     BEGSR
083900000000     C*
084000000000     C                   Clear                   SDGCUW000
084100000000     C                   Clear                   SDGSK61DSA
084200000000     C*
084300000000     C*-----------------------------------
084400000000     C* DATI IDENTIFICATIVI DELLA FORNITURA
084500000000     C*
084600000000     C*-
084700000000     C* Tipo record
084800000000     C                   MoveL     'A'           K61ATR
084900000000     C*-
085000000000     C* Codice identificativo della fornitura
085100160205R211 X****               MoveL     'CUR15'       K61AFOR
085200170125R250 X***  ex r211       MoveL     'CUR16'       K61AFOR
085300180123R279 X***  ex R250       MoveL     'CUR17'       K61AFOR
085400180123R279 C                   MoveL     'CUR18'       K61AFOR
085500000000     C*-
085600000000     C* Tipo fornitore
085700000000     C*      "01" Soggetti che inviano la propria
085800000000     C*      "10" Intermediari
085900000000     C                   MoveL     '01'          K61ATFO
086000000000     C                   if        $FisInt       <> *Blanks
086100000000     C                   MoveL     '10'          K61ATFO
086200000000     C                   endif
086300000000     C*-
086400000000     C* Codice fiscale del contribuente
086500000000     C                   MoveL     XScCdF        K61ACFI
086600000000     C                   if        $FisInt       <> *Blanks
086700000000     C                   Move      $FisInt       K61ACFI
086800000000     C                   endif
086900000000     C*
087000000000     C*-----------------------------------
087100000000     C*-
087200000000     C* Carattere di controllo
087300000000     C                   MoveL     'A'           K61AK1
087400000000     C*-
087500000000     C* Elementi di fine riga
087600000000     C* Impostare i valori esadecimali '0D' e '0A'
087700000000     C* (caratteri ASCI 'CR' ed 'LF')
087800000000     C*
087900000000     C                   Eval      K61AK2  = *Blanks
088000000000     C                   Eval      K61AK2  = X'0D25'
088100000000     C*
088200000000     C                   MoveL     SDGSK61DSA    CUWREC
088300000000     C                   write     SDGCUW000
088400000000     C                   feod      SDGCUW00F
088500000000     C*
088600000000     C                   ENDSR
088700000000     C/EJECT
088800000000     C******************************************************
088900000000     C* Riempimento dati per t.r. B
089000000000     C******************************************************
089100000000     C     WrtTpRecB     BEGSR
089200000000     C*
089300000000     C                   Clear                   SDGCUW000
089400000000     C                   Clear                   SDGSK61DSB
089500000000     C*
089600000000     C* Tipo record
089700000000     C                   MoveL     'B'           K61BTR
089800000000     C*-
089900000000     C* Codice fiscale del contribuente
090000000000     C                   MoveL     XScCdF        K61BCFI
090100000000     C* Progressivo modulo
090200000000     C                   Z-Add     1             K61BPRGMOD
090300000000     C*-
090400000000     C* Identificativo produttore software
090500000000     C                   MoveL     $IdProd       K61BIPS
090600000000     C*-
090700000000     C* Tipo Comunicazione
090800000000     C                   Move      '0'           K61BCSOS
090900000000     C                   Move      '0'           K61BCANN
091000000000     C*-
091100000000     C                   Select
091200000000     C**                 When      TIPINVK61     = 0
091300000000     C**                 Move      '1'           K61BCORD
091400000000     C* comunicazione da sostituire
091500000000     C                   When      TIPINVK61     = 1
091600000000     C                   Move      '1'           K61BCSOS
091700000000     C* comunicazione da annullare
091800000000     C                   When      TIPINVK61     = 2
091900000000     C                   Move      '1'           K61BCANN
092000000000     C                   EndSl
092100000000     C*
092200000000     C*
092300000000     C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
092400000000     C* reperisco la natura giuridica del contribuente
092500000000     C                   MoveL     XScNCR        WrkNatGiu
092600000000     C                   EXSR      RepNatGiu
092700000000     C*
092800000000     C                   Select
092900000000     C*-
093000000000     C*-----------------------
093100000000     C* PERSONA FISICA
093200000000     C* da impostare solo nel caso di persona fisica
093300000000     C                   When      TPNG16      = '1'
093400000000     C*
093500000000     C                   EXSR      RepPersFis
093600000000     C*-
093700000000     C* Cognome
093800000000     C                   MoveL     $CogRagSog    K61BCOGNO
093900000000     C*-
094000000000     C* Nome
094100000000     C                   MoveL     $NomeSog      K61BNOME
094200000000     C*-----------------------
094300000000     C* PERSONA GIURIDICA
094400000000     C* da impostare solo nel caso di persona non fisica
094500000000     C                   When      TPNG16      = '2'  or
094600000000     C                             TPNG16      = '3'
094700000000     C*-
094800000000     C* Denominazione
094900000000     C                   MoveL     XSCRGS        K61BDENOMI
095000000000     C*
095100000000     C                   EndSl
095200000000     C*
095300000000     C* e-mail
095400000000     C                   MoveL     $Email        K61BEMAIL
095500000000     C*
095600000000     C* Telefono
095700000000     C                   if        $Telefon <> *blank
095800000000     C                   move      $Telefon      K61Btel
095900000000     C                   endif
096000170207R250 C* Eventi eccezionali (preimposto)
096100170207R250 C                   Eval      K61BEVE = '0'
096200000000     C*
096300000000     C*-----------------------------------
096400000000     C* DATI SOGGETTO CHE SI IMPEGNA ALLA TRASMISSIONE
096500000000     C* (soggetto che effettua la comunicazione, se diverso dal soggetto
096600000000     C*  a cui si riferisce la comunicazione)
096700000000     C*
096800000000     C                   Move      *Blanks       K61BCDFIS1
096900000000     C                   Move      *zeros        K61BCDFIS2
097000000000     C* Se significativo
097100000000     C                   if        $FisSog       <> *Blanks
097200000000     C* Cod.Fiscale del rappresentante
097300000000     C                   Move      $FisSog       K61BCDFIS1
097400000000     C* Cod.Carica
097500000000     C                   Move      $CarSog       K61BCARR1
097600000000     C* Persona Fisica
097700000000     c                   if        $COGSOG       <> *Blanks
097800000000     C                   Eval      K61BCOGNO1   = $CogSOG
097900000000     C                   Eval      K61BNOME1    = $NomSOG
098000000000     C                   endif
098100000000     C*
098200000000     C* Cod.Fiscale società dichiarante
098300000000     C                   if        $FisDic <> *blank
098400000000     C                   Movel     $FisDic       K61BCDFIS2
098500000000     C                   endif
098600000000     C*-
098700000000     C                   endif
098800000000     C*-----------------------------------
098900000000     C*
099000160204R211 X***                z-add     0             K61BNrDip
099100000000     C                   z-add     0             K61BNrAut
099200000000     C                   Move      '0'           K61BQuaCT
099300000000     C*
099400000000     C* Firma del dichiarante
099500000000     C                   if        $FirDic    <> *blank
099600000000     C                   Eval      K61BFirma   = '1'
099700000000     C                   endif
099800000000     C*
099900000000     C*-----------------------------------
100000000000     C* DATI SOGGETTO CHE SI IMPEGNA ALLA TRASMISSIONE (Intermediario)
100100000000     C*
100200000000     C* Cod.Fisc.Intermediario
100300000000     C                   Move      *Blanks       K61BCFINT
100400000000     C* Impegno alla presentazione
100500000000     C                   Z-Add     0             K61BIMPE
100600000000     C* Data dell'Impegno alla presentazione
100700000000     C                   Move      *All'0'       K61BDTIMPE
100800000000     C* Firma
100900000000     C                   move      '0'           K61BFIRINT
101000000000     C*
101100000000     C* Se significativo
101200000000     C                   if        $FisInt       <> *Blanks
101300000000     C* Cod.Fisc.Intermediario
101400000000     C                   Move      $FisInt       K61BCFINT
101500000000     C* Impegno alla presentazione
101600000000     C                   Move      $ImpTrs       K61BIMPE
101700000000     C* Data dell'Impegno alla presentazione
101800000000     C                   Move      $ImpDta       DtAMG
101900000000     C                   EXSR      ValorizDTA
102000000000     C                   Eval      K61BDTIMPE   = WrkDt
102100000000     C* Firma dell'intemediario
102200000000     C                   if        $FirInt    <> *blank
102300000000     C                   Eval      K61BFirInt  = '1'
102400000000     C                   endif
102500000000     C                   endif
102600000000     C*-
102700000000     C*-----------------------------------
102800000000     C*-
102900000000     C* Carattere di controllo
103000000000     C                   MoveL     'A'           K61BK1
103100000000     C*-
103200000000     C* Elementi di fine riga
103300000000     C* Impostare i valori esadecimali '0D' e '0A'
103400000000     C* (caratteri ASCI 'CR' ed 'LF')
103500000000     C*
103600000000     C                   Eval      K61BK2 =  *Blanks
103700000000     C                   Eval      K61BK2 =  X'0D25'
103800000000     C*
103900000000     C                   MoveL     SDGSK61DSB    CUWREC
104000000000     C                   write     SDGCUW000
104100000000     C                   feod      SDGCUW00F
104200000000     C                   z-add     NrrCuw        SavNrrCuw
104300000000     C*
104400000000     C                   ENDSR
104500000000     C/EJECT
104600000000     C******************************************************
104700000000     C* Aggiorno record B
104800000000     C******************************************************
104900000000     C     UpdTpRecB     BEGSR
105000000000     C*
105100000000     C     2             chain     SdgCuw00f
105200000000     C                   if        %found(SdgCuw00f)
105300000000     C                   movel     CuwRec        SdgSk61DsB
105400000000     C*
105500150305R172 X****               z-add     Cnt_Righe     K61BNrAut
105600150305R172 C                   z-add     Cnt_D         K61BNrAut
105700000000     C*
105800000000     C                   movel     SdgSk61DsB    CuwRec
105900000000     C                   update    SdgCuw000
106000000000     C                   endif
106100000000     C*
106200000000     C                   ENDSR
106300000000     C/EJECT
106400000000     C******************************************************
106500000000     C* Reperimento della natura giuridica
106600000000     C******************************************************
106700000000     C     RepNatGiu     BEGSR
106800000000     C*
106900000000     C                   Reset                   SDGTABDS
107000000000     C*
107100000000     C                   MOVE      '1'           XTBRIC
107200000000     C                   MOVE      SocietaK61    XTBAZI
107300000000     C                   MOVE      *ZERO         XTBKEY
107400000000     C                   MOVE      'G16'         XTBCOD
107500000000     C                   MOVE      WrkNatGiu     XTBKEY
107600000000     C                   CALL      'SDGTABDR'
107700000000     C                   PARM                    SDGTABDS
107800000000     C                   if        XTBERR      = *Off
107900000000     C                   MoveL     XTBUNI        ANGG16DS
108000000000     C                   endif
108100000000     C*
108200000000     C                   ENDSR
108300000000     C/EJECT
108400000000     C************************************************************
108500000000     C* Reperisci dati persona fisica attraverso soggetto
108600000000     C************************************************************
108700000000     C     RepPersFis    BEGSR
108800000000     C*
108900000000     C                   Clear                   $CdFiscSog
109000000000     C                   Clear                   $CdIVASog
109100000000     C                   Clear                   $CogRagSog
109200000000     C                   Clear                   $NomeSog
109300000000     C                   Clear                   $DtNascSog
109400000000     C                   Clear                   $LocNascSog
109500000000     C                   Clear                   $PrvNascSog
109600000000     C                   Clear                   $StatoSog
109700000000     C                   Clear                   $StaFedSog
109800000000     C                   Clear                   $LocalitSog
109900000000     C                   Clear                   $IndirESog
110000000000     C                   Clear                   $SexSog
110100000000     C*
110200000000     C                   Eval      SO3Sogg     = SocSogg
110300000000     C                   Eval      SO3DtFiVl   = DatMin
110400000000     C                   Eval      SO3Sys      = 0
110500000000     C                   Eval      SO3NrAsInt  = 0
110600000000     C*
110700160530R224 X**** K04SO301      CHAIN     SDGSO301L                          21
110800160530R224 C                   EXSR      ChainSDGSO3
110900160530R224 C*
111000000000     C                   if        *In21       = *Off
111100000000     C                   MoveL(P)  SO3CdFisc     $CdFiscSog
111200000000     C                   MoveL(P)  SO3PartIva    $CdIVASog
111300000000     C                   MoveL(P)  SO3CogRag     $CogRagSog
111400000000     C                   MoveL(P)  SO3Nome       $NomeSog
111500000000     C                   Move      SO3DtNasc     $DtNascSog
111600000000     C                   MoveL(P)  SO3LocNasc    $LocNascSog
111700000000     C                   MoveL(P)  SO3PrvNasc    $PrvNascSog
111800000000     C                   MoveL(P)  SO3Stato      $StatoSog
111900000000     C                   MoveL(P)  SO3StaFed     $StaFedSog
112000000000     C                   MoveL(P)  SO3Localit    $LocalitSog
112100000000     C                   MoveL(P)  SO3IndirE     $IndirESog
112200000000     C                   MoveL     SO3Sex        $SexSog
112300000000     C                   endif
112400000000     C*
112500000000     C                   ENDSR
112600000000     C/EJECT
112700160530R224 C************************************************************
112800160530 "   C* Lettura dati aggiuntivi anagrafici
112900160530 "   C************************************************************
113000160530R224 C     ChainSDGSO3   BEGSR
113100160530     C*
113200160607     C                   Eval      SO3Dest     = $DestSos
113300160530     C*
113400160530     C     K05SO301      CHAIN     SDGSO301L                          21
113500160530     C*
113600160530     C* se non trovato, provo con la destinazione Generica
113700160530     C                   If        *In21       = *On     and
113800160530     C                             SO3Dest    <> *Blank
113900160530     C                   Eval      SO3Dest     = *Blank
114000160530     C     K05SO301      CHAIN     SDGSO301L                          21
114100160530     C                   endif
114200160530     C*
114300160530R224 C                   ENDSR
114400160530R224 C/EJECT
114500000000     C******************************************************
114600000000     C* Valorizzazione data per telematico
114700000000     C******************************************************
114800000000     C     ValorizDTA    BEGSR
114900000000     C*
115000000000     C                   Clear                   WrkDt
115100000000     C*
115200000000     C                   Move      AMGAnno       GMAAnno
115300000000     C                   Move      AMGMese       GMAMese
115400000000     C                   Move      AMGGiorno     GMAGiorno
115500000000     C*
115600000000     C                   Eval      WrkDt        = DtGMA
115700000000     C*
115800000000     C                   ENDSR
115900000000     C******************************************************
116000000000     C* Scrittura t.r. D
116100000000     C******************************************************
116200000000     C     WrtTpRecD     BEGSR
116300000000     C*
116400150305R172 X****               add       1             Cnt_Righe
116500000000     C*
116600000000     C                   Clear                   SDGSK61DSD
116700000000     C* Tipo Record
116800000000     C                   Eval      K61DTR       = 'D'
116900000000     C*
117000000000     C* Cod.Fiscale
117100000000     C                   Eval      K61DCFI      = XScCdF
117200000000     C*
117300000000     C* Progressivo modulo
117400000000     C                   Eval      K61DPRGMOD   = 1
117500000000     C*
117600000000     C* Cod.Fiscale Percipiente
117700000000     C                   Eval      K61DCFP      = SavCdf
117800000000     C*
117900000000     C* Progressivo Certificazione
118000000000     C                   Eval      K61DPrgCer   = Cnt_Righe
118100000000     C*
118200000000     C* Protocollo
118300000000     C                   Eval      K61DProt     = ProtocK61
118400150306R172 C                   if        TIPINVK61     = 1 or
118500150306R172 C                             TIPINVK61     = 2
118600150306R172 C                   if        ProtodK61   <> 0
118700000000     C                   Eval      K61DProtD    = ProtodK61
118800150306R172 C                   else
118900150306 "   C                   Eval      K61DProtD    = SavProtCea
119000150306 "   C                   endif
119100150306R172 C                   endif
119200000000     C*
119300000000     C* Tipo operazione
119400000000     C                   Select
119500000000     C* Sostituzione
119600000000     C                   When      TIPINVK61     = 1
119700000000     C                   Move      'S'           K61DAnnSos
119800000000     C* Annullamento
119900000000     C                   When      TIPINVK61     = 2
120000000000     C                   Move      'A'           K61DAnnSos
120100000000     C                   EndSl
120200000000     C*
120300000000     C* Singola certificazione confermata
120400000000     C                   Move      '0'           K61DSCConf
120500000000     C*
120600000000     C                   MoveA     Skiera        K61DDATI
120700000000     C*
120800000000     C* Carattere di controllo
120900000000     C                   MoveL     'A'           K61DK1
121000000000     C*-
121100000000     C* Elementi di fine riga
121200000000     C* Impostare i valori esadecimali '0D' e '0A'
121300000000     C* (caratteri ASCI 'CR' ed 'LF')
121400000000     C*
121500000000     C                   Eval      K61DK2  = *Blanks
121600000000     C                   Eval      K61DK2  = X'0D25'
121700000000     C*
121800000000     C                   Add       1             Cnt_D
121900000000     C                   Clear                   SDGCUW000
122000000000     C                   Movel     SDGSK61DSD    CUWREC
122100000000?    C                   write     SDGCUW000
122200000000     C                   feod      SDGCUW00F
122300150305R172 C*
122400150305R172 C                   add       1             Cnt_Righe
122500000000     C*
122600000000     C                   ENDSR
122700000000     C/EJECT
122800000000     C******************************************************
122900000000     C* Inizializzo campi per la gestione della tabella
123000000000     C******************************************************
123100000000     C     InzSkieraD    BEGSR
123200000000     C*
123300000000     C                   reset                   Skiera
123400000000     C                   z-add     0             $I
123500000000     C*
123600000000     C                   move      *Off          $FlDatiAna
123700000000     C*
123800000000     C                   ENDSR
123900000000     C/EJECT
124000000000     C******************************************************
124100000000     C* Dati anagrafici del percipiente
124200000000     C******************************************************
124300000000     C     ElaboraRecD   BEGSR
124400000000     C*
124500000000     C                   Eval      WSavQuaD  ='DA'
124600000000     C*
124700000000     C                   if        $Fine770  = *Off
124800000000     C*
124900000000     C                   Z-Add     1             Prog_DA
125000000000     C*
125100000000     C                   if        $FlDatiAna    = *Off
125200000000     C*
125300000000     C* ---------------------------------------------------------------------------------
125400000000     C* Dati relativi al datore di lavoro, ente pensionistico o altro sostituto d'imposta
125500000000     C* ---------------------------------------------------------------------------------
125600000000     C* DA001
125700000000     C*
125800000000     C* 1) Cod.Fiscale
125900000000     C                   Z-Add     Prog_DA       Conta_Rigo
126000000000     C                   Z-Add     1             Conta_Colo
126100000000     C                   MoveL     XscCdf        Campo_Valore
126200000000     C                   MoveL     XscCdf        C60
126300000000     C                   if        Campo_Valore <> *Blanks
126400000000     C                   EXSR      RieSkieraAlf
126500000000     C                   endif
126600000000     C*
126700000000     C                   Select
126800000000     C*-----------------------
126900000000     C* PERSONA FISICA
127000000000     C                   When      TPNG16      = '1'
127100000000     C*
127200000000     C* 2) Cognome
127300000000     C                   if        $CogRagSog    <> *Blanks
127400000000     C                   Z-Add     Prog_DA       Conta_Rigo
127500000000     C                   Z-Add     2             Conta_Colo
127600000000     C                   MoveL     $CogRagSog    Campo_Valore
127700000000     C                   MoveL     $CogRagSog    C60
127800000000     C                   if        Campo_Valore <> *Blanks
127900000000     C                   EXSR      RieSkieraAlf
128000000000     C                   endif
128100000000     C                   endif
128200000000     C*
128300000000     C* 3) Nome
128400000000     C                   if        $NomeSog      <> *Blanks
128500000000     C                   Z-Add     Prog_DA       Conta_Rigo
128600000000     C                   Z-Add     3             Conta_Colo
128700000000     C                   Clear                   Campo_Valore
128800000000     C                   MoveL     $NomeSog      Campo_Valore
128900000000     C                   MoveL     $NomeSog      C60
129000000000     C                   if        Campo_Valore <> *Blanks
129100000000     C                   EXSR      RieSkieraAlf
129200000000     C                   endif
129300000000     C                   endif
129400000000     C*
129500000000     C* 4) Comune di residenza
129600000000     C                   if        $LocalitSog   <> *Blanks
129700000000     C                   Z-Add     Prog_DA       Conta_Rigo
129800000000     C                   Z-Add     4             Conta_Colo
129900000000     C                   Clear                   Campo_Valore
130000000000     C                   MoveL     $LocalitSog   Campo_Valore
130100000000     C                   MoveL     $LocalitSog   C60
130200000000     C                   if        Campo_Valore <> *Blanks
130300000000     C                   EXSR      RieSkieraAlf
130400000000     C                   endif
130500000000     C                   endif
130600000000     C*
130700000000     C* 5) Provincia di residenza - CONTROLLARE
130800000000     C**                 if        Provincia     <> *Blanks
130900000000     C**                 Z-Add     Prog_DA       Conta_Rigo
131000000000     C**                 Z-Add     5             Conta_Colo
131100000000     C**                 Clear                   Campo_Valore
131200000000     C**                 MoveL     N77Prv        Campo_Valore
131300000000     C**                 MoveL     N77Prv        C60
131400000000     C**                 if        Campo_Valore <> *Blanks
131500000000     C**                 EXSR      RieSkieraAlf
131600000000     C**                 endif
131700000000     C**                 endif
131800000000     C*-----------------------
131900000000     C* PERSONA GIURIDICA
132000000000     C* da impostare solo nel caso di persona non fisica
132100000000     C                   When      TPNG16      = '2'  or
132200000000     C                             TPNG16      = '3'
132300000000     C*
132400000000     C* 2) Denominazione
132500000000     C                   if        XscRgs        <> *Blanks
132600000000     C                   Z-Add     Prog_DA       Conta_Rigo
132700000000     C                   Z-Add     2             Conta_Colo
132800000000     C                   MoveL     XscRgs        Campo_Valore
132900000000     C                   MoveL     XscRgs        C60
133000000000     C                   if        Campo_Valore <> *Blanks
133100000000     C                   EXSR      RieSkieraAlf
133200000000     C                   endif
133300000000     C                   endif
133400150302R172 C*
133500150302 "   C* 4) Comune di Residenza
133600150302 "   C                   if        XscCit        <> *Blanks
133700150302 "   C                   Z-Add     Prog_DA       Conta_Rigo
133800150302 "   C                   Z-Add     4             Conta_Colo
133900150302 "   C                   MoveL     XscCit        Campo_Valore
134000150302 "   C                   MoveL     XscCit        C60
134100150302 "   C                   if        Campo_Valore <> *Blanks
134200150302 "   C                   EXSR      RieSkieraAlf
134300150302 "   C                   endif
134400150302 "   C                   endif
134500150302 "   C*
134600150302 "   C* 5) Provincia
134700150302 "   C                   if        XscPrv        <> *Blanks
134800150302 "   C                   Z-Add     Prog_DA       Conta_Rigo
134900150302 "   C                   Z-Add     5             Conta_Colo
135000150302 "   C                   MoveL     XscPrv        Campo_Valore
135100150302 "   C                   MoveL     XscPrv        C60
135200150302 "   C                   if        Campo_Valore <> *Blanks
135300150302 "   C                   EXSR      RieSkieraAlf
135400150302 "   C                   endif
135500150302 "   C                   endif
135600150302 "   C*
135700150302 "   C* 6) CAP
135800150302 "   C                   if        XscCAP        <> *Blanks
135900150302 "   C                   Z-Add     Prog_DA       Conta_Rigo
136000150302 "   C                   Z-Add     6             Conta_Colo
136100150302 "   C                   MoveL     XscCAP        Campo_Valore
136200150302 "   C                   MoveL     XscCAP        C60
136300150302 "   C                   if        Campo_Valore <> *Blanks
136400150302 "   C                   EXSR      RieSkieraAlf
136500150302 "   C                   endif
136600150302 "   C                   endif
136700150302 "   C*
136800150302 "   C* 7) Indirizzo
136900150302 "   C                   if        XscVia        <> *Blanks
137000150302 "   C                   Z-Add     Prog_DA       Conta_Rigo
137100150302 "   C                   Z-Add     7             Conta_Colo
137200150302 "   C                   MoveL     XscVia        Campo_Valore
137300150302 "   C                   MoveL     XscVia        C60
137400150302 "   C                   if        Campo_Valore <> *Blanks
137500150302 "   C                   EXSR      RieSkieraAlf
137600150302 "   C                   endif
137700150302R172 C                   endif
137800000000     C*
137900000000     C                   EndSl
138000000000     C*
138100000000     C*
138200000000     C* 8) Telefono
138300000000     C                   if        $Telefon      <> *Blanks
138400000000     C                   Z-Add     Prog_DA       Conta_Rigo
138500000000     C                   Z-Add     8             Conta_Colo
138600000000     C                   Clear                   Campo_Valore
138700000000     C                   MoveL     $Telefon      Campo_Valore
138800000000     C                   MoveL     $Telefon      C60
138900000000     C                   if        Campo_Valore <> *Blanks
139000000000     C                   EXSR      RieSkieraAlf
139100000000     C                   endif
139200000000     C                   endif
139300000000     C*
139400000000     C* 9) E-mail
139500000000     C                   if        $Email        <> *Blanks
139600000000     C                   Z-Add     Prog_DA       Conta_Rigo
139700000000     C                   Z-Add     9             Conta_Colo
139800000000     C                   Clear                   Campo_Valore
139900000000     C                   MoveL     $Email        Campo_Valore
140000000000     C                   MoveL     $Email        C60
140100000000     C                   if        Campo_Valore <> *Blanks
140200000000     C                   EXSR      RieSkieraAlf
140300000000     C                   endif
140400000000     C                   endif
140500000000     C*
140600000000     C* 10) Codice attività
140700000000     C                   if        XscAtt        <> *Blanks
140800000000     C                   Z-Add     Prog_DA       Conta_Rigo
140900000000     C                   Z-Add     10            Conta_Colo
141000000000     C                   Clear                   Campo_Valore
141100000000     C                   MoveL     XscAtt        Campo_Valore
141200000000     C                   MoveL     XscAtt        C60
141300000000     C                   if        Campo_Valore <> *Blanks
141400000000     C                   EXSR      RieSkieraAlf
141500000000     C                   endif
141600000000     C                   endif
141700000000     C*
141800000000     C* DA002
141900000000     C* Dati relativi al dipendente, pensionato o altro percettore delle somme
142000000000     C* ----------------------------------------------------------------------
142100000000     C                   Add       1             Prog_DA
142200000000     C*
142300000000     C* 1) Cod.Fiscale
142400000000     C                   if        N77Cdf        <> *Blanks
142500000000     C                   Z-Add     Prog_DA       Conta_Rigo
142600000000     C                   Z-Add     1             Conta_Colo
142700000000     C                   Clear                   Campo_Valore
142800000000     C                   MoveL     N77Cdf        Campo_Valore
142900000000     C                   MoveL     N77Cdf        C60
143000000000     C                   if        Campo_Valore <> *Blanks
143100000000     C                   EXSR      RieSkieraAlf
143200000000     C                   endif
143300000000     C                   endif
143400000000     C*
143500000000     C* 2) Cognome
143600000000     C                   if        N77Cog        <> *Blanks
143700000000     C                   Z-Add     Prog_DA       Conta_Rigo
143800000000     C                   Z-Add     2             Conta_Colo
143900000000     C                   Clear                   Campo_Valore
144000000000     C                   MoveL     N77Cog        Campo_Valore
144100000000     C                   MoveL     N77Cog        C60
144200000000     C                   if        Campo_Valore <> *Blanks
144300000000     C                   EXSR      RieSkieraAlf
144400000000     C                   endif
144500000000     C                   endif
144600000000     C*
144700000000     C* 3) Nome
144800000000     C                   if        N77Nom        <> *Blanks
144900000000     C                   Z-Add     Prog_DA       Conta_Rigo
145000000000     C                   Z-Add     3             Conta_Colo
145100000000     C                   Clear                   Campo_Valore
145200000000     C                   MoveL     N77Nom        Campo_Valore
145300000000     C                   MoveL     N77Nom        C60
145400000000     C                   if        Campo_Valore <> *Blanks
145500000000     C                   EXSR      RieSkieraAlf
145600000000     C                   endif
145700000000     C                   endif
145800000000     C*
145900000000     C* 4) Sesso
146000000000     C                   if        N77Ses        <> *Blanks
146100000000     C                   Z-Add     Prog_DA       Conta_Rigo
146200000000     C                   Z-Add     4             Conta_Colo
146300000000     C                   Clear                   Campo_Valore
146400000000     C                   MoveL     N77Ses        Campo_Valore
146500000000     C                   MoveL     N77Ses        C60
146600000000     C                   if        Campo_Valore <> *Blanks
146700000000     C                   EXSR      RieSkieraAlf
146800000000     C                   endif
146900000000     C                   endif
147000000000     C*
147100000000     C* 5) Data di nascita
147200000000     C                   if        N77Dnc        <> Datmin
147300000000     C                   Move      N77Dnc        DtAMG
147400000000     C                   Move      AMGAnno       GMAAnno
147500000000     C                   Move      AMGMese       GMAMese
147600000000     C                   Move      AMGGiorno     GMAGiorno
147700000000     C                   Z-Add     Prog_DA       Conta_Rigo
147800000000     C                   Z-Add     5             Conta_Colo
147900000000     C                   Clear                   Campo_Valore
148000000000     C                   Move      DtGMA         Campo_Valore
148100000000     C                   if        Campo_Valore <> *Blanks
148200000000     C                   EXSR      RieSkiera
148300000000     C                   endif
148400000000     C                   endif
148500000000     C*
148600000000     C* 6) Comune o Stato estero di nascita
148700000000     C                   if        N77Lnc        <> *Blanks
148800000000     C                   Z-Add     Prog_DA       Conta_Rigo
148900000000     C                   Z-Add     6             Conta_Colo
149000000000     C                   Clear                   Campo_Valore
149100000000     C                   MoveL     N77Lnc        Campo_Valore
149200000000     C                   MoveL     N77Lnc        C60
149300000000     C                   if        Campo_Valore <> *Blanks
149400000000     C                   EXSR      RieSkieraAlf
149500000000     C                   endif
149600000000     C                   endif
149700000000     C*
149800000000     C* 7) Provincia di nascita
149900000000     C                   if        N77Pnc        <> *Blanks
150000000000     C                   Z-Add     Prog_DA       Conta_Rigo
150100000000     C                   Z-Add     7             Conta_Colo
150200000000     C                   Clear                   Campo_Valore
150300000000     C                   MoveL     N77Pnc        Campo_Valore
150400000000     C                   if        Campo_Valore <> *Blanks
150500000000     C                   EXSR      RieSkiera
150600000000     C                   endif
150700000000     C                   endif
150800000000     C*
150900000000     C* 8) Categorie particolari
151000150305R172 C                   if        N77CtgPrt     <> *Blanks
151100150305 "   C                   Z-Add     Prog_DA       Conta_Rigo
151200150305 "   C                   Z-Add     8             Conta_Colo
151300150305 "   C                   Clear                   Campo_Valore
151400150305 "   C                   MoveL     N77CtgPrt     Campo_Valore
151500150305 "   C                   if        Campo_Valore <> *Blanks
151600150305 "   C                   EXSR      RieSkiera
151700150305 "   C                   endif
151800150305R172 C                   endif
151900170207R250 X**** 9) Eventi eccezionali
152000000000     C* 10) Casi di esclusione dalla precompilata
152100000000     C*
152200000000     C* DA002020
152300000000     C*
152400160202R211 X*** Domicilio fiscale al 1/1/2014
152500160202R211 C* Domicilio fiscale anno precedente
152600000000     C* -----------------------------
152700000000     C                   if        %subst(N77C77:1:1) = 'N'
152800000000     C* 20) Comune
152900160203R211 X***                if        N77Cit        <> *Blanks
153000160203R211 C                   if        N77CitAp      <> *Blanks
153100000000     C                   Z-Add     Prog_DA       Conta_Rigo
153200000000     C                   Z-Add     20            Conta_Colo
153300000000     C                   Clear                   Campo_Valore
153400160203R211 X***                MoveL     N77Cit        Campo_Valore
153500160203R211 X***                MoveL     N77Cit        C60
153600160203R211 C                   MoveL     N77CitAP      Campo_Valore
153700160203R211 C                   MoveL     N77CitAP      C60
153800000000     C                   if        Campo_Valore <> *Blanks
153900000000     C                   EXSR      RieSkieraAlf
154000000000     C                   endif
154100000000     C                   endif
154200000000     C*
154300000000     C* 21) Provincia
154400160203R211 X***                if        N77Prv        <> *Blanks
154500160203R211 C                   if        N77PrvAP      <> *Blanks
154600000000     C                   Z-Add     Prog_DA       Conta_Rigo
154700000000     C                   Z-Add     21            Conta_Colo
154800000000     C                   Clear                   Campo_Valore
154900160203R211 X***                MoveL     N77Prv        Campo_Valore
155000160203R211 X***                MoveL     N77Prv        C60
155100160203R211 C                   MoveL     N77PrvAP      Campo_Valore
155200160203R211 C                   MoveL     N77PrvAP      C60
155300000000     C                   if        Campo_Valore <> *Blanks
155400000000     C                   EXSR      RieSkiera
155500000000     C                   endif
155600160203     C                   endif
155700000000     C*
155800000000     C* 22) Codice comune
155900160203R211 X***                if        N77Com        <> *Blanks
156000160217R215 X*** Ex R211        if        N77Com        <> *Blanks
156100160217R215 C                   if        N77ComAp      <> *Blanks
156200000000     C                   Z-Add     Prog_DA       Conta_Rigo
156300000000     C                   Z-Add     22            Conta_Colo
156400000000     C                   Clear                   Campo_Valore
156500160203R211 X***                MoveL     N77Com        Campo_Valore
156600160203R211 X***                MoveL     N77Com        C60
156700160203R211 C                   MoveL     N77ComAP      Campo_Valore
156800160203R211 C                   MoveL     N77ComAP      C60
156900000000     C                   if        Campo_Valore <> *Blanks
157000000000     C                   EXSR      RieSkieraAlf
157100000000     C                   endif
157200000000     C                   endif
157300180123R279 C*
157400180123  "  C* 23) Fusione comune
157500180123  "  C                   if        N77FusComP    <> *Blanks
157600180123  "  C                   Z-Add     Prog_DA       Conta_Rigo
157700180123  "  C                   Z-Add     23            Conta_Colo
157800180123  "  C                   Clear                   Campo_Valore
157900180123  "  C                   MoveL     N77FusComP    Campo_Valore
158000180123  "  C                   MoveL     N77FusComP    C60
158100180123  "  C                   if        Campo_Valore <> *Blanks
158200180123  "  C                   EXSR      RieSkiera
158300180123  "  C                   endif
158400180123R279 C                   endif
158500000000     C*
158600160203R211 X***                endif
158700000000     C* DA002023
158800000000     C*
158900160202R211 X*** Domicilio fiscale al 1/1/2015
159000160202R211 C* Domicilio fiscale anno corrente
159100000000     C* -----------------------------
159200160217R215 C* Solo se diverso da quello attuale
159300160217  "  C                   if        N77Cit <> N77CitAp  or
159400160217  "  C                             N77Prv <> N77PrvAp  Or
159500160217R215 C                             N77Com <> N77ComAp
159501180206R2791C                             or N77FusCom <> N77FusComP
159600180123R279 X*** 23) Comune
159700180123R279 C* 24) Comune
159800000000     C                   if        N77Cit        <> *Blanks
159900000000     C                   Z-Add     Prog_DA       Conta_Rigo
160000180123R279 X***                Z-Add     23            Conta_Colo
160100180123R279 C                   Z-Add     24            Conta_Colo
160200000000     C                   Clear                   Campo_Valore
160300000000     C                   MoveL     N77Cit        Campo_Valore
160400000000     C                   MoveL     N77Cit        C60
160500000000     C                   if        Campo_Valore <> *Blanks
160600000000     C                   EXSR      RieSkieraAlf
160700000000     C                   endif
160800000000     C                   endif
160900000000     C*
161000180123R279 X*** 24) Provincia
161100180123R279 C* 25) Provincia
161200000000     C                   if        N77Prv        <> *Blanks
161300000000     C                   Z-Add     Prog_DA       Conta_Rigo
161400180123R279 X***                Z-Add     24            Conta_Colo
161500180123R279 C                   Z-Add     25            Conta_Colo
161600000000     C                   Clear                   Campo_Valore
161700000000     C                   MoveL     N77Prv        Campo_Valore
161800000000     C                   MoveL     N77Prv        C60
161900000000     C                   if        Campo_Valore <> *Blanks
162000000000     C                   EXSR      RieSkieraAlf
162100000000     C                   endif
162200000000     C                   endif
162300000000     C*
162400180123R279 X*** 25) Codice comune
162500180123R279 C* 26) Codice comune
162600000000     C                   if        N77Com        <> *Blanks
162700000000     C                   Z-Add     Prog_DA       Conta_Rigo
162800180123R279 X***                Z-Add     25            Conta_Colo
162900180123R279 C                   Z-Add     26            Conta_Colo
163000000000     C                   Clear                   Campo_Valore
163100000000     C                   MoveL     N77Com        Campo_Valore
163200000000     C                   MoveL     N77Com        C60
163300000000     C                   if        Campo_Valore <> *Blanks
163400000000     C                   EXSR      RieSkiera
163500000000     C                   endif
163600000000     C                   endif
163700160217R215 C*
163800180206R2791C** ex R215         endif
163900170125R250 C*
164000180123R279 X***ExR250 26) Fusione comune (sembra vada indicato sempre se presente)
164100180123R279 C* 27) Fusione comune (sembra vada indicato sempre se presente)
164200180123R250 C                   if        N77FusCom     <> *Blanks
164300180123R250 C                   Z-Add     Prog_DA       Conta_Rigo
164400180123R279 X***Exr250          Z-Add     26            Conta_Colo
164500180123R279 C                   Z-Add     27            Conta_Colo
164600180123R250 C                   Clear                   Campo_Valore
164700170125  "  C                   MoveL     N77FusCom     Campo_Valore
164800170125  "  C                   MoveL     N77FusCom     C60
164900170125  "  C                   if        Campo_Valore <> *Blanks
165000170125  "  C                   EXSR      RieSkiera
165100170125  "  C                   endif
165200170125R250 C                   endif
165201180206R2791C                   endif
165300160203R211 C*
165400160203R211 C                   endif
165500000000     C*
165600000000     C* Dati relativi al rappresentante
165700000000     C* -------------------------------
165800000000     C* DA002030
165900000000     C*
166000000000     C* 30) Cod.Fiscale
166100000000     C***                if        K61BCdFis1    <> *Blanks
166200000000     C***                Z-Add     Prog_DA       Conta_Rigo
166300000000     C***                Z-Add     30            Conta_Colo
166400000000     C***                Clear                   Campo_Valore
166500000000     C***                MoveL     K61BCdFis1    Campo_Valore
166600000000     C***                MoveL     K61BCdFis1    C60
166700000000     C***                if        Campo_Valore <> *Blanks
166800000000     C***                EXSR      RieSkieraAlf
166900000000     C***                endif
167000000000     C***                endif
167100000000     C*
167200000000     C* Riservato ai percipienti esteri
167300000000     C* -------------------------------
167400000000     C* DA002040
167500000000     C*
167600000000     C* 40) Codice di identificazione fiscale estero
167700000000     C                   if        N77Cde        <> *Blank
167800000000     C                   Z-Add     Prog_DA       Conta_Rigo
167900000000     C                   Z-Add     40            Conta_Colo
168000000000     C                   Clear                   Campo_Valore
168100000000     C                   Movel     N77Cde        Campo_Valore
168200000000     C                   Movel     N77Cde        C60
168300000000     C                   if        Campo_Valore <> *Blanks
168400000000     C                   EXSR      RieSkieraAlf
168500000000     C                   endif
168600000000     C                   endif
168700000000     C*
168800000000     C* 41) Località di residenza estera
168900000000     C                   if        N77RCit       <> *Blank
169000000000     C                   Z-Add     Prog_DA       Conta_Rigo
169100000000     C                   Z-Add     41            Conta_Colo
169200000000     C                   Clear                   Campo_Valore
169300000000     C                   Movel     N77RCit       Campo_Valore
169400000000     C                   Movel     N77RCit       C60
169500000000     C                   if        Campo_Valore <> *Blanks
169600000000     C                   EXSR      RieSkieraAlf
169700000000     C                   endif
169800000000     C                   endif
169900000000     C*
170000000000     C* 42) Via e numero civico
170100000000     C                   if        N77RVia       <> *Blank
170200000000     C                   Z-Add     Prog_DA       Conta_Rigo
170300000000     C                   Z-Add     42            Conta_Colo
170400000000     C                   Clear                   Campo_Valore
170500000000     C                   Movel     N77RVia       Campo_Valore
170600000000     C                   Movel     N77RVia       C60
170700000000     C                   if        Campo_Valore <> *Blanks
170800000000     C                   EXSR      RieSkieraAlf
170900000000     C                   endif
171000000000     C                   endif
171100000000     C*
171200160128R211 C* 43) Non Residenti Shumacker
171300160201  "  C                   if        N77NResS  <> *blank
171400160128  "  C                   Z-Add     Prog_DA       Conta_Rigo
171500160128  "  C                   Z-Add     43            Conta_Colo
171600160128  "  C                   Clear                   Campo_Valore
171700160128  "  C                   Move      '1'           Campo_Valore
171800160128  "  C                   EXSR      Ablank
171900160128  "  C                   if        Campo_Valore <> *Blanks
172000160128  "  C                   EXSR      RieSkiera
172100160128  "  C                   endif
172200160128R211 C                   endif
172300160201R211 X*** 43) Codice stato estero
172400160201R211 C* 44) Codice stato estero
172500000000     C                   if        N77Cse        <> *Blank
172600160222R216 X*ex R169                     and N77Ser <> *blank
172700000000     C                   Z-Add     Prog_DA       Conta_Rigo
172800160128R211 X**                 Z-Add     43            Conta_Colo
172900160128R211 C                   Z-Add     44            Conta_Colo
173000000000     C                   Clear                   Campo_Valore
173100000000     C                   movel     N77Cse        Cse               9
173200000000     C                   callb     'XALLINEA'
173300000000     C                   parm                    Cse
173400000000     C                   parm      9             Len2              5 0
173500000000     C                   parm      '2'           Input             1
173600000000     C                   move      Cse           Campo_Valore
173700000000     C                   if        Campo_Valore <> *Blanks
173800000000     C                   EXSR      RieSkiera
173900000000     C                   endif
174000000000     C                   endif
174100000000     C*
174200000000     C* Sezione firma certificazione unica
174300000000     C* ----------------------------------
174400000000     C* DA003001
174500000000     C*
174600000000     C                   Add       1             Prog_DA
174700000000     C*
174800000000     C* 1) Data Firma Sostituto d'imposta
174900000000     C                   if        $DtFirDic    <> *blank
175000000000     C                   Move      $DtFirDic     DtAMG
175100000000     C                   Move      AMGAnno       GMAAnno
175200000000     C                   Move      AMGMese       GMAMese
175300000000     C                   Move      AMGGiorno     GMAGiorno
175400000000     C                   Z-Add     Prog_DA       Conta_Rigo
175500000000     C                   Z-Add     1             Conta_Colo
175600000000     C                   Clear                   Campo_Valore
175700000000     C                   Move      DtGMA         Campo_Valore
175800000000     C                   if        Campo_Valore <> *Blanks
175900000000     C                   EXSR      RieSkiera
176000000000     C                   endif
176100000000     C                   endif
176200000000     C*
176300000000     C* 2) Firma
176400000000     C                   if        $FirDic    <> *blank
176500000000     C                   Z-Add     Prog_DA       Conta_Rigo
176600000000     C                   Z-Add     2             Conta_Colo
176700000000     C                   Clear                   Campo_Valore
176800000000     C                   Move      '1'           Campo_Valore
176900000000     C                   EXSR      Ablank
177000000000     C                   if        Campo_Valore <> *Blanks
177100000000     C                   EXSR      RieSkiera
177200000000     C                   endif
177300000000     C                   endif
177400000000     C*
177500000000     C                   Move      *On           $FlDatiAna
177600000000     C*
177700000000     C                   endif
177800000000     C*
177900000000     C                   endif
178000000000     C*
178100000000     C                   ENDSR
178200000000     C/EJECT
178300000000     C******************************************************
178400000000     C* Scrittura t.r. H
178500000000     C******************************************************
178600000000     C     WrtTpRecH     BEGSR
178700000000     C*
178800000000     C                   exsr      GesImporti
178900160201R211 C* Dati aggiuntibi cu 2016
179000160201R211 C                   exsr      GesAltriDati
179100000000     C*
179200000000     C                   Clear                   SDGSK61DSH
179300000000     C* Tipo Record
179400000000     C                   Eval      K61HTR       = 'H'
179500000000     C*
179600000000     C* Cod.Fiscale
179700000000     C                   Eval      K61HCFI      = XScCdF
179800000000     C*
179900000000     C* Progressivo modulo
180000000000     C                   Eval      K61HPRGMOD   = 1
180100000000     C*
180200000000     C* Cod.Fiscale Percipiente
180300000000     C                   Eval      K61HCFP      = SavCdf
180400000000     C*
180500000000     C* Progressivo Certificazione
180600000000     C                   Eval      K61HPrgCer   = K61DPrgCer
180700000000     C*
180800000000     C                   MoveA     SkieraH       K61HDATI
180900000000     C*
181000000000     C* Carattere di controllo
181100000000     C                   MoveL     'A'           K61HK1
181200000000     C*-
181300000000     C* Elementi di fine riga
181400000000     C* Impostare i valori esadecimali '0D' e '0A'
181500000000     C* (caratteri ASCI 'CR' ed 'LF')
181600000000     C*
181700000000     C                   Eval      K61HK2  = *Blanks
181800000000     C                   Eval      K61HK2  = X'0D25'
181900000000     C*
182000000000     C                   Add       1             Cnt_H
182100000000     C                   Clear                   SDGCUW000
182200000000     C                   Movel     SDGSK61DSH    CUWREC
182300000000?    C                   write     SDGCUW000
182400000000     C                   feod      SDGCUW00F
182500000000     C*
182600000000     C                   ENDSR
182700000000     C/EJECT
182800000000     C******************************************************
182900000000     C* Inizializzo campi per la gestione della tabella
183000000000     C******************************************************
183100000000     C     InzSkieraH    BEGSR
183200000000     C*
183300000000     C                   Reset                   SkieraH
183400000000     C                   z-add     0             $H
183500000000     C*
183600000000     C                   move      *Off          $FlDatiImp
183700000000     C*
183800000000     C                   Eval      WImp004    =  0
183900000000     C                   Eval      WImp005    =  0
184000150224R169 C                   Eval      WImp0051   =  0
184100000000     C                   Eval      WImp006    =  0
184200000000     C                   Eval      WImp007    =  0
184300150224R169 C                   Eval      WImp0071   =  0
184400000000     C                   Eval      WImp008    =  0
184500000000     C                   Eval      WImp009    =  0
184600000000     C                   Eval      WImp010    =  0
184700000000     C                   Eval      WImp011    =  0
184800000000     C                   Eval      WImp012    =  0
184900000000     C                   Eval      WImp013    =  0
185000000000     C                   Eval      WImp014    =  0
185100000000     C                   Eval      WImp015    =  0
185200000000     C                   Eval      WImp016    =  0
185300000000     C                   Eval      WImp017    =  0
185400000000     C                   Eval      WImp018    =  0
185500000000     C                   Eval      WImp019    =  0
185600000000     C                   Eval      WImp020    =  0
185700000000     C                   Eval      WImp021    =  0
185800160129R211 X***                Eval      WImp022    =  0
185900160129R211 X***                Eval      WImp023    =  0
186000160129R211 C                   Eval      WImp034    =  0
186100160129  "  C                   Eval      WImp035    =  0
186200160129  "  C                   Eval      WImp037    =  0
186300160129  "  C                   Eval      WImp038    =  0
186400160129  "  C                   Eval      WImp039    =  0
186500160129  "  C                   Eval      WImp041    =  0
186600160129  "  C                   Eval      WImp042    =  0
186700160129  "  C                   Eval      WImp053    =  0
186800160129  "  C                   Eval      WImp054    =  0
186900160129  "  C                   Eval      WImp055    =  0
187000160129  "  C                   Eval      WImp056    =  0
187100160129  "  C                   Eval      WImp057    =  0
187200160129  "  C                   Eval      WImp058    =  0
187300160129  "  C                   Eval      WImp059    =  0
187400160129  "  C                   Eval      WImp060    =  0
187500160129  "  C                   Eval      WImp061    =  0
187600160129  "  C                   Eval      WImp062    =  0
187700160129  "  C                   Eval      WImp102    =  0
187800170125R211 C                   Eval      WImp103    =  0
187900170125R250 C                   Eval      WImp104    =  0
188000170125R211 C                   Eval      WImp106    =  0
188100170125R211 C                   Eval      WImp107    =  0
188200170125R250 C                   Eval      WImp108    =  0
188300170125R211 C                   Eval      WImp131    =  0
188400160129  "  C                   Eval      WImp132    =  0
188500160129  "  C                   Eval      WImp133    =  0
188600160129  "  C                   Eval      WImp134    =  0
188700160129  "  C                   Eval      WImp135    =  0
188800160129  "  C                   Eval      WImp136    =  0
188900160129  "  C                   Eval      WImp137    =  0
189000160201  "  C                   Eval      WImp138    =  0
189100160201  "  C*
189200160201  "  C                   Clear                   WDpCdf
189300160201  "  C                   Clear                   WDpDen
189400160201  "  C                   Clear                   WDpCenP
189500160201  "  C                   Clear                   WDpCazi
189600160201  "  C                   Clear                   WDpCate
189700160201  "  C                   Clear                   WDpAcon
189800160201  "  C                   Clear                   WAsCdf
189900160201  "  C                   Clear                   WOsCdf1
190000160201  "  C                   Clear                   WOsCdf2
190100160201  "  C                   Clear                   WOsCdf3
190200170125R211 C                   Clear                   WF2Cdf
190300170125R250 X*** ex r211        Clear                   WF2Rino
190400170125R211 C                   Clear                   WF2Cdf2
190500170125R250 X*** ex r211        Clear                   WF2Rino2
190600000000     C*
190700000000     C                   ENDSR
190800000000     C/EJECT
190900000000     C******************************************************
191000000000     C* Dati relativi alle certificazioni
191100000000     C******************************************************
191200000000     C     ElaboraRecH   BEGSR
191300000000     C*
191400000000     C                   Eval      WSavQuaH  = 'AU'
191500000000     C*
191600000000     C                   if        $Fine770  = *Off
191700000000     C*
191800000000     C* AU001
191900000000     C*
192000000000     C                   Z-Add     1             Prog_AU
192100000000     C*
192200000000     C                   if        $FlDatiImp    = *Off
192300000000     C*
192400000000     C* 1) Causale
192500000000     C                   if        N77C77        <> *Blanks
192600000000     C                   Z-Add     Prog_AU       Conta_Rigo
192700000000     C                   Z-Add     1             Conta_Colo
192800000000     C                   Clear                   Campo_Valore
192900000000     C****               MoveL     N77C77        Campo_Valore
193000000000     C                   eval      Campo_Valore = %subst(N77C77:1:2)
193100000000     C                   if        Campo_Valore <> *Blanks
193200000000     C                   EXSR      RieSkieraH
193300000000     C                   endif
193400000000     C                   endif
193500000000     C*
193600000000     C* 2) Anno
193700150224R169 X****               if        N77Acf        <> 0
193800150224R169 C                   if        N77Anno       <> 0
193900000000     C                   Z-Add     Prog_AU       Conta_Rigo
194000000000     C                   Z-Add     2             Conta_Colo
194100000000     C                   Clear                   Campo_Valore
194200150223R169 X****               Move      N77Anno       Campo_Valore
194300150224R169 X****               Move      N77Acf        Campo_Valore
194400150224R169 C                   Move      N77Anno       Campo_Valore
194500000000     C                   EXSR      Ablank
194600000000     C                   if        Campo_Valore <> *Blanks
194700000000     C                   EXSR      RieSkieraH
194800000000     C                   endif
194900000000     C                   endif
195000000000     C*
195100000000     C* 3) Anticipazione
195200000000     C                   if        N77Tit    <> *blank
195300000000     C                   Z-Add     Prog_AU       Conta_Rigo
195400000000     C                   Z-Add     3             Conta_Colo
195500000000     C                   Clear                   Campo_Valore
195600000000     C                   Move      '1'           Campo_Valore
195700000000     C                   EXSR      Ablank
195800000000     C                   if        Campo_Valore <> *Blanks
195900000000     C                   EXSR      RieSkieraH
196000000000     C                   endif
196100000000     C                   endif
196200000000     C*
196300000000     C                   Move      *On           $FlDatiImp
196400000000     C*
196500000000     C                   endif
196600000000     C*
196700000000     C*-----------------------------------
196800000000     C* MEMORIZZA IMPORTI
196900000000     C*
197000000000     C* 4) Ammontare lordo corrisposto
197100000000     C                   add       N77Itc        WImp004
197200000000     C*
197300000000     C* 5) Somme non soggette a ritenuta per regime convenzionale
197400000000     C                   setoff                                       202122
197500000000     C     N77Num        comp      *blank                             2020
197600000000     C   20N77Rac        comp      0                                      21
197700000000     C   20N77Imn        comp      0                                  22
197800000000     C*    Straniero senza ritenute e con imp. non soggetto
197900000000     C   20
198000000000     Can 21
198100150224R169 Can 22N77Imn        add       N77SNS        WImp0051
198200150224R169 X****an 22N77Imn        add       N77SNS        WImp005
198300150224 "   C   20
198400150224 "   Can 21
198500150224R169 Can 22              add       WImp0051      WImp005
198600000000     C*
198700000000     C* 7) Altre somme non soggette a ritenute
198800000000     C* Italia
198900150224R169 X****  N20N77IMN        add       N77SNS        WImp007
199000150224R169 C  N20N77IMN        add       N77SNS        WImp0071
199100150224R169 C  N20              add       WImp0071      WImp007
199200000000     C* Straniero senza ritenute senza imp. non soggetto
199300000000     C   20
199400000000     CAN 21
199500150224R169 CANN22N77IMN        add       N77SNS        WImp0071
199600150224R169 X****ANN22N77IMN        add       N77SNS        WImp007
199700150224 "   C   20
199800150224 "   CAN 21
199900150224R169 CANN22              add       WImp0071      WImp007
200000000000     C* STRANIERO CON RITENUTE
200100000000     C   20
200200150224R169 CANN21N77IMN        add       N77SNS        WImp0071
200300150224R169 X****ANN21N77IMN        add       N77SNS        WImp007
200400150224 "   C   20
200500150224R169 CANN21              add       WImp0071      WImp007
200600000000     C*
200700000000     C* 8) Imponibile
200800000000     C                   add       N77Ims        WImp008
200900000000     C*
201000000000     C* 9) Ritenute a titolo d'acconto
201100000000     C                   add       N77Rac        WImp009
201200000000     C*
201300000000     C* 10) Ritenute a titolo di imposta
201400000000     C                   add       N77Rim        WImp010
201500000000     C*
201600000000     C* 11) Ritenute sospese
201700000000     C                   add       N77Net        WImp011
201800000000     C*
201900000000     C* 12) Addizionale regionale a titolo d'acconto
202000000000     C                   add       N77Arg        WImp012
202100000000     C*
202200000000     C* 13) Addizionale regionale a titolo di imposta
202300000000     C                   add       N77Ari        WImp013
202400000000     C*
202500000000     C* 14) Addizionale regionale sospesa
202600000000     C                   add       N77Ars        WImp014
202700000000     C*
202800000000     C* 15) Addizionale comunale a titolo d'acconto
202900000000     C                   add       N77Acta       WImp015
203000000000     C*
203100000000     C* 16) Addizionale comunale a titolo di imposta
203200000000     C                   add       N77Acg        WImp016
203300000000     C*
203400000000     C* 17) Addizionale comunale sospesa
203500000000     C                   add       N77Acs        WImp017
203600000000     C*
203700000000     C* 18) Imponibile anni precedenti
203800000000     C                   add       N77Imsap      WImp018
203900000000     C*
204000000000     C* 19) Ritenute operate anni precedenti
204100000000     C                   add       N77Racap      WImp019
204200000000     C*
204300160129R211 X**** 20) Contributi previdenziali a carico del soggetto erogante
204400160129R211 X****               add       N77Ctbdl      WImp020
204500160202     C*
204600160129R211 X**** 21) Contributi previdenziali a carico del percipiente
204700160129R211 X****               add       N77Ctblv      WImp021
204800160129     C*
204900160129R211 X**** 22) Spese rimborsate
205000160129R211 X***                add       N77SpeRimb    WImp022
205100000000     C*
205200160129R211 X**** 23) Ritenute rimborsate
205300160129R211 X****               add       N77RitRimb    WImp023
205400000000     C*
205500160129R211 C* 20) Spese rimborsate
205600160129  "  C                   add       N77SpeRimb    WImp020
205700160129  "  C*
205800160129  "  C* 21) Ritenute rimborsate
205900160129  "  C                   add       N77RitRimb    WImp021
206000160129  "  C*
206100160129  "  C* 34) Contributi previdenziali a carico del soggetto erogante
206200160202  "  C                   add       N77Ctbdl      WImp034
206300160129  "  C*
206400160129  "  C* 35) Contributi previdenziali a carico del percipiente
206500160202  "  C                   add       N77Ctblv      WImp035
206600160129  "  C*
206700160129  "  C* 37) Importo altri contributi
206800160201  "  C                   add       N77DpImpa     WImp037
206900160129  "  C*
207000160129  "  C* 38) Contributi dovuti
207100160129  "  C                   add       N77DpCodu     WImp038
207200160129  "  C*
207300160129  "  C* 39) Contributi versati
207400160129  "  C                   add       N77DpCove     WImp039
207500160129  "  C*
207600160129  "  C* 41) Somme corrisposte prima del fallimento
207700160201  "  C                   add       N77ImpPrFa    WImp041
207800160129  "  C*
207900160217R211 C* 42) Somme corrisposte dal curatore/commissario
208000160217R215 C*                  add       N77ImpCuCo    WImp038
208100160217R215 C                   add       N77ImpCuCo    WImp042
208200160217R211 C*
208300160129  "  C* 53) Redditi da altri soggetti: Imponibile
208400160129  "  C                   add       N77AsIms      WImp053
208500160129  "  C*
208600160129  "  C* 54) Redditi da altri soggetti: Ritenute a titolo di acconto
208700160129  "  C                   add       N77AsRac      WImp054
208800160216R211 C*
208900160216R215 X*** ex r211 55) Redditi da altri soggetti: Ritenute sospese
209000160216  "  X*** ex r211                 add       N77AsRso      WImp055
209100160216  "  X*** ex r211
209200160216  "  X*** ex r211 56) Redditi da altri soggetti: Addizionale regionale a titolo di acconto
209300160216  "  X*** ex r211                 add       N77AsAra      WImp056
209400160216  "  X*** ex r211
209500160216  "  X*** ex r211 57) Redditi da altri soggetti: Addizionale regionale a titolo di imposta
209600160216  "  X*** ex r211                 add       N77AsAri      WImp057
209700160216  "  X*** ex r211
209800160216  "  X*** ex r211
209900160216  "  X*** ex r211 58) Redditi da altri soggetti: Addizionale regionale sospesa
210000160216  "  X*** ex r211                add       N77AsArs      WImp058
210100160216  "  X*** ex r211
210200160216  "  X*** ex r211 59) Redditi da altri soggetti: Addizionale comunale a titolo di acconto
210300160216  "  X*** ex r211                 add       N77AsAca      WImp059
210400160216  "  X*** ex r211
210500160216  "  X*** ex r211 60) Redditi da altri soggetti: Addizionale comunale a titolo di imposta
210600160216  "  X*** ex r211                 add       N77AsAci      WImp060
210700160216  "  X*** ex r211
210800160216  "  X**** ex r211 61) Redditi da altri soggetti: Addizionale comunale sospesa
210900160216R215 X**** ex r211             add       N77AsAcs      WImp061
211000160216R215 C*
211100160216  "  C* 55) Redditi da altri soggetti: Ritenute a titolo di imposta
211200160216  "  C                   add       N77AsRim      WImp055
211300160129  "  C*
211400160216  "  C* 56) Redditi da altri soggetti: Ritenute sospese
211500160216  "  C                   add       N77AsRso      WImp056
211600160216  "  C*
211700160216  "  C* 57) Redditi da altri soggetti: Addizionale regionale a titolo di acconto
211800160216  "  C                   add       N77AsAra      WImp057
211900160216  "  C*
212000160216  "  C* 58) Redditi da altri soggetti: Addizionale regionale a titolo di imposta
212100160216  "  C                   add       N77AsAri      WImp058
212200160216  "  C*
212300160216  "  C*
212400160216  "  C* 59) Redditi da altri soggetti: Addizionale regionale sospesa
212500160216  "  C                   add       N77AsArs      WImp059
212600160216  "  C*
212700160216  "  C* 60) Redditi da altri soggetti: Addizionale comunale a titolo di acconto
212800160216  "  C                   add       N77AsAca      WImp060
212900160216  "  C*
213000160216  "  C* 61) Redditi da altri soggetti: Addizionale comunale a titolo di imposta
213100160216  "  C                   add       N77AsAci      WImp061
213200160216  "  C*
213300160216  "  C* 62) Redditi da altri soggetti: Addizionale comunale sospesa
213400160216R215 C                   add       N77AsAcs      WImp062
213500160216R211 C* 102) Pignoramento verso terzi : Somme erogate
213600160129  "  C                   add       N77F2Ime      WImp102
213700160129  "  C*
213800160129  "  C* 103) Pignoramento verso terzi : Ritenute operate
213900160129  "  C                   add       N77F2Rio      WImp103
214000170125R211 C*
214100170125R250 C* 104) Pignoramento verso terzi : Somme erogate non tassate
214200170125  "  C                   add       N77F2Imnt     WImp104
214300170125R250 C*
214400170125R211 C* 106) Pignoramento verso terzi : Somme erogate
214500160129  "  C                   add       N77F2Ime2     WImp106
214600160129  "  C*
214700160129  "  C* 107) Pignoramento verso terzi : Ritenute operate
214800160129  "  C                   add       N77F2Rio2     WImp107
214900170125R211 C*
215000170125R250 C* 108) Pignoramento verso terzi : Somme erogate non tassate
215100170125  "  C                   add       N77F2Imn2     WImp108
215200170125R250 C*
215300170125R211 C* 131) Indennità di esproprio   : Somme corrisposte
215400160129  "  C                   add       N77F3Ine      WImp131
215500160129  "  C*
215600160129  "  C* 132) Indennità di esproprio   : Ritenute operate
215700160129  "  C                   add       N77F3Rie      WImp132
215800160129  "  C*
215900160129  "  C* 133) Altre indennità e interessi: Somme corrisposte
216000160129  "  C                   add       N77F3Ain      WImp133
216100160129  "  C*
216200160129  "  C* 134) Indennità di esproprio   : Ritenute operate
216300160129  "  C                   add       N77F3Ari      WImp134
216400160129  "  C*
216500160129  "  C* 135) Indennità di esproprio   : Somme corrisposte
216600160129  "  C                   add       N77F3Ine2     WImp135
216700160129  "  C*
216800160129  "  C* 136) Indennità di esproprio   : Ritenute operate
216900160129  "  C                   add       N77F3Rie2     WImp136
217000160129  "  C*
217100160129  "  C* 137) Altre indennità e interessi: Somme corrisposte
217200160129  "  C                   add       N77F3Ain2     WImp137
217300160129  "  C*
217400160129  "  C* 138) Indennità di esproprio   : Ritenute operate
217500160129  "  C                   add       N77F3Ari2     WImp138
217600160201  "  C*
217700160201  "  C* Memorizza campi alfabetici
217800160201  "  C*
217900160201  "  C*  29) Codice fiscale Ente previdenziale
218000160201  "  C                   Eval      WDpCdf    =   N77DpCdf
218100160201  "  C*  30) Denomimazione Ente previdenziale
218200170125R211 C                   Eval      WDpDen    =   N77DpDen
218300170125R250 X****  31) Codice Ente Previdenziale
218400170125R250 X***                Eval      WDpCenP   =   N77DpCenP
218500170125R211 C*  32) Codice Azienda
218600160201  "  C                   Eval      WDpCazi   =   N77DpCazi
218700160201  "  C*  33) Categoria
218800160201  "  C                   Eval      WDpCate   =   N77DpCate
218900160201  "  C*  36) Altri contributi
219000160201  "  C                   Eval      WDpAcon   =   N77DpAcon
219100160201  "  C*  52) Codice fiscale redditi erogati da altri soggetti
219200160201  "  C                   Eval      WAsCdf    =   N77AsCdf
219300160201  "  C*  71) Codice fiscale (sez. lavoro autonomo e redd. diversi)
219400160201  "  C                   Eval      WOsCdf1  =   N77OsCdf1
219500160201  "  C*  72) Codice fiscale (sez. pignoramento verso terzi)
219600160201  "  C                   Eval      WOsCdf2  =   N77OsCdf2
219700160201  "  C*  73) Codice fiscale (sez. indennità di esproprio)
219800160201  "  C                   Eval      WOsCdf3  =   N77OsCdf3
219900160201  "  C* 101) Codice fiscale debitore principale
220000170125R211 C                   Eval      WF2Cdf   =   N77F2Cdf
220100170125R250 X*** ex r211 * 104) Ritenute non operate
220200170125R250 X*** ex r211        Eval      WF2Rino  =   N77F2Rino
220300170125R211 C* 105) Codice fiscale debitore principale
220400170125R211 C                   Eval      WF2Cdf2  =   N77F2Cdf2
220500170125R250 X*** ex r211 * 108) Ritenute non operate
220600170125R250 X*** ex r211        Eval      WF2Rino2 =   N77F2Rino2
220700000000     C                   endif
220800000000     C*
220900000000     C                   ENDSR
221000000000     C/EJECT
221100000000     C******************************************************
221200000000     C* Tratta campi alfanumerici
221300000000     C******************************************************
221400000000     C     RieSkieraAlf  BEGSR
221500000000     C*
221600000000     C                   Z-ADD     Conta_Rigo    Cod_Rigo
221700000000     C                   Z-ADD     Conta_Colo    Cod_Colo
221800000000     C*
221900000000     C                   Add       1             $I
222000000000     C                   MoveL     Campo_Codice  Skiera($I)
222100000000     C                   MoveL     WSavQuaD      Skiera($I)
222200000000     C                   Move      Campo_Valore  Skiera($I)
222300000000     C                   Clear                   Campo_Valore
222400000000     C*
222500000000     C                   Clear                   Campo
222600000000     C*
222700000000     C* Controllo se ci sono dati dopo la 17° posizione
222800000000     C                   Do        3             XY                1 0
222900000000     C*
223000000000     C                   if        C15(XY) <> *Blanks
223100000000     C                   Add       1             $I
223200000000     C                   MOVEL     '+'           C16              16
223300000000     C                   MOVE      C15(XY)       C16
223400000000     C                   MoveL     Campo_Codice  Skiera($I)
223500000000     C                   MoveL     WSavQuaD      Skiera($I)
223600000000     C                   Move      C16           Skiera($I)
223700000000     C                   endif
223800000000     C*
223900000000     C                   endDo
224000000000     C*
224100000000     C                   Clear                   C60
224200000000     C*
224300000000     C                   ENDSR
224400000000     C/EJECT
224500000000     C******************************************************
224600000000     C* Riempimento skiera per i dati non posizionali
224700000000     C******************************************************
224800000000     C     RieSkiera     BEGSR
224900000000     C*
225000000000     C                   Z-ADD     Conta_Rigo    Cod_Rigo
225100000000     C                   Z-ADD     Conta_Colo    Cod_Colo
225200000000     C*
225300000000     C                   ADD       1             $I
225400000000     C                   MoveL     Campo_Codice  Skiera($I)
225500000000     C                   MoveL     WSavQuaD      Skiera($I)
225600000000     C                   Move      Campo_Valore  Skiera($I)
225700000000     C                   Clear                   Campo_Valore
225800000000     C*
225900000000     C                   ENDSR
226000000000      /EJECT
226100000000     C******************************************************
226200000000     C* Tratta campi alfanumerici
226300000000     C******************************************************
226400000000     C     RieSkieraAlfH BEGSR
226500000000     C*
226600000000     C                   Z-ADD     Conta_Rigo    Cod_Rigo
226700000000     C                   Z-ADD     Conta_Colo    Cod_Colo
226800000000     C*
226900000000     C                   Add       1             $H
227000000000     C                   MoveL     Campo_Codice  SkieraH($H)
227100000000     C                   MoveL     WSavQuaH      SkieraH($H)
227200000000     C                   Move      Campo_Valore  SkieraH($H)
227300000000     C                   Clear                   Campo_Valore
227400000000     C*
227500000000     C                   Clear                   Campo
227600000000     C*
227700000000     C* Controllo se ci sono dati dopo la 17° posizione
227800000000     C                   Do        3             XY                1 0
227900000000     C*
228000000000     C                   if        C15(XY) <> *Blanks
228100000000     C                   Add       1             $H
228200000000     C                   MOVEL     '+'           C16              16
228300000000     C                   MOVE      C15(XY)       C16
228400000000     C                   MoveL     Campo_Codice  SkieraH($H)
228500000000     C                   MoveL     WSavQuaH      SkieraH($H)
228600000000     C                   Move      C16           SkieraH($H)
228700000000     C                   endif
228800000000     C*
228900000000     C                   endDo
229000000000     C*
229100000000     C                   Clear                   C60
229200000000     C*
229300000000     C                   ENDSR
229400000000     C/EJECT
229500000000     C******************************************************
229600000000     C* Riempimento skiera per i dati non posizionali
229700000000     C******************************************************
229800000000     C     RieSkieraH    BEGSR
229900000000     C*
230000000000     C                   Z-ADD     Conta_Rigo    Cod_Rigo
230100000000     C                   Z-ADD     Conta_Colo    Cod_Colo
230200000000     C*
230300000000     C                   ADD       1             $H
230400000000     C                   MoveL     Campo_Codice  SkieraH($H)
230500000000     C                   MoveL     WSavQuaH      SkieraH($H)
230600000000     C                   Move      Campo_Valore  SkieraH($H)
230700000000     C                   Clear                   Campo_Valore
230800000000     C*
230900000000     C                   ENDSR
231000000000      /EJECT
231100000000     C******************************************************
231200000000     C* Valorizzo i campi importi
231300000000     C******************************************************
231400000000     C     GesImporti    BEGSR
231500000000     C*
231600000000     C* AU001
231700000000     C*
231800000000     C* 4) Ammontare lordo corrisposto
231900000000     C                   Eval      WImporto      = WImp004
232000000000     C*
232100000000     C                   if        WImporto      <> 0
232200000000     C                   Z-Add     Prog_AU       Conta_Rigo
232300000000     C                   Z-Add     4             Conta_Colo
232400000000     C                   EXSR      RieImportoH
232500000000     C                   endif
232600000000     C*
232700000000     C* 5) Somme non soggette a ritenuta per regime convenzionale
232800000000     C                   Eval      WImporto      = WImp005
232900000000     C*
233000000000     C                   if        WImporto      <> 0
233100000000     C                   Z-Add     Prog_AU       Conta_Rigo
233200000000     C                   Z-Add     5             Conta_Colo
233300000000     C                   EXSR      RieImportoH
233400000000     C                   endif
233500000000     C*
233600000000     C* 6) Codice
233700000000     C                   if        SavCodice <> *blank
233800000000     C                   Z-Add     Prog_AU       Conta_Rigo
233900000000     C                   Z-Add     6             Conta_Colo
234000000000     C                   Clear                   Campo_Valore
234100000000     C                   Move      SavCodice     Campo_Valore
234200000000     C                   EXSR      Ablank
234300000000     C                   if        Campo_Valore <> *Blanks
234400000000     C                   EXSR      RieSkieraH
234500000000     C                   endif
234600000000     C                   endif
234700000000     C*
234800000000     C* 7) Altre somme non soggette a ritenute
234900000000     C                   Eval      WImporto      = WImp007
235000000000     C*
235100000000     C                   if        WImporto      <> 0
235200000000     C                   Z-Add     Prog_AU       Conta_Rigo
235300000000     C                   Z-Add     7             Conta_Colo
235400000000     C                   EXSR      RieImportoH
235500000000     C                   endif
235600000000     C*
235700000000     C* 8) Imponibile
235800000000     C                   Eval      WImporto      = WImp008
235900000000     C*
236000000000     C                   if        WImporto      <> 0
236100000000     C                   Z-Add     Prog_AU       Conta_Rigo
236200000000     C                   Z-Add     8             Conta_Colo
236300000000     C                   EXSR      RieImportoH
236400000000     C                   endif
236500000000     C*
236600000000     C* 9) Ritenute a titolo d'acconto
236700000000     C                   Eval      WImporto      = WImp009
236800000000     C*
236900000000     C                   if        WImporto      <> 0
237000000000     C                   Z-Add     Prog_AU       Conta_Rigo
237100000000     C                   Z-Add     9             Conta_Colo
237200000000     C                   EXSR      RieImportoH
237300000000     C                   endif
237400000000     C*
237500000000     C* 10) Ritenute a titolo di imposta
237600000000     C                   Eval      WImporto      = WImp010
237700000000     C*
237800000000     C                   if        WImporto      <> 0
237900000000     C                   Z-Add     Prog_AU       Conta_Rigo
238000000000     C                   Z-Add     10            Conta_Colo
238100000000     C                   EXSR      RieImportoH
238200000000     C                   endif
238300000000     C*
238400000000     C* 11) Ritenute sospese
238500000000     C                   Eval      WImporto      = WImp011
238600000000     C*
238700000000     C                   if        WImporto      <> 0
238800000000     C                   Z-Add     Prog_AU       Conta_Rigo
238900000000     C                   Z-Add     11            Conta_Colo
239000000000     C                   EXSR      RieImportoH
239100000000     C                   endif
239200000000     C*
239300000000     C* 12) Addizionale regionale a titolo d'acconto
239400000000     C                   Eval      WImporto      = WImp012
239500000000     C*
239600000000     C                   if        WImporto      <> 0
239700000000     C                   Z-Add     Prog_AU       Conta_Rigo
239800000000     C                   Z-Add     12            Conta_Colo
239900000000     C                   EXSR      RieImportoH
240000000000     C                   endif
240100000000     C*
240200000000     C* 13) Addizionale regionale a titolo di imposta
240300000000     C                   Eval      WImporto      = WImp013
240400000000     C*
240500000000     C                   if        WImporto      <> 0
240600000000     C                   Z-Add     Prog_AU       Conta_Rigo
240700000000     C                   Z-Add     13            Conta_Colo
240800000000     C                   EXSR      RieImportoH
240900000000     C                   endif
241000000000     C*
241100000000     C* 14) Addizionale regionale sospesa
241200000000     C                   Eval      WImporto      = WImp014
241300000000     C*
241400000000     C                   if        WImporto      <> 0
241500000000     C                   Z-Add     Prog_AU       Conta_Rigo
241600000000     C                   Z-Add     14            Conta_Colo
241700000000     C                   EXSR      RieImportoH
241800000000     C                   endif
241900000000     C*
242000000000     C* 15) Addizionale comunale a titolo d'acconto
242100000000     C                   Eval      WImporto      = WImp015
242200000000     C*
242300000000     C                   if        WImporto      <> 0
242400000000     C                   Z-Add     Prog_AU       Conta_Rigo
242500000000     C                   Z-Add     15            Conta_Colo
242600000000     C                   EXSR      RieImportoH
242700000000     C                   endif
242800000000     C*
242900000000     C* 16) Addizionale comunale a titolo di imposta
243000000000     C                   Eval      WImporto      = WImp016
243100000000     C*
243200000000     C                   if        WImporto      <> 0
243300000000     C                   Z-Add     Prog_AU       Conta_Rigo
243400000000     C                   Z-Add     16            Conta_Colo
243500000000     C                   EXSR      RieImportoH
243600000000     C                   endif
243700000000     C*
243800000000     C* 17) Addizionale comunale sospesa
243900000000     C                   Eval      WImporto      = WImp017
244000000000     C*
244100000000     C                   if        WImporto      <> 0
244200000000     C                   Z-Add     Prog_AU       Conta_Rigo
244300000000     C                   Z-Add     17            Conta_Colo
244400000000     C                   EXSR      RieImportoH
244500000000     C                   endif
244600000000     C*
244700000000     C* 18) Imponibile anni precedenti
244800000000     C                   Eval      WImporto      = WImp018
244900000000     C*
245000000000     C                   if        WImporto      <> 0
245100000000     C                   Z-Add     Prog_AU       Conta_Rigo
245200000000     C                   Z-Add     18            Conta_Colo
245300000000     C                   EXSR      RieImportoH
245400000000     C                   endif
245500000000     C*
245600000000     C* 19) Ritenute operate anni precedenti
245700000000     C                   Eval      WImporto      = WImp019
245800000000     C*
245900000000     C                   if        WImporto      <> 0
246000000000     C                   Z-Add     Prog_AU       Conta_Rigo
246100000000     C                   Z-Add     19            Conta_Colo
246200000000     C                   EXSR      RieImportoH
246300000000     C                   endif
246400000000     C*
246500160129R211 X**** 20) Contributi previdenziali a carico del soggetto erogante
246600160129R211 C* 20) Spese rimporsate
246700000000     C                   Eval      WImporto      = WImp020
246800000000     C*
246900000000     C                   if        WImporto      <> 0
247000000000     C                   Z-Add     Prog_AU       Conta_Rigo
247100000000     C                   Z-Add     20            Conta_Colo
247200000000     C                   EXSR      RieImportoH
247300000000     C                   endif
247400000000     C*
247500160129R211 X**** 21) Contributi previdenziali a carico del percipiente
247600160129R211 C* 21) Spese rimporsate
247700000000     C                   Eval      WImporto      = WImp021
247800000000     C*
247900000000     C                   if        WImporto      <> 0
248000000000     C                   Z-Add     Prog_AU       Conta_Rigo
248100000000     C                   Z-Add     21            Conta_Colo
248200000000     C                   EXSR      RieImportoH
248300000000     C                   endif
248400000000     C*
248500160129R211 X**** 22) Spese rimborsate
248600160129R211 X****               Eval      WImporto      = WImp022
248700000000     C*
248800160129R211 X****               if        WImporto      <> 0
248900160129R211 X****               Z-Add     Prog_AU       Conta_Rigo
249000160129R211 X****               Z-Add     22            Conta_Colo
249100160129R211 X****               EXSR      RieImportoH
249200160129R211 X****               endif
249300000000     C*
249400160129R211 X****  23) Ritenute rimborsate
249500160129R211 X****               Eval      WImporto      = WImp023
249600000000     C*
249700160129R211 X****               if        WImporto      <> 0
249800160129R211 X****               Z-Add     Prog_AU       Conta_Rigo
249900160129R211 X****               Z-Add     23            Conta_Colo
250000160129R211 X****               EXSR      RieImportoH
250100160129R211 X****               endif
250200000000     C*
250300000000     C*
250400000000     C                   ENDSR
250500000000     C/EJECT
250600160201R211 C******************************************************
250700160201  "  C* Valorizzo i dati aggiuntivi Cu 2016
250800160201  "  C******************************************************
250900160201R211 C     GesAltriDati  BEGSR
251000160201     C*
251100160201     C* AU0XXX
251200160201     C*
251300160201     C* 29) Codice fiscale ente previdenziale
251400160201     C                   If        WdpCdf <> *blanks
251500160201     C                   Z-Add     Prog_AU       Conta_Rigo
251600160201     C                   Z-Add     29            Conta_Colo
251700160201     C                   MoveL     WDpCdf        Campo_Valore
251800160201     C                   MoveL     WDpCdf        C60
251900160201     C                   if        Campo_Valore <> *Blanks
252000160201     C                   EXSR      RieSkieraAlfH
252100160201     C                   endif
252200160201     C                   endif
252300160201     C* 30) Denominazione Ente previdenziale
252400160201     C                   If        WdpDen <> *blanks
252500160201     C                   Z-Add     Prog_AU       Conta_Rigo
252600160201     C                   Z-Add     30            Conta_Colo
252700160201     C                   MoveL     WDpDen        Campo_Valore
252800160201     C                   MoveL     WDpDen        C60
252900160201     C                   if        Campo_Valore <> *Blanks
253000160201     C                   EXSR      RieSkieraAlfH
253100160201     C                   endif
253200160201     C                   endif
253300170125R250 X*** * 31) Codice Ente previdenziale
253400170125  "  X***                If        WDpCenP <> *blanks
253500170125  "  X***                Z-Add     Prog_AU       Conta_Rigo
253600170125  "  X***                Z-Add     31            Conta_Colo
253700170125  "  X***                MoveL     WDpCenP       Campo_Valore
253800170125  "  X***                MoveL     WDpCenP       C60
253900170125  "  X***                if        Campo_Valore <> *Blanks
254000170125  "  X***                EXSR      RieSkieraAlfH
254100170125  "  X***                endif
254200170125R250 X***                endif
254300160201     C* 32) Codice Azienda
254400160201     C                   If        WDpCazi <> *blanks
254500160201     C                   Z-Add     Prog_AU       Conta_Rigo
254600160201     C                   Z-Add     32            Conta_Colo
254700160201     C                   MoveL     WDpCazi       Campo_Valore
254800160201     C                   MoveL     WDpCazi       C60
254900160201     C                   if        Campo_Valore <> *Blanks
255000160201     C                   EXSR      RieSkieraAlfH
255100160201     C                   endif
255200160201     C                   endif
255300160201     C* 33) Categoria
255400160201     C                   If        WDpCate <> *blanks
255500160201     C                   Z-Add     Prog_AU       Conta_Rigo
255600160201     C                   Z-Add     33            Conta_Colo
255700160201     C                   MoveL     WDpCate       Campo_Valore
255800160201     C                   MoveL     WDpCate       C60
255900160201     C                   if        Campo_Valore <> *Blanks
256000160201     C                   EXSR      RieSkieraAlfH
256100160201     C                   endif
256200160201     C                   endif
256300160201     C*
256400160201     C* 34) Contributi previdenziali a carico del soggetto erogante
256500160201     C                   Eval      WImporto      = WImp034
256600160201     C*
256700160201     C                   if        WImporto      <> 0
256800160201     C                   Z-Add     Prog_AU       Conta_Rigo
256900160201     C                   Z-Add     34            Conta_Colo
257000160201     C                   EXSR      RieImportoH
257100160201     C                   endif
257200160201     C*
257300160201     C* 35) Contributi previdenziali a carico del percipiente
257400160201     C                   Eval      WImporto      = WImp035
257500160201     C*
257600160201     C                   if        WImporto      <> 0
257700160201     C                   Z-Add     Prog_AU       Conta_Rigo
257800160201     C                   Z-Add     35            Conta_Colo
257900160201     C                   EXSR      RieImportoH
258000160201     C                   endif
258100160201     C*
258200160201     C* 36) Altri contributi
258300160202     C                   if        WDpAcon <> *blank
258400160201     C                   Z-Add     Prog_AU       Conta_Rigo
258500160201     C                   Z-Add     36            Conta_Colo
258600160201     C                   Clear                   Campo_Valore
258700160201     C                   Move      '1'           Campo_Valore
258800160201     C                   EXSR      Ablank
258900160201     C                   if        Campo_Valore <> *Blanks
259000160201     C                   EXSR      RieSkieraH
259100160201     C                   endif
259200160201     C                   endif
259300160201     C* 37) Importo altri contributi
259400160201     C                   Eval      WImporto      = WImp037
259500160201     C*
259600160201     C                   if        WImporto      <> 0
259700160201     C                   Z-Add     Prog_AU       Conta_Rigo
259800160201     C                   Z-Add     37            Conta_Colo
259900160201     C                   EXSR      RieImportoH
260000160201     C                   endif
260100160201     C* 38) Contributi dovuti
260200160201     C                   Eval      WImporto      = WImp038
260300160201     C*
260400160201     C                   if        WImporto      <> 0
260500160201     C                   Z-Add     Prog_AU       Conta_Rigo
260600160201     C                   Z-Add     38            Conta_Colo
260700160201     C                   EXSR      RieImportoH
260800160201     C                   endif
260900160201     C* 39) Contributi versati
261000160201     C                   Eval      WImporto      = WImp039
261100160201     C*
261200160201     C                   if        WImporto      <> 0
261300160201     C                   Z-Add     Prog_AU       Conta_Rigo
261400160201     C                   Z-Add     39            Conta_Colo
261500160201     C                   EXSR      RieImportoH
261600160201     C                   endif
261700160201     C* 41) Somme corrisposte prima del fallimento
261800160201     C                   Eval      WImporto      = WImp041
261900160201     C*
262000160201     C                   if        WImporto      <> 0
262100160201     C                   Z-Add     Prog_AU       Conta_Rigo
262200160201     C                   Z-Add     41            Conta_Colo
262300160201     C                   EXSR      RieImportoH
262400160201     C                   endif
262500160201     C* 42) Somme corrisposte prima dal curatore
262600160201     C                   Eval      WImporto      = WImp042
262700160201     C*
262800160201     C                   if        WImporto      <> 0
262900160201     C                   Z-Add     Prog_AU       Conta_Rigo
263000160201     C                   Z-Add     42            Conta_Colo
263100160201     C                   EXSR      RieImportoH
263200160201     C                   endif
263300160201     C*
263400160201     C* 52) Redditi erogati da altri soggetti: Codice Fiscale
263500160201     C                   If        WAsCdf <> *blanks
263600160201     C                   Z-Add     Prog_AU       Conta_Rigo
263700160201     C                   Z-Add     52            Conta_Colo
263800160201     C                   MoveL     WAsCdf        Campo_Valore
263900160201     C                   MoveL     WAsCdf        C60
264000160201     C                   if        Campo_Valore <> *Blanks
264100160201     C                   EXSR      RieSkieraAlfH
264200160201     C                   endif
264300160201     C                   endif
264400160201     C*
264500160201     C* 53) Redditi erogati da altri soggetti: Imponibile
264600160201     C                   Eval      WImporto      = WImp053
264700160201     C*
264800160201     C                   if        WImporto      <> 0
264900160201     C                   Z-Add     Prog_AU       Conta_Rigo
265000160201     C                   Z-Add     53            Conta_Colo
265100160201     C                   EXSR      RieImportoH
265200160201     C                   endif
265300160201     C*
265400160217R215 X**** 53) Redditi erogati da altri soggetti: Imponibile
265500160217  "  X***                Eval      WImporto      = WImp053
265600160217  "  X***
265700160217  "  X***                if        WImporto      <> 0
265800160217  "  X***                Z-Add     Prog_AU       Conta_Rigo
265900160217  "  X***                Z-Add     53            Conta_Colo
266000160217  "  X***                EXSR      RieImportoH
266100160217R215 X***                endif
266200160201     C*
266300160201     C* 54) Redditi erogati da altri soggetti: Ritenute a titolo di acconto
266400160201     C                   Eval      WImporto      = WImp054
266500160201     C*
266600160201     C                   if        WImporto      <> 0
266700160201     C                   Z-Add     Prog_AU       Conta_Rigo
266800160201     C                   Z-Add     54            Conta_Colo
266900160201     C                   EXSR      RieImportoH
267000160201     C                   endif
267100160201     C*
267200160201     C* 55) Redditi erogati da altri soggetti: Ritenute a titolo di imposta
267300160201     C                   Eval      WImporto      = WImp055
267400160201     C*
267500160201     C                   if        WImporto      <> 0
267600160201     C                   Z-Add     Prog_AU       Conta_Rigo
267700160201     C                   Z-Add     55            Conta_Colo
267800160201     C                   EXSR      RieImportoH
267900160201     C                   endif
268000160201     C*
268100160201     C* 56) Redditi erogati da altri soggetti: Ritenute sospese
268200160201     C                   Eval      WImporto      = WImp056
268300160201     C*
268400160201     C                   if        WImporto      <> 0
268500160201     C                   Z-Add     Prog_AU       Conta_Rigo
268600160201     C                   Z-Add     56            Conta_Colo
268700160201     C                   EXSR      RieImportoH
268800160201     C                   endif
268900160201     C*
269000160201     C* 57) Redditi erogati da altri soggetti: Add.Regionale a titolo di acconto
269100160201     C                   Eval      WImporto      = WImp057
269200160201     C*
269300160201     C                   if        WImporto      <> 0
269400160201     C                   Z-Add     Prog_AU       Conta_Rigo
269500160201     C                   Z-Add     57            Conta_Colo
269600160201     C                   EXSR      RieImportoH
269700160201     C                   endif
269800160201     C*
269900160201     C* 58) Redditi erogati da altri soggetti: Add.Regionale a titolo di imposta
270000160201     C                   Eval      WImporto      = WImp058
270100160201     C*
270200160201     C                   if        WImporto      <> 0
270300160201     C                   Z-Add     Prog_AU       Conta_Rigo
270400160201     C                   Z-Add     58            Conta_Colo
270500160201     C                   EXSR      RieImportoH
270600160201     C                   endif
270700160201     C*
270800160201     C* 59) Redditi erogati da altri soggetti: Add.Regionale sospesa
270900160201     C                   Eval      WImporto      = WImp059
271000160201     C*
271100160201     C                   if        WImporto      <> 0
271200160201     C                   Z-Add     Prog_AU       Conta_Rigo
271300160201     C                   Z-Add     59            Conta_Colo
271400160201     C                   EXSR      RieImportoH
271500160201     C                   endif
271600160201     C*
271700160201     C* 60) Redditi erogati da altri soggetti: Add.Comunale a titolo di acconto
271800160201     C                   Eval      WImporto      = WImp060
271900160201     C*
272000160201     C                   if        WImporto      <> 0
272100160201     C                   Z-Add     Prog_AU       Conta_Rigo
272200160201     C                   Z-Add     60            Conta_Colo
272300160201     C                   EXSR      RieImportoH
272400160201     C                   endif
272500160201     C*
272600160201     C* 61) Redditi erogati da altri soggetti: Add.Comunale a titolo di imposta
272700160201     C                   Eval      WImporto      = WImp061
272800160201     C*
272900160201     C                   if        WImporto      <> 0
273000160201     C                   Z-Add     Prog_AU       Conta_Rigo
273100160201     C                   Z-Add     61            Conta_Colo
273200160201     C                   EXSR      RieImportoH
273300160201     C                   endif
273400160201     C*
273500160201     C* 62) Redditi erogati da altri soggetti: Add.Comunale sospesa
273600160201     C                   Eval      WImporto      = WImp062
273700160201     C*
273800160201     C                   if        WImporto      <> 0
273900160201     C                   Z-Add     Prog_AU       Conta_Rigo
274000160201     C                   Z-Add     62            Conta_Colo
274100160201     C                   EXSR      RieImportoH
274200160201     C                   endif
274300160201     C*
274400160201     C* 71) Codice fiscale (sez. lavoro autonomo e redd. diversi)
274500160201     C                   If        WOsCdf1<> *blanks
274600160201     C                   Z-Add     Prog_AU       Conta_Rigo
274700160201     C                   Z-Add     71            Conta_Colo
274800160201     C                   MoveL     WOsCdf1       Campo_Valore
274900160201     C                   MoveL     WOsCdf1       C60
275000160201     C                   if        Campo_Valore <> *Blanks
275100160201     C                   EXSR      RieSkieraAlfH
275200160201     C                   endif
275300160201     C                   endif
275400160201     C*
275500160201     C* 72) Codice fiscale (sez. pignoramento verso terzi)
275600160201     C                   If        WOsCdf2<> *blanks
275700160201     C                   Z-Add     Prog_AU       Conta_Rigo
275800160201     C                   Z-Add     72            Conta_Colo
275900160201     C                   MoveL     WOsCdf2       Campo_Valore
276000160201     C                   MoveL     WOsCdf2       C60
276100160201     C                   if        Campo_Valore <> *Blanks
276200160201     C                   EXSR      RieSkieraAlfH
276300160201     C                   endif
276400160201     C                   endif
276500160201     C*
276600160201     C*  73) Codice fiscale (sez. indennità di esproprio)
276700160201     C                   If        WOsCdf3<> *blanks
276800160201     C                   Z-Add     Prog_AU       Conta_Rigo
276900160201     C                   Z-Add     73            Conta_Colo
277000160201     C                   MoveL     WOsCdf3       Campo_Valore
277100160201     C                   MoveL     WOsCdf3       C60
277200160201     C                   if        Campo_Valore <> *Blanks
277300160201     C                   EXSR      RieSkieraAlfH
277400160201     C                   endif
277500160201     C                   endif
277600160201     C*
277700160201     C* 101) Pignoramento verso terzi : Codice Fiscale debitore principale
277800160201     C                   If        WF2Cdf <> *blanks
277900160201     C                   Z-Add     Prog_AU       Conta_Rigo
278000160201     C                   Z-Add     101           Conta_Colo
278100160201     C                   MoveL     WF2Cdf        Campo_Valore
278200160201     C                   MoveL     WF2Cdf        C60
278300160201     C                   if        Campo_Valore <> *Blanks
278400160201     C                   EXSR      RieSkieraAlfH
278500160201     C                   endif
278600160201     C                   endif
278700160201     C*
278800160201     C* 102) Pignoramento verso terzi : Somme erogate
278900160201     C                   Eval      WImporto      = WImp102
279000160201     C*
279100160201     C                   if        WImporto      <> 0
279200160201     C                   Z-Add     Prog_AU       Conta_Rigo
279300160201     C                   Z-Add     102           Conta_Colo
279400160201     C                   EXSR      RieImportoH
279500160201     C                   endif
279600160201     C*
279700160201     C* 103) Pignoramento verso terzi : Ritenute operate
279800160201     C                   Eval      WImporto      = WImp103
279900160201     C*
280000160201     C                   if        WImporto      <> 0
280100160201     C                   Z-Add     Prog_AU       Conta_Rigo
280200160201     C                   Z-Add     103           Conta_Colo
280300160201     C                   EXSR      RieImportoH
280400160201     C                   endif
280500160201     C*
280600170125R250 C* 104) Pignoramento verso terzi : Somme erogate e non tassate
280700170125  "  C                   Eval      WImporto      = WImp104
280800170125  "  C*
280900170125  "  C                   if        WImporto      <> 0
281000170125  "  C                   Z-Add     Prog_AU       Conta_Rigo
281100170125  "  C                   Z-Add     104           Conta_Colo
281200170125  "  C                   EXSR      RieImportoH
281300170125  "  C                   endif
281400170125R250 C*
281500170125R250 X**** 104) Pignoramento verso terzi : Ritenute non operate
281600170125  "  X***                if        WF2Rino <> *blank
281700170125  "  X***                Z-Add     Prog_AU       Conta_Rigo
281800170125  "  X***                Z-Add     104           Conta_Colo
281900170125  "  X***                Clear                   Campo_Valore
282000170125  "  X***                Move      '1'           Campo_Valore
282100170125  "  X***                EXSR      Ablank
282200170125  "  X***                if        Campo_Valore <> *Blanks
282300170125  "  X***                EXSR      RieSkieraH
282400170125  "  X***                endif
282500170125R250 X***                endif
282600160201     C*
282700160201     C* 105) Pignoramento verso terzi : Codice Fiscale debitore principale
282800160202     C                   If        WF2Cdf2 <> *blanks
282900160201     C                   Z-Add     Prog_AU       Conta_Rigo
283000160201     C                   Z-Add     105           Conta_Colo
283100160201     C                   MoveL     WF2Cdf2       Campo_Valore
283200160201     C                   MoveL     WF2Cdf2       C60
283300160201     C                   if        Campo_Valore <> *Blanks
283400160201     C                   EXSR      RieSkieraAlfH
283500160201     C                   endif
283600160201     C                   endif
283700160201     C*
283800160201     C* 106) Pignoramento verso terzi : Somme erogate
283900160201     C                   Eval      WImporto      = WImp106
284000160201     C*
284100160201     C                   if        WImporto      <> 0
284200160201     C                   Z-Add     Prog_AU       Conta_Rigo
284300160201     C                   Z-Add     106           Conta_Colo
284400160201     C                   EXSR      RieImportoH
284500160201     C                   endif
284600160201     C*
284700160201     C* 107) Pignoramento verso terzi : Ritenute operate
284800160201     C                   Eval      WImporto      = WImp107
284900160201     C*
285000160201     C                   if        WImporto      <> 0
285100160201     C                   Z-Add     Prog_AU       Conta_Rigo
285200160201     C                   Z-Add     107           Conta_Colo
285300160201     C                   EXSR      RieImportoH
285400160201     C                   endif
285500160201     C*
285600170125R250 C* 108) Pignoramento verso terzi : Somme erogate e non tassate
285700170125  "  C                   Eval      WImporto      = WImp108
285800170125  "  C*
285900170125  "  C                   if        WImporto      <> 0
286000170125  "  C                   Z-Add     Prog_AU       Conta_Rigo
286100170125  "  C                   Z-Add     108           Conta_Colo
286200170125  "  C                   EXSR      RieImportoH
286300170125  "  C                   endif
286400170125R250 C*
286500170125R250 X***C* 108) Pignoramento verso terzi : Ritenute non operate
286600170125  "  X***                if        WF2Rino2 <> *blank
286700170125  "  X***                Z-Add     Prog_AU       Conta_Rigo
286800170125  "  X***                Z-Add     108           Conta_Colo
286900170125  "  X***                Clear                   Campo_Valore
287000170125  "  X***                Move      '1'           Campo_Valore
287100170125  "  X***                EXSR      Ablank
287200170125  "  X***                if        Campo_Valore <> *Blanks
287300170125  "  X***                EXSR      RieSkieraH
287400170125  "  X***                endif
287500170125R250 X***                endif
287600160201     C*
287700160201     C* 131) Indennità di esproprio   : Somme corrisposte
287800160201     C                   Eval      WImporto      = WImp131
287900160201     C*
288000160201     C                   if        WImporto      <> 0
288100160201     C                   Z-Add     Prog_AU       Conta_Rigo
288200160201     C                   Z-Add     131           Conta_Colo
288300160201     C                   EXSR      RieImportoH
288400160201     C                   endif
288500160201     C*
288600160201     C* 132) Indennità di esproprio   : Ritenute operate
288700160201     C                   Eval      WImporto      = WImp132
288800160201     C*
288900160201     C                   if        WImporto      <> 0
289000160201     C                   Z-Add     Prog_AU       Conta_Rigo
289100160201     C                   Z-Add     132           Conta_Colo
289200160201     C                   EXSR      RieImportoH
289300160201     C                   endif
289400160201     C*
289500160201     C* 133) Altre indennità e interessi: Somme corrisposte
289600160201     C                   Eval      WImporto      = WImp133
289700160201     C*
289800160201     C                   if        WImporto      <> 0
289900160201     C                   Z-Add     Prog_AU       Conta_Rigo
290000160201     C                   Z-Add     133           Conta_Colo
290100160201     C                   EXSR      RieImportoH
290200160201     C                   endif
290300160201     C*
290400160201     C* 134) Altre indennità e interessi: Somme corrisposte
290500160201     C                   Eval      WImporto      = WImp134
290600160201     C*
290700160201     C                   if        WImporto      <> 0
290800160201     C                   Z-Add     Prog_AU       Conta_Rigo
290900160201     C                   Z-Add     134           Conta_Colo
291000160201     C                   EXSR      RieImportoH
291100160201     C                   endif
291200160201     C*
291300160201     C* 135) Indennità di esproprio   : Ritenute operate
291400160201     C                   Eval      WImporto      = WImp135
291500160201     C*
291600160201     C                   if        WImporto      <> 0
291700160201     C                   Z-Add     Prog_AU       Conta_Rigo
291800160201     C                   Z-Add     135           Conta_Colo
291900160201     C                   EXSR      RieImportoH
292000160201     C                   endif
292100160201     C*
292200160201     C* 136) Indennità di esproprio   : Ritenute operate
292300160201     C                   Eval      WImporto      = WImp136
292400160201     C*
292500160201     C                   if        WImporto      <> 0
292600160201     C                   Z-Add     Prog_AU       Conta_Rigo
292700160201     C                   Z-Add     136           Conta_Colo
292800160201     C                   EXSR      RieImportoH
292900160201     C                   endif
293000160201     C*
293100160201     C* 137) Altre indennità e interessi: Somme corrisposte
293200160201     C                   Eval      WImporto      = WImp137
293300160201     C*
293400160201     C                   if        WImporto      <> 0
293500160201     C                   Z-Add     Prog_AU       Conta_Rigo
293600160201     C                   Z-Add     137           Conta_Colo
293700160201     C                   EXSR      RieImportoH
293800160201     C                   endif
293900160201     C*
294000160201     C* 138) Indennità di esproprio   : Ritenute operate
294100160201     C                   Eval      WImporto      = WImp138
294200160201     C*
294300160201     C                   if        WImporto      <> 0
294400160201     C                   Z-Add     Prog_AU       Conta_Rigo
294500160201     C                   Z-Add     138           Conta_Colo
294600160201     C                   EXSR      RieImportoH
294700160201     C                   endif
294800160201     C*
294900160201R211 C                   Endsr
295000160201R211 C/EJECT
295100000000     C******************************************************
295200000000     C* Toglie gli zeri non significativi nelle caselle con
295300000000     C* importo dei campi posizionali
295400000000     C******************************************************
295500000000     C     RieImportoH   BEGSR
295600000000     C*
295700160202R211 C                   Eval      $FlgNeg       = '0'
295800160202  "  C*
295900160202  "  C* Se l'importo è negativo
296000160201  "  C                   If        Wimporto < 0
296100160202  "  C                   Eval      $FlgNeg       = '1'
296200160202  "  C                   Eval      Wimporto     = wimporto * -1
296300160202  "  C                   Endif
296400160201R211 C*
296500000000     C                   Clear                   Campo_Valore
296600000000     C                   Z-Add     WImporto      Importo16_2
296700000000     C*
296800000000     C                   Move      Importo16_2   CampoWrk
296900000000     C                   Move      CampoInt      Int
297000000000     C                   Move      ','           Virgola
297100000000     C                   Move      CampoDec      Dec
297200000000     C                   Move      CampoIntero   Importo16A
297300000000     C*
297400000000     C                   EXSR      Ablank2
297500000000     C*
297600000000     C                   if        Campo_Valore <> *Blanks
297700000000     C                   EXSR      RieSkieraH
297800000000     C                   endif
297900000000     C*
298000000000     C                   ENDSR
298100000000     C/EJECT
298200000000     C******************************************************
298300000000     C* Toglie gli zeri non significativi nelle caselle con
298400000000     C* importo dei campi posizionali
298500000000     C******************************************************
298600000000     C     RieImportoH2  BEGSR
298700000000     C*
298800000000     C                   Clear                   Campo_Valore
298900000000     C                   Z-Add     WImporto      Importo16_0
299000000000     C*
299100000000     C                   Eval      $FlgNeg       = '0'
299200000000     C                   if        WImporto     <   0
299300160201     C                   Eval      $FlgNeg       = '1'
299400000000     C                   Eval      Importo16_0  = Importo16_0 * -1
299500000000     C                   endif
299600000000     C                   EXSR      Ablank2
299700000000     C*
299800000000     C                   if        Campo_Valore <> *Blanks
299900000000     C                   EXSR      RieSkieraH
300000000000     C                   endif
300100000000     C*
300200000000     C                   ENDSR
300300000000     C/EJECT
300400000000     C******************************************************
300500000000     C* Toglie gli zeri non significativi al Campo_Valore
300600000000     C******************************************************
300700000000     C     ABlank        BEGSR
300800000000     C*
300900000000     C                   MoveA     Campo_Valore  Sk_Campo
301000000000     C*
301100000000     C                   DO        Max$X         $X
301200000000     C*
301300000000     C                   if        Sk_Campo($X) = ' '   or
301400000000     C                             Sk_Campo($X) = '0'
301500000000     C                   Move      ' '           Sk_Campo($X)
301600000000     C                   else
301700000000     C                   LEAVE
301800000000     C                   endif
301900000000     C*
302000000000     C                   endDo
302100000000     C*
302200000000     C                   MoveA     Sk_Campo      Campo_Valore
302300000000     C*
302400000000     C                   ENDSR
302500000000      /EJECT
302600000000     C******************************************************
302700000000     C* Toglie gli zeri non significativi nelle caselle con
302800000000     C* importo dei campi posizionali
302900000000     C******************************************************
303000000000     C     ABlank2       BEGSR
303100000000     C*
303200000000     C                   MoveA     Importo16A    SK_Importo
303300000000     C*
303400000000     C                   DO        Max$Z         $Z
303500000000     C*
303600000000     C                   if        Sk_Importo($Z) = ' '   or
303700000000     C                             Sk_Importo($Z) = '0'
303800000000     C                   Move      ' '           Sk_Importo($Z)
303900000000     C                   else
304000000000     C                   if        $FlgNeg       = '1'
304100000000     C                   Eval      Sk_Importo($Z-1) = '-'
304200000000     C                   Eval      $FlgNeg       = '0'
304300000000     C                   endif
304400000000     C                   LEAVE
304500000000     C                   endif
304600000000     C*
304700000000     C                   endDo
304800150219R169 C*
304900000000  "  C                   If        Sk_Importo(13) =  ' '  and
305000000000  "  C                             Sk_Importo(14) =  ','  and
305100000000  "  C                             Sk_Importo(15) <> ' '
305200000000  "  C                   Eval      Sk_Importo(13) = '0'
305300150219R169 C                   EndIf
305400000000     C*
305500000000     C                   MoveA     Sk_Importo    Campo_Valore
305600000000     C*
305700000000     C                   ENDSR
305800000000      /EJECT
305900000000     C******************************************************
306000000000     C* Scrittura t.r. Z
306100000000     C******************************************************
306200000000     C     WrtTpRecZ     BEGSR
306300000000     C*
306400000000     C                   Clear                   SDGSK61DSZ
306500000000     C* Tipo Record
306600000000     C                   Eval      K61ZTR       = 'Z'
306700000000     C*-
306800000000     C                   Z-ADD     1             K61ZNB
306900000000     C*-
307000000000     C                   Z-ADD     0             K61ZNC
307100000000     C*-
307200000000     C                   Z-ADD     Cnt_D         K61ZND
307300000000     C*-
307400000000     C                   Z-ADD     0             K61ZNG
307500000000     C*-
307600000000     C                   Z-ADD     Cnt_H         K61ZNH
307700180123R279 C*-
307800180123R279 C                   Z-ADD     0             K61ZNL
307900000000     C*-
308000000000     C* Carattere di controllo
308100000000     C                   MoveL     'A'           K61ZK1
308200000000     C*-
308300000000     C* Elementi di fine riga
308400000000     C* Impostare i valori esadecimali '0D' e '0A'
308500000000     C* (caratteri ASCI 'CR' ed 'LF')
308600000000     C*
308700000000     C                   Eval      K61ZK2  = *Blanks
308800000000     C                   Eval      K61ZK2  = X'0D25'
308900000000     C*
309000000000     C                   Clear                   SDGCUW000
309100000000     C                   MoveL     SDGSK61DSZ    CUWREC
309200000000?    C                   WRITE     SDGCUW000
309300000000     C                   FEOD      SDGCUW00F
309400000000     C*
309500000000     C                   ENDSR
309600000000     C/EJECT
309700000000     C************************************************************
309800000000     C* Iscrizione a catalogo del file scarico
309900000000     C************************************************************
310000000000     C     IscrCatF      BEGSR
310100000000     C*
310200000000     C
310300000000     C                   Eval      WNumero     = *All'0'
310400000000     C                   Z-Add     AnnoK61       WAnno
310500000000     C                   Z-Add     $ProgrInvio   WPrgInvio
310600000000     C                   Move      WNumero       $Numero
310700000000     C*
310800000000     C                   Move      UDateIso      $DtAlfa
310900000000     C*
311000000000     C                   Call      'X36CATFR'
311100000000     C                   PARM                    SocietaK61
311200000000     C                   PARM      *Blank        $Unita
311300000000     C                   PARM                    $File
311400000000     C                   PARM                    $Libreria
311500000000     C                   PARM                    $DesFileO
311600000000     C                   PARM      'CU'          $CdScarico        2
311700000000     C                   PARM      'P'           $TpSupp           1
311800000000     C                   PARM      *Blank        $NmSupp          10
311900000000     C                   PARM      '4'           $TpDist           1
312000000000     C                   PARM                    $DtAlfa          10
312100000000     C                   PARM                    $Numero           9
312200000000     C                   PARM      *Blank        $DesBan          44
312300000000     C                   PARM                    KNmus
312400000000     C                   PARM      '0'           Sospeso           1
312500000000     C                   PARM      '1'           Video             1
312600000000     C*
312700000000     C                   ENDSR
312800000000     C/EJECT
312900000000     C******************************************************
313000000000     C* Gestione invio multiplo
313100000000     C******************************************************
313200000000     C     GesMultInv    BEGSR
313300000000     C*
313400000000     C* Inizializzazione schiera per record D
313500000000     C                   EXSR      InzSkieraD
313600000000     C*
313700000000     C* Inizializzazione schiera per record H
313800000000     C                   EXSR      InzSkieraH
313900000000     C*
314000000000     C                   EXSR      Inz770
314100000000     C                   EXSR      Set770
314200000000     C*
314300000000     C                   DOW       $Fine770    = *Off     and
314400000000     C                             $ProgrInvio < $NrInvii
314500000000     C*
314600000000     C                   Add       1             $ProgrInvio
314700150305R172 X****               Z-Add     0             Cnt_Righe
314800150305R172 C                   Z-Add     1             Cnt_Righe
314900000000     C                   Eval      SoloPrimaVolta = *On
315000000000     C* Azzeramento
315100000000     C                   EXSR      AzzeraProg
315200000000     C                   EXSR      InzSkieraD
315300000000     C                   EXSR      InzSkieraH
315400000000     C*
315500000000     C                   EXSR      RepNomeF
315600000000     C                   EXSR      GesFilTel
315700000000     C*
315800000000     C                   OPEN      SDGCUW00F
315900000000     C*
316000000000     C* Record A  testata della fornitura
316100000000     C                   EXSR      WrtTpRecA
316200000000     C*
316300000000     C* Record B  testata della fornitura
316400000000     C                   EXSR      WrtTpRecB
316500000000     C*
316600000000     C                   if        $ProgrInvio > 1
316700000000     C                   EXSR      ElaboraKey
316800000000     C*
316900000000     C                   if        SavKey   <> NewKey  or
317000000000     C                             $Fine770  = *On
317100000000     C*
317200000000     C                   EXSR      WrtTpRecD
317300000000     C                   EXSR      InzSkieraD
317400000000     C*
317500150305R172 C                   if        TipInvK61 <> 2
317600000000     C                   EXSR      WrtTpRecH
317700000000     C                   EXSR      InzSkieraH
317800150305R172 C                   endif
317900000000     C*
318000000000     C                   Add       1             $NrRecTot
318100000000     C                   Add       1             CntDA
318200000000     C                   Add       1             CntAU
318300000000     C                   EXSR      ElaboraKey
318400000000     C                   endif
318500000000     C*
318600000000     C                   if        N77Escludi <> '1'
318700150306R172 C                             and
318800150306 "   C                             ProtocK61  =  0 and
318900150306 "   C                             ProtoDK61  =  0
319000150306 "   C                             or
319100150306 "   C                             ((ProtocK61  <> 0 and
319200150306 "   C                              ProtoDK61  =  0 and
319300150306 "   C                              N77Protoc  =  ProtocK61)
319400150306 "   C                             or
319500150306 "   C                             (ProtocK61  <> 0         and
319600150306 "   C                              ProtoDK61  <> 0         and
319700150306 "   C                              N77Protoc  =  ProtocK61 and
319800150310 "   C                              N77ProtCea =  ProtoDK61))
319900150310R172 C                             or $Fine770  = *On
320000000000     C****                         and N77Cde  =  *blank
320100000000     C****                         and N77Num  =  *blank
320200000000     C                   EXSR      ElaboraRecD
320300000000     C*
320400150305R172 C                   if        TipInvK61 <> 2
320500000000     C                   EXSR      ElaboraRecH
320600150305R172 C                   endif
320700150305R172 C*
320800150305R172 C                   EXSR      Aggiorna770
320900000000     C                   endif
321000000000     C*
321100000000     C                   endif
321200000000     C*
321300000000     C* seconda lettura per scrivere il multifile
321400000000     C                   DOW       $Fine770    = *Off  and
321500000000     C                             (Cnt_Righe   <  MaxRecK61 and
321600000000     C                              MaxRecK61   > 0            )
321700000000     C*
321800000000     C                   if        TipInvK61   <> 2
321900000000     C*
322000000000     C                   EXSR      Lettura770
322100000000     C*
322200000000     C                   if        N77Escludi <> '1'
322300150306R172 C                             and
322400150309 "   C                             (ProtocK61  =  0 and
322500150309 "   C                              ProtoDK61  =  0 or
322600150310 "   C                              $RcdOk = *on)
322700150310R172 C                             or $Fine770  = *On
322800000000     C****                         and N77Cde  =  *blank
322900000000     C****                         and N77Num  =  *blank
323000000000     C                   if        SoloPrimaVolta  = *On
323100000000     C                   EXSR      ElaboraKey
323200000000     C                   endif
323300000000     C*
323400000000     C                   if        SavKey   <> NewKey  or
323500000000     C                             $Fine770  = *On
323600000000     C*
323700000000     C                   EXSR      WrtTpRecD
323800000000     C                   EXSR      InzSkieraD
323900000000     C*
324000150305R172 C                   if        TipInvK61 <> 2
324100000000     C                   EXSR      WrtTpRecH
324200000000     C                   EXSR      InzSkieraH
324300150305R172 C                   endif
324400000000     C*
324500000000     C                   Add       1             $NrRecTot
324600000000     C                   Add       1             CntDA
324700000000     C                   Add       1             CntAU
324800000000     C                   EXSR      ElaboraKey
324900000000     C                   endif
325000000000     C*
325100000000     C                   EXSR      ElaboraRecD
325200000000     C*
325300150305R172 C                   if        TipInvK61 <> 2
325400000000     C                   EXSR      ElaboraRecH
325500150305R172 C                   endif
325600150305R172 C*
325700150305R172 C                   EXSR      Aggiorna770
325800000000     C                   endif
325900000000     C*
326000000000     C                   endif
326100000000     C*
326200000000     C                   endDo
326300000000     C*
326400000000     C* Record Z di coda della fornitura
326500000000     C                   EXSR      WrtTpRecZ
326600000000     C*
326700000000     C                   EXSR      IscrCatF
326800000000     C*
326900000000     C                   CLOSE     SDGCUW00F
327000000000     C*
327100000000     C                   endDo
327200000000     C*
327300000000     C                   ENDSR
327400000000     C/EJECT
327500000000     C************************************************************
327600000000     C* Stampa errore
327700000000     C************************************************************
327800000000     C     StpErrore     BEGSR
327900000000     C*
328000000000     C*
328100000000     C                   ENDSR
328200000000     C/EJECT
328300000000     C************************************************************
328400000000     C* Inizializzazioni variabili
328500000000     C************************************************************
328600000000     C     INZVAR        BEGSR
328700000000     C*
328800000000     C                   MoveL     KPJBU         SDGSK61DS
328900000000     C*
329000000000     C                   Eval      ERRK61      = *Off
329100000000     C                   Eval      MaxRecK61   = MaxRecK61 -4
329200000000     C*
329300000000     C                   MoveL     'SOC001'      TIPXSC
329400000000     C                   MoveL     SOCIETAK61    SOCXSC
329500000000     C*
329600000000     C                   EXSR      RepSoc
329700000000     C*
329800000000     C                   if        RTNXSC     <> '1'
329900000000     C                   MoveL     XSOCDS        SOC001
330000000000     C                   else
330100000000     C                   Eval      $Errori     = *On
330200000000     C                   EXSR      ENDPGM
330300000000     C                   endif
330400000000     C*
330500000000     C                   Move      XScSoc        SIFSocieta
330600000000     C     K01SIF01      Chain     ANSIF01L                           21
330700000000     C  N21              Move      SIFFlgOpe     WrkFlgOpe
330800000000     C   21              Move      *Blanks       SocSogg
330900000000     C*
331000000000     C                   Z-Add     0             $I
331100160530R224 C*
331200160607R224 C                   EXSR      RepDatiSos
331300000000     C*
331400000000     C                   ENDSR
331500000000     C/EJECT
331600000000     C************************************************************
331700000000     C* Reperimento dati società
331800000000     C************************************************************
331900000000     C     RepSoc        BEGSR
332000000000     C*
332100000000     C                   CALL      'SDGSOCDR'
332200000000     C                   PARM                    TIPXSC            6
332300000000     C                   PARM                    SOCXSC            3
332400000000     C                   PARM                    CDSXSC            9 0
332500000000     C                   PARM                    MODXSC            3
332600000000     C                   PARM      *Blanks       RTNXSC            1
332700000000     C                   PARM                    XSOCDS
332800000000     C                   PARM                    KPJBA
332900000000     C*
333000000000     C                   ENDSR
333100000000     C/EJECT
333200160530R224 C************************************************************
333300160607 "   C* REPERIMENTO dati anagafici aggiuntivi sostituto d'imposta
333400160530 "   C************************************************************
333500160607R224 C     RepDatiSos    BegSr
333600160530     C*
333700160530     C* preparo i parametri di chiamata al driver
333800160530     C                   Clear                   XMPEDS
333900160530     C                   EVAL      TpChiamata = '01'
334000160530     C                   EVAL      NChiavi = 5
334100160530     C                   EVAL      VLogica = 2
334200160530     C*
334300160530     C* preparo le chiavi per la chiamata al driver
334400160530     C                   Clear                   XMPEDS1
334500160530     C                   MoveL     XSCSOC        DSMPESOC
334600160607     C                   MoveL     'SDGSK60R C'  DSMPEPGM
334700160607     C                   MoveL     *Blanks       DSMPEPRF
334800160530     C* Si reperiscono i dati di default a seconda delle modalita con
334900160530     C* cui il pgm e usato
335000160530     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
335100160530     C* MPETIP = 'PV'  pgm richiamato da menu
335200160530     C* MPETIP = 'PB'  x lancio Batch
335300160530     C                   MoveL     'PB'          DSMPETIP
335400160530     C*
335500160530     C                   CALLB     'XMPE'
335600160530     C                   PARM                    XMPEDS
335700160530     C                   PARM                    XMPEDS1
335800160530     C*
335900160530     C     Risultato     IfEq      '0'
336000160607     C                   MoveL     DSMPEPAR      DSSostit
336100160530     C                   EndIf
336200160530     C*
336300160530R224 C                   EndSr
336400000000     C************************************************************
336500000000     C* OPERAZIONI INIZIALI
336600000000     C************************************************************
336700000000     C     *INZSR        BEGSR
336800000000     C*
336900000000     C* Reperimento parametri
337000000000     C*
337100000000     C     *ENTRY        PLIST
337200000000     C                   PARM                    KPJBA
337300000000     C*
337400000000     C     *DMY          Move      Udate         UDateISO
337500000000     C*
337600000000     C                   ENDSR
337700000000     C/EJECT
337800000000     C************************************************************
337900000000     C* DEFINIZIONE KLIST
338000000000     C************************************************************
338100000000     C     DefKlist      BEGSR
338200000000     C*
338300000000     C* KLIST
338400000000     C*
338500000000     C* Nd77007L
338600000000     C     K0277007      KLIST
338700000000     C                   KFLD                    N77Societa
338800000000     C                   KFLD                    N77Acf
338900000000     C*
339000000000     C* SDGSo301L
339100160530R224 X**** K04SO301      KLIST
339200160530R224 C     K05SO301      KLIST
339300000000     C                   KFLD                    SO3Sogg
339400000000     C                   KFLD                    SO3DtFiVL
339500000000     C                   KFLD                    SO3Sys
339600000000     C                   KFLD                    SO3NrAsint
339700160530R224 C                   KFLD                    SO3Dest
339800000000     C*
339900000000     C* ANSIF01L
340000000000     C     K01SIF01      KLIST
340100000000     C                   KFLD                    SIFSocieta
340200000000     C*
340300000000     C                   ENDSR
340400000000     C/EJECT
