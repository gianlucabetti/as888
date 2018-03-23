000100000000     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(*CALLER)
000200000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*DATETIME)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      ********************************************************************
000500000000      *
000600000000      *         ------------------------------------------------
000700150128      *                            CU 2015
000800000000      *                   Creazione file telematico
000900000000      *         ------------------------------------------------
001000000000      *
001100000000     F*----------------------------------------------------------------
001200150130     FND77007L  IF   E           K DISK
001300131115     F*----------------
001400000000     F* File contenente i dati da trasmettere all'agenzia
001500150130     FSDGCUW00F UF A E             DISK    UsrOpn
001600150210     F                                     InfDs(INFO00)
001700000000     F*-------------
001800000000     F* Dati anagrafici aggiuntivi
001900000000     F* Key: Sogg/DtFiVl/Sys/NrAsInt      UNIQUE
002000000000     FSDGSO301L IF   E           K DISK
002100000000     F*-------------
002200000000     FANSIF01L  IF   E           K DISK
002300000000     D*-------------
002400000000     D* Prototipo per la procedura Euro
002500000000     D/COPY *LIBL/SRCCPY,PJX002PR
002600000000     D*----------------
002700000000     D* Parametro di base
002800000000     D XPAOUT          DS          2000
002900000000     D*----------------
003000000000     D* PARAMETRI ESTERNI
003100131118     D KPJBA         E DS
003200000000     D*----------------
003300000000     D* Valori assunti
003400000000     D XMPEDS        E DS                  INZ
003500000000     D XMPEDS1       E DS                  INZ
003600131114     D DSInterm        DS                  INZ
003700150205     D $IdProd                       16A
003800150205     D $FisInt                       16A
003900150205     D $ImpTrs                        1A
004000150205     D $FirInt                       30A
004100150205     D $ImpDta                       10A
004200150205     D $FisSog                       16A
004300150205     D $CarSog                        2A
004400150205     D $CogSog                       24A
004500150205     D $NomSog                       20A
004600150205     D $FisDic                       11A
004700150205     D $DtFirDic                     10A
004800150205     D $FirDic                       30A
004900150205     D*-------------
005000150205     D DSInterm2       DS                  INZ
005100150205     D $Telefon                      12A
005200150205     D $Email                        69A
005300000000     D*----------------
005400000000     D* Parametri ricevuti
005500150130     D SDGSK61DS     E DS
005600131112     D*-------------
005700131112     D* Dati di ambiente ottenuti da SDGSOCDR
005800131112     D SOC001        E DS                  EXTNAME(SDGSOCDS )
005900131112     D*-------------
006000000000     D* DS Interna per dati di output di SDGSOCDR
006100000000     D XSOCDS          S           1000
006200000000     D*----------------
006300000000     D* Reperimento nome PGM
006400000000     D STATUS         SDS           333
006500000000     D  DSPGM            *PROC
006600000000     D*----------------
006700000000     D* Richiamo a SDGTABDR
006800000000     D SDGTABDS      E DS                  inz
006900000000     D*----------------
007000000000     D* tabelle delle nature giuridiche
007100000000     D ANGG16DS      E DS                  INZ
007200000000     D*------------
007300150130     D* Immagine SDGCUW00F
007400150130     D SDGCUWDS      E DS                  ExtName(SDGCUW00F)
007500131112     D*------------
007600131112     D* Ds tracciati appoggio
007700150128     D SDGSK61DSA    E DS
007800150128     D SDGSK61DSB    E DS
007900150128     D SDGSK61DSD    E DS
008000150128     D SDGSK61DSH    E DS
008100150128     D SDGSK61DSZ    E DS
008200150210     D*-------------
008300150210     D Info00          DS           512
008400150210     D  NrrCuw               397    400B 0
008500131112     D*----------------
008600150202     D Cnt_Righe       S              7S 0 Inz(*Zero)
008700131112     D*
008800150130     D Cnt_D           S              7S 0 Inz(*Zero)
008900150202     D CntDA           S              7S 0 Inz(*Zero)
009000150202     D Prog_DA         S              3S 0 Inz(*Zero)
009100150202     D*
009200150202     D Cnt_H           S              7S 0 Inz(*Zero)
009300150202     D CntAU           S              7S 0 Inz(*Zero)
009400150202     D Prog_AU         S              3S 0 Inz(*Zero)
009500131112     D*----------------
009600000000     D* Salvataggio KEY
009700000000     D SavKey          DS
009800150202     D SavCdf                              Like(N77Cdf    )
009900150202     D SavCde                              Like(N77Cde    )
010000150206     D SavC77                              Like(N77C77    )
010100000000     D
010200131112     D* Nuova       KEY
010300131112     D NewKey          DS
010400150202     D N77Cdf
010500150202     D N77Cde
010600150206     D N77C77
010700131129     D*----------------
010800000000     D* campi di lavoro
010900131114     D $NrRecTot       S              9S 0 Inz(*Zero)
011000150210     D SavNrrCuw       S              9S 0 Inz(*Zero)
011100150205     D*----------------
011200150206     D WImp004         S             19S 3
011300150206     D WImp005         S             19S 3
011400150206     D WImp006         S             19S 3
011500150206     D WImp007         S             19S 3
011600150206     D WImp008         S             19S 3
011700150206     D WImp009         S             19S 3
011800150206     D WImp010         S             19S 3
011900150206     D WImp011         S             19S 3
012000150206     D WImp012         S             19S 3
012100150206     D WImp013         S             19S 3
012200150206     D WImp014         S             19S 3
012300150206     D WImp015         S             19S 3
012400150206     D WImp016         S             19S 3
012500150206     D WImp017         S             19S 3
012600150206     D WImp018         S             19S 3
012700150206     D WImp019         S             19S 3
012800150206     D WImp020         S             19S 3
012900150206     D WImp021         S             19S 3
013000150206     D WImp022         S             19S 3
013100150206     D WImp023         S             19S 3
013200000000     D*----------------
013300131115     D WImporto        S             19S 3
013400000000     D Importo16_0     S             16S 0
013500150206     D Importo16_2     S             16S 2
013600131115     D Importo16A      S             16A
013700131115     D $FlgNeg         S              1A   Inz('0')
013800150206     D*-------------
013900150206     D CampoWrk        DS
014000150206     D CampoInt                1     14
014100150206     D CampoDec               15     16
014200150206     D*
014300150206     D CampoIntero     DS
014400150206     D  Int                    1     13
014500150206     D  virgola               14     14
014600150206     D  Dec                   15     16
014700000000     D*-------------
014800150206     D SavCodice       S                   Like(N77Codice )
014900000000     D UDateISO        S               D
015000000000     D $TpErrore       S              2A
015100131118     D SoloPrimaVolta  S              1A   Inz('1')
015200000000     D $Errori         S              1A   Inz('0')
015300150130     D $Fine770        S              1A   Inz('0')
015400131118     D $FlDatiAna      S              1A   Inz('0')
015500150206     D $FlDatiImp      S              1A   Inz('0')
015600000000     D*-------------
015700150130     D $CdFiscSog      S                   Like(SO3CdFisc )
015800150205     D $CdIVASog       S                   Like(SO3PartIva )
015900150130     D $CogRagSog      S                   Like(SO3CogRag)
016000150130     D $NomeSog        S                   Like(SO3Nome )
016100150130     D $DtNascSog      S                   Like(SO3DtNasc)
016200150130     D $LocNascSog     S                   Like(SO3LocNasc)
016300150130     D $PrvNascSog     S                   Like(SO3PrvNasc)
016400150205     D $StatoSog       S                   Like(SO3Stato   )
016500150205     D $StaFedSog      S                   Like(SO3StaFed  )
016600150130     D $LocalitSog     S                   Like(SO3Localit)
016700150205     D $IndirESog      S                   Like(SO3IndirE  )
016800000000     D $SexSog         S              1A
016900000000     D*------
017000150130     D $CdFisc         S                   Like(SO3CdFisc)
017100150130     D $PartIVA        S                   Like(SO3PartIva)
017200131115     D WrKNatGiu       S              2A
017300150205     D WSavQuaD        S              2A
017400150205     D WSavQuaH        S              2A
017500000000     D*----------------
017600000000     D* Ds flag ansif
017700000000     D                 DS
017800000000     D  SocSogg               31     38
017900000000     D  WrkFlgOpe              1     50
018000000000     D*----------------
018100000000     D* Ds per nome file
018200000000     D                 DS
018300000000     D  WAnno                  3      6  0
018400000000     D  WPer                   7      8
018500000000     D  WPrgInvio              9     10  0
018600000000     D  WNumero                1     10
018700131114     D*--
018800131114     D* ogni elemento della tabella Skiera è costituito da:
018900131114     D* - 1 campo-codice di 8 caratteri;
019000131114     D* - 1 campo-valore di 16 caratteri.
019100131114     D*
019200131114     D                 DS
019300131114     D  Cod_Elenco             1      2
019400131114     D  Cod_Rigo               3      5  0
019500131114     D  Cod_Colo               6      8  0
019600131114     D  Campo_Codice           1      8
019700131114     D*
019800000000     D                 DS
019900000000     D  Campo                  1     16
020000000000     D  C15I                  17     61
020100000000     D    C15                        15    Dim(03) Overlay(C15I)
020200000000     D  C60                    1     61
020300000000     D*-------------
020400000000     D* Elementi della variabile CAMPO
020500000000     D Conta_Rigo      S              3S 0 Inz(*Zero)
020600000000     D Conta_Colo      S              3S 0 Inz(*Zero)
020700000000     D*-------------
020800150203     D* tabella per i dati NON posizionali del record D
020900131114     D Max$I           C                   CONST(100)
021000150205     D Skiera          S             24    DIM(Max$I)
021100000000     D $I              S              3S 0
021200150203     D*-------------
021300150203     D* tabella per i dati NON posizionali del record H
021400150203     D Max$H           C                   CONST(100)
021500150203     D SkieraH         S             24    DIM(Max$H)
021600150203     D $H              S              3S 0
021700131115     D*--
021800131115     D Sk_Campo        S              1    DIM(Max$X)
021900131115     D Max$X           C                   CONST(61)
022000131115     D $X              S              3S 0
022100000000     D*----------------
022200131114     D* questa schiera serve per togliere gli zeri non significativi
022300131114     D* alle caselle che contengono degli importi dei campi posizionali
022400131114     D Max$Z           C                   CONST(16)
022500131114     D $Z              S              3S 0
022600131114     D*
022700131114     D Sk_Importo      S              1    DIM(Max$Z)
022800131115     D*---------------------------------------------------
022900131115     D* Elemento tabella Skiera :
023000131115     D* campo 16 caratteri (valore)
023100131115     D                 DS
023200131115     D  Campo_Valore           1     16
023300131115     D  Caratt_45             17     61
023400000000     D*-------------
023500000000     D* variabili per invio multiplo
023600131115     D $MultiInvio     S              1A   Inz(*Off)
023700000000     D $NrInvii        S              5S 0 Inz(0)
023800000000     D $ProgrInvio     S              5S 0 Inz(0)
023900000000     D*-------------
024000000000     D WImpInp         S                   Like(ImporD0202)
024100000000     D WImpOut         S                   Like(ImporD0202)
024200000000     D*-------------
024300000000     D DtAMG           DS
024400000000     D  AMGAnno                1      4
024500000000     D  T1                     5      5
024600000000     D  AMGMese                6      7
024700000000     D  T2                     8      8
024800000000     D  AMGGiorno              9     10
024900000000     D*-------------
025000000000     D DtGMA           DS
025100000000     D  GMAGiorno              1      2  0
025200000000     D  GMAMese                3      4  0
025300000000     D  GMAAnno                5      8  0
025400131115     D*-------------
025500131115     D WrkDt           S              8A
025600000000     D*---------------------------------------------------------------*
025700000000     D DatMin          C                   CONST('0001-01-01')
025800150130     D CostFile        C                   CONST('Certificaz. Unica')
025900131118     C******************************************************
026000131118     C*                MAIN LINE PROGRAM
026100131118     C******************************************************
026200000000     C*
026300131115     C                   EXSR      INZVAR
026400000000     C*
026500000000     C* Dati Rappresentante
026600131115     C                   EXSR      RepDatInter
026700150205     C                   Exsr      RepDatInter2
026800131114     C*
026900131114     C* Azzeramento
027000131115     C                   EXSR      AzzeraProg
027100131114     C*
027200131114     C*-----------------------------------------------
027300131114     C* LETTURA DATI
027400131114     C*
027500131114     C* Elaborazione dati per preparazione invio
027600131115     C                   EXSR      ElaboraDati
027700131114     C*
027800131114     C* Determino nr invii
027900131115     C                   EXSR      RepNrInvii
028000131114     C*
028100131115     C                   if        $NrInvii      > 1
028200131114     C                   Eval      $MultiInvio    = *On
028300131115     C                   else
028400131115     C                   Eval      $MultiInvio    = *Off
028500131115     C                   endif
028600131114     C*
028700131114     C*-----------------------------------------------
028800131114     C*
028900131114     C* ELABORAZIONE DATI per SCRITTURA TELEMATICO ---
029000131114     C*
029100131114     C*
029200131114     C* Se ho record allora continuo
029300131115     C                   if        $NrRecTot     > 0
029400131118     C*
029500131118     C* Azzeramento
029600131118     C                   EXSR      AzzeraProg
029700131114     C*
029800131114     C* Unico invio  o multi invio
029900131115     C                   if        $MultiInvio    = *Off
030000131114     C                   Z-Add     1             $ProgrInvio
030100131115     C                   EXSR      GesSingInv
030200131115     C                   else
030300131118     C                   EXSR      GesMultInv
030400131115     C                   endif
030500131114     C*
030600131115     C                   endif
030700131112     C*
030800131118     C                   EXSR      ENDPGM
030900131115     C*
031000131115     C/EJECT
031100150130     C************************************************************
031200150130     C* FINE PROGRAMMA
031300150130     C************************************************************
031400150130     C     ENDPGM        BEGSR
031500150130     C*
031600150130     C                   if        $Errori     = *On
031700150130     C                   Eval      ERRK61      = '1'
031800150130     C                   endif
031900150130     C*
032000150130     C                   MoveL     SDGSK61DS     KPJBU
032100150130     C                   SETON                                        LR
032200150130     C*
032300150130     C                   RETURN
032400150130     C*
032500150130     C                   ENDSR
032600150130     C/EJECT
032700131118     C************************************************************
032800131118     C* Reperimento dati Intermediario
032900131118     C************************************************************
033000131118     C     RepDatInter   BEGSR
033100131118     C*
033200131118     C* preparo i parametri di chiamata al driver
033300131118     C                   Clear                   XMPEDS
033400131118     C                   EVAL      TpChiamata = '01'
033500131118     C                   EVAL      NChiavi = 5
033600131118     C                   EVAL      VLogica = 2
033700131118     C*
033800131118     C* preparo le chiavi per la chiamata al driver
033900131118     C                   Clear                   XMPEDS1
034000131118     C                   MoveL     XSCSOC        DSMPESOC
034100150130     C                   MoveL     'SDGSK60R R'  DSMPEPGM
034200131118     C                   MoveL     *Blanks       DSMPEPRF
034300131118     C* Si reperiscono i dati di default a seconda delle modalità con
034400131118     C* cui il pgm è usato
034500131118     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
034600131118     C* MPETIP = 'PV'  pgm richiamato da menù
034700131118     C* MPETIP = 'PB'  x lancio Batch
034800131118     C                   MoveL     'PB'          DSMPETIP
034900131118     C*
035000131118     C                   CALLB     'XMPE'
035100131118     C                   PARM                    XMPEDS
035200131118     C                   PARM                    XMPEDS1
035300131118     C*
035400131118     C     Risultato     IfEq      '0'
035500131118     C                   MoveL     DSMPEPAR      DSInterm
035600131118     C                   endif
035700131118     C*
035800131118     C                   ENDSR
035900131118     C/EJECT
036000150205     C************************************************************
036100150205     C* Reperimento dati Intermediario
036200150205     C************************************************************
036300150205     C     RepDatInter2  BEGSR
036400150205     C*
036500150205     C* preparo i parametri di chiamata al driver
036600150205     C                   Clear                   XMPEDS
036700150205     C                   EVAL      TpChiamata = '01'
036800150205     C                   EVAL      NChiavi = 5
036900150205     C                   EVAL      VLogica = 2
037000150205     C*
037100150205     C* preparo le chiavi per la chiamata al driver
037200150205     C                   Clear                   XMPEDS1
037300150205     C                   MoveL     XSCSOC        DSMPESOC
037400150205     C                   MoveL     'SDGSK60R S'  DSMPEPGM
037500150205     C                   Move      'S'           DSMPEPGM
037600150205     C                   MoveL     *blanks       DSMPEPRF
037700150205     C* Si reperiscono i dati di default a seconda delle modalità con
037800150205     C* cui il pgm è usato
037900150205     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
038000150205     C* MPETIP = 'PV'  pgm richiamato da menù
038100150205     C* MPETIP = 'PB'  x lancio Batch
038200150205     C                   MoveL     'PB'          DSMPETIP
038300150205     C*
038400150205     C                   CALLB     'XMPE'
038500150205     C                   PARM                    XMPEDS
038600150205     C                   PARM                    XMPEDS1
038700150205     C*
038800150205     C     Risultato     IfEq      '0'
038900150205     C                   MoveL     DSMPEPAR      DSInterm2
039000150205     C                   EndIf
039100150205     C*
039200150205     C                   EndSR
039300131114     C************************************************************
039400131114     C* Azzeramento progressivi
039500131114     C************************************************************
039600131115     C     AzzeraProg    BEGSR
039700131114     C*
039800150130     C                   Z-Add     0             Cnt_D
039900150130     C                   Z-Add     0             Cnt_H
040000131115     C*
040100150202     C                   Z-Add     0             CntDA
040200150202     C                   Z-Add     0             CntAU
040300131114     C*
040400150202     C                   Z-Add     0             Prog_DA
040500150202     C                   Z-Add     0             Prog_AU
040600131114     C*
040700131114     C                   Z-Add     0             $NrRecTot
040800131114     C*
040900131115     C                   ENDSR
041000131115     C/EJECT
041100131118     C******************************************************
041200131118     C* Elaboro dati di invio per contare il numero di record
041300131118     C******************************************************
041400131118     C     ElaboraDati   BEGSR
041500131118     C*
041600150130     C                   EXSR      Inz770
041700150130     C                   EXSR      Set770
041800131118     C*
041900131118     C* Conto quanti record per calcolare il numero di invii
042000150130     C                   DOW       $Fine770    = *Off
042100150130     C                   EXSR      Lettura770
042200131118     C*
042300150212     C                   if        N77Escludi <> '1'
042400150216     C****                         and N77Cde  =  *blank
042500150216     C****                         and N77Num  =  *blank
042600131118     C                   if        SoloPrimaVolta = *On
042700131118     C                   EXSR      ElaboraKey
042800131118     C                   endif
042900131118     C*
043000131118     C                   if        SavKey   <> NewKey  or
043100150130     C                             $Fine770  = *On
043200131118     C                   Add       1             $NrRecTot
043300150202     C                   Add       1             CntDA
043400150203     C                   Add       1             CntAU
043500131118     C                   EXSR      ElaboraKey
043600131118     C                   endif
043700150210     C                   endif
043800131118     C*
043900131118     C                   endDo
044000131118     C*
044100131118     C                   ENDSR
044200131118     C/EJECT
044300131118     C******************************************************
044400150130     C* Inizializzo campi
044500131118     C******************************************************
044600150130     C     Inz770        BEGSR
044700131118     C*
044800150130     C                   Eval      $Fine770    = *Off
044900131118     C                   Clear                   NewKey
045000150205     C*
045100150206     C                   Eval      WImp004    =  0
045200150206     C                   Eval      WImp005    =  0
045300150206     C                   Eval      WImp006    =  0
045400150206     C                   Eval      WImp007    =  0
045500150206     C                   Eval      WImp008    =  0
045600150206     C                   Eval      WImp009    =  0
045700150206     C                   Eval      WImp010    =  0
045800150206     C                   Eval      WImp011    =  0
045900150206     C                   Eval      WImp012    =  0
046000150206     C                   Eval      WImp013    =  0
046100150206     C                   Eval      WImp014    =  0
046200150206     C                   Eval      WImp015    =  0
046300150206     C                   Eval      WImp016    =  0
046400150206     C                   Eval      WImp017    =  0
046500150206     C                   Eval      WImp018    =  0
046600150206     C                   Eval      WImp019    =  0
046700150206     C                   Eval      WImp020    =  0
046800150206     C                   Eval      WImp021    =  0
046900150206     C                   Eval      WImp022    =  0
047000150206     C                   Eval      WImp023    =  0
047100131118     C*
047200131118     C                   ENDSR
047300131118     C/EJECT
047400131118     C************************************************************
047500131118     C* inz ds
047600131118     C************************************************************
047700150130     C     Set770        BEGSR
047800131118     C*
047900131118     C                   Eval      SoloPrimaVolta = *On
048000150130     C*
048100150130     C                   Eval      N77Societa  = SocietaK61
048200150130     C                   Eval      N77Acf      = AnnoK61
048300150130     C     K0277007      setll     Nd77007l
048400131118     C*
048500131118     C                   ENDSR
048600131118     C/EJECT
048700131118     C******************************************************
048800131118     C* Ciclo dei dati estratti
048900131118     C******************************************************
049000150130     C     Lettura770    BEGSR
049100131118     C*
049200150130     C     K0277007      reade     Nd77007l
049300150130     C                   if        %eof(Nd77007l)
049400150130     C                   eval      $Fine770  = *On
049500131118     C                   endif
049600131118     C*
049700131118     C                   ENDSR
049800131118     C/EJECT
049900131118     C******************************************************
050000131118     C* Salvataggio chiavi
050100131118     C******************************************************
050200131118     C     ElaboraKey    BEGSR
050300131118     C*
050400150202     C                   eval      SavCdf      = N77Cdf
050500150202     C                   eval      SavCde      = N77Cde
050600150206     C                   eval      SavC77      = N77C77
050700150206     C                   eval      SavCodice   = N77Codice
050800131118     C*
050900131118     C                   Eval      SoloPrimaVolta = *Off
051000131118     C*
051100131118     C                   ENDSR
051200131118     C/EJECT
051300131118     C************************************************************
051400131118     C* Reperimento numero max di invii
051500131118     C************************************************************
051600131118     C     RepNrInvii    BEGSR
051700131118     C*
051800131118     C                   if        $NrRecTot   > 0
051900131118     C                   Z-Add     $NrRecTot     WImpInp
052000150130     C                   CallP     X0201MultC(WImpInp: 1: MaxRecK61:
052100131118     C                                        '+': '0': 0: WImpOut)
052200131118     C                   Z-Add     WImpOut       $NrInvii
052300131118     C                   endif
052400131118     C*
052500131118     C                   ENDSR
052600131118     C/EJECT
052700000000     C******************************************************
052800000000     C* Gestione singolo invio
052900000000     C******************************************************
053000131115     C     GesSingInv    BEGSR
053100000000     C*
053200131115     C                   EXSR      RepNomeF
053300131115     C                   EXSR      GesFilTel
053400000000     C*
053500150130     C                   OPEN      SDGCUW00F
053600000000     C*
053700131112     C* Record A  testata della fornitura
053800131115     C                   EXSR      WrtTpRecA
053900131112     C*
054000131112     C* Record B  testata della fornitura
054100131115     C                   EXSR      WrtTpRecB
054200131115     C*
054300150130     C                   EXSR      Inz770
054400150130     C                   EXSR      Set770
054500000000     C*
054600150130     C                   DOW       $Fine770    = *Off
054700150130     C                   EXSR      Lettura770
054800000000     C*
054900150212     C                   if        N77Escludi <> '1'
055000150216     C****                         and N77Cde  =  *blank
055100150216     C****                         and N77Num  =  *blank
055200131118     C                   if        SoloPrimaVolta = *On
055300131115     C                   EXSR      ElaboraKey
055400131115     C                   endif
055500000000     C*
055600131115     C                   if        SavKey        <> NewKey  or
055700150130     C                             $Fine770  = *On
055800131115     C                   if        $I         <> 0
055900150203     C*
056000150203     C                   EXSR      WrtTpRecD
056100150203     C                   EXSR      InzSkieraD
056200150203     C*
056300150203     C                   EXSR      WrtTpRecH
056400150203     C                   EXSR      InzSkieraH
056500150203     C*
056600131118     C                   Add       1             $NrRecTot
056700150202     C                   Add       1             CntDA
056800150203     C                   Add       1             CntAU
056900131115     C                   EXSR      ElaboraKey
057000131115     C                   endif
057100131115     C                   endif
057200000000     C*
057300150130     C                   EXSR      ElaboraRecD
057400150203     C*
057500150203     C                   EXSR      ElaboraRecH
057600150209     C                   endif
057700000000     C*
057800150130     C                   enddo
057900150210     C*
058000150210     C* Aggiorna record B con totale comunicazioni relative
058100150210     C* a certificazioni lavoro autonomo
058200150210     C                   EXSR      UpdTpRecB
058300131112     C*
058400131112     C* Record Z di coda della fornitura
058500131115     C                   EXSR      WrtTpRecZ
058600131112     C*
058700131115     C                   EXSR      IscrCatF
058800000000     C*
058900150130     C                   CLOSE     SDGCUW00F
059000000000     C*
059100131115     C                   ENDSR
059200131115     C/EJECT
059300131118     C************************************************************
059400150130     C* Reperimento nome file
059500131118     C************************************************************
059600131118     C     RepNomeF      BEGSR
059700131118     C*
059800131118     C                   Eval      WNumero     = *All'0'
059900150130     C                   Z-Add     AnnoK61       WAnno
060000131118     C                   Z-Add     $ProgrInvio   WPrgInvio
060100131118     C                   Move      WNumero       $Numero
060200131118     C*
060300131118     C                   CALL      'X35NOMFR'
060400150130     C                   PARM                    SocietaK61
060500131118     C                   PARM      *Blank        $Unita            8
060600150205     C                   PARM      'CU'          $CdScarico        2
060700131118     C                   PARM                    $Numero           9
060800131118     C                   PARM                    $File            10
060900131118     C                   PARM                    $Libreria        10
061000131118     C                   PARM                    $Errore           1
061100131118     C*
061200131118     C                   if        $Errore    <> '0'
061300131118     C                   Eval      $TpErrore   = '10'
061400131118     C                   EXSR      StpErrore
061500131118     C                   EXSR      ENDPGM
061600131118     C                   endif
061700131118     C*
061800131118     C                   ENDSR
061900131118     C/EJECT
062000131118     C************************************************************
062100131118     C* Gestione file per invio telematico
062200131118     C************************************************************
062300131118     C     GesFilTel     BEGSR
062400131118     C*
062500131118     C* Creo già la descrizione del file
062600131118     C                   Clear                   $DesFileO
062700131118     C*
062800150130     C                   Move      AnnoK61       AnnoA             4
062900131118     C                   MoveL     AnnoA         $DesFileO
063000131118     C*
063100131118     C                   Cat       CostFile:1    $DesFileO
063200131118     C                   Cat       XScSoc :1     $DesFileO
063300131118     C*
063400131118     C                   Move      UDateIso      $DtAlfa
063500131118     C*
063600150130     C                   Eval      $Libreria   = LibK61
063700131118     C                   Eval      $Errore     = *Off
063800131118     C*
063900150130     C                   CALL      'SDGSK61C1'
064000131118     C                   PARM                    $File
064100131118     C                   PARM                    $Libreria
064200131118     C                   PARM                    WTpDist           2
064300131118     C                   PARM                    $DtAlfa          10
064400131118     C                   PARM                    $DesFileI         8
064500131118     C                   PARM                    $DesFileO        50
064600131118     C                   PARM                    $Errore           1
064700131118     C*
064800131118     C                   if        $Errore    <> '0'
064900131118     C                   Eval      $TpErrore   = '11'
065000131118     C                   EXSR      StpErrore
065100131118     C                   EXSR      ENDPGM
065200131118     C                   endif
065300131118     C*
065400131118     C                   ENDSR
065500131118     C/EJECT
065600131118     C******************************************************
065700131118     C* Riempimento dati per t.r. A
065800131118     C******************************************************
065900131118     C     WrtTpRecA     BEGSR
066000131118     C*
066100150130     C                   Clear                   SDGCUW000
066200150130     C                   Clear                   SDGSK61DSA
066300131118     C*
066400131118     C*-----------------------------------
066500131118     C* DATI IDENTIFICATIVI DELLA FORNITURA
066600131118     C*
066700131118     C*-
066800131118     C* Tipo record
066900150130     C                   MoveL     'A'           K61ATR
067000131118     C*-
067100131118     C* Codice identificativo della fornitura
067200150130     C                   MoveL     'CUR15'       K61AFOR
067300131118     C*-
067400131118     C* Tipo fornitore
067500131118     C*      "01" Soggetti che inviano la propria
067600131118     C*      "10" Intermediari
067700150130     C                   MoveL     '01'          K61ATFO
067800131118     C                   if        $FisInt       <> *Blanks
067900150130     C                   MoveL     '10'          K61ATFO
068000131118     C                   endif
068100131118     C*-
068200131118     C* Codice fiscale del contribuente
068300150130     C                   MoveL     XScCdF        K61ACFI
068400131118     C                   if        $FisInt       <> *Blanks
068500150130     C                   Move      $FisInt       K61ACFI
068600131118     C                   endif
068700131118     C*
068800131118     C*-----------------------------------
068900131118     C*-
069000131118     C* Carattere di controllo
069100150130     C                   MoveL     'A'           K61AK1
069200131118     C*-
069300131118     C* Elementi di fine riga
069400131118     C* Impostare i valori esadecimali '0D' e '0A'
069500131118     C* (caratteri ASCI 'CR' ed 'LF')
069600131118     C*
069700150130     C                   Eval      K61AK2  = *Blanks
069800150130     C                   Eval      K61AK2  = X'0D25'
069900131118     C*
070000150130     C                   MoveL     SDGSK61DSA    CUWREC
070100150130     C                   write     SDGCUW000
070200150130     C                   feod      SDGCUW00F
070300131118     C*
070400131118     C                   ENDSR
070500131118     C/EJECT
070600131118     C******************************************************
070700131118     C* Riempimento dati per t.r. B
070800131118     C******************************************************
070900131118     C     WrtTpRecB     BEGSR
071000131118     C*
071100150130     C                   Clear                   SDGCUW000
071200150130     C                   Clear                   SDGSK61DSB
071300131118     C*
071400131118     C* Tipo record
071500150130     C                   MoveL     'B'           K61BTR
071600131118     C*-
071700131118     C* Codice fiscale del contribuente
071800150130     C                   MoveL     XScCdF        K61BCFI
071900131118     C* Progressivo modulo
072000150130     C                   Z-Add     1             K61BPRGMOD
072100131118     C*-
072200131118     C* Identificativo produttore software
072300150205     C                   MoveL     $IdProd       K61BIPS
072400131118     C*-
072500131118     C* Tipo Comunicazione
072600150130     C                   Move      '0'           K61BCSOS
072700150130     C                   Move      '0'           K61BCANN
072800131118     C*-
072900131118     C                   Select
073000150130     C**                 When      TIPINVK61     = 0
073100150130     C**                 Move      '1'           K61BCORD
073200150130     C* comunicazione da sostituire
073300150130     C                   When      TIPINVK61     = 1
073400150130     C                   Move      '1'           K61BCSOS
073500150130     C* comunicazione da annullare
073600150130     C                   When      TIPINVK61     = 2
073700150130     C                   Move      '1'           K61BCANN
073800131118     C                   EndSl
073900131118     C*
074000131118     C*
074100150130     C* DATI IDENTIFICATIVI DEL CONTRIBUENTE
074200131118     C* reperisco la natura giuridica del contribuente
074300131118     C                   MoveL     XScNCR        WrkNatGiu
074400131118     C                   EXSR      RepNatGiu
074500131118     C*
074600131118     C                   Select
074700131118     C*-
074800131118     C*-----------------------
074900131118     C* PERSONA FISICA
075000131118     C* da impostare solo nel caso di persona fisica
075100131118     C                   When      TPNG16      = '1'
075200131118     C*
075300131118     C                   EXSR      RepPersFis
075400131118     C*-
075500131118     C* Cognome
075600150130     C                   MoveL     $CogRagSog    K61BCOGNO
075700131118     C*-
075800131118     C* Nome
075900150130     C                   MoveL     $NomeSog      K61BNOME
076000131118     C*-----------------------
076100131118     C* PERSONA GIURIDICA
076200131118     C* da impostare solo nel caso di persona non fisica
076300131118     C                   When      TPNG16      = '2'  or
076400131118     C                             TPNG16      = '3'
076500131118     C*-
076600131118     C* Denominazione
076700150130     C                   MoveL     XSCRGS        K61BDENOMI
076800131118     C*
076900131118     C                   EndSl
077000150130     C*
077100150130     C* e-mail
077200150205     C                   MoveL     $Email        K61BEMAIL
077300150130     C*
077400150130     C* Telefono
077500150205     C                   if        $Telefon <> *blank
077600150205     C                   move      $Telefon      K61Btel
077700150205     C                   endif
077800150130     C*
077900150130     C*-----------------------------------
078000150130     C* DATI SOGGETTO CHE SI IMPEGNA ALLA TRASMISSIONE
078100150130     C* (soggetto che effettua la comunicazione, se diverso dal soggetto
078200150130     C*  a cui si riferisce la comunicazione)
078300150130     C*
078400150130     C                   Move      *Blanks       K61BCDFIS1
078500150205     C                   Move      *zeros        K61BCDFIS2
078600150130     C* Se significativo
078700150130     C                   if        $FisSog       <> *Blanks
078800150205     C* Cod.Fiscale del rappresentante
078900150130     C                   Move      $FisSog       K61BCDFIS1
079000150130     C* Cod.Carica
079100150130     C                   Move      $CarSog       K61BCARR1
079200150130     C* Persona Fisica
079300150130     c                   if        $COGSOG       <> *Blanks
079400150130     C                   Eval      K61BCOGNO1   = $CogSOG
079500150130     C                   Eval      K61BNOME1    = $NomSOG
079600150130     C                   endif
079700150130     C*
079800150130     C* Cod.Fiscale società dichiarante
079900150130     C                   if        $FisDic <> *blank
080000150205     C                   Movel     $FisDic       K61BCDFIS2
080100150130     C                   endif
080200150130     C*-
080300150130     C                   endif
080400131118     C*-----------------------------------
080500150130     C*
080600150130     C                   z-add     0             K61BNrDip
080700150210     C                   z-add     0             K61BNrAut
080800150205     C                   Move      '0'           K61BQuaCT
080900150130     C*
081000150130     C* Firma del dichiarante
081100150130     C                   if        $FirDic    <> *blank
081200150205     C                   Eval      K61BFirma   = '1'
081300150130     C                   endif
081400150130     C*
081500150130     C*-----------------------------------
081600131118     C* DATI SOGGETTO CHE SI IMPEGNA ALLA TRASMISSIONE (Intermediario)
081700131118     C*
081800131118     C* Cod.Fisc.Intermediario
081900150130     C                   Move      *Blanks       K61BCFINT
082000131118     C* Impegno alla presentazione
082100150130     C                   Z-Add     0             K61BIMPE
082200131118     C* Data dell'Impegno alla presentazione
082300150130     C                   Move      *All'0'       K61BDTIMPE
082400150130     C* Firma
082500150205     C                   move      '0'           K61BFIRINT
082600131118     C*
082700131118     C* Se significativo
082800131118     C                   if        $FisInt       <> *Blanks
082900131118     C* Cod.Fisc.Intermediario
083000150130     C                   Move      $FisInt       K61BCFINT
083100131118     C* Impegno alla presentazione
083200150130     C                   Move      $ImpTrs       K61BIMPE
083300131118     C* Data dell'Impegno alla presentazione
083400131118     C                   Move      $ImpDta       DtAMG
083500131118     C                   EXSR      ValorizDTA
083600150130     C                   Eval      K61BDTIMPE   = WrkDt
083700150130     C* Firma dell'intemediario
083800150130     C                   if        $FirInt    <> *blank
083900150205     C                   Eval      K61BFirInt  = '1'
084000150130     C                   endif
084100131118     C                   endif
084200150130     C*-
084300150130     C*-----------------------------------
084400131118     C*-
084500131118     C* Carattere di controllo
084600150130     C                   MoveL     'A'           K61BK1
084700131118     C*-
084800131118     C* Elementi di fine riga
084900131118     C* Impostare i valori esadecimali '0D' e '0A'
085000131118     C* (caratteri ASCI 'CR' ed 'LF')
085100131118     C*
085200150130     C                   Eval      K61BK2 =  *Blanks
085300150130     C                   Eval      K61BK2 =  X'0D25'
085400131118     C*
085500150130     C                   MoveL     SDGSK61DSB    CUWREC
085600150130     C                   write     SDGCUW000
085700150130     C                   feod      SDGCUW00F
085800150210     C                   z-add     NrrCuw        SavNrrCuw
085900131118     C*
086000131118     C                   ENDSR
086100131118     C/EJECT
086200150210     C******************************************************
086300150210     C* Aggiorno record B
086400150210     C******************************************************
086500150210     C     UpdTpRecB     BEGSR
086600150210     C*
086700150210     C     2             chain     SdgCuw00f
086800150210     C                   if        %found(SdgCuw00f)
086900150210     C                   movel     CuwRec        SdgSk61DsB
087000150210     C*
087100150210     C                   z-add     Cnt_Righe     K61BNrAut
087200150210     C*
087300150210     C                   movel     SdgSk61DsB    CuwRec
087400150210     C                   update    SdgCuw000
087500150210     C                   endif
087600150210     C*
087700150210     C                   ENDSR
087800150210     C/EJECT
087900131118     C******************************************************
088000131118     C* Reperimento della natura giuridica
088100131118     C******************************************************
088200131118     C     RepNatGiu     BEGSR
088300131118     C*
088400131118     C                   Reset                   SDGTABDS
088500131118     C*
088600131118     C                   MOVE      '1'           XTBRIC
088700150130     C                   MOVE      SocietaK61    XTBAZI
088800131118     C                   MOVE      *ZERO         XTBKEY
088900131118     C                   MOVE      'G16'         XTBCOD
089000131118     C                   MOVE      WrkNatGiu     XTBKEY
089100131118     C                   CALL      'SDGTABDR'
089200131118     C                   PARM                    SDGTABDS
089300131118     C                   if        XTBERR      = *Off
089400131118     C                   MoveL     XTBUNI        ANGG16DS
089500131118     C                   endif
089600131118     C*
089700131118     C                   ENDSR
089800131118     C/EJECT
089900131118     C************************************************************
090000131118     C* Reperisci dati persona fisica attraverso soggetto
090100131118     C************************************************************
090200131118     C     RepPersFis    BEGSR
090300131118     C*
090400131118     C                   Clear                   $CdFiscSog
090500131118     C                   Clear                   $CdIVASog
090600131118     C                   Clear                   $CogRagSog
090700131118     C                   Clear                   $NomeSog
090800131118     C                   Clear                   $DtNascSog
090900131118     C                   Clear                   $LocNascSog
091000131118     C                   Clear                   $PrvNascSog
091100131118     C                   Clear                   $StatoSog
091200131118     C                   Clear                   $StaFedSog
091300131118     C                   Clear                   $LocalitSog
091400131118     C                   Clear                   $IndirESog
091500131118     C                   Clear                   $SexSog
091600131118     C*
091700131118     C                   Eval      SO3Sogg     = SocSogg
091800131118     C                   Eval      SO3DtFiVl   = DatMin
091900131118     C                   Eval      SO3Sys      = 0
092000131118     C                   Eval      SO3NrAsInt  = 0
092100131118     C*
092200131118     C     K04SO301      CHAIN     SDGSO301L                          21
092300131118     C                   if        *In21       = *Off
092400131118     C                   MoveL(P)  SO3CdFisc     $CdFiscSog
092500131118     C                   MoveL(P)  SO3PartIva    $CdIVASog
092600131118     C                   MoveL(P)  SO3CogRag     $CogRagSog
092700131118     C                   MoveL(P)  SO3Nome       $NomeSog
092800131118     C                   Move      SO3DtNasc     $DtNascSog
092900131118     C                   MoveL(P)  SO3LocNasc    $LocNascSog
093000131118     C                   MoveL(P)  SO3PrvNasc    $PrvNascSog
093100131118     C                   MoveL(P)  SO3Stato      $StatoSog
093200131118     C                   MoveL(P)  SO3StaFed     $StaFedSog
093300131118     C                   MoveL(P)  SO3Localit    $LocalitSog
093400131118     C                   MoveL(P)  SO3IndirE     $IndirESog
093500131118     C                   MoveL     SO3Sex        $SexSog
093600131118     C                   endif
093700131118     C*
093800131118     C                   ENDSR
093900131118     C/EJECT
094000131118     C******************************************************
094100131118     C* Valorizzazione data per telematico
094200131118     C******************************************************
094300131118     C     ValorizDTA    BEGSR
094400131118     C*
094500131118     C                   Clear                   WrkDt
094600131118     C*
094700131118     C                   Move      AMGAnno       GMAAnno
094800131118     C                   Move      AMGMese       GMAMese
094900131118     C                   Move      AMGGiorno     GMAGiorno
095000131118     C*
095100131118     C                   Eval      WrkDt        = DtGMA
095200131118     C*
095300131118     C                   ENDSR
095400131118     C******************************************************
095500150130     C* Scrittura t.r. D
095600131118     C******************************************************
095700150130     C     WrtTpRecD     BEGSR
095800131128     C*
095900150202     C                   add       1             Cnt_Righe
096000131118     C*
096100150130     C                   Clear                   SDGSK61DSD
096200131118     C* Tipo Record
096300150130     C                   Eval      K61DTR       = 'D'
096400150130     C*
096500131118     C* Cod.Fiscale
096600150130     C                   Eval      K61DCFI      = XScCdF
096700150130     C*
096800131118     C* Progressivo modulo
096900150202     C                   Eval      K61DPRGMOD   = 1
097000150130     C*
097100150130     C* Cod.Fiscale Percipiente
097200150206     C                   Eval      K61DCFP      = SavCdf
097300150130     C*
097400150130     C* Progressivo Certificazione
097500150202     C                   Eval      K61DPrgCer   = Cnt_Righe
097600150130     C*
097700150130     C* Protocollo
097800150130     C                   Eval      K61DProt     = ProtocK61
097900150130     C                   Eval      K61DProtD    = ProtodK61
098000150130     C*
098100150130     C* Tipo operazione
098200150130     C                   Select
098300150130     C* Sostituzione
098400150130     C                   When      TIPINVK61     = 1
098500150130     C                   Move      'S'           K61DAnnSos
098600150130     C* Annullamento
098700150130     C                   When      TIPINVK61     = 2
098800150130     C                   Move      'A'           K61DAnnSos
098900150130     C                   EndSl
099000150130     C*
099100150130     C* Singola certificazione confermata
099200150130     C                   Move      '0'           K61DSCConf
099300150130     C*
099400150205     C                   MoveA     Skiera        K61DDATI
099500150130     C*
099600131118     C* Carattere di controllo
099700150130     C                   MoveL     'A'           K61DK1
099800131118     C*-
099900131118     C* Elementi di fine riga
100000131118     C* Impostare i valori esadecimali '0D' e '0A'
100100131118     C* (caratteri ASCI 'CR' ed 'LF')
100200131118     C*
100300150130     C                   Eval      K61DK2  = *Blanks
100400150130     C                   Eval      K61DK2  = X'0D25'
100500131118     C*
100600150130     C                   Add       1             Cnt_D
100700150130     C                   Clear                   SDGCUW000
100800150130     C                   Movel     SDGSK61DSD    CUWREC
100900150130?    C                   write     SDGCUW000
101000150130     C                   feod      SDGCUW00F
101100131118     C*
101200131118     C                   ENDSR
101300131118     C/EJECT
101400131118     C******************************************************
101500131118     C* Inizializzo campi per la gestione della tabella
101600131118     C******************************************************
101700150203     C     InzSkieraD    BEGSR
101800131118     C*
101900150205     C                   reset                   Skiera
102000150205     C                   z-add     0             $I
102100131118     C*
102200150205     C                   move      *Off          $FlDatiAna
102300131118     C*
102400131118     C                   ENDSR
102500131118     C/EJECT
102600131118     C******************************************************
102700150203     C* Dati anagrafici del percipiente
102800131118     C******************************************************
102900150130     C     ElaboraRecD   BEGSR
103000131118     C*
103100150205     C                   Eval      WSavQuaD  ='DA'
103200131118     C*
103300150130     C                   if        $Fine770  = *Off
103400131118     C*
103500150202     C                   Z-Add     1             Prog_DA
103600131118     C*
103700131118     C                   if        $FlDatiAna    = *Off
103800131118     C*
103900150203     C* ---------------------------------------------------------------------------------
104000150202     C* Dati relativi al datore di lavoro, ente pensionistico o altro sostituto d'imposta
104100150203     C* ---------------------------------------------------------------------------------
104200150202     C* DA001
104300150202     C*
104400150202     C* 1) Cod.Fiscale
104500150202     C                   Z-Add     Prog_DA       Conta_Rigo
104600150202     C                   Z-Add     1             Conta_Colo
104700150202     C                   MoveL     XscCdf        Campo_Valore
104800150202     C                   MoveL     XscCdf        C60
104900150202     C                   if        Campo_Valore <> *Blanks
105000150203     C                   EXSR      RieSkieraAlf
105100150202     C                   endif
105200150202     C*
105300150202     C                   Select
105400150202     C*-----------------------
105500150202     C* PERSONA FISICA
105600150202     C                   When      TPNG16      = '1'
105700150202     C*
105800150202     C* 2) Cognome
105900150202     C                   if        $CogRagSog    <> *Blanks
106000150202     C                   Z-Add     Prog_DA       Conta_Rigo
106100150202     C                   Z-Add     2             Conta_Colo
106200150202     C                   MoveL     $CogRagSog    Campo_Valore
106300150202     C                   MoveL     $CogRagSog    C60
106400150202     C                   if        Campo_Valore <> *Blanks
106500150203     C                   EXSR      RieSkieraAlf
106600150202     C                   endif
106700150202     C                   endif
106800150202     C*
106900150202     C* 3) Nome
107000150202     C                   if        $NomeSog      <> *Blanks
107100150203     C                   Z-Add     Prog_DA       Conta_Rigo
107200150202     C                   Z-Add     3             Conta_Colo
107300150202     C                   Clear                   Campo_Valore
107400150202     C                   MoveL     $NomeSog      Campo_Valore
107500150202     C                   MoveL     $NomeSog      C60
107600150202     C                   if        Campo_Valore <> *Blanks
107700150203     C                   EXSR      RieSkieraAlf
107800150202     C                   endif
107900150202     C                   endif
108000150202     C*
108100150202     C* 4) Comune di residenza
108200150205     C                   if        $LocalitSog   <> *Blanks
108300150202     C                   Z-Add     Prog_DA       Conta_Rigo
108400150202     C                   Z-Add     4             Conta_Colo
108500150202     C                   Clear                   Campo_Valore
108600150205     C                   MoveL     $LocalitSog   Campo_Valore
108700150205     C                   MoveL     $LocalitSog   C60
108800150202     C                   if        Campo_Valore <> *Blanks
108900150203     C                   EXSR      RieSkieraAlf
109000150202     C                   endif
109100150202     C                   endif
109200150202     C*
109300150202     C* 5) Provincia di residenza - CONTROLLARE
109400150205     C**                 if        Provincia     <> *Blanks
109500150205     C**                 Z-Add     Prog_DA       Conta_Rigo
109600150205     C**                 Z-Add     5             Conta_Colo
109700150205     C**                 Clear                   Campo_Valore
109800150205     C**                 MoveL     N77Prv        Campo_Valore
109900150205     C**                 MoveL     N77Prv        C60
110000150205     C**                 if        Campo_Valore <> *Blanks
110100150205     C**                 EXSR      RieSkieraAlf
110200150205     C**                 endif
110300150205     C**                 endif
110400150202     C*-----------------------
110500150202     C* PERSONA GIURIDICA
110600150202     C* da impostare solo nel caso di persona non fisica
110700150202     C                   When      TPNG16      = '2'  or
110800150202     C                             TPNG16      = '3'
110900150202     C*
111000150202     C* 2) Denominazione
111100150202     C                   if        XscRgs        <> *Blanks
111200150202     C                   Z-Add     Prog_DA       Conta_Rigo
111300150202     C                   Z-Add     2             Conta_Colo
111400150202     C                   MoveL     XscRgs        Campo_Valore
111500150202     C                   MoveL     XscRgs        C60
111600150202     C                   if        Campo_Valore <> *Blanks
111700150203     C                   EXSR      RieSkieraAlf
111800150202     C                   endif
111900150202     C                   endif
112000150202     C*
112100150202     C                   EndSl
112200150202     C*
112300150205     C*
112400150205     C* 8) Telefono
112500150205     C                   if        $Telefon      <> *Blanks
112600150205     C                   Z-Add     Prog_DA       Conta_Rigo
112700150205     C                   Z-Add     8             Conta_Colo
112800150205     C                   Clear                   Campo_Valore
112900150205     C                   MoveL     $Telefon      Campo_Valore
113000150205     C                   MoveL     $Telefon      C60
113100150205     C                   if        Campo_Valore <> *Blanks
113200150205     C                   EXSR      RieSkieraAlf
113300150205     C                   endif
113400150205     C                   endif
113500150205     C*
113600150205     C* 9) E-mail
113700150205     C                   if        $Email        <> *Blanks
113800150205     C                   Z-Add     Prog_DA       Conta_Rigo
113900150205     C                   Z-Add     9             Conta_Colo
114000150205     C                   Clear                   Campo_Valore
114100150205     C                   MoveL     $Email        Campo_Valore
114200150205     C                   MoveL     $Email        C60
114300150205     C                   if        Campo_Valore <> *Blanks
114400150205     C                   EXSR      RieSkieraAlf
114500150205     C                   endif
114600150205     C                   endif
114700150205     C*
114800150205     C* 10) Codice attività
114900150205     C                   if        XscAtt        <> *Blanks
115000150205     C                   Z-Add     Prog_DA       Conta_Rigo
115100150205     C                   Z-Add     10            Conta_Colo
115200150205     C                   Clear                   Campo_Valore
115300150205     C                   MoveL     XscAtt        Campo_Valore
115400150205     C                   MoveL     XscAtt        C60
115500150205     C                   if        Campo_Valore <> *Blanks
115600150205     C                   EXSR      RieSkieraAlf
115700150205     C                   endif
115800150205     C                   endif
115900150202     C*
116000150202     C* DA002
116100150202     C* Dati relativi al dipendente, pensionato o altro percettore delle somme
116200150203     C* ----------------------------------------------------------------------
116300150202     C                   Add       1             Prog_DA
116400150202     C*
116500150202     C* 1) Cod.Fiscale
116600150202     C                   if        N77Cdf        <> *Blanks
116700150202     C                   Z-Add     Prog_DA       Conta_Rigo
116800150202     C                   Z-Add     1             Conta_Colo
116900150202     C                   Clear                   Campo_Valore
117000150202     C                   MoveL     N77Cdf        Campo_Valore
117100150202     C                   MoveL     N77Cdf        C60
117200150202     C                   if        Campo_Valore <> *Blanks
117300150203     C                   EXSR      RieSkieraAlf
117400150202     C                   endif
117500150202     C                   endif
117600131118     C*
117700150202     C* 2) Cognome
117800150202     C                   if        N77Cog        <> *Blanks
117900150202     C                   Z-Add     Prog_DA       Conta_Rigo
118000150202     C                   Z-Add     2             Conta_Colo
118100150202     C                   Clear                   Campo_Valore
118200150202     C                   MoveL     N77Cog        Campo_Valore
118300150202     C                   MoveL     N77Cog        C60
118400131118     C                   if        Campo_Valore <> *Blanks
118500150203     C                   EXSR      RieSkieraAlf
118600131118     C                   endif
118700131118     C                   endif
118800131118     C*
118900150202     C* 3) Nome
119000150202     C                   if        N77Nom        <> *Blanks
119100150202     C                   Z-Add     Prog_DA       Conta_Rigo
119200150202     C                   Z-Add     3             Conta_Colo
119300131118     C                   Clear                   Campo_Valore
119400150202     C                   MoveL     N77Nom        Campo_Valore
119500150202     C                   MoveL     N77Nom        C60
119600131118     C                   if        Campo_Valore <> *Blanks
119700150203     C                   EXSR      RieSkieraAlf
119800131118     C                   endif
119900131118     C                   endif
120000150202     C*
120100150202     C* 4) Sesso
120200150202     C                   if        N77Ses        <> *Blanks
120300150202     C                   Z-Add     Prog_DA       Conta_Rigo
120400150202     C                   Z-Add     4             Conta_Colo
120500150202     C                   Clear                   Campo_Valore
120600150202     C                   MoveL     N77Ses        Campo_Valore
120700150202     C                   MoveL     N77Ses        C60
120800150202     C                   if        Campo_Valore <> *Blanks
120900150203     C                   EXSR      RieSkieraAlf
121000150202     C                   endif
121100150202     C                   endif
121200150202     C*
121300150202     C* 5) Data di nascita
121400150206     C                   if        N77Dnc        <> Datmin
121500150202     C                   Move      N77Dnc        DtAMG
121600150202     C                   Move      AMGAnno       GMAAnno
121700150202     C                   Move      AMGMese       GMAMese
121800150202     C                   Move      AMGGiorno     GMAGiorno
121900150202     C                   Z-Add     Prog_DA       Conta_Rigo
122000150202     C                   Z-Add     5             Conta_Colo
122100150202     C                   Clear                   Campo_Valore
122200150202     C                   Move      DtGMA         Campo_Valore
122300150202     C                   if        Campo_Valore <> *Blanks
122400150203     C                   EXSR      RieSkiera
122500150202     C                   endif
122600150202     C                   endif
122700150202     C*
122800150202     C* 6) Comune o Stato estero di nascita
122900150202     C                   if        N77Lnc        <> *Blanks
123000150202     C                   Z-Add     Prog_DA       Conta_Rigo
123100150202     C                   Z-Add     6             Conta_Colo
123200150202     C                   Clear                   Campo_Valore
123300150202     C                   MoveL     N77Lnc        Campo_Valore
123400150202     C                   MoveL     N77Lnc        C60
123500150202     C                   if        Campo_Valore <> *Blanks
123600150203     C                   EXSR      RieSkieraAlf
123700150202     C                   endif
123800150202     C                   endif
123900150202     C*
124000150202     C* 7) Provincia di nascita
124100150202     C                   if        N77Pnc        <> *Blanks
124200150202     C                   Z-Add     Prog_DA       Conta_Rigo
124300150202     C                   Z-Add     7             Conta_Colo
124400150202     C                   Clear                   Campo_Valore
124500150202     C                   MoveL     N77Pnc        Campo_Valore
124600150202     C                   if        Campo_Valore <> *Blanks
124700150203     C                   EXSR      RieSkiera
124800150202     C                   endif
124900150202     C                   endif
125000150202     C*
125100150205     C* 8) Categorie particolari
125200150205     C* 9) Eventi eccezionali
125300150205     C* 10) Casi di esclusione dalla precompilata
125400150202     C*
125500150202     C* DA002020
125600150202     C*
125700150202     C* Domicilio fiscale al 1/1/2014
125800150203     C* -----------------------------
125900150206     C                   if        %subst(N77C77:1:1) = 'N'
126000150202     C* 20) Comune
126100150202     C                   if        N77Cit        <> *Blanks
126200150202     C                   Z-Add     Prog_DA       Conta_Rigo
126300150202     C                   Z-Add     20            Conta_Colo
126400150202     C                   Clear                   Campo_Valore
126500150202     C                   MoveL     N77Cit        Campo_Valore
126600150202     C                   MoveL     N77Cit        C60
126700150202     C                   if        Campo_Valore <> *Blanks
126800150203     C                   EXSR      RieSkieraAlf
126900150202     C                   endif
127000150202     C                   endif
127100150202     C*
127200150202     C* 21) Provincia
127300150202     C                   if        N77Prv        <> *Blanks
127400150202     C                   Z-Add     Prog_DA       Conta_Rigo
127500150202     C                   Z-Add     21            Conta_Colo
127600150202     C                   Clear                   Campo_Valore
127700150202     C                   MoveL     N77Prv        Campo_Valore
127800150202     C                   MoveL     N77Prv        C60
127900150202     C                   if        Campo_Valore <> *Blanks
128000150203     C                   EXSR      RieSkiera
128100150202     C                   endif
128200150202     C                   endif
128300150202     C*
128400150202     C* 22) Codice comune
128500150202     C                   if        N77Com        <> *Blanks
128600150202     C                   Z-Add     Prog_DA       Conta_Rigo
128700150202     C                   Z-Add     22            Conta_Colo
128800150202     C                   Clear                   Campo_Valore
128900150202     C                   MoveL     N77Com        Campo_Valore
129000150202     C                   MoveL     N77Com        C60
129100150202     C                   if        Campo_Valore <> *Blanks
129200150203     C                   EXSR      RieSkieraAlf
129300150202     C                   endif
129400150202     C                   endif
129500150202     C*
129600150204     C                   endif
129700150202     C* DA002023
129800150202     C*
129900150202     C* Domicilio fiscale al 1/1/2015
130000150203     C* -----------------------------
130100150202     C* 23) Comune
130200150202     C                   if        N77Cit        <> *Blanks
130300150202     C                   Z-Add     Prog_DA       Conta_Rigo
130400150202     C                   Z-Add     23            Conta_Colo
130500150202     C                   Clear                   Campo_Valore
130600150202     C                   MoveL     N77Cit        Campo_Valore
130700150202     C                   MoveL     N77Cit        C60
130800150202     C                   if        Campo_Valore <> *Blanks
130900150203     C                   EXSR      RieSkieraAlf
131000150202     C                   endif
131100150202     C                   endif
131200150202     C*
131300150202     C* 24) Provincia
131400150202     C                   if        N77Prv        <> *Blanks
131500150202     C                   Z-Add     Prog_DA       Conta_Rigo
131600150202     C                   Z-Add     24            Conta_Colo
131700150202     C                   Clear                   Campo_Valore
131800150202     C                   MoveL     N77Prv        Campo_Valore
131900150202     C                   MoveL     N77Prv        C60
132000150202     C                   if        Campo_Valore <> *Blanks
132100150203     C                   EXSR      RieSkieraAlf
132200150202     C                   endif
132300150202     C                   endif
132400150202     C*
132500150202     C* 25) Codice comune
132600150202     C                   if        N77Com        <> *Blanks
132700150202     C                   Z-Add     Prog_DA       Conta_Rigo
132800150202     C                   Z-Add     25            Conta_Colo
132900150202     C                   Clear                   Campo_Valore
133000150202     C                   MoveL     N77Com        Campo_Valore
133100150202     C                   MoveL     N77Com        C60
133200150202     C                   if        Campo_Valore <> *Blanks
133300150203     C                   EXSR      RieSkiera
133400150202     C                   endif
133500150202     C                   endif
133600131118     C*
133700150202     C* Dati relativi al rappresentante
133800150203     C* -------------------------------
133900150202     C* DA002030
134000150202     C*
134100150203     C* 30) Cod.Fiscale
134200150205     C***                if        K61BCdFis1    <> *Blanks
134300150205     C***                Z-Add     Prog_DA       Conta_Rigo
134400150205     C***                Z-Add     30            Conta_Colo
134500150205     C***                Clear                   Campo_Valore
134600150205     C***                MoveL     K61BCdFis1    Campo_Valore
134700150205     C***                MoveL     K61BCdFis1    C60
134800150205     C***                if        Campo_Valore <> *Blanks
134900150205     C***                EXSR      RieSkieraAlf
135000150205     C***                endif
135100150205     C***                endif
135200150203     C*
135300150203     C* Riservato ai percipienti esteri
135400150205     C* -------------------------------
135500150203     C* DA002040
135600150202     C*
135700150203     C* 40) Codice di identificazione fiscale estero
135800150203     C                   if        N77Cde        <> *Blank
135900150203     C                   Z-Add     Prog_DA       Conta_Rigo
136000150203     C                   Z-Add     40            Conta_Colo
136100131118     C                   Clear                   Campo_Valore
136200150203     C                   Movel     N77Cde        Campo_Valore
136300150203     C                   Movel     N77Cde        C60
136400131118     C                   if        Campo_Valore <> *Blanks
136500150203     C                   EXSR      RieSkieraAlf
136600131118     C                   endif
136700131118     C                   endif
136800150203     C*
136900150203     C* 41) Località di residenza estera
137000150203     C                   if        N77RCit       <> *Blank
137100150203     C                   Z-Add     Prog_DA       Conta_Rigo
137200150203     C                   Z-Add     41            Conta_Colo
137300150203     C                   Clear                   Campo_Valore
137400150203     C                   Movel     N77RCit       Campo_Valore
137500150203     C                   Movel     N77RCit       C60
137600150203     C                   if        Campo_Valore <> *Blanks
137700150203     C                   EXSR      RieSkieraAlf
137800150203     C                   endif
137900150203     C                   endif
138000150203     C*
138100150203     C* 42) Via e numero civico
138200150203     C                   if        N77RVia       <> *Blank
138300150203     C                   Z-Add     Prog_DA       Conta_Rigo
138400150203     C                   Z-Add     42            Conta_Colo
138500150203     C                   Clear                   Campo_Valore
138600150203     C                   Movel     N77RVia       Campo_Valore
138700150203     C                   Movel     N77RVia       C60
138800150203     C                   if        Campo_Valore <> *Blanks
138900150203     C                   EXSR      RieSkieraAlf
139000150203     C                   endif
139100150203     C                   endif
139200150203     C*
139300150203     C* 43) Codice stato estero
139400150203     C                   if        N77Cse        <> *Blank
139500150203     C                   Z-Add     Prog_DA       Conta_Rigo
139600150203     C                   Z-Add     43            Conta_Colo
139700150203     C                   Clear                   Campo_Valore
139800150209     C                   movel     N77Cse        Cse               9
139900150209     C                   callb     'XALLINEA'
140000150209     C                   parm                    Cse
140100150209     C                   parm      9             Len2              5 0
140200150209     C                   parm      '2'           Input             1
140300150209     C                   move      Cse           Campo_Valore
140400150203     C                   if        Campo_Valore <> *Blanks
140500150203     C                   EXSR      RieSkiera
140600150203     C                   endif
140700150203     C                   endif
140800131118     C*
140900150203     C* Sezione firma certificazione unica
141000150205     C* ----------------------------------
141100150203     C* DA003001
141200150203     C*
141300150203     C                   Add       1             Prog_DA
141400131118     C*
141500150203     C* 1) Data Firma Sostituto d'imposta
141600150203     C                   if        $DtFirDic    <> *blank
141700150203     C                   Move      $DtFirDic     DtAMG
141800150203     C                   Move      AMGAnno       GMAAnno
141900150203     C                   Move      AMGMese       GMAMese
142000150203     C                   Move      AMGGiorno     GMAGiorno
142100150203     C                   Z-Add     Prog_DA       Conta_Rigo
142200150203     C                   Z-Add     1             Conta_Colo
142300150203     C                   Clear                   Campo_Valore
142400150203     C                   Move      DtGMA         Campo_Valore
142500150203     C                   if        Campo_Valore <> *Blanks
142600150203     C                   EXSR      RieSkiera
142700150203     C                   endif
142800150203     C                   endif
142900150203     C*
143000150203     C* 2) Firma
143100150205     C                   if        $FirDic    <> *blank
143200150203     C                   Z-Add     Prog_DA       Conta_Rigo
143300150203     C                   Z-Add     2             Conta_Colo
143400150203     C                   Clear                   Campo_Valore
143500150203     C                   Move      '1'           Campo_Valore
143600150203     C                   EXSR      Ablank
143700150203     C                   if        Campo_Valore <> *Blanks
143800150203     C                   EXSR      RieSkiera
143900150203     C                   endif
144000150203     C                   endif
144100131118     C*
144200131118     C                   Move      *On           $FlDatiAna
144300131118     C*
144400131118     C                   endif
144500131118     C*
144600131118     C                   endif
144700131118     C*
144800131118     C                   ENDSR
144900131118     C/EJECT
145000150203     C******************************************************
145100150203     C* Scrittura t.r. H
145200150203     C******************************************************
145300150203     C     WrtTpRecH     BEGSR
145400150206     C*
145500150206     C                   exsr      GesImporti
145600150203     C*
145700150203     C                   Clear                   SDGSK61DSH
145800150203     C* Tipo Record
145900150203     C                   Eval      K61HTR       = 'H'
146000150203     C*
146100150203     C* Cod.Fiscale
146200150203     C                   Eval      K61HCFI      = XScCdF
146300150203     C*
146400150203     C* Progressivo modulo
146500150203     C                   Eval      K61HPRGMOD   = 1
146600150203     C*
146700150203     C* Cod.Fiscale Percipiente
146800150206     C                   Eval      K61HCFP      = SavCdf
146900150203     C*
147000150203     C* Progressivo Certificazione
147100150203     C                   Eval      K61HPrgCer   = K61DPrgCer
147200150203     C*
147300150203     C                   MoveA     SkieraH       K61HDATI
147400150203     C*
147500150203     C* Carattere di controllo
147600150206     C                   MoveL     'A'           K61HK1
147700150203     C*-
147800150203     C* Elementi di fine riga
147900150203     C* Impostare i valori esadecimali '0D' e '0A'
148000150203     C* (caratteri ASCI 'CR' ed 'LF')
148100150203     C*
148200150203     C                   Eval      K61HK2  = *Blanks
148300150203     C                   Eval      K61HK2  = X'0D25'
148400150203     C*
148500150203     C                   Add       1             Cnt_H
148600150203     C                   Clear                   SDGCUW000
148700150203     C                   Movel     SDGSK61DSH    CUWREC
148800150203?    C                   write     SDGCUW000
148900150203     C                   feod      SDGCUW00F
149000150203     C*
149100150203     C                   ENDSR
149200150203     C/EJECT
149300150203     C******************************************************
149400150203     C* Inizializzo campi per la gestione della tabella
149500150203     C******************************************************
149600150203     C     InzSkieraH    BEGSR
149700150203     C*
149800150203     C                   Reset                   SkieraH
149900150203     C                   z-add     0             $H
150000150206     C*
150100150206     C                   move      *Off          $FlDatiImp
150200150205     C*
150300150206     C                   Eval      WImp004    =  0
150400150206     C                   Eval      WImp005    =  0
150500150206     C                   Eval      WImp006    =  0
150600150206     C                   Eval      WImp007    =  0
150700150206     C                   Eval      WImp008    =  0
150800150206     C                   Eval      WImp009    =  0
150900150206     C                   Eval      WImp010    =  0
151000150206     C                   Eval      WImp011    =  0
151100150206     C                   Eval      WImp012    =  0
151200150206     C                   Eval      WImp013    =  0
151300150206     C                   Eval      WImp014    =  0
151400150206     C                   Eval      WImp015    =  0
151500150206     C                   Eval      WImp016    =  0
151600150206     C                   Eval      WImp017    =  0
151700150206     C                   Eval      WImp018    =  0
151800150206     C                   Eval      WImp019    =  0
151900150206     C                   Eval      WImp020    =  0
152000150206     C                   Eval      WImp021    =  0
152100150206     C                   Eval      WImp022    =  0
152200150206     C                   Eval      WImp023    =  0
152300150203     C*
152400150203     C                   ENDSR
152500150203     C/EJECT
152600150203     C******************************************************
152700150203     C* Dati relativi alle certificazioni
152800150203     C******************************************************
152900150203     C     ElaboraRecH   BEGSR
153000150203     C*
153100150205     C                   Eval      WSavQuaH  = 'AU'
153200150203     C*
153300150203     C                   if        $Fine770  = *Off
153400150203     C*
153500150203     C* AU001
153600150203     C*
153700150203     C                   Z-Add     1             Prog_AU
153800150206     C*
153900150206     C                   if        $FlDatiImp    = *Off
154000150203     C*
154100150203     C* 1) Causale
154200150203     C                   if        N77C77        <> *Blanks
154300150203     C                   Z-Add     Prog_AU       Conta_Rigo
154400150203     C                   Z-Add     1             Conta_Colo
154500150203     C                   Clear                   Campo_Valore
154600150206     C****               MoveL     N77C77        Campo_Valore
154700150216     C                   eval      Campo_Valore = %subst(N77C77:1:2)
154800150203     C                   if        Campo_Valore <> *Blanks
154900150203     C                   EXSR      RieSkieraH
155000150203     C                   endif
155100150203     C                   endif
155200150203     C*
155300150203     C* 2) Anno
155400150206     C                   if        N77Acf        <> 0
155500150203     C                   Z-Add     Prog_AU       Conta_Rigo
155600150203     C                   Z-Add     2             Conta_Colo
155700150203     C                   Clear                   Campo_Valore
155800150203     C                   Move      N77Anno       Campo_Valore
155900150203     C                   EXSR      Ablank
156000150203     C                   if        Campo_Valore <> *Blanks
156100150203     C                   EXSR      RieSkieraH
156200150203     C                   endif
156300150203     C                   endif
156400150203     C*
156500150203     C* 3) Anticipazione
156600150203     C                   if        N77Tit    <> *blank
156700150203     C                   Z-Add     Prog_AU       Conta_Rigo
156800150203     C                   Z-Add     3             Conta_Colo
156900150203     C                   Clear                   Campo_Valore
157000150203     C                   Move      '1'           Campo_Valore
157100150203     C                   EXSR      Ablank
157200150203     C                   if        Campo_Valore <> *Blanks
157300150203     C                   EXSR      RieSkieraH
157400150203     C                   endif
157500150203     C                   endif
157600150206     C*
157700150206     C                   Move      *On           $FlDatiImp
157800150206     C*
157900150206     C                   endif
158000150206     C*
158100150206     C*-----------------------------------
158200150206     C* MEMORIZZA IMPORTI
158300150203     C*
158400150203     C* 4) Ammontare lordo corrisposto
158500150206     C                   add       N77Itc        WImp004
158600150206     C*
158700150206     C* 5) Somme non soggette a ritenuta per regime convenzionale
158800150206     C                   setoff                                       202122
158900150206     C     N77Num        comp      *blank                             2020
159000150206     C   20N77Rac        comp      0                                      21
159100150206     C   20N77Imn        comp      0                                  22
159200150206     C*    Straniero senza ritenute e con imp. non soggetto
159300150206     C   20
159400150206     Can 21
159500150206     Can 22N77Imn        add       N77SNS        WImp005
159600150206     C*
159700150206     C* 7) Altre somme non soggette a ritenute
159800150206     C* Italia
159900150206     C  N20N77IMN        add       N77SNS        WImp007
160000150206     C* Straniero senza ritenute senza imp. non soggetto
160100150206     C   20
160200150206     CAN 21
160300150206     CANN22N77IMN        add       N77SNS        WImp007
160400150206     C* STRANIERO CON RITENUTE
160500150206     C   20
160600150206     CANN21N77IMN        add       N77SNS        WImp007
160700150206     C*
160800150206     C* 8) Imponibile
160900150206     C                   add       N77Ims        WImp008
161000150206     C*
161100150206     C* 9) Ritenute a titolo d'acconto
161200150206     C                   add       N77Rac        WImp009
161300150206     C*
161400150206     C* 10) Ritenute a titolo di imposta
161500150206     C                   add       N77Rim        WImp010
161600150206     C*
161700150206     C* 11) Ritenute sospese
161800150206     C                   add       N77Net        WImp011
161900150206     C*
162000150206     C* 12) Addizionale regionale a titolo d'acconto
162100150206     C                   add       N77Arg        WImp012
162200150206     C*
162300150206     C* 13) Addizionale regionale a titolo di imposta
162400150206     C                   add       N77Ari        WImp013
162500150206     C*
162600150206     C* 14) Addizionale regionale sospesa
162700150206     C                   add       N77Ars        WImp014
162800150206     C*
162900150206     C* 15) Addizionale comunale a titolo d'acconto
163000150206     C                   add       N77Acta       WImp015
163100150206     C*
163200150206     C* 16) Addizionale comunale a titolo di imposta
163300150206     C                   add       N77Acg        WImp016
163400150206     C*
163500150206     C* 17) Addizionale comunale sospesa
163600150206     C                   add       N77Acs        WImp017
163700150206     C*
163800150206     C* 18) Imponibile anni precedenti
163900150206     C                   add       N77Imsap      WImp018
164000150206     C*
164100150206     C* 19) Ritenute operate anni precedenti
164200150206     C                   add       N77Racap      WImp019
164300150206     C*
164400150206     C* 20) Contributi previdenziali a carico del soggetto erogante
164500150206     C                   add       N77Ctbdl      WImp020
164600150206     C*
164700150206     C* 21) Contributi previdenziali a carico del percipiente
164800150206     C                   add       N77Ctblv      WImp021
164900150206     C*
165000150206     C* 22) Spese rimborsate
165100150206     C                   add       N77SpeRimb    WImp022
165200150206     C*
165300150206     C* 23) Ritenute rimborsate
165400150206     C                   add       N77RitRimb    WImp023
165500150203     C*
165600150203     C                   endif
165700150203     C*
165800150203     C                   ENDSR
165900150203     C/EJECT
166000131118     C******************************************************
166100131118     C* Tratta campi alfanumerici
166200131118     C******************************************************
166300150203     C     RieSkieraAlf  BEGSR
166400131118     C*
166500131118     C                   Z-ADD     Conta_Rigo    Cod_Rigo
166600131118     C                   Z-ADD     Conta_Colo    Cod_Colo
166700131118     C*
166800131118     C                   Add       1             $I
166900131118     C                   MoveL     Campo_Codice  Skiera($I)
167000150205     C                   MoveL     WSavQuaD      Skiera($I)
167100131118     C                   Move      Campo_Valore  Skiera($I)
167200131118     C                   Clear                   Campo_Valore
167300131118     C*
167400131118     C                   Clear                   Campo
167500131118     C*
167600131118     C* Controllo se ci sono dati dopo la 17° posizione
167700131118     C                   Do        3             XY                1 0
167800131118     C*
167900131118     C                   if        C15(XY) <> *Blanks
168000131118     C                   Add       1             $I
168100131118     C                   MOVEL     '+'           C16              16
168200131118     C                   MOVE      C15(XY)       C16
168300131118     C                   MoveL     Campo_Codice  Skiera($I)
168400150205     C                   MoveL     WSavQuaD      Skiera($I)
168500131118     C                   Move      C16           Skiera($I)
168600131118     C                   endif
168700131118     C*
168800131118     C                   endDo
168900131118     C*
169000131118     C                   Clear                   C60
169100131118     C*
169200131118     C                   ENDSR
169300131118     C/EJECT
169400131118     C******************************************************
169500131118     C* Riempimento skiera per i dati non posizionali
169600131118     C******************************************************
169700150203     C     RieSkiera     BEGSR
169800131118     C*
169900131118     C                   Z-ADD     Conta_Rigo    Cod_Rigo
170000131118     C                   Z-ADD     Conta_Colo    Cod_Colo
170100131118     C*
170200131118     C                   ADD       1             $I
170300131118     C                   MoveL     Campo_Codice  Skiera($I)
170400150205     C                   MoveL     WSavQuaD      Skiera($I)
170500131118     C                   Move      Campo_Valore  Skiera($I)
170600131118     C                   Clear                   Campo_Valore
170700131118     C*
170800131118     C                   ENDSR
170900131118      /EJECT
171000150203     C******************************************************
171100150203     C* Tratta campi alfanumerici
171200150203     C******************************************************
171300150203     C     RieSkieraAlfH BEGSR
171400150203     C*
171500150203     C                   Z-ADD     Conta_Rigo    Cod_Rigo
171600150203     C                   Z-ADD     Conta_Colo    Cod_Colo
171700150203     C*
171800150203     C                   Add       1             $H
171900150205     C                   MoveL     Campo_Codice  SkieraH($H)
172000150205     C                   MoveL     WSavQuaH      SkieraH($H)
172100150205     C                   Move      Campo_Valore  SkieraH($H)
172200150203     C                   Clear                   Campo_Valore
172300150203     C*
172400150203     C                   Clear                   Campo
172500150203     C*
172600150203     C* Controllo se ci sono dati dopo la 17° posizione
172700150203     C                   Do        3             XY                1 0
172800150203     C*
172900150203     C                   if        C15(XY) <> *Blanks
173000150203     C                   Add       1             $H
173100150203     C                   MOVEL     '+'           C16              16
173200150203     C                   MOVE      C15(XY)       C16
173300150205     C                   MoveL     Campo_Codice  SkieraH($H)
173400150205     C                   MoveL     WSavQuaH      SkieraH($H)
173500150205     C                   Move      C16           SkieraH($H)
173600150203     C                   endif
173700150203     C*
173800150203     C                   endDo
173900150203     C*
174000150203     C                   Clear                   C60
174100150203     C*
174200150203     C                   ENDSR
174300150203     C/EJECT
174400150203     C******************************************************
174500150203     C* Riempimento skiera per i dati non posizionali
174600150203     C******************************************************
174700150203     C     RieSkieraH    BEGSR
174800150203     C*
174900150203     C                   Z-ADD     Conta_Rigo    Cod_Rigo
175000150203     C                   Z-ADD     Conta_Colo    Cod_Colo
175100150203     C*
175200150203     C                   ADD       1             $H
175300150205     C                   MoveL     Campo_Codice  SkieraH($H)
175400150205     C                   MoveL     WSavQuaH      SkieraH($H)
175500150205     C                   Move      Campo_Valore  SkieraH($H)
175600150203     C                   Clear                   Campo_Valore
175700150203     C*
175800150203     C                   ENDSR
175900150203      /EJECT
176000150206     C******************************************************
176100150206     C* Valorizzo i campi importi
176200150206     C******************************************************
176300150206     C     GesImporti    BEGSR
176400150206     C*
176500150206     C* AU001
176600150206     C*
176700150206     C* 4) Ammontare lordo corrisposto
176800150206     C                   Eval      WImporto      = WImp004
176900150206     C*
177000150206     C                   if        WImporto      <> 0
177100150206     C                   Z-Add     Prog_AU       Conta_Rigo
177200150206     C                   Z-Add     4             Conta_Colo
177300150206     C                   EXSR      RieImportoH
177400150206     C                   endif
177500150206     C*
177600150206     C* 5) Somme non soggette a ritenuta per regime convenzionale
177700150206     C                   Eval      WImporto      = WImp005
177800150206     C*
177900150206     C                   if        WImporto      <> 0
178000150206     C                   Z-Add     Prog_AU       Conta_Rigo
178100150206     C                   Z-Add     5             Conta_Colo
178200150206     C                   EXSR      RieImportoH
178300150206     C                   endif
178400150206     C*
178500150206     C* 6) Codice
178600150206     C                   if        SavCodice <> *blank
178700150206     C                   Z-Add     Prog_AU       Conta_Rigo
178800150206     C                   Z-Add     6             Conta_Colo
178900150206     C                   Clear                   Campo_Valore
179000150206     C                   Move      SavCodice     Campo_Valore
179100150206     C                   EXSR      Ablank
179200150206     C                   if        Campo_Valore <> *Blanks
179300150206     C                   EXSR      RieSkieraH
179400150206     C                   endif
179500150206     C                   endif
179600150206     C*
179700150206     C* 7) Altre somme non soggette a ritenute
179800150206     C                   Eval      WImporto      = WImp007
179900150206     C*
180000150206     C                   if        WImporto      <> 0
180100150206     C                   Z-Add     Prog_AU       Conta_Rigo
180200150206     C                   Z-Add     7             Conta_Colo
180300150206     C                   EXSR      RieImportoH
180400150206     C                   endif
180500150206     C*
180600150206     C* 8) Imponibile
180700150206     C                   Eval      WImporto      = WImp008
180800150206     C*
180900150206     C                   if        WImporto      <> 0
181000150206     C                   Z-Add     Prog_AU       Conta_Rigo
181100150206     C                   Z-Add     8             Conta_Colo
181200150206     C                   EXSR      RieImportoH
181300150206     C                   endif
181400150206     C*
181500150206     C* 9) Ritenute a titolo d'acconto
181600150206     C                   Eval      WImporto      = WImp009
181700150206     C*
181800150206     C                   if        WImporto      <> 0
181900150206     C                   Z-Add     Prog_AU       Conta_Rigo
182000150206     C                   Z-Add     9             Conta_Colo
182100150206     C                   EXSR      RieImportoH
182200150206     C                   endif
182300150206     C*
182400150206     C* 10) Ritenute a titolo di imposta
182500150206     C                   Eval      WImporto      = WImp010
182600150206     C*
182700150206     C                   if        WImporto      <> 0
182800150206     C                   Z-Add     Prog_AU       Conta_Rigo
182900150206     C                   Z-Add     10            Conta_Colo
183000150206     C                   EXSR      RieImportoH
183100150206     C                   endif
183200150206     C*
183300150206     C* 11) Ritenute sospese
183400150206     C                   Eval      WImporto      = WImp011
183500150206     C*
183600150206     C                   if        WImporto      <> 0
183700150206     C                   Z-Add     Prog_AU       Conta_Rigo
183800150206     C                   Z-Add     11            Conta_Colo
183900150206     C                   EXSR      RieImportoH
184000150206     C                   endif
184100150206     C*
184200150206     C* 12) Addizionale regionale a titolo d'acconto
184300150206     C                   Eval      WImporto      = WImp012
184400150206     C*
184500150206     C                   if        WImporto      <> 0
184600150206     C                   Z-Add     Prog_AU       Conta_Rigo
184700150206     C                   Z-Add     12            Conta_Colo
184800150206     C                   EXSR      RieImportoH
184900150206     C                   endif
185000150206     C*
185100150206     C* 13) Addizionale regionale a titolo di imposta
185200150206     C                   Eval      WImporto      = WImp013
185300150206     C*
185400150206     C                   if        WImporto      <> 0
185500150206     C                   Z-Add     Prog_AU       Conta_Rigo
185600150206     C                   Z-Add     13            Conta_Colo
185700150206     C                   EXSR      RieImportoH
185800150206     C                   endif
185900150206     C*
186000150206     C* 14) Addizionale regionale sospesa
186100150206     C                   Eval      WImporto      = WImp014
186200150206     C*
186300150206     C                   if        WImporto      <> 0
186400150206     C                   Z-Add     Prog_AU       Conta_Rigo
186500150206     C                   Z-Add     14            Conta_Colo
186600150206     C                   EXSR      RieImportoH
186700150206     C                   endif
186800150206     C*
186900150206     C* 15) Addizionale comunale a titolo d'acconto
187000150206     C                   Eval      WImporto      = WImp015
187100150206     C*
187200150206     C                   if        WImporto      <> 0
187300150206     C                   Z-Add     Prog_AU       Conta_Rigo
187400150206     C                   Z-Add     15            Conta_Colo
187500150206     C                   EXSR      RieImportoH
187600150206     C                   endif
187700150206     C*
187800150206     C* 16) Addizionale comunale a titolo di imposta
187900150206     C                   Eval      WImporto      = WImp016
188000150206     C*
188100150206     C                   if        WImporto      <> 0
188200150206     C                   Z-Add     Prog_AU       Conta_Rigo
188300150206     C                   Z-Add     16            Conta_Colo
188400150206     C                   EXSR      RieImportoH
188500150206     C                   endif
188600150206     C*
188700150206     C* 17) Addizionale comunale sospesa
188800150206     C                   Eval      WImporto      = WImp017
188900150206     C*
189000150206     C                   if        WImporto      <> 0
189100150206     C                   Z-Add     Prog_AU       Conta_Rigo
189200150206     C                   Z-Add     17            Conta_Colo
189300150206     C                   EXSR      RieImportoH
189400150206     C                   endif
189500150206     C*
189600150206     C* 18) Imponibile anni precedenti
189700150206     C                   Eval      WImporto      = WImp018
189800150206     C*
189900150206     C                   if        WImporto      <> 0
190000150206     C                   Z-Add     Prog_AU       Conta_Rigo
190100150206     C                   Z-Add     18            Conta_Colo
190200150206     C                   EXSR      RieImportoH
190300150206     C                   endif
190400150206     C*
190500150206     C* 19) Ritenute operate anni precedenti
190600150206     C                   Eval      WImporto      = WImp019
190700150206     C*
190800150206     C                   if        WImporto      <> 0
190900150206     C                   Z-Add     Prog_AU       Conta_Rigo
191000150206     C                   Z-Add     19            Conta_Colo
191100150206     C                   EXSR      RieImportoH
191200150206     C                   endif
191300150206     C*
191400150206     C* 20) Contributi previdenziali a carico del soggetto erogante
191500150206     C                   Eval      WImporto      = WImp020
191600150206     C*
191700150206     C                   if        WImporto      <> 0
191800150206     C                   Z-Add     Prog_AU       Conta_Rigo
191900150206     C                   Z-Add     20            Conta_Colo
192000150206     C                   EXSR      RieImportoH
192100150206     C                   endif
192200150206     C*
192300150206     C* 21) Contributi previdenziali a carico del percipiente
192400150206     C                   Eval      WImporto      = WImp021
192500150206     C*
192600150206     C                   if        WImporto      <> 0
192700150206     C                   Z-Add     Prog_AU       Conta_Rigo
192800150206     C                   Z-Add     21            Conta_Colo
192900150206     C                   EXSR      RieImportoH
193000150206     C                   endif
193100150206     C*
193200150206     C* 22) Spese rimborsate
193300150206     C                   Eval      WImporto      = WImp022
193400150206     C*
193500150206     C                   if        WImporto      <> 0
193600150206     C                   Z-Add     Prog_AU       Conta_Rigo
193700150206     C                   Z-Add     22            Conta_Colo
193800150206     C                   EXSR      RieImportoH
193900150206     C                   endif
194000150206     C*
194100150206     C* 23) Ritenute rimborsate
194200150206     C                   Eval      WImporto      = WImp023
194300150206     C*
194400150206     C                   if        WImporto      <> 0
194500150206     C                   Z-Add     Prog_AU       Conta_Rigo
194600150206     C                   Z-Add     23            Conta_Colo
194700150206     C                   EXSR      RieImportoH
194800150206     C                   endif
194900150206     C*
195000150206     C*
195100150206     C                   ENDSR
195200150206     C/EJECT
195300150203     C******************************************************
195400150203     C* Toglie gli zeri non significativi nelle caselle con
195500150203     C* importo dei campi posizionali
195600150203     C******************************************************
195700150203     C     RieImportoH   BEGSR
195800150203     C*
195900150203     C                   Clear                   Campo_Valore
196000150206     C                   Z-Add     WImporto      Importo16_2
196100150203     C*
196200150206     C                   Move      Importo16_2   CampoWrk
196300150206     C                   Move      CampoInt      Int
196400150206     C                   Move      ','           Virgola
196500150206     C                   Move      CampoDec      Dec
196600150206     C                   Move      CampoIntero   Importo16A
196700150206     C*
196800150203     C                   EXSR      Ablank2
196900150203     C*
197000150203     C                   if        Campo_Valore <> *Blanks
197100150203     C                   EXSR      RieSkieraH
197200150203     C                   endif
197300150203     C*
197400150203     C                   ENDSR
197500150203     C/EJECT
197600150206     C******************************************************
197700150206     C* Toglie gli zeri non significativi nelle caselle con
197800150206     C* importo dei campi posizionali
197900150206     C******************************************************
198000150206     C     RieImportoH2  BEGSR
198100150206     C*
198200150206     C                   Clear                   Campo_Valore
198300150206     C                   Z-Add     WImporto      Importo16_0
198400150206     C*
198500150206     C                   Eval      $FlgNeg       = '0'
198600150206     C                   if        WImporto     <   0
198700150206     C                   Eval      Importo16_0  = Importo16_0 * -1
198800150206     C                   endif
198900150206     C                   EXSR      Ablank2
199000150206     C*
199100150206     C                   if        Campo_Valore <> *Blanks
199200150206     C                   EXSR      RieSkieraH
199300150206     C                   endif
199400150206     C*
199500150206     C                   ENDSR
199600150206     C/EJECT
199700131118     C******************************************************
199800131118     C* Toglie gli zeri non significativi al Campo_Valore
199900131118     C******************************************************
200000131118     C     ABlank        BEGSR
200100131118     C*
200200131118     C                   MoveA     Campo_Valore  Sk_Campo
200300131118     C*
200400131118     C                   DO        Max$X         $X
200500131118     C*
200600131118     C                   if        Sk_Campo($X) = ' '   or
200700131118     C                             Sk_Campo($X) = '0'
200800131118     C                   Move      ' '           Sk_Campo($X)
200900131118     C                   else
201000131118     C                   LEAVE
201100131118     C                   endif
201200131118     C*
201300131118     C                   endDo
201400131118     C*
201500131118     C                   MoveA     Sk_Campo      Campo_Valore
201600131118     C*
201700131118     C                   ENDSR
201800131118      /EJECT
201900131118     C******************************************************
202000131118     C* Toglie gli zeri non significativi nelle caselle con
202100131118     C* importo dei campi posizionali
202200131118     C******************************************************
202300131118     C     ABlank2       BEGSR
202400131118     C*
202500131118     C                   MoveA     Importo16A    SK_Importo
202600131118     C*
202700131118     C                   DO        Max$Z         $Z
202800131118     C*
202900131118     C                   if        Sk_Importo($Z) = ' '   or
203000131118     C                             Sk_Importo($Z) = '0'
203100131118     C                   Move      ' '           Sk_Importo($Z)
203200131118     C                   else
203300131118     C                   if        $FlgNeg       = '1'
203400131118     C                   Eval      Sk_Importo($Z-1) = '-'
203500131118     C                   Eval      $FlgNeg       = '0'
203600131118     C                   endif
203700131118     C                   LEAVE
203800131118     C                   endif
203900131118     C*
204000131118     C                   endDo
204100131118     C*
204200131118     C                   MoveA     Sk_Importo    Campo_Valore
204300131118     C*
204400131118     C                   ENDSR
204500131118      /EJECT
204600131118     C******************************************************
204700131118     C* Scrittura t.r. Z
204800131118     C******************************************************
204900131118     C     WrtTpRecZ     BEGSR
205000131118     C*
205100150130     C                   Clear                   SDGSK61DSZ
205200131118     C* Tipo Record
205300150130     C                   Eval      K61ZTR       = 'Z'
205400131118     C*-
205500150130     C                   Z-ADD     1             K61ZNB
205600131118     C*-
205700150130     C                   Z-ADD     0             K61ZNC
205800150130     C*-
205900150130     C                   Z-ADD     Cnt_D         K61ZND
206000150130     C*-
206100150130     C                   Z-ADD     0             K61ZNG
206200150130     C*-
206300150130     C                   Z-ADD     Cnt_H         K61ZNH
206400131118     C*-
206500131118     C* Carattere di controllo
206600150130     C                   MoveL     'A'           K61ZK1
206700131118     C*-
206800131118     C* Elementi di fine riga
206900131118     C* Impostare i valori esadecimali '0D' e '0A'
207000131118     C* (caratteri ASCI 'CR' ed 'LF')
207100131118     C*
207200150130     C                   Eval      K61ZK2  = *Blanks
207300150130     C                   Eval      K61ZK2  = X'0D25'
207400131118     C*
207500150130     C                   Clear                   SDGCUW000
207600150130     C                   MoveL     SDGSK61DSZ    CUWREC
207700150130?    C                   WRITE     SDGCUW000
207800150130     C                   FEOD      SDGCUW00F
207900131118     C*
208000131118     C                   ENDSR
208100131118     C/EJECT
208200131118     C************************************************************
208300131118     C* Iscrizione a catalogo del file scarico
208400131118     C************************************************************
208500131118     C     IscrCatF      BEGSR
208600131118     C*
208700131118     C
208800131118     C                   Eval      WNumero     = *All'0'
208900150130     C                   Z-Add     AnnoK61       WAnno
209000131118     C                   Z-Add     $ProgrInvio   WPrgInvio
209100131118     C                   Move      WNumero       $Numero
209200131118     C*
209300131118     C                   Move      UDateIso      $DtAlfa
209400131118     C*
209500131118     C                   Call      'X36CATFR'
209600150130     C                   PARM                    SocietaK61
209700131118     C                   PARM      *Blank        $Unita
209800131118     C                   PARM                    $File
209900131118     C                   PARM                    $Libreria
210000131118     C                   PARM                    $DesFileO
210100150130     C                   PARM      'CU'          $CdScarico        2
210200131118     C                   PARM      'P'           $TpSupp           1
210300131118     C                   PARM      *Blank        $NmSupp          10
210400131118     C                   PARM      '4'           $TpDist           1
210500131118     C                   PARM                    $DtAlfa          10
210600131118     C                   PARM                    $Numero           9
210700131118     C                   PARM      *Blank        $DesBan          44
210800131118     C                   PARM                    KNmus
210900131118     C                   PARM      '0'           Sospeso           1
211000131118     C                   PARM      '1'           Video             1
211100131118     C*
211200131118     C                   ENDSR
211300131118     C/EJECT
211400000000     C******************************************************
211500000000     C* Gestione invio multiplo
211600000000     C******************************************************
211700131115     C     GesMultInv    BEGSR
211800000000     C*
211900150130     C* Inizializzazione schiera per record D
212000150205     C                   EXSR      InzSkieraD
212100150210     C*
212200150210     C* Inizializzazione schiera per record H
212300150210     C                   EXSR      InzSkieraH
212400131115     C*
212500150130     C                   EXSR      Inz770
212600150130     C                   EXSR      Set770
212700000000     C*
212800150130     C                   DOW       $Fine770    = *Off     and
212900131118     C                             $ProgrInvio < $NrInvii
213000131114     C*
213100131114     C                   Add       1             $ProgrInvio
213200150202     C                   Z-Add     0             Cnt_Righe
213300131122     C                   Eval      SoloPrimaVolta = *On
213400131118     C* Azzeramento
213500131118     C                   EXSR      AzzeraProg
213600150205     C                   EXSR      InzSkieraD
213700150210     C                   EXSR      InzSkieraH
213800131114     C*
213900131115     C                   EXSR      RepNomeF
214000131115     C                   EXSR      GesFilTel
214100131114     C*
214200150130     C                   OPEN      SDGCUW00F
214300131114     C*
214400131114     C* Record A  testata della fornitura
214500131115     C                   EXSR      WrtTpRecA
214600131114     C*
214700131114     C* Record B  testata della fornitura
214800131115     C                   EXSR      WrtTpRecB
214900131118     C*
215000131118     C                   if        $ProgrInvio > 1
215100131122     C                   EXSR      ElaboraKey
215200131122     C*
215300131122     C                   if        SavKey   <> NewKey  or
215400150130     C                             $Fine770  = *On
215500131122     C*
215600150130     C                   EXSR      WrtTpRecD
215700150203     C                   EXSR      InzSkieraD
215800150203     C*
215900150203     C                   EXSR      WrtTpRecH
216000150203     C                   EXSR      InzSkieraH
216100131122     C*
216200131122     C                   Add       1             $NrRecTot
216300150202     C                   Add       1             CntDA
216400150203     C                   Add       1             CntAU
216500131122     C                   EXSR      ElaboraKey
216600131122     C                   endif
216700131122     C*
216800150212     C                   if        N77Escludi <> '1'
216900150216     C****                         and N77Cde  =  *blank
217000150216     C****                         and N77Num  =  *blank
217100150130     C                   EXSR      ElaboraRecD
217200150203     C*
217300150203     C                   EXSR      ElaboraRecH
217400150210     C                   endif
217500131122     C*
217600131118     C                   endif
217700131114     C*
217800131114     C* seconda lettura per scrivere il multifile
217900150130     C                   DOW       $Fine770    = *Off  and
218000150202     C                             (Cnt_Righe   <  MaxRecK61 and
218100150130     C                              MaxRecK61   > 0            )
218200131114     C*
218300150130     C                   if        TipInvK61   <> 2
218400131118     C*
218500150130     C                   EXSR      Lettura770
218600131118     C*
218700150212     C                   if        N77Escludi <> '1'
218800150216     C****                         and N77Cde  =  *blank
218900150216     C****                         and N77Num  =  *blank
219000131118     C                   if        SoloPrimaVolta  = *On
219100131118     C                   EXSR      ElaboraKey
219200131118     C                   endif
219300131118     C*
219400131118     C                   if        SavKey   <> NewKey  or
219500150130     C                             $Fine770  = *On
219600131118     C*
219700150130     C                   EXSR      WrtTpRecD
219800150203     C                   EXSR      InzSkieraD
219900150203     C*
220000150203     C                   EXSR      WrtTpRecH
220100150203     C                   EXSR      InzSkieraH
220200131118     C*
220300131118     C                   Add       1             $NrRecTot
220400150202     C                   Add       1             CntDA
220500150203     C                   Add       1             CntAU
220600131118     C                   EXSR      ElaboraKey
220700131118     C                   endif
220800131118     C*
220900150130     C                   EXSR      ElaboraRecD
221000150203     C*
221100150203     C                   EXSR      ElaboraRecH
221200150210     C                   endif
221300131118     C*
221400131118     C                   endif
221500131114     C*
221600131115     C                   endDo
221700131114     C*
221800131114     C* Record Z di coda della fornitura
221900131115     C                   EXSR      WrtTpRecZ
222000131114     C*
222100131115     C                   EXSR      IscrCatF
222200131114     C*
222300150130     C                   CLOSE     SDGCUW00F
222400131115     C*
222500131115     C                   endDo
222600131115     C*
222700131115     C                   ENDSR
222800131115     C/EJECT
222900000000     C************************************************************
223000000000     C* Stampa errore
223100000000     C************************************************************
223200131115     C     StpErrore     BEGSR
223300000000     C*
223400131114     C*
223500131115     C                   ENDSR
223600131115     C/EJECT
223700150130     C************************************************************
223800150130     C* Inizializzazioni variabili
223900150130     C************************************************************
224000150130     C     INZVAR        BEGSR
224100150130     C*
224200150130     C                   MoveL     KPJBU         SDGSK61DS
224300150130     C*
224400150130     C                   Eval      ERRK61      = *Off
224500150130     C                   Eval      MaxRecK61   = MaxRecK61 -4
224600150130     C*
224700150130     C                   MoveL     'SOC001'      TIPXSC
224800150130     C                   MoveL     SOCIETAK61    SOCXSC
224900150130     C*
225000150130     C                   EXSR      RepSoc
225100150130     C*
225200150130     C                   if        RTNXSC     <> '1'
225300150130     C                   MoveL     XSOCDS        SOC001
225400150130     C                   else
225500150130     C                   Eval      $Errori     = *On
225600150130     C                   EXSR      ENDPGM
225700150130     C                   endif
225800150130     C*
225900150130     C                   Move      XScSoc        SIFSocieta
226000150130     C     K01SIF01      Chain     ANSIF01L                           21
226100150130     C  N21              Move      SIFFlgOpe     WrkFlgOpe
226200150130     C   21              Move      *Blanks       SocSogg
226300150130     C*
226400150130     C                   Z-Add     0             $I
226500150130     C*
226600150130     C                   ENDSR
226700150130     C/EJECT
226800150130     C************************************************************
226900150130     C* Reperimento dati società
227000150130     C************************************************************
227100150130     C     RepSoc        BEGSR
227200150130     C*
227300150130     C                   CALL      'SDGSOCDR'
227400150130     C                   PARM                    TIPXSC            6
227500150130     C                   PARM                    SOCXSC            3
227600150130     C                   PARM                    CDSXSC            9 0
227700150130     C                   PARM                    MODXSC            3
227800150130     C                   PARM      *Blanks       RTNXSC            1
227900150130     C                   PARM                    XSOCDS
228000150130     C                   PARM                    KPJBA
228100150130     C*
228200150130     C                   ENDSR
228300150130     C/EJECT
228400000000     C************************************************************
228500000000     C* OPERAZIONI INIZIALI
228600000000     C************************************************************
228700131115     C     *INZSR        BEGSR
228800000000     C*
228900000000     C* Reperimento parametri
229000000000     C*
229100131115     C     *ENTRY        PLIST
229200131118     C                   PARM                    KPJBA
229300000000     C*
229400000000     C     *DMY          Move      Udate         UDateISO
229500000000     C*
229600131115     C                   ENDSR
229700131115     C/EJECT
229800000000     C************************************************************
229900000000     C* DEFINIZIONE KLIST
230000000000     C************************************************************
230100131115     C     DefKlist      BEGSR
230200000000     C*
230300000000     C* KLIST
230400150130     C*
230500150130     C* Nd77007L
230600150130     C     K0277007      KLIST
230700150130     C                   KFLD                    N77Societa
230800150130     C                   KFLD                    N77Acf
230900000000     C*
231000000000     C* SDGSo301L
231100000000     C     K04SO301      KLIST
231200000000     C                   KFLD                    SO3Sogg
231300000000     C                   KFLD                    SO3DtFiVL
231400000000     C                   KFLD                    SO3Sys
231500000000     C                   KFLD                    SO3NrAsint
231600000000     C*
231700131115     C* ANSIF01L
231800000000     C     K01SIF01      KLIST
231900000000     C                   KFLD                    SIFSocieta
232000000000     C*
232100131115     C                   ENDSR
232200131115     C/EJECT
