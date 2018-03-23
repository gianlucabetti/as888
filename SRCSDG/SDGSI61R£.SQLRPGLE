000100000000     H*PARMS DFTACTGRP(*NO)
000200000000     H*PARMS BNDDIR(PJXBND PJCBND)
000300000000     H*PARMS ACTGRP(QILE)
000400000000     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000500000000     H*PARMS RDB(*NONE) DYNUSRPRF(*OWNER)
000600000000     H DECEDIT('0.') DATEDIT(*DMY.)
000700000000     H*
000800000000      *         ------------------------------------------------
000900000000      *                     Spesometro Trimestrale
001000000000      *                       Creazione file XML
001100000000      *         ------------------------------------------------
001200000000      *
001300000000R258  * Questa PGM è stato creato con il task R258
001400000000      *
001500000000     F*----------------------------------------------------*
001600000000     F* Spesometro trimestrale
001700000000     FSDGCST00F UF   E             DISK    Commit
001800000000     F                                     UsrOpn
001900000000     F                                     Prefix(Cs0:3)
002000170817     FSDGCST10L UF   E           K DISK    Commit
002100170817     F                                     UsrOpn
002200170817     F                                     Prefix(C10:3)
002300170817     F                                     Rename(SdgCst000:SdgCst010)
002301170828R264 FSDGCST02L IF   E           K DISK
002302170828 "   F                                     UsrOpn
002303170828 "   F                                     Prefix(C02:3)
002304170828R264 F                                     Rename(SdgCst000:SdgCst002)
002400000000     F* Catalogo file
002500000000     FSDGCSF00F O    E             DISK    Commit
002600000000     F                                     UsrOpn
002700000000     F*---------------
002800000000     F* File definito a pgm con cui viene sovrascritto il file
002900000000     F* creato in qtemp contenente il metadata
003000000000     FFileO     UF A F  500        DISK    USROPN
003100000000     D*---------------
003200000000     D* DS per mappare metadata
003300000000     D FileoDS         DS
003400000000     D  DsOutput               1    500
003500000000     D*---------------------
003600000000     D* Passaggio Parametri
003700000000     D KPJBA         E DS
003800000000     D*-------------
003900000000     D* Reperimento nome PGM
004000000000     D STATUS         SDS           333
004100000000     D  DSPGM            *PROC
004200000000     D  PARMS            *PARMS
004300000000     D*----------------
004400000000     D* Dati di ambiente ottenuti da SDGSOCDR
004500000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
004600000000     D*-------------
004700000000     D* DS Interna per dati di output di SDGSOCDR
004800000000     D XSOCDS          S           1000
004900000000     D*---------------------
005000000000     D NrDecMC         S              1  0
005100000000     D WNrDecMC        S              1
005200000000     D*-------------
005300000000     D* Parametri modulo SDSPETRI
005400000000     D SDGPA12DS     E DS
005500000000     D*---------------------
005600000000     D SDGSI61DS     E DS
005700000000     D*-------------
005800000000     D* Valori assunti
005900000000     D XMPEDS        E DS                  INZ
006000000000     D XMPEDS1       E DS                  INZ
006100000000     D*-------------
006200000000     D* IdentificativiFiscali
006300000000     D DsFrontesp1     DS                  INZ
006400000000     D $Stato1                             Like(CSTStato1 )
006500000000     D $IdPaes1                            Like(CSTIdPaes1)
006600000000     D $PIva1                              like(CstPiva1)
006700000000     D $CdF1                               like(CstCdFisc1)
006800000000     D $CogRag1                            like(CstCogRag1)
006900000000     D $TpN1                               like(CstTpN1   )
007000000000     D $Denom1                             like(CstDenom1 )
007100000000     D $Cognom1                            like(CstCognom1)
007200000000     D $Nome1                              like(CstNome1  )
007300000000     D DsFrontesp2     DS                  INZ
007400000000     D $INDIR2                             like(CstINDIR2 )
007500000000     D $NRCIV2                             like(CstNRCIV2 )
007600000000     D $CAP2                               like(CstCAP2   )
007700000000     D $LOCALI2                            like(CstLOCALI2)
007800000000     D $PROV2                              like(CstPROV2  )
007900000000     D $Stato2                             Like(CSTStato2 )
008000000000     D $IDPAES2                            like(CstIDPAES2)
008100000000     D* Stabile Organizzazione
008200000000     D DsFrontesp3     DS                  INZ
008300000000     D $INDIR3                             like(CstINDIR3 )
008400000000     D $NRCIV3                             like(CstNRCIV3 )
008500000000     D $CAP3                               like(CstCAP3   )
008600000000     D $LOCALI3                            like(CstLOCALI3)
008700000000     D $PROV3                              like(CstPROV3  )
008800000000     D $Stato3                             Like(CSTStato3 )
008900000000     D $IDPAES3                            like(CstIDPAES3)
009000000000     D* Rappresentante Fiscale
009100000000     D DsFrontesp4     DS                  INZ
009200000000     D $Stato4                             Like(CSTStato4 )
009300000000     D $IDPAES4                            like(CstIDPAES4)
009400000000     D $PIVA4                              like(CstPIVA4  )
009500000000     D $CogRag4                            like(CstCogRag4)
009600000000     D $TpN4                               like(CstTpN4   )
009700000000     D $Denom4                             like(CstDenom4 )
009800000000     D $Cognom4                            like(CstCognom4)
009801170905R265 D**** $Nome4                              like(CstNome4  )
009802170905  "  D* Rappresentante Fiscale
009803170905  "  D DsFrontesp41    DS                  INZ
009804170905R2651D $Nome4                              like(CstNome4  )
010000000000     D*-------------
010001170905R2651D $DimSk          C                   Const(5)
010100000000     D $Cont           S              1S 0
010200170817     D $Errore         S              1    inz('0')
010300000000     D $Trovato        S              1
010400000000     D $RotturaCF      S              1
010500000000     D $RotturaDoc     S              1
010600000000     D $SkDati         S                   Like(DsMpePar) dim(8)
010700000000     D*-------------
010800000000     D RRNCst          S              9b 0
010900000000     D SdgCstDs      E DS                  Extname(SdgCst00F)
011000000000     D*----------
011100000000     D SavCstDs      E DS                  Extname(SdgCst00F)
011200000000     D                                     Prefix(Sav:3)
011300000000     D*----------
011400000000     D Sv1CstDs      E DS                  Extname(SdgCst00F)
011500000000     D                                     Prefix(Sv1:3)
011600000000     D*----------
011700000000     D Sv2CstDs      E DS                  Extname(SdgCst00F)
011800000000     D                                     Prefix(Sv2:3)
011801170828R264 D*----------
011802170828  "  D Sv3CstDs      E DS                  Extname(SdgCst00F)
011803170828R264 D                                     Prefix(Sv3:3)
011900000000     D*----------
012000000000     D indent1         C                   CONST('  ')
012100000000     D indent2         C                   CONST('    ')
012200000000     D indent3         C                   CONST('      ')
012300000000     D indent4         C                   CONST('        ')
012400000000     D indent5         C                   CONST('          ')
012500000000     D indent6         C                   CONST('            ')
012600000000     D indent7         C                   CONST('              ')
012700000000     D indent8         C                   CONST('                ')
012800000000     D indent9         C                   CONST('                  ')
012900170812     D cMaxNrDoc       C                   CONST(1000)
013000000000     D*-------------
013100000000     D* Variabili
013200000000     D $PosizO         S                   like(CstPosizO)
013300000000     D $X              S              5  0
013400000000     D RigaMod         S              9  0 Inz
013500000000     D WDesFile        S             15
013600000000     D WMbr            S             60
013700000000     D WSTMF           S                   LIKE(CartellI61)
013800000000     D wFile           S             10
013900000000     D FileXml         S             50
014000000000     D FileSav         S             50
014100000000     D FileDte         C                   CONST('FileDte')
014200000000     D FileDtr         C                   CONST('FileDtr')
014300000000     D FileAnn         C                   CONST('FileAnn')
014400000000     D UDateISO        S               D    datfmt(*iso)
014500000000     D $Cf             S              1
014600000000     D $TpCom          S              1    inz('T')
014700000000     D $TpRecOrd       S              1    inz('O')
014800000000     D $TpRecRet       S              1    inz('R')
014900000000     D $TpRecAnn       S              1    inz('A')
015000000000     D $FineCST        S              1    Inz(*Off)
015100000000     D $PrimaV         S              1    Inz(*Off)
015200000000     D $NomeF          S             16
015300000000     D StringaSql      DS          5000
015400000000     D SqlCST          S           5000
015500000000     D $Ind            S              4  0
015600000000     D w_ImpOpe        S                   LIKE(CstImpOpe)
015700000000     D w_IvaOpe        S                   LIKE(CstIvaOpe)
015800000000     D WImpInp         S                   Like(ImporD0202)
015900000000     D WImpOut         S                   Like(ImporD0202)
016000000000     D $ProgrInvio     S              5S 0 Inz(0)
016100000000     D $ProgrInt       S              3S 0 Inz(0)
016200000000     D*-------------
016300000000     D* Data 10A
016400000000     D WDataDs         DS            10
016500000000     D WAnnoDs                 1      4A
016600000000     D WMeseDs                 6      7A
016700000000     D WGiornoDs               9     10A
016800000000     D*--
016900000000     D WWImporto       S             19  3
017000000000     D $ImportoA       S             19A
017100000000     D $MeseA          S              2A
017200000000     D*-------------
017300000000     D* Campi I/O per routine editazione di un numero
017400000000     D*
017500000000     D* Parametri X23EDITR
017600000000     D* Numero da editare
017700000000     D $SrcVar         s             30S 0
017800000000     D* Lunghezza del campo da editare
017900000000     D $SrcVarPr       s              9B 0 inz(30)
018000000000     D* Numero cifre decimali da editare
018100000000     D $SrcVarDe       s              9B 0
018200000000     D* Codice di editazione
018300000000     D $EdtCode        s              1    inz('4')
018400000000     D* Lunghezza campo video
018500000000     D $LenFld         s              3S 0
018600000000     D* Campo editato
018700000000     D $RcvrVar        s             50
018800000000     D* Flag errore
018900000000     D $Err            s              1
019000000000     D*----------------
019100000000     D* variabili per invio multiplo
019200000000     D $MultiInvio     S              1A   Inz(*Off)
019300170812     D $MaxRighe       S              5S 0 Inz(0)
019400170812     D $MaxNrDoc       S              5S 0 Inz(0)
019500000000     D*-------------
019600000000     D* DS Esterna gestione messaggi
019700000000     D SDGMSGDS      E DS
019800000000     D  STS          E                     EXTFLD(MSGSTS)
019900000000     D                                     DIM(333)
020000000000     D  JBA          E                     EXTFLD(MSGJBA)
020100000000     D                                     DIM(502)
020200000000     D*-------------
020300000000     D* Prototipo per la procedura Euro
020400000000     D/COPY *LIBL/SRCCPY,PJX002PR
020500000000     D*------------------------------------------------------
020600000000     I* FileOutPut
020700000000     IFileO     NS  01
020800000000     I                                  1  500  OutPut
020900000000     C*----------------------------------------------------*
021000000000     C*                MAIN LINE PROGRAM
021100000000     C*----------------------------------------------------*
021200000000     C*
021300000000     C                   exsr      InzVar
021400000000     C*
021500000000     C                   select
021600000000     C* Invio ordinario o Rettifica
021700000000     C                   when      TipInvI61 = 'O' or
021800000000     C                             TipInvI61 = 'R'
021900000000     C* Vengono creati fino a due file:
022000000000     C* - uno per le fatture emesse   - DTE
022100000000     C* - uno per le fatture ricevute - DTR
022200000000     C*
022300000000     C                   if        CfI61 = 'C'  or
022400000000     C                             CfI61 = *blank
022500000000     C                   eval      $CF   = 'C'
022600000000     C                   eval      wFile = FileDte
022700000000     C                   exsr      Creazione
022800000000     C                   endif
022900000000     C*
023000000000     C                   if        CfI61 = 'F'  or
023100000000     C                             CfI61 = *blank
023200000000     C                   eval      $CF   = 'F'
023300000000     C                   eval      wFile = FileDtr
023400000000     C                   exsr      Creazione
023500000000     C                   endif
023600000000     C*
023700000000     C* Annullamento
023800000000     C                   when      TipInvI61 = 'A'
023900000000     C*
024000000000     C                   eval      wFile = FileAnn
024100000000     C                   exsr      CreaAnnullo
024200000000     C*
024300000000     C                   endsl
024400000000     C*
024500000000     C                   exsr      EndPgm
024600000000     C*
024700000000     C/EJECT
024800000000     C******************************************************
024900000000     C* FINE PROGRAMMA
025000000000     C******************************************************
025100000000     C     ENDPGM        BEGSR
025200000000     C*
025300000000     C                   Seton                                            LR
025400000000     C                   Return
025500000000     C*
025600000000     C                   ENDSR
025700000000     C/EJECT
025800000000     C******************************************************
025900000000     C* Creazione archivio
026000000000     C******************************************************
026100000000     C     Creazione     BEGSR
026200000000     C*
026300000000     C                   exsr      CreoFileO
026400000000     C*
026500000000     C* Lettura file spesometro
026600000000     C                   exsr      CicloSql
026700000000     C*
026800000000     C                   exsr      ChiudoFileO
026900000000     C*
027000000000     C                   ENDSR
027100000000     C/EJECT
027200000000     C******************************************************
027300000000     C* Creazione archivio annullamento
027400000000     C******************************************************
027500000000     C     CreaAnnullo   BEGSR
027600000000     C*
027700000000     C                   exsr      CreoFileO
027800000000     C* Frontespizio
027900000000     C                   EXSR      WrtIntestaz
028000000000     C*
028100000000     C* 4 <ANN>
028200000000     C                   Eval      DsOutput     =
028300000000     C                                      indent1 +
028400000000     C                              '<ANN>'
028500000000     C                   EXSR      WrtXML
028600000000     C*
028700000000     C* 4.1 IdFile
028800000000     C                   Eval      DsOutput     =
028900000000     C                                      indent2 +
029000000000     C                              '<IdFile>' +
029100000000     C                                %trim(IdFileI61) +
029200000000     C                              '</IdFile>'
029300000000     C                   EXSR      WrtXML
029400000000     C*
029500000000     C* 4.2 Posizione
029600000000     C                   if        PosizI61 <> 0
029700000000     C                   Eval      DsOutput     =
029800000000     C                                      indent2 +
029900000000     C                              '<Posizione>' +
030000000000     C                                %trim(%editc(PosizI61:'Z'))  +
030100000000     C                              '</Posizione>'
030200000000     C                   EXSR      WrtXML
030300000000     C                   endif
030400000000     C*
030500000000     C* 4 </ANN>
030600000000     C                   Eval      DsOutput     =
030700000000     C                                      indent1 +
030800000000     C                              '</ANN>'
030900000000     C                   EXSR      WrtXML
031000170817     C*
031100170817     C* Aggiorna record con l'indicazione del file di annullamento
031200170817     C                   EXSR      UpdCstAnn
031300170817     C*
031400000000     C* Chiude File
031500000000     C                   Eval      DsOutput     =
031600000000     C                             '</ns2:DatiFattura>'
031700000000     C                   EXSR      WrtXML
031800000000     C*
031900000000     C                   exsr      ChiudoFileO
032000000000     C*
032100000000     C                   ENDSR
032200000000     C/EJECT
032300000000     C******************************************************
032400000000     C* Scrive TAG INTESTAZIONE
032500000000     C******************************************************
032600000000     C     WrtIntestaz   BEGSR
032700000000     C*
032800000000     C                   Eval      DsOutput     =
032900000000     C                             '<?xml version="1.0" encoding="UTF-8"'
033000000000     C                               + ' standalone="yes"?>'
033100000000     C                   EXSR      WrtXML
033200000000     C*
033300000000     C                   Eval      DsOutput     =
033400000000     C                             '<ns2:DatiFattura versione="DAT20" '
033500000000     C                             + 'xmlns:ns2="http://ivaservizi.'
033600000000     C                             + 'agenziaentrate.gov.it'
033700000000     C                             + '/docs/xsd/fatture/v2.0">'
033800000000     C                   EXSR      WrtXML
033900000000     C*
034000000000     C* 1.DatiFatturaHeader
034100000000     C                   Eval      DsOutput     =
034200000000     C                                      indent1 +
034300000000     C                              '<DatiFatturaHeader>'
034400000000     C                   EXSR      WrtXML
034500000000     C*
034600000000     C* 1.1.ProgressivoInvio
034700000000     C                   Eval      DsOutput     =
034800000000     C                                      indent2 +
034900000000     C                              '<ProgressivoInvio>' +
035000000000     C                               %char($ProgrInvio) +
035100000000     C                              '</ProgressivoInvio>'
035200000000     C                   EXSR      WrtXML
035300000000     C*
035400000000     C                   Eval      DsOutput     =
035500000000     C                                      indent1 +
035600000000     C                              '</DatiFatturaHeader>'
035700000000     C                   EXSR      WrtXML
035800000000     C*
035900000000     C* Invio ordinario o Rettifica
036000000000     C                   if        TipInvI61 = 'O' or
036100000000     C                             TipInvI61 = 'R'
036200000000     C* DTE/DTR
036300000000     C* CedentePrestatoreDTE / CessionarioCommittenteDTR
036400000000     C                   select
036500000000     C                   when      $Cf   = 'C'
036600000000     C* 2.  DTE
036700000000     C                   Eval      DsOutput     =
036800000000     C                                      indent1 +
036900000000     C                              '<DTE>'
037000000000     C                   EXSR      WrtXML
037100000000     C*
037200000000     C* 2.  CedentePrestatoreDTE
037300000000     C                   Eval      DsOutput     =
037400000000     C                                      indent2 +
037500000000     C                              '<CedentePrestatoreDTE>'
037600000000     C                   EXSR      WrtXML
037700000000     C*
037800000000     C                   when      $Cf   = 'F'
037900000000     C* 3.  DTR
038000000000     C                   Eval      DsOutput     =
038100000000     C                                      indent1 +
038200000000     C                              '<DTR>'
038300000000     C                   EXSR      WrtXML
038400000000     C*
038500000000     C* 3.  CessionarioCommittenteDTR
038600000000     C                   Eval      DsOutput     =
038700000000     C                                      indent2 +
038800000000     C                              '<CessionarioCommittenteDTR>'
038900000000     C                   EXSR      WrtXML
039000000000     C                   endsl
039100000000     C*
039200000000     C* Frontespizio
039300000000     C                   EXSR      WrtFrontesp
039400000000     C*
039500000000     C* Chiusura Tag
039600000000     C* CedentePrestatoreDTE / CessionarioCommittenteDTR
039700000000     C                   select
039800000000     C                   when      $Cf   = 'C'
039900000000     C*
040000000000     C* 2.  /CedentePrestatoreDTE
040100000000     C                   Eval      DsOutput     =
040200000000     C                                      indent2 +
040300000000     C                              '</CedentePrestatoreDTE>'
040400000000     C                   EXSR      WrtXML
040500000000     C*
040600000000     C                   when      $Cf   = 'F'
040700000000     C*
040800000000     C* 3.  /CessionarioCommittenteDTR
040900000000     C                   Eval      DsOutput     =
041000000000     C                                      indent2 +
041100000000     C                              '</CessionarioCommittenteDTR>'
041200000000     C                   EXSR      WrtXML
041300000000     C                   endsl
041400000000     C                   endif
041500000000     C*
041600000000     C                   ENDSR
041700000000     C/EJECT
041800000000     C******************************************************
041900000000     C* Scrivi TAG FRONTESPIZIO
042000000000     C******************************************************
042100000000     C     WrtFrontesp   BEGSR
042200000000     C*
042300000000     C* 2.1.1 <IdentificativiFiscali>
042400000000     C                   Eval      DsOutput     =
042500000000     C                                      indent3 +
042600000000     C                              '<IdentificativiFiscali>'
042700000000     C                   EXSR      WrtXML
042800000000     C*
042900000000     C* 2.1.1.1 <IdFiscaleIVA>
043000000000     C                   Eval      DsOutput     =
043100000000     C                                      indent4  +
043200000000     C                              '<IdFiscaleIVA>'
043300000000     C                   EXSR      WrtXML
043400000000     C*
043500000000     C* 2.1.1.1.1 <IdPaese>  </IdPaese>
043600000000     C                   Eval      DsOutput     =
043700000000     C                                      indent5  +
043800000000     C                              '<IdPaese>' +
043900000000     C                                %trim($IdPaes1) +
044000000000     C                              '</IdPaese>'
044100000000     C                   EXSR      WrtXML
044200000000     C*
044300000000     C* 2.1.1.1.2 <IdCodice>  </IdCodice>
044400000000     C                   Eval      DsOutput     =
044500000000     C                                      indent5  +
044600000000     C                              '<IdCodice>'     +
044700000000     C                                %trim($PIVA1) +
044800000000     C                              '</IdCodice>'
044900000000     C                   EXSR      WrtXML
045000000000     C*
045100000000     C* 2.1.1.1 </IdFiscaleIVA>
045200000000     C                   Eval      DsOutput     =
045300000000     C                                      indent4  +
045400000000     C                              '</IdFiscaleIVA>'
045500000000     C                   EXSR      WrtXML
045600000000     C*
045700000000     C* 2.1.1.2 <CodiceFiscale> (facoltativo)
045800000000     C                   if        $Cdf1 <> *blank
045900000000     C                   Eval      DsOutput     =
046000000000     C                                      indent4  +
046100000000     C                              '<CodiceFiscale>' +
046200000000     C                                %trim($Cdf1) +
046300000000     C                              '</CodiceFiscale>'
046400000000     C                   EXSR      WrtXML
046500000000     C                   endif
046600000000     C*
046700000000     C* 2.1.1 </IdentificativiFiscali>
046800000000     C                   Eval      DsOutput     =
046900000000     C                                      indent3 +
047000000000     C                              '</IdentificativiFiscali>'
047100000000     C                   EXSR      WrtXML
047200000000     C*
047300000000     C* 2.1.2 <AltriDatiIdentificativi>
047400000000     C                   Eval      DsOutput     =
047500000000     C                                      indent3 +
047600000000     C                              '<AltriDatiIdentificativi>'
047700000000     C                   EXSR      WrtXML
047800000000     C*
047900000000     C* 2.1.2.1 <Denominazione>
048000000000     C                   if        $Denom1  <> *blank
048100000000     C                   Eval      DsOutput     =
048200000000     C                                      indent4 +
048300000000     C                              '<Denominazione>' +
048400000000     C                                %trim($Denom1) +
048500000000     C                              '</Denominazione>'
048600000000     C                   EXSR      WrtXML
048700000000     C                   else
048800000000     C* 2.1.2.2 <Nome>
048900000000     C                   if        $Nome1   <> *blank
049000000000     C                   Eval      DsOutput     =
049100000000     C                                      indent4 +
049200000000     C                              '<Nome>' +
049300000000     C                                %trim($Nome1) +
049400000000     C                              '</Nome>'
049500000000     C                   EXSR      WrtXML
049600000000     C* 2.1.2.3 <Cognome>
049700000000     C                   Eval      DsOutput     =
049800000000     C                                      indent4 +
049900000000     C                              '<Cognome>' +
050000000000     C                                %trim($Cognom1) +
050100000000     C                              '</Cognome>'
050200000000     C                   EXSR      WrtXML
050300000000     C                   endif
050400000000     C                   endif
050500000000     C*
050600000000     C* 2.1.2.4 <Sede>
050700000000     C                   Eval      DsOutput     =
050800000000     C                                      indent4 +
050900000000     C                              '<Sede>'
051000000000     C                   EXSR      WrtXML
051100000000     C*
051200000000     C* 2.1.2.4.1 <Indirizzo>
051300000000     C                   Eval      DsOutput     =
051400000000     C                                      indent5 +
051500000000     C                              '<Indirizzo>' +
051600000000     C                                %trim($Indir2) +
051700000000     C                              '</Indirizzo>'
051800000000     C                   EXSR      WrtXML
051900000000     C*
052000000000     C* 2.1.2.4.2 <NumeroCivico>
052100000000     C                   if        $NrCiv2 <> *blank
052200000000     C                   Eval      DsOutput     =
052300000000     C                                      indent5 +
052400000000     C                              '<NumeroCivico>' +
052500000000     C                                %trim($NrCiv2) +
052600000000     C                              '</NumeroCivico>'
052700000000     C                   EXSR      WrtXML
052800000000     C                   endif
052900000000     C*
053000000000     C* 2.1.2.4.3 <CAP>
053100000000     C                   Eval      DsOutput     =
053200000000     C                                      indent5 +
053300000000     C                              '<CAP>' +
053400000000     C                                %trim($Cap2) +
053500000000     C                              '</CAP>'
053600000000     C                   EXSR      WrtXML
053700000000     C*
053800000000     C* 2.1.2.4.4 <Comune>
053900000000     C                   Eval      DsOutput     =
054000000000     C                                      indent5 +
054100000000     C                              '<Comune>' +
054200000000     C                                %trim($Locali2) +
054300000000     C                              '</Comune>'
054400000000     C                   EXSR      WrtXML
054500000000     C*
054600000000     C* 2.1.2.4.5 <Provincia>
054700000000     C                   if        $IdPaes2 = 'IT'
054800000000     C                   Eval      DsOutput     =
054900000000     C                                      indent5 +
055000000000     C                              '<Provincia>' +
055100000000     C                                %trim($Prov2) +
055200000000     C                              '</Provincia>'
055300000000     C                   EXSR      WrtXML
055400000000     C                   endif
055500000000     C*
055600000000     C* 2.1.2.4.6 <Nazione>
055700000000     C                   Eval      DsOutput     =
055800000000     C                                      indent5 +
055900000000     C                              '<Nazione>' +
056000000000     C                                %trim($IdPaes2) +
056100000000     C                              '</Nazione>'
056200000000     C                   EXSR      WrtXML
056300000000     C*
056400000000     C* 2.1.2.4 </Sede>
056500000000     C                   Eval      DsOutput     =
056600000000     C                                      indent4 +
056700000000     C                              '</Sede>'
056800000000     C                   EXSR      WrtXML
056900000000     C*
057000000000     C* 2.1.2.5 <StabileOrganizzazione>
057100000000     C                   if        $Indir3 <> *blank
057200000000     C                   Eval      DsOutput     =
057300000000     C                                      indent4 +
057400000000     C                              '<StabileOrganizzazione>'
057500000000     C                   EXSR      WrtXML
057600000000     C*
057700000000     C* 2.1.2.5.1 <Indirizzo>
057800000000     C                   Eval      DsOutput     =
057900000000     C                                      indent5 +
058000000000     C                              '<Indirizzo>' +
058100000000     C                                %trim($Indir3) +
058200000000     C                              '</Indirizzo>'
058300000000     C                   EXSR      WrtXML
058400000000     C*
058500000000     C* 2.1.2.5.2 <NumeroCivico>
058600000000     C                   if        $NrCiv3 <> *blank
058700000000     C                   Eval      DsOutput     =
058800000000     C                                      indent5 +
058900000000     C                              '<NumeroCivico>' +
059000000000     C                                %trim($NrCiv3) +
059100000000     C                              '</NumeroCivico>'
059200000000     C                   EXSR      WrtXML
059300000000     C                   endif
059400000000     C*
059500000000     C* 2.1.2.5.3 <CAP>
059600000000     C                   Eval      DsOutput     =
059700000000     C                                      indent5 +
059800000000     C                              '<CAP>' +
059900000000     C                                %trim($Cap3) +
060000000000     C                              '</CAP>'
060100000000     C                   EXSR      WrtXML
060200000000     C*
060300000000     C* 2.1.2.5.4 <Comune>
060400000000     C                   Eval      DsOutput     =
060500000000     C                                      indent5 +
060600000000     C                              '<Comune>' +
060700000000     C                                %trim($Locali3) +
060800000000     C                              '</Comune>'
060900000000     C                   EXSR      WrtXML
061000000000     C*
061100000000     C* 2.1.2.5.5 <Provincia>
061200000000     C                   if        $IdPaes3 = 'IT'
061300000000     C                   Eval      DsOutput     =
061400000000     C                                      indent5 +
061500000000     C                              '<Provincia>' +
061600000000     C                                %trim($Prov3) +
061700000000     C                              '</Provincia>'
061800000000     C                   EXSR      WrtXML
061900000000     C                   endif
062000000000     C*
062100000000     C* 2.1.2.4.6 <Nazione>
062200000000     C                   Eval      DsOutput     =
062300000000     C                                      indent5 +
062400000000     C                              '<Nazione>' +
062500000000     C                                %trim($IdPaes3) +
062600000000     C                              '</Nazione>'
062700000000     C                   EXSR      WrtXML
062800000000     C*
062900000000     C* 2.1.2.5 </StabileOrganizzazione>
063000000000     C                   Eval      DsOutput     =
063100000000     C                                      indent4 +
063200000000     C                              '</StabileOrganizzazione>'
063300000000     C                   EXSR      WrtXML
063400000000     C                   endif
063500000000     C*
063600000000     C* 2.1.2.6 <RappresentanteFiscale>
063700000000     C                   if        $IdPaes4    <> *blank
063800000000     C                   Eval      DsOutput     =
063900000000     C                                      indent4 +
064000000000     C                              '<RappresentanteFiscale>'
064100000000     C                   EXSR      WrtXML
064200000000     C*
064300000000     C* 2.1.2.6.1 <IdFiscaleIVA>
064400000000     C                   Eval      DsOutput     =
064500000000     C                                      indent5  +
064600000000     C                              '<IdFiscaleIVA>'
064700000000     C                   EXSR      WrtXML
064800000000     C*
064900000000     C* 2.1.2.6.1 <IdPaese>  </IdPaese>
065000000000     C                   Eval      DsOutput     =
065100000000     C                                      indent6  +
065200000000     C                              '<IdPaese>' +
065300000000     C                                $IdPaes4  +
065400000000     C                              '</IdPaese>'
065500000000     C                   EXSR      WrtXML
065600000000     C*
065700000000     C* 2.1.2.6.2 <IdCodice>  </IdCodice>
065800000000     C                   Eval      DsOutput     =
065900000000     C                                      indent6  +
066000000000     C                              '<IdCodice>'     +
066100000000     C                                %trim($Piva4) +
066200000000     C                              '</IdCodice>'
066300000000     C                   EXSR      WrtXML
066400000000     C*
066500000000     C* 2.1.2.6.1 </IdFiscaleIVA>
066600000000     C                   Eval      DsOutput     =
066700000000     C                                      indent5  +
066800000000     C                              '</IdFiscaleIVA>'
066900000000     C                   EXSR      WrtXML
067000000000     C*
067100000000     C* 2.1.2.6.2 <Denominazione>
067200000000     C                   if        $Denom4  <> *blank
067300000000     C                   Eval      DsOutput     =
067400000000     C                                      indent5 +
067500000000     C                              '<Denominazione>' +
067600000000     C                                %trim($Denom4) +
067700000000     C                              '</Denominazione>'
067800000000     C                   EXSR      WrtXML
067900000000     C                   else
068000000000     C*
068100000000     C* 2.1.2.6.3 <Nome>
068200000000     C                   if        $Nome4         <> *blank
068300000000     C                   Eval      DsOutput     =
068400000000     C                                      indent5 +
068500000000     C                              '<Nome>' +
068600000000     C                                %trim($Nome4) +
068700000000     C                              '</Nome>'
068800000000     C                   EXSR      WrtXML
068900000000     C*
069000000000     C* 2.1.2.6.4 <Cognome>
069100000000     C                   Eval      DsOutput     =
069200000000     C                                      indent5 +
069300000000     C                              '<Cognome>' +
069400000000     C                                %trim($Cognom4) +
069500000000     C                              '</Cognome>'
069600000000     C                   EXSR      WrtXML
069700000000     C                   endif
069800000000     C                   endif
069900000000     C*
070000000000     C* 2.1.2.6 </RappresentanteFiscale>
070100000000     C                   Eval      DsOutput     =
070200000000     C                                      indent4 +
070300000000     C                              '</RappresentanteFiscale>'
070400000000     C                   EXSR      WrtXML
070500000000     C                   endif
070600000000     C*
070700000000     C* 2.1.2 </AltriDatiIdentificativi>
070800000000     C                   Eval      DsOutput     =
070900000000     C                                      indent3 +
071000000000     C                              '</AltriDatiIdentificativi>'
071100000000     C                   EXSR      WrtXML
071200000000     C*
071300000000     C                   ENDSR
071400000000     C/EJECT
071500000000     C******************************************************
071600000000     C* Scrivi TAG FRONTESPIZIO
071700000000     C******************************************************
071800000000     C     CicloSql      BEGSR
071900000000     C*
072000000000     C                   EXSR      RepSqlCST
072100000000     C*
072200000000     C                   EXSR      OpenSqlCST
072300000000     C*
072400000000     C                   EXSR      ExeSqlCST
072500000000     C*
072600000000     C                   EXSR      CloseSql
072700000000     C*
072800000000     C                   ENDSR
072900000000     C/EJECT
073000000000     C************************************************************
073100000000     C* Reperimento stringa sql su SdgCst00f
073200000000     C************************************************************
073300000000     C     RepSqlCST     BEGSR
073400000000     C*
073500000000     C* SELECT rrn(SDGCST00F), sdgcst00f.* FROM SDGCST00F
073600000000     C* WHERE CSTSOCIETA ='XXX'
073700000000     C*  AND CSTTPCOM = 'T'
073800000000     C*  AND CSTTPREC = Tipo Record (O=Ordinari/R=Rettifica)
073900000000     C*  AND CSTAnno = AnnoI61
074000000000     C*  AND CSTTrim = TrimI61
074100000000     C*  AND CSTCF   = Sottonatura
074200000000     C*  [record non annullati]
074300000000     C*  AND CSTANN <> '1'
074400000000     C* -------------------------------------------------------------------
074500000000     C* ORDER BY CSTSOCIETA, CSTTPCOM, CSTCF, CSTANNO, CSTTRIM, CSTMESE,
074600000000     C* CSTIDPAES1, CSTPIVA1, CSTCDFISC1, CSTDENOM1,
074700000000     C* CSTCOGNOM1, CSTNOME1,
074800000000     C* CSTINDIR2, CSTNRCIV2, CSTCAP2, CSTLOCALI2, CSTPROV2, CSTIDPAES2,
074900000000     C* CSTINDIR3, CSTNRCIV3, CSTCAP3, CSTLOCALI3, CSTPROV3, CSTIDPAES3,
075000000000     C* CSTIDPAES4, CSTPIVA4, CSTDENOM4, CSTCOGNOM4, CSTNOME4,
075100000000     C* CSTDTDOC, CSTNRDOC, CSTSERDOC, CSTDTREG, CSTNRREG, CSTSERREG,
075200000000     C* CSTDTANNOT, CSTTPDOC,
075300000000     C* CSTALIQ, CSTRIFIVA, CSTMOTESCL, CSTPERCDET, CSTDEDUCIB, CSTESIGIB
075400000000     C*
075500000000     C*
075600000000     C                   Clear                   SqlCST
075700000000     C*
075800000000     C                   Eval      SqlCST =
075900000000     C                              'SELECT rrn(SDGCST00F), sdgcst00f.* ' +
076000000000     C                              'FROM SDGCST00F ' +
076100000000     C                             ' WHERE CSTSOCIETA=' + '''' +
076200000000     C                             SocietaI61 + ''''
076300000000     C*
076400000000     C                   Eval      SqlCST = %trimr(SqlCST)+
076500000000     C                             ' AND CSTCf     =' + '''' +
076600000000     C                             $Cf    + ''''
076700000000     C*
076800000000     C                   Eval      SqlCST = %trimr(SqlCST)+
076900000000     C                             ' AND CSTAnno   =' +
077000000000     C                             %char(AnnoI61)
077100000000     C*
077200000000     C                   Eval      SqlCST = %trimr(SqlCST)+
077300000000     C                             ' AND CSTTrim   =' +
077400000000     C                             %char(TrimI61)
077500000000     C*
077600000000     C                   Eval      SqlCST = %trimr(SqlCST)+
077700000000     C                             ' AND CSTAnn   <>' + '''1'''
077800000000     C*
077900000000     C                   Select
078000000000     C*
078100000000     C* invio ordinario: tipo record = 'O'
078200000000     C                   When      TipInvI61   = 'O'
078300000000     C                   Eval      SqlCST = %trimr(SqlCST)+
078400000000     C                             ' AND CSTTpRec  =' + '''' +
078500000000     C                             $TpRecOrd + ''''
078600000000     C*
078700000000     C* Rettifica: tipo record = 'R'
078800000000     C                   When      TipInvI61   = 'R'
078900000000     C                   Eval      SqlCST = %trimr(SqlCST)+
079000000000     C                             ' AND CSTTpRec  =' + '''' +
079100000000     C                             $TpRecRet + ''''
079200000000     C*
079300000000     C                   Eval      SqlCST = %trimr(SqlCST)+
079400000000     C                             ' AND CSTIdFileO =' + '''' +
079500000000     C                             IdFileI61 + ''''
079600000000     C*
079700000000     C                   if        PosizI61 <> 0
079800000000     C                   Eval      SqlCST = %trimr(SqlCST)+
079900000000     C                             ' AND CSTIdPosO  =' +
080000000000     C                             %char(PosizI61)
080100000000     C                   endif
080200000000     C*
080300170817     C**** annullamento: seleziono i record di tipo 'O'
080400170817     C****               When      TipInvI61   = 'A'
080500170817     C****               Eval      SqlCST = %trimr(SqlCST)+
080600170817     C****                         ' AND CSTTpRec  =' + '''' +
080700170817     C****                         $TpRecOrd + ''''
080800170817     C****
080900170817     C****               Eval      SqlCST = %trimr(SqlCST)+
081000170817     C****                         ' AND CSTIdFileO =' + '''' +
081100170817     C****                         IdFileI61 + ''''
081200170817     C****
081300170817     C****               if        PosizI61 <> 0
081400170817     C****               Eval      SqlCST = %trimr(SqlCST)+
081500170817     C****                         ' AND CSTIdPosO  =' +
081600170817     C****                         %char(PosizI61)
081700170817     C****               endif
081800000000     C                   endSl
081900000000     C*
082000000000     C                   Eval      SqlCST      = %trimr(SqlCST)+
082100000000     C                               ' ORDER BY '          +
082200000000     C                               ' CSTSOCIETA, CSTTPCOM, CSTCF, ' +
082300000000     C                               ' CSTANNO, CSTTRIM, CSTMESE,   ' +
082400000000     C                               ' CSTIDPAES1, CSTPIVA1, CSTCDFISC1,  ' +
082500000000     C                               ' CSTDENOM1, '                         +
082600000000     C                               ' CSTCOGNOM1, CSTNOME1, '              +
082700000000     C                               ' CSTINDIR2, '                         +
082800000000     C                               ' CSTNRCIV2, CSTCAP2, CSTLOCALI2, '    +
082900000000     C                               ' CSTPROV2, CSTIDPAES2, CSTINDIR3, '   +
083000000000     C                               ' CSTNRCIV3, CSTCAP3, CSTLOCALI3, '    +
083100000000     C                               ' CSTPROV3, CSTIDPAES3, CSTIDPAES4, '  +
083200000000     C                               ' CSTPIVA4, '                          +
083300000000     C                               ' CSTDENOM4, '                         +
083400000000     C                               ' CSTCOGNOM4, CSTNOME4, '              +
083500000000     C                               ' CSTDTDOC, CSTNRDOC, CSTSERDOC, '     +
083600000000     C                               ' CSTDTREG, CSTNRREG, CSTSERREG, '     +
083700000000     C                               ' CSTDTANNOT,'                         +
083800000000     C                               ' CSTTPDOC, '                          +
083900000000     C                               ' CSTALIQ, CSTRIFIVA, CSTMOTESCL, '    +
084000000000     C                               ' CSTPERCDET, CSTDEDUCIB, CSTESIGIB'
084100000000     C*
084200000000     C* For Fetch Only
084300000000     C                   Eval      SqlCST      = %trimr(SqlCST)+
084400000000     C                               ' FOR FETCH ONLY'
084500000000     C*
084600000000     C                   ENDSR
084700000000     C/EJECT
084800000000     C******************************************************
084900000000     C* Open cursore sql
085000000000     C******************************************************
085100000000     C     OpenSqlCST    BEGSR
085200000000     C*
085300000000     C/EXEC SQL
085400000000     C+     PREPARE S1 FROM :SqlCST
085500000000     C/END-EXEC
085600000000     C*
085700000000     C/EXEC SQL
085800000000     C+     Declare A1 cursor for S1
085900000000     C/END-EXEC
086000000000     C*
086100000000     C/EXEC SQL
086200000000     C+     OPEN A1
086300000000     C/END-EXEC
086400000000     C*
086500000000     C                   ENDSR
086600000000     C/EJECT
086700000000     C************************************************************
086800000000     C* Eseguo lettura dei movimenti
086900000000     C************************************************************
087000000000     C     ExeSqlCST     BEGSR
087100000000     C*
087200000000     C                   Move      *Off          $FineCST
087300000000     C                   eval      RigaMod  = 0
087400000000     C                   clear                   $PosizO
087500170812     C                   z-add     0             $MaxRighe
087600000000     C                   z-add     0             $MaxNrDoc
087700000000     C                   clear                   w_ImpOpe
087800000000     C                   clear                   w_IvaOpe
087900000000     C*
088000000000     C                   eval      $RotturaCF  = *off
088100000000     C                   eval      $RotturaDoc = *off
088200170817     C                   eval      $Errore     = *off
088300000000     C*
088400000000     C                   DoW       $FineCST    = *Off
088500000000     C*
088600000000     C                   EXSR      FetchCST
088700000000     C*
088800000000     C                   Select
088900000000     C*-----------------------
089000000000     C* OK
089100000000     C                   When      SqlCod     >= 0     and
089200000000     C                             SqlCod     <> 100
089300000000     C*
089400000000     C                   if        IdFileI61  =  *blank  and
089500000000     C                             PosizI61   =  0       and
089600000000     C                             (CstIdFileO <> *blank or
089700000000     C                              CstIdFileR <> *blank or
089800000000     C                              CstIdFileA <> *blank)
089900000000     C                   iter
090000000000     C                   endif
090100000000     C*
090200000000     C                   eval      RigaMod  = RigaMod + 1
090300000000     C*
090400000000     C* Scrivo TAG Intestazione e Frontespizio
090500000000     C                   if        RigaMod     = 1
090600000000     C                   eval      SavCstDs    = SdgCstDs
090700000000     C                   exsr      WrtIntestaz
090800000000     C                   endif
090900000000     C*
091000000000     C* Dati anagrafici cliente/fornitore
091100000000     C                   exsr      DatiAnaCF
091200000000     C*
091300000000     C* Intestazione dati fattura
091400000000     C                   if        $RotturaCF  = *off  or
091500000000     C                             RigaMod     = 1
091600000000     C                   exsr      DatiFattura
091700000000     C                   endif
091800000000     C*
091900000000     C* Dettaglio dati fattura
092000000000     C                   exsr      ElabCST
092100000000     C*
092200000000     C                   exsr      UpdateCst
092300000000     C*
092400000000     C                   eval      Sv1CstDs  = SdgCstDs
092500000000     C                   eval      Sv2CstDs  = SdgCstDs
092501170828R264 C                   eval      Sv3CstDs  = SdgCstDs
092600000000     C*-----------------------
092700000000     C* fine lettura
092800000000     C                   When      SqlCod      = 100
092900000000     C*
093000000000     C                   if        RigaMod     >= 1
093100000000     C                             or $RotturaDoc = *on
093200000000     C*
093300000000     C                   exsr      UpdateCst
093400000000     C*
093500000000     C* Chiude Tag DatiRiepilogo
093501170828R264 C                   exsr      AccorpaDati
093600000000     C                   exsr      DatiRiepilogo
093700000000     C* Chiude Tag Dati fattura
093800000000     C                   exsr      CloseTagFat
093900000000     C* Chiude Tag Dati anagrafici
094000000000     C                   exsr      CloseTagAna
094100000000     C* Eventuale tag di rettifica
094200000000     C                   exsr      DatiRettifica
094300000000     C* Chiude Tag DTE/DTR
094400000000     C                   exsr      CloseTagDT
094500000000     C* Chiude File
094600000000     C                   Eval      DsOutput     =
094700000000     C                             '</ns2:DatiFattura>'
094800000000     C                   EXSR      WrtXML
094900000000     C                   endif
095000000000     C*
095100000000     C                   Move      *On           $FineCST
095200000000     C*
095300000000     C                   Other
095400000000     C*
095500000000     C*-----------------------
095600000000     C* si è verificato un errore
095700000000     C***                Eval      $TpErr1     = '99'
095800000000     C***                Eval      StringaSql  = SqlCST
095900000000     C***                EXSR      StpErr1
096000000000     C                   Eval      ERRI61      = '1'
096100000000     C*
096200000000     C                   Move      *On           $FineCST
096300000000     C                   endsl
096400000000     C*
096500000000     C                   endDo
096600000000     C*
096700000000     C                   ENDSR
096800000000     C/EJECT
096900000000     C******************************************************
097000000000     C* Eseguo lettura
097100000000     C******************************************************
097200000000     C     FetchCST      BEGSR
097300000000     C*
097400000000     C/EXEC SQL
097500000000     C+     FETCH A1 INTO :RRNCst, :SdgCstDs
097600000000     C/END-EXEC
097700000000     C*
097800000000     C                   ENDSR
097900000000     C/EJECT
098000000000     C********************************************************************
098100000000     C* Dati anagrafici cliente/fornitore
098200000000     C********************************************************************
098300000000     C     DatiAnaCF     BEGSR
098400000000     C*
098500000000     C                   if        CstSOCIETA <> Sv1SOCIETA or
098600000000     C                             CstTPCOM   <> Sv1TPCOM   or
098700000000     C                             CstCF      <> Sv1CF      or
098800000000     C                             CstANNO    <> Sv1ANNO    or
098900000000     C                             CstTRIM    <> Sv1TRIM    or
099000000000     C                             CstMESE    <> Sv1MESE    or
099100000000     C                             CstIDPAES1 <> Sv1IDPAES1 or
099200000000     C                             CstPIVA1   <> Sv1PIVA1   or
099300000000     C                             CstCDFISC1 <> Sv1CDFISC1 or
099400000000     C                             CstDENOM1  <> Sv1DENOM1  or
099500000000     C                             CstCOGNOM1 <> Sv1COGNOM1 or
099600000000     C                             CstNOME1   <> Sv1NOME1   or
099700000000     C                             CstINDIR2  <> Sv1INDIR2  or
099800000000     C                             CstNRCIV2  <> Sv1NRCIV2  or
099900000000     C                             CstCAP2    <> Sv1CAP2    or
100000000000     C                             CstLOCALI2 <> Sv1LOCALI2 or
100100000000     C                             CstPROV2   <> Sv1PROV2   or
100200000000     C                             CstIDPAES2 <> Sv1IDPAES2 or
100300000000     C                             CstINDIR3  <> Sv1INDIR3  or
100400000000     C                             CstNRCIV3  <> Sv1NRCIV3  or
100500000000     C                             CstCAP3    <> Sv1CAP3    or
100600000000     C                             CstLOCALI3 <> Sv1LOCALI3 or
100700000000     C                             CstPROV3   <> Sv1PROV3   or
100800000000     C                             CstIDPAES3 <> Sv1IDPAES3 or
100900000000     C                             CstIDPAES4 <> Sv1IDPAES4 or
101000000000     C                             CstPIVA4   <> Sv1PIVA4   or
101100000000     C                             CstDENOM4  <> Sv1DENOM4  or
101200000000     C                             CstCOGNOM4 <> Sv1COGNOM4 or
101300000000     C                             CstNOME4   <> Sv1NOME4
101400000000     C*
101500000000     C                   eval      $RotturaCF  = *on
101600000000     C*
101700000000     C                   z-add     0             $MaxNrDoc
101800000000     C                   eval      $MaxRighe = $MaxRighe  + 1
101900000000     C*
102000000000     C                   if        RigaMod > 1
102100000000     C*
102101170828R264 C                   exsr      AccorpaDati
102200000000     C                   exsr      DatiRiepilogo
102300000000     C*
102400000000     C                   exsr      DatiFattura
102500000000     C*
102600000000     C* Chiude Tag Dati anagrafici
102700000000     C                   exsr      CloseTagAna
102800000000     C* Eventuale tag di rettifica
102900000000     C                   exsr      DatiRettifica
103000000000     C*
103100000000     C                   endif
103200000000     C*
103300000000     C* Superato il numero massimo di clienti per singolo file,
103400000000     C* chiudo file, copio in cartella IFS, stacco nuovo progressivo
103500000000     C                   if        $MaxRighe >  MaxRecI61
103600000000     C                             and RigaMod > 1
103700000000     C                   Exsr      GesMultInv
103800000000     C                   Eval      $MultiInvio  = *On
103900000000     C                   endif
104000000000     C*
104100000000     C* Apre   Tag Dati anagrafici
104200000000     C                   exsr      OpenTagAna
104300000000     C*
104400000000     C                   eval      RigaMod  = 1
104500000000     C*
104600000000     C                   else
104700000000     C                   eval      $RotturaCF  = *off
104800000000     C                   endif
104900000000     C*
105000000000     C                   ENDSR
105100000000     C/EJECT
105200000000     C******************************************************
105300000000     C* Apertura TAG dati anagrafici cli/for
105400000000     C******************************************************
105500000000     C     OpenTagAna    BEGSR
105600000000     C*
105700000000     C                   select
105800000000     C                   when      $Cf   = 'C'
105900000000     C*
106000000000     C* 2.2 <CessionarioCommittenteDTE>
106100000000     C                   Eval      DsOutput     =
106200000000     C                                      indent2 +
106300000000     C                              '<CessionarioCommittenteDTE>'
106400000000     C                   EXSR      WrtXML
106500000000     C*
106600000000     C                   when      $Cf   = 'F'
106700000000     C*
106800000000     C* 3.2 CedentePrestatoreDTR
106900000000     C                   Eval      DsOutput     =
107000000000     C                                      indent2 +
107100000000     C                              '<CedentePrestatoreDTR>'
107200000000     C                   EXSR      WrtXML
107300000000     C                   endsl
107400000000     C*
107500000000     C* Dettaglio dati anagrafici cli/for
107600000000     C                   EXSR      DettAnaCF
107700000000     C*
107800000000     C                   ENDSR
107900000000     C/EJECT
108000000000     C******************************************************
108100000000     C* Dettaglio dati anagrafici cli/for
108200000000     C******************************************************
108300000000     C     DettAnaCF     BEGSR
108400000000     C*
108500000000     C* 2.2.1 <IdentificativiFiscali>
108600000000     C* 3.2.1 <IdentificativiFiscali>
108700000000     C                   Eval      DsOutput     =
108800000000     C                                      indent3 +
108900000000     C                              '<IdentificativiFiscali>'
109000000000     C                   EXSR      WrtXML
109100000000     C*
109200000000     C*  2.2.1.1 <IdFiscaleIVA>
109300000000     C*  3.2.1.1 <IdFiscaleIVA>
109400000000     C*   - in alternativa al codice fiscale
109401170828R264 X****               if        CstPiva1 <> *blank
109600000000     C                   Eval      DsOutput     =
109700000000     C                                      indent4  +
109800000000     C                              '<IdFiscaleIVA>'
109900000000     C                   EXSR      WrtXML
110000000000     C*
110100000000     C*   2.2.1.1.1 <IdPaese>  </IdPaese>
110200000000     C*   3.2.1.1.1 <IdPaese>  </IdPaese>
110300000000     C                   Eval      DsOutput     =
110400000000     C                                      indent5  +
110500000000     C                              '<IdPaese>' +
110600000000     C                                %trim(CstIdPaes1) +
110700000000     C                              '</IdPaese>'
110800000000     C                   EXSR      WrtXML
110900000000     C*
111000000000     C*   2.2.1.1.2 <IdCodice>  </IdCodice>
111100000000     C*   3.2.1.1.2 <IdCodice>  </IdCodice>
111101170828R264 C                   if        CstPiva1 <> *blank
111200000000     C                   Eval      DsOutput     =
111300000000     C                                      indent5  +
111400000000     C                              '<IdCodice>'     +
111500000000     C                                %trim(CstPIva1) +
111600000000     C                              '</IdCodice>'
111700000000     C                   EXSR      WrtXML
111701170828R264 C                   else
111702170828 "   C* In mancanza della partita iva, è necessaria la
111703170828 "   C* presenza del tag IdCodice con almeno un carattere
111704170828 "   C* blank, pena il rifiuto del file.
111705170828 "   C                   Eval      DsOutput     =
111706170828 "   C                                      indent5  +
111707170828 "   C                              '<IdCodice>'     +
111708170828 "   C                                ' '             +
111709170828 "   C                              '</IdCodice>'
111710170828 "   C                   EXSR      WrtXML
111711170828R264 C                   endif
111800000000     C*
111900000000     C*  2.2.1.1 </IdFiscaleIVA>
112000000000     C*  3.2.1.1 </IdFiscaleIVA>
112100000000     C                   Eval      DsOutput     =
112200000000     C                                      indent4  +
112300000000     C                              '</IdFiscaleIVA>'
112400000000     C                   EXSR      WrtXML
112500000000     C*
112501170828R264 X*****              else
112700000000     C*  2.2.1.2 <CodiceFiscale> (facoltativo)
112800000000     C*  3.2.1.2 <CodiceFiscale> (facoltativo)
112900000000     C                   if        CstCdFisc1 <> *blank
113000000000     C                   Eval      DsOutput     =
113100000000     C                                      indent4  +
113200000000     C                              '<CodiceFiscale>' +
113300000000     C                                %trim(CstCdFisc1) +
113400000000     C                              '</CodiceFiscale>'
113500000000     C                   EXSR      WrtXML
113600000000     C                   endif
113601170828R264 X*****              endif
113800000000     C*
113900000000     C* 2.2.1 </IdentificativiFiscali>
114000000000     C* 3.2.1 </IdentificativiFiscali>
114100000000     C                   Eval      DsOutput     =
114200000000     C                                      indent3 +
114300000000     C                              '</IdentificativiFiscali>'
114400000000     C                   EXSR      WrtXML
114500000000     C*
114600000000     C* 2.2.2 <AltriDatiIdentificativi>
114700000000     C* 3.2.2 <AltriDatiIdentificativi>
114800000000     C                   Eval      DsOutput     =
114900000000     C                                      indent3 +
115000000000     C                              '<AltriDatiIdentificativi>'
115100000000     C                   EXSR      WrtXML
115200000000     C*
115300000000     C*  2.2.2.1 <Denominazione>
115400000000     C*  3.2.2.1 <Denominazione>
115500000000     C                   if        CstDenom1 <> *blank
115600000000     C                   Eval      DsOutput     =
115700000000     C                                      indent4 +
115800000000     C                              '<Denominazione>' +
115900000000     C                                %trim(CstDenom1) +
116000000000     C                              '</Denominazione>'
116100000000     C                   EXSR      WrtXML
116200000000     C                   else
116300000000     C*  2.2.2.2 <Nome>
116400000000     C*  3.2.2.2 <Nome>
116500000000     C                   if        CstNome1       <> *blank
116600000000     C                   Eval      DsOutput     =
116700000000     C                                      indent4 +
116800000000     C                              '<Nome>' +
116900000000     C                                %trim(CstNome1) +
117000000000     C                              '</Nome>'
117100000000     C                   EXSR      WrtXML
117200000000     C*  2.2.2.3 <Cognome>
117300000000     C*  3.2.2.3 <Cognome>
117400000000     C                   Eval      DsOutput     =
117500000000     C                                      indent4 +
117600000000     C                              '<Cognome>' +
117700000000     C                                %trim(CstCognom1) +
117800000000     C                              '</Cognome>'
117900000000     C                   EXSR      WrtXML
118000000000     C                   endif
118100000000     C                   endif
118200000000     C*
118300000000     C*  2.2.2.4 <Sede>
118400000000     C*  3.2.2.4 <Sede>
118500000000     C                   Eval      DsOutput     =
118600000000     C                                      indent4 +
118700000000     C                              '<Sede>'
118800000000     C                   EXSR      WrtXML
118900000000     C*
119000000000     C*   2.2.2.4.1 <Indirizzo>
119100000000     C*   3.2.2.4.1 <Indirizzo>
119200000000     C                   Eval      DsOutput     =
119300000000     C                                      indent5 +
119400000000     C                              '<Indirizzo>' +
119500000000     C                                %trim(CstIndir2) +
119600000000     C                              '</Indirizzo>'
119700000000     C                   EXSR      WrtXML
119800000000     C*
119900000000     C*   2.2.2.4.2 <NumeroCivico>
120000000000     C*   3.2.2.4.2 <NumeroCivico>
120100000000     C                   if        CstNrCiv2 <> *blank
120200000000     C                   Eval      DsOutput     =
120300000000     C                                      indent5 +
120400000000     C                              '<NumeroCivico>' +
120500000000     C                                %trim(CstNrCiv2) +
120600000000     C                              '</NumeroCivico>'
120700000000     C                   EXSR      WrtXML
120800000000     C                   endif
120900000000     C*
121000000000     C*   2.2.2.4.3 <CAP>
121100000000     C*   3.2.2.4.3 <CAP>
121200000000     C                   if        CstCap2 <> *blank
121201170828R264 C                             and CstIdPaes2 = 'IT'
121300000000     C                   Eval      DsOutput     =
121400000000     C                                      indent5 +
121500000000     C                              '<CAP>' +
121600000000     C                                %trim(CstCap2) +
121700000000     C                              '</CAP>'
121800000000     C                   EXSR      WrtXML
121900000000     C                   endif
122000000000     C*
122100000000     C*   2.2.2.4.4 <Comune>
122200000000     C*   3.2.2.4.4 <Comune>
122300000000     C                   Eval      DsOutput     =
122400000000     C                                      indent5 +
122500000000     C                              '<Comune>' +
122600000000     C                                %trim(CstLocali2) +
122700000000     C                              '</Comune>'
122800000000     C                   EXSR      WrtXML
122900000000     C*
123000000000     C*   2.2.2.4.5 <Provincia>
123100000000     C*   3.2.2.4.5 <Provincia>
123200000000     C                   if        CstIdPaes2 = 'IT'
123300000000     C                   Eval      DsOutput     =
123400000000     C                                      indent5 +
123500000000     C                              '<Provincia>' +
123600000000     C                                %trim(CstProv2) +
123700000000     C                              '</Provincia>'
123800000000     C                   EXSR      WrtXML
123900000000     C                   endif
124000000000     C*
124100000000     C*   2.2.2.4.6 <Nazione>
124200000000     C*   3.2.2.4.6 <Nazione>
124300000000     C                   Eval      DsOutput     =
124400000000     C                                      indent5 +
124500000000     C                              '<Nazione>' +
124600000000     C                                %trim(CstIdPaes2) +
124700000000     C                              '</Nazione>'
124800000000     C                   EXSR      WrtXML
124900000000     C*
125000000000     C*  2.2.2.4 </Sede>
125100000000     C*  3.2.2.4 </Sede>
125200000000     C                   Eval      DsOutput     =
125300000000     C                                      indent4 +
125400000000     C                              '</Sede>'
125500000000     C                   EXSR      WrtXML
125600000000     C*
125700000000     C*  2.2.2.5 <StabileOrganizzazione>
125800000000     C*  3.2.2.5 <StabileOrganizzazione>
125900000000     C                   if        CstIndir3 <> *blank
126000000000     C                   Eval      DsOutput     =
126100000000     C                                      indent4 +
126200000000     C                              '<StabileOrganizzazione>'
126300000000     C                   EXSR      WrtXML
126400000000     C*
126500000000     C*  2.2.2.5.1 <Indirizzo>
126600000000     C*  3.2.2.5.1 <Indirizzo>
126700000000     C                   Eval      DsOutput     =
126800000000     C                                      indent5 +
126900000000     C                              '<Indirizzo>' +
127000000000     C                                %trim(CstIndir3) +
127100000000     C                              '</Indirizzo>'
127200000000     C                   EXSR      WrtXML
127300000000     C*
127400000000     C*  2.2.2.5.2 <NumeroCivico>
127500000000     C*  3.2.2.5.2 <NumeroCivico>
127600000000     C                   if        CstNrCiv3 <> *blank
127700000000     C                   Eval      DsOutput     =
127800000000     C                                      indent5 +
127900000000     C                              '<NumeroCivico>' +
128000000000     C                                %trim(CstNrCiv3) +
128100000000     C                              '</NumeroCivico>'
128200000000     C                   EXSR      WrtXML
128300000000     C                   endif
128400000000     C*
128500000000     C*  2.2.2.5.3 <CAP>
128600000000     C*  3.2.2.5.3 <CAP>
128700000000     C                   Eval      DsOutput     =
128800000000     C                                      indent5 +
128900000000     C                              '<CAP>' +
129000000000     C                                %trim(CstCap3) +
129100000000     C                              '</CAP>'
129200000000     C                   EXSR      WrtXML
129300000000     C*
129400000000     C*  2.2.2.5.4 <Comune>
129500000000     C*  3.2.2.5.4 <Comune>
129600000000     C                   Eval      DsOutput     =
129700000000     C                                      indent5 +
129800000000     C                              '<Comune>' +
129900000000     C                                %trim(CstLocali3) +
130000000000     C                              '</Comune>'
130100000000     C                   EXSR      WrtXML
130200000000     C*
130300000000     C*  2.2.2.5.5 <Provincia>
130400000000     C*  3.2.2.5.5 <Provincia>
130500000000     C                   if        CstIdPaes3 = 'IT'
130600000000     C                   Eval      DsOutput     =
130700000000     C                                      indent5 +
130800000000     C                              '<Provincia>' +
130900000000     C                                %trim(CstProv3) +
131000000000     C                              '</Provincia>'
131100000000     C                   EXSR      WrtXML
131200000000     C                   endif
131300000000     C*
131400000000     C*  2.2.2.4.6 <Nazione>
131500000000     C*  3.2.2.4.6 <Nazione>
131600000000     C                   Eval      DsOutput     =
131700000000     C                                      indent5 +
131800000000     C                              '<Nazione>' +
131900000000     C                                %trim(CstIdPaes3) +
132000000000     C                              '</Nazione>'
132100000000     C                   EXSR      WrtXML
132200000000     C*
132300000000     C*  2.2.2.5 </StabileOrganizzazione>
132400000000     C*  3.2.2.5 </StabileOrganizzazione>
132500000000     C                   Eval      DsOutput     =
132600000000     C                                      indent4 +
132700000000     C                              '</StabileOrganizzazione>'
132800000000     C                   EXSR      WrtXML
132900000000     C                   endif
133000000000     C*
133100000000     C*  2.2.2.6 <RappresentanteFiscale>
133200000000     C*  3.2.2.6 <RappresentanteFiscale>
133300000000     C                   if        CstPiva4    <> *blank
133400000000     C                   Eval      DsOutput     =
133500000000     C                                      indent4 +
133600000000     C                              '<RappresentanteFiscale>'
133700000000     C                   EXSR      WrtXML
133800000000     C*
133900000000     C*   2.2.2.6.1 <IdFiscaleIVA>
134000000000     C*   3.2.2.6.1 <IdFiscaleIVA>
134100000000     C                   Eval      DsOutput     =
134200000000     C                                      indent5  +
134300000000     C                              '<IdFiscaleIVA>'
134400000000     C                   EXSR      WrtXML
134500000000     C*
134600000000     C*    2.2.2.6.1 <IdPaese>  </IdPaese>
134700000000     C*    3.2.2.6.1 <IdPaese>  </IdPaese>
134800000000     C                   Eval      DsOutput     =
134900000000     C                                      indent6  +
135000000000     C                              '<IdPaese>' +
135100000000     C                                %trim(CstIdPaes4) +
135200000000     C                              '</IdPaese>'
135300000000     C                   EXSR      WrtXML
135400000000     C*
135500000000     C*    2.2.2.6.2 <IdCodice>  </IdCodice>
135600000000     C*    3.2.2.6.2 <IdCodice>  </IdCodice>
135700000000     C                   Eval      DsOutput     =
135800000000     C                                      indent6  +
135900000000     C                              '<IdCodice>'     +
136000000000     C                                %trim(CstPIva4) +
136100000000     C                              '</IdCodice>'
136200000000     C                   EXSR      WrtXML
136300000000     C*
136400000000     C*  2.2.2.6.1 </IdFiscaleIVA>
136500000000     C*  3.2.2.6.1 </IdFiscaleIVA>
136600000000     C                   Eval      DsOutput     =
136700000000     C                                      indent5  +
136800000000     C                              '</IdFiscaleIVA>'
136900000000     C                   EXSR      WrtXML
137000000000     C*
137100000000     C*  2.2.2.6.2 <Denominazione>
137200000000     C*  3.2.2.6.2 <Denominazione>
137300000000     C                   if        CstDenom4  <> *blank
137400000000     C                   Eval      DsOutput     =
137500000000     C                                      indent5 +
137600000000     C                              '<Denominazione>' +
137700000000     C                                %trim(CstDenom4) +
137800000000     C                              '</Denominazione>'
137900000000     C                   EXSR      WrtXML
138000000000     C                   else
138100000000     C*
138200000000     C*  2.2.2.6.3 <Nome>
138300000000     C*  3.2.2.6.3 <Nome>
138400000000     C                   if        CstNome4     <> *blank
138500000000     C                   Eval      DsOutput     =
138600000000     C                                      indent5 +
138700000000     C                              '<Nome>' +
138800000000     C                                %trim(CstNome4) +
138900000000     C                              '</Nome>'
139000000000     C                   EXSR      WrtXML
139100000000     C*
139200000000     C*  2.2.2.6.4 <Cognome>
139300000000     C*  3.2.2.6.4 <Cognome>
139400000000     C                   Eval      DsOutput     =
139500000000     C                                      indent5 +
139600000000     C                              '<Cognome>' +
139700000000     C                                %trim(CstCognom4) +
139800000000     C                              '</Cognome>'
139900000000     C                   EXSR      WrtXML
140000000000     C                   endif
140100000000     C                   endif
140200000000     C*
140300000000     C*  2.2.2.6 </RappresentanteFiscale>
140400000000     C*  3.2.2.6 </RappresentanteFiscale>
140500000000     C                   Eval      DsOutput     =
140600000000     C                                      indent4 +
140700000000     C                              '</RappresentanteFiscale>'
140800000000     C                   EXSR      WrtXML
140900000000     C                   endif
141000000000     C*
141100000000     C* 2.2.2 </AltriDatiIdentificativi>
141200000000     C* 3.2.2 </AltriDatiIdentificativi>
141300000000     C                   Eval      DsOutput     =
141400000000     C                                      indent3 +
141500000000     C                              '</AltriDatiIdentificativi>'
141600000000     C                   EXSR      WrtXML
141700000000     C*
141800000000     C                   ENDSR
141900000000     C/EJECT
142000000000     C******************************************************
142100000000     C* Chiusura TAG dati anagrafici cli/for
142200000000     C******************************************************
142300000000     C     CloseTagAna   BEGSR
142400000000     C*
142500000000     C                   select
142600000000     C                   when      $Cf   = 'C'
142700000000     C*
142800000000     C* 2.2 </CessionarioCommittenteDTE>
142900000000     C                   Eval      DsOutput     =
143000000000     C                                      indent2 +
143100000000     C                              '</CessionarioCommittenteDTE>'
143200000000     C                   EXSR      WrtXML
143300000000     C*
143400000000     C                   when      $Cf   = 'F'
143500000000     C*
143600000000     C* 3.2 CedentePrestatoreDTR
143700000000     C                   Eval      DsOutput     =
143800000000     C                                      indent2 +
143900000000     C                              '</CedentePrestatoreDTR>'
144000000000     C                   EXSR      WrtXML
144100000000     C                   endsl
144200000000     C*
144300000000     C                   ENDSR
144400000000     C/EJECT
144500000000     C********************************************************************
144600000000     C* Intestazione dati fattura
144700000000     C********************************************************************
144800000000     C     DatiFattura   BEGSR
144900000000     C*
145000000000     C                   if        CstSOCIETA <> Sv2SOCIETA or
145100000000     C                             CstTPCOM   <> Sv2TPCOM   or
145200000000     C                             CstCF      <> Sv2CF      or
145300000000     C                             CstANNO    <> Sv2ANNO    or
145400000000     C                             CstTRIM    <> Sv2TRIM    or
145500000000     C                             CstMESE    <> Sv2MESE    or
145600000000     C                             CstIDPAES1 <> Sv2IDPAES1 or
145700000000     C                             CstPIVA1   <> Sv2PIVA1   or
145800000000     C                             CstCDFISC1 <> Sv2CDFISC1 or
145900000000     C                             CstDENOM1  <> Sv2DENOM1  or
146000000000     C                             CstCOGNOM1 <> Sv2COGNOM1 or
146100000000     C                             CstNOME1   <> Sv2NOME1   or
146200000000     C                             CstINDIR2  <> Sv2INDIR2  or
146300000000     C                             CstNRCIV2  <> Sv2NRCIV2  or
146400000000     C                             CstCAP2    <> Sv2CAP2    or
146500000000     C                             CstLOCALI2 <> Sv2LOCALI2 or
146600000000     C                             CstPROV2   <> Sv2PROV2   or
146700000000     C                             CstIDPAES2 <> Sv2IDPAES2 or
146800000000     C                             CstINDIR3  <> Sv2INDIR3  or
146900000000     C                             CstNRCIV3  <> Sv2NRCIV3  or
147000000000     C                             CstCAP3    <> Sv2CAP3    or
147100000000     C                             CstLOCALI3 <> Sv2LOCALI3 or
147200000000     C                             CstPROV3   <> Sv2PROV3   or
147300000000     C                             CstIDPAES3 <> Sv2IDPAES3 or
147400000000     C                             CstIDPAES4 <> Sv2IDPAES4 or
147500000000     C                             CstPIVA4   <> Sv2PIVA4   or
147600000000     C                             CstDENOM4  <> Sv2DENOM4  or
147700000000     C                             CstCOGNOM4 <> Sv2COGNOM4 or
147800000000     C                             CstNOME4   <> Sv2NOME4   or
147900000000     C                             CstDTDOC   <> Sv2DTDOC   or
148000000000     C                             CstNRDOC   <> Sv2NRDOC   or
148100000000     C                             CstSERDOC  <> Sv2SERDOC  or
148200000000     C                             CstDTREG   <> Sv2DTREG   or
148300000000     C                             CstNRREG   <> Sv2NRREG   or
148400000000     C                             CstSERREG  <> Sv2SERREG  or
148401170828     C                             CstDTANNOT <> Sv2DTANNOT
148402170828R264 X***                          CstTPDOC   <> Sv2TPDOC   or
148403170828  "  X***                          CstALIQ    <> Sv2ALIQ    or
148404170828  "  X***                          CstRIFIVA  <> Sv2RIFIVA  or
148405170828  "  X***                          CstMOTESCL <> Sv2MOTESCL or
148406170828  "  X***                          CstPERCDET <> Sv2PERCDET or
148407170828  "  X***                          CstDEDUCIB <> Sv2DEDUCIB or
148408170828R264 X***                          CstESIGIB  <> Sv2ESIGIB
149300000000     C*
149400000000     C                   eval      $RotturaDoc = *on
149500000000     C*
149600000000     C                   if        RigaMod > 1
149700000000     C*
149800000000     C                   if        $RotturaCF  = *off
149801170828R264 C                   exsr      AccorpaDati
149900000000     C                   exsr      DatiRiepilogo
150000000000     C                   endif
150100000000     C*
150200000000     C* Chiude Tag Dati Fattura
150300000000     C                   exsr      CloseTagFat
150400000000     C*
150500000000     C                   clear                   w_ImpOpe
150600000000     C                   clear                   w_IvaOpe
150700000000     C                   endif
150800000000     C*
150900000000     C* Apre   Tag Dati fattura
151000000000     C                   if        $RotturaCF  = *off or
151100000000     C                             RigaMod     = 1
151200000000     C*
151300000000     C                   eval      $MaxNrDoc  = $MaxNrDoc  + 1
151400000000     C*
151500000000     C* Raggiunto il numero massimo di documenti per cliente
151600000000     C                   if        $MaxNrDoc > cMaxNrDoc
151700000000     C                   z-add     1             $MaxNrDoc
151800000000     C* Chiude Tag Dati anagrafici
151900000000     C                   exsr      CloseTagAna
152000000000     C* Apre   Tag Dati anagrafici
152100000000     C                   exsr      OpenTagAna
152200000000     C                   endif
152300000000     C*
152400000000     C                   eval      $PosizO  = $PosizO  + 1
152500000000     C                   exsr      OpenTagFat
152600000000     C                   endif
152700000000     C*
152800000000     C                   else
152900000000     C                   eval      $RotturaDoc = *off
152901170828R264 C                   if        $RotturaCF  = *off
152902170828  "  C                   if        CstTPDOC   <> Sv3TPDOC   or
152903170828  "  C                             CstALIQ    <> Sv3ALIQ    or
152904170828  "  C****                         CstRIFIVA  <> Sv3RIFIVA  or
152905170828  "  C                             CstMOTESCL <> Sv3MOTESCL or
152906170828  "  C                             CstPERCDET <> Sv3PERCDET or
152907170828  "  C                             CstDEDUCIB <> Sv3DEDUCIB or
152908170828  "  C                             CstESIGIB  <> Sv3ESIGIB
152909170828  "  C                   exsr      AccorpaDati
152910170828  "  C                   exsr      DatiRiepilogo
152911170828  "  C                   endif
152912170828R264 C                   endif
153000000000     C                   endif
153100000000     C*
153200000000     C                   ENDSR
153300000000     C/EJECT
153400000000     C******************************************************
153500000000     C* Apertura TAG dati fattura cli/for
153600000000     C******************************************************
153700000000     C     OpenTagFat    BEGSR
153800000000     C*
153900000000     C                   select
154000000000     C                   when      $Cf   = 'C'
154100000000     C*
154200000000     C* 2.2.3 <DatiFatturaBodyDTE>
154300000000     C                   Eval      DsOutput     =
154400000000     C                                      indent3 +
154500000000     C                              '<DatiFatturaBodyDTE>'
154600000000     C                   EXSR      WrtXML
154700000000     C*
154800000000     C                   when      $Cf   = 'F'
154900000000     C*
155000000000     C* 3.2.3 <DatiFatturaBodyDTR>
155100000000     C                   Eval      DsOutput     =
155200000000     C                                      indent3 +
155300000000     C                              '<DatiFatturaBodyDTR>'
155400000000     C                   EXSR      WrtXML
155500000000     C                   endsl
155600000000     C*
155700000000     C* 2.2.3.1  <DatiGenerali>
155800000000     C* 3.2.3.1  <DatiGenerali>
155900000000     C                   Eval      DsOutput     =
156000000000     C                                      indent4 +
156100000000     C                              '<DatiGenerali>'
156200000000     C                   EXSR      WrtXML
156300000000     C*
156400000000     C*  2.2.3.1.1  <TipoDocumento>
156500000000     C*  3.2.3.1.1  <TipoDocumento>
156600000000     C                   Eval      DsOutput     =
156700000000     C                                      indent5 +
156800000000     C                              '<TipoDocumento>'  +
156900000000     C                                %trim(CstTpDoc)  +
157000000000     C                              '</TipoDocumento>'
157100000000     C                   EXSR      WrtXML
157200000000     C*
157300000000     C*  2.2.3.1.2  <Data>
157400000000     C*  3.2.3.1.2  <Data>
157500000000     C                   Eval      DsOutput     =
157600000000     C                                      indent5 +
157700000000     C                              '<Data>'  +
157800000000     C                                %char(CstDtDoc)  +
157900000000     C                              '</Data>'
158000000000     C                   EXSR      WrtXML
158100000000     C*
158200000000     C*  2.2.3.1.3  <Numero>
158300000000     C*  3.2.3.1.3  <Numero>
158400000000     C                   Eval      DsOutput     =
158500000000     C                                      indent5 +
158600000000     C                              '<Numero>' +
158700000000     C                                %trim(%editc(CstNrDoc:'Z'))  +
158800000000     C                              '</Numero>'
158900000000     C                   EXSR      WrtXML
159000000000     C*
159100000000     C                   if        $Cf   = 'F'
159200000000     C*
159300000000     C*  3.2.3.1.4  <DataRegistrazione>
159400000000     C                   Eval      DsOutput     =
159500000000     C                                      indent5 +
159600000000     C                              '<DataRegistrazione>' +
159700000000     C                                %char(CstDtReg)  +
159800000000     C                              '</DataRegistrazione>'
159900000000     C                   EXSR      WrtXML
160000000000     C                   endif
160100000000     C*
160200000000     C* 2.2.3.1  </DatiGenerali>
160300000000     C* 3.2.3.1  </DatiGenerali>
160400000000     C                   Eval      DsOutput     =
160500000000     C                                      indent4 +
160600000000     C                              '</DatiGenerali>'
160700000000     C                   EXSR      WrtXML
160800000000     C*
160900000000     C                   ENDSR
161000000000     C/EJECT
161100000000     C******************************************************
161200000000     C* Chiusura TAG dati fattura
161300000000     C******************************************************
161400000000     C     CloseTagFat   BEGSR
161500000000     C*
161600000000     C                   select
161700000000     C                   when      $Cf   = 'C'
161800000000     C*
161900000000     C* 2.2.3 </DatiFatturaBodyDTE>
162000000000     C                   Eval      DsOutput     =
162100000000     C                                      indent3 +
162200000000     C                              '</DatiFatturaBodyDTE>'
162300000000     C                   EXSR      WrtXML
162400000000     C*
162500000000     C                   when      $Cf   = 'F'
162600000000     C*
162700000000     C* 3.2.3 </DatiFatturaBodyDTR>
162800000000     C                   Eval      DsOutput     =
162900000000     C                                      indent3 +
163000000000     C                              '</DatiFatturaBodyDTR>'
163100000000     C                   EXSR      WrtXML
163200000000     C                   endsl
163300000000     C*
163400000000     C                   ENDSR
163500000000     C/EJECT
163600000000     C********************************************************************
163700000000     C* Elaboro i movimenti
163800000000     C********************************************************************
163900000000     C     ElabCST       BEGSR
164000000000     C*
164100000000     C                   if        CstSOCIETA = SavSOCIETA and
164200000000     C                             CstTPCOM   = SavTPCOM   and
164300000000     C                             CstCF      = SavCF      and
164400000000     C                             CstANNO    = SavANNO    and
164500000000     C                             CstTRIM    = SavTRIM    and
164600000000     C                             CstMESE    = SavMESE    and
164700000000     C                             CstIDPAES1 = SavIDPAES1 and
164800000000     C                             CstPIVA1   = SavPIVA1   and
164900000000     C                             CstCDFISC1 = SavCDFISC1 and
165000000000     C                             CstDENOM1  = SavDENOM1  and
165100000000     C                             CstCOGNOM1 = SavCOGNOM1 and
165200000000     C                             CstNOME1   = SavNOME1   and
165300000000     C                             CstINDIR2  = SavINDIR2  and
165400000000     C                             CstNRCIV2  = SavNRCIV2  and
165500000000     C                             CstCAP2    = SavCAP2    and
165600000000     C                             CstLOCALI2 = SavLOCALI2 and
165700000000     C                             CstPROV2   = SavPROV2   and
165800000000     C                             CstIDPAES2 = SavIDPAES2 and
165900000000     C                             CstINDIR3  = SavINDIR3  and
166000000000     C                             CstNRCIV3  = SavNRCIV3  and
166100000000     C                             CstCAP3    = SavCAP3    and
166200000000     C                             CstLOCALI3 = SavLOCALI3 and
166300000000     C                             CstPROV3   = SavPROV3   and
166400000000     C                             CstIDPAES3 = SavIDPAES3 and
166500000000     C                             CstIDPAES4 = SavIDPAES4 and
166600000000     C                             CstPIVA4   = SavPIVA4   and
166700000000     C                             CstDENOM4  = SavDENOM4  and
166800000000     C                             CstCOGNOM4 = SavCOGNOM4 and
166900000000     C                             CstNOME4   = SavNOME4   and
167000000000     C                             CstDTDOC   = SavDTDOC   and
167100000000     C                             CstNRDOC   = SavNRDOC   and
167200000000     C                             CstSERDOC  = SavSERDOC  and
167300000000     C                             CstDTREG   = SavDTREG   and
167400000000     C                             CstNRREG   = SavNRREG   and
167500000000     C                             CstSERREG  = SavSERREG  and
167600000000     C                             CstDTANNOT = SavDTANNOT and
167700000000     C                             CstTPDOC   = SavTPDOC   and
167800000000     C                             CstALIQ    = SavALIQ    and
167801170828R64  X**                           CstRIFIVA  = SavRIFIVA  and
168000000000     C                             CstMOTESCL = SavMOTESCL and
168100000000     C                             CstPERCDET = SavPERCDET and
168200000000     C                             CstDEDUCIB = SavDEDUCIB and
168300000000     C                             CstESIGIB  = SavESIGIB
168400000000     C*
168401170828R264 X**                 add       CstImpOpe     w_ImpOpe
168402170828R264 X**                 add       CstIvaOpe     w_IvaOpe
168700000000     C*
168800000000     C                   else
168900000000     C*
168901170828R264 X**                 eval      w_ImpOpe   = CstImpOpe
168902170828R264 X**                 eval      w_IvaOpe   = CstIvaOpe
169200000000     C*
169300000000     C                   eval      SavCstDs = SdgCstDs
169301170828R264 C                   eval      Sv3CstDs = SdgCstDs
169400000000     C                   endif
169500000000     C*
169600000000     C                   ENDSR
169700000000     C/EJECT
169800000000     C******************************************************
169900000000     C* Aggiorna posizione fattura
170000000000     C******************************************************
170100000000     C     UpdateCst     BEGSR
170200000000     C*
170300000000     C                   dou       %error = *off
170400000000     C                             or  MsgRtn <> 'R'
170500000000     C     RRNCst        chain(e)  SdgCst00f
170600000000     C* Gestione RCD allocato
170700000000     C                   if        %error
170800000000     C                   movel     'SDGCST00F'   MSGFIL
170900000000     C                   exsr      MSGPGM
171000000000     C                   endif
171100000000     C                   enddo
171200170817     C*
171300170817     C                   if        %error
171400170817     C                   eval      $Errore = *on
171500170817     C                   else
171600170817     C*
171700000000     C                   if        %found(SdgCst00f)
171800000000     C                   select
171900000000     C                   when      TipInvI61   = 'O'
172000000000     C                   eval      Cs0FileO   = FileXml
172100000000     C                   eval      Cs0PosizO  = $PosizO
172200000000     C                   when      TipInvI61   = 'R'
172300000000     C                   eval      Cs0FileR   = FileXml
172400000000     C                   eval      Cs0PosizR  = $PosizO
172500170817     C***                when      TipInvI61   = 'A'
172600170817     C***                eval      Cs0FileA   = FileXml
172700000000     C                   endsl
172800170817     C*
172900170817     C                   eval      CS0DtUlMo   = %date()
173000170817     C                   eval      CS0UteUlMo  = XscCut
173100170817     C*
173200000000     C                   update    SdgCst000
173300000000     C                   endif
173400170817     C*
173500170817     C                   endif
173600000000     C*
173700000000     C                   ENDSR
173800000000     C/EJECT
173900170817     C******************************************************
174000170817     C* Aggiorna posizione fattura in caso di annullamento
174100170817     C******************************************************
174200170817     C     UpdCstAnn     BEGSR
174300170817     C*
174400170817     C                   eval      C10Societa = XscSoc
174500170817     C                   eval      C10IdFileO = IdFileI61
174600170817     C     K02Cst10      setll     SdgCst10l
174700170817     C                   do        *Hival
174800170817     C     K02Cst10      reade(e)  SdgCst10l
174900170817     C                   if        %eof(SdgCst10l)
175000170817     C                   leave
175100170817     C                   endif
175200170817     C*
175300170817     C* Gestione RCD allocato
175400170817     C                   if        %error
175500170817     C                   movel     'SDGCST00F'   MSGFIL
175600170817     C                   exsr      MSGPGM
175700170817     C                   select
175800170817     C                   when      MsgRtn = 'R'
175900170817     C                   iter
176000170817     C                   when      MsgRtn = 'C'
176100170817     C                   eval      $Errore = *on
176200170817     C                   leave
176300170817     C                   endsl
176400170817     C*
176500170817     C                   else
176600170817     C*
176700170817     C                   if         C10TpRec   <> 'O'
176800170817     C                   iter
176900170817     C                   endif
177000170817     C*
177100170817     C                   if         C10IdFileA <> *blank
177200170817     C                   iter
177300170817     C                   endif
177400170817     C*
177500170817     C                   if        PosizI61  <> 0 and
177600170817     C                             C10PosizO <> PosizI61
177700170817     C                   iter
177800170817     C                   endif
177900170817     C*
178000170817     C                   eval      C10FileA   = FileXml
178100170817     C                   eval      C10DtUlMo  = %date()
178200170817     C                   eval      C10UteUlMo = XscCut
178300170817     C                   update    SdgCst010
178400170817     C*
178500170817     C                   endif
178600170817     C                   enddo
178700170817     C*
178800170817     C                   ENDSR
178900170817     C/EJECT
178901170828R264 C******************************************************
178902170828  "  C* Accorpa DATI RIEPILOGO
178903170828R264 C******************************************************
178904170828     C     AccorpaDati   BEGSR
178905170828     C*
178906170828     C                   clear                   w_ImpOpe
178907170828     C                   clear                   w_IvaOpe
178908170828     C                   Eval      C02Societa   = XScSoc
178909170828     C                   Eval      C02TpCom     = Sv3Tpcom
178910170828     C                   Eval      C02TpRec     = Sv3Tprec
178911170828     C                   Eval      C02Anno      = Sv3Anno
178912170828     C                   Eval      C02Trim      = Sv3Trim
178913170828     C                   Eval      C02Mese      = Sv3Mese
178914170828     C*
178915170828     C                   eval      C02CliFor    = Sv3COGRAG1+Sv3CF+Sv3PARTIVA
178916170828     C                                          + Sv3CDFISC+Sv3STATO1+Sv3IDPAES1
178917170828     C                                          + Sv3PIVA1+Sv3CDFISC1+Sv3TPN1
178918170828     C                                          + Sv3DENOM1+Sv3NOME1+Sv3COGNOM1
178919170828     C                                          + Sv3INDIR2+Sv3NRCIV2+Sv3CAP2
178920170828     C                                          + Sv3LOCALI2+Sv3PROV2+Sv3STATO2
178921170828     C                                          + Sv3IDPAES2+Sv3INDIR3+Sv3NRCIV3
178922170828     C                                          + Sv3CAP3+Sv3LOCALI3+Sv3PROV3
178923170828     C                                          + Sv3STATO3+Sv3IDPAES3+Sv3STATO4
178924170828     C                                          + Sv3IDPAES4+Sv3PIVA4+Sv3TPN4
178925170828     C                                          + Sv3COGRAG4+Sv3DENOM4+Sv3NOME4
178926170828     C                                          + Sv3COGNOM4
178927170828     C*
178928170828     C                   eval      C02TPDOC     = Sv3TPDOC
178929170828     C                   eval      C02DTDOC     = Sv3DTDOC
178930170828     C                   eval      C02NRDOC     = Sv3NRDOC
178931170828     C                   eval      C02SERDOC    = Sv3SERDOC
178932170828     C                   eval      C02DTREG     = Sv3DTREG
178933170828     C                   eval      C02NRREG     = Sv3NRREG
178934170828     C                   eval      C02SERREG    = Sv3SERREG
178935170828     C                   eval      C02DTANNOT   = Sv3DTANNOT
178936170828     C                   eval      C02ALIQ      = Sv3ALIQ
178937170828     C                   eval      C02MOTESCL   = Sv3MOTESCL
178938170828     C                   eval      C02PERCDET   = Sv3PERCDET
178939170828     C                   eval      C02DEDUCIB   = Sv3DEDUCIB
178940170828     C                   eval      C02ESIGIB    = Sv3ESIGIB
178941170828     C     K20Cst02      setll     SdgCst02l
178942170828     C                   do        *hival
178943170828     C     K20Cst02      reade     SdgCst02l
178944170828     C                   if        %eof(SdgCst02l)
178945170828     C                   leave
178946170828     C                   endif
178947170828     C*
178948170828     C                   add       C02ImpOpe     w_ImpOpe
178949170828     C                   add       C02IvaOpe     w_IvaOpe
178950170828     C*
178951170828     C                   enddo
178952170828R264 C*
178953170828  "  C                   ENDSR
178954170828R264 C/EJECT
179000000000     C******************************************************
179100000000     C* Scrive TAG DATI RIEPILOGO
179200000000     C******************************************************
179300000000     C     DatiRiepilogo BEGSR
179400000000     C*
179500000000     C* 2.2.3.2  <DatiRiepilogo>
179600000000     C* 3.2.3.2  <DatiRiepilogo>
179700000000     C                   Eval      DsOutput     =
179800000000     C                                      indent4 +
179900000000     C                              '<DatiRiepilogo>'
180000000000     C                   EXSR      WrtXML
180100000000     C*
180200000000     C*  2.2.3.2.1 <ImponibileImporto>
180300000000     C*  3.2.3.2.1 <ImponibileImporto>
180400000000     C*
180500000000     C                   eval      WWImporto = %abs(w_ImpOpe)
180600000000     C                   EXSR      EditaImporto
180700000000     C*
180800000000     C                   Eval      DsOutput     =
180900000000     C                                      indent5 +
181000000000     C                              '<ImponibileImporto>' +
181100000000     C                               %trim($ImportoA) +
181200000000     C                              '</ImponibileImporto>'
181300000000     C                   EXSR      WrtXML
181400000000     C*
181500000000     C*  2.2.3.2.2 <DatiIVA>
181600000000     C*  3.2.3.2.2 <DatiIVA>
181700000000     C                   Eval      DsOutput     =
181800000000     C                                      indent5 +
181900000000     C                              '<DatiIVA>'
182000000000     C                   EXSR      WrtXML
182100000000     C*
182200000000     C*   2.2.3.2.2.1 <Imposta>
182300000000     C*   3.2.3.2.2.1 <Imposta>
182400000000     C*
182500000000     C                   eval      WWImporto = %abs(w_IvaOpe)
182600000000     C                   EXSR      EditaImporto
182700000000     C*
182800000000     C                   Eval      DsOutput     =
182900000000     C                                      indent6 +
183000000000     C                              '<Imposta>' +
183100000000     C                               %trim($ImportoA) +
183200000000     C                              '</Imposta>'
183300000000     C                   EXSR      WrtXML
183400000000     C*
183500000000     C*   2.2.3.2.2.2 <Aliquota>
183600000000     C*   3.2.3.2.2.2 <Aliquota>
183700000000     C*
183800000000     C                   Z-Add     SavAliq       WWImporto
183900000000     C                   EXSR      EditaImporto
184000000000     C*
184100000000     C                   Eval      DsOutput     =
184200000000     C                                      indent6 +
184300000000     C                              '<Aliquota>' +
184400000000     C                               %trim($ImportoA) +
184500000000     C                              '</Aliquota>'
184600000000     C                   EXSR      WrtXML
184700000000     C*
184800000000     C*  2.2.3.2.2 </DatiIVA>
184900000000     C*  3.2.3.2.2 </DatiIVA>
185000000000     C                   Eval      DsOutput     =
185100000000     C                                      indent5 +
185200000000     C                              '</DatiIVA>'
185300000000     C                   EXSR      WrtXML
185400000000     C*
185500000000     C*  2.2.3.2.3 <Natura>
185600000000     C*  3.2.3.2.3 <Natura>
185700000000     C                   if        SavMotEscl <> *blank
185800000000     C                   Eval      DsOutput     =
185900000000     C                                      indent5 +
186000000000     C                              '<Natura>' +
186100000000     C                               %trim(SavMotEscl) +
186200000000     C                              '</Natura>'
186300000000     C                   EXSR      WrtXML
186400000000     C                   endif
186500000000     C*
186600000000     C*  2.2.3.2.4 <Detraibile>
186700000000     C*  3.2.3.2.4 <Detraibile>
186800000000     C                   if        SavPercDet <> 0
186900000000     C*
187000000000     C                   Z-Add     SavPercDet    WWImporto
187100000000     C                   EXSR      EditaImporto
187200000000     C*
187300000000     C                   Eval      DsOutput     =
187400000000     C                                      indent5 +
187500000000     C                              '<Detraibile>' +
187600000000     C                               %trim($ImportoA) +
187700000000     C                              '</Detraibile>'
187800000000     C                   EXSR      WrtXML
187900000000     C                   endif
188000000000     C*
188100000000     C*  2.2.3.2.5 <Deducibile>
188200000000     C*  3.2.3.2.5 <Deducibile>
188300000000     C                   if        SavDeducib = '1'
188400000000     C                   Eval      DsOutput     =
188500000000     C                                      indent5 +
188600000000     C                              '<Deducibile>' +
188700000000     C                               'SI' +
188800000000     C                              '</Deducibile>'
188900000000     C                   EXSR      WrtXML
189000000000     C                   endif
189100000000     C*
189200000000     C*  2.2.3.2.6 <EsigibilitaIVA>
189300000000     C*  3.2.3.2.6 <EsigibilitaIVA>
189400000000     C                   if        SavEsigib <> *blank
189500000000     C                   Eval      DsOutput     =
189600000000     C                                      indent5 +
189700000000     C                              '<EsigibilitaIVA>' +
189800000000     C                               SavEsigib +
189900000000     C                              '</EsigibilitaIVA>'
190000000000     C                   EXSR      WrtXML
190100000000     C                   endif
190200000000     C*
190300000000     C* 2.2.3.2  </DatiRiepilogo>
190400000000     C* 3.2.3.2  </DatiRiepilogo>
190500000000     C                   Eval      DsOutput     =
190600000000     C                                      indent4 +
190700000000     C                              '</DatiRiepilogo>'
190800000000     C                   EXSR      WrtXML
190900000000     C*
191000000000     C                   ENDSR
191100000000     C/EJECT
191200000000     C******************************************************
191300000000     C* Dati rettifica
191400000000     C******************************************************
191500000000     C     DatiRettifica BEGSR
191600000000     C*
191700000000     C*
191800000000     C* 2.3 <Rettifica>
191900000000     C* 3.3 <Rettifica>
192000000000     C                   if        TipInvI61 = 'R'
192100000000     C                   Eval      DsOutput     =
192200000000     C                                      indent2 +
192300000000     C                              '<Rettifica>'
192400000000     C                   EXSR      WrtXML
192500000000     C*
192600000000     C* 2.3.1 IdFile
192700000000     C* 3.3.1 IdFile
192800000000     C                   Eval      DsOutput     =
192900000000     C                                      indent3 +
193000000000     C                              '<IdFile>' +
193100000000     C                                %trim(IdFileI61) +
193200000000     C                              '</IdFile>'
193300000000     C                   EXSR      WrtXML
193400000000     C*
193500000000     C* 2.3.2 Posizione
193600000000     C* 3.3.2 Posizione
193700000000     C                   Eval      DsOutput     =
193800000000     C                                      indent3 +
193900000000     C                              '<Posizione>' +
194000000000     C                                %trim(%editc(CstPosizO:'Z'))  +
194100000000     C                              '</Posizione>'
194200000000     C                   EXSR      WrtXML
194300000000     C*
194400000000     C* 2.3 </Rettifica>
194500000000     C* 3.3 </Rettifica>
194600000000     C                   Eval      DsOutput     =
194700000000     C                                      indent2 +
194800000000     C                              '</Rettifica>'
194900000000     C                   EXSR      WrtXML
195000000000     C                   endif
195100000000     C*
195200000000     C                   ENDSR
195300000000     C/EJECT
195400000000     C******************************************************
195500000000     C* Chiusura TAG DTE/DTR
195600000000     C******************************************************
195700000000     C     CloseTagDT    BEGSR
195800000000     C*
195900000000     C                   select
196000000000     C                   when      $Cf   = 'C'
196100000000     C*
196200000000     C* 2.  /DTE
196300000000     C                   Eval      DsOutput     =
196400000000     C                                      indent1 +
196500000000     C                              '</DTE>'
196600000000     C                   EXSR      WrtXML
196700000000     C*
196800000000     C                   when      $Cf   = 'F'
196900000000     C*
197000000000     C* 3.  /DTR
197100000000     C                   Eval      DsOutput     =
197200000000     C                                      indent1 +
197300000000     C                              '</DTR>'
197400000000     C                   EXSR      WrtXML
197500000000     C                   endsl
197600000000     C*
197700000000     C                   ENDSR
197800000000     C/EJECT
197900000000     C************************************************************
198000000000     C* Editazione di un importo
198100000000     C* Tolgo il punto decimale e metto la virgola decimale
198200000000     C* Non metto il punto delle migliaia
198300000000     C************************************************************
198400000000     C     EditaImporto  BEGSR
198500000000     C*
198600000000     C                   EXSR      AllineaDec
198700000000     C*
198800000000     C                   Eval      $SrcVarDe =   NrDecMC
198900000000     C                   Eval      $LenFld     = %Size($ImportoA)
199000000000     C                   Eval      $EdtCode    = 'Q'
199100000000     C*
199200000000     C                   EXSR      CallEdit
199300000000     C*
199400000000     C                   Eval      $ImportoA   = $RcvrVar
199500000000     C     ',':'.'       xlate     $ImportoA     $ImportoA
199600000000     C*
199700000000     C                   ENDSR
199800000000     C/EJECT
199900000000     C************************************************************
200000000000     C* Editazione del mese
200100000000     C* Tolgo lo 0 non significativo
200200000000     C************************************************************
200300000000     C     EditaMese     BEGSR
200400000000     C*
200500000000     C                   Z-add     CSTMese       $SrcVar
200600000000     C                   Eval      $SrcVarDe =   0
200700000000     C                   Eval      $LenFld     = %Size($MeseA)
200800000000     C                   Eval      $EdtCode    = '4'
200900000000     C*
201000000000     C                   EXSR      CallEdit
201100000000     C*
201200000000     C                   Eval      $MeseA      = $RcvrVar
201300000000     C*
201400000000     C                   ENDSR
201500000000     C/EJECT
201600000000     C************************************************************
201700000000     C* Chiamata al driver per editazione campi numerici
201800000000     C************************************************************
201900000000     C     CallEdit      BEGSR
202000000000     C*
202100000000     C                   CallB     'X23EDITR'
202200000000     C                   Parm                    $SrcVar
202300000000     C                   Parm                    $SrcVarPr
202400000000     C                   Parm                    $SrcVarDe
202500000000     C                   Parm                    $EdtCode
202600000000     C                   Parm                    $LenFld
202700000000     C                   Parm      *Blanks       $RcvrVar
202800000000     C                   Parm                    $Err
202900000000     C*
203000000000     C                   ENDSR
203100000000     C/EJECT
203200000000     C************************************************************
203300000000     C* Allinea i numeri decimali prima della chiamata a X23Editr
203400000000     C************************************************************
203500000000     C     AllineaDec    BEGSR
203600000000     C*
203700000000     C                   Select
203800000000     C                   When      NrDecMC  = 0
203900000000     C                   Z-add     WWImporto     $SrcVar
204000000000     C                   When      NrDecMC  = 1
204100000000     C     WWImporto     Mult      10            $SrcVar
204200000000     C                   When      NrDecMC  = 2
204300000000     C     WWImporto     Mult      100           $SrcVar
204400000000     C                   Other
204500000000     C     WWImporto     Mult      1000          $SrcVar
204600000000     C                   Endsl
204700000000     C*
204800000000     C                   ENDSR
204900000000     C/EJECT
205000000000     C************************************************************
205100000000     C* Creo File Output
205200000000     C************************************************************
205300000000     C     CreoFileO     BEGSR
205400000000     C*
205500000000     C* Cancello il file in qtemp (se esiste)
205600000000     C                   EXSR      DelFileO
205700000000     C*
205800000000     C* Creo file in qtemp per metadata
205900000000     C* Il file creato a pgm serve per poter compilare il pgm che
206000000000     C* si appoggia a un file creato in QTEMP che al termine del
206100000000     C* lavoro sparisce
206200000000     C                   EXSR      CrtFileO
206300000000     C*
206400000000     C* Ovrprtf per file in qtemp
206500000000     C                   EXSR      OvrFileO
206600000000     C*
206700000000     C                   OPEN      FileO
206800000000     C*
206900000000     C* Recupera progressivo invio
207000000000     C                   exsr      TakeNrProg
207100000000     C* Imposta nome file Xml
207200000000     C                   exsr      SetNomeF
207300000000     C*
207400000000     C                   ENDSR
207500000000     C/EJECT
207600000000     C************************************************************
207700000000     C* Creo files in qtemp
207800000000     C************************************************************
207900000000     C     CrtFileO      BEGSR
208000000000     C*
208100000000     C* Creo il metadata in QTEMP
208200000000     C                   Eval      Cmd = 'CRTPF FILE(QTEMP/' +
208300000000     C                                   wFile   + ')'
208400000000     C                              + ' RCDLEN(500)'
208500000000     C                              + ' TEXT(''XML Spesometro Trimestrale'')'
208600170812     C                              + ' SIZE(2000000 1000)'
208700000000     C                   call      'QCMDEXC'
208800000000     C                   Parm                    Cmd             500
208900000000     C                   Parm      500           Len              15 5
209000000000     C*
209100000000     C                   ENDSR
209200000000     C/EJECT
209300000000     C************************************************************
209400000000     C* Ovr file in qtemp
209500000000     C************************************************************
209600000000     C     OvrFileO      BEGSR
209700000000     C*
209800000000     C* File sostituente
209900000000     C                   Eval      Cmd = 'OVRDBF FILE(FILEO) '
210000000000     C                              + 'TOFILE(QTEMP/'
210100000000     C                              + wFile    +')'
210200000000     C                   call      'QCMDEXC'
210300000000     C                   Parm                    Cmd             500
210400000000     C                   Parm      500           Len              15 5
210500000000     C*
210600000000     C                   ENDSR
210700000000     C/EJECT
210800000000     C************************************************************
210900000000     C* Chiudo e copio File Output
211000000000     C************************************************************
211100000000     C     ChiudoFileO   BEGSR
211200000000     C*
211300000000     C                   Close     FileO
211400000000     C*
211500000000     C* Imposta nome file Xml
211600170817     C                   if        $Errore      = *off  and
211700170817     C                             (RigaMod     >= 1   or
211800170817     C                              $RotturaDoc = *on  or
211900170817     C                              TipInvI61    = 'A')
212000000000     C*
212100000000     C* Copio file XML nella directory dell'IFS
212200000000     C                   exsr      CpyToStmF
212300000000     C                   else
212400000000     C                   clear                   $ProgrInt
212500000000     C                   rolbk
212600000000     C                   endif
212700000000     C*
212800000000     C* Cancello il file in qtemp
212900000000     C                   exsr      DelFileO
213000000000     C*
213100000000     C                   ENDSR
213200000000     C/EJECT
213300000000     C************************************************************
213400000000     C* Copia file creato sulla directory
213500000000     C************************************************************
213600000000     C     CpyToStmF     BEGSR
213700000000     C*
213800000000     C*
213900000000     C                   Eval       WMbr='/qsys.lib/qtemp.lib/'
214000000000     C                              + %trimr(wFile) + '.file/'
214100000000     C                              + %trimr(wFile) + '.mbr'
214200000000     C*
214300000000     C                   Eval       WStmF= %trimr(CartellI61)
214400000000     C                              +'/' + %trimr(FileXml) + '.xml'
214500000000     C*
214600000000     C                   Eval      Cmd = 'CPYTOSTMF  FROMMBR('''
214700000000     C                              + %trimr(WMbr) + ''')'
214800000000     C                              + ' TOSTMF('''                     +'/'
214900000000     C                              + %trimr(WStmF) + ''')'
215000000000     C                              + ' STMFOPT(*REPLACE) '
215100000000     C                              + ' STMFCODPAG(*PCASCII) '
215200000000     C*
215300000000     C                   CALL      'QCMDEXC'                            33
215400000000     C                   Parm                    Cmd             500
215500000000     C                   Parm      500           Len              15 5
215600000000     C*
215700000000     C* se è riuscita la copia cambio l'autorizzazione all'oggetto
215800000000     C                   if        *In33 = *Off
215900000000     C*
216000000000     C                   Eval      Cmd = 'CHGAUT OBJ('''
216100000000     C                              + %trimr(WStmF) + ''')'
216200000000     C                              + ' USER(*PUBLIC) DTAAUT(*RWX)'
216300000000     C*
216400000000     C                   CALL      'QCMDEXC'                            33
216500000000     C                   Parm                    Cmd             500
216600000000     C                   Parm      500           Len              15 5
216700000000     C*
216800000000     C* salvo le informazioni nel catalogo
216900000000     C                   exsr      WrtCatalogo
217000000000     C*
217100000000     C                   commit
217200000000     C                   feod      SdgCst00F
217300170817     C                   feod      SdgCst10l
217400000000     C                   feod      SdgCsf00F
217500000000     C*
217600000000     C* emissione a video di avvenuta creazione
217700000000     C                   exsr      CallSI62R
217800000000     C*
217900000000     C                   endif
218000000000     C*
218100000000     C                   ENDSR
218200000000     C/EJECT
218300000000     C************************************************************
218400000000     C* Cancello file in qtemp
218500000000     C************************************************************
218600000000     C     DelFileO      BEGSR
218700000000     C*
218800000000     C* Cancello il metadata
218900000000     C                   Eval      Cmd= 'DLTF FILE(QTEMP/'  +
219000000000     C                                   WFile   +')'
219100000000     C                   Call      'QCMDEXC'                            33
219200000000     C                   Parm                    Cmd             500
219300000000     C                   Parm      500           len              15 5
219400000000     C*
219500000000     C                   ENDSR
219600000000     C/EJECT
219700000000     C********************************************************************
219800000000     C* Close Sql fetch
219900000000     C********************************************************************
220000000000     C     CloseSql      BEGSR
220100000000     C*
220200000000     C/EXEC SQL
220300000000     C+     Close A1
220400000000     C/END-EXEC
220500000000     C*
220600000000     C                   ENDSR
220700000000     C/EJECT
220800000000     C******************************************************
220900000000     C* Gestione invio multiplo
221000000000     C******************************************************
221100000000     C     GesMultInv    BegSr
221200000000     C*
221300000000     C                   eval      $PosizO  = $PosizO  + 1
221400000000     C                   exsr      UpdateCst
221500000000     C*
221600000000     C* Chiude Tag DTE/DTR
221700000000     C                   exsr      CloseTagDT
221800000000     C* Chiude File
221900000000     C                   Eval      DsOutput     =
222000000000     C                             '</ns2:DatiFattura>'
222100000000     C                   EXSR      WrtXML
222200000000     C*
222300000000     C                   exsr      ChiudoFileO
222400000000     C*
222500000000     C                   select
222600000000     C                   when      $CF   = 'C'
222700000000     C                   eval      wFile = FileDte
222800000000     C*
222900000000     C                   when      $CF   = 'F'
223000000000     C                   eval      wFile = FileDtr
223100000000     C                   endsl
223200000000     C*
223300000000     C                   exsr      CreoFileO
223400000000     C*
223500000000     C                   clear                   RigaMod
223600000000     C                   clear                   $PosizO
223700170812     C                   z-add     0             $MaxRighe
223800000000     C                   z-add     0             $MaxNrDoc
223900000000     C                   clear                   Sv1CstDs
224000000000     C                   clear                   Sv2CstDs
224100000000     C                   eval      $RotturaCF  = *off
224200000000     C                   eval      $RotturaDoc = *off
224300170817     C                   eval      $Errore     = *off
224400000000     C*
224500000000     C* Scrivo TAG Intestazione e Frontespizio
224600000000     C                   exsr      WrtIntestaz
224700000000     C*
224800000000     C                   ENDSR
224900000000     C/EJECT
225000000000     C******************************************************
225100000000     C* Scrittura catalogo
225200000000     C******************************************************
225300000000     C     WrtCatalogo   BEGSR
225400000000     C*
225500000000     C                   clear                   WDesFile
225600000000     C                   clear                   SdgCsf000
225700000000     C*
225800000000     C                   eval      CSFUTEIMM  = XscCut
225900000000     C                   eval      CSFDTIMM   = %date()
226000000000     C                   eval      CSFUTEULMO = XscCut
226100000000     C                   eval      CSFDTULMO  = %date()
226200000000     C                   eval      CSFUTEXML  = XscCut
226300000000     C                   eval      CSFDTXML   = %date()
226400000000     C                   eval      CSFORIGINE = 'A'
226500000000     C                   eval      CSFSOCIETA = XscSoc
226600000000     C                   eval      CSFTPCOM   = 'T'
226700170817     C                   eval      CSFCF      = $Cf
226800000000     C*
226900000000     C                   select
227000000000     C* invio ordinario: tipo record = 'O'
227100000000     C                   when      TipInvI61  = 'O'
227200000000     C                   eval      CSFTPREC   = 'O'
227300000000     C                   eval      WDesFile   = 'Ordinario'
227400000000     C* Rettifica: tipo record = 'R'
227500000000     C                   When      TipInvI61   = 'R'
227600000000     C                   eval      CSFTPREC   = 'R'
227700000000     C                   eval      WDesFile   = 'Rettifica'
227800000000     C* Annullam.: tipo record = 'A'
227900000000     C                   When      TipInvI61   = 'A'
228000000000     C                   eval      CSFTPREC   = 'A'
228100170817     C                   eval      CSFCF      = C10Cf
228200000000     C                   eval      WDesFile   = 'Annullamento'
228300000000     C                   endsl
228400000000     C*
228500000000     C                   eval      CSFANNO    = AnnoI61
228600000000     C                   eval      CSFTRIM    = TrimI61
228700000000     C                   eval      CSFMESE    = 0
228800000000     C                   eval      CSFPRGESTR = 0
228900000000     C                   eval      CSFFILE    = FileXml
229000000000     C                   eval      CSFFILERGP = %trim(FileSav)
229100000000     C*
229200000000     C                   if        TipInvI61  <> 'A'
229300000000     C                   if        $Cf        = 'C'
229400000000     C                   eval      WDesFile   = %trim(WDesFile)
229500000000     C                                        + ' DTE'
229600000000     C                   else
229700000000     C                   eval      WDesFile   = %trim(WDesFile)
229800000000     C                                        + ' DTR'
229900000000     C                   endif
230000000000     C                   endif
230100000000     C*
230200000000     C                   if        CsfTpCom   = 'T'
230300000000     C                   eval      CSFNOTE    = 'Spesometro Trimestrale '
230400000000     C                                        + %trim(WDesFile)
230500000000     C                                        + ' '
230600000000     C                                        + %editc(AnnoI61:'X')
230700000000     C                                        + ' trim. '
230800000000     C                                        + %editc(TrimI61:'X')
230900000000     C                   endif
231000000000     C
231100000000     C                   eval      CSFIDFILE  = *blank
231200000000     C                   eval      CSFDTINVIO = *loval
231300000000     C                   eval      CSFFILESIT = *blank
231400000000     C                   eval      CSFDTESITO = *loval
231500000000     C                   eval      CSFESITO   = *blank
231600000000     C*
231700000000     C                   write     SdgCsf000                            99
231800000000     C*
231900000000     C                   ENDSR
232000000000     C/EJECT
232100000000     C************************************************************
232200000000     C* Emissione a video di avvenuta creazione file
232300000000     C************************************************************
232400000000     C     CallSI62R     BegSr
232500000000     C*
232600000000     C                   call      'SDGSI62R'
232700000000     C                   parm                    SocietaI61
232800000000     C                   parm                    CsfFile
232900000000     C                   parm                    CsfNote
233000000000     C                   parm                    $ProgrInt
233100000000     C                   parm                    CartellI61
233200170822     C                   parm      '0'           Video             1
233300000000     C*
233400000000     C                   EndSR
233500000000     C******************************************************
233600000000     C* Operazioni iniziali
233700000000     C******************************************************
233800000000     C     *INZSR        BEGSR
233900000000     C*
234000000000     C     *Entry        PLIST
234100000000     C                   Parm                    KPJBA
234200000000     C*
234300000000     C                   MoveL     KPJBU         SDGSI61DS
234400000000     C*
234500000000     C     *DMY          Move      UDate         UDateISO
234600000000     C*
234700000000     C                   ENDSR
234800000000     C/EJECT
234900000000     C******************************************************
235000000000     C* Inizializzazione variabili
235100000000     C******************************************************
235200000000     C     INZVAR        BEGSR
235300000000     C*
235400000000     C                   MoveL     KPJBU         SDGSI61DS
235500000000     C*
235600000000     C                   MoveL     'SOC001'      TIPXSC
235700000000     C                   MoveL     SocietaI61    SOCXSC
235800000000     C*
235900000000     C                   EXSR      RepSoc
236000000000     C*
236100000000     C                   if        RTNXSC     <> '1'
236200000000     C                   MoveL     XSOCDS        SOC001
236300000000     C                   else
236400000000     C                   Eval      ERRI61      = *On
236500000000     C                   EXSR      ENDPGM
236600000000     C                   endif
236700000000     C*
236800000000     C                   Eval      WNrDecMC   =  %SubSt(XScSKp:6:1)
236900000000     C                   Move      WNrDecMC      NrDecMC
237000000000     C*
237100000000     C* Reperisco i dati di setup del modulo Spesometro Trimestrale
237200000000     C                   EXSR      RepSDSPETRI
237300000000     C*
237400000000     C* reperisco i dati salvati su ANMPE
237500000000     C                   EXSR      RepFrontesp
237600000000     C*
237700000000     C                   callb     'XSTRCMT'
237800000000     C                   open      SDGCST00F
237900000000     C                   open      SDGCSF00F
238000170817     C                   open      SDGCST10L
238001170828R264 C                   open      SDGCST02L
238100000000     C*
238200000000     C                   ENDSR
238300000000     C/EJECT
238400000000     C************************************************************
238500000000     C* Rerimento parametri di setup per Spesometro Trimestrale
238600000000     C************************************************************
238700000000     C     RepSDSPETRI   BEGSR
238800000000     C*
238900000000     C                   Clear                   SDGPA12DS
239000000000     C*
239100000000     C                   CALLB     'XPAR'
239200000000     C                   Parm                    XScSoc
239300000000     C                   Parm      'SDSPETRI'    XPaPar            8
239400000000     C                   Parm                    XPaOut         2000
239500000000     C                   Parm      *OFF          XPaErr            1
239600000000     C                   Parm      ' '           XPaRic            1
239700000000     C*
239800000000     C                   if        XPaErr      = *On
239900000000     C                   EXSR      ENDPGM
240000000000     C*
240100000000     C                   else
240200000000     C                   MoveL     XPaOut        SDGPA12DS
240300000000     C*
240400000000     C                   if        AttCSTA12  <> *On
240500000000     C                   EXSR      ENDPGM
240600000000     C                   endif
240700000000     C*
240800000000     C                   endif
240900000000     C*
241000000000     C                   ENDSR
241100000000     C/EJECT
241200000000     C************************************************************
241300000000     C* Reperisco i dati per il Frontespizio
241400000000     C******************************************************
241500000000     C     RepFrontesp   BEGSR
241600000000     C*
241700000000     C* preparo i parametri di chiamata al driver
241800000000     C                   Movel     *off          $Trovato
241900000000     C                   Clear                   $SkDati
242000000000     C                   Clear                   XMPEDS
242100000000     C                   EVAL      TpChiamata = '01'
242200000000     C                   EVAL      NChiavi = 5
242300000000     C                   EVAL      VLogica = 2
242400000000     C*
242401170905R265 X***                Do        4             $Cont
242402170905R265 C                   Do        $DimSk        $Cont
242600000000     C                   Clear                   XMPEDS1
242700000000     C                   Eval      DSMPEPGM = 'SDGSI74R' +
242800000000     C                                        %char($cont)
242900000000     C* preparo le chiavi per la chiamata al driver
243000000000     C                   MoveL     XSCSOC        DSMPESOC
243100000000     C                   MoveL     *Blanks       DSMPEPRF
243200000000     C* Si reperiscono i dati di default a seconda delle modalità con
243300000000     C* cui il pgm è usato
243400000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
243500000000     C* MPETIP = 'PV'  pgm richiamato da menù
243600000000     C* MPETIP = 'PB'  x lancio Batch
243700000000     C                   MoveL     'PV'          DSMPETIP
243800000000     C*
243900000000     C                   CALLB     'XMPE'
244000000000     C                   PARM                    XMPEDS
244100000000     C                   PARM                    XMPEDS1
244200000000     C*
244300000000     C     Risultato     IfEq      '0'
244400000000     C                   Movel     *on           $Trovato
244500000000     C*
244600000000     C                   select
244700000000     C                   When      $Cont   = 1
244800000000     C                   MoveL     DSMPEPAR      DsFrontesp1
244900000000     C                   When      $Cont   = 2
245000000000     C                   MoveL     DSMPEPAR      DsFrontesp2
245100000000     C                   When      $Cont   = 3
245200000000     C                   MoveL     DSMPEPAR      DsFrontesp3
245300000000     C                   When      $Cont   = 4
245400000000     C                   MoveL     DSMPEPAR      DsFrontesp4
245401170905R265 C                   When      $Cont   = 5
245402170905R265 C                   MoveL     DSMPEPAR      DsFrontesp41
245500000000     C                   EndSl
245600000000     C*
245700000000     C                   else
245800000000     C*
245900000000     C                   Eval      ERRI61      = '2'
246000000000     C                   EXSR      ENDPGM
246100000000     C                   endif
246200000000     C*
246300000000     C                   Enddo
246400000000     C*
246500000000     C                   Endsr
246600000000     C/EJECT
246700000000     C******************************************************
246800000000     C* Imposta nome file da trasmettere
246900000000     C******************************************************
247000000000     C     SetNomeF      begsr
247100000000      *
247200000000     C                   Clear                   FileXml
247300000000      * il nome del file deve rispettare la seguente nomenclatura:
247400000000      *  1) codice paese
247500000000      *  2) identificativo univoco del soggetto trasmittente
247600000000      *  3) tipologia file
247700000000      *  4) progressivo univoco del file (numeratore SD12)
247800000000      * dove:
247900000000      *  1) il codice paese va espresso secondo lo standard ISO 3166-1 alpha-2
248000000000      *     code;
248100000000      *  2) l'identificativo univoco del responsabile della trasmissione, sia esso
248200000000      *     persona fisica o persona giuridica, è rappresentato dal suo identificativo
248300000000      *     fiscale (codice fiscale nel caso di soggetto trasmittente residente in
248400000000      *     Italia, identificativo proprio del paese di appartenenza nel caso di soggetto
248500000000      *     trasmittente residente all?estero);
248600000000      *    la lunghezza di questo identificativo è di:
248700000000      *     - 11 caratteri (minimo) e 16 caratteri (massimo) nel caso di
248800000000      *       codice paese IT;
248900000000      *     - 2 caratteri (minimo) e 28 caratteri (massimo) altrimenti;
249000000000      *  3) la tipologia file è rappresentata dal valore fisso "DF";
249100000000      *  4) il progressivo univoco del file è rappresentato da una stringa
249200000000      *     alfanumerica di lunghezza massima di 5 caratteri e con valori
249300000000      *     ammessi [a-z], [A-Z], [0-9]
249400000000      *
249500000000      *  Es. ITAAABBB99T99X999W_DF_00001.xml
249600000000      *      IT99999999999_DF_00002.xml
249700000000     C                   clear                   $NomeF
249800000000     C                   if        $Piva1 <> *blank
249900000000     C                   eval      $NomeF = $PIva1
250000000000     C                   else
250100000000     C                   eval      $NomeF = $Cdf1
250200000000     C                   endif
250300000000      *
250400000000     C                   eval      FileXml = 'IT' + %trim($NomeF)
250500000000     C                                     + '_' + 'DF' + '_'
250600000000     C                                     + %editc($ProgrInvio:'X')
250700000000      *
250800000000     C                   if        $PrimaV  = *off
250900000000     C                   eval      FileSav  = FileXml
251000000000     C                   eval      $PrimaV  = *on
251100000000     C                   endif
251200000000      *
251300000000     C                   endsr
251400000000     C/EJECT
251500000000     C************************************************************
251600000000     C* Reperisco il progressivo univoco del file
251700000000     C************************************************************
251800000000     C     TakeNrProg    BEGSR
251900000000     C*
252000000000     C                   Move      UDateIso      XNmDat
252100000000     C                   Move      UDateIso      XNmDag
252200000000     C*
252300000000     C                   CallB     'XNMR'
252400000000     C                   Parm      '3'           XNmRic            1
252500000000     C                   Parm      XScSoc        XNmSoc            3
252600000000     C                   Parm      *Blank        XNmUni            8
252700000000     C                   Parm      *Blank        XNmCtb            2
252800000000     C                   Parm      *Blank        XNmKey            8
252900000000     C                   Parm      'SD12'        XNmSer            4
253000000000     C                   Parm      *On           XNmCmt            1
253100000000     C                   Parm                    XNmDat           10
253200000000     C                   Parm      *Off          XNmIVA            1
253300000000     C                   Parm      *Off          XNmReg            1
253400000000     C                   Parm                    XNmErr            1
253500000000     C                   Parm                    XNmNum            9 0
253600000000     C                   Parm                    XNmAut            1
253700000000     C                   Parm                    XNmDag           10
253800000000     C*
253900000000     C* se viene rilevato un errore
254000000000     C                   if        XNmErr      = '1'      or
254100000000     C                             XNmErr      = '2'      or
254200000000     C                             XNmErr      = '7'
254300000000     C                   exsr      EndPgm
254400000000     C                   else
254500000000     C                   move      xnmnum        $ProgrInvio
254600000000     C                   add       1             $ProgrInt
254700000000     C                   endif
254800000000     C*
254900000000     C                   ENDSR
255000000000     C/EJECT
255100000000     C******************************************************
255200000000     C* Scrittura file XML
255300000000     C******************************************************
255400000000     C     WrtXML        BEGSR
255500000000     C*
255600000000     C                   Eval      Output      = DsOutput
255700000000     C                   Write     FileO         FileODS
255800000000     C*
255900000000     C                   ENDSR
256000000000     C/EJECT
256100000000     C************************************************************
256200000000     C* Reperimento dati società
256300000000     C************************************************************
256400000000     C     RepSoc        BEGSR
256500000000     C*
256600000000     C                   CALL      'SDGSOCDR'
256700000000     C                   Parm                    TIPXSC            6
256800000000     C                   Parm                    SOCXSC            3
256900000000     C                   Parm                    CDSXSC            9 0
257000000000     C                   Parm                    MODXSC            3
257100000000     C                   Parm      *Blanks       RTNXSC            1
257200000000     C                   Parm                    XSOCDS
257300000000     C                   Parm                    KPJBA
257400000000     C*
257500000000     C                   ENDSR
257600000000     C/EJECT
257700000000     C******************************************************
257800000000     C* DEFINIZIONE KLIST
257900000000     C******************************************************
258000000000     C     DefKList      BEGSR
258100170817     C*
258200170817     C* SDGCST10L
258300170817     C     K02CST10      Klist
258400170817     C                   Kfld                    C10SOCIETA
258500170817     C                   Kfld                    C10IDFILEO
258501170828R264 C*
258502170828  "  C* SDGCST02L
258503170828  "  C     K20CST02      Klist
258504170828  "  C                   Kfld                    C02Societa
258505170828  "  C                   Kfld                    C02TpCom
258506170828  "  C                   kfld                    C02Tprec
258507170828  "  C                   kfld                    C02Anno
258508170828  "  C                   kfld                    C02Trim
258509170828  "  C                   kfld                    C02Mese
258510170828  "  C                   kfld                    C02CLIFOR
258511170828  "  C                   kfld                    C02TPDOC
258512170828  "  C                   kfld                    C02DTDOC
258513170828  "  C                   kfld                    C02NRDOC
258514170828  "  C                   kfld                    C02SERDOC
258515170828  "  C                   kfld                    C02DTREG
258516170828  "  C                   kfld                    C02NRREG
258517170828  "  C                   kfld                    C02SERREG
258518170828  "  C                   kfld                    C02DTANNOT
258519170828  "  C                   kfld                    C02ALIQ
258520170828  "  C                   kfld                    C02MOTESCL
258521170828  "  C                   kfld                    C02PERCDET
258522170828  "  C                   kfld                    C02DEDUCIB
258523170828R264 C                   kfld                    C02ESIGIB
258600000000     C*
258700000000     C                   ENDSR
258800000000     C/EJECT
258900000000     C******************************************************
259000000000     C* PGM PER GESTIONE MESSAGGI ERRORE
259100000000     C******************************************************
259200000000     C     MSGPGM        BEGSR
259300000000     C*
259400000000     C                   Z-ADD     2             MSGMSG
259500000000     C                   MOVEL     'S'           MSGRSP
259600000000     C                   MOVEL     'CR'          MSGTPR
259700000000     C                   MOVEL     'S'           MSGEMV
259800000000     C                   Z-ADD     10            MSGRGP
259900000000     C                   MOVEL     'S'           MSGLCK
260000000000     C                   MOVEA     STATUS        STS
260100000000     C                   MOVEL     'N'           MSGCNL
260200000000     C                   MOVEL     'N'           MSGVID
260300000000     C                   MOVEL     'N'           MSGOPE
260400000000     C                   MOVEL     'N'           MSGSTP
260500000000     C                   CALL      'SDGMSGR'
260600000000     C                   PARM                    SDGMSGDS
260700000000     C*
260800000000     C                   ENDSR
260900000000     C/EJECT
