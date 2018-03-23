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
002000000000     FSDGCST10L UF   E           K DISK    Commit
002100000000     F                                     UsrOpn
002200000000     F                                     Prefix(C10:3)
002300000000     F                                     Rename(SdgCst000:SdgCst010)
002400000000R264 FSDGCST02L IF   E           K DISK
002500000000 "   F                                     UsrOpn
002600000000 "   F                                     Prefix(C02:3)
002700000000R264 F                                     Rename(SdgCst000:SdgCst002)
002800000000     F* Catalogo file
002900000000     FSDGCSF00F O    E             DISK    Commit
003000000000     F                                     UsrOpn
003100000000     F*---------------
003200000000     F* File definito a pgm con cui viene sovrascritto il file
003300000000     F* creato in qtemp contenente il metadata
003400000000     FFileO     UF A F  500        DISK    USROPN
003500000000     D*---------------
003600000000     D* DS per mappare metadata
003700000000     D FileoDS         DS
003800000000     D  DsOutput               1    500
003900000000     D*---------------------
004000000000     D* Passaggio Parametri
004100000000     D KPJBA         E DS
004200000000     D*-------------
004300000000     D* Reperimento nome PGM
004400000000     D STATUS         SDS           333
004500000000     D  DSPGM            *PROC
004600000000     D  PARMS            *PARMS
004700000000     D*----------------
004800000000     D* Dati di ambiente ottenuti da SDGSOCDR
004900000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
005000000000     D*-------------
005100000000     D* DS Interna per dati di output di SDGSOCDR
005200000000     D XSOCDS          S           1000
005300000000     D*---------------------
005400000000     D NrDecMC         S              1  0
005500000000     D WNrDecMC        S              1
005600000000     D*-------------
005700000000     D* Parametri modulo SDSPETRI
005800000000     D SDGPA12DS     E DS
005900000000     D*---------------------
006000000000     D SDGSI61DS     E DS
006100000000     D*-------------
006200000000     D* Valori assunti
006300000000     D XMPEDS        E DS                  INZ
006400000000     D XMPEDS1       E DS                  INZ
006500000000     D*-------------
006600000000     D* IdentificativiFiscali
006700000000     D DsFrontesp1     DS                  INZ
006800000000     D $Stato1                             Like(CSTStato1 )
006900000000     D $IdPaes1                            Like(CSTIdPaes1)
007000000000     D $PIva1                              like(CstPiva1)
007100000000     D $CdF1                               like(CstCdFisc1)
007200000000     D $CogRag1                            like(CstCogRag1)
007300000000     D $TpN1                               like(CstTpN1   )
007400000000     D $Denom1                             like(CstDenom1 )
007500000000     D $Cognom1                            like(CstCognom1)
007600000000     D $Nome1                              like(CstNome1  )
007700000000     D DsFrontesp2     DS                  INZ
007800000000     D $INDIR2                             like(CstINDIR2 )
007900000000     D $NRCIV2                             like(CstNRCIV2 )
008000000000     D $CAP2                               like(CstCAP2   )
008100000000     D $LOCALI2                            like(CstLOCALI2)
008200000000     D $PROV2                              like(CstPROV2  )
008300000000     D $Stato2                             Like(CSTStato2 )
008400000000     D $IDPAES2                            like(CstIDPAES2)
008500000000     D* Stabile Organizzazione
008600000000     D DsFrontesp3     DS                  INZ
008700000000     D $INDIR3                             like(CstINDIR3 )
008800000000     D $NRCIV3                             like(CstNRCIV3 )
008900000000     D $CAP3                               like(CstCAP3   )
009000000000     D $LOCALI3                            like(CstLOCALI3)
009100000000     D $PROV3                              like(CstPROV3  )
009200000000     D $Stato3                             Like(CSTStato3 )
009300000000     D $IDPAES3                            like(CstIDPAES3)
009400000000     D* Rappresentante Fiscale
009500000000     D DsFrontesp4     DS                  INZ
009600000000     D $Stato4                             Like(CSTStato4 )
009700000000     D $IDPAES4                            like(CstIDPAES4)
009800000000     D $PIVA4                              like(CstPIVA4  )
009900000000     D $CogRag4                            like(CstCogRag4)
010000000000     D $TpN4                               like(CstTpN4   )
010100000000     D $Denom4                             like(CstDenom4 )
010200000000     D $Cognom4                            like(CstCognom4)
010300170905R2654D**** $Nome4                              like(CstNome4  )
010400170904  "  D* Rappresentante Fiscale
010500170904  "  D DsFrontesp41    DS                  INZ
010600170905R2654D $Nome4                              like(CstNome4  )
010700170911R266 D WCdFisc                             like(CstCdFisc )
010800170911R266 D WCarica                        2
010900000000     D*-------------
011000170905R2654D $DimSk          C                   Const(5)
011100000000     D $Cont           S              1S 0
011200000000     D $Errore         S              1    inz('0')
011300000000     D $Trovato        S              1
011400000000     D $RotturaCF      S              1
011500000000     D $RotturaDoc     S              1
011600000000     D $SkDati         S                   Like(DsMpePar) dim(8)
011700000000     D*-------------
011800000000     D RRNCst          S              9b 0
011900000000     D SdgCstDs      E DS                  Extname(SdgCst00F)
012000000000     D*----------
012100000000     D SavCstDs      E DS                  Extname(SdgCst00F)
012200000000     D                                     Prefix(Sav:3)
012300000000     D*----------
012400000000     D Sv1CstDs      E DS                  Extname(SdgCst00F)
012500000000     D                                     Prefix(Sv1:3)
012600000000     D*----------
012700000000     D Sv2CstDs      E DS                  Extname(SdgCst00F)
012800000000     D                                     Prefix(Sv2:3)
012900000000R264 D*----------
013000000000  "  D Sv3CstDs      E DS                  Extname(SdgCst00F)
013100000000R264 D                                     Prefix(Sv3:3)
013200000000     D*----------
013300000000     D indent1         C                   CONST('  ')
013400000000     D indent2         C                   CONST('    ')
013500000000     D indent3         C                   CONST('      ')
013600000000     D indent4         C                   CONST('        ')
013700000000     D indent5         C                   CONST('          ')
013800000000     D indent6         C                   CONST('            ')
013900000000     D indent7         C                   CONST('              ')
014000000000     D indent8         C                   CONST('                ')
014100000000     D indent9         C                   CONST('                  ')
014200000000     D cMaxNrDoc       C                   CONST(1000)
014300000000     D*-------------
014400000000     D* Variabili
014500000000     D $PosizO         S                   like(CstPosizO)
014600000000     D $X              S              5  0
014700000000     D RigaMod         S              9  0 Inz
014800000000     D WDesFile        S             15
014900000000     D WMbr            S             60
015000000000     D WSTMF           S                   LIKE(CartellI61)
015100000000     D wFile           S             10
015200000000     D FileXml         S             50
015300000000     D FileSav         S             50
015400000000     D FileDte         C                   CONST('FileDte')
015500000000     D FileDtr         C                   CONST('FileDtr')
015600000000     D FileAnn         C                   CONST('FileAnn')
015700000000     D UDateISO        S               D    datfmt(*iso)
015800000000     D $Cf             S              1
015900000000     D $TpCom          S              1    inz('T')
016000000000     D $TpRecOrd       S              1    inz('O')
016100000000     D $TpRecRet       S              1    inz('R')
016200000000     D $TpRecAnn       S              1    inz('A')
016300000000     D $FineCST        S              1    Inz(*Off)
016400000000     D $PrimaV         S              1    Inz(*Off)
016500000000     D $NomeF          S             16
016600000000     D StringaSql      DS          5000
016700000000     D SqlCST          S           5000
016800000000     D $Ind            S              4  0
016900000000     D w_ImpOpe        S                   LIKE(CstImpOpe)
017000000000     D w_IvaOpe        S                   LIKE(CstIvaOpe)
017100000000     D WImpInp         S                   Like(ImporD0202)
017200000000     D WImpOut         S                   Like(ImporD0202)
017300000000     D $ProgrInvio     S              5S 0 Inz(0)
017400000000     D $ProgrInt       S              3S 0 Inz(0)
017500000000     D*-------------
017600000000     D* Data 10A
017700000000     D WDataDs         DS            10
017800000000     D WAnnoDs                 1      4A
017900000000     D WMeseDs                 6      7A
018000000000     D WGiornoDs               9     10A
018100000000     D*--
018200000000     D WWImporto       S             19  3
018300000000     D $ImportoA       S             19A
018400000000     D $MeseA          S              2A
018500000000     D*-------------
018600000000     D* Campi I/O per routine editazione di un numero
018700000000     D*
018800000000     D* Parametri X23EDITR
018900000000     D* Numero da editare
019000000000     D $SrcVar         s             30S 0
019100000000     D* Lunghezza del campo da editare
019200000000     D $SrcVarPr       s              9B 0 inz(30)
019300000000     D* Numero cifre decimali da editare
019400000000     D $SrcVarDe       s              9B 0
019500000000     D* Codice di editazione
019600000000     D $EdtCode        s              1    inz('4')
019700000000     D* Lunghezza campo video
019800000000     D $LenFld         s              3S 0
019900000000     D* Campo editato
020000000000     D $RcvrVar        s             50
020100000000     D* Flag errore
020200000000     D $Err            s              1
020300000000     D*----------------
020400000000     D* variabili per invio multiplo
020500000000     D $MultiInvio     S              1A   Inz(*Off)
020600000000     D $MaxRighe       S              5S 0 Inz(0)
020700000000     D $MaxNrDoc       S              5S 0 Inz(0)
020800000000     D*-------------
020900000000     D* DS Esterna gestione messaggi
021000000000     D SDGMSGDS      E DS
021100000000     D  STS          E                     EXTFLD(MSGSTS)
021200000000     D                                     DIM(333)
021300000000     D  JBA          E                     EXTFLD(MSGJBA)
021400000000     D                                     DIM(502)
021500000000     D*-------------
021600000000     D* Prototipo per la procedura Euro
021700000000     D/COPY *LIBL/SRCCPY,PJX002PR
021800000000     D*------------------------------------------------------
021900000000     I* FileOutPut
022000000000     IFileO     NS  01
022100000000     I                                  1  500  OutPut
022200000000     C*----------------------------------------------------*
022300000000     C*                MAIN LINE PROGRAM
022400000000     C*----------------------------------------------------*
022500000000     C*
022600000000     C                   exsr      InzVar
022700000000     C*
022800000000     C                   select
022900000000     C* Invio ordinario o Rettifica
023000000000     C                   when      TipInvI61 = 'O' or
023100000000     C                             TipInvI61 = 'R'
023200000000     C* Vengono creati fino a due file:
023300000000     C* - uno per le fatture emesse   - DTE
023400000000     C* - uno per le fatture ricevute - DTR
023500000000     C*
023600000000     C                   if        CfI61 = 'C'  or
023700000000     C                             CfI61 = *blank
023800000000     C                   eval      $CF   = 'C'
023900000000     C                   eval      wFile = FileDte
024000000000     C                   exsr      Creazione
024100000000     C                   endif
024200000000     C*
024300000000     C                   if        CfI61 = 'F'  or
024400000000     C                             CfI61 = *blank
024500000000     C                   eval      $CF   = 'F'
024600000000     C                   eval      wFile = FileDtr
024700000000     C                   exsr      Creazione
024800000000     C                   endif
024900000000     C*
025000000000     C* Annullamento
025100000000     C                   when      TipInvI61 = 'A'
025200000000     C*
025300000000     C                   eval      wFile = FileAnn
025400000000     C                   exsr      CreaAnnullo
025500000000     C*
025600000000     C                   endsl
025700000000     C*
025800000000     C                   exsr      EndPgm
025900000000     C*
026000000000     C/EJECT
026100000000     C******************************************************
026200000000     C* FINE PROGRAMMA
026300000000     C******************************************************
026400000000     C     ENDPGM        BEGSR
026500000000     C*
026600000000     C                   Seton                                            LR
026700000000     C                   Return
026800000000     C*
026900000000     C                   ENDSR
027000000000     C/EJECT
027100000000     C******************************************************
027200000000     C* Creazione archivio
027300000000     C******************************************************
027400000000     C     Creazione     BEGSR
027500000000     C*
027600000000     C                   exsr      CreoFileO
027700000000     C*
027800000000     C* Lettura file spesometro
027900000000     C                   exsr      CicloSql
028000000000     C*
028100000000     C                   exsr      ChiudoFileO
028200000000     C*
028300000000     C                   ENDSR
028400000000     C/EJECT
028500000000     C******************************************************
028600000000     C* Creazione archivio annullamento
028700000000     C******************************************************
028800000000     C     CreaAnnullo   BEGSR
028900000000     C*
029000000000     C                   exsr      CreoFileO
029100000000     C* Frontespizio
029200000000     C                   EXSR      WrtIntestaz
029300000000     C*
029400000000     C* 4 <ANN>
029500000000     C                   Eval      DsOutput     =
029600000000     C                                      indent1 +
029700000000     C                              '<ANN>'
029800000000     C                   EXSR      WrtXML
029900000000     C*
030000000000     C* 4.1 IdFile
030100000000     C                   Eval      DsOutput     =
030200000000     C                                      indent2 +
030300000000     C                              '<IdFile>' +
030400000000     C                                %trim(IdFileI61) +
030500000000     C                              '</IdFile>'
030600000000     C                   EXSR      WrtXML
030700000000     C*
030800000000     C* 4.2 Posizione
030900000000     C                   if        PosizI61 <> 0
031000000000     C                   Eval      DsOutput     =
031100000000     C                                      indent2 +
031200000000     C                              '<Posizione>' +
031300000000     C                                %trim(%editc(PosizI61:'Z'))  +
031400000000     C                              '</Posizione>'
031500000000     C                   EXSR      WrtXML
031600000000     C                   endif
031700000000     C*
031800000000     C* 4 </ANN>
031900000000     C                   Eval      DsOutput     =
032000000000     C                                      indent1 +
032100000000     C                              '</ANN>'
032200000000     C                   EXSR      WrtXML
032300000000     C*
032400000000     C* Aggiorna record con l'indicazione del file di annullamento
032500000000     C                   EXSR      UpdCstAnn
032600000000     C*
032700000000     C* Chiude File
032800000000     C                   Eval      DsOutput     =
032900000000     C                             '</ns2:DatiFattura>'
033000000000     C                   EXSR      WrtXML
033100000000     C*
033200000000     C                   exsr      ChiudoFileO
033300000000     C*
033400000000     C                   ENDSR
033500000000     C/EJECT
033600000000     C******************************************************
033700000000     C* Scrive TAG INTESTAZIONE
033800000000     C******************************************************
033900000000     C     WrtIntestaz   BEGSR
034000000000     C*
034100000000     C                   Eval      DsOutput     =
034200000000     C                             '<?xml version="1.0" encoding="UTF-8"'
034300000000     C                               + ' standalone="yes"?>'
034400000000     C                   EXSR      WrtXML
034500000000     C*
034600000000     C                   Eval      DsOutput     =
034700000000     C                             '<ns2:DatiFattura versione="DAT20" '
034800000000     C                             + 'xmlns:ns2="http://ivaservizi.'
034900000000     C                             + 'agenziaentrate.gov.it'
035000000000     C                             + '/docs/xsd/fatture/v2.0">'
035100000000     C                   EXSR      WrtXML
035200000000     C*
035300000000     C* 1.DatiFatturaHeader
035400000000     C                   Eval      DsOutput     =
035500000000     C                                      indent1 +
035600000000     C                              '<DatiFatturaHeader>'
035700000000     C                   EXSR      WrtXML
035800000000     C*
035900000000     C* 1.1.ProgressivoInvio
036000000000     C                   Eval      DsOutput     =
036100000000     C                                      indent2 +
036200000000     C                              '<ProgressivoInvio>' +
036300000000     C                               %char($ProgrInvio) +
036400000000     C                              '</ProgressivoInvio>'
036500000000     C                   EXSR      WrtXML
036600170911R266 C*
036700170911  "  C*  1.2. <Dichiarante> (facoltativo)
036800170911  "  C                   if        WCdFisc  <> *blank
036900170911  "  C                   Eval      DsOutput     =
037000170911  "  C                                      indent2  +
037100170911  "  C                              '<Dichiarante>'
037200170911  "  C                   EXSR      WrtXML
037300170911  "  C*
037400170911  "  C*  1.2.1 <CodiceFiscale> (obbligatorio se presente tag Dichiarante)
037500170911  "  C                   Eval      DsOutput     =
037600170911  "  C                                      indent3  +
037700170911  "  C                              '<CodiceFiscale>' +
037800170911  "  C                                %trim(WCdFisc) +
037900170911  "  C                              '</CodiceFiscale>'
038000170911  "  C                   EXSR      WrtXML
038100170911  "  C*  1.2.2 <Carica> (obbligatorio se presente tag Dichiarante)
038200170911  "  C                   Eval      DsOutput     =
038300170911  "  C                                      indent3  +
038400170911  "  C                              '<Carica>' +
038500170911  "  C                                %trim(WCarica) +
038600170911  "  C                              '</Carica>'
038700170911  "  C                   EXSR      WrtXML
038800170911  "  C*
038900170911  "  C                   Eval      DsOutput     =
039000170911  "  C                                      indent2  +
039100170911  "  C                              '</Dichiarante>'
039200170911  "  C                   EXSR      WrtXML
039300170911R266 C                   endif
039400000000     C*
039500000000     C                   Eval      DsOutput     =
039600000000     C                                      indent1 +
039700000000     C                              '</DatiFatturaHeader>'
039800000000     C                   EXSR      WrtXML
039900000000     C*
040000000000     C* Invio ordinario o Rettifica
040100000000     C                   if        TipInvI61 = 'O' or
040200000000     C                             TipInvI61 = 'R'
040300000000     C* DTE/DTR
040400000000     C* CedentePrestatoreDTE / CessionarioCommittenteDTR
040500000000     C                   select
040600000000     C                   when      $Cf   = 'C'
040700000000     C* 2.  DTE
040800000000     C                   Eval      DsOutput     =
040900000000     C                                      indent1 +
041000000000     C                              '<DTE>'
041100000000     C                   EXSR      WrtXML
041200000000     C*
041300000000     C* 2.  CedentePrestatoreDTE
041400000000     C                   Eval      DsOutput     =
041500000000     C                                      indent2 +
041600000000     C                              '<CedentePrestatoreDTE>'
041700000000     C                   EXSR      WrtXML
041800000000     C*
041900000000     C                   when      $Cf   = 'F'
042000000000     C* 3.  DTR
042100000000     C                   Eval      DsOutput     =
042200000000     C                                      indent1 +
042300000000     C                              '<DTR>'
042400000000     C                   EXSR      WrtXML
042500000000     C*
042600000000     C* 3.  CessionarioCommittenteDTR
042700000000     C                   Eval      DsOutput     =
042800000000     C                                      indent2 +
042900000000     C                              '<CessionarioCommittenteDTR>'
043000000000     C                   EXSR      WrtXML
043100000000     C                   endsl
043200000000     C*
043300000000     C* Frontespizio
043400000000     C                   EXSR      WrtFrontesp
043500000000     C*
043600000000     C* Chiusura Tag
043700000000     C* CedentePrestatoreDTE / CessionarioCommittenteDTR
043800000000     C                   select
043900000000     C                   when      $Cf   = 'C'
044000000000     C*
044100000000     C* 2.  /CedentePrestatoreDTE
044200000000     C                   Eval      DsOutput     =
044300000000     C                                      indent2 +
044400000000     C                              '</CedentePrestatoreDTE>'
044500000000     C                   EXSR      WrtXML
044600000000     C*
044700000000     C                   when      $Cf   = 'F'
044800000000     C*
044900000000     C* 3.  /CessionarioCommittenteDTR
045000000000     C                   Eval      DsOutput     =
045100000000     C                                      indent2 +
045200000000     C                              '</CessionarioCommittenteDTR>'
045300000000     C                   EXSR      WrtXML
045400000000     C                   endsl
045500000000     C                   endif
045600000000     C*
045700000000     C                   ENDSR
045800000000     C/EJECT
045900000000     C******************************************************
046000000000     C* Scrivi TAG FRONTESPIZIO
046100000000     C******************************************************
046200000000     C     WrtFrontesp   BEGSR
046300000000     C*
046400000000     C* 2.1.1 <IdentificativiFiscali>
046500000000     C                   Eval      DsOutput     =
046600000000     C                                      indent3 +
046700000000     C                              '<IdentificativiFiscali>'
046800000000     C                   EXSR      WrtXML
046900000000     C*
047000000000     C* 2.1.1.1 <IdFiscaleIVA>
047100000000     C                   Eval      DsOutput     =
047200000000     C                                      indent4  +
047300000000     C                              '<IdFiscaleIVA>'
047400000000     C                   EXSR      WrtXML
047500000000     C*
047600000000     C* 2.1.1.1.1 <IdPaese>  </IdPaese>
047700000000     C                   Eval      DsOutput     =
047800000000     C                                      indent5  +
047900000000     C                              '<IdPaese>' +
048000000000     C                                %trim($IdPaes1) +
048100000000     C                              '</IdPaese>'
048200000000     C                   EXSR      WrtXML
048300000000     C*
048400000000     C* 2.1.1.1.2 <IdCodice>  </IdCodice>
048500000000     C                   Eval      DsOutput     =
048600000000     C                                      indent5  +
048700000000     C                              '<IdCodice>'     +
048800000000     C                                %trim($PIVA1) +
048900000000     C                              '</IdCodice>'
049000000000     C                   EXSR      WrtXML
049100000000     C*
049200000000     C* 2.1.1.1 </IdFiscaleIVA>
049300000000     C                   Eval      DsOutput     =
049400000000     C                                      indent4  +
049500000000     C                              '</IdFiscaleIVA>'
049600000000     C                   EXSR      WrtXML
049700000000     C*
049800000000     C* 2.1.1.2 <CodiceFiscale> (facoltativo)
049900000000     C                   if        $Cdf1 <> *blank
050000000000     C                   Eval      DsOutput     =
050100000000     C                                      indent4  +
050200000000     C                              '<CodiceFiscale>' +
050300000000     C                                %trim($Cdf1) +
050400000000     C                              '</CodiceFiscale>'
050500000000     C                   EXSR      WrtXML
050600000000     C                   endif
050700000000     C*
050800000000     C* 2.1.1 </IdentificativiFiscali>
050900000000     C                   Eval      DsOutput     =
051000000000     C                                      indent3 +
051100000000     C                              '</IdentificativiFiscali>'
051200000000     C                   EXSR      WrtXML
051300000000     C*
051400000000     C* 2.1.2 <AltriDatiIdentificativi>
051500000000     C                   Eval      DsOutput     =
051600000000     C                                      indent3 +
051700000000     C                              '<AltriDatiIdentificativi>'
051800000000     C                   EXSR      WrtXML
051900000000     C*
052000000000     C* 2.1.2.1 <Denominazione>
052100000000     C                   if        $Denom1  <> *blank
052200000000     C                   Eval      DsOutput     =
052300000000     C                                      indent4 +
052400000000     C                              '<Denominazione>' +
052500000000     C                                %trim($Denom1) +
052600000000     C                              '</Denominazione>'
052700000000     C                   EXSR      WrtXML
052800000000     C                   else
052900000000     C* 2.1.2.2 <Nome>
053000000000     C                   if        $Nome1   <> *blank
053100000000     C                   Eval      DsOutput     =
053200000000     C                                      indent4 +
053300000000     C                              '<Nome>' +
053400000000     C                                %trim($Nome1) +
053500000000     C                              '</Nome>'
053600000000     C                   EXSR      WrtXML
053700000000     C* 2.1.2.3 <Cognome>
053800000000     C                   Eval      DsOutput     =
053900000000     C                                      indent4 +
054000000000     C                              '<Cognome>' +
054100000000     C                                %trim($Cognom1) +
054200000000     C                              '</Cognome>'
054300000000     C                   EXSR      WrtXML
054400000000     C                   endif
054500000000     C                   endif
054600000000     C*
054700000000     C* 2.1.2.4 <Sede>
054800000000     C                   Eval      DsOutput     =
054900000000     C                                      indent4 +
055000000000     C                              '<Sede>'
055100000000     C                   EXSR      WrtXML
055200000000     C*
055300000000     C* 2.1.2.4.1 <Indirizzo>
055400000000     C                   Eval      DsOutput     =
055500000000     C                                      indent5 +
055600000000     C                              '<Indirizzo>' +
055700000000     C                                %trim($Indir2) +
055800000000     C                              '</Indirizzo>'
055900000000     C                   EXSR      WrtXML
056000000000     C*
056100000000     C* 2.1.2.4.2 <NumeroCivico>
056200000000     C                   if        $NrCiv2 <> *blank
056300000000     C                   Eval      DsOutput     =
056400000000     C                                      indent5 +
056500000000     C                              '<NumeroCivico>' +
056600000000     C                                %trim($NrCiv2) +
056700000000     C                              '</NumeroCivico>'
056800000000     C                   EXSR      WrtXML
056900000000     C                   endif
057000000000     C*
057100000000     C* 2.1.2.4.3 <CAP>
057200000000     C                   Eval      DsOutput     =
057300000000     C                                      indent5 +
057400000000     C                              '<CAP>' +
057500000000     C                                %trim($Cap2) +
057600000000     C                              '</CAP>'
057700000000     C                   EXSR      WrtXML
057800000000     C*
057900000000     C* 2.1.2.4.4 <Comune>
058000000000     C                   Eval      DsOutput     =
058100000000     C                                      indent5 +
058200000000     C                              '<Comune>' +
058300000000     C                                %trim($Locali2) +
058400000000     C                              '</Comune>'
058500000000     C                   EXSR      WrtXML
058600000000     C*
058700000000     C* 2.1.2.4.5 <Provincia>
058800000000     C                   if        $IdPaes2 = 'IT'
058900000000     C                   Eval      DsOutput     =
059000000000     C                                      indent5 +
059100000000     C                              '<Provincia>' +
059200000000     C                                %trim($Prov2) +
059300000000     C                              '</Provincia>'
059400000000     C                   EXSR      WrtXML
059500000000     C                   endif
059600000000     C*
059700000000     C* 2.1.2.4.6 <Nazione>
059800000000     C                   Eval      DsOutput     =
059900000000     C                                      indent5 +
060000000000     C                              '<Nazione>' +
060100000000     C                                %trim($IdPaes2) +
060200000000     C                              '</Nazione>'
060300000000     C                   EXSR      WrtXML
060400000000     C*
060500000000     C* 2.1.2.4 </Sede>
060600000000     C                   Eval      DsOutput     =
060700000000     C                                      indent4 +
060800000000     C                              '</Sede>'
060900000000     C                   EXSR      WrtXML
061000000000     C*
061100000000     C* 2.1.2.5 <StabileOrganizzazione>
061200000000     C                   if        $Indir3 <> *blank
061300000000     C                   Eval      DsOutput     =
061400000000     C                                      indent4 +
061500000000     C                              '<StabileOrganizzazione>'
061600000000     C                   EXSR      WrtXML
061700000000     C*
061800000000     C* 2.1.2.5.1 <Indirizzo>
061900000000     C                   Eval      DsOutput     =
062000000000     C                                      indent5 +
062100000000     C                              '<Indirizzo>' +
062200000000     C                                %trim($Indir3) +
062300000000     C                              '</Indirizzo>'
062400000000     C                   EXSR      WrtXML
062500000000     C*
062600000000     C* 2.1.2.5.2 <NumeroCivico>
062700000000     C                   if        $NrCiv3 <> *blank
062800000000     C                   Eval      DsOutput     =
062900000000     C                                      indent5 +
063000000000     C                              '<NumeroCivico>' +
063100000000     C                                %trim($NrCiv3) +
063200000000     C                              '</NumeroCivico>'
063300000000     C                   EXSR      WrtXML
063400000000     C                   endif
063500000000     C*
063600000000     C* 2.1.2.5.3 <CAP>
063700000000     C                   Eval      DsOutput     =
063800000000     C                                      indent5 +
063900000000     C                              '<CAP>' +
064000000000     C                                %trim($Cap3) +
064100000000     C                              '</CAP>'
064200000000     C                   EXSR      WrtXML
064300000000     C*
064400000000     C* 2.1.2.5.4 <Comune>
064500000000     C                   Eval      DsOutput     =
064600000000     C                                      indent5 +
064700000000     C                              '<Comune>' +
064800000000     C                                %trim($Locali3) +
064900000000     C                              '</Comune>'
065000000000     C                   EXSR      WrtXML
065100000000     C*
065200000000     C* 2.1.2.5.5 <Provincia>
065300000000     C                   if        $IdPaes3 = 'IT'
065400000000     C                   Eval      DsOutput     =
065500000000     C                                      indent5 +
065600000000     C                              '<Provincia>' +
065700000000     C                                %trim($Prov3) +
065800000000     C                              '</Provincia>'
065900000000     C                   EXSR      WrtXML
066000000000     C                   endif
066100000000     C*
066200000000     C* 2.1.2.4.6 <Nazione>
066300000000     C                   Eval      DsOutput     =
066400000000     C                                      indent5 +
066500000000     C                              '<Nazione>' +
066600000000     C                                %trim($IdPaes3) +
066700000000     C                              '</Nazione>'
066800000000     C                   EXSR      WrtXML
066900000000     C*
067000000000     C* 2.1.2.5 </StabileOrganizzazione>
067100000000     C                   Eval      DsOutput     =
067200000000     C                                      indent4 +
067300000000     C                              '</StabileOrganizzazione>'
067400000000     C                   EXSR      WrtXML
067500000000     C                   endif
067600000000     C*
067700000000     C* 2.1.2.6 <RappresentanteFiscale>
067800000000     C                   if        $IdPaes4    <> *blank
067900000000     C                   Eval      DsOutput     =
068000000000     C                                      indent4 +
068100000000     C                              '<RappresentanteFiscale>'
068200000000     C                   EXSR      WrtXML
068300000000     C*
068400000000     C* 2.1.2.6.1 <IdFiscaleIVA>
068500000000     C                   Eval      DsOutput     =
068600000000     C                                      indent5  +
068700000000     C                              '<IdFiscaleIVA>'
068800000000     C                   EXSR      WrtXML
068900000000     C*
069000000000     C* 2.1.2.6.1 <IdPaese>  </IdPaese>
069100000000     C                   Eval      DsOutput     =
069200000000     C                                      indent6  +
069300000000     C                              '<IdPaese>' +
069400000000     C                                $IdPaes4  +
069500000000     C                              '</IdPaese>'
069600000000     C                   EXSR      WrtXML
069700000000     C*
069800000000     C* 2.1.2.6.2 <IdCodice>  </IdCodice>
069900000000     C                   Eval      DsOutput     =
070000000000     C                                      indent6  +
070100000000     C                              '<IdCodice>'     +
070200000000     C                                %trim($Piva4) +
070300000000     C                              '</IdCodice>'
070400000000     C                   EXSR      WrtXML
070500000000     C*
070600000000     C* 2.1.2.6.1 </IdFiscaleIVA>
070700000000     C                   Eval      DsOutput     =
070800000000     C                                      indent5  +
070900000000     C                              '</IdFiscaleIVA>'
071000000000     C                   EXSR      WrtXML
071100000000     C*
071200000000     C* 2.1.2.6.2 <Denominazione>
071300000000     C                   if        $Denom4  <> *blank
071400000000     C                   Eval      DsOutput     =
071500000000     C                                      indent5 +
071600000000     C                              '<Denominazione>' +
071700000000     C                                %trim($Denom4) +
071800000000     C                              '</Denominazione>'
071900000000     C                   EXSR      WrtXML
072000000000     C                   else
072100000000     C*
072200000000     C* 2.1.2.6.3 <Nome>
072300000000     C                   if        $Nome4         <> *blank
072400000000     C                   Eval      DsOutput     =
072500000000     C                                      indent5 +
072600000000     C                              '<Nome>' +
072700000000     C                                %trim($Nome4) +
072800000000     C                              '</Nome>'
072900000000     C                   EXSR      WrtXML
073000000000     C*
073100000000     C* 2.1.2.6.4 <Cognome>
073200000000     C                   Eval      DsOutput     =
073300000000     C                                      indent5 +
073400000000     C                              '<Cognome>' +
073500000000     C                                %trim($Cognom4) +
073600000000     C                              '</Cognome>'
073700000000     C                   EXSR      WrtXML
073800000000     C                   endif
073900000000     C                   endif
074000000000     C*
074100000000     C* 2.1.2.6 </RappresentanteFiscale>
074200000000     C                   Eval      DsOutput     =
074300000000     C                                      indent4 +
074400000000     C                              '</RappresentanteFiscale>'
074500000000     C                   EXSR      WrtXML
074600000000     C                   endif
074700000000     C*
074800000000     C* 2.1.2 </AltriDatiIdentificativi>
074900000000     C                   Eval      DsOutput     =
075000000000     C                                      indent3 +
075100000000     C                              '</AltriDatiIdentificativi>'
075200000000     C                   EXSR      WrtXML
075300000000     C*
075400000000     C                   ENDSR
075500000000     C/EJECT
075600000000     C******************************************************
075700000000     C* Scrivi TAG FRONTESPIZIO
075800000000     C******************************************************
075900000000     C     CicloSql      BEGSR
076000000000     C*
076100000000     C                   EXSR      RepSqlCST
076200000000     C*
076300000000     C                   EXSR      OpenSqlCST
076400000000     C*
076500000000     C                   EXSR      ExeSqlCST
076600000000     C*
076700000000     C                   EXSR      CloseSql
076800000000     C*
076900000000     C                   ENDSR
077000000000     C/EJECT
077100000000     C************************************************************
077200000000     C* Reperimento stringa sql su SdgCst00f
077300000000     C************************************************************
077400000000     C     RepSqlCST     BEGSR
077500000000     C*
077600000000     C* SELECT rrn(SDGCST00F), sdgcst00f.* FROM SDGCST00F
077700000000     C* WHERE CSTSOCIETA ='XXX'
077800000000     C*  AND CSTTPCOM = 'T'
077900000000     C*  AND CSTTPREC = Tipo Record (O=Ordinari/R=Rettifica)
078000000000     C*  AND CSTAnno = AnnoI61
078100000000     C*  AND CSTTrim = TrimI61
078200000000     C*  AND CSTCF   = Sottonatura
078300000000     C*  [record non annullati]
078400000000     C*  AND CSTANN <> '1'
078500000000     C* -------------------------------------------------------------------
078600000000     C* ORDER BY CSTSOCIETA, CSTTPCOM, CSTCF, CSTANNO, CSTTRIM, CSTMESE,
078700000000     C* CSTIDPAES1, CSTPIVA1, CSTCDFISC1, CSTDENOM1,
078800000000     C* CSTCOGNOM1, CSTNOME1,
078900000000     C* CSTINDIR2, CSTNRCIV2, CSTCAP2, CSTLOCALI2, CSTPROV2, CSTIDPAES2,
079000000000     C* CSTINDIR3, CSTNRCIV3, CSTCAP3, CSTLOCALI3, CSTPROV3, CSTIDPAES3,
079100000000     C* CSTIDPAES4, CSTPIVA4, CSTDENOM4, CSTCOGNOM4, CSTNOME4,
079200000000     C* CSTDTDOC, CSTNRDOC, CSTSERDOC, CSTDTREG, CSTNRREG, CSTSERREG,
079300000000     C* CSTDTANNOT, CSTTPDOC,
079400000000     C* CSTALIQ, CSTRIFIVA, CSTMOTESCL, CSTPERCDET, CSTDEDUCIB, CSTESIGIB
079500000000     C*
079600000000     C*
079700000000     C                   Clear                   SqlCST
079800000000     C*
079900000000     C                   Eval      SqlCST =
080000000000     C                              'SELECT rrn(SDGCST00F), sdgcst00f.* ' +
080100000000     C                              'FROM SDGCST00F ' +
080200000000     C                             ' WHERE CSTSOCIETA=' + '''' +
080300000000     C                             SocietaI61 + ''''
080400000000     C*
080500000000     C                   Eval      SqlCST = %trimr(SqlCST)+
080600000000     C                             ' AND CSTCf     =' + '''' +
080700000000     C                             $Cf    + ''''
080800000000     C*
080900000000     C                   Eval      SqlCST = %trimr(SqlCST)+
081000000000     C                             ' AND CSTAnno   =' +
081100000000     C                             %char(AnnoI61)
081200000000     C*
081300000000     C                   Eval      SqlCST = %trimr(SqlCST)+
081400000000     C                             ' AND CSTTrim   =' +
081500000000     C                             %char(TrimI61)
081600000000     C*
081700000000     C                   Eval      SqlCST = %trimr(SqlCST)+
081800000000     C                             ' AND CSTAnn   <>' + '''1'''
081900000000     C*
082000000000     C                   Select
082100000000     C*
082200000000     C* invio ordinario: tipo record = 'O'
082300000000     C                   When      TipInvI61   = 'O'
082400000000     C                   Eval      SqlCST = %trimr(SqlCST)+
082500000000     C                             ' AND CSTTpRec  =' + '''' +
082600000000     C                             $TpRecOrd + ''''
082700000000     C*
082800000000     C* Rettifica: tipo record = 'R'
082900000000     C                   When      TipInvI61   = 'R'
083000000000     C                   Eval      SqlCST = %trimr(SqlCST)+
083100000000     C                             ' AND CSTTpRec  =' + '''' +
083200000000     C                             $TpRecRet + ''''
083300000000     C*
083400000000     C                   Eval      SqlCST = %trimr(SqlCST)+
083500000000     C                             ' AND CSTIdFileO =' + '''' +
083600000000     C                             IdFileI61 + ''''
083700000000     C*
083800000000     C                   if        PosizI61 <> 0
083900000000     C                   Eval      SqlCST = %trimr(SqlCST)+
084000000000     C                             ' AND CSTIdPosO  =' +
084100000000     C                             %char(PosizI61)
084200000000     C                   endif
084300000000     C*
084400000000     C**** annullamento: seleziono i record di tipo 'O'
084500000000     C****               When      TipInvI61   = 'A'
084600000000     C****               Eval      SqlCST = %trimr(SqlCST)+
084700000000     C****                         ' AND CSTTpRec  =' + '''' +
084800000000     C****                         $TpRecOrd + ''''
084900000000     C****
085000000000     C****               Eval      SqlCST = %trimr(SqlCST)+
085100000000     C****                         ' AND CSTIdFileO =' + '''' +
085200000000     C****                         IdFileI61 + ''''
085300000000     C****
085400000000     C****               if        PosizI61 <> 0
085500000000     C****               Eval      SqlCST = %trimr(SqlCST)+
085600000000     C****                         ' AND CSTIdPosO  =' +
085700000000     C****                         %char(PosizI61)
085800000000     C****               endif
085900000000     C                   endSl
086000000000     C*
086100000000     C                   Eval      SqlCST      = %trimr(SqlCST)+
086200000000     C                               ' ORDER BY '          +
086300000000     C                               ' CSTSOCIETA, CSTTPCOM, CSTCF, ' +
086400000000     C                               ' CSTANNO, CSTTRIM, CSTMESE,   ' +
086500000000     C                               ' CSTIDPAES1, CSTPIVA1, CSTCDFISC1,  ' +
086600000000     C                               ' CSTDENOM1, '                         +
086700000000     C                               ' CSTCOGNOM1, CSTNOME1, '              +
086800000000     C                               ' CSTINDIR2, '                         +
086900000000     C                               ' CSTNRCIV2, CSTCAP2, CSTLOCALI2, '    +
087000000000     C                               ' CSTPROV2, CSTIDPAES2, CSTINDIR3, '   +
087100000000     C                               ' CSTNRCIV3, CSTCAP3, CSTLOCALI3, '    +
087200000000     C                               ' CSTPROV3, CSTIDPAES3, CSTIDPAES4, '  +
087300000000     C                               ' CSTPIVA4, '                          +
087400000000     C                               ' CSTDENOM4, '                         +
087500000000     C                               ' CSTCOGNOM4, CSTNOME4, '              +
087600000000     C                               ' CSTDTDOC, CSTNRDOC, CSTSERDOC, '     +
087700000000     C                               ' CSTDTREG, CSTNRREG, CSTSERREG, '     +
087800000000     C                               ' CSTDTANNOT,'                         +
087900000000     C                               ' CSTTPDOC, '                          +
088000000000     C                               ' CSTALIQ, CSTRIFIVA, CSTMOTESCL, '    +
088100000000     C                               ' CSTPERCDET, CSTDEDUCIB, CSTESIGIB'
088200000000     C*
088300000000     C* For Fetch Only
088400000000     C                   Eval      SqlCST      = %trimr(SqlCST)+
088500000000     C                               ' FOR FETCH ONLY'
088600000000     C*
088700000000     C                   ENDSR
088800000000     C/EJECT
088900000000     C******************************************************
089000000000     C* Open cursore sql
089100000000     C******************************************************
089200000000     C     OpenSqlCST    BEGSR
089300000000     C*
089400000000     C/EXEC SQL
089500000000     C+     PREPARE S1 FROM :SqlCST
089600000000     C/END-EXEC
089700000000     C*
089800000000     C/EXEC SQL
089900000000     C+     Declare A1 cursor for S1
090000000000     C/END-EXEC
090100000000     C*
090200000000     C/EXEC SQL
090300000000     C+     OPEN A1
090400000000     C/END-EXEC
090500000000     C*
090600000000     C                   ENDSR
090700000000     C/EJECT
090800000000     C************************************************************
090900000000     C* Eseguo lettura dei movimenti
091000000000     C************************************************************
091100000000     C     ExeSqlCST     BEGSR
091200000000     C*
091300000000     C                   Move      *Off          $FineCST
091400000000     C                   eval      RigaMod  = 0
091500000000     C                   clear                   $PosizO
091600170911     C                   z-add     0             $MaxRighe
091700000000     C                   z-add     0             $MaxNrDoc
091800000000     C                   clear                   w_ImpOpe
091900000000     C                   clear                   w_IvaOpe
092000000000     C*
092100000000     C                   eval      $RotturaCF  = *off
092200000000     C                   eval      $RotturaDoc = *off
092300000000     C                   eval      $Errore     = *off
092400000000     C*
092500000000     C                   DoW       $FineCST    = *Off
092600000000     C*
092700000000     C                   EXSR      FetchCST
092800000000     C*
092900000000     C                   Select
093000000000     C*-----------------------
093100000000     C* OK
093200000000     C                   When      SqlCod     >= 0     and
093300000000     C                             SqlCod     <> 100
093400000000     C*
093500000000     C                   if        IdFileI61  =  *blank  and
093600000000     C                             PosizI61   =  0       and
093700000000     C                             (CstIdFileO <> *blank or
093800000000     C                              CstIdFileR <> *blank or
093900000000     C                              CstIdFileA <> *blank)
094000000000     C                   iter
094100000000     C                   endif
094200000000     C*
094300000000     C                   eval      RigaMod  = RigaMod + 1
094400000000     C*
094500000000     C* Scrivo TAG Intestazione e Frontespizio
094600000000     C                   if        RigaMod     = 1
094700000000     C                   eval      SavCstDs    = SdgCstDs
094800000000     C                   exsr      WrtIntestaz
094900000000     C                   endif
095000000000     C*
095100000000     C* Dati anagrafici cliente/fornitore
095200000000     C                   exsr      DatiAnaCF
095300000000     C*
095400000000     C* Intestazione dati fattura
095500000000     C                   if        $RotturaCF  = *off  or
095600000000     C                             RigaMod     = 1
095700000000     C                   exsr      DatiFattura
095800000000     C                   endif
095900000000     C*
096000000000     C* Dettaglio dati fattura
096100000000     C                   exsr      ElabCST
096200000000     C*
096300000000     C                   exsr      UpdateCst
096400000000     C*
096500000000     C                   eval      Sv1CstDs  = SdgCstDs
096600000000     C                   eval      Sv2CstDs  = SdgCstDs
096700000000R264 C                   eval      Sv3CstDs  = SdgCstDs
096800000000     C*-----------------------
096900000000     C* fine lettura
097000000000     C                   When      SqlCod      = 100
097100000000     C*
097200000000     C                   if        RigaMod     >= 1
097300000000     C                             or $RotturaDoc = *on
097400000000     C*
097500000000     C                   exsr      UpdateCst
097600000000     C*
097700000000     C* Chiude Tag DatiRiepilogo
097800000000R264 C                   exsr      AccorpaDati
097900000000     C                   exsr      DatiRiepilogo
098000000000     C* Chiude Tag Dati fattura
098100000000     C                   exsr      CloseTagFat
098200000000     C* Chiude Tag Dati anagrafici
098300000000     C                   exsr      CloseTagAna
098400000000     C* Eventuale tag di rettifica
098500000000     C                   exsr      DatiRettifica
098600000000     C* Chiude Tag DTE/DTR
098700000000     C                   exsr      CloseTagDT
098800000000     C* Chiude File
098900000000     C                   Eval      DsOutput     =
099000000000     C                             '</ns2:DatiFattura>'
099100000000     C                   EXSR      WrtXML
099200000000     C                   endif
099300000000     C*
099400000000     C                   Move      *On           $FineCST
099500000000     C*
099600000000     C                   Other
099700000000     C*
099800000000     C*-----------------------
099900000000     C* si è verificato un errore
100000000000     C***                Eval      $TpErr1     = '99'
100100000000     C***                Eval      StringaSql  = SqlCST
100200000000     C***                EXSR      StpErr1
100300000000     C                   Eval      ERRI61      = '1'
100400000000     C*
100500000000     C                   Move      *On           $FineCST
100600000000     C                   endsl
100700000000     C*
100800000000     C                   endDo
100900000000     C*
101000000000     C                   ENDSR
101100000000     C/EJECT
101200000000     C******************************************************
101300000000     C* Eseguo lettura
101400000000     C******************************************************
101500000000     C     FetchCST      BEGSR
101600000000     C*
101700000000     C/EXEC SQL
101800000000     C+     FETCH A1 INTO :RRNCst, :SdgCstDs
101900000000     C/END-EXEC
102000000000     C*
102100000000     C                   ENDSR
102200000000     C/EJECT
102300000000     C********************************************************************
102400000000     C* Dati anagrafici cliente/fornitore
102500000000     C********************************************************************
102600000000     C     DatiAnaCF     BEGSR
102700000000     C*
102800000000     C                   if        CstSOCIETA <> Sv1SOCIETA or
102900000000     C                             CstTPCOM   <> Sv1TPCOM   or
103000000000     C                             CstCF      <> Sv1CF      or
103100000000     C                             CstANNO    <> Sv1ANNO    or
103200000000     C                             CstTRIM    <> Sv1TRIM    or
103300000000     C                             CstMESE    <> Sv1MESE    or
103400000000     C                             CstIDPAES1 <> Sv1IDPAES1 or
103500000000     C                             CstPIVA1   <> Sv1PIVA1   or
103600000000     C                             CstCDFISC1 <> Sv1CDFISC1 or
103700000000     C                             CstDENOM1  <> Sv1DENOM1  or
103800000000     C                             CstCOGNOM1 <> Sv1COGNOM1 or
103900000000     C                             CstNOME1   <> Sv1NOME1   or
104000000000     C                             CstINDIR2  <> Sv1INDIR2  or
104100000000     C                             CstNRCIV2  <> Sv1NRCIV2  or
104200000000     C                             CstCAP2    <> Sv1CAP2    or
104300000000     C                             CstLOCALI2 <> Sv1LOCALI2 or
104400000000     C                             CstPROV2   <> Sv1PROV2   or
104500000000     C                             CstIDPAES2 <> Sv1IDPAES2 or
104600000000     C                             CstINDIR3  <> Sv1INDIR3  or
104700000000     C                             CstNRCIV3  <> Sv1NRCIV3  or
104800000000     C                             CstCAP3    <> Sv1CAP3    or
104900000000     C                             CstLOCALI3 <> Sv1LOCALI3 or
105000000000     C                             CstPROV3   <> Sv1PROV3   or
105100000000     C                             CstIDPAES3 <> Sv1IDPAES3 or
105200000000     C                             CstIDPAES4 <> Sv1IDPAES4 or
105300000000     C                             CstPIVA4   <> Sv1PIVA4   or
105400000000     C                             CstDENOM4  <> Sv1DENOM4  or
105500000000     C                             CstCOGNOM4 <> Sv1COGNOM4 or
105600000000     C                             CstNOME4   <> Sv1NOME4
105700000000     C*
105800000000     C                   eval      $RotturaCF  = *on
105900000000     C*
106000000000     C                   z-add     0             $MaxNrDoc
106100000000     C                   eval      $MaxRighe = $MaxRighe  + 1
106200000000     C*
106300000000     C                   if        RigaMod > 1
106400000000     C*
106500000000R264 C                   exsr      AccorpaDati
106600000000     C                   exsr      DatiRiepilogo
106700000000     C*
106800000000     C                   exsr      DatiFattura
106900000000     C*
107000000000     C* Chiude Tag Dati anagrafici
107100000000     C                   exsr      CloseTagAna
107200000000     C* Eventuale tag di rettifica
107300000000     C                   exsr      DatiRettifica
107400000000     C*
107500000000     C                   endif
107600000000     C*
107700000000     C* Superato il numero massimo di clienti per singolo file,
107800000000     C* chiudo file, copio in cartella IFS, stacco nuovo progressivo
107900000000     C                   if        $MaxRighe >  MaxRecI61
108000000000     C                             and RigaMod > 1
108100000000     C                   Exsr      GesMultInv
108200000000     C                   Eval      $MultiInvio  = *On
108300000000     C                   endif
108400000000     C*
108500000000     C* Apre   Tag Dati anagrafici
108600000000     C                   exsr      OpenTagAna
108700000000     C*
108800000000     C                   eval      RigaMod  = 1
108900000000     C*
109000000000     C                   else
109100000000     C                   eval      $RotturaCF  = *off
109200000000     C                   endif
109300000000     C*
109400000000     C                   ENDSR
109500000000     C/EJECT
109600000000     C******************************************************
109700000000     C* Apertura TAG dati anagrafici cli/for
109800000000     C******************************************************
109900000000     C     OpenTagAna    BEGSR
110000000000     C*
110100000000     C                   select
110200000000     C                   when      $Cf   = 'C'
110300000000     C*
110400000000     C* 2.2 <CessionarioCommittenteDTE>
110500000000     C                   Eval      DsOutput     =
110600000000     C                                      indent2 +
110700000000     C                              '<CessionarioCommittenteDTE>'
110800000000     C                   EXSR      WrtXML
110900000000     C*
111000000000     C                   when      $Cf   = 'F'
111100000000     C*
111200000000     C* 3.2 CedentePrestatoreDTR
111300000000     C                   Eval      DsOutput     =
111400000000     C                                      indent2 +
111500000000     C                              '<CedentePrestatoreDTR>'
111600000000     C                   EXSR      WrtXML
111700000000     C                   endsl
111800000000     C*
111900000000     C* Dettaglio dati anagrafici cli/for
112000000000     C                   EXSR      DettAnaCF
112100000000     C*
112200000000     C                   ENDSR
112300000000     C/EJECT
112400000000     C******************************************************
112500000000     C* Dettaglio dati anagrafici cli/for
112600000000     C******************************************************
112700000000     C     DettAnaCF     BEGSR
112800000000     C*
112900000000     C* 2.2.1 <IdentificativiFiscali>
113000000000     C* 3.2.1 <IdentificativiFiscali>
113100000000     C                   Eval      DsOutput     =
113200000000     C                                      indent3 +
113300000000     C                              '<IdentificativiFiscali>'
113400000000     C                   EXSR      WrtXML
113500000000     C*
113600000000     C*  2.2.1.1 <IdFiscaleIVA>
113700000000     C*  3.2.1.1 <IdFiscaleIVA>
113800000000     C*   - in alternativa al codice fiscale
113900000000R264 X****               if        CstPiva1 <> *blank
114000000000     C                   Eval      DsOutput     =
114100000000     C                                      indent4  +
114200000000     C                              '<IdFiscaleIVA>'
114300000000     C                   EXSR      WrtXML
114400000000     C*
114500000000     C*   2.2.1.1.1 <IdPaese>  </IdPaese>
114600000000     C*   3.2.1.1.1 <IdPaese>  </IdPaese>
114700000000     C                   Eval      DsOutput     =
114800000000     C                                      indent5  +
114900000000     C                              '<IdPaese>' +
115000000000     C                                %trim(CstIdPaes1) +
115100000000     C                              '</IdPaese>'
115200000000     C                   EXSR      WrtXML
115300000000     C*
115400000000     C*   2.2.1.1.2 <IdCodice>  </IdCodice>
115500000000     C*   3.2.1.1.2 <IdCodice>  </IdCodice>
115600000000R264 C                   if        CstPiva1 <> *blank
115700000000     C                   Eval      DsOutput     =
115800000000     C                                      indent5  +
115900000000     C                              '<IdCodice>'     +
116000000000     C                                %trim(CstPIva1) +
116100000000     C                              '</IdCodice>'
116200000000     C                   EXSR      WrtXML
116300000000R264 C                   else
116400000000 "   C* In mancanza della partita iva, è necessaria la
116500000000 "   C* presenza del tag IdCodice con almeno un carattere
116600000000 "   C* blank, pena il rifiuto del file.
116700000000 "   C                   Eval      DsOutput     =
116800000000 "   C                                      indent5  +
116900000000 "   C                              '<IdCodice>'     +
117000000000 "   C                                ' '             +
117100000000 "   C                              '</IdCodice>'
117200000000 "   C                   EXSR      WrtXML
117300000000R264 C                   endif
117400000000     C*
117500000000     C*  2.2.1.1 </IdFiscaleIVA>
117600000000     C*  3.2.1.1 </IdFiscaleIVA>
117700000000     C                   Eval      DsOutput     =
117800000000     C                                      indent4  +
117900000000     C                              '</IdFiscaleIVA>'
118000000000     C                   EXSR      WrtXML
118100000000     C*
118200000000R264 X*****              else
118300000000     C*  2.2.1.2 <CodiceFiscale> (facoltativo)
118400000000     C*  3.2.1.2 <CodiceFiscale> (facoltativo)
118500000000     C                   if        CstCdFisc1 <> *blank
118600000000     C                   Eval      DsOutput     =
118700000000     C                                      indent4  +
118800000000     C                              '<CodiceFiscale>' +
118900000000     C                                %trim(CstCdFisc1) +
119000000000     C                              '</CodiceFiscale>'
119100000000     C                   EXSR      WrtXML
119200000000     C                   endif
119300000000R264 X*****              endif
119400000000     C*
119500000000     C* 2.2.1 </IdentificativiFiscali>
119600000000     C* 3.2.1 </IdentificativiFiscali>
119700000000     C                   Eval      DsOutput     =
119800000000     C                                      indent3 +
119900000000     C                              '</IdentificativiFiscali>'
120000000000     C                   EXSR      WrtXML
120100000000     C*
120200000000     C* 2.2.2 <AltriDatiIdentificativi>
120300000000     C* 3.2.2 <AltriDatiIdentificativi>
120400000000     C                   Eval      DsOutput     =
120500000000     C                                      indent3 +
120600000000     C                              '<AltriDatiIdentificativi>'
120700000000     C                   EXSR      WrtXML
120800000000     C*
120900000000     C*  2.2.2.1 <Denominazione>
121000000000     C*  3.2.2.1 <Denominazione>
121100000000     C                   if        CstDenom1 <> *blank
121200000000     C                   Eval      DsOutput     =
121300000000     C                                      indent4 +
121400000000     C                              '<Denominazione>' +
121500000000     C                                %trim(CstDenom1) +
121600000000     C                              '</Denominazione>'
121700000000     C                   EXSR      WrtXML
121800000000     C                   else
121900000000     C*  2.2.2.2 <Nome>
122000000000     C*  3.2.2.2 <Nome>
122100000000     C                   if        CstNome1       <> *blank
122200000000     C                   Eval      DsOutput     =
122300000000     C                                      indent4 +
122400000000     C                              '<Nome>' +
122500000000     C                                %trim(CstNome1) +
122600000000     C                              '</Nome>'
122700000000     C                   EXSR      WrtXML
122800000000     C*  2.2.2.3 <Cognome>
122900000000     C*  3.2.2.3 <Cognome>
123000000000     C                   Eval      DsOutput     =
123100000000     C                                      indent4 +
123200000000     C                              '<Cognome>' +
123300000000     C                                %trim(CstCognom1) +
123400000000     C                              '</Cognome>'
123500000000     C                   EXSR      WrtXML
123600000000     C                   endif
123700000000     C                   endif
123800000000     C*
123900000000     C*  2.2.2.4 <Sede>
124000000000     C*  3.2.2.4 <Sede>
124100000000     C                   Eval      DsOutput     =
124200000000     C                                      indent4 +
124300000000     C                              '<Sede>'
124400000000     C                   EXSR      WrtXML
124500000000     C*
124600000000     C*   2.2.2.4.1 <Indirizzo>
124700000000     C*   3.2.2.4.1 <Indirizzo>
124800000000     C                   Eval      DsOutput     =
124900000000     C                                      indent5 +
125000000000     C                              '<Indirizzo>' +
125100000000     C                                %trim(CstIndir2) +
125200000000     C                              '</Indirizzo>'
125300000000     C                   EXSR      WrtXML
125400000000     C*
125500000000     C*   2.2.2.4.2 <NumeroCivico>
125600000000     C*   3.2.2.4.2 <NumeroCivico>
125700000000     C                   if        CstNrCiv2 <> *blank
125800000000     C                   Eval      DsOutput     =
125900000000     C                                      indent5 +
126000000000     C                              '<NumeroCivico>' +
126100000000     C                                %trim(CstNrCiv2) +
126200000000     C                              '</NumeroCivico>'
126300000000     C                   EXSR      WrtXML
126400000000     C                   endif
126500000000     C*
126600000000     C*   2.2.2.4.3 <CAP>
126700000000     C*   3.2.2.4.3 <CAP>
126800000000     C                   if        CstCap2 <> *blank
126900000000R264 C                             and CstIdPaes2 = 'IT'
127000000000     C                   Eval      DsOutput     =
127100000000     C                                      indent5 +
127200000000     C                              '<CAP>' +
127300000000     C                                %trim(CstCap2) +
127400000000     C                              '</CAP>'
127500000000     C                   EXSR      WrtXML
127600000000     C                   endif
127700000000     C*
127800000000     C*   2.2.2.4.4 <Comune>
127900000000     C*   3.2.2.4.4 <Comune>
128000000000     C                   Eval      DsOutput     =
128100000000     C                                      indent5 +
128200000000     C                              '<Comune>' +
128300000000     C                                %trim(CstLocali2) +
128400000000     C                              '</Comune>'
128500000000     C                   EXSR      WrtXML
128600000000     C*
128700000000     C*   2.2.2.4.5 <Provincia>
128800000000     C*   3.2.2.4.5 <Provincia>
128900000000     C                   if        CstIdPaes2 = 'IT'
129000000000     C                   Eval      DsOutput     =
129100000000     C                                      indent5 +
129200000000     C                              '<Provincia>' +
129300000000     C                                %trim(CstProv2) +
129400000000     C                              '</Provincia>'
129500000000     C                   EXSR      WrtXML
129600000000     C                   endif
129700000000     C*
129800000000     C*   2.2.2.4.6 <Nazione>
129900000000     C*   3.2.2.4.6 <Nazione>
130000000000     C                   Eval      DsOutput     =
130100000000     C                                      indent5 +
130200000000     C                              '<Nazione>' +
130300000000     C                                %trim(CstIdPaes2) +
130400000000     C                              '</Nazione>'
130500000000     C                   EXSR      WrtXML
130600000000     C*
130700000000     C*  2.2.2.4 </Sede>
130800000000     C*  3.2.2.4 </Sede>
130900000000     C                   Eval      DsOutput     =
131000000000     C                                      indent4 +
131100000000     C                              '</Sede>'
131200000000     C                   EXSR      WrtXML
131300000000     C*
131400000000     C*  2.2.2.5 <StabileOrganizzazione>
131500000000     C*  3.2.2.5 <StabileOrganizzazione>
131600000000     C                   if        CstIndir3 <> *blank
131700000000     C                   Eval      DsOutput     =
131800000000     C                                      indent4 +
131900000000     C                              '<StabileOrganizzazione>'
132000000000     C                   EXSR      WrtXML
132100000000     C*
132200000000     C*  2.2.2.5.1 <Indirizzo>
132300000000     C*  3.2.2.5.1 <Indirizzo>
132400000000     C                   Eval      DsOutput     =
132500000000     C                                      indent5 +
132600000000     C                              '<Indirizzo>' +
132700000000     C                                %trim(CstIndir3) +
132800000000     C                              '</Indirizzo>'
132900000000     C                   EXSR      WrtXML
133000000000     C*
133100000000     C*  2.2.2.5.2 <NumeroCivico>
133200000000     C*  3.2.2.5.2 <NumeroCivico>
133300000000     C                   if        CstNrCiv3 <> *blank
133400000000     C                   Eval      DsOutput     =
133500000000     C                                      indent5 +
133600000000     C                              '<NumeroCivico>' +
133700000000     C                                %trim(CstNrCiv3) +
133800000000     C                              '</NumeroCivico>'
133900000000     C                   EXSR      WrtXML
134000000000     C                   endif
134100000000     C*
134200000000     C*  2.2.2.5.3 <CAP>
134300000000     C*  3.2.2.5.3 <CAP>
134400000000     C                   Eval      DsOutput     =
134500000000     C                                      indent5 +
134600000000     C                              '<CAP>' +
134700000000     C                                %trim(CstCap3) +
134800000000     C                              '</CAP>'
134900000000     C                   EXSR      WrtXML
135000000000     C*
135100000000     C*  2.2.2.5.4 <Comune>
135200000000     C*  3.2.2.5.4 <Comune>
135300000000     C                   Eval      DsOutput     =
135400000000     C                                      indent5 +
135500000000     C                              '<Comune>' +
135600000000     C                                %trim(CstLocali3) +
135700000000     C                              '</Comune>'
135800000000     C                   EXSR      WrtXML
135900000000     C*
136000000000     C*  2.2.2.5.5 <Provincia>
136100000000     C*  3.2.2.5.5 <Provincia>
136200000000     C                   if        CstIdPaes3 = 'IT'
136300000000     C                   Eval      DsOutput     =
136400000000     C                                      indent5 +
136500000000     C                              '<Provincia>' +
136600000000     C                                %trim(CstProv3) +
136700000000     C                              '</Provincia>'
136800000000     C                   EXSR      WrtXML
136900000000     C                   endif
137000000000     C*
137100000000     C*  2.2.2.4.6 <Nazione>
137200000000     C*  3.2.2.4.6 <Nazione>
137300000000     C                   Eval      DsOutput     =
137400000000     C                                      indent5 +
137500000000     C                              '<Nazione>' +
137600000000     C                                %trim(CstIdPaes3) +
137700000000     C                              '</Nazione>'
137800000000     C                   EXSR      WrtXML
137900000000     C*
138000000000     C*  2.2.2.5 </StabileOrganizzazione>
138100000000     C*  3.2.2.5 </StabileOrganizzazione>
138200000000     C                   Eval      DsOutput     =
138300000000     C                                      indent4 +
138400000000     C                              '</StabileOrganizzazione>'
138500000000     C                   EXSR      WrtXML
138600000000     C                   endif
138700000000     C*
138800000000     C*  2.2.2.6 <RappresentanteFiscale>
138900000000     C*  3.2.2.6 <RappresentanteFiscale>
139000000000     C                   if        CstPiva4    <> *blank
139100000000     C                   Eval      DsOutput     =
139200000000     C                                      indent4 +
139300000000     C                              '<RappresentanteFiscale>'
139400000000     C                   EXSR      WrtXML
139500000000     C*
139600000000     C*   2.2.2.6.1 <IdFiscaleIVA>
139700000000     C*   3.2.2.6.1 <IdFiscaleIVA>
139800000000     C                   Eval      DsOutput     =
139900000000     C                                      indent5  +
140000000000     C                              '<IdFiscaleIVA>'
140100000000     C                   EXSR      WrtXML
140200000000     C*
140300000000     C*    2.2.2.6.1 <IdPaese>  </IdPaese>
140400000000     C*    3.2.2.6.1 <IdPaese>  </IdPaese>
140500000000     C                   Eval      DsOutput     =
140600000000     C                                      indent6  +
140700000000     C                              '<IdPaese>' +
140800000000     C                                %trim(CstIdPaes4) +
140900000000     C                              '</IdPaese>'
141000000000     C                   EXSR      WrtXML
141100000000     C*
141200000000     C*    2.2.2.6.2 <IdCodice>  </IdCodice>
141300000000     C*    3.2.2.6.2 <IdCodice>  </IdCodice>
141400000000     C                   Eval      DsOutput     =
141500000000     C                                      indent6  +
141600000000     C                              '<IdCodice>'     +
141700000000     C                                %trim(CstPIva4) +
141800000000     C                              '</IdCodice>'
141900000000     C                   EXSR      WrtXML
142000000000     C*
142100000000     C*  2.2.2.6.1 </IdFiscaleIVA>
142200000000     C*  3.2.2.6.1 </IdFiscaleIVA>
142300000000     C                   Eval      DsOutput     =
142400000000     C                                      indent5  +
142500000000     C                              '</IdFiscaleIVA>'
142600000000     C                   EXSR      WrtXML
142700000000     C*
142800000000     C*  2.2.2.6.2 <Denominazione>
142900000000     C*  3.2.2.6.2 <Denominazione>
143000000000     C                   if        CstDenom4  <> *blank
143100000000     C                   Eval      DsOutput     =
143200000000     C                                      indent5 +
143300000000     C                              '<Denominazione>' +
143400000000     C                                %trim(CstDenom4) +
143500000000     C                              '</Denominazione>'
143600000000     C                   EXSR      WrtXML
143700000000     C                   else
143800000000     C*
143900000000     C*  2.2.2.6.3 <Nome>
144000000000     C*  3.2.2.6.3 <Nome>
144100000000     C                   if        CstNome4     <> *blank
144200000000     C                   Eval      DsOutput     =
144300000000     C                                      indent5 +
144400000000     C                              '<Nome>' +
144500000000     C                                %trim(CstNome4) +
144600000000     C                              '</Nome>'
144700000000     C                   EXSR      WrtXML
144800000000     C*
144900000000     C*  2.2.2.6.4 <Cognome>
145000000000     C*  3.2.2.6.4 <Cognome>
145100000000     C                   Eval      DsOutput     =
145200000000     C                                      indent5 +
145300000000     C                              '<Cognome>' +
145400000000     C                                %trim(CstCognom4) +
145500000000     C                              '</Cognome>'
145600000000     C                   EXSR      WrtXML
145700000000     C                   endif
145800000000     C                   endif
145900000000     C*
146000000000     C*  2.2.2.6 </RappresentanteFiscale>
146100000000     C*  3.2.2.6 </RappresentanteFiscale>
146200000000     C                   Eval      DsOutput     =
146300000000     C                                      indent4 +
146400000000     C                              '</RappresentanteFiscale>'
146500000000     C                   EXSR      WrtXML
146600000000     C                   endif
146700000000     C*
146800000000     C* 2.2.2 </AltriDatiIdentificativi>
146900000000     C* 3.2.2 </AltriDatiIdentificativi>
147000000000     C                   Eval      DsOutput     =
147100000000     C                                      indent3 +
147200000000     C                              '</AltriDatiIdentificativi>'
147300000000     C                   EXSR      WrtXML
147400000000     C*
147500000000     C                   ENDSR
147600000000     C/EJECT
147700000000     C******************************************************
147800000000     C* Chiusura TAG dati anagrafici cli/for
147900000000     C******************************************************
148000000000     C     CloseTagAna   BEGSR
148100000000     C*
148200000000     C                   select
148300000000     C                   when      $Cf   = 'C'
148400000000     C*
148500000000     C* 2.2 </CessionarioCommittenteDTE>
148600000000     C                   Eval      DsOutput     =
148700000000     C                                      indent2 +
148800000000     C                              '</CessionarioCommittenteDTE>'
148900000000     C                   EXSR      WrtXML
149000000000     C*
149100000000     C                   when      $Cf   = 'F'
149200000000     C*
149300000000     C* 3.2 CedentePrestatoreDTR
149400000000     C                   Eval      DsOutput     =
149500000000     C                                      indent2 +
149600000000     C                              '</CedentePrestatoreDTR>'
149700000000     C                   EXSR      WrtXML
149800000000     C                   endsl
149900000000     C*
150000000000     C                   ENDSR
150100000000     C/EJECT
150200000000     C********************************************************************
150300000000     C* Intestazione dati fattura
150400000000     C********************************************************************
150500000000     C     DatiFattura   BEGSR
150600000000     C*
150700000000     C                   if        CstSOCIETA <> Sv2SOCIETA or
150800000000     C                             CstTPCOM   <> Sv2TPCOM   or
150900000000     C                             CstCF      <> Sv2CF      or
151000000000     C                             CstANNO    <> Sv2ANNO    or
151100000000     C                             CstTRIM    <> Sv2TRIM    or
151200000000     C                             CstMESE    <> Sv2MESE    or
151300000000     C                             CstIDPAES1 <> Sv2IDPAES1 or
151400000000     C                             CstPIVA1   <> Sv2PIVA1   or
151500000000     C                             CstCDFISC1 <> Sv2CDFISC1 or
151600000000     C                             CstDENOM1  <> Sv2DENOM1  or
151700000000     C                             CstCOGNOM1 <> Sv2COGNOM1 or
151800000000     C                             CstNOME1   <> Sv2NOME1   or
151900000000     C                             CstINDIR2  <> Sv2INDIR2  or
152000000000     C                             CstNRCIV2  <> Sv2NRCIV2  or
152100000000     C                             CstCAP2    <> Sv2CAP2    or
152200000000     C                             CstLOCALI2 <> Sv2LOCALI2 or
152300000000     C                             CstPROV2   <> Sv2PROV2   or
152400000000     C                             CstIDPAES2 <> Sv2IDPAES2 or
152500000000     C                             CstINDIR3  <> Sv2INDIR3  or
152600000000     C                             CstNRCIV3  <> Sv2NRCIV3  or
152700000000     C                             CstCAP3    <> Sv2CAP3    or
152800000000     C                             CstLOCALI3 <> Sv2LOCALI3 or
152900000000     C                             CstPROV3   <> Sv2PROV3   or
153000000000     C                             CstIDPAES3 <> Sv2IDPAES3 or
153100000000     C                             CstIDPAES4 <> Sv2IDPAES4 or
153200000000     C                             CstPIVA4   <> Sv2PIVA4   or
153300000000     C                             CstDENOM4  <> Sv2DENOM4  or
153400000000     C                             CstCOGNOM4 <> Sv2COGNOM4 or
153500000000     C                             CstNOME4   <> Sv2NOME4   or
153600000000     C                             CstDTDOC   <> Sv2DTDOC   or
153700000000     C                             CstNRDOC   <> Sv2NRDOC   or
153800000000     C                             CstSERDOC  <> Sv2SERDOC  or
153900000000     C                             CstDTREG   <> Sv2DTREG   or
154000000000     C                             CstNRREG   <> Sv2NRREG   or
154100000000     C                             CstSERREG  <> Sv2SERREG  or
154200000000     C                             CstDTANNOT <> Sv2DTANNOT
154300000000R264 X***                          CstTPDOC   <> Sv2TPDOC   or
154400000000  "  X***                          CstALIQ    <> Sv2ALIQ    or
154500000000  "  X***                          CstRIFIVA  <> Sv2RIFIVA  or
154600000000  "  X***                          CstMOTESCL <> Sv2MOTESCL or
154700000000  "  X***                          CstPERCDET <> Sv2PERCDET or
154800000000  "  X***                          CstDEDUCIB <> Sv2DEDUCIB or
154900000000R264 X***                          CstESIGIB  <> Sv2ESIGIB
155000000000     C*
155100000000     C                   eval      $RotturaDoc = *on
155200000000     C*
155300000000     C                   if        RigaMod > 1
155400000000     C*
155500000000     C                   if        $RotturaCF  = *off
155600000000R264 C                   exsr      AccorpaDati
155700000000     C                   exsr      DatiRiepilogo
155800000000     C                   endif
155900000000     C*
156000000000     C* Chiude Tag Dati Fattura
156100000000     C                   exsr      CloseTagFat
156200000000     C*
156300000000     C                   clear                   w_ImpOpe
156400000000     C                   clear                   w_IvaOpe
156500000000     C                   endif
156600000000     C*
156700000000     C* Apre   Tag Dati fattura
156800000000     C                   if        $RotturaCF  = *off or
156900000000     C                             RigaMod     = 1
157000000000     C*
157100000000     C                   eval      $MaxNrDoc  = $MaxNrDoc  + 1
157200000000     C*
157300000000     C* Raggiunto il numero massimo di documenti per cliente
157400000000     C                   if        $MaxNrDoc > cMaxNrDoc
157500000000     C                   z-add     1             $MaxNrDoc
157600000000     C* Chiude Tag Dati anagrafici
157700000000     C                   exsr      CloseTagAna
157800000000     C* Apre   Tag Dati anagrafici
157900000000     C                   exsr      OpenTagAna
158000000000     C                   endif
158100000000     C*
158200000000     C                   eval      $PosizO  = $PosizO  + 1
158300000000     C                   exsr      OpenTagFat
158400000000     C                   endif
158500000000     C*
158600000000     C                   else
158700000000     C                   eval      $RotturaDoc = *off
158800000000R264 C                   if        $RotturaCF  = *off
158900000000  "  C                   if        CstTPDOC   <> Sv3TPDOC   or
159000000000  "  C                             CstALIQ    <> Sv3ALIQ    or
159100000000  "  C****                         CstRIFIVA  <> Sv3RIFIVA  or
159200000000  "  C                             CstMOTESCL <> Sv3MOTESCL or
159300000000  "  C                             CstPERCDET <> Sv3PERCDET or
159400000000  "  C                             CstDEDUCIB <> Sv3DEDUCIB or
159500000000  "  C                             CstESIGIB  <> Sv3ESIGIB
159600000000  "  C                   exsr      AccorpaDati
159700000000  "  C                   exsr      DatiRiepilogo
159800000000  "  C                   endif
159900000000R264 C                   endif
160000000000     C                   endif
160100000000     C*
160200000000     C                   ENDSR
160300000000     C/EJECT
160400000000     C******************************************************
160500000000     C* Apertura TAG dati fattura cli/for
160600000000     C******************************************************
160700000000     C     OpenTagFat    BEGSR
160800000000     C*
160900000000     C                   select
161000000000     C                   when      $Cf   = 'C'
161100000000     C*
161200000000     C* 2.2.3 <DatiFatturaBodyDTE>
161300000000     C                   Eval      DsOutput     =
161400000000     C                                      indent3 +
161500000000     C                              '<DatiFatturaBodyDTE>'
161600000000     C                   EXSR      WrtXML
161700000000     C*
161800000000     C                   when      $Cf   = 'F'
161900000000     C*
162000000000     C* 3.2.3 <DatiFatturaBodyDTR>
162100000000     C                   Eval      DsOutput     =
162200000000     C                                      indent3 +
162300000000     C                              '<DatiFatturaBodyDTR>'
162400000000     C                   EXSR      WrtXML
162500000000     C                   endsl
162600000000     C*
162700000000     C* 2.2.3.1  <DatiGenerali>
162800000000     C* 3.2.3.1  <DatiGenerali>
162900000000     C                   Eval      DsOutput     =
163000000000     C                                      indent4 +
163100000000     C                              '<DatiGenerali>'
163200000000     C                   EXSR      WrtXML
163300000000     C*
163400000000     C*  2.2.3.1.1  <TipoDocumento>
163500000000     C*  3.2.3.1.1  <TipoDocumento>
163600000000     C                   Eval      DsOutput     =
163700000000     C                                      indent5 +
163800000000     C                              '<TipoDocumento>'  +
163900000000     C                                %trim(CstTpDoc)  +
164000000000     C                              '</TipoDocumento>'
164100000000     C                   EXSR      WrtXML
164200000000     C*
164300000000     C*  2.2.3.1.2  <Data>
164400000000     C*  3.2.3.1.2  <Data>
164500000000     C                   Eval      DsOutput     =
164600000000     C                                      indent5 +
164700000000     C                              '<Data>'  +
164800000000     C                                %char(CstDtDoc)  +
164900000000     C                              '</Data>'
165000000000     C                   EXSR      WrtXML
165100000000     C*
165200000000     C*  2.2.3.1.3  <Numero>
165300000000     C*  3.2.3.1.3  <Numero>
165400000000     C                   Eval      DsOutput     =
165500000000     C                                      indent5 +
165600000000     C                              '<Numero>' +
165700000000     C                                %trim(%editc(CstNrDoc:'Z'))  +
165800000000     C                              '</Numero>'
165900000000     C                   EXSR      WrtXML
166000000000     C*
166100000000     C                   if        $Cf   = 'F'
166200000000     C*
166300000000     C*  3.2.3.1.4  <DataRegistrazione>
166400000000     C                   Eval      DsOutput     =
166500000000     C                                      indent5 +
166600000000     C                              '<DataRegistrazione>' +
166700000000     C                                %char(CstDtReg)  +
166800000000     C                              '</DataRegistrazione>'
166900000000     C                   EXSR      WrtXML
167000000000     C                   endif
167100000000     C*
167200000000     C* 2.2.3.1  </DatiGenerali>
167300000000     C* 3.2.3.1  </DatiGenerali>
167400000000     C                   Eval      DsOutput     =
167500000000     C                                      indent4 +
167600000000     C                              '</DatiGenerali>'
167700000000     C                   EXSR      WrtXML
167800000000     C*
167900000000     C                   ENDSR
168000000000     C/EJECT
168100000000     C******************************************************
168200000000     C* Chiusura TAG dati fattura
168300000000     C******************************************************
168400000000     C     CloseTagFat   BEGSR
168500000000     C*
168600000000     C                   select
168700000000     C                   when      $Cf   = 'C'
168800000000     C*
168900000000     C* 2.2.3 </DatiFatturaBodyDTE>
169000000000     C                   Eval      DsOutput     =
169100000000     C                                      indent3 +
169200000000     C                              '</DatiFatturaBodyDTE>'
169300000000     C                   EXSR      WrtXML
169400000000     C*
169500000000     C                   when      $Cf   = 'F'
169600000000     C*
169700000000     C* 3.2.3 </DatiFatturaBodyDTR>
169800000000     C                   Eval      DsOutput     =
169900000000     C                                      indent3 +
170000000000     C                              '</DatiFatturaBodyDTR>'
170100000000     C                   EXSR      WrtXML
170200000000     C                   endsl
170300000000     C*
170400000000     C                   ENDSR
170500000000     C/EJECT
170600000000     C********************************************************************
170700000000     C* Elaboro i movimenti
170800000000     C********************************************************************
170900000000     C     ElabCST       BEGSR
171000000000     C*
171100000000     C                   if        CstSOCIETA = SavSOCIETA and
171200000000     C                             CstTPCOM   = SavTPCOM   and
171300000000     C                             CstCF      = SavCF      and
171400000000     C                             CstANNO    = SavANNO    and
171500000000     C                             CstTRIM    = SavTRIM    and
171600000000     C                             CstMESE    = SavMESE    and
171700000000     C                             CstIDPAES1 = SavIDPAES1 and
171800000000     C                             CstPIVA1   = SavPIVA1   and
171900000000     C                             CstCDFISC1 = SavCDFISC1 and
172000000000     C                             CstDENOM1  = SavDENOM1  and
172100000000     C                             CstCOGNOM1 = SavCOGNOM1 and
172200000000     C                             CstNOME1   = SavNOME1   and
172300000000     C                             CstINDIR2  = SavINDIR2  and
172400000000     C                             CstNRCIV2  = SavNRCIV2  and
172500000000     C                             CstCAP2    = SavCAP2    and
172600000000     C                             CstLOCALI2 = SavLOCALI2 and
172700000000     C                             CstPROV2   = SavPROV2   and
172800000000     C                             CstIDPAES2 = SavIDPAES2 and
172900000000     C                             CstINDIR3  = SavINDIR3  and
173000000000     C                             CstNRCIV3  = SavNRCIV3  and
173100000000     C                             CstCAP3    = SavCAP3    and
173200000000     C                             CstLOCALI3 = SavLOCALI3 and
173300000000     C                             CstPROV3   = SavPROV3   and
173400000000     C                             CstIDPAES3 = SavIDPAES3 and
173500000000     C                             CstIDPAES4 = SavIDPAES4 and
173600000000     C                             CstPIVA4   = SavPIVA4   and
173700000000     C                             CstDENOM4  = SavDENOM4  and
173800000000     C                             CstCOGNOM4 = SavCOGNOM4 and
173900000000     C                             CstNOME4   = SavNOME4   and
174000000000     C                             CstDTDOC   = SavDTDOC   and
174100000000     C                             CstNRDOC   = SavNRDOC   and
174200000000     C                             CstSERDOC  = SavSERDOC  and
174300000000     C                             CstDTREG   = SavDTREG   and
174400000000     C                             CstNRREG   = SavNRREG   and
174500000000     C                             CstSERREG  = SavSERREG  and
174600000000     C                             CstDTANNOT = SavDTANNOT and
174700000000     C                             CstTPDOC   = SavTPDOC   and
174800000000     C                             CstALIQ    = SavALIQ    and
174900000000R64  X**                           CstRIFIVA  = SavRIFIVA  and
175000000000     C                             CstMOTESCL = SavMOTESCL and
175100000000     C                             CstPERCDET = SavPERCDET and
175200000000     C                             CstDEDUCIB = SavDEDUCIB and
175300000000     C                             CstESIGIB  = SavESIGIB
175400000000     C*
175500000000R264 X**                 add       CstImpOpe     w_ImpOpe
175600000000R264 X**                 add       CstIvaOpe     w_IvaOpe
175700000000     C*
175800000000     C                   else
175900000000     C*
176000000000R264 X**                 eval      w_ImpOpe   = CstImpOpe
176100000000R264 X**                 eval      w_IvaOpe   = CstIvaOpe
176200000000     C*
176300000000     C                   eval      SavCstDs = SdgCstDs
176400000000R264 C                   eval      Sv3CstDs = SdgCstDs
176500000000     C                   endif
176600000000     C*
176700000000     C                   ENDSR
176800000000     C/EJECT
176900000000     C******************************************************
177000000000     C* Aggiorna posizione fattura
177100000000     C******************************************************
177200000000     C     UpdateCst     BEGSR
177300000000     C*
177400000000     C                   dou       %error = *off
177500000000     C                             or  MsgRtn <> 'R'
177600000000     C     RRNCst        chain(e)  SdgCst00f
177700000000     C* Gestione RCD allocato
177800000000     C                   if        %error
177900000000     C                   movel     'SDGCST00F'   MSGFIL
178000000000     C                   exsr      MSGPGM
178100000000     C                   endif
178200000000     C                   enddo
178300000000     C*
178400000000     C                   if        %error
178500000000     C                   eval      $Errore = *on
178600000000     C                   else
178700000000     C*
178800000000     C                   if        %found(SdgCst00f)
178900000000     C                   select
179000000000     C                   when      TipInvI61   = 'O'
179100000000     C                   eval      Cs0FileO   = FileXml
179200000000     C                   eval      Cs0PosizO  = $PosizO
179300000000     C                   when      TipInvI61   = 'R'
179400000000     C                   eval      Cs0FileR   = FileXml
179500000000     C                   eval      Cs0PosizR  = $PosizO
179600000000     C***                when      TipInvI61   = 'A'
179700000000     C***                eval      Cs0FileA   = FileXml
179800000000     C                   endsl
179900000000     C*
180000000000     C                   eval      CS0DtUlMo   = %date()
180100000000     C                   eval      CS0UteUlMo  = XscCut
180200000000     C*
180300000000     C                   update    SdgCst000
180400000000     C                   endif
180500000000     C*
180600000000     C                   endif
180700000000     C*
180800000000     C                   ENDSR
180900000000     C/EJECT
181000000000     C******************************************************
181100000000     C* Aggiorna posizione fattura in caso di annullamento
181200000000     C******************************************************
181300000000     C     UpdCstAnn     BEGSR
181400000000     C*
181500000000     C                   eval      C10Societa = XscSoc
181600000000     C                   eval      C10IdFileO = IdFileI61
181700000000     C     K02Cst10      setll     SdgCst10l
181800000000     C                   do        *Hival
181900000000     C     K02Cst10      reade(e)  SdgCst10l
182000000000     C                   if        %eof(SdgCst10l)
182100000000     C                   leave
182200000000     C                   endif
182300000000     C*
182400000000     C* Gestione RCD allocato
182500000000     C                   if        %error
182600000000     C                   movel     'SDGCST00F'   MSGFIL
182700000000     C                   exsr      MSGPGM
182800000000     C                   select
182900000000     C                   when      MsgRtn = 'R'
183000000000     C                   iter
183100000000     C                   when      MsgRtn = 'C'
183200000000     C                   eval      $Errore = *on
183300000000     C                   leave
183400000000     C                   endsl
183500000000     C*
183600000000     C                   else
183700000000     C*
183800000000     C                   if         C10TpRec   <> 'O'
183900000000     C                   iter
184000000000     C                   endif
184100000000     C*
184200000000     C                   if         C10IdFileA <> *blank
184300000000     C                   iter
184400000000     C                   endif
184500000000     C*
184600000000     C                   if        PosizI61  <> 0 and
184700000000     C                             C10PosizO <> PosizI61
184800000000     C                   iter
184900000000     C                   endif
185000000000     C*
185100000000     C                   eval      C10FileA   = FileXml
185200000000     C                   eval      C10DtUlMo  = %date()
185300000000     C                   eval      C10UteUlMo = XscCut
185400000000     C                   update    SdgCst010
185500000000     C*
185600000000     C                   endif
185700000000     C                   enddo
185800000000     C*
185900000000     C                   ENDSR
186000000000     C/EJECT
186100000000R264 C******************************************************
186200000000  "  C* Accorpa DATI RIEPILOGO
186300000000R264 C******************************************************
186400000000     C     AccorpaDati   BEGSR
186500000000     C*
186600000000     C                   clear                   w_ImpOpe
186700000000     C                   clear                   w_IvaOpe
186800000000     C                   Eval      C02Societa   = XScSoc
186900000000     C                   Eval      C02TpCom     = Sv3Tpcom
187000000000     C                   Eval      C02TpRec     = Sv3Tprec
187100000000     C                   Eval      C02Anno      = Sv3Anno
187200000000     C                   Eval      C02Trim      = Sv3Trim
187300000000     C                   Eval      C02Mese      = Sv3Mese
187400000000     C*
187500000000     C                   eval      C02CliFor    = Sv3COGRAG1+Sv3CF+Sv3PARTIVA
187600000000     C                                          + Sv3CDFISC+Sv3STATO1+Sv3IDPAES1
187700000000     C                                          + Sv3PIVA1+Sv3CDFISC1+Sv3TPN1
187800000000     C                                          + Sv3DENOM1+Sv3NOME1+Sv3COGNOM1
187900000000     C                                          + Sv3INDIR2+Sv3NRCIV2+Sv3CAP2
188000000000     C                                          + Sv3LOCALI2+Sv3PROV2+Sv3STATO2
188100000000     C                                          + Sv3IDPAES2+Sv3INDIR3+Sv3NRCIV3
188200000000     C                                          + Sv3CAP3+Sv3LOCALI3+Sv3PROV3
188300000000     C                                          + Sv3STATO3+Sv3IDPAES3+Sv3STATO4
188400000000     C                                          + Sv3IDPAES4+Sv3PIVA4+Sv3TPN4
188500000000     C                                          + Sv3COGRAG4+Sv3DENOM4+Sv3NOME4
188600000000     C                                          + Sv3COGNOM4
188700000000     C*
188800000000     C                   eval      C02TPDOC     = Sv3TPDOC
188900000000     C                   eval      C02DTDOC     = Sv3DTDOC
189000000000     C                   eval      C02NRDOC     = Sv3NRDOC
189100000000     C                   eval      C02SERDOC    = Sv3SERDOC
189200000000     C                   eval      C02DTREG     = Sv3DTREG
189300000000     C                   eval      C02NRREG     = Sv3NRREG
189400000000     C                   eval      C02SERREG    = Sv3SERREG
189500000000     C                   eval      C02DTANNOT   = Sv3DTANNOT
189600000000     C                   eval      C02ALIQ      = Sv3ALIQ
189700000000     C                   eval      C02MOTESCL   = Sv3MOTESCL
189800000000     C                   eval      C02PERCDET   = Sv3PERCDET
189900000000     C                   eval      C02DEDUCIB   = Sv3DEDUCIB
190000000000     C                   eval      C02ESIGIB    = Sv3ESIGIB
190100000000     C     K20Cst02      setll     SdgCst02l
190200000000     C                   do        *hival
190300000000     C     K20Cst02      reade     SdgCst02l
190400000000     C                   if        %eof(SdgCst02l)
190500000000     C                   leave
190600000000     C                   endif
190700000000     C*
190800000000     C                   add       C02ImpOpe     w_ImpOpe
190900000000     C                   add       C02IvaOpe     w_IvaOpe
191000000000     C*
191100000000     C                   enddo
191200000000R264 C*
191300000000  "  C                   ENDSR
191400000000R264 C/EJECT
191500000000     C******************************************************
191600000000     C* Scrive TAG DATI RIEPILOGO
191700000000     C******************************************************
191800000000     C     DatiRiepilogo BEGSR
191900000000     C*
192000000000     C* 2.2.3.2  <DatiRiepilogo>
192100000000     C* 3.2.3.2  <DatiRiepilogo>
192200000000     C                   Eval      DsOutput     =
192300000000     C                                      indent4 +
192400000000     C                              '<DatiRiepilogo>'
192500000000     C                   EXSR      WrtXML
192600000000     C*
192700000000     C*  2.2.3.2.1 <ImponibileImporto>
192800000000     C*  3.2.3.2.1 <ImponibileImporto>
192900000000     C*
193000000000     C                   eval      WWImporto = %abs(w_ImpOpe)
193100000000     C                   EXSR      EditaImporto
193200000000     C*
193300000000     C                   Eval      DsOutput     =
193400000000     C                                      indent5 +
193500000000     C                              '<ImponibileImporto>' +
193600000000     C                               %trim($ImportoA) +
193700000000     C                              '</ImponibileImporto>'
193800000000     C                   EXSR      WrtXML
193900000000     C*
194000000000     C*  2.2.3.2.2 <DatiIVA>
194100000000     C*  3.2.3.2.2 <DatiIVA>
194200000000     C                   Eval      DsOutput     =
194300000000     C                                      indent5 +
194400000000     C                              '<DatiIVA>'
194500000000     C                   EXSR      WrtXML
194600000000     C*
194700000000     C*   2.2.3.2.2.1 <Imposta>
194800000000     C*   3.2.3.2.2.1 <Imposta>
194900000000     C*
195000000000     C                   eval      WWImporto = %abs(w_IvaOpe)
195100000000     C                   EXSR      EditaImporto
195200000000     C*
195300000000     C                   Eval      DsOutput     =
195400000000     C                                      indent6 +
195500000000     C                              '<Imposta>' +
195600000000     C                               %trim($ImportoA) +
195700000000     C                              '</Imposta>'
195800000000     C                   EXSR      WrtXML
195900000000     C*
196000000000     C*   2.2.3.2.2.2 <Aliquota>
196100000000     C*   3.2.3.2.2.2 <Aliquota>
196200000000     C*
196300000000     C                   Z-Add     SavAliq       WWImporto
196400000000     C                   EXSR      EditaImporto
196500000000     C*
196600000000     C                   Eval      DsOutput     =
196700000000     C                                      indent6 +
196800000000     C                              '<Aliquota>' +
196900000000     C                               %trim($ImportoA) +
197000000000     C                              '</Aliquota>'
197100000000     C                   EXSR      WrtXML
197200000000     C*
197300000000     C*  2.2.3.2.2 </DatiIVA>
197400000000     C*  3.2.3.2.2 </DatiIVA>
197500000000     C                   Eval      DsOutput     =
197600000000     C                                      indent5 +
197700000000     C                              '</DatiIVA>'
197800000000     C                   EXSR      WrtXML
197900000000     C*
198000000000     C*  2.2.3.2.3 <Natura>
198100000000     C*  3.2.3.2.3 <Natura>
198200000000     C                   if        SavMotEscl <> *blank
198300000000     C                   Eval      DsOutput     =
198400000000     C                                      indent5 +
198500000000     C                              '<Natura>' +
198600000000     C                               %trim(SavMotEscl) +
198700000000     C                              '</Natura>'
198800000000     C                   EXSR      WrtXML
198900000000     C                   endif
199000000000     C*
199100000000     C*  2.2.3.2.4 <Detraibile>
199200000000     C*  3.2.3.2.4 <Detraibile>
199300000000     C                   if        SavPercDet <> 0
199400000000     C*
199500000000     C                   Z-Add     SavPercDet    WWImporto
199600000000     C                   EXSR      EditaImporto
199700000000     C*
199800000000     C                   Eval      DsOutput     =
199900000000     C                                      indent5 +
200000000000     C                              '<Detraibile>' +
200100000000     C                               %trim($ImportoA) +
200200000000     C                              '</Detraibile>'
200300000000     C                   EXSR      WrtXML
200400000000     C                   endif
200500000000     C*
200600000000     C*  2.2.3.2.5 <Deducibile>
200700000000     C*  3.2.3.2.5 <Deducibile>
200800000000     C                   if        SavDeducib = '1'
200900000000     C                   Eval      DsOutput     =
201000000000     C                                      indent5 +
201100000000     C                              '<Deducibile>' +
201200000000     C                               'SI' +
201300000000     C                              '</Deducibile>'
201400000000     C                   EXSR      WrtXML
201500000000     C                   endif
201600000000     C*
201700000000     C*  2.2.3.2.6 <EsigibilitaIVA>
201800000000     C*  3.2.3.2.6 <EsigibilitaIVA>
201900000000     C                   if        SavEsigib <> *blank
202000000000     C                   Eval      DsOutput     =
202100000000     C                                      indent5 +
202200000000     C                              '<EsigibilitaIVA>' +
202300000000     C                               SavEsigib +
202400000000     C                              '</EsigibilitaIVA>'
202500000000     C                   EXSR      WrtXML
202600000000     C                   endif
202700000000     C*
202800000000     C* 2.2.3.2  </DatiRiepilogo>
202900000000     C* 3.2.3.2  </DatiRiepilogo>
203000000000     C                   Eval      DsOutput     =
203100000000     C                                      indent4 +
203200000000     C                              '</DatiRiepilogo>'
203300000000     C                   EXSR      WrtXML
203400000000     C*
203500000000     C                   ENDSR
203600000000     C/EJECT
203700000000     C******************************************************
203800000000     C* Dati rettifica
203900000000     C******************************************************
204000000000     C     DatiRettifica BEGSR
204100000000     C*
204200000000     C*
204300000000     C* 2.3 <Rettifica>
204400000000     C* 3.3 <Rettifica>
204500000000     C                   if        TipInvI61 = 'R'
204600000000     C                   Eval      DsOutput     =
204700000000     C                                      indent2 +
204800000000     C                              '<Rettifica>'
204900000000     C                   EXSR      WrtXML
205000000000     C*
205100000000     C* 2.3.1 IdFile
205200000000     C* 3.3.1 IdFile
205300000000     C                   Eval      DsOutput     =
205400000000     C                                      indent3 +
205500000000     C                              '<IdFile>' +
205600000000     C                                %trim(IdFileI61) +
205700000000     C                              '</IdFile>'
205800000000     C                   EXSR      WrtXML
205900000000     C*
206000000000     C* 2.3.2 Posizione
206100000000     C* 3.3.2 Posizione
206200000000     C                   Eval      DsOutput     =
206300000000     C                                      indent3 +
206400000000     C                              '<Posizione>' +
206500000000     C                                %trim(%editc(CstPosizO:'Z'))  +
206600000000     C                              '</Posizione>'
206700000000     C                   EXSR      WrtXML
206800000000     C*
206900000000     C* 2.3 </Rettifica>
207000000000     C* 3.3 </Rettifica>
207100000000     C                   Eval      DsOutput     =
207200000000     C                                      indent2 +
207300000000     C                              '</Rettifica>'
207400000000     C                   EXSR      WrtXML
207500000000     C                   endif
207600000000     C*
207700000000     C                   ENDSR
207800000000     C/EJECT
207900000000     C******************************************************
208000000000     C* Chiusura TAG DTE/DTR
208100000000     C******************************************************
208200000000     C     CloseTagDT    BEGSR
208300000000     C*
208400000000     C                   select
208500000000     C                   when      $Cf   = 'C'
208600000000     C*
208700000000     C* 2.  /DTE
208800000000     C                   Eval      DsOutput     =
208900000000     C                                      indent1 +
209000000000     C                              '</DTE>'
209100000000     C                   EXSR      WrtXML
209200000000     C*
209300000000     C                   when      $Cf   = 'F'
209400000000     C*
209500000000     C* 3.  /DTR
209600000000     C                   Eval      DsOutput     =
209700000000     C                                      indent1 +
209800000000     C                              '</DTR>'
209900000000     C                   EXSR      WrtXML
210000000000     C                   endsl
210100000000     C*
210200000000     C                   ENDSR
210300000000     C/EJECT
210400000000     C************************************************************
210500000000     C* Editazione di un importo
210600000000     C* Tolgo il punto decimale e metto la virgola decimale
210700000000     C* Non metto il punto delle migliaia
210800000000     C************************************************************
210900000000     C     EditaImporto  BEGSR
211000000000     C*
211100000000     C                   EXSR      AllineaDec
211200000000     C*
211300000000     C                   Eval      $SrcVarDe =   NrDecMC
211400000000     C                   Eval      $LenFld     = %Size($ImportoA)
211500000000     C                   Eval      $EdtCode    = 'Q'
211600000000     C*
211700000000     C                   EXSR      CallEdit
211800000000     C*
211900000000     C                   Eval      $ImportoA   = $RcvrVar
212000000000     C     ',':'.'       xlate     $ImportoA     $ImportoA
212100000000     C*
212200000000     C                   ENDSR
212300000000     C/EJECT
212400000000     C************************************************************
212500000000     C* Editazione del mese
212600000000     C* Tolgo lo 0 non significativo
212700000000     C************************************************************
212800000000     C     EditaMese     BEGSR
212900000000     C*
213000000000     C                   Z-add     CSTMese       $SrcVar
213100000000     C                   Eval      $SrcVarDe =   0
213200000000     C                   Eval      $LenFld     = %Size($MeseA)
213300000000     C                   Eval      $EdtCode    = '4'
213400000000     C*
213500000000     C                   EXSR      CallEdit
213600000000     C*
213700000000     C                   Eval      $MeseA      = $RcvrVar
213800000000     C*
213900000000     C                   ENDSR
214000000000     C/EJECT
214100000000     C************************************************************
214200000000     C* Chiamata al driver per editazione campi numerici
214300000000     C************************************************************
214400000000     C     CallEdit      BEGSR
214500000000     C*
214600000000     C                   CallB     'X23EDITR'
214700000000     C                   Parm                    $SrcVar
214800000000     C                   Parm                    $SrcVarPr
214900000000     C                   Parm                    $SrcVarDe
215000000000     C                   Parm                    $EdtCode
215100000000     C                   Parm                    $LenFld
215200000000     C                   Parm      *Blanks       $RcvrVar
215300000000     C                   Parm                    $Err
215400000000     C*
215500000000     C                   ENDSR
215600000000     C/EJECT
215700000000     C************************************************************
215800000000     C* Allinea i numeri decimali prima della chiamata a X23Editr
215900000000     C************************************************************
216000000000     C     AllineaDec    BEGSR
216100000000     C*
216200000000     C                   Select
216300000000     C                   When      NrDecMC  = 0
216400000000     C                   Z-add     WWImporto     $SrcVar
216500000000     C                   When      NrDecMC  = 1
216600000000     C     WWImporto     Mult      10            $SrcVar
216700000000     C                   When      NrDecMC  = 2
216800000000     C     WWImporto     Mult      100           $SrcVar
216900000000     C                   Other
217000000000     C     WWImporto     Mult      1000          $SrcVar
217100000000     C                   Endsl
217200000000     C*
217300000000     C                   ENDSR
217400000000     C/EJECT
217500000000     C************************************************************
217600000000     C* Creo File Output
217700000000     C************************************************************
217800000000     C     CreoFileO     BEGSR
217900000000     C*
218000000000     C* Cancello il file in qtemp (se esiste)
218100000000     C                   EXSR      DelFileO
218200000000     C*
218300000000     C* Creo file in qtemp per metadata
218400000000     C* Il file creato a pgm serve per poter compilare il pgm che
218500000000     C* si appoggia a un file creato in QTEMP che al termine del
218600000000     C* lavoro sparisce
218700000000     C                   EXSR      CrtFileO
218800000000     C*
218900000000     C* Ovrprtf per file in qtemp
219000000000     C                   EXSR      OvrFileO
219100000000     C*
219200000000     C                   OPEN      FileO
219300000000     C*
219400000000     C* Recupera progressivo invio
219500000000     C                   exsr      TakeNrProg
219600000000     C* Imposta nome file Xml
219700000000     C                   exsr      SetNomeF
219800000000     C*
219900000000     C                   ENDSR
220000000000     C/EJECT
220100000000     C************************************************************
220200000000     C* Creo files in qtemp
220300000000     C************************************************************
220400000000     C     CrtFileO      BEGSR
220500000000     C*
220600000000     C* Creo il metadata in QTEMP
220700000000     C                   Eval      Cmd = 'CRTPF FILE(QTEMP/' +
220800000000     C                                   wFile   + ')'
220900000000     C                              + ' RCDLEN(500)'
221000000000     C                              + ' TEXT(''XML Spesometro Trimestrale'')'
221100000000     C                              + ' SIZE(2000000 1000)'
221200000000     C                   call      'QCMDEXC'
221300000000     C                   Parm                    Cmd             500
221400000000     C                   Parm      500           Len              15 5
221500000000     C*
221600000000     C                   ENDSR
221700000000     C/EJECT
221800000000     C************************************************************
221900000000     C* Ovr file in qtemp
222000000000     C************************************************************
222100000000     C     OvrFileO      BEGSR
222200000000     C*
222300000000     C* File sostituente
222400000000     C                   Eval      Cmd = 'OVRDBF FILE(FILEO) '
222500000000     C                              + 'TOFILE(QTEMP/'
222600000000     C                              + wFile    +')'
222700000000     C                   call      'QCMDEXC'
222800000000     C                   Parm                    Cmd             500
222900000000     C                   Parm      500           Len              15 5
223000000000     C*
223100000000     C                   ENDSR
223200000000     C/EJECT
223300000000     C************************************************************
223400000000     C* Chiudo e copio File Output
223500000000     C************************************************************
223600000000     C     ChiudoFileO   BEGSR
223700000000     C*
223800000000     C                   Close     FileO
223900000000     C*
224000000000     C* Imposta nome file Xml
224100000000     C                   if        $Errore      = *off  and
224200000000     C                             (RigaMod     >= 1   or
224300000000     C                              $RotturaDoc = *on  or
224400000000     C                              TipInvI61    = 'A')
224500000000     C*
224600000000     C* Copio file XML nella directory dell'IFS
224700000000     C                   exsr      CpyToStmF
224800000000     C                   else
224900000000     C                   clear                   $ProgrInt
225000000000     C                   rolbk
225100000000     C                   endif
225200000000     C*
225300000000     C* Cancello il file in qtemp
225400000000     C                   exsr      DelFileO
225500000000     C*
225600000000     C                   ENDSR
225700000000     C/EJECT
225800000000     C************************************************************
225900000000     C* Copia file creato sulla directory
226000000000     C************************************************************
226100000000     C     CpyToStmF     BEGSR
226200000000     C*
226300000000     C*
226400000000     C                   Eval       WMbr='/qsys.lib/qtemp.lib/'
226500000000     C                              + %trimr(wFile) + '.file/'
226600000000     C                              + %trimr(wFile) + '.mbr'
226700000000     C*
226800000000     C                   Eval       WStmF= %trimr(CartellI61)
226900000000     C                              +'/' + %trimr(FileXml) + '.xml'
227000000000     C*
227100000000     C                   Eval      Cmd = 'CPYTOSTMF  FROMMBR('''
227200000000     C                              + %trimr(WMbr) + ''')'
227300000000     C                              + ' TOSTMF('''                     +'/'
227400000000     C                              + %trimr(WStmF) + ''')'
227500000000     C                              + ' STMFOPT(*REPLACE) '
227600000000     C                              + ' STMFCODPAG(*PCASCII) '
227700000000     C*
227800000000     C                   CALL      'QCMDEXC'                            33
227900000000     C                   Parm                    Cmd             500
228000000000     C                   Parm      500           Len              15 5
228100000000     C*
228200000000     C* se è riuscita la copia cambio l'autorizzazione all'oggetto
228300000000     C                   if        *In33 = *Off
228400000000     C*
228500000000     C                   Eval      Cmd = 'CHGAUT OBJ('''
228600000000     C                              + %trimr(WStmF) + ''')'
228700000000     C                              + ' USER(*PUBLIC) DTAAUT(*RWX)'
228800000000     C*
228900000000     C                   CALL      'QCMDEXC'                            33
229000000000     C                   Parm                    Cmd             500
229100000000     C                   Parm      500           Len              15 5
229200000000     C*
229300000000     C* salvo le informazioni nel catalogo
229400000000     C                   exsr      WrtCatalogo
229500000000     C*
229600000000     C                   commit
229700000000     C                   feod      SdgCst00F
229800000000     C                   feod      SdgCst10l
229900000000     C                   feod      SdgCsf00F
230000000000     C*
230100000000     C* emissione a video di avvenuta creazione
230200000000     C                   exsr      CallSI62R
230300000000     C*
230400000000     C                   endif
230500000000     C*
230600000000     C                   ENDSR
230700000000     C/EJECT
230800000000     C************************************************************
230900000000     C* Cancello file in qtemp
231000000000     C************************************************************
231100000000     C     DelFileO      BEGSR
231200000000     C*
231300000000     C* Cancello il metadata
231400000000     C                   Eval      Cmd= 'DLTF FILE(QTEMP/'  +
231500000000     C                                   WFile   +')'
231600000000     C                   Call      'QCMDEXC'                            33
231700000000     C                   Parm                    Cmd             500
231800000000     C                   Parm      500           len              15 5
231900000000     C*
232000000000     C                   ENDSR
232100000000     C/EJECT
232200000000     C********************************************************************
232300000000     C* Close Sql fetch
232400000000     C********************************************************************
232500000000     C     CloseSql      BEGSR
232600000000     C*
232700000000     C/EXEC SQL
232800000000     C+     Close A1
232900000000     C/END-EXEC
233000000000     C*
233100000000     C                   ENDSR
233200000000     C/EJECT
233300000000     C******************************************************
233400000000     C* Gestione invio multiplo
233500000000     C******************************************************
233600000000     C     GesMultInv    BegSr
233700000000     C*
233800000000     C                   eval      $PosizO  = $PosizO  + 1
233900000000     C                   exsr      UpdateCst
234000000000     C*
234100000000     C* Chiude Tag DTE/DTR
234200000000     C                   exsr      CloseTagDT
234300000000     C* Chiude File
234400000000     C                   Eval      DsOutput     =
234500000000     C                             '</ns2:DatiFattura>'
234600000000     C                   EXSR      WrtXML
234700000000     C*
234800000000     C                   exsr      ChiudoFileO
234900000000     C*
235000000000     C                   select
235100000000     C                   when      $CF   = 'C'
235200000000     C                   eval      wFile = FileDte
235300000000     C*
235400000000     C                   when      $CF   = 'F'
235500000000     C                   eval      wFile = FileDtr
235600000000     C                   endsl
235700000000     C*
235800000000     C                   exsr      CreoFileO
235900000000     C*
236000000000     C                   clear                   RigaMod
236100000000     C                   clear                   $PosizO
236200170911R2661C****               z-add     0             $MaxRighe
236300170911  "  C****               z-add     0             $MaxNrDoc
236400170911  "  C                   z-add     1             $MaxRighe
236500170911R2661C                   z-add     1             $MaxNrDoc
236600000000     C                   clear                   Sv1CstDs
236700000000     C                   clear                   Sv2CstDs
236800000000     C                   eval      $RotturaCF  = *off
236900000000     C                   eval      $RotturaDoc = *off
237000000000     C                   eval      $Errore     = *off
237100000000     C*
237200000000     C* Scrivo TAG Intestazione e Frontespizio
237300000000     C                   exsr      WrtIntestaz
237400000000     C*
237500000000     C                   ENDSR
237600000000     C/EJECT
237700000000     C******************************************************
237800000000     C* Scrittura catalogo
237900000000     C******************************************************
238000000000     C     WrtCatalogo   BEGSR
238100000000     C*
238200000000     C                   clear                   WDesFile
238300000000     C                   clear                   SdgCsf000
238400000000     C*
238500000000     C                   eval      CSFUTEIMM  = XscCut
238600000000     C                   eval      CSFDTIMM   = %date()
238700000000     C                   eval      CSFUTEULMO = XscCut
238800000000     C                   eval      CSFDTULMO  = %date()
238900000000     C                   eval      CSFUTEXML  = XscCut
239000000000     C                   eval      CSFDTXML   = %date()
239100000000     C                   eval      CSFORIGINE = 'A'
239200000000     C                   eval      CSFSOCIETA = XscSoc
239300000000     C                   eval      CSFTPCOM   = 'T'
239400000000     C                   eval      CSFCF      = $Cf
239500000000     C*
239600000000     C                   select
239700000000     C* invio ordinario: tipo record = 'O'
239800000000     C                   when      TipInvI61  = 'O'
239900000000     C                   eval      CSFTPREC   = 'O'
240000000000     C                   eval      WDesFile   = 'Ordinario'
240100000000     C* Rettifica: tipo record = 'R'
240200000000     C                   When      TipInvI61   = 'R'
240300000000     C                   eval      CSFTPREC   = 'R'
240400000000     C                   eval      WDesFile   = 'Rettifica'
240500000000     C* Annullam.: tipo record = 'A'
240600000000     C                   When      TipInvI61   = 'A'
240700000000     C                   eval      CSFTPREC   = 'A'
240800000000     C                   eval      CSFCF      = C10Cf
240900000000     C                   eval      WDesFile   = 'Annullamento'
241000000000     C                   endsl
241100000000     C*
241200000000     C                   eval      CSFANNO    = AnnoI61
241300000000     C                   eval      CSFTRIM    = TrimI61
241400000000     C                   eval      CSFMESE    = 0
241500000000     C                   eval      CSFPRGESTR = 0
241600000000     C                   eval      CSFFILE    = FileXml
241700000000     C                   eval      CSFFILERGP = %trim(FileSav)
241800000000     C*
241900000000     C                   if        TipInvI61  <> 'A'
242000000000     C                   if        $Cf        = 'C'
242100000000     C                   eval      WDesFile   = %trim(WDesFile)
242200000000     C                                        + ' DTE'
242300000000     C                   else
242400000000     C                   eval      WDesFile   = %trim(WDesFile)
242500000000     C                                        + ' DTR'
242600000000     C                   endif
242700000000     C                   endif
242800000000     C*
242900000000     C                   if        CsfTpCom   = 'T'
243000000000     C                   eval      CSFNOTE    = 'Spesometro Trimestrale '
243100000000     C                                        + %trim(WDesFile)
243200000000     C                                        + ' '
243300000000     C                                        + %editc(AnnoI61:'X')
243400000000     C                                        + ' trim. '
243500000000     C                                        + %editc(TrimI61:'X')
243600000000     C                   endif
243700000000     C
243800000000     C                   eval      CSFIDFILE  = *blank
243900000000     C                   eval      CSFDTINVIO = *loval
244000000000     C                   eval      CSFFILESIT = *blank
244100000000     C                   eval      CSFDTESITO = *loval
244200000000     C                   eval      CSFESITO   = *blank
244300000000     C*
244400000000     C                   write     SdgCsf000                            99
244500000000     C*
244600000000     C                   ENDSR
244700000000     C/EJECT
244800000000     C************************************************************
244900000000     C* Emissione a video di avvenuta creazione file
245000000000     C************************************************************
245100000000     C     CallSI62R     BegSr
245200000000     C*
245300000000     C                   call      'SDGSI62R'
245400000000     C                   parm                    SocietaI61
245500000000     C                   parm                    CsfFile
245600000000     C                   parm                    CsfNote
245700000000     C                   parm                    $ProgrInt
245800000000     C                   parm                    CartellI61
245801170920R2671X****               parm      '1'           Video             1
245802170920R2671C**                 parm                    Video             1
245803170920BRT  C                   parm      '0'           Video             1
246000000000     C*
246100000000     C                   EndSR
246200000000     C******************************************************
246300000000     C* Operazioni iniziali
246400000000     C******************************************************
246500000000     C     *INZSR        BEGSR
246600000000     C*
246700000000     C     *Entry        PLIST
246800000000     C                   Parm                    KPJBA
246900000000     C*
247000000000     C                   MoveL     KPJBU         SDGSI61DS
247100000000     C*
247200000000     C     *DMY          Move      UDate         UDateISO
247300000000     C*
247400000000     C                   ENDSR
247500000000     C/EJECT
247600000000     C******************************************************
247700000000     C* Inizializzazione variabili
247800000000     C******************************************************
247900000000     C     INZVAR        BEGSR
248000000000     C*
248100000000     C                   MoveL     KPJBU         SDGSI61DS
248200000000     C*
248300000000     C                   MoveL     'SOC001'      TIPXSC
248400000000     C                   MoveL     SocietaI61    SOCXSC
248500000000     C*
248600000000     C                   EXSR      RepSoc
248700000000     C*
248800000000     C                   if        RTNXSC     <> '1'
248900000000     C                   MoveL     XSOCDS        SOC001
249000000000     C                   else
249100000000     C                   Eval      ERRI61      = *On
249200000000     C                   EXSR      ENDPGM
249300000000     C                   endif
249400000000     C*
249500000000     C                   Eval      WNrDecMC   =  %SubSt(XScSKp:6:1)
249600000000     C                   Move      WNrDecMC      NrDecMC
249700000000     C*
249800000000     C* Reperisco i dati di setup del modulo Spesometro Trimestrale
249900000000     C                   EXSR      RepSDSPETRI
250000000000     C*
250100000000     C* reperisco i dati salvati su ANMPE
250200000000     C                   EXSR      RepFrontesp
250300000000     C*
250400000000     C                   callb     'XSTRCMT'
250500000000     C                   open      SDGCST00F
250600000000     C                   open      SDGCSF00F
250700000000     C                   open      SDGCST10L
250800000000R264 C                   open      SDGCST02L
250900000000     C*
251000000000     C                   ENDSR
251100000000     C/EJECT
251200000000     C************************************************************
251300000000     C* Rerimento parametri di setup per Spesometro Trimestrale
251400000000     C************************************************************
251500000000     C     RepSDSPETRI   BEGSR
251600000000     C*
251700000000     C                   Clear                   SDGPA12DS
251800000000     C*
251900000000     C                   CALLB     'XPAR'
252000000000     C                   Parm                    XScSoc
252100000000     C                   Parm      'SDSPETRI'    XPaPar            8
252200000000     C                   Parm                    XPaOut         2000
252300000000     C                   Parm      *OFF          XPaErr            1
252400000000     C                   Parm      ' '           XPaRic            1
252500000000     C*
252600000000     C                   if        XPaErr      = *On
252700000000     C                   EXSR      ENDPGM
252800000000     C*
252900000000     C                   else
253000000000     C                   MoveL     XPaOut        SDGPA12DS
253100000000     C*
253200000000     C                   if        AttCSTA12  <> *On
253300000000     C                   EXSR      ENDPGM
253400000000     C                   endif
253500000000     C*
253600000000     C                   endif
253700000000     C*
253800000000     C                   ENDSR
253900000000     C/EJECT
254000000000     C************************************************************
254100000000     C* Reperisco i dati per il Frontespizio
254200000000     C******************************************************
254300000000     C     RepFrontesp   BEGSR
254400000000     C*
254500000000     C* preparo i parametri di chiamata al driver
254600000000     C                   Movel     *off          $Trovato
254700000000     C                   Clear                   $SkDati
254800000000     C                   Clear                   XMPEDS
254900000000     C                   EVAL      TpChiamata = '01'
255000000000     C                   EVAL      NChiavi = 5
255100000000     C                   EVAL      VLogica = 2
255200000000     C*
255300170905R2654X***                Do        4             $Cont
255400170905R2654C                   Do        $DimSk        $Cont
255500000000     C                   Clear                   XMPEDS1
255600000000     C                   Eval      DSMPEPGM = 'SDGSI74R' +
255700000000     C                                        %char($cont)
255800000000     C* preparo le chiavi per la chiamata al driver
255900000000     C                   MoveL     XSCSOC        DSMPESOC
256000000000     C                   MoveL     *Blanks       DSMPEPRF
256100000000     C* Si reperiscono i dati di default a seconda delle modalità con
256200000000     C* cui il pgm è usato
256300000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
256400000000     C* MPETIP = 'PV'  pgm richiamato da menù
256500000000     C* MPETIP = 'PB'  x lancio Batch
256600000000     C                   MoveL     'PV'          DSMPETIP
256700000000     C*
256800000000     C                   CALLB     'XMPE'
256900000000     C                   PARM                    XMPEDS
257000000000     C                   PARM                    XMPEDS1
257100000000     C*
257200000000     C     Risultato     IfEq      '0'
257300000000     C                   Movel     *on           $Trovato
257400000000     C*
257500000000     C                   select
257600000000     C                   When      $Cont   = 1
257700000000     C                   MoveL     DSMPEPAR      DsFrontesp1
257800000000     C                   When      $Cont   = 2
257900000000     C                   MoveL     DSMPEPAR      DsFrontesp2
258000000000     C                   When      $Cont   = 3
258100000000     C                   MoveL     DSMPEPAR      DsFrontesp3
258200000000     C                   When      $Cont   = 4
258300000000     C                   MoveL     DSMPEPAR      DsFrontesp4
258400170905R2654C                   When      $Cont   = 5
258500170905R2654C                   MoveL     DSMPEPAR      DsFrontesp41
258600000000     C                   EndSl
258700000000     C*
258800000000     C                   else
258900000000     C*
259000000000     C                   Eval      ERRI61      = '2'
259100000000     C                   EXSR      ENDPGM
259200000000     C                   endif
259300000000     C*
259400000000     C                   Enddo
259500000000     C*
259600000000     C                   Endsr
259700000000     C/EJECT
259800000000     C******************************************************
259900000000     C* Imposta nome file da trasmettere
260000000000     C******************************************************
260100000000     C     SetNomeF      begsr
260200000000      *
260300000000     C                   Clear                   FileXml
260400000000      * il nome del file deve rispettare la seguente nomenclatura:
260500000000      *  1) codice paese
260600000000      *  2) identificativo univoco del soggetto trasmittente
260700000000      *  3) tipologia file
260800000000      *  4) progressivo univoco del file (numeratore SD12)
260900000000      * dove:
261000000000      *  1) il codice paese va espresso secondo lo standard ISO 3166-1 alpha-2
261100000000      *     code;
261200000000      *  2) l'identificativo univoco del responsabile della trasmissione, sia esso
261300000000      *     persona fisica o persona giuridica, è rappresentato dal suo identificativo
261400000000      *     fiscale (codice fiscale nel caso di soggetto trasmittente residente in
261500000000      *     Italia, identificativo proprio del paese di appartenenza nel caso di soggetto
261600000000      *     trasmittente residente all?estero);
261700000000      *    la lunghezza di questo identificativo è di:
261800000000      *     - 11 caratteri (minimo) e 16 caratteri (massimo) nel caso di
261900000000      *       codice paese IT;
262000000000      *     - 2 caratteri (minimo) e 28 caratteri (massimo) altrimenti;
262100000000      *  3) la tipologia file è rappresentata dal valore fisso "DF";
262200000000      *  4) il progressivo univoco del file è rappresentato da una stringa
262300000000      *     alfanumerica di lunghezza massima di 5 caratteri e con valori
262400000000      *     ammessi [a-z], [A-Z], [0-9]
262500000000      *
262600000000      *  Es. ITAAABBB99T99X999W_DF_00001.xml
262700000000      *      IT99999999999_DF_00002.xml
262800000000     C                   clear                   $NomeF
262900000000     C                   if        $Piva1 <> *blank
263000000000     C                   eval      $NomeF = $PIva1
263100000000     C                   else
263200000000     C                   eval      $NomeF = $Cdf1
263300000000     C                   endif
263400000000      *
263500000000     C                   eval      FileXml = 'IT' + %trim($NomeF)
263600000000     C                                     + '_' + 'DF' + '_'
263700000000     C                                     + %editc($ProgrInvio:'X')
263800000000      *
263900000000     C                   if        $PrimaV  = *off
264000000000     C                   eval      FileSav  = FileXml
264100000000     C                   eval      $PrimaV  = *on
264200000000     C                   endif
264300000000      *
264400000000     C                   endsr
264500000000     C/EJECT
264600000000     C************************************************************
264700000000     C* Reperisco il progressivo univoco del file
264800000000     C************************************************************
264900000000     C     TakeNrProg    BEGSR
265000000000     C*
265100000000     C                   Move      UDateIso      XNmDat
265200000000     C                   Move      UDateIso      XNmDag
265300000000     C*
265400000000     C                   CallB     'XNMR'
265500000000     C                   Parm      '3'           XNmRic            1
265600000000     C                   Parm      XScSoc        XNmSoc            3
265700000000     C                   Parm      *Blank        XNmUni            8
265800000000     C                   Parm      *Blank        XNmCtb            2
265900000000     C                   Parm      *Blank        XNmKey            8
266000000000     C                   Parm      'SD12'        XNmSer            4
266100000000     C                   Parm      *On           XNmCmt            1
266200000000     C                   Parm                    XNmDat           10
266300000000     C                   Parm      *Off          XNmIVA            1
266400000000     C                   Parm      *Off          XNmReg            1
266500000000     C                   Parm                    XNmErr            1
266600000000     C                   Parm                    XNmNum            9 0
266700000000     C                   Parm                    XNmAut            1
266800000000     C                   Parm                    XNmDag           10
266900000000     C*
267000000000     C* se viene rilevato un errore
267100000000     C                   if        XNmErr      = '1'      or
267200000000     C                             XNmErr      = '2'      or
267300000000     C                             XNmErr      = '7'
267400000000     C                   exsr      EndPgm
267500000000     C                   else
267600000000     C                   move      xnmnum        $ProgrInvio
267700000000     C                   add       1             $ProgrInt
267800000000     C                   endif
267900000000     C*
268000000000     C                   ENDSR
268100000000     C/EJECT
268200000000     C******************************************************
268300000000     C* Scrittura file XML
268400000000     C******************************************************
268500000000     C     WrtXML        BEGSR
268600000000     C*
268700000000     C                   Eval      Output      = DsOutput
268800000000     C                   Write     FileO         FileODS
268900000000     C*
269000000000     C                   ENDSR
269100000000     C/EJECT
269200000000     C************************************************************
269300000000     C* Reperimento dati società
269400000000     C************************************************************
269500000000     C     RepSoc        BEGSR
269600000000     C*
269700000000     C                   CALL      'SDGSOCDR'
269800000000     C                   Parm                    TIPXSC            6
269900000000     C                   Parm                    SOCXSC            3
270000000000     C                   Parm                    CDSXSC            9 0
270100000000     C                   Parm                    MODXSC            3
270200000000     C                   Parm      *Blanks       RTNXSC            1
270300000000     C                   Parm                    XSOCDS
270400000000     C                   Parm                    KPJBA
270500000000     C*
270600000000     C                   ENDSR
270700000000     C/EJECT
270800000000     C******************************************************
270900000000     C* DEFINIZIONE KLIST
271000000000     C******************************************************
271100000000     C     DefKList      BEGSR
271200000000     C*
271300000000     C* SDGCST10L
271400000000     C     K02CST10      Klist
271500000000     C                   Kfld                    C10SOCIETA
271600000000     C                   Kfld                    C10IDFILEO
271700000000R264 C*
271800000000  "  C* SDGCST02L
271900000000  "  C     K20CST02      Klist
272000000000  "  C                   Kfld                    C02Societa
272100000000  "  C                   Kfld                    C02TpCom
272200000000  "  C                   kfld                    C02Tprec
272300000000  "  C                   kfld                    C02Anno
272400000000  "  C                   kfld                    C02Trim
272500000000  "  C                   kfld                    C02Mese
272600000000  "  C                   kfld                    C02CLIFOR
272700000000  "  C                   kfld                    C02TPDOC
272800000000  "  C                   kfld                    C02DTDOC
272900000000  "  C                   kfld                    C02NRDOC
273000000000  "  C                   kfld                    C02SERDOC
273100000000  "  C                   kfld                    C02DTREG
273200000000  "  C                   kfld                    C02NRREG
273300000000  "  C                   kfld                    C02SERREG
273400000000  "  C                   kfld                    C02DTANNOT
273500000000  "  C                   kfld                    C02ALIQ
273600000000  "  C                   kfld                    C02MOTESCL
273700000000  "  C                   kfld                    C02PERCDET
273800000000  "  C                   kfld                    C02DEDUCIB
273900000000R264 C                   kfld                    C02ESIGIB
274000000000     C*
274100000000     C                   ENDSR
274200000000     C/EJECT
274300000000     C******************************************************
274400000000     C* PGM PER GESTIONE MESSAGGI ERRORE
274500000000     C******************************************************
274600000000     C     MSGPGM        BEGSR
274700000000     C*
274800000000     C                   Z-ADD     2             MSGMSG
274900000000     C                   MOVEL     'S'           MSGRSP
275000000000     C                   MOVEL     'CR'          MSGTPR
275100000000     C                   MOVEL     'S'           MSGEMV
275200000000     C                   Z-ADD     10            MSGRGP
275300000000     C                   MOVEL     'S'           MSGLCK
275400000000     C                   MOVEA     STATUS        STS
275500000000     C                   MOVEL     'N'           MSGCNL
275600000000     C                   MOVEL     'N'           MSGVID
275700000000     C                   MOVEL     'N'           MSGOPE
275800000000     C                   MOVEL     'N'           MSGSTP
275900000000     C                   CALL      'SDGMSGR'
276000000000     C                   PARM                    SDGMSGDS
276100000000     C*
276200000000     C                   ENDSR
276300000000     C/EJECT
