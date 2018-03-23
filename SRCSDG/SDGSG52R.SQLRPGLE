000100000000     H*PARMS DFTACTGRP(*NO)
000200130826     H*PARMS BNDDIR(PJXBND PJCBND SDG005)
000300000000     H*PARMS ACTGRP(QILE)
000400000000     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000500000000     H*PARMS RDB(*NONE) DYNUSRPRF(*OWNER)
000600000000     H*****************************************************************
000700000000     H DECEDIT('0,') DATEDIT(*DMY.)
000800000000     H*****************************************************************
000900000000     H*
001000151130R195 H*                 Comunicazione Polivalente
001100130820     H*                Spesometro clienti/fornitori
001200160107R195 H*           Acquisti da San Marino e Black List
001300000000     H*                    Estrazione batch
001400000000     H*           --------------------------------------
001500000000     H*
001600000000     H* Flusso per reperimento dati fiscali:
001700000000     H*  - se conto Estemporaneo (RCOTecnico='2') ,  da NDCPA00F
001800000000     H*  - se conto "normale" da ANCPI00F o, in mancanza di
001900000000     H*    storicizzazione, da ANSOG00F
002000000000     H*
002100000000     H*  In tutti i casi, in presenza di dati aggiuntivi viene
002200000000     H*  usato SDGSO300F
002300000000      *
002400000000      *  ANALISI della registrazione
002500000000      *  - COMMIT gestito a livello di registrazione
002600130820      *    Se una riga di SDGSCF00F è modificata manualmente, non
002700000000      *    viene fatto nulla per tutti i movimenti della registrazione
002800000000      *  - vengono letti i movimenti (NDMOV00F) della registrazione:
002900000000      *    per l'analisi delle contropartite per definire su si
003000000000      *    tratta di beni o servizi
003100130918      *
003200130918      *  Le operazioni senza fattura (=scontrini, ovvero corrispettivi)
003300130918      *  non vengono estratte a standard dal batch perchè in Proj non
003400130918      *  vengono registrati in maniera identificabile.
003500130918      *
003600000000     H*
003700000000     F*****************************************************************
003800000000     F* libri IVA
003900000000     FANDLI01L  IF   E           K DISK
004000000000     F*-------------
004100000000     F* riferimenti/aliquote
004200000000     FANRIV01L  IF   E           K DISK
004300000000     F*-------------
004400000000     F* associazione libri/riferim-aliquote
004500000000     FANALR01L  IF   E           K DISK
004600151222R195 F*------------
004700151222 "   F* Associazioni libri/rifer. IVA escluse per Black List
004800151222R195 FSDGBLA01L IF   E           K DISK
004900000000     F*------------
005000000000     F* Regole per estrazione dati
005100000000     F* Key: Soc/Regola/Assoc/Cau/Conto/Voce
005200130823     FSDGSPR04L IF   E           K DISK
005300000000     F*------------
005400000000     F* partite  key: SYS/NRASREG/NRRIGAM/NRRIGA
005500000000     FNDPAS01L  IF   E           K DISK
005600000000     F*-------------
005700000000     F* Key: SYS/NRASREG/NRRIGAM
005800000000     FNDMOA01L  IF   E           K DISK
005900000000     F*------------
006000000000     F* Dati anagrafici aggiuntivi
006100160523R224 X**** Key: Sogg/DtFiVl/Sys/NrAsInt      UNIQUE
006101160523R224 F* Key: Sogg/DtFiVl/Sys/NrAsInt/Dest      UNIQUE
006200000000     FSDGSO301L IF   E           K DISK
006300130826     F*------------
006400130826     F* Dati per elenchi
006500160201R195 X**** Key: Sys/NrAsReg/Anno/TpDoc/NatOpe
006600160201R195 F* Key: TpCom/Sys/NrAsReg/Anno/TpDoc/NatOpe
006700130826     FSDGSCF03L UF A E           K DISK    UsrOpn
006800130826     F                                     COMMIT
006900130826     F*------------
007000130826     F* Movimenti estratti
007100160201R195 X**** Key: Soc/Kcc/Ksc/Doc/Reg/Par/TpDoc/NatOpe
007200160201R195 F* Key: Soc/TpCom/Kcc/Ksc/Doc/Reg/Par/TpDoc/NatOpe
007300130826     FSDGSCF04L IF   E           K DISK    RENAME(SDGSCF000:SDGSCF04)
007400130826     F                                     USROPN
007500000000     F*------------
007600000000     F* Segnalazioni - Errori e avvertimenti
007700130820     FSDGSG52P1 O    E             PRINTER  OFLIND(*In41)
007800110720     F                                     USROPN
007900000000     F*-------------
008000131206R141 X**** Avvertimenti per registrazioni di autofatture escluse
008100131206R141 F* Avvertimenti per registrazioni di autofatture
008200130820     FSDGSG52P2 O    E             PRINTER  OFLIND(*In42)
008300000000     F*-------------
008400000000     F* Avvertimenti per dati fiscali non impostati
008500130820     FSDGSG52P3 O    E             PRINTER  OFLIND(*In43)
008600000000     D*****************************************************************
008700000000     D* Passaggio Parametri
008800000000     D KPJBA         E DS
008900110721     D*-------------
009000110721     D* Salvataggio Kpjbu per richiamo altri pgm
009100110721     D SavKPJBU        S            256
009200000000     D*-------------------------------------------------------*
009300000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
009400131206     D    SocSogg                     8    overlay(XScFGO:31)
009500000000     D*-------------
009600000000     D* DS Interna per dati di output di SDGSOCDR
009700000000     D XSOCDS          DS          1000
009800000000     D*-------------
009900000000     D* DS Esterna gestione messaggi
010000000000     D SDGMSGDS      E DS                  INZ
010100000000     D  STS          E                     EXTFLD(MSGSTS)
010200000000     D                                     DIM(333)
010300000000     D  JBA          E                     EXTFLD(MSGJBA)
010400000000     D                                     DIM(502)
010401160607R224 D*----------------
010402160607 "   D* Valori assunti
010403160607 "   D XMPEDS        E DS                  INZ
010404160607 "   D XMPEDS1       E DS                  INZ
010405160607 "   D*-------------
010406160607 "   D DSDatiAna       DS                  INZ
010407160607R224 D $Dest                          2A
010500000000     D*-------------------------------------------------------*
010600000000     D* Reperimento nome PGM
010700000000     D STATUS         SDS           333
010800000000     D  DSPGM            *Proc
010900000000     D  Utente               254    263
011000000000     D*-------------------------------------------------------*
011100000000     D* Parametri che ricevo dal lancio
011200130820     D SDGSG52DS     E DS
011300130820     D  ERRG52       E                     INZ('0')
011400000000     D*-------------------------------------------------------*
011500130822     D* Parametri per apertura file di stampa
011600130826     D SDG00500DS    E DS
011700131014R135 D*-------------
011800131014 "   D* Parametri per applicazione soglia a
011900131014 "   D* oeprazioni di Regime del Margine
012000131014R135 D SDG00507DS    E DS
012100151123R195 D*-------------
012200151123 "   D* Parametri per applicazione soglia a
012300151123 "   D* operazioni con operatori in Black List
012400151123R195 D SDG00508DS    E DS
012500130822     D*-------------
012600000000     D* Ds x fetch
012700000000     D NDIVADS       E DS                  Occurs(1000) Extname(NDIVA00F)
012800110902     D $X              S              5  0
012900000000     D*----------
013000000000     D StringaSql      DS          5000
013100130820     D SqlSCF          S           5000
013200000000     D SqlIVA          S           5000
013300110902     D SqlREG_MOV      S           5000
013400110902     D SqlREG_MOVDS    DS          5000
013500110902     D SqlREG_MOV1             1   1000
013600110902     D SqlREG_MOV2          1001   2000
013700110902     D SqlREG_MOV3          2001   3000
013800110902     D SqlREG_MOV4          3001   4000
013900110902     D SqlREG_MOV5          4001   5000
014000000000     D*----------
014100000000     D* Indice DsMultipla
014200000000     D $Ind            S              4  0
014300000000     D*---------
014400130820     D* per prtf SDGSG52P1 - errori
014500000000     D $TpErr1         S              2A   Inz
014600000000     D*---------
014700130820     D* per prtf SDGSG52P2 - autofatture scartate
014800000000     D SAVSys          S                   Like(IVASys    )
014900000000     D SAVNrAsReg      S                   Like(IVANrAsReg)
015000000000     D $Segno          S              1  0 Inz
015100000000     D*---------
015200000000     D* per reperimento tipologia operazione da contropartita
015300000000     D SAV2Sys         S                   Like(IVASys    )
015400000000     D SAV2NrAsReg     S                   Like(IVANrAsReg)
015500131206R141 D*---------
015600131206 "   D* per controllo dati movimento su NDMOV00F
015700131206 "   D SAV3Sys         S                   Like(IVASys    )
015800131206R141 D SAV3NrAsReg     S                   Like(IVANrAsReg)
015900160119R195 D*---------
016000160119 "   D* per controllo registrazione già scritta in SDGSCF00F
016100160119 "   D* e reperimento legame bolla/fattura
016200160119 "   D SAV4Sys         S                   Like(IVASys    )
016300160119R195 D SAV4NrAsReg     S                   Like(IVANrAsReg)
016400000000     D*------------------
016500000000     D* Controlli validità legende
016600000000     D X07ValDS      E DS
016700000000     D*-------------
016800130820     D* Parametri modulo SDGSPECF
016900130821     D SDGPA08DS     E DS
017000151201R195 D*-------------
017100151201 "   D* Parametri modulo SDBLACK2
017200151201R195 D SDGPA11DS     E DS
017300000000     D*-------------
017400000000     D* Tabella degli Stati
017500000000     D ANGG12DS      E DS
017600000000     D*-------------
017700000000     D* Tabella degli Stati Black List
017800000000     D SDGHD1DS      E DS
017900130903     D*-------------
018000130903     D* Tabella tipologie natura giuridica
018100130903     D ANGG16DS      E DS
018200000000     D*---------
018300000000     D NDIVA00F      E DS                  INZ
018400000000     D                                     Prefix(IV2:3)
018500000000     D*---------
018600000000     D NDMOV00F      E DS                  INZ
018700000000     D NDMOVSav      E DS                  Extname(NDMOV00F)
018800000000     D                                     Prefix(MOS:3)
018900000000     D NDMOVSav2     E DS                  Extname(NDMOV00F)
019000000000     D                                     Prefix(MO2:3)
019100000000     D*---------
019200000000     D NDREG00F      E DS                  INZ
019300000000     D*---------
019400000000     D* Definizioni parametri standard chiamate moduli
019500000000     D $Esito          S              1
019600000000     D $GesMsg         S              1
019700000000     D $LenOut         S              9B 0
019800000000     D $Struttura      S             10
019900000000     D $CodOper        S              1
020000000000     D $Output         S           4000
020100000000     D $Refresh        S              1
020200000000     D $Uso            S              1
020300000000     D*-------------
020400130918     D WStato          S                   Like(XScSta )
020500130918     D $CodUIC         S                   Like(CODUICG12)
020600130918     D $UICSoc         S                   Like(CODUICG12)
020700130918     D $UICKsc         S                   Like(CODUICG12)
020800130918     D $NazSoc         S                   Like(NazG12 )
020900000000     D $NazISO         S                   Like(NazG12 )
021000000000     D*---------
021100000000     D* dati conto
021200000000     D ND002DS       E DS
021300110902     D Attributi             131    250
021400110902     D*-------------
021500110902     D DsAttributi     DS           120
021600110902     D SkAttr                         4    Dim(30)
021700000000     D*-------------
021800000000     D* Ds per reperimento dati su causali operative contabili
021900000000     D ANOPE         E DS                  ExtName(ANOPE00F) Inz
022000000000     D*---------
022100000000     D* Variazioni estremi fiscali
022200000000     D ANCPI00F      E DS
022300000000     D*-------------
022400000000     D* Driver estremi fiscali (ANCPI00F)
022500000000     D DVPRic          S              1
022600000000     D DVPSocieta      S              3
022700000000     D DVPUnita        S              8
022800000000     D DVPStrutt       S             10
022900000000     D DVPDtRif        S               D
023000000000     D DVPLenOut       S              9B 0
023100000000     D DVPSogg         S              8
023200000000     D DVPErrore       S              1
023300000000     D DVPOutput       S           4000
023400000000     D*---------
023500000000     D* Dati soggetti
023600000000     D DVASog        E DS
023700000000     D* Dati fornitori
023800000000     D DVAFor        E DS
023900000000     D* Dati clienti
024000000000     D DVACli        E DS
024100000000     D*---------
024200000000     D* driver soggetti
024300000000     D DVARic          S              1
024400000000     D DVASocieta      S              3
024500000000     D DVAUnita        S              8
024600000000     D DVADtRif        S               D
024700000000     D DVAStrutt       S             10
024800000000     D DVALenOut       S              9B 0
024900000000     D DVASnatura      S              1
025000000000     D DVACodice       S              8
025100000000     D DVALineav       S              3
025200000000     D DVAFiliale      S              3  0
025300000000     D DVASogg         S              8
025400000000     D DVATpInd        S              2
025500000000     D DVACdInd        S              3
025600000000     D DVAOutput       S           4000
025700000000     D DVAErrore       S              1
025800000000     D*----------------------------------------------------*
025900000000     D* Driver estensione nominativi partite per conti tecnici
026000000000     D* (=conti estemporanei)
026100000000     D NDCPA00F      E DS                  INZ
026200000000     D $Sys            S                   Like(MOVSys)
026300000000     D $NrAsInt        S                   Like(MOVNrAsReg)
026400000000     D $NrAsGen        S                   Like(MOVNrAsReg)
026500000000     D*---------
026600000000     D* Richiamo a SDGTABDR
026700000000     D SDGTABDS      E DS
026800000000     D*---------
026900000000     D* Contabilità
027000000000     D ANGG84DS      E DS                  INZ
027100130926     D*-------------
027200130926     D* Passaggio Parametri al PGM di recupero dati
027300130926     D* anagrafici
027400130926     D SDGSG53DS     E DS                  INZ
027500160118R195 D*-------------
027600160118 "   D* Recupero legame bolla doganale/fattura
027700160118R195 D SDGSG54DS     E DS                  INZ
027800000000     D*---------
027900000000     D* Campi di lavoro
028000000000     D $NrRigaM        s                   like(IVANrRigaM) inz
028100130826     D $ExistSCF       S              1
028200130826     D $WriteSCF       S              1
028300151204R195 X**** $AnnullaSCF     S              1
028400130826     D $PrimaStp1      S              1    INZ(*On)
028500000000     D $PrimaStp2      S              1    INZ(*On)
028600000000     D $PrimaStp3      S              1    INZ(*On)
028700000000     D $OkConto        S              1    INZ('1')
028800000000     D $OkMOV          S              1    INZ('1')
028900000000     D $AnnConto       S              1    INZ('0')
029000000000     D $AnnMOV         S              1    INZ('0')
029100000000     D $AnnIVA         S              1    INZ('0')
029200131206R141 D $Autofatt       S              1    INZ('0')
029300150420R1786D $AutoCons       S              1    INZ('0')
029400150420R1786D***$CodUICAlfa     S              3A
029500130820     D $TpAnn          S                   Like(SCFTpAnn) INZ(*Blank)
029600000000     D $OkContrP       S              1    INZ('1')
029700000000     D $OkVoce         S              1    INZ('1')
029800130821     D $NatOpe         S                   Like(SCFNatOpe  )
029900130826     D $ImpDoc         S                   Like(MOVImporto)
030000130903     D $NatGiur        S                   Like(DVFNatGiur)
030100151130R195 D $Voce           S                   Like(MOAVoce   )
030200131014R135 D $FoundMarg      S              1
030300000000     D*---------
030400130924     D $OkRegBenSrv    S              1A   INZ('0')
030500130924     D $OkRegMargine   S              1A   INZ('0')
030600130924     D $OkRegReverse   S              1A   INZ('0')
030700130828     D $OkRegDoc       S              1A   INZ('0')
030800000000     D*---------
030900000000     D* reperimento dati fiscali anagrafici
031000000000     D SAVKccCP        S                   Like(IVAKccCP   )
031100000000     D SAVKscCP        S                   Like(IVAKscCP   )
031200000000     D SAVDtDoc        S                   Like(MOVDtDoc  )
031300000000     D*---------
031400000000     D* segnalazione  dati fiscali mancanti
031500000000     D SAVKcc2         S                   Like(MOVKcc    )
031600000000     D SAVKsc2         S                   Like(MOVKsc    )
031700000000     D SAVDtDoc2       S                   Like(MOVDtDoc  )
031800000000     D*---------
031900130903     D $TpNSog         S                   Like(SCFTpN  )
032000130820     D $CdFiscSog      S                   Like(SCFCdFisc )
032100130820     D $PartIVASog     S                   Like(SCFPartIVA)
032200130820     D $CogRagSog      S                   Like(SCFCogRag)
032300130820     D $NomeSog        S                   Like(SCFNome )
032400130820     D $DtNascSog      S                   Like(SCFDtNasc)
032500130820     D $LocNascSog     S                   Like(SCFLocNasc)
032600130820     D $PrvNascSog     S                   Like(SCFPrvNasc)
032700130820     D $StatoSog       S                   Like(SCFStato)
032800130820     D $LocalitSog     S                   Like(SCFLocalit)
032900130820     D $IndirESog      S                   Like(SCFIndirE)
033000000000     D*------
033100130903     D $TpN            S                   Like(SCFTpN  )
033200130820     D $CdFisc         S                   Like(SCFCdFisc )
033300130820     D $PartIVA        S                   Like(SCFPartIVA)
033400130820     D $CogRag         S                   Like(SCFCogRag)
033500130820     D $Nome           S                   Like(SCFNome )
033600130820     D $DtNasc         S                   Like(SCFDtNasc)
033700130820     D $LocNasc        S                   Like(SCFLocNasc)
033800130820     D $PrvNasc        S                   Like(SCFPrvNasc)
033900130820     D $Stato          S                   Like(SCFStato)
034000130820     D $Localit        S                   Like(SCFLocalit)
034100130820     D $IndirE         S                   Like(SCFIndirE)
034200000000     D*-------------
034300000000     D WSogg           S                   Like(DVASogg)
034400000000     D WKsc            S                   Like(DVACodice)
034500130820     D WKcc            S                   Like(SCFKcc)
034600000000     D WTpInd          S                   Like(DVATpInd)
034700000000     D WCdInd          S                   Like(DVACdInd)
034800000000     D WSNatura        S                   Like(DVASNatura)
034900131025R138 D SNaturaSav      S                   Like(DVASNatura)
035000000000     D $FineMOA        S              1    Inz(*Off)
035100000000     D $FineIVA        S              1    Inz(*Off)
035200110902     D $FineREG_MOV    S              1    Inz(*Off)
035300130820     D $FineSCF        S              1    Inz(*Off)
035400000000     D $Causale        S                   Like(MOVCausTes)
035500000000     D $FineScan       S              1    Inz(*Off)
035600000000     D $CampoScan      S             50A
035700000000     D SAVVoce         S                   Like(MOAVoce   )
035800151123R195 D $PrimoSCF       S              1A   Inz(*On)
035900160201 "   D SavCogRagR      S                   Like(SCFCogRagR)
036000160201 "   D SavNomeR        S                   Like(SCFNomeR  )
036100160201 "   D SavDtNasR       S                   Like(SCFDtNasR )
036200160201 "   D SavLocNasR      S                   Like(SCFLocNasR)
036300160201 "   D SavPrvNasR      S                   Like(SCFPrvNasR)
036400160201 "   D SavStatoR       S                   Like(SCFStatoR )
036500160201 "   D SavCodUICR      S                   Like(SCFCodUICR)
036600160201 "   D SavLocaliR      S                   Like(SCFLocaliR)
036700160201 "   D SavIndireR      S                   Like(SCFIndireR)
036800160201 "   D SavTpNR         S                   Like(SCFTpNR   )
036900160201 "   D SavPIVAR        S                   Like(SCFPIVAR  )
037000160201 "   D SavCdFiscR      S                   Like(SCFCdFiscR)
037100160201 "   D SavTpOpeR       S                   Like(SCFTpOpeR )
037200160201 "   D SavNatOpeR      S                   Like(SCFNatOpeR)
037300160201 "   D SavLeasinR      S                   Like(SCFLeasinR)
037400160201 "   D SavDtRegR       S                   Like(SCFDtRegR )
037500160201 "   D SavNrRegR       S                   Like(SCFNrRegR )
037600160201R195 D SavSerRegR      S                   Like(SCFSerRegR)
037700000000     D*---------
037800000000     D DataDs          DS
037900000000     D  AnnoDs                        4A
038000000000     D   Tratt1                       1A   Inz('-')
038100000000     D  MeseDs                        2A
038200000000     D   Tratt2                       1A   Inz('-')
038300000000     D  GiornoDs                      2A
038400000000     D*----------
038500000000     D $Data10A1       S             10
038600000000     D $Data10A2       S             10
038700111018     D $DataAnno       S               D
038800000000     D $Data           S               D
038900000000     D $Anno           S              4A
039000000000     D*-------------
039100000000     D UDateISO        S               D    datfmt(*iso)
039200000000     D $OkBeni         S              1A
039300130826     D $OkServizi      S              1A
039400130826     D $KscIncl        S              1A
039500130826     D $KscEscl        S              1A
039600000000     D SUMImponib      S                    Like(IVAImponib)
039700110902     D $ChkNDIVA       S              1A
039800000000     D*-------------
039900000000     D* campi di salvataggio riferimenti registrazione e partita
040000000000     D SavDtReg        S                    Like(REGDtReg  )
040100000000     D SavNrReg        S                    Like(REGNrReg  )
040200000000     D SavSerReg       S                    Like(REGSerReg )
040300000000     D SavDtPar        S                    Like(PASDtPar  )
040400000000     D SavNrPar        S                    Like(PASNrPar  )
040500000000     D SavSerPar       S                    Like(PASSerPar )
040600130826     D*-------------
040700130826     D* campi di salvataggio riferimenti registrazione e partita
040800130826     D $KccDoc         S                    Like(MOVKcc    )
040900130826     D $KscDoc         S                    Like(MOVKsc    )
041000130826     D $DtDoc          S                    Like(MOVDtDoc  )
041100130826     D $NrDoc          S                    Like(MOVNrDoc  )
041200130826     D $SerDoc         S                    Like(MOVSerDoc )
041300130826     D $DtReg          S                    Like(REGDtReg  )
041400130826     D $NrReg          S                    Like(REGNrReg  )
041500130826     D $SerReg         S                    Like(REGSerReg )
041600130826     D $DtPar          S                    Like(PASDtPar  )
041700130826     D $NrPar          S                    Like(PASNrPar  )
041800130826     D $SerPar         S                    Like(PASSerPar )
041900131031R140 D WData           S               D
042000131031R140 D WDtAlfa         S             10A
042100151203R195 D*-------------
042200151203 "   D* Sottotitolo di testata Black List
042300151203 "   D* "Spesometro"
042400151203 "   D SOTTITTE        C                   CONST('SDGMSG    *LIBL-
042500151203 "   D                                          SDC0087')
042600151203 "   D* Sottotitolo di testata Black List
042700151203 "   D* "Acquisti da san marino'
042800151203 "   D SOTTITTS        C                   CONST('SDGMSG    *LIBL-
042900151203 "   D                                          SDC0075')
043000151203 "   D* Sottotitolo di testata Black List
043100151203 "   D* "Black List"
043200151203 "   D SOTTITTB        C                   CONST('SDGMSG    *LIBL-
043300151203 "   D                                          SDC0076')
043400151203R195 D*-------------
043500000000     C******************************************************
043600000000     C*                MAIN LINE PROGRAM
043700000000     C******************************************************
043800000000     C*
043900000000     C                   EXSR      INZVAR
044000000000     C*
044100000000     C                   EXSR      CicloIVA
044200110902     C*
044300110902     C* recupero le importazioni senza movimenti NDIVA00F
044400110902     C                   EXSR      CicloREG_MOV
044500151123R195 C*
044600151123 "   C* se Black List, al cambio dei dati anagrafici
044700151123 "   C* controllo la soglia (solo per gli operatori di cui
044800151123 "   C* sono stati estratti dei movimenti, gli altri non li
044900151123 "   C* tocco). Qui controllo l'ultima ragione sociale inserita
045000151123 "   C                   if        TpComG52     = 'B'
045100151123 "   C*
045200151123 "   C                   if        $PrimoSCF   <> *On
045300151123 "   C*
045400151123 "   C                   EXSR      CtrSogliaBl
045500151123 "   C*
045600151123 "   C                   endif
045700151123 "   C*
045800151123R195 C                   else
045900131014R135 C*
046000131014 "   C* se anno 2012 o 2013, e trovato almeno un movimento
046100131014 "   C* soggetto a regime del margine, controllo la soglia
046200131014 "   C                   if        (AnnoG52     = 2012   or
046300131014 "   C                              AnnoG52     = 2013     ) and
046400131014 "   C                             $FoundMarg   = *On
046500131014 "   C                   EXSR      CtrSoglia
046600131014R135 C                   endif
046700151123R195 C*
046800151123R195 C                   endif
046900000000     C*
047000000000     C                   EXSR      ENDPGM
047100110902     C*
047200131014R135 C************************************************************
047300131014 "   C* Controllo soglia per Regime del Margine
047400131014 "   C************************************************************
047500131014R135 C     CtrSoglia     BEGSR
047600131014     C*
047700131014     C                   Clear                   SDG00507DS
047800131014     C*
047900131014     C                   Eval      Lr0507      = *On
048000131014     C*
048100131014     C                   Eval      Soc0507     = SocietaG52
048200131014     C                   Eval      Ctb0507     = CtbG52
048300131014     C                   Eval      TpRegz0507  = TpRegzG52
048400131014     C                   Eval      TpCom0507   = 'E'
048500131014     C                   Eval      Anno0507    = AnnoG52
048600131014     C                   Eval      Mese0507    = 0
048700151123R195 C                   Eval      Stato0507   = StatoG52
048800131014     C                   Eval      CF0507      = CFG52
048900131014     C                   Eval      PIVA0507    = PartIVAG52
049000131014     C                   Eval      CdFisc0507  = CdFiscG52
049100131014     C                   Eval      Sogg0507    = SoggG52
049200131014     C                   Eval      Kcc10507    = Kcc1G52
049300131014     C                   Eval      Kcc20507    = Kcc2G52
049400131014     C                   Eval      Kcc30507    = Kcc3G52
049500131014     C                   Eval      Kcc40507    = Kcc4G52
049600131014     C                   Eval      Kcc50507    = Kcc5G52
049700131014     C                   Eval      Kcc60507    = Kcc6G52
049800131014     C                   Eval      Kcc0507     = KccG52
049900131014     C                   Eval      Ksc0507     = KscG52
050000131014     C                   Eval      DelMan0507  = DelManuG52
050100131014     C                   Eval      StpAnn0507  = StpAnnG52
050200131014     C*
050300131014     C* salvo la kpjbu per ripristinarla dopo il richiamo allo u.e.
050400131014     C                   MoveL     KPJBU         SavKPJBU
050500131014     C*
050600131014     C                   MoveL     SDG00507DS    KPJBU
050700131014     C*
050800131014     C                   CALLB     'SDG00507R'
050900131014     C                   PARM                    KPJBA
051000131014     C*
051100131014     C                   MOVEL     KPJBU         SDG00507DS
051200131014     C*
051300131014     C* ripristino la kpjbu per non "turbare" i richiami successivi
051400131014     C* ad eventuali altri pgm che ricevono la kpjba
051500131014     C                   MoveL     SavKPJBU      KPJBU
051600131014     C*
051700131014R135 C                   ENDSR
051800131014R135 C/EJECT
051900000000     C************************************************************
052000000000     C* Fine programma
052100000000     C************************************************************
052200000000     C     ENDPGM        BEGSR
052300000000     C*
052400000000     C                   EXSR      CloseSql
052500000000     C*
052600130820     C                   MoveL     SDGSG52DS     KPJBU
052700000000     C*
052800130820     C                   if        %open (SDGSG52P1)
052900130820     C                   Close     SDGSG52P1
053000110720     C                   endif
053100000000     C                   Eval       *InLR      = *On
053200000000     C*
053300000000     C                   Return
053400000000     C*
053500000000     C                   ENDSR
053600000000     C/EJECT
053700000000     C************************************************************
053800000000     C* Ciclo di lettura di NDIVA00F
053900000000     C************************************************************
054000000000     C     CicloIVA      BEGSR
054100000000     C*
054200000000     C                   EXSR      RepSqlIVA
054300000000     C*
054400000000     C                   EXSR      OpenSqlIVA
054500000000     C*
054600000000     C                   EXSR      ExeSqlIVA
054700000000     C*
054800000000     C                   ENDSR
054900000000     C/EJECT
055000000000     C************************************************************
055100000000     C* Reperimento stringa sql su NDIVA00F
055200000000     C************************************************************
055300000000     C     RepSqlIVA     BEGSR
055400000000     C*
055500000000     C* SELECT * FROM NDIVA00F
055600000000     C* WHERE IVASOCIETA='XXX'
055700151203R195 C*  [se Spesometro o Acquisti da San Marino o richiesta
055800151203R195 C*   data di registrazione per Black List]
055900130820     C*  AND IVADTREG BETWEEN 'ANNOG52-01-01' AND 'ANNOG52-12-31'
056000151203R195 C*  [se richiesta data documento per Black List]
056100151203R195 C*  AND IVADTDOC BETWEEN 'ANNOG52-01-01' AND 'ANNOG52-12-31'
056200000000     C* -------------------------------------------------------------------
056300000000     C*  [se capoconti impostati]
056400130820     C*  and IVAKCCCP in (Kcc1G52, Kcc2G52,
056500130820     C*                   Kcc3G52, Kcc4G52,
056600130820     C*                   Kcc5G52, Kcc6G52 )
056700000000     C*  [se conto impostato]
056800130820     C*  and IVAKCCCP = KccG52  and IVAKscCP = KscG52
056900000000     C*
057000000000     C* -------------------------------------------------------------------
057100110826     C* ORDER BY IVAKCCCP, IVAKSCCP, IVADTREG, IVATPREG, IVASYS, IVANRASREG,
057200110826     C* IVANRRIGAM, IVANRRIGA  FOR FETCH ONLY
057300000000     C*
057400000000     C                   Clear                   SqlIVA
057500000000     C*
057600000000     C                   Eval      SqlIVA =
057700000000     C                              'SELECT * FROM NDIVA00F ' +
057800000000     C                             ' WHERE IVASOCIETA=' + '''' +
057900130820     C                             SocietaG52 + ''''
058000000000     C*
058100000000     C                   EXSR      RepPeriodo
058200000000     C*
058300151203R195 C* se Spesometro o Acquisti da San Marino o richiesta
058400151203 "   C* data di registrazione per Black List]
058500151203 "   C                   if        TpComG52    = 'E'       or
058600151203 "   C                             TpComG52    = 'S'       or
058700151203 "   C                             (TpComG52   = 'B'   and
058800151203R195 C                              TpDtRifA11 = 'R'      )
058900000000     C                   Eval      SqlIVA      = %trimr(SqlIVA) +
059000000000     C                             ' AND (IVADtReg'
059100151203R195 C                   endif
059200151203 "   C*
059300151203 "   C* se richiesta data documento per Black List
059400151203 "   C                   if        TpComG52    = 'B'   and
059500151203 "   C                             TpDtRifA11  = 'D'
059600151203 "   C                   Eval      SqlIVA      = %trimr(SqlIVA) +
059700151203 "   C                             ' AND (IVADtDoc'
059800151203R195 C                   endif
059900000000     C*
060000000000     C                   Eval      SqlIVA      = %trimr(SqlIVA) +
060100000000     C                             ' BETWEEN ''' + $Data10A1 +
060200000000     C                             ''' AND ''' + $Data10A2 + ''''
060300000000     C                             + ' ) '
060400000000     C*
060500130821     C                   if        Kcc1G52    <> *Blank   or
060600130820     C                             Kcc2G52    <> *Blank   or
060700130820     C                             Kcc3G52    <> *Blank   or
060800130820     C                             Kcc4G52    <> *Blank   or
060900130820     C                             Kcc5G52    <> *Blank   or
061000130820     C                             Kcc6G52    <> *Blank
061100000000     C*
061200000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
061300000000     C                             ' AND IVAKccCP   in (' + '''' +
061400130820     C                             Kcc1G52 + ''''  +
061500000000     C                             ' , '   + ''''  +
061600130820     C                             Kcc2G52 + ''''  +
061700000000     C                             ' , '   + ''''  +
061800130820     C                             Kcc3G52 + ''''  +
061900000000     C                             ' , '   + ''''  +
062000130820     C                             Kcc4G52 + ''''  +
062100000000     C                             ' , '   + ''''  +
062200130820     C                             Kcc5G52 + ''''  +
062300000000     C                             ' , '   + ''''  +
062400130820     C                             Kcc6G52 + ''''  +
062500000000     C                             ' ) '
062600000000     C                   endif
062700000000     C*
062800000000     C*
062900130820     C                   if        KccG52     <> *Blank
063000000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
063100000000     C                             ' AND IVAKccCP  =' + '''' +
063200130820     C                             KccG52 + ''''
063300000000     C                   endif
063400000000     C*
063500130820     C                   if        KscG52     <> *Blank
063600000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
063700000000     C                             ' AND IVAKscCP =' + '''' +
063800130820     C                             KscG52 + ''''
063900000000     C                   endif
064000000000     C*
064100000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
064200000000     C                               ' ORDER BY IVAKCCCP, '+
064300000000     C                               ' IVAKSCCP, IVADTREG,' +
064400000000     C                               ' IVATPREG,' +
064500110826     C                               ' IVASYS, IVANRASREG,' +
064600110826     C                               ' IVANRRIGAM, IVANRRIGA'
064700000000     C*
064800000000     C* For Fetch Only
064900000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
065000000000     C                               ' FOR FETCH ONLY'
065100000000     C*
065200000000     C                   ENDSR
065300000000     C/EJECT
065400000000     C******************************************************
065500000000     C* Reperisco le date di inizio e fine periodo
065600000000     C******************************************************
065700000000     C     RepPeriodo    BEGSR
065800000000     C*
065900000000     C                   Reset                   DataDs
066000000000     C                   Clear                   $Data10A1
066100000000     C*
066200140328R133 C                   Select
066300151130R133 C*
066400151130R195 C* Acquisti da San Marino
066500151130R133 C                   When      TpComG52      = 'S'
066600140328 "   C* Periodo Da
066700140328 "   C                   MoveL     AnnoG52       AnnoDs
066800140328 "   C                   MoveL     MeseG52       MeseDs
066900140328 "   C                   MoveL     '01'          GiornoDs
067000140328 "   C                   Move      DataDs        $Data10A1
067100140328 "   C*
067200140328 "   C* Periodo A
067300140328 "   C                   Clear                   $Data
067400140328 "   C                   Reset                   DataDs
067500140328 "   C                   Clear                   $Data10A2
067600140328 "   C*
067700140328 "   C                   MoveL     AnnoG52       AnnoDs
067800140328 "   C                   MoveL     MeseG52       MeseDs
067900140328 "   C                   MoveL     '01'          GiornoDs
068000140328 "   C                   Move      DataDs        $Data
068100140328 "   C* reperisco il fine mese
068200140328 "   C                   CALL      'XDATCD'
068300140328 "   C                   PARM      4             XCod              2 0
068400140328 "   C                   PARM                    $Data
068500140328 "   C*
068600140328 "   C                   Move      $Data         $Data10A2
068700140328 "   C*
068800140328R133 C                   Other
068900111018     C* limite "da"
069000130820     C                   MoveL     AnnoG52       AnnoDs
069100000000     C                   MoveL     '01'          MeseDs
069200000000     C                   MoveL     '01'          GiornoDs
069300000000     C                   Move      DataDs        $Data10A1
069400000000     C*
069500111018     C* limite "a"
069600000000     C                   Reset                   DataDs
069700000000     C                   Clear                   $Data10A2
069800111018     C*
069900130821     C                   MoveL     AnnoG52       AnnoDs
070000130821     C                   MoveL     '12'          MeseDs
070100130821     C                   MoveL     '31'          GiornoDs
070200130821     C                   Move      DataDs        $Data10A2
070300000000     C*
070400111018     C* fine anno
070500111018     C                   Reset                   DataDs
070600111018     C                   Clear                   $DataAnno
070700111018     C*
070800130820     C                   MoveL     AnnoG52       AnnoDs
070900111018     C                   MoveL     '12'          MeseDs
071000111018     C                   MoveL     '31'          GiornoDs
071100111018     C                   Move      DataDs        $DataAnno
071200140328R133 C*
071300140328R133 C                   EndSl
071400111018     C*
071500000000     C                   ENDSR
071600000000     C/EJECT
071700000000     C******************************************************
071800000000     C* Open cursore sql
071900000000     C******************************************************
072000000000     C     OpenSqlIVA    BEGSR
072100000000     C*
072200000000     C/EXEC SQL
072300000000     C+     PREPARE S1 FROM :SqlIVA
072400000000     C/END-EXEC
072500000000     C*
072600000000     C/EXEC SQL
072700000000     C+     Declare A1 cursor for S1
072800000000     C/END-EXEC
072900000000     C*
073000000000     C/EXEC SQL
073100000000     C+     OPEN A1
073200000000     C/END-EXEC
073300000000     C*
073400000000     C                   ENDSR
073500000000     C/EJECT
073600000000     C************************************************************
073700000000     C* Eseguo lettura dei movimenti IVA
073800000000     C************************************************************
073900000000     C     ExeSqlIVA     BEGSR
074000000000     C*
074100000000     C                   Move      *Off          $FineIVA
074200000000     C                   Z-Add     0             $Ind
074300000000     C                   Z-Add     0             SqlEr3
074400000000     C*
074500000000     C                   DoW       $FineIVA    = *Off
074600000000     C*
074700000000     C                   EXSR      FetchIVA
074800000000     C*
074900000000     C                   Select
075000000000     C*-----------------------
075100000000     C* OK
075200000000     C                   When      SqlCod     >= 0     and
075300000000     C                             SqlCod     <> 100
075400000000     C*
075500000000     C     $Ind          OCCUR     NDIVADS
075600000000     C*
075700110902     C                   Eval      $ChkNDIVA   = *On
075800000000     C                   EXSR      ElabIVA
075900000000     C*
076000000000     C*-----------------------
076100000000     C* fine lettura
076200000000     C                   When      SqlCod      = 100
076300000000     C*
076400000000     C                   COMMIT
076500000000     C*
076600000000     C* Stampa autofatture ultima riga totali
076601160314R1951C                   if        TpComG52   <> 'B'
076700000000     C                   EXSR      StpAutof_Tot
076701160314R1951C                   endif
076800000000     C*
076900000000     C                   Move      *On           $FineIVA
077000000000     C*
077100000000     C                   Other
077200000000     C*
077300000000     C*-----------------------
077400000000     C* si è verificato un errore
077500000000     C                   Eval      $TpErr1     = '99'
077600000000     C                   Eval      StringaSql  = SqlIVA
077700000000     C                   EXSR      StpErr1
077800130820     C                   Eval      ERRG52      = '1'
077900000000     C*
078000000000     C                   Move      *On           $FineIVA
078100000000     C                   endsl
078200000000     C*
078300000000     C                   endDo
078400000000     C*
078500000000     C                   ENDSR
078600000000     C/EJECT
078700000000     C******************************************************
078800000000     C* Eseguo lettura
078900000000     C******************************************************
079000000000     C     FetchIVA      BEGSR
079100000000     C*
079200000000     C     $Ind          Ifeq      SqlEr3
079300000000     C/EXEC SQL
079400000000     C+     FETCH A1 FOR 1000 ROWS INTO :NDIVADS
079500000000     C/END-EXEC
079600000000     C                   Z-ADD     1             $Ind
079700000000     C                   else
079800000000     C                   ADD       1             $Ind
079900000000     C                   endif
080000000000     C*
080100000000     C                   ENDSR
080200000000     C/EJECT
080300110902     C************************************************************
080400110902     C* Ciclo di lettura di NDREG00F/NDMOV00F
080500110902     C************************************************************
080600110902     C     CicloREG_MOV  BEGSR
080700110902     C*
080800110902     C                   EXSR      ClrNDIVADS
080900110902     C*
081000110902     C                   EXSR      RepSqlREG_MOV
081100110902     C*
081200110902     C                   EXSR      OpenSqlREG_MOV
081300110902     C*
081400111018     C                   EXSR      ExeSqlREG_MOV
081500110902     C*
081600110902     C                   ENDSR
081700110902     C/EJECT
081800110902     C************************************************************
081900110902     C* Clear NDIVADS
082000110902     C************************************************************
082100110902     C     ClrNDIVADS    BEGSR
082200110902     C*
082300110902     C* Pulisco la ds multipla
082400110902     C*
082500110902     C     1             Do        1000          $X
082600110902     C     $X            Occur     NDIVADS
082700110902     C                   Clear                   NDIVADS
082800110902     C                   endDo
082900110902     C*
083000110902     C                   ENDSR
083100110902     C/EJECT
083200110902     C************************************************************
083300110902     C* Reperimento stringa sql su NDREG00F e NDMOV00F
083400110902     C************************************************************
083500110902     C     RepSqlREG_MOV BEGSR
083600110902     C*
083700110902     C* SELECT
083800110902     C* REGNRASREG  IVANRASREG  ,
083900110902     C* MOVNRRIGAM  IVANRRIGAM  ,
084000110902     C* 1           IVANRRIGA   ,
084100110902     C* REGSYS      IVASYS      ,
084200110902     C* REGSOCIETA  IVASOCIETA  ,
084300110902     C* REGUNITA    IVAUNITA    ,
084400110902     C* ' '         IVAKCC      ,
084500110902     C* ' '         IVAKSC      ,
084600110902     C* REGDTREG    IVADTREG    ,
084700110902     C* REGNRREG    IVANRREG    ,
084800110902     C* REGSERREG   IVASERREG   ,
084900110902     C* MOVCAUSTES  IVACAUSTES  ,
085000110902     C* MOVDTDOC    IVADTDOC    ,
085100110902     C* MOVNRDOC    IVANRDOC    ,
085200110902     C* MOVSERDOC   IVASERDOC   ,
085300110902     C* ' '         IVACAUSRIG  ,
085400110902     C* '0001-01-01'  IVADTRIF  ,
085500110902     C* MOVAVERE    IVADARE     ,
085600110902     C* MOVDARE     IVAAVERE    ,
085700110902     C* 0           IVAIMPORTO  ,
085800110902     C* 0           IVAIMPORTD  ,
085900110902     C* MOVIMPORTO  IVAIMPONIB  ,
086000110902     C* MOVIMPORTD  IVAIMPOND   ,
086100110902     C* MOVKCC      IVAKCCCP    ,
086200110902     C* MOVKSC      IVAKSCCP    ,
086300110902     C* '0001-01-01'  IVADTCOMP ,
086400110902     C* ' '         IVADESBREV  ,
086500130826     C* '1'         IVATPREG    ,
086600110902     C* ' '         IVALIBRO    ,
086700110902     C* 0           IVAALIQ     ,
086800110902     C* ' '         IVARIFIVA   ,
086900110902     C* ' '         IVAINDETR   ,
087000110902     C* ' '         IVASOSP     ,
087100110902     C* '0001-01-01'  IVADTANNOT,
087200110902     C* ' '          IVAFLAG5A
087300110902     C*
087400110902     C* FROM NDREG00F join NODMOV00F on
087500110902     C* REGSYS = MOVSYS  and
087600110902     C* REGNRASREG = MOVNRASREG
087700110902     C* EXCEPTION JOIN NDIVA00F on
087800110902     C* REGSYS = IVASYS  and
087900110902     C* REGNRASREG = IVANRASREG
088000110902     C* WHERE REGSOCIETA= 'XXX'    and
088100110902     C*       REGANN    = '0'      and
088200110902     C*       REGDOCIVA = '1'      and
088300151130R195 C*  [se Spesometro o Acquisti da San Marino]
088400110902     C*    MOVSNATURA   = 'F'      and
088500151130R195 C*  [se Black list ]
088600151130R195 C*    (MOVSNATURA  = 'F'  or  MOVSNATURA = 'C')    and
088700110902     C*    MOVNRRIGAM   =  1       and
088800130820     C*  AND REGDTREG BETWEEN 'ANNOG52-01-01' AND 'ANNOG52-12-31'
088900110902     C* -------------------------------------------------------------------
089000110902     C*  [se capoconti impostati]
089100130820     C*  and REGKCC   in (Kcc1G52, Kcc2G52,
089200130820     C*                   Kcc3G52, Kcc4G52,
089300130820     C*                   Kcc5G52, Kcc6G52 )
089400110902     C*  [se conto impostato]
089500130820     C*  and MOVKCC   = KccG52  and MOVKsc   = KscG52
089600110902     C*
089700110902     C* -------------------------------------------------------------------
089800150420R1788X**** ORDER BY MOVKCC, MOVKSC, MOVDTREG, REGSYS, REGNRASREG,
089900150420R1788X**** MOVNRRIGAM  FOR FETCH ONLY
090000150420R1788C* ORDER BY IVAKCCCP, IVAKSCCP, IVADTREG, IVASYS, IVANRASREG
090100150420R1788C* FOR FETCH ONLY
090200110902     C*
090300110902     C                   Clear                   SqlREG_MOV
090400110902     C*
090500110902     C                   Eval      SqlREG_MOV =
090600110902     C                              'Select  ' +
090700110902     C                              'REGNRASREG  IVANRASREG, ' +
090800110902     C                              'MOVNRRIGAM  IVANRRIGAM, ' +
090900110902     C                              '1           IVANRRIGA,  ' +
091000110902     C                              'REGSYS      IVASYS,     ' +
091100110902     C                              'REGSOCIETA  IVASOCIETA, ' +
091200110902     C                              'REGUNITA    IVAUNITA,   ' +
091300110902     C                              '''' + '''' +  ' IVAKCC,     ' +
091400110902     C                              '''' + '''' +  ' IVAKSC,     ' +
091500110902     C                              'REGDTREG    IVADTREG,   ' +
091600110902     C                              'REGNRREG    IVANRREG,   ' +
091700110902     C                              'REGSERREG   IVASERREG,  ' +
091800110902     C                              'MOVCAUSTES  IVACAUSTES, ' +
091900110902     C                              'MOVDTDOC    IVADTDOC,   ' +
092000110902     C                              'MOVNRDOC    IVANRDOC,   ' +
092100110902     C                              'MOVSERDOC   IVASERDOC,  ' +
092200110902     C                              '''' + '''' +  ' IVACAUSRIG, ' +
092300110902     C                              '''' + '0001-01-01' + '''' +
092400110902     C                                ' IVADTRIF,  ' +
092500110902     C                              'MOVAVERE    IVADARE,    ' +
092600110902     C                              'MOVDARE     IVAAVERE,   ' +
092700110902     C                              '0           IVAIMPORTO, ' +
092800110902     C                              '0           IVAIMPORTD, ' +
092900110902     C                              'MOVIMPORTO  IVAIMPONIB, ' +
093000110902     C                              'MOVIMPORTD  IVAIMPOND,  ' +
093100110902     C                              'MOVKCC      IVAKCCCP,   ' +
093200110902     C                              'MOVKSC      IVAKSCCP,   ' +
093300110902     C                              '''' + '0001-01-01' + '''' +
093400110902     C                                ' IVADTCOMP, ' +
093500130826     C                              '''' + '''' +  ' IVADESBREV, ' +
093600130826     C                              '''' + '1' + '''' +
093700130826     C                                ' IVATPREG,  ' +
093800130826     C                              '''' + '''' +  ' IVALIBRO,   ' +
093900110902     C                              '0           IVAALIQ,    ' +
094000110902     C                              '''' + '''' +  ' IVARIFIVA,  ' +
094100110902     C                              '''' + '''' +  ' IVAINDETR,  ' +
094200110902     C                              '''' + '''' +  ' IVASOSP,    ' +
094300110902     C                              '''' + '0001-01-01' + '''' +
094400110902     C                                ' IVADTANNOT, ' +
094500110902     C                              '''' + '''' +  ' IVAFLAG5A   ' +
094600110902     C                              'from NDREG00F          ' +
094700110902     C                              'JOIN NDMOV00F  on      ' +
094800110902     C                              'REGSYS = MOVSYS  and   ' +
094900110902     C                              'REGNRASREG = MOVNRASREG ' +
095000110902     C                              'EXCEPTION JOIN NDIVA00F on ' +
095100110902     C                              'REGSYS = IVASYS  and   ' +
095200110902     C                              'REGNRASREG = IVANRASREG ' +
095300110902     C                             ' WHERE REGSOCIETA=' + '''' +
095400130820     C                             SocietaG52 + ''''
095500110902     C*
095600110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
095700110902     C                             ' AND  REGAnn    =' + '''' +
095800110902     C                             '0' + ''''
095900110902     C*
096000110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
096100110902     C                             ' AND  REGDocIVA =' + '''' +
096200110902     C                             '1' + ''''
096300110902     C*
096400151130R195 C                   Select
096500151130 "   C* Spesometro o Acquisti da San Marino
096600151130 "   C                   when      TpComG52    = 'E'       or
096700151130R195 C                             TpComG52    = 'S'
096800110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
096900110902     C                             ' AND  MOVSNatura=' + '''' +
097000110902     C                             'F' + ''''
097100151130R195 C*
097200151130 "   C* Black List
097300151130 "   C                   When      TpComG52    = 'B'
097400151130 "   C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
097500151130 "   C                             ' AND  (MOVSNatura=' + '''' +
097600151130 "   C                             'C' + '''' +
097700151203 "   C                             ' or    MOVSNatura=' + '''' +
097800151203 "   C                             'F' + '''' + ')'
097900151130R195 C                   endsl
098000110902     C*
098100110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
098200110902     C                             ' AND  MOVNRRIGAM= 1 '
098300110902     C*
098400110902     C                   EXSR      RepPeriodo
098500110902     C*
098600110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
098700110902     C                             ' AND (REGDtReg'
098800110902     C*
098900110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
099000110902     C                             ' BETWEEN ''' + $Data10A1 +
099100110902     C                             ''' AND ''' + $Data10A2 + ''''
099200110902     C                             + ' ) '
099300110902     C*
099400130820     C                   if        Kcc1G52    <> *Blank   or
099500130820     C                             Kcc2G52    <> *Blank   or
099600130820     C                             Kcc3G52    <> *Blank   or
099700130820     C                             Kcc4G52    <> *Blank   or
099800130820     C                             Kcc5G52    <> *Blank   or
099900130820     C                             Kcc6G52    <> *Blank
100000110902     C*
100100110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
100200110902     C                             ' AND MOVKcc     in (' + '''' +
100300130820     C                             Kcc1G52 + ''''  +
100400110902     C                             ' , '   + ''''  +
100500130820     C                             Kcc2G52 + ''''  +
100600110902     C                             ' , '   + ''''  +
100700130820     C                             Kcc3G52 + ''''  +
100800110902     C                             ' , '   + ''''  +
100900130820     C                             Kcc4G52 + ''''  +
101000110902     C                             ' , '   + ''''  +
101100130820     C                             Kcc5G52 + ''''  +
101200110902     C                             ' , '   + ''''  +
101300130820     C                             Kcc6G52 + ''''  +
101400110902     C                             ' ) '
101500110902     C                   endif
101600110902     C*
101700110902     C*
101800130820     C                   if        KccG52     <> *Blank
101900110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
102000110902     C                             ' AND MOVKcc    =' + '''' +
102100130820     C                             KccG52 + ''''
102200110902     C                   endif
102300110902     C*
102400130820     C                   if        KscG52     <> *Blank
102500110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
102600110902     C                             ' AND MOVKsc   =' + '''' +
102700130820     C                             KscG52 + ''''
102800110902     C                   endif
102900110902     C*
103000110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
103100150420R1788X****                           ' ORDER BY MOVKCC  , '+
103200150420 "   X****                           ' MOVKSC  , REGDTREG,' +
103300150420 "   X****                           ' REGSYS, REGNRASREG,' +
103400150420R1788X****                           ' MOVNRRIGAM'
103500150420R1788C                               ' ORDER BY IVAKCCCP, '+
103600150420 "   C                               ' IVAKSCCP, IVADTREG,' +
103700150420R1788C                               ' IVASYS, IVANRASREG '
103800110902     C*
103900110902     C* For Fetch Only
104000110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
104100110902     C                               ' FOR FETCH ONLY'
104200110902     C*
104300110902     C                   Move      SqlREG_MOV    SqlREG_MOVDS
104400110902     C*
104500110902     C                   ENDSR
104600110902     C/EJECT
104700110902     C******************************************************
104800110902     C* Open cursore sql
104900110902     C******************************************************
105000110902     C     OpenSqlREG_MOVBEGSR
105100110902     C*
105200110902     C/EXEC SQL
105300110902     C+     PREPARE S2 FROM :SqlREG_MOV
105400110902     C/END-EXEC
105500110902     C*
105600110902     C/EXEC SQL
105700110902     C+     Declare A2 cursor for S2
105800110902     C/END-EXEC
105900110902     C*
106000110902     C/EXEC SQL
106100110902     C+     OPEN A2
106200110902     C/END-EXEC
106300110902     C*
106400110902     C                   ENDSR
106500110902     C/EJECT
106600110902     C************************************************************
106700110902     C* Eseguo lettura dei movimenti NDREG00F e NDMOV00F
106800110902     C************************************************************
106900110902     C     ExeSqlREG_MOV BEGSR
107000110902     C*
107100110902     C                   Move      *Off          $FineREG_MOV
107200110902     C                   Z-Add     0             $Ind
107300110902     C                   Z-Add     0             SqlEr3
107400110902     C*
107500110902     C                   DoW       $FineREG_MOV= *Off
107600110902     C*
107700110902     C                   EXSR      FetchREG_MOV
107800110902     C*
107900110902     C                   Select
108000110902     C*-----------------------
108100110902     C* OK
108200110902     C                   When      SqlCod     >= 0     and
108300110902     C                             SqlCod     <> 100
108400110902     C*
108500110902     C     $Ind          OCCUR     NDIVADS
108600110902     C*
108700110902     C                   Eval      $ChkNDIVA   = *Off
108800110902     C                   EXSR      ElabIVA
108900110902     C*
109000110902     C*-----------------------
109100110902     C* fine lettura
109200110902     C                   When      SqlCod      = 100
109300110902     C*
109400110902     C                   COMMIT
109500110902     C*
109600110902     C* Stampa autofatture ultima riga totali
109601160314R1951C                   if        TpComG52   <> 'B'
109700110902     C                   EXSR      StpAutof_Tot
109701160314R1951C                   endif
109800110902     C*
109900110902     C                   Move      *On           $FineREG_MOV
110000110902     C*
110100110902     C                   Other
110200110902     C*
110300110902     C*-----------------------
110400110902     C* si è verificato un errore
110500110902     C                   Eval      $TpErr1     = '99'
110600151203R195 X****               Eval      StringaSql  = SqlREG_MOV
110700151203R195 C                   MoveL(P)  SqlREG_MOV    StringaSql
110800110902     C                   EXSR      StpErr1
110900130820     C                   Eval      ERRG52      = '1'
111000110902     C*
111100110902     C                   Move      *On           $FineREG_MOV
111200110902     C                   endsl
111300110902     C*
111400110902     C                   endDo
111500110902     C*
111600110902     C                   ENDSR
111700110902     C/EJECT
111800110902     C******************************************************
111900110902     C* Eseguo lettura
112000110902     C******************************************************
112100110902     C     FetchREG_MOV  BEGSR
112200110902     C*
112300110902     C     $Ind          Ifeq      SqlEr3
112400110902     C/EXEC SQL
112500110902     C+     FETCH A2 FOR 1000 ROWS INTO :NDIVADS
112600110902     C/END-EXEC
112700110902     C                   Z-ADD     1             $Ind
112800110902     C                   else
112900110902     C                   ADD       1             $Ind
113000110902     C                   endif
113100110902     C*
113200110902     C                   ENDSR
113300110902     C/EJECT
113400000000     C********************************************************************
113500000000     C* Elaboro i movimenti IVA
113600000000     C********************************************************************
113700130826     C     ElabIVA       BEGSR
113800130826     C*
113900160119R195 C*
114000160119 "   C                   IF        IVASys     <> SAV4Sys   or
114100160119 "   C                             IVANrAsReg <> SAV4NrAsReg
114200160129 "   C*
114300160129 "   C*
114400160129 "   C                   Clear                   SDGSG54DS
114500160129R195 C*
114600130826     C* verifico se il documento era già stato estratto precedentemente
114700130826     C                   EXSR      CtrExistSCF2
114701160405R2212C*
114702160405 "   C                   Move      IVASys        SAV4Sys
114703160405 "   C                   Move      IVANrAsReg    SAV4NrAsReg
114704160405 "   C                   endif
114705160405R2212C*
114800160126R195 C*----------------------
114900160126 "   C                   if        $WriteSCF   = *On
115000160126 "   C* se Black List, controllo se presente legame
115100160126 "   C* "bolla doganale/fattura"
115200160126 "   C                   if        TpComG52    = 'B'
115300160126 "   C                   EXSR      RepLegame
115400160126 "   C                   endif
115500160126 "   C                   endif
115600160405R195 C*
115700160405R2212X*ex R195           Move      IVASys        SAV4Sys
115800160405 "   X*ex R195           Move      IVANrAsReg    SAV4NrAsReg
115900160405R2212X*ex R195           endif
116000160119R195 C*
116100130826     C                   if        $WriteSCF   = *On
116200130826     C*
116300130826     C                   EXSR      ChkConto
116400000000     C*
116500000000     C*-------------------
116600000000     C                   if        $OkConto    = *On
116700160201     C                   EXSR      ChkNDMOV
116800000000     C                   else
116900000000     C                   Eval      $OkMOV      = *Off
117000000000     C                   endif
117100151204R195 C*
117200151204 "   C                   if        $OkMOV      = *Off
117300151204 "   C                   Eval      $WriteSCF   = *Off
117400151204R195 C                   endif
117500000000     C*
117600000000     C*-------------------
117700000000     C                   if        $OkMOV      = *On
117800110902     C                   if        $ChkNDIVA   = *On
117900160201     C                   EXSR      ChkNDIVA
118000110902     C*
118100110902     C                   else
118200110902     C*
118300110902     C                   EXSR      InzNDIVA
118400110902     C                   endif
118500151204R195 C
118600151204R195 C                   endif
118700000000     C*
118800130826     C*
118900130826     C* ---------------
119000151204R195 X*ex R133           if        $WriteSCF   = *ON
119100151204 "   X**** verifico se il documento era già stato estratto precedentemente
119200151204 "   X***                EXSR      CtrExistSCF
119300151204R195 X*ex R133           endif
119400130826     C*
119500130826     C                   if        $WriteSCF   = *ON
119600130826     C* SCRIVO SDGSCF00F da NDIVA
119700130826     C                   EXSR      GesSCF_IVA
119800130826     C*
119900130826     C                   endif
120000151204R195 X****
120100151204R195 X****               endif
120200130826     C*
120300130826     C                   endif
120400130826     C*
120500130826     C                   ENDSR
120600130826     C/EJECT
120700130826     C************************************************************
120800130826     C* Controllo se registrazione già estratta e attiva
120900130826     C************************************************************
121000130826     C     CtrExistSCF2  BEGSR
121100111018     C*
121200130820     C                   Eval      $ExistSCF   = *Off
121300130820     C                   Eval      $WriteSCF   = *On
121400151204R195 X****               Eval      $AnnullaSCF = *Off
121500111012     C*
121600160201R195 C                   Move      TpComG52      SCFTpCom
121700130820     C                   Z-Add     IVASys        SCFSys
121800130820     C                   Z-Add     IVANrAsReg    SCFNrAsReg
121900160201R195 X**** K02SCF03      SETLL     SDGSCF03L                              21
122000160201R195 X**** K02SCF03      READE     SDGSCF03L                              21
122100160201R195 C     K03SCF03      SETLL     SDGSCF03L                              21
122200160201R195 C     K03SCF03      READE     SDGSCF03L                              21
122300130820     C                   Eval      $ExistSCF   = Not(*In21)
122400111013     C*
122500130820     C                   Eval      $FineSCF    = *In21
122600130820     C                   DoW       $FineSCF    = *Off
122700111013     C*
122800150420R1784X*ex R133           If        SCFTpCom = TpComG52
122900130820     C                   EXSR      GestExistSCF
123000150420R1784X*ex R133           EndIf
123100111013     C*
123200160201R195 X**** K02SCF03      READE     SDGSCF03L                              21
123201160405R2216C                   if        $FineSCF    = *Off
123300160201R195 C     K03SCF03      READE     SDGSCF03L                              21
123400130820     C                   Eval      $FineSCF    = *In21
123401160405R2216C                   endif
123500111013     C                   endDo
123600111012     C*
123700111013     C*
123800151204R195 X**** se NON trovato, provo con la chiave documento
123900151204R195 X****               if        $ExistSCF   = *Off
124000111013     C*
124100111013     C                   EXSR      RepNDMOV2
124200111013     C                   EXSR      RepNDREG2
124300111013     C                   EXSR      RepNDPAS2
124400111012     C*
124500151204R195 C* se NON trovato, provo con la chiave documento
124600151204 "   C                   if        $ExistSCF   = *Off
124700151204R195 C*
124800130820     C                   Eval      SCFSocieta  = IVASocieta
124900160201R195 C                   Eval      SCFTpCom    = TpComG52
125000130820     C                   Eval      SCFKCC      = $KccDoc
125100130820     C                   Eval      SCFKSC      = $KscDoc
125200130820     C                   Eval      SCFDTDOC    = $DtDoc
125300130820     C                   Eval      SCFNRDOC    = $NrDoc
125400130820     C                   Eval      SCFSERDOC   = $SerDoc
125500130820     C                   Eval      SCFDTREG    = $DtReg
125600130820     C                   Eval      SCFNRREG    = $NrReg
125700130820     C                   Eval      SCFSERREG   = $SerReg
125800130820     C                   Eval      SCFDTPAR    = $DtPar
125900130820     C                   Eval      SCFNRPAR    = $NrPar
126000130820     C                   Eval      SCFSERPAR   = $SerPar
126100160201R195 X**** K12SCF04      SETLL     SDGSCF04L                              21
126200160201R195 X**** K12SCF04      READE     SDGSCF04L                              21
126300160201R195 C     K13SCF04      SETLL     SDGSCF04L                              21
126400160201R195 C     K13SCF04      READE     SDGSCF04L                              21
126500130820     C                   Eval      $ExistSCF   = Not(*In21)
126600111013     C*
126700130820     C                   Eval      $FineSCF    = *In21
126800130820     C                   DoW       $FineSCF    = *Off
126900111013     C*
127000130820     C                   EXSR      GestExistSCF
127100111013     C*
127200160201R195 X**** K12SCF04      READE     SDGSCF04L                              21
127201160405R2216C                   if        $FineSCF    = *Off
127300160201R195 C     K13SCF04      READE     SDGSCF04L                              21
127400130820     C                   Eval      $FineSCF    = *In21
127401160405R2216C                   endif
127500111013     C                   endDo
127600111013     C*
127700111013     C                   endif
127800130826     C*
127900130826     C                   ENDSR
128000130826     C/EJECT
128100130826     C************************************************************
128200130826     c* Reperisce dati del movimento del cliente/fornitore
128300130826     C************************************************************
128400130826     C     RepNDMOV2     BEGSR
128500111013     C*
128600111013     C                   Eval      $KccDoc     = *Blank
128700111013     C                   Eval      $KscDoc     = *Blank
128800111013     C                   Eval      $DtDoc      = *Loval
128900111013     C                   Eval      $NrDoc      = 0
129000111013     C                   Eval      $SerDoc     = *Blank
129100111013     C*
129200111013     C                   Eval      $LenOut     = %size(NDMOV00F)
129300111013     C                   CALLB     'NDDRVMOV'
129400111013     C                   PARM                    IVASys
129500111013     C                   PARM                    IVANrAsReg
129600111013     C* cerco la riga del cliente/fornitore (è sempre la num.1)
129700111013     C* quella dell'IVA è sempre la 3
129800111013     C                   PARM      1             $NrRigaM
129900111013     C                   PARM      'NDMOV     '  $Struttura
130000111013     C                   PARM                    $LenOut
130100111013     C                   PARM      '1'           $CodOper
130200111013     C                   PARM                    NDMOV00F
130300111013     C*
130400111013     C                   Eval      $KccDoc     = MOVKcc
130500111013     C                   Eval      $KscDoc     = MOVKsc
130600111013     C                   Eval      $DtDoc      = MOVDtDoc
130700111013     C                   Eval      $NrDoc      = MOVNrDoc
130800111013     C                   Eval      $SerDoc     = MOVSerDoc
130900111013     C*
131000130826     C                   ENDSR
131100130826     C/EJECT
131200130826     C************************************************************
131300130826     C* Reperimento dei dati di registrazione
131400130826     C************************************************************
131500130826     C     RepNDREG2     BEGSR
131600111013     C*
131700111013     C                   Eval      $DtReg      = *Loval
131800111013     C                   Eval      $NrReg      = 0
131900111013     C                   Eval      $SerReg     = *Blank
132000111013     C*
132100111013     C                   Clear                   NDREG00F
132200111013     C*
132300111013     C                   EVAL      $LenOut     = %Size(NDREG00F)
132400111013     C*
132500111013     C                   CALLB     'NDDRVREG'
132600111013     C                   PARM                    IVASys
132700111013     C                   PARM                    IVANrAsReg
132800111013     C                   PARM      'NDREG     '  $Struttura
132900111013     C                   PARM                    $LenOut
133000111013     C                   PARM      *Blank        $CodOper
133100111013     C                   PARM                    $Output
133200111013     C                   PARM                    NDREG00F
133300111013     C*
133400111013     C                   if        $CodOper   <> '9'
133500111013     C                   EVAL      %subst(NDREG00F:1:$LenOut)
133600111013     C                             = %subst($Output:1:$LenOut)
133700111013     C*
133800111013     C                   Eval      $DtReg      = REGDtReg
133900111013     C                   Eval      $NrReg      = REGNrReg
134000111013     C                   Eval      $SerReg     = REGSerReg
134100111013     C*
134200111013     C                   endif
134300130826     C*
134400130826     C                   ENDSR
134500130826     C/EJECT
134600130826     C************************************************************
134700130826     C* Reperimento dei dati di partita
134800130826     C************************************************************
134900130826     C     RepNDPAS2     BEGSR
135000111013     C*
135100111013     C                   Eval      $DtPar      = *Loval
135200111013     C                   Eval      $NrPar      = 0
135300111013     C                   Eval      $SerPar     = *Blank
135400111013     C*
135500111013     C                   Eval      PASSys      = IVASys
135600111013     C                   Eval      PASNrAsReg  = IVANrAsReg
135700111013     C                   Eval      PASNrRigaM  = 1
135800111013     C     K03PAS01      SETLL     NDPAS01L
135900111013     C     K03PAS01      READE     NDPAS01L                               21
136000111013     C                   if        *In21       = *Off
136100111013     C                   Eval      $DtPar      = PASDtPar
136200111013     C                   Eval      $NrPar      = PASNrPar
136300111013     C                   Eval      $SerPar     = PASSerPar
136400111013     C                   endif
136500130826     C*
136600130826     C                   ENDSR
136700130826     C/EJECT
136800130826     C************************************************************
136900130826     c* Gestione documento trovato nei dati estratti
137000130826     C************************************************************
137100130826     C     GestExistSCF  BEGSR
137200130826     C*
137300150420R1784C* controllo solo se il movimento trovato è della
137400150407 "   C* Comunicazione che sto facendo ora
137500150420R1784C                   If        SCFTpCom = TpComG52
137600130826     C* se è manuale, non faccio nulla
137700130826     C                   if        SCFOrigine  = 'M'
137800130826     C                   Eval      $WriteSCF   = *Off
137900130826     C                   Eval      $FineSCF    = *On
138000130826     C                   endif
138100150420R1784C                   endif
138200130826     C*
138300130826     C                   ENDSR
138400130826     C/EJECT
138500160119R195 C************************************************************
138600160119 "   C* Reperimento legame bolla doganale/fattura
138700160119 "   C************************************************************
138800160119R195 C     RepLegame     BEGSR
138900160119     C*
139000160125     C                   Eval      WKcc        = IVAKccCP
139100160125     C                   Eval      WKsc        = IVAKscCP
139101160711R2314C                   Eval      $Data       = *Loval
139200160125     C                   EXSR      CallMVC002
139300160125     C*
139400160125     C                   if        $Esito     <> *On     and
139500160125     C                             SNatura002  = 'F'
139600160119     C*
139700160119     C                   Eval      SocietaG54  = XScSoc
139800160119     C                   Eval      SysG54      = IVASys
139900160119     C                   Eval      NrAsRegG54  = IVANrAsReg
140000160119     C                   if        AlleIVA002  = '02'
140100160119     C                   Eval      TpDocG54    = 'B'
140200160119     C                   else
140300160125     C                   Eval      TpDocG54    = 'F'
140400160119     C                   endif
140500160125     C                   Eval      TpDtRifG54  = TpDtRifA11
140600160119     C*
140700160125     C                   MoveL     SDGSG54DS     KPJBU
140800160125     C*
140900160119     C                   CALL      'SDGSG54R'
141000160119     C                   PARM                    KPJBA
141100160119     C*
141200160119     C                   MoveL     KPJBU         SDGSG54DS
141300160119     C*
141400160126     C* se Dogana, recupero il conto del fornitore se presente
141500160303     C                   if        AlleIVA002  = '02'    and
141600160303     C                             KccG54     <> *Blank  and
141700160303     C                             KscG54     <> *Blank
141701160711R2314X****               Eval      IVAKccCp    = KccG54
141702160711R2314X****               Eval      IVAKscCp    = KscG54
142000160126     C                   endif
142100160125     C*
142200160125     C                   endif
142300160119     C*
142400160119R195 C                   ENDSR
142500160119R195 C/EJECT
142600000000     C************************************************************
142700000000     C* Controllo dati conto
142800000000     C************************************************************
142900000000     C     ChkConto      BEGSR
143000000000     C*
143100000000     C                   IF        IVAKccCP   <> SAVKccCP or
143200000000     C                             IVAKscCP   <> SAVKscCP
143201160711R2322C                             OR
143202160711 "   C                             (TpComG52    = 'B'     and
143203170822R2322C                              AlleIVA002  = '02'        )
143300000000     C*
143400000000     C                   Move      IVAKccCP      SAVKccCP
143500000000     C                   Move      IVAKscCP      SAVKscCP
143600000000     C*
143700000000     C                   EXSR      InzConto
143800000000     C*
143900000000     C                   Eval      WKcc        = IVAKccCP
144000000000     C                   Eval      WKsc        = IVAKscCP
144100000000     C*
144200151204R195 X****               Eval      $Data       = UDateIso
144300151204R195 C                   Eval      $Data       = *Loval
144400110902     C                   Clear                   DsAttributi
144500000000     C                   EXSR      CallMVC002
144600000000     C*
144700000000     C                   if        $Esito      = *On
144800131025R138 C                   Eval      SNaturaSav  = *Blank
144900000000     C* annullo il conto
145000000000     C                   Eval      $AnnConto   = *On
145100000000     C                   if        $TpAnn      = *Blank
145200000000     C                   Eval      $TpAnn      = '07'
145300000000     C* stampo avvertimento per mancato reperimento dati conto
145400160418R221AX****               EXSR      StpErr1
145500000000     C                   endif
145600110902     C                   else
145700110902     C                   MoveL     Attributi     DsAttributi
145800131025R138 C                   Eval      SNaturaSav  = SNatura002
145900000000     C                   endif
146000150420R1786C*----------------------
146100150420 "   C*
146200150420 "   C                   if        $OkConto    = *On
146300150420 "   C                   EXSR      SelConto
146400150420R1786C                   endif
146500140306R133 C*
146600140306  "  C*----------------------
146700140306  "  C* se comunicazione di Acquisti da San Marino,
146800140306  "  C* scarto i clienti
146900140306  "  C                   Eval      $WriteSCF   = *On
147000140306  "  C                   if        $OkConto    = *On
147100140306  "  C                   if        TpComG52    = 'S'     and
147200140306  "  C                             SNatura002 <> 'F'
147300140306  "  C                   Eval      $WriteSCF   = *Off
147400140306  "  C                   Eval      $OkConto    = *Off
147500140306  "  C                   endif
147600140306R133 C                   endif
147700000000     C*
147800000000     C*----------------------
147900000000     C*
148000160201     C                   if        $OkConto    = *On
148100000000     C* reperimento dati fiscali dipendenti solo dal conto
148200000000     C                   EXSR      RepDati_Conto
148300151222R195 X*ex R133           if        TpComG52    = 'S'
148400151222R133 C                   MoveL     $StatoSog     $Stato
148500151222R133 C                   EXSR      RepCodUIC
148600151222R195 C* Acquisti da San Marino
148700151222R195 C                   if        TpComG52    = 'S'
148800151222R133 C                   If        $UICKsc    <> 037
148900140526  "  C                   Eval      $OkConto    = *Off
149000140526  "  C                   endif
149100140526R133 C                   endif
149200160201     C                   endif
149300000000     C*
149400000000     C*----------------------
149500000000     C*
149600150420R1786X****               if        $OkConto    = *On
149700150420 "   X****               EXSR      SelConto
149800150420R1786X****               endif
149900000000     C*
150000000000     C*----------------------
150100000000     C*
150200000000     C                   ENDIF
150300000000     C*
150400000000     C                   ENDSR
150500000000     C/EJECT
150600140526     C************************************************************
150700000000     C* Inizializzo variabili appoggio per dati conto cli/for
150800000000     C************************************************************
150900000000     C     InzConto      BEGSR
151000000000     C*
151100000000     C                   Eval      $OkConto    = *On
151200000000     C                   Eval      $AnnConto   = *Off
151300000000     C                   Eval      $TpAnn      = *Blank
151400000000     C*
151500130903     C                   Clear                   $TpNSog
151600000000     C                   Clear                   $CdFiscSog
151700000000     C                   Clear                   $PartIVASog
151800000000     C                   Clear                   $CogRagSog
151900000000     C                   Clear                   $NomeSog
152000000000     C                   Clear                   $DtNascSog
152100000000     C                   Clear                   $LocNascSog
152200000000     C                   Clear                   $PrvNascSog
152300000000     C                   Clear                   $StatoSog
152400000000     C                   Clear                   $LocalitSog
152500000000     C                   Clear                   $IndirESog
152600000000     C*
152700130903     C                   Clear                   $TpN
152800000000     C                   Clear                   $CdFisc
152900000000     C                   Clear                   $PartIVA
153000000000     C                   Clear                   $CogRag
153100130903     C                   Clear                   $Nome
153200000000     C                   Clear                   $DtNasc
153300000000     C                   Clear                   $LocNasc
153400000000     C                   Clear                   $PrvNasc
153500000000     C                   Clear                   $Stato
153600000000     C                   Clear                   $Localit
153700000000     C                   Clear                   $IndirE
153800000000     C*
153900000000     C* SAVDtDoc serve per reperire i dati fiscali
154000000000     C* storicizzati per il soggetto legato al conto
154100000000     C                   Move      *Loval        SAVDtDoc
154200000000     C*
154300000000     C* SAVDtDoc2 serve per stampare i dati fiscali
154400000000     C* non valorizzati sul soggetto legato al conto
154500000000     C                   Move      *Loval        SAVDtDoc2
154600131107R140 C*
154700131107R140 C                   Eval      $ImpDoc     = 0
154800000000     C*
154900000000     C                   ENDSR
155000000000     C/EJECT
155100000000     C***********************************************************
155200000000     C* Driver per controllo conto
155300000000     C***********************************************************
155400000000     C     CallMvc002    BEGSR
155500000000     C*
155600000000     C                   Clear                   ND002DS
155700000000     C*
155800000000     C                   Eval      $LenOut    =  %Size(ND002DS)
155900000000     C*
156000000000     C                   CallB     'NDMVC002'
156100000000     C                   PARM                    XScSoc
156200000000     C                   PARM                    WKCC
156300000000     C                   PARM                    WKSC
156400000000     C                   PARM                    $Data
156500000000     C                   PARM      *Off          $GesMsg
156600000000     C                   PARM      *Off          $Esito
156700000000     C                   PARM      'ND002DS   '  $Struttura
156800000000     C                   PARM                    ND002DS
156900000000     C                   PARM                    $LenOut
157000000000     C*
157100000000     C                   ENDSR
157200000000     C/EJECT
157300000000     C************************************************************
157400000000     C* Reperimento dati fiscali del conto
157500000000     C************************************************************
157600000000     C     RepDati_Conto BEGSR
157700000000     C*
157701160711R2322C*
157702160711 "   C* se Black List e se è Dogana, recupero i dati anagrafici
157703160711 "   C* e fiscali del soggetto fornitore
157704160711 "   C                   if        TpComG52    = 'B'     and
157705160711 "   C                             AlleIVA002  = '02'    and
157706160711 "   C                             KccG54      = *Blank  and
157707160711 "   C                             KscG54      = *Blank  and
157708160711 "   C                             SoggG54    <> *Blank
157709160711 "   C                   Eval      WSogg       = SoggG54
157710160711R2322C                   else
157800000000     C                   Eval      WSogg       = Sogg002
157801160711R2322C                   endif
157900000000     C                   Eval      WSNatura    = SNatura002
158000130826     C                   Eval      WTpInd      = *Blank
158100130826     C                   Eval      WCdInd      = *Blank
158200000000     C*
158300000000     C                   select
158400000000     C                   when      WSNatura    = 'F'
158500000000     C                   MoveL     'DVAFOR    '  DVAStrutt
158600000000     C                   Eval      DVALenout   = %Size(DVAFor)
158700000000     C*
158800000000     C                   when      WSNatura    = 'C'
158900000000     C                   MoveL     'DVACLI    '  DVAStrutt
159000000000     C                   Eval      DVALenout   = %Size(DVACli)
159100000000     C                   endsl
159200000000     C*
159300000000     C                   EXSR      CallDvaSog
159400000000     C*
159500000000     C                   if        DVAErrore  <> *Off
159600000000     C* annullo il conto
159700000000     C                   Eval      $AnnConto   = *On
159800000000     C                   if        $TpAnn      = *Blank
159900130821     C                   Eval      $TpAnn      = '07'
160000160418R221AX****               EXSR      StpErr1
160100000000     C                   endif
160200000000     C*
160300000000     C                   else
160400000000     C*
160500000000     C                   Select
160600000000     C                   when      SNatura002  = 'F'
160700000000     C                   EXSR      RieDvaFor
160800000000     C                   when      SNatura002  = 'C'
160900000000     C                   EXSR      RieDvaCli
161000000000     C                   endSl
161100000000     C*
161200000000     C                   endif
161300000000     C*
161400000000     C                   ENDSR
161500000000     C/EJECT
161600000000     C************************************************************
161700000000     C* Chiamata al driver dei soggetti
161800000000     C************************************************************
161900000000     C     CallDvaSog    BEGSR
162000000000     C*
162100000000     C                   CALLB     'NDDVASOG'
162200000000     C                   PARM      '1'           DVARic
162300000000     C                   PARM      XScSoc        DVASocieta
162400000000     C                   PARM      *Blank        DVAUnita
162500000000     C                   PARM                    DVAStrutt
162600000000     C                   PARM                    DVADtRif
162700000000     C                   PARM                    DVALenOut
162800000000     C                   PARM      WSNatura      DVASNatura
162900000000     C                   PARM      WKsc          DVACodice
163000000000     C                   PARM      *Blank        DVALineaV
163100000000     C                   PARM      *Zero         DVAFiliale
163200000000     C                   PARM      WSogg         DVASogg
163300000000     C                   PARM      WTpInd        DVATpInd
163400000000     C                   PARM      WCdInd        DVACdInd
163500000000     C                   PARM      *Off          DVAErrore
163600000000     C                   PARM                    DVAOutPut
163700000000     C*
163800000000     C                   ENDSR
163900000000     C/EJECT
164000000000     C************************************************************
164100000000     C* Dati del fornitore
164200000000     C************************************************************
164300000000     C     RieDvaFor     BEGSR
164400000000     C*
164500000000     C                   Eval      %subst(DvaFor:1:DVALenout)
164600000000     C                              = %subst(DVAOutput:1:DVALenout)
164700000000     C*
164800130903     C                   Eval      $NatGiur    = DVFNatGiur
164900130903     C                   EXSR      RepTpN
165000130903     C                   if        XTbErr      = '0'
165100130903     C                   MoveL     XTbUni        ANGG16DS
165200130903     C                   Eval      $TpNSog     = TpNG16
165300130903     C                   endif
165400130903     C*
165500000000     C                   MoveL     DVFPartIva    $PartIVASog
165600000000     C                   MoveL     DVFCdFisc     $CdFiscSog
165700000000     C                   MoveL     DVFDes        $CogRagSog
165800000000     C                   MoveL     *Blank        $NomeSog
165900000000     C                   Move      DVFDtNasc     $DtNascSog
166000000000     C                   MoveL     *Blank        $LocNascSog
166100000000     C                   MoveL     *Blank        $PrvNascSog
166200000000     C                   MoveL     DVFStato      $StatoSog
166300000000     C                   MoveL     DVFLocalit    $LocalitSog
166400000000     C                   MoveL     DVFIndriz     $IndirESog
166500000000     C*
166600000000     C*
166700000000     C* verifico se sono presenti i dati aggiuntivi anagrafici
166800000000     C* se esistono sovrascrivo i dati appena reperiti
166900000000     C                   Eval      SO3Sogg     = WSogg
167000000000     C                   Eval      SO3DtFiVl   = *Loval
167100000000     C                   Eval      SO3Sys      = 0
167200000000     C                   Eval      SO3NrAsInt  = 0
167300000000     C                   EXSR      RepSO3_SOG
167400000000     C*
167500000000     C                   ENDSR
167600000000     C/EJECT
167700000000     C********************************************************************
167800130903     C* Reperisco la tipologia della natura giuridica
167900000000     C********************************************************************
168000130903     C     RepTpN        BEGSR
168100000000     C*
168200130903     C                   Clear                   SDGTABDS
168300130903     C*
168400130903     C                   Eval      XTbRic      = '1'
168500130903     C                   Eval      XTbAzi      = XScSoc
168600130903     C                   Eval      XTbKey      = *Zero
168700130903     C                   Eval      XTbCod      = 'G16'
168800130903     C                   Move      $NatGiur      XTbKey
168900130903     C*
169000130903     C                   CALL      'SDGTABDR'
169100130903     C                   PARM                    SDGTABDS
169200130903     C*
169300130903     C                   if        XTbErr      = '1'
169400160419     C                   Eval      $AnnConto   = *On
169500130903     C                   if        $TpAnn      = *Blank
169600130903     C                   Eval      $TpAnn      = '50'
169700160418R221AX****               EXSR      StpErr1
169800130903     C                   endif
169900130903     C                   endif
170000130903     C*
170100130903     C                   ENDSR
170200130903     C/EJECT
170300130903     C********************************************************************
170400130903     C* Reperisco i dati anagrafici aggiuntivi
170500130903     C********************************************************************
170600130903     C     RepSO3_SOG    BEGSR
170700130903     C*
170800160523R224 X**** K04SO301      CHAIN     SDGSO301L                          21
170801160523R224 C                   EXSR      ChainSDGSO3
170802160523R224 C*
170900000000     C                   if        *In21       = *Off
171000000000     C                   MoveL(P)  SO3CdFisc     $CdFiscSog
171100000000     C                   MoveL(P)  SO3PartIva    $PartIVASog
171200000000     C                   MoveL(P)  SO3CogRag     $CogRagSog
171300000000     C                   MoveL(P)  SO3Nome       $NomeSog
171400000000     C                   Move      SO3DtNasc     $DtNascSog
171500000000     C                   MoveL(P)  SO3LocNasc    $LocNascSog
171600000000     C                   MoveL(P)  SO3PrvNasc    $PrvNascSog
171700000000     C                   MoveL(P)  SO3Stato      $StatoSog
171800000000     C                   MoveL(P)  SO3Localit    $LocalitSog
171900000000     C                   MoveL(P)  SO3IndirE     $IndirESog
172000000000     C                   endif
172100000000     C*
172200000000     C                   ENDSR
172300000000     C/EJECT
172301160523R224 C************************************************************
172302160523 "   C* Lettura dati aggiuntivi anagrafici
172303160523 "   C************************************************************
172304160523R224 C     ChainSDGSO3   BEGSR
172319160607     C*
172320160607     C                   Eval      SO3Dest     = $Dest
172321160523     C*
172322160523     C     K05SO301      CHAIN     SDGSO301L                          21
172323160523     C*
172324160523     C* se non trovato, provo con la destinazione Generica
172325160523     C                   if        *In21       = *On     and
172326160523     C                             SO3Dest    <> *Blank
172327160523     C                   Eval      SO3Dest     = *Blank
172328160523     C     K05SO301      CHAIN     SDGSO301L                          21
172329160523     C                   endif
172330160523     C*
172331160523R224 C                   ENDSR
172332160523R224 C/EJECT
172400000000     C************************************************************
172500000000     C* Dati del cliente
172600000000     C************************************************************
172700000000     C     RieDvaCli     BEGSR
172800000000     C*
172900000000     C                   Eval      %subst(DvaCli:1:DVALenout)
173000000000     C                              = %subst(DVAOutput:1:DVALenout)
173100000000     C*
173200130903     C                   Eval      $NatGiur    = DVCNatGiur
173300130903     C                   EXSR      RepTpN
173400130903     C                   if        XTbErr      = '0'
173500130903     C                   MoveL     XTbUni        ANGG16DS
173600130903     C                   Eval      $TpNSog     = TpNG16
173700130903     C                   endif
173800130903     C*
173900000000     C                   MoveL     DVCPartIVA    $PartIVASog
174000000000     C                   MoveL     DVCCdFisc     $CdFiscSog
174100000000     C                   MoveL     DVCDes        $CogRagSog
174200000000     C                   MoveL     *Blank        $NomeSog
174300000000     C                   Move      DVCDtNasc     $DtNascSog
174400000000     C                   MoveL     *Blank        $LocNascSog
174500000000     C                   MoveL     *Blank        $PrvNascSog
174600000000     C                   MoveL     DVCStato      $StatoSog
174700000000     C                   MoveL     DVCLocalit    $LocalitSog
174800000000     C                   MoveL     DVCIndriz     $IndirESog
174900000000     C*
175000000000     C*
175100000000     C* verifico se sono presenti i dati aggiuntivi anagrafici
175200000000     C* se esistono sovrascrivo i dati appena reperiti
175300000000     C                   Eval      SO3Sogg     = WSogg
175400000000     C                   Eval      SO3DtFiVl   = *Loval
175500000000     C                   Eval      SO3Sys      = 0
175600000000     C                   Eval      SO3NrAsInt  = 0
175700000000     C                   EXSR      RepSO3_SOG
175800000000     C*
175900000000     C                   ENDSR
176000000000     C/EJECT
176100000000     C************************************************************
176200000000     C* Selezioni sul conto
176300000000     C************************************************************
176400000000     C     SelConto      BEGSR
176500000000     C*
176600000000     C                   Select
176700000000     C*
176800000000     C* il soggetto non rientra nelle selezioni, scarto il conto
176900130820     C                   When      Sogg002    <> SoggG52  and
177000130820     C                             SoggG52    <> *Blank
177100130826     C                   Eval      $OkConto    = *Off
177200130826     C*
177300130826     C* la sottonatura non rientra nelle selezioni, scarto il conto
177400130826     C                   When      SNatura002 <> CFG52    and
177500130826     C                             CFG52      <> *Blank
177600130826     C                   Eval      $OkConto    = *Off
177700130826     C*
177800130826     C* la sottonatura non è cliente nè fornitore, scarto il conto
177900130826     C                   When      SNatura002 <> 'C'      and
178000130826     C                             SNatura002 <> 'F'
178100130826     C                   Eval      $OkConto    = *Off
178200130826     C*
178300130826     C                   endSl
178301160711R2314C*
178302160711 "   C* se Black List e se è Dogana, sostituisco il conto
178303160711 "   C* e il soggetto della bolla con quelli della fattura
178304160711 "   C* legata alla bolla, in questo modo vengono recuperati i
178305160711 "   C* dati fiscali e anagrafici del fornitore
178306160711 "   C                   if        TpComG52    = 'B'   and
178307160711 "   C                             $OkConto    = *On
178308160711 "   C                   EXSR      RepFor
178309160711 "   C                   endif
178310160711R2314C*
178400000000     C*
178500000000     C                   if        $OkConto    = *On
178600000000     C*
178700151201R195 C* Spesometro o Acquisti da San Marino
178800151201 "   C                   if        TpComG52    = 'E'   or
178900151201 "   C                             TpComG52    = 'S'
179000151201R195 C*
179100000000     C* conto escluso dagli elenchi
179200000000     C                   if        AlleIVA002  = '01'
179300000000     C                   Eval      $AnnConto   = *On
179400000000     C                   if        $TpAnn      = *Blank
179500000000     C                   Eval      $TpAnn      = '08'
179600160418R221AX****               EXSR      StpErr1
179700000000     C                   endif
179800000000     C                   endif
179900000000     C*
180000151201R195 C                   endif
180100151201 "   C*
180200151201 "   C* Black List
180300151201 "   C                   if        TpComG52    = 'B'
180400151201 "   C*
180500151201 "   C* conto escluso dagli elenchi Black List
180600151201 "   C                   if        BLPosA11    > 0                   and
180700151201 "   C                             SkAttr(BLPosA11)  =  BLEsclA11
180800151201 "   C                   Eval      $AnnConto   = *On
180900151201 "   C                   if        $TpAnn      = *Blank
181000160418R195 C                   Eval      $TpAnn      = '08'
181100160418R221AX*ex R195           EXSR      StpErr1
181200160418R195 C                   endif
181300151201 "   C                   endif
181400151201 "   C*
181500151201 "   C                   endif
181600151201R195 C*
181700000000     C                   endif
181800000000     C*
181900000000     C                   ENDSR
182000000000     C/EJECT
182001160711R2314C************************************************************
182002160711 "   C* Reperimento conto fornitore da legame bolla/fattura
182003160711 "   C************************************************************
182004160711R2314C     RepFor        BEGSR
182005160711     C*
182006160711     C* se Dogana, recupero il conto del fornitore se presente
182007160711     C                   if        AlleIVA002  = '02'    and
182008160711     C                             KccG54     <> *Blank  and
182009160711     C                             KscG54     <> *Blank
182010160711     C*
182011160711     C                   Eval      IVAKccCp    = KccG54
182012160711     C                   Eval      IVAKscCp    = KscG54
182013160711     C*
182014160711     C                   Eval      WKcc        = IVAKccCP
182015160711     C                   Eval      WKsc        = IVAKscCP
182016160711     C                   Eval      $Data       = *Loval
182017160711     C                   Clear                   DsAttributi
182018160711     C                   EXSR      CallMVC002
182019160711     C*
182020160711     C                   if        $Esito      = *On
182021160711     C                   Eval      SNaturaSav  = *Blank
182022160711     C* annullo il conto
182023160711     C                   Eval      $AnnConto   = *On
182024160711     C                   if        $TpAnn      = *Blank
182025160711     C                   Eval      $TpAnn      = '07'
182026160711     C                   endif
182027160711     C                   else
182028160711     C                   MoveL     Attributi     DsAttributi
182029160711     C                   Eval      SNaturaSav  = SNatura002
182030160711     C                   endif
182031160711     C                   endif
182032160711     C*
182033160711R2314C                   ENDSR
182034160711R2314C/EJECT
182100000000     C********************************************************************
182200000000     C* Controllo dati del movimento cliente/fornitore
182300000000     C********************************************************************
182400000000     C     ChkNDMOV      BEGSR
182500000000     C*
182600000000     C* esamino il movimento solo a cambio di registrazione
182700131206R141 X****               IF        IVASys     <> SAVSys   or
182800131206R141 X****                         IVANrAsReg <> SAVNrAsReg
182900131206R141 C*
183000131206 "   C                   IF        IVASys     <> SAV3Sys   or
183100131206R141 C                             IVANrAsReg <> SAV3NrAsReg
183101160711R2322C                             OR
183102160711 "   C                             (TpComG52    = 'B'     and
183103170822R2322C                              AlleIVA002  = '02'        )
183200000000     C*
183300000000     C                   COMMIT
183400130826     C                   FEOD      SDGSCF03L
183500000000     C*
183600131206R141 X****               Move      IVASys        SAVSys
183700131206R141 X****               Move      IVANrAsReg    SAVNrAsReg
183800131206R141 C                   Move      IVASys        SAV3Sys
183900131206R141 C                   Move      IVANrAsReg    SAV3NrAsReg
184000000000     C*
184100000000     C                   EXSR      InzNDMOV
184200000000     C*
184300000000     C                   EXSR      GetMOVCF
184400000000     C*
184500000000     C* salvo i dati del movimento cliente/fornitore
184600000000     C                   MoveL     NDMOV00F      NDMOVSAV
184700000000     C*
184800000000     C                   EXSR      SelNDMOV1
184900000000     C                   if        $OkMOV      = *On
185000160104R195 X****               EXSR      CtrNDMOV1
185100160129R195 C*
185200160129 "   C* se Black List, annullo se previsto da controlli da legame
185300160129 "   C* "bolla doganale/fattura"
185400160129 "   C* se FATTURA
185500160129 "   C* potrebbe essere da annullare perchè legata a bolla
185600160129 "   C* doganale registrata nel medesimo anno solare
185700160129 "   C*
185800160129 "   C* se BOLLA DOGANALE
185900160129 "   C* potrebbe essere da annullare perchè legata a fattura
186000160129 "   C* registrata in anni precedenti
186100160129 "   C* oppure perchè NON legata a fattura
186200160129 "   C*
186300160129 "   C* se FATTURA o se BOLLA DOGANALE potrebbe essere da
186400160129 "   C* annullare perchà non trovata la registrazione del legame
186500160129 "   C                   if        TpComG52    = 'B'     and
186600160129 "   C                             SNatura002  = 'F'     and
186700160129 "   C                             TpAnnG54   <> *Blank
186800160418 "   C                   Eval      $AnnMOV     = *On
186900160418R195 C                   Eval      $TpAnn      = TpAnnG54
187000160418R221AX*ex R195           EXSR      StpErr1
187100160129R195 C                   endif
187200000000     C*
187300000000     C* reperimento dati fiscali dipendenti dal
187400000000     C* movimento contabile
187500000000     C                   EXSR      RepDati_MOV
187600000000     C*
187700000000     C                   EXSR      SelNDMOV2
187800000000     C                   if        $OkMOV      = *On
187900000000     C                   EXSR      CtrNDMOV2
188000000000     C                   endif
188100160104R195 C*
188200160104 "   C                   if        $OkMOV      = *On
188300160104 "   C* controllo i dati del movimento
188400160104 "   C                   EXSR      CtrNDMOV1
188500160104R195 C                   endif
188600000000     C*
188700000000     C                   endif
188800000000     C*
188900000000     C                   endif
189000140306R133 C*
189100140306  "  C                   Eval      $WriteSCF   = *On
189200140306  "  C* se sto facendo la comunicazione degli Acquisti da
189300140306  "  C* San Marino, scarto tutti i movimenti che non sono
189400140306  "  C* di San Marino
189500140306  "  C                   if        TpComG52    = 'S'     and
189600140306  "  C                             $UICKsc    <> 037
189700140306  "  C                   Eval      $WriteSCF   = *Off
189800140306  "  C                   Eval      $AnnMOV     = *On
189900140306R133 C                   endif
190000000000     C*
190100000000     C                   ENDSR
190200000000     C/EJECT
190300000000     C************************************************************
190400000000     C* Inizializzo variabili appoggio per dati movimento
190500000000     C************************************************************
190600000000     C     InzNDMOV      BEGSR
190700000000     C*
190800000000     C                   Eval      $OkMOV      = *On
190900000000     C* se il conto è annullato , estraggo cmq i movimenti
191000000000     C* del periodo e li scrivo annullati
191100000000     C                   Eval      $AnnMOV     = $AnnConto
191200000000     C                   if        $AnnMOV    <> *On
191300000000     C                   Eval      $TpAnn      = *Blank
191400000000     C                   endif
191500000000     C*
191600131206R141 C                   Eval      $Autofatt   = *Off
191700150420R1786C                   Eval      $AutoCons   = *Off
191800130821     C                   Eval      $NatOpe     = *Blank
191900130826     C                   Eval      $ImpDoc     = 0
192000000000     C*
192100000000     C                   ENDSR
192200000000     C/EJECT
192300000000     C************************************************************
192400000000     c* Reperisce dati del movimento del cliente/fornitore
192500000000     C************************************************************
192600000000     C     GetMOVCF      BEGSR
192700000000     C*
192800000000     C                   Eval      $LenOut     = %size(NDMOV00F)
192900000000     C                   CALLB     'NDDRVMOV'
193000000000     C                   PARM                    IVASys
193100000000     C                   PARM                    IVANrAsReg
193200000000     C* cerco la riga del cliente/fornitore (è sempre la num.1)
193300000000     C* quella dell'IVA è sempre la 3
193400000000     C                   PARM      1             $NrRigaM
193500000000     C                   PARM      'NDMOV     '  $Struttura
193600000000     C                   PARM                    $LenOut
193700000000     C                   PARM      '1'           $CodOper
193800000000     C                   PARM                    NDMOV00F
193900000000     C*
194000000000     C                   ENDSR
194100000000     C/EJECT
194200000000     C********************************************************************
194300000000     C* Seleziono dati sul movimento del cliente/fornitore
194400000000     C********************************************************************
194500000000     C     SelNDMOV1     BEGSR
194600000000     C*
194700130826     C                   if        $CodOper   <> '9'
194800130826     C                   Eval      $ImpDoc     = MOVimporto
194900130826     C                   endif
195000130826     C*
195100000000     C                   Select
195200000000     C* movimento cliente/fornitore non trovato
195300000000     C                   When      $CodOper    = '9'
195400000000     C                   Eval      $AnnMOV     = *On
195500160125     C                   if        $TpAnn      = *Blank
195600000000     C                   Eval      $TpAnn      = '22'
195700160418R221AX****               EXSR      StpErr1
195800000000     C                   endif
195900000000     C*
196000130820     C                   When      MOVTpRegZ  <> TpRegzG52
196100000000     C                   Eval      $OkMOV      = *Off
196200000000     C*
196300130820     C                   When      MOVCtb     <> CtbG52
196400000000     C                   Eval      $OkMOV      = *Off
196500000000     C*
196600000000     C                   endSl
196700000000     C*
196800000000     C                   ENDSR
196900000000     C/EJECT
197000000000     C************************************************************
197100000000     C* Controllo i dati del movimento
197200000000     C************************************************************
197300000000     C     CtrNDMOV1     BEGSR
197400000000     C*
197500000000     C                   EXSR      CtrCausale
197600000000     C*
197700000000     C                   EXSR      CtrClasse
197800000000     C*
197900000000     C*
198000000000     C                   ENDSR
198100000000     C/EJECT
198200000000     C************************************************************
198300000000     C* Controllo  causale
198400000000     C************************************************************
198500000000     C     CtrCausale    BEGSR
198600000000     C*
198700000000     C                   Eval      $Causale    = MOVCausTes
198800000000     C                   EXSR      CallMVC003
198900000000     C                   if        $Esito      = *On
199000000000     C*
199100000000     C* se la causale non esiste, annullo il documento
199200000000     C                   Eval      $AnnMOV     = *On
199300000000     C                   if        $TpAnn      = *Blank
199400000000     C                   Eval      $TpAnn      = '04'
199500000000     C                   endif
199600000000     C*
199700000000     C                   else
199800000000     C*
199900000000     C                   Select
200000000000     C* la sottotipologia deve essere per Clienti o Fornitori
200100000000     C                   When      OPESTP     <> 'C'     and
200200000000     C                             OPESTP     <> 'F'
200300000000     C                   Eval      $AnnMOV     = *On
200400000000     C                   if        $TpAnn      = *Blank
200500000000     C                   Eval      $TpAnn      = '24'
200600000000     C                   endif
200700000000     C*
200800000000     C* la tipologia deve essere per registrazioni IVA
200900000000     C                   When      OPETPC     <> '1'
201000000000     C                   Eval      $AnnMOV     = *On
201100000000     C                   if        $TpAnn      = *Blank
201200000000     C                   Eval      $TpAnn      = '24'
201300000000     C                   endif
201400000000     C*
201500131206R141 X**** non deve essere causale di autofattura
201600131206 "   X**** N.B. ci si riferisce agli acquisti intracomunitari dove
201700131206 "   X**** la fattura del fornitore viene scartata perchè gestita
201800131206 "   X**** nel circuito INTRA, mentre la fattura di vendita
201900131206 "   X**** per stornare l'IVA (è quella che qui viene intercettata
202000131206 "   X**** come autofattura) viene scartata per non considerare 2
202100131206R141 X**** volte lo stesso importo IVA sul fornitore
202200131206R141 C*
202300131206 "   C* Da chiarimenti dell'Agenzia delle Entrate, come principio
202400131206 "   C* generale, vanno comunicate come operazioni Attive e con
202500131206 "   C* i riferimenti anagrafici della società comunicante (quadro BL
202600131206 "   C* o SE).
202700131206 "   C* Le autofatture CEE di Paesi INTRA, verranno annullate per
202800131206 "   C* INTRA, sarà poi a carico dell'utente riattivare le righe
202900131206 "   C* eventualmente non comunicate in INTRA.
203000131206 "   C* Rimangono le autofatture di Paesi EXTRACEE: se riferite a Beni
203100131206 "   C* verranno annullate automaticamente perchè passando da Dogana
203200131206 "   C* è presente la bolla doganle; se riferite a Servizi, a meno di
203300131206R141 C* altri criteri di annullamento, saranno da comunicare.
203400000000     C                   When      OPETpDocI   = '8'
203500131206R141 X****               Eval      $AnnMOV     = *On
203600131206 "   X****               if        $TpAnn      = *Blank
203700131206 "   X****               Eval      $TpAnn      = '01'
203800131206R141 X****               endif
203900131206R141 C*
204000131206 "   C* dico che è autofattura
204100131206R141 C                   Eval      $Autofatt   = *On
204200151222R195 C*
204300151222 "   C* se Black List, le autofatture sono da annullare
204400151222 "   C                   if        TpComG52    = 'B'
204401160329 "   C* anche se Acquisti da San marino
204402160329 "   C                             OR
204403160329 "   C                             TpComG52    = 'S'
204500151222 "   C                   Eval      $AnnMOV     = *On
204600151222 "   C                   if        $TpAnn      = *Blank
204700151222 "   C                   Eval      $TpAnn      = '01'
204800151222 "   C                   endif
204900151222 "   C                   endif
205000151222R195 C*
205100000000     C                   endSl
205200000000     C*
205300000000     C                   endif
205400000000     C*
205500000000     C* ---------------
205600000000     C* verifico se causale è da escludere dalla comunicazione
205700130823     C                   Eval      SPRSocieta  = XScSoc
205800151123R195 C* è Black List
205900151123 "   C                   if        TpComG52    = 'B'
206000151123 "   C                   Eval      SPRTipReg   = '5'
206100151123 "   C                   else
206200151123R195 C* se è Spesometro o Acquisti San Marino
206300130823     C                   Eval      SPRTipReg   = '3'
206400151123R195 C                   endif
206500130823     C                   Eval      SPRSTpReg   = '1'
206600130823     C                   Eval      SPRTPREG    = *Blank
206700130823     C                   Eval      SPRLIBRO    = *Blank
206800130823     C                   Eval      SPRALIQ     = *Zero
206900130823     C                   Eval      SPRRIFIVA   = *Blank
207000130823     C                   Eval      SPRCAUSALE  = MOVCausTes
207100130823     C                   Eval      SPRKCC      = *Blank
207200130823     C                   Eval      SPRKSC      = *Blank
207300130823     C                   Eval      SPRVoce     = *Blank
207400130823     C                   Eval      SPRClasse   = *Blank
207500130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
207600000000     C                   if        *In21       = *Off
207700000000     C* se trovo il record significa che la causale è esclusa
207800000000     C                   Eval      $AnnMOV     = *On
207900000000     C                   if        $TpAnn      = *Blank
208000000000     C                   Eval      $TpAnn      = '13'
208100160418R221AX****               EXSR      StpErr1
208200000000     C                   endif
208300000000     C                   endif
208400000000     C*
208500000000     C                   ENDSR
208600000000     C/EJECT
208700000000     C************************************************************
208800000000     C* Reperimento dati di causale operativa
208900000000     C************************************************************
209000000000     C     CallMVC003    BEGSR
209100000000     C*
209200000000     C                   Clear                   ANOPE
209300131031R140 C*
209400131031 "   C     MOVDtReg      AddDur    1:*D          WData
209500131031 "   C                   Move      WData         WDtAlfa
209600131031 "   C*
209700131031 "   C* la causale deve essere valida alla data di registrazione
209800131031 "   C* passo quindi data di registrazione + un giorno
209900131031 "   C
210000131031R140 C*
210100000000     C*
210200000000     C                   Eval      $LenOut     = %Size(ANOPE)
210300000000     C                   CallB     'NDMVC003'
210400000000     C                   Parm                    XScSoc
210500000000     C                   Parm                    $Causale
210600131031R140 X****               Parm                    UDateIso
210700131031R140 C                   Parm                    WDtAlfa
210800000000     C                   Parm      '0'           $Uso              1
210900000000     C                   Parm      *OFF          $GesMsg
211000000000     C                   Parm      *ON           $Esito
211100000000     C                   Parm      'ANOPE     '  $Struttura
211200000000     C                   Parm                    ANOPE
211300000000     C                   Parm                    $LenOut
211400000000     C*
211500000000     C                   ENDSR
211600000000     C/EJECT
211700000000     C************************************************************
211800000000     C* Controllo  classe
211900000000     C************************************************************
212000000000     C     CtrClasse     BEGSR
212100000000     C*
212200000000     C* verifico se classe è da escludere dalla comunicazione
212300130823     C                   Eval      SPRSocieta  = XScSoc
212400151123R195 C* è Black List
212500151123 "   C                   if        TpComG52    = 'B'
212600151123 "   C                   Eval      SPRTipReg   = '5'
212700151123 "   C                   else
212800151123R195 C* se è Spesometro o Acquisti San Marino
212900130823     C                   Eval      SPRTipReg   = '3'
213000151123R195 C                   endif
213100130823     C                   Eval      SPRSTpReg   = '4'
213200130823     C                   Eval      SPRTPREG    = *Blank
213300130823     C                   Eval      SPRLIBRO    = *Blank
213400130823     C                   Eval      SPRALIQ     = *Zero
213500130823     C                   Eval      SPRRIFIVA   = *Blank
213600130823     C                   Eval      SPRCAUSALE  = *Blank
213700130823     C                   Eval      SPRKCC      = *Blank
213800130823     C                   Eval      SPRKSC      = *Blank
213900130823     C                   Eval      SPRVoce     = *Blank
214000130823     C                   Eval      SPRClasse   = MOVClasse
214100130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
214200000000     C                   if        *In21       = *Off
214300000000     C* se trovo il record significa che la classe è esclusa
214400000000     C                   Eval      $AnnMOV     = *On
214500000000     C                   if        $TpAnn      = *Blank
214600000000     C                   Eval      $TpAnn      = '27'
214700000000     C                   endif
214800000000     C                   endif
214900000000     C*
215000000000     C                   ENDSR
215100000000      /EJECT
215200151204R195 X************************************************************
215300151204 "   X**** Controllo se registrazione già estratta e attiva
215400151204 "   X**** *******************************************************
215500151204 "   X**** CtrExistSCF   BEGSR
215600151204 "   X****
215700151204 "   X****               Eval      $ExistSCF   = *Off
215800151204 "   X****               Eval      $WriteSCF   = *On
215900151204 "   X****               Eval      $AnnullaSCF = *Off
216000151204 "   X****
216100151204 "   X****               Z-Add     IVASys        SCFSys
216200151204 "   X****               Z-Add     IVANrAsReg    SCFNrAsReg
216300151204 "   X**** K02SCF03      SETLL     SDGSCF03L                              21
216400151204 "   X**** K02SCF03      READE     SDGSCF03L                              21
216500151204 "   X****               Eval      $ExistSCF   = Not(*In21)
216600151204 "   X****               Eval      $FineSCF    = *In21
216700151204 "   X****               DoW       $FineSCF    = *Off
216800151204 "   X****
216900151204 "   X****               EXSR      GestExistSCF
217000151204 "   X****
217100151204 "   X**** K02SCF03      READE     SDGSCF03L                              21
217200151204 "   X****               Eval      $FineSCF    = *In21
217300151204 "   X****               endDo
217400151204 "   X****
217500151204 "   X****
217600151204 "   X**** se non trovato, provo con la chiave documento
217700151204 "   X****               if        $ExistSCF   = *Off
217800151204 "   X****
217900151204 "   X****               Eval      SCFSocieta  = MOVSocieta
218000151204 "   X****               Eval      SCFKCC      = MOVKCC
218100151204 "   X****               Eval      SCFKSC      = MOVKSC
218200151204 "   X****               Eval      SCFDTDOC    = MOVDTDOC
218300151204 "   X****               Eval      SCFNRDOC    = MOVNRDOC
218400151204 "   X****               Eval      SCFSERDOC   = MOVSERDOC
218500151204 "   X****               Eval      SCFDTREG    = SavDTREG
218600151204 "   X****               Eval      SCFNRREG    = SavNRREG
218700151204 "   X****               Eval      SCFSERREG   = SavSERREG
218800151204 "   X****               Eval      SCFDTPAR    = SavDTPAR
218900151204 "   X****               Eval      SCFNRPAR    = SavNRPAR
219000151204 "   X****               Eval      SCFSERPAR   = SavSERPAR
219100151204 "   X**** K12SCF04      SETLL     SDGSCF04L                              21
219200151204 "   X**** K12SCF04      READE     SDGSCF04L                              21
219300151204 "   X****               Eval      $ExistSCF   = Not(*In21)
219400151204 "   X****
219500151204 "   X****               Eval      $FineSCF    = *In21
219600151204 "   X****               DoW       $FineSCF    = *Off
219700151204 "   X****
219800151204 "   X****               EXSR      GestExistSCF
219900151204 "   X****
220000151204 "   X**** K12SCF04      READE     SDGSCF04L                              21
220100151204 "   X****               Eval      $FineSCF    = *In21
220200151204 "   X****               endDo
220300151204 "   X****
220400151204 "   X****               endif
220500151204 "   X****
220600151204 "   X****
220700151204 "   X**** trovato, lo annullo
220800151204 "   X****               if        $ExistSCF   = *On       and
220900151204 "   X****                         $AnnullaSCF = *On
221000151204 "   X****               Eval      $AnnMOV     = *On
221100151204 "   X****               Eval      $AnnIVA     = *On
221200151204 "   X****               if        $TpAnn      = *Blank
221300151204 "   X****               Eval      $TpAnn      = '16'
221400151204 "   X****               EXSR      StpErr1
221500151204 "   X****               endif
221600151204 "   X****               endif
221700151204 "   X****
221800151204 "   X****               ENDSR
221900151204R195 X****/EJECT
222000000000     C************************************************************
222100000000     C* Reperimento del totale imponibile IVA
222200000000     C************************************************************
222300000000     C     RepSUMImponib BEGSR
222400000000     C*
222500000000     C                   Eval      $CodOper    = '1'
222600000000     C                   EXSR      GetIVA
222700000000     C*
222800000000     C                   DoW       $CodOper   <> '9'
222900000000     C*
223000000000     C* incremento il totale dell'imponibile IVA
223100000000     C                   Eval      SUMImponib  = + ((IV2Dare - IV2Avere) *
223200000000     C                                               IV2Importo           )
223300000000     C*
223400000000     C                   Eval      $Codoper    = '2'
223500000000     C                   EXSR      GetIVA
223600000000     C                   endDO
223700000000     C*
223800000000     C                   if        SUMImponib      < 0
223900000000     C                   Eval      SUMImponib      = - SUMImponib
224000000000     C                   endif
224100000000     C*
224200000000     C                   ENDSR
224300000000      /EJECT
224400000000     C******************************************************
224500000000     c* Reperisce dati di IVA
224600000000     C******************************************************
224700000000     C     GetIVA        BEGSR
224800000000     C*
224900000000     C                   Eval      $LenOut     = %size(NDIVA00F)
225000000000     C                   CALLB     'NDDRVIVA'
225100000000     C                   PARM                    MOVSys
225200000000     C                   PARM                    MOVNrAsReg
225300000000     C                   PARM                    *Omit
225400000000     c                   PARM                    *Omit
225500000000     C                   PARM      *OFF          $Lock             1
225600000000     C                   PARM      'NDIVA     '  $Struttura
225700000000     C                   PARM                    $LenOut
225800000000     C                   PARM                    $CodOper
225900000000     C                   PARM                    NDIVA00F
226000000000     C*
226100000000     C                   ENDSR
226200000000     C/EJECT
226300000000     C************************************************************
226400000000     C* Reperimento dati fiscali dipendenti dal movimento
226500000000     C************************************************************
226600000000     C     RepDati_MOV   BEGSR
226700000000     C*
226800000000     C                   Select
226900000000     C                   When      Tecnico002 <> '2'     and
227000000000     C                             MOVDtDoc   <> SAVDtDoc
227100000000     C                   EXSR      RepANCPI
227200000000     C                   Move      MOVDtDoc      SAVDtDoc
227300000000     C*
227400000000     C                   When      Tecnico002  = '2'
227500000000     C                   EXSR      RepNDCPA
227600000000     C*
227700000000     C                   endSl
227800000000     C*
227900000000     C                   ENDSR
228000000000     C/EJECT
228100000000     C************************************************************
228200000000     C* Reperimento dati anagrafici da dati storicizzati
228300000000     C************************************************************
228400000000     C     RepANCPI      BEGSR
228500000000     C*
228600000000     C*  inizializzo i dati anagrafici con quelli del soggetto,
228700000000     C*  verranno sostituiti da quelli specifici se reperiti
228800130903     C                   MoveL(P)  $TpNSog       $TpN
228900000000     C                   MoveL(P)  $CdFiscSog    $CdFisc
229000000000     C                   MoveL(P)  $PartIVASog   $PartIVA
229100000000     C                   MoveL(P)  $CogRagSog    $CogRag
229200000000     C                   MoveL(P)  $NomeSog      $Nome
229300000000     C                   Clear                   $DtNasc
229400000000     C                   Move      $DtNascSog    $DtNasc
229500000000     C                   MoveL(P)  $LocNascSog   $LocNasc
229600000000     C                   MoveL(P)  $PrvNascSog   $PrvNasc
229700000000     C                   MoveL(P)  $StatoSog     $Stato
229800000000     C                   MoveL(P)  $LocalitSog   $Localit
229900000000     C                   MoveL(P)  $IndirESog    $IndirE
230000000000     C*
230100000000     C                   Eval      DVPLenOut = %size(ANCPI00F)
230200000000     C*
230300000000     C                   CALLB     'NDDVACPI'
230400000000     C                   PARM      '2'           DVPRic
230500000000     C                   PARM      MOVSocieta    DVPSocieta
230600000000     C                   PARM      *Blank        DVPUnita
230700000000     C                   PARM      'ANCPI     '  DVPStrutt
230800000000     C                   PARM      MOVDtDoc      DVPDtRif
230900000000     C                   PARM                    DVPLenOut
230901160711R2322X****               PARM      Sogg002       DVPSogg
230902160711R2322C                   PARM      WSogg         DVPSogg
231100000000     C                   PARM      *Off          DVPErrore
231200000000     C                   PARM                    DVPOutPut
231300000000     C*
231400000000     C                   if        DVPErrore   = *Off
231500000000     C                   Eval      %subst(ANCPI00F:1:DVPLenout)
231600000000     C                              = %subst(DVPOutput:1:DVPLenout)
231700000000     C*
231800000000     C* trovati dati storicizzati alla data documento, sovrascrivo
231900000000     C* quelli validi attualmente per il soggetto
232000130903     C                   Eval      $NatGiur    = CPINatGiur
232100130903     C                   EXSR      RepTpN
232200130903     C                   if        XTbErr      = '0'
232300130903     C                   MoveL     XTbUni        ANGG16DS
232400130903     C                   Eval      $TpN        = TpNG16
232500130903     C                   endif
232600130903     C*
232700000000     C                   MoveL(P)  CPICdFisc     $CdFisc
232800000000     C                   MoveL(P)  CPIPartIva    $PartIVA
232900000000     C                   MoveL(P)  CPIDesSog1    $CogRag
233000000000     C*
233100000000     C* se il soggetto è persona fisica i seguenti dati NON cambiano
233200000000     C* mai, percui se reperiti per il soggetto vanno comunque sempre
233300000000     C* bene
233400000000     C*******            MoveL     *Blank        $Nome
233500000000     C*******            Move      *Loval        $DtNasc
233600000000     C*******            MoveL     *Blank        $LocNasc
233700000000     C*******            MoveL     *Blank        $PrvNasc
233800000000     C*
233900000000     C                   MoveL     CPIStato      $Stato
234000000000     C                   MoveL(P)  CPILocalit    $Localit
234100000000     C                   MoveL(P)  CPIIndriz     $IndirE
234200000000     C*
234300000000     C*
234400000000     C* verifico se sono presenti i dati aggiuntivi anagrafici
234500000000     C* se esistono sovrascrivo i dati appena reperiti
234600000000     C                   Eval      SO3Sogg     = CPISogg
234700000000     C                   Eval      SO3DtFiVl   = CPIDtFiVL
234800000000     C                   Eval      SO3Sys      = 0
234900000000     C                   Eval      SO3NrAsInt  = 0
235000000000     C                   EXSR      RepSO3_2
235100000000     C*
235200000000     C                   endif
235300000000     C*
235400000000     C* se non ci sono dati storicizzati alla data, non è un problema
235500000000     C* significa solo che sono validi quelli impostati sul soggetto
235600000000     C*
235700000000     C                   ENDSR
235800000000     C/EJECT
235900000000     C********************************************************************
236000000000     C* Reperimento dati anagrafici per conti estemporanei
236100000000     C********************************************************************
236200000000     C     RepNDCPA      BEGSR
236300000000     C*
236400130903     C*  inizializzo la tipologia della natura giuridica da quella
236500130903     C*  del soggetto perchè non presente sui dati NDCPA00F
236600130930     C                   MoveL(P)  $TpNSog       $TpN
236700000000     C                   Clear                   $CdFisc
236800000000     C                   Clear                   $PartIVA
236900000000     C                   Clear                   $CogRag
237000000000     C                   Clear                   $Nome
237100000000     C                   Clear                   $DtNasc
237200000000     C                   Clear                   $LocNasc
237300000000     C                   Clear                   $PrvNasc
237400000000     C                   Clear                   $Stato
237500000000     C                   Clear                   $Localit
237600000000     C                   Clear                   $IndirE
237700000000     C*
237800000000     C                   Clear                   NDCPA00F
237900000000     C*
238000000000     C                   Eval      $LenOut     = %size(NDCPA00F)
238100000000     C                   CALL      'NDC066R2'
238200000000     C                   PARM      MOVSys        $Sys
238300000000     C                   PARM      0             $NrAsInt
238400000000     C                   PARM      MOVNrAsReg    $NrAsGen
238500000000     C                   PARM                    $LenOut
238600000000     C                   PARM                    NDCPA00F
238700000000     C                   PARM      '0'           $Esito
238800000000     C*
238900000000     C* trovati dati fiscali per conti estemporanei, sovrascrivo
239000000000     C* quelli validi attualmente per il soggetto
239100130903     C                   if        $Esito      = '0'
239200000000     C                   MoveL     CPACdFisc     $CdFisc
239300000000     C                   MoveL     CPAPartIva    $PartIVA
239400000000     C                   MoveL     CPADescr      $CogRag
239500000000     C                   MoveL     *Blank        $Nome
239600000000     C                   Move      *Loval        $DtNasc
239700000000     C                   MoveL     *Blank        $LocNasc
239800000000     C                   MoveL     *Blank        $PrvNasc
239900000000     C                   MoveL     CPAStato      $Stato
240000000000     C                   MoveL(P)  CPALocalit    $Localit
240100000000     C                   MoveL(P)  CPAIndiriz    $IndirE
240200000000     C*
240300000000     C*
240400000000     C* verifico se sono presenti i dati aggiuntivi anagrafici
240500000000     C* se esistono sovrascrivo i dati appena reperiti
240600000000     C                   Eval      SO3Sogg     = *Blank
240700000000     C                   Eval      SO3DtFiVl   = *Loval
240800000000     C                   Eval      SO3Sys      = CPASys
240900000000     C                   Eval      SO3NrAsInt  = CPANrAsInt
241000000000     C                   EXSR      RepSO3_2
241100000000     C*
241200000000     C                   else
241300000000     C*
241400000000     C                   if        $AnnMOV     = *Off
241500000000     C                   Eval      $AnnMOV     = *On
241600000000     C                   if        $TpAnn      = *Blank
241700000000     C                   Eval      $TpAnn      = '07'
241800160418R221AX****               EXSR      StpErr1
241900000000     C                   endif
242000000000     C                   endif
242100000000     C*
242200000000     C                   endif
242300000000     C*
242400000000     C                   ENDSR
242500000000     C/EJECT
242600000000     C********************************************************************
242700000000     C* Reperisco i dati anagrafici aggiuntivi
242800000000     C********************************************************************
242900000000     C     RepSO3_2      BEGSR
243000000000     C*
243100160523R224 X**** K04SO301      CHAIN     SDGSO301L                          21
243101160523R224 C                   EXSR      ChainSDGSO3
243102160523R224 C*
243200000000     C                   if        *In21       = *Off
243300000000     C                   MoveL(P)  SO3CdFisc     $CdFisc
243400000000     C                   MoveL(P)  SO3PartIva    $PartIVA
243500000000     C                   MoveL(P)  SO3CogRag     $CogRag
243600000000     C                   MoveL(P)  SO3Nome       $Nome
243700000000     C                   Move      SO3DtNasc     $DtNasc
243800000000     C                   MoveL(P)  SO3LocNasc    $LocNasc
243900000000     C                   MoveL(P)  SO3PrvNasc    $PrvNasc
244000000000     C                   MoveL(P)  SO3Stato      $Stato
244100000000     C                   MoveL(P)  SO3Localit    $Localit
244200000000     C                   MoveL(P)  SO3IndirE     $IndirE
244300000000     C                   endif
244400000000     C*
244500000000     C                   ENDSR
244600000000     C/EJECT
244700000000     C********************************************************************
244800000000     C* Seleziono dati sul movimento del cliente/fornitore
244900000000     C********************************************************************
245000000000     C     SelNDMOV2     BEGSR
245100000000     C*
245200000000     C                   Select
245300151123R195 C*
245400151123 "   C                   When      $Stato     <> StatoG52   and
245500151123 "   C                             StatoG52   <> *Blank
245600151123R195 C                   Eval      $OkMOV      = *Off
245700000000     C*
245800130820     C                   When      $PartIVA   <> PartIVAG52 and
245900130826     C                             PartIVAG52 <> *Blank
246000130826     C                   Eval      $OkMOV      = *Off
246100130826     C*
246200130826     C                   When      $CdFisc    <> CdFiscG52  and
246300130826     C                             CdFiscG52  <> *Blank
246400130826     C                   Eval      $OkMOV      = *Off
246500130826     C*
246600130826     C                   endSl
246700130826     C*
246800130826     C                   ENDSR
246900130826     C/EJECT
247000130826     C************************************************************
247100130826     C* Controllo i dati del movimento
247200130826     C************************************************************
247300130826     C     CtrNDMOV2     BEGSR
247400140306R133 C*
247500140306  "  C* Comunicazione Acquisti da San Marino
247600140306  "  C                   if        TpComG52    = 'S'
247700140306  "  C                   EXSR      RepCodUIC
247800140306  "  C                   endif
247900140306  "  C*
248000140306  "  C* se si tratta di Repubblica si San Marino
248100140306  "  C* non controllo se lo Stato è in Black list, nè se è in INTRA
248200140306  "  C                   if        $UICKsc     = 037     and
248300140306  "  C                             TpComG52    = 'S'
248400140306  "  C*
248500140306R133 C                   else
248600130826     C*
248700130826     C* cerco lo Stato nella tabella degli Stati della Black List
248800151130R195 C* Se Spesometro,
248900130826     C* se lo trovo annullo il movimento
249000151130R195 C* Se Black List, annullo il movimento se non lo trovo
249100130826     C                   if        MOVDtReg   >= %date('2010-07-01')
249200151130R195 C                             or
249300151130R195 C                             TpComG52    = 'B'
249400130826     C                   EXSR      ChkStatoBL
249500130826     C                   endif
249600130826     C*
249700000000     C*
249800151130R195 C* controllo se è INTRA solo se Spesomentro
249900151130R195 C                   if        TpComG52    = 'E'
250000000000     C* cerco lo Stato nella tabella degli Stati
250100000000     C* se è una Nazione CEE/INTRA annullo il movimento
250200000000     C                   EXSR      ChkStatoINTRA
250300151130R195 C                   endif
250400151130R195 C*
250500140306R133 C                   endif
250600000000     C*
250700000000     C                   EXSR      RepNDREG
250800000000     C*
250900000000     C                   EXSR      RepNDPAS
251000130826     C*
251100130826     C                   EXSR      ChkContrP3
251200130826     C*
251300000000     C                   ENDSR
251400000000     C/EJECT
251500140306R133 C********************************************************************
251600140306  "  C* Reperisco il codice UIC dello Stato del cliente/fornitore
251700140306  "  C********************************************************************
251800140306R133 C     RepCodUIC     BEGSR
251900140306     C*
252000140306     C                   Clear                   SDGTABDS
252100140306     C*
252200140306     C                   Eval      XTbRic      = '1'
252300140306     C                   Eval      XTbAzi      = XScSoc
252400140306     C                   Eval      XTbKey      = *Zero
252500140306     C                   Eval      XTbCod      = 'G12'
252600140306     C                   Move      $Stato        XTbKey
252700140306     C*
252800140306     C                   CALL      'SDGTABDR'
252900140306     C                   PARM                    SDGTABDS
253000140306     C*
253100140306     C                   if        XTbErr      = '0'
253200140306     C                   MoveL     XTbUni        ANGG12DS
253300140306     C*
253400140306     C                   if        %SubSt(ANGG12DS:50:3)   = *Blank
253500140306     C                   Z-Add     0             $UICKsc
253600140306     C*
253700140306     C                   Eval      $AnnMOV     = *On
253800140306     C                   if        $TpAnn      = *Blank
253900140306     C* "Codice UIC Stato non impostato    "
254000140306     C                   Eval      $TpAnn      = '42'
254100160418R221AX*ex R133           EXSR      StpErr1
254200140306     C                   endif
254300140306     C*
254400140306     C                   else
254500140306     C*
254600140306     C                   Z-Add     CodUICG12     $UICKsc
254700140306     C                   endif
254800140306     C*
254900140306     C                   endif
255000140306     C*
255100140306     C                   if        XTbErr      = '1'
255200140306     C                   Eval      $AnnMOV     = *On
255300140306     C                   if        $TpAnn      = *Blank
255400140306     C                   Eval      $TpAnn      = '51'
255500160418R221AX*ex R133           EXSR      StpErr1
255600140306     C                   endif
255700140306     C                   endif
255800140306     C*
255900140306R133 C                   ENDSR
256000140306R133 C/EJECT
256100000000     C********************************************************************
256200000000     C* Controllo se lo Stato è tra quelli della Black List
256300000000     C********************************************************************
256400000000     C     ChkStatoBL    BEGSR
256500000000     C*
256600000000     C                   Clear                   SDGTABDS
256700000000     C*
256800000000     C                   Eval      XTbRic      = '1'
256900000000     C                   Eval      XTbAzi      = XScSoc
257000000000     C                   Eval      XTbKey      = *Zero
257100000000     C                   Eval      XTbCod      = 'HD1'
257200000000     C                   Move      $Stato        XTbKey
257300000000     C*
257400000000     C                   CALL      'SDGTABDR'
257500000000     C                   PARM                    SDGTABDS
257600000000     C*
257700151130R195 C* se Spesometro, annullo la riga perchè Paese è Black List
257800151130 "   C* e quindi movimento già comunicato in Black List
257900151130 "   C                   if        TpComG52    = 'E'
258000151130R195 C*
258100000000     C                   if        XTbErr      = '0'
258200000000     C                   Eval      $AnnMOV     = *On
258201170822R257 X***                if        $TpAnn      = *Blank
258400000000     C                   Eval      $TpAnn      = '23'
258500160418R221AX****               EXSR      StpErr1
258501170822R257 X***                endif
258700000000     C                   endif
258800151130R195 C*
258900151130 "   C                   endif
259000151130 "   C*
259100151130 "   C*
259200151130 "   C* se Black List, annullo il movimento se il Paese
259300151130 "   C* non è codificato nella tabella degli stati Black list
259400151130 "   C                   if        TpComG52    = 'B'
259500151130 "   C*
259600151130 "   C                   if        XTbErr     <> '0'
259601170822R195 C                   Eval      $AnnMOV     = *On
259602170822R257 X**  ex R195        if        $TpAnn      = *Blank
259900160418R195 C                   Eval      $TpAnn      = '33'
260000160418R221AX*ex R195           EXSR      StpErr1
260001170822R257 X*** ex R195        endif
260100160418R195 C                   endif
260300151130 "   C*
260400151130R195 C                   endif
260500000000     C*
260600000000     C                   ENDSR
260700000000     C/EJECT
260800000000     C********************************************************************
260900000000     C* Controllo se lo Stato è tra quelli dell'INTRA
261000000000     C********************************************************************
261100000000     C     ChkStatoINTRA BEGSR
261200000000     C*
261300000000     C                   Clear                   SDGTABDS
261400000000     C*
261500000000     C                   Eval      XTbRic      = '1'
261600000000     C                   Eval      XTbAzi      = XScSoc
261700000000     C                   Eval      XTbKey      = *Zero
261800000000     C                   Eval      XTbCod      = 'G12'
261900000000     C                   Move      $Stato        XTbKey
262000000000     C*
262100000000     C                   CALL      'SDGTABDR'
262200000000     C                   PARM                    SDGTABDS
262300000000     C*
262400000000     C                   if        XTbErr      = '0'
262500000000     C                   MoveL     XTbUni        ANGG12DS
262600130918     C*
262700130918     C                   if        %SubSt(ANGG12DS:50:3)   = *Blank
262800130918     C                   Z-Add     0             $UICKsc
262900130918     C*
263000130918     C                   Eval      $AnnMOV     = *On
263100130918     C                   if        $TpAnn      = *Blank
263200130918     C* "Codice UIC Stato non impostato    "
263300130918     C                   Eval      $TpAnn      = '42'
263400160418R221AX****               EXSR      StpErr1
263500130918     C                   endif
263600130918     C*
263700130918     C                   else
263800130918     C*
263900130918     C                   Z-Add     CodUICG12     $UICKsc
264000130918     C                   endif
264100000000     C*
264200000000     C                   if        NazG12     <> *Blank     and
264300130826     C                             NazG12     <> $NazSoc
264400131108R140 C*
264500131108 "   C* se Croazia, annullo solo se data registrazione successiva
264600131108 "   C* al 30/06/2013
264700131108 "   C                   Move      '2013-06-30'  WData
264800131108 "   C                   if        ($UICKsc     = 261        and
264900131108 "   C                              IVADtReg    > WData          ) or
265000131108 "   C                             $UICKsc    <> 261
265100131108R140 C*
265200000000     C                   Eval      $AnnMOV     = *On
265300000000     C                   if        $TpAnn      = *Blank
265400000000     C                   Eval      $TpAnn      = '25'
265500160418R221AX****               EXSR      StpErr1
265600000000     C                   endif
265700131108R140 C*
265800131108 "   C                   endif
265900131108R140 C*
266000000000     C                   endif
266100000000     C*
266200000000     C                   endif
266300130918     C*
266400130918     C                   if        XTbErr      = '1'
266500130918     C                   Eval      $AnnMOV     = *On
266600130918     C                   if        $TpAnn      = *Blank
266700130918     C                   Eval      $TpAnn      = '51'
266800160418R221AX****               EXSR      StpErr1
266900130918     C                   endif
267000130918     C                   endif
267100000000     C*
267200000000     C                   ENDSR
267300000000     C/EJECT
267400000000     C************************************************************
267500000000     C* Reperimento dei dati di registrazione
267600000000     C************************************************************
267700000000     C     RepNDREG      BEGSR
267800000000     C*
267900000000     C                   Clear                   SavDTREG
268000000000     C                   Clear                   SavNRREG
268100000000     C                   Clear                   SavSERREG
268200000000     C*
268300000000     C                   Clear                   NDREG00F
268400000000     C*
268500000000     C                   EVAL      $LenOut     = %Size(NDREG00F)
268600000000     C*
268700000000     C                   CALLB     'NDDRVREG'
268800000000     C                   PARM                    MOVSys
268900000000     C                   PARM                    MOVNrAsReg
269000000000     C                   PARM      'NDREG     '  $Struttura
269100000000     C                   PARM                    $LenOut
269200000000     C                   PARM      *Blank        $CodOper
269300000000     C                   PARM                    $Output
269400000000     C                   PARM                    NDREG00F
269500000000     C*
269600000000     C                   if        $CodOper   <> '9'
269700000000     C                   EVAL      %subst(NDREG00F:1:$LenOut)
269800000000     C                             = %subst($Output:1:$LenOut)
269900000000     C*
270000000000     C                   Eval      SavDTREG    = REGDtReg
270100000000     C                   Eval      SavNRREG    = REGNrReg
270200000000     C                   Eval      SavSERREG   = REGSerReg
270300000000     C                   else
270400000000     C                   Eval      $TpAnn      = '30'
270500160418R221AX****               EXSR      StpErr1
270600000000     C                   endif
270700000000     C*
270800000000     C                   ENDSR
270900000000     C/EJECT
271000000000     C************************************************************
271100000000     C* Reperimento dei dati di partita
271200000000     C************************************************************
271300000000     C     RepNDPAS      BEGSR
271400000000     C*
271500000000     C                   Clear                   SavDTPAR
271600000000     C                   Clear                   SavNRPAR
271700000000     C                   Clear                   SavSERPAR
271800000000     C*
271900000000     C                   Eval      PASSys      = MOVSys
272000000000     C                   Eval      PASNrAsReg  = MOVNrAsReg
272100000000     C                   Eval      PASNrRigaM  = MOVNrRigaM
272200000000     C     K03PAS01      SETLL     NDPAS01L
272300000000     C     K03PAS01      READE     NDPAS01L                               21
272400000000     C                   if        *In21       = *Off
272500000000     C                   Eval      SavDTPAR    = PASDtPar
272600000000     C                   Eval      SavNRPAR    = PASNrPar
272700000000     C                   Eval      SavSERPAR   = PASSerPar
272800000000     C                   else
272900000000     C                   Eval      $TpAnn      = '31'
273000160418R221AX****               EXSR      StpErr1
273100000000     C                   endif
273200000000     C*
273300000000     C                   ENDSR
273400130826     C/EJECT
273500130826     C************************************************************
273600130826     C* Analisi della contropartita (costo/ricavo) per verificare
273700130826     C* se escludere o meno la contropartita e quindi il documento
273800130826     C************************************************************
273900130826     C     ChkContrP3    BEGSR
274000130826     C*
274100111026     C                   Eval      $KscEscl    = *Off
274200111026     C                   Eval      $KscIncl    = *Off
274300111026     C*
274400111026     C* reperisco le informazioni sulle righe di contropartita
274500111026     C                   EXSR      RepContrp3
274600111026     C*
274700111026     C* ripristino il movimento cliente/fornitore per la stampa errore
274800111026     C                   Move      NDMOVSav      NDMOV00F
274900111026     C*
275000111026     C* gestisco solo i casi fattibili, ovvero quelli in cui
275100111026     C* sono presenti solo conti inclusi
275200111026     C* Se ci sono conti esclusi e conti inclusi, annullo
275300111026     C* comunque il documento per indicare una gestione
275400111026     C* manuale
275500111026     C*
275600111026     C                   Select
275700111026     C*
275800111026     C*--------------
275900111026     C* se presenti solo conti inclusi, non faccio nulla
276000111026     C* oppure non esistono contropartite (es. registrazione
276100111026     C* di sola IVA per passaggio da aliquota 20 al 21%)
276200111026     C                   When      ($KscIncl        = *On  and
276300111026     C                              $KscEscl       <> *On     ) or
276400111026     C                             ($KscIncl       <> *On  and
276500111026     C                              $KscEscl       <> *On     )
276600111026     C*
276700111026     C*--------------
276800111026     C* se presenti conti inclusi e conti esclusi, annullo
276900111026     C                   When      $KscIncl        = *On  and
277000111026     C                             $KscEscl        = *On
277100111026     C*
277200111026     C* annullo la riga in elaborazione  e la registrazione
277300111026     C* in modo da annullare anche tutte le altre
277400111026     C                   Eval      $AnnIVA     = *On
277500111026     C                   Eval      $AnnMOV     = *On
277600111026     C                   if        $TpAnn      = *Blank
277700111026     C                   Eval      $TpAnn      = '48'
277800160418R221AX****               EXSR      StpErr1
277900111026     C                   endif
278000111026     C*
278100111026     C*--------------
278200111026     C* se presenti solo conti esclusi, annullo
278300111026     C                   When      $KscIncl       <> *On  and
278400111026     C                             $KscEscl        = *On
278500111026     C*
278600111026     C* annullo la riga in elaborazione  e la registrazione
278700111026     C* in modo da annullare anche tutte le altre
278800111026     C                   Eval      $AnnIVA     = *On
278900111026     C                   Eval      $AnnMOV     = *On
279000111026     C                   if        $TpAnn      = *Blank
279100111026     C                   Eval      $TpAnn      = '47'
279200160418R221AX****               EXSR      StpErr1
279300111026     C                   endif
279400111026     C*
279500111026     C                   endSl
279600130826     C*
279700130826     C                   ENDSR
279800130826     C/EJECT
279900130826     C************************************************************
280000130826     C* Reperimento tipologia di contropartita
280100130826     C************************************************************
280200130826     C     RepContrp3    BEGSR
280300130826     C*
280400111026     C* Lettura dei movimenti di contropartita legati alla registrazione
280500111026     C                   Eval      $CodOper    = '1'
280600111026     C                   EXSR      GetMOVCP
280700111026     C*
280800111026     C                   DoW       $CodOper   <> '9'
280900111026     C*
281000111026     C                   EXSR      SelContrP3
281001160321R2193C*
281002160321 "   C                   if        $OkContrP   = *On
281003160321 "   C* verifico se conto di controparita è da escludere nella determinazione
281004160321 "   C* della tipologia di operazione ( sconti, spese, commissioni)
281005160321 "   C*
281006160321 "   C* recupero l'eventuale voce di analitica per verificare
281007160321 "   C* se il movimento deve essere escluso tramite la voce
281008160321 "   C                   Eval      $Voce       = *Blank
281009160321 "   C*
281010160321 "   C* Scorro le righe di analitica della prima dimensione
281011160321 "   C* in ordine alfabetico. E' sufficiente scorrere le righe di
281012160321 "   C* una sola dimensione, in quanto se presenti anche più voci
281013160321 "   C* legate allo stesso conto, la definizione di beni/servizi
281014160321 "   C* o l'esclusione sarebbe logicamente la stessa in quanto
281015160321 "   C* le voci sono legate allo stesso conto
281016160321 "   C                   Eval      MOASys     = MOVSys
281017160321 "   C                   Eval      MOANrAsReg = MOVNrAsReg
281018160321 "   C                   Eval      MOANrRigaM = MOVNrRigaM
281019160321 "   C     K03MOA01      Setll     NDMOA01L
281020160321 "   C     K03MOA01      READE     NDMOA01L                               21
281021160321 "   C                   Eval      $FineMOA   = *In21
281025160321 "   C*
281026160321 "   C                   Eval      SPRSocieta  = XScSoc
281027160321 "   C* se è Black List
281028160321 "   C                   if        TpComG52    = 'B'
281029160321 "   C                   Eval      SPRTipReg   = '5'
281030160321 "   C                   else
281031160321 "   C* se è Spesometro o Acquisti San Marino
281032160321 "   C                   Eval      SPRTipReg   = '3'
281033160321 "   C                   endif
281034160321 "   C                   Eval      SPRSTpReg   = '2'
281035160321 "   C                   Eval      SPRTPREG    = *Blank
281036160321 "   C                   Eval      SPRLIBRO    = *Blank
281037160321 "   C                   Eval      SPRALIQ     = 0
281038160321 "   C                   Eval      SPRRIFIVA   = *Blank
281039160321 "   C                   Eval      SPRCAUSALE  = *Blank
281040160321 "   C                   Eval      SPRClasse   = *Blank
281041160321 "   C*
281042160321 "   C* la prima volta entra comunque, anche se non ha trovato
281043160321 "   C* il record di NDMOA, esce dal ciclo quando ha finito di
281044160321 "   C* leggere tutte le righe della prima dimensione
281045160321 "   C                   DoU       $FineMOA   = *On
281046160321 "   C*
281047160321 "   C                   if        $OkContrP   = *On
281048160321 "   C* 1° tentativo con KCC + KSC + VOCE
281049160321 "   C                   Eval      SPRKCC      = MOVKcc
281050160321 "   C                   Eval      SPRKSC      = MOVKsc
281051160321 "   C                   Eval      SPRVoce     = MOAVoce
281052160321 "   C     K12SPR04      CHAIN     SDGSPR04L                          21
281053160321 "   C                   if        *In21       = *Off
281054160321 "   C* se trovo il record significa che il conto è escluso
281055160321 "   C                   Eval      $KscEscl    = *On
281056160321 "   C                   Eval      $OkContrP   = *Off
281057160321 "   C                   Eval      $FineMOA   = *On
281058160321 "   C                   endif
281059160321 "   C*
281060160321 "   C                   endif
281061160321 "   C*
281062160321R2193C* ---------------
281100111026     C*
281200111026     C                   if        $OkContrP   = *On
281300111026     C* ---------------
281400160321R2193X**** 1° tentativo con KCC + KSC
281401160321R2193C* 2° tentativo con KCC + KSC
281500111026     C* verifico se conto di controparita è da escludere
281600130823     C                   Eval      SPRSocieta  = XScSoc
281700151123R195 C* è Black List
281800151123 "   C                   if        TpComG52    = 'B'
281900151123 "   C                   Eval      SPRTipReg   = '5'
282000151123 "   C                   else
282100151123R195 C* se è Spesometro o Acquisti San Marino
282200130823     C                   Eval      SPRTipReg   = '3'
282300151123R195 C                   endif
282400130823     C                   Eval      SPRSTpReg   = '2'
282500130823     C                   Eval      SPRTPREG    = *Blank
282600130823     C                   Eval      SPRLIBRO    = *Blank
282700130823     C                   Eval      SPRALIQ     = 0
282800130823     C                   Eval      SPRRIFIVA   = *Blank
282900130823     C                   Eval      SPRCAUSALE  = *Blank
283000130823     C                   Eval      SPRKCC      = MOVKcc
283100130823     C                   Eval      SPRKSC      = MOVKsc
283200130823     C                   Eval      SPRVoce     = *Blank
283300130823     C                   Eval      SPRClasse   = *Blank
283400130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
283500111026     C                   if        *In21       = *Off
283600111026     C* se trovo il record significa che il conto è escluso
283700111026     C                   Eval      $KscEscl    = *On
283701160321R2193C                   Eval      $OkContrP   = *Off
283702160321R2193C                   Eval      $FineMOA   = *On
283703160321R2193C                   endif
283704160321R2193C*
283705160321R2193C                   endif
283800111026     C*
283900111026     C* ---------------
283901160321R2193C*
283904160321 "   C                   if        $OkContrP   = *On
283905160321 "   C* 3° tentativo con KCC + VOCE
283906160321 "   C* verifico se conto di controparita è da escludere nella determinazione
283907160321 "   C* della tipologia di operazione ( sconti, spese, commissioni)
283908160321 "   C                   Eval      SPRKCC      = MOVKcc
283909160321 "   C                   Eval      SPRKSC      = *Blank
283910160321 "   C                   Eval      SPRVoce     = MOAVoce
283911160321 "   C     K12SPR04      CHAIN     SDGSPR04L                          21
283912160321 "   C                   if        *In21       = *Off
283913160321 "   C* se trovo il record significa che il conto è escluso
283914160321 "   C                   Eval      $KscEscl    = *On
283915160321 "   C                   Eval      $OkContrP   = *Off
283916160321 "   C                   Eval      $FineMOA   = *On
283917160321 "   C                   endif
283918160321 "   C*
283919160321 "   C                   endif
283920160321 "   C*
283921160321R2193C* ---------------
284000160321R2193X****               else
284001160321R2193X**** 2° tentativo solo con KCC
284002160321R2193C*
284003160321 "   C* 4° tentativo solo con KCC
284004160321R2193C                   if        $OkContrP   = *On
284200111026     C* verifico se conto di controparita è da escludere nella determinazione
284300111026     C* della tipologia di operazione ( sconti, spese, commissioni)
284400130823     C                   Eval      SPRSocieta  = XScSoc
284500151123R195 C* è Black List
284600151123 "   C                   if        TpComG52    = 'B'
284700151123 "   C                   Eval      SPRTipReg   = '5'
284800151123 "   C                   else
284900151123R195 C* se è Spesometro o Acquisti San Marino
285000130823     C                   Eval      SPRTipReg   = '3'
285100151123R195 C                   endif
285200130823     C                   Eval      SPRSTpReg   = '2'
285300130823     C                   Eval      SPRTPREG    = *Blank
285400130823     C                   Eval      SPRLIBRO    = *Blank
285500130823     C                   Eval      SPRALIQ     = 0
285600130823     C                   Eval      SPRRIFIVA   = *Blank
285700130823     C                   Eval      SPRCAUSALE  = *Blank
285800130823     C                   Eval      SPRKCC      = MOVKcc
285900130823     C                   Eval      SPRKSC      = *Blank
286000130823     C                   Eval      SPRVoce     = *Blank
286100130823     C                   Eval      SPRClasse   = *Blank
286200130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
286300111026     C                   if        *In21       = *Off
286400111026     C* se trovo il record significa che il conto è escluso
286500111026     C                   Eval      $KscEscl    = *On
286501160321R2193C                   Eval      $OkContrP   = *Off
286502160321 "   C                   Eval      $FineMOA   = *On
286503160321 "   C                   endif
286504160321 "   C*
286505160321 "   C                   endif
286506160321 "   C*
286507160321 "   C* 5° tentativo solo con VOCE
286508160321 "   C                   if        $OkContrP   = *On
286509160321 "   C* verifico se conto di controparita è da escludere nella determinazione
286510160321 "   C* della tipologia di operazione ( sconti, spese, commissioni)
286511160321 "   C                   Eval      SPRSocieta  = XScSoc
286512160321 "   C* è Black List
286513160321 "   C                   if        TpComG52    = 'B'
286514160321 "   C                   Eval      SPRTipReg   = '5'
286515160321 "   C                   else
286516160321 "   C* se è Spesometro o Acquisti San Marino
286517160321 "   C                   Eval      SPRTipReg   = '3'
286518160321 "   C                   endif
286519160321 "   C                   Eval      SPRSTpReg   = '2'
286520160321 "   C                   Eval      SPRTPREG    = *Blank
286521160321 "   C                   Eval      SPRLIBRO    = *Blank
286522160321 "   C                   Eval      SPRALIQ     = 0
286523160321 "   C                   Eval      SPRRIFIVA   = *Blank
286524160321 "   C                   Eval      SPRCAUSALE  = *Blank
286525160321 "   C                   Eval      SPRKCC      = *Blank
286526160321 "   C                   Eval      SPRKSC      = *Blank
286527160321 "   C                   Eval      SPRVoce     = MOAVoce
286528160321 "   C                   Eval      SPRClasse   = *Blank
286529160321 "   C     K12SPR04      CHAIN     SDGSPR04L                          21
286530160321 "   C                   if        *In21       = *Off
286531160321 "   C* se trovo il record significa che il conto è escluso
286532160321 "   C                   Eval      $KscEscl    = *On
286533160321 "   C                   Eval      $OkContrP   = *Off
286534160321 "   C                   Eval      $FineMOA   = *On
286535160321 "   C                   endif
286536160321 "   C*
286537160321 "   C                   endif
286538160321 "   C*
286539160321 "   C                   if        $FineMOA    = *Off
286540160321 "   C* leggo solo i movimenti della prima dimensione su cui
286541160321 "   C* mi sono posizionata (vd. VL K04MOA01)
286542160321 "   C     K04MOA01      READE     NDMOA01L                               21
286543160321 "   C                   Eval      $FineMOA   = *In21
286544160321 "   C                   endif
286545160321 "   C*
286546160321 "   C                   EndDo
286547160321R2193C
286600160321R2193X****               else
286601160321R2193C*
286602160321 "   C*
286603160321R2193C                   if        $OkContrP   = *On
286700111026     C* se NON trovo il record significa che il conto è incluso
286800111026     C                   Eval      $KscIncl    = *On
286900111026     C                   endif
287000111026     C*
287100160321R2193X****               endif
287200160321R2193X****
287300160321     C                   endif
287400111026     C*
287500111026     C                   Eval      $CodOper    = '2'
287600111026     C                   EXSR      GetMOVCP
287700130826     C                   ENDDO
287800130826     C*
287900130826     C                   ENDSR
288000130826      /EJECT
288100130826     C****************************************************************
288200130826     C* Selezione dei movimenti di controparita
288300130826     C****************************************************************
288400130826     C     SelContrP3    BEGSR
288500130826     C*
288600111026     C                   Eval      $OkContrP   = *On
288700111026     C*
288800111026     C                   Select
288900111026     C* seleziono solo i cespiti ammortizzabili e gli altri conti
289000111026     C* (costi e ricavi)
289100111026     C                   When      Not(MOVSNatura  = '4'  or
289200111026     C                                 MOVSNatura  = '9'    )
289300111026     C                   Eval      $OkContrP   = *Off
289400111026     C*
289500150420R1785X****               When      MOVImporto  = 0
289600150420R1785X****               Eval      $OkContrP   = *Off
289700111026     C                   endSl
289800111026     C*
289900130826     C                   ENDSR
290000130826      /EJECT
290100130826     C******************************************************
290200000000     C* Controllo movimento IVA
290300000000     C******************************************************
290400000000     C     ChkNDIVA      BEGSR
290500000000     C*
290600000000     C                   EXSR      InzNDIVA
290700110826     C*
290800000000     C                   EXSR      ChkLibro
290900000000     C*
291000000000     C* codice riferimento IVA (ANRIV00F)
291100000000     C                   EXSR      ChkRifIVA
291200000000     C*
291300000000     C*----------------------
291400160129R195 X**** contropartita su riga diversa dal conto
291500160129 "   X**** o non impostata
291600160129 "   X****               if        IVAKccCp   <> MOVKcc     or
291700160129 "   X****                         IVAKscCp   <> MOVKsc
291800160129 "   X****
291900160129 "   X****               Eval      $AnnIVA     = *On
292000160129 "   X****               if        $TpAnn      = *Blank
292100160129 "   X****               Eval      $TpAnn      = '14'
292200160129 "   X****               EXSR      StpErr1
292300160129 "   X****               endif
292400160129R195 X****               endif
292500000000     C*
292600000000     C*----------------------
292700000000     C* Associazione libri-riferimenti IVA (AnALR00f)
292800000000     C                   EXSR      ChkAssIVA
292900000000     C*
293000000000     C                   ENDSR
293100000000     C/EJECT
293200000000     C******************************************************
293300000000     c* Inizializzo le variabili per righe IVA del movimento
293400000000     C******************************************************
293500000000     C     InzNDIVA      BEGSR
293600000000     C*
293700000000     C* se il movimento è annullato , estraggo cmq le righe
293800000000     C* IVA del periodo e le scrivo annullate
293900000000     C                   Eval      $AnnIVA     = $AnnMOV
294000000000     C                   if        $AnnIVA    <> *On
294100000000     C                   Eval      $TpAnn      = *Blank
294200000000     C                   endif
294300000000     C*
294400000000     C                   ENDSR
294500000000     C/EJECT
294600000000     C******************************************************
294700000000     C* Controllo libro IVA
294800000000     C******************************************************
294900000000     C     ChkLibro      BEGSR
295000000000     C*
295100000000     C                   MoveL     IVASocieta    DLISocieta
295200000000     C                   MoveL     IVATpReg      DLITpReg
295300000000     C                   MoveL     IVALibro      DLILibro
295400000000     C     K03DLI01      CHAIN     ANDLI01L                           21
295500000000     C*
295600000000     C* libro IVA non trovato, annullo la riga movimento
295700000000     C                   IF        *In21       = *On
295800000000     C*
295900131025R138 C* se la registrazione ha più righe IVA con problemi per
296000131025 "   C* attribuire la natura, considero predominante l'annullamento
296100131025R138 C* per controlli specifici sulla riga IVA
296200000000     C                   Eval      $AnnIVA     = *On
296300000000     C                   if        $TpAnn      = *Blank
296400131025R138 C                             or
296500131025R138 C                             $TpAnn      = '20'
296600000000     C                   Eval      $TpAnn      = '10'
296700160418R221AX****               EXSR      StpErr1
296800000000     C                   endif
296900000000     C*
297000000000     C                   ENDIF
297100000000     C*
297200000000     C*----------------------------------
297300000000     C*
297400000000     C* verifico se il libro IVA è da escludere
297500130823     C                   Eval      SPRSocieta  = XScSoc
297600151123R195 C* è Black List
297700151123 "   C                   if        TpComG52    = 'B'
297800151123 "   C                   Eval      SPRTipReg   = '5'
297900151123 "   C                   else
298000151123R195 C* se è Spesometro o Acquisti San Marino
298100130823     C                   Eval      SPRTipReg   = '3'
298200151123R195 C                   endif
298300130823     C                   Eval      SPRSTpReg   = '3'
298400130823     C                   Eval      SPRTPREG    = DLITpReg
298500130823     C                   Eval      SPRLIBRO    = DLILibro
298600130823     C                   Eval      SPRALIQ     = 0
298700130823     C                   Eval      SPRRIFIVA   = *Blank
298800130823     C                   Eval      SPRCAUSALE  = *Blank
298900130823     C                   Eval      SPRKCC      = *Blank
299000130823     C                   Eval      SPRKSC      = *Blank
299100130823     C                   Eval      SPRVoce     = *Blank
299200130823     C                   Eval      SPRClasse   = *Blank
299300130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
299400000000     C                   if        *In21       = *Off
299500000000     C* se trovo il record significa che il libro è escluso
299600131025R138 C* se la registrazione ha più righe IVA con problemi per
299700131025 "   C* attribuire la natura, considero predominante l'annullamento
299800131025R138 C* per controlli specifici sulla riga IVA
299900000000     C                   Eval      $AnnIVA     = *On
300000000000     C                   if        $TpAnn      = *Blank
300100131025R138 C                             or
300200131025R138 C                             $TpAnn      = '20'
300300000000     C                   Eval      $TpAnn      = '26'
300400160418R221AX****               EXSR      StpErr1
300500000000     C                   endif
300600000000     C                   endif
300700000000     C*
300800000000     C*----------------------------------
300900000000     C*
301000000000     C                   EXSR      ChkAutof
301100000000     C*
301200000000     C*----------------------------------
301300000000     C*
301400000000     C*  Libro per IVA sospesa/differita già liquidata
301500000000     C*
301600000000     C* 3 = IVA norm. da sos. (f.31/12/97)
301700000000     C* 5 = Sola liquidazione
301800000000     C                   if        DLISospens  = '3'   or
301900000000     C                             DLISospens  = '5'
302000131025R138 C* se la registrazione ha più righe IVA con problemi per
302100131025 "   C* attribuire la natura, considero predominante l'annullamento
302200131025R138 C* per controlli specifici sulla riga IVA
302300000000     C                   Eval      $AnnIVA     = *On
302400000000     C                   if        $TpAnn      = *Blank
302500131025R138 C                             or
302600131025R138 C                             $TpAnn      = '20'
302700000000     C                   Eval      $TpAnn      = '03'
302800160418R221AX****               EXSR      StpErr1
302900000000     C                   endif
303000000000     C                   endif
303100000000     C*
303200000000     C                   ENDSR
303300000000     C/EJECT
303400000000     C********************************************************************
303500000000     C* Controllo se registrazione di autofattura
303600000000     C********************************************************************
303700000000     C     ChkAutof      BEGSR
303800000000     C*
303900000000     C* Se fornitore + registro vendite + territorialità libro '1' (CEE)
304000000000     C* significa che sto trattando un'autofattura (che devo ESCLUDERE !!!)
304100000000     C* Con questo test escludo anche le autofatture gestite senza
304200000000     C* automatismi di primanota
304300000000     C* N.B. se la causale ha tipo registro 2=Vendite, il fornitore in DARE
304400000000     C*      NON è ammesso, a meno che il tipo documento sia 8=Autofatture
304500000000     C*      In questo caso, la causale è automaticamente esclusa dalla
304600000000     C*      comunicazione. Quindi, questo test secondo me NON serve mai
304700000000     C*
304800000000     C                   if        MOVSNatura  = 'F'   and
304900000000     C                             IVATpReg    = '2'   and
305000000000     C                             DLITerrito  = '1'
305100000000     C*
305200131206R141 X*ex R138 se la registrazione ha più righe IVA con problemi per
305300131206 "   X*ex R138 attribuire la natura, considero predominante l'annullamento
305400131206 "   X*ex R138 per controlli specifici sulla riga IVA
305500131206 "   X****               Eval      $AnnIVA     = *On
305600131206 "   X****               if        $TpAnn      = *Blank
305700131206 "   X*ex R138                     or
305800131206 "   X*ex R138                     $TpAnn      = '20'
305900131206 "   X****               Eval      $TpAnn      = '01'
306000131206 "   X****               EXSR      StpErr1
306100131206R141 X****               endif
306200151201R195 C*
306300151201 "   C* se Black List, le autofatture sono da annullare
306400151201 "   C                   if        TpComG52    = 'B'
306500151201 "   C* se la registrazione ha più righe IVA con problemi per
306600151201 "   C* attribuire la natura, considero predominante l'annullamento
306700151201 "   C* per controlli specifici sulla riga IVA
306800151201 "   C                   Eval      $AnnIVA     = *On
306900151201 "   C                   if        $TpAnn      = *Blank
307000151201 "   C                             or
307100151201 "   C                             $TpAnn      = '20'
307200160418R195 C                   Eval      $TpAnn      = '01'
307300160418R221AX*ex R195           EXSR      StpErr1
307400160418R195 C                   endif
307500151201R195 C                   endif
307600131206R141 C*
307700131206 "   C* dico che è autofattura, non annullo più
307800131206 "   C                   Eval      $Autofatt   = *On
307900131206R141 C                   endif
308000150420R1786C*
308100150420 "   C* ---------------
308200150420 "   C* AUTOFATTURA
308300150420 "   C* verifico se si tratta di fattura di vendita per OMAGGI,
308400150420 "   C* AUTOCONSUMO (può essere anche fattura di reverse charge)
308500150420 "   C* In questo caso il cliente è sempre la società che emette
308600150420 "   C* la fattura
308700150420 "   C                   if        IVATpReg    = '2'
308800150420 "   C                   EXSR      ChkAutoCons
308900150420 "   C                   endif
309000150420R1786C*
309100000000     C*
309200131206R141 C                   if        $Autofatt   = *On
309300131206R141 X**** STAMPA AUTOFATTURA esclusa
309400131206R141 C* STAMPA AUTOFATTURA
309401160314R1951C                   if        TpComG52   <> 'B'
309500000000     C                   EXSR      StpAutof
309501160314R1951C                   endif
309600000000     C                   endif
309700000000     C*
309800000000     C                   ENDSR
309900000000     C/EJECT
310000150420R1786C************************************************************
310100150420 "   C* Verifico se si tratta di autofattura per autoconsumo
310200150420 "   C************************************************************
310300150420R1786C     ChkAutocons   BEGSR
310400150420     C*
310500150420     C* reperisco la natura giuridica del contribuente
310600150420     C                   Eval      $NatGiur    = XScNCR
310700150420     C                   EXSR      RepTpN
310800150420     C                   if        XTbErr      = '0'
310900150420     C                   MoveL     XTbUni        ANGG16DS
311000150420     C*
311100150420     C                   Select
311200150420     C*-
311300150420     C*-----------------------
311400150420     C* PERSONA FISICA
311500150420     C                   When      TpNG16      = '1'
311600150420     C*
311700150420     C                   Eval      SO3Sogg     = SocSogg
311800150420     C                   Move      *Loval        SO3DtFiVl
311900150420     C                   Eval      SO3Sys      = 0
312000150420     C                   Eval      SO3NrAsInt  = 0
312100150420     C*
312200160523R224 X**** K04SO301      CHAIN     SDGSO301L                          21
312201160523R224 C                   EXSR      ChainSDGSO3
312202160523R224 C*
312300150420     C                   if        *In21       = *Off
312400150420     C*
312500150420     C                   if        SO3CdFisc   = $CdFisc
312600150420     C***                MoveL(P)  $CodUIC       $CodUICAlfa
312700150420     C***                if        SO3CdFisc   = $CdFisc      and
312800150420     C***                          SO3PartIva  = $PartIVA     and
312900150420     C***                          SO3CogRag   = $CogRag      and
313000150420     C***                          SO3Nome     = $Nome        and
313100150420     C***                          SO3DtNasc   = $DtNasc      and
313200150420     C***                          SO3LocNasc  = $LocNasc     and
313300150420     C***                          SO3PrvNasc  = $PrvNasc     and
313400150420     C***                          SO3Stato    = $Stato       and
313500150420     C***                          SO3CodUIC   = $CodUICAlfa  and
313600150420     C***                          SO3Localit  = $Localit     and
313700150420     C***                          SO3IndirE   = $IndirE
313800150420     C                   Eval      $Autocons   = *On
313900150420     C                   Eval      $Autofatt   = *On
314000150420     C*
314100150420     C                   endif
314200150420     C*
314300150420     C                   endif
314400150420     C*
314500150420     C*-----------------------
314600150420     C* PERSONA GIURIDICA
314700150420     C                   When      TpNG16      = '2'  or
314800150420     C                             TpNG16      = '3'
314900150420     C*
315000150420     C                   if        XScIVA      = $PartIVA
315100150420     C***                if        XScRgS      = $CogRag      and
315200150420     C***                          XScCdF      = $CdFisc      and
315300150420     C***                          XScIVA      = $PartIVA     and
315400150420     C***                          *Blank      = $Nome        and
315500150420     C***                          *Loval      = $DtNasc      and
315600150420     C***                          *Blank      = $LocNasc     and
315700150420     C***                          *Blank      = $PrvNasc     and
315800150420     C***                          XScSta      = $Stato       and
315900150420     C***                          $UICSoc     = $CodUIC      and
316000150420     C***                          XScCit      = $Localit     and
316100150420     C***                          XScVia      = $IndirE
316200150420     C                   Eval      $Autocons   = *On
316300150420     C                   Eval      $Autofatt   = *On
316400150420     C*
316500150420     C                   endif
316600150420     C*
316700150420     C                   EndSl
316800150420     C*
316900150420     C                   endif
317000150420     C*
317100150420R1786C                   ENDSR
317200150420R1786C/EJECT
317300000000     C******************************************************
317400000000     C* Controllo riferimento IVA
317500000000     C******************************************************
317600000000     C     ChkRifIVA     BEGSR
317700000000     C*
317800000000     C                   Move      IVASocieta    RIVSocieta
317900000000     C                   Move      IVATpReg      RIVTpReg
318000000000     C                   Move      IVAAliq       RIVAliq
318100000000     C                   Move      IVARifIVA     RIVRifIVA
318200000000     C     K04RIV01      CHAIN     ANRIV01L                           21
318300000000     C                   if        *In21       = *On
318400131025R138 C* se la registrazione ha più righe IVA con problemi per
318500131025 "   C* attribuire la natura, considero predominante l'annullamento
318600131025R138 C* per controlli specifici sulla riga IVA
318700000000     C                   Eval      $AnnIVA     = *On
318800000000     C                   if        $TpAnn      = *Blank
318900131025R138 C                             or
319000131025R138 C                             $TpAnn      = '20'
319100000000     C                   Eval      $TpAnn      = '09'
319200160418R221AX****               EXSR      StpErr1
319300000000     C                   endif
319400000000     C                   endif
319500000000     C*
319600000000     C                   ENDSR
319700000000     C/EJECT
319800000000     C******************************************************
319900000000     C* Controllo associazione libri IVA
320000000000     C******************************************************
320100000000     C     ChkAssIVA     BEGSR
320200000000     C*
320300000000     C                   Move      IVASocieta    ALRSocieta
320400000000     C                   Move      IVATpReg      ALRTpReg
320500000000     C                   Move      IVALibro      ALRLibro
320600000000     C                   z-add     IVAAliq       ALRAliq
320700000000     C                   Move      IVARifIVA     ALRRifIVA
320800000000     C     K05ALR01      CHAIN     ANALR01L                           21
320900000000     C                   EXSR      SelANALR
321000000000     C*
321100000000     C                   ENDSR
321200000000     C/EJECT
321300000000     C************************************************************
321400000000     C* Selezione del movimento in base alle regole su associazione
321500000000     C************************************************************
321600000000     C     SelANALR      BEGSR
321700000000     C*
321800000000     C                   Select
321900000000     C*-----------
322000000000     C* associazione inesistente
322100000000     C                   WHEN      *In21       = *On
322200131025R138 C* se la registrazione ha più righe IVA con problemi per
322300131025 "   C* attribuire la natura, considero predominante l'annullamento
322400131025R138 C* per controlli specifici sulla riga IVA
322500000000     C                   Eval      $AnnIVA     = *On
322600131025     C                   if        $TpAnn      = *Blank
322700131025R138 C                             or
322800131025R138 C                             $TpAnn      = '20'
322900000000     C                   Eval      $TpAnn      = '11'
323000160418R221AX****               EXSR      StpErr1
323100000000     C                   endif
323200000000     C*
323300000000     C*-----------
323400000000     C* associazione non associata
323500000000     C                   WHEN      ALRAnn      = '1'
323600131025R138 C* se la registrazione ha più righe IVA con problemi per
323700131025 "   C* attribuire la natura, considero predominante l'annullamento
323800131025R138 C* per controlli specifici sulla riga IVA
323900000000     C                   Eval      $AnnIVA     = *On
324000000000     C                   if        $TpAnn      = *Blank
324100131025R138 C                             or
324200131025R138 C                             $TpAnn      = '20'
324300000000     C                   Eval      $TpAnn      = '12'
324400160418R221AX****               EXSR      StpErr1
324500000000     C                   endif
324600000000     C*
324700000000     C*-----------
324800000000     C* movimento da escludere dalla comunicazione
324900000000     C                   When      ALRElenchi = '01'
325000151222R195 C                             and
325100151222 "   C* solo per Spesometro e Acquisti da San Marino
325200151222 "   C                             (TpComG52  = 'E'   or
325300151222R195 C                              TpComG52  = 'S'     )
325400000000     C*
325500131025R138 C* se la registrazione ha più righe IVA con problemi per
325600131025 "   C* attribuire la natura, considero predominante l'annullamento
325700131025R138 C* per controlli specifici sulla riga IVA
325800000000     C                   Eval      $AnnIVA    = *On
325900000000     C                   if        $TpAnn      = *Blank
326000131025R138 C                             or
326100131025R138 C                             $TpAnn      = '20'
326200000000     C                   Eval      $TpAnn      = '02'
326300160418R221AX****               EXSR      StpErr1
326400000000     C                   endif
326500000000     C*
326600000000     C                   endSl
326700151222R195 C*
326800151222 "   C*
326900151222 "   C                   if        $AnnIVA    <> *On
327000151222 "   C*
327100151222 "   C* solo per Black List
327200151222 "   C                   if        TpComG52  = 'B'
327300151222 "   C*
327400151222 "   C* movimento da escludere dalla comunicazione
327500151222 "   C     K05ALR01      CHAIN     SDGBLA01L                          21
327600151222 "   C                   if        *In21       = *Off
327700151222 "   C* se trovo il record significa che l'associazione è esclusa
327800151222 "   C                   Eval      $AnnIVA    = *On
327900151222 "   C                   if        $TpAnn      = *Blank
328000151222 "   C                             or
328100151222 "   C                             $TpAnn      = '20'
328200160418R195 C                   Eval      $TpAnn      = '02'
328300160418R221AX*ex R195           EXSR      StpErr1
328400160418R195 C                   endif
328500151222 "   C*
328600151222 "   C                   endif
328700151222 "   C*
328800151222 "   C                   endif
328900151222 "   C*
329000151222R195 C                   endif
329100000000     C*
329200000000     C                   ENDSR
329300000000     C/EJECT
329400000000     C************************************************************
329500130820     C* Scrivo SDGSCF00F a dettaglio di NDIVA00F
329600000000     C************************************************************
329700130820     C     GesSCF_IVA    BEGSR
329800000000     C*
329900151123R195 C* Black list
330000151123 "   C                   if        TpComG52    = 'B'
330100151123 "   C                   EXSR      ChkDatiFiscBL
330200151123 "   C                   else
330300151123R195 C* Spesometro e San Marino
330400000000     C                   EXSR      ChkDatiFisc
330500151123R195 C                   endif
330600000000     C*
330700130820     C                   EXSR      RieSDGSCF
330709160418R2216C                   if        $WriteSCF   = *On
330710160418R221AC*
330711160418 "   C                   if        SCFTPANN   <> *Blank
330712160418 "   C                   Eval      $TpAnn      = SCFTpAnn
330713160418 "   C                   EXSR      StpErr1
330714160418 "   C                   endif
330715160418R221AC*
330800130820     C                   WRITE     SDGSCF000
330801160405R2216C                   endif
330900000000     C*
331000000000     C                   ENDSR
331100000000     C/EJECT
331200151123R195 C********************************************************************
331300151123 "   C* Controllo se presenti i dati fiscali obbligatori Black List
331400151123 "   C********************************************************************
331500151123R195 C     ChkDatiFiscBL BEGSR
331600151123     C*
331601160314R1951C* solo se lo Stato è in Black List
331602160314R1951C                   if        $TpAnn     <> '33'
331603160418R2214C                             or   $Stato    = *Blank
331700160303     C                   if        $Stato      = *Blank        or
331800160303     C                             ($TpN      = '1'        and
331900160303     C                              ($Nome    = *Blank  or
332000160303     C                               $DtNasc  = *Loval  or
332100160405R2217X****                           $LocNasc = *Blank  or
332200160405R2217X****                           $PrvNasc <> 'EE'     )   )or
332201160405R2217C                               $LocNasc = *Blank    )   )or
332300160303     C                             ($TpN     <> '1'        and
332400160303     C                              ($Localit = *Blank  or
332500160303     C                               $IndirE  = *Blank    )   )
332600151123     C*
332700151123     C* se la registrazione ha più righe IVA con problemi per
332800151123     C* attribuire la natura, considero predominante l'annullamento
332900151123     C* per controlli specifici sulla riga IVA
333000151123     C                   Eval      $AnnIVA     = *On
333100151123     C                   if        $TpAnn      = *Blank
333200151123     C                             or
333300151123     C                             $TpAnn      = '20'
333400151123     C* Codici fisc. o Stato  mancanti
333500151123     C                   Eval      $TpAnn      = '18'
333600160418R221AX****               EXSR      StpErr1
333700151123     C                   endif
333800151123     C*
333900151123     C*
334000151123     C                   Select
334100151123     C*
334200151123     C* conto normale: stampo solo a cambio di data documento
334300151123     C                   When      Tecnico002 <> '2'          and
334400151123     C                             (MOVDtDoc   <> SAVDtDoc2    or
334500151123     C                              MOVKcc     <> SAVKcc2      or
334600151123     C                              MOVKsc     <> SAVKsc2          )
334700151123     C*
334800151123     C                   Move      MOVDtDoc      SAVDtDoc2
334900151123     C                   MoveL     MOVKcc        SAVKcc2
335000151123     C                   MoveL     MOVKsc        SAVKsc2
335100151123     C                   EXSR      StpNoFisc
335200151123     C*
335300151123     C* conto tecnico
335400151123     C                   When      Tecnico002 = '2'
335500151123     C                   EXSR      StpNoFisc
335600151123     C*
335700151123     C                   endsl
335800151123     C*
335900151123     C                   endif
335901160314R1951C*
335902160314R1951C                   endif
336000151123     C*
336100151123R195 C                   ENDSR
336200151123R195 C/EJECT
336300000000     C********************************************************************
336400000000     C* Controllo se presenti i dati fiscali obbligatori
336500000000     C********************************************************************
336600000000     C     ChkDatiFisc   BEGSR
336700000000     C*
336800000000     C* nel caso in cui lo Stato sia Italia, partita Iva o
336900000000     C* codice fiscale devono essere valorizzati
337000130918     C                   if        $PartIVA    = *Blank   and
337100130918     C                             $CdFisc     = *Blank   and
337200151123R195 C                             $UICSoc     = 086      and
337300130918     C                             $UICKsc     = $UICSoc
337400000000     C*
337500131025R138 C* se la registrazione ha più righe IVA con problemi per
337600131025 "   C* attribuire la natura, considero predominante l'annullamento
337700131025R138 C* per controlli specifici sulla riga IVA
337800000000     C                   Eval      $AnnIVA     = *On
337900000000     C                   if        $TpAnn      = *Blank
338000131025R138 C                             or
338100131025R138 C                             $TpAnn      = '20'
338200151123R195 C* Partita IVA/Cd.Fisc. mancante
338300000000     C                   Eval      $TpAnn      = '17'
338400160418R221AX****               EXSR      StpErr1
338500000000     C                   endif
338600000000     C*
338700000000     C*
338800000000     C                   Select
338900000000     C*
339000000000     C* conto normale: stampo solo a cambio di data documento
339100000000     C                   When      Tecnico002 <> '2'          and
339200000000     C                             (MOVDtDoc   <> SAVDtDoc2    or
339300000000     C                              MOVKcc     <> SAVKcc2      or
339400000000     C                              MOVKsc     <> SAVKsc2          )
339500000000     C*
339600000000     C                   Move      MOVDtDoc      SAVDtDoc2
339700000000     C                   MoveL     MOVKcc        SAVKcc2
339800000000     C                   MoveL     MOVKsc        SAVKsc2
339900000000     C                   EXSR      StpNoFisc
340000000000     C*
340100000000     C* conto tecnico
340200000000     C                   When      Tecnico002 = '2'
340300000000     C                   EXSR      StpNoFisc
340400000000     C*
340500000000     C                   endsl
340600000000     C*
340700000000     C                   endif
340800000000     C*
340900000000     C                   ENDSR
341000000000     C/EJECT
341100000000     C************************************************************
341200130820     C* Riempio campi descrittivi di SDGSCF00F
341300000000     C************************************************************
341400130820     C     RieSDGSCF     BEGSR
341500000000     C*
341600130820     C                   Clear                   SDGSCF000
341700000000     C*
341800000000     C                   Eval      WKcc        = MOVKcc
341900000000     C                   Eval      WKsc        = MOVKsc
342000000000     C                   Eval      $Data       = MOVDtDoc
342100000000     C                   EXSR      CallMVC002
342200130820     C                   Eval      SCFUTEIMM   = XScCUt
342300130820     C                   Eval      SCFDTIMM    = UDateIso
342400130820     C                   Eval      SCFUTEULMO  = XScCUt
342500130820     C                   Eval      SCFDTULMO   = UDateIso
342600000000     C*
342700130821     C* Automatica
342800130821     C                   Eval      SCFORIGINE  = 'A'
342900130820     C                   Eval      SCFSOCIETA  = XScSoc
343000130821     C* Elenchi clienti/fornitori
343100140306R133 X****               Eval      SCFTpCom    = 'E'
343200140306R133 C                   Eval      SCFTpCom    = TpComG52
343300130820     C                   Eval      SCFAnno     = AnnoG52
343400140328R133 C                   Select
343500151123R195 C* Acquisti da San Marino
343600140328R133 C                   When      TpComG52    = 'S'
343700140328R133 C                   Eval      SCFMese     = MeseG52
343800140328R133 C                   Other
343900130822     C                   Eval      SCFMese     = 0
344000140328R133 C                   EndSl
344100130820     C                   Eval      SCFCF       = MOVSNatura
344200130820     C                   Eval      SCFSOGG     = Sogg002
344300130903     C*
344400131206R141 C* in caso di Autofattura, giro l'operazione sull'azienda
344500131206 "   C* comunicante impostando i suoi dati anagrafici
344600150420R141 C                   if        $Autofatt   = *On
344700150420R1786C***                          and
344800150420R1786C***                          $AutoCons  <> *On
344900150420R141 C                   EXSR      RieAnaSoc
345000131206 "   C*
345100131206 "   C                   else
345200131206R141 C*
345300130903     C                   Eval      SCFTpN      = $TpN
345400000000     C*
345500130820     C                   Eval      SCFPARTIVA  = $PartIVA
345600130820     C                   Eval      SCFCDFISC   = $CdFisc
345700000000     C*
345800000000     C                   Eval      $CampoScan  = $CogRag
345900000000     C                   EXSR      TolgoApice
346000000000     C                   Eval      $CogRag     = $CampoScan
346100130820     C                   Eval      SCFCOGRAG   = $CogRag
346200000000     C*
346300000000     C                   Eval      $CampoScan  = $Nome
346400000000     C                   EXSR      TolgoApice
346500000000     C                   Eval      $Nome       = $CampoScan
346600130820     C                   Eval      SCFNOME     = $Nome
346700000000     C*
346800130820     C                   Eval      SCFDTNASC   = $DtNasc
346900000000     C*
347000000000     C                   Eval      $CampoScan  = $LocNasc
347100000000     C                   EXSR      TolgoApice
347200000000     C                   Eval      $LocNasc    = $CampoScan
347300130820     C                   Eval      SCFLOCNASC  = $LocNasc
347400000000     C*
347500130820     C                   Eval      SCFPRVNASC  = $PrvNasc
347600000000     C*
347700130820     C                   Eval      SCFSTATO    = $Stato
347800130918     C*
347900130918     C                   Eval      SCFCodUIC   = $UICKsc
348000160104R195 C*
348100160104 "   C* siamo nella situazione di provincia estera forzata per
348200160104 "   C* soggetti esteri che sono persona fisica
348300160104 "   C                   if        SCFPrvNasc  = *Blank   and
348400160104 "   C                             SCFTpN      = '1'      and
348500160104 "   C                             SCFCodUIC  <> 0        and
348600160104 "   C                             SCFCodUIC  <> $UICSoc
348700160104 "   C                   Eval      SCFPrvNasc  = 'EE'
348800160104R195 C                   endif
348900000000     C*
349000000000     C                   Eval      $CampoScan  = $Localit
349100000000     C                   EXSR      TolgoApice
349200000000     C                   Eval      $Localit    = $CampoScan
349300130820     C                   Eval      SCFLOCALIT  = $Localit
349400000000     C*
349500000000     C                   Eval      $CampoScan  = $IndirE
349600000000     C                   EXSR      TolgoApice
349700000000     C                   Eval      $IndirE     = $CampoScan
349800130820     C                   Eval      SCFINDIRE   = $IndirE
349900131206R141 C                   endif
350000000000     C*
350100130820     C                   Eval      SCFKCC      = MOVKcc
350200130820     C                   Eval      SCFKSC      = MOVKsc
350300000000     C*
350400000000     C* uso la causale di testata perchè quella di riga sul
350500000000     C* movimento del cliente o fornitore non è modificabile
350600000000     C* e in caso di doc.IVA coincide con quella di testata
350700130820     C                   Eval      SCFCAUSALE  = MOVCausTes
350800000000     C*
350900130820     C                   Eval      SCFALIQ     = IVAAliq
351000130826     C                   Eval      SCFRIFIVA   = IVARifIVA
351100000000     C*
351200130820     C                   Eval      SCFDTDOC    = MOVDtDoc
351300130820     C                   Eval      SCFNRDOC    = MOVNrDoc
351400130820     C                   Eval      SCFSERDOC   = MOVSerDoc
351500000000     C*
351600130820     C                   Eval      SCFDTREG    = SavDtReg
351700130820     C                   Eval      SCFNRREG    = SavNrReg
351800130820     C                   Eval      SCFSERREG   = SavSerReg
351900000000     C*
352000130820     C                   Eval      SCFDTPAR    = SavDtPar
352100130820     C                   Eval      SCFNRPAR    = SavNrPar
352200130820     C                   Eval      SCFSERPAR   = SavSerPar
352300000000     C*
352400130820     C                   Eval      SCFSYS      = MOVSys
352500130820     C                   Eval      SCFNRASREG  = MOVNrAsReg
352600130820     C                   Eval      SCFNRRIGAM  = IVANrRigaM
352700130820     C                   Eval      SCFNRRIGA   = IVANrRiga
352800130821     C*
352900130822     C                   Eval      SCFTPREG    = IVATpReg
353000130822     C                   Eval      SCFLIBRO    = IVALibro
353100000000     C*
353200000000     C*
353300130821     C* classifico l'operazione in base alle regole impostate
353400130821     C                   EXSR      DefOperazione
353500130821     C*
353600130821     C*
353700130822     C* reperisco la tipologia degli importi dal movimento IVA
353800130822     C* Imponibile, non imponibile, esente, fuori campo IVA
353900130822     C                   EXSR      RepTpImp
354000000000     C*
354100130822     C*
354200130822     C* reperisco gli importi
354300130822     C                   EXSR      RepImporti
354400130822     C*
354500130822     C*
354600130822     C* TIPO VARIAZIONE DELLA NOTA
354700130822     C* - Diminuzione = Credito
354800130822     C* - Aumento = Debito
354900130822     C                   EXSR      RepVarNota
355000130822     C*
355100130826     C*
355200130826     C* definizione dei dati per i quadri
355300160104R195 C                   Select
355400160104 "   C* Spesometro
355500160104R195 C                   When      TpComG52    = 'E'
355600130826     C                   EXSR      DefQuadri
355700160104R195 C*
355800160104 "   C* Acquisti da San Marino
355900160104 "   C                   When      TpComG52    = 'S'
356000160104 "   C                   EXSR      DefQuadroSE
356100160104 "   C*
356200160104 "   C* Black List
356300160104 "   C                   When      TpComG52    = 'B'
356400160104 "   C                   EXSR      DefQuadroBL
356500160104 "   C                   endSl
356600160104R195 C*
356700131016R135 C*
356800131016 "   C* se si tratta di acquisti di beni da non residente,
356900131206R135 C* annullo il movimento
357000131206R141 X*ex R135           if        SCFNatOpe   = 'B'    and
357100131206 "   X*ex R135                     SCFTpOpe    = 'P'    and
357200131206R141 X*ex R135                     SCFQuadro   = 'SE'
357300140328R133 C*
357400151123R195 C* Spesometro  o Autofattura di Acquisti da San Marino
357401170822R257 C* o Black List
357500140328R133 C                   If        TpComG52    = 'E'    or
357501170822R257 C                             TpComG52    = 'B'    or
357600140328R133 C                             (TpComG52   = 'S'    and
357700140328R133 C                             SCFAutoFat  = '1')
357800140328R141 C*
357900131206 "   C                   if        (SCFNatOpe   = 'B'    and
358000131206 "   C                              SCFTpOpe    = 'P'    and
358100131206 "   C                              (SCFQuadro   = 'SE'  or
358200131206 "   C                               SCFQuadro   = 'BL'     )) or
358300150420R141 C                             (SCFAutofat  = '1'    and
358400150420R1786C                              $AutoCons  <> *On    and
358401160329R2202C                              SCFReverse <> '1'    and
358500150420R141 C                              SCFNatOpe   = 'B'    and
358600131206 "   C                              SCFTpOpe    = 'A'    and
358700131206 "   C                              (SCFQuadro   = 'FA'  or
358800131206 "   C                               SCFQuadro   = 'FE'  or
358900131206R141 C                               SCFQuadro   = 'NE'     ))
359000131206R135 C*
359100131016 "   C                   Eval      $AnnIVA     = *On
359200131016 "   C                   if        $TpAnn      = *Blank
359300131016 "   C                   Eval      $TpAnn      = '53'
359400160418R135 C* "Acquisto beni da non resisente"
359500160418R221AX*ex R135           EXSR      StpErr1
359600160418R135 C                   endif
359700131016 "   C*
359800131016R135 C                   endif
359900140328R133 C                   endif
359901170822R257 C* se si tratta di vendite di Beni da paesi Black List
359902170822 "   C* annullo il movimento
359903170822 "   C*
359904170822 "   C                   If        TpComG52    = 'B'
359905170822 "   C*
359906170822 "   C                   if        (SCFNatOpe   = 'B'    and
359907170822 "   C                              SCFTpOpe    = 'A'    and
359908170822 "   C                              (SCFQuadro   = 'FN'  or
359909170822 "   C                               SCFQuadro   = 'BL'     ))
359910170822 "   C*
359911170822 "   C                   Eval      $AnnIVA     = *On
359912170822 "   C                   if        $TpAnn      = *Blank
359913170822 "   C                   Eval      $TpAnn      = '60'
359914170822 "   C* "Vendita beni da non residente (Black list)
359915170822 "   C                   endif
359916170822 "   C*
359917170822 "   C                   endif
359918170822R257 C                   endif
360000000000     C*
360100130926     C*
360200130926     C* reperisco i dati anagrafici del primo record
360300130926     C* inserito con stessa partita IVA e codice fiscale
360400130926     C                   if        SCFPartIVA <> *Blank   or
360500130926     C                             SCFCdFisc  <> *Blank
360600130926     C                   EXSR      RepAnagraf
360700130926     C                   endif
360800151123R195 C*
360900151123 "   C* se Black List, al cambio dei dati anagrafici
361000151123 "   C* controllo la soglia (solo per gli operatori di cui
361100151123 "   C* sono stati estratti dei movimenti, gli altri non li
361200151123 "   C* tocco)
361300151123 "   C                   if        TpComG52     = 'B'           and
361400160201 "   C                             (SCFCogRagR <> SavCogRagR or
361500160201 "   C                              SCFNomeR   <> SavNomeR   or
361600160201 "   C                              SCFDtNasR  <> SavDtNasR  or
361700160201 "   C                              SCFLocNasR <> SavLocNasR or
361800160201 "   C                              SCFPrvNasR <> SavPrvNasR or
361900160201 "   C                              SCFStatoR  <> SavStatoR  or
362000160201 "   C                              SCFCodUICR <> SavCodUICR or
362100160201 "   C                              SCFLocaliR <> SavLocaliR or
362200160201 "   C                              SCFIndireR <> SavIndireR or
362300160201 "   C                              SCFTpNR    <> SavTpNR    or
362400160201 "   C                              SCFPIVAR   <> SavPIVAR   or
362500160201 "   C                              SCFCdFiscR <> SavCdFiscR or
362600160201 "   C                              SCFTpOpeR  <> SavTpOpeR  or
362700160201 "   C                              SCFNatOpeR <> SavNatOpeR or
362800160201 "   C                              SCFLeasinR <> SavLeasinR or
362900160201 "   C                              SCFDtRegR  <> SavDtRegR  or
363000160201 "   C                              SCFNrRegR  <> SavNrRegR  or
363100160201 "   C                              SCFSerRegR <> SavSerRegR   )
363200151202 "   C*
363300151202 "   C                   If        $PrimoSCF   <> *On
363400151123 "   C*
363500151123 "   C                   EXSR      CtrSogliaBl
363600151202 "   C*
363700151202 "   C                   Endif
363800151123 "   C*
363900151123 "   C                   Eval      $PrimoSCF   = *Off
364000160201 "   C                   Eval      SavCogRagR  = SCFCogRagR
364100160201 "   C                   Eval      SavNomeR    = SCFNomeR
364200160201 "   C                   Eval      SavDtNasR   = SCFDtNasR
364300160201 "   C                   Eval      SavLocNasR  = SCFLocNasR
364400160201 "   C                   Eval      SavPrvNasR  = SCFPrvNasR
364500160201 "   C                   Eval      SavStatoR   = SCFStatoR
364600160201 "   C                   Eval      SavCodUICR  = SCFCodUICR
364700160201 "   C                   Eval      SavLocaliR  = SCFLocaliR
364800160201 "   C                   Eval      SavIndireR  = SCFIndireR
364900160201 "   C                   Eval      SavTpNR     = SCFTpNR
365000160201 "   C                   Eval      SavPIVAR    = SCFPIVAR
365100160201 "   C                   Eval      SavCdFiscR  = SCFCdFiscR
365200160201 "   C                   Eval      SavTpOpeR   = SCFTpOpeR
365300160201 "   C                   Eval      SavNatOpeR  = SCFNatOpeR
365400160201 "   C                   Eval      SavLeasinR  = SCFLeasinR
365500160201 "   C                   Eval      SavDtRegR   = SCFDtRegR
365600160201 "   C                   Eval      SavNrRegR   = SCFNrRegR
365700160201 "   C                   Eval      SavSerRegR  = SCFSerRegR
365800151123R195 C                   endif
365900130822     C*
366000000000     C                   if        $AnnIVA     = *On
366100130820     C                   Eval      SCFANN      = *On
366200130820     C                   Eval      SCFTPANN    = $TpAnn
366300000000     C                   endif
366400000000     C*
366500000000     C                   ENDSR
366600000000     C/EJECT
366700131206R141 C************************************************************
366800131206 "   C* Imposto i dati anagrafici della società comunicante
366900131206 "   C************************************************************
367000131206R141 C     RieAnaSoc     BEGSR
367100131206     C*
367200131206     C* reperisco la natura giuridica del contribuente
367300131206     C                   Eval      $NatGiur    = XScNCR
367400131206     C                   EXSR      RepTpN
367500131206     C                   if        XTbErr      = '0'
367600131206     C                   MoveL     XTbUni        ANGG16DS
367700131206     C                   Eval      SCFTpN      = TpNG16
367800131206     C                   endif
367900131206     C*
368000131206     C                   Select
368100131206     C*-
368200131206     C*-----------------------
368300131206     C* PERSONA FISICA
368400131206     C                   When      SCFTpN      = '1'
368500131206     C*
368600131206     C                   Eval      SO3Sogg     = SocSogg
368700131206     C                   Move      *Loval        SO3DtFiVl
368800131206     C                   Eval      SO3Sys      = 0
368900131206     C                   Eval      SO3NrAsInt  = 0
369000131206     C*
369100160523R224 X**** K04SO301      CHAIN     SDGSO301L                          21
369101160523R224 C                   EXSR      ChainSDGSO3
369102160523R224 C*
369200131206     C                   if        *In21       = *Off
369300131206     C                   MoveL(P)  SO3CdFisc     SCFCdFisc
369400131206     C                   MoveL(P)  SO3PartIva    SCFPartIVA
369500131206     C                   MoveL(P)  SO3CogRag     SCFCogRag
369600131206     C                   MoveL(P)  SO3Nome       SCFNome
369700131206     C                   Move      SO3DtNasc     SCFDtNasc
369800131206     C                   MoveL(P)  SO3LocNasc    SCFLocNasc
369900131206     C                   MoveL(P)  SO3PrvNasc    SCFPrvNasc
370000131206     C                   MoveL(P)  SO3Stato      SCFStato
370100131206     C                   MoveL(P)  SO3CodUIC     SCFCodUIC
370200131206     C                   MoveL(P)  SO3Localit    SCFLocalit
370300131206     C                   MoveL(P)  SO3IndirE     SCFIndirE
370400131206     C                   endif
370500131206     C*
370600131206     C*-----------------------
370700131206     C* PERSONA GIURIDICA
370800131206     C                   When      SCFTpN      = '2'  or
370900131206     C                             SCFTpN      = '3'
371000131206     C*-
371100131206     C* Denominazione
371200131206     C                   MoveL(P)  XScRgS        SCFCogRag
371300131206     C                   MoveL(P)  XScCdF        SCFCdFisc
371400131206     C                   MoveL(P)  XScIVA        SCFPartIVA
371500131206     C                   MoveL(P)  *Blank        SCFNome
371600131206     C                   Move      *Loval        SCFDtNasc
371700131206     C                   MoveL(P)  *Blank        SCFLocNasc
371800131206     C                   MoveL(P)  *Blank        SCFPrvNasc
371900131206     C                   MoveL(P)  XScSta        SCFStato
372000131206     C                   MoveL(P)  $UICSoc       SCFCodUIC
372100131206     C                   MoveL(P)  XScCit        SCFLocalit
372200131206     C                   MoveL(P)  XScVia        SCFIndirE
372300131206     C*
372400131206     C                   EndSl
372401160405R2216C*
372402160405 "   C* riapplico i filtri anagrafici sui dati "drogati"
372403160405 "   C* dell'autofattura
372404160405 "   C                   Select
372405160405 "   C*
372406160405 "   C                   When      SCFStato   <> StatoG52   and
372407160405 "   C                             StatoG52   <> *Blank
372408160405 "   C                   Eval      $WriteSCF   = *Off
372409160405 "   C*
372410160405 "   C                   When      SCFPartIVA <> PartIVAG52 and
372411160405 "   C                             PartIVAG52 <> *Blank
372412160405 "   C                   Eval      $WriteSCF   = *Off
372413160405 "   C*
372414160405 "   C                   When      SCFCdFisc  <> CdFiscG52  and
372415160405 "   C                             CdFiscG52  <> *Blank
372416160405 "   C                   Eval      $WriteSCF   = *Off
372417160405 "   C*
372418160405R2216C                   endSl
372500131206     C*-
372600131206     C*
372700131206     C                   Eval      $CampoScan  = SCFCogRag
372800131206     C                   EXSR      TolgoApice
372900131206     C                   Eval      SCFCOGRAG   = $CampoScan
373000131206     C*
373100131206     C                   Eval      $CampoScan  = SCFNome
373200131206     C                   EXSR      TolgoApice
373300131206     C                   Eval      SCFNOME     = $CampoScan
373400131206     C*
373500131206     C                   Eval      $CampoScan  = SCFLocNasc
373600131206     C                   EXSR      TolgoApice
373700131206     C                   Eval      SCFLOCNASC  = $CampoScan
373800131206     C*
373900131206     C                   Eval      $CampoScan  = SCFLocalit
374000131206     C                   EXSR      TolgoApice
374100131206     C                   Eval      SCFLOCALIT  = $CampoScan
374200131206     C*
374300131206     C                   Eval      $CampoScan  = SCFIndirE
374400131206     C                   EXSR      TolgoApice
374500131206     C                   Eval      SCFINDIRE   = $CampoScan
374600131206     C*
374700131206R141 C                   ENDSR
374800131206R141 C/EJECT
374900000000     C************************************************************
375000000000     C* Tolgo l'apice dai campi descrittivi
375100000000     C************************************************************
375200000000     C     TolgoApice    BEGSR
375300000000     C*
375400000000     C                   Eval      $FineScan   = *Off
375500000000     C*
375600000000     C                   DoW       $FineScan   = *Off
375700000000     C                   Clear                   PosIni            4 0
375800000000     C     ''''          Scan      $CampoScan    PosIni                   22
375900000000     C     *IN22         IFEQ      *On
376000000000     C                   Eval      %SubSt($CampoScan:Posini:1)=' '
376100000000     C                   else
376200000000     C                   Eval      $FineScan   = *On
376300000000     C                   endif
376400000000     C                   endDo
376500000000     C*
376600000000     C                   ENDSR
376700000000     C/EJECT
376800130821     C************************************************************
376900130821     C* Classifico l'operazione in base alle regole fissate
377000130821     C************************************************************
377100130821     C     DefOperazione BEGSR
377200130821     C*
377300130821     C* -------------------------
377400130821     C* TIPO DOCUMENTO
377500130821     C                   EXSR      RepTpDoc
377600130821     C*
377700130821     C* -------------------------
377800130821     C* NATURA OPERAZIONE
377900130821     C* Beni / Servizi
378000130822     C                   EXSR      RepNatOpe
378100130821     C*
378200130821     C* -------------------------
378300130821     C* TIPO OPERAZIONE
378400130918     C* Passive (acquisti), Attive (vendite e corrispettivi)
378500130918     C                   if        SCFTpDoc    = 'S'
378600130918     C                   Eval      SCFTpOpe    = 'A'
378700130918     C                   else
378800130821     C                   if        IVATpReg    = '1'
378900130821     C                   Eval      SCFTpOpe    = 'P'
379000130821     C                   else
379100130821     C                   Eval      SCFTpOpe    = 'A'
379200130821     C                   endif
379300130918     C                   endif
379400131206R141 C*
379500131206 "   C* in caso di Autofattura,
379600131206 "   C* dico che l'operazione è Attiva perchè è come se fosse una
379700131206 "   C* fattura emessa dall'azienda comunicante
379800131206 "   C                   if        $Autofatt   = *On         or
379900131206 "   C                             OPETpDocI   = '8'         or
380000131206 "   C                             (MOVSNatura  = 'F'   and
380100131206 "   C                              IVATpReg    = '2'   and
380200131206 "   C                              DLITerrito  = '1'       )
380300131206 "   C                   Eval      SCFTpOpe    = 'A'
380400131206R141 C                   endif
380500130821     C*
380600130821     C* -------------------------
380700130821     C* OPERAZIONE registrata senza NDIVA
380800130821     C*
380900110902     C                   if        $ChkNDIVA   = *On
381000130820     C                   Eval      SCFNoIVA    = *Off
381100110902     C                   else
381200110902     C*
381300130821     C* QUADRO 'SE' - acquisti di servizio per non residenti
381400110902     C* in caso di importazioni senza IVA considero che siano
381500130930     C* Tutti NON imponibili e di servizi
381600130820     C                   Eval      SCFNoIVA    = *On
381700110902     C                   endif
381800130821     C*
381900130821     C* -------------------------
382000130821     C* TIPO DI FATTURA
382100130821     C* Regime del margine, Reverse charge, autofattura
382200130826     C                   Eval      SCFMargine  = '0'
382300130826     C                   Eval      SCFReverse  = '0'
382400130826     C                   Eval      SCFAutofat  = '0'
382500130821     C                   if        $ChkNDIVA   = *On
382600130821     C                   EXSR      RepTpFatt
382700130821     C                   endif
382800130821     C*
382900130821     C* -------------------------
383000130821     C* NOLEGGIO/LEASING
383100130930     C* Prevediamo che sia gestito solo dalle società di leasing
383200130821     C*
383300130828     C                   Eval      SCFLeasing  = '0'
383400151217R195 C* se NON è Black List, ma Spesometro o Acquisti San Marino
383500151217R195 C                   if        TpComG52   <> 'B'
383600130828     C                   Eval      SPRTipReg   = '4'
383700130828     C                   Eval      SPRSTpReg   = '3'
383800130828     C                   EXSR      ChkRegDoc
383900130828     C                   if        $OkRegDoc    = *On
384000130828     C                   Eval      SCFLeasing  = '1'
384100130828     C                   endif
384200000000     C*
384300151123R195 C                   endif
384400151123R195 C*
384500000000     C                   ENDSR
384600000000     C/EJECT
384700130821     C************************************************************
384800130821     C* Reperisco il tipo documento
384900130821     C************************************************************
385000130821     C     RepTpDoc      BEGSR
385100130821     C*
385200130821     C* Può essere :
385300130821     C* - fattura
385400130821     C* - nota di variazione
385500130821     C* - documento riepilogativo
385600130821     C* - documento senza fattura (scontrini, non vengono estratti
385700130821     C*                            in automatico da questo pgm)
385800130821     C* Imposto che è fattura, poi saranno le regole a definire se
385900130821     C* si tratta di altro documento
386000130821     C                   Eval      SCFTpDoc    = 'F'
386100130821     C*
386200151123R195 C* se NON è Black List, ma Spesometro o Acquisti San Marino
386300151123R195 C                   if        TpComG52   <> 'B'
386400130821     C* ---------------
386500130821     C* DOCUMENTO RIEPILOGATIVO
386600130828     C                   Eval      SPRTipReg   = '4'
386700130828     C                   Eval      SPRSTpReg   = '2'
386800130828     C                   EXSR      ChkRegDoc
386900130828     C                   if        $OkRegDoc    = *On
387000130828     C* se trovo il record significa che è documento riepilogativo
387100130828     C                   Eval      SCFTpDoc    = 'R'
387200130828     C                   endif
387300151123R195 C                   endif
387400151123R195 C*
387500130821     C*
387600130821     C*
387700130821     C* ---------------
387800130821     C* NOTA DI VARIAZIONE - in diminuzione o in aumento
387900130821     C                   if        SCFTpDoc    = 'F'      and
388000130821     C                             (OPETpDocI = '1'   or
388100130821     C                              OPETpDocI = '4'   or
388200130821     C                              OPETpDocI = '7'     )
388300130821     C*
388400130821     C                   Eval      SCFTpDoc    = 'N'
388500130823     C*
388600130823     C                   endif
388700130823     C*
388800130823     C*
388900130823     C* ---------------
389000130823     C* NOTA DI VARIAZIONE - in aumento
389100130823     C                   if        SCFTpDoc    = 'F'
389200130823     C* verifico se la causale è catalogata come nota in addebito
389300130823     C                   EXSR      ChkRegNotaAdd
389400130823     C                   endif
389500130823     C*
389600130823     C                   ENDSR
389700130823     C/EJECT
389800130823     C************************************************************
389900130823     C* Verifico se si tratta di causale/conto per documento
390000130828     C* riepilogativo e leasing
390100130823     C************************************************************
390200130828     C     ChkRegDoc     BEGSR
390300130823     C*
390400130828     C                   Eval      $OkRegDoc   = *Off
390500130823     C*
390600130823     C* 1° tentativo con causale + KCC + KSC
390700130823     C                   Eval      SPRSocieta  = XScSoc
390800130823     C                   Eval      SPRTPREG    = *Blank
390900130823     C                   Eval      SPRLIBRO    = *Blank
391000130823     C                   Eval      SPRALIQ     = *Zero
391100130823     C                   Eval      SPRRIFIVA   = *Blank
391200130823     C                   Eval      SPRCAUSALE  = MOVCausTes
391300130823     C                   Eval      SPRKCC      = MOVKCC
391400130823     C                   Eval      SPRKSC      = MOVKSC
391500130823     C                   Eval      SPRVoce     = *Blank
391600130823     C                   Eval      SPRClasse   = *Blank
391700130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
391800130823     C                   if        *In21       = *Off
391900130828     C                   Eval      $OkRegDoc   = *On
392000130823     C                   endif
392100130823     C*
392200130828     C                   if        $OkRegDoc   = *Off
392300130823     C* 2° tentativo con KCC + KSC
392400130823     C                   Eval      SPRSocieta  = XScSoc
392500130823     C                   Eval      SPRTPREG    = *Blank
392600130823     C                   Eval      SPRLIBRO    = *Blank
392700130823     C                   Eval      SPRALIQ     = *Zero
392800130823     C                   Eval      SPRRIFIVA   = *Blank
392900130823     C                   Eval      SPRCAUSALE  = *Blank
393000130823     C                   Eval      SPRKCC      = MOVKCC
393100130823     C                   Eval      SPRKSC      = MOVKSC
393200130823     C                   Eval      SPRVoce     = *Blank
393300130823     C                   Eval      SPRClasse   = *Blank
393400130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
393500130823     C                   if        *In21       = *Off
393600130828     C                   Eval      $OkRegDoc   = *On
393700130823     C                   endif
393800130823     C                   endif
393900130823     C*
394000130828     C                   if        $OkRegDoc   = *Off
394100130823     C* 3° tentativo con causale
394200130823     C                   Eval      SPRSocieta  = XScSoc
394300130823     C                   Eval      SPRTPREG    = *Blank
394400130823     C                   Eval      SPRLIBRO    = *Blank
394500130823     C                   Eval      SPRALIQ     = *Zero
394600130823     C                   Eval      SPRRIFIVA   = *Blank
394700130823     C                   Eval      SPRCAUSALE  = MOVCausTes
394800130823     C                   Eval      SPRKCC      = *Blank
394900130823     C                   Eval      SPRKSC      = *Blank
395000130823     C                   Eval      SPRVoce     = *Blank
395100130823     C                   Eval      SPRClasse   = *Blank
395200130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
395300130823     C                   if        *In21       = *Off
395400130828     C                   Eval      $OkRegDoc   = *On
395500130823     C                   endif
395600130823     C                   endif
395700130823     C*
395800130823     C                   ENDSR
395900130823     C/EJECT
396000130826     C************************************************************
396100130826     C* Verifico se si tratta di causale per nota di
396200130826     C* di variazione in addebito
396300130826     C************************************************************
396400130826     C     ChkRegNotaAdd BEGSR
396500130821     C*
396600120123     C* verifico se causale è di nota di addebito
396700130823     C                   Eval      SPRSocieta  = XScSoc
396800130823     C                   Eval      SPRTipReg   = '4'
396900130823     C                   Eval      SPRSTpReg   = '1'
397000130823     C                   Eval      SPRTPREG    = *Blank
397100130823     C                   Eval      SPRLIBRO    = *Blank
397200130823     C                   Eval      SPRALIQ     = *Zero
397300130823     C                   Eval      SPRRIFIVA   = *Blank
397400130823     C                   Eval      SPRCAUSALE  = MOVCausTes
397500130823     C                   Eval      SPRKCC      = *Blank
397600130823     C                   Eval      SPRKSC      = *Blank
397700130823     C                   Eval      SPRVoce     = *Blank
397800130823     C                   Eval      SPRClasse   = *Blank
397900130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
398000120123     C                   if        *In21       = *Off
398100120123     C* se trovo il record significa che è nota di addebito
398200130821     C                   Eval      SCFTpDoc    = 'N'
398300120123     C                   endif
398400130826     C*
398500130826     C                   ENDSR
398600130826     C/EJECT
398700000000     C************************************************************
398800130821     C* Reperisco il tipo di fattura
398900000000     C************************************************************
399000130821     C     RepTpFatt     BEGSR
399100000000     C*
399200151123R195 C* se NON è Black List, ma Spesometro o Acquisti San Marino
399300151123R195 C                   if        TpComG52   <> 'B'
399400130821     C* ---------------
399500130821     C* REGIME DEL MARGINE
399600130821     C* verifico se si tratta di imponibile con
399700130821     C* IVA a Margine
399800130821     C* (cioè IVA non esposta in fattura)
399900130821     C* es. agenzie viaggio e cessione di beni usati (art.74 e 74ter)
400000130823     C                   Eval      SPRTipReg   = '1'
400100130823     C                   Eval      SPRSTpReg   = '2'
400200130924     C                   EXSR      ChkRegMargine
400300130924     C                   if        $OkRegMargine = *On
400400130821     C                   Eval      SCFMargine  = '1'
400500131014R135 C                   Eval      $FoundMarg  = *On
400600130821     C                   endif
400700130821     C*
400800130821     C* ---------------
400900130821     C* REVERSE CHARGE
401000000000     C* verifico se si tratta di imponibile con
401100000000     C* imponibile di "Reverse charge"
401200000000     C* es. compravendita di oro e argento, servizi edili di subappalto
401300000000     C*     (art. 17);
401400000000     C*     compravendita di rottami e metalli non ferrosi
401500000000     C*     (art.74 commi 7 e 8).
401600130823     C                   Eval      SPRTipReg   = '1'
401700130823     C                   Eval      SPRSTpReg   = '1'
401800130924     C                   EXSR      ChkRegReverse
401900130924     C                   if        $OkRegReverse = *On
402000130826     C                   Eval      SCFReverse  = '1'
402100110829     C                   endif
402200130821     C*
402300151123R195 C                   endif
402400151123R195 C*
402500130821     C* ---------------
402600130821     C* AUTOFATTURA
402700130821     C* verifico se si tratta di fattura di vendita per OMAGGI,
402800130821     C* AUTOCONSUMO (può essere anche fattura di reverse charge)
402900130821     C* In questo caso il cliente è sempre la società che emette
403000130821     C* la fattura
403100131206R141 X****               if        SCFPartIVA  = XScIVA    and
403200131206R141 X****                         SCFTpReg    = '2'
403300131206R141 C*
403400150420R1786X*ex R141           if        (SCFPartIVA  = XScIVA    and
403500150420 "   X*ex R141                      SCFTpReg    = '2'          ) or
403600150420R1786X*ex R141                     $Autofatt   = *On
403700130821     C*
403800150420R1786C                   if        $Autofatt   = *On
403900130821     C                   Eval      SCFAutofat  = '1'
404000130821     C*
404100130821     C                   endif
404200000000     C*
404300000000     C                   ENDSR
404400000000     C/EJECT
404500130924     C************************************************************
404600130924     C* Verifico se si tratta di Regime del margine
404700130924     C************************************************************
404800130924     C     ChkRegMargine BEGSR
404900130924     C*
405000130924     C                   Eval      $OkRegMargine = *Off
405100130924     C*
405200130924     C                   Eval      SPRSocieta  = XScSoc
405300130924     C*
405400130924     C*------------------------------
405500130924     C* 1° tentativo con chiave completa:
405600130924     C*    assoc. IVA e rif. IVA + causale + conto
405700130924     C                   Eval      SPRTPREG    = IVATpReg
405800130924     C                   Eval      SPRLIBRO    = IVALibro
405900130924     C                   Eval      SPRALIQ     = IVAAliq
406000130924     C                   Eval      SPRRIFIVA   = IVARifIVA
406100130924     C                   Eval      SPRCAUSALE  = MOVCausTes
406200130924     C                   Eval      SPRKCC      = MOVKcc
406300130924     C                   Eval      SPRKSC      = MOVKsc
406400130924     C                   Eval      SPRVOCE     = *Blank
406500130924     C                   Eval      SPRCLASSE   = *Blank
406600130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
406700130924     C                   if        *In21       = *Off
406800130924     C                   Eval      $OkRegMargine = *On
406900130924     C                   endif
407000130924     C*
407100130924     C*------------------------------
407200130924     C* 2° tentativo: chiave senza associazione:
407300130924     C*    causale + conto
407400130924     C                   if        $OkRegMargine = *Off
407500130924     C                   Eval      SPRTPREG    = *Blank
407600130924     C                   Eval      SPRLIBRO    = *Blank
407700130924     C                   Eval      SPRALIQ     = 0
407800130924     C                   Eval      SPRRIFIVA   = *Blank
407900130924     C                   Eval      SPRCAUSALE  = MOVCausTes
408000130924     C                   Eval      SPRKCC      = MOVKcc
408100130924     C                   Eval      SPRKSC      = MOVKsc
408200130924     C                   Eval      SPRVOCE     = *Blank
408300130924     C                   Eval      SPRCLASSE   = *Blank
408400130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
408500130924     C                   if        *In21       = *Off
408600130924     C                   Eval      $OkRegMargine = *On
408700130924     C                   endif
408800130924     C                   endif
408900130924     C*
409000130924     C*------------------------------
409100130924     C* 3° tentativo: chiave senza associazione e senza causale:
409200130924     C*    SOLO conto
409300130924     C                   if        $OkRegMargine = *Off
409400130924     C                   Eval      SPRTPREG    = *Blank
409500130924     C                   Eval      SPRLIBRO    = *Blank
409600130924     C                   Eval      SPRALIQ     = 0
409700130924     C                   Eval      SPRRIFIVA   = *Blank
409800130924     C                   Eval      SPRCAUSALE  = *Blank
409900130924     C                   Eval      SPRKCC      = MOVKcc
410000130924     C                   Eval      SPRKSC      = MOVKsc
410100130924     C                   Eval      SPRVOCE     = *Blank
410200130924     C                   Eval      SPRCLASSE   = *Blank
410300130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
410400130924     C                   if        *In21       = *Off
410500130924     C                   Eval      $OkRegMargine = *On
410600130924     C                   endif
410700130924     C                   endif
410800130924     C*
410900130924     C*------------------------------
411000130924     C* 4° tentativo: chiave senza associazione e senza conto:
411100130924     C*    SOLO causale
411200130924     C                   if        $OkRegMargine = *Off
411300130924     C                   Eval      SPRTPREG    = *Blank
411400130924     C                   Eval      SPRLIBRO    = *Blank
411500130924     C                   Eval      SPRALIQ     = 0
411600130924     C                   Eval      SPRRIFIVA   = *Blank
411700130924     C                   Eval      SPRCAUSALE  = MOVCausTes
411800130924     C                   Eval      SPRKCC      = *Blank
411900130924     C                   Eval      SPRKSC      = *Blank
412000130924     C                   Eval      SPRVOCE     = *Blank
412100130924     C                   Eval      SPRCLASSE   = *Blank
412200130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
412300130924     C                   if        *In21       = *Off
412400130924     C                   Eval      $OkRegMargine = *On
412500130924     C                   endif
412600130924     C                   endif
412700130924     C*
412800130924     C*------------------------------
412900130924     C* 5° tentativo: chiave causale e senza conto:
413000130924     C*    SOLO associazione
413100130924     C                   if        $OkRegMargine = *Off
413200130924     C                   Eval      SPRTPREG    = IVATpReg
413300130924     C                   Eval      SPRLIBRO    = IVALibro
413400130924     C                   Eval      SPRALIQ     = IVAAliq
413500130924     C                   Eval      SPRRIFIVA   = IVARifIVA
413600130924     C                   Eval      SPRCAUSALE  = *Blank
413700130924     C                   Eval      SPRKCC      = *Blank
413800130924     C                   Eval      SPRKSC      = *Blank
413900130924     C                   Eval      SPRVOCE     = *Blank
414000130924     C                   Eval      SPRCLASSE   = *Blank
414100130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
414200130924     C                   if        *In21       = *Off
414300130924     C                   Eval      $OkRegMargine = *On
414400130924     C                   endif
414500130924     C                   endif
414600130924     C*
414700130924     C*------------------------------
414800130924     C*
414900130924     C                   ENDSR
415000130924     C/EJECT
415100130924     C************************************************************
415200130924     C* Verifico se si tratta di Reverse charge
415300130924     C************************************************************
415400130924     C     ChkRegReverse BEGSR
415500130924     C*
415600130924     C                   Eval      $OkRegReverse = *Off
415700130924     C*
415800130924     C                   Eval      SPRSocieta  = XScSoc
415900130924     C*
416000130924     C*------------------------------
416100130924     C* 1° tentativo con chiave completa:
416200130924     C*    assoc. IVA e rif. IVA + causale + conto
416300130924     C                   Eval      SPRTPREG    = IVATpReg
416400130924     C                   Eval      SPRLIBRO    = IVALibro
416500130924     C                   Eval      SPRALIQ     = IVAAliq
416600130924     C                   Eval      SPRRIFIVA   = IVARifIVA
416700130924     C                   Eval      SPRCAUSALE  = MOVCausTes
416800130924     C                   Eval      SPRKCC      = MOVKcc
416900130924     C                   Eval      SPRKSC      = MOVKsc
417000130924     C                   Eval      SPRVOCE     = *Blank
417100130924     C                   Eval      SPRCLASSE   = *Blank
417200130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
417300130924     C                   if        *In21       = *Off
417400130924     C                   Eval      $OkRegReverse = *On
417500130924     C                   endif
417600130924     C*
417700130924     C*------------------------------
417800130924     C* 2° tentativo: chiave senza conto/sottoconto:
417900130924     C*    assoc. IVA e rif. IVA + causale
418000130924     C                   if        $OkRegReverse = *Off
418100130924     C                   Eval      SPRTPREG    = IVATpReg
418200130924     C                   Eval      SPRLIBRO    = IVALibro
418300130924     C                   Eval      SPRALIQ     = IVAAliq
418400130924     C                   Eval      SPRRIFIVA   = IVARifIVA
418500130924     C                   Eval      SPRCAUSALE  = MOVCausTes
418600130924     C                   Eval      SPRKCC      = *blank
418700130924     C                   Eval      SPRKSC      = *Blank
418800130924     C                   Eval      SPRVOCE     = *Blank
418900130924     C                   Eval      SPRCLASSE   = *Blank
419000130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
419100130924     C                   if        *In21       = *Off
419200130924     C                   Eval      $OkRegReverse = *On
419300130924     C                   endif
419400130924     C                   endif
419500130924     C*
419600130924     C*------------------------------
419700130924     C* 3° tentativo: chiave senza causale
419800130924     C*    assoc. IVA e rif. IVA + causale
419900130924     C                   if        $OkRegReverse = *Off
420000130924     C                   Eval      SPRTPREG    = IVATpReg
420100130924     C                   Eval      SPRLIBRO    = IVALibro
420200130924     C                   Eval      SPRALIQ     = IVAAliq
420300130924     C                   Eval      SPRRIFIVA   = IVARifIVA
420400130924     C                   Eval      SPRCAUSALE  = *Blank
420500130924     C                   Eval      SPRKCC      = MOVKcc
420600130924     C                   Eval      SPRKSC      = MOVKsc
420700130924     C                   Eval      SPRVOCE     = *Blank
420800130924     C                   Eval      SPRCLASSE   = *Blank
420900130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
421000130924     C                   if        *In21       = *Off
421100130924     C                   Eval      $OkRegReverse = *On
421200130924     C                   endif
421300130924     C                   endif
421400130924     C*
421500130924     C*------------------------------
421600130924     C* 4° tentativo: chiave senza conto nè causale
421700130924     C*    assoc. IVA e rif. IVA
421800130924     C                   if        $OkRegReverse = *Off
421900130924     C                   Eval      SPRTPREG    = IVATpReg
422000130924     C                   Eval      SPRLIBRO    = IVALibro
422100130924     C                   Eval      SPRALIQ     = IVAAliq
422200130924     C                   Eval      SPRRIFIVA   = IVARifIVA
422300130924     C                   Eval      SPRCAUSALE  = *Blank
422400130924     C                   Eval      SPRKCC      = *Blank
422500130924     C                   Eval      SPRKSC      = *Blank
422600130924     C                   Eval      SPRVOCE     = *Blank
422700130924     C                   Eval      SPRCLASSE   = *Blank
422800130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
422900130924     C                   if        *In21       = *Off
423000130924     C                   Eval      $OkRegReverse = *On
423100130924     C                   endif
423200130924     C                   endif
423300130924     C*
423400130924     C*------------------------------
423500130924     C* 5° tentativo: chiave con solo aliquota/riferimento IVA
423600130924     C                   if        $OkRegReverse = *Off
423700130924     C                   Eval      SPRTPREG    = *Blank
423800130924     C                   Eval      SPRLIBRO    = *Blank
423900130924     C                   Eval      SPRALIQ     = IVAAliq
424000130924     C                   Eval      SPRRIFIVA   = IVARifIVA
424100130924     C                   Eval      SPRCAUSALE  = *Blank
424200130924     C                   Eval      SPRKCC      = *Blank
424300130924     C                   Eval      SPRKSC      = *Blank
424400130924     C                   Eval      SPRVOCE     = *Blank
424500130924     C                   Eval      SPRCLASSE   = *Blank
424600130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
424700130924     C                   if        *In21       = *Off
424800130924     C                   Eval      $OkRegReverse = *On
424900130924     C                   endif
425000130924     C                   endif
425100130924     C*
425200130924     C*------------------------------
425300130924     C* 6° tentativo: chiave con solo tipo registro IVA
425400130924     C*    e libro IVA
425500130924     C                   if        $OkRegReverse = *Off
425600130924     C                   Eval      SPRTPREG    = IVATpReg
425700130924     C                   Eval      SPRLIBRO    = IVALibro
425800130924     C                   Eval      SPRALIQ     = 0
425900130924     C                   Eval      SPRRIFIVA   = *Blank
426000130924     C                   Eval      SPRCAUSALE  = *Blank
426100130924     C                   Eval      SPRKCC      = *Blank
426200130924     C                   Eval      SPRKSC      = *Blank
426300130924     C                   Eval      SPRVOCE     = *Blank
426400130924     C                   Eval      SPRCLASSE   = *Blank
426500130924     C     K12SPR04      CHAIN     SDGSPR04L                          21
426600130924     C                   if        *In21       = *Off
426700130924     C                   Eval      $OkRegReverse = *On
426800130924     C                   endif
426900130924     C                   endif
427000130924     C*
427100130924     C*------------------------------
427200130924     C*
427300130924     C                   ENDSR
427400130924     C/EJECT
427500000000     C************************************************************
427600130821     C* Reperisco la natura dell'operazione
427700000000     C************************************************************
427800130826     C     RepNatOpe     BEGSR
427900130826     C*
428000151130R195 X**** Al momento non è gestita, ma lascio la predisposizione
428100151130R195 X**** all'acquisizione dell'informazione
428200130826     C*
428300130826     C* esamino le contropartite solo a cambio di registrazione
428400130826     C                   IF        IVASys     <> SAV2Sys   or
428500130826     C                             IVANrAsReg <> SAV2NrAsReg
428501160711R2322C                             OR
428502160711 "   C                             (TpComG52    = 'B'     and
428503170822R2322C                              AlleIVA002  = '02'        )
428600130826     C*
428700130826     C* verifica default da attributo contabile
428800130826     C                   EXSR      ChkBenServAttr
428900130826     C*
429000130826     C                   If        $NatOpe    = *blanks
429100130826     C                   EXSR      ChkBeniServizi
429200130826     C                   EndIf
429300130918     C*
429400130918     C                   If        $NatOpe    = *blanks
429500130918     C* Imposto il default dal parametro modulo
429600151201R195 C* Spesometro o Acquisti da San Marino
429700151201R195 C                   if        TpComG52    = 'E'     or
429800151201R195 C                             TpComG52    = 'S'
429900130918     C                   Eval      $NatOpe         = BeniSrvA08
430000151201R195 C                   endif
430100151201 "   C*
430200151201 "   C* Black List
430300151201 "   C                   if        TpComG52    = 'B'
430400151201 "   C                   Eval      $NatOpe         = BeniSrvA11
430500151201R195 C                   endif
430600130918     C                   EndIf
430700130826     C*
430800130826     C                   EndIf
430900130826     C*
431000000000     C* ripristino i dati del movimento del cliente/fornitore
431100000000     C                   MoveL     NDMOVSav      NDMOV00F
431200000000     C*
431300000000     C                   Move      IVASys        SAV2Sys
431400000000     C                   Move      IVANrAsReg    SAV2NrAsReg
431500000000     C*
431600130821     C                   Eval      SCFNatOpe   = $NatOpe
431700000000     C*
431800000000     C                   ENDSR
431900000000     C/EJECT
432000130821     C************************************************************
432100130822     C* Reperisco la tipologia degli importi da movimento IVA
432200130821     C************************************************************
432300130822     C     RepTpImp      BEGSR
432400130821     C*
432500130822     C* ATTENZIONE
432600130822     C* la tipologia importo è  gestita solo per Black List
432700130822     C                   Eval      SCFTpImp    = ' '
432800130822     C*
432900130822     C* se i dati derivano da NDIVA, cerco comunque di classificare
433000130822     C* l'importo, nel caso di QUADRO 'SE' operazioni per non
433100130822     C* residenti registrate senza NDIVA00F il tipo importo
433200130822     C* rimane non definitio
433300130821     C                   if        $ChkNDIVA   = *On
433400130822     C*
433500130822     C                   Select
433600130822     C* movimento imponibile e relativa imposta
433700130822     C                   When      IVAAliq     > 0
433800130822     C                   Eval      SCFTpImp    = 'I'
433900130822     C*
434000130822     C* movimento NON imponibile
434100130822     C                   When      %subst(RIVFlg:4:1) = '1'   and
434200130822     C                             IVARifIva  <> *Blank
434300130822     C                   Eval      SCFTpImp    = 'N'
434400130822     C*
434500130822     C* movimento esente
434600130822     C                   When      %subst(RIVFlg:3:1) = '1'   and
434700130822     C                             IVARifIva  <> *Blank
434800130822     C                   Eval      SCFTpImp    = 'E'
434900130822     C*
435000130822     C                   Other
435100130822     C*
435200130822     C* Fuori campo IVA
435300130822     C                   Eval      SCFTpImp    = 'F'
435400151222R195 C*
435500151222 "   C                   Select
435600151222 "   C* se Spesometro o Acquisti da San Marino
435700151222 "   C                   when      TpComG52    = 'E'  or
435800151222R195 C                             TpComG52    = 'S'
435900130822     C* ATTENZIONE
436000130822     C* anche per lo Spesometro 2013 manteniamo l'esclusione dei fuori
436100130822     C* campo IVA perchè non indicato esplicitamente nel tracciato che
436200130822     C* ci vogliono (per imponibile, non imponibile ed esente è presente
436300130822     C* l'indicazione che ci devono essere)
436400131025R138 C* se la registrazione ha più righe IVA con problemi per
436500131025 "   C* attribuire la natura, considero predominante l'annullamento
436600131025R138 C* per controlli specifici sulla riga IVA
436700130822     C                   Eval      $AnnIVA     = *On
436800130822     C                   if        $TpAnn      = *Blank
436900131025R138 C                             or
437000131025R138 C                             $TpAnn      = '20'
437100130822     C                   Eval      $TpAnn      = '15'
437200130822     C* esclusione movimento fuori campo IVA
437300160418R221AX****               EXSR      StpErr1
437400130822     C                   endif
437500151222R195 C*
437600151222 "   C* se Black List
437700151222 "   C                   when      TpComG52    = 'B'
437800151222 "   C* Per i movimenti FUORI CAMPO IVA dall'1/09/10,
437900151222 "   C* c'è OBBLIGO di presentazione SOLO se riguardano
438000151222 "   C* i SERVIZI (vengono estratti tutti, anche quelli
438100151222 "   C* riferiti a beni).
438200151222 "   C*
438300151222R195 C                   endSl
438400130822     C*
438500130822     C                   endSl
438600130821     C*
438601160329R2201C                   else
438602160329 "   C* se Black List
438603160329 "   C                   if        TpComG52    = 'B'
438604160329 "   C* Fuori campo IVA
438605160329 "   C                   Eval      SCFTpImp    = 'F'
438606160329 "   C                   endif
438607160329R2201C*
438700130821     C                   endif
438800130821     C*
438900130821     C                   ENDSR
439000130821     C/EJECT
439100000000     C************************************************************
439200000000     C* Reperimento importi da movimento IVA
439300000000     C************************************************************
439400130822     C     RepImporti    BEGSR
439500000000     C*
439600111021     C*---------------------------------------------------------------------------
439700111021     C*
439800111021     C* LEGENDA:
439900111021     C*
440000130820     C* SCFIMPOPEO = IVAIMPONIB
440100130820     C* SCFIVAOPEO = IVAIMPORTO
440200130930     C* SCFIMPOPE  = SCFIMPOPEO
440300130930     C* SCFIVAOPE  = SCFIVAOPEO
440400130930     C* SCFIMPTOT  = 0
440500130930     C* SCFIVATOT  = 0
440600130930     C* SCFIMPSOGL = 0
440700130930     C* SCFIVASOGL = 0
440800111021     C*
440900111021     C*---------------------------------------------------------------------------
441000130930     C*
441100130930     C* CLIENTE (mi paga)    D/A di MOV    Segno memorizzato    Variaz. per controp.
441200130930     C*  Fattura             Dare                +                 (DEBITO)
441300130930     C*  Nota di credito     Avere               -                  CREDITO
441400130930     C*  Nota di debito      Dare                +                  DEBITO
441500130930     C*
441600130930     C*
441700130930     C* FORNITORE (lo pago)  D/A di MOV    Segno memorizzato    Variaz. per controp.
441800130930     C*  Fattura             Avere               +                 (CREDITO)
441900130930     C*  Nota di credito     Dare                -                  DEBITO
442000130930     C*  Nota di debito      Avere               +                  CREDITO
442100130930     C*
442200130930     C*
442300130930     C*---------------------------------------------------------------------------
442400111021     C* rendo negativo l'importo se il segno non è quello
442500111021     C* "naturale" per la sottonatura del conto, e cioè:
442600111021     C*
442700111021     C* Sottonatura   Segno di NDIVA00F   Segno calcolato dell'importo
442800130930     C*      F       D (segno "naturale")     ==>    +
442900130930     C*      F       A (segno nota di accred) ==>    -
443000130930     C*      C       A (segno "naturale")     ==>    +
443100130930     C*      C       D (segno nota di accred) ==>    -
443200111021     C*
443300111021     C                   if        (MOVSNatura = 'F' and
443400130930     C                              IVAAvere   = 1        )
443500111021     C                             or
443600111021     C                             (MOVSNatura = 'C' and
443700130930     C                              IVADare    = 1        )
443800111021     C*
443900111021     C     IVAImporto    MULT      -1            IVAImporto
444000111021     C     IVAImponib    MULT      -1            IVAImponib
444100111021     C                   endif
444200110826     C*
444300110826     C*
444400111003     C* in caso di autofattura, giro il segno rispetto alla logica
444500111003     C* generale
444600131206R141 X****               if        $TpAnn      = '01'        or
444700150420R1786X*ex R141           if        $Autofatt   = *On         or
444800150420R1786C                   if        ($Autofatt   = *On        or
444900111003     C                             OPETpDocI   = '8'         or
445000111003     C                             (MOVSNatura  = 'F'   and
445100111003     C                              IVATpReg    = '2'   and
445200111003     C                              DLITerrito  = '1'       )
445300150420R1786C                             )
445400150420 "   C                             and
445500150420R1786C                             $AutoCons  <> *On
445600111003     C*
445700111003     C     IVAImporto    MULT      -1            IVAImporto
445800111003     C     IVAImponib    MULT      -1            IVAImponib
445900111003     C*
446000111003     C                   endif
446100000000     C*
446200130918     C* operazione senza fattura
446300130930     C* N.B. da estrazione standard dovrebbero essere estratte solo
446400130930     C* le operazioni con fattura
446500130930     C                   if        SCFTpDoc    = 'S'
446600130918     C                   Eval      SCFImpOpeO  = IVAImponib + IVAImporto
446700130918     C                   Eval      SCFIVAOpeO  = 0
446800130918     C                   else
446900130918     C* operazione con fattura (fattura, nota, doc.di riepilogo)
447000130918     C                   Eval      SCFImpOpeO  = IVAImponib
447100130918     C                   Eval      SCFIVAOpeO  = IVAImporto
447200130918     C                   endif
447300110722     C*
447400130820     C                   Eval      SCFImpOpe   = SCFImpOpeO
447500130820     C                   Eval      SCFIVAOpe   = SCFIVAOpeO
447600110829     C*
447700130918     C                   Eval      SCFImpTot   = 0
447800130918     C                   Eval      SCFIVAtot   = 0
447900130823     C*
448000130823     C                   Eval      SCFIMPSOGL  = 0
448100130823     C                   Eval      SCFIVASOGL  = 0
448200000000     C*
448300000000     C                   ENDSR
448400000000     C/EJECT
448500110902     C************************************************************
448600130822     C* Reperisco il segno di variazione della nota
448700110902     C************************************************************
448800130822     C     RepVarNota    BEGSR
448900110902     C*
449000130826     C                   Eval      SCFVARNOTA  = *Blank
449100130826     C* se nota definisco il segno della variazione
449200130826     C                   if        SCFTpDoc    = 'N'
449300130826     C*
449400160105     C                   if        IVAAvere    = 1
449500130826     C* Variazione a Debito
449600130826     C                   Eval      SCFVARNOTA  = 'D'
449700130826     C                   else
449800130826     C* Variazione a Credito
449900130826     C                   Eval      SCFVARNOTA  = 'C'
450000130826     C                   endif
450100130826     C*
450200130826     C                   endif
450300130826     C*
450400130826     C                   ENDSR
450500130826     C/EJECT
450600130826     C************************************************************
450700130826     C* Definizione dati per Quadri
450800130826     C************************************************************
450900130826     C     DefQuadri     BEGSR
451000130826     C*
451100130826     C                   Eval      SCFFormato  = FORMATOA08
451200130826     C*
451300130826     C                   Select
451400130826     C*
451500130826     C*--------------------
451600130826     C* Formato AGGREGATO
451700151201R195 X****               When      FormatoA08  = 'A'
451800151201 "   X*ex R133                     and
451900151201R195 X*ex R133                     TpComG52    = 'E'
452000151201R195 C*
452100151201 "   C                   When      SCFFormato   = 'A'
452200151201R195 C*
452300130826     C                   Select
452400130826     C*-------
452500130826     C* se conto Nazionale e documento Fattura, Nota o Riepilogo
452600131206R141 X****               When      $UICKsc     = $UICSoc       and
452700131206 "   X****                         (SCFTpDoc    = 'F'       or
452800131206 "   X****                          SCFTpDoc    = 'N'       or
452900131206R141 X****                          SCFTpDoc    = 'R'         )
453000131206R141 C*
453100131206 "   C* in caso di Autofattura, deve finire in FA perchè andrà
453200131206 "   C* girata sull'azienda comunicante
453300131206 "   C                   When      ($UICKsc     = $UICSoc       and
453400131206 "   C                              (SCFTpDoc    = 'F'       or
453500131206 "   C                               SCFTpDoc    = 'N'       or
453600131206 "   C                               SCFTpDoc    = 'R'         )   ) or
453700131206R141 C                             $Autofatt   = *On
453800130826     C                   Eval      SCFQuadro   = 'FA'
453900130826     C                   Eval      SCFSeqQua   =  01
454000130826     C*
454100130904     C*
454200130826     C                   Eval      SCFCOGRAGR  = *Blank
454300130826     C                   Eval      SCFNOMER    = *Blank
454400130826     C                   Eval      SCFDTNASR   = *Loval
454500130826     C                   Eval      SCFLOCNASR  = *Blank
454600130826     C                   Eval      SCFPRVNASR  = *Blank
454700130826     C                   Eval      SCFSTATOR   = *Blank
454800130918     C                   Eval      SCFCODUICR  = 0
454900130826     C                   Eval      SCFLOCALIR  = *Blank
455000130826     C                   Eval      SCFINDIRER  = *Blank
455100130923     C                   Eval      SCFTpNR     = *Blank
455200130826     C                   Eval      SCFPIVAR    = SCFPartIVA
455300130826     C                   Eval      SCFCDFISCR  = SCFCdFisc
455400130925     C                   Eval      SCFTpOpeR   = *Blank
455500130925     C                   Eval      SCFNatOpeR  = *Blank
455600130923     C                   Eval      SCFLeasinR  = SCFLeasing
455700131111R140 C                   if        SCFLeasinR  = '0'
455800131111 "   C                   Eval      SCFLeasinR  = *Blank
455900131111R140 C                   endif
456000130826     C                   Eval      SCFDTREGR   = *Loval
456100130826     C                   Eval      SCFNRREGR   = *Zero
456200130826     C                   Eval      SCFSERREGR  = *Blank
456300131015R135 C* in caso di Doc.Riepilogo verifico l'effettiva modalità di
456400131015 "   C* aggregazione impostata sul parametro modulo
456500131015 "   C                   if        SCFTpDoc    = 'R'       and
456600160104 "   C                             DocRieA08   = 'D'
456700160104 "   C                   Eval      SCFDTREGR   = SCFDTREG
456800131015 "   C                   Eval      SCFNRREGR   = SCFNRREG
456900131015 "   C                   Eval      SCFSERREGR  = SCFSERREG
457000131015R135 C                   endif
457100130826     C*
457200130826     C*-------
457300130826     C* se documento Senza Fattura
457400130826     C                   When      SCFTpDoc    = 'S'
457500131017R136 C* controllo se documento legato al turismo (ovvero,
457600131017 "   C* codice fiscale non impostato)
457700131017R136 C                   if        SCFCdFisc  <> *Blank
457800130826     C                   Eval      SCFQuadro   = 'SA'
457900130826     C                   Eval      SCFSeqQua   = 02
458000130826     C*
458100130826     C                   Eval      SCFCOGRAGR  = *Blank
458200130826     C                   Eval      SCFNOMER    = *Blank
458300130826     C                   Eval      SCFDTNASR   = *Loval
458400130826     C                   Eval      SCFLOCNASR  = *Blank
458500130826     C                   Eval      SCFPRVNASR  = *Blank
458600130826     C                   Eval      SCFSTATOR   = *Blank
458700130918     C                   Eval      SCFCODUICR  = 0
458800130826     C                   Eval      SCFLOCALIR  = *Blank
458900130826     C                   Eval      SCFINDIRER  = *Blank
459000130923     C                   Eval      SCFTpNR     = *Blank
459100130826     C                   Eval      SCFPIVAR    = *Blank
459200130826     C                   Eval      SCFCDFISCR  = SCFCdFisc
459300130925     C                   Eval      SCFTpOpeR   = *Blank
459400130923     C                   Eval      SCFNatOpeR  = *Blank
459500130923     C                   Eval      SCFLeasinR  = SCFLeasing
459600131111R140 C                   if        SCFLeasinR  = '0'
459700131111 "   C                   Eval      SCFLeasinR  = *Blank
459800131111R140 C                   endif
459900130826     C                   Eval      SCFDTREGR   = *Loval
460000130826     C                   Eval      SCFNRREGR   = *Zero
460100130826     C                   Eval      SCFSERREGR  = *Blank
460200131017R136 C*
460300131017 "   C* si tratta di operazioni legate al Turismo
460400131017 "   C                   else
460500131017 "   C                   Eval      SCFQuadro   = 'TU'
460600131017 "   C                   Eval      SCFSeqQua   = 11
460700131017 "   C*
460800131017 "   C                   Eval      SCFCOGRAGR  = SCFCOGRAG
460900131017 "   C                   if        SCFTpN      = '1'
461000131017 "   C                   Eval      SCFNOMER    = SCFNOME
461100131017 "   C                   Eval      SCFDTNASR   = SCFDTNASC
461200131017 "   C                   Eval      SCFLOCNASR  = SCFLOCNASC
461300131017 "   C                   Eval      SCFPRVNASR  = SCFPRVNASC
461400131017 "   C                   Eval      SCFSTATOR   = SCFSTATO
461500131017 "   C                   Eval      SCFCODUICR  = SCFCODUIC
461600131017 "   C                   Eval      SCFLOCALIR  = SCFLOCALIT
461700131017 "   C                   Eval      SCFINDIRER  = SCFINDIRE
461800131017 "   C                   else
461900131017 "   C                   endif
462000131017 "   C                   Eval      SCFTpNR     = SCFTPN
462100131017 "   C                   Eval      SCFPIVAR    = *Blank
462200131017 "   C                   Eval      SCFCDFISCR  = *Blank
462300131017 "   C                   Eval      SCFTpOpeR   = *Blank
462400131017 "   C                   Eval      SCFNatOpeR  = *Blank
462500131017 "   C                   Eval      SCFLeasinR  = *Blank
462600131017 "   C                   Eval      SCFDTREGR   = SCFDTREG
462700131017 "   C                   Eval      SCFNRREGR   = SCFNRREG
462800131017 "   C                   Eval      SCFSERREGR  = SCFSERREG
462900131017R136 C                   endif
463000130826     C*
463100130826     C                   other
463200130826     C*-------
463300130826     C* se conto non residente
463400130826     C                   Eval      SCFQuadro   = 'BL'
463500130826     C                   Eval      SCFSeqQua   = 03
463600130826     C*
463700130826     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
463800131001     C                   if        SCFTpN      = '1'
463900130826     C                   Eval      SCFNOMER    = SCFNOME
464000130826     C                   Eval      SCFDTNASR   = SCFDTNASC
464100130826     C                   Eval      SCFLOCNASR  = SCFLOCNASC
464200130826     C                   Eval      SCFPRVNASR  = SCFPRVNASC
464300130826     C                   Eval      SCFSTATOR   = SCFSTATO
464400130826     C                   Eval      SCFCODUICR  = SCFCODUIC
464500131001     C                   else
464600131001     C                   Eval      SCFSTATOR   = SCFSTATO
464700131001     C                   Eval      SCFCODUICR  = SCFCODUIC
464800130826     C                   Eval      SCFLOCALIR  = SCFLOCALIT
464900130826     C                   Eval      SCFINDIRER  = SCFINDIRE
465000131001     C                   endif
465100130923     C                   Eval      SCFTpNR     = SCFTpN
465200130826     C                   Eval      SCFPIVAR    = SCFPartIVA
465300130826     C                   Eval      SCFCDFISCR  = SCFCdFisc
465400130925     C                   if        SCFTpOpe    = 'P'        and
465500130925     C                             SCFNatOpe   = 'S'
465600130925     C                   Eval      SCFTpOpeR   = 'P'
465700130925     C                   Eval      SCFNatOpeR  = 'S'
465800130925     C                   else
465900130925     C                   Eval      SCFTpOpeR   = *Blank
466000130925     C                   Eval      SCFNatOpeR  = *Blank
466100130925     C                   endif
466200130923     C                   Eval      SCFLeasinR  = *Blank
466300130826     C                   Eval      SCFDTREGR   = *Loval
466400130826     C                   Eval      SCFNRREGR   = *Zero
466500130826     C                   Eval      SCFSERREGR  = *Blank
466600130826     C*
466700130826     C                   endSl
466800130826     C*
466900130826     C*
467000130826     C*--------------------
467100130826     C* Formato DETTAGLIO
467200151201R195 X****               When      FormatoA08  = 'D'
467300151201 "   X*ex R133                     or
467400151201R195 X*ex R133                     TpComG52    = 'S'
467500151201R195 C*
467600151201 "   C                   When      SCFFormato   = 'D'
467700151201R195 C*
467800130826     C                   Select
467900130826     C*-------
468000131206     C* se conto Nazionale e documento Fattura o Riepilogo
468100131206R141 X****               When      $UICKsc     = $UICSoc       and
468200131206 "   X****                         (SCFTpDoc    = 'F'       or
468300131206R141 X****                          SCFTpDoc    = 'R'         )
468400131206R141 C*
468500131206 "   C* in caso di Autofattura, deve finire in FE perchè andrà
468600131206 "   C* girata sull'azienda comunicante
468700131206 "   C                   When      ($UICKsc     = $UICSoc       and
468800131206 "   C                              (SCFTpDoc    = 'F'       or
468900131206 "   C                               SCFTpDoc    = 'R'         )   ) or
469000131206 "   C                             ($Autofatt   = *On           and
469100131206 "   C                              (SCFTpDoc    = 'F'       or
469200131206R141 C                               SCFTpDoc    = 'R'         )   )
469300130826     C* se documenti Emessi
469400130826     C                   if        SCFTpOpe     = 'A'
469500130826     C                   Eval      SCFQuadro   = 'FE'
469600130826     C                   Eval      SCFSeqQua   = 04
469700130826     C*
469800130826     C                   Eval      SCFCOGRAGR  = *Blank
469900130826     C                   Eval      SCFNOMER    = *Blank
470000130826     C                   Eval      SCFDTNASR   = *Loval
470100130826     C                   Eval      SCFLOCNASR  = *Blank
470200130826     C                   Eval      SCFPRVNASR  = *Blank
470300130826     C                   Eval      SCFSTATOR   = *Blank
470400130918     C                   Eval      SCFCODUICR  = 0
470500130826     C                   Eval      SCFLOCALIR  = *Blank
470600130826     C                   Eval      SCFINDIRER  = *Blank
470700130923     C                   Eval      SCFTpNR     = *Blank
470800130826     C                   Eval      SCFPIVAR    = SCFPartIVA
470900130826     C                   Eval      SCFCDFISCR  = SCFCdFisc
471000130925     C                   Eval      SCFTpOpeR   = *Blank
471100130923     C                   Eval      SCFNatOpeR  = *Blank
471200130923     C                   Eval      SCFLeasinR  = SCFLeasing
471300131111R140 C                   if        SCFLeasinR  = '0'
471400131111 "   C                   Eval      SCFLeasinR  = *Blank
471500131111R140 C                   endif
471600130826     C                   Eval      SCFDTREGR   = SCFDTREG
471700130826     C                   Eval      SCFNRREGR   = SCFNRREG
471800130826     C                   Eval      SCFSERREGR  = SCFSERREG
471900130826     C*
472000130826     C                   else
472100130826     C*
472200130826     C* se documenti Ricevuti
472300130826     C                   Eval      SCFQuadro   = 'FR'
472400130826     C                   Eval      SCFSeqQua   = 05
472500130826     C*
472600130826     C                   Eval      SCFCOGRAGR  = *Blank
472700130826     C                   Eval      SCFNOMER    = *Blank
472800130826     C                   Eval      SCFDTNASR   = *Loval
472900130826     C                   Eval      SCFLOCNASR  = *Blank
473000130826     C                   Eval      SCFPRVNASR  = *Blank
473100130826     C                   Eval      SCFSTATOR   = *Blank
473200130918     C                   Eval      SCFCODUICR  = 0
473300130826     C                   Eval      SCFLOCALIR  = *Blank
473400130923     C                   Eval      SCFINDIRER  = *Blank
473500130923     C                   Eval      SCFTpNR     = *Blank
473600130826     C                   Eval      SCFPIVAR    = SCFPartIVA
473700130826     C                   Eval      SCFCDFISCR  = *Blank
473800130925     C                   Eval      SCFTpOpeR   = *Blank
473900130923     C                   Eval      SCFNatOpeR  = *Blank
474000130923     C                   Eval      SCFLeasinR  = *Blank
474100130826     C                   Eval      SCFDTREGR   = SCFDTREG
474200130826     C                   Eval      SCFNRREGR   = SCFNRREG
474300130826     C                   Eval      SCFSERREGR  = SCFSERREG
474400130826     C*
474500130826     C                   endif
474600130826     C*
474700130826     C*-------
474800131206     C* se conto Nazionale e documento Nota
474900131206R141 X****               When      $UICKsc     = $UICSoc       and
475000131206R141 X****                         SCFTpDoc    = 'N'
475100131206R141 C*
475200131206 "   C* in caso di Autofattura, deve finire in NE perchè andrà
475300131206 "   C* girata sull'azienda comunicante
475400131206 "   C                   When      ($UICKsc     = $UICSoc  and
475500131206 "   C                              SCFTpDoc    = 'N'         ) or
475600131206 "   C                             ($Autofatt   = *On      and
475700131206R141 C                              SCFTpDoc    = 'N'         )
475800130826     C* se documenti Emessi
475900130826     C                   if        SCFTpOpe     = 'A'
476000130826     C                   Eval      SCFQuadro   = 'NE'
476100130826     C                   Eval      SCFSeqQua   = 06
476200130826     C*
476300130826     C                   Eval      SCFCOGRAGR  = *Blank
476400130826     C                   Eval      SCFNOMER    = *Blank
476500130826     C                   Eval      SCFDTNASR   = *Loval
476600130826     C                   Eval      SCFLOCNASR  = *Blank
476700130826     C                   Eval      SCFPRVNASR  = *Blank
476800130826     C                   Eval      SCFSTATOR   = *Blank
476900130918     C                   Eval      SCFCODUICR  = 0
477000130826     C                   Eval      SCFLOCALIR  = *Blank
477100130826     C                   Eval      SCFINDIRER  = *Blank
477200130923     C                   Eval      SCFTpNR     = *Blank
477300130826     C                   Eval      SCFPIVAR    = SCFPartIVA
477400130826     C                   Eval      SCFCDFISCR  = SCFCdFisc
477500130925     C                   Eval      SCFTpOpeR   = *Blank
477600130923     C                   Eval      SCFNatOpeR  = *Blank
477700130923     C                   Eval      SCFLeasinR  = *Blank
477800130826     C                   Eval      SCFDTREGR   = SCFDTREG
477900130826     C                   Eval      SCFNRREGR   = SCFNRREG
478000130826     C                   Eval      SCFSERREGR  = SCFSERREG
478100130826     C*
478200130826     C                   else
478300130826     C*
478400130826     C* se documenti Ricevuti
478500130826     C                   Eval      SCFQuadro   = 'NR'
478600130826     C                   Eval      SCFSeqQua   = 07
478700130826     C*
478800130826     C                   Eval      SCFCOGRAGR  = *Blank
478900130826     C                   Eval      SCFNOMER    = *Blank
479000130826     C                   Eval      SCFDTNASR   = *Loval
479100130826     C                   Eval      SCFLOCNASR  = *Blank
479200130826     C                   Eval      SCFPRVNASR  = *Blank
479300130826     C                   Eval      SCFSTATOR   = *Blank
479400130918     C                   Eval      SCFCODUICR  = 0
479500130826     C                   Eval      SCFLOCALIR  = *Blank
479600130826     C                   Eval      SCFINDIRER  = *Blank
479700130923     C                   Eval      SCFTpNR     = *Blank
479800130826     C                   Eval      SCFPIVAR    = SCFPartIVA
479900130826     C                   Eval      SCFCDFISCR  = *Blank
480000130925     C                   Eval      SCFTpOpeR   = *Blank
480100130923     C                   Eval      SCFNatOpeR  = *Blank
480200130923     C                   Eval      SCFLeasinR  = *Blank
480300130826     C                   Eval      SCFDTREGR   = SCFDTREG
480400130826     C                   Eval      SCFNRREGR   = SCFNRREG
480500130826     C                   Eval      SCFSERREGR  = SCFSERREG
480600130826     C*
480700130826     C                   endif
480800130826     C*
480900130826     C*-------
481000130826     C* se documento Senza Fattura
481100130826     C                   When      SCFTpDoc    = 'S'
481200131017R136 C* controllo se documento legato al turismo (ovvero,
481300131017 "   C* codice fiscale non impostato)
481400131017R136 C                   if        SCFCdFisc  <> *Blank
481500130826     C                   Eval      SCFQuadro   = 'DF'
481600130826     C                   Eval      SCFSeqQua   = 08
481700130826     C*
481800130826     C                   Eval      SCFCOGRAGR  = *Blank
481900130826     C                   Eval      SCFNOMER    = *Blank
482000130826     C                   Eval      SCFDTNASR   = *Loval
482100130826     C                   Eval      SCFLOCNASR  = *Blank
482200130826     C                   Eval      SCFPRVNASR  = *Blank
482300130826     C                   Eval      SCFSTATOR   = *Blank
482400130918     C                   Eval      SCFCODUICR  = 0
482500130826     C                   Eval      SCFLOCALIR  = *Blank
482600130826     C                   Eval      SCFINDIRER  = *Blank
482700130923     C                   Eval      SCFTpNR     = *Blank
482800130826     C                   Eval      SCFPIVAR    = *Blank
482900130826     C                   Eval      SCFCDFISCR  = SCFCdFisc
483000130925     C                   Eval      SCFTpOpeR   = *Blank
483100130923     C                   Eval      SCFNatOpeR  = *Blank
483200130923     C                   Eval      SCFLeasinR  = SCFLeasing
483300131111R140 C                   if        SCFLeasinR  = '0'
483400131111 "   C                   Eval      SCFLeasinR  = *Blank
483500131111R140 C                   endif
483600130826     C                   Eval      SCFDTREGR   = SCFDTREG
483700130826     C                   Eval      SCFNRREGR   = SCFNRREG
483800130826     C                   Eval      SCFSERREGR  = SCFSERREG
483900131017R136 C*
484000131017 "   C* si tratta di operazioni legate al Turismo
484100131017 "   C                   else
484200131017 "   C                   Eval      SCFQuadro   = 'TU'
484300131017 "   C                   Eval      SCFSeqQua   = 11
484400131017 "   C*
484500131017 "   C                   Eval      SCFCOGRAGR  = SCFCOGRAG
484600131017 "   C                   if        SCFTpN      = '1'
484700131017 "   C                   Eval      SCFNOMER    = SCFNOME
484800131017 "   C                   Eval      SCFDTNASR   = SCFDTNASC
484900131017 "   C                   Eval      SCFLOCNASR  = SCFLOCNASC
485000131017 "   C                   Eval      SCFPRVNASR  = SCFPRVNASC
485100131017 "   C                   Eval      SCFSTATOR   = SCFSTATO
485200131017 "   C                   Eval      SCFCODUICR  = SCFCODUIC
485300131017 "   C                   Eval      SCFLOCALIR  = SCFLOCALIT
485400131017 "   C                   Eval      SCFINDIRER  = SCFINDIRE
485500131017 "   C                   else
485600131017 "   C                   endif
485700131017 "   C                   Eval      SCFTpNR     = SCFTPN
485800131017 "   C                   Eval      SCFPIVAR    = *Blank
485900131017 "   C                   Eval      SCFCDFISCR  = *Blank
486000131017 "   C                   Eval      SCFTpOpeR   = *Blank
486100131017 "   C                   Eval      SCFNatOpeR  = *Blank
486200131017 "   C                   Eval      SCFLeasinR  = *Blank
486300131017 "   C                   Eval      SCFDTREGR   = SCFDTREG
486400131017 "   C                   Eval      SCFNRREGR   = SCFNRREG
486500131017 "   C                   Eval      SCFSERREGR  = SCFSERREG
486600131017R136 C                   endif
486700130826     C*
486800130826     C*
486900130826     C                   other
487000130826     C*-------
487100130826     C* se conto non residente
487200131015R135 X**** e Acquisti di Servizi
487300131015 "   X****               if        SCFTpOpe    = 'P'    and
487400131015R135 X****                         SCFNatOpe   = 'S'
487500131015R135 C* e Acquisti di Beni o di Servizi
487600131015R135 C                   if        SCFTpOpe    = 'P'
487700160104R195 X*ex R133                     or
487800160104 "   X*ex R133 Acquisti da San Marino
487900160104R195 X*ex R133                     TpComG52    = 'S'
488000130826     C                   Eval      SCFQuadro   = 'SE'
488100130826     C                   Eval      SCFSeqQua   = 10
488200130826     C*
488300130826     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
488400131001     C                   if        SCFTpN      = '1'
488500131001     C                   Eval      SCFNOMER    = SCFNOME
488600131001     C                   Eval      SCFDTNASR   = SCFDTNASC
488700131001     C                   Eval      SCFLOCNASR  = SCFLOCNASC
488800131001     C                   Eval      SCFPRVNASR  = SCFPRVNASC
488900131001     C                   Eval      SCFSTATOR   = SCFSTATO
489000131001     C                   Eval      SCFCODUICR  = SCFCODUIC
489100131001     C                   else
489200131001     C                   Eval      SCFSTATOR   = SCFSTATO
489300131001     C                   Eval      SCFCODUICR  = SCFCODUIC
489400131001     C                   Eval      SCFLOCALIR  = SCFLOCALIT
489500131001     C                   Eval      SCFINDIRER  = SCFINDIRE
489600131001     C                   endif
489700130923     C                   Eval      SCFTpNR     = SCFTPN
489800130826     C                   Eval      SCFPIVAR    = SCFPartIVA
489900130826     C                   Eval      SCFCDFISCR  = SCFCdFisc
490000160201R195 X****               Eval      SCFTpOpeR   = *Blank
490100160201R195 C                   Eval      SCFTpOpeR   = SCFTpOpe
490200130923     C                   Eval      SCFNatOpeR  = *Blank
490300130923     C                   Eval      SCFLeasinR  = *Blank
490400130826     C                   Eval      SCFDTREGR   = SCFDTREG
490500130826     C                   Eval      SCFNRREGR   = SCFNRREG
490600130826     C                   Eval      SCFSERREGR  = SCFSERREG
490700130826     C*
490800130826     C                   else
490900130826     C*
491000130826     C* se conto non residente
491100131015R135 X**** e Vendite o Acquisti di Beni
491200131015R135 C* e Vendite di Beni o Servizi
491300130826     C                   Eval      SCFQuadro   = 'FN'
491400130826     C                   Eval      SCFSeqQua   = 09
491500130826     C*
491600130826     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
491700131001     C                   if        SCFTpN      = '1'
491800131001     C                   Eval      SCFNOMER    = SCFNOME
491900131001     C                   Eval      SCFDTNASR   = SCFDTNASC
492000131001     C                   Eval      SCFLOCNASR  = SCFLOCNASC
492100131001     C                   Eval      SCFPRVNASR  = SCFPRVNASC
492200131001     C                   Eval      SCFSTATOR   = SCFSTATO
492300131001     C                   Eval      SCFCODUICR  = SCFCODUIC
492400131001     C                   else
492500131001     C                   Eval      SCFSTATOR   = SCFSTATO
492600131001     C                   Eval      SCFCODUICR  = SCFCODUIC
492700131001     C                   Eval      SCFLOCALIR  = SCFLOCALIT
492800131001     C                   Eval      SCFINDIRER  = SCFINDIRE
492900131001     C                   endif
493000130923     C                   Eval      SCFTpNR     = SCFTPN
493100130826     C                   Eval      SCFPIVAR    = *Blank
493200130826     C                   Eval      SCFCDFISCR  = *Blank
493300130925     C                   Eval      SCFTpOpeR   = *Blank
493400130923     C                   Eval      SCFNatOpeR  = *Blank
493500130923     C                   Eval      SCFLeasinR  = SCFLeasing
493600131111R140 C                   if        SCFLeasinR  = '0'
493700131111 "   C                   Eval      SCFLeasinR  = *Blank
493800131111R140 C                   endif
493900130826     C                   Eval      SCFDTREGR   = SCFDTREG
494000130826     C                   Eval      SCFNRREGR   = SCFNRREG
494100130826     C                   Eval      SCFSERREGR  = SCFSERREG
494200130826     C*
494300130826     C                   endif
494400130826     C*
494500130826     C                   endSl
494600130826     C*
494700130826     C                   endSl
494800130822     C*
494900130822     C                   ENDSR
495000130822     C/EJECT
495100160104R195 C************************************************************
495200160104 "   C* Definizione dati per Quadro SE per Aquisti da San Marino
495300160104 "   C************************************************************
495400160104R195 C     DefQuadroSE   BEGSR
495500160104     C*
495600160104     C* Formato DETTAGLIO
495700160104     C                   Eval      SCFFormato  = 'D'
495800160104     C*
495900160104     C                   Eval      SCFQuadro   = 'SE'
496000160104     C                   Eval      SCFSeqQua   = 10
496100160104     C*
496200160104     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
496300160104     C                   if        SCFTpN      = '1'
496400160104     C                   Eval      SCFNOMER    = SCFNOME
496500160104     C                   Eval      SCFDTNASR   = SCFDTNASC
496600160104     C                   Eval      SCFLOCNASR  = SCFLOCNASC
496700160104     C                   Eval      SCFPRVNASR  = SCFPRVNASC
496800160104     C                   Eval      SCFSTATOR   = SCFSTATO
496900160104     C                   Eval      SCFCODUICR  = SCFCODUIC
497000160104     C                   else
497100160104     C                   Eval      SCFSTATOR   = SCFSTATO
497200160104     C                   Eval      SCFCODUICR  = SCFCODUIC
497300160104     C                   Eval      SCFLOCALIR  = SCFLOCALIT
497400160104     C                   Eval      SCFINDIRER  = SCFINDIRE
497500160104     C                   endif
497600160104     C                   Eval      SCFTpNR     = SCFTPN
497700160104     C                   Eval      SCFPIVAR    = SCFPartIVA
497800160104     C                   Eval      SCFCDFISCR  = SCFCdFisc
497900160201     C                   Eval      SCFTpOpeR   = SCFTpOpe
498000160104     C                   Eval      SCFNatOpeR  = *Blank
498100160104     C                   Eval      SCFLeasinR  = *Blank
498200160104     C                   Eval      SCFDTREGR   = SCFDTREG
498300160104     C                   Eval      SCFNRREGR   = SCFNRREG
498400160104     C                   Eval      SCFSERREGR  = SCFSERREG
498500160104     C*
498600160104R195 C                   ENDSR
498700160104R195 C/EJECT
498800160104R195 C************************************************************
498900160104 "   C* Definizione dati per Quadro BL in comunicaz. Black List
499000160104 "   C************************************************************
499100160104R195 C     DefQuadroBL   BEGSR
499200160104     C*
499300160104     C                   Eval      SCFFormato  = 'A'
499400160104     C                   Eval      SCFQuadro   = 'BL'
499500160104     C                   Eval      SCFSeqQua   = 03
499600160104     C*
499700160104     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
499800160104     C                   if        SCFTpN      = '1'
499900160104     C                   Eval      SCFNOMER    = SCFNOME
500000160104     C                   Eval      SCFDTNASR   = SCFDTNASC
500100160104     C                   Eval      SCFLOCNASR  = SCFLOCNASC
500200160104     C                   Eval      SCFPRVNASR  = SCFPRVNASC
500300160104     C                   Eval      SCFSTATOR   = SCFSTATO
500400160104     C                   Eval      SCFCODUICR  = SCFCODUIC
500500160104     C                   else
500600160104     C                   Eval      SCFSTATOR   = SCFSTATO
500700160104     C                   Eval      SCFCODUICR  = SCFCODUIC
500800160104     C                   Eval      SCFLOCALIR  = SCFLOCALIT
500900160104     C                   Eval      SCFINDIRER  = SCFINDIRE
501000160104     C                   endif
501100160104     C                   Eval      SCFTpNR     = SCFTpN
501200160104     C                   Eval      SCFPIVAR    = SCFPartIVA
501300160104     C                   Eval      SCFCDFISCR  = SCFCdFisc
501400160104     C                   Eval      SCFTpOpeR   = *Blank
501500160104     C                   Eval      SCFNatOpeR  = *Blank
501600160104     C                   Eval      SCFLeasinR  = *Blank
501700160104     C                   Eval      SCFDTREGR   = *Loval
501800160104     C                   Eval      SCFNRREGR   = *Zero
501900160104     C                   Eval      SCFSERREGR  = *Blank
502000160104     C*
502100160104R195 C                   ENDSR
502200160104R195 C/EJECT
502300130822     C************************************************************
502400130926     C* Reperiti dati anagrafici a parità di partita IVA/cd fiscale
502500130822     C************************************************************
502600130926     C     RepAnagraf    BEGSR
502700130822     C*
502800130926     C                   if        SCFQuadro   = 'FA'     or
502900130926     C                             SCFQuadro   = 'SA'     or
503000130926     C                             SCFQuadro   = 'FE'     or
503100130926     C                             SCFQuadro   = 'FR'     or
503200130926     C                             SCFQuadro   = 'NE'     or
503300130926     C                             SCFQuadro   = 'NR'     or
503400130926     C                             SCFQuadro   = 'DF'
503500130926     C*
503600130926     C                   Clear                   SDGSG53DS
503700130926     C*
503800130926     C                   Eval      SOCIETAG53  = XScSoc
503900130926     C                   Eval      TPCOMG53    = SCFTpCom
504000130926     C                   Eval      ANNOG53     = SCFAnno
504100130926     C                   Eval      MESEG53     = SCFMese
504200130926     C                   Eval      PartIVAG53  = SCFPartIVA
504300130926     C                   Eval      CdFiscG53   = SCFCdFisc
504400130926     C*
504500130926     C                   CALL      'SDGSG53R'
504600130926     C                   PARM                    KPJBA
504700130926     C                   PARM                    SDGSG53DS
504800130926     C*
504900130926     C                   if        FoundG53    = *On
505000130926     C*
505100130926     C                   Eval      SCFCOGRAG   = COGRAGG53
505200130926     C                   Eval      SCFNOME     = NOMEG53
505300130926     C                   Eval      SCFDTNASC   = DTNASCG53
505400130926     C                   Eval      SCFLOCNASC  = LOCNASCG53
505500130926     C                   Eval      SCFPRVNASC  = PRVNASCG53
505600130926     C                   Eval      SCFSTATO    = STATOG53
505700130926     C                   Eval      SCFCODUIC   = CODUICG53
505800130926     C                   Eval      SCFLOCALIT  = LOCALITG53
505900130926     C                   Eval      SCFINDIRE   = INDIREG53
506000130926     C*
506100130926     C                   endif
506200130926     C                   endif
506201160405R2216C*
506202160405 "   C* riapplico i filtri anagrafici sui dati "drogati"
506203160405 "   C                   Select
506204160405 "   C*
506205160405 "   C                   When      SCFStato   <> StatoG52   and
506206160405 "   C                             StatoG52   <> *Blank
506207160405 "   C                   Eval      $WriteSCF   = *Off
506208160405 "   C*
506209160405R2216C                   endSl
506300130926     C*
506400130926     C                   ENDSR
506500130926     C/EJECT
506600151123R195 C************************************************************
506700151123 "   C* Controllo soglia su operazioni Black List
506800151123 "   C************************************************************
506900151123R195 C     CtrSogliaBl   BEGSR
507000151123     C*
507100151123     C                   Clear                   SDG00508DS
507200151123     C*
507300151203     C                   Eval      Lr0508      = *OFF
507400151123     C*
507500151123     C                   Eval      Soc0508     = SocietaG52
507600151123     C                   Eval      Ctb0508     = CtbG52
507700151123     C                   Eval      TpRegz0508  = TpRegzG52
507800151123     C                   Eval      TpCom0508   = 'B'
507900151123     C                   Eval      Anno0508    = AnnoG52
508000151123     C                   Eval      Mese0508    = 0
508100160201     C                   Eval      CogRaR0508  = SavCogRagR
508200160201     C                   Eval      NomeR0508   = SavNomeR
508300160201     C                   Eval      DtNasR0508  = SavDtNasR
508400160201     C                   Eval      LocNaR0508  = SavLocNasR
508500160201     C                   Eval      PrvNaR0508  = SavPrvNasR
508600160201     C                   Eval      StatoR0508  = SavStatoR
508700160201     C                   Eval      CodUIR0508  = SavCodUICR
508800160201     C                   Eval      LocalR0508  = SavLocaliR
508900160201     C                   Eval      IndiER0508  = SavIndireR
509000160201     C                   Eval      TpNR0508    = SavTpNR
509100160201     C                   Eval      PIVAR0508   = SavPIVAR
509200160201     C                   Eval      CdFisR0508  = SavCdFiscR
509300160201     C                   Eval      TpOpeR0508  = SavTpOpeR
509400160201     C                   Eval      NatOpR0508  = SavNatOpeR
509500160201     C                   Eval      LeasiR0508  = SavLeasinR
509600160201     C                   Eval      DtRegR0508  = SavDtRegR
509700160201     C                   Eval      NrRegR0508  = SavNrRegR
509800160201     C                   Eval      SerReR0508  = SavSerRegR
509900151203     C* passo anche i dati di filtro per la stampa
510000160201     C                   Eval      Stato0508   = StatoG52
510100151202     C                   Eval      Sogg0508    = SoggG52
510200151202     C                   Eval      Kcc10508    = Kcc1G52
510300151202     C                   Eval      Kcc20508    = Kcc2G52
510400151202     C                   Eval      Kcc30508    = Kcc3G52
510500151202     C                   Eval      Kcc40508    = Kcc4G52
510600151202     C                   Eval      Kcc50508    = Kcc5G52
510700151202     C                   Eval      Kcc60508    = Kcc6G52
510800151202     C                   Eval      Kcc0508     = KccG52
510900151202     C                   Eval      Ksc0508     = KscG52
511000151123     C                   Eval      DelMan0508  = DelManuG52
511100151123     C                   Eval      StpAnn0508  = StpAnnG52
511200151123     C*
511300151123     C* salvo la kpjbu per ripristinarla dopo il richiamo allo u.e.
511400151123     C                   MoveL     KPJBU         SavKPJBU
511500151123     C*
511600151123     C*
511700151202     C                   CALLB     'SDG00508R'
511800151202     C                   PARM                    KPJBA
511900151203     C                   PARM                    SDG00508DS
512000151123     C*
512100151123R195 C                   ENDSR
512200151123R195 C/EJECT
512300130926     C************************************************************
512400130926     C* Verifica dei default beni/servizi da attributo contabile
512500130926     C************************************************************
512600130926     C     ChkBenServAttrBEGSR
512700130926     C*
512800110902     C* l'attributo contabile può contenere la forzatura per i
512900110902     C* Clienti/Fornitori che operano prevalentemente con
513000110902     C* con beni o servizi
513100151201R195 C*
513200151201 "   C* se Spesometro o Acquisti da San Marino
513300151201 "   C                   if        TpComG52        = 'E'    or
513400151201 "   C                             TpComG52        = 'S'
513500151201R195 C*
513600110902     C                   Select
513700110902     C*
513800130821     C                   When      PosAttA08        > 0            and
513900130821     C                             SkAttr(PosAttA08) = BeniA08
514000110902     C                   Eval      $OkBeni         = *On
514100110902     C                   Eval      $OkServizi      = *Off
514200110902     C                   EXSR      AssVal_BS
514300110902     C*
514400130821     C                   When      PosAttA08        > 0            and
514500130823     C                             SkAttr(PosAttA08)= ServiziA08
514600110902     C                   Eval      $OkBeni         = *Off
514700110902     C                   Eval      $OkServizi      = *On
514800110902     C                   EXSR      AssVal_BS
514900110902     C*
515000110902     C                   EndSl
515100151201R195 C*
515200151201 "   C                   endif
515300151201 "   C*
515400151201 "   C*
515500151201 "   C* se Black List
515600151201 "   C                   if        TpComG52        = 'B'
515700151201 "   C*
515800151201 "   C                   Select
515900151201 "   C*
516000151201 "   C                   When      BLPosA11         > 0            and
516100151201 "   C                             SkAttr(BLPosA11) = BLBeniA11
516200151201 "   C                   Eval      $OkBeni         = *On
516300151201 "   C                   Eval      $OkServizi      = *Off
516400151201 "   C                   EXSR      AssVal_BS
516500151201 "   C*
516600151201 "   C                   When      BLPosA11         > 0            and
516700151201 "   C                             SkAttr(BLPosA11) = BLServA11
516800151201 "   C                   Eval      $OkBeni         = *Off
516900151201 "   C                   Eval      $OkServizi      = *On
517000151201 "   C                   EXSR      AssVal_BS
517100151201 "   C*
517200151201 "   C                   EndSl
517300151201 "   C*
517400151201R195 C                   endif
517500110902     C*
517600110902     C                   ENDSR
517700110902     C/EJECT
517800110902     C************************************************************
517900110902     C* Assegna valore tipo operazione
518000110902     C************************************************************
518100110902     C     AssVal_BS     BEGSR
518200110902     C*
518300110902     C                   Select
518400110902     C*
518500110902     C                   When      $OkBeni         = *On  and
518600110902     C                             $OkServizi     <> *On
518700130823     C                   Eval      $NatOpe         = 'B'
518800110902     C*
518900110902     C                   When      $OkBeni        <> *On  and
519000110902     C                             $OkServizi      = *On
519100130823     C                   Eval      $NatOpe         = 'S'
519200110902     C*
519300110902     C                   EndSl
519400110902     C*
519500110902     C                   ENDSR
519600110902     C/EJECT
519700000000     C************************************************************
519800000000     C* Analisi della contropartita (costo/ricavo) per definire
519900000000     C* se si tratta di un bene o di un servizio
520000000000     C************************************************************
520100000000     C     ChkBeniServiziBEGSR
520200000000     C*
520300000000     C* reperisco le informazioni sulle righe di contropartita
520400151222     C                   EXSR      RepContrp
520500000000     C*
520600000000     C* gestisco solo i casi fattibili, ovvero quelli in cui
520700000000     C* è presente una sola tipologia operazione
520800000000     C* 1 : 1   una tipologia iva e una tipologia operazione
520900000000     C* N : 1   n  tipologie iva e una tipologia operazione
521000000000     C*
521100000000     C* casistiche non gestite
521200000000     C* 1 : N   una tipologia iva e 2 tipologie operazione
521300000000     C* N : N   più tipologie iva e 2 tipologie operazione
521400000000     C*
521500000000     C*
521600000000     C                   Select
521700000000     C*
521800000000     C*  se presente una sola tipologia di operazione, imposto
521900000000     C*  quella tipologia su tutte le righe
522000000000     C                   When      $OkBeni         = *On  and
522100000000     C                             $OkServizi     <> *On
522200130821     C                   Eval      $NatOpe         = 'B'
522300000000     C*
522400000000     C                   When      $OkBeni        <> *On  and
522500000000     C                             $OkServizi      = *On
522600130821     C                   Eval      $NatOpe         = 'S'
522700000000     C*
522800000000     C                   When      ($OkBeni    = *On   and  $OkServizi = *On)
522900160310R138 C*
523000131025 "   C* annullo solo se fornitore estero dove è importante stabilire se beni o
523100160310R138 C* servizi
523200160310R195 X*ex R138           if        $UICKsc    <> $UICSoc      and
523300160310 "   X*ex R138                     SNaturaSav  = 'F'
523400160310R195 X*ex R138
523401160310R195 C*
523402160310 "   C* in caso di Black List, è importante sempre sapere la natura dell'oeprazione
523403160310 "   C                   if        TpComG52    = 'B'               or
523404160310 "   C
523405160310 "   C                             ($UICKsc    <> $UICSoc  and
523406160310 "   C                              SNaturaSav  = 'F'         )
523407160310R195 C*
523500000000     C* annullo la riga in elaborazione  e la registrazione
523600000000     C* in modo da annullare anche tutte le altre
523700000000     C                   Eval      $AnnIVA     = *On
523800000000     C                   Eval      $AnnMOV     = *On
523900000000     C                   if        $TpAnn      = *Blank
524000000000     C*
524100000000     C* salvo il movimento di contropartita
524200000000     C                   Move      NDMOV00F      NDMOVSav2
524300000000     C* ripristino il movimento cliente/fornitore per la stampa errore
524400000000     C                   Move      NDMOVSav      NDMOV00F
524500000000     C*
524600000000     C                   Eval      $TpAnn      = '20'
524700160418R221AX****               EXSR      StpErr1
524800000000     C*
524900000000     C* ripristino il movimento di contropartita
525000000000     C                   Move      NDMOVSav2     NDMOV00F
525100000000     C*
525200000000     C                   endif
525300131025R138 C*
525400131025R138 C                   endif
525500000000     C*
525600000000     C                   endSl
525700000000     C*
525800000000     C                   ENDSR
525900000000     C/EJECT
526000000000     C************************************************************
526100000000     C* Reperimento tipologia di contropartita
526200000000     C************************************************************
526300000000     C     RepContrp     BEGSR
526400000000     C*
526500000000     C                   Eval      $OkBeni     = *Off
526600000000     C                   Eval      $OkServizi  = *Off
526700000000     C*
526800000000     C* Lettura dei movimenti di contropartita legati alla registrazione
526900000000     C                   Eval      $CodOper    = '1'
527000000000     C                   EXSR      GetMOVCP
527100000000     C*
527200000000     C                   DoW       $CodOper   <> '9'
527300000000     C*
527400000000     C                   EXSR      SelContrP
527500000000     C*
527600000000     C                   if        $OkContrP   = *On
527700000000     C* verifico di quale caso di contropartita si tratta
527800000000     C                   EXSR      ChkContrP
527900000000     C                   endif
528000000000     C*
528100000000     C                   Eval      $CodOper    = '2'
528200000000     C                   EXSR      GetMOVCP
528300000000     C                   ENDDO
528400000000     C*
528500000000     C                   ENDSR
528600000000      /EJECT
528700000000     C************************************************************
528800000000     c* Reperisce NDMOV di contropartita
528900000000     C************************************************************
529000000000     C     GetMOVCP      BEGSR
529100000000     C*
529200000000     C                   Eval      $LenOut     = %size(NDMOV00F)
529300000000     C                   CALLB     'NDDRVMOV'
529400000000     C                   PARM                    IVASys
529500000000     C                   PARM                    IVANrAsReg
529600000000     C                   PARM                    *Omit
529700000000     C                   PARM      'NDMOV     '  $Struttura
529800000000     C                   PARM                    $LenOut
529900000000     C                   PARM                    $CodOper
530000000000     C                   PARM                    NDMOV00F
530100000000     C*
530200000000     C                   ENDSR
530300000000     C/EJECT
530400000000     C****************************************************************
530500000000     C* Selezione dei movimenti di controparita
530600000000     C****************************************************************
530700000000     C     SelContrP     BEGSR
530800000000     C*
530900000000     C                   Eval      $OkContrP   = *On
531000000000     C*
531100000000     C                   Select
531200000000     C* seleziono solo i cespiti ammortizzabili e gli altri conti
531300000000     C* (costi e ricavi)
531400000000     C                   When      Not(MOVSNatura  = '4'  or
531500000000     C                                 MOVSNatura  = '9'    )
531600000000     C                   Eval      $OkContrP   = *Off
531700000000     C*
531800150420R1785X****               When      MOVImporto  = 0
531900150420R1785X****               Eval      $OkContrP   = *Off
532000000000     C                   endSl
532100000000     C*
532200000000     C* ---------------
532300151130R195 X**** 1° tentativo con KCC + KSC
532400000000     C                   if        $OkContrP   = *On
532500151130R195 C*
532600151130 "   C* verifico se conto di controparita è da escludere nella determinazione
532700151130 "   C* della tipologia di operazione ( sconti, spese, commissioni)
532800151130 "   C*
532900151130 "   C* recupero l'eventuale voce di analitica per verificare
533000151130 "   C* se il movimento deve essere escluso tramite la voce
533100151130 "   C                   Eval      $Voce       = *Blank
533200151130 "   C*
533300151130 "   C* Scorro le righe di analitica della prima dimensione
533400151130 "   C* in ordine alfabetico. E' sufficiente scorrere le righe di
533500151130 "   C* una sola dimensione, in quanto se presenti anche più voci
533600151130 "   C* legate allo stesso conto, la definizione di beni/servizi
533700151130 "   C* o l'esclusione sarebbe logicamente la stessa in quanto
533800151130 "   C* le voci sono legate allo stesso conto
533900151130 "   C                   Eval      MOASys     = MOVSys
534000151130 "   C                   Eval      MOANrAsReg = MOVNrAsReg
534100151130 "   C                   Eval      MOANrRigaM = MOVNrRigaM
534200151130 "   C     K03MOA01      Setll     NDMOA01L
534300151130 "   C     K03MOA01      READE     NDMOA01L                               21
534400160321 "   C                   Eval      $FineMOA   = *In21
534500160321 "   C*
534600151130 "   C                   Eval      SPRSocieta  = XScSoc
534700151130 "   C* se è Black List
534800151130 "   C                   if        TpComG52    = 'B'
534900151130 "   C                   Eval      SPRTipReg   = '5'
535000151130 "   C                   else
535100151130 "   C* se è Spesometro o Acquisti San Marino
535200151130 "   C                   Eval      SPRTipReg   = '3'
535300151130 "   C                   endif
535400151130 "   C                   Eval      SPRSTpReg   = '2'
535500151130 "   C                   Eval      SPRTPREG    = *Blank
535600151130 "   C                   Eval      SPRLIBRO    = *Blank
535700151130 "   C                   Eval      SPRALIQ     = 0
535800151130 "   C                   Eval      SPRRIFIVA   = *Blank
535900151130 "   C                   Eval      SPRCAUSALE  = *Blank
536000151130 "   C                   Eval      SPRClasse   = *Blank
536100151130 "   C*
536200151130 "   C* la prima volta entra comunque, anche se non ha trovato
536300151130 "   C* il record di NDMOA, esce dal ciclo quando ha finito di
536400151130 "   C* leggere tutte le righe della prima dimensione
536500151130 "   C                   DoU       $FineMOA   = *On
536600151130 "   C*
536700151130 "   C                   if        $OkContrP   = *On
536800151130 "   C* 1° tentativo con KCC + KSC + VOCE
536900151130 "   C                   Eval      SPRKCC      = MOVKcc
537000160321R195 C                   Eval      SPRKSC      = MOVKsc
537100160321R2193X*ex R195           Eval      SPRVoce     = $Voce
537101160321R2193C                   Eval      SPRVoce     = MOAVoce
537200160321R195 C     K12SPR04      CHAIN     SDGSPR04L                          21
537300151130 "   C                   if        *In21       = *Off
537400151130 "   C* se trovo il record significa che il conto è escluso
537500151130 "   C                   Eval      $OkContrP   = *Off
537600151130 "   C                   Eval      $FineMOA   = *On
537700151130 "   C                   endif
537800151130 "   C*
537900151130 "   C                   endif
538000151130 "   C*
538100151130 "   C* ---------------
538200151130 "   C*
538300151130 "   C                   if        $OkContrP   = *On
538400151130R195 C* 2° tentativo con KCC + KSC
538500000000     C* verifico se conto di controparita è da escludere nella determinazione
538600000000     C* della tipologia di operazione ( sconti, spese, commissioni)
538700151130R195 X****               Eval      SPRSocieta  = XScSoc
538800151130 "   X****               Eval      SPRTipReg   = '3'
538900151130 "   X****               Eval      SPRSTpReg   = '2'
539000151130 "   X****               Eval      SPRTPREG    = *Blank
539100151130 "   X****               Eval      SPRLIBRO    = *Blank
539200151130 "   X****               Eval      SPRALIQ     = 0
539300151130 "   X****               Eval      SPRRIFIVA   = *Blank
539400151130R195 X****               Eval      SPRCAUSALE  = *Blank
539500130823     C                   Eval      SPRKCC      = MOVKcc
539600130823     C                   Eval      SPRKSC      = MOVKsc
539700130823     C                   Eval      SPRVoce     = *Blank
539800151130R195 X****               Eval      SPRClasse   = *Blank
539900130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
540000000000     C                   if        *In21       = *Off
540100000000     C* se trovo il record significa che il conto è escluso
540200000000     C                   Eval      $OkContrP   = *Off
540300151130R195 C                   Eval      $FineMOA   = *On
540400000000     C                   endif
540500000000     C*
540600000000     C                   endif
540700151130R195 C*
540800151130 "   C* ---------------
540900151130 "   C*
541000151130 "   C                   if        $OkContrP   = *On
541100151130 "   C* 3° tentativo con KCC + VOCE
541200151130 "   C* verifico se conto di controparita è da escludere nella determinazione
541300151130 "   C* della tipologia di operazione ( sconti, spese, commissioni)
541400151130 "   C                   Eval      SPRKCC      = MOVKcc
541500151130 "   C                   Eval      SPRKSC      = *Blank
541600151130 "   C                   Eval      SPRVoce     = MOAVoce
541700151130 "   C     K12SPR04      CHAIN     SDGSPR04L                          21
541800151130 "   C                   if        *In21       = *Off
541900151130 "   C* se trovo il record significa che il conto è escluso
542000151130 "   C                   Eval      $OkContrP   = *Off
542100151130 "   C                   Eval      $FineMOA   = *On
542200151130 "   C                   endif
542300151130 "   C*
542400151130R195 C                   endif
542500000000     C*
542600110830     C* ---------------
542700151130R195 X**** 2° tentativo solo con KCC                                      azione
542800151130R195 C* 4° tentativo solo con KCC
542900110830     C                   if        $OkContrP   = *On
543000110830     C* verifico se conto di controparita è da escludere nella determinazione
543100110830     C* della tipologia di operazione ( sconti, spese, commissioni)
543200151130R195 X****               Eval      SPRSocieta  = XScSoc
543300151130 "   X****               Eval      SPRTipReg   = '3'
543400151130 "   X****               Eval      SPRSTpReg   = '2'
543500151130 "   X****               Eval      SPRTPREG    = *Blank
543600151130 "   X****               Eval      SPRLIBRO    = *Blank
543700151130 "   X****               Eval      SPRALIQ     = 0
543800151130 "   X****               Eval      SPRRIFIVA   = *Blank
543900151130R195 X****               Eval      SPRCAUSALE  = *Blank
544000130823     C                   Eval      SPRKCC      = MOVKcc
544100130823     C                   Eval      SPRKSC      = *Blank
544200130823     C                   Eval      SPRVoce     = *Blank
544300151130R195 X****               Eval      SPRClasse   = *Blank
544400130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
544500110830     C                   if        *In21       = *Off
544600110830     C* se trovo il record significa che il conto è escluso
544700110830     C                   Eval      $OkContrP   = *Off
544800151130R195 C                   Eval      $FineMOA   = *On
544900110830     C                   endif
545000110830     C*
545100110830     C                   endif
545101160321R2193C*
545102160321 "   C* ---------------
545104160321 "   C* 5° tentativo solo con VOCE
545105160321 "   C                   if        $OkContrP   = *On
545106160321 "   C* verifico se conto di controparita è da escludere nella determinazione
545107160321 "   C* della tipologia di operazione ( sconti, spese, commissioni)
545116160321 "   C                   Eval      SPRKCC      = *Blank
545117160321 "   C                   Eval      SPRKSC      = *Blank
545118160321 "   C                   Eval      SPRVoce     = MOAVoce
545120160321 "   C     K12SPR04      CHAIN     SDGSPR04L                          21
545121160321 "   C                   if        *In21       = *Off
545122160321 "   C* se trovo il record significa che il conto è escluso
545123160321 "   C                   Eval      $OkContrP   = *Off
545124160321 "   C                   Eval      $FineMOA   = *On
545125160321 "   C                   endif
545126160321 "   C*
545127160321 "   C                   endif
545128160321 "   C*
545129160321R2193C                   if        $FineMOA    = *Off
545200151130R195 C*
545300151130 "   C* leggo solo i movimenti della prima dimensione su cui
545400151130 "   C* mi sono posizionata (vd. VL K04MOA01)
545500151130 "   C     K04MOA01      READE     NDMOA01L                               21
545600160321R195 C                   Eval      $FineMOA   = *In21
545601160321R2193C                   endif
545700160321R195 C*
545800151130 "   C                   EndDo
545900151130 "   C
546000151130R195 C                   endif
546100110830     C*
546200000000     C                   ENDSR
546300000000      /EJECT
546400000000     C****************************************************************
546500000000     C* Verifico di quale caso di contropartita si tratta
546600000000     C****************************************************************
546700000000     C     ChkContrP     BEGSR
546800000000     C*
546900000000     C* reperisco la voce sul movimento di analitica
547000000000     C                   EXSR      RepMOAVoce
547100000000     C*
547200000000     C* verifico se si tratta di operazione relativa
547300000000     C* a cessione di beni
547400130823     C                   Eval      SPRTipReg   = '2'
547500130823     C                   Eval      SPRSTpReg   = '1'
547600130924     C                   EXSR      ChkRegBenSrv
547700000000     C*
547800130924     C                   if        $OkRegBenSrv<> *On
547900000000     C* verifico se si tratta di operazione relativa
548000000000     C* a prestazione di servizi
548100130823     C                   Eval      SPRTipReg   = '2'
548200130823     C                   Eval      SPRSTpReg   = '2'
548300130924     C                   EXSR      ChkRegBenSrv
548400000000     C                   endif
548500000000     C*
548600000000     C*
548700130924     C                   if        $OkRegBenSrv= *On
548800000000     C*
548900130823     C                   if        SPRSTpReg   = '1'
549000000000     C*
549100000000     C                   Eval      $OkBeni     = *On
549200000000     C*
549300000000     C                   endif
549400000000     C*
549500000000     C*
549600130823     C                   if        SPRSTpReg   = '2'
549700000000     C*
549800000000     C                   Eval      $OkServizi  = *On
549900000000     C*
550000000000     C                   endif
550100000000     C*
550200131018R136 C                   else
550300151201R195 C*
550400151201 "   C* se Spesometro o Acquisti da San Marino
550500151201 "   C                   if        TpComG52        = 'E'    or
550600151201 "   C                             TpComG52        = 'S'
550700151201R195 C*
550800151201R136 C* se non trovata la regola, imposto il default
550900131018 "   C                   if        BeniSrvA08  = 'B'
551000131018 "   C                   Eval      $OkBeni     = *On
551100131018 "   C                   else
551200131018 "   C                   Eval      $OkServizi  = *On
551300131018R136 C                   endif
551400151201R195 C*
551500151201 "   C                   endif
551600151201 "   C*
551700151201 "   C*
551800151201 "   C* se Black List
551900151201 "   C                   if        TpComG52        = 'B'
552000151201 "   C* se non trovata la regola, imposto il default
552100151201 "   C                   if        BeniSrvA11  = 'B'
552200151201 "   C                   Eval      $OkBeni     = *On
552300151201 "   C                   else
552400151201 "   C                   Eval      $OkServizi  = *On
552500151201 "   C                   endif
552600151201 "   C                   endif
552700151201R195 C*
552800000000     C                   endif
552900000000     C*
553000000000     C                   ENDSR
553100000000      /EJECT
553200000000     C************************************************************
553300000000     C* Reperisco l'eventuale voce di analitica
553400000000     C************************************************************
553500000000     C     RepMOAVoce    BEGSR
553600000000     C*
553700000000     C                   Eval      $OkVoce     = *On
553800000000     C                   Eval      SavVoce     = *Blank
553900000000     C*
554000000000     C                   Eval      MOASys      = MOVSys
554100000000     C                   Eval      MOANrAsReg  = MOVNrAsReg
554200000000     C                   Eval      MOANrRigaM  = MOVNrRigaM
554300000000     C     K03MOA01      SETLL     NDMOA01L
554400000000     C     K03MOA01      READE     NDMOA01L                               21
554500000000     C                   Move      *In21         $FineMOA
554600000000     C                   if        $FineMOA    = *Off
554700000000     C                   Eval      SavVoce     = MOAVoce
554800000000     C                   endif
554900000000     C*
555000000000     C                   DoW       $FineMOA    = *Off
555100000000     C*
555200000000     C                   if        SavVoce    <> MOAVoce
555300000000     C                   Eval      $OkVoce     = *Off
555400000000     C                   endif
555500000000     C*
555600151130R195 X**** K03MOA01      READE     NDMOA01L                               21
555700151130R195 C* leggo solo i movimenti della prima dimensione su cui
555800151130 "   C* mi sono posizionata (vd. VL K04MOA01)
555900151130 "   C* In questo modo non scarto la voce se ne trovo una
556000151130 "   C* diversa su un'altra dimensione, in quanto in quel caso
556100151130 "   C* potrebbe avere la stessa natura e sarebbe sbagliato
556200151130 "   C* scartarla
556300151130R195 C     K04MOA01      READE     NDMOA01L                               21
556400000000     C                   Move      *In21         $FineMOA
556500000000     C                   endDo
556600000000     C*
556700000000     C                   if        SavVoce     = *Blank
556800000000     C                   Eval      $OkVoce     = *Off
556900000000     C                   endif
557000000000     C*
557100000000     C                   ENDSR
557200000000      /EJECT
557300000000     C************************************************************
557400000000     C* Verifico se si tratta di cessione di beni
557500000000     C* o di prestazione di servizi
557600000000     C************************************************************
557700130924     C     ChkRegBenSrv  BEGSR
557800000000     C*
557900000000     C* si assume che per definire la natura un costo/ricavo
558000000000     C* siano da considerare in ordine di importanza:
558100000000     C* 1) il conto di costo/ricavo
558200000000     C* 2) la causale
558300000000     C* 3) la voce
558400130924     C                   Eval      $OkRegBenSrv = *Off
558500000000     C*
558600130823     C                   Eval      SPRSocieta  = XScSoc
558700000000     C*
558800000000     C*------------------------------
558900000000     C* 1° tentativo con chiave completa:
559000000000     C*    causale + conto ctp + voce
559100000000     C                   if        $OkVoce     = *On
559200130823     C                   Eval      SPRTPREG    = *Blank
559300130823     C                   Eval      SPRLIBRO    = *Blank
559400130823     C                   Eval      SPRALIQ     = 0
559500130823     C                   Eval      SPRRIFIVA   = *Blank
559600130823     C                   Eval      SPRCAUSALE  = MOVCausTes
559700130823     C                   Eval      SPRKCC      = MOVKcc
559800130823     C                   Eval      SPRKSC      = MOVKsc
559900130823     C                   Eval      SPRVOCE     = MOAVoce
560000130823     C                   Eval      SPRCLASSE   = *Blank
560100130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
560200000000     C                   if        *In21       = *Off
560300130924     C                   Eval      $OkRegBenSrv = *On
560400000000     C                   endif
560500000000     C                   endif
560600000000     C*
560700110830     C*------------------------------
560800110830     C* 1°bis tentativo con chiave completa:
560900110830     C*    causale + conto ctp (solo KCC) + voce
561000110830     C                   if        $OkVoce     = *On
561100130823     C                   Eval      SPRTPREG    = *Blank
561200130823     C                   Eval      SPRLIBRO    = *Blank
561300130823     C                   Eval      SPRALIQ     = 0
561400130823     C                   Eval      SPRRIFIVA   = *Blank
561500130823     C                   Eval      SPRCAUSALE  = MOVCausTes
561600130823     C                   Eval      SPRKCC      = MOVKcc
561700130823     C                   Eval      SPRKSC      = *Blank
561800130823     C                   Eval      SPRVOCE     = MOAVoce
561900130823     C                   Eval      SPRCLASSE   = *Blank
562000130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
562100110830     C                   if        *In21       = *Off
562200130924     C                   Eval      $OkRegBenSrv = *On
562300110830     C                   endif
562400110830     C                   endif
562500110830     C*
562600000000     C*------------------------------
562700000000     C* 2° tentativo: chiave senza voce:
562800000000     C*    conto ctp + causale
562900130924     C                   if        $OkRegBenSrv = *Off
563000130823     C                   Eval      SPRTPREG    = *Blank
563100130823     C                   Eval      SPRLIBRO    = *Blank
563200130823     C                   Eval      SPRALIQ     = 0
563300130823     C                   Eval      SPRRIFIVA   = *Blank
563400130823     C                   Eval      SPRCAUSALE  = MOVCausTes
563500130823     C                   Eval      SPRKCC      = MOVKcc
563600130823     C                   Eval      SPRKSC      = MOVKsc
563700130823     C                   Eval      SPRVOCE     = *Blank
563800130823     C                   Eval      SPRCLASSE   = *Blank
563900130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
564000000000     C                   if        *In21       = *Off
564100130924     C                   Eval      $OkRegBenSrv = *On
564200000000     C                   endif
564300000000     C                   endif
564400110825     C*
564500110830     C*------------------------------
564600110830     C* 2°bis tentativo: chiave senza voce:
564700110830     C*    conto ctp (solo KCC)+ causale
564800130924     C                   if        $OkRegBenSrv = *Off
564900130823     C                   Eval      SPRTPREG    = *Blank
565000130823     C                   Eval      SPRLIBRO    = *Blank
565100130823     C                   Eval      SPRALIQ     = 0
565200130823     C                   Eval      SPRRIFIVA   = *Blank
565300130823     C                   Eval      SPRCAUSALE  = MOVCausTes
565400130823     C                   Eval      SPRKCC      = MOVKcc
565500130823     C                   Eval      SPRKSC      = *Blank
565600130823     C                   Eval      SPRVOCE     = *Blank
565700130823     C                   Eval      SPRCLASSE   = *Blank
565800130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
565900110830     C                   if        *In21       = *Off
566000130924     C                   Eval      $OkRegBenSrv = *On
566100110830     C                   endif
566200110830     C                   endif
566300110830     C*
566400110825     C*------------------------------
566500110830     C* 3° tentativo: chiave senza conto e voce:
566600110830     C*    Causale
566700130924     C                   if        $OkRegBenSrv = *Off
566800130823     C                   Eval      SPRTPREG    = *Blank
566900130823     C                   Eval      SPRLIBRO    = *Blank
567000130823     C                   Eval      SPRALIQ     = 0
567100130823     C                   Eval      SPRRIFIVA   = *Blank
567200130823     C                   Eval      SPRCAUSALE  = MOVCausTes
567300130823     C                   Eval      SPRKCC      = *Blank
567400130823     C                   Eval      SPRKSC      = *Blank
567500130823     C                   Eval      SPRVOCE     = *Blank
567600130823     C                   Eval      SPRCLASSE   = *Blank
567700130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
567800110825     C                   if        *In21       = *Off
567900130924     C                   Eval      $OkRegBenSrv = *On
568000110825     C                   endif
568100110825     C                   endif
568101160321R2194C*
568102160321 "   C*------------------------------
568103160321 "   C* 3° bis tentativo: chiave kcc + ksc + voce
568105160321 "   C                   if        $OkRegBenSrv = *Off
568106160321 "   C                   Eval      SPRTPREG    = *Blank
568107160321 "   C                   Eval      SPRLIBRO    = *Blank
568108160321 "   C                   Eval      SPRALIQ     = 0
568109160321 "   C                   Eval      SPRRIFIVA   = *Blank
568110160321 "   C                   Eval      SPRCAUSALE  = *Blank
568111160321 "   C                   Eval      SPRKCC      = MOVKcc
568112160321 "   C                   Eval      SPRKSC      = MOVKsc
568113160321 "   C                   Eval      SPRVOCE     = MOAVoce
568114160321 "   C                   Eval      SPRCLASSE   = *Blank
568115160321 "   C     K12SPR04      CHAIN     SDGSPR04L                          21
568116160321 "   C                   if        *In21       = *Off
568117160321 "   C                   Eval      $OkRegBenSrv = *On
568118160321 "   C                   endif
568119160321R2194C                   endif
568200000000     C*
568300000000     C*------------------------------
568400110830     C* 4° tentativo: chiave senza causale e voce:
568500110830     C*    conto ctp
568600130924     C                   if        $OkRegBenSrv = *Off
568700130823     C                   Eval      SPRTPREG    = *Blank
568800130823     C                   Eval      SPRLIBRO    = *Blank
568900130823     C                   Eval      SPRALIQ     = 0
569000130823     C                   Eval      SPRRIFIVA   = *Blank
569100130823     C                   Eval      SPRCAUSALE  = *Blank
569200130823     C                   Eval      SPRKCC      = MOVKcc
569300130823     C                   Eval      SPRKSC      = MOVKsc
569400130823     C                   Eval      SPRVOCE     = *Blank
569500130823     C                   Eval      SPRCLASSE   = *Blank
569600130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
569700000000     C                   if        *In21       = *Off
569800130924     C                   Eval      $OkRegBenSrv = *On
569900000000     C                   endif
570000000000     C                   endif
570001160321R2194C*
570002160321 "   C*------------------------------
570003160321 "   C* 4° bis tentativo: chiave kcc + voce
570004160321 "   C                   if        $OkRegBenSrv = *Off
570005160321 "   C                   Eval      SPRTPREG    = *Blank
570006160321 "   C                   Eval      SPRLIBRO    = *Blank
570007160321 "   C                   Eval      SPRALIQ     = 0
570008160321 "   C                   Eval      SPRRIFIVA   = *Blank
570009160321 "   C                   Eval      SPRCAUSALE  = *Blank
570010160321 "   C                   Eval      SPRKCC      = MOVKcc
570011160321 "   C                   Eval      SPRKSC      = *Blank
570012160321 "   C                   Eval      SPRVOCE     = MOAVoce
570013160321 "   C                   Eval      SPRCLASSE   = *Blank
570014160321 "   C     K12SPR04      CHAIN     SDGSPR04L                          21
570015160321 "   C                   if        *In21       = *Off
570016160321 "   C                   Eval      $OkRegBenSrv = *On
570017160321 "   C                   endif
570018160321R2194C                   endif
570100110825     C*
570200110825     C*------------------------------
570300110830     C* 4° bis tentativo: chiave senza causale e voce:
570400110830     C*    conto ctp (solo KCC)
570500130924     C                   if        $OkRegBenSrv = *Off
570600130823     C                   Eval      SPRTPREG    = *Blank
570700130823     C                   Eval      SPRLIBRO    = *Blank
570800130823     C                   Eval      SPRALIQ     = 0
570900130823     C                   Eval      SPRRIFIVA   = *Blank
571000130823     C                   Eval      SPRCAUSALE  = *Blank
571100130823     C                   Eval      SPRKCC      = MOVKcc
571200130823     C                   Eval      SPRKSC      = *Blank
571300130823     C                   Eval      SPRVOCE     = *Blank
571400130823     C                   Eval      SPRCLASSE   = *Blank
571500130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
571600110830     C                   if        *In21       = *Off
571700130924     C                   Eval      $OkRegBenSrv = *On
571800110830     C                   endif
571900110830     C                   endif
572000110830     C*
572100110830     C*------------------------------
572200110830     C* 5° tentativo: chiave senza conto e causale:
572300110830     C*    voce
572400130924     C                   if        $OkRegBenSrv = *Off
572500110825     C                   if        $OkVoce     = *On
572600130823     C                   Eval      SPRTPREG    = *Blank
572700130823     C                   Eval      SPRLIBRO    = *Blank
572800130823     C                   Eval      SPRALIQ     = 0
572900130823     C                   Eval      SPRRIFIVA   = *Blank
573000130823     C                   Eval      SPRCAUSALE  = *Blank
573100130823     C                   Eval      SPRKCC      = *Blank
573200130823     C                   Eval      SPRKSC      = *Blank
573300130823     C                   Eval      SPRVOCE     = MOAVoce
573400130823     C                   Eval      SPRCLASSE   = *Blank
573500130823     C     K12SPR04      CHAIN     SDGSPR04L                          21
573600110825     C                   if        *In21       = *Off
573700130924     C                   Eval      $OkRegBenSrv = *On
573800110825     C                   endif
573900110825     C                   endif
574000110825     C                   endif
574100000000     C*
574200000000     C                   ENDSR
574300000000     C/EJECT
574400000000     C************************************************************
574500000000     C* INIZIALIZZAZIONE VARIABILI
574600000000     C************************************************************
574700000000     C     INZVAR        BEGSR
574800000000     C*
574900130820     C                   Eval      SDGSG52DS   = KPJBU
575000131014R135 C                   Eval      $FoundMarg  = *Off
575100000000     C*
575200000000     C*-------------------
575300130820     C* cancello SDGSCF00F in base alle selezioni effettuate
575400130820     C                   EXSR      DelSDGSCF
575500000000     C*
575600000000     C                   CALLB     'XSTRCMT'
575700130826     C                   OPEN      SDGSCF03L
575800130826     C                   OPEN      SDGSCF04L
575900130826     C*
576000000000     C                   ENDSR
576100000000     C/EJECT
576200000000     C************************************************************
576300130820     C* Cancellazione  SDGSCF00F
576400000000     C************************************************************
576500130820     C     DelSDGSCF     BEGSR
576600000000     C*
576700000000     C* cancello solo i record inseriti automaticamente
576800130820     C                   EXSR      RepSQLSCF
576900000000     C*
577000130820     C                   EXSR      ExeSQLSCF
577100000000     C*
577200000000     C* Riorganizzazione membro
577300000000     C                   Clear                   CmdQcmd
577400130820     C                   Eval      CmdQcmd='RGZPFM FILE(SDGSCF00F)'
577500000000     C                   call      'QCMDEXC'                            28
577600000000     C                   PARM                    CmdQcmd         160
577700000000     C                   PARM      80            Len              15 5
577800000000     C*
577900000000     C                   ENDSR
578000000000     C/EJECT
578100000000     C************************************************************
578200130820     C* REPERIMENTO SCHIERA PER SQLSCF
578300000000     C************************************************************
578400130820     C     RepSQLSCF     BEGSR
578500000000     C*
578600130820     C                   Clear                   SqlSCF
578700000000     C*
578800130820     C* Delete from SDGSCF00F
578900130820     C                   CAT       'DELETE':1    SqlSCF
579000130820     C                   CAT       'from':1      SqlSCF
579100130820     C                   CAT       'SDGSCF00F':1 SqlSCF
579200000000     C*
579300000000     C* SELEZIONI
579400130820     C                   CAT       'WHERE':1     SqlSCF
579500130820     C                   EXSR      WhereSCF
579600000000     C*
579700000000     C                   ENDSR
579800000000     C/EJECT
579900000000     C******************************************************
580000130820     C* Selezioni su SDGSCF00F
580100000000     C******************************************************
580200130820     C     WhereSCF      BEGSR
580300000000     C*
580400130820     C* SCFSocieta = SocietaG52
580500130820     C                   CAT       'SCFSocieta':1SQLSCF
580600130820     C                   CAT       '=''':0       SQLSCF
580700130820     C                   CAT       SocietaG52:0  SQLSCF
580800130820     C                   CAT       '''':0        SQLSCF
580900000000     C*
581000000000     C* cancello solo gli automatici se è richiesto di
581100000000     C* non cancellare i manuali
581200130820     C                   if        DelManuG52 <> *On
581300130820     C* SCFOrigine = 'A'
581400130820     C                   CAT       'and':1       SQLSCF
581500130820     C                   CAT       'SCFOrigine':1SQLSCF
581600130820     C                   CAT       '=''':0       SQLSCF
581700130820     C                   CAT       'A''':0       SQLSCF
581800000000     C                   endif
581900000000     C*
582000140328R133 C                   Select
582100151123R195 C* Spesometro o Black Listi
582200140328R133 C                   When      TpComG52      = 'E'
582300151123R195 C                             OR
582400151123R195 C                             TpComG52      = 'B'
582500130820     C* and SCFAnno = AnnoG52
582600130820     C                   MoveL     AnnoG52       $Anno
582700130820     C                   CAT       'and':1       SQLSCF
582800130820     C                   CAT       'SCFAnno':1   SQLSCF
582900130820     C                   CAT       '=':0         SQLSCF
583000130820     C                   CAT       $Anno:0       SQLSCF
583100140328R133 C*
583200151123R195 C* Acquisti da San Marino
583300140328R133 C                   When      TpComG52      = 'S'
583400140328R133 C                   Exsr      RepPeriodo
583500140328R133 C                   Eval      SqlSCF      = %trimr(SqlSCF) +
583600140328R133 C                             ' AND (SCFDtReg'
583700140328R133 C*
583800140328R133 C                   Eval      SQLSCF      = %trimr(SQLSCF) +
583900140328R133 C                             ' BETWEEN ''' + $Data10A1 +
584000140328R133 C                             ''' AND ''' + $Data10A2 + ''''
584100140328R133 C                             + ' ) '
584200140328R133 C                   EndSl
584300160202R195 C*
584400160202 "   C* and SCFStato = StatoG52
584500160202 "   C                   if        StatoG52   <> *Blank
584600160202 "   C                   CAT       'and':1       SQLSCF
584700160202 "   C                   CAT       'SCFStato':1  SQLSCF
584800160202 "   C                   CAT       '=''':0       SQLSCF
584900160202 "   C                   CAT       StatoG52:0    SQLSCF
585000160202 "   C                   CAT       '''':0        SQLSCF
585100160202 "   C                   endif
585200160202R195 C*
585300130820     C* and SCFCF   = CFG52
585400160202R195 X*ex R133           if        TPCOMG52   = 'S'
585500160202 "   X*ex R133           Eval      CFG52      = 'F'
585600160202R195 X*ex R133           endif
585700140328R133 C*
585800140328     C                   if        CFG52      <> *Blank
585900130820     C                   CAT       'and':1       SQLSCF
586000130820     C                   CAT       'SCFCF':1     SQLSCF
586100130820     C                   CAT       '=''':0       SQLSCF
586200130820     C                   CAT       CFG52:0       SQLSCF
586300130820     C                   CAT       '''':0        SQLSCF
586400000000     C                   endif
586500000000     C*
586600130820     C* and SCFPartIVA = PartIVAG52
586700130820     C                   if        PartIVAG52 <> *Blank
586800130820     C                   CAT       'and':1       SQLSCF
586900130820     C                   CAT       'SCFPartIVA':1SQLSCF
587000130820     C                   CAT       '=''':0       SQLSCF
587100130820     C                   CAT       PartIVAG52:0  SQLSCF
587200130820     C                   CAT       '''':0        SQLSCF
587300000000     C                   endif
587400000000     C*
587500130820     C* and SCFCdFisc = CdFiscG52
587600130820     C                   if        CdFiscG52  <> *Blank
587700130820     C                   CAT       'and':1       SQLSCF
587800130820     C                   CAT       'SCFCdFisc':1 SQLSCF
587900130820     C                   CAT       '=''':0       SQLSCF
588000130820     C                   CAT       CdFiscG52:0   SQLSCF
588100130820     C                   CAT       '''':0        SQLSCF
588200000000     C                   endif
588300000000     C*
588400130820     C* and SCFSogg = SoggG52
588500130820     C                   if        SoggG52    <> *Blank
588600130820     C                   CAT       'and':1       SQLSCF
588700130820     C                   CAT       'SCFSogg':1   SQLSCF
588800130820     C                   CAT       '=''':0       SQLSCF
588900130820     C                   CAT       SoggG52:0     SQLSCF
589000130820     C                   CAT       '''':0        SQLSCF
589100000000     C                   endif
589200000000     C*
589300130820     C* and SCFKcc = Kcc1G52
589400130820     C                   if        Kcc1G52    <> *Blank
589500130820     C                   CAT       'and':1       SQLSCF
589600130820     C                   CAT       'SCFKcc':1    SQLSCF
589700130820     C                   CAT       '=''':0       SQLSCF
589800130820     C                   CAT       Kcc1G52:0     SQLSCF
589900130820     C                   CAT       '''':0        SQLSCF
590000000000     C                   endif
590100000000     C*
590200130820     C* and SCFKcc = Kcc2G52
590300130820     C                   if        Kcc2G52    <> *Blank
590400130820     C                   CAT       'and':1       SQLSCF
590500130820     C                   CAT       'SCFKcc':1    SQLSCF
590600130820     C                   CAT       '=''':0       SQLSCF
590700130820     C                   CAT       Kcc2G52:0     SQLSCF
590800130820     C                   CAT       '''':0        SQLSCF
590900000000     C                   endif
591000000000     C*
591100130820     C* and SCFKcc = Kcc3G52
591200130820     C                   if        Kcc3G52    <> *Blank
591300130820     C                   CAT       'and':1       SQLSCF
591400130820     C                   CAT       'SCFKcc':1    SQLSCF
591500130820     C                   CAT       '=''':0       SQLSCF
591600130820     C                   CAT       Kcc3G52:0     SQLSCF
591700130820     C                   CAT       '''':0        SQLSCF
591800000000     C                   endif
591900000000     C*
592000130820     C* and SCFKcc = Kcc4G52
592100130820     C                   if        Kcc4G52    <> *Blank
592200130820     C                   CAT       'and':1       SQLSCF
592300130820     C                   CAT       'SCFKcc':1    SQLSCF
592400130820     C                   CAT       '=''':0       SQLSCF
592500130820     C                   CAT       Kcc4G52:0     SQLSCF
592600130820     C                   CAT       '''':0        SQLSCF
592700000000     C                   endif
592800000000     C*
592900130820     C* and SCFKcc = Kcc5G52
593000130820     C                   if        Kcc5G52    <> *Blank
593100130820     C                   CAT       'and':1       SQLSCF
593200130820     C                   CAT       'SCFKcc':1    SQLSCF
593300130820     C                   CAT       '=''':0       SQLSCF
593400130820     C                   CAT       Kcc5G52:0     SQLSCF
593500130820     C                   CAT       '''':0        SQLSCF
593600000000     C                   endif
593700000000     C*
593800130820     C* and SCFKcc = Kcc6G52
593900130820     C                   if        Kcc6G52    <> *Blank
594000130820     C                   CAT       'and':1       SQLSCF
594100130820     C                   CAT       'SCFKcc':1    SQLSCF
594200130820     C                   CAT       '=''':0       SQLSCF
594300130820     C                   CAT       Kcc6G52:0     SQLSCF
594400130820     C                   CAT       '''':0        SQLSCF
594500000000     C                   endif
594600000000     C*
594700130820     C* and SCFKsc = KscG52
594800130820     C                   if        KscG52     <> *Blank
594900130820     C                   CAT       'and':1       SQLSCF
595000130820     C                   CAT       'SCFKsc':1    SQLSCF
595100130820     C                   CAT       '=''':0       SQLSCF
595200130820     C                   CAT       KscG52:0      SQLSCF
595300130820     C                   CAT       '''':0        SQLSCF
595400000000     C                   endif
595500140328     C*
595600140328R133 C                   CAT       'and':1       SQLSCF
595700140328 "   C                   CAT       'SCFTPCOM':1  SQLSCF
595800140328 "   C                   CAT       '=''':0       SQLSCF
595900140328 "   C                   CAT       TpcomG52:0    SQLSCF
596000140328 "   C                   CAT       '''':0        SQLSCF
596100140328R133 C*
596200000000     C                   ENDSR
596300000000     C/EJECT
596400000000     C******************************************************
596500130820     C* ESECUZIONE SQL per SDGSCF
596600000000     C******************************************************
596700130820     C     EXESQLSCF     BEGSR
596800000000     C*
596900000000     C/EXEC SQL
597000130820     C+     EXECUTE IMMEDIATE :SQLSCF
597100000000     C/END-EXEC
597200000000     C*
597300000000     C                   if        SQLCOD      < 0
597400000000     C                   CALL      'X66CHGJOB'
597500000000     C*
597600130820     C                   Eval      StringaSql  = SQLSCF
597700000000     C                   Eval      $TpErr1     = '99'
597800000000     C                   EXSR      StpErr1
597900130820     C                   Eval      ERRG52      = '1'
598000000000     C                   endif
598100000000     C*
598200000000     C                   ENDSR
598300000000     C/EJECT
598400000000     C********************************************************************
598500000000     C* Close Sql fetch
598600000000     C********************************************************************
598700000000     C     CloseSql      BEGSR
598800000000     C*
598900000000     C/EXEC SQL
599000000000     C+     Close A1
599100000000     C/END-EXEC
599200000000     C*
599300000000     C                   ENDSR
599400000000     C/EJECT
599500000000     C************************************************************
599600000000     C* OPERAZIONI INIZIALI
599700000000     C************************************************************
599800000000     C     *INZSR        BEGSR
599900000000     C*
600000000000     C     *ENTRY        PLIST
600100000000     C                   PARM                    KPJBA
600200000000     C*
600300000000     C     *DMY          Move      UDate         UDateISO
600400000000     C*
600500130820     C                   Eval      SDGSG52DS   = KPJBU
600600000000     C*
600700110720     C                   EXSR      REPSOC
600800110720     C*
600900110720     C* Se non è già aperto, apro il printer file e faccio OVR
601000110720     C                   EXSR      OpnPrtf
601100000000     C*
601200130918     C* Reperisco dati della società da tab. G12
601300130918     C                   EXSR      RepG12Soc
601400000000     C*
601500160104R195 C*
601600160104 "   C                   Select
601700160104 "   C* se Spesometro o Acquisti da San marino
601800160104 "   C                   When      TpComG52    = 'E'     or
601900160104R195 C                             TpComG52    = 'S'
602000130820     C                   EXSR      RepSDGSPECF
602100160104R195 C* se Black List
602200160104 "   C                   When      TpComG52    = 'B'
602300151201 "   C                   EXSR      RepSDBLACK2
602400160104 "   C*
602500160104R195 C                   endSl
602600000000     C*
602700000000     C*-------------------
602800000000     C* valorizzo i dati di testata comuni a tutte le stampe
602900000000     C                   MoveL     KNSIF         NOMSIF
603000000000     C                   MoveL     XSCDSI        NOMDIT
603100000000     C                   MoveL     DSPGM         NOMPGM
603200000000     C                   CALL      'XNETA'                              29
603300000000     C                   PARM                    NOMSYS
603400151202R195 C* Repperisco il sottotitolo di testata
603500151202  "  C                   Exsr      RepSottTitt
603600151202R195 C*
603700000000     C*
603800000000     C                   EXSR      RieParam
603900000000     C*
604000000000     C                   EXSR      StpErr1_Inz
604001160607R224 C*
604002160607R224 C                   EXSR      RepDatiAna
604100000000     C*
604200000000     C                   ENDSR
604300000000     C/EJECT
604400110720     C************************************************************
604500110720     C* Apertura file si stampa
604600110720     C************************************************************
604700110720     C     OpnPrtf       BEGSR
604800110720     C*
604900110720     C* imposto il nome del pgm da mettere in stampa
605000130826     C                   MOVEL     'SDG00500R'   NOMPGM
605100110720     C*-------------------
605200110720     C* valorizzo i dati di testata comuni a tutte le stampe
605300110720     C                   MoveL     KNSIF         NOMSIF
605400110720     C                   MoveL     XSCDSI        NOMDIT
605500110720     C**************     MoveL     DSPGM         NOMPGM
605600110720     C                   CALL      'XNETA'                              29
605700110720     C                   PARM                    NOMSYS
605800110720     C*
605900110720     C                   EXSR      RieParam
606000110720     C*
606100130820     C                   if        NOT %open(SDGSG52P1)
606200110721     C*
606300110721     C* salvo la kpjbu per ripristinarla dopo il richiamo allo u.e.
606400110721     C                   MoveL     KPJBU         SavKPJBU
606500110720     C*
606600110720     C* eseguo ovrprtf in *Share
606700130826     C                   Clear                   SDG00500DS
606800130826     C                   Eval      KPJBU       = SDG00500DS
606900110720     C*
607000130826     C                   CallB     'SDG00500R'
607100110720     C                   PARM                    KPJBA
607200110720     C*
607300130826     C                   MoveL     KPJBU         SDG00500DS
607400110720     C*
607500130826     C                   if        Esito0500   = *On
607600110720     C*
607700110720     C* Fallito l'ovrprtf del file di spool
607800110720     C* apro il prtf per fare la stampa dell'errore
607900130820     C                   OPEN      SDGSG52P1
608000110720     C*
608100110720     C                   else
608200130820     C                   OPEN      SDGSG52P1
608300110720     C                   endif
608400110721     C*
608500110721     C* ripristino la kpjbu per non "turbare" i richiami successivi
608600110721     C* ad eventuali altri pgm che ricevono la kpjba
608700110721     C                   MoveL     SavKPJBU      KPJBU
608800110720     C*
608900110720     C                   endif
609000110720     C*
609100110720     C                   EndSr
609200110720     C/EJECT
609300000000     C************************************************************
609400000000     C* Reperimento dati società
609500000000     C************************************************************
609600000000     C     REPSOC        BEGSR
609700000000     C*
609800000000     C                   CALL      'SDGSOCDR'
609900000000     C                   PARM      'SOC001'      TIPXSC            6
610000130820     C                   PARM      SocietaG52    SOCXSC            3
610100000000     C                   PARM                    CDSXSC            9 0
610200000000     C                   PARM                    MODXSC            3
610300000000     C                   PARM      *BLANKS       RTNXSC            1
610400000000     C                   PARM                    XSOCDS
610500000000     C                   PARM                    KPJBA
610600000000     C*
610700000000     C                   if        RtnXsc     <> '1'
610800000000     C                   MoveL     XSOCDS        SOC001
610900000000     C                   else
611000000000     C                   Eval      $TpErr1     = '02'
611100110720     C***************    EXSR      StpErr1
611200130820     C                   Eval      ERRG52      = '1'
611300000000     C                   EXSR      ENDPGM
611400000000     C                   endif
611500000000     C*
611600000000     C                   ENDSR
611700000000     C/EJECT
611800000000     C************************************************************
611900130918     C* Reperisco dati da G12 della società
612000000000     C************************************************************
612100130918     C     RepG12Soc     BEGSR
612200000000     C*
612300130918     C                   Eval      $UICSoc     = 0
612400130918     C                   Eval      $NazSoc     = *Blank
612500000000     C*
612600130918     C                   Eval      WStato      = XScSta
612700130918     C                   EXSR      RepTabG12
612800130918     C*
612900000000     C                   if        XTBErr     <> '0'
613000000000     C                   Eval      $TpErr1     = '01'
613100000000     C                   EXSR      StpErr1
613200130820     C                   Eval      ERRG52      = '1'
613300000000     C                   EXSR      ENDPGM
613400000000     C                   endif
613500130918     C*
613600130918     C                   if        $CodUIC     = 0
613700130918     C                   Eval      $TpErr1     = '05'
613800130918     C                   EXSR      StpErr1
613900130918     C                   Eval      ERRG52      = '1'
614000130918     C                   EXSR      ENDPGM
614100130918     C                   endif
614200130918     C*
614300130918     C                   Eval      $UICSoc     = $CodUIC
614400130918     C*
614500130918     C                   Eval      $NazSoc     = $NazISO
614600130918     C                   if        $NazSoc     = *Blank
614700130918     C                   Eval      $NazSoc     = XScSta
614800130918     C                   endif
614900130918     C*
615000000000     C                   ENDSR
615100000000      /EJECT
615200000000     C************************************************************
615300000000     C* Reperisco dati tabella G12
615400000000     C************************************************************
615500130918     C     RepTabG12     BEGSR
615600000000     C*
615700130918     C                   Eval      $CodUIC     = 0
615800130918     C                   Eval      $NazISO     = *Blank
615900130918     C*
616000000000     C                   Clear                   ANGG12DS
616100000000     C                   Clear                   SDGTABDS
616200000000     C*
616300000000     C                   Eval      XTbRic      = '1'
616400000000     C                   Eval      XTbAzi      = XScSoc
616500000000     C                   Eval      XTbKey      = *Zero
616600000000     C                   Eval      XTbCod      = 'G12'
616700130918     C                   Move      WStato        XTbKey
616800000000     C*
616900000000     C                   CALL      'SDGTABDR'
617000000000     C                   Parm                    SDGTABDS
617100000000     C*
617200000000     C                   if        XTBErr      = '0'
617300000000     C                   Eval      ANGG12DS    = XTbUni
617400130918     C*
617500130918     C                   if        %SubSt(ANGG12DS:50:3)   = *Blank
617600130918     C                   Z-Add     0             $CodUIC
617700130918     C                   else
617800130918     C                   Z-Add     CodUICG12     $CodUIC
617900130918     C                   endif
618000130918     C*
618100130918     C                   Eval      $NazISO     = NazG12
618200000000     C*
618300000000     C                   endif
618400000000     C*
618500000000     C                   ENDSR
618600000000      /EJECT
618700000000     C************************************************************
618800130820     C* Reperimento parametri di setup per Spesometro cli/for
618900000000     C************************************************************
619000130820     C     RepSDGSPECF   BEGSR
619100000000     C*
619200130821     C                   Clear                   SDGPA08DS
619300000000     C*
619400000000     C                   CALLB     'XPAR'
619500000000     C                   Parm                    XScSoc
619600130821     C                   Parm      'SDGSPECF'    XPaPar            8
619700000000     C                   Parm                    XPaOut         2000
619800000000     C                   Parm      *OFF          XPaErr            1
619900000000     C                   Parm      ' '           XPaRic            1
620000000000     C*
620100000000     C                   if        XPaErr      = *On
620200000000     C                   Eval      $TpErr1     = '03'
620300000000     C                   EXSR      StpErr1
620400130820     C                   Eval      ERRG52      = '1'
620500000000     C                   EXSR      ENDPGM
620600000000     C*
620700000000     C                   else
620800130821     C                   MoveL     XPaOut        SDGPA08DS
620900000000     C*
621000130821     C                   if        AttSCFA08   <> *On
621100000000     C                   Eval      $TpErr1     = '04'
621200000000     C                   EXSR      StpErr1
621300130820     C                   Eval      ERRG52      = '1'
621400000000     C                   EXSR      ENDPGM
621500000000     C*
621600000000     C                   endif
621700000000     C*
621800000000     C                   endif
621900000000     C*
622000000000     C                   ENDSR
622100000000     C/EJECT
622200151201R195 C************************************************************
622300151201 "   C* Reperimento parametri di setup per Black List
622400151201 "   C************************************************************
622500151201R195 C     RepSDBLACK2   BEGSR
622600151201     C*
622700151201     C                   Clear                   SDGPA11DS
622800151201     C*
622900151201     C                   CALLB     'XPAR'
623000151201     C                   Parm                    XScSoc
623100151201     C                   Parm      'SDBLACK2'    XPaPar            8
623200151201     C                   Parm                    XPaOut         2000
623300151201     C                   Parm      *OFF          XPaErr            1
623400151201     C                   Parm      ' '           XPaRic            1
623500151201     C*
623600151201     C                   if        XPaErr      = *On
623700151201     C                   Eval      $TpErr1     = '03'
623800151201     C                   EXSR      StpErr1
623900151201     C                   Eval      ERRG52      = '1'
624000151201     C                   EXSR      ENDPGM
624100151201     C*
624200151201     C                   else
624300151201     C                   MoveL     XPaOut        SDGPA11DS
624400151201     C*
624500151201     C                   if        AttBLA11    <> *On
624600151203     C                   Eval      $TpErr1     = '06'
624700151201     C                   EXSR      StpErr1
624800151201     C                   Eval      ERRG52      = '1'
624900151201     C                   EXSR      ENDPGM
625000151201     C*
625100151201     C                   endif
625200151201     C*
625300151201     C                   endif
625400151201     C*
625500151201R195 C                   ENDSR
625600151201R195 C/EJECT
625700151202R195 C************************************************************
625800151202 "   C* Reperimento Sottotitolo di testata
625900151202 "   C************************************************************
626000151202R195 C     RepSottTitT   BEGSR
626100151202     C                   Select
626200151202     C                   When      TpcomG52 = 'E'
626300151202     C                   Eval      IDMSG    = SOTTITTE
626400151202     C                   When      TpcomG52 = 'S'
626500151202     C                   Eval      IDMSG    = SOTTITTS
626600151202     C                   When      TpcomG52 = 'B'
626700151202     C                   Eval      IDMSG    = SOTTITTB
626800151202     C                   EndSl
626900151202     C*
627000151202     C                   CALLB     'XRTVM'
627100151202     C                   PARM                    IDMSG            27
627200151202     C                   PARM                    TXTMSG          100
627300151202     C                   PARM                    ERRMSG            1
627400160208     C                   PARM      *Off          CTRMSG            1            centratura
627500151202     C                   PARM      30            LENMSG            3 0          lun output.
627600151202     C     ERRMSG        IFNE      *ON
627700151202     C                   MOVEL     TXTMSG        SOTTITT
627800151202     C                   ELSE
627900151202     C                   MOVEL     *ALL'?'       SOTTITT
628000151202     C                   ENDIF
628100151202R195 C                   ENDSR
628200151202R195 C/EJECT
628201160607R224 C************************************************************
628202160607 "   C* REPERIMENTO dati anagafici aggiuntivi
628203160607 "   C************************************************************
628204160607R224 C     RepDatiAna    BegSr
628205160607     C*
628206160607     C* preparo i parametri di chiamata al driver
628207160607     C                   Clear                   XMPEDS
628208160607     C                   EVAL      TpChiamata = '01'
628209160607     C                   EVAL      NChiavi = 5
628210160607     C                   EVAL      VLogica = 2
628211160607     C*
628212160607     C* preparo le chiavi per la chiamata al driver
628213160607     C                   Clear                   XMPEDS1
628214160607     C                   MoveL     XSCSOC        DSMPESOC
628215160607     C*
628216160607     C                   Select
628217160607     C* Spesometro
628219160607     C                   When      TpComG52    = 'E'
628222160607     C                   MoveL     'SDGSG51REA'  DSMPEPGM
628223160607     C*
628224160607     C* Acquisti da San Marino
628225160607     C                   When      TpComG52    = 'S'
628227160607     C                   MoveL     'SDGSG51RSA'  DSMPEPGM
628229160607     C*
628230160607     C* Black List
628231160607     C                   When      TpComG52    = 'B'
628232160607     C                   MoveL     'SDGSG51RBA'  DSMPEPGM
628234160607     C*
628235160607     C                   endsl
628236160607     C*
628237160607     C                   MoveL     *blanks       DSMPEPRF
628238160607     C* Si reperiscono i dati di default a seconda delle modalita con
628239160607     C* cui il pgm e usato
628240160607     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
628241160607     C* MPETIP = 'PV'  pgm richiamato da menu
628242160607     C* MPETIP = 'PB'  x lancio Batch
628243160607     C                   MoveL     'PB'          DSMPETIP
628244160607     C*
628245160607     C                   CALLB     'XMPE'
628246160607     C                   PARM                    XMPEDS
628247160607     C                   PARM                    XMPEDS1
628248160607     C*
628249160607     C     Risultato     IfEq      '0'
628250160607     C                   MoveL     DSMPEPAR      DSDatiAna
628251160607     C                   EndIf
628252160607     C*
628253160607R224 C                   EndSr
628300000000     C************************************************************
628400000000     C* Imposto i parametri di elaborazione
628500000000     C************************************************************
628600000000     C     RieParam      BEGSR
628700160201R195 C*
628800160201 "   C                   Select
628900160201 "   C* Acquisti da San Marino
629000160201 "   C                   When      TpComG52    = 'S'
629100160201 "   C                   Eval      *In11       = *On
629200160201 "   C                   Other
629300160201 "   C* Spesometro  o
629400160201 "   C* Acquisti da San Marino
629500160201 "   C                   Eval      *In11       = *Off
629600160201 "   C                   endSl
629700160201R195 C*
629800000000     C*
629900130820     C                   Eval      PACtb       = CtbG52
630000000000     C                   EXSR      RepCtbD
630100000000     C*
630200130820     C                   Eval      PATpRegZ    = TpRegzG52
630300000000     C                   EXSR      RepTpRegzD
630400000000     C*
630500130820     C                   Eval      PAAnno      = AnnoG52
630600160201R195 C                   Eval      PAMese      = MeseG52
630700160201 "   C*
630800151123 "   C                   Eval      PAStato     = StatoG52
630900151123R195 C                   EXSR      RepStatoD
631000000000     C*
631100130820     C                   Eval      PACF        = CFG52
631200000000     C                   EXSR      RepCFD
631300000000     C*
631400130820     C                   Eval      PAPartIVA   = PartIVAG52
631500130820     C                   Eval      PACdFisc    = CdFiscG52
631600000000     C*
631700130820     C                   Eval      PASogg      = SoggG52
631800000000     C                   EXSR      RepSoggD
631900000000     C*
632000130820     C                   Eval      PAKcc1      = Kcc1G52
632100130820     C                   Eval      PAKcc2      = Kcc2G52
632200130820     C                   Eval      PAKcc3      = Kcc3G52
632300130820     C                   Eval      PAKcc4      = Kcc4G52
632400130820     C                   Eval      PAKcc5      = Kcc5G52
632500130820     C                   Eval      PAKcc6      = Kcc6G52
632600000000     C*
632700130820     C                   Eval      PAKsc       = KscG52
632800000000     C                   EXSR      RepKscD
632900000000     C*
633000000000     C                   Eval      PAOn1       = XScOn
633100000000     C                   Eval      PAOff1      = XScOff
633200130820     C                   if        DelManuG52  = *On
633300000000     C                   Eval      PADelManu   = XScOn
633400000000     C                   else
633500000000     C                   Eval      PADelManu   = XScOff
633600000000     C                   endif
633700110720     C*
633800110720     C                   Eval      PAOn2       = XScOn
633900110720     C                   Eval      PAOff2      = XScOff
634000130820     C                   if        StpAnnG52   = *On
634100110720     C                   Eval      PAStpAnn    = XScOn
634200110720     C                   else
634300110720     C                   Eval      PAStpAnn    = XScOff
634400110720     C                   endif
634500000000     C*
634600000000     C                   ENDSR
634700000000     C/EJECT
634800000000     C************************************************************
634900000000     C* Reperisco la descrizione della contabilità
635000000000     C************************************************************
635100000000     C     RepCtbD       BEGSR
635200000000     C*
635300000000     C                   Clear                   SDGTABDS
635400000000     C*
635500000000     C                   Eval      XTbRic     =  '1'
635600000000     C                   Eval      XTbAzi     =  XScSoc
635700000000     C                   Eval      XTbKey     =  *Zero
635800000000     C                   Eval      XTbCod     =  'G84'
635900130820     C                   Move      CtbG52        XTbKey
636000000000     C*
636100000000     C                   CALL      'SDGTABDR'
636200000000     C                   PARM                    SDGTABDS
636300000000     C*
636400000000     C                   if        XTbErr      = '0'
636500000000     C                   MoveL     XTbUni        ANGG84DS
636600000000     C                   MoveL     DsBG84        PACtbD
636700000000     C                   else
636800000000     C                   MoveL     *All'?'       PACtbD
636900000000     C                   endif
637000000000     C*
637100000000     C                   ENDSR
637200000000     C/EJECT
637300000000     C************************************************************
637400000000     C* Reperisco la descrizione del tipo registrazione
637500000000     C************************************************************
637600000000     C     RepTpRegzD    BEGSR
637700000000     C*
637800000000     C                   Clear                   X07VALDS
637900000000     C*
638000000000     C                   Move      '1'           X07Ric
638100000000     C                   MoveL     'ANDIZ000'    X07Trc
638200000000     C                   MoveL     'DSPTPREGZ '  X07Cam
638300130820     C                   MoveL     TpRegzG52     X07Val
638400000000     C*
638500000000     C                   CALLB     'X07VALR'
638600000000     C                   PARM                    X07VALDS
638700000000     C*
638800000000     C                   if        X07ERR      = '0'
638900000000     C                   MoveL     X07Des        PATpRegzD
639000000000     C                   else
639100000000     C                   MoveL     *All'?'       PATpRegzD
639200000000     C                   endif
639300000000     C*
639400000000     C                   ENDSR
639500000000     C/EJECT
639600151123R195 C************************************************************
639700151123 "   C* Reperisco la descrizione dello Stato
639800151123 "   C************************************************************
639900151123R195 C     RepStatoD     BEGSR
640000151123     C*
640100151123     C                   Clear                   SDGTABDS
640200151123     C                   Clear                   AngG12DS
640400151123     C*
640500151123     C                   Eval      XTbRic     =  '1'
640600151123     C                   Eval      XTbAzi     =  XScSoc
640700151123     C                   Eval      XTbKey     =  *Zero
641500151123     C                   Eval      XTbCod      = 'G12'
641700151123     C*
641800151123     C                   Move      StatoG52      XTbKey
641900151123     C*
642000151123     C                   CALL      'SDGTABDR'
642100151123     C                   PARM                    SDGTABDS
642200151123     C*
643300151123     C                   MoveL     XTbUni        ANGG12DS
643400151123     C                   MoveL     DesG12        PAStatoD
643600151123     C*
643700151123R195 C                   ENDSR
643800151123R195 C/EJECT
643900000000     C************************************************************
644000000000     C* Reperisco la descrizione della natura del soggetto
644100000000     C************************************************************
644200000000     C     RepCFD        BEGSR
644300000000     C*
644400000000     C                   Clear                   X07VALDS
644500000000     C*
644600130820     C                   if        CFG52      <> *Blank
644700000000     C*
644800000000     C                   Move      '1'           X07Ric
644900000000     C                   MoveL     'ANDIZ000'    X07Trc
645000000000     C                   MoveL     'DSPCLIFOR '  X07Cam
645100130820     C                   MoveL     CFG52         X07Val
645200000000     C                   Move      *On           X07All
645300000000     C                   CALLB     'X07VALR'
645400000000     C                   PARM                    X07VALDS
645500000000     C*
645600000000     C                   if        X07Err      = '0'
645700000000     C                   MoveL     X07Des        PACFD
645800000000     C                   else
645900000000     C                   MoveL     *All'?'       PACFD
646000000000     C                   endif
646100000000     C*
646200000000     C                   endif
646300000000     C*
646400000000     C                   ENDSR
646500000000     C/EJECT
646600000000     C************************************************************
646700000000     C* Reperisco la descrizione del soggetto
646800000000     C************************************************************
646900000000     C     RepSoggD      BEGSR
647000000000     C*
647100000000     C                   Clear                   PASoggD
647200000000     C*
647300130820     C                   if        SoggG52    <> *Blank
647400000000     C*
647500130820     C                   Eval      WSogg       = SoggG52
647600000000     C                   Eval      WKsc        = *Blank
647700000000     C                   Eval      WKcc        = *Blank
647800000000     C                   Eval      WSNatura    = *Blank
647900000000     C                   Eval      WTpInd      = *Blank
648000000000     C                   Eval      WCdInd      = *Blank
648100000000     C                   MoveL     'DVASOG    '  DVAStrutt
648200000000     C                   Eval      DVALENOUT   = %size(DvaSog)
648300000000     C*
648400000000     C                   EXSR      CallDvaSog
648500000000     C*
648600000000     C                   if        DVAErrore  <> '1'
648700000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
648800000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
648900000000     C*
649000000000     C                   MoveL     DVSDes        PASoggD
649100000000     C                   else
649200000000     C                   MoveL     *All'?'       PASoggD
649300000000     C                   endif
649400000000     C*
649500000000     C                   endif
649600000000     C*
649700000000     C                   ENDSR
649800000000     C/EJECT
649900000000     C************************************************************
650000000000     C* Reperisco la descrizione del conto
650100000000     C************************************************************
650200000000     C     RepKscD       BEGSR
650300000000     C*
650400000000     C                   Clear                   PAKscD
650500000000     C*
650600130820     C                   if        KccG52     <> *Blank   or
650700130820     C                             KscG52     <> *Blank
650800000000     C*
650900130820     C                   Eval      WKcc        = KccG52
651000130820     C                   Eval      WKsc        = KscG52
651100151204R195 X****               Eval      $Data       = UDateIso
651200151204R195 C                   Eval      $Data       = *Loval
651300000000     C*
651400000000     C                   EXSR      CallMVC002
651500000000     C*
651600000000     C                   if        $Esito     <> *On
651700000000     C                   MoveL     DesBrev002    PAKscD
651800000000     C                   else
651900000000     C                   MoveL     *All'?'       PAKscD
652000000000     C                   endif
652100000000     C*
652200000000     C                   endif
652300000000     C*
652400000000     C                   ENDSR
652500000000     C/EJECT
652600000000     C******************************************************
652700130820     C* Preparo dati 'fissi' (SDGSG52P1)     (solo I° volta)
652800000000     C******************************************************
652900000000     C     StpErr1_Inz   BEGSR
653000000000     C*
653100000000     C* Overflow
653200000000     C                   if        *In41       = *On        or
653300000000     C                             $PrimaStp1  = *On
653400000000     C* Testata
653500130820     C                   WRITE     SG52T11
653600000000     C*
653700000000     C                   if        $PrimaStp1  = *On
653800000000     C* Parametri di elaborazione
653900130820     C                   WRITE     SG52PA1
654000000000     C                   endif
654100000000     C*
654200000000     C* Intestazione colonne
654300000000     C                   if        $TpErr1     = *Blank
654400130820     C                   WRITE     SG52T21
654500000000     C                   endif
654600000000     C*
654700000000     C                   Eval      *In41       = *Off
654800000000     C                   Eval      $PrimaStp1  = *Off
654900000000     C                   endif
655000000000     C*
655100000000     C                   ENDSR
655200000000     C/EJECT
655300000000     C************************************************************
655400130820     C* Stampa errori e avvertimenti (SDGSG52P1)
655500000000     C************************************************************
655600000000     C     StpErr1       BEGSR
655700000000     C*
655800000000     C                   EXSR      StpErr1_Inz
655900000000     C*
656000000000     C                   Select
656100000000     C*
656200000000     C* Stampa riga di avvertimento per movimento annullato.
656300000000     C                   When      $TpAnn     <> *Blank
656400110720     C*
656500130820     C                   if        StpAnnG52   = *On
656600000000     C                   Eval      AVVTpAnn    = $TpAnn
656700000000     C                   EXSR      RepTpAnnD
656800130820     C                   WRITE     SG52PAnn
656900110720     C                   EXSR      StpErr1_Dati
657000110720     C                   endif
657100000000     C*
657200000000     C*
657300000000     C*  ERRORE: elaborazione interrotta. Codice Stato società di collegamento
657400000000     C*   non valido. Codice Stato =
657500000000     C                   When      $TpErr1     = '01'
657600000000     C                   Eval      P101Stato   = XScSta
657700130820     C                   WRITE     SG52P101
657800000000     C*
657900000000     C*  ERRORE: elaborazione interrotta. Codice società di collegamento
658000000000     C*   non valido. Codice società =
658100000000     C                   When      $TpErr1     = '02'
658200130820     C                   Eval      P102Soc     = SocietaG52
658300130820     C                   WRITE     SG52P102
658400000000     C*
658500130821     C*  ERRORE: elaborazione interrotta. Parametro modulo SDGSPECF non
658600000000     C*  trovato per la società di collegamento. Codice società =
658700000000     C                   When      $TpErr1     = '03'
658800130820     C                   Eval      P103Soc     = SocietaG52
658900130820     C                   WRITE     SG52P103
659000000000     C*
659100130821     C*  ERRORE: elaborazione interrotta. Parametro modulo SDGSPECF non
659200000000     C*  attivato per la società di collegamento. Codice società =
659300000000     C                   When      $TpErr1     = '04'
659400130820     C                   Eval      P104Soc     = SocietaG52
659500130820     C                   WRITE     SG52P104
659600130918     C*
659700130918     C*  ERRORE: elaborazione interrotta. Manca codice UIC in tab. G12
659800130918     C*  per la società di collegamento. Codice società =
659900130918     C                   When      $TpErr1     = '05'
660000130918     C                   Eval      P105Soc     = SocietaG52
660100130918     C                   WRITE     SG52P105
660200151201R195 C*
660300151201 "   C*  ERRORE: elaborazione interrotta. Parametro modulo SDBLACK2 non
660400151201 "   C*  attivato per la società di collegamento. Codice società =
660500151201 "   C                   When      $TpErr1     = '06'
660600151201 "   C                   Eval      P106Soc     = SocietaG52
660700151201R195 C                   WRITE     SG52P106
660800000000     C*
660900000000     C*
661000000000     C*  ERRORE: elaborazione interrotta causa errori nella stringa sql.
661100000000     C*  Codice errore SQL 99999 nell'esecuzione della seguente stringa:
661200000000     C                   When      $TpErr1     = '99'
661300000000     C                   Eval      P199SqlCd   = SqlCod
661400000000     C                   Eval      P199SQL1    = %subst(StringaSql:1 :197)
661500000000     C                   Eval      P199SQL2    = %subst(StringaSql:198 :197)
661600000000     C                   Eval      P199SQL3    = %subst(StringaSql:395 :197)
661700000000     C                   Eval      P199SQL4    = %subst(StringaSql:595 :197)
661800000000     C                   Eval      P199SQL5    = %subst(StringaSql:792 :197)
661900151203R195 C                   Eval      P199SQL6    = %subst(StringaSql:989 :197)
662000151203 "   C                   Eval      P199SQL7    = %subst(StringaSql:1186 :197)
662100151203 "   C                   Eval      P199SQL8    = %subst(StringaSql:1383 :197)
662200151203 "   C                   Eval      P199SQL9    = %subst(StringaSql:1580 :197)
662300151203 "   C                   Eval      P199SQL10   = %subst(StringaSql:1777 :197)
662400151203 "   C                   Eval      P199SQL11   = %subst(StringaSql:1974 :197)
662500151203 "   C                   Eval      P199SQL12   = %subst(StringaSql:2171 :197)
662600151203 "   C                   Eval      P199SQL13   = %subst(StringaSql:2368 :197)
662700151203 "   C                   Eval      P199SQL14   = %subst(StringaSql:2565 :197)
662800151203R195 C                   Eval      P199SQL15   = %subst(StringaSql:2762 :197)
662900130820     C                   WRITE     SG52P199
663000000000     C*
663100000000     C                   OTHER
663200000000     C*  ERR . Avvertimento/errore non codificato.
663300130820     C                   WRITE     SG52P1XX
663400000000     C*
663500000000     C                   endSl
663600000000     C*
663700000000     C                   ENDSR
663800000000     C/EJECT
663900000000     C******************************************************
664000000000     C* Stampo i riferimenti del movimento
664100000000     C******************************************************
664200000000     C     StpErr1_Dati  BEGSR
664300000000     C*
664400130820     C                   Clear                   SG52DA1
664500000000     C*
664600000000     C                   MoveL     MOVKcc        DA1Kcc
664700000000     C                   MoveL     MOVKsc        DA1Ksc
664800000000     C*
664900000000     C                   Eval      WKcc        = MOVKcc
665000000000     C                   Eval      WKsc        = MOVKsc
665100000000     C                   Eval      $Data       = MOVDtDoc
665200000000     C                   EXSR      CallMVC002
665300000000     C                   MoveL     Sogg002       DA1Sogg
665400000000     C*
665500000000     C                   if        MOVDtDoc   <> *Loval
665600000000     C     *JobRun       Move      MOVDtDoc      DA1DtDoc
665700000000     C                   endif
665800000000     C                   Z-Add     MOVNrDoc      DA1NrDoc
665900000000     C                   MoveL     MOVSerDoc     DA1SerDoc
666000000000     C*
666100000000     C                   MoveL     MOVCausTes    DA1Causale
666200000000     C*
666300000000     C                   if        IVAAliq     > 0
666400000000     C                   SetOn                                        13
666500000000     C                   Z-Add     IVAAliq       DA1Aliq
666600000000     C                   else
666700000000     C                   SetOff                                       13
666800000000     C                   Move      IVARifIva     DA1RifIVA
666900000000     C                   endif
667000000000     C*
667100130826     C                   Z-Add     $ImpDoc       DA1ImpDoc
667200000000     C*
667300000000     C                   Z-Add     MOVSys        DA1Sys
667400000000     C                   Z-Add     MOVNrAsReg    DA1NrAsReg
667500000000     C*
667600130820     C                   WRITE     SG52DA1
667700000000     C*
667800000000     C                   ENDSR
667900000000     C/EJECT
668000000000     C************************************************************
668100000000     C* Reperisco la descrizione del tipo annullamento
668200000000     C************************************************************
668300000000     C     RepTpAnnD     BEGSR
668400000000     C*
668500000000     C                   Clear                   X07VALDS
668600000000     C*
668700000000     C                   Move      '1'           X07Ric
668800130820     C                   MoveL     'SDGSCF000 '  X07Trc
668900130820     C                   MoveL     'SCFTPANN  '  X07Cam
669000000000     C                   MoveL     $TpAnn        X07Val
669100000000     C*
669200000000     C                   CALLB     'X07VALR'
669300000000     C                   PARM                    X07VALDS
669400000000     C*
669500000000     C                   if        X07ERR      = '0'
669600000000     C                   MoveL     X07Des        AVVTpAnnD
669700000000     C                   else
669800000000     C                   MoveL     *All'?'       AVVTpAnnD
669900000000     C                   endif
670000000000     C*
670100000000     C                   ENDSR
670200000000     C/EJECT
670300000000     C******************************************************
670400130820     C* Preparo dati 'fissi' stampa (SDGSG52P2) (solo I° volta)
670500000000     C******************************************************
670600000000     C     StpAutof_Inz  BEGSR
670700000000     C*
670800000000     C* Overflow
670900000000     C                   if        *In42       = *On        or
671000000000     C                             $PrimaStp2  = *On
671100000000     C* Testata
671200130820     C                   WRITE     SG52T12
671300000000     C*
671400000000     C                   if        $PrimaStp2  = *On
671500000000     C* Parametri di elaborazione
671600130820     C                   WRITE     SG52PA2
671700000000     C                   endif
671800000000     C*
671900000000     C* Intestazione colonne
672000130820     C                   WRITE     SG52T22
672100000000     C*
672200000000     C                   Eval      *In42       = *Off
672300000000     C                   Eval      $PrimaStp2  = *Off
672400000000     C                   endif
672500000000     C*
672600000000     C                   ENDSR
672700000000     C/EJECT
672800000000     C********************************************************************
672900000000     C* Stampa autofatture scartate
673000000000     C********************************************************************
673100000000     C     StpAutof      BEGSR
673200000000     C*
673300000000     C                   if        MOVSys     <> SAVSys       or
673400000000     C                             MOVNrAsReg <> SAVNrAsReg
673500000000     C*
673600000000     C                   Z-Add     MOVSys        SAVSys
673700000000     C                   Z-Add     MOVNrAsReg    SAVNrAsReg
673800000000     C*
673900000000     C*
674000000000     C                   EXSR      StpAutof_Inz
674100000000     C*
674200000000     C*
674300000000     C                   MoveL     MOVKcc        P201Kcc
674400000000     C                   MoveL     MOVKsc        P201Ksc
674500000000     C                   MoveL     MOVCausTes    P201CausT
674600000000     C*
674700000000     C                   if        IVADtReg   <> *Loval
674800000000     C     *JobRun       Move      IVADtReg      P201DtReg
674900000000     C                   endif
675000000000     C                   Z-Add     IVANrReg      P201NrReg
675100000000     C                   MoveL     IVASerReg     P201SerReg
675200000000     C*
675300000000     C                   if        MOVDtDoc   <> *Loval
675400000000     C     *JobRun       Move      MOVDtDoc      P201DtDoc
675500000000     C                   endif
675600000000     C                   Z-Add     MOVNrDoc      P201NrDoc
675700000000     C                   MoveL     MOVSerDoc     P201SerDoc
675800000000     C*
675900000000     C                   if        MOVDare     = 1
676000000000     C                   Z-Sub     MOVImporto    P201ImpDoc
676100000000     C                   else
676200000000     C                   Z-Add     MOVImporto    P201ImpDoc
676300000000     C                   endif
676400000000     C*
676500000000     C                   Z-Add     MOVSys        P201Sys
676600000000     C                   Z-Add     MOVNrAsReg    P201NrAsRe
676700000000     C*
676800130820     C                   WRITE     SG52P201
676900000000     C*
677000000000     C* aggiorno i totali
677100000000     C                   if        MOVDare     = 1
677200000000     C                   Eval      $Segno      = +1
677300000000     C                   else
677400000000     C                   Eval      $Segno      = -1
677500000000     C                   endif
677600000000     C*
677700000000     C                   Eval      P299ImpDoc  =
677800000000     C                               P299ImpDoc + (MOVImporto * $Segno)
677900000000     C*
678000000000     C                   endif
678100000000     C*
678200000000     C                   ENDSR
678300000000     C/EJECT
678400000000     C************************************************************
678500130820     C* Stampa totale autofatture   (SDGSG52P2)
678600000000     C************************************************************
678700000000     C     StpAutof_Tot  BEGSR
678800000000     C*
678900000000     C* solo se stampata almeno una riga
679000000000     C                   if        $PrimaStp2  = *Off
679100130820     C                   WRITE     SG52P299
679200000000     C                   endif
679300000000     C*
679400000000     C                   ENDSR
679500000000     C/EJECT
679600000000     C******************************************************
679700130820     C* Preparo dati 'fissi' stampa (SDGSG52P3) (solo I° volta)
679800000000     C******************************************************
679900000000     C     StpNoFisc_Inz BEGSR
680000000000     C*
680100000000     C* Overflow
680200000000     C                   if        *In43       = *On        or
680300000000     C                             $PrimaStp3  = *On
680400000000     C* Testata
680500130820     C                   WRITE     SG52T13
680600000000     C*
680700000000     C                   if        $PrimaStp3  = *On
680800000000     C* Parametri di elaborazione
680900130820     C                   WRITE     SG52PA3
681000000000     C                   endif
681100000000     C*
681200000000     C* Intestazione colonne
681300130820     C                   WRITE     SG52T23
681400000000     C*
681500000000     C                   Eval      *In43       = *Off
681600000000     C                   Eval      $PrimaStp3  = *Off
681700000000     C                   endif
681800000000     C*
681900000000     C                   ENDSR
682000000000     C/EJECT
682100000000     C********************************************************************
682200130820     C* Stampa dati fiscali non valorizzati (SDGSG52P3)
682300000000     C********************************************************************
682400000000     C     StpNoFisc     BEGSR
682500000000     C*
682600000000     C                   EXSR      StpNoFisc_Inz
682700000000     C*
682800130820     C                   Clear                   SG52P301
682900000000     C*
683000000000     C                   Eval      WKcc        = MOVKcc
683100000000     C                   Eval      WKsc        = MOVKsc
683200000000     C                   Eval      $Data       = MOVDtDoc
683300000000     C                   EXSR      CallMVC002
683400000000     C*
683500000000     C                   MoveL     SNatura002    P301CF
683600000000     C                   MoveL     MOVKcc        P301Kcc
683700000000     C                   MoveL     MOVKsc        P301Ksc
683800000000     C                   MoveL     Sogg002       P301Sogg
683900000000     C*
684000000000     C                   if        Tecnico002 <>'2'
684100000000     C                   Move      Descr002      P301Descr
684200000000     C*
684300000000     C                   if        MOVDtDoc <> *loval
684400000000     C     *JobRun       Move      MOVDtDoc      P301DtDoc
684500000000     C                   endif
684600000000     C*
684700000000     C                   else
684800000000     C*
684900000000     C                   Move      CPADescr      P301Descr
685000000000     C                   Move      'X'           P301Tecnic
685100000000     C*
685200000000     C                   SetOn                                        12
685300000000     C*
685400000000     C                   if        MOVDtDoc   <> *Loval
685500000000     C     *JobRun       Move      MOVDtDoc      P301DtDoc
685600000000     C                   endif
685700000000     C*
685800000000     C                   if        MOVDtReg   <> *Loval
685900000000     C     *JobRun       Move      MOVDtReg      P301DtReg
686000000000     C                   endif
686100000000     C*
686200000000     C                   Z-Add     MOVSys        P301Sys
686300000000     C                   Z-Add     MOVNrAsReg    P301NrAsRe
686400000000     C*
686500000000     C                   endif
686600000000     C*
686700000000     C                   MoveL     $PartIVA      P301PartIV
686800000000     C                   MoveL     $CdFisc       P301CdFisc
686900000000     C                   MoveL     $Stato        P301Stato
687000000000     C*
687100130820     C                   WRITE     SG52P301
687200000000     C*
687300000000     C                   ENDSR
687400000000     C/EJECT
687500000000     C************************************************************
687600000000     C* DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
687700000000     C************************************************************
687800000000     C     DefCam        BEGSR
687900000000     C*
688000130826     C*
688100130826     C*  SDGSCF03L
688200160201R195 X**** K02SCF03      KLIST
688300160201 "   X****               KFLD                    SCFSys
688400160201R195 X****               KFLD                    SCFNrAsReg
688500160201R195 C*
688600160201 "   C     K03SCF03      KLIST
688700160201 "   C                   KFLD                    SCFTpCom
688800160201 "   C                   KFLD                    SCFSys
688900160201R195 C                   KFLD                    SCFNrAsReg
689000130826     C*
689100130820     C*  SDGSCF04L
689200160201R195 X**** K12SCF04      KLIST
689300160201R195 C     K13SCF04      KLIST
689400130820     C                   KFLD                    SCFSocieta
689500160201R195 C                   KFLD                    SCFTpCom
689600130820     C                   KFLD                    SCFKcc
689700130820     C                   KFLD                    SCFKsc
689800130820     C                   KFLD                    SCFDTDOC
689900130820     C                   KFLD                    SCFNRDOC
690000130820     C                   KFLD                    SCFSERDOC
690100130820     C                   KFLD                    SCFDTREG
690200130820     C                   KFLD                    SCFNRREG
690300130820     C                   KFLD                    SCFSERREG
690400130820     C                   KFLD                    SCFDTPAR
690500130820     C                   KFLD                    SCFNRPAR
690600130820     C                   KFLD                    SCFSERPAR
690700000000     C*
690800130823     C* SDGSPR04L
690900130823     C     K12SPR04      KLIST
691000130823     C                   KFLD                    SPRSocieta
691100130823     C                   KFLD                    SPRTipReg
691200130823     C                   KFLD                    SPRStpReg
691300130823     C                   KFLD                    SPRTPREG
691400130823     C                   KFLD                    SPRLIBRO
691500130823     C                   KFLD                    SPRALIQ
691600130823     C                   KFLD                    SPRRIFIVA
691700130823     C                   KFLD                    SPRCAUSALE
691800130823     C                   KFLD                    SPRKCC
691900130823     C                   KFLD                    SPRKSC
692000130823     C                   KFLD                    SPRVOCE
692100130823     C                   KFLD                    SPRCLASSE
692200000000     C*
692300000000     C* SDGSO301L
692400160523R224 X**** K04SO301      KLIST
692401160523R224 C     K05SO301      KLIST
692500000000     C                   KFLD                    SO3Sogg
692600000000     C                   KFLD                    SO3DtFiVL
692700000000     C                   KFLD                    SO3Sys
692800000000     C                   KFLD                    SO3NrAsint
692801160523R224 C                   KFLD                    SO3Dest
692900000000     C*
693000000000     C*  ANDLI01L
693100000000     C     K03DLI01      KLIST
693200000000     C                   KFLD                    DLISocieta
693300000000     C                   KFLD                    DLITpReg
693400000000     C                   KFLD                    DLILibro
693500000000     C*
693600000000     C*  ANRIV01L
693700000000     C     K04RIV01      KLIST
693800000000     C                   KFLD                    RIVSocieta
693900000000     C                   KFLD                    RIVTpReg
694000000000     C                   KFLD                    RIVAliq
694100000000     C                   KFLD                    RIVRifIva
694200000000     C*
694300000000     C*  ANALR01L
694400000000     C     K05ALR01      KLIST
694500000000     C                   KFLD                    ALRSocieta
694600000000     C                   KFLD                    ALRTpReg
694700000000     C                   KFLD                    ALRLibro
694800000000     C                   KFLD                    ALRAliq
694900000000     C                   KFLD                    ALRRifIva
695000000000     C*
695100000000     C*  NDPAS01L
695200000000     C     K03PAS01      KLIST
695300000000     C                   KFLD                    PASSys
695400000000     C                   KFLD                    PASNrAsReg
695500000000     C                   KFLD                    PASNrRigaM
695600000000     C*
695700000000     C*  NDMOA01L
695800000000     C     K03MOA01      KLIST
695900000000     C                   KFLD                    MOASys
696000000000     C                   KFLD                    MOANrAsReg
696100000000     C                   KFLD                    MOANrRigaM
696200151130R195 C*
696300151130 "   C     K04MOA01      KLIST
696400151130 "   C                   KFLD                    MOASys
696500151130 "   C                   KFLD                    MOANrAsReg
696600151130 "   C                   KFLD                    MOANrRigaM
696700151130R195 C                   KFLD                    MOADimen
696800131014     C*
696900131014     C*  NDIVA01L
697000131014     C     K02IVA01      KLIST
697100131014     C                   KFLD                    IVASys
697200131014     C                   KFLD                    IVANrAsReg
697300000000     C*
697400000000     C                   ENDSR
697500000000     C/EJECT
