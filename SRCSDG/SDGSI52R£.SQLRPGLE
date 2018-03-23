000100000000     H*PARMS DFTACTGRP(*NO)
000200170707     H*PARMS BNDDIR(PJXBND PJCBND SDG007)
000300000000     H*PARMS ACTGRP(QILE)
000400000000     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000500000000     H*PARMS RDB(*NONE) DYNUSRPRF(*OWNER)
000600000000     H*****************************************************************
000700170906     H DECEDIT('0,') DATEDIT(*DMY.) DATFMT(*ISO)
000702170906     H DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND':'SDG007')
000703170906     H ACTGRP('QILE')
000704170906
000800000000     H*****************************************************************
000900170529      *
001000170529R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
001100000000     H*
001200170616     H*                 Comunicazione Spesometro Trimestrale
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
002600170529      *    Se una riga di SDGCST00F è modificata manualmente, non
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
004600000000     F*------------
004700000000     F* Regole per estrazione dati
004800000000     F* Key: Soc/Regola/Assoc/Cau/Conto/Voce
004900130823     FSDGSPR04L IF   E           K DISK
005000000000     F*------------
005100000000     F* partite  key: SYS/NRASREG/NRRIGAM/NRRIGA
005200000000     FNDPAS01L  IF   E           K DISK
005300000000     F*-------------
005400000000     F* Key: SYS/NRASREG/NRRIGAM
005500000000     FNDMOA01L  IF   E           K DISK
005600170811  fm3F*------------
005700170811   " F* Percipienti
005800170811  fm3FANAPE01L  IF   E           K DISK
005900130826     F*------------
006000170616     F* Comunicazione Spesometro Trimestrale
006100170718     FSDGCST00F O    E             DISK    UsrOpn
006200130826     F                                     COMMIT
006201170906BRT  F*** LOG BRT Comunicazione Spesometro Trimestrale
006202170906BRT  F***SDGCSTLOG O    E             DISK    UsrOpn RENAME(SDGCST000:SDGCST999)
006203170906BRT  F***                                     COMMIT
006204170906     F*------------
006400170724     F* Catologo File Spesometro Trimestrale
006500170724     F* Key: Soc/TpCom/TpRec/A/T/M/PrgEstr     FIFO
006600170724     FSDGCSF03L UF A E           K DISK    UsrOpn
006700170724     F                                     COMMIT
006800000000     F*------------
006900000000     F* Segnalazioni - Errori e avvertimenti
007000170615     FSDGSI52P1 O    E             PRINTER  OFLIND(*In41)
007100110720     F                                     USROPN
007200000000     F*-------------
007300170704     F* Avvertimenti per registrazioni di autofatture
007400170615     FSDGSI52P2 O    E             PRINTER  OFLIND(*In42)
007500000000     F*-------------
007600000000     F* Avvertimenti per dati fiscali non impostati
007700170615     FSDGSI52P3 O    E             PRINTER  OFLIND(*In43)
007800000000     D*****************************************************************
007900000000     D* Passaggio Parametri
008000000000     D KPJBA         E DS
008100110721     D*-------------
008200110721     D* Salvataggio Kpjbu per richiamo altri pgm
008300110721     D SavKPJBU        S            256
008400000000     D*-------------------------------------------------------*
008500000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
008600170710     D*  DS per reperire il valore di XScSKP nella posizione 30
008700170710     D* legislazione della società
008800170710     D  StaSKP                30     30
008900000000     D*-------------
009000000000     D* DS Interna per dati di output di SDGSOCDR
009100000000     D XSOCDS          DS          1000
009200000000     D*-------------
009300000000     D* DS Esterna gestione messaggi
009400000000     D SDGMSGDS      E DS                  INZ
009500000000     D  STS          E                     EXTFLD(MSGSTS)
009600000000     D                                     DIM(333)
009700000000     D  JBA          E                     EXTFLD(MSGJBA)
009800000000     D                                     DIM(502)
009900000000     D*-------------------------------------------------------*
010000000000     D* Reperimento nome PGM
010100000000     D STATUS         SDS           333
010200000000     D  DSPGM            *Proc
010300000000     D  Utente               254    263
010400000000     D*-------------------------------------------------------*
010500000000     D* Parametri che ricevo dal lancio
010600170615     D SDGSI52DS     E DS
010700170615     D  ERRI52       E                     INZ('0')
010800000000     D*-------------------------------------------------------*
010900130822     D* Parametri per apertura file di stampa
011000170707     D SDG00700DS    E DS
011100130822     D*-------------
011200170728     D* Parametri per recuperare i dati anagrafici
011300170728     D SDG00701DS    E DS
011400170728     D*-------------
011500000000     D* Ds x fetch
011600000000     D NDIVADS       E DS                  Occurs(1000) Extname(NDIVA00F)
011700110902     D $X              S              5  0
011800000000     D*----------
011900000000     D StringaSql      DS          5000
012000170529     D SqlCST          S           5000
012100000000     D SqlIVA          S           5000
012200110902     D SqlREG_MOV      S           5000
012300110902     D SqlREG_MOVDS    DS          5000
012400110902     D SqlREG_MOV1             1   1000
012500110902     D SqlREG_MOV2          1001   2000
012600110902     D SqlREG_MOV3          2001   3000
012700110902     D SqlREG_MOV4          3001   4000
012800110902     D SqlREG_MOV5          4001   5000
012900000000     D*----------
013000000000     D* Indice DsMultipla
013100000000     D $Ind            S              4  0
013200000000     D*---------
013300170615     D* per prtf SDGSI52P1 - errori
013400000000     D $TpErr1         S              2A   Inz
013500000000     D*---------
013600170619     D* per prtf SDGSI52P2 - autofatture
013700000000     D SAVSys          S                   Like(IVASys    )
013800000000     D SAVNrAsReg      S                   Like(IVANrAsReg)
013900000000     D $Segno          S              1  0 Inz
014000000000     D*---------
014100000000     D* per reperimento tipologia operazione da contropartita
014200000000     D SAV2Sys         S                   Like(IVASys    )
014300000000     D SAV2NrAsReg     S                   Like(IVANrAsReg)
014400170724     D*---------
014500170724     D* per controllo dati movimento su NDMOV00F
014600170724     D* ed esistenza SDGCST e quindi scrittura o non scrittura
014700170724     D SAV3Sys         S                   Like(IVASys    )
014800170724     D SAV3NrAsReg     S                   Like(IVANrAsReg)
014900170724     D*---------
015000170724     D* per reperimento legame bolla/fattura
015100170724     D SAV4Sys         S                   Like(IVASys    )
015200170724     D SAV4NrAsReg     S                   Like(IVANrAsReg)
015300170724     D*---------
015400170724     D* per controllo catalogo già scritto in SDGCSF00F
015500170731     D SAV5Societ      S                   Like(CSTSocieta)
015600170731     D SAV5TpCom       S                   Like(CSTTpCom  )
015700170731     D SAV5TpRec       S                   Like(CSTTpRec  )
015800170731     D SAV5Anno        S                   Like(CSTAnno   )
015900170731     D SAV5Trim        S                   Like(CSTTrim   )
016000170731     D SAV5Mese        S                   Like(CSTMese   )
016100170731     D SAV5PrgEst      S                   Like(CSTPrgEstr)
016200170810fm2  D*---------
016300000000     D*------------------
016400000000     D* Controlli validità legende
016500000000     D X07ValDS      E DS
016600000000     D*-------------
016700170616     D* Parametri modulo SDSPETRI
016800170616     D SDGPA12DS     E DS
016900000000     D*-------------
017000170703     D* Tabella degli Stati con codici ISO 3Num e 2Alpha
017100170703     D SDGHD4DS      E DS
017200170703     D*-------------
017300170703     D* Tabella degli Stati
017400170703     D ANGG12DS      E DS
017500170811fm3  D*-------------
017600170811 "   D* Tabella degli Stati  INTRA
017700170811fm3  D ANGW06DS      E DS
017800130903     D*-------------
017900000000     D NDMOV00F      E DS                  INZ
018000170724     D* contiene i dati del movimento cliente/fornitore
018100170724     D NDMOVSav        S                   Like(NDMOV00F)
018200170724     D* contiene i dati del movimento di contropartita
018300170724     D NDMOVSav2       S                   Like(NDMOV00F)
018400000000     D*---------
018500000000     D NDREG00F      E DS                  INZ
018600170724     D* contiene i dati di registraz. del movimento cliente/fornitore
018700170724     D NDREGSav        S                   Like(NDREG00F)
018800170724     D*---------
018900170724     D NDPAS00F      E DS                  INZ
019000170724     D* contiene i dati di partita del movimento cliente/fornitore
019100170724     D NDPASSav        S                   Like(NDPAS00F)
019200000000     D*---------
019300000000     D* Definizioni parametri standard chiamate moduli
019400000000     D $Esito          S              1
019500000000     D $GesMsg         S              1
019600000000     D $LenOut         S              9B 0
019700000000     D $Struttura      S             10
019800170724     D $CodOper        S              1
019900000000     D $Output         S           4000
020000000000     D $Refresh        S              1
020100000000     D $Uso            S              1
020200170724     D*-------------
020300170724     D $CodOperMOV     S              1
020400170724     D $CodOperREG     S              1
020500170724     D $CodOperCP      S              1
020600000000     D*-------------
020700130918     D WStato          S                   Like(XScSta )
020800170718     D WIdPaese        S                   Like(CSTIdPaes1)
020900000000     D*---------
021000000000     D* dati conto
021100000000     D ND002DS       E DS
021200110902     D Attributi             131    250
021300110902     D*-------------
021400110902     D DsAttributi     DS           120
021500110902     D SkAttr                         4    Dim(30)
021600000000     D*-------------
021700000000     D* Ds per reperimento dati su causali operative contabili
021800000000     D ANOPE         E DS                  ExtName(ANOPE00F) Inz
021900000000     D*---------
022000000000     D* Dati soggetti
022100000000     D DVASog        E DS
022200000000     D*---------
022300000000     D* driver soggetti
022400000000     D DVARic          S              1
022500000000     D DVASocieta      S              3
022600000000     D DVAUnita        S              8
022700000000     D DVADtRif        S               D
022800000000     D DVAStrutt       S             10
022900000000     D DVALenOut       S              9B 0
023000000000     D DVASnatura      S              1
023100000000     D DVACodice       S              8
023200000000     D DVALineav       S              3
023300000000     D DVAFiliale      S              3  0
023400000000     D DVASogg         S              8
023500000000     D DVATpInd        S              2
023600000000     D DVACdInd        S              3
023700000000     D DVAOutput       S           4000
023800000000     D DVAErrore       S              1
023900000000     D*---------
024000000000     D* Richiamo a SDGTABDR
024100000000     D SDGTABDS      E DS
024200000000     D*---------
024300000000     D* Contabilità
024400000000     D ANGG84DS      E DS                  INZ
024500170801     D*-------------
024600170801     D* Recupero legame bolla doganale/fattura
024700170801     D SDGSG54DS     E DS                  INZ
024800170717     D*-------------
024900170717     D* Passaggio Parametri al PGM di recupero dati
025000170717     D* estratti già presenti sul sistema
025100170731     D SDG00703DS    E DS                  INZ
025200170731     D*-------------
025300170731     D* Passaggio Parametri al PGM di recupero dati
025400170731     D* anagrafici
025500170731     D SDG00704DS    E DS                  INZ
025600000000     D*---------
025700000000     D* Campi di lavoro
025800000000     D $NrRigaM        s                   like(IVANrRigaM) inz
025900170529     D $WriteCST       S              1
026000130826     D $PrimaStp1      S              1    INZ(*On)
026100000000     D $PrimaStp2      S              1    INZ(*On)
026200000000     D $PrimaStp3      S              1    INZ(*On)
026300000000     D $OkConto        S              1    INZ('1')
026400000000     D $OkMOV          S              1    INZ('1')
026500000000     D $AnnConto       S              1    INZ('0')
026600000000     D $AnnMOV         S              1    INZ('0')
026700000000     D $AnnIVA         S              1    INZ('0')
026800170810     D $LegameBF       S              1    INZ('0')
026900170810     D $Intra          S              1    INZ('0')
027000170801     D $Margine        S              1    INZ('0')
027100170801     D $Reverse        S              1    INZ('0')
027200170703     D $Differita      S              1    INZ('0')
027300170801     D $Autofatt       S              1    INZ('0')
027400170801     D $AutoCons       S              1    INZ('0')
027500170529     D $TpAnn          S                   Like(CSTTpAnn) INZ(*Blank)
027600170816     D $OkCP_BS        S              1    INZ('1')
027700170816     D $OkBeni         S              1A
027800170816     D $OkServizi      S              1A
027900170816     D $ImpBeni        S                    Like(MOVImporto)
028000170816     D $ImpServizi     S                    Like(MOVImporto)
028100000000     D $OkVoce         S              1    INZ('1')
028200170714     D $NatOpe         S                   Like(CSTNatOpe  )
028300130826     D $ImpDoc         S                   Like(MOVImporto)
028400170801     D $Voce           S                   Like(MOAVoce   )
028500000000     D*---------
028600130924     D $OkRegBenSrv    S              1A   INZ('0')
028700170703     D $OkRegola       S              1A   INZ('0')
028800000000     D*---------
028900000000     D* reperimento dati fiscali anagrafici
029000000000     D SAVKccCP        S                   Like(IVAKccCP   )
029100000000     D SAVKscCP        S                   Like(IVAKscCP   )
029200170727     D SAVDtDoc        S                   Like(MOVDtDoc  )
029300000000     D*---------
029400000000     D* segnalazione  dati fiscali mancanti
029500000000     D SAVKcc2         S                   Like(MOVKcc    )
029600000000     D SAVKsc2         S                   Like(MOVKsc    )
029700000000     D SAVDtDoc2       S                   Like(MOVDtDoc  )
029800170714     D*---------
029900170714     D* Dati anagrafici della società contribuente
030000170720     D $NazSoc         S                   Like(NazG12 )
030100170717     D $PartIVASoc     S                   Like(CSTPartIVA)
030200170714     D $CdFiscSoc      S                   Like(CSTCdFisc )
030300170731     D $Stato1Soc      S                   Like(CSTStato1 )
030400170731     D $IdPaes1Soc     S                   Like(CSTIdPaes1)
030500170714     D $PIVA1Soc       S                   Like(CSTPIVA1  )
030600170714     D $CdFisc1Soc     S                   Like(CSTCdFisc1)
030700170714     D $CogRag1Soc     S                   Like(CSTCogRag1)
030800170725     D $TpN1Soc        S                   Like(CSTTpN1  )
030900170725     D $Denom1Soc      S                   Like(CSTDenom1)
031000170727     D $Cognom1Soc     S                   Like(CSTCognom1)
031100170714     D $Nome1Soc       S                   Like(CSTNome1)
031200170714     D $Indir2Soc      S                   Like(CSTIndir2)
031300170714     D $NrCiv2Soc      S                   Like(CSTNrCiv2)
031400170714     D $CAP2Soc        S                   Like(CSTCAP2  )
031500170714     D $Locali2Soc     S                   Like(CSTLocali2)
031600170714     D $Prov2Soc       S                   Like(CSTProv2 )
031700170714     D $Stato2Soc      S                   Like(CSTStato2)
031800170714     D $IdPaes2Soc     S                   Like(CSTIdPaes2)
031900170720     D $Indir3Soc      S                   Like(CSTIndir3 )
032000170720     D $NrCiv3Soc      S                   Like(CSTNrCiv3 )
032100170720     D $CAP3Soc        S                   Like(CSTCAP3   )
032200170720     D $Locali3Soc     S                   Like(CSTLocali3)
032300170720     D $Prov3Soc       S                   Like(CSTProv3  )
032400170720     D $Stato3Soc      S                   Like(CSTStato3 )
032500170720     D $IdPaes3Soc     S                   Like(CSTIdPaes3)
032600170731     D $Stato4Soc      S                   Like(CSTStato4 )
032700170720     D $IdPaes4Soc     S                   Like(CSTIdPaes4)
032800170720     D $PIVA4Soc       S                   Like(CSTPIVA4  )
032900170720     D $CogRag4Soc     S                   Like(CSTCogRag4)
033000170725     D $TpN4Soc        S                   Like(CSTTpN4  )
033100170725     D $Denom4Soc      S                   Like(CSTDenom4)
033200170727     D $Cognom4Soc     S                   Like(CSTCognom4)
033300170720     D $Nome4Soc       S                   Like(CSTNome4  )
033400170720     D*------
033500170714     D* Dati anagrafici del soggetto cliente/fornitore
033600170714     D $PartIVASog     S                   Like(CSTPartIVA)
033700170714     D $CdFiscSog      S                   Like(CSTCdFisc )
033800170731     D $Stato1Sog      S                   Like(CSTStato1 )
033900170731     D $IdPaes1Sog     S                   Like(CSTIdPaes1)
034000170714     D $PIVA1Sog       S                   Like(CSTPIVA1  )
034100170714     D $CdFisc1Sog     S                   Like(CSTCdFisc1)
034200170714     D $CogRag1Sog     S                   Like(CSTCogRag1)
034300170725     D $TpN1Sog        S                   Like(CSTTpN1  )
034400170725     D $Denom1Sog      S                   Like(CSTDenom1)
034500170727     D $Cognom1Sog     S                   Like(CSTCognom1)
034600170714     D $Nome1Sog       S                   Like(CSTNome1)
034700170714     D $Indir2Sog      S                   Like(CSTIndir2)
034800170714     D $NrCiv2Sog      S                   Like(CSTNrCiv2)
034900170714     D $CAP2Sog        S                   Like(CSTCAP2  )
035000170714     D $Locali2Sog     S                   Like(CSTLocali2)
035100170714     D $Prov2Sog       S                   Like(CSTProv2 )
035200170714     D $Stato2Sog      S                   Like(CSTStato2)
035300170714     D $IdPaes2Sog     S                   Like(CSTIdPaes2)
035400170720     D $Indir3Sog      S                   Like(CSTIndir3 )
035500170720     D $NrCiv3Sog      S                   Like(CSTNrCiv3 )
035600170720     D $CAP3Sog        S                   Like(CSTCAP3   )
035700170720     D $Locali3Sog     S                   Like(CSTLocali3)
035800170720     D $Prov3Sog       S                   Like(CSTProv3  )
035900170720     D $Stato3Sog      S                   Like(CSTStato3 )
036000170720     D $IdPaes3Sog     S                   Like(CSTIdPaes3)
036100170731     D $Stato4Sog      S                   Like(CSTStato4 )
036200170720     D $IdPaes4Sog     S                   Like(CSTIdPaes4)
036300170720     D $PIVA4Sog       S                   Like(CSTPIVA4  )
036400170720     D $CogRag4Sog     S                   Like(CSTCogRag4)
036500170725     D $TpN4Sog        S                   Like(CSTTpN4  )
036600170725     D $Denom4Sog      S                   Like(CSTDenom4)
036700170727     D $Cognom4Sog     S                   Like(CSTCognom4)
036800170720     D $Nome4Sog       S                   Like(CSTNome4  )
036900000000     D*------
037000170731     D* Dati anagrafici del cliente/fornitore (considerato il movimento)
037100170731     D $PartIVAMov     S                   Like(CSTPartIVA)
037200170731     D $CdFiscMov      S                   Like(CSTCdFisc )
037300170731     D $Stato1Mov      S                   Like(CSTStato1 )
037400170731     D $IdPaes1Mov     S                   Like(CSTIdPaes1)
037500170731     D $PIVA1Mov       S                   Like(CSTPIVA1  )
037600170731     D $CdFisc1Mov     S                   Like(CSTCdFisc1)
037700170731     D $CogRag1Mov     S                   Like(CSTCogRag1)
037800170731     D $TpN1Mov        S                   Like(CSTTpN1  )
037900170731     D $Denom1Mov      S                   Like(CSTDenom1)
038000170731     D $Cognom1Mov     S                   Like(CSTCognom1)
038100170731     D $Nome1Mov       S                   Like(CSTNome1)
038200170731     D $Indir2Mov      S                   Like(CSTIndir2)
038300170731     D $NrCiv2Mov      S                   Like(CSTNrCiv2)
038400170731     D $CAP2Mov        S                   Like(CSTCAP2  )
038500170731     D $Locali2Mov     S                   Like(CSTLocali2)
038600170731     D $Prov2Mov       S                   Like(CSTProv2 )
038700170731     D $Stato2Mov      S                   Like(CSTStato2)
038800170731     D $IdPaes2Mov     S                   Like(CSTIdPaes2)
038900170731     D $Indir3Mov      S                   Like(CSTIndir3 )
039000170731     D $NrCiv3Mov      S                   Like(CSTNrCiv3 )
039100170731     D $CAP3Mov        S                   Like(CSTCAP3   )
039200170731     D $Locali3Mov     S                   Like(CSTLocali3)
039300170731     D $Prov3Mov       S                   Like(CSTProv3  )
039400170731     D $Stato3Mov      S                   Like(CSTStato3 )
039500170731     D $IdPaes3Mov     S                   Like(CSTIdPaes3)
039600170731     D $Stato4Mov      S                   Like(CSTStato4 )
039700170731     D $IdPaes4Mov     S                   Like(CSTIdPaes4)
039800170731     D $PIVA4Mov       S                   Like(CSTPIVA4  )
039900170731     D $CogRag4Mov     S                   Like(CSTCogRag4)
040000170731     D $TpN4Mov        S                   Like(CSTTpN4  )
040100170731     D $Denom4Mov      S                   Like(CSTDenom4)
040200170731     D $Cognom4Mov     S                   Like(CSTCognom4)
040300170731     D $Nome4Mov       S                   Like(CSTNome4  )
040400000000     D*-------------
040500170724     D* campi di lavoro per la chiamata ai driver anagrafici
040600000000     D WSogg           S                   Like(DVASogg)
040700000000     D WKsc            S                   Like(DVACodice)
040800170529     D WKcc            S                   Like(CSTKcc)
040900170724     D WData           S               D
041000170724     D WDtAlfa         S             10A
041100170721     D*-------------
041200000000     D $FineMOA        S              1    Inz(*Off)
041300000000     D $FineIVA        S              1    Inz(*Off)
041400110902     D $FineREG_MOV    S              1    Inz(*Off)
041500170529     D $FineCST        S              1    Inz(*Off)
041600000000     D $Causale        S                   Like(MOVCausTes)
041700000000     D SAVVoce         S                   Like(MOAVoce   )
041800170801     D $PrimoCST       S              1A   Inz(*On)
041900000000     D*---------
042000000000     D DataDs          DS
042100000000     D  AnnoDs                        4A
042200000000     D   Tratt1                       1A   Inz('-')
042300000000     D  MeseDs                        2A
042400000000     D   Tratt2                       1A   Inz('-')
042500000000     D  GiornoDs                      2A
042600000000     D*----------
042700000000     D $Data10A1       S             10
042800000000     D $Data10A2       S             10
042900111018     D $DataAnno       S               D
043000170706     D $DtRif          S               D
043100000000     D $Anno           S              4A
043200170616     D $Trim           S              1A
043300170619     D $Mese           S              2A
043400000000     D*-------------
043500000000     D UDateISO        S               D    datfmt(*iso)
043600130826     D $KscIncl        S              1A
043700130826     D $KscEscl        S              1A
043800170816fm6  D $OkCP_Incl      S              1    INZ('1')
043900110902     D $ChkNDIVA       S              1A
044000130826     D*-------------
044100170615     D*-------------
044200170721     D* Sottotitolo di testata
044300170721     D SOTTITTT        C                   CONST('               -
044400170721     D                                                 ')
044500170728     D*---------
044600170728     D* Lista caratteri da convertire e ammessi (Code Table Basic Latin) ASCII
044700170807     D CnvErr          C                   Const('ÁÃÉÍÑÓÚ[ÀÈÒÙÌ&')
044800170807     D CnvOk           C                   Const('AAEINOU AEOUIE')
044900170807     D*CharOK          C                   Const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
045000170807     D*******                                     abcdefghijklmnopqrstuvwxyz-
045100170807     D*******                                     01234567890/+-?:().,'''' -
045200170807     D*******                                     !#$%&*+-;<=>@[\]^_`{|}~')
045300170807     D CharOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
045400170807     D                                            abcdefghijklmnopqrstuvwxyz-
045500170807     D                                            01234567890/+-?:().,'''' ')
045600170728     D $FineScan       S              1    Inz(*Off)
045700170728     D $CampoScan      S            100A
045701170906
045702170906     D YSDG000R        PR                  EXTPGM('YSDG000R')
045703170906     D cognome                       60
045704170906     D o_nome                        60
045706170906
045800000000     C******************************************************
045900000000     C*                MAIN LINE PROGRAM
046000000000     C******************************************************
046100000000     C*
046200000000     C                   EXSR      INZVAR
046300000000     C*
046400170616     C                   Clear                   $CF               1
046500170616     C* eseguo 2 cicli se chiesti sia Clienti che Fornitori
046600170616     C* questa distinzione di estrazione serve per definire
046700170616     C* il range di date corretto (data documento, registrazione
046800170616     C* o annotazione)
046900170616     C                   if        CFI52      = ' '    or
047000170616     C                             CFI52      = 'C'
047100170616     C                   Eval      $CF        = 'C'
047200000000     C                   EXSR      CicloIVA
047300110902     C*
047400110902     C* recupero le importazioni senza movimenti NDIVA00F
047500110902     C                   EXSR      CicloREG_MOV
047600170616     C*
047700170616     C                   EXSR      CloseSql
047800170616     C                   endif
047900170616     C*
048000170616     C                   if        CFI52      = ' '    or
048100170616     C                             CFI52      = 'F'
048200170616     C                   Eval      $CF        = 'F'
048300170616     C                   EXSR      CicloIVA
048400170616     C*
048500170616     C* recupero le importazioni senza movimenti NDIVA00F
048600170616     C                   EXSR      CicloREG_MOV
048601170831R2654C*
048602170831R2654C                   EXSR      CloseSql
048700170616     C                   endif
048800000000     C*
048900000000     C                   EXSR      ENDPGM
049000110902     C*
049100000000     C************************************************************
049200000000     C* Fine programma
049300000000     C************************************************************
049400000000     C     ENDPGM        BEGSR
049500000000     C*
049600000000     C                   EXSR      CloseSql
049700000000     C*
049800170615     C                   MoveL     SDGSI52DS     KPJBU
049900000000     C*
050000170615     C                   if        %open (SDGSI52P1)
050100170615     C                   Close     SDGSI52P1
050200110720     C                   endif
050300000000     C                   Eval       *InLR      = *On
050400000000     C*
050500000000     C                   Return
050600000000     C*
050700000000     C                   ENDSR
050800000000     C/EJECT
050900000000     C************************************************************
051000000000     C* Ciclo di lettura di NDIVA00F
051100000000     C************************************************************
051200000000     C     CicloIVA      BEGSR
051300000000     C*
051400000000     C                   EXSR      RepSqlIVA
051500000000     C*
051600000000     C                   EXSR      OpenSqlIVA
051700000000     C*
051800000000     C                   EXSR      ExeSqlIVA
051900000000     C*
052000000000     C                   ENDSR
052100000000     C/EJECT
052200000000     C************************************************************
052300000000     C* Reperimento stringa sql su NDIVA00F
052400000000     C************************************************************
052500000000     C     RepSqlIVA     BEGSR
052600000000     C*
052700000000     C* SELECT * FROM NDIVA00F
052800000000     C* WHERE IVASOCIETA='XXX'
052900170616     C*  [se richiesta data di documento, solo Clienti ]
053000170616     C*  AND IVADTDOC BETWEEN 'ANNOI52-01-01' AND 'ANNOI52-03-31'
053100170616     C*  [se richiesta data di registrazione, sia Clienti che Fornitori ]
053200170616     C*  AND IVADTREG BETWEEN 'ANNOI52-01-01' AND 'ANNOI52-03-31'
053300170707     C*  [se richiesta data di annotazione, solo Clienti ]
053400170616     C*  AND IVADTANNOT BETWEEN 'ANNOI52-01-01' AND 'ANNOI52-03-31'
053500000000     C* -------------------------------------------------------------------
053600000000     C*  [se capoconti impostati]
053700170615     C*  and IVAKCCCP in (Kcc1I52, Kcc2I52,
053800170615     C*                   Kcc3I52, Kcc4I52,
053900170615     C*                   Kcc5I52, Kcc6I52 )
054000000000     C*  [se conto impostato]
054100170615     C*  and IVAKCCCP = KccI52  and IVAKscCP = KscI52
054200000000     C*
054300000000     C* -------------------------------------------------------------------
054400110826     C* ORDER BY IVAKCCCP, IVAKSCCP, IVADTREG, IVATPREG, IVASYS, IVANRASREG,
054500110826     C* IVANRRIGAM, IVANRRIGA  FOR FETCH ONLY
054600000000     C*
054700000000     C                   Clear                   SqlIVA
054800000000     C*
054900000000     C                   Eval      SqlIVA =
055000000000     C                              'SELECT * FROM NDIVA00F ' +
055100000000     C                             ' WHERE IVASOCIETA=' + '''' +
055200170615     C                             SocietaI52 + ''''
055300000000     C*
055400170616     C*
055500000000     C                   EXSR      RepPeriodo
055600000000     C*
055700170616     C                   Select
055800170616     C* se richiesta data registrazione
055900170616     C                   When      ($CF        = 'C'   and
056000170616     C                              DtRifCA12  = 'R'      )
056100170616     C                             OR
056200170707     C                             $CF        = 'F'
056300000000     C                   Eval      SqlIVA      = %trimr(SqlIVA) +
056400000000     C                             ' AND (IVADtReg'
056500170616     C*
056600170616     C* se richiesta data documento
056700170616     C                   When      ($CF        = 'C'   and
056800170616     C                              DtRifCA12  = 'D'      )
056900170616     C                   Eval      SqlIVA      = %trimr(SqlIVA) +
057000170616     C                             ' AND (IVADtDoc'
057100170616     C*
057200170616     C* se richiesta data annotazione
057300170706     C                   When      ($CF        = 'C'   and
057400170706     C                              DtRifCA12  = 'A'      )
057500170616     C                   Eval      SqlIVA      = %trimr(SqlIVA) +
057600170616     C                             ' AND (IVADtAnnot'
057700170616     C                   endSl
057800000000     C*
057900000000     C                   Eval      SqlIVA      = %trimr(SqlIVA) +
058000000000     C                             ' BETWEEN ''' + $Data10A1 +
058100000000     C                             ''' AND ''' + $Data10A2 + ''''
058200000000     C                             + ' ) '
058300000000     C*
058400170616     C*
058500170615     C                   if        Kcc1I52    <> *Blank   or
058600170615     C                             Kcc2I52    <> *Blank   or
058700170615     C                             Kcc3I52    <> *Blank   or
058800170615     C                             Kcc4I52    <> *Blank   or
058900170615     C                             Kcc5I52    <> *Blank   or
059000170615     C                             Kcc6I52    <> *Blank
059100000000     C*
059200000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
059300000000     C                             ' AND IVAKccCP   in (' + '''' +
059400170615     C                             Kcc1I52 + ''''  +
059500000000     C                             ' , '   + ''''  +
059600170615     C                             Kcc2I52 + ''''  +
059700000000     C                             ' , '   + ''''  +
059800170615     C                             Kcc3I52 + ''''  +
059900000000     C                             ' , '   + ''''  +
060000170615     C                             Kcc4I52 + ''''  +
060100000000     C                             ' , '   + ''''  +
060200170615     C                             Kcc5I52 + ''''  +
060300000000     C                             ' , '   + ''''  +
060400170615     C                             Kcc6I52 + ''''  +
060500000000     C                             ' ) '
060600000000     C                   endif
060700000000     C*
060800000000     C*
060900170615     C                   if        KccI52     <> *Blank
061000000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
061100000000     C                             ' AND IVAKccCP  =' + '''' +
061200170615     C                             KccI52 + ''''
061300000000     C                   endif
061400000000     C*
061500170615     C                   if        KscI52     <> *Blank
061600000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
061700000000     C                             ' AND IVAKscCP =' + '''' +
061800170615     C                             KscI52 + ''''
061900000000     C                   endif
062000000000     C*
062100000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
062200000000     C                               ' ORDER BY IVAKCCCP, '+
062300000000     C                               ' IVAKSCCP, IVADTREG,' +
062400000000     C                               ' IVATPREG,' +
062500110826     C                               ' IVASYS, IVANRASREG,' +
062600110826     C                               ' IVANRRIGAM, IVANRRIGA'
062700000000     C*
062800000000     C* For Fetch Only
062900000000     C                   Eval      SqlIVA      = %trimr(SqlIVA)+
063000000000     C                               ' FOR FETCH ONLY'
063100000000     C*
063200000000     C                   ENDSR
063300000000     C/EJECT
063400000000     C******************************************************
063500000000     C* Reperisco le date di inizio e fine periodo
063600000000     C******************************************************
063700000000     C     RepPeriodo    BEGSR
063800000000     C*
063900000000     C                   Reset                   DataDs
064000000000     C                   Clear                   $Data10A1
064100170616     C                   Clear                   $MeseI            2 0
064200170616     C                   Clear                   $MeseF            2 0
064300000000     C*
064400170616     C* recupero il mese di inizio trimestre e
064500170616     C*          il mese di fine   trimestre
064600170616     C                   Select
064700170616     C*
064800170616     C                   When      TrimI52     = 1
064900170616     C                   Eval      $MeseI      = 1
065000170616     C                   Eval      $MeseF      = 3
065100170616     C*
065200170616     C                   When      TrimI52     = 2
065300170616     C                   Eval      $MeseI      = 4
065400170616     C                   Eval      $MeseF      = 6
065500170616     C*
065600170616     C                   When      TrimI52     = 3
065700170616     C                   Eval      $MeseI      = 7
065800170616     C                   Eval      $MeseF      = 9
065900170616     C*
066000170616     C                   When      TrimI52     = 4
066100170616     C                   Eval      $MeseI      = 10
066200170616     C                   Eval      $MeseF      = 12
066300170616     C*
066400170616     C                   endSl
066500170616     C*
066600170616     C* Periodo Da
066700170616     C                   MoveL     AnnoI52       AnnoDs
066800170616     C                   MoveL     $MeseI        MeseDs
066900170616     C                   MoveL     '01'          GiornoDs
067000170616     C                   Move      DataDs        $Data10A1
067100170616     C*
067200170616     C* Periodo A
067300170724     C                   Clear                   WData
067400170616     C                   Reset                   DataDs
067500170616     C                   Clear                   $Data10A2
067600170616     C*
067700170616     C                   MoveL     AnnoI52       AnnoDs
067800170616     C                   MoveL     $MeseF        MeseDs
067900170616     C                   MoveL     '01'          GiornoDs
068000170724     C                   Move      DataDs        WData
068100170616     C*
068200170616     C* reperisco il fine mese
068300170616     C                   CALL      'XDATCD'
068400170616     C                   PARM      4             XCod              2 0
068500170724     C                   PARM                    WData
068600170616     C*
068700170724     C                   Move      WData         $Data10A2
068800111018     C*
068900000000     C                   ENDSR
069000000000     C/EJECT
069100000000     C******************************************************
069200000000     C* Open cursore sql
069300000000     C******************************************************
069400000000     C     OpenSqlIVA    BEGSR
069500000000     C*
069600180302     C/EXEC SQL
069700180302     C+     PREPARE S1 FROM :SqlIVA
069800180302     C/END-EXEC
069900000000     C*
070000000000     C/EXEC SQL
070100000000     C+     Declare A1 cursor for S1
070200000000     C/END-EXEC
070300000000     C*
070400000000     C/EXEC SQL
070500000000     C+     OPEN A1
070600000000     C/END-EXEC
070700000000     C*
070800000000     C                   ENDSR
070900000000     C/EJECT
071000000000     C************************************************************
071100000000     C* Eseguo lettura dei movimenti IVA
071200000000     C************************************************************
071300000000     C     ExeSqlIVA     BEGSR
071400000000     C*
071500000000     C                   Move      *Off          $FineIVA
071600000000     C                   Z-Add     0             $Ind
071700000000     C                   Z-Add     0             SqlEr3
071800000000     C*
071900000000     C                   DoW       $FineIVA    = *Off
072000000000     C*
072100000000     C                   EXSR      FetchIVA
072200000000     C*
072300000000     C                   Select
072400000000     C*-----------------------
072500000000     C* OK
072600000000     C                   When      SqlCod     >= 0     and
072700000000     C                             SqlCod     <> 100
072800000000     C*
072900000000     C     $Ind          OCCUR     NDIVADS
073000170616     C*
073100170724     C                   EXSR      RepNDMOV
073200170616     C*
073300170616     C                   if        MOVSNatura  = $CF
073400170724     C*
073500170724     C                   EXSR      RepNDREG
073600170724     C*
073700170616     C                   Eval      $ChkNDIVA   = *On
073800000000     C                   EXSR      ElabIVA
073900170616     C                   endif
074000000000     C*
074100000000     C*-----------------------
074200000000     C* fine lettura
074300000000     C                   When      SqlCod      = 100
074400000000     C*
074500000000     C                   COMMIT
074600170724     C                   FEOD      SDGCST00F
074700000000     C*
074800000000     C* Stampa autofatture ultima riga totali
074900000000     C                   EXSR      StpAutof_Tot
075000000000     C*
075100000000     C                   Move      *On           $FineIVA
075200000000     C*
075300000000     C                   Other
075400000000     C*
075500000000     C*-----------------------
075600000000     C* si è verificato un errore
075700000000     C                   Eval      $TpErr1     = '99'
075800000000     C                   Eval      StringaSql  = SqlIVA
075900000000     C                   EXSR      StpErr1
076000170615     C                   Eval      ERRI52      = '1'
076100000000     C*
076200000000     C                   Move      *On           $FineIVA
076300000000     C                   endsl
076400000000     C*
076500000000     C                   endDo
076600000000     C*
076700000000     C                   ENDSR
076800000000     C/EJECT
076900000000     C******************************************************
077000000000     C* Eseguo lettura
077100000000     C******************************************************
077200000000     C     FetchIVA      BEGSR
077300000000     C*
077400000000     C     $Ind          Ifeq      SqlEr3
077500000000     C/EXEC SQL
077600000000     C+     FETCH A1 FOR 1000 ROWS INTO :NDIVADS
077700000000     C/END-EXEC
077800000000     C                   Z-ADD     1             $Ind
077900000000     C                   else
078000000000     C                   ADD       1             $Ind
078100000000     C                   endif
078200000000     C*
078300000000     C                   ENDSR
078400000000     C/EJECT
078500110902     C************************************************************
078600110902     C* Ciclo di lettura di NDREG00F/NDMOV00F
078700110902     C************************************************************
078800110902     C     CicloREG_MOV  BEGSR
078900110902     C*
079000110902     C                   EXSR      ClrNDIVADS
079100110902     C*
079200110902     C                   EXSR      RepSqlREG_MOV
079300110902     C*
079400110902     C                   EXSR      OpenSqlREG_MOV
079500110902     C*
079600111018     C                   EXSR      ExeSqlREG_MOV
079700110902     C*
079800110902     C                   ENDSR
079900110902     C/EJECT
080000110902     C************************************************************
080100110902     C* Clear NDIVADS
080200110902     C************************************************************
080300110902     C     ClrNDIVADS    BEGSR
080400110902     C*
080500110902     C* Pulisco la ds multipla
080600110902     C*
080700110902     C     1             Do        1000          $X
080800110902     C     $X            Occur     NDIVADS
080900110902     C                   Clear                   NDIVADS
081000110902     C                   endDo
081100110902     C*
081200110902     C                   ENDSR
081300110902     C/EJECT
081400110902     C************************************************************
081500110902     C* Reperimento stringa sql su NDREG00F e NDMOV00F
081600110902     C************************************************************
081700110902     C     RepSqlREG_MOV BEGSR
081800110902     C*
081900110902     C* SELECT
082000110902     C* REGNRASREG  IVANRASREG  ,
082100110902     C* MOVNRRIGAM  IVANRRIGAM  ,
082200110902     C* 1           IVANRRIGA   ,
082300110902     C* REGSYS      IVASYS      ,
082400110902     C* REGSOCIETA  IVASOCIETA  ,
082500110902     C* REGUNITA    IVAUNITA    ,
082600110902     C* ' '         IVAKCC      ,
082700110902     C* ' '         IVAKSC      ,
082800110902     C* REGDTREG    IVADTREG    ,
082900110902     C* REGNRREG    IVANRREG    ,
083000110902     C* REGSERREG   IVASERREG   ,
083100110902     C* MOVCAUSTES  IVACAUSTES  ,
083200110902     C* MOVDTDOC    IVADTDOC    ,
083300110902     C* MOVNRDOC    IVANRDOC    ,
083400110902     C* MOVSERDOC   IVASERDOC   ,
083500110902     C* ' '         IVACAUSRIG  ,
083600110902     C* '0001-01-01'  IVADTRIF  ,
083700110902     C* MOVAVERE    IVADARE     ,
083800110902     C* MOVDARE     IVAAVERE    ,
083900110902     C* 0           IVAIMPORTO  ,
084000110902     C* 0           IVAIMPORTD  ,
084100110902     C* MOVIMPORTO  IVAIMPONIB  ,
084200110902     C* MOVIMPORTD  IVAIMPOND   ,
084300110902     C* MOVKCC      IVAKCCCP    ,
084400110902     C* MOVKSC      IVAKSCCP    ,
084500110902     C* '0001-01-01'  IVADTCOMP ,
084600110902     C* ' '         IVADESBREV  ,
084700170807     C* case when MOVSNATURA = 'F' then '1'
084800170807     C* else '2' end as IVATPREG,
084900110902     C* ' '         IVALIBRO    ,
085000110902     C* 0           IVAALIQ     ,
085100110902     C* ' '         IVARIFIVA   ,
085200110902     C* ' '         IVAINDETR   ,
085300110902     C* ' '         IVASOSP     ,
085400170718     C* MOVDTREG      IVADTANNOT,
085500110902     C* ' '          IVAFLAG5A
085600110902     C*
085700110902     C* FROM NDREG00F join NODMOV00F on
085800110902     C* REGSYS = MOVSYS  and
085900110902     C* REGNRASREG = MOVNRASREG
086000110902     C* EXCEPTION JOIN NDIVA00F on
086100110902     C* REGSYS = IVASYS  and
086200110902     C* REGNRASREG = IVANRASREG
086300110902     C* WHERE REGSOCIETA= 'XXX'    and
086400110902     C*       REGANN    = '0'      and
086500110902     C*       REGDOCIVA = '1'      and
086600170616     C*  [se lettura   Fornitori ]
086700110902     C*    MOVSNATURA   = 'F'      and
086800170616     C*  [se lettura   Clienti   ]
086900170616     C*    MOVSNATURA   = 'C'      and
087000110902     C*    MOVNRRIGAM   =  1       and
087100170616     C*  [se richiesta data di documento, solo Clienti ]
087200170616     C*  AND REGDTDOC BETWEEN 'ANNOI52-01-01' AND 'ANNOI52-03-31'
087300170616     C*  [se richiesta data di registrazione, sia Clienti che Fornitori ]
087400170616     C*  AND REGDTREG BETWEEN 'ANNOI52-01-01' AND 'ANNOI52-03-31'
087500170619     C*  [se richiesta data di annotazione, posso usare solo la data di
087600170707     C*   registrazione, solo Clienti ]
087700170619     C*  AND REGDTREG BETWEEN 'ANNOI52-01-01' AND 'ANNOI52-03-31'
087800110902     C* -------------------------------------------------------------------
087900110902     C*  [se capoconti impostati]
088000170615     C*  and REGKCC   in (Kcc1I52, Kcc2I52,
088100170615     C*                   Kcc3I52, Kcc4I52,
088200170615     C*                   Kcc5I52, Kcc6I52 )
088300110902     C*  [se conto impostato]
088400170615     C*  and MOVKCC   = KccI52  and MOVKsc   = KscI52
088500110902     C*
088600110902     C* -------------------------------------------------------------------
088700170801     C* ORDER BY IVAKCCCP, IVAKSCCP, IVADTREG, IVASYS, IVANRASREG
088800170801     C* FOR FETCH ONLY
088900110902     C*
089000110902     C                   Clear                   SqlREG_MOV
089100110902     C*
089200110902     C                   Eval      SqlREG_MOV =
089300110902     C                              'Select  ' +
089400110902     C                              'REGNRASREG  IVANRASREG, ' +
089500110902     C                              'MOVNRRIGAM  IVANRRIGAM, ' +
089600110902     C                              '1           IVANRRIGA,  ' +
089700110902     C                              'REGSYS      IVASYS,     ' +
089800110902     C                              'REGSOCIETA  IVASOCIETA, ' +
089900110902     C                              'REGUNITA    IVAUNITA,   ' +
090000110902     C                              '''' + '''' +  ' IVAKCC,     ' +
090100110902     C                              '''' + '''' +  ' IVAKSC,     ' +
090200110902     C                              'REGDTREG    IVADTREG,   ' +
090300110902     C                              'REGNRREG    IVANRREG,   ' +
090400110902     C                              'REGSERREG   IVASERREG,  ' +
090500110902     C                              'MOVCAUSTES  IVACAUSTES, ' +
090600110902     C                              'MOVDTDOC    IVADTDOC,   ' +
090700110902     C                              'MOVNRDOC    IVANRDOC,   ' +
090800110902     C                              'MOVSERDOC   IVASERDOC,  ' +
090900110902     C                              '''' + '''' +  ' IVACAUSRIG, ' +
091000110902     C                              '''' + '0001-01-01' + '''' +
091100110902     C                                ' IVADTRIF,  ' +
091200110902     C                              'MOVAVERE    IVADARE,    ' +
091300110902     C                              'MOVDARE     IVAAVERE,   ' +
091400110902     C                              '0           IVAIMPORTO, ' +
091500110902     C                              '0           IVAIMPORTD, ' +
091600110902     C                              'MOVIMPORTO  IVAIMPONIB, ' +
091700110902     C                              'MOVIMPORTD  IVAIMPOND,  ' +
091800110902     C                              'MOVKCC      IVAKCCCP,   ' +
091900110902     C                              'MOVKSC      IVAKSCCP,   ' +
092000110902     C                              '''' + '0001-01-01' + '''' +
092100110902     C                                ' IVADTCOMP, ' +
092200130826     C                              '''' + '''' +  ' IVADESBREV, ' +
092300170807     C                              'case when MOVSNATURA = ''F''then ' +
092400170807     C                              '''' + '1' + '''' +
092500170807     C                              'else '+
092600170807     C                              '''' + '2' + '''' +
092700170807     C                              ' end as IVATPREG,  ' +
092800130826     C                              '''' + '''' +  ' IVALIBRO,   ' +
092900110902     C                              '0           IVAALIQ,    ' +
093000110902     C                              '''' + '''' +  ' IVARIFIVA,  ' +
093100110902     C                              '''' + '''' +  ' IVAINDETR,  ' +
093200110902     C                              '''' + '''' +  ' IVASOSP,    ' +
093300170718     C                              'MOVDTREG    IVADTANNOT, ' +
093400110902     C                              '''' + '''' +  ' IVAFLAG5A   ' +
093500110902     C                              'from NDREG00F          ' +
093600110902     C                              'JOIN NDMOV00F  on      ' +
093700110902     C                              'REGSYS = MOVSYS  and   ' +
093800110902     C                              'REGNRASREG = MOVNRASREG ' +
093900110902     C                              'EXCEPTION JOIN NDIVA00F on ' +
094000110902     C                              'REGSYS = IVASYS  and   ' +
094100110902     C                              'REGNRASREG = IVANRASREG ' +
094200110902     C                             ' WHERE REGSOCIETA=' + '''' +
094300170615     C                             SocietaI52 + ''''
094400110902     C*
094500110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
094600110902     C                             ' AND  REGAnn    =' + '''' +
094700110902     C                             '0' + ''''
094800110902     C*
094900110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
095000110902     C                             ' AND  REGDocIVA =' + '''' +
095100110902     C                             '1' + ''''
095200170616     C*
095300110902     C*
095400170616     C                   Select
095500170616     C*
095600170616     C                   When      $CF         = 'F'
095700170616     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
095800170616     C                             ' AND  MOVSNatura=' + '''' +
095900170616     C                             'F' + ''''
096000170616     C*
096100170616     C                   When      $CF         = 'C'
096200170616     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
096300170616     C                             ' AND  MOVSNatura=' + '''' +
096400170616     C                             'C' + ''''
096500170616     C                   endsl
096600110902     C*
096700170616     C*
096800110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
096900110902     C                             ' AND  MOVNRRIGAM= 1 '
097000110902     C*
097100170616     C*
097200110902     C                   EXSR      RepPeriodo
097300170616     C*
097400170616     C                   Select
097500170616     C* se richiesta data registrazione
097600170616     C                   When      ($CF        = 'C'   and
097700170616     C                              DtRifCA12  = 'R'      )
097800170616     C                             OR
097900170707     C                             $CF        = 'F'
098000170616     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
098100170616     C                             ' AND (REGDtReg'
098200170616     C*
098300170616     C* se richiesta data documento
098400170616     C                   When      ($CF        = 'C'   and
098500170616     C                              DtRifCA12  = 'D'      )
098600170616     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
098700170616     C                             ' AND (REGDtDoc'
098800170616     C*
098900170616     C* se richiesta data annotazione, posso andare solo con la data
099000170616     C* di registrazione
099100170706     C                   When      ($CF        = 'C'   and
099200170706     C                              DtRifCA12  = 'A'      )
099300170616     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
099400170616     C                             ' AND (REGDtReg'
099500170616     C                   endSl
099600110902     C*
099700110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV) +
099800110902     C                             ' BETWEEN ''' + $Data10A1 +
099900110902     C                             ''' AND ''' + $Data10A2 + ''''
100000110902     C                             + ' ) '
100100110902     C*
100200170616     C*
100300170615     C                   if        Kcc1I52    <> *Blank   or
100400170615     C                             Kcc2I52    <> *Blank   or
100500170615     C                             Kcc3I52    <> *Blank   or
100600170615     C                             Kcc4I52    <> *Blank   or
100700170615     C                             Kcc5I52    <> *Blank   or
100800170615     C                             Kcc6I52    <> *Blank
100900110902     C*
101000110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
101100110902     C                             ' AND MOVKcc     in (' + '''' +
101200170615     C                             Kcc1I52 + ''''  +
101300110902     C                             ' , '   + ''''  +
101400170615     C                             Kcc2I52 + ''''  +
101500110902     C                             ' , '   + ''''  +
101600170615     C                             Kcc3I52 + ''''  +
101700110902     C                             ' , '   + ''''  +
101800170615     C                             Kcc4I52 + ''''  +
101900110902     C                             ' , '   + ''''  +
102000170615     C                             Kcc5I52 + ''''  +
102100110902     C                             ' , '   + ''''  +
102200170615     C                             Kcc6I52 + ''''  +
102300110902     C                             ' ) '
102400110902     C                   endif
102500110902     C*
102600110902     C*
102700170615     C                   if        KccI52     <> *Blank
102800110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
102900110902     C                             ' AND MOVKcc    =' + '''' +
103000170615     C                             KccI52 + ''''
103100110902     C                   endif
103200110902     C*
103300170615     C                   if        KscI52     <> *Blank
103400110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
103500110902     C                             ' AND MOVKsc   =' + '''' +
103600170615     C                             KscI52 + ''''
103700110902     C                   endif
103800110902     C*
103900110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
104000170801     C                               ' ORDER BY IVAKCCCP, '+
104100170801     C                               ' IVAKSCCP, IVADTREG,' +
104200170801     C                               ' IVASYS, IVANRASREG '
104300110902     C*
104400110902     C* For Fetch Only
104500110902     C                   Eval      SqlREG_MOV  = %trimr(SqlREG_MOV)+
104600110902     C                               ' FOR FETCH ONLY'
104700110902     C*
104800110902     C                   Move      SqlREG_MOV    SqlREG_MOVDS
104900110902     C*
105000110902     C                   ENDSR
105100110902     C/EJECT
105200110902     C******************************************************
105300110902     C* Open cursore sql
105400110902     C******************************************************
105500110902     C     OpenSqlREG_MOVBEGSR
105600110902     C*
105700110902     C/EXEC SQL
105800110902     C+     PREPARE S2 FROM :SqlREG_MOV
105900110902     C/END-EXEC
106000110902     C*
106100110902     C/EXEC SQL
106200110902     C+     Declare A2 cursor for S2
106300110902     C/END-EXEC
106400110902     C*
106500110902     C/EXEC SQL
106600110902     C+     OPEN A2
106700110902     C/END-EXEC
106800110902     C*
106900110902     C                   ENDSR
107000110902     C/EJECT
107100110902     C************************************************************
107200110902     C* Eseguo lettura dei movimenti NDREG00F e NDMOV00F
107300110902     C************************************************************
107400110902     C     ExeSqlREG_MOV BEGSR
107500110902     C*
107600110902     C                   Move      *Off          $FineREG_MOV
107700110902     C                   Z-Add     0             $Ind
107800110902     C                   Z-Add     0             SqlEr3
107900110902     C*
108000110902     C                   DoW       $FineREG_MOV= *Off
108100110902     C*
108200110902     C                   EXSR      FetchREG_MOV
108300110902     C*
108400110902     C                   Select
108500110902     C*-----------------------
108600110902     C* OK
108700110902     C                   When      SqlCod     >= 0     and
108800110902     C                             SqlCod     <> 100
108900110902     C*
109000110902     C     $Ind          OCCUR     NDIVADS
109100110902     C*
109200170724     C                   EXSR      RepNDMOV
109300170616     C*
109400170616     C                   if        MOVSNatura  = $CF
109500170724     C*
109600170724     C                   EXSR      RepNDREG
109700170724     C*
109800110902     C                   Eval      $ChkNDIVA   = *Off
109900110902     C                   EXSR      ElabIVA
110000170616     C                   endif
110100110902     C*
110200110902     C*-----------------------
110300110902     C* fine lettura
110400110902     C                   When      SqlCod      = 100
110500110902     C*
110600110902     C                   COMMIT
110700170724     C                   FEOD      SDGCST00F
110800110902     C*
110900110902     C* Stampa autofatture ultima riga totali
111000110902     C                   EXSR      StpAutof_Tot
111100110902     C*
111200110902     C                   Move      *On           $FineREG_MOV
111300110902     C*
111400110902     C                   Other
111500110902     C*
111600110902     C*-----------------------
111700110902     C* si è verificato un errore
111800110902     C                   Eval      $TpErr1     = '99'
111900170801     C                   MoveL(P)  SqlREG_MOV    StringaSql
112000110902     C                   EXSR      StpErr1
112100170615     C                   Eval      ERRI52      = '1'
112200110902     C*
112300110902     C                   Move      *On           $FineREG_MOV
112400110902     C                   endsl
112500110902     C*
112600110902     C                   endDo
112700110902     C*
112800110902     C                   ENDSR
112900110902     C/EJECT
113000110902     C******************************************************
113100110902     C* Eseguo lettura
113200110902     C******************************************************
113300110902     C     FetchREG_MOV  BEGSR
113400110902     C*
113500110902     C     $Ind          Ifeq      SqlEr3
113600110902     C/EXEC SQL
113700110902     C+     FETCH A2 FOR 1000 ROWS INTO :NDIVADS
113800110902     C/END-EXEC
113900110902     C                   Z-ADD     1             $Ind
114000110902     C                   else
114100110902     C                   ADD       1             $Ind
114200110902     C                   endif
114300110902     C*
114400110902     C                   ENDSR
114500110902     C/EJECT
114600000000     C********************************************************************
114700000000     C* Elaboro i movimenti IVA
114800000000     C********************************************************************
114900130826     C     ElabIVA       BEGSR
115000170724     C*
115100170724     C* controllo se presente legame
115200170724     C* "bolla doganale/fattura"
115300170724     C* Se bolletta e trovo il fornitore, sostituisco il conto
115400170724     C* dogana con il conto del fornitore
115500170724     C                   if        $CF         = 'F'
115600170724     C*
115700170724     C                   IF        IVASys     <> SAV4Sys     or
115800170724     C                             IVANrAsReg <> SAV4NrAsReg
115900170724     C*
116000170724     C                   Clear                   SDGSG54DS
116100170724     C                   EXSR      RepLegame
116200170724     C                   endif
116300170724     C*
116400170724     C                   Move      IVASys        SAV4Sys
116500170724     C                   Move      IVANrAsReg    SAV4NrAsReg
116600170724     C*
116700170724     C                   ENDIF
116800170724     C*
116900170724     C                   EXSR      ChkConto
117000000000     C*
117100000000     C*-------------------
117200000000     C                   if        $OkConto    = *On
117300160201     C                   EXSR      ChkNDMOV
117400000000     C                   else
117500000000     C                   Eval      $OkMOV      = *Off
117600000000     C                   endif
117700170801     C*
117800170801     C                   if        $OkMOV      = *Off
117900170801     C                   Eval      $WriteCST   = *Off
118000170801     C                   endif
118100170801     C*
118200170801     C*-------------------
118300170801     C                   if        $OkMOV      = *On
118400170801     C                   if        $ChkNDIVA   = *On
118500170801     C                   EXSR      ChkNDIVA
118600170801     C*
118700170801     C                   else
118800170801     C*
118900170801     C                   EXSR      InzNDIVA
119000170801     C                   endif
119100170801     C
119200170801     C                   endif
119300000000     C*
119400130826     C* ---------------
119500130826     C*
119600170529     C                   if        $WriteCST   = *ON
119700170529     C* SCRIVO SDGCST00F da NDIVA
119800170529     C                   EXSR      GesCST_IVA
119900130826     C*
120000130826     C                   endif
120100130826     C*
120200130826     C                   ENDSR
120300130826     C/EJECT
120400130826     C************************************************************
120500130826     C* Controllo se registrazione già estratta e attiva
120600130826     C************************************************************
120700170724     C     CtrExistCST   BEGSR
120800170717     C*
120900170717     C* a questo punto del pgm, quello che doveva essere cancellato,
121000170717     C* è stato cancellato  (a parità di tipo comunicaz. e tp record)
121100170717     C*
121200170717     C* Rimangono (e a cui possono essere aggiunti nuovi rec di tipo):
121300170717     C* diverso):
121400170717     C* - se faccio estrazione Ordinaria, rimangono i rec di Rettifica
121500170717     C*   e quelli di Annullamento
121600170717     C* - se faccio estrazione di Rettifica, rimangono i rec Ordinari
121700170717     C*   e quelli di Annullamento
121800170717     C*
121900170717     C*
122000170717     C* A quelli che rimangono, NON devono essere aggiunti nuovi record
122100170717     C* riferiti allo stesso documento (identificato per num. assol. di
122200170717     C* registrazione oppure per riferimenti del documento)
122300170717     C*
122400170717     C* Rimangono (e a cui NON devono essere aggiunti nuovi rec):
122500170717     C* - i record Inviati
122600170717     C* - i record Manuali
122700170717     C* - i record Annullati
122800170717     C*
122900170717     C* Se sto estraendo i dati Ordinari, non deve esistere lo stesso
123000170717     C* documento come Rettifica (può esistere come Annullato)
123100170717     C* Se sto estraendo i dati di Rettifica, deve esistere lo stesso
123200170717     C* documento come rec Ordinario e dev'essere inviato: chiamo
123300170717     C* funzione per recuperare l'idfile ordinario inviato e la
123400170717     C* posizione sull'idfile ordinario dell'AdE
123500111018     C*
123600170529     C                   Eval      $WriteCST   = *On
123700170717     C*
123800170731     C                   Clear                   SDG00703DS
123900170717     C*
124000170731     C                   Eval      TpCom0703   = TpComI52
124100170731     C                   Eval      TpRec0703   = TpRecI52
124200170731     C                   Eval      Sys0703     = IVASys
124300170731     C                   Eval      NrAsRe0703  = IVANrAsReg
124400170717     C*
124500170731     C                   Eval      Soc0703     = IVASocieta
124600170731     C                   Eval      TpCom0703   = TpComI52
124700170731     C                   Eval      TpRec0703   = TpRecI52
124800170724     C*
124900170724     C                   if        $CodOperMOV <> '9'
125000170724     C*
125100170724     C* ripristino il movimento cliente/fornitore
125200170724     C                   Move      NDMOVSav      NDMOV00F
125300170724     C*
125400170731     C                   Eval      Kcc0703     = MOVKcc
125500170731     C                   Eval      Ksc0703     = MOVKsc
125600170731     C                   Eval      DtDoc0703   = MOVDtDoc
125700170731     C                   Eval      NrDoc0703   = MOVNrDoc
125800170731     C                   Eval      SerDoc0703  = MOVSerDoc
125900170724     C                   endif
126000170724     C*
126100170724     C                   if        $CodOperREG <> '9'
126200170724     C*
126300170724     C* ripristino di registrazione del movimento cliente/fornitore
126400170724     C                   Move      NDREGSav      NDREG00F
126500170724     C*
126600170731     C                   Eval      DtReg0703   = REGDtReg
126700170731     C                   Eval      NrReg0703   = REGNrReg
126800170731     C                   Eval      SerReg0703  = REGSerReg
126900170724     C                   endif
127000170724     C*
127100170731     C                   Eval      DtAnn0703   = IVADtAnnot
127200170717     C*
127300170731     C                   CALLB     'SDG00703R'
127400170717     C                   PARM                    KPJBA
127500170731     C                   PARM                    SDG00703DS
127600170717     C*
127700170731     C                   Eval      $WriteCST   = WrtCST0703
127800130826     C*
127900130826     C                   ENDSR
128000130826     C/EJECT
128100170801     C************************************************************
128200170801     C* Reperimento legame bolla doganale/fattura
128300170801     C************************************************************
128400170801     C     RepLegame     BEGSR
128500170801     C*
128600170801     C                   Eval      WKcc        = IVAKccCP
128700170801     C                   Eval      WKsc        = IVAKscCP
128800170801     C                   Eval      WData       = *Loval
128900160125     C                   EXSR      CallMVC002
129000160125     C*
129100160125     C                   if        $Esito     <> *On     and
129200170814fm4  C                             SNatura002  = 'F'     and
129300170814fm4  C                             AlleIVA002  = '02'
129400160119     C*
129500160119     C                   Eval      SocietaG54  = XScSoc
129600160119     C                   Eval      SysG54      = IVASys
129700160119     C                   Eval      NrAsRegG54  = IVANrAsReg
129800160119     C                   if        AlleIVA002  = '02'
129900160119     C                   Eval      TpDocG54    = 'B'
130000160119     C                   else
130100160125     C                   Eval      TpDocG54    = 'F'
130200160119     C                   endif
130300170616     C                   Eval      TpDtRifG54  = 'R'
130400160119     C*
130500160125     C                   MoveL     SDGSG54DS     KPJBU
130600160125     C*
130700160119     C                   CALL      'SDGSG54R'
130800160119     C                   PARM                    KPJBA
130900160119     C*
131000160119     C                   MoveL     KPJBU         SDGSG54DS
131100160119     C*
131200160126     C* se Dogana, recupero il conto del fornitore se presente
131300160303     C                   if        AlleIVA002  = '02'    and
131400170811fm3  X****                         KccG54     <> *Blank  and
131500170811fm3  X****                         KscG54     <> *Blank
131600170811     C                             (KccG54     <> *Blank  and
131700170811     C                              KscG54     <> *Blank     )or
131800170811fm3  C                             SoggG54    <> *Blank
131900170801     C*
132000170801     C* se è Dogana, sostituisco il conto
132100170801     C* e il soggetto della bolla con quelli della fattura
132200170801     C* legata alla bolla, in questo modo vengono recuperati i
132300170801     C* dati fiscali e anagrafici del fornitore
132400170801     C*
132500170801     C* sostituisco il conto della contropartita IVA
132600170801     C                   Eval      IVAKccCp    = KccG54
132700170801     C                   Eval      IVAKscCp    = KscG54
132800170801     C*
132900170810     C* se ho trovato il legame Bolla/fattura , lo segnalo
133000170810     C                   if        TpAnnG54   <> '57'
133100170810     C                   Eval      $LegameBF   = *On
133200170810     C                   endif
133300170810     C*
133400170801     C* se cerco fattura e non trovo la registrazione contabile
133500170801     C                   if        TpAnnG54    = '55'          or
133600170814fm4  X****** se cerco bolla   e non trovo la registrazione contabile
133700170814fm4  X******                       TpAnnG54    = '58'          or
133800170801     C* se sto elaborando la bolla e non trovo alcun legame (fattura
133900170801     C* o soggetto), dal 2018 è errore
134000170801     C                             (TpAnnG54    = '57'    and
134100170801     C                              AnnoI52     > 2017        )
134200170801     C*
134300170801     C                   Eval      $AnnMOV     = *On
134400170801     C                   Eval      $TpAnn      = TpAnnG54
134500170801     C*
134600170801     C                   endif
134700170801     C*
134800170801     C                   endif
134900170801     C*
135000170801     C                   endif
135100170801     C*
135200170801     C                   ENDSR
135300170801     C/EJECT
135400000000     C************************************************************
135500000000     C* Controllo dati conto
135600000000     C************************************************************
135700000000     C     ChkConto      BEGSR
135800000000     C*
135900000000     C                   IF        IVAKccCP   <> SAVKccCP or
136000000000     C                             IVAKscCP   <> SAVKscCP
136100000000     C*
136200000000     C                   Move      IVAKccCP      SAVKccCP
136300000000     C                   Move      IVAKscCP      SAVKscCP
136400000000     C*
136500000000     C                   EXSR      InzConto
136600170721     C                   EXSR      InzNDMOV
136700170721     C                   EXSR      InzNDIVA
136800000000     C*
136900170728     C* reperisco i dati anagrafici della contropartita, che in caso
137000170728     C* di bolletta doganale potrebbe essere il fornitore collegato
137100000000     C                   Eval      WKcc        = IVAKccCP
137200000000     C                   Eval      WKsc        = IVAKscCP
137300170801     C                   Eval      WData       = *Loval
137400110902     C                   Clear                   DsAttributi
137500000000     C                   EXSR      CallMVC002
137600000000     C*
137700000000     C                   if        $Esito      = *On
137800000000     C* annullo il conto
137900000000     C                   Eval      $AnnConto   = *On
138000000000     C                   if        $TpAnn      = *Blank
138100000000     C                   Eval      $TpAnn      = '07'
138200000000     C                   endif
138300110902     C                   else
138400110902     C                   MoveL     Attributi     DsAttributi
138500000000     C                   endif
138600170801     C*----------------------
138700170801     C*
138800170801     C                   EXSR      SelConto
138900000000     C*
139000000000     C*----------------------
139100000000     C*
139200160201     C                   if        $OkConto    = *On
139300170728     C* reperimento dati fiscali dipendenti solo dal soggetto
139400170728     C                   EXSR      RepFisc_Sog
139500160201     C                   endif
139600000000     C*
139700000000     C*----------------------
139800000000     C*
139900000000     C                   ENDIF
140000000000     C*
140100000000     C                   ENDSR
140200000000     C/EJECT
140300140526     C************************************************************
140400000000     C* Inizializzo variabili appoggio per dati conto cli/for
140500000000     C************************************************************
140600000000     C     InzConto      BEGSR
140700000000     C*
140800000000     C                   Eval      $OkConto    = *On
140900000000     C                   Eval      $AnnConto   = *Off
141000170724     C                   Eval      $TpAnn      = *Blank
141100000000     C*
141200170714     C                   Clear                   $PartIVASog
141300170714     C                   Clear                   $CdFiscSog
141400170731     C                   Clear                   $Stato1Sog
141500170731     C                   Clear                   $IdPaes1Sog
141600170714     C                   Clear                   $PIVA1Sog
141700170706     C                   Clear                   $CdFisc1Sog
141800170707     C                   Clear                   $CogRag1Sog
141900170725     C                   Clear                   $TpN1Sog
142000170725     C                   Clear                   $Denom1Sog
142100170727     C                   Clear                   $Cognom1Sog
142200170707     C                   Clear                   $Nome1Sog
142300170714     C                   Clear                   $Indir2Sog
142400170714     C                   Clear                   $NrCiv2Sog
142500170714     C                   Clear                   $CAP2Sog
142600170707     C                   Clear                   $Locali2Sog
142700170714     C                   Clear                   $Prov2Sog
142800170714     C                   Clear                   $Stato2Sog
142900170714     C                   Clear                   $IdPaes2Sog
143000170721     C                   Clear                   $Indir3Sog
143100170721     C                   Clear                   $NrCiv3Sog
143200170721     C                   Clear                   $CAP3Sog
143300170721     C                   Clear                   $Locali3Sog
143400170721     C                   Clear                   $Prov3Sog
143500170721     C                   Clear                   $Stato3Sog
143600170721     C                   Clear                   $IdPaes3Sog
143700170731     C                   Clear                   $Stato4Sog
143800170721     C                   Clear                   $IdPaes4Sog
143900170721     C                   Clear                   $PIVA4Sog
144000170721     C                   Clear                   $CogRag4Sog
144100170725     C                   Clear                   $TpN4Sog
144200170725     C                   Clear                   $Denom4Sog
144300170727     C                   Clear                   $Cognom4Sog
144400170721     C                   Clear                   $Nome4Sog
144500170714     C*
144600170731     C                   Clear                   $PartIVAMov
144700170731     C                   Clear                   $CdFiscMov
144800170731     C                   Clear                   $IdPaes1Mov
144900170731     C                   Clear                   $PIVA1Mov
145000170731     C                   Clear                   $CdFisc1Mov
145100170731     C                   Clear                   $CogRag1Mov
145200170731     C                   Clear                   $TpN1Mov
145300170731     C                   Clear                   $Denom1Mov
145400170731     C                   Clear                   $Cognom1Mov
145500170731     C                   Clear                   $Nome1Mov
145600170731     C                   Clear                   $Indir2Mov
145700170731     C                   Clear                   $NrCiv2Mov
145800170731     C                   Clear                   $CAP2Mov
145900170731     C                   Clear                   $Locali2Mov
146000170731     C                   Clear                   $Prov2Mov
146100170731     C                   Clear                   $Stato2Mov
146200170731     C                   Clear                   $IdPaes2Mov
146300170731     C                   Clear                   $Indir3Mov
146400170731     C                   Clear                   $NrCiv3Mov
146500170731     C                   Clear                   $CAP3Mov
146600170731     C                   Clear                   $Locali3Mov
146700170731     C                   Clear                   $Prov3Mov
146800170731     C                   Clear                   $Stato3Mov
146900170731     C                   Clear                   $IdPaes3Mov
147000170731     C                   Clear                   $Stato4Mov
147100170731     C                   Clear                   $IdPaes4Mov
147200170731     C                   Clear                   $PIVA4Mov
147300170731     C                   Clear                   $CogRag4Mov
147400170731     C                   Clear                   $TpN4Mov
147500170731     C                   Clear                   $Denom4Mov
147600170731     C                   Clear                   $Cognom4Mov
147700170731     C                   Clear                   $Nome4Mov
147800170727     C*
147900170727     C* SAVDtDoc serve per reperire i dati fiscali
148000170727     C* storicizzati per il soggetto legato al conto
148100170727     C                   Move      *Loval        SAVDtDoc
148200000000     C*
148300000000     C* SAVDtDoc2 serve per stampare i dati fiscali
148400000000     C* non valorizzati sul soggetto legato al conto
148500000000     C                   Move      *Loval        SAVDtDoc2
148600170801     C*
148700170801     C                   Eval      $ImpDoc     = 0
148800000000     C*
148900000000     C                   ENDSR
149000000000     C/EJECT
149100000000     C***********************************************************
149200000000     C* Driver per controllo conto
149300000000     C***********************************************************
149400000000     C     CallMvc002    BEGSR
149500000000     C*
149600000000     C                   Clear                   ND002DS
149700000000     C*
149800000000     C                   Eval      $LenOut    =  %Size(ND002DS)
149900000000     C*
150000000000     C                   CallB     'NDMVC002'
150100000000     C                   PARM                    XScSoc
150200000000     C                   PARM                    WKCC
150300000000     C                   PARM                    WKSC
150400170724     C                   PARM                    WData
150500000000     C                   PARM      *Off          $GesMsg
150600000000     C                   PARM      *Off          $Esito
150700000000     C                   PARM      'ND002DS   '  $Struttura
150800000000     C                   PARM                    ND002DS
150900000000     C                   PARM                    $LenOut
151000000000     C*
151100000000     C                   ENDSR
151200000000     C/EJECT
151300000000     C************************************************************
151400000000     C* Reperimento dati fiscali del conto
151500000000     C************************************************************
151600170728     C     RepFisc_Sog   BEGSR
151700000000     C*
151800170727     C* se è Dogana, recupero i dati anagrafici
151900170727     C* e fiscali del soggetto fornitore
152000170727     C                   if        $CF         = 'F'     and
152100170727     C                             AlleIVA002  = '02'    and
152200170727     C                             KccG54      = *Blank  and
152300170727     C                             KscG54      = *Blank  and
152400170727     C                             SoggG54    <> *Blank
152500170727     C                   Eval      WSogg       = SoggG54
152600170727     C                   else
152700000000     C                   Eval      WSogg       = Sogg002
152800170727     C                   endif
152900170727     C*
153000170727     C                   Clear                   SDG00701DS
153100170727     C*
153200170727     C                   Eval      Opz0701     = '01'
153300170731     C                   Eval      Err0701     = '00'
153400170728     C                   Eval      Soc0701     = SocietaI52
153500170727     C                   Eval      TpCom0701   = TpComI52
153600170727     C                   Eval      Anno0701    = AnnoI52
153700170727     C                   Eval      Trim0701    = TrimI52
153800170727     C                   Eval      Mese0701    = 0
153900170727     C                   Eval      Sogg0701    = WSogg
154000170727     C                   Eval      CF0701      = SNatura002
154100170727     C                   Eval      Tecnic0701  = *Off
154200170728     C                   Eval      TpN1Sg0701  = *Blank
154300170727     C                   Eval      DtRif0701   = *Loval
154400170727     C                   Eval      NrAsRe0701  = 0
154500170727     C*
154600170727     C* salvo la kpjbu per ripristinarla dopo il richiamo
154700170727     C                   MoveL     KPJBU         SavKPJBU
154800170727     C*
154900170727     C                   CALLB     'SDG00701R'
155000170727     C                   PARM                    KPJBA
155100170727     C                   PARM                    SDG00701DS
155200170727     C*
155300170727     C* ripristino KPJBU
155400170727     C                   MoveL     SavKPJBU      KPJBU
155500170727     C*
155600170727     C* annullo il conto
155700170727     C                   if        AnnCon0701  = *On
155800170727     C                   Eval      $AnnConto   = *On
155900170727     C                   if        TpAnn0701  <> *Blank
156000170727     C                   if        $TpAnn      = *Blank
156100170727     C                   Eval      $TpAnn      = TpAnn0701
156200170727     C                   endif
156300170727     C                   endif
156400170727     C                   endif
156500170714     C*
156600170731     C* DATI FISCALI E SEDE
156700170727     C                   MoveL(P)  PartIV0701    $PartIVASog
156800170727     C                   MoveL(P)  CdFisc0701    $CdFiscSog
156900170731     C                   MoveL(P)  Stato10701    $Stato1Sog
157000170731     C                   MoveL(P)  IdPae10701    $IdPaes1Sog
157100170727     C                   MoveL(P)  PIVA10701     $PIVA1Sog
157200170727     C                   MoveL(P)  CdFisc0701    $CdFisc1Sog
157300170727     C*
157400170727     C                   MoveL(P)  CogRa10701    $CogRag1Sog
157500170727     C                   MoveL(P)  TpN10701      $TpN1Sog
157600170727     C                   MoveL(P)  Denom10701    $Denom1Sog
157700170727     C                   MoveL(P)  Cogno10701    $Cognom1Sog
157800170727     C                   MoveL(P)  Nome10701     $Nome1Sog
157900170727     C*
158000170727     C                   MoveL(P)  Indir20701    $Indir2Sog
158100170727     C                   MoveL(P)  NrCiv20701    $NrCiv2Sog
158200170727     C                   MoveL(P)  CAP20701      $CAP2Sog
158300170727     C                   MoveL(P)  Local20701    $Locali2Sog
158400170727     C                   MoveL(P)  Prov20701     $Prov2Sog
158500170727     C                   MoveL(P)  Stato20701    $Stato2Sog
158600170727     C                   MoveL(P)  IdPae20701    $IdPaes2Sog
158700170731     C*
158800170731     C* STABILE ORGANIZZAZIONE
158900170731     C                   MoveL(P)  Indir30701    $Indir3Sog
159000170731     C                   MoveL(P)  NrCiv30701    $NrCiv3Sog
159100170731     C                   MoveL(P)  CAP30701      $CAP3Sog
159200170731     C                   MoveL(P)  Local30701    $Locali3Sog
159300170731     C                   MoveL(P)  Prov30701     $Prov3Sog
159400170731     C                   MoveL(P)  Stato30701    $Stato3Sog
159500170731     C                   MoveL(P)  IdPae30701    $IdPaes3Sog
159600170731     C*
159700170731     C* RAPPRESENTANTE FISCALE
159800170731     C                   MoveL(P)  Stato40701    $Stato4Sog
159900170731     C                   MoveL(P)  IdPae40701    $IdPaes4Sog
160000170731     C                   MoveL(P)  PIVA40701     $PIVA4Sog
160100170731     C*
160200170731     C                   MoveL(P)  CogRa40701    $CogRag4Sog
160300170731     C                   MoveL(P)  TpN40701      $TpN4Sog
160400170731     C                   MoveL(P)  Denom40701    $Denom4Sog
160500170731     C                   MoveL(P)  Cogno40701    $Cognom4Sog
160600170731     C                   MoveL(P)  Nome40701     $Nome4Sog
160700000000     C*
160800000000     C                   ENDSR
160900000000     C/EJECT
161000000000     C************************************************************
161100000000     C* Chiamata al driver dei soggetti
161200000000     C************************************************************
161300000000     C     CallDvaSog    BEGSR
161400000000     C*
161500000000     C                   CALLB     'NDDVASOG'
161600000000     C                   PARM      '1'           DVARic
161700000000     C                   PARM      XScSoc        DVASocieta
161800000000     C                   PARM      *Blank        DVAUnita
161900000000     C                   PARM                    DVAStrutt
162000000000     C                   PARM                    DVADtRif
162100000000     C                   PARM                    DVALenOut
162200170728     C                   PARM      *Blank        DVASNatura
162300170728     C                   PARM      *Blank        DVACodice
162400000000     C                   PARM      *Blank        DVALineaV
162500000000     C                   PARM      *Zero         DVAFiliale
162600000000     C                   PARM      WSogg         DVASogg
162700170728     C                   PARM      *Blank        DVATpInd
162800170728     C                   PARM      *Blank        DVACdInd
162900000000     C                   PARM      *Off          DVAErrore
163000000000     C                   PARM                    DVAOutPut
163100000000     C*
163200000000     C                   ENDSR
163300000000     C/EJECT
163400000000     C************************************************************
163500000000     C* Selezioni sul conto
163600000000     C************************************************************
163700000000     C     SelConto      BEGSR
163800000000     C*
163900000000     C                   Select
164000000000     C*
164100000000     C* il soggetto non rientra nelle selezioni, scarto il conto
164200170615     C                   When      Sogg002    <> SoggI52  and
164300170615     C                             SoggI52    <> *Blank
164400130826     C                   Eval      $OkConto    = *Off
164500130826     C*
164600130826     C* la sottonatura non rientra nelle selezioni, scarto il conto
164700170615     C                   When      SNatura002 <> CFI52    and
164800170615     C                             CFI52      <> *Blank
164900130826     C                   Eval      $OkConto    = *Off
165000130826     C*
165100130826     C* la sottonatura non è cliente nè fornitore, scarto il conto
165200130826     C                   When      SNatura002 <> 'C'      and
165300130826     C                             SNatura002 <> 'F'
165400130826     C                   Eval      $OkConto    = *Off
165500130826     C*
165600130826     C                   endSl
165700000000     C*
165800000000     C                   if        $OkConto    = *On
165900000000     C*
166000000000     C* conto escluso dagli elenchi
166100000000     C                   if        AlleIVA002  = '01'
166200170810     C***************    Eval      $AnnConto   = *On
166300000000     C                   if        $TpAnn      = *Blank
166400170810     C***************    Eval      $TpAnn      = '08'
166500000000     C                   endif
166600000000     C                   endif
166700000000     C*
166800000000     C                   endif
166900000000     C*
167000000000     C                   ENDSR
167100000000     C/EJECT
167200000000     C********************************************************************
167300000000     C* Controllo dati del movimento cliente/fornitore
167400000000     C********************************************************************
167500000000     C     ChkNDMOV      BEGSR
167600000000     C*
167700000000     C* esamino il movimento solo a cambio di registrazione
167800170801     C*
167900170801     C                   IF        IVASys     <> SAV3Sys   or
168000170801     C                             IVANrAsReg <> SAV3NrAsReg
168100000000     C*
168200000000     C                   COMMIT
168300170718     C                   FEOD      SDGCST00F
168400000000     C*
168500170801     C                   Move      IVASys        SAV3Sys
168600170801     C                   Move      IVANrAsReg    SAV3NrAsReg
168700000000     C*
168800000000     C                   EXSR      InzNDMOV
168900170721     C                   EXSR      InzNDIVA
169000170801     C*
169100170724     C* verifico se il documento era già stato estratto precedentemente
169200170724     C                   EXSR      CtrExistCST
169300000000     C*
169400170724     C                   if        $WriteCST   = *On
169500000000     C*
169600000000     C                   EXSR      SelNDMOV1
169700000000     C                   if        $OkMOV      = *On
169800000000     C*
169900000000     C* reperimento dati fiscali dipendenti dal
170000000000     C* movimento contabile
170100170728     C                   EXSR      RepFisc_Mov
170200000000     C*
170300170721     C                   EXSR      CtrNDMOV
170400170721     C*
170500170801     C* in caso di Autofattura, giro l'operazione sull'azienda
170600170801     C* comunicante impostando i suoi dati anagrafici
170700170801     C                   if        $Autofatt   = *On
170800170801     C                   EXSR      RieAnaSoc
170900170721     C                   endif
171000170710     C*
171100170721     C* applico i filtri anagrafici dipendenti dal movimento
171200170710     C                   EXSR      SelNDMOV2
171300000000     C*
171400000000     C                   endif
171500170724     C*
171600170724     C                   endif
171700170721     C*
171800170721     C                   ENDIF
171900000000     C*
172000000000     C                   ENDSR
172100000000     C/EJECT
172200000000     C************************************************************
172300000000     C* Inizializzo variabili appoggio per dati movimento
172400000000     C************************************************************
172500000000     C     InzNDMOV      BEGSR
172600000000     C*
172700170810     C                   Eval      $LegameBF   = *Off
172800170810     C                   Eval      $Intra      = *Off
172900170801     C                   Eval      $Reverse    = *Off
173000170801     C                   Eval      $Margine    = *Off
173100000000     C                   Eval      $OkMOV      = *On
173200000000     C* se il conto è annullato , estraggo cmq i movimenti
173300000000     C* del periodo e li scrivo annullati
173400000000     C                   Eval      $AnnMOV     = $AnnConto
173500000000     C                   if        $AnnMOV    <> *On
173600000000     C                   Eval      $TpAnn      = *Blank
173700000000     C                   endif
173800000000     C*
173900170801     C                   Eval      $Autofatt   = *Off
174000170801     C                   Eval      $AutoCons   = *Off
174100130821     C                   Eval      $NatOpe     = *Blank
174200130826     C                   Eval      $ImpDoc     = 0
174300000000     C*
174400000000     C                   ENDSR
174500000000     C/EJECT
174600000000     C************************************************************
174700000000     c* Reperisce dati del movimento del cliente/fornitore
174800000000     C************************************************************
174900170724     C     RepNDMOV      BEGSR
175000170724     C*
175100170724     C                   Clear                   NDMOV00F
175200170724     C                   Clear                   NDMOVSav
175300000000     C*
175400000000     C                   Eval      $LenOut     = %size(NDMOV00F)
175500000000     C                   CALLB     'NDDRVMOV'
175600000000     C                   PARM                    IVASys
175700000000     C                   PARM                    IVANrAsReg
175800000000     C* cerco la riga del cliente/fornitore (è sempre la num.1)
175900000000     C* quella dell'IVA è sempre la 3
176000000000     C                   PARM      1             $NrRigaM
176100000000     C                   PARM      'NDMOV     '  $Struttura
176200000000     C                   PARM                    $LenOut
176300170724     C                   PARM      '1'           $CodOperMOV
176400000000     C                   PARM                    NDMOV00F
176500170724     C*
176600170724     C                   if        $CodOperMOV <> '9'
176700170724     C*
176800170724     C* salvo i dati del movimento cliente/fornitore
176900170724     C                   MoveL     NDMOV00F      NDMOVSAV
177000170724     C*
177100170724     C                   endif
177200000000     C*
177300000000     C                   ENDSR
177400000000     C/EJECT
177500170724     C************************************************************
177600170724     c* Reperisce dati di registraz. del movimento cliente/fornitore
177700170724     C************************************************************
177800170724     C     RepNDREG      BEGSR
177900170724     C*
178000170724     C                   Clear                   NDREG00F
178100170724     C                   Clear                   NDREGSav
178200170724     C*
178300170724     C                   EVAL      $LenOut     = %Size(NDREG00F)
178400170724     C*
178500170724     C                   CALLB     'NDDRVREG'
178600170724     C                   PARM                    IVASys
178700170724     C                   PARM                    IVANrAsReg
178800170724     C                   PARM      'NDREG     '  $Struttura
178900170724     C                   PARM                    $LenOut
179000170724     C                   PARM      *Blank        $CodOperREG
179100170724     C                   PARM                    $Output
179200170724     C                   PARM                    NDREG00F
179300170724     C*
179400170724     C                   if        $CodOperREG <> '9'
179500170724     C*
179600170724     C                   EVAL      %subst(NDREG00F:1:$LenOut)
179700170724     C                             = %subst($Output:1:$LenOut)
179800170724     C*
179900170724     C* salvo i dati di registrazione del movimento cliente/fornitore
180000170724     C                   MoveL     NDREG00F      NDREGSAV
180100170724     C*
180200170724     C                   endif
180300170724     C*
180400170724     C                   ENDSR
180500170724     C/EJECT
180600000000     C********************************************************************
180700000000     C* Seleziono dati sul movimento del cliente/fornitore
180800000000     C********************************************************************
180900000000     C     SelNDMOV1     BEGSR
181000000000     C*
181100170724     C                   if        $CodOperMOV <> '9'
181200130826     C                   Eval      $ImpDoc     = MOVimporto
181300130826     C                   endif
181400130826     C*
181500000000     C                   Select
181600000000     C* movimento cliente/fornitore non trovato
181700170724     C                   When      $CodOperMOV = '9'
181800000000     C                   Eval      $AnnMOV     = *On
181900160125     C                   if        $TpAnn      = *Blank
182000000000     C                   Eval      $TpAnn      = '22'
182100000000     C                   endif
182200170724     C*
182300170724     C                   When      $CodOperREG = '9'
182400170724     C                   Eval      $AnnMOV     = *On
182500170724     C                   if        $TpAnn      = *Blank
182600170801     C* registraz. cliente/fornitore non trovata
182700170801     C                   Eval      $TpAnn      = '30'
182800170724     C                   endif
182900170724     C*
183000170731     C                   When      ERR0703     = '2'
183100170724     C                   Eval      $AnnMOV     = *On
183200170724     C                   if        $TpAnn      = *Blank
183300170724      * - annulla Ord perchè manca l'ordinario inviato ma è stato
183400170724      *   trovato la rettifica  (ERRI52='2')
183500170801     C                   Eval      $TpAnn      = '72'
183600170724     C                   endif
183700170724     C*
183800170731     C                   When      ERR0703     = '3'
183900170724     C                   Eval      $AnnMOV     = *On
184000170724     C                   if        $TpAnn      = *Blank
184100170724      * - annulla Ret perchè manca l'ordinario inviato
184200170801     C                   Eval      $TpAnn      = '73'
184300170724     C                   endif
184400170724     C*
184500170724     C                   endSl
184600170724     C*
184700170724     C*
184800170724     C                   Select
184900000000     C*
185000170615     C                   When      MOVTpRegZ  <> TpRegzI52
185100000000     C                   Eval      $OkMOV      = *Off
185200000000     C*
185300170615     C                   When      MOVCtb     <> CtbI52
185400000000     C                   Eval      $OkMOV      = *Off
185500000000     C*
185600000000     C                   endSl
185700000000     C*
185800000000     C                   ENDSR
185900000000     C/EJECT
186000000000     C************************************************************
186100000000     C* Reperimento dati fiscali dipendenti dal movimento
186200000000     C************************************************************
186300170728     C     RepFisc_Mov   BEGSR
186400170727     C*
186500170727     C                   Eval      $DtRif      = MOVDtdoc
186600170727     C* per convenzione imposto sempre il fine periodo
186700170727     C                   Move      $Data10A2     $DtRif
186800170727     C*
186900170727     C                   Select
187000170727     C*
187100170824R264 X****               When      Tecnico002 <> '2'     and
187200170824R264 X****                         $DtRif     <> SAVDtDoc
187300170824R264 C                   When      Tecnico002 <> '2'
187400170731     C                   EXSR      InzDati_CPI
187500170727     C                   Move      $DtRif        SAVDtDoc
187600170727     C*
187700170727     C                   When      Tecnico002  = '2'
187800170731     C                   EXSR      InzDati_CPA
187900170727     C*
188000170727     C                   endSl
188100170727     C*
188200170727     C                   Clear                   SDG00701DS
188300170727     C*
188400170727     C                   Eval      Opz0701     = '02'
188500170731     C                   Eval      Err0701     = '00'
188600170728     C                   Eval      Soc0701     = SocietaI52
188700170727     C                   Eval      TpCom0701   = TpComI52
188800170727     C                   Eval      Anno0701    = AnnoI52
188900170727     C                   Eval      Trim0701    = TrimI52
189000170727     C                   Eval      Mese0701    = 0
189100170727     C                   Eval      Sogg0701    = WSogg
189200170727     C                   Eval      CF0701      = *Blank
189300170727     C                   Eval      Tecnic0701  = Tecnico002
189400170728     C                   Eval      TpN1Sg0701  = $TpN1Sog
189500170728     C                   Eval      DtRif0701   = $DtRif
189600170727     C                   Eval      Sys0701     = MOVSys
189700170727     C                   Eval      NrAsRe0701  = MOVNrAsREg
189800170727     C*
189900170727     C* salvo la kpjbu per ripristinarla dopo il richiamo
190000170727     C                   MoveL     KPJBU         SavKPJBU
190100170727     C*
190200170727     C                   CALLB     'SDG00701R'
190300170727     C                   PARM                    KPJBA
190400170727     C                   PARM                    SDG00701DS
190500170727     C*
190600170727     C* ripristino KPJBU
190700170727     C                   MoveL     SavKPJBU      KPJBU
190800170727     C*
190900170727     C* annullo il movimento
191000170727     C                   if        AnnMOV0701  = *On
191100170727     C                   Eval      $AnnMOV     = *On
191200170727     C                   if        TpAnn0701  <> *Blank
191300170727     C                   if        $TpAnn      = *Blank
191400170727     C                   Eval      $TpAnn      = TpAnn0701
191500170727     C                   endif
191600170727     C                   endif
191700170727     C                   endif
191800170727     C*
191900170728     C                   if        Found0701   = *On
192000170731     C*
192100170731     C* DATI FISCALI E SEDE
192200170731     C                   MoveL(P)  PartIV0701    $PartIVAMov
192300170731     C                   MoveL(P)  CdFisc0701    $CdFiscMov
192400170731     C                   MoveL(P)  Stato10701    $Stato1Mov
192500170731     C                   MoveL(P)  IdPae10701    $IdPaes1Mov
192600170731     C                   MoveL(P)  PIVA10701     $PIVA1Mov
192700170731     C                   MoveL(P)  CdFisc0701    $CdFisc1Mov
192800170727     C*
192900170731     C                   MoveL(P)  CogRa10701    $CogRag1Mov
193000170731     C                   MoveL(P)  TpN10701      $TpN1Mov
193100170731     C                   MoveL(P)  Denom10701    $Denom1Mov
193200170731     C                   MoveL(P)  Cogno10701    $Cognom1Mov
193300170727     C*
193400170727     C* se il soggetto è persona fisica i seguenti dati NON cambiano
193500170727     C* mai, percui se reperiti per il soggetto vanno comunque sempre
193600170727     C* bene
193700170731     C*******            MoveL(P)  Nome10701     $Nome1Mov
193800170727     C*
193900170731     C                   MoveL(P)  Indir20701    $Indir2Mov
194000170731     C                   MoveL(P)  NrCiv20701    $NrCiv2Mov
194100170731     C                   MoveL(P)  CAP20701      $CAP2Mov
194200170731     C                   MoveL(P)  Local20701    $Locali2Mov
194300170731     C                   MoveL(P)  Prov20701     $Prov2Mov
194400170731     C                   MoveL(P)  Stato20701    $Stato2Mov
194500170731     C                   MoveL(P)  IdPae20701    $IdPaes2Mov
194600170731     C*
194700170731     C* STABILE ORGANIZZAZIONE
194800170731     C                   MoveL(P)  Indir30701    $Indir3Mov
194900170731     C                   MoveL(P)  NrCiv30701    $NrCiv3Mov
195000170731     C                   MoveL(P)  CAP30701      $CAP3Mov
195100170731     C                   MoveL(P)  Local30701    $Locali3Mov
195200170731     C                   MoveL(P)  Prov30701     $Prov3Mov
195300170731     C                   MoveL(P)  Stato30701    $Stato3Mov
195400170731     C                   MoveL(P)  IdPae30701    $IdPaes3Mov
195500170731     C*
195600170731     C* RAPPRESENTANTE FISCALE
195700170731     C                   MoveL(P)  Stato40701    $Stato4Mov
195800170731     C                   MoveL(P)  IdPae40701    $IdPaes4Mov
195900170731     C                   MoveL(P)  PIVA40701     $PIVA4Mov
196000170731     C*
196100170731     C                   MoveL(P)  CogRa40701    $CogRag4Mov
196200170731     C                   MoveL(P)  TpN40701      $TpN4Mov
196300170731     C                   MoveL(P)  Denom40701    $Denom4Mov
196400170731     C                   MoveL(P)  Cogno40701    $Cognom4Mov
196500170731     C                   MoveL(P)  Nome40701     $Nome4Mov
196600170727     C*
196700170727     C                   endif
196800170811fm3  C*
196900170811fm3  C* se persona fisica  (potrebbe essere anche il fornitore
197000170811fm3  C* collegato alla bolletta doganale se link per fattura e non sogg)
197100170811fm3  C*    manca il nome o il cognome  e
197200170811fm3  C*    è percipiente
197300170811fm3  C* allora recupo i dati da ANAPE
197400170811fm3  C                   if        $TpN1Mov    = '1'          and
197500170811fm3  C                             Percipi002  = '1'          and
197600170811fm3  C                             ($Nome1Mov    = *Blank  or
197700170811fm3  C                              $Cognom1Mov  = *Blank    )
197800170811fm3  C                   MoveL     IVASocieta    APESocieta
197900170811fm3  C                   MoveL     IVAKccCP      APEKcc
198000170811fm3  C                   MoveL     IVAKscCP      APEKsc
198100170811fm3  C     K03APE01      CHAIN     ANAPE01L                           21
198200170811fm3  C*
198300170811fm3  C* percipiente trovato, recupero nome/cognome se presenti
198400170811fm3  C                   if        *In21       = *Off
198500170811fm3  C                   if        APECognome <> *Blank   or
198600170811fm3  C                             APENome    <> *Blank
198700170811fm3  C                   Eval      $CogRag1Mov = APECognome
198800170811fm3  C                   Eval      $Denom1Mov  = *Blank
198900170811fm3  C                   Eval      $Cognom1Mov = APECognome
199000170811fm3  C                   Eval      $Nome1Mov   = APENome
199100170811fm3  C                   endif
199200170811fm3  C                   endif
199300170811fm3  C*
199400170811fm3  C                   endif
199500000000     C*
199600000000     C                   ENDSR
199700000000     C/EJECT
199800000000     C************************************************************
199900170731     C* Inizializzo i dati anagrafici d0 dati storicizzati
200000000000     C************************************************************
200100170731     C     InzDati_CPI   BEGSR
200200000000     C*
200300000000     C*  inizializzo i dati anagrafici con quelli del soggetto,
200400000000     C*  verranno sostituiti da quelli specifici se reperiti
200500170731     C                   MoveL(P)  $PartIVASog   $PartIVAMov
200600170731     C                   MoveL(P)  $CdFiscSog    $CdFiscMov
200700170731     C                   MoveL(P)  $Stato1Sog    $Stato1Mov
200800170731     C                   MoveL(P)  $IdPaes1Sog   $IdPaes1Mov
200900170731     C                   MoveL(P)  $PIVA1Sog     $PIVA1Mov
201000170731     C                   MoveL(P)  $CdFisc1Sog   $CdFisc1Mov
201100170731     C                   MoveL(P)  $CogRag1Sog   $CogRag1Mov
201200170731     C                   MoveL(P)  $TpN1Sog      $TpN1Mov
201300170731     C                   MoveL(P)  $Denom1Sog    $Denom1Mov
201400170731     C                   MoveL(P)  $Cognom1Sog   $Cognom1Mov
201500170731     C                   MoveL(P)  $Nome1Sog     $Nome1Mov
201600170731     C                   MoveL(P)  $Indir2Sog    $Indir2Mov
201700170731     C                   Movel(P)  $NrCiv2Sog    $NrCiv2Mov
201800170731     C                   Movel(P)  $CAP2Sog      $CAP2Mov
201900170731     C                   MoveL(P)  $Locali2Sog   $Locali2Mov
202000170731     C                   MoveL(P)  $Prov2Sog     $Prov2Mov
202100170731     C                   MoveL(P)  $Stato2Sog    $Stato2Mov
202200170731     C                   MoveL(P)  $IdPaes2Sog   $IdPaes2Mov
202300000000     C*
202400000000     C* se non ci sono dati storicizzati alla data, non è un problema
202500000000     C* significa solo che sono validi quelli impostati sul soggetto
202600000000     C*
202700000000     C                   ENDSR
202800000000     C/EJECT
202900000000     C********************************************************************
203000170731     C* Inizializzo i dati anagrafici per conti estemporanei
203100000000     C********************************************************************
203200170731     C     InzDati_CPA   BEGSR
203300000000     C*
203400130903     C*  inizializzo la tipologia della natura giuridica da quella
203500130903     C*  del soggetto perchè non presente sui dati NDCPA00F
203600170731     C                   Clear                   $PartIVAMov
203700170731     C                   Clear                   $CdFiscMov
203800170731     C                   Clear                   $Stato1Mov
203900170731     C                   Clear                   $IdPaes1Mov
204000170731     C                   Clear                   $PIVA1Mov
204100170731     C                   Clear                   $CdFisc1Mov
204200170731     C                   Clear                   $CogRag1Mov
204300170731     C                   Clear                   $TpN1Mov
204400170731     C                   Clear                   $Denom1Mov
204500170731     C                   Clear                   $Cognom1Mov
204600170731     C                   Clear                   $Nome1Mov
204700170731     C                   Clear                   $Indir2Mov
204800170731     C                   Clear                   $NrCiv2Mov
204900170731     C                   Clear                   $CAP2Mov
205000170731     C                   Clear                   $Locali2Mov
205100170731     C                   Clear                   $Prov2Mov
205200170731     C                   Clear                   $Stato2Mov
205300170731     C                   Clear                   $IdPaes2Mov
205400000000     C*
205500000000     C                   ENDSR
205600000000     C/EJECT
205700000000     C********************************************************************
205800000000     C* Seleziono dati sul movimento del cliente/fornitore
205900000000     C********************************************************************
206000000000     C     SelNDMOV2     BEGSR
206100000000     C*
206200000000     C                   Select
206300000000     C*
206400170731     C                   When      $PartIVAMov <> PartIVAI52 and
206500170731     C                             PartIVAI52  <> *Blank
206600130826     C                   Eval      $OkMOV      = *Off
206700130826     C*
206800170731     C                   When      $CdFiscMov  <> CdFiscI52  and
206900170731     C                             CdFiscI52   <> *Blank
207000130826     C                   Eval      $OkMOV      = *Off
207100130826     C*
207200130826     C                   endSl
207300130826     C*
207400130826     C                   ENDSR
207500130826     C/EJECT
207600130826     C************************************************************
207700130826     C* Controllo i dati del movimento
207800130826     C************************************************************
207900170721     C     CtrNDMOV      BEGSR
208000000000     C*
208100000000     C                   EXSR      RepNDPAS
208200130826     C*
208300170816     C                   EXSR      ChkCP_Escl
208400170721     C*
208500170721     C                   EXSR      CtrCausale
208600170721     C*
208700170721     C                   EXSR      CtrClasse
208701170831R2652C*
208702170831R2652C                   EXSR      ChkAutof
208800130826     C*
208900000000     C                   ENDSR
209000000000     C/EJECT
209100000000     C************************************************************
209200000000     C* Reperimento dei dati di partita
209300000000     C************************************************************
209400000000     C     RepNDPAS      BEGSR
209500170724     C*
209600170724     C                   Clear                   NDPASSav
209700170724     C*
209800170724     C* ripristino i dati del movimento cliente/fornitore
209900170724     C                   Move      NDMOVSav      NDMOV00F
210000000000     C*
210100000000     C                   Eval      PASSys      = MOVSys
210200000000     C                   Eval      PASNrAsReg  = MOVNrAsReg
210300000000     C                   Eval      PASNrRigaM  = MOVNrRigaM
210400000000     C     K03PAS01      SETLL     NDPAS01L
210500000000     C     K03PAS01      READE     NDPAS01L                               21
210600170724     C                   if        *In21       = *On
210700170724     C                   Eval      $AnnMov     = *On
210800170724     C                   if        $TpAnn      = *Blank
210900170529     C                   Eval      $TpAnn      = '31'
211000000000     C                   endif
211100170724     C                   else
211200170724     C* salvo i dati della partita cliente/fornitore
211300170724     C                   Move      NDPAS00F      NDPASSav
211400170724     C                   endif
211500000000     C*
211600000000     C                   ENDSR
211700130826     C/EJECT
211800130826     C************************************************************
211900130826     C* Analisi della contropartita (costo/ricavo) per verificare
212000170816     C* se escludere o meno la contropartita e quindi il documento
212100130826     C************************************************************
212200170816     C     ChkCP_Escl    BEGSR
212300130826     C*
212400111026     C                   Eval      $KscEscl    = *Off
212500111026     C                   Eval      $KscIncl    = *Off
212600111026     C*
212700111026     C* reperisco le informazioni sulle righe di contropartita
212800170816     C                   EXSR      RepCP_Escl
212900111026     C*
213000111026     C* ripristino il movimento cliente/fornitore per la stampa errore
213100111026     C                   Move      NDMOVSav      NDMOV00F
213200111026     C*
213300111026     C* gestisco solo i casi fattibili, ovvero quelli in cui
213400111026     C* sono presenti solo conti inclusi
213500111026     C* Se ci sono conti esclusi e conti inclusi, annullo
213600111026     C* comunque il documento per indicare una gestione
213700111026     C* manuale
213800111026     C*
213900111026     C                   Select
214000111026     C*
214100111026     C*--------------
214200111026     C* se presenti solo conti inclusi, non faccio nulla
214300111026     C* oppure non esistono contropartite (es. registrazione
214400111026     C* di sola IVA per passaggio da aliquota 20 al 21%)
214500111026     C                   When      ($KscIncl        = *On  and
214600111026     C                              $KscEscl       <> *On     ) or
214700111026     C                             ($KscIncl       <> *On  and
214800111026     C                              $KscEscl       <> *On     )
214900111026     C*
215000111026     C*--------------
215100111026     C* se presenti conti inclusi e conti esclusi, annullo
215200111026     C                   When      $KscIncl        = *On  and
215300111026     C                             $KscEscl        = *On
215400111026     C*
215500111026     C* annullo la riga in elaborazione  e la registrazione
215600111026     C* in modo da annullare anche tutte le altre
215700170816     C                   Eval      $AnnIVA     = *On
215800170816     C                   Eval      $AnnMOV     = *On
215900170816     C                   if        $TpAnn      = *Blank
216000170816     C                   Eval      $TpAnn      = '48'
216100170816     C                   endif
216200111026     C*
216300111026     C*--------------
216400111026     C* se presenti solo conti esclusi, annullo
216500111026     C                   When      $KscIncl       <> *On  and
216600111026     C                             $KscEscl        = *On
216700111026     C*
216800111026     C* annullo la riga in elaborazione  e la registrazione
216900111026     C* in modo da annullare anche tutte le altre
217000170816     C                   Eval      $AnnIVA     = *On
217100170816     C                   Eval      $AnnMOV     = *On
217200170816     C                   if        $TpAnn      = *Blank
217300170816     C                   Eval      $TpAnn      = '47'
217400170816     C                   endif
217500111026     C*
217600111026     C                   endSl
217700130826     C*
217800130826     C                   ENDSR
217900130826     C/EJECT
218000130826     C************************************************************
218100130826     C* Reperimento tipologia di contropartita
218200130826     C************************************************************
218300170816     C     RepCP_Escl    BEGSR
218400130826     C*
218500111026     C* Lettura dei movimenti di contropartita legati alla registrazione
218600170724     C                   Eval      $CodOperCP  = '1'
218700111026     C                   EXSR      GetMOVCP
218800111026     C*
218900170724     C                   DoW       $CodOperCP <> '9'
219000111026     C*
219100170816     C                   EXSR      SelCP_Incl
219200170801     C*
219300170816     C                   if        $OkCP_Incl  = *On
219400170801     C* verifico se conto di controparita è da escludere nella determinazione
219500170801     C* della tipologia di operazione ( sconti, spese, commissioni)
219600170801     C*
219700170801     C* recupero l'eventuale voce di analitica per verificare
219800170801     C* se il movimento deve essere escluso tramite la voce
219900170801     C                   Eval      $Voce       = *Blank
220000170801     C*
220100170801     C* Scorro le righe di analitica della prima dimensione
220200170801     C* in ordine alfabetico. E' sufficiente scorrere le righe di
220300170801     C* una sola dimensione, in quanto se presenti anche più voci
220400170801     C* legate allo stesso conto, la definizione di beni/servizi
220500170801     C* o l'esclusione sarebbe logicamente la stessa in quanto
220600170801     C* le voci sono legate allo stesso conto
220700170801     C                   Eval      MOASys     = MOVSys
220800170801     C                   Eval      MOANrAsReg = MOVNrAsReg
220900170801     C                   Eval      MOANrRigaM = MOVNrRigaM
221000170801     C     K03MOA01      Setll     NDMOA01L
221100170801     C     K03MOA01      READE     NDMOA01L                               21
221200170801     C                   Eval      $FineMOA   = *In21
221300170801     C*
221400170801     C                   Eval      SPRSocieta  = XScSoc
221500170801     C                   Eval      SPRTipReg   = '6'
221600170801     C                   Eval      SPRSTpReg   = '2'
221700170801     C                   Eval      SPRTPREG    = *Blank
221800170801     C                   Eval      SPRLIBRO    = *Blank
221900170801     C                   Eval      SPRALIQ     = 0
222000170801     C                   Eval      SPRRIFIVA   = *Blank
222100170801     C                   Eval      SPRCAUSALE  = *Blank
222200170801     C                   Eval      SPRClasse   = *Blank
222300170801     C*
222400170801     C* la prima volta entra comunque, anche se non ha trovato
222500170801     C* il record di NDMOA, esce dal ciclo quando ha finito di
222600170801     C* leggere tutte le righe della prima dimensione
222700170801     C                   DoU       $FineMOA   = *On
222800170801     C*
222900170816     C                   if        $OkCP_Incl  = *On
223000170801     C                             and  $FineMOA <> *On
223100170801     C* 1° tentativo con KCC + KSC + VOCE
223200170801     C                   Eval      SPRKCC      = MOVKcc
223300170801     C                   Eval      SPRKSC      = MOVKsc
223400170801     C                   Eval      SPRVoce     = MOAVoce
223500170801     C                   EXSR      ChainRegola
223600170801     C                   if        *In21       = *Off
223700170801     C* se trovo il record significa che il conto è escluso
223800170801     C                   Eval      $KscEscl    = *On
223900170816     C                   Eval      $OkCP_Incl  = *Off
224000170801     C                   Eval      $FineMOA   = *On
224100170801     C                   endif
224200170801     C*
224300170801     C                   endif
224400170801     C*
224500170801     C* ---------------
224600170801     C*
224700170816     C                   if        $OkCP_Incl  = *On
224800170801     C* 2° tentativo con KCC + KSC
224900130823     C                   Eval      SPRKCC      = MOVKcc
225000130823     C                   Eval      SPRKSC      = MOVKsc
225100130823     C                   Eval      SPRVoce     = *Blank
225200170703     C                   EXSR      ChainRegola
225300111026     C                   if        *In21       = *Off
225400111026     C* se trovo il record significa che il conto è escluso
225500111026     C                   Eval      $KscEscl    = *On
225600170816     C                   Eval      $OkCP_Incl  = *Off
225700170801     C                   Eval      $FineMOA   = *On
225800170801     C                   endif
225900170801     C*
226000170801     C                   endif
226100170801     C*
226200170801     C* ---------------
226300170801     C*
226400170816     C                   if        $OkCP_Incl  = *On
226500170801     C                             and  $FineMOA <> *On
226600170801     C* 3° tentativo con KCC + VOCE
226700170801     C                   Eval      SPRKCC      = MOVKcc
226800170801     C                   Eval      SPRKSC      = *Blank
226900170801     C                   Eval      SPRVoce     = MOAVoce
227000170801     C                   EXSR      ChainRegola
227100170801     C                   if        *In21       = *Off
227200170801     C* se trovo il record significa che il conto è escluso
227300170801     C                   Eval      $KscEscl    = *On
227400170816     C                   Eval      $OkCP_Incl  = *Off
227500170801     C                   Eval      $FineMOA   = *On
227600170801     C                   endif
227700170801     C*
227800170801     C                   endif
227900170801     C*
228000170801     C* ---------------
228100170801     C*
228200170801     C* 4° tentativo solo con KCC
228300170816     C                   if        $OkCP_Incl  = *On
228400130823     C                   Eval      SPRKCC      = MOVKcc
228500130823     C                   Eval      SPRKSC      = *Blank
228600130823     C                   Eval      SPRVoce     = *Blank
228700170703     C                   EXSR      ChainRegola
228800111026     C                   if        *In21       = *Off
228900111026     C* se trovo il record significa che il conto è escluso
229000111026     C                   Eval      $KscEscl    = *On
229100170816     C                   Eval      $OkCP_Incl  = *Off
229200170801     C                   Eval      $FineMOA   = *On
229300170801     C                   endif
229400170801     C*
229500170801     C                   endif
229600170801     C*
229700170801     C* 5° tentativo solo con VOCE
229800170816     C                   if        $OkCP_Incl  = *On
229900170801     C                             and  $FineMOA <> *On
230000170801     C                   Eval      SPRKCC      = *Blank
230100170801     C                   Eval      SPRKSC      = *Blank
230200170801     C                   Eval      SPRVoce     = MOAVoce
230300170801     C                   EXSR      ChainRegola
230400170801     C                   if        *In21       = *Off
230500170801     C* se trovo il record significa che il conto è escluso
230600170801     C                   Eval      $KscEscl    = *On
230700170816     C                   Eval      $OkCP_Incl  = *Off
230800170801     C                   Eval      $FineMOA   = *On
230900170801     C                   endif
231000170801     C*
231100170801     C                   endif
231200170801     C*
231300170801     C                   if        $FineMOA    = *Off
231400170801     C* leggo solo i movimenti della prima dimensione su cui
231500170801     C* mi sono posizionata (vd. VL K04MOA01)
231600170801     C     K04MOA01      READE     NDMOA01L                               21
231700170801     C                   Eval      $FineMOA   = *In21
231800170801     C                   endif
231900170801     C*
232000170801     C                   EndDo
232100170801     C
232200170801     C*
232300170816     C                   if        $OkCP_Incl  = *On
232400111026     C* se NON trovo il record significa che il conto è incluso
232500111026     C                   Eval      $KscIncl    = *On
232600111026     C                   endif
232700111026     C*
232800160321     C                   endif
232900111026     C*
233000170724     C                   Eval      $CodOperCP  = '2'
233100111026     C                   EXSR      GetMOVCP
233200130826     C                   ENDDO
233300130826     C*
233400130826     C                   ENDSR
233500130826      /EJECT
233600130826     C****************************************************************
233700130826     C* Selezione dei movimenti di controparita
233800130826     C****************************************************************
233900170816     C     SelCP_Incl    BEGSR
234000130826     C*
234100170816     C                   Eval      $OkCP_Incl  = *On
234200111026     C*
234300111026     C                   Select
234400111026     C* seleziono solo i cespiti ammortizzabili e gli altri conti
234500111026     C* (costi e ricavi)
234600111026     C                   When      Not(MOVSNatura  = '4'  or
234700111026     C                                 MOVSNatura  = '9'    )
234800170816     C                   Eval      $OkCP_Incl  = *Off
234900111026     C*
235000111026     C                   endSl
235100111026     C*
235200130826     C                   ENDSR
235300130826      /EJECT
235400130826     C******************************************************
235500000000     C* Controllo movimento IVA
235600000000     C******************************************************
235700000000     C     ChkNDIVA      BEGSR
235800000000     C*
235900000000     C                   EXSR      InzNDIVA
236000110826     C*
236100000000     C                   EXSR      ChkLibro
236200000000     C*
236300000000     C* codice riferimento IVA (ANRIV00F)
236400000000     C                   EXSR      ChkRifIVA
236500000000     C*
236600000000     C*
236700000000     C*----------------------
236800000000     C* Associazione libri-riferimenti IVA (AnALR00f)
236900000000     C                   EXSR      ChkAssIVA
237000000000     C*
237100000000     C                   ENDSR
237200000000     C/EJECT
237300000000     C******************************************************
237400000000     c* Inizializzo le variabili per righe IVA del movimento
237500000000     C******************************************************
237600000000     C     InzNDIVA      BEGSR
237700000000     C*
237800170703     C                   Eval      $Differita  = *Off
237900170703     C*
238000000000     C* se il movimento è annullato , estraggo cmq le righe
238100000000     C* IVA del periodo e le scrivo annullate
238200000000     C                   Eval      $AnnIVA     = $AnnMOV
238300000000     C                   if        $AnnIVA    <> *On
238400000000     C                   Eval      $TpAnn      = *Blank
238500000000     C                   endif
238600000000     C*
238700000000     C                   ENDSR
238800000000     C/EJECT
238900000000     C******************************************************
239000000000     C* Controllo libro IVA
239100000000     C******************************************************
239200000000     C     ChkLibro      BEGSR
239300000000     C*
239400000000     C                   MoveL     IVASocieta    DLISocieta
239500000000     C                   MoveL     IVATpReg      DLITpReg
239600000000     C                   MoveL     IVALibro      DLILibro
239700000000     C     K03DLI01      CHAIN     ANDLI01L                           21
239800000000     C*
239900000000     C* libro IVA non trovato, annullo la riga movimento
240000000000     C                   IF        *In21       = *On
240100000000     C*
240200170801     C* se la registrazione ha più righe IVA con problemi per
240300170801     C* attribuire la natura, considero predominante l'annullamento
240400170801     C* per controlli specifici sulla riga IVA
240500170801     C                   Eval      $AnnIVA     = *On
240600170801     C                   if        $TpAnn      = *Blank
240700170801     C                             or
240800170801     C                             $TpAnn      = '20'
240900000000     C                   Eval      $TpAnn      = '10'
241000000000     C                   endif
241100000000     C*
241200000000     C                   ENDIF
241300000000     C*
241400000000     C*----------------------------------
241500000000     C*
241600000000     C* verifico se il libro IVA è da escludere
241700130823     C                   Eval      SPRSocieta  = XScSoc
241800170616     C                   Eval      SPRTipReg   = '6'
241900170616     C                   Eval      SPRSTpReg   = '3'
242000130823     C                   Eval      SPRTPREG    = DLITpReg
242100130823     C                   Eval      SPRLIBRO    = DLILibro
242200130823     C                   Eval      SPRALIQ     = 0
242300130823     C                   Eval      SPRRIFIVA   = *Blank
242400130823     C                   Eval      SPRCAUSALE  = *Blank
242500130823     C                   Eval      SPRKCC      = *Blank
242600130823     C                   Eval      SPRKSC      = *Blank
242700130823     C                   Eval      SPRVoce     = *Blank
242800130823     C                   Eval      SPRClasse   = *Blank
242900170703     C                   EXSR      ChainRegola
243000000000     C                   if        *In21       = *Off
243100000000     C* se trovo il record significa che il libro è escluso
243200170801     C* se la registrazione ha più righe IVA con problemi per
243300170801     C* attribuire la natura, considero predominante l'annullamento
243400170801     C* per controlli specifici sulla riga IVA
243500170801     C                   Eval      $AnnIVA     = *On
243600170801     C                   if        $TpAnn      = *Blank
243700170801     C                             or
243800170801     C                             $TpAnn      = '20'
243900000000     C                   Eval      $TpAnn      = '26'
244000000000     C                   endif
244100000000     C                   endif
244200000000     C*
244300000000     C*----------------------------------
244400000000     C*
244401170831R2652X****               EXSR      ChkAutof
244600000000     C*
244700000000     C*----------------------------------
244800000000     C*
244900000000     C*  Libro per IVA sospesa/differita già liquidata
245000000000     C*
245100000000     C* 3 = IVA norm. da sos. (f.31/12/97)
245200000000     C                   if        DLISospens  = '3'   or
245300170801     C* 5 = Sola liquidazione
245400170801     C                             DLISospens  = '5'
245500170801     C* se la registrazione ha più righe IVA con problemi per
245600170801     C* attribuire la natura, considero predominante l'annullamento
245700170801     C* per controlli specifici sulla riga IVA
245800170801     C                   Eval      $AnnIVA     = *On
245900170801     C                   if        $TpAnn      = *Blank
246000170801     C                             or
246100170801     C                             $TpAnn      = '20'
246200170801     C                   Eval      $TpAnn      = '03'
246300000000     C                   endif
246400000000     C                   endif
246500170703     C*
246600170704     C*
246700170703     C                   Eval      $Differita  = *Off
246800170704     C* 1 = IVA sospesa (fino 31/12/97)
246900170704     C                   if        DLISospens  = '1'     or
247000170704     C* 4 = IVA differita (da 01/01/98)
247100170704     C                             DLISospens  = '4'
247200170703     C                   Eval      $Differita  = *On
247300170703     C                   endif
247400000000     C*
247500000000     C                   ENDSR
247600000000     C/EJECT
247700000000     C********************************************************************
247800000000     C* Controllo se registrazione di autofattura
247900000000     C********************************************************************
248000000000     C     ChkAutof      BEGSR
248100000000     C*
248200000000     C* Se fornitore + registro vendite + territorialità libro '1' (CEE)
248300000000     C* significa che sto trattando un'autofattura (che devo ESCLUDERE !!!)
248400000000     C* Con questo test escludo anche le autofatture gestite senza
248500000000     C* automatismi di primanota
248600000000     C* N.B. se la causale ha tipo registro 2=Vendite, il fornitore in DARE
248700000000     C*      NON è ammesso, a meno che il tipo documento sia 8=Autofatture
248800000000     C*      In questo caso, la causale è automaticamente esclusa dalla
248900000000     C*      comunicazione. Quindi, questo test secondo me NON serve mai
249000000000     C*
249100000000     C                   if        MOVSNatura  = 'F'   and
249200000000     C                             IVATpReg    = '2'   and
249300000000     C                             DLITerrito  = '1'
249400170801     C*
249500170801     C* dico che è autofattura, non annullo più
249600170801     C                   Eval      $Autofatt   = *On
249700170801     C                   endif
249800170801     C*
249900170801     C* ---------------
250000170801     C* AUTOFATTURA
250100170801     C* verifico se si tratta di fattura di vendita per OMAGGI,
250200170801     C* AUTOCONSUMO (può essere anche fattura di reverse charge)
250300170801     C* In questo caso il cliente è sempre la società che emette
250400170801     C* la fattura
250500170801     C                   if        IVATpReg    = '2'
250600170801     C                   EXSR      ChkAutoCons
250700170801     C                   endif
250800170801     C*
250900170801     C*
251000170801     C                   if        $Autofatt   = *On
251100170801     C* STAMPA AUTOFATTURA
251200000000     C                   EXSR      StpAutof
251300000000     C                   endif
251400000000     C*
251500000000     C                   ENDSR
251600000000     C/EJECT
251700170728     C************************************************************
251800170728     C* Verifico se si tratta di autofattura per autoconsumo
251900170728     C************************************************************
252000170728     C     ChkAutocons   BEGSR
252100170728     C*
252200170731     C                   if        ($CdFiscMov   = $CdFiscSoc  and
252300170731     C                              $CdFiscMov  <> *Blank         )or
252400170731     C                             ($PartIVAMov  = $PartIVASoc and
252500170731     C                              $PartIVAMov <> *Blank         )
252600170728     C*
252700150420     C                   Eval      $Autocons   = *On
252800150420     C                   Eval      $Autofatt   = *On
252900170728     C*
253000150420     C                   endif
253100150420     C*
253200170728     C                   ENDSR
253300170728     C/EJECT
253400000000     C******************************************************
253500000000     C* Controllo riferimento IVA
253600000000     C******************************************************
253700000000     C     ChkRifIVA     BEGSR
253800000000     C*
253900000000     C                   Move      IVASocieta    RIVSocieta
254000000000     C                   Move      IVATpReg      RIVTpReg
254100000000     C                   Move      IVAAliq       RIVAliq
254200000000     C                   Move      IVARifIVA     RIVRifIVA
254300000000     C     K04RIV01      CHAIN     ANRIV01L                           21
254400000000     C                   if        *In21       = *On
254500170801     C* se la registrazione ha più righe IVA con problemi per
254600170801     C* attribuire la natura, considero predominante l'annullamento
254700170801     C* per controlli specifici sulla riga IVA
254800170801     C                   Eval      $AnnIVA     = *On
254900170801     C                   if        $TpAnn      = *Blank
255000170801     C                             or
255100170801     C                             $TpAnn      = '20'
255200170801     C                   Eval      $TpAnn      = '09'
255300170801     C                   endif
255400170801     C                   endif
255500170801     C*
255600000000     C                   ENDSR
255700000000     C/EJECT
255800000000     C******************************************************
255900000000     C* Controllo associazione libri IVA
256000000000     C******************************************************
256100000000     C     ChkAssIVA     BEGSR
256200000000     C*
256300000000     C                   Move      IVASocieta    ALRSocieta
256400000000     C                   Move      IVATpReg      ALRTpReg
256500000000     C                   Move      IVALibro      ALRLibro
256600000000     C                   z-add     IVAAliq       ALRAliq
256700000000     C                   Move      IVARifIVA     ALRRifIVA
256800000000     C     K05ALR01      CHAIN     ANALR01L                           21
256900000000     C                   EXSR      SelANALR
257000000000     C*
257100000000     C                   ENDSR
257200000000     C/EJECT
257300000000     C************************************************************
257400000000     C* Selezione del movimento in base alle regole su associazione
257500000000     C************************************************************
257600000000     C     SelANALR      BEGSR
257700000000     C*
257800000000     C                   Select
257900000000     C*-----------
258000000000     C* associazione inesistente
258100000000     C                   WHEN      *In21       = *On
258200170801     C* se la registrazione ha più righe IVA con problemi per
258300170801     C* attribuire la natura, considero predominante l'annullamento
258400170801     C* per controlli specifici sulla riga IVA
258500170801     C                   Eval      $AnnIVA     = *On
258600170801     C                   if        $TpAnn      = *Blank
258700170801     C                             or
258800170801     C                             $TpAnn      = '20'
258900170801     C                   Eval      $TpAnn      = '11'
259000170801     C                   endif
259100170801     C*
259200170801     C*-----------
259300170801     C* associazione non associata
259400170801     C                   WHEN      ALRAnn      = '1'
259500170801     C* se la registrazione ha più righe IVA con problemi per
259600170801     C* attribuire la natura, considero predominante l'annullamento
259700170801     C* per controlli specifici sulla riga IVA
259800170801     C                   Eval      $AnnIVA     = *On
259900170801     C                   if        $TpAnn      = *Blank
260000170801     C                             or
260100170801     C                             $TpAnn      = '20'
260200170801     C                   Eval      $TpAnn      = '12'
260300000000     C                   endif
260400000000     C*
260500000000     C*-----------
260600170801     C* movimento da escludere dalla comunicazione
260700170801     C                   When      ALRElenchi = '01'
260800170801     C*
260900170801     C* se la registrazione ha più righe IVA con problemi per
261000170801     C* attribuire la natura, considero predominante l'annullamento
261100170801     C* per controlli specifici sulla riga IVA
261200170810     C**************     Eval      $AnnIVA    = *On
261300170801     C                   if        $TpAnn      = *Blank
261400170801     C                             or
261500170801     C                             $TpAnn      = '20'
261600170810     C**************     Eval      $TpAnn      = '02'
261700000000     C                   endif
261800000000     C*
261900000000     C                   endSl
262000000000     C*
262100000000     C                   ENDSR
262200000000     C/EJECT
262300170721     C************************************************************
262400170721     C* Controllo  causale
262500170721     C************************************************************
262600170721     C     CtrCausale    BEGSR
262700170721     C*
262800170721     C                   Eval      $Causale    = MOVCausTes
262900170721     C                   EXSR      CallMVC003
263000170721     C                   if        $Esito      = *On
263100170721     C*
263200170721     C* se la causale non esiste, annullo il documento
263300170721     C                   Eval      $AnnMOV     = *On
263400170721     C                   if        $TpAnn      = *Blank
263500170721     C                   Eval      $TpAnn      = '04'
263600170721     C                   endif
263700170721     C*
263800170721     C                   else
263900170721     C*
264000170721     C                   Select
264100170721     C* la sottotipologia deve essere per Clienti o Fornitori
264200170721     C                   When      OPESTP     <> 'C'     and
264300170721     C                             OPESTP     <> 'F'
264400170721     C                   Eval      $AnnMOV     = *On
264500170721     C                   if        $TpAnn      = *Blank
264600170721     C                   Eval      $TpAnn      = '24'
264700170721     C                   endif
264800170721     C*
264900170721     C* la tipologia deve essere per registrazioni IVA
265000170721     C                   When      OPETPC     <> '1'
265100170721     C                   Eval      $AnnMOV     = *On
265200170721     C                   if        $TpAnn      = *Blank
265300170721     C                   Eval      $TpAnn      = '24'
265400170721     C                   endif
265500170721     C*
265600170801     C*
265700170801     C* Da chiarimenti dell'Agenzia delle Entrate, come principio
265800170801     C* generale, vanno comunicate come operazioni Attive e con
265900170801     C* i riferimenti anagrafici della società comunicante (quadro BL
266000170801     C* o SE).
266100170801     C* Le autofatture CEE di Paesi INTRA, verranno annullate per
266200170801     C* INTRA, sarà poi a carico dell'utente riattivare le righe
266300170801     C* eventualmente non comunicate in INTRA.
266400170801     C* Rimangono le autofatture di Paesi EXTRACEE: se riferite a Beni
266500170801     C* verranno annullate automaticamente perchè passando da Dogana
266600170801     C* è presente la bolla doganle; se riferite a Servizi, a meno di
266700170801     C* altri criteri di annullamento, saranno da comunicare.
266800170801     C                   When      OPETpDocI   = '8'
266900170801     C*
267000170801     C* dico che è autofattura
267100170801     C                   Eval      $Autofatt   = *On
267200170801     C                   endSl
267300170801     C*
267400170801     C                   endif
267500170721     C*
267600170721     C* ---------------
267700170721     C* verifico se causale è da escludere dalla comunicazione
267800170721     C                   Eval      SPRSocieta  = XScSoc
267900170721     C                   Eval      SPRTipReg   = '6'
268000170721     C                   Eval      SPRSTpReg   = '1'
268100170721     C                   Eval      SPRTPREG    = *Blank
268200170721     C                   Eval      SPRLIBRO    = *Blank
268300170721     C                   Eval      SPRALIQ     = *Zero
268400170721     C                   Eval      SPRRIFIVA   = *Blank
268500170721     C                   Eval      SPRCAUSALE  = MOVCausTes
268600170721     C                   Eval      SPRKCC      = *Blank
268700170721     C                   Eval      SPRKSC      = *Blank
268800170721     C                   Eval      SPRVoce     = *Blank
268900170721     C                   Eval      SPRClasse   = *Blank
269000170721     C                   EXSR      ChainRegola
269100170721     C                   if        *In21       = *Off
269200170721     C* se trovo il record significa che la causale è esclusa
269300170721     C                   Eval      $AnnMOV     = *On
269400170721     C                   if        $TpAnn      = *Blank
269500170721     C                   Eval      $TpAnn      = '13'
269600170721     C                   endif
269700170721     C                   endif
269800170721     C*
269900170721     C                   ENDSR
270000170721     C/EJECT
270100170721     C************************************************************
270200170721     C* Reperimento dati di causale operativa
270300170721     C************************************************************
270400170721     C     CallMVC003    BEGSR
270500170721     C*
270600170801     C                   Clear                   ANOPE
270700170801     C*
270800170801     C     MOVDtReg      AddDur    1:*D          WData
270900170801     C                   Move      WData         WDtAlfa
271000170801     C*
271100170801     C* la causale deve essere valida alla data di registrazione
271200170801     C* passo quindi data di registrazione + un giorno
271300170801     C
271400170801     C*
271500170721     C*
271600170721     C                   Eval      $LenOut     = %Size(ANOPE)
271700170721     C                   CallB     'NDMVC003'
271800170721     C                   Parm                    XScSoc
271900170801     C                   Parm                    $Causale
272000170801     C                   Parm                    WDtAlfa
272100170721     C                   Parm      '0'           $Uso              1
272200170721     C                   Parm      *OFF          $GesMsg
272300170721     C                   Parm      *ON           $Esito
272400170721     C                   Parm      'ANOPE     '  $Struttura
272500170721     C                   Parm                    ANOPE
272600170721     C                   Parm                    $LenOut
272700170721     C*
272800170721     C                   ENDSR
272900170721     C/EJECT
273000170721     C************************************************************
273100170721     C* Controllo  classe
273200170721     C************************************************************
273300170721     C     CtrClasse     BEGSR
273400170721     C*
273500170721     C* verifico se classe è da escludere dalla comunicazione
273600170721     C                   Eval      SPRSocieta  = XScSoc
273700170721     C                   Eval      SPRTipReg   = '6'
273800170721     C                   Eval      SPRSTpReg   = '4'
273900170721     C                   Eval      SPRTPREG    = *Blank
274000170721     C                   Eval      SPRLIBRO    = *Blank
274100170721     C                   Eval      SPRALIQ     = *Zero
274200170721     C                   Eval      SPRRIFIVA   = *Blank
274300170721     C                   Eval      SPRCAUSALE  = *Blank
274400170721     C                   Eval      SPRKCC      = *Blank
274500170721     C                   Eval      SPRKSC      = *Blank
274600170721     C                   Eval      SPRVoce     = *Blank
274700170721     C                   Eval      SPRClasse   = MOVClasse
274800170721     C                   EXSR      ChainRegola
274900170721     C                   if        *In21       = *Off
275000170721     C* se trovo il record significa che la classe è esclusa
275100170721     C                   Eval      $AnnMOV     = *On
275200170721     C                   if        $TpAnn      = *Blank
275300170721     C                   Eval      $TpAnn      = '27'
275400170721     C                   endif
275500170721     C                   endif
275600170721     C*
275700170721     C                   ENDSR
275800170721      /EJECT
275900000000     C************************************************************
276000170529     C* Scrivo SDGCST00F a dettaglio di NDIVA00F
276100000000     C************************************************************
276200170529     C     GesCST_IVA    BEGSR
276300170811fm3  C*
276400170811fm3  C                   Eval      $Stato1Mov  = *Blank
276500170811fm3  C                   Eval      $IdPaes1Mov = *Blank
276600170811fm3  C                   Eval      $PIVA1Mov   = $PartIVAMov
276700170811fm2  C                   Eval      $CdFisc1Mov = $CdFiscMov
276800170810     C*
276900170810     C                   Select
277000170810     C*
277100170810     C* solo per il 2017,
277200170810     C* se Dogana e non trovato il legame con la fattura,
277300170810     C* imposto codici fittizi
277400170810     C                   WHEN      AnnoI52     = 2017     and
277500170810     C                             $CF         = 'F'      and
277600170810     C                             AlleIVA002  = '02'     and
277700170810     C                             $LegameBF  <> *On
277800170810     C*
277900170810     C                   Eval      $Stato1Mov  = *Blank
278000170810     C                   Eval      $IdPaes1Mov = 'OO'
278100170810ga1  C****               Eval      $PIVA1Mov   = *All'9'
278200170810ga1  C                   Eval      $PIVA1Mov   = '99999999999'
278300170810     C                   Eval      $CdFisc1Mov = *Blank
278400170810fm2  C*
278500170810fm2  C                   EXSR      ScanChar
278600170810     C*
278700170810     C* -----
278800170810     C                   WHEN      $PIVA1Mov   = *Blank    and
278900170811fm3  C                             $CdFisc1Mov = *Blank    and
279000170810     C                             $IdPaes1Mov = *Blank    and
279100170810     C                             $IdPaes2Mov <> *Blank   and
279200170810     C                             $IdPaes2Mov <> 'IT'
279300170810     C
279400170810     C* Se la Sede è ExtraCee, recupero l'IdPaese dall'IdPaese
279500170810     C* della Sede Legale (nel 99% dei casi andrà bene)
279600170810     C*
279700170810fm2  C                   Eval      $Stato1Mov  = $Stato2Mov
279800170810     C                   Eval      $IdPaes1Mov = $IdPaes2Mov
279900170810     C*
280000170811fm3  C                   if        $IdPaes1Mov <>*Blank
280100170811fm3  C                   Eval      WIdPaese    = $IdPaes1Mov
280200170810     C                   EXSR      ChkIntra
280300170810     C                   Eval      $Intra      = WIntra
280400170811fm3  C                   else
280500170811fm3  C                   Eval      $Intra      = *Off
280600170811fm3  C                   endif
280700170810     C*
280800170810     C* se anche la SEDE è extracomunicaria,
280900170810     C* imposto l'identificativo fiscale uguale al kcc + ksc
281000170810     C                   if        $Intra      <> *On      and
281100170811fm3  C                             $PIVA1Mov   = *Blank    and
281200170810     C                             $IdPaes1Mov <> 'IT'
281300170810     C                   Eval      $PIVA1Mov   = MOVKcc + MOVKsc
281400170810     C                   endif
281500170810fm2  C*
281600170810fm2  C                   EXSR      ScanChar
281700170810     C*
281800170810     C                   OTHER
281900170810     C*
282000170810     C* reperisco i dati anagrafici del primo record
282100170810     C* inserito con stessa partita IVA e codice fiscale
282200170810     C                   if        $PartIVAMov <> *Blank   or
282300170810     C                             $CdFiscMov  <> *Blank
282400170810     C*
282500170810     C                   EXSR      RepAnagraf
282600170810     C*
282700170810     C                   endif
282800170810     C*
282900170810fm1  C*
283000170810fm1  C                   if        (($PartIVAMov <> *Blank   or
283100170810fm1  C                               $CdFiscMov  <> *Blank     ) and
283200170810fm1  C                              Found0704  <> *On                )
283300170810fm1  C                             OR
283400170810fm1  C                             (($PartIVAMov  = *Blank   and
283500170810fm1  C                               $CdFiscMov   = *Blank     ))
283600170810fm2  C*
283700170810fm2  C* controllo il codice fiscale e la partita IVA
283800170810fm2  C                   EXSR      CtrFiscIVA
283900170810fm2  C*
284000170810fm2  C                   EXSR      ScanChar
284100170810fm1  C                   endif
284101170906R2656C                   EXSR      ChkNomPerF
284200170810fm2  C*
284300170810fm2  C                   EXSR      ChkDatiFisc
284400170810     C*
284500170810     C* il codice fiscale è presente solo per residenti in Italia
284600170810     C                   if        $CdFisc1Mov <> *Blank    and
284700170810     C                             $IdPaes2Mov <>  'IT'
284800170810     C                   Eval      $CdFisc1Mov = *Blank
284900170810     C                   endif
285000170810     C*
285100170810     C                   endSl
285200170811     C*
285300170721     C                   EXSR      SelDatiFisc
285400170721     C*
285500170721     C                   if        $WriteCST   = *On
285600170718     C*
285700170721     C* deve essere valorizzato almeno uno dei 2 codici
285701170830R2651X****               if        $IdPaes1Mov = *Blank   and
285702170830 "   X****                         $PIVA1Mov   = *Blank   and
285703170830R2651X****                         $CdFisc1Mov = *Blank
285704170830R2651C*
285705170830 "   C* Italia
285706170830 "   C                   if        ($IdPaes1Mov = 'IT'         or
285707170830 "   C* Paese CEE
285708170830 "   C                              $Intra      = *On          or
285709170830 "   C* Paese extra CEE
285710170830 "   C                              ($IdPaes1Mov = *Blank  and
285711170830R2651C                               $Intra     <> *On        )  ) and
285712170830BRT  C***  ex R2651                $PIVA1Mov   = *Blank            and
285713170830BRT  C                              ($PIVA1Mov   = *Blank  or
285714170830BRT  C                               $PIVA1Mov   = '$$' )  and
285715170830BRT  C***  ex R2651                $CdFisc1Mov = *Blank
285716170830BRT  C                              ($CdFisc1Mov   = *Blank  or
285717170830BRT  C                               $CdFisc1Mov   = '$$' )
285718170830     C*
286100170801     C*
286200170801     C* se la registrazione ha più righe IVA con problemi per
286300170801     C* attribuire la natura, considero predominante l'annullamento
286400170801     C* per controlli specifici sulla riga IVA
286500170801     C                   Eval      $AnnIVA     = *On
286600170801     C                   if        $TpAnn      = *Blank
286700170801     C                             or
286800170801     C                             $TpAnn      = '20'
286900170801     C* Partita IVA/Cd.Fisc. mancante
287000170718     C                   Eval      $TpAnn      = '17'
287100170718     C                   endif
287101170830     C*
287300170718     C                   Select
287400170718     C*
287500170728     C* conto normale: stampo solo a cambio di data documento e conto
287600170718     C                   When      Tecnico002 <> '2'          and
287700170718     C                             (MOVDtDoc   <> SAVDtDoc2    or
287800170718     C                              MOVKcc     <> SAVKcc2      or
287900170718     C                              MOVKsc     <> SAVKsc2          )
288000170718     C*
288100170720     C                   Eval      $AnnConto   = *On
288200170718     C                   Move      MOVDtDoc      SAVDtDoc2
288300170718     C                   MoveL     MOVKcc        SAVKcc2
288400170718     C                   MoveL     MOVKsc        SAVKsc2
288500170718     C                   EXSR      StpNoFisc
288600170718     C*
288700170718     C* conto tecnico
288800170718     C                   When      Tecnico002 = '2'
288900170720     C                   Eval      $AnnMOV     = *On
289000170718     C                   EXSR      StpNoFisc
289100170718     C*
289200170718     C                   endsl
289300170718     C*
289400170718     C                   endif
289500170718     C*
289600000000     C*
289700170529     C                   EXSR      RieSDGCST
289800170718     C                   endif
289900170710     C*
290000170801     C                   if        $WriteCST   = *On
290100170801     C*
290200170801     C                   if        CSTTpAnn   <> *Blank
290300170801     C                   Eval      $TpAnn      = CSTTpAnn
290400170801     C                   EXSR      StpErr1
290500170801     C                   endif
290600170801     C*
290601170830BRT  C** gestisco casi particolari BRT
290602170906BRT  C                   exsr      BRT
290605170906     C*
290700170801     C                   WRITE     SDGCST000
290800170724     C*
290900170731     C*
291000170731     C                   if        CSTSocieta <> SAV5Societ or
291100170731     C                             CSTTpCom   <> SAV5TpCom  or
291200170731     C                             CSTTpRec   <> SAV5TpRec  or
291300170731     C                             CSTAnno    <> SAV5Anno   or
291400170731     C                             CSTTrim    <> SAV5Trim   or
291500170731     C                             CSTMese    <> SAV5Mese   or
291600170731     C                             CSTPrgEstr <> SAV5PrgEst
291700170731     C*
291800170731     C* controllo se esiste il catologo file per il trimestre
291900170731     C                   EXSR      CtrExistCSF
292000170731     C*
292100170731     C                   Move      CSTSocieta    SAV5Societ
292200170731     C                   Move      CSTTpCom      SAV5TpCom
292300170731     C                   Move      CSTTpRec      SAV5TpRec
292400170731     C                   Move      CSTAnno       SAV5Anno
292500170731     C                   Move      CSTTrim       SAV5Trim
292600170731     C                   Move      CSTMese       SAV5Mese
292700170731     C                   Move      CSTPrgEstr    SAV5PrgEst
292800170731     C                   endif
292900170731     C*
293000170731     C                   endif
293100000000     C*
293200000000     C                   ENDSR
293300000000     C/EJECT
293400170710     C********************************************************************
293500170724     C* Controllo se il catologo per il periodo
293600170710     C********************************************************************
293700170724     C     CtrExistCSF   BEGSR
293800170710     C*
293900170724     C                   Eval      CSFSocieta  = CSTSocieta
294000170724     C                   Eval      CSFTpCom    = CSTTpCom
294100170810     C                   Eval      CSFTpRec    = CSTTpRec
294200170724     C                   Eval      CSFAnno     = CSTAnno
294300170724     C                   Eval      CSFTrim     = CSTTrim
294400170724     C                   Eval      CSFMese     = CSTMese
294500170724     C                   Eval      CSFPrgEstr  = CSTPrgEstr
294600170724     C*
294700170724     C     K07CSF03      CHAIN     SDGCSF03L                          21
294800170724     C*
294900170724     C* se non trovato,
295000170724     C* scrivo un record per il periodo
295100170724     C                   if        *In21       = *On
295200170724     C*
295300170724     C                   Clear                   SDGCSF000
295400170724     C*
295500170724     C                   Eval      CSFUteImm   = CSTUteImm
295600170724     C                   Eval      CSFDtImm    = CSTDtImm
295700170724     C                   Eval      CSFUteUlMo  = CSTUteUlMo
295800170724     C                   Eval      CSFDtUlMo   = CSTDtUlMo
295900170724     C*
296000170727     C                   Eval      CSFCF       = *Blank
296100170724     C* Automatica
296200170724     C                   Eval      CSFOrigine  = CSTOrigine
296300170724     C                   Eval      CSFSocieta  = CSTSocieta
296400170724     C                   Eval      CSFTpCom    = CSTTpCom
296500170810     C                   Eval      CSFTpRec    = CSTTpRec
296600170724     C                   Eval      CSFAnno     = CSTAnno
296700170724     C                   Eval      CSFTrim     = CSTTrim
296800170724     C                   Eval      CSFMese     = CSTMese
296900170724     C*
297000170724     C                   Eval      CSFPrgEstr  = CSTPrgEstr
297100170724     C*
297200170724     C                   MoveL     AnnoI52       $Anno
297300170724     C                   MoveL     TrimI52       $Trim
297400170724     C                   Eval      CSFNote     = 'Spesometro Trimestrale'+
297500170724     C                             ' Dati estratti ' + $Anno + ' trim. ' +
297600170724     C                             $Trim
297700170724     C*
297800170724     C                   WRITE     SDGCSF000
297900170724     C*
298000170724     C                   endif
298100170724     C*
298200170724     C                   ENDSR
298300170724     C/EJECT
298400000000     C************************************************************
298500170721     C* Selezione dei dati fiscali
298600000000     C************************************************************
298700170721     C     SelDatiFisc   BEGSR
298800000000     C*
298900170721     C* applico i filtri anagrafici sui dati "drogati"
299000170721     C                   Select
299100170721     C*
299200170731     C                   When      $Stato2Mov <> Stato2I52  and
299300170721     C                             Stato2I52  <> *Blank
299400170721     C                   Eval      $WriteCST   = *Off
299500170721     C*
299600170721     C                   endSl
299700170721     C*
299800170721     C                   ENDSR
299900170721     C/EJECT
299901170830     C************************************************************
300100170721     C* Riempio campi descrittivi di SDGCST00F
300200170721     C************************************************************
300300170721     C     RieSDGCST     BEGSR
300400170721     C*
300500170724     C* ripristino i dati del movimento del cliente/fornitore
300600170724     C                   MoveL     NDMOVSav      NDMOV00F
300700170724     C*
300800170724     C* ripristino dati regisrtaz. del movimento cliente/fornitore
300900170724     C                   Move      NDREGSav      NDREG00F
301000170724     C*
301100170724     C* ripristino dati partita del movimento cliente/fornitore
301200170724     C                   Move      NDPASSav      NDPAS00F
301300170724     C*
301400170529     C                   Clear                   SDGCST000
301500000000     C*
301600170724     C* prendo i dati di ProJ
301700170724     C* del cliente/fornitore originale (non drogato per
301800170724     C* bolle o autofattura)
301900000000     C                   Eval      WKcc        = MOVKcc
302000000000     C                   Eval      WKsc        = MOVKsc
302100170724     C                   Eval      WData       = MOVDtDoc
302200000000     C                   EXSR      CallMVC002
302300170710     C*
302400170710     C                   Eval      CSTUteImm   = XScCUt
302500170710     C                   Eval      CSTDtImm    = UDateIso
302600170710     C                   Eval      CSTUteUlMo  = XScCUt
302700170710     C                   Eval      CSTDtUlMo   = UDateIso
302800000000     C*
302900130821     C* Automatica
303000170710     C                   Eval      CSTOrigine  = 'A'
303100170710     C                   Eval      CSTSocieta  = XScSoc
303200170703     C                   Eval      CSTTpCom    = TpComI52
303300170720     C                   Eval      CSTTpRec    = TpRecI52
303400170615     C                   Eval      CSTAnno     = AnnoI52
303500170703     C                   Eval      CSTTrim     = TrimI52
303600170703     C                   Eval      CSTMese     = MeseI52
303700170720     C*
303800170720     C                   Eval      CSTPrgEstr  = 0
303900170720     C*
304000170529     C                   Eval      CSTCF       = MOVSNatura
304100170710     C                   Eval      CSTSogg     = Sogg002
304200000000     C*
304300170731     C                   Eval      CSTPartIVA  = $PartIVAMov
304400170731     C                   Eval      CSTCdFisc   = $CdFiscMov
304500170717     C*
304600170731     C                   Eval      CSTStato1   = $Stato1Mov
304700170731     C                   Eval      CSTIdPaes1  = $IdPaes1Mov
304800170731     C                   Eval      CSTpIVA1    = $PIVA1Mov
304900170731     C                   Eval      CSTCdFisc1  = $CdFisc1Mov
305000000000     C*
305100170731     C                   Eval      CSTCogRag1  = $CogRag1Mov
305200170731     C                   Eval      CSTTpN1     = $TpN1Mov
305300170731     C                   Eval      CSTDenom1   = $Denom1Mov
305400170731     C                   Eval      CSTCognom1  = $Cognom1Mov
305500170731     C                   Eval      CSTNome1    = $Nome1Mov
305600170710     C*
305700170731     C                   Eval      CSTIndir2   = $Indir2Mov
305800170731     C                   Eval      CSTNrCiv2   = $NrCiv2Mov
305900170731     C                   Eval      CSTCAP2     = $CAP2Mov
306000170731     C                   Eval      CSTLocali2  = $Locali2Mov
306100170731     C                   Eval      CSTProv2    = $Prov2Mov
306200170731     C                   Eval      CSTStato2   = $Stato2Mov
306300170731     C                   Eval      CSTIdPaes2  = $IdPaes2Mov
306400170720     C*
306500170731     C                   Eval      CSTIndir3   = $Indir3Mov
306600170731     C                   Eval      CSTNrCiv3   = $NrCiv3Mov
306700170731     C                   Eval      CSTCAP3     = $CAP3Mov
306800170731     C                   Eval      CSTLocali3  = $Locali3Mov
306900170731     C                   Eval      CSTProv3    = $Prov3Mov
307000170731     C                   Eval      CSTStato3   = $Stato3Mov
307100170731     C                   Eval      CSTIdPaes3  = $IdPaes3Mov
307200170720     C*
307300170731     C                   Eval      CSTStato4   = $Stato4Mov
307400170731     C                   Eval      CSTIdPaes4  = $IdPaes4Mov
307500170731     C                   Eval      CSTpIVA4    = $PIVA4Mov
307600170731     C                   Eval      CSTCogRag4  = $CogRag4Mov
307700170731     C                   Eval      CSTNome4    = $Nome4Mov
307800000000     C*
307900170718     C                   Eval      CSTKcc      = MOVKcc
308000170718     C                   Eval      CSTKsc      = MOVKsc
308100000000     C*
308200000000     C* uso la causale di testata perchè quella di riga sul
308300000000     C* movimento del cliente o fornitore non è modificabile
308400000000     C* e in caso di doc.IVA coincide con quella di testata
308500170718     C                   Eval      CSTCausale  = MOVCausTes
308600000000     C*
308700170718     C                   Eval      CSTAliq     = IVAAliq
308800170718     C                   Eval      CSTRifIVA   = IVARifIVA
308900170720     C*
309000170720     C                   Eval      CSTTpReg    = IVATpReg
309100170720     C                   Eval      CSTLibro    = IVALibro
309200000000     C*
309300170718     C                   Eval      CSTDtDoc    = MOVDtDoc
309400170718     C                   Eval      CSTNrDoc    = MOVNrDoc
309500170718     C                   Eval      CSTSerDoc   = MOVSerDoc
309600000000     C*
309700170724     C                   Eval      CSTDtReg    = REGDtReg
309800170724     C                   Eval      CSTNrReg    = REGNrReg
309900170724     C                   Eval      CSTSerReg   = REGSerReg
310000000000     C*
310100170724     C                   Eval      CSTDtPar    = PASDtPar
310200170724     C                   Eval      CSTNrPar    = PASNrPar
310300170724     C                   Eval      CSTSerPar   = PASSerPar
310400170720     C*
310500170720     C                   Eval      CSTDtAnnot  = IVADtAnnot
310600000000     C*
310700170718     C                   Eval      CSTSys      = MOVSys
310800170718     C                   Eval      CSTNrAsReg  = MOVNrAsReg
310900170718     C                   Eval      CSTNrRigaM  = IVANrRigaM
311000170718     C                   Eval      CSTNrRiga   = IVANrRiga
311100130821     C*
311200170630     C*
311300170630     C*
311400170630     C* classifico l'operazione in base alle regole impostate
311500170703     C                   EXSR      DefOperazione
311600170720     C*
311700170720     C                   Eval      CSTIntra    = $Intra
311800170801     C                   Eval      CSTReverse  = $Reverse
311900170801     C                   Eval      CSTMargine  = $Margine
312000170801     C*
312100170801     C* AUTOFATTURA
312200170801     C* potrebbe essere di fattura di vendita per OMAGGI,
312300170801     C* AUTOCONSUMO (può essere anche fattura di reverse charge)
312400170801     C* In questo caso il cliente è sempre la società che emette
312500170801     C* la fattura
312600170801     C*
312700170801     C                   Eval      CSTAutofat  = $Autofatt
312800000000     C*
312900130822     C* reperisco gli importi
313000130822     C                   EXSR      RepImporti
313100170817fm7  C*
313200170817fm7  C                   Eval      CSTPercDet  = 0
313300170817fm7  C                   Eval      CSTDeducib  = *Off
313400170724     C*
313500170724     C                   Eval      CSTFileO    = *Blank
313600170724     C                   Eval      CSTPosizO   = 0
313700170724     C                   Eval      CSTIdFileO  = *Blank
313800170724     C                   Eval      CSTIdPosO   = 0
313900170724     C                   Eval      CSTFileR    = *Blank
314000170724     C                   Eval      CSTPosizR   = 0
314100170724     C                   Eval      CSTIdFileR  = *Blank
314200170724     C                   Eval      CSTIdPosR   = 0
314300170720     C                   Eval      CSTFileA    = *Blank
314400170720     C                   Eval      CSTIdFileA  = *Blank
314500170724     C*
314600170724     C                   if        CSTTpRec    = 'R'
314700170731     C                   Eval      CSTFileO    = FileO0703
314800170731     C                   Eval      CSTPosizO   = PosizO0703
314900170731     C                   Eval      CSTIdFileO  = IdFilO0703
315000170731     C                   Eval      CSTIdPosO   = IdPosO0703
315100170724     C                   endif
315200130822     C*
315300000000     C                   if        $AnnIVA     = *On
315400170529     C                   Eval      CSTANN      = *On
315500170529     C                   Eval      CSTTPANN    = $TpAnn
315600000000     C                   endif
315700000000     C*
315800000000     C                   ENDSR
315900000000     C/EJECT
316000170801     C************************************************************
316100170801     C* Imposto i dati anagrafici della società comunicante
316200170801     C************************************************************
316300170801     C     RieAnaSoc     BEGSR
316400170811fm3  C*
316500170811fm3  C                   MoveL(P)  $PartIVASoc   $PartIVAMov
316600170811fm3  C                   MoveL(P)  $CdFiscSoc    $CdFiscMov
316700131206     C*
316800170731     C                   MoveL(P)  $Stato1Soc    $Stato1Mov
316900170731     C                   MoveL(P)  $IdPaes1Soc   $IdPaes1Mov
317000170731     C                   MoveL(P)  $PIVA1Soc     $PIVA1Mov
317100170731     C                   MoveL(P)  $CdFisc1Soc   $CdFisc1Mov
317200170727     C*
317300170731     C                   MoveL(P)  $CogRag1Soc   $CogRag1Mov
317400170731     C                   MoveL(P)  $TpN1Soc      $TpN1Mov
317500170727     C*
317600170727     C* persona fisica
317700170731     C                   if        $TpN1Mov    = '1'
317800170731     C                   MoveL     *Blank        $Denom1Mov
317900170731     C                   MoveL(P)  $CogRag1Soc   $Cognom1Mov
318000170731     C                   MoveL(P)  $Nome1Soc     $Nome1Mov
318100170727     C*
318200170727     C                   else
318300170727     C* società
318400170731     C                   MoveL(P)  $CogRag1Soc   $Denom1Mov
318500170731     C                   MoveL     *Blank        $Cognom1Mov
318600170731     C                   MoveL     *Blank        $Nome1Mov
318700170727     C                   endif
318800170727     C*
318900170731     C                   MoveL(P)  $Indir2Soc    $Indir2Mov
319000170731     C                   MoveL(P)  $NrCiv2Soc    $NrCiv2Mov
319100170731     C                   MoveL(P)  $CAP2Soc      $CAP2Mov
319200170731     C                   MoveL(P)  $Locali2Soc   $Locali2Mov
319300170731     C                   MoveL(P)  $Prov2Soc     $Prov2Mov
319400170731     C                   MoveL(P)  $Stato2Soc    $Stato2Mov
319500170731     C                   MoveL(P)  $IdPaes2Soc   $IdPaes2Mov
319600131206     C*
319700170801     C                   ENDSR
319800170801     C/EJECT
319900170728     C************************************************************
320000170728     C* Scansione per caratteri non ammessi
320100170728     C************************************************************
320200170728     C     ScanCharOk    BEGSR
320300170728     C*
320400170731     C* prima converte alcuni caratteri, sostituendoli con altri ammessi
320500170728     C     CnvErr:CnvOk  XLATE     $CampoScan    $CampoScan
320600170728     C*
320700170731     C* poi sostituisce con uno spazio i caratteri rimanenti non ammessi
320800170728     C                   Eval      $FineScan   = *Off
320900170728     C*
321000170728     C                   DoW       $FineScan   = *Off
321100170728     C*
321200170728     C                   Clear                   PosIni            4 0
321300170731     C     CharOK        Check     $CampoScan    PosIni
321400170728     C*
321500170728     C                   if        PosIni <> 0
321600170728     C                   Eval      %SubSt($CampoScan:Posini:1)=' '
321700170728     C                   else
321800170728     C                   Eval      $FineScan   = *On
321900170728     C                   endif
322000170728     C*
322100170728     C                   endDo
322200170728     C*
322300170728     C                   ENDSR
322400170728     C/EJECT
322500130821     C************************************************************
322600130821     C* Classifico l'operazione in base alle regole fissate
322700130821     C************************************************************
322800130821     C     DefOperazione BEGSR
322900130821     C*
323000130821     C* -------------------------
323100130821     C* NATURA OPERAZIONE
323200130821     C* Beni / Servizi
323300130822     C                   EXSR      RepNatOpe
323400170703     C*
323500170703     C* -------------------------
323600170703     C* TIPO DOCUMENTO
323700170703     C                   EXSR      RepTpDoc
323800130821     C*
323900130821     C* -------------------------
324000130821     C* OPERAZIONE registrata senza NDIVA
324100110902     C                   if        $ChkNDIVA   = *On
324200170529     C                   Eval      CSTNoIVA    = *Off
324300110902     C                   else
324400170529     C                   Eval      CSTNoIVA    = *On
324500110902     C                   endif
324600170703     C*
324700170703     C* -------------------------
324800170703     C* reperisco la motivazione di esclusione dell'imposta
324900170703     C* (ovvero la NATURA)
325000170703     C                   EXSR      RepMotEscl
325100130821     C*
325200130821     C* -------------------------
325300170630     C* Esigibilità
325400170703     C*
325500170810fm2  C* I=Immediata
325600170810fm2  C                   Eval      CSTEsigib   = 'I'
325700170703     C*
325800170810fm2  C* D=Differita
325900170703     C                   if        $Differita  = *On
326000170810fm2  C                   Eval      CSTEsigib   = 'D'
326100170703     C                   endif
326200170703     C*
326300170810fm2  C* S=Scissione pagamenti (Split Payment)
326400170630     C                   Eval      SPRTipReg   = '1'
326500170630     C                   Eval      SPRSTpReg   = '5'
326600170703     C                   EXSR      ChkRegOperaz
326700170703     C                   if        $OkRegola   = *On
326800170810fm2  C                   Eval      CSTEsigib   = 'S'
326900170630     C                   endif
327000130821     C*
327100000000     C                   ENDSR
327200000000     C/EJECT
327300130821     C************************************************************
327400130821     C* Reperisco il tipo documento
327500130821     C************************************************************
327600130821     C     RepTpDoc      BEGSR
327700130821     C*
327800130821     C* Può essere :
327900170703     C* - TD01 fattura
328000170703     C* - TD04 nota di credito
328100170703     C* - TD05 nota di debito
328200170703     C* - TD10 fattura di acquisto intracomunitario beni
328300170703     C* - TD11 fattura di acquisto intracomunitario servizi
328400170703     C*
328500170703     C*
328600170703     C* ---------------
328700170703     C* Imposto che è FATTURA, poi saranno le regole a definire se
328800130821     C* si tratta di altro documento
328900170703     C                   Eval      CSTTpDoc    = 'TD01'
329000130821     C*
329100130821     C* ---------------
329200170703     C* NOTA DI CREDITO
329300170814fm4  C                   if        OPETpDocI   = '1'   or
329400170814fm4  C                             OPETpDocI   = '4'   or
329500170814fm4  C                             OPETpDocI   = '7'
329600170703     C                   Eval      CSTTpDoc    = 'TD04'
329700130823     C                   endif
329800130823     C*
329900130823     C* ---------------
330000170703     C* nota di addebito (TD05)
330100170703     C                   Eval      SPRTipReg   = '4'
330200170703     C                   Eval      SPRSTpReg   = '1'
330300170703     C                   EXSR      ChkRegTpDoc
330400170703     C                   if        $OkRegola   = *On
330500170703     C                   Eval      CSTTpDoc    = 'TD05'
330600170703     C                   endif
330700170703     C*
330800170703     C* ---------------
330900170703     C* fattura semplificata (TD07)
331000170703     C* verifico se la causale è catalogata come:
331100170703     C* nota di addebito (TD05), fattura semplificata (TD07) o
331200170703     C* nota di accredito semplificata (TD08)
331300170703     C                   Eval      SPRTipReg   = '4'
331400170703     C                   Eval      SPRSTpReg   = '4'
331500170703     C                   EXSR      ChkRegTpDoc
331600170703     C                   if        $OkRegola   = *On
331700170703     C                   Eval      CSTTpDoc    = 'TD07'
331800170703     C                   endif
331900170703     C*
332000170703     C* ---------------
332100170703     C* nota di accredito semplificata (TD08)
332200170801     C                   Eval      SPRTipReg   = '4'
332300170703     C                   Eval      SPRSTpReg   = '5'
332400170703     C                   EXSR      ChkRegTpDoc
332500170703     C                   if        $OkRegola   = *On
332600170703     C                   Eval      CSTTpDoc    = 'TD08'
332700170703     C                   endif
332800170703     C*
332900170703     C* ---------------
333000170724     C*
333100170724     C* - TD10 fattura di acquisto intracomunitario beni
333200170724     C* - TD11 fattura di acquisto intracomunitario servizi
333300170724     C                   if        $CF        = 'F'         and
333400170724     C                             $Intra       = *On       and
333500170807     C************                 (CSTTpDoc    = 'TD01'    or
333600170807     C************                  CSTTpDoc    = 'TD04'       )
333700170807     C                             CSTTpDoc    = 'TD01'
333800170703     C*
333900170703     C                   if        CSTNatOpe   = 'B'
334000170703     C                   Eval      CSTTpDoc    = 'TD10'
334100170703     C                   else
334200170703     C                   Eval      CSTTpDoc    = 'TD11'
334300170724     C                   endif
334400170703     C*
334500170703     C                   endif
334600130823     C*
334700130823     C                   ENDSR
334800130823     C/EJECT
334900130823     C************************************************************
335000170703     C* Verifico se è un tipo documento classificato
335100130823     C************************************************************
335200170703     C     ChkRegTpDoc   BEGSR
335300130823     C*
335400170703     C*------------------------------
335500170703     C* 1° tentativo:  causale + conto
335600170703     C                   EXSR      KeyRegTpDoc
335700170703     C                   EXSR      ChainRegola
335800130823     C*
335900170703     C*------------------------------
336000170703     C* 2° tentativo:  Blank   + conto
336100170703     C                   if        $OkRegola   = *Off
336200170703     C                   EXSR      KeyRegTpDoc
336300170703     C                   Eval      SPRCAUSALE  = *Blank
336400170703     C                   EXSR      ChainRegola
336500130823     C                   endif
336600130823     C*
336700170703     C*------------------------------
336800170703     C* 3° tentativo:  causale + Blank
336900170703     C                   if        $OkRegola   = *Off
337000170703     C                   EXSR      KeyRegTpDoc
337100130823     C                   Eval      SPRKCC      = *Blank
337200130823     C                   Eval      SPRKSC      = *Blank
337300170703     C                   EXSR      ChainRegola
337400130823     C                   endif
337500130823     C*
337600130823     C                   ENDSR
337700130823     C/EJECT
337800170703     C************************************************************
337900170703     C* Impostazione chiave completa per tipo documento
338000170703     C************************************************************
338100170703     C     KeyRegTpDoc   BEGSR
338200170703     C*
338300170703     C                   Eval      SPRSocieta  = XScSoc
338400170703     C                   Eval      SPRTpReg    = *Blank
338500170703     C                   Eval      SPRLibro    = *Blank
338600170703     C                   Eval      SPRAliq     = *Zero
338700170703     C                   Eval      SPRRifIVA   = *Blank
338800170703     C                   Eval      SPRCausale  = MOVCausTes
338900170703     C                   Eval      SPRKcc      = MOVKcc
339000170703     C                   Eval      SPRKsc      = MOVKsc
339100170703     C                   Eval      SPRVoce     = *Blank
339200170703     C                   Eval      SPRClasse   = *Blank
339300170703     C*
339400170703     C                   ENDSR
339500170703     C/EJECT
339600170703     C************************************************************
339700170703     C* Aggancio la regola
339800170703     C************************************************************
339900170703     C     ChainRegola   BEGSR
340000170703     C*
340100170703     C                   Eval      $OkRegola   = *Off
340200170703     C*
340300170703     C     K12SPR04      CHAIN     SDGSPR04L                          21
340400170703     C                   if        *In21       = *Off
340500170703     C                   Eval      $OkRegola   = *On
340600170703     C                   endif
340700170703     C*
340800170703     C                   ENDSR
340900170703     C/EJECT
341000130924     C************************************************************
341100170630     C* Recupero la definizione dell'operazione
341200130924     C************************************************************
341300170703     C     ChkRegOperaz  BEGSR
341400130924     C*
341500170703     C                   Eval      $OkRegola   = *Off
341600170703     C*
341700130924     C*------------------------------
341800170630     C* 1° tentativo: libro +  IVA    + causale + conto
341900170703     C                   if        $ChkNDIVA   = *On
342000170703     C                   EXSR      KeyRegOperaz
342100170703     C                   EXSR      ChainRegola
342200130924     C*
342300130924     C*------------------------------
342400170630     C* 2° tentativo: libro +  IVA    + causale + Blank
342500170703     C                   if        $OkRegola   = *Off
342600170703     C                   EXSR      KeyRegOperaz
342700170630     C                   Eval      SPRKcc      = *Blank
342800170630     C                   Eval      SPRKsc      = *Blank
342900170703     C                   EXSR      ChainRegola
343000130924     C                   endif
343100170630     C*
343200170630     C*------------------------------
343300170630     C* 3° tentativo: libro +  IVA    + Blank   + conto
343400170703     C                   if        $OkRegola   = *Off
343500170703     C                   EXSR      KeyRegOperaz
343600170630     C                   Eval      SPRCausale  = *Blank
343700170703     C                   EXSR      ChainRegola
343800170630     C                   endif
343900170630     C*
344000170630     C*------------------------------
344100170630     C* 4° tentativo: libro +  IVA    + Blank   + Blank
344200170703     C                   if        $OkRegola   = *Off
344300170703     C                   EXSR      KeyRegOperaz
344400170630     C                   Eval      SPRCausale  = *Blank
344500170630     C                   Eval      SPRKcc      = *Blank
344600170630     C                   Eval      SPRKsc      = *Blank
344700170703     C                   EXSR      ChainRegola
344800170630     C                   endif
344900170630     C*
345000170630     C*------------------------------
345100170630     C* 5° tentativo: Blank +  IVA    + Blank   + Blank
345200170703     C                   if        $OkRegola   = *Off
345300170703     C                   EXSR      KeyRegOperaz
345400170630     C                   Eval      SPRTPREG    = *Blank
345500170630     C                   Eval      SPRLIBRO    = *Blank
345600170630     C                   Eval      SPRCausale  = *Blank
345700170630     C                   Eval      SPRKcc      = *Blank
345800170630     C                   Eval      SPRKsc      = *Blank
345900170703     C                   EXSR      ChainRegola
346000170630     C                   endif
346100170630     C*
346200170630     C*------------------------------
346300170630     C* 6° tentativo: libro +  Blank  + Blank   + Blank
346400170703     C                   if        $OkRegola   = *Off
346500170703     C                   EXSR      KeyRegOperaz
346600170630     C                   Eval      SPRALIQ     = 0
346700170630     C                   Eval      SPRRIFIVA   = *Blank
346800170630     C                   Eval      SPRCAUSALE  = *Blank
346900170630     C                   Eval      SPRKCC      = *Blank
347000170630     C                   Eval      SPRKSC      = *Blank
347100170703     C                   EXSR      ChainRegola
347200170630     C                   endif
347300170703     C*
347400170703     C                   endif
347500170630     C*
347600170630     C*------------------------------
347700170630     C* 7° tentativo: Blank +  Blank  + causale + conto
347800170703     C                   if        $OkRegola   = *Off
347900170703     C                   EXSR      KeyRegOperaz
348000170630     C                   Eval      SPRTPREG    = *Blank
348100170630     C                   Eval      SPRLIBRO    = *Blank
348200170630     C                   Eval      SPRALIQ     = 0
348300170630     C                   Eval      SPRRIFIVA   = *Blank
348400170703     C                   EXSR      ChainRegola
348500170630     C                   endif
348600170630     C*
348700170630     C*------------------------------
348800170630     C* 8° tentativo: Blank +  Blank  + causale + Blank
348900170703     C                   if        $OkRegola   = *Off
349000170703     C                   EXSR      KeyRegOperaz
349100170630     C                   Eval      SPRTPREG    = *Blank
349200170630     C                   Eval      SPRLIBRO    = *Blank
349300170630     C                   Eval      SPRALIQ     = 0
349400170630     C                   Eval      SPRRIFIVA   = *Blank
349500170630     C                   Eval      SPRKcc      = *Blank
349600170630     C                   Eval      SPRKsc      = *Blank
349700170703     C                   EXSR      ChainRegola
349800170630     C                   endif
349900170630     C*
350000170630     C*------------------------------
350100170630     C* 9° tentativo: Blank +  Blank  + Blank   + conto
350200170703     C                   if        $OkRegola   = *Off
350300170703     C                   EXSR      KeyRegOperaz
350400170630     C                   Eval      SPRTPREG    = *Blank
350500170630     C                   Eval      SPRLIBRO    = *Blank
350600170630     C                   Eval      SPRALIQ     = 0
350700170630     C                   Eval      SPRRIFIVA   = *Blank
350800170630     C                   Eval      SPRCAUSALE  = *Blank
350900170703     C                   EXSR      ChainRegola
351000170630     C                   endif
351100170630     C*
351200170630     C*------------------------------
351300170703     C                   if        $ChkNDIVA   = *On
351400170703     C*
351500170630     C* 10° tentativo: libro +  Blank  + causale + conto
351600170703     C                   if        $OkRegola   = *Off
351700170703     C                   EXSR      KeyRegOperaz
351800170630     C                   Eval      SPRALIQ     = 0
351900170630     C                   Eval      SPRRIFIVA   = *Blank
352000170703     C                   EXSR      ChainRegola
352100170630     C                   endif
352200170630     C*
352300170630     C*------------------------------
352400170630     C* 11° tentativo: libro +  Blank  + Blank   + conto
352500170703     C                   if        $OkRegola   = *Off
352600170703     C                   EXSR      KeyRegOperaz
352700170630     C                   Eval      SPRALIQ     = 0
352800170630     C                   Eval      SPRRIFIVA   = *Blank
352900170630     C                   Eval      SPRCAUSALE  = *Blank
353000170703     C                   EXSR      ChainRegola
353100170630     C                   endif
353200130924     C*
353300170703     C                   endif
353400170703     C*
353500130924     C*------------------------------
353600130924     C*
353700130924     C                   ENDSR
353800130924     C/EJECT
353900130924     C************************************************************
354000170630     C* Impostazione chiave completa per definizione operazione
354100130924     C************************************************************
354200170703     C     KeyRegOperaz  BEGSR
354300130924     C*
354400170630     C                   Eval      SPRSocieta  = XScSoc
354500170703     C                   Eval      SPRTpReg    = IVATpReg
354600170703     C                   Eval      SPRLibro    = IVALibro
354700170703     C                   Eval      SPRAliq     = IVAAliq
354800170703     C                   Eval      SPRRifIVA   = IVARifIVA
354900170703     C                   Eval      SPRCausale  = MOVCausTes
355000170703     C                   Eval      SPRKcc      = MOVKcc
355100170703     C                   Eval      SPRKsc      = MOVKsc
355200170703     C                   Eval      SPRVoce     = *Blank
355300170703     C                   Eval      SPRClasse   = *Blank
355400170630     C*
355500170630     C                   ENDSR
355600170630     C/EJECT
355700000000     C************************************************************
355800130821     C* Reperisco la natura dell'operazione
355900000000     C************************************************************
356000130826     C     RepNatOpe     BEGSR
356100130826     C*
356200130826     C* esamino le contropartite solo a cambio di registrazione
356300130826     C                   IF        IVASys     <> SAV2Sys   or
356400130826     C                             IVANrAsReg <> SAV2NrAsReg
356500170801     C                             OR
356600170801     C                             ($CF         = 'F'     and
356700170801     C                              AlleIVA002  = '02'        )
356800170706     C*
356900170706     C                   Eval      $OkBeni     = *Off
357000170706     C                   Eval      $OkServizi  = *Off
357100170706     C                   Eval      $ImpBeni    = 0
357200170706     C                   Eval      $ImpServizi = 0
357300130826     C*
357400170703     C* reperimento eventuale default da attributo contabile
357500170703     C                   EXSR      RepBenServAttr
357600170816fm6  C*
357700170816 "   C                   if        $NatOpe     = *Blanks
357800170816 "   C                   EXSR      RepBenServCaus
357900170816fm6  C                   endif
358000130826     C*
358100170703     C                   if        $NatOpe     = *Blanks
358200130826     C                   EXSR      ChkBeniServizi
358300170703     C                   endif
358400130918     C*
358500170703     C                   if        $NatOpe     = *Blanks
358600170703     C* imposto il default dal parametro modulo
358700170703     C                   Eval      $NatOpe     = BeniSrvA12
358800170703     C                   endif
358900130826     C*
359000170703     C                   endif
359100130826     C*
359200000000     C* ripristino i dati del movimento del cliente/fornitore
359300000000     C                   MoveL     NDMOVSav      NDMOV00F
359400000000     C*
359500000000     C                   Move      IVASys        SAV2Sys
359600000000     C                   Move      IVANrAsReg    SAV2NrAsReg
359700000000     C*
359800170529     C                   Eval      CSTNatOpe   = $NatOpe
359900000000     C*
360000000000     C                   ENDSR
360100000000     C/EJECT
360200130821     C************************************************************
360300170630     C* Reperisco la Natura degli importi da movimento IVA
360400170630     C* (ovvero la motivazione di esclusione dell'IVA)
360500130821     C************************************************************
360600170630     C     RepMotEscl    BEGSR
360700130822     C*
360800170703     C                   Select
360900130822     C* movimento NON imponibile
361000130822     C                   When      %subst(RIVFlg:4:1) = '1'   and
361100130822     C                             IVARifIva  <> *Blank
361200170630     C                   Eval      CSTMotEscl  = 'N3'
361300170630     C*
361400130822     C* movimento esente
361500130822     C                   When      %subst(RIVFlg:3:1) = '1'   and
361600130822     C                             IVARifIva  <> *Blank
361700170630     C                   Eval      CSTMotEscl  = 'N4'
361800130822     C*
361900130822     C                   endSl
362000170630     C*
362100170703     C*
362200170703     C* ---------------
362300170703     C* REGIME DEL MARGINE
362400170703     C* Natura = N5 = Regime del margine/IVA non esposta in fattura
362500170703     C* es. agenzie viaggio e cessione di beni usati (art.74 e 74ter)
362600170801     C                   Eval      $Margine    = *Off
362700170703     C                   Eval      SPRTipReg   = '1'
362800170703     C                   Eval      SPRSTpReg   = '2'
362900170703     C                   EXSR      ChkRegOperaz
363000170703     C                   if        $OkRegola   = *On
363100170801     C                   Eval      $Margine    = *On
363200170703     C                   Eval      CSTMotEscl  = 'N5'
363300170703     C                   endif
363400170703     C*
363500170703     C* ---------------
363600170703     C* REVERSE CHARGE
363700170703     C* Natura = N6 = Inversione contabile (Reverse charge, autofatturazione
363800170703     C*         acquisti extra UE di servizi, importazioni di beni
363900170703     C*         nei soli casi previsti)
364000170703     C* es. compravendita di oro e argento, servizi edili di subappalto
364100170703     C*     (art. 17);
364200170703     C*     compravendita di rottami e metalli non ferrosi
364300170703     C*     (art.74 commi 7 e 8).
364400170801     C                   Eval      $Reverse    = *Off
364500170703     C                   Eval      SPRTipReg   = '1'
364600170703     C                   Eval      SPRSTpReg   = '1'
364700170703     C                   EXSR      ChkRegOperaz
364800170703     C                   if        $OkRegola   = *On
364900170801     C                   Eval      $Reverse    = *On
365000170703     C                   Eval      CSTMotEscl  = 'N6'
365100170703     C                   endif
365200170703     C*
365300170703     C* ---------------
365400170703     C* Natura = N1 = Esclusione ex art.15
365500170703     C                   Eval      SPRTipReg   = '1'
365600170703     C                   Eval      SPRSTpReg   = '3'
365700170703     C                   EXSR      ChkRegOperaz
365800170703     C                   if        $OkRegola   = *On
365900170703     C                   Eval      CSTMotEscl  = 'N1'
366000170703     C                   endif
366100170703     C*
366200170703     C* ---------------
366300170703     C* Natura = N7 = IVA assolta in altro stato UE (vendite a distanza ex art. 40
366400170703     C*         c.3 e 4 art. 41 c. 1 lett. b, DL 331/93; prestazione di
366500170703     C*         servizi di telecomunicazioni, tele-radiodiffusione ed
366600170703     C*         elettronici ...)
366700170703     C                   Eval      SPRTipReg   = '1'
366800170703     C                   Eval      SPRSTpReg   = '4'
366900170703     C                   EXSR      ChkRegOperaz
367000170703     C                   if        $OkRegola   = *On
367100170703     C                   Eval      CSTMotEscl  = 'N7'
367200170703     C                   endif
367300130821     C*
367400170803     C* ---------------
367500170803     C* CONGRUENZA ALIQUOTA E NATURA DI ESCLUSIONE
367600170803     C                   Select
367700170803     C* ----------
367800170803     C                   When      CSTAliq    <> 0      and
367900170803     C                             CSTReverse  = *On    and
368000170803     C                             CSTMotEscl  = 'N6'
368100170803     C*                  Ok
368200170803     C* ----------
368300170803     C                   When      CSTAliq    <> 0      and
368400170803     C                             CSTMotEscl  = *Blank
368500170803     C*                  Ok
368600170803     C* ----------
368700170803     C                   When      CSTAliq     = 0      and
368800170803     C                             CSTMotEscl  = *Blank
368900170803     C* imposto che è non soggetto
369000170803     C                   Eval      CSTMotEscl  = 'N2'
369100170803     C* ----------
369200170803     C                   When      CSTAliq    <> 0      and
369300170803     C                             CSTMotEscl <> *Blank
369400170810fm2  C                             and
369500170810fm2  C                             CSTMotEscl <> 'N6'
369600170803     C*
369700170803     C* Incongruenza aliquota/natura
369800170803     C                   Eval      $AnnIVA     = *On
369900170803     C                   if        $TpAnn      = *Blank
370000170803     C                   Eval      $TpAnn      = '01'
370100170803     C                   endif
370200170803     C*
370300170803     C                   endSl
370400170803     C*
370500130821     C                   ENDSR
370600130821     C/EJECT
370700000000     C************************************************************
370800000000     C* Reperimento importi da movimento IVA
370900000000     C************************************************************
371000130822     C     RepImporti    BEGSR
371100000000     C*
371200170724     C* operazione con fattura (fattura, nota, doc.di riepilogo)
371300170724     C                   Eval      CSTImpOpeO  = IVAImponib
371400170724     C                   Eval      CSTIVAOpeO  = IVAImporto
371500111021     C*
371600170724     C*
371700170810     C******* rendo negativo l'importo se si tratta di note di credito su
371800170810     C******* acquisti INTRA
371900111021     C*
372000170810     C******* Sottonatura   Segno di NDIVA00F   Segno calcolato dell'importo
372100170810     C*******      F          D (segno "naturale")     ==>    +
372200170810     C*******      F          A (segno nota di accred) ==>    -
372300111021     C*
372400170807     C*******            if        (CSTTpDoc    = 'TD10'    or
372500170807     C*******                       CSTTpDoc    = 'TD11'      )    and
372600170807     C*******                      ((IVAAvere    = 1        and
372700170807     C*******                        CSTCF       = 'C'         ) or
372800170807     C*******                       (IVADare     = 1        and
372900170807     C*******                        CSTCF       = 'F'         )   )
373000170807     C*
373100170810     C                   if        (MOVSNatura = 'F' and
373200170810     C                              IVAAvere   = 1        )
373300170810     C                             or
373400170810     C                             (MOVSNatura = 'C' and
373500170810     C                              IVADare    = 1        )
373600170724     C*
373700170810     C     IVAImponib    MULT      -1            CSTImpOpeO
373800170810     C     IVAImporto    MULT      -1            CSTIVAOpeO
373900170724     C*
374000170724     C                   endif
374001170831R2652C*
374002170831 "   C*
374003170831 "   C* in caso di autofattura, giro il segno rispetto alla logica
374004170831 "   C* generale
374005170831 "   C                   if        ($Autofatt   = *On        or
374006170831 "   C                             OPETpDocI   = '8'         or
374007170831 "   C                             (MOVSNatura  = 'F'   and
374008170831 "   C                              IVATpReg    = '2'   and
374009170831 "   C                              DLITerrito  = '1'       )
374010170831 "   C                             )
374011170831 "   C                             and
374012170831 "   C                             $AutoCons  <> *On
374013170831 "   C*
374014170831 "   C     CSTImpOpeO    MULT      -1            CSTImpOpeO
374015170831 "   C     CSTIVAOpeO    MULT      -1            CSTIVAOpeO
374016170831 "   C*
374017170831R2652C                   endif
374100110722     C*
374200170529     C                   Eval      CSTImpOpe   = CSTImpOpeO
374300170529     C                   Eval      CSTIVAOpe   = CSTIVAOpeO
374400000000     C*
374500000000     C                   ENDSR
374600000000     C/EJECT
374700130822     C************************************************************
374800130926     C* Reperiti dati anagrafici a parità di partita IVA/cd fiscale
374900130822     C************************************************************
375000130926     C     RepAnagraf    BEGSR
375100130926     C*
375200170731     C                   Clear                   SDG00704DS
375300130926     C*
375400170731     C                   Eval      Soc0704     = SocietaI52
375500170731     C                   Eval      TpCom0704   = TpComI52
375600170718     C*
375700170718     C* i dati anagrafici devono gli stessi nel trimestre,
375800170718     C* indipendentemente dal tipo record
375900170801     C****               Eval      TpRec0704   = TpRecI52
376000170718     C*
376100170731     C                   Eval      Anno0704    = AnnoI52
376200170731     C                   Eval      Trim0704    = TrimI52
376300170731     C                   Eval      Mese0704    = MeseI52
376400170803     C                   Eval      CF0704      = $CF
376500170803     C                   Eval      PartIV0704  = $PartIVAMov
376600170731     C                   Eval      CdFisc0704  = $CdFiscMov
376700130926     C*
376800170731     C                   CALLB     'SDG00704R'
376900130926     C                   PARM                    KPJBA
377000170731     C                   PARM                    SDG00704DS
377100130926     C*
377200170731     C                   if        Found0704   = *On
377300130926     C*
377400170731     C                   Eval      $Intra      = Intra0704
377500170718     C*
377600170731     C                   Eval      $Stato1Mov  = Stato10704
377700170731     C                   Eval      $IdPaes1Mov = IdPae10704
377800170731     C                   Eval      $PIVA1Mov   = PIVA10704
377900170731     C                   Eval      $CdFisc1Mov = CdFis10704
378000170727     C*
378100170731     C                   Eval      $CogRag1Mov = CogRa10704
378200170731     C                   Eval      $TpN1Mov    = TpN10704
378300170731     C                   Eval      $Denom1Mov  = Denom10704
378400170731     C                   Eval      $Cognom1Mov = Cogno10704
378500170731     C                   Eval      $Nome1Mov   = Nome10704
378600170727     C*
378700170731     C                   Eval      $Indir2Mov  = Indir20704
378800170731     C                   Eval      $NrCiv2Mov  = NrCiv20704
378900170731     C                   Eval      $CAP2Mov    = CAP20704
379000170731     C                   Eval      $Locali2Mov = Local20704
379100170731     C                   Eval      $Prov2Mov   = Prov20704
379200170731     C                   Eval      $Stato2Mov  = Stato20704
379300170731     C                   Eval      $IdPaes2Mov = IdPae20704
379400170731     C                   Eval      $Indir3Mov  = Indir30704
379500170731     C                   Eval      $NrCiv3Mov  = NrCiv30704
379600170731     C                   Eval      $CAP3Mov    = CAP30704
379700170731     C                   Eval      $Locali3Mov = Local30704
379800170731     C                   Eval      $Prov3Mov   = Prov30704
379900170731     C                   Eval      $Stato3Mov  = Stato30704
380000170731     C                   Eval      $IdPaes3Mov = IdPae30704
380100170731     C*
380200170731     C                   Eval      $Stato4Mov  = Stato40704
380300170731     C                   Eval      $IdPaes4Mov = IdPae40704
380400170731     C                   Eval      $PIVA4Mov   = PIVA40704
380500170731     C                   Eval      $CogRag4Mov = CogRa40704
380600170731     C                   Eval      $TpN4Mov    = TpN40704
380700170731     C                   Eval      $Denom4Mov  = Denom40704
380800170731     C                   Eval      $Cognom4Mov = Cogno40704
380900170731     C                   Eval      $Nome4Mov   = Nome40704
381000170718     C*
381100170718     C                   endif
381200170810     C*
381300170810     C                   ENDSR
381400170810     C/EJECT
381500170810     C********************************************************************
381600170810     C* Controllo se presenti i dati fiscali obbligatori
381700170810     C********************************************************************
381800170810     C     ScanChar      BEGSR
381900170810     C*
382000170810fm2  C* controllo il codice fiscale e la partita IVA
382100170810fm2  C******             EXSR      CtrFiscIVA
382200170810     C*
382300170810     C                   Eval      $CampoScan  = $CogRag1Mov
382400170810     C                   Exsr      ScanCharOk
382500170810     C                   Eval      $CogRag1Mov = $CampoScan
382600170810     C*
382700170810     C                   Eval      $CampoScan  = $Denom1Mov
382800170810     C                   Exsr      ScanCharOk
382900170810     C                   Eval      $Denom1Mov  = $CampoScan
383000170810     C*
383100170810     C                   Eval      $CampoScan  = $Cognom1Mov
383200170810     C                   Exsr      ScanCharOk
383300170810     C                   Eval      $Cognom1Mov = $CampoScan
383400170810     C*
383500170810     C                   Eval      $CampoScan  = $Nome1Mov
383600170810     C                   Exsr      ScanCharOk
383700170810     C                   Eval      $Nome1Mov   = $CampoScan
383701170906R2656X*** Ex R264
383702170906 "   X***   "            if        $TpN1Mov    = '1'         and
383703170906 "   X***   "                      $Nome1Mov   = *Blank
383704170906 "   X***   "            Eval      $AnnIVA     = *On
383705170906 "   X***   "            Eval      $AnnMOV     = *On
383706170906 "   X***   "            if        $TpAnn      = *Blank
383707170906 "   X***   "                      or
383708170906 "   X***   "                      $TpAnn      = '20'
383709170906 "   X***   "            Eval      $TpAnn      = '07'
383710170906 "   X***   "            endif
383711170906R2656X*** Ex R264        endif
384900170810     C*
385000170810     C                   Eval      $CampoScan  = $Indir2Mov
385100170810     C                   Exsr      ScanCharOk
385200170810     C                   Eval      $Indir2Mov  = $CampoScan
385300170810     C                   if        $Indir2Mov  = *Blank
385400170810     C                   Eval      $Indir2Mov  = 'Dato assente'
385500170810     C                   endif
385600170810     C*
385700170810     C                   Eval      $CampoScan  = $Locali2Mov
385800170810     C                   Exsr      ScanCharOk
385900170810     C                   Eval      $Locali2Mov = $CampoScan
386000170810     C                   if        $Locali2Mov = *Blank
386100170810     C                   Eval      $Locali2Mov = 'Dato assente'
386200170810     C                   endif
386300170810     C*
386400170810     C                   if        $IdPaes2Mov = 'IT'      and
386500170810     C                             $Prov2Mov   = *Blank
386600170810     C                   Eval      $Prov2Mov   = 'DA'
386700170810     C                   endif
386800170810     C*
386900170811     C                   if        $IdPaes2Mov = *Blank    and
387000170811fm3  C                             $Stato2Mov  = *Blank
387100170810     C                   Eval      $IdPaes2Mov = 'DA'
387200170810     C                   endif
387300170810     C*
387400170810     C*
387500170810     C                   Eval      $CampoScan  = $Indir3Mov
387600170810     C                   Exsr      ScanCharOk
387700170810     C                   Eval      $Indir3Mov  = $CampoScan
387800170810     C*
387900170810     C                   Eval      $CampoScan  = $Locali3Mov
388000170810     C                   Exsr      ScanCharOk
388100170810     C                   Eval      $Locali3Mov = $CampoScan
388200170810     C*
388300170810     C*
388400170810     C                   Eval      $CampoScan  = $CogRag4Mov
388500170810     C                   Exsr      ScanCharOk
388600170810     C                   Eval      $CogRag4Mov = $CampoScan
388700170810     C*
388800170810     C                   Eval      $CampoScan  = $Denom4Mov
388900170810     C                   Exsr      ScanCharOk
389000170810     C                   Eval      $Denom4Mov  = $CampoScan
389100170810     C*
389200170810     C                   Eval      $CampoScan  = $Cognom4Mov
389300170810     C                   Exsr      ScanCharOk
389400170810     C                   Eval      $Cognom4Mov = $CampoScan
389500170810     C*
389600170810     C                   Eval      $CampoScan  = $Nome4Mov
389700170810     C                   Exsr      ScanCharOk
389800170810     C                   Eval      $Nome4Mov   = $CampoScan
389801170906R2656X*** Ex R264
389802170906 "   X***   "            if        $TpN4Mov    = '1'         and
389803170906 "   X***   "                      $Nome4Mov   = *Blank
389804170906 "   X***   "            Eval      $AnnIVA     = *On
389805170906 "   X***   "            Eval      $AnnMOV     = *On
389806170906 "   X***   "            if        $TpAnn      = *Blank
389807170906 "   X***   "                      or
389808170906 "   X***   "                      $TpAnn      = '20'
389809170906 "   X***   "            Eval      $TpAnn      = '07'
389810170906 "   X***   "            endif
389811170906R2656X*** Ex R264        endif
389812170906R2656C*
389813170906R2656C                   EXSR      ChkNomPerF
391000170810     C*
391100170810     C                   ENDSR
391200170810     C/EJECT
391300170810     C************************************************************
391400170810     C* Controllo codice fiscale /partita iva, se italiani
391500170810     C* e se Paese della partita iva è INTRA
391600170810     C************************************************************
391700170810     C     CtrFiscIVA    BEGSR
391800170810     C*
391900170810     C* se presenti il codice fiscale e/o la partita IVA,
392000170810     C* verifico se sono corretti per la legislazione Italiana
392100170810     C* Se la partita IVA è valida e italiana, imposto 'IT'
392200170810     C* nell'idPaese
392300170810     C*
392400170810     C*
392500170810     C                   Eval      $Intra      = *Off
392600170810     C*
392700170810     C* assumo che è Italia
392800170811fm3  X*****              Eval      $IdPaes1Mov = 'IT'
392900170810     C*
393000170810     C* ulteriori controlli per
393100170810     C* legislazione italiana e Intra
393200170810     C*
393300170810     C* -----
393400170810     C* PARTITA IVA
393500170810     C                   if        $PIVA1Mov  <> *Blank
393600170810     C*
393700170810     C                   if        StaSKP      = '1'
393800170810     C*
393900170811fm3  X*****              CALLB     'XPARIVA'
394000170811fm3  X*****              PARM                    $PIVA1Mov
394100170811fm3  X*****              PARM      *Zero         $Errfisc          1 0
394200170811fm3  X*****              PARM                    XScSoc
394300170810     C*
394400170810     C                   endif
394500170810     C*
394600170810     C*
394700170811fm3  X*****              if        $Errfisc    < 0     or
394800170811fm3  X*****                        StaSKP     <> '1'
394900170810     C*
395000170810     C* verifico se i primi 2 caratteri della Partita IVA corrispondono
395100170810     C* a un codice ISO 2 Alpha
395200170810     C* Se in tab. HD4 trovo il codice ISO associato a uno Stato,
395300170810     C* allora accedo alla tab. G12 e se presente il codice Nazione,
395400170810     C* allora è Intra, altrimenti è estero!!!!!!
395500170810     C* N.B. in W06 non è presente l'Italia, meglio!
395600170810     C*
395700170810     C                   Eval      WIdPaese    = %subst($PIVA1Mov:1:2)
395800170811fm3  C                   if        WIdPaese   <> *Blank
395900170811fm3  C                   EXSR      ChkIntraPIVA
396000170810     C                   Eval      $Intra      = WIntra
396100170811fm3  C                   else
396200170811fm3  C                   Eval      $Intra      = *Off
396300170811fm3  C                   endif
396400170810     C*
396500170811fm3  X*****              endif
396600170810     C*
396700170811     C                   endif
396800170810     C*
396900170810     C* imposto l'idpaese se CEE altrimenti per extracee è 99
397000170810     C*
397100170810     C                   Select
397200170810     C*
397300170810     C* ----
397400170810     C                   When      $Intra      = *On    and
397500170811fm3  C                             FoundW06    = *On    and
397600170810     C                             $PIVA1Mov  <> *Blank
397700170810     C*
397800170811fm3  X****               Eval      $IdPaes1Mov = %subst($PIVA1Mov:1:2)
397900170811fm3  C                   EXSR      RepIdPaesW06
398000170811fm3  C                   if        FoundHD4    = *On
398100170811fm3  C                   Eval      $IdPaes1Mov = CODISO2HD4
398200170811fm3  C                   Eval      $Stato1Mov  = SavStatoG12
398300170811fm3  C                   endif
398400170810     C                   Eval      $LenOut     = %size($PIVA1Mov)
398500170810     C                   Eval      $PIVA1Mov   =
398600170811fm3  X*****                               %subst($PIVA1Mov:1:$Lenout-2)
398700170811fm3  C                                    %subst($PIVA1Mov:3:$Lenout-2)
398800170810     C*
398900170810     C* ----
399000170810     C                   When      ($Intra     <> *On    and
399100170810     C                              $PIVA1Mov  <> *Blank    ) or
399200170810     C                             $PIVA1Mov   = *Blank
399300170810     C* Se la Sede è ExtraCee, recupero l'IdPaese dall'IdPaese
399400170810     C* della Sede Legale (nel 99% dei casi andrà bene)
399500170810     C*
399600170810 fm2 C                   Eval      $Stato1Mov  = $Stato2Mov
399700170810     C                   Eval      $IdPaes1Mov = $IdPaes2Mov
399800170810     C*
399900170811fm3  C                   if        $IdPaes1Mov <> *Blank
400000170811fm3  C                   Eval      WIdPaese    = $IdPaes1Mov
400100170810     C                   EXSR      ChkIntra
400200170810     C                   Eval      $Intra      = WIntra
400300170811fm3  C                   if        $Intra      = *On     and
400400170811fm3  C                             FoundW06    = *Off
400500170811fm3  C                   Eval      $LenOut     = %size($PIVA1Mov)
400600170811fm3  C                   Eval      $PIVA1Mov   =
400700170811fm3  C                                    %subst($PIVA1Mov:3:$Lenout-2)
400800170811fm3  C                   endif
400900170811fm3  C                   else
401000170811fm3  C                   Eval      $Intra      = *Off
401100170811fm3  C                   endif
401200170810     C*
401300170810     C* se anche la SEDE è extracomunicaria,
401400170810     C* imposto l'identificativo fiscale uguale al kcc + ksc
401500170810     C                   if        $Intra      <> *On      and
401600170811fm3  C                             $PIVA1Mov    = *Blank   and
401700170810     C                             $IdPaes1Mov <> 'IT'
401800170810     C                   Eval      $PIVA1Mov   = MOVKcc + MOVKsc
401900170810     C                   endif
402000170810     C*
402100170810     C                   endSl
402200170810     C*
402300170810     C                   ENDSR
402400170810     C/EJECT
402500170811fm3  C********************************************************************
402600170811 "   C* Verifico se si tratta di Stato CEE / Intra
402700170811 "   C********************************************************************
402800170811fm3  C     ChkIntraPIVA  BEGSR
402900170811     C*
403000170811     C                   MoveL     *Off          WIntra            1
403100170811     C                   Move      *Blank        WStatoW06         2
403200170811     C*
403300170811     C                   Eval      WStatoW06   = WIdPaese
403400170811     C                   EXSR      RepTabW06
403500170811     C                   if        FoundW06    = *On
403600170811     C                   Eval      WIntra      = *On
403700170811     C                   endif
403800170811     C*
403900170811fm3  C                   ENDSR
404000170811fm3  C/EJECT
404100170811fm3  C************************************************************
404200170811 "   C* Reperisco dati tabella W06
404300170811 "   C************************************************************
404400170811fm3  C     RepTabW06     BEGSR
404500170811     C*
404600170811     C                   Move      *Off          FoundW06          1
404700170811     C                   Clear                   ANGW06DS
404800170811     C                   Clear                   SDGTABDS
404900170811     C*
405000170811     C                   Eval      XTbRic      = '1'
405100170811     C                   Eval      XTbAzi      = XScSoc
405200170811     C                   Eval      XTbKey      = *Zero
405300170811     C                   Eval      XTbCod      = 'W06'
405400170811     C                   Move      WStatoW06     XTbKey
405500170811     C*
405600170811     C                   CALL      'SDGTABDR'
405700170811     C                   Parm                    SDGTABDS
405800170811     C*
405900170811     C                   if        XTBErr      = '0'
406000170811     C                   Eval      ANGW06DS    = XTbUni
406100170811     C                   Eval      FoundW06    = *On
406200170811     C                   if        WStatoW06   = 'EL'
406300170811     C                   Eval      WStatoW06   = 'GR'
406400170811     C                   endif
406500170811     C                   endif
406600170811     C*
406700170811fm3  C                   ENDSR
406800170811fm3   /EJECT
406900170811fm3  C********************************************************************
407000170811 "   C* Recupero il codice stato G12 a partire dal codice nazione W06
407100170811 "   C* trovato sulla partita IVA, e con questo accedo alla HD4
407200170811 "   C********************************************************************
407300170811fm3  C     RepIdPaesW06  BEGSR
407400170811     C*
407500170811     C                   Move      *Off          FoundHD4          1
407600170811     C                   Move      *Blank        SavStatoG12       4
407700170811     C*
407800170811     C                   Clear                   SDGTABDS
407900170811     C*
408000170811     C                   MOVE      'G12'         XTBCOD
408100170811     C                   MOVE      XScSoc        XTBAZI
408200170811     C                   MOVE      XScLin        XTBLIN
408300170811     C                   MOVE      '0'           XTBALC
408400170811     C                   MOVE      XScCut        XTBUTE
408500170811     C                   MOVE      '0'           XTBERR
408600170811     C*
408700170811     C* SETLL + READE
408800170811     C                   MOVE      '2'           XTBRIC
408900170811     C                   CALL      'SDGTABDR'
409000170811     C                   PARM                    SDGTABDS
409100170811     C*
409200170811     C                   DoW       XTBErr      = '0'
409300170811     C*
409400170811     C                   if        XTBErr      = '0'
409500170811     C                   MoveL     XtbUni        ANGG12DS
409600170811     C*
409700170811     C                   if        NazG12      = WStatoW06
409800170811     C                   Move(P)   XtbKey        WStato
409900170811     C                   EXSR      RepHD4
410000170811     C*
410100170811     C                   if        FoundHD4    = *On
410200170811     C                   Eval      SavStatoG12 = WStato
410300170811     C                   LEAVE
410400170811     C                   endif
410500170811     C*
410600170811     C                   endif
410700170811     C*
410800170811     C                   endif
410900170811     C*
411000170811     C* READE
411100170811     C                   Move      '3'           XTBRIC
411200170811     C                   MOVE      'G12'         XTBCOD
411300170811     C                   MOVE      '0'           XTBERR
411400170811     C                   CALL      'SDGTABDR'
411500170811     C                   PARM                    SDGTABDS
411600170811     C*
411700170811     C                   endDo
411800170811     C*
411900170811fm3  C                   ENDSR
412000170811fm3  C/EJECT
412100170811fm3  C************************************************************
412200170811 "   C* Reperisco il codice iso da stato G12
412300170811 "   C************************************************************
412400170811fm3  C     RepHD4        BEGSR
412500170811     C*
412600170811     C                   Move      *Off          FoundHD4
412700170811     C                   Clear                   SDGTABDS
412800170811     C*
412900170811     C                   Eval      XTbRic     =  '1'
413000170811     C                   Eval      XTbAzi     =  XScSoc
413100170811     C                   Eval      XTbKey     =  *Zero
413200170811     C                   Eval      XTbCod     =  'HD4'
413300170811     C                   Move      WStato        XTbKey
413400170811     C*
413500170811     C                   CALL      'SDGTABDR'
413600170811     C                   PARM                    SDGTABDS
413700170811     C*
413800170811     C                   if        XTbErr      = '0'
413900170811     C                   MoveL     XTbUni        SDGHD4DS
414000170811     C                   Eval      FoundHD4    = *On
414100170811     C                   endif
414200170811     C*
414300170811fm3  C                   ENDSR
414400170811fm3  C/EJECT
414500170810     C********************************************************************
414600170810     C* Verifico se si tratta di Stato CEE / Intra
414700170810     C********************************************************************
414800170810     C     ChkIntra      BEGSR
414900170810     C*
415000170810     C                   MoveL     *Off          WIntra            1
415100170810     C*
415200170811fm3  X*************      Eval      WIdPaese    = $IdPaes1Mov
415300170810     C                   EXSR      RepNazG12
415400170810     C                   if        FoundNaz    = *On      and
415500170810     C                             NazG12     <> $NazSoc
415600170810     C*
415700170810     C* Controllo se lo Stato è tra quelli dell'INTRA
415800170810     C                   Eval      WIntra      = *On
415900170810     C*
416000170810     C                   endif
416100170810     C*
416200170810     C                   ENDSR
416300170810     C/EJECT
416400170810     C********************************************************************
416500170810     C* Recupero la nazione CEE da G12, a partire dal codice Stato
416600170810     C* associato all'IdPaese della partita IVA
416700170810     C********************************************************************
416800170810     C     RepNazG12     BEGSR
416900170810     C*
417000170810     C                   Move      *Off          FoundNaz          1
417100170810     C                   Move      *Off          FoundHD4          1
417200170810     C                   Move      *Blank        SavStatoHD4       4
417300170810     C*
417400170810     C* Lettura tabella HD4, cercando per codice ISO 2 Alpha
417500170810     C                   Clear                   SDGTABDS
417600170810     C*
417700170810     C                   MOVE      'HD4'         XTBCOD
417800170810     C                   MOVE      XScSoc        XTBAZI
417900170810     C                   MOVE      XScLin        XTBLIN
418000170810     C                   MOVE      '0'           XTBALC
418100170810     C                   MOVE      XScCut        XTBUTE
418200170810     C                   MOVE      '0'           XTBERR
418300170810     C*
418400170810     C* SETLL + READE
418500170810     C                   MOVE      '2'           XTBRIC
418600170810     C                   CALL      'SDGTABDR'
418700170810     C                   PARM                    SDGTABDS
418800170810     C*
418900170810     C                   DoW       XTBErr      = '0'
419000170810     C*
419100170810     C                   if        XTBErr      = '0'
419200170810     C                   MoveL     XtbUni        SDGHD4DS
419300170810     C*
419400170810     C                   if        CodISO2HD4  = WIdPaese
419500170810     C                   Move(P)   XtbKey        WStato
419600170810     C*
419700170810     C                   if        FoundHD4    = *Off
419800170810     C                   Eval      FoundHD4    = *On
419900170810     C                   Eval      SavStatoHD4 = WStato
420000170810     C                   endif
420100170810     C*
420200170810     C                   EXSR      RepTabG12
420300170810     C*
420400170810     C                   if        NazG12     <> *Blank
420500170810     C                   Eval      FoundNaz    = *On
420600170810     C*
420700170810     C* esco al primo codice ISO trovato
420800170810     C                   LEAVE
420900170810     C                   endif
421000170810     C*
421100170810     C                   endif
421200170810     C*
421300170810     C                   endif
421400170810     C*
421500170810     C* READE
421600170810     C                   Move      '3'           XTBRIC
421700170810     C                   MOVE      'HD4'         XTBCOD
421800170810     C                   MOVE      '0'           XTBERR
421900170810     C                   CALL      'SDGTABDR'
422000170810     C                   PARM                    SDGTABDS
422100170810     C*
422200170810     C                   endDo
422300170810     C*
422400170810     C                   ENDSR
422500170810     C/EJECT
422600170810     C********************************************************************
422700170810     C* Controllo se presenti i dati fiscali obbligatori
422800170810     C********************************************************************
422900170810fm2  C     ChkDatiFisc   BEGSR
423000170731     C*
423100170731     C* -----
423200170731     C* recupero lo Stato G12 dell'IdPaese del codice IVA
423300170731     C                   if        $Stato1Mov   = *Blank and
423400170731     C                             $IdPaes1Mov <> *Blank
423500170731     C                   Eval      WIdPaese    = $IdPaes1Mov
423600170731     C                   EXSR      RepNazG12
423700170810     C                   if        FoundHD4    = *On
423800170810     C                   Eval      $Stato1Mov  = SavStatoHD4
423900170731     C                   endif
424000170731     C                   endif
424100170718     C* -----
424200170718     C* CONGRUENZA PARTITA IVA E CODICE FISCALE
424300170718     C*
424400170810     C* se partita iva è estera ed è presente il cod.fiscale,
424500170718     C* segnalo anomalia
424600170731     C                   if        $IdPaes1Mov <> 'IT'    and
424700170731     C                             $IdPaes1Mov <> *Blank  and
424800170731     C                             $PIVA1Mov   <> *Blank  and
424900170731     C                             $CdFisc1Mov <> *Blank
425000170718     C*
425100170810     C************       Eval      $AnnIVA     = *On
425200170718     C                   if        $TpAnn      = *Blank
425300170801     C                             or
425400170801     C                             $TpAnn      = '20'
425500170801     C* incongruenza tra partita iva non italiana e cod. fisc. italiano
425600170810     C************       Eval      $TpAnn      = '70'
425700170718     C                   endif
425800170718     C*
425900170718     C                   endif
426000170718     C*
426100170718     C* -----
426200170718     C* CODICE FISCALE
426300170731     C                   if        $CdFisc1Mov <> *Blank   and
426400170810     C                             $IdPaes2Mov  = 'IT'     and
426500170731     C                             StaSKP       = '1'
426600170718     C*
426700170718     C                   CALLB     'XCDFIS'
426800170731     C                   PARM                    $CdFisc1Mov
426900170718     C                   PARM      *Zero         $Errfisc          1 0
427000170718     C*
427100170718     C* se codice fiscale non valido per la legislazione Italiana
427200170718     C                   if        $Errfisc    < 0
427300170718     C*
427400170718     C* segnalo errore nel valore del codice fiscale, in quanto
427500170718     C* se presente, deve essere valido per i soli soggetti
427600170718     C* residenti in Italia
427700170718     C                   if        Tecnico002  = '2'
427800170810     C*************      Eval      $AnnMov     = *On
427900170810     C*************      Eval      $AnnIVA     = *On
428000170718     C                   else
428100170810     C*************      Eval      $AnnConto   = *On
428200170810     C*************      Eval      $AnnMov     = *On
428300170810     C*************      Eval      $AnnIVA     = *On
428400170718     C                   endif
428500170718     C*
428600170801     C                   if        $TpAnn      = *Blank
428700170801     C* codice fiscale non valido per la legislazione Italiana
428800170810     C*************      Eval      $TpAnn      = '71'
428900170718     C                   endif
429000170718     C*
429100170718     C                   endif
429200170718     C*
429300170718     C                   endif
429400130926     C*
429500130926     C                   ENDSR
429600130926     C/EJECT
429601170906R2656C********************************************************************
429602170906  "  C* Controllo per persona fisica (presenza del nome)
429603170906  "  C********************************************************************
429604170906R2656C     ChkNomPerF    BEGSR
429605170906     C*
429606170906     C                   if        $TpN1Mov    = '1'         and
429607170906     C                             $Nome1Mov   = *Blank
429608170906     C                   Eval      $AnnIVA     = *On
429609170906     C                   Eval      $AnnMOV     = *On
429610170906     C                   if        $TpAnn      = *Blank
429611170906     C                             or
429612170906     C                             $TpAnn      = '20'
429613170906     C                   Eval      $TpAnn      = '07'
429614170906     C                   endif
429615170906     C                   endif
429616170906     C*
429617170906     C                   if        $TpN4Mov    = '1'         and
429618170906     C                             $Nome4Mov   = *Blank
429619170906     C                   Eval      $AnnIVA     = *On
429620170906     C                   Eval      $AnnMOV     = *On
429621170906     C                   if        $TpAnn      = *Blank
429622170906     C                             or
429623170906     C                             $TpAnn      = '20'
429624170906     C                   Eval      $TpAnn      = '07'
429625170906     C                   endif
429626170906     C                   endif
429627170906R2656C                   ENDSR
429628170906R2656C/EJECT
429700130926     C************************************************************
429800130926     C* Verifica dei default beni/servizi da attributo contabile
429900130926     C************************************************************
430000170703     C     RepBenServAttrBEGSR
430100130926     C*
430200110902     C* l'attributo contabile può contenere la forzatura per i
430300110902     C* Clienti/Fornitori che operano prevalentemente con
430400110902     C* con beni o servizi
430500110902     C                   Select
430600110902     C*
430700170616     C                   When      PosAttA12        > 0            and
430800170616     C                             SkAttr(PosAttA12) = BeniA12
430900110902     C                   Eval      $OkBeni         = *On
431000110902     C                   Eval      $OkServizi      = *Off
431100170703     C                   Eval      $NatOpe         = 'B'
431200110902     C*
431300170616     C                   When      PosAttA12        > 0            and
431400170616     C                             SkAttr(PosAttA12)= ServiziA12
431500110902     C                   Eval      $OkBeni         = *Off
431600110902     C                   Eval      $OkServizi      = *On
431700170703     C                   Eval      $NatOpe         = 'S'
431800110902     C*
431900170703     C                   endSl
432000110902     C*
432100110902     C                   ENDSR
432200110902     C/EJECT
432300170816fm6  C************************************************************
432400170816 "   C* Verifica dei default beni/servizi da causale di testata
432500170816 "   C************************************************************
432600170816fm6  C     RepBenServCausBEGSR
432700170816     C*
432800170816     C* verifico con solo la CAUSALE
432900170816     C                   Eval      SPRSocieta  = XScSoc
433000170816     C                   Eval      SPRTPREG    = *Blank
433100170816     C                   Eval      SPRLIBRO    = *Blank
433200170816     C                   Eval      SPRALIQ     = 0
433300170816     C                   Eval      SPRRIFIVA   = *Blank
433400170816     C                   Eval      SPRCAUSALE  = MOVCausTes
433500170816     C                   Eval      SPRKCC      = *Blank
433600170816     C                   Eval      SPRKSC      = *Blank
433700170816     C                   Eval      SPRVOCE     = *Blank
433800170816     C                   Eval      SPRCLASSE   = *Blank
433900170816     C*
434000170816     C* verifico se si tratta di operazione relativa a BENI
434100170816     C                   Eval      SPRTipReg   = '2'
434200170816     C                   Eval      SPRSTpReg   = '1'
434300170816     C                   EXSR      ChainRegola
434400170816     C                   if        *In21       = *Off
434500170816     C                   Eval      $OkBeni     = *On
434600170816     C                   Eval      $OkServizi  = *Off
434700170816     C                   Eval      $NatOpe     = 'B'
434800170816     C                   endif
434900170816     C*
435000170816     C                   if        $NatOpe     = *Blank
435100170816     C*
435200170816     C* verifico se si tratta di operazione relativa a SERVIZI
435300170816     C                   Eval      SPRTipReg   = '2'
435400170816     C                   Eval      SPRSTpReg   = '2'
435500170816     C                   EXSR      ChainRegola
435600170816     C                   if        *In21       = *Off
435700170816     C                   Eval      $OkBeni     = *Off
435800170816     C                   Eval      $OkServizi  = *On
435900170816     C                   Eval      $NatOpe     = 'S'
436000170816     C                   endif
436100170816     C*
436200170816     C                   endif
436300170816     C*
436400170816fm6  C                   ENDSR
436500170816fm6  C/EJECT
436600000000     C************************************************************
436700000000     C* Analisi della contropartita (costo/ricavo) per definire
436800000000     C* se si tratta di un bene o di un servizio
436900000000     C************************************************************
437000000000     C     ChkBeniServiziBEGSR
437100000000     C*
437200000000     C* reperisco le informazioni sulle righe di contropartita
437300170816     C                   EXSR      RepCP_BS
437400000000     C*
437500000000     C* gestisco solo i casi fattibili, ovvero quelli in cui
437600000000     C* è presente una sola tipologia operazione
437700000000     C* 1 : 1   una tipologia iva e una tipologia operazione
437800000000     C* N : 1   n  tipologie iva e una tipologia operazione
437900000000     C*
438000000000     C* casistiche non gestite
438100000000     C* 1 : N   una tipologia iva e 2 tipologie operazione
438200000000     C* N : N   più tipologie iva e 2 tipologie operazione
438300000000     C*
438400000000     C*
438500000000     C                   Select
438600000000     C*
438700000000     C*  se presente una sola tipologia di operazione, imposto
438800000000     C*  quella tipologia su tutte le righe
438900000000     C                   When      $OkBeni         = *On  and
439000000000     C                             $OkServizi     <> *On
439100130821     C                   Eval      $NatOpe         = 'B'
439200000000     C*
439300000000     C                   When      $OkBeni        <> *On  and
439400000000     C                             $OkServizi      = *On
439500130821     C                   Eval      $NatOpe         = 'S'
439600170706     C*
439700170706     C                   When      $OkBeni         = *On  and
439800170706     C                             $OkServizi      = *On  and
439900170706     C                             %Abs($ImpBeni)  > %Abs($ImpServizi)
440000170706     C                   Eval      $NatOpe         = 'B'
440100170706     C*
440200170706     C                   When      $OkBeni         = *On  and
440300170706     C                             $OkServizi      = *On  and
440400170706     C                             %Abs($ImpBeni)  < %Abs($ImpServizi)
440500170706     C                   Eval      $NatOpe         = 'S'
440600000000     C*
440700170703     C                   When      $OkBeni         = *On  and
440800170706     C                             $OkServizi      = *On  and
440900170706     C                             %Abs($ImpBeni)  = %Abs($ImpServizi)
441000170706     C*
441100170706     C* in caso fatture ricevute intracomunitarie, serve distinguere tra
441200170706     C* beni e servizi: annullo
441300170706     C                   if        $Intra     = *On      and
441400170724     C                             $CF         = 'F'
441500170706     C*
441600000000     C* annullo la riga in elaborazione  e la registrazione
441700000000     C* in modo da annullare anche tutte le altre
441800000000     C                   Eval      $AnnIVA     = *On
441900000000     C                   Eval      $AnnMOV     = *On
442000000000     C                   if        $TpAnn      = *Blank
442100000000     C*
442200000000     C* salvo il movimento di contropartita
442300170801     C                   Move      NDMOV00F      NDMOVSav2
442400000000     C*
442500000000     C                   Eval      $TpAnn      = '20'
442600000000     C*
442700000000     C                   endif
442800170706     C*
442900170706     C                   endif
443000000000     C*
443100000000     C                   endSl
443200000000     C*
443300000000     C                   ENDSR
443400000000     C/EJECT
443500000000     C************************************************************
443600000000     C* Reperimento tipologia di contropartita
443700000000     C************************************************************
443800170816     C     RepCP_BS      BEGSR
443900000000     C*
444000000000     C* Lettura dei movimenti di contropartita legati alla registrazione
444100170724     C                   Eval      $CodOperCP  = '1'
444200000000     C                   EXSR      GetMOVCP
444300000000     C*
444400170724     C                   DoW       $CodOperCP <> '9'
444500000000     C*
444600170816     C                   EXSR      SelCP_BS
444700000000     C*
444800170818fm9  C                   if        $OkCP_BS    = *On
444900000000     C* verifico di quale caso di contropartita si tratta
445000170816     C                   EXSR      ChkCP_BS
445100170818fm9  C                   endif
445200000000     C*
445300170724     C                   Eval      $CodOperCP  = '2'
445400000000     C                   EXSR      GetMOVCP
445500000000     C                   ENDDO
445600000000     C*
445700000000     C                   ENDSR
445800000000      /EJECT
445900000000     C************************************************************
446000000000     c* Reperisce NDMOV di contropartita
446100000000     C************************************************************
446200000000     C     GetMOVCP      BEGSR
446300000000     C*
446400000000     C                   Eval      $LenOut     = %size(NDMOV00F)
446500000000     C                   CALLB     'NDDRVMOV'
446600000000     C                   PARM                    IVASys
446700000000     C                   PARM                    IVANrAsReg
446800000000     C                   PARM                    *Omit
446900000000     C                   PARM      'NDMOV     '  $Struttura
447000000000     C                   PARM                    $LenOut
447100170724     C                   PARM                    $CodOperCP
447200000000     C                   PARM                    NDMOV00F
447300000000     C*
447400000000     C                   ENDSR
447500000000     C/EJECT
447600000000     C****************************************************************
447700000000     C* Selezione dei movimenti di controparita
447800000000     C****************************************************************
447900170816     C     SelCP_BS      BEGSR
448000000000     C*
448100170816     C                   Eval      $OkCP_BS    = *On
448200000000     C*
448300000000     C                   Select
448400000000     C* seleziono solo i cespiti ammortizzabili e gli altri conti
448500000000     C* (costi e ricavi)
448600000000     C                   When      Not(MOVSNatura  = '4'  or
448700000000     C                                 MOVSNatura  = '9'    )
448800170816     C                   Eval      $OkCP_BS    = *Off
448900000000     C*
449000000000     C                   endSl
449100000000     C*
449200170818fm8  X**** ---------------
449300170818 "   X****               if        $OkCP_BS    = *On
449400170818 "   X****
449500170818 "   X****verifico se conto di controparita è da escludere nella determinazione
449600170818 "   X**** della tipologia di operazione ( sconti, spese, commissioni)
449700170818 "   X****
449800170818 "   X**** recupero l'eventuale voce di analitica per verificare
449900170818 "   X**** se il movimento deve essere escluso tramite la voce
450000170818 "   X****               Eval      $Voce       = *Blank
450100170818 "   X****
450200170818 "   X**** Scorro le righe di analitica della prima dimensione
450300170818 "   X**** in ordine alfabetico. E' sufficiente scorrere le righe di
450400170818 "   X**** una sola dimensione, in quanto se presenti anche più voci
450500170818 "   X**** legate allo stesso conto, la definizione di beni/servizi
450600170818 "   X**** o l'esclusione sarebbe logicamente la stessa in quanto
450700170818 "   X**** le voci sono legate allo stesso conto
450800170818 "   X****               Eval      MOASys     = MOVSys
450900170818 "   X****               Eval      MOANrAsReg = MOVNrAsReg
451000170818 "   X****               Eval      MOANrRigaM = MOVNrRigaM
451100170818 "   X**** K03MOA01      Setll     NDMOA01L
451200170818 "   X**** K03MOA01      READE     NDMOA01L                               21
451300170818 "   X****               Eval      $FineMOA   = *In21
451400170818 "   X****
451500170818 "   X****               Eval      SPRSocieta  = XScSoc
451600170818 "   X****               Eval      SPRTipReg   = '6'
451700170818 "   X****               Eval      SPRSTpReg   = '2'
451800170818 "   X****               Eval      SPRTPREG    = *Blank
451900170818 "   X****               Eval      SPRLIBRO    = *Blank
452000170818 "   X****               Eval      SPRALIQ     = 0
452100170818 "   X****               Eval      SPRRIFIVA   = *Blank
452200170818 "   X****               Eval      SPRCAUSALE  = *Blank
452300170818 "   X****               Eval      SPRClasse   = *Blank
452400170818 "   X****
452500170818 "   X**** la prima volta entra comunque, anche se non ha trovato
452600170818 "   X****  il record di NDMOA, esce dal ciclo quando ha finito di
452700170818 "   X****  leggere tutte le righe della prima dimensione
452800170818 "   X****               DoU       $FineMOA   = *On
452900170818 "   X****
453000170818 "   X****               if        $OkCP_BS    = *On
453100170818 "   X****                         and  $FineMOA <> *On
453200170818 "   X****  1° tentativo con KCC + KSC + VOCE
453300170818 "   X****               Eval      SPRKCC      = MOVKcc
453400170818 "   X****               Eval      SPRKSC      = MOVKsc
453500170818 "   X****               Eval      SPRVoce     = MOAVoce
453600170818 "   X****               EXSR      ChainRegola
453700170818 "   X****               if        *In21       = *Off
453800170818 "   X****  se trovo il record significa che il conto è escluso
453900170818 "   X****               Eval      $OkCP_BS    = *Off
454000170818 "   X****               Eval      $FineMOA   = *On
454100170818 "   X****               endif
454200170818 "   X****
454300170818 "   X****               endif
454400170818 "   X****
454500170818 "   X****  ---------------
454600170818 "   X****
454700170818 "   X****               if        $OkCP_BS    = *On
454800170818 "   X**** 2° tentativo con KCC + KSC
454900170818 "   X****verifico se conto di controparita è da escludere nella determinazione
455000170818 "   X**** della tipologia di operazione ( sconti, spese, commissioni)
455100170818 "   X****               Eval      SPRKCC      = MOVKcc
455200170818 "   X****               Eval      SPRKSC      = MOVKsc
455300170818 "   X****               Eval      SPRVoce     = *Blank
455400170818 "   X****               EXSR      ChainRegola
455500170818 "   X****               if        *In21       = *Off
455600170818 "   X**** se trovo il record significa che il conto è escluso
455700170818 "   X****               Eval      $OkCP_BS    = *Off
455800170818 "   X****               Eval      $FineMOA   = *On
455900170818 "   X****               endif
456000170818 "   X****
456100170818 "   X****               endif
456200170818 "   X****
456300170818 "   X**** ---------------
456400170818 "   X****
456500170818 "   X****               if        $OkCP_BS    = *On
456600170818 "   X****                         and  $FineMOA <> *On
456700170818 "   X**** 3° tentativo con KCC + VOCE
456800170818 "   X****verifico se conto di controparita è da escludere nella determinazione
456900170818 "   X**** della tipologia di operazione ( sconti, spese, commissioni)
457000170818 "   X****               Eval      SPRKCC      = MOVKcc
457100170818 "   X****               Eval      SPRKSC      = *Blank
457200170818 "   X****               Eval      SPRVoce     = MOAVoce
457300170818 "   X****               EXSR      ChainRegola
457400170818 "   X****               if        *In21       = *Off
457500170818 "   X**** se trovo il record significa che il conto è escluso
457600170818 "   X****               Eval      $OkCP_BS    = *Off
457700170818 "   X****               Eval      $FineMOA   = *On
457800170818 "   X****               endif
457900170818 "   X****
458000170818 "   X****               endif
458100170818 "   X****
458200170818 "   X**** ---------------
458300170818 "   X**** 4° tentativo solo con KCC
458400170818 "   X****               if        $OkCP_BS    = *On
458500170818 "   X****verifico se conto di controparita è da escludere nella determinazione
458600170818 "   X**** della tipologia di operazione ( sconti, spese, commissioni)
458700170818 "   X****               Eval      SPRKCC      = MOVKcc
458800170818 "   X****               Eval      SPRKSC      = *Blank
458900170818 "   X****               Eval      SPRVoce     = *Blank
459000170818 "   X****               EXSR      ChainRegola
459100170818 "   X****               if        *In21       = *Off
459200170818 "   X**** se trovo il record significa che il conto è escluso
459300170818 "   X****               Eval      $OkCP_BS    = *Off
459400170818 "   X****               Eval      $FineMOA   = *On
459500170818 "   X****               endif
459600170818 "   X****
459700170818 "   X****               endif
459800170818 "   X****
459900170818 "   X**** ---------------
460000170818 "   X**** 5° tentativo solo con VOCE
460100170818 "   X****               if        $OkCP_BS    = *On
460200170818 "   X****                         and  $FineMOA <> *On
460300170818 "   X****verifico se conto di controparita è da escludere nella determinazione
460400170818 "   X**** della tipologia di operazione ( sconti, spese, commissioni)
460500170818 "   X****               Eval      SPRKCC      = *Blank
460600170818 "   X****               Eval      SPRKSC      = *Blank
460700170818 "   X****               Eval      SPRVoce     = MOAVoce
460800170818 "   X****               EXSR      ChainRegola
460900170818 "   X****               if        *In21       = *Off
461000170818 "   X**** se trovo il record significa che il conto è escluso
461100170818 "   X****               Eval      $OkCP_BS    = *Off
461200170818 "   X****               Eval      $FineMOA   = *On
461300170818 "   X****               endif
461400170818 "   X****
461500170818 "   X****               endif
461600170818 "   X****
461700170818 "   X****               if        $FineMOA    = *Off
461800170818 "   X****
461900170818 "   X**** leggo solo i movimenti della prima dimensione su cui
462000170818 "   X**** mi sono posizionata (vd. VL K04MOA01)
462100170818 "   X**** K04MOA01      READE     NDMOA01L                               21
462200170818 "   X****               Eval      $FineMOA   = *In21
462300170818 "   X****               endif
462400170818 "   X****
462500170818 "   X****               EndDo
462600170818 "   X****
462700170818fm8  X****               endif
462800170801     C*
462900170801     C                   ENDSR
463000170801      /EJECT
463100000000     C****************************************************************
463200000000     C* Verifico di quale caso di contropartita si tratta
463300000000     C****************************************************************
463400170816     C     ChkCP_BS      BEGSR
463500000000     C*
463600000000     C* reperisco la voce sul movimento di analitica
463700000000     C                   EXSR      RepMOAVoce
463800000000     C*
463900000000     C* verifico se si tratta di operazione relativa
464000000000     C* a cessione di beni
464100130823     C                   Eval      SPRTipReg   = '2'
464200130823     C                   Eval      SPRSTpReg   = '1'
464300130924     C                   EXSR      ChkRegBenSrv
464400000000     C*
464500130924     C                   if        $OkRegBenSrv<> *On
464600000000     C* verifico se si tratta di operazione relativa
464700000000     C* a prestazione di servizi
464800130823     C                   Eval      SPRTipReg   = '2'
464900130823     C                   Eval      SPRSTpReg   = '2'
465000130924     C                   EXSR      ChkRegBenSrv
465100000000     C                   endif
465200000000     C*
465300000000     C*
465400130924     C                   if        $OkRegBenSrv= *On
465500000000     C*
465600130823     C                   if        SPRSTpReg   = '1'
465700000000     C*
465800000000     C                   Eval      $OkBeni     = *On
465900170811fm3  C                   Eval      $ImpBeni        = $ImpBeni
466000170811fm3  C                                               + ((MOVDare - MOVAvere) *
466100170706     C                                                 MOVImporto )
466200000000     C*
466300000000     C                   endif
466400000000     C*
466500000000     C*
466600130823     C                   if        SPRSTpReg   = '2'
466700000000     C*
466800000000     C                   Eval      $OkServizi  = *On
466900170811fm3  C                   Eval      $ImpServizi     = $ImpServizi
467000170811fm3  C                                               + ((MOVDare - MOVAvere) *
467100170706     C                                                 MOVImporto )
467200000000     C*
467300000000     C                   endif
467400000000     C*
467500170801     C                   else
467600170801     C* se non trovata la regola, imposto il default
467700170801     C                   if        BeniSrvA12  = 'B'
467800170801     C                   Eval      $OkBeni     = *On
467900170815fm5  C                   Eval      $ImpBeni        = $ImpBeni
468000170815fm5  C                                               + ((MOVDare - MOVAvere) *
468100170801     C                                                 MOVImporto )
468200170801     C                   else
468300170801     C                   Eval      $OkServizi  = *On
468400170815fm5  C                   Eval      $ImpServizi     = $ImpServizi
468500170815fm5  C                                               + ((MOVDare - MOVAvere) *
468600170801     C                                                 MOVImporto )
468700170801     C                   endif
468800170801     C                   endif
468900000000     C*
469000000000     C                   ENDSR
469100000000      /EJECT
469200000000     C************************************************************
469300000000     C* Reperisco l'eventuale voce di analitica
469400000000     C************************************************************
469500000000     C     RepMOAVoce    BEGSR
469600000000     C*
469700000000     C                   Eval      $OkVoce     = *On
469800000000     C                   Eval      SavVoce     = *Blank
469900000000     C*
470000000000     C                   Eval      MOASys      = MOVSys
470100000000     C                   Eval      MOANrAsReg  = MOVNrAsReg
470200000000     C                   Eval      MOANrRigaM  = MOVNrRigaM
470300000000     C     K03MOA01      SETLL     NDMOA01L
470400000000     C     K03MOA01      READE     NDMOA01L                               21
470500000000     C                   Move      *In21         $FineMOA
470600000000     C                   if        $FineMOA    = *Off
470700000000     C                   Eval      SavVoce     = MOAVoce
470800000000     C                   endif
470900000000     C*
471000000000     C                   DoW       $FineMOA    = *Off
471100000000     C*
471200000000     C                   if        SavVoce    <> MOAVoce
471300000000     C                   Eval      $OkVoce     = *Off
471400000000     C                   endif
471500000000     C*
471600170801     C* leggo solo i movimenti della prima dimensione su cui
471700170801     C* mi sono posizionata (vd. VL K04MOA01)
471800170801     C* In questo modo non scarto la voce se ne trovo una
471900170801     C* diversa su un'altra dimensione, in quanto in quel caso
472000170801     C* potrebbe avere la stessa natura e sarebbe sbagliato
472100170801     C* scartarla
472200170801     C     K04MOA01      READE     NDMOA01L                               21
472300170801     C                   Move      *In21         $FineMOA
472400170801     C                   endDo
472500000000     C*
472600000000     C                   if        SavVoce     = *Blank
472700000000     C                   Eval      $OkVoce     = *Off
472800000000     C                   endif
472900000000     C*
473000000000     C                   ENDSR
473100000000      /EJECT
473200000000     C************************************************************
473300000000     C* Verifico se si tratta di cessione di beni
473400000000     C* o di prestazione di servizi
473500000000     C************************************************************
473600130924     C     ChkRegBenSrv  BEGSR
473700000000     C*
473800000000     C* si assume che per definire la natura un costo/ricavo
473900000000     C* siano da considerare in ordine di importanza:
474000000000     C* 1) il conto di costo/ricavo
474100170816fm6  X***** 2) la causale
474200170816fm6  X***** 3) la voce
474300170816fm6  C* 2) la voce
474400130924     C                   Eval      $OkRegBenSrv = *Off
474500000000     C*
474600130823     C                   Eval      SPRSocieta  = XScSoc
474700000000     C*
474800000000     C*------------------------------
474900000000     C* 1° tentativo con chiave completa:
475000000000     C*    causale + conto ctp + voce
475100000000     C                   if        $OkVoce     = *On
475200170816     C                             and  $OkCP_BS    = *On
475300130823     C                   Eval      SPRTPREG    = *Blank
475400130823     C                   Eval      SPRLIBRO    = *Blank
475500130823     C                   Eval      SPRALIQ     = 0
475600130823     C                   Eval      SPRRIFIVA   = *Blank
475700130823     C                   Eval      SPRCAUSALE  = MOVCausTes
475800130823     C                   Eval      SPRKCC      = MOVKcc
475900130823     C                   Eval      SPRKSC      = MOVKsc
476000130823     C                   Eval      SPRVOCE     = MOAVoce
476100130823     C                   Eval      SPRCLASSE   = *Blank
476200170703     C                   EXSR      ChainRegola
476300000000     C                   if        *In21       = *Off
476400130924     C                   Eval      $OkRegBenSrv = *On
476500000000     C                   endif
476600000000     C                   endif
476700000000     C*
476800110830     C*------------------------------
476900110830     C* 1°bis tentativo con chiave completa:
477000110830     C*    causale + conto ctp (solo KCC) + voce
477100110830     C                   if        $OkVoce     = *On
477200170816     C                             and  $OkCP_BS    = *On
477300170801     C                             and  $OkRegBenSrv = *Off
477400130823     C                   Eval      SPRTPREG    = *Blank
477500130823     C                   Eval      SPRLIBRO    = *Blank
477600130823     C                   Eval      SPRALIQ     = 0
477700130823     C                   Eval      SPRRIFIVA   = *Blank
477800130823     C                   Eval      SPRCAUSALE  = MOVCausTes
477900130823     C                   Eval      SPRKCC      = MOVKcc
478000130823     C                   Eval      SPRKSC      = *Blank
478100130823     C                   Eval      SPRVOCE     = MOAVoce
478200130823     C                   Eval      SPRCLASSE   = *Blank
478300170703     C                   EXSR      ChainRegola
478400110830     C                   if        *In21       = *Off
478500130924     C                   Eval      $OkRegBenSrv = *On
478600110830     C                   endif
478700110830     C                   endif
478800110830     C*
478900000000     C*------------------------------
479000000000     C* 2° tentativo: chiave senza voce:
479100000000     C*    conto ctp + causale
479200130924     C                   if        $OkRegBenSrv = *Off
479300170816     C                             and  $OkCP_BS    = *On
479400130823     C                   Eval      SPRTPREG    = *Blank
479500130823     C                   Eval      SPRLIBRO    = *Blank
479600130823     C                   Eval      SPRALIQ     = 0
479700130823     C                   Eval      SPRRIFIVA   = *Blank
479800130823     C                   Eval      SPRCAUSALE  = MOVCausTes
479900130823     C                   Eval      SPRKCC      = MOVKcc
480000130823     C                   Eval      SPRKSC      = MOVKsc
480100130823     C                   Eval      SPRVOCE     = *Blank
480200130823     C                   Eval      SPRCLASSE   = *Blank
480300170703     C                   EXSR      ChainRegola
480400000000     C                   if        *In21       = *Off
480500130924     C                   Eval      $OkRegBenSrv = *On
480600000000     C                   endif
480700000000     C                   endif
480800110825     C*
480900110830     C*------------------------------
481000110830     C* 2°bis tentativo: chiave senza voce:
481100110830     C*    conto ctp (solo KCC)+ causale
481200130924     C                   if        $OkRegBenSrv = *Off
481300170816     C                             and  $OkCP_BS    = *On
481400130823     C                   Eval      SPRTPREG    = *Blank
481500130823     C                   Eval      SPRLIBRO    = *Blank
481600130823     C                   Eval      SPRALIQ     = 0
481700130823     C                   Eval      SPRRIFIVA   = *Blank
481800130823     C                   Eval      SPRCAUSALE  = MOVCausTes
481900130823     C                   Eval      SPRKCC      = MOVKcc
482000130823     C                   Eval      SPRKSC      = *Blank
482100130823     C                   Eval      SPRVOCE     = *Blank
482200130823     C                   Eval      SPRCLASSE   = *Blank
482300170703     C                   EXSR      ChainRegola
482400110830     C                   if        *In21       = *Off
482500130924     C                   Eval      $OkRegBenSrv = *On
482600110830     C                   endif
482700110830     C                   endif
482800110830     C*
482900170816fm6  X****------------------------------
483000170816 "   X**** 3° tentativo: chiave senza conto e voce:
483100170816 "   X**** Causale
483200170816 "   X****               if        $OkRegBenSrv = *Off
483300170816 "   X****               Eval      SPRTPREG    = *Blank
483400170816 "   X****               Eval      SPRLIBRO    = *Blank
483500170816 "   X****               Eval      SPRALIQ     = 0
483600170816 "   X****               Eval      SPRRIFIVA   = *Blank
483700170816 "   X****               Eval      SPRCAUSALE  = MOVCausTes
483800170816 "   X****               Eval      SPRKCC      = *Blank
483900170816 "   X****               Eval      SPRKSC      = *Blank
484000170816 "   X****               Eval      SPRVOCE     = *Blank
484100170816 "   X****               Eval      SPRCLASSE   = *Blank
484200170816 "   X****               EXSR      ChainRegola
484300170816 "   X****               if        *In21       = *Off
484400170816 "   X****               Eval      $OkRegBenSrv = *On
484500170816 "   X****               endif
484600170816fm6  X****               endif
484700170801     C*
484800170801     C*------------------------------
484900170801     C* 3° bis tentativo: chiave kcc + ksc + voce
485000170801     C                   if        $OkRegBenSrv = *Off
485100170801     C                             and  $OkVoce     = *On
485200170816     C                             and  $OkCP_BS    = *On
485300170801     C                   Eval      SPRTPREG    = *Blank
485400170801     C                   Eval      SPRLIBRO    = *Blank
485500170801     C                   Eval      SPRALIQ     = 0
485600170801     C                   Eval      SPRRIFIVA   = *Blank
485700170801     C                   Eval      SPRCAUSALE  = *Blank
485800170801     C                   Eval      SPRKCC      = MOVKcc
485900170801     C                   Eval      SPRKSC      = MOVKsc
486000170801     C                   Eval      SPRVOCE     = MOAVoce
486100170801     C                   Eval      SPRCLASSE   = *Blank
486200170801     C                   EXSR      ChainRegola
486300170801     C                   if        *In21       = *Off
486400170801     C                   Eval      $OkRegBenSrv = *On
486500170801     C                   endif
486600170801     C                   endif
486700170801     C*
486800000000     C*------------------------------
486900110830     C* 4° tentativo: chiave senza causale e voce:
487000110830     C*    conto ctp
487100130924     C                   if        $OkRegBenSrv = *Off
487200170816     C                             and  $OkCP_BS    = *On
487300170801     C                   Eval      SPRTPREG    = *Blank
487400170801     C                   Eval      SPRLIBRO    = *Blank
487500170801     C                   Eval      SPRALIQ     = 0
487600170801     C                   Eval      SPRRIFIVA   = *Blank
487700170801     C                   Eval      SPRCAUSALE  = *Blank
487800170801     C                   Eval      SPRKCC      = MOVKcc
487900170801     C                   Eval      SPRKSC      = MOVKsc
488000170801     C                   Eval      SPRVOCE     = *Blank
488100170801     C                   Eval      SPRCLASSE   = *Blank
488200170801     C                   EXSR      ChainRegola
488300170801     C                   if        *In21       = *Off
488400170801     C                   Eval      $OkRegBenSrv = *On
488500170801     C                   endif
488600170801     C                   endif
488700170801     C*
488800170801     C*------------------------------
488900170801     C* 4° bis tentativo: chiave kcc + voce
489000170801     C                   if        $OkRegBenSrv = *Off
489100170801     C                             and  $OkVoce     = *On
489200170816     C                             and  $OkCP_BS    = *On
489300170801     C                   Eval      SPRTPREG    = *Blank
489400170801     C                   Eval      SPRLIBRO    = *Blank
489500170801     C                   Eval      SPRALIQ     = 0
489600170801     C                   Eval      SPRRIFIVA   = *Blank
489700170801     C                   Eval      SPRCAUSALE  = *Blank
489800170801     C                   Eval      SPRKCC      = MOVKcc
489900170801     C                   Eval      SPRKSC      = *Blank
490000170801     C                   Eval      SPRVOCE     = MOAVoce
490100170801     C                   Eval      SPRCLASSE   = *Blank
490200170801     C                   EXSR      ChainRegola
490300170801     C                   if        *In21       = *Off
490400170801     C                   Eval      $OkRegBenSrv = *On
490500170801     C                   endif
490600170801     C                   endif
490700170801     C*
490800170801     C*------------------------------
490900170801     C* 4° bis tentativo: chiave senza causale e voce:
491000170801     C*    conto ctp (solo KCC)
491100170801     C                   if        $OkRegBenSrv = *Off
491200170816     C                             and  $OkCP_BS    = *On
491300170801     C                   Eval      SPRTPREG    = *Blank
491400130823     C                   Eval      SPRLIBRO    = *Blank
491500130823     C                   Eval      SPRALIQ     = 0
491600130823     C                   Eval      SPRRIFIVA   = *Blank
491700130823     C                   Eval      SPRCAUSALE  = *Blank
491800130823     C                   Eval      SPRKCC      = MOVKcc
491900130823     C                   Eval      SPRKSC      = *Blank
492000130823     C                   Eval      SPRVOCE     = *Blank
492100130823     C                   Eval      SPRCLASSE   = *Blank
492200170703     C                   EXSR      ChainRegola
492300110830     C                   if        *In21       = *Off
492400130924     C                   Eval      $OkRegBenSrv = *On
492500110830     C                   endif
492600110830     C                   endif
492700110830     C*
492800110830     C*------------------------------
492900110830     C* 5° tentativo: chiave senza conto e causale:
493000110830     C*    voce
493100130924     C                   if        $OkRegBenSrv = *Off
493200110825     C                   if        $OkVoce     = *On
493300130823     C                   Eval      SPRTPREG    = *Blank
493400130823     C                   Eval      SPRLIBRO    = *Blank
493500130823     C                   Eval      SPRALIQ     = 0
493600130823     C                   Eval      SPRRIFIVA   = *Blank
493700130823     C                   Eval      SPRCAUSALE  = *Blank
493800130823     C                   Eval      SPRKCC      = *Blank
493900130823     C                   Eval      SPRKSC      = *Blank
494000130823     C                   Eval      SPRVOCE     = MOAVoce
494100130823     C                   Eval      SPRCLASSE   = *Blank
494200170703     C                   EXSR      ChainRegola
494300110825     C                   if        *In21       = *Off
494400130924     C                   Eval      $OkRegBenSrv = *On
494500110825     C                   endif
494600110825     C                   endif
494700110825     C                   endif
494800000000     C*
494900000000     C                   ENDSR
495000000000     C/EJECT
495100000000     C************************************************************
495200000000     C* INIZIALIZZAZIONE VARIABILI
495300000000     C************************************************************
495400000000     C     INZVAR        BEGSR
495500000000     C*
495600170615     C                   Eval      SDGSI52DS   = KPJBU
495700000000     C*
495800000000     C*-------------------
495900170529     C* cancello SDGCST00F in base alle selezioni effettuate
496000170529     C                   EXSR      DelSDGCST
496100000000     C*
496200000000     C                   CALLB     'XSTRCMT'
496300170718     C                   OPEN      SDGCST00F
496400170724     C                   OPEN      SDGCSF03L
496401170906BRT  C***                   open      sdgcstlog
496500130826     C*
496600000000     C                   ENDSR
496700000000     C/EJECT
496800000000     C************************************************************
496900170529     C* Cancellazione  SDGCST00F
497000000000     C************************************************************
497100170529     C     DelSDGCST     BEGSR
497200000000     C*
497300000000     C* cancello solo i record inseriti automaticamente
497400170529     C                   EXSR      RepSQLCST
497500000000     C*
497600170529     C                   EXSR      ExeSQLCST
497700000000     C*
497800000000     C* Riorganizzazione membro
497900000000     C                   Clear                   CmdQcmd
498000170529     C                   Eval      CmdQcmd='RGZPFM FILE(SDGCST00F)'
498100000000     C                   call      'QCMDEXC'                            28
498200000000     C                   PARM                    CmdQcmd         160
498300000000     C                   PARM      80            Len              15 5
498400000000     C*
498500000000     C                   ENDSR
498600000000     C/EJECT
498700000000     C************************************************************
498800170529     C* REPERIMENTO SCHIERA PER SQLCST
498900000000     C************************************************************
499000170529     C     RepSQLCST     BEGSR
499100000000     C*
499200170529     C                   Clear                   SqlCST
499300000000     C*
499400170529     C* Delete from SDGCST00F
499500170529     C                   CAT       'DELETE':1    SqlCST
499600170529     C                   CAT       'from':1      SqlCST
499700170529     C                   CAT       'SDGCST00F':1 SqlCST
499800000000     C*
499900000000     C* SELEZIONI
500000170529     C                   CAT       'WHERE':1     SqlCST
500100170529     C                   EXSR      WhereCST
500200000000     C*
500300000000     C                   ENDSR
500400000000     C/EJECT
500500000000     C******************************************************
500600170529     C* Selezioni su SDGCST00F
500700000000     C******************************************************
500800170529     C     WhereCST      BEGSR
500900000000     C*
501000170615     C* CSTSocieta = SocietaI52
501100170529     C                   CAT       'CSTSocieta':1SQLCST
501200170529     C                   CAT       '=''':0       SQLCST
501300170615     C                   CAT       SocietaI52:0  SQLCST
501400170717     C                   CAT       '''':0        SQLCST
501500170721     C*
501600170721     C* and CSTTpCom = TpComI52
501700170721     C                   CAT       'and':1       SQLCST
501800170721     C                   CAT       'CSTTpCom':1  SQLCST
501900170721     C                   CAT       '=''':0       SQLCST
502000170721     C                   CAT       TpComI52:0    SQLCST
502100170721     C                   CAT       '''':0        SQLCST
502200170721     C*
502300170721     C* and CSTAnno = AnnoI52
502400170721     C                   MoveL     AnnoI52       $Anno
502500170721     C                   CAT       'and':1       SQLCST
502600170721     C                   CAT       'CSTAnno':1   SQLCST
502700170721     C                   CAT       '=':0         SQLCST
502800170721     C                   CAT       $Anno:0       SQLCST
502900170721     C*
503000170721     C* and CSTTrim = TrimI52
503100170721     C                   MoveL     TrimI52       $Trim
503200170721     C                   CAT       'and':1       SQLCST
503300170721     C                   CAT       'CSTTrim':1   SQLCST
503400170721     C                   CAT       '=':0         SQLCST
503500170721     C                   CAT       $Trim:0       SQLCST
503600170721     C*
503700170721     C* and CSTMese = MeseI52
503800170721     C                   MoveL     MeseI52       $Mese
503900170721     C                   CAT       'and':1       SQLCST
504000170721     C                   CAT       'CSTMese':1   SQLCST
504100170721     C                   CAT       '=':0         SQLCST
504200170721     C                   CAT       $Mese:0       SQLCST
504300170717     C*
504400170717     C* and CSTTpRec = TpRecI52
504500170717     C                   CAT       'and':1       SQLCST
504600170717     C                   CAT       'CSTTpRec':1  SQLCST
504700170717     C                   CAT       '=''':0       SQLCST
504800170717     C                   CAT       TpRecI52:0    SQLCST
504900170717     C                   CAT       '''':0        SQLCST
505000170721     C*
505100170721     C* and CSTStato2 = Stato2I52
505200170721     C                   if        Stato2I52  <> *Blank
505300170721     C                   CAT       'and':1       SQLCST
505400170721     C                   CAT       'CSTStato2':1 SQLCST
505500170721     C                   CAT       '=''':0       SQLCST
505600170721     C                   CAT       Stato2I52:0   SQLCST
505700170721     C                   CAT       '''':0        SQLCST
505800170721     C                   endif
505900170721     C*
506000170721     C* and CSTCF   = CFI52
506100170721     C                   if        CFI52      <> *Blank
506200170721     C                   CAT       'and':1       SQLCST
506300170721     C                   CAT       'CSTCF':1     SQLCST
506400170721     C                   CAT       '=''':0       SQLCST
506500170721     C                   CAT       CFI52:0       SQLCST
506600170721     C                   CAT       '''':0        SQLCST
506700170721     C                   endif
506800170721     C*
506900170721     C* and CSTPartIVA =  PartIVAI52
507000170721     C                   if        PartIVAI52 <> *Blank
507100170721     C                   CAT       'and':1       SQLCST
507200170721     C                   CAT       'CSTPartIVA':1SQLCST
507300170721     C                   CAT       '=''':0       SQLCST
507400170721     C                   CAT       PartIVAI52:0  SQLCST
507500170721     C                   CAT       '''':0        SQLCST
507600170721     C                   endif
507700170721     C*
507800170721     C* and CSTCdFisc  = CdFiscI52
507900170721     C                   if        CdFiscI52  <> *Blank
508000170721     C                   CAT       'and':1       SQLCST
508100170721     C                   CAT       'CSTCdFisc':1 SQLCST
508200170721     C                   CAT       '=''':0       SQLCST
508300170721     C                   CAT       CdFiscI52:0   SQLCST
508400170721     C                   CAT       '''':0        SQLCST
508500170721     C                   endif
508600170721     C*
508700170721     C* and CSTSogg = SoggI52
508800170721     C                   if        SoggI52    <> *Blank
508900170721     C                   CAT       'and':1       SQLCST
509000170721     C                   CAT       'CSTSogg':1   SQLCST
509100170721     C                   CAT       '=''':0       SQLCST
509200170721     C                   CAT       SoggI52:0     SQLCST
509300170721     C                   CAT       '''':0        SQLCST
509400170721     C                   endif
509500170721     C*
509600170721     C* and CSTKcc = Kcc1I52
509700170721     C                   if        Kcc1I52    <> *Blank
509800170721     C                   CAT       'and':1       SQLCST
509900170721     C                   CAT       'CSTKcc':1    SQLCST
510000170721     C                   CAT       '=''':0       SQLCST
510100170721     C                   CAT       Kcc1I52:0     SQLCST
510200170721     C                   CAT       '''':0        SQLCST
510300170721     C                   endif
510400170721     C*
510500170721     C* and CSTKcc = Kcc2I52
510600170721     C                   if        Kcc2I52    <> *Blank
510700170721     C                   CAT       'and':1       SQLCST
510800170721     C                   CAT       'CSTKcc':1    SQLCST
510900170721     C                   CAT       '=''':0       SQLCST
511000170721     C                   CAT       Kcc2I52:0     SQLCST
511100170721     C                   CAT       '''':0        SQLCST
511200170721     C                   endif
511300170721     C*
511400170721     C* and CSTKcc = Kcc3I52
511500170721     C                   if        Kcc3I52    <> *Blank
511600170721     C                   CAT       'and':1       SQLCST
511700170721     C                   CAT       'CSTKcc':1    SQLCST
511800170721     C                   CAT       '=''':0       SQLCST
511900170721     C                   CAT       Kcc3I52:0     SQLCST
512000170721     C                   CAT       '''':0        SQLCST
512100170721     C                   endif
512200170721     C*
512300170721     C* and CSTKcc = Kcc4I52
512400170721     C                   if        Kcc4I52    <> *Blank
512500170721     C                   CAT       'and':1       SQLCST
512600170721     C                   CAT       'CSTKcc':1    SQLCST
512700170721     C                   CAT       '=''':0       SQLCST
512800170721     C                   CAT       Kcc4I52:0     SQLCST
512900170721     C                   CAT       '''':0        SQLCST
513000170721     C                   endif
513100170721     C*
513200170721     C* and CSTKcc = Kcc5I52
513300170721     C                   if        Kcc5I52    <> *Blank
513400170721     C                   CAT       'and':1       SQLCST
513500170721     C                   CAT       'CSTKcc':1    SQLCST
513600170721     C                   CAT       '=''':0       SQLCST
513700170721     C                   CAT       Kcc5I52:0     SQLCST
513800170721     C                   CAT       '''':0        SQLCST
513900170721     C                   endif
514000170721     C*
514100170721     C* and CSTKcc = Kcc6I52
514200170721     C                   if        Kcc6I52    <> *Blank
514300170721     C                   CAT       'and':1       SQLCST
514400170721     C                   CAT       'CSTKcc':1    SQLCST
514500170721     C                   CAT       '=''':0       SQLCST
514600170721     C                   CAT       Kcc6I52:0     SQLCST
514700170721     C                   CAT       '''':0        SQLCST
514800170721     C                   endif
514900170721     C*
515000170721     C* and CSTKsc = KscI52
515100170721     C                   if        KscI52     <> *Blank
515200170721     C                   CAT       'and':1       SQLCST
515300170721     C                   CAT       'CSTKsc':1    SQLCST
515400170721     C                   CAT       '=''':0       SQLCST
515500170721     C                   CAT       KscI52:0      SQLCST
515600170721     C                   CAT       '''':0        SQLCST
515700170721     C                   endif
515800170811PP   C*
515900170816fm6  C                   if        TpRecI52    = 'O'
516000170816PP   C* and CSTIdFileO =  blanks
516100170811"    C                   CAT       'and':1       SQLCST
516200170811"    C                   CAT       'CSTIdFileO':1SQLCST
516300170811"    C                   CAT       '=''':0       SQLCST
516400170811PP   C                   CAT       ' ''' :0      SQLCST
516500170816fm6  C* and CSTIdFileR =  blanks
516600170816     C                   CAT       'and':1       SQLCST
516700170816 "   C                   CAT       'CSTIdFileR':1SQLCST
516800170816 "   C                   CAT       '=''':0       SQLCST
516900170816 "   C                   CAT       ' ''' :0      SQLCST
517000170816fm6  C                   endif
517100170814fm4  C*
517200170816fm6  C                   if        TpRecI52    = 'R'
517300170816fm4  C* and CSTIdFileR =  blanks
517400170816 "   C                   CAT       'and':1       SQLCST
517500170816 "   C                   CAT       'CSTIdFileR':1SQLCST
517600170816 "   C                   CAT       '=''':0       SQLCST
517700170814fm4  C                   CAT       ' ''' :0      SQLCST
517800170816fm6  C                   endif
517900170814fm4  C*
518000170814"    C* and CSTIdFileA =  blanks
518100170814"    C                   CAT       'and':1       SQLCST
518200170814"    C                   CAT       'CSTIdFileA':1SQLCST
518300170814"    C                   CAT       '=''':0       SQLCST
518400170814fm4  C                   CAT       ' ''' :0      SQLCST
518500000000     C*
518600000000     C* cancello solo gli automatici se è richiesto di
518700000000     C* non cancellare i manuali
518800170615     C                   if        DelManuI52 <> *On
518900170529     C* CSTOrigine = 'A'
519000170529     C                   CAT       'and':1       SQLCST
519100170529     C                   CAT       'CSTOrigine':1SQLCST
519200170529     C                   CAT       '=''':0       SQLCST
519300170529     C                   CAT       'A''':0       SQLCST
519400000000     C                   endif
519500170703     C*
519600000000     C                   ENDSR
519700000000     C/EJECT
519800000000     C******************************************************
519900170529     C* ESECUZIONE SQL per SDGCST
520000000000     C******************************************************
520100170529     C     EXESQLCST     BEGSR
520200000000     C*
520300000000     C/EXEC SQL
520400170529     C+     EXECUTE IMMEDIATE :SQLCST
520500000000     C/END-EXEC
520600000000     C*
520700000000     C                   if        SQLCOD      < 0
520800000000     C                   CALL      'X66CHGJOB'
520900000000     C*
521000170529     C                   Eval      StringaSql  = SQLCST
521100000000     C                   Eval      $TpErr1     = '99'
521200000000     C                   EXSR      StpErr1
521300170615     C                   Eval      ERRI52      = '1'
521400000000     C                   endif
521500000000     C*
521600000000     C                   ENDSR
521700000000     C/EJECT
521800000000     C********************************************************************
521900000000     C* Close Sql fetch
522000000000     C********************************************************************
522100000000     C     CloseSql      BEGSR
522200000000     C*
522300000000     C/EXEC SQL
522400000000     C+     Close A1
522500000000     C/END-EXEC
522501170831R2654C*
522502170831 "   C/EXEC SQL
522503170831 "   C+     Close A2
522504170831R2654C/END-EXEC
522600000000     C*
522700000000     C                   ENDSR
522800000000     C/EJECT
522900000000     C************************************************************
523000000000     C* OPERAZIONI INIZIALI
523100000000     C************************************************************
523200000000     C     *INZSR        BEGSR
523300000000     C*
523400000000     C     *ENTRY        PLIST
523500000000     C                   PARM                    KPJBA
523600000000     C*
523700000000     C     *DMY          Move      UDate         UDateISO
523800000000     C*
523900170615     C                   Eval      SDGSI52DS   = KPJBU
524000000000     C*
524100110720     C                   EXSR      REPSOC
524200110720     C*
524300110720     C* Se non è già aperto, apro il printer file e faccio OVR
524400110720     C                   EXSR      OpnPrtf
524500170714     C*
524600170728     C                   EXSR      RepFisc_Soc
524700000000     C*
524800170616     C                   EXSR      RepSDSPETRI
524900000000     C*
525000000000     C*-------------------
525100000000     C* valorizzo i dati di testata comuni a tutte le stampe
525200000000     C                   MoveL     KNSIF         NOMSIF
525300000000     C                   MoveL     XSCDSI        NOMDIT
525400000000     C                   MoveL     DSPGM         NOMPGM
525500000000     C                   CALL      'XNETA'                              29
525600000000     C                   PARM                    NOMSYS
525700170721     C*
525800170721     C* Repperisco il sottotitolo di testata
525900170721     C                   if        SottitTT   <> *Blank
526000170721     C                   EXSR      RepSottTitT
526100170721     C                   endif
526200000000     C*
526300000000     C                   EXSR      RieParam
526400000000     C*
526500000000     C                   EXSR      StpErr1_Inz
526600000000     C*
526700000000     C                   ENDSR
526800000000     C/EJECT
526900110720     C************************************************************
527000110720     C* Apertura file si stampa
527100110720     C************************************************************
527200110720     C     OpnPrtf       BEGSR
527300110720     C*
527400110720     C* imposto il nome del pgm da mettere in stampa
527500170707     C                   MOVEL     'SDG00700R'   NOMPGM
527600110720     C*-------------------
527700110720     C* valorizzo i dati di testata comuni a tutte le stampe
527800110720     C                   MoveL     KNSIF         NOMSIF
527900110720     C                   MoveL     XSCDSI        NOMDIT
528000110720     C**************     MoveL     DSPGM         NOMPGM
528100110720     C                   CALL      'XNETA'                              29
528200110720     C                   PARM                    NOMSYS
528300110720     C*
528400110720     C                   EXSR      RieParam
528500110720     C*
528600170615     C                   if        NOT %open(SDGSI52P1)
528700110721     C*
528800110721     C* salvo la kpjbu per ripristinarla dopo il richiamo allo u.e.
528900110721     C                   MoveL     KPJBU         SavKPJBU
529000110720     C*
529100110720     C* eseguo ovrprtf in *Share
529200170707     C                   Clear                   SDG00700DS
529300170707     C                   Eval      KPJBU       = SDG00700DS
529400110720     C*
529500170707     C                   CallB     'SDG00700R'
529600110720     C                   PARM                    KPJBA
529700110720     C*
529800170707     C                   MoveL     KPJBU         SDG00700DS
529900110720     C*
530000170707     C                   if        Esito0700   = *On
530100110720     C*
530200110720     C* Fallito l'ovrprtf del file di spool
530300110720     C* apro il prtf per fare la stampa dell'errore
530400170615     C                   OPEN      SDGSI52P1
530500110720     C*
530600110720     C                   else
530700170615     C                   OPEN      SDGSI52P1
530800110720     C                   endif
530900110721     C*
531000110721     C* ripristino la kpjbu per non "turbare" i richiami successivi
531100110721     C* ad eventuali altri pgm che ricevono la kpjba
531200110721     C                   MoveL     SavKPJBU      KPJBU
531300110720     C*
531400110720     C                   endif
531500110720     C*
531600110720     C                   EndSr
531700110720     C/EJECT
531800000000     C************************************************************
531900000000     C* Reperimento dati società
532000000000     C************************************************************
532100000000     C     REPSOC        BEGSR
532200000000     C*
532300000000     C                   CALL      'SDGSOCDR'
532400000000     C                   PARM      'SOC001'      TIPXSC            6
532500170615     C                   PARM      SocietaI52    SOCXSC            3
532600000000     C                   PARM                    CDSXSC            9 0
532700000000     C                   PARM                    MODXSC            3
532800000000     C                   PARM      *BLANKS       RTNXSC            1
532900000000     C                   PARM                    XSOCDS
533000000000     C                   PARM                    KPJBA
533100000000     C*
533200000000     C                   if        RtnXsc     <> '1'
533300000000     C                   MoveL     XSOCDS        SOC001
533400000000     C                   else
533500000000     C                   Eval      $TpErr1     = '02'
533600170615     C                   Eval      ERRI52      = '1'
533700000000     C                   EXSR      ENDPGM
533800000000     C                   endif
533900000000     C*
534000000000     C                   ENDSR
534100000000     C/EJECT
534200000000     C************************************************************
534300170714     C* Reperisco i dati anagrafici della società
534400000000     C************************************************************
534500170728     C     RepFisc_Soc   BEGSR
534600000000     C*
534700170714     C                   Clear                   $PartIVASoc
534800170714     C                   Clear                   $CdFiscSoc
534900170731     C                   Clear                   $Stato1Soc
535000170731     C                   Clear                   $IdPaes1Soc
535100170714     C                   Clear                   $PIVA1Soc
535200170714     C                   Clear                   $CdFisc1Soc
535300170714     C                   Clear                   $CogRag1Soc
535400170727     C                   Clear                   $TpN1Soc
535500170727     C                   Clear                   $Denom1Soc
535600170727     C                   Clear                   $Cognom1Soc
535700170727     C                   Clear                   $Nome1Soc
535800170714     C                   Clear                   $Indir2Soc
535900170714     C                   Clear                   $NrCiv2Soc
536000170714     C                   Clear                   $CAP2Soc
536100170714     C                   Clear                   $Locali2Soc
536200170714     C                   Clear                   $Prov2Soc
536300170714     C                   Clear                   $Stato2Soc
536400170714     C                   Clear                   $IdPaes2Soc
536500170731     C                   Clear                   $Indir3Soc
536600170731     C                   Clear                   $NrCiv3Soc
536700170731     C                   Clear                   $CAP3Soc
536800170731     C                   Clear                   $Locali3Soc
536900170731     C                   Clear                   $Prov3Soc
537000170731     C                   Clear                   $Stato3Soc
537100170731     C                   Clear                   $IdPaes3Soc
537200170731     C                   Clear                   $Stato4Soc
537300170731     C                   Clear                   $IdPaes4Soc
537400170731     C                   Clear                   $PIVA4Soc
537500170731     C                   Clear                   $CogRag4Soc
537600170731     C                   Clear                   $TpN4Soc
537700170731     C                   Clear                   $Denom4Soc
537800170731     C                   Clear                   $Cognom4Soc
537900170731     C                   Clear                   $Nome4Soc
538000170714     C*
538100170714     C                   Clear                   $NazSoc
538200170714     C*
538300170728     C                   Clear                   SDG00701DS
538400170728     C*
538500170728     C                   Eval      Opz0701     = '03'
538600170731     C                   Eval      Err0701     = '00'
538700170728     C                   Eval      Soc0701     = SocietaI52
538800170728     C                   Eval      TpCom0701   = TpComI52
538900170728     C                   Eval      Anno0701    = AnnoI52
539000170728     C                   Eval      Trim0701    = TrimI52
539100170728     C                   Eval      Mese0701    = 0
539200170731     C                   Eval      Sogg0701    = *Blank
539300170728     C                   Eval      CF0701      = *Blank
539400170728     C                   Eval      Tecnic0701  = *Off
539500170728     C                   Eval      TpN1Sg0701  = *Blank
539600170728     C                   Eval      DtRif0701   = *Loval
539700170728     C                   Eval      NrAsRe0701  = 0
539800170728     C*
539900170728     C* salvo la kpjbu per ripristinarla dopo il richiamo
540000170728     C                   MoveL     KPJBU         SavKPJBU
540100170728     C*
540200170728     C                   CALLB     'SDG00701R'
540300170728     C                   PARM                    KPJBA
540400170728     C                   PARM                    SDG00701DS
540500170728     C*
540600170728     C* ripristino KPJBU
540700170728     C                   MoveL     SavKPJBU      KPJBU
540800170728     C*
540900170728     C* se errore, esco
541000170731     C                   if        ERR0701    <> '00'
541100170731     C                   Eval      $TpErr1     = Err0701
541200170728     C                   EXSR      StpErr1
541300170728     C                   Eval      ERRI52      = '1'
541400170728     C                   EXSR      ENDPGM
541500170728     C                   endif
541600170728     C*
541700170728     C                   MoveL(P)  PartIV0701    $PartIVASoc
541800170728     C                   MoveL(P)  CdFisc0701    $CdFiscSoc
541900170731     C                   MoveL(P)  Stato10701    $Stato1Soc
542000170731     C                   MoveL(P)  IdPae10701    $IdPaes1Soc
542100170728     C                   MoveL(P)  PIVA10701     $PIVA1Soc
542200170728     C                   MoveL(P)  CdFisc0701    $CdFisc1Soc
542300170728     C*
542400170728     C                   MoveL(P)  CogRa10701    $CogRag1Soc
542500170728     C                   MoveL(P)  TpN10701      $TpN1Soc
542600170728     C                   MoveL(P)  Denom10701    $Denom1Soc
542700170728     C                   MoveL(P)  Cogno10701    $Cognom1Soc
542800170728     C                   MoveL(P)  Nome10701     $Nome1Soc
542900170728     C*
543000170728     C                   MoveL(P)  Indir20701    $Indir2Soc
543100170728     C                   MoveL(P)  NrCiv20701    $NrCiv2Soc
543200170728     C                   MoveL(P)  CAP20701      $CAP2Soc
543300170728     C                   MoveL(P)  Local20701    $Locali2Soc
543400170728     C                   MoveL(P)  Prov20701     $Prov2Soc
543500170728     C                   MoveL(P)  Stato20701    $Stato2Soc
543600170728     C                   MoveL(P)  IdPae20701    $IdPaes2Soc
543700170731     C*
543800170731     C* STABILE ORGANIZZAZIONE
543900170731     C                   MoveL(P)  Indir30701    $Indir3Soc
544000170731     C                   MoveL(P)  NrCiv30701    $NrCiv3Soc
544100170731     C                   MoveL(P)  CAP30701      $CAP3Soc
544200170731     C                   MoveL(P)  Local30701    $Locali3Soc
544300170731     C                   MoveL(P)  Prov30701     $Prov3Soc
544400170731     C                   MoveL(P)  Stato30701    $Stato3Soc
544500170731     C                   MoveL(P)  IdPae30701    $IdPaes3Soc
544600170731     C*
544700170731     C* RAPPRESENTANTE FISCALE
544800170731     C                   MoveL(P)  Stato40701    $Stato4Soc
544900170731     C                   MoveL(P)  IdPae40701    $IdPaes4Soc
545000170731     C                   MoveL(P)  PIVA40701     $PIVA4Soc
545100170731     C                   MoveL(P)  CogRa40701    $CogRag4Soc
545200170731     C                   MoveL(P)  TpN40701      $TpN4Soc
545300170731     C                   MoveL(P)  Denom40701    $Denom4Soc
545400170731     C                   MoveL(P)  Cogno40701    $Cognom4Soc
545500170731     C                   MoveL(P)  Nome40701     $Nome4Soc
545600170731     C*
545700170731     C* -----
545800170731     C* reperisco lo Stato G12 e il codice nazione CEE W06
545900170731     C* dall'IdPaese della Partita IVA
546000170731     C* serve per verificare se la fattura è operazione INTRA o meno
546100170731     C                   Eval      WIdPaese    = $IdPaes1Soc
546200170731     C                   EXSR      RepNazG12
546300170810     C                   if        FoundHD4    = *On
546400170810     C                   Eval      $Stato1Soc  = SavStatoHD4
546500170731     C                   EXSR      RepNazSoc
546600170731     C                   Eval      $NazSoc     = NazG12
546700170731     C                   endif
546800130918     C*
546900000000     C                   ENDSR
547000000000      /EJECT
547100170703     C************************************************************
547200170703     C* Reperisco dati da G12 della società
547300170703     C************************************************************
547400170714     C     RepNazSoc     BEGSR
547500170703     C*
547600170714     C                   Eval      WStato      = $Stato2Soc
547700170703     C                   EXSR      RepTabG12
547800170703     C*
547900170703     C                   if        XTBErr     <> '0'
548000170703     C                   Eval      $TpErr1     = '01'
548100170703     C                   EXSR      StpErr1
548200170703     C                   Eval      ERRI52      = '1'
548300170703     C                   EXSR      ENDPGM
548400170703     C                   endif
548500170703     C*
548600170703     C                   ENDSR
548700170703      /EJECT
548800000000     C************************************************************
548900170616     C* Reperimento parametri di setup per Spesometro Trimestrale
549000000000     C************************************************************
549100170616     C     RepSDSPETRI   BEGSR
549200000000     C*
549300170616     C                   Clear                   SDGPA12DS
549400000000     C*
549500000000     C                   CALLB     'XPAR'
549600000000     C                   Parm                    XScSoc
549700170616     C                   Parm      'SDSPETRI    'XPaPar            8
549800000000     C                   Parm                    XPaOut         2000
549900000000     C                   Parm      *OFF          XPaErr            1
550000000000     C                   Parm      ' '           XPaRic            1
550100000000     C*
550200000000     C                   if        XPaErr      = *On
550300000000     C                   Eval      $TpErr1     = '03'
550400000000     C                   EXSR      StpErr1
550500170615     C                   Eval      ERRI52      = '1'
550600000000     C                   EXSR      ENDPGM
550700000000     C*
550800000000     C                   else
550900170616     C                   MoveL     XPaOut        SDGPA12DS
551000000000     C*
551100170616     C                   if        AttCSTA12   <> *On
551200000000     C                   Eval      $TpErr1     = '04'
551300000000     C                   EXSR      StpErr1
551400170615     C                   Eval      ERRI52      = '1'
551500000000     C                   EXSR      ENDPGM
551600000000     C*
551700000000     C                   endif
551800000000     C*
551900000000     C                   endif
552000000000     C*
552100000000     C                   ENDSR
552200000000     C/EJECT
552300170801     C************************************************************
552400170801     C* Reperimento Sottotitolo di testata
552500170801     C************************************************************
552600170801     C     RepSottTitT   BEGSR
552700170801     C*
552800151202     C                   Select
552900170615     C                   When      TpcomI52 = 'T'
553000170615     C                   Eval      IDMSG    = SOTTITTT
553100151202     C                   EndSl
553200151202     C*
553300151202     C                   CALLB     'XRTVM'
553400151202     C                   PARM                    IDMSG            27
553500151202     C                   PARM                    TXTMSG          100
553600151202     C                   PARM                    ERRMSG            1
553700160208     C                   PARM      *Off          CTRMSG            1            centratura
553800151202     C                   PARM      30            LENMSG            3 0          lun output.
553900151202     C     ERRMSG        IFNE      *ON
554000151202     C                   MOVEL     TXTMSG        SOTTITT
554100151202     C                   ELSE
554200151202     C                   MOVEL     *ALL'?'       SOTTITT
554300151202     C                   ENDIF
554400170801     C                   ENDSR
554500170801     C/EJECT
554600000000     C************************************************************
554700000000     C* Imposto i parametri di elaborazione
554800000000     C************************************************************
554900000000     C     RieParam      BEGSR
555000000000     C*
555100170615     C                   Eval      PACtb       = CtbI52
555200000000     C                   EXSR      RepCtbD
555300000000     C*
555400170615     C                   Eval      PATpRegZ    = TpRegzI52
555500000000     C                   EXSR      RepTpRegzD
555600000000     C*
555700170615     C                   Eval      PAAnno      = AnnoI52
555800170721     C                   Eval      PATrim      = TrimI52
555900170721     C*
556000170721     C                   Eval      PATpRec     = TpRecI52
556100170721     C                   EXSR      RepTpRecD
556200170721     C*
556300170721     C                   Eval      PAStato2    = Stato2I52
556400170721     C                   EXSR      RepStato2D
556500000000     C*
556600170615     C                   Eval      PACF        = CFI52
556700000000     C                   EXSR      RepCFD
556800000000     C*
556900170714     C                   Eval      PAPartIVA   = PartIVAI52
557000170714     C                   Eval      PACdFisc    = CdFiscI52
557100000000     C*
557200170615     C                   Eval      PASogg      = SoggI52
557300000000     C                   EXSR      RepSoggD
557400000000     C*
557500170615     C                   Eval      PAKcc1      = Kcc1I52
557600170615     C                   Eval      PAKcc2      = Kcc2I52
557700170615     C                   Eval      PAKcc3      = Kcc3I52
557800170615     C                   Eval      PAKcc4      = Kcc4I52
557900170615     C                   Eval      PAKcc5      = Kcc5I52
558000170615     C                   Eval      PAKcc6      = Kcc6I52
558100000000     C*
558200170615     C                   Eval      PAKsc       = KscI52
558300000000     C                   EXSR      RepKscD
558400000000     C*
558500000000     C                   Eval      PAOn1       = XScOn
558600000000     C                   Eval      PAOff1      = XScOff
558700170615     C                   if        DelManuI52  = *On
558800000000     C                   Eval      PADelManu   = XScOn
558900000000     C                   else
559000000000     C                   Eval      PADelManu   = XScOff
559100000000     C                   endif
559200110720     C*
559300110720     C                   Eval      PAOn2       = XScOn
559400110720     C                   Eval      PAOff2      = XScOff
559500170615     C                   if        StpAnnI52   = *On
559600110720     C                   Eval      PAStpAnn    = XScOn
559700110720     C                   else
559800110720     C                   Eval      PAStpAnn    = XScOff
559900110720     C                   endif
560000000000     C*
560100000000     C                   ENDSR
560200000000     C/EJECT
560300000000     C************************************************************
560400000000     C* Reperisco la descrizione della contabilità
560500000000     C************************************************************
560600000000     C     RepCtbD       BEGSR
560700000000     C*
560800000000     C                   Clear                   SDGTABDS
560900000000     C*
561000000000     C                   Eval      XTbRic     =  '1'
561100000000     C                   Eval      XTbAzi     =  XScSoc
561200000000     C                   Eval      XTbKey     =  *Zero
561300000000     C                   Eval      XTbCod     =  'G84'
561400170615     C                   Move      CtbI52        XTbKey
561500000000     C*
561600000000     C                   CALL      'SDGTABDR'
561700000000     C                   PARM                    SDGTABDS
561800000000     C*
561900000000     C                   if        XTbErr      = '0'
562000000000     C                   MoveL     XTbUni        ANGG84DS
562100000000     C                   MoveL     DsBG84        PACtbD
562200000000     C                   else
562300000000     C                   MoveL     *All'?'       PACtbD
562400000000     C                   endif
562500000000     C*
562600000000     C                   ENDSR
562700000000     C/EJECT
562800000000     C************************************************************
562900000000     C* Reperisco la descrizione del tipo registrazione
563000000000     C************************************************************
563100000000     C     RepTpRegzD    BEGSR
563200000000     C*
563300000000     C                   Clear                   X07VALDS
563400000000     C*
563500000000     C                   Move      '1'           X07Ric
563600000000     C                   MoveL     'ANDIZ000'    X07Trc
563700000000     C                   MoveL     'DSPTPREGZ '  X07Cam
563800170615     C                   MoveL     TpRegzI52     X07Val
563900000000     C*
564000000000     C                   CALLB     'X07VALR'
564100000000     C                   PARM                    X07VALDS
564200000000     C*
564300000000     C                   if        X07ERR      = '0'
564400000000     C                   MoveL     X07Des        PATpRegzD
564500000000     C                   else
564600000000     C                   MoveL     *All'?'       PATpRegzD
564700000000     C                   endif
564800000000     C*
564900000000     C                   ENDSR
565000000000     C/EJECT
565100170703     C************************************************************
565200170703     C* Reperisco dati tabella G12
565300170703     C************************************************************
565400170703     C     RepTabG12     BEGSR
565500170703     C*
565600170703     C                   Clear                   ANGG12DS
565700170703     C                   Clear                   SDGTABDS
565800170703     C*
565900170703     C                   Eval      XTbRic      = '1'
566000170703     C                   Eval      XTbAzi      = XScSoc
566100170703     C                   Eval      XTbKey      = *Zero
566200170703     C                   Eval      XTbCod      = 'G12'
566300170703     C                   Move      WStato        XTbKey
566400170703     C*
566500170703     C                   CALL      'SDGTABDR'
566600170703     C                   Parm                    SDGTABDS
566700170703     C*
566800170703     C                   if        XTBErr      = '0'
566900170703     C                   Eval      ANGG12DS    = XTbUni
567000170703     C                   endif
567100170703     C*
567200170703     C                   ENDSR
567300170703      /EJECT
567400170721     C************************************************************
567500170721     C* Reperisco la descrizione del tipo di estrazione
567600170721     C************************************************************
567700170721     C     RepTpRecD     BEGSR
567800170721     C*
567900170721     C                   Clear                   X07ValDs
568000170721     C*
568100170721     C                   Move      '1'           X07Ric
568200170721     C                   Move      *On           X07All
568300170721     C                   Movel     'SDGSI52S '   X07TRc
568400170721     C                   Movel     'TPRECI52 '   X07Cam
568500170721     C                   Movel     TpRecI52      X07VAL
568600170721     C*
568700170721     C                   CallB     'X07VALR'
568800170721     C                   Parm                    X07ValDs
568900170721     C*
569000170721     C     X07ERR        IFEQ      *Off
569100170721     C                   MOVEL     X07DES        PATpRecD
569200170721     C                   else
569300170721     C                   MoveL     *All'?'       PATpRecD
569400170721     C                   endif
569500170721     C*
569600170721     C                   ENDSR
569700170721     C/EJECT
569800170721     C************************************************************
569900170721     C* Reperisco la descrizione dello Stato
570000170721     C************************************************************
570100170721     C     RepStato2D    BEGSR
570200151123     C*
570300170707     C                   Move      Stato2I52     WStato
570400170703     C                   EXSR      RepTabG12
570500151123     C*
570600170707     C                   MoveL     DesG12        PAStato2D
570700151123     C*
570800170721     C                   ENDSR
570900170721     C/EJECT
571000000000     C************************************************************
571100000000     C* Reperisco la descrizione della natura del soggetto
571200000000     C************************************************************
571300000000     C     RepCFD        BEGSR
571400000000     C*
571500000000     C                   Clear                   X07VALDS
571600000000     C*
571700170615     C                   if        CFI52      <> *Blank
571800000000     C*
571900000000     C                   Move      '1'           X07Ric
572000000000     C                   MoveL     'ANDIZ000'    X07Trc
572100000000     C                   MoveL     'DSPCLIFOR '  X07Cam
572200170615     C                   MoveL     CFI52         X07Val
572300000000     C                   Move      *On           X07All
572400000000     C                   CALLB     'X07VALR'
572500000000     C                   PARM                    X07VALDS
572600000000     C*
572700000000     C                   if        X07Err      = '0'
572800000000     C                   MoveL     X07Des        PACFD
572900000000     C                   else
573000000000     C                   MoveL     *All'?'       PACFD
573100000000     C                   endif
573200000000     C*
573300000000     C                   endif
573400000000     C*
573500000000     C                   ENDSR
573600000000     C/EJECT
573700000000     C************************************************************
573800000000     C* Reperisco la descrizione del soggetto
573900000000     C************************************************************
574000000000     C     RepSoggD      BEGSR
574100000000     C*
574200000000     C                   Clear                   PASoggD
574300000000     C*
574400170615     C                   if        SoggI52    <> *Blank
574500000000     C*
574600170615     C                   Eval      WSogg       = SoggI52
574700000000     C                   MoveL     'DVASOG    '  DVAStrutt
574800000000     C                   Eval      DVALENOUT   = %size(DvaSog)
574900000000     C*
575000000000     C                   EXSR      CallDvaSog
575100000000     C*
575200000000     C                   if        DVAErrore  <> '1'
575300000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
575400000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
575500000000     C*
575600000000     C                   MoveL     DVSDes        PASoggD
575700000000     C                   else
575800000000     C                   MoveL     *All'?'       PASoggD
575900000000     C                   endif
576000000000     C*
576100000000     C                   endif
576200000000     C*
576300000000     C                   ENDSR
576400000000     C/EJECT
576500000000     C************************************************************
576600000000     C* Reperisco la descrizione del conto
576700000000     C************************************************************
576800000000     C     RepKscD       BEGSR
576900000000     C*
577000000000     C                   Clear                   PAKscD
577100000000     C*
577200170615     C                   if        KccI52     <> *Blank   or
577300170615     C                             KscI52     <> *Blank
577400000000     C*
577500170615     C                   Eval      WKcc        = KccI52
577600170615     C                   Eval      WKsc        = KscI52
577700170801     C                   Eval      WData       = *Loval
577800000000     C*
577900000000     C                   EXSR      CallMVC002
578000000000     C*
578100000000     C                   if        $Esito     <> *On
578200000000     C                   MoveL     DesBrev002    PAKscD
578300000000     C                   else
578400000000     C                   MoveL     *All'?'       PAKscD
578500000000     C                   endif
578600000000     C*
578700000000     C                   endif
578800000000     C*
578900000000     C                   ENDSR
579000000000     C/EJECT
579100000000     C******************************************************
579200170615     C* Preparo dati 'fissi' (SDGSI52P1)     (solo I° volta)
579300000000     C******************************************************
579400000000     C     StpErr1_Inz   BEGSR
579500000000     C*
579600000000     C* Overflow
579700000000     C                   if        *In41       = *On        or
579800000000     C                             $PrimaStp1  = *On
579900000000     C* Testata
580000170615     C                   WRITE     SI52T11
580100000000     C*
580200000000     C                   if        $PrimaStp1  = *On
580300000000     C* Parametri di elaborazione
580400170615     C                   WRITE     SI52PA1
580500000000     C                   endif
580600000000     C*
580700000000     C* Intestazione colonne
580800000000     C                   if        $TpErr1     = *Blank
580900170615     C                   WRITE     SI52T21
581000000000     C                   endif
581100000000     C*
581200000000     C                   Eval      *In41       = *Off
581300000000     C                   Eval      $PrimaStp1  = *Off
581400000000     C                   endif
581500000000     C*
581600000000     C                   ENDSR
581700000000     C/EJECT
581800000000     C************************************************************
581900170615     C* Stampa errori e avvertimenti (SDGSI52P1)
582000000000     C************************************************************
582100000000     C     StpErr1       BEGSR
582200000000     C*
582300000000     C                   EXSR      StpErr1_Inz
582400000000     C*
582500000000     C                   Select
582600000000     C*
582700000000     C* Stampa riga di avvertimento per movimento annullato.
582800000000     C                   When      $TpAnn     <> *Blank
582900110720     C*
583000170615     C                   if        StpAnnI52   = *On
583100000000     C                   Eval      AVVTpAnn    = $TpAnn
583200000000     C                   EXSR      RepTpAnnD
583300170615     C                   WRITE     SI52PAnn
583400110720     C                   EXSR      StpErr1_Dati
583500110720     C                   endif
583600000000     C*
583700000000     C*
583800000000     C*  ERRORE: elaborazione interrotta. Codice Stato società di collegamento
583900000000     C*   non valido. Codice Stato =
584000000000     C                   When      $TpErr1     = '01'
584100000000     C                   Eval      P101Stato   = XScSta
584200170615     C                   WRITE     SI52P101
584300000000     C*
584400000000     C*  ERRORE: elaborazione interrotta. Codice società di collegamento
584500000000     C*   non valido. Codice società =
584600000000     C                   When      $TpErr1     = '02'
584700170615     C                   Eval      P102Soc     = SocietaI52
584800170615     C                   WRITE     SI52P102
584900000000     C*
585000170731     C*  ERRORE: elaborazione interrotta. Parametro modulo SDSPETRI non
585100000000     C*  trovato per la società di collegamento. Codice società =
585200000000     C                   When      $TpErr1     = '03'
585300170615     C                   Eval      P103Soc     = SocietaI52
585400170615     C                   WRITE     SI52P103
585500000000     C*
585600170731     C*  ERRORE: elaborazione interrotta. Parametro modulo SDSPETRI non
585700000000     C*  attivato per la società di collegamento. Codice società =
585800000000     C                   When      $TpErr1     = '04'
585900170615     C                   Eval      P104Soc     = SocietaI52
586000170615     C                   WRITE     SI52P104
586100130918     C*
586200170703     C*  ERRORE: elaborazione interrotta. Manca codice ISO 2 Alpha
586300170731     C*  in tab. HD4 per la società di collegamento. Codice società =
586400130918     C                   When      $TpErr1     = '05'
586500170731     C                   Eval      P105Soc     = SocietaI52
586600170615     C                   WRITE     SI52P105
586700170731     C*
586800170731     C*  ERRORE: elaborazione interrotta. Tipo natura giuridica societào
586900170731     C*  di collegamento non trovato in tab. G16. Codice società =
587000170731     C                   When      $TpErr1     = '06'
587100170731     C                   Eval      P106Soc     = SocietaI52
587200170731     C                   WRITE     SI52P106
587300170731     C*
587400170731     C*  ERRORE: elaborazione interrotta. Tipo natura giuridica societào
587500170731     C*  di collegamento non trovato in tab. G16. Codice società =
587600170731     C                   When      $TpErr1     = '07'
587700170731     C                   Eval      P106Soc     = SocietaI52
587800170731     C                   WRITE     SI52P106
587900000000     C*
588000000000     C*
588100000000     C*  ERRORE: elaborazione interrotta causa errori nella stringa sql.
588200000000     C*  Codice errore SQL 99999 nell'esecuzione della seguente stringa:
588300000000     C                   When      $TpErr1     = '99'
588400000000     C                   Eval      P199SqlCd   = SqlCod
588500000000     C                   Eval      P199SQL1    = %subst(StringaSql:1 :197)
588600000000     C                   Eval      P199SQL2    = %subst(StringaSql:198 :197)
588700000000     C                   Eval      P199SQL3    = %subst(StringaSql:395 :197)
588800000000     C                   Eval      P199SQL4    = %subst(StringaSql:595 :197)
588900000000     C                   Eval      P199SQL5    = %subst(StringaSql:792 :197)
589000170801     C                   Eval      P199SQL6    = %subst(StringaSql:989 :197)
589100170801     C                   Eval      P199SQL7    = %subst(StringaSql:1186 :197)
589200170801     C                   Eval      P199SQL8    = %subst(StringaSql:1383 :197)
589300170801     C                   Eval      P199SQL9    = %subst(StringaSql:1580 :197)
589400170801     C                   Eval      P199SQL10   = %subst(StringaSql:1777 :197)
589500170801     C                   Eval      P199SQL11   = %subst(StringaSql:1974 :197)
589600170801     C                   Eval      P199SQL12   = %subst(StringaSql:2171 :197)
589700170801     C                   Eval      P199SQL13   = %subst(StringaSql:2368 :197)
589800170801     C                   Eval      P199SQL14   = %subst(StringaSql:2565 :197)
589900170801     C                   Eval      P199SQL15   = %subst(StringaSql:2762 :197)
590000170615     C                   WRITE     SI52P199
590100000000     C*
590200000000     C                   OTHER
590300000000     C*  ERR . Avvertimento/errore non codificato.
590400170615     C                   WRITE     SI52P1XX
590500000000     C*
590600000000     C                   endSl
590700000000     C*
590800000000     C                   ENDSR
590900000000     C/EJECT
591000000000     C******************************************************
591100000000     C* Stampo i riferimenti del movimento
591200000000     C******************************************************
591300000000     C     StpErr1_Dati  BEGSR
591400000000     C*
591500170724     C* ripristino dati del movimento cliente/fornitore
591600170724     C                   Move      NDMOVSav      NDMOV00F
591700170724     C*
591800170615     C                   Clear                   SI52DA1
591900000000     C*
592000000000     C                   MoveL     MOVKcc        DA1Kcc
592100000000     C                   MoveL     MOVKsc        DA1Ksc
592200000000     C*
592300000000     C                   Eval      WKcc        = MOVKcc
592400000000     C                   Eval      WKsc        = MOVKsc
592500170724     C                   Eval      WData       = MOVDtDoc
592600000000     C                   EXSR      CallMVC002
592700000000     C                   MoveL     Sogg002       DA1Sogg
592800000000     C*
592900000000     C                   if        MOVDtDoc   <> *Loval
593000000000     C     *JobRun       Move      MOVDtDoc      DA1DtDoc
593100000000     C                   endif
593200000000     C                   Z-Add     MOVNrDoc      DA1NrDoc
593300000000     C                   MoveL     MOVSerDoc     DA1SerDoc
593400000000     C*
593500000000     C                   MoveL     MOVCausTes    DA1Causale
593600000000     C*
593700000000     C                   if        IVAAliq     > 0
593800000000     C                   SetOn                                        13
593900000000     C                   Z-Add     IVAAliq       DA1Aliq
594000000000     C                   else
594100000000     C                   SetOff                                       13
594200000000     C                   Move      IVARifIva     DA1RifIVA
594300000000     C                   endif
594400000000     C*
594500130826     C                   Z-Add     $ImpDoc       DA1ImpDoc
594600000000     C*
594700000000     C                   Z-Add     MOVSys        DA1Sys
594800000000     C                   Z-Add     MOVNrAsReg    DA1NrAsReg
594900000000     C*
595000170615     C                   WRITE     SI52DA1
595100000000     C*
595200000000     C                   ENDSR
595300000000     C/EJECT
595400000000     C************************************************************
595500000000     C* Reperisco la descrizione del tipo annullamento
595600000000     C************************************************************
595700000000     C     RepTpAnnD     BEGSR
595800000000     C*
595900000000     C                   Clear                   X07VALDS
596000000000     C*
596100000000     C                   Move      '1'           X07Ric
596200170529     C                   MoveL     'SDGCST000 '  X07Trc
596300170529     C                   MoveL     'CSTTPANN  '  X07Cam
596400000000     C                   MoveL     $TpAnn        X07Val
596500000000     C*
596600000000     C                   CALLB     'X07VALR'
596700000000     C                   PARM                    X07VALDS
596800000000     C*
596900000000     C                   if        X07ERR      = '0'
597000000000     C                   MoveL     X07Des        AVVTpAnnD
597100000000     C                   else
597200000000     C                   MoveL     *All'?'       AVVTpAnnD
597300000000     C                   endif
597400000000     C*
597500000000     C                   ENDSR
597600000000     C/EJECT
597700000000     C******************************************************
597800170615     C* Preparo dati 'fissi' stampa (SDGSI52P2) (solo I° volta)
597900000000     C******************************************************
598000000000     C     StpAutof_Inz  BEGSR
598100000000     C*
598200000000     C* Overflow
598300000000     C                   if        *In42       = *On        or
598400000000     C                             $PrimaStp2  = *On
598500000000     C* Testata
598600170615     C                   WRITE     SI52T12
598700000000     C*
598800000000     C                   if        $PrimaStp2  = *On
598900000000     C* Parametri di elaborazione
599000170615     C                   WRITE     SI52PA2
599100000000     C                   endif
599200000000     C*
599300000000     C* Intestazione colonne
599400170615     C                   WRITE     SI52T22
599500000000     C*
599600000000     C                   Eval      *In42       = *Off
599700000000     C                   Eval      $PrimaStp2  = *Off
599800000000     C                   endif
599900000000     C*
600000000000     C                   ENDSR
600100000000     C/EJECT
600200000000     C********************************************************************
600300000000     C* Stampa autofatture scartate
600400000000     C********************************************************************
600500000000     C     StpAutof      BEGSR
600600000000     C*
600700000000     C                   if        MOVSys     <> SAVSys       or
600800000000     C                             MOVNrAsReg <> SAVNrAsReg
600900000000     C*
601000000000     C                   Z-Add     MOVSys        SAVSys
601100000000     C                   Z-Add     MOVNrAsReg    SAVNrAsReg
601200000000     C*
601300000000     C*
601400000000     C                   EXSR      StpAutof_Inz
601500000000     C*
601600000000     C*
601700000000     C                   MoveL     MOVKcc        P201Kcc
601800000000     C                   MoveL     MOVKsc        P201Ksc
601900000000     C                   MoveL     MOVCausTes    P201CausT
602000000000     C*
602100000000     C                   if        IVADtReg   <> *Loval
602200000000     C     *JobRun       Move      IVADtReg      P201DtReg
602300000000     C                   endif
602400000000     C                   Z-Add     IVANrReg      P201NrReg
602500000000     C                   MoveL     IVASerReg     P201SerReg
602600000000     C*
602700000000     C                   if        MOVDtDoc   <> *Loval
602800000000     C     *JobRun       Move      MOVDtDoc      P201DtDoc
602900000000     C                   endif
603000000000     C                   Z-Add     MOVNrDoc      P201NrDoc
603100000000     C                   MoveL     MOVSerDoc     P201SerDoc
603200000000     C*
603300000000     C                   if        MOVDare     = 1
603400000000     C                   Z-Sub     MOVImporto    P201ImpDoc
603500000000     C                   else
603600000000     C                   Z-Add     MOVImporto    P201ImpDoc
603700000000     C                   endif
603800000000     C*
603900000000     C                   Z-Add     MOVSys        P201Sys
604000000000     C                   Z-Add     MOVNrAsReg    P201NrAsRe
604100000000     C*
604200170615     C                   WRITE     SI52P201
604300000000     C*
604400000000     C* aggiorno i totali
604500000000     C                   if        MOVDare     = 1
604600000000     C                   Eval      $Segno      = +1
604700000000     C                   else
604800000000     C                   Eval      $Segno      = -1
604900000000     C                   endif
605000000000     C*
605100000000     C                   Eval      P299ImpDoc  =
605200000000     C                               P299ImpDoc + (MOVImporto * $Segno)
605300000000     C*
605400000000     C                   endif
605500000000     C*
605600000000     C                   ENDSR
605700000000     C/EJECT
605800000000     C************************************************************
605900170615     C* Stampa totale autofatture   (SDGSI52P2)
606000000000     C************************************************************
606100000000     C     StpAutof_Tot  BEGSR
606200000000     C*
606300000000     C* solo se stampata almeno una riga
606400000000     C                   if        $PrimaStp2  = *Off
606500170615     C                   WRITE     SI52P299
606600000000     C                   endif
606700000000     C*
606800000000     C                   ENDSR
606900000000     C/EJECT
607000000000     C******************************************************
607100170615     C* Preparo dati 'fissi' stampa (SDGSI52P3) (solo I° volta)
607200000000     C******************************************************
607300000000     C     StpNoFisc_Inz BEGSR
607400000000     C*
607500000000     C* Overflow
607600000000     C                   if        *In43       = *On        or
607700000000     C                             $PrimaStp3  = *On
607800000000     C* Testata
607900170615     C                   WRITE     SI52T13
608000000000     C*
608100000000     C                   if        $PrimaStp3  = *On
608200000000     C* Parametri di elaborazione
608300170615     C                   WRITE     SI52PA3
608400000000     C                   endif
608500000000     C*
608600000000     C* Intestazione colonne
608700170615     C                   WRITE     SI52T23
608800000000     C*
608900000000     C                   Eval      *In43       = *Off
609000000000     C                   Eval      $PrimaStp3  = *Off
609100000000     C                   endif
609200000000     C*
609300000000     C                   ENDSR
609400000000     C/EJECT
609500000000     C********************************************************************
609600170615     C* Stampa dati fiscali non valorizzati (SDGSI52P3)
609700000000     C********************************************************************
609800000000     C     StpNoFisc     BEGSR
609900170724     C*
610000170724     C* ripristino il movimento cliente/fornitore
610100170724     C                   Move      NDMOVSav      NDMOV00F
610200000000     C*
610300000000     C                   EXSR      StpNoFisc_Inz
610400000000     C*
610500170615     C                   Clear                   SI52P301
610600000000     C*
610700000000     C                   Eval      WKcc        = MOVKcc
610800000000     C                   Eval      WKsc        = MOVKsc
610900170724     C                   Eval      WData       = MOVDtDoc
611000000000     C                   EXSR      CallMVC002
611100000000     C*
611200000000     C                   MoveL     SNatura002    P301CF
611300000000     C                   MoveL     MOVKcc        P301Kcc
611400000000     C                   MoveL     MOVKsc        P301Ksc
611500000000     C                   MoveL     Sogg002       P301Sogg
611600000000     C*
611700000000     C                   if        Tecnico002 <>'2'
611800000000     C                   Move      Descr002      P301Descr
611900000000     C*
612000000000     C                   if        MOVDtDoc <> *loval
612100000000     C     *JobRun       Move      MOVDtDoc      P301DtDoc
612200000000     C                   endif
612300000000     C*
612400000000     C                   else
612500000000     C*
612600170728     C* nel caso di conto tecnico, non sarà mai la dogana,
612700170728     C* e quindi la ragione sociale recuperata sarà sempre
612800170728     C* quella di NDMOV00F(NDCPA00F)
612900170731     C                   Move      $CogRag1Mov   P301Descr
613000000000     C                   Move      'X'           P301Tecnic
613100000000     C*
613200000000     C                   SetOn                                        12
613300000000     C*
613400000000     C                   if        MOVDtDoc   <> *Loval
613500000000     C     *JobRun       Move      MOVDtDoc      P301DtDoc
613600000000     C                   endif
613700000000     C*
613800000000     C                   if        MOVDtReg   <> *Loval
613900000000     C     *JobRun       Move      MOVDtReg      P301DtReg
614000000000     C                   endif
614100000000     C*
614200000000     C                   Z-Add     MOVSys        P301Sys
614300000000     C                   Z-Add     MOVNrAsReg    P301NrAsRe
614400000000     C*
614500000000     C                   endif
614600000000     C*
614700170731     C                   MoveL     $PartIVAMov   P301PartIV
614800170731     C                   MoveL     $CdFiscMov    P301CdFisc
614900170731     C                   MoveL     $Stato2Mov    P301Stato2
615000000000     C*
615100170615     C                   WRITE     SI52P301
615200000000     C*
615300000000     C                   ENDSR
615400000000     C/EJECT
615500000000     C************************************************************
615600000000     C* DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
615700000000     C************************************************************
615800000000     C     DefCam        BEGSR
615900170724     C*
616000170724     C* SDGCSF03L
616100170724     C     K07CSF03      KLIST
616200170724     C                   KFLD                    CSFSocieta
616300170724     C                   KFLD                    CSFTpCom
616400170724     C                   KFLD                    CSFTpRec
616500170724     C                   KFLD                    CSFAnno
616600170724     C                   KFLD                    CSFTrim
616700170724     C                   KFLD                    CSFMese
616800170724     C                   KFLD                    CSFPrgEstr
616900000000     C*
617000130823     C* SDGSPR04L
617100130823     C     K12SPR04      KLIST
617200130823     C                   KFLD                    SPRSocieta
617300130823     C                   KFLD                    SPRTipReg
617400130823     C                   KFLD                    SPRStpReg
617500130823     C                   KFLD                    SPRTPREG
617600130823     C                   KFLD                    SPRLIBRO
617700130823     C                   KFLD                    SPRALIQ
617800130823     C                   KFLD                    SPRRIFIVA
617900130823     C                   KFLD                    SPRCAUSALE
618000130823     C                   KFLD                    SPRKCC
618100130823     C                   KFLD                    SPRKSC
618200130823     C                   KFLD                    SPRVOCE
618300130823     C                   KFLD                    SPRCLASSE
618400000000     C*
618500000000     C*  ANDLI01L
618600000000     C     K03DLI01      KLIST
618700000000     C                   KFLD                    DLISocieta
618800000000     C                   KFLD                    DLITpReg
618900000000     C                   KFLD                    DLILibro
619000170811  fm3C*
619100170811  fm3C*  ANAPE01L
619200170811  fm3C     K03APE01      KLIST
619300170811  fm3C                   KFLD                    APESocieta
619400170811  fm3C                   KFLD                    APEKcc
619500170811  fm3C                   KFLD                    APEKsc
619600000000     C*
619700000000     C*  ANRIV01L
619800000000     C     K04RIV01      KLIST
619900000000     C                   KFLD                    RIVSocieta
620000000000     C                   KFLD                    RIVTpReg
620100000000     C                   KFLD                    RIVAliq
620200000000     C                   KFLD                    RIVRifIva
620300000000     C*
620400000000     C*  ANALR01L
620500000000     C     K05ALR01      KLIST
620600000000     C                   KFLD                    ALRSocieta
620700000000     C                   KFLD                    ALRTpReg
620800000000     C                   KFLD                    ALRLibro
620900000000     C                   KFLD                    ALRAliq
621000000000     C                   KFLD                    ALRRifIva
621100000000     C*
621200000000     C*  NDPAS01L
621300000000     C     K03PAS01      KLIST
621400000000     C                   KFLD                    PASSys
621500000000     C                   KFLD                    PASNrAsReg
621600000000     C                   KFLD                    PASNrRigaM
621700000000     C*
621800000000     C*  NDMOA01L
621900000000     C     K03MOA01      KLIST
622000000000     C                   KFLD                    MOASys
622100000000     C                   KFLD                    MOANrAsReg
622200000000     C                   KFLD                    MOANrRigaM
622300170801     C*
622400170801     C     K04MOA01      KLIST
622500170801     C                   KFLD                    MOASys
622600170801     C                   KFLD                    MOANrAsReg
622700170801     C                   KFLD                    MOANrRigaM
622800170801     C                   KFLD                    MOADimen
622900131014     C*
623000131014     C*  NDIVA01L
623100131014     C     K02IVA01      KLIST
623200131014     C                   KFLD                    IVASys
623300131014     C                   KFLD                    IVANrAsReg
623400000000     C*
623500000000     C                   ENDSR
623600000000     C/EJECT
623601180302
623602170830     C************************************************************
623603170830     C* casi particolari BRT
623604170830     C************************************************************
623605180302
623606180302       BEGSR BRT;
623607170830
623608180302         CLEAR cstUteUlMo;
623609180302         CLEAR cstDtUlMo;
623610180302
623611180302         // solo se non annullati per altri motivi
623612180302         if cstTpAnn = '';
623613170906
623614180302           // Partita IVA BRT solo per BRT
623615180302           if XScSoc = '201';
623616180302             if       cstpartIVA = '03987100371' or
623617180302             cstpartIVA = '04507990150' or
623618180302             cstPiva1 = '03987100371' or
623619180302             cstPiva1 = '04507990150' or
623620180302             CSTCDFISC = '03987100371' or
623621180302             CSTCDFISC = '04507990150' or
623622180302             CSTCDFISC1 = '03987100371' or
623623180302             CSTCDFISC1 = '04507990150' ;
623624180302               cstann = *On;
623625180302               cstTpAnn = '99';
623626180302               cstUteUlMo = '*AU';
623627180302               cstDtUlMo = %DATE;
623628180302             endif;
623629180302           endif;
623630170906
623631180302           // San Marino solo clienti solo BRT
623632180302           if XScSoc = '201' and cstcf = 'C' and cstidpaes1 = 'SM' ;
623633180302             cstidpaes2 = 'SM';
623634180302             cststato2 = 'SM';
623635180302             clear cstprov2;
623636180302             cstUteUlMo = '*AU';
623637180302             cstDtUlMo = %DATE;
623638180302           // cstann = *On;
623639180302           // stTpAnn = '94';
623640180302           endif;
623641170906
623642180302           // p.iva "$$" solo clienti
623643180302
623644180302           if cstcf = 'C';
623645180302             if cstpartIVA = '$$';
623646180302               //cstann = *On;
623647180302               //cstTpAnn = '97';
623648180302               CLEAR cstPartIva;
623649180302               cstUteUlMo = '*AU';
623650180302               cstDtUlMo = %DATE;
623651180302             endif;
623652180302             if cstPiva1 = '$$';
623653180302               //cstann = *On;
623654180302               //cstTpAnn = '97';
623655180302               CLEAR cstPiva1;
623656180302               cstUteUlMo = '*AU';
623657180302               cstDtUlMo = %DATE;
623658180302             endif;
623659180302             if CSTCDFISC = '$$';
623660180302               //cstann = *On;
623661180302               //cstTpAnn = '97';
623662180302               CLEAR cstCdFisc;
623663180302               cstUteUlMo = '*AU';
623664180302               cstDtUlMo = %DATE;
623665180302             endif;
623666180302             if CSTCDFISC1 = '$$';
623667180302               //cstann = *On;
623668180302               //cstTpAnn = '97';
623669180302               CLEAR cstCdFisc1;
623670180302               cstUteUlMo = '*AU';
623671180302               cstDtUlMo = %DATE;
623672180302             endif;
623673180302             IF cstPartIva = *BLANK AND cstCdFisc = *BLANK AND cstStato1 = 'IT';
623674180302               cstAnn = *ON;
623675180302               cstTpAnn = '17';
623676180302               cstUteUlMo = '*AU';
623677180302               cstDtUlMo = %DATE;
623678180302             ENDIF;
623679180302           endif;
623680170830
623681180302           // partite Iva che iniziano con '8' e '9'
623682180302           // solo clienti e solo BRT
623683180302
623684180302           if XScSoc = '201' and cstcf = 'C';
623685180302             if (%subst(cstpartiva:1:1) = '8' or %subst(cstpartiva:1:1) = '9')
623686180302             and cststato1 = 'IT';
623687180302               //if cstpartiva = cstcdfisc or cstcdfisc = *blanks;
623688180302               cstcdfisc = cstpartiva;
623689180302               clear cstpartiva;
623690180302               cstUteUlMo = '*AU';
623691180302               cstDtUlMo = %DATE;
623692180302             //else;
623693180302             //  cstann = *On;
623694180302             //  cstTpAnn = '98';
623695180302             //endif;
623696170912             endif;
623697170912
623698170912             if (%subst(cstPiva1:1:1) = '8' or %subst(cstPiva1:1:1) = '9')
623699170907                  and cststato1 = 'IT';
623700180302               //if cstPiva1 = CSTCDFISC1 or CSTCDFISC1 = *blanks;
623701180302               CSTCDFISC1 = cstPiva1;
623702180302               clear cstPiva1;
623703180302               cstUteUlMo = '*AU';
623704180302               cstDtUlMo = %DATE;
623705180302             //else;
623706180302             //  cstann = *On;
623707180302             //  cstTpAnn = '98';
623708180302             //endif;
623709170912             endif;
623710180302           endif;
623711170912
623712180302         endif;
623713170830
623714180302         // problema nome / cognome da separare
623715180302         // Non li separo, semplicemente metto sia nel cognome che nel nome tutta la ragione sociale
623716180302         // sarà un po' grossolano ma è meno sbagliato.
623717180302
623718180302         if csttpann = '07' and cstcognom1 <> '' and cstnome1 = '';
623719180302           //ysdg000r(cstcognom1:cstnome1);
623720180302           //cstann = *On;
623721180302           //cstTpAnn = '95';
623722180302           cstNome1 = cstCognom1;
623723180302           CLEAR cstAnn;
623724180302           CLEAR cstTpAnn;
623725180302           cstUteUlMo = '*AU';
623726180302           cstDtUlMo = %DATE;
623727180302         endif;
623728170906
623729180302       ENDSR;
