000100000000     H*BIND
000200000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*
000500130826      *           -----------------------------------------
000600130826      *                Spesometro clienti/fornitori
000700130826      *           Raggruppamento dati estratti per Spesometro
000800130826      *           -----------------------------------------
000900130826      *
001000000000     C*----------------------------------------------------*
001100130827     C*
001200130827     C* TPCOM0502 - tipo comunicazione dello Spesometro:
001300130827     C*             B  = Black List
001400130827     C*
001500130827     C*             E  = Elenchi clienti/fornitori
001600130827     C*
001700130827     C*             S  = Acquisti da Rep. di San Marino
001800130827     C*
001900130827     C*             T  = Turismo
002000130827     C*             (non gestito)
002100130827     C*
002200130827     C* TPLIST0502- indica se la chiamata è fatta dal video
002300130827     C*             oppure per la creazione del tracciato:
002400130827     C*             01 = Video
002500130827     C*
002600130827     C*             02 = Tracciato ministeriale
002700000000     C*
002800130826     C* OPZ0502   - indica se si lavora in:
002900000000     C*             01 = allocazione primo record
003000000000     C*                  (serve in manutenzione dati di testata, per
003100000000     C*                   allocare il 1° record)
003200000000     C*
003300000000     C*             02 = aggiornamento
003400000000     C*                  (serve in manutenzione dati di testata, per
003500000000     C*                   aggiornare massivamente i record SELEZIONATI
003600000000     C*                   In questo caso i dati "nuovi" sono nella DS
003700130826     C*                   SDG00592DS)
003800000000     C*
003900000000     C*             04 = cancellazione
004000000000     C*                  (serve in manutenzione dati di testata, per
004100000000     C*                   cancellare massivamente i record SELEZIONATI)
004200000000     C*
004300000000     C*             44 = annullamento/ripristino
004400000000     C*                  (serve in lista dati di testata e in lista
004500000000     C*                   righe di dettaglio per annullare/
004600000000     C*                   ripristinare massivamente i record SELEZIONATI)
004700000000     C*
004800000000     C*             05 = visualizzazione
004900000000     C*                  (serve in visualizzaione dati di testata e
005000000000     C*                   in visualizzazione dei totali per testata
005100000000     C*                   e per quadro)
005200000000     C*
005300130826     C* TPROTT0502 - indica se il pgm viene richiamato per:
005400000000     C*              01 = lista di set di dati
005500000000     C*                   (serve in lista testata, quadri e dettaglio)
005600000000     C*              02 = un unico set di dati
005700000000     C*                   (serve in manutenzione testata, totali testata
005800000000     C*                    e importi quadro)
005900000000     C*
006000130826     C* ROTT0502  - definisce il livello di rottura per il
006100000000     C*             raggruppamento dei dati:
006200000000     C*             01 = dati di testata
006300000000     C*             02 = dati per quadro
006400000000     C*             03 = dati di dettaglio
006500000000     C*
006600000000     F*----------------------------------------------------*
006700000000     F* Movimenti estratti
006800130826     F* Key: Soc/TpCom/Anno/Sogg/Dati di raggruppam.
006900130826     FSDGSCF01L UF   E           K DISK    USROPN
007000130826     F                                     RENAME(SDGSCF000:SDGSCF01)
007100130826     F*-------------
007200130826     F* Movimenti estratti
007300130826     F* Key: Soc/TpCom/Anno/Dati di raggruppam.
007400130826     FSDGSCF02L UF   E           K DISK    USROPN
007500130826     F                                     RENAME(SDGSCF000:SDGSCF02)
007600000000     F*-------------
007700000000     D* Passaggio parametri
007800000000     D KPJBA         E DS
007900000000     D*-------------
008000000000     D* Dati di ambiente ottenuti da SDGSOCDR
008100000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
008200000000     D*-------------
008300000000     D* DS Interna per dati di output di SDGSOCDR
008400000000     D XSOCDS          DS          1000
008500000000     D*-------------
008600130828     D* Parametri di INPUT
008700130826     D SDG00502DS    E DS
008800000000     D*----------
008900130828     D* Parametri di OUTPUT
009000130828     D* per manutenzione di testata, quadri e dettaglio
009100130826     D SDG00592DS    E DS
009200130828     D*--------------------
009300130828     D* Parametri di OUTPUT
009400130828     D* per lista quadri a video e creazione tracciato ministeriale
009500130828     D*---------
009600130828     D*---------
009700130828     D* formato AGGREGATO
009800130828     D*---
009900130828     D* SOGGETTI NAZIONALI: FATTURE, NOTE E DOCUMENTI RIEPILOGATIVI
010000130828     D*                     (NOLEGGIO/LEASING)
010100130828     D SDG005FADS    E DS
010200130828     D*
010300130828     D*---
010400130828     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
010500130828     D SDG005SADS    E DS
010600130828     D*
010700130828     D*---
010800130828     D* SOGGETTI NON RESIDENTI E ACQUISTI DI SERVIZI DA NON RESIDENTI
010900130828     D SDG005BLDS    E DS
011000130828     D*---------
011100130828     D*---------
011200130828     D* formato DETTAGLIATO
011300130828     D*---
011400130828     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI EMESSI
011500130828     D*                     (NOLEGGIO/LEASING)
011600130828     D SDG005FEDS    E DS
011700130828     D*---
011800130828     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI RICEVUTI
011900130828     D SDG005FRDS    E DS
012000130828     D*---
012100130828     D* SOGGETTI NAZIONALI: NOTE EMESSE
012200130828     D SDG005NEDS    E DS
012300130828     D*---
012400130828     D* SOGGETTI NAZIONALI: NOTE RICEVUTE
012500130828     D SDG005NRDS    E DS
012600130828     D*---
012700130828     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
012800130828     D*                     (NOLEGGIO/LEASING)
012900130828     D SDG005DFDS    E DS
013000130828     D*---
013100130828     D* SOGGETTI NON RESIDENTI
013200130828     D*                     (NOLEGGIO/LEASING)
013300130828     D SDG005FNDS    E DS
013400130828     D*---
013500130828     D* ACQUISTI DI SERVIZI DA NON RESIDENTI
013600130828     D SDG005SEDS    E DS
013700131017R136 D*---
013800131017 "   D* SOGGETTI NON RESIDENTI: OPERAZIONI SENZA FATTURA
013900131017 "   D* LEGATE AL TURISMO
014000131017R136 D SDG005TUDS    E DS
014100130828     D*---
014200130828     D* RIEPILOGO
014300130828     D SDG005TADS    E DS
014400000000     D*-------------
014500000000     D* Reperimento nome PGM
014600000000     D STATUS         SDS           333
014700000000     D  DSPGM            *PROC
014800000000     D  PARMS            *PARMS
014900000000     D*-------------
015000000000     D*  campi per parzializzazioni su rcd
015100130826     D $AnnMin         S                   like(Ann0502 )
015200130826     D TpAnn111        S                   like(TpAnn0502 )
015300130826     D CF111           S                   like(CF0502  )
015400131206R141 D Causal111       S                   like(Causal0502)
015500130826     D Sogg111         S                   like(Sogg0502)
015600130903     D TPN111          S                   like(TPN0502)
015700130827     D PIVA111         S                   like(PIVA0502)
015800130826     D CdFisc111       S                   like(CdFisc0502)
015900130826     D CogRag111       S                   like(CogRag0502)
016000130826     D Nome111         S                   like(Nome0502 )
016100130826     D DtNasc111       S                   like(DtNasc0502)
016200130827     D LocNas111       S                   like(LocNas0502)
016300130827     D PrvNas111       S                   like(PrvNas0502)
016400130826     D Stato111        S                   like(Stato0502)
016500130826     D CodUIC111       S                   like(CodUIC0502)
016600130827     D Locali111       S                   like(Locali0502)
016700130826     D IndirE111       S                   like(IndirE0502)
016800130919     D TpOpe111        S                   like(TpOpe0502)
016900130919     D Quadro111       S                   like(Quadro0502)
017000130827     D COGRAr111       S                   like(COGRAR0502)
017100130827     D NOMER111        S                   like(NOMER0502)
017200130827     D DTNASR111       S                   like(DTNASR0502)
017300130827     D LOCNAR111       S                   like(LOCNAR0502)
017400130827     D PRVNAR111       S                   like(PRVNAR0502)
017500130827     D STATOR111       S                   like(STATOR0502)
017600130827     D CODUIR111       S                   like(CODUIR0502)
017700130827     D LOCALR111       S                   like(LOCALR0502)
017800130827     D INDIER111       S                   like(INDIER0502)
017900130903     D TPNR111         S                   like(TPNR0502)
018000130903     D PIVAR111        S                   like(PIVAR0502)
018100130827     D CDFISR111       S                   like(CDFISR0502)
018200130925     D TpOpeR111       S                   like(TpOpeR0502)
018300130925     D NatOpR111       S                   like(NatOpR0502)
018400130902     D LeasiR111       S                   like(LeasiR0502)
018500130827     D DTREGR111       S                   like(DTREGR0502)
018600130827     D NRREGR111       S                   like(NRREGR0502)
018700130827     D SERRER111       S                   like(SERRER0502)
018800130827     D*-----
018900130827     D Ann999          S                   like(Ann0502 )
019000130827     D TpAnn999        S                   like(TpAnn0502 )
019100130827     D CF999           S                   like(CF0502  )
019200131206R141 D Causal999       S                   like(Causal0502)
019300130827     D Sogg999         S                   like(Sogg0502)
019400130903     D TPN999          S                   like(TPN0502)
019500130827     D PIVA999         S                   like(PIVA0502)
019600130827     D CdFisc999       S                   like(CdFisc0502)
019700130827     D CogRag999       S                   like(CogRag0502)
019800130827     D Nome999         S                   like(Nome0502 )
019900130827     D DtNasc999       S                   like(DtNasc0502)
020000130827     D LocNas999       S                   like(LocNas0502)
020100130827     D PrvNas999       S                   like(PrvNas0502)
020200130827     D Stato999        S                   like(Stato0502)
020300130827     D CodUIC999       S                   like(CodUIC0502)
020400130827     D Locali999       S                   like(Locali0502)
020500130827     D IndirE999       S                   like(IndirE0502)
020600130919     D TpOpe999        S                   like(TpOpe0502)
020700130919     D Quadro999       S                   like(Quadro0502)
020800130827     D COGRAr999       S                   like(COGRAR0502)
020900130827     D NOMER999        S                   like(NOMER0502)
021000130827     D DTNASR999       S                   like(DTNASR0502)
021100130827     D LOCNAR999       S                   like(LOCNAR0502)
021200130827     D PRVNAR999       S                   like(PRVNAR0502)
021300130827     D STATOR999       S                   like(STATOR0502)
021400130827     D CODUIR999       S                   like(CODUIR0502)
021500130827     D LOCALR999       S                   like(LOCALR0502)
021600130827     D INDIER999       S                   like(INDIER0502)
021700130903     D TPNR999         S                   like(TPNR0502)
021800130827     D PIVAR999        S                   like(PIVAR0502)
021900130827     D CDFISR999       S                   like(CDFISR0502)
022000130925     D TpOpeR999       S                   like(TpOpeR0502)
022100130902     D NatOpR999       S                   like(NatOpR0502)
022200130902     D LeasiR999       S                   like(LeasiR0502)
022300130828     D DTREGR999       S                   like(DTREGR0502)
022400130827     D NRREGR999       S                   like(NRREGR0502)
022500130827     D SERRER999       S                   like(SERRER0502)
022600000000     D*-------------
022700000000     D*  campi per rottura/salvataggio
022800130827     D SavAnnAtt       S                   like(SCFAnn   )
022900130827     D SavAnn          S                   like(SCFAnn    )
023000130827     D SavTpAnn        S                   like(SCFTpAnn  )
023100130827     D SavOrigine      S                   like(SCFOrigine)
023200130827     D SavSocieta      S                   like(SCFSocieta)
023300130827     D SavTpCom        S                   like(SCFTpCom  )
023400130827     D SavAnno         S                   like(SCFAnno   )
023500130827     D SavMese         S                   like(SCFMese   )
023600130827     D SavCF           S                   like(SCFCF     )
023700130827     D SavSogg         S                   like(SCFSogg   )
023800130903     D SavTpN          S                   like(SCFTpN    )
023900130827     D SavPartIVA      S                   like(SCFPartIVA)
024000130827     D SavCdFisc       S                   like(SCFCdFisc )
024100130827     D SavCogRag       S                   like(SCFCogRag )
024200130827     D SavNome         S                   like(SCFNome   )
024300130827     D SavDtNasc       S                   like(SCFDtNasc )
024400130827     D SavLocNasc      S                   like(SCFLocNasc)
024500130827     D SavPrvNasc      S                   like(SCFPrvNasc)
024600130827     D SavStato        S                   like(SCFStato  )
024700130827     D SavCodUIC       S                   like(SCFCodUIC )
024800130827     D SavLocalit      S                   like(SCFLocalit)
024900130827     D SavIndirE       S                   like(SCFIndirE )
025000130827     D SavKcc          S                   like(SCFKcc    )
025100130827     D SavKsc          S                   like(SCFKsc    )
025200130827     D SavCausale      S                   like(SCFCausale)
025300130827     D SavAliq         S                   like(SCFAliq   )
025400130827     D SavRifIVA       S                   like(SCFRifIVA )
025500130827     D SavDtDoc        S                   like(SCFDtDoc  )
025600130827     D SavNrDoc        S                   like(SCFNrDoc  )
025700130827     D SavSerDoc       S                   like(SCFSerDoc )
025800130827     D SavDtReg        S                   like(SCFDtReg  )
025900130827     D SavNrReg        S                   like(SCFNrReg  )
026000130827     D SavSerReg       S                   like(SCFSerReg )
026100130827     D SavDtPar        S                   like(SCFDtPar  )
026200130827     D SavNrPar        S                   like(SCFNrPar  )
026300130827     D SavSerPar       S                   like(SCFSerPar )
026400130827     D SavSys          S                   like(SCFSys    )
026500130827     D SavNrAsReg      S                   like(SCFNrAsReg)
026600130827     D SavNrRigaM      S                   like(SCFNrRigaM)
026700130827     D SavNrRiga       S                   like(SCFNrRiga )
026800130827     D SavTpDoc        S                   like(SCFTpDoc  )
026900130827     D SavTpOpe        S                   like(SCFTpOpe  )
027000130827     D SavNoIVA        S                   like(SCFNoIVA  )
027100130827     D SavMargine      S                   like(SCFMargine)
027200130827     D SavReverse      S                   like(SCFReverse)
027300130827     D SavAutofat      S                   like(SCFAutofat)
027400130827     D SavTpImp        S                   like(SCFTpImp  )
027500130827     D SavImpOpe       S                   like(SCFImpOpe )
027600130827     D SavIVAOpe       S                   like(SCFIVAOpe )
027700130827     D SavImpTot       S                   like(SCFImpTot )
027800130827     D SavIVATot       S                   like(SCFIVATot )
027900130827     D SavTpReg        S                   like(SCFTpReg  )
028000130827     D SavLibro        S                   like(SCFLibro  )
028100130827     D SavVarNota      S                   like(SCFVarNota)
028200130827     D SavFormato      S                   like(SCFFormato)
028300130827     D SavSEQQUA       S                   like(SCFSEQQUA )
028400130827     D SavQUADRO       S                   like(SCFQUADRO )
028500130827     D SavCOGRAGR      S                   like(SCFCOGRAGR)
028600130827     D SavNOMER        S                   like(SCFNOMER  )
028700130827     D SavDTNASR       S                   like(SCFDTNASR )
028800130827     D SavLOCNASR      S                   like(SCFLOCNASR)
028900130827     D SavPRVNASR      S                   like(SCFPRVNASR)
029000130827     D SavSTATOR       S                   like(SCFSTATOR )
029100130827     D SavCODUICR      S                   like(SCFCODUICR)
029200130827     D SavLOCALIR      S                   like(SCFLOCALIR)
029300130827     D SavINDIRER      S                   like(SCFINDIRER)
029400130903     D SavTpNR         S                   like(SCFTpNR   )
029500130827     D SavPIVAR        S                   like(SCFPIVAR  )
029600130827     D SavCDFISCR      S                   like(SCFCDFISCR)
029700130925     D SavTpOpeR       S                   like(SCFTpOpeR )
029800130925     D SavNatOpeR      S                   like(SCFNatOpeR)
029900130902     D SavLeasinR      S                   like(SCFLeasinR)
030000130827     D SavDTREGR       S                   like(SCFDTREGR )
030100130827     D SavNRREGR       S                   like(SCFNRREGR )
030200130827     D SavSERREGR      S                   like(SCFSERREGR)
030300131108R140 X*ex R136  -------------
030400131108 "   X*ex R136  per numero clienti con quadro TU
030500131108 "   X*ex R136  Sa2COGRAGR      S                   like(SCFCOGRAGR)
030600131108 "   X*ex R136  Sa2NOMER        S                   like(SCFNOMER  )
030700131108 "   X*ex R136  Sa2DTNASR       S                   like(SCFDTNASR )
030800131108 "   X*ex R136  Sa2LOCNASR      S                   like(SCFLOCNASR)
030900131108 "   X*ex R136  Sa2PRVNASR      S                   like(SCFPRVNASR)
031000131108 "   X*ex R136  Sa2STATOR       S                   like(SCFSTATOR )
031100131108 "   X*ex R136  Sa2CODUICR      S                   like(SCFCODUICR)
031200131108 "   X*ex R136  Sa2LOCALIR      S                   like(SCFLOCALIR)
031300131108R140 X*ex R136  Sa2INDIRER      S                   like(SCFINDIRER)
031400130904     D*-------------
031500130904     D* per calcolo importi
031600130904     D SavDtReg2       S                   like(SCFDtReg  )
031700130904     D SavNrReg2       S                   like(SCFNrReg  )
031800130904     D SavSerReg2      S                   like(SCFSerReg )
031900000000     D*-------------
032000000000     D* campi di lavoro
032100000000     D WDtMax          S               D   Inz(*Hival)
032200000000     D WDataAlfa       S             10A
032300000000     D UDateISO        S               D
032400130826     D $FineSCF        S              1A
032500000000     D $RcdOk          S              1A
032600000000     D $PrimaLett      S              1A
032700130902     D*--------------
032800130903     D* conta i numeri di quadri ????
032900000000     D $NrTrovati      S              9S 0
033000130902     D*--------------
033100130902     D* servono per il flag di annullato "aggregato"
033200110824     D $NrRcdLett      S              9S 0
033300130902     D $NrRcdAnn       S              9S 0
033400130902     D*----------------------------------------------------*
033500130902     D $NrOpeAFA       S                   Like(NROPEA05FA)
033600130902     D $NrOpePFA       S                   Like(NROPEP05FA)
033700130902     D $ImpAttFA       S                   Like(IMPATT05FA)
033800130902     D $IVAAttFA       S                   Like(IVAATT05FA)
033900130902     D $MargiAFA       S                   Like(MARGIA05FA)
034000130902     D $ImpNDeFA       S                   Like(IMPNDE05FA)
034100130902     D $IVANDeFA       S                   Like(IVANDE05FA)
034200130902     D $ImpPasFA       S                   Like(IMPPAS05FA)
034300130902     D $IVAPasFA       S                   Like(IVAPAS05FA)
034400130902     D $MargiPFA       S                   Like(MARGIP05FA)
034500130902     D $ImpNCrFA       S                   Like(IMPNCR05FA)
034600130902     D $IVANCrFA       S                   Like(IVANCR05FA)
034700130903     D $NrOpeSA        S                   Like(NROPE05SA)
034800130902     D $ImportSA       S                   Like(IMPORT05SA)
034900130902     D $IMPATTBL       S                   Like(IMPATT05BL)
035000130902     D $IVAATTBL       S                   Like(IVAATT05BL)
035100130902     D $NOIBATBL       S                   Like(NOIBAT05BL)
035200130902     D $NOISATBL       S                   Like(NOISAT05BL)
035300130902     D $IMPNATBL       S                   Like(IMPNAT05BL)
035400130902     D $IVANATBL       S                   Like(IVANAT05BL)
035500130902     D $IMPPASBL       S                   Like(IMPPAS05BL)
035600130902     D $IVAPASBL       S                   Like(IVAPAS05BL)
035700130902     D $NOIVAPBL       S                   Like(NOIVAP05BL)
035800130902     D $IMPNPABL       S                   Like(IMPNPA05BL)
035900130902     D $IVANPABL       S                   Like(IVANPA05BL)
036000130903     D $ImportFE       S                   Like(IMPORT05FE)
036100130903     D $IVAFE          S                   Like(IVA05FE)
036200130903     D $ImportFR       S                   Like(IMPORT05FR)
036300130903     D $IVAFR          S                   Like(IVA05FR)
036400130903     D $ImportNE       S                   Like(IMPORT05NE)
036500130903     D $IVANE          S                   Like(IVA05NE)
036600130903     D $ImportNR       S                   Like(IMPORT05NR)
036700130903     D $IVANR          S                   Like(IVA05NR)
036800130903     D $ImportDF       S                   Like(IMPORT05DF)
036900130903     D $ImportFN       S                   Like(IMPORT05FN)
037000130903     D $IVAFN          S                   Like(IVA05FN)
037100130903     D $ImportSE       S                   Like(IMPORT05SE)
037200130903     D $IVASE          S                   Like(IVA05SE)
037300131017R136 D $ImportTU       S                   Like(IMPORT05TU)
037400131017R136 D $IVATU          S                   Like(IVA05TU)
037500130903     D $NRCPFATA       S                   Like(NRCPFA05TA)
037600130903     D $NRCLSATA       S                   Like(NRCLSA05TA)
037700130903     D $NRBLBLTA       S                   Like(NRBLBL05TA)
037800130903     D $NRNRBLTA       S                   Like(NRNRBL05TA)
037900130903     D $NRSRBLTA       S                   Like(NRSRBL05TA)
038000130903     D $NRFEFETA       S                   Like(NRFEFE05TA)
038100130903     D $NRRIFETA       S                   Like(NRRIFE05TA)
038200130903     D $NRFRFRTA       S                   Like(NRFRFR05TA)
038300130903     D $NRRIFRTA       S                   Like(NRRIFR05TA)
038400130903     D $NRNONETA       S                   Like(NRNONE05TA)
038500130903     D $NRNONRTA       S                   Like(NRNONR05TA)
038600131015R135 X**** $NRCLDFTA       S                   Like(NRCLDF05TA)
038700131015R135 D $NRRIDFTA       S                   Like(NRRIDF05TA)
038800130903     D $NRRIFNTA       S                   Like(NRRIFN05TA)
038900130903     D $NRRISETA       S                   Like(NRRISE05TA)
039000130903     D $NRCLTUTA       S                   Like(NRCLTU05TA)
039100000000     C*----------------------------------------------------*
039200000000     C*                MAIN LINE PROGRAM
039300000000     C*----------------------------------------------------*
039400000000     C*
039500130827     C                   EXSR      INZVAR
039600000000     C*
039700000000     C* Se è la prima chiamata eseguo il posizionamento
039800130826     C                   if        RIC0502     = '0'
039900000000     C                   Eval      $PrimaLett  = *On
040000130902     C                   Eval      $NrTrovati = 0
040100130903     C                   EXSR      InzTotali
040200130902     C*
040300130827     C                   EXSR      SETANA
040400130827     C                   EXSR      REDANA
040500000000     C                   endif
040600000000     C*
040700130826     C                   DoW       $FineSCF    = *Off
040800000000     C*
040900130827     C                   EXSR      SELANA
041000000000     C*
041100000000     C                   if        $RcdOk      = *On
041200000000     C*
041300000000     C                   Select
041400000000     C* allocazione record
041500130826     C                   When      Opz0502     = '01'
041600130827     C                   EXSR      Opz_01
041700000000     C*
041800000000     C* dati anagrafici
041900130826     C                   When      Rott0502    = '01'
042000130827     C                   EXSR      Rott_01
042100000000     C*
042200000000     C* quadri di presentazione
042300130826     C                   When      Rott0502    = '02'
042400130827     C                   EXSR      Rott_02
042500000000     C*
042600000000     C* dati di dettaglio
042700130826     C                   When      Rott0502    = '03'
042800130827     C                   EXSR      Rott_03
042900000000     C                   endSL
043000000000     C*
043100000000     C                   Eval      $PrimaLett  = *Off
043200000000     C*
043300000000     C*
043400000000     C                   Select
043500000000     C*
043600130826     C                   When      Opz0502     = '02'
043700130827     C                   EXSR      RieSDGSCF
043800130828     C                   if        TpList0502  = '01'
043900130828     C                   UPDATE    SDGSCF01
044000130828     C                   else
044100130828     C                   UPDATE    SDGSCF02
044200130828     C                   endif
044300000000     C*
044400130826     C                   When      Opz0502     = '04'
044500130828     C                   if        TpList0502  = '01'
044600130828     C                   DELETE    SDGSCF01
044700130828     C                   else
044800130828     C                   DELETE    SDGSCF02
044900130828     C                   endif
045000000000     C*
045100130826     C                   When      Opz0502     = '44'
045200130827     C                   EXSR      AnnSDGSCF
045300130828     C                   if        TpList0502  = '01'
045400130828     C                   UPDATE    SDGSCF01
045500130828     C                   else
045600130828     C                   UPDATE    SDGSCF02
045700130828     C                   endif
045800000000     C                   endSl
045900000000     C*
046000000000     C                   endif
046100000000     C*
046200130827     C                   EXSR      REDANA
046300000000     C                   endDo
046400000000     C*
046500130827     C                   EXSR      ENDPGM
046600000000     C*
046700000000     C************************************************************
046800000000     C* Gestione opzione allocazione
046900000000     C************************************************************
047000000000     C     Opz_01        BEGSR
047100000000     C*
047200130902     C                   Add       1             $NrTrovati
047300000000     C*
047400130827     C                   EXSR      RieSavRcd
047500130827     C                   EXSR      RieOutput
047600000000     C*
047700130827     C                   EXSR      ENDPGM
047800000000     C*
047900000000     C                   ENDSR
048000000000     C/EJECT
048100130902     C************************************************************
048200130902     C* Imposto i dati del record letto
048300130902     C************************************************************
048400130902     C     RieSavRcd     BEGSR
048500130902     C*
048600130827     C                   Eval      SavAnnAtt   = *Blank
048700130827     C                   Z-Add     0             $NrRcdLett
048800130827     C                   Z-Add     0             $NrRcdAnn
048900130827     C*
049000130827     C                   Eval      SavAnn      = SCFAnn
049100130827     C                   Eval      SavTpAnn    = SCFTpAnn
049200130827     C                   Eval      SavOrigine  = SCFOrigine
049300130827     C                   Eval      SavSocieta  = SCFSocieta
049400130827     C                   Eval      SavTpCom    = SCFTpCom
049500130827     C                   Eval      SavAnno     = SCFAnno
049600130827     C                   Eval      SavMese     = SCFMese
049700130827     C                   Eval      SavCF       = SCFCF
049800130827     C                   Eval      SavSogg     = SCFSogg
049900130903     C                   Eval      SavTpN      = SCFTpN
050000130827     C                   Eval      SavPartIVA  = SCFPartIVA
050100130827     C                   Eval      SavCdFisc   = SCFCdFisc
050200130827     C                   Eval      SavCogRag   = SCFCogRag
050300130827     C                   Eval      SavNome     = SCFNome
050400130827     C                   Eval      SavDtNasc   = SCFDtNasc
050500130827     C                   Eval      SavLocNasc  = SCFLocNasc
050600130827     C                   Eval      SavPrvNasc  = SCFPrvNasc
050700130827     C                   Eval      SavStato    = SCFStato
050800130827     C                   Eval      SavCodUIC   = SCFCodUIC
050900130827     C                   Eval      SavLocalit  = SCFLocalit
051000130827     C                   Eval      SavIndirE   = SCFIndirE
051100130827     C*
051200130903     C                   Eval      SavKcc      = SCFKcc
051300130903     C                   Eval      SavKsc      = SCFKsc
051400130903     C                   Eval      SavCausale  = SCFCausale
051500130903     C                   Eval      SavAliq     = SCFAliq
051600130903     C                   Eval      SavRifIVA   = SCFRifIVA
051700130903     C                   Eval      SavDtDoc    = SCFDtDoc
051800130903     C                   Eval      SavNrDoc    = SCFNrDoc
051900130903     C                   Eval      SavSerDoc   = SCFSerDoc
052000130903     C                   Eval      SavDtReg    = SCFDtReg
052100130903     C                   Eval      SavNrReg    = SCFNrReg
052200130903     C                   Eval      SavSerReg   = SCFSerReg
052300130903     C                   Eval      SavDtPar    = SCFDtPar
052400130903     C                   Eval      SavNrPar    = SCFNrPar
052500130903     C                   Eval      SavSerPar   = SCFSerPar
052600130903     C                   Eval      SavSys      = SCFSys
052700130903     C                   Eval      SavNrAsReg  = SCFNrAsReg
052800130903     C                   Eval      SavNrRigaM  = SCFNrRigaM
052900130903     C                   Eval      SavNrRiga   = SCFNrRiga
053000130827     C*
053100130903     C                   Eval      SavTpDoc    = SCFTpDoc
053200130903     C                   Eval      SavTpOpe    = SCFTpOpe
053300130903     C                   Eval      SavNoIVA    = SCFNoIVA
053400130903     C                   Eval      SavMargine  = SCFMargine
053500130903     C                   Eval      SavReverse  = SCFReverse
053600130903     C                   Eval      SavAutofat  = SCFAutofat
053700130903     C                   Eval      SavTpImp    = SCFTpImp
053800130827     C*
053900130903     C                   Eval      SavImpOpe   = 0
054000130903     C                   Eval      SavIVAOpe   = 0
054100130903     C                   Eval      SavImpTot   = 0
054200130903     C                   Eval      SavIVATot   = 0
054300130902     C*
054400130923     C****               Eval      SavDtReg2   = SCFDtReg
054500130923     C****               Eval      SavNrReg2   = SCFNrReg
054600130923     C****               Eval      SavSerReg2  = SCFSerReg
054700130923     C                   Eval      SavDtReg2   = *Loval
054800130923     C                   Eval      SavNrReg2   = 0
054900130923     C                   Eval      SavSerReg2  = *Blank
055000130902     C                   EXSR      InzImporti
055100130827     C*
055200130903     C                   Eval      SavTpReg    = SCFTpReg
055300130903     C                   Eval      SavLibro    = SCFLibro
055400130903     C                   Eval      SavVarNota  = SCFVarNota
055500130903     C                   Eval      SavFormato  = SCFFormato
055600130827     C*
055700130827     C                   EXSR      RieSavQuadri
055800130827     C*
055900130827     C                   ENDSR
056000130827     C/EJECT
056100130902     C************************************************************
056200130902     C* Inizializzo i campi importi
056300130902     C************************************************************
056400130902     C     InzImporti    BEGSR
056500130903     C*
056600130903     C                   Eval      $NrOpeAFA   = 0
056700130903     C                   Eval      $NrOpePFA   = 0
056800130903     C*
056900130903     C                   Eval      $NrOpeSA    = 0
057000130902     C*
057100130903     C                   Eval      $ImpAttFA   = 0
057200130903     C                   Eval      $IVAAttFA   = 0
057300130903     C                   Eval      $MargiAFA   = 0
057400130903     C                   Eval      $ImpNDeFA   = 0
057500130903     C                   Eval      $IVANDeFA   = 0
057600130902     C*
057700130903     C                   Eval      $ImpPasFA   = 0
057800130903     C                   Eval      $IVAPasFA   = 0
057900130903     C                   Eval      $MargiPFA   = 0
058000130903     C                   Eval      $ImpNCrFA   = 0
058100130903     C                   Eval      $IVANCrFA   = 0
058200130902     C*
058300130903     C                   Eval      $ImportSA   = 0
058400130902     C*
058500130903     C                   Eval      $IMPATTBL   = 0
058600130903     C                   Eval      $IVAATTBL   = 0
058700130903     C                   Eval      $NOIBATBL   = 0
058800130903     C                   Eval      $NOISATBL   = 0
058900130903     C                   Eval      $IMPNATBL   = 0
059000130903     C                   Eval      $IVANATBL   = 0
059100130903     C                   Eval      $IMPPASBL   = 0
059200130903     C                   Eval      $IVAPASBL   = 0
059300130903     C                   Eval      $NOIVAPBL   = 0
059400130903     C                   Eval      $IMPNPABL   = 0
059500130903     C                   Eval      $IVANPABL   = 0
059600130903     C*
059700130903     C                   Eval      $ImportFE   = 0
059800130903     C                   Eval      $IVAFE      = 0
059900130903     C*
060000130903     C                   Eval      $ImportFR   = 0
060100130903     C                   Eval      $IVAFR      = 0
060200130903     C*
060300130903     C                   Eval      $ImportNE   = 0
060400130903     C                   Eval      $IVANE      = 0
060500130903     C*
060600130903     C                   Eval      $ImportNR   = 0
060700130903     C                   Eval      $IVANR      = 0
060800130903     C*
060900130903     C                   Eval      $ImportDF   = 0
061000130903     C*
061100130903     C                   Eval      $ImportFN   = 0
061200130903     C                   Eval      $IVAFN      = 0
061300130903     C*
061400130903     C                   Eval      $ImportSE   = 0
061500130903     C                   Eval      $IVASE      = 0
061600131017R136 C*
061700131017 "   C                   Eval      $ImportTU   = 0
061800131017R136 C                   Eval      $IVATU      = 0
061900130902     C*
062000130902     C                   ENDSR
062100130902     C/EJECT
062200130827     C************************************************************
062300130827     C* Imposto i dati del record letto  per rottura quadri
062400130827     C************************************************************
062500130827     C     RieSavQuadri  BEGSR
062600130827     C*
062700130903     C                   Eval      SavSEQQUA   = SCFSEQQUA
062800130903     C                   Eval      SavQUADRO   = SCFQUADRO
062900130903     C                   Eval      SavCOGRAGR  = SCFCOGRAGR
063000130903     C                   Eval      SavNOMER    = SCFNOMER
063100130903     C                   Eval      SavDTNASR   = SCFDTNASR
063200130903     C                   Eval      SavLOCNASR  = SCFLOCNASR
063300130903     C                   Eval      SavPRVNASR  = SCFPRVNASR
063400130903     C                   Eval      SavSTATOR   = SCFSTATOR
063500130903     C                   Eval      SavCODUICR  = SCFCODUICR
063600130903     C                   Eval      SavLOCALIR  = SCFLOCALIR
063700130903     C                   Eval      SavINDIRER  = SCFINDIRER
063800130903     C                   Eval      SavTpNR     = SCFTpNR
063900130903     C                   Eval      SavPIVAR    = SCFPIVAR
064000130903     C                   Eval      SavCDFISCR  = SCFCDFISCR
064100130925     C                   Eval      SavTpOpeR   = SCFTpOpeR
064200130925     C                   Eval      SavNATOPER  = SCFNATOPER
064300130903     C                   Eval      SavLEASINR  = SCFLEASINR
064400130903     C                   Eval      SavDTREGR   = SCFDTREGR
064500130903     C                   Eval      SavNRREGR   = SCFNRREGR
064600130903     C                   Eval      SavSERREGR  = SCFSERREGR
064700130827     C*
064800130827     C                   ENDSR
064900130827     C/EJECT
065000130827     C************************************************************
065100130827     C* Imposto i dati di output
065200130827     C************************************************************
065300130827     C     RieOutput     BEGSR
065400130827     C*
065500130827     C                   Clear                   SDG00592DS
065600130827     C*
065700130827     C* valorizzo i dati di TESTATA validi per tutti i livelli
065800130827     C* di rottura
065900130827     C                   if        $NrRcdLett  = $NrRcdAnn  and
066000130827     C                             $NrRcdLett  > 0          and
066100130827     C                             SavAnnAtt   = '1'
066200130827     C                   Eval      AnnAtt0592  = '9'
066300130827     C                   else
066400130827     C                   Eval      AnnAtt0592  = SavAnnAtt
066500130827     C                   endif
066600130827     C*
066700130827     C                   Eval      Ann0592     = SavAnn
066800130827     C                   Eval      TpAnn0592   = SavTpAnn
066900130827     C                   Eval      Soc0592     = SavSocieta
067000130827     C                   Eval      Anno0592    = SavAnno
067100130827     C                   Eval      Mese0592    = SavMese
067200130827     C*
067300130827     C                   Eval      Sogg0592    = SavSogg
067400130903     C                   Eval      TpN0592     = SavTpN
067500130827     C                   Eval      PIVA0592    = SavPartIVA
067600130827     C                   Eval      CdFisc0592  = SavCdFisc
067700130827     C                   Eval      CogRag0592  = SavCogRag
067800130827     C                   Eval      Nome0592    = SavNome
067900130827     C                   Eval      DtNasc0592  = SavDtNasc
068000130827     C                   Eval      LocNas0592  = SavLocNasc
068100130827     C                   Eval      PrvNas0592  = SavPrvNasc
068200130827     C                   Eval      Stato0592   = SavStato
068300130827     C                   Eval      CodUIC0592  = SavCodUIC
068400130827     C                   Eval      Locali0592  = SavLocalit
068500130827     C                   Eval      IndirE0592  = SavIndirE
068600130827     C                   Eval      Format0592  = SavFormato
068700130827     C*
068800130827     C*
068900130827     C* valorizzo i dati relativi al QUADRO e validi anche per le
069000130827     C* righe di dettaglio
069100130827     C                   if        Rott0502    = '02'           or
069200130827     C                             Rott0502    = '03'
069300130827     C*
069400130827     C                   Eval      SEQQUA0592  = SavSEQQUA
069500130827     C*
069600130827     C                   Eval      QUADRO0592  = SavQUADRO
069700130827     C                   Eval      COGRAR0592  = SavCOGRAGR
069800130827     C                   Eval      NOMER0592   = SavNOMER
069900130827     C                   Eval      DTNASR0592  = SavDTNASR
070000130827     C                   Eval      LOCNAR0592  = SavLOCNASR
070100130827     C                   Eval      PRVNAR0592  = SavPRVNASR
070200130827     C                   Eval      STATOR0592  = SavSTATOR
070300130828     C                   Eval      CODUIR0592  = SavCODUICR
070400130827     C                   Eval      LOCALR0592  = SavLOCALIR
070500130828     C                   Eval      INDIER0592  = SavINDIRER
070600130903     C                   Eval      TPNR0592    = SavTpNR
070700130903     C                   Eval      PIVAR0592   = SavPIVAR
070800130828     C                   Eval      CDFISR0592  = SavCDFISCR
070900130925     C                   Eval      TpOpeR0592  = SavTpOpeR
071000130925     C                   Eval      NATOPR0592  = SavNATOPER
071100130902     C                   Eval      LEASIR0592  = SavLEASINR
071200130827     C                   Eval      DTREGR0592  = SavDTREGR
071300130827     C                   Eval      NRREGR0592  = SavNRREGR
071400130827     C                   Eval      SERRER0592  = SavSERREGR
071500130827     C*
071600130827     C                   endif
071700130827     C*
071800130827     C* valorizzo i campi validi solo per le RIGHE DI DETTAGLIO
071900130827     C                   if        Rott0502    = '03'
072000130827     C*
072100130827     C                   Eval      CF0592      = SavCF
072200130827     C                   Eval      TpOpe0592   = SavTpOpe
072300130827     C                   Eval      NoIVA0592   = SavNoIVA
072400130827     C                   Eval      Margin0592  = SavMargine
072500130827     C                   Eval      Revers0592  = SavReverse
072600130827     C                   Eval      Autofa0592  = SavAutofat
072700130827     C*
072800130919     C                   Eval      TpDoc0592   = SavTpDoc
072900130827     C                   Eval      TpImp0592   = SavTpImp
073000130827     C                   Eval      TpReg0592   = SavTpReg
073100130827     C                   Eval      Libro0592   = SavLibro
073200130828     C                   Eval      VarNot0592  = SavVarNota
073300130827     C                   Eval      ORIGIN0592  = SavOrigine
073400130827     C                   Eval      KCC0592     = SavKcc
073500130827     C                   Eval      KSC0592     = SavKsc
073600130827     C                   Eval      CAUSAL0592  = SavCausale
073700130827     C                   Eval      ALIQ0592    = SavAliq
073800130827     C                   Eval      RIFIVA0592  = SavRifIVA
073900130827     C                   Eval      DTDOC0592   = SavDtDoc
074000130827     C                   Eval      NRDOC0592   = SavNrDoc
074100130827     C                   Eval      SERDOC0592  = SavSerDoc
074200130827     C                   Eval      DTREG0592   = SavDtReg
074300130827     C                   Eval      NRREG0592   = SavNrReg
074400130827     C                   Eval      SERREG0592  = SavSerReg
074500130827     C                   Eval      DTPAR0592   = SavDtPar
074600130827     C                   Eval      NRPAR0592   = SavNrPar
074700130827     C                   Eval      SERPAR0592  = SavSerPar
074800130827     C                   Eval      SYS0592     = SavSys
074900130827     C                   Eval      NRASRE0592  = SavNrAsReg
075000130827     C                   Eval      NRRIGM0592  = SavNrRigaM
075100130827     C                   Eval      NRRIGA0592  = SavNrRiga
075200130827     C                   endif
075300130827     C*
075400130827     C* IMPORTI CALCOLATI VALIDI PER LE VARIE ROTTURE
075500130827     C                   Eval      ImpOpe0592  = SavImpOpe
075600130827     C                   Eval      IVAOpe0592  = SavIVAOpe
075700130827     C                   Eval      ImpTot0592  = SavImpTot
075800130827     C                   Eval      IVATot0592  = SavIVATot
075900130827     C*
076000130827     C                   ENDSR
076100130827     C/EJECT
076200000000     C************************************************************
076300000000     C* Gestione rottura anagrafica
076400000000     C************************************************************
076500000000     C     Rott_01       BEGSR
076600000000     C*
076700130904     C                   if        SCFPartIVA <> SavPartIVA   or
076800130826     C                             SCFCdFisc  <> SavCdFisc    or
076900130826     C                             SCFCOGRAG  <> SavCOGRAG    or
077000130826     C                             SCFNOME    <> SavNOME      or
077100130826     C                             SCFDTNASC  <> SavDTNASC    or
077200130826     C                             SCFLOCNASC <> SavLOCNASC   or
077300130826     C                             SCFPRVNASC <> SavPRVNASC   or
077400130826     C                             SCFSTATO   <> SavSTATO     or
077500130826     C                             SCFCODUIC  <> SavCODUIC    or
077600130826     C                             SCFLOCALIT <> SavLOCALIT   or
077700130826     C                             SCFINDIRE  <> SavINDIRE
077800130902     C*
077900130902     C                   Add       1             $NrTrovati
078000000000     C*
078100000000     C                   if        $PrimaLett <> *On
078200000000     C*
078300130826     C                   if        Opz0502     = '05'
078400130827     C                   EXSR      RieOutput
078500000000     C                   endif
078600000000     C*
078700000000     C                   endif
078800000000     C*
078900130827     C                   EXSR      RieSavRcd
079000000000     C*
079100000000     C                   if        $PrimaLett <> *On
079200000000     C*
079300130826     C                   if        Opz0502     = '05'
079400130827     C                   EXSR      ENDPGM
079500000000     C                   endif
079600000000     C*
079700000000     C                   endif
079800000000     C*
079900000000     C                   endif
080000110902     C*
080100130826     C                   if        Opz0502     = '05'
080200130827     C                   EXSR      AddImporti
080300110902     C*
080400130827     C*
080500130827     C* campi per QUADRI
080600130829     C                   if        SCFQUADRO  <> SavQUADRO    or
080700130903     C                             SCFCOGRAGR <> SavCOGRAGR   or
080800130827     C                             SCFNOMER   <> SavNOMER     or
080900130827     C                             SCFDTNASR  <> SavDTNASR    or
081000130827     C                             SCFLOCNASR <> SavLOCNASR   or
081100130827     C                             SCFPRVNASR <> SavPRVNASR   or
081200130827     C                             SCFSTATOR  <> SavSTATOR    or
081300130827     C                             SCFCODUICR <> SavCODUICR   or
081400130827     C                             SCFLOCALIR <> SavLOCALIR   or
081500130827     C                             SCFINDIRER <> SavINDIRER   or
081600130903     C                             SCFTpNR    <> SavTpNR      or
081700130827     C                             SCFPIVAR   <> SavPIVAR     or
081800130827     C                             SCFCDFISCR <> SavCDFISCR   or
081900130925     C                             SCFTpOpeR  <> SavTpOpeR    or
082000130925     C                             SCFNATOPER <> SavNATOPER   or
082100130902     C                             SCFLEASINR <> SavLEASINR   or
082200130827     C                             SCFDTREGR  <> SavDTREGR    or
082300130827     C                             SCFNRREGR  <> SavNRREGR    or
082400130827     C                             SCFSERREGR <> SavSERREGR
082500110902     C* aggiorno comunque i dati salvati per i quadri, servono per
082600130827     C* i totali sul soggetto
082700130903     C                   EXSR      RieSavQuadri
082800110902     C                   endif
082900110902     C*
083000130827     C                   EXSR      UpdAnnAtt
083100000000     C                   endif
083200000000     C*
083300000000     C                   ENDSR
083400000000     C/EJECT
083500000000     C************************************************************
083600000000     C* Gestione rottura per quadri
083700000000     C************************************************************
083800000000     C     Rott_02       BEGSR
083900000000     C*
084000130925     C                   if        (TpList0502  = '01'         and
084100130925     C                              (SCFPartIVA <> SavPartIVA   or
084200130925     C                               SCFCdFisc  <> SavCdFisc    or
084300130925     C                               SCFCOGRAG  <> SavCOGRAG    or
084400130925     C                               SCFNOME    <> SavNOME      or
084500130925     C                               SCFDTNASC  <> SavDTNASC    or
084600130925     C                               SCFLOCNASC <> SavLOCNASC   or
084700130925     C                               SCFPRVNASC <> SavPRVNASC   or
084800130925     C                               SCFSTATO   <> SavSTATO     or
084900130925     C                               SCFCODUIC  <> SavCODUIC    or
085000130925     C                               SCFLOCALIT <> SavLOCALIT   or
085100130925     C                               SCFINDIRE  <> SavINDIRE    or
085200130925     C* campi per QUADRI
085300130925     C                               SCFQUADRO  <> SavQUADRO    or
085400130925     C                               SCFCOGRAGR <> SavCOGRAGR   or
085500130925     C                               SCFNOMER   <> SavNOMER     or
085600130925     C                               SCFDTNASR  <> SavDTNASR    or
085700130925     C                               SCFLOCNASR <> SavLOCNASR   or
085800130925     C                               SCFPRVNASR <> SavPRVNASR   or
085900130925     C                               SCFSTATOR  <> SavSTATOR    or
086000130925     C                               SCFCODUICR <> SavCODUICR   or
086100130925     C                               SCFLOCALIR <> SavLOCALIR   or
086200130925     C                               SCFINDIRER <> SavINDIRER   or
086300130925     C                               SCFTpNR    <> SavTpNR      or
086400130925     C                               SCFPIVAR   <> SavPIVAR     or
086500130925     C                               SCFCDFISCR <> SavCDFISCR   or
086600130925     C                               SCFTpOpeR  <> SavTpOpeR    or
086700130925     C                               SCFNATOPER <> SavNATOPER   or
086800130925     C                               SCFLEASINR <> SavLEASINR   or
086900130925     C                               SCFDTREGR  <> SavDTREGR    or
087000130925     C                               SCFNRREGR  <> SavNRREGR    or
087100130925     C                               SCFSERREGR <> SavSERREGR     ))
087200130925     C                             OR
087300130925     C                             (TpList0502  = '02'        and
087400130925     C
087500130826     C* campi per QUADRI
087600130925     C                              (SCFQUADRO  <> SavQUADRO    or
087700130925     C                               SCFCOGRAGR <> SavCOGRAGR   or
087800130925     C                               SCFNOMER   <> SavNOMER     or
087900130925     C                               SCFDTNASR  <> SavDTNASR    or
088000130925     C                               SCFLOCNASR <> SavLOCNASR   or
088100130925     C                               SCFPRVNASR <> SavPRVNASR   or
088200130925     C                               SCFSTATOR  <> SavSTATOR    or
088300130925     C                               SCFCODUICR <> SavCODUICR   or
088400130925     C                               SCFLOCALIR <> SavLOCALIR   or
088500130925     C                               SCFINDIRER <> SavINDIRER   or
088600130925     C                               SCFTpNR    <> SavTpNR      or
088700130925     C                               SCFPIVAR   <> SavPIVAR     or
088800130925     C                               SCFCDFISCR <> SavCDFISCR   or
088900130925     C                               SCFTpOpeR  <> SavTpOpeR    or
089000130925     C                               SCFNATOPER <> SavNATOPER   or
089100130925     C                               SCFLEASINR <> SavLEASINR   or
089200130925     C                               SCFDTREGR  <> SavDTREGR    or
089300130925     C                               SCFNRREGR  <> SavNRREGR    or
089400130925     C                               SCFSERREGR <> SavSERREGR     ))
089500130902     C*
089600130902     C                   Add       1             $NrTrovati
089700000000     C*
089800000000     C                   if        $PrimaLett <> *On
089900000000     C*
090000130826     C                   if        Opz0502     = '05'
090100130909     C                   EXSR      RieOutput
090200130909     C                   EXSR      RieQuadri
090300000000     C                   endif
090400000000     C*
090500000000     C                   endif
090600000000     C*
090700130827     C                   EXSR      RieSavRcd
090800000000     C*
090900000000     C                   if        $PrimaLett <> *On
091000000000     C*
091100130826     C                   if        Opz0502     = '05'
091200130827     C                   EXSR      ENDPGM
091300000000     C                   endif
091400000000     C*
091500000000     C                   endif
091600000000     C*
091700130902     C                   endif
091800000000     C*
091900130826     C                   if        Opz0502     = '05'
092000130827     C                   EXSR      AddImporti
092100130827     C                   EXSR      UpdAnnAtt
092200000000     C                   endif
092300000000     C*
092400000000     C                   ENDSR
092500000000     C/EJECT
092600130827     C************************************************************
092700130827     C* Imposto i dati di output per i Quadri
092800130827     C************************************************************
092900130827     C     RieQuadri     BEGSR
093000130827     C*
093100130827     C                   Select
093200130827     C*
093300130828     C*------------------
093400130828     C* formato Aggregato
093500130828     C*
093600130828     C* SOGGETTI NAZIONALI: FATTURE, NOTE E DOCUMENTI RIEPILOGATIVI
093700130828     C*                     (NOLEGGIO/LEASING)
093800130827     C                   When      SavQuadro   = 'FA'
093900130827     C                   EXSR      RieQuadro_FA
094000130827     C*
094100130828     C* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
094200130827     C                   When      SavQuadro   = 'SA'
094300130827     C                   EXSR      RieQuadro_SA
094400130827     C*
094500130828     C* SOGGETTI NON RESIDENTI E ACQUISTI DI SERVIZI DA NON RESIDENTI
094600130827     C                   When      SavQuadro   = 'BL'
094700130827     C                   EXSR      RieQuadro_BL
094800131017R136 C*
094900131017 "   C* SOGGETTI NON RESIDENTI: OPERAZIONI SENZA FATTURA LEGATE AL
095000131017 "   C*                         TURISMO
095100131017 "   C                   When      SavQuadro   = 'TU'
095200131017R136 C                   EXSR      RieQuadro_TU
095300130828     C*
095400130828     C*------------------
095500130828     C* formato Dettagliato
095600130827     C*
095700130828     C* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI EMESSI
095800130828     C*                     (NOLEGGIO/LEASING)
095900130827     C                   When      SavQuadro   = 'FE'
096000130827     C                   EXSR      RieQuadro_FE
096100130827     C*
096200130828     C* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI RICEVUTI
096300130827     C                   When      SavQuadro   = 'FR'
096400130827     C                   EXSR      RieQuadro_FR
096500130827     C*
096600130828     C* SOGGETTI NAZIONALI: NOTE EMESSE
096700130827     C                   When      SavQuadro   = 'NE'
096800130827     C                   EXSR      RieQuadro_NE
096900130827     C*
097000130828     C* SOGGETTI NAZIONALI: NOTE RICEVUTE
097100130827     C                   When      SavQuadro   = 'NR'
097200130827     C                   EXSR      RieQuadro_NR
097300130827     C*
097400130828     C* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
097500130828     C*                     (NOLEGGIO/LEASING)
097600130827     C                   When      SavQuadro   = 'DF'
097700130827     C                   EXSR      RieQuadro_DF
097800130827     C*
097900130828     C* SOGGETTI NON RESIDENTI
098000130828     C*                     (NOLEGGIO/LEASING)
098100130827     C                   When      SavQuadro   = 'FN'
098200130827     C                   EXSR      RieQuadro_FN
098300130827     C*
098400130828     C* ACQUISTI DI SERVIZI DA NON RESIDENTI
098500130827     C                   When      SavQuadro   = 'SE'
098600130827     C                   EXSR      RieQuadro_SE
098700131017R136 C*
098800131017 "   C* SOGGETTI NON RESIDENTI: OPERAZIONI SENZA FATTURA LEGATE AL
098900131017 "   C*                         TURISMO
099000131017 "   C                   When      SavQuadro   = 'TU'
099100131017R136 C                   EXSR      RieQuadro_TU
099200130827     C*
099300130827     C                   endSl
099400130903     C*
099500130903     C*
099600130903     C* RIEPILOGO
099700130903     C                   EXSR      RieQuadro_TA
099800130827     C*
099900130827     C                   ENDSR
100000130827     C/EJECT
100100130827     C************************************************************
100200130827     C* Imposto i dati di output per il Quadro FA
100300130828     C* formato Aggregato
100400130828     C* SOGGETTI NAZIONALI: FATTURE, NOTE E DOCUMENTI RIEPILOGATIVI
100500130827     C************************************************************
100600130827     C     RieQuadro_FA  BEGSR
100700130827     C*
100800130903     C                   Add       1             $NRCPFATA
100900130903     C*
101000130827     C                   Clear                   SDG005FADS
101100130827     C*
101200130827     C                   Eval      QUADRO05FA  = SavQuadro
101300130902     C                   Eval      PIVA05FA    = SavPIVAR
101400130902     C                   Eval      CDFISC05FA  = SavCdFiscR
101500131014R135 C                   if        SavTpDoc    = 'R'
101600131014 "   C                   Eval      DOCRIE05FA  = '1'
101700131014 "   C                   else
101800131014 "   C                   Eval      DOCRIE05FA  = '0'
101900131014R135 C                   endif
102000130902     C                   Eval      NROPEA05FA  = $NrOpeAFA
102100130902     C                   Eval      NROPEP05FA  = $NrOpePFA
102200130902     C                   Eval      LEASIN05FA  = SavLeasinR
102300130902     C                   Eval      IMPATT05FA  = $ImpAttFA
102400130902     C                   Eval      IVAATT05FA  = $IVAAttFA
102500130902     C                   Eval      MARGIA05FA  = $MargiAFA
102600130902     C                   Eval      IMPNDE05FA  = $ImpNDeFA
102700130902     C                   Eval      IVANDE05FA  = $IVANDeFA
102800130902     C                   Eval      IMPPAS05FA  = $ImpPasFA
102900130902     C                   Eval      IVAPAS05FA  = $IVAPasFA
103000130902     C                   Eval      MARGIP05FA  = $MargiPFA
103100130902     C                   Eval      IMPNCR05FA  = $ImpNCrFA
103200130902     C                   Eval      IVANCR05FA  = $IVANCrFA
103300130902     C*
103400130827     C                   ENDSR
103500130827     C/EJECT
103600130827     C************************************************************
103700130827     C* Imposto i dati di output per il Quadro SA
103800130828     C* formato Aggregato
103900130828     C* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
104000130827     C************************************************************
104100130827     C     RieQuadro_SA  BEGSR
104200130903     C*
104300130903     C                   Add       1             $NRCLSATA
104400130827     C*
104500130827     C                   Clear                   SDG005SADS
104600130827     C*
104700130827     C                   Eval      QUADRO05SA  = SavQuadro
104800130902     C                   Eval      CDFISC05SA  = SavCdFiscR
104900130902     C                   Eval      NROPE05SA   = $NrOpeSA
105000130902     C                   Eval      IMPORT05SA  = $ImportSA
105100130902     C                   Eval      LEASIN05SA  = SavLeasinR
105200130827     C*
105300130827     C                   ENDSR
105400130827     C/EJECT
105500130827     C************************************************************
105600130827     C* Imposto i dati di output per il Quadro BL
105700130828     C* formato Aggregato
105800130828     C* SOGGETTI NON RESIDENTI E ACQUISTI DI SERVIZI DA NON RESIDENTI
105900130827     C************************************************************
106000130827     C     RieQuadro_BL  BEGSR
106100130903     C*
106200160303R195 X****               Z-add     0             $NRBLBLTA
106300130827     C*
106400130827     C                   Clear                   SDG005BLDS
106500130827     C*
106600130827     C                   Eval      QUADRO05BL  = SavQuadro
106700130903     C* persona fisica
106800130904     C                   if        SavTpNR     = '1'
106900130903     C                   Eval      COGRAG05BL  = SavCogRagR
107000130902     C                   Eval      NOME05BL    = SavNomeR
107100130902     C                   Eval      DTNASC05BL  = SavDtNasR
107200130902     C                   Eval      LOCNAS05BL  = SavLocNasR
107300130902     C                   Eval      PRVNAS05BL  = SavPrvNasR
107400130919     C                   Eval      UIC105BL    = SavCodUICR
107500130903     C                   else
107600130903     C* persona non fisica (con natura giuridica)
107700130903     C                   Eval      RAGSOC05BL  = SavCogRagR
107800130902     C                   Eval      CITTAE05BL  = SavLocaliR
107900130919     C                   Eval      UIC205BL    = SavCodUICR
108000130902     C                   Eval      INDIRE05BL  = SavIndirER
108100130903     C                   endif
108200130827     C                   if        SavPartIVA <> *Blank
108300130902     C                   Eval      CIVA05BL    = SavPIVAR
108400130827     C                   else
108500130902     C                   Eval      CIVA05BL    = SavCdFiscR
108600130827     C                   endif
108700130827     C                   Eval      OPEBL05BL   = '0'
108800160104R195 C*
108900160104 "   C* Spesometro
109000160104 "   C                   if        TpCom0502   = 'E'
109100160104R195 C*
109200130902     C                   if        SavNatOpeR  = 'S'     and
109300130925     C                             SavTpOpeR   = 'P'
109400130903     C                   Add       1             $NRSRBLTA
109500130827     C                   Eval      ACQSRV05BL  = '1'
109600130827     C                   Eval      OPENRE05BL  = '0'
109700130827     C                   else
109800130903     C                   Add       1             $NRNRBLTA
109900130827     C                   Eval      ACQSRV05BL  = '0'
110000130827     C                   Eval      OPENRE05BL  = '1'
110100130827     C                   endif
110200160104R195 C*
110300160303 "   C                   else
110400160303 "   C                   Add       1             $NRBLBLTA
110500160303 "   C                   Eval      OPEBL05BL   = '1'
110600160303 "   C                   Eval      ACQSRV05BL  = '0'
110700160303 "   C                   Eval      OPENRE05BL  = '0'
110800160104 "   C                   endif
110900160104R195 C*
111000130902     C                   Eval      IMPATT05BL  = $IMPATTBL
111100130902     C                   Eval      IVAATT05BL  = $IVAATTBL
111200130902     C                   Eval      NOIBAT05BL  = $NOIBATBL
111300130902     C                   Eval      NOISAT05BL  = $NOISATBL
111400130902     C                   Eval      IMPNAT05BL  = $IMPNATBL
111500130902     C                   Eval      IVANAT05BL  = $IVANATBL
111600130902     C                   Eval      IMPPAS05BL  = $IMPPASBL
111700130902     C                   Eval      IVAPAS05BL  = $IVAPASBL
111800130902     C                   Eval      NOIVAP05BL  = $NOIVAPBL
111900130902     C                   Eval      IMPNPA05BL  = $IMPNPABL
112000130902     C                   Eval      IVANPA05BL  = $IVANPABL
112100130827     C*
112200130827     C                   ENDSR
112300130827     C/EJECT
112400130827     C************************************************************
112500130827     C* Imposto i dati di output per il Quadro FE
112600130828     C* formato Dettagliato
112700130828     C* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI EMESSI
112800130828     C*                     (NOLEGGIO/LEASING)
112900130827     C************************************************************
113000130827     C     RieQuadro_FE  BEGSR
113100130827     C*
113200130827     C                   Clear                   SDG005FEDS
113300130827     C*
113400130827     C                   Eval      QUADRO05FE  = SavQuadro
113500130902     C                   Eval      PIVA05FE    = SavPIVAR
113600130902     C                   Eval      CDFISC05FE  = SavCdFiscR
113700130827     C                   if        SavTpDoc    = 'R'
113800130827     C                   Eval      DOCRIE05FE  = '1'
113900130903     C                   Add       1             $NRRIFETA
114000130827     C                   else
114100130827     C                   Eval      DOCRIE05FE  = '0'
114200130903     C                   Add       1             $NRFEFETA
114300130827     C                   endif
114400130902     C                   Eval      MARGIN05FE  = SavMargine
114500130902     C                   Eval      LEASIN05FE  = SavLeasinR
114600130903     C                   Eval      AUTOFA05FE  = SavAutofat
114700130827     C                   Eval      DTDOC05FE   = SavDtDoc
114800130827     C                   Eval      DTREG05FE   = SavDtReg
114900130827     C                   Eval      NRDOC05FE   = SavNrDoc
115000130903     C                   Eval      IMPORT05FE  = $ImportFE
115100130903     C                   Eval      IVA05FE     = $IVAFE
115200130827     C*
115300130827     C                   ENDSR
115400130827     C/EJECT
115500130827     C************************************************************
115600130827     C* Imposto i dati di output per il Quadro FR
115700130828     C* formato Dettagliato
115800130828     C* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI RICEVUTI
115900130827     C************************************************************
116000130827     C     RieQuadro_FR  BEGSR
116100130827     C*
116200130827     C                   Clear                   SDG005FRDS
116300130827     C*
116400130827     C                   Eval      QUADRO05FR  = SavQuadro
116500130902     C                   Eval      PIVA05FR    = SavPIVAR
116600130827     C                   if        SavTpDoc    = 'R'
116700130827     C                   Eval      DOCRIE05FR  = '1'
116800130903     C                   Add       1             $NRRIFRTA
116900130827     C                   else
117000130827     C                   Eval      DOCRIE05FR  = '0'
117100130903     C                   Add       1             $NRFRFRTA
117200130827     C                   endif
117300130827     C                   Eval      DTDOC05FR   = SavDtDoc
117400130827     C                   Eval      DTREG05FR   = SavDtReg
117500130827     C                   Eval      MARGIN05FR  = SavMargine
117600130827     C                   Eval      REVERS05FR  = SavReverse
117700130827     C                   Eval      AUTOFA05FR  = SavAutofat
117800130903     C                   Eval      IMPORT05FR  = $ImportFR
117900130903     C                   Eval      IVA05FR     = $IVAFR
118000130903     C*
118100130827     C                   ENDSR
118200130827     C/EJECT
118300130828     C************************************************************
118400130828     C* Imposto i dati di output per il Quadro NE
118500130828     C* formato Dettagliato
118600130828     C* SOGGETTI NAZIONALI: NOTE EMESSE
118700130828     C************************************************************
118800130828     C     RieQuadro_NE  BEGSR
118900130903     C*
119000130918     C                   Add       1             $NRNONETA
119100130828     C*
119200130828     C                   Clear                   SDG005NEDS
119300130828     C*
119400130828     C                   Eval      QUADRO05NE  = SavQuadro
119500130902     C                   Eval      PIVA05NE    = SavPIVAR
119600130918     C                   Eval      CDFISC05NE  = SavCdFiscR
119700130828     C                   Eval      DTDOC05NE   = SavDtDoc
119800130828     C                   Eval      DTREG05NE   = SavDtReg
119900130828     C                   Eval      NRDOC05NE   = SavNrDoc
120000130903     C                   Eval      IMPORT05NE  = $ImportNE
120100130903     C                   Eval      IVA05NE     = $IVANE
120200130828     C*
120300130828     C                   ENDSR
120400130828     C/EJECT
120500130828     C************************************************************
120600130828     C* Imposto i dati di output per il Quadro NR
120700130828     C* formato Dettagliato
120800130828     C* SOGGETTI NAZIONALI: NOTE RICEVUTE
120900130828     C************************************************************
121000130828     C     RieQuadro_NR  BEGSR
121100130903     C*
121200130903     C                   Add       1             $NRNONRTA
121300130828     C*
121400130828     C                   Clear                   SDG005NRDS
121500130828     C*
121600130828     C                   Eval      QUADRO05NR  = SavQuadro
121700130902     C                   Eval      PIVA05NR    = SavPIVAR
121800130828     C                   Eval      DTDOC05NR   = SavDtDoc
121900130828     C                   Eval      DTREG05NR   = SavDtReg
122000130903     C                   Eval      IMPORT05NR  = $ImportNR
122100130903     C                   Eval      IVA05NR     = $IVANR
122200130828     C*
122300130828     C                   ENDSR
122400130828     C/EJECT
122500130828     C************************************************************
122600130828     C* Imposto i dati di output per il Quadro DF
122700130828     C* formato Dettagliato
122800130828     C* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
122900130828     C*                     (NOLEGGIO/LEASING)
123000130828     C************************************************************
123100130828     C     RieQuadro_DF  BEGSR
123200130903     C*
123300131015R135 X****               Add       1             $NRCLDFTA
123400131015R135 C                   Add       1             $NRRIDFTA
123500130828     C*
123600130828     C                   Clear                   SDG005DFDS
123700130828     C*
123800130828     C                   Eval      QUADRO05DF  = SavQuadro
123900130902     C                   Eval      CDFISC05dF  = SavCdFiscR
124000130828     C                   Eval      DTREG05DF   = SavDtReg
124100130903     C                   Eval      IMPORT05DF  = $ImportDF
124200130902     C                   Eval      LEASIN05DF  = SavLeasinR
124300130828     C*
124400130828     C                   ENDSR
124500130828     C/EJECT
124600130828     C************************************************************
124700130828     C* Imposto i dati di output per il Quadro FN
124800130828     C* formato Dettagliato
124900130828     C* SOGGETTI NON DA NON RESIDENTI
125000130828     C*                     (NOLEGGIO/LEASING)
125100130828     C************************************************************
125200130828     C     RieQuadro_FN  BEGSR
125300130903     C*
125400130903     C                   Add       1             $NRRIFNTA
125500130828     C*
125600130828     C                   Clear                   SDG005FNDS
125700130828     C*
125800130828     C                   Eval      QUADRO05FN  = SavQuadro
125900130903     C* persona fisica
126000130904     C                   if        SavTpNR     = '1'
126100130903     C                   Eval      COGRAG05FN  = SavCogRagR
126200130902     C                   Eval      NOME05FN    = SavNomeR
126300130902     C                   Eval      DTNASC05FN  = SavDtNasR
126400130902     C                   Eval      LOCNAS05FN  = SavLocNasR
126500130902     C                   Eval      PRVNAS05FN  = SavPrvNasR
126600130919     C                   Eval      UIC105FN    = SavCodUICR
126700130903     C                   else
126800130903     C* persona non fisica (con natura giuridica)
126900130903     C                   Eval      RAGSOC05FN  = SavCogRagR
127000130902     C                   Eval      CITTAE05FN  = SavLocaliR
127100130919     C                   Eval      UIC205FN    = SavCodUICR
127200130902     C                   Eval      INDIRE05FN  = SavIndirER
127300130903     C                   endif
127400130828     C                   Eval      DTDOC05FN   = SavDtDoc
127500130828     C                   Eval      DTREG05FN   = SavDtReg
127600130828     C                   Eval      NRDOC05FN   = SavNrDoc
127700130902     C                   Eval      LEASIN05FN  = SavLeasinR
127800130903     C                   Eval      IMPORT05FN  = $ImportFN
127900130903     C                   Eval      IVA05FN     = $IVAFN
128000130828     C*
128100130828     C                   ENDSR
128200130828     C/EJECT
128300130828     C************************************************************
128400130828     C* Imposto i dati di output per il Quadro SE
128500130828     C* formato Dettagliato
128600130828     C* ACQUISTI DI SERVIZI DA NON RESIDENTI
128700130828     C************************************************************
128800130828     C     RieQuadro_SE  BEGSR
128900130903     C*
129000130903     C                   Add       1             $NRRISETA
129100130828     C*
129200130828     C                   Clear                   SDG005SEDS
129300130828     C*
129400130828     C                   Eval      QUADRO05SE  = SavQuadro
129500130903     C* persona fisica
129600130904     C                   if        SavTpNR     = '1'
129700130903     C                   Eval      COGRAG05SE  = SavCogRagR
129800130902     C                   Eval      NOME05SE    = SavNomeR
129900130902     C                   Eval      DTNASC05SE  = SavDtNasR
130000130902     C                   Eval      LOCNAS05SE  = SavLocNasR
130100130902     C                   Eval      PRVNAS05SE  = SavPrvNasR
130200130919     C                   Eval      UIC105SE    = SavCodUICR
130300130903     C                   else
130400130903     C* persona non fisica (con natura giuridica)
130500130903     C                   Eval      RAGSOC05SE  = SavCogRagR
130600130902     C                   Eval      CITTAE05SE  = SavLocaliR
130700130919     C                   Eval      UIC205SE    = SavCodUICR
130800130902     C                   Eval      INDIRE05SE  = SavIndirER
130900130903     C                   endif
131000130828     C                   if        SavPartIVA <> *Blank
131100130902     C                   Eval      CIVA05SE    = SavPIVAR
131200130828     C                   else
131300130902     C                   Eval      CIVA05SE    = SavCdFiscR
131400130828     C                   endif
131500130828     C                   Eval      DTDOC05SE   = SavDtDoc
131600130828     C                   Eval      DTREG05SE   = SavDtReg
131700130828     C                   Eval      NRDOC05SE   = SavNrDoc
131800130903     C                   Eval      IMPORT05SE  = $ImportSE
131900130903     C                   Eval      IVA05SE     = $IVASE
132000130828     C*
132100130828     C                   ENDSR
132200130828     C/EJECT
132300131017R136 C************************************************************
132400131017 "   C* Imposto i dati di output per il Quadro TU
132500131017 "   C* formato Dettagliato
132600131017 "   C* OPERAZIONI LEGATE AL TURISMO - SOGGETTI NON RESIDENTI
132700131017 "   C************************************************************
132800131017R136 C     RieQuadro_TU  BEGSR
132900131017     C*
133000131108R140 X****               if        SavCogRagR <> Sa2CogRagR   or
133100131108 "   X****                         SavNomeR   <> Sa2NomeR     or
133200131108 "   X****                         SavDtNasR  <> Sa2DtNasR    or
133300131108 "   X****                         SavLocNasR <> Sa2LocNasR   or
133400131108 "   X****                         SavPrvNasR <> Sa2PrvNasR   or
133500131108 "   X****                         SavLocaliR <> Sa2LocaliR   or
133600131108 "   X****                         SavCodUICR <> Sa2CodUICR   or
133700131108R140 X****                         SavIndirER <> Sa2IndirER
133800131017     C*
133900131017     C                   Add       1             $NRCLTUTA
134000131017     C*
134100131108R140 X****               Eval      Sa2CogRagR  = SavCogRagR
134200131108 "   X****               Eval      Sa2NomeR    = SavNomeR
134300131108 "   X****               Eval      Sa2DtNasR   = SavDtNasR
134400131108 "   X****               Eval      Sa2LocNasR  = SavLocNasR
134500131108 "   X****               Eval      Sa2PrvNasR  = SavPrvNasR
134600131108 "   X****               Eval      Sa2LocaliR  = SavLocaliR
134700131108 "   X****               Eval      Sa2CodUICR  = SavCodUICR
134800131108 "   X****               Eval      Sa2IndirER  = SavIndirER
134900131108R140 X****               endif
135000131017     C*
135100131017     C                   Clear                   SDG005TUDS
135200131017     C*
135300131017     C                   Eval      QUADRO05TU  = SavQuadro
135400131017     C* persona fisica
135500131017     C                   if        SavTpNR     = '1'
135600131017     C                   Eval      COGRAG05TU  = SavCogRagR
135700131017     C                   Eval      NOME05TU    = SavNomeR
135800131017     C                   Eval      DTNASC05TU  = SavDtNasR
135900131017     C                   Eval      LOCNAS05TU  = SavLocNasR
136000131017     C                   Eval      PRVNAS05TU  = SavPrvNasR
136100131017     C                   Eval      CITTAE05TU  = SavLocaliR
136200131017     C                   Eval      UIC05TU     = SavCodUICR
136300131017     C                   Eval      INDIRE05TU  = SavIndirER
136400131017     C                   else
136500131017     C* persona non fisica (con natura giuridica)
136600131017     C                   endif
136700131017     C                   Eval      DTDOC05TU   = SavDtDoc
136800131017     C                   Eval      DTREG05TU   = SavDtReg
136900131017     C                   Eval      NRDOC05TU   = SavNrDoc
137000131017     C                   Eval      IMPORT05TU  = $ImportTU
137100131017     C                   Eval      IVA05TU     = $IVATU
137200131017     C*
137300131017R136 C                   ENDSR
137400131017R136 C/EJECT
137500000000     C************************************************************
137600000000     C* Gestione rottura per dettaglio
137700000000     C************************************************************
137800000000     C     Rott_03       BEGSR
137900000000     C*
138000130828     C* campi di TESTATA
138100130925     C                   if        SCFPartIVA <> SavPartIVA   or
138200130925     C                             SCFCdFisc  <> SavCdFisc    or
138300130925     C                             SCFCOGRAG  <> SavCOGRAG    or
138400131017     C                             SCFNOME    <> SavNOME      or
138500130925     C                             SCFDTNASC  <> SavDTNASC    or
138600130925     C                             SCFLOCNASC <> SavLOCNASC   or
138700130925     C                             SCFPRVNASC <> SavPRVNASC   or
138800130925     C                             SCFSTATO   <> SavSTATO     or
138900130925     C                             SCFCODUIC  <> SavCODUIC    or
139000130925     C                             SCFLOCALIT <> SavLOCALIT   or
139100130925     C                             SCFINDIRE  <> SavINDIRE    or
139200130826     C* campi per QUADRI
139300130828     C                             SCFQUADRO  <> SavQUADRO    or
139400130828     C                             SCFCOGRAGR <> SavCOGRAGR   or
139500130828     C                             SCFNOMER   <> SavNOMER     or
139600130828     C                             SCFDTNASR  <> SavDTNASR    or
139700130828     C                             SCFLOCNASR <> SavLOCNASR   or
139800130828     C                             SCFPRVNASR <> SavPRVNASR   or
139900130828     C                             SCFSTATOR  <> SavSTATOR    or
140000130828     C                             SCFCODUICR <> SavCODUICR   or
140100130828     C                             SCFLOCALIR <> SavLOCALIR   or
140200130828     C                             SCFINDIRER <> SavINDIRER   or
140300130903     C                             SCFTpNR    <> SavTpNR      or
140400130903     C                             SCFPIVAR   <> SavPIVAR     or
140500130828     C                             SCFCDFISCR <> SavCDFISCR   or
140600130925     C                             SCFTpOpeR  <> SavTpOpeR    or
140700130925     C                             SCFNATOPER <> SavNATOPER   or
140800130902     C                             SCFLEASINR <> SavLEASINR   or
140900130828     C                             SCFDTREGR  <> SavDTREGR    or
141000130828     C                             SCFNRREGR  <> SavNRREGR    or
141100130828     C                             SCFSERREGR <> SavSERREGR   or
141200130828     C* campi solo per DETTAGLIO di registrazione
141300130828     C                             SCFOrigine <> SavOrigine   or
141400130828     C                             SCFTPDOC   <> SavTpDoc     or
141500130828     C                             SCFTPOPE   <> SavTpOpe     or
141600130828     C                             SCFNOIVA   <> SavNoIVA     or
141700130828     C                             SCFMARGINE <> SavMargine   or
141800130828     C                             SCFREVERSE <> SavReverse   or
141900130828     C                             SCFAUTOFAT <> SavAutofat   or
142000130828     C                             SCFVARNOTA <> SavVarNota   or
142100130828     C                             SCFCF      <> SavCF        or
142200130828     C                             SCFSogg    <> SavSogg      or
142300130828     C                             SCFKcc     <> SavKcc       or
142400130826     C                             SCFKsc     <> SavKsc       or
142500130826     C                             SCFCausale <> SavCausale   or
142600130826     C                             SCFAliq    <> SavAliq      or
142700130826     C                             SCFRifIVA  <> SavRifIVA    or
142800130826     C                             SCFDtDoc   <> SavDtDoc     or
142900130826     C                             SCFNrDoc   <> SavNrDoc     or
143000130826     C                             SCFSerDoc  <> SavSerDoc    or
143100130826     C                             SCFDtReg   <> SavDtReg     or
143200130826     C                             SCFNrReg   <> SavNrReg     or
143300130826     C                             SCFSerReg  <> SavSerReg    or
143400130826     C                             SCFDtPar   <> SavDtPar     or
143500130826     C                             SCFNrPar   <> SavNrPar     or
143600130826     C                             SCFSerPar  <> SavSerPar    or
143700130826     C                             SCFSys     <> SavSys       or
143800130826     C                             SCFNrAsReg <> SavNrAsReg   or
143900130826     C                             SCFNrRigaM <> SavNrRigaM   or
144000130826     C                             SCFNrRiga  <> SavNrRiga
144100000000     C*
144200000000     C                   Add       1             $NrTrovati
144300000000     C*
144400000000     C                   if        $PrimaLett <> *On
144500000000     C*
144600130826     C                   if        Opz0502     = '05'
144700130827     C                   EXSR      RieOutput
144800000000     C                   endif
144900000000     C*
145000000000     C                   endif
145100000000     C*
145200130827     C                   EXSR      RieSavRcd
145300000000     C*
145400000000     C                   if        $PrimaLett <> *On
145500000000     C*
145600130826     C                   if        Opz0502     = '05'
145700130827     C                   EXSR      ENDPGM
145800000000     C                   endif
145900000000     C*
146000000000     C                   endif
146100000000     C*
146200000000     C                   endif
146300000000     C*
146400130826     C                   if        Opz0502     = '05'
146500130827     C                   EXSR      AddImporti
146600130827     C                   EXSR      UpdAnnAtt
146700000000     C                   endif
146800000000     C*
146900000000     C                   ENDSR
147000000000     C/EJECT
147100000000     C************************************************************
147200000000     C* Incremento gli importi
147300000000     C************************************************************
147400000000     C     AddImporti    BEGSR
147500130903     C*
147600130904     C                   if        SCFDTREG   <> SavDTREG2    or
147700130904     C                             SCFNRREG   <> SavNRREG2    or
147800130904     C                             SCFSERREG  <> SavSERREG2
147900130904     C*
148000130904     C                   Eval      SavDTREG2   = SCFDTREG
148100130904     C                   Eval      SavNRREG2   = SCFNRREG
148200130904     C                   Eval      SavSERREG2  = SCFSERREG
148300130903     C*
148400130925     C                   if        SCFNrOpe    > 0
148500130925     C                   Add       SCFNrOpe      $NrOpeSA
148600130925     C                   else
148700130925     C                   Add       1             $NrOpeSA
148800130925     C                   endif
148900131205R141 C*
149000131205 "   C                   Select
149100131205 "   C*
149200131205 "   C* Non è nota oppure è gestione note con metodo 1
149300131205 "   C                   When      NoteVa0502  = '1'     or
149400131205R141 C                             SCFTpDoc   <> 'N'
149500130903     C*
149600130903     C                   if        SCFTpOpe    = 'A'
149700130903     C*
149800130925     C                   if        SCFNrOpe    > 0
149900130925     C                   Add       SCFNrOpe      $NrOpeAFA
150000130925     C                   else
150100130903     C                   Add       1             $NrOpeAFA
150200130925     C                   endif
150300130903     C*
150400130903     C                   else
150500130903     C*
150600130925     C                   if        SCFNrOpe    > 0
150700130925     C                   Add       SCFNrOpe      $NrOpePFA
150800130925     C                   else
150900130903     C                   Add       1             $NrOpePFA
151000130925     C                   endif
151100130903     C*
151200130903     C                   endif
151300131205R141 C*
151400131205 "   C* è nota ed è gestione note con metodo 2
151500131205 "   C                   When      NoteVa0502  = '2'     and
151600131205 "   C                             SCFTpDoc    = 'N'
151700131205 "   C*
151800131205 "   C* A Debito  per la controparte
151900131205 "   C                   if        SCFVarNota  = 'D'
152000131205 "   C*
152100131205 "   C                   if        SCFNrOpe    > 0
152200131205 "   C                   Add       SCFNrOpe      $NrOpeAFA
152300131205 "   C                   else
152400131205 "   C                   Add       1             $NrOpeAFA
152500131205 "   C                   endif
152600131205 "   C*
152700131205 "   C                   else
152800131205 "   C* A Credito per la controparte
152900131205 "   C*
153000131205 "   C                   if        SCFNrOpe    > 0
153100131205 "   C                   Add       SCFNrOpe      $NrOpePFA
153200131205 "   C                   else
153300131205 "   C                   Add       1             $NrOpePFA
153400131205 "   C                   endif
153500131205 "   C*
153600131205 "   C                   endif
153700131205 "   C*
153800131205R141 C                   endSl
153900130903     C*
154000130903     C                   endif
154100000000     C*
154200130903     C*
154300130826     C                   Add       SCFImpOpe     SavImpOpe
154400130826     C                   Add       SCFIVAOpe     SavIVAOpe
154500131015R135 X****
154600131015 "   X****
154700131015 "   X**** ATTENZIONE
154800131015 "   X**** gli importi per i quadri FA sono stati interpretati in
154900131015 "   X**** questo modo:
155000131015 "   X****
155100131015 "   X**** - FA001006 e FA001007 Totale delle operazioni imponibili,
155200131015 "   X****   non imponibili ed esenti
155300131015 "   X****   ==> vengono sommati tutti gli importi, sia fatture che note
155400131015 "   X****       (con IVA non esposta e IVA esposta)
155500131015 "   X****
155600131015 "   X**** - FA001008 Operazioni con IVA non esposta
155700131015 "   X****   ==> vengono sommati tutti gli importi, sia fatture che note
155800131015 "   X****       con IVA non esposta
155900131015 "   X****
156000131015 "   X**** - FA001009  e FA001010 Totale note di variazione a Credito
156100131015 "   X****       /Debito per la controparte
156200131015 "   X****   ==> vengono sommati tutti gli importi, relativi a note, sia
156300131015R135 X****       a Credito che a Debito per la controparte
156400131015R135 C*
156500131015 "   C*
156600131015 "   C* ATTENZIONE
156700131015 "   C* gli importi per i quadri FA sono stati interpretati in
156800131015 "   C* questo modo:
156900131015 "   C*
157000131015 "   C* - FA001006 e FA001007 Totale delle operazioni imponibili,
157100131015 "   C*   non imponibili ed esenti
157200131015 "   C*   ==> vengono sommati tutti gli importi relativi a fatture
157300131015 "   C*       nornmali         ta
157400131015 "   C*       (no note nè operazioni con IVA non esposta)
157500131015 "   C*
157600131015 "   C* - FA001008 Operazioni con IVA non esposta
157700131015 "   C*   ==> vengono sommati tutti gli importi relativi a fatture
157800131015 "   C*       con IVA non esposta
157900131015 "   C*       (no note nè operazioni con fatture normali)
158000131015 "   C*
158100131015 "   C* - FA001009  e FA001010 Totale note di variazione a Credito
158200131015 "   C*       /Debito per la controparte
158300131015 "   C*   ==> vengono sommati tutti gli importi, relativi a note, sia
158400131015 "   C*       a Credito che a Debito per la controparte
158500131015R135 C*       (sia fatture normali che con IVA non esposta)
158600130930     C*
158700130930     C*
158800131205R141 C*
158900131205 "   C                   Select
159000131205 "   C*
159100131205 "   C* Non è nota oppure è gestione note con metodo 1
159200131205 "   C                   When      NoteVa0502  = '1'     or
159300131205R141 C                             SCFTpDoc   <> 'N'
159400130903     C* Attive
159500130902     C                   if        SCFTpOpe    = 'A'
159600130930     C*
159700131015R135 X****               if        SCFMargine  = '1'
159800131015 "   X****               Add       SCFImpOpe     $MargiAFA
159900131015 "   X****               Add       SCFIVAOpe     $MargiAFA
160000131015 "   X****               Add       SCFImpOpe     $ImpAttFA
160100131015 "   X****               Add       SCFIVAOpe     $ImpAttFA
160200131015 "   X****               else
160300131015 "   X****               Add       SCFImpOpe     $ImpAttFA
160400131015 "   X****               Add       SCFIVAOpe     $IVAAttFA
160500131015R135 X****               endif
160600130902     C*
160700130903     C* Nota
160800130902     C                   if        SCFTpDoc    = 'N'
160900130930     C*
161000130930     C                   if        SCFMargine  = '1'
161100130930     C                   Add       SCFImpOpe     $ImpNDeFA
161200130930     C                   Add       SCFIVAOpe     $ImpNDeFA
161300130930     C                   Add       SCFImpOpe     $ImportNE
161400130930     C                   Add       SCFIVAOpe     $ImportNE
161500130930     C                   else
161600130930     C                   Add       SCFImpOpe     $ImpNDeFA
161700130930     C                   Add       SCFIVAOpe     $IVANDeFA
161800130930     C                   Add       SCFImpOpe     $ImportNE
161900130930     C                   Add       SCFIVAOpe     $IVANE
162000130930     C                   endif
162100130902     C*
162200130902     C                   else
162300130903     C* Fattura
162400130902     C                   if        SCFMargine  = '1'
162500130903     C                   Add       SCFImpOpe     $ImportFE
162600130903     C                   Add       SCFIVAOpe     $ImportFE
162700131015R135 C                   Add       SCFImpOpe     $MargiAFA
162800131015R135 C                   Add       SCFIVAOpe     $MargiAFA
162900130902     C                   else
163000130903     C                   Add       SCFImpOpe     $ImportFE
163100130903     C                   Add       SCFIVAOpe     $IVAFE
163200131015R135 C                   Add       SCFImpOpe     $ImpAttFA
163300131015R135 C                   Add       SCFIVAOpe     $IVAAttFA
163400130902     C                   endif
163500130902     C*
163600130902     C                   endif
163700130902     C*
163800130902     C                   else
163900130903     C* Passive
164000130930     C*
164100131015R135 X****               if        SCFMargine  = '1'
164200131015 "   X****               Add       SCFImpOpe     $MargiPFA
164300131015 "   X****               Add       SCFIVAOpe     $MargiPFA
164400131015 "   X****               Add       SCFImpOpe     $ImpPasFA
164500131015 "   X****               Add       SCFIVAOpe     $ImpPasFA
164600131015 "   X****               else
164700131015 "   X****               Add       SCFImpOpe     $ImpPasFA
164800131015 "   X****               Add       SCFIVAOpe     $IVAPasFA
164900131015R135 X****               endif
165000130930     C*
165100130930     C* Nota
165200130930     C                   if        SCFTpDoc    = 'N'
165300130930     C*
165400130930     C                   if        SCFMargine  = '1'
165500130930     C                   Add       SCFImpOpe     $ImpNCrFA
165600130930     C                   Add       SCFIVAOpe     $ImpNCrFA
165700130930     C                   Add       SCFImpOpe     $ImportNR
165800130930     C                   Add       SCFIVAOpe     $ImportNR
165900130930     C                   else
166000130930     C                   Add       SCFImpOpe     $ImpNCrFA
166100130930     C                   Add       SCFIVAOpe     $IVANCrFA
166200130930     C                   Add       SCFImpOpe     $ImportNR
166300130930     C                   Add       SCFIVAOpe     $IVANR
166400130930     C                   endif
166500130930     C*
166600130930     C                   else
166700130930     C* Fattura
166800130930     C                   if        SCFMargine  = '1'
166900130930     C                   Add       SCFImpOpe     $ImportFR
167000130930     C                   Add       SCFIVAOpe     $ImportFR
167100131015R135 C                   Add       SCFImpOpe     $MargiPFA
167200131015R135 C                   Add       SCFIVAOpe     $MargiPFA
167300130930     C                   else
167400130930     C                   Add       SCFImpOpe     $ImportFR
167500130930     C                   Add       SCFIVAOpe     $IVAFR
167600131015R135 C                   Add       SCFImpOpe     $ImpPasFA
167700131015R135 C                   Add       SCFIVAOpe     $IVAPasFA
167800130930     C                   endif
167900130930     C*
168000130930     C                   endif
168100130902     C*
168200130902     C                   endif
168300131205R141 C*
168400131205 "   C* è nota ed è gestione note con metodo 2
168500131205 "   C                   When      NoteVa0502  = '2'     and
168600131205 "   C                             SCFTpDoc    = 'N'
168700131205 "   C* Attive
168800131205 "   C                   if        SCFTpOpe    = 'A'
168900131205 "   C*
169000131205 "   C                   if        SCFMargine  = '1'
169100131205 "   C                   Add       SCFImpOpe     $ImportNE
169200131205 "   C                   Add       SCFIVAOpe     $ImportNE
169300131205 "   C                   else
169400131205 "   C                   Add       SCFImpOpe     $ImportNE
169500131205 "   C                   Add       SCFIVAOpe     $IVANE
169600131205 "   C                   endif
169700131205 "   C*
169800131205 "   C                   else
169900131205 "   C*
170000131205 "   C* Passive
170100131205 "   C                   if        SCFMargine  = '1'
170200131205 "   C                   Add       SCFImpOpe     $ImportNR
170300131205 "   C                   Add       SCFIVAOpe     $ImportNR
170400131205 "   C                   else
170500131205 "   C                   Add       SCFImpOpe     $ImportNR
170600131205 "   C                   Add       SCFIVAOpe     $IVANR
170700131205 "   C                   endif
170800131205 "   C*
170900131205 "   C                   endif
171000131205 "   C*
171100131205 "   C* A Debito  per la controparte
171200131205 "   C                   if        SCFVarNota  = 'D'
171300131205 "   C*
171400131205 "   C                   Eval      $ImpNDeFA   = $ImpNDeFA + %Abs(SCFImpOpe)
171500131205 "   C                   Eval      $IVANDeFA   = $IVANDeFA + %Abs(SCFIVAOpe)
171600131205 "   C*
171700131205 "   C                   else
171800131205 "   C* A Credito per la controparte
171900131205 "   C                   Eval      $ImpNCrFA   = $ImpNCrFA + %Abs(SCFImpOpe)
172000131205 "   C                   Eval      $IVANCrFA   = $IVANCrFA + %Abs(SCFIVAOpe)
172100131205 "   C*
172200131205 "   C                   endif
172300131205 "   C*
172400131205R141 C                   endSl
172500130902     C*
172600130902     C                   Add       SCFImpOpe     $ImportSA
172700130902     C                   Add       SCFIVAOpe     $ImportSA
172800000000     C*
172900160105R195 C                   Select
173000160105 "   C* Spesometro
173100160105R195 C                   When      TpCom0502   = 'E'
173200131205R141 C*
173300131205 "   C                   Select
173400131205 "   C*
173500131205 "   C* Non è nota oppure è gestione note con metodo 1
173600131205 "   C                   When      NoteVa0502  = '1'     or
173700131205R141 C                             SCFTpDoc   <> 'N'
173800130902     C                   if        SCFTpOpe    = 'A'
173900130923     C                   Add       SCFImpOpe     $IMPATTBL
174000130902     C                   Add       SCFIVAOpe     $IVAATTBL
174100130902     C**********         Add                     $NOIBATBL
174200130902     C**********         Add                     $NOISATBL
174300130902     C**********         Add                     $IMPNATBL
174400130902     C**********         Add                     $IVANATBL
174500130902     C                   else
174600130923     C                   Add       SCFImpOpe     $IMPPASBL
174700130902     C                   Add       SCFIVAOpe     $IVAPASBL
174800130902     C**********         Add                     $NOIVAPBL
174900130902     C**********         Add                     $IMPNPABL
175000130902     C**********         Add                     $IVANPABL
175100130902     C                   endif
175200131205R141 C*
175300131205 "   C* è nota ed è gestione note con metodo 2
175400131205 "   C                   When      NoteVa0502  = '2'     and
175500131205 "   C                             SCFTpDoc    = 'N'
175600131205 "   C*
175700131205 "   C* A Debito  per la controparte
175800131205 "   C                   if        SCFVarNota  = 'D'
175900131205 "   C                   Eval      $IMPATTBL   = $IMPATTBL + %Abs(SCFImpOpe)
176000131205 "   C                   Eval      $IVAATTBL   = $IVAATTBL + %Abs(SCFIVAOpe)
176100131205 "   C                   else
176200131205 "   C* A Credito per la controparte
176300131205 "   C                   Eval      $IMPPASBL   = $IMPPASBL + %Abs(SCFImpOpe)
176400131205 "   C                   Eval      $IVAPASBL   = $IVAPASBL + %Abs(SCFIVAOpe)
176500131205 "   C                   endif
176600131205 "   C*
176700131205R141 C                   endSl
176800160105R195 C*
176900160105 "   C* ---------------
177000160105 "   C* Black List
177100160105 "   C                   When      TpCom0502   = 'B'
177200160105 "   C*
177300160105 "   C                   Select
177400160105 "   C*
177500160105 "   C* Non è nota oppure è gestione note con metodo 1
177600160105 "   C                   When      NoteVa0502  = '1'     or
177700160105 "   C                             SCFTpDoc   <> 'N'
177800160105 "   C*
177900160105 "   C* ATTIVE
178000160105 "   C                   if        SCFTpOpe    = 'A'
178100160105 "   C*
178200160105 "   C                   Select
178300160105 "   C*
178400160105 "   C* BL003 - Operazioni imponbili, non imponibili e esenti
178500160105 "   C                   When      SCFTpDoc   <> 'N'     and
178600160105 "   C                             (SCFTpImp    = 'I'     or
178700160105 "   C                              SCFTpImp    = 'N'     or
178800160105 "   C                              SCFTpImp    = 'E'       )
178900160105 "   C*
179000160105 "   C                   Add       SCFImpOpe     $IMPAttBL
179100160105 "   C                   Add       SCFIVAOpe     $IVAAttBL
179200160105 "   C*
179300160105 "   C* BL004 - Operazioni non soggette ad IVA
179400160105 "   C                   When      SCFTpDoc   <> 'N'     and
179500160105 "   C                             SCFTpImp    = 'F'
179600160105 "   C                   if        SCFNatOpe   = 'B'
179700160105 "   C                   Add       SCFImpOpe     $NOIBAtBL
179800160105 "   C                   else
179900160105 "   C                   Add       SCFImpOpe     $NOISAtBL
180000160105 "   C                   endif
180100160105 "   C*
180200160105 "   C* BL005 - Note di variazione
180300160105 "   C                   When      SCFTpDoc    = 'N'
180400160105 "   C                   Add       SCFImpOpe     $IMPNATBL
180500160105 "   C                   Add       SCFIVAOpe     $IVANATBL
180600160105 "   C                   endSl
180700160105 "   C*
180800160105 "   C                   else
180900160105 "   C* PASSIVE
181000160105 "   C                   Select
181100160105 "   C*
181200160105 "   C* BL003 - Operazioni imponibili, non imponibili e esenti
181300160105 "   C                   When      SCFTpDoc   <> 'N'     and
181400160105 "   C                             (SCFTpImp    = 'I'     or
181500160105 "   C                              SCFTpImp    = 'N'     or
181600160105 "   C                              SCFTpImp    = 'E'       )
181700160105 "   C*
181800160105 "   C                   Add       SCFImpOpe     $IMPPasBL
181900160105 "   C                   Add       SCFIVAOpe     $IVAPasBL
182000160105 "   C*
182100160105 "   C* BL004 - Operazioni non soggette ad IVA
182200160105 "   C                   When      SCFTpDoc   <> 'N'     and
182300160105 "   C                             SCFTpImp    = 'F'
182400160105 "   C                   Add       SCFImpOpe     $NOIVAPBL
182500160105 "   C*
182600160105 "   C* BL005 - Note di variazione
182700160105 "   C                   When      SCFTpDoc    = 'N'
182800160105 "   C                   Add       SCFImpOpe     $IMPNPaBL
182900160105 "   C                   Add       SCFIVAOpe     $IVANPaBL
183000160105 "   C                   endSl
183100160105 "   C*
183200160105 "   C                   endif
183300160105 "   C*
183400160105 "   C*
183500160105 "   C* è nota ed è gestione note con metodo 2
183600160105 "   C                   When      NoteVa0502  = '2'     and
183700160105 "   C                             SCFTpDoc    = 'N'
183800160105 "   C*
183900160105 "   C* A Debito  per la controparte
184000160105 "   C                   if        SCFVarNota  = 'D'
184100160105 "   C                   Eval      $ImpNAtBL   = $ImpNAtBL + %Abs(SCFImpOpe)
184200160105 "   C                   Eval      $IVANAtBL   = $IVANAtBL + %Abs(SCFIVAOpe)
184300160105 "   C                   else
184400160105 "   C* A Credito per la controparte
184500160105 "   C                   Eval      $ImpNPaBL   = $ImpNPaBL + %Abs(SCFImpOpe)
184600160105 "   C                   Eval      $IVANPaBL   = $IVANPaBL + %Abs(SCFIVAOpe)
184700160105 "   C                   endif
184800160105 "   C*
184900160105 "   C                   endSl
185000160105 "   C*
185100160105R195 C                   endSl
185200130903     C*
185300130903     C                   Add       SCFImpOpe     $ImportDF
185400130903     C                   Add       SCFIVAOpe     $ImportDF
185500130906     C*
185600130906     C                   Add       SCFImpOpe     $ImportFN
185700131017R136 X****               Add       SCFIVAOpe     $ImportFN
185800131017R136 C                   Add       SCFIVAOpe     $IVAFN
185900130906     C*
186000130906     C                   Add       SCFImpOpe     $ImportSE
186100131017R136 X****               Add       SCFIVAOpe     $ImportSE
186200131017R136 C                   Add       SCFIVAOpe     $IVASE
186300131017 "   C*
186400131017 "   C                   Add       SCFImpOpe     $ImportTU
186500131017R136 C                   Add       SCFIVAOpe     $IVATU
186600130902     C*
186700000000     C                   ENDSR
186800000000     C/EJECT
186900000000     C************************************************************
187000000000     C* Aggiorno il flag di raggruppamento attivo/annullato
187100000000     C************************************************************
187200000000     C     UpdAnnAtt     BEGSR
187300000000     C*
187400110824     C                   Add       1             $NrRcdLett
187500130826     C                   if        SCFAnn      = '1'
187600000000     C                   Eval      SavAnnAtt   = '1'
187700110824     C                   Add       1             $NrRcdAnn
187800000000     C                   endif
187900000000     C*
188000000000     C                   ENDSR
188100000000     C/EJECT
188200000000     C************************************************************
188300130826     C* Riempio campi descrittivi di SDGSCF00F
188400000000     C************************************************************
188500130826     C     RieSDGSCF     BEGSR
188600000000     C*
188700130826     C                   Eval      SCFORIGINE  = 'M'
188800130826     C                   Eval      SCFUTEULMO  = XScCUt
188900130826     C                   Eval      SCFDTULMO   = UDateIso
189000000000     C*
189100130826     C                   Eval      SCFPARTIVA  = PIVA0592
189200130826     C                   Eval      SCFCDFISC   = CdFisc0592
189300130826     C                   Eval      SCFCOGRAG   = CogRag0592
189400130826     C                   Eval      SCFNOME     = Nome0592
189500130826     C                   Eval      SCFDTNASC   = DtNasc0592
189600130826     C                   Eval      SCFLOCNASC  = LocNas0592
189700130826     C                   Eval      SCFPRVNASC  = PrvNas0592
189800000000     C*
189900130826     C                   Eval      SCFSTATO    = Stato0592
190000130826     C                   Eval      SCFCodUIC   = CodUIC0592
190100000000     C*
190200130826     C                   Eval      SCFLOCALIT  = Locali0592
190300130826     C                   Eval      SCFINDIRE   = IndirE0592
190400130912     C*
190500160104R195 C                   Select
190600160104 "   C* Spesometro
190700160104R195 C                   When      TpCom0502   = 'E'
190800130912     C                   EXSR      DefQuadri
190900160104R195 C*
191000160104 "   C* Acquisti da San Marino
191100160104 "   C                   When      TpCom0502   = 'S'
191200160104 "   C                   EXSR      DefQuadroSE
191300160104 "   C*
191400160104 "   C* Black List
191500160104 "   C                   When      TpCom0502   = 'B'
191600160104 "   C                   EXSR      DefQuadroBL
191700160104 "   C                   endSl
191800160104R195 C*
191900000000     C*
192000000000     C                   ENDSR
192100000000     C/EJECT
192200130912     C************************************************************
192300130912     C* Definizione dati per Quadri
192400130912     C************************************************************
192500130912     C     DefQuadri     BEGSR
192600130912     C*
192700130918     C                   Eval      SCFFormato  = FORMAT0502
192800130912     C*
192900130912     C                   Select
193000130912     C*
193100130912     C*--------------------
193200130912     C* Formato AGGREGATO
193300130918     C                   When      SCFFormato  = 'A'
193400130912     C                   Select
193500130912     C*-------
193600130912     C* se conto Nazionale e documento Fattura, Nota o Riepilogo
193700160104R195 X****               When      SCFCodUIC   = UICSoc0502    and
193800160104 "   X****                         (SCFTpDoc    = 'F'       or
193900160104 "   X****                          SCFTpDoc    = 'N'       or
194000160104R195 X****                          SCFTpDoc    = 'R'         )
194100160104R195 C*
194200160104 "   C* in caso di Autofattura, deve finire in FA perchè andrà
194300160104 "   C* girata sull'azienda comunicante
194400160104 "   C                   When      (SCFCodUIC   = UICSoc0502    and
194500160104 "   C                              (SCFTpDoc    = 'F'       or
194600160104 "   C                               SCFTpDoc    = 'N'       or
194700160104 "   C                               SCFTpDoc    = 'R'         )   ) or
194800160104R195 C                             SCFAutofat  = *On
194900130912     C                   Eval      SCFQuadro   = 'FA'
195000130912     C                   Eval      SCFSeqQua   =  01
195100130912     C*
195200130912     C*
195300130912     C                   Eval      SCFCOGRAGR  = *Blank
195400130912     C                   Eval      SCFNOMER    = *Blank
195500130912     C                   Eval      SCFDTNASR   = *Loval
195600130912     C                   Eval      SCFLOCNASR  = *Blank
195700130912     C                   Eval      SCFPRVNASR  = *Blank
195800130912     C                   Eval      SCFSTATOR   = *Blank
195900130918     C                   Eval      SCFCODUICR  = 0
196000130912     C                   Eval      SCFLOCALIR  = *Blank
196100130912     C                   Eval      SCFINDIRER  = *Blank
196200130924     C                   Eval      SCFTpNR     = *Blank
196300130912     C                   Eval      SCFPIVAR    = SCFPartIVA
196400130912     C                   Eval      SCFCDFISCR  = SCFCdFisc
196500130925     C                   Eval      SCFTpOpeR   = *Blank
196600130925     C                   Eval      SCFNatOpeR  = *Blank
196700130924     C                   Eval      SCFLeasinR  = SCFLeasing
196800160104R195 C                   if        SCFLeasinR  = '0'
196900160104 "   C                   Eval      SCFLeasinR  = *Blank
197000160104R195 C                   endif
197100130912     C                   Eval      SCFDTREGR   = *Loval
197200130912     C                   Eval      SCFNRREGR   = *Zero
197300130912     C                   Eval      SCFSERREGR  = *Blank
197400131015R135 C* in caso di Doc.Riepilogo verifico l'effettiva modalità di
197500131015 "   C* aggregazione impostata sul parametro modulo
197600131015 "   C                   if        SCFTpDoc    = 'R'        and
197700131015 "   C                             DocRie0502  = 'D'
197800131015 "   C                   Eval      SCFDTREGR   = SCFDTREG
197900131015 "   C                   Eval      SCFNRREGR   = SCFNRREG
198000131015 "   C                   Eval      SCFSERREGR  = SCFSERREG
198100131015R135 C                   endif
198200130912     C*
198300130912     C*-------
198400130912     C* se documento Senza Fattura
198500130912     C                   When      SCFTpDoc    = 'S'
198600131017R136 C* controllo se documento legato al turismo (ovvero,
198700131017 "   C* codice fiscale non impostato)
198800131017R136 C                   if        SCFCdFisc  <> *Blank
198900130912     C                   Eval      SCFQuadro   = 'SA'
199000130912     C                   Eval      SCFSeqQua   = 02
199100130912     C*
199200130912     C                   Eval      SCFCOGRAGR  = *Blank
199300130912     C                   Eval      SCFNOMER    = *Blank
199400130912     C                   Eval      SCFDTNASR   = *Loval
199500130912     C                   Eval      SCFLOCNASR  = *Blank
199600130912     C                   Eval      SCFPRVNASR  = *Blank
199700130912     C                   Eval      SCFSTATOR   = *Blank
199800130918     C                   Eval      SCFCODUICR  = 0
199900130912     C                   Eval      SCFLOCALIR  = *Blank
200000130912     C                   Eval      SCFINDIRER  = *Blank
200100130924     C                   Eval      SCFTpNR     = *Blank
200200130912     C                   Eval      SCFPIVAR    = *Blank
200300130912     C                   Eval      SCFCDFISCR  = SCFCdFisc
200400130925     C                   Eval      SCFTpOpeR   = *Blank
200500130924     C                   Eval      SCFNatOpeR  = *Blank
200600130924     C                   Eval      SCFLeasinR  = SCFLeasing
200700160104R195 C                   if        SCFLeasinR  = '0'
200800160104 "   C                   Eval      SCFLeasinR  = *Blank
200900160104R195 C                   endif
201000130912     C                   Eval      SCFDTREGR   = *Loval
201100130912     C                   Eval      SCFNRREGR   = *Zero
201200130912     C                   Eval      SCFSERREGR  = *Blank
201300131017R136 C*
201400131017 "   C* si tratta di operazioni legate al Turismo
201500131017 "   C                   else
201600131017 "   C                   Eval      SCFQuadro   = 'TU'
201700131017 "   C                   Eval      SCFSeqQua   = 11
201800131017 "   C*
201900131017 "   C                   Eval      SCFCOGRAGR  = SCFCOGRAG
202000131017 "   C                   if        SCFTpN      = '1'
202100131017 "   C                   Eval      SCFNOMER    = SCFNOME
202200131017 "   C                   Eval      SCFDTNASR   = SCFDTNASC
202300131017 "   C                   Eval      SCFLOCNASR  = SCFLOCNASC
202400131017 "   C                   Eval      SCFPRVNASR  = SCFPRVNASC
202500131017 "   C                   Eval      SCFSTATOR   = SCFSTATO
202600131017 "   C                   Eval      SCFCODUICR  = SCFCODUIC
202700131017 "   C                   Eval      SCFLOCALIR  = SCFLOCALIT
202800131017 "   C                   Eval      SCFINDIRER  = SCFINDIRE
202900131017 "   C                   else
203000131017 "   C                   endif
203100131017 "   C                   Eval      SCFTpNR     = SCFTPN
203200131017 "   C                   Eval      SCFPIVAR    = *Blank
203300131017 "   C                   Eval      SCFCDFISCR  = *Blank
203400131017 "   C                   Eval      SCFTpOpeR   = *Blank
203500131017 "   C                   Eval      SCFNatOpeR  = *Blank
203600131017 "   C                   Eval      SCFLeasinR  = *Blank
203700131017 "   C                   Eval      SCFDTREGR   = SCFDTREG
203800131017 "   C                   Eval      SCFNRREGR   = SCFNRREG
203900131017 "   C                   Eval      SCFSERREGR  = SCFSERREG
204000131017R136 C                   endif
204100130912     C*
204200130912     C                   other
204300130912     C*-------
204400130912     C* se conto non residente
204500130912     C                   Eval      SCFQuadro   = 'BL'
204600130912     C                   Eval      SCFSeqQua   = 03
204700130912     C*
204800130912     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
204900131001     C                   if        SCFTpN      = '1'
205000131001     C                   Eval      SCFNOMER    = SCFNOME
205100131001     C                   Eval      SCFDTNASR   = SCFDTNASC
205200131001     C                   Eval      SCFLOCNASR  = SCFLOCNASC
205300131001     C                   Eval      SCFPRVNASR  = SCFPRVNASC
205400131001     C                   Eval      SCFSTATOR   = SCFSTATO
205500131001     C                   Eval      SCFCODUICR  = SCFCODUIC
205600131001     C                   else
205700131001     C                   Eval      SCFSTATOR   = SCFSTATO
205800131001     C                   Eval      SCFCODUICR  = SCFCODUIC
205900131001     C                   Eval      SCFLOCALIR  = SCFLOCALIT
206000131001     C                   Eval      SCFINDIRER  = SCFINDIRE
206100131001     C                   endif
206200130924     C                   Eval      SCFTpNR     = SCFTpN
206300130912     C                   Eval      SCFPIVAR    = SCFPartIVA
206400130912     C                   Eval      SCFCDFISCR  = SCFCdFisc
206500130925     C                   if        SCFTpOpe    = 'P'        and
206600130925     C                             SCFNatOpe   = 'S'
206700130925     C                   Eval      SCFTpOpeR   = 'P'
206800130925     C                   Eval      SCFNatOpeR  = 'S'
206900130925     C                   else
207000130925     C                   Eval      SCFTpOpeR   = *Blank
207100130925     C                   Eval      SCFNatOpeR  = *Blank
207200130925     C                   endif
207300130924     C                   Eval      SCFLeasinR  = *Blank
207400130912     C                   Eval      SCFDTREGR   = *Loval
207500130912     C                   Eval      SCFNRREGR   = *Zero
207600130912     C                   Eval      SCFSERREGR  = *Blank
207700130912     C*
207800130912     C                   endSl
207900130912     C*
208000130912     C*
208100130912     C*--------------------
208200130912     C* Formato DETTAGLIO
208300130918     C                   When      SCFFormato  = 'D'
208400130912     C                   Select
208500130912     C*-------
208600130912     C* se conto Nazionale e documento Fattura o Riepilogo
208700160104R195 X****               When      SCFCodUIC   = UICSoc0502    and
208800160104 "   X****                         (SCFTpDoc    = 'F'       or
208900160104R195 X****                          SCFTpDoc    = 'R'         )
209000160104R195 C*
209100160104 "   C* in caso di Autofattura, deve finire in FE perchè andrà
209200160104 "   C* girata sull'azienda comunicante
209300160104 "   C                   When      (SCFCodUIC   = UICSoc0502    and
209400160104 "   C                              (SCFTpDoc    = 'F'       or
209500160104 "   C                               SCFTpDoc    = 'R'         )   ) or
209600160104 "   C                             (SCFAutofat  = *On           and
209700160104 "   C                              (SCFTpDoc    = 'F'       or
209800160104R195 C                               SCFTpDoc    = 'R'         )   )
209900130912     C* se documenti Emessi
210000130912     C                   if        SCFTpOpe     = 'A'
210100130912     C                   Eval      SCFQuadro   = 'FE'
210200130912     C                   Eval      SCFSeqQua   = 04
210300130912     C*
210400130912     C                   Eval      SCFCOGRAGR  = *Blank
210500130912     C                   Eval      SCFNOMER    = *Blank
210600130912     C                   Eval      SCFDTNASR   = *Loval
210700130912     C                   Eval      SCFLOCNASR  = *Blank
210800130912     C                   Eval      SCFPRVNASR  = *Blank
210900130912     C                   Eval      SCFSTATOR   = *Blank
211000130918     C                   Eval      SCFCODUICR  = 0
211100130912     C                   Eval      SCFLOCALIR  = *Blank
211200130912     C                   Eval      SCFINDIRER  = *Blank
211300130924     C                   Eval      SCFTpNR     = *Blank
211400130912     C                   Eval      SCFPIVAR    = SCFPartIVA
211500130912     C                   Eval      SCFCDFISCR  = SCFCdFisc
211600130925     C                   Eval      SCFTpOpeR   = *Blank
211700130924     C                   Eval      SCFNatOpeR  = *Blank
211800130924     C                   Eval      SCFLeasinR  = SCFLeasing
211900160104R140 C                   if        SCFLeasinR  = '0'
212000160104 "   C                   Eval      SCFLeasinR  = *Blank
212100160104R140 C                   endif
212200130912     C                   Eval      SCFDTREGR   = SCFDTREG
212300130912     C                   Eval      SCFNRREGR   = SCFNRREG
212400130912     C                   Eval      SCFSERREGR  = SCFSERREG
212500130912     C*
212600130912     C                   else
212700130912     C*
212800130912     C* se documenti Ricevuti
212900130912     C                   Eval      SCFQuadro   = 'FR'
213000130912     C                   Eval      SCFSeqQua   = 05
213100130912     C*
213200130912     C                   Eval      SCFCOGRAGR  = *Blank
213300130912     C                   Eval      SCFNOMER    = *Blank
213400130912     C                   Eval      SCFDTNASR   = *Loval
213500130912     C                   Eval      SCFLOCNASR  = *Blank
213600130912     C                   Eval      SCFPRVNASR  = *Blank
213700130912     C                   Eval      SCFSTATOR   = *Blank
213800130918     C                   Eval      SCFCODUICR  = 0
213900130912     C                   Eval      SCFLOCALIR  = *Blank
214000130912     C                   Eval      SCFINDIRER  = *Blank
214100130924     C                   Eval      SCFTpNR     = *Blank
214200130912     C                   Eval      SCFPIVAR    = SCFPartIVA
214300130912     C                   Eval      SCFCDFISCR  = *Blank
214400130925     C                   Eval      SCFTpOpeR   = *Blank
214500130924     C                   Eval      SCFNatOpeR  = *Blank
214600130924     C                   Eval      SCFLeasinR  = *Blank
214700130912     C                   Eval      SCFDTREGR   = SCFDTREG
214800130912     C                   Eval      SCFNRREGR   = SCFNRREG
214900130912     C                   Eval      SCFSERREGR  = SCFSERREG
215000130912     C*
215100130912     C                   endif
215200130912     C*
215300130912     C*-------
215400130912     C* se conto Nazionale e documento Nota
215500160104R195 X****               When      SCFCodUIC   = UICSoc0502 and
215600160104R195 X****                         SCFTpDoc    = 'N'
215700160104R195 C*
215800160104 "   C* in caso di Autofattura, deve finire in NE perchè andrà
215900160104 "   C* girata sull'azienda comunicante
216000160104 "   C                   When      (SCFCodUIC   = UICSoc0502 and
216100160104 "   C                              SCFTpDoc    = 'N'           ) or
216200160104 "   C                             (SCFAutofat  = *On      and
216300160104R195 C                              SCFTpDoc    = 'N'         )
216400130912     C* se documenti Emessi
216500130912     C                   if        SCFTpOpe     = 'A'
216600130912     C                   Eval      SCFQuadro   = 'NE'
216700130912     C                   Eval      SCFSeqQua   = 06
216800130912     C*
216900130912     C                   Eval      SCFCOGRAGR  = *Blank
217000130912     C                   Eval      SCFNOMER    = *Blank
217100130912     C                   Eval      SCFDTNASR   = *Loval
217200130912     C                   Eval      SCFLOCNASR  = *Blank
217300130912     C                   Eval      SCFPRVNASR  = *Blank
217400130912     C                   Eval      SCFSTATOR   = *Blank
217500130918     C                   Eval      SCFCODUICR  = 0
217600130912     C                   Eval      SCFLOCALIR  = *Blank
217700130912     C                   Eval      SCFINDIRER  = *Blank
217800130924     C                   Eval      SCFTpNR     = *Blank
217900130912     C                   Eval      SCFPIVAR    = SCFPartIVA
218000130912     C                   Eval      SCFCDFISCR  = SCFCdFisc
218100130925     C                   Eval      SCFTpOpeR   = *Blank
218200130924     C                   Eval      SCFNatOpeR  = *Blank
218300130924     C                   Eval      SCFLeasinR  = *Blank
218400130912     C                   Eval      SCFDTREGR   = SCFDTREG
218500130912     C                   Eval      SCFNRREGR   = SCFNRREG
218600130912     C                   Eval      SCFSERREGR  = SCFSERREG
218700130912     C*
218800130912     C                   else
218900130912     C*
219000130912     C* se documenti Ricevuti
219100130912     C                   Eval      SCFQuadro   = 'NR'
219200130912     C                   Eval      SCFSeqQua   = 07
219300130912     C*
219400130912     C                   Eval      SCFCOGRAGR  = *Blank
219500130912     C                   Eval      SCFNOMER    = *Blank
219600130912     C                   Eval      SCFDTNASR   = *Loval
219700130912     C                   Eval      SCFLOCNASR  = *Blank
219800130912     C                   Eval      SCFPRVNASR  = *Blank
219900130912     C                   Eval      SCFSTATOR   = *Blank
220000130918     C                   Eval      SCFCODUICR  = 0
220100130912     C                   Eval      SCFLOCALIR  = *Blank
220200130912     C                   Eval      SCFINDIRER  = *Blank
220300130924     C                   Eval      SCFTpNR     = *Blank
220400130912     C                   Eval      SCFPIVAR    = SCFPartIVA
220500130912     C                   Eval      SCFCDFISCR  = *Blank
220600130925     C                   Eval      SCFTpOpeR   = *Blank
220700130924     C                   Eval      SCFNatOpeR  = *Blank
220800130924     C                   Eval      SCFLeasinR  = *Blank
220900130912     C                   Eval      SCFDTREGR   = SCFDTREG
221000130912     C                   Eval      SCFNRREGR   = SCFNRREG
221100130912     C                   Eval      SCFSERREGR  = SCFSERREG
221200130912     C*
221300130912     C                   endif
221400130912     C*
221500130912     C*-------
221600130912     C* se documento Senza Fattura
221700130912     C                   When      SCFTpDoc    = 'S'
221800131017R136 C* controllo se documento legato al turismo (ovvero,
221900131017 "   C* codice fiscale non impostato)
222000131017R136 C                   if        SCFCdFisc  <> *Blank
222100130912     C                   Eval      SCFQuadro   = 'DF'
222200130912     C                   Eval      SCFSeqQua   = 08
222300130912     C*
222400130912     C                   Eval      SCFCOGRAGR  = *Blank
222500130912     C                   Eval      SCFNOMER    = *Blank
222600130912     C                   Eval      SCFDTNASR   = *Loval
222700130912     C                   Eval      SCFLOCNASR  = *Blank
222800130912     C                   Eval      SCFPRVNASR  = *Blank
222900130912     C                   Eval      SCFSTATOR   = *Blank
223000130918     C                   Eval      SCFCODUICR  = 0
223100130912     C                   Eval      SCFLOCALIR  = *Blank
223200130912     C                   Eval      SCFINDIRER  = *Blank
223300130924     C                   Eval      SCFTpNR     = *Blank
223400130912     C                   Eval      SCFPIVAR    = *Blank
223500130912     C                   Eval      SCFCDFISCR  = SCFCdFisc
223600130925     C                   Eval      SCFTpOpeR   = *Blank
223700130924     C                   Eval      SCFNatOpeR  = *Blank
223800130924     C                   Eval      SCFLeasinR  = SCFLeasing
223900160104R195 C                   if        SCFLeasinR  = '0'
224000160104 "   C                   Eval      SCFLeasinR  = *Blank
224100160104R195 C                   endif
224200130912     C                   Eval      SCFDTREGR   = SCFDTREG
224300130912     C                   Eval      SCFNRREGR   = SCFNRREG
224400130912     C                   Eval      SCFSERREGR  = SCFSERREG
224500131017R136 C*
224600131017 "   C* si tratta di operazioni legate al Turismo
224700131017 "   C                   else
224800131017 "   C                   Eval      SCFQuadro   = 'TU'
224900131017 "   C                   Eval      SCFSeqQua   = 11
225000131017 "   C*
225100131017 "   C                   Eval      SCFCOGRAGR  = SCFCOGRAG
225200131017 "   C                   if        SCFTpN      = '1'
225300131017 "   C                   Eval      SCFNOMER    = SCFNOME
225400131017 "   C                   Eval      SCFDTNASR   = SCFDTNASC
225500131017 "   C                   Eval      SCFLOCNASR  = SCFLOCNASC
225600131017 "   C                   Eval      SCFPRVNASR  = SCFPRVNASC
225700131017 "   C                   Eval      SCFSTATOR   = SCFSTATO
225800131017 "   C                   Eval      SCFCODUICR  = SCFCODUIC
225900131017 "   C                   Eval      SCFLOCALIR  = SCFLOCALIT
226000131017 "   C                   Eval      SCFINDIRER  = SCFINDIRE
226100131017 "   C                   else
226200131017 "   C                   endif
226300131017 "   C                   Eval      SCFTpNR     = SCFTPN
226400131017 "   C                   Eval      SCFPIVAR    = *Blank
226500131017 "   C                   Eval      SCFCDFISCR  = *Blank
226600131017 "   C                   Eval      SCFTpOpeR   = *Blank
226700131017 "   C                   Eval      SCFNatOpeR  = *Blank
226800131017 "   C                   Eval      SCFLeasinR  = *Blank
226900131017 "   C                   Eval      SCFDTREGR   = SCFDTREG
227000131017 "   C                   Eval      SCFNRREGR   = SCFNRREG
227100131017 "   C                   Eval      SCFSERREGR  = SCFSERREG
227200131017R136 C                   endif
227300130912     C*
227400130912     C*
227500130912     C                   other
227600130912     C*-------
227700130912     C* se conto non residente
227800131015R135 X**** e Acquisti di Servizi
227900131015R135 X****               if        SCFTpOpe    = 'P'    and
228000131015R135 X****                         SCFNatOpe   = 'S'
228100131015R135 C* e Acquisti di Beni o di Servizi
228200131015R135 C                   if        SCFTpOpe    = 'P'
228300130912     C                   Eval      SCFQuadro   = 'SE'
228400130912     C                   Eval      SCFSeqQua   = 10
228500130912     C*
228600130912     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
228700131001     C                   if        SCFTpN      = '1'
228800131001     C                   Eval      SCFNOMER    = SCFNOME
228900131001     C                   Eval      SCFDTNASR   = SCFDTNASC
229000131001     C                   Eval      SCFLOCNASR  = SCFLOCNASC
229100131001     C                   Eval      SCFPRVNASR  = SCFPRVNASC
229200131001     C                   Eval      SCFSTATOR   = SCFSTATO
229300131001     C                   Eval      SCFCODUICR  = SCFCODUIC
229400131001     C                   else
229500131001     C                   Eval      SCFSTATOR   = SCFSTATO
229600131001     C                   Eval      SCFCODUICR  = SCFCODUIC
229700131001     C                   Eval      SCFLOCALIR  = SCFLOCALIT
229800131001     C                   Eval      SCFINDIRER  = SCFINDIRE
229900131001     C                   endif
230000160104R195 X****               Eval      SCFTpNR     = SCFTPNR
230100160104R195 C                   Eval      SCFTpNR     = SCFTPN
230200130912     C                   Eval      SCFPIVAR    = SCFPartIVA
230300130912     C                   Eval      SCFCDFISCR  = SCFCdFisc
230400160201R195 X****               Eval      SCFTpOpeR   = *Blank
230500160201R195 C                   Eval      SCFTpOpeR   = SCFTpOpe
230600130924     C                   Eval      SCFNatOpeR  = *Blank
230700130924     C                   Eval      SCFLeasinR  = *Blank
230800130912     C                   Eval      SCFDTREGR   = SCFDTREG
230900130912     C                   Eval      SCFNRREGR   = SCFNRREG
231000130912     C                   Eval      SCFSERREGR  = SCFSERREG
231100130912     C*
231200130912     C                   else
231300130912     C*
231400130912     C* se conto non residente
231500131015R135 X**** e Vendite o Acquisti di Beni
231600131015R135 C* e Vendite di Beni o Servizi
231700130912     C                   Eval      SCFQuadro   = 'FN'
231800130912     C                   Eval      SCFSeqQua   = 09
231900130912     C*
232000130912     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
232100131001     C                   if        SCFTpN      = '1'
232200131001     C                   Eval      SCFNOMER    = SCFNOME
232300131001     C                   Eval      SCFDTNASR   = SCFDTNASC
232400131001     C                   Eval      SCFLOCNASR  = SCFLOCNASC
232500131001     C                   Eval      SCFPRVNASR  = SCFPRVNASC
232600131001     C                   Eval      SCFSTATOR   = SCFSTATO
232700131001     C                   Eval      SCFCODUICR  = SCFCODUIC
232800131001     C                   else
232900131001     C                   Eval      SCFSTATOR   = SCFSTATO
233000131001     C                   Eval      SCFCODUICR  = SCFCODUIC
233100131001     C                   Eval      SCFLOCALIR  = SCFLOCALIT
233200131001     C                   Eval      SCFINDIRER  = SCFINDIRE
233300131001     C                   endif
233400160104R195 X****               Eval      SCFTpNR     = SCFTPNR
233500160104R195 C                   Eval      SCFTpNR     = SCFTPN
233600130912     C                   Eval      SCFPIVAR    = *Blank
233700130912     C                   Eval      SCFCDFISCR  = *Blank
233800130925     C                   Eval      SCFTpOpeR   = *Blank
233900130924     C                   Eval      SCFNatOpeR  = *Blank
234000130924     C                   Eval      SCFLeasinR  = SCFLeasing
234100160104R195 C                   if        SCFLeasinR  = '0'
234200160104 "   C                   Eval      SCFLeasinR  = *Blank
234300160104R195 C                   endif
234400130912     C                   Eval      SCFDTREGR   = SCFDTREG
234500130912     C                   Eval      SCFNRREGR   = SCFNRREG
234600130912     C                   Eval      SCFSERREGR  = SCFSERREG
234700130912     C*
234800130912     C                   endif
234900130912     C*
235000130912     C                   endSl
235100130912     C*
235200130912     C                   endSl
235300131016R135 C*
235400131016 "   C* se Acquisto di Beni da non residente e non annullato,
235500131016 "   C* segnalo errore
235600131016 "   C                   if        SCFTpOpe    = 'P'           and
235700131016 "   C                             SCFNatOpe   = 'B'           and
235800131016 "   C                             SCFQuadro   = 'SE'          and
235900131016 "   C                             SCFAnn     <> '1'
236000131016 "   C                   Eval      SCFAnn      = '1'
236100131016 "   C                   Eval      SCFTpAnn    = '53'
236200131016R135 C                   endif
236300130912     C*
236400130912     C                   ENDSR
236500130912     C/EJECT
236600160104R195 C************************************************************
236700160104 "   C* Definizione dati per Quadro SE per Aquisti da San Marino
236800160104 "   C************************************************************
236900160104R195 C     DefQuadroSE   BEGSR
237000160104     C*
237100160104     C* Formato DETTAGLIO
237200160104     C                   Eval      SCFFormato  = 'D'
237300160104     C*
237400160104     C                   Eval      SCFQuadro   = 'SE'
237500160104     C                   Eval      SCFSeqQua   = 10
237600160104     C*
237700160104     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
237800160104     C                   if        SCFTpN      = '1'
237900160104     C                   Eval      SCFNOMER    = SCFNOME
238000160104     C                   Eval      SCFDTNASR   = SCFDTNASC
238100160104     C                   Eval      SCFLOCNASR  = SCFLOCNASC
238200160104     C                   Eval      SCFPRVNASR  = SCFPRVNASC
238300160104     C                   Eval      SCFSTATOR   = SCFSTATO
238400160104     C                   Eval      SCFCODUICR  = SCFCODUIC
238500160104     C                   else
238600160104     C                   Eval      SCFSTATOR   = SCFSTATO
238700160104     C                   Eval      SCFCODUICR  = SCFCODUIC
238800160104     C                   Eval      SCFLOCALIR  = SCFLOCALIT
238900160104     C                   Eval      SCFINDIRER  = SCFINDIRE
239000160104     C                   endif
239100160104     C                   Eval      SCFTpNR     = SCFTPN
239200160104     C                   Eval      SCFPIVAR    = SCFPartIVA
239300160104     C                   Eval      SCFCDFISCR  = SCFCdFisc
239400160201     C                   Eval      SCFTpOpeR   = SCFTpOpe
239500160104     C                   Eval      SCFNatOpeR  = *Blank
239600160104     C                   Eval      SCFLeasinR  = *Blank
239700160104     C                   Eval      SCFDTREGR   = SCFDTREG
239800160104     C                   Eval      SCFNRREGR   = SCFNRREG
239900160104     C                   Eval      SCFSERREGR  = SCFSERREG
240000160104     C*
240100160104R195 C                   ENDSR
240200160104R195 C/EJECT
240300160104R195 C************************************************************
240400160104 "   C* Definizione dati per Quadro BL in comunicaz. Black List
240500160104 "   C************************************************************
240600160104R195 C     DefQuadroBL   BEGSR
240700160104     C*
240800160104     C                   Eval      SCFFormato  = 'A'
240900160104     C                   Eval      SCFQuadro   = 'BL'
241000160104     C                   Eval      SCFSeqQua   = 03
241100160104     C*
241200160104     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
241300160104     C                   if        SCFTpN      = '1'
241400160104     C                   Eval      SCFNOMER    = SCFNOME
241500160104     C                   Eval      SCFDTNASR   = SCFDTNASC
241600160104     C                   Eval      SCFLOCNASR  = SCFLOCNASC
241700160104     C                   Eval      SCFPRVNASR  = SCFPRVNASC
241800160104     C                   Eval      SCFSTATOR   = SCFSTATO
241900160104     C                   Eval      SCFCODUICR  = SCFCODUIC
242000160104     C                   else
242100160104     C                   Eval      SCFSTATOR   = SCFSTATO
242200160104     C                   Eval      SCFCODUICR  = SCFCODUIC
242300160104     C                   Eval      SCFLOCALIR  = SCFLOCALIT
242400160104     C                   Eval      SCFINDIRER  = SCFINDIRE
242500160104     C                   endif
242600160104     C                   Eval      SCFTpNR     = SCFTpN
242700160104     C                   Eval      SCFPIVAR    = SCFPartIVA
242800160104     C                   Eval      SCFCDFISCR  = SCFCdFisc
242900160104     C                   Eval      SCFTpOpeR   = *Blank
243000160104     C                   Eval      SCFNatOpeR  = *Blank
243100160104     C                   Eval      SCFLeasinR  = *Blank
243200160104     C                   Eval      SCFDTREGR   = *Loval
243300160104     C                   Eval      SCFNRREGR   = *Zero
243400160104     C                   Eval      SCFSERREGR  = *Blank
243500160104     C*
243600160104R195 C                   ENDSR
243700160104R195 C/EJECT
243800000000     C************************************************************
243900000000     C* Annullo/ripristino a seconda dello stato di partenza
244000000000     C************************************************************
244100130826     C     AnnSDGSCF     BEGSR
244200000000     C*
244300130826     C                   Eval      SCFORIGINE  = 'M'
244400130826     C                   Eval      SCFUTEULMO  = XScCUt
244500130826     C                   Eval      SCFDTULMO   = UDateIso
244600000000     C*
244700000000     C                   Select
244800000000     C*
244900000000     C* Annulla
245000130826     C                   When      AnnAtt0502  = *Blank
245100130826     C                   Eval      SCFAnn      = '1'
245200130826     C                   Eval      SCFTpAnn    = '00'
245300000000     C*
245400000000     C* Ripristina
245500130826     C                   When      AnnAtt0502  = '1'
245600130826     C                   Eval      SCFAnn      = *Blank
245700130826     C                   Eval      SCFTpAnn    = *Blank
245800000000     C*
245900130828     C                   endSl
246000000000     C*
246100000000     C                   ENDSR
246200000000     C/EJECT
246300000000     C************************************************************
246400000000     C* OPERAZIONI INIZIALI
246500000000     C************************************************************
246600000000     C     *INZSR        BEGSR
246700000000     C*
246800000000     C     *ENTRY        PLIST
246900000000     C                   PARM                    KPJBA
247000130826     C                   PARM                    SDG00502DS
247100130826     C                   PARM                    SDG00592DS
247200130828     C                   PARM                    SDG005FADS
247300130828     C                   PARM                    SDG005SADS
247400130828     C                   PARM                    SDG005BLDS
247500130828     C                   PARM                    SDG005FEDS
247600130828     C                   PARM                    SDG005FRDS
247700130828     C                   PARM                    SDG005NEDS
247800130828     C                   PARM                    SDG005NRDS
247900130828     C                   PARM                    SDG005DFDS
248000130828     C                   PARM                    SDG005FNDS
248100130828     C                   PARM                    SDG005SEDS
248200131017R136 C                   PARM                    SDG005TUDS
248300130828     C                   PARM                    SDG005TADS
248400000000     C*
248500000000     C     *DMY          Move      Udate         UDateISO
248600000000     C*
248700000000     C* Reperimento dati società
248800000000     C                   MOVEL     'SOC001'      TIPXSC
248900000000     C                   MOVEL     *BLANK        SOCXSC
249000130827     C                   EXSR      REPSOC
249100000000     C*
249200000000     C* Controlli dati società
249300000000     C     RTNXSC        IFNE      '1'
249400000000     C                   MOVEL     XSOCDS        SOC001
249500000000     C                   ELSE
249600130827    >C                   MOVE      '1'           ERR0502
249700130826    >C                   MOVE      '1'           End0502
249800130827     C                   EXSR      ENDPGM
249900000000     C                   ENDIF
250000131008R134 C*
250100131008R134 C                   Eval      $FineSCF    = *Off
250200000000     C*
250300000000     C                   ENDSR
250400000000     C/EJECT
250500000000     C************************************************************
250600000000     C* REPERIMENTO DATI SOCIETA'
250700000000     C************************************************************
250800000000     C     REPSOC        BEGSR
250900000000     C*
251000000000     C                   CALL      'SDGSOCDR'
251100000000     C                   PARM                    TIPXSC            6
251200000000     C                   PARM                    SOCXSC            3
251300000000     C                   PARM                    CDSXSC            9 0
251400000000     C                   PARM                    MODXSC            3
251500000000     C                   PARM      *BLANKS       RTNXSC            1
251600000000     C                   PARM                    XSOCDS
251700000000     C                   PARM                    KPJBA
251800000000     C*
251900000000     C                   ENDSR
252000000000     C/EJECT
252100000000     C************************************************************
252200000000     C* INIZIALIZZAZIONE VARIABILI
252300000000     C************************************************************
252400000000     C     INZVAR        BEGSR
252500000000     C*
252600130827     C                   Eval      ERR0502     = '0'
252700130826     C                   Eval      End0502     = '0'
252800000000     C*
252900000000     C* Se è la prima chiamata eseguo il posizionamento
253000130826     C                   if        RIC0502     = '0'
253100130827     C*
253200130902     C                   EXSR      InzCampiSav
253300130902     C                   EXSR      InzImporti
253400130827     C*
253500130826     C                   if        TpList0502  = '01'
253600130826     C                   Close     SDGSCF01L
253700130826     C                   Open      SDGSCF01L
253800130826     C                   else
253900130826     C                   Close     SDGSCF02L
254000130826     C                   Open      SDGSCF02L
254100130826     C                   endif
254200000000     C*
254300000000     C                   endif
254400000000     C*
254500130827     C* creo limiti inferiore/superiore
254600130827     C                   EXSR      DefMinMax
254700000000     C*
254800000000     C                   ENDSR
254900000000     C/EJECT
255000130827     C************************************************************
255100130827     C* Inizializzo i campi di lavoro e di salvataggio
255200130827     C************************************************************
255300130902     C     InzCampiSav   BEGSR
255400130827     C*
255500130827     C                   Clear                   SavAnnAtt
255600130827     C                   Z-Add     0             $NrRcdLett
255700130827     C                   Z-Add     0             $NrRcdAnn
255800130827     C*
255900130827     C                   Clear                   SavAnn
256000130827     C                   Clear                   SavTpAnn
256100130827     C                   Clear                   SavOrigine
256200130827     C                   Clear                   SavSocieta
256300130827     C                   Clear                   SavTpCom
256400130827     C                   Clear                   SavAnno
256500130827     C                   Clear                   SavMese
256600130827     C                   Clear                   SavCF
256700130827     C                   Clear                   SavSogg
256800130903     C                   Clear                   SavTpN
256900130827     C                   Clear                   SavPartIVA
257000130827     C                   Clear                   SavCdFisc
257100130827     C                   Clear                   SavCogRag
257200130827     C                   Clear                   SavNome
257300130827     C                   Clear                   SavDtNasc
257400130827     C                   Clear                   SavLocNasc
257500130827     C                   Clear                   SavPrvNasc
257600130827     C                   Clear                   SavStato
257700130827     C                   Clear                   SavCodUIC
257800130827     C                   Clear                   SavLocalit
257900130827     C                   Clear                   SavIndirE
258000000000     C*
258100130827     C                   Clear                   SavKcc
258200130827     C                   Clear                   SavKsc
258300130827     C                   Clear                   SavCausale
258400130827     C                   Clear                   SavAliq
258500130827     C                   Clear                   SavRifIVA
258600130827     C                   Clear                   SavDtDoc
258700130827     C                   Clear                   SavNrDoc
258800130827     C                   Clear                   SavSerDoc
258900130827     C                   Clear                   SavDtReg
259000130827     C                   Clear                   SavNrReg
259100130827     C                   Clear                   SavSerReg
259200130827     C                   Clear                   SavDtPar
259300130827     C                   Clear                   SavNrPar
259400130827     C                   Clear                   SavSerPar
259500130827     C                   Clear                   SavSys
259600130827     C                   Clear                   SavNrAsReg
259700130827     C                   Clear                   SavNrRigaM
259800130827     C                   Clear                   SavNrRiga
259900130827     C*
260000130827     C                   Clear                   SavTpDoc
260100130827     C                   Clear                   SavTpOpe
260200130827     C                   Clear                   SavNoIVA
260300130827     C                   Clear                   SavMargine
260400130827     C                   Clear                   SavReverse
260500130827     C                   Clear                   SavAutofat
260600130827     C*
260700130827     C                   Clear                   SavTpImp
260800130827     C                   Clear                   SavImpOpe
260900130827     C                   Clear                   SavIVAOpe
261000130827     C                   Clear                   SavImpTot
261100130827     C                   Clear                   SavIVATot
261200130827     C*
261300130827     C                   Clear                   SavTpReg
261400130827     C                   Clear                   SavLibro
261500130827     C                   Clear                   SavVarNota
261600130827     C                   Clear                   SavFormato
261700130827     C*
261800130827     C                   Clear                   SavSEQQUA
261900130827     C                   Clear                   SavQUADRO
262000130827     C                   Clear                   SavCOGRAGR
262100130827     C                   Clear                   SavNOMER
262200130827     C                   Clear                   SavDTNASR
262300130827     C                   Clear                   SavLOCNASR
262400130827     C                   Clear                   SavPRVNASR
262500130827     C                   Clear                   SavSTATOR
262600130827     C                   Clear                   SavCODUICR
262700130827     C                   Clear                   SavLOCALIR
262800130827     C                   Clear                   SavINDIRER
262900130903     C                   Clear                   SavTpNR
263000130827     C                   Clear                   SavPIVAR
263100130827     C                   Clear                   SavCDFISCR
263200130925     C                   Clear                   SavTpOpeR
263300130925     C                   Clear                   SavNatOpeR
263400130902     C                   Clear                   SavLeasinR
263500130827     C                   Clear                   SavDTREGR
263600130827     C                   Clear                   SavNRREGR
263700130827     C                   Clear                   SavSERREGR
263800131017R136 C*
263900131017 "   C                   Clear                   SavCOGRAGR
264000131017 "   C                   Clear                   SavNOMER
264100131017 "   C                   Clear                   SavDTNASR
264200131017 "   C                   Clear                   SavLOCNASR
264300131017 "   C                   Clear                   SavPRVNASR
264400131017 "   C                   Clear                   SavSTATOR
264500131017 "   C                   Clear                   SavCODUICR
264600131017 "   C                   Clear                   SavLOCALIR
264700131017R136 C                   Clear                   SavINDIRER
264800130904     C*
264900130904     C                   Clear                   SavDtReg2
265000130904     C                   Clear                   SavNrReg2
265100130904     C                   Clear                   SavSerReg2
265200130827     C*
265300130827     C                   ENDSR
265400130827     C/EJECT
265500000000     C************************************************************
265600130827     C* Definisco limiti minimi e massimi per le selezioni
265700000000     C************************************************************
265800130827     C     DefMinMax     BEGSR
265900000000     C*
266000000000     C                   MOVEL     *ALL'9'       ALL9             75
266100110822     C*
266200110822     C* ---------------------------------------------------
266300110822     C* il filtro sui dati attivi/annullati vale per tutti i
266400110822     C* tipi di lettura dei dati
266500110822     C*
266600130827     C* Questi campi sono di filtro nella manutenzione
266700130827     C* e nella creazione del tracciato ministeriale
266800110822     C                   Select
266900110822     C* chiesti attivi e non attivi
267000130826     C                   When      Ann0502     = '1'
267100110822     C                   Eval      $AnnMin     = *Blank
267200110822     C                   Eval      Ann999      = '1'
267300110822     C* chiesti solo attivi
267400130826     C                   When      Ann0502     = '2'
267500110822     C                   Eval      $AnnMin     = *Blank
267600110822     C                   Eval      Ann999      = *Blank
267700110822     C* chiesti solo non attivi
267800130826     C                   When      Ann0502     = '3'
267900110822     C                   Eval      $AnnMin     = '1'
268000110822     C                   Eval      Ann999      = '1'
268100110822     C                   endSl
268200110822     C*
268300130826     C                   if        TpAnn0502  <> *Hival
268400130826     C                   MoveL     TpAnn0502     TpAnn111
268500130826     C                   MoveL     TpAnn0502     TpAnn999
268600110822     C                   else
268700110824     C                   MoveL     *Blank        TpAnn111
268800130919     C                   MOVEL(P)  *ALL'9'       TpAnn999
268900110822     C                   endif
269000110822     C*
269100110822     C*
269200130827     C* ---------------------------------------------------
269300130827     C* selezioni sui dati di testata
269400130827     C* (alcuni campi sono di filtro nella manutenzione
269500130827     C*  tutti lo sono per filtrare sulle righe del "soggetto"/
269600130827     C*  quadro elaborato tramite opz.11=Dettaglio)
269700130826     C                   if        CF0502     <> *Hival
269800130826     C                   MoveL     CF0502        CF999
269900130826     C                   MoveL     CF0502        CF111
270000110822     C                   else
270100110824     C                   MoveL     *Blank        CF111
270200130919     C                   MOVEL(P)  *ALL'9'       CF999
270300110822     C                   endif
270400131206R141 C*
270500131206 "   C                   if        Causal0502 <> *Hival
270600131206 "   C                   MoveL     Causal0502    Causal111
270700131206 "   C                   MoveL     Causal0502    Causal999
270800131206 "   C                   else
270900131206 "   C                   MoveL     *Blank        Causal111
271000131206 "   C                   MOVEL(P)  *ALL'9'       Causal999
271100131206R141 C                   endif
271200110822     C*
271300130826     C                   if        Sogg0502   <> *Hival
271400130826     C                   MoveL     Sogg0502      Sogg111
271500130826     C                   MoveL     Sogg0502      Sogg999
271600110822     C                   else
271700110824     C                   MoveL     *Blank        Sogg111
271800130919     C                   MOVEL(P)  *ALL'9'       Sogg999
271900110822     C                   endif
272000110822     C*
272100130903     C                   if        TpN0502    <> *Hival
272200130903     C                   MoveL     TpN0502       TpN111
272300130903     C                   MoveL     TpN0502       TpN999
272400110822     C                   else
272500130919     C                   MoveL     *Blank        TpN111
272600130919     C                   MOVEL(P)  *ALL'9'       TpN999
272700110822     C                   endif
272800130903     C*
272900130903     C                   if        PIVA0502   <> *Hival
273000130903     C                   MoveL     PIVA0502      PIVA111
273100130903     C                   MoveL     PIVA0502      PIVA999
273200130903     C                   else
273300130919     C                   MoveL     *Blank        PIVA111
273400130919     C                   MOVEL(P)  *ALL'9'       PIVA999
273500130903     C                   endif
273600110822     C*
273700130826     C                   if        CdFisc0502 <> *Hival
273800130826     C                   MoveL     CdFisc0502    CdFisc111
273900130826     C                   MoveL     CdFisc0502    CdFisc999
274000110822     C                   else
274100110824     C                   MoveL     *Blank        CdFisc111
274200130919     C                   MOVEL(P)  *ALL'9'       CdFisc999
274300110822     C                   endif
274400110822     C*
274500130826     C                   if        CogRag0502 <> *Hival
274600130826     C                   MoveL     CogRag0502    CogRag111
274700130826     C                   MoveL     CogRag0502    CogRag999
274800110822     C                   else
274900110824     C                   MoveL     *Blank        CogRag111
275000130919     C                   MOVEL(P)  *ALL'9'       CogRag999
275100110822     C                   endif
275200110822     C*
275300130826     C                   if        Nome0502   <> *Hival
275400130826     C                   MoveL     Nome0502      Nome111
275500130826     C                   MoveL     Nome0502      Nome999
275600110822     C                   else
275700110824     C                   MoveL     *Blank        Nome111
275800130919     C                   MOVEL(P)  *ALL'9'       Nome999
275900110822     C                   endif
276000110822     C*
276100130826     C                   if        DtNasc0502 <> *Hival
276200130826     C                   Eval      DtNasc111   = DtNasc0502
276300130826     C                   Eval      DtNasc999   = DtNasc0502
276400110822     C                   else
276500110824     C                   Move      *Loval        DtNasc111
276600110822     C                   Move      WDtMax        DtNasc999
276700110822     C                   endif
276800110822     C*
276900130826     C                   if        LocNas0502 <> *Hival
277000130828     C                   MoveL     LocNas0502    LocNas111
277100130828     C                   MoveL     LocNas0502    LocNas999
277200110822     C                   else
277300130828     C                   MoveL     *Blank        LocNas111
277400130919     C                   MOVEL(P)  *ALL'9'       LocNas999
277500110822     C                   endif
277600110822     C*
277700130826     C                   if        PrvNas0502  <> *Hival
277800130828     C                   MoveL     PrvNas0502    PrvNas111
277900130828     C                   MoveL     PrvNas0502    PrvNas999
278000110822     C                   else
278100130828     C                   MoveL     *Blank        PrvNas111
278200130919     C                   MOVEL(P)  *ALL'9'       PrvNas999
278300110822     C                   endif
278400110822     C*
278500130826     C                   if        Stato0502   <> *Hival
278600130826     C                   MoveL     Stato0502     Stato111
278700130826     C                   MoveL     Stato0502     Stato999
278800110822     C                   else
278900110824     C                   MoveL     *Blank        Stato111
279000130919     C                   MOVEL(P)  *ALL'9'       Stato999
279100110822     C                   endif
279200110822     C*
279300130826     C                   if        CodUIC0502 <> *Hival
279400130918     C                   Z-ADD     CodUIC0502    CodUIC111
279500130918     C                   Z-ADD     CodUIC0502    CodUIC999
279600110822     C                   else
279700130918     C                   Z-ADD     0             CodUIC111
279800130918     C                   Z-ADD     999           CodUIC999
279900110822     C                   endif
280000110822     C*
280100130826     C                   if        Locali0502 <> *Hival
280200130828     C                   MoveL     Locali0502    Locali111
280300130828     C                   MoveL     Locali0502    Locali999
280400110822     C                   else
280500130828     C                   MoveL     *Blank        Locali111
280600130919     C                   MOVEL(P)  *ALL'9'       Locali999
280700110822     C                   endif
280800110822     C*
280900130826     C                   if        IndirE0502 <> *Hival
281000130826     C                   MoveL     IndirE0502    IndirE111
281100130826     C                   MoveL     IndirE0502    IndirE999
281200110822     C                   else
281300110824     C                   MoveL     *Blank        IndirE111
281400130919     C                   MOVEL(P)  *ALL'9'       IndirE999
281500110822     C                   endif
281600110822     C*
281700110822     C* ---------------------------------------------------
281800110822     C* le selezioni sui seguenti campi hanno senso solo per
281900110822     C* i quadri e per la determinazione del relativo dettaglio
282000130827     C*
282100130919     C                   if        TpOpe0502  <> *Hival
282200130919     C                   Eval      TpOpe111    = TpOpe0502
282300130919     C                   Eval      TpOpe999    = TpOpe0502
282400130827     C                   else
282500130919     C                   MoveL     *Blank        TpOpe111
282600130919     C                   MOVEL(P)  *ALL'9'       TpOpe999
282700130827     C                   endif
282800130919     C*
282900130919     C                   if        Quadro0502 <> *Hival
283000130919     C                   Eval      Quadro111   = Quadro0502
283100130919     C                   Eval      Quadro999   = Quadro0502
283200130919     C                   else
283300130919     C                   MoveL     *Blank        Quadro111
283400130919     C                   MOVEL(P)  *ALL'9'       Quadro999
283500130919     C                   endif
283600130827     C*
283700130919     C                   if        COGRAR0502 <> *Hival
283800130827     C                   Eval      COGRAR111   = COGRAR0502
283900130827     C                   Eval      COGRAR999   = COGRAR0502
284000130827     C                   else
284100130827     C                   MoveL     *Blank        COGRAR111
284200130919     C                   MOVEL(P)  *ALL'9'       CogRaR999
284300130827     C                   endif
284400130827     C*
284500130919     C                   if        NOMER0502  <> *Hival
284600130827     C                   Eval      NOMER111    = NOMER0502
284700130827     C                   Eval      NOMER999    = NOMER0502
284800130827     C                   else
284900130919     C                   Move      *Blank        NomeR111
285000130919     C                   MOVEL(P)  *ALL'9'       NomeR999
285100130827     C                   endif
285200130827     C*
285300130919     C                   if        DtNasR0502 <> *Hival
285400130828     C                   Eval      DtNasR111   = DtNasR0502
285500130828     C                   Eval      DtNasR999   = DtNasR0502
285600130828     C                   else
285700130828     C                   MoveL     *Loval        DtNasR111
285800130828     C                   Move      WDtMax        DtNasR999
285900130828     C                   endif
286000130827     C*
286100130827     C                   if        LocNaR0502 <> *Hival
286200130827     C                   MoveL     LocNaR0502    LocNaR111
286300130827     C                   MoveL     LocNaR0502    LocNaR999
286400130827     C                   else
286500130827     C                   MoveL     *Blank        LocNaR111
286600130919     C                   MOVEL(P)  *ALL'9'       LocNaR999
286700130827     C                   endif
286800130827     C*
286900130827     C                   if        PrvNaR0502 <> *Hival
287000130827     C                   MoveL     PrvNaR0502    PrvNaR111
287100130827     C                   MoveL     PrvNaR0502    PrvNaR999
287200130827     C                   else
287300130827     C                   MoveL     *Blank        PrvNaR111
287400130919     C                   MOVEL(P)  *ALL'9'       PrvNaR999
287500130827     C                   endif
287600130827     C*
287700130827     C                   if        StatoR0502 <> *Hival
287800130827     C                   MoveL     StatoR0502    StatoR111
287900130827     C                   MoveL     StatoR0502    StatoR999
288000130827     C                   else
288100130827     C                   MoveL     *Blank        StatoR111
288200130919     C                   MOVEL(P)  *ALL'9'       StatoR999
288300130827     C                   endif
288400130827     C*
288500130827     C                   if        CodUIR0502 <> *Hival
288600130918     C                   Z-ADD     CodUIR0502    CodUIR111
288700130918     C                   Z-ADD     CodUIR0502    CodUIR999
288800130827     C                   else
288900130918     C                   Z-ADD     0             CodUIR111
289000130918     C                   Z-ADD     999           CodUIR999
289100130827     C                   endif
289200130827     C*
289300130827     C                   if        LocalR0502 <> *Hival
289400130827     C                   MoveL     LocalR0502    LocalR111
289500130827     C                   MoveL     LocalR0502    LocalR999
289600130827     C                   else
289700130827     C                   MoveL     *Blank        LocalR111
289800130919     C                   MOVEL(P)  *ALL'9'       LocalR999
289900130827     C                   endif
290000130827     C*
290100130827     C                   if        IndiER0502 <> *Hival
290200130827     C                   MoveL     IndiER0502    IndiER111
290300130827     C                   MoveL     IndiER0502    IndiER999
290400130827     C                   else
290500130827     C                   MoveL     *Blank        IndiER111
290600130919     C                   MOVEL(P)  *ALL'9'       IndiER999
290700130827     C                   endif
290800130827     C*
290900130903     C                   if        TpNR0502   <> *Hival
291000130903     C                   MoveL     TpNR0502      TpNR111
291100130903     C                   MoveL     TpNR0502      TpNR999
291200130827     C                   else
291300130903     C                   MoveL     *Blank        TpNR111
291400130919     C                   MOVEL(P)  *ALL'9'       TpNR999
291500130827     C                   endif
291600130903     C*
291700130903     C                   if        PIVAR0502  <> *Hival
291800130903     C                   MoveL     PIVAR0502     PIVAR111
291900130903     C                   MoveL     PIVAR0502     PIVAR999
292000130903     C                   else
292100130903     C                   MoveL     *Blank        PIVAR111
292200130919     C                   MOVEL(P)  *ALL'9'       PIVAR999
292300130919     C                   endif
292400130827     C*
292500130827     C                   if        CdFisR0502 <> *Hival
292600130827     C                   MoveL     CdFisR0502    CdFisR111
292700130827     C                   MoveL     CdFisR0502    CdFisR999
292800130827     C                   else
292900130827     C                   MoveL     *Blank        CdFisR111
293000130919     C                   MOVEL(P)  *ALL'9'       CdFisR999
293100130827     C                   endif
293200130827     C*
293300130925     C                   if        TpOpeR0502 <> *Hival
293400130925     C                   MoveL     TpOpeR0502    TpOpeR111
293500130925     C                   MoveL     TpOpeR0502    TpOpeR999
293600130827     C                   else
293700130925     C                   MoveL     *Blank        TpOpeR111
293800130925     C                   MOVEL(P)  *ALL'9'       TpOpeR999
293900130827     C                   endif
294000130925     C*
294100130925     C                   if        NatOpR0502 <> *Hival
294200130925     C                   MoveL     NatOpR0502    NatOpR111
294300130925     C                   MoveL     NatOpR0502    NatOpR999
294400130925     C                   else
294500130925     C                   MoveL     *Blank        NatOpR111
294600130925     C                   MOVEL(P)  *ALL'9'       NatOpR999
294700130925     C                   endif
294800130827     C*
294900130902     C                   if        LeasiR0502  <> *Hival
295000130902     C                   MoveL     LeasiR0502    LeasiR111
295100130902     C                   MoveL     LeasiR0502    LeasiR999
295200130827     C                   else
295300130902     C                   MoveL     *Blank        LeasiR111
295400130919     C                   MOVEL(P)  *ALL'9'       LeasiR999
295500130827     C                   endif
295600130827     C*
295700130827     C                   if        DtRegR0502  <> *Hival
295800130828     C                   Eval      DtRegR111   = DtRegR0502
295900130828     C                   Eval      DtRegR999   = DtRegR0502
296000130827     C                   else
296100130828     C                   MoveL     *Loval        DtRegR111
296200130828     C                   Move      WDtMax        DtRegR999
296300130827     C                   endif
296400130827     C*
296500130827     C                   if        NrRegR0502  <> *Hival
296600130828     C                   Eval      NrRegR111   = NrRegR0502
296700130828     C                   Eval      NrRegR999   = NrRegR0502
296800130827     C                   else
296900130828     C                   Z-Add     0             NrRegR111
297000130828     C                   Move      *Hival        NrRegR999
297100130827     C                   endif
297200130827     C*
297300130827     C                   if        SerReR0502  <> *Hival
297400130827     C                   MoveL     SerReR0502    SerReR111
297500130827     C                   MoveL     SerReR0502    SerReR999
297600130827     C                   else
297700130827     C                   MoveL     *Blank        SerReR111
297800130919     C                   MOVEL(P)  *ALL'9'       SerReR999
297900130827     C                   endif
298000000000     C*
298100000000     C                   ENDSR
298200000000     C/EJECT
298300130902     C************************************************************
298400130903     C* Inizializzo i totali del quadro TA
298500130902     C************************************************************
298600130903     C     InzTotali     BEGSR
298700130902     C*
298800130903     C                   Clear                   $NRCPFATA
298900130903     C                   Clear                   $NRCLSATA
299000130903     C                   Clear                   $NRBLBLTA
299100130903     C                   Clear                   $NRNRBLTA
299200130903     C                   Clear                   $NRSRBLTA
299300130903     C                   Clear                   $NRFEFETA
299400130903     C                   Clear                   $NRRIFETA
299500130903     C                   Clear                   $NRFRFRTA
299600130903     C                   Clear                   $NRRIFRTA
299700130903     C                   Clear                   $NRNONETA
299800130903     C                   Clear                   $NRNONRTA
299900131015R135 X****               Clear                   $NRCLDFTA
300000131015R135 C                   Clear                   $NRRIDFTA
300100130903     C                   Clear                   $NRRIFNTA
300200130903     C                   Clear                   $NRRISETA
300300130903     C                   Clear                   $NRCLTUTA
300400130903     C*
300500130903     C                   ENDSR
300600130903     C/EJECT
300700130903     C************************************************************
300800130903     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
300900130903     C************************************************************
301000130903     C     SETANA        BEGSR
301100130903     C*
301200000000     C* disallocazione eventuale rcd precedentemente allocato
301300130826     C                   if        TpList0502  = '01'
301400130826     C                   UNLOCK    SDGSCF01L                            22
301500130826     C                   else
301600130826     C                   UNLOCK    SDGSCF02L                            22
301700130826     C                   endif
301800000000     C*
301900130826     C                   Eval      SCFSocieta  = Soc0502
302000130827     C                   Eval      SCFTpCom    = TpCom0502
302100130826     C                   Eval      SCFAnno     = Anno0502
302200130827     C                   Eval      SCFMese     = Mese0502
302300130826     C                   if        TpList0502  = '01'
302400131108R140 C                   if        CogRag0502 <> *Hival
302500131108 "   C                   Eval      SCFCogRag   = CogRag0502
302600131108 "   C     K05SCF01      SETLL     SDGSCF01L
302700131108R140 C                   else
302800131108     C     K04SCF01      SETLL     SDGSCF01L
302900131108R140 C                   endif
303000130826     C                   else
303100130826     C     K04SCF02      SETLL     SDGSCF02L
303200130826     C                   endif
303300000000     C*
303400000000     C                   ENDSR
303500000000     C/EJECT
303600130827     C************************************************************
303700130827     C* Lettura dati estratti
303800130827     C************************************************************
303900130827     C     REDANA        BEGSR
304000130827     C*
304100130827     C                   if        Opz0502     = '02'    or
304200130827     C                             Opz0502     = '04'    or
304300130827     C                             Opz0502     = '44'    or
304400130827     C                             Opz0502     = '01'
304500130827     C* lista  a video
304600130827     C                   if        TpList0502  = '01'
304700131108R140 C                   if        CogRag0502 <> *Hival
304800150309R147 C                   Eval      SCFCogRag   = CogRag0502
304900150309R140 C     K05SCF01      READE     SDGSCF01L                            2221
305000131108R140 C                   else
305100131108     C     K04SCF01      READE     SDGSCF01L                            2221
305200131108R140 C                   endif
305300130827     C                   else
305400130827     C* lista su tracciato
305500130827     C     K04SCF02      READE     SDGSCF02L                            2221
305600130827     C                   endif
305700130827     C*
305800130827     C* RCD allocato
305900130829     C                   if        *IN22       = *ON         or
306000130829     C* RCD non trovato
306100130829     C                             *IN21       = *ON
306200130827    >C                   MOVE      '1'           ERR0502
306300130827    >C                   MOVE      '1'           End0502
306400130827     C                   ENDIF
306500130827     C*
306600130827     C                   else
306700130827     C* lista  a video
306800130827     C                   if        TpList0502  = '01'
306900131108R140 C                   if        CogRag0502 <> *Hival
307000131108 "   C     K05SCF01      READE(N)  SDGSCF01L                              21
307100131108R140 C                   else
307200131108     C     K04SCF01      READE(N)  SDGSCF01L                              21
307300131108R140 C                   endif
307400130827     C                   else
307500130827     C* lista su tracciato
307600130827     C     K04SCF02      READE(N)  SDGSCF02L                              21
307700130829     C*
307800130827     C                   endif
307900130829     C*
308000130829     C                   endif
308100130827     C*
308200130827     C                   Eval      $FineSCF    = *In21
308300130827     C*
308400130827     C                   ENDSR
308500130827     C/EJECT
308600130827     C************************************************************
308700130827     C* Selezione dei dati estratti
308800130827     C************************************************************
308900130827     C     SELANA        BEGSR
309000130827     C*
309100130827     C                   Eval      $RcdOk      = *On
309200130827     C*
309300130827     C                   Select
309400130827     C*
309500130827     C                   When      SCFAnn      < $AnnMin       or
309600130827     C                             SCFAnn      > Ann999
309700130827     C                   Eval      $RcdOk      = *Off
309800130827     C*
309900130827     C                   When      SCFTpAnn    < TpAnn111      or
310000130827     C                             SCFTpAnn    > TpAnn999
310100130827     C                   Eval      $RcdOk      = *Off
310200130827     C*
310300130827     C                   When      SCFCF       < CF111         or
310400130827     C                             SCFCF       > CF999
310500130827     C                   Eval      $RcdOk      = *Off
310600131206R141 C*
310700131206 "   C                   When      SCFCausale  < Causal111     or
310800131206 "   C                             SCFCausale  > Causal999
310900131206R141 C                   Eval      $RcdOk      = *Off
311000130827     C*
311100130827     C                   When      SCFSogg     < Sogg111       or
311200130827     C                             SCFSogg     > Sogg999
311300130827     C                   Eval      $RcdOk      = *Off
311400130827     C*
311500130903     C                   When      SCFTpN      < TpN111        or
311600130903     C                             SCFTpN      > TpN999
311700130827     C                   Eval      $RcdOk      = *Off
311800130903     C*
311900130903     C                   When      SCFPartIVA  < PIVA111       or
312000130903     C                             SCFPartIVA  > PIVA999
312100130903     C                   Eval      $RcdOk      = *Off
312200130827     C*
312300130827     C                   When      SCFCdFisc   < CdFisc111     or
312400130827     C                             SCFCdFisc   > CdFIsc999
312500130827     C                   Eval      $RcdOk      = *Off
312600130827     C*
312700130827     C                   When      SCFCogRag   < CogRag111     or
312800130827     C                             SCFCogRag   > CogRag999
312900130827     C                   Eval      $RcdOk      = *Off
313000130827     C*
313100130827     C                   When      SCFNome     < Nome111       or
313200130827     C                             SCFNome     > Nome999
313300130827     C                   Eval      $RcdOk      = *Off
313400130827     C*
313500130827     C                   When      SCFDtNasc   < DtNasc111     or
313600130827     C                             SCFDtNasc   > DtNasc999
313700130827     C                   Eval      $RcdOk      = *Off
313800130827     C*
313900130828     C                   When      SCFLocNasc  < LocNas111     or
314000130828     C                             SCFLocNasc  > LocNas999
314100130827     C                   Eval      $RcdOk      = *Off
314200130827     C*
314300130828     C                   When      SCFPrvNasc  < PrvNas111     or
314400130828     C                             SCFPrvNasc  > PrvNas999
314500130827     C                   Eval      $RcdOk      = *Off
314600130827     C*
314700130827     C                   When      SCFStato    < Stato111      or
314800130827     C                             SCFStato    > Stato999
314900130827     C                   Eval      $RcdOk      = *Off
315000130827     C*
315100130827     C                   When      SCFCodUIC   < CodUIC111     or
315200130827     C                             SCFCodUIC   > CodUIC999
315300130827     C                   Eval      $RcdOk      = *Off
315400130827     C*
315500130828     C                   When      SCFLocalit  < Locali111     or
315600130828     C                             SCFLocalit  > Locali999
315700130827     C                   Eval      $RcdOk      = *Off
315800130827     C*
315900130827     C                   When      SCFIndirE   < IndirE111     or
316000130827     C                             SCFIndirE   > IndirE999
316100130827     C                   Eval      $RcdOk      = *Off
316200130919     C*
316300130919     C                   When      SCFTpOpe    < TpOpe111      or
316400130919     C                             SCFTpOpe    > TpOpe999
316500130919     C                   Eval      $RcdOk      = *Off
316600130827     C*
316700130827     C                   When      SCFQuadro   < Quadro111     or
316800130827     C                             SCFQuadro   > Quadro999
316900130827     C                   Eval      $RcdOk      = *Off
317000130827     C*
317100130827     C                   When      SCFCogRagR  < CogRaR111     or
317200130827     C                             SCFCogRagR  > CogRaR999
317300130827     C                   Eval      $RcdOk      = *Off
317400130827     C*
317500130827     C                   When      SCFNomeR    < NomeR111      or
317600130827     C                             SCFNomeR    > NomeR999
317700130827     C                   Eval      $RcdOk      = *Off
317800130827     C*
317900130827     C                   When      SCFDtNasR   < DtNasR111    or
318000130827     C                             SCFDtNasR   > DtNasR999
318100130827     C                   Eval      $RcdOk      = *Off
318200130827     C*
318300130827     C                   When      SCFLocNasR  < LocNaR111     or
318400130827     C                             SCFLocNasR  > LocNaR999
318500130827     C                   Eval      $RcdOk      = *Off
318600130827     C*
318700130827     C                   When      SCFPrvNasR  < PrvNaR111     or
318800130827     C                             SCFPrvNasR  > PrvNaR999
318900130827     C                   Eval      $RcdOk      = *Off
319000130827     C*
319100130827     C                   When      SCFStatoR   < StatoR111     or
319200130827     C                             SCFStatoR   > StatoR999
319300130827     C                   Eval      $RcdOk      = *Off
319400130827     C*
319500130828     C                   When      SCFCodUICR  < CodUIR111     or
319600130828     C                             SCFCodUICR  > CodUIR999
319700130827     C                   Eval      $RcdOk      = *Off
319800130827     C*
319900130827     C                   When      SCFLocaliR  < LocalR111    or
320000130827     C                             SCFLocaliR  > LocalR999
320100130827     C                   Eval      $RcdOk      = *Off
320200130827     C*
320300130828     C                   When      SCFIndirER  < IndiER111     or
320400130828     C                             SCFIndirER  > IndiER999
320500130827     C                   Eval      $RcdOk      = *Off
320600130827     C*
320700130903     C                   When      SCFTpNR     < TpNR111       or
320800130903     C                             SCFTpNR     > TpNR999
320900130827     C                   Eval      $RcdOk      = *Off
321000130903     C*
321100130903     C                   When      SCFPIVAR    < PIVAR111      or
321200130903     C                             SCFPIVAR    > PIVAR999
321300130903     C                   Eval      $RcdOk      = *Off
321400130827     C*
321500130827     C                   When      SCFCdFiscR  < CdFisR111     or
321600130827     C                             SCFCdFiscR  > CdFIsR999
321700130827     C                   Eval      $RcdOk      = *Off
321800130925     C*
321900130925     C                   When      SCFTpOpeR   < TpOpeR111     or
322000130925     C                             SCFTpOpeR   > TpOpeR999
322100130925     C                   Eval      $RcdOk      = *Off
322200130827     C*
322300130902     C                   When      SCFNatOpeR  < NatOpR111     or
322400130902     C                             SCFNatOpeR  > NatOpR999
322500130827     C                   Eval      $RcdOk      = *Off
322600130827     C*
322700130902     C                   When      SCFLeasinR  < LeasiR111     or
322800130902     C                             SCFLeasinR  > LeasiR999
322900130827     C                   Eval      $RcdOk      = *Off
323000130827     C*
323100130827     C                   When      SCFDtRegR   < DtRegR111     or
323200130827     C                             SCFDtRegR   > DtRegR999
323300130827     C                   Eval      $RcdOk      = *Off
323400130827     C*
323500130827     C                   When      SCFNrRegR   < NrRegR111     or
323600130827     C                             SCFNrRegR   > NrRegR999
323700130827     C                   Eval      $RcdOk      = *Off
323800130827     C*
323900130828     C                   When      SCFSerRegR  < SerReR111     or
324000130828     C                             SCFSerRegR  > SerReR999
324100130827     C                   Eval      $RcdOk      = *Off
324200130827     C*
324300130827     C*
324400130827     C                   When      Not (
324500130827     C                             (Kcc10502    = *Blank     and
324600130827     C                              Kcc20502    = *Blank     and
324700130827     C                              Kcc30502    = *Blank     and
324800130827     C                              Kcc40502    = *Blank     and
324900130827     C                              Kcc50502    = *Blank     and
325000130827     C                              Kcc60502    = *Blank       )
325100130827     C                             or
325200130827     C                             ((Kcc10502   <> *Blank    and
325300130827     C                               SCFKcc      = Kcc10502     ) or
325400130827     C                              (Kcc20502   <> *Blank    and
325500130827     C                               SCFKcc      = Kcc20502     ) or
325600130827     C                              (Kcc30502   <> *Blank    and
325700130827     C                               SCFKcc      = Kcc30502     ) or
325800130827     C                              (Kcc40502   <> *Blank    and
325900130827     C                               SCFKcc      = Kcc40502     ) or
326000130827     C                              (Kcc50502   <> *Blank    and
326100130827     C                               SCFKcc      = Kcc50502     ) or
326200130827     C                              (Kcc60502   <> *Blank    and
326300130827     C                               SCFKcc      = Kcc60502     )   )
326400130827     C                             )
326500130827     C                   Eval      $RcdOk      = *Off
326600130827     C*
326700130827     C                   When      Not (
326800130827     C                             (Kcc0502     = *Blank    and
326900130827     C                              Ksc0502     = *Blank       )
327000130827     C                             or
327100130827     C                             (Kcc0502    <> *Blank    and
327200130827     C                              SCFKcc      = Kcc0502   and
327300130827     C                              Ksc0502    <> *Blank    and
327400130827     C                              SCFKsc      = Ksc0502      )
327500130827     C                             )
327600130827     C                   Eval      $RcdOk      = *Off
327700130827     C*
327800130827     C                   endSl
327900130827     C*
328000130827     C                   ENDSR
328100130827     C/EJECT
328200000000     C************************************************************
328300000000     C* DEFINIZIONE KLIST
328400000000     C************************************************************
328500000000     C     DefKlist      BEGSR
328600000000     C*
328700000000     C* klist
328800000000     C*
328900130826     C* SDGSCF01L
329000131108     C     K04SCF01      KLIST
329100131108     C                   KFLD                    SCFSocieta
329200131108     C                   KFLD                    SCFTpCom
329300131108     C                   KFLD                    SCFAnno
329400131108     C                   KFLD                    SCFMese
329500131108R140 C*
329600131108 "   C* SDGSCF01L
329700131108 "   C     K05SCF01      KLIST
329800131108 "   C                   KFLD                    SCFSocieta
329900131108 "   C                   KFLD                    SCFTpCom
330000131108 "   C                   KFLD                    SCFAnno
330100131108 "   C                   KFLD                    SCFMese
330200131108R140 C                   KFLD                    SCFCogRag
330300130826     C*
330400130826     C* SDGSCF02L
330500130826     C     K04SCF02      KLIST
330600130826     C                   KFLD                    SCFSocieta
330700130826     C                   KFLD                    SCFTpCom
330800130826     C                   KFLD                    SCFAnno
330900130826     C                   KFLD                    SCFMese
331000000000     C*
331100000000     C                   ENDSR
331200000000     C/EJECT
331300000000     C************************************************************
331400000000     C* FINE PROGRAMMA
331500000000     C************************************************************
331600000000     C     ENDPGM        BEGSR
331700000000     C*
331800000000     C                   Sub       1             $NrTrovati
331900000000     C*
332000000000     C                   if        $NrTrovati   < 0
332100130826     C                   Eval      End0502      = *On
332200000000     C                   endif
332300130923     C*
332400131025R138 X* R137             if        END0502      =  *On
332500131025R137 C                   if        OPZ0502      = '02'       or
332600131025R138 C                             OPZ0502      = '01'       or
332700131025R137 C                             OPZ0502      = '04'
332800131023 "   C                   if        TpList0502  = '01'
332900131025R137 C                   UNLOCK    SDGSCF01L
333000131025R138 C                   CLOSE     SDGSCF01L
333100131025R137 C                   else
333200131025R137 C                   UNLOCK    SDGSCF02L
333300131025R138 C                   CLOSE     SDGSCF02L
333400131025R137 C                   endif
333500131025R137 C                   endif
333600131025R138 X* R137             endif
333700131023R137 C*
333800130923     C                   if        $FineSCF    = *On         and
333900130923     C                             END0502    <> *On         and
334000130923     C                             OPZ0502     = '05'
334100130923     C                   EXSR      RieOutput
334200130923     C                   if        Rott0502    = '02'
334300130923     C                   EXSR      RieQuadri
334400130923     C                   endif
334500130923     C                   endif
334600000000     C*
334700130826     C                   if        End0502    <> *On           and
334800130826     C                             ((OPZ0502     = '05'   and
334900110822     C* lista
335000130826     C                              TpRott0502   = '01'      )
335100110822     C                             or
335200130826     C                              OPZ0502     = '01'        )
335300000000     C                   Eval      *InRT       = *On
335400000000     C                   else
335500130826     C                   if        TpList0502  = '01'
335600130826     C                   Close     SDGSCF01L
335700130826     C                   else
335800130826     C                   Close     SDGSCF02L
335900130826     C                   endif
336000000000     C                   Eval      *InLR       = *On
336100000000     C                   endif
336200000000     C*
336300000000     C                   RETURN
336400000000     C*
336500000000     C                   ENDSR
336600000000     C/EJECT
336700130828     C************************************************************
336800130828     C* Imposto i dati di output per il Quadro TA
336900130828     C* RIEPILOGO
337000130828     C************************************************************
337100130828     C     RieQuadro_TA  BEGSR
337200130828     C*
337300130828     C                   Clear                   SDG005TADS
337400130828     C*
337500130828     C                   Eval      QUADRO05TA  = SavQuadro
337600130903     C                   Eval      NRCPFA05TA  = $NRCPFATA
337700130903     C                   Eval      NRCLSA05TA  = $NRCLSATA
337800130903     C                   Eval      NRBLBL05TA  = $NRBLBLTA
337900130903     C                   Eval      NRNRBL05TA  = $NRNRBLTA
338000130903     C                   Eval      NRSRBL05TA  = $NRSRBLTA
338100130903     C                   Eval      NRFEFE05TA  = $NRFEFETA
338200130903     C                   Eval      NRRIFE05TA  = $NRRIFETA
338300130903     C                   Eval      NRFRFR05TA  = $NRFRFRTA
338400130903     C                   Eval      NRRIFR05TA  = $NRRIFRTA
338500130903     C                   Eval      NRNONE05TA  = $NRNONETA
338600130903     C                   Eval      NRNONR05TA  = $NRNONRTA
338700131015R135 X****               Eval      NRCLDF05TA  = $NRCLDFTA
338800131015R135 C                   Eval      NRRIDF05TA  = $NRRIDFTA
338900130903     C                   Eval      NRRIFN05TA  = $NRRIFNTA
339000130903     C                   Eval      NRRISE05TA  = $NRRISETA
339100130903     C                   Eval      NRCLTU05TA  = $NRCLTUTA
339200130828     C*
339300130828     C                   ENDSR
339400130828     C/EJECT
