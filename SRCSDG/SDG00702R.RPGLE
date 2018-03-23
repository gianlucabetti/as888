000100000000     H*BIND
000200000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*
000500130826      *           -----------------------------------------
000600170727      *                Spesometro Trimestrale
000700170727      *              Aggiornamento dei dati anagrafici
000800130826      *           -----------------------------------------
000900130826      *
001000000000     C*----------------------------------------------------*
001100130827     C*
001200170726     C* TPCOM0702 - tipo comunicazione dello Spesometro:
001300170726     C*             T  = Spesometro trimestrale
001400130827     C*             (non gestito)
001500000000     C*
001600170728     C* OPZ0702   - indica se si lavora in:
001700170728     C*
001800170728     C*             01 = inserimento
001900000000     C*
002000000000     C*             02 = aggiornamento
002100000000     C*                  (serve in manutenzione dati di testata, per
002200000000     C*                   aggiornare massivamente i record SELEZIONATI
002300000000     C*                   In questo caso i dati "nuovi" sono nella DS
002400130826     C*                   SDG00592DS)
002500000000     C*
002600000000     C*             04 = cancellazione
002700000000     C*                  (serve in manutenzione dati di testata, per
002800000000     C*                   cancellare massivamente i record SELEZIONATI)
002900000000     C*
003000000000     C*             44 = annullamento/ripristino
003100000000     C*                  (serve in lista dati di testata e in lista
003200000000     C*                   righe di dettaglio per annullare/
003300000000     C*                   ripristinare massivamente i record SELEZIONATI)
003400000000     C*
003500000000     F*----------------------------------------------------*
003600000000     F* Movimenti estratti
003700170726     F* Key: Soc/TpCom/Anno/Trim/Mese/Tipo rec./dati anagrafici
003800170808     FSDGCST02L IF   E           K DISK
003900170807     F                                     USROPN
004000170726     F                                     RENAME(SDGCST000:SDGCST02)
004100170808     F                                     INFDS(NRRDS)
004200170808     FSDGCST00F UF A E             DISK
004300170727     F                                     RENAME(SDGCST000:SDGCST00)
004400170808     F                                     PREFIX(XXX:3)
004500170808     F                                     COMMIT
004600170808     F                                     USROPN
004700170808     F                                     INFDS(NRRDS2)
004800170818     F*------------
004900170818     F* Catologo File Spesometro Trimestrale
005000170818     F* Key: Soc/TpCom/TpRec/A/T/M/PrgEstr     FIFO
005100170818     FSDGCSF03L UF A E           K DISK    UsrOpn
005200170818     F                                     COMMIT
005300170818     F*-------------
005400170808     D*Num.relativo record per SDGCST00F
005500170808     D NRRDS2          DS           512
005600170808     D  NrRCST2              397    400B 0
005700170818     D*-------------
005800000000     D* Passaggio parametri
005900000000     D KPJBA         E DS
006000000000     D*-------------
006100000000     D* Dati di ambiente ottenuti da SDGSOCDR
006200000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
006300000000     D*-------------
006400000000     D* DS Interna per dati di output di SDGSOCDR
006500000000     D XSOCDS          DS          1000
006600000000     D*-------------
006700170726     D* Parametri
006800170726     D SDG00702DS    E DS
006900000000     D*-------------
007000000000     D* Reperimento nome PGM
007100000000     D STATUS         SDS           333
007200000000     D  DSPGM            *PROC
007300000000     D  PARMS            *PARMS
007400000000     D*-------------
007500000000     D*  campi per parzializzazioni su rcd
007600000000     D*-------------
007700000000     D* campi di lavoro
007800000000     D WDtMax          S               D   Inz(*Hival)
007900000000     D WDataAlfa       S             10A
008000000000     D UDateISO        S               D
008100170726     D $FineCST        S              1A
008200000000     D $RcdOk          S              1A
008300170726     D SavCliFor       S                   Like(cstclifor)
008400170726     D Ann999          s                   Like(cstAnn  )
008500170726     D AnnMin999       S                   like(CstAnn)
008600170726     D $AnnMin         S                   like(CstAnn)
008700170728     D $Anno           S              4A
008800170728     D $Trim           S              1A
008900130902     D*--------------
009000170808     D*Num.relativo record per SDGCST02L
009100170808     D NRRDS           DS           512
009200170808     D  NrRCST               397    400B 0
009300170809     D*------
009400170809     D $Esito          S              1
009500170809     D $Prenot         S              1
009600170809     D $Fine           S              1
009700170809     D*------
009800170809     D $Sys            S                    Like(CSTSys    )
009900170809     D $NrAsReg        S                    Like(CSTNrAsReg)
010000000000     C*----------------------------------------------------*
010100000000     C*                MAIN LINE PROGRAM
010200000000     C*----------------------------------------------------*
010300000000     C*
010400130827     C                   EXSR      INZVAR
010500130902     C*
010600170727     C                   Select
010700170727     C* Chiamata con il numero assoluto di record (da dettaglio)
010800170727     C                   When      NRRCST0702  <> 0
010900170727     C                   EXSR      ElaDetta
011000170727     C                   Other
011100170727     C* Chiamata da anagrafica (da testata)
011200170727     C                   EXSR      ElaTesta
011300170727     C                   Endsl
011400170726     C                   EXSR      GESAGG
011500000000     C*
011600130827     C                   EXSR      ENDPGM
011700170727     C/EJECT
011800170727     C************************************************************
011900170727     C* Elaboro dal dettaglio di riga
012000170727     C************************************************************
012100170727     C     ElaDetta      BEGSR
012200170727     C*
012300170727     C     NRRCST0702    Chain     sdgcst00f                          2322
012400170727     C*
012500170808     C                   If        *in23 or *in22
012600170728     C                   Movel     *on           Err0702
012700170727     C                   Exsr      EndPgm
012800170727     C                   Endif
012900170727     C*
013000170727     C                   Select
013100170727     C*
013200170727     C                   When      Opz0702     = '02'
013300170727     C                   EXSR      RieSDGCST
013400170728     C                   Update    SDGCST00                             99
013500170807     C                   Feod      SDGCST00F                            99
013600170727     C*
013700170727     C                   When      Opz0702     = '04'
013800170728     C                   Delete    SDGCST00                             99
013900170807     C                   Feod      SDGCST00F                            99
014000170727     C*
014100170727     C                   When      Opz0702     = '44'
014200170727     C                   EXSR      annSDGCST
014300170728     C                   Update    SDGCST00                             99
014400170807     C                   Feod      SDGCST00F                            99
014500170727     C*
014600170727     C                   endSl
014700170728     C*
014800170728     C                   If        *in99 = *on
014900170728     C                   Movel     '1'           Err0702
015000170728     C                   Exsr      EndPgm
015100170728     C                   Endif
015200170727     C*
015300170727     C
015400170727     C                   Endsr
015500170727     C/EJECT
015600170727     C************************************************************
015700170727     C* Elaboro dalla testata anagrafica
015800170727     C************************************************************
015900170727     C     ElaTesta      BEGSR
016000170809     C*
016100170809     C* INSERIMENTO
016200170809     C*
016300170808     C                   IF        Opz0702     = '01'
016400170808     C                   Clear                   SDGCST00
016500170808     C                   EXSR      RieSDGCST
016600170809     C* Reperisco il numero assoluto della registrazione fittizia
016700170809     C                   EXSR      RepNrAss
016800170808     C* gestione catalogo
016900170808     C                   EXSR      GesSDGCSF
017000170809     C*
017100170808     C                   Write     SDGCST00                             99
017200170809     C*
017300170809     C                   If        *in99
017400170809    >C                   MOVE      '1'           ERR0702
017500170809     C                   EXSR      ENDPGM
017600170809     C                   Endif
017700170808     C*
017800170808     C                   EVAL      NRRCST0702= NRRCST2
017900170808     C*
018000170808     C                   ELSE
018100170808     C* MODIFICA
018200170808     C
018300170727     C                   EXSR      SETANA
018400170727     C                   EXSR      REDANA
018500170727     C*
018600170727     C                   DoW       $FineCST    = *Off
018700170727     C*
018800170727     C                   EXSR      SELANA
018900170727     C*
019000170727     C                   if        $RcdOk      = *On
019100170728     C*
019200170727     C                   Select
019300170728     C*
019400170728     C                   When      Opz0702     = '01'
019500170808     C                   Clear                   SDGCST00
019600170728     C                   EXSR      RieSDGCST
019700170728     C* gestione catalogo
019800170728     C                   EXSR      GesSDGCSF
019900170808     C                   Write     SDGCST00                             99
020000170727     C*
020100170727     C                   When      Opz0702     = '02'
020200170727     C                   EXSR      RieSDGCST
020300170808     C                   Update    SDGCST00                             99
020400170808     C                   Feod      SDGCST00F                            99
020500170727     C*
020600170727     C                   When      Opz0702     = '04'
020700170808     C                   Delete    SDGCST00                             99
020800170808     C                   Feod      SDGCST00F                            99
020900170727     C*
021000170727     C                   When      Opz0702     = '44'
021100170727     C                   EXSR      annSDGCST
021200170808     C                   Update    SDGCST00                             99
021300170808     C                   Feod      SDGCST00F                            99
021400170727     C*
021500170727     C                   endSl
021600170728     C*
021700170728     C                   If        *in99
021800170728    >C                   MOVE      '1'           ERR0702
021900170728     C                   EXSR      ENDPGM
022000170728     C                   Endif
022100170727     C*
022200170727     C                   endif
022300170727     C*
022400170727     C                   EXSR      REDANA
022500170728     C*
022600170727     C                   endDo
022700170808     C*
022800170808     C                   Endif
022900170727     C*
023000170727     C                   Endsr
023100170727     C/EJECT
023200170809     C***********************************************************
023300170809     C* Reperisco il numero assoluto di registrazione
023400170809     C***********************************************************
023500170809     C     RepNrAss      BEGSR
023600170809     C*
023700170809     C                   CALLB     'NDMVC101'
023800170809     C                   PARM      *Off          $Esito
023900170809     C                   PARM      0             $Sys
024000170809     C                   PARM      0             $NrAsReg
024100170809     C                   PARM      '1'           $Prenot
024200170809     C*
024300170809     C                   If        $Esito     =  *On
024400170809    >C                   MOVE      '1'           ERR0702
024500170809     C                   MOVE      *on           $Fine
024600170809     C                   EXSR      ENDPGM
024700170809     C                   endif
024800170809     C*
024900170809     C                   Eval      XXXSys      = $Sys
025000170809     C                   Eval      XXXNrAsReg  = $NrAsReg
025100170809     C                   Eval      XXXNrRigam  = 1
025200170809     C                   Eval      XXXNrRiga   = 1
025300170809     C*
025400170809     C                   ENDSR
025500170809      /EJECT
025600170726     C************************************************************
025700170726     C* Annullo/ripristino a seconda dello stato di partenza
025800170726     C************************************************************
025900170726     C     AnnSDGCST     BEGSR
026000170726     C*
026100170808     C                   Eval      XXXORIGINE  = 'M'
026200170808     C                   Eval      XXXUTEULMO  = XScCUt
026300170808     C                   Eval      XXXDTULMO   = UDateIso
026400170726     C*
026500170726     C                   Select
026600170726     C*
026700170726     C* Annulla
026800170726     C                   When      AnnAtt0702  = *Blank
026900170808     C                   Eval      XXXAnn      = '1'
027000170808     C                   Eval      XXXTpAnn    = '00'
027100170726     C*
027200170726     C* Ripristina
027300170726     C                   When      AnnAtt0702  = '1'
027400170808     C                   Eval      XXXAnn      = *Blank
027500170808     C                   Eval      XXXTpAnn    = *Blank
027600170726     C*
027700170726     C                   endSl
027800170726     C*
027900170726     C                   ENDSR
028000170726     C/EJECT
028100170728     C********************************************************************
028200170728     C* Gestisco la scrittura del catalogo per il periodo
028300170728     C********************************************************************
028400170728     C     GESSDGCSF     BEGSR
028500170728     C*
028600170808     C                   Eval      CSFSocieta  = XXXSocieta
028700170808     C                   Eval      CSFTpCom    = XXXTpCom
028800170810     C                   Eval      CSFTpRec    = XXXTpRec
028900170808     C                   Eval      CSFAnno     = XXXAnno
029000170808     C                   Eval      CSFTrim     = XXXTrim
029100170808     C                   Eval      CSFMese     = XXXMese
029200170728     C                   Eval      CSFPrgEstr  = 0
029300170728     C*
029400170728     C     K07CSF03      CHAIN     SDGCSF03L                          25
029500170728     C*
029600170728     C* se non trovato,
029700170728     C* scrivo un record per il periodo
029800170728     C                   if        *In25       = *On
029900170728     C*
030000170728     C                   Clear                   SDGCSF000
030100170728     C*
030200170808     C                   Eval      CSFUteImm   = XXXUteImm
030300170808     C                   Eval      CSFDtImm    = XXXDtImm
030400170808     C                   Eval      CSFUteUlMo  = XXXUteUlMo
030500170808     C                   Eval      CSFDtUlMo   = XXXDtUlMo
030600170728     C*
030700170728     C                   Eval      CSFCF       = *Blank
030800170728     C* Automatica
030900170808     C                   Eval      CSFOrigine  = XXXOrigine
031000170808     C                   Eval      CSFSocieta  = XXXSocieta
031100170808     C                   Eval      CSFTpCom    = XXXTpCom
031200170728     C                   Eval      CSFTpRec    = *Blank
031300170808     C                   Eval      CSFAnno     = XXXAnno
031400170808     C                   Eval      CSFTrim     = XXXTrim
031500170808     C                   Eval      CSFMese     = XXXMese
031600170728     C*
031700170808     C                   Eval      CSFPrgEstr  = XXXPrgEstr
031800170728     C*
031900170728     C                   MoveL     CsfAnno       $Anno
032000170728     C                   MoveL     CsfTrim       $Trim
032100170728     C                   Eval      CSFNote     = 'Spesometro Trimestrale'+
032200170728     C                             ' Dati estratti ' + $Anno + ' trim. ' +
032300170728     C                             $Trim
032400170728     C*
032500170728     C                   WRITE     SDGCSF000                            99
032600170728     C*
032700170728     C                   If        *in99
032800170728    >C                   MOVE      *On           ERR0702
032900170728     C                   EXSR      ENDPGM
033000170728     C                   Endif
033100170728     C*
033200170728     C                   endif
033300170728     C*
033400170728     C                   ENDSR
033500170728     C/EJECT
033600000000     C************************************************************
033700170726     C* Riempio campi descrittivi di SDGCST00F
033800000000     C************************************************************
033900170726     C     RieSDGCST     BEGSR
034000170728     C* Se Inserimento
034100170728     C                   If        OPz0702 = '01'
034200170808     C                   Eval      XXXUTEIMM   = XScCUt
034300170808     C                   Eval      XXXDTIMM    = UDateIso
034400170808     C                   Eval      XXXSOCIETA  = Soc0702
034500170808     C                   Eval      XXXTPCOM    = TpCom0702
034600170808     C                   Eval      XXXANNO     = Anno0702
034700170808     C                   Eval      XXXTRIM     = Trim0702
034800170808     C                   Eval      XXXMESE     = Mese0702
034900170808     C                   Eval      XXXTPREC    = Tprec0702
035000170728     C                   Endif
035100170728     C*
035200170808     C                   Eval      XXXORIGINE  = 'M'
035300170808     C                   Eval      XXXUTEULMO  = XScCUt
035400170808     C                   Eval      XXXDTULMO   = UDateIso
035500000000     C*
035600170808     C                   Eval      XXXSOGG     = SOGG0702
035700170808     C                   Eval      XXXCF       = CF0702
035800170808     C                   Eval      XXXPARTIVA  = Partiv0702
035900170808     C                   Eval      XXXCDFISC   = CdFisc0702
036000170808     C                   Eval      XXXSTATO1   = Stato10702
036100170808     C                   Eval      XXXIDPAES1  = Idpae10702
036200170808     C                   Eval      XXXPIVA1    = Piva10702
036300170808     C                   Eval      XXXCDFISC1  = CdFis10702
036400170808     C                   Eval      XXXCOGRAG1  = Cogra10702
036500170808     C                   Eval      XXXTPN1     = Tpn10702
036600170808     C                   Eval      XXXDENOM1   = Denom10702
036700170808     C                   Eval      XXXCOGNOM1  = COGNO10702
036800170808     C                   Eval      XXXNOME1    = NOME10702
036900170726     C*
037000170726     C* Sede
037100170726     C*
037200170808     C                   Eval      XXXINDIR2   = INDIR20702
037300170808     C                   Eval      XXXNRCIV2   = NRCIV20702
037400170808     C                   Eval      XXXCAP2     = CAP20702
037500170808     C                   Eval      XXXLOCALI2  = LOCAL20702
037600170808     C                   Eval      XXXPROV2    = PROV20702
037700170808     C                   Eval      XXXSTATO2   = STATO20702
037800170808     C                   Eval      XXXIDPAES2  = IDPAE20702
037900170726     C*
038000170726     C* Stabile organizzazione
038100170726     C*
038200170808     C                   Eval      XXXINDIR3   = INDIR30702
038300170808     C                   Eval      XXXNRCIV3   = NRCIV30702
038400170808     C                   Eval      XXXCAP3     = CAP30702
038500170808     C                   Eval      XXXLOCALI3  = LOCAL30702
038600170808     C                   Eval      XXXPROV3    = PROV30702
038700170808     C                   Eval      XXXSTATO3   = STATO30702
038800170808     C                   Eval      XXXIDPAES3  = IDPAE30702
038900170726     C*
039000170726     C* Rappresentante Fiscale
039100170726     C*
039200170808     C                   Eval      XXXSTATO4   = STATO40702
039300170808     C                   Eval      XXXIDPAES4  = IDPAE40702
039400170808     C                   Eval      XXXPIVA4    = PIVA40702
039500170808     C                   Eval      XXXCOGRAG4  = COGRA40702
039600170808     C                   Eval      XXXTPN4     = TPN40702
039700170808     C                   Eval      XXXDENOM4   = DENOM40702
039800170808     C                   Eval      XXXCOGNOM4  = COGNO40702
039900170808     C                   Eval      XXXNOME4    = NOME40702
040000170807     C*
040100000000     C                   ENDSR
040200000000     C/EJECT
040300170726     C************************************************************
040400170726     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
040500170726     C************************************************************
040600170726     C     GESAGG        BEGSR
040700170726     C*
040800170726     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
040900170726    >C     CMT0702       IFEQ      '0'
041000170726     C                   COMMIT
041100170726     C                   ENDIF
041200170726     C*
041300170726     C* segnalo al pgm chiamante l'avvenuta conferma
041400170726    >C                   MOVE      *ON           OPR0702
041500170726     C* segnalo al pgm chiamante l'aver premuto l'F6
041600170726    >C                   MOVE      '0'           RET0702
041700170726     C*
041800170726     C                   ENDSR
041900170726     C/EJECT
042000000000     C************************************************************
042100000000     C* OPERAZIONI INIZIALI
042200000000     C************************************************************
042300000000     C     *INZSR        BEGSR
042400000000     C*
042500000000     C     *ENTRY        PLIST
042600000000     C                   PARM                    KPJBA
042700170726     C                   PARM                    SDG00702DS
042800000000     C*
042900000000     C     *DMY          Move      Udate         UDateISO
043000000000     C*
043100000000     C* Reperimento dati società
043200000000     C                   MOVEL     'SOC001'      TIPXSC
043300000000     C                   MOVEL     *BLANK        SOCXSC
043400130827     C                   EXSR      REPSOC
043500000000     C*
043600000000     C* Controlli dati società
043700000000     C     RTNXSC        IFNE      '1'
043800000000     C                   MOVEL     XSOCDS        SOC001
043900000000     C                   ELSE
044000170726    >C                   MOVE      '1'           ERR0702
044100130827     C                   EXSR      ENDPGM
044200000000     C                   ENDIF
044300170726     C*
044400170726     C                   Eval      $FineCST    = *Off
044500170727     C*
044600170727     C* Attiva Commitment control se necessario
044700170727     C*
044800170727     C                   IF        CMT0702     = '0'
044900170727    >C                   CALLB     'XSTRCMT'
045000170727     C                   ENDIF
045100170808     C*
045200170808     C                   If        NrrCst0702 >0
045300170808     C                   If        Not %open (SDGCST00F)
045400170808    >C                   OPEN      SDGCST00F
045500170808     C                   Endif
045600170808     C                   Else
045700170808     C                   If        Not %open (SDGCST02l)
045800170808    >C                   OPEN      SDGCST02L
045900170808    >C                   OPEN      SDGCST00F
046000170808     C                   Endif
046100170808     C                   Endif
046200170808     C*
046300170808     C                   If        Not %open (SDGCSF03L)
046400170808    >C                   OPEN      SDGCSF03L
046500170808     C                   Endif
046600000000     C*
046700000000     C                   ENDSR
046800000000     C/EJECT
046900000000     C************************************************************
047000000000     C* REPERIMENTO DATI SOCIETA'
047100000000     C************************************************************
047200000000     C     REPSOC        BEGSR
047300000000     C*
047400000000     C                   CALL      'SDGSOCDR'
047500000000     C                   PARM                    TIPXSC            6
047600000000     C                   PARM                    SOCXSC            3
047700000000     C                   PARM                    CDSXSC            9 0
047800000000     C                   PARM                    MODXSC            3
047900000000     C                   PARM      *BLANKS       RTNXSC            1
048000000000     C                   PARM                    XSOCDS
048100000000     C                   PARM                    KPJBA
048200000000     C*
048300000000     C                   ENDSR
048400000000     C/EJECT
048500000000     C************************************************************
048600000000     C* INIZIALIZZAZIONE VARIABILI
048700000000     C************************************************************
048800000000     C     INZVAR        BEGSR
048900000000     C*
049000170726     C                   Eval      ERR0702     = '0'
049100000000     C*
049200000000     C                   ENDSR
049300170726     C/EJECT
049400130903     C************************************************************
049500130903     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
049600130903     C************************************************************
049700170726     C     SETANA        BEGSR
049800130903     C*
049900000000     C*
050000170726     C                   Eval      CSTSocieta  = Soc0702
050100170726     C                   Eval      CSTTpCom    = TpCom0702
050200170726     C                   Eval      CSTTpRec    = TpRec0702
050300170726     C                   Eval      CSTAnno     = Anno0702
050400170726     C                   Eval      CSTTrim     = Trim0702
050500170726     C                   Eval      CSTMese     = Mese0702
050600170727     C                   Eval      SAVCLIFOR   = CliFor0702
050700170726     C*
050800170726     C     K07CST02      SETLL     SDGCST02L
050900000000     C*
051000000000     C                   ENDSR
051100000000     C/EJECT
051200130827     C************************************************************
051300130827     C* Lettura dati estratti
051400130827     C************************************************************
051500130827     C     REDANA        BEGSR
051600170726     C*
051700170808     C*    K07CST02      SETLL     SDGCST02L
051800170726     C     K07CST02      READE     SDGCST02L                            2221
051900130827     C*
052000170808     C* Aggiorno sempre il file fisico
052100170808     C*
052200170808     C     NrrCST        Chain     SDGCST00F                          2322
052300170808     C*
052400170808     C                   IF        *IN23  OR *IN22
052500170726     C                   MOVEL     *ON           ERR0702
052600170726     C                   Exsr      EndPgm
052700170726     C                   Endif
052800130827     C*
052900170726     C                   Eval      $FineCST    = *In21
053000130827     C*
053100130827     C                   ENDSR
053200130827     C/EJECT
053300130827     C************************************************************
053400130827     C* Selezione dei dati estratti
053500130827     C************************************************************
053600130827     C     SELANA        BEGSR
053700130827     C*
053800130827     C                   Eval      $RcdOk      = *On
053900170726     C                   MOVE      *hival        ALL9             75
054000130827     C*
054100170726     C* creo limite massimo parzializzazioni :
054200170726     C* per ogni valore alfanumerico creo il limite superiore
054300170726     C* per cui caricare, creando una variabile che contiene
054400170726     C* il valore digitato nel campo a video riempito di "9"
054500170726     C*
054600170726     C* chiesti attivi e non attivi
054700170726     C                   Select
054800170726     C                   When      Ann0702     = '1'
054900170726     C                   Eval      $AnnMin     = *Blank
055000170726     C                   Eval      Ann999      = '1'
055100170726     C* chiesti solo attivi
055200170726     C                   When      Ann0702     = '2'
055300170726     C                   Eval      $AnnMin     = *Blank
055400170726     C                   Eval      Ann999      = *Blank
055500170726     C* chiesti solo non attivi
055600170726     C                   When      Ann0702     = '3'
055700170726     C                   Eval      $AnnMin     = '1'
055800170726     C                   Eval      Ann999      = '1'
055900170726     C                   endSl
056000170726     C
056100170726     C                   SELECT
056200170726     C*
056300170726     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
056400170726     C*
056500170808    >C     TpAnn0702     WHENNE    *Blanks
056600170808    >C     CSTTpAnn      ANDNE     TpAnn0702
056700170726    >C                   MOVEL     *OFF          $RCDOK
056800170726     C*
056900170808    >C     Causal0702    WHENNE    *blanks
057000170808    >C     CSTCausale    ANDNE     Causal0702
057100170726    >C                   MOVEL     *OFF          $RCDOK
057200170726     C*
057300170809    >C     Sogg0702      WHENNE    *Blanks
057400170809    >C     CSTSogg       ANDNE     Sogg0702
057500170726    >C                   MOVEL     *OFF          $RCDOK
057600170726     C*
057700170809    >C     CfIn0702      WHENNE    *Blanks
057800170809    >C     CSTCf         ANDNE     CfIn0702
057900170726    >C                   MOVEL     *OFF          $RCDOK
058000170726     C*
058100170726     C                   When      CSTAnn      < $AnnMin       or
058200170726     C                             CSTAnn      > Ann999
058300170726    >C                   MOVEL     *OFF          $RCDOK
058400170726     C*
058500170726     C
058600170726     C                   ENDSL
058700130827     C*
058800130827     C                   ENDSR
058900130827     C/EJECT
059000000000     C************************************************************
059100000000     C* DEFINIZIONE KLIST
059200000000     C************************************************************
059300000000     C     DefKlist      BEGSR
059400000000     C*
059500000000     C* klist
059600000000     C*
059700170726     C* SDGCST02L
059800170726     C     K07CST02      KLIST
059900170726     C                   KFLD                    CSTSocieta
060000170726     C                   KFLD                    CSTTpCom
060100170805     C                   KFLD                    CSTTpRec
060200170726     C                   KFLD                    CSTAnno
060300170726     C                   KFLD                    CSTTrim
060400170726     C                   KFLD                    CSTMese
060500170726     C                   KFLD                    SAVCliFor
060600170728     C* SDGCSF03L
060700170728     C     K07CSF03      KLIST
060800170728     C                   KFLD                    CSFSocieta
060900170728     C                   KFLD                    CSFTpCom
061000170728     C                   KFLD                    CSFTpRec
061100170728     C                   KFLD                    CSFAnno
061200170728     C                   KFLD                    CSFTrim
061300170728     C                   KFLD                    CSFMese
061400170728     C                   KFLD                    CSFPrgEstr
061500000000     C*
061600000000     C                   ENDSR
061700000000     C/EJECT
061800000000     C************************************************************
061900000000     C* FINE PROGRAMMA
062000000000     C************************************************************
062100000000     C     ENDPGM        BEGSR
062200170727     C* se non esco per errore
062300170727    >C     Err0702       IFEQ      '0'
062400170727     C*
062500170727     C* se il COMMIT non è demandato al pgm chiamante
062600170727    >C     CMT0702       IFEQ      '0'
062700170727     C*
062800170727     C* test se esistono transazioni attive ed eventualmente cosa fare
062900170727     C                   Z-ADD     0             EXECOD
063000170727     C                   CALL      'XRCMTI'
063100170727     C                   PARM                    EXECOD            1 0
063200170727     C*
063300170727     C                   SELECT
063400170727     C* Conferma uscita
063500170727     C     EXECOD        WHENEQ    1
063600170808     C                   UNLOCK    SDGCST00F
063700170808     C                   CLOSE     SDGCST00F
063800170727     C* Conferma uscita
063900170727     C     EXECOD        WHENEQ    2
064000170807     C***************    ROLBK
064100170727     C*
064200170727     C                   ENDSL
064300170727     C*
064400170727     C                   ENDIF
064500170727     C*
064600170727     C                   ELSE
064700170727     C*  ERRORE
064800170727     C     CMT0702       IFEQ      '0'
064900170727     C                   ROLBK
065000170727     C                   ENDIF
065100170727     C*
065200170727     C                   ENDIF
065300170727     C*
065400170727     C* passaggio dati al pgm chiamante
065500170727    >C                   MOVEL     SDG00702DS    KPJBU
065600170727     C*
065700170727     C                   SETON                                            LR
065800000000     C*
065900000000     C                   ENDSR
