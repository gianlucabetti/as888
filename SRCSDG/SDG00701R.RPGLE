000100000000     H*BIND
000200000000     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*
000500130826      *           -----------------------------------------
000600170727      *                Spesometro trimestrale
000700170727      *               Recupero dati anagrafici
000800130826      *           -----------------------------------------
000900170727      *
001000170727R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
001100130826      *
001200000000     C*----------------------------------------------------*
001300130827     C*
001400170727     C* TPCOM0701 - tipo comunicazione :
001500170726     C*             T  = Spesometro trimestrale
001600000000     C*
001700170727     C* OPZ0701   -
001800170728     C*             01 = recupero dati C/F da anagrafica soggetto
001900170727     C*
002000170727     C*             02 = recupero dati C/F dipendenti dal movimento
002100000000     C*
002200170801     C*             03 = recupero dati dichiarante (XSCSOC)
002300000000     C*
002400000000     F*----------------------------------------------------*
002500170727     F* Dati anagrafici aggiuntivi
002600170727     F* Key: Sogg/DtFiVl/Sys/NrAsInt/Dest      UNIQUE
002700170727     FSDGSO301L IF   E           K DISK
002800000000     F*-------------
002900000000     D* Passaggio parametri
003000000000     D KPJBA         E DS
003100000000     D*-------------
003200000000     D* Dati di ambiente ottenuti da SDGSOCDR
003300000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
003400170728     D    SocSogg                     8    overlay(XSCFGO:31)
003500000000     D*-------------
003600000000     D* DS Interna per dati di output di SDGSOCDR
003700170727     D XSOCDS          DS          1000
003800000000     D*-------------
003900170726     D* Parametri
004000170727     D SDG00701DS    E DS
004100170727     D*-------------
004200170727     D* Parametri modulo SDSPETRI
004300170727     D SDGPA12DS     E DS
004400170727     D*---------
004500170727     D* Spesometro trimestrale
004600170727     D SDGCST00F     E DS
004700170727     D*---------
004800170727     D* Variazioni estremi fiscali
004900170727     D ANCPI00F      E DS
005000170727     D*---------
005100170727     D* Driver estensione nominativi partite per conti tecnici
005200170727     D* (=conti estemporanei)
005300170727     D NDCPA00F      E DS                  INZ
005301170824R264 X**** $Sys            S                   Like(CSTSys)
005302170824 "   X**** $NrAsInt        S                   Like(CSTNrAsReg)
005303170824R264 X**** $NrAsGen        S                   Like(CSTNrAsReg)
005304170824R264 D* definiti Packed perchè in caso di ANCPA chiama NDC066R2
005305170824 "   D* in cui i campi sono passati nelle PARM e definiti come Packed.
005306170824 "   D* Definendoli qui come Signed, si rompe per numeri decimali
005307170824 "   D $Sys            S              3P 0
005308170824 "   D $NrAsInt        S              9P 0
005309170824R264 D $NrAsGen        S              9P 0
005700170727     D*-------------
005800170727     D* Driver estremi fiscali (ANCPI00F)
005900170727     D DVPRic          S              1
006000170727     D DVPSocieta      S              3
006100170727     D DVPUnita        S              8
006200170727     D DVPStrutt       S             10
006300170727     D DVPDtRif        S               D
006400170727     D DVPLenOut       S              9B 0
006500170727     D DVPSogg         S              8
006600170727     D DVPErrore       S              1
006700170727     D DVPOutput       S           4000
006800170727     D*---------
006900170727     D* Dati soggetti
007000170727     D DVASog        E DS
007100170727     D* Dati fornitori
007200170727     D DVAFor        E DS
007300170727     D* Dati clienti
007400170727     D DVACli        E DS
007500170727     D*---------
007600170727     D* driver soggetti
007700170727     D DVARic          S              1
007800170727     D DVASocieta      S              3
007900170727     D DVAUnita        S              8
008000170727     D DVADtRif        S               D
008100170727     D DVAStrutt       S             10
008200170727     D DVALenOut       S              9B 0
008300170727     D DVASnatura      S              1
008400170727     D DVACodice       S              8
008500170727     D DVALineav       S              3
008600170727     D DVAFiliale      S              3  0
008700170727     D DVASogg         S              8
008800170727     D DVATpInd        S              2
008900170727     D DVACdInd        S              3
009000170727     D DVAOutput       S           4000
009100170727     D DVAErrore       S              1
009200170727     D*---------
009300170727     D* Richiamo a SDGTABDR
009400170727     D SDGTABDS      E DS
009500170727     D*-------------
009600170727     D* Tabella degli Stati con codici ISO 3Num e 2Alpha
009700170727     D SDGHD4DS      E DS
009800170727     D*-------------
009900170727     D* Tabella tipologie natura giuridica
010000170727     D ANGG16DS      E DS
010100000000     D*-------------
010200000000     D* Reperimento nome PGM
010300000000     D STATUS         SDS           333
010400000000     D  DSPGM            *PROC
010500000000     D  PARMS            *PARMS
010600170727     D*---------
010700170727     D* Definizioni parametri standard chiamate moduli
010800170727     D $Esito          S              1
010900170727     D $GesMsg         S              1
011000170727     D $LenOut         S              9B 0
011100170727     D $Struttura      S             10
011200170727     D $CodOper        S              1
011300170727     D $Output         S           4000
011400170727     D $Refresh        S              1
011500170727     D $Uso            S              1
011600000000     D*-------------
011700000000     D* campi di lavoro
011800170727     D WStato          S                   Like(XScSta )
011900170727     D WNatGiur        S                   Like(DVFNatGiur)
012000170727     D*---------
012100170727     D* Dati anagrafici della società contribuente
012200170727     D $PartIVASoc     S                   Like(CSTPartIVA)
012300170727     D $CdFiscSoc      S                   Like(CSTCdFisc )
012400170731     D $Stato1Soc      S                   Like(CSTStato1)
012500170731     D $IdPaes1Soc     S                   Like(CSTIdPaes1)
012600170727     D $PIVA1Soc       S                   Like(CSTPIVA1  )
012700170727     D $CdFisc1Soc     S                   Like(CSTCdFisc1)
012800170727     D $CogRag1Soc     S                   Like(CSTCogRag1)
012900170727     D $TpN1Soc        S                   Like(CSTTpN1  )
013000170727     D $Denom1Soc      S                   Like(CSTDenom1)
013100170727     D $Cognom1Soc     S                   Like(CSTCognom1)
013200170727     D $Nome1Soc       S                   Like(CSTNome1)
013300170727     D $Indir2Soc      S                   Like(CSTIndir2)
013400170727     D $NrCiv2Soc      S                   Like(CSTNrCiv2)
013500170727     D $CAP2Soc        S                   Like(CSTCAP2  )
013600170727     D $Locali2Soc     S                   Like(CSTLocali2)
013700170727     D $Prov2Soc       S                   Like(CSTProv2 )
013800170727     D $Stato2Soc      S                   Like(CSTStato2)
013900170727     D $IdPaes2Soc     S                   Like(CSTIdPaes2)
014000170727     D $Indir3Soc      S                   Like(CSTIndir3 )
014100170727     D $NrCiv3Soc      S                   Like(CSTNrCiv3 )
014200170727     D $CAP3Soc        S                   Like(CSTCAP3   )
014300170727     D $Locali3Soc     S                   Like(CSTLocali3)
014400170727     D $Prov3Soc       S                   Like(CSTProv3  )
014500170727     D $Stato3Soc      S                   Like(CSTStato3 )
014600170727     D $IdPaes3Soc     S                   Like(CSTIdPaes3)
014700170728     D $Stato4Soc      S                   Like(CSTStato4 )
014800170727     D $IdPaes4Soc     S                   Like(CSTIdPaes4)
014900170727     D $PIVA4Soc       S                   Like(CSTPIVA4  )
015000170727     D $CogRag4Soc     S                   Like(CSTCogRag4)
015100170727     D $TpN4Soc        S                   Like(CSTTpN4  )
015200170727     D $Denom4Soc      S                   Like(CSTDenom4)
015300170727     D $Cognom4Soc     S                   Like(CSTCognom4)
015400170727     D $Nome4Soc       S                   Like(CSTNome4  )
015500170727     D*------
015600170727     D* Dati anagrafici del soggetto cliente/fornitore
015700170727     D $PartIVASog     S                   Like(CSTPartIVA)
015800170727     D $CdFiscSog      S                   Like(CSTCdFisc )
015900170731     D $Stato1Sog      S                   Like(CSTStato1 )
016000170731     D $IdPaes1Sog     S                   Like(CSTIdPaes1)
016100170727     D $PIVA1Sog       S                   Like(CSTPIVA1  )
016200170727     D $CdFisc1Sog     S                   Like(CSTCdFisc1)
016300170727     D $CogRag1Sog     S                   Like(CSTCogRag1)
016400170727     D $TpN1Sog        S                   Like(CSTTpN1  )
016500170727     D $Denom1Sog      S                   Like(CSTDenom1)
016600170727     D $Cognom1Sog     S                   Like(CSTCognom1)
016700170727     D $Nome1Sog       S                   Like(CSTNome1)
016800170727     D $Indir2Sog      S                   Like(CSTIndir2)
016900170727     D $NrCiv2Sog      S                   Like(CSTNrCiv2)
017000170727     D $CAP2Sog        S                   Like(CSTCAP2  )
017100170727     D $Locali2Sog     S                   Like(CSTLocali2)
017200170727     D $Prov2Sog       S                   Like(CSTProv2 )
017300170727     D $Stato2Sog      S                   Like(CSTStato2)
017400170727     D $IdPaes2Sog     S                   Like(CSTIdPaes2)
017500170727     D $Indir3Sog      S                   Like(CSTIndir3 )
017600170727     D $NrCiv3Sog      S                   Like(CSTNrCiv3 )
017700170727     D $CAP3Sog        S                   Like(CSTCAP3   )
017800170727     D $Locali3Sog     S                   Like(CSTLocali3)
017900170727     D $Prov3Sog       S                   Like(CSTProv3  )
018000170727     D $Stato3Sog      S                   Like(CSTStato3 )
018100170727     D $IdPaes3Sog     S                   Like(CSTIdPaes3)
018200170728     D $Stato4Sog      S                   Like(CSTStato4 )
018300170727     D $IdPaes4Sog     S                   Like(CSTIdPaes4)
018400170727     D $PIVA4Sog       S                   Like(CSTPIVA4  )
018500170727     D $CogRag4Sog     S                   Like(CSTCogRag4)
018600170727     D $TpN4Sog        S                   Like(CSTTpN4  )
018700170727     D $Denom4Sog      S                   Like(CSTDenom4)
018800170727     D $Cognom4Sog     S                   Like(CSTCognom4)
018900170727     D $Nome4Sog       S                   Like(CSTNome4  )
019000170727     D*------
019100170727     D* Dati anagrafici del cliente/fornitore
019200170728     D $PartIVAMov     S                   Like(CSTPartIVA)
019300170728     D $CdFiscMov      S                   Like(CSTCdFisc )
019400170731     D $Stato1Mov      S                   Like(CSTStato1 )
019500170731     D $IdPaes1Mov     S                   Like(CSTIdPaes1)
019600170728     D $PIVA1Mov       S                   Like(CSTPIVA1  )
019700170728     D $CdFisc1Mov     S                   Like(CSTCdFisc1)
019800170728     D $CogRag1Mov     S                   Like(CSTCogRag1)
019900170728     D $TpN1Mov        S                   Like(CSTTpN1  )
020000170728     D $Denom1Mov      S                   Like(CSTDenom1)
020100170728     D $Cognom1Mov     S                   Like(CSTCognom1)
020200170728     D $Nome1Mov       S                   Like(CSTNome1)
020300170728     D $Indir2Mov      S                   Like(CSTIndir2)
020400170728     D $NrCiv2Mov      S                   Like(CSTNrCiv2)
020500170728     D $CAP2Mov        S                   Like(CSTCAP2  )
020600170728     D $Locali2Mov     S                   Like(CSTLocali2)
020700170728     D $Prov2Mov       S                   Like(CSTProv2 )
020800170728     D $Stato2Mov      S                   Like(CSTStato2)
020900170728     D $IdPaes2Mov     S                   Like(CSTIdPaes2)
021000170728     D $Indir3Mov      S                   Like(CSTIndir3 )
021100170728     D $NrCiv3Mov      S                   Like(CSTNrCiv3 )
021200170728     D $CAP3Mov        S                   Like(CSTCAP3   )
021300170728     D $Locali3Mov     S                   Like(CSTLocali3)
021400170728     D $Prov3Mov       S                   Like(CSTProv3  )
021500170728     D $Stato3Mov      S                   Like(CSTStato3 )
021600170728     D $IdPaes3Mov     S                   Like(CSTIdPaes3)
021700170728     D $Stato4Mov      S                   Like(CSTStato4 )
021800170728     D $IdPaes4Mov     S                   Like(CSTIdPaes4)
021900170728     D $PIVA4Mov       S                   Like(CSTPIVA4  )
022000170728     D $CogRag4Mov     S                   Like(CSTCogRag4)
022100170728     D $TpN4Mov        S                   Like(CSTTpN4  )
022200170728     D $Denom4Mov      S                   Like(CSTDenom4)
022300170728     D $Cognom4Mov     S                   Like(CSTCognom4)
022400170728     D $Nome4Mov       S                   Like(CSTNome4  )
022500170727     D*--------------
022600170728     D* campi di salvataggio per non reperire sempre le stesse
022700170728     D* informazioni
022800170728     D SavOpz          S                   Like(Opz0701   )
022900170728     D SavSocieta      S                   Like(Soc0701   )
023000170728     D SavTpCom        S                   Like(TpCom0701 )
023100170728     D SavAnno         S                   Like(Anno0701  )
023200170728     D SavTrim         S                   Like(Trim0701  )
023300170728     D SavMese         S                   Like(Mese0701  )
023400170728     D SavSogg         S                   Like(Sogg0701  )
023500170728     D SavCF           S                   Like(CF0701    )
023600170728     D SavTecnico      S                   Like(Tecnic0701)
023700170728     D SavDtRif        S                   Like(DtRif0701 )
023800170728     D SavSys          S                   Like(Sys0701   )
023900170728     D SavNrAsReg      S                   Like(NrAsRe0701)
024000000000     C*----------------------------------------------------*
024100000000     C*                MAIN LINE PROGRAM
024200000000     C*----------------------------------------------------*
024300000000     C*
024400130827     C                   EXSR      INZVAR
024500130902     C*
024600170727     C                   Select
024700170727     C*
024800170728     C* recupero dati C/F da anagrafica soggetto
024900170727     C                   When      OPZ0701  = '01'
025000170728     C                   EXSR      RepFisc_Sog
025100170727     C*
025200170728     C* recupero dati C/F dipendenti dal movimento
025300170727     C                   When      OPZ0701  = '02'
025400170728     C                   EXSR      RepFisc_Mov
025500170728     C*
025600170728     C* recupero dati dichiarante (XSCSOC)
025700170728     C                   When      OPZ0701  = '03'
025800170728     C                   EXSR      RepFisc_Soc
025900170727     C                   endSl
026000000000     C*
026100130827     C                   EXSR      ENDPGM
026200000000     C/EJECT
026300170727     C************************************************************
026400170727     C* Reperimento dati fiscali del soggetto del conto
026500170727     C************************************************************
026600170728     C     RepFisc_Sog   BEGSR
026700170728     C*
026800170728     C                   EXSR      Clear_Sog
026900170728     C*
027000170728     C                   Select
027100170728     C                   When      CF0701      = 'F'
027200170727     C                   MoveL     'DVAFOR    '  DVAStrutt
027300170727     C                   Eval      DVALenout   = %Size(DVAFor)
027400170727     C*
027500170728     C                   When      CF0701      = 'C'
027600170727     C                   MoveL     'DVACLI    '  DVAStrutt
027700170727     C                   Eval      DVALenout   = %Size(DVACli)
027800170728     C                   endSl
027900170727     C*
028000170727     C                   EXSR      CallDvaSog
028100170727     C*
028200170727     C                   if        DVAErrore  <> *Off
028300170727     C* annullo il conto
028400170728     C                   Eval      AnnCon0701  = *On
028500170728     C                   if        TpAnn0701   = *Blank
028600170728     C                   Eval      TpAnn0701   = '07'
028700170727     C                   endif
028800170727     C*
028900170727     C                   else
029000170727     C*
029100170728     C                   Eval      Found0701   = *On
029200170728     C*
029300170727     C                   Select
029400170728     C*
029500170728     C                   When      CF0701      = 'F'
029600170727     C                   EXSR      RieDvaFor
029700170728     C*
029800170728     C                   When      CF0701      = 'C'
029900170727     C                   EXSR      RieDvaCli
030000170728     C*
030100170727     C                   endSl
030200170728     C*
030300170728     C*
030400170731     C                   if        Found0701   = *On
030500170731     C*
030600170728     C* verifico se sono presenti i dati aggiuntivi anagrafici
030700170728     C* se esistono sovrascrivo i dati appena reperiti
030800170731     C* Reperisco, se esistono, i dati per la Stabile Organizzazione
030900170731     C* e il Rappresentante Fiscale
031000170728     C                   Eval      SO3Sogg     = Sogg0701
031100170728     C                   Eval      SO3DtFiVl   = *Loval
031200170728     C                   Eval      SO3Sys      = 0
031300170728     C                   Eval      SO3NrAsInt  = 0
031400170728     C                   EXSR      RepSO3_SOG
031500170731     C*
031600170731     C                   endif
031700170727     C*
031800170727     C                   endif
031900170727     C*
032000170728     C*-------------------
032100170728     C                   EXSR      RieOut_Sog
032200170727     C*
032300170727     C                   ENDSR
032400170727     C/EJECT
032500170727     C************************************************************
032600170728     C* Pulisco i campi del soggetto
032700170727     C************************************************************
032800170728     C     Clear_Sog     BEGSR
032900170727     C*
033000170728     C* IDENTIFICATIVI FISCALI
033100170728     C                   Clear                   $PartIVASog
033200170728     C                   Clear                   $CdFiscSog
033300170731     C                   Clear                   $Stato1Sog
033400170731     C                   Clear                   $IdPaes1Sog
033500170728     C                   Clear                   $PIVA1Sog
033600170728     C                   Clear                   $CdFisc1Sog
033700170728     C*
033800170728     C* ALTRI DATI IDENTIFICATIVI
033900170728     C                   Clear                   $CogRag1Sog
034000170728     C                   Clear                   $TpN1Sog
034100170728     C                   Clear                   $Denom1Sog
034200170728     C                   Clear                   $Cognom1Sog
034300170728     C                   Clear                   $Nome1Sog
034400170728     C                   Clear                   $Indir2Sog
034500170728     C                   Clear                   $NrCiv2Sog
034600170728     C                   Clear                   $CAP2Sog
034700170728     C                   Clear                   $Locali2Sog
034800170728     C                   Clear                   $Prov2Sog
034900170728     C                   Clear                   $Stato2Sog
035000170728     C                   Clear                   $IdPaes2Sog
035100170728     C*
035200170728     C* STABILE ORGANIZZAZIONE
035300170728     C                   Clear                   $Indir3Sog
035400170728     C                   Clear                   $NrCiv3Sog
035500170728     C                   Clear                   $CAP3Sog
035600170728     C                   Clear                   $Locali3Sog
035700170728     C                   Clear                   $Prov3Sog
035800170728     C                   Clear                   $Stato3Sog
035900170728     C                   Clear                   $IdPaes3Sog
036000170728     C*
036100170728     C* RAPPRESENTANTE FISCALE
036200170728     C                   Clear                   $Stato4Sog
036300170728     C                   Clear                   $IdPaes4Sog
036400170728     C                   Clear                   $PIVA4Sog
036500170728     C*
036600170728     C                   Clear                   $CogRag4Sog
036700170728     C                   Clear                   $TpN4Sog
036800170728     C                   Clear                   $Denom4Sog
036900170728     C                   Clear                   $Cognom4Sog
037000170728     C                   Clear                   $Nome4Sog
037100170728     C*
037200170728     C                   ENDSR
037300170728     C/EJECT
037400170728     C************************************************************
037500170728     C* Chiamata al driver dei soggetti
037600170728     C************************************************************
037700170728     C     CallDvaSog    BEGSR
037800170728     C*
037900170727     C                   CALLB     'NDDVASOG'
038000170727     C                   PARM      '1'           DVARic
038100170727     C                   PARM      XScSoc        DVASocieta
038200170727     C                   PARM      *Blank        DVAUnita
038300170727     C                   PARM                    DVAStrutt
038400170727     C                   PARM                    DVADtRif
038500170727     C                   PARM                    DVALenOut
038600170727     C                   PARM      CF0701        DVASNatura
038700170727     C                   PARM      *Blank        DVACodice
038800170727     C                   PARM      *Blank        DVALineaV
038900170727     C                   PARM      *Zero         DVAFiliale
039000170727     C                   PARM      Sogg0701      DVASogg
039100170727     C                   PARM      *Blank        DVATpInd
039200170727     C                   PARM      *Blank        DVACdInd
039300170727     C                   PARM      *Off          DVAErrore
039400170727     C                   PARM                    DVAOutPut
039500170727     C*
039600170727     C                   ENDSR
039700170727     C/EJECT
039800170727     C************************************************************
039900170727     C* Dati del fornitore
040000170727     C************************************************************
040100170727     C     RieDvaFor     BEGSR
040200170727     C*
040300170727     C                   Eval      %subst(DvaFor:1:DVALenout)
040400170727     C                              = %subst(DVAOutput:1:DVALenout)
040500170727     C*
040600170727     C                   Eval      WNatGiur    = DVFNatGiur
040700170727     C                   EXSR      RepTpN
040800170727     C                   if        XTbErr      = '0'
040900170727     C                   MoveL     XTbUni        ANGG16DS
041000170727     C                   Eval      $TpN1Sog    = TpNG16
041100170728     C                   else
041200170728     C* annullo il conto
041300170728     C                   Eval      AnnCon0701  = *On
041400170728     C                   if        TpAnn0701   = *Blank
041500170728     C                   Eval      TpAnn0701   = '50'
041600170728     C                   endif
041700170727     C                   endif
041800170727     C*
041900170727     C                   MoveL(P)  DVFPartIVA    $PartIVASog
042000170727     C                   MoveL(P)  DVFCdFisc     $CdFiscSog
042100170731     C                   Movel     *Blank        $Stato1Sog
042200170727     C                   MoveL     *Blank        $IdPaes1Sog
042300170727     C                   MoveL(P)  DVFPartIva    $PIVA1Sog
042400170727     C                   MoveL(P)  DVFCdFisc     $CdFisc1Sog
042500170727     C*
042600170727     C                   MoveL(P)  DVFDes        $CogRag1Sog
042700170727     C*
042800170727     C* persona fisica
042900170727     C                   if        $TpN1Sog    = '1'
043000170727     C                   MoveL     *Blank        $Denom1Sog
043100170727     C                   MoveL(P)  DVFDes        $Cognom1Sog
043200170727     C                   MoveL     *Blank        $Nome1Sog
043300170727     C*
043400170727     C                   else
043500170727     C* società
043600170727     C                   MoveL(P)  DVFDes        $Denom1Sog
043700170727     C                   MoveL     *Blank        $Cognom1Sog
043800170727     C                   MoveL     *Blank        $Nome1Sog
043900170727     C                   endif
044000170727     C*
044100170727     C                   MoveL(P)  DVFIndriz     $Indir2Sog
044200170727     C                   MoveL     *Blank        $NrCiv2Sog
044300170818fm8  C                   MoveL(P)  DVFCAP        $CAP2Sog
044400170727     C                   MoveL(P)  DVFLocalit    $Locali2Sog
044500170818fm8  C                   MoveL(P)  DVFProv       $Prov2Sog
044600170727     C                   MoveL(P)  DVFStato      $Stato2Sog
044700170727     C                   MoveL     *Blank        $IdPaes2Sog
044800170727     C*
044900170727     C                   ENDSR
045000170727     C/EJECT
045100170727     C********************************************************************
045200170727     C* Reperisco la tipologia della natura giuridica
045300170727     C********************************************************************
045400170727     C     RepTpN        BEGSR
045500170727     C*
045600170727     C                   Clear                   SDGTABDS
045700170727     C*
045800170727     C                   Eval      XTbRic      = '1'
045900170727     C                   Eval      XTbAzi      = XScSoc
046000170727     C                   Eval      XTbKey      = *Zero
046100170727     C                   Eval      XTbCod      = 'G16'
046200170727     C                   Move      WNatGiur      XTbKey
046300170727     C*
046400170727     C                   CALL      'SDGTABDR'
046500170727     C                   PARM                    SDGTABDS
046600170727     C*
046700170727     C                   ENDSR
046800170727     C/EJECT
046900170727     C********************************************************************
047000170727     C* Reperisco i dati anagrafici aggiuntivi
047100170727     C********************************************************************
047200170727     C     RepSO3_SOG    BEGSR
047300170727     C*
047400170728     C*-----------------
047500170731     C* IDENTIFICATIVI FISCALI E ALTRI DATI IDENTIFICATIVI (SEDE)
047600170728     C                   EXSR      RepSO3_Sede
047700170727     C*
047800170727     C                   if        *In21       = *Off
047900170728     C*
048000170727     C                   MoveL(P)  SO3PartIva    $PartIVASog
048100170727     C                   MoveL(P)  SO3CdFisc     $CdFiscSog
048200170731     C                   Movel     *Blank        $Stato1Sog
048300170727     C                   MoveL     *Blank        $IdPaes1Sog
048400170727     C                   MoveL(P)  SO3PartIva    $PIVA1Sog
048500170727     C                   MoveL(P)  SO3CdFisc     $CdFisc1Sog
048600170727     C*
048700170727     C                   MoveL(P)  SO3CogRag     $CogRag1Sog
048800170727     C*
048900170727     C* persona fisica
049000170727     C                   if        $TpN1Sog    = '1'
049100170727     C                   MoveL     *Blank        $Denom1Sog
049200170727     C                   MoveL(P)  SO3CogRag     $Cognom1Sog
049300170727     C                   MoveL(P)  SO3Nome       $Nome1Sog
049400170727     C*
049500170727     C                   else
049600170727     C* società
049700170727     C                   MoveL(P)  SO3CogRag     $Denom1Sog
049800170727     C                   MoveL     *Blank        $Cognom1Sog
049900170727     C                   MoveL     *Blank        $Nome1Sog
050000170727     C                   endif
050100170727     C*
050200170727     C                   MoveL(P)  SO3IndirE     $Indir2Sog
050300170727     C                   MoveL     *Blank        $NrCiv2Sog
050400170801     C                   MoveL(P)  SO3CAP        $CAP2Sog
050500170727     C                   MoveL(P)  SO3Localit    $Locali2Sog
050600170801     C                   MoveL(P)  SO3Prov       $Prov2Sog
050700170727     C                   MoveL(P)  SO3Stato      $Stato2Sog
050800170727     C                   MoveL     *Blank        $IdPaes2Sog
050900170728     C*
051000170727     C                   endif
051100170731     C*
051200170731     C* Reperisco il codice ISO 2 Alpha della nazione del soggetto
051300170731     C                   Eval      WStato      = $Stato2Sog
051400170731     C                   EXSR      RepTabHD4
051500170731     C*
051600170731     C                   if        XTbErr      = '1'
051700170731     C* annullo il conto
051800170731     C                   Eval      AnnCon0701  = *On
051900170731     C                   if        TpAnn0701   = *Blank
052000170731     C                   Eval      TpAnn0701   = '51'
052100170731     C                   endif
052200170731     C                   endif
052300170731     C*
052400170731     C                   Eval      $IdPaes2Sog = CodISO2HD4
052500170728     C*
052600170731     C*
052700170728     C*-----------------
052800170731     C* STABILE ORGANIZZAZIONE
052900170728     C                   EXSR      RepSO3_StaOrg
053000170728     C*
053100170728     C                   if        *In21       = *Off
053200170728     C*
053300170728     C                   MoveL(P)  SO3IndirE     $Indir3Sog
053400170728     C                   MoveL     *Blank        $NrCiv3Sog
053500170801     C                   MoveL(P)  SO3CAP        $CAP3Sog
053600170728     C                   MoveL(P)  SO3Localit    $Locali3Sog
053700170801     C                   MoveL(P)  SO3Prov       $Prov3Sog
053800170728     C                   MoveL(P)  SO3Stato      $Stato3Sog
053900170728     C                   MoveL     *Blank        $IdPaes3Sog
054000170728     C*
054100170731     C*
054200170731     C* Reperisco il codice ISO 2 Alpha della nazione del soggetto
054300170731     C                   if        $Stato3Sog <> *Blank
054400170731     C                   Eval      WStato      = $Stato3Sog
054500170731     C                   EXSR      RepTabHD4
054600170731     C*
054700170731     C                   if        XTbErr      = '1'
054800170731     C* annullo il conto
054900170731     C                   Eval      AnnCon0701  = *On
055000170731     C                   if        TpAnn0701   = *Blank
055100170731     C                   Eval      TpAnn0701   = '51'
055200170731     C                   endif
055300170731     C                   endif
055400170731     C*
055500170731     C                   Eval      $IdPaes3Sog = CodISO2HD4
055600170731     C                   endif
055700170731     C*
055800170728     C                   endif
055900170728     C*
056000170731     C*
056100170728     C*-----------------
056200170731     C* RAPPRESENTANTE FISCALE
056300170728     C                   EXSR      RepSO3_RapFisc
056400170728     C*
056500170728     C                   if        *In21       = *Off
056600170728     C*
056700170728     C                   MoveL(P)  SO3Stato      $Stato4Sog
056800170728     C                   MoveL     *Blank        $IdPaes4Sog
056900170728     C                   MoveL(P)  SO3PartIva    $PIVA4Sog
057000170728     C*
057100170728     C                   MoveL(P)  SO3CogRag     $CogRag4Sog
057200170728     C*
057300170728     C* persona fisica
057400170728     C                   if        SO3Nome    <> *Blank
057500170731     C                   MoveL(P)  '1'           $TpN4Sog
057600170728     C                   MoveL     *Blank        $Denom4Sog
057700170728     C                   MoveL(P)  SO3CogRag     $Cognom4Sog
057800170728     C                   MoveL(P)  SO3Nome       $Nome4Sog
057900170728     C*
058000170728     C                   else
058100170731     C* società generica
058200170731     C                   MoveL(P)  '2'           $TpN4Sog
058300170728     C                   MoveL(P)  SO3CogRag     $Denom4Sog
058400170728     C                   MoveL     *Blank        $Cognom4Sog
058500170728     C                   MoveL     *Blank        $Nome4Sog
058600170728     C                   endif
058700170731     C*
058800170731     C*-------------------
058900170731     C* Reperisco il codice ISO 2 Alpha della nazione del soggetto
059000170731     C                   if        $Stato4Sog <> *Blank
059100170731     C                   Eval      WStato      = $Stato4Sog
059200170731     C                   EXSR      RepTabHD4
059300170731     C*
059400170731     C                   if        XTbErr      = '1'
059500170731     C* annullo il conto
059600170731     C                   Eval      AnnCon0701  = *On
059700170731     C                   if        TpAnn0701   = *Blank
059800170731     C                   Eval      TpAnn0701   = '51'
059900170731     C                   endif
060000170731     C                   endif
060100170731     C*
060200170731     C                   Eval      $IdPaes4Sog = CodISO2HD4
060300170731     C                   endif
060400170728     C*
060500170728     C                   endif
060600170727     C*
060700170727     C                   ENDSR
060800170727     C/EJECT
060900170727     C************************************************************
061000170727     C* Lettura dati aggiuntivi anagrafici
061100170727     C************************************************************
061200170728     C     RepSO3_Sede   BEGSR
061300170727     C*
061400170727     C                   Eval      SO3Dest     = DestSA12
061500170728     C                   EXSR      ChainSDGSO3
061600170727     C*
061700170727     C* se non trovato, provo con la destinazione Generica
061800170727     C                   if        *In21       = *On     and
061900170727     C                             SO3Dest    <> *Blank
062000170727     C                   Eval      SO3Dest     = *Blank
062100170728     C                   EXSR      ChainSDGSO3
062200170727     C                   endif
062300170727     C*
062400170727     C                   ENDSR
062500170727     C/EJECT
062600170728     C************************************************************
062700170728     C* Lettura dati aggiuntivi anagrafici Stabile organizzazione
062800170728     C************************************************************
062900170728     C     RepSO3_StaOrg BEGSR
063000170728     C*
063100170728     C                   Eval      SO3Dest     = DestOA12
063200170728     C                   EXSR      ChainSDGSO3
063300170728     C*
063400170728     C                   ENDSR
063500170728     C/EJECT
063600170728     C************************************************************
063700170728     C* Lettura dati aggiuntivi anagrafici Rappresentante fiscale
063800170728     C************************************************************
063900170731     C     RepSO3_RapFiscBEGSR
064000170728     C*
064100170728     C                   Eval      SO3Dest     = DestRA12
064200170728     C                   EXSR      ChainSDGSO3
064300170728     C*
064400170728     C                   ENDSR
064500170728     C/EJECT
064600170728     C************************************************************
064700170728     C* Lettura dati aggiuntivi anagrafici
064800170728     C************************************************************
064900170728     C     ChainSDGSO3   BEGSR
065000170728     C*
065100170728     C     K05SO301      CHAIN     SDGSO301L                          21
065200170728     C*
065300170728     C                   ENDSR
065400170728     C/EJECT
065500170727     C************************************************************
065600170727     C* Dati del cliente
065700170727     C************************************************************
065800170727     C     RieDvaCli     BEGSR
065900170727     C*
066000170727     C                   Eval      %subst(DvaCli:1:DVALenout)
066100170727     C                              = %subst(DVAOutput:1:DVALenout)
066200170727     C*
066300170727     C                   Eval      WNatGiur    = DVCNatGiur
066400170727     C                   EXSR      RepTpN
066500170727     C                   if        XTbErr      = '0'
066600170727     C                   MoveL     XTbUni        ANGG16DS
066700170727     C                   Eval      $TpN1Sog    = TpNG16
066800170727     C                   endif
066900170727     C*
067000170818fm8  C                   MoveL(P)  DVCPartIVA    $PartIVASog
067100170818fm8  C                   MoveL(P)  DVCCdFisc     $CdFiscSog
067200170731     C                   Movel     *Blank        $Stato1Sog
067300170727     C                   MoveL     *Blank        $IdPaes1Sog
067400170818fm8  C                   MoveL(P)  DVCPartIVA    $PIVA1Sog
067500170818fm8  C                   MoveL(P)  DVCCdFisc     $CdFisc1Sog
067600170727     C*
067700170818fm8  C                   MoveL(P)  DVCDes        $CogRag1Sog
067800170727     C*
067900170727     C* persona fisica
068000170727     C                   if        $TpN1Sog    = '1'
068100170727     C                   MoveL     *Blank        $Denom1Sog
068200170727     C                   MoveL(P)  DVCDes        $Cognom1Sog
068300170727     C                   MoveL     *Blank        $Nome1Sog
068400170727     C*
068500170727     C                   else
068600170727     C* società
068700170727     C                   MoveL(P)  DVCDes        $Denom1Sog
068800170727     C                   MoveL     *Blank        $Cognom1Sog
068900170727     C                   MoveL     *Blank        $Nome1Sog
069000170727     C                   endif
069100170727     C*
069200170818fm8  C                   MoveL(P)  DVCIndriz     $Indir2Sog
069300170727     C                   MoveL     *Blank        $NrCiv2Sog
069400170818fm8  C                   MoveL(P)  DVCCAP        $CAP2Sog
069500170818fm8  C                   MoveL(P)  DVCLocalit    $Locali2Sog
069600170818fm8  C                   MoveL(P)  DVCProv       $Prov2Sog
069700170818fm8  C                   MoveL(P)  DVCStato      $Stato2Sog
069800170727     C                   MoveL     *Blank        $IdPaes2Sog
069900170727     C*
070000170727     C                   ENDSR
070100170727     C/EJECT
070200170727     C************************************************************
070300170727     C* Reperisco dati tabella HD4
070400170727     C************************************************************
070500170727     C     RepTabHD4     BEGSR
070600170727     C*
070700170727     C                   Clear                   SDGHD4DS
070800170727     C                   Clear                   SDGTABDS
070900170727     C*
071000170727     C                   Eval      XTbRic      = '1'
071100170727     C                   Eval      XTbAzi      = XScSoc
071200170727     C                   Eval      XTbKey      = *Zero
071300170727     C                   Eval      XTbCod      = 'HD4'
071400170727     C                   Move      WStato        XTbKey
071500170727     C*
071600170727     C                   CALL      'SDGTABDR'
071700170727     C                   Parm                    SDGTABDS
071800170727     C*
071900170727     C                   if        XTBErr      = '0'
072000170727     C                   Eval      SDGHD4DS    = XTbUni
072100170727     C                   endif
072200170727     C*
072300170727     C                   ENDSR
072400170727      /EJECT
072500170727     C************************************************************
072600170727     C* Imposto i dati di output recuperati dal soggetto del conto
072700170727     C************************************************************
072800170728     C     RieOut_Sog    BEGSR
072900170727     C*
073000170728     C* DATI FISCALI
073100170727     C                   Eval      PartIV0701  = $PartIVASog
073200170727     C                   Eval      CdFisc0701  = $CdFiscSog
073300170731     C                   Eval      Stato10701  = $Stato1Sog
073400170731     C                   Eval      IdPae10701  = $IdPaes1Sog
073500170727     C                   Eval      PIVA10701   = $PIVA1Sog
073600170728     C                   Eval      CdFis10701  = $CdFisc1Sog
073700170727     C*
073800170728     C* ALTRI DATI IDENTIFICATIVI
073900170727     C                   Eval      CogRa10701  = $CogRag1Sog
074000170727     C                   Eval      TpN10701    = $TpN1Sog
074100170727     C                   Eval      Denom10701  = $Denom1Sog
074200170727     C                   Eval      Cogno10701  = $Cognom1Sog
074300170727     C                   Eval      Nome10701   = $Nome1Sog
074400170727     C*
074500170728     C* SEDE
074600170727     C                   Eval      Indir20701  = $Indir2Sog
074700170727     C                   Eval      NrCiv20701  = $NrCiv2Sog
074800170727     C                   Eval      CAP20701    = $CAP2Sog
074900170727     C                   Eval      Local20701  = $Locali2Sog
075000170727     C                   Eval      Prov20701   = $Prov2Sog
075100170727     C                   Eval      Stato20701  = $Stato2Sog
075200170727     C                   Eval      IdPae20701  = $IdPaes2Sog
075300170728     C*
075400170728     C* STABILE ORGANIZZAZIONE
075500170728     C                   Eval      Indir30701  = $Indir3Sog
075600170728     C                   Eval      NrCiv30701  = $NrCiv3Sog
075700170728     C                   Eval      CAP30701    = $CAP3Sog
075800170728     C                   Eval      Local30701  = $Locali3Sog
075900170728     C                   Eval      Prov30701   = $Prov3Sog
076000170728     C                   Eval      Stato30701  = $Stato3Sog
076100170728     C                   Eval      IdPae30701  = $IdPaes3Sog
076200170728     C*
076300170728     C* RAPPRESENTANTE FISCALE
076400170728     C                   Eval      Stato40701  = $Stato4Sog
076500170728     C                   Eval      IdPae40701  = $IdPaes4Sog
076600170728     C                   Eval      PIVA40701   = $PIVA4Sog
076700170728     C*
076800170728     C                   Eval      CogRa40701  = $CogRag4Sog
076900170728     C                   Eval      TpN40701    = $TpN4Sog
077000170728     C                   Eval      Denom40701  = $Denom4Sog
077100170728     C                   Eval      Cogno40701  = $Cognom4Sog
077200170728     C                   Eval      Nome40701   = $Nome4Sog
077300170727     C*
077400170727     C                   ENDSR
077500170727      /EJECT
077600170727     C************************************************************
077700170727     C* Reperimento dati fiscali dipendenti dal movimento
077800170727     C************************************************************
077900170728     C     RepFisc_Mov   BEGSR
078000170728     C*
078100170728     C                   EXSR      Clear_Mov
078200170728     C*
078300170727     C                   Select
078400170728     C*
078500170728     C                   When      Tecnic0701 <> '2'
078600170727     C                   EXSR      RepANCPI
078700170728     C*
078800170727     C                   When      Tecnic0701  = '2'
078900170727     C                   EXSR      RepNDCPA
079000170727     C*
079100170727     C                   endSl
079200170728     C*
079300170731     C*
079400170731     C                   if        Found0701   = *On
079500170731     C*
079600170731     C                   Select
079700170731     C*
079800170731     C                   When      Tecnic0701 <> '2'
079900170731     C* verifico se sono presenti i dati aggiuntivi anagrafici
080000170731     C* se esistono sovrascrivo i dati appena reperiti
080100170731     C                   Eval      SO3Sogg     = CPISogg
080200170731     C                   Eval      SO3DtFiVl   = CPIDtFiVL
080300170731     C                   Eval      SO3Sys      = 0
080400170731     C                   Eval      SO3NrAsInt  = 0
080500170731     C                   EXSR      RepSO3_Mov
080600170731     C*
080700170731     C*
080800170731     C                   When      Tecnic0701  = '2'
080900170731     C* verifico se sono presenti i dati aggiuntivi anagrafici
081000170731     C* se esistono sovrascrivo i dati appena reperiti
081100170731     C                   Eval      SO3Sogg     = *Blank
081200170731     C                   Eval      SO3DtFiVl   = *Loval
081300170731     C                   Eval      SO3Sys      = CPASys
081400170731     C                   Eval      SO3NrAsInt  = CPANrAsInt
081500170731     C                   EXSR      RepSO3_Mov
081600170731     C*
081700170731     C                   endSl
081800170731     C*
081900170731     C                   endif
082000170727     C*
082100170728     C                   EXSR      RieOut_Mov
082200170727     C*
082300170727     C                   ENDSR
082400170727     C/EJECT
082500170727     C************************************************************
082600170727     C* Reperimento dati anagrafici da dati storicizzati
082700170727     C************************************************************
082800170727     C     RepANCPI      BEGSR
082900170727     C*
083000170727     C                   Eval      DVPLenOut = %size(ANCPI00F)
083100170727     C*
083200170727     C                   CALLB     'NDDVACPI'
083300170727     C                   PARM      '2'           DVPRic
083400170727     C                   PARM      XScSoc        DVPSocieta
083500170727     C                   PARM      *Blank        DVPUnita
083600170727     C                   PARM      'ANCPI     '  DVPStrutt
083700170727     C                   PARM      DtRif0701     DVPDtRif
083800170727     C                   PARM                    DVPLenOut
083900170727     C                   PARM      Sogg0701      DVPSogg
084000170727     C                   PARM      *Off          DVPErrore
084100170727     C                   PARM                    DVPOutPut
084200170727     C*
084300170728     C                   if        DVPErrore   = *Off
084400170728     C*
084500170728     C                   Eval      Found0701   = '1'
084600170728     C*
084700170727     C                   Eval      %subst(ANCPI00F:1:DVPLenout)
084800170727     C                              = %subst(DVPOutput:1:DVPLenout)
084900170727     C*
085000170727     C* trovati dati storicizzati alla data documento, sovrascrivo
085100170727     C* quelli validi attualmente per il soggetto
085200170727     C                   Eval      WNatGiur    = CPINatGiur
085300170727     C                   EXSR      RepTpN
085400170727     C                   if        XTbErr      = '0'
085500170727     C                   MoveL     XTbUni        ANGG16DS
085600170728     C                   Eval      $TpN1Mov    = TpNG16
085700170728     C                   else
085800170728     C* annullo il movimento
085900170728     C                   Eval      AnnMov0701  = *On
086000170728     C                   if        TpAnn0701   = *Blank
086100170728     C                   Eval      TpAnn0701   = '50'
086200170728     C                   endif
086300170727     C                   endif
086400170727     C*
086500170728     C                   MoveL(P)  CPIPartIva    $PartIVAMov
086600170728     C                   MoveL(P)  CPICdFisc     $CdFiscMov
086700170731     C                   MoveL     *Blank        $Stato1Mov
086800170731     C                   MoveL     *Blank        $IdPaes1Mov
086900170728     C                   MoveL(P)  CPIPartIva    $PIVA1Mov
087000170728     C                   MoveL(P)  CPICdFisc     $CdFisc1Mov
087100170727     C*
087200170728     C                   MoveL(P)  CPIDesSog1    $CogRag1Mov
087300170727     C*
087400170727     C* persona fisica
087500170728     C                   if        $TpN1Mov    = '1'
087600170728     C                   MoveL     *Blank        $Denom1Mov
087700170728     C                   MoveL(P)  CPIDesSog1    $Cognom1Mov
087800170731     C*
087900170731     C* se il soggetto è persona fisica i seguenti dati NON cambiano
088000170731     C* mai, percui se reperiti per il soggetto vanno comunque sempre
088100170731     C* bene
088200170731     C*******            MoveL     *Blank        $Nome1Mov
088300170727     C*
088400170727     C                   else
088500170727     C* società
088600170728     C                   MoveL(P)  CPIDesSog1    $Denom1Mov
088700170728     C                   MoveL     *Blank        $Cognom1Mov
088800170731     C                   MoveL     *Blank        $Nome1Mov
088900170727     C                   endif
089000170727     C*
089100170728     C                   MoveL(P)  CPIIndriz     $Indir2Mov
089200170728     C                   Move      *Blank        $NrCiv2Mov
089300170818fm8  C                   MoveL(P)  CPICAP        $CAP2Mov
089400170728     C                   MoveL(P)  CPILocalit    $Locali2Mov
089500170818fm8  C                   MoveL(P)  CPIProv       $Prov2Mov
089600170818fm8  C                   MoveL(P)  CPIStato      $Stato2Mov
089700170728     C                   Move      *Blank        $IdPaes2Mov
089800170731     C                   endif
089900170731     C*
090000170727     C*
090100170727     C* se non ci sono dati storicizzati alla data, non è un problema
090200170727     C* significa solo che sono validi quelli impostati sul soggetto
090300170727     C*
090400170727     C                   ENDSR
090500170727     C/EJECT
090600170727     C********************************************************************
090700170727     C* Reperimento dati anagrafici per conti estemporanei
090800170727     C********************************************************************
090900170727     C     RepNDCPA      BEGSR
091000170727     C*
091100170727     C                   Clear                   NDCPA00F
091200170727     C*
091300170727     C                   Eval      $LenOut     = %size(NDCPA00F)
091400170727     C                   CALL      'NDC066R2'
091500170727     C                   PARM      Sys0701       $Sys
091600170727     C                   PARM      0             $NrAsInt
091700170727     C                   PARM      NrAsRe0701    $NrAsGen
091800170727     C                   PARM                    $LenOut
091900170727     C                   PARM                    NDCPA00F
092000170727     C                   PARM      '0'           $Esito
092100170727     C*
092200170727     C* trovati dati fiscali per conti estemporanei, sovrascrivo
092300170727     C* quelli validi attualmente per il soggetto
092400170727     C                   if        $Esito      = '0'
092500170728     C*
092600170728     C                   Eval      Found0701   = '1'
092700170728     C*
092800170728     C                   MoveL(P)  CPAPartIva    $PartIVAMov
092900170728     C                   MoveL(P)  CPACdFisc     $CdFiscMov
093000170731     C                   MoveL     *Blank        $Stato1Mov
093100170731     C                   MoveL     *Blank        $IdPaes1Mov
093200170728     C                   MoveL(P)  CPAPartIva    $PIVA1Mov
093300170728     C                   MoveL(P)  CPACdFisc     $CdFisc1Mov
093400170727     C*
093500170728     C                   MoveL(P)  CPADescr      $CogRag1Mov
093600170728     C*
093700170731     C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
093800170731     C* ATTENZIONE:
093900170728     C* recupero la tipologia della natura giuridica da quella
094000170728     C* del soggetto perchè non presente sui dati NDCPA00F
094100170728     C                   MoveL(P)  TpN1Sg0701    $TpN1Mov
094200170731     C*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
094300170727     C*
094400170727     C* persona fisica
094500170728     C                   if        $TpN1Mov    = '1'
094600170728     C                   MoveL     *Blank        $Denom1Mov
094700170728     C                   MoveL(P)  CPADescr      $Cognom1Mov
094800170728     C                   MoveL     *Blank        $Nome1Mov
094900170727     C*
095000170727     C                   else
095100170727     C* società
095200170728     C                   MoveL(P)  CPADescr      $Denom1Mov
095300170728     C                   MoveL     *Blank        $Cognom1Mov
095400170728     C                   MoveL     *Blank        $Nome1Mov
095500170727     C                   endif
095600170727     C*
095700170728     C                   MoveL(P)  CPAIndiriz    $Indir2Mov
095800170731     C                   MoveL     *Blank        $NrCiv2Mov
095900170818fm8  C                   MoveL(P)  CPACAP        $CAP2Mov
096000170728     C                   MoveL(P)  CPALocalit    $Locali2Mov
096100170818fm8  C                   MoveL(P)  CPAProv       $Prov2Mov
096200170728     C                   MoveL(P)  CPAStato      $Stato2Mov
096300170731     C                   MoveL     *Blank        $IdPaes2Mov
096400170727     C*
096500170731     C                   endif
096600170731     C*
096700170731     C*
096800170731     C                   if        Found0701  <> '1'
096900170727     C*
097000170731     C* se il conto è tecnico, è fondamentale che esista ANCPA
097100170728     C                   Eval      AnnMov0701  = *On
097200170728     C                   if        TpAnn0701   = *Blank
097300170728     C                   Eval      TpAnn0701   = '07'
097400170731     C                   endif
097500170727     C*
097600170727     C                   endif
097700170727     C*
097800170727     C                   ENDSR
097900170727     C/EJECT
098000170727     C********************************************************************
098100170731     C* Reperisco i dati anagrafici aggiuntivi storicizzati del movimen
098200170727     C********************************************************************
098300170731     C     RepSO3_Mov    BEGSR
098400170727     C*
098500170731     C* IDENTIFICATIVI FISCALI E ALTRI DATI IDENTIFICATIVI (SEDE)
098600170728     C                   EXSR      RepSO3_Sede
098700170727     C*
098800170727     C                   if        *In21       = *Off
098900170728     C                   MoveL(P)  SO3PartIva    $PartIVAMov
099000170728     C                   MoveL(P)  SO3CdFisc     $CdFiscMov
099100170731     C                   MoveL     *Blank        $Stato1Mov
099200170731     C                   MoveL     *Blank        $IdPaes1Mov
099300170728     C                   MoveL(P)  SO3PartIva    $PIVA1Mov
099400170728     C                   MoveL(P)  SO3CdFisc     $CdFisc1Mov
099500170727     C*
099600170728     C                   MoveL(P)  SO3CogRag     $CogRag1Mov
099700170727     C*
099800170727     C* persona fisica
099900170728     C                   if        $TpN1Mov    = '1'
100000170728     C                   MoveL     *Blank        $Denom1Mov
100100170728     C                   MoveL(P)  SO3CogRag     $Cognom1Mov
100200170728     C                   MoveL(P)  SO3Nome       $Nome1Mov
100300170727     C*
100400170727     C                   else
100500170727     C* società
100600170728     C                   MoveL(P)  SO3CogRag     $Denom1Mov
100700170728     C                   MoveL     *Blank        $Cognom1Mov
100800170728     C                   MoveL     *Blank        $Nome1Mov
100900170727     C                   endif
101000170727     C*
101100170728     C                   MoveL(P)  SO3IndirE     $Indir2Mov
101200170728     C                   MoveL     *Blank        $NrCiv2Mov
101300170801     C                   MoveL(P)  SO3CAP        $CAP2Mov
101400170728     C                   MoveL(P)  SO3Localit    $Locali2Mov
101500170801     C                   MoveL(P)  SO3Prov       $Prov2Mov
101600170728     C                   MoveL(P)  SO3Stato      $Stato2Mov
101700170728     C                   MoveL     *Blank        $IdPaes2Mov
101800170727     C                   endif
101900170731     C*
102000170731     C*
102100170731     C* Reperisco il codice ISO 2 Alpha della nazione del movimento
102200170731     C                   if        $Stato2Mov <> *Blank
102300170731     C                   Eval      WStato      = $Stato2Mov
102400170731     C                   EXSR      RepTabHD4
102500170731     C*
102600170731     C                   if        XTbErr      = '1'
102700170731     C* annullo il movimento
102800170731     C                   Eval      AnnMov0701  = *On
102900170731     C                   if        TpAnn0701   = *Blank
103000170731     C                   Eval      TpAnn0701   = '51'
103100170731     C                   endif
103200170731     C                   endif
103300170731     C*
103400170731     C                   Eval      $IdPaes2Mov = CodISO2HD4
103500170731     C                   endif
103600170731     C*
103700170731     C*
103800170731     C*-----------------
103900170731     C* STABILE ORGANIZZAZIONE
104000170731     C                   EXSR      RepSO3_StaOrg
104100170731     C*
104200170731     C                   if        *In21       = *Off
104300170731     C*
104400170731     C                   MoveL(P)  SO3IndirE     $Indir3Mov
104500170731     C                   MoveL     *Blank        $NrCiv3Mov
104600170801     C                   MoveL(P)  SO3CAP        $CAP3Mov
104700170731     C                   MoveL(P)  SO3Localit    $Locali3Mov
104800170801     C                   MoveL(P)  SO3Prov       $Prov3Mov
104900170731     C                   MoveL(P)  SO3Stato      $Stato3Mov
105000170731     C                   MoveL     *Blank        $IdPaes3Mov
105100170731     C*
105200170731     C*
105300170731     C* Reperisco il codice ISO 2 Alpha della nazione del movimento
105400170731     C                   if        $Stato3Mov <> *Blank
105500170731     C                   Eval      WStato      = $Stato3Mov
105600170731     C                   EXSR      RepTabHD4
105700170731     C*
105800170731     C                   if        XTbErr      = '1'
105900170731     C* annullo il movimento
106000170731     C                   Eval      AnnMov0701  = *On
106100170731     C                   if        TpAnn0701   = *Blank
106200170731     C                   Eval      TpAnn0701   = '51'
106300170731     C                   endif
106400170731     C                   endif
106500170731     C*
106600170731     C                   Eval      $IdPaes3Mov = CodISO2HD4
106700170731     C                   endif
106800170731     C*
106900170731     C                   endif
107000170731     C*
107100170731     C*
107200170731     C*-----------------
107300170731     C* RAPPRESENTANTE FISCALE
107400170731     C                   EXSR      RepSO3_RapFisc
107500170731     C*
107600170731     C                   if        *In21       = *Off
107700170731     C*
107800170731     C                   MoveL(P)  SO3Stato      $Stato4Mov
107900170731     C                   MoveL     *Blank        $IdPaes4Mov
108000170731     C                   MoveL(P)  SO3PartIva    $PIVA4Mov
108100170731     C*
108200170731     C                   MoveL(P)  SO3CogRag     $CogRag4Mov
108300170731     C*
108400170731     C* persona fisica
108500170731     C                   if        SO3Nome    <> *Blank
108600170731     C                   MoveL(P)  '1'           $TpN4Mov
108700170731     C                   MoveL     *Blank        $Denom4Mov
108800170731     C                   MoveL(P)  SO3CogRag     $Cognom4Mov
108900170731     C                   MoveL(P)  SO3Nome       $Nome4Mov
109000170731     C*
109100170731     C                   else
109200170731     C* società generica
109300170731     C                   MoveL(P)  '2'           $TpN4Mov
109400170731     C                   MoveL(P)  SO3CogRag     $Denom4Mov
109500170731     C                   MoveL     *Blank        $Cognom4Mov
109600170731     C                   MoveL     *Blank        $Nome4Mov
109700170731     C                   endif
109800170731     C*
109900170731     C*-------------------
110000170731     C* Reperisco il codice ISO 2 Alpha della nazione del movimento
110100170731     C                   if        $Stato4Mov <> *Blank
110200170731     C                   Eval      WStato      = $Stato4Mov
110300170731     C                   EXSR      RepTabHD4
110400170731     C*
110500170731     C                   if        XTbErr      = '1'
110600170731     C* annullo il movimento
110700170731     C                   Eval      AnnMov0701  = *On
110800170731     C                   if        TpAnn0701   = *Blank
110900170731     C                   Eval      TpAnn0701   = '51'
111000170731     C                   endif
111100170731     C                   endif
111200170731     C*
111300170731     C                   Eval      $IdPaes4Mov = CodISO2HD4
111400170731     C                   endif
111500170731     C*
111600170731     C                   endif
111700170727     C*
111800170727     C                   ENDSR
111900170727     C/EJECT
112000170728     C************************************************************
112100170728     C* Pulisco i campi del movimento
112200170728     C************************************************************
112300170728     C     Clear_Mov     BEGSR
112400170728     C*
112500170728     C* IDENTIFICATIVI FISCALI
112600170728     C                   Clear                   $PartIVAMov
112700170728     C                   Clear                   $CdFiscMov
112800170731     C                   Clear                   $Stato1Mov
112900170731     C                   Clear                   $IdPaes1Mov
113000170728     C                   Clear                   $PIVA1Mov
113100170728     C                   Clear                   $CdFisc1Mov
113200170728     C*
113300170728     C* ALTRI DATI IDENTIFICATIVI
113400170728     C                   Clear                   $CogRag1Mov
113500170728     C                   Clear                   $TpN1Mov
113600170728     C                   Clear                   $Denom1Mov
113700170728     C                   Clear                   $Cognom1Mov
113800170728     C                   Clear                   $Nome1Mov
113900170728     C                   Clear                   $Indir2Mov
114000170728     C                   Clear                   $NrCiv2Mov
114100170728     C                   Clear                   $CAP2Mov
114200170728     C                   Clear                   $Locali2Mov
114300170728     C                   Clear                   $Prov2Mov
114400170728     C                   Clear                   $Stato2Mov
114500170728     C                   Clear                   $IdPaes2Mov
114600170728     C*
114700170728     C* STABILE ORGANIZZAZIONE
114800170728     C                   Clear                   $Indir3Mov
114900170728     C                   Clear                   $NrCiv3Mov
115000170728     C                   Clear                   $CAP3Mov
115100170728     C                   Clear                   $Locali3Mov
115200170728     C                   Clear                   $Prov3Mov
115300170728     C                   Clear                   $Stato3Mov
115400170728     C                   Clear                   $IdPaes3Mov
115500170728     C*
115600170728     C* RAPPRESENTANTE FISCALE
115700170728     C                   Clear                   $Stato4Mov
115800170728     C                   Clear                   $IdPaes4Mov
115900170728     C                   Clear                   $PIVA4Mov
116000170728     C*
116100170728     C                   Clear                   $CogRag4Mov
116200170728     C                   Clear                   $TpN4Mov
116300170728     C                   Clear                   $Denom4Mov
116400170728     C                   Clear                   $Cognom4Mov
116500170728     C                   Clear                   $Nome4Mov
116600170728     C*
116700170728     C                   ENDSR
116800170728     C/EJECT
116900170727     C************************************************************
117000170727     C* Imposto i dati di output recuperati dal movimento del conto
117100170727     C************************************************************
117200170728     C     RieOut_Mov    BEGSR
117300170727     C*
117400170728     C                   Eval      PartIV0701  = $PartIVAMov
117500170728     C                   Eval      CdFisc0701  = $CdFiscMov
117600170731     C                   Eval      Stato10701  = $Stato1Mov
117700170731     C                   Eval      IdPae10701  = $IdPaes1Mov
117800170728     C                   Eval      PIVA10701   = $PIVA1Mov
117900170728     C                   Eval      CdFisc0701  = $CdFisc1Mov
118000170727     C*
118100170728     C                   Eval      CogRa10701  = $CogRag1Mov
118200170728     C                   Eval      TpN10701    = $TpN1Mov
118300170728     C                   Eval      Denom10701  = $Denom1Mov
118400170728     C                   Eval      Cogno10701  = $Cognom1Mov
118500170728     C                   Eval      Nome10701   = $Nome1Mov
118600170727     C*
118700170728     C                   Eval      Indir20701  = $Indir2Mov
118800170728     C                   Eval      NrCiv20701  = $NrCiv2Mov
118900170728     C                   Eval      CAP20701    = $CAP2Mov
119000170728     C                   Eval      Local20701  = $Locali2Mov
119100170728     C                   Eval      Prov20701   = $Prov2Mov
119200170728     C                   Eval      Stato20701  = $Stato2Mov
119300170728     C                   Eval      IdPae20701  = $IdPaes2Mov
119400170727     C*
119500170727     C                   ENDSR
119600170727      /EJECT
119700170728     C************************************************************
119800170728     C* Reperimento dati fiscali della società dichiarante
119900170728     C************************************************************
120000170728     C     RepFisc_Soc   BEGSR
120100170728     C*
120200170728     C                   EXSR      Clear_Soc
120300170728     C*
120400170728     C* reperisco la natura giuridica del contribuente
120500170728     C                   Eval      WNatGiur    = XScNCR
120600170728     C                   EXSR      RepTpN
120700170728     C                   if        XTbErr      = '0'
120800170728     C                   MoveL     XTbUni        ANGG16DS
120900170728     C                   Eval      $TpN1Soc    = TpNG16
121000170728     C                   else
121100170728     C* errore, esco
121200170731    >C                   MOVE      '06'          ERR0701
121300170728     C                   EXSR      ENDPGM
121400170728     C                   endif
121500170728     C*
121600170728     C                   Select
121700170728     C*
121800170728     C*-----------------------
121900170728     C* PERSONA FISICA
122000170728     C                   When      $TpN1Soc    = '1'
122100170728     C*
122200170728     C                   Eval      SO3Sogg     = SocSogg
122300170728     C                   Move      *Loval        SO3DtFiVl
122400170728     C                   Eval      SO3Sys      = 0
122500170728     C                   Eval      SO3NrAsInt  = 0
122600170728     C                   EXSR      RepSO3_Sede
122700170728     C*
122800170728     C                   if        *In21       = *Off
122900170728     C*
123000170728     C                   Eval      Found0701   = *On
123100170728     C*
123200170728     C                   MoveL(P)  SO3PartIva    $PartIVASoc
123300170728     C                   MoveL(P)  SO3CdFisc     $CdFiscSoc
123400170731     C                   MoveL(P)  *Blank        $Stato1Soc
123500170728     C* per definizione il contribuente deve avere partita IVA italiana
123600170728     C                   MoveL(P)  'IT'          $IdPaes1Soc
123700170728     C                   MoveL(P)  SO3PartIva    $PIVA1Soc
123800170728     C                   MoveL(P)  SO3CdFisc     $CdFisc1Soc
123900170728     C*
124000170728     C                   MoveL(P)  SO3CogRag     $CogRag1Soc
124100170728     C                   MoveL(P)  *Blank        $Denom1Soc
124200170728     C                   MoveL(P)  SO3CogRag     $Cognom1Soc
124300170728     C                   MoveL(P)  SO3Nome       $Nome1Soc
124400170728     C*
124500170728     C                   MoveL(P)  SO3IndirE     $Indir2Soc
124600170728     C                   MoveL     *Blank        $NrCiv2Soc
124700170801     C                   MoveL(P)  SO3CAP        $CAP2Soc
124800170728     C                   MoveL(P)  SO3Localit    $Locali2Soc
124900170801     C                   MoveL(P)  SO3Prov       $Prov2Soc
125000170728     C                   MoveL(P)  SO3Stato      $Stato2Soc
125100170728     C                   MoveL     *Blank        $IdPaes2Soc
125200170728     C*
125300170728     C                   endif
125400170728     C*
125500170728     C*-----------------------
125600170728     C* PERSONA GIURIDICA
125700170728     C                   When      $TpN1Soc    = '2'  or
125800170728     C                             $TpN1Soc    = '3'
125900170728     C*
126000170728     C                   Eval      Found0701   = *On
126100170728     C*
126200170728     C                   MoveL(P)  XScIVA        $PartIVASoc
126300170728     C                   MoveL(P)  XScCdF        $CdFiscSoc
126400170731     C                   MoveL     *Blank        $Stato1Soc
126500170728     C* per definizione il contribuente deve avere partita IVA italiana
126600170728     C                   MoveL(P)  'IT'          $IdPaes1Soc
126700170728     C                   MoveL(P)  XScIVA        $PIVA1Soc
126800170728     C                   MoveL(P)  XScCdF        $CdFisc1Soc
126900170728     C* Denominazione
127000170728     C                   MoveL(P)  XScRgS        $CogRag1Soc
127100170728     C                   MoveL(P)  XScRgS        $Denom1Soc
127200170728     C                   MoveL(P)  *Blank        $Cognom1Soc
127300170728     C                   MoveL(P)  *Blank        $Nome1Soc
127400170728     C*
127500170728     C                   MoveL(P)  XScVia        $Indir2Soc
127600170728     C                   MoveL     *Blank        $NrCiv2Soc
127700170818fm8  C                   MoveL(P)  XScCAP        $CAP2Soc
127800170728     C                   MoveL(P)  XScCit        $Locali2Soc
127900170818fm8  C                   MoveL(P)  XScPrv        $Prov2Soc
128000170728     C                   MoveL(P)  XScSta        $Stato2Soc
128100170728     C                   MoveL     *Blank        $IdPaes2Soc
128200170728     C*
128300170728     C                   endSl
128400170728     C*
128500170728     C*
128600170731     C                   if        Found0701   = *On
128700170731     C*
128800170728     C* Reperisco il codice ISO 2 Alpha della Sede del dichiarante
128900170728     C                   Eval      WStato      = $Stato2Soc
129000170728     C                   EXSR      RepTabHD4
129100170728     C*
129200170728     C                   if        XTbErr      = '1'
129300170728     C* errore, esco
129400170821fm10 C                   MOVE      '05'          ERR0701
129500170728     C                   EXSR      ENDPGM
129600170728     C                   endif
129700170728     C*
129800170728     C                   if        CodISO2HD4  = *Blank
129900170731     C                   MOVE      '05'          ERR0701
130000170728     C                   EXSR      ENDPGM
130100170728     C                   endif
130200170728     C*
130300170728     C                   Eval      $IdPaes2Soc = CodISO2HD4
130400170728     C*
130500170731     C                   Eval      SO3Sogg     = SocSogg
130600170731     C                   Move      *Loval        SO3DtFiVl
130700170731     C                   Eval      SO3Sys      = 0
130800170731     C                   Eval      SO3NrAsInt  = 0
130900170731     C                   EXSR      RepSO3_Soc
131000170731     C*
131100170731     C                   endif
131200170728     C*
131300170728     C                   EXSR      RieOut_Soc
131400170728     C*
131500170728     C                   ENDSR
131600170728     C/EJECT
131700170728     C************************************************************
131800170728     C* Pulisco i campi della società
131900170728     C************************************************************
132000170728     C     Clear_Soc     BEGSR
132100170728     C*
132200170728     C* IDENTIFICATIVI FISCALI
132300170728     C                   Clear                   $PartIVASoc
132400170728     C                   Clear                   $CdFiscSoc
132500170731     C                   Clear                   $Stato1Soc
132600170731     C                   Clear                   $IdPaes1Soc
132700170728     C                   Clear                   $PIVA1Soc
132800170728     C                   Clear                   $CdFisc1Soc
132900170728     C*
133000170728     C* ALTRI DATI IDENTIFICATIVI
133100170728     C                   Clear                   $CogRag1Soc
133200170728     C                   Clear                   $TpN1Soc
133300170728     C                   Clear                   $Denom1Soc
133400170728     C                   Clear                   $Cognom1Soc
133500170728     C                   Clear                   $Nome1Soc
133600170728     C                   Clear                   $Indir2Soc
133700170728     C                   Clear                   $NrCiv2Soc
133800170728     C                   Clear                   $CAP2Soc
133900170728     C                   Clear                   $Locali2Soc
134000170728     C                   Clear                   $Prov2Soc
134100170728     C                   Clear                   $Stato2Soc
134200170728     C                   Clear                   $IdPaes2Soc
134300170728     C*
134400170728     C* STABILE ORGANIZZAZIONE
134500170728     C                   Clear                   $Indir3Soc
134600170728     C                   Clear                   $NrCiv3Soc
134700170728     C                   Clear                   $CAP3Soc
134800170728     C                   Clear                   $Locali3Soc
134900170728     C                   Clear                   $Prov3Soc
135000170728     C                   Clear                   $Stato3Soc
135100170728     C                   Clear                   $IdPaes3Soc
135200170728     C*
135300170728     C* RAPPRESENTANTE FISCALE
135400170728     C                   Clear                   $Stato4Soc
135500170728     C                   Clear                   $IdPaes4Soc
135600170728     C                   Clear                   $PIVA4Soc
135700170728     C*
135800170728     C                   Clear                   $CogRag4Soc
135900170728     C                   Clear                   $TpN4Soc
136000170728     C                   Clear                   $Denom4Soc
136100170728     C                   Clear                   $Cognom4Soc
136200170728     C                   Clear                   $Nome4Soc
136300170731     C*
136400170731     C                   ENDSR
136500170731     C/EJECT
136600170731     C************************************************************
136700170731     C* Recupero i dati aggiuntivi anagrafici della società
136800170731     C************************************************************
136900170731     C     RepSO3_Soc    BEGSR
137000170731     C*
137100170731     C*-----------------
137200170731     C* STABILE ORGANIZZAZIONE
137300170731     C                   EXSR      RepSO3_StaOrg
137400170731     C*
137500170731     C                   if        *In21       = *Off
137600170731     C*
137700170731     C                   MoveL(P)  SO3IndirE     $Indir3Soc
137800170731     C                   MoveL     *Blank        $NrCiv3Soc
137900170801     C                   MoveL(P)  SO3CAP        $CAP3Soc
138000170731     C                   MoveL(P)  SO3Localit    $Locali3Soc
138100170801     C                   MoveL(P)  SO3Prov       $Prov3Soc
138200170731     C                   MoveL(P)  SO3Stato      $Stato3Soc
138300170731     C                   MoveL     *Blank        $IdPaes3Soc
138400170731     C*
138500170731     C*
138600170731     C* Reperisco il codice ISO 2 Alpha della Sede del dichiarante
138700170731     C                   Eval      WStato      = $Stato3Soc
138800170731     C                   EXSR      RepTabHD4
138900170731     C*
139000170731     C                   if        XTbErr      = '1'
139100170731     C* errore, esco
139200170731    >C                   MOVE      '04'          ERR0701
139300170731     C                   EXSR      ENDPGM
139400170731     C                   endif
139500170731     C*
139600170731     C                   if        CodISO2HD4  = *Blank
139700170731     C                   MOVE      '05'          ERR0701
139800170731     C                   EXSR      ENDPGM
139900170731     C                   endif
140000170731     C*
140100170731     C                   Eval      $IdPaes3Soc = CodISO2HD4
140200170731     C*
140300170731     C*
140400170731     C                   endif
140500170731     C*
140600170731     C*
140700170731     C*-----------------
140800170731     C* RAPPRESENTANTE FISCALE
140900170731     C*
141000170731     C                   Eval      SO3Sogg     = SocSogg
141100170731     C                   Move      *Loval        SO3DtFiVl
141200170731     C                   Eval      SO3Sys      = 0
141300170731     C                   Eval      SO3NrAsInt  = 0
141400170731     C                   EXSR      RepSO3_RapFisc
141500170731     C*
141600170731     C                   if        *In21       = *Off
141700170731     C*
141800170731     C                   MoveL(P)  SO3Stato      $Stato4Soc
141900170731     C                   MoveL     *Blank        $IdPaes4Soc
142000170731     C                   MoveL(P)  SO3PartIva    $PIVA4Soc
142100170731     C*
142200170731     C                   MoveL(P)  SO3CogRag     $CogRag4Soc
142300170731     C*
142400170731     C* persona fisica
142500170731     C                   if        SO3Nome    <> *Blank
142600170731     C                   MoveL(P)  '1'           $TpN4Soc
142700170731     C                   MoveL     *Blank        $Denom4Soc
142800170731     C                   MoveL(P)  SO3CogRag     $Cognom4Soc
142900170731     C                   MoveL(P)  SO3Nome       $Nome4Soc
143000170731     C*
143100170731     C                   else
143200170731     C* società generica
143300170731     C                   MoveL(P)  '2'           $TpN4Soc
143400170731     C                   MoveL(P)  SO3CogRag     $Denom4Soc
143500170731     C                   MoveL     *Blank        $Cognom4Soc
143600170731     C                   MoveL     *Blank        $Nome4Soc
143700170731     C                   endif
143800170731     C*
143900170731     C*
144000170731     C* Reperisco il codice ISO 2 Alpha della Sede del dichiarante
144100170731     C                   Eval      WStato      = $Stato4Soc
144200170731     C                   EXSR      RepTabHD4
144300170731     C*
144400170731     C                   if        XTbErr      = '1'
144500170731     C* errore, esco
144600170731    >C                   MOVE      '04'          ERR0701
144700170731     C                   EXSR      ENDPGM
144800170731     C                   endif
144900170731     C*
145000170731     C                   if        CodISO2HD4  = *Blank
145100170731     C                   MOVE      '05'          ERR0701
145200170731     C                   EXSR      ENDPGM
145300170731     C                   endif
145400170731     C*
145500170731     C                   Eval      $IdPaes4Soc = CodISO2HD4
145600170731     C*
145700170731     C                   endif
145800170728     C*
145900170728     C                   ENDSR
146000170728     C/EJECT
146100170728     C************************************************************
146200170728     C* Imposto i dati di output recuperati dalla società contribuente
146300170728     C************************************************************
146400170728     C     RieOut_Soc    BEGSR
146500170728     C*
146600170728     C                   Eval      PartIV0701  = $PartIVASoc
146700170728     C                   Eval      CdFisc0701  = $CdFiscSoc
146800170731     C                   Eval      Stato10701  = $Stato1Soc
146900170731     C                   Eval      IdPae10701  = $IdPaes1Soc
147000170728     C                   Eval      PIVA10701   = $PIVA1Soc
147100170728     C                   Eval      CdFisc0701  = $CdFisc1Soc
147200170728     C*
147300170728     C                   Eval      CogRa10701  = $CogRag1Soc
147400170728     C                   Eval      TpN10701    = $TpN1Soc
147500170728     C                   Eval      Denom10701  = $Denom1Soc
147600170728     C                   Eval      Cogno10701  = $Cognom1Soc
147700170728     C                   Eval      Nome10701   = $Nome1Soc
147800170728     C*
147900170728     C                   Eval      Indir20701  = $Indir2Soc
148000170728     C                   Eval      NrCiv20701  = $NrCiv2Soc
148100170728     C                   Eval      CAP20701    = $CAP2Soc
148200170728     C                   Eval      Local20701  = $Locali2Soc
148300170728     C                   Eval      Prov20701   = $Prov2Soc
148400170728     C                   Eval      Stato20701  = $Stato2Soc
148500170728     C                   Eval      IdPae20701  = $IdPaes2Soc
148600170728     C*
148700170728     C                   ENDSR
148800170728      /EJECT
148900000000     C************************************************************
149000000000     C* OPERAZIONI INIZIALI
149100000000     C************************************************************
149200000000     C     *INZSR        BEGSR
149300000000     C*
149400000000     C     *ENTRY        PLIST
149500000000     C                   PARM                    KPJBA
149600170727     C                   PARM                    SDG00701DS
149700000000     C*
149800000000     C* Reperimento dati società
149900000000     C                   MOVEL     'SOC001'      TIPXSC
150000000000     C                   MOVEL     *BLANK        SOCXSC
150100130827     C                   EXSR      REPSOC
150200000000     C*
150300000000     C* Controlli dati società
150400000000     C     RTNXSC        IFNE      '1'
150500000000     C                   MOVEL     XSOCDS        SOC001
150600000000     C                   ELSE
150700170731    >C                   MOVE      '02'          ERR0701
150800130827     C                   EXSR      ENDPGM
150900000000     C                   ENDIF
151000170727     C*
151100170727     C                   EXSR      RepSDSPETRI
151200000000     C*
151300000000     C                   ENDSR
151400000000     C/EJECT
151500000000     C************************************************************
151600000000     C* REPERIMENTO DATI SOCIETA'
151700000000     C************************************************************
151800000000     C     REPSOC        BEGSR
151900000000     C*
152000000000     C                   CALL      'SDGSOCDR'
152100000000     C                   PARM                    TIPXSC            6
152200000000     C                   PARM                    SOCXSC            3
152300000000     C                   PARM                    CDSXSC            9 0
152400000000     C                   PARM                    MODXSC            3
152500000000     C                   PARM      *BLANKS       RTNXSC            1
152600000000     C                   PARM                    XSOCDS
152700000000     C                   PARM                    KPJBA
152800000000     C*
152900000000     C                   ENDSR
153000000000     C/EJECT
153100170727     C************************************************************
153200170727     C* Reperimento parametri di setup per Spesometro Trimestrale
153300170727     C************************************************************
153400170727     C     RepSDSPETRI   BEGSR
153500170727     C*
153600170727     C                   Clear                   SDGPA12DS
153700170727     C*
153800170727     C                   CALLB     'XPAR'
153900170727     C                   Parm                    XScSoc
154000170727     C                   Parm      'SDSPETRI    'XPaPar            8
154100170727     C                   Parm                    XPaOut         2000
154200170727     C                   Parm      *OFF          XPaErr            1
154300170727     C                   Parm      ' '           XPaRic            1
154400170727     C*
154500170727     C                   if        XPaErr      = *On
154600170731     C                   Eval      ERR0701     = '03'
154700170727     C                   EXSR      ENDPGM
154800170727     C*
154900170727     C                   else
155000170727     C                   MoveL     XPaOut        SDGPA12DS
155100170727     C*
155200170727     C                   if        AttCSTA12   <> *On
155300170731     C                   Eval      ERR0701     = '04'
155400170727     C                   EXSR      ENDPGM
155500170727     C*
155600170727     C                   endif
155700170727     C*
155800170727     C                   endif
155900170727     C*
156000170727     C                   ENDSR
156100170727     C/EJECT
156200000000     C************************************************************
156300000000     C* INIZIALIZZAZIONE VARIABILI
156400000000     C************************************************************
156500000000     C     INZVAR        BEGSR
156600170731     C*
156700170731     C                   SELECT
156800170731     C*
156900170731     C* recupero dati C/F da anagrafica soggetto
157000170731     C                   WHEN      OPZ0701  = '01'
157100170731     C*
157200170731     C                   if        Sogg0701    = *Blank    or
157300170731     C                             CF0701      = *Blank
157400170731     C* annullo il conto
157500170731     C                   Eval      AnnCon0701  = *On
157600170731     C                   if        TpAnn0701   = *Blank
157700170731     C                   Eval      TpAnn0701   = '07'
157800170731     C                   endif
157900170731     C*
158000170731    >C                   MOVE      '07'          ERR0701
158100170731     C                   EXSR      ENDPGM
158200170731     C*
158300170731     C                   endif
158400170731     C*
158500170731     C*
158600170731     C* recupero dati C/F dipendenti dal movimento
158700170731     C                   WHEN      OPZ0701  = '02'
158800170731     C*
158900170731     C                   Select
159000170731     C*
159100170731     C                   When      Tecnic0701 <> '2'
159200170731     C*
159300170731     C                   if        DtRif0701   = *Loval    or
159400170731     C                             Sogg0701    = *Blank
159500170731     C                   Eval      AnnMov0701  = *On
159600170731     C                   if        TpAnn0701   = *Blank
159700170731     C                   Eval      TpAnn0701   = '07'
159800170731     C                   endif
159900170731    >C                   MOVE      '07'          ERR0701
160000170731     C                   EXSR      ENDPGM
160100170808     C                   endif
160200170731     C*
160300170731     C                   When      Tecnic0701  = '2'
160400170731     C*
160500170731     C                   if        NrAsRe0701  = 0         or
160600170731     C                             TpN1Sg0701  = *Blank
160700170731     C*
160800170731     C* se il conto è tecnico, è fondamentale che esista ANCPA
160900170731     C                   Eval      AnnMov0701  = *On
161000170731     C                   if        TpAnn0701   = *Blank
161100170731     C                   Eval      TpAnn0701   = '07'
161200170731     C                   endif
161300170731     C*
161400170731    >C                   MOVE      '07'          ERR0701
161500170731     C                   EXSR      ENDPGM
161600170731     C*
161700170731     C                   endif
161800170731     C*
161900170731     C                   endSl
162000170731     C*
162100170731     C* recupero dati dichiarante (XSCSOC)
162200170731     C                   WHEN      OPZ0701  = '03'
162300170731     C*
162400170731     C* se non trovo la società, esco già per questa anomalia
162500170731     C*
162600170731     C                   ENDSL
162700170728     C*
162800170731     C*
162900170728     C* se i parametri sono gli stessi dell'ultima chiamata,
163000170728     C* ripasso i dati recuperati in precedenza
163100170728     C                   if        Opz0701     = SavOpz      and
163200170728     C                             Soc0701     = SavSocieta  and
163300170728     C                             TpCom0701   = SavTpCom    and
163400170728     C                             Anno0701    = SavAnno     and
163500170728     C                             Trim0701    = SavTrim     and
163600170728     C                             Mese0701    = SavMese     and
163700170728     C                             Sogg0701    = SavSogg     and
163800170728     C                             CF0701      = SavCF       and
163900170728     C                             Tecnic0701  = SavTecnico  and
164000170728     C                             DtRif0701   = SavDtRif    and
164100170728     C                             Sys0701     = SavSys      and
164200170728     C                             NrAsRe0701  = SavNrAsReg
164300170728     C*
164400170728     C                   EXSR      ENDPGM
164500170728     C*
164600170728     C                   else
164700170728     C*
164800170731     C                   Eval      Err0701     = '00'
164900170728     C                   Eval      Found0701   = '0'
165000170728     C                   Eval      TpAnn0701   = *Blank
165100170728     C                   Eval      AnnCon0701  = *Off
165200170728     C                   Eval      AnnMov0701  = *Off
165300170728     C*
165400170728     C                   Eval      PartIV0701  = *Blank
165500170728     C                   Eval      CdFisc0701  = *Blank
165600170731     C                   Eval      Stato10701  = *Blank
165700170731     C                   Eval      IdPae10701  = *Blank
165800170728     C                   Eval      PIVA10701   = *Blank
165900170728     C                   Eval      CdFis10701  = *Blank
166000170728     C                   Eval      CogRa10701  = *Blank
166100170728     C                   Eval      TpN10701    = *Blank
166200170728     C                   Eval      Denom10701  = *Blank
166300170728     C                   Eval      Cogno10701  = *Blank
166400170728     C                   Eval      Nome10701   = *Blank
166500170728     C                   Eval      Indir20701  = *Blank
166600170728     C                   Eval      NrCiv20701  = *Blank
166700170728     C                   Eval      CAP20701    = *Blank
166800170728     C                   Eval      Local20701  = *Blank
166900170728     C                   Eval      Prov20701   = *Blank
167000170728     C                   Eval      Stato20701  = *Blank
167100170728     C                   Eval      IdPae20701  = *Blank
167200170728     C                   Eval      Indir30701  = *Blank
167300170728     C                   Eval      NrCiv30701  = *Blank
167400170728     C                   Eval      CAP30701    = *Blank
167500170728     C                   Eval      Local30701  = *Blank
167600170728     C                   Eval      Prov30701   = *Blank
167700170728     C                   Eval      Stato30701  = *Blank
167800170728     C                   Eval      IdPae30701  = *Blank
167900170731     C                   Eval      Stato40701  = *Blank
168000170728     C                   Eval      IdPae40701  = *Blank
168100170728     C                   Eval      PIVA40701   = *Blank
168200170728     C                   Eval      CogRa40701  = *Blank
168300170728     C                   Eval      TpN40701    = *Blank
168400170728     C                   Eval      Denom40701  = *Blank
168500170728     C                   Eval      Cogno40701  = *Blank
168600170728     C                   Eval      Nome40701   = *Blank
168700170728     C*
168800170728     C                   endif
168900000000     C*
169000000000     C                   ENDSR
169100170726     C/EJECT
169200000000     C************************************************************
169300000000     C* DEFINIZIONE KLIST
169400000000     C************************************************************
169500000000     C     DefKlist      BEGSR
169600000000     C*
169700000000     C* klist
169800170727     C*
169900170727     C* SDGSO301L
170000170727     C     K05SO301      KLIST
170100170727     C                   KFLD                    SO3Sogg
170200170727     C                   KFLD                    SO3DtFiVL
170300170727     C                   KFLD                    SO3Sys
170400170727     C                   KFLD                    SO3NrAsint
170500170727     C                   KFLD                    SO3Dest
170600000000     C*
170700000000     C*
170800000000     C                   ENDSR
170900000000     C/EJECT
171000000000     C************************************************************
171100000000     C* FINE PROGRAMMA
171200000000     C************************************************************
171300000000     C     ENDPGM        BEGSR
171400000000     C*
171401180306>>>>>C                   IF        partIv0701 = '$$'
171402180306>>>>>C                   CLEAR                   partIv0701
171403180306>>>>>C                   ENDIF
171404180306>>>>>C                   IF        cdFisc0701 = '$$'
171405180306>>>>>C                   CLEAR                   cdFisc0701
171406180306>>>>>C                   ENDIF
171500170727     C                   Eval      *InRT       = *On
171600170728     C                   RETURN
171700000000     C*
171800000000     C                   ENDSR
171900170727     C/EJECT
