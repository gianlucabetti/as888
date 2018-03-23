000100110721     H*BIND
000200110721     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300110721     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400110721      *
000500110721     F*----------------------------------------------------*
000600110721      *
000601151124      *                  Black List
000602151124      *
000603151120      *  Verifica raggiungimento soglia per dati anagrafici
000604131014      *
000700151120      * Annullo i movimenti che non raggiungono la soglia se
000701151118      * richiesto
000800110722      *
002100110826      *
002200110721     F*----------------------------------------------------*
002201151120     F* Bl - Soglie di comunicazione
002202151120     F* Key: Soc/DtIni
002203151120     FSDGBLS01L IF   E           K DISK
002300131014     F* Movimenti estratti
002700160129     F* Key: Soc/TpCom/Anno/Dati di raggruppam.
002800160129     FSDGSCF02L UF A E           K DISK    UsrOpn
002900110721     F                                     COMMIT
004700110721     F*------------
004800131014     FSDGSG52P1 O    E             PRINTER OFLIND(*In41)
004900110721     F                                     USROPN
005000110721     D*----------------------------------------------------*
005100110721     D* Parametri per apertura file di stampa
005200131014     D SDG00500DS    E DS
005300110721     D*-------------
005400110721     D* Passaggio Parametri
005500110721     D KPJBA         E DS
005600110721     D*-------------
005700151118     D SDG00508DS    E DS
006600110721     D*---------
006700110721     D* Richiamo a SDGTABDR
006800110721     D SDGTABDS      E DS
006801160201     D*-------------
006802160201     D* Tabella degli Stati
006803160201     D ANGG12DS      E DS
006900110721     D*---------
007000110721     D* Contabilità
007100110721     D ANGG84DS      E DS                  INZ
007200110721     D*------------------
007300110721     D* Controlli validità legende
007400110721     D X07ValDS      E DS
007500110721     D*-------------
007600110721     D* Dati di societa
007700110721     D SOC001        E DS                  EXTNAME(SDGSOCDS)
007800110721     D XSOCDS          DS          1000
008300110721     D*-------------
008400110721     D* Reperimento nome PGM
008500110721     D STATUS         SDS           333
008600110721     D  DSPGM            *PROC
008700110721     D  PARMS            *PARMS
008800110721     D*-------------
008900110721     D* DS Esterna gestione messaggi
009000110721     D XMSGDS        E DS                  Inz
009100110721     D  STS          E                     EXTFLD(MSGSTS)
009200110721     D                                     DIM(333)
009300110721     D  JBA          E                     EXTFLD(MSGJBA)
009400110721     D                                     DIM(502)
009500110721     D*---------
009600110721     D* Definizioni parametri standard chiamate moduli
009700110721     D $Esito          S              1
009800110721     D $GesMsg         S              1
009900110721     D $LenOut         S              9B 0
010000110721     D $Struttura      S             10
010100110721     D $CodOper        S              1
010200110721     D $Output         S           4000
010300110721     D $Refresh        S              1
010400110721     D $Uso            S              1
010500110721     D*-------------
010600110721     D* Salvataggio Kpjbu per richiamo altri pgm
010700110721     D SavKPJBU        S            256
010800110721     D*-------------
010900110721     D* Campi di lavoro
010901131014     D $Totale         S                   Like(SCFImpOpe)
010902160201     D $ImpSoglia      S                   Like(SCFImpSogl)
010903160201     D $IVASoglia      S                   Like(SCFIVASogl)
010904131014     D $FoundSCF       S              1A
010905160129      *
010907160129     D SavCogRagR      S                   Like(SCFCogRagR)
010908160129     D SavNomeR        S                   Like(SCFNomeR  )
010909160129     D SavDtNasR       S                   Like(SCFDtNasR )
010910160129     D SavLocNasR      S                   Like(SCFLocNasR)
010911160129     D SavPrvNasR      S                   Like(SCFPrvNasR)
010912160129     D SavStatoR       S                   Like(SCFStatoR )
010913160129     D SavCodUICR      S                   Like(SCFCodUICR)
010914160129     D SavLocaliR      S                   Like(SCFLocaliR)
010915160129     D SavIndireR      S                   Like(SCFIndireR)
010916160129     D SavTpNR         S                   Like(SCFTpNR   )
010917160129     D SavPIVAR        S                   Like(SCFPIVAR  )
010918160129     D SavCdFiscR      S                   Like(SCFCdFiscR)
010919160129     D SavTpOpeR       S                   Like(SCFTpOpeR )
010920160129     D SavNatOpeR      S                   Like(SCFNatOpeR)
010921160129     D SavLeasinR      S                   Like(SCFLeasinR)
010922160129     D SavDtRegR       S                   Like(SCFDtRegR )
010923160129     D SavNrRegR       S                   Like(SCFNrRegR )
010924160129     D SavSerRegR      S                   Like(SCFSerRegR)
010925160129      *
010926160129     D Sa2CogRagR      S                   Like(SCFCogRagR)
010927160129     D Sa2NomeR        S                   Like(SCFNomeR  )
010928160129     D Sa2DtNasR       S                   Like(SCFDtNasR )
010929160129     D Sa2LocNasR      S                   Like(SCFLocNasR)
010930160129     D Sa2PrvNasR      S                   Like(SCFPrvNasR)
010931160129     D Sa2StatoR       S                   Like(SCFStatoR )
010932160129     D Sa2CodUICR      S                   Like(SCFCodUICR)
010933160129     D Sa2LocaliR      S                   Like(SCFLocaliR)
010934160129     D Sa2IndireR      S                   Like(SCFIndireR)
010935160129     D Sa2TpNR         S                   Like(SCFTpNR   )
010936160129     D Sa2PIVAR        S                   Like(SCFPIVAR  )
010937160129     D Sa2CdFiscR      S                   Like(SCFCdFiscR)
010938160129     D Sa2TpOpeR       S                   Like(SCFTpOpeR )
010939160129     D Sa2NatOpeR      S                   Like(SCFNatOpeR)
010940160129     D Sa2LeasinR      S                   Like(SCFLeasinR)
010941160129     D Sa2DtRegR       S                   Like(SCFDtRegR )
010942160129     D Sa2NrRegR       S                   Like(SCFNrRegR )
010943160129     D Sa2SerRegR      S                   Like(SCFSerRegR)
010944160129      *
010945151120     D SavImpSogl      S                   Like(BLSImpSogl)
010946151202     D AnnoSoglia      S                   Like(Anno0508)
010947151119      *
011000110721     D UDateISO        S               D
011100131014     D $FineSCF        S              1A
011101151204     D $FineSCFAGG     S              1A
011102151204     D $OkSoglia       S              1A
011103160201     D $OkSCF          S              1A   Inz(*On)
011500110721     D $PrimaStp1      S              1    INZ(*On)
011501151204     D $PrimoAgg       S              1A   INZ(*On)
011600131014     D $TpAnn          S                   Like(SCFTpAnn) INZ(*Blank)
011700110721     D $Data           S               D
011701151120     D $RicAna         S              1A
011800110721     D WSogg           S                   Like(DVASogg)
011900110721     D WKsc            S                   Like(DVACodice)
012000131014     D WKcc            S                   Like(SCFKcc)
012100110721     D WTpInd          S                   Like(DVATpInd)
012200110721     D WCdInd          S                   Like(DVACdInd)
012300110721     D WSNatura        S                   Like(DVASNatura)
012800110721     D*---------
012900110721     D* dati conto
013000110721     D ND002DS       E DS                  INZ
013100111020     D*---------
013200111020     D DataDs          DS
013300111020     D  AnnoDs                        4A
013400111020     D   Tratt1                       1A   Inz('-')
013500111020     D  MeseDs                        2A
013600111020     D   Tratt2                       1A   Inz('-')
013700111020     D  GiornoDs                      2A
014400110721     D*---------
014500110721     D* Dati soggetti
014600110721     D DVASog        E DS
014700110721     D*---------
014800110721     D* driver soggetti
014900110721     D DVARic          S              1
015000110721     D DVASocieta      S              3
015100110721     D DVAUnita        S              8
015200110721     D DVADtRif        S               D
015300110721     D DVAStrutt       S             10
015400110721     D DVALenOut       S              9B 0
015500110721     D DVASnatura      S              1
015600110721     D DVACodice       S              8
015700110721     D DVALineav       S              3
015800110721     D DVAFiliale      S              3  0
015900110721     D DVASogg         S              8
016000110721     D DVATpInd        S              2
016100110721     D DVACdInd        S              3
016200110721     D DVAOutput       S           4000
016300110721     D DVAErrore       S              1
016400110721     D*---------
016500131014     D* per prtf SDGSG52P1 - errori
016600110721     D $TpErr1         S              2A   Inz
016700110721     D*-------------
017200110831     D $PrimaVolta     S              1A
017202151201     D*-------------
017209151201     D* Sottotitolo di testata Black List
017210151201     D* "Black List"
017211151202     D SOTTITTB        C                   CONST('SDGMSG    *LIBL-
017212151201     D                                          SDC0076')
018300110721     C*************************************************************
018400110721     C*
018500110721     C                   EXSR      INZVAR
018600110721     C*
018700110721     C                   CALLB     'XSTRCMT'
018800160129     C                   if        NOT %open (SDGSCF02L)
018900160129     C                   OPEN      SDGSCF02L
019000110826     C                   endif
019100110721     C*
019200131014     C* Leggo i soli movimenti Attivi (non annullati),
019300151118     C* Automatici.
019700151120     C* A rottura di dati anagrafici calcolo i totali in
019800151118     C* base alla soglia
020100151120     C                   EXSR      RepAna
020400110721     C*
020500110721     C                   EXSR      ENDPGM
020600110721     C/EJECT
020700110722     C************************************************************
020800151120     C* Reperimento per dati anagrafici
020900110722     C************************************************************
021000151120     C     RepAna        BEGSR
021004131014     C*
021005160129     C                   Clear                   Sa2CogRagR
021006160129     C                   Clear                   Sa2NomeR
021007160129     C                   Clear                   Sa2DtNasR
021008160129     C                   Clear                   Sa2LocNasR
021009160129     C                   Clear                   Sa2PrvNasR
021010160129     C                   Clear                   Sa2StatoR
021011160129     C                   Clear                   Sa2CodUICR
021012160129     C                   Clear                   Sa2LocaliR
021013160129     C                   Clear                   Sa2IndireR
021014160129     C                   Clear                   Sa2TpNR
021015160129     C                   Clear                   Sa2PIVAR
021016160129     C                   Clear                   Sa2CdFiscR
021017160129     C                   Clear                   Sa2TpOpeR
021018160129     C                   Clear                   Sa2NatOpeR
021019160129     C                   Clear                   Sa2LeasinR
021020160129     C                   Clear                   Sa2DtRegR
021021160129     C                   Clear                   Sa2NrRegR
021022160129     C                   Clear                   Sa2SerRegR
021023131014     C*
021024160201     C* azzero i totali per dati anagrafici
021025131014     C                   Z-Add     0             $Totale
021026160201     C                   Z-Add     0             $ImpSoglia
021027160201     C                   Z-Add     0             $IVASoglia
021028131014     C*
021029131014     C                   Eval      $FoundSCF   = *Off
021100110722     C*
021200110722     C                   EXSR      SETANA
021300110722     C                   EXSR      REDANA
021400110722     C*
021500131014     C                   DOW       $FineSCF    = *Off
021600110722     C*
021900110818     C                   EXSR      GesRighe
022000110722     C*
022100110722     C                   EXSR      REDANA
022200110722     C                   ENDDO
022201131014     C*
022202151120     C                   If        $FoundSCF   = *On
022204131014     C                   EXSR      CtrSoglia
022205131014     C                   endif
022300110722     C*
022400110722     C                   ENDSR
022500110722     C/EJECT
022600110722     C************************************************************
022700110722     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
022800110722     C************************************************************
022900110722     C     SETANA        BEGSR
023000110722     C*
023001151119     C                   Eval      SCFSocieta  = Soc0508
023002151118     C                   Eval      SCFTpCom    = TpCom0508
023003151118     C                   Eval      SCFAnno     = Anno0508
023004151118     C                   Eval      SCFMese     = Mese0508
023005160129     C                   Eval      SCFQuadro   = 'BL'
023006160201     C                   Eval      SCFSeqQua   = 03
023007160129     C                   Eval      SCFCogRagR  = CogRaR0508
023008160129     C                   Eval      SCFNomeR    = NomeR0508
023009160129     C                   Eval      SCFDtNasR   = DtNasR0508
023010160129     C                   Eval      SCFLocNasR  = LocNaR0508
023011160129     C                   Eval      SCFPrvNasR  = PrvNaR0508
023012160129     C                   Eval      SCFStatoR   = StatoR0508
023013160129     C                   Eval      SCFCodUICR  = CodUIR0508
023014160129     C                   Eval      SCFLocaliR  = LocalR0508
023015160129     C                   Eval      SCFIndireR  = IndiER0508
023016160129     C                   Eval      SCFTpNR     = TpNR0508
023017160129     C                   Eval      SCFPIVAR    = PIVAR0508
023018160129     C                   Eval      SCFCdFiscR  = CdFisR0508
023019160129     C                   Eval      SCFTpOpeR   = TpOpeR0508
023020160129     C                   Eval      SCFNatOpeR  = NatOpR0508
023021160129     C                   Eval      SCFLeasinR  = LeasiR0508
023022160129     C                   Eval      SCFDtRegR   = DtRegR0508
023023160129     C                   Eval      SCFNrRegR   = NrRegR0508
023024160129     C                   Eval      SCFSerRegR  = SerReR0508
023025151120     C*
023026151202     C                   If        $RicAna = *on
023027160129     C     K24SCF02      SETLL     SDGSCF02L                              21
023028151202     C                   Else
023029160129     C     K04SCF02      SETLL     SDGSCF02L                              21
023030151202     C                   Endif
030700110722     C*
030800110722     C                   ENDSR
030900110722     C/EJECT
031000110722     C************************************************************
031100110722     C* LETTURA RCD ARCHIVIO PILOTA
031200110722     C************************************************************
031300110722     C     REDANA        BEGSR
031400110722     C*
031401151120     C                   If        $RicAna = *on
031402160129     C     K24SCF02      READE(N)  SDGSCF02L                              21
031403151120     C                   Else
031404160129     C     K04SCF02      READE(N)  SDGSCF02L                              21
031405151120     C                   Endif
031406131014     C                   Eval      $FineSCF    = *In21
033600110722     C*
033700110722     C                   ENDSR
033701151204     C/EJECT
033702151204     C************************************************************
033703151204     C* LETTURA RCD ARCHIVIO PILOTA PER AGGIORNAMENTO
033704151204     C************************************************************
033705151204     C     REDANAAgg     BEGSR
033706151204     C*
033708160129     C     K24SCF02      READE     SDGSCF02L                              21
033712151204     C                   Eval      $FineSCFAGg = *In21
033713151204     C*
033714151204     C                   ENDSR
033800110722     C/EJECT
033801131014     C************************************************************
033802131014     C* LETTURA RCD ARCHIVIO PILOTA
033803131014     C************************************************************
033804131014     C     GesRighe      BEGSR
033805151204     C*
033806151204     C                   if        $FoundSCF   = *On
033807151204     C                   EXSR      CtrSoglia
033808151204     C                   endif
033832131014     C*
033835160129     C                   if        (SCFCogRagR <> SavCogRagR or
033836160129     C                              SCFNomeR   <> SavNomeR   or
033837160129     C                              SCFDtNasR  <> SavDtNasR  or
033838160129     C                              SCFLocNasR <> SavLocNasR or
033839160129     C                              SCFPrvNasR <> SavPrvNasR or
033840160129     C                              SCFStatoR  <> SavStatoR  or
033841160129     C                              SCFCodUICR <> SavCodUICR or
033842160129     C                              SCFLocaliR <> SavLocaliR or
033843160129     C                              SCFIndireR <> SavIndireR or
033844160129     C                              SCFTpNR    <> SavTpNR    or
033845160129     C                              SCFPIVAR   <> SavPIVAR   or
033846160129     C                              SCFCdFiscR <> SavCdFiscR or
033847160129     C                              SCFTpOpeR  <> SavTpOpeR  or
033848160129     C                              SCFNatOpeR <> SavNatOpeR or
033849160129     C                              SCFLeasinR <> SavLeasinR or
033850160129     C                              SCFDtRegR  <> SavDtRegR  or
033851160129     C                              SCFNrRegR  <> SavNrRegR  or
033852160129     C                              SCFSerRegR <> SavSerRegR )
033853151119     C*
033854131014     C                   Eval      $FoundSCF   = *Off
033855151119     C*
033856160129     C                   Eval      SavCogRagR  = SCFCogRagR
033857160129     C                   Eval      SavNomeR    = SCFNomeR
033858160129     C                   Eval      SavDtNasR   = SCFDtNasR
033859160129     C                   Eval      SavLocNasR  = SCFLocNasR
033860160129     C                   Eval      SavPrvNasR  = SCFPrvNasR
033861160129     C                   Eval      SavStatoR   = SCFStatoR
033862160129     C                   Eval      SavCodUICR  = SCFCodUICR
033863160129     C                   Eval      SavLocaliR  = SCFLocaliR
033864160129     C                   Eval      SavIndireR  = SCFIndireR
033865160129     C                   Eval      SavTpNR     = SCFTpNR
033866160129     C                   Eval      SavPIVAR    = SCFPIVAR
033867160129     C                   Eval      SavCdFiscR  = SCFCdFiscR
033868160129     C                   Eval      SavTpOpeR   = SCFTpOpeR
033869160129     C                   Eval      SavNatOpeR  = SCFNatOpeR
033870160129     C                   Eval      SavLeasinR  = SCFLeasinR
033871160129     C                   Eval      SavDtRegR   = SCFDtRegR
033872160129     C                   Eval      SavNrRegR   = SCFNrRegR
033873160129     C                   Eval      SavSerRegR  = SCFSerRegR
033874131014     C                   endif
033875131014     C*
033876160201     C***********        EXSR      SELANA
033877151119     C*
033878131014     C                   if        $OkSCF      = *On
033879131014     C*
033880131014     C                   Eval      $FoundSCF   = *On
033881151203     C* sommo gli importi relativi ai medesimi dati anagrafici
033882131014     C                   Add       SCFImpOpe     $Totale
033883131014     C                   Add       SCFIVAOpe     $Totale
033884160201     C*
033885160201     C                   Add       SCFImpOpe     $ImpSoglia
033886160201     C                   Add       SCFIVAOpe     $IVASoglia
033887131014     C*
033888131014     C                   endif
033889131014     C*
033890131014     C                   ENDSR
033891131014     C/EJECT
035500110721     C************************************************************
035600131014     C* Controllo soglia
035700110721     C************************************************************
035800131014     C     CtrSoglia     BEGSR
035801131014     C*
035802151204     C                   Movel     *off          $okSoglia
035803151203     C* controllo soglia a cambio di dati anagrafici confermo o meno
035804131014     C* l'annullamento per mancato raggiungimento soglia
035805151204     C                   if        $FineScf  = *on           or
035806160129     C                             ((SavCogRagR <> *blanks    or
035807160129     C                              SavNomeR   <> *blanks    or
035808160129     C                              SavDtNasR  <> *loval     or
035809160129     C                              SavLocNasR <> *blanks    or
035810160129     C                              SavPrvNasR <> *blanks    or
035811160129     C                              SavStatoR  <> *blanks    or
035812160129     C                              SavCodUICR <> *zeros     or
035813160129     C                              SavLocaliR <> *blanks    or
035814160129     C                              SavIndireR <> *blanks    or
035815160129     C                              SavTpNR    <> *blanks    or
035816160129     C                              SavPIVAR   <> *blanks    or
035817160129     C                              SavCdFiscR <> *blanks    or
035818160129     C                              SavTpOpeR  <> *blanks    or
035819160129     C                              SavNatOpeR <> *blanks    or
035820160129     C                              SavLeasinR <> *blanks    or
035821160129     C                              SavDtRegR  <> *loval     or
035822160129     C                              SavNrRegR  <> *Zero      or
035823160129     C                              SavSerRegR <> *blanks      ) and
035824160129     C                             (SCFCogRagR <> SavCogRagR or
035825160129     C                              SCFNomeR   <> SavNomeR   or
035826160129     C                              SCFDtNasR  <> SavDtNasR  or
035827160129     C                              SCFLocNasR <> SavLocNasR or
035828160129     C                              SCFPrvNasR <> SavPrvNasR or
035829160129     C                              SCFStatoR  <> SavStatoR  or
035830160129     C                              SCFCodUICR <> SavCodUICR or
035831160129     C                              SCFLocaliR <> SavLocaliR or
035832160129     C                              SCFIndireR <> SavIndireR or
035833160129     C                              SCFTpNR    <> SavTpNR    or
035834160129     C                              SCFPIVAR   <> SavPIVAR   or
035835160129     C                              SCFCdFiscR <> SavCdFiscR or
035836160129     C                              SCFTpOpeR  <> SavTpOpeR  or
035837160129     C                              SCFNatOpeR <> SavNatOpeR or
035838160129     C                              SCFLeasinR <> SavLeasinR or
035839160129     C                              SCFDtRegR  <> SavDtRegR  or
035840160129     C                              SCFNrRegR  <> SavNrRegR  or
035841160129     C                              SCFSerRegR <> SavSerRegR   ))
035842131014     C*
035843151119     C* salvo i riferimenti anagrafici               , serviranno
035844131014     C* per riposizionarmi dopo la conferma o meno dell'annullamento
035845160129     C                   Eval       Sa2CogRagR  =  SCFCogRagR
035846160129     C                   Eval       Sa2NomeR    =  SCFNomeR
035847160129     C                   Eval       Sa2DtNasR   =  SCFDtNasR
035848160129     C                   Eval       Sa2LocNasR  =  SCFLocNasR
035849160129     C                   Eval       Sa2PrvNasR  =  SCFPrvNasR
035850160129     C                   Eval       Sa2StatoR   =  SCFStatoR
035851160129     C                   Eval       Sa2CodUICR  =  SCFCodUICR
035852160129     C                   Eval       Sa2LocaliR  =  SCFLocaliR
035853160129     C                   Eval       Sa2IndireR  =  SCFIndireR
035854160129     C                   Eval       Sa2TpNR     =  SCFTpNR
035855160129     C                   Eval       Sa2PIVAR    =  SCFPIVAR
035856160129     C                   Eval       Sa2CdFiscR  =  SCFCdFiscR
035857160129     C                   Eval       Sa2TpOpeR   =  SCFTpOpeR
035858160129     C                   Eval       Sa2NatOpeR  =  SCFNatOpeR
035859160129     C                   Eval       Sa2LeasinR  =  SCFLeasinR
035860160129     C                   Eval       Sa2DtRegR   =  SCFDtRegR
035861160129     C                   Eval       Sa2NrRegR   =  SCFNrRegR
035862160129     C                   Eval       Sa2SerRegR  =  SCFSerRegR
036800110817     C*
036802151120     C                   if        $Totale    >= SavImpSogl
036803151203     C* Devo Pulire il campi dell'errore se c'era
036804151203     C                   Movel     *on           $OkSoglia
036806131014     C                   else
036807151203     C                   Movel     *off          $OkSoglia
036808151203     C                   Endif
036817151204     C* Gestisco gli aggiornamenti e stampo l'eventuale errore
036818151204     C                   Exsr      GesAgg
036819151204     C*
036820151204     C* Registro le modifiche effettuate
036821151204     C                   COMMIT
036822160129     C                   FEOD      SDGSCF02L
036823151204     C* Mi riposiziono solo se non sono a fine file
036824151204     C                   If        $FineSCF = *off
036825151204     C* mi riposiziono sulla prima riga della nuova anagrafica
036826151204     C* Serve anche per attivare il commit già dalla prima riga
036827160129     C                   Eval       SCFCogRagR = Sa2CogRagR
036828160129     C                   Eval       SCFNomeR   = Sa2NomeR
036829160129     C                   Eval       SCFDtNasR  = Sa2DtNasR
036830160129     C                   Eval       SCFLocNasR = Sa2LocNasR
036831160129     C                   Eval       SCFPrvNasR = Sa2PrvNasR
036832160129     C                   Eval       SCFStatoR  = Sa2StatoR
036833160129     C                   Eval       SCFCodUICR = Sa2CodUICR
036834160129     C                   Eval       SCFLocaliR = Sa2LocaliR
036835160129     C                   Eval       SCFIndireR = Sa2IndireR
036836160129     C                   Eval       SCFTpNR    = Sa2TpNR
036837160129     C                   Eval       SCFPIVAR   = Sa2PIVAR
036838160129     C                   Eval       SCFCdFiscR = Sa2CdFiscR
036839160129     C                   Eval       SCFTpOpeR  = Sa2TpOpeR
036840160129     C                   Eval       SCFNatOpeR = Sa2NatOpeR
036841160129     C                   Eval       SCFLeasinR = Sa2LeasinR
036842160129     C                   Eval       SCFDtRegR  = Sa2DtRegR
036843160129     C                   Eval       SCFNrRegR  = Sa2NrRegR
036844160129     C                   Eval       SCFSerRegR = Sa2SerRegR
036845151204     C*
036846160129     C     K24SCF02      SETLL     SDGSCF02L                              21
036847160129     C     K24SCF02      READE(N)  SDGSCF02L                              21
036848151204     C*
036849160201     C* azzero i totali per la anagrafica successiva
036850151204     C                   Z-Add     0             $Totale
036851160201     C                   Z-Add     0             $ImpSoglia
036852160201     C                   Z-Add     0             $IVASoglia
036853151204     C*
036854151204     C                   EndIf
036855151204     C*
036856151204     C                   EndIf
036862131014     C*
036900110817     C                   ENDSR
036901151204     C/EJECT
036902151204     C************************************************************
036903151204     C* Aggiornamento Archivio
036904151204     C************************************************************
036905151204     C     GesAgg        BEGSR
036909151204     C*
036911151204     C* mi riposiziono per effettuare l'aggiornamento
036914151204     C*
036915151204     C                   Movel     *on           $PrimoAgg
036916151204     C*
036917160129     C                   Eval      SCFCogRagR =  SavCogRagR
036918160129     C                   Eval      SCFNomeR   =  SavNomeR
036919160129     C                   Eval      SCFDtNasR  =  SavDtNasR
036920160129     C                   Eval      SCFLocNasR =  SavLocNasR
036921160129     C                   Eval      SCFPrvNasR =  SavPrvNasR
036922160129     C                   Eval      SCFStatoR  =  SavStatoR
036923160129     C                   Eval      SCFCodUICR =  SavCodUICR
036924160129     C                   Eval      SCFLocaliR =  SavLocaliR
036925160129     C                   Eval      SCFIndireR =  SavIndireR
036926160129     C                   Eval      SCFTpNR    =  SavTpNR
036927160129     C                   Eval      SCFPIVAR   =  SavPIVAR
036928160129     C                   Eval      SCFCdFiscR =  SavCdFiscR
036929160129     C                   Eval      SCFTpOpeR  =  SavTpOpeR
036930160129     C                   Eval      SCFNatOpeR =  SavNatOpeR
036931160129     C                   Eval      SCFLeasinR =  SavLeasinR
036932160129     C                   Eval      SCFDtRegR  =  SavDtRegR
036933160129     C                   Eval      SCFNrRegR  =  SavNrRegR
036934160129     C                   Eval      SCFSerRegR =  SavSerRegR
036935151204     C*
036936160129     C     K24SCF02      SETLL     SDGSCF02l
036937151204     C*
036938151204     C                   EXSR      REDANAAGG
036939151204     C*
036940151204     C                   DOW       $FineSCFAgg = *Off
036941160201     C*
036942160201     C                   Eval      SCFImpSogl  = $ImpSoglia
036943160201     C                   Eval      SCFIVASogl  = $IVASoglia
036944160201     C*
036945160201     C                   if        SCFTpAnn    = *Blank   or
036946160201     C                             SCFTpAnn    = '54'
036947151204     C*
036948151204     C                   If        $OkSoglia = *Off
036949151204     C* Soglia non raggiunta
036950151204     C                   Eval      SCFAnn      = '1'
036951151209     C                   Eval      SCFTpAnn    = '54'
036952151204     C                   EXSR      UPDSCF
036953151209     C* Stampo l'errore se richiesto
036954151209     C                   If        StpAnn0508 = *on
036955151204     C                   Eval      $TpAnn      = ScfTpAnn
036956151204     C                   EXSR      StpErr1
036957151209     C                   Endif
036958151204     C                   Else
036959151204     C* Soglia raggiunta
036960151204     C                   Eval      SCFAnn      = ' '
036961151204     C                   Eval      SCFTpAnn    = '  '
036962151204     C                   EXSR      UPDSCF
036963151204     C*
036964151204     C                   Endif
036967160201     C*
036968160201     C                   Else
036969160201     C*
036970160201     C* Aggiorno solo gli importi soglia
036972160201     C                   EXSR      UPDSCF
036973160201     C*
036974160201     C                   Endif
036975151204     C*
036976151204     C                   Movel     *off          $PrimoAgg
036977151204     C*
036978151204     C                   EXSR      REDANAAGG
036979151204     C*
036980151204     C                   ENDDO
036981151204     C*
036982151204     C                   ENDSR
037000110817     C/EJECT
037001131014     C************************************************************
037002131014     C* SELEZIONI SULLA RIGA LETTA
037003131014     C************************************************************
037004131014     C     SELANA        BEGSR
037005131014     C*
037006131014     C                   MOVEL     *ON           $OkSCF
037007131014     C*
037008131014     C                   SELECT
037009131014     C*
037010151120     C                   When      SCFAnn      = *On and
037011151209     C                             SCFTpAnn    <> '54'
037012131014     C                   Eval      $OkSCF      = *Off
037056131014     C*
037057131014     C                   ENDSL
037058131014     C*
037059131014     C                   ENDSR
037060131014     C/EJECT
037061131014     C************************************************************
037062131014     C* Aggiornamento RIGA LETTA
037063131014     C************************************************************
037064131014     C     UpdSCF        BEGSR
037071131014     C*
037072131014     C                   UPDATE    SDGSCF000
037073131014     C*
037074131014     C                   ENDSR
037075131014     C/EJECT
115400110721     C************************************************************
115500110721     C* INIZIALIZZAZIONE VARIABILI
115600110721     C************************************************************
115700110721     C     INZVAR        BEGSR
115800110721     C*
116000151118     C                   Eval      Err0508     = *Off
116001151120     C                   Eval      $RicAna     = *Off
116002151120     C*
116003160129     C                   if         CogRaR0508 <> *blanks    or
116004160129     C                              NomeR0508  <> *blanks    or
116005160129     C                              DtNasR0508 <> *loval     or
116006160129     C                              LocNaR0508 <> *blanks    or
116007160129     C                              PrvNaR0508 <> *blanks    or
116008160129     C                              StatoR0508 <> *blanks    or
116009160129     C                              CodUIR0508 <> *zeros     or
116010160129     C                              LocalR0508 <> *blanks    or
116011160129     C                              IndiER0508 <> *blanks    or
116012160129     C                              TpNR0508   <> *blanks    or
116013160129     C                              PIVAR0508  <> *blanks    or
116014160129     C                              CdFisR0508 <> *blanks    or
116015160129     C                              TpOpeR0508 <> *blanks    or
116016160129     C                              NatOpR0508 <> *blanks    or
116017160129     C                              LeasiR0508 <> *blanks    or
116018160129     C                              DtRegR0508 <> *loval     or
116019160129     C                              NrRegR0508 <> *Zero      or
116020160129     C                              SerReR0508 <> *blanks
116021151120     C                   Movel     *On           $RicAna
116022151120     C                   Endif
116100110721     C*
116200110721     C* Reperimento dati società
116300151118     C                   MoveL     Soc0508       SOCXSC
116400110721     C                   EXSR      REPSOC
116500110721     C*
116600151118     C                   if        Anno0508    = 0
116700151118     C                   Eval      Err0508     = '2'
116800110721     C                   EXSR      ENDPGM
116900110721     C                   endif
117000151120     C* Reperisco la soglia
117001151120     C                   Exsr      RepSoglia
117002160104     C*
117003160104     C                   if        SavImpSogl  = 0
117005160104     C                   EXSR      ENDPGM
117006160104     C                   endif
117007160104     C*
117100110721     C* Se non è già aperto, apro il printer file e faccio OVR
117200110721     C                   EXSR      OpnPrtf
117300110721     C*
117400110721     C                   ENDSR
117500110721     C/EJECT
117600110721     C************************************************************
117700110721     C* REPERIMENTO DATI SOCIETA'
117800110721     C************************************************************
117900110721     C     REPSOC        BEGSR
118000110721     C*
118100110721     C                   MoveL     'SOC001'      TIPXSC
118200110721     C                   CALL      'SDGSOCDR'
118300110721     C                   PARM                    TIPXSC            6
118400110721     C                   PARM                    SOCXSC            3
118500110721     C                   PARM                    CDSXSC            9 0
118600110721     C                   PARM                    MODXSC            3
118700110721     C                   PARM      *BLANKS       RTNXSC            1
118800110721     C                   PARM                    XSOCDS
118900110721     C                   PARM                    KPJBA
119000110721     C*
119100110721     C     RTNXSC        IFNE      '1'
119200110721     C                   MoveL     XSOCDS        SOC001
119300110721     C                   else
119400151118     C                   Eval      Err0508     = '1'
119500110721     C                   EXSR      ENDPGM
119600110721     C                   endif
119700110721     C*
119800110721     C                   ENDSR
119801151120     C/EJECT
119802151120     C************************************************************
119803151120     C* REPERIMENTO SOGLIA
119804151120     C************************************************************
119805151120     C     REPSOGLIA     BEGSR
119810151120     C*
119811151120     C                   Clear                   SavImpSogl
119812151120     C*
119813151120     C                   Movel     Soc0508       BLSSOCIETA
119814151120     C                   Movel     *hival        BLSDTINI
119815151120     C*
119816151120     C     K02BLS01      Setgt     SDGBLS01L
119817151120     C*
119818151120     C                   Do        *hival
119819151120     C     BLSSOCIETA    Readpe    SDGBLS01L
119820151120     C                   If        %eof(SDGBLS01L)
119821151120     C                   Leave
119822151120     C                   Endif
119823151120     C*
119824151120     C                   extrct    BlsDtIni:*Y   AnnoSoglia
119825151120     C*
119826151120     C                   If        AnnoSoglia = Anno0508
119827151120     C                   Eval      SavImpSogl = BlsImpSogl
119828151120     C                   Leave
119829151120     C                   Endif
119830151120     C*
119831151120     C                   EndDO
119834151120     C*
119835151120     C                   ENDSR
119900110721     C/EJECT
120000110721     C************************************************************
120100110721     C* Apertura file si stampa
120200110721     C************************************************************
120300110721     C     OpnPrtf       BEGSR
120400110721     C*
120500110721     C* imposto il nome del pgm da mettere in stampa
120600131014     C                   MOVEL     'SDG00500R'   NOMPGM
120700110721     C*-------------------
120800110721     C* valorizzo i dati di testata comuni a tutte le stampe
120900110721     C                   MoveL     KNSIF         NOMSIF
121000110721     C                   MoveL     XSCDSI        NOMDIT
121200110721     C                   CALL      'XNETA'                              29
121300110721     C                   PARM                    NOMSYS
121301151202     C* Recupero il titolo con il tipo di comunicazione
121302151202     C                   MOVEL     *BLANK        SOTTITT
121303151202     C*
121304151202     C                   MOVEL     SOTTITTB      IDMSG
121305151202     C*
121306151202     C                   CALLB     'XRTVM'
121307151202     C                   PARM                    IDMSG            27
121308151202     C                   PARM                    TXTMSG          100
121309151202     C                   PARM                    ERRMSG            1
121310151202     C                   PARM      *ON           CTRMSG            1            centratura
121311151202     C                   PARM      30            LENMSG            3 0          lun output.
121312151202     C     ERRMSG        IFNE      *ON
121313151202     C                   MOVEL     TXTMSG        SOTTITT
121314151202     C                   ELSE
121315151202     C                   MOVEL     *ALL'?'       SOTTITT
121316151202     C                   ENDIF
121317151202     C*
121400110721     C*
121500110721     C                   EXSR      RieParam
121600110721     C*
121700131014     C                   if        NOT %open(SDGSG52P1)
121800110721     C*
121900110721     C* salvo la kpjbu per ripristinarla dopo il richiamo
122000110721     C                   MoveL     KPJBU         SavKPJBU
122100110721     C*
122200110721     C* eseguo ovrprtf in *Share
122300131014     C                   Reset                   SDG00500DS
122400131014     C                   Eval      KPJBU       = SDG00500DS
122500110721     C*
122600131014     C                   CallB     'SDG00500R'
122700110721     C                   PARM                    KPJBA
122800110721     C*
122900131014     C                   MoveL     KPJBU         SDG00500DS
123000110721     C*
123100131014     C                   if        Esito0500   = *On
123200110721     C*
123300110721     C* Fallito l'ovrprtf del file di spool
123400110721     C* apro il prtf per fare la stampa dell'errore
123500131014     C                   OPEN      SDGSG52P1
123600110721     C*
123700110721     C                   else
123800131014     C                   OPEN      SDGSG52P1
123900110721     C                   endif
124000110721     C*
124100110721     C* ripristino la kpjbu per non "turbare" i richiami successivi
124200110721     C* ad eventuali altri pgm che ricevono la kpjba
124300110721     C                   MoveL     SavKPJBU      KPJBU
124400110721     C*
124500110721     C                   endif
124600110721     C*
124700110721     C                   EndSr
124800110721     C/EJECT
124900110721     C************************************************************
125000110721     C* FINE PROGRAMMA
125100110721     C************************************************************
125200110721     C     ENDPGM        BEGSR
125300110721     C*
125301160129     C                   if        SavImpSogl <> 0
125400160129     C                   if        ERR0508     =  *On
125500110721     C                   ROLBK
125600110721     C                   else
125700110721     C                   COMMIT
125800110721     C                   endif
125900110721     C*
126000160129     C                   if        %open (SDGSCF02L)
126100160129     C                   FEOD      SDGSCF02L
126200110721     C                   endif
126201160129     C                   endif
126500110721     C*
126600151118     C                   if        Lr0508 = *on
126700131014     C                   if        %open (SDGSG52P1)
126800131014     C                   Close     SDGSG52P1
126900110721     C                   endif
127000110721     C                   SetOn                                        LR
127100110721     C                   else
127200110721     C                   SetOn                                        RT
127300110721     C                   endif
127400110721     C                   Return
127500110721     C*
127600110721     C                   ENDSR
127700110721     C/EJECT
127800110721     C************************************************************
127900110721     C* Imposto i parametri di elaborazione
128000110721     C************************************************************
128100110721     C     RieParam      BEGSR
128101160201     C*
128102160201     C                   Select
128103160201     C* Acquisti da San Marino
128104160201     C                   When      TpCom0508   = 'S'
128105160201     C                   Eval      *In11       = *On
128106160201     C                   Other
128107160201     C* Spesometro  o
128108160201     C* Acquisti da San Marino
128109160201     C                   Eval      *In11       = *Off
128110160201     C                   endSl
128111160201     C*
128200110721     C*
128300151118     C                   Eval      PACtb       = Ctb0508
128400110721     C                   EXSR      RepCtbD
128401151201     C*
128600151118     C                   Eval      PATpRegZ    = TpRegz0508
128700110721     C                   EXSR      RepTpRegzD
128800110721     C*
128900151118     C                   Eval      PAAnno      = Anno0508
128902160201     C                   Eval      PAMese      = Mese0508
128903160201     C*
128904160201     C                   Eval      PAStato     = Stato0508
128905160201     C                   EXSR      RepStatoD
129600110721     C*
129700151118     C                   Eval      PACF        = CF0508
129800110721     C                   EXSR      RepCFD
129900110721     C*
130000160129     C                   Eval      PAPartIVA   = PIVAR0508
130100160129     C                   Eval      PACdFisc    = CdFisR0508
130200110721     C*
130300151118     C                   Eval      PASogg      = Sogg0508
130400110721     C                   EXSR      RepSoggD
130500110721     C*
130600151118     C                   Eval      PAKcc1      = Kcc10508
130700151118     C                   Eval      PAKcc2      = Kcc20508
130800151118     C                   Eval      PAKcc3      = Kcc30508
130900151118     C                   Eval      PAKcc4      = Kcc40508
131000151118     C                   Eval      PAKcc5      = Kcc50508
131100151118     C                   Eval      PAKcc6      = Kcc60508
131200110721     C*
131300151118     C                   Eval      PAKsc       = Ksc0508
131400110721     C                   EXSR      RepKscD
131900110721     C*
132000110721     C                   Eval      PAOn1       = XScOn
132100110721     C                   Eval      PAOff1      = XScOff
132200151118     C                   if        DelMan0508  = *On
132300110721     C                   Eval      PADelManu   = XScOn
132400110721     C                   else
132500110721     C                   Eval      PADelManu   = XScOff
132600110721     C                   endif
132700110721     C*
133100110721     C                   Eval      PAOn2       = XScOn
133200110721     C                   Eval      PAOff2      = XScOff
133300151118     C                   if        StpAnn0508  = *On
133400110721     C                   Eval      PAStpAnn    = XScOn
133500110721     C                   else
133600110721     C                   Eval      PAStpAnn    = XScOff
133700110721     C                   endif
133800110721     C*
133900110721     C                   ENDSR
134000110721     C/EJECT
134100110721     C************************************************************
134200110721     C* Reperisco la descrizione della contabilità
134300110721     C************************************************************
134400110721     C     RepCtbD       BEGSR
134500110721     C*
134600110721     C                   Clear                   SDGTABDS
134700110721     C*
134800110721     C                   Eval      XTbRic     =  '1'
134900110721     C                   Eval      XTbAzi     =  XScSoc
135000110721     C                   Eval      XTbKey     =  *Zero
135100110721     C                   Eval      XTbCod     =  'G84'
135200151118     C                   Move      Ctb0508       XTbKey
135300110721     C*
135400110721     C                   CALL      'SDGTABDR'
135500110721     C                   PARM                    SDGTABDS
135600110721     C*
135700110721     C                   if        XTbErr      = '0'
135800110721     C                   MoveL     XTbUni        ANGG84DS
135900110721     C                   MoveL     DsBG84        PACtbD
136000110721     C                   else
136100110721     C                   MoveL     *All'?'       PACtbD
136200110721     C                   endif
136300110721     C*
136400110721     C                   ENDSR
136500110721     C/EJECT
136600110721     C************************************************************
136700110721     C* Reperisco la descrizione del tipo registrazione
136800110721     C************************************************************
136900110721     C     RepTpRegzD    BEGSR
137000110721     C*
137100110721     C                   Clear                   X07VALDS
137200110721     C*
137300110721     C                   Move      '1'           X07Ric
137400110721     C                   MoveL     'ANDIZ000'    X07Trc
137500110721     C                   MoveL     'DSPTPREGZ '  X07Cam
137600151118     C                   MoveL     TpRegz0508    X07Val
137700110721     C*
137800110721     C                   CALLB     'X07VALR'
137900110721     C                   PARM                    X07VALDS
138000110721     C*
138100110721     C                   if        X07ERR      = '0'
138200110721     C                   MoveL     X07Des        PATpRegzD
138300110721     C                   else
138400110721     C                   MoveL     *All'?'       PATpRegzD
138500110721     C                   endif
138600110721     C*
138700110721     C                   ENDSR
138800110721     C/EJECT
138844160201     C************************************************************
138845160201     C* Reperisco la descrizione dello Stato
138846160201     C************************************************************
138847160201     C     RepStatoD     BEGSR
138848160201     C*
138849160201     C                   Clear                   SDGTABDS
138850160201     C                   Clear                   AngG12DS
138852160201     C*
138853160201     C                   Eval      XTbRic     =  '1'
138854160201     C                   Eval      XTbAzi     =  XScSoc
138855160201     C                   Eval      XTbKey     =  *Zero
138863160201     C                   Eval      XTbCod      = 'G12'
138865160201     C*
138866160201     C                   Move      Stato0508     XTbKey
138867160201     C*
138868160201     C                   CALL      'SDGTABDR'
138869160201     C                   PARM                    SDGTABDS
138870160201     C*
138881160201     C                   MoveL     XTbUni        ANGG12DS
138882160201     C                   MoveL     DesG12        PAStatoD
138884160201     C*
138885160201     C                   ENDSR
138886160201     C/EJECT
138900110721     C************************************************************
139000110721     C* Reperisco la descrizione della natura del soggetto
139100110721     C************************************************************
139200110721     C     RepCFD        BEGSR
139300110721     C*
139400110721     C                   Clear                   X07VALDS
139500110721     C*
139600151118     C                   if        CF0508     <> *Blank
139700110721     C*
139800110721     C                   Move      '1'           X07Ric
139900110721     C                   MoveL     'ANDIZ000'    X07Trc
140000110721     C                   MoveL     'DSPCLIFOR '  X07Cam
140100151118     C                   MoveL     CF0508        X07Val
140200110721     C                   Move      *On           X07All
140300110721     C                   CALLB     'X07VALR'
140400110721     C                   PARM                    X07VALDS
140500110721     C*
140600110721     C                   if        X07Err      = '0'
140700110721     C                   MoveL     X07Des        PACFD
140800110721     C                   else
140900110721     C                   MoveL     *All'?'       PACFD
141000110721     C                   endif
141100110721     C*
141200110721     C                   endif
141300110721     C*
141400110721     C                   ENDSR
141500110721     C/EJECT
141600110721     C************************************************************
141700110721     C* Reperisco la descrizione del soggetto
141800110721     C************************************************************
141900110721     C     RepSoggD      BEGSR
142000110721     C*
142100110721     C                   Clear                   PASoggD
142200110721     C*
142300151118     C                   if        Sogg0508   <> *Blank
142400110721     C*
142500151118     C                   Eval      WSogg       = Sogg0508
142600110721     C                   Eval      WKsc        = *Blank
142700110721     C                   Eval      WKcc        = *Blank
142800110721     C                   Eval      WSNatura    = *Blank
142900110721     C                   Eval      WTpInd      = *Blank
143000110721     C                   Eval      WCdInd      = *Blank
143100110721     C                   MoveL     'DVASOG    '  DVAStrutt
143200110721     C                   Eval      DVALENOUT   = %size(DvaSog)
143300110721     C*
143400110721     C                   EXSR      CallDvaSog
143500110721     C*
143600110721     C                   if        DVAErrore  <> '1'
143700110721     C                   Eval      %subst(DVASOG:1:DVALENOUT)
143800110721     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
143900110721     C*
144000110721     C                   MoveL     DVSDes        PASoggD
144100110721     C                   else
144200110721     C                   MoveL     *All'?'       PASoggD
144300110721     C                   endif
144400110721     C*
144500110721     C                   endif
144600110721     C*
144700110721     C                   ENDSR
144800110721     C/EJECT
144900110721     C************************************************************
145000110721     C* Chiamata al driver dei soggetti
145100110721     C************************************************************
145200110721     C     CallDvaSog    BEGSR
145300110721     C*
145400110721     C                   CALLB     'NDDVASOG'
145500110721     C                   PARM      '1'           DVARic
145600110721     C                   PARM      XScSoc        DVASocieta
145700110721     C                   PARM      *Blank        DVAUnita
145800110721     C                   PARM                    DVAStrutt
145900110721     C                   PARM                    DVADtRif
146000110721     C                   PARM                    DVALenOut
146100110721     C                   PARM      WSNatura      DVASNatura
146200110721     C                   PARM      WKsc          DVACodice
146300110721     C                   PARM      *Blank        DVALineaV
146400110721     C                   PARM      *Zero         DVAFiliale
146500110721     C                   PARM      WSogg         DVASogg
146600110721     C                   PARM      WTpInd        DVATpInd
146700110721     C                   PARM      WCdInd        DVACdInd
146800110721     C                   PARM      *Off          DVAErrore
146900110721     C                   PARM                    DVAOutPut
147000110721     C*
147100110721     C                   ENDSR
147200110721     C/EJECT
147300110721     C************************************************************
147400110721     C* Reperisco la descrizione del conto
147500110721     C************************************************************
147600110721     C     RepKscD       BEGSR
147700110721     C*
147800110721     C                   Clear                   PAKscD
147900110721     C*
148000151118     C                   if        Kcc0508    <> *Blank   or
148100151118     C                             Ksc0508    <> *Blank
148200110721     C*
148300151118     C                   Eval      WKcc        = Kcc0508
148400151118     C                   Eval      WKsc        = Ksc0508
148500110721     C                   Eval      $Data       = UDateIso
148600110721     C*
148700110721     C                   EXSR      CallMVC002
148800110721     C*
148900110721     C                   if        $Esito     <> *On
149000110721     C                   MoveL     DesBrev002    PAKscD
149100110721     C                   else
149200110721     C                   MoveL     *All'?'       PAKscD
149300110721     C                   endif
149400110721     C*
149500110721     C                   endif
149600110721     C*
149700110721     C                   ENDSR
149800110721     C/EJECT
149900110721     C***********************************************************
150000110721     C* Driver per controllo conto
150100110721     C***********************************************************
150200110721     C     CallMvc002    BEGSR
150300110721     C*
150400110721     C                   Clear                   ND002DS
150500110721     C*
150600110721     C                   Eval      $LenOut    =  %Size(ND002DS)
150700110721     C*
150800110721     C                   CallB     'NDMVC002'
150900110721     C                   PARM                    XScSoc
151000110721     C                   PARM                    WKcc
151100110721     C                   PARM                    WKsc
151200110721     C                   PARM                    $Data
151300110721     C                   PARM      *Off          $GesMsg
151400110721     C                   PARM      *Off          $Esito
151500110721     C                   PARM      'ND002DS   '  $Struttura
151600110721     C                   PARM                    ND002DS
151700110721     C                   PARM                    $LenOut
151800110721     C*
151900110721     C                   ENDSR
152000110721     C/EJECT
154600110721     C******************************************************
154700131014     C* Preparo dati 'fissi' (SDGSG52P1)     (solo I° volta)
154800110721     C******************************************************
154900110721     C     StpErr1_Inz   BEGSR
155000110721     C*
155100110721     C* Overflow
155200110721     C                   if        *In41       = *On        or
155300110721     C                             $PrimaStp1  = *On
155400110721     C* Testata
155500131014     C                   WRITE     SG52T11
155600110721     C*
155700110721     C                   if        $PrimaStp1  = *On
155701151201     C*
155800110721     C* Parametri di elaborazione
155900131014     C                   WRITE     SG52PA1
156000110721     C                   endif
156100110721     C*
156200110721     C* Intestazione colonne
156300110721     C                   if        $TpErr1     = *Blank
156400131014     C                   WRITE     SG52T21
156500110721     C                   endif
156600110721     C*
156700110721     C                   Eval      *In41       = *Off
156800110721     C                   Eval      $PrimaStp1  = *Off
156900110721     C                   endif
157000110721     C*
157100110721     C                   ENDSR
157200110721     C/EJECT
157300110721     C************************************************************
157400131014     C* Stampa errori e avvertimenti (SDGSG52P1)
157500110721     C************************************************************
157600110721     C     StpErr1       BEGSR
157700110721     C*
157800110721     C                   EXSR      StpErr1_Inz
157900110721     C*
158000110721     C                   Select
158100110721     C*
158200110721     C* Stampa riga di avvertimento per movimento annullato.
158300110721     C                   When      $TpAnn     <> *Blank
158400110721     C*
158501151204     C                   if        $primoagg = *on
158502151204     C                   Eval      DA2COgRag = SCFCogRag
158503151204     C                   Eval      DA2IMPDOC = $Totale
158504151204     C                   WRITE     SG52DA2
158505151209     C                   Endif
158506151204     C*
158507151204     C                   EXSR      StpErr1_Inz
158600110721     C                   Eval      AVVTpAnn    = $TpAnn
158700110721     C                   EXSR      RepTpAnnD
158800131014     C                   WRITE     SG52PAnn
158900110721     C                   EXSR      StpErr1_Dati
159100110721     C*
159200110721     C*  ERRORE: elaborazione interrotta. Codice società di collegamento
159300110721     C*   non valido. Codice società =
159400110721     C                   When      $TpErr1     = '02'
159500151118     C                   Eval      P102Soc     = Soc0508
159600131014     C                   WRITE     SG52P102
159700110721     C*
159800110721     C                   OTHER
159900110721     C*  ERR . Avvertimento/errore non codificato.
160000131014     C                   WRITE     SG52P1XX
160100110721     C*
160200110721     C                   endSl
160300110721     C*
160400110721     C                   ENDSR
160500110721     C/EJECT
160600110721     C******************************************************
160700110721     C* Stampo i riferimenti del movimento
160800110721     C******************************************************
160900110721     C     StpErr1_Dati  BEGSR
161000110721     C*
161100131014     C                   Clear                   SG52DA1
161200110721     C*
161300131014     C                   MoveL     SCFKcc        DA1Kcc
161400131014     C                   MoveL     SCFKsc        DA1Ksc
161500110721     C*
161600131014     C                   Eval      WKcc        = SCFKcc
161700131014     C                   Eval      WKsc        = SCFKsc
161800131014     C                   Eval      $Data       = SCFDtDoc
161900110721     C                   EXSR      CallMVC002
162000110721     C                   MoveL     Sogg002       DA1Sogg
162100110721     C*
162200131014     C                   if        SCFDtDoc   <> *Loval
162300131014     C     *JobRun       Move      SCFDtDoc      DA1DtDoc
162400110721     C                   endif
162500131014     C                   Z-Add     SCFNrDoc      DA1NrDoc
162600131014     C                   MoveL     SCFSerDoc     DA1SerDoc
163900110721     C*
164000131014     C                   Z-Add     SCFSys        DA1Sys
164100131014     C                   Z-Add     SCFNrAsReg    DA1NrAsReg
164101151204     C*
164102151204     C                   Eval      Da1ImpDoc = SCFImpOpe + SCFIVAope
164200110721     C*
164300131014     C                   WRITE     SG52DA1
164400110721     C*
164500110721     C                   ENDSR
164600110721     C/EJECT
164700110721     C************************************************************
164800110721     C* Reperisco la descrizione del tipo annullamento
164900110721     C************************************************************
165000110721     C     RepTpAnnD     BEGSR
165100110721     C*
165200110721     C                   Clear                   X07VALDS
165300110721     C*
165400110721     C                   Move      '1'           X07Ric
165500131014     C                   MoveL     'SDGSCF000 '  X07Trc
165600131014     C                   MoveL     'SCFTPANN  '  X07Cam
165700110721     C                   MoveL     $TpAnn        X07Val
165800110721     C*
165900110721     C                   CALLB     'X07VALR'
166000110721     C                   PARM                    X07VALDS
166100110721     C*
166200110721     C                   if        X07ERR      = '0'
166300110721     C                   MoveL     X07Des        AVVTpAnnD
166400110721     C                   else
166500110721     C                   MoveL     *All'?'       AVVTpAnnD
166600110721     C                   endif
166700110721     C*
166800110721     C                   ENDSR
166900110721     C/EJECT
167000110721     C************************************************************
167100110721     C* DEFINIZIONE CAMPI :
167200110721     C************************************************************
167300110721     C     *INZSR        BEGSR
167400110721     C*
167500110721     C     *ENTRY        PLIST
167600110721     C                   PARM                    KPJBA
167601151203     C                   PARM                    SDG00508DS
167700110721     C*
167800110721     C     *DMY          Move      UDate         UDateISO
167900110721     C*
168000110721     C                   ENDSR
168100110721     C/EJECT
168200110721     C************************************************************
168300110721     C* DEFINIZIONE KLIST
168400110721     C************************************************************
168500110721     C     DefKlist      BEGSR
168600110721     C*
168700110721     C* klist
168800110721     C*
168900160129     C* SDGSCF02L
169100160129     C     K04SCF02      KLIST
169101151120     C                   KFLD                    SCFSocieta
169200131014     C                   KFLD                    SCFTpCom
169300131014     C                   KFLD                    SCFAnno
169400131014     C                   KFLD                    SCFMese
177100110721     C*
177101160129     C     K24SCF02      KLIST
177102151120     C                   KFLD                    SCFSocieta
177103131014     C                   KFLD                    SCFTpCom
177104131014     C                   KFLD                    SCFAnno
177105131014     C                   KFLD                    SCFMese
177106160201     C                   KFLD                    SCFSeqQua
177107160129     C                   KFLD                    SCFQuadro
177108160129     C                   KFLD                    SCFCogRagR
177109160129     C                   KFLD                    SCFNomeR
177110160129     C                   KFLD                    SCFDtNasR
177111160129     C                   KFLD                    SCFLocNasR
177112160129     C                   KFLD                    SCFPrvNasR
177113160129     C                   KFLD                    SCFStatoR
177114160129     C                   KFLD                    SCFCodUICR
177115160129     C                   KFLD                    SCFLocaliR
177116160129     C                   KFLD                    SCFIndireR
177117160129     C                   KFLD                    SCFTpNR
177118160129     C                   KFLD                    SCFPIVAR
177119160129     C                   KFLD                    SCFCdFiscR
177120160129     C                   KFLD                    SCFTpOpeR
177121160129     C                   KFLD                    SCFNatOpeR
177122160129     C                   KFLD                    SCFLeasinR
177123160129     C                   KFLD                    SCFDtRegR
177124160129     C                   KFLD                    SCFNrRegR
177125160129     C                   KFLD                    SCFSerRegR
177126160129     C*
177127151120     C* SDGBLS01L
177128151120     C     K02BLS01      KLIST
177129151120     C                   KFLD                    BLSSocieta
177130151120     C                   KFLD                    BLSDtIni
177131131014     C*
177200110721     C                   ENDSR
177300110721     C/EJECT
