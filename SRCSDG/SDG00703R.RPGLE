000100170731     H*BIND
000200170731     H*PARMS OPTION(*NOXREF *NODEBUGIO) CVTOPT(*NONE)
000300170731     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400940316      *
000500130925      *-------------------------------------------------------------------
000600170704      *
000700170704R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
000800170704      *
000900130925      *-------------------------------------------------------------------
001000170717      * Recupera eventuale della tipologia richiesta e resistuisce se
001100170731      * - scrivere il record (WrtCST0703 = *On)
001200170731      * - inviato (IdFilO0703 <> *Blank)
001300170731      * - posizione su file inviato (PosIdF0703 <> 0)
001400170721      * - annulla Ord perchè manca l'ordinario inviato ma è stato
001500170731      *   trovato la rettifica  (ERR0703='2')
001600170717      *
001700130925      *
001800130925      *-------------------------------------------------------------------
001900091019     F*------------
002000130925     F* Spesometro - Movimenti estratti
002100170717     F* Key: TpCom/TpRec/Sys/NrAsReg      FIFO
002200170717     FSDGCST03L IF   E           K DISK
002300170717     F*------------
002400170717     F* Comunicazione Spesometro Trimestrale
002500170717     F* Key: Soc/TpCom/TpRec/Kcc/Ksc/Doc/Reg/DtAnnot
002600170717     FSDGCST04L IF   E           K DISK    RENAME(SDGCST000:SDGCST04)
002700091019     D*------------
002800091019     D* DS per il passaggio dei parametri
002900170731     D SDG00703DS    E DS
003000091019     D*-------------
003100091019     D* Passaggio Parametri
003200091019     D KPJBA         E DS
003300130925     D*-------------
003400130925     D* Campi di lavoro
003500170704     D $FineCST        S              1A
003600170717     D $ExistCST       S              1
003700170717     D $WriteCST       S              1
003800170717     D $TpRec          S                   Like(CSTTpRec)
003900170717     D SavSocieta      S                   Like(CSTSocieta)
004000170717     D SavTpCom        S                   Like(CSTTpCom  )
004100170717     D SavTpRec        S                   Like(CSTTpRec  )
004200170717     D SavSys          S                   Like(CSTSys    )
004300170717     D SavNrAsReg      S                   Like(CSTNrAsReg)
004400170717     D SavKcc          S                   Like(CSTKcc    )
004500170717     D SavKsc          S                   Like(CSTKsc    )
004600170717     D SavDtDoc        S                   Like(CSTDtDoc  )
004700170717     D SavNrDoc        S                   Like(CSTNrDoc  )
004800170717     D SavSerDoc       S                   Like(CSTSerDoc )
004900170717     D SavDtReg        S                   Like(CSTDtReg  )
005000170717     D SavNrReg        S                   Like(CSTNrReg  )
005100170717     D SavSerReg       S                   Like(CSTSerReg )
005200170717     D SavDtAnnot      S                   Like(CSTDtAnnot)
005300130925     C*----------------------------------------------------*
005400130925     C*                MAIN LINE PROGRAM
005500130925     C*----------------------------------------------------*
005600130926     C*
005700940330     C     *ENTRY        PLIST
005800091019     C                   PARM                    KPJBA
005900170731     C                   PARM                    SDG00703DS
006000130926     C*
006100130926     C*
006200130926     C* se la partita IVA e codice fiscale sono gli stessi dell'ultima
006300130926     C* chiamata, ripasso i dati recuperati in precedenza
006400170731     C                   if        Soc0703     = SavSocieta  and
006500170731     C                             TpCom0703   = SavTpCom    and
006600170731     C                             TpRec0703   = SavTpRec    and
006700170731     C                             Sys0703     = SavSys      and
006800170731     C                             NrAsRe0703  = SavNrAsReg  and
006900170731     C                             Kcc0703     = SavKcc      and
007000170731     C                             Ksc0703     = SavKsc      and
007100170731     C                             DtDoc0703   = SavDtDoc    and
007200170731     C                             NrDoc0703   = SavNrDoc    and
007300170731     C                             SerDoc0703  = SavSerDoc   and
007400170731     C                             DtReg0703   = SavDtReg    and
007500170731     C                             NrReg0703   = SavNrReg    and
007600170731     C                             SerReg0703  = SavSerReg   and
007700170731     C                             DtAnn0703   = SavDtAnnot
007800130926     C*
007900130926     C                   SetOn                                            RT
008000130926     C                   RETURN
008100130926     C*
008200130926     C                   else
008300130926     C*
008400170731     C                   Eval      Err0703     = *Off
008500170731     C                   Eval      WrtCST0703  = *Off
008600170704     C*
008700170731     C                   Eval      FileO0703   = *Blank
008800170731     C                   Eval      PosizO0703  = 0
008900170731     C                   Eval      IdFilO0703  = *Blank
009000170731     C                   Eval      IdPosO0703  = 0
009100170731     C                   Eval      FileR0703   = *Blank
009200170731     C                   Eval      PosizR0703  = 0
009300170731     C                   Eval      IdFilR0703  = *Blank
009400170731     C                   Eval      IdPosR0703  = 0
009500130926     C*
009600130926     C                   endif
009700130926     C*
009800130925     C*
009900170717     C                   Eval      $TpRec      = 'O'
010000170717     C                   EXSR      CtrExistCST
010100170717     C*
010200170808     C                   if        $WriteCST   = *On
010300170717     C                   Eval      $TpRec      = 'R'
010400170717     C                   EXSR      CtrExistCST
010500170808     C                   endif
010600091019     C*
010700130925     C                   SetOn                                            RT
010800130925     C                   RETURN
010900170717     C*
011000170717     C************************************************************
011100170717     C* Controllo se registrazione già estratta e attiva
011200170717     C************************************************************
011300170717     C     CtrExistCST   BEGSR
011400170717     C*
011500170717     C                   Eval      $ExistCST   = *Off
011600170717     C                   Eval      $WriteCST   = *On
011700170717     C*
011800170731     C                   Move      TpCom0703     CSTTpCom
011900170717     C                   Move      $TpRec        CSTTpRec
012000170731     C                   Z-Add     Sys0703       CSTSys
012100170731     C                   Z-Add     NrAsRe0703    CSTNrAsReg
012200170717     C     K04CST03      SETLL     SDGCST03L                              21
012300170717     C     K04CST03      READE     SDGCST03L                              21
012400170717     C                   Eval      $ExistCST   = Not(*In21)
012500170717     C*
012600170717     C                   Eval      $FineCST    = *In21
012700170717     C                   DoW       $FineCST    = *Off
012800170717     C*
012900170717     C                   EXSR      GestExistCST
013000170717     C*
013100170717     C                   if        $FineCST    = *Off
013200170717     C     K04CST03      READE     SDGCST03L                              21
013300170717     C                   Eval      $FineCST    = *In21
013400170717     C                   endif
013500170717     C*
013600170717     C                   endDo
013700170717     C*
013800170717     C*
013900170717     C* se NON trovato, provo con la chiave documento
014000170717     C                   if        $ExistCST   = *Off
014100170717     C*
014200170731     C                   Eval      CSTSocieta  = Soc0703
014300170731     C                   Eval      CSTTpCom    = TpCom0703
014400170731     C                   Eval      CSTTpRec    = TpRec0703
014500170731     C                   Eval      CSTKCC      = Kcc0703
014600170731     C                   Eval      CSTKSC      = Ksc0703
014700170731     C                   Eval      CSTDTDOC    = DtDoc0703
014800170731     C                   Eval      CSTNRDOC    = NrDoc0703
014900170731     C                   Eval      CSTSERDOC   = SerDoc0703
015000170731     C                   Eval      CSTDTREG    = DtReg0703
015100170731     C                   Eval      CSTNRREG    = NrReg0703
015200170731     C                   Eval      CSTSERREG   = SerReg0703
015300170731     C                   Eval      CSTDtAnnot  = DtAnn0703
015400170717     C     K12CST04      SETLL     SDGCST04L                              21
015500170717     C     K12CST04      READE     SDGCST04L                              21
015600170717     C                   Eval      $ExistCST   = Not(*In21)
015700170717     C*
015800170717     C                   Eval      $FineCST    = *In21
015900170717     C                   DoW       $FineCST    = *Off
016000170717     C*
016100170717     C                   EXSR      GestExistCST
016200170717     C*
016300170717     C                   if        $FineCST    = *Off
016400170717     C     K12CST04      READE     SDGCST04L                              21
016500170717     C                   Eval      $FineCST    = *In21
016600170717     C                   endif
016700170717     C*
016800170717     C                   endDo
016900170717     C*
017000170717     C                   endif
017100170717     C*
017200170731     C                   Eval      WrtCST0703  = $WriteCST
017300170717     C*
017400170717     C                   ENDSR
017500170717     C/EJECT
017600170717     C************************************************************
017700170717     c* Gestione documento trovato nei dati estratti
017800170717     C************************************************************
017900170717     C     GestExistCST  BEGSR
018000170717     C*
018100170717     C                   SELECT
018200170717     C*
018300170717     C*------------
018400170717     C* se devo fare controlli per un Ordinario
018500170731     C                   WHEN      TpRec0703   = 'O'
018600170717     C*
018700170717     C                   Select
018800170717     C*
018900170717     C* se trovo un Ordinario
019000170717     C                   When      CSTTpRec    = 'O'
019100170717     C*
019200170717     C* se è manuale o inviato, non estraggo ed esco
019300170717     C                   if        CSTOrigine  = 'M'        or
019400170717     C                             CSTIdFileO <> *Blank
019500170724     C*
019600170731     C                   Eval      FileO0703   = CSTFileO
019700170731     C                   Eval      PosizO0703  = CSTPosizO
019800170731     C                   Eval      IdFilO0703  = CSTIdFileO
019900170731     C                   Eval      IdPosO0703  = CSTIdPosO
020000170724     C*
020100170731     C                   Eval      FileR0703   = CSTFileR
020200170731     C                   Eval      PosizR0703  = CSTPosizR
020300170731     C                   Eval      IdFilR0703  = CSTIdFileR
020400170731     C                   Eval      IdPosR0703  = CSTIdPosR
020500170717     C*
020600170717     C                   Eval      $WriteCST   = *Off
020700170717     C                   Eval      $FineCST    = *On
020800170717     C*
020900170717     C                   endif
021000170717     C*
021100170717     C* se trovo una Rettifica
021200170717     C                   When      CSTTpRec    = 'R'
021300170717     C*
021400170731     C                   Eval      FileO0703   = CSTFileO
021500170731     C                   Eval      PosizO0703  = CSTPosizO
021600170731     C                   Eval      IdFilO0703  = CSTIdFileO
021700170731     C                   Eval      IdPosO0703  = CSTIdPosO
021800170724     C*
021900170731     C                   Eval      FileR0703   = CSTFileR
022000170731     C                   Eval      PosizR0703  = CSTPosizR
022100170731     C                   Eval      IdFilR0703  = CSTIdFileR
022200170731     C                   Eval      IdPosR0703  = CSTIdPosR
022300170717     C*
022400170721     C* trovato rettifica, e sto estraendo Ord, estraggo (Ann) ed esco
022500170731     C                   Eval      Err0703     = '2'
022600170717     C                   Eval      $FineCST    = *On
022700170717     C*
022800170717     C                   endSl
022900170717     C*
023000170717     C*
023100170717     C*------------
023200170717     C* se devo fare controlli per una Rettifica
023300170731     C                   WHEN      TpRec0703   = 'R'
023400170717     C*
023500170717     C                   Select
023600170717     C*
023700170717     C* se trovo un Ordinario
023800170717     C                   When      CSTTpRec    = 'O'
023900170717     C*
024000170717     C* se inviato, recupero il nome file e la posizione
024100170717     C                   if        CSTIdFileO <> *Blank
024200170717     C*
024300170731     C                   Eval      FileO0703   = CSTFileO
024400170731     C                   Eval      PosizO0703  = CSTPosizO
024500170731     C                   Eval      IdFilO0703  = CSTIdFileO
024600170731     C                   Eval      IdPosO0703  = CSTIdPosO
024700170724     C*
024800170731     C                   Eval      FileR0703   = CSTFileR
024900170731     C                   Eval      PosizR0703  = CSTPosizR
025000170731     C                   Eval      IdFilR0703  = CSTIdFileR
025100170731     C                   Eval      IdPosR0703  = CSTIdPosR
025200170717     C*
025300170724     C                   else
025400170724     C*
025500170724     C* trovato ordinario non inviato,
025600170724     C* e sto estraendo Rett, estraggo (Ann) ed esco
025700170731     C                   Eval      Err0703     = '3'
025800170724     C                   Eval      $FineCST    = *On
025900170724     C                   endif
026000170717     C*
026100170717     C*
026200170717     C* se trovo una Rettifica
026300170717     C                   When      CSTTpRec    = 'R'
026400170717     C*
026500170724     C* se è manuale e NON inviato, non estraggo ed esco
026600170717     C                   if        CSTOrigine  = 'M'        and
026700170717     C                             CSTIdFileR  = *Blank
026800170717     C*
026900170731     C                   Eval      FileO0703   = CSTFileO
027000170731     C                   Eval      PosizO0703  = CSTPosizO
027100170731     C                   Eval      IdFilO0703  = CSTIdFileO
027200170731     C                   Eval      IdPosO0703  = CSTIdPosO
027300170724     C*
027400170731     C                   Eval      FileR0703   = CSTFileR
027500170731     C                   Eval      PosizR0703  = CSTPosizR
027600170731     C                   Eval      IdFilR0703  = CSTIdFileR
027700170731     C                   Eval      IdPosR0703  = CSTIdPosR
027800170717     C*
027900170717     C                   Eval      $WriteCST   = *Off
028000170724     C                   Eval      $FineCST    = *On
028100170717     C*
028200170717     C                   endif
028300170717     C*
028400170717     C                   endSl
028500170717     C*
028600170717     C*
028700170717     C                   ENDSL
028800170717     C*
028900170717     C                   ENDSR
029000170717     C/EJECT
029100170717     C************************************************************
029200170717     C* DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
029300170717     C************************************************************
029400170717     C     DefCam        BEGSR
029500170717     C*
029600170717     C*  SDGCST03L
029700170717     C     K04CST03      KLIST
029800170717     C                   KFLD                    CSTTpCom
029900170717     C                   KFLD                    CSTTpRec
030000170717     C                   KFLD                    CSTSys
030100170717     C                   KFLD                    CSTNrAsReg
030200170717     C*
030300170717     C*  SDGCST04L
030400170717     C     K12CST04      KLIST
030500170717     C                   KFLD                    CSTSocieta
030600170717     C                   KFLD                    CSTTpCom
030700170717     C                   KFLD                    CSTTpRec
030800170717     C                   KFLD                    CSTKcc
030900170717     C                   KFLD                    CSTKsc
031000170717     C                   KFLD                    CSTDtDoc
031100170717     C                   KFLD                    CSTNrDoc
031200170717     C                   KFLD                    CSTSerDoc
031300170717     C                   KFLD                    CSTDtReg
031400170717     C                   KFLD                    CSTNrReg
031500170717     C                   KFLD                    CSTSerReg
031600170717     C                   KFLD                    CSTDTAnnot
031700170717     C*
031800170717     C                   ENDSR
031900170717     C/EJECT
