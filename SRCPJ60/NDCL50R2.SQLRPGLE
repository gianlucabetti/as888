000100061003     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJJMBND)
000200061003     H*PARMS COMMIT(*NONE) DATFMT(*ISO) RDB(*NONE) ACTGRP(QILE)
000300061003     H*PARMS DYNUSRPRF(*OWNER)
000400061003     H DECEDIT('0,') DATEDIT(*DMY) CVTOPT(*VARCHAR)
000500061003     F*--------------------------------------------------------------*
000600061003     F**                                                            **
000700061003     F**           - Analisi notturna impegni                       **
000800061003     F**                                                            **
000900061003     F*--------------------------------------------------------------*
001000061004     FNDImp01L  UF   E           K DISK
001100061003     FNDScp01L  IF   E           K DISK
001200061004     FNDScd01L  IF   E           K DISK
001300090421     FNdPsc01L  UF A E           K DISK    usropn commit
001400061003     D* Passaggio Parametri
001500061003     D KPJBA         E DS
001600061005     D*-------------
001700061005     D* Dati di ambiente ottenuti da XSOC
001800061005    >D SOC001        E DS                  EXTNAME(XSOC001DS)
001900061005     D*-------------
002000061005     D* DS Interna per dati di output di XSOC
002100061005     D XSOCDS          DS          1000
002200061005     D*-------------
002300061004     D* DS gestione parametri modulo
002400061004     D XPAOUT          DS          2000
002500061004     D*-------------
002600090421     D* DS param NDCL1S
002700090421     D anp057ds      E DS
002800061003     D*-------------
002900061003     D* Parametri in entrata
003000061003     D NDCL50DS      E DS
003100061004     D*-------------
003200061004     D* driver tabelle
003300061004     D XATBDS        E DS                  INZ
003400061004     D*-------------
003500061003     D* Reperimento nome PGM
003600061003     D STATUS         SDS           333
003700061003     D  DSPGM            *PROC
003800061003     D  PARMS            *PARMS
003900090421     D  SDSUSR               254    263
004000090421     D  JOB_NUM              264    269S 0
004100061003     D*-------------
004200061003     D* campi di lavoro
004300061004     D Annull          S              1A   inz('1')
004400090421     D Blanc           S              1A   inz(' ')
004500090422     D No              S              1A   inz('0')
004600061004     D TotLScadu       S                   like(ScpLScadu)
004700061004     D SavKcc          S                   like(Scpkcc)
004800061004     D SavKsc          S                   like(Scpksc)
004900061005     D ZDateISO        S               D
005000061004     D*-------------
005100061004     D* Controllo conti
005200061004     D ND002DS       E DS                  inz
005300061005     D*-------------
005400061004     DRefresh          S              1
005500061004     DDataIso          S               D
005600061004     DEsito            S              1
005700061004     DGesMsg           S              1
005800061004     DOutput           S           4000
005900061004     DLenOut           S              9B 0
006000061004     DStrutturaO       S             10
006100090421     D*-------------
006200090421     D* Routine editazione di un numero
006300090421     D $SrcVar         s             30S 0
006400090421     D $SrcVarPr       s              9B 0 inz(30)
006500090421     D $SrcVarDe       s              9B 0
006600090421     D $EdtCode        s              1    inz('K')
006700090421     D $LenFld         s              3S 0 inz(19)
006800090421     D $RcvrVar        s             50
006900090421     D $Err            s              1
007000061004     D*-------------
007100061004     D* Schiera categorie da ND002DS
007200061004     D DsCateg         DS           120
007300061004     D SkCateg                        4    dim(30)
007400100427     D*-------------
007500090421     D* schiera costanti
007600090421     D MSG             S             27    DIM(10) CTDATA PERRCD(1)             messaggi possibili
007700061003     C*----------------------------------------------------*
007800061003     C* Ciclo principale
007900061003     C*----------------------------------------------------*
008000061003     c*
008100061003      * operazioni iniziale
008200061003     C                   exsr      InzVar
008300061003      * esecuzione controllo
008400061003     C                   exsr      Esegui
008500061003      * fine programma
008600061003     c                   exsr      EndPgm
008700061003     C/EJECT
008800061003     C************************************************************
008900061003     C* fine lavoro
009000061003     C************************************************************
009100061003     C     EndPgm        Begsr
009200061003     c*
009300061004     c                   seton                                        LR
009400061004     c                   return
009500061003     C*
009600061003     c                   endsr
009700061003     C/EJECT
009800061003     C************************************************************
009900061003     C* Operazioni iniziali
010000061003     C************************************************************
010100061003     C     InzVAR        BEGSR
010200061004     c*
010300061004     c                   eval      Ndcl50ds=Kpjbu
010400061005     C*
010500061005     c* Reperimento societa
010600061005    >C                   MOVEL     'SOC001'      TIPXSC
010700061005     C                   MOVEL     SocietaL50    SOCXSC
010800061005     C                   CALLB     'XSOC'
010900061005     C                   PARM                    TIPXSC            6
011000061005     C                   PARM                    SOCXSC            3
011100061005     C                   PARM                    CDSXSC            9 0
011200061005     C                   PARM                    MODXSC            3
011300061005     C                   PARM      *BLANKS       RTNXSC            1
011400061005     C                   PARM                    XSOCDS
011500061005     C                   PARM                    KPJBA
011600061005     C*
011700061005     C     RTNXSC        IFNE      '1'
011800061005     C                   MOVEL     XSOCDS        SOC001
011900061005     C                   ELSE
012000061005     c                   exsr      endpgm
012100061005     C                   ENDIF
012200061005     C*
012300061005     C     *DMY          Move      Udate         ZDateISO
012400061004     C*
012500090421     c* Reperimento dati parametro modulo ndcl1s
012600090421     C                   movel(P)  'NDCL1S  '    xpapar
012700061004     C                   movel     *blank        xparic
012800061004     C                   CALLb     'XPAR'
012900061004     C                   PARM                    societal50
013000061004     C                   PARM                    XPAPAR            8
013100061004     C                   PARM                    XPAOUT
013200061004     C                   PARM                    XPAERR            1
013300061004     C                   PARM                    XPARIC            1
013400061004     C                   PARM                    KPJBAE          502
013500061004     C                   PARM                    XPAOPZ            2
013600061004     C                   PARM      '0'           XPAMSG            1
013700061004     C     XPAERR        IFEQ      '0'
013800090421     C                   movel(p)  XpaOut        Anp057Ds
013900061004     c* se non gestiti gli impegni esco subito
014000090421     c                   if        Impegn057 =*off
014100061004     c                   exsr      endpgm
014200061004     C                   endif
014300061004     c                   else
014400061004     c* se non trovato esco
014500061004     c                   exsr      endpgm
014600061004     C                   endif
014700061003     C*
014800061003     c                   endsr
014900061003     C************************************************************
015000061003     C* Esecuzione programma
015100061003     C************************************************************
015200061003     C     Esegui        BEGSR
015300061003     c*
015400061003     c     SocietaL50    setll     NdImp01l
015500061003     c                   do        *hival
015600061004     c     SocietaL50    reade     NdImp01L
015700061003     c                   if        %eof(NdImp01L)
015800061003     c                   leave
015900061003     c                   endif
016000061004     c* CLIENTE
016100070412b1   c                   if        Impksc <> SavKsc
016200090421     c                   eval      SavKcc = ImpKcc
016300070412     c                   eval      SavKsc = ImpKsc
016400061003     c* analisi scaduto cliente
016500061003     c                   exsr      ScadCli
016600061004     c* se devo passare al cliente successivo, mi posiziono
016700061005b2   c                   if        CliSucc= *on
016800061003     c     K03Imp01      setgt     NdImp01L
016900061003     c                   iter
017000061005e2   c                   endif
017100061005e1   c                   endif
017200061004     c*
017300061004     c* PARTITA
017400100427     c* partita fittizia (non ci sono più)
017500061004     C* Se la partita è fittizia, non si fa nessun controllo,
017600061005b1   c                   IF        ImpFitt = *on
017700061004     c*
017800061005X1   c                   ELSE
017900061004     c* partita NON fittizia
018000061003     c* analisi scaduto per partita
018100061003     c                   exsr      ScadPar
018200061003     c* se partita successiva, mi posiziono
018300061005b2   c                   if        ParSucc= *on
018400061003     c     K06Imp01      setgt     NdImp01L
018500061003     c                   iter
018600061005e2   c                   endif
018700061003     C*
018800061004     c* analisi scadenza singola
018900061003     c                   exsr      DetScaPar
019000061003     C*
019100061005e1   c                   endif
019200061004     c*
019300061003     c                   enddo
019400061003     C*
019500061003     c                   endsr
019600061003     C/EJECT
019700061003     C*----------------------------------------------------*
019800061003     C*   Analisi scaduto cliente
019900061003     C*----------------------------------------------------*
020000061003     C     ScadCli       begsr
020100061003     C*
020200061003     c                   clear                   CliSucc           1
020300061004     c                   clear                   TotLScadu
020400061003     C*
020500061003     c                   eval      ScpSocieta=SocietaL50
020600061003     c                   eval      ScpCtb    ='CG'
020700061003     c                   eval      ScpTpScad ='0'
020800061004     c                   eval      ScpDett   = 'P'
020900061003     c                   eval      ScpKcc    =ImpKcc
021000061003     c                   eval      ScpKsc    =ImpKsc
021100061004     c     K06Scp01      setll     Ndscp01l
021200061004     c                   do        *hival
021300061004     c     K06Scp01      reade     Ndscp01l
021400061004     c                   if        %eof(NdScp01L)
021500061004     c                   leave
021600061004     c                   else
021700061004     c* incremento totale scaduto in moneta di conto
021800061004     c                   add       ScpLScadu     TotLScadu
021900090421     C* Pulisco tutti i blocchi della partita del cliente
022000090421     c                   exsr      PulBloPar
022100061004     c                   endif
022200061004     c*
022300061004     c                   enddo
022400061003     C*
022500061004     c* se scaduto non c'è, quindi <=0 annullo tutti gli impegni del
022600061004     c* cliente e indico di passare al cliente succ.
022700061004     c                   if        TotLscadu <=0
022800061004     c                   exsr      AnnImpCli
022900090421     c* confermo le pulizie dei blocchi
023000090421     c                   COMMIT
023100061004     c                   eval      CliSucc=*on
023200090421     C                   else
023300090421     c* ripristino i blocchi delle rate
023400090421     c                   ROLBK
023500061004     c                   endif
023600061004     c*
023700061003     c                   endsr
023800061003     C/EJECT
023900061003     C************************************************************
024000061003     C* ESECUZIONE annullamento impegni per cliente
024100061003     C************************************************************
024200061003     C     AnnImpCli     BEGSR
024300061003     C*
024400061003     C/EXEC SQL
024500061003     C+ UPDATE NDIMP00F
024600061003     C+ SET IMPANN    = :ANNULL
024700061003     C+ WHERE IMPSOCIETA = :SOCIETAL50 AND IMPKCC  = :SCPKCC  AND
024800061003     C+ IMPKSC = : SCPKSC
024900061003     C/END-EXEC
025000061003     C*
025100061003     C                   ENDSR
025200061003     C/EJECT
025300061003     C*----------------------------------------------------*
025400061003     C*   Analisi scaduto cliente/partita
025500061003     C*----------------------------------------------------*
025600061003     C     ScadPar       begsr
025700061003     C*
025800061003     c                   clear                   ParSucc           1
025900061003     C*
026000061003     c                   eval      ScpSocieta=SocietaL50
026100061003     c                   eval      ScpCtb    ='CG'
026200061003     c                   eval      ScpTpScad ='0'
026300061003     c                   eval      ScpDett   = 'P'
026400061003     c                   eval      ScpKcc    =ImpKcc
026500061003     c                   eval      ScpKsc    =ImpKsc
026600061003     c                   eval      Scpdtpar  =ImpDtpar
026700061003     c                   eval      ScpNrpar  =ImpNrpar
026800061003     c                   eval      ScpSerPar =ImpSerPar
026900061003     c     K09Scp01      Chain     Ndscp01l
027000061004     c* se non trovato annullo tutte le scadenze della partita
027100061003     c                   if        not %found(NdSCp01L)
027200061004     c* o se trovato e scaduto < o = 0 annullo tutte le scadenze impegn
027300061003     c                             OR %found(NDscp01L)
027400061003     c                             and ScplScadu <= 0
027500061003     c                   exsr      AnnImpPar
027600090421     C* Pulisco tutti i blocchi della partita del cliente
027700090421     c                   exsr      PulBloPar
027800090421     C                   COMMIT
027900061003     c                   eval      ParSucc=*on
028000061003     c                   endif
028100061003     C*
028200061003     c                   endsr
028300061003     C/EJECT
028400061003     C************************************************************
028500061003     C* ESECUZIONE annullamento impegni per cliente/partita
028600061003     C************************************************************
028700061003     C     AnnImpPar     BEGSR
028800061003     C*
028900061003     C/EXEC SQL
029000061003     C+ UPDATE NDIMP00F
029100061003     C+ SET IMPANN    = :ANNULL
029200061003     C+ WHERE IMPSOCIETA = :SOCIETAL50 AND IMPKCC  = :SCPKCC  AND
029300061003     C+ IMPKSC = : SCPKSC and IMPDTPAR= :SCPDTPAR AND IMPNRPAR =:SCPNRPAR
029400061003     C+ AND IMPSERPAR = :SCPSERPAR
029500061003     C/END-EXEC
029600061003     C*
029700061003     C                   ENDSR
029800061003     C/EJECT
029900090421     C************************************************************
030000070605     C* ESECUZIONE pulizia blocchi partita legati agli impegni
030100090421     C************************************************************
030200070605     C     PulBloPar     BEGSR
030300070605     C*
030400131218     C                   leavesr
030500131218     C*
030600070613     c                   eval      PscSys = Scpsys
030700070613     c                   eval      PscNrasInt=ScpNrasint
030800070613     c     K02Psc01      setll     NdPsc01l
030900070613     c                   do        *hival
031000070613     c     K02Psc01      reade     NdPsc01l
031100070613     c                   if        %eof(NdPsc01l)
031200070613     c                   leave
031300070613     c                   endif
031400070613     c*
031500070613     c                   clear                   PscBKAmm
031600070613     c                   eval      PscCliSoll=*off
031700070613     C                   update    NdPsc000
031800070613     C*
031900070613     c                   enddo
032000070605     C*
032100090421     C                   ENDSR
032200070605     C/EJECT
032300061003     C*----------------------------------------------------*
032400061004     C*   Analisi scadenza singola della partita
032500061003     C*----------------------------------------------------*
032600061003     C     DetScaPar     begsr
032700061003     C*
032800061003     c                   eval      ScdSocieta=SocietaL50
032900061003     c                   eval      ScdCtb    ='CG'
033000061003     c                   eval      ScdTpScad ='0'
033100061003     c                   eval      ScdKcc    =ImpKcc
033200061003     c                   eval      ScdKsc    =ImpKsc
033300061003     c                   eval      Scddtpar  =ImpDtpar
033400061003     c                   eval      ScdNrpar  =ImpNrpar
033500061003     c                   eval      ScdSerPar =ImpSerPar
033600061003     c                   eval      ScdDtSca  =ImpDtSca
033700061004     c     K09Scd01      Chain     NdScd01l
033800061003     c* se non trovato annulla impegno scadenza
033900090422 B1  c                   if        not %found(NdScd01L)
034000090422     C*
034100061005     c                   eval      ImpAnn = *on
034200061005     c                   update    NdImp000
034300090422     c*
034400090421     C* e pulisco blocco rata se presente
034500090422     C                   exsr      PulBloRat
034600090422     C*
034700090422 X1  c                   ELSE
034800090422     C*
034900090421     C* aggiorno o scrivo blocco rata
035000090422     C                   exsr      AggBloRat
035100061004     c*
035200090422E1   c                   ENDIF
035300061003     C*
035400061004     c                   ENDSR
035500090422     C************************************************************
035600090422     C* Pulizia blocco rata
035700090422     C************************************************************
035800090422     C     PulBloRat     BEGSR
035900131218     C*
036000131218     C                   leavesr
036100090422     C*
036200090422     c                   eval      PscSys = Scpsys
036300090422     c                   eval      PscNrasInt=ScpNrasint
036400090422     c                   eval      PscDtSca  =ImpDtSca
036500090422     c     K03Psc01      chain     NdPsc01l
036600090422     c                   if        %found(NdPsc01l)
036700090422     c                   clear                   PscBKAmm
036800090422     c                   eval      PscCliSoll=*off
036900090422     C                   update    NdPsc000
037000090422     c                   COMMIT
037100090422     c                   endif
037200090422     C*
037300090422     c                   ENDSR
037400090422     C************************************************************
037500090422     C* Aggiorna o scrive blocco rata
037600090422     C************************************************************
037700090422     C     AggBloRat     BEGSR
037800131218     C*
037900131218     C                   leavesr
038000090422     C*
038100090422     c                   eval      PscSys = Scpsys
038200090422     c                   eval      PscNrasInt=ScpNrasint
038300090422     c                   eval      PscDtSca  =ImpDtSca
038400090422     c     K03Psc01      chain     NdPsc01l
038500090422     c                   if        %found(NdPsc01l)
038600090422     c                   eval      PscBKAmm=BklSca057
038700090422     c                   eval      PscCliSoll=AggBkl057
038800090422     c                   update    NdPsc000
038900090422     C                   COMMIT
039000090422     c*
039100090422     c                   else
039200090422     c*
039300090422     c* solo se richiesto nel parametro modulo NDCL1S
039400090422     c                   if        BklSca057 <> *blanks or
039500090422     c                             AggBkl057 =*on
039600090422     c                   eval      PscBKAmm=BklSca057
039700090422     c                   eval      PscCliSoll=AggBkl057
039800090422     c                   eval      PscNrRata=ScdNrRata
039900090422     c                   write     NdPsc000
040000090422     C                   COMMIT
040100090422     c                   endif
040200090422     c*
040300090422     c                   endif
040400090422     C*
040500090422     c                   ENDSR
040600061005     C************************************************************
040700061005     C* ESECUZIONE aggiornamento impegni per cliente/partita
040800061005     C************************************************************
040900061005     C     AggImpPar     BEGSR
041000061005     C*
041100061005     C/EXEC SQL
041200061005     C+ UPDATE NDIMP00F
041300061005     C+ SET IMPDTINV  = :DTRIFL50
041400061005     C+ WHERE IMPSOCIETA = :SOCIETAL50 AND IMPKCC  = :SCDKCC  AND
041500061005     C+ IMPKSC = : SCDKSC and IMPDTPAR= :SCDDTPAR AND IMPNRPAR =:SCDNRPAR
041600061005     C+ AND IMPSERPAR = :SCDSERPAR
041700061005     C/END-EXEC
041800061005     C*
041900061005     C                   ENDSR
042000061005     C/EJECT
042100061003     C*----------------------------------------------------*
042200061003     C*     DEFINIZIONE CHIAVI                             *
042300061003     C*----------------------------------------------------*
042400061003     C     DEFklist      begsr
042500061003     C*
042600061003     C* klist
042700061003    >C* ndImp01l
042800061003    >C     K03Imp01      KLIST
042900061003    >C                   KFLD                    ImpSocieta
043000061003    >C                   KFLD                    ImpKCC
043100061003    >C                   KFLD                    ImpKSC
043200061003    >C     K06Imp01      KLIST
043300061003    >C                   KFLD                    ImpSocieta
043400061003    >C                   KFLD                    ImpKCC
043500061003    >C                   KFLD                    ImpKSC
043600061003    >C                   KFLD                    ImpDtpar
043700061003    >C                   KFLD                    ImpNrpar
043800061003    >C                   KFLD                    ImpSerPar
043900061003    >C* ndscP01l
044000061003    >C     K06scP01      KLIST
044100061003    >C                   KFLD                    scPSocieta
044200061003    >C                   KFLD                    ScPCTB
044300061003    >C                   KFLD                    ScPtpscad
044400061003     C                   KFLD                    SCPDETT
044500061003    >C                   KFLD                    ScPKCC
044600061003    >C                   KFLD                    ScPKSC
044700061003    >C     K09scP01      KLIST
044800061003    >C                   KFLD                    scPSocieta
044900061003    >C                   KFLD                    ScPCTB
045000061003    >C                   KFLD                    ScPtpscad
045100061003     C                   KFLD                    SCPDETT
045200061003    >C                   KFLD                    ScPKCC
045300061003    >C                   KFLD                    ScPKSC
045400061003    >C                   KFLD                    ScPDtpar
045500061003    >C                   KFLD                    ScPNrpar
045600061003    >C                   KFLD                    ScPSerPar
045700061003    >C* ndscd01l
045800061003    >C     K09scd01      KLIST
045900061003    >C                   KFLD                    scdSocieta
046000061003    >C                   KFLD                    ScdCTB
046100061003    >C                   KFLD                    Scdtpscad
046200061003    >C                   KFLD                    ScdKCC
046300061003    >C                   KFLD                    ScdKSC
046400061003    >C                   KFLD                    ScdDtpar
046500061003    >C                   KFLD                    ScdNrpar
046600061003    >C                   KFLD                    ScdSerPar
046700061003    >C                   KFLD                    ScdDtSca
046800090421     C*NDpsc01l
046900090421     C     K03PSc01      KLIST
047000090421     C                   KFLD                    PSCSys
047100090421     C                   KFLD                    PSCNrasInt
047200090421     C                   KFLD                    PSCDtSca
047300090421     C     K02PSc01      KLIST
047400090421     C                   KFLD                    PSCSys
047500090421     C                   KFLD                    PSCNrasInt
047600061003     c*
047700061003     c                   endsr
047800061004     C********************************************************************
047900061004     C* Chiamata NDMVC002  controlla conto
048000061004     C***********************************************************
048100061004     C     CallMvc002    BEGSR
048200061004     C*
048300061004     C* Controllo conto
048400061004     c                   move      *loval        Dataiso
048500061004     C                   Clear                   ND002DS
048600061004     C                   EVAL      LenOut = %Size(ND002DS)
048700061004     C                   CallB     'NDMVC002'
048800061004     C                   PARM                    societaL50
048900061004     C                   PARM                    Savkcc
049000061004     C                   PARM                    Savksc
049100061004     C                   PARM                    DataIso
049200061004     C                   PARM      *OFF          GesMsg
049300061004     C                   PARM      *OFF          Esito
049400061004     C                   PARM      'ND002DS'     StrutturaO
049500061004     C                   PARM                    ND002DS
049600061004     C                   PARM                    LenOut
049700061004     C*
049800061004     C                   ENDSR
049900061004     c*
050000061004     C/EJECT
050100090422     c*
050200090421     C************************************************************
050300070605     C* Operazioni iniziali
050400090421     C************************************************************
050500070605     C     *INZSR        BEGSR
050600070605     c*
050700070605     C     *ENTRY        PLIST
050800070605     C                   PARM                    kpjba
050900070605     c                   eval      Ndcl50ds=Kpjbu
051000070605     C*
051100070605    >C                   CALLB     'XSTRCMT'
051200070605    >C                   OPEN      NDPSC01L
051300090421     c*
051400090421     c                   endsr
051500090421     C************************************************************
051600090421     C* Chiamata a xrtvm
051700090421     C************************************************************
051800090421     C     PrendiMsg     BEGSR
051900090421     C*
052000090421     C                   CALLB     'XRTVM'
052100090421     C                   PARM                    IDMSG            27
052200090421     C                   PARM                    TXTMSG          100
052300090421     C                   PARM                    ERRMSG            1
052400090421     C                   PARM      *off          CTRMSG            1            centratura
052500090421     C                   PARM                    LENMSG            3 0          lun output.
052600090421     C                   PARM                    MSgDta          100            lun output.
052700090421     C                   PARM                    TxtMsg30         30            lun output.
052800090421     C                   PARM                    TxtMsg3000     3000            lun output.
052900090421     C     ERRMSG        IFNE      *ON
053000090421     C                   MOVEL     TxTMsg        WWMSG           100            *
053100090421     C                   ELSE
053200090421     C                   MOVEL     *ALL'?'       WWMSG
053300090421     C                   ENDIF
053400090421     C*
053500090421     C                   ENDSR
053600070605     C*
053700070605      *
053800090421** MSG
053900090421PROMSG    *LIBL     COS2765    Impegno
054000090421PROMSG    *LIBL     COS2766    Importo Impegno
