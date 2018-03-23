000100100317     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200100317     H*PARMS COMMIT(*NONE) DATFMT(*ISO) RDB(*NONE) ACTGRP(QILE)
000300100317     H*PARMS DYNUSRPRF(*OWNER)
000400100317     H DECEDIT('0,') DATEDIT(*DMY)
000500061003     F*--------------------------------------------------------------*
000600061003     F**                                                            **
000700100309     F**           - Analisi notturna attività recupero credito     **
000800061003     F**                                                            **
000900061003     F*--------------------------------------------------------------*
001000100408     FNDRCA01L  UF A E           K DISK
001100100317     FNDRCA02L  IF   E           K DISK
001200100317     F                                     RENAME(NDRCA000:NDRCA02)
001300131210     FNDRCA04L  UF A E           K DISK
001400131210     F                                     RENAME(NDRCA000:NDRCA04)
001500131210     F                                     extfile('NDRCA01L')
001600130607C2225FNDRCA07L  IF A E           K DISK
001700130607C2225F                                     RENAME(NDRCA000:NDRCA07)
001800100309     FNDRCI01L  IF   E           K DISK
001900130409C2221FNDRCP01L  IF   E           K DISK
002000100331     FNDSca05L  IF   E           K DISK
002100110419C2184FNDEpa01L  UF   E           K DISK
002200100331     FNDScl01L  IF   E           K DISK
002300100331     FNDScp01L  IF   E           K DISK
002400100331     FNDScd01L  IF   E           K DISK
002500090421     FNdPsc01L  UF A E           K DISK    usropn commit
002600100317     FNdPps01L  UF   E           K DISK
002700100521     FNdPpa01L  IF   E           K DISK
002800100317     FNdPas03L  IF   E           K DISK
002900100525     FNDImp01L  IF   E           K DISK
003000100419    >FNDCl91P   O    E             PRINTER OFLIND(*IN39) USROPN
003100061003     D* Passaggio Parametri
003200061003     D KPJBA         E DS
003300061005     D*-------------
003400061005     D* Dati di ambiente ottenuti da XSOC
003500061005    >D SOC001        E DS                  EXTNAME(XSOC001DS)
003600061005     D*-------------
003700061005     D* DS Interna per dati di output di XSOC
003800061005     D XSOCDS          DS          1000
003900061005     D*-------------
004000061004     D* DS gestione parametri modulo
004100061004     D XPAOUT          DS          2000
004200061004     D*-------------
004300090421     D* DS param NDCL1S
004400090421     D anp057ds      E DS
004500061003     D*-------------
004600061003     D* Parametri in entrata
004700100309     D NDCL90DS      E DS
004800100309     D*-------------
004900100309     D* Controllo conti
005000100309     D ND002DS       E DS                  inz
005100100309     D Categorie             131    250
005200100317     D*-------------
005300100317     D SCDFLG          DS
005400100317     D  SCDFLG1                       1A
005500100317     D  SCDFLG2                       1A
005600100317     D  SCDFLG3                       1A
005700100317     D  SCDFLG4                       7A
005800061004     D*-------------
005900061003     D* Reperimento nome PGM
006000061003     D STATUS         SDS           333
006100061003     D  DSPGM            *PROC
006200061003     D  PARMS            *PARMS
006300090421     D  SDSUSR               254    263
006400090421     D  JOB_NUM              264    269S 0
006500061003     D*-------------
006600061003     D* campi di lavoro
006700100413     D Chiusa          S              1A   inz('2')
006800100331     D TotLScadu       S             19  3
006900100413     D TotLAScad       S             19  3
007000100420     D*totale movimenti provvisori dare/avere
007100100420     D TotTDLP         S             19  3
007200100420     D TotTALP         S             19  3
007300100420     D*totale movimenti gestionali dare/avere
007400100420     D TotTDLG         S             19  3
007500100420     D TotTALG         S             19  3
007600100408     D Lettura         S              3
007700100331     D SavKcc          S                   like(Rcakcc)
007800100331     D SavKsc          S                   like(Rcaksc)
007900100331     D $kcc            S                   like(Rcakcc)
008000100331     D $ksc            S                   like(Rcaksc)
008100100323     D SavAttrib       S                   like(RcaAttrib)
008200100323     D SavUnita        S                   like(RcaUnita)
008300100421     D SavProgr        S                   like(RcaProgr)
008400110401D2680D SavAzione       S                   like(RcaAzione)
008500061005     D ZDateISO        S               D
008600100316     D WRkDtSca        S               D
008700130409C2221D TestUltFas      S              1
008800130410  "  D $progr          S                   like(RCAProgr)
008900130410C2221D $Ruf            S                   like(RCPRuf)
009000100309     D*-------------
009100100309     DRefresh          S              1
009200100309     DDataIso          S               D
009300100309     DEsito            S              1
009400100309     DGesMsg           S              1
009500100309     DOutput           S           4000
009600100309     DLenOut           S              9B 0
009700100309     DStrutturaO       S             10
009800110429c2184DAnope          E DS                  EXTNAME(ANOPE00F) inz
009900100309     D*-------------
010000061004     D* Schiera categorie da ND002DS
010100061004     D DsCateg         DS           120
010200061004     D SkCateg                        4    dim(30)
010300100317     D*-------------
010400100317     D* Tabelle
010500100317     D XATBDs        E DS                  INZ
010600100323     D*-------------
010700100324     D* Tabella rimesse dirette
010800100324     D angG56ds      E DS                  inz
010900100317     D*-------------
011000100324     D* Modalità di pagamento
011100100324     D angG57ds      E DS                  inz
011200100324     D*-------------
011300100317     D*  Ds gruppi utenti
011400100317     D Ang048ds      E DS                  INZ
011500100324     D*-------------
011600100324     D*  Ds tabella azioni
011700100324     D Ang047Ds      E DS                  INZ
011800100323     D*-------------
011900100323     D*  DS per input DVAGE
012000100323     D DVAGE         E DS                  EXTNAME(DVAGEDS) INZ
012100100323     D*-------------
012200100323     D*  Ds per output DVAGE
012300100323     D DVOAGE        E DS                  EXTNAME(DVOAGEDS) INZ
012400100323     D*-------------
012500140312  "  D* parametri per calcolo giorni
012600140312D0948D X17CLVALDS    E DS                  INZ
012700061003     C*----------------------------------------------------*
012800061003     C* Ciclo principale
012900061003     C*----------------------------------------------------*
013000061003     c*
013100061003      * operazioni iniziale
013200061003     C                   exsr      InzVar
013300061003      * esecuzione controllo
013400061003     C                   exsr      Esegui
013500061003      * fine programma
013600061003     c                   exsr      EndPgm
013700061003     C/EJECT
013800061003     C************************************************************
013900061003     C* fine lavoro
014000061003     C************************************************************
014100061003     C     EndPgm        Begsr
014200061003     c*
014300130915C2226C                   Close     Ndcl91P
014400061004     c                   seton                                        LR
014500061004     c                   return
014600061003     C*
014700061003     c                   endsr
014800061003     C/EJECT
014900061003     C************************************************************
015000061003     C* Operazioni iniziali
015100061003     C************************************************************
015200061003     C     InzVAR        BEGSR
015300061004     c*
015400100309     c                   eval      Ndcl90ds=Kpjbu
015500061005     C*
015600061005     c* Reperimento societa
015700061005    >C                   MOVEL     'SOC001'      TIPXSC
015800100309     C                   MOVEL     SocietaL90    SOCXSC
015900061005     C                   CALLB     'XSOC'
016000061005     C                   PARM                    TIPXSC            6
016100061005     C                   PARM                    SOCXSC            3
016200061005     C                   PARM                    CDSXSC            9 0
016300061005     C                   PARM                    MODXSC            3
016400061005     C                   PARM      *BLANKS       RTNXSC            1
016500061005     C                   PARM                    XSOCDS
016600061005     C                   PARM                    KPJBA
016700061005     C*
016800061005     C     RTNXSC        IFNE      '1'
016900061005     C                   MOVEL     XSOCDS        SOC001
017000061005     C                   ELSE
017100061005     c                   exsr      endpgm
017200061005     C                   ENDIF
017300061005     C*
017400061005     C     *DMY          Move      Udate         ZDateISO
017500061004     C*
017600090421     c* Reperimento dati parametro modulo ndcl1s
017700090421     C                   movel(P)  'NDCL1S  '    xpapar
017800061004     C                   movel     *blank        xparic
017900061004     C                   CALLb     'XPAR'
018000100309     C                   PARM                    societal90
018100061004     C                   PARM                    XPAPAR            8
018200061004     C                   PARM                    XPAOUT
018300061004     C                   PARM                    XPAERR            1
018400061004     C                   PARM                    XPARIC            1
018500061004     C                   PARM                    KPJBAE          502
018600061004     C                   PARM                    XPAOPZ            2
018700061004     C                   PARM      '0'           XPAMSG            1
018800061004     C     XPAERR        IFEQ      '0'
018900090421     C                   movel(p)  XpaOut        Anp057Ds
019000100309     c                   move      posattr057    posiz             2 0
019100061004     c                   else
019200061004     c* se non trovato esco
019300061004     c                   exsr      endpgm
019400061004     C                   endif
019500100319     c*
019600100319     C* Valorizzazione campi univoci testate
019700100319     C                   MOVEL     KNSIF         NOMSIF
019800100319     C                   MOVEL     XSCDSI        NOMDIT
019900100319     C                   MOVEL     DSPGM         NOMPGM
020000100319     C* Reperisco nome sistema informatico
020100100319     C                   CALL      'XNETA'                              21
020200100319     C                   PARM                    NOMSYS
020300061003     C*
020400100309     c                   endsr
020500100309     C************************************************************
020600100309     C* Esecuzione programma
020700100309     C************************************************************
020800100309     C     Esegui        BEGSR
020900100317     c*
021000100413     c* lettura attività RC e chiusura delle attività legate
021100100317     c* a partite chiuse
021200100317     c                   exsr      LeggiRCA
021300100309     c*
021400100331     c* lettura file di lavoro avvisi e scrittura attività RC
021500100331     c                   exsr      LeggiAVV
021600100331     c* lettura file di lavoro solleciti e scrittura attività RC
021700100331     c                   exsr      LeggiSOL
021800100317     c*
021900100317     c* lettura attività RC dopo nuova scrittura e creazione stampe
022000100317     c* per fase da mandare al gruppo utenti
022100100317     c                   exsr      StampaRCA
022200100309     c*
022300061003     c                   endsr
022400061003     C************************************************************
022500100413     c* lettura attività RC fasi aperte e annullate e chiude le attività
022600100413     c* legate a partite chiuse  e cancella le attività annullate
022700061003     C************************************************************
022800100309     C     LeggiRCA      BEGSR
022900061003     c*
023000100318     c                   move      SocietaL90    RcaSocieta
023100100318     c                   move      posattr057    RcaPosiz
023200100318     c*
023300100318     c     K02Rca01      setll     NdRcA01l
023400061003     c                   do        *hival
023500100408     c     K02Rca01      reade     NdRCA01L
023600100309     c                   if        %eof(NDRCA01L)
023700061003     c                   leave
023800061003     c                   endif
023900110505     c* escludo le attività chiuse
024000110505c2184X**************     if        RcaStato='1'or
024100110505  "  x* lo stato '1' lo deve analizzare!!!
024200110505  "  x**************               RcaStato='2'
024300110505c2184c                   If        RcaStato='2'
024400110505     c                   iter
024500110505     c                   endif
024600061004     c* CLIENTE
024700100309     c* a rottura cliente
024800100309b1   c                   if        Rcaksc <> SavKsc
024900100309     c                   eval      SavKcc = RcaKcc
025000100309     c                   eval      SavKsc = RcaKsc
025100100309     c* eseguo analisi scaduto cliente
025200061003     c                   exsr      ScadCli
025300061004     c* se devo passare al cliente successivo, mi posiziono
025400061005b2   c                   if        CliSucc= *on
025500100318     c     K04Rca01      setgt     NdRca01L
025600061003     c                   iter
025700061005e2   c                   endif
025800061005e1   c                   endif
025900061004     c*
026000061004     c* PARTITA
026100061003     c* analisi scaduto per partita
026200061003     c                   exsr      ScadPar
026300061003     c* se partita successiva, mi posiziono
026400061005b2   c                   if        ParSucc= *on
026500100318     c     K07Rca01      setgt     NdRca01L
026600061003     c                   iter
026700061005e2   c                   endif
026800061003     C*
026900061004     c* analisi scadenza singola
027000061003     c                   exsr      DetScaPar
027100061004     c*
027200061003     c                   enddo
027300061003     C*
027400061003     c                   endsr
027500061003     C/EJECT
027600061003     C*----------------------------------------------------*
027700061003     C*   Analisi scaduto cliente
027800061003     C*----------------------------------------------------*
027900061003     C     ScadCli       begsr
028000061003     C*
028100061003     c                   clear                   CliSucc           1
028200061004     c                   clear                   TotLScadu
028300100413     c                   clear                   TotLAScad
028400100420     c                   clear                   TotTDLP
028500100420     c                   clear                   TotTALP
028600100420     c                   clear                   TotTDLG
028700100420     c                   clear                   TotTALG
028800061003     C*
028900100309     c                   eval      ScpSocieta=SocietaL90
029000061003     c                   eval      ScpCtb    ='CG'
029100061003     c                   eval      ScpTpScad ='0'
029200061004     c                   eval      ScpDett   = 'P'
029300100316     c                   eval      ScpKcc    =RCAKcc
029400100316     c                   eval      ScpKsc    =RCAKsc
029500061004     c     K06Scp01      setll     Ndscp01l
029600061004     c                   do        *hival
029700061004     c     K06Scp01      reade     Ndscp01l
029800061004     c                   if        %eof(NdScp01L)
029900061004     c                   leave
030000061004     c                   else
030100061004     c* incremento totale scaduto in moneta di conto
030200061004     c                   add       ScpLScadu     TotLScadu
030300100413     c                   add       ScpLAScad     TotLAScad
030400100420     C                   add       scpTDLP       TotTDLP
030500100420     C                   add       scpTALP       TotTALP
030600100420     C                   add       scpTDLG       TotTDLG
030700100420     C                   add       scpTALG       TotTALG
030800090421     C* Pulisco tutti i blocchi della partita del cliente
030900090421     c                   exsr      PulBloPar
031000061004     c                   endif
031100061004     c*
031200061004     c                   enddo
031300061003     C*
031400100416     c* se scaduto non c'è, quindi  =0 chiudo tutte le attività del
031500061004     c* cliente e indico di passare al cliente succ.
031600100420     c* ma anche se non c'è dell'a scadere e NON CI SONO dare/avere provviso
031700100420     c* ri
031800100416     c                   if        TotLscadu  =0 and
031900100420     c                             TotLAScad  =0 and
032000100420     c                             TotTDLP    =0 and
032100100420     c                             TotTALP    =0 and
032200100420     c                             TotTDLG    =0 and
032300100420     c                             TotTALG    =0
032400100413     c                   exsr      ChiAttCli
032500090421     c* confermo le pulizie dei blocchi
032600090421     c                   COMMIT
032700061004     c                   eval      CliSucc=*on
032800090421     C                   else
032900090421     c* ripristino i blocchi delle rate
033000090421     c                   ROLBK
033100061004     c                   endif
033200061004     c*
033300061003     c                   endsr
033400061003     C/EJECT
033500061003     C************************************************************
033600100413     C* ESECUZIONE chiusura attività per il cliente:partita chiusa
033700061003     C************************************************************
033800100413     C     ChiAttCli     BEGSR
033900100413     C*
034000100413     C* mette stato '2'=partita chiusa nelle fasi non annullate
034100100413     C*
034200061003     C/EXEC SQL
034300100309     C+ UPDATE NDRCA00F
034400140314     C+ SET RCASTATO  = :CHIUSA , rcadtchiu = current date
034500100309     C+ WHERE RCASOCIETA = :SOCIETAL90 AND RCAKCC  = :SCPKCC  AND
034600140314     C+ RCAKSC = : SCPKSC and RCAANN <> 'A'
034700140314     C+ and RCASTATO <> :CHIUSA
034800061003     C/END-EXEC
034900061003     C*
035000100413     C* cancella le fasi annullate
035100100413     C*
035200100413     C/EXEC SQL
035300100413     C+ DELETE FROM NDRCA00F
035400100413     C+ WHERE RCASOCIETA = :SOCIETAL90 AND RCAKCC  = :SCPKCC  AND
035500100413     C+ RCAKSC = : SCPKSC and RCAANN = 'A'
035600100413     C/END-EXEC
035700061003     C                   ENDSR
035800061003     C/EJECT
035900061003     C*----------------------------------------------------*
036000061003     C*   Analisi scaduto cliente/partita
036100061003     C*----------------------------------------------------*
036200061003     C     ScadPar       begsr
036300061003     C*
036400061003     c                   clear                   ParSucc           1
036500061003     C*
036600100309     c                   eval      ScpSocieta=SocietaL90
036700061003     c                   eval      ScpCtb    ='CG'
036800061003     c                   eval      ScpTpScad ='0'
036900061003     c                   eval      ScpDett   = 'P'
037000100309     c                   eval      ScpKcc    =RCAKcc
037100100309     c                   eval      ScpKsc    =RCAKsc
037200100309     c                   eval      Scpdtpar  =RCADtpar
037300100309     c                   eval      ScpNrpar  =RCANrpar
037400100309     c                   eval      ScpSerPar =RCASerPar
037500061003     c     K09Scp01      Chain     Ndscp01l
037600100413     c* se non trovato chiudo tutte le scadenze della partita
037700061003     c                   if        not %found(NdSCp01L)
037800100413     c* o se trovato e scaduto < o = 0 chiudo tutte le attività
037900100413     c* e se trovato e a scadere<o = 0 chiudo tutte le attività
038000061003     c                             OR %found(NDscp01L)
038100100416     c                             and ScplScadu  = 0
038200100416     c                             and ScplAScad  = 0
038300100420     c                             and ScpTDLP    = 0
038400100420     c                             and ScpTALP    = 0
038500100420     c                             and ScpTDLG    = 0
038600100420     c                             and ScpTALG    = 0
038700100413     c                   exsr      ChiRCAPar
038800090421     C* Pulisco tutti i blocchi della partita del cliente
038900090421     c                   exsr      PulBloPar
039000090421     C                   COMMIT
039100061003     c                   eval      ParSucc=*on
039200061003     c                   endif
039300061003     C*
039400061003     c                   endsr
039500061003     C/EJECT
039600100413     C******************************************************************
039700100413     C* ESECUZIONE chiusura attività per cliente/partita:partita chiusa
039800100413     C******************************************************************
039900100413     C     ChiRcaPar     BEGSR
040000100413     C*
040100100413     C* mette stato '2'=partita chiusa nelle fasi non annullate
040200100413     C*
040300061003     C/EXEC SQL
040400100309     C+ UPDATE NDRCA00F
040500140314     C+ SET RCASTATO  = :CHIUSA , rcadtchiu = current date
040600100309     C+ WHERE RCASOCIETA = :SOCIETAL90 AND RCAKCC  = :SCPKCC  AND
040700100309     C+ RCAKSC = : SCPKSC and RCADTPAR= :SCPDTPAR AND RCANRPAR =:SCPNRPAR
040800100413     C+ AND RCASERPAR = :SCPSERPAR and RCAANN <> 'A'
040900140314     C+ and RCASTATO <> :CHIUSA
041000061003     C/END-EXEC
041100100413     C*
041200100413     C* cancella le fasi annullate
041300100413     C*
041400100413     C/EXEC SQL
041500100413     C+ DELETE FROM NDRCA00F
041600100413     C+ WHERE RCASOCIETA = :SOCIETAL90 AND RCAKCC  = :SCPKCC  AND
041700100413     C+ RCAKSC = : SCPKSC and RCADTPAR= :SCPDTPAR AND RCANRPAR =:SCPNRPAR
041800100413     C+ AND RCASERPAR = :SCPSERPAR and RCAANN = 'A'
041900100413     C/END-EXEC
042000061003     C*
042100061003     C                   ENDSR
042200061003     C/EJECT
042300090421     C************************************************************
042400100309     C* ESECUZIONE pulizia blocchi partita legati alle attività
042500090421     C************************************************************
042600070605     C     PulBloPar     BEGSR
042700131204      * !!!!!!!!!!!
042800131204     C                   leavesr
042900070605     C*
043000070613     c                   eval      PscSys = Scpsys
043100070613     c                   eval      PscNrasInt=ScpNrasint
043200070613     c     K02Psc01      setll     NdPsc01l
043300070613     c                   do        *hival
043400070613     c     K02Psc01      reade     NdPsc01l
043500070613     c                   if        %eof(NdPsc01l)
043600070613     c                   leave
043700070613     c                   endif
043800070613     c*
043900070613     c                   clear                   PscBKAmm
044000070613     c                   eval      PscCliSoll=*off
044100070613     C                   update    NdPsc000
044200070613     C*
044300070613     c                   enddo
044400070605     C*
044500090421     C                   ENDSR
044600070605     C/EJECT
044700061003     C*----------------------------------------------------*
044800100309     C*   Analisi scadenze della singola partita
044900061003     C*----------------------------------------------------*
045000061003     C     DetScaPar     begsr
045100061003     C*
045200100309     c                   eval      ScdSocieta=SocietaL90
045300061003     c                   eval      ScdCtb    ='CG'
045400061003     c                   eval      ScdTpScad ='0'
045500100309     c                   eval      ScdKcc    =RcaKcc
045600100309     c                   eval      ScdKsc    =RcaKsc
045700100309     c                   eval      Scddtpar  =RcaDtpar
045800100309     c                   eval      ScdNrpar  =RcaNrpar
045900100309     c                   eval      ScdSerPar =RcaSerPar
046000100309     c                   eval      ScdDtSca  =RCADtSca
046100061004     c     K09Scd01      Chain     NdScd01l
046200100413     c* se non trovato chiude l'attività della scadenza stessa
046300090422 B1  c                   if        not %found(NdScd01L)
046400100413     C                   if        RcaAnn <> 'A'
046500100421     c                   eval      RcaStato = '2'
046600100309     c                   update    NdRca000
046700100413     c                   else
046800100413     c                   delete    NdRca000
046900100413     c                   endif
047000090421     C* e pulisco blocco rata se presente
047100090422     C                   exsr      PulBloRat
047200061004     c*
047300090422E1   c                   ENDIF
047400061003     C*
047500061004     c                   ENDSR
047600090422     C************************************************************
047700090422     C* Pulizia blocco rata
047800090422     C************************************************************
047900090422     C     PulBloRat     BEGSR
048000131204      * !!!!!!!!!!!
048100131204     C                   leavesr
048200090422     C*
048300090422     c                   eval      PscSys = Scpsys
048400090422     c                   eval      PscNrasInt=ScpNrasint
048500100309     c                   eval      PscDtSca  =RcaDtSca
048600090422     c     K03Psc01      chain     NdPsc01l
048700090422     c                   if        %found(NdPsc01l)
048800090422     c                   clear                   PscBKAmm
048900090422     c                   eval      PscCliSoll=*off
049000090422     C                   update    NdPsc000
049100090422     c                   COMMIT
049200090422     c                   endif
049300090422     C*
049400090422     c                   ENDSR
049500100309     c*
049600100331     C******************************************************************
049700100331     c* lettura file di lavoro avvisi e scrittura attività RC
049800100331     C******************************************************************
049900100331     C     LeggiAVV      BEGSR
050000100331     c*
050100100408     C                   eval      Lettura='AVV'
050200100331     c                   clear                   SavKcc
050300100331     c                   clear                   SavKsc
050400100331     c*
050500100331     c                   eval      ScaSocieta=SocietaL90
050600100331     c*
050700100331     c     ScaSocieta    setll     NdSca05l
050800100331     c                   do        *hival
050900100331     c     ScaSocieta    reade     NdSCa05L
051000100331     c                   if        %eof(NDSCa05L)
051100100331     c                   leave
051200100331     c                   endif
051300110429c2184C* Escludo partite con saldo avere                                te
051400110429  "  C                   IF        scasegno='A'
051500110429  "  c                   move      Scacaus2      causale
051600110429  "  C                   exsr      NDMVC003
051700110429  "  C* ...che non derivano da nota credito, effetti e altri titoli.
051800110429  "  C     OPETPC        IFNE      '1'
051900110429  "  C     OPEPORTAF     ANDNE     '1'
052000110429  "  C     OPECALSCAD    ANDNE     '2'
052100110429  "  C                   iter
052200110429  "  c                   endif
052300110429  "  c*
052400110429  "  C* Escludo note di credito
052500110429  "  C     opetpc        IFeq      '1'
052600110429  "  c                   iter
052700110429  "  c                   endif
052800110429c2184c                   endif
052900100331     c*
053000100331     c* reperisco l'attributo del sottoconto al cambio di sottoc.
053100100331     c                   if        Scaksc <> Savksc
053200100331     C* se valorizzata posizione attr. nel param. modulo solleciti
053300100331     c                   if        Posattr057 <> *blanks
053400100331     c                   move      Scakcc        $kcc
053500100331     c                   move      Scaksc        $ksc
053600100331     c                   exsr      Callmvc002
053700100331     c                   move      Posattr057    RciPosiz
053800100331     c                   move      WrkAttr       RciAttrib
053900100331     c                   endif
054000100331     c                   move      Scaksc        Savksc
054100100331     c                   endif
054200100408     c*
054300100408     c* LEGGO ULTIMA ATTIVITÀ PRESENTE PER LA SCADENZA
054400100413     c* (devono essere considerate anche quelle annullate)
054500130409C2221C                   Movel     *Off          TestUltFas
054600130410  "  C                   Clear                   $Progr
054700130410C2221C                   Clear                   $Ruf
054800100408     c                   eval      RcaSocieta=SocietaL90
054900100426     c                   eval      RcaPosiz  =PosAttr057
055000100408     c                   eval      RcaKcc    =ScaKcc
055100100408     c                   eval      RcaKsc    =ScaKsc
055200100408     c                   eval      Rcadtpar  =ScaDtPar
055300100408     c                   eval      RcaNrpar  =ScaNrPar
055400100408     c                   eval      RcaSerPar =ScaSerPar
055500100408     c                   eval      RcaDtSca  =ScaDtSca
055600131031     c***                eval      RcaDtAtt  = *hival
055700131031     c***  k09Rca04      setll     NdRca04L
055800131031     c***  k08Rca04      reade     NdRca04L
055900131210     c     k08Rca04      setgt     NdRca04L
056000131210     c     k08Rca04      readpe(N) NdRca04L
056100100512     C* se rata pagata, non faccio nulla e leggo altra rata
056200131031D2755C***                If        not %eof(Ndrca04l)
056300131210D2755C                   If        not %eof(Ndrca04l)
056400100512     c                   if        RcaStato ='2'
056500100512     c                   iter
056600100512     c                   endif
056700121105D2755C                   endif
056800100408     c* se non cè nessuna attività, aggancio impostazioni
056900131031     c***                if        %eof(NdRca04l)
057000131210     c                   if        %eof(NdRca04l)
057100100408     c                   exsr      LeggiImpo
057200100408     c                   else
057300100426     c* se la fase è aperta, non si fa nulla, a meno che non  si tratti
057400100511     c* di un impegno codice '98'
057500130607C2225c* o di un impegno con codice '99' se cambia la data lo devo
057600130607C2225c* riportare a '98'
057700100426     c                   select
057800100511     c                   when      RcaAzione ='98'
057900130607C2225C                             or RcaAzione = '99'
058000100426     c                   exsr      TestImpegni
058100100408     c* se la fase è chiusa, aggiungo i giorni indicati nelle
058200100408     c* impostazioni alla data attività e se la data risulta <= alla
058300100408     c* data del giorno, scrive la segnalazione della fase successiva
058400100512     C* da impostazioni
058500100426     c                   when      RcaStato ='1'
058600130409C2221X**** EX D2762                and Rcafase < 9
058700100408     c                   eval      RciSocieta=SocietaL90
058800110407D2680c                   clear                   RciUnita
058900130906C2226X** exD2680         if        GesUni057='D'
059000130906C2226C                   if        GesUniI057='D'
059100100408     c                   eval      RciUnita  =ScaUniPar
059200110407D2680c                   endif
059300100408     c                   eval      RciKcc    =ScaKcc
059400100408     c                   eval      RciKcc    =ScaKcc
059500121213D2762C                   eval      Rciattrib =RcaAttrib
059600130409C2221C                   If        RcaFase < 9
059700100408     c                   eval      Rcifase   =RcaFase +1
059800130409C2221C                   Else
059900130409  "  C                   Movel     *on           TestUltFas
060000130409  "  C                   eval      Rcifase   =RcaFase
060100130410  "  C                   eval      $Progr    =RcaProgr + 1
060200130409C2221C                   Endif
060300100408     c     K06Rci01      chain     NdRci01l
060400130410C2221C                   If        Not %Found(ndrci01l) or
060500130410  "  C                             (%Found(ndrci01l) and
060600130410  "  C                             RCIAzione = *blanks)
060700130410  "  C                   eval      RciFase = RcaFase
060800130410  "  C                   eval      $Progr    =RcaProgr + 1
060900130410  "  C                   Movel     *on           TestUltFas
061000130410  "  c     K06Rci01      chain     NdRci01l
061100130410C2221C                   Endif
061200100408     c                   if        %Found(ndrci01l)
061300100413     c                             and RciAzione <> *blanks
061400130409C2221C*
061500130409  "  C                   If        TestUltFas = *on
061600130409  "  C                   exsr      RepParImp
061700130409C2221C                   endif
061800100409     c*
061900130410C2221C                   If        ($Ruf = *on and TestUltFas=*on)
062000130410  "  C                             or TestUltFas = *off
062100130409C2221C
062200140312     c                   if        calcgg057 = *blanks
062300100409     c                   if        RciTpData='1'
062400100409     c     ScaDtSca      adddur    RCIGgDts:*D   WrkDtSca
062500100409     c                   else
062600100409     c     RcaDtChiu     adddur    RCIGgDts:*D   WrkDtSca
062700100409     c                   endif
062800140312     c                   ELSE
062900140312     c                   if        RciTpData='1'
063000140312     c                   eval      WrkDtSca = ScaDtSca
063100140312     c                   else
063200140312     c                   eval      WrkDtSca = RcaDTChiu
063300140312     c                   endif
063400140312     c                   exsr      CallX17
063500140312     c                   endif
063600100409     c*
063700100408     c                   if        WrkDtsca <=DtRifL90
063800100408     c                   exsr      ScriveRcaA
063900130409     c                   endif
064000130409C2221C*
064100130409  "  C                   EndIf
064200130409C2221C*
064300100408     c                   endif
064400100408     c* fine stato='1'
064500100426     c                   endsl
064600100408     c* fine lettura attività
064700100408     c                   endif
064800100408     c                   enddo
064900100408     c*
065000100408     c                   endsr
065100100331     c*
065200140312D0948C************************************************************
065300140312  "  C* Chiamo il calendario bancario
065400140312  "  C************************************************************
065500140312D0948C     CallX17       BEGSR
065600140312      *
065700140312     C                   Reset                   X17ClValDS
065800140312     c                   Eval      X17DatRif   = WrkDtSca
065900140312     C                   Eval      X17Calend   = 'CB'
066000140312     C                   Eval      X17CalDtV   = CalcGG057
066100140312     C                   Eval      X17GGPiu    = RCIGgDts
066200140312     C                   Eval      X17GGMeno   = 0
066300140312     C                   CALL      'X17CLVAL'
066400140312     C                   PARM                    X17CLVALDS
066500140312     C*
066600140312     C                   if        X17Errore  = 0
066700140312     c                   eval      WrkDtSca = X17DatVal
066800140312     C                   endif
066900140312     C*
067000140312D0948C                   ENDSR
067100140312D0948C************************************************************
067200140312  "  C* Chiamo il calendario bancario
067300140312  "  C************************************************************
067400140312D0948C     CallX17meno   BEGSR
067500140312      *
067600140312     C                   Reset                   X17ClValDS
067700140312     c                   Eval      X17DatRif   = WrkDtSca
067800140312     C                   Eval      X17Calend   = 'CB'
067900140312     C                   Eval      X17CalDtV   = CalcGG057
068000140312     C                   Eval      X17GGPiu    = 0
068100140312     C                   Eval      X17GGMeno   = RCIGgDts
068200140312     C                   CALL      'X17CLVAL'
068300140312     C                   PARM                    X17CLVALDS
068400140312     C*
068500140312     C                   if        X17Errore  = 0
068600140312     c                   eval      WrkDtSca = X17DatVal
068700140312     C                   endif
068800140312     C*
068900140312D0948C                   ENDSR
069000100331     C********************************************************************
069100100331     C* Scrive segnalazione DA AVVISI
069200100331     C***********************************************************
069300100331     C     ScriveRcaA    BEGSR
069400100331     C*
069500100331     c                   clear                   NdRCA000
069600100331     c                   eval      RcaSocieta=SocietaL90
069700130906C2226X** EX D2680        if        GesUni057='D'
069800130906C2226C                   if        GesUniR057='D'
069900100331     c                   eval      RcaUnita  =ScAUnipar
070000110407D2680c                   endif
070100100331     c                   eval      RcaKcc    =ScaKcc
070200100331     c                   eval      RcaKsc    =ScaKsc
070300100331     c                   eval      RcaPosiz  =RciPosiz
070400100331     c                   eval      RcaAttrib =RciAttrib
070500100331     c                   eval      RcaFase   =RciFase
070600100331     c                   eval      RcaAzione =RciAzione
070700100331     c                   eval      RcaGrute  =RciGrute
070800100331     c                   eval      RcaBlk    =RciBlk
070900100331     c                   eval      Rcadtpar  =ScaDtPar
071000100331     c                   eval      RcaNrpar  =ScaNrPar
071100100331     c                   eval      RcaSerPar =ScaSerPar
071200100331     c                   eval      RcaDtSca  =ScaDtSca
071300100331     c                   eval      RcaImporto=ScaImporto
071400100331     c                   eval      RcaImportd=ScaImportD
071500100419     c                   eval      RcaSegno  =ScaSegno
071600100331     c                   eval      Rcadivisa =ScaDivisa
071700100331     c                   eval      RcakscAge =ScaKscAge
071800100331     c                   eval      RcaDtAtt  =Zdateiso
071900100331     c                   eval      RcaMdRd =ScaMdRd
072000100331     c                   eval      RcaMdPag=ScaMdPag
072100100331     c                   eval      RcaAnn=ScaAnn
072200100331     c                   eval      RcaCodAnn=ScaCodAnn
072300130410C2221c                   eval      RcaProgr =$Progr
072400100331     c*
072500110421C2184c* scrivo attività se ammesse da parametro modulo
072600110421  "  c                   if
072700110421  "  c* avviso
072800110421  "  c                             (Attivit47='A'
072900110421  "  c                             and AvvSca057=*on)
073000110421  "  c                             OR
073100110421  "  c* sollecito
073200110421  "  c                             (Attivit47='S'
073300110421  "  c                             and Sollec057=*on)
073400110421  "  c                             OR
073500110421  "  c* impegno e impegno scaduto
073600110421  "  c                             ((Attivit47= 'I'or
073700110421  "  c                              (Attivit47= 'X') and
073800110421  "  c                               Impegn057=*on))
073900110421  "  c* tutte le altre
074000110421  "  c                             OR
074100110421  "  c                             (Attivit47<>'A' and
074200110421  "  c                              Attivit47<>'S' and
074300110421  "  c                              Attivit47<>'I' and
074400110421C2184c                              Attivit47<>'X' )
074500131104     c**                 write     NdRca000                             23
074600131104     c                   write     NdRca000
074700110426C2184c                   endif
074800100331     c*
074900100331     c* se indicato il blocco rata, aggiorna file
075000100331     c                   if        RciBlk   <> *blanks
075100100331     c                   exsr      PutbloRat
075200100331     c                   endif
075300100331     c*
075400100331     c                   endsr
075500100426     C********************************************************************
075600100426     C* Test impegni per scrivere impegno scaduto
075700100426     C***********************************************************
075800100426     C     TestImpegni   BEGSR
075900100525     c* rileggo dall'ultima attività all'indietro perchè ci potrebbe
076000100525     c* ro essere + fasi impegno aperte
076100100525     c                   eval      RcaSocieta=SocietaL90
076200131104     c                   eval      RcaPosiz  =PosAttr057
076300100525     c                   eval      RcaKcc    =SclKcc
076400100525     c                   eval      RcaKsc    =SclKsc
076500100525     c                   eval      Rcadtpar  =SclDrp
076600100525     c                   eval      RcaNrpar  =SclNrp
076700100525     c                   eval      RcaSerPar =SclSerPar
076800100525     c                   eval      RcaDtSca  =SclDsc
076900131104     c***                eval      RcaDtAtt  = *hival
077000131104     c***  k09Rca04      setll     NdRca04L
077100131210     c     k08Rca04      setgt     NdRca04L
077200100525     c                   do        *hival
077300131104     c***  k08Rca04      reade     NdRca04L
077400131210     c     k08Rca04      readpe    NdRca04L
077500131104     c***                if        %eof(ndrca04l)
077600131210     c                   if        %eof(ndrca04l)
077700100525     c                   leave
077800100525     c                   endif
077900131211     C* se diverso da 98/99 iter
078000131211     c                   if        RcaAzione <> '98'
078100131211     c                             and RcaAzione <> '99'
078200131211     c                   iter
078300131211     c                   endif
078400100525     C* al primo stato <> da blank. esco dal loop perchè devo leggere
078500100525     c* solo impegni aperti
078600100525     c                   if        RcaStato <> ' '
078700131211     c                   iter
078800131211     c****               leave
078900100525     c                   endif
079000100525     C* reperisce il relativo impegno
079100100426     C                   EVAL      IMPSocieta = rcasocieta
079200100426     C                   EVAL      IMPKcc =   rcakcc
079300100426     C                   EVAL      IMPKsc =   rcaksc
079400100426     C                   EVAL      IMPDtPar =  rcaDtpar
079500100426     C                   EVAL      IMPNrPar =  rcanrpar
079600100426     C                   EVAL      IMPSerPar = rcaserpar
079700100426     C                   EVAL      IMPDtSca =  rcadtsca
079800100525     c                   eval      ImpProgr =  rcaProgrIm
079900100426     C*
080000100525     C     K08IMP01      chain     NDIMP01L
080100100525     c                   if        %found(ndimp01l)
080200100426     c* testo data scadenza impegno
080300100426     c* aggiungo giorni di tolleranza
080400100507     c                   adddur    GGtol057:*D   impdtimp
080500100426     c* se non ancora scaduto, non si fa nulla
080600130607C2225C* se non ancora scaduto ma con azione 99 devo modificare
080700100507     c                   if        impdtimp > dtrifl90
080800130607C2225c                   if        RcaAzione='99'
080900130607  "  c                   eval      RcaAzione='98'
081000131104  "  c***                update    NDRCA04                              23
081100131210brt  c                   update    NDRCA04                              23
081200130607C2225C                   endif
081300100426     c                   else
081400100525     c* se scaduto l'azione 98 diventa 99=impegno scaduto
081500100525     c                   eval      RcaAzione='99'
081600131104     c***                update    NDRCA04                              23
081700131210brt  c                   update    NDRCA04                              23
081800100426     c                   endif
081900100426     c*
082000100426     c                   endif
082100100426     c*
082200100525     c                   enddo
082300100525     c*
082400100426     c                   endsr
082500100330     C******************************************************************
082600100331     c* lettura file di lavoro solleciti e scrittura attività RC
082700100330     C******************************************************************
082800100331     C     LeggiSOL      BEGSR
082900100316     c*
083000100408     C                   eval      Lettura='SOL'
083100100316     c                   clear                   SavKcc
083200100316     c                   clear                   SavKsc
083300100309     c*
083400100331     c                   eval      ScLCSI    =SocietaL90
083500100309     c*
083600100331     c     SclCSI        setll     NdScl01l
083700100309     c                   do        *hival
083800100331     c     SclCsi        reade     NdSCl01L
083900100331     c                   if        %eof(NDSCl01L)
084000100309     c                   leave
084100100309     c                   endif
084200100331     C* Escludo le partite a scadere perchè sono già state trattate
084300100331     c* dagli avvisi
084400100331     c                   if        SclDsc > DtRifL90
084500100331     c                   iter
084600100331     c                   endif
084700100420     C* Escludo le partite CON EFFETTI A rischio perchè non sono trattate
084800100420     c* in quanto impossibile: sono chiuse dallo scadenzario ma vengono
084900100420     c* estratte dai solleciti
085000110429c2184x********           if        %subst(sclflg:1:7) ='NDC0611'
085100110429c2184c                   if        SclTif <>*blanks and
085200110429     c                             SclDsc <= DtRifl90
085300100420     c                   iter
085400100420     c                   endif
085500100512     C* Escludo cliente, partita, rata e condizione di pagamento da    te
085600100518     c* non sollecitare  se richiesto da parametro modulo
085700100518     c                   if        DaNSol057=*off
085800100512     c                   if        %subst(sclflg:1:7) ='NDC0603' or
085900100512     c                             %subst(sclflg:1:7) ='NDC0604' or
086000100512     c                             %subst(sclflg:1:7) ='NDC0605' or
086100100512     c                             %subst(sclflg:1:7) ='NDC0606'
086200100512     c                   iter
086300100512     c                   endif
086400100518     c                   endif
086500100518     C* Escludo cliente con saldo avere se richiesto da parametro      te
086600100518     c* modulo
086700100518     c                   if        CliSav057=*off
086800100518     c                   if        %subst(sclflg:1:7) ='NDC0612'
086900100518     c                   iter
087000100518     c                   endif
087100100518     c                   endif
087200110428     c*
087300110429c2184C* Escludo partite con saldo avere                                te
087400110429  "  C                   IF        sclseg='A'
087500110429  "  c                   move      sclcau        causale           4
087600110429  "  C                   exsr      NDMVC003
087700110429  "  C* ...che non derivano da nota credito, effetti e altri titoli.
087800110429  "  C     OPETPC        IFNE      '1'
087900110429  "  C     OPEPORTAF     ANDNE     '1'
088000110429  "  C     OPECALSCAD    ANDNE     '2'
088100110429  "  C                   iter
088200110429  "  c                   endif
088300110429  "  c*
088400110429  "  C* Escludo note di credito
088500110429  "  C     opetpc        IFeq      '1'
088600110429  "  c                   iter
088700110429  "  c                   endif
088800110429c2184c                   endif
088900100317     c*
089000100316     c* reperisco l'attributo del sottoconto al cambio di sottoc.
089100100331     c                   if        Sclksc <> Savksc
089200100316     C* se valorizzata posizione attr. nel param. modulo solleciti
089300100316     c                   if        Posattr057 <> *blanks
089400100331     c                   move      Sclkcc        $kcc
089500100331     c                   move      Sclksc        $ksc
089600100316     c                   exsr      Callmvc002
089700100407     c                   move      Posattr057    RcaPosiz
089800100407     c                   move      Posattr057    RciPosiz
089900100407     c                   move      WrkAttr       RcaAttrib
090000100407     c                   move      WrkAttr       RciAttrib
090100100316     c                   endif
090200100331     c                   move      Sclksc        Savksc
090300100316     c                   endif
090400100407     c*
090500100408     c* LEGGO ULTIMA ATTIVITÀ PRESENTE PER LA SCADENZA
090600130409C2221C                   Movel     *Off          TestUltFas
090700130410  "  C                   Movel     *Off          $Ruf
090800130410C2221C                   Movel     *Off          $Progr
090900100407     c                   eval      RcaSocieta=SocietaL90
091000100407     c                   eval      RcaKcc    =SclKcc
091100100407     c                   eval      RcaKsc    =SclKsc
091200100407     c                   eval      Rcadtpar  =SclDrp
091300100407     c                   eval      RcaNrpar  =SclNrp
091400100407     c                   eval      RcaSerPar =SclSerPar
091500100407     c                   eval      RcaDtSca  =SclDsc
091600131031     c***                eval      RcaDtAtt  = *hival
091700131031     c**   k09Rca04      setll     NdRca04L
091800131031     c**   k08Rca04      reade     NdRca04L
091900131210     c     k08Rca01      setgt     NdRca04L
092000131210     c     k08Rca01      readpe(N) NdRca04L
092100131211     c                   if        %eof(NdRca04l)
092200131211     c                   clear                   test              1
092300131211     c                   else
092400131211     c                   clear                   test              1
092500131211     c                   endif
092600100512     C* se rata pagata, non faccio nulla e leggo altra rata
092700131031D2755C***                If        not %eof(Ndrca04l)
092800131210D2755C                   If        not %eof(Ndrca04l)
092900100512     c                   if        RcaStato ='2'
093000131212     c******             iter
093100100512     c                   endif
093200121031D2755c                   endif
093300100407     c* se non cè nessuna attività, aggancio impostazioni
093400131031     c*****              if        %eof(NdRca04l)
093500131210     c                   if        %eof(NdRca04l)
093600100407     c                   exsr      LeggiImpo
093700100407     c                   else
093800100426     c* se la fase è aperta, non si fa nulla, a meno che non  si tratti
093900100525     c* di un impegno codice '98'
094000130607C2225c* o di un impegno con codice '99' se cambia la data lo devo
094100130607C2225c* riportare a '98'
094200100426     c                   select
094300100511     c                   when      RcaAzione ='98'
094400131219     c                             and RcaStato = ' '
094500130607C2225C                             or RcaAzione = '99'
094600131219     c                             and RcaStato = ' '
094700100426     c                   exsr      TestImpegni
094800100407     c* se la fase è chiusa, aggiungo i giorni indicati nelle
094900100408     c* impostazioni alla data attività e se la data risulta <= alla
095000100407     c* data del giorno, scrive la segnalazione della fase successiva
095100100512     C* da impostazioni
095200100426     c                   when      RcaStato ='1'
095300130409C2221X**** EX D2762                and Rcafase < 9
095400131212     c                             or RcaStato ='2'
095500100407     c                   eval      RciSocieta=SocietaL90
095600130916C2226c                   clear                   RciUnita
095700130916C2226C                   if        GesUniI057='D'
095800100407     c                   eval      RciUnita  =SclDva
095900130916C2226C                   endif
096000100407     c                   eval      RciKcc    =SclKcc
096100100407     c                   eval      RciKcc    =SclKcc
096200121213D2762C                   eval      Rciattrib =RcaAttrib
096300130409C2221C                   If        RcaFase < 9
096400100408     c                   eval      Rcifase   =RcaFase +1
096500130409C2221C                   Else
096600130409  "  C                   Movel     *on           TestUltFas
096700130409  "  C                   eval      Rcifase   =RcaFase
096800130410  "  C                   eval      $Progr    =RcaProgr + 1
096900130409C2221C                   Endif
097000100407     c     K06Rci01      chain     NdRci01l
097100130410C2221C                   If        Not %Found(ndrci01l) or
097200130410  "  C                             (%Found(ndrci01l) and
097300130410  "  C                             RCIAzione = *blanks)
097400130410  "  C                   eval      RciFase = RcaFase
097500130410  "  C                   eval      $Progr    =RcaProgr + 1
097600130410  "  C                   Movel     *on           TestUltFas
097700130410  "  c     K06Rci01      chain     NdRci01l
097800130410C2221C                   Endif
097900100407     c                   if        %Found(ndrci01l)
098000100413     c                             and rciAzione <> *blanks
098100130409C2221C*
098200130409  "  C                   If        TestUltFas = *on
098300130409  "  C                   exsr      RepParImp
098400130409C2221C                   endif
098500100409     c*
098600130410C2221C                   If        ($Ruf = *on and TestUltFas = *on)
098700130410  "  C                             or TestUltFas = *off
098800130409C2221C
098900140312     c                   if        calcgg057 = *blanks
099000100409     c                   if        RciTpData='1'
099100140312     c     SclDsc        adddur    RCIGgDts:*D   WrkDtSca
099200100409     c                   else
099300100409     c     RcaDTChiu     adddur    RCIGgDts:*D   WrkDtSca
099400100409     c                   endif
099500140312     c                   ELSE
099600140312     c                   if        RciTpData='1'
099700140312     c                   eval      WrkDtSca = SclDsc
099800140312     c                   else
099900140312     c                   eval      WrkDtSca = RcaDTChiu
100000140312     c                   endif
100100140312     c                   exsr      CallX17
100200140312     c                   endif
100300100409     c*
100400100408     c                   if        WrkDtsca <=DtRifL90
100500100407     c                   exsr      ScriveRcaS
100600100407     c                   endif
100700130409C2221C*
100800130409  "  C                   EndIf
100900130409C2221C*
101000100408     c                   endif
101100100408     c* fine stato='1'
101200100426     c                   endsl
101300100408     c* fine lettura attività
101400100407     c                   endif
101500100407     c                   enddo
101600100407     c*
101700100407     c                   endsr
101800100407     C***********************************************************
101900100407     C* Leggi impostazioni se non esiste nessuna attività
102000100408     C* scrive PRIMA FASE
102100100407     C***********************************************************
102200100407     C     LeggiImpo     BEGSR
102300100407     c*
102400100407     c* aggancio fasi per capoconto/unita
102500100407     c                   eval      RciSocieta=SocietaL90
102600100408     c                   if        Lettura='AVV'
102700110407D2680c                   clear                   RciUnita
102800130906C2226X** EX D2680        if        GesUni057='D'
102900130906C2226C                   if        GesUniI057='D'
103000100408     c                   eval      RciUnita  =ScaUniPar
103100110407D2680c                   endif
103200100408     c                   eval      RciKcc    =ScaKcc
103300100408     c                   endif
103400100408     c                   if        Lettura='SOL'
103500130916C2226c                   clear                   RciUnita
103600130916C2226C                   if        GesUniI057='D'
103700100408     c                   eval      RciUnita  =SclDva
103800130916C2226c                   endif
103900100408     c                   eval      RciKcc    =SclKcc
104000100408     c                   endif
104100110428     c*
104200110428c2184c* vedo se esistono le impostazioni con attributo, altrimenti
104300110428  "  c* legge senza attributo
104400110428  "  c                   if        Rciattrib<>*blanks
104500110428  "  c     K05Rci01      chain     NdRci01l
104600110428  "  c                   if        not %found(ndrci01l)
104700110428  "  c                   eval      Rciattrib=*blanks
104800110428  "  c                   endif
104900110428c2184c                   endif
105000100407     c*
105100100407     c     K05Rci01      setll     NdRci01l
105200100407     c                   do        *hival
105300100407     c     K05Rci01      reade     NdRci01l                               21
105400100407     c   21              leave
105500100413     C*
105600100413     C                   IF        rciAzione = '  '
105700100413     c                   leave
105800100413     c                   endif
105900100512     c*
106000110401D2680C                   clear                   WrkDtSca
106100100512     c* reperisco tipo attività dell'azione
106200100512     c                   reset                   XatBds
106300100512     C                   EVAL      XtbRic = '1'
106400100512     C                   EVAL      XtbAzi = SocietaL90
106500100512     C                   EVAL      XtbKey = *zero
106600100512     C                   EVAL      XtbCod = '047'
106700100512     C                   MOVE      RciAzione     XtbKey
106800100512     C                   CALLB     'XATB'
106900100512     C                   PARM                    XAtbDS
107000100512     c                   if        xtbErr = *off
107100100512     C                   EVAL      ANG047DS = XtbUni
107200130916     c*
107300100512     c* se azione avviso e sto leggendo gli avvisi
107400100408     c                   if        Lettura='AVV' and
107500100512     c                             (Attivit47 =  'A')
107600100408     c* tolgo i gg indicati alla data scadenza della partita in
107700100409     c* lettura  (per forza la data scadenza in quanto non c'è una
107800100409     c* fase precedente)
107900140312     c                   if        calcgg057 = *blanks
108000110401D2680c                   if        RciTpData='1'
108100100408     c     ScaDtSca      subdur    RCIGgDts:*D   WrkDtSca
108200110401D2680c                   endif
108300140312     c                   else
108400140312     c                   eval      WrkDtSca = ScaDtSca
108500140312     c                   exsr      CallX17meno
108600140312D2680c                   endif
108700100408     c                   endif
108800100408     c*
108900100512     c* se azione sollecito/telefonata/e-mail e sto leggendo i sollecit
109000100408     c                   if        Lettura='SOL' and
109100100512     c                             (Attivit47 =  'S'  or
109200100512     c                              Attivit47 =  'T'  or
109300100512     c                              Attivit47 =  'E'  or
109400100512     c                              Attivit47 =  'L')
109500100407     c* aggiungo i gg indicati alla data scadenza della partita in
109600100409     c* lettura  (per forza la data scadenza in quanto non c'è una
109700100409     c* fase precedente)
109800140312     c                   if        calcgg057 = *blanks
109900110401D2680c                   if        RciTpData='1'
110000100407     c     SclDsc        adddur    RCIGgDts:*D   WrkDtSca
110100110401D2680c                   endif
110200140312     c                   else
110300140312     c                   eval      WrkDtSca = SclDsc
110400140312     c                   exsr      CallX17
110500140312D2680c                   endif
110600100408     c                   endif
110700100407     c*
110800100408     c                   if        (Lettura='AVV' and
110900100512     c                             Attivit47 =  'A'  )
111000100408     C                                    or
111100100408     c                             (Lettura='SOL' and
111200100512     c                             (Attivit47 =  'S' or
111300100512     c                              Attivit47 =  'T' or
111400100512     c                              Attivit47 =  'E' or
111500100512     c                              Attivit47 =  'L'))
111600100408     c* se la data calcolata è <=della data di rif., si scrive la
111700100407     c* azione indicata dalla fase
111800100408     c                   if        WrkDtsca <=DtRifL90
111900110401D2680c                             and WrkDtsca <> *loval
112000100408     c                   if        Lettura='AVV'
112100100408     c                   exsr      ScriveRCAA
112200100408     c                   endif
112300100408     c                   if        Lettura='SOL'
112400100408     c                   exsr      ScriveRCAS
112500100408     c                   endif
112600100407     c                   leave
112700100407     C                   endif
112800100512     C                   endif
112900100407     c*
113000100512     C                   endif
113100100407     c                   enddo
113200100407     c*
113300100407     c                   endsr
113400100407     C*
113500100316     C********************************************************************
113600100331     C* Scrive segnalazione DA SOLLECITI
113700100316     C***********************************************************
113800100331     C     ScriveRcaS    BEGSR
113900100316     C*
114000100316     c                   clear                   NdRCA000
114100100316     c                   eval      RcaSocieta=SocietaL90
114200130917C2226C                   If        GesUniR057 = 'D'
114300130917  "  C                   If        GesUniI057 <> 'D'
114400130917  "  c                   eval      RcaUnita  =SclUniPar
114500130917C2226C                   Else
114600100331     c                   eval      RcaUnita  =SclDVA
114700130917C2226C                   Endif
114800130917C2226C                   Endif
114900100331     c                   eval      RcaKcc    =SclKcc
115000100331     c                   eval      RcaKsc    =SclKsc
115100100319     c                   eval      RcaPosiz  =RciPosiz
115200100319     c                   eval      RcaAttrib =RciAttrib
115300100319     c                   eval      RcaFase   =RciFase
115400100319     c                   eval      RcaAzione =RciAzione
115500100319     c                   eval      RcaGrute  =RciGrute
115600100319     c                   eval      RcaBlk    =RciBlk
115700100331     c                   eval      Rcadtpar  =SclDrp
115800110419     c                   eval      RcaNrpar  =SclNrp
115900100331     c                   eval      RcaSerPar =SclSerPar
116000100331     c                   eval      RcaDtSca  =SclDSc
116100100331     c                   eval      RcaImporto=SclImp
116200100331     c                   eval      RcaImportd=SclImD
116300100419     c                   eval      RcaSegno  =SclSeg
116400130410C2221C                   eval      RcaProgr  =$Progr
116500100331     c* per reperire divisa e agente vado su NDEPA
116600100331     c                   eval      EpaCsi    =SocietaL90
116700100331     c                   eval      EpaKcc    =RCAKcc
116800100331     c                   eval      EpaKsc    =RCAKsc
116900100331     c                   eval      EpaDrp    =RCADtpar
117000100331     c                   eval      EpaNrp    =RCANrpar
117100100331     c                   eval      EpaSerPar =RCASerPar
117200100331     c     K06Epa01      Chain     NdEpa01l
117300100331     c                   if        %found(NDepa01L)
117400100331     c                   eval      Rcadivisa =EpaDiv
117500100331     c                   eval      RcakscAge =EpaSap
117600110505C2184c* se estrazione automatica solleciti, metto la gravità partita
117700110505  "  c* e testo indicata nelle impostazioni attività rec. credito
117800110421  "  c                   if        EsAutom057=*on
117900110421  "  C                   eval      EpaGra=%subst(RciFlag:1:1)
118000110421  "  C                   eval      EpaTsl=%subst(RciFlag:1:1)
118100110426  "  C                   eval      EpaTtx=%subst(RciFlag:2:2)
118200110421  "  c                   endif
118300110421  "  c* aggiorno ugualmente altrimenti rimane allocato
118400110419c2184c                   update    ndepa000
118500100324     C                   endif
118600100325     c                   eval      RcaDtAtt  =Zdateiso
118700130916     c                   eval      RcaMdRd =SclTpg
118800100331     c                   eval      RcaMdPag=SclTif
118900100331     c                   eval      RcaAnn=SclAnn
119000100331     c                   eval      RcaCodAnn=SclFlg
119100100316     c*
119200110421C2184c* scrivo attività se ammesse da parametro modulo
119300110421  "  c                   if
119400110421  "  c* avviso
119500110421  "  c                             (Attivit47='A'
119600110421  "  c                             and AvvSca057=*on)
119700110421  "  c                             OR
119800110421  "  c* sollecito
119900110421  "  c                             (Attivit47='S'
120000110421  "  c                             and Sollec057=*on)
120100110421  "  c                             OR
120200110421  "  c* impegno e impegno scaduto
120300110421  "  c                             ((Attivit47= 'I'or
120400110421  "  c                              (Attivit47= 'X') and
120500110421  "  c                               Impegn057=*on))
120600110421  "  c* tutte le altre
120700110421  "  c                             OR
120800110421  "  c                             (Attivit47<>'A' and
120900110421  "  c                              Attivit47<>'S' and
121000110421  "  c                              Attivit47<>'I' and
121100110421C2184c                              Attivit47<>'X' )
121200131104     c**                 write     NdRca000                             23
121300131104     c                   write     NdRca000
121400110421c2184c                   endif
121500100317     c*
121600100317     c* se indicato il blocco rata, aggiorna file
121700100317     c                   if        RciBlk   <> *blanks
121800100317     c                   exsr      PutbloRat
121900100317     c                   endif
122000100316     c*
122100100316     c                   endsr
122200130409C2221C/EJECT
122300130409C2221 *----------------------------------------------------
122400130409  "   * Reperisco parametri da impostazione
122500130409  "   *----------------------------------------------------
122600130409C2221C     RepParImp     BEGSR
122700130409     C*
122800130410     C                   Movel     *Off          $Ruf
122900130409     C*
123000130409     C                   Eval      RCPSocieta = RciSocieta
123100130409     C                   Eval      RCPUnita   = RciUnita
123200130409     C                   Eval      RCPKcc     = RciKcc
123300130409     C                   Eval      RCPPosiz   = RciPosiz
123400130409     C                   Eval      RCPAttrib  = RciAttrib
123500130409     C     K05RCP01      Chain     NdRCP01L
123600130410     C                   IF        %found (NDRCP01L)
123700130410     C                   movel     RCPRuf        $Ruf
123800130409     C                   Endif
123900130409     C*
124000130409C2221C                   Endsr
124100130409C2221C/EJECT
124200100317     C************************************************************
124300100317     C* Mette blocco rata
124400100317     C************************************************************
124500100317     C     PutBloRat     BEGSR
124600131204     c                   leavesr
124700100317     c*
124800100317     c* prima reperisco nrrata da ndPas
124900100521     C                   move      RcaSocieta    PasSocieta
125000100521     C                   move      'CG'          PasCtb
125100100521     C                   move      Rcakcc        PasKcc
125200100521     C                   move      Rcaksc        PasKsc
125300100521     C                   move      RcaDtPar      PasDtPar
125400100521     C                   move      RcaNrPar      PasNrPar
125500100521     C                   move      RcaSerPar     PasSerPar
125600100521     C                   move      RcaDtSca      PasDtSca
125700100317     C     k08Pas03      chain     ndPas03l
125800100317     c* se trovo pas e c'è il numero rata, metto blocco su rata
125900100317     c                   if        %found(ndpas03l)
126000100317     C                             and pasnrrata <> 0
126100100521     C* aggancio ppa per avere pparasint
126200100521     c                   eval      ppasocieta=rcasocieta
126300100521     c                   eval      ppactb    ='CG'
126400100521     c                   eval      ppakcc    =rcakcc
126500100521     c                   eval      ppaksc    =rcaksc
126600100521     c                   eval      ppadtpar  =rcadtpar
126700100521     c                   eval      ppanrpar  =rcanrpar
126800100521     c                   eval      ppaserpar =rcaserpar
126900100521     C     k07Ppa01      chain     ndPpa01l
127000100521     c                   if        %found(ndppa01l)
127100100521     c                   eval      PscSys = PPasys
127200100521     c                   eval      PscNrasInt=PPaNrasint
127300100521     c                   eval      PscDtSca  =RcaDtSca
127400100317     c     K03Psc01      chain     NdPsc01l
127500100317     c                   if        %found(NdPsc01l)
127600100317     c                   eval      PscBKAmm  = RciBlk
127700100317     c                   eval      PscCliSoll=*on
127800100317     C                   update    NdPsc000
127900100317     c                   else
128000100317     c                   eval      PscBKAmm  = RciBlk
128100100317     c                   eval      PscCliSoll=*on
128200100317     c                   eval      PscNrRata =PasNrRata
128300100317     C                   write     NdPsc000
128400100317     c                   COMMIT
128500100317     c                   endif
128600100521     c                   endif
128700100317     C*
128800100317     c* se non c'è pas metto il blocco sulla partita
128900100317     c                   ELSE
129000100317     C*
129100100317     c                   eval      PPSSys = Scdsys
129200100317     c                   eval      PPSNrasInt=ScdNrasint
129300100317     c     K02Pps01      chain     NdPps01l
129400100317     c                   if        %found(ndpps01l)
129500100317     c                   eval      PpsBKAmm  = RciBlk
129600100317     c                   eval      PpsCliSoll=*off
129700100317     c                   update    ndpps000
129800100317     c                   endif
129900100317     c*
130000100317     c                   endif
130100100317     C*
130200100317     c                   ENDSR
130300100317     C**************************************************************
130400100317     C* Lettura attività RC e stampa per fase per i gruppi di utenti
130500100317     C**************************************************************
130600100317     C     StampaRCA     BEGSR
130700100317     c*
130800100318     c                   move      SocietaL90    RcaSocieta
130900100318     c                   move      Posattr057    RcaPosiz
131000100319     c                   clear                   SavUnita
131100100319     c                   clear                   Savkcc
131200100513     c                   clear                   Savksc
131300100319     c                   clear                   SavAttrib
131400100421     c                   clear                   SavProgr
131500110401D2680c                   clear                   SavAzione
131600100319     c*
131700100318     c                   DO        9             $X                3 0
131800100318     C                   Z-Add     $x            RcaFase
131900100318     c* leggo per fase
132000100325     c* la v.l. omette le annullate/chiuse
132100130607C2225X***  K03Rca02      setll     NdRcA02l
132200130607C2225C     K03Rca07      setll     NdRcA07l
132300110721D2703c                   move      *off          $rca              1
132400130604C2225X*** EX D2703       if        %found(ndrca02l)
132500130604C2225X*** EX D2703       move      *on           $rca
132600130604C2225X*** EX D2703       endif
132700100319     c                   do        *hival
132800130607C2225X***  k03Rca02      reade     NdRCA02L
132900130607C2225X***                if        %eof(NDRCA02L)
133000130607C2225c     k03Rca07      reade     NdRCA07L
133100130607C2225c                   if        %eof(NDRCA07L)
133200100319     c                   leave
133300100319     c                   endif
133400130604C2225C* Esclusa l'azione degli impegni non scaduti (automatica 98)
133500130604  "  C* perchè non significativa.
133600130604  "  C                   If        RcaAzione = '98'
133700130604  "  C                   Iter
133800130604C2225C                   Endif
133900100319     c* salvo campi rottura (primo record letto)
134000100319     c                   if        savkcc=*blanks
134100100318     c                   eval      SavUnita =RcaUnita
134200100318     c                   eval      Savkcc   =Rcakcc
134300100513     c                   eval      Savksc   =Rcaksc
134400100318     c                   eval      SavAttrib=RcaAttrib
134500130604C2225C* Escludo il progressivo, lo inserisco nel dettaglio
134600130604C2225X****               eval      SavProgr=RcaProgr
134700110401D2680c                   eval      SavAzione=RcaAzione
134800130604C2225C                   Movel     *on           $RCA
134900100319     c                   endif
135000100513     c*
135100100513     c* al cambio di cliente, stampo il totale del cliente precedente
135200100513     c                   if        RcaUnita=SavUnita and
135300100513     c                             Rcakcc=Savkcc     and
135400100513     c                             RcaAttrib=SavAttrib and
135500130604C2225X****                         RcaProgr =SavProgr  and
135600100513     c                             Rcaksc <> Savksc
135700110401D2680c                             and RcaAzione=SavAzione
135800130915C2226C* Solo se richiesto nel parametro modulo
135900130915C2226C                   If        StpAtt057 = *on
136000100513     c                   write     TotCli
136100130915C2226C                   Endif
136200100513     c                   clear                   Totcli
136300100513     c                   eval      Savksc=Rcaksc
136400100513     c                   endif
136500100319     c*
136600100421     c* se i 4 campi sono uguali, stampa
136700100318     c                   if        RcaUnita=SavUnita and
136800100318     c                             Rcakcc=Savkcc     and
136900130604C2225C                             RcaAttrib=SavAttrib
137000130604C2225X****                         RcaAttrib=SavAttrib and
137100130604C2225X****                         RcaProgr =SavProgr
137200110401D2680c                             and RcaAzione=SavAzione
137300100318     c                   exsr      Stampa
137400100319     c*
137500100318     c                   else
137600100319     c*
137700100421     c* al cambio di uno dei 4 campi
137800100513     c* stampo prima il totale cliente precedente
137900100513     c                   if        T1imprmc <> 0
138000130915C2226C* Solo se richiesto nel parametro modulo
138100130915C2226C                   If        StpAtt057 = *on
138200100513     c                   write     TotCli
138300130915C2226c                   endif
138400100513     c                   clear                   Totcli
138500100513     c                   endif
138600100513     c*
138700100319     c* mando agli utenti indicati nelle impostazioni
138800130915C2226C* solo se la stampa attività è attiva nel parametro modulo
138900130915C2226C                   If        StpAtt057 = *on
139000100419     c                   close     ndcl91P
139100100518     c                   exsr      Invia
139200130915C2226C                   Endif
139300100324     c                   clear                   Primatest
139400130915C2226C                   If        StpAtt057 = *on
139500100419     c                   open      ndcl91P
139600130915C2226C                   Endif
139700100513     c                   eval      SavUnita =RcaUnita
139800100513     c                   eval      SavKcc   =Rcakcc
139900100513     c                   eval      SavAttrib=RcaAttrib
140000110401D2680c                   eval      SavAzione=RcaAzione
140100130604C2225X****               eval      SavProgr =RcaProgr
140200100513     c                   eval      SavKsc   =Rcaksc
140300100513     c* rifaccio setll per rileggere ultimo record
140400110401D2680x***  K08Rca02      setll     NdRcA02l
140500130607C2225x*** EX D2680  K09Rca02      setll     NdRcA02l
140600130607C2225c     K09Rca07      setll     NdRcA07l
140700100318     c                   endif
140800100317     c*
140900100318     C                   enddo
141000100513     c* stampo totale ultimo cliente
141100100513     c                   if        T1imprmc <> 0
141200130915C2226C* Solo se richiesto nel parametro modulo
141300130915C2226C                   If        StpAtt057 = *on
141400100513     c                   write     TotCli
141500130915C2226c                   endif
141600100513     c                   clear                   Totcli
141700100513     c                   endif
141800100513     c* per la prox fase
141900130915C2226C                   If        StpAtt057 = *on
142000100419     c                   close     ndcl91P
142100110721D2703c                   if        $rca = *on
142200110721  "  c* se trovato almeno un record, eseguo invio per ultimo cliente
142300110721  "  c                   exsr      Invia
142400110721D2703c                   endif
142500130915C2226C                   Endif
142600100324     c                   clear                   Primatest
142700130915C2226C                   If        StpAtt057 = *on
142800100419     c                   open      ndcl91P
142900130915C2226C                   Endif
143000100317     C                   enddo
143100100317     c*
143200100317     c                   ENDSR
143300100319     C********************************************************************
143400100324     C* Stampa dettaglio dati attività recupero credito
143500100319     C***********************************************************
143600100319     C     Stampa        BEGSR
143700100324     c*
143800100324     c                   move      RcaUnita      T1Unita
143900100324     C                   MOVE      Rcakcc        T1Kcc
144000100324     c                   move      RcaPosiz      T1Posiz
144100100324     c                   move      RcaAttrib     T1Attrib
144200100324     c                   movel     RcaFase       T1Fase
144300130604C2225X*****              z-add     RcaProgr      T1Progr
144400130604C2225C                   z-add     RcaProgr      P1Progr
144500100324     c                   movel     RcaAzione     T1Azione
144600100324     c                   reset                   XatBds
144700100324     C                   EVAL      XtbRic = '1'
144800100324     C                   EVAL      XtbAzi = SocietaL90
144900100324     C                   EVAL      XtbKey = *zero
145000100324     C                   EVAL      XtbCod = '047'
145100100324     C                   MOVE      RcaAzione     XtbKey
145200100324     C                   CALLB     'XATB'
145300100324     C                   PARM                    XAtbDS
145400100324     c                   if        xtbErr = *off
145500100324     C                   EVAL      ANG047DS = XtbUni
145600100324     c                   eval      T1AzioneD=Des047
145700100324     c                   endif
145800100324     c                   movel     RcaGrUte      T1GrUte
145900100324     c*
146000100324     C                   MOVE      Rcaksc        P1Ksc
146100100324     C                   CLEAR                   P1kscD
146200100324     C* reperimento descrizione conto
146300100324     c                   move      Rcakcc        $kcc
146400100324     c                   move      Rcaksc        $ksc
146500100324     C                   EXSR      CallMVC002
146600100324  B1 C     Esito         ifeq      *ON
146700100324     C                   CLEAR                   ND002DS
146800100324     C                   else
146900100324     C                   movel     desbrev002    P1kscD
147000100324     c                   endif
147100100323     c*
147200100323     c     *jobrun       move      RcaDtPar      P1DtPar
147300100323     c                   z-add     RcaNrPar      P1NrPar
147400100323     c                   move      RcaSerPar     P1SerPar
147500100324     c     *jobrun       move      RcaDtSca      P1DtSca
147600100324     c                   eval      P1Blk   = RcaBlk
147700100324     c* tipo pagamento
147800100324     c                   clear                   P1DesTpg
147900100323     c                   if        rcaMdRd <> *blanks
148000100323     c                   move      Rcamdrd       P1Tpg
148100100324     c                   reset                   XatBds
148200100324     C                   EVAL      XtbRic = '1'
148300100324     C                   EVAL      XtbAzi = SocietaL90
148400100324     C                   EVAL      XtbKey = *zero
148500100331     C                   EVAL      XtbCod = 'G56'
148600100324     C                   MOVE      RcaMdRd       XtbKey
148700100324     C                   CALLB     'XATB'
148800100324     C                   PARM                    XAtbDS
148900100324     c                   if        xtbErr = *off
149000100324     C                   EVAL      ANGg56DS = XtbUni
149100100324     c                   eval      P1DesTpg=DesG56
149200100324     c                   endif
149300100323     c                   else
149400100323     c                   move      RcamdPag      P1Tpg
149500100324     c                   reset                   XatBds
149600100324     C                   EVAL      XtbRic = '1'
149700100324     C                   EVAL      XtbAzi = SocietaL90
149800100324     C                   EVAL      XtbKey = *zero
149900100331     C                   EVAL      XtbCod = 'G57'
150000100324     C                   MOVE      RcaMdPag      XtbKey
150100100324     C                   CALLB     'XATB'
150200100324     C                   PARM                    XAtbDS
150300100324     c                   if        xtbErr = *off
150400100324     C                   EVAL      ANGg57DS = XtbUni
150500100324     c                   eval      P1DesTpg=DesG57
150600100324     c                   endif
150700100323     c                   endif
150800100324     c*
150900100323     c                   move      RcaDivisa     P1Divisa
151000100323     c                   z-add     RcaImporto    P1ImpRmc
151100100323     c                   z-add     RcaImportD    P1ImpRD
151200100324     c                   move      Rcakscage     P1Agente
151300100324     c* decodifica agente
151400100324     c                   clear                   P1AgeDes
151500100324 b3  c                   if        p1agente  <> *blanks
151600100324     C                   RESET                   DVAGE
151700100324     C                   MOVE      '1'           DAGRIC
151800100324     C                   MOVE      RcaSocieta    DAGSOC
151900100324     C                   MOVE      RcaUnita      DAGUNI
152000100324     C                   MOVE      '1'           DAGVIS
152100100324     C                   MOVE      *OFF          DAGALC
152200100324     C                   MOVE      1             DAGNRK
152300100324     C                   CLEAR                   DAGDTA
152400100324     C                   MOVEL     P1agente      DAGKSC
152500100324     C                   MOVE      'DVOAGEDS'    DAGSET
152600100324     C                   MOVE      *OFF          DAGERR
152700100324     C                   CALL      'DVAGE'
152800100324     C                   PARM                    DVAGE
152900100324     C                   PARM                    DVOAGE
153000100324     C*
153100100324 b4  C     DAGERR        IFEQ      *OFF
153200100324     C                   MOVEL     AGERAGsoc     P1AgeDes
153300100324 e4  c                   endif
153400100324 e3  c                   endif
153500100324     c*
153600100324     c                   if        *in39=*on or PrimaTest=*blanks
153700130915C2226C* Solo se richiesto nel parametro modulo
153800130915C2226C                   If        StpAtt057 = *on
153900100324     c                   WRITE     Testa
154000130915C2226C                   Endif
154100100324     c                   move      *on           PrimaTest         1
154200100324     C                   move      *off          *in39
154300100324     c                   endif
154400100324     c*
154500100513     c*
154600130915C2226C* Solo se richiesto nel parametro modulo
154700130915C2226C                   If        StpAtt057 = *on
154800100324     c                   write     Dettaglio
154900130915C2226C                   Endif
155000100513     c                   add       P1ImpRmc      T1ImpRmc
155100100323     c*
155200100319     c                   ENDSR
155300100323     c*
155400100318     C********************************************************************
155500100318     C* Invia lo spool effettuato fin ora agli utenti indicati
155600100318     C***********************************************************
155700100318     C     Invia         BEGSR
155800100318     C*
155900100318     c* aggancio impostazioni
156000100318     c                   eval      RciSocieta=SocietaL90
156100110721D2703x****               eval      RciUnita  =RcaUnita
156200110721  "  x****               eval      RciKcc    =RcaKcc
156300110721  "  x****               eval      RcIPosiz  =RcaPosiz
156400110721  "  x****               eval      RcIAttrib =RcaAttrib
156500110721D2703x****               eval      RciFase   =RcaFase
156600110721D2703c                   eval      RciUnita  =SavUnita
156700110721  "  c                   eval      RciKcc    =SavKcc
156800110721  "  c                   eval      RcIPosiz  =PosAttr057
156900110721  "  c                   eval      RcIAttrib =SavAttrib
157000110721D2703c                   eval      RciFase   =$X
157100100318     c*
157200100318     c     K06Rci01      chain     NdRci01l
157300100318     c                   if        %found(ndRci01L)
157400100318     c                   if        RciGrute <> *blanks
157500100318     c                   reset                   XatBds
157600100318     C                   EVAL      XtbRic = '1'
157700100318     C                   EVAL      XtbAzi = SocietaL90
157800100318     C                   EVAL      XtbKey = *zero
157900100318     C                   EVAL      XtbCod = '048'
158000100318     C                   MOVE      RciGrute      XtbKey
158100100318     C                   CALLB     'XATB'
158200100318     C                   PARM                    XAtbDS
158300100318     c                   if        xtbErr = *off
158400100318     C                   EVAL      ANG048DS = XtbUni
158500100518     c                   call      'NDCL91C'
158600100518     C                   parm                    usr1048
158700100518     C                   parm                    usr2048
158800100518     C                   parm                    usr3048
158900100518     C                   parm                    usr4048
159000100518     C                   parm                    usr5048
159100100518     C                   parm                    usr6048
159200100518     C                   parm                    usr7048
159300100518     C                   parm                    usr8048
159400100318     c                   endif
159500100318     c                   endif
159600100318     c                   endif
159700100318     c*
159800100318     c                   ENDSR
159900100319     C/EJECT
160000100309     C********************************************************************
160100100309     C* Chiamata NDMVC002  controlla conto
160200100309     C***********************************************************
160300100309     C     CallMvc002    BEGSR
160400100309     C*
160500100309     C* Controllo conto
160600100309     C                   Clear                   ND002DS
160700100309     C                   EVAL      LenOut = %Size(ND002DS)
160800100309     C                   CallB     'NDMVC002'
160900100309     C                   PARM                    societaL90
161000100324     C                   PARM                    $kcc
161100100324     C                   PARM                    $ksc
161200100309     C                   PARM                    DataIso
161300100309     C                   PARM      *OFF          GesMsg
161400100309     C                   PARM      *OFF          Esito
161500100309     C                   PARM      'ND002DS'     StrutturaO
161600100309     C                   PARM                    ND002DS
161700100309     C                   PARM                    LenOut
161800100309     C*
161900100309     c                   clear                   WrkAttr           4
162000100309     c                   if         esito = *off and PosAttr057 <> *blanks
162100100309     c                   movel     Categorie     DsCateg
162200100309     c                   eval      WrkAttr  = SkCateg(Posiz)
162300100309     c                   endif
162400100309     C*
162500100309     C                   ENDSR
162600100309     c*
162700110429c2184c*---------------------------------------------------------------
162800110429  "  C* N D M V C 0 0 3     *                                              *
162900110429  "  c*---------------------------------------------------------------
163000110429c2184C     NDMVC003      BEGSR
163100110429     c*
163200110429     C                   CLEAR                   ANOPE
163300110429     C                   EVAL      LenOut = %Size(Anope)
163400110429     C                   CALLB     'NDMVC003'
163500110429     C                   PARM                    SocietaL90
163600110429     C                   PARM                    Causale
163700110429     C                   PARM      *LOVAL        DataIso
163800110429     C                   PARM      *OFF          USO               1
163900110429     C                   PARM      *OFF          GesMsg
164000110429     C                   PARM      *OFF          Esito
164100110429     C                   PARM      'ANOPE'       StrutturaO
164200110429     C                   PARM                    ANOPE
164300110429     C                   PARM                    LenOut
164400110429     C                   PARM      *OFF          Refresh
164500110429     c*
164600110429c2184C                   ENDSR
164700090422     C************************************************************
164800090422     C* Aggiorna o scrive blocco rata
164900090422     C************************************************************
165000090422     C     AggBloRat     BEGSR
165100131204      *
165200131204     C                   leavesr
165300090422     C*
165400090422     c                   eval      PscSys = Scpsys
165500090422     c                   eval      PscNrasInt=ScpNrasint
165600100309     c                   eval      PscDtSca  =RCADtSca
165700090422     c     K03Psc01      chain     NdPsc01l
165800090422     c                   if        %found(NdPsc01l)
165900090422     c                   eval      PscBKAmm=BklSca057
166000090422     c                   eval      PscCliSoll=AggBkl057
166100090422     c                   update    NdPsc000
166200090422     C                   COMMIT
166300090422     c*
166400090422     c                   else
166500090422     c*
166600090422     c* solo se richiesto nel parametro modulo NDCL1S
166700090422     c                   if        BklSca057 <> *blanks or
166800090422     c                             AggBkl057 =*on
166900090422     c                   eval      PscBKAmm=BklSca057
167000090422     c                   eval      PscCliSoll=AggBkl057
167100090422     c                   eval      PscNrRata=ScdNrRata
167200090422     c                   write     NdPsc000
167300090422     C                   COMMIT
167400090422     c                   endif
167500090422     c*
167600090422     c                   endif
167700090422     C*
167800090422     c                   ENDSR
167900061005     C************************************************************
168000100309     C* ESECUZIONE aggiornamento attività per cliente/partita
168100061005     C************************************************************
168200100309     C     AggRCAPar     BEGSR
168300061005     C*
168400061005     C/EXEC SQL
168500100309     C+ UPDATE NDRCA00F
168600100325     C+ SET RCADTATT  = :DTRIFL90
168700100317     C+ WHERE RCASOCIETA = :SOCIETAL90 AND RCAKCC  = :SCDKCC  AND
168800100309     C+ RCAKSC = : SCDKSC and RCADTPAR= :SCDDTPAR AND RCANRPAR =:SCDNRPAR
168900100309     C+ AND RCASERPAR = :SCDSERPAR
169000061005     C/END-EXEC
169100061005     C*
169200061005     C                   ENDSR
169300061005     C/EJECT
169400061003     C*----------------------------------------------------*
169500061003     C*     DEFINIZIONE CHIAVI                             *
169600061003     C*----------------------------------------------------*
169700061003     C     DEFklist      begsr
169800061003     C*
169900061003     C* klist
170000100309    >C* NDRCi01l
170100100309    >C     K05RCI01      KLIST
170200100309    >C                   KFLD                    RciSocieta
170300100309    >C                   KFLD                    RciUnita
170400100309    >C                   KFLD                    RciKcc
170500100309    >C                   KFLD                    RciPosiz
170600100309    >C                   KFLD                    RciAttrib
170700100317    >C     K06RCI01      KLIST
170800100317    >C                   KFLD                    RciSocieta
170900100317    >C                   KFLD                    RciUnita
171000100317    >C                   KFLD                    RciKcc
171100100317    >C                   KFLD                    RciPosiz
171200100317    >C                   KFLD                    RciAttrib
171300100317    >C                   KFLD                    RciFase
171400130409C2221C* NDRCP01L
171500130409  "  C     K05RCP01      KLIST
171600130409  "  C                   KFLD                    RCPSocieta
171700130409  "  C                   KFLD                    RCPUnita
171800130409  "  C                   KFLD                    RCPKcc
171900130409  "  C                   KFLD                    RCPPosiz
172000130409C2221C                   KFLD                    RCPAttrib
172100100309    >C* ndRCA01l
172200100318    >C     K02Rca01      KLIST
172300100318    >C                   KFLD                    RcaSocieta
172400100318    >C                   KFLD                    RcaPosiz
172500100318    >C     K04Rca01      KLIST
172600100309    >C                   KFLD                    RcaSocieta
172700100318    >C                   KFLD                    RcaPosiz
172800100309    >C                   KFLD                    RcaKCC
172900100309    >C                   KFLD                    RcaKSC
173000100318    >C     K07Rca01      KLIST
173100100317    >C                   KFLD                    RcaSocieta
173200100318    >C                   KFLD                    RcaPosiz
173300100317    >C                   KFLD                    RcaKCC
173400100317    >C                   KFLD                    RcaKSC
173500100317    >C                   KFLD                    RcaDtpar
173600100317    >C                   KFLD                    RcaNrpar
173700100317    >C                   KFLD                    RcaSerPar
173800100318    >C* ndRCA02l
173900100318    >C     K03Rca02      KLIST
174000100318    >C                   KFLD                    RcaSocieta
174100100318    >C                   KFLD                    RcaPosiz
174200100318    >C                   KFLD                    RcaFase
174300110401D2680X***  K08Rca02      KLIST
174400110401D2680C     K09Rca02      KLIST
174500100513    >C                   KFLD                    RcaSocieta
174600100513    >C                   KFLD                    RcaPosiz
174700100513    >C                   KFLD                    RcaFase
174800100513    >C                   KFLD                    RcaProgr
174900100513     c                   kfld                    RcaUnita
175000100513     c                   Kfld                    RcaAttrib
175100100513     c                   Kfld                    RcaKcc
175200110401D2680c                   Kfld                    RcaAzione
175300100513     c                   Kfld                    Rcaksc
175400100407    >C* ndRCA04l
175500100408    >C     K09Rca04      KLIST
175600100407    >C                   KFLD                    RcaSocieta
175700100407    >C                   KFLD                    RcaPosiz
175800100407    >C                   KFLD                    RcaKCC
175900100407    >C                   KFLD                    RcaKSC
176000100407    >C                   KFLD                    RcaDtpar
176100100407    >C                   KFLD                    RcaNrpar
176200100407    >C                   KFLD                    RcaSerPar
176300100407    >C                   KFLD                    RcaDtSca
176400100407    >C                   KFLD                    RcaDtAtt
176500100408    >C     K08Rca04      KLIST
176600100407    >C                   KFLD                    RcaSocieta
176700100407    >C                   KFLD                    RcaPosiz
176800100407    >C                   KFLD                    RcaKCC
176900100407    >C                   KFLD                    RcaKSC
177000100407    >C                   KFLD                    RcaDtpar
177100100407    >C                   KFLD                    RcaNrpar
177200100407    >C                   KFLD                    RcaSerPar
177300100407    >C                   KFLD                    RcaDtSca
177400130607C2225C* NDRCA07l
177500130607  "  C     K03Rca07      KLIST
177600130607  "  C                   KFLD                    RcaSocieta
177700130607  "  C                   KFLD                    RcaPosiz
177800130607  "  C                   KFLD                    RcaFase
177900130607  "  C     K09Rca07      KLIST
178000130607  "  C                   KFLD                    RcaSocieta
178100130607  "  C                   KFLD                    RcaPosiz
178200130607  "  C                   KFLD                    RcaFase
178300130607  "  c                   kfld                    RcaUnita
178400130607  "  c                   Kfld                    RcaAttrib
178500130607  "  c                   Kfld                    RcaKcc
178600130607  "  c                   Kfld                    RcaAzione
178700130607  "  c                   Kfld                    Rcaksc
178800130607C2225C                   KFLD                    RcaProgr
178900061003    >C* ndscP01l
179000100318    >C     K06ScP01      KLIST
179100061003    >C                   KFLD                    scPSocieta
179200061003    >C                   KFLD                    ScPCTB
179300061003    >C                   KFLD                    ScPtpscad
179400061003     C                   KFLD                    SCPDETT
179500061003    >C                   KFLD                    ScPKCC
179600061003    >C                   KFLD                    ScPKSC
179700061003    >C     K09scP01      KLIST
179800061003    >C                   KFLD                    scPSocieta
179900061003    >C                   KFLD                    ScPCTB
180000061003    >C                   KFLD                    ScPtpscad
180100061003     C                   KFLD                    SCPDETT
180200061003    >C                   KFLD                    ScPKCC
180300061003    >C                   KFLD                    ScPKSC
180400061003    >C                   KFLD                    ScPDtpar
180500061003    >C                   KFLD                    ScPNrpar
180600061003    >C                   KFLD                    ScPSerPar
180700061003    >C* ndscd01l
180800061003    >C     K09scd01      KLIST
180900061003    >C                   KFLD                    scdSocieta
181000061003    >C                   KFLD                    ScdCTB
181100061003    >C                   KFLD                    Scdtpscad
181200061003    >C                   KFLD                    ScdKCC
181300061003    >C                   KFLD                    ScdKSC
181400061003    >C                   KFLD                    ScdDtpar
181500061003    >C                   KFLD                    ScdNrpar
181600061003    >C                   KFLD                    ScdSerPar
181700061003    >C                   KFLD                    ScdDtSca
181800131031    >C     K08Rca01      KLIST
181900131031    >C                   KFLD                    RcaSocieta
182000131031    >C                   KFLD                    RcaPosiz
182100131031    >C                   KFLD                    RcaKCC
182200131031    >C                   KFLD                    RcaKSC
182300131031    >C                   KFLD                    RcaDtpar
182400131031    >C                   KFLD                    RcaNrpar
182500131031    >C                   KFLD                    RcaSerPar
182600131031    >C                   KFLD                    RcaDtSca
182700100309    >C* ndscd01l
182800100309    >C     K03scd01      KLIST
182900100309    >C                   KFLD                    scdSocieta
183000100309    >C                   KFLD                    ScdCTB
183100100309    >C                   KFLD                    Scdtpscad
183200090421     C*NDpsc01l
183300090421     C     K03PSc01      KLIST
183400090421     C                   KFLD                    PSCSys
183500090421     C                   KFLD                    PSCNrasInt
183600090421     C                   KFLD                    PSCDtSca
183700090421     C     K02PSc01      KLIST
183800090421     C                   KFLD                    PSCSys
183900090421     C                   KFLD                    PSCNrasInt
184000100317     C* NDPas03L
184100100317     C     k08Pas03      KLIST
184200100317     C                   KFLD                    Passocieta
184300100317     C                   KFLD                    Pasctb
184400100317     C                   KFLD                    Paskcc
184500100317     C                   KFLD                    Pasksc
184600100317     C                   KFLD                    Pasdtpar
184700100317     C                   KFLD                    PasNrpar
184800100317     C                   KFLD                    PasSerPar
184900100317     C                   KFLD                    Pasdtsca
185000100317     C*NDpps01l
185100100317     C     K02Pps01      KLIST
185200100317     C                   KFLD                    PpsSys
185300100317     C                   KFLD                    PpsNrasInt
185400100521    >C* ndppa01l
185500100521    >C     K07ppa01      KLIST
185600100521    >C                   KFLD                    ppaSocieta
185700100521    >C                   KFLD                    ppaCTB
185800100521    >C                   KFLD                    ppaKCC
185900100521    >C                   KFLD                    ppaKSC
186000100521    >C                   KFLD                    ppaDtpar
186100100521    >C                   KFLD                    ppaNrpar
186200100521    >C                   KFLD                    ppaSerPar
186300100331    >C* ndEpa01l
186400100331    >C     K06Epa01      KLIST
186500100331    >C                   KFLD                    EpaCsi
186600100331    >C                   KFLD                    EpaKCC
186700100331    >C                   KFLD                    EpaKSC
186800100331    >C                   KFLD                    EpaDrp
186900100331    >C                   KFLD                    EpaNrp
187000100331    >C                   KFLD                    EpaSerPar
187100061003     c*
187200100426     C* NDIMP01L
187300100426     C*
187400100426     c     K08IMP01      KLIST
187500100426     C                   KFLD                    IMPSocieta
187600100426     C                   KFLD                    IMPKcc
187700100426     C                   KFLD                    IMPKsc
187800100426     C                   KFLD                    IMPDtPar
187900100426     C                   KFLD                    IMPNrPar
188000100426     C                   KFLD                    IMPSerPar
188100100426     C                   KFLD                    IMPDtSca
188200100426     C                   KFLD                    IMPProgr
188300061003     c                   endsr
188400090422     c*
188500090421     C************************************************************
188600070605     C* Operazioni iniziali
188700090421     C************************************************************
188800070605     C     *INZSR        BEGSR
188900070605     c*
189000070605     C     *ENTRY        PLIST
189100070605     C                   PARM                    kpjba
189200100309     c                   eval      Ndcl90ds=Kpjbu
189300070605     C*
189400070605    >C                   CALLB     'XSTRCMT'
189500070605    >C                   OPEN      NDPSC01L
189600090421     c*
189700100419     c                   open      ndcl91P
189800100324     c*
189900090421     c                   endsr
