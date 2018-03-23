000100170801     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR':'UBBRTETI') ACTGRP(*CALLER)
000200170412
000300170412
000400170412      //********************************************************************************************
000500170412      //
000600170412      // Definizione prototipi procedure.
000700170412      //
000800170412      //********************************************************************************************
000900170418     D/COPY GAITRASRC/SRCPROTOPR,UBBRTETIWR
001000170418     D/COPY GAITRASRC/SRCPROTOPR,UBSPL2ALL
001100170803     D/COPY GAITRASRC/SRCPROTOPR,UBIFS
001200170413
001300170412
001400170412
001500170412      //********************************************************************************************
001600170412      //
001700170412      // Definizione interfacce procedure.
001800170412      //
001900170412      //********************************************************************************************
002000170413     D/COPY GAITRASRC/SRCPROTOPI,UBSGU1R
002100170412     D/COPY GAITRASRC/SRCPROTOPI,UBSPL2ALL
002200170413     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
002300170413     D/COPY GAITRASRC/SRCPROTOPI,UBWAS70R
002400170518     D/COPY GAITRASRC/SRCPROTOPI,UBGETIFSR
002500170413
002600010524
002700170412     D* Variabili di work
002800170413     D  UBSGU1DSA    e ds                  inz qualified
002900170413     D  UBBRTETIDS   e ds                  inz qualified
003000170413     D  EDIVABDS     e ds                  inz extname(EDIVABWR)
003100170413     D  FNLV55DS     e ds                  inz
003200170420     D  DVPC         e ds                  inz
003300170418     D  wPath          s            256    inz
003400170418     D  wPathDest      s            256    inz
003500170413     D  wPathDatiBV    s            256    inz
003600170413     D  wPathTipoFile  s            256    inz
003700170412     D  wFileName      s            256    inz
003800170420     D  wFileNameSav   s            256    inz
003900170413     D  wIFSKEY        s                   inz like(ifsKEY)
004000170413     D  wTipOut        s             15    inz
004100170413     D  wMsg           s            256    inz
004200170413     D  wFNUM_DS       ds
004300170413     D   wFNUM                        6a   dim(100)
004400170420     D  wUDTA_DS       ds
004500170420     D   wUDTA                       10a   dim(100)
004600170804     D  wIFSfileName   s           1024a   inz varying
004700171003     D  DS_OutDati     ds                  inz qualified
004800171003     D   Lungh                       10i 0
004900171003     D   Dati                      9000a
005000171004     D  wUBSGU1OUT     s               a   inz len(1000000) varying
005100171003     D  wOutDatiDS     ds                  inz likeds(DS_OutDati) dim(85)
005200180313     D wTMA            s              3S 0 inz
005300170413
005400170413     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
005500170413     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
005600010525
005700010525
005800010525     C*-----------------------------------------------------------------------------
005900170412     C*
006000170412     C* Data di elaborazione
006100170412     C                   time                    w0140            14 0
006200170412     C                   movel     w0140         oracor            6 0          *ORA  (6) IN H/M/SS
006300170412     C                   z-add     *zeros        datcor            8 0
006400170412     C                   eval      datcor = %dec(%date() : *ISO)
006500170412     C*
006600170412     C* Avvio il monitoring del intero flusso
006700170412     C                   monitor
006800170413     C*
006900170413     C* Inizializzazioni variabili di work
007000170413     C                   exsr      exeINZ
007100170413     C*
007200170413     C* Verifica correttezza parametri di lancio
007300170413     C                   exsr      chkPAR
007400170412     C*
007500170412     C* Reperisco dati da tabella di procedura
007600170413     C                   exsr      rtvTAB
007700170413     C*
007800170413     C* Eseguo attività richeiste
007900170413     C                   exsr      exeELA
008000170413     C*
008100170413     C* Imposto esito chiamata a OK
008200170413     C                   eval      UBSGU1IDS.OSGU1IESI = *zeros
008300170413     C                   exsr      outMSG
008400170412     C*
008500170412     C* Gestisco eventuale errore
008600170412     C                   on-error
008700170412     C*
008800170412     C* Arresto il monitoring
008900170412     C                   endmon
009000100624     C*
009100171016     C                   return
009200010525     C*
009300010525     C*-----------------------------------------------------------------------------
009400170413
009500170413
009600170413
009700170413     C     exeINZ        begsr
009800170413     C*
009900170418     C                   clear                   wPath
010000170418     C                   clear                   wPathDest
010100170418     C                   clear                   wPathDatiBV
010200170418     C                   clear                   wPathTipoFile
010300170418     C                   clear                   wFileName
010400170418     C                   clear                   wIFSKEY
010500170418     C                   clear                   wTipOut
010600170418     C                   clear                   wMsg
010700170418     C                   clear                   wFNUM_DS
010800170420     C                   clear                   wUDTA_DS
010900170413     C*
011000170413     C                   endsr
011100170413
011200170413
011300170413
011400170413     C*
011500170413     C     chkPAR        begsr
011600170413     C*
011700170413     C* Normalizzo la lingua indicata in input
011800170413     C                   if        UBSGU1IDS.ISGU1ILIN = *blanks
011900170413     C                   eval      UBSGU1IDS.ISGU1ILIN = 'IT'
012000170418     C                   else
012100170413     C                   eval      UBSGU1IDS.ISGU1ILIN =
012200170413     C                             %xlate(Lower:Upper:UBSGU1IDS.ISGU1ILIN)
012300170413     C                   endif
012400170418     C                   if        UBSGU1IDS.ISGU1ILIN = 'IT'
012500170418     C                   else
012600170418     C                   eval      UBSGU1IDS.ISGU1ILIN = 'EN'
012700170418     C                   endif
012800170413     C*
012900170413     C* Inizializzo i parametri di output ed esito ad errore generico
013000170804     C                   clear                   UBSGU1OLEN
013100170413     C                   clear                   UBSGU1OUT
013200170413     C                   eval      UBSGU1IDS.OSGU1IESI = -01
013300170413     C                   exsr      outMSG
013400170413     C*
013500170413     C* Ridefinisco i parametri relativi al driver di stampa segnacollo
013600170413     C                   eval      UBBRTETIDS = %subst(UBSGU1IDS.ISGU1IBRTE:1:
013700170413     C                                          UBSGU1IDS.ISGU1IBLEN)
013800170413     C*
013900170413     C* Spengo tutti indicatori (*in30-*in40) riferiti ai vari formati
014000170413     C                   setoff                                       303132
014100170413     C                   setoff                                       333435
014200170413     C                   setoff                                       363738
014300170413     C                   setoff                                       3940
014400170413     C*
014500170413     C* Gestisco il formato specifico richiesto
014600170413     C                   select
014700170413     C*
014800170413     C* - Controlli formali formato "A "
014900170418     C                   when      %trim(UBSGU1IDS.ISGU1IFMT) = 'A'
015000170413     C                   eval      UBSGU1DSA = %subst(UBSGU1IDS.ISGU1IFLAT:1:
015100170413     C                                         UBSGU1IDS.ISGU1IFLEN)
015200170418     C                   exsr      chkFMT_A
015300170413     C                   other
015400170413     C                   eval      UBSGU1IDS.OSGU1IESI = -02
015500170413     C                   exsr      outMSG
015600170609     C                   return
015700170413     C                   endsl
015800170413     C*
015900170413     C                   endsr
016000170413
016100170413
016200170413
016300170413     C*
016400170418     C     chkFMT_A      begsr
016500170418     C*
016600170418     C                   if        UBSGU1DSA.ISGU1AOPZ = *blanks
016700170418     C                   eval      UBSGU1DSA.ISGU1AOPZ = 'STD01'
016800170418     C                   endif
016900170420     C*
017000170420     C                   if        UBSGU1DSA.ISGU1AUNI <> *blanks
017100170420     C                   eval      DVPC = UBSGU1DSA.ISGU1AUNI
017200170420     C                   else
017300170420     C                   clear                   DVPC
017400170420     C                   endif
017500171003     C*
017600171003     C                   if        §VPCSGR = *blanks
017700171003     C                   eval      §VPCSGR = 'S'
017800171003     C                   endif
017900170413     C*
018000170413     C                   select
018100170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'SPOOL'
018200170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'FILE'
018300170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'DATI'
018400170418     C                   when      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
018500170413     C                   other
018600170413     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'SPOOL'
018700170413     C                   endsl
018800170413     C*
018900170413     C                   if        UBSGU1DSA.ISGU1AKSU  = *blanks AND
019000170413     C                             UBSGU1DSA.ISGU1ADSTO = 'FILE'
019100170413     C                   eval      UBSGU1IDS.OSGU1IESI = -02
019200170413     C                   exsr      outMSG
019300170609     C                   return
019400170413     C                   endif
019500170413     C*
019600170413     C                   select
019700170413     C                   when      UBSGU1DSA.ISGU1ATIPO = 'PDF'
019800170413     C                   when      UBSGU1DSA.ISGU1ATIPO = 'TIF'
019900170413     C                   when      UBSGU1DSA.ISGU1ATIPO = 'TXT'
020000170413     C                   when      UBSGU1DSA.ISGU1ATIPO = 'PCL'
020100170413     C                   other
020200170413     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
020300170413     C                   endsl
020400170418     C*
020500170418     C                   if        UBSGU1DSA.ISGU1ATIPF = *blanks
020600170418     C                   eval      UBSGU1DSA.ISGU1ATIPF = 'SG'
020700170418     C                   endif
020800170418     C*
020900170418     C                   select
021000170418     C                   when      UBSGU1DSA.ISGU1ATIPT = 'BV'                  * BARTVAS
021100170418     C                   when      UBSGU1DSA.ISGU1ATIPT = 'BM'                  * BARTMAIL
021200170418     C                   when      UBSGU1DSA.ISGU1ATIPT = 'TF'                  * TEMP FTP
021300170419     C                   other
021400170419     C                   eval      UBSGU1DSA.ISGU1ATIPT = 'BV'
021500170418     C                   endsl
021600170413     C*
021700170413     C                   if        UBSGU1DSA.ISGU1AMSKF = *blanks
021800170413     C                   eval      UBSGU1DSA.ISGU1AMSKF = 'N1'
021900170413     C                   endif
022000170413     C*
022100170413     C                   endsr
022200170412
022300170412
022400170412
022500170412     C*
022600170413     C     rtvTAB        begsr
022700170412     C*
022800170412     C* Richiami per reperimento vari parametri di procedura
022900170412     C*
023000170412     C* - Percorso di procedura BartVAS
023100170412     C                   eval      IS713TBECOD    = 'VPR'
023200170413     C                   eval      IS713TBEKE1    = '*BARTVAS'
023300170412     C                   eval      IS713TBEKE2    = '*PATHDATI'
023400170412     C                   eval      IS713TBELIN    = *blanks
023500170412     C                   eval      IS713TBESIF    = *blanks
023600170412     C*
023700170412     C                   call      'TIS713R'
023800170412     C                   parm                    IS713TBECOD
023900170412     C                   parm                    IS713TBEKE1
024000170412     C                   parm                    IS713TBEKE2
024100170412     C                   parm                    IS713TBELIN
024200170412     C                   parm                    IS713TBESIF
024300170412     C                   parm                    OS713TBEUNI
024400170412     C                   parm                    OS713ESITO
024500170412     C*
024600170412     C                   if        OS713ESITO = '1'
024700170418     C                   eval      wPathDatiBV = %trim(OS713TBEUNI)
024800170418     C                   eval      wPath = wPathDatiBV
024900170418     C                   exsr      addSlash
025000170418     C                   eval      wPathDatiBV = wPath
025100170412     C                   else
025200170413     C                   eval      UBSGU1IDS.OSGU1IESI = -50
025300170413     C                   eval      wMSG = %trim(IS713TBECOD) + '-' +
025400170413     C                                    %trim(IS713TBEKE1) + '-' +
025500170413     C                                    %trim(IS713TBEKE2)
025600170413     C                   exsr      outMSG
025700170609     C                   return
025800170412     C                   endif
025900170412     C*
026000170412     C* - Percorso del Tipo File associato ai file segnacolli
026100170412     C                   eval      IS713TBECOD    = 'VPR'
026200170413     C                   eval      IS713TBEKE1    = '*VASWS'
026300170413     C                   eval      IS713TBEKE2    = '*PATH-SG'
026400170412     C                   eval      IS713TBELIN    = *blanks
026500170412     C                   eval      IS713TBESIF    = *blanks
026600170412     C*
026700170412     C                   call      'TIS713R'
026800170412     C                   parm                    IS713TBECOD
026900170412     C                   parm                    IS713TBEKE1
027000170412     C                   parm                    IS713TBEKE2
027100170412     C                   parm                    IS713TBELIN
027200170412     C                   parm                    IS713TBESIF
027300170412     C                   parm                    OS713TBEUNI
027400170412     C                   parm                    OS713ESITO
027500170412     C*
027600170412     C                   if        OS713ESITO = '1'
027700170412     C                   eval      wPathTipoFile = %trim(OS713TBEUNI)
027800170418     C                   eval      wPath = wPathTipoFile
027900170418     C                   exsr      addSlash
028000170418     C                   eval      wPathTipoFile = wPath
028100170412     C                   else
028200170413     C                   eval      UBSGU1IDS.OSGU1IESI = -50
028300170413     C                   eval      wMSG = %trim(IS713TBECOD) + '-' +
028400170413     C                                    %trim(IS713TBEKE1) + '-' +
028500170413     C                                    %trim(IS713TBEKE2)
028600170413     C                   exsr      outMSG
028700170609     C                   return
028800170412     C                   endif
028900170412     C*
029000170412     C                   endsr
029100170413
029200170413
029300170413
029400170413     C     exeELA        begsr
029500170413     C*
029600170413     C                   select
029700170413     C*
029800170413     C* Elaborazione formato "A "
029900170418     C                   when      %trim(UBSGU1IDS.ISGU1IFMT) = 'A'
030000170418     C                   seton                                        30
030100170418     C                   exsr      exeFMT_A
030200170413     C*
030300170413     C                   endsl
030400170413     C*
030500170413     C                   endsr
030600170413
030700170413
030800170413
030900170418     C     exeFMT_A      begsr
031000170413     C*
031100170413     C* Se passati in input dati spedizione forzo tali dati ai fini stampa
031200170418     C                   if        UBSGU1DSA.ISGU1ADLEN > *zeros
031300170413     C                   clear                   EDIVABDS
031400180313     C                   eval      wTMA= 0
031500180313     C                   eval      %subst(EDIVABDS:1:
031600180313     C                                              UBSGU1DSA.ISGU1ADLEN) =
031700180313     C                             %subst(UBSGU1DSA.ISGU1ADATI:1:
031800180313     C                                              UBSGU1DSA.ISGU1ADLEN)
031900180313
032000180313     C                   select
032100180313     C* se richiesta maschera etichetta "TA" (TerminalArrivo + LNA + RMA) / Parcel
032200180313     C                   when      UBSGU1DSA.ISGU1AMSKF='TA' AND §VPCSGR<>'S'
032300170413     C                   eval      %subst(EDIVABDS:1:
032400180313     C                                             %len(EDIVABDS)) =
032500180313     C***                                           UBSGU1DSA.ISGU1ADLEN) =
032600170413     C                             %subst(UBSGU1DSA.ISGU1ADATI:1:
032700180313     C                                             %len(EDIVABDS))
032800180313     C***                                           UBSGU1DSA.ISGU1ADLEN)
032900180313     C                   eval      wTMA= %dec(
033000180313     C                                   %subst(UBSGU1DSA.ISGU1ADATI:
033100180313     C                                   %len(EDIVABDS)+1 : 3): 3 : 0)
033200180313     C                   endsl
033300180313
033400170419     C                   exsr      valDati
033500170413     C                   endif
033600170413     C*
033700170413     C* Reperisco il Treminal Partenza
033800170413     C                   exsr      rtvTFA
033900170413     C*
034000170413     C* 1) Stampa segnacollo/i
034100170413     C                   exsr      exeSTAMPA
034200170413     C*
034300170413     C* 2) Direziono output su destinazione richiesta in input
034400170413     C                   exsr      exeDSTOUT
034500170413     C*
034600170413     C                   endsr
034700170413
034800170413
034900170413
035000170413     C     rtvTFA        begsr
035100170413     C*
035200170413     C* Se non già impostato nei parametri di input reperisco il terminal di Arrivo
035300170413     C                   if        UBBRTETIDS.PIN_TMA = *zeros
035400170413     C                   clear                   fnlv55ds
035500170419     C                   eval      d55LNP = UBBRTETIDS.PIN_FLS
035600170419     C                   eval      d55LIN = UBBRTETIDS.PIN_LNA
035700170419     C                   eval      d55TPT = 'A'
035800170419     C                   eval      d55DRF = UBBRTETIDS.PIN_AAS*10000+
035900170419     C                                      UBBRTETIDS.PIN_MGS
036000170413     C                   call      'FNLV55R'
036100170413     C                   parm                    fnlv55ds
036200170413     C                   eval      UBBRTETIDS.PIN_TMA = d55TFA
036300170413     C                   endif
036400170413     C*
036500170413     C                   endsr
036600170413
036700170413
036800170413
036900170413     C*
037000170413     C     exeSTAMPA     begsr
037100170420     C*
037200170516     C                   if        UBBRTETIDS.PIN_1SPLXC = *blanks
037300170420     C                   if        §VPCSGR = 'P'
037400170420     C                   eval      UBBRTETIDS.PIN_1SPLXC = 'S'
037500170420     C                   endif
037600170516     C                   endif
037700170413     C*
037800170418     C                   CALLP     UBBRTETIWR (UBBRTETIDS)
037900170418     C*
038000170418     C                   if        UBBRTETIDS.POUT_ESITO <> *zeros
038100170418     C                   eval      UBSGU1IDS.OSGU1IESI = -10
038200170418     C                   exsr      outMSG
038300170609     C                   return
038400170418     C                   endif
038500170413     C*
038600170413     C                   endsr
038700170413
038800170413
038900170413
039000170413     C*
039100170413     C     exeDSTOUT     begsr
039200171003     C*
039300171003     C                   clear                   wOutDatiDS
039400171004     C                   clear                   wUBSGU1OUT
039500171003     C*
039600170803     C                   setoff                                       50
039700171003     C                   setoff                                       6061
039800170413     C*
039900170413     C                   select
040000170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'SPOOL'               * già in SPOOL
040100170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'FILE'
040200170803     C                   seton                                        50
040300170418     C                   exsr      dstFILE                                      * etich. su file
040400170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'DATI'
040500171003     C                   seton                                        60        * exe dstDATI
040600170803     C                   exsr      dstFILE                                      * etich. su file
040700170418     C                   when      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
040800171003     C                   seton                                        61        * exe dstBAS64
040900170803     C                   exsr      dstFILE                                      * etich. su file
041000170413     C                   endsl
041100170413     C*
041200170413     C                   endsr
041300170413
041400170413
041500170413
041600170413     C*
041700170413     C     dstFILE       begsr
041800170413     C*
041900170413     C* Reperisco il nome file di output da maschera file ricevuta in input
042000170413     C   30              exsr      rtvMask_A
042100170413     C*
042200170413     C* Converto spool nel tipo output richiesto in input
042300170420     C                   eval      wFNUM_DS = UBBRTETIDS.POUT_FNUM
042400170420     C                   eval      wUDTA_DS = UBBRTETIDS.POUT_UDTA
042500170420     C*
042600170420     C* Ciclo per ogni spool generato
042700170420     C                   eval      wFileNameSav = wFileName
042800170420     C                   z-add     1             idx               3 0
042900170420     C                   dow       idx <= %elem(wFNUM) AND
043000170420     C                             wFNUM(idx) <> *blanks
043100170420     C* - compongo il nome file corrente
043200170420     C                   eval      wFileName = %scanrpl('§§§':
043300170420     C                                        %trim(wUDTA(idx)):wFileNameSav)
043400170420     C
043500170420     C* - eseguo conversione da spool a file
043600170420     C                   exsr      cvtETI
043700171003     C
043800171003     C* - se richiesto effettuo reperimento DATI
043900171003     C                   select
044000171003     C                   when      *in60 = *on
044100171003     C                   exsr      dstDATI                                      * dati etich.
044200171003     C                   when      *in61 = *on
044300171003     C                   exsr      dstBAS64                                     * dati B64 etich.
044400171003     C                   endsl
044500170420     C
044600170420     C* - posiziono i file ottenuti per innescare l'invio dato come da anagrafica cliente VAS
044700170803     C   50              exsr      movFIL
044800170420     C
044900170420     C                   add       1             idx
045000170420     C                   enddo
045100171003     C*
045200171003     C* Fuori ciclo se richiesta elaborazione al livello di collo => valorizzo output
045300171003     C                   if        §VPCSGR<>'S'
045400171003     C                   eval      UBSGU1OLEN=%len(wUBSGU1OUT)
045500171003     C                   eval      UBSGU1OUT=wUBSGU1OUT
045600171003     C                   endif
045700170413     C*
045800170413     C                   endsr
045900170413
046000170413
046100170413
046200170413     C*
046300170413     C     rtvMask_A     begsr
046400170419     C*
046500170419     C* In relazione al tipo trasferimento richiesto in input costruisco il path adeguato
046600170419     C                   select
046700170419     C                   when      UBSGU1DSA.ISGU1ATIPT = 'BV'
046800171021     C                   eval      wPathDest = %trim(wPathDatiBV)  +
046900171021     C                                         UBSGU1DSA.ISGU1AKSU + '/OUT/' +
047000171021     C                                         UBSGU1DSA.ISGU1ATIPF
047100170419     C                   other
047200170419     C                   eval      wPathDest = %trim(wPathTipoFile) + 'ERR'
047300170419     C                   endsl
047400171021     C*
047500171021     C* Verifica / creazione folder per esportazione spool
047600171021     C                   exsr      chkFLD
047700170419     C*
047800171021     C                   eval      wTipOut   = %trim(UBSGU1DSA.ISGU1ATIPO)
047900171021     C                   if        wTipOut   = 'TXT'
048000171021     C                   eval      wTipOut   = 'ZPL'
048100171021     C                   endif
048200170413     C*
048300170419     C* Sviluppo la maschera per il tipo naming-convention ricevuta in input
048400170413     C                   select
048500170420     C* "N1" / Shipment
048600170420     C                   when      UBSGU1DSA.ISGU1AMSKF='N1' AND §VPCSGR='S'
048700171021     C                   eval      wFileName = %editc(vabCCM:'X') + '_' +
048800170524     C                                         %editc(vabAAS:'X') +
048900170524     C                                         %editc(vabMGS:'X') + '_' +
049000170524     C                                         %trim(%editc(vabNSP:'4')) + '_' +
049100170418     C                                         %trim(%editc(vabRMN:'4')) + '.' +
049200170413     C                                         %trim(wTipOut)
049300170413     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
049400170413     C                                                    %editc(vabLNP:'X')+
049500170413     C                                                    %editc(vabNRS:'X')+
049600170413     C                                                    %editc(vabNSP:'X')
049700170420     C* "N1" / Parcel
049800170420     C                   when      UBSGU1DSA.ISGU1AMSKF='N1' AND §VPCSGR<>'S'
049900170420     C                   eval      wFileName = 'P' +
050000170420     C                                         %editc(vabCCM:'X') + '_' +
050100170524     C                                         %editc(vabAAS:'X') +
050200170524     C                                         %editc(vabMGS:'X') + '_' +
050300170524     C                                         %trim(%editc(vabNSP:'4')) + '_' +
050400170420     C                                         %editc(vabNRS:'X') +
050500170420     C                                         '§§§' + '.' + %trim(wTipOut)
050600170420     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
050700170420     C                                                    %editc(vabLNP:'X')+
050800170420     C                                                    %editc(vabNRS:'X')+
050900170420     C                                                    %editc(vabNSP:'X')
051000171009     C* "C1" (CCM + RMN + RMA) / Shipment
051100171009     C                   when      UBSGU1DSA.ISGU1AMSKF='C1' AND §VPCSGR='S'
051200171021     C                   eval      wFileName = %editc(vabCCM:'X') + '_' +
051300171009     C                                         %trim(%editc(vabRMN:'4')) + '_' +
051400171009     C                                         %trim(vabRMA) + '.' +
051500171009     C                                         %trim(wTipOut)
051600171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
051700171009     C                                                    %editc(vabLNP:'X')+
051800171009     C                                                    %editc(vabNRS:'X')+
051900171009     C                                                    %editc(vabNSP:'X')
052000171009     C* "C1" (CCM + RMN + RMA) / Parcel
052100171009     C                   when      UBSGU1DSA.ISGU1AMSKF='C1' AND §VPCSGR<>'S'
052200171009     C                   eval      wFileName = 'P' +
052300171009     C                                         %editc(vabCCM:'X') + '_' +
052400171009     C                                         %trim(%editc(vabRMN:'4')) + '_' +
052500171009     C                                         %trim(vabRMA) + '_' +
052600171009     C                                         '§§§' + '.' + %trim(wTipOut)
052700171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
052800171009     C                                                    %editc(vabLNP:'X')+
052900171009     C                                                    %editc(vabNRS:'X')+
053000171009     C                                                    %editc(vabNSP:'X')
053100171009     C* "CN" (CCM + RMN) / Shipment
053200171009     C                   when      UBSGU1DSA.ISGU1AMSKF='CN' AND §VPCSGR='S'
053300171021     C                   eval      wFileName = %editc(vabCCM:'X') + '_' +
053400171009     C                                         %trim(%editc(vabRMN:'4')) + '.' +
053500171009     C                                         %trim(wTipOut)
053600171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
053700171009     C                                                    %editc(vabLNP:'X')+
053800171009     C                                                    %editc(vabNRS:'X')+
053900171009     C                                                    %editc(vabNSP:'X')
054000171009     C* "CN" (CCM + RMN) / Parcel
054100171009     C                   when      UBSGU1DSA.ISGU1AMSKF='CN' AND §VPCSGR<>'S'
054200171009     C                   eval      wFileName = 'P' +
054300171009     C                                         %editc(vabCCM:'X') + '_' +
054400171009     C                                         %trim(%editc(vabRMN:'4')) + '_' +
054500171009     C                                         '§§§' + '.' + %trim(wTipOut)
054600171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
054700171009     C                                                    %editc(vabLNP:'X')+
054800171009     C                                                    %editc(vabNRS:'X')+
054900171009     C                                                    %editc(vabNSP:'X')
055000171009     C* "CA" (CCM + RMA) / Shipment
055100171009     C                   when      UBSGU1DSA.ISGU1AMSKF='CA' AND §VPCSGR='S'
055200171021     C                   eval      wFileName = %editc(vabCCM:'X') + '_' +
055300171009     C                                         %trim(vabRMA) + '.' +
055400171009     C                                         %trim(wTipOut)
055500171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
055600171009     C                                                    %editc(vabLNP:'X')+
055700171009     C                                                    %editc(vabNRS:'X')+
055800171009     C                                                    %editc(vabNSP:'X')
055900171009     C* "CA" (CCM + RMA) / Parcel
056000171009     C                   when      UBSGU1DSA.ISGU1AMSKF='CA' AND §VPCSGR<>'S'
056100171009     C                   eval      wFileName = 'P' +
056200171009     C                                         %editc(vabCCM:'X') + '_' +
056300171009     C                                         %trim(vabRMA) + '_' +
056400171009     C                                         '§§§' + '.' + %trim(wTipOut)
056500171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
056600171009     C                                                    %editc(vabLNP:'X')+
056700171009     C                                                    %editc(vabNRS:'X')+
056800171009     C                                                    %editc(vabNSP:'X')
056900171021     C* "LN" (LNA + RMN) / Shipment
057000171021     C                   when      UBSGU1DSA.ISGU1AMSKF='LN' AND §VPCSGR='S'
057100171021     C                   eval      wFileName = %editc(vabLNA:'X') + '_' +
057200171021     C                                         %trim(%editc(vabRMN:'4')) + '.' +
057300171021     C                                         %trim(wTipOut)
057400171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
057500171021     C                                                    %editc(vabLNP:'X')+
057600171021     C                                                    %editc(vabNRS:'X')+
057700171021     C                                                    %editc(vabNSP:'X')
057800171021     C* "LN" (LNA + RMN) / Parcel
057900171021     C                   when      UBSGU1DSA.ISGU1AMSKF='LN' AND §VPCSGR<>'S'
058000171021     C                   eval      wFileName = 'P' +
058100171021     C                                         %editc(vabLNA:'X') + '_' +
058200171021     C                                         %trim(%editc(vabRMN:'4')) + '_' +
058300171021     C                                         '§§§' + '.' + %trim(wTipOut)
058400171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
058500171021     C                                                    %editc(vabLNP:'X')+
058600171021     C                                                    %editc(vabNRS:'X')+
058700171021     C                                                    %editc(vabNSP:'X')
058800171021     C* "LA" (LNA + RMA) / Shipment
058900171021     C                   when      UBSGU1DSA.ISGU1AMSKF='LA' AND §VPCSGR='S'
059000171021     C                   eval      wFileName = %editc(vabLNA:'X') + '_' +
059100171021     C                                         %trim(vabRMA) + '.' +
059200171021     C                                         %trim(wTipOut)
059300171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
059400171021     C                                                    %editc(vabLNP:'X')+
059500171021     C                                                    %editc(vabNRS:'X')+
059600171021     C                                                    %editc(vabNSP:'X')
059700171021     C* "LA" (LNA + RMA) / Parcel
059800171021     C                   when      UBSGU1DSA.ISGU1AMSKF='LA' AND §VPCSGR<>'S'
059900171021     C                   eval      wFileName = 'P' +
060000171021     C                                         %editc(vabLNA:'X') + '_' +
060100171021     C                                         %trim(vabRMA) + '_' +
060200171021     C                                         '§§§' + '.' + %trim(wTipOut)
060300171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
060400171021     C                                                    %editc(vabLNP:'X')+
060500171021     C                                                    %editc(vabNRS:'X')+
060600171021     C                                                    %editc(vabNSP:'X')
060700171021     C* "NA" (RMN + RMA) / Shipment
060800171021     C                   when      UBSGU1DSA.ISGU1AMSKF='NA' AND §VPCSGR='S'
060900171021     C                   eval      wFileName = %editc(vabRMN:'4') + '_' +
061000171021     C                                         %trim(vabRMA) + '.' +
061100171021     C                                         %trim(wTipOut)
061200171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
061300171021     C                                                    %editc(vabLNP:'X')+
061400171021     C                                                    %editc(vabNRS:'X')+
061500171021     C                                                    %editc(vabNSP:'X')
061600171021     C* "NA" (RMN + RMA) / Parcel
061700171021     C                   when      UBSGU1DSA.ISGU1AMSKF='NA' AND §VPCSGR<>'S'
061800171021     C                   eval      wFileName = 'P' +
061900171021     C                                         %editc(vabRMN:'4') + '_' +
062000171021     C                                         %trim(vabRMA) + '_' +
062100171021     C                                         '§§§' + '.' + %trim(wTipOut)
062200171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
062300171021     C                                                    %editc(vabLNP:'X')+
062400171021     C                                                    %editc(vabNRS:'X')+
062500171021     C                                                    %editc(vabNSP:'X')
062600180313     C* "TA" (TerminalArrivo + LNA + RMA) / Parcel
062700180313     C                   when      UBSGU1DSA.ISGU1AMSKF='TA' AND §VPCSGR<>'S'
062800180313     C                   eval      wFileName = 'P' +
062900180313     C                                         %editc(wTMA:'X') + '_' +
063000180313     C                                         %editc(vabLNA:'X') + '_' +
063100180313     C                                         %trim(vabRMA) + '_' +
063200180313     C                                         '§§§' + '.' + %trim(wTipOut)
063300180313     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
063400180313     C                                                    %editc(vabLNP:'X')+
063500180313     C                                                    %editc(vabNRS:'X')+
063600180313     C                                                    %editc(vabNSP:'X')
063700170413     C                   endsl
063800171018     C*
063900171018     C* Se tratta di etichetta di "errore" faccio sempre precedere da prefizzo ERR_
064000171018     C                   if        UBBRTETIDS.PIN_SEGERR = 'S'
064100171018     C                   eval      wFileName = 'ERR_' + %trim(wFileName)
064200171018     C                   endif
064300170413     C*
064400170413     C                   endsr
064500171021
064600171021
064700171021
064800171021     C*
064900171021     C     chkFLD        begsr
065000171021     C*
065100171021     C* Chiamo driver per conversione spool
065200171021     C                   eval      Ispl2A_Opz     = 'FLD'
065300171021     C                   eval      Ispl2A_DltSpl  = *blanks
065400171021     C                   eval      Ispl2A_FilNam  = *blanks
065500171021     C                   eval      Ispl2A_JobNam  = *blanks
065600171021     C                   eval      Ispl2A_UsrNam  = *blanks
065700171021     C                   eval      Ispl2A_JobNum  = *blanks
065800171021     C                   eval      Ispl2A_NumFil  = *blanks
065900171021     C                   eval      Ispl2A_PathFil = %trim(wPathTipoFile) +
066000171021     C                                              %editc(vabCCM:'X')
066100171021     C*
066200171021     C                   callp     UBSPL2ALL(Ispl2A_Opz
066300171021     C                                      :Ispl2A_DltSpl
066400171021     C                                      :Ispl2A_FilNam
066500171021     C                                      :Ispl2A_JobNam
066600171021     C                                      :Ispl2A_UsrNam
066700171021     C                                      :Ispl2A_JobNum
066800171021     C                                      :Ispl2A_NumFil
066900171021     C                                      :Ispl2A_PathFil
067000171021     C                                      :Ospl2A_Esito
067100171021     C                                      :Ospl2A_Msg
067200171021     C                                      )
067300171021     C*
067400171021     C                   endsr
067500170412
067600170412
067700170412
067800170413     C*
067900170413     C     cvtETI        begsr
068000170413     C*
068100170413     C* Chiamo driver per conversione spool
068200171021     C                   eval      Ispl2A_Opz     = %trim(UBSGU1DSA.ISGU1ATIPO)
068300170412     C                   eval      Ispl2A_DltSpl  = 'Y'
068400170413     C                   eval      Ispl2A_FilNam  = UBBRTETIDS.POUT_FNAM
068500170413     C                   eval      Ispl2A_JobNam  = UBBRTETIDS.POUT_JNAM
068600170413     C                   eval      Ispl2A_UsrNam  = UBBRTETIDS.POUT_UNAM
068700170413     C                   eval      Ispl2A_JobNum  = UBBRTETIDS.POUT_JNUM
068800170420     C                   eval      Ispl2A_NumFil  = wFNUM(idx)
068900171021     C                   eval      Ispl2A_PathFil = %trim(wPathTipoFile) +
069000170418     C                                              %trim(wFileName)
069100170418     C*
069200170412     C                   callp     UBSPL2ALL(Ispl2A_Opz
069300170412     C                                      :Ispl2A_DltSpl
069400170412     C                                      :Ispl2A_FilNam
069500170412     C                                      :Ispl2A_JobNam
069600170412     C                                      :Ispl2A_UsrNam
069700170412     C                                      :Ispl2A_JobNum
069800170412     C                                      :Ispl2A_NumFil
069900170412     C                                      :Ispl2A_PathFil
070000170412     C                                      :Ospl2A_Esito
070100170412     C                                      :Ospl2A_Msg
070200170412     C                                      )
070300170412     C*
070400170413     C                   if        Ospl2A_Esito <> '1'
070500170413     C                   eval      UBSGU1IDS.OSGU1IESI = -20
070600170413     C                   exsr      outMSG
070700170609     C                   return
070800170412     C                   endif
070900170412     C*
071000170412     C                   endsr
071100170412
071200170412
071300170412
071400170418     C     movFIL        begsr
071500170412     C*
071600170418     C                   clear                   IWAS7IFSDS
071700170412     C                   eval      ifstip    = '*IMMED'
071800170412     C                   eval      ifsins    = datcor*1000000+oracor
071900170413     C                   eval      ifskey    = wIFSKEY
072000170418     C                   eval      ifspths   = %trim(wPathTipoFile)
072100170412     C                   eval      ifsfils   = %trim(wFileName)
072200170418     C                   eval      ifspthd   = %trim(wPathDest)
072300170412     C                   eval      ifstrytot = 1000
072400171016     C                   eval      ifsopz    = 'MOVN'
072500170413     C*
072600171016     C                   call      'UBWAS70R'
072700171016     C                   parm                    IWAS7IFSDS
072800171016     C                   parm                    OWAS7ESI
072900170418     C*
073000171016     C                   if        OWAS7ESI <> *zeros
073100170418     C                   eval      UBSGU1IDS.OSGU1IESI = -30
073200170418     C                   exsr      outMSG
073300170609     C                   return
073400170418     C                   endif
073500170412     C*
073600170412     C                   endsr
073700170803
073800170803
073900170803
074000170803     C     rmvFIL        begsr
074100170803     C*
074200170803     C                   eval      wIFSfileName = %trim(iGETIFSfile)
074300170803     C                   callp(e)  IFS_RemoveFile(wIFSfileName)
074400170803     C*
074500170803     C                   endsr
074600170413
074700170413
074800170413
074900170413     C*
075000170518     C     dstDATI       begsr
075100170518     C*
075200170518     C                   eval      iGETIFSfile = %trim(wPathTipoFile) +
075300170518     C                                           %trim(wFileName)
075400170518     C                   eval      iGETIFSbinTxt  = 'TXT'
075500170518     C                   eval      iGETIFSbase64  = *blanks
075600170518     C                   eval      iGETIFSmaxLen  = 1000000
075700170804     C*
075800170804     C                   exsr      rtvDati
075900170413     C*
076000170413     C                   endsr
076100170418
076200170418
076300170418
076400170418     C*
076500170418     C     dstBAS64      begsr
076600170518     C*
076700170518     C                   eval      iGETIFSfile = %trim(wPathTipoFile) +
076800170518     C                                           %trim(wFileName)
076900170518     C                   eval      iGETIFSbinTxt  = 'BIN'
077000170518     C                   eval      iGETIFSbase64  = 'Y'
077100170518     C                   eval      iGETIFSmaxLen  = 750000
077200170804     C*
077300170804     C                   exsr      rtvDati
077400170418     C*
077500170418     C                   endsr
077600170804
077700170804
077800170804
077900170804     C*
078000170804     C     rtvDATI       begsr
078100170804     C*
078200170804     C                   Call      'UBGETIFSR'
078300170804     C                   Parm                    iGETIFSfile
078400170804     C                   Parm                    iGETIFSbinTxt
078500170804     C                   Parm                    iGETIFSbase64
078600170804     C                   Parm                    iGETIFSmaxLen
078700170804     C                   Parm                    oGETIFSesito
078800170804     C                   Parm                    oGETIFScodPage
078900170804     C                   Parm                    oGETIFS_CCSID
079000170804     C                   Parm                    oGETIFSlen
079100170804     C                   Parm                    oGETIFSdati4MB
079200170804     C*
079300170804     C* Se esito OK => ritorno dati reperiti, altrimenti esco subito con errore
079400171003     C                   if        oGETIFSesito = 0
079500171003     C                   select
079600171003     C                   when      §VPCSGR='S'
079700171003     C                   eval      UBSGU1OLEN=oGETIFSlen
079800171003     C                   eval      UBSGU1OUT=%subst(oGETIFSdati4MB:1:oGETIFSlen)
079900171003     C                   other
080000171003     C                   eval      DS_OutDati.Lungh=oGETIFSlen
080100171003     C                   eval      DS_OutDati.Dati=
080200171003     C                                      %subst(oGETIFSdati4MB:1:oGETIFSlen)
080300171003     C                   eval      wOutDatiDS(idx)=DS_OutDati
080400171003     C                   eval      wUBSGU1OUT=wUBSGU1OUT+DS_OutDati
080500171003     C                   endsl
080600170804     C                   exsr      rmvFIL
080700170804     C                   else
080800170804     C                   eval      UBSGU1IDS.OSGU1IESI = -40
080900170804     C                   exsr      outMSG
081000170804     C                   return
081100170804     C                   endif
081200170804     C*
081300170804     C                   endsr
081400170413
081500170418
081600170418
081700170418     C*
081800170418     C     valDati       begsr
081900170418     C*
082000170418     C* A seconda del opzione richiesta in input effettuo valorizzazione dati specifica
082100170418     C                   select
082200170418     C*
082300170418     C                   when      UBSGU1DSA.ISGU1AOPZ = 'STD01'
082400170418     C                   eval      UBBRTETIDS.PIN_AAS = vabAAS
082500170418     C                   eval      UBBRTETIDS.PIN_MGS = vabMGS
082600170418     C                   eval      UBBRTETIDS.PIN_TSP = vabTSP
082700170418     C                   eval      UBBRTETIDS.PIN_RSM = vabRMO
082800170418     C                   eval      UBBRTETIDS.PIN_PRM = *blanks
082900170418     C                   eval      UBBRTETIDS.PIN_NZM = vabNMO
083000170418     C                   eval      UBBRTETIDS.PIN_RSD = vabRSD
083100170418     C                   eval      UBBRTETIDS.PIN_IND = vabIND
083200170418     C                   eval      UBBRTETIDS.PIN_CAD = vabCAD
083300170418     C                   eval      UBBRTETIDS.PIN_LOD = vabLOD
083400170418     C                   eval      UBBRTETIDS.PIN_PRD = vabPRD
083500170418     C                   eval      UBBRTETIDS.PIN_NZD = vabNZD
083600170419 xxx C                   eval      UBBRTETIDS.PIN_ISA = *blanks
083700170419     C                   eval      UBBRTETIDS.PIN_FLS = vabLNP
083800170418     C                   eval      UBBRTETIDS.PIN_NRS = vabNRS
083900170418     C                   eval      UBBRTETIDS.PIN_NCD = vabNCD
084000170418     C                   if        UBBRTETIDS.PIN_NCA = *zeros
084100170418     C                   eval      UBBRTETIDS.PIN_NCA = vabNCD + vabNCL - 1
084200170418     C                   endif
084300170418     C                   eval      UBBRTETIDS.PIN_LNA = vabLNA
084400170418     C                   eval      UBBRTETIDS.PIN_ZNC = vabZNC
084500170418     C                   eval      UBBRTETIDS.PIN_NC2 = vabNCL
084600170418     C                   eval      UBBRTETIDS.PIN_PKG = vabPKB
084700170418     C                   eval      UBBRTETIDS.PIN_VOL = vabVLB
084800171010     C*
084900171010     C                   if        vabRMA <> *blanks
085000171010     C                   eval      UBBRTETIDS.PIN_CDP = vabRMA
085100171010     C                   else
085200171010     C                   eval      UBBRTETIDS.PIN_CDP = %editc(vabRMN:'4')
085300171010     C                   endif
085400170418     C*
085500170418     C                   endsl
085600170418     C*
085700170418     C                   endsr
085800170413
085900170418
086000170413
086100170413     C     outMSG        begsr
086200170413     C*
086300170413     C                   select
086400170413     C                   when      UBSGU1IDS.ISGU1ILIN = 'IT'
086500170413     C*
086600170413     C                   select
086700170413     C                   when      UBSGU1IDS.OSGU1IESI = *zeros
086800170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'OK'
086900170413     C                   when      UBSGU1IDS.OSGU1IESI = -01
087000170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err generico'
087100170413     C                   when      UBSGU1IDS.OSGU1IESI = -02
087200170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err parametri'
087300170418     C                   when      UBSGU1IDS.OSGU1IESI = -10
087400170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err stampa segnacollo'
087500170413     C                   when      UBSGU1IDS.OSGU1IESI = -20
087600170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err conversione spool'
087700170803     C                   when      UBSGU1IDS.OSGU1IESI = -30
087800170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err spostamento file'
087900170803     C                   when      UBSGU1IDS.OSGU1IESI = -40
088000170803     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err reperimento file'
088100170803     C                   when      UBSGU1IDS.OSGU1IESI = -50
088200170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err Tab non trovata'
088300170413     C                   endsl
088400170413     C*
088500170413     C                   when      UBSGU1IDS.ISGU1ILIN = 'EN'
088600170413     C*
088700170413     C                   select
088800170413     C                   when      UBSGU1IDS.OSGU1IESI = *zeros
088900170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'OK'
089000170413     C                   when      UBSGU1IDS.OSGU1IESI = -01
089100170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Generic Err'
089200170413     C                   when      UBSGU1IDS.OSGU1IESI = -02
089300170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Parameter Err'
089400170418     C                   when      UBSGU1IDS.OSGU1IESI = -10
089500170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Label Err'
089600170418     C                   when      UBSGU1IDS.OSGU1IESI = -20
089700170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err convert spool'
089800170418     C                   when      UBSGU1IDS.OSGU1IESI = -30
089900170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err move file'
090000170803     C                   when      UBSGU1IDS.OSGU1IESI = -40
090100170803     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err retrieve file'
090200170413     C                   when      UBSGU1IDS.OSGU1IESI = -50
090300170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err Tab not found'
090400170413     C                   endsl
090500170413     C*
090600170413     C                   endsl
090700170413     C*
090800170413     C                   eval      UBSGU1IDS.OSGU1IMSG =
090900170413     C                             %trim(UBSGU1IDS.OSGU1IMSG)+
091000170413     C                             ' (' + %trim(wMSG) + ')'
091100171016     C*
091200171016     C                   seton                                        lr
091300170413     C*
091400170413     C                   endsr
091500170418
091600170418
091700170418
091800170418     C*
091900170418     C     addSlash      begsr
092000170418     C*
092100170418     C                   if        %subst(wPath:%len(%trim(wPath)):1) = '/'
092200170418     C                   else
092300170418     C                   eval      wPath = %trim(wPath) + '/'
092400170418     C                   endif
092500170418     C*
092600170418     C                   endsr
092700000714
092800050920
092900050920
093000000714      /TITLE Routine di *init PGM
093100000714     C*
093200000714     C     *inzsr        begsr
093300060103     C*
093400060103     C     *entry        plist
093500170413     C                   parm                    UBSGU1IDS
093600170804     C                   parm                    UBSGU1OLEN
093700170803     C                   parm                    UBSGU1OUT
093800000714     C*
093900000714     C                   endsr
094000000714
