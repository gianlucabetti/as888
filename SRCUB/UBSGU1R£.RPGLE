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
005200170413
005300170413     D  Upper          c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
005400170413     D  Lower          c                   'abcdefghijklmnopqrstuvwxyz'
005500010525
005600010525
005700010525     C*-----------------------------------------------------------------------------
005800170412     C*
005900170412     C* Data di elaborazione
006000170412     C                   time                    w0140            14 0
006100170412     C                   movel     w0140         oracor            6 0          *ORA  (6) IN H/M/SS
006200170412     C                   z-add     *zeros        datcor            8 0
006300170412     C                   eval      datcor = %dec(%date() : *ISO)
006400170412     C*
006500170412     C* Avvio il monitoring del intero flusso
006600170412     C                   monitor
006700170413     C*
006800170413     C* Inizializzazioni variabili di work
006900170413     C                   exsr      exeINZ
007000170413     C*
007100170413     C* Verifica correttezza parametri di lancio
007200170413     C                   exsr      chkPAR
007300170412     C*
007400170412     C* Reperisco dati da tabella di procedura
007500170413     C                   exsr      rtvTAB
007600170413     C*
007700170413     C* Eseguo attività richeiste
007800170413     C                   exsr      exeELA
007900170413     C*
008000170413     C* Imposto esito chiamata a OK
008100170413     C                   eval      UBSGU1IDS.OSGU1IESI = *zeros
008200170413     C                   exsr      outMSG
008300170412     C*
008400170412     C* Gestisco eventuale errore
008500170412     C                   on-error
008600170412     C*
008700170412     C* Arresto il monitoring
008800170412     C                   endmon
008900100624     C*
009000171016     C                   return
009100010525     C*
009200010525     C*-----------------------------------------------------------------------------
009300170413
009400170413
009500170413
009600170413     C     exeINZ        begsr
009700170413     C*
009800170418     C                   clear                   wPath
009900170418     C                   clear                   wPathDest
010000170418     C                   clear                   wPathDatiBV
010100170418     C                   clear                   wPathTipoFile
010200170418     C                   clear                   wFileName
010300170418     C                   clear                   wIFSKEY
010400170418     C                   clear                   wTipOut
010500170418     C                   clear                   wMsg
010600170418     C                   clear                   wFNUM_DS
010700170420     C                   clear                   wUDTA_DS
010800170413     C*
010900170413     C                   endsr
011000170413
011100170413
011200170413
011300170413     C*
011400170413     C     chkPAR        begsr
011500170413     C*
011600170413     C* Normalizzo la lingua indicata in input
011700170413     C                   if        UBSGU1IDS.ISGU1ILIN = *blanks
011800170413     C                   eval      UBSGU1IDS.ISGU1ILIN = 'IT'
011900170418     C                   else
012000170413     C                   eval      UBSGU1IDS.ISGU1ILIN =
012100170413     C                             %xlate(Lower:Upper:UBSGU1IDS.ISGU1ILIN)
012200170413     C                   endif
012300170418     C                   if        UBSGU1IDS.ISGU1ILIN = 'IT'
012400170418     C                   else
012500170418     C                   eval      UBSGU1IDS.ISGU1ILIN = 'EN'
012600170418     C                   endif
012700170413     C*
012800170413     C* Inizializzo i parametri di output ed esito ad errore generico
012900170804     C                   clear                   UBSGU1OLEN
013000170413     C                   clear                   UBSGU1OUT
013100170413     C                   eval      UBSGU1IDS.OSGU1IESI = -01
013200170413     C                   exsr      outMSG
013300170413     C*
013400170413     C* Ridefinisco i parametri relativi al driver di stampa segnacollo
013500170413     C                   eval      UBBRTETIDS = %subst(UBSGU1IDS.ISGU1IBRTE:1:
013600170413     C                                          UBSGU1IDS.ISGU1IBLEN)
013700170413     C*
013800170413     C* Spengo tutti indicatori (*in30-*in40) riferiti ai vari formati
013900170413     C                   setoff                                       303132
014000170413     C                   setoff                                       333435
014100170413     C                   setoff                                       363738
014200170413     C                   setoff                                       3940
014300170413     C*
014400170413     C* Gestisco il formato specifico richiesto
014500170413     C                   select
014600170413     C*
014700170413     C* - Controlli formali formato "A "
014800170418     C                   when      %trim(UBSGU1IDS.ISGU1IFMT) = 'A'
014900170413     C                   eval      UBSGU1DSA = %subst(UBSGU1IDS.ISGU1IFLAT:1:
015000170413     C                                         UBSGU1IDS.ISGU1IFLEN)
015100170418     C                   exsr      chkFMT_A
015200170413     C                   other
015300170413     C                   eval      UBSGU1IDS.OSGU1IESI = -02
015400170413     C                   exsr      outMSG
015500170609     C                   return
015600170413     C                   endsl
015700170413     C*
015800170413     C                   endsr
015900170413
016000170413
016100170413
016200170413     C*
016300170418     C     chkFMT_A      begsr
016400170418     C*
016500170418     C                   if        UBSGU1DSA.ISGU1AOPZ = *blanks
016600170418     C                   eval      UBSGU1DSA.ISGU1AOPZ = 'STD01'
016700170418     C                   endif
016800170420     C*
016900170420     C                   if        UBSGU1DSA.ISGU1AUNI <> *blanks
017000170420     C                   eval      DVPC = UBSGU1DSA.ISGU1AUNI
017100170420     C                   else
017200170420     C                   clear                   DVPC
017300170420     C                   endif
017400171003     C*
017500171003     C                   if        §VPCSGR = *blanks
017600171003     C                   eval      §VPCSGR = 'S'
017700171003     C                   endif
017800170413     C*
017900170413     C                   select
018000170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'SPOOL'
018100170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'FILE'
018200170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'DATI'
018300170418     C                   when      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
018400170413     C                   other
018500170413     C                   eval      UBSGU1DSA.ISGU1ADSTO = 'SPOOL'
018600170413     C                   endsl
018700170413     C*
018800170413     C                   if        UBSGU1DSA.ISGU1AKSU  = *blanks AND
018900170413     C                             UBSGU1DSA.ISGU1ADSTO = 'FILE'
019000170413     C                   eval      UBSGU1IDS.OSGU1IESI = -02
019100170413     C                   exsr      outMSG
019200170609     C                   return
019300170413     C                   endif
019400170413     C*
019500170413     C                   select
019600170413     C                   when      UBSGU1DSA.ISGU1ATIPO = 'PDF'
019700170413     C                   when      UBSGU1DSA.ISGU1ATIPO = 'TIF'
019800170413     C                   when      UBSGU1DSA.ISGU1ATIPO = 'TXT'
019900170413     C                   when      UBSGU1DSA.ISGU1ATIPO = 'PCL'
020000170413     C                   other
020100170413     C                   eval      UBSGU1DSA.ISGU1ATIPO = 'TXT'
020200170413     C                   endsl
020300170418     C*
020400170418     C                   if        UBSGU1DSA.ISGU1ATIPF = *blanks
020500170418     C                   eval      UBSGU1DSA.ISGU1ATIPF = 'SG'
020600170418     C                   endif
020700170418     C*
020800170418     C                   select
020900170418     C                   when      UBSGU1DSA.ISGU1ATIPT = 'BV'                  * BARTVAS
021000170418     C                   when      UBSGU1DSA.ISGU1ATIPT = 'BM'                  * BARTMAIL
021100170418     C                   when      UBSGU1DSA.ISGU1ATIPT = 'TF'                  * TEMP FTP
021200170419     C                   other
021300170419     C                   eval      UBSGU1DSA.ISGU1ATIPT = 'BV'
021400170418     C                   endsl
021500170413     C*
021600170413     C                   if        UBSGU1DSA.ISGU1AMSKF = *blanks
021700170413     C                   eval      UBSGU1DSA.ISGU1AMSKF = 'N1'
021800170413     C                   endif
021900170413     C*
022000170413     C                   endsr
022100170412
022200170412
022300170412
022400170412     C*
022500170413     C     rtvTAB        begsr
022600170412     C*
022700170412     C* Richiami per reperimento vari parametri di procedura
022800170412     C*
022900170412     C* - Percorso di procedura BartVAS
023000170412     C                   eval      IS713TBECOD    = 'VPR'
023100170413     C                   eval      IS713TBEKE1    = '*BARTVAS'
023200170412     C                   eval      IS713TBEKE2    = '*PATHDATI'
023300170412     C                   eval      IS713TBELIN    = *blanks
023400170412     C                   eval      IS713TBESIF    = *blanks
023500170412     C*
023600170412     C                   call      'TIS713R'
023700170412     C                   parm                    IS713TBECOD
023800170412     C                   parm                    IS713TBEKE1
023900170412     C                   parm                    IS713TBEKE2
024000170412     C                   parm                    IS713TBELIN
024100170412     C                   parm                    IS713TBESIF
024200170412     C                   parm                    OS713TBEUNI
024300170412     C                   parm                    OS713ESITO
024400170412     C*
024500170412     C                   if        OS713ESITO = '1'
024600170418     C                   eval      wPathDatiBV = %trim(OS713TBEUNI)
024700170418     C                   eval      wPath = wPathDatiBV
024800170418     C                   exsr      addSlash
024900170418     C                   eval      wPathDatiBV = wPath
025000170412     C                   else
025100170413     C                   eval      UBSGU1IDS.OSGU1IESI = -50
025200170413     C                   eval      wMSG = %trim(IS713TBECOD) + '-' +
025300170413     C                                    %trim(IS713TBEKE1) + '-' +
025400170413     C                                    %trim(IS713TBEKE2)
025500170413     C                   exsr      outMSG
025600170609     C                   return
025700170412     C                   endif
025800170412     C*
025900170412     C* - Percorso del Tipo File associato ai file segnacolli
026000170412     C                   eval      IS713TBECOD    = 'VPR'
026100170413     C                   eval      IS713TBEKE1    = '*VASWS'
026200170413     C                   eval      IS713TBEKE2    = '*PATH-SG'
026300170412     C                   eval      IS713TBELIN    = *blanks
026400170412     C                   eval      IS713TBESIF    = *blanks
026500170412     C*
026600170412     C                   call      'TIS713R'
026700170412     C                   parm                    IS713TBECOD
026800170412     C                   parm                    IS713TBEKE1
026900170412     C                   parm                    IS713TBEKE2
027000170412     C                   parm                    IS713TBELIN
027100170412     C                   parm                    IS713TBESIF
027200170412     C                   parm                    OS713TBEUNI
027300170412     C                   parm                    OS713ESITO
027400170412     C*
027500170412     C                   if        OS713ESITO = '1'
027600170412     C                   eval      wPathTipoFile = %trim(OS713TBEUNI)
027700170418     C                   eval      wPath = wPathTipoFile
027800170418     C                   exsr      addSlash
027900170418     C                   eval      wPathTipoFile = wPath
028000170412     C                   else
028100170413     C                   eval      UBSGU1IDS.OSGU1IESI = -50
028200170413     C                   eval      wMSG = %trim(IS713TBECOD) + '-' +
028300170413     C                                    %trim(IS713TBEKE1) + '-' +
028400170413     C                                    %trim(IS713TBEKE2)
028500170413     C                   exsr      outMSG
028600170609     C                   return
028700170412     C                   endif
028800170412     C*
028900170412     C                   endsr
029000170413
029100170413
029200170413
029300170413     C     exeELA        begsr
029400170413     C*
029500170413     C                   select
029600170413     C*
029700170413     C* Elaborazione formato "A "
029800170418     C                   when      %trim(UBSGU1IDS.ISGU1IFMT) = 'A'
029900170418     C                   seton                                        30
030000170418     C                   exsr      exeFMT_A
030100170413     C*
030200170413     C                   endsl
030300170413     C*
030400170413     C                   endsr
030500170413
030600170413
030700170413
030800170418     C     exeFMT_A      begsr
030900170413     C*
031000170413     C* Se passati in input dati spedizione forzo tali dati ai fini stampa
031100170418     C                   if        UBSGU1DSA.ISGU1ADLEN > *zeros
031200170413     C                   clear                   EDIVABDS
031300170413     C                   eval      %subst(EDIVABDS:1:
031400170413     C                                              UBSGU1DSA.ISGU1ADLEN) =
031500170413     C                             %subst(UBSGU1DSA.ISGU1ADATI:1:
031600170413     C                                              UBSGU1DSA.ISGU1ADLEN)
031700170419     C                   exsr      valDati
031800170413     C                   endif
031900170413     C*
032000170413     C* Reperisco il Treminal Partenza
032100170413     C                   exsr      rtvTFA
032200170413     C*
032300170413     C* 1) Stampa segnacollo/i
032400170413     C                   exsr      exeSTAMPA
032500170413     C*
032600170413     C* 2) Direziono output su destinazione richiesta in input
032700170413     C                   exsr      exeDSTOUT
032800170413     C*
032900170413     C                   endsr
033000170413
033100170413
033200170413
033300170413     C     rtvTFA        begsr
033400170413     C*
033500170413     C* Se non già impostato nei parametri di input reperisco il terminal di Arrivo
033600170413     C                   if        UBBRTETIDS.PIN_TMA = *zeros
033700170413     C                   clear                   fnlv55ds
033800170419     C                   eval      d55LNP = UBBRTETIDS.PIN_FLS
033900170419     C                   eval      d55LIN = UBBRTETIDS.PIN_LNA
034000170419     C                   eval      d55TPT = 'A'
034100170419     C                   eval      d55DRF = UBBRTETIDS.PIN_AAS*10000+
034200170419     C                                      UBBRTETIDS.PIN_MGS
034300170413     C                   call      'FNLV55R'
034400170413     C                   parm                    fnlv55ds
034500170413     C                   eval      UBBRTETIDS.PIN_TMA = d55TFA
034600170413     C                   endif
034700170413     C*
034800170413     C                   endsr
034900170413
035000170413
035100170413
035200170413     C*
035300170413     C     exeSTAMPA     begsr
035400170420     C*
035500170516     C                   if        UBBRTETIDS.PIN_1SPLXC = *blanks
035600170420     C                   if        §VPCSGR = 'P'
035700170420     C                   eval      UBBRTETIDS.PIN_1SPLXC = 'S'
035800170420     C                   endif
035900170516     C                   endif
036000170413     C*
036100170418     C                   CALLP     UBBRTETIWR (UBBRTETIDS)
036200170418     C*
036300170418     C                   if        UBBRTETIDS.POUT_ESITO <> *zeros
036400170418     C                   eval      UBSGU1IDS.OSGU1IESI = -10
036500170418     C                   exsr      outMSG
036600170609     C                   return
036700170418     C                   endif
036800170413     C*
036900170413     C                   endsr
037000170413
037100170413
037200170413
037300170413     C*
037400170413     C     exeDSTOUT     begsr
037500171003     C*
037600171003     C                   clear                   wOutDatiDS
037700171004     C                   clear                   wUBSGU1OUT
037800171003     C*
037900170803     C                   setoff                                       50
038000171003     C                   setoff                                       6061
038100170413     C*
038200170413     C                   select
038300170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'SPOOL'               * già in SPOOL
038400170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'FILE'
038500170803     C                   seton                                        50
038600170418     C                   exsr      dstFILE                                      * etich. su file
038700170413     C                   when      UBSGU1DSA.ISGU1ADSTO = 'DATI'
038800171003     C                   seton                                        60        * exe dstDATI
038900170803     C                   exsr      dstFILE                                      * etich. su file
039000170418     C                   when      UBSGU1DSA.ISGU1ADSTO = 'BAS64'
039100171003     C                   seton                                        61        * exe dstBAS64
039200170803     C                   exsr      dstFILE                                      * etich. su file
039300170413     C                   endsl
039400170413     C*
039500170413     C                   endsr
039600170413
039700170413
039800170413
039900170413     C*
040000170413     C     dstFILE       begsr
040100170413     C*
040200170413     C* Reperisco il nome file di output da maschera file ricevuta in input
040300170413     C   30              exsr      rtvMask_A
040400170413     C*
040500170413     C* Converto spool nel tipo output richiesto in input
040600170420     C                   eval      wFNUM_DS = UBBRTETIDS.POUT_FNUM
040700170420     C                   eval      wUDTA_DS = UBBRTETIDS.POUT_UDTA
040800170420     C*
040900170420     C* Ciclo per ogni spool generato
041000170420     C                   eval      wFileNameSav = wFileName
041100170420     C                   z-add     1             idx               3 0
041200170420     C                   dow       idx <= %elem(wFNUM) AND
041300170420     C                             wFNUM(idx) <> *blanks
041400170420     C* - compongo il nome file corrente
041500170420     C                   eval      wFileName = %scanrpl('§§§':
041600170420     C                                        %trim(wUDTA(idx)):wFileNameSav)
041700170420     C
041800170420     C* - eseguo conversione da spool a file
041900170420     C                   exsr      cvtETI
042000171003     C
042100171003     C* - se richiesto effettuo reperimento DATI
042200171003     C                   select
042300171003     C                   when      *in60 = *on
042400171003     C                   exsr      dstDATI                                      * dati etich.
042500171003     C                   when      *in61 = *on
042600171003     C                   exsr      dstBAS64                                     * dati B64 etich.
042700171003     C                   endsl
042800170420     C
042900170420     C* - posiziono i file ottenuti per innescare l'invio dato come da anagrafica cliente VAS
043000170803     C   50              exsr      movFIL
043100170420     C
043200170420     C                   add       1             idx
043300170420     C                   enddo
043400171003     C*
043500171003     C* Fuori ciclo se richiesta elaborazione al livello di collo => valorizzo output
043600171003     C                   if        §VPCSGR<>'S'
043700171003     C                   eval      UBSGU1OLEN=%len(wUBSGU1OUT)
043800171003     C                   eval      UBSGU1OUT=wUBSGU1OUT
043900171003     C                   endif
044000170413     C*
044100170413     C                   endsr
044200170413
044300170413
044400170413
044500170413     C*
044600170413     C     rtvMask_A     begsr
044700170419     C*
044800170419     C* In relazione al tipo trasferimento richiesto in input costruisco il path adeguato
044900170419     C                   select
045000170419     C                   when      UBSGU1DSA.ISGU1ATIPT = 'BV'
045100171021     C                   eval      wPathDest = %trim(wPathDatiBV)  +
045200171021     C                                         UBSGU1DSA.ISGU1AKSU + '/OUT/' +
045300171021     C                                         UBSGU1DSA.ISGU1ATIPF
045400170419     C                   other
045500170419     C                   eval      wPathDest = %trim(wPathTipoFile) + 'ERR'
045600170419     C                   endsl
045700171021     C*
045800171021     C* Verifica / creazione folder per esportazione spool
045900171021     C                   exsr      chkFLD
046000170419     C*
046100171021     C                   eval      wTipOut   = %trim(UBSGU1DSA.ISGU1ATIPO)
046200171021     C                   if        wTipOut   = 'TXT'
046300171021     C                   eval      wTipOut   = 'ZPL'
046400171021     C                   endif
046500170413     C*
046600170419     C* Sviluppo la maschera per il tipo naming-convention ricevuta in input
046700170413     C                   select
046800170420     C* "N1" / Shipment
046900170420     C                   when      UBSGU1DSA.ISGU1AMSKF='N1' AND §VPCSGR='S'
047000171021     C                   eval      wFileName = %editc(vabCCM:'X') + '_' +
047100170524     C                                         %editc(vabAAS:'X') +
047200170524     C                                         %editc(vabMGS:'X') + '_' +
047300170524     C                                         %trim(%editc(vabNSP:'4')) + '_' +
047400170418     C                                         %trim(%editc(vabRMN:'4')) + '.' +
047500170413     C                                         %trim(wTipOut)
047600170413     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
047700170413     C                                                    %editc(vabLNP:'X')+
047800170413     C                                                    %editc(vabNRS:'X')+
047900170413     C                                                    %editc(vabNSP:'X')
048000170420     C* "N1" / Parcel
048100170420     C                   when      UBSGU1DSA.ISGU1AMSKF='N1' AND §VPCSGR<>'S'
048200170420     C                   eval      wFileName = 'P' +
048300170420     C                                         %editc(vabCCM:'X') + '_' +
048400170524     C                                         %editc(vabAAS:'X') +
048500170524     C                                         %editc(vabMGS:'X') + '_' +
048600170524     C                                         %trim(%editc(vabNSP:'4')) + '_' +
048700170420     C                                         %editc(vabNRS:'X') +
048800170420     C                                         '§§§' + '.' + %trim(wTipOut)
048900170420     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
049000170420     C                                                    %editc(vabLNP:'X')+
049100170420     C                                                    %editc(vabNRS:'X')+
049200170420     C                                                    %editc(vabNSP:'X')
049300171009     C* "C1" (CCM + RMN + RMA) / Shipment
049400171009     C                   when      UBSGU1DSA.ISGU1AMSKF='C1' AND §VPCSGR='S'
049500171021     C                   eval      wFileName = %editc(vabCCM:'X') + '_' +
049600171009     C                                         %trim(%editc(vabRMN:'4')) + '_' +
049700171009     C                                         %trim(vabRMA) + '.' +
049800171009     C                                         %trim(wTipOut)
049900171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
050000171009     C                                                    %editc(vabLNP:'X')+
050100171009     C                                                    %editc(vabNRS:'X')+
050200171009     C                                                    %editc(vabNSP:'X')
050300171009     C* "C1" (CCM + RMN + RMA) / Parcel
050400171009     C                   when      UBSGU1DSA.ISGU1AMSKF='C1' AND §VPCSGR<>'S'
050500171009     C                   eval      wFileName = 'P' +
050600171009     C                                         %editc(vabCCM:'X') + '_' +
050700171009     C                                         %trim(%editc(vabRMN:'4')) + '_' +
050800171009     C                                         %trim(vabRMA) + '_' +
050900171009     C                                         '§§§' + '.' + %trim(wTipOut)
051000171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
051100171009     C                                                    %editc(vabLNP:'X')+
051200171009     C                                                    %editc(vabNRS:'X')+
051300171009     C                                                    %editc(vabNSP:'X')
051400171009     C* "CN" (CCM + RMN) / Shipment
051500171009     C                   when      UBSGU1DSA.ISGU1AMSKF='CN' AND §VPCSGR='S'
051600171021     C                   eval      wFileName = %editc(vabCCM:'X') + '_' +
051700171009     C                                         %trim(%editc(vabRMN:'4')) + '.' +
051800171009     C                                         %trim(wTipOut)
051900171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
052000171009     C                                                    %editc(vabLNP:'X')+
052100171009     C                                                    %editc(vabNRS:'X')+
052200171009     C                                                    %editc(vabNSP:'X')
052300171009     C* "CN" (CCM + RMN) / Parcel
052400171009     C                   when      UBSGU1DSA.ISGU1AMSKF='CN' AND §VPCSGR<>'S'
052500171009     C                   eval      wFileName = 'P' +
052600171009     C                                         %editc(vabCCM:'X') + '_' +
052700171009     C                                         %trim(%editc(vabRMN:'4')) + '_' +
052800171009     C                                         '§§§' + '.' + %trim(wTipOut)
052900171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
053000171009     C                                                    %editc(vabLNP:'X')+
053100171009     C                                                    %editc(vabNRS:'X')+
053200171009     C                                                    %editc(vabNSP:'X')
053300171009     C* "CA" (CCM + RMA) / Shipment
053400171009     C                   when      UBSGU1DSA.ISGU1AMSKF='CA' AND §VPCSGR='S'
053500171021     C                   eval      wFileName = %editc(vabCCM:'X') + '_' +
053600171009     C                                         %trim(vabRMA) + '.' +
053700171009     C                                         %trim(wTipOut)
053800171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
053900171009     C                                                    %editc(vabLNP:'X')+
054000171009     C                                                    %editc(vabNRS:'X')+
054100171009     C                                                    %editc(vabNSP:'X')
054200171009     C* "CA" (CCM + RMA) / Parcel
054300171009     C                   when      UBSGU1DSA.ISGU1AMSKF='CA' AND §VPCSGR<>'S'
054400171009     C                   eval      wFileName = 'P' +
054500171009     C                                         %editc(vabCCM:'X') + '_' +
054600171009     C                                         %trim(vabRMA) + '_' +
054700171009     C                                         '§§§' + '.' + %trim(wTipOut)
054800171009     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
054900171009     C                                                    %editc(vabLNP:'X')+
055000171009     C                                                    %editc(vabNRS:'X')+
055100171009     C                                                    %editc(vabNSP:'X')
055200171021     C* "LN" (LNA + RMN) / Shipment
055300171021     C                   when      UBSGU1DSA.ISGU1AMSKF='LN' AND §VPCSGR='S'
055400171021     C                   eval      wFileName = %editc(vabLNA:'X') + '_' +
055500171021     C                                         %trim(%editc(vabRMN:'4')) + '.' +
055600171021     C                                         %trim(wTipOut)
055700171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
055800171021     C                                                    %editc(vabLNP:'X')+
055900171021     C                                                    %editc(vabNRS:'X')+
056000171021     C                                                    %editc(vabNSP:'X')
056100171021     C* "LN" (LNA + RMN) / Parcel
056200171021     C                   when      UBSGU1DSA.ISGU1AMSKF='LN' AND §VPCSGR<>'S'
056300171021     C                   eval      wFileName = 'P' +
056400171021     C                                         %editc(vabLNA:'X') + '_' +
056500171021     C                                         %trim(%editc(vabRMN:'4')) + '_' +
056600171021     C                                         '§§§' + '.' + %trim(wTipOut)
056700171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
056800171021     C                                                    %editc(vabLNP:'X')+
056900171021     C                                                    %editc(vabNRS:'X')+
057000171021     C                                                    %editc(vabNSP:'X')
057100171021     C* "LA" (LNA + RMA) / Shipment
057200171021     C                   when      UBSGU1DSA.ISGU1AMSKF='LA' AND §VPCSGR='S'
057300171021     C                   eval      wFileName = %editc(vabLNA:'X') + '_' +
057400171021     C                                         %trim(vabRMA) + '.' +
057500171021     C                                         %trim(wTipOut)
057600171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
057700171021     C                                                    %editc(vabLNP:'X')+
057800171021     C                                                    %editc(vabNRS:'X')+
057900171021     C                                                    %editc(vabNSP:'X')
058000171021     C* "LA" (LNA + RMA) / Parcel
058100171021     C                   when      UBSGU1DSA.ISGU1AMSKF='LA' AND §VPCSGR<>'S'
058200171021     C                   eval      wFileName = 'P' +
058300171021     C                                         %editc(vabLNA:'X') + '_' +
058400171021     C                                         %trim(vabRMA) + '_' +
058500171021     C                                         '§§§' + '.' + %trim(wTipOut)
058600171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
058700171021     C                                                    %editc(vabLNP:'X')+
058800171021     C                                                    %editc(vabNRS:'X')+
058900171021     C                                                    %editc(vabNSP:'X')
059000171021     C* "NA" (RMN + RMA) / Shipment
059100171021     C                   when      UBSGU1DSA.ISGU1AMSKF='NA' AND §VPCSGR='S'
059200171021     C                   eval      wFileName = %editc(vabRMN:'4') + '_' +
059300171021     C                                         %trim(vabRMA) + '.' +
059400171021     C                                         %trim(wTipOut)
059500171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
059600171021     C                                                    %editc(vabLNP:'X')+
059700171021     C                                                    %editc(vabNRS:'X')+
059800171021     C                                                    %editc(vabNSP:'X')
059900171021     C* "NA" (RMN + RMA) / Parcel
060000171021     C                   when      UBSGU1DSA.ISGU1AMSKF='NA' AND §VPCSGR<>'S'
060100171021     C                   eval      wFileName = 'P' +
060200171021     C                                         %editc(vabRMN:'4') + '_' +
060300171021     C                                         %trim(vabRMA) + '_' +
060400171021     C                                         '§§§' + '.' + %trim(wTipOut)
060500171021     C                   eval      wIFSKEY   = 'VAS_DWL_'+%editc(vabAAS:'X')+
060600171021     C                                                    %editc(vabLNP:'X')+
060700171021     C                                                    %editc(vabNRS:'X')+
060800171021     C                                                    %editc(vabNSP:'X')
060900170413     C                   endsl
061000171018     C*
061100171018     C* Se tratta di etichetta di "errore" faccio sempre precedere da prefizzo ERR_
061200171018     C                   if        UBBRTETIDS.PIN_SEGERR = 'S'
061300171018     C                   eval      wFileName = 'ERR_' + %trim(wFileName)
061400171018     C                   endif
061500170413     C*
061600170413     C                   endsr
061700171021
061800171021
061900171021
062000171021     C*
062100171021     C     chkFLD        begsr
062200171021     C*
062300171021     C* Chiamo driver per conversione spool
062400171021     C                   eval      Ispl2A_Opz     = 'FLD'
062500171021     C                   eval      Ispl2A_DltSpl  = *blanks
062600171021     C                   eval      Ispl2A_FilNam  = *blanks
062700171021     C                   eval      Ispl2A_JobNam  = *blanks
062800171021     C                   eval      Ispl2A_UsrNam  = *blanks
062900171021     C                   eval      Ispl2A_JobNum  = *blanks
063000171021     C                   eval      Ispl2A_NumFil  = *blanks
063100171021     C                   eval      Ispl2A_PathFil = %trim(wPathTipoFile) +
063200171021     C                                              %editc(vabCCM:'X')
063300171021     C*
063400171021     C                   callp     UBSPL2ALL(Ispl2A_Opz
063500171021     C                                      :Ispl2A_DltSpl
063600171021     C                                      :Ispl2A_FilNam
063700171021     C                                      :Ispl2A_JobNam
063800171021     C                                      :Ispl2A_UsrNam
063900171021     C                                      :Ispl2A_JobNum
064000171021     C                                      :Ispl2A_NumFil
064100171021     C                                      :Ispl2A_PathFil
064200171021     C                                      :Ospl2A_Esito
064300171021     C                                      :Ospl2A_Msg
064400171021     C                                      )
064500171021     C*
064600171021     C                   endsr
064700170412
064800170412
064900170412
065000170413     C*
065100170413     C     cvtETI        begsr
065200170413     C*
065300170413     C* Chiamo driver per conversione spool
065400171021     C                   eval      Ispl2A_Opz     = %trim(UBSGU1DSA.ISGU1ATIPO)
065500170412     C                   eval      Ispl2A_DltSpl  = 'Y'
065600170413     C                   eval      Ispl2A_FilNam  = UBBRTETIDS.POUT_FNAM
065700170413     C                   eval      Ispl2A_JobNam  = UBBRTETIDS.POUT_JNAM
065800170413     C                   eval      Ispl2A_UsrNam  = UBBRTETIDS.POUT_UNAM
065900170413     C                   eval      Ispl2A_JobNum  = UBBRTETIDS.POUT_JNUM
066000170420     C                   eval      Ispl2A_NumFil  = wFNUM(idx)
066100171021     C                   eval      Ispl2A_PathFil = %trim(wPathTipoFile) +
066200170418     C                                              %trim(wFileName)
066300170418     C*
066400170412     C                   callp     UBSPL2ALL(Ispl2A_Opz
066500170412     C                                      :Ispl2A_DltSpl
066600170412     C                                      :Ispl2A_FilNam
066700170412     C                                      :Ispl2A_JobNam
066800170412     C                                      :Ispl2A_UsrNam
066900170412     C                                      :Ispl2A_JobNum
067000170412     C                                      :Ispl2A_NumFil
067100170412     C                                      :Ispl2A_PathFil
067200170412     C                                      :Ospl2A_Esito
067300170412     C                                      :Ospl2A_Msg
067400170412     C                                      )
067500170412     C*
067600170413     C                   if        Ospl2A_Esito <> '1'
067700170413     C                   eval      UBSGU1IDS.OSGU1IESI = -20
067800170413     C                   exsr      outMSG
067900170609     C                   return
068000170412     C                   endif
068100170412     C*
068200170412     C                   endsr
068300170412
068400170412
068500170412
068600170418     C     movFIL        begsr
068700170412     C*
068800170418     C                   clear                   IWAS7IFSDS
068900170412     C                   eval      ifstip    = '*IMMED'
069000170412     C                   eval      ifsins    = datcor*1000000+oracor
069100170413     C                   eval      ifskey    = wIFSKEY
069200170418     C                   eval      ifspths   = %trim(wPathTipoFile)
069300170412     C                   eval      ifsfils   = %trim(wFileName)
069400170418     C                   eval      ifspthd   = %trim(wPathDest)
069500170412     C                   eval      ifstrytot = 1000
069600171016     C                   eval      ifsopz    = 'MOVN'
069700170413     C*
069800171016     C                   call      'UBWAS70R'
069900171016     C                   parm                    IWAS7IFSDS
070000171016     C                   parm                    OWAS7ESI
070100170418     C*
070200171016     C                   if        OWAS7ESI <> *zeros
070300170418     C                   eval      UBSGU1IDS.OSGU1IESI = -30
070400170418     C                   exsr      outMSG
070500170609     C                   return
070600170418     C                   endif
070700170412     C*
070800170412     C                   endsr
070900170803
071000170803
071100170803
071200170803     C     rmvFIL        begsr
071300170803     C*
071400170803     C                   eval      wIFSfileName = %trim(iGETIFSfile)
071500170803     C                   callp(e)  IFS_RemoveFile(wIFSfileName)
071600170803     C*
071700170803     C                   endsr
071800170413
071900170413
072000170413
072100170413     C*
072200170518     C     dstDATI       begsr
072300170518     C*
072400170518     C                   eval      iGETIFSfile = %trim(wPathTipoFile) +
072500170518     C                                           %trim(wFileName)
072600170518     C                   eval      iGETIFSbinTxt  = 'TXT'
072700170518     C                   eval      iGETIFSbase64  = *blanks
072800170518     C                   eval      iGETIFSmaxLen  = 1000000
072900170804     C*
073000170804     C                   exsr      rtvDati
073100170413     C*
073200170413     C                   endsr
073300170418
073400170418
073500170418
073600170418     C*
073700170418     C     dstBAS64      begsr
073800170518     C*
073900170518     C                   eval      iGETIFSfile = %trim(wPathTipoFile) +
074000170518     C                                           %trim(wFileName)
074100170518     C                   eval      iGETIFSbinTxt  = 'BIN'
074200170518     C                   eval      iGETIFSbase64  = 'Y'
074300170518     C                   eval      iGETIFSmaxLen  = 750000
074400170804     C*
074500170804     C                   exsr      rtvDati
074600170418     C*
074700170418     C                   endsr
074800170804
074900170804
075000170804
075100170804     C*
075200170804     C     rtvDATI       begsr
075300170804     C*
075400170804     C                   Call      'UBGETIFSR'
075500170804     C                   Parm                    iGETIFSfile
075600170804     C                   Parm                    iGETIFSbinTxt
075700170804     C                   Parm                    iGETIFSbase64
075800170804     C                   Parm                    iGETIFSmaxLen
075900170804     C                   Parm                    oGETIFSesito
076000170804     C                   Parm                    oGETIFScodPage
076100170804     C                   Parm                    oGETIFS_CCSID
076200170804     C                   Parm                    oGETIFSlen
076300170804     C                   Parm                    oGETIFSdati4MB
076400170804     C*
076500170804     C* Se esito OK => ritorno dati reperiti, altrimenti esco subito con errore
076600171003     C                   if        oGETIFSesito = 0
076700171003     C                   select
076800171003     C                   when      §VPCSGR='S'
076900171003     C                   eval      UBSGU1OLEN=oGETIFSlen
077000171003     C                   eval      UBSGU1OUT=%subst(oGETIFSdati4MB:1:oGETIFSlen)
077100171003     C                   other
077200171003     C                   eval      DS_OutDati.Lungh=oGETIFSlen
077300171003     C                   eval      DS_OutDati.Dati=
077400171003     C                                      %subst(oGETIFSdati4MB:1:oGETIFSlen)
077500171003     C                   eval      wOutDatiDS(idx)=DS_OutDati
077600171003     C                   eval      wUBSGU1OUT=wUBSGU1OUT+DS_OutDati
077700171003     C                   endsl
077800170804     C                   exsr      rmvFIL
077900170804     C                   else
078000170804     C                   eval      UBSGU1IDS.OSGU1IESI = -40
078100170804     C                   exsr      outMSG
078200170804     C                   return
078300170804     C                   endif
078400170804     C*
078500170804     C                   endsr
078600170413
078700170418
078800170418
078900170418     C*
079000170418     C     valDati       begsr
079100170418     C*
079200170418     C* A seconda del opzione richiesta in input effettuo valorizzazione dati specifica
079300170418     C                   select
079400170418     C*
079500170418     C                   when      UBSGU1DSA.ISGU1AOPZ = 'STD01'
079600170418     C                   eval      UBBRTETIDS.PIN_AAS = vabAAS
079700170418     C                   eval      UBBRTETIDS.PIN_MGS = vabMGS
079800170418     C                   eval      UBBRTETIDS.PIN_TSP = vabTSP
079900170418     C                   eval      UBBRTETIDS.PIN_RSM = vabRMO
080000170418     C                   eval      UBBRTETIDS.PIN_PRM = *blanks
080100170418     C                   eval      UBBRTETIDS.PIN_NZM = vabNMO
080200170418     C                   eval      UBBRTETIDS.PIN_RSD = vabRSD
080300170418     C                   eval      UBBRTETIDS.PIN_IND = vabIND
080400170418     C                   eval      UBBRTETIDS.PIN_CAD = vabCAD
080500170418     C                   eval      UBBRTETIDS.PIN_LOD = vabLOD
080600170418     C                   eval      UBBRTETIDS.PIN_PRD = vabPRD
080700170418     C                   eval      UBBRTETIDS.PIN_NZD = vabNZD
080800170419 xxx C                   eval      UBBRTETIDS.PIN_ISA = *blanks
080900170419     C                   eval      UBBRTETIDS.PIN_FLS = vabLNP
081000170418     C                   eval      UBBRTETIDS.PIN_NRS = vabNRS
081100170418     C                   eval      UBBRTETIDS.PIN_NCD = vabNCD
081200170418     C                   if        UBBRTETIDS.PIN_NCA = *zeros
081300170418     C                   eval      UBBRTETIDS.PIN_NCA = vabNCD + vabNCL - 1
081400170418     C                   endif
081500170418     C                   eval      UBBRTETIDS.PIN_LNA = vabLNA
081600170418     C                   eval      UBBRTETIDS.PIN_ZNC = vabZNC
081700170418     C                   eval      UBBRTETIDS.PIN_NC2 = vabNCL
081800170418     C                   eval      UBBRTETIDS.PIN_PKG = vabPKB
081900170418     C                   eval      UBBRTETIDS.PIN_VOL = vabVLB
082000171010     C*
082100171010     C                   if        vabRMA <> *blanks
082200171010     C                   eval      UBBRTETIDS.PIN_CDP = vabRMA
082300171010     C                   else
082400171010     C                   eval      UBBRTETIDS.PIN_CDP = %editc(vabRMN:'4')
082500171010     C                   endif
082600170418     C*
082700170418     C                   endsl
082800170418     C*
082900170418     C                   endsr
083000170413
083100170418
083200170413
083300170413     C     outMSG        begsr
083400170413     C*
083500170413     C                   select
083600170413     C                   when      UBSGU1IDS.ISGU1ILIN = 'IT'
083700170413     C*
083800170413     C                   select
083900170413     C                   when      UBSGU1IDS.OSGU1IESI = *zeros
084000170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'OK'
084100170413     C                   when      UBSGU1IDS.OSGU1IESI = -01
084200170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err generico'
084300170413     C                   when      UBSGU1IDS.OSGU1IESI = -02
084400170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err parametri'
084500170418     C                   when      UBSGU1IDS.OSGU1IESI = -10
084600170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err stampa segnacollo'
084700170413     C                   when      UBSGU1IDS.OSGU1IESI = -20
084800170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err conversione spool'
084900170803     C                   when      UBSGU1IDS.OSGU1IESI = -30
085000170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err spostamento file'
085100170803     C                   when      UBSGU1IDS.OSGU1IESI = -40
085200170803     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err reperimento file'
085300170803     C                   when      UBSGU1IDS.OSGU1IESI = -50
085400170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err Tab non trovata'
085500170413     C                   endsl
085600170413     C*
085700170413     C                   when      UBSGU1IDS.ISGU1ILIN = 'EN'
085800170413     C*
085900170413     C                   select
086000170413     C                   when      UBSGU1IDS.OSGU1IESI = *zeros
086100170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'OK'
086200170413     C                   when      UBSGU1IDS.OSGU1IESI = -01
086300170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Generic Err'
086400170413     C                   when      UBSGU1IDS.OSGU1IESI = -02
086500170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Parameter Err'
086600170418     C                   when      UBSGU1IDS.OSGU1IESI = -10
086700170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Label Err'
086800170418     C                   when      UBSGU1IDS.OSGU1IESI = -20
086900170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err convert spool'
087000170418     C                   when      UBSGU1IDS.OSGU1IESI = -30
087100170418     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err move file'
087200170803     C                   when      UBSGU1IDS.OSGU1IESI = -40
087300170803     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err retrieve file'
087400170413     C                   when      UBSGU1IDS.OSGU1IESI = -50
087500170413     C                   eval      UBSGU1IDS.OSGU1IMSG = 'Err Tab not found'
087600170413     C                   endsl
087700170413     C*
087800170413     C                   endsl
087900170413     C*
088000170413     C                   eval      UBSGU1IDS.OSGU1IMSG =
088100170413     C                             %trim(UBSGU1IDS.OSGU1IMSG)+
088200170413     C                             ' (' + %trim(wMSG) + ')'
088300171016     C*
088400171016     C                   seton                                        lr
088500170413     C*
088600170413     C                   endsr
088700170418
088800170418
088900170418
089000170418     C*
089100170418     C     addSlash      begsr
089200170418     C*
089300170418     C                   if        %subst(wPath:%len(%trim(wPath)):1) = '/'
089400170418     C                   else
089500170418     C                   eval      wPath = %trim(wPath) + '/'
089600170418     C                   endif
089700170418     C*
089800170418     C                   endsr
089900000714
090000050920
090100050920
090200000714      /TITLE Routine di *init PGM
090300000714     C*
090400000714     C     *inzsr        begsr
090500060103     C*
090600060103     C     *entry        plist
090700170413     C                   parm                    UBSGU1IDS
090800170804     C                   parm                    UBSGU1OLEN
090900170803     C                   parm                    UBSGU1OUT
091000000714     C*
091100000714     C                   endsr
091200000714
