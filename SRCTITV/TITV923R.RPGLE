000100080922      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR/FIVADWWR.
000200130327     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040119     FFIVATwwr  O    E             DISK    usropn
000700080922     FFIVADwwr  O    E             DISK    usropn
000800170519     FEDIVABwr  O    E             DISK    usropn prefix(e_)
000900170519     FEDIVATwr  O    E             DISK    usropn prefix(e_)
001000170519     FEDIVADwr  O    E             DISK    usropn prefix(e_)
001100000313     D*
001200040113     D*------------
001300040113     D* SCHIEDE A PROGRAMMA
001400040113     D*------------
001500040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900170519     D FIVABDS       e ds                  extname(fivabwwr)
002000170519     D FIVATDS       e ds                  extname(fivatwwr)
002100170519     D FIVADDS       e ds                  extname(fivadwwr)
002200170519     D EDIVABDS      e ds                  extname(edivabwr) prefix(e_)
002300170519     D EDIVATDS      e ds                  extname(edivatwr) prefix(e_)
002400170519     D EDIVADDS      e ds                  extname(edivadwr) prefix(e_)
002500010330     D dscmz         e ds                  inz
002600010330     D psds           sds
002700010330     D  procname         *PROC
002800170519     D tivlrds       e ds                  extname(tivlr00f)
002900040128     D tisi95ds      e ds
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003600040510     D parccm          s              8    INZ(*blanks)
003700040119     D parmbr          s             10    INZ(*blanks)
003800040119     D paropz          s              1    INZ(*blanks)
003900040119     D chkcall         s              1    INZ(*blanks)
004000170403     D s_vindta        s                   like(vindta) inz(*blanks)
004100030715     D*------------------
004200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004300030715     D*------------------
004400030715     D SkSplitFLD      S             10    DIM(100)
004500030715     D SkSplitCSV      S            256    DIM(100)
004600030715     D CharCSV         S              1
004700030715     D CharTXT         S              1
004800030715     D CharNUM         S              1
004900090403     D CharSOS         S              1
005000030715     D posDa           S              3  0 INZ(*zeros)
005100030715     D posA            S              3  0 INZ(*zeros)
005200030715     D i               s              3  0 INZ(1)
005300030715     D wGiro           s              1  0 INZ(*zeros)
005400151209     D wNOT_P_OPZ      s              2    INZ
005500170629     D wVABCMR         s                   LIKE(e_VABCMR)
005600060331     D wVATNOT_A       s                   LIKE(VATNOT)
005700060331     D wVATNOT_B       s                   LIKE(VATNOT)
005800060331     D wVATNOT_E       s                   LIKE(VATNOT)
005900110912     D wVATNOT_I       s                   LIKE(VATNOT)
006000110912     D wVATNOT_J       s                   LIKE(VATNOT)
006100110912     D wVATNOT_4       s                   LIKE(VATNOT)
006200131018     D wVATNOT_S       s                   LIKE(VATNOT)
006300140123     D wVATNOT_P       s                   LIKE(VATNOT)
006400141128     D wVATNOT_Y       s                   LIKE(VATNOT)
006500060331     D wVABNSP         s                   LIKE(VABNSP) INZ
006600140130     D wVADCDP_01      s                   LIKE(VADCDP)
006700140130     D wVADCDP_02      s                   LIKE(VADCDP)
006800140130     D wVADCDP_03      s                   LIKE(VADCDP)
006900140130     D wVADCDP_04      s                   LIKE(VADCDP)
007000140130     D wVADCDP_05      s                   LIKE(VADCDP)
007100140130     D wVADCDP_06      s                   LIKE(VADCDP)
007200140130     D wVADCDP_07      s                   LIKE(VADCDP)
007300140130     D wVADCDP_08      s                   LIKE(VADCDP)
007400140130     D wVADCDP_09      s                   LIKE(VADCDP)
007500140130     D wVADCDP_10      s                   LIKE(VADCDP)
007600140130     D wVADCDP_11      s                   LIKE(VADCDP)
007700140130     D wVADCDP_12      s                   LIKE(VADCDP)
007800140130     D wVADCDP_13      s                   LIKE(VADCDP)
007900140130     D wVADCDP_14      s                   LIKE(VADCDP)
008000140130     D wVADCDP_15      s                   LIKE(VADCDP)
008100140130     D wVADCDP_16      s                   LIKE(VADCDP)
008200140130     D wVADCDP_17      s                   LIKE(VADCDP)
008300140130     D wVADCDP_18      s                   LIKE(VADCDP)
008400140130     D wVADCDP_19      s                   LIKE(VADCDP)
008500140130     D wVADCDP_20      s                   LIKE(VADCDP)
008600140130     D wVADCDP_21      s                   LIKE(VADCDP)
008700140130     D wVADCDP_22      s                   LIKE(VADCDP)
008800140130     D wVADCDP_23      s                   LIKE(VADCDP)
008900140130     D wVADCDP_24      s                   LIKE(VADCDP)
009000140130     D wVADCDP_25      s                   LIKE(VADCDP)
009100140130     D wVADCDP_26      s                   LIKE(VADCDP)
009200140130     D wVADCDP_27      s                   LIKE(VADCDP)
009300140130     D wVADCDP_28      s                   LIKE(VADCDP)
009400140130     D wVADCDP_29      s                   LIKE(VADCDP)
009500140130     D wVADCDP_30      s                   LIKE(VADCDP)
009600140130
009700140130     D wVADNCL_01      s                   LIKE(VADNCL)
009800140130     D wVADNCL_02      s                   LIKE(VADNCL)
009900140130     D wVADNCL_03      s                   LIKE(VADNCL)
010000140130     D wVADNCL_04      s                   LIKE(VADNCL)
010100140130     D wVADNCL_05      s                   LIKE(VADNCL)
010200140130     D wVADNCL_06      s                   LIKE(VADNCL)
010300140130     D wVADNCL_07      s                   LIKE(VADNCL)
010400140130     D wVADNCL_08      s                   LIKE(VADNCL)
010500140130     D wVADNCL_09      s                   LIKE(VADNCL)
010600140130     D wVADNCL_10      s                   LIKE(VADNCL)
010700140130     D wVADNCL_11      s                   LIKE(VADNCL)
010800140130     D wVADNCL_12      s                   LIKE(VADNCL)
010900140130     D wVADNCL_13      s                   LIKE(VADNCL)
011000140130     D wVADNCL_14      s                   LIKE(VADNCL)
011100140130     D wVADNCL_15      s                   LIKE(VADNCL)
011200140130     D wVADNCL_16      s                   LIKE(VADNCL)
011300140130     D wVADNCL_17      s                   LIKE(VADNCL)
011400140130     D wVADNCL_18      s                   LIKE(VADNCL)
011500140130     D wVADNCL_19      s                   LIKE(VADNCL)
011600140130     D wVADNCL_20      s                   LIKE(VADNCL)
011700140130     D wVADNCL_21      s                   LIKE(VADNCL)
011800140130     D wVADNCL_22      s                   LIKE(VADNCL)
011900140130     D wVADNCL_23      s                   LIKE(VADNCL)
012000140130     D wVADNCL_24      s                   LIKE(VADNCL)
012100140130     D wVADNCL_25      s                   LIKE(VADNCL)
012200140130     D wVADNCL_26      s                   LIKE(VADNCL)
012300140130     D wVADNCL_27      s                   LIKE(VADNCL)
012400140130     D wVADNCL_28      s                   LIKE(VADNCL)
012500140130     D wVADNCL_29      s                   LIKE(VADNCL)
012600140130     D wVADNCL_30      s                   LIKE(VADNCL)
012700030822     D*------------------
012800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
012900030822     D*------------------
013000030822     D posDaDft        S              3  0 INZ(*zeros)
013100030822     D posADft         S              3  0 INZ(*zeros)
013200030822     D j               s              3  0 INZ(1)
013300030822     D wGiroDft        s              1  0 INZ(*zeros)
013400031201     D*------------------
013500031201     D* Costanti
013600031201     D*------------------
013700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
013800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
013900170403
014000170403     D*------------------
014100170403     D* DS REPERIMENTO NUMERATORE
014200170403     D*------------------
014300170403     D trul33ds      e ds                  inz
014400170403     D*------------------
014500170403     D* DS ARCHITETTURA
014600170403     D*------------------
014700170403     D KPJBA         e ds                  inz
014800170403
014900010330
015000010330
015100000913     C                   reset                   rrnum
015200990921     C                   reset                   esito
015300990921     C                   reset                   wrkesito
015400010601     C*
015500010601     C                   exsr      opeini
015600010605     C                   exsr      rwvab
015700170505     C                   exsr      endela
015800010601     C*
015900010601     C                   seton                                        lr
016000010601
016100010601
016200010601
016300010601     C*--------------------------------------------------------
016400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
016500010601     C*--------------------------------------------------------
016600010601     C     PREELA        BEGSR
016700010601     C*
016800010601     C* SE OCCORRE SPEDIRE IN FILIALE
016900010601     C                   if        invfil <> *zeros and
017000010601     C                             flgGiro = '0'
017100010601     C*
017200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
017300010601     C                   eval      flgGiro = '1'
017400010601     C*
017500010601     C                   endif
017600010601     C*
017700010601     C                   ENDSR
017800010601     C***
017900170505
018000170505
018100170505
018200170505
018300170505     C*--------------------------------------------------------
018400170505     C* EXECLOSE - OPERAZIONI DI CHIUSURA FILE                *
018500170505     C*--------------------------------------------------------
018600170505     C     EXECLOSE      BEGSR
018700170505     C*
018800170505     C                   if        %open(tivin00r)
018900170505     C                   close     tivin00r
019000170505     C                   endif
019100170519     C  N77              if        %open(fivabwwr)
019200170519     C                   close     fivabwwr
019300170505     C                   endif
019400170519     C  N77              if        %open(fivatwwr)
019500170519     C                   close     fivatwwr
019600170505     C                   endif
019700170519     C  N77              if        %open(fivadwwr)
019800170519     C                   close     fivadwwr
019900170505     C                   endif
020000170519     C   77              if        %open(edivabwr)
020100170519     C                   close     edivabwr
020200170519     C                   endif
020300170519     C   77              if        %open(edivatwr)
020400170519     C                   close     edivatwr
020500170519     C                   endif
020600170519     C   77              if        %open(edivadwr)
020700170519     C                   close     edivadwr
020800170519     C                   endif
020900170505     C*
021000170505     C                   ENDSR
021100170505     C***
021200010601
021300010601
021400010601
021500010601
021600010601     C*--------------------------------------------------------
021700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
021800010601     C*--------------------------------------------------------
021900010601     C     ENDELA        BEGSR
022000050201     C*
022100050201     C* Effettuo lancio TISI95 solo x chiusura
022200050201     C                   CLEAR                   TISI95DS
022300050201     C                   EVAL      I95TLA = 'C'
022400050201     C                   CALL      'TISI95R'
022500050201     C                   PARM                    TISI95DS
022600170509     C*
022700170509     C* Chiusura controllata file
022800170509     C                   exsr      exeClose
022900080930     C*
023000140123     C* Effettuo la chiamata al CLLE preposto (solo se non richiesta forzat. per PDF)
023100140123     C                   if        not *in53
023200170519     C  N77              call(e)   'TITVVDTC'
023300140123     C                   parm                    parccm
023400140123     C                   parm                    parmbr
023500140123     C                   parm      '2'           paropz
023600170519     C   77              call(e)   'TITVEVDTC'
023700170519     C                   parm                    parccm
023800170519     C                   parm                    parmbr
023900170519     C                   parm      '2'           paropz
024000140123     C                   endif
024100170731     C*
024200170731     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
024300170731     C                   if        *in53
024400170731     C  N77              call      'TIS7P2SR'
024500170731     C                   parm      'C'           pIn_Opz           1
024600170731     C                   parm                    tivlrds
024700170731     C                   parm                    FIVABDS
024800170731     C                   parm      *blanks       pIn_CdIdAz        2
024900170731     C                   parm      *blanks       pIn_MaskPDF      50
025000170731     C                   parm      *blanks       pOut_Esito        1
025100170731     C*
025200170731     C   77              call      'TIS7P2SER'
025300170731     C                   parm      'C'           pIn_Opz           1
025400170731     C                   parm                    tivlrds
025500170731     C                   parm                    EDIVABDS
025600170731     C                   parm      *blanks       pIn_CdIdAz        2
025700170731     C                   parm      *blanks       pIn_MaskPDF      50
025800170731     C                   parm      *blanks       pOut_Esito        1
025900170731     C                   endif
026000000616     C*
026100010601     C                   ENDSR
026200010601     C***
026300000613
026400010601
026500010601
026600010330     C*--------------------------------------------------------
026700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
026800010330     C*--------------------------------------------------------
026900010605     C     RWVAB         BEGSR
027000010330     C*
027100010330     C                   if        not %open(tivin00r)
027200010330     C                   open      tivin00r
027300010330     C                   endif
027400170519     C  N77              if        not %open(fivabwwr)
027500170519     C                   open      fivabwwr
027600010330     C                   endif
027700170519     C   77              if        not %open(edivabwr)
027800170519     C                   open      edivabwr
027900170519     C                   endif
028000040119     C*
028100080922     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR e FIVADWWR
028200080922     C                   exsr      prevatvad
028300040119     C*
028400040119     C                   if        chkcall = '0'
028500040119     C*
028600170519     C  N77              if        not %open(fivatwwr)
028700170519     C                   open      fivatwwr
028800170519     C                   endif
028900170519     C  N77              if        not %open(fivadwwr)
029000080922     C                   open      fivadwwr
029100080922     C                   endif
029200170519     C   77              if        not %open(edivatwr)
029300170519     C                   open      edivatwr
029400170519     C                   endif
029500170519     C   77              if        not %open(edivadwr)
029600170519     C                   open      edivadwr
029700170519     C                   endif
029800150219     C*
029900150219     C* Reimposto i valori di default
030000150219     C                   EXSR      DEFCAM
030100140123     C*
030200140123     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
030300140123     C                   if        *in53
030400170519     C  N77              call      'TIS7P2SR'
030500170519     C                   parm      'O'           pIn_Opz           1
030600170519     C                   parm                    tivlrds
030700170519     C                   parm                    FIVABDS
030800170519     C                   parm      *blanks       pIn_CdIdAz        2
030900170519     C                   parm      *blanks       pIn_MaskPDF      50
031000170519     C                   parm      *blanks       pOut_Esito        1
031100170519     C*
031200170519     C   77              call      'TIS7P2SER'
031300140123     C                   parm      'O'           pIn_Opz           1
031400140123     C                   parm                    tivlrds
031500170519     C                   parm                    EDIVABDS
031600140123     C                   parm      *blanks       pIn_CdIdAz        2
031700140123     C                   parm      *blanks       pIn_MaskPDF      50
031800140123     C                   parm      *blanks       pOut_Esito        1
031900140123     C                   endif
032000010330     C*
032100040119     C                   clear                   §CTROKVB          7 0
032200040119     C                   clear                   §CTROKVT          7 0
032300080922     C                   clear                   §CTROKVD          7 0
032400040119     C                   clear                   §CTRMO            7 0
032500040119     C                   clear                   §CTRNO            7 0
032600010330     C*
032700030822     C                   do        *HIVAL
032800010330     C*
032900030822     C                   read      tivin00r                               70
033000170403     C*
033100170403     C* Salvo il buffer orginale ricevuto dal cliente
033200170403     C                   eval      s_vindta = vindta
033300170403     C*
033400170403     C                   if        *in70 = *off and
033500170403     C                             vindta <> *blanks and
033600170403     C                             (vinflg = *blanks
033700170403     C                              or vinflg = '0'
033800170403     C                              or vinflg = '2')
033900170403     C*
034000170403     C* Controlli di "base" sui dati grezzi
034100170403     C                   exsr      normCSV
034200170403     C*
034300170403     C                   if        wFlgVINDTA = 1
034400010330     C*
034500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
034600010711     C                   if        vinflg = *blanks or vinflg = '0'
034700010711     C                   clear                   vinmsg
034800010711     C                   endif
034900010601     C*
035000010330     C                   exsr      inzvar
035100010605     C                   exsr      impvab
035200010601     C*
035300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
035400010605     C                   eval      depfil = VABLNP
035500010601     C                   exsr      repfil
035600010601     C                   if        depfil = invfil
035700021025     C                   if        vlrpoi = 999
035800030822     C                   move(P)   invfil        VABFGS
035900070704     C                   move(P)   invfil        winvcmz           3
036000021025     C                   else
036100030822     C                   move(P)   vlrpoi        VABFGS
036200070704     C                   move(P)   vlrpoi        winvcmz
036300021025     C                   endif
036400010601     C*
036500010601     C                   exsr      PREELA
036600010601     C*
036700010604     C* Ebbene...
036800010604     C*
036900040119     C  N31              add       1             §CTROKVB
037000040119     C   32              add       1             §CTRMO
037100040119     C   31              add       1             §CTRNO
037200030822     C                   if        wGiro = 2
037300151214     C* Verifico codice opzone per packing-list PDF
037400151214     C                   if        wNOT_P_OPZ = *blanks
037500151214     C                   eval      wNOT_P_OPZ = '10'
037600151214     C                   endif
037700040510     C* Scrivo sempre il VAT
037800040510     C  N31              exsr      wrivat
037900080922     C* Scrivo sempre il VAD
038000080922     C  N31              exsr      wrivad
038100040510     C* Scrivo il VAB solo a rottura d spedizione
038200040119     C                   if        depspe =  *zeros OR
038300170403     C                             depspe <> vabNSP
038400040119     C  N31              exsr      wrivab
038500040119     C  N31              eval      depspe = vabNSP
038600040119     C                   endif
038700040119     C                   endif
038800010604     C*
038900010604     C                   if        *in31 = *off and
039000010604     C                             *in32 = *off
039100010604     C                   eval      vinflg = '1'
039200010604     C                   else
039300010604     C                   eval      vinflg = '2'
039400010604     C                   endif
039500170403     C*
039600170403     C                   else
039700170403     C                   eval      vinflg = '1'
039800170403     C                   endif
039900170403     C*
040000170403     C                   else
040100170403     C                   eval      vinflg = '1'
040200170403     C                   endif
040300010604     C*
040400010330     C                   else
040500010330     C                   eval      vinflg = '1'
040600010330     C                   endif
040700010601     C*
040800170403     C                   eval      vindta = s_vindta
040900010601     C  N70              update    tivin000
041000010330     C*
041100030822     C  N70              enddo
041200040119     C*
041300040119     C                   endif                                                  (endif - chkcall)
041400010601     C*
041500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
041600010601     C                   if        cntNonEl = *zeros or
041700010601     C                             flgMulti = '0'
041800010330     C* Se non ci sono record con errori ...
041900010601     C                   if        §ctrno = 0 and
042000010604     C                             §ctrmo = 0 and
042100010601     C                             flgStato <> '2'
042200010330     C* ... restituisco esito OK.
042300010330     C                   eval      wrkesito = '0'
042400010330     C                   else
042500040119     C                   if        §ctrokvb > 0
042600010330     C                   eval      wrkesito = '1'
042700010330     C                   else
042800010615     C                   if        flgOk = '0'
042900010615     C                   eval      wrkesito = '2'
043000010615     C                   else
043100010615     C                   eval      wrkesito = '6'
043200010615     C                   endif
043300010330     C                   endif
043400010330     C                   endif
043500010601     C                   else
043600010601     C                   eval      wrkesito = '9'
043700010601     C                   endif
043800010601     C*
043900010601     C                   if        vlrpoi <> 999
044000010601     C                   eval      invfil = vlrpoi
044100010601     C                   endif
044200170504     C*
044300170504     C                   if        §ctrokvb > 0
044400170504     C                             and invfil > *zeros
044500170505     C                   exsr      exeClose
044600170504     C                   exsr      invio
044700170504     C                   endif
044800010330     C*
044900010330     C                   ENDSR
045000010330     C***
045100040119
045200040119
045300040119
045400040119     C*----------------------------------------------------*
045500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
045600040119     C*----------------------------------------------------*
045700040510     C     WRIVAB        BEGSR
045800040119     C*
045900060331     C* Gestisco l'eventuale rottura x numero spedizione
046000060331     C                   if        VABNSP <> wVABNSP
046100140123     C*
046200140123     C* Se richiesta scrittura del tipo record 'P' del VAT (x packing list PDF)
046300140123     C                   if        *in53
046400140123     C                   eval      pIn_MaskPDF = '&0RMN*.pdf'
046500170519     C  N77              call      'TIS7P2SR'
046600170519     C                   parm      'W'           pIn_Opz           1
046700170519     C                   parm                    tivlrds
046800170519     C                   parm                    FIVABDS
046900170519     C                   parm      wNOT_P_OPZ    pIn_CdIdAz        2
047000170519     C                   parm                    pIn_MaskPDF      50
047100170519     C                   parm      *blanks       pOut_Esito        1
047200170519     C*
047300170519     C   77              eval      EDIVABDS = FIVABDS
047400170519     C   77              call      'TIS7P2SER'
047500140123     C                   parm      'W'           pIn_Opz           1
047600140123     C                   parm                    tivlrds
047700170519     C                   parm                    EDIVABDS
047800151214     C                   parm      wNOT_P_OPZ    pIn_CdIdAz        2
047900140123     C                   parm                    pIn_MaskPDF      50
048000140123     C                   parm      *blanks       pOut_Esito        1
048100140123     C                   endif
048200140123     C*
048300070301     C                   if        wVATNOT_E <> *blanks
048400150710     C                   if        VABCTM = *blanks
048500070301     C                   eval      VABCTM = '7Q'
048600150710     C                   endif
048700150710     C                   endif
048800170519     C  N77              write     fivab000                                     => scarico il VAB
048900170519     C   77              exsr      wriEVAB
049000060331     C                   eval      wVABNSP = VABNSP
049100060331     C                   endif
049200040119     C*
049300040119     C                   ENDSR
049400040119
049500040119
049600040119
049700040119     C*----------------------------------------------------*
049800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
049900040119     C*----------------------------------------------------*
050000040510     C     WRIVAT        BEGSR
050100060512     C*
050200060512     C* Inizializzo i campi chiave
050300060512     C                   eval      VATFGS = VABFGS
050400080922     C                   eval      VATCCM = VABCCM
050500060512     C                   eval      VATAAS = VABAAS
050600080922     C                   eval      VATLNP = VABLNP
050700060512     C                   eval      VATNSP = VABNSP
050800080922     C                   eval      VATNRS = VABNRS
050900040714     C*
051000040714     C* Solo se VATNOT valorizzato scarico il buffer
051100040714     C                   if        VATNOT <> *blanks
051200040119     C*
051300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
051400040510     C                   if        VATTRC = *blanks
051500040510     C                   eval      VATTRC = 'E'
051600040510     C                   endif
051700170519     C  N77              write     FIVAT000
051800170519     C   77              exsr      wriEVAT
051900040119     C                   add       1             §CTROKVT
052000040714     C*
052100040714     C                   endif
052200060331     C*
052300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
052400060331     C                   if        wVATNOT_A <> *blanks
052500060331     C                   eval      VATNOT = wVATNOT_A
052600060331     C                   eval      VATTRC = 'A'
052700170519     C  N77              write     FIVAT000
052800170519     C   77              exsr      wriEVAT
052900060331     C                   add       1             §CTROKVT
053000060331     C                   endif
053100060331     C*
053200060331     C                   if        wVATNOT_B <> *blanks
053300060331     C                   eval      VATNOT = wVATNOT_B
053400060331     C                   eval      VATTRC = 'B'
053500170519     C  N77              write     FIVAT000
053600170519     C   77              exsr      wriEVAT
053700060331     C                   add       1             §CTROKVT
053800060331     C                   endif
053900060331     C*
054000060331     C                   if        wVATNOT_E <> *blanks
054100060331     C                   eval      VATNOT = wVATNOT_E
054200060331     C                   eval      VATTRC = 'E'
054300170519     C  N77              write     FIVAT000
054400170519     C   77              exsr      wriEVAT
054500060331     C                   add       1             §CTROKVT
054600060331     C                   endif
054700110912     C*
054800110912     C                   if        wVATNOT_I <> *blanks
054900110912     C                   eval      VATNOT = wVATNOT_I
055000110912     C                   eval      VATTRC = 'I'
055100170519     C  N77              write     FIVAT000
055200170519     C   77              exsr      wriEVAT
055300110912     C                   add       1             §CTROKVT
055400110912     C                   endif
055500110912     C*
055600110912     C                   if        wVATNOT_J <> *blanks
055700110912     C                   eval      VATNOT = wVATNOT_J
055800110912     C                   eval      VATTRC = 'J'
055900170519     C  N77              write     FIVAT000
056000170519     C   77              exsr      wriEVAT
056100110912     C                   add       1             §CTROKVT
056200110912     C                   endif
056300110912     C*
056400110912     C                   if        wVATNOT_4 <> *blanks
056500110912     C                   eval      VATNOT = wVATNOT_4
056600110912     C                   eval      VATTRC = '4'
056700170519     C  N77              write     FIVAT000
056800170519     C   77              exsr      wriEVAT
056900110912     C                   add       1             §CTROKVT
057000110912     C                   endif
057100131018     C*
057200131018     C                   if        wVATNOT_S <> *blanks
057300131018     C                   eval      VATNOT = wVATNOT_S
057400131018     C                   eval      VATTRC = 'S'
057500170519     C  N77              write     FIVAT000
057600170519     C   77              exsr      wriEVAT
057700131018     C                   add       1             §CTROKVT
057800131018     C                   endif
057900140123     C*
058000140123     C                   if        wVATNOT_P <> *blanks
058100140123     C                   eval      VATNOT = wVATNOT_P
058200140123     C                   if        %subst(VATNOT:34:2) = *blanks
058300151209     C                   eval      %subst(VATNOT:34:2) = wNOT_P_OPZ             * da parametri anag.
058400140123     C                   endif
058500140123     C                   eval      VATTRC = 'P'
058600170519     C  N77              write     FIVAT000
058700170519     C   77              exsr      wriEVAT
058800140123     C                   add       1             §CTROKVT
058900140123     C                   endif
059000141128     C*
059100141128     C                   if        wVATNOT_Y <> *blanks
059200141128     C                   eval      VATNOT = wVATNOT_Y
059300141128     C                   eval      VATTRC = 'Y'
059400170519     C  N77              write     FIVAT000
059500170519     C   77              exsr      wriEVAT
059600141128     C                   add       1             §CTROKVT
059700141128     C                   endif
059800040119     C*
059900040119     C                   ENDSR
060000080922
060100080922
060200080922
060300080922     C*----------------------------------------------------*
060400080922     C*  SCARICAMENTO BUFFER RECORDS VAD
060500080922     C*----------------------------------------------------*
060600080922     C     WRIVAD        BEGSR
060700080922     C*
060800080922     C* Inizializzo i campi chiave
060900080922     C                   eval      VADFGS = VABFGS
061000080922     C                   eval      VADCCM = VABCCM
061100080922     C                   eval      VADAAS = VABAAS
061200080922     C                   eval      VADLNP = VABLNP
061300080922     C                   eval      VADNSP = VABNSP
061400080922     C                   eval      VADNRS = VABNRS
061500080922     C*
061600140130     C* Gestisco codice prodotto 01
061700140130     C                   if        wVADNCL_01 > *zeros
061800140130     C                   eval      VADCDP = wVADCDP_01
061900140130     C                   eval      VADNCL = wVADNCL_01
062000170519     C  N77              write     FIVAD000
062100170519     C   77              exsr      wriEVAD
062200080922     C                   add       1             §CTROKVD
062300080922     C                   endif
062400080922     C*
062500140130     C* Gestisco codice prodotto 02
062600140130     C                   if        wVADNCL_02 > *zeros
062700140130     C                   eval      VADCDP = wVADCDP_02
062800140130     C                   eval      VADNCL = wVADNCL_02
062900170519     C  N77              write     FIVAD000
063000170519     C   77              exsr      wriEVAD
063100080922     C                   add       1             §CTROKVD
063200080922     C                   endif
063300080922     C*
063400140130     C* Gestisco codice prodotto 03
063500140130     C                   if        wVADNCL_03 > *zeros
063600140130     C                   eval      VADCDP = wVADCDP_03
063700140130     C                   eval      VADNCL = wVADNCL_03
063800170519     C  N77              write     FIVAD000
063900170519     C   77              exsr      wriEVAD
064000080922     C                   add       1             §CTROKVD
064100080922     C                   endif
064200120103     C*
064300140130     C* Gestisco codice prodotto 04
064400140130     C                   if        wVADNCL_04 > *zeros
064500140130     C                   eval      VADCDP = wVADCDP_04
064600140130     C                   eval      VADNCL = wVADNCL_04
064700170519     C  N77              write     FIVAD000
064800170519     C   77              exsr      wriEVAD
064900120103     C                   add       1             §CTROKVD
065000120103     C                   endif
065100120103     C*
065200140130     C* Gestisco codice prodotto 05
065300140130     C                   if        wVADNCL_05 > *zeros
065400140130     C                   eval      VADCDP = wVADCDP_05
065500140130     C                   eval      VADNCL = wVADNCL_05
065600170519     C  N77              write     FIVAD000
065700170519     C   77              exsr      wriEVAD
065800120103     C                   add       1             §CTROKVD
065900120103     C                   endif
066000140130     C*
066100140130     C* Gestisco codice prodotto 06
066200140130     C                   if        wVADNCL_06 > *zeros
066300140130     C                   eval      VADCDP = wVADCDP_06
066400140130     C                   eval      VADNCL = wVADNCL_06
066500170519     C  N77              write     FIVAD000
066600170519     C   77              exsr      wriEVAD
066700140130     C                   add       1             §CTROKVD
066800140130     C                   endif
066900140130     C*
067000140130     C* Gestisco codice prodotto 07
067100140130     C                   if        wVADNCL_07 > *zeros
067200140130     C                   eval      VADCDP = wVADCDP_07
067300140130     C                   eval      VADNCL = wVADNCL_07
067400170519     C  N77              write     FIVAD000
067500170519     C   77              exsr      wriEVAD
067600140130     C                   add       1             §CTROKVD
067700140130     C                   endif
067800140130     C*
067900140130     C* Gestisco codice prodotto 08
068000140130     C                   if        wVADNCL_08 > *zeros
068100140130     C                   eval      VADCDP = wVADCDP_08
068200140130     C                   eval      VADNCL = wVADNCL_08
068300170519     C  N77              write     FIVAD000
068400170519     C   77              exsr      wriEVAD
068500140130     C                   add       1             §CTROKVD
068600140130     C                   endif
068700140130     C*
068800140130     C* Gestisco codice prodotto 09
068900140130     C                   if        wVADNCL_09 > *zeros
069000140130     C                   eval      VADCDP = wVADCDP_09
069100140130     C                   eval      VADNCL = wVADNCL_09
069200170519     C  N77              write     FIVAD000
069300170519     C   77              exsr      wriEVAD
069400140130     C                   add       1             §CTROKVD
069500140130     C                   endif
069600140130     C*
069700140130     C* Gestisco codice prodotto 10
069800140130     C                   if        wVADNCL_10 > *zeros
069900140130     C                   eval      VADCDP = wVADCDP_10
070000140130     C                   eval      VADNCL = wVADNCL_10
070100170519     C  N77              write     FIVAD000
070200170519     C   77              exsr      wriEVAD
070300140130     C                   add       1             §CTROKVD
070400140130     C                   endif
070500140130     C*
070600140130     C* Gestisco codice prodotto 11
070700140130     C                   if        wVADNCL_11 > *zeros
070800140130     C                   eval      VADCDP = wVADCDP_11
070900140130     C                   eval      VADNCL = wVADNCL_11
071000170519     C  N77              write     FIVAD000
071100170519     C   77              exsr      wriEVAD
071200140130     C                   add       1             §CTROKVD
071300140130     C                   endif
071400140130     C*
071500140130     C* Gestisco codice prodotto 12
071600140130     C                   if        wVADNCL_12 > *zeros
071700140130     C                   eval      VADCDP = wVADCDP_12
071800140130     C                   eval      VADNCL = wVADNCL_12
071900170519     C  N77              write     FIVAD000
072000170519     C   77              exsr      wriEVAD
072100140130     C                   add       1             §CTROKVD
072200140130     C                   endif
072300140130     C*
072400140130     C* Gestisco codice prodotto 13
072500140130     C                   if        wVADNCL_13 > *zeros
072600140130     C                   eval      VADCDP = wVADCDP_13
072700140130     C                   eval      VADNCL = wVADNCL_13
072800170519     C  N77              write     FIVAD000
072900170519     C   77              exsr      wriEVAD
073000140130     C                   add       1             §CTROKVD
073100140130     C                   endif
073200140130     C*
073300140130     C* Gestisco codice prodotto 14
073400140130     C                   if        wVADNCL_14 > *zeros
073500140130     C                   eval      VADCDP = wVADCDP_14
073600140130     C                   eval      VADNCL = wVADNCL_14
073700170519     C  N77              write     FIVAD000
073800170519     C   77              exsr      wriEVAD
073900140130     C                   add       1             §CTROKVD
074000140130     C                   endif
074100140130     C*
074200140130     C* Gestisco codice prodotto 15
074300140130     C                   if        wVADNCL_15 > *zeros
074400140130     C                   eval      VADCDP = wVADCDP_15
074500140130     C                   eval      VADNCL = wVADNCL_15
074600170519     C  N77              write     FIVAD000
074700170519     C   77              exsr      wriEVAD
074800140130     C                   add       1             §CTROKVD
074900140130     C                   endif
075000140130     C*
075100140130     C* Gestisco codice prodotto 16
075200140130     C                   if        wVADNCL_16 > *zeros
075300140130     C                   eval      VADCDP = wVADCDP_16
075400140130     C                   eval      VADNCL = wVADNCL_16
075500170519     C  N77              write     FIVAD000
075600170519     C   77              exsr      wriEVAD
075700140130     C                   add       1             §CTROKVD
075800140130     C                   endif
075900140130     C*
076000140130     C* Gestisco codice prodotto 17
076100140130     C                   if        wVADNCL_17 > *zeros
076200140130     C                   eval      VADCDP = wVADCDP_17
076300140130     C                   eval      VADNCL = wVADNCL_17
076400170519     C  N77              write     FIVAD000
076500170519     C   77              exsr      wriEVAD
076600140130     C                   add       1             §CTROKVD
076700140130     C                   endif
076800140130     C*
076900140130     C* Gestisco codice prodotto 18
077000140130     C                   if        wVADNCL_18 > *zeros
077100140130     C                   eval      VADCDP = wVADCDP_18
077200140130     C                   eval      VADNCL = wVADNCL_18
077300170519     C  N77              write     FIVAD000
077400170519     C   77              exsr      wriEVAD
077500140130     C                   add       1             §CTROKVD
077600140130     C                   endif
077700140130     C*
077800140130     C* Gestisco codice prodotto 19
077900140130     C                   if        wVADNCL_19 > *zeros
078000140130     C                   eval      VADCDP = wVADCDP_19
078100140130     C                   eval      VADNCL = wVADNCL_19
078200170519     C  N77              write     FIVAD000
078300170519     C   77              exsr      wriEVAD
078400140130     C                   add       1             §CTROKVD
078500140130     C                   endif
078600140130     C*
078700140130     C* Gestisco codice prodotto 20
078800140130     C                   if        wVADNCL_20 > *zeros
078900140130     C                   eval      VADCDP = wVADCDP_20
079000140130     C                   eval      VADNCL = wVADNCL_20
079100170519     C  N77              write     FIVAD000
079200170519     C   77              exsr      wriEVAD
079300140130     C                   add       1             §CTROKVD
079400140130     C                   endif
079500140130     C*
079600140130     C* Gestisco codice prodotto 21
079700140130     C                   if        wVADNCL_21 > *zeros
079800140130     C                   eval      VADCDP = wVADCDP_21
079900140130     C                   eval      VADNCL = wVADNCL_21
080000170519     C  N77              write     FIVAD000
080100170519     C   77              exsr      wriEVAD
080200140130     C                   add       1             §CTROKVD
080300140130     C                   endif
080400140130     C*
080500140130     C* Gestisco codice prodotto 22
080600140130     C                   if        wVADNCL_22 > *zeros
080700140130     C                   eval      VADCDP = wVADCDP_22
080800140130     C                   eval      VADNCL = wVADNCL_22
080900170519     C  N77              write     FIVAD000
081000170519     C   77              exsr      wriEVAD
081100140130     C                   add       1             §CTROKVD
081200140130     C                   endif
081300140130     C*
081400140130     C* Gestisco codice prodotto 23
081500140130     C                   if        wVADNCL_23 > *zeros
081600140130     C                   eval      VADCDP = wVADCDP_23
081700140130     C                   eval      VADNCL = wVADNCL_23
081800170519     C  N77              write     FIVAD000
081900170519     C   77              exsr      wriEVAD
082000140130     C                   add       1             §CTROKVD
082100140130     C                   endif
082200140130     C*
082300140130     C* Gestisco codice prodotto 24
082400140130     C                   if        wVADNCL_24 > *zeros
082500140130     C                   eval      VADCDP = wVADCDP_24
082600140130     C                   eval      VADNCL = wVADNCL_24
082700170519     C  N77              write     FIVAD000
082800170519     C   77              exsr      wriEVAD
082900140130     C                   add       1             §CTROKVD
083000140130     C                   endif
083100140130     C*
083200140130     C* Gestisco codice prodotto 25
083300140130     C                   if        wVADNCL_25 > *zeros
083400140130     C                   eval      VADCDP = wVADCDP_25
083500140130     C                   eval      VADNCL = wVADNCL_25
083600170519     C  N77              write     FIVAD000
083700170519     C   77              exsr      wriEVAD
083800140130     C                   add       1             §CTROKVD
083900140130     C                   endif
084000140130     C*
084100140130     C* Gestisco codice prodotto 26
084200140130     C                   if        wVADNCL_26 > *zeros
084300140130     C                   eval      VADCDP = wVADCDP_26
084400140130     C                   eval      VADNCL = wVADNCL_26
084500170519     C  N77              write     FIVAD000
084600170519     C   77              exsr      wriEVAD
084700140130     C                   add       1             §CTROKVD
084800140130     C                   endif
084900140130     C*
085000140130     C* Gestisco codice prodotto 27
085100140130     C                   if        wVADNCL_27 > *zeros
085200140130     C                   eval      VADCDP = wVADCDP_27
085300140130     C                   eval      VADNCL = wVADNCL_27
085400170519     C  N77              write     FIVAD000
085500170519     C   77              exsr      wriEVAD
085600140130     C                   add       1             §CTROKVD
085700140130     C                   endif
085800140130     C*
085900140130     C* Gestisco codice prodotto 28
086000140130     C                   if        wVADNCL_28 > *zeros
086100140130     C                   eval      VADCDP = wVADCDP_28
086200140130     C                   eval      VADNCL = wVADNCL_28
086300170519     C  N77              write     FIVAD000
086400170519     C   77              exsr      wriEVAD
086500140130     C                   add       1             §CTROKVD
086600140130     C                   endif
086700140130     C*
086800140130     C* Gestisco codice prodotto 29
086900140130     C                   if        wVADNCL_29 > *zeros
087000140130     C                   eval      VADCDP = wVADCDP_29
087100140130     C                   eval      VADNCL = wVADNCL_29
087200170519     C  N77              write     FIVAD000
087300170519     C   77              exsr      wriEVAD
087400140130     C                   add       1             §CTROKVD
087500140130     C                   endif
087600140130     C*
087700140130     C* Gestisco codice prodotto 30
087800140130     C                   if        wVADNCL_30 > *zeros
087900140130     C                   eval      VADCDP = wVADCDP_30
088000140130     C                   eval      VADNCL = wVADNCL_30
088100170519     C  N77              write     FIVAD000
088200170519     C   77              exsr      wriEVAD
088300140130     C                   add       1             §CTROKVD
088400140130     C                   endif
088500080922     C*
088600080922     C* Verifica "quadratura" colli
088700170404     C                   z-add     *zeros        wTotNCL           5 0
088800170404     C                   eval      wTotNCL =
088900170404     C                             wVADNCL_01 + wVADNCL_02 + wVADNCL_03 +
089000140130     C                             wVADNCL_04 + wVADNCL_05 + wVADNCL_06 +
089100140130     C                             wVADNCL_07 + wVADNCL_08 + wVADNCL_09 +
089200140130     C                             wVADNCL_10 + wVADNCL_11 + wVADNCL_12 +
089300140130     C                             wVADNCL_13 + wVADNCL_14 + wVADNCL_15 +
089400140130     C                             wVADNCL_16 + wVADNCL_17 + wVADNCL_18 +
089500140130     C                             wVADNCL_19 + wVADNCL_20 + wVADNCL_21 +
089600140130     C                             wVADNCL_22 + wVADNCL_23 + wVADNCL_24 +
089700140130     C                             wVADNCL_25 + wVADNCL_26 + wVADNCL_27 +
089800140130     C                             wVADNCL_28 + wVADNCL_29 + wVADNCL_30
089900170404     C*
090000170404     C                   if        wTotNCL  > *zeros AND
090100170404     C                             wTotNCL <> VABNCL
090200080922     C                   SETON                                        32
090300080922     C                   EVAL      vinmsg = %trimr(vinmsg)
090400080922     C                             + ' ' + 'VABNCL/VADNCL'
090500080922     C                   endif
090600080922     C*
090700080922     C                   ENDSR
090800040119
090900040119
091000040119
091100040119     C*----------------------------------------------------*
091200080922     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR E FIVADWWR
091300040119     C*----------------------------------------------------*
091400080922     C     PREVATVAD     BEGSR
091500040119     C*
091600080922     C* Compongo il nome del membro da dare al FIVATWWR e al FIVADWWR
091700040119     C                   eval      parmbr = vlrhdl
091800040119     C                   movel     'M'           parmbr
091900040510     C                   eval      parccm = vlrksc
092000040119     C                   eval      paropz = '1'
092100040119     C* Effettuo la chiamata al CLLE preposto
092200170519     C  N77              call(e)   'TITVVDTC'
092300170519     C                   parm                    parccm
092400170519     C                   parm                    parmbr
092500170519     C                   parm                    paropz
092600170519     C   77              call(e)   'TITVEVDTC'
092700170519     C                   parm                    parccm
092800170519     C                   parm                    parmbr
092900170519     C                   parm                    paropz
093000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
093100040119     C                   if        %error
093200040119     C                   movel     '1'           chkcall
093300040119     C                   else
093400040119     C                   movel     '0'           chkcall
093500040119     C                   endif
093600040119     C*
093700040119     C                   ENDSR
093800010601
093900010601
094000010601
094100010330     C*----------------------------------------------------*
094200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
094300010330     C*----------------------------------------------------*
094400040119     C     INZVAR        BEGSR
094500010330     C*
094600030822     C* Inizializzo variabili di wrk
094700020204     C                   Z-ADD     *zeros        Num5_0            5 0
094800090403     C                   z-add     *zeros        invfil
094900170629     C                   MOVEL     *blanks       wVABCMR
095000060331     C                   MOVEL     *blanks       wVATNOT_A
095100060331     C                   MOVEL     *blanks       wVATNOT_B
095200060331     C                   MOVEL     *blanks       wVATNOT_E
095300110912     C                   MOVEL     *blanks       wVATNOT_I
095400110912     C                   MOVEL     *blanks       wVATNOT_J
095500110912     C                   MOVEL     *blanks       wVATNOT_4
095600131018     C                   MOVEL     *blanks       wVATNOT_S
095700140123     C                   MOVEL     *blanks       wVATNOT_P
095800141128     C                   MOVEL     *blanks       wVATNOT_Y
095900140130     C                   MOVEL     *blanks       wVADCDP_01
096000170531     C                   MOVEL     *blanks       wVADCDP_02
096100140130     C                   MOVEL     *blanks       wVADCDP_03
096200140130     C                   MOVEL     *blanks       wVADCDP_04
096300140130     C                   MOVEL     *blanks       wVADCDP_05
096400140130     C                   MOVEL     *blanks       wVADCDP_06
096500140130     C                   MOVEL     *blanks       wVADCDP_07
096600140130     C                   MOVEL     *blanks       wVADCDP_08
096700140130     C                   MOVEL     *blanks       wVADCDP_09
096800140130     C                   MOVEL     *blanks       wVADCDP_10
096900140130     C                   MOVEL     *blanks       wVADCDP_11
097000140130     C                   MOVEL     *blanks       wVADCDP_11
097100140130     C                   MOVEL     *blanks       wVADCDP_13
097200140130     C                   MOVEL     *blanks       wVADCDP_14
097300140130     C                   MOVEL     *blanks       wVADCDP_15
097400140130     C                   MOVEL     *blanks       wVADCDP_16
097500140130     C                   MOVEL     *blanks       wVADCDP_17
097600140130     C                   MOVEL     *blanks       wVADCDP_18
097700140130     C                   MOVEL     *blanks       wVADCDP_19
097800140130     C                   MOVEL     *blanks       wVADCDP_20
097900140130     C                   MOVEL     *blanks       wVADCDP_21
098000140130     C                   MOVEL     *blanks       wVADCDP_21
098100140130     C                   MOVEL     *blanks       wVADCDP_23
098200140130     C                   MOVEL     *blanks       wVADCDP_24
098300140130     C                   MOVEL     *blanks       wVADCDP_25
098400140130     C                   MOVEL     *blanks       wVADCDP_26
098500140130     C                   MOVEL     *blanks       wVADCDP_27
098600140130     C                   MOVEL     *blanks       wVADCDP_28
098700140130     C                   MOVEL     *blanks       wVADCDP_29
098800140130     C                   MOVEL     *blanks       wVADCDP_30
098900140130     C                   Z-ADD     *zeros        wVADNCL_01
099000140130     C                   Z-ADD     *zeros        wVADNCL_02
099100140130     C                   Z-ADD     *zeros        wVADNCL_03
099200140130     C                   Z-ADD     *zeros        wVADNCL_04
099300140130     C                   Z-ADD     *zeros        wVADNCL_05
099400140130     C                   Z-ADD     *zeros        wVADNCL_06
099500140130     C                   Z-ADD     *zeros        wVADNCL_07
099600140130     C                   Z-ADD     *zeros        wVADNCL_08
099700140130     C                   Z-ADD     *zeros        wVADNCL_09
099800140130     C                   Z-ADD     *zeros        wVADNCL_10
099900140130     C                   Z-ADD     *zeros        wVADNCL_11
100000140130     C                   Z-ADD     *zeros        wVADNCL_12
100100140130     C                   Z-ADD     *zeros        wVADNCL_13
100200140130     C                   Z-ADD     *zeros        wVADNCL_14
100300140130     C                   Z-ADD     *zeros        wVADNCL_15
100400140130     C                   Z-ADD     *zeros        wVADNCL_16
100500140130     C                   Z-ADD     *zeros        wVADNCL_17
100600140130     C                   Z-ADD     *zeros        wVADNCL_18
100700140130     C                   Z-ADD     *zeros        wVADNCL_19
100800140130     C                   Z-ADD     *zeros        wVADNCL_20
100900140130     C                   Z-ADD     *zeros        wVADNCL_21
101000140130     C                   Z-ADD     *zeros        wVADNCL_22
101100140130     C                   Z-ADD     *zeros        wVADNCL_23
101200140130     C                   Z-ADD     *zeros        wVADNCL_24
101300140130     C                   Z-ADD     *zeros        wVADNCL_25
101400140130     C                   Z-ADD     *zeros        wVADNCL_26
101500140130     C                   Z-ADD     *zeros        wVADNCL_27
101600140130     C                   Z-ADD     *zeros        wVADNCL_28
101700140130     C                   Z-ADD     *zeros        wVADNCL_29
101800140130     C                   Z-ADD     *zeros        wVADNCL_30
101900030822     C*
102000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
102100030822     C                   CLEAR                   FIVAB000
102200040119     C                   CLEAR                   FIVAT000
102300080922     C                   CLEAR                   FIVAD000
102400170519     C                   CLEAR                   EDIVAB00
102500170519     C                   CLEAR                   EDIVAT00
102600170519     C                   CLEAR                   EDIVAD00
102700030822     C                   CLEAR                   SkSplitCSV
102800030822     C                   EVAL      i = 1
102900030822     C                   EVAL      posDa = *zeros
103000030822     C                   EVAL      posA  = *zeros
103100030822     C*
103200030822     C* Reimposto i valori di default
103300030822     C                   EXSR      DEFCAM
103400030822     C*
103500010330     C                   ENDSR
103600010330     C*----------------------------------------------------*
103700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
103800010330     C*----------------------------------------------------*
103900010330     C     DEFCAM        BEGSR
104000030822     C*
104100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
104200030715     C* e delimitatore testo.
104300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
104400170403     C*
104500170403     C* Verifico eventuali valori particolari per il carattere separatore campo
104600170403     C                   IF        CharCSV = '5'
104700170403     C                   EVAL      CharCSV = x'05'
104800170403     C                   ENDIF
104900170403     C*
105000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
105100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
105200090403     C*
105300090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
105400090403     C                   EVAL      CharSOS = CharNUM
105500030822     C*
105600140123     C* Reperisco il flag che mi indica se richiesta generazione "automatica" record VATNOT_P
105700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
105800140123     C                   SETON                                        53
105900030715     C                   ELSE
106000140123     C                   SETOFF                                       53
106100030715     C                   ENDIF
106200170503     C*
106300170503     C* Reperisco il flag che mi indica se richiesto considerare RMN come NSP
106400170503     C                   IF        %subst(vlrppt:1:1) = 'R'
106500170503     C                   SETON                                        54
106600170503     C                   ELSE
106700170503     C                   SETOFF                                       54
106800170503     C                   ENDIF
106900170327     C*
107000170327     C* Reperisco il parametro che mi indica se sommare i valori di "testata"
107100170419     C                   IF        %subst(vlrppt:5:1) = 'S'
107200170327     C                   SETON                                        40
107300170327     C                   ELSE
107400170327     C                   SETOFF                                       40
107500170327     C                   ENDIF
107600030822     C*
107700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
107800030822     C                   EVAL      posDaDft = 1
107900030822     C                   EVAL      posADft  = 0
108000030822     C                   EVAL      wGiroDft = 0
108100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
108200030822     C                             posDaDft > 0
108300030822     C*
108400030822     C* Gestisco il 1° giro
108500030822     C                   IF        wGiroDft = 0
108600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
108700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
108800030822     C* Incremento il contatore dei "giri"
108900030822     C                   EVAL      wGiroDft = 1
109000030822     C                   ELSE
109100030822     C                   EVAL      posDaDft = posADft
109200030822     C                   ENDIF
109300030822     C* Eseguo lo scan x trovare la fine del campo corrente
109400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
109500030822     C*
109600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
109700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
109800030822     C                   IF        posDaDft > 0 AND
109900030822     C                             posADft  > 0
110000080922     C****** Campi "speciali" x gestione codici prodotto (Sol. A2)
110100080922     C* CP1
110200080922     C                   IF        %subst(
110300080922     C                             %subst(vlrppt:posDaDft+1:
110400080922     C                             posADft-posDaDft-1):1:3)
110500080922     C                             = 'CP1'
110600140130     C                   EVAL      wVADCDP_01=%trim(%subst(
110700080922     C                             %subst(vlrppt:posDaDft+1:
110800080922     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
110900080922     C                   ENDIF
111000080922     C* CP2
111100080922     C                   IF        %subst(
111200080922     C                             %subst(vlrppt:posDaDft+1:
111300080922     C                             posADft-posDaDft-1):1:3)
111400080922     C                             = 'CP2'
111500140130     C                   EVAL      wVADCDP_02=%trim(%subst(
111600080922     C                             %subst(vlrppt:posDaDft+1:
111700080922     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
111800080922     C                   ENDIF
111900080922     C* CP3
112000080922     C                   IF        %subst(
112100080922     C                             %subst(vlrppt:posDaDft+1:
112200080922     C                             posADft-posDaDft-1):1:3)
112300080922     C                             = 'CP3'
112400140130     C                   EVAL      wVADCDP_03=%trim(%subst(
112500080922     C                             %subst(vlrppt:posDaDft+1:
112600080922     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
112700080922     C                   ENDIF
112800120103     C* CP4
112900120103     C                   IF        %subst(
113000120103     C                             %subst(vlrppt:posDaDft+1:
113100120103     C                             posADft-posDaDft-1):1:3)
113200120103     C                             = 'CP4'
113300140130     C                   EVAL      wVADCDP_04=%trim(%subst(
113400120103     C                             %subst(vlrppt:posDaDft+1:
113500120103     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
113600120103     C                   ENDIF
113700120103     C* CP5
113800120103     C                   IF        %subst(
113900120103     C                             %subst(vlrppt:posDaDft+1:
114000120103     C                             posADft-posDaDft-1):1:3)
114100120103     C                             = 'CP5'
114200140130     C                   EVAL      wVADCDP_05=%trim(%subst(
114300120103     C                             %subst(vlrppt:posDaDft+1:
114400120103     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
114500120103     C                   ENDIF
114600151209     C****** Campo "speciali" x gestione codici opzione per VATNOT_P
114700151209     C* OPZ
114800151209     C                   IF        %subst(
114900151209     C                             %subst(vlrppt:posDaDft+1:
115000151209     C                             posADft-posDaDft-1):1:3)
115100151209     C                             = 'OPZ'
115200151209     C                   EVAL      wNOT_P_OPZ=%trim(%subst(
115300151209     C                             %subst(vlrppt:posDaDft+1:
115400151209     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
115500151209     C                   ENDIF
115600151209     C******
115700030822     C* NCL
115800030822     C                   IF        %subst(
115900030822     C                             %subst(vlrppt:posDaDft+1:
116000030822     C                             posADft-posDaDft-1):1:3)
116100030822     C                             = 'NCL'
116200030822     C                   EVAL      PiStr=%trim(%subst(
116300030822     C                             %subst(vlrppt:posDaDft+1:
116400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
116500030822     C                   EXSR      CHKNUM
116600030822     C                   IF        PiInt=*on
116700030822     C                   Z-ADD     PiVal         VABNCL
116800030822     C                   ENDIF
116900030822     C                   ENDIF
117000030822     C* CCM
117100030822     C                   IF        %subst(
117200030822     C                             %subst(vlrppt:posDaDft+1:
117300030822     C                             posADft-posDaDft-1):1:3)
117400030822     C                             = 'CCM'
117500030822     C                   EVAL      PiStr=%trim(%subst(
117600030822     C                             %subst(vlrppt:posDaDft+1:
117700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
117800030822     C                   EXSR      CHKNUM
117900030822     C                   IF        PiInt=*on
118000030822     C                   Z-ADD     PiVal         VABCCM
118100030822     C                   ENDIF
118200030822     C                   ENDIF
118300030822     C* LNP
118400030822     C                   IF        %subst(
118500030822     C                             %subst(vlrppt:posDaDft+1:
118600030822     C                             posADft-posDaDft-1):1:3)
118700030822     C                             = 'LNP'
118800030822     C                   EVAL      PiStr=%trim(%subst(
118900030822     C                             %subst(vlrppt:posDaDft+1:
119000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
119100030822     C                   EXSR      CHKNUM
119200030822     C                   IF        PiInt=*on
119300030822     C                   Z-ADD     PiVal         VABLNP
119400030822     C                   ENDIF
119500030822     C                   ENDIF
119600030822     C* NRS
119700030822     C                   IF        %subst(
119800030822     C                             %subst(vlrppt:posDaDft+1:
119900030822     C                             posADft-posDaDft-1):1:3)
120000030822     C                             = 'NRS'
120100030822     C                   EVAL      PiStr=%trim(%subst(
120200030822     C                             %subst(vlrppt:posDaDft+1:
120300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
120400030822     C                   EXSR      CHKNUM
120500030822     C                   IF        PiInt=*on
120600030822     C                   Z-ADD     PiVal         VABNRS
120700030822     C                   ENDIF
120800030822     C                   ENDIF
120900030822     C* CTR
121000030822     C                   IF        %subst(
121100030822     C                             %subst(vlrppt:posDaDft+1:
121200030822     C                             posADft-posDaDft-1):1:3)
121300030822     C                             = 'CTR'
121400030822     C                   EVAL      PiStr=%trim(%subst(
121500030822     C                             %subst(vlrppt:posDaDft+1:
121600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
121700030822     C                   EXSR      CHKNUM
121800030822     C                   IF        PiInt=*on
121900030822     C                   Z-ADD     PiVal         VABCTR
122000030822     C                   ENDIF
122100030822     C                   ENDIF
122200030822     C* PKB
122300030822     C                   IF        %subst(
122400030822     C                             %subst(vlrppt:posDaDft+1:
122500030822     C                             posADft-posDaDft-1):1:3)
122600030822     C                             = 'PKB'
122700030822     C                   EVAL      PiStr=%trim(%subst(
122800030822     C                             %subst(vlrppt:posDaDft+1:
122900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
123000030822     C                   EXSR      CHKNUM
123100030822     C                   IF        PiNum=*on
123200030822     C                   Z-ADD     PiVal         VABPKB
123300030822     C                   ENDIF
123400030822     C                   ENDIF
123500030822     C* VLB
123600030822     C                   IF        %subst(
123700030822     C                             %subst(vlrppt:posDaDft+1:
123800030822     C                             posADft-posDaDft-1):1:3)
123900030822     C                             = 'VLB'
124000030822     C                   EVAL      PiStr=%trim(%subst(
124100030822     C                             %subst(vlrppt:posDaDft+1:
124200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
124300030822     C                   EXSR      CHKNUM
124400030822     C                   IF        PiNum=*on
124500030822     C                   Z-ADD     PiVal         VABVLB
124600030822     C                   ENDIF
124700030822     C                   ENDIF
124800030822     C* QFT
124900030822     C                   IF        %subst(
125000030822     C                             %subst(vlrppt:posDaDft+1:
125100030822     C                             posADft-posDaDft-1):1:3)
125200030822     C                             = 'QFT'
125300030822     C                   EVAL      PiStr=%trim(%subst(
125400030822     C                             %subst(vlrppt:posDaDft+1:
125500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
125600030822     C                   EXSR      CHKNUM
125700030822     C                   IF        PiNum=*on
125800030822     C                   Z-ADD     PiVal         VABQFT
125900030822     C                   ENDIF
126000030822     C                   ENDIF
126100030822     C* CBO
126200030822     C                   IF        %subst(
126300030822     C                             %subst(vlrppt:posDaDft+1:
126400030822     C                             posADft-posDaDft-1):1:3)
126500030822     C                             = 'CBO'
126600030822     C                   EVAL      VABCBO=%trim(%subst(
126700030822     C                             %subst(vlrppt:posDaDft+1:
126800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
126900030822     C                   ENDIF
127000030822     C* TSP
127100030822     C                   IF        %subst(
127200030822     C                             %subst(vlrppt:posDaDft+1:
127300030822     C                             posADft-posDaDft-1):1:3)
127400030822     C                             = 'TSP'
127500030822     C                   EVAL      VABTSP=%trim(%subst(
127600030822     C                             %subst(vlrppt:posDaDft+1:
127700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
127800030822     C                   ENDIF
127900030822     C* VAS
128000030822     C                   IF        %subst(
128100030822     C                             %subst(vlrppt:posDaDft+1:
128200030822     C                             posADft-posDaDft-1):1:3)
128300030822     C                             = 'VAS'
128400030822     C                   EVAL      VABVAS=%trim(%subst(
128500030822     C                             %subst(vlrppt:posDaDft+1:
128600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
128700030822     C                   ENDIF
128800030822     C* VCA
128900030822     C                   IF        %subst(
129000030822     C                             %subst(vlrppt:posDaDft+1:
129100030822     C                             posADft-posDaDft-1):1:3)
129200030822     C                             = 'VCA'
129300030822     C                   EVAL      VABVCA=%trim(%subst(
129400030822     C                             %subst(vlrppt:posDaDft+1:
129500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
129600030822     C                   ENDIF
129700030822     C* GCA
129800030822     C                   IF        %subst(
129900030822     C                             %subst(vlrppt:posDaDft+1:
130000030822     C                             posADft-posDaDft-1):1:3)
130100030822     C                             = 'GCA'
130200030822     C                   EVAL      VABGCA=%trim(%subst(
130300030822     C                             %subst(vlrppt:posDaDft+1:
130400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
130500030822     C                   ENDIF
130600030822     C* CTM
130700030822     C                   IF        %subst(
130800030822     C                             %subst(vlrppt:posDaDft+1:
130900030822     C                             posADft-posDaDft-1):1:3)
131000030822     C                             = 'CTM'
131100030822     C                   EVAL      VABCTM=%trim(%subst(
131200030822     C                             %subst(vlrppt:posDaDft+1:
131300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
131400030822     C                   ENDIF
131500030822     C* FFD
131600030822     C                   IF        %subst(
131700030822     C                             %subst(vlrppt:posDaDft+1:
131800030822     C                             posADft-posDaDft-1):1:3)
131900030822     C                             = 'FFD'
132000030822     C                   EVAL      VABFFD=%trim(%subst(
132100030822     C                             %subst(vlrppt:posDaDft+1:
132200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
132300030822     C                   ENDIF
132400030822     C* VAD
132500030822     C                   IF        %subst(
132600030822     C                             %subst(vlrppt:posDaDft+1:
132700030822     C                             posADft-posDaDft-1):1:3)
132800030822     C                             = 'VAD'
132900030822     C                   EVAL      VABVAD=%trim(%subst(
133000030822     C                             %subst(vlrppt:posDaDft+1:
133100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
133200030822     C                   ENDIF
133300030822     C* GMA
133400030822     C                   IF        %subst(
133500030822     C                             %subst(vlrppt:posDaDft+1:
133600030822     C                             posADft-posDaDft-1):1:3)
133700030822     C                             = 'GMA'
133800030822     C                   EVAL      VABGMA=%trim(%subst(
133900030822     C                             %subst(vlrppt:posDaDft+1:
134000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
134100030822     C                   ENDIF
134200030822     C* GGA
134300030822     C                   IF        %subst(
134400030822     C                             %subst(vlrppt:posDaDft+1:
134500030822     C                             posADft-posDaDft-1):1:3)
134600030822     C                             = 'GGA'
134700030822     C                   EVAL      VABGGA=%trim(%subst(
134800030822     C                             %subst(vlrppt:posDaDft+1:
134900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
135000030822     C                   ENDIF
135100030822     C* GVA
135200030822     C                   IF        %subst(
135300030822     C                             %subst(vlrppt:posDaDft+1:
135400030822     C                             posADft-posDaDft-1):1:3)
135500030822     C                             = 'GVA'
135600030822     C                   EVAL      VABGVA=%trim(%subst(
135700030822     C                             %subst(vlrppt:posDaDft+1:
135800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
135900030822     C                   ENDIF
136000030822     C* TC1
136100030822     C                   IF        %subst(
136200030822     C                             %subst(vlrppt:posDaDft+1:
136300030822     C                             posADft-posDaDft-1):1:3)
136400030822     C                             = 'TC1'
136500030822     C                   EVAL      VABTC1=%trim(%subst(
136600030822     C                             %subst(vlrppt:posDaDft+1:
136700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
136800030822     C                   ENDIF
136900030822     C* TC2
137000030822     C                   IF        %subst(
137100030822     C                             %subst(vlrppt:posDaDft+1:
137200030822     C                             posADft-posDaDft-1):1:3)
137300030822     C                             = 'TC2'
137400030822     C                   EVAL      VABTC2=%trim(%subst(
137500030822     C                             %subst(vlrppt:posDaDft+1:
137600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
137700030822     C                   ENDIF
137800080516     C* TIC
137900080516     C                   IF        %subst(
138000080516     C                             %subst(vlrppt:posDaDft+1:
138100080516     C                             posADft-posDaDft-1):1:3)
138200080516     C                             = 'TIC'
138300080516     C                   EVAL      VABTIC=%trim(%subst(
138400080516     C                             %subst(vlrppt:posDaDft+1:
138500080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
138600080516     C                   ENDIF
138700040714     C* VATTRC
138800040714     C                   IF        %subst(
138900040714     C                             %subst(vlrppt:posDaDft+1:
139000040714     C                             posADft-posDaDft-1):1:3)
139100040714     C                             = 'TRC'
139200040714     C                   EVAL      VATTRC=%trim(%subst(
139300040714     C                             %subst(vlrppt:posDaDft+1:
139400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
139500040714     C                   ENDIF
139600030822     C* ...
139700030822     C                   ENDIF
139800030822     C                   ENDDO
139900020204     C*
140000010330     C                   ENDSR
140100010607     C*----------------------------------------------------*
140200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
140300010607     C*----------------------------------------------------*
140400010607     C     IMPVAB        BEGSR
140500010607     C*
140600010607     C                   SETOFF                                       3132
140700030515     C*
140800010607     C                   MOVEL     datcor        VABAAS
140900010607     C                   MOVE      datcor        VABMGS
141000170329     C*
141100030715     C**********
141200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
141300030715     C**********
141400030715     C                   IF        wGiro = *zeros
141500040113     C*
141600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
141700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
141800040510     C                             %trim(intcol(3))<>*blanks
141900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
142000040113     C                                      %trim(intcol(3))
142100040510     C                   ENDIF
142200170403     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
142300031201     C     minu:maiu     XLATE     vindta        vindta
142400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
142500170331     C                   DOW       posDa <= %len(%trimr(vindta))
142600030715     C*
142700030715     C* Gestisco il 1° campo
142800030715     C                   IF        i = 1
142900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
143000030715     C                   EVAL      posDa = 1
143100030715     C* Eseguo lo scan x trovare la fine del primo campo
143200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
143300030715     C* A questo punto "estraggo" il campo corrente
143400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
143500030715     C                                                    (posA-posDa))
143600030715     C* X i campi successivi al 1°
143700030715     C                   ELSE
143800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
143900030715     C                   EVAL      posDa = posA + 1
144000030715     C* Eseguo lo scan x trovare la fine del campo corrente
144100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
144200030715     C* Gestisco l'ultimo campo
144300030715     C                   IF        posA = *zeros
144400170331     C                   EVAL      posA = %len(%trimr(vindta)) + 1
144500030715     C* A questo punto "estraggo" il campo corrente
144600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
144700030715     C                                                    (posA-posDa))
144800030715     C                   LEAVE
144900030715     C                   ELSE
145000030715     C* A questo punto "estraggo" il campo corrente
145100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
145200030715     C                                                    (posA-posDa))
145300030715     C                   ENDIF
145400030715     C                   ENDIF
145500030715     C* Incremento il contatore d campo
145600030715     C                   EVAL      i = i +1
145700030715     C                   ENDDO
145800030715     C*
145900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
146000170403     C* delimitazione testo e verifico presenta campo VABNSP, VABRMN e campo VAT*
146100170403     C                   Z-ADD     *zeros        wChkNSP           1 0
146200170403     C                   Z-ADD     *zeros        wChkVAT           1 0
146300170419     C                   Z-ADD     *zeros        wChkVAD           1 0
146400170403     C                   Z-ADD     *zeros        wRepNSP           3 0
146500170403     C                   Z-ADD     *zeros        wPosRMN           3 0
146600170403     C                   Z-ADD     *zeros        wLastCSV          3 0
146700030715     C                   EVAL      i = 1
146800030715     C                   DOW       i < %elem(SkSplitFLD)
146900170403     C                   IF        SkSplitFLD(i) <> *blanks
147000170403     C                   EVAL      wLastCSV = i
147100170403     C                   ENDIF
147200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
147300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
147400170419     C                   IF        SkSplitFLD(i) = 'VABNSP'
147500170403     C                   EVAL      wChkNSP = 1
147600170327     C                   ENDIF
147700170419     C                   IF        %scan('VAT':SkSplitFLD(i)) > *zeros
147800170403     C                   EVAL      wChkVAT = 1
147900170403     C                   ENDIF
148000170419     C                   IF        %scan('VAD':SkSplitFLD(i)) > *zeros AND
148100170419     C                             SkSplitFLD(i) <> 'VABVAD'
148200170419     C                   EVAL      wChkVAD = 1
148300170419     C                   ENDIF
148400170327     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
148500170327     C                   EVAL      wPosRMN = i
148600170327     C                   ENDIF
148700030715     C                   EVAL      i = i + 1
148800030715     C                   ENDDO
148900170327     C*
149000170403     C* Considerazioni particolari su presenza campi VABNSP, VAT* e VABRMN
149100170403     C                   SELECT
149200170403     C                   WHEN      wChkNSP = *zeros AND
149300170403     C                             wPosRMN > *zeros AND
149400170419     C                             (wChkVAT > *zeros OR
149500170419     C                              wChkVAD > *zeros)
149600170328     C                   EVAL      SkSplitFLD(wPosRMN) = 'W_VABRMN'
149700170403     C                   WHEN      wChkNSP = *zeros AND
149800170419     C                             wChkVAT = *zeros AND
149900170419     C                             wChkVAD = *zeros
150000170403     C                   EVAL      wRepNSP = wLastCSV+1
150100170403     C                   EVAL      SkSplitFLD(wRepNSP) = 'W_VABNSP'
150200170403     C                   ENDSL
150300170503     C*
150400170503     C                   IF        *in54 AND wPosRMN > *zeros
150500170503     C                   EVAL      SkSplitFLD(wPosRMN) = 'W_VABRMN'
150600170503     C                   ENDIF
150700170327     C*
150800030715     C                   EVAL      wGiro = 1
150900170327     C*
151000170403     C* Se NO Primo Giro
151100030715     C                   ELSE
151200030715     C                   EVAL      wGiro = 2
151300030715     C**********
151400030715     C* Effettuo lo split del campo dati d input
151500030715     C**********
151600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
151700170331     C                   DOW       posDa <= %len(%trimr(vindta))
151800030715     C*
151900030715     C* Gestisco il 1° campo
152000030715     C                   IF        i = 1
152100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
152200030715     C                   EVAL      posDa = 1
152300030715     C* Eseguo lo scan x trovare la fine del primo campo
152400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
152500030715     C* A questo punto "estraggo" il campo corrente
152600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
152700030715     C                                                    (posA-posDa))
152800030715     C* X i campi successivi al 1°
152900030715     C                   ELSE
153000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
153100030715     C                   EVAL      posDa = posA + 1
153200031204     C* Verifico che nn vi sia il campo nn valorizzato
153300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
153400031204     C* Se campo nn valorizzato skippo al prossimo
153500031204     C                   EVAL      SkSplitCSV(i) = *blanks
153600031204     C                   EVAL      posA  = posA + 1
153700031204     C                   ELSE
153800030715     C* Eseguo lo scan x trovare la fine del campo corrente
153900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
154000030715     C* Gestisco l'ultimo campo
154100030715     C                   IF        posA = *zeros
154200170331     C                   EVAL      posA = %len(%trimr(vindta)) + 1
154300030715     C* A questo punto "estraggo" il campo corrente
154400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
154500030715     C                                                    (posA-posDa))
154600030715     C                   LEAVE
154700030715     C                   ELSE
154800030715     C* A questo punto "estraggo" il campo corrente
154900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
155000030715     C                                                    (posA-posDa))
155100030715     C                   ENDIF
155200031204     C                   ENDIF
155300030715     C                   ENDIF
155400030715     C* Incremento il contatore d campo
155500030715     C                   EVAL      i = i +1
155600030715     C                   ENDDO
155700030715     C*
155800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
155900030715     C                   EVAL      i = 1
156000030715     C                   DOW       i < %elem(SkSplitFLD)
156100030715     C*
156200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
156300170403     C                   IF        SkSplitFLD(i) <> *blanks AND
156400170403     C                             (SkSplitCSV(i) <> *blanks OR i = wRepNSP)
156500170403     C*
156600170403     C* Normalizzo per eliminare caratteri delimitazione testo ed eventuali apici
156700170403     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
156800170403     C     '''':' '      XLATE     SkSplitCSV(i) SkSplitCSV(i)
156900170403     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
157000170629     C*
157100170629     C*** CMR
157200170629     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMR'
157300170629     C                   EVAL      wVABCMR = SkSplitCSV(i)
157400170629     C                   ENDIF
157500030721     C*** ATB
157600170831     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
157700170831     C                   IF        %trim(SkSplitCSV(i)) <> 'A'
157800170831     C                   EVAL      VABATB = SkSplitCSV(i)
157900170831     C                   ENDIF
158000170831     C                   ENDIF
158100030715     C*** RSD
158200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
158300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
158400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
158500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
158600030715     C* ==
158700030715     C                   ENDIF
158800030721     C*** RD2
158900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
159000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
159100030721     C                   ENDIF
159200030715     C*** IND
159300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
159400030715     C                   EVAL      VABIND = SkSplitCSV(i)
159500030715     C                   ENDIF
159600030715     C*** LOD
159700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
159800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
159900030715     C                   ENDIF
160000030715     C*** PRD
160100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
160200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
160300030715     C                   ENDIF
160400030721     C*** NZD
160500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
160600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
160700030721     C                   ENDIF
160800030715     C*** RMA
160900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
161000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
161100030715     C                   ENDIF
161200030715     C*** NOT
161300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
161400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
161500030715     C                   ENDIF
161600030715     C*** NT2
161700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
161800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
161900030715     C                   ENDIF
162000030715     C*** VCA
162100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
162200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
162300030715     C                   ENDIF
162400030715     C*** VAS
162500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
162600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
162700030715     C                   ENDIF
162800030715     C*** FFD
162900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
163000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
163100030715     C                   ENDIF
163200030721     C*** GC1
163300170327     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
163400170327     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
163500170327     C                   MOVEL     *blanks       wGC               1
163600170327     C                   EVAL      wGC = %trim(SkSplitCSV(i))
163700170327     C                   IF        SkSplitCSV(i) = 'M' OR
163800170327     C                             SkSplitCSV(i) = 'P'
163900170327     C                   MOVE(P)   wGC           VABGC1
164000170327     C                   ELSE
164100170327     C                   MOVEL(P)  wGC           VABGC1
164200170327     C                   ENDIF
164300170327     C                   ELSE
164400170327     C                   EVAL      VABGC1 = SkSplitCSV(i)
164500170327     C                   ENDIF
164600170327     C                   ENDIF
164700030721     C*** GC2
164800170327     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
164900170327     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
165000170327     C                   MOVEL     *blanks       wGC               1
165100170327     C                   EVAL      wGC = %trim(SkSplitCSV(i))
165200170327     C                   IF        SkSplitCSV(i) = 'M' OR
165300170327     C                             SkSplitCSV(i) = 'P'
165400170327     C                   MOVE(P)   wGC           VABGC2
165500170327     C                   ELSE
165600170327     C                   MOVEL(P)  wGC           VABGC2
165700170327     C                   ENDIF
165800170327     C                   ELSE
165900170327     C                   EVAL      VABGC2 = SkSplitCSV(i)
166000170327     C                   ENDIF
166100170327     C                   ENDIF
166200030721     C*** TSP
166300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
166400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
166500030721     C                   ENDIF
166600040510     C*** CBO
166700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
166800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
166900040510     C                   ENDIF
167000030721     C*** NAS
167100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
167200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
167300030721     C                   ENDIF
167400030721     C*** TIC
167500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
167600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
167700030721     C                   ENDIF
167800030721     C*** GCA
167900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
168000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
168100030721     C                   ENDIF
168200030721     C*** XCO
168300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
168400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
168500030721     C                   ENDIF
168600030721     C*** CTM
168700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
168800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
168900030721     C                   ENDIF
169000030721     C*** TCR
169100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
169200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
169300030721     C                   ENDIF
169400030721     C*** CTS
169500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
169600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
169700030721     C                   ENDIF
169800030721     C*** FTM
169900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
170000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
170100030721     C                   ENDIF
170200030721     C*** VAD
170300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
170400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
170500030721     C                   ENDIF
170600030721     C*** GMA
170700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
170800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
170900030721     C                   ENDIF
171000030721     C*** GGA
171100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
171200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
171300030721     C                   ENDIF
171400030721     C*** GVA
171500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
171600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
171700030721     C                   ENDIF
171800030721     C*** TC1
171900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
172000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
172100030721     C                   ENDIF
172200030721     C*** TC2
172300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
172400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
172500030721     C                   ENDIF
172600030721     C*** SCL
172700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
172800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
172900030721     C                   ENDIF
173000030721     C*** RMO
173100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
173200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
173300030721     C                   ENDIF
173400170403     C*** CMO
173500170403     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
173600170403     C                   EVAL      VABCMO = SkSplitCSV(i)
173700170403     C                   ENDIF
173800030721     C*** NMO
173900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
174000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
174100030721     C                   ENDIF
174200030721     C*
174300030721     C* Reperisco quindi i campi numerici...
174400040510     C*** NSP
174500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
174600040510     C                   EVAL      PiStr=SkSplitCSV(i)
174700040510     C                   EXSR      CHKNUM
174800040510     C                   IF        PiInt=*on
174900040510     C                   Z-ADD     PiVal         VABNSP
175000040510     C                   ELSE
175100170403     C                   IF        wChkVAT = *zeros
175200170403     C                   exsr      repNSP
175300170403     C                   ELSE
175400040510     C                   SETON                                        31
175500040510     C                   EVAL      VABNSP = *zeros
175600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
175700040510     C                             + ' ' + 'VABNSP'
175800170403     C                   ENDIF
175900040510     C                   ENDIF
176000040510     C                   ENDIF
176100070703     C*** RMN
176200070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
176300070703     C                   EVAL      PiStr=SkSplitCSV(i)
176400070703     C                   EXSR      CHKNUM
176500070703     C                   IF        PiInt=*on
176600070703     C                   Z-ADD     PiVal         VABRMN
176700070703     C                   ELSE
176800070703     C                   SETON                                        32
176900070703     C                   EVAL      vinmsg = %trimr(vinmsg)
177000070703     C                             + ' ' + 'VABRMN'
177100070703     C                   ENDIF
177200070703     C                   ENDIF
177300170328     C*** RMN
177400170328     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
177500170328     C                   EVAL      PiStr=SkSplitCSV(i)
177600170328     C                   EXSR      CHKNUM
177700170328     C                   IF        PiInt=*on
177800170328     C                   Z-ADD     PiVal         VABRMN
177900170328     C                   Z-ADD     PiVal         VABNSP
178000170328     C                   ELSE
178100170328     C                   SETON                                        32
178200170328     C                   EVAL      vinmsg = %trimr(vinmsg)
178300170328     C                             + ' ' + 'VABRMN VABNSP'
178400170328     C                   ENDIF
178500170328     C                   ENDIF
178600170403     C*** NSP
178700170403     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
178800170403     C                   exsr      repNSP
178900170403     C                   ENDIF
179000070705     C*** CAD
179100070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
179200070705     C                   EVAL      VABCAD = SkSplitCSV(i)
179300070705     C                   ENDIF
179400030721     C*** NCL
179500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
179600030721     C                   EVAL      PiStr=SkSplitCSV(i)
179700030721     C                   EXSR      CHKNUM
179800030721     C                   IF        PiInt=*on
179900170327     C  N40              Z-ADD     PiVal         VABNCL
180000170327     C   40              ADD       PiVal         VABNCL
180100030721     C                   ELSE
180200030721     C                   SETON                                        32
180300030721     C                   EVAL      VABNCL = *zeros
180400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
180500040122     C                             + ' ' + 'VABNCL'
180600030721     C                   ENDIF
180700030721     C                   ENDIF
180800030721     C*** PKB
180900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
181000030721     C                   EVAL      PiStr=SkSplitCSV(i)
181100030721     C                   EXSR      CHKNUM
181200030721     C                   IF        PiNum=*on
181300170327     C  N40              Z-ADD     PiVal         VABPKB
181400170327     C   40              ADD       PiVal         VABPKB
181500030721     C                   ELSE
181600030721     C                   SETON                                        32
181700030721     C                   EVAL      VABPKB = *zeros
181800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
181900030721     C                             + ' ' + 'VABPKB'
182000030721     C                   ENDIF
182100030721     C                   ENDIF
182200030721     C*** CAS
182300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
182400030721     C                   EVAL      PiStr=SkSplitCSV(i)
182500030721     C                   EXSR      CHKNUM
182600030721     C                   IF        PiNum=*on
182700030721     C                   Z-ADD     PiVal         VABCAS
182800030721     C                   ELSE
182900030721     C                   SETON                                        32
183000030721     C                   EVAL      VABCAS = *zeros
183100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
183200030721     C                             + ' ' + 'VABCAS'
183300030721     C                   ENDIF
183400030721     C                   ENDIF
183500030721     C*** CCM
183600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
183700030721     C                   EVAL      PiStr=SkSplitCSV(i)
183800030721     C                   EXSR      CHKNUM
183900030721     C                   IF        PiInt=*on      AND
184000030721     C                             PiVal<=9999999 AND
184100030721     C                             PiVal>*zeros
184200030721     C                   Z-ADD     PiVal         VABCCM
184300030721     C                   ELSE
184400030721     C                   SETON                                        32
184500030721     C                   EVAL      VABCCM = *zeros
184600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
184700040510     C                             + ' ' + 'VABCCM'
184800030721     C                   ENDIF
184900030721     C                   ENDIF
185000030721     C*** LNP
185100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
185200030721     C                   EVAL      PiStr=SkSplitCSV(i)
185300030721     C                   EXSR      CHKNUM
185400030721     C                   IF        PiInt=*on    AND
185500030721     C                             PiVal<=999   AND
185600030721     C                             PiVal>*zeros
185700030721     C                   Z-ADD     PiVal         VABLNP
185800030721     C                   ELSE
185900040715     C                   SETON                                        32
186000030721     C                   EVAL      VABLNP = *zeros
186100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
186200040510     C                             + ' ' + 'VABLNP'
186300030721     C                   ENDIF
186400030721     C                   ENDIF
186500030721     C*** NRS
186600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
186700030721     C                   EVAL      PiStr=SkSplitCSV(i)
186800030721     C                   EXSR      CHKNUM
186900030721     C                   IF        PiInt=*on AND
187000030721     C                             PiVal<=99
187100030721     C                   Z-ADD     PiVal         VABNRS
187200030721     C                   ELSE
187300030721     C                   SETON                                        32
187400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
187500030721     C                             + ' ' + 'VABNRS'
187600030721     C                   ENDIF
187700030721     C                   ENDIF
187800030721     C*** LNA
187900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
188000170327     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
188100170327     C                             %trim(SkSplitCSV(i)) <> '0'     AND
188200170327     C                             %trim(SkSplitCSV(i)) <> '000'   AND
188300170327     C                             %trim(SkSplitCSV(i)) <> *blanks
188400030721     C                   EVAL      PiStr=SkSplitCSV(i)
188500030721     C                   EXSR      CHKNUM
188600030721     C                   IF        PiInt=*on    AND
188700030721     C                             PiVal<=999   AND
188800030721     C                             PiVal>*zeros
188900030721     C                   Z-ADD     PiVal         VABLNA
189000030721     C                   ELSE
189100030721     C                   SETON                                        32
189200030721     C                   EVAL      VABLNA = *zeros
189300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
189400030721     C                             + ' ' + 'VABLNA'
189500170327     C                   ENDIF
189600030721     C                   ENDIF
189700030721     C                   ENDIF
189800030721     C*** CTR
189900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
190000030721     C                   EVAL      PiStr=SkSplitCSV(i)
190100030721     C                   EXSR      CHKNUM
190200030721     C                   IF        PiInt=*on  AND
190300030721     C                             PiVal<=999
190400030721     C                   Z-ADD     PiVal         VABCTR
190500030721     C                   ELSE
190600030721     C                   SETON                                        32
190700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
190800030721     C                             + ' ' + 'VABCTR'
190900030721     C                   ENDIF
191000030721     C                   ENDIF
191100030721     C*** IAS
191200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
191300030721     C                   EVAL      PiStr=SkSplitCSV(i)
191400030721     C                   EXSR      CHKNUM
191500030721     C                   IF        PiNum=*on
191600030721     C                   Z-ADD     PiVal         VABIAS
191700030721     C                   ELSE
191800030721     C                   SETON                                        32
191900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
192000030721     C                             + ' ' + 'VABIAS'
192100030721     C                   ENDIF
192200030721     C                   ENDIF
192300030721     C*** VLB
192400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
192500030721     C                   EVAL      PiStr=SkSplitCSV(i)
192600030721     C                   EXSR      CHKNUM
192700030721     C                   IF        PiNum=*on
192800170327     C  N40              Z-ADD     PiVal         VABVLB
192900170327     C   40              ADD       PiVal         VABVLB
193000030721     C                   ELSE
193100030721     C                   SETON                                        32
193200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
193300030721     C                             + ' ' + 'VABVLB'
193400030721     C                   ENDIF
193500030721     C                   ENDIF
193600030721     C*** QFT
193700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
193800030721     C                   EVAL      PiStr=SkSplitCSV(i)
193900030721     C                   EXSR      CHKNUM
194000030721     C                   IF        PiNum=*on
194100170327     C  N40              Z-ADD     PiVal         VABQFT
194200170327     C   40              ADD       PiVal         VABQFT
194300030721     C                   ELSE
194400030721     C                   SETON                                        32
194500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
194600030721     C                             + ' ' + 'VABQFT'
194700030721     C                   ENDIF
194800030721     C                   ENDIF
194900030721     C*** NCD
195000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
195100030721     C                   EVAL      PiStr=SkSplitCSV(i)
195200030721     C                   EXSR      CHKNUM
195300030721     C                   IF        PiInt=*on      AND
195400030721     C                             PiVal<=9999999
195500030721     C                   Z-ADD     PiVal         VABNCD
195600030721     C                   ELSE
195700030721     C                   SETON                                        32
195800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
195900030721     C                             + ' ' + 'VABNCD'
196000030721     C                   ENDIF
196100030721     C                   ENDIF
196200030721     C*** NCA
196300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
196400030721     C                   EVAL      PiStr=SkSplitCSV(i)
196500030721     C                   EXSR      CHKNUM
196600030721     C                   IF        PiInt=*on      AND
196700030721     C                             PiVal<=9999999
196800030721     C                   Z-ADD     PiVal         VABNCA
196900030721     C                   ELSE
197000030721     C                   SETON                                        32
197100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
197200030721     C                             + ' ' + 'VABNCA'
197300030721     C                   ENDIF
197400030721     C                   ENDIF
197500030721     C*** ZNC
197600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
197700030721     C                   EVAL      PiStr=SkSplitCSV(i)
197800030721     C                   EXSR      CHKNUM
197900030721     C                   IF        PiInt=*on AND
198000030721     C                             PiVal<=99
198100030721     C                   Z-ADD     PiVal         VABZNC
198200030721     C                   ELSE
198300030721     C                   SETON                                        32
198400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
198500030721     C                             + ' ' + 'VABZNC'
198600030721     C                   ENDIF
198700030721     C                   ENDIF
198800030721     C*** DCR
198900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
199000030721     C                   EVAL      PiStr=SkSplitCSV(i)
199100030721     C                   EXSR      CHKNUM
199200030721     C                   IF        PiInt=*on       AND
199300030721     C                             PiVal<=99999999
199400030721     C                   Z-ADD     PiVal         VABDCR
199500030721     C                   ELSE
199600030721     C                   SETON                                        32
199700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
199800030721     C                             + ' ' + 'VABDCR'
199900030721     C                   ENDIF
200000030721     C                   ENDIF
200100030721     C*** HCR
200200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
200300030721     C                   EVAL      PiStr=SkSplitCSV(i)
200400030721     C                   EXSR      CHKNUM
200500030721     C                   IF        PiInt=*on   AND
200600030721     C                             PiVal<=9999
200700030721     C                   Z-ADD     PiVal         VABHCR
200800030721     C                   ELSE
200900030721     C                   SETON                                        32
201000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
201100030721     C                             + ' ' + 'VABHCR'
201200030721     C                   ENDIF
201300030721     C                   ENDIF
201400030721     C*** VMD
201500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
201600030721     C                   EVAL      PiStr=SkSplitCSV(i)
201700030721     C                   EXSR      CHKNUM
201800030721     C                   IF        PiNum=*on
201900030721     C                   Z-ADD     PiVal         VABVMD
202000030721     C                   ELSE
202100030721     C                   SETON                                        32
202200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
202300030721     C                             + ' ' + 'VABVMD'
202400030721     C                   ENDIF
202500030721     C                   ENDIF
202600030721     C*** ANT
202700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
202800030721     C                   EVAL      PiStr=SkSplitCSV(i)
202900030721     C                   EXSR      CHKNUM
203000030721     C                   IF        PiInt=*on        AND
203100030721     C                             PiVal<=999999999
203200030721     C                   Z-ADD     PiVal         VABANT
203300030721     C                   ELSE
203400030721     C                   SETON                                        32
203500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
203600030721     C                             + ' ' + 'VABANT'
203700030721     C                   ENDIF
203800030721     C                   ENDIF
203900040510     C*
204000040510     C* Reperisco i campi del FIVAT
204100040510     C*** VATNOT
204200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
204300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
204400040510     C                   ENDIF
204500040510     C*** VATTRC
204600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
204700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
204800040510     C                   ENDIF
204900060331     C*** VATNOT_A
205000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
205100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
205200060331     C                   ENDIF
205300060331     C*** VATNOT_B
205400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
205500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
205600060331     C                   ENDIF
205700060331     C*** VATNOT_E
205800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
205900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
206000060331     C                   ENDIF
206100110912     C*** VATNOT_I
206200110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
206300110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
206400110912     C                   ENDIF
206500110912     C*** VATNOT_J
206600110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
206700110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
206800110912     C                   ENDIF
206900110912     C*** VATNOT_4
207000110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
207100110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
207200110912     C                   ENDIF
207300131018     C*** VATNOT_S
207400131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
207500131018     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
207600131018     C                   ENDIF
207700140123     C*** VATNOT_P
207800140123     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_P'
207900140123     C                   EVAL      wVATNOT_P = %trim(SkSplitCSV(i))
208000140123     C                   ENDIF
208100141128     C*** VATNOT_Y
208200141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
208300141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
208400141128     C                   ENDIF
208500080922     C*
208600080922     C* Reperisco i campi del FIVAD
208700140130     C*** VADNCL_01
208800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_1'
208900140123     C                   EVAL      PiStr=SkSplitCSV(i)
209000140123     C                   EXSR      CHKNUM
209100140123     C                   IF        PiInt=*on
209200140130     C                   Z-ADD     PiVal         wVADNCL_01
209300140123     C                   ENDIF
209400140123     C                   ENDIF
209500140130     C*** VADNCL_02
209600140123     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_2'
209700140123     C                   EVAL      PiStr=SkSplitCSV(i)
209800140123     C                   EXSR      CHKNUM
209900140123     C                   IF        PiInt=*on
210000140130     C                   Z-ADD     PiVal         wVADNCL_02
210100140123     C                   ENDIF
210200140123     C                   ENDIF
210300140130     C*** VADNCL_03
210400140123     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_3'
210500140123     C                   EVAL      PiStr=SkSplitCSV(i)
210600140123     C                   EXSR      CHKNUM
210700140123     C                   IF        PiInt=*on
210800140130     C                   Z-ADD     PiVal         wVADNCL_03
210900140123     C                   ENDIF
211000140123     C                   ENDIF
211100140130     C*** VADNCL_04
211200140123     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_4'
211300140123     C                   EVAL      PiStr=SkSplitCSV(i)
211400140123     C                   EXSR      CHKNUM
211500140123     C                   IF        PiInt=*on
211600140130     C                   Z-ADD     PiVal         wVADNCL_04
211700140123     C                   ENDIF
211800140123     C                   ENDIF
211900140130     C*** VADNCL_05
212000140123     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_5'
212100140123     C                   EVAL      PiStr=SkSplitCSV(i)
212200140123     C                   EXSR      CHKNUM
212300140123     C                   IF        PiInt=*on
212400140130     C                   Z-ADD     PiVal         wVADNCL_05
212500140123     C                   ENDIF
212600140123     C                   ENDIF
212700140130     C*** VADNCL_06
212800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_6'
212900140130     C                   EVAL      PiStr=SkSplitCSV(i)
213000140130     C                   EXSR      CHKNUM
213100140130     C                   IF        PiInt=*on
213200140130     C                   Z-ADD     PiVal         wVADNCL_06
213300140130     C                   ENDIF
213400140130     C                   ENDIF
213500140130     C*** VADNCL_07
213600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_7'
213700140130     C                   EVAL      PiStr=SkSplitCSV(i)
213800140130     C                   EXSR      CHKNUM
213900140130     C                   IF        PiInt=*on
214000140130     C                   Z-ADD     PiVal         wVADNCL_07
214100140130     C                   ENDIF
214200140130     C                   ENDIF
214300140130     C*** VADNCL_08
214400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_8'
214500140130     C                   EVAL      PiStr=SkSplitCSV(i)
214600140130     C                   EXSR      CHKNUM
214700140130     C                   IF        PiInt=*on
214800140130     C                   Z-ADD     PiVal         wVADNCL_08
214900140130     C                   ENDIF
215000140130     C                   ENDIF
215100140130     C*** VADNCL_09
215200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_9'
215300140130     C                   EVAL      PiStr=SkSplitCSV(i)
215400140130     C                   EXSR      CHKNUM
215500140130     C                   IF        PiInt=*on
215600140130     C                   Z-ADD     PiVal         wVADNCL_09
215700140130     C                   ENDIF
215800140130     C                   ENDIF
215900140130     C*** VADNCL_10
216000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_10'
216100140130     C                   EVAL      PiStr=SkSplitCSV(i)
216200140130     C                   EXSR      CHKNUM
216300140130     C                   IF        PiInt=*on
216400140130     C                   Z-ADD     PiVal         wVADNCL_10
216500140130     C                   ENDIF
216600140130     C                   ENDIF
216700140130     C*** VADNCL_11
216800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_11'
216900140130     C                   EVAL      PiStr=SkSplitCSV(i)
217000140130     C                   EXSR      CHKNUM
217100140130     C                   IF        PiInt=*on
217200140130     C                   Z-ADD     PiVal         wVADNCL_11
217300140130     C                   ENDIF
217400140130     C                   ENDIF
217500140130     C*** VADNCL_12
217600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_12'
217700140130     C                   EVAL      PiStr=SkSplitCSV(i)
217800140130     C                   EXSR      CHKNUM
217900140130     C                   IF        PiInt=*on
218000140130     C                   Z-ADD     PiVal         wVADNCL_12
218100140130     C                   ENDIF
218200140130     C                   ENDIF
218300140130     C*** VADNCL_13
218400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_13'
218500140130     C                   EVAL      PiStr=SkSplitCSV(i)
218600140130     C                   EXSR      CHKNUM
218700140130     C                   IF        PiInt=*on
218800140130     C                   Z-ADD     PiVal         wVADNCL_13
218900140130     C                   ENDIF
219000140130     C                   ENDIF
219100140130     C*** VADNCL_14
219200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_14'
219300140130     C                   EVAL      PiStr=SkSplitCSV(i)
219400140130     C                   EXSR      CHKNUM
219500140130     C                   IF        PiInt=*on
219600140130     C                   Z-ADD     PiVal         wVADNCL_14
219700140130     C                   ENDIF
219800140130     C                   ENDIF
219900140130     C*** VADNCL_15
220000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_15'
220100140130     C                   EVAL      PiStr=SkSplitCSV(i)
220200140130     C                   EXSR      CHKNUM
220300140130     C                   IF        PiInt=*on
220400140130     C                   Z-ADD     PiVal         wVADNCL_15
220500140130     C                   ENDIF
220600140130     C                   ENDIF
220700140130     C*** VADNCL_16
220800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_16'
220900140130     C                   EVAL      PiStr=SkSplitCSV(i)
221000140130     C                   EXSR      CHKNUM
221100140130     C                   IF        PiInt=*on
221200140130     C                   Z-ADD     PiVal         wVADNCL_16
221300140130     C                   ENDIF
221400140130     C                   ENDIF
221500140130     C*** VADNCL_17
221600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_17'
221700140130     C                   EVAL      PiStr=SkSplitCSV(i)
221800140130     C                   EXSR      CHKNUM
221900140130     C                   IF        PiInt=*on
222000140130     C                   Z-ADD     PiVal         wVADNCL_17
222100140130     C                   ENDIF
222200140130     C                   ENDIF
222300140130     C*** VADNCL_18
222400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_18'
222500140130     C                   EVAL      PiStr=SkSplitCSV(i)
222600140130     C                   EXSR      CHKNUM
222700140130     C                   IF        PiInt=*on
222800140130     C                   Z-ADD     PiVal         wVADNCL_18
222900140130     C                   ENDIF
223000140130     C                   ENDIF
223100140130     C*** VADNCL_19
223200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_19'
223300140130     C                   EVAL      PiStr=SkSplitCSV(i)
223400140130     C                   EXSR      CHKNUM
223500140130     C                   IF        PiInt=*on
223600140130     C                   Z-ADD     PiVal         wVADNCL_19
223700140130     C                   ENDIF
223800140130     C                   ENDIF
223900140130     C*** VADNCL_20
224000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_20'
224100140130     C                   EVAL      PiStr=SkSplitCSV(i)
224200140130     C                   EXSR      CHKNUM
224300140130     C                   IF        PiInt=*on
224400140130     C                   Z-ADD     PiVal         wVADNCL_20
224500140130     C                   ENDIF
224600140130     C                   ENDIF
224700140130     C*** VADNCL_21
224800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_21'
224900140130     C                   EVAL      PiStr=SkSplitCSV(i)
225000140130     C                   EXSR      CHKNUM
225100140130     C                   IF        PiInt=*on
225200140130     C                   Z-ADD     PiVal         wVADNCL_21
225300140130     C                   ENDIF
225400140130     C                   ENDIF
225500140130     C*** VADNCL_22
225600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_22'
225700140130     C                   EVAL      PiStr=SkSplitCSV(i)
225800140130     C                   EXSR      CHKNUM
225900140130     C                   IF        PiInt=*on
226000140130     C                   Z-ADD     PiVal         wVADNCL_22
226100140130     C                   ENDIF
226200140130     C                   ENDIF
226300140130     C*** VADNCL_23
226400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_23'
226500140130     C                   EVAL      PiStr=SkSplitCSV(i)
226600140130     C                   EXSR      CHKNUM
226700140130     C                   IF        PiInt=*on
226800140130     C                   Z-ADD     PiVal         wVADNCL_23
226900140130     C                   ENDIF
227000140130     C                   ENDIF
227100140130     C*** VADNCL_24
227200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_24'
227300140130     C                   EVAL      PiStr=SkSplitCSV(i)
227400140130     C                   EXSR      CHKNUM
227500140130     C                   IF        PiInt=*on
227600140130     C                   Z-ADD     PiVal         wVADNCL_24
227700140130     C                   ENDIF
227800140130     C                   ENDIF
227900140130     C*** VADNCL_25
228000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_25'
228100140130     C                   EVAL      PiStr=SkSplitCSV(i)
228200140130     C                   EXSR      CHKNUM
228300140130     C                   IF        PiInt=*on
228400140130     C                   Z-ADD     PiVal         wVADNCL_25
228500140130     C                   ENDIF
228600140130     C                   ENDIF
228700140130     C*** VADNCL_26
228800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_26'
228900140130     C                   EVAL      PiStr=SkSplitCSV(i)
229000140130     C                   EXSR      CHKNUM
229100140130     C                   IF        PiInt=*on
229200140130     C                   Z-ADD     PiVal         wVADNCL_26
229300140130     C                   ENDIF
229400140130     C                   ENDIF
229500140130     C*** VADNCL_27
229600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_27'
229700140130     C                   EVAL      PiStr=SkSplitCSV(i)
229800140130     C                   EXSR      CHKNUM
229900140130     C                   IF        PiInt=*on
230000140130     C                   Z-ADD     PiVal         wVADNCL_27
230100140130     C                   ENDIF
230200140130     C                   ENDIF
230300140130     C*** VADNCL_28
230400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_28'
230500140130     C                   EVAL      PiStr=SkSplitCSV(i)
230600140130     C                   EXSR      CHKNUM
230700140130     C                   IF        PiInt=*on
230800140130     C                   Z-ADD     PiVal         wVADNCL_28
230900140130     C                   ENDIF
231000140130     C                   ENDIF
231100140130     C*** VADNCL_29
231200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_29'
231300140130     C                   EVAL      PiStr=SkSplitCSV(i)
231400140130     C                   EXSR      CHKNUM
231500140130     C                   IF        PiInt=*on
231600140130     C                   Z-ADD     PiVal         wVADNCL_29
231700140130     C                   ENDIF
231800140130     C                   ENDIF
231900140130     C*** VADNCL_30
232000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_30'
232100140130     C                   EVAL      PiStr=SkSplitCSV(i)
232200140130     C                   EXSR      CHKNUM
232300140130     C                   IF        PiInt=*on
232400140130     C                   Z-ADD     PiVal         wVADNCL_30
232500140130     C                   ENDIF
232600140130     C                   ENDIF
232700140130
232800140130
232900140130     C*** VADCDP_01
233000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_1'
233100140130     C                   EVAL      wVADCDP_01 = SkSplitCSV(i)
233200080922     C                   ENDIF
233300140130     C*** VADCDP_02
233400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_2'
233500140130     C                   EVAL      wVADCDP_02 = SkSplitCSV(i)
233600080922     C                   ENDIF
233700140130     C*** VADCDP_03
233800080922     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_3'
233900140130     C                   EVAL      wVADCDP_03 = SkSplitCSV(i)
234000140123     C                   ENDIF
234100140130     C*** VADCDP_04
234200120103     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_4'
234300140130     C                   EVAL      wVADCDP_04 = SkSplitCSV(i)
234400120103     C                   ENDIF
234500140130     C*** VADCDP_05
234600120103     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_5'
234700140130     C                   EVAL      wVADCDP_05 = SkSplitCSV(i)
234800120103     C                   ENDIF
234900140130     C*** VADCDP_06
235000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_6'
235100140130     C                   EVAL      wVADCDP_06 = SkSplitCSV(i)
235200140130     C                   ENDIF
235300140130     C*** VADCDP_07
235400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_7'
235500140130     C                   EVAL      wVADCDP_07 = SkSplitCSV(i)
235600140130     C                   ENDIF
235700140130     C*** VADCDP_08
235800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_8'
235900140130     C                   EVAL      wVADCDP_08 = SkSplitCSV(i)
236000140130     C                   ENDIF
236100140130     C*** VADCDP_09
236200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_9'
236300140130     C                   EVAL      wVADCDP_09 = SkSplitCSV(i)
236400140130     C                   ENDIF
236500140130     C*** VADCDP_10
236600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_10'
236700140130     C                   EVAL      wVADCDP_10 = SkSplitCSV(i)
236800140130     C                   ENDIF
236900140130     C*** VADCDP_11
237000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_11'
237100140130     C                   EVAL      wVADCDP_11 = SkSplitCSV(i)
237200140130     C                   ENDIF
237300140130     C*** VADCDP_12
237400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_12'
237500140130     C                   EVAL      wVADCDP_12 = SkSplitCSV(i)
237600140130     C                   ENDIF
237700140130     C*** VADCDP_13
237800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_13'
237900140130     C                   EVAL      wVADCDP_13 = SkSplitCSV(i)
238000140130     C                   ENDIF
238100140130     C*** VADCDP_14
238200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_14'
238300140130     C                   EVAL      wVADCDP_14 = SkSplitCSV(i)
238400140130     C                   ENDIF
238500140130     C*** VADCDP_15
238600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_15'
238700140130     C                   EVAL      wVADCDP_15 = SkSplitCSV(i)
238800140130     C                   ENDIF
238900140130     C*** VADCDP_16
239000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_16'
239100140130     C                   EVAL      wVADCDP_16 = SkSplitCSV(i)
239200140130     C                   ENDIF
239300140130     C*** VADCDP_17
239400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_17'
239500140130     C                   EVAL      wVADCDP_17 = SkSplitCSV(i)
239600140130     C                   ENDIF
239700140130     C*** VADCDP_18
239800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_18'
239900140130     C                   EVAL      wVADCDP_18 = SkSplitCSV(i)
240000140130     C                   ENDIF
240100140130     C*** VADCDP_19
240200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_19'
240300140130     C                   EVAL      wVADCDP_19 = SkSplitCSV(i)
240400140130     C                   ENDIF
240500140130     C*** VADCDP_20
240600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_20'
240700140130     C                   EVAL      wVADCDP_20 = SkSplitCSV(i)
240800140130     C                   ENDIF
240900140130     C*** VADCDP_21
241000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_21'
241100140130     C                   EVAL      wVADCDP_21 = SkSplitCSV(i)
241200140130     C                   ENDIF
241300140130     C*** VADCDP_22
241400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_22'
241500140130     C                   EVAL      wVADCDP_22 = SkSplitCSV(i)
241600140130     C                   ENDIF
241700140130     C*** VADCDP_23
241800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_23'
241900140130     C                   EVAL      wVADCDP_23 = SkSplitCSV(i)
242000140130     C                   ENDIF
242100140130     C*** VADCDP_24
242200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_24'
242300140130     C                   EVAL      wVADCDP_24 = SkSplitCSV(i)
242400140130     C                   ENDIF
242500140130     C*** VADCDP_25
242600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_25'
242700140130     C                   EVAL      wVADCDP_25 = SkSplitCSV(i)
242800140130     C                   ENDIF
242900140130     C*** VADCDP_26
243000140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_26'
243100140130     C                   EVAL      wVADCDP_26 = SkSplitCSV(i)
243200140130     C                   ENDIF
243300140130     C*** VADCDP_27
243400140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_27'
243500140130     C                   EVAL      wVADCDP_27 = SkSplitCSV(i)
243600140130     C                   ENDIF
243700140130     C*** VADCDP_28
243800140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_28'
243900140130     C                   EVAL      wVADCDP_28 = SkSplitCSV(i)
244000140130     C                   ENDIF
244100140130     C*** VADCDP_29
244200140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_29'
244300140130     C                   EVAL      wVADCDP_29 = SkSplitCSV(i)
244400140130     C                   ENDIF
244500140130     C*** VADCDP_30
244600140130     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_30'
244700140130     C                   EVAL      wVADCDP_30 = SkSplitCSV(i)
244800140130     C                   ENDIF
244900040510     C*
245000030715     C                   ENDIF
245100030715     C                   EVAL      i = i + 1
245200030715     C                   ENDDO
245300040510     C*
245400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
245500170419     C                   IF        VABNSP = *zeros AND
245600170419     C                             (wChkVAT > *zeros OR
245700170419     C                              wChkVAD > *zeros)
245800040510     C                   SETON                                        31
245900040510     C                   ENDIF
246000170406     C*
246100170406     C* Se LNP nn valorizzato e P.O. invio diverso da 999 forzo il P.O. invio
246200170406     C                   IF        VABLNP = *zeros AND
246300170406     C                             vlrPOI <> 999
246400170406     C                   EVAL      VABLNP = vlrPOI
246500170406     C                   ENDIF
246600040715     C*
246700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
246800040715     C                   IF        VABLNP = *zeros
246900040715     C                   SETON                                        31
247000040715     C                   ENDIF
247100040510     C*
247200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
247300040510     C                   IF        VABRMN = *zeros
247400040510     C                   Z-ADD     VABNSP        VABRMN
247500040510     C                   ENDIF
247600040510     C*
247700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
247800040510     C                   IF        VABRMA = *blanks
247900040714     C                   MOVEL     VABRMN        VABRMA
248000040510     C                   ENDIF
248100080922     C*
248200080922     C* Se CBO nn valorizzato lo forzo a 1 (porto franco)
248300080922     C                   IF        VABCBO = *blanks
248400080922     C                   EVAL      VABCBO = '1'
248500080922     C                   ENDIF
248600070705     C*
248700070705     C* Considerazioni particolari
248800070705     C*** NZD
248900070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
249000070705     C                   EVAL      VABNZD = *blanks
249100070705     C                   ENDIF
249200070705     C*** CAD
249300070705     C                   IF        VABNZD = *blanks
249400070705     C                   EVAL      PiStr=VABCAD
249500070705     C                   EXSR      CHKNUM
249600070705     C                   IF        PiInt=*on
249700070705     C                   Z-ADD     PiVal         Num5_0
249800070705     C                   MOVEL(p)  Num5_0        VABCAD
249900070705     C                   ELSE
250000080922     C                   SETON                                        32
250100070705     C                   EVAL      VABCAD = *zeros
250200070705     C                   EVAL      vinmsg = %trimr(vinmsg)
250300080922     C                             + ' ' + 'VABCAD'
250400070705     C                   ENDIF
250500070705     C                   ENDIF
250600040128     C*
250700040510     C* Se provincia nn valorizzata la reperisco
250800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
250900040510     C                   IF        VABPRD = *blanks
251000040128     C                   CLEAR                   TISI95DS
251100040128     C                   EVAL      I95TCN = '3'
251200040128     C                   Z-ADD     datcor        I95DAT
251300040128     C                   EVAL      I95NAR = VABNZD
251400040128     C                   EVAL      I95CAP = VABCAD
251500040128     C                   EVAL      I95LOC = VABLOD
251600040128     C                   CALL      'TISI95R'
251700040128     C                   PARM                    TISI95DS
251800040128     C                   EVAL      VABPRD = O95PRV
251900040510     C                   ENDIF
252000040113     C*
252100040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
252200040113     C                   IF        VABCAS > *zeros
252300040119     C                   EVAL      VABVCA = 'EUR'
252400040119     C                   IF        VABCBO = '1'
252500040113     C                   EVAL      VABCBO = '4'
252600040119     C                   ENDIF
252700040119     C                   IF        VABCBO = '2'
252800040119     C                   EVAL      VABCBO = '6'
252900040119     C                   ENDIF
253000040113     C                   ENDIF
253100020204     C*
253200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
253300020204     C                   EXSR      CHKIMPDIV
253400030715     C                   ENDIF
253500020204     C*
253600010607     C                   ENDSR
253700010607     C*----------------------------------------------------*
253800010601
253900020204
254000020204     C*----------------------------------------------------*
254100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
254200020204     C*----------------------------------------------------*
254300020204     C     CHKIMPDIV     BEGSR
254400020204     C*
254500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
254600020204     C                   Z-ADD     *zeros        wrkDec            9 9
254700020204     C*
254800020204     C* Come prima cosa effettuo considerazioni sulla divisa
254900020204     C                   IF        vabIAS > *zeros
255000020204     C                   IF        vabVAS <> 'EUR'
255100020204     C                   EVAL      vabVAS =  'ITL'
255200020204     C                   ENDIF
255300020204     C                   ENDIF
255400020204     C*
255500020204     C                   IF        vabCAS > *zeros
255600020204     C                   IF        vabVCA <> 'EUR'
255700020204     C                   EVAL      vabVCA =  'ITL'
255800020204     C                   ENDIF
255900020204     C                   ENDIF
256000020204     C*
256100020204     C                   IF        vabVMD > *zeros
256200020204     C                   IF        vabVAD <> 'EUR'
256300020204     C                   EVAL      vabVAD =  'ITL'
256400020204     C                   ENDIF
256500020204     C                   ENDIF
256600020204     C*
256700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
256800020204     C                   Z-ADD     vabIAS        wrkDec
256900020204     C                   IF        wrkDec > *zeros
257000020204     C                   IF        vabVAS = 'ITL'
257100020204     C                   EVAL      vabIAS = *zeros
257200020204     C                   ENDIF
257300020204     C                   ENDIF
257400020204     C*
257500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
257600020204     C                   Z-ADD     vabCAS        wrkDec
257700020204     C                   IF        wrkDec > *zeros
257800020204     C                   IF        vabVCA = 'ITL'
257900020204     C                   EVAL      vabCAS = *zeros
258000020204     C                   ENDIF
258100020204     C                   ENDIF
258200020204     C*
258300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
258400020204     C                   Z-ADD     vabVMD        wrkDec
258500020204     C                   IF        wrkDec > *zeros
258600020204     C                   IF        vabVAD = 'ITL'
258700020204     C                   EVAL      vabVMD = *zeros
258800020204     C                   ENDIF
258900020204     C                   ENDIF
259000020204     C*
259100020204     C                   ENDSR
259200020204     C***
259300020204
259400010330
259500010330
259600010330     C*----------------------------------------------------*
259700010330     C*  CONTROLLO NUMERICITA' CAMPI
259800010330     C*----------------------------------------------------*
259900010330     C     CHKNUM        BEGSR
260000010330     C*
260100010606     C                   IF        PiDecChr = *blanks
260200030715     C                   EVAL      PiDecChr = CharNUM
260300010606     C                   ENDIF
260400010606     C*
260500010606     C                   CALL(e)   'ISNUMERIC'
260600010330     C                   PARM                    PiStr            30
260700010606     C                   PARM                    PiDecChr          1
260800010330     C                   PARM      *ZEROS        PiVal            30 9
260900010330     C                   PARM      '0'           PiInt             1
261000010330     C                   PARM      '0'           PiNum             1
261100010330     C                   IF        %error
261200010606     C                   EVAL      PiNum=*off
261300010330     C                   ENDIF
261400010330     C*
261500010330     C                   ENDSR
261600010330     C***
261700010330
261800010601
261900010601
262000010601
262100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
262200010601     C     repfil        BEGSR
262300010601     C*
262400010601     C                   if        invfil = *zeros and
262500010601     C                             depfil > *zeros and
262600010629     C                             (vinflg = *blanks or
262700010629     C                              vinflg = *zeros)
262800010601     C
262900010601     C                   eval      invfil = depfil
263000010601     C                   endif
263100010601     C*
263200010601     C                   if        depfil <> invfil and
263300010601     C                             invfil > *zeros
263400010601     C                   eval      flgMulti = '1'
263500010601     C                   if        vinflg = *blanks
263600010601     C                   add       1             cntNonEl
263700010601     C                   endif
263800010601     C                   endif
263900010601     C*
264000010601     C                   if        vinflg = '2'
264100010601     C                   eval      flgStato = '2'
264200010601     C                   endif
264300010601     C*
264400010601     C                   ENDSR
264500010601     C***
264600010601
264700010601
264800010601
264900010330
265000040119      /TITLE Invio dei dati al punto operativo.
265100040119     C     invio         BEGSR
265200080922     C*
265300080922     C* 1° invio FIVAD
265400080922     C                   reset                   dscmz
265500080922     C                   move      winvcmz       cmzdst
265600170519     C  N77              eval      cmzfld = 'FIVADWWR'
265700170519     C   77              eval      cmzfld = 'EDIVADWR'
265800080922     C                   eval      cmzmbd = vlrhdl
265900080922     C                   eval      %subst(cmzmbd:1:1) = 'M'
266000080922     C***                if        prmfir = *blanks
266100170519     C  N77              eval      cmzfla = 'FIVAD00F'
266200170519     C   77              eval      cmzfla = 'EDIVAD0F'
266300170519     C  N77              eval      cmzmba = 'FIVAD00F'
266400170519     C   77              eval      cmzmba = 'EDIVAD0F'
266500080922     C***                else
266600080922     C***                eval      cmzfla = prmfir
266700080922     C***                eval      cmzmba = prmfir
266800080922     C***                endif
266900080922     C                   eval      cmznrr = *zeros
267000080922     C                   move      §ctrokvd      cmznrr
267100080922     C                   eval      cmzlba = vlrfl1
267200080922     C                   call(e)   'TIS711C'
267300080922     C                   parm                    dscmz
267400080922     C                   parm      *blanks       esito
267500080922     C                   if        %error
267600080922     C                             or cmzerr = '1'
267700080922     C                             or esito  = '1'
267800080922     C                   eval      wrkesito = '3'
267900080922     C                   else
268000040119     C*
268100080922     C* 2° invio FIVAT
268200040119     C                   reset                   dscmz
268300070704     C                   move      winvcmz       cmzdst
268400170519     C  N77              eval      cmzfld = 'FIVATWWR'
268500170519     C   77              eval      cmzfld = 'EDIVATWR'
268600170519     C                   eval      cmzmbd = vlrhdl
268700170519     C                   eval      %subst(cmzmbd:1:1) = 'M'
268800170519     C***                if        prmfir = *blanks
268900170519     C  N77              eval      cmzfla = 'FIVAT00F'
269000170519     C   77              eval      cmzfla = 'EDIVAT0F'
269100170519     C  N77              eval      cmzmba = 'FIVAT00F'
269200170519     C   77              eval      cmzmba = 'EDIVAT0F'
269300170519     C***                else
269400040119     C***                eval      cmzfla = prmfir
269500040119     C***                eval      cmzmba = prmfir
269600040119     C***                endif
269700040119     C                   eval      cmznrr = *zeros
269800040119     C                   move      §ctrokvt      cmznrr
269900040119     C                   eval      cmzlba = vlrfl1
270000040119     C                   call(e)   'TIS711C'
270100040119     C                   parm                    dscmz
270200040119     C                   parm      *blanks       esito
270300040119     C                   if        %error
270400040119     C                             or cmzerr = '1'
270500040119     C                             or esito  = '1'
270600040119     C                   eval      wrkesito = '3'
270700040119     C                   else
270800040119     C*
270900080922     C* 3° invio FIVAB
271000040119     C                   reset                   dscmz
271100070704     C                   move      winvcmz       cmzdst
271200040119     C                   eval      cmzfld = vlrfou
271300040119     C                   eval      cmzmbd = vlrhdl
271400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
271500040119     C***                if        prmfir = *blanks
271600170519     C  N77              eval      cmzfla = 'FIVAB00F'
271700170519     C   77              eval      cmzfla = 'EDIVAB0F'
271800170519     C  N77              eval      cmzmba = 'FIVAB00F'
271900170519     C   77              eval      cmzmba = 'EDIVAB0F'
272000170519     C***                else
272100040119     C***                eval      cmzfla = prmfir
272200040119     C***                eval      cmzmba = prmfir
272300040119     C***                endif
272400040119     C                   eval      cmznrr = *zeros
272500040119     C                   move      §ctrokvb      cmznrr
272600040119     C                   eval      cmzlba = vlrfl1
272700040119     C                   call(e)   'TIS711C'
272800040119     C                   parm                    dscmz
272900040119     C                   parm      *blanks       esito
273000040119     C                   if        %error
273100040119     C                             or cmzerr = '1'
273200040119     C                             or esito  = '1'
273300040119     C                   eval      wrkesito = '3'
273400040119     C                   endif
273500040119     C                   endif
273600080922     C                   endif
273700040119     C*
273800040119     C                   ENDSR
273900040119     C***
274000010601
274100010601
274200010601
274300010601
274400010601      /TITLE Invio dei dati al punto operativo.
274500010601     C     opeini        BEGSR
274600010601     C*
274700010601     C* Inizializzo flag e contatori operativi
274800010601     C                   movel     '0'           flgGiro           1
274900010601     C                   movel     '0'           flgMulti          1
275000010601     C                   movel     '1'           flgStato          1
275100010615     C                   movel     '0'           flgOk             1
275200010601     C                   z-add     *zeros        cntNonEl         10 0
275300010601     C                   z-add     *zeros        depfil            3 0
275400010601     C                   z-add     *zeros        invfil            3 0
275500010601     C*
275600010601     C                   ENDSR
275700010601     C***
275800070326
275900070326
276000070326
276100070326
276200070326     C     *pssr         BEGSR
276300080930     C*
276400080930     C                   if        %open(tivin00r)
276500080930     C                   close     tivin00r
276600080930     C                   endif
276700170519     C  N77              if        %open(fivabwwr)
276800080930     C                   close     fivabwwr
276900080930     C                   endif
277000170519     C  N77              if        %open(fivatwwr)
277100080930     C                   close     fivatwwr
277200080930     C                   endif
277300170519     C  N77              if        %open(fivadwwr)
277400080930     C                   close     fivadwwr
277500080930     C                   endif
277600170519     C   77              if        %open(edivabwr)
277700170519     C                   close     edivabwr
277800170519     C                   endif
277900170519     C   77              if        %open(edivatwr)
278000170519     C                   close     edivatwr
278100170519     C                   endif
278200170519     C   77              if        %open(edivadwr)
278300170519     C                   close     edivadwr
278400170519     C                   endif
278500070326     C*
278600070326     C* Effettuo la chiamata al CLLE preposto
278700170519     C  N77              call(e)   'TITVVDTC'
278800170519     C                   parm                    parccm
278900170519     C                   parm                    parmbr
279000170519     C                   parm      '2'           paropz
279100170519     C   77              call(e)   'TITVEVDTC'
279200170519     C                   parm                    parccm
279300170519     C                   parm                    parmbr
279400170519     C                   parm      '2'           paropz
279500070326     C*
279600070326     C                   eval      wrkesito = '2'
279700070404     C*
279800070404     C                   seton                                        LR
279900070326     C*
280000070326     C                   ENDSR     '*CANCL'
280100070326     C***
280200170327
280300170327
280400170327
280500170327     C     normCSV       BEGSR
280600170403     C*
280700170403     C                   z-add     *zeros        wLenVINDTA        4 0
280800170403     C                   z-add     *zeros        wFlgVINDTA        1 0
280900170403     C                   setoff                                       80
281000170403     C*
281100170403     C                   dow       wLenVINDTA < %len(vindta)
281200170403     C                   eval      wLenVINDTA = wLenVINDTA + 1
281300170403     C                   movel     *blanks       wAlfa1            1
281400170403     C                   eval      wAlfa1 = %subst(vindta:wLenVINDTA:1)
281500170403     C*
281600170403     C* Se byte inferiore a HEX40 è un byte "non stampabile" per cui considero *blank
281700170403     C                   if        wAlfa1 <  X'40' AND
281800170403     C                             wAlfa1 <> CharCSV
281900170403     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
282000170403     C                   endif
282100170403     C*
282200170403     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
282300170403     C                   if        wAlfa1 <> CharCSV and
282400170403     C                             wAlfa1 <> CharTXT and
282500170403     C                             wAlfa1 <> CharNUM
282600170403     C                   z-add     1             wFlgVINDTA
282700170403     C                   endif
282800170403     C*
282900170403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
283000170403     C* separatori campi
283100170403     C                   if        CharTXT <> *blanks
283200170403     C                   if        wAlfa1 = CharTXT
283300170403     C                   if        not *in80
283400170403     C                   seton                                        80
283500170403     C                   else
283600170403     C                   setoff                                       80
283700170403     C                   endif
283800170403     C                   endif
283900170403     C                   if        wAlfa1 = CharCSV and *in80 = *on
284000170403     C                   eval      %subst(vindta:wLenVINDTA:1) = CharSOS
284100170403     C                   endif
284200170403     C                   endif
284300170403     C*
284400170403     C                   enddo
284500170403     C*
284600170403     C* Normalizzo i dati di input in modo tale che NON inizino e NON finiscano MAI
284700170403     C* con il carattere di separatore campo
284800170404     C                   IF        %len(%trim(vindta)) > 1
284900170403     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
285000170403     C                   EVAL      vindta = ' ' + vindta
285100170403     C                   ENDIF
285200170403     C*
285300170403     C                   Z-ADD     *zeros        lunghInput        4 0
285400170403     C                   EVAL      vindta = %trimr(vindta)
285500170403     C                   EVAL      lunghInput = %len(%trimr(vindta))
285600170403     C                   IF        %subst(vindta:lunghInput:1) = CharCSV
285700170403     C                   EVAL      %subst(vindta:lunghInput:1) = ' '
285800170403     C                   ENDIF
285900170404     C                   ELSE
286000170404     C                   z-add     *zeros        wFlgVINDTA
286100170404     C                   ENDIF
286200170403     C*
286300170327     C                   ENDSR
286400170327     C***
286500170519
286600170519
286700170519
286800170519     C     wriEVAB       BEGSR
286900170519     C*
287000170519     C                   eval      EDIVABDS = FIVABDS
287100170629     C                   if        wVABCMR = *blanks
287200170519     C                   eval      e_VABCMR = wCMR
287300170629     C                   else
287400170629     C                   eval      e_VABCMR = wVABCMR
287500170629     C                   endif
287600170519     C                   eval      e_VABDCM = datcor
287700170519     C                   eval      e_VABDTS = datcor
287800170519     C                   eval      e_VABHMS = oracor
287900170519     C                   eval      e_VABCNT = 1
288000170519     C                   write     edivab00                                     => scarico il VAB
288100170519     C*
288200170519     C                   ENDSR
288300170519
288400170519
288500170519
288600170519     C     wriEVAT       BEGSR
288700170519     C*
288800170519     C                   eval      EDIVATDS = FIVATDS
288900170629     C                   if        wVABCMR = *blanks
289000170629     C                   eval      e_VATCMR = wCMR
289100170629     C                   else
289200170629     C                   eval      e_VATCMR = wVABCMR
289300170629     C                   endif
289400170519     C                   eval      e_VATCNT = 1
289500170519     C                   write     edivat00
289600170519     C*
289700170519     C                   ENDSR
289800170519
289900170519
290000170519
290100170519     C     wriEVAD       BEGSR
290200170519     C*
290300170519     C                   eval      EDIVADDS = FIVADDS
290400170629     C                   if        wVABCMR = *blanks
290500170629     C                   eval      e_VADCMR = wCMR
290600170629     C                   else
290700170629     C                   eval      e_VADCMR = wVABCMR
290800170629     C                   endif
290900170519     C                   eval      e_VADCNT = 1
291000170519     C                   write     edivad00
291100170519     C*
291200170519     C                   ENDSR
291300170403
291400170403
291500170403
291600170403     C*----------------------------------------------------*
291700170403     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
291800170403     C*----------------------------------------------------*
291900170403     C     REPNSP        BEGSR
292000170403     C*
292100170403     C                   clear                   TRUL33DS
292200170403     C                   eval      I33OPE = *zeros
292300170403     C                   eval      I33CNU = 302
292400170403     C                   eval      I33NUM = 1
292500170403     C                   movel     TRUL33DS      KPJBU
292600170403     C                   call      'TRUL33R'
292700170403     C                   parm                    KPJBA
292800170403     C                   movel     KPJBU         TRUL33DS
292900170403     C                   if        O33ERR = *zeros
293000170403     C                   z-add     O33NRF        VABNSP
293100170403     C                   z-add     O33NRF        VATNSP
293200170403     C                   else
293300170403     C                   SETON                                        31
293400170403     C                   EVAL      vinmsg = %trimr(vinmsg)
293500170403     C                             + ' ' + 'VABNSP VATNSP'
293600170403     C                   endif
293700170403     C*
293800170403     C                   ENDSR
293900070326
294000010330
294100010330
294200000613     C     *inzsr        BEGSR
294300990910     C*
294400990910     C     *entry        plist
294500990920     C                   parm                    tivlrds
294600990921     C                   parm      wrkesito      esito
294700000724     C                   parm                    prmlit
294800000710     C                   parm                    prmfir
294900010330     C*
295000010330     C* CALCOLA LA DATA CORRENTE
295100170519     C                   time                    wn14             14 0
295200170519     C                   movel     wn14          oracor            6 0
295300170519     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
295400170327     C                   eval      datcor = %dec(%date() : *iso)
295500170519     C*
295600170629     C* Verifico subito se trattasi di traduzione su "FIVAB" o su "EDIVAB"
295700170519     C                   if        vlrFOU = 'FIVABWWR'
295800170519     C                   setoff                                       77
295900170629     C                   movel     *blanks       wCMR             35
296000170519     C                   else
296100170519     C                   seton                                        77
296200170519     C                   eval      wCMR = 'ID: ' + vlrPRG
296300170519     C                   endif
296400170519     C*
296500000613     C                   ENDSR
296600000613     C***
296700990908
296800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
296900040510
297000040122
297100040113
