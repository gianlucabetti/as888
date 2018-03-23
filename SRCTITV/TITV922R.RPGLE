000100080922      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR/FIVADWWR.
000200130327     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040119     FFIVATwwr  O    E             DISK    usropn
000700080922     FFIVADwwr  O    E             DISK    usropn
000800000313     D*
000900040113     D*------------
001000040113     D* SCHIEDE A PROGRAMMA
001100040113     D*------------
001200040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001300010330     D*----------------------------------------------------
001400010330     D* DICHIARAZIOINE VARIABILI DI WRK
001500010330     D*----------------------------------------------------
001600010330     D dscmz         e ds                  inz
001700010330     D psds           sds
001800010330     D  procname         *PROC
001900010330     D tivlrds       e ds                  extname(tivlr00f)
002000040128     D tisi95ds      e ds
002100010330     D esito           s              1
002200010330     D prmlit          s             10
002300010330     D prmfir          s             10
002400010330     D wrkesito        s                   like(esito)
002500010330     D rrnum           s              6  0 INZ(*zeros)
002600040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
002700040510     D parccm          s              8    INZ(*blanks)
002800040119     D parmbr          s             10    INZ(*blanks)
002900040119     D paropz          s              1    INZ(*blanks)
003000040119     D chkcall         s              1    INZ(*blanks)
003100030715     D*------------------
003200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003300030715     D*------------------
003400030715     D SkSplitFLD      S             10    DIM(100)
003500030715     D SkSplitCSV      S            256    DIM(100)
003600030715     D CharCSV         S              1
003700030715     D CharTXT         S              1
003800030715     D CharNUM         S              1
003900090403     D CharSOS         S              1
004000030715     D posDa           S              3  0 INZ(*zeros)
004100030715     D posA            S              3  0 INZ(*zeros)
004200030715     D i               s              3  0 INZ(1)
004300030715     D wGiro           s              1  0 INZ(*zeros)
004400060331     D wVATNOT_A       s                   LIKE(VATNOT)
004500060331     D wVATNOT_B       s                   LIKE(VATNOT)
004600060331     D wVATNOT_E       s                   LIKE(VATNOT)
004700110912     D wVATNOT_I       s                   LIKE(VATNOT)
004800110912     D wVATNOT_J       s                   LIKE(VATNOT)
004900110912     D wVATNOT_4       s                   LIKE(VATNOT)
005000131018     D wVATNOT_S       s                   LIKE(VATNOT)
005100141128     D wVATNOT_Y       s                   LIKE(VATNOT)
005200060331     D wVABNSP         s                   LIKE(VABNSP) INZ
005300080922     D wVADCDP_1       s                   LIKE(VADCDP)
005400080922     D wVADCDP_2       s                   LIKE(VADCDP)
005500080922     D wVADCDP_3       s                   LIKE(VADCDP)
005600120103     D wVADCDP_4       s                   LIKE(VADCDP)
005700120103     D wVADCDP_5       s                   LIKE(VADCDP)
005800080922     D wVADNCL_1       s                   LIKE(VADNCL)
005900080922     D wVADNCL_2       s                   LIKE(VADNCL)
006000080922     D wVADNCL_3       s                   LIKE(VADNCL)
006100120103     D wVADNCL_4       s                   LIKE(VADNCL)
006200120103     D wVADNCL_5       s                   LIKE(VADNCL)
006300030822     D*------------------
006400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006500030822     D*------------------
006600030822     D posDaDft        S              3  0 INZ(*zeros)
006700030822     D posADft         S              3  0 INZ(*zeros)
006800030822     D j               s              3  0 INZ(1)
006900030822     D wGiroDft        s              1  0 INZ(*zeros)
007000010330     D*------------------
007100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007200010330     D*------------------
007300010330     D WLBDA8          DS                  INZ
007400010330     D  G08DAT                 1      8  0
007500010330     D  G08INV                 9     16  0
007600010330     D  G08ERR                17     17
007700010330     D  G08TGI                18     22  0
007800031201     D*------------------
007900031201     D* Costanti
008000031201     D*------------------
008100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008300010330
008400010330
008500010330
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800990921     C                   reset                   wrkesito
008900010601     C*
009000010601     C                   exsr      opeini
009100010605     C                   exsr      rwvab
009200080930     C*
009300080930     C                   exsr      endela
009400010601     C*
009500010601     C                   seton                                        lr
009600010601
009700010601
009800010601
009900010601     C*--------------------------------------------------------
010000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010100010601     C*--------------------------------------------------------
010200010601     C     PREELA        BEGSR
010300010601     C*
010400010601     C* SE OCCORRE SPEDIRE IN FILIALE
010500010601     C                   if        invfil <> *zeros and
010600010601     C                             flgGiro = '0'
010700010601     C*
010800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010900010601     C                   eval      flgGiro = '1'
011000010601     C*
011100010601     C                   endif
011200010601     C*
011300010601     C                   ENDSR
011400010601     C***
011500010601
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012100010601     C*--------------------------------------------------------
012200010601     C     ENDELA        BEGSR
012300050201     C*
012400050201     C* Effettuo lancio TISI95 solo x chiusura
012500050201     C                   CLEAR                   TISI95DS
012600050201     C                   EVAL      I95TLA = 'C'
012700050201     C                   CALL      'TISI95R'
012800050201     C                   PARM                    TISI95DS
012900080930     C*
013000080930     C                   if        %open(tivin00r)
013100080930     C                   close     tivin00r
013200080930     C                   endif
013300080930     C                   if        %open(fivabwwr)
013400080930     C                   close     fivabwwr
013500080930     C                   endif
013600080930     C                   if        %open(fivatwwr)
013700080930     C                   close     fivatwwr
013800080930     C                   endif
013900080930     C                   if        %open(fivadwwr)
014000080930     C                   close     fivadwwr
014100080930     C                   endif
014200080930     C*
014300080930     C* Effettuo la chiamata al CLLE preposto
014400080930     C                   call(e)   'TITVVDTC'
014500080930     C                   parm                    parccm
014600080930     C                   parm                    parmbr
014700080930     C                   parm      '2'           paropz
014800000616     C*
014900010601     C                   ENDSR
015000010601     C***
015100000613
015200010601
015300010601
015400010330     C*--------------------------------------------------------
015500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015600010330     C*--------------------------------------------------------
015700010605     C     RWVAB         BEGSR
015800010330     C*
015900010330     C                   if        not %open(tivin00r)
016000010330     C                   open      tivin00r
016100010330     C                   endif
016200021025     C                   if        not %open(fivabwwr)
016300021025     C                   open      fivabwwr
016400010330     C                   endif
016500040119     C*
016600080922     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR e FIVADWWR
016700080922     C                   exsr      prevatvad
016800040119     C*
016900040119     C                   if        chkcall = '0'
017000040119     C*
017100040119     C                   if        not %open(fivatwwr)
017200040119     C                   open      fivatwwr
017300040119     C                   endif
017400080922     C                   if        not %open(fivadwwr)
017500080922     C                   open      fivadwwr
017600080922     C                   endif
017700010330     C*
017800040119     C                   clear                   §CTROKVB          7 0
017900040119     C                   clear                   §CTROKVT          7 0
018000080922     C                   clear                   §CTROKVD          7 0
018100040119     C                   clear                   §CTRMO            7 0
018200040119     C                   clear                   §CTRNO            7 0
018300010330     C*
018400030822     C                   do        *HIVAL
018500010330     C*
018600030822     C                   read      tivin00r                               70
018700010618     C*
018800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018900010618     C                   if        vinflg = '1'
019000010618     C                   eval      flgOk = '1'
019100010618     C                   endif
019200040510     C*
019300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019400040510     C                   z-add     *zeros        wLenVINDTA        4 0
019500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019900040510     C                   z-add     1             wFlgVINDTA
020000040510     C                   leave
020100040510     C                   endif
020200040510     C                   enddo
020300010618     C*
020400040510     C                   if        vindta > *blanks AND
020500040510     C                             wFlgVINDTA = 1
020600010330     C                   add       1             rrnum
020700010330     C*
020800010601     C                   if        *in70 = *off and
020900010330     C                             (vinflg = *blanks
021000010330     C                              or vinflg = '0'
021100010330     C                              or vinflg = '2')
021200010330     C*
021300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021400010711     C                   if        vinflg = *blanks or vinflg = '0'
021500010711     C                   clear                   vinmsg
021600010711     C                   endif
021700010601     C*
021800010330     C                   exsr      inzvar
021900090403     C*
022000090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
022100090403     C* separatori campi
022200090403     C                   if        CharTXT <> *blanks
022300090403     C                   z-add     *zeros        wLenVINDTA        4 0
022400090403     C                   z-add     *zeros        wFlgVINDTA        1 0
022500090403     C                   setoff                                       80
022600090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
022700090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
022800090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
022900090403     C                   if        not *in80
023000090403     C                   seton                                        80
023100090403     C                   else
023200090403     C                   setoff                                       80
023300090403     C                   endif
023400090403     C                   endif
023500090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
023600090403     C                             and *in80 = *on
023700090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
023800090403     C                   endif
023900090403     C                   enddo
024000090403     C                   endif
024100090403     C*
024200010605     C                   exsr      impvab
024300010601     C*
024400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024500010605     C                   eval      depfil = VABLNP
024600010601     C                   exsr      repfil
024700010601     C                   if        depfil = invfil
024800021025     C                   if        vlrpoi = 999
024900030822     C                   move(P)   invfil        VABFGS
025000070704     C                   move(P)   invfil        winvcmz           3
025100021025     C                   else
025200030822     C                   move(P)   vlrpoi        VABFGS
025300070704     C                   move(P)   vlrpoi        winvcmz
025400021025     C                   endif
025500010601     C*
025600010601     C                   exsr      PREELA
025700010601     C*
025800010604     C* Ebbene...
025900010604     C*
026000040119     C  N31              add       1             §CTROKVB
026100040119     C   32              add       1             §CTRMO
026200040119     C   31              add       1             §CTRNO
026300030822     C                   if        wGiro = 2
026400040510     C* Scrivo sempre il VAT
026500040510     C  N31              exsr      wrivat
026600080922     C* Scrivo sempre il VAD
026700080922     C  N31              exsr      wrivad
026800040510     C* Scrivo il VAB solo a rottura d spedizione
026900040119     C                   if        depspe =  *zeros OR
027000040119     C                             depspe <> vabNSP
027100040119     C  N31              exsr      wrivab
027200040119     C  N31              eval      depspe = vabNSP
027300040119     C                   endif
027400040119     C                   endif
027500010604     C*
027600010604     C                   if        *in31 = *off and
027700010604     C                             *in32 = *off
027800010604     C                   eval      vinflg = '1'
027900010604     C                   else
028000010604     C                   eval      vinflg = '2'
028100010604     C                   endif
028200010604     C                   endif
028300010601     C*
028400010604     C                   endif
028500010604     C*
028600010330     C                   else
028700010330     C                   eval      vinflg = '1'
028800010330     C                   endif
028900010601     C*
029000010601     C  N70              update    tivin000
029100010330     C*
029200030822     C  N70              enddo
029300040119     C*
029400040119     C                   endif                                                  (endif - chkcall)
029500010601     C*
029600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029700010601     C                   if        cntNonEl = *zeros or
029800010601     C                             flgMulti = '0'
029900010330     C* Se non ci sono record con errori ...
030000010601     C                   if        §ctrno = 0 and
030100010604     C                             §ctrmo = 0 and
030200010601     C                             flgStato <> '2'
030300010330     C* ... restituisco esito OK.
030400010330     C                   eval      wrkesito = '0'
030500010330     C                   else
030600040119     C                   if        §ctrokvb > 0
030700010330     C                   eval      wrkesito = '1'
030800010330     C                   else
030900010615     C                   if        flgOk = '0'
031000010615     C                   eval      wrkesito = '2'
031100010615     C                   else
031200010615     C                   eval      wrkesito = '6'
031300010615     C                   endif
031400010330     C                   endif
031500010330     C                   endif
031600010601     C                   else
031700010601     C                   eval      wrkesito = '9'
031800010601     C                   endif
031900010601     C*
032000010601     C                   if        vlrpoi <> 999
032100010601     C                   eval      invfil = vlrpoi
032200010601     C                   endif
032300010330     C*
032400040119     C                   if        §ctrokvb > 0
032500010601     C                             and invfil > *zeros
032600010330     C                   exsr      invio
032700010330     C                   endif
032800010601     C*
032900130328     C***                if        flgGiro = '1'
033000130328     C***                exsr      endela
033100130328     C***                endif
033200010330     C*
033300010330     C                   ENDSR
033400010330     C***
033500040119
033600040119
033700040119
033800040119     C*----------------------------------------------------*
033900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034000040119     C*----------------------------------------------------*
034100040510     C     WRIVAB        BEGSR
034200040119     C*
034300060331     C* Gestisco l'eventuale rottura x numero spedizione
034400060331     C                   if        VABNSP <> wVABNSP
034500070301     C                   if        wVATNOT_E <> *blanks
034600070301     C                   eval      VABCTM = '7Q'
034700070301     C                   endif
034800040119     C                   write     fivab000                                     => scarico il VAB
034900060331     C                   eval      wVABNSP = VABNSP
035000060331     C                   endif
035100040119     C*
035200040119     C                   ENDSR
035300040119
035400040119
035500040119
035600040119     C*----------------------------------------------------*
035700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
035800040119     C*----------------------------------------------------*
035900040510     C     WRIVAT        BEGSR
036000060512     C*
036100060512     C* Inizializzo i campi chiave
036200060512     C                   eval      VATFGS = VABFGS
036300080922     C                   eval      VATCCM = VABCCM
036400060512     C                   eval      VATAAS = VABAAS
036500080922     C                   eval      VATLNP = VABLNP
036600060512     C                   eval      VATNSP = VABNSP
036700080922     C                   eval      VATNRS = VABNRS
036800040714     C*
036900040714     C* Solo se VATNOT valorizzato scarico il buffer
037000040714     C                   if        VATNOT <> *blanks
037100040119     C*
037200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
037300040510     C                   if        VATTRC = *blanks
037400040510     C                   eval      VATTRC = 'E'
037500040510     C                   endif
037600040510     C                   write     FIVAT000
037700040119     C                   add       1             §CTROKVT
037800040714     C*
037900040714     C                   endif
038000060331     C*
038100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
038200060331     C                   if        wVATNOT_A <> *blanks
038300060331     C                   eval      VATNOT = wVATNOT_A
038400060331     C                   eval      VATTRC = 'A'
038500060331     C                   write     FIVAT000
038600060331     C                   add       1             §CTROKVT
038700060331     C                   endif
038800060331     C*
038900060331     C                   if        wVATNOT_B <> *blanks
039000060331     C                   eval      VATNOT = wVATNOT_B
039100060331     C                   eval      VATTRC = 'B'
039200060331     C                   write     FIVAT000
039300060331     C                   add       1             §CTROKVT
039400060331     C                   endif
039500060331     C*
039600060331     C                   if        wVATNOT_E <> *blanks
039700060331     C                   eval      VATNOT = wVATNOT_E
039800060331     C                   eval      VATTRC = 'E'
039900060331     C                   write     FIVAT000
040000060331     C                   add       1             §CTROKVT
040100060331     C                   endif
040200110912     C*
040300110912     C                   if        wVATNOT_I <> *blanks
040400110912     C                   eval      VATNOT = wVATNOT_I
040500110912     C                   eval      VATTRC = 'I'
040600110912     C                   write     FIVAT000
040700110912     C                   add       1             §CTROKVT
040800110912     C                   endif
040900110912     C*
041000110912     C                   if        wVATNOT_J <> *blanks
041100110912     C                   eval      VATNOT = wVATNOT_J
041200110912     C                   eval      VATTRC = 'J'
041300110912     C                   write     FIVAT000
041400110912     C                   add       1             §CTROKVT
041500110912     C                   endif
041600110912     C*
041700110912     C                   if        wVATNOT_4 <> *blanks
041800110912     C                   eval      VATNOT = wVATNOT_4
041900110912     C                   eval      VATTRC = '4'
042000110912     C                   write     FIVAT000
042100110912     C                   add       1             §CTROKVT
042200110912     C                   endif
042300131018     C*
042400131018     C                   if        wVATNOT_S <> *blanks
042500131018     C                   eval      VATNOT = wVATNOT_S
042600131018     C                   eval      VATTRC = 'S'
042700131018     C                   write     FIVAT000
042800131018     C                   add       1             §CTROKVT
042900131018     C                   endif
043000141128     C*
043100141128     C                   if        wVATNOT_Y <> *blanks
043200141128     C                   eval      VATNOT = wVATNOT_Y
043300141128     C                   eval      VATTRC = 'Y'
043400141128     C                   write     FIVAT000
043500141128     C                   add       1             §CTROKVT
043600141128     C                   endif
043700040119     C*
043800040119     C                   ENDSR
043900080922
044000080922
044100080922
044200080922     C*----------------------------------------------------*
044300080922     C*  SCARICAMENTO BUFFER RECORDS VAD
044400080922     C*----------------------------------------------------*
044500080922     C     WRIVAD        BEGSR
044600080922     C*
044700080922     C* Inizializzo i campi chiave
044800080922     C                   eval      VADFGS = VABFGS
044900080922     C                   eval      VADCCM = VABCCM
045000080922     C                   eval      VADAAS = VABAAS
045100080922     C                   eval      VADLNP = VABLNP
045200080922     C                   eval      VADNSP = VABNSP
045300080922     C                   eval      VADNRS = VABNRS
045400080922     C*
045500080922     C* Gestisco codice prodotto 1
045600080922     C                   if        wVADNCL_1 > *zeros
045700080922     C                   eval      VADCDP = wVADCDP_1
045800080922     C                   eval      VADNCL = wVADNCL_1
045900080922     C                   write     FIVAD000
046000080922     C                   add       1             §CTROKVD
046100080922     C                   endif
046200080922     C*
046300080922     C* Gestisco codice prodotto 2
046400080922     C                   if        wVADNCL_2 > *zeros
046500080922     C                   eval      VADCDP = wVADCDP_2
046600080922     C                   eval      VADNCL = wVADNCL_2
046700080922     C                   write     FIVAD000
046800080922     C                   add       1             §CTROKVD
046900080922     C                   endif
047000080922     C*
047100080922     C* Gestisco codice prodotto 3
047200080922     C                   if        wVADNCL_3 > *zeros
047300080922     C                   eval      VADCDP = wVADCDP_3
047400080922     C                   eval      VADNCL = wVADNCL_3
047500080922     C                   write     FIVAD000
047600080922     C                   add       1             §CTROKVD
047700080922     C                   endif
047800120103     C*
047900120103     C* Gestisco codice prodotto 4
048000120103     C                   if        wVADNCL_4 > *zeros
048100120103     C                   eval      VADCDP = wVADCDP_4
048200120103     C                   eval      VADNCL = wVADNCL_4
048300120103     C                   write     FIVAD000
048400120103     C                   add       1             §CTROKVD
048500120103     C                   endif
048600120103     C*
048700120103     C* Gestisco codice prodotto 5
048800120103     C                   if        wVADNCL_5 > *zeros
048900120103     C                   eval      VADCDP = wVADCDP_5
049000120103     C                   eval      VADNCL = wVADNCL_5
049100120103     C                   write     FIVAD000
049200120103     C                   add       1             §CTROKVD
049300120103     C                   endif
049400080922     C*
049500080922     C* Verifica "quadratura" colli
049600120103     C                   if        wVADNCL_1 + wVADNCL_2 + wVADNCL_3 +
049700120103     C                             wVADNCL_4 + wVADNCL_5 <> VABNCL
049800080922     C                   SETON                                        32
049900080922     C                   EVAL      vinmsg = %trimr(vinmsg)
050000080922     C                             + ' ' + 'VABNCL/VADNCL'
050100080922     C                   endif
050200080922     C*
050300080922     C                   ENDSR
050400040119
050500040119
050600040119
050700040119     C*----------------------------------------------------*
050800080922     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR E FIVADWWR
050900040119     C*----------------------------------------------------*
051000080922     C     PREVATVAD     BEGSR
051100040119     C*
051200080922     C* Compongo il nome del membro da dare al FIVATWWR e al FIVADWWR
051300040119     C                   eval      parmbr = vlrhdl
051400040119     C                   movel     'M'           parmbr
051500040510     C                   eval      parccm = vlrksc
051600040119     C                   eval      paropz = '1'
051700040119     C* Effettuo la chiamata al CLLE preposto
051800080922     C                   call(e)   'TITVVDTC'
051900040119     C                   parm                    parccm
052000040119     C                   parm                    parmbr
052100040119     C                   parm                    paropz
052200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052300040119     C                   if        %error
052400040119     C                   movel     '1'           chkcall
052500040119     C                   else
052600040119     C                   movel     '0'           chkcall
052700040119     C                   endif
052800040119     C*
052900040119     C                   ENDSR
053000010601
053100010601
053200010601
053300010330     C*----------------------------------------------------*
053400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053500010330     C*----------------------------------------------------*
053600040119     C     INZVAR        BEGSR
053700010330     C*
053800030822     C* Inizializzo variabili di wrk
053900020204     C                   Z-ADD     *zeros        Num5_0            5 0
054000090403     C                   z-add     *zeros        invfil
054100060331     C                   MOVEL     *blanks       wVATNOT_A
054200060331     C                   MOVEL     *blanks       wVATNOT_B
054300060331     C                   MOVEL     *blanks       wVATNOT_E
054400110912     C                   MOVEL     *blanks       wVATNOT_I
054500110912     C                   MOVEL     *blanks       wVATNOT_J
054600110912     C                   MOVEL     *blanks       wVATNOT_4
054700131018     C                   MOVEL     *blanks       wVATNOT_S
054800141128     C                   MOVEL     *blanks       wVATNOT_Y
054900080922     C                   MOVEL     *blanks       wVADCDP_1
055000080922     C                   MOVEL     *blanks       wVADCDP_1
055100080922     C                   MOVEL     *blanks       wVADCDP_3
055200120103     C                   MOVEL     *blanks       wVADCDP_4
055300120103     C                   MOVEL     *blanks       wVADCDP_5
055400080922     C                   Z-ADD     *zeros        wVADNCL_1
055500080922     C                   Z-ADD     *zeros        wVADNCL_2
055600080922     C                   Z-ADD     *zeros        wVADNCL_3
055700120103     C                   Z-ADD     *zeros        wVADNCL_4
055800120103     C                   Z-ADD     *zeros        wVADNCL_5
055900030822     C*
056000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056100030822     C                   CLEAR                   FIVAB000
056200040119     C                   CLEAR                   FIVAT000
056300080922     C                   CLEAR                   FIVAD000
056400030822     C                   CLEAR                   SkSplitCSV
056500030822     C                   EVAL      i = 1
056600030822     C                   EVAL      posDa = *zeros
056700030822     C                   EVAL      posA  = *zeros
056800030822     C*
056900030822     C* Reimposto i valori di default
057000030822     C                   EXSR      DEFCAM
057100030822     C*
057200010330     C                   ENDSR
057300010330     C*----------------------------------------------------*
057400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057500010330     C*----------------------------------------------------*
057600010330     C     DEFCAM        BEGSR
057700030822     C*
057800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057900030715     C* e delimitatore testo.
058000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058300090403     C*
058400090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058500090403     C                   EVAL      CharSOS = CharNUM
058600030822     C*
058700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
058900030715     C                   SETON                                        50
059000030715     C                   ELSE
059100030715     C                   SETOFF                                       50
059200030715     C                   ENDIF
059300030822     C*
059400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059500030822     C                   EVAL      posDaDft = 1
059600030822     C                   EVAL      posADft  = 0
059700030822     C                   EVAL      wGiroDft = 0
059800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059900030822     C                             posDaDft > 0
060000030822     C*
060100030822     C* Gestisco il 1° giro
060200030822     C                   IF        wGiroDft = 0
060300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060500030822     C* Incremento il contatore dei "giri"
060600030822     C                   EVAL      wGiroDft = 1
060700030822     C                   ELSE
060800030822     C                   EVAL      posDaDft = posADft
060900030822     C                   ENDIF
061000030822     C* Eseguo lo scan x trovare la fine del campo corrente
061100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061200030822     C*
061300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061500030822     C                   IF        posDaDft > 0 AND
061600030822     C                             posADft  > 0
061700080922     C****** Campi "speciali" x gestione codici prodotto (Sol. A2)
061800080922     C* CP1
061900080922     C                   IF        %subst(
062000080922     C                             %subst(vlrppt:posDaDft+1:
062100080922     C                             posADft-posDaDft-1):1:3)
062200080922     C                             = 'CP1'
062300080922     C                   EVAL      wVADCDP_1=%trim(%subst(
062400080922     C                             %subst(vlrppt:posDaDft+1:
062500080922     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600080922     C                   ENDIF
062700080922     C* CP2
062800080922     C                   IF        %subst(
062900080922     C                             %subst(vlrppt:posDaDft+1:
063000080922     C                             posADft-posDaDft-1):1:3)
063100080922     C                             = 'CP2'
063200080922     C                   EVAL      wVADCDP_2=%trim(%subst(
063300080922     C                             %subst(vlrppt:posDaDft+1:
063400080922     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500080922     C                   ENDIF
063600080922     C* CP3
063700080922     C                   IF        %subst(
063800080922     C                             %subst(vlrppt:posDaDft+1:
063900080922     C                             posADft-posDaDft-1):1:3)
064000080922     C                             = 'CP3'
064100080922     C                   EVAL      wVADCDP_3=%trim(%subst(
064200080922     C                             %subst(vlrppt:posDaDft+1:
064300080922     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400080922     C                   ENDIF
064500120103     C* CP4
064600120103     C                   IF        %subst(
064700120103     C                             %subst(vlrppt:posDaDft+1:
064800120103     C                             posADft-posDaDft-1):1:3)
064900120103     C                             = 'CP4'
065000120103     C                   EVAL      wVADCDP_4=%trim(%subst(
065100120103     C                             %subst(vlrppt:posDaDft+1:
065200120103     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300120103     C                   ENDIF
065400120103     C* CP5
065500120103     C                   IF        %subst(
065600120103     C                             %subst(vlrppt:posDaDft+1:
065700120103     C                             posADft-posDaDft-1):1:3)
065800120103     C                             = 'CP5'
065900120103     C                   EVAL      wVADCDP_5=%trim(%subst(
066000120103     C                             %subst(vlrppt:posDaDft+1:
066100120103     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200120103     C                   ENDIF
066300080922     C******
066400080922     C
066500030822     C* NCL
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'NCL'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiInt=*on
067500030822     C                   Z-ADD     PiVal         VABNCL
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* CCM
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'CCM'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiInt=*on
068800030822     C                   Z-ADD     PiVal         VABCCM
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100030822     C* LNP
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'LNP'
069600030822     C                   EVAL      PiStr=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   EXSR      CHKNUM
070000030822     C                   IF        PiInt=*on
070100030822     C                   Z-ADD     PiVal         VABLNP
070200030822     C                   ENDIF
070300030822     C                   ENDIF
070400030822     C* NRS
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'NRS'
070900030822     C                   EVAL      PiStr=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   EXSR      CHKNUM
071300030822     C                   IF        PiInt=*on
071400030822     C                   Z-ADD     PiVal         VABNRS
071500030822     C                   ENDIF
071600030822     C                   ENDIF
071700030822     C* CTR
071800030822     C                   IF        %subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):1:3)
072100030822     C                             = 'CTR'
072200030822     C                   EVAL      PiStr=%trim(%subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500030822     C                   EXSR      CHKNUM
072600030822     C                   IF        PiInt=*on
072700030822     C                   Z-ADD     PiVal         VABCTR
072800030822     C                   ENDIF
072900030822     C                   ENDIF
073000030822     C* PKB
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'PKB'
073500030822     C                   EVAL      PiStr=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   EXSR      CHKNUM
073900030822     C                   IF        PiNum=*on
074000030822     C                   Z-ADD     PiVal         VABPKB
074100030822     C                   ENDIF
074200030822     C                   ENDIF
074300030822     C* VLB
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'VLB'
074800030822     C                   EVAL      PiStr=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   EXSR      CHKNUM
075200030822     C                   IF        PiNum=*on
075300030822     C                   Z-ADD     PiVal         VABVLB
075400030822     C                   ENDIF
075500030822     C                   ENDIF
075600030822     C* QFT
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'QFT'
076100030822     C                   EVAL      PiStr=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   EXSR      CHKNUM
076500030822     C                   IF        PiNum=*on
076600030822     C                   Z-ADD     PiVal         VABQFT
076700030822     C                   ENDIF
076800030822     C                   ENDIF
076900030822     C* CBO
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'CBO'
077400030822     C                   EVAL      VABCBO=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* TSP
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'TSP'
078300030822     C                   EVAL      VABTSP=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700030822     C* VAS
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'VAS'
079200030822     C                   EVAL      VABVAS=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* VCA
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'VCA'
080100030822     C                   EVAL      VABVCA=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* GCA
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'GCA'
081000030822     C                   EVAL      VABGCA=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* CTM
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'CTM'
081900030822     C                   EVAL      VABCTM=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300030822     C* FFD
082400030822     C                   IF        %subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):1:3)
082700030822     C                             = 'FFD'
082800030822     C                   EVAL      VABFFD=%trim(%subst(
082900030822     C                             %subst(vlrppt:posDaDft+1:
083000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100030822     C                   ENDIF
083200030822     C* VAD
083300030822     C                   IF        %subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):1:3)
083600030822     C                             = 'VAD'
083700030822     C                   EVAL      VABVAD=%trim(%subst(
083800030822     C                             %subst(vlrppt:posDaDft+1:
083900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084000030822     C                   ENDIF
084100030822     C* GMA
084200030822     C                   IF        %subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):1:3)
084500030822     C                             = 'GMA'
084600030822     C                   EVAL      VABGMA=%trim(%subst(
084700030822     C                             %subst(vlrppt:posDaDft+1:
084800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084900030822     C                   ENDIF
085000030822     C* GGA
085100030822     C                   IF        %subst(
085200030822     C                             %subst(vlrppt:posDaDft+1:
085300030822     C                             posADft-posDaDft-1):1:3)
085400030822     C                             = 'GGA'
085500030822     C                   EVAL      VABGGA=%trim(%subst(
085600030822     C                             %subst(vlrppt:posDaDft+1:
085700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085800030822     C                   ENDIF
085900030822     C* GVA
086000030822     C                   IF        %subst(
086100030822     C                             %subst(vlrppt:posDaDft+1:
086200030822     C                             posADft-posDaDft-1):1:3)
086300030822     C                             = 'GVA'
086400030822     C                   EVAL      VABGVA=%trim(%subst(
086500030822     C                             %subst(vlrppt:posDaDft+1:
086600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086700030822     C                   ENDIF
086800030822     C* TC1
086900030822     C                   IF        %subst(
087000030822     C                             %subst(vlrppt:posDaDft+1:
087100030822     C                             posADft-posDaDft-1):1:3)
087200030822     C                             = 'TC1'
087300030822     C                   EVAL      VABTC1=%trim(%subst(
087400030822     C                             %subst(vlrppt:posDaDft+1:
087500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087600030822     C                   ENDIF
087700030822     C* TC2
087800030822     C                   IF        %subst(
087900030822     C                             %subst(vlrppt:posDaDft+1:
088000030822     C                             posADft-posDaDft-1):1:3)
088100030822     C                             = 'TC2'
088200030822     C                   EVAL      VABTC2=%trim(%subst(
088300030822     C                             %subst(vlrppt:posDaDft+1:
088400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088500030822     C                   ENDIF
088600080516     C* TIC
088700080516     C                   IF        %subst(
088800080516     C                             %subst(vlrppt:posDaDft+1:
088900080516     C                             posADft-posDaDft-1):1:3)
089000080516     C                             = 'TIC'
089100080516     C                   EVAL      VABTIC=%trim(%subst(
089200080516     C                             %subst(vlrppt:posDaDft+1:
089300080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089400080516     C                   ENDIF
089500040714     C* VATTRC
089600040714     C                   IF        %subst(
089700040714     C                             %subst(vlrppt:posDaDft+1:
089800040714     C                             posADft-posDaDft-1):1:3)
089900040714     C                             = 'TRC'
090000040714     C                   EVAL      VATTRC=%trim(%subst(
090100040714     C                             %subst(vlrppt:posDaDft+1:
090200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090300040714     C                   ENDIF
090400030822     C* ...
090500030822     C                   ENDIF
090600030822     C                   ENDDO
090700020204     C*
090800010330     C                   ENDSR
090900010607     C*----------------------------------------------------*
091000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
091100010607     C*----------------------------------------------------*
091200010607     C     IMPVAB        BEGSR
091300010607     C*
091400010607     C                   SETOFF                                       3132
091500030515     C*
091600010607     C                   MOVEL     datcor        VABAAS
091700010607     C                   MOVE      datcor        VABMGS
091800010607     C*
091900030715     C**********
092000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
092100030715     C**********
092200030715     C                   IF        wGiro = *zeros
092300040113     C*
092400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
092500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
092600040510     C                             %trim(intcol(3))<>*blanks
092700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
092800040113     C                                      %trim(intcol(3))
092900040510     C                   ENDIF
093000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
093100031201     C     minu:maiu     XLATE     vindta        vindta
093200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093300030715     C                   DOW       posDa <= %len(%trim(vindta))
093400030715     C*
093500030715     C* Gestisco il 1° campo
093600030715     C                   IF        i = 1
093700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093800030715     C                   EVAL      posDa = 1
093900030715     C* Eseguo lo scan x trovare la fine del primo campo
094000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094100030715     C* A questo punto "estraggo" il campo corrente
094200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094300030715     C                                                    (posA-posDa))
094400030715     C* X i campi successivi al 1°
094500030715     C                   ELSE
094600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094700030715     C                   EVAL      posDa = posA + 1
094800030715     C* Eseguo lo scan x trovare la fine del campo corrente
094900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095000030715     C* Gestisco l'ultimo campo
095100030715     C                   IF        posA = *zeros
095200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095300030715     C* A questo punto "estraggo" il campo corrente
095400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
095500030715     C                                                    (posA-posDa))
095600030715     C                   LEAVE
095700030715     C                   ELSE
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   ENDIF
096200030715     C                   ENDIF
096300030715     C* Incremento il contatore d campo
096400030715     C                   EVAL      i = i +1
096500030715     C                   ENDDO
096600030715     C*
096700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096800030715     C* delimitazione testo
096900030715     C                   EVAL      i = 1
097000030715     C                   DOW       i < %elem(SkSplitFLD)
097100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
097200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
097300030715     C                   EVAL      i = i + 1
097400030715     C                   ENDDO
097500030715     C                   EVAL      wGiro = 1
097600030715     C                   ELSE
097700030715     C                   EVAL      wGiro = 2
097800031204     C**********
097900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
098000031204     C* d separatore campo
098100031204     C**********
098200031204     C                   EVAL      vindta = %trim(vindta)
098300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
098400031204     C                   EVAL      vindta = %subst(vindta:2)
098500031204     C                   ENDDO
098600031204     C*
098700031204     C                   Z-ADD     *zeros        lunghInput        4 0
098800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
098900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
099000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
099100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
099200031204     C                   ENDDO
099300030715     C**********
099400030715     C* Effettuo lo split del campo dati d input
099500030715     C**********
099600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
099700030715     C                   DOW       posDa <= %len(%trim(vindta))
099800030715     C*
099900030715     C* Gestisco il 1° campo
100000030715     C                   IF        i = 1
100100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
100200030715     C                   EVAL      posDa = 1
100300030715     C* Eseguo lo scan x trovare la fine del primo campo
100400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
100500030715     C* A questo punto "estraggo" il campo corrente
100600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100700030715     C                                                    (posA-posDa))
100800030715     C* X i campi successivi al 1°
100900030715     C                   ELSE
101000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
101100030715     C                   EVAL      posDa = posA + 1
101200031204     C* Verifico che nn vi sia il campo nn valorizzato
101300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
101400031204     C* Se campo nn valorizzato skippo al prossimo
101500031204     C                   EVAL      SkSplitCSV(i) = *blanks
101600031204     C                   EVAL      posA  = posA + 1
101700031204     C                   ELSE
101800030715     C* Eseguo lo scan x trovare la fine del campo corrente
101900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
102000030715     C* Gestisco l'ultimo campo
102100030715     C                   IF        posA = *zeros
102200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
102300030715     C* A questo punto "estraggo" il campo corrente
102400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
102500030715     C                                                    (posA-posDa))
102600030715     C                   LEAVE
102700030715     C                   ELSE
102800030715     C* A questo punto "estraggo" il campo corrente
102900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103000030715     C                                                    (posA-posDa))
103100030715     C                   ENDIF
103200031204     C                   ENDIF
103300030715     C                   ENDIF
103400030715     C* Incremento il contatore d campo
103500030715     C                   EVAL      i = i +1
103600030715     C                   ENDDO
103700030715     C*
103800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
103900030715     C* delimitazione testo
104000030715     C                   EVAL      i = 1
104100030715     C                   DOW       i < %elem(SkSplitCSV)
104200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
104300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
104400030715     C                   EVAL      i = i + 1
104500030715     C                   ENDDO
104600030715     C*
104700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
104800030715     C                   EVAL      i = 1
104900030715     C                   DOW       i < %elem(SkSplitFLD)
105000030715     C*
105100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
105200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
105300030715     C                             SkSplitCSV(i) <> *blanks
105400030721     C*** ATB
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
105600030721     C                   EVAL      VABATB = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030715     C*** RSD
105900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
106000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
106100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
106200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
106300030715     C* ==
106400030715     C                   ENDIF
106500030721     C*** RD2
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
106700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030715     C*** IND
107000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
107100030715     C                   EVAL      VABIND = SkSplitCSV(i)
107200030715     C                   ENDIF
107300030715     C*** LOD
107400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
107500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
107600030715     C                   ENDIF
107700030715     C*** PRD
107800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
107900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
108000030715     C                   ENDIF
108100030721     C*** NZD
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
108300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030715     C*** RMA
108600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
108700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
108800030715     C                   ENDIF
108900030715     C*** NOT
109000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
109100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
109200030715     C                   ENDIF
109300030715     C*** NT2
109400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
109500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
109600030715     C                   ENDIF
109700030715     C*** VCA
109800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
109900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
110000030715     C                   ENDIF
110100030715     C*** VAS
110200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
110300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
110400030715     C                   ENDIF
110500030715     C*** FFD
110600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
110700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
110800030715     C                   ENDIF
110900030721     C*** GC1
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
111100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
111200100304     C                   IF        %subst(VABGC1:1) = 'P' or
111300100304     C                             %subst(VABGC1:1) = 'M'
111400100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
111500100304     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** GC2
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
111900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
112000100304     C                   IF        %subst(VABGC2:1) = 'P' or
112100100304     C                             %subst(VABGC2:1) = 'M'
112200100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
112300100304     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** TSP
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
112700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
112800030721     C                   ENDIF
112900040510     C*** CBO
113000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
113100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
113200040510     C                   ENDIF
113300030721     C*** NAS
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
113500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** TIC
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
113900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** GCA
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
114300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** XCO
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
114700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*** CTM
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
115100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
115200030721     C                   ENDIF
115300030721     C*** TCR
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
115500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
115600030721     C                   ENDIF
115700030721     C*** CTS
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
115900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
116000030721     C                   ENDIF
116100030721     C*** FTM
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
116300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
116400030721     C                   ENDIF
116500030721     C*** VAD
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
116700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
116800030721     C                   ENDIF
116900030721     C*** GMA
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
117100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
117200030721     C                   ENDIF
117300030721     C*** GGA
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
117500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
117600030721     C                   ENDIF
117700030721     C*** GVA
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
117900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
118000030721     C                   ENDIF
118100030721     C*** TC1
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
118300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
118400030721     C                   ENDIF
118500030721     C*** TC2
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
118700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
118800030721     C                   ENDIF
118900030721     C*** SCL
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
119100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
119200030721     C                   ENDIF
119300030721     C*** RMO
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
119500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
119600030721     C                   ENDIF
119700030721     C*** NMO
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
119900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
120000030721     C                   ENDIF
120100030721     C*
120200030721     C* Reperisco quindi i campi numerici...
120300040510     C*** NSP
120400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
120500040510     C                   EVAL      PiStr=SkSplitCSV(i)
120600040510     C                   EXSR      CHKNUM
120700040510     C                   IF        PiInt=*on
120800040510     C                   Z-ADD     PiVal         VABNSP
120900040510     C                   ELSE
121000040510     C                   SETON                                        31
121100040510     C                   EVAL      VABNSP = *zeros
121200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
121300040510     C                             + ' ' + 'VABNSP'
121400040510     C                   ENDIF
121500040510     C                   ENDIF
121600070703     C*** RMN
121700070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
121800070703     C                   EVAL      PiStr=SkSplitCSV(i)
121900070703     C                   EXSR      CHKNUM
122000070703     C                   IF        PiInt=*on
122100070703     C                   Z-ADD     PiVal         VABRMN
122200070703     C                   ELSE
122300070703     C                   SETON                                        32
122400070703     C                   EVAL      vinmsg = %trimr(vinmsg)
122500070703     C                             + ' ' + 'VABRMN'
122600070703     C                   ENDIF
122700070703     C                   ENDIF
122800070705     C*** CAD
122900070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
123000070705     C                   EVAL      VABCAD = SkSplitCSV(i)
123100070705     C                   ENDIF
123200030721     C*** NCL
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on
123700030721     C                   Z-ADD     PiVal         VABNCL
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      VABNCL = *zeros
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200040122     C                             + ' ' + 'VABNCL'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** PKB
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiNum=*on
125000030721     C                   Z-ADD     PiVal         VABPKB
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      VABPKB = *zeros
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABPKB'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** CAS
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiNum=*on
126300030721     C                   Z-ADD     PiVal         VABCAS
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      VABCAS = *zeros
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABCAS'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** CCM
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on      AND
127600030721     C                             PiVal<=9999999 AND
127700030721     C                             PiVal>*zeros
127800030721     C                   Z-ADD     PiVal         VABCCM
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      VABCCM = *zeros
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300040510     C                             + ' ' + 'VABCCM'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** LNP
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on    AND
129100030721     C                             PiVal<=999   AND
129200030721     C                             PiVal>*zeros
129300030721     C                   Z-ADD     PiVal         VABLNP
129400030721     C                   ELSE
129500040715     C                   SETON                                        32
129600030721     C                   EVAL      VABLNP = *zeros
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800040510     C                             + ' ' + 'VABLNP'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** NRS
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on AND
130600030721     C                             PiVal<=99
130700030721     C                   Z-ADD     PiVal         VABNRS
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABNRS'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** LNA
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on    AND
131900030721     C                             PiVal<=999   AND
132000030721     C                             PiVal>*zeros
132100030721     C                   Z-ADD     PiVal         VABLNA
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      VABLNA = *zeros
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABLNA'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** CTR
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiInt=*on  AND
133400030721     C                             PiVal<=999
133500030721     C                   Z-ADD     PiVal         VABCTR
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABCTR'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** IAS
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiNum=*on
134700030721     C                   Z-ADD     PiVal         VABIAS
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABIAS'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** VLB
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiNum=*on
135900030721     C                   Z-ADD     PiVal         VABVLB
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABVLB'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** QFT
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiNum=*on
137100030721     C                   Z-ADD     PiVal         VABQFT
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABQFT'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** NCD
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on      AND
138300030721     C                             PiVal<=9999999
138400030721     C                   Z-ADD     PiVal         VABNCD
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138800030721     C                             + ' ' + 'VABNCD'
138900030721     C                   ENDIF
139000030721     C                   ENDIF
139100030721     C*** NCA
139200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
139300030721     C                   EVAL      PiStr=SkSplitCSV(i)
139400030721     C                   EXSR      CHKNUM
139500030721     C                   IF        PiInt=*on      AND
139600030721     C                             PiVal<=9999999
139700030721     C                   Z-ADD     PiVal         VABNCA
139800030721     C                   ELSE
139900030721     C                   SETON                                        32
140000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140100030721     C                             + ' ' + 'VABNCA'
140200030721     C                   ENDIF
140300030721     C                   ENDIF
140400030721     C*** ZNC
140500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
140600030721     C                   EVAL      PiStr=SkSplitCSV(i)
140700030721     C                   EXSR      CHKNUM
140800030721     C                   IF        PiInt=*on AND
140900030721     C                             PiVal<=99
141000030721     C                   Z-ADD     PiVal         VABZNC
141100030721     C                   ELSE
141200030721     C                   SETON                                        32
141300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141400030721     C                             + ' ' + 'VABZNC'
141500030721     C                   ENDIF
141600030721     C                   ENDIF
141700030721     C*** DCR
141800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
141900030721     C                   EVAL      PiStr=SkSplitCSV(i)
142000030721     C                   EXSR      CHKNUM
142100030721     C                   IF        PiInt=*on       AND
142200030721     C                             PiVal<=99999999
142300030721     C                   Z-ADD     PiVal         VABDCR
142400030721     C                   ELSE
142500030721     C                   SETON                                        32
142600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142700030721     C                             + ' ' + 'VABDCR'
142800030721     C                   ENDIF
142900030721     C                   ENDIF
143000030721     C*** HCR
143100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
143200030721     C                   EVAL      PiStr=SkSplitCSV(i)
143300030721     C                   EXSR      CHKNUM
143400030721     C                   IF        PiInt=*on   AND
143500030721     C                             PiVal<=9999
143600030721     C                   Z-ADD     PiVal         VABHCR
143700030721     C                   ELSE
143800030721     C                   SETON                                        32
143900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144000030721     C                             + ' ' + 'VABHCR'
144100030721     C                   ENDIF
144200030721     C                   ENDIF
144300030721     C*** VMD
144400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
144500030721     C                   EVAL      PiStr=SkSplitCSV(i)
144600030721     C                   EXSR      CHKNUM
144700030721     C                   IF        PiNum=*on
144800030721     C                   Z-ADD     PiVal         VABVMD
144900030721     C                   ELSE
145000030721     C                   SETON                                        32
145100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145200030721     C                             + ' ' + 'VABVMD'
145300030721     C                   ENDIF
145400030721     C                   ENDIF
145500030721     C*** ANT
145600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
145700030721     C                   EVAL      PiStr=SkSplitCSV(i)
145800030721     C                   EXSR      CHKNUM
145900030721     C                   IF        PiInt=*on        AND
146000030721     C                             PiVal<=999999999
146100030721     C                   Z-ADD     PiVal         VABANT
146200030721     C                   ELSE
146300030721     C                   SETON                                        32
146400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146500030721     C                             + ' ' + 'VABANT'
146600030721     C                   ENDIF
146700030721     C                   ENDIF
146800030721     C*** CMO
146900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
147000030721     C                   EVAL      PiStr=SkSplitCSV(i)
147100030721     C                   EXSR      CHKNUM
147200030721     C                   IF        PiInt=*on
147300030721     C                   Z-ADD     PiVal         Num5_0
147400030721     C                   MOVEL(p)  Num5_0        VABCMO
147500030721     C                   ELSE
147600030721     C                   SETON                                        32
147700030721     C                   EVAL      VABCMO = *zeros
147800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
147900030721     C                             + ' ' + 'VABCMO'
148000030721     C                   ENDIF
148100030721     C                   ENDIF
148200040510     C*
148300040510     C* Reperisco i campi del FIVAT
148400040510     C*** VATNOT
148500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
148600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
148700040510     C                   ENDIF
148800040510     C*** VATTRC
148900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
149000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
149100040510     C                   ENDIF
149200060331     C*** VATNOT_A
149300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
149400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
149500060331     C                   ENDIF
149600060331     C*** VATNOT_B
149700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
149800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
149900060331     C                   ENDIF
150000060331     C*** VATNOT_E
150100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
150200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
150300060331     C                   ENDIF
150400110912     C*** VATNOT_I
150500110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
150600110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
150700110912     C                   ENDIF
150800110912     C*** VATNOT_J
150900110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
151000110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
151100110912     C                   ENDIF
151200110912     C*** VATNOT_4
151300110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
151400110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
151500110912     C                   ENDIF
151600131018     C*** VATNOT_S
151700131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
151800131018     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
151900131018     C                   ENDIF
152000141128     C*** VATNOT_Y
152100141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
152200141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
152300141128     C                   ENDIF
152400080922     C*
152500080922     C* Reperisco i campi del FIVAD
152600080922     C*** VADCDP_1
152700080922     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_1'
152800080922     C                   EVAL      PiStr=SkSplitCSV(i)
152900080922     C                   EXSR      CHKNUM
153000080922     C                   IF        PiInt=*on
153100080922     C                   Z-ADD     PiVal         wVADNCL_1
153200080922     C                   ENDIF
153300080922     C                   ENDIF
153400080922     C*** VADCDP_2
153500080922     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_2'
153600080922     C                   EVAL      PiStr=SkSplitCSV(i)
153700080922     C                   EXSR      CHKNUM
153800080922     C                   IF        PiInt=*on
153900080922     C                   Z-ADD     PiVal         wVADNCL_2
154000080922     C                   ENDIF
154100080922     C                   ENDIF
154200080922     C*** VADCDP_3
154300080922     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_3'
154400080922     C                   EVAL      PiStr=SkSplitCSV(i)
154500080922     C                   EXSR      CHKNUM
154600080922     C                   IF        PiInt=*on
154700080922     C                   Z-ADD     PiVal         wVADNCL_3
154800080922     C                   ENDIF
154900080922     C                   ENDIF
155000120103     C*** VADCDP_4
155100120103     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_4'
155200120103     C                   EVAL      PiStr=SkSplitCSV(i)
155300120103     C                   EXSR      CHKNUM
155400120103     C                   IF        PiInt=*on
155500120103     C                   Z-ADD     PiVal         wVADNCL_4
155600120103     C                   ENDIF
155700120103     C                   ENDIF
155800120103     C*** VADCDP_5
155900120103     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_5'
156000120103     C                   EVAL      PiStr=SkSplitCSV(i)
156100120103     C                   EXSR      CHKNUM
156200120103     C                   IF        PiInt=*on
156300120103     C                   Z-ADD     PiVal         wVADNCL_5
156400120103     C                   ENDIF
156500120103     C                   ENDIF
156600040510     C*
156700030715     C                   ENDIF
156800030715     C                   EVAL      i = i + 1
156900030715     C                   ENDDO
157000040510     C*
157100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
157200040510     C                   IF        VABNSP = *zeros
157300040510     C                   SETON                                        31
157400040510     C                   ENDIF
157500040715     C*
157600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
157700040715     C                   IF        VABLNP = *zeros
157800040715     C                   SETON                                        31
157900040715     C                   ENDIF
158000040510     C*
158100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
158200040510     C                   IF        VABRMN = *zeros
158300040510     C                   Z-ADD     VABNSP        VABRMN
158400040510     C                   ENDIF
158500040510     C*
158600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
158700040510     C                   IF        VABRMA = *blanks
158800040714     C                   MOVEL     VABRMN        VABRMA
158900040510     C                   ENDIF
159000080922     C*
159100080922     C* Se CBO nn valorizzato lo forzo a 1 (porto franco)
159200080922     C                   IF        VABCBO = *blanks
159300080922     C                   EVAL      VABCBO = '1'
159400080922     C                   ENDIF
159500070705     C*
159600070705     C* Considerazioni particolari
159700070705     C*** NZD
159800070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
159900070705     C                   EVAL      VABNZD = *blanks
160000070705     C                   ENDIF
160100070705     C*** CAD
160200070705     C                   IF        VABNZD = *blanks
160300070705     C                   EVAL      PiStr=VABCAD
160400070705     C                   EXSR      CHKNUM
160500070705     C                   IF        PiInt=*on
160600070705     C                   Z-ADD     PiVal         Num5_0
160700070705     C                   MOVEL(p)  Num5_0        VABCAD
160800070705     C                   ELSE
160900080922     C                   SETON                                        32
161000070705     C                   EVAL      VABCAD = *zeros
161100070705     C                   EVAL      vinmsg = %trimr(vinmsg)
161200080922     C                             + ' ' + 'VABCAD'
161300070705     C                   ENDIF
161400070705     C                   ENDIF
161500040128     C*
161600040510     C* Se provincia nn valorizzata la reperisco
161700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
161800040510     C                   IF        VABPRD = *blanks
161900040128     C                   CLEAR                   TISI95DS
162000040128     C                   EVAL      I95TCN = '3'
162100040128     C                   Z-ADD     datcor        I95DAT
162200040128     C                   EVAL      I95NAR = VABNZD
162300040128     C                   EVAL      I95CAP = VABCAD
162400040128     C                   EVAL      I95LOC = VABLOD
162500040128     C                   CALL      'TISI95R'
162600040128     C                   PARM                    TISI95DS
162700040128     C                   EVAL      VABPRD = O95PRV
162800040510     C                   ENDIF
162900040113     C*
163000040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
163100040113     C                   IF        VABCAS > *zeros
163200040119     C                   EVAL      VABVCA = 'EUR'
163300040119     C                   IF        VABCBO = '1'
163400040113     C                   EVAL      VABCBO = '4'
163500040119     C                   ENDIF
163600040119     C                   IF        VABCBO = '2'
163700040119     C                   EVAL      VABCBO = '6'
163800040119     C                   ENDIF
163900040113     C                   ENDIF
164000020204     C*
164100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
164200020204     C                   EXSR      CHKIMPDIV
164300030715     C                   ENDIF
164400020204     C*
164500010607     C                   ENDSR
164600010607     C*----------------------------------------------------*
164700010601
164800020204
164900020204     C*----------------------------------------------------*
165000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
165100020204     C*----------------------------------------------------*
165200020204     C     CHKIMPDIV     BEGSR
165300020204     C*
165400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
165500020204     C                   Z-ADD     *zeros        wrkDec            9 9
165600020204     C*
165700020204     C* Come prima cosa effettuo considerazioni sulla divisa
165800020204     C                   IF        vabIAS > *zeros
165900020204     C                   IF        vabVAS <> 'EUR'
166000020204     C                   EVAL      vabVAS =  'ITL'
166100020204     C                   ENDIF
166200020204     C                   ENDIF
166300020204     C*
166400020204     C                   IF        vabCAS > *zeros
166500020204     C                   IF        vabVCA <> 'EUR'
166600020204     C                   EVAL      vabVCA =  'ITL'
166700020204     C                   ENDIF
166800020204     C                   ENDIF
166900020204     C*
167000020204     C                   IF        vabVMD > *zeros
167100020204     C                   IF        vabVAD <> 'EUR'
167200020204     C                   EVAL      vabVAD =  'ITL'
167300020204     C                   ENDIF
167400020204     C                   ENDIF
167500020204     C*
167600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
167700020204     C                   Z-ADD     vabIAS        wrkDec
167800020204     C                   IF        wrkDec > *zeros
167900020204     C                   IF        vabVAS = 'ITL'
168000020204     C                   EVAL      vabIAS = *zeros
168100020204     C                   ENDIF
168200020204     C                   ENDIF
168300020204     C*
168400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
168500020204     C                   Z-ADD     vabCAS        wrkDec
168600020204     C                   IF        wrkDec > *zeros
168700020204     C                   IF        vabVCA = 'ITL'
168800020204     C                   EVAL      vabCAS = *zeros
168900020204     C                   ENDIF
169000020204     C                   ENDIF
169100020204     C*
169200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
169300020204     C                   Z-ADD     vabVMD        wrkDec
169400020204     C                   IF        wrkDec > *zeros
169500020204     C                   IF        vabVAD = 'ITL'
169600020204     C                   EVAL      vabVMD = *zeros
169700020204     C                   ENDIF
169800020204     C                   ENDIF
169900020204     C*
170000020204     C                   ENDSR
170100020204     C***
170200020204
170300010330
170400010330
170500010330     C*----------------------------------------------------*
170600010330     C*  CONTROLLO NUMERICITA' CAMPI
170700010330     C*----------------------------------------------------*
170800010330     C     CHKNUM        BEGSR
170900010330     C*
171000010606     C                   IF        PiDecChr = *blanks
171100030715     C                   EVAL      PiDecChr = CharNUM
171200010606     C                   ENDIF
171300010606     C*
171400010606     C                   CALL(e)   'ISNUMERIC'
171500010330     C                   PARM                    PiStr            30
171600010606     C                   PARM                    PiDecChr          1
171700010330     C                   PARM      *ZEROS        PiVal            30 9
171800010330     C                   PARM      '0'           PiInt             1
171900010330     C                   PARM      '0'           PiNum             1
172000010330     C                   IF        %error
172100010606     C                   EVAL      PiNum=*off
172200010330     C                   ENDIF
172300010330     C*
172400010330     C                   ENDSR
172500010330     C***
172600010330
172700010601
172800010601
172900010601
173000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
173100010601     C     repfil        BEGSR
173200010601     C*
173300010601     C                   if        invfil = *zeros and
173400010601     C                             depfil > *zeros and
173500010629     C                             (vinflg = *blanks or
173600010629     C                              vinflg = *zeros)
173700010601     C
173800010601     C                   eval      invfil = depfil
173900010601     C                   endif
174000010601     C*
174100010601     C                   if        depfil <> invfil and
174200010601     C                             invfil > *zeros
174300010601     C                   eval      flgMulti = '1'
174400010601     C                   if        vinflg = *blanks
174500010601     C                   add       1             cntNonEl
174600010601     C                   endif
174700010601     C                   endif
174800010601     C*
174900010601     C                   if        vinflg = '2'
175000010601     C                   eval      flgStato = '2'
175100010601     C                   endif
175200010601     C*
175300010601     C                   ENDSR
175400010601     C***
175500010601
175600010601
175700010601
175800010330
175900040119      /TITLE Invio dei dati al punto operativo.
176000040119     C     invio         BEGSR
176100080922     C*
176200080922     C* 1° invio FIVAD
176300080922     C                   reset                   dscmz
176400080922     C                   move      winvcmz       cmzdst
176500080922     C                   eval      cmzfld = 'FIVADWWR'
176600080922     C                   eval      cmzmbd = vlrhdl
176700080922     C                   eval      %subst(cmzmbd:1:1) = 'M'
176800080922     C***                if        prmfir = *blanks
176900080922     C                   eval      cmzfla = 'FIVAD00F'
177000080922     C                   eval      cmzmba = 'FIVAD00F'
177100080922     C***                else
177200080922     C***                eval      cmzfla = prmfir
177300080922     C***                eval      cmzmba = prmfir
177400080922     C***                endif
177500080922     C                   eval      cmznrr = *zeros
177600080922     C                   move      §ctrokvd      cmznrr
177700080922     C                   eval      cmzlba = vlrfl1
177800080922     C                   call(e)   'TIS711C'
177900080922     C                   parm                    dscmz
178000080922     C                   parm      *blanks       esito
178100080922     C                   if        %error
178200080922     C                             or cmzerr = '1'
178300080922     C                             or esito  = '1'
178400080922     C                   eval      wrkesito = '3'
178500080922     C                   else
178600040119     C*
178700080922     C* 2° invio FIVAT
178800040119     C                   reset                   dscmz
178900070704     C                   move      winvcmz       cmzdst
179000040119     C                   eval      cmzfld = 'FIVATWWR'
179100040119     C                   eval      cmzmbd = vlrhdl
179200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
179300040119     C***                if        prmfir = *blanks
179400040119     C                   eval      cmzfla = 'FIVAT00F'
179500040119     C                   eval      cmzmba = 'FIVAT00F'
179600040119     C***                else
179700040119     C***                eval      cmzfla = prmfir
179800040119     C***                eval      cmzmba = prmfir
179900040119     C***                endif
180000040119     C                   eval      cmznrr = *zeros
180100040119     C                   move      §ctrokvt      cmznrr
180200040119     C                   eval      cmzlba = vlrfl1
180300040119     C                   call(e)   'TIS711C'
180400040119     C                   parm                    dscmz
180500040119     C                   parm      *blanks       esito
180600040119     C                   if        %error
180700040119     C                             or cmzerr = '1'
180800040119     C                             or esito  = '1'
180900040119     C                   eval      wrkesito = '3'
181000040119     C                   else
181100040119     C*
181200080922     C* 3° invio FIVAB
181300040119     C                   reset                   dscmz
181400070704     C                   move      winvcmz       cmzdst
181500040119     C                   eval      cmzfld = vlrfou
181600040119     C                   eval      cmzmbd = vlrhdl
181700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
181800040119     C***                if        prmfir = *blanks
181900040119     C                   eval      cmzfla = 'FIVAB00F'
182000040119     C                   eval      cmzmba = 'FIVAB00F'
182100040119     C***                else
182200040119     C***                eval      cmzfla = prmfir
182300040119     C***                eval      cmzmba = prmfir
182400040119     C***                endif
182500040119     C                   eval      cmznrr = *zeros
182600040119     C                   move      §ctrokvb      cmznrr
182700040119     C                   eval      cmzlba = vlrfl1
182800040119     C                   call(e)   'TIS711C'
182900040119     C                   parm                    dscmz
183000040119     C                   parm      *blanks       esito
183100040119     C                   if        %error
183200040119     C                             or cmzerr = '1'
183300040119     C                             or esito  = '1'
183400040119     C                   eval      wrkesito = '3'
183500040119     C                   endif
183600040119     C                   endif
183700080922     C                   endif
183800040119     C*
183900040119     C                   ENDSR
184000040119     C***
184100010601
184200010601
184300010601
184400010601
184500010601      /TITLE Invio dei dati al punto operativo.
184600010601     C     opeini        BEGSR
184700010601     C*
184800010601     C* Inizializzo flag e contatori operativi
184900010601     C                   movel     '0'           flgGiro           1
185000010601     C                   movel     '0'           flgMulti          1
185100010601     C                   movel     '1'           flgStato          1
185200010615     C                   movel     '0'           flgOk             1
185300010601     C                   z-add     *zeros        cntNonEl         10 0
185400010601     C                   z-add     *zeros        depfil            3 0
185500010601     C                   z-add     *zeros        invfil            3 0
185600010601     C*
185700010601     C                   ENDSR
185800010601     C***
185900070326
186000070326
186100070326
186200070326
186300070326     C     *pssr         BEGSR
186400080930     C*
186500080930     C                   if        %open(tivin00r)
186600080930     C                   close     tivin00r
186700080930     C                   endif
186800080930     C                   if        %open(fivabwwr)
186900080930     C                   close     fivabwwr
187000080930     C                   endif
187100080930     C                   if        %open(fivatwwr)
187200080930     C                   close     fivatwwr
187300080930     C                   endif
187400080930     C                   if        %open(fivadwwr)
187500080930     C                   close     fivadwwr
187600080930     C                   endif
187700070326     C*
187800070326     C* Effettuo la chiamata al CLLE preposto
187900080922     C                   call(e)   'TITVVDTC'
188000070326     C                   parm                    parccm
188100070326     C                   parm                    parmbr
188200070326     C                   parm      '2'           paropz
188300070326     C*
188400070326     C                   eval      wrkesito = '2'
188500070404     C*
188600070404     C                   seton                                        LR
188700070326     C*
188800070326     C                   ENDSR     '*CANCL'
188900070326     C***
189000070326
189100070326
189200010330
189300010330
189400000613     C     *inzsr        BEGSR
189500990910     C*
189600990910     C     *entry        plist
189700990920     C                   parm                    tivlrds
189800990921     C                   parm      wrkesito      esito
189900000724     C                   parm                    prmlit
190000000710     C                   parm                    prmfir
190100010330     C*
190200010330     C* CALCOLA LA DATA CORRENTE
190300010330     C                   time                    wn14             14 0
190400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
190500010330     C                   z-add     wn8           g08dat
190600010330     C                   z-add     *zeros        g08inv
190700010330     C                   movel     '0'           g08err
190800010330     C                   call      'XSRDA8'
190900010330     C                   parm                    wlbda8
191000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
191100000613     C*
191200000613     C                   ENDSR
191300000613     C***
191400990908
191500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
191600040510
191700040122
191800040113
