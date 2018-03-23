000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000600040119     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900040128     D tisi95ds      e ds
002000100413     D fivabds       e ds                  extname(fivab00f)
002100100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002200100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D rrnum           s              6  0 INZ(*zeros)
002800040510     D parccm          s              8    INZ(*blanks)
002900040119     D parmbr          s             10    INZ(*blanks)
003000040119     D paropz          s              1    INZ(*blanks)
003100040119     D chkcall         s              1    INZ(*blanks)
003200080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003300080418     D wFlgCAS         s              1    INZ(*blanks)
003400100326     D*------------------
003500100326     D* DS REPERIMENTO NUMERATORE
003600100326     D*------------------
003700100326     D trul33ds      e ds                  inz
003800100326     D*------------------
003900100326     D* DS ARCHITETTURA
004000100326     D*------------------
004100100326     D kpjba         e ds                  inz
004200100326
004300030715     D*------------------
004400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004500030715     D*------------------
004600030715     D SkSplitFLD      S             10    DIM(100)
004700030715     D SkSplitCSV      S            256    DIM(100)
004800030715     D CharCSV         S              1
004900030715     D CharTXT         S              1
005000030715     D CharNUM         S              1
005100100318     D CharSOS         S              1
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600070823     D wVATNOT_A       s                   LIKE(VATNOT)
005700060331     D wVATNOT_B       s                   LIKE(VATNOT)
005800060331     D wVATNOT_E       s                   LIKE(VATNOT)
005900100831     D wVABRSD         s                   LIKE(VABRSD)
006000100831     D wVABIND         s                   LIKE(VABIND)
006100100831     D wVABCAD         s                   LIKE(VABCAD)
006200100831     D wVABLOD         s                   LIKE(VABLOD)
006300100831     D wVABPRD         s                   LIKE(VABPRD)
006400030822     D*------------------
006500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006600030822     D*------------------
006700030822     D posDaDft        S              3  0 INZ(*zeros)
006800030822     D posADft         S              3  0 INZ(*zeros)
006900030822     D j               s              3  0 INZ(1)
007000030822     D wGiroDft        s              1  0 INZ(*zeros)
007100080418     D*------------------
007200080418     D* Costanti
007300080418     D*------------------
007400080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007500080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007600100318
007700100318     D*------------------
007800100318     D* LINKING A DEFINIZIONI ESTERNE
007900100318     D*------------------
008000100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008100100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008200100318
008300100909     D*------------------
008400100909     D* DS CALCOLO CHECK-DIGITS
008500100909     D*------------------
008600100909     D TRUL28DSX     e ds                  inz
008700010330
008800010330
008900010330
009000000913     C                   reset                   rrnum
009100990921     C                   reset                   esito
009200990921     C                   reset                   wrkesito
009300010601     C*
009400010601     C                   exsr      opeini
009500010605     C                   exsr      rwvab
009600010601     C*
009700010601     C                   seton                                        lr
009800010601
009900010601
010000010601
010100010601     C*--------------------------------------------------------
010200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010300010601     C*--------------------------------------------------------
010400010601     C     PREELA        BEGSR
010500010601     C*
010600010601     C* SE OCCORRE SPEDIRE IN FILIALE
010700010601     C                   if        invfil <> *zeros and
010800010601     C                             flgGiro = '0'
010900010601     C*
011000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011100010601     C                   eval      flgGiro = '1'
011200010601     C*
011300010601     C                   endif
011400010601     C*
011500010601     C                   ENDSR
011600010601     C***
011700010601
011800010601
011900010601
012000010601
012100010601     C*--------------------------------------------------------
012200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012300010601     C*--------------------------------------------------------
012400010601     C     ENDELA        BEGSR
012500040119     C*
012600040119     C* Effettuo la chiamata al CLLE preposto
012700040119     C                   call(e)   'TITVVTC'
012800040510     C                   parm                    parccm
012900040119     C                   parm                    parmbr
013000040119     C                   parm      '2'           paropz
013100050201     C*
013200050201     C* Effettuo lancio TISI95 solo x chiusura
013300050201     C                   CLEAR                   TISI95DS
013400050201     C                   EVAL      I95TLA = 'C'
013500050201     C                   CALL      'TISI95R'
013600050201     C                   PARM                    TISI95DS
013700000616     C*
013800010601     C                   ENDSR
013900010601     C***
014000000613
014100010601
014200010601
014300010330     C*--------------------------------------------------------
014400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014500010330     C*--------------------------------------------------------
014600010605     C     RWVAB         BEGSR
014700010330     C*
014800010330     C                   if        not %open(tivin00r)
014900010330     C                   open      tivin00r
015000010330     C                   endif
015100021025     C                   if        not %open(fivabwwr)
015200021025     C                   open      fivabwwr
015300010330     C                   endif
015400040119     C*
015500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015600040119     C                   exsr      prevat
015700040119     C*
015800040119     C                   if        chkcall = '0'
015900040119     C*
016000040119     C                   if        not %open(fivatwwr)
016100040119     C                   open      fivatwwr
016200040119     C                   endif
016300010330     C*
016400040119     C                   clear                   §CTROKVB          7 0
016500040119     C                   clear                   §CTROKVT          7 0
016600040119     C                   clear                   §CTRMO            7 0
016700040119     C                   clear                   §CTRNO            7 0
016800070823     C*
016900070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017000070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017100100318     C***                exsr      inzvar
017200100318     C***                exsr      impvab
017300010330     C*
017400030822     C                   do        *HIVAL
017500010330     C*
017600030822     C                   read      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200040510     C*
018300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018400040510     C                   z-add     *zeros        wLenVINDTA        4 0
018500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018900040510     C                   z-add     1             wFlgVINDTA
019000040510     C                   leave
019100040510     C                   endif
019200040510     C                   enddo
019300010618     C*
019400040510     C                   if        vindta > *blanks AND
019500040510     C                             wFlgVINDTA = 1
019600010330     C                   add       1             rrnum
019700010330     C*
019800010601     C                   if        *in70 = *off and
019900010330     C                             (vinflg = *blanks
020000010330     C                              or vinflg = '0'
020100010330     C                              or vinflg = '2')
020200010330     C*
020300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020400010711     C                   if        vinflg = *blanks or vinflg = '0'
020500010711     C                   clear                   vinmsg
020600010711     C                   endif
020700010601     C*
020800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020900070925     C                   eval      vindta  = ' ' + %trim(vindta)
021000070925     C*
021100080527     C                   exsr      inzvar
021200100318     C                   exsr      normCSV
021300010605     C                   exsr      impvab
021400010601     C*
021500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021600010605     C                   eval      depfil = VABLNP
021700010601     C                   exsr      repfil
021800010601     C                   if        depfil = invfil
021900021025     C                   if        vlrpoi = 999
022000030822     C                   move(P)   invfil        VABFGS
022100021025     C                   else
022200030822     C                   move(P)   vlrpoi        VABFGS
022300021025     C                   endif
022400010601     C*
022500010601     C                   exsr      PREELA
022600010601     C*
022700010604     C* Ebbene...
022800010604     C*
022900040119     C  N31              add       1             §CTROKVB
023000040119     C   32              add       1             §CTRMO
023100040119     C   31              add       1             §CTRNO
023200080418     C*
023300030822     C                   if        wGiro = 2
023400080513     C* Scrivo sempre il VAT
023500080513     C  N31              exsr      wrivat
023600080527     C                   exsr      chkvab
023700080513     C                   endif
023800010604     C*
023900010604     C                   if        *in31 = *off and
024000010604     C                             *in32 = *off
024100010604     C                   eval      vinflg = '1'
024200010604     C                   else
024300010604     C                   eval      vinflg = '2'
024400010604     C                   endif
024500010604     C                   endif
024600010601     C*
024700010604     C                   endif
024800010604     C*
024900010330     C                   else
025000010330     C                   eval      vinflg = '1'
025100010330     C                   endif
025200010601     C*
025300010601     C  N70              update    tivin000
025400010330     C*
025500030822     C  N70              enddo
025600040119     C*
025700080527     C* Verifico dati rimasti "in cannna"...
025800080527     C                   seton                                        65
025900080527     C                   exsr      chkvab
026000080527     C*
026100040119     C                   endif                                                  (endif - chkcall)
026200010601     C*
026300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026400010601     C                   if        cntNonEl = *zeros or
026500010601     C                             flgMulti = '0'
026600010330     C* Se non ci sono record con errori ...
026700010601     C                   if        §ctrno = 0 and
026800010604     C                             §ctrmo = 0 and
026900010601     C                             flgStato <> '2'
027000010330     C* ... restituisco esito OK.
027100010330     C                   eval      wrkesito = '0'
027200010330     C                   else
027300040119     C                   if        §ctrokvb > 0
027400010330     C                   eval      wrkesito = '1'
027500010330     C                   else
027600010615     C                   if        flgOk = '0'
027700010615     C                   eval      wrkesito = '2'
027800010615     C                   else
027900010615     C                   eval      wrkesito = '6'
028000010615     C                   endif
028100010330     C                   endif
028200010330     C                   endif
028300010601     C                   else
028400010601     C                   eval      wrkesito = '9'
028500010601     C                   endif
028600010330     C*
028700010330     C                   if        %open(tivin00r)
028800010330     C                   close     tivin00r
028900010330     C                   endif
029000021025     C                   if        %open(fivabwwr)
029100021025     C                   close     fivabwwr
029200010330     C                   endif
029300040512     C                   if        %open(fivatwwr)
029400040512     C                   close     fivatwwr
029500040512     C                   endif
029600010601     C*
029700010601     C                   if        vlrpoi <> 999
029800010601     C                   eval      invfil = vlrpoi
029900010601     C                   endif
030000010330     C*
030100040119     C                   if        §ctrokvb > 0
030200010601     C                             and invfil > *zeros
030300010330     C                   exsr      invio
030400010330     C                   endif
030500010601     C*
030600010618     C                   if        flgGiro = '1'
030700010601     C                   exsr      endela
030800010618     C                   endif
030900010330     C*
031000010330     C                   ENDSR
031100010330     C***
031200080527
031300080527
031400080527
031500080527     C*----------------------------------------------------*
031600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031700080527     C*----------------------------------------------------*
031800100413     C     CHKVAB        BEGSR
031900080527     C*
032000080527     C* Scrivo il VAB solo a rottura d spedizione
032100080527     C                   if        depspe = *zeros
032200080527     C  N31              eval      fivabds_sav = fivabds
032300080527     C                   eval      depspe = vabNSP
032400080527     C                   else
032500080527     C                   if        depspe = vabNSP AND *in65 = *off
032600100413     C   51              add       VABNCL        sav_VABNCL
032700100413     C   51              add       VABPKB        sav_VABPKB
032800100413     C   51              add       VABVLB        sav_VABVLB
032900100720     C   52              z-add     VABNCL        sav_VABNCL
033000100720     C   52              z-add     VABPKB        sav_VABPKB
033100100720     C   52              z-add     VABVLB        sav_VABVLB
033200100720     C   53              add       VABNCL        sav_VABNCL
033300100720     C   53              z-add     VABPKB        sav_VABPKB
033400100720     C   53              z-add     VABVLB        sav_VABVLB
033500080527     C                   else
033600080527     C  N31              eval      fivabds_ok = fivabds_sav
033700080527     C  N31              exsr      wrivab
033800080527     C  N31              eval      fivabds_sav = fivabds
033900080527     C                   eval      depspe = vabNSP
034000080527     C                   endif
034100080527     C                   endif
034200080527     C*
034300080527     C                   ENDSR
034400040119
034500040119
034600040119
034700040119     C*----------------------------------------------------*
034800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034900040119     C*----------------------------------------------------*
035000040510     C     WRIVAB        BEGSR
035100080513     C*
035200080513     C* Gestisco l'eventuale rottura x numero spedizione
035300080527     C                   if        VABNSP <> depspe or *in65 = *on
035400070301     C                   if        wVATNOT_E <> *blanks
035500070301     C                   eval      VABCTM = '7Q'
035600070301     C                   endif
035700100413     C                   write     fivab000
035800080513     C                   endif
035900040119     C*
036000040119     C                   ENDSR
036100040119
036200040119
036300040119
036400040119     C*----------------------------------------------------*
036500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036600040119     C*----------------------------------------------------*
036700040510     C     WRIVAT        BEGSR
036800060512     C*
036900060512     C* Inizializzo i campi chiave
037000060512     C                   eval      VATFGS = VABFGS
037100060512     C                   eval      VATAAS = VABAAS
037200060512     C                   eval      VATCCM = VABCCM
037300070823     C                   eval      VATNRS = VABNRS
037400060512     C                   eval      VATNSP = VABNSP
037500060512     C                   eval      VATLNP = VABLNP
037600040714     C*
037700040714     C* Solo se VATNOT valorizzato scarico il buffer
037800040714     C                   if        VATNOT <> *blanks
037900040119     C*
038000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038100040510     C                   if        VATTRC = *blanks
038200040510     C                   eval      VATTRC = 'E'
038300040510     C                   endif
038400040510     C                   write     FIVAT000
038500040119     C                   add       1             §CTROKVT
038600040714     C*
038700040714     C                   endif
038800060331     C*
038900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039000060331     C                   if        wVATNOT_A <> *blanks
039100060331     C                   eval      VATNOT = wVATNOT_A
039200060331     C                   eval      VATTRC = 'A'
039300060331     C                   write     FIVAT000
039400060331     C                   add       1             §CTROKVT
039500060331     C                   endif
039600060331     C*
039700060331     C                   if        wVATNOT_B <> *blanks
039800060331     C                   eval      VATNOT = wVATNOT_B
039900060331     C                   eval      VATTRC = 'B'
040000060331     C                   write     FIVAT000
040100060331     C                   add       1             §CTROKVT
040200060331     C                   endif
040300060331     C*
040400060331     C                   if        wVATNOT_E <> *blanks
040500060331     C                   eval      VATTRC = 'E'
040600100901     C*
040700100901     C* X ogni collo indicato in testata
040800110616     C***                clear                   TRUL28DSX
040900110616     C***                z-add     *zeros        wWhoIs           11 0
041000110616     C***                z-add     1             wCollo            5 0
041100110616     C***                eval      wWhoIs=%dec(%trim(%subst(wVATNOT_E:3:11))
041200110616     C***                                 :11:0)
041300110616     C***                dow       wCollo <= VABNCL
041400100901     C*
041500100901     C* Valorizzo il buffer di scrittura del FIVAT
041600110616     C***                eval      I284STD = 'E10'
041700110616     C***                eval      I284COD = %trim(%editc(wWhoIs:'X'))
041800110616     C***                call      'TRUL28R4'
041900110616     C***                parm                    TRUL28DSX
042000100909     C* Se tutto ok scrivo il "chi sono" corrente
042100110616     C***                if        O284err = *blanks
042200110616     C***                eval      VATNOT = %subst(wVATNOT_E:1:2) +
042300110616     C***                                   %trim(O284COD)
042400110919 xxx C                   eval      VATNOT = wVATNOT_E
042500110616     C                   write     FIVAT000
042600110616     C                   add       1             §CTROKVT
042700110616     C***                endif
042800100901     C*
042900110616     C***                eval      wWhoIs = wWhoIs + 1
043000110616     C***                add       1             wCollo
043100110616     C***                enddo
043200100901     C*
043300060331     C                   endif
043400040119     C*
043500040119     C                   ENDSR
043600040119
043700040119
043800040119
043900040119     C*----------------------------------------------------*
044000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044100040119     C*----------------------------------------------------*
044200040119     C     PREVAT        BEGSR
044300040119     C*
044400040119     C* Compongo il nome del membro da dare al FIVATWWR
044500040119     C                   eval      parmbr = vlrhdl
044600040119     C                   movel     'M'           parmbr
044700040510     C                   eval      parccm = vlrksc
044800040119     C                   eval      paropz = '1'
044900040119     C* Effettuo la chiamata al CLLE preposto
045000040119     C                   call(e)   'TITVVTC'
045100040119     C                   parm                    parccm
045200040119     C                   parm                    parmbr
045300040119     C                   parm                    paropz
045400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045500040119     C                   if        %error
045600040119     C                   movel     '1'           chkcall
045700040119     C                   else
045800040119     C                   movel     '0'           chkcall
045900040119     C                   endif
046000040119     C*
046100040119     C                   ENDSR
046200010601
046300010601
046400010601
046500010330     C*----------------------------------------------------*
046600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046700010330     C*----------------------------------------------------*
046800040119     C     INZVAR        BEGSR
046900080527     C*
047000080527     C                   setoff                                       65
047100010330     C*
047200030822     C* Inizializzo variabili di wrk
047300020204     C                   Z-ADD     *zeros        Num5_0            5 0
047400101018     C                   MOVEL     *blanks       wFlgCAS
047500060331     C                   MOVEL     *blanks       wVATNOT_A
047600060331     C                   MOVEL     *blanks       wVATNOT_B
047700060331     C                   MOVEL     *blanks       wVATNOT_E
047800100831     C                   MOVEL     *blanks       wVABRSD
047900100831     C                   MOVEL     *blanks       wVABIND
048000100831     C                   MOVEL     *blanks       wVABCAD
048100100831     C                   MOVEL     *blanks       wVABLOD
048200100831     C                   MOVEL     *blanks       wVABPRD
048300030822     C*
048400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048500030822     C                   CLEAR                   SkSplitCSV
048600030822     C                   EVAL      i = 1
048700030822     C                   EVAL      posDa = *zeros
048800030822     C                   EVAL      posA  = *zeros
048900080527     C*
049000080527     C                   CLEAR                   FIVABDS
049100080527     C                   CLEAR                   FIVAT000
049200030822     C*
049300030822     C* Reimposto i valori di default
049400030822     C                   EXSR      DEFCAM
049500030822     C*
049600010330     C                   ENDSR
049700010330     C*----------------------------------------------------*
049800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049900010330     C*----------------------------------------------------*
050000010330     C     DEFCAM        BEGSR
050100030822     C*
050200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050300030715     C* e delimitatore testo.
050400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050700100318     C*
050800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050900100318     C                   EVAL      CharSOS = CharNUM
051000030822     C*
051100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051200100720     C                   SETOFF                                       505152
051300100720     C                   SETOFF                                       53
051400100413     C                   SELECT
051500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051600100720     C                   SETON                                        50
051700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051800100413     C                   SETON                                        51
051900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052000100720     C                   SETON                                        52
052100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052200100720     C                   SETON                                        53
052300100413     C                   ENDSL
052400030822     C*
052500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052600030822     C                   EVAL      posDaDft = 1
052700030822     C                   EVAL      posADft  = 0
052800030822     C                   EVAL      wGiroDft = 0
052900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053000030822     C                             posDaDft > 0
053100030822     C*
053200030822     C* Gestisco il 1° giro
053300030822     C                   IF        wGiroDft = 0
053400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053600030822     C* Incremento il contatore dei "giri"
053700030822     C                   EVAL      wGiroDft = 1
053800030822     C                   ELSE
053900030822     C                   EVAL      posDaDft = posADft
054000030822     C                   ENDIF
054100030822     C* Eseguo lo scan x trovare la fine del campo corrente
054200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054300030822     C*
054400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054600030822     C                   IF        posDaDft > 0 AND
054700030822     C                             posADft  > 0
054800030822     C* NCL
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'NCL'
055300030822     C                   EVAL      PiStr=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   EXSR      CHKNUM
055700030822     C                   IF        PiInt=*on
055800030822     C                   Z-ADD     PiVal         VABNCL
055900030822     C                   ENDIF
056000030822     C                   ENDIF
056100030822     C* CCM
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'CCM'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiInt=*on
057100030822     C                   Z-ADD     PiVal         VABCCM
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400030822     C* LNP
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'LNP'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiInt=*on
058400030822     C                   Z-ADD     PiVal         VABLNP
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* NRS
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'NRS'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABNRS
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* CTR
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'CTR'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABCTR
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* PKB
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'PKB'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiNum=*on
062300030822     C                   Z-ADD     PiVal         VABPKB
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* VLB
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'VLB'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiNum=*on
063600030822     C                   Z-ADD     PiVal         VABVLB
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* QFT
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'QFT'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiNum=*on
064900030822     C                   Z-ADD     PiVal         VABQFT
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* CBO
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'CBO'
065700030822     C                   EVAL      VABCBO=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* TSP
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'TSP'
066600030822     C                   EVAL      VABTSP=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* VAS
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'VAS'
067500030822     C                   EVAL      VABVAS=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* VCA
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'VCA'
068400030822     C                   EVAL      VABVCA=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800070925     C* TIC
068900070925     C                   IF        %subst(
069000070925     C                             %subst(vlrppt:posDaDft+1:
069100070925     C                             posADft-posDaDft-1):1:3)
069200070925     C                             = 'TIC'
069300070925     C                   EVAL      VABTIC=%trim(%subst(
069400070925     C                             %subst(vlrppt:posDaDft+1:
069500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600070925     C                   ENDIF
069700030822     C* GCA
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'GCA'
070200030822     C                   EVAL      VABGCA=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* CTM
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'CTM'
071100030822     C                   EVAL      VABCTM=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* FFD
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'FFD'
072000030822     C                   EVAL      VABFFD=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* VAD
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'VAD'
072900030822     C                   EVAL      VABVAD=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* GMA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'GMA'
073800030822     C                   EVAL      VABGMA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* GGA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GGA'
074700030822     C                   EVAL      VABGGA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* GVA
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'GVA'
075600030822     C                   EVAL      VABGVA=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* TC1
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'TC1'
076500030822     C                   EVAL      VABTC1=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* TC2
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'TC2'
077400030822     C                   EVAL      VABTC2=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800040714     C* VATTRC
077900040714     C                   IF        %subst(
078000040714     C                             %subst(vlrppt:posDaDft+1:
078100040714     C                             posADft-posDaDft-1):1:3)
078200040714     C                             = 'TRC'
078300040714     C                   EVAL      VATTRC=%trim(%subst(
078400040714     C                             %subst(vlrppt:posDaDft+1:
078500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600040714     C                   ENDIF
078700030822     C* ...
078800030822     C                   ENDIF
078900030822     C                   ENDDO
079000020204     C*
079100010330     C                   ENDSR
079200010607     C*----------------------------------------------------*
079300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079400010607     C*----------------------------------------------------*
079500010607     C     IMPVAB        BEGSR
079600010607     C*
079700010607     C                   SETOFF                                       3132
079800030515     C*
079900010607     C                   MOVEL     datcor        VABAAS
080000010607     C                   MOVE      datcor        VABMGS
080100010607     C*
080200030715     C**********
080300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080400030715     C**********
080500030715     C                   IF        wGiro = *zeros
080600040113     C*
080700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081000080429     C                             %trim(intcol(5))<>*blanks
081100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081300080429     C                                      %trim(intcol(5))
081400070823     C     ';':CharCSV   XLATE     vindta        vindta
081500040510     C                   ENDIF
081600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081700031201     C     minu:maiu     XLATE     vindta        vindta
081800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081900030715     C                   DOW       posDa <= %len(%trim(vindta))
082000030715     C*
082100030715     C* Gestisco il 1° campo
082200030715     C                   IF        i = 1
082300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082400030715     C                   EVAL      posDa = 1
082500030715     C* Eseguo lo scan x trovare la fine del primo campo
082600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C* X i campi successivi al 1°
083100030715     C                   ELSE
083200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083300030715     C                   EVAL      posDa = posA + 1
083400030715     C* Eseguo lo scan x trovare la fine del campo corrente
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* Gestisco l'ultimo campo
083700030715     C                   IF        posA = *zeros
083800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C                   LEAVE
084300030715     C                   ELSE
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   ENDIF
084800030715     C                   ENDIF
084900030715     C* Incremento il contatore d campo
085000030715     C                   EVAL      i = i +1
085100030715     C                   ENDDO
085200030715     C*
085300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085400030715     C* delimitazione testo
085500030715     C                   EVAL      i = 1
085600030715     C                   DOW       i < %elem(SkSplitFLD)
085700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085900030715     C                   EVAL      i = i + 1
086000030715     C                   ENDDO
086100030715     C                   EVAL      wGiro = 1
086200030715     C                   ELSE
086300030715     C                   EVAL      wGiro = 2
086400031204     C**********
086500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086600031204     C* d separatore campo
086700031204     C**********
086800031204     C                   EVAL      vindta = %trim(vindta)
086900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087000031204     C                   EVAL      vindta = %subst(vindta:2)
087100031204     C                   ENDDO
087200031204     C*
087300031204     C                   Z-ADD     *zeros        lunghInput        4 0
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087800031204     C                   ENDDO
087900030715     C**********
088000030715     C* Effettuo lo split del campo dati d input
088100030715     C**********
088200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088300030715     C                   DOW       posDa <= %len(%trim(vindta))
088400030715     C*
088500030715     C* Gestisco il 1° campo
088600030715     C                   IF        i = 1
088700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088800030715     C                   EVAL      posDa = 1
088900030715     C* Eseguo lo scan x trovare la fine del primo campo
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* A questo punto "estraggo" il campo corrente
089200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089300030715     C                                                    (posA-posDa))
089400030715     C* X i campi successivi al 1°
089500030715     C                   ELSE
089600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089700030715     C                   EVAL      posDa = posA + 1
089800031204     C* Verifico che nn vi sia il campo nn valorizzato
089900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090000031204     C* Se campo nn valorizzato skippo al prossimo
090100031204     C                   EVAL      SkSplitCSV(i) = *blanks
090200031204     C                   EVAL      posA  = posA + 1
090300031204     C                   ELSE
090400030715     C* Eseguo lo scan x trovare la fine del campo corrente
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* Gestisco l'ultimo campo
090700030715     C                   IF        posA = *zeros
090800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   LEAVE
091300030715     C                   ELSE
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   ENDIF
091800031204     C                   ENDIF
091900030715     C                   ENDIF
092000030715     C* Incremento il contatore d campo
092100030715     C                   EVAL      i = i +1
092200030715     C                   ENDDO
092300030715     C*
092400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092500030715     C* delimitazione testo
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitCSV)
092800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093000030715     C                   EVAL      i = i + 1
093100030715     C                   ENDDO
093200100720     C*
093300100720     C* Stacco un progressivo x numero spedizione
093400100720 xxx C***                EXSR      repNSP
093500100326     C*
093600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitFLD)
093900030715     C*
094000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094200030715     C                             SkSplitCSV(i) <> *blanks
094300030721     C*** ATB
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094500030721     C                   EVAL      VABATB = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030715     C*** RSD
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095200030715     C* ==
095300030715     C                   ENDIF
095400030721     C*** RD2
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** IND
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096000030715     C                   EVAL      VABIND = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** LOD
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** PRD
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030721     C*** NZD
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030715     C*** RMA
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** NOT
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** NT2
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** VCA
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** VAS
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** FFD
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030721     C*** GC1
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** GC2
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** TSP
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100900030721     C                   ENDIF
101000040510     C*** CBO
101100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101300040510     C                   ENDIF
101400030721     C*** NAS
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TIC
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GCA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** XCO
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** CTM
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TCR
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** CTS
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** FTM
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** VAD
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GMA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** GGA
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GVA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** TC1
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** TC2
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** SCL
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** RMO
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** NMO
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*
108300030721     C* Reperisco quindi i campi numerici...
108400040510     C*** NSP
108500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108600040510     C                   EVAL      PiStr=SkSplitCSV(i)
108700040510     C                   EXSR      CHKNUM
108800040510     C                   IF        PiInt=*on
108900040510     C                   Z-ADD     PiVal         VABNSP
109000040510     C                   ELSE
109100040510     C                   SETON                                        31
109200040510     C                   EVAL      VABNSP = *zeros
109300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109400040510     C                             + ' ' + 'VABNSP'
109500040510     C                   ENDIF
109600040510     C                   ENDIF
109700071106     C*** RMN
109800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109900071106     C                   EVAL      PiStr=SkSplitCSV(i)
110000071106     C                   EXSR      CHKNUM
110100071106     C                   IF        PiInt=*on
110200071106     C                   Z-ADD     PiVal         VABRMN
110300071106     C                   ELSE
110400071106     C                   SETON                                        32
110500071106     C                   EVAL      VABRMN = *zeros
110600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110700071106     C                             + ' ' + 'VABRMN'
110800071106     C                   ENDIF
110900071106     C                   ENDIF
111000030721     C*** CAD
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiInt=*on
111500030721     C                   Z-ADD     PiVal         Num5_0
111600030721     C                   MOVEL(p)  Num5_0        VABCAD
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABCAD = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCAD'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** NCL
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on
112900030721     C                   Z-ADD     PiVal         VABNCL
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      VABNCL = *zeros
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040122     C                             + ' ' + 'VABNCL'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** PKB
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiNum=*on
114200030721     C                   Z-ADD     PiVal         VABPKB
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABPKB = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABPKB'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** CAS
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115200101018     C                   IF        SkSplitCSV(i) <> *blanks AND
115300101018     C                             SkSplitCSV(i) <> *zeros
115400101018     C                   EVAL      wFlgCAS = '1'
115500101018     C                   ENDIF
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiNum=*on
115900030721     C                   Z-ADD     PiVal         VABCAS
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      VABCAS = *zeros
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABCAS'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** CCM
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on      AND
117200030721     C                             PiVal<=9999999 AND
117300030721     C                             PiVal>*zeros
117400030721     C                   Z-ADD     PiVal         VABCCM
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      VABCCM = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900040510     C                             + ' ' + 'VABCCM'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** LNP
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiInt=*on    AND
118700030721     C                             PiVal<=999   AND
118800030721     C                             PiVal>*zeros
118900030721     C                   Z-ADD     PiVal         VABLNP
119000030721     C                   ELSE
119100040715     C                   SETON                                        32
119200030721     C                   EVAL      VABLNP = *zeros
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400040510     C                             + ' ' + 'VABLNP'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** NRS
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on AND
120200030721     C                             PiVal<=99
120300030721     C                   Z-ADD     PiVal         VABNRS
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABNRS'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** LNA
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on    AND
121500030721     C                             PiVal<=999   AND
121600030721     C                             PiVal>*zeros
121700030721     C                   Z-ADD     PiVal         VABLNA
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      VABLNA = *zeros
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABLNA'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** CTR
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on  AND
123000030721     C                             PiVal<=999
123100030721     C                   Z-ADD     PiVal         VABCTR
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABCTR'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** IAS
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABIAS
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABIAS'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** VLB
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiNum=*on
125500030721     C                   Z-ADD     PiVal         VABVLB
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABVLB'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** QFT
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiNum=*on
126700030721     C                   Z-ADD     PiVal         VABQFT
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABQFT'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** NCD
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on      AND
127900030721     C                             PiVal<=9999999
128000030721     C                   Z-ADD     PiVal         VABNCD
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABNCD'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** NCA
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiInt=*on      AND
129200030721     C                             PiVal<=9999999
129300030721     C                   Z-ADD     PiVal         VABNCA
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABNCA'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** ZNC
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on AND
130500030721     C                             PiVal<=99
130600030721     C                   Z-ADD     PiVal         VABZNC
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABZNC'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** DCR
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiInt=*on       AND
131800030721     C                             PiVal<=99999999
131900030721     C                   Z-ADD     PiVal         VABDCR
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132300030721     C                             + ' ' + 'VABDCR'
132400030721     C                   ENDIF
132500030721     C                   ENDIF
132600030721     C*** HCR
132700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132800030721     C                   EVAL      PiStr=SkSplitCSV(i)
132900030721     C                   EXSR      CHKNUM
133000030721     C                   IF        PiInt=*on   AND
133100030721     C                             PiVal<=9999
133200030721     C                   Z-ADD     PiVal         VABHCR
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABHCR'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900030721     C*** VMD
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiNum=*on
134400030721     C                   Z-ADD     PiVal         VABVMD
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABVMD'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** ANT
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on        AND
135600030721     C                             PiVal<=999999999
135700030721     C                   Z-ADD     PiVal         VABANT
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABANT'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** CMO
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on
136900030721     C                   Z-ADD     PiVal         Num5_0
137000030721     C                   MOVEL(p)  Num5_0        VABCMO
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      VABCMO = *zeros
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABCMO'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800040510     C*
137900040510     C* Reperisco i campi del FIVAT
138000040510     C*** VATNOT
138100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138300040510     C                   ENDIF
138400040510     C*** VATTRC
138500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138700040510     C                   ENDIF
138800060331     C*** VATNOT_A
138900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139100060331     C                   ENDIF
139200060331     C*** VATNOT_B
139300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139500060331     C                   ENDIF
139600060331     C*** VATNOT_E
139700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139900060331     C                   ENDIF
140000070823     C*
140100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140200100831     C*** RSD
140300100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
140400101018     C                   MOVEL     *blanks       wRSD             70
140500101018     C                   EVAL      wRSD = %trim(SkSplitCSV(i))
140600101018     C                   EVAL      VABRSD = %subst(wRSD:1:35)
140700101018     C                   EVAL      VABRD2 = %subst(wRSD:36:35)
140800100831     C                   ENDIF
140900110613     C*** NSP / RMN / NOT_E
141000101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
141100110613     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141200101021     C                   EVAL      PiStr=SkSplitCSV(i)
141300101021     C                   EXSR      CHKNUM
141400101021     C                   IF        PiInt=*on
141500101021     C                   Z-ADD     PiVal         VABNSP
141600101021     C                   Z-ADD     PiVal         VABRMN
141700101021     C                   ELSE
141800101021     C                   SETON                                        31
141900101021     C                   EVAL      VABNSP = *zeros
142000101021     C                   EVAL      VABRMN = *zeros
142100101021     C                   EVAL      vinmsg = %trimr(vinmsg)
142200110613     C                             + ' ' + 'VABNSP VABRMN VATNOT_E'
142300101021     C                   ENDIF
142400101021     C                   ENDIF
142500110613     C*** GC1
142600110613     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABGC1'
142700110613     C                   SELECT
142800110613     C                   WHEN      %trim(SkSplitCSV(i)) =
142900110613     C                                  'CONSEGNA MATTINA'
143000110613     C                   EVAL      VABGC1 = 'P'
143100110613     C                   WHEN      %trim(SkSplitCSV(i)) =
143200110613     C                                  'CONSEGNA POMERIGGIO'
143300110613     C                   EVAL      VABGC1 = 'M'
143400110613     C                   ENDSL
143500110613     C                   ENDIF
143600070925     C*
143700071029     C***  <===  -----------------------------------------
143800040510     C*
143900030715     C                   ENDIF
144000030715     C                   EVAL      i = i + 1
144100030715     C                   ENDDO
144200070925     C*
144300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144400100831     C*
144500100831     C                   IF        wVABRSD <> *blanks
144600100831     C                   EVAL      VABRSD = wVABRSD
144700100831     C                   ENDIF
144800100831     C*
144900100831     C                   IF        wVABIND <> *blanks
145000100831     C                   EVAL      VABIND = wVABIND
145100100831     C                   ENDIF
145200100831     C*
145300100831     C                   IF        wVABCAD <> *blanks
145400100831     C                   EVAL      VABCAD = wVABCAD
145500100831     C                   ENDIF
145600100831     C*
145700100831     C                   IF        wVABLOD <> *blanks
145800100831     C                   EVAL      VABLOD = wVABLOD
145900100831     C                   ENDIF
146000100831     C*
146100100831     C                   IF        wVABPRD <> *blanks
146200100831     C                   EVAL      VABPRD = wVABPRD
146300100831     C                   ENDIF
146400070925     C*
146500071029     C***  <===  -----------------------------------------
146600070925     C*
146700070925     C* Considerazioni finali su CBO/CAS
146800100318     C                   IF        wFlgCAS = '1'    OR
146900100318     C                             VABCAS  > *zeros
147000070925     C                   IF        VABCBO = '1'
147100070925     C                   EVAL      VABCBO = '4'
147200070925     C                   ENDIF
147300070925     C                   IF        VABCBO = '2'
147400070925     C                   EVAL      VABCBO = '6'
147500070925     C                   ENDIF
147600070925     C                   ENDIF
147700070925     C***  <===  ----------------------------
147800040510     C*
147900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148000040510     C                   IF        VABNSP = *zeros
148100040510     C                   SETON                                        31
148200040510     C                   ENDIF
148300040715     C*
148400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148500040715     C                   IF        VABLNP = *zeros
148600040715     C                   SETON                                        31
148700040715     C                   ENDIF
148800040510     C*
148900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149000040510     C                   IF        VABRMN = *zeros
149100040510     C                   Z-ADD     VABNSP        VABRMN
149200040510     C                   ENDIF
149300040510     C*
149400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149500040510     C                   IF        VABRMA = *blanks
149600040714     C                   MOVEL     VABRMN        VABRMA
149700040510     C                   ENDIF
149800040128     C*
149900040510     C* Se provincia nn valorizzata la reperisco
150000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150100040510     C                   IF        VABPRD = *blanks
150200040128     C                   CLEAR                   TISI95DS
150300040128     C                   EVAL      I95TCN = '3'
150400040128     C                   Z-ADD     datcor        I95DAT
150500040128     C                   EVAL      I95NAR = VABNZD
150600040128     C                   EVAL      I95CAP = VABCAD
150700040128     C                   EVAL      I95LOC = VABLOD
150800040128     C                   CALL      'TISI95R'
150900040128     C                   PARM                    TISI95DS
151000040128     C                   EVAL      VABPRD = O95PRV
151100040510     C                   ENDIF
151200020204     C*
151300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151400020204     C                   EXSR      CHKIMPDIV
151500030715     C                   ENDIF
151600020204     C*
151700010607     C                   ENDSR
151800010607     C*----------------------------------------------------*
151900010601
152000020204
152100020204     C*----------------------------------------------------*
152200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152300020204     C*----------------------------------------------------*
152400020204     C     CHKIMPDIV     BEGSR
152500020204     C*
152600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152700020204     C                   Z-ADD     *zeros        wrkDec            9 9
152800020204     C*
152900020204     C* Come prima cosa effettuo considerazioni sulla divisa
153000020204     C                   IF        vabIAS > *zeros
153100020204     C                   IF        vabVAS <> 'EUR'
153200020204     C                   EVAL      vabVAS =  'ITL'
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C                   IF        vabCAS > *zeros
153700020204     C                   IF        vabVCA <> 'EUR'
153800020204     C                   EVAL      vabVCA =  'ITL'
153900020204     C                   ENDIF
154000020204     C                   ENDIF
154100020204     C*
154200020204     C                   IF        vabVMD > *zeros
154300020204     C                   IF        vabVAD <> 'EUR'
154400020204     C                   EVAL      vabVAD =  'ITL'
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154900020204     C                   Z-ADD     vabIAS        wrkDec
155000020204     C                   IF        wrkDec > *zeros
155100020204     C                   IF        vabVAS = 'ITL'
155200020204     C                   EVAL      vabIAS = *zeros
155300020204     C                   ENDIF
155400020204     C                   ENDIF
155500020204     C*
155600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155700020204     C                   Z-ADD     vabCAS        wrkDec
155800020204     C                   IF        wrkDec > *zeros
155900020204     C                   IF        vabVCA = 'ITL'
156000020204     C                   EVAL      vabCAS = *zeros
156100020204     C                   ENDIF
156200020204     C                   ENDIF
156300020204     C*
156400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156500020204     C                   Z-ADD     vabVMD        wrkDec
156600020204     C                   IF        wrkDec > *zeros
156700020204     C                   IF        vabVAD = 'ITL'
156800020204     C                   EVAL      vabVMD = *zeros
156900020204     C                   ENDIF
157000020204     C                   ENDIF
157100020204     C*
157200020204     C                   ENDSR
157300020204     C***
157400010330
157500100318
157600100318
157700100318     C*----------------------------------------------------*
157800100318     C*  CONTROLLO NUMERICITA' CAMPI
157900100318     C*----------------------------------------------------*
158000100318     C     CHKNUM        BEGSR
158100100318     C*
158200100318     C                   IF        PiDecChr = *blanks
158300100318     C                   EVAL      PiDecChr = CharNUM
158400100318     C                   ENDIF
158500100318     C*
158600100318     C                   callp     UBISNUM_Check(PiStr
158700100318     C                                          :PiDecChr
158800100318     C                                          :PiVal
158900100318     C                                          :PiNum
159000100318     C                                          :PiInt)
159100100318     C*
159200100318     C                   ENDSR
159300100318     C***
159400010601
159500010601
159600010601
159700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159800010601     C     repfil        BEGSR
159900010601     C*
160000010601     C                   if        invfil = *zeros and
160100010601     C                             depfil > *zeros and
160200010629     C                             (vinflg = *blanks or
160300010629     C                              vinflg = *zeros)
160400010601     C
160500010601     C                   eval      invfil = depfil
160600010601     C                   endif
160700010601     C*
160800010601     C                   if        depfil <> invfil and
160900010601     C                             invfil > *zeros
161000010601     C                   eval      flgMulti = '1'
161100010601     C                   if        vinflg = *blanks
161200010601     C                   add       1             cntNonEl
161300010601     C                   endif
161400010601     C                   endif
161500010601     C*
161600010601     C                   if        vinflg = '2'
161700010601     C                   eval      flgStato = '2'
161800010601     C                   endif
161900010601     C*
162000010601     C                   ENDSR
162100010601     C***
162200010601
162300010601
162400010601
162500010330
162600040119      /TITLE Invio dei dati al punto operativo.
162700040119     C     invio         BEGSR
162800040119     C*
162900040119     C* 1° invio FIVAT
163000040119     C                   reset                   dscmz
163100040119     C                   move      vlrpoi        cmzdst
163200040119     C                   eval      cmzfld = 'FIVATWWR'
163300040119     C                   eval      cmzmbd = vlrhdl
163400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163500040119     C***                if        prmfir = *blanks
163600040119     C                   eval      cmzfla = 'FIVAT00F'
163700040119     C                   eval      cmzmba = 'FIVAT00F'
163800040119     C***                else
163900040119     C***                eval      cmzfla = prmfir
164000040119     C***                eval      cmzmba = prmfir
164100040119     C***                endif
164200040119     C                   eval      cmznrr = *zeros
164300040119     C                   move      §ctrokvt      cmznrr
164400040119     C                   eval      cmzlba = vlrfl1
164500040119     C                   call(e)   'TIS711C'
164600040119     C                   parm                    dscmz
164700040119     C                   parm      *blanks       esito
164800040119     C                   if        %error
164900040119     C                             or cmzerr = '1'
165000040119     C                             or esito  = '1'
165100040119     C                   eval      wrkesito = '3'
165200040119     C                   else
165300040119     C*
165400040119     C* 2° invio FIVAB
165500040119     C                   reset                   dscmz
165600040119     C                   move      vlrpoi        cmzdst
165700040119     C                   eval      cmzfld = vlrfou
165800040119     C                   eval      cmzmbd = vlrhdl
165900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166000040119     C***                if        prmfir = *blanks
166100040119     C                   eval      cmzfla = 'FIVAB00F'
166200040119     C                   eval      cmzmba = 'FIVAB00F'
166300040119     C***                else
166400040119     C***                eval      cmzfla = prmfir
166500040119     C***                eval      cmzmba = prmfir
166600040119     C***                endif
166700040119     C                   eval      cmznrr = *zeros
166800040119     C                   move      §ctrokvb      cmznrr
166900040119     C                   eval      cmzlba = vlrfl1
167000040119     C                   call(e)   'TIS711C'
167100040119     C                   parm                    dscmz
167200040119     C                   parm      *blanks       esito
167300040119     C                   if        %error
167400040119     C                             or cmzerr = '1'
167500040119     C                             or esito  = '1'
167600040119     C                   eval      wrkesito = '3'
167700040119     C                   endif
167800040119     C                   endif
167900040119     C*
168000040119     C                   ENDSR
168100040119     C***
168200010601
168300010601
168400010601
168500010601
168600010601      /TITLE Invio dei dati al punto operativo.
168700010601     C     opeini        BEGSR
168800010601     C*
168900010601     C* Inizializzo flag e contatori operativi
169000010601     C                   movel     '0'           flgGiro           1
169100010601     C                   movel     '0'           flgMulti          1
169200010601     C                   movel     '1'           flgStato          1
169300010615     C                   movel     '0'           flgOk             1
169400010601     C                   z-add     *zeros        cntNonEl         10 0
169500010601     C                   z-add     *zeros        depfil            3 0
169600010601     C                   z-add     *zeros        invfil            3 0
169700010601     C*
169800010601     C                   ENDSR
169900010601     C***
170000070326
170100070326
170200070326
170300070326
170400070326     C     *pssr         BEGSR
170500070329     C*
170600070329     C                   if        %open(tivin00r)
170700070329     C                   close     tivin00r
170800070329     C                   endif
170900070329     C                   if        %open(fivabwwr)
171000070329     C                   close     fivabwwr
171100070329     C                   endif
171200070329     C                   if        %open(fivatwwr)
171300070329     C                   close     fivatwwr
171400070329     C                   endif
171500070326     C*
171600070326     C* Effettuo la chiamata al CLLE preposto
171700070326     C                   call(e)   'TITVVTC'
171800070326     C                   parm                    parccm
171900070326     C                   parm                    parmbr
172000070326     C                   parm      '2'           paropz
172100070326     C*
172200070326     C                   eval      wrkesito = '2'
172300070404     C*
172400070404     C                   seton                                        LR
172500070326     C*
172600070326     C                   ENDSR     '*CANCL'
172700070326     C***
172800100318
172900100318
173000100318
173100100318     C     normCSV       BEGSR
173200100318     C*
173300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173400100318     C* separatori campi
173500100318     C                   if        CharTXT <> *blanks
173600100318     C                   z-add     *zeros        wLenVINDTA        4 0
173700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173800100318     C                   setoff                                       80
173900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174200100318     C                   if        not *in80
174300100318     C                   seton                                        80
174400100318     C                   else
174500100318     C                   setoff                                       80
174600100318     C                   endif
174700100318     C                   endif
174800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174900100318     C                             and *in80 = *on
175000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175100100318     C                   endif
175200100318     C                   enddo
175300100318     C                   endif
175400100318     C*
175500100318     C                   ENDSR
175600100318     C***
175700070326
175800100326
175900100326
176000100326     C*----------------------------------------------------*
176100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176200100326     C*----------------------------------------------------*
176300100326     C     REPNSP        BEGSR
176400100326     C*
176500100326     C* NSP => Stacco un numeratore da AZNUM
176600100326     C                   clear                   TRUL33DS
176700100326     C                   eval      I33OPE = *zeros
176800100326     C                   eval      I33CNU = 302
176900100326     C                   eval      I33NUM = 1
177000100326     C                   movel     TRUL33DS      KPJBU
177100100326     C                   call      'TRUL33R'
177200100326     C                   parm                    KPJBA
177300100326     C                   movel     KPJBU         TRUL33DS
177400100326     C                   if        O33ERR = *zeros
177500100326     C                   z-add     O33NRF        VABNSP
177600100326     C                   z-add     O33NRF        VATNSP
177700100326     C                   else
177800100326     C                   SETON                                        31
177900100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178000100326     C                             + ' ' + 'VABNSP VATNSP'
178100100326     C                   endif
178200100326     C*
178300100326     C                   ENDSR
178400010330
178500100326
178600010330
178700000613     C     *inzsr        BEGSR
178800990910     C*
178900990910     C     *entry        plist
179000990920     C                   parm                    tivlrds
179100990921     C                   parm      wrkesito      esito
179200000724     C                   parm                    prmlit
179300000710     C                   parm                    prmfir
179400010330     C*
179500010330     C* CALCOLA LA DATA CORRENTE
179600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179700100318     C                   eval      datcor = %dec(%date() : *ISO)
179800000613     C*
179900000613     C                   ENDSR
180000000613     C***
180100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180200110613W_VABRSD;VABIND;VABLOD;VABPRD;VABCAD;W_VABNSP;VATNOT_B;VABRMA;
180300110613VABNCL;VABPKB;VABNAS;VABNOT;W_VABGC1;VABCAS;FIL001;VABIAS;
180400110613
180500101018
180600100318
