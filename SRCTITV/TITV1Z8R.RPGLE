000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
040800100909     C                   clear                   TRUL28DSX
040900100909     C                   z-add     *zeros        wWhoIs           11 0
041000100901     C                   z-add     1             wCollo            5 0
041100100909     C                   eval      wWhoIs=%dec(%trim(%subst(wVATNOT_E:3:11))
041200100909     C                                    :11:0)
041300100901     C                   dow       wCollo <= VABNCL
041400100901     C*
041500100901     C* Valorizzo il buffer di scrittura del FIVAT
041600100909     C                   eval      I284STD = 'E10'
041700100909     C                   eval      I284COD = %trim(%editc(wWhoIs:'X'))
041800100909     C                   call      'TRUL28R4'
041900100909     C                   parm                    TRUL28DSX
042000100909     C* Se tutto ok scrivo il "chi sono" corrente
042100100909     C                   if        O284err = *blanks
042200100901     C                   eval      VATNOT = %subst(wVATNOT_E:1:2) +
042300100909     C                                      %trim(O284COD)
042400100901     C                   write     FIVAT000
042500100901     C                   add       1             §CTROKVT
042600100909     C                   endif
042700100901     C*
042800100901     C                   eval      wWhoIs = wWhoIs + 1
042900100901     C                   add       1             wCollo
043000100901     C                   enddo
043100100901     C*
043200060331     C                   endif
043300040119     C*
043400040119     C                   ENDSR
043500040119
043600040119
043700040119
043800040119     C*----------------------------------------------------*
043900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044000040119     C*----------------------------------------------------*
044100040119     C     PREVAT        BEGSR
044200040119     C*
044300040119     C* Compongo il nome del membro da dare al FIVATWWR
044400040119     C                   eval      parmbr = vlrhdl
044500040119     C                   movel     'M'           parmbr
044600040510     C                   eval      parccm = vlrksc
044700040119     C                   eval      paropz = '1'
044800040119     C* Effettuo la chiamata al CLLE preposto
044900040119     C                   call(e)   'TITVVTC'
045000040119     C                   parm                    parccm
045100040119     C                   parm                    parmbr
045200040119     C                   parm                    paropz
045300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045400040119     C                   if        %error
045500040119     C                   movel     '1'           chkcall
045600040119     C                   else
045700040119     C                   movel     '0'           chkcall
045800040119     C                   endif
045900040119     C*
046000040119     C                   ENDSR
046100010601
046200010601
046300010601
046400010330     C*----------------------------------------------------*
046500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046600010330     C*----------------------------------------------------*
046700040119     C     INZVAR        BEGSR
046800080527     C*
046900080527     C                   setoff                                       65
047000010330     C*
047100030822     C* Inizializzo variabili di wrk
047200020204     C                   Z-ADD     *zeros        Num5_0            5 0
047300060331     C                   MOVEL     *blanks       wVATNOT_A
047400060331     C                   MOVEL     *blanks       wVATNOT_B
047500060331     C                   MOVEL     *blanks       wVATNOT_E
047600100831     C                   MOVEL     *blanks       wVABRSD
047700100831     C                   MOVEL     *blanks       wVABIND
047800100831     C                   MOVEL     *blanks       wVABCAD
047900100831     C                   MOVEL     *blanks       wVABLOD
048000100831     C                   MOVEL     *blanks       wVABPRD
048100030822     C*
048200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048300030822     C                   CLEAR                   SkSplitCSV
048400030822     C                   EVAL      i = 1
048500030822     C                   EVAL      posDa = *zeros
048600030822     C                   EVAL      posA  = *zeros
048700080527     C*
048800080527     C                   CLEAR                   FIVABDS
048900080527     C                   CLEAR                   FIVAT000
049000030822     C*
049100030822     C* Reimposto i valori di default
049200030822     C                   EXSR      DEFCAM
049300030822     C*
049400010330     C                   ENDSR
049500010330     C*----------------------------------------------------*
049600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049700010330     C*----------------------------------------------------*
049800010330     C     DEFCAM        BEGSR
049900030822     C*
050000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050100030715     C* e delimitatore testo.
050200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050500100318     C*
050600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050700100318     C                   EVAL      CharSOS = CharNUM
050800030822     C*
050900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051000100720     C                   SETOFF                                       505152
051100100720     C                   SETOFF                                       53
051200100413     C                   SELECT
051300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051400100720     C                   SETON                                        50
051500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051600100413     C                   SETON                                        51
051700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051800100720     C                   SETON                                        52
051900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052000100720     C                   SETON                                        53
052100100413     C                   ENDSL
052200030822     C*
052300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052400030822     C                   EVAL      posDaDft = 1
052500030822     C                   EVAL      posADft  = 0
052600030822     C                   EVAL      wGiroDft = 0
052700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052800030822     C                             posDaDft > 0
052900030822     C*
053000030822     C* Gestisco il 1° giro
053100030822     C                   IF        wGiroDft = 0
053200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053400030822     C* Incremento il contatore dei "giri"
053500030822     C                   EVAL      wGiroDft = 1
053600030822     C                   ELSE
053700030822     C                   EVAL      posDaDft = posADft
053800030822     C                   ENDIF
053900030822     C* Eseguo lo scan x trovare la fine del campo corrente
054000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054100030822     C*
054200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054400030822     C                   IF        posDaDft > 0 AND
054500030822     C                             posADft  > 0
054600030822     C* NCL
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'NCL'
055100030822     C                   EVAL      PiStr=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   EXSR      CHKNUM
055500030822     C                   IF        PiInt=*on
055600030822     C                   Z-ADD     PiVal         VABNCL
055700030822     C                   ENDIF
055800030822     C                   ENDIF
055900030822     C* CCM
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'CCM'
056400030822     C                   EVAL      PiStr=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   EXSR      CHKNUM
056800030822     C                   IF        PiInt=*on
056900030822     C                   Z-ADD     PiVal         VABCCM
057000030822     C                   ENDIF
057100030822     C                   ENDIF
057200030822     C* LNP
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'LNP'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiInt=*on
058200030822     C                   Z-ADD     PiVal         VABLNP
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* NRS
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'NRS'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABNRS
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* CTR
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'CTR'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABCTR
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* PKB
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'PKB'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiNum=*on
062100030822     C                   Z-ADD     PiVal         VABPKB
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* VLB
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'VLB'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiNum=*on
063400030822     C                   Z-ADD     PiVal         VABVLB
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* QFT
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'QFT'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiNum=*on
064700030822     C                   Z-ADD     PiVal         VABQFT
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* CBO
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'CBO'
065500030822     C                   EVAL      VABCBO=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   ENDIF
065900030822     C* TSP
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'TSP'
066400030822     C                   EVAL      VABTSP=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   ENDIF
066800030822     C* VAS
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'VAS'
067300030822     C                   EVAL      VABVAS=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   ENDIF
067700030822     C* VCA
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'VCA'
068200030822     C                   EVAL      VABVCA=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   ENDIF
068600070925     C* TIC
068700070925     C                   IF        %subst(
068800070925     C                             %subst(vlrppt:posDaDft+1:
068900070925     C                             posADft-posDaDft-1):1:3)
069000070925     C                             = 'TIC'
069100070925     C                   EVAL      VABTIC=%trim(%subst(
069200070925     C                             %subst(vlrppt:posDaDft+1:
069300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400070925     C                   ENDIF
069500030822     C* GCA
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'GCA'
070000030822     C                   EVAL      VABGCA=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* CTM
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'CTM'
070900030822     C                   EVAL      VABCTM=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* FFD
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'FFD'
071800030822     C                   EVAL      VABFFD=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* VAD
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'VAD'
072700030822     C                   EVAL      VABVAD=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* GMA
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'GMA'
073600030822     C                   EVAL      VABGMA=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* GGA
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'GGA'
074500030822     C                   EVAL      VABGGA=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* GVA
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'GVA'
075400030822     C                   EVAL      VABGVA=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* TC1
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'TC1'
076300030822     C                   EVAL      VABTC1=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* TC2
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'TC2'
077200030822     C                   EVAL      VABTC2=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600040714     C* VATTRC
077700040714     C                   IF        %subst(
077800040714     C                             %subst(vlrppt:posDaDft+1:
077900040714     C                             posADft-posDaDft-1):1:3)
078000040714     C                             = 'TRC'
078100040714     C                   EVAL      VATTRC=%trim(%subst(
078200040714     C                             %subst(vlrppt:posDaDft+1:
078300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400040714     C                   ENDIF
078500030822     C* ...
078600030822     C                   ENDIF
078700030822     C                   ENDDO
078800020204     C*
078900010330     C                   ENDSR
079000010607     C*----------------------------------------------------*
079100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079200010607     C*----------------------------------------------------*
079300010607     C     IMPVAB        BEGSR
079400010607     C*
079500010607     C                   SETOFF                                       3132
079600030515     C*
079700010607     C                   MOVEL     datcor        VABAAS
079800010607     C                   MOVE      datcor        VABMGS
079900010607     C*
080000030715     C**********
080100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080200030715     C**********
080300030715     C                   IF        wGiro = *zeros
080400040113     C*
080500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080800080429     C                             %trim(intcol(5))<>*blanks
080900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081100080429     C                                      %trim(intcol(5))
081200070823     C     ';':CharCSV   XLATE     vindta        vindta
081300040510     C                   ENDIF
081400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081500031201     C     minu:maiu     XLATE     vindta        vindta
081600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081700030715     C                   DOW       posDa <= %len(%trim(vindta))
081800030715     C*
081900030715     C* Gestisco il 1° campo
082000030715     C                   IF        i = 1
082100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082200030715     C                   EVAL      posDa = 1
082300030715     C* Eseguo lo scan x trovare la fine del primo campo
082400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082500030715     C* A questo punto "estraggo" il campo corrente
082600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082700030715     C                                                    (posA-posDa))
082800030715     C* X i campi successivi al 1°
082900030715     C                   ELSE
083000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083100030715     C                   EVAL      posDa = posA + 1
083200030715     C* Eseguo lo scan x trovare la fine del campo corrente
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* Gestisco l'ultimo campo
083500030715     C                   IF        posA = *zeros
083600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   LEAVE
084100030715     C                   ELSE
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   ENDIF
084600030715     C                   ENDIF
084700030715     C* Incremento il contatore d campo
084800030715     C                   EVAL      i = i +1
084900030715     C                   ENDDO
085000030715     C*
085100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085200030715     C* delimitazione testo
085300030715     C                   EVAL      i = 1
085400030715     C                   DOW       i < %elem(SkSplitFLD)
085500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085700030715     C                   EVAL      i = i + 1
085800030715     C                   ENDDO
085900030715     C                   EVAL      wGiro = 1
086000030715     C                   ELSE
086100030715     C                   EVAL      wGiro = 2
086200031204     C**********
086300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086400031204     C* d separatore campo
086500031204     C**********
086600031204     C                   EVAL      vindta = %trim(vindta)
086700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086800031204     C                   EVAL      vindta = %subst(vindta:2)
086900031204     C                   ENDDO
087000031204     C*
087100031204     C                   Z-ADD     *zeros        lunghInput        4 0
087200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087600031204     C                   ENDDO
087700030715     C**********
087800030715     C* Effettuo lo split del campo dati d input
087900030715     C**********
088000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088100030715     C                   DOW       posDa <= %len(%trim(vindta))
088200030715     C*
088300030715     C* Gestisco il 1° campo
088400030715     C                   IF        i = 1
088500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088600030715     C                   EVAL      posDa = 1
088700030715     C* Eseguo lo scan x trovare la fine del primo campo
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C* X i campi successivi al 1°
089300030715     C                   ELSE
089400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089500030715     C                   EVAL      posDa = posA + 1
089600031204     C* Verifico che nn vi sia il campo nn valorizzato
089700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089800031204     C* Se campo nn valorizzato skippo al prossimo
089900031204     C                   EVAL      SkSplitCSV(i) = *blanks
090000031204     C                   EVAL      posA  = posA + 1
090100031204     C                   ELSE
090200030715     C* Eseguo lo scan x trovare la fine del campo corrente
090300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090400030715     C* Gestisco l'ultimo campo
090500030715     C                   IF        posA = *zeros
090600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C                   LEAVE
091100030715     C                   ELSE
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C                   ENDIF
091600031204     C                   ENDIF
091700030715     C                   ENDIF
091800030715     C* Incremento il contatore d campo
091900030715     C                   EVAL      i = i +1
092000030715     C                   ENDDO
092100030715     C*
092200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092300030715     C* delimitazione testo
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitCSV)
092600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092800030715     C                   EVAL      i = i + 1
092900030715     C                   ENDDO
093000100720     C*
093100100720     C* Stacco un progressivo x numero spedizione
093200100720 xxx C***                EXSR      repNSP
093300100326     C*
093400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093500030715     C                   EVAL      i = 1
093600030715     C                   DOW       i < %elem(SkSplitFLD)
093700030715     C*
093800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094000030715     C                             SkSplitCSV(i) <> *blanks
094100030721     C*** ATB
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094300030721     C                   EVAL      VABATB = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030715     C*** RSD
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095000030715     C* ==
095100030715     C                   ENDIF
095200030721     C*** RD2
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030715     C*** IND
095700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095800030715     C                   EVAL      VABIND = SkSplitCSV(i)
095900030715     C                   ENDIF
096000030715     C*** LOD
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096300030715     C                   ENDIF
096400030715     C*** PRD
096500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096700030715     C                   ENDIF
096800030721     C*** NZD
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030715     C*** RMA
097300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097500030715     C                   ENDIF
097600030715     C*** NOT
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** NT2
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** VCA
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** VAS
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** FFD
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030721     C*** GC1
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** GC2
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** TSP
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100700030721     C                   ENDIF
100800040510     C*** CBO
100900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101100040510     C                   ENDIF
101200030721     C*** NAS
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** TIC
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** GCA
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** XCO
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** CTM
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** TCR
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** CTS
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** FTM
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** VAD
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** GMA
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** GGA
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** GVA
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** TC1
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TC2
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** SCL
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** RMO
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** NMO
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*
108100030721     C* Reperisco quindi i campi numerici...
108200040510     C*** NSP
108300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108400040510     C                   EVAL      PiStr=SkSplitCSV(i)
108500040510     C                   EXSR      CHKNUM
108600040510     C                   IF        PiInt=*on
108700040510     C                   Z-ADD     PiVal         VABNSP
108800040510     C                   ELSE
108900040510     C                   SETON                                        31
109000040510     C                   EVAL      VABNSP = *zeros
109100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109200040510     C                             + ' ' + 'VABNSP'
109300040510     C                   ENDIF
109400040510     C                   ENDIF
109500071106     C*** RMN
109600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109700071106     C                   EVAL      PiStr=SkSplitCSV(i)
109800071106     C                   EXSR      CHKNUM
109900071106     C                   IF        PiInt=*on
110000071106     C                   Z-ADD     PiVal         VABRMN
110100071106     C                   ELSE
110200071106     C                   SETON                                        32
110300071106     C                   EVAL      VABRMN = *zeros
110400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110500071106     C                             + ' ' + 'VABRMN'
110600071106     C                   ENDIF
110700071106     C                   ENDIF
110800030721     C*** CAD
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiInt=*on
111300030721     C                   Z-ADD     PiVal         Num5_0
111400030721     C                   MOVEL(p)  Num5_0        VABCAD
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      VABCAD = *zeros
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABCAD'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** NCL
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on
112700030721     C                   Z-ADD     PiVal         VABNCL
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000030721     C                   EVAL      VABNCL = *zeros
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200040122     C                             + ' ' + 'VABNCL'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** PKB
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiNum=*on
114000030721     C                   Z-ADD     PiVal         VABPKB
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      VABPKB = *zeros
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABPKB'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** CAS
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiNum=*on
115300030721     C                   Z-ADD     PiVal         VABCAS
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      VABCAS = *zeros
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABCAS'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** CCM
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on      AND
116600030721     C                             PiVal<=9999999 AND
116700030721     C                             PiVal>*zeros
116800030721     C                   Z-ADD     PiVal         VABCCM
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      VABCCM = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300040510     C                             + ' ' + 'VABCCM'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** LNP
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on    AND
118100030721     C                             PiVal<=999   AND
118200030721     C                             PiVal>*zeros
118300030721     C                   Z-ADD     PiVal         VABLNP
118400030721     C                   ELSE
118500040715     C                   SETON                                        32
118600030721     C                   EVAL      VABLNP = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800040510     C                             + ' ' + 'VABLNP'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** NRS
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on AND
119600030721     C                             PiVal<=99
119700030721     C                   Z-ADD     PiVal         VABNRS
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABNRS'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** LNA
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on    AND
120900030721     C                             PiVal<=999   AND
121000030721     C                             PiVal>*zeros
121100030721     C                   Z-ADD     PiVal         VABLNA
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      VABLNA = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABLNA'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** CTR
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on  AND
122400030721     C                             PiVal<=999
122500030721     C                   Z-ADD     PiVal         VABCTR
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABCTR'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** IAS
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiNum=*on
123700030721     C                   Z-ADD     PiVal         VABIAS
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABIAS'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** VLB
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiNum=*on
124900030721     C                   Z-ADD     PiVal         VABVLB
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300030721     C                             + ' ' + 'VABVLB'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** QFT
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiNum=*on
126100030721     C                   Z-ADD     PiVal         VABQFT
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABQFT'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** NCD
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiInt=*on      AND
127300030721     C                             PiVal<=9999999
127400030721     C                   Z-ADD     PiVal         VABNCD
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABNCD'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** NCA
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on      AND
128600030721     C                             PiVal<=9999999
128700030721     C                   Z-ADD     PiVal         VABNCA
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABNCA'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** ZNC
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on AND
129900030721     C                             PiVal<=99
130000030721     C                   Z-ADD     PiVal         VABZNC
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABZNC'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** DCR
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on       AND
131200030721     C                             PiVal<=99999999
131300030721     C                   Z-ADD     PiVal         VABDCR
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABDCR'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** HCR
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on   AND
132500030721     C                             PiVal<=9999
132600030721     C                   Z-ADD     PiVal         VABHCR
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABHCR'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** VMD
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiNum=*on
133800030721     C                   Z-ADD     PiVal         VABVMD
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABVMD'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** ANT
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on        AND
135000030721     C                             PiVal<=999999999
135100030721     C                   Z-ADD     PiVal         VABANT
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABANT'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** CMO
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on
136300030721     C                   Z-ADD     PiVal         Num5_0
136400030721     C                   MOVEL(p)  Num5_0        VABCMO
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      VABCMO = *zeros
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABCMO'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200040510     C*
137300040510     C* Reperisco i campi del FIVAT
137400040510     C*** VATNOT
137500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137700040510     C                   ENDIF
137800040510     C*** VATTRC
137900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138100040510     C                   ENDIF
138200060331     C*** VATNOT_A
138300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138500060331     C                   ENDIF
138600060331     C*** VATNOT_B
138700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138900060331     C                   ENDIF
139000060331     C*** VATNOT_E
139100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139300060331     C                   ENDIF
139400070823     C*
139500100831     C***  ===>  Gestione campi "particolari" dentro ciclo
139600100720     C*** RMN / NSP
139700100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
139800100413     C                   EVAL      PiStr=SkSplitCSV(i)
139900100413     C                   EXSR      CHKNUM
140000100413     C                   IF        PiInt=*on
140100100419     C                   Z-ADD     PiVal         VABNSP
140200100720     C                   Z-ADD     PiVal         VABRMN
140300100413     C                   ELSE
140400100720     C                   Z-ADD     *zeros        VABRMN
140500100413     C                   SETON                                        32
140600100413     C                   EVAL      vinmsg = %trimr(vinmsg)
140700100720     C                             + ' ' + 'VABNSP VABRMN'
140800100413     C                   ENDIF
140900100326     C                   ENDIF
141000100831     C*** RSD
141100100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141200100831     C                   EVAL      wVABRSD = %trim(SkSplitCSV(i))
141300100831     C                   ENDIF
141400100831     C*** IND
141500100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND'
141600100831     C                   EVAL      wVABIND = %trim(SkSplitCSV(i))
141700100831     C                   ENDIF
141800100831     C*** CAD
141900100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
142000100831     C                   EVAL      wVABCAD = %trim(SkSplitCSV(i))
142100100831     C                   ENDIF
142200100831     C*** LOD
142300100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD'
142400100831     C                   EVAL      wVABLOD = %trim(SkSplitCSV(i))
142500100831     C                   ENDIF
142600100831     C*** PRD
142700100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPRD'
142800100831     C                   EVAL      wVABPRD = %trim(SkSplitCSV(i))
142900100831     C                   ENDIF
143000100831     C*** CBO
143100100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
143200100831     C     minu:maiu     XLATE     SkSplitCSV(i) SkSplitCSV(i)
143300100831     C                   IF        %scan('ASSEGNATO':SkSplitCSV(i)) > 0
143400100831     C                   EVAL      VABCBO = '2'
143500100831     C                   ENDIF
143600100831     C                   ENDIF
143700070925     C*
143800071029     C***  <===  -----------------------------------------
143900040510     C*
144000030715     C                   ENDIF
144100030715     C                   EVAL      i = i + 1
144200030715     C                   ENDDO
144300070925     C*
144400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144500100831     C*
144600100831     C                   IF        wVABRSD <> *blanks
144700100831     C                   EVAL      VABRSD = wVABRSD
144800100831     C                   ENDIF
144900100831     C*
145000100831     C                   IF        wVABIND <> *blanks
145100100831     C                   EVAL      VABIND = wVABIND
145200100831     C                   ENDIF
145300100831     C*
145400100831     C                   IF        wVABCAD <> *blanks
145500100831     C                   EVAL      VABCAD = wVABCAD
145600100831     C                   ENDIF
145700100831     C*
145800100831     C                   IF        wVABLOD <> *blanks
145900100831     C                   EVAL      VABLOD = wVABLOD
146000100831     C                   ENDIF
146100100831     C*
146200100831     C                   IF        wVABPRD <> *blanks
146300100831     C                   EVAL      VABPRD = wVABPRD
146400100831     C                   ENDIF
146500070925     C*
146600071029     C***  <===  -----------------------------------------
146700070925     C*
146800070925     C* Considerazioni finali su CBO/CAS
146900100318     C                   IF        wFlgCAS = '1'    OR
147000100318     C                             VABCAS  > *zeros
147100070925     C                   IF        VABCBO = '1'
147200070925     C                   EVAL      VABCBO = '4'
147300070925     C                   ENDIF
147400070925     C                   IF        VABCBO = '2'
147500070925     C                   EVAL      VABCBO = '6'
147600070925     C                   ENDIF
147700070925     C                   ENDIF
147800070925     C***  <===  ----------------------------
147900040510     C*
148000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148100040510     C                   IF        VABNSP = *zeros
148200040510     C                   SETON                                        31
148300040510     C                   ENDIF
148400040715     C*
148500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148600040715     C                   IF        VABLNP = *zeros
148700040715     C                   SETON                                        31
148800040715     C                   ENDIF
148900040510     C*
149000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149100040510     C                   IF        VABRMN = *zeros
149200040510     C                   Z-ADD     VABNSP        VABRMN
149300040510     C                   ENDIF
149400040510     C*
149500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149600040510     C                   IF        VABRMA = *blanks
149700040714     C                   MOVEL     VABRMN        VABRMA
149800040510     C                   ENDIF
149900040128     C*
150000040510     C* Se provincia nn valorizzata la reperisco
150100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150200040510     C                   IF        VABPRD = *blanks
150300040128     C                   CLEAR                   TISI95DS
150400040128     C                   EVAL      I95TCN = '3'
150500040128     C                   Z-ADD     datcor        I95DAT
150600040128     C                   EVAL      I95NAR = VABNZD
150700040128     C                   EVAL      I95CAP = VABCAD
150800040128     C                   EVAL      I95LOC = VABLOD
150900040128     C                   CALL      'TISI95R'
151000040128     C                   PARM                    TISI95DS
151100040128     C                   EVAL      VABPRD = O95PRV
151200040510     C                   ENDIF
151300020204     C*
151400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151500020204     C                   EXSR      CHKIMPDIV
151600030715     C                   ENDIF
151700020204     C*
151800010607     C                   ENDSR
151900010607     C*----------------------------------------------------*
152000010601
152100020204
152200020204     C*----------------------------------------------------*
152300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152400020204     C*----------------------------------------------------*
152500020204     C     CHKIMPDIV     BEGSR
152600020204     C*
152700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152800020204     C                   Z-ADD     *zeros        wrkDec            9 9
152900020204     C*
153000020204     C* Come prima cosa effettuo considerazioni sulla divisa
153100020204     C                   IF        vabIAS > *zeros
153200160413     C                   IF        vabVAS =  *blanks
153300160413     C                   EVAL      vabVAS =  'EUR'
153400020204     C                   ENDIF
153500020204     C                   ENDIF
153600020204     C*
153700020204     C                   IF        vabCAS > *zeros
153800160413     C                   IF        vabVCA =  *blanks
153900160413     C                   EVAL      vabVCA =  'EUR'
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C                   IF        vabVMD > *zeros
154400160413     C                   IF        vabVAD =  *blanks
154500160413     C                   EVAL      vabVAD =  'EUR'
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155000020204     C                   Z-ADD     vabIAS        wrkDec
155100020204     C                   IF        wrkDec > *zeros
155200020204     C                   IF        vabVAS = 'ITL'
155300020204     C                   EVAL      vabIAS = *zeros
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155800020204     C                   Z-ADD     vabCAS        wrkDec
155900020204     C                   IF        wrkDec > *zeros
156000020204     C                   IF        vabVCA = 'ITL'
156100020204     C                   EVAL      vabCAS = *zeros
156200020204     C                   ENDIF
156300020204     C                   ENDIF
156400020204     C*
156500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156600020204     C                   Z-ADD     vabVMD        wrkDec
156700020204     C                   IF        wrkDec > *zeros
156800020204     C                   IF        vabVAD = 'ITL'
156900020204     C                   EVAL      vabVMD = *zeros
157000020204     C                   ENDIF
157100020204     C                   ENDIF
157200020204     C*
157300020204     C                   ENDSR
157400020204     C***
157500010330
157600100318
157700100318
157800100318     C*----------------------------------------------------*
157900100318     C*  CONTROLLO NUMERICITA' CAMPI
158000100318     C*----------------------------------------------------*
158100100318     C     CHKNUM        BEGSR
158200100318     C*
158300100318     C                   IF        PiDecChr = *blanks
158400100318     C                   EVAL      PiDecChr = CharNUM
158500100318     C                   ENDIF
158600100318     C*
158700100318     C                   callp     UBISNUM_Check(PiStr
158800100318     C                                          :PiDecChr
158900100318     C                                          :PiVal
159000100318     C                                          :PiNum
159100100318     C                                          :PiInt)
159200100318     C*
159300100318     C                   ENDSR
159400100318     C***
159500010601
159600010601
159700010601
159800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159900010601     C     repfil        BEGSR
160000010601     C*
160100010601     C                   if        invfil = *zeros and
160200010601     C                             depfil > *zeros and
160300010629     C                             (vinflg = *blanks or
160400010629     C                              vinflg = *zeros)
160500010601     C
160600010601     C                   eval      invfil = depfil
160700010601     C                   endif
160800010601     C*
160900010601     C                   if        depfil <> invfil and
161000010601     C                             invfil > *zeros
161100010601     C                   eval      flgMulti = '1'
161200010601     C                   if        vinflg = *blanks
161300010601     C                   add       1             cntNonEl
161400010601     C                   endif
161500010601     C                   endif
161600010601     C*
161700010601     C                   if        vinflg = '2'
161800010601     C                   eval      flgStato = '2'
161900010601     C                   endif
162000010601     C*
162100010601     C                   ENDSR
162200010601     C***
162300010601
162400010601
162500010601
162600010330
162700040119      /TITLE Invio dei dati al punto operativo.
162800040119     C     invio         BEGSR
162900040119     C*
163000040119     C* 1° invio FIVAT
163100040119     C                   reset                   dscmz
163200040119     C                   move      vlrpoi        cmzdst
163300040119     C                   eval      cmzfld = 'FIVATWWR'
163400040119     C                   eval      cmzmbd = vlrhdl
163500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163600040119     C***                if        prmfir = *blanks
163700040119     C                   eval      cmzfla = 'FIVAT00F'
163800040119     C                   eval      cmzmba = 'FIVAT00F'
163900040119     C***                else
164000040119     C***                eval      cmzfla = prmfir
164100040119     C***                eval      cmzmba = prmfir
164200040119     C***                endif
164300040119     C                   eval      cmznrr = *zeros
164400040119     C                   move      §ctrokvt      cmznrr
164500040119     C                   eval      cmzlba = vlrfl1
164600040119     C                   call(e)   'TIS711C'
164700040119     C                   parm                    dscmz
164800040119     C                   parm      *blanks       esito
164900040119     C                   if        %error
165000040119     C                             or cmzerr = '1'
165100040119     C                             or esito  = '1'
165200040119     C                   eval      wrkesito = '3'
165300040119     C                   else
165400040119     C*
165500040119     C* 2° invio FIVAB
165600040119     C                   reset                   dscmz
165700040119     C                   move      vlrpoi        cmzdst
165800040119     C                   eval      cmzfld = vlrfou
165900040119     C                   eval      cmzmbd = vlrhdl
166000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166100040119     C***                if        prmfir = *blanks
166200040119     C                   eval      cmzfla = 'FIVAB00F'
166300040119     C                   eval      cmzmba = 'FIVAB00F'
166400040119     C***                else
166500040119     C***                eval      cmzfla = prmfir
166600040119     C***                eval      cmzmba = prmfir
166700040119     C***                endif
166800040119     C                   eval      cmznrr = *zeros
166900040119     C                   move      §ctrokvb      cmznrr
167000040119     C                   eval      cmzlba = vlrfl1
167100040119     C                   call(e)   'TIS711C'
167200040119     C                   parm                    dscmz
167300040119     C                   parm      *blanks       esito
167400040119     C                   if        %error
167500040119     C                             or cmzerr = '1'
167600040119     C                             or esito  = '1'
167700040119     C                   eval      wrkesito = '3'
167800040119     C                   endif
167900040119     C                   endif
168000040119     C*
168100040119     C                   ENDSR
168200040119     C***
168300010601
168400010601
168500010601
168600010601
168700010601      /TITLE Invio dei dati al punto operativo.
168800010601     C     opeini        BEGSR
168900010601     C*
169000010601     C* Inizializzo flag e contatori operativi
169100010601     C                   movel     '0'           flgGiro           1
169200010601     C                   movel     '0'           flgMulti          1
169300010601     C                   movel     '1'           flgStato          1
169400010615     C                   movel     '0'           flgOk             1
169500010601     C                   z-add     *zeros        cntNonEl         10 0
169600010601     C                   z-add     *zeros        depfil            3 0
169700010601     C                   z-add     *zeros        invfil            3 0
169800010601     C*
169900010601     C                   ENDSR
170000010601     C***
170100070326
170200070326
170300070326
170400070326
170500070326     C     *pssr         BEGSR
170600070329     C*
170700070329     C                   if        %open(tivin00r)
170800070329     C                   close     tivin00r
170900070329     C                   endif
171000070329     C                   if        %open(fivabwwr)
171100070329     C                   close     fivabwwr
171200070329     C                   endif
171300070329     C                   if        %open(fivatwwr)
171400070329     C                   close     fivatwwr
171500070329     C                   endif
171600070326     C*
171700070326     C* Effettuo la chiamata al CLLE preposto
171800070326     C                   call(e)   'TITVVTC'
171900070326     C                   parm                    parccm
172000070326     C                   parm                    parmbr
172100070326     C                   parm      '2'           paropz
172200070326     C*
172300070326     C                   eval      wrkesito = '2'
172400070404     C*
172500070404     C                   seton                                        LR
172600070326     C*
172700070326     C                   ENDSR     '*CANCL'
172800070326     C***
172900100318
173000100318
173100100318
173200100318     C     normCSV       BEGSR
173300100318     C*
173400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173500100318     C* separatori campi
173600100318     C                   if        CharTXT <> *blanks
173700100318     C                   z-add     *zeros        wLenVINDTA        4 0
173800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173900100318     C                   setoff                                       80
174000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174300100318     C                   if        not *in80
174400100318     C                   seton                                        80
174500100318     C                   else
174600100318     C                   setoff                                       80
174700100318     C                   endif
174800100318     C                   endif
174900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
175000100318     C                             and *in80 = *on
175100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175200100318     C                   endif
175300100318     C                   enddo
175400100318     C                   endif
175500100318     C*
175600100318     C                   ENDSR
175700100318     C***
175800070326
175900100326
176000100326
176100100326     C*----------------------------------------------------*
176200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176300100326     C*----------------------------------------------------*
176400100326     C     REPNSP        BEGSR
176500100326     C*
176600100326     C* NSP => Stacco un numeratore da AZNUM
176700100326     C                   clear                   TRUL33DS
176800100326     C                   eval      I33OPE = *zeros
176900100326     C                   eval      I33CNU = 302
177000100326     C                   eval      I33NUM = 1
177100100326     C                   movel     TRUL33DS      KPJBU
177200100326     C                   call      'TRUL33R'
177300100326     C                   parm                    KPJBA
177400100326     C                   movel     KPJBU         TRUL33DS
177500100326     C                   if        O33ERR = *zeros
177600100326     C                   z-add     O33NRF        VABNSP
177700100326     C                   z-add     O33NRF        VATNSP
177800100326     C                   else
177900100326     C                   SETON                                        31
178000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178100100326     C                             + ' ' + 'VABNSP VATNSP'
178200100326     C                   endif
178300100326     C*
178400100326     C                   ENDSR
178500010330
178600100326
178700010330
178800000613     C     *inzsr        BEGSR
178900990910     C*
179000990910     C     *entry        plist
179100990920     C                   parm                    tivlrds
179200990921     C                   parm      wrkesito      esito
179300000724     C                   parm                    prmlit
179400000710     C                   parm                    prmfir
179500010330     C*
179600010330     C* CALCOLA LA DATA CORRENTE
179700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179800100318     C                   eval      datcor = %dec(%date() : *ISO)
179900000613     C*
180000000613     C                   ENDSR
180100000613     C***
180200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180300100831FIL001;FIL002;W_VABRMN;FIL003;FIL004;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;
180400100831W_VABRSD;W_VABIND;W_VABCAD;W_VABLOD;W_VABPRD;FIL005;FIL006;W_VABCBO;
180500100831VATNOT_A;VABNCL;VABPKB;FIL007;VABRMA;VABNOT;VATNOT_E;VABTSP;VABFFD;
180600160413VABTC1;VABCTR;VABCAS;VABTIC;VABIAS;
180700100318
