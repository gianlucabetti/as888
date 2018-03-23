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
005900150512     D wVATNOT_S       s                   LIKE(VATNOT)
006000100831     D wVABRSD         s                   LIKE(VABRSD)
006100100831     D wVABIND         s                   LIKE(VABIND)
006200100831     D wVABCAD         s                   LIKE(VABCAD)
006300100831     D wVABLOD         s                   LIKE(VABLOD)
006400100831     D wVABPRD         s                   LIKE(VABPRD)
006500030822     D*------------------
006600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006700030822     D*------------------
006800030822     D posDaDft        S              3  0 INZ(*zeros)
006900030822     D posADft         S              3  0 INZ(*zeros)
007000030822     D j               s              3  0 INZ(1)
007100030822     D wGiroDft        s              1  0 INZ(*zeros)
007200080418     D*------------------
007300080418     D* Costanti
007400080418     D*------------------
007500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007700100318
007800100318     D*------------------
007900100318     D* LINKING A DEFINIZIONI ESTERNE
008000100318     D*------------------
008100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008300100318
008400100909     D*------------------
008500100909     D* DS CALCOLO CHECK-DIGITS
008600100909     D*------------------
008700100909     D TRUL28DSX     e ds                  inz
008800010330
008900010330
009000010330
009100000913     C                   reset                   rrnum
009200990921     C                   reset                   esito
009300990921     C                   reset                   wrkesito
009400010601     C*
009500010601     C                   exsr      opeini
009600010605     C                   exsr      rwvab
009700010601     C*
009800010601     C                   seton                                        lr
009900010601
010000010601
010100010601
010200010601     C*--------------------------------------------------------
010300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010400010601     C*--------------------------------------------------------
010500010601     C     PREELA        BEGSR
010600010601     C*
010700010601     C* SE OCCORRE SPEDIRE IN FILIALE
010800010601     C                   if        invfil <> *zeros and
010900010601     C                             flgGiro = '0'
011000010601     C*
011100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011200010601     C                   eval      flgGiro = '1'
011300010601     C*
011400010601     C                   endif
011500010601     C*
011600010601     C                   ENDSR
011700010601     C***
011800010601
011900010601
012000010601
012100010601
012200010601     C*--------------------------------------------------------
012300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012400010601     C*--------------------------------------------------------
012500010601     C     ENDELA        BEGSR
012600040119     C*
012700040119     C* Effettuo la chiamata al CLLE preposto
012800040119     C                   call(e)   'TITVVTC'
012900040510     C                   parm                    parccm
013000040119     C                   parm                    parmbr
013100040119     C                   parm      '2'           paropz
013200050201     C*
013300050201     C* Effettuo lancio TISI95 solo x chiusura
013400050201     C                   CLEAR                   TISI95DS
013500050201     C                   EVAL      I95TLA = 'C'
013600050201     C                   CALL      'TISI95R'
013700050201     C                   PARM                    TISI95DS
013800000616     C*
013900010601     C                   ENDSR
014000010601     C***
014100000613
014200010601
014300010601
014400010330     C*--------------------------------------------------------
014500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014600010330     C*--------------------------------------------------------
014700010605     C     RWVAB         BEGSR
014800010330     C*
014900010330     C                   if        not %open(tivin00r)
015000010330     C                   open      tivin00r
015100010330     C                   endif
015200021025     C                   if        not %open(fivabwwr)
015300021025     C                   open      fivabwwr
015400010330     C                   endif
015500040119     C*
015600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015700040119     C                   exsr      prevat
015800040119     C*
015900040119     C                   if        chkcall = '0'
016000040119     C*
016100040119     C                   if        not %open(fivatwwr)
016200040119     C                   open      fivatwwr
016300040119     C                   endif
016400010330     C*
016500040119     C                   clear                   §CTROKVB          7 0
016600040119     C                   clear                   §CTROKVT          7 0
016700040119     C                   clear                   §CTRMO            7 0
016800040119     C                   clear                   §CTRNO            7 0
016900070823     C*
017000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017200100318     C***                exsr      inzvar
017300100318     C***                exsr      impvab
017400010330     C*
017500030822     C                   do        *HIVAL
017600010330     C*
017700030822     C                   read      tivin00r                               70
017800010618     C*
017900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018000010618     C                   if        vinflg = '1'
018100010618     C                   eval      flgOk = '1'
018200010618     C                   endif
018300040510     C*
018400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018500040510     C                   z-add     *zeros        wLenVINDTA        4 0
018600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019000040510     C                   z-add     1             wFlgVINDTA
019100040510     C                   leave
019200040510     C                   endif
019300040510     C                   enddo
019400010618     C*
019500040510     C                   if        vindta > *blanks AND
019600040510     C                             wFlgVINDTA = 1
019700010330     C                   add       1             rrnum
019800010330     C*
019900010601     C                   if        *in70 = *off and
020000010330     C                             (vinflg = *blanks
020100010330     C                              or vinflg = '0'
020200010330     C                              or vinflg = '2')
020300010330     C*
020400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020500010711     C                   if        vinflg = *blanks or vinflg = '0'
020600010711     C                   clear                   vinmsg
020700010711     C                   endif
020800010601     C*
020900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021000070925     C                   eval      vindta  = ' ' + %trim(vindta)
021100070925     C*
021200080527     C                   exsr      inzvar
021300100318     C                   exsr      normCSV
021400010605     C                   exsr      impvab
021500010601     C*
021600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021700010605     C                   eval      depfil = VABLNP
021800010601     C                   exsr      repfil
021900010601     C                   if        depfil = invfil
022000021025     C                   if        vlrpoi = 999
022100030822     C                   move(P)   invfil        VABFGS
022200021025     C                   else
022300030822     C                   move(P)   vlrpoi        VABFGS
022400021025     C                   endif
022500010601     C*
022600010601     C                   exsr      PREELA
022700010601     C*
022800010604     C* Ebbene...
022900010604     C*
023000040119     C  N31              add       1             §CTROKVB
023100040119     C   32              add       1             §CTRMO
023200040119     C   31              add       1             §CTRNO
023300080418     C*
023400030822     C                   if        wGiro = 2
023500080513     C* Scrivo sempre il VAT
023600080513     C  N31              exsr      wrivat
023700080527     C                   exsr      chkvab
023800080513     C                   endif
023900010604     C*
024000010604     C                   if        *in31 = *off and
024100010604     C                             *in32 = *off
024200010604     C                   eval      vinflg = '1'
024300010604     C                   else
024400010604     C                   eval      vinflg = '2'
024500010604     C                   endif
024600010604     C                   endif
024700010601     C*
024800010604     C                   endif
024900010604     C*
025000010330     C                   else
025100010330     C                   eval      vinflg = '1'
025200010330     C                   endif
025300010601     C*
025400010601     C  N70              update    tivin000
025500010330     C*
025600030822     C  N70              enddo
025700040119     C*
025800080527     C* Verifico dati rimasti "in cannna"...
025900080527     C                   seton                                        65
026000080527     C                   exsr      chkvab
026100080527     C*
026200040119     C                   endif                                                  (endif - chkcall)
026300010601     C*
026400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026500010601     C                   if        cntNonEl = *zeros or
026600010601     C                             flgMulti = '0'
026700010330     C* Se non ci sono record con errori ...
026800010601     C                   if        §ctrno = 0 and
026900010604     C                             §ctrmo = 0 and
027000010601     C                             flgStato <> '2'
027100010330     C* ... restituisco esito OK.
027200010330     C                   eval      wrkesito = '0'
027300010330     C                   else
027400040119     C                   if        §ctrokvb > 0
027500010330     C                   eval      wrkesito = '1'
027600010330     C                   else
027700010615     C                   if        flgOk = '0'
027800010615     C                   eval      wrkesito = '2'
027900010615     C                   else
028000010615     C                   eval      wrkesito = '6'
028100010615     C                   endif
028200010330     C                   endif
028300010330     C                   endif
028400010601     C                   else
028500010601     C                   eval      wrkesito = '9'
028600010601     C                   endif
028700010330     C*
028800010330     C                   if        %open(tivin00r)
028900010330     C                   close     tivin00r
029000010330     C                   endif
029100021025     C                   if        %open(fivabwwr)
029200021025     C                   close     fivabwwr
029300010330     C                   endif
029400040512     C                   if        %open(fivatwwr)
029500040512     C                   close     fivatwwr
029600040512     C                   endif
029700010601     C*
029800010601     C                   if        vlrpoi <> 999
029900010601     C                   eval      invfil = vlrpoi
030000010601     C                   endif
030100010330     C*
030200040119     C                   if        §ctrokvb > 0
030300010601     C                             and invfil > *zeros
030400010330     C                   exsr      invio
030500010330     C                   endif
030600010601     C*
030700010618     C                   if        flgGiro = '1'
030800010601     C                   exsr      endela
030900010618     C                   endif
031000010330     C*
031100010330     C                   ENDSR
031200010330     C***
031300080527
031400080527
031500080527
031600080527     C*----------------------------------------------------*
031700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031800080527     C*----------------------------------------------------*
031900100413     C     CHKVAB        BEGSR
032000080527     C*
032100080527     C* Scrivo il VAB solo a rottura d spedizione
032200080527     C                   if        depspe = *zeros
032300080527     C  N31              eval      fivabds_sav = fivabds
032400080527     C                   eval      depspe = vabNSP
032500080527     C                   else
032600080527     C                   if        depspe = vabNSP AND *in65 = *off
032700100413     C   51              add       VABNCL        sav_VABNCL
032800100413     C   51              add       VABPKB        sav_VABPKB
032900100413     C   51              add       VABVLB        sav_VABVLB
033000100720     C   52              z-add     VABNCL        sav_VABNCL
033100100720     C   52              z-add     VABPKB        sav_VABPKB
033200100720     C   52              z-add     VABVLB        sav_VABVLB
033300100720     C   53              add       VABNCL        sav_VABNCL
033400100720     C   53              z-add     VABPKB        sav_VABPKB
033500100720     C   53              z-add     VABVLB        sav_VABVLB
033600080527     C                   else
033700080527     C  N31              eval      fivabds_ok = fivabds_sav
033800080527     C  N31              exsr      wrivab
033900080527     C  N31              eval      fivabds_sav = fivabds
034000080527     C                   eval      depspe = vabNSP
034100080527     C                   endif
034200080527     C                   endif
034300080527     C*
034400080527     C                   ENDSR
034500040119
034600040119
034700040119
034800040119     C*----------------------------------------------------*
034900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035000040119     C*----------------------------------------------------*
035100040510     C     WRIVAB        BEGSR
035200080513     C*
035300080513     C* Gestisco l'eventuale rottura x numero spedizione
035400080527     C                   if        VABNSP <> depspe or *in65 = *on
035500070301     C                   if        wVATNOT_E <> *blanks
035600070301     C                   eval      VABCTM = '7Q'
035700070301     C                   endif
035800100413     C                   write     fivab000
035900080513     C                   endif
036000040119     C*
036100040119     C                   ENDSR
036200040119
036300040119
036400040119
036500040119     C*----------------------------------------------------*
036600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036700040119     C*----------------------------------------------------*
036800040510     C     WRIVAT        BEGSR
036900060512     C*
037000060512     C* Inizializzo i campi chiave
037100060512     C                   eval      VATFGS = VABFGS
037200060512     C                   eval      VATAAS = VABAAS
037300060512     C                   eval      VATCCM = VABCCM
037400070823     C                   eval      VATNRS = VABNRS
037500060512     C                   eval      VATNSP = VABNSP
037600060512     C                   eval      VATLNP = VABLNP
037700040714     C*
037800040714     C* Solo se VATNOT valorizzato scarico il buffer
037900040714     C                   if        VATNOT <> *blanks
038000040119     C*
038100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038200040510     C                   if        VATTRC = *blanks
038300040510     C                   eval      VATTRC = 'E'
038400040510     C                   endif
038500040510     C                   write     FIVAT000
038600040119     C                   add       1             §CTROKVT
038700040714     C*
038800040714     C                   endif
038900060331     C*
039000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039100060331     C                   if        wVATNOT_A <> *blanks
039200060331     C                   eval      VATNOT = wVATNOT_A
039300060331     C                   eval      VATTRC = 'A'
039400060331     C                   write     FIVAT000
039500060331     C                   add       1             §CTROKVT
039600060331     C                   endif
039700060331     C*
039800060331     C                   if        wVATNOT_B <> *blanks
039900060331     C                   eval      VATNOT = wVATNOT_B
040000060331     C                   eval      VATTRC = 'B'
040100060331     C                   write     FIVAT000
040200060331     C                   add       1             §CTROKVT
040300060331     C                   endif
040400150512     C*
040500150512     C                   if        wVATNOT_S <> *blanks
040600150512     C                   eval      VATNOT = wVATNOT_S
040700150512     C                   eval      VATTRC = 'S'
040800150512     C                   write     FIVAT000
040900150512     C                   add       1             §CTROKVT
041000150512     C                   endif
041100060331     C*
041200060331     C                   if        wVATNOT_E <> *blanks
041300060331     C                   eval      VATTRC = 'E'
041400100901     C*
041500100901     C* X ogni collo indicato in testata
041600110616     C***                clear                   TRUL28DSX
041700110616     C***                z-add     *zeros        wWhoIs           11 0
041800110616     C***                z-add     1             wCollo            5 0
041900110616     C***                eval      wWhoIs=%dec(%trim(%subst(wVATNOT_E:3:11))
042000110616     C***                                 :11:0)
042100110616     C***                dow       wCollo <= VABNCL
042200100901     C*
042300100901     C* Valorizzo il buffer di scrittura del FIVAT
042400110616     C***                eval      I284STD = 'E10'
042500110616     C***                eval      I284COD = %trim(%editc(wWhoIs:'X'))
042600110616     C***                call      'TRUL28R4'
042700110616     C***                parm                    TRUL28DSX
042800100909     C* Se tutto ok scrivo il "chi sono" corrente
042900110616     C***                if        O284err = *blanks
043000110616     C***                eval      VATNOT = %subst(wVATNOT_E:1:2) +
043100110616     C***                                   %trim(O284COD)
043200110919 xxx C                   eval      VATNOT = wVATNOT_E
043300110616     C                   write     FIVAT000
043400110616     C                   add       1             §CTROKVT
043500110616     C***                endif
043600100901     C*
043700110616     C***                eval      wWhoIs = wWhoIs + 1
043800110616     C***                add       1             wCollo
043900110616     C***                enddo
044000100901     C*
044100060331     C                   endif
044200040119     C*
044300040119     C                   ENDSR
044400040119
044500040119
044600040119
044700040119     C*----------------------------------------------------*
044800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044900040119     C*----------------------------------------------------*
045000040119     C     PREVAT        BEGSR
045100040119     C*
045200040119     C* Compongo il nome del membro da dare al FIVATWWR
045300040119     C                   eval      parmbr = vlrhdl
045400040119     C                   movel     'M'           parmbr
045500040510     C                   eval      parccm = vlrksc
045600040119     C                   eval      paropz = '1'
045700040119     C* Effettuo la chiamata al CLLE preposto
045800040119     C                   call(e)   'TITVVTC'
045900040119     C                   parm                    parccm
046000040119     C                   parm                    parmbr
046100040119     C                   parm                    paropz
046200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046300040119     C                   if        %error
046400040119     C                   movel     '1'           chkcall
046500040119     C                   else
046600040119     C                   movel     '0'           chkcall
046700040119     C                   endif
046800040119     C*
046900040119     C                   ENDSR
047000010601
047100010601
047200010601
047300010330     C*----------------------------------------------------*
047400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047500010330     C*----------------------------------------------------*
047600040119     C     INZVAR        BEGSR
047700080527     C*
047800080527     C                   setoff                                       65
047900010330     C*
048000030822     C* Inizializzo variabili di wrk
048100020204     C                   Z-ADD     *zeros        Num5_0            5 0
048200101018     C                   MOVEL     *blanks       wFlgCAS
048300060331     C                   MOVEL     *blanks       wVATNOT_A
048400060331     C                   MOVEL     *blanks       wVATNOT_B
048500060331     C                   MOVEL     *blanks       wVATNOT_E
048600150512     C                   MOVEL     *blanks       wVATNOT_S
048700100831     C                   MOVEL     *blanks       wVABRSD
048800100831     C                   MOVEL     *blanks       wVABIND
048900100831     C                   MOVEL     *blanks       wVABCAD
049000100831     C                   MOVEL     *blanks       wVABLOD
049100100831     C                   MOVEL     *blanks       wVABPRD
049200030822     C*
049300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049400030822     C                   CLEAR                   SkSplitCSV
049500030822     C                   EVAL      i = 1
049600030822     C                   EVAL      posDa = *zeros
049700030822     C                   EVAL      posA  = *zeros
049800080527     C*
049900080527     C                   CLEAR                   FIVABDS
050000080527     C                   CLEAR                   FIVAT000
050100030822     C*
050200030822     C* Reimposto i valori di default
050300030822     C                   EXSR      DEFCAM
050400030822     C*
050500010330     C                   ENDSR
050600010330     C*----------------------------------------------------*
050700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050800010330     C*----------------------------------------------------*
050900010330     C     DEFCAM        BEGSR
051000030822     C*
051100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051200030715     C* e delimitatore testo.
051300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051600100318     C*
051700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051800100318     C                   EVAL      CharSOS = CharNUM
051900030822     C*
052000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052100100720     C                   SETOFF                                       505152
052200100720     C                   SETOFF                                       53
052300100413     C                   SELECT
052400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052500100720     C                   SETON                                        50
052600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052700100413     C                   SETON                                        51
052800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052900100720     C                   SETON                                        52
053000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053100100720     C                   SETON                                        53
053200100413     C                   ENDSL
053300030822     C*
053400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053500030822     C                   EVAL      posDaDft = 1
053600030822     C                   EVAL      posADft  = 0
053700030822     C                   EVAL      wGiroDft = 0
053800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053900030822     C                             posDaDft > 0
054000030822     C*
054100030822     C* Gestisco il 1° giro
054200030822     C                   IF        wGiroDft = 0
054300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054500030822     C* Incremento il contatore dei "giri"
054600030822     C                   EVAL      wGiroDft = 1
054700030822     C                   ELSE
054800030822     C                   EVAL      posDaDft = posADft
054900030822     C                   ENDIF
055000030822     C* Eseguo lo scan x trovare la fine del campo corrente
055100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055200030822     C*
055300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055500030822     C                   IF        posDaDft > 0 AND
055600030822     C                             posADft  > 0
055700030822     C* NCL
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'NCL'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiInt=*on
056700030822     C                   Z-ADD     PiVal         VABNCL
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* CCM
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'CCM'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiInt=*on
058000030822     C                   Z-ADD     PiVal         VABCCM
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* LNP
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'LNP'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiInt=*on
059300030822     C                   Z-ADD     PiVal         VABLNP
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* NRS
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'NRS'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600030822     C                   Z-ADD     PiVal         VABNRS
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* CTR
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'CTR'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiInt=*on
061900030822     C                   Z-ADD     PiVal         VABCTR
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* PKB
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'PKB'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiNum=*on
063200030822     C                   Z-ADD     PiVal         VABPKB
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* VLB
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'VLB'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiNum=*on
064500030822     C                   Z-ADD     PiVal         VABVLB
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* QFT
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'QFT'
065300030822     C                   EVAL      PiStr=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   EXSR      CHKNUM
065700030822     C                   IF        PiNum=*on
065800030822     C                   Z-ADD     PiVal         VABQFT
065900030822     C                   ENDIF
066000030822     C                   ENDIF
066100030822     C* CBO
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'CBO'
066600030822     C                   EVAL      VABCBO=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* TSP
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'TSP'
067500030822     C                   EVAL      VABTSP=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* VAS
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'VAS'
068400030822     C                   EVAL      VABVAS=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* VCA
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'VCA'
069300030822     C                   EVAL      VABVCA=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700070925     C* TIC
069800070925     C                   IF        %subst(
069900070925     C                             %subst(vlrppt:posDaDft+1:
070000070925     C                             posADft-posDaDft-1):1:3)
070100070925     C                             = 'TIC'
070200070925     C                   EVAL      VABTIC=%trim(%subst(
070300070925     C                             %subst(vlrppt:posDaDft+1:
070400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500070925     C                   ENDIF
070600030822     C* GCA
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'GCA'
071100030822     C                   EVAL      VABGCA=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* CTM
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'CTM'
072000030822     C                   EVAL      VABCTM=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* FFD
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'FFD'
072900030822     C                   EVAL      VABFFD=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* VAD
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'VAD'
073800030822     C                   EVAL      VABVAD=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* GMA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GMA'
074700030822     C                   EVAL      VABGMA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* GGA
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'GGA'
075600030822     C                   EVAL      VABGGA=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* GVA
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'GVA'
076500030822     C                   EVAL      VABGVA=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* TC1
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'TC1'
077400030822     C                   EVAL      VABTC1=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* TC2
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'TC2'
078300030822     C                   EVAL      VABTC2=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700040714     C* VATTRC
078800040714     C                   IF        %subst(
078900040714     C                             %subst(vlrppt:posDaDft+1:
079000040714     C                             posADft-posDaDft-1):1:3)
079100040714     C                             = 'TRC'
079200040714     C                   EVAL      VATTRC=%trim(%subst(
079300040714     C                             %subst(vlrppt:posDaDft+1:
079400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500040714     C                   ENDIF
079600030822     C* ...
079700030822     C                   ENDIF
079800030822     C                   ENDDO
079900020204     C*
080000010330     C                   ENDSR
080100010607     C*----------------------------------------------------*
080200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080300010607     C*----------------------------------------------------*
080400010607     C     IMPVAB        BEGSR
080500010607     C*
080600010607     C                   SETOFF                                       3132
080700030515     C*
080800010607     C                   MOVEL     datcor        VABAAS
080900010607     C                   MOVE      datcor        VABMGS
081000010607     C*
081100030715     C**********
081200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081300030715     C**********
081400030715     C                   IF        wGiro = *zeros
081500040113     C*
081600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081900080429     C                             %trim(intcol(5))<>*blanks
082000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082200080429     C                                      %trim(intcol(5))
082300070823     C     ';':CharCSV   XLATE     vindta        vindta
082400040510     C                   ENDIF
082500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082600031201     C     minu:maiu     XLATE     vindta        vindta
082700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082800030715     C                   DOW       posDa <= %len(%trim(vindta))
082900030715     C*
083000030715     C* Gestisco il 1° campo
083100030715     C                   IF        i = 1
083200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083300030715     C                   EVAL      posDa = 1
083400030715     C* Eseguo lo scan x trovare la fine del primo campo
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C* X i campi successivi al 1°
084000030715     C                   ELSE
084100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084200030715     C                   EVAL      posDa = posA + 1
084300030715     C* Eseguo lo scan x trovare la fine del campo corrente
084400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084500030715     C* Gestisco l'ultimo campo
084600030715     C                   IF        posA = *zeros
084700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C                   LEAVE
085200030715     C                   ELSE
085300030715     C* A questo punto "estraggo" il campo corrente
085400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085500030715     C                                                    (posA-posDa))
085600030715     C                   ENDIF
085700030715     C                   ENDIF
085800030715     C* Incremento il contatore d campo
085900030715     C                   EVAL      i = i +1
086000030715     C                   ENDDO
086100030715     C*
086200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086300030715     C* delimitazione testo
086400030715     C                   EVAL      i = 1
086500030715     C                   DOW       i < %elem(SkSplitFLD)
086600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086800030715     C                   EVAL      i = i + 1
086900030715     C                   ENDDO
087000030715     C                   EVAL      wGiro = 1
087100030715     C                   ELSE
087200030715     C                   EVAL      wGiro = 2
087300031204     C**********
087400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087500031204     C* d separatore campo
087600031204     C**********
087700031204     C                   EVAL      vindta = %trim(vindta)
087800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087900031204     C                   EVAL      vindta = %subst(vindta:2)
088000031204     C                   ENDDO
088100031204     C*
088200031204     C                   Z-ADD     *zeros        lunghInput        4 0
088300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088700031204     C                   ENDDO
088800030715     C**********
088900030715     C* Effettuo lo split del campo dati d input
089000030715     C**********
089100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089200030715     C                   DOW       posDa <= %len(%trim(vindta))
089300030715     C*
089400030715     C* Gestisco il 1° campo
089500030715     C                   IF        i = 1
089600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089700030715     C                   EVAL      posDa = 1
089800030715     C* Eseguo lo scan x trovare la fine del primo campo
089900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090000030715     C* A questo punto "estraggo" il campo corrente
090100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090200030715     C                                                    (posA-posDa))
090300030715     C* X i campi successivi al 1°
090400030715     C                   ELSE
090500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090600030715     C                   EVAL      posDa = posA + 1
090700031204     C* Verifico che nn vi sia il campo nn valorizzato
090800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090900031204     C* Se campo nn valorizzato skippo al prossimo
091000031204     C                   EVAL      SkSplitCSV(i) = *blanks
091100031204     C                   EVAL      posA  = posA + 1
091200031204     C                   ELSE
091300030715     C* Eseguo lo scan x trovare la fine del campo corrente
091400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091500030715     C* Gestisco l'ultimo campo
091600030715     C                   IF        posA = *zeros
091700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C                   LEAVE
092200030715     C                   ELSE
092300030715     C* A questo punto "estraggo" il campo corrente
092400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092500030715     C                                                    (posA-posDa))
092600030715     C                   ENDIF
092700031204     C                   ENDIF
092800030715     C                   ENDIF
092900030715     C* Incremento il contatore d campo
093000030715     C                   EVAL      i = i +1
093100030715     C                   ENDDO
093200030715     C*
093300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093400030715     C* delimitazione testo
093500030715     C                   EVAL      i = 1
093600030715     C                   DOW       i < %elem(SkSplitCSV)
093700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093900030715     C                   EVAL      i = i + 1
094000030715     C                   ENDDO
094100100720     C*
094200100720     C* Stacco un progressivo x numero spedizione
094300100720 xxx C***                EXSR      repNSP
094400100326     C*
094500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094600030715     C                   EVAL      i = 1
094700030715     C                   DOW       i < %elem(SkSplitFLD)
094800030715     C*
094900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095100030715     C                             SkSplitCSV(i) <> *blanks
095200030721     C*** ATB
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095400030721     C                   EVAL      VABATB = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030715     C*** RSD
095700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096100030715     C* ==
096200030715     C                   ENDIF
096300030721     C*** RD2
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030715     C*** IND
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096900030715     C                   EVAL      VABIND = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** LOD
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** PRD
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030721     C*** NZD
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030715     C*** RMA
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** NOT
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** NT2
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** VCA
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** VAS
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030715     C*** FFD
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030721     C*** GC1
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** GC2
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** TSP
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101800030721     C                   ENDIF
101900040510     C*** CBO
102000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102200040510     C                   ENDIF
102300030721     C*** NAS
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** TIC
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** GCA
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** XCO
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** CTM
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** TCR
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** CTS
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** FTM
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** VAD
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** GMA
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GGA
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** GVA
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TC1
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** TC2
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** SCL
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** RMO
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** NMO
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*
109200030721     C* Reperisco quindi i campi numerici...
109300040510     C*** NSP
109400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109500040510     C                   EVAL      PiStr=SkSplitCSV(i)
109600040510     C                   EXSR      CHKNUM
109700040510     C                   IF        PiInt=*on
109800040510     C                   Z-ADD     PiVal         VABNSP
109900040510     C                   ELSE
110000040510     C                   SETON                                        31
110100040510     C                   EVAL      VABNSP = *zeros
110200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110300040510     C                             + ' ' + 'VABNSP'
110400040510     C                   ENDIF
110500040510     C                   ENDIF
110600071106     C*** RMN
110700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110800071106     C                   EVAL      PiStr=SkSplitCSV(i)
110900071106     C                   EXSR      CHKNUM
111000071106     C                   IF        PiInt=*on
111100071106     C                   Z-ADD     PiVal         VABRMN
111200071106     C                   ELSE
111300071106     C                   SETON                                        32
111400071106     C                   EVAL      VABRMN = *zeros
111500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111600071106     C                             + ' ' + 'VABRMN'
111700071106     C                   ENDIF
111800071106     C                   ENDIF
111900030721     C*** CAD
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiInt=*on
112400030721     C                   Z-ADD     PiVal         Num5_0
112500030721     C                   MOVEL(p)  Num5_0        VABCAD
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      VABCAD = *zeros
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000030721     C                             + ' ' + 'VABCAD'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** NCL
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiInt=*on
113800030721     C                   Z-ADD     PiVal         VABNCL
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      VABNCL = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040122     C                             + ' ' + 'VABNCL'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** PKB
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiNum=*on
115100030721     C                   Z-ADD     PiVal         VABPKB
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      VABPKB = *zeros
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600030721     C                             + ' ' + 'VABPKB'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** CAS
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116100101018     C                   IF        SkSplitCSV(i) <> *blanks AND
116200101018     C                             SkSplitCSV(i) <> *zeros
116300101018     C                   EVAL      wFlgCAS = '1'
116400101018     C                   ENDIF
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiNum=*on
116800030721     C                   Z-ADD     PiVal         VABCAS
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      VABCAS = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABCAS'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** CCM
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on      AND
118100030721     C                             PiVal<=9999999 AND
118200030721     C                             PiVal>*zeros
118300030721     C                   Z-ADD     PiVal         VABCCM
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABCCM = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800040510     C                             + ' ' + 'VABCCM'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** LNP
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on    AND
119600030721     C                             PiVal<=999   AND
119700030721     C                             PiVal>*zeros
119800030721     C                   Z-ADD     PiVal         VABLNP
119900030721     C                   ELSE
120000040715     C                   SETON                                        32
120100030721     C                   EVAL      VABLNP = *zeros
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300040510     C                             + ' ' + 'VABLNP'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** NRS
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on AND
121100030721     C                             PiVal<=99
121200030721     C                   Z-ADD     PiVal         VABNRS
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABNRS'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** LNA
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on    AND
122400030721     C                             PiVal<=999   AND
122500030721     C                             PiVal>*zeros
122600030721     C                   Z-ADD     PiVal         VABLNA
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      VABLNA = *zeros
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABLNA'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** CTR
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on  AND
123900030721     C                             PiVal<=999
124000030721     C                   Z-ADD     PiVal         VABCTR
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABCTR'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** IAS
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiNum=*on
125200030721     C                   Z-ADD     PiVal         VABIAS
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABIAS'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** VLB
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiNum=*on
126400030721     C                   Z-ADD     PiVal         VABVLB
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABVLB'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** QFT
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiNum=*on
127600030721     C                   Z-ADD     PiVal         VABQFT
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABQFT'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** NCD
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on      AND
128800030721     C                             PiVal<=9999999
128900030721     C                   Z-ADD     PiVal         VABNCD
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABNCD'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** NCA
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on      AND
130100030721     C                             PiVal<=9999999
130200030721     C                   Z-ADD     PiVal         VABNCA
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABNCA'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** ZNC
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on AND
131400030721     C                             PiVal<=99
131500030721     C                   Z-ADD     PiVal         VABZNC
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABZNC'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** DCR
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on       AND
132700030721     C                             PiVal<=99999999
132800030721     C                   Z-ADD     PiVal         VABDCR
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABDCR'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** HCR
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on   AND
134000030721     C                             PiVal<=9999
134100030721     C                   Z-ADD     PiVal         VABHCR
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABHCR'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** VMD
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiNum=*on
135300030721     C                   Z-ADD     PiVal         VABVMD
135400030721     C                   ELSE
135500030721     C                   SETON                                        32
135600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135700030721     C                             + ' ' + 'VABVMD'
135800030721     C                   ENDIF
135900030721     C                   ENDIF
136000030721     C*** ANT
136100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136200030721     C                   EVAL      PiStr=SkSplitCSV(i)
136300030721     C                   EXSR      CHKNUM
136400030721     C                   IF        PiInt=*on        AND
136500030721     C                             PiVal<=999999999
136600030721     C                   Z-ADD     PiVal         VABANT
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABANT'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** CMO
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiInt=*on
137800030721     C                   Z-ADD     PiVal         Num5_0
137900030721     C                   MOVEL(p)  Num5_0        VABCMO
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      VABCMO = *zeros
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABCMO'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700040510     C*
138800040510     C* Reperisco i campi del FIVAT
138900040510     C*** VATNOT
139000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139200040510     C                   ENDIF
139300040510     C*** VATTRC
139400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139600040510     C                   ENDIF
139700060331     C*** VATNOT_A
139800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140000060331     C                   ENDIF
140100060331     C*** VATNOT_B
140200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140400060331     C                   ENDIF
140500150512     C*** VATNOT_S
140600150512     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
140700150512     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
140800150512     C                   ENDIF
140900060331     C*** VATNOT_E
141000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141200060331     C                   ENDIF
141300070823     C*
141400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
141500100831     C*** RSD
141600100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141700101018     C                   MOVEL     *blanks       wRSD             70
141800101018     C                   EVAL      wRSD = %trim(SkSplitCSV(i))
141900101018     C                   EVAL      VABRSD = %subst(wRSD:1:35)
142000101018     C                   EVAL      VABRD2 = %subst(wRSD:36:35)
142100100831     C                   ENDIF
142200110613     C*** NSP / RMN / NOT_E
142300101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
142400110613     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142500101021     C                   EVAL      PiStr=SkSplitCSV(i)
142600101021     C                   EXSR      CHKNUM
142700101021     C                   IF        PiInt=*on
142800101021     C                   Z-ADD     PiVal         VABNSP
142900101021     C                   Z-ADD     PiVal         VABRMN
143000101021     C                   ELSE
143100101021     C                   SETON                                        31
143200101021     C                   EVAL      VABNSP = *zeros
143300101021     C                   EVAL      VABRMN = *zeros
143400101021     C                   EVAL      vinmsg = %trimr(vinmsg)
143500110613     C                             + ' ' + 'VABNSP VABRMN VATNOT_E'
143600101021     C                   ENDIF
143700101021     C                   ENDIF
143800110613     C*** GC1
143900110613     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABGC1'
144000110613     C                   SELECT
144100110613     C                   WHEN      %trim(SkSplitCSV(i)) =
144200110613     C                                  'CONSEGNA MATTINA'
144300110613     C                   EVAL      VABGC1 = 'P'
144400110613     C                   WHEN      %trim(SkSplitCSV(i)) =
144500110613     C                                  'CONSEGNA POMERIGGIO'
144600110613     C                   EVAL      VABGC1 = 'M'
144700110613     C                   ENDSL
144800110613     C                   ENDIF
144900070925     C*
145000071029     C***  <===  -----------------------------------------
145100040510     C*
145200030715     C                   ENDIF
145300030715     C                   EVAL      i = i + 1
145400030715     C                   ENDDO
145500070925     C*
145600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145700100831     C*
145800100831     C                   IF        wVABRSD <> *blanks
145900100831     C                   EVAL      VABRSD = wVABRSD
146000100831     C                   ENDIF
146100100831     C*
146200100831     C                   IF        wVABIND <> *blanks
146300100831     C                   EVAL      VABIND = wVABIND
146400100831     C                   ENDIF
146500100831     C*
146600100831     C                   IF        wVABCAD <> *blanks
146700100831     C                   EVAL      VABCAD = wVABCAD
146800100831     C                   ENDIF
146900100831     C*
147000100831     C                   IF        wVABLOD <> *blanks
147100100831     C                   EVAL      VABLOD = wVABLOD
147200100831     C                   ENDIF
147300100831     C*
147400100831     C                   IF        wVABPRD <> *blanks
147500100831     C                   EVAL      VABPRD = wVABPRD
147600100831     C                   ENDIF
147700070925     C*
147800071029     C***  <===  -----------------------------------------
147900070925     C*
148000070925     C* Considerazioni finali su CBO/CAS
148100100318     C                   IF        wFlgCAS = '1'    OR
148200100318     C                             VABCAS  > *zeros
148300070925     C                   IF        VABCBO = '1'
148400070925     C                   EVAL      VABCBO = '4'
148500070925     C                   ENDIF
148600070925     C                   IF        VABCBO = '2'
148700070925     C                   EVAL      VABCBO = '6'
148800070925     C                   ENDIF
148900070925     C                   ENDIF
149000070925     C***  <===  ----------------------------
149100040510     C*
149200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149300040510     C                   IF        VABNSP = *zeros
149400040510     C                   SETON                                        31
149500040510     C                   ENDIF
149600040715     C*
149700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149800040715     C                   IF        VABLNP = *zeros
149900040715     C                   SETON                                        31
150000040715     C                   ENDIF
150100040510     C*
150200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150300040510     C                   IF        VABRMN = *zeros
150400040510     C                   Z-ADD     VABNSP        VABRMN
150500040510     C                   ENDIF
150600040510     C*
150700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150800040510     C                   IF        VABRMA = *blanks
150900040714     C                   MOVEL     VABRMN        VABRMA
151000040510     C                   ENDIF
151100040128     C*
151200040510     C* Se provincia nn valorizzata la reperisco
151300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151400040510     C                   IF        VABPRD = *blanks
151500040128     C                   CLEAR                   TISI95DS
151600040128     C                   EVAL      I95TCN = '3'
151700040128     C                   Z-ADD     datcor        I95DAT
151800040128     C                   EVAL      I95NAR = VABNZD
151900040128     C                   EVAL      I95CAP = VABCAD
152000040128     C                   EVAL      I95LOC = VABLOD
152100040128     C                   CALL      'TISI95R'
152200040128     C                   PARM                    TISI95DS
152300040128     C                   EVAL      VABPRD = O95PRV
152400040510     C                   ENDIF
152500020204     C*
152600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152700020204     C                   EXSR      CHKIMPDIV
152800030715     C                   ENDIF
152900020204     C*
153000010607     C                   ENDSR
153100010607     C*----------------------------------------------------*
153200010601
153300020204
153400020204     C*----------------------------------------------------*
153500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153600020204     C*----------------------------------------------------*
153700020204     C     CHKIMPDIV     BEGSR
153800020204     C*
153900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154000020204     C                   Z-ADD     *zeros        wrkDec            9 9
154100020204     C*
154200020204     C* Come prima cosa effettuo considerazioni sulla divisa
154300020204     C                   IF        vabIAS > *zeros
154400020204     C                   IF        vabVAS <> 'EUR'
154500020204     C                   EVAL      vabVAS =  'ITL'
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C                   IF        vabCAS > *zeros
155000020204     C                   IF        vabVCA <> 'EUR'
155100020204     C                   EVAL      vabVCA =  'ITL'
155200020204     C                   ENDIF
155300020204     C                   ENDIF
155400020204     C*
155500020204     C                   IF        vabVMD > *zeros
155600020204     C                   IF        vabVAD <> 'EUR'
155700020204     C                   EVAL      vabVAD =  'ITL'
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156200020204     C                   Z-ADD     vabIAS        wrkDec
156300020204     C                   IF        wrkDec > *zeros
156400020204     C                   IF        vabVAS = 'ITL'
156500020204     C                   EVAL      vabIAS = *zeros
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157000020204     C                   Z-ADD     vabCAS        wrkDec
157100020204     C                   IF        wrkDec > *zeros
157200020204     C                   IF        vabVCA = 'ITL'
157300020204     C                   EVAL      vabCAS = *zeros
157400020204     C                   ENDIF
157500020204     C                   ENDIF
157600020204     C*
157700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157800020204     C                   Z-ADD     vabVMD        wrkDec
157900020204     C                   IF        wrkDec > *zeros
158000020204     C                   IF        vabVAD = 'ITL'
158100020204     C                   EVAL      vabVMD = *zeros
158200020204     C                   ENDIF
158300020204     C                   ENDIF
158400020204     C*
158500020204     C                   ENDSR
158600020204     C***
158700010330
158800100318
158900100318
159000100318     C*----------------------------------------------------*
159100100318     C*  CONTROLLO NUMERICITA' CAMPI
159200100318     C*----------------------------------------------------*
159300100318     C     CHKNUM        BEGSR
159400100318     C*
159500100318     C                   IF        PiDecChr = *blanks
159600100318     C                   EVAL      PiDecChr = CharNUM
159700100318     C                   ENDIF
159800100318     C*
159900100318     C                   callp     UBISNUM_Check(PiStr
160000100318     C                                          :PiDecChr
160100100318     C                                          :PiVal
160200100318     C                                          :PiNum
160300100318     C                                          :PiInt)
160400100318     C*
160500100318     C                   ENDSR
160600100318     C***
160700010601
160800010601
160900010601
161000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161100010601     C     repfil        BEGSR
161200010601     C*
161300010601     C                   if        invfil = *zeros and
161400010601     C                             depfil > *zeros and
161500010629     C                             (vinflg = *blanks or
161600010629     C                              vinflg = *zeros)
161700010601     C
161800010601     C                   eval      invfil = depfil
161900010601     C                   endif
162000010601     C*
162100010601     C                   if        depfil <> invfil and
162200010601     C                             invfil > *zeros
162300010601     C                   eval      flgMulti = '1'
162400010601     C                   if        vinflg = *blanks
162500010601     C                   add       1             cntNonEl
162600010601     C                   endif
162700010601     C                   endif
162800010601     C*
162900010601     C                   if        vinflg = '2'
163000010601     C                   eval      flgStato = '2'
163100010601     C                   endif
163200010601     C*
163300010601     C                   ENDSR
163400010601     C***
163500010601
163600010601
163700010601
163800010330
163900040119      /TITLE Invio dei dati al punto operativo.
164000040119     C     invio         BEGSR
164100040119     C*
164200040119     C* 1° invio FIVAT
164300040119     C                   reset                   dscmz
164400040119     C                   move      vlrpoi        cmzdst
164500040119     C                   eval      cmzfld = 'FIVATWWR'
164600040119     C                   eval      cmzmbd = vlrhdl
164700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164800040119     C***                if        prmfir = *blanks
164900040119     C                   eval      cmzfla = 'FIVAT00F'
165000040119     C                   eval      cmzmba = 'FIVAT00F'
165100040119     C***                else
165200040119     C***                eval      cmzfla = prmfir
165300040119     C***                eval      cmzmba = prmfir
165400040119     C***                endif
165500040119     C                   eval      cmznrr = *zeros
165600040119     C                   move      §ctrokvt      cmznrr
165700040119     C                   eval      cmzlba = vlrfl1
165800040119     C                   call(e)   'TIS711C'
165900040119     C                   parm                    dscmz
166000040119     C                   parm      *blanks       esito
166100040119     C                   if        %error
166200040119     C                             or cmzerr = '1'
166300040119     C                             or esito  = '1'
166400040119     C                   eval      wrkesito = '3'
166500040119     C                   else
166600040119     C*
166700040119     C* 2° invio FIVAB
166800040119     C                   reset                   dscmz
166900040119     C                   move      vlrpoi        cmzdst
167000040119     C                   eval      cmzfld = vlrfou
167100040119     C                   eval      cmzmbd = vlrhdl
167200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167300040119     C***                if        prmfir = *blanks
167400040119     C                   eval      cmzfla = 'FIVAB00F'
167500040119     C                   eval      cmzmba = 'FIVAB00F'
167600040119     C***                else
167700040119     C***                eval      cmzfla = prmfir
167800040119     C***                eval      cmzmba = prmfir
167900040119     C***                endif
168000040119     C                   eval      cmznrr = *zeros
168100040119     C                   move      §ctrokvb      cmznrr
168200040119     C                   eval      cmzlba = vlrfl1
168300040119     C                   call(e)   'TIS711C'
168400040119     C                   parm                    dscmz
168500040119     C                   parm      *blanks       esito
168600040119     C                   if        %error
168700040119     C                             or cmzerr = '1'
168800040119     C                             or esito  = '1'
168900040119     C                   eval      wrkesito = '3'
169000040119     C                   endif
169100040119     C                   endif
169200040119     C*
169300040119     C                   ENDSR
169400040119     C***
169500010601
169600010601
169700010601
169800010601
169900010601      /TITLE Invio dei dati al punto operativo.
170000010601     C     opeini        BEGSR
170100010601     C*
170200010601     C* Inizializzo flag e contatori operativi
170300010601     C                   movel     '0'           flgGiro           1
170400010601     C                   movel     '0'           flgMulti          1
170500010601     C                   movel     '1'           flgStato          1
170600010615     C                   movel     '0'           flgOk             1
170700010601     C                   z-add     *zeros        cntNonEl         10 0
170800010601     C                   z-add     *zeros        depfil            3 0
170900010601     C                   z-add     *zeros        invfil            3 0
171000010601     C*
171100010601     C                   ENDSR
171200010601     C***
171300070326
171400070326
171500070326
171600070326
171700070326     C     *pssr         BEGSR
171800070329     C*
171900070329     C                   if        %open(tivin00r)
172000070329     C                   close     tivin00r
172100070329     C                   endif
172200070329     C                   if        %open(fivabwwr)
172300070329     C                   close     fivabwwr
172400070329     C                   endif
172500070329     C                   if        %open(fivatwwr)
172600070329     C                   close     fivatwwr
172700070329     C                   endif
172800070326     C*
172900070326     C* Effettuo la chiamata al CLLE preposto
173000070326     C                   call(e)   'TITVVTC'
173100070326     C                   parm                    parccm
173200070326     C                   parm                    parmbr
173300070326     C                   parm      '2'           paropz
173400070326     C*
173500070326     C                   eval      wrkesito = '2'
173600070404     C*
173700070404     C                   seton                                        LR
173800070326     C*
173900070326     C                   ENDSR     '*CANCL'
174000070326     C***
174100100318
174200100318
174300100318
174400100318     C     normCSV       BEGSR
174500100318     C*
174600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
174700100318     C* separatori campi
174800100318     C                   if        CharTXT <> *blanks
174900100318     C                   z-add     *zeros        wLenVINDTA        4 0
175000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175100100318     C                   setoff                                       80
175200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175500100318     C                   if        not *in80
175600100318     C                   seton                                        80
175700100318     C                   else
175800100318     C                   setoff                                       80
175900100318     C                   endif
176000100318     C                   endif
176100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176200100318     C                             and *in80 = *on
176300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176400100318     C                   endif
176500100318     C                   enddo
176600100318     C                   endif
176700100318     C*
176800100318     C                   ENDSR
176900100318     C***
177000070326
177100100326
177200100326
177300100326     C*----------------------------------------------------*
177400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
177500100326     C*----------------------------------------------------*
177600100326     C     REPNSP        BEGSR
177700100326     C*
177800100326     C* NSP => Stacco un numeratore da AZNUM
177900100326     C                   clear                   TRUL33DS
178000100326     C                   eval      I33OPE = *zeros
178100100326     C                   eval      I33CNU = 302
178200100326     C                   eval      I33NUM = 1
178300100326     C                   movel     TRUL33DS      KPJBU
178400100326     C                   call      'TRUL33R'
178500100326     C                   parm                    KPJBA
178600100326     C                   movel     KPJBU         TRUL33DS
178700100326     C                   if        O33ERR = *zeros
178800100326     C                   z-add     O33NRF        VABNSP
178900100326     C                   z-add     O33NRF        VATNSP
179000100326     C                   else
179100100326     C                   SETON                                        31
179200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179300100326     C                             + ' ' + 'VABNSP VATNSP'
179400100326     C                   endif
179500100326     C*
179600100326     C                   ENDSR
179700010330
179800100326
179900010330
180000000613     C     *inzsr        BEGSR
180100990910     C*
180200990910     C     *entry        plist
180300990920     C                   parm                    tivlrds
180400990921     C                   parm      wrkesito      esito
180500000724     C                   parm                    prmlit
180600000710     C                   parm                    prmfir
180700010330     C*
180800010330     C* CALCOLA LA DATA CORRENTE
180900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181000100318     C                   eval      datcor = %dec(%date() : *ISO)
181100000613     C*
181200000613     C                   ENDSR
181300000613     C***
181400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181500110613W_VABRSD;VABIND;VABLOD;VABPRD;VABCAD;W_VABNSP;VATNOT_B;VABRMA;
181600150512VABNCL;VABPKB;VABNAS;VABNOT;W_VABGC1;VABCAS;X;VABIAS;VATNOT_S;
181700110613
181800101018
181900100318
