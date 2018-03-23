000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400130618      * Disk A
000500140220      *
000600121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700130917     H decedit('0,')
000800120209     F*
000900990910     Ftivin00r  uF   E             DISK    usropn
001000100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001100040119     FFIVATwwr  O    E             DISK    usropn
001200000313     D*
001300040113     D*------------
001400040113     D* SCHIEDE A PROGRAMMA
001500040113     D*------------
001600080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400040128     D tisi95ds      e ds
002500100413     D fivabds       e ds                  extname(fivab00f)
002600100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002700100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D rrnum           s              6  0 INZ(*zeros)
003300040510     D parccm          s              8    INZ(*blanks)
003400040119     D parmbr          s             10    INZ(*blanks)
003500040119     D paropz          s              1    INZ(*blanks)
003600040119     D chkcall         s              1    INZ(*blanks)
003700080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003800080418     D wFlgCAS         s              1    INZ(*blanks)
003900120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004000120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004100140305     D Intestazioni    s               n   inz(*on)
004200100326     D*------------------
004300100326     D* DS REPERIMENTO NUMERATORE
004400100326     D*------------------
004500100326     D trul33ds      e ds                  inz
004600100326     D*------------------
004700100326     D* DS ARCHITETTURA
004800100326     D*------------------
004900100326     D kpjba         e ds                  inz
005000130611     D*------------------
005100130611     D TRUL28DSX     e ds                  inz
005200100326
005300030715     D*------------------
005400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005500030715     D*------------------
005600030715     D SkSplitFLD      S             10    DIM(100)
005700030715     D SkSplitCSV      S            256    DIM(100)
005800030715     D CharCSV         S              1
005900030715     D CharTXT         S              1
006000030715     D CharNUM         S              1
006100100318     D CharSOS         S              1
006200130626     D posDa           S              4  0 INZ(*zeros)
006300130626     D posA            S              4  0 INZ(*zeros)
006400030715     D i               s              3  0 INZ(1)
006500030715     D wGiro           s              1  0 INZ(*zeros)
006600070823     D wVATNOT_A       s                   LIKE(VATNOT)
006700060331     D wVATNOT_B       s                   LIKE(VATNOT)
006800060331     D wVATNOT_E       s                   LIKE(VATNOT)
006900110927     D wVATNOT_I       s                   LIKE(VATNOT)
007000140220     D wVATNOT_J       s                   LIKE(VATNOT)
007100140220     D wVATNOT_IJ      s             70
007200120411     D wNOT            s             70
007300120411     D Data8A          s              8
007400030822     D*------------------
007500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007600030822     D*------------------
007700030822     D posDaDft        S              3  0 INZ(*zeros)
007800030822     D posADft         S              3  0 INZ(*zeros)
007900030822     D j               s              3  0 INZ(1)
008000030822     D wGiroDft        s              1  0 INZ(*zeros)
008100080418     D*------------------
008200080418     D* Costanti
008300080418     D*------------------
008400080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008500080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008600100318
008700100318     D*------------------
008800100318     D* LINKING A DEFINIZIONI ESTERNE
008900100318     D*------------------
009000100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009200100318
009300010330
009400010330
009500010330
009600000913     C                   reset                   rrnum
009700990921     C                   reset                   esito
009800990921     C                   reset                   wrkesito
009900010601     C*
010000010601     C                   exsr      opeini
010100010605     C                   exsr      rwvab
010200010601     C*
010300010601     C                   seton                                        lr
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010900010601     C*--------------------------------------------------------
011000010601     C     PREELA        BEGSR
011100010601     C*
011200010601     C* SE OCCORRE SPEDIRE IN FILIALE
011300010601     C                   if        invfil <> *zeros and
011400010601     C                             flgGiro = '0'
011500010601     C*
011600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010601     C                   eval      flgGiro = '1'
011800010601     C*
011900010601     C                   endif
012000010601     C*
012100010601     C                   ENDSR
012200010601     C***
012300010601
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010601     C*--------------------------------------------------------
013000010601     C     ENDELA        BEGSR
013100040119     C*
013200040119     C* Effettuo la chiamata al CLLE preposto
013300040119     C                   call(e)   'TITVVTC'
013400040510     C                   parm                    parccm
013500040119     C                   parm                    parmbr
013600040119     C                   parm      '2'           paropz
013700050201     C*
013800050201     C* Effettuo lancio TISI95 solo x chiusura
013900050201     C                   CLEAR                   TISI95DS
014000050201     C                   EVAL      I95TLA = 'C'
014100050201     C                   CALL      'TISI95R'
014200050201     C                   PARM                    TISI95DS
014300000616     C*
014400010601     C                   ENDSR
014500010601
014600010601
014700010330     C*--------------------------------------------------------
014800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014900010330     C*--------------------------------------------------------
015000010605     C     RWVAB         BEGSR
015100010330     C*
015200010330     C                   if        not %open(tivin00r)
015300010330     C                   open      tivin00r
015400010330     C                   endif
015500021025     C                   if        not %open(fivabwwr)
015600021025     C                   open      fivabwwr
015700010330     C                   endif
015800040119     C*
015900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016000040119     C                   exsr      prevat
016100040119     C*
016200040119     C                   if        chkcall = '0'
016300040119     C*
016400040119     C                   if        not %open(fivatwwr)
016500040119     C                   open      fivatwwr
016600040119     C                   endif
016700010330     C*
016800040119     C                   clear                   §CTROKVB          7 0
016900040119     C                   clear                   §CTROKVT          7 0
017000040119     C                   clear                   §CTRMO            7 0
017100040119     C                   clear                   §CTRNO            7 0
017200120410     C*
017300120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017400120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017500120410     C                   IF        Intestazioni = *off
017600120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017700120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017800120410     C                   exsr      inzvar
017900120410     C                   exsr      impvab
018000120410     C                   ENDIF
018100010330     C*
018200030822     C                   do        *HIVAL
018300010330     C*
018400030822     C                   read      tivin00r                               70
018500010618     C*
018600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018700010618     C                   if        vinflg = '1'
018800010618     C                   eval      flgOk = '1'
018900010618     C                   endif
019000040510     C*
019100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019200040510     C                   z-add     *zeros        wLenVINDTA        4 0
019300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019700040510     C                   z-add     1             wFlgVINDTA
019800040510     C                   leave
019900040510     C                   endif
020000040510     C                   enddo
020100010618     C*
020200040510     C                   if        vindta > *blanks AND
020300040510     C                             wFlgVINDTA = 1
020400010330     C                   add       1             rrnum
020500010330     C*
020600010601     C                   if        *in70 = *off and
020700010330     C                             (vinflg = *blanks
020800010330     C                              or vinflg = '0'
020900010330     C                              or vinflg = '2')
021000010330     C*
021100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021200010711     C                   if        vinflg = *blanks or vinflg = '0'
021300010711     C                   clear                   vinmsg
021400010711     C                   endif
021500130917     C*
021600130917     C* Aggiungo un byte a blanks all'inizio d ogni record
021700130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021800130917     C* evitare che vengano elisi i blank
021900140220     C***                eval      vindta  = '*' + %trim(vindta)
022000130917     C*
022100080527     C                   exsr      inzvar
022200100318     C                   exsr      normCSV
022300010605     C                   exsr      impvab
022400010601     C*
022500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022600010605     C                   eval      depfil = VABLNP
022700010601     C                   exsr      repfil
022800010601     C                   if        depfil = invfil
022900021025     C                   if        vlrpoi = 999
023000030822     C                   move(P)   invfil        VABFGS
023100021025     C                   else
023200030822     C                   move(P)   vlrpoi        VABFGS
023300021025     C                   endif
023400010601     C*
023500010601     C                   exsr      PREELA
023600010601     C*
023700010604     C* Ebbene...
023800010604     C*
023900040119     C  N31              add       1             §CTROKVB
024000040119     C   32              add       1             §CTRMO
024100040119     C   31              add       1             §CTRNO
024200080418     C*
024300030822     C                   if        wGiro = 2
024400080513     C* Scrivo sempre il VAT
024500080513     C  N31              exsr      wrivat
024600110201     C                   exsr      chkvab
024700080513     C                   endif
024800010604     C*
024900010604     C                   if        *in31 = *off and
025000010604     C                             *in32 = *off
025100010604     C                   eval      vinflg = '1'
025200010604     C                   else
025300010604     C                   eval      vinflg = '2'
025400010604     C                   endif
025500010604     C                   endif
025600010601     C*
025700010604     C                   endif
025800010604     C*
025900010330     C                   else
026000010330     C                   eval      vinflg = '1'
026100010330     C                   endif
026200010601     C*
026300010601     C  N70              update    tivin000
026400010330     C*
026500030822     C  N70              enddo
026600040119     C*
026700080527     C* Verifico dati rimasti "in cannna"...
026800080527     C                   seton                                        65
026900110201     C                   exsr      chkvab
027000080527     C*
027100040119     C                   endif                                                  (endif - chkcall)
027200010601     C*
027300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027400010601     C                   if        cntNonEl = *zeros or
027500010601     C                             flgMulti = '0'
027600010330     C* Se non ci sono record con errori ...
027700010601     C                   if        §ctrno = 0 and
027800010604     C                             §ctrmo = 0 and
027900010601     C                             flgStato <> '2'
028000010330     C* ... restituisco esito OK.
028100010330     C                   eval      wrkesito = '0'
028200010330     C                   else
028300040119     C                   if        §ctrokvb > 0
028400010330     C                   eval      wrkesito = '1'
028500010330     C                   else
028600010615     C                   if        flgOk = '0'
028700010615     C                   eval      wrkesito = '2'
028800010615     C                   else
028900010615     C                   eval      wrkesito = '6'
029000010615     C                   endif
029100010330     C                   endif
029200010330     C                   endif
029300010601     C                   else
029400010601     C                   eval      wrkesito = '9'
029500010601     C                   endif
029600010330     C*
029700010330     C                   if        %open(tivin00r)
029800010330     C                   close     tivin00r
029900010330     C                   endif
030000021025     C                   if        %open(fivabwwr)
030100021025     C                   close     fivabwwr
030200010330     C                   endif
030300040512     C                   if        %open(fivatwwr)
030400040512     C                   close     fivatwwr
030500040512     C                   endif
030600010601     C*
030700010601     C                   if        vlrpoi <> 999
030800010601     C                   eval      invfil = vlrpoi
030900010601     C                   endif
031000010330     C*
031100040119     C                   if        §ctrokvb > 0
031200010601     C                             and invfil > *zeros
031300010330     C                   exsr      invio
031400010330     C                   endif
031500010601     C*
031600010618     C                   if        flgGiro = '1'
031700010601     C                   exsr      endela
031800010618     C                   endif
031900010330     C*
032000010330     C                   ENDSR
032100010330     C***
032200080527
032300080527
032400080527
032500080527     C*----------------------------------------------------*
032600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032700080527     C*----------------------------------------------------*
032800100413     C     CHKVAB        BEGSR
032900110201     C*
033000110201     C* Se richiesti ignorare colli / peso / volume
033100110315 xxx C***                if        *in35
033200110315 xxx C***                z-add     *zeros        VABNCL
033300110315 xxx C***                z-add     *zeros        VABPKB
033400110315 xxx C***                z-add     *zeros        VABVLB
033500110315 xxx C***                endif
033600080527     C*
033700080527     C* Scrivo il VAB solo a rottura d spedizione
033800080527     C                   if        depspe = *zeros
033900080527     C  N31              eval      fivabds_sav = fivabds
034000080527     C                   eval      depspe = vabNSP
034100080527     C                   else
034200080527     C                   if        depspe = vabNSP AND *in65 = *off
034300100413     C   51              add       VABNCL        sav_VABNCL
034400100413     C   51              add       VABPKB        sav_VABPKB
034500100413     C   51              add       VABVLB        sav_VABVLB
034600100720     C   52              z-add     VABNCL        sav_VABNCL
034700100720     C   52              z-add     VABPKB        sav_VABPKB
034800100720     C   52              z-add     VABVLB        sav_VABVLB
034900100720     C   53              add       VABNCL        sav_VABNCL
035000100720     C   53              z-add     VABPKB        sav_VABPKB
035100100720     C   53              z-add     VABVLB        sav_VABVLB
035200080527     C                   else
035300080527     C  N31              eval      fivabds_ok = fivabds_sav
035400080527     C  N31              exsr      wrivab
035500080527     C  N31              eval      fivabds_sav = fivabds
035600080527     C                   eval      depspe = vabNSP
035700080527     C                   endif
035800080527     C                   endif
035900080527     C*
036000080527     C                   ENDSR
036100040119
036200040119
036300040119
036400040119     C*----------------------------------------------------*
036500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036600040119     C*----------------------------------------------------*
036700040510     C     WRIVAB        BEGSR
036800080513     C*
036900080513     C* Gestisco l'eventuale rottura x numero spedizione
037000080527     C                   if        VABNSP <> depspe or *in65 = *on
037100070301     C                   if        wVATNOT_E <> *blanks
037200130610     C                   eval      ok_VABCTM = '7Q'
037300070301     C                   endif
037400100413     C                   write     fivab000
037500080513     C                   endif
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038300040119     C*----------------------------------------------------*
038400040510     C     WRIVAT        BEGSR
038500060512     C*
038600060512     C* Inizializzo i campi chiave
038700060512     C                   eval      VATFGS = VABFGS
038800060512     C                   eval      VATAAS = VABAAS
038900060512     C                   eval      VATCCM = VABCCM
039000070823     C                   eval      VATNRS = VABNRS
039100060512     C                   eval      VATNSP = VABNSP
039200060512     C                   eval      VATLNP = VABLNP
039300040119     C*
039400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039500040510     C                   if        VATTRC = *blanks
039600040510     C                   eval      VATTRC = 'E'
039700040510     C                   endif
039800110318     C                   if        VATNOT <> *blanks
039900040510     C                   write     FIVAT000
040000040119     C                   add       1             §CTROKVT
040100110318     C                   endif
040200060331     C*
040300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040400060331     C                   if        wVATNOT_A <> *blanks
040500060331     C                   eval      VATNOT = wVATNOT_A
040600060331     C                   eval      VATTRC = 'A'
040700060331     C                   write     FIVAT000
040800060331     C                   add       1             §CTROKVT
040900060331     C                   endif
041000060331     C*
041100060331     C                   if        wVATNOT_B <> *blanks
041200060331     C                   eval      VATNOT = wVATNOT_B
041300060331     C                   eval      VATTRC = 'B'
041400060331     C                   write     FIVAT000
041500060331     C                   add       1             §CTROKVT
041600060331     C                   endif
041700060331     C*
041800060331     C                   if        wVATNOT_E <> *blanks
041900110201     C                   eval      VATNOT = wVATNOT_E
042000060331     C                   eval      VATTRC = 'E'
042100140220     C* è un disk A, per cui niente barcode
042200120320     C                   endif
042300110927     C*
042400110927     C                   if        wVATNOT_I <> *blanks
042500110927     C                   eval      VATNOT = wVATNOT_I
042600110927     C                   eval      VATTRC = 'I'
042700110927     C                   write     FIVAT000
042800110927     C                   add       1             §CTROKVT
042900110927     C                   endif
043000140220     C*
043100140220     C                   if        wVATNOT_J <> *blanks
043200140220     C                   eval      VATNOT = wVATNOT_J
043300140220     C                   eval      VATTRC = 'J'
043400140220     C                   write     FIVAT000
043500140220     C                   add       1             §CTROKVT
043600140220     C                   endif
043700040119     C*
043800040119     C                   ENDSR
043900040119
044000040119
044100040119
044200040119     C*----------------------------------------------------*
044300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044400040119     C*----------------------------------------------------*
044500040119     C     PREVAT        BEGSR
044600040119     C*
044700040119     C* Compongo il nome del membro da dare al FIVATWWR
044800040119     C                   eval      parmbr = vlrhdl
044900040119     C                   movel     'M'           parmbr
045000040510     C                   eval      parccm = vlrksc
045100040119     C                   eval      paropz = '1'
045200040119     C* Effettuo la chiamata al CLLE preposto
045300040119     C                   call(e)   'TITVVTC'
045400040119     C                   parm                    parccm
045500040119     C                   parm                    parmbr
045600040119     C                   parm                    paropz
045700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045800040119     C                   if        %error
045900040119     C                   movel     '1'           chkcall
046000040119     C                   else
046100040119     C                   movel     '0'           chkcall
046200040119     C                   endif
046300040119     C*
046400040119     C                   ENDSR
046500010601
046600010601
046700010601
046800010330     C*----------------------------------------------------*
046900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047000010330     C*----------------------------------------------------*
047100040119     C     INZVAR        BEGSR
047200080527     C*
047300080527     C                   setoff                                       65
047400010330     C*
047500030822     C* Inizializzo variabili di wrk
047600020204     C                   Z-ADD     *zeros        Num5_0            5 0
047700101018     C                   MOVEL     *blanks       wFlgCAS
047800060331     C                   MOVEL     *blanks       wVATNOT_A
047900060331     C                   MOVEL     *blanks       wVATNOT_B
048000060331     C                   MOVEL     *blanks       wVATNOT_E
048100110927     C                   MOVEL     *blanks       wVATNOT_I
048200140220     C                   MOVEL     *blanks       wVATNOT_J
048300140220     C                   MOVEL     *blanks       wVATNOT_IJ
048400120411     C                   MOVEL     *blanks       wNOT
048500120411     C                   MOVEL     *blanks       Data8A
048600030822     C*
048700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048800030822     C                   CLEAR                   SkSplitCSV
048900030822     C                   EVAL      i = 1
049000030822     C                   EVAL      posDa = *zeros
049100030822     C                   EVAL      posA  = *zeros
049200080527     C*
049300080527     C                   CLEAR                   FIVABDS
049400080527     C                   CLEAR                   FIVAT000
049500030822     C*
049600030822     C* Reimposto i valori di default
049700030822     C                   EXSR      DEFCAM
049800030822     C*
049900010330     C                   ENDSR
050000010330     C*----------------------------------------------------*
050100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050200010330     C*----------------------------------------------------*
050300010330     C     DEFCAM        BEGSR
050400030822     C*
050500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050600030715     C* e delimitatore testo.
050700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051000100318     C*
051100100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051200100318     C                   EVAL      CharSOS = CharNUM
051300030822     C*
051400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051500100720     C                   SETOFF                                       505152
051600100720     C                   SETOFF                                       53
051700100413     C                   SELECT
051800100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051900100720     C                   SETON                                        50
052000100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052100100413     C                   SETON                                        51
052200100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052300100720     C                   SETON                                        52
052400100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052500100720     C                   SETON                                        53
052600100413     C                   ENDSL
052700030822     C*
052800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052900030822     C                   EVAL      posDaDft = 1
053000030822     C                   EVAL      posADft  = 0
053100030822     C                   EVAL      wGiroDft = 0
053200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053300030822     C                             posDaDft > 0
053400030822     C*
053500030822     C* Gestisco il 1° giro
053600030822     C                   IF        wGiroDft = 0
053700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053900030822     C* Incremento il contatore dei "giri"
054000030822     C                   EVAL      wGiroDft = 1
054100030822     C                   ELSE
054200030822     C                   EVAL      posDaDft = posADft
054300030822     C                   ENDIF
054400030822     C* Eseguo lo scan x trovare la fine del campo corrente
054500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054600030822     C*
054700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054900030822     C                   IF        posDaDft > 0 AND
055000030822     C                             posADft  > 0
055100030822     C* NCL
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'NCL'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiInt=*on
056100030822     C                   Z-ADD     PiVal         VABNCL
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* CCM
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'CCM'
056900030822     C                   EVAL      PiStr=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   EXSR      CHKNUM
057300030822     C                   IF        PiInt=*on
057400030822     C                   Z-ADD     PiVal         VABCCM
057500030822     C                   ENDIF
057600030822     C                   ENDIF
057700030822     C* LNP
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'LNP'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiInt=*on
058700030822     C                   Z-ADD     PiVal         VABLNP
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* NRS
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'NRS'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiInt=*on
060000030822     C                   Z-ADD     PiVal         VABNRS
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* CTR
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'CTR'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiInt=*on
061300030822     C                   Z-ADD     PiVal         VABCTR
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C* PKB
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'PKB'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiNum=*on
062600130610     C                   Z-ADD     PiVal         VABPKB
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* VLB
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'VLB'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiNum=*on
063900130610     C                   Z-ADD     PiVal         VABVLB
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* QFT
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'QFT'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiNum=*on
065200030822     C                   Z-ADD     PiVal         VABQFT
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* CBO
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'CBO'
066000030822     C                   EVAL      VABCBO=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* TSP
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'TSP'
066900030822     C                   EVAL      VABTSP=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* VAS
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'VAS'
067800030822     C                   EVAL      VABVAS=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* VCA
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'VCA'
068700030822     C                   EVAL      VABVCA=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100070925     C* TIC
069200070925     C                   IF        %subst(
069300070925     C                             %subst(vlrppt:posDaDft+1:
069400070925     C                             posADft-posDaDft-1):1:3)
069500070925     C                             = 'TIC'
069600070925     C                   EVAL      VABTIC=%trim(%subst(
069700070925     C                             %subst(vlrppt:posDaDft+1:
069800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900070925     C                   ENDIF
070000030822     C* GCA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'GCA'
070500030822     C                   EVAL      VABGCA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* CTM
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'CTM'
071400030822     C                   EVAL      VABCTM=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* FFD
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'FFD'
072300030822     C                   EVAL      VABFFD=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* VAD
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'VAD'
073200030822     C                   EVAL      VABVAD=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* GMA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GMA'
074100030822     C                   EVAL      VABGMA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* GGA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GGA'
075000030822     C                   EVAL      VABGGA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GVA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GVA'
075900030822     C                   EVAL      VABGVA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* TC1
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'TC1'
076800030822     C                   EVAL      VABTC1=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* TC2
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'TC2'
077700030822     C                   EVAL      VABTC2=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100040714     C* VATTRC
078200040714     C                   IF        %subst(
078300040714     C                             %subst(vlrppt:posDaDft+1:
078400040714     C                             posADft-posDaDft-1):1:3)
078500040714     C                             = 'TRC'
078600040714     C                   EVAL      VATTRC=%trim(%subst(
078700040714     C                             %subst(vlrppt:posDaDft+1:
078800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900040714     C                   ENDIF
079000030822     C* ...
079100030822     C                   ENDIF
079200030822     C                   ENDDO
079300020204     C*
079400010330     C                   ENDSR
079500010607     C*----------------------------------------------------*
079600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079700010607     C*----------------------------------------------------*
079800010607     C     IMPVAB        BEGSR
079900010607     C*
080000010607     C                   SETOFF                                       3132
080100110315 xxx C***                SETON                                        35
080200030515     C*
080300010607     C                   MOVEL     datcor        VABAAS
080400010607     C                   MOVE      datcor        VABMGS
080500010607     C*
080600030715     C**********
080700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080800030715     C**********
080900030715     C                   IF        wGiro = *zeros
081000040113     C*
081100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081400080429     C                             %trim(intcol(5))<>*blanks
081500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081700080429     C                                      %trim(intcol(5))
081800070823     C     ';':CharCSV   XLATE     vindta        vindta
081900040510     C                   ENDIF
082000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082100031201     C     minu:maiu     XLATE     vindta        vindta
082200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082300030715     C                   DOW       posDa <= %len(%trim(vindta))
082400030715     C*
082500030715     C* Gestisco il 1° campo
082600030715     C                   IF        i = 1
082700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082800030715     C                   EVAL      posDa = 1
082900030715     C* Eseguo lo scan x trovare la fine del primo campo
083000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083100030715     C* A questo punto "estraggo" il campo corrente
083200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083300030715     C                                                    (posA-posDa))
083400030715     C* X i campi successivi al 1°
083500030715     C                   ELSE
083600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083700030715     C                   EVAL      posDa = posA + 1
083800030715     C* Eseguo lo scan x trovare la fine del campo corrente
083900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084000030715     C* Gestisco l'ultimo campo
084100030715     C                   IF        posA = *zeros
084200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C                   LEAVE
084700030715     C                   ELSE
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C                   ENDIF
085200030715     C                   ENDIF
085300030715     C* Incremento il contatore d campo
085400030715     C                   EVAL      i = i +1
085500030715     C                   ENDDO
085600030715     C*
085700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085800030715     C* delimitazione testo
085900030715     C                   EVAL      i = 1
086000030715     C                   DOW       i < %elem(SkSplitFLD)
086100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086300030715     C                   EVAL      i = i + 1
086400030715     C                   ENDDO
086500030715     C                   EVAL      wGiro = 1
086600030715     C                   ELSE
086700030715     C                   EVAL      wGiro = 2
086800031204     C**********
086900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087000031204     C* d separatore campo
087100031204     C**********
087200031204     C                   EVAL      vindta = %trim(vindta)
087300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087400031204     C                   EVAL      vindta = %subst(vindta:2)
087500031204     C                   ENDDO
087600031204     C*
087700031204     C                   Z-ADD     *zeros        lunghInput        4 0
087800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088200110912     C                   ENDDO
088300030715     C**********
088400030715     C* Effettuo lo split del campo dati d input
088500030715     C**********
088600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088700030715     C                   DOW       posDa <= %len(%trim(vindta))
088800030715     C*
088900030715     C* Gestisco il 1° campo
089000030715     C                   IF        i = 1
089100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089200030715     C                   EVAL      posDa = 1
089300030715     C* Eseguo lo scan x trovare la fine del primo campo
089400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C* X i campi successivi al 1°
089900030715     C                   ELSE
090000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090100030715     C                   EVAL      posDa = posA + 1
090200031204     C* Verifico che nn vi sia il campo nn valorizzato
090300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090400031204     C* Se campo nn valorizzato skippo al prossimo
090500031204     C                   EVAL      SkSplitCSV(i) = *blanks
090600031204     C                   EVAL      posA  = posA + 1
090700031204     C                   ELSE
090800030715     C* Eseguo lo scan x trovare la fine del campo corrente
090900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091000030715     C* Gestisco l'ultimo campo
091100030715     C                   IF        posA = *zeros
091200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   LEAVE
091700030715     C                   ELSE
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C                   ENDIF
092200031204     C                   ENDIF
092300030715     C                   ENDIF
092400030715     C* Incremento il contatore d campo
092500030715     C                   EVAL      i = i +1
092600030715     C                   ENDDO
092700030715     C*
092800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092900030715     C* delimitazione testo
093000030715     C                   EVAL      i = 1
093100030715     C                   DOW       i < %elem(SkSplitCSV)
093200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093400030715     C                   EVAL      i = i + 1
093500030715     C                   ENDDO
093600100720     C*
093700100720     C* Stacco un progressivo x numero spedizione
093800130917 xxx C                   EXSR      repNSP
093900100326     C*
094000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094100030715     C                   EVAL      i = 1
094200030715     C                   DOW       i < %elem(SkSplitFLD)
094300030715     C*
094400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094600030715     C                             SkSplitCSV(i) <> *blanks
094700030721     C*** ATB
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094900030721     C                   EVAL      VABATB = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030715     C*** RSD
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095600030715     C* ==
095700130528     C                   ENDIF
095800030721     C*** RD2
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030715     C*** IND
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096400030715     C                   EVAL      VABIND = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** LOD
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** PRD
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030721     C*** NZD
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030715     C*** RMA
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** NOT
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** NT2
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** VCA
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** VAS
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** FFD
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030721     C*** GC1
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** GC2
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** TSP
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101300030721     C                   ENDIF
101400040510     C*** CBO
101500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101700040510     C                   ENDIF
101800030721     C*** NAS
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** TIC
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** GCA
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** XCO
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** CTM
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** TCR
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** CTS
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** FTM
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** VAD
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** GMA
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GGA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GVA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** TC1
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** TC2
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** SCL
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** RMO
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** NMO
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*
108700030721     C* Reperisco quindi i campi numerici...
108800040510     C*** NSP
108900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109000040510     C                   EVAL      PiStr=SkSplitCSV(i)
109100040510     C                   EXSR      CHKNUM
109200040510     C                   IF        PiInt=*on
109300040510     C                   Z-ADD     PiVal         VABNSP
109400040510     C                   ELSE
109500040510     C                   SETON                                        31
109600040510     C                   EVAL      VABNSP = *zeros
109700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109800040510     C                             + ' ' + 'VABNSP'
109900040510     C                   ENDIF
110000040510     C                   ENDIF
110100071106     C*** RMN
110200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110300071106     C                   EVAL      PiStr=SkSplitCSV(i)
110400071106     C                   EXSR      CHKNUM
110500071106     C                   IF        PiInt=*on
110600071106     C                   Z-ADD     PiVal         VABRMN
110700071106     C                   ELSE
110800071106     C                   SETON                                        32
110900071106     C                   EVAL      VABRMN = *zeros
111000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111100071106     C                             + ' ' + 'VABRMN'
111200071106     C                   ENDIF
111300071106     C                   ENDIF
111400030721     C*** CAD
111500130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111600130610     C                   EVAL      PiStr=SkSplitCSV(i)
111700130610     C                   EXSR      CHKNUM
111800130610     C                   IF        PiInt=*on
111900130610     C                   Z-ADD     PiVal         Num5_0
112000130610     C                   MOVEL(p)  Num5_0        VABCAD
112100130610     C                   ELSE
112200130610     C                   SETON                                        32
112300130610     C                   EVAL      VABCAD = *zeros
112400130610     C                   EVAL      vinmsg = %trimr(vinmsg)
112500130610     C                             + ' ' + 'VABCAD'
112600130610     C                   ENDIF
112700130605     C                   ENDIF
112800030721     C*** NCL
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on
113300030721     C                   Z-ADD     PiVal         VABNCL
113400030721     C                   ELSE
113500030721     C                   SETON                                        32
113600030721     C                   EVAL      VABNCL = *zeros
113700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113800040122     C                             + ' ' + 'VABNCL'
113900030721     C                   ENDIF
114000030721     C                   ENDIF
114100030721     C*** PKB
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114300030721     C                   EVAL      PiStr=SkSplitCSV(i)
114400030721     C                   EXSR      CHKNUM
114500030721     C                   IF        PiNum=*on
114600130610     C                   Z-ADD(H)  PiVal         VABPKB
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      VABPKB = *zeros
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABPKB'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** CAS
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115600110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115700111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115800111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
115900130703     C                   EVAL      VABVCA = 'EUR'
116000101018     C                   ENDIF
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiNum=*on
116400030721     C                   Z-ADD     PiVal         VABCAS
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABCAS = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900030721     C                             + ' ' + 'VABCAS'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** CCM
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiInt=*on      AND
117700030721     C                             PiVal<=9999999 AND
117800030721     C                             PiVal>*zeros
117900030721     C                   Z-ADD     PiVal         VABCCM
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      VABCCM = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400040510     C                             + ' ' + 'VABCCM'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** LNP
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on    AND
119200030721     C                             PiVal<=999   AND
119300030721     C                             PiVal>*zeros
119400030721     C                   Z-ADD     PiVal         VABLNP
119500030721     C                   ELSE
119600040715     C                   SETON                                        32
119700030721     C                   EVAL      VABLNP = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040510     C                             + ' ' + 'VABLNP'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** NRS
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on AND
120700030721     C                             PiVal<=99
120800030721     C                   Z-ADD     PiVal         VABNRS
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABNRS'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** LNA
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on    AND
122000030721     C                             PiVal<=999   AND
122100030721     C                             PiVal>*zeros
122200030721     C                   Z-ADD     PiVal         VABLNA
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      VABLNA = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABLNA'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** CTR
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on  AND
123500030721     C                             PiVal<=999
123600030721     C                   Z-ADD     PiVal         VABCTR
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABCTR'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** IAS
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiNum=*on
124800030721     C                   Z-ADD     PiVal         VABIAS
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABIAS'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** VLB
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiNum=*on
126000130610     C                   Z-ADD(H)  PiVal         VABVLB
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABVLB'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** QFT
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiNum=*on
127200030721     C                   Z-ADD     PiVal         VABQFT
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABQFT'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** NCD
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on      AND
128400030721     C                             PiVal<=9999999
128500030721     C                   Z-ADD     PiVal         VABNCD
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABNCD'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** NCA
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on      AND
129700030721     C                             PiVal<=9999999
129800030721     C                   Z-ADD     PiVal         VABNCA
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABNCA'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** ZNC
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on AND
131000030721     C                             PiVal<=99
131100030721     C                   Z-ADD     PiVal         VABZNC
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABZNC'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** DCR
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiInt=*on       AND
132300030721     C                             PiVal<=99999999
132400030721     C                   Z-ADD     PiVal         VABDCR
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800030721     C                             + ' ' + 'VABDCR'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100030721     C*** HCR
133200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133300030721     C                   EVAL      PiStr=SkSplitCSV(i)
133400030721     C                   EXSR      CHKNUM
133500030721     C                   IF        PiInt=*on   AND
133600030721     C                             PiVal<=9999
133700030721     C                   Z-ADD     PiVal         VABHCR
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABHCR'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** VMD
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiNum=*on
134900030721     C                   Z-ADD     PiVal         VABVMD
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABVMD'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** ANT
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on        AND
136100030721     C                             PiVal<=999999999
136200030721     C                   Z-ADD     PiVal         VABANT
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABANT'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** CMO
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on
137400030721     C                   Z-ADD     PiVal         Num5_0
137500030721     C                   MOVEL(p)  Num5_0        VABCMO
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      VABCMO = *zeros
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABCMO'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300040510     C*
138400040510     C* Reperisco i campi del FIVAT
138500040510     C*** VATNOT
138600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138800040510     C                   ENDIF
138900040510     C*** VATTRC
139000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139200040510     C                   ENDIF
139300060331     C*** VATNOT_A
139400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139600060331     C                   ENDIF
139700060331     C*** VATNOT_B
139800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140000060331     C                   ENDIF
140100060331     C*** VATNOT_E
140200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140400060331     C                   ENDIF
140500110927     C*** VATNOT_I
140600110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140700110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140800110927     C                   ENDIF
140900070823     C*
141000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
141100120301
141200140220      *** W_VABNOT: è sia VABNOT che VABNT2
141300140220     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
141400140220     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
141500140220     C                   EVAL      VABNOT =
141600140220     C                              %subst(wNOT:1:35)
141700140220     C                   EVAL      VABNT2 =
141800140220     C                              %subst(wNOT:36:35)
141900140220     C                   ENDIF
142000130626
142100070925     C*
142200071029     C***  <===  -----------------------------------------
142300040510     C*
142400030715     C                   ENDIF
142500030715     C                   EVAL      i = i + 1
142600030715     C                   ENDDO
142700070925     C*
142800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142900140220     C*
143000140305     C* VATNOT_A = VATNOT_B
143100140305     C                   EVAL      wVATNOT_A = wVATNOT_B
143200140220     C*
143300140305     C* se la RSD è vuoto, allora lo valorizzo con VATNOT_B e svuoto questo
143400140305     C                   IF        VABRSD = *blanks
143500140305     C                   EVAL      VABRSD = wVATNOT_B
143600140305     C                   EVAL      wVATNOT_B = *blank
143700140220     C                   ENDIF
143800140305     C*
143900140305     C* se IAS valorizzato, valorizzo a EUR la valuta
144000140305     C                   IF        vabIAS > *zeros
144100140305     C                   EVAL      vabVAS = 'EUR'
144200140305     C                   ENDIF
144300070925     C*
144400071029     C***  <===  -----------------------------------------
144500070925     C*
144600070925     C* Considerazioni finali su CBO/CAS
144700140220     C                   IF        wFlgCAS = '1'    OR
144800140220     C                             VABCAS  > *zeros
144900140220     C                   IF        VABCBO = '1'
145000140220     C                   EVAL      VABCBO = '4'
145100140220     C                   ENDIF
145200140220     C                   IF        VABCBO = '2'
145300140220     C                   EVAL      VABCBO = '6'
145400140220     C                   ENDIF
145500140220     C                   ENDIF
145600070925     C***  <===  ----------------------------
145700040510     C*
145800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145900040510     C                   IF        VABNSP = *zeros
146000040510     C                   SETON                                        31
146100040510     C                   ENDIF
146200040715     C*
146300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146400040715     C                   IF        VABLNP = *zeros
146500040715     C                   SETON                                        31
146600040715     C                   ENDIF
146700040510     C*
146800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146900120411     C                   IF        VABRMN = *zeros
147000120411     C                   Z-ADD     VABNSP        VABRMN
147100120411     C                   ENDIF
147200120328     C* IN QUESTO CASO LO FORZO = 1
147300120411     C***                IF        VABRMN = *zeros
147400120411     C***                Z-ADD     1             VABRMN
147500120411     C***                ENDIF
147600040510     C*
147700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147800040510     C                   IF        VABRMA = *blanks
147900040714     C                   MOVEL     VABRMN        VABRMA
148000040510     C                   ENDIF
148100040128     C*
148200040510     C* Se provincia nn valorizzata la reperisco
148300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148400110912     C                   IF        VABPRD =  *blanks AND
148500110912     C                             VABNZD =  *blanks AND
148600110912     C                             VABCAD <> *blanks AND
148700110912     C                             VABLOD <> *blanks
148800040128     C                   CLEAR                   TISI95DS
148900040128     C                   EVAL      I95TCN = '3'
149000040128     C                   Z-ADD     datcor        I95DAT
149100040128     C                   EVAL      I95NAR = VABNZD
149200040128     C                   EVAL      I95CAP = VABCAD
149300040128     C                   EVAL      I95LOC = VABLOD
149400040128     C                   CALL      'TISI95R'
149500040128     C                   PARM                    TISI95DS
149600040128     C                   EVAL      VABPRD = O95PRV
149700040510     C                   ENDIF
149800020204     C*
149900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
150000020204     C                   EXSR      CHKIMPDIV
150100030715     C                   ENDIF
150200020204     C*
150300010607     C                   ENDSR
150400010607     C*----------------------------------------------------*
150500010601
150600020204
150700020204     C*----------------------------------------------------*
150800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150900020204     C*----------------------------------------------------*
151000020204     C     CHKIMPDIV     BEGSR
151100020204     C*
151200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
151300020204     C                   Z-ADD     *zeros        wrkDec            9 9
151400020204     C*
151500020204     C* Come prima cosa effettuo considerazioni sulla divisa
151600020204     C                   IF        vabIAS > *zeros
151700020204     C                   IF        vabVAS <> 'EUR'
151800020204     C                   EVAL      vabVAS =  'ITL'
151900020204     C                   ENDIF
152000020204     C                   ENDIF
152100020204     C*
152200020204     C                   IF        vabCAS > *zeros
152300020204     C                   IF        vabVCA <> 'EUR'
152400020204     C                   EVAL      vabVCA =  'ITL'
152500020204     C                   ENDIF
152600020204     C                   ENDIF
152700020204     C*
152800020204     C                   IF        vabVMD > *zeros
152900020204     C                   IF        vabVAD <> 'EUR'
153000020204     C                   EVAL      vabVAD =  'ITL'
153100020204     C                   ENDIF
153200020204     C                   ENDIF
153300020204     C*
153400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153500020204     C                   Z-ADD     vabIAS        wrkDec
153600020204     C                   IF        wrkDec > *zeros
153700020204     C                   IF        vabVAS = 'ITL'
153800020204     C                   EVAL      vabIAS = *zeros
153900020204     C                   ENDIF
154000020204     C                   ENDIF
154100020204     C*
154200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
154300020204     C                   Z-ADD     vabCAS        wrkDec
154400020204     C                   IF        wrkDec > *zeros
154500020204     C                   IF        vabVCA = 'ITL'
154600020204     C                   EVAL      vabCAS = *zeros
154700020204     C                   ENDIF
154800020204     C                   ENDIF
154900020204     C*
155000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
155100020204     C                   Z-ADD     vabVMD        wrkDec
155200020204     C                   IF        wrkDec > *zeros
155300020204     C                   IF        vabVAD = 'ITL'
155400020204     C                   EVAL      vabVMD = *zeros
155500020204     C                   ENDIF
155600020204     C                   ENDIF
155700020204     C*
155800020204     C                   ENDSR
155900020204     C***
156000010330
156100100318
156200100318
156300100318     C*----------------------------------------------------*
156400100318     C*  CONTROLLO NUMERICITA' CAMPI
156500100318     C*----------------------------------------------------*
156600100318     C     CHKNUM        BEGSR
156700100318     C*
156800100318     C                   IF        PiDecChr = *blanks
156900100318     C                   EVAL      PiDecChr = CharNUM
157000100318     C                   ENDIF
157100100318     C*
157200100318     C                   callp     UBISNUM_Check(PiStr
157300100318     C                                          :PiDecChr
157400100318     C                                          :PiVal
157500100318     C                                          :PiNum
157600100318     C                                          :PiInt)
157700100318     C*
157800100318     C                   ENDSR
157900100318     C***
158000010601
158100010601
158200010601
158300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158400010601     C     repfil        BEGSR
158500010601     C*
158600010601     C                   if        invfil = *zeros and
158700010601     C                             depfil > *zeros and
158800010629     C                             (vinflg = *blanks or
158900010629     C                              vinflg = *zeros)
159000010601     C
159100010601     C                   eval      invfil = depfil
159200010601     C                   endif
159300010601     C*
159400010601     C                   if        depfil <> invfil and
159500010601     C                             invfil > *zeros
159600010601     C                   eval      flgMulti = '1'
159700010601     C                   if        vinflg = *blanks
159800010601     C                   add       1             cntNonEl
159900010601     C                   endif
160000010601     C                   endif
160100010601     C*
160200010601     C                   if        vinflg = '2'
160300010601     C                   eval      flgStato = '2'
160400010601     C                   endif
160500010601     C*
160600010601     C                   ENDSR
160700010601     C***
160800010601
160900010601
161000010601
161100010330
161200040119      /TITLE Invio dei dati al punto operativo.
161300040119     C     invio         BEGSR
161400040119     C*
161500040119     C* 1° invio FIVAT
161600040119     C                   reset                   dscmz
161700040119     C                   move      vlrpoi        cmzdst
161800040119     C                   eval      cmzfld = 'FIVATWWR'
161900040119     C                   eval      cmzmbd = vlrhdl
162000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162100040119     C***                if        prmfir = *blanks
162200040119     C                   eval      cmzfla = 'FIVAT00F'
162300040119     C                   eval      cmzmba = 'FIVAT00F'
162400040119     C***                else
162500040119     C***                eval      cmzfla = prmfir
162600040119     C***                eval      cmzmba = prmfir
162700040119     C***                endif
162800040119     C                   eval      cmznrr = *zeros
162900040119     C                   move      §ctrokvt      cmznrr
163000040119     C                   eval      cmzlba = vlrfl1
163100040119     C                   call(e)   'TIS711C'
163200040119     C                   parm                    dscmz
163300040119     C                   parm      *blanks       esito
163400040119     C                   if        %error
163500040119     C                             or cmzerr = '1'
163600040119     C                             or esito  = '1'
163700040119     C                   eval      wrkesito = '3'
163800040119     C                   else
163900040119     C*
164000040119     C* 2° invio FIVAB
164100040119     C                   reset                   dscmz
164200040119     C                   move      vlrpoi        cmzdst
164300040119     C                   eval      cmzfld = vlrfou
164400040119     C                   eval      cmzmbd = vlrhdl
164500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164600040119     C***                if        prmfir = *blanks
164700040119     C                   eval      cmzfla = 'FIVAB00F'
164800040119     C                   eval      cmzmba = 'FIVAB00F'
164900040119     C***                else
165000040119     C***                eval      cmzfla = prmfir
165100040119     C***                eval      cmzmba = prmfir
165200040119     C***                endif
165300040119     C                   eval      cmznrr = *zeros
165400040119     C                   move      §ctrokvb      cmznrr
165500040119     C                   eval      cmzlba = vlrfl1
165600040119     C                   call(e)   'TIS711C'
165700040119     C                   parm                    dscmz
165800040119     C                   parm      *blanks       esito
165900040119     C                   if        %error
166000040119     C                             or cmzerr = '1'
166100040119     C                             or esito  = '1'
166200040119     C                   eval      wrkesito = '3'
166300040119     C                   endif
166400040119     C                   endif
166500040119     C*
166600040119     C                   ENDSR
166700040119     C***
166800010601
166900010601
167000010601
167100010601
167200010601      /TITLE Invio dei dati al punto operativo.
167300010601     C     opeini        BEGSR
167400010601     C*
167500010601     C* Inizializzo flag e contatori operativi
167600010601     C                   movel     '0'           flgGiro           1
167700010601     C                   movel     '0'           flgMulti          1
167800010601     C                   movel     '1'           flgStato          1
167900010615     C                   movel     '0'           flgOk             1
168000010601     C                   z-add     *zeros        cntNonEl         10 0
168100010601     C                   z-add     *zeros        depfil            3 0
168200010601     C                   z-add     *zeros        invfil            3 0
168300010601     C*
168400010601     C                   ENDSR
168500010601     C***
168600070326
168700070326
168800070326
168900070326
169000070326     C     *pssr         BEGSR
169100070329     C*
169200070329     C                   if        %open(tivin00r)
169300070329     C                   close     tivin00r
169400070329     C                   endif
169500070329     C                   if        %open(fivabwwr)
169600070329     C                   close     fivabwwr
169700070329     C                   endif
169800070329     C                   if        %open(fivatwwr)
169900070329     C                   close     fivatwwr
170000070329     C                   endif
170100070326     C*
170200070326     C* Effettuo la chiamata al CLLE preposto
170300070326     C                   call(e)   'TITVVTC'
170400070326     C                   parm                    parccm
170500070326     C                   parm                    parmbr
170600070326     C                   parm      '2'           paropz
170700070326     C*
170800070326     C                   eval      wrkesito = '2'
170900070404     C*
171000070404     C                   seton                                        LR
171100070326     C*
171200070326     C                   ENDSR     '*CANCL'
171300070326     C***
171400100318
171500100318
171600100318
171700100318     C     normCSV       BEGSR
171800100318     C*
171900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
172000100318     C* separatori campi
172100100318     C                   if        CharTXT <> *blanks
172200100318     C                   z-add     *zeros        wLenVINDTA        4 0
172300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
172400100318     C                   setoff                                       80
172500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
172600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
172700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
172800100318     C                   if        not *in80
172900100318     C                   seton                                        80
173000100318     C                   else
173100100318     C                   setoff                                       80
173200100318     C                   endif
173300100318     C                   endif
173400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173500100318     C                             and *in80 = *on
173600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
173700100318     C                   endif
173800100318     C                   enddo
173900100318     C                   endif
174000100318     C*
174100100318     C                   ENDSR
174200100318     C***
174300070326
174400100326
174500100326
174600100326     C*----------------------------------------------------*
174700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
174800100326     C*----------------------------------------------------*
174900100326     C     REPNSP        BEGSR
175000100326     C*
175100100326     C* NSP => Stacco un numeratore da AZNUM
175200100326     C                   clear                   TRUL33DS
175300100326     C                   eval      I33OPE = *zeros
175400100326     C                   eval      I33CNU = 302
175500100326     C                   eval      I33NUM = 1
175600100326     C                   movel     TRUL33DS      KPJBU
175700100326     C                   call      'TRUL33R'
175800100326     C                   parm                    KPJBA
175900100326     C                   movel     KPJBU         TRUL33DS
176000100326     C                   if        O33ERR = *zeros
176100100326     C                   z-add     O33NRF        VABNSP
176200100326     C                   z-add     O33NRF        VATNSP
176300140305     C                   z-add     O33NRF        VABRMN
176400100326     C                   else
176500100326     C                   SETON                                        31
176600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
176700140305     C                             + ' ' + 'VABNSP VATNSP VABRMN'
176800100326     C                   endif
176900100326     C*
177000100326     C                   ENDSR
177100010330
177200100326
177300010330
177400000613     C     *inzsr        BEGSR
177500990910     C*
177600990910     C     *entry        plist
177700990920     C                   parm                    tivlrds
177800990921     C                   parm      wrkesito      esito
177900000724     C                   parm                    prmlit
178000000710     C                   parm                    prmfir
178100010330     C*
178200010330     C* CALCOLA LA DATA CORRENTE
178300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
178400100318     C                   eval      datcor = %dec(%date() : *ISO)
178500000613     C*
178600000613     C                   ENDSR
178700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178800140305VABRMA;VATNOT_B;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;W_VABNOT;VABTSP;VABIAS;
178900140305
179000130528
179100111116
179200100318
