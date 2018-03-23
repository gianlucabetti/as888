000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120604      *
000300120604      * PARTICOLARITA':
000400120604      * Il file è strutturato a una riga per collo ergo più righe per medesima spedizione; la
000500120604      * rottura di codice deve avvenire per campo cliente DDT
000600120604      * a standard si rompe per NSP, per ottenere lo stesso effetto impostiamo NSP = RMN.
000700120604      * I dati relativi a peso e volume vanno sommati al interno della stessa spedizione.
000800120604      * Il numero di colli invece è già espresso in maniera complessiva x spedizione.
000900120604      * Il contrassegno è già espresso in maniera complessiva x spedizione.
001000120604      * Non si sviluppano i CHI SONO perché vengono passati sul .csv.
001100120604
001200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001300120209     F*
001400990910     Ftivin00r  uF   E             DISK    usropn
001500100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001600040119     FFIVATwwr  O    E             DISK    usropn
001700000313     D*
001800040113     D*------------
001900040113     D* SCHIEDE A PROGRAMMA
002000040113     D*------------
002100080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002200010330     D*----------------------------------------------------
002300010330     D* DICHIARAZIOINE VARIABILI DI WRK
002400010330     D*----------------------------------------------------
002500010330     D dscmz         e ds                  inz
002600010330     D psds           sds
002700010330     D  procname         *PROC
002800010330     D tivlrds       e ds                  extname(tivlr00f)
002900040128     D tisi95ds      e ds
003000100413     D fivabds       e ds                  extname(fivab00f)
003100100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003200100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003300010330     D esito           s              1
003400010330     D prmlit          s             10
003500010330     D prmfir          s             10
003600010330     D wrkesito        s                   like(esito)
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800040510     D parccm          s              8    INZ(*blanks)
003900040119     D parmbr          s             10    INZ(*blanks)
004000040119     D paropz          s              1    INZ(*blanks)
004100040119     D chkcall         s              1    INZ(*blanks)
004200080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004300080418     D wFlgCAS         s              1    INZ(*blanks)
004400120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004500120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004600120604     D Intestazioni    s               n   inz(*on)
004700100326     D*------------------
004800100326     D* DS REPERIMENTO NUMERATORE
004900100326     D*------------------
005000100326     D trul33ds      e ds                  inz
005100100326     D*------------------
005200100326     D* DS ARCHITETTURA
005300100326     D*------------------
005400100326     D kpjba         e ds                  inz
005500100326
005600030715     D*------------------
005700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005800030715     D*------------------
005900030715     D SkSplitFLD      S             10    DIM(100)
006000030715     D SkSplitCSV      S            256    DIM(100)
006100030715     D CharCSV         S              1
006200030715     D CharTXT         S              1
006300030715     D CharNUM         S              1
006400100318     D CharSOS         S              1
006500030715     D posDa           S              3  0 INZ(*zeros)
006600030715     D posA            S              3  0 INZ(*zeros)
006700030715     D i               s              3  0 INZ(1)
006800030715     D wGiro           s              1  0 INZ(*zeros)
006900070823     D wVATNOT_A       s                   LIKE(VATNOT)
007000060331     D wVATNOT_B       s                   LIKE(VATNOT)
007100060331     D wVATNOT_E       s                   LIKE(VATNOT)
007200110927     D wVATNOT_I       s                   LIKE(VATNOT)
007300120411     D wNOT            s             70
007400120411     D Data8A          s              8
007500030822     D*------------------
007600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007700030822     D*------------------
007800030822     D posDaDft        S              3  0 INZ(*zeros)
007900030822     D posADft         S              3  0 INZ(*zeros)
008000030822     D j               s              3  0 INZ(1)
008100030822     D wGiroDft        s              1  0 INZ(*zeros)
008200080418     D*------------------
008300080418     D* Costanti
008400080418     D*------------------
008500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008700100318
008800100318     D*------------------
008900100318     D* LINKING A DEFINIZIONI ESTERNE
009000100318     D*------------------
009100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009300010330
009400000913     C                   reset                   rrnum
009500990921     C                   reset                   esito
009600990921     C                   reset                   wrkesito
009700010601     C*
009800010601     C                   exsr      opeini
009900010605     C                   exsr      rwvab
010000010601     C*
010100010601     C                   seton                                        lr
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010700010601     C*--------------------------------------------------------
010800010601     C     PREELA        BEGSR
010900010601     C*
011000010601     C* SE OCCORRE SPEDIRE IN FILIALE
011100010601     C                   if        invfil <> *zeros and
011200010601     C                             flgGiro = '0'
011300010601     C*
011400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011500010601     C                   eval      flgGiro = '1'
011600010601     C*
011700010601     C                   endif
011800010601     C*
011900010601     C                   ENDSR
012000010601     C***
012100010601
012200010601
012300010601
012400010601
012500010601     C*--------------------------------------------------------
012600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012700010601     C*--------------------------------------------------------
012800010601     C     ENDELA        BEGSR
012900040119     C*
013000040119     C* Effettuo la chiamata al CLLE preposto
013100040119     C                   call(e)   'TITVVTC'
013200040510     C                   parm                    parccm
013300040119     C                   parm                    parmbr
013400040119     C                   parm      '2'           paropz
013500050201     C*
013600050201     C* Effettuo lancio TISI95 solo x chiusura
013700050201     C                   CLEAR                   TISI95DS
013800050201     C                   EVAL      I95TLA = 'C'
013900050201     C                   CALL      'TISI95R'
014000050201     C                   PARM                    TISI95DS
014100000616     C*
014200010601     C                   ENDSR
014300010601
014400010601
014500010330     C*--------------------------------------------------------
014600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014700010330     C*--------------------------------------------------------
014800010605     C     RWVAB         BEGSR
014900010330     C*
015000010330     C                   if        not %open(tivin00r)
015100010330     C                   open      tivin00r
015200010330     C                   endif
015300021025     C                   if        not %open(fivabwwr)
015400021025     C                   open      fivabwwr
015500010330     C                   endif
015600040119     C*
015700040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015800040119     C                   exsr      prevat
015900040119     C*
016000040119     C                   if        chkcall = '0'
016100040119     C*
016200040119     C                   if        not %open(fivatwwr)
016300040119     C                   open      fivatwwr
016400040119     C                   endif
016500010330     C*
016600040119     C                   clear                   §CTROKVB          7 0
016700040119     C                   clear                   §CTROKVT          7 0
016800040119     C                   clear                   §CTRMO            7 0
016900040119     C                   clear                   §CTRNO            7 0
017000120410     C*
017100120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017200120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017300120410     C                   IF        Intestazioni = *off
017400120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017500120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017600120410     C                   exsr      inzvar
017700120410     C                   exsr      impvab
017800120410     C                   ENDIF
017900010330     C*
018000030822     C                   do        *HIVAL
018100010330     C*
018200030822     C                   read      tivin00r                               70
018300010618     C*
018400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018500010618     C                   if        vinflg = '1'
018600010618     C                   eval      flgOk = '1'
018700010618     C                   endif
018800040510     C*
018900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019000040510     C                   z-add     *zeros        wLenVINDTA        4 0
019100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019500040510     C                   z-add     1             wFlgVINDTA
019600040510     C                   leave
019700040510     C                   endif
019800040510     C                   enddo
019900010618     C*
020000040510     C                   if        vindta > *blanks AND
020100040510     C                             wFlgVINDTA = 1
020200010330     C                   add       1             rrnum
020300010330     C*
020400010601     C                   if        *in70 = *off and
020500010330     C                             (vinflg = *blanks
020600010330     C                              or vinflg = '0'
020700010330     C                              or vinflg = '2')
020800010330     C*
020900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021000010711     C                   if        vinflg = *blanks or vinflg = '0'
021100010711     C                   clear                   vinmsg
021200010711     C                   endif
021300010601     C*
021400070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021500070925     C                   eval      vindta  = ' ' + %trim(vindta)
021600070925     C*
021700080527     C                   exsr      inzvar
021800100318     C                   exsr      normCSV
021900010605     C                   exsr      impvab
022000010601     C*
022100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200010605     C                   eval      depfil = VABLNP
022300010601     C                   exsr      repfil
022400010601     C                   if        depfil = invfil
022500021025     C                   if        vlrpoi = 999
022600030822     C                   move(P)   invfil        VABFGS
022700021025     C                   else
022800030822     C                   move(P)   vlrpoi        VABFGS
022900021025     C                   endif
023000010601     C*
023100010601     C                   exsr      PREELA
023200010601     C*
023300010604     C* Ebbene...
023400010604     C*
023500040119     C  N31              add       1             §CTROKVB
023600040119     C   32              add       1             §CTRMO
023700040119     C   31              add       1             §CTRNO
023800080418     C*
023900030822     C                   if        wGiro = 2
024000080513     C* Scrivo sempre il VAT
024100080513     C  N31              exsr      wrivat
024200110201     C                   exsr      chkvab
024300080513     C                   endif
024400010604     C*
024500010604     C                   if        *in31 = *off and
024600010604     C                             *in32 = *off
024700010604     C                   eval      vinflg = '1'
024800010604     C                   else
024900010604     C                   eval      vinflg = '2'
025000010604     C                   endif
025100010604     C                   endif
025200010601     C*
025300010604     C                   endif
025400010604     C*
025500010330     C                   else
025600010330     C                   eval      vinflg = '1'
025700010330     C                   endif
025800010601     C*
025900010601     C  N70              update    tivin000
026000010330     C*
026100030822     C  N70              enddo
026200040119     C*
026300080527     C* Verifico dati rimasti "in cannna"...
026400080527     C                   seton                                        65
026500110201     C                   exsr      chkvab
026600080527     C*
026700040119     C                   endif                                                  (endif - chkcall)
026800010601     C*
026900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027000010601     C                   if        cntNonEl = *zeros or
027100010601     C                             flgMulti = '0'
027200010330     C* Se non ci sono record con errori ...
027300010601     C                   if        §ctrno = 0 and
027400010604     C                             §ctrmo = 0 and
027500010601     C                             flgStato <> '2'
027600010330     C* ... restituisco esito OK.
027700010330     C                   eval      wrkesito = '0'
027800010330     C                   else
027900040119     C                   if        §ctrokvb > 0
028000010330     C                   eval      wrkesito = '1'
028100010330     C                   else
028200010615     C                   if        flgOk = '0'
028300010615     C                   eval      wrkesito = '2'
028400010615     C                   else
028500010615     C                   eval      wrkesito = '6'
028600010615     C                   endif
028700010330     C                   endif
028800010330     C                   endif
028900010601     C                   else
029000010601     C                   eval      wrkesito = '9'
029100010601     C                   endif
029200010330     C*
029300010330     C                   if        %open(tivin00r)
029400010330     C                   close     tivin00r
029500010330     C                   endif
029600021025     C                   if        %open(fivabwwr)
029700021025     C                   close     fivabwwr
029800010330     C                   endif
029900040512     C                   if        %open(fivatwwr)
030000040512     C                   close     fivatwwr
030100040512     C                   endif
030200010601     C*
030300010601     C                   if        vlrpoi <> 999
030400010601     C                   eval      invfil = vlrpoi
030500010601     C                   endif
030600010330     C*
030700040119     C                   if        §ctrokvb > 0
030800010601     C                             and invfil > *zeros
030900010330     C                   exsr      invio
031000010330     C                   endif
031100010601     C*
031200010618     C                   if        flgGiro = '1'
031300010601     C                   exsr      endela
031400010618     C                   endif
031500010330     C*
031600010330     C                   ENDSR
031700010330     C***
031800080527
031900080527
032000080527
032100080527     C*----------------------------------------------------*
032200080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032300080527     C*----------------------------------------------------*
032400100413     C     CHKVAB        BEGSR
032500110201     C*
032600110201     C* Se richiesti ignorare colli / peso / volume
032700110315 xxx C***                if        *in35
032800110315 xxx C***                z-add     *zeros        VABNCL
032900110315 xxx C***                z-add     *zeros        VABPKB
033000110315 xxx C***                z-add     *zeros        VABVLB
033100110315 xxx C***                endif
033200080527     C*
033300080527     C* Scrivo il VAB solo a rottura d spedizione
033400080527     C                   if        depspe = *zeros
033500080527     C  N31              eval      fivabds_sav = fivabds
033600080527     C                   eval      depspe = vabNSP
033700080527     C                   else
033800080527     C                   if        depspe = vabNSP AND *in65 = *off
033900120604      * Il numero di colli invece è già espresso in maniera complessiva x spedizione.
034000120604     C***51              add       VABNCL        sav_VABNCL
034100120604     C   51              z-add     VABNCL        sav_VABNCL
034200100413     C   51              add       VABPKB        sav_VABPKB
034300100413     C   51              add       VABVLB        sav_VABVLB
034400100720     C   52              z-add     VABNCL        sav_VABNCL
034500100720     C   52              z-add     VABPKB        sav_VABPKB
034600100720     C   52              z-add     VABVLB        sav_VABVLB
034700100720     C   53              add       VABNCL        sav_VABNCL
034800100720     C   53              z-add     VABPKB        sav_VABPKB
034900100720     C   53              z-add     VABVLB        sav_VABVLB
035000080527     C                   else
035100080527     C  N31              eval      fivabds_ok = fivabds_sav
035200080527     C  N31              exsr      wrivab
035300080527     C  N31              eval      fivabds_sav = fivabds
035400080527     C                   eval      depspe = vabNSP
035500080527     C                   endif
035600080527     C                   endif
035700080527     C*
035800080527     C                   ENDSR
035900040119
036000040119
036100040119
036200040119     C*----------------------------------------------------*
036300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036400040119     C*----------------------------------------------------*
036500040510     C     WRIVAB        BEGSR
036600080513     C*
036700080513     C* Gestisco l'eventuale rottura x numero spedizione
036800080527     C                   if        VABNSP <> depspe or *in65 = *on
036900070301     C                   if        wVATNOT_E <> *blanks
037000070301     C                   eval      VABCTM = '7Q'
037100070301     C                   endif
037200100413     C                   write     fivab000
037300080513     C                   endif
037400040119     C*
037500040119     C                   ENDSR
037600040119
037700040119
037800040119
037900040119     C*----------------------------------------------------*
038000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038100040119     C*----------------------------------------------------*
038200040510     C     WRIVAT        BEGSR
038300060512     C*
038400060512     C* Inizializzo i campi chiave
038500060512     C                   eval      VATFGS = VABFGS
038600060512     C                   eval      VATAAS = VABAAS
038700060512     C                   eval      VATCCM = VABCCM
038800070823     C                   eval      VATNRS = VABNRS
038900060512     C                   eval      VATNSP = VABNSP
039000060512     C                   eval      VATLNP = VABLNP
039100040119     C*
039200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039300040510     C                   if        VATTRC = *blanks
039400040510     C                   eval      VATTRC = 'E'
039500040510     C                   endif
039600110318     C                   if        VATNOT <> *blanks
039700040510     C                   write     FIVAT000
039800040119     C                   add       1             §CTROKVT
039900110318     C                   endif
040000060331     C*
040100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040200060331     C                   if        wVATNOT_A <> *blanks
040300060331     C                   eval      VATNOT = wVATNOT_A
040400060331     C                   eval      VATTRC = 'A'
040500060331     C                   write     FIVAT000
040600060331     C                   add       1             §CTROKVT
040700060331     C                   endif
040800060331     C*
040900060331     C                   if        wVATNOT_B <> *blanks
041000060331     C                   eval      VATNOT = wVATNOT_B
041100060331     C                   eval      VATTRC = 'B'
041200060331     C                   write     FIVAT000
041300060331     C                   add       1             §CTROKVT
041400060331     C                   endif
041500060331     C*
041600060331     C                   if        wVATNOT_E <> *blanks
041700110201     C                   eval      VATNOT = wVATNOT_E
041800060331     C                   eval      VATTRC = 'E'
041900120604     C*
042000120604     C* Non devo sviluppare i "CHI SONO" perché sono scritti sul .csv
042100120604     C*
042200120604     C                   write     FIVAT000
042300120604     C                   add       1             §CTROKVT
042400120604     C                   endif
042500110927     C*
042600110927     C                   if        wVATNOT_I <> *blanks
042700110927     C                   eval      VATNOT = wVATNOT_I
042800110927     C                   eval      VATTRC = 'I'
042900110927     C                   write     FIVAT000
043000110927     C                   add       1             §CTROKVT
043100110927     C                   endif
043200040119     C*
043300040119     C                   ENDSR
043400040119
043500040119
043600040119
043700040119     C*----------------------------------------------------*
043800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043900040119     C*----------------------------------------------------*
044000040119     C     PREVAT        BEGSR
044100040119     C*
044200040119     C* Compongo il nome del membro da dare al FIVATWWR
044300040119     C                   eval      parmbr = vlrhdl
044400040119     C                   movel     'M'           parmbr
044500040510     C                   eval      parccm = vlrksc
044600040119     C                   eval      paropz = '1'
044700040119     C* Effettuo la chiamata al CLLE preposto
044800040119     C                   call(e)   'TITVVTC'
044900040119     C                   parm                    parccm
045000040119     C                   parm                    parmbr
045100040119     C                   parm                    paropz
045200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045300040119     C                   if        %error
045400040119     C                   movel     '1'           chkcall
045500040119     C                   else
045600040119     C                   movel     '0'           chkcall
045700040119     C                   endif
045800040119     C*
045900040119     C                   ENDSR
046000010601
046100010601
046200010601
046300010330     C*----------------------------------------------------*
046400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046500010330     C*----------------------------------------------------*
046600040119     C     INZVAR        BEGSR
046700080527     C*
046800080527     C                   setoff                                       65
046900010330     C*
047000030822     C* Inizializzo variabili di wrk
047100020204     C                   Z-ADD     *zeros        Num5_0            5 0
047200101018     C                   MOVEL     *blanks       wFlgCAS
047300060331     C                   MOVEL     *blanks       wVATNOT_A
047400060331     C                   MOVEL     *blanks       wVATNOT_B
047500060331     C                   MOVEL     *blanks       wVATNOT_E
047600110927     C                   MOVEL     *blanks       wVATNOT_I
047700120411     C                   MOVEL     *blanks       wNOT
047800120411     C                   MOVEL     *blanks       Data8A
047900030822     C*
048000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048100030822     C                   CLEAR                   SkSplitCSV
048200030822     C                   EVAL      i = 1
048300030822     C                   EVAL      posDa = *zeros
048400030822     C                   EVAL      posA  = *zeros
048500080527     C*
048600080527     C                   CLEAR                   FIVABDS
048700080527     C                   CLEAR                   FIVAT000
048800030822     C*
048900030822     C* Reimposto i valori di default
049000030822     C                   EXSR      DEFCAM
049100030822     C*
049200010330     C                   ENDSR
049300010330     C*----------------------------------------------------*
049400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049500010330     C*----------------------------------------------------*
049600010330     C     DEFCAM        BEGSR
049700030822     C*
049800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049900030715     C* e delimitatore testo.
050000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050300100318     C*
050400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050500100318     C                   EVAL      CharSOS = CharNUM
050600030822     C*
050700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050800100720     C                   SETOFF                                       505152
050900100720     C                   SETOFF                                       53
051000100413     C                   SELECT
051100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051200100720     C                   SETON                                        50
051300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051400100413     C                   SETON                                        51
051500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051600100720     C                   SETON                                        52
051700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051800100720     C                   SETON                                        53
051900100413     C                   ENDSL
052000030822     C*
052100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052200030822     C                   EVAL      posDaDft = 1
052300030822     C                   EVAL      posADft  = 0
052400030822     C                   EVAL      wGiroDft = 0
052500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052600030822     C                             posDaDft > 0
052700030822     C*
052800030822     C* Gestisco il 1° giro
052900030822     C                   IF        wGiroDft = 0
053000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053200030822     C* Incremento il contatore dei "giri"
053300030822     C                   EVAL      wGiroDft = 1
053400030822     C                   ELSE
053500030822     C                   EVAL      posDaDft = posADft
053600030822     C                   ENDIF
053700030822     C* Eseguo lo scan x trovare la fine del campo corrente
053800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053900030822     C*
054000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054200030822     C                   IF        posDaDft > 0 AND
054300030822     C                             posADft  > 0
054400030822     C* NCL
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'NCL'
054900030822     C                   EVAL      PiStr=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   EXSR      CHKNUM
055300030822     C                   IF        PiInt=*on
055400030822     C                   Z-ADD     PiVal         VABNCL
055500030822     C                   ENDIF
055600030822     C                   ENDIF
055700030822     C* CCM
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'CCM'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiInt=*on
056700030822     C                   Z-ADD     PiVal         VABCCM
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* LNP
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'LNP'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiInt=*on
058000030822     C                   Z-ADD     PiVal         VABLNP
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* NRS
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'NRS'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiInt=*on
059300030822     C                   Z-ADD     PiVal         VABNRS
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* CTR
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'CTR'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600030822     C                   Z-ADD     PiVal         VABCTR
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* PKB
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'PKB'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiNum=*on
061900120418     C                   Z-ADD     PiVal         VABPKB
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* VLB
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'VLB'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiNum=*on
063200030822     C                   Z-ADD     PiVal         VABVLB
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* QFT
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'QFT'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiNum=*on
064500030822     C                   Z-ADD     PiVal         VABQFT
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* CBO
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'CBO'
065300030822     C                   EVAL      VABCBO=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* TSP
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'TSP'
066200030822     C                   EVAL      VABTSP=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* VAS
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'VAS'
067100030822     C                   EVAL      VABVAS=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* VCA
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'VCA'
068000030822     C                   EVAL      VABVCA=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400070925     C* TIC
068500070925     C                   IF        %subst(
068600070925     C                             %subst(vlrppt:posDaDft+1:
068700070925     C                             posADft-posDaDft-1):1:3)
068800070925     C                             = 'TIC'
068900070925     C                   EVAL      VABTIC=%trim(%subst(
069000070925     C                             %subst(vlrppt:posDaDft+1:
069100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200070925     C                   ENDIF
069300030822     C* GCA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'GCA'
069800030822     C                   EVAL      VABGCA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* CTM
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'CTM'
070700030822     C                   EVAL      VABCTM=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* FFD
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'FFD'
071600030822     C                   EVAL      VABFFD=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* VAD
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'VAD'
072500030822     C                   EVAL      VABVAD=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* GMA
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'GMA'
073400030822     C                   EVAL      VABGMA=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* GGA
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'GGA'
074300030822     C                   EVAL      VABGGA=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* GVA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'GVA'
075200030822     C                   EVAL      VABGVA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* TC1
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'TC1'
076100030822     C                   EVAL      VABTC1=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* TC2
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'TC2'
077000030822     C                   EVAL      VABTC2=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400040714     C* VATTRC
077500040714     C                   IF        %subst(
077600040714     C                             %subst(vlrppt:posDaDft+1:
077700040714     C                             posADft-posDaDft-1):1:3)
077800040714     C                             = 'TRC'
077900040714     C                   EVAL      VATTRC=%trim(%subst(
078000040714     C                             %subst(vlrppt:posDaDft+1:
078100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200040714     C                   ENDIF
078300030822     C* ...
078400030822     C                   ENDIF
078500030822     C                   ENDDO
078600020204     C*
078700010330     C                   ENDSR
078800010607     C*----------------------------------------------------*
078900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079000010607     C*----------------------------------------------------*
079100010607     C     IMPVAB        BEGSR
079200010607     C*
079300010607     C                   SETOFF                                       3132
079400110315 xxx C***                SETON                                        35
079500030515     C*
079600010607     C                   MOVEL     datcor        VABAAS
079700010607     C                   MOVE      datcor        VABMGS
079800010607     C*
079900030715     C**********
080000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080100030715     C**********
080200030715     C                   IF        wGiro = *zeros
080300040113     C*
080400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080700080429     C                             %trim(intcol(5))<>*blanks
080800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081000080429     C                                      %trim(intcol(5))
081100070823     C     ';':CharCSV   XLATE     vindta        vindta
081200040510     C                   ENDIF
081300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081400031201     C     minu:maiu     XLATE     vindta        vindta
081500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081600030715     C                   DOW       posDa <= %len(%trim(vindta))
081700030715     C*
081800030715     C* Gestisco il 1° campo
081900030715     C                   IF        i = 1
082000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082100030715     C                   EVAL      posDa = 1
082200030715     C* Eseguo lo scan x trovare la fine del primo campo
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* A questo punto "estraggo" il campo corrente
082500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082600030715     C                                                    (posA-posDa))
082700030715     C* X i campi successivi al 1°
082800030715     C                   ELSE
082900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083000030715     C                   EVAL      posDa = posA + 1
083100030715     C* Eseguo lo scan x trovare la fine del campo corrente
083200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083300030715     C* Gestisco l'ultimo campo
083400030715     C                   IF        posA = *zeros
083500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C                   LEAVE
084000030715     C                   ELSE
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   ENDIF
084500030715     C                   ENDIF
084600030715     C* Incremento il contatore d campo
084700030715     C                   EVAL      i = i +1
084800030715     C                   ENDDO
084900030715     C*
085000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085100030715     C* delimitazione testo
085200030715     C                   EVAL      i = 1
085300030715     C                   DOW       i < %elem(SkSplitFLD)
085400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085600030715     C                   EVAL      i = i + 1
085700030715     C                   ENDDO
085800030715     C                   EVAL      wGiro = 1
085900030715     C                   ELSE
086000030715     C                   EVAL      wGiro = 2
086100031204     C**********
086200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086300031204     C* d separatore campo
086400031204     C**********
086500031204     C                   EVAL      vindta = %trim(vindta)
086600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086700031204     C                   EVAL      vindta = %subst(vindta:2)
086800031204     C                   ENDDO
086900031204     C*
087000031204     C                   Z-ADD     *zeros        lunghInput        4 0
087100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500110912     C                   ENDDO
087600030715     C**********
087700030715     C* Effettuo lo split del campo dati d input
087800030715     C**********
087900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088000030715     C                   DOW       posDa <= %len(%trim(vindta))
088100030715     C*
088200030715     C* Gestisco il 1° campo
088300030715     C                   IF        i = 1
088400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088500030715     C                   EVAL      posDa = 1
088600030715     C* Eseguo lo scan x trovare la fine del primo campo
088700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C* X i campi successivi al 1°
089200030715     C                   ELSE
089300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089400030715     C                   EVAL      posDa = posA + 1
089500031204     C* Verifico che nn vi sia il campo nn valorizzato
089600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089700031204     C* Se campo nn valorizzato skippo al prossimo
089800031204     C                   EVAL      SkSplitCSV(i) = *blanks
089900031204     C                   EVAL      posA  = posA + 1
090000031204     C                   ELSE
090100030715     C* Eseguo lo scan x trovare la fine del campo corrente
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* Gestisco l'ultimo campo
090400030715     C                   IF        posA = *zeros
090500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   LEAVE
091000030715     C                   ELSE
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   ENDIF
091500031204     C                   ENDIF
091600030715     C                   ENDIF
091700030715     C* Incremento il contatore d campo
091800030715     C                   EVAL      i = i +1
091900030715     C                   ENDDO
092000030715     C*
092100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092200030715     C* delimitazione testo
092300030715     C                   EVAL      i = 1
092400030715     C                   DOW       i < %elem(SkSplitCSV)
092500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092700030715     C                   EVAL      i = i + 1
092800030715     C                   ENDDO
092900100720     C*
093000120604     C* NON Stacco un progressivo x numero spedizione, in quanto NSP = RMN
093100120604 xxx C***                EXSR      repNSP
093200100326     C*
093300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093400030715     C                   EVAL      i = 1
093500030715     C                   DOW       i < %elem(SkSplitFLD)
093600030715     C*
093700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093900030715     C                             SkSplitCSV(i) <> *blanks
094000030721     C*** ATB
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094200030721     C                   EVAL      VABATB = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030715     C*** RSD
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094900030715     C* ==
095000030715     C                   ENDIF
095100030721     C*** RD2
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030715     C*** IND
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095700030715     C                   EVAL      VABIND = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** LOD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** PRD
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030721     C*** NZD
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030715     C*** RMA
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** NOT
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** NT2
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** VCA
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** VAS
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** FFD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030721     C*** GC1
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** GC2
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** TSP
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100600030721     C                   ENDIF
100700040510     C*** CBO
100800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101000040510     C                   ENDIF
101100030721     C*** NAS
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** TIC
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** GCA
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** XCO
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** CTM
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** TCR
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** CTS
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** FTM
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** VAD
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** GMA
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GGA
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GVA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TC1
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** TC2
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** SCL
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** RMO
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** NMO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*
108000030721     C* Reperisco quindi i campi numerici...
108100040510     C*** NSP
108200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108300040510     C                   EVAL      PiStr=SkSplitCSV(i)
108400040510     C                   EXSR      CHKNUM
108500040510     C                   IF        PiInt=*on
108600040510     C                   Z-ADD     PiVal         VABNSP
108700040510     C                   ELSE
108800040510     C                   SETON                                        31
108900040510     C                   EVAL      VABNSP = *zeros
109000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109100040510     C                             + ' ' + 'VABNSP'
109200040510     C                   ENDIF
109300040510     C                   ENDIF
109400071106     C*** RMN
109500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109600071106     C                   EVAL      PiStr=SkSplitCSV(i)
109700071106     C                   EXSR      CHKNUM
109800071106     C                   IF        PiInt=*on
109900071106     C                   Z-ADD     PiVal         VABRMN
110000071106     C                   ELSE
110100071106     C                   SETON                                        32
110200071106     C                   EVAL      VABRMN = *zeros
110300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110400071106     C                             + ' ' + 'VABRMN'
110500071106     C                   ENDIF
110600071106     C                   ENDIF
110700030721     C*** CAD
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on
111200030721     C                   Z-ADD     PiVal         Num5_0
111300030721     C                   MOVEL(p)  Num5_0        VABCAD
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABCAD = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABCAD'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** NCL
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on
112600030721     C                   Z-ADD     PiVal         VABNCL
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABNCL = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040122     C                             + ' ' + 'VABNCL'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** PKB
113500120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
113600120418     C* per cui si arrotonda il valore passato agli etti
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiNum=*on
114100120418     C                   Z-ADD(H)  PiVal         VABPKB
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      VABPKB = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABPKB'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** CAS
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115100110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115200111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115300111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
115400120605     C                   EVAL      VABVCA = 'EUR'
115500101018     C                   EVAL      wFlgCAS = '1'
115600101018     C                   ENDIF
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiNum=*on
116000030721     C                   Z-ADD     PiVal         VABCAS
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABCAS = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABCAS'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** CCM
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on      AND
117300030721     C                             PiVal<=9999999 AND
117400030721     C                             PiVal>*zeros
117500030721     C                   Z-ADD     PiVal         VABCCM
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABCCM = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000040510     C                             + ' ' + 'VABCCM'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** LNP
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiInt=*on    AND
118800030721     C                             PiVal<=999   AND
118900030721     C                             PiVal>*zeros
119000030721     C                   Z-ADD     PiVal         VABLNP
119100030721     C                   ELSE
119200040715     C                   SETON                                        32
119300030721     C                   EVAL      VABLNP = *zeros
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500040510     C                             + ' ' + 'VABLNP'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** NRS
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on AND
120300030721     C                             PiVal<=99
120400030721     C                   Z-ADD     PiVal         VABNRS
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABNRS'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** LNA
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on    AND
121600030721     C                             PiVal<=999   AND
121700030721     C                             PiVal>*zeros
121800030721     C                   Z-ADD     PiVal         VABLNA
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABLNA = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABLNA'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** CTR
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on  AND
123100030721     C                             PiVal<=999
123200030721     C                   Z-ADD     PiVal         VABCTR
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABCTR'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** IAS
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiNum=*on
124400030721     C                   Z-ADD     PiVal         VABIAS
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABIAS'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** VLB
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiNum=*on
125600030721     C                   Z-ADD     PiVal         VABVLB
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABVLB'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** QFT
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiNum=*on
126800030721     C                   Z-ADD     PiVal         VABQFT
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABQFT'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** NCD
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on      AND
128000030721     C                             PiVal<=9999999
128100030721     C                   Z-ADD     PiVal         VABNCD
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABNCD'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** NCA
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on      AND
129300030721     C                             PiVal<=9999999
129400030721     C                   Z-ADD     PiVal         VABNCA
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABNCA'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** ZNC
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on AND
130600030721     C                             PiVal<=99
130700030721     C                   Z-ADD     PiVal         VABZNC
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABZNC'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** DCR
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on       AND
131900030721     C                             PiVal<=99999999
132000030721     C                   Z-ADD     PiVal         VABDCR
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABDCR'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** HCR
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on   AND
133200030721     C                             PiVal<=9999
133300030721     C                   Z-ADD     PiVal         VABHCR
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABHCR'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** VMD
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiNum=*on
134500030721     C                   Z-ADD     PiVal         VABVMD
134600030721     C                   ELSE
134700030721     C                   SETON                                        32
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABVMD'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200030721     C*** ANT
135300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiInt=*on        AND
135700030721     C                             PiVal<=999999999
135800030721     C                   Z-ADD     PiVal         VABANT
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABANT'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** CMO
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on
137000030721     C                   Z-ADD     PiVal         Num5_0
137100030721     C                   MOVEL(p)  Num5_0        VABCMO
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      VABCMO = *zeros
137500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137600030721     C                             + ' ' + 'VABCMO'
137700030721     C                   ENDIF
137800030721     C                   ENDIF
137900040510     C*
138000040510     C* Reperisco i campi del FIVAT
138100040510     C*** VATNOT
138200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138400040510     C                   ENDIF
138500040510     C*** VATTRC
138600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138800040510     C                   ENDIF
138900060331     C*** VATNOT_A
139000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139200060331     C                   ENDIF
139300060331     C*** VATNOT_B
139400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139600060331     C                   ENDIF
139700060331     C*** VATNOT_E
139800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140000060331     C                   ENDIF
140100110927     C*** VATNOT_I
140200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140400110927     C                   ENDIF
140500070823     C*
140600100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140700120301
140800120418      *** W_VABRSD = VABRSD + VABRD2
140900120418     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141000120418     C                   MOVEL     *blanks       wRSD             70
141100120418     C                   EVAL      wRSD=%trim(SkSplitCSV(i))
141200120418     C                   EVAL      VABRSD=%subst(wRSD:1:35)
141300120418     C                   EVAL      VABRD2=%subst(wRSD:36:35)
141400120418     C                   ENDIF
141500120605      *** W_VABRMN / VABRMA / VABNSP
141600120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
141700120605     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
141800120605     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):
141900120605     C                                   %scan('/':%trim(SkSplitCSV(i)):1)+1)
142000120605     C                   EXSR      CHKNUM
142100120605     C                   IF        PiInt=*on
142200120605     C                   Z-ADD     PiVal         VABRMN
142300120605     C                   Z-ADD     PiVal         VABNSP
142400120605     C                   ELSE
142500120605     C                   SETON                                        32
142600120605     C                   EVAL      VABRMN = *zeros
142700120605     C                   EVAL      vinmsg = %trimr(vinmsg)
142800120605     C                             + ' ' + 'VABRMN'
142900120605     C                   ENDIF
143000120604     C                   ENDIF
143100120605      *** W_VATNOT_E
143200120605      * il nr. segnacollo sul barcode è di 8 char, se quello su csv è < devono essere presi anche
143300120605      * gli 0 non significativi
143400120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
143500120605     C                   EVAL      PiStr=SkSplitCSV(i)
143600120605     C                   EXSR      CHKNUM
143700120605     C                   IF        PiInt=*on
143800120605     C                   Z-ADD     PiVal         wNum08            8 0
143900120605     C                   eval      wVATNOT_E = %editc(wNum08:'X')
144000120605     C                   ELSE
144100120605     C                   SETON                                        31
144200120605     C                   EVAL      wVATNOT_E = *blank
144300120605     C                   EVAL      vinmsg = %trimr(vinmsg)
144400120605     C                             + ' ' + 'W_VATNOT_E'
144500120605     C                   ENDIF
144600120605     C                   ENDIF
144700070925     C*
144800071029     C***  <===  -----------------------------------------
144900040510     C*
145000030715     C                   ENDIF
145100030715     C                   EVAL      i = i + 1
145200030715     C                   ENDDO
145300070925     C*
145400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145500120411     C*
145600120604     C* non ci sono
145700100831     C*
145800070925     C*
145900071029     C***  <===  -----------------------------------------
146000070925     C*
146100070925     C* Considerazioni finali su CBO/CAS
146200100318     C                   IF        wFlgCAS = '1'    OR
146300100318     C                             VABCAS  > *zeros
146400070925     C                   IF        VABCBO = '1'
146500070925     C                   EVAL      VABCBO = '4'
146600070925     C                   ENDIF
146700070925     C                   IF        VABCBO = '2'
146800070925     C                   EVAL      VABCBO = '6'
146900070925     C                   ENDIF
147000070925     C                   ENDIF
147100070925     C***  <===  ----------------------------
147200040510     C*
147300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147400040510     C                   IF        VABNSP = *zeros
147500040510     C                   SETON                                        31
147600040510     C                   ENDIF
147700040715     C*
147800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147900040715     C                   IF        VABLNP = *zeros
148000040715     C                   SETON                                        31
148100040715     C                   ENDIF
148200040510     C*
148300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148400120411     C                   IF        VABRMN = *zeros
148500120411     C                   Z-ADD     VABNSP        VABRMN
148600120411     C                   ENDIF
148700120328     C* IN QUESTO CASO LO FORZO = 1
148800120411     C***                IF        VABRMN = *zeros
148900120411     C***                Z-ADD     1             VABRMN
149000120411     C***                ENDIF
149100040510     C*
149200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149300040510     C                   IF        VABRMA = *blanks
149400040714     C                   MOVEL     VABRMN        VABRMA
149500040510     C                   ENDIF
149600040128     C*
149700040510     C* Se provincia nn valorizzata la reperisco
149800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149900110912     C                   IF        VABPRD =  *blanks AND
150000110912     C                             VABNZD =  *blanks AND
150100110912     C                             VABCAD <> *blanks AND
150200110912     C                             VABLOD <> *blanks
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
153200020204     C                   IF        vabVAS <> 'EUR'
153300020204     C                   EVAL      vabVAS =  'ITL'
153400020204     C                   ENDIF
153500020204     C                   ENDIF
153600020204     C*
153700020204     C                   IF        vabCAS > *zeros
153800020204     C                   IF        vabVCA <> 'EUR'
153900020204     C                   EVAL      vabVCA =  'ITL'
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C                   IF        vabVMD > *zeros
154400020204     C                   IF        vabVAD <> 'EUR'
154500020204     C                   EVAL      vabVAD =  'ITL'
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
180100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180200120605X;W_VABRSD;VABIND;VABLOD;VABCAD;VABPRD;X;VABPKB;X;VABVLB;VABNAS;W_VABRMN;X;X;VABNCL;
180300120605W_VATNOT_E;VABTIC;VABCBO;VABCAS
180400120604
180500120604
180600120604
