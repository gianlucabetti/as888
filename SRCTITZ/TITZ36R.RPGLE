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
004200140116     D depspe          s                   LIKE(VABRMA) INZ(*zeros)
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
017900140116     C*
018000140116     C* Staccco un numeratore per il numero spedizione
018100140116     C                   exsr      repNSP
018200010330     C*
018300030822     C                   do        *HIVAL
018400010330     C*
018500030822     C                   read      tivin00r                               70
018600010618     C*
018700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018800010618     C                   if        vinflg = '1'
018900010618     C                   eval      flgOk = '1'
019000010618     C                   endif
019100040510     C*
019200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019300040510     C                   z-add     *zeros        wLenVINDTA        4 0
019400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019800040510     C                   z-add     1             wFlgVINDTA
019900040510     C                   leave
020000040510     C                   endif
020100040510     C                   enddo
020200010618     C*
020300040510     C                   if        vindta > *blanks AND
020400040510     C                             wFlgVINDTA = 1
020500010330     C                   add       1             rrnum
020600010330     C*
020700010601     C                   if        *in70 = *off and
020800010330     C                             (vinflg = *blanks
020900010330     C                              or vinflg = '0'
021000010330     C                              or vinflg = '2')
021100010330     C*
021200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021300010711     C                   if        vinflg = *blanks or vinflg = '0'
021400010711     C                   clear                   vinmsg
021500010711     C                   endif
021600010601     C*
021700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021800070925     C                   eval      vindta  = ' ' + %trim(vindta)
021900070925     C*
022000080527     C                   exsr      inzvar
022100100318     C                   exsr      normCSV
022200010605     C                   exsr      impvab
022300010601     C*
022400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022500010605     C                   eval      depfil = VABLNP
022600010601     C                   exsr      repfil
022700010601     C                   if        depfil = invfil
022800021025     C                   if        vlrpoi = 999
022900030822     C                   move(P)   invfil        VABFGS
023000021025     C                   else
023100030822     C                   move(P)   vlrpoi        VABFGS
023200021025     C                   endif
023300010601     C*
023400010601     C                   exsr      PREELA
023500010601     C*
023600010604     C* Ebbene...
023700010604     C*
023800040119     C  N31              add       1             §CTROKVB
023900040119     C   32              add       1             §CTRMO
024000040119     C   31              add       1             §CTRNO
024100080418     C*
024200030822     C                   if        wGiro = 2
024300110201     C                   exsr      chkvab
024400080513     C                   endif
024500010604     C*
024600010604     C                   if        *in31 = *off and
024700010604     C                             *in32 = *off
024800010604     C                   eval      vinflg = '1'
024900010604     C                   else
025000010604     C                   eval      vinflg = '2'
025100010604     C                   endif
025200010604     C                   endif
025300010601     C*
025400010604     C                   endif
025500010604     C*
025600010330     C                   else
025700010330     C                   eval      vinflg = '1'
025800010330     C                   endif
025900010601     C*
026000010601     C  N70              update    tivin000
026100010330     C*
026200030822     C  N70              enddo
026300040119     C*
026400080527     C* Verifico dati rimasti "in cannna"...
026500080527     C                   seton                                        65
026600110201     C                   exsr      chkvab
026700080527     C*
026800040119     C                   endif                                                  (endif - chkcall)
026900010601     C*
027000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027100010601     C                   if        cntNonEl = *zeros or
027200010601     C                             flgMulti = '0'
027300010330     C* Se non ci sono record con errori ...
027400010601     C                   if        §ctrno = 0 and
027500010604     C                             §ctrmo = 0 and
027600010601     C                             flgStato <> '2'
027700010330     C* ... restituisco esito OK.
027800010330     C                   eval      wrkesito = '0'
027900010330     C                   else
028000040119     C                   if        §ctrokvb > 0
028100010330     C                   eval      wrkesito = '1'
028200010330     C                   else
028300010615     C                   if        flgOk = '0'
028400010615     C                   eval      wrkesito = '2'
028500010615     C                   else
028600010615     C                   eval      wrkesito = '6'
028700010615     C                   endif
028800010330     C                   endif
028900010330     C                   endif
029000010601     C                   else
029100010601     C                   eval      wrkesito = '9'
029200010601     C                   endif
029300010330     C*
029400010330     C                   if        %open(tivin00r)
029500010330     C                   close     tivin00r
029600010330     C                   endif
029700021025     C                   if        %open(fivabwwr)
029800021025     C                   close     fivabwwr
029900010330     C                   endif
030000040512     C                   if        %open(fivatwwr)
030100040512     C                   close     fivatwwr
030200040512     C                   endif
030300010601     C*
030400010601     C                   if        vlrpoi <> 999
030500010601     C                   eval      invfil = vlrpoi
030600010601     C                   endif
030700010330     C*
030800040119     C                   if        §ctrokvb > 0
030900010601     C                             and invfil > *zeros
031000010330     C                   exsr      invio
031100010330     C                   endif
031200010601     C*
031300010618     C                   if        flgGiro = '1'
031400010601     C                   exsr      endela
031500010618     C                   endif
031600010330     C*
031700010330     C                   ENDSR
031800010330     C***
031900080527
032000080527
032100080527
032200080527     C*----------------------------------------------------*
032300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032400080527     C*----------------------------------------------------*
032500100413     C     CHKVAB        BEGSR
032600110201     C*
032700110201     C* Se richiesti ignorare colli / peso / volume
032800110315 xxx C***                if        *in35
032900110315 xxx C***                z-add     *zeros        VABNCL
033000110315 xxx C***                z-add     *zeros        VABPKB
033100110315 xxx C***                z-add     *zeros        VABVLB
033200110315 xxx C***                endif
033300080527     C*
033400080527     C* Scrivo il VAB solo a rottura d spedizione
033500080527     C                   if        depspe = *zeros
033600080527     C  N31              eval      fivabds_sav = fivabds
033700140116     C                   eval      depspe = vabRMA
033800080527     C                   else
033900140116     C                   if        depspe = vabRMA AND *in65 = *off
034000120604      * Il numero di colli invece è già espresso in maniera complessiva x spedizione.
034100120604     C***51              add       VABNCL        sav_VABNCL
034200120604     C   51              z-add     VABNCL        sav_VABNCL
034300100413     C   51              add       VABPKB        sav_VABPKB
034400100413     C   51              add       VABVLB        sav_VABVLB
034500100720     C   52              z-add     VABNCL        sav_VABNCL
034600100720     C   52              z-add     VABPKB        sav_VABPKB
034700100720     C   52              z-add     VABVLB        sav_VABVLB
034800100720     C   53              add       VABNCL        sav_VABNCL
034900100720     C   53              z-add     VABPKB        sav_VABPKB
035000100720     C   53              z-add     VABVLB        sav_VABVLB
035100080527     C                   else
035200080527     C  N31              eval      fivabds_ok = fivabds_sav
035300080527     C  N31              exsr      wrivab
035400080527     C  N31              eval      fivabds_sav = fivabds
035500140116     C                   eval      depspe = vabRMA
035600140116     C                   exsr      repNSP
035700080527     C                   endif
035800080527     C                   endif
035900140116     C*
036000140116     C* Scrivo sempre il VAT
036100140116     C                   if        not *in65
036200140116     C  N31              exsr      wrivat
036300140116     C                   endif
036400080527     C*
036500080527     C                   ENDSR
036600040119
036700040119
036800040119
036900040119     C*----------------------------------------------------*
037000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037100040119     C*----------------------------------------------------*
037200040510     C     WRIVAB        BEGSR
037300080513     C*
037400080513     C* Gestisco l'eventuale rottura x numero spedizione
037500140116     C                   if        VABRMA <> depspe or *in65 = *on
037600070301     C                   if        wVATNOT_E <> *blanks
037700070301     C                   eval      VABCTM = '7Q'
037800070301     C                   endif
037900140116     C                   eval      ok_vabNSP = wVABNSP
038000100413     C                   write     fivab000
038100080513     C                   endif
038200040119     C*
038300040119     C                   ENDSR
038400040119
038500040119
038600040119
038700040119     C*----------------------------------------------------*
038800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038900040119     C*----------------------------------------------------*
039000040510     C     WRIVAT        BEGSR
039100060512     C*
039200060512     C* Inizializzo i campi chiave
039300060512     C                   eval      VATFGS = VABFGS
039400060512     C                   eval      VATAAS = VABAAS
039500060512     C                   eval      VATCCM = VABCCM
039600070823     C                   eval      VATNRS = VABNRS
039700140116     C                   eval      VATNSP = wVABNSP
039800060512     C                   eval      VATLNP = VABLNP
039900040119     C*
040000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040100040510     C                   if        VATTRC = *blanks
040200040510     C                   eval      VATTRC = 'E'
040300040510     C                   endif
040400110318     C                   if        VATNOT <> *blanks
040500040510     C                   write     FIVAT000
040600040119     C                   add       1             §CTROKVT
040700110318     C                   endif
040800060331     C*
040900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041000060331     C                   if        wVATNOT_A <> *blanks
041100060331     C                   eval      VATNOT = wVATNOT_A
041200060331     C                   eval      VATTRC = 'A'
041300060331     C                   write     FIVAT000
041400060331     C                   add       1             §CTROKVT
041500060331     C                   endif
041600060331     C*
041700060331     C                   if        wVATNOT_B <> *blanks
041800060331     C                   eval      VATNOT = wVATNOT_B
041900060331     C                   eval      VATTRC = 'B'
042000060331     C                   write     FIVAT000
042100060331     C                   add       1             §CTROKVT
042200060331     C                   endif
042300060331     C*
042400060331     C                   if        wVATNOT_E <> *blanks
042500110201     C                   eval      VATNOT = wVATNOT_E
042600060331     C                   eval      VATTRC = 'E'
042700120604     C*
042800120604     C* Non devo sviluppare i "CHI SONO" perché sono scritti sul .csv
042900120604     C*
043000120604     C                   write     FIVAT000
043100120604     C                   add       1             §CTROKVT
043200120604     C                   endif
043300110927     C*
043400110927     C                   if        wVATNOT_I <> *blanks
043500110927     C                   eval      VATNOT = wVATNOT_I
043600110927     C                   eval      VATTRC = 'I'
043700110927     C                   write     FIVAT000
043800110927     C                   add       1             §CTROKVT
043900110927     C                   endif
044000040119     C*
044100040119     C                   ENDSR
044200040119
044300040119
044400040119
044500040119     C*----------------------------------------------------*
044600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044700040119     C*----------------------------------------------------*
044800040119     C     PREVAT        BEGSR
044900040119     C*
045000040119     C* Compongo il nome del membro da dare al FIVATWWR
045100040119     C                   eval      parmbr = vlrhdl
045200040119     C                   movel     'M'           parmbr
045300040510     C                   eval      parccm = vlrksc
045400040119     C                   eval      paropz = '1'
045500040119     C* Effettuo la chiamata al CLLE preposto
045600040119     C                   call(e)   'TITVVTC'
045700040119     C                   parm                    parccm
045800040119     C                   parm                    parmbr
045900040119     C                   parm                    paropz
046000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046100040119     C                   if        %error
046200040119     C                   movel     '1'           chkcall
046300040119     C                   else
046400040119     C                   movel     '0'           chkcall
046500040119     C                   endif
046600040119     C*
046700040119     C                   ENDSR
046800010601
046900010601
047000010601
047100010330     C*----------------------------------------------------*
047200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047300010330     C*----------------------------------------------------*
047400040119     C     INZVAR        BEGSR
047500080527     C*
047600080527     C                   setoff                                       65
047700010330     C*
047800030822     C* Inizializzo variabili di wrk
047900020204     C                   Z-ADD     *zeros        Num5_0            5 0
048000101018     C                   MOVEL     *blanks       wFlgCAS
048100060331     C                   MOVEL     *blanks       wVATNOT_A
048200060331     C                   MOVEL     *blanks       wVATNOT_B
048300060331     C                   MOVEL     *blanks       wVATNOT_E
048400110927     C                   MOVEL     *blanks       wVATNOT_I
048500120411     C                   MOVEL     *blanks       wNOT
048600120411     C                   MOVEL     *blanks       Data8A
048700030822     C*
048800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048900030822     C                   CLEAR                   SkSplitCSV
049000030822     C                   EVAL      i = 1
049100030822     C                   EVAL      posDa = *zeros
049200030822     C                   EVAL      posA  = *zeros
049300080527     C*
049400080527     C                   CLEAR                   FIVABDS
049500080527     C                   CLEAR                   FIVAT000
049600030822     C*
049700030822     C* Reimposto i valori di default
049800030822     C                   EXSR      DEFCAM
049900030822     C*
050000010330     C                   ENDSR
050100010330     C*----------------------------------------------------*
050200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050300010330     C*----------------------------------------------------*
050400010330     C     DEFCAM        BEGSR
050500030822     C*
050600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050700030715     C* e delimitatore testo.
050800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051100100318     C*
051200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051300100318     C                   EVAL      CharSOS = CharNUM
051400030822     C*
051500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051600100720     C                   SETOFF                                       505152
051700100720     C                   SETOFF                                       53
051800100413     C                   SELECT
051900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052000100720     C                   SETON                                        50
052100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052200100413     C                   SETON                                        51
052300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052400100720     C                   SETON                                        52
052500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052600100720     C                   SETON                                        53
052700100413     C                   ENDSL
052800030822     C*
052900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053000030822     C                   EVAL      posDaDft = 1
053100030822     C                   EVAL      posADft  = 0
053200030822     C                   EVAL      wGiroDft = 0
053300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053400030822     C                             posDaDft > 0
053500030822     C*
053600030822     C* Gestisco il 1° giro
053700030822     C                   IF        wGiroDft = 0
053800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054000030822     C* Incremento il contatore dei "giri"
054100030822     C                   EVAL      wGiroDft = 1
054200030822     C                   ELSE
054300030822     C                   EVAL      posDaDft = posADft
054400030822     C                   ENDIF
054500030822     C* Eseguo lo scan x trovare la fine del campo corrente
054600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054700030822     C*
054800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055000030822     C                   IF        posDaDft > 0 AND
055100030822     C                             posADft  > 0
055200030822     C* NCL
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'NCL'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiInt=*on
056200030822     C                   Z-ADD     PiVal         VABNCL
056300030822     C                   ENDIF
056400030822     C                   ENDIF
056500030822     C* CCM
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'CCM'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABCCM
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* LNP
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'LNP'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABLNP
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* NRS
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'NRS'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABNRS
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* CTR
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'CTR'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABCTR
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* PKB
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'PKB'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiNum=*on
062700120418     C                   Z-ADD     PiVal         VABPKB
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* VLB
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'VLB'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000030822     C                   Z-ADD     PiVal         VABVLB
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* QFT
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'QFT'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiNum=*on
065300030822     C                   Z-ADD     PiVal         VABQFT
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* CBO
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'CBO'
066100030822     C                   EVAL      VABCBO=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* TSP
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'TSP'
067000030822     C                   EVAL      VABTSP=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400030822     C* VAS
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'VAS'
067900030822     C                   EVAL      VABVAS=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300030822     C* VCA
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'VCA'
068800030822     C                   EVAL      VABVCA=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200070925     C* TIC
069300070925     C                   IF        %subst(
069400070925     C                             %subst(vlrppt:posDaDft+1:
069500070925     C                             posADft-posDaDft-1):1:3)
069600070925     C                             = 'TIC'
069700070925     C                   EVAL      VABTIC=%trim(%subst(
069800070925     C                             %subst(vlrppt:posDaDft+1:
069900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000070925     C                   ENDIF
070100030822     C* GCA
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'GCA'
070600030822     C                   EVAL      VABGCA=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* CTM
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'CTM'
071500030822     C                   EVAL      VABCTM=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* FFD
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'FFD'
072400030822     C                   EVAL      VABFFD=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* VAD
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'VAD'
073300030822     C                   EVAL      VABVAD=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* GMA
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'GMA'
074200030822     C                   EVAL      VABGMA=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* GGA
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'GGA'
075100030822     C                   EVAL      VABGGA=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* GVA
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'GVA'
076000030822     C                   EVAL      VABGVA=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* TC1
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'TC1'
076900030822     C                   EVAL      VABTC1=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300030822     C* TC2
077400030822     C                   IF        %subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):1:3)
077700030822     C                             = 'TC2'
077800030822     C                   EVAL      VABTC2=%trim(%subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100030822     C                   ENDIF
078200040714     C* VATTRC
078300040714     C                   IF        %subst(
078400040714     C                             %subst(vlrppt:posDaDft+1:
078500040714     C                             posADft-posDaDft-1):1:3)
078600040714     C                             = 'TRC'
078700040714     C                   EVAL      VATTRC=%trim(%subst(
078800040714     C                             %subst(vlrppt:posDaDft+1:
078900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000040714     C                   ENDIF
079100030822     C* ...
079200030822     C                   ENDIF
079300030822     C                   ENDDO
079400020204     C*
079500010330     C                   ENDSR
079600010607     C*----------------------------------------------------*
079700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079800010607     C*----------------------------------------------------*
079900010607     C     IMPVAB        BEGSR
080000010607     C*
080100010607     C                   SETOFF                                       3132
080200110315 xxx C***                SETON                                        35
080300030515     C*
080400010607     C                   MOVEL     datcor        VABAAS
080500010607     C                   MOVE      datcor        VABMGS
080600010607     C*
080700030715     C**********
080800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080900030715     C**********
081000030715     C                   IF        wGiro = *zeros
081100040113     C*
081200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081500080429     C                             %trim(intcol(5))<>*blanks
081600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081800080429     C                                      %trim(intcol(5))
081900070823     C     ';':CharCSV   XLATE     vindta        vindta
082000040510     C                   ENDIF
082100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082200031201     C     minu:maiu     XLATE     vindta        vindta
082300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082400030715     C                   DOW       posDa <= %len(%trim(vindta))
082500030715     C*
082600030715     C* Gestisco il 1° campo
082700030715     C                   IF        i = 1
082800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082900030715     C                   EVAL      posDa = 1
083000030715     C* Eseguo lo scan x trovare la fine del primo campo
083100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C* X i campi successivi al 1°
083600030715     C                   ELSE
083700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083800030715     C                   EVAL      posDa = posA + 1
083900030715     C* Eseguo lo scan x trovare la fine del campo corrente
084000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084100030715     C* Gestisco l'ultimo campo
084200030715     C                   IF        posA = *zeros
084300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   LEAVE
084800030715     C                   ELSE
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C                   ENDIF
085300030715     C                   ENDIF
085400030715     C* Incremento il contatore d campo
085500030715     C                   EVAL      i = i +1
085600030715     C                   ENDDO
085700030715     C*
085800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085900030715     C* delimitazione testo
086000030715     C                   EVAL      i = 1
086100030715     C                   DOW       i < %elem(SkSplitFLD)
086200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086400030715     C                   EVAL      i = i + 1
086500030715     C                   ENDDO
086600030715     C                   EVAL      wGiro = 1
086700030715     C                   ELSE
086800030715     C                   EVAL      wGiro = 2
086900031204     C**********
087000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087100031204     C* d separatore campo
087200031204     C**********
087300031204     C                   EVAL      vindta = %trim(vindta)
087400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087500031204     C                   EVAL      vindta = %subst(vindta:2)
087600031204     C                   ENDDO
087700031204     C*
087800031204     C                   Z-ADD     *zeros        lunghInput        4 0
087900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088300110912     C                   ENDDO
088400030715     C**********
088500030715     C* Effettuo lo split del campo dati d input
088600030715     C**********
088700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088800030715     C                   DOW       posDa <= %len(%trim(vindta))
088900030715     C*
089000030715     C* Gestisco il 1° campo
089100030715     C                   IF        i = 1
089200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089300030715     C                   EVAL      posDa = 1
089400030715     C* Eseguo lo scan x trovare la fine del primo campo
089500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C* X i campi successivi al 1°
090000030715     C                   ELSE
090100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090200030715     C                   EVAL      posDa = posA + 1
090300031204     C* Verifico che nn vi sia il campo nn valorizzato
090400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090500031204     C* Se campo nn valorizzato skippo al prossimo
090600031204     C                   EVAL      SkSplitCSV(i) = *blanks
090700031204     C                   EVAL      posA  = posA + 1
090800031204     C                   ELSE
090900030715     C* Eseguo lo scan x trovare la fine del campo corrente
091000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091100030715     C* Gestisco l'ultimo campo
091200030715     C                   IF        posA = *zeros
091300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   LEAVE
091800030715     C                   ELSE
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C                   ENDIF
092300031204     C                   ENDIF
092400030715     C                   ENDIF
092500030715     C* Incremento il contatore d campo
092600030715     C                   EVAL      i = i +1
092700030715     C                   ENDDO
092800030715     C*
092900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093000030715     C* delimitazione testo
093100030715     C                   EVAL      i = 1
093200030715     C                   DOW       i < %elem(SkSplitCSV)
093300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093500030715     C                   EVAL      i = i + 1
093600030715     C                   ENDDO
093700100326     C*
093800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093900030715     C                   EVAL      i = 1
094000030715     C                   DOW       i < %elem(SkSplitFLD)
094100030715     C*
094200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094400030715     C                             SkSplitCSV(i) <> *blanks
094500030721     C*** ATB
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094700030721     C                   EVAL      VABATB = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030715     C*** RSD
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095400030715     C* ==
095500030715     C                   ENDIF
095600030721     C*** RD2
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030715     C*** IND
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096200030715     C                   EVAL      VABIND = SkSplitCSV(i)
096300030715     C                   ENDIF
096400030715     C*** LOD
096500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096700030715     C                   ENDIF
096800030715     C*** PRD
096900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097100030715     C                   ENDIF
097200030721     C*** NZD
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030715     C*** RMA
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** NOT
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** NT2
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** VCA
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** VAS
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** FFD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030721     C*** GC1
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GC2
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** TSP
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101100030721     C                   ENDIF
101200040510     C*** CBO
101300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101500040510     C                   ENDIF
101600030721     C*** NAS
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TIC
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** GCA
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** XCO
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** CTM
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** TCR
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** CTS
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** FTM
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** VAD
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** GMA
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** GGA
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** GVA
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TC1
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** TC2
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** SCL
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** RMO
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** NMO
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*
108500030721     C* Reperisco quindi i campi numerici...
108600040510     C*** NSP
108700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108800040510     C                   EVAL      PiStr=SkSplitCSV(i)
108900040510     C                   EXSR      CHKNUM
109000040510     C                   IF        PiInt=*on
109100040510     C                   Z-ADD     PiVal         VABNSP
109200040510     C                   ELSE
109300040510     C                   SETON                                        31
109400040510     C                   EVAL      VABNSP = *zeros
109500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109600040510     C                             + ' ' + 'VABNSP'
109700040510     C                   ENDIF
109800040510     C                   ENDIF
109900071106     C*** RMN
110000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110100071106     C                   EVAL      PiStr=SkSplitCSV(i)
110200071106     C                   EXSR      CHKNUM
110300071106     C                   IF        PiInt=*on
110400071106     C                   Z-ADD     PiVal         VABRMN
110500071106     C                   ELSE
110600071106     C                   SETON                                        32
110700071106     C                   EVAL      VABRMN = *zeros
110800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110900071106     C                             + ' ' + 'VABRMN'
111000071106     C                   ENDIF
111100071106     C                   ENDIF
111200030721     C*** CAD
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on
111700030721     C                   Z-ADD     PiVal         Num5_0
111800030721     C                   MOVEL(p)  Num5_0        VABCAD
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100030721     C                   EVAL      VABCAD = *zeros
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300030721     C                             + ' ' + 'VABCAD'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600030721     C*** NCL
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on
113100030721     C                   Z-ADD     PiVal         VABNCL
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      VABNCL = *zeros
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600040122     C                             + ' ' + 'VABNCL'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** PKB
114000120418     C* il cliente passa fino ai decigrammi ma noi gestiamo solo gli etti
114100120418     C* per cui si arrotonda il valore passato agli etti
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114300030721     C                   EVAL      PiStr=SkSplitCSV(i)
114400030721     C                   EXSR      CHKNUM
114500030721     C                   IF        PiNum=*on
114600120418     C                   Z-ADD(H)  PiVal         VABPKB
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
115900120605     C                   EVAL      VABVCA = 'EUR'
116000101018     C                   EVAL      wFlgCAS = '1'
116100101018     C                   ENDIF
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiNum=*on
116500030721     C                   Z-ADD     PiVal         VABCAS
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABCAS = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABCAS'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** CCM
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on      AND
117800030721     C                             PiVal<=9999999 AND
117900030721     C                             PiVal>*zeros
118000030721     C                   Z-ADD     PiVal         VABCCM
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      VABCCM = *zeros
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040510     C                             + ' ' + 'VABCCM'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800030721     C*** LNP
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiInt=*on    AND
119300030721     C                             PiVal<=999   AND
119400030721     C                             PiVal>*zeros
119500030721     C                   Z-ADD     PiVal         VABLNP
119600030721     C                   ELSE
119700040715     C                   SETON                                        32
119800030721     C                   EVAL      VABLNP = *zeros
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000040510     C                             + ' ' + 'VABLNP'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** NRS
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiInt=*on AND
120800030721     C                             PiVal<=99
120900030721     C                   Z-ADD     PiVal         VABNRS
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABNRS'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** LNA
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiInt=*on    AND
122100030721     C                             PiVal<=999   AND
122200030721     C                             PiVal>*zeros
122300030721     C                   Z-ADD     PiVal         VABLNA
122400030721     C                   ELSE
122500030721     C                   SETON                                        32
122600030721     C                   EVAL      VABLNA = *zeros
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABLNA'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** CTR
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiInt=*on  AND
123600030721     C                             PiVal<=999
123700030721     C                   Z-ADD     PiVal         VABCTR
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABCTR'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** IAS
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiNum=*on
124900030721     C                   Z-ADD     PiVal         VABIAS
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300030721     C                             + ' ' + 'VABIAS'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** VLB
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiNum=*on
126100030721     C                   Z-ADD     PiVal         VABVLB
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABVLB'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** QFT
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiNum=*on
127300030721     C                   Z-ADD     PiVal         VABQFT
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABQFT'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** NCD
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on      AND
128500030721     C                             PiVal<=9999999
128600030721     C                   Z-ADD     PiVal         VABNCD
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABNCD'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** NCA
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on      AND
129800030721     C                             PiVal<=9999999
129900030721     C                   Z-ADD     PiVal         VABNCA
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABNCA'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** ZNC
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on AND
131100030721     C                             PiVal<=99
131200030721     C                   Z-ADD     PiVal         VABZNC
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABZNC'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** DCR
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on       AND
132400030721     C                             PiVal<=99999999
132500030721     C                   Z-ADD     PiVal         VABDCR
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABDCR'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** HCR
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on   AND
133700030721     C                             PiVal<=9999
133800030721     C                   Z-ADD     PiVal         VABHCR
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABHCR'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** VMD
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiNum=*on
135000030721     C                   Z-ADD     PiVal         VABVMD
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABVMD'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700030721     C*** ANT
135800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on        AND
136200030721     C                             PiVal<=999999999
136300030721     C                   Z-ADD     PiVal         VABANT
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700030721     C                             + ' ' + 'VABANT'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000030721     C*** CMO
137100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137200030721     C                   EVAL      PiStr=SkSplitCSV(i)
137300030721     C                   EXSR      CHKNUM
137400030721     C                   IF        PiInt=*on
137500030721     C                   Z-ADD     PiVal         Num5_0
137600030721     C                   MOVEL(p)  Num5_0        VABCMO
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      VABCMO = *zeros
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABCMO'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400040510     C*
138500040510     C* Reperisco i campi del FIVAT
138600040510     C*** VATNOT
138700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138900040510     C                   ENDIF
139000040510     C*** VATTRC
139100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139300040510     C                   ENDIF
139400060331     C*** VATNOT_A
139500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139700060331     C                   ENDIF
139800060331     C*** VATNOT_B
139900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140100060331     C                   ENDIF
140200060331     C*** VATNOT_E
140300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140500060331     C                   ENDIF
140600110927     C*** VATNOT_I
140700110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140800110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140900110927     C                   ENDIF
141000070823     C*
141100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
141200120301
141300120418      *** W_VABRSD = VABRSD + VABRD2
141400120418     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
141500120418     C                   MOVEL     *blanks       wRSD             70
141600120418     C                   EVAL      wRSD=%trim(SkSplitCSV(i))
141700120418     C                   EVAL      VABRSD=%subst(wRSD:1:35)
141800120418     C                   EVAL      VABRD2=%subst(wRSD:36:35)
141900120418     C                   ENDIF
142000140116      *** W_VABRMN / VABRMA
142100120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142200120605     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
142300170704     C* controllo se c'è / o \ e a seconda di chi c'è eseguo statement diversi
142400170704     C                   SELECT
142500170704     C                   WHEN      %scan('/':%trim(SkSplitCSV(i)):1) > 0
142600170704     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):
142700170704     C                                   %scan('/':%trim(SkSplitCSV(i)):1)+1 :
142800170704     C                                   (%len(%trim(SkSplitCSV(i)))    )     -
142900170704     C                                    %scan('/':%trim(SkSplitCSV(i)):1)   )
143000170704     C                   WHEN      %scan('\':%trim(SkSplitCSV(i)):1) > 0
143100170704     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):
143200170704     C                                   %scan('\':%trim(SkSplitCSV(i)):1)+1 :
143300170704     C                                   (%len(%trim(SkSplitCSV(i)))    )     -
143400170704     C                                    %scan('\':%trim(SkSplitCSV(i)):1)   )
143500170704     C                   OTHER
143600170704     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
143700170704     C                   ENDSL
143800120605     C                   EXSR      CHKNUM
143900120605     C                   IF        PiInt=*on
144000120605     C                   Z-ADD     PiVal         VABRMN
144100120605     C                   ELSE
144200120605     C                   SETON                                        32
144300120605     C                   EVAL      VABRMN = *zeros
144400120605     C                   EVAL      vinmsg = %trimr(vinmsg)
144500120605     C                             + ' ' + 'VABRMN'
144600120605     C                   ENDIF
144700120604     C                   ENDIF
144800170704     C*** DCR
144900170704     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABDCR' and
145000170704     C                             SkSplitCSV(i) <> *blank
145100170704     C                   MONITOR
145200170704     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):7:4)
145300170704     C                                   + %subst(%trim(SkSplitCSV(i)):4:2)
145400170704     C                                   + %subst(%trim(SkSplitCSV(i)):1:2)
145500170704     C                   EXSR      CHKNUM
145600170704     C                   IF        PiInt=*on       AND
145700170704     C                             PiVal<=99999999
145800170704     C                   Z-ADD     PiVal         VABDCR
145900170704     C                   ELSE
146000170704     C                   SETON                                        32
146100170704     C                   EVAL      vinmsg = %trimr(vinmsg)
146200170704     C                             + ' ' + 'VABDCR'
146300170704     C                   ENDIF
146400170704     C                   ON-ERROR
146500170704     C                   SETON                                        32
146600170704     C                   EVAL      vinmsg = %trimr(vinmsg)
146700170704     C                             + ' ' + 'VABDCR'
146800170704     C                   ENDMON
146900170704     C                   ENDIF
147000120605      *** W_VATNOT_E
147100150129      * il nr. segnacollo sul barcode è di 10 char, se quello su csv è < devono essere presi anche
147200120605      * gli 0 non significativi
147300120605     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
147400120605     C                   EVAL      PiStr=SkSplitCSV(i)
147500120605     C                   EXSR      CHKNUM
147600120605     C                   IF        PiInt=*on
147700150129     C                   Z-ADD     PiVal         wNum10           10 0
147800150129     C                   eval      wVATNOT_E = %editc(wNum10:'X')
147900120605     C                   ELSE
148000120605     C                   SETON                                        31
148100120605     C                   EVAL      wVATNOT_E = *blank
148200120605     C                   EVAL      vinmsg = %trimr(vinmsg)
148300120605     C                             + ' ' + 'W_VATNOT_E'
148400120605     C                   ENDIF
148500120605     C                   ENDIF
148600070925     C*
148700071029     C***  <===  -----------------------------------------
148800040510     C*
148900030715     C                   ENDIF
149000030715     C                   EVAL      i = i + 1
149100030715     C                   ENDDO
149200070925     C*
149300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
149400120411     C*
149500120604     C* non ci sono
149600100831     C*
149700070925     C*
149800071029     C***  <===  -----------------------------------------
149900070925     C*
150000070925     C* Considerazioni finali su CBO/CAS
150100100318     C                   IF        wFlgCAS = '1'    OR
150200100318     C                             VABCAS  > *zeros
150300070925     C                   IF        VABCBO = '1'
150400070925     C                   EVAL      VABCBO = '4'
150500070925     C                   ENDIF
150600070925     C                   IF        VABCBO = '2'
150700070925     C                   EVAL      VABCBO = '6'
150800070925     C                   ENDIF
150900070925     C                   ENDIF
151000070925     C***  <===  ----------------------------
151100040510     C*
151200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151300140116     C                   IF        VABNSP = *zeros
151400140116 xxx C***                SETON                                        31        * porcheria !!!
151500140116     C                   ENDIF
151600040715     C*
151700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
151800040715     C                   IF        VABLNP = *zeros
151900040715     C                   SETON                                        31
152000040715     C                   ENDIF
152100040510     C*
152200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152300120411     C                   IF        VABRMN = *zeros
152400120411     C                   Z-ADD     VABNSP        VABRMN
152500120411     C                   ENDIF
152600120328     C* IN QUESTO CASO LO FORZO = 1
152700120411     C***                IF        VABRMN = *zeros
152800120411     C***                Z-ADD     1             VABRMN
152900120411     C***                ENDIF
153000040510     C*
153100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153200040510     C                   IF        VABRMA = *blanks
153300040714     C                   MOVEL     VABRMN        VABRMA
153400040510     C                   ENDIF
153500040128     C*
153600040510     C* Se provincia nn valorizzata la reperisco
153700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153800110912     C                   IF        VABPRD =  *blanks AND
153900110912     C                             VABNZD =  *blanks AND
154000110912     C                             VABCAD <> *blanks AND
154100110912     C                             VABLOD <> *blanks
154200040128     C                   CLEAR                   TISI95DS
154300040128     C                   EVAL      I95TCN = '3'
154400040128     C                   Z-ADD     datcor        I95DAT
154500040128     C                   EVAL      I95NAR = VABNZD
154600040128     C                   EVAL      I95CAP = VABCAD
154700040128     C                   EVAL      I95LOC = VABLOD
154800040128     C                   CALL      'TISI95R'
154900040128     C                   PARM                    TISI95DS
155000040128     C                   EVAL      VABPRD = O95PRV
155100040510     C                   ENDIF
155200020204     C*
155300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155400020204     C                   EXSR      CHKIMPDIV
155500030715     C                   ENDIF
155600020204     C*
155700010607     C                   ENDSR
155800010607     C*----------------------------------------------------*
155900010601
156000020204
156100020204     C*----------------------------------------------------*
156200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156300020204     C*----------------------------------------------------*
156400020204     C     CHKIMPDIV     BEGSR
156500020204     C*
156600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156700020204     C                   Z-ADD     *zeros        wrkDec            9 9
156800020204     C*
156900020204     C* Come prima cosa effettuo considerazioni sulla divisa
157000020204     C                   IF        vabIAS > *zeros
157100020204     C                   IF        vabVAS <> 'EUR'
157200020204     C                   EVAL      vabVAS =  'ITL'
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C                   IF        vabCAS > *zeros
157700020204     C                   IF        vabVCA <> 'EUR'
157800020204     C                   EVAL      vabVCA =  'ITL'
157900020204     C                   ENDIF
158000020204     C                   ENDIF
158100020204     C*
158200020204     C                   IF        vabVMD > *zeros
158300020204     C                   IF        vabVAD <> 'EUR'
158400020204     C                   EVAL      vabVAD =  'ITL'
158500020204     C                   ENDIF
158600020204     C                   ENDIF
158700020204     C*
158800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
158900020204     C                   Z-ADD     vabIAS        wrkDec
159000020204     C                   IF        wrkDec > *zeros
159100020204     C                   IF        vabVAS = 'ITL'
159200020204     C                   EVAL      vabIAS = *zeros
159300020204     C                   ENDIF
159400020204     C                   ENDIF
159500020204     C*
159600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
159700020204     C                   Z-ADD     vabCAS        wrkDec
159800020204     C                   IF        wrkDec > *zeros
159900020204     C                   IF        vabVCA = 'ITL'
160000020204     C                   EVAL      vabCAS = *zeros
160100020204     C                   ENDIF
160200020204     C                   ENDIF
160300020204     C*
160400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160500020204     C                   Z-ADD     vabVMD        wrkDec
160600020204     C                   IF        wrkDec > *zeros
160700020204     C                   IF        vabVAD = 'ITL'
160800020204     C                   EVAL      vabVMD = *zeros
160900020204     C                   ENDIF
161000020204     C                   ENDIF
161100020204     C*
161200020204     C                   ENDSR
161300020204     C***
161400010330
161500100318
161600100318
161700100318     C*----------------------------------------------------*
161800100318     C*  CONTROLLO NUMERICITA' CAMPI
161900100318     C*----------------------------------------------------*
162000100318     C     CHKNUM        BEGSR
162100100318     C*
162200100318     C                   IF        PiDecChr = *blanks
162300100318     C                   EVAL      PiDecChr = CharNUM
162400100318     C                   ENDIF
162500100318     C*
162600100318     C                   callp     UBISNUM_Check(PiStr
162700100318     C                                          :PiDecChr
162800100318     C                                          :PiVal
162900100318     C                                          :PiNum
163000100318     C                                          :PiInt)
163100100318     C*
163200100318     C                   ENDSR
163300100318     C***
163400010601
163500010601
163600010601
163700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163800010601     C     repfil        BEGSR
163900010601     C*
164000010601     C                   if        invfil = *zeros and
164100010601     C                             depfil > *zeros and
164200010629     C                             (vinflg = *blanks or
164300010629     C                              vinflg = *zeros)
164400010601     C
164500010601     C                   eval      invfil = depfil
164600010601     C                   endif
164700010601     C*
164800010601     C                   if        depfil <> invfil and
164900010601     C                             invfil > *zeros
165000010601     C                   eval      flgMulti = '1'
165100010601     C                   if        vinflg = *blanks
165200010601     C                   add       1             cntNonEl
165300010601     C                   endif
165400010601     C                   endif
165500010601     C*
165600010601     C                   if        vinflg = '2'
165700010601     C                   eval      flgStato = '2'
165800010601     C                   endif
165900010601     C*
166000010601     C                   ENDSR
166100010601     C***
166200010601
166300010601
166400010601
166500010330
166600040119      /TITLE Invio dei dati al punto operativo.
166700040119     C     invio         BEGSR
166800040119     C*
166900040119     C* 1° invio FIVAT
167000040119     C                   reset                   dscmz
167100040119     C                   move      vlrpoi        cmzdst
167200040119     C                   eval      cmzfld = 'FIVATWWR'
167300040119     C                   eval      cmzmbd = vlrhdl
167400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167500040119     C***                if        prmfir = *blanks
167600040119     C                   eval      cmzfla = 'FIVAT00F'
167700040119     C                   eval      cmzmba = 'FIVAT00F'
167800040119     C***                else
167900040119     C***                eval      cmzfla = prmfir
168000040119     C***                eval      cmzmba = prmfir
168100040119     C***                endif
168200040119     C                   eval      cmznrr = *zeros
168300040119     C                   move      §ctrokvt      cmznrr
168400040119     C                   eval      cmzlba = vlrfl1
168500040119     C                   call(e)   'TIS711C'
168600040119     C                   parm                    dscmz
168700040119     C                   parm      *blanks       esito
168800040119     C                   if        %error
168900040119     C                             or cmzerr = '1'
169000040119     C                             or esito  = '1'
169100040119     C                   eval      wrkesito = '3'
169200040119     C                   else
169300040119     C*
169400040119     C* 2° invio FIVAB
169500040119     C                   reset                   dscmz
169600040119     C                   move      vlrpoi        cmzdst
169700040119     C                   eval      cmzfld = vlrfou
169800040119     C                   eval      cmzmbd = vlrhdl
169900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170000040119     C***                if        prmfir = *blanks
170100040119     C                   eval      cmzfla = 'FIVAB00F'
170200040119     C                   eval      cmzmba = 'FIVAB00F'
170300040119     C***                else
170400040119     C***                eval      cmzfla = prmfir
170500040119     C***                eval      cmzmba = prmfir
170600040119     C***                endif
170700040119     C                   eval      cmznrr = *zeros
170800040119     C                   move      §ctrokvb      cmznrr
170900040119     C                   eval      cmzlba = vlrfl1
171000040119     C                   call(e)   'TIS711C'
171100040119     C                   parm                    dscmz
171200040119     C                   parm      *blanks       esito
171300040119     C                   if        %error
171400040119     C                             or cmzerr = '1'
171500040119     C                             or esito  = '1'
171600040119     C                   eval      wrkesito = '3'
171700040119     C                   endif
171800040119     C                   endif
171900040119     C*
172000040119     C                   ENDSR
172100040119     C***
172200010601
172300010601
172400010601
172500010601
172600010601      /TITLE Invio dei dati al punto operativo.
172700010601     C     opeini        BEGSR
172800010601     C*
172900010601     C* Inizializzo flag e contatori operativi
173000010601     C                   movel     '0'           flgGiro           1
173100010601     C                   movel     '0'           flgMulti          1
173200010601     C                   movel     '1'           flgStato          1
173300010615     C                   movel     '0'           flgOk             1
173400010601     C                   z-add     *zeros        cntNonEl         10 0
173500010601     C                   z-add     *zeros        depfil            3 0
173600010601     C                   z-add     *zeros        invfil            3 0
173700010601     C*
173800010601     C                   ENDSR
173900010601     C***
174000070326
174100070326
174200070326
174300070326
174400070326     C     *pssr         BEGSR
174500070329     C*
174600070329     C                   if        %open(tivin00r)
174700070329     C                   close     tivin00r
174800070329     C                   endif
174900070329     C                   if        %open(fivabwwr)
175000070329     C                   close     fivabwwr
175100070329     C                   endif
175200070329     C                   if        %open(fivatwwr)
175300070329     C                   close     fivatwwr
175400070329     C                   endif
175500070326     C*
175600070326     C* Effettuo la chiamata al CLLE preposto
175700070326     C                   call(e)   'TITVVTC'
175800070326     C                   parm                    parccm
175900070326     C                   parm                    parmbr
176000070326     C                   parm      '2'           paropz
176100070326     C*
176200070326     C                   eval      wrkesito = '2'
176300070404     C*
176400070404     C                   seton                                        LR
176500070326     C*
176600070326     C                   ENDSR     '*CANCL'
176700070326     C***
176800100318
176900100318
177000100318
177100100318     C     normCSV       BEGSR
177200100318     C*
177300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177400100318     C* separatori campi
177500100318     C                   if        CharTXT <> *blanks
177600100318     C                   z-add     *zeros        wLenVINDTA        4 0
177700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
177800100318     C                   setoff                                       80
177900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
178000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
178100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
178200100318     C                   if        not *in80
178300100318     C                   seton                                        80
178400100318     C                   else
178500100318     C                   setoff                                       80
178600100318     C                   endif
178700100318     C                   endif
178800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
178900100318     C                             and *in80 = *on
179000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
179100100318     C                   endif
179200100318     C                   enddo
179300100318     C                   endif
179400100318     C*
179500100318     C                   ENDSR
179600100318     C***
179700070326
179800100326
179900100326
180000100326     C*----------------------------------------------------*
180100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
180200100326     C*----------------------------------------------------*
180300100326     C     REPNSP        BEGSR
180400100326     C*
180500100326     C* NSP => Stacco un numeratore da AZNUM
180600100326     C                   clear                   TRUL33DS
180700100326     C                   eval      I33OPE = *zeros
180800100326     C                   eval      I33CNU = 302
180900100326     C                   eval      I33NUM = 1
181000100326     C                   movel     TRUL33DS      KPJBU
181100100326     C                   call      'TRUL33R'
181200100326     C                   parm                    KPJBA
181300100326     C                   movel     KPJBU         TRUL33DS
181400100326     C                   if        O33ERR = *zeros
181500140116     C                   z-add     O33NRF        wVABNSP           7 0
181600100326     C                   else
181700100326     C                   SETON                                        31
181800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
181900140116     C                             + ' ' + 'VABNSP'
182000100326     C                   endif
182100100326     C*
182200100326     C                   ENDSR
182300010330
182400100326
182500010330
182600000613     C     *inzsr        BEGSR
182700990910     C*
182800990910     C     *entry        plist
182900990920     C                   parm                    tivlrds
183000990921     C                   parm      wrkesito      esito
183100000724     C                   parm                    prmlit
183200000710     C                   parm                    prmfir
183300010330     C*
183400010330     C* CALCOLA LA DATA CORRENTE
183500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
183600100318     C                   eval      datcor = %dec(%date() : *ISO)
183700000613     C*
183800000613     C                   ENDSR
183900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
184000120605X;W_VABRSD;VABIND;VABLOD;VABCAD;VABPRD;X;VABPKB;X;VABVLB;VABNAS;W_VABRMN;X;X;VABNCL;
184100170704W_VATNOT_E;VABTIC;VABCBO;VABCAS;W_VABDCR;VABNOT;
184200120604
184300120604
184400120604
