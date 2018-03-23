000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400130610      * RMN ed RMA sono, come la parte fissa dei "CHI SONO", gli ultimi 15 caratteri
000500130610      * a dx del valore inviato dal cliente;
000600130611      * i "CHI SONO" si sviluppano dal nr.colli poi gli si aggiunge il check digit E10;
000700130611      * peso e volume vanno arrotondati;
000800160331      * Se il cliente è 0506139 allora VABTIC è sempre blank
000900130611      *
001000121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001100120209     F*
001200990910     Ftivin00r  uF   E             DISK    usropn
001300100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001400040119     FFIVATwwr  O    E             DISK    usropn
001500000313     D*
001600040113     D*------------
001700040113     D* SCHIEDE A PROGRAMMA
001800040113     D*------------
001900080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700040128     D tisi95ds      e ds
002800100413     D fivabds       e ds                  extname(fivab00f)
002900100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003000100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600040510     D parccm          s              8    INZ(*blanks)
003700040119     D parmbr          s             10    INZ(*blanks)
003800040119     D paropz          s              1    INZ(*blanks)
003900040119     D chkcall         s              1    INZ(*blanks)
004000080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100080418     D wFlgCAS         s              1    INZ(*blanks)
004200120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004300120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004400130528     D Intestazioni    s               n   inz(*off)
004500100326     D*------------------
004600100326     D* DS REPERIMENTO NUMERATORE
004700100326     D*------------------
004800100326     D trul33ds      e ds                  inz
004900100326     D*------------------
005000100326     D* DS ARCHITETTURA
005100100326     D*------------------
005200100326     D kpjba         e ds                  inz
005300130611     D*------------------
005400130611     D TRUL28DSX     e ds                  inz
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
007300131129     D wVATNOT_J       s                   LIKE(VATNOT)
007400131129     D wVATNOT_IJ      s             70
007500131129     D wNOT            s             70
007600120411     D Data8A          s              8
007700030822     D*------------------
007800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007900030822     D*------------------
008000030822     D posDaDft        S              3  0 INZ(*zeros)
008100030822     D posADft         S              3  0 INZ(*zeros)
008200030822     D j               s              3  0 INZ(1)
008300030822     D wGiroDft        s              1  0 INZ(*zeros)
008400080418     D*------------------
008500080418     D* Costanti
008600080418     D*------------------
008700080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008800080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008900100318
009000100318     D*------------------
009100100318     D* LINKING A DEFINIZIONI ESTERNE
009200100318     D*------------------
009300100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009400100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009500100318
009600010330
009700010330
009800010330
009900000913     C                   reset                   rrnum
010000990921     C                   reset                   esito
010100990921     C                   reset                   wrkesito
010200010601     C*
010300010601     C                   exsr      opeini
010400010605     C                   exsr      rwvab
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011200010601     C*--------------------------------------------------------
011300010601     C     PREELA        BEGSR
011400010601     C*
011500010601     C* SE OCCORRE SPEDIRE IN FILIALE
011600010601     C                   if        invfil <> *zeros and
011700010601     C                             flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100010601     C*
012200010601     C                   endif
012300010601     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800010601
012900010601
013000010601     C*--------------------------------------------------------
013100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013200010601     C*--------------------------------------------------------
013300010601     C     ENDELA        BEGSR
013400040119     C*
013500040119     C* Effettuo la chiamata al CLLE preposto
013600040119     C                   call(e)   'TITVVTC'
013700040510     C                   parm                    parccm
013800040119     C                   parm                    parmbr
013900040119     C                   parm      '2'           paropz
014000050201     C*
014100050201     C* Effettuo lancio TISI95 solo x chiusura
014200050201     C                   CLEAR                   TISI95DS
014300050201     C                   EVAL      I95TLA = 'C'
014400050201     C                   CALL      'TISI95R'
014500050201     C                   PARM                    TISI95DS
014600000616     C*
014700010601     C                   ENDSR
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015200010330     C*--------------------------------------------------------
015300010605     C     RWVAB         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800021025     C                   if        not %open(fivabwwr)
015900021025     C                   open      fivabwwr
016000010330     C                   endif
016100040119     C*
016200040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300040119     C                   exsr      prevat
016400040119     C*
016500040119     C                   if        chkcall = '0'
016600040119     C*
016700040119     C                   if        not %open(fivatwwr)
016800040119     C                   open      fivatwwr
016900040119     C                   endif
017000010330     C*
017100040119     C                   clear                   §CTROKVB          7 0
017200040119     C                   clear                   §CTROKVT          7 0
017300040119     C                   clear                   §CTRMO            7 0
017400040119     C                   clear                   §CTRNO            7 0
017500120410     C*
017600120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017700120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017800120410     C                   IF        Intestazioni = *off
017900120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018000120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018100120410     C                   exsr      inzvar
018200120410     C                   exsr      impvab
018300120410     C                   ENDIF
018400010330     C*
018500030822     C                   do        *HIVAL
018600010330     C*
018700030822     C                   read      tivin00r                               70
018800010618     C*
018900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019000010618     C                   if        vinflg = '1'
019100010618     C                   eval      flgOk = '1'
019200010618     C                   endif
019300040510     C*
019400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019500040510     C                   z-add     *zeros        wLenVINDTA        4 0
019600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020000040510     C                   z-add     1             wFlgVINDTA
020100040510     C                   leave
020200040510     C                   endif
020300040510     C                   enddo
020400010618     C*
020500040510     C                   if        vindta > *blanks AND
020600040510     C                             wFlgVINDTA = 1
020700010330     C                   add       1             rrnum
020800010330     C*
020900010601     C                   if        *in70 = *off and
021000010330     C                             (vinflg = *blanks
021100010330     C                              or vinflg = '0'
021200010330     C                              or vinflg = '2')
021300010330     C*
021400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021500010711     C                   if        vinflg = *blanks or vinflg = '0'
021600010711     C                   clear                   vinmsg
021700010711     C                   endif
021800010601     C*
021900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022000130605     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022100130605     C* evitare che vengano elisi i blank
022200130610     C                   eval      vindta  = ' ' + %trim(vindta)
022300070925     C*
022400080527     C                   exsr      inzvar
022500100318     C                   exsr      normCSV
022600010605     C                   exsr      impvab
022700010601     C*
022800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022900010605     C                   eval      depfil = VABLNP
023000010601     C                   exsr      repfil
023100010601     C                   if        depfil = invfil
023200021025     C                   if        vlrpoi = 999
023300030822     C                   move(P)   invfil        VABFGS
023400021025     C                   else
023500030822     C                   move(P)   vlrpoi        VABFGS
023600021025     C                   endif
023700010601     C*
023800010601     C                   exsr      PREELA
023900010601     C*
024000010604     C* Ebbene...
024100010604     C*
024200040119     C  N31              add       1             §CTROKVB
024300040119     C   32              add       1             §CTRMO
024400040119     C   31              add       1             §CTRNO
024500080418     C*
024600030822     C                   if        wGiro = 2
024700080513     C* Scrivo sempre il VAT
024800080513     C  N31              exsr      wrivat
024900110201     C                   exsr      chkvab
025000080513     C                   endif
025100010604     C*
025200010604     C                   if        *in31 = *off and
025300010604     C                             *in32 = *off
025400010604     C                   eval      vinflg = '1'
025500010604     C                   else
025600010604     C                   eval      vinflg = '2'
025700010604     C                   endif
025800010604     C                   endif
025900010601     C*
026000010604     C                   endif
026100010604     C*
026200010330     C                   else
026300010330     C                   eval      vinflg = '1'
026400010330     C                   endif
026500010601     C*
026600010601     C  N70              update    tivin000
026700010330     C*
026800030822     C  N70              enddo
026900040119     C*
027000080527     C* Verifico dati rimasti "in cannna"...
027100080527     C                   seton                                        65
027200110201     C                   exsr      chkvab
027300080527     C*
027400040119     C                   endif                                                  (endif - chkcall)
027500010601     C*
027600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027700010601     C                   if        cntNonEl = *zeros or
027800010601     C                             flgMulti = '0'
027900010330     C* Se non ci sono record con errori ...
028000010601     C                   if        §ctrno = 0 and
028100010604     C                             §ctrmo = 0 and
028200010601     C                             flgStato <> '2'
028300010330     C* ... restituisco esito OK.
028400010330     C                   eval      wrkesito = '0'
028500010330     C                   else
028600040119     C                   if        §ctrokvb > 0
028700010330     C                   eval      wrkesito = '1'
028800010330     C                   else
028900010615     C                   if        flgOk = '0'
029000010615     C                   eval      wrkesito = '2'
029100010615     C                   else
029200010615     C                   eval      wrkesito = '6'
029300010615     C                   endif
029400010330     C                   endif
029500010330     C                   endif
029600010601     C                   else
029700010601     C                   eval      wrkesito = '9'
029800010601     C                   endif
029900010330     C*
030000010330     C                   if        %open(tivin00r)
030100010330     C                   close     tivin00r
030200010330     C                   endif
030300021025     C                   if        %open(fivabwwr)
030400021025     C                   close     fivabwwr
030500010330     C                   endif
030600040512     C                   if        %open(fivatwwr)
030700040512     C                   close     fivatwwr
030800040512     C                   endif
030900010601     C*
031000010601     C                   if        vlrpoi <> 999
031100010601     C                   eval      invfil = vlrpoi
031200010601     C                   endif
031300010330     C*
031400040119     C                   if        §ctrokvb > 0
031500010601     C                             and invfil > *zeros
031600010330     C                   exsr      invio
031700010330     C                   endif
031800010601     C*
031900010618     C                   if        flgGiro = '1'
032000010601     C                   exsr      endela
032100010618     C                   endif
032200010330     C*
032300010330     C                   ENDSR
032400010330     C***
032500080527
032600080527
032700080527
032800080527     C*----------------------------------------------------*
032900080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033000080527     C*----------------------------------------------------*
033100100413     C     CHKVAB        BEGSR
033200110201     C*
033300110201     C* Se richiesti ignorare colli / peso / volume
033400110315 xxx C***                if        *in35
033500110315 xxx C***                z-add     *zeros        VABNCL
033600110315 xxx C***                z-add     *zeros        VABPKB
033700110315 xxx C***                z-add     *zeros        VABVLB
033800110315 xxx C***                endif
033900080527     C*
034000080527     C* Scrivo il VAB solo a rottura d spedizione
034100080527     C                   if        depspe = *zeros
034200080527     C  N31              eval      fivabds_sav = fivabds
034300080527     C                   eval      depspe = vabNSP
034400080527     C                   else
034500080527     C                   if        depspe = vabNSP AND *in65 = *off
034600100413     C   51              add       VABNCL        sav_VABNCL
034700100413     C   51              add       VABPKB        sav_VABPKB
034800100413     C   51              add       VABVLB        sav_VABVLB
034900100720     C   52              z-add     VABNCL        sav_VABNCL
035000100720     C   52              z-add     VABPKB        sav_VABPKB
035100100720     C   52              z-add     VABVLB        sav_VABVLB
035200100720     C   53              add       VABNCL        sav_VABNCL
035300100720     C   53              z-add     VABPKB        sav_VABPKB
035400100720     C   53              z-add     VABVLB        sav_VABVLB
035500080527     C                   else
035600080527     C  N31              eval      fivabds_ok = fivabds_sav
035700080527     C  N31              exsr      wrivab
035800080527     C  N31              eval      fivabds_sav = fivabds
035900080527     C                   eval      depspe = vabNSP
036000080527     C                   endif
036100080527     C                   endif
036200080527     C*
036300080527     C                   ENDSR
036400040119
036500040119
036600040119
036700040119     C*----------------------------------------------------*
036800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036900040119     C*----------------------------------------------------*
037000040510     C     WRIVAB        BEGSR
037100080513     C*
037200080513     C* Gestisco l'eventuale rottura x numero spedizione
037300080527     C                   if        VABNSP <> depspe or *in65 = *on
037400070301     C                   if        wVATNOT_E <> *blanks
037500130610     C                   eval      ok_VABCTM = '7Q'
037600070301     C                   endif
037700100413     C                   write     fivab000
037800080513     C                   endif
037900040119     C*
038000040119     C                   ENDSR
038100040119
038200040119
038300040119
038400040119     C*----------------------------------------------------*
038500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038600040119     C*----------------------------------------------------*
038700040510     C     WRIVAT        BEGSR
038800060512     C*
038900060512     C* Inizializzo i campi chiave
039000060512     C                   eval      VATFGS = VABFGS
039100060512     C                   eval      VATAAS = VABAAS
039200060512     C                   eval      VATCCM = VABCCM
039300070823     C                   eval      VATNRS = VABNRS
039400060512     C                   eval      VATNSP = VABNSP
039500060512     C                   eval      VATLNP = VABLNP
039600040119     C*
039700040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039800040510     C                   if        VATTRC = *blanks
039900040510     C                   eval      VATTRC = 'E'
040000040510     C                   endif
040100110318     C                   if        VATNOT <> *blanks
040200040510     C                   write     FIVAT000
040300040119     C                   add       1             §CTROKVT
040400110318     C                   endif
040500060331     C*
040600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040700060331     C                   if        wVATNOT_A <> *blanks
040800060331     C                   eval      VATNOT = wVATNOT_A
040900060331     C                   eval      VATTRC = 'A'
041000060331     C                   write     FIVAT000
041100060331     C                   add       1             §CTROKVT
041200060331     C                   endif
041300060331     C*
041400060331     C                   if        wVATNOT_B <> *blanks
041500060331     C                   eval      VATNOT = wVATNOT_B
041600060331     C                   eval      VATTRC = 'B'
041700060331     C                   write     FIVAT000
041800060331     C                   add       1             §CTROKVT
041900060331     C                   endif
042000060331     C*
042100060331     C                   if        wVATNOT_E <> *blanks
042200110201     C                   eval      VATNOT = wVATNOT_E
042300060331     C                   eval      VATTRC = 'E'
042400120411     C*
042500120411     C* Sviluppati i "CHI SONO" in relazione al numero colli
042600120411     C*
042700120411     C     1             DO        VABNCL        wNumCollo         3 0
042800130611     C*** da analisi del POC il progressivo colli è max 3 char e deve contenere gli zeri non
042900120411     C*** significativi
043000120411     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
043100120411     C                                %editc(wNumCollo:'X')
043200130611     C* recupero il check digit
043300130611     C                   clear                   TRUL28DSX
043400130611     C                   eval      I284STD = 'E10'
043500130611     C                   eval      I284COD = VATNOT
043600130611     C                   call      'TRUL28R4'
043700130611     C                   parm                    TRUL28DSX
043800130611     C* Se tutto ok scrivo il "chi sono" corrente col check digit
043900130611     C                   IF        O284err = *blanks
044000130611     C                   EVAL      VATNOT = %trim(VATNOT)
044100130611     C                              + %trim(O284CkD)
044200120410     C                   write     FIVAT000
044300120410     C                   add       1             §CTROKVT
044400130611     C                   ELSE
044500130611     C                   SETON                                        31
044600130611     C                   EVAL      VATNOT = *blank
044700130611     C                   EVAL      vinmsg = %trimr(vinmsg)
044800130611     C                             + ' ' + 'VATNOT'
044900130611     C                   ENDIF
045000120411     C                   ENDDO
045100120320     C*
045200120320     C                   endif
045300120320     C*
045400110927     C*
045500110927     C                   if        wVATNOT_I <> *blanks
045600110927     C                   eval      VATNOT = wVATNOT_I
045700110927     C                   eval      VATTRC = 'I'
045800110927     C                   write     FIVAT000
045900110927     C                   add       1             §CTROKVT
046000110927     C                   endif
046100131129     C*
046200131129     C                   if        wVATNOT_J <> *blanks
046300131129     C                   eval      VATNOT = wVATNOT_J
046400131129     C                   eval      VATTRC = 'J'
046500131129     C                   write     FIVAT000
046600131129     C                   add       1             §CTROKVT
046700131129     C                   endif
046800040119     C*
046900040119     C                   ENDSR
047000040119
047100040119
047200040119
047300040119     C*----------------------------------------------------*
047400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047500040119     C*----------------------------------------------------*
047600040119     C     PREVAT        BEGSR
047700040119     C*
047800040119     C* Compongo il nome del membro da dare al FIVATWWR
047900040119     C                   eval      parmbr = vlrhdl
048000040119     C                   movel     'M'           parmbr
048100040510     C                   eval      parccm = vlrksc
048200040119     C                   eval      paropz = '1'
048300040119     C* Effettuo la chiamata al CLLE preposto
048400040119     C                   call(e)   'TITVVTC'
048500040119     C                   parm                    parccm
048600040119     C                   parm                    parmbr
048700040119     C                   parm                    paropz
048800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048900040119     C                   if        %error
049000040119     C                   movel     '1'           chkcall
049100040119     C                   else
049200040119     C                   movel     '0'           chkcall
049300040119     C                   endif
049400040119     C*
049500040119     C                   ENDSR
049600010601
049700010601
049800010601
049900010330     C*----------------------------------------------------*
050000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050100010330     C*----------------------------------------------------*
050200040119     C     INZVAR        BEGSR
050300080527     C*
050400080527     C                   setoff                                       65
050500010330     C*
050600030822     C* Inizializzo variabili di wrk
050700020204     C                   Z-ADD     *zeros        Num5_0            5 0
050800101018     C                   MOVEL     *blanks       wFlgCAS
050900060331     C                   MOVEL     *blanks       wVATNOT_A
051000060331     C                   MOVEL     *blanks       wVATNOT_B
051100060331     C                   MOVEL     *blanks       wVATNOT_E
051200110927     C                   MOVEL     *blanks       wVATNOT_I
051300131129     C                   MOVEL     *blanks       wVATNOT_J
051400131129     C                   MOVEL     *blanks       wVATNOT_IJ
051500120411     C                   MOVEL     *blanks       wNOT
051600120411     C                   MOVEL     *blanks       Data8A
051700030822     C*
051800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051900030822     C                   CLEAR                   SkSplitCSV
052000030822     C                   EVAL      i = 1
052100030822     C                   EVAL      posDa = *zeros
052200030822     C                   EVAL      posA  = *zeros
052300080527     C*
052400080527     C                   CLEAR                   FIVABDS
052500080527     C                   CLEAR                   FIVAT000
052600030822     C*
052700030822     C* Reimposto i valori di default
052800030822     C                   EXSR      DEFCAM
052900030822     C*
053000010330     C                   ENDSR
053100010330     C*----------------------------------------------------*
053200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053300010330     C*----------------------------------------------------*
053400010330     C     DEFCAM        BEGSR
053500030822     C*
053600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053700030715     C* e delimitatore testo.
053800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054100100318     C*
054200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054300100318     C                   EVAL      CharSOS = CharNUM
054400030822     C*
054500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054600100720     C                   SETOFF                                       505152
054700100720     C                   SETOFF                                       53
054800100413     C                   SELECT
054900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055000100720     C                   SETON                                        50
055100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055200100413     C                   SETON                                        51
055300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
055400100720     C                   SETON                                        52
055500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
055600100720     C                   SETON                                        53
055700100413     C                   ENDSL
055800030822     C*
055900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056000030822     C                   EVAL      posDaDft = 1
056100030822     C                   EVAL      posADft  = 0
056200030822     C                   EVAL      wGiroDft = 0
056300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056400030822     C                             posDaDft > 0
056500030822     C*
056600030822     C* Gestisco il 1° giro
056700030822     C                   IF        wGiroDft = 0
056800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057000030822     C* Incremento il contatore dei "giri"
057100030822     C                   EVAL      wGiroDft = 1
057200030822     C                   ELSE
057300030822     C                   EVAL      posDaDft = posADft
057400030822     C                   ENDIF
057500030822     C* Eseguo lo scan x trovare la fine del campo corrente
057600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057700030822     C*
057800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058000030822     C                   IF        posDaDft > 0 AND
058100030822     C                             posADft  > 0
058200030822     C* NCL
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'NCL'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiInt=*on
059200030822     C                   Z-ADD     PiVal         VABNCL
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* CCM
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'CCM'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500030822     C                   Z-ADD     PiVal         VABCCM
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* LNP
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'LNP'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiInt=*on
061800030822     C                   Z-ADD     PiVal         VABLNP
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* NRS
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'NRS'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiInt=*on
063100030822     C                   Z-ADD     PiVal         VABNRS
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* CTR
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'CTR'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiInt=*on
064400030822     C                   Z-ADD     PiVal         VABCTR
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* PKB
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'PKB'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiNum=*on
065700130610     C                   Z-ADD     PiVal         VABPKB
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* VLB
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'VLB'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiNum=*on
067000130610     C                   Z-ADD     PiVal         VABVLB
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* QFT
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'QFT'
067800030822     C                   EVAL      PiStr=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   EXSR      CHKNUM
068200030822     C                   IF        PiNum=*on
068300030822     C                   Z-ADD     PiVal         VABQFT
068400030822     C                   ENDIF
068500030822     C                   ENDIF
068600030822     C* CBO
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'CBO'
069100030822     C                   EVAL      VABCBO=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500030822     C* TSP
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'TSP'
070000030822     C                   EVAL      VABTSP=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* VAS
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'VAS'
070900030822     C                   EVAL      VABVAS=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* VCA
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'VCA'
071800030822     C                   EVAL      VABVCA=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200070925     C* TIC
072300070925     C                   IF        %subst(
072400070925     C                             %subst(vlrppt:posDaDft+1:
072500070925     C                             posADft-posDaDft-1):1:3)
072600070925     C                             = 'TIC'
072700070925     C                   EVAL      VABTIC=%trim(%subst(
072800070925     C                             %subst(vlrppt:posDaDft+1:
072900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000070925     C                   ENDIF
073100030822     C* GCA
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'GCA'
073600030822     C                   EVAL      VABGCA=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* CTM
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'CTM'
074500030822     C                   EVAL      VABCTM=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* FFD
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'FFD'
075400030822     C                   EVAL      VABFFD=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* VAD
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'VAD'
076300030822     C                   EVAL      VABVAD=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* GMA
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'GMA'
077200030822     C                   EVAL      VABGMA=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600030822     C* GGA
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'GGA'
078100030822     C                   EVAL      VABGGA=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500030822     C* GVA
078600030822     C                   IF        %subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):1:3)
078900030822     C                             = 'GVA'
079000030822     C                   EVAL      VABGVA=%trim(%subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300030822     C                   ENDIF
079400030822     C* TC1
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800030822     C                             = 'TC1'
079900030822     C                   EVAL      VABTC1=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300030822     C* TC2
080400030822     C                   IF        %subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):1:3)
080700030822     C                             = 'TC2'
080800030822     C                   EVAL      VABTC2=%trim(%subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100030822     C                   ENDIF
081200040714     C* VATTRC
081300040714     C                   IF        %subst(
081400040714     C                             %subst(vlrppt:posDaDft+1:
081500040714     C                             posADft-posDaDft-1):1:3)
081600040714     C                             = 'TRC'
081700040714     C                   EVAL      VATTRC=%trim(%subst(
081800040714     C                             %subst(vlrppt:posDaDft+1:
081900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000040714     C                   ENDIF
082100030822     C* ...
082200030822     C                   ENDIF
082300030822     C                   ENDDO
082400020204     C*
082500010330     C                   ENDSR
082600010607     C*----------------------------------------------------*
082700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082800010607     C*----------------------------------------------------*
082900010607     C     IMPVAB        BEGSR
083000010607     C*
083100010607     C                   SETOFF                                       3132
083200110315 xxx C***                SETON                                        35
083300030515     C*
083400010607     C                   MOVEL     datcor        VABAAS
083500010607     C                   MOVE      datcor        VABMGS
083600010607     C*
083700030715     C**********
083800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083900030715     C**********
084000030715     C                   IF        wGiro = *zeros
084100040113     C*
084200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084500080429     C                             %trim(intcol(5))<>*blanks
084600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084800080429     C                                      %trim(intcol(5))
084900070823     C     ';':CharCSV   XLATE     vindta        vindta
085000040510     C                   ENDIF
085100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085200031201     C     minu:maiu     XLATE     vindta        vindta
085300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085400030715     C                   DOW       posDa <= %len(%trim(vindta))
085500030715     C*
085600030715     C* Gestisco il 1° campo
085700030715     C                   IF        i = 1
085800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085900030715     C                   EVAL      posDa = 1
086000030715     C* Eseguo lo scan x trovare la fine del primo campo
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C* X i campi successivi al 1°
086600030715     C                   ELSE
086700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086800030715     C                   EVAL      posDa = posA + 1
086900030715     C* Eseguo lo scan x trovare la fine del campo corrente
087000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087100030715     C* Gestisco l'ultimo campo
087200030715     C                   IF        posA = *zeros
087300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087400030715     C* A questo punto "estraggo" il campo corrente
087500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087600030715     C                                                    (posA-posDa))
087700030715     C                   LEAVE
087800030715     C                   ELSE
087900030715     C* A questo punto "estraggo" il campo corrente
088000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088100030715     C                                                    (posA-posDa))
088200030715     C                   ENDIF
088300030715     C                   ENDIF
088400030715     C* Incremento il contatore d campo
088500030715     C                   EVAL      i = i +1
088600030715     C                   ENDDO
088700030715     C*
088800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088900030715     C* delimitazione testo
089000030715     C                   EVAL      i = 1
089100030715     C                   DOW       i < %elem(SkSplitFLD)
089200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089400030715     C                   EVAL      i = i + 1
089500030715     C                   ENDDO
089600030715     C                   EVAL      wGiro = 1
089700030715     C                   ELSE
089800030715     C                   EVAL      wGiro = 2
089900031204     C**********
090000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090100031204     C* d separatore campo
090200031204     C**********
090300031204     C                   EVAL      vindta = %trim(vindta)
090400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090500031204     C                   EVAL      vindta = %subst(vindta:2)
090600031204     C                   ENDDO
090700031204     C*
090800031204     C                   Z-ADD     *zeros        lunghInput        4 0
090900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091300110912     C                   ENDDO
091400030715     C**********
091500030715     C* Effettuo lo split del campo dati d input
091600030715     C**********
091700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091800030715     C                   DOW       posDa <= %len(%trim(vindta))
091900030715     C*
092000030715     C* Gestisco il 1° campo
092100030715     C                   IF        i = 1
092200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092300030715     C                   EVAL      posDa = 1
092400030715     C* Eseguo lo scan x trovare la fine del primo campo
092500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092600030715     C* A questo punto "estraggo" il campo corrente
092700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092800030715     C                                                    (posA-posDa))
092900030715     C* X i campi successivi al 1°
093000030715     C                   ELSE
093100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093200030715     C                   EVAL      posDa = posA + 1
093300031204     C* Verifico che nn vi sia il campo nn valorizzato
093400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093500031204     C* Se campo nn valorizzato skippo al prossimo
093600031204     C                   EVAL      SkSplitCSV(i) = *blanks
093700031204     C                   EVAL      posA  = posA + 1
093800031204     C                   ELSE
093900030715     C* Eseguo lo scan x trovare la fine del campo corrente
094000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094100030715     C* Gestisco l'ultimo campo
094200030715     C                   IF        posA = *zeros
094300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094400030715     C* A questo punto "estraggo" il campo corrente
094500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094600030715     C                                                    (posA-posDa))
094700030715     C                   LEAVE
094800030715     C                   ELSE
094900030715     C* A questo punto "estraggo" il campo corrente
095000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095100030715     C                                                    (posA-posDa))
095200030715     C                   ENDIF
095300031204     C                   ENDIF
095400030715     C                   ENDIF
095500030715     C* Incremento il contatore d campo
095600030715     C                   EVAL      i = i +1
095700030715     C                   ENDDO
095800030715     C*
095900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096000030715     C* delimitazione testo
096100030715     C                   EVAL      i = 1
096200030715     C                   DOW       i < %elem(SkSplitCSV)
096300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096500030715     C                   EVAL      i = i + 1
096600030715     C                   ENDDO
096700100720     C*
096800100720     C* Stacco un progressivo x numero spedizione
096900130610 xxx C                   EXSR      repNSP
097000100326     C*
097100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097200030715     C                   EVAL      i = 1
097300030715     C                   DOW       i < %elem(SkSplitFLD)
097400030715     C*
097500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097700030715     C                             SkSplitCSV(i) <> *blanks
097800030721     C*** ATB
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098000030721     C                   EVAL      VABATB = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030715     C*** RSD
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098700030715     C* ==
098800130528     C                   ENDIF
098900030721     C*** RD2
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030715     C*** IND
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099500030715     C                   EVAL      VABIND = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** LOD
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030715     C*** PRD
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030721     C*** NZD
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030715     C*** RMA
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** NOT
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** NT2
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** VCA
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030715     C*** VAS
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** FFD
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030721     C*** GC1
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** GC2
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** TSP
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104400030721     C                   ENDIF
104500040510     C*** CBO
104600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104800040510     C                   ENDIF
104900030721     C*** NAS
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** TIC
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** GCA
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** XCO
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** CTM
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** TCR
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** CTS
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** FTM
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** VAD
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** GMA
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** GGA
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** GVA
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** TC1
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** TC2
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** SCL
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** RMO
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** NMO
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*
111800030721     C* Reperisco quindi i campi numerici...
111900040510     C*** NSP
112000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112100040510     C                   EVAL      PiStr=SkSplitCSV(i)
112200040510     C                   EXSR      CHKNUM
112300040510     C                   IF        PiInt=*on
112400040510     C                   Z-ADD     PiVal         VABNSP
112500040510     C                   ELSE
112600040510     C                   SETON                                        31
112700040510     C                   EVAL      VABNSP = *zeros
112800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112900040510     C                             + ' ' + 'VABNSP'
113000040510     C                   ENDIF
113100040510     C                   ENDIF
113200071106     C*** RMN
113300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113400071106     C                   EVAL      PiStr=SkSplitCSV(i)
113500071106     C                   EXSR      CHKNUM
113600071106     C                   IF        PiInt=*on
113700071106     C                   Z-ADD     PiVal         VABRMN
113800071106     C                   ELSE
113900071106     C                   SETON                                        32
114000071106     C                   EVAL      VABRMN = *zeros
114100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114200071106     C                             + ' ' + 'VABRMN'
114300071106     C                   ENDIF
114400071106     C                   ENDIF
114500030721     C*** CAD
114600130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114700130610     C                   EVAL      PiStr=SkSplitCSV(i)
114800130610     C                   EXSR      CHKNUM
114900130610     C                   IF        PiInt=*on
115000130610     C                   Z-ADD     PiVal         Num5_0
115100130610     C                   MOVEL(p)  Num5_0        VABCAD
115200130610     C                   ELSE
115300130610     C                   SETON                                        32
115400130610     C                   EVAL      VABCAD = *zeros
115500130610     C                   EVAL      vinmsg = %trimr(vinmsg)
115600130610     C                             + ' ' + 'VABCAD'
115700130610     C                   ENDIF
115800130605     C                   ENDIF
115900030721     C*** NCL
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on
116400030721     C                   Z-ADD     PiVal         VABNCL
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABNCL = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900040122     C                             + ' ' + 'VABNCL'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** PKB
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiNum=*on
117700130610     C                   Z-ADD(H)  PiVal         VABPKB
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      VABPKB = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200030721     C                             + ' ' + 'VABPKB'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** CAS
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118700110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
118800111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
118900111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
119000130703     C                   EVAL      VABVCA = 'EUR'
119100101018     C                   EVAL      wFlgCAS = '1'
119200101018     C                   ENDIF
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiNum=*on
119600030721     C                   Z-ADD     PiVal         VABCAS
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABCAS = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABCAS'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** CCM
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on      AND
120900030721     C                             PiVal<=9999999 AND
121000030721     C                             PiVal>*zeros
121100030721     C                   Z-ADD     PiVal         VABCCM
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      VABCCM = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600040510     C                             + ' ' + 'VABCCM'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** LNP
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on    AND
122400030721     C                             PiVal<=999   AND
122500030721     C                             PiVal>*zeros
122600030721     C                   Z-ADD     PiVal         VABLNP
122700030721     C                   ELSE
122800040715     C                   SETON                                        32
122900030721     C                   EVAL      VABLNP = *zeros
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100040510     C                             + ' ' + 'VABLNP'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** NRS
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on AND
123900030721     C                             PiVal<=99
124000030721     C                   Z-ADD     PiVal         VABNRS
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABNRS'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** LNA
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on    AND
125200030721     C                             PiVal<=999   AND
125300030721     C                             PiVal>*zeros
125400030721     C                   Z-ADD     PiVal         VABLNA
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      VABLNA = *zeros
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABLNA'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** CTR
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on  AND
126700030721     C                             PiVal<=999
126800030721     C                   Z-ADD     PiVal         VABCTR
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABCTR'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** IAS
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiNum=*on
128000030721     C                   Z-ADD     PiVal         VABIAS
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABIAS'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** VLB
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiNum=*on
129200130610     C                   Z-ADD(H)  PiVal         VABVLB
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABVLB'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** QFT
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiNum=*on
130400030721     C                   Z-ADD     PiVal         VABQFT
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABQFT'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** NCD
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiInt=*on      AND
131600030721     C                             PiVal<=9999999
131700030721     C                   Z-ADD     PiVal         VABNCD
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABNCD'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** NCA
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on      AND
132900030721     C                             PiVal<=9999999
133000030721     C                   Z-ADD     PiVal         VABNCA
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABNCA'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030721     C*** ZNC
133800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on AND
134200030721     C                             PiVal<=99
134300030721     C                   Z-ADD     PiVal         VABZNC
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134700030721     C                             + ' ' + 'VABZNC'
134800030721     C                   ENDIF
134900030721     C                   ENDIF
135000030721     C*** DCR
135100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135200030721     C                   EVAL      PiStr=SkSplitCSV(i)
135300030721     C                   EXSR      CHKNUM
135400030721     C                   IF        PiInt=*on       AND
135500030721     C                             PiVal<=99999999
135600030721     C                   Z-ADD     PiVal         VABDCR
135700030721     C                   ELSE
135800030721     C                   SETON                                        32
135900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136000030721     C                             + ' ' + 'VABDCR'
136100030721     C                   ENDIF
136200030721     C                   ENDIF
136300030721     C*** HCR
136400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136500030721     C                   EVAL      PiStr=SkSplitCSV(i)
136600030721     C                   EXSR      CHKNUM
136700030721     C                   IF        PiInt=*on   AND
136800030721     C                             PiVal<=9999
136900030721     C                   Z-ADD     PiVal         VABHCR
137000030721     C                   ELSE
137100030721     C                   SETON                                        32
137200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137300030721     C                             + ' ' + 'VABHCR'
137400030721     C                   ENDIF
137500030721     C                   ENDIF
137600030721     C*** VMD
137700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137800030721     C                   EVAL      PiStr=SkSplitCSV(i)
137900030721     C                   EXSR      CHKNUM
138000030721     C                   IF        PiNum=*on
138100030721     C                   Z-ADD     PiVal         VABVMD
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABVMD'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800030721     C*** ANT
138900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139000030721     C                   EVAL      PiStr=SkSplitCSV(i)
139100030721     C                   EXSR      CHKNUM
139200030721     C                   IF        PiInt=*on        AND
139300030721     C                             PiVal<=999999999
139400030721     C                   Z-ADD     PiVal         VABANT
139500030721     C                   ELSE
139600030721     C                   SETON                                        32
139700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139800030721     C                             + ' ' + 'VABANT'
139900030721     C                   ENDIF
140000030721     C                   ENDIF
140100030721     C*** CMO
140200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140300030721     C                   EVAL      PiStr=SkSplitCSV(i)
140400030721     C                   EXSR      CHKNUM
140500030721     C                   IF        PiInt=*on
140600030721     C                   Z-ADD     PiVal         Num5_0
140700030721     C                   MOVEL(p)  Num5_0        VABCMO
140800030721     C                   ELSE
140900030721     C                   SETON                                        32
141000030721     C                   EVAL      VABCMO = *zeros
141100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141200030721     C                             + ' ' + 'VABCMO'
141300030721     C                   ENDIF
141400030721     C                   ENDIF
141500040510     C*
141600040510     C* Reperisco i campi del FIVAT
141700040510     C*** VATNOT
141800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142000040510     C                   ENDIF
142100040510     C*** VATTRC
142200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142400040510     C                   ENDIF
142500060331     C*** VATNOT_A
142600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142800060331     C                   ENDIF
142900060331     C*** VATNOT_B
143000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143200060331     C                   ENDIF
143300060331     C*** VATNOT_E
143400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143600060331     C                   ENDIF
143700110927     C*** VATNOT_I
143800131129     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
143900131129     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
144000110927     C                   ENDIF
144100070823     C*
144200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
144300120301
144400130805      *** W_VATNOT_E: deve essere numerico perché viena calcolato il check digit
144500130805      *** dopodiché è sia VABRMA che VABRMN ma solo gli ultimi 15 char a dx
144600130610     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
144700130610     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144800130805     C                   EVAL      PiStr=wVATNOT_E
144900130805     C                   EXSR      CHKNUM
145000130805     C                   IF        PiInt=*on
145100130805     C                   EVAL      VABRMA =
145200130805     C                              %subst(wVATNOT_E:%scan(' ':wVATNOT_E)-15:15)
145300130805     C                   EVAL      VABRMN = %dec(VABRMA:15:0)
145400130805     C                   ELSE
145500130805     C                   EVAL      wVATNOT_E = *blank
145600130805     C                   SETON                                        32
145700130805     C                   EVAL      vinmsg = %trimr(vinmsg)
145800130805     C                             + ' ' + 'VATNOT_E VABRMA VABRMN'
145900130805     C                   ENDIF
146000120411     C                   ENDIF
146100130610
146200130610      *** W_VABNOT: è sia VABNOT che VABNT2
146300130610     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
146400130610     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
146500130610     C                   EVAL      VABNOT =
146600130610     C                              %subst(wNOT:1:35)
146700130610     C                   EVAL      VABNT2 =
146800130610     C                              %subst(wNOT:36:35)
146900130610     C                   ENDIF
147000070925     C*
147100071029     C***  <===  -----------------------------------------
147200040510     C*
147300030715     C                   ENDIF
147400030715     C                   EVAL      i = i + 1
147500030715     C                   ENDDO
147600070925     C*
147700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147800120411     C*
147900130626     C* Forzatura tipo incasso 'BM' in caso di non indicazione specifica dal cliente
148000130703     C                   IF        wFlgCAS = '1'
148100140422     C                   IF        VABTIC = *blanks AND VABCCM <> 0506138
148200130626     C                   EVAL      VABTIC = 'BM'
148300130626     C                   ENDIF
148400130703     C                   ENDIF
148500160331     C*
148600160331     C* Forzatura tipo incasso blank in caso si tratti del cliente 0506139
148700160331     C                   IF        VABCCM = 0506139
148800160331     C                   EVAL      VABTIC = *blank
148900160331     C                   ENDIF
149000100831     C*
149100131129     C* email destinatario
149200131129     C                   IF        %trim(wVATNOT_IJ) <> *blanks
149300131129     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
149400131129     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
149500131129     C                   ENDIF
149600070925     C*
149700071029     C***  <===  -----------------------------------------
149800070925     C*
149900070925     C* Considerazioni finali su CBO/CAS
150000100318     C                   IF        wFlgCAS = '1'    OR
150100100318     C                             VABCAS  > *zeros
150200070925     C                   IF        VABCBO = '1'
150300070925     C                   EVAL      VABCBO = '4'
150400070925     C                   ENDIF
150500070925     C                   IF        VABCBO = '2'
150600070925     C                   EVAL      VABCBO = '6'
150700070925     C                   ENDIF
150800070925     C                   ENDIF
150900070925     C***  <===  ----------------------------
151000040510     C*
151100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151200040510     C                   IF        VABNSP = *zeros
151300040510     C                   SETON                                        31
151400040510     C                   ENDIF
151500040715     C*
151600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
151700040715     C                   IF        VABLNP = *zeros
151800040715     C                   SETON                                        31
151900040715     C                   ENDIF
152000040510     C*
152100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152200120411     C                   IF        VABRMN = *zeros
152300120411     C                   Z-ADD     VABNSP        VABRMN
152400120411     C                   ENDIF
152500120328     C* IN QUESTO CASO LO FORZO = 1
152600120411     C***                IF        VABRMN = *zeros
152700120411     C***                Z-ADD     1             VABRMN
152800120411     C***                ENDIF
152900040510     C*
153000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153100040510     C                   IF        VABRMA = *blanks
153200040714     C                   MOVEL     VABRMN        VABRMA
153300040510     C                   ENDIF
153400040128     C*
153500040510     C* Se provincia nn valorizzata la reperisco
153600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153700110912     C                   IF        VABPRD =  *blanks AND
153800110912     C                             VABNZD =  *blanks AND
153900110912     C                             VABCAD <> *blanks AND
154000110912     C                             VABLOD <> *blanks
154100040128     C                   CLEAR                   TISI95DS
154200040128     C                   EVAL      I95TCN = '3'
154300040128     C                   Z-ADD     datcor        I95DAT
154400040128     C                   EVAL      I95NAR = VABNZD
154500040128     C                   EVAL      I95CAP = VABCAD
154600040128     C                   EVAL      I95LOC = VABLOD
154700040128     C                   CALL      'TISI95R'
154800040128     C                   PARM                    TISI95DS
154900040128     C                   EVAL      VABPRD = O95PRV
155000040510     C                   ENDIF
155100020204     C*
155200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155300020204     C                   EXSR      CHKIMPDIV
155400030715     C                   ENDIF
155500020204     C*
155600010607     C                   ENDSR
155700010607     C*----------------------------------------------------*
155800010601
155900020204
156000020204     C*----------------------------------------------------*
156100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156200020204     C*----------------------------------------------------*
156300020204     C     CHKIMPDIV     BEGSR
156400020204     C*
156500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156600020204     C                   Z-ADD     *zeros        wrkDec            9 9
156700020204     C*
156800020204     C* Come prima cosa effettuo considerazioni sulla divisa
156900020204     C                   IF        vabIAS > *zeros
157000020204     C                   IF        vabVAS <> 'EUR'
157100020204     C                   EVAL      vabVAS =  'ITL'
157200020204     C                   ENDIF
157300020204     C                   ENDIF
157400020204     C*
157500020204     C                   IF        vabCAS > *zeros
157600020204     C                   IF        vabVCA <> 'EUR'
157700020204     C                   EVAL      vabVCA =  'ITL'
157800020204     C                   ENDIF
157900020204     C                   ENDIF
158000020204     C*
158100020204     C                   IF        vabVMD > *zeros
158200020204     C                   IF        vabVAD <> 'EUR'
158300020204     C                   EVAL      vabVAD =  'ITL'
158400020204     C                   ENDIF
158500020204     C                   ENDIF
158600020204     C*
158700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
158800020204     C                   Z-ADD     vabIAS        wrkDec
158900020204     C                   IF        wrkDec > *zeros
159000020204     C                   IF        vabVAS = 'ITL'
159100020204     C                   EVAL      vabIAS = *zeros
159200020204     C                   ENDIF
159300020204     C                   ENDIF
159400020204     C*
159500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
159600020204     C                   Z-ADD     vabCAS        wrkDec
159700020204     C                   IF        wrkDec > *zeros
159800020204     C                   IF        vabVCA = 'ITL'
159900020204     C                   EVAL      vabCAS = *zeros
160000020204     C                   ENDIF
160100020204     C                   ENDIF
160200020204     C*
160300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160400020204     C                   Z-ADD     vabVMD        wrkDec
160500020204     C                   IF        wrkDec > *zeros
160600020204     C                   IF        vabVAD = 'ITL'
160700020204     C                   EVAL      vabVMD = *zeros
160800020204     C                   ENDIF
160900020204     C                   ENDIF
161000020204     C*
161100020204     C                   ENDSR
161200020204     C***
161300010330
161400100318
161500100318
161600100318     C*----------------------------------------------------*
161700100318     C*  CONTROLLO NUMERICITA' CAMPI
161800100318     C*----------------------------------------------------*
161900100318     C     CHKNUM        BEGSR
162000100318     C*
162100100318     C                   IF        PiDecChr = *blanks
162200100318     C                   EVAL      PiDecChr = CharNUM
162300100318     C                   ENDIF
162400100318     C*
162500100318     C                   callp     UBISNUM_Check(PiStr
162600100318     C                                          :PiDecChr
162700100318     C                                          :PiVal
162800100318     C                                          :PiNum
162900100318     C                                          :PiInt)
163000100318     C*
163100100318     C                   ENDSR
163200100318     C***
163300010601
163400010601
163500010601
163600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163700010601     C     repfil        BEGSR
163800010601     C*
163900010601     C                   if        invfil = *zeros and
164000010601     C                             depfil > *zeros and
164100010629     C                             (vinflg = *blanks or
164200010629     C                              vinflg = *zeros)
164300010601     C
164400010601     C                   eval      invfil = depfil
164500010601     C                   endif
164600010601     C*
164700010601     C                   if        depfil <> invfil and
164800010601     C                             invfil > *zeros
164900010601     C                   eval      flgMulti = '1'
165000010601     C                   if        vinflg = *blanks
165100010601     C                   add       1             cntNonEl
165200010601     C                   endif
165300010601     C                   endif
165400010601     C*
165500010601     C                   if        vinflg = '2'
165600010601     C                   eval      flgStato = '2'
165700010601     C                   endif
165800010601     C*
165900010601     C                   ENDSR
166000010601     C***
166100010601
166200010601
166300010601
166400010330
166500040119      /TITLE Invio dei dati al punto operativo.
166600040119     C     invio         BEGSR
166700040119     C*
166800040119     C* 1° invio FIVAT
166900040119     C                   reset                   dscmz
167000040119     C                   move      vlrpoi        cmzdst
167100040119     C                   eval      cmzfld = 'FIVATWWR'
167200040119     C                   eval      cmzmbd = vlrhdl
167300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167400040119     C***                if        prmfir = *blanks
167500040119     C                   eval      cmzfla = 'FIVAT00F'
167600040119     C                   eval      cmzmba = 'FIVAT00F'
167700040119     C***                else
167800040119     C***                eval      cmzfla = prmfir
167900040119     C***                eval      cmzmba = prmfir
168000040119     C***                endif
168100040119     C                   eval      cmznrr = *zeros
168200040119     C                   move      §ctrokvt      cmznrr
168300040119     C                   eval      cmzlba = vlrfl1
168400040119     C                   call(e)   'TIS711C'
168500040119     C                   parm                    dscmz
168600040119     C                   parm      *blanks       esito
168700040119     C                   if        %error
168800040119     C                             or cmzerr = '1'
168900040119     C                             or esito  = '1'
169000040119     C                   eval      wrkesito = '3'
169100040119     C                   else
169200040119     C*
169300040119     C* 2° invio FIVAB
169400040119     C                   reset                   dscmz
169500040119     C                   move      vlrpoi        cmzdst
169600040119     C                   eval      cmzfld = vlrfou
169700040119     C                   eval      cmzmbd = vlrhdl
169800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169900040119     C***                if        prmfir = *blanks
170000040119     C                   eval      cmzfla = 'FIVAB00F'
170100040119     C                   eval      cmzmba = 'FIVAB00F'
170200040119     C***                else
170300040119     C***                eval      cmzfla = prmfir
170400040119     C***                eval      cmzmba = prmfir
170500040119     C***                endif
170600040119     C                   eval      cmznrr = *zeros
170700040119     C                   move      §ctrokvb      cmznrr
170800040119     C                   eval      cmzlba = vlrfl1
170900040119     C                   call(e)   'TIS711C'
171000040119     C                   parm                    dscmz
171100040119     C                   parm      *blanks       esito
171200040119     C                   if        %error
171300040119     C                             or cmzerr = '1'
171400040119     C                             or esito  = '1'
171500040119     C                   eval      wrkesito = '3'
171600040119     C                   endif
171700040119     C                   endif
171800040119     C*
171900040119     C                   ENDSR
172000040119     C***
172100010601
172200010601
172300010601
172400010601
172500010601      /TITLE Invio dei dati al punto operativo.
172600010601     C     opeini        BEGSR
172700010601     C*
172800010601     C* Inizializzo flag e contatori operativi
172900010601     C                   movel     '0'           flgGiro           1
173000010601     C                   movel     '0'           flgMulti          1
173100010601     C                   movel     '1'           flgStato          1
173200010615     C                   movel     '0'           flgOk             1
173300010601     C                   z-add     *zeros        cntNonEl         10 0
173400010601     C                   z-add     *zeros        depfil            3 0
173500010601     C                   z-add     *zeros        invfil            3 0
173600010601     C*
173700010601     C                   ENDSR
173800010601     C***
173900070326
174000070326
174100070326
174200070326
174300070326     C     *pssr         BEGSR
174400070329     C*
174500070329     C                   if        %open(tivin00r)
174600070329     C                   close     tivin00r
174700070329     C                   endif
174800070329     C                   if        %open(fivabwwr)
174900070329     C                   close     fivabwwr
175000070329     C                   endif
175100070329     C                   if        %open(fivatwwr)
175200070329     C                   close     fivatwwr
175300070329     C                   endif
175400070326     C*
175500070326     C* Effettuo la chiamata al CLLE preposto
175600070326     C                   call(e)   'TITVVTC'
175700070326     C                   parm                    parccm
175800070326     C                   parm                    parmbr
175900070326     C                   parm      '2'           paropz
176000070326     C*
176100070326     C                   eval      wrkesito = '2'
176200070404     C*
176300070404     C                   seton                                        LR
176400070326     C*
176500070326     C                   ENDSR     '*CANCL'
176600070326     C***
176700100318
176800100318
176900100318
177000100318     C     normCSV       BEGSR
177100100318     C*
177200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177300100318     C* separatori campi
177400100318     C                   if        CharTXT <> *blanks
177500100318     C                   z-add     *zeros        wLenVINDTA        4 0
177600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
177700100318     C                   setoff                                       80
177800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
177900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
178000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
178100100318     C                   if        not *in80
178200100318     C                   seton                                        80
178300100318     C                   else
178400100318     C                   setoff                                       80
178500100318     C                   endif
178600100318     C                   endif
178700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
178800100318     C                             and *in80 = *on
178900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
179000100318     C                   endif
179100100318     C                   enddo
179200100318     C                   endif
179300100318     C*
179400100318     C                   ENDSR
179500100318     C***
179600070326
179700100326
179800100326
179900100326     C*----------------------------------------------------*
180000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
180100100326     C*----------------------------------------------------*
180200100326     C     REPNSP        BEGSR
180300100326     C*
180400100326     C* NSP => Stacco un numeratore da AZNUM
180500100326     C                   clear                   TRUL33DS
180600100326     C                   eval      I33OPE = *zeros
180700100326     C                   eval      I33CNU = 302
180800100326     C                   eval      I33NUM = 1
180900100326     C                   movel     TRUL33DS      KPJBU
181000100326     C                   call      'TRUL33R'
181100100326     C                   parm                    KPJBA
181200100326     C                   movel     KPJBU         TRUL33DS
181300100326     C                   if        O33ERR = *zeros
181400100326     C                   z-add     O33NRF        VABNSP
181500100326     C                   z-add     O33NRF        VATNSP
181600100326     C                   else
181700100326     C                   SETON                                        31
181800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
181900100326     C                             + ' ' + 'VABNSP VATNSP'
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
184000130610W_VATNOT_E;VATNOT_A;VABRSD;VABIND;VABLOD;VABCAD;VATNOT_B;VABCAS;VABPKB;VABVLB;VABNCL;
184100131129W_VABNOT;VABTIC;W_VATNOT_I
184200130528
184300111116
184400100318
